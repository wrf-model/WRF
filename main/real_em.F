!  Create an initial data set for the WRF model based on real data.  This
!  program is specifically set up for the Eulerian, mass-based coordinate.
PROGRAM real_data

   USE module_machine
   USE module_domain
   USE module_initialize
   USE module_io_domain
   USE module_driver_constants
   USE module_configure
   USE module_timing
   USE esmf_mod
#ifdef DM_PARALLEL
   USE module_dm
#endif

   IMPLICIT NONE

   REAL    :: time , bdyfrq

   INTEGER :: loop , levels_to_process , debug_level


   TYPE(domain) , POINTER :: null_domain
   TYPE(domain) , POINTER :: grid
   TYPE (grid_config_rec_type)              :: config_flags
   INTEGER                :: number_at_same_level

   INTEGER :: max_dom, domain_id
   INTEGER :: idum1, idum2 
#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: i , j , k , idts, rc

   CHARACTER (LEN=80)     :: message

   INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second
   INTEGER ::   end_year ,   end_month ,   end_day ,   end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop
   TYPE(ESMF_TimeInterval) :: time_interval
real::t1,t2
   INTERFACE
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "REAL_EM V2.0 PREPROCESSOR"

#ifdef DM_PARALLEL
   CALL disable_quilting
#endif

   !  Initialize the modules used by the WRF system.  Many of the CALLs made from the
   !  init_modules routine are NO-OPs.  Typical initializations are: the size of a
   !  REAL, setting the file handles to a pre-use value, defining moisture and
   !  chemistry indices, etc.

   CALL       wrf_debug ( 100 , 'real_em: calling init_modules ' )
   CALL init_modules

   !  The configuration switches mostly come from the NAMELIST input.

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
      CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize
#else
   CALL initial_config
#endif

   CALL get_debug_level ( debug_level )
   CALL set_wrf_debug_level ( debug_level )

   CALL  wrf_message ( program_name )

   !  Allocate the space for the mother of all domains.

   NULLIFY( null_domain )
   CALL       wrf_debug ( 100 , 'real_em: calling alloc_and_configure_domain ' )
   CALL alloc_and_configure_domain ( domain_id  = 1           , &
                                     grid       = head_grid   , &
                                     parent     = null_domain , &
                                     kid        = -1            )

   grid => head_grid

   CALL Setup_Timekeeping ( grid )
   CALL ESMF_TimeIntervalSet ( time_interval , S=model_config_rec%interval_seconds, rc=rc )
   CALL ESMF_ClockSetTimeStep ( grid%domain_clock , time_interval , rc=rc )

   CALL       wrf_debug ( 100 , 'real_em: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

   CALL       wrf_debug ( 100 , 'real_em: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   !  Initialize the WRF IO: open files, init file handles, etc.

   CALL       wrf_debug ( 100 , 'real_em: calling init_wrfio' )
   CALL init_wrfio

   !  Some of the configuration values may have been modified from the initial READ
   !  of the NAMELIST, so we re-broadcast the configuration records.

#ifdef DM_PARALLEL
   CALL       wrf_debug ( 100 , 'real_em: re-broadcast the configuration records' )
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
#endif

   !   No looping in this layer.  

   CALL       wrf_debug ( 100 , 'calling med_sidata_input' )
   CALL med_sidata_input ( grid , config_flags )
   CALL       wrf_debug ( 100 , 'backfrom med_sidata_input' )

   !  We are done.

   CALL       wrf_debug (   0 , 'real_em: SUCCESS COMPLETE REAL_EM INIT' )

   CALL wrf_shutdown

END PROGRAM real_data

SUBROUTINE med_sidata_input ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE module_initialize
   USE module_optional_si_input

   USE module_date_time

   IMPLICIT NONE


  ! Interface 
   INTERFACE
     SUBROUTINE start_domain ( grid )  ! comes from module_start in appropriate dyn_ directory
       USE module_domain
       TYPE (domain) grid
     END SUBROUTINE start_domain
   END INTERFACE

  ! Arguments
   TYPE(domain)                :: grid
   TYPE (grid_config_rec_type) :: config_flags
  ! Local
   INTEGER                :: time_step_begin_restart
   INTEGER                :: idsi , ierr , myproc
   CHARACTER (LEN=80)      :: si_inpname
   CHARACTER (LEN=80)      :: message

   CHARACTER(LEN=19) :: start_date_char , end_date_char , current_date_char , next_date_char

   INTEGER :: time_loop_max , loop, rc
   INTEGER :: julyr , julday 
   REAL :: gmt
real::t1,t2

   grid%input_from_file = .true.
   grid%input_from_file = .false.

   CALL compute_si_start_and_end ( model_config_rec%start_year  (grid%id) , &
                                   model_config_rec%start_month (grid%id) , &
                                   model_config_rec%start_day   (grid%id) , &
                                   model_config_rec%start_hour  (grid%id) , &
                                   model_config_rec%start_minute(grid%id) , &
                                   model_config_rec%start_second(grid%id) , &
                                   model_config_rec%  end_year  (grid%id) , & 
                                   model_config_rec%  end_month (grid%id) , &
                                   model_config_rec%  end_day   (grid%id) , &
                                   model_config_rec%  end_hour  (grid%id) , &
                                   model_config_rec%  end_minute(grid%id) , &
                                   model_config_rec%  end_second(grid%id) , &
                                   model_config_rec%interval_seconds      , &
                                   model_config_rec%real_data_init_type   , &
                                   start_date_char , end_date_char , time_loop_max )

   !  Here we define the initial time to process, for later use by the code.
   
   print*,'line 207: start_date_char=',start_date_char,'  current_date_char=',current_date_char
   current_date_char = start_date_char
   start_date = start_date_char // '.0000'
   current_date = start_date
   print*,'line 211: start_date=',start_date,'  current_date=',current_date

   CALL set_bdyfrq ( grid%id , REAL(model_config_rec%interval_seconds) )

   !!!!!!!  Loop over each time period to process.

   DO loop = 1 , time_loop_max

      print *,'-----------------------------------------------------------------------------'
      print *,' '
      print '(A,A,A,I2,A,I2)' , ' Current date being processed: ',current_date, ', which is loop #',loop,' out of ',time_loop_max

      !  After current_date has been set, fill in the julgmt stuff.

      CALL geth_julgmt ( config_flags%julyr , config_flags%julday , config_flags%gmt )

        print *,'configflags%julyr, %julday, %gmt:',config_flags%julyr, config_flags%julday, config_flags%gmt
      !  Now that the specific Julian info is available, save these in the model config record.

      CALL set_gmt (grid%id, config_flags%gmt)
      CALL set_julyr (grid%id, config_flags%julyr)
      CALL set_julday (grid%id, config_flags%julday)

      !  Open the wrfinput file.

      IF ( grid%dyn_opt .EQ. dyn_em ) THEN
         CALL       wrf_debug ( 100 , 'med_sidata_input: calling open_r_dataset for wrf_real_input_em' )
         CALL construct_filename2( si_inpname , 'wrf_real_input_em' , grid%id , 2 , current_date_char )
      END IF
      CALL open_r_dataset ( idsi, TRIM(si_inpname) , grid , config_flags , "DATASET=INPUT", ierr )
      IF ( ( ierr .NE. 0 ) .AND. ( grid%dyn_opt .EQ. dyn_em ) ) THEN
        CALL wrf_error_fatal( 'error opening ' // TRIM(si_inpname) // ' for input; bad date in namelist or file not in directory' )
      ENDIF

      !  Input data.

      CALL       wrf_debug ( 100 , 'med_sidata_input: calling input_aux_model_input1' )
      CALL input_aux_model_input1 ( idsi ,   grid , config_flags , ierr )

      !  Possible optional SI input.  This sets flags used by init_domain.
      IF ( loop .EQ. 1 ) THEN
         CALL       wrf_debug ( 100 , 'med_sidata_input: calling init_module_optional_si_input' )
         CALL init_module_optional_si_input ( grid , config_flags )
      END IF
      CALL       wrf_debug ( 100 , 'med_sidata_input: calling optional_si_input' )
      CALL  optional_si_input ( grid , idsi )

      !  Initialize the mother domain for this time period with input data.

      CALL       wrf_debug ( 100 , 'med_sidata_input: calling init_domain' )
      grid%input_from_file = .true.
      CALL init_domain (  grid )
      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

      !  Close this file that is output from the SI and input to this pre-proc.

      CALL       wrf_debug ( 100 , 'med_sidata_input: back from init_domain' )
      CALL close_dataset ( idsi , config_flags , "DATASET=INPUT" )

!     CALL start_domain ( grid )

      CALL assemble_output ( grid , config_flags , loop , time_loop_max )

      !  Here we define the next time that we are going to process.

      CALL geth_newdate ( current_date_char , start_date_char , loop * model_config_rec%interval_seconds )
      current_date =  current_date_char // '.0000'
      CALL atotime( current_date(1:19), grid%current_time )
      CALL ESMF_ClockSetCurrTime(grid%domain_clock, grid%current_time, rc)
if (loop.ne.time_loop_max )then
print *,'set esmf clock to ',current_date(1:19)
endif

   END DO
END SUBROUTINE med_sidata_input

SUBROUTINE compute_si_start_and_end (  &
   start_year , start_month , start_day , start_hour , start_minute , start_second , &
     end_year ,   end_month ,   end_day ,   end_hour ,   end_minute ,   end_second , &
   interval_seconds , real_data_init_type , &
   start_date_char , end_date_char , time_loop_max )

   USE module_date_time

   IMPLICIT NONE

   INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second
   INTEGER ::   end_year ,   end_month ,   end_day ,   end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop

   CHARACTER(LEN=19) :: current_date_char , start_date_char , end_date_char , next_date_char

   WRITE ( start_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           start_year,start_month,start_day,start_hour,start_minute,start_second
   WRITE (   end_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
             end_year,  end_month,  end_day,  end_hour,  end_minute,  end_second

   IF ( end_date_char .LT. start_date_char ) THEN
      CALL wrf_error_fatal( 'Ending date in namelist ' // end_date_char // ' prior to beginning date ' // start_date_char )
   END IF

!  start_date = start_date_char // '.0000'

   !  Figure out our loop count for the processing times.

   time_loop = 1
   PRINT '(A,I4,A,A,A)','Time period #',time_loop,' to process = ',start_date_char,'.'
   current_date_char = start_date_char
   loop_count : DO
      CALL geth_newdate ( next_date_char , current_date_char , interval_seconds )
      IF      ( next_date_char .LT. end_date_char ) THEN
         time_loop = time_loop + 1
         PRINT '(A,I4,A,A,A)','Time period #',time_loop,' to process = ',next_date_char,'.'
         current_date_char = next_date_char
      ELSE IF ( next_date_char .EQ. end_date_char ) THEN
         time_loop = time_loop + 1
         PRINT '(A,I4,A,A,A)','Time period #',time_loop,' to process = ',next_date_char,'.'
         PRINT '(A,I4,A)','Total analysis times to input = ',time_loop,'.'
         time_loop_max = time_loop
         EXIT loop_count
      ELSE IF ( next_date_char .GT. end_date_char ) THEN
         PRINT '(A,I4,A)','Total analysis times to input = ',time_loop,'.'
         time_loop_max = time_loop
         EXIT loop_count
      END IF
   END DO loop_count
END SUBROUTINE compute_si_start_and_end

SUBROUTINE assemble_output ( grid , config_flags , loop , time_loop_max )

   USE module_big_step_utilities_em
   USE module_domain
   USE module_io_domain
   USE module_configure
   USE module_date_time
   USE module_bc
   IMPLICIT NONE

   TYPE(domain)                 :: grid
   TYPE (grid_config_rec_type)  :: config_flags
   INTEGER , INTENT(IN)         :: loop , time_loop_max

   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: i , j , k , idts

   INTEGER :: id1 , interval_seconds , ierr, rc
   INTEGER , SAVE :: id 
   CHARACTER (LEN=80) :: inpname , bdyname
   CHARACTER(LEN= 4) :: loop_char
character *19 :: temp19
character *24 :: temp24 , temp24b

   REAL , DIMENSION(:,:,:) , ALLOCATABLE , SAVE :: ubdy3dtemp1 , vbdy3dtemp1 , tbdy3dtemp1 , pbdy3dtemp1 , qbdy3dtemp1
   REAL , DIMENSION(:,:,:) , ALLOCATABLE , SAVE :: mbdy2dtemp1
   REAL , DIMENSION(:,:,:) , ALLOCATABLE , SAVE :: ubdy3dtemp2 , vbdy3dtemp2 , tbdy3dtemp2 , pbdy3dtemp2 , qbdy3dtemp2
   REAL , DIMENSION(:,:,:) , ALLOCATABLE , SAVE :: mbdy2dtemp2
real::t1,t2

   !  Various sizes that we need to be concerned about.

   ids = grid%sd31
   ide = grid%ed31
   kds = grid%sd32
   kde = grid%ed32
   jds = grid%sd33
   jde = grid%ed33

   ims = grid%sm31
   ime = grid%em31
   kms = grid%sm32
   kme = grid%em32
   jms = grid%sm33
   jme = grid%em33

   ips = grid%sp31
   ipe = grid%ep31
   kps = grid%sp32
   kpe = grid%ep32
   jps = grid%sp33
   jpe = grid%ep33

   ijds = MIN ( ids , jds )
   ijde = MAX ( ide , jde )

   !  Boundary width, scalar value.

   spec_bdy_width = model_config_rec%spec_bdy_width
   interval_seconds = model_config_rec%interval_seconds

print *,'spec_bdy_width, interval_seconds, loop=',spec_bdy_width , interval_seconds,loop

   IF ( loop .EQ. 1 ) THEN

      !  This is the space needed to save the current 3d data for use in computing
      !  the lateral boundary tendencies.

      ALLOCATE ( ubdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( vbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( tbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( pbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( qbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( mbdy2dtemp1(ims:ime,1:1,    jms:jme) )
      ALLOCATE ( ubdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( vbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( tbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( pbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( qbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( mbdy2dtemp2(ims:ime,1:1,    jms:jme) )

      !  Open the wrfinput file.  From this program, this is an *output* file.

      CALL construct_filename1( inpname , 'wrfinput' , grid%id , 2 )
      CALL open_w_dataset ( id1, TRIM(inpname) , grid , config_flags , output_model_input , "DATASET=INPUT", ierr )
      IF ( ierr .NE. 0 ) THEN
         CALL wrf_error_fatal( 'real: error opening wrfinput for writing' )
      ENDIF
!     CALL calc_current_date ( grid%id , 0. )
      grid%write_metadata = .true.
      CALL output_model_input ( id1, grid , config_flags , ierr )
      CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

      !  We need to save the 3d data to compute a difference during the next loop.  Couple the
      !  3d fields with total mu (mub + mu_2) and the stagger-specific map scale factor.

      CALL couple ( grid%em_mu_2 , grid%em_mub , ubdy3dtemp1 , grid%em_u_2                 , 'u' , grid%msfu , &
                    ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
      CALL couple ( grid%em_mu_2 , grid%em_mub , vbdy3dtemp1 , grid%em_v_2                 , 'v' , grid%msfv , &
                    ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
      CALL couple ( grid%em_mu_2 , grid%em_mub , tbdy3dtemp1 , grid%em_t_2                 , 't' , grid%msft , &
                    ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
      CALL couple ( grid%em_mu_2 , grid%em_mub , pbdy3dtemp1 , grid%em_ph_2                , 'h' , grid%msft , &
                    ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
      CALL couple ( grid%em_mu_2 , grid%em_mub , qbdy3dtemp1 , grid%moist_2(:,:,:,P_QV) , 't' , grid%msft , &
                    ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )

      DO j = jps , MIN(jde-1,jpe)
         DO i = ips , MIN(ide-1,ipe)
            mbdy2dtemp1(i,1,j) = grid%em_mu_2(i,j)
         END DO
      END DO

      !  There are 2 components to the lateral boundaries.  First, there is the starting
      !  point of this time period - just the outer few rows and columns.

      CALL stuff_bdy     ( ubdy3dtemp1 , grid%em_u_b     , 'U' , ijds , ijde , spec_bdy_width      , &
                                                                 ids , ide , jds , jde , kds , kde , &
                                                                 ims , ime , jms , jme , kms , kme , &
                                                                 ips , ipe , jps , jpe , kps , kpe )
      CALL stuff_bdy     ( vbdy3dtemp1 , grid%em_v_b     , 'V' , ijds , ijde , spec_bdy_width      , &
                                                                 ids , ide , jds , jde , kds , kde , &
                                                                 ims , ime , jms , jme , kms , kme , &
                                                                 ips , ipe , jps , jpe , kps , kpe )
      CALL stuff_bdy     ( tbdy3dtemp1 , grid%em_t_b     , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                 ids , ide , jds , jde , kds , kde , &
                                                                 ims , ime , jms , jme , kms , kme , &
                                                                 ips , ipe , jps , jpe , kps , kpe )
      CALL stuff_bdy     ( pbdy3dtemp1 , grid%em_ph_b    , 'W' , ijds , ijde , spec_bdy_width      , &
                                                                 ids , ide , jds , jde , kds , kde , &
                                                                 ims , ime , jms , jme , kms , kme , &
                                                                 ips , ipe , jps , jpe , kps , kpe )
      CALL stuff_bdy     ( qbdy3dtemp1 , grid%em_rqv_b   , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                 ids , ide , jds , jde , kds , kde , &
                                                                 ims , ime , jms , jme , kms , kme , &
                                                                 ips , ipe , jps , jpe , kps , kpe )
      CALL stuff_bdy     ( mbdy2dtemp1 , grid%em_mu_b    , 'M' , ijds , ijde , spec_bdy_width      , &
                                                                 ids , ide , jds , jde , 1 , 1 , &
                                                                 ims , ime , jms , jme , 1 , 1 , &
                                                                 ips , ipe , jps , jpe , 1 , 1 )


   ELSE IF ( loop .GT. 1 ) THEN

      !  Open the boundary file.


      IF ( loop .eq. 2 ) THEN
         CALL construct_filename1( bdyname , 'wrfbdy' , grid%id , 2 )
         CALL open_w_dataset ( id, TRIM(bdyname) , grid , config_flags , output_boundary , "DATASET=BOUNDARY", ierr )
         IF ( ierr .NE. 0 ) THEN
               CALL wrf_error_fatal( 'real: error opening wrfbdy for writing' )
         ENDIF
         grid%write_metadata = .true.
      ELSE
! what's this do?
         grid%write_metadata = .false.
         CALL ESMF_ClockAdvance( grid%domain_clock, rc=rc )
      END IF


      !  Couple this time period's data with total mu, and save it in the *bdy3dtemp2 arrays.

      CALL couple ( grid%em_mu_2 , grid%em_mub , ubdy3dtemp2 , grid%em_u_2                 , 'u' , grid%msfu , &
                    ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
      CALL couple ( grid%em_mu_2 , grid%em_mub , vbdy3dtemp2 , grid%em_v_2                 , 'v' , grid%msfv , &
                    ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
      CALL couple ( grid%em_mu_2 , grid%em_mub , tbdy3dtemp2 , grid%em_t_2                 , 't' , grid%msft , &
                    ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
      CALL couple ( grid%em_mu_2 , grid%em_mub , pbdy3dtemp2 , grid%em_ph_2                , 'h' , grid%msft , &
                    ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
      CALL couple ( grid%em_mu_2 , grid%em_mub , qbdy3dtemp2 , grid%moist_2(:,:,:,P_QV) , 't' , grid%msft , &
                    ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )

      DO j = jps , jpe
         DO i = ips , ipe
            mbdy2dtemp2(i,1,j) = grid%em_mu_2(i,j)
         END DO
      END DO

      !  During all of the loops after the first loop, we first compute the boundary
      !  tendencies with the current data values (*bdy3dtemp2 arrays) and the previously 
      !  saved information stored in the *bdy3dtemp1 arrays.

      CALL stuff_bdytend ( ubdy3dtemp2 , ubdy3dtemp1 , REAL(interval_seconds) , grid%em_u_bt  , 'U' , &
                                                            ijds , ijde , spec_bdy_width      , &
                                                            ids , ide , jds , jde , kds , kde , &
                                                            ims , ime , jms , jme , kms , kme , &
                                                            ips , ipe , jps , jpe , kps , kpe )
      CALL stuff_bdytend ( vbdy3dtemp2 , vbdy3dtemp1 , REAL(interval_seconds) , grid%em_v_bt  , 'V' , &
                                                            ijds , ijde , spec_bdy_width      , &
                                                            ids , ide , jds , jde , kds , kde , &
                                                            ims , ime , jms , jme , kms , kme , &
                                                            ips , ipe , jps , jpe , kps , kpe )
      CALL stuff_bdytend ( tbdy3dtemp2 , tbdy3dtemp1 , REAL(interval_seconds) , grid%em_t_bt  , 'T' , &
                                                            ijds , ijde , spec_bdy_width      , &
                                                            ids , ide , jds , jde , kds , kde , &
                                                            ims , ime , jms , jme , kms , kme , &
                                                            ips , ipe , jps , jpe , kps , kpe )
      CALL stuff_bdytend ( pbdy3dtemp2 , pbdy3dtemp1 , REAL(interval_seconds) , grid%em_ph_bt  , 'W' , &
                                                            ijds , ijde , spec_bdy_width      , &
                                                            ids , ide , jds , jde , kds , kde , &
                                                            ims , ime , jms , jme , kms , kme , &
                                                            ips , ipe , jps , jpe , kps , kpe )
      CALL stuff_bdytend ( qbdy3dtemp2 , qbdy3dtemp1 , REAL(interval_seconds) , grid%em_rqv_bt , 'T' , &
                                                            ijds , ijde , spec_bdy_width      , &
                                                            ids , ide , jds , jde , kds , kde , &
                                                            ims , ime , jms , jme , kms , kme , &
                                                            ips , ipe , jps , jpe , kps , kpe )
      CALL stuff_bdytend ( mbdy2dtemp2 , mbdy2dtemp1 , REAL(interval_seconds) , grid%em_mu_bt  , 'M' , &
                                                            ijds , ijde , spec_bdy_width      , &
                                                            ids , ide , jds , jde , 1 , 1 , &
                                                            ims , ime , jms , jme , 1 , 1 , &
                                                            ips , ipe , jps , jpe , 1 , 1 )

      !  Both pieces of the boundary data are now available to be written (initial time and tendency).
      !  This looks ugly, these date shifting things.  What's it for?  We want the "Times" variable
      !  in the lateral BDY file to have the valid times of when the initial fields are written.
      !  That's what the loop-2 thingy is for with the start date.  We increment the start_date so
      !  that the starting time in the attributes is the second time period.  Why you may ask.  I
      !  agree, why indeed.

      CALL atotime( current_date(1:19), grid%current_time )
      CALL ESMF_ClockSetCurrTime(grid%domain_clock, grid%current_time, rc)

      temp24= current_date
      temp24b=start_date
      start_date = current_date
      CALL geth_newdate ( temp19 , temp24b(1:19) , (loop-2) * model_config_rec%interval_seconds )
      current_date = temp19 //  '.0000'
      CALL atotime( current_date(1:19), grid%current_time )
      CALL ESMF_ClockSetCurrTime(grid%domain_clock, grid%current_time, rc)
print *,'LBC valid between these times ',current_date, ' ',start_date
      CALL output_boundary ( id, grid , config_flags , ierr )
      current_date = temp24
      start_date = temp24b
      CALL atotime( current_date(1:19), grid%current_time )
      CALL ESMF_ClockSetCurrTime(grid%domain_clock, grid%current_time, rc)

      !  OK, for all of the loops, we output the initialzation data, which would allow us to
      !  start the model at any of the available analysis time periods.

!     WRITE ( loop_char , FMT = '(I4.4)' ) loop
!     CALL open_w_dataset ( id1, 'wrfinput'//loop_char , grid , config_flags , output_model_input , "DATASET=INPUT", ierr )
!     IF ( ierr .NE. 0 ) THEN
!       CALL wrf_error_fatal( 'real: error opening wrfinput'//loop_char//' for writing' )
!     ENDIF
!     grid%write_metadata = .true.

!     CALL calc_current_date ( grid%id , 0. )
!     CALL output_model_input ( id1, grid , config_flags , ierr )
!     CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

      !  Is this or is this not the last time time?  We can remove some unnecessary
      !  stores if it is not.

      IF     ( loop .LT. time_loop_max ) THEN

         !  We need to save the 3d data to compute a difference during the next loop.  Couple the
         !  3d fields with total mu (mub + mu_2) and the stagger-specific map scale factor.
         !  We load up the boundary data again for use in the next loop.

         DO j = jps , jpe
            DO k = kps , kpe
               DO i = ips , ipe
                  ubdy3dtemp1(i,k,j) = ubdy3dtemp2(i,k,j)
                  vbdy3dtemp1(i,k,j) = vbdy3dtemp2(i,k,j)
                  tbdy3dtemp1(i,k,j) = tbdy3dtemp2(i,k,j)
                  pbdy3dtemp1(i,k,j) = pbdy3dtemp2(i,k,j)
                  qbdy3dtemp1(i,k,j) = qbdy3dtemp2(i,k,j)
               END DO
            END DO
         END DO

         DO j = jps , jpe
            DO i = ips , ipe
               mbdy2dtemp1(i,1,j) = mbdy2dtemp2(i,1,j)
            END DO
         END DO

         !  There are 2 components to the lateral boundaries.  First, there is the starting
         !  point of this time period - just the outer few rows and columns.

         CALL stuff_bdy     ( ubdy3dtemp1 , grid%em_u_b     , 'U' , ijds , ijde , spec_bdy_width      , &
                                                                    ids , ide , jds , jde , kds , kde , &
                                                                    ims , ime , jms , jme , kms , kme , &
                                                                    ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( vbdy3dtemp1 , grid%em_v_b     , 'V' , ijds , ijde , spec_bdy_width      , &
                                                                    ids , ide , jds , jde , kds , kde , &
                                                                    ims , ime , jms , jme , kms , kme , &
                                                                    ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( tbdy3dtemp1 , grid%em_t_b     , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                    ids , ide , jds , jde , kds , kde , &
                                                                    ims , ime , jms , jme , kms , kme , &
                                                                    ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( pbdy3dtemp1 , grid%em_ph_b    , 'W' , ijds , ijde , spec_bdy_width      , &
                                                                    ids , ide , jds , jde , kds , kde , &
                                                                    ims , ime , jms , jme , kms , kme , &
                                                                    ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( qbdy3dtemp1 , grid%em_rqv_b   , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                    ids , ide , jds , jde , kds , kde , &
                                                                    ims , ime , jms , jme , kms , kme , &
                                                                    ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( mbdy2dtemp1 , grid%em_mu_b    , 'M' , ijds , ijde , spec_bdy_width      , &
                                                                    ids , ide , jds , jde , 1 , 1 , &
                                                                    ims , ime , jms , jme , 1 , 1 , &
                                                                    ips , ipe , jps , jpe , 1 , 1 )

      ELSE IF ( loop .EQ. time_loop_max ) THEN

         !  If this is the last time through here, we need to close the files.

         CALL close_dataset ( id , config_flags , "DATASET=BOUNDARY" )

      END IF

   END IF

END SUBROUTINE assemble_output
