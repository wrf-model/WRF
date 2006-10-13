!  Create an initial data set for the WRF model based on real data.  This
!  program is specifically set up for the NMM core.

PROGRAM real_data

   USE module_machine
   USE module_domain
   USE module_initialize
   USE module_io_domain
   USE module_driver_constants
   USE module_configure
   USE module_timing
#ifdef WRF_CHEM
   USE module_input_chem_data
   USE module_input_chem_bioemiss
#endif
   USE module_utility
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
!   INTEGER, PARAMETER      :: configbuflen = 2*1024
   INTEGER, PARAMETER      :: configbuflen = 4*CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: i , j , k , idts

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

   CHARACTER (LEN=80)     :: message

   INTEGER :: start_year , start_month , start_day 
   INTEGER :: start_hour , start_minute , start_second
   INTEGER :: end_year ,   end_month ,   end_day ,   &
              end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop, rc
   REAL    :: t1,t2

#include "version_decl"

   INTERFACE
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "REAL_NMM " // TRIM(release_version) // " PREPROCESSOR"

#ifdef DM_PARALLEL
   CALL disable_quilting
#endif

   !  Initialize the modules used by the WRF system.  
   !  Many of the CALLs made from the
   !  init_modules routine are NO-OPs.  Typical initializations 
   !  are: the size of a
   !  REAL, setting the file handles to a pre-use value, defining moisture and
   !  chemistry indices, etc.

   CALL       wrf_debug ( 100 , 'real_nmm: calling init_modules ' )

!!!!   CALL init_modules
   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)
   CALL WRFU_Initialize( defaultCalendar=WRFU_CAL_GREGORIAN, rc=rc )
   CALL init_modules(2)   ! Phase 2 resumes after MPI_INIT() (if it is called)

   !  The configuration switches mostly come from the NAMELIST input.

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
	write(0,*) 'call initial_config'
      CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize
#else
   CALL initial_config
#endif


   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   CALL  wrf_message ( program_name )

   !  Allocate the space for the mother of all domains.

   NULLIFY( null_domain )
   CALL  wrf_debug ( 100 , 'real_nmm: calling alloc_and_configure_domain ' )
   CALL alloc_and_configure_domain ( domain_id  = 1           , &
                                     grid       = head_grid   , &
                                     parent     = null_domain , &
                                     kid        = -1            )

   grid => head_grid

#include "deref_kludge.h"
   CALL Setup_Timekeeping ( grid )
   CALL domain_clock_set( grid, &
                          time_step_seconds=model_config_rec%interval_seconds )
   CALL wrf_debug ( 100 , 'real_nmm: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

   CALL     wrf_debug ( 100 , 'real_nmm: calling model_to_grid_config_rec ' )

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

	write(0,*) 'after model_to_grid_config_rec, e_we, e_sn are: ', &
                    config_flags%e_we, config_flags%e_sn

   !  Initialize the WRF IO: open files, init file handles, etc.

   CALL       wrf_debug ( 100 , 'real_nmm: calling init_wrfio' )
   CALL init_wrfio

!  Some of the configuration values may have been modified from the initial READ
!  of the NAMELIST, so we re-broadcast the configuration records.

#ifdef DM_PARALLEL
   CALL wrf_debug ( 100 , 'real_nmm: re-broadcast the configuration records' )
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
#endif

   !   No looping in this layer.  

   CALL med_sidata_input ( grid , config_flags )

   !  We are done.

   CALL       wrf_debug (   0 , 'real_nmm: SUCCESS COMPLETE REAL_NMM INIT' )

#ifdef DM_PARALLEL
    CALL wrf_dm_shutdown
#endif

   CALL WRFU_Finalize( rc=rc )

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
#ifdef WRF_CHEM
   USE module_input_chem_data
   USE module_input_chem_bioemiss
#endif

   USE module_si_io_nmm

   USE module_date_time

   IMPLICIT NONE


  ! Interface 
   INTERFACE
     SUBROUTINE start_domain ( grid , allowed_to_read )
       USE module_domain
       TYPE (domain) grid
       LOGICAL, INTENT(IN) :: allowed_to_read
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

   CHARACTER(LEN=19) :: start_date_char , end_date_char , &
                        current_date_char , next_date_char

   INTEGER :: time_loop_max , loop
   INTEGER :: julyr , julday , LEN

   INTEGER :: io_form_auxinput1
   INTEGER, EXTERNAL :: use_package

   REAL :: gmt
   REAL :: t1,t2

   INTEGER :: numx_sm_levels_input,numx_st_levels_input
   REAL,DIMENSION(100) :: smx_levels_input,stx_levels_input


#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

#include "deref_kludge.h"


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

   current_date_char = start_date_char
!   start_date = start_date_char // '.0000'
   start_date = start_date_char 
   current_date = start_date

   CALL nl_set_bdyfrq ( grid%id , REAL(model_config_rec%interval_seconds) )

   !  Loop over each time period to process.

   write(0,*) 'time_loop_max: ', time_loop_max
   DO loop = 1 , time_loop_max

      write(0,*) 'loop=', loop

      print *,'-----------------------------------------------------------'
      print *,' '
      print '(A,A,A,I2,A,I2)' , ' Current date being processed: ', &
        current_date, ', which is loop #',loop,' out of ',time_loop_max

      !  After current_date has been set, fill in the julgmt stuff.

      CALL geth_julgmt ( config_flags%julyr , config_flags%julday , &
                                              config_flags%gmt )

      !  Now that the specific Julian info is available, 
      !  save these in the model config record.

      CALL nl_set_gmt (grid%id, config_flags%gmt)
      CALL nl_set_julyr (grid%id, config_flags%julyr)
      CALL nl_set_julday (grid%id, config_flags%julday)

      CALL nl_get_io_form_auxinput1( 1, io_form_auxinput1 )
      write(0,*)" io_form_auxinput1 = ",io_form_auxinput1

      SELECT CASE ( use_package(io_form_auxinput1) )
#ifdef NETCDF
      CASE ( IO_NETCDF   )

      !  Open the wrfinput file.

      IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
         CALL  wrf_debug ( 100 , 'med_sidata_input: calling open_r_dataset for wrf_real_input_nm' )
         current_date_char(11:11)='_'
         CALL construct_filename2( si_inpname , 'wrf_real_input_nm' , &
                                         grid%id , 2 , current_date_char )
         current_date_char(11:11)='T'
      ELSE
         CALL wrf_error_fatal('real: error cant handle this grid%dyn_opt' )
      END IF

      CALL open_r_dataset ( idsi, TRIM(si_inpname), grid, config_flags, "DATASET=AUXINPUT1", ierr )

      IF ( ( ierr .NE. 0 ) .AND. ( grid%dyn_opt .EQ. dyn_nmm ) ) THEN
        CALL wrf_error_fatal('real: error opening wrf_real_input_nm for read '//TRIM(si_inpname) )
      ENDIF

      !  Input data.

      CALL wrf_debug (100, 'med_sidata_input: call input_aux_model_input1_wrf')

      CALL input_aux_model_input1 ( idsi, grid, config_flags, ierr )

      !  Possible optional SI input.  This sets flags used by init_domain.

      IF ( loop .EQ. 1 ) THEN
         CALL  wrf_debug (100, 'med_sidata_input: call init_module_optional_si_input' )
         CALL init_module_optional_si_input ( grid , config_flags )
      END IF
      CALL wrf_debug ( 100 , 'med_sidata_input: calling optional_si_input' )
!
      CALL optional_si_input ( grid , idsi )
	write(0,*) 'maxval st_input(1) within real_nmm: ', maxval(st_input(:,1,:))
!
      CALL close_dataset ( idsi , config_flags , "DATASET=AUXINPUT1" )

#endif
#ifdef INTIO
      CASE ( IO_INTIO )

      !  Possible optional SI input.  This sets flags used by init_domain.

      IF ( loop .EQ. 1 ) THEN
         CALL  wrf_debug (100, 'med_sidata_input: call init_module_optional_si_input' )
         CALL init_module_optional_si_input ( grid , config_flags )
      END IF

      current_date_char(11:11)='_'
      CALL read_si ( grid, current_date_char )
      current_date_char(11:11)='T'

#endif
      CASE DEFAULT
        CALL wrf_error_fatal('real: not valid io_form_auxinput1')
      END SELECT

      grid%nmm_islope=1
      grid%vegfra=grid%nmm_vegfrc
      grid%nmm_dfrlg=grid%nmm_dfl/9.81

      grid%isurban=1
      grid%isoilwater=14

      !  Initialize the mother domain for this time period with input data.

      CALL wrf_debug ( 100 , 'med_sidata_input: calling init_domain' )
      grid%input_from_file = .true.

      CALL init_domain ( grid )

      CALL model_to_grid_config_rec ( grid%id, model_config_rec, config_flags )

      !  Close this file that is output from the SI and input to this pre-proc.

      CALL wrf_debug ( 100 , 'med_sidata_input: back from init_domain' )


!!! not sure about this, but doesnt seem like needs to be called each time
      IF ( loop .EQ. 1 ) THEN
	write(0,*) 'call start_domain'
        CALL start_domain ( grid , .TRUE.)
      END IF

#ifdef WRF_CHEM
      IF ( loop == 1 ) THEN
         IF ( ( grid%chem_opt .EQ. RADM2     ) .OR. &
              ( grid%chem_opt .EQ. RADM2SORG ) .OR. &
              ( grid%chem_opt .EQ. RACM      ) .OR. &
              ( grid%chem_opt .EQ. RACMSORG  ) ) THEN
           ! Read the chemistry data from a previous wrf forecast (wrfout file)
           IF(grid%chem_in_opt == 1 ) THEN
              message = 'INITIALIZING CHEMISTRY WITH OLD SIMULATION'
              CALL  wrf_message ( message )

              CALL input_ext_chem_file( grid )

              IF(grid%bio_emiss_opt == BEIS311 ) THEN
                 message = 'READING BEIS3.11 EMISSIONS DATA'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_bioemiss ( grid , config_flags)
              END IF

           ELSEIF(grid%chem_in_opt == 0)then
              ! Generate chemistry data from a idealized vertical profile
              message = 'STARTING WITH BACKGROUND CHEMISTRY '
              CALL  wrf_message ( message )

              print *,' ETA1 '
              print *, grid%nmm_eta1

              CALL input_chem_profile ( grid )

              IF(grid%bio_emiss_opt == BEIS311 ) THEN
                 message = 'READING BEIS3.11 EMISSIONS DATA'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_bioemiss ( grid , config_flags)
              END IF

           ELSE
             message = 'RUNNING WITHOUT CHEMISTRY INITIALIZATION'
             CALL  wrf_message ( message )
           ENDIF
         ENDIF
      ENDIF
#endif

      config_flags%isurban=1
      config_flags%isoilwater=14

      CALL assemble_output ( grid , config_flags , loop , time_loop_max )

      !  Here we define the next time that we are going to process.

      CALL geth_newdate ( current_date_char , start_date_char , &
                          loop * model_config_rec%interval_seconds )
      current_date =  current_date_char // '.0000'

      CALL domain_clock_set( grid, current_date(1:19) )

      write(0,*) 'current_date= ', current_date

   END DO
END SUBROUTINE med_sidata_input

SUBROUTINE compute_si_start_and_end (  &
          start_year, start_month, start_day, start_hour, &
          start_minute, start_second, &
          end_year ,   end_month ,   end_day ,   end_hour , &
          end_minute ,   end_second , &
          interval_seconds , real_data_init_type , &
          start_date_char , end_date_char , time_loop_max )

   USE module_date_time

   IMPLICIT NONE

   INTEGER :: start_year , start_month , start_day , &
              start_hour , start_minute , start_second
   INTEGER ::   end_year ,   end_month ,   end_day , &
                end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop

   CHARACTER(LEN=19) :: current_date_char , start_date_char , &
                        end_date_char , next_date_char

!   WRITE ( start_date_char , FMT = &
!         '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
!         start_year,start_month,start_day,start_hour,start_minute,start_second
!   WRITE (   end_date_char , FMT = &
!         '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
!          end_year,  end_month,  end_day,  end_hour,  end_minute,  end_second

   WRITE ( start_date_char , FMT = &
         '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)' ) &
         start_year,start_month,start_day,start_hour,start_minute,start_second
   WRITE (   end_date_char , FMT = &
         '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)' ) &
          end_year,  end_month,  end_day,  end_hour,  end_minute,  end_second

!  start_date = start_date_char // '.0000'

   !  Figure out our loop count for the processing times.

   time_loop = 1
   PRINT '(A,I4,A,A,A)','Time period #',time_loop, &
                        ' to process = ',start_date_char,'.'
   current_date_char = start_date_char
   loop_count : DO
      CALL geth_newdate (next_date_char, current_date_char, interval_seconds )
      IF      ( next_date_char .LT. end_date_char ) THEN
         time_loop = time_loop + 1
         PRINT '(A,I4,A,A,A)','Time period #',time_loop,&
                              ' to process = ',next_date_char,'.'
         current_date_char = next_date_char
      ELSE IF ( next_date_char .EQ. end_date_char ) THEN
         time_loop = time_loop + 1
         PRINT '(A,I4,A,A,A)','Time period #',time_loop,&
                              ' to process = ',next_date_char,'.'
         PRINT '(A,I4,A)','Total analysis times to input = ',time_loop,'.'
         time_loop_max = time_loop
         EXIT loop_count
      ELSE IF ( next_date_char .GT. end_date_char ) THEN
         PRINT '(A,I4,A)','Total analysis times to input = ',time_loop,'.'
         time_loop_max = time_loop
         EXIT loop_count
      END IF
   END DO loop_count
	write(0,*) 'done in si_start_and_end'
END SUBROUTINE compute_si_start_and_end

SUBROUTINE assemble_output ( grid , config_flags , loop , time_loop_max )

!!! replace with something?   USE module_big_step_utilities_em

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
   INTEGER :: inc_h,inc_v
   INTEGER :: i , j , k , idts

   INTEGER :: id1 , interval_seconds , ierr, rc
   INTEGER , SAVE :: id 
   CHARACTER (LEN=80) :: inpname , bdyname
   CHARACTER(LEN= 4) :: loop_char
character *19 :: temp19
character *24 :: temp24 , temp24b

   REAL, DIMENSION(:,:,:), ALLOCATABLE, SAVE :: ubdy3dtemp1 , vbdy3dtemp1 ,&
                                                tbdy3dtemp1 , &
				                cwmbdy3dtemp1 , qbdy3dtemp1,&
                                                q2bdy3dtemp1 , pdbdy2dtemp1
   REAL, DIMENSION(:,:,:), ALLOCATABLE, SAVE :: ubdy3dtemp2 , vbdy3dtemp2 , &
                                                tbdy3dtemp2 , & 
                                                cwmbdy3dtemp2 , qbdy3dtemp2, &
                                                q2bdy3dtemp2, pdbdy2dtemp2
   REAL :: t1,t2

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

#include "deref_kludge.h"


   !  Various sizes that we need to be concerned about.

   ids = grid%sd31
   ide = grid%ed31-1 ! 030730tst
   kds = grid%sd32
   kde = grid%ed32-1 ! 030730tst
   jds = grid%sd33
   jde = grid%ed33-1 ! 030730tst

   ims = grid%sm31
   ime = grid%em31
   kms = grid%sm32
   kme = grid%em32
   jms = grid%sm33
   jme = grid%em33

   ips = grid%sp31
   ipe = grid%ep31-1 ! 030730tst
   kps = grid%sp32
   kpe = grid%ep32-1 ! 030730tst
   jps = grid%sp33
   jpe = grid%ep33-1 ! 030730tst

!!!!!!! believe IPE and JPE should be larger if they aren't on the global
!!!!!!! boundary

        if (IPE .ne. IDE) IPE=IPE+1
        if (JPE .ne. JDE) JPE=JPE+1

	write(0,*) 'assemble output (ids,ide): ', ids,ide
	write(0,*) 'assemble output (ims,ime): ', ims,ime
	write(0,*) 'assemble output (ips,ipe): ', ips,ipe

	write(0,*) 'assemble output (jds,jde): ', jds,jde
	write(0,*) 'assemble output (jms,jme): ', jms,jme
	write(0,*) 'assemble output (jps,jpe): ', jps,jpe

	write(0,*) 'assemble output (kds,kde): ', kds,kde
	write(0,*) 'assemble output (kms,kme): ', kms,kme
	write(0,*) 'assemble output (kps,kpe): ', kps,kpe

   ijds = MIN ( ids , jds )
!mptest030805   ijde = MAX ( ide , jde )
   ijde = MAX ( ide , jde ) + 1   ! to make stuff_bdy dimensions consistent with alloc

   !  Boundary width, scalar value.

   spec_bdy_width = model_config_rec%spec_bdy_width
   interval_seconds = model_config_rec%interval_seconds

      write(0,*)' in assemble_ouput loop=',loop
!-----------------------------------------------------------------------
!
   main_loop_test: IF ( loop .EQ. 1 ) THEN
!
!-----------------------------------------------------------------------

   !  This is the space needed to save the current 3d data for use in computing
   !  the lateral boundary tendencies.

	write(0,*) 'allocating 3d arrays passed into stuff_bdy with vert lims: ', &
                  kms, kme
      ALLOCATE ( ubdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( vbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( tbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( qbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( cwmbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( q2bdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( pdbdy2dtemp1(ims:ime,  1:1  ,jms:jme) )

	ubdy3dtemp1=0.
	vbdy3dtemp1=0.
	tbdy3dtemp1=0.
	qbdy3dtemp1=0.
	cwmbdy3dtemp1=0.
	q2bdy3dtemp1=0.
	pdbdy2dtemp1=0.

      ALLOCATE ( ubdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( vbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( tbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( qbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( cwmbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( q2bdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( pdbdy2dtemp2(ims:ime,  1:1  ,jms:jme) )

	ubdy3dtemp2=0.
	vbdy3dtemp2=0.
	tbdy3dtemp2=0.
	qbdy3dtemp2=0.
	cwmbdy3dtemp2=0.
	q2bdy3dtemp2=0.
	pdbdy2dtemp2=0.

      !  Open the wrfinput file.  From this program, this is an *output* file.

      CALL construct_filename1( inpname , 'wrfinput' , grid%id , 2 )

      CALL open_w_dataset ( id1, TRIM(inpname) , grid , config_flags , &
                            output_model_input , "DATASET=INPUT", ierr )

      IF ( ierr .NE. 0 ) THEN
      CALL wrf_error_fatal( 'real: error opening wrfinput for writing' )
      ENDIF

!     CALL calc_current_date ( grid%id , 0. )
!      grid%write_metadata = .true.

	write(0,*) 'making call to output_model_input'

        CALL output_model_input ( id1, grid , config_flags , ierr )
	write(0,*) 'ierr from output_model_input: ', ierr

!***
!***  CLOSE THE WRFINPUT DATASET
!***
      CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

      !  We need to save the 3d data to compute a 
      !  difference during the next loop. 

   write(0,*) 'I,J,K lims: ', MIN(ide,ipe), MIN(jde,jpe), MIN(kde,kpe)
!
!-----------------------------------------------------------------------
!***  SOUTHERN BOUNDARY
!-----------------------------------------------------------------------
!

        IF(JPS==JDS)THEN
          J=1
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp1(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp1(i,k,j) = grid%nmm_v(i,k,j)
            tbdy3dtemp1(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp1(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp1(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp1(i,k,j) = grid%nmm_q2(i,k,j)
          END DO
          END DO

          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp1(i,1,j) = grid%nmm_pd(i,j)
          END DO
        ENDIF

!
!-----------------------------------------------------------------------
!***  NORTHERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(JPE==JDE)THEN
          J=MIN(JDE,JPE)
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp1(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp1(i,k,j) = grid%nmm_v(i,k,j)
            tbdy3dtemp1(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp1(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp1(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp1(i,k,j) = grid%nmm_q2(i,k,j)
          END DO
          END DO

          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp1(i,1,j) = grid%nmm_pd(i,j)
          END DO
        ENDIF

!
!-----------------------------------------------------------------------
!***  WESTERN BOUNDARY
!-----------------------------------------------------------------------
!
	write(0,*) 'western boundary, store winds over J: ', jps, min(jpe,jde)
        IF(IPS==IDS)THEN
          I=1
          DO k = kps , MIN(kde,kpe)
          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, min(jde,jpe),2

        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp1(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp1(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp1(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp1(i,k,j) = grid%nmm_q2(i,k,j)
      if(k==1)then
        write(0,*)' loop=',loop,' i=',i,' j=',j,' tbdy3dtemp1(i,k,j)=',tbdy3dtemp1(i,k,j)
      endif
	endif
          END DO
          END DO

          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, min(jde,jpe),2
        if (J .ge. 2 .and. J .le. JDE-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp1(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp1(i,k,j) = grid%nmm_v(i,k,j)
	endif
          END DO
          END DO
!
          inc_h=mod(jps+1,2)
        DO j = jps+inc_h, min(jde,jpe),2
        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            pdbdy2dtemp1(i,1,j) = grid%nmm_pd(i,j)
      write(0,*)' loop=',loop,' i=',i,' j=',j,' pdbdy2dtemp1(i,1,j)=',pdbdy2dtemp1(i,1,j)
	endif
          END DO
        ENDIF
!
!-----------------------------------------------------------------------
!***  EASTERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(IPE==IDE)THEN
          I=MIN(IDE,IPE)
!
          DO k = kps , MIN(kde,kpe)
!
!***   Make sure the J loop is on the global boundary
!
          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, min(jde,jpe),2
        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp1(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp1(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp1(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp1(i,k,j) = grid%nmm_q2(i,k,j)
	endif
          END DO
          END DO

          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, min(jde,jpe),2
        if (J .ge. 2 .and. J .le. JDE-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp1(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp1(i,k,j) = grid%nmm_v(i,k,j)
        endif
          END DO
          END DO
!
          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, min(jde,jpe),2
        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            pdbdy2dtemp1(i,1,j) = grid%nmm_pd(i,j)
	endif
          END DO
        ENDIF


      !  There are 2 components to the lateral boundaries.  
      !  First, there is the starting
      !  point of this time period - just the outer few rows and columns.


 CALL stuff_bdy (ubdy3dtemp1, grid%nmm_u_b, 'N', ijds, ijde, spec_bdy_width  , &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )
 CALL stuff_bdy ( vbdy3dtemp1, grid%nmm_v_b, 'N', ijds, ijde, spec_bdy_width, &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )
	write(0,*) 'size (dim 1) nmm_t_b: ', size(grid%nmm_t_b, dim=1)
	write(0,*) 'size (dim 2) nmm_t_b: ', size(grid%nmm_t_b, dim=2)
	write(0,*) 'size (dim 3) nmm_t_b: ', size(grid%nmm_t_b, dim=3)
	write(0,*) 'size (dim 4) nmm_t_b: ', size(grid%nmm_t_b, dim=4)

 CALL stuff_bdy ( tbdy3dtemp1, grid%nmm_t_b, 'N', ijds, ijde, spec_bdy_width, &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy ( cwmbdy3dtemp1,grid%nmm_cwm_b,'N',ijds,ijde, spec_bdy_width, &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy ( qbdy3dtemp1, grid%nmm_q_b, 'N', ijds, ijde, spec_bdy_width, &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy ( q2bdy3dtemp1,grid%nmm_q2_b,'N', ijds, ijde, spec_bdy_width, &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

	write(0,*) 'stuff_bdy lims for pdbdy I: ', ids, ide+1
	write(0,*) 'stuff_bdy lims for pdbdy J: ', jds, jde+1

 CALL stuff_bdy ( pdbdy2dtemp1,grid%nmm_pd_b,'M', ijds,ijde, spec_bdy_width, &
                                        ids , ide+1 , jds , jde+1 , 1 , 1 , &
                                        ims , ime , jms , jme , 1 , 1 , &
                                        ips , ipe , jps , jpe , 1 , 1 )

!-----------------------------------------------------------------------
!
   ELSE IF ( loop .GT. 1 ) THEN
!
!-----------------------------------------------------------------------

      write(0,*)' assemble_ouput loop=',loop,' in IF block'
      !  Open the boundary file.

      IF ( loop .eq. 2 ) THEN
         CALL construct_filename1( bdyname , 'wrfbdy' , grid%id , 2 )
      CALL open_w_dataset ( id, TRIM(bdyname) , grid , config_flags , &
                          output_boundary , "DATASET=BOUNDARY", ierr )
         IF ( ierr .NE. 0 ) THEN
               CALL wrf_error_fatal( 'real: error opening wrfbdy for writing' )
         ENDIF
!         grid%write_metadata = .true.
      ELSE
! what's this do?
!         grid%write_metadata = .true.
!         grid%write_metadata = .false.
         CALL domain_clockadvance( grid )
      END IF

      write(0,*)' assemble_ouput loop=',loop,' point 2'

!
!-----------------------------------------------------------------------
!***  SOUTHERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(JPS==JDS)THEN
          J=1
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp2(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp2(i,k,j) = grid%nmm_v(i,k,j)
            tbdy3dtemp2(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp2(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp2(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp2(i,k,j) = grid%nmm_q2(i,k,j)
          END DO
          END DO
!
          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp2(i,1,j) = grid%nmm_pd(i,j)
          END DO
        ENDIF

!
!-----------------------------------------------------------------------
!***  NORTHERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(JPE==JDE)THEN
          J=MIN(JDE,JPE)
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp2(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp2(i,k,j) = grid%nmm_v(i,k,j)
            tbdy3dtemp2(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp2(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp2(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp2(i,k,j) = grid%nmm_q2(i,k,j)
          END DO
          END DO

          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp2(i,1,j) = grid%nmm_pd(i,j)
          END DO
        ENDIF
!
!-----------------------------------------------------------------------
!***  WESTERN BOUNDARY
!-----------------------------------------------------------------------
!
      write(0,*)' assemble_ouput loop=',loop,' point 3 ips=',ips,' ids=',ids
        IF(IPS==IDS)THEN
          I=1
          DO k = kps , MIN(kde,kpe)
          inc_h=mod(jps+1,2)
      if(k==1)then
        write(0,*)' assemble_ouput loop=',loop,' inc_h=',inc_h,' jps=',jps
      endif
          DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp2(i,k,j) = grid%nmm_t(i,k,j)
      if(k==1)then
        write(0,*)' loop=',loop,' i=',i,' j=',j,' tbdy3dtemp1(i,k,j)=',tbdy3dtemp1(i,k,j)
      endif
            qbdy3dtemp2(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp2(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp2(i,k,j) = grid%nmm_q2(i,k,j)
	endif
          END DO
          END DO
!
          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, MIN(jde,jpe),2
        if (J .ge. 2 .and. J .le. jde-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp2(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp2(i,k,j) = grid%nmm_v(i,k,j)
	endif
          END DO
          END DO

	write(0,*) 'western boundary pdbdy J lims: ',  jps, min(jde,jpe)
          inc_h=mod(jps+1,2)
        DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
            pdbdy2dtemp2(i,1,j) = grid%nmm_pd(i,j)
      write(0,*)' loop=',loop,' i=',i,' j=',j,' pdbdy2dtemp1(i,1,j)=',pdbdy2dtemp1(i,1,j)
	endif
          END DO
        ENDIF
!
!-----------------------------------------------------------------------
!***  EASTERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(IPE==IDE)THEN
          I=MIN(IDE,IPE)

          DO k = kps , MIN(kde,kpe)
          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp2(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp2(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp2(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp2(i,k,j) = grid%nmm_q2(i,k,j)
	endif
          END DO
          END DO

          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, MIN(jde,jpe),2
        if (J .ge. 2 .and. J .le. jde-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp2(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp2(i,k,j) = grid%nmm_v(i,k,j)
	endif
          END DO
          END DO

          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
            pdbdy2dtemp2(i,1,j) = grid%nmm_pd(i,j)
	endif
          END DO
        ENDIF
!-----------------------------------------------------------------------
      !  During all of the loops after the first loop, 
      !  we first compute the boundary
      !  tendencies with the current data values 
      !  (*bdy3dtemp2 arrays) and the previously 
      !  saved information stored in the *bdy3dtemp1 arrays.


      CALL stuff_bdytend ( ubdy3dtemp2 , ubdy3dtemp1 , REAL(interval_seconds),&
                                   grid%nmm_u_bt  , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )
      CALL stuff_bdytend ( vbdy3dtemp2 , vbdy3dtemp1 , REAL(interval_seconds),&
                                    grid%nmm_v_bt  , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )
      CALL stuff_bdytend ( tbdy3dtemp2 , tbdy3dtemp1 , REAL(interval_seconds),&
                                   grid%nmm_t_bt  , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend ( cwmbdy3dtemp2,cwmbdy3dtemp1,REAL(interval_seconds),&
                                   grid%nmm_cwm_bt  , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend ( qbdy3dtemp2 , qbdy3dtemp1 , REAL(interval_seconds),&
                                   grid%nmm_q_bt , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

    CALL stuff_bdytend ( q2bdy3dtemp2, q2bdy3dtemp1 , REAL(interval_seconds),&
                                   grid%nmm_q2_bt , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

       if(jps==jds.and.ips==ids) write(0,*) 'pdbdy2dtemp2(1,1,1): ', pdbdy2dtemp2(1,1,1)

	write(0,*) 'stuff_bdytend lims for pdbdy I: ', ids, ide+1
	write(0,*) 'stuff_bdytend lims for pdbdy J: ', jds, jde+1

    CALL stuff_bdytend( pdbdy2dtemp2 , pdbdy2dtemp1, REAL(interval_seconds),&
                                   grid%nmm_pd_bt  , 'M' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , 1 , 1 , &
                                   ims , ime , jms , jme , 1 , 1 , &
                                   ips , ipe , jps , jpe , 1 , 1 )

       write(0,*) 'grid%nmm_pd_bt(1,1): ', grid%nmm_pd_bt(1,1,1,1)

      !  Both pieces of the boundary data are now 
      !  available to be written (initial time and tendency).
      !  This looks ugly, these date shifting things.  
      !  What's it for?  We want the "Times" variable
      !  in the lateral BDY file to have the valid times 
      !  of when the initial fields are written.
      !  That's what the loop-2 thingy is for with the start date.  
      !  We increment the start_date so
      !  that the starting time in the attributes is the 
      !  second time period.  Why you may ask.  I
      !  agree, why indeed.

      temp24= current_date
      temp24b=start_date
      start_date = current_date
      CALL geth_newdate ( temp19 , temp24b(1:19) , &
                         (loop-2) * model_config_rec%interval_seconds )
      current_date = temp19 //  '.0000'
       CALL domain_clock_set( grid, current_date(1:19) )
 print *,'LBC valid between these times ',current_date, ' ',start_date

      CALL output_boundary ( id, grid , config_flags , ierr )
      current_date = temp24
      start_date = temp24b

      !  OK, for all of the loops, we output the initialzation 
      !  data, which would allow us to
      !  start the model at any of the available analysis time periods.

!  WRITE ( loop_char , FMT = '(I4.4)' ) loop
!  CALL open_w_dataset ( id1, 'wrfinput'//loop_char , grid , config_flags , output_model_input , "DATASET=INPUT", ierr )
!  IF ( ierr .NE. 0 ) THEN
!    CALL wrf_error_fatal( 'real: error opening wrfinput'//loop_char//' for writing' )
!  ENDIF
!  grid%write_metadata = .true.

!  CALL calc_current_date ( grid%id , 0. )
!  CALL output_model_input ( id1, grid , config_flags , ierr )
!  CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

  !  Is this or is this not the last time time?  We can remove some unnecessary
  !  stores if it is not.

      IF     ( loop .LT. time_loop_max ) THEN

         !  We need to save the 3d data to compute a 
         !  difference during the next loop.  Couple the
         !  3d fields with total mu (mub + mu_2) and the 
         !  stagger-specific map scale factor.
         !  We load up the boundary data again for use in the next loop.


!mp	change these limits?????????

	write(0,*) 'limits at end: ipe,jpe,kpe: ', ipe,jpe,kpe

         DO j = jps , jpe
            DO k = kps , kpe
               DO i = ips , ipe
                  ubdy3dtemp1(i,k,j) = ubdy3dtemp2(i,k,j)
                  vbdy3dtemp1(i,k,j) = vbdy3dtemp2(i,k,j)
                  tbdy3dtemp1(i,k,j) = tbdy3dtemp2(i,k,j)
                  cwmbdy3dtemp1(i,k,j) = cwmbdy3dtemp2(i,k,j)
                  qbdy3dtemp1(i,k,j) = qbdy3dtemp2(i,k,j)
                  q2bdy3dtemp1(i,k,j) = q2bdy3dtemp2(i,k,j)
               END DO
            END DO
         END DO

!mp	change these limits?????????

         DO j = jps , jpe
            DO i = ips , ipe
               pdbdy2dtemp1(i,1,j) = pdbdy2dtemp2(i,1,j)
            END DO
         END DO

  !  There are 2 components to the lateral boundaries.  
  !   First, there is the starting
  !  point of this time period - just the outer few rows and columns.


         CALL stuff_bdy ( ubdy3dtemp1 , grid%nmm_u_b  , 'N' ,& 
                                        ijds , ijde , spec_bdy_width      , &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )
         CALL stuff_bdy ( vbdy3dtemp1 , grid%nmm_v_b  , 'N' , &
                                        ijds , ijde , spec_bdy_width      , &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )
         CALL stuff_bdy ( tbdy3dtemp1 , grid%nmm_t_b  , 'N' , &
                                        ijds , ijde , spec_bdy_width      , &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

         CALL stuff_bdy ( cwmbdy3dtemp1 , grid%nmm_cwm_b , 'N' , &
                                          ijds , ijde , spec_bdy_width      , &
                                          ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                          ims , ime , jms , jme , kms , kme , &
                                          ips , ipe , jps , jpe , kps , kpe+1 )

         CALL stuff_bdy ( qbdy3dtemp1 , grid%nmm_q_b , 'N' ,&
                                        ijds , ijde , spec_bdy_width      , &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

         CALL stuff_bdy ( q2bdy3dtemp1 , grid%nmm_q2_b, 'N' ,&
                                         ijds , ijde , spec_bdy_width      , &
                                         ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                         ims , ime , jms , jme , kms , kme , &
                                         ips , ipe , jps , jpe , kps , kpe+1 )

         CALL stuff_bdy ( pdbdy2dtemp1 , grid%nmm_pd_b , 'M' ,&
                                          ijds , ijde , spec_bdy_width  , &
                                          ids , ide+1 , jds , jde+1 , 1 , 1 , &
                                          ims , ime , jms , jme , 1 , 1 , &
                                          ips , ipe , jps , jpe , 1 , 1 )

            write(0,*) 'grid%nmm_pd_b(1,1): ', grid%nmm_pd_b(1,1,1,1)

      ELSE IF ( loop .EQ. time_loop_max ) THEN

    !  If this is the last time through here, we need to close the files.

         CALL close_dataset ( id , config_flags , "DATASET=BOUNDARY" )

      END IF

   END IF main_loop_test

END SUBROUTINE assemble_output
