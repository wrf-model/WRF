!WRF:DRIVER_LAYER:MAIN
!

PROGRAM ndown_em

   USE module_machine
   USE module_domain
   USE module_initialize
   USE module_integrate
   USE module_driver_constants
   USE module_configure
   USE module_io_domain
   USE esmf_mod

   USE module_timing
   USE module_wrf_error
#ifdef DM_PARALLEL
   USE module_dm
#endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!new for bc
   USE module_bc
   USE module_big_step_utilities_em
   USE module_get_file_names
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   IMPLICIT NONE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!new for bc
   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: i , j , k
   INTEGER :: time_loop_max , time_loop
   INTEGER :: total_time_sec , file_counter
   INTEGER :: julyr , julday , iswater , map_proj
   INTEGER :: icnt

   REAL    :: dt , new_bdy_frq
   REAL    :: gmt , cen_lat , cen_lon , dx , dy , truelat1 , truelat2 , moad_cen_lat , stand_lon

   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp1 , vbdy3dtemp1 , tbdy3dtemp1 , pbdy3dtemp1 , qbdy3dtemp1
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: mbdy2dtemp1
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp2 , vbdy3dtemp2 , tbdy3dtemp2 , pbdy3dtemp2 , qbdy3dtemp2
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: mbdy2dtemp2

   CHARACTER(LEN=19) :: start_date_char , current_date_char , end_date_char

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

   REAL    :: time
   INTEGER :: rc

   INTEGER :: loop , levels_to_process
   INTEGER , PARAMETER :: max_sanity_file_loop = 100

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain, parent_grid , nested_grid
   TYPE (domain)           :: dummy
   TYPE (grid_config_rec_type)              :: config_flags
   INTEGER                 :: number_at_same_level
   INTEGER                 :: time_step_begin_restart

   INTEGER :: max_dom , domain_id , fid , fido, fidb , oid , idum1 , idum2 , ierr
   INTEGER :: status_next_var
   INTEGER :: debug_level
   LOGICAL :: input_from_file , need_new_file
   CHARACTER (LEN=19) :: date_string

#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   INTEGER                 :: idsi
   CHARACTER (LEN=80)      :: inpname , outname , bdyname
   CHARACTER (LEN=80)      :: si_inpname
character *19 :: temp19
character *24 :: temp24 , temp24b
character(len=24) :: start_date_hold

   CHARACTER (LEN=80)      :: message
integer :: ii
   TYPE(ESMF_TimeInterval) :: time_interval

   !  Interface block for routine that passes pointers and needs to know that they
   !  are receiving pointers.

   INTERFACE

      SUBROUTINE med_interp_domain ( parent_grid , nested_grid )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: parent_grid , nested_grid
      END SUBROUTINE med_interp_domain

      SUBROUTINE Setup_Timekeeping( parent_grid )
         USE module_domain
         TYPE(domain), POINTER :: parent_grid
      END SUBROUTINE Setup_Timekeeping

   END INTERFACE
#if 0
call opngks
#endif

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "NDOWN_EM V2.0 PREPROCESSOR"

   !  Initialize the modules used by the WRF system.  Many of the CALLs made from the
   !  init_modules routine are NO-OPs.  Typical initializations are: the size of a 
   !  REAL, setting the file handles to a pre-use value, defining moisture and 
   !  chemistry indices, etc.

   CALL init_modules

   !  Get the NAMELIST data.  This is handled in the initial_config routine.  All of the
   !  NAMELIST input variables are assigned to the model_config_rec structure.  Below,
   !  note for parallel processing, only the monitor processor handles the raw Fortran
   !  I/O, and then broadcasts the info to each of the other nodes.

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

   !  And here is an instance of using the information in the NAMELIST.  

   CALL get_debug_level ( debug_level )
   CALL set_wrf_debug_level ( debug_level )

   !  Allocated and configure the mother domain.  Since we are in the nesting down
   !  mode, we know a) we got a nest, and b) we only got 1 nest.

   NULLIFY( null_domain )

   CALL       wrf_message ( program_name )
   CALL       wrf_debug ( 100 , 'wrf: calling alloc_and_configure_domain coarse ' )
   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   parent_grid => head_grid

   !  Set up time initializations.

   CALL Setup_Timekeeping ( parent_grid )
   CALL ESMF_TimeIntervalSet ( time_interval , S=model_config_rec%interval_seconds, rc=rc )
   CALL ESMF_ClockSetTimeStep ( head_grid%domain_clock , time_interval , rc=rc )

   CALL       wrf_debug ( 100 , 'wrf: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( parent_grid%id , idum1, idum2 )

   !  Initialize the I/O for WRF.

   CALL       wrf_debug ( 100 , 'wrf: calling init_wrfio' )
   CALL init_wrfio

   !  Some of the configuration values may have been modified from the initial READ
   !  of the NAMELIST, so we re-broadcast the configuration records.

#ifdef DM_PARALLEL
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
#endif

   !  We need to current and starting dates for the output files.  The times need to be incremented
   !  so that the lateral BC files are not overwritten.

   WRITE ( start_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           model_config_rec%start_year  (parent_grid%id) , &
           model_config_rec%start_month (parent_grid%id) , &
           model_config_rec%start_day   (parent_grid%id) , &
           model_config_rec%start_hour  (parent_grid%id) , &
           model_config_rec%start_minute(parent_grid%id) , &
           model_config_rec%start_second(parent_grid%id) 

   WRITE (   end_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           model_config_rec%  end_year  (parent_grid%id) , &
           model_config_rec%  end_month (parent_grid%id) , &
           model_config_rec%  end_day   (parent_grid%id) , &
           model_config_rec%  end_hour  (parent_grid%id) , &
           model_config_rec%  end_minute(parent_grid%id) , &
           model_config_rec%  end_second(parent_grid%id) 

   CALL geth_idts ( end_date_char , start_date_char , total_time_sec ) 

   new_bdy_frq = model_config_rec%interval_seconds
   time_loop_max = total_time_sec / model_config_rec%interval_seconds + 1

   start_date        = start_date_char // '.0000' 
   current_date      = start_date_char // '.0000' 
   start_date_hold   = start_date_char // '.0000'
   current_date_char = start_date_char

   !  Get a list of available file names to try.  This fills up the eligible_file_name
   !  array with number_of_eligible_files entries.  This routine issues a nonstandard
   !  call (system).

   file_counter = 1
   need_new_file = .FALSE.
   CALL init_module_get_file_names
   CALL unix_ls ( 'wrfout' , parent_grid%id )

   !  Open the input data (wrfout_d01_xxxxxx) for reading.
   
   CALL wrf_debug          ( 100 , 'ndown_em main: calling open_r_dataset for ' // TRIM(eligible_file_name(file_counter)) )
   CALL open_r_dataset     ( fid, TRIM(eligible_file_name(file_counter)) , head_grid , config_flags , "DATASET=INPUT", ierr )
   IF ( ierr .NE. 0 ) THEN
      WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(eligible_file_name(file_counter)), &
                                                  ' for reading ierr=',ierr
      CALL WRF_ERROR_FATAL ( wrf_err_message )
   ENDIF

   !  We know how many time periods to process, so we begin.

   big_time_loop_thingy : DO time_loop = 1 , time_loop_max

      !  Which date are we currently soliciting?

      CALL geth_newdate ( date_string , start_date_char , ( time_loop - 1 ) * NINT ( new_bdy_frq) )
print *,'-------->>>  Processing data: loop=',time_loop,'  date/time = ',date_string
      current_date_char = date_string
      current_date      = date_string // '.0000'
      start_date        = date_string // '.0000'
print *,'loopmax = ', time_loop_max, '   ending date = ',end_date_char
      CALL atotime( current_date(1:19), parent_grid%current_time )
      CALL ESMF_ClockSetCurrTime(parent_grid%domain_clock, parent_grid%current_time, rc)

      !  Which times are in this file, and more importantly, are any of them the
      !  ones that we want?  We need to loop over times in each files, loop
      !  over files.

      get_the_right_time : DO
      
         CALL ext_ncd_get_next_time ( fid , date_string , status_next_var )
print *,'file date/time = ',date_string,'     desired date = ',current_date_char,'     status = ', status_next_var

         IF      (  status_next_var .NE. 0 ) THEN
            CALL wrf_debug          ( 100 , 'ndown_em main: calling close_dataset  for ' // TRIM(eligible_file_name(file_counter)) )
            CALL close_dataset      ( fid , config_flags , "DATASET=INPUT" )
            file_counter = file_counter + 1
            IF ( file_counter .GT. number_of_eligible_files ) THEN
               WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: opening too many files'
               CALL WRF_ERROR_FATAL ( wrf_err_message )
            END IF
            CALL wrf_debug      ( 100 , 'ndown_em main: calling open_r_dataset for ' // TRIM(eligible_file_name(file_counter)) )
            CALL open_r_dataset ( fid, TRIM(eligible_file_name(file_counter)) , head_grid , config_flags , "DATASET=INPUT", ierr )
            IF ( ierr .NE. 0 ) THEN
               WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(eligible_file_name(file_counter)), &
                                                           ' for reading ierr=',ierr
               CALL WRF_ERROR_FATAL ( wrf_err_message )
            ENDIF
            CYCLE get_the_right_time
         ELSE IF ( TRIM(date_string) .LT. TRIM(current_date_char) ) THEN
            CYCLE get_the_right_time
         ELSE IF ( TRIM(date_string) .EQ. TRIM(current_date_char) ) THEN
            EXIT get_the_right_time
         ELSE IF ( TRIM(date_string) .GT. TRIM(current_date_char) ) THEN
            WRITE( wrf_err_message , FMT='(A,A,A,A,A)' ) 'Found ',TRIM(date_string),' before I found ',TRIM(current_date_char),'.'
            CALL WRF_ERROR_FATAL ( wrf_err_message )
         END IF
      END DO get_the_right_time 

      CALL wrf_debug          ( 100 , 'wrf: calling input_history' )
      CALL ext_ncd_get_previous_time ( fid , date_string , status_next_var )
      CALL input_history      ( fid , head_grid , config_flags, ierr )
      CALL wrf_debug          ( 100 , 'wrf: back from input_history' )

      !  Get the coarse grid info for later transfer to the fine grid domain.

      CALL wrf_get_dom_ti_integer ( fid , 'MAP_PROJ' , map_proj , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'DX'  , dx  , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'DY'  , dy  , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'CEN_LAT' , cen_lat , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'CEN_LON' , cen_lon , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'TRUELAT1' , truelat1 , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'TRUELAT2' , truelat2 , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'MOAD_CEN_LAT' , moad_cen_lat , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'STAND_LON' , stand_lon , 1 , icnt , ierr ) 
!     CALL wrf_get_dom_ti_real    ( fid , 'GMT' , gmt , 1 , icnt , ierr ) 
!     CALL wrf_get_dom_ti_integer ( fid , 'JULYR' , julyr , 1 , icnt , ierr ) 
!     CALL wrf_get_dom_ti_integer ( fid , 'JULDAY' , julday , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_integer ( fid , 'ISWATER' , iswater , 1 , icnt , ierr ) 

      !  First time in, do this: allocate sapce for the fine grid, get the config flags, open the 
      !  wrfinput and wrfbdy files.  This COULD be done outside the time loop, I think, so check it
      !  out and move it up if you can.

      IF ( time_loop .EQ. 1 ) THEN

         CALL       wrf_message ( program_name )
         CALL       wrf_debug ( 100 , 'wrf: calling alloc_and_configure_domain fine ' )
         CALL alloc_and_configure_domain ( domain_id  = 2 ,                  &
                                           grid       = nested_grid ,        &
                                           parent     = parent_grid ,        &
                                           kid        = 1                   )
   
         CALL       wrf_debug ( 100 , 'wrf: calling model_to_grid_config_rec ' )
         CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
         CALL       wrf_debug ( 100 , 'wrf: calling set_scalar_indices_from_config ' )
         CALL set_scalar_indices_from_config ( nested_grid%id , idum1, idum2 )

         !  Set up time initializations for the fine grid.

!        CALL Setup_Timekeeping ( nested_grid )
!        CALL ESMF_TimeIntervalSet ( time_interval , S=model_config_rec%interval_seconds, rc=rc )
!        CALL ESMF_ClockSetTimeStep ( nested_grid%domain_clock , time_interval , rc=rc )
         CALL atotime( current_date(1:19), nested_grid%current_time )
         CALL ESMF_ClockSetCurrTime(nested_grid%domain_clock, nested_grid%current_time, rc)
   
         !  Generate an output file from this program, which will be an input file to WRF.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! major kludge
print *,'kludge - stupid way to get 1d and consts into new grid'
nested_grid%em_fnm    = parent_grid%em_fnm
nested_grid%em_fnp    = parent_grid%em_fnp
nested_grid%em_rdnw   = parent_grid%em_rdnw
nested_grid%em_rdn    = parent_grid%em_rdn
nested_grid%em_dnw    = parent_grid%em_dnw
nested_grid%em_dn     = parent_grid%em_dn 
nested_grid%em_znu    = parent_grid%em_znu
nested_grid%em_znw    = parent_grid%em_znw

print *,'nested_grid%em_fnm    =', parent_grid%em_fnm
print *,'nested_grid%em_fnp    =', parent_grid%em_fnp
print *,'nested_grid%em_rdnw   =', parent_grid%em_rdnw
print *,'nested_grid%em_rdn    =', parent_grid%em_rdn
print *,'nested_grid%em_dnw    =', parent_grid%em_dnw
print *,'nested_grid%em_dn     =', parent_grid%em_dn 
print *,'nested_grid%em_znu    =', parent_grid%em_znu
print *,'nested_grid%em_znw    =', parent_grid%em_znw

nested_grid%zs        = parent_grid%zs
nested_grid%dzs       = parent_grid%dzs

nested_grid%p_top     = parent_grid%p_top
nested_grid%rdx       = parent_grid%rdx * 3.
nested_grid%rdy       = parent_grid%rdy * 3.
nested_grid%resm      = parent_grid%resm
nested_grid%zetatop   = parent_grid%zetatop
nested_grid%cf1       = parent_grid%cf1
nested_grid%cf2       = parent_grid%cf2
nested_grid%cf3       = parent_grid%cf3

print *,'nested_grid%zs        =', parent_grid%zs
print *,'nested_grid%dzs       =', parent_grid%dzs

print *,'nested_grid%p_top     =', parent_grid%p_top
print *,'nested_grid%rdx       =', parent_grid%rdx *3.
print *,'nested_grid%rdy       =', parent_grid%rdy * 3.
print *,'nested_grid%resm      =', parent_grid%resm
print *,'nested_grid%zetatop   =', parent_grid%zetatop
print *,'nested_grid%cf1       =', parent_grid%cf1
print *,'nested_grid%cf2       =', parent_grid%cf2
print *,'nested_grid%cf3       =', parent_grid%cf3

nested_grid%cfn       = parent_grid%cfn 
nested_grid%cfn1      = parent_grid%cfn1
nested_grid%epsts     = parent_grid%epsts

print *,'nested_grid%cfn       =', parent_grid%cfn 
print *,'nested_grid%cfn1      =', parent_grid%cfn1
print *,'nested_grid%epsts     =', parent_grid%epsts
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
         CALL wrf_debug          ( 100 , 'ndown_em main: calling open_w_dataset for wrfinput' )
         CALL construct_filename1( outname , 'wrfinput' , nested_grid%id , 2 )
         CALL open_w_dataset     ( fido, TRIM(outname) , nested_grid , config_flags , output_model_input , "DATASET=INPUT", ierr )
         IF ( ierr .NE. 0 ) THEN
            WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(outname),' for reading ierr=',ierr
            CALL WRF_ERROR_FATAL ( wrf_err_message )
         ENDIF

         !  Various sizes that we need to be concerned about.

         ids = nested_grid%sd31
         ide = nested_grid%ed31
         kds = nested_grid%sd32
         kde = nested_grid%ed32
         jds = nested_grid%sd33
         jde = nested_grid%ed33

         ims = nested_grid%sm31
         ime = nested_grid%em31
         kms = nested_grid%sm32
         kme = nested_grid%em32
         jms = nested_grid%sm33
         jme = nested_grid%em33

         ips = nested_grid%sp31
         ipe = nested_grid%ep31
         kps = nested_grid%sp32
         kpe = nested_grid%ep32
         jps = nested_grid%sp33
         jpe = nested_grid%ep33

         ijds = MIN ( ids , jds )
         ijde = MAX ( ide , jde )

         print *, ids , ide , jds , jde , kds , kde
         print *, ims , ime , jms , jme , kms , kme
         print *, ips , ipe , jps , jpe , kps , kpe
         print *, ijds , ijde

         spec_bdy_width = model_config_rec%spec_bdy_width
         print *,'spec_bdy_width=',spec_bdy_width

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

      END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF ( time_loop .EQ. 1 ) THEN

         !  Open the fine grid SI static file.
   
         CALL construct_filename2( si_inpname , 'wrf_real_input_em' , nested_grid%id , 2 , start_date_char )
         CALL       wrf_debug ( 100 , 'med_sidata_input: calling open_r_dataset for ' // TRIM(si_inpname) )
         CALL open_r_dataset ( idsi, TRIM(si_inpname) , nested_grid , config_flags , "DATASET=INPUT", ierr )
         IF ( ierr .NE. 0 ) THEN
            CALL wrf_error_fatal( 'real: error opening wrf_real_input_em for reading: ' // TRIM (si_inpname) )
         END IF
   
         !  Input data.
   
         CALL       wrf_debug ( 100 , 'ndown_em: calling input_aux_model_input2' )
         CALL input_aux_model_input2 ( idsi , nested_grid , config_flags , ierr )
         nested_grid%ht_fine = nested_grid%ht
   
         !  Close this fine grid static input file.
   
         CALL       wrf_debug ( 100 , 'ndown_em: closing fine grid static input' )
         CALL close_dataset ( idsi , config_flags , "DATASET=INPUT" )

         !  We need a fine grid landuse in the interpolation.  So we need to generate
         !  that field now.

         IF ( ( nested_grid% vegcat(ips,jps) .GT. 0.5 ) .AND. &
              ( nested_grid%soilcat(ips,jps) .GT. 0.5 ) ) THEN
            DO j = jps, MIN(jde-1,jpe)
               DO i = ips, MIN(ide-1,ipe)
                  nested_grid%ivgtyp(i,j) = NINT(nested_grid% vegcat(i,j))
                  nested_grid%isltyp(i,j) = NINT(nested_grid%soilcat(i,j))
               END DO
            END DO
         ELSE

            num_veg_cat      = SIZE ( nested_grid%landusef , DIM=2 )
            num_soil_top_cat = SIZE ( nested_grid%soilctop , DIM=2 )
            num_soil_bot_cat = SIZE ( nested_grid%soilcbot , DIM=2 )
   
            CALL land_percentages (  nested_grid%xland , &
                                     nested_grid%landusef , nested_grid%soilctop , nested_grid%soilcbot , &
                                     nested_grid%isltyp , nested_grid%ivgtyp , &
                                     num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     ips , ipe , jps , jpe , kps , kpe , &
                                     model_config_rec%iswater(nested_grid%id) )

          END IF

          DO j = jps, MIN(jde-1,jpe)
            DO i = ips, MIN(ide-1,ipe)
               nested_grid%lu_index(i,j) = nested_grid%ivgtyp(i,j)
            END DO
         END DO

         CALL check_consistency ( nested_grid%ivgtyp , nested_grid%isltyp , nested_grid%landmask , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe , &
                                  model_config_rec%iswater(nested_grid%id) )

      END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Do the interpolation.

      CALL med_interp_domain ( head_grid , nested_grid )

      !  We have both the interpolated fields and the higher-resolution static fields.  From these
      !  the rebalancing is now done.

      CALL rebalance_driver ( nested_grid ) 

      !  Different things happen during the different time loops:
      !      first loop - write wrfinput file, close data set, copy files to holder arrays
      !      middle loops - diff 3d/2d arrays, compute and output bc
      !      last loop - diff 3d/2d arrays, compute and output bc, write wrfbdy file, close wrfbdy file

      IF ( time_loop .EQ. 1 ) THEN

         !  Set the time info.

print *,'current_date = ',current_date
         CALL atotime( current_date(1:19), nested_grid%current_time )
         CALL ESMF_ClockSetCurrTime(nested_grid%domain_clock, nested_grid%current_time, rc)

         !  Output the first time period of the data.
   
         nested_grid%write_metadata = .TRUE.
         CALL output_model_input ( fido , nested_grid , config_flags , ierr )

         CALL wrf_put_dom_ti_integer ( fido , 'MAP_PROJ' , map_proj , 1 , ierr ) 
!        CALL wrf_put_dom_ti_real    ( fido , 'DX'  , dx  , 1 , ierr ) 
!        CALL wrf_put_dom_ti_real    ( fido , 'DY'  , dy  , 1 , ierr ) 
         CALL wrf_put_dom_ti_real    ( fido , 'CEN_LAT' , cen_lat , 1 , ierr ) 
         CALL wrf_put_dom_ti_real    ( fido , 'CEN_LON' , cen_lon , 1 , ierr ) 
         CALL wrf_put_dom_ti_real    ( fido , 'TRUELAT1' , truelat1 , 1 , ierr ) 
         CALL wrf_put_dom_ti_real    ( fido , 'TRUELAT2' , truelat2 , 1 , ierr ) 
         CALL wrf_put_dom_ti_real    ( fido , 'MOAD_CEN_LAT' , moad_cen_lat , 1 , ierr ) 
         CALL wrf_put_dom_ti_real    ( fido , 'STAND_LON' , stand_lon , 1 , ierr ) 
         CALL wrf_put_dom_ti_integer ( fido , 'ISWATER' , iswater , 1 , ierr ) 

         !  These change if the initial time for the nest is not the same as the
         !  first time period in the WRF output file.
         !  Now that we know the starting date, we need to set the GMT, JULYR, and JULDAY
         !  values for the global attributes.  This call is based on the setting of the 
         !  current_date string.

         CALL geth_julgmt ( julyr , julday , gmt)
         CALL set_julyr  ( nested_grid%id , julyr  )
         CALL set_julday ( nested_grid%id , julday )
         CALL set_gmt    ( nested_grid%id , gmt    )
         CALL wrf_put_dom_ti_real    ( fido , 'GMT' , gmt , 1 , ierr ) 
         CALL wrf_put_dom_ti_integer ( fido , 'JULYR' , julyr , 1 , ierr ) 
         CALL wrf_put_dom_ti_integer ( fido , 'JULDAY' , julday , 1 , ierr ) 
print *,'current_date =',current_date
print *,'julyr=',julyr
print *,'julday=',julday
print *,'gmt=',gmt
         
         !  Close the input (wrfout_d01_000000, for example) file.  That's right, the 
         !  input is an output file.  Who'd've thunk.
   
         CALL close_dataset      ( fido , config_flags , "DATASET=INPUT" )

         !  We need to save the 3d/2d data to compute a difference during the next loop.  Couple the
         !  3d fields with total mu (mub + mu_2) and the stagger-specific map scale factor.

         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , ubdy3dtemp1 , nested_grid%em_u_2                 , &
                       'u' , nested_grid%msfu , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , vbdy3dtemp1 , nested_grid%em_v_2                 , &
                       'v' , nested_grid%msfv , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , tbdy3dtemp1 , nested_grid%em_t_2                 , &
                       't' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , pbdy3dtemp1 , nested_grid%em_ph_2                , &
                       'h' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , qbdy3dtemp1 , nested_grid%moist_2(:,:,:,P_QV)    , &
                       't' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )

          DO j = jps , jpe
             DO i = ips , ipe
                mbdy2dtemp1(i,1,j) = nested_grid%em_mu_2(i,j)
             END DO
          END DO

         !  There are 2 components to the lateral boundaries.  First, there is the starting
         !  point of this time period - just the outer few rows and columns.

         CALL stuff_bdy     ( ubdy3dtemp1 , nested_grid%em_u_b     , 'U' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( vbdy3dtemp1 , nested_grid%em_v_b     , 'V' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( tbdy3dtemp1 , nested_grid%em_t_b     , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( pbdy3dtemp1 , nested_grid%em_ph_b    , 'W' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( qbdy3dtemp1 , nested_grid%em_rqv_b   , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( mbdy2dtemp1 , nested_grid%em_mu_b    , 'M' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , 1 , 1 , &
                                                                           ims , ime , jms , jme , 1 , 1 , &
                                                                           ips , ipe , jps , jpe , 1 , 1 )

      ELSE IF ( ( time_loop .GT. 1 ) .AND. ( time_loop .LT. time_loop_max ) ) THEN

         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , ubdy3dtemp2 , nested_grid%em_u_2                 , &
                       'u' , nested_grid%msfu , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , vbdy3dtemp2 , nested_grid%em_v_2                 , &
                       'v' , nested_grid%msfv , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , tbdy3dtemp2 , nested_grid%em_t_2                 , &
                       't' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , pbdy3dtemp2 , nested_grid%em_ph_2                , &
                       'h' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , qbdy3dtemp2 , nested_grid%moist_2(:,:,:,P_QV)    , &
                       't' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )

          DO j = jps , jpe
             DO i = ips , ipe
                mbdy2dtemp2(i,1,j) = nested_grid%em_mu_2(i,j)
             END DO
          END DO

         !  During all of the loops after the first loop, we first compute the boundary
         !  tendencies with the current data values and the previously save information
         !  stored in the *bdy3dtemp1 arrays.

         CALL stuff_bdytend ( ubdy3dtemp2 , ubdy3dtemp1 , new_bdy_frq , nested_grid%em_u_bt  , 'U'  , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( vbdy3dtemp2 , vbdy3dtemp1 , new_bdy_frq , nested_grid%em_v_bt  , 'V'  , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( tbdy3dtemp2 , tbdy3dtemp1 , new_bdy_frq , nested_grid%em_t_bt  , 'T'  , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( pbdy3dtemp2 , pbdy3dtemp1 , new_bdy_frq , nested_grid%em_ph_bt  , 'W' , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( qbdy3dtemp2 , qbdy3dtemp1 , new_bdy_frq , nested_grid%em_rqv_bt , 'T' , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( mbdy2dtemp2 , mbdy2dtemp1 , new_bdy_frq , nested_grid%em_mu_bt  , 'M' , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , 1 , 1 , &
                                                                  ims , ime , jms , jme , 1 , 1 , &
                                                                  ips , ipe , jps , jpe , 1 , 1 )
         IF ( time_loop .EQ. 2 ) THEN
   
            !  Generate an output file from this program, which will be an input file to WRF.

            nested_grid%write_metadata = .TRUE.
   
            CALL wrf_debug          ( 100 , 'ndown_em main: calling open_w_dataset for wrfbdy' )
            CALL construct_filename1( bdyname , 'wrfbdy' , nested_grid%id , 2 )
            CALL open_w_dataset     ( fidb, TRIM(bdyname) , nested_grid , config_flags , output_boundary , &
                                      "DATASET=BOUNDARY", ierr )
            IF ( ierr .NE. 0 ) THEN
               WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(bdyname),' for reading ierr=',ierr
               CALL WRF_ERROR_FATAL ( wrf_err_message )
            ENDIF

         ELSE

            nested_grid%write_metadata = .FALSE.

         END IF

         !  Both pieces of the boundary data are now available to be written.
         
      CALL atotime( current_date(1:19), nested_grid%current_time )
      CALL ESMF_ClockSetCurrTime(nested_grid%domain_clock, nested_grid%current_time, rc)
      temp24= current_date
      temp24b=start_date_hold
      start_date = start_date_hold
      CALL geth_newdate ( temp19 , temp24b(1:19) , (time_loop-2) * model_config_rec%interval_seconds )
      current_date = temp19 //  '.0000'
      CALL geth_julgmt ( julyr , julday , gmt)
      CALL set_julyr  ( nested_grid%id , julyr  )
      CALL set_julday ( nested_grid%id , julday )
      CALL set_gmt    ( nested_grid%id , gmt    )
      CALL wrf_put_dom_ti_real    ( fidb , 'GMT' , gmt , 1 , ierr ) 
      CALL wrf_put_dom_ti_integer ( fidb , 'JULYR' , julyr , 1 , ierr ) 
      CALL wrf_put_dom_ti_integer ( fidb , 'JULDAY' , julday , 1 , ierr ) 
      CALL atotime( current_date(1:19), nested_grid%current_time )
      CALL ESMF_ClockSetCurrTime(nested_grid%domain_clock, nested_grid%current_time, rc)
print *,'bdy time = ',time_loop-1,'  bdy date = ',current_date,' ',start_date
      CALL output_boundary ( fidb , nested_grid , config_flags , ierr )
      current_date = temp24
      start_date = temp24b
      CALL atotime( current_date(1:19), nested_grid%current_time )
      CALL ESMF_ClockSetCurrTime(nested_grid%domain_clock, nested_grid%current_time, rc)

         IF ( time_loop .EQ. 2 ) THEN
            CALL wrf_put_dom_ti_real    ( fidb , 'BDYFRQ' , new_bdy_frq , 1 , ierr ) 
         END IF

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

         CALL stuff_bdy     ( ubdy3dtemp1 , nested_grid%em_u_b     , 'U' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( vbdy3dtemp1 , nested_grid%em_v_b     , 'V' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( tbdy3dtemp1 , nested_grid%em_t_b     , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( pbdy3dtemp1 , nested_grid%em_ph_b    , 'W' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( qbdy3dtemp1 , nested_grid%em_rqv_b   , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( mbdy2dtemp1 , nested_grid%em_mu_b    , 'M' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , 1 , 1 , &
                                                                           ims , ime , jms , jme , 1 , 1 , &
                                                                           ips , ipe , jps , jpe , 1 , 1 )

      ELSE IF ( time_loop .EQ. time_loop_max ) THEN

         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , ubdy3dtemp2 , nested_grid%em_u_2                 , &
                       'u' , nested_grid%msfu , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , vbdy3dtemp2 , nested_grid%em_v_2                 , &
                       'v' , nested_grid%msfv , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , tbdy3dtemp2 , nested_grid%em_t_2                 , &
                       't' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , pbdy3dtemp2 , nested_grid%em_ph_2                , &
                       'h' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , qbdy3dtemp2 , nested_grid%moist_2(:,:,:,P_QV)    , &
                       't' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         mbdy2dtemp2(:,1,:) = nested_grid%em_mu_2(:,:)

         !  During all of the loops after the first loop, we first compute the boundary
         !  tendencies with the current data values and the previously save information
         !  stored in the *bdy3dtemp1 arrays.

         CALL stuff_bdytend ( ubdy3dtemp2 , ubdy3dtemp1 , new_bdy_frq , nested_grid%em_u_bt  , 'U'  , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( vbdy3dtemp2 , vbdy3dtemp1 , new_bdy_frq , nested_grid%em_v_bt  , 'V'  , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( tbdy3dtemp2 , tbdy3dtemp1 , new_bdy_frq , nested_grid%em_t_bt  , 'T'  , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( pbdy3dtemp2 , pbdy3dtemp1 , new_bdy_frq , nested_grid%em_ph_bt  , 'W' , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( qbdy3dtemp2 , qbdy3dtemp1 , new_bdy_frq , nested_grid%em_rqv_bt , 'T' , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( mbdy2dtemp2 , mbdy2dtemp1 , new_bdy_frq , nested_grid%em_mu_bt  , 'M' , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , 1 , 1 , &
                                                                  ims , ime , jms , jme , 1 , 1 , &
                                                                  ips , ipe , jps , jpe , 1 , 1 )

         IF ( time_loop .EQ. 2 ) THEN
   
            !  Generate an output file from this program, which will be an input file to WRF.

            nested_grid%write_metadata = .TRUE.
   
            CALL wrf_debug          ( 100 , 'ndown_em main: calling open_w_dataset for wrfbdy' )
            CALL construct_filename1( bdyname , 'wrfbdy' , nested_grid%id , 2 )
            CALL open_w_dataset     ( fidb, TRIM(bdyname) , nested_grid , config_flags , output_boundary , &
                                      "DATASET=BOUNDARY", ierr )
            IF ( ierr .NE. 0 ) THEN
               WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(bdyname),' for reading ierr=',ierr
               CALL WRF_ERROR_FATAL ( wrf_err_message )
            ENDIF

         ELSE

            nested_grid%write_metadata = .FALSE.

         END IF

         !  Both pieces of the boundary data are now available to be written.

      CALL atotime( current_date(1:19), nested_grid%current_time )
      CALL ESMF_ClockSetCurrTime(nested_grid%domain_clock, nested_grid%current_time, rc)
      temp24= current_date
      temp24b=start_date_hold
      start_date = start_date_hold
      CALL geth_newdate ( temp19 , temp24b(1:19) , (time_loop-2) * model_config_rec%interval_seconds )
      current_date = temp19 //  '.0000'
      CALL geth_julgmt ( julyr , julday , gmt)
      CALL set_julyr  ( nested_grid%id , julyr  )
      CALL set_julday ( nested_grid%id , julday )
      CALL set_gmt    ( nested_grid%id , gmt    )
      CALL wrf_put_dom_ti_real    ( fidb , 'GMT' , gmt , 1 , ierr ) 
      CALL wrf_put_dom_ti_integer ( fidb , 'JULYR' , julyr , 1 , ierr ) 
      CALL wrf_put_dom_ti_integer ( fidb , 'JULDAY' , julday , 1 , ierr ) 
      CALL atotime( current_date(1:19), nested_grid%current_time )
      CALL ESMF_ClockSetCurrTime(nested_grid%domain_clock, nested_grid%current_time, rc)
print *,'bdy time = ',time_loop-1,'  bdy date = ',current_date,' ',start_date
      CALL output_boundary ( fidb , nested_grid , config_flags , ierr )
      current_date = temp24
      start_date = temp24b
      CALL atotime( current_date(1:19), nested_grid%current_time )
      CALL ESMF_ClockSetCurrTime(nested_grid%domain_clock, nested_grid%current_time, rc)

         IF ( time_loop .EQ. 2 ) THEN
            CALL wrf_put_dom_ti_real    ( fidb , 'BDYFRQ' , new_bdy_frq , 1 , ierr ) 
         END IF

         !  Since this is the last time through here, we need to close the boundary file.

         CALL close_dataset ( fidb , config_flags , "DATASET=BOUNDARY" )


      END IF

      !  Process which time now?

   END DO big_time_loop_thingy

END PROGRAM ndown_em

SUBROUTINE land_percentages ( xland , &
                              landuse_frac , soil_top_cat , soil_bot_cat , &
                              isltyp , ivgtyp , &
                              num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater )
   USE module_soil_pre

   IMPLICIT NONE

   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte , &
                           iswater

   INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat
   REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landuse_frac
   REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(IN):: soil_top_cat
   REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(IN):: soil_bot_cat
   INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
   REAL , DIMENSION(ims:ime,jms:jme) , INTENT(OUT) :: xland

   CALL process_percent_cat_new ( xland , &
                                  landuse_frac , soil_top_cat , soil_bot_cat , &
                                  isltyp , ivgtyp , &
                                  num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte , &
                                  iswater )

END SUBROUTINE land_percentages

SUBROUTINE check_consistency ( ivgtyp , isltyp , landmask , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte , &
                                  iswater )

   IMPLICIT NONE

   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte , &
                           iswater
   INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: isltyp , ivgtyp
   REAL    , DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: landmask

   LOGICAL :: oops
   INTEGER :: oops_count , i , j

   oops = .FALSE.
   oops_count = 0

   DO j = jts, MIN(jde-1,jte)
      DO i = its, MIN(ide-1,ite)
         IF ( ( ( landmask(i,j) .LT. 0.5 ) .AND. ( ivgtyp(i,j) .NE. iswater ) ) .OR. &
              ( ( landmask(i,j) .GT. 0.5 ) .AND. ( ivgtyp(i,j) .EQ. iswater ) ) ) THEN
            print *,'mismatch in landmask and veg type'
            print *,'i,j=',i,j, '  landmask =',NINT(landmask(i,j)),'  ivgtyp=',ivgtyp(i,j)
            oops = .TRUE.
            oops_count = oops_count + 1
landmask(i,j) = 0
ivgtyp(i,j)=16
isltyp(i,j)=14
         END IF
      END DO
   END DO

   IF ( oops ) THEN
      print *,'DAVE mismatch in',oops_count,' locations'
!           STOP
   END IF

END SUBROUTINE check_consistency
