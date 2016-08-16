!WRF:DRIVER_LAYER:MAIN
!

! "Nest up" program in WRFV2.
! 
! Description:
! 
! The nest up (nup.exe) program reads from wrfout_d02_<date> files for
! the nest and generates wrfout_d01_<date> files for the same periods as
! are in the input files.  The fields in the output are the fields in the
! input for state variables that have 'h' and 'u' in the I/O string of
! the Registry.  In other words, these are the fields that are normally
! fed back from nest->parent during 2-way nesting.  It will read and
! output over multiple files of nest data and generate an equivalent
! number of files of parent data.  The dimensions of the fields in the
! output are the size of the nest fields divided by the nesting ratio.
! 
! Source file:   main/nup_em.F
! 
! Compile with WRF: compile em_real
! 
! Resulting executable:  
! 
!     main/nup.exe 
!      -and-
!     symbolic link in test/em_real/nup.exe
! 
! Run as:  nup.exe (no arguments)
! 
! Namelist information:
! 
! Nup.exe uses the same namelist as a nested run of the wrf.exe.
! Important settings are:
! 
!  &time_control
! 
!    start_*            <start time information for both domains>
!    end_*              <start time information for both domains>
!    history_interval   <interval between frames in input/output files>
!    frames_per_outfile <number of frames in input/output files>
!    io_form_history    <2 for NetCDF>
! 
!  &domains
!     ...
!    max_dom            <number of domains; must be 2>
!    e_we               <col 2 is size of nested grid in west-east>
!                       <col 1 is ignored in the namelist>
!    e_sn               <col 2 is size of nested grid in south-north>
!                       <col 1 is ignored in the namelist>
!    parent_grid_ratio  <col 2 is nesting ratio in both dims>
!    feedback           <must be 1>
!    smooth_option      <recommend 0>
! 
!  &physics
!             <all options in this section should be the same
!              as the run that generated the nest data>
! 
!  created: JM 2006 01 25 

PROGRAM nup_em

   USE module_machine
   USE module_domain, ONLY : domain, wrfu_timeinterval, alloc_and_configure_domain, &
      domain_clock_set, domain_get_current_time, domain_get_stop_time, head_grid, &
      domain_clock_get, domain_clockadvance
   USE module_domain_type, ONLY : program_name
   USE module_streams
   USE module_initialize_real, only : wrfu_initialize
   USE module_integrate
   USE module_driver_constants
   USE module_configure, only : grid_config_rec_type, model_config_rec
   USE module_io_domain
   USE module_utility

   USE module_timing
   USE module_wrf_error
#ifdef DM_PARALLEL
   USE module_dm
#endif
!  USE read_util_module

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!new for bc
   USE module_bc
   USE module_big_step_utilities_em
   USE module_get_file_names
#if ( WRF_CHEM == 1 )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! for chemistry
   USE module_input_chem_data
!  USE module_input_chem_bioemiss
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#endif

   IMPLICIT NONE
 ! interface
   INTERFACE
     ! mediation-supplied
     SUBROUTINE med_read_wrf_chem_bioemiss ( grid , config_flags)
       USE module_domain
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_read_wrf_chem_bioemiss
     SUBROUTINE nup ( parent_grid , nested_grid, in_id, out_id, newly_opened )
       USE module_domain
       TYPE (domain), POINTER :: parent_grid, nested_grid
       INTEGER, INTENT(IN) :: in_id, out_id    ! io units
       LOGICAL, INTENT(IN) :: newly_opened     ! whether to add global metadata
     END SUBROUTINE nup

   END INTERFACE

   TYPE(WRFU_TimeInterval) :: RingInterval

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!new for bc
   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: its , ite , jts , jte , kts , kte
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

   CHARACTER(LEN=19) :: start_timestr , current_timestr , end_timestr, timestr
   CHARACTER(LEN=19) :: stopTimeStr

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

   INTEGER :: max_dom , domain_id , fid , fido, fidb , idum1 , idum2 , ierr
   INTEGER :: status_next_var
   INTEGER :: debug_level
   LOGICAL :: newly_opened
   CHARACTER (LEN=19) :: date_string

#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   INTEGER                 :: idsi, in_id, out_id
   INTEGER                 :: e_sn, e_we, pgr
   CHARACTER (LEN=80)      :: inpname , outname , bdyname
   CHARACTER (LEN=80)      :: si_inpname
   CHARACTER *19 :: temp19
   CHARACTER *24 :: temp24 , temp24b
   CHARACTER *256 :: fname
   CHARACTER(len=24) :: start_date_hold

   CHARACTER (LEN=80)      :: message
integer :: ii

#include "version_decl"

   !  Interface block for routine that passes pointers and needs to know that they
   !  are receiving pointers.

   INTERFACE

      SUBROUTINE med_feedback_domain ( parent_grid , nested_grid )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: parent_grid , nested_grid
      END SUBROUTINE med_feedback_domain

      SUBROUTINE Setup_Timekeeping( parent_grid )
         USE module_domain
         TYPE(domain), POINTER :: parent_grid
      END SUBROUTINE Setup_Timekeeping

   END INTERFACE

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "NUP_EM " // TRIM(release_version) // " PREPROCESSOR"

#ifdef DM_PARALLEL
   CALL disable_quilting
#endif

   !  Initialize the modules used by the WRF system.  Many of the CALLs made from the
   !  init_modules routine are NO-OPs.  Typical initializations are: the size of a 
   !  REAL, setting the file handles to a pre-use value, defining moisture and 
   !  chemistry indices, etc.

   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)
#ifdef NO_LEAP_CALENDAR
   CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_NOLEAP, rc=rc )
#else
   CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_GREGORIAN, rc=rc )
#endif
   CALL init_modules(2)   ! Phase 2 resumes after MPI_INIT() (if it is called)

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

   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   ! set the specified boundary to zero so the feedback goes all the way
   ! to the edge of the coarse domain
   CALL nl_set_spec_zone( 1, 0 )

   !  Allocated and configure the mother domain.  Since we are in the nesting down
   !  mode, we know a) we got a nest, and b) we only got 1 nest.

   NULLIFY( null_domain )

!!!! set up the parent grid  (for nup_em, this is the grid we do output from)

   CALL       nl_set_shw( 1, 0 )
   CALL       nl_set_shw( 2, 0 )
   CALL       nl_set_i_parent_start( 2, 1 )
   CALL       nl_set_j_parent_start( 2, 1 )
   CALL       nl_get_e_we( 2, e_we )
   CALL       nl_get_e_sn( 2, e_sn )
   CALL       nl_get_parent_grid_ratio( 2, pgr )

   ! parent grid must cover the entire nest, which is always dimensioned a factor of 3 + 1
   ! so add two here temporarily, then remove later after nest is allocated. 

   e_we = e_we / pgr + 2
   e_sn = e_sn / pgr + 2 
   CALL       nl_set_e_we( 1, e_we )
   CALL       nl_set_e_sn( 1, e_sn )

   CALL       wrf_message ( program_name )
   CALL       wrf_debug ( 100 , 'nup_em: calling alloc_and_configure_domain coarse ' )
   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   parent_grid => head_grid

   !  Set up time initializations.

   CALL Setup_Timekeeping ( parent_grid )

   CALL domain_clock_set( head_grid, &
                          time_step_seconds=model_config_rec%interval_seconds )

   CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
   CALL set_scalar_indices_from_config ( parent_grid%id , idum1, idum2 )

!!!! set up the fine grid  (for nup_em, this is the grid we do input into)

   CALL       wrf_message ( program_name )
   CALL       wrf_debug ( 100 , 'wrf: calling alloc_and_configure_domain fine ' )
   CALL alloc_and_configure_domain ( domain_id  = 2 ,                  &
                                     grid       = nested_grid ,        &
                                     parent     = parent_grid ,        &
                                     kid        = 1                   )

! now that the nest is allocated, pinch off the extra two rows/columns of the parent
! note the IKJ assumption here.
   parent_grid%ed31 = parent_grid%ed31 - 2
   parent_grid%ed33 = parent_grid%ed33 - 2
   CALL       nl_set_e_we( 1, e_we-2 )
   CALL       nl_set_e_sn( 1, e_sn-2 )

!write(0,*)'after alloc_and_configure_domain ',associated(nested_grid%intermediate_grid)

   CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
   CALL set_scalar_indices_from_config ( nested_grid%id , idum1, idum2 )

   !  Set up time initializations for the fine grid.

   CALL Setup_Timekeeping ( nested_grid )
   !  Adjust the time step on the clock so that it's the same as the history interval

   CALL WRFU_AlarmGet( nested_grid%alarms(HISTORY_ALARM), RingInterval=RingInterval )
   CALL WRFU_ClockSet( nested_grid%domain_clock, TimeStep=RingInterval, rc=rc )
   CALL WRFU_ClockSet( parent_grid%domain_clock, TimeStep=RingInterval, rc=rc )
   
   !  Get and store the history interval from the fine grid; use for time loop 


   !  Initialize the I/O for WRF.

   CALL init_wrfio

   !  Some of the configuration values may have been modified from the initial READ
   !  of the NAMELIST, so we re-broadcast the configuration records.

#ifdef DM_PARALLEL
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
#endif

   !  Open the input data (wrfout_d01_xxxxxx) for reading.
   in_id = 0
   out_id = 0
   main_loop : DO WHILE ( domain_get_current_time(nested_grid) .LT. domain_get_stop_time(nested_grid) )

      IF( WRFU_AlarmIsRinging( nested_grid%alarms( HISTORY_ALARM ), rc=rc ) ) THEN
        CALL domain_clock_get( nested_grid, current_timestr=timestr )
        newly_opened = .FALSE.
        IF ( in_id.EQ. 0 ) THEN
          CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
          CALL construct_filename2a ( fname , config_flags%history_outname , nested_grid%id , 2 , timestr )
          CALL open_r_dataset ( in_id, TRIM(fname), nested_grid ,  &
                                 config_flags , 'DATASET=HISTORY' , ierr )
          IF ( ierr .NE. 0 ) THEN
            WRITE(message,*)'Failed to open ',TRIM(fname),' for reading. '
            CALL wrf_message(message)
            EXIT main_loop
          ENDIF

          CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
          CALL construct_filename2a ( fname , config_flags%history_outname , parent_grid%id , 2 , timestr )
          CALL open_w_dataset ( out_id, TRIM(fname), parent_grid ,  &
                                 config_flags , output_history, 'DATASET=HISTORY' , ierr )
          IF ( ierr .NE. 0 ) THEN
            WRITE(message,*)'Failed to open ',TRIM(fname),' for writing. '
            CALL wrf_message(message)
            EXIT main_loop
          ENDIF
          newly_opened = .TRUE.
        ENDIF

        CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
        CALL input_history ( in_id, nested_grid , config_flags , ierr )
        IF ( ierr .NE. 0 ) THEN
          WRITE(message,*)'Unable to read time ',timestr
          CALL wrf_message(message)
          EXIT main_loop
        ENDIF
!
        CALL nup ( nested_grid , parent_grid, in_id, out_id, newly_opened  )
!
        CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
        CALL output_history ( out_id, parent_grid , config_flags , ierr )
        IF ( ierr .NE. 0 ) THEN
          WRITE(message,*)'Unable to write time ',timestr
          CALL wrf_message(message)
          EXIT main_loop
        ENDIF

        nested_grid%nframes(history_only) = nested_grid%nframes(history_only) + 1
        IF ( nested_grid%nframes(history_only) >= config_flags%frames_per_outfile ) THEN
          CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
          CALL close_dataset ( in_id , config_flags , "DATASET=HISTORY" )
          CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
          CALL close_dataset ( out_id , config_flags , "DATASET=HISTORY" )
          in_id = 0
          out_id = 0
          nested_grid%nframes(history_only) = 0
        ENDIF
        CALL WRFU_AlarmRingerOff( nested_grid%alarms( HISTORY_ALARM ), rc=rc )
      ENDIF
      CALL domain_clockadvance( nested_grid )
      CALL domain_clockadvance( parent_grid )
   ENDDO main_loop
   CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
   CALL med_shutdown_io ( parent_grid , config_flags )

   CALL wrf_debug ( 0 , 'nup_em: SUCCESS COMPLETE NUP_EM INIT' )

!  CALL wrf_shutdown

   CALL WRFU_Finalize( rc=rc )

END PROGRAM nup_em

SUBROUTINE nup ( nested_grid, parent_grid , in_id, out_id, newly_opened ) 
  USE module_domain
  USE module_io_domain
  USE module_utility
  USE module_timing
  USE module_wrf_error
!
  IMPLICIT NONE

! Args
  TYPE(domain), POINTER :: parent_grid, nested_grid
  INTEGER, INTENT(IN) :: in_id, out_id    ! io descriptors 
  LOGICAL, INTENT(IN) :: newly_opened     ! whether to add global metadata
! Local
  INTEGER :: julyr , julday , iswater , map_proj
  INTEGER :: icnt, ierr
  REAL    :: dt , new_bdy_frq
  REAL    :: gmt , cen_lat , cen_lon , dx , dy , truelat1 , truelat2 , moad_cen_lat , stand_lon
  REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp1 , vbdy3dtemp1 , tbdy3dtemp1 , pbdy3dtemp1 , qbdy3dtemp1
  REAL , DIMENSION(:,:,:) , ALLOCATABLE :: mbdy2dtemp1
  REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp2 , vbdy3dtemp2 , tbdy3dtemp2 , pbdy3dtemp2 , qbdy3dtemp2
  REAL , DIMENSION(:,:,:) , ALLOCATABLE :: mbdy2dtemp2
  INTEGER :: ids , ide , jds , jde , kds , kde
  INTEGER :: ims , ime , jms , jme , kms , kme
  INTEGER :: ips , ipe , jps , jpe , kps , kpe
  INTEGER :: its , ite , jts , jte , kts , kte

  INTERFACE
     SUBROUTINE med_feedback_domain ( parent_grid , nested_grid )
        USE module_domain
        USE module_configure
        TYPE(domain), POINTER :: parent_grid , nested_grid
     END SUBROUTINE med_feedback_domain
     SUBROUTINE med_interp_domain ( parent_grid , nested_grid )
        USE module_domain
        USE module_configure
        TYPE(domain), POINTER :: parent_grid , nested_grid
     END SUBROUTINE med_interp_domain
  END INTERFACE

  IF ( newly_opened ) THEN
    CALL wrf_get_dom_ti_integer ( in_id , 'MAP_PROJ' , map_proj , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'DX'  , dx  , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'DY'  , dy  , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'CEN_LAT' , cen_lat , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'CEN_LON' , cen_lon , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'TRUELAT1' , truelat1 , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'TRUELAT2' , truelat2 , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'MOAD_CEN_LAT' , moad_cen_lat , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'STAND_LON' , stand_lon , 1 , icnt , ierr ) 
!     CALL wrf_get_dom_ti_real    ( in_id , 'GMT' , gmt , 1 , icnt , ierr ) 
!     CALL wrf_get_dom_ti_integer ( in_id , 'JULYR' , julyr , 1 , icnt , ierr ) 
!     CALL wrf_get_dom_ti_integer ( in_id , 'JULDAY' , julday , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_integer ( in_id , 'ISWATER' , iswater , 1 , icnt , ierr ) 
  ENDIF

  parent_grid%fnm    = nested_grid%fnm
  parent_grid%fnp    = nested_grid%fnp
  parent_grid%rdnw   = nested_grid%rdnw
  parent_grid%rdn    = nested_grid%rdn
  parent_grid%dnw    = nested_grid%dnw
  parent_grid%dn     = nested_grid%dn 
  parent_grid%znu    = nested_grid%znu
  parent_grid%znw    = nested_grid%znw

  parent_grid%zs        = nested_grid%zs
  parent_grid%dzs       = nested_grid%dzs

  parent_grid%p_top     = nested_grid%p_top
  parent_grid%rdx       = nested_grid%rdx * 3.
  parent_grid%rdy       = nested_grid%rdy * 3.
  parent_grid%resm      = nested_grid%resm
  parent_grid%zetatop   = nested_grid%zetatop
  parent_grid%cf1       = nested_grid%cf1
  parent_grid%cf2       = nested_grid%cf2
  parent_grid%cf3       = nested_grid%cf3

  parent_grid%cfn       = nested_grid%cfn 
  parent_grid%cfn1      = nested_grid%cfn1

#if ( WRF_CHEM == 1 )
  parent_grid%chem_opt    = nested_grid%chem_opt
  parent_grid%chem_in_opt = nested_grid%chem_in_opt
#endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Various sizes that we need to be concerned about.

  ids = parent_grid%sd31
  ide = parent_grid%ed31
  kds = parent_grid%sd32
  kde = parent_grid%ed32
  jds = parent_grid%sd33
  jde = parent_grid%ed33

  ims = parent_grid%sm31
  ime = parent_grid%em31
  kms = parent_grid%sm32
  kme = parent_grid%em32
  jms = parent_grid%sm33
  jme = parent_grid%em33

  ips = parent_grid%sp31
  ipe = parent_grid%ep31
  kps = parent_grid%sp32
  kpe = parent_grid%ep32
  jps = parent_grid%sp33
  jpe = parent_grid%ep33

  nested_grid%imask_nostag = 1
  nested_grid%imask_xstag = 1
  nested_grid%imask_ystag = 1
  nested_grid%imask_xystag = 1

! Interpolate from nested_grid back onto parent_grid
  CALL med_feedback_domain ( parent_grid , nested_grid )

  parent_grid%ht_int = parent_grid%ht

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#if 0
         CALL construct_filename2( si_inpname , 'wrf_real_input_em' , parent_grid%id , 2 , start_date_char )
         CALL       wrf_debug ( 100 , 'med_sidata_input: calling open_r_dataset for ' // TRIM(si_inpname) )
         CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
         CALL open_r_dataset ( idsi, TRIM(si_inpname) , parent_grid , config_flags , "DATASET=INPUT", ierr )
         IF ( ierr .NE. 0 ) THEN
            CALL wrf_error_fatal( 'real: error opening wrf_real_input_em for reading: ' // TRIM (si_inpname) )
         END IF

         !  Input data.
   
         CALL       wrf_debug ( 100 , 'nup_em: calling input_aux_model_input2' )
         CALL input_aux_model_input2 ( idsi , parent_grid , config_flags , ierr )
         parent_grid%ht_input = parent_grid%ht
   
         !  Close this fine grid static input file.
   
         CALL       wrf_debug ( 100 , 'nup_em: closing fine grid static input' )
         CALL close_dataset ( idsi , config_flags , "DATASET=INPUT" )

         !  We need a parent grid landuse in the interpolation.  So we need to generate
         !  that field now.

         IF      ( ( parent_grid%ivgtyp(ips,jps) .GT. 0 ) .AND. &
                   ( parent_grid%isltyp(ips,jps) .GT. 0 ) ) THEN
            DO j = jps, MIN(jde-1,jpe)
               DO i = ips, MIN(ide-1,ipe)
                  parent_grid% vegcat(i,j) = parent_grid%ivgtyp(i,j)
                  parent_grid%soilcat(i,j) = parent_grid%isltyp(i,j)
               END DO
            END DO

         ELSE IF ( ( parent_grid% vegcat(ips,jps) .GT. 0.5 ) .AND. &
                   ( parent_grid%soilcat(ips,jps) .GT. 0.5 ) ) THEN
            DO j = jps, MIN(jde-1,jpe)
               DO i = ips, MIN(ide-1,ipe)
                  parent_grid%ivgtyp(i,j) = NINT(parent_grid% vegcat(i,j))
                  parent_grid%isltyp(i,j) = NINT(parent_grid%soilcat(i,j))
               END DO
            END DO

         ELSE
            num_veg_cat      = SIZE ( parent_grid%landusef , DIM=2 )
            num_soil_top_cat = SIZE ( parent_grid%soilctop , DIM=2 )
            num_soil_bot_cat = SIZE ( parent_grid%soilcbot , DIM=2 )
   
            CALL land_percentages (  parent_grid%xland , &
                                     parent_grid%landusef , parent_grid%soilctop , parent_grid%soilcbot , &
                                     parent_grid%isltyp , parent_grid%ivgtyp , &
                                     num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     ips , ipe , jps , jpe , kps , kpe , &
                                     model_config_rec%iswater(parent_grid%id) )

          END IF

          DO j = jps, MIN(jde-1,jpe)
            DO i = ips, MIN(ide-1,ipe)
               parent_grid%lu_index(i,j) = parent_grid%ivgtyp(i,j)
            END DO
         END DO

         CALL check_consistency ( parent_grid%ivgtyp , parent_grid%isltyp , parent_grid%landmask , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe , &
                                  model_config_rec%iswater(parent_grid%id) )

         CALL check_consistency2( parent_grid%ivgtyp , parent_grid%isltyp , parent_grid%landmask , &
                                  parent_grid%tmn , parent_grid%tsk , parent_grid%sst , parent_grid%xland , &
                                  parent_grid%tslb , parent_grid%smois , parent_grid%sh2o , &
                                  config_flags%num_soil_layers , parent_grid%id , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe , &
                                  model_config_rec%iswater(parent_grid%id) )


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
      !  We have 2 terrain elevations.  One is from input and the other is from the
      !  the horizontal interpolation.

      parent_grid%ht_fine = parent_grid%ht_input
      parent_grid%ht      = parent_grid%ht_int

      !  We have both the interpolated fields and the higher-resolution static fields.  From these
      !  the rebalancing is now done.  Note also that the field parent_grid%ht is now from the 
      !  fine grid input file (after this call is completed).

      CALL rebalance_driver ( parent_grid ) 

      !  Different things happen during the different time loops:
      !      first loop - write wrfinput file, close data set, copy files to holder arrays
      !      middle loops - diff 3d/2d arrays, compute and output bc
      !      last loop - diff 3d/2d arrays, compute and output bc, write wrfbdy file, close wrfbdy file

         !  Set the time info.

         print *,'current_date = ',current_date
         CALL domain_clock_set( parent_grid, &
                                current_timestr=current_date(1:19) )
!
! SEP     Put in chemistry data
!
#if ( WRF_CHEM == 1 )
         IF( parent_grid%chem_opt .NE. 0 ) then
            IF( parent_grid%chem_in_opt .EQ. 0 ) then
             ! Read the chemistry data from a previous wrf forecast (wrfout file)
              ! Generate chemistry data from a idealized vertical profile
              message = 'STARTING WITH BACKGROUND CHEMISTRY '
              CALL  wrf_message ( message )

              CALL input_chem_profile ( parent_grid )

              message = 'READING BEIS3.11 EMISSIONS DATA'
              CALL  wrf_message ( message )

              CALL med_read_wrf_chem_bioemiss ( parent_grid , config_flags)
            ELSE
              message = 'RUNNING WITHOUT CHEMISTRY INITIALIZATION'
              CALL  wrf_message ( message )
            ENDIF
         ENDIF
#endif

#endif

         !  Output the first time period of the data.
   
  IF ( newly_opened ) THEN
    CALL wrf_put_dom_ti_integer ( out_id , 'MAP_PROJ' , map_proj , 1 , ierr ) 
!     CALL wrf_put_dom_ti_real    ( out_id , 'DX'  , dx  , 1 , ierr ) 
!     CALL wrf_put_dom_ti_real    ( out_id , 'DY'  , dy  , 1 , ierr ) 
    CALL wrf_put_dom_ti_real    ( out_id , 'CEN_LAT' , cen_lat , 1 , ierr ) 
    CALL wrf_put_dom_ti_real    ( out_id , 'CEN_LON' , cen_lon , 1 , ierr ) 
    CALL wrf_put_dom_ti_real    ( out_id , 'TRUELAT1' , truelat1 , 1 , ierr ) 
    CALL wrf_put_dom_ti_real    ( out_id , 'TRUELAT2' , truelat2 , 1 , ierr ) 
    CALL wrf_put_dom_ti_real    ( out_id , 'MOAD_CEN_LAT' , moad_cen_lat , 1 , ierr ) 
    CALL wrf_put_dom_ti_real    ( out_id , 'STAND_LON' , stand_lon , 1 , ierr ) 
    CALL wrf_put_dom_ti_integer ( out_id , 'ISWATER' , iswater , 1 , ierr ) 

    CALL wrf_put_dom_ti_real    ( out_id , 'GMT' , gmt , 1 , ierr ) 
    CALL wrf_put_dom_ti_integer ( out_id , 'JULYR' , julyr , 1 , ierr ) 
    CALL wrf_put_dom_ti_integer ( out_id , 'JULDAY' , julday , 1 , ierr ) 
  ENDIF

END SUBROUTINE nup

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
      CALL wrf_debug( 0, 'mismatch in check_consistency, turned to water points, be careful' )
   END IF

END SUBROUTINE check_consistency

SUBROUTINE check_consistency2( ivgtyp , isltyp , landmask , &
                               tmn , tsk , sst , xland , &
                               tslb , smois , sh2o , &
                               num_soil_layers , id , &
                               ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte , &
                               iswater )

   USE module_configure
   USE module_optional_input

   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte 
   INTEGER , INTENT(IN) :: num_soil_layers , id

   INTEGER , DIMENSION(ims:ime,jms:jme) :: ivgtyp , isltyp
   REAL    , DIMENSION(ims:ime,jms:jme) :: landmask , tmn , tsk , sst , xland
   REAL    , DIMENSION(ims:ime,num_soil_layers,jms:jme) :: tslb , smois , sh2o

   INTEGER :: oops1 , oops2
   INTEGER :: i , j , k

      fix_tsk_tmn : SELECT CASE ( model_config_rec%sf_surface_physics(id) )

         CASE ( SLABSCHEME , LSMSCHEME , RUCLSMSCHEME )
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( ( landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
                     tmn(i,j) = sst(i,j)
                     tsk(i,j) = sst(i,j)
                  ELSE IF ( landmask(i,j) .LT. 0.5 ) THEN
                     tmn(i,j) = tsk(i,j)
                  END IF
               END DO
            END DO
      END SELECT fix_tsk_tmn

      !  Is the TSK reasonable?

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( tsk(i,j) .LT. 170 .or. tsk(i,j) .GT. 400. ) THEN
               print *,'error in the TSK'
               print *,'i,j=',i,j
               print *,'landmask=',landmask(i,j)
               print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
               if(tmn(i,j).gt.170. .and. tmn(i,j).lt.400.)then
                  tsk(i,j)=tmn(i,j)
               else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                  tsk(i,j)=sst(i,j)
               else
                  CALL wrf_error_fatal ( 'TSK unreasonable' )
               end if
            END IF
         END DO
      END DO

      !  Is the TMN reasonable?

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( tmn(i,j) .LT. 170. ) .OR. ( tmn(i,j) .GT. 400. ) ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
                  print *,'error in the TMN'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
               if(tsk(i,j).gt.170. .and. tsk(i,j).lt.400.)then
                  tmn(i,j)=tsk(i,j)
               else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                  tmn(i,j)=sst(i,j)
               else
                  CALL wrf_error_fatal ( 'TMN unreasonable' )
               endif
            END IF
         END DO
      END DO

      !  Is the TSLB reasonable?

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( tslb(i,1,j) .LT. 170. ) .OR. ( tslb(i,1,j) .GT. 400. ) ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
                  print *,'error in the TSLB'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
                  print *,'tslb = ',tslb(i,:,j)
                  print *,'old smois = ',smois(i,:,j)
                  DO l = 1 , num_soil_layers
                     sh2o(i,l,j) = 0.0
                  END DO
                  DO l = 1 , num_soil_layers
                     smois(i,l,j) = 0.3
                  END DO
                  if(tsk(i,j).gt.170. .and. tsk(i,j).lt.400.)then
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)=tsk(i,j)
                     END DO
                  else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)=sst(i,j)
                     END DO
                  else if(tmn(i,j).gt.170. .and. tmn(i,j).lt.400.)then
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)=tmn(i,j)
                     END DO
                  else
                     CALL wrf_error_fatal ( 'TSLB unreasonable' )
                  endif
            END IF
         END DO
      END DO

      !  Let us make sure (again) that the landmask and the veg/soil categories match.

oops1=0
oops2=0
      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( landmask(i,j) .LT. 0.5 ) .AND. ( ivgtyp(i,j) .NE. iswater .OR. isltyp(i,j) .NE. 14 ) ) .OR. &
                 ( ( landmask(i,j) .GT. 0.5 ) .AND. ( ivgtyp(i,j) .EQ. iswater .OR. isltyp(i,j) .EQ. 14 ) ) ) THEN
               IF ( tslb(i,1,j) .GT. 1. ) THEN
oops1=oops1+1
                  ivgtyp(i,j) = 5
                  isltyp(i,j) = 8
                  landmask(i,j) = 1
                  xland(i,j) = 1
               ELSE IF ( sst(i,j) .GT. 1. ) THEN
oops2=oops2+1
                  ivgtyp(i,j) = iswater
                  isltyp(i,j) = 14
                  landmask(i,j) = 0
                  xland(i,j) = 2
               ELSE
                  print *,'the landmask and soil/veg cats do not match'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'ivgtyp=',ivgtyp(i,j)
                  print *,'isltyp=',isltyp(i,j)
                  print *,'iswater=', iswater
                  print *,'tslb=',tslb(i,:,j)
                  print *,'sst=',sst(i,j)
                  CALL wrf_error_fatal ( 'mismatch_landmask_ivgtyp' )
               END IF
            END IF
         END DO
      END DO
if (oops1.gt.0) then
print *,'points artificially set to land : ',oops1
endif
if(oops2.gt.0) then
print *,'points artificially set to water: ',oops2
endif

END SUBROUTINE check_consistency2
