!IDEAL:DRIVER_LAYER
!
! create an initial data set for the WRF model based on an ideal condition
PROGRAM ideal

   USE module_domain , ONLY : domain
   USE module_initialize_ideal
   USE module_configure , ONLY : grid_config_rec_type

   USE module_timing
   USE module_wrf_error
#if ( WRF_CHEM == 1 )
   USE module_input_chem_data
   USE module_input_chem_bioemiss
#endif

   IMPLICIT NONE
#if ( WRF_CHEM == 1 )
  ! interface
   INTERFACE
     ! mediation-supplied
     SUBROUTINE med_read_wrf_chem_bioemiss ( grid , config_flags)
       USE module_domain
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_read_wrf_chem_bioemiss
   END INTERFACE
#endif

   REAL    :: time

   INTEGER :: loop , &
              levels_to_process


   TYPE(domain) , POINTER :: keep_grid, grid_ptr, null_domain, grid
   TYPE(domain)           :: dummy
   TYPE (grid_config_rec_type)              :: config_flags
   TYPE (WRFU_Time) startTime, stopTime, currentTime
   TYPE (WRFU_TimeInterval) stepTime

   INTEGER :: max_dom , domain_id , fid , oid , idum1 , idum2 , ierr
   INTEGER :: debug_level, rc
   LOGICAL :: input_from_file

   INTERFACE
     SUBROUTINE med_initialdata_output ( grid , config_flags )
       USE module_domain , ONLY : domain
       USE module_configure , ONLY : grid_config_rec_type
       TYPE (domain) , POINTER :: grid
       TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
     END SUBROUTINE med_initialdata_output 
   END INTERFACE

#include "version_decl"


#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   CHARACTER (LEN=80)     :: message

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "IDEAL " // TRIM(release_version) // " PREPROCESSOR"

   !  Get the NAMELIST data for input.

   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)
#ifdef NO_LEAP_CALENDAR
   CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_NOLEAP, rc=rc )
#else
   CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_GREGORIAN, rc=rc )
#endif
   CALL init_modules(2)   ! Phase 2 resumes after MPI_INIT() (if it is called)

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
   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   CALL  wrf_message ( program_name )


   ! allocated and configure the mother domain

   NULLIFY( null_domain )

   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   grid => head_grid
   ! TBH:  Note that historically, IDEAL did not set up clocks.  These 
   ! TBH:  are explicit replacements for old default initializations...  They 
   ! TBH:  are needed to ensure that time manager calls do not fail due to 
   ! TBH:  uninitialized clock.  Clean this up later...  
   CALL WRFU_TimeSet(startTime, YY=1, MM=1, DD=1, H=0, M=0, S=0, rc=rc)
   stopTime = startTime
   currentTime = startTime
   ! TBH:  Bogus time step value -- clock is never advanced...  
   CALL WRFU_TimeIntervalSet(stepTime, S=180, rc=rc)
   grid%domain_clock = WRFU_ClockCreate( TimeStep= stepTime,  &
                                         StartTime=startTime, &
                                         StopTime= stopTime,  &
                                         rc=rc )
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                         'grid%domain_clock = WRFU_ClockCreate() FAILED', &
                         __FILE__ , &
                         __LINE__  )
   CALL       wrf_debug ( 100 , 'wrf: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( head_grid%id , model_config_rec , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

#ifdef PLANET
   WRITE ( current_date , FMT = '(I4.4,"-",I5.5,"_",I2.2,":",I2.2,":",I2.2,".0000")' ) &
           config_flags%start_year, &
           config_flags%start_day, &
           config_flags%start_hour, &
           config_flags%start_minute, &
           config_flags%start_second 
#else
   WRITE ( current_date , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2,".0000")' ) &
           config_flags%start_year, &
           config_flags%start_month, &
           config_flags%start_day, &
           config_flags%start_hour, &
           config_flags%start_minute, &
           config_flags%start_second 
#endif
   CALL domain_clockprint ( 150, grid, &
          'DEBUG assemble_output:  clock before 1st currTime set,' )
   WRITE (wrf_err_message,*) &
        'DEBUG assemble_output:  before 1st currTime set, current_date = ',TRIM(current_date)
   CALL wrf_debug ( 150 , wrf_err_message )
   CALL domain_clock_set( grid, current_timestr=current_date(1:19) )
   CALL domain_clockprint ( 150, grid, &
          'DEBUG assemble_output:  clock after 1st currTime set,' )

   CALL       wrf_debug ( 100 , 'wrf: calling init_wrfio' )
   CALL init_wrfio

#ifdef DM_PARALLEL
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
#endif
      
#if ( WRF_CHEM == 1 )
         IF( grid%chem_opt > 0 ) then
           ! Read the chemistry data from a previous wrf forecast (wrfout file)
           IF(grid%chem_in_opt == 1 ) THEN
              message = 'INITIALIZING CHEMISTRY WITH OLD SIMULATION'
              CALL  wrf_message ( message )

              CALL med_read_wrf_chem_input ( grid , config_flags)
              IF(grid%emiss_opt == ECPTEC .or. grid%emiss_opt == GOCART_ECPTEC   &
                                         .or. grid%biomass_burn_opt == BIOMASSB) THEN
                 message = 'READING EMISSIONS DATA OPT 3'
                 CALL  wrf_message ( message )
!                CALL med_read_bin_chem_emissopt3 ( grid , config_flags)
                 CALL med_read_wrf_chem_emissopt3 ( grid , config_flags)
              END IF

              IF(grid%bio_emiss_opt == 2 ) THEN
                 message = 'READING BEIS3.11 EMISSIONS DATA'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_bioemiss ( grid , config_flags)
              else IF(grid%bio_emiss_opt == 3 ) THEN !shc
                 message = 'READING MEGAN 2 EMISSIONS DATA'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_bioemiss ( grid , config_flags)
              END IF

              IF(grid%dust_opt == 1 .or. grid%dmsemis_opt == 1 .or. grid%chem_opt == 300) THEN !shc
                 message = 'READING GOCART BG AND/OR DUST and DMS REF FIELDS'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_gocart_bg ( grid , config_flags)
              END IF

           ELSEIF(grid%chem_in_opt == 0)then
              ! Generate chemistry data from a idealized vertical profile
              message = 'STARTING WITH BACKGROUND CHEMISTRY '
              CALL  wrf_message ( message )

              CALL input_chem_profile ( grid )

              IF(grid%bio_emiss_opt == 2 ) THEN
                 message = 'READING BEIS3.11 EMISSIONS DATA'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_bioemiss ( grid , config_flags)
              else IF(grid%bio_emiss_opt == 3 ) THEN !shc
                 message = 'READING MEGAN 2 EMISSIONS DATA'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_bioemiss ( grid , config_flags)
              END IF
              IF(grid%emiss_opt == ECPTEC .or. grid%emiss_opt == GOCART_ECPTEC   &
                                         .or. grid%biomass_burn_opt == BIOMASSB) THEN
                 message = 'READING EMISSIONS DATA OPT 3'
                 CALL  wrf_message ( message )
!                CALL med_read_bin_chem_emissopt3 ( grid , config_flags)
                 CALL med_read_wrf_chem_emissopt3 ( grid , config_flags)
              END IF

              IF(grid%dust_opt == 1 .or. grid%dmsemis_opt == 1 .or. grid%chem_opt == 300) THEN !shc
                 message = 'READING GOCART BG AND/OR DUST and DMS REF FIELDS'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_gocart_bg ( grid , config_flags)
              END IF

           ELSE
             message = 'RUNNING WITHOUT CHEMISTRY INITIALIZATION'
             CALL  wrf_message ( message )
           END IF
         END IF
#endif

   grid%this_is_an_ideal_run = .TRUE.
   CALL med_initialdata_output( head_grid , config_flags )

   CALL       wrf_debug (   0 , 'wrf: SUCCESS COMPLETE IDEAL INIT' )
   CALL med_shutdown_io ( head_grid , config_flags )
   CALL wrf_shutdown

   CALL WRFU_Finalize( rc=rc )

END PROGRAM ideal

SUBROUTINE med_initialdata_output ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_initialize_ideal
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)  , POINTER                    :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                :: time_step_begin_restart
   INTEGER                :: fid , ierr , id
   CHARACTER (LEN=80)      :: rstname
   CHARACTER (LEN=80)      :: message
   CHARACTER (LEN=80)      :: inpname , bdyname

   !  Initialize the mother domain.

   grid%input_from_file = .false.
   CALL init_domain (  grid )
   CALL calc_current_date ( grid%id, 0.)

   CALL construct_filename1 ( inpname , 'wrfinput' , grid%id , 2 )
   CALL open_w_dataset ( id, TRIM(inpname) , grid , config_flags , output_input , "DATASET=INPUT", ierr )
   IF ( ierr .NE. 0 ) THEN
     WRITE (wrf_err_message,*)'ideal: error opening wrfinput for writing ',ierr
     CALL wrf_error_fatal( wrf_err_message )
   ENDIF
   CALL output_input ( id, grid , config_flags , ierr )
   CALL close_dataset ( id , config_flags, "DATASET=INPUT" )


   IF ( config_flags%specified ) THEN
 
     CALL construct_filename1 ( bdyname , 'wrfbdy' , grid%id , 2 )
     CALL open_w_dataset ( id, TRIM(bdyname) , grid , config_flags , output_boundary , "DATASET=BOUNDARY", ierr )
     IF ( ierr .NE. 0 ) THEN
       WRITE (wrf_err_message,*)'ideal: error opening wrfbdy for writing ',ierr
       CALL wrf_error_fatal( wrf_err_message )
     ENDIF
     CALL output_boundary ( id, grid , config_flags , ierr )
     CALL close_dataset ( id , config_flags , "DATASET=BOUNDARY" )
 
   ENDIF

   RETURN
END SUBROUTINE med_initialdata_output

