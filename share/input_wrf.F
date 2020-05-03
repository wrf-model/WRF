!WRF:MEDIATION:IO
!  ---principal wrf input routine (called from routines in module_io_domain )

  SUBROUTINE input_wrf ( fid , grid , config_flags , switch , ierr )
    USE module_domain
    USE module_state_description
    USE module_configure
    USE module_io
    USE module_io_wrf
    USE module_date_time
    USE module_bc_time_utilities
    USE module_utility

    IMPLICIT NONE
#include "wrf_io_flags.h"
#include "wrf_status_codes.h"
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(INOUT)    :: config_flags
    INTEGER, INTENT(IN) :: fid
    INTEGER, INTENT(IN) :: switch
    INTEGER, INTENT(INOUT) :: ierr

    ! Local data
    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe

    TYPE( fieldlist ), POINTER :: p

    INTEGER newswitch, itrace

    INTEGER       iname(9)
    INTEGER       iordering(3)
    INTEGER       icurrent_date(24)
    INTEGER       i,j,k
    INTEGER       icnt
    INTEGER       ndim
    INTEGER       ilen
    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    CHARACTER*256 errmess, currtimestr
    CHARACTER*40            :: this_datestr, next_datestr
    CHARACTER*9   NAMESTR
    INTEGER       IBDY, NAMELEN
    LOGICAL wrf_dm_on_monitor
    EXTERNAL wrf_dm_on_monitor
    Type(WRFU_Time) time, currtime, currentTime
    CHARACTER*19  new_date
    CHARACTER*24  base_date
    CHARACTER*256  fname, version_name
    CHARACTER*80  dname, memord, sim_type
    LOGICAL dryrun
    INTEGER idt
    INTEGER itmp
    INTEGER filestate, ierr3
    INTEGER hybrid_opt, use_theta_m
    INTEGER :: ide_compare , jde_compare , kde_compare
    CHARACTER (len=19) simulation_start_date , first_date_input , first_date_nml
    INTEGER first_date_start_year   , &
            first_date_start_month  , &
            first_date_start_day    , &
            first_date_start_hour   , &
            first_date_start_minute , &
            first_date_start_second
    INTEGER simulation_start_year   , &
            simulation_start_month  , &
            simulation_start_day    , &
            simulation_start_hour   , &
            simulation_start_minute , &
            simulation_start_second
    LOGICAL reset_simulation_start
    REAL dx_compare , dy_compare , dum
    INTEGER :: num_land_cat_compare
    CHARACTER (LEN=256) :: MMINLU

    !  Local variables for vertical interpolation.

    REAL, ALLOCATABLE, DIMENSION(:      ) ::  f_vint_1d
    REAL, ALLOCATABLE, DIMENSION(:,:,:  ) ::  f_vint_3d
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:) ::  f_vint_4d
    integer :: ed1_c,em1_c,ep1_c
    integer :: ed2_c,em2_c,ep2_c
    integer :: n_ref_m,i_inter

    !  Local variables for the alarms in the input restart file.

    INTEGER max_wrf_alarms_compare, seconds
    CHARACTER*80 alarmname, timestr
    TYPE(WRFU_Time) :: curtime, ringTime
    TYPE(WRFU_TimeInterval) :: interval, interval2
    integer s, iring

    !  Local variables: are we are using the correct hypsometric option for ARW ideal cases.

    CHARACTER (LEN=256) :: input_name
    INTEGER :: loop, hypsometric_opt, icount

    CHARACTER (LEN=256) :: a_message

    !  Bundle up the fatal errors in one piece at the end of the file.

    INTEGER :: count_fatal_error

    !  Make sure that the input data is consistent with the current code.

    LOGICAL :: yes_use_this_data

    CHARACTER(10) :: check_which_switch 
    CHARACTER(10) :: my_string


!<DESCRIPTION>
!
! Core wrf input routine for all input data streams. Part of mediation layer.
!
! Note that WRF IOAPI routines wrf_get_dom_ti_*() do not return values during
! training reads (dryrun).
!
!</DESCRIPTION>

    WRITE(wrf_err_message,*)'input_wrf: begin'
    CALL wrf_debug( 300 , wrf_err_message )

    CALL modify_io_masks ( grid%id )   ! this adjusts the I/O masks according to the users run-time specs, if any

    !  Initializations for error checking

    ierr = 0
    count_fatal_error = 0


    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )

! If this was not a training read (dry run) check for erroneous values.
    CALL wrf_inquire_filename ( fid , fname , filestate , ierr )
    IF ( ierr /= 0 ) THEN
      WRITE(wrf_err_message,*)'---- ERROR: module_io_wrf: input_wrf: wrf_inquire_filename Status = ',ierr
      CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
      count_fatal_error = count_fatal_error + 1
    ENDIF

    WRITE(wrf_err_message,*)'input_wrf: filestate = ',filestate
    CALL wrf_debug( 300 , wrf_err_message )

    dryrun        = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )

    WRITE(wrf_err_message,*)'input_wrf: dryrun = ',dryrun,' switch ',switch
    CALL wrf_debug( 300 , wrf_err_message )

    check_if_dryrun : IF ( .NOT. dryrun ) THEN

#if ( ( EM_CORE == 1 ) && ( DA_CORE != 1 ) )
      IF ( switch .EQ. boundary_only ) THEN
         grid%just_read_boundary = .true.
      END IF
#endif

#if ( NMM_CORE != 1 )
    IF ( .NOT. dryrun ) THEN
   
       !  Does this file exist?
   
       CALL wrf_inquire_filename ( fid , fname , filestate , ierr )
       IF ( ( filestate .EQ. WRF_FILE_NOT_OPENED       ) .OR. &
            ( filestate .EQ. WRF_FILE_OPENED_FOR_WRITE ) ) THEN
          my_string = check_which_switch(switch)
          CALL wrf_error_fatal( 'Possibly missing file for = ' // TRIM(my_string) )
       END IF

       !  Determine is the input file we are reading is acceptable given the
       !  assumptions about the current version of the modeling system.
   
       CALL is_this_data_ok_to_use ( fid , yes_use_this_data )
       IF ( ( yes_use_this_data ) .OR. ( config_flags%force_use_old_data ) ) THEN
          WRITE(wrf_err_message,*)'Input data is acceptable to use: ' // TRIM(fname) 
          CALL wrf_debug( 0 , wrf_err_message )
       ELSE
          CALL wrf_debug( 0 , 'File name that is causing troubles = ' // TRIM(fname) )
          WRITE(wrf_err_message,*)'You can try 1) ensure that the input file was created with WRF v4 pre-processors, or '
          CALL wrf_debug( 0 , TRIM(wrf_err_message) )
          WRITE(wrf_err_message,*)'2) use force_use_old_data=T in the time_control record of the namelist.input file'
          CALL wrf_debug( 0 , TRIM(wrf_err_message) )
          WRITE(wrf_err_message,*)'---- ERROR: The input file appears to be from a pre-v4 version of WRF initialization routines'
          CALL wrf_error_fatal( wrf_err_message )
       END IF
    END IF
#endif

#if ( NMM_CORE != 1 )
    ! Verify feature consistency between model input and model nml settings

    IF ( .NOT. dryrun ) THEN

      IF ( switch .EQ. input_only ) THEN
        CALL wrf_get_dom_ti_char ( fid , 'TITLE' , version_name, ierr )

        IF ( INDEX(TRIM(version_name),' V4.') .NE. 0 ) THEN
          CALL wrf_get_dom_ti_integer( fid , 'HYBRID_OPT',  hybrid_opt,  1, icnt, ierr )
          CALL wrf_get_dom_ti_integer( fid , 'USE_THETA_M', use_theta_m, 1, icnt, ierr )

          IF ( hybrid_opt .NE. config_flags%hybrid_opt ) THEN
            WRITE(wrf_err_message,*) '---- ERROR: Input file hybrid_opt = ',hybrid_opt
            CALL wrf_debug ( 0, TRIM(wrf_err_message) )
            WRITE(wrf_err_message,*) '---- ERROR: Namelist hybrid_opt   = ',config_flags%hybrid_opt
            CALL wrf_debug ( 0, TRIM(wrf_err_message) )
            CALL wrf_debug ( 0, "---- ERROR: hybrid_opt values must be consistent" )
            count_fatal_error = count_fatal_error + 1
          END IF

          IF ( use_theta_m .NE. config_flags%use_theta_m ) THEN
            WRITE(wrf_err_message,*) '---- ERROR: Input file use_theta_m = ',use_theta_m
            CALL wrf_debug ( 0, TRIM(wrf_err_message) )
            WRITE(wrf_err_message,*) '---- ERROR: Namelist use_theta_m   = ',config_flags%use_theta_m
            CALL wrf_debug ( 0, TRIM(wrf_err_message) )
            CALL wrf_debug ( 0, "---- ERROR: use_theta_m values must be consistent" )
            count_fatal_error = count_fatal_error + 1
          END IF

        END IF

      ELSE IF ( switch .EQ. auxinput1_only ) THEN
        CALL wrf_get_dom_ti_char ( fid , 'TITLE' , version_name, ierr )
        IF ( INDEX(TRIM(version_name),' V4.') .NE. 0 ) THEN
           grid%v4_metgrid = .TRUE.
        ELSE
           grid%v4_metgrid = .FALSE.
        END IF
      END IF
    END IF
#endif

      ! INPUT ONLY (KK)

      IF ( switch .EQ. restart_only ) THEN

        ! recover the restart alarms from input if available
 
        CALL wrf_get_dom_ti_integer( fid , 'MAX_WRF_ALARMS', max_wrf_alarms_compare, 1, icnt, ierr )
        IF ( max_wrf_alarms_compare .NE. MAX_WRF_ALARMS ) THEN
          WRITE(wrf_err_message,*)'MAX_WRF_ALARMS different in restart file (',max_wrf_alarms_compare,&
                                  ') from in code (',MAX_WRF_ALARMS,').  Disregarding info in restart file.'
        ELSE
          curtime = domain_get_current_time( grid )
          DO i = auxinput1_only, MAX_WRF_ALARMS
            IF ( grid%alarms_created(i) .AND. .NOT. i .EQ. boundary_only ) THEN
              write(alarmname,'("WRF_ALARM_ISRINGING_",i2.2)')i
              CALL wrf_get_dom_ti_integer( fid, TRIM(alarmname), iring, 1, icnt, ierr )
  
              write(alarmname,'("WRF_ALARM_SECS_TIL_NEXT_RING_",i2.2)')i
              CALL wrf_get_dom_ti_integer( fid, TRIM(alarmname), seconds, 1, icnt, ierr )
              IF ( ierr .EQ. 0 &
                   .AND. seconds .GE. 0 ) THEN  ! disallow negative intervals; can happen with wrfbdy datasets
                                                ! which keep time differently
  
                ! Get and set interval so that we are sure to have both the
                ! interval and first ring time set correctly
                CALL WRFU_AlarmGet( grid%alarms(i), ringinterval=interval2 )

                IF (config_flags%override_restart_timers) THEN
                   IF (i .EQ. restart_only) THEN
                      seconds = grid%restart_interval_d * 86400 + &
                                grid%restart_interval_h *  3600 + &
                                grid%restart_interval_m *    60 + &
                                grid%restart_interval   *    60 + &
                                grid%restart_interval_s
                   ENDIF
                ENDIF

                CALL WRFU_TimeIntervalSet(interval,S=seconds)
                ringTime = curtime + interval
                CALL WRFU_AlarmSet( grid%alarms(i), RingInterval=interval2, RingTime=ringTime )

              ENDIF

              IF ( iring .EQ. 1 ) THEN
                CALL WRFU_AlarmRingerOn( grid%alarms( i ) )
              ELSE
                CALL WRFU_AlarmRingerOff( grid%alarms( i ) )
              ENDIF
            ENDIF
          ENDDO
        ENDIF

     
     !OUTPUT ONLY (KK)

      IF ( switch .EQ. restart_only .AND. .NOT. config_flags%override_restart_timers ) THEN

        ! recover the restart alarms from input if available
 
        CALL wrf_get_dom_ti_integer( fid , 'MAX_WRF_ALARMS', max_wrf_alarms_compare, 1, icnt, ierr )
        IF ( max_wrf_alarms_compare .NE. MAX_WRF_ALARMS ) THEN
          WRITE(wrf_err_message,*)'MAX_WRF_ALARMS different in restart file (',max_wrf_alarms_compare,&
                                  ') from in code (',MAX_WRF_ALARMS,').  Disregarding info in restart file.'
        ELSE
          curtime = domain_get_current_time( grid )
          DO i = 1, auxinput1_only-1
            IF ( grid%alarms_created(i) .AND. .NOT. i .EQ. boundary_only ) THEN
              write(alarmname,'("WRF_ALARM_ISRINGING_",i2.2)')i
              CALL wrf_get_dom_ti_integer( fid, TRIM(alarmname), iring, 1, icnt, ierr )
  
              write(alarmname,'("WRF_ALARM_SECS_TIL_NEXT_RING_",i2.2)')i
              CALL wrf_get_dom_ti_integer( fid, TRIM(alarmname), seconds, 1, icnt, ierr )
              IF ( ierr .EQ. 0 &
                   .AND. seconds .GE. 0 ) THEN  ! disallow negative intervals; can happen with wrfbdy datasets
                                                ! which keep time differently
  
                ! Get and set interval so that we are sure to have both the
                ! interval and first ring time set correctly
                CALL WRFU_AlarmGet( grid%alarms(i), ringinterval=interval2 )

                IF (config_flags%override_restart_timers) THEN
                   IF (i .EQ. history_only) THEN
                      seconds = grid%history_interval_d * 86400 + &
                                grid%history_interval_h *  3600 + &
                                grid%history_interval_m *    60 + &
                                grid%history_interval   *    60 + &
                                grid%history_interval_s
                   ENDIF
                ENDIF

                CALL WRFU_TimeIntervalSet(interval,S=seconds)
                ringTime = curtime + interval
                CALL WRFU_AlarmSet( grid%alarms(i), RingInterval=interval, RingTime=ringTime )

              ENDIF

              IF ( iring .EQ. 1 ) THEN
                CALL WRFU_AlarmRingerOn( grid%alarms( i ) )
              ELSE
                CALL WRFU_AlarmRingerOff( grid%alarms( i ) )
              ENDIF
            ENDIF
          ENDDO
        ENDIF

      ENDIF

      CALL wrf_get_dom_ti_char ( fid , 'SIMULATION_START_DATE' , simulation_start_date , ierr )
      CALL nl_get_reset_simulation_start ( 1, reset_simulation_start )

      IF ( ( ierr .EQ. 0 ) .AND. ( .NOT. reset_simulation_start ) ) THEN
        ! Overwrite simulation start date with metadata.
#ifdef PLANET
        READ ( simulation_start_date , fmt = '(I4,1x,I5,1x,I2,1x,I2,1x,I2)' )       &
               simulation_start_year,                                               &
               simulation_start_day,    simulation_start_hour,                      &
               simulation_start_minute, simulation_start_second
        simulation_start_month = 0
#else
        READ ( simulation_start_date , fmt = '(I4,1x,I2,1x,I2,1x,I2,1x,I2,1x,I2)' ) &
               simulation_start_year,   simulation_start_month,                     &
               simulation_start_day,    simulation_start_hour,                      &
               simulation_start_minute, simulation_start_second
#endif
        CALL nl_set_simulation_start_year   ( 1 , simulation_start_year   )
        CALL nl_set_simulation_start_month  ( 1 , simulation_start_month  )
        CALL nl_set_simulation_start_day    ( 1 , simulation_start_day    )
        CALL nl_set_simulation_start_hour   ( 1 , simulation_start_hour   )
        CALL nl_set_simulation_start_minute ( 1 , simulation_start_minute )
        CALL nl_set_simulation_start_second ( 1 , simulation_start_second )
        IF ( switch .EQ. input_only  ) THEN
          WRITE(wrf_err_message,*) ' input_wrf, input_only:  SIMULATION_START_DATE = ', &
                                  simulation_start_date(1:19)
          CALL wrf_debug ( 300 , TRIM(wrf_err_message ) )
        ELSE IF ( switch .EQ. restart_only  ) THEN
          WRITE(wrf_err_message,*) ' input_wrf, restart_only:  SIMULATION_START_DATE = ', &
                                  simulation_start_date(1:19)
          CALL wrf_debug ( 300 , TRIM(wrf_err_message ) )
        ENDIF
      ELSE
        CALL nl_get_start_year   ( 1 , simulation_start_year   )
        CALL nl_get_start_month  ( 1 , simulation_start_month  )
        CALL nl_get_start_day    ( 1 , simulation_start_day    )
        CALL nl_get_start_hour   ( 1 , simulation_start_hour   )
        CALL nl_get_start_minute ( 1 , simulation_start_minute )
        CALL nl_get_start_second ( 1 , simulation_start_second )
        CALL nl_set_simulation_start_year   ( 1 , simulation_start_year   )
        CALL nl_set_simulation_start_month  ( 1 , simulation_start_month  )
        CALL nl_set_simulation_start_day    ( 1 , simulation_start_day    )
        CALL nl_set_simulation_start_hour   ( 1 , simulation_start_hour   )
        CALL nl_set_simulation_start_minute ( 1 , simulation_start_minute )
        CALL nl_set_simulation_start_second ( 1 , simulation_start_second )
        IF ( reset_simulation_start ) THEN
          CALL wrf_message('input_wrf: forcing SIMULATION_START_DATE = head_grid start time')
          CALL wrf_message('           due to namelist variable reset_simulation_start')
        ELSE
          CALL wrf_message('input_wrf: SIMULATION_START_DATE not available in input')
          CALL wrf_message('will use head_grid start time from namelist')
        ENDIF
      ENDIF
      ! Initialize derived time quantity in grid%xtime.
      ! Note that this call is also made in setup_timekeeping().
      ! Ugh, what a hack.  Simplify all this later...
      CALL domain_clock_get( grid, minutesSinceSimulationStart=grid%xtime )
      ! Note that it is NOT necessary to reset grid%julian here.
      WRITE(wrf_err_message,*) 'input_wrf:  set xtime to ',grid%xtime
      CALL wrf_debug ( 100, TRIM(wrf_err_message) )
    ELSE IF ( switch .EQ. auxinput1_only ) then
      CALL wrf_get_dom_ti_char ( fid , 'SIMULATION_START_DATE' , first_date_input , ierr )
      WRITE(wrf_err_message,*)'metgrid input_wrf.F first_date_input = ',first_date_input
      CALL wrf_message(wrf_err_message)
      CALL nl_get_start_year   ( 1 , first_date_start_year   )
      CALL nl_get_start_month  ( 1 , first_date_start_month  )
      CALL nl_get_start_day    ( 1 , first_date_start_day    )
      CALL nl_get_start_hour   ( 1 , first_date_start_hour   )
      CALL nl_get_start_minute ( 1 , first_date_start_minute )
      CALL nl_get_start_second ( 1 , first_date_start_second )
      WRITE ( first_date_nml, fmt = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
             first_date_start_year,   first_date_start_month,                     &
             first_date_start_day,    first_date_start_hour,                      &
             first_date_start_minute, first_date_start_second
      WRITE (wrf_err_message,*) 'metgrid input_wrf.F first_date_nml = ',first_date_nml
      CALL wrf_message( TRIM(wrf_err_message ) )
    ENDIF

    !  Test to make sure that the input data is the right size.  Do this for input from real/ideal into
    !  WRF, and from the standard initialization into real.

    IF ( ( switch .EQ.     input_only  ) .OR. &
         ( switch .EQ. auxinput1_only ) ) THEN
       ierr = 0
       CALL wrf_get_dom_ti_integer ( fid , 'WEST-EAST_GRID_DIMENSION' ,    ide_compare , 1 , icnt , ierr3 )
       ierr = max( ierr, ierr3 )
       CALL wrf_get_dom_ti_integer ( fid , 'SOUTH-NORTH_GRID_DIMENSION' ,  jde_compare , 1 , icnt , ierr3 )
       ierr = max( ierr, ierr3 )
       CALL wrf_get_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMENSION' ,   kde_compare , 1 , icnt , ierr3 )
       ierr = max( ierr, ierr3 )
       IF ( ierr3 .NE. 0 ) THEN
          CALL wrf_debug ( 0, '---- ERROR: wrf_get_dom_ti_integer getting dimension information from dataset' )
          count_fatal_error = count_fatal_error + 1
       END IF

#if (EM_CORE == 1)

       IF ( switch .EQ. input_only ) then
          CALL wrf_get_dom_ti_char ( fid , 'SIMULATION_START_DATE' , simulation_start_date , ierr )
       END IF

       !  Test to make sure that the grid distances are the right size.

       CALL wrf_get_dom_ti_real ( fid , 'DX' ,  dx_compare , 1 , icnt , ierr )
       CALL wrf_get_dom_ti_real ( fid , 'DY' ,  dy_compare , 1 , icnt , ierr )

       IF ( ( ABS ( dx_compare - config_flags%dx ) .GT. 1.E-5 * dx_compare ) .OR. &
            ( ABS ( dy_compare - config_flags%dy ) .GT. 1.E-5 * dy_compare ) ) THEN
          IF ( ( config_flags%polar ) .AND. ( config_flags%grid_id .EQ. 1 ) ) THEN
             WRITE(wrf_err_message,*)'input_wrf: DX and DY from input file expected to be wrong'
             CALL wrf_debug ( 1 , wrf_err_message )
          ELSE
             WRITE(wrf_err_message,*)'dx and dy from file     ',dx_compare,dy_compare
             CALL wrf_message(wrf_err_message)
             WRITE(wrf_err_message,*)'dx and dy from namelist ',config_flags%dx,config_flags%dy
             CALL wrf_message(wrf_err_message)
             CALL wrf_debug ( 0, '---- ERROR: DX and DY do not match comparing namelist to the input file' )
             count_fatal_error = count_fatal_error + 1
          END IF
       END IF
#endif
    END IF

#if (EM_CORE == 1)
    IF ( ( switch .EQ. input_only ) .OR. ( switch .EQ. auxinput2_only ) .OR. ( switch .EQ. auxinput1_only ) ) THEN
       ierr  = 0
       ierr3 = 0
       IF      ( ( ( switch .EQ. input_only ) .AND. ( grid%id .GE. 2 ) ) .OR. ( switch .EQ. auxinput2_only ) ) THEN
          CALL wrf_get_dom_ti_integer ( fid , 'I_PARENT_START' ,    itmp , 1 , icnt , ierr3 )
       ELSE IF ( ( switch .EQ. auxinput1_only ) .AND. ( grid%id .GE. 2 ) ) THEN
          CALL wrf_get_dom_ti_integer ( fid , 'i_parent_start' ,    itmp , 1 , icnt , ierr3 )
       ELSE IF ( ( ( switch .EQ. auxinput1_only ) .OR. ( switch .EQ. input_only ) ) .AND. ( grid%id .EQ. 1 ) ) THEN
          itmp  = config_flags%i_parent_start
          ierr3 = 0 
       END IF
       ierr = max( ierr, ierr3 )
       IF ( itmp .NE. config_flags%i_parent_start ) THEN
          ierr = 1
          WRITE(wrf_err_message,*)'i_parent_start from namelist.input file = ',config_flags%i_parent_start
          CALL wrf_message(wrf_err_message)
          WRITE(wrf_err_message,*)'i_parent_start from gridded input file  = ',itmp
          CALL wrf_message(wrf_err_message)
       END IF
       IF      ( ( ( switch .EQ. input_only ) .AND. ( grid%id .GE. 2 ) ) .OR. ( switch .EQ. auxinput2_only ) ) THEN
          CALL wrf_get_dom_ti_integer ( fid , 'J_PARENT_START' ,    itmp , 1 , icnt , ierr3 )
       ELSE IF ( ( switch .EQ. auxinput1_only ) .AND. ( grid%id .GE. 2 ) ) THEN
          CALL wrf_get_dom_ti_integer ( fid , 'j_parent_start' ,    itmp , 1 , icnt , ierr3 )
       ELSE IF ( ( ( switch .EQ. auxinput1_only ) .OR. ( switch .EQ. input_only ) ) .AND. ( grid%id .EQ. 1 ) ) THEN
          itmp  = config_flags%j_parent_start
          ierr3 = 0 
       END IF
       ierr = max( ierr, ierr3 )
       IF ( itmp .NE. config_flags%j_parent_start ) THEN
          ierr = 1
          WRITE(wrf_err_message,*)'j_parent_start from namelist.input file = ',config_flags%j_parent_start
          CALL wrf_message(wrf_err_message)
          WRITE(wrf_err_message,*)'j_parent_start from gridded input file  = ',itmp
          CALL wrf_message(wrf_err_message)
       END IF
       IF ( ierr .NE. 0 ) THEN
          CALL wrf_debug ( 0, '---- ERROR: Nest start locations do not match: namelist.input vs gridded input file' )
          count_fatal_error = count_fatal_error + 1
       END IF
    END IF
#endif

    ! do the check later (see check_if_dryrun below)

    !  We do not want the CEN_LAT LON values from the boundary file.  For 1-way nests
    !  with ndown, this ends up being the data from the previous coarse domain.

    IF ( switch .NE. boundary_only ) THEN
       CALL wrf_get_dom_ti_real ( fid , 'CEN_LAT' ,  config_flags%cen_lat , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LAT returns ',config_flags%cen_lat
       CALL wrf_debug ( 300 , wrf_err_message )
       CALL nl_set_cen_lat ( grid%id , config_flags%cen_lat )

       CALL wrf_get_dom_ti_real ( fid , 'CEN_LON' ,  config_flags%cen_lon , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LON returns ',config_flags%cen_lon
       CALL wrf_debug ( 300 , wrf_err_message )
       CALL nl_set_cen_lon ( grid%id , config_flags%cen_lon )
    ELSE
       CALL wrf_get_dom_ti_real ( fid , 'CEN_LAT' ,  dum , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LAT returns ',dum
       CALL wrf_debug ( 300 , wrf_err_message )

       CALL wrf_get_dom_ti_real ( fid , 'CEN_LON' ,  dum , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LON returns ',dum
       CALL wrf_debug ( 300 , wrf_err_message )
    END IF

    CALL wrf_get_dom_ti_real ( fid , 'TRUELAT1' ,  config_flags%truelat1 , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TRUELAT1 returns ',config_flags%truelat1
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_truelat1 ( grid%id , config_flags%truelat1 )

    CALL wrf_get_dom_ti_real ( fid , 'TRUELAT2' ,  config_flags%truelat2 , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TRUELAT2 returns ',config_flags%truelat2
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_truelat2 ( grid%id , config_flags%truelat2 )

    CALL wrf_get_dom_ti_real ( fid , 'MOAD_CEN_LAT' ,  config_flags%moad_cen_lat , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for MOAD_CEN_LAT returns ',config_flags%moad_cen_lat
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_moad_cen_lat ( grid%id , config_flags%moad_cen_lat )

    CALL wrf_get_dom_ti_real ( fid , 'STAND_LON' ,  config_flags%stand_lon , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for STAND_LON returns ',config_flags%stand_lon
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_stand_lon ( grid%id , config_flags%stand_lon )

#if ( NMM_CORE != 1 )

    CALL wrf_get_dom_ti_real ( fid , 'POLE_LAT' ,  config_flags%pole_lat , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for POLE_LAT returns ',config_flags%pole_lat
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_pole_lat ( grid%id , config_flags%pole_lat )

    CALL wrf_get_dom_ti_real ( fid , 'POLE_LON' ,  config_flags%pole_lon , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for POLE_LON returns ',config_flags%pole_lon
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_pole_lon ( grid%id , config_flags%pole_lon )

! program_name is defined in module_domain and set in the main program for whatever application
! is using subroutine input_wrf (that is, the subroutine you are looking at here). Data files
! written by SI have P_TOP as a metadata item; the real program and wrf model have it as a
! state variable. This test is to supress non-fatal but confusing messages from the model complaining
! that P_TOP cannot be read from the metadata for this dataset.  JM 20040905
!
! Note, P_TOP is not defined in the NMM core.

    IF ( program_name(1:7) .EQ. "REAL_EM" ) THEN
      CALL wrf_get_dom_ti_real ( fid , 'P_TOP' ,  grid%p_top , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for P_TOP returns ',grid%p_top
      CALL wrf_debug ( 300 , wrf_err_message )
    ENDIF
#endif

    IF ( switch .NE. boundary_only ) THEN
      CALL wrf_get_dom_ti_real ( fid , 'GMT' ,  config_flags%gmt , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for GMT returns ',config_flags%gmt
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL nl_set_gmt ( grid%id , config_flags%gmt )

      CALL wrf_get_dom_ti_integer ( fid , 'JULYR' ,  config_flags%julyr , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for JULYR returns ',config_flags%julyr
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL nl_set_julyr ( grid%id , config_flags%julyr )

      CALL wrf_get_dom_ti_integer ( fid , 'JULDAY' ,  config_flags%julday , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for JULDAY returns ',config_flags%julday
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL nl_set_julday ( grid%id , config_flags%julday )
    ENDIF

    CALL wrf_get_dom_ti_integer ( fid , 'MAP_PROJ' ,  config_flags%map_proj , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for MAP_PROJ returns ',config_flags%map_proj
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_map_proj ( grid%id , config_flags%map_proj )
    grid%map_proj = config_flags%map_proj

    mminlu = " "
    CALL wrf_get_dom_ti_char ( fid , 'MMINLU', mminlu , ierr )
    IF ( ierr .NE. 0 ) mminlu = " "
#if ( ( EM_CORE == 1 ) && ( DA_CORE != 1 ) )
    IF ( ( ( switch .EQ. input_only ) .OR. ( switch .EQ. restart_only ) ) .AND. &
         ( ( config_flags%io_form_input .EQ.  2 ) .OR. &
           ( config_flags%io_form_input .EQ. 11 ) ) ) THEN
      CALL wrf_get_dom_ti_char ( fid , 'SIMULATION_INITIALIZATION_TYPE', sim_type , icnt )
      IF ( TRIM(sim_type) .NE. "IDEALIZED DATA" ) THEN
        IF ( ierr .NE. 0 ) THEN
          WRITE(wrf_err_message,*)'MMINLU error on input'
          mminlu = " "
          CALL wrf_debug ( 0 , wrf_err_message )
        END IF
      END IF
    ELSE IF ( ( switch .EQ. input_only ) .OR. ( switch .EQ. restart_only ) ) THEN
      IF ( ierr .NE. 0 ) THEN
        WRITE(wrf_err_message,*)'MMINLU error on input'
        mminlu = " "
        CALL wrf_debug ( 0 , wrf_err_message )
      END IF
    END IF
#endif
    IF ( ierr .EQ. 0 ) THEN
      IF ( ( ( mminlu(1:1) .GE. "A" ) .AND. ( mminlu(1:1) .LE. "Z" ) ) .OR. &
                ( ( mminlu(1:1) .GE. "a" ) .AND. ( mminlu(1:1) .LE. "z" ) ) .OR. &
                ( ( mminlu(1:1) .GE. "0" ) .AND. ( mminlu(1:1) .LE. "9" ) ) ) THEN
         ! no-op, the mminlu field is probably OK
      ELSE IF ( mminlu(1:1) .EQ. " " ) THEN
         mminlu = " "
      ELSE
         mminlu = " "
      END IF
    END IF
    call wrf_debug( 1 , "mminlu = '" // TRIM(mminlu) // "'")
    if (index(mminlu, char(0)) > 0) mminlu(index(mminlu, char(0)):) = " "
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_char for MMINLU returns ' // TRIM(mminlu)
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_mminlu ( grid%id, mminlu )

    ! Test to make sure that the number of land categories is set correctly
    ! The default is set to 24 somewhere, from the number of categories
    ! in the traditional USGS dataset

    IF ( ( switch .EQ. input_only )       .OR. & 
         ( switch .EQ. auxinput1_only )   .OR. &
         ( switch .EQ. auxinput2_only ) ) THEN
      call wrf_get_dom_ti_integer(fid, "NUM_LAND_CAT", num_land_cat_compare, 1, icnt, ierr)
      if ( (ierr .NE. 0) .OR. ( num_land_cat_compare .LT. 1 ) .OR. ( num_land_cat_compare .GT. 1000 ) ) then
        IF (mminlu == 'MODIFIED_IGBP_MODIS_NOAH') THEN
          call wrf_debug( 1 , "Must be old WPS data, assuming 20 levels for NUM_LAND_CAT")
          num_land_cat_compare = 20
        ELSE
          call wrf_debug( 1 , "Must be old WPS data, assuming 24 levels for NUM_LAND_CAT")
          num_land_cat_compare = 24
        END IF
      endif
      if ( config_flags%num_land_cat /= num_land_cat_compare ) then
        call wrf_message("----------------- ERROR -------------------")
        WRITE(wrf_err_message,'("namelist    : NUM_LAND_CAT = ",I10)') config_flags%num_land_cat
        call wrf_message(wrf_err_message)
        WRITE(wrf_err_message,'("input files : NUM_LAND_CAT = ",I10, " (from geogrid selections).")') num_land_cat_compare
        call wrf_message(wrf_err_message)
        CALL wrf_debug ( 0, '---- ERROR: Mismatch between namelist and wrf input files for dimension NUM_LAND_CAT')
        count_fatal_error = count_fatal_error + 1
      endif
    ENDIF

    ! Test here to check that config_flags%num_metgrid_soil_levels in namelist
    ! is equal to what is in the global attributes of the met_em files.  Note that
    ! if this is not the first time period, we don't really care about soil data.

    IF ( ( switch .EQ. auxinput1_only ) .AND. &
         ( first_date_nml .EQ. first_date_input ) )  THEN
       CALL wrf_get_dom_ti_integer ( fid, 'NUM_METGRID_SOIL_LEVELS', itmp, 1, icnt, ierr )
   
       IF ( ierr .EQ. 0 ) THEN

#if (EM_CORE == 1)
          IF ( itmp .EQ. 1 ) THEN
             CALL wrf_debug ( 0, "---- ERROR: NUM_METGRID_SOIL_LEVELS must be greater than 1")
             count_fatal_error = count_fatal_error + 1
          END IF
#endif
          WRITE(wrf_err_message,*)'input_wrf: global attribute NUM_METGRID_SOIL_LEVELS returns ', itmp
          CALL wrf_debug ( 300 , wrf_err_message )
          IF ( config_flags%num_metgrid_soil_levels /= itmp ) THEN
             call wrf_message("----------------- ERROR -------------------")
             WRITE(wrf_err_message,'("namelist    : num_metgrid_soil_levels = ",I10)') config_flags%num_metgrid_soil_levels
             call wrf_message(wrf_err_message)
#if (EM_CORE == 1)
             WRITE(wrf_err_message,'("input files : NUM_METGRID_SOIL_LEVELS = ",I10, " (from met_em files).")') itmp
#else
             WRITE(wrf_err_message,'("input files : NUM_METGRID_SOIL_LEVELS = ",I10, " (from met_nmm files).")') itmp
#endif
             call wrf_message(wrf_err_message)
             CALL wrf_debug ( 0, "---- ERROR: Mismatch between namelist and global attribute NUM_METGRID_SOIL_LEVELS")
             count_fatal_error = count_fatal_error + 1
          END IF
       END IF
    END IF

#if 0
#if ( WRF_CHEM == 1 )
    !  Dust erosion static data.

    CALL wrf_get_dom_ti_integer ( fid, 'EROSION_DIM', itmp, 1, icnt, ierr )

    IF ( ierr .EQ. 0 ) THEN
       WRITE(wrf_err_message,*)'input_wrf: global attribute EROSION_DIM returns ', itmp
       CALL wrf_debug ( 300 , wrf_err_message )
       IF ( config_flags%erosion_dim /= itmp ) THEN
          call wrf_message("----------------- ERROR -------------------")
          WRITE(wrf_err_message,'("namelist    : erosion_dim = ",I10)') config_flags%erosion_dim
          call wrf_message(wrf_err_message)
          WRITE(wrf_err_message,'("input files : EROSION_DIM = ",I10, " (from met_em files).")') itmp
          call wrf_message(wrf_err_message)
          CALL wrf_debug ( 0, "---- ERROR: Mismatch between namelist and global attribute EROSION_DIM")
          count_fatal_error = count_fatal_error + 1
       END IF
    END IF
#endif
#endif

#if ( DA_CORE != 1 )
    ! Test here to check that config_flags%sf_surface_physics in namelist
    ! is equal to what is in the global attributes of the wrfinput files

    IF ( config_flags%sf_surface_physics /= 0 ) THEN
       IF ( switch .EQ. input_only  ) THEN
          CALL wrf_get_dom_ti_integer ( fid, 'SF_SURFACE_PHYSICS', itmp, 1, icnt, ierr )
          IF ( ierr .EQ. 0 ) THEN
             WRITE(wrf_err_message,*)'input_wrf: global attribute SF_SURFACE_PHYSICS returns ', itmp
             CALL wrf_debug ( 300 , wrf_err_message )
             IF ( config_flags%sf_surface_physics /= itmp ) THEN
                IF ( ( config_flags%sf_surface_physics == LSMSCHEME ) .and. ( itmp == NOAHMPSCHEME ) ) then
                   ! All is well.  Noah-MP and Noah have compatible wrfinput files.
                ELSE IF ( ( config_flags%sf_surface_physics == NOAHMPSCHEME ) .and. ( itmp == LSMSCHEME ) ) then
                   ! All is well.  Noah-MP and Noah have compatible wrfinput files.
                ELSE
                   call wrf_message("----------------- ERROR -------------------")
                   WRITE(wrf_err_message,'("namelist    : sf_surface_physics = ",I10)') config_flags%sf_surface_physics
                   call wrf_message(wrf_err_message)
                   WRITE(wrf_err_message,'("input files : SF_SURFACE_PHYSICS = ",I10, " (from wrfinput files).")') itmp
                   call wrf_message(wrf_err_message)
                   CALL wrf_debug ( 0, "---- ERROR: Mismatch between namelist and global attribute SF_SURFACE_PHYSICS")
                   count_fatal_error = count_fatal_error + 1
                END IF
             END IF
          END IF
       END IF
    END IF


    ! Test here to check that config_flags%gwd_opt in namelist
    ! is equal to what is in the global attributes of the wrfinput files

    IF ( config_flags%gwd_opt /= 0 ) THEN
       IF ( switch .EQ. input_only  ) THEN
          CALL wrf_get_dom_ti_integer ( fid, 'GWD_OPT', itmp, 1, icnt, ierr )
          IF ( ierr .EQ. 0 ) THEN
             WRITE(wrf_err_message,*)'input_wrf: global attribute GWD_OPT returns ', itmp
             CALL wrf_debug ( 300 , wrf_err_message )
             IF ( config_flags%gwd_opt /= itmp ) THEN
                call wrf_message("----------------- ERROR -------------------")
                WRITE(wrf_err_message,'("namelist    : gwd_opt            = ",I10)') config_flags%gwd_opt
                call wrf_message(wrf_err_message)
                WRITE(wrf_err_message,'("input files : GWD_OPT            = ",I10, " (from wrfinput files).")') itmp
                call wrf_message(wrf_err_message)
                call wrf_debug ( 0, "---- ERROR: Mismatch between namelist and global attribute GWD_OPT")
                count_fatal_error = count_fatal_error + 1
             END IF
          END IF
       END IF
    END IF
#endif

#if ( ( EM_CORE == 1 ) && ( DA_CORE != 1 ) )
    ! Test here to check that config_flags%sf_ocean_physics in namelist
    ! is equal to what is in the global attributes of the wrfinput files

    IF ( config_flags%sf_ocean_physics /= 0 ) THEN
       IF ( switch .EQ. input_only  ) THEN
          CALL wrf_get_dom_ti_integer ( fid, 'SF_OCEAN_PHYSICS', itmp, 1, icnt, ierr )
          IF ( ierr .EQ. 0 ) THEN
             WRITE(wrf_err_message,*)'input_wrf: global attribute SF_OCEAN_PHYSICS returns ', itmp
             CALL wrf_debug ( 300 , wrf_err_message )
             IF ( config_flags%sf_ocean_physics /= itmp ) THEN
                call wrf_message("----------------- ERROR -------------------")
                WRITE(wrf_err_message,'("namelist    : sf_ocean_physics   = ",I10)') config_flags%sf_ocean_physics
                call wrf_message(wrf_err_message)
                WRITE(wrf_err_message,'("input files : SF_OCEAN_PHYSICS   = ",I10, " (from wrfinput files).")') itmp
                call wrf_message(wrf_err_message)
                call wrf_debug ( 0, "---- ERROR: Mismatch between namelist and global attribute SF_OCEAN_PHYSICS")
                count_fatal_error = count_fatal_error + 1
             END IF
          END IF
       END IF
    END IF
#endif

#if ( DA_CORE != 1 )
    ! Test here to check that config_flags%sf_urban_physics in namelist
    ! is equal to the value listed the global attributes of the wrfinput files.
    ! We only perform this check if the WRF model has sf_urban_physics turned on.
    ! If the WRF model runs with nml sf_urban_physics==0, then any setting of the 
    ! sf_urban_physics in the metadata is acceptable, so not test is required.

    IF ( config_flags%sf_urban_physics /= 0 ) THEN
       IF ( switch .EQ. input_only  ) THEN
          CALL wrf_get_dom_ti_integer ( fid, 'SF_URBAN_PHYSICS', itmp, 1, icnt, ierr )
          IF ( ierr .EQ. 0 ) THEN
             WRITE(wrf_err_message,*)'input_wrf: global attribute SF_URBAN_PHYSICS returns ', itmp
             CALL wrf_debug ( 300 , wrf_err_message )
             IF ( config_flags%sf_urban_physics /= itmp ) THEN
                call wrf_message("----------------- ERROR -------------------")
                WRITE(wrf_err_message,'("namelist    : sf_urban_physics   = ",I10)') config_flags%sf_urban_physics
                call wrf_message(wrf_err_message)
                WRITE(wrf_err_message,'("input files : SF_URBAN_PHYSICS   = ",I10, " (from wrfinput files).")') itmp
                call wrf_message(wrf_err_message)
                call wrf_debug ( 0, "---- ERROR: Mismatch between namelist and global attribute SF_URBAN_PHYSICS")
                count_fatal_error = count_fatal_error + 1
             END IF
          END IF
       END IF
    END IF
#endif

    CALL wrf_get_dom_ti_integer ( fid , 'ISWATER' ,  config_flags%iswater , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISWATER returns ',config_flags%iswater
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         IF (mminlu == 'UMD') THEN
              config_flags%iswater = 14
         ELSE IF (mminlu == 'MODIFIED_IGBP_MODIS_NOAH') THEN
              config_flags%iswater = 17
         ELSE
              config_flags%iswater = 16
         ENDIF
    ENDIF
    CALL nl_set_iswater ( grid%id , config_flags%iswater )
    grid%iswater = config_flags%iswater

    CALL wrf_get_dom_ti_integer ( fid , 'ISLAKE' ,  config_flags%islake , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISLAKE returns ',config_flags%islake
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         config_flags%islake = -1
    ENDIF
    CALL nl_set_islake ( grid%id , config_flags%islake )
    grid%islake = config_flags%islake

    CALL wrf_get_dom_ti_integer ( fid , 'ISICE' ,  config_flags%isice , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISICE returns ',config_flags%isice
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE.  0 ) THEN
         IF (mminlu == 'UMD') THEN
              config_flags%isice = 14
         ELSE IF (mminlu == 'MODIFIED_IGBP_MODIS_NOAH') THEN
              config_flags%isice = 15
         ELSE
              config_flags%isice = 24
         ENDIF
    ENDIF
    CALL nl_set_isice ( grid%id , config_flags%isice )
    grid%isice = config_flags%isice

    CALL wrf_get_dom_ti_integer ( fid , 'ISURBAN' ,  config_flags%isurban , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISURBAN returns ',config_flags%isurban
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         IF (mminlu == 'UMD') THEN
              config_flags%isurban = 13
         ELSE IF (mminlu == 'MODIFIED_IGBP_MODIS_NOAH') THEN
              config_flags%isurban = 13
         ELSE
              config_flags%isurban = 1
         ENDIF
    ENDIF
    CALL nl_set_isurban ( grid%id , config_flags%isurban )
    grid%isurban = config_flags%isurban

    CALL wrf_get_dom_ti_integer ( fid , 'ISOILWATER' ,  config_flags%isoilwater , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISOILWATER returns ',config_flags%isoilwater
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         config_flags%isoilwater = 14
    ENDIF
    CALL nl_set_isoilwater ( grid%id , config_flags%isoilwater )
    grid%isoilwater = config_flags%isoilwater

#ifdef MOVE_NESTS
! Added these fields for restarting of moving nests, JM
! DANGER and TODO
! It is very important that these be set correctly if they are set at all in here.
! Garbage values will produce unpredictable results, possibly segfaults, in the nesting
! code. Need some integrity checking here or elsewhere in the code to at least check to
! make sure that the istart and jstart values make sense with respect to the nest dimensions
! and the position in the parent domain.
    CALL wrf_get_dom_ti_integer ( fid , 'I_PARENT_START' ,  itmp  , 1 , icnt, ierr )
    IF ( ierr .EQ. 0 .AND. switch .EQ. restart_only ) THEN
      config_flags%i_parent_start = itmp
      CALL nl_set_i_parent_start ( grid%id , config_flags%i_parent_start )
    ENDIF
    CALL wrf_get_dom_ti_integer ( fid , 'J_PARENT_START' ,  itmp  , 1 , icnt, ierr )
    IF ( ierr .EQ. 0 .AND. switch .EQ. restart_only ) THEN
      config_flags%j_parent_start = itmp
      CALL nl_set_j_parent_start ( grid%id , config_flags%j_parent_start )
    ENDIF
#endif

#if (EM_CORE == 1)

!KLUDGE - is there a more elegant way to determine "old si" input
    IF      ( ( switch .EQ.     input_only  ) .OR. &
            ( ( switch .EQ. auxinput1_only ) .AND. &
              ( config_flags%auxinput1_inname(1:8) .EQ. 'wrf_real' ) ) ) THEN

       !  Test to make sure that the input data is the right size: real into WRF.

       IF ( ide .NE. ide_compare ) THEN
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  namelist e_we                         = ',ide
          CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  input file WEST-EAST_GRID_DIMENSION   = ',ide_compare
          CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
          CALL wrf_debug ( 0, "---- ERROR: Mismatch between namelist and input file dimensions" )
          count_fatal_error = count_fatal_error + 1
       ENDIF
       IF ( jde .NE. jde_compare ) THEN
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  namelist e_sn                         = ',jde
          CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  input file SOUTH-NORTH_GRID_DIMENSION = ',jde_compare
          CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
          CALL wrf_debug ( 0, "---- ERROR: Mismatch between namelist and input file dimensions" )
          count_fatal_error = count_fatal_error + 1
       ENDIF
       IF ( kde .NE. kde_compare ) THEN
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  namelist e_vert                       = ',kde
          CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  input file BOTTOM-TOP_GRID_DIMENSION  = ',kde_compare
          CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
          CALL wrf_debug ( 0, "---- ERROR: Mismatch between namelist and input file dimensions" )
          count_fatal_error = count_fatal_error + 1
       ENDIF

    ELSE IF ( switch .EQ. auxinput1_only ) THEN

       !  Test to make sure that the input data is the right size: metgrid into real.

       IF ( ide                             .NE. ide_compare ) THEN
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  namelist e_we                         = ',ide
          CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  input file WEST-EAST_GRID_DIMENSION   = ',ide_compare
          CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
          CALL wrf_debug ( 0, "---- ERROR: Mismatch between namelist and input file dimensions" )
          count_fatal_error = count_fatal_error + 1
       ENDIF
       IF ( jde                             .NE. jde_compare ) THEN
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  namelist e_sn                         = ',jde
          CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  input file SOUTH-NORTH_GRID_DIMENSION = ',jde_compare
          CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
          CALL wrf_debug ( 0, "---- ERROR: Mismatch between namelist and input file dimensions" )
          count_fatal_error = count_fatal_error + 1
       ENDIF
       IF ( config_flags%num_metgrid_levels .NE. kde_compare ) THEN
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  namelist num_metgrid_levels           = ',config_flags%num_metgrid_levels
          CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  input file BOTTOM-TOP_GRID_DIMENSION  = ',kde_compare
          CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
          CALL wrf_debug ( 0, "---- ERROR: Mismatch between namelist and input file dimensions" )
          count_fatal_error = count_fatal_error + 1
       ENDIF
    ENDIF

#endif

#if (NMM_CORE == 1)

    IF      ( ( switch .EQ. auxinput1_only  ) .AND. &
              ( config_flags%auxinput1_inname(1:8) .EQ. 'wrf_real' ) ) THEN

       CALL wrf_get_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMENSION' ,   kde_compare , 1 , icnt , ierr3 )

       !  Test to make sure that the input data is the right size.

       IF ( ( ide-1 .NE. ide_compare    ) .OR. &
            ( kde   .NE. kde_compare    ) .OR. &
            ( jde-1 .NE. jde_compare    ) .AND. ierr3 .EQ. 0 ) THEN
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  namelist ide-1,jde-1,kde=',ide-1,jde-1,kde,&
                                  '; input data ide,jde,kde=',ide_compare , jde_compare , kde_compare
          CALL wrf_debug( 100, wrf_err_message )
       ENDIF

       ELSEIF ( switch .EQ. auxinput1_only ) THEN          ! assume just WPS in this branch
         IF ( ( ide-1                           .NE. ide_compare ) .OR. &
            ( config_flags%num_metgrid_levels .NE. kde_compare ) .OR. &
            ( jde-1                             .NE. jde_compare ) .AND. ierr3 .EQ. 0 ) THEN
                WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  ',&
                 'namelist ide-1,jde-1,num_metgrid_levels=',ide-1,jde-1,config_flags%num_metgrid_levels,&
                 '; input data ide,jde,num_metgrid_levels=',ide_compare , jde_compare , kde_compare
                IF (ide-1 .eq. ide_compare .AND. jde-1 .EQ. jde_compare) THEN
                  CALL wrf_message(wrf_err_message)
                  CALL wrf_debug ( 0, "---- ERROR: appears that the vertical dimension is wrong - quitting" )
                  count_fatal_error = count_fatal_error + 1
                ELSE
                  CALL wrf_message(wrf_err_message)
                  CALL wrf_debug ( 0, "---- ERROR: appears that I or J dimensions are wrong - quitting" )
                  count_fatal_error = count_fatal_error + 1
                ENDIF
         ENDIF
       ENDIF

#endif

#if (DA_CORE == 1)
    ! Test here to check that config_flags%hypsometric_opt in namelist
    ! is equal to what is in the global attributes of the wrfinput files

    IF ( switch .EQ. input_only  ) THEN
       CALL wrf_get_dom_ti_integer ( fid, 'HYPSOMETRIC_OPT', itmp, 1, icnt, ierr )
       IF ( ierr .EQ. 0 ) THEN
          WRITE(wrf_err_message,*)'input_wrf: global attribute HYPSOMETRIC_OPT returns ', itmp
          CALL wrf_debug ( 300 , wrf_err_message )
          IF ( config_flags%hypsometric_opt /= itmp ) THEN
             call wrf_message("----------------- WARNING  -------------------")
             WRITE(wrf_err_message,'("namelist    : hypsometric_opt = ",I10)') config_flags%hypsometric_opt
             call wrf_message(wrf_err_message)
             WRITE(wrf_err_message,'("input files : HYPSOMETRIC_OPT = ",I10, " (from wrfinput files).")') itmp
             call wrf_message(wrf_err_message)
             CALL wrf_debug ( 0, "---- ERROR: Mismatch between namelist and global attribute HYPSOMETRIC_OPT")
             count_fatal_error = count_fatal_error + 1
          END IF
       ELSE
          ! For WRFDA backward compatibility.  If hypsometric_opt is not defined in the fg file, it is
          ! pre-version 3.4 WRF input data.  For older versions, hypsometric_opt should be 1.
          hypsometric_opt = -1
          CALL wrf_get_dom_ti_integer ( fid , 'HYPSOMETRIC_OPT' , hypsometric_opt, 1 , icnt , ierr )
          IF ( ( hypsometric_opt .NE. 1 ) .AND. ( hypsometric_opt .NE. 2 ) ) THEN
             grid%hypsometric_opt = 1
             config_flags%hypsometric_opt = 1
             DO loop = 1 , grid%max_dom
                CALL nl_set_hypsometric_opt ( loop , 1 )
             END DO
             WRITE(wrf_err_message,*)'Background (fg) file appears to be from earlier than WRF V3.4;'
             call wrf_message(wrf_err_message)
             WRITE(wrf_err_message,*)'Resetting the hypsometric_opt from default value of 2 to 1'
             CALL wrf_message(wrf_err_message)
          END IF

       END IF
    END IF

#endif

#if ( ( EM_CORE == 1 ) && ( DA_CORE != 1 ) )
      IF ( ( switch .EQ. input_only ) .OR. ( switch .EQ. restart_only ) ) THEN

        !  Make sure for ARW ideal cases that the hypsometric option, the
        !  way that we integrate the height field, is set to 1.  This is the
        !  method that is used in all of the "ideal" programs to get the
        !  base-state height (phb).

        CALL wrf_get_dom_ti_char ( fid , 'SIMULATION_INITIALIZATION_TYPE', sim_type , ierr )
        IF      ( TRIM(sim_type) .EQ. "IDEALIZED DATA" ) THEN
           grid%this_is_an_ideal_run = .TRUE.
        ELSE IF ( TRIM(sim_type) .EQ. "REAL-DATA CASE" ) THEN
           grid%this_is_an_ideal_run = .FALSE.
        ELSE IF ( ierr .NE. 0 ) THEN
           CALL wrf_get_dom_ti_char ( fid , 'START_DATE' , input_name , ierr )
           grid%this_is_an_ideal_run = INDEX(TRIM(input_name) , '0001-' ) .NE. 0
        END IF

        IF ( grid%this_is_an_ideal_run ) THEN
          grid%hypsometric_opt = 1
          config_flags%hypsometric_opt = 1
	  DO loop = 1 , grid%max_dom
            CALL nl_set_hypsometric_opt ( loop , 1 ) 
          END DO
          WRITE(wrf_err_message,*)'NOTE:  Ideal cases always use hypsometric_opt=1, regardless of namelist setting'
          CALL wrf_debug( 1 , wrf_err_message )
        END IF
      END IF

       IF ( ( switch .EQ. input_only ) .AND. ( config_flags%io_form_input .EQ. 2 ) ) THEN

        !  For backward compatibility.  If we do not find the hypsometric_opt defined
        !  in the input data, this is pre version 3.4.  Most likely, the hypsometric_opt
        !  was the default value, 1.  

        hypsometric_opt = -1
        CALL wrf_get_dom_ti_integer ( fid , 'HYPSOMETRIC_OPT' , hypsometric_opt , 1 , icnt , ierr )
        IF ( ( hypsometric_opt .NE. 1 ) .AND. ( hypsometric_opt .NE. 2 ) ) THEN
          grid%hypsometric_opt = 1
          config_flags%hypsometric_opt = 1
	  DO loop = 1 , grid%max_dom
            CALL nl_set_hypsometric_opt ( loop , 1 ) 
          END DO
          WRITE(wrf_err_message,*)'Resetting the hypsometric_opt from default value of 2 to 1'
          CALL wrf_debug( 0 , wrf_err_message )
        END IF
       END IF

       !  In pre v4.0 data, the moist potential temperature was entirely handled in the model. In 
       !  v4.0 and later, the pre-processors handle the moist potential temperature field directly.

       !                      Input from old data
       !     
       !                        WRF NML Setting
       !                        ---------------
       !                  use_theta_m=1 | use_theta_m=0
       !                  ---------------------------
       !                  |             |           | 
       !         thetam=1 |   NO        |     OK    |
       ! Input            |             |           | 
       ! NML     ------------------------------------
       ! Setting          |             |           |
       !         thetam=0 |   NO        |     OK    |
       !                  |             |           |
       !                  ---------------------------

       !  Old input data cannot be used when the namelist has use_theta_m=1

       CALL wrf_get_dom_ti_char ( fid , 'TITLE' , version_name , ierr )
       IF ( ( switch .EQ. input_only ) .AND. &
            ( INDEX(TRIM(version_name),' V4.' ) .EQ. 0 ) .AND. &
            ( config_flags%use_theta_m .EQ. 1 ) ) THEN
          CALL wrf_debug ( 0, "---- ERROR: Cannot have old input data when requesting use_theta_m=1" )
          count_fatal_error = count_fatal_error + 1
       END IF
     
#endif

    ENDIF check_if_dryrun

!
! This call to wrf_get_next_time will position the dataset over the next time-frame
! in the file and return the current_date, which is used as an argument to the
! read_field routines in the blocks of code included below.  Note that we read the
! next time *after* all the meta data has been read. This is only important for the
! WRF internal I/O format because it is order-dependent. Other formats shouldn't care
! about this.
!

    3003 continue

    CALL wrf_get_next_time(fid, current_date , ierr)
    WRITE(wrf_err_message,*)fid,' input_wrf: wrf_get_next_time current_date: ',current_date(1:19),' Status = ',ierr
    CALL wrf_debug ( 300 , TRIM(wrf_err_message ) )
    IF ( ierr .NE. 0 .AND. ierr .NE. WRF_WARN_NOTSUPPORTED .AND. ierr .NE. WRF_WARN_DRYRUN_READ ) THEN
      CALL wrf_message ( TRIM(wrf_err_message ) )
      IF ( switch .EQ. boundary_only ) THEN
        WRITE(wrf_err_message,*) '---- ERROR: Ran out of valid boundary conditions in file ',TRIM(fname)
        CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
      ELSE
#if ( NMM_CORE != 1 )
        WRITE(wrf_err_message,*) '---- ERROR: Could not find matching time in input file ',TRIM(fname)
        CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
        count_fatal_error = count_fatal_error + 1
#endif
      ENDIF
    ELSE IF ( ierr .NE. WRF_WARN_NOTSUPPORTED .AND. ierr .NE. WRF_WARN_DRYRUN_READ) THEN
!
! check input time against domain time (which will be start time at beginning, see share/set_timekeeping.F)
! JM 20040511
!
      SELECT CASE ( switch )
        CASE ( input_only, auxinput1_only, auxinput2_only,       &
               auxinput3_only, auxinput4_only, auxinput5_only,  &
               auxinput6_only, auxinput7_only, auxinput8_only,  &
               auxinput9_only, auxinput10_only )
#if ( WRF_CHEM == 1 )
           IF( (config_flags%io_style_emissions .eq. 1) .and.                                &
              ((switch.eq.auxinput5_only) .or. (switch.eq.auxinput6_only) .or. &
               (switch.eq.auxinput7_only) .or. (switch.eq.auxinput8_only)) ) then
               CALL wrf_message( "**WARNING** Time in input file not being checked **WARNING**" )
           ELSE
#endif
            CALL wrf_atotime( current_date(1:19), time )
            CALL domain_clock_get( grid, current_time=currtime, &
                                         current_timestr=currtimestr )
#if (DA_CORE != 1) 
            IF ( (switch .EQ. input_only) .and. (grid%id .EQ. 1) .and. (grid%itimestep .EQ. 0) ) THEN
               WRITE( wrf_err_message , * ) 'CURRENT DATE          = ',TRIM( currtimestr )
               CALL wrf_message ( trim(wrf_err_message) )
               WRITE( wrf_err_message , * ) 'SIMULATION START DATE = ', simulation_start_date(1:19)
               CALL wrf_message ( TRIM(wrf_err_message ) )
            END IF
! Don't perform the check for WRFVAR, as we're not passing the right dates
! around
            CALL domain_clockprint(150, grid, &
                   'DEBUG input_wrf():  get CurrTime from clock,')
            IF ( time .NE. currtime ) THEN
                WRITE( wrf_err_message , * )'Time in file: ',trim( current_date(1:19) )
                CALL wrf_message ( trim(wrf_err_message) )
                WRITE( wrf_err_message , * )'Time on domain: ',trim( currtimestr )
                CALL wrf_message ( trim(wrf_err_message) )
                CALL wrf_message( "**WARNING** Time in input file not equal to time on domain **WARNING**" )
                WRITE(wrf_err_message,*) "**WARNING** Trying next time in file ",TRIM(fname)," ..."
                CALL wrf_message( TRIM(wrf_err_message) )
                GOTO 3003
            ENDIF
#endif
#if ( WRF_CHEM == 1 )
            ENDIF
#endif
        CASE DEFAULT
      END SELECT
    ENDIF

! set the lbc time interval fields in the domain data structure
! these time values are checked as stopping condition for the while loop in
! latbound_in() defined in share/medation_integrate.F, which is used to
! iterate forward to the correct interval in the input LBC file
!
    IF ( switch .EQ. boundary_only ) THEN
       IF ( config_flags%restart ) THEN
          ! get WRF time of current_date position in boundary file
          CALL wrf_atotime( current_date(1:19), time )
          ! jump straight to the restart time
          CALL domain_clock_get( grid, current_time=currentTime, &
                                     current_timestr=currtimestr )
          write(wrf_err_message, '(4a)') "WRF restart, LBC starts at ", &
                                     & trim(current_date), " and restart starts at ", trim(currtimestr)
          CALL wrf_debug( 0 , wrf_err_message )
          CALL wrf_get_dom_td_char ( fid , 'THISBDYTIME' ,  current_date(1:19), this_datestr , ierr )
          CALL wrf_atotime( this_datestr(1:19), grid%this_bdy_time )
          CALL wrf_get_dom_td_char ( fid , 'NEXTBDYTIME' ,  current_date(1:19), next_datestr , ierr )
          CALL wrf_atotime( next_datestr(1:19), grid%next_bdy_time )
         
          WRITE ( wrf_err_message , * ) 'LBC for restart: Starting valid date = ',this_datestr(1:19),', Ending valid date = ',next_datestr(1:19)
          CALL wrf_message( TRIM(wrf_err_message) )
          WRITE ( wrf_err_message , * ) 'LBC for restart: Restart time            = ',trim(currtimestr)
          CALL wrf_message( TRIM(wrf_err_message) )

          IF ( ( grid%this_bdy_time .LE. currentTime ) .AND. ( grid%next_bdy_time .GT. currentTime ) ) THEN
              WRITE ( wrf_err_message , * ) 'LBC for restart: Found the correct bounding LBC time periods'
              CALL wrf_message( TRIM(wrf_err_message) )
          ELSE

              WRITE ( wrf_err_message , * ) 'LBC for restart: Looking for a bounding time'
              CALL wrf_message( TRIM(wrf_err_message) )

              !  While the lateral BC time is less than the restart time, advance forward to the next LBC time.

              icount = 0
              DO WHILE ( ( currentTime .GE. grid%next_bdy_time ) .AND. ( icount < 10000 ) ) 
                  CALL wrf_get_next_time(fid, current_date , ierr)
                  IF ( ierr .NE. 0 ) THEN
                     CALL wrf_debug ( 0, '---- ERROR: Cannot find a valid time to start the LBC during this restart, likely ran out of time periods to test' )
                     count_fatal_error = count_fatal_error + 1
                  END IF
                  CALL wrf_get_dom_td_char ( fid , 'THISBDYTIME' ,  current_date(1:19), this_datestr , ierr )
                  CALL wrf_get_dom_td_char ( fid , 'NEXTBDYTIME' ,  current_date(1:19), next_datestr , ierr )
                  CALL wrf_atotime( next_datestr(1:19), grid%next_bdy_time )
                  WRITE ( wrf_err_message , * ) 'LBC for restart: Starting valid date = ',this_datestr(1:19),', Ending valid date = ',next_datestr(1:19)
                  CALL wrf_message( TRIM(wrf_err_message) )
                  icount = icount + 1
              END DO
             
              !  Now the LBC time either matches or is beyond the restart time.  If it matches, we are at the
              !  right time.  If we have gone too far, then back up one time period, and we are good to go.

              IF ( time .eq. currentTime ) THEN
                  CALL wrf_debug ( 0 , 'Found correct time, LBC matches the restart interval.' )
              ELSE IF ( time .gt. currentTime ) THEN
                  CALL wrf_debug ( 0 , 'Went one LBC interval too far, backing up for restart.' )
                  CALL wrf_get_previous_time(fid, current_date , ierr)
                  IF ( ierr .EQ. 0 ) THEN
                     CALL wrf_atotime(current_date(1:19), time)
                     WRITE(wrf_err_message,*) 'LBC: wrf_get_prev_time current_date: ',&
                                             & current_date(1:19),' Status = ',ierr
                     CALL wrf_debug ( 0 , TRIM(wrf_err_message ) )
                     CALL wrf_debug ( 0 , 'LBC is now correctly positioned for requested restart time' )
                  ELSE
                     CALL wrf_debug ( 0, '---- ERROR: Problems backing up in the LBC file to find starting location for restart' )
                     count_fatal_error = count_fatal_error + 1
                  END IF
              END IF
          END IF

          CALL wrf_get_dom_td_char ( fid , 'THISBDYTIME' ,  current_date(1:19), this_datestr , ierr )
          CALL wrf_atotime( this_datestr(1:19), grid%this_bdy_time )
          CALL wrf_get_dom_td_char ( fid , 'NEXTBDYTIME' ,  current_date(1:19), next_datestr , ierr )
          CALL wrf_atotime( next_datestr(1:19), grid%next_bdy_time )
          WRITE ( wrf_err_message , * ) 'LBC for restart: Found the correct bounding LBC time periods for restart time = ',trim(currtimestr)
          CALL wrf_message ( TRIM(wrf_err_message) )

       ELSE IF ( .NOT. config_flags%restart ) THEN
          CALL domain_clock_get( grid, current_time=currentTime )
          CALL wrf_get_dom_td_char ( fid , 'THISBDYTIME' ,  current_date(1:19), this_datestr , ierr )
          CALL wrf_atotime( this_datestr(1:19), grid%this_bdy_time )
          CALL wrf_get_dom_td_char ( fid , 'NEXTBDYTIME' ,  current_date(1:19), next_datestr , ierr )
          CALL wrf_atotime( next_datestr(1:19), grid%next_bdy_time )
       END IF
#if (DA_CORE != 1)
#if ( WRFPLUS == 1 )
       IF( config_flags%dyn_opt .NE. dyn_em_ad .AND. currentTime .GE. grid%next_bdy_time ) THEN
#else
       IF( currentTime .GE. grid%next_bdy_time ) THEN
#endif
          IF ( wrf_dm_on_monitor() ) THEN
             write(a_message,*) 'THIS TIME ',this_datestr(1:19),', NEXT TIME ',next_datestr(1:19)
             CALL wrf_message ( a_message ) 
          END IF
          RETURN
       ENDIF
#if ( WRFPLUS == 1 )
       IF( config_flags%dyn_opt .EQ. dyn_em_ad .AND. currentTime .GT. grid%next_bdy_time ) THEN
          IF ( wrf_dm_on_monitor() ) write(0,*) 'THIS TIME ',this_datestr(1:19),'NEXT TIME ',next_datestr(1:19)
          RETURN
       ENDIF
#endif
#endif
    ENDIF

#if ( ( EM_CORE == 1 ) && ( DA_CORE != 1 ) )
    !  Vertical interpolation space is required if specified by user.

    n_ref_m = config_flags%vert_refine_fact
#else
    !  Default value indicating no vertical interpolation required.

    n_ref_m = 0
#endif

    !  This test should go after all of the metadata is input, and before the gridded input is ingested.
     
    IF ( count_fatal_error .GT. 0 ) THEN
       WRITE (wrf_err_message, FMT='(A,I6, A)') 'NOTE:  ', count_fatal_error, ' namelist vs input data inconsistencies found.'
       CALL wrf_message ( wrf_err_message )
       WRITE (wrf_err_message, FMT='(A,I6, A)') 'NOTE:  Please check and reset these options'
       CALL wrf_error_fatal ( wrf_err_message )
    END IF

    IF ( (first_input   .LE. switch .AND. switch .LE. last_input) .OR. &
         (first_history .LE. switch .AND. switch .LE. last_history) .OR. &
         switch .EQ. restart_only    ) THEN
      newswitch = switch
      p => grid%head_statevars%next
      DO WHILE ( ASSOCIATED( p ) ) 
        IF ( p%ProcOrient .NE. 'X' .AND. p%ProcOrient .NE. 'Y' ) THEN   ! no I/O for xposed variables
          IF ( p%Ndim .EQ. 0 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream( p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) ) THEN
                IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                dname = p%DataName
                IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
                  IF      ( p%Type .EQ. 'r' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%rfield_0d             , & ! Field
                                    WRF_FLOAT               , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    '0'                     , & ! MemoryOrder
                                    ''                      , & ! Stagger
                     __FILE__ // ' reading 0d real ' // TRIM(p%VarName)     , & ! Debug message
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%dfield_0d             , & ! Field
                                    WRF_DOUBLE              , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    '0'                     , & ! MemoryOrder
                                    ''                      , & ! Stagger
                     __FILE__ // ' reading 0d double ' // TRIM(p%VarName)     , & ! Debug message
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%ifield_0d             , & ! Field
                                    WRF_INTEGER             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    '0'                     , & ! MemoryOrder
                                    ''                      , & ! Stagger
                     __FILE__ // ' reading 0d integer ' // TRIM(p%VarName)     , & ! Debug message
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'l' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%lfield_0d             , & ! Field
                                    WRF_LOGICAL             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    '0'                     , & ! MemoryOrder
                                    ''                      , & ! Stagger
                     __FILE__ // ' reading 0d logical ' // TRIM(p%VarName)     , & ! Debug message
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     ierr )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 1 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream( p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) ) THEN
                IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
                  dname = p%DataName
                  IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                  memord = p%MemoryOrder

                  i_inter = 0
#if ( ( EM_CORE == 1 ) && ( DA_CORE != 1 ) )
                  !  Vertical interpolation space is required if specified by user.

                  if( TRIM(p%dimname1).EQ.'bottom_top'.OR.TRIM(p%dimname1).EQ.'bottom_top_stag') i_inter = 1 
#endif
                  
                  IF      ( p%Type .EQ. 'r' ) THEN
                    IF( (i_inter.eq.1.and.n_ref_m.ge.2).and.(switch.eq.history_only) ) THEN
                       em1_c = (p%em1 - 1)/n_ref_m +1
                       ed1_c = em1_c
                       ep1_c = em1_c
                       if (TRIM(p%dimname1).EQ.'bottom_top') then
                       ed1_c = em1_c-1
                       ep1_c = em1_c-1
                       endif
                       allocate (f_vint_1d(em1_c))

                       CALL wrf_ext_read_field (  &
                                       fid                     , & ! DataHandle
                                       current_date(1:19)      , & ! DateStr
                                       TRIM(dname)             , & ! Data Name
                                       f_vint_1d               , & ! Field
                                       WRF_FLOAT               , & ! FieldType
                                       grid                    , & ! grid
                                       grid%domdesc            , & ! domdesc
                                       grid%bdy_mask           , & ! bdy_mask
                                       TRIM(memord)            , & ! MemoryOrder
                                       p%Stagger               , & ! Stagger
                        __FILE__ // ' reading 1d real ' // TRIM(p%VarName)     , & ! Debug message
                        p%sd1 , ed1_c , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                        p%sm1 , em1_c , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                        p%sp1 , ep1_c , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                        ierr )

                        do k=1,ed1_c
                        p%rfield_1d(k) = f_vint_1d(k)
                        enddo
                        deallocate (f_vint_1d)

                     ELSE
                       CALL wrf_ext_read_field (  &
                                       fid                     , & ! DataHandle
                                       current_date(1:19)      , & ! DateStr
                                       TRIM(dname)             , & ! Data Name
                                       p%rfield_1d             , & ! Field
                                       WRF_FLOAT               , & ! FieldType
                                       grid                    , & ! grid
                                       grid%domdesc            , & ! domdesc
                                       grid%bdy_mask           , & ! bdy_mask
                                       TRIM(memord)            , & ! MemoryOrder
                                       p%Stagger               , & ! Stagger
                        __FILE__ // ' reading 1d real ' // TRIM(p%VarName)     , & ! Debug message
                        p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                        p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                        p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                        ierr )
                     END IF
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%dfield_1d             , & ! Field
                                    WRF_DOUBLE              , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                     __FILE__ // ' reading 1d double ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%ifield_1d             , & ! Field
                                    WRF_INTEGER             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                     __FILE__ // ' reading 1d integer ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'l' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%lfield_1d             , & ! Field
                                    WRF_LOGICAL             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                     __FILE__ // ' reading 1d logical ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 2 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream( p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) .AND.  &
                   ( .NOT. p%subgrid_x .OR. (p%subgrid_x .AND. grid%sr_x .GT. 0) ) .AND. &
                   ( .NOT. p%subgrid_y .OR. (p%subgrid_y .AND. grid%sr_y .GT. 0) )       &
                 ) THEN
                IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
                  dname = p%DataName
                  IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                  memord = p%MemoryOrder
                  IF      ( p%Type .EQ. 'r' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%rfield_2d             , & ! Field
                                    WRF_FLOAT               , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                     __FILE__ // ' reading 2d real ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%dfield_2d             , & ! Field
                                    WRF_DOUBLE              , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                     __FILE__ // ' reading 2d double ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%ifield_2d             , & ! Field
                                    WRF_INTEGER             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                     __FILE__ // ' reading 2d integer ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'l' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%lfield_2d             , & ! Field
                                    WRF_LOGICAL             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                     __FILE__ // ' reading 2d logical ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 3 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream( p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) .AND.  &
                   ( .NOT. p%subgrid_x .OR. (p%subgrid_x .AND. grid%sr_x .GT. 0) ) .AND. &
                   ( .NOT. p%subgrid_y .OR. (p%subgrid_y .AND. grid%sr_y .GT. 0) )       &
                 ) THEN
                IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
                  dname = p%DataName
                  IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                  memord = p%MemoryOrder

                  i_inter = 0
#if ( ( EM_CORE == 1 ) && ( DA_CORE != 1 ) )
                  !  Vertical interpolation space is required if specified by user.

                  if( TRIM(p%dimname2).EQ.'bottom_top'.OR.TRIM(p%dimname2).EQ.'bottom_top_stag') i_inter = 1
#endif

                  IF      ( p%Type .EQ. 'r' ) THEN
                    IF( (i_inter.eq.1.and.n_ref_m.ge.2).and.(switch.eq.history_only) ) then
                       em2_c = (p%em2 - 1)/n_ref_m +1
                       ed2_c = em2_c
                       ep2_c = em2_c
                       if (TRIM(p%dimname2).EQ.'bottom_top') then
                       ed2_c = em2_c-1
                       ep2_c = em2_c-1
                       endif
                       allocate (f_vint_3d(p%sm1:p%em1,em2_c,p%sm3:p%em3))
                       CALL wrf_ext_read_field (  &
                                       fid                     , & ! DataHandle
                                       current_date(1:19)      , & ! DateStr
                                       TRIM(dname)             , & ! Data Name
                                       f_vint_3d               , & ! Field
                                       WRF_FLOAT               , & ! FieldType
                                       grid                    , & ! grid
                                       grid%domdesc            , & ! domdesc
                                       grid%bdy_mask           , & ! bdy_mask
                                       TRIM(memord)            , & ! MemoryOrder
                                       TRIM(p%Stagger)         , & ! Stagger
                        __FILE__ // ' reading 3d real ' // TRIM(p%VarName)     , & ! Debug message
                        p%sd1 , p%ed1 , p%sd2 , ed2_c , p%sd3 , p%ed3 ,  &
                        p%sm1 , p%em1 , p%sm2 , em2_c , p%sm3 , p%em3 ,  &
                        p%sp1 , p%ep1 , p%sp2 , ep2_c , p%sp3 , p%ep3 ,  &
                        ierr )

                        do j = p%sm3,p%em3
                        do k = 1,ed2_c
                        do i = p%sm1,p%em1
                        p%rfield_3d(i,k,j) = f_vint_3d(i,k,j)
                        enddo
                        enddo
                        enddo
                        deallocate (f_vint_3d)
                    ELSE
                       CALL wrf_ext_read_field (  &
                                       fid                     , & ! DataHandle
                                       current_date(1:19)      , & ! DateStr
                                       TRIM(dname)             , & ! Data Name
                                       p%rfield_3d             , & ! Field
                                       WRF_FLOAT               , & ! FieldType
                                       grid                    , & ! grid
                                       grid%domdesc            , & ! domdesc
                                       grid%bdy_mask           , & ! bdy_mask
                                       TRIM(memord)            , & ! MemoryOrder
                                       TRIM(p%Stagger)         , & ! Stagger
                        __FILE__ // ' reading 3d real ' // TRIM(p%VarName)     , & ! Debug message
                        p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                        p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                        p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                        ierr )
                    ENDIF
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%dfield_3d             , & ! Field
                                    WRF_DOUBLE              , & ! FieldType
                                    grid%communicator       , & ! Comm
                                    grid%iocommunicator     , & ! Comm
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                     __FILE__ // ' reading 3d double ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%ifield_3d             , & ! Field
                                    WRF_INTEGER             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                     __FILE__ // ' reading 3d integer ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
! NOTE no io on logical arrays greater than 2d
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 4 .AND. p%scalar_array ) THEN
! Use a different read routine, wrf_ext_read_field_arr, and pass in the
! tracer indeces so that p%rfield_4d can be passsed in without arguments,
! avoiding the possiblity of a copy-in/copy-out problem for some compilers.
! JM 20091208
            DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
              IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream( p%streams_table(grid%id,itrace)%stream,newswitch)) THEN
                dname = p%dname_table( grid%id, itrace )
                IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                memord = p%MemoryOrder

                i_inter = 0
#if ( ( EM_CORE == 1 ) && ( DA_CORE != 1 ) )
                !  Vertical interpolation space is required if specified by user.

                if( TRIM(p%dimname2).EQ.'bottom_top'.OR.TRIM(p%dimname2).EQ.'bottom_top_stag') i_inter = 1
#endif

                IF      ( p%Type .EQ. 'r' ) THEN
                    IF( (i_inter.eq.1.and.n_ref_m.ge.2).and.(switch.eq.history_only) ) then
                       em2_c = (p%em2 - 1)/n_ref_m +1
                       ed2_c = em2_c
                       ep2_c = em2_c
                       if (TRIM(p%dimname2).EQ.'bottom_top') then
                       ed2_c = em2_c-1
                       ep2_c = em2_c-1
                       endif
                       allocate (f_vint_4d(p%sm1:p%em1,em2_c,p%sm3:p%em3,p%num_table(grid%id)))

                       CALL wrf_ext_read_field_arr (  &
                                         fid                     , & ! DataHandle
                                         current_date(1:19)      , & ! DateStr
                                         TRIM(dname)             , & ! Data Name
                                         f_vint_4d               , & ! Field
                                         itrace, 1, 1, 1         , & ! see comment above
                                         1, 1, 1                 , & ! see comment above
                                         RWORDSIZE               , &
                                         WRF_FLOAT               , & ! FieldType
                                         grid                    , & ! grid
                                         grid%domdesc            , & ! domdesc
                                         grid%bdy_mask           , & ! bdy_mask
                                         TRIM(memord)            , & ! MemoryOrder
                                         TRIM(p%Stagger)         , & ! Stagger
                        __FILE__ // ' reading 4d real ' // TRIM(p%dname_table(grid%id,itrace))     , & ! Debug message
                        p%sd1 , p%ed1 , p%sd2 , ed2_c , p%sd3 , p%ed3 ,  &
                        p%sm1 , p%em1 , p%sm2 , em2_c , p%sm3 , p%em3 ,  &
                        p%sp1 , p%ep1 , p%sp2 , ep2_c , p%sp3 , p%ep3 ,  &
                        ierr )
                        do j = p%sm3,p%em3
                        do k = 1,ed2_c
                        do i = p%sm1,p%em1
                        p%rfield_4d(i,k,j,itrace) = f_vint_4d(i,k,j,itrace)
                        enddo
                        enddo
                        enddo
                        deallocate (f_vint_4d)
                   ELSE
                        CALL wrf_ext_read_field_arr (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%rfield_4d             , & ! Field
                                    itrace, 1, 1, 1         , & ! see comment above
                                    1, 1, 1                 , & ! see comment above
                                    RWORDSIZE               , &
                                    WRF_FLOAT               , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                       __FILE__ // ' reading 4d real ' // TRIM(p%dname_table(grid%id,itrace))     , & ! Debug message
                       p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                       p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                       p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                       ierr )
                   ENDIF
                ELSE IF ( p%Type .EQ. 'd' ) THEN
                  CALL wrf_ext_read_field_arr (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%dfield_4d             , & ! Field
                                    itrace, 1, 1, 1         , & ! see comment above
                                    1, 1, 1                 , & ! see comment above
                                    DWORDSIZE               , &
                                    WRF_DOUBLE              , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                   __FILE__ // ' reading 4d double ' // TRIM(p%dname_table(grid%id,itrace))     , & ! Debug message
                   p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                   p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                   p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                   ierr )
                ELSE IF ( p%Type .EQ. 'i' ) THEN
                  CALL wrf_ext_read_field_arr (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%ifield_4d             , & ! Field
                                    itrace, 1, 1, 1         , & ! see comment above
                                    1, 1, 1                 , & ! see comment above
                                    IWORDSIZE               , &
                                    WRF_INTEGER             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! domdesc
                                    grid%bdy_mask           , & ! bdy_mask
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                   __FILE__ // ' reading 4d integer ' // TRIM(p%dname_table(grid%id,itrace))     , & ! Debug message
                   p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                   p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                   p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                   ierr )
                ENDIF
              ENDIF
            ENDDO  ! loop over tracers
          ENDIF
        ENDIF
        p => p%next
      ENDDO
    ELSE
      IF ( switch .EQ. boundary_only ) THEN
        CALL wrf_bdyin( fid , grid , config_flags , switch , ierr )
      ENDIF
    ENDIF

#if (DA_CORE != 1)
    CALL wrf_tsin( grid , ierr )
#if (EM_CORE == 1)
    if (config_flags%track_loc_in > 0 ) then
       call track_input( grid , ierr )
    end if
#endif
#endif

    WRITE(wrf_err_message,*)'input_wrf: end, fid = ',fid
    CALL wrf_debug( 300 , wrf_err_message )

     IF ( (switch .EQ. input_only) .and. (grid%id .EQ. 1) ) THEN
         grid%dtbc = 0.0
      ENDIF

    RETURN
  END SUBROUTINE input_wrf
    
#if ( NMM_CORE != 1 )
  SUBROUTINE is_this_data_ok_to_use ( fid , yes_use_this_data )

    USE module_io
   
    IMPLICIT NONE

    INTEGER , INTENT(IN)  :: fid
    LOGICAL , INTENT(OUT) :: yes_use_this_data

    !  Local vars
  
    CHARACTER (LEN=1024) :: input_title_char
    LOGICAL :: skip_rigorous_and_return_right_away
    INTEGER :: ierr

#include "version_decl"

    !  Make sure that the input data is consistent with the current code.

    input_title_char = ' '
    CALL wrf_get_dom_ti_char ( fid , 'TITLE' , input_title_char , ierr )
    IF      ( ierr .NE. 0 ) THEN
      WRITE(wrf_err_message,*)'Error trying to read metadata'
      CALL wrf_debug( 0 , wrf_err_message )
      yes_use_this_data = .FALSE.
      RETURN
    ELSE IF ( TRIM(input_title_char) .EQ. "" ) THEN
      WRITE(wrf_err_message,*)'The type of file is empty'
      CALL wrf_debug( 0 , wrf_err_message )
      yes_use_this_data = .FALSE.
      RETURN
    END IF

    !  There are some data sets that will be immune to the rigorous vetting.

    skip_rigorous_and_return_right_away = .FALSE.

    IF ( &
         ( INDEX(TRIM(input_title_char),'METGRID'     ) .NE. 0 ) .OR. &
         ( INDEX(TRIM(input_title_char),'OBSGRID'     ) .NE. 0 ) .OR. &
         ( INDEX(TRIM(input_title_char),'SFCFDDA'     ) .NE. 0 ) .OR. &
         ( INDEX(TRIM(input_title_char),'EMISSIONS'   ) .NE. 0 ) .OR. &
         ( INDEX(TRIM(input_title_char),'XYZZY'       ) .NE. 0 )      &
       ) THEN
      WRITE(wrf_err_message,*)'Yes, this special data is acceptable to use: ' // TRIM(input_title_char)
      CALL wrf_debug( 0 , wrf_err_message )
      skip_rigorous_and_return_right_away = .TRUE.
    END IF

    !  Based on the file testing, do we return right away?

    IF ( skip_rigorous_and_return_right_away ) THEN
       yes_use_this_data = .TRUE.
       RETURN
    END IF

    IF ( INDEX(TRIM(input_title_char),' V4.' ) .NE. 0 ) THEN
      yes_use_this_data = .TRUE.
    ELSE
      WRITE(wrf_err_message,*)'This input data is not V4: ' // TRIM(input_title_char)
      CALL wrf_debug( 0 , wrf_err_message )
      yes_use_this_data = .FALSE.
    END IF
    
  END SUBROUTINE is_this_data_ok_to_use

  FUNCTION check_which_switch (switch) RESULT (aux_thing)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: switch
    CHARACTER(10) :: aux_thing
  
    CHARACTER(LEN=80) :: joe_message
    INTEGER :: switch_loop, start, end, count
  
    aux_thing = 'auxinputxx'
  
    start =   MAX_HISTORY + 1
    end   = 2*MAX_HISTORY
    count = 0
    DO switch_loop = start, end
      IF ( switch .EQ. switch_loop ) THEN
        IF ( count .EQ. 0 ) THEN
          WRITE(aux_thing,FMT='(a      )') 'wrfinput '
        ELSE IF ( count .LT. 10 ) THEN
          WRITE(aux_thing,FMT='(a,i1   )') 'auxinput',count
        ELSE 
          WRITE(aux_thing,FMT='(a,i2.2 )') 'auxinput',count
        END IF
      END IF
      count = count +1
    END DO
  END FUNCTION check_which_switch

#endif
