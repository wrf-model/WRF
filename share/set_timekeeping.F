SUBROUTINE Setup_Timekeeping ( grid )
   USE module_domain
   USE module_configure
   USE module_utility
   IMPLICIT NONE
   TYPE(domain), POINTER :: grid
! Local
   TYPE(WRFU_TimeInterval) :: begin_time, end_time, zero_time, one_minute, one_hour, forever, padding_interval
   TYPE(WRFU_TimeInterval) :: interval, run_length, dfl_length
   TYPE(WRFU_Time) :: startTime, stopTime, initialTime
   TYPE(WRFU_TimeInterval) :: stepTime
   TYPE(WRFU_TimeInterval) :: tmp_step
   INTEGER :: start_year,start_month,start_day,start_hour,start_minute,start_second
   INTEGER :: end_year,end_month,end_day,end_hour,end_minute,end_second
   INTEGER :: vortex_interval
#if ( HWRF == 1 )
!zhang's doing
   real (kind=8) :: day_in_sec
   REAL :: tstart
!end of zhang's doing
#endif

! #if (EM_CORE == 1)
   INTEGER :: dfi_fwdstop_year,dfi_fwdstop_month,dfi_fwdstop_day,dfi_fwdstop_hour,dfi_fwdstop_minute,dfi_fwdstop_second
   INTEGER :: dfi_bckstop_year,dfi_bckstop_month,dfi_bckstop_day,dfi_bckstop_hour,dfi_bckstop_minute,dfi_bckstop_second
! #endif

   INTEGER :: restart_interval_d
   INTEGER :: inputout_interval_d
   INTEGER :: inputout_begin_y
   INTEGER :: inputout_end_y
   INTEGER :: inputout_begin_m
   INTEGER :: inputout_begin_s
   INTEGER :: inputout_begin_d
   INTEGER :: inputout_begin_h
   INTEGER :: inputout_end_m
   INTEGER :: inputout_end_s
   INTEGER :: inputout_end_d
   INTEGER :: inputout_end_h
   INTEGER :: restart_interval_m
   INTEGER :: restart_interval_s
   INTEGER :: restart_interval
   INTEGER :: restart_interval_h
   INTEGER :: inputout_interval_m
   INTEGER :: inputout_interval_s
   INTEGER :: inputout_interval
   INTEGER :: inputout_interval_h

#  include "set_timekeeping_defs.inc"

   INTEGER :: grid_fdda, grid_sfdda

   INTEGER :: run_days, run_hours, run_minutes, run_seconds
   INTEGER :: time_step, time_step_fract_num, time_step_fract_den
   INTEGER :: rc
   REAL    :: dt

   CALL WRFU_TimeIntervalSet ( zero_time, rc=rc )
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                         'WRFU_TimeIntervalSet(zero_time) FAILED', &
                         __FILE__ , &
                         __LINE__  )
   CALL WRFU_TimeIntervalSet ( one_minute, M=1, rc=rc )
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                         'WRFU_TimeIntervalSet(one_minute) FAILED', &
                         __FILE__ , &
                         __LINE__  )
   CALL WRFU_TimeIntervalSet ( one_hour, H=1, rc=rc )
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                         'WRFU_TimeIntervalSet(one_hour) FAILED', &
                         __FILE__ , &
                         __LINE__  )
   CALL WRFU_TimeIntervalSet ( forever, S=1700000000, rc=rc )  ! magic number; indicats an interval that is forever
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                         'WRFU_TimeIntervalSet(forever) FAILED', &
                         __FILE__ , &
                         __LINE__  )

! #if (EM_CORE == 1)
   IF ( (grid%dfi_opt .EQ. DFI_NODFI) .OR. (grid%dfi_stage .EQ. DFI_SETUP) ) THEN
! #endif
      CALL nl_get_start_year(grid%id,start_year)
      CALL nl_get_start_month(grid%id,start_month)
      CALL nl_get_start_day(grid%id,start_day)
      CALL nl_get_start_hour(grid%id,start_hour)
      CALL nl_get_start_minute(grid%id,start_minute)
      CALL nl_get_start_second(grid%id,start_second)
#if ( HWRF == 1 )
!zhang's doing - check with zhan before adding this bit
!   CALL nl_get_tstart ( grid%id , tstart )
!   CALL jdn_sec(day_in_sec,start_year,start_month,start_day,start_hour,start_minute,start_second)
!   day_in_sec = day_in_sec + tstart*3600.
!   CALL jdn_ymd_hms(day_in_sec,start_year,start_month,start_day,start_hour,start_minute,start_second)
!end of zhang's doing
#endif
      CALL WRFU_TimeSet(startTime, YY=start_year, MM=start_month, DD=start_day, &
                                   H=start_hour, M=start_minute, S=start_second,&
                                   rc=rc)
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeSet(startTime) FAILED', &
                            __FILE__ , &
                            __LINE__  )
! #if (EM_CORE == 1)
   ELSE
      IF ( grid%dfi_opt .EQ. DFI_DFL ) THEN
         IF ( grid%dfi_stage .EQ. DFI_FWD ) THEN
            CALL nl_get_start_year(grid%id,start_year)
            CALL nl_get_start_month(grid%id,start_month)
            CALL nl_get_start_day(grid%id,start_day)
            CALL nl_get_start_hour(grid%id,start_hour)
            CALL nl_get_start_minute(grid%id,start_minute)
            CALL nl_get_start_second(grid%id,start_second)
         ELSE IF ( grid%dfi_stage .EQ. DFI_FST ) THEN
            CALL nl_get_start_year(grid%id,start_year)
            CALL nl_get_start_month(grid%id,start_month)
            CALL nl_get_start_day(grid%id,start_day)
            CALL nl_get_start_hour(grid%id,start_hour)
            CALL nl_get_start_minute(grid%id,start_minute)
            CALL nl_get_start_second(grid%id,start_second)

            run_length = grid%stop_subtime - grid%start_subtime
            CALL WRFU_TimeIntervalGet( run_length, S=run_seconds, rc=rc )
! What about fractional seconds?
            run_seconds = run_seconds / 2
            CALL WRFU_TimeIntervalSet ( run_length, S=run_seconds, rc=rc )
            CALL WRFU_TimeSet(startTime, YY=start_year, MM=start_month, DD=start_day, &
                                         H=start_hour, M=start_minute, S=start_second,&
                                         rc=rc)
            startTime = startTime + run_length
            CALL WRFU_TimeGet(startTime, YY=start_year, MM=start_month, DD=start_day, &
                                         H=start_hour, M=start_minute, S=start_second,&
                                         rc=rc)
         END IF

      ELSE IF ( grid%dfi_opt .EQ. DFI_DDFI ) THEN
         IF ( grid%dfi_stage .EQ. DFI_FWD ) THEN
            CALL nl_get_dfi_bckstop_year(grid%id,start_year)
            CALL nl_get_dfi_bckstop_month(grid%id,start_month)
            CALL nl_get_dfi_bckstop_day(grid%id,start_day)
            CALL nl_get_dfi_bckstop_hour(grid%id,start_hour)
            CALL nl_get_dfi_bckstop_minute(grid%id,start_minute)
            CALL nl_get_dfi_bckstop_second(grid%id,start_second)
         ELSE IF ( grid%dfi_stage .EQ. DFI_BCK ) THEN
            CALL nl_get_start_year(grid%id,start_year)
            CALL nl_get_start_month(grid%id,start_month)
            CALL nl_get_start_day(grid%id,start_day)
            CALL nl_get_start_hour(grid%id,start_hour)
            CALL nl_get_start_minute(grid%id,start_minute)
            CALL nl_get_start_second(grid%id,start_second)
         ELSE IF ( grid%dfi_stage .EQ. DFI_FST ) THEN
            CALL nl_get_start_year(grid%id,start_year)
            CALL nl_get_start_month(grid%id,start_month)
            CALL nl_get_start_day(grid%id,start_day)
            CALL nl_get_start_hour(grid%id,start_hour)
            CALL nl_get_start_minute(grid%id,start_minute)
            CALL nl_get_start_second(grid%id,start_second)
         END IF

      ELSE IF ( grid%dfi_opt .EQ. DFI_TDFI ) THEN
         IF ( grid%dfi_stage .EQ. DFI_FWD ) THEN
            CALL nl_get_dfi_bckstop_year(grid%id,start_year)
            CALL nl_get_dfi_bckstop_month(grid%id,start_month)
            CALL nl_get_dfi_bckstop_day(grid%id,start_day)
            CALL nl_get_dfi_bckstop_hour(grid%id,start_hour)
            CALL nl_get_dfi_bckstop_minute(grid%id,start_minute)
            CALL nl_get_dfi_bckstop_second(grid%id,start_second)

            ! Here, we look at head_grid to determine run_length.
            !   Since start_subtime and stop_subtime were
            !   updated for nesting, they no longer bound the dfi
            !   time window, so, start_subtime and stop_subtime from
            !   from the grid structure won't work.  However, we can use
            !   head_grid since the dfi time window is the same for all 
            !   domains.

            run_length = head_grid%start_subtime - head_grid%stop_subtime
            CALL WRFU_TimeIntervalGet( run_length, S=run_seconds, rc=rc )
! What about fractional seconds?
            run_seconds = run_seconds / 2
            CALL WRFU_TimeIntervalSet ( run_length, S=run_seconds, rc=rc )
            CALL WRFU_TimeSet(startTime, YY=start_year, MM=start_month, DD=start_day, &
                                         H=start_hour, M=start_minute, S=start_second,&
                                         rc=rc)
            startTime = startTime + run_length
            CALL WRFU_TimeGet(startTime, YY=start_year, MM=start_month, DD=start_day, &
                                         H=start_hour, M=start_minute, S=start_second,&
                                         rc=rc)
         ELSE IF ( grid%dfi_stage .EQ. DFI_BCK ) THEN
            CALL nl_get_start_year(grid%id,start_year)
            CALL nl_get_start_month(grid%id,start_month)
            CALL nl_get_start_day(grid%id,start_day)
            CALL nl_get_start_hour(grid%id,start_hour)
            CALL nl_get_start_minute(grid%id,start_minute)
            CALL nl_get_start_second(grid%id,start_second)
         ELSE IF ( grid%dfi_stage .EQ. DFI_FST ) THEN
            CALL nl_get_start_year(grid%id,start_year)
            CALL nl_get_start_month(grid%id,start_month)
            CALL nl_get_start_day(grid%id,start_day)
            CALL nl_get_start_hour(grid%id,start_hour)
            CALL nl_get_start_minute(grid%id,start_minute)
            CALL nl_get_start_second(grid%id,start_second)
         ELSE IF ( grid%dfi_stage .EQ. DFI_STARTFWD ) THEN
            CALL nl_get_start_year(grid%id,start_year)
            CALL nl_get_start_month(grid%id,start_month)
            CALL nl_get_start_day(grid%id,start_day)
            CALL nl_get_start_hour(grid%id,start_hour)
            CALL nl_get_start_minute(grid%id,start_minute)
            CALL nl_get_start_second(grid%id,start_second)
         END IF
      END IF

      IF ( grid%dfi_stage .EQ. DFI_STARTBCK ) THEN
         CALL WRFU_ClockGet( grid%domain_clock, CurrTime=startTime, rc=rc)
      ELSE
         CALL WRFU_TimeSet(startTime, YY=start_year, MM=start_month, DD=start_day, &
              H=start_hour, M=start_minute, S=start_second,&
              rc=rc)
      ENDIF
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeSet(startTime) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   END IF
! #endif

   CALL nl_get_run_days(1,run_days)
   CALL nl_get_run_hours(1,run_hours)
   CALL nl_get_run_minutes(1,run_minutes)
   CALL nl_get_run_seconds(1,run_seconds)

! #if (EM_CORE == 1)
   IF ( (grid%dfi_opt .EQ. DFI_NODFI) .OR. (grid%dfi_stage .EQ. DFI_SETUP) .OR. (grid%dfi_stage .EQ. DFI_FST)) THEN
! #endif

      IF ( grid%id .EQ. head_grid%id .AND. &
           ( run_days .gt. 0 .or. run_hours .gt. 0 .or. run_minutes .gt. 0 .or. run_seconds .gt. 0 )) THEN
        CALL WRFU_TimeIntervalSet ( run_length , D=run_days, H=run_hours, M=run_minutes, S=run_seconds, rc=rc )
! #if (EM_CORE == 1)
        IF ( grid%dfi_stage .EQ. DFI_FST .AND. grid%dfi_opt .EQ. DFI_DFL ) THEN
           CALL nl_get_start_year(grid%id,start_year)
           CALL nl_get_start_month(grid%id,start_month)
           CALL nl_get_start_day(grid%id,start_day)
           CALL nl_get_start_hour(grid%id,start_hour)
           CALL nl_get_start_minute(grid%id,start_minute)
           CALL nl_get_start_second(grid%id,start_second)
           CALL WRFU_TimeSet(initialTime, YY=start_year, MM=start_month, DD=start_day, &
                                        H=start_hour, M=start_minute, S=start_second,&
                                        rc=rc)
           dfl_length = startTime - initialTime
           run_length = run_length - dfl_length
        END IF
! #endif
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(run_length) FAILED', &
                           __FILE__ , &
                           __LINE__  )
        stopTime = startTime + run_length
      ELSE
        CALL nl_get_end_year(grid%id,end_year)
        CALL nl_get_end_month(grid%id,end_month)
        CALL nl_get_end_day(grid%id,end_day)
        CALL nl_get_end_hour(grid%id,end_hour)
        CALL nl_get_end_minute(grid%id,end_minute)
        CALL nl_get_end_second(grid%id,end_second)
        CALL WRFU_TimeSet(stopTime, YY=end_year, MM=end_month, DD=end_day, &
                                 H=end_hour, M=end_minute, S=end_second,&
                                 rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeSet(stopTime) FAILED', &
                           __FILE__ , &
                           __LINE__  )
        run_length = stopTime - startTime
      ENDIF

! #if (EM_CORE == 1)
   ELSE IF ( grid%dfi_stage .EQ. DFI_STARTFWD ) THEN
      CALL nl_get_time_step ( 1, time_step )
      CALL nl_get_time_step_fract_num( 1, time_step_fract_num )
      CALL nl_get_time_step_fract_den( 1, time_step_fract_den )
      CALL WRFU_TimeIntervalSet( run_length, S=time_step, Sn=time_step_fract_num, Sd=time_step_fract_den, rc=rc)
      stopTime = startTime + run_length
   ELSE IF ( grid%dfi_stage .EQ. DFI_STARTBCK ) THEN
      CALL nl_get_time_step ( 1, time_step )
      CALL nl_get_time_step_fract_num( 1, time_step_fract_num )
      CALL nl_get_time_step_fract_den( 1, time_step_fract_den )
      CALL WRFU_TimeIntervalSet( run_length, S=time_step, Sn=time_step_fract_num, Sd=time_step_fract_den, rc=rc)
      stopTime = startTime + run_length
   ELSE
      IF ( grid%dfi_opt .EQ. DFI_DFL ) THEN 
         IF ( grid%dfi_stage .EQ. DFI_FWD ) THEN
            CALL nl_get_dfi_fwdstop_year(grid%id,end_year)
            CALL nl_get_dfi_fwdstop_month(grid%id,end_month)
            CALL nl_get_dfi_fwdstop_day(grid%id,end_day)
            CALL nl_get_dfi_fwdstop_hour(grid%id,end_hour)
            CALL nl_get_dfi_fwdstop_minute(grid%id,end_minute)
            CALL nl_get_dfi_fwdstop_second(grid%id,end_second)
         END IF

      ELSE IF ( grid%dfi_opt .EQ. DFI_DDFI ) THEN 
         IF ( grid%dfi_stage .EQ. DFI_FWD ) THEN
            CALL nl_get_dfi_fwdstop_year(grid%id,end_year)
            CALL nl_get_dfi_fwdstop_month(grid%id,end_month)
            CALL nl_get_dfi_fwdstop_day(grid%id,end_day)
            CALL nl_get_dfi_fwdstop_hour(grid%id,end_hour)
            CALL nl_get_dfi_fwdstop_minute(grid%id,end_minute)
            CALL nl_get_dfi_fwdstop_second(grid%id,end_second)
         ELSE IF ( grid%dfi_stage .EQ. DFI_BCK ) THEN
            CALL nl_get_dfi_bckstop_year(grid%id,end_year)
            CALL nl_get_dfi_bckstop_month(grid%id,end_month)
            CALL nl_get_dfi_bckstop_day(grid%id,end_day)
            CALL nl_get_dfi_bckstop_hour(grid%id,end_hour)
            CALL nl_get_dfi_bckstop_minute(grid%id,end_minute)
            CALL nl_get_dfi_bckstop_second(grid%id,end_second)
         END IF

      ELSE IF ( grid%dfi_opt .EQ. DFI_TDFI ) THEN 
         IF ( grid%dfi_stage .EQ. DFI_FWD ) THEN
            CALL nl_get_dfi_fwdstop_year(grid%id,end_year)
            CALL nl_get_dfi_fwdstop_month(grid%id,end_month)
            CALL nl_get_dfi_fwdstop_day(grid%id,end_day)
            CALL nl_get_dfi_fwdstop_hour(grid%id,end_hour)
            CALL nl_get_dfi_fwdstop_minute(grid%id,end_minute)
            CALL nl_get_dfi_fwdstop_second(grid%id,end_second)
         ELSE IF ( grid%dfi_stage .EQ. DFI_BCK ) THEN
            CALL nl_get_dfi_bckstop_year(grid%id,end_year)
            CALL nl_get_dfi_bckstop_month(grid%id,end_month)
            CALL nl_get_dfi_bckstop_day(grid%id,end_day)
            CALL nl_get_dfi_bckstop_hour(grid%id,end_hour)
            CALL nl_get_dfi_bckstop_minute(grid%id,end_minute)
            CALL nl_get_dfi_bckstop_second(grid%id,end_second)
         END IF
      END IF
      CALL WRFU_TimeSet(stopTime, YY=end_year, MM=end_month, DD=end_day, &
                         H=end_hour, M=end_minute, S=end_second,&
                                rc=rc)

      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                   'WRFU_TimeSet(dfistopfwdTime) FAILED', &
                   __FILE__ , &
                   __LINE__  )

      run_length = stopTime - startTime

   END IF
! #endif

   IF ( run_length .GT. zero_time ) THEN
     padding_interval = forever
   ELSE
     padding_interval = zero_time - forever
   ENDIF

   IF ( grid%id .EQ. head_grid%id ) THEN
      CALL nl_get_time_step ( 1, time_step )
      CALL nl_get_time_step_fract_num( 1, time_step_fract_num )
      CALL nl_get_time_step_fract_den( 1, time_step_fract_den )
      dt = real(time_step) + real(time_step_fract_num) / real(time_step_fract_den)
#ifdef PLANET
      ! 2004-12-08 ADT notes:
      ! We have gotten the timestep from integers in the namelist, and they have just
      ! been converted to the timestep, "dt", used by the physics code just above.
      ! After this point, the integers are only used to update the clock used for,
      ! and we want to leave that on a "24-hour" type schedule, so we don't need to
      ! modify those integers.  Theoretically they refer to a portion of the planet's
      ! solar day.  The only thing we have to do is convert the *real* timestep, dt,
      ! to useful SI units.  This is easily accomplished by multiplying it by the
      ! variable P2SI, which was designed for just this purpose.  After multiplication,
      ! make sure every subsequent part of the model knows what the value is.
      dt = dt * P2SI
#endif
      CALL nl_set_dt( grid%id, dt )
      grid%dt = dt
      CALL WRFU_TimeIntervalSet(stepTime, S=time_step, Sn=time_step_fract_num, Sd=time_step_fract_den, rc=rc)
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(stepTime) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      tmp_step = domain_get_time_step( grid%parents(1)%ptr )
      stepTime = domain_get_time_step( grid%parents(1)%ptr ) / &
           grid%parent_time_step_ratio
      grid%dt = grid%parents(1)%ptr%dt / grid%parent_time_step_ratio
      CALL nl_set_dt( grid%id, grid%dt )
   ENDIF

   ! create grid%domain_clock and associated state
   CALL domain_clock_create( grid, TimeStep= stepTime,  &
                                   StartTime=startTime, &
                                   StopTime= stopTime )
   CALL domain_clockprint ( 150, grid, &
          'DEBUG setup_timekeeping():  clock after creation,' )

   ! Set default value for SIMULATION_START_DATE.  
   ! This is overwritten later in input_wrf(), if needed.  
   IF ( grid%id .EQ. head_grid%id ) THEN
      CALL nl_set_simulation_start_year   ( 1 , start_year   )
      CALL nl_set_simulation_start_month  ( 1 , start_month  )
      CALL nl_set_simulation_start_day    ( 1 , start_day    )
      CALL nl_set_simulation_start_hour   ( 1 , start_hour   )
      CALL nl_set_simulation_start_minute ( 1 , start_minute )
      CALL nl_set_simulation_start_second ( 1 , start_second )
   ENDIF

#include "set_timekeeping_alarms.inc"

! RESTART INTERVAL
! restart_interval is left there (and means minutes) for consistency, but
! restart_interval_m will take precedence if specified
   CALL nl_get_restart_interval( 1, restart_interval )   ! same as minutes
   CALL nl_get_restart_interval_d( 1, restart_interval_d )
   CALL nl_get_restart_interval_h( 1, restart_interval_h )
   CALL nl_get_restart_interval_m( 1, restart_interval_m )
   CALL nl_get_restart_interval_s( 1, restart_interval_s )
   IF ( restart_interval_m .EQ. 0 ) restart_interval_m = restart_interval
   IF ( MAX( restart_interval_d,   &
             restart_interval_h, restart_interval_m , restart_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=restart_interval_d, &
                                        H=restart_interval_h, M=restart_interval_m, S=restart_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(restart_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL domain_alarm_create( grid, RESTART_ALARM, interval )

! INPUTOUT INTERVAL
   CALL nl_get_inputout_interval( grid%id, inputout_interval )   ! same as minutes
   CALL nl_get_inputout_interval_d( grid%id, inputout_interval_d )
   CALL nl_get_inputout_interval_h( grid%id, inputout_interval_h )
   CALL nl_get_inputout_interval_m( grid%id, inputout_interval_m )
   CALL nl_get_inputout_interval_s( grid%id, inputout_interval_s )
   IF ( inputout_interval_m .EQ. 0 ) inputout_interval_m = inputout_interval

   IF ( MAX( inputout_interval_d,   &
             inputout_interval_h, inputout_interval_m , inputout_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=inputout_interval_d, &
                                        H=inputout_interval_h, M=inputout_interval_m, S=inputout_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(inputout_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF

   CALL nl_get_inputout_begin_y( grid%id, inputout_begin_y )
   CALL nl_get_inputout_begin_d( grid%id, inputout_begin_d )
   CALL nl_get_inputout_begin_h( grid%id, inputout_begin_h )
   CALL nl_get_inputout_begin_m( grid%id, inputout_begin_m )
   CALL nl_get_inputout_begin_s( grid%id, inputout_begin_s )
   IF ( MAX( inputout_begin_y, inputout_begin_d,   &
             inputout_begin_h, inputout_begin_m , inputout_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=inputout_begin_d, &
                                      H=inputout_begin_h, M=inputout_begin_m, S=inputout_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(inputout_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_inputout_end_y( grid%id, inputout_end_y )
   CALL nl_get_inputout_end_d( grid%id, inputout_end_d )
   CALL nl_get_inputout_end_h( grid%id, inputout_end_h )
   CALL nl_get_inputout_end_m( grid%id, inputout_end_m )
   CALL nl_get_inputout_end_s( grid%id, inputout_end_s )
   IF ( MAX( inputout_end_y, inputout_end_d,   &
             inputout_end_h, inputout_end_m , inputout_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=inputout_end_d, &
                                     H=inputout_end_h, M=inputout_end_m, S=inputout_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(inputout_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time =  padding_interval
   ENDIF

   CALL domain_alarm_create( grid, INPUTOUT_ALARM, interval, begin_time, end_time )

#if ( WRF_CHEM == 1 )
! AUXINPUT5_ INTERVAL
! auxinput5_interval is left there (and means minutes) for consistency, but
! auxinput5_interval_m will take precedence if specified
   CALL nl_get_auxinput5_interval( grid%id, auxinput5_interval )   ! same as minutes
   CALL nl_get_auxinput5_interval_d( grid%id, auxinput5_interval_d )
   CALL nl_get_auxinput5_interval_h( grid%id, auxinput5_interval_h )
   CALL nl_get_auxinput5_interval_m( grid%id, auxinput5_interval_m )
   CALL nl_get_auxinput5_interval_s( grid%id, auxinput5_interval_s )
   IF ( auxinput5_interval_m .EQ. 0 ) auxinput5_interval_m = auxinput5_interval

   IF ( MAX( auxinput5_interval_d,   &
             auxinput5_interval_h, auxinput5_interval_m , auxinput5_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput5_interval_d, &
                                        H=auxinput5_interval_h, M=auxinput5_interval_m, S=auxinput5_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput5_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF

   CALL nl_get_auxinput5_begin_y( grid%id, auxinput5_begin_y )
   CALL nl_get_auxinput5_begin_d( grid%id, auxinput5_begin_d )
   CALL nl_get_auxinput5_begin_h( grid%id, auxinput5_begin_h )
   CALL nl_get_auxinput5_begin_m( grid%id, auxinput5_begin_m )
   CALL nl_get_auxinput5_begin_s( grid%id, auxinput5_begin_s )
   IF ( MAX( auxinput5_begin_y, auxinput5_begin_d,   &
             auxinput5_begin_h, auxinput5_begin_m , auxinput5_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput5_begin_d, &
                                      H=auxinput5_begin_h, M=auxinput5_begin_m, S=auxinput5_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput5_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput5_end_y( grid%id, auxinput5_end_y )
   CALL nl_get_auxinput5_end_d( grid%id, auxinput5_end_d )
   CALL nl_get_auxinput5_end_h( grid%id, auxinput5_end_h )
   CALL nl_get_auxinput5_end_m( grid%id, auxinput5_end_m )
   CALL nl_get_auxinput5_end_s( grid%id, auxinput5_end_s )
   IF ( MAX( auxinput5_end_y, auxinput5_end_d,   &
             auxinput5_end_h, auxinput5_end_m , auxinput5_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput5_end_d, &
                                     H=auxinput5_end_h, M=auxinput5_end_m, S=auxinput5_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput5_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time =  padding_interval
   ENDIF
   CALL domain_alarm_create( grid, AUXINPUT5_ALARM, interval, begin_time, end_time )
!TBH:  Should be OK to remove the "#else" section and the code it contains 
!TBH:  because later code overwrites grid%alarms( AUXINPUT5_ALARM )...  
!TBH:  In fact, by setting namelist values for auxinput5 correctly, it ought 
!TBH:  to be possible to get rid of all "#if ( WRF_CHEM == 1 )" bits in this file...  
   CALL WRFU_AlarmEnable( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
   CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
! TBH:  NOTE:  Proper setting of namelist variables for auxinput5 ought to 
! TBH:         make this hard-coded bit unnecessary.  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! add for wrf_chem emiss input
   CALL WRFU_AlarmEnable( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
   CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
! end for wrf chem emiss input
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#endif

! without this test, it's possible for the value of the WRF_ALARM_SECS_TIL_NEXT_RING
! that is written as metadata to a restart file to be garbage for BOUNDARY_ALARM for 
! the nests.  Parallel NetCDF does a header check on all the metadata being written
! from multiple processors and if it differs, it throws up an error. This avoids that.
   IF ( grid%id .EQ. 1 ) THEN   ! only moad can have specified boundaries
     CALL domain_alarm_create( grid, BOUNDARY_ALARM, interval )
     CALL WRFU_AlarmEnable( grid%alarms( BOUNDARY_ALARM ), rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmEnable(BOUNDARY_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
     CALL WRFU_AlarmRingerOn( grid%alarms( BOUNDARY_ALARM ), rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(BOUNDARY_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF

! This is the interval at which the code in time_for_move in share/mediation_integrate.F
! will recompute the center of the Vortex.  Other times, it will use the last position.
!
   vortex_interval = 0
#ifdef MOVE_NESTS
   CALL nl_get_vortex_interval ( grid%id , vortex_interval ) 
#endif
   CALL WRFU_TimeIntervalSet( interval, M=vortex_interval, rc=rc )
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(interval) for computing vortex center FAILED', &
                           __FILE__ , &
                           __LINE__  )
   CALL domain_alarm_create( grid,  COMPUTE_VORTEX_CENTER_ALARM, interval  )
#ifdef MOVE_NESTS
   CALL WRFU_AlarmEnable( grid%alarms( COMPUTE_VORTEX_CENTER_ALARM ), rc=rc )
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                         'WRFU_AlarmEnable(COMPUTE_VORTEX_CENTER_ALARM) FAILED', &
                         __FILE__ , &
                         __LINE__  )
   CALL WRFU_AlarmRingerOn( grid%alarms( COMPUTE_VORTEX_CENTER_ALARM ), rc=rc )
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                         'WRFU_AlarmRingerOn(COMPUTE_VORTEX_CENTER_ALARM) FAILED', &
                         __FILE__ , &
                         __LINE__  )
#else
! Go ahead and let the alarm be defined, but disable it, since we are not using moving nests here.
   CALL WRFU_AlarmDisable( grid%alarms( COMPUTE_VORTEX_CENTER_ALARM ), rc=rc )
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                         'WRFU_AlarmDisable(COMPUTE_VORTEX_CENTER_ALARM) FAILED', &
                         __FILE__ , &
                         __LINE__  )
#endif

   grid%time_set = .TRUE.

   ! Initialize derived time quantities in grid state.  
   ! These are updated in domain_clockadvance().  
   CALL domain_clock_get( grid, minutesSinceSimulationStart=grid%xtime )
   CALL domain_clock_get( grid, currentDayOfYearReal=grid%julian )
   WRITE(wrf_err_message,*) 'setup_timekeeping:  set xtime to ',grid%xtime
   CALL wrf_debug ( 100, TRIM(wrf_err_message) )
   WRITE(wrf_err_message,*) 'setup_timekeeping:  set julian to ',grid%julian
   CALL wrf_debug ( 100, TRIM(wrf_err_message) )

   CALL wrf_debug ( 100 , 'setup_timekeeping:  returning...' )

END SUBROUTINE Setup_Timekeeping


