SUBROUTINE Setup_Timekeeping ( grid )
   USE module_domain
   USE module_configure
   USE module_utility
   IMPLICIT NONE
   TYPE(domain), POINTER :: grid
! Local
   TYPE(WRFU_TimeInterval) :: begin_time, end_time, zero_time, one_minute, one_hour, padding_interval
   TYPE(WRFU_TimeInterval) :: interval, run_length
   TYPE(WRFU_Time) :: startTime, stopTime
   TYPE(WRFU_TimeInterval) :: stepTime
   INTEGER :: start_year,start_month,start_day,start_hour,start_minute,start_second
   INTEGER :: end_year,end_month,end_day,end_hour,end_minute,end_second
#ifdef MOVE_NESTS
   INTEGER :: vortex_interval
#endif

   INTEGER :: history_interval  , restart_interval  ,  &
              history_interval_mo, restart_interval_mo,  &
              history_interval_d, restart_interval_d,  &
              history_interval_h, restart_interval_h,  &
              history_interval_m, restart_interval_m,  &
              history_interval_s, restart_interval_s

   INTEGER :: auxhist1_interval  , auxhist2_interval  , auxhist3_interval  , &
              auxhist1_interval_mo, auxhist2_interval_mo, auxhist3_interval_mo, &
              auxhist1_interval_d, auxhist2_interval_d, auxhist3_interval_d, &
              auxhist1_interval_h, auxhist2_interval_h, auxhist3_interval_h, &
              auxhist1_interval_m, auxhist2_interval_m, auxhist3_interval_m, &
              auxhist1_interval_s, auxhist2_interval_s, auxhist3_interval_s

   INTEGER :: auxhist4_interval  , auxhist5_interval,   &
              auxhist4_interval_mo, auxhist5_interval_mo, &
              auxhist4_interval_d, auxhist5_interval_d, &
              auxhist4_interval_h, auxhist5_interval_h, &
              auxhist4_interval_m, auxhist5_interval_m, &
              auxhist4_interval_s, auxhist5_interval_s

   INTEGER :: auxhist6_interval  , auxhist7_interval  , auxhist8_interval  , &
              auxhist6_interval_mo, auxhist7_interval_mo, auxhist8_interval_mo, &
              auxhist6_interval_d, auxhist7_interval_d, auxhist8_interval_d, &
              auxhist6_interval_h, auxhist7_interval_h, auxhist8_interval_h, &
              auxhist6_interval_m, auxhist7_interval_m, auxhist8_interval_m, &
              auxhist6_interval_s, auxhist7_interval_s, auxhist8_interval_s

   INTEGER :: auxhist9_interval  , auxhist10_interval  , auxhist11_interval  , &
              auxhist9_interval_mo, auxhist10_interval_mo, auxhist11_interval_mo, &
              auxhist9_interval_d, auxhist10_interval_d, auxhist11_interval_d, &
              auxhist9_interval_h, auxhist10_interval_h, auxhist11_interval_h, &
              auxhist9_interval_m, auxhist10_interval_m, auxhist11_interval_m, &
              auxhist9_interval_s, auxhist10_interval_s, auxhist11_interval_s

   INTEGER :: auxinput1_interval  , auxinput2_interval  , auxinput3_interval  , &
              auxinput1_interval_mo, auxinput2_interval_mo, auxinput3_interval_mo, &
              auxinput1_interval_d, auxinput2_interval_d, auxinput3_interval_d, &
              auxinput1_interval_h, auxinput2_interval_h, auxinput3_interval_h, &
              auxinput1_interval_m, auxinput2_interval_m, auxinput3_interval_m, &
              auxinput1_interval_s, auxinput2_interval_s, auxinput3_interval_s

   INTEGER :: auxinput4_interval  , auxinput5_interval  , &
              auxinput4_interval_mo, auxinput5_interval_mo, &
              auxinput4_interval_d, auxinput5_interval_d, &
              auxinput4_interval_h, auxinput5_interval_h, &
              auxinput4_interval_m, auxinput5_interval_m, &
              auxinput4_interval_s, auxinput5_interval_s

   INTEGER :: auxinput6_interval  , auxinput7_interval  , auxinput8_interval  , &
              auxinput6_interval_mo, auxinput7_interval_mo, auxinput8_interval_mo, &
              auxinput6_interval_d, auxinput7_interval_d, auxinput8_interval_d, &
              auxinput6_interval_h, auxinput7_interval_h, auxinput8_interval_h, &
              auxinput6_interval_m, auxinput7_interval_m, auxinput8_interval_m, &
              auxinput6_interval_s, auxinput7_interval_s, auxinput8_interval_s

   INTEGER :: auxinput9_interval  , gfdda_interval  , auxinput11_interval  , &
              auxinput9_interval_mo, gfdda_interval_mo, auxinput11_interval_mo, &
              auxinput9_interval_d, gfdda_interval_d, auxinput11_interval_d, &
              auxinput9_interval_h, gfdda_interval_h, auxinput11_interval_h, &
              auxinput9_interval_m, gfdda_interval_m, auxinput11_interval_m, &
              auxinput9_interval_s, gfdda_interval_s, auxinput11_interval_s

   INTEGER :: history_begin  , restart_begin  ,  &
              history_begin_y, restart_begin_y,  &
              history_begin_mo, restart_begin_mo,  &
              history_begin_d, restart_begin_d,  &
              history_begin_h, restart_begin_h,  &
              history_begin_m, restart_begin_m,  &
              history_begin_s, restart_begin_s

   INTEGER :: auxhist1_begin  , auxhist2_begin  , auxhist3_begin  , &
              auxhist1_begin_y, auxhist2_begin_y, auxhist3_begin_y, &
              auxhist1_begin_mo, auxhist2_begin_mo, auxhist3_begin_mo, &
              auxhist1_begin_d, auxhist2_begin_d, auxhist3_begin_d, &
              auxhist1_begin_h, auxhist2_begin_h, auxhist3_begin_h, &
              auxhist1_begin_m, auxhist2_begin_m, auxhist3_begin_m, &
              auxhist1_begin_s, auxhist2_begin_s, auxhist3_begin_s

   INTEGER :: auxhist4_begin  , auxhist5_begin,   &
              auxhist4_begin_y, auxhist5_begin_y, &
              auxhist4_begin_mo, auxhist5_begin_mo, &
              auxhist4_begin_d, auxhist5_begin_d, &
              auxhist4_begin_h, auxhist5_begin_h, &
              auxhist4_begin_m, auxhist5_begin_m, &
              auxhist4_begin_s, auxhist5_begin_s

   INTEGER :: auxhist6_begin  , auxhist7_begin  , auxhist8_begin  , &
              auxhist6_begin_y, auxhist7_begin_y, auxhist8_begin_y, &
              auxhist6_begin_mo, auxhist7_begin_mo, auxhist8_begin_mo, &
              auxhist6_begin_d, auxhist7_begin_d, auxhist8_begin_d, &
              auxhist6_begin_h, auxhist7_begin_h, auxhist8_begin_h, &
              auxhist6_begin_m, auxhist7_begin_m, auxhist8_begin_m, &
              auxhist6_begin_s, auxhist7_begin_s, auxhist8_begin_s

   INTEGER :: auxhist9_begin  , auxhist10_begin  , auxhist11_begin  , &
              auxhist9_begin_y, auxhist10_begin_y, auxhist11_begin_y, &
              auxhist9_begin_mo, auxhist10_begin_mo, auxhist11_begin_mo, &
              auxhist9_begin_d, auxhist10_begin_d, auxhist11_begin_d, &
              auxhist9_begin_h, auxhist10_begin_h, auxhist11_begin_h, &
              auxhist9_begin_m, auxhist10_begin_m, auxhist11_begin_m, &
              auxhist9_begin_s, auxhist10_begin_s, auxhist11_begin_s

   INTEGER :: inputout_begin  ,  inputout_end,    inputout_interval ,    &
              inputout_begin_y,  inputout_end_y,  inputout_interval_y ,    &
              inputout_begin_mo, inputout_end_mo, inputout_interval_mo ,   &
              inputout_begin_d,  inputout_end_d,  inputout_interval_d ,    &
              inputout_begin_h,  inputout_end_h,  inputout_interval_h ,    &
              inputout_begin_m,  inputout_end_m,  inputout_interval_m ,    &
              inputout_begin_s,  inputout_end_s,  inputout_interval_s

   INTEGER :: auxinput1_begin  , auxinput2_begin  , auxinput3_begin  , &
              auxinput1_begin_y, auxinput2_begin_y, auxinput3_begin_y, &
              auxinput1_begin_mo, auxinput2_begin_mo, auxinput3_begin_mo, &
              auxinput1_begin_d, auxinput2_begin_d, auxinput3_begin_d, &
              auxinput1_begin_h, auxinput2_begin_h, auxinput3_begin_h, &
              auxinput1_begin_m, auxinput2_begin_m, auxinput3_begin_m, &
              auxinput1_begin_s, auxinput2_begin_s, auxinput3_begin_s

   INTEGER :: auxinput4_begin  , auxinput5_begin  , &
              auxinput4_begin_y, auxinput5_begin_y, &
              auxinput4_begin_mo, auxinput5_begin_mo, &
              auxinput4_begin_d, auxinput5_begin_d, &
              auxinput4_begin_h, auxinput5_begin_h, &
              auxinput4_begin_m, auxinput5_begin_m, &
              auxinput4_begin_s, auxinput5_begin_s

   INTEGER :: auxinput6_begin  , auxinput7_begin  , auxinput8_begin  , &
              auxinput6_begin_y, auxinput7_begin_y, auxinput8_begin_y, &
              auxinput6_begin_mo, auxinput7_begin_mo, auxinput8_begin_mo, &
              auxinput6_begin_d, auxinput7_begin_d, auxinput8_begin_d, &
              auxinput6_begin_h, auxinput7_begin_h, auxinput8_begin_h, &
              auxinput6_begin_m, auxinput7_begin_m, auxinput8_begin_m, &
              auxinput6_begin_s, auxinput7_begin_s, auxinput8_begin_s

   INTEGER :: auxinput9_begin  , gfdda_begin  , auxinput11_begin  , &
              auxinput9_begin_y, gfdda_begin_y, auxinput11_begin_y, &
              auxinput9_begin_mo, gfdda_begin_mo, auxinput11_begin_mo, &
              auxinput9_begin_d, gfdda_begin_d, auxinput11_begin_d, &
              auxinput9_begin_h, gfdda_begin_h, auxinput11_begin_h, &
              auxinput9_begin_m, gfdda_begin_m, auxinput11_begin_m, &
              auxinput9_begin_s, gfdda_begin_s, auxinput11_begin_s

   INTEGER :: history_end  , restart_end  ,  &
              history_end_y, restart_end_y,  &
              history_end_mo, restart_end_mo,  &
              history_end_d, restart_end_d,  &
              history_end_h, restart_end_h,  &
              history_end_m, restart_end_m,  &
              history_end_s, restart_end_s

   INTEGER :: auxhist1_end  , auxhist2_end  , auxhist3_end  , &
              auxhist1_end_y, auxhist2_end_y, auxhist3_end_y, &
              auxhist1_end_mo, auxhist2_end_mo, auxhist3_end_mo, &
              auxhist1_end_d, auxhist2_end_d, auxhist3_end_d, &
              auxhist1_end_h, auxhist2_end_h, auxhist3_end_h, &
              auxhist1_end_m, auxhist2_end_m, auxhist3_end_m, &
              auxhist1_end_s, auxhist2_end_s, auxhist3_end_s

   INTEGER :: auxhist4_end  , auxhist5_end,   &
              auxhist4_end_y, auxhist5_end_y, &
              auxhist4_end_mo, auxhist5_end_mo, &
              auxhist4_end_d, auxhist5_end_d, &
              auxhist4_end_h, auxhist5_end_h, &
              auxhist4_end_m, auxhist5_end_m, &
              auxhist4_end_s, auxhist5_end_s

   INTEGER :: auxhist6_end  , auxhist7_end  , auxhist8_end  , &
              auxhist6_end_y, auxhist7_end_y, auxhist8_end_y, &
              auxhist6_end_mo, auxhist7_end_mo, auxhist8_end_mo, &
              auxhist6_end_d, auxhist7_end_d, auxhist8_end_d, &
              auxhist6_end_h, auxhist7_end_h, auxhist8_end_h, &
              auxhist6_end_m, auxhist7_end_m, auxhist8_end_m, &
              auxhist6_end_s, auxhist7_end_s, auxhist8_end_s

   INTEGER :: auxhist9_end  , auxhist10_end  , auxhist11_end  , &
              auxhist9_end_y, auxhist10_end_y, auxhist11_end_y, &
              auxhist9_end_mo, auxhist10_end_mo, auxhist11_end_mo, &
              auxhist9_end_d, auxhist10_end_d, auxhist11_end_d, &
              auxhist9_end_h, auxhist10_end_h, auxhist11_end_h, &
              auxhist9_end_m, auxhist10_end_m, auxhist11_end_m, &
              auxhist9_end_s, auxhist10_end_s, auxhist11_end_s

   INTEGER :: auxinput1_end  , auxinput2_end  , auxinput3_end  , &
              auxinput1_end_y, auxinput2_end_y, auxinput3_end_y, &
              auxinput1_end_mo, auxinput2_end_mo, auxinput3_end_mo, &
              auxinput1_end_d, auxinput2_end_d, auxinput3_end_d, &
              auxinput1_end_h, auxinput2_end_h, auxinput3_end_h, &
              auxinput1_end_m, auxinput2_end_m, auxinput3_end_m, &
              auxinput1_end_s, auxinput2_end_s, auxinput3_end_s

   INTEGER :: auxinput4_end  , auxinput5_end  , &
              auxinput4_end_y, auxinput5_end_y, &
              auxinput4_end_mo, auxinput5_end_mo, &
              auxinput4_end_d, auxinput5_end_d, &
              auxinput4_end_h, auxinput5_end_h, &
              auxinput4_end_m, auxinput5_end_m, &
              auxinput4_end_s, auxinput5_end_s

   INTEGER :: auxinput6_end  , auxinput7_end  , auxinput8_end  , &
              auxinput6_end_y, auxinput7_end_y, auxinput8_end_y, &
              auxinput6_end_mo, auxinput7_end_mo, auxinput8_end_mo, &
              auxinput6_end_d, auxinput7_end_d, auxinput8_end_d, &
              auxinput6_end_h, auxinput7_end_h, auxinput8_end_h, &
              auxinput6_end_m, auxinput7_end_m, auxinput8_end_m, &
              auxinput6_end_s, auxinput7_end_s, auxinput8_end_s

   INTEGER :: auxinput9_end  , gfdda_end  , auxinput11_end  , &
              auxinput9_end_y, gfdda_end_y, auxinput11_end_y, &
              auxinput9_end_mo, gfdda_end_mo, auxinput11_end_mo, &
              auxinput9_end_d, gfdda_end_d, auxinput11_end_d, &
              auxinput9_end_h, gfdda_end_h, auxinput11_end_h, &
              auxinput9_end_m, gfdda_end_m, auxinput11_end_m, &
              auxinput9_end_s, gfdda_end_s, auxinput11_end_s

   INTEGER :: grid_fdda

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

   CALL nl_get_start_year(grid%id,start_year)
   CALL nl_get_start_month(grid%id,start_month)
   CALL nl_get_start_day(grid%id,start_day)
   CALL nl_get_start_hour(grid%id,start_hour)
   CALL nl_get_start_minute(grid%id,start_minute)
   CALL nl_get_start_second(grid%id,start_second)
   CALL WRFU_TimeSet(startTime, YY=start_year, MM=start_month, DD=start_day, &
                                H=start_hour, M=start_minute, S=start_second,&
                                      rc=rc)
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                         'WRFU_TimeSet(startTime) FAILED', &
                         __FILE__ , &
                         __LINE__  )
   CALL nl_get_run_days(1,run_days)
   CALL nl_get_run_hours(1,run_hours)
   CALL nl_get_run_minutes(1,run_minutes)
   CALL nl_get_run_seconds(1,run_seconds)

   IF ( grid%id .EQ. head_grid%id .AND. &
        ( run_days .gt. 0 .or. run_hours .gt. 0 .or. run_minutes .gt. 0 .or. run_seconds .gt. 0 )) THEN
     CALL WRFU_TimeIntervalSet ( run_length , D=run_days, H=run_hours, M=run_minutes, S=run_seconds, rc=rc )
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
   IF ( run_length .GT. zero_time ) THEN
     padding_interval = one_hour
   ELSE
     padding_interval = zero_time - one_hour
   ENDIF

   IF ( grid%id .EQ. head_grid%id ) THEN
      CALL nl_get_time_step ( 1, time_step )
      CALL nl_get_time_step_fract_num( 1, time_step_fract_num )
      CALL nl_get_time_step_fract_den( 1, time_step_fract_den )
      dt = real(time_step) + real(time_step_fract_num) / real(time_step_fract_den)
      CALL nl_set_dt( grid%id, dt )
      grid%dt = dt
      CALL WRFU_TimeIntervalSet(stepTime, S=time_step, Sn=time_step_fract_num, Sd=time_step_fract_den, rc=rc)
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(stepTime) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
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

! HISTORY INTERVAL
! history_interval is left there (and means minutes) for consistency, but 
! history_interval_m will take precedence if specified

   CALL nl_get_history_interval( grid%id, history_interval )   ! same as minutes
   CALL nl_get_history_interval_mo( grid%id, history_interval_mo )
   CALL nl_get_history_interval_d( grid%id, history_interval_d )
   CALL nl_get_history_interval_h( grid%id, history_interval_h )
   CALL nl_get_history_interval_m( grid%id, history_interval_m )
   CALL nl_get_history_interval_s( grid%id, history_interval_s )
   IF ( history_interval_m .EQ. 0 ) history_interval_m = history_interval

   IF ( MAX( history_interval_mo, history_interval_d,   &
             history_interval_h, history_interval_m , history_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=history_interval_mo, D=history_interval_d, &
                                          H=history_interval_h, M=history_interval_m, S=history_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(history_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_history_begin_y( grid%id, history_begin_y )
   CALL nl_get_history_begin_mo( grid%id, history_begin_mo )
   CALL nl_get_history_begin_d( grid%id, history_begin_d )
   CALL nl_get_history_begin_h( grid%id, history_begin_h )
   CALL nl_get_history_begin_m( grid%id, history_begin_m )
   CALL nl_get_history_begin_s( grid%id, history_begin_s )
   IF ( MAX( history_begin_y, history_begin_mo, history_begin_d,   &
             history_begin_h, history_begin_m , history_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=history_begin_mo, D=history_begin_d, &
                                              H=history_begin_h, M=history_begin_m, S=history_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(history_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_history_end_y( grid%id, history_end_y )
   CALL nl_get_history_end_mo( grid%id, history_end_mo )
   CALL nl_get_history_end_d( grid%id, history_end_d )
   CALL nl_get_history_end_h( grid%id, history_end_h )
   CALL nl_get_history_end_m( grid%id, history_end_m )
   CALL nl_get_history_end_s( grid%id, history_end_s )
   IF ( MAX( history_end_y, history_end_mo, history_end_d,   &
             history_end_h, history_end_m , history_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=history_end_mo, D=history_end_d, &
                                     H=history_end_h, M=history_end_m, S=history_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(history_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, HISTORY_ALARM, interval, begin_time, end_time )

   IF ( begin_time .EQ. zero_time ) THEN
      CALL WRFU_AlarmRingerOn( grid%alarms( HISTORY_ALARM ),  rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_AlarmRingerOn(HISTORY_ALARM) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ENDIF


! RESTART INTERVAL
! restart_interval is left there (and means minutes) for consistency, but
! restart_interval_m will take precedence if specified
   CALL nl_get_restart_interval( 1, restart_interval )   ! same as minutes
   CALL nl_get_restart_interval_mo( 1, restart_interval_mo )
   CALL nl_get_restart_interval_d( 1, restart_interval_d )
   CALL nl_get_restart_interval_h( 1, restart_interval_h )
   CALL nl_get_restart_interval_m( 1, restart_interval_m )
   CALL nl_get_restart_interval_s( 1, restart_interval_s )
   IF ( restart_interval_m .EQ. 0 ) restart_interval_m = restart_interval
   IF ( MAX( restart_interval_mo, restart_interval_d,   &
             restart_interval_h, restart_interval_m , restart_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=restart_interval_mo, D=restart_interval_d, &
                                        H=restart_interval_h, M=restart_interval_m, S=restart_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(restart_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, RESTART_ALARM, interval )

! INPUTOUT INTERVAL
   CALL nl_get_inputout_interval( grid%id, inputout_interval )   ! same as minutes
   CALL nl_get_inputout_interval_mo( grid%id, inputout_interval_mo )
   CALL nl_get_inputout_interval_d( grid%id, inputout_interval_d )
   CALL nl_get_inputout_interval_h( grid%id, inputout_interval_h )
   CALL nl_get_inputout_interval_m( grid%id, inputout_interval_m )
   CALL nl_get_inputout_interval_s( grid%id, inputout_interval_s )
   IF ( inputout_interval_m .EQ. 0 ) inputout_interval_m = inputout_interval

   IF ( MAX( inputout_interval_mo, inputout_interval_d,   &
             inputout_interval_h, inputout_interval_m , inputout_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=inputout_interval_mo, D=inputout_interval_d, &
                                        H=inputout_interval_h, M=inputout_interval_m, S=inputout_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(inputout_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_inputout_begin_y( grid%id, inputout_begin_y )
   CALL nl_get_inputout_begin_mo( grid%id, inputout_begin_mo )
   CALL nl_get_inputout_begin_d( grid%id, inputout_begin_d )
   CALL nl_get_inputout_begin_h( grid%id, inputout_begin_h )
   CALL nl_get_inputout_begin_m( grid%id, inputout_begin_m )
   CALL nl_get_inputout_begin_s( grid%id, inputout_begin_s )
   IF ( MAX( inputout_begin_y, inputout_begin_mo, inputout_begin_d,   &
             inputout_begin_h, inputout_begin_m , inputout_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=inputout_begin_mo, D=inputout_begin_d, &
                                      H=inputout_begin_h, M=inputout_begin_m, S=inputout_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(inputout_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_inputout_end_y( grid%id, inputout_end_y )
   CALL nl_get_inputout_end_mo( grid%id, inputout_end_mo )
   CALL nl_get_inputout_end_d( grid%id, inputout_end_d )
   CALL nl_get_inputout_end_h( grid%id, inputout_end_h )
   CALL nl_get_inputout_end_m( grid%id, inputout_end_m )
   CALL nl_get_inputout_end_s( grid%id, inputout_end_s )
   IF ( MAX( inputout_end_y, inputout_end_mo, inputout_end_d,   &
             inputout_end_h, inputout_end_m , inputout_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=inputout_end_mo, D=inputout_end_d, &
                                     H=inputout_end_h, M=inputout_end_m, S=inputout_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(inputout_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, INPUTOUT_ALARM, interval, begin_time, end_time )

! AUXHIST1 INTERVAL
! auxhist1_interval is left there (and means minutes) for consistency, but
! auxhist1_interval_m will take precedence if specified
   CALL nl_get_auxhist1_interval( grid%id, auxhist1_interval )   ! same as minutes
   CALL nl_get_auxhist1_interval_mo( grid%id, auxhist1_interval_mo )
   CALL nl_get_auxhist1_interval_d( grid%id, auxhist1_interval_d )
   CALL nl_get_auxhist1_interval_h( grid%id, auxhist1_interval_h )
   CALL nl_get_auxhist1_interval_m( grid%id, auxhist1_interval_m )
   CALL nl_get_auxhist1_interval_s( grid%id, auxhist1_interval_s )
   IF ( auxhist1_interval_m .EQ. 0 ) auxhist1_interval_m = auxhist1_interval

   IF ( MAX( auxhist1_interval_mo, auxhist1_interval_d,   &
             auxhist1_interval_h, auxhist1_interval_m , auxhist1_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxhist1_interval_mo, D=auxhist1_interval_d, &
                                        H=auxhist1_interval_h, M=auxhist1_interval_m, S=auxhist1_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist1_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxhist1_begin_y( grid%id, auxhist1_begin_y )
   CALL nl_get_auxhist1_begin_mo( grid%id, auxhist1_begin_mo )
   CALL nl_get_auxhist1_begin_d( grid%id, auxhist1_begin_d )
   CALL nl_get_auxhist1_begin_h( grid%id, auxhist1_begin_h )
   CALL nl_get_auxhist1_begin_m( grid%id, auxhist1_begin_m )
   CALL nl_get_auxhist1_begin_s( grid%id, auxhist1_begin_s )
   IF ( MAX( auxhist1_begin_y, auxhist1_begin_mo, auxhist1_begin_d,   &
             auxhist1_begin_h, auxhist1_begin_m , auxhist1_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxhist1_begin_mo, D=auxhist1_begin_d, &
                                      H=auxhist1_begin_h, M=auxhist1_begin_m, S=auxhist1_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist1_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxhist1_end_y( grid%id, auxhist1_end_y )
   CALL nl_get_auxhist1_end_mo( grid%id, auxhist1_end_mo )
   CALL nl_get_auxhist1_end_d( grid%id, auxhist1_end_d )
   CALL nl_get_auxhist1_end_h( grid%id, auxhist1_end_h )
   CALL nl_get_auxhist1_end_m( grid%id, auxhist1_end_m )
   CALL nl_get_auxhist1_end_s( grid%id, auxhist1_end_s )
   IF ( MAX( auxhist1_end_y, auxhist1_end_mo, auxhist1_end_d,   &
             auxhist1_end_h, auxhist1_end_m , auxhist1_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxhist1_end_mo, D=auxhist1_end_d, &
                                     H=auxhist1_end_h, M=auxhist1_end_m, S=auxhist1_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist1_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXHIST1_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXHIST1_ALARM ),  rc=rc )
   ENDIF


! AUXHIST2_ INTERVAL
! auxhist2_interval is left there (and means minutes) for consistency, but
! auxhist2_interval_m will take precedence if specified
   CALL nl_get_auxhist2_interval( grid%id, auxhist2_interval )   ! same as minutes
   CALL nl_get_auxhist2_interval_mo( grid%id, auxhist2_interval_mo )
   CALL nl_get_auxhist2_interval_d( grid%id, auxhist2_interval_d )
   CALL nl_get_auxhist2_interval_h( grid%id, auxhist2_interval_h )
   CALL nl_get_auxhist2_interval_m( grid%id, auxhist2_interval_m )
   CALL nl_get_auxhist2_interval_s( grid%id, auxhist2_interval_s )
   IF ( auxhist2_interval_m .EQ. 0) auxhist2_interval_m = auxhist2_interval

   IF ( MAX( auxhist2_interval_mo, auxhist2_interval_d,   &
             auxhist2_interval_h, auxhist2_interval_m , auxhist2_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxhist2_interval_mo, D=auxhist2_interval_d, &
                                        H=auxhist2_interval_h, M=auxhist2_interval_m, S=auxhist2_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist2_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxhist2_begin_y( grid%id, auxhist2_begin_y )
   CALL nl_get_auxhist2_begin_mo( grid%id, auxhist2_begin_mo )
   CALL nl_get_auxhist2_begin_d( grid%id, auxhist2_begin_d )
   CALL nl_get_auxhist2_begin_h( grid%id, auxhist2_begin_h )
   CALL nl_get_auxhist2_begin_m( grid%id, auxhist2_begin_m )
   CALL nl_get_auxhist2_begin_s( grid%id, auxhist2_begin_s )
   IF ( MAX( auxhist2_begin_y, auxhist2_begin_mo, auxhist2_begin_d,   &
             auxhist2_begin_h, auxhist2_begin_m , auxhist2_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxhist2_begin_mo, D=auxhist2_begin_d, &
                                      H=auxhist2_begin_h, M=auxhist2_begin_m, S=auxhist2_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist2_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxhist2_end_y( grid%id, auxhist2_end_y )
   CALL nl_get_auxhist2_end_mo( grid%id, auxhist2_end_mo )
   CALL nl_get_auxhist2_end_d( grid%id, auxhist2_end_d )
   CALL nl_get_auxhist2_end_h( grid%id, auxhist2_end_h )
   CALL nl_get_auxhist2_end_m( grid%id, auxhist2_end_m )
   CALL nl_get_auxhist2_end_s( grid%id, auxhist2_end_s )
   IF ( MAX( auxhist2_end_y, auxhist2_end_mo, auxhist2_end_d,   &
             auxhist2_end_h, auxhist2_end_m , auxhist2_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxhist2_end_mo, D=auxhist2_end_d, &
                                     H=auxhist2_end_h, M=auxhist2_end_m, S=auxhist2_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist2_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXHIST2_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXHIST2_ALARM ),  rc=rc )
   ENDIF

! AUXHIST3_ INTERVAL
! auxhist3_interval is left there (and means minutes) for consistency, but
! auxhist3_interval_m will take precedence if specified
   CALL nl_get_auxhist3_interval( grid%id, auxhist3_interval )   ! same as minutes
   CALL nl_get_auxhist3_interval_mo( grid%id, auxhist3_interval_mo )
   CALL nl_get_auxhist3_interval_d( grid%id, auxhist3_interval_d )
   CALL nl_get_auxhist3_interval_h( grid%id, auxhist3_interval_h )
   CALL nl_get_auxhist3_interval_m( grid%id, auxhist3_interval_m )
   CALL nl_get_auxhist3_interval_s( grid%id, auxhist3_interval_s )
   IF ( auxhist3_interval_m .EQ. 0 ) auxhist3_interval_m = auxhist3_interval

   IF ( MAX( auxhist3_interval_mo, auxhist3_interval_d,   &
             auxhist3_interval_h, auxhist3_interval_m , auxhist3_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxhist3_interval_mo, D=auxhist3_interval_d, &
                                        H=auxhist3_interval_h, M=auxhist3_interval_m, S=auxhist3_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist3_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxhist3_begin_y( grid%id, auxhist3_begin_y )
   CALL nl_get_auxhist3_begin_mo( grid%id, auxhist3_begin_mo )
   CALL nl_get_auxhist3_begin_d( grid%id, auxhist3_begin_d )
   CALL nl_get_auxhist3_begin_h( grid%id, auxhist3_begin_h )
   CALL nl_get_auxhist3_begin_m( grid%id, auxhist3_begin_m )
   CALL nl_get_auxhist3_begin_s( grid%id, auxhist3_begin_s )
   IF ( MAX( auxhist3_begin_y, auxhist3_begin_mo, auxhist3_begin_d,   &
             auxhist3_begin_h, auxhist3_begin_m , auxhist3_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxhist3_begin_mo, D=auxhist3_begin_d, &
                                      H=auxhist3_begin_h, M=auxhist3_begin_m, S=auxhist3_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist3_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxhist3_end_y( grid%id, auxhist3_end_y )
   CALL nl_get_auxhist3_end_mo( grid%id, auxhist3_end_mo )
   CALL nl_get_auxhist3_end_d( grid%id, auxhist3_end_d )
   CALL nl_get_auxhist3_end_h( grid%id, auxhist3_end_h )
   CALL nl_get_auxhist3_end_m( grid%id, auxhist3_end_m )
   CALL nl_get_auxhist3_end_s( grid%id, auxhist3_end_s )
   IF ( MAX( auxhist3_end_y, auxhist3_end_mo, auxhist3_end_d,   &
             auxhist3_end_h, auxhist3_end_m , auxhist3_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxhist3_end_mo, D=auxhist3_end_d, &
                                     H=auxhist3_end_h, M=auxhist3_end_m, S=auxhist3_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist3_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXHIST3_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXHIST3_ALARM ),  rc=rc )
   ENDIF

! AUXHIST4_ INTERVAL
! auxhist4_interval is left there (and means minutes) for consistency, but
! auxhist4_interval_m will take precedence if specified
   CALL nl_get_auxhist4_interval( grid%id, auxhist4_interval )   ! same as minutes
   CALL nl_get_auxhist4_interval_mo( grid%id, auxhist4_interval_mo )
   CALL nl_get_auxhist4_interval_d( grid%id, auxhist4_interval_d )
   CALL nl_get_auxhist4_interval_h( grid%id, auxhist4_interval_h )
   CALL nl_get_auxhist4_interval_m( grid%id, auxhist4_interval_m )
   CALL nl_get_auxhist4_interval_s( grid%id, auxhist4_interval_s )
   IF ( auxhist4_interval_m .EQ. 0 ) auxhist4_interval_m = auxhist4_interval

   IF ( MAX( auxhist4_interval_mo, auxhist4_interval_d,   &
             auxhist4_interval_h, auxhist4_interval_m , auxhist4_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxhist4_interval_mo, D=auxhist4_interval_d, &
                                        H=auxhist4_interval_h, M=auxhist4_interval_m, S=auxhist4_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist4_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxhist4_begin_y( grid%id, auxhist4_begin_y )
   CALL nl_get_auxhist4_begin_mo( grid%id, auxhist4_begin_mo )
   CALL nl_get_auxhist4_begin_d( grid%id, auxhist4_begin_d )
   CALL nl_get_auxhist4_begin_h( grid%id, auxhist4_begin_h )
   CALL nl_get_auxhist4_begin_m( grid%id, auxhist4_begin_m )
   CALL nl_get_auxhist4_begin_s( grid%id, auxhist4_begin_s )
   IF ( MAX( auxhist4_begin_y, auxhist4_begin_mo, auxhist4_begin_d,   &
             auxhist4_begin_h, auxhist4_begin_m , auxhist4_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxhist4_begin_mo, D=auxhist4_begin_d, &
                                      H=auxhist4_begin_h, M=auxhist4_begin_m, S=auxhist4_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist4_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxhist4_end_y( grid%id, auxhist4_end_y )
   CALL nl_get_auxhist4_end_mo( grid%id, auxhist4_end_mo )
   CALL nl_get_auxhist4_end_d( grid%id, auxhist4_end_d )
   CALL nl_get_auxhist4_end_h( grid%id, auxhist4_end_h )
   CALL nl_get_auxhist4_end_m( grid%id, auxhist4_end_m )
   CALL nl_get_auxhist4_end_s( grid%id, auxhist4_end_s )
   IF ( MAX( auxhist4_end_y, auxhist4_end_mo, auxhist4_end_d,   &
             auxhist4_end_h, auxhist4_end_m , auxhist4_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxhist4_end_mo, D=auxhist4_end_d, &
                                     H=auxhist4_end_h, M=auxhist4_end_m, S=auxhist4_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist4_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXHIST4_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXHIST4_ALARM ),  rc=rc )
   ENDIF

! AUXHIST5_ INTERVAL
! auxhist5_interval is left there (and means minutes) for consistency, but
! auxhist5_interval_m will take precedence if specified
   CALL nl_get_auxhist5_interval( grid%id, auxhist5_interval )   ! same as minutes
   CALL nl_get_auxhist5_interval_mo( grid%id, auxhist5_interval_mo )
   CALL nl_get_auxhist5_interval_d( grid%id, auxhist5_interval_d )
   CALL nl_get_auxhist5_interval_h( grid%id, auxhist5_interval_h )
   CALL nl_get_auxhist5_interval_m( grid%id, auxhist5_interval_m )
   CALL nl_get_auxhist5_interval_s( grid%id, auxhist5_interval_s )
   IF ( auxhist5_interval_m .EQ. 0 ) auxhist5_interval_m = auxhist5_interval

   IF ( MAX( auxhist5_interval_mo, auxhist5_interval_d,   &
             auxhist5_interval_h, auxhist5_interval_m , auxhist5_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxhist5_interval_mo, D=auxhist5_interval_d, &
                                        H=auxhist5_interval_h, M=auxhist5_interval_m, S=auxhist5_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist5_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxhist5_begin_y( grid%id, auxhist5_begin_y )
   CALL nl_get_auxhist5_begin_mo( grid%id, auxhist5_begin_mo )
   CALL nl_get_auxhist5_begin_d( grid%id, auxhist5_begin_d )
   CALL nl_get_auxhist5_begin_h( grid%id, auxhist5_begin_h )
   CALL nl_get_auxhist5_begin_m( grid%id, auxhist5_begin_m )
   CALL nl_get_auxhist5_begin_s( grid%id, auxhist5_begin_s )
   IF ( MAX( auxhist5_begin_y, auxhist5_begin_mo, auxhist5_begin_d,   &
             auxhist5_begin_h, auxhist5_begin_m , auxhist5_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxhist5_begin_mo, D=auxhist5_begin_d, &
                                      H=auxhist5_begin_h, M=auxhist5_begin_m, S=auxhist5_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist5_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxhist5_end_y( grid%id, auxhist5_end_y )
   CALL nl_get_auxhist5_end_mo( grid%id, auxhist5_end_mo )
   CALL nl_get_auxhist5_end_d( grid%id, auxhist5_end_d )
   CALL nl_get_auxhist5_end_h( grid%id, auxhist5_end_h )
   CALL nl_get_auxhist5_end_m( grid%id, auxhist5_end_m )
   CALL nl_get_auxhist5_end_s( grid%id, auxhist5_end_s )
   IF ( MAX( auxhist5_end_y, auxhist5_end_mo, auxhist5_end_d,   &
             auxhist5_end_h, auxhist5_end_m , auxhist5_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxhist5_end_mo, D=auxhist5_end_d, &
                                     H=auxhist5_end_h, M=auxhist5_end_m, S=auxhist5_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist5_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXHIST5_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXHIST5_ALARM ),  rc=rc )
   ENDIF

! AUXHIST6_ INTERVAL
! auxhist6_interval is left there (and means minutes) for consistency, but
! auxhist6_interval_m will take precedence if specified
   CALL nl_get_auxhist6_interval( grid%id, auxhist6_interval )   ! same as minutes
   CALL nl_get_auxhist6_interval_mo( grid%id, auxhist6_interval_mo )
   CALL nl_get_auxhist6_interval_d( grid%id, auxhist6_interval_d )
   CALL nl_get_auxhist6_interval_h( grid%id, auxhist6_interval_h )
   CALL nl_get_auxhist6_interval_m( grid%id, auxhist6_interval_m )
   CALL nl_get_auxhist6_interval_s( grid%id, auxhist6_interval_s )
   IF ( auxhist6_interval_m .EQ. 0 ) auxhist6_interval_m = auxhist6_interval

   IF ( MAX( auxhist6_interval_mo, auxhist6_interval_d,   &
             auxhist6_interval_h, auxhist6_interval_m , auxhist6_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxhist6_interval_mo, D=auxhist6_interval_d, &
                                        H=auxhist6_interval_h, M=auxhist6_interval_m, S=auxhist6_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist6_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxhist6_begin_y( grid%id, auxhist6_begin_y )
   CALL nl_get_auxhist6_begin_mo( grid%id, auxhist6_begin_mo )
   CALL nl_get_auxhist6_begin_d( grid%id, auxhist6_begin_d )
   CALL nl_get_auxhist6_begin_h( grid%id, auxhist6_begin_h )
   CALL nl_get_auxhist6_begin_m( grid%id, auxhist6_begin_m )
   CALL nl_get_auxhist6_begin_s( grid%id, auxhist6_begin_s )
   IF ( MAX( auxhist6_begin_y, auxhist6_begin_mo, auxhist6_begin_d,   &
             auxhist6_begin_h, auxhist6_begin_m , auxhist6_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxhist6_begin_mo, D=auxhist6_begin_d, &
                                      H=auxhist6_begin_h, M=auxhist6_begin_m, S=auxhist6_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist6_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxhist6_end_y( grid%id, auxhist6_end_y )
   CALL nl_get_auxhist6_end_mo( grid%id, auxhist6_end_mo )
   CALL nl_get_auxhist6_end_d( grid%id, auxhist6_end_d )
   CALL nl_get_auxhist6_end_h( grid%id, auxhist6_end_h )
   CALL nl_get_auxhist6_end_m( grid%id, auxhist6_end_m )
   CALL nl_get_auxhist6_end_s( grid%id, auxhist6_end_s )
   IF ( MAX( auxhist6_end_y, auxhist6_end_mo, auxhist6_end_d,   &
             auxhist6_end_h, auxhist6_end_m , auxhist6_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxhist6_end_mo, D=auxhist6_end_d, &
                                     H=auxhist6_end_h, M=auxhist6_end_m, S=auxhist6_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist6_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXHIST6_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXHIST6_ALARM ),  rc=rc )
   ENDIF


! AUXHIST7_ INTERVAL
! auxhist7_interval is left there (and means minutes) for consistency, but
! auxhist7_interval_m will take precedence if specified
   CALL nl_get_auxhist7_interval( grid%id, auxhist7_interval )   ! same as minutes
   CALL nl_get_auxhist7_interval_mo( grid%id, auxhist7_interval_mo )
   CALL nl_get_auxhist7_interval_d( grid%id, auxhist7_interval_d )
   CALL nl_get_auxhist7_interval_h( grid%id, auxhist7_interval_h )
   CALL nl_get_auxhist7_interval_m( grid%id, auxhist7_interval_m )
   CALL nl_get_auxhist7_interval_s( grid%id, auxhist7_interval_s )
   IF ( auxhist7_interval_m .EQ. 0 ) auxhist7_interval_m = auxhist7_interval

   IF ( MAX( auxhist7_interval_mo, auxhist7_interval_d,   &
             auxhist7_interval_h, auxhist7_interval_m , auxhist7_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxhist7_interval_mo, D=auxhist7_interval_d, &
                                        H=auxhist7_interval_h, M=auxhist7_interval_m, S=auxhist7_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist7_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxhist7_begin_y( grid%id, auxhist7_begin_y )
   CALL nl_get_auxhist7_begin_mo( grid%id, auxhist7_begin_mo )
   CALL nl_get_auxhist7_begin_d( grid%id, auxhist7_begin_d )
   CALL nl_get_auxhist7_begin_h( grid%id, auxhist7_begin_h )
   CALL nl_get_auxhist7_begin_m( grid%id, auxhist7_begin_m )
   CALL nl_get_auxhist7_begin_s( grid%id, auxhist7_begin_s )
   IF ( MAX( auxhist7_begin_y, auxhist7_begin_mo, auxhist7_begin_d,   &
             auxhist7_begin_h, auxhist7_begin_m , auxhist7_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxhist7_begin_mo, D=auxhist7_begin_d, &
                                      H=auxhist7_begin_h, M=auxhist7_begin_m, S=auxhist7_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist7_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxhist7_end_y( grid%id, auxhist7_end_y )
   CALL nl_get_auxhist7_end_mo( grid%id, auxhist7_end_mo )
   CALL nl_get_auxhist7_end_d( grid%id, auxhist7_end_d )
   CALL nl_get_auxhist7_end_h( grid%id, auxhist7_end_h )
   CALL nl_get_auxhist7_end_m( grid%id, auxhist7_end_m )
   CALL nl_get_auxhist7_end_s( grid%id, auxhist7_end_s )
   IF ( MAX( auxhist7_end_y, auxhist7_end_mo, auxhist7_end_d,   &
             auxhist7_end_h, auxhist7_end_m , auxhist7_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxhist7_end_mo, D=auxhist7_end_d, &
                                     H=auxhist7_end_h, M=auxhist7_end_m, S=auxhist7_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist7_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXHIST7_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXHIST7_ALARM ),  rc=rc )
   ENDIF

! AUXHIST8_ INTERVAL
! auxhist8_interval is left there (and means minutes) for consistency, but
! auxhist8_interval_m will take precedence if specified
   CALL nl_get_auxhist8_interval( grid%id, auxhist8_interval )   ! same as minutes
   CALL nl_get_auxhist8_interval_mo( grid%id, auxhist8_interval_mo )
   CALL nl_get_auxhist8_interval_d( grid%id, auxhist8_interval_d )
   CALL nl_get_auxhist8_interval_h( grid%id, auxhist8_interval_h )
   CALL nl_get_auxhist8_interval_m( grid%id, auxhist8_interval_m )
   CALL nl_get_auxhist8_interval_s( grid%id, auxhist8_interval_s )
   IF ( auxhist8_interval_m .EQ. 0 ) auxhist8_interval_m = auxhist8_interval

   IF ( MAX( auxhist8_interval_mo, auxhist8_interval_d,   &
             auxhist8_interval_h, auxhist8_interval_m , auxhist8_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxhist8_interval_mo, D=auxhist8_interval_d, &
                                        H=auxhist8_interval_h, M=auxhist8_interval_m, S=auxhist8_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist8_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxhist8_begin_y( grid%id, auxhist8_begin_y )
   CALL nl_get_auxhist8_begin_mo( grid%id, auxhist8_begin_mo )
   CALL nl_get_auxhist8_begin_d( grid%id, auxhist8_begin_d )
   CALL nl_get_auxhist8_begin_h( grid%id, auxhist8_begin_h )
   CALL nl_get_auxhist8_begin_m( grid%id, auxhist8_begin_m )
   CALL nl_get_auxhist8_begin_s( grid%id, auxhist8_begin_s )
   IF ( MAX( auxhist8_begin_y, auxhist8_begin_mo, auxhist8_begin_d,   &
             auxhist8_begin_h, auxhist8_begin_m , auxhist8_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxhist8_begin_mo, D=auxhist8_begin_d, &
                                      H=auxhist8_begin_h, M=auxhist8_begin_m, S=auxhist8_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist8_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxhist8_end_y( grid%id, auxhist8_end_y )
   CALL nl_get_auxhist8_end_mo( grid%id, auxhist8_end_mo )
   CALL nl_get_auxhist8_end_d( grid%id, auxhist8_end_d )
   CALL nl_get_auxhist8_end_h( grid%id, auxhist8_end_h )
   CALL nl_get_auxhist8_end_m( grid%id, auxhist8_end_m )
   CALL nl_get_auxhist8_end_s( grid%id, auxhist8_end_s )
   IF ( MAX( auxhist8_end_y, auxhist8_end_mo, auxhist8_end_d,   &
             auxhist8_end_h, auxhist8_end_m , auxhist8_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxhist8_end_mo, D=auxhist8_end_d, &
                                     H=auxhist8_end_h, M=auxhist8_end_m, S=auxhist8_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist8_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXHIST8_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXHIST8_ALARM ),  rc=rc )
   ENDIF

! AUXHIST9_ INTERVAL
! auxhist9_interval is left there (and means minutes) for consistency, but
! auxhist9_interval_m will take precedence if specified
   CALL nl_get_auxhist9_interval( grid%id, auxhist9_interval )   ! same as minutes
   CALL nl_get_auxhist9_interval_mo( grid%id, auxhist9_interval_mo )
   CALL nl_get_auxhist9_interval_d( grid%id, auxhist9_interval_d )
   CALL nl_get_auxhist9_interval_h( grid%id, auxhist9_interval_h )
   CALL nl_get_auxhist9_interval_m( grid%id, auxhist9_interval_m )
   CALL nl_get_auxhist9_interval_s( grid%id, auxhist9_interval_s )
   IF ( auxhist9_interval_m .EQ. 0 ) auxhist9_interval_m = auxhist9_interval

   IF ( MAX( auxhist9_interval_mo, auxhist9_interval_d,   &
             auxhist9_interval_h, auxhist9_interval_m , auxhist9_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxhist9_interval_mo, D=auxhist9_interval_d, &
                                        H=auxhist9_interval_h, M=auxhist9_interval_m, S=auxhist9_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist9_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxhist9_begin_y( grid%id, auxhist9_begin_y )
   CALL nl_get_auxhist9_begin_mo( grid%id, auxhist9_begin_mo )
   CALL nl_get_auxhist9_begin_d( grid%id, auxhist9_begin_d )
   CALL nl_get_auxhist9_begin_h( grid%id, auxhist9_begin_h )
   CALL nl_get_auxhist9_begin_m( grid%id, auxhist9_begin_m )
   CALL nl_get_auxhist9_begin_s( grid%id, auxhist9_begin_s )
   IF ( MAX( auxhist9_begin_y, auxhist9_begin_mo, auxhist9_begin_d,   &
             auxhist9_begin_h, auxhist9_begin_m , auxhist9_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxhist9_begin_mo, D=auxhist9_begin_d, &
                                      H=auxhist9_begin_h, M=auxhist9_begin_m, S=auxhist9_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist9_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxhist9_end_y( grid%id, auxhist9_end_y )
   CALL nl_get_auxhist9_end_mo( grid%id, auxhist9_end_mo )
   CALL nl_get_auxhist9_end_d( grid%id, auxhist9_end_d )
   CALL nl_get_auxhist9_end_h( grid%id, auxhist9_end_h )
   CALL nl_get_auxhist9_end_m( grid%id, auxhist9_end_m )
   CALL nl_get_auxhist9_end_s( grid%id, auxhist9_end_s )
   IF ( MAX( auxhist9_end_y, auxhist9_end_mo, auxhist9_end_d,   &
             auxhist9_end_h, auxhist9_end_m , auxhist9_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxhist9_end_mo, D=auxhist9_end_d, &
                                     H=auxhist9_end_h, M=auxhist9_end_m, S=auxhist9_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist9_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXHIST9_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXHIST9_ALARM ),  rc=rc )
   ENDIF

! AUXHIST10_ INTERVAL
! auxhist10_interval is left there (and means minutes) for consistency, but
! auxhist10_interval_m will take precedence if specified
   CALL nl_get_auxhist10_interval( grid%id, auxhist10_interval )   ! same as minutes
   CALL nl_get_auxhist10_interval_mo( grid%id, auxhist10_interval_mo )
   CALL nl_get_auxhist10_interval_d( grid%id, auxhist10_interval_d )
   CALL nl_get_auxhist10_interval_h( grid%id, auxhist10_interval_h )
   CALL nl_get_auxhist10_interval_m( grid%id, auxhist10_interval_m )
   CALL nl_get_auxhist10_interval_s( grid%id, auxhist10_interval_s )
   IF ( auxhist10_interval_m .EQ. 0 ) auxhist10_interval_m = auxhist10_interval

   IF ( MAX( auxhist10_interval_mo, auxhist10_interval_d,   &
             auxhist10_interval_h, auxhist10_interval_m , auxhist10_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxhist10_interval_mo, D=auxhist10_interval_d, &
                                        H=auxhist10_interval_h, M=auxhist10_interval_m, S=auxhist10_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist10_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxhist10_begin_y( grid%id, auxhist10_begin_y )
   CALL nl_get_auxhist10_begin_mo( grid%id, auxhist10_begin_mo )
   CALL nl_get_auxhist10_begin_d( grid%id, auxhist10_begin_d )
   CALL nl_get_auxhist10_begin_h( grid%id, auxhist10_begin_h )
   CALL nl_get_auxhist10_begin_m( grid%id, auxhist10_begin_m )
   CALL nl_get_auxhist10_begin_s( grid%id, auxhist10_begin_s )
   IF ( MAX( auxhist10_begin_y, auxhist10_begin_mo, auxhist10_begin_d,   &
             auxhist10_begin_h, auxhist10_begin_m , auxhist10_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxhist10_begin_mo, D=auxhist10_begin_d, &
                                      H=auxhist10_begin_h, M=auxhist10_begin_m, S=auxhist10_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist10_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxhist10_end_y( grid%id, auxhist10_end_y )
   CALL nl_get_auxhist10_end_mo( grid%id, auxhist10_end_mo )
   CALL nl_get_auxhist10_end_d( grid%id, auxhist10_end_d )
   CALL nl_get_auxhist10_end_h( grid%id, auxhist10_end_h )
   CALL nl_get_auxhist10_end_m( grid%id, auxhist10_end_m )
   CALL nl_get_auxhist10_end_s( grid%id, auxhist10_end_s )
   IF ( MAX( auxhist10_end_y, auxhist10_end_mo, auxhist10_end_d,   &
             auxhist10_end_h, auxhist10_end_m , auxhist10_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxhist10_end_mo, D=auxhist10_end_d, &
                                     H=auxhist10_end_h, M=auxhist10_end_m, S=auxhist10_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist10_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXHIST10_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXHIST10_ALARM ),  rc=rc )
   ENDIF

! AUXHIST11_ INTERVAL
! auxhist11_interval is left there (and means minutes) for consistency, but
! auxhist11_interval_m will take precedence if specified
   CALL nl_get_auxhist11_interval( grid%id, auxhist11_interval )   ! same as minutes
   CALL nl_get_auxhist11_interval_mo( grid%id, auxhist11_interval_mo )
   CALL nl_get_auxhist11_interval_d( grid%id, auxhist11_interval_d )
   CALL nl_get_auxhist11_interval_h( grid%id, auxhist11_interval_h )
   CALL nl_get_auxhist11_interval_m( grid%id, auxhist11_interval_m )
   CALL nl_get_auxhist11_interval_s( grid%id, auxhist11_interval_s )
   IF ( auxhist11_interval_m .EQ. 0 ) auxhist11_interval_m = auxhist11_interval

   IF ( MAX( auxhist11_interval_mo, auxhist11_interval_d,   &
             auxhist11_interval_h, auxhist11_interval_m , auxhist11_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxhist11_interval_mo, D=auxhist11_interval_d, &
                                        H=auxhist11_interval_h, M=auxhist11_interval_m, S=auxhist11_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist11_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxhist11_begin_y( grid%id, auxhist11_begin_y )
   CALL nl_get_auxhist11_begin_mo( grid%id, auxhist11_begin_mo )
   CALL nl_get_auxhist11_begin_d( grid%id, auxhist11_begin_d )
   CALL nl_get_auxhist11_begin_h( grid%id, auxhist11_begin_h )
   CALL nl_get_auxhist11_begin_m( grid%id, auxhist11_begin_m )
   CALL nl_get_auxhist11_begin_s( grid%id, auxhist11_begin_s )
   IF ( MAX( auxhist11_begin_y, auxhist11_begin_mo, auxhist11_begin_d,   &
             auxhist11_begin_h, auxhist11_begin_m , auxhist11_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxhist11_begin_mo, D=auxhist11_begin_d, &
                                      H=auxhist11_begin_h, M=auxhist11_begin_m, S=auxhist11_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist11_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxhist11_end_y( grid%id, auxhist11_end_y )
   CALL nl_get_auxhist11_end_mo( grid%id, auxhist11_end_mo )
   CALL nl_get_auxhist11_end_d( grid%id, auxhist11_end_d )
   CALL nl_get_auxhist11_end_h( grid%id, auxhist11_end_h )
   CALL nl_get_auxhist11_end_m( grid%id, auxhist11_end_m )
   CALL nl_get_auxhist11_end_s( grid%id, auxhist11_end_s )
   IF ( MAX( auxhist11_end_y, auxhist11_end_mo, auxhist11_end_d,   &
             auxhist11_end_h, auxhist11_end_m , auxhist11_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxhist11_end_mo, D=auxhist11_end_d, &
                                     H=auxhist11_end_h, M=auxhist11_end_m, S=auxhist11_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist11_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXHIST11_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXHIST11_ALARM ),  rc=rc )
   ENDIF

! AUXINPUT1_ INTERVAL
! auxinput1_interval is left there (and means minutes) for consistency, but
! auxinput1_interval_m will take precedence if specified
   CALL nl_get_auxinput1_interval( grid%id, auxinput1_interval )   ! same as minutes
   CALL nl_get_auxinput1_interval_mo( grid%id, auxinput1_interval_mo )
   CALL nl_get_auxinput1_interval_d( grid%id, auxinput1_interval_d )
   CALL nl_get_auxinput1_interval_h( grid%id, auxinput1_interval_h )
   CALL nl_get_auxinput1_interval_m( grid%id, auxinput1_interval_m )
   CALL nl_get_auxinput1_interval_s( grid%id, auxinput1_interval_s )
   IF ( auxinput1_interval_m .EQ. 0 ) auxinput1_interval_m = auxinput1_interval

   IF ( MAX( auxinput1_interval_mo, auxinput1_interval_d,   &
             auxinput1_interval_h, auxinput1_interval_m , auxinput1_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxinput1_interval_mo, D=auxinput1_interval_d, &
                                        H=auxinput1_interval_h, M=auxinput1_interval_m, S=auxinput1_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput1_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxinput1_begin_y( grid%id, auxinput1_begin_y )
   CALL nl_get_auxinput1_begin_mo( grid%id, auxinput1_begin_mo )
   CALL nl_get_auxinput1_begin_d( grid%id, auxinput1_begin_d )
   CALL nl_get_auxinput1_begin_h( grid%id, auxinput1_begin_h )
   CALL nl_get_auxinput1_begin_m( grid%id, auxinput1_begin_m )
   CALL nl_get_auxinput1_begin_s( grid%id, auxinput1_begin_s )
   IF ( MAX( auxinput1_begin_y, auxinput1_begin_mo, auxinput1_begin_d,   &
             auxinput1_begin_h, auxinput1_begin_m , auxinput1_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxinput1_begin_mo, D=auxinput1_begin_d, &
                                      H=auxinput1_begin_h, M=auxinput1_begin_m, S=auxinput1_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput1_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxinput1_end_y( grid%id, auxinput1_end_y )
   CALL nl_get_auxinput1_end_mo( grid%id, auxinput1_end_mo )
   CALL nl_get_auxinput1_end_d( grid%id, auxinput1_end_d )
   CALL nl_get_auxinput1_end_h( grid%id, auxinput1_end_h )
   CALL nl_get_auxinput1_end_m( grid%id, auxinput1_end_m )
   CALL nl_get_auxinput1_end_s( grid%id, auxinput1_end_s )
   IF ( MAX( auxinput1_end_y, auxinput1_end_mo, auxinput1_end_d,   &
             auxinput1_end_h, auxinput1_end_m , auxinput1_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxinput1_end_mo, D=auxinput1_end_d, &
                                     H=auxinput1_end_h, M=auxinput1_end_m, S=auxinput1_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput1_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXINPUT1_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT1_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(AUXINPUT1_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF

! AUXINPUT2_ INTERVAL
! auxinput2_interval is left there (and means minutes) for consistency, but
! auxinput2_interval_m will take precedence if specified
   CALL nl_get_auxinput2_interval( grid%id, auxinput2_interval )   ! same as minutes
   CALL nl_get_auxinput2_interval_mo( grid%id, auxinput2_interval_mo )
   CALL nl_get_auxinput2_interval_d( grid%id, auxinput2_interval_d )
   CALL nl_get_auxinput2_interval_h( grid%id, auxinput2_interval_h )
   CALL nl_get_auxinput2_interval_m( grid%id, auxinput2_interval_m )
   CALL nl_get_auxinput2_interval_s( grid%id, auxinput2_interval_s )
   IF ( auxinput2_interval_m .EQ. 0 ) auxinput2_interval_m = auxinput2_interval

   IF ( MAX( auxinput2_interval_mo, auxinput2_interval_d,   &
             auxinput2_interval_h, auxinput2_interval_m , auxinput2_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxinput2_interval_mo, D=auxinput2_interval_d, &
                                        H=auxinput2_interval_h, M=auxinput2_interval_m, S=auxinput2_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput2_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxinput2_begin_y( grid%id, auxinput2_begin_y )
   CALL nl_get_auxinput2_begin_mo( grid%id, auxinput2_begin_mo )
   CALL nl_get_auxinput2_begin_d( grid%id, auxinput2_begin_d )
   CALL nl_get_auxinput2_begin_h( grid%id, auxinput2_begin_h )
   CALL nl_get_auxinput2_begin_m( grid%id, auxinput2_begin_m )
   CALL nl_get_auxinput2_begin_s( grid%id, auxinput2_begin_s )
   IF ( MAX( auxinput2_begin_y, auxinput2_begin_mo, auxinput2_begin_d,   &
             auxinput2_begin_h, auxinput2_begin_m , auxinput2_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxinput2_begin_mo, D=auxinput2_begin_d, &
                                      H=auxinput2_begin_h, M=auxinput2_begin_m, S=auxinput2_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput2_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxinput2_end_y( grid%id, auxinput2_end_y )
   CALL nl_get_auxinput2_end_mo( grid%id, auxinput2_end_mo )
   CALL nl_get_auxinput2_end_d( grid%id, auxinput2_end_d )
   CALL nl_get_auxinput2_end_h( grid%id, auxinput2_end_h )
   CALL nl_get_auxinput2_end_m( grid%id, auxinput2_end_m )
   CALL nl_get_auxinput2_end_s( grid%id, auxinput2_end_s )
   IF ( MAX( auxinput2_end_y, auxinput2_end_mo, auxinput2_end_d,   &
             auxinput2_end_h, auxinput2_end_m , auxinput2_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxinput2_end_mo, D=auxinput2_end_d, &
                                     H=auxinput2_end_h, M=auxinput2_end_m, S=auxinput2_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput2_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXINPUT2_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT2_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(AUXINPUT2_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF

! AUXINPUT3_ INTERVAL
! auxinput3_interval is left there (and means minutes) for consistency, but
! auxinput3_interval_m will take precedence if specified
   CALL nl_get_auxinput3_interval( grid%id, auxinput3_interval )   ! same as minutes
   CALL nl_get_auxinput3_interval_mo( grid%id, auxinput3_interval_mo )
   CALL nl_get_auxinput3_interval_d( grid%id, auxinput3_interval_d )
   CALL nl_get_auxinput3_interval_h( grid%id, auxinput3_interval_h )
   CALL nl_get_auxinput3_interval_m( grid%id, auxinput3_interval_m )
   CALL nl_get_auxinput3_interval_s( grid%id, auxinput3_interval_s )
   IF ( auxinput3_interval_m .EQ. 0 ) auxinput3_interval_m = auxinput3_interval

   IF ( MAX( auxinput3_interval_mo, auxinput3_interval_d,   &
             auxinput3_interval_h, auxinput3_interval_m , auxinput3_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxinput3_interval_mo, D=auxinput3_interval_d, &
                                        H=auxinput3_interval_h, M=auxinput3_interval_m, S=auxinput3_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput3_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxinput3_begin_y( grid%id, auxinput3_begin_y )
   CALL nl_get_auxinput3_begin_mo( grid%id, auxinput3_begin_mo )
   CALL nl_get_auxinput3_begin_d( grid%id, auxinput3_begin_d )
   CALL nl_get_auxinput3_begin_h( grid%id, auxinput3_begin_h )
   CALL nl_get_auxinput3_begin_m( grid%id, auxinput3_begin_m )
   CALL nl_get_auxinput3_begin_s( grid%id, auxinput3_begin_s )
   IF ( MAX( auxinput3_begin_y, auxinput3_begin_mo, auxinput3_begin_d,   &
             auxinput3_begin_h, auxinput3_begin_m , auxinput3_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxinput3_begin_mo, D=auxinput3_begin_d, &
                                      H=auxinput3_begin_h, M=auxinput3_begin_m, S=auxinput3_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput3_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxinput3_end_y( grid%id, auxinput3_end_y )
   CALL nl_get_auxinput3_end_mo( grid%id, auxinput3_end_mo )
   CALL nl_get_auxinput3_end_d( grid%id, auxinput3_end_d )
   CALL nl_get_auxinput3_end_h( grid%id, auxinput3_end_h )
   CALL nl_get_auxinput3_end_m( grid%id, auxinput3_end_m )
   CALL nl_get_auxinput3_end_s( grid%id, auxinput3_end_s )
   IF ( MAX( auxinput3_end_y, auxinput3_end_mo, auxinput3_end_d,   &
             auxinput3_end_h, auxinput3_end_m , auxinput3_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxinput3_end_mo, D=auxinput3_end_d, &
                                     H=auxinput3_end_h, M=auxinput3_end_m, S=auxinput3_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput3_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXINPUT3_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT3_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(AUXINPUT3_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF

! AUXINPUT4_ INTERVAL
! auxinput4_interval is left there (and means minutes) for consistency, but
! auxinput4_interval_m will take precedence if specified
   CALL nl_get_auxinput4_interval( grid%id, auxinput4_interval )   ! same as minutes
   CALL nl_get_auxinput4_interval_mo( grid%id, auxinput4_interval_mo )
   CALL nl_get_auxinput4_interval_d( grid%id, auxinput4_interval_d )
   CALL nl_get_auxinput4_interval_h( grid%id, auxinput4_interval_h )
   CALL nl_get_auxinput4_interval_m( grid%id, auxinput4_interval_m )
   CALL nl_get_auxinput4_interval_s( grid%id, auxinput4_interval_s )
   IF ( auxinput4_interval_m .EQ. 0 ) auxinput4_interval_m = auxinput4_interval

   IF ( MAX( auxinput4_interval_mo, auxinput4_interval_d,   &
             auxinput4_interval_h, auxinput4_interval_m , auxinput4_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxinput4_interval_mo, D=auxinput4_interval_d, &
                                        H=auxinput4_interval_h, M=auxinput4_interval_m, S=auxinput4_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput4_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxinput4_begin_y( grid%id, auxinput4_begin_y )
   CALL nl_get_auxinput4_begin_mo( grid%id, auxinput4_begin_mo )
   CALL nl_get_auxinput4_begin_d( grid%id, auxinput4_begin_d )
   CALL nl_get_auxinput4_begin_h( grid%id, auxinput4_begin_h )
   CALL nl_get_auxinput4_begin_m( grid%id, auxinput4_begin_m )
   CALL nl_get_auxinput4_begin_s( grid%id, auxinput4_begin_s )
   IF ( MAX( auxinput4_begin_y, auxinput4_begin_mo, auxinput4_begin_d,   &
             auxinput4_begin_h, auxinput4_begin_m , auxinput4_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxinput4_begin_mo, D=auxinput4_begin_d, &
                                      H=auxinput4_begin_h, M=auxinput4_begin_m, S=auxinput4_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput4_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxinput4_end_y( grid%id, auxinput4_end_y )
   CALL nl_get_auxinput4_end_mo( grid%id, auxinput4_end_mo )
   CALL nl_get_auxinput4_end_d( grid%id, auxinput4_end_d )
   CALL nl_get_auxinput4_end_h( grid%id, auxinput4_end_h )
   CALL nl_get_auxinput4_end_m( grid%id, auxinput4_end_m )
   CALL nl_get_auxinput4_end_s( grid%id, auxinput4_end_s )
   IF ( MAX( auxinput4_end_y, auxinput4_end_mo, auxinput4_end_d,   &
             auxinput4_end_h, auxinput4_end_m , auxinput4_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxinput4_end_mo, D=auxinput4_end_d, &
                                     H=auxinput4_end_h, M=auxinput4_end_m, S=auxinput4_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput4_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXINPUT4_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT4_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(AUXINPUT4_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF

! AUXINPUT5_ INTERVAL
! auxinput5_interval is left there (and means minutes) for consistency, but
! auxinput5_interval_m will take precedence if specified
   CALL nl_get_auxinput5_interval( grid%id, auxinput5_interval )   ! same as minutes
   CALL nl_get_auxinput5_interval_mo( grid%id, auxinput5_interval_mo )
   CALL nl_get_auxinput5_interval_d( grid%id, auxinput5_interval_d )
   CALL nl_get_auxinput5_interval_h( grid%id, auxinput5_interval_h )
   CALL nl_get_auxinput5_interval_m( grid%id, auxinput5_interval_m )
   CALL nl_get_auxinput5_interval_s( grid%id, auxinput5_interval_s )
   IF ( auxinput5_interval_m .EQ. 0 ) auxinput5_interval_m = auxinput5_interval

   IF ( MAX( auxinput5_interval_mo, auxinput5_interval_d,   &
             auxinput5_interval_h, auxinput5_interval_m , auxinput5_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxinput5_interval_mo, D=auxinput5_interval_d, &
                                        H=auxinput5_interval_h, M=auxinput5_interval_m, S=auxinput5_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput5_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxinput5_begin_y( grid%id, auxinput5_begin_y )
   CALL nl_get_auxinput5_begin_mo( grid%id, auxinput5_begin_mo )
   CALL nl_get_auxinput5_begin_d( grid%id, auxinput5_begin_d )
   CALL nl_get_auxinput5_begin_h( grid%id, auxinput5_begin_h )
   CALL nl_get_auxinput5_begin_m( grid%id, auxinput5_begin_m )
   CALL nl_get_auxinput5_begin_s( grid%id, auxinput5_begin_s )
   IF ( MAX( auxinput5_begin_y, auxinput5_begin_mo, auxinput5_begin_d,   &
             auxinput5_begin_h, auxinput5_begin_m , auxinput5_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxinput5_begin_mo, D=auxinput5_begin_d, &
                                      H=auxinput5_begin_h, M=auxinput5_begin_m, S=auxinput5_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput5_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxinput5_end_y( grid%id, auxinput5_end_y )
   CALL nl_get_auxinput5_end_mo( grid%id, auxinput5_end_mo )
   CALL nl_get_auxinput5_end_d( grid%id, auxinput5_end_d )
   CALL nl_get_auxinput5_end_h( grid%id, auxinput5_end_h )
   CALL nl_get_auxinput5_end_m( grid%id, auxinput5_end_m )
   CALL nl_get_auxinput5_end_s( grid%id, auxinput5_end_s )
   IF ( MAX( auxinput5_end_y, auxinput5_end_mo, auxinput5_end_d,   &
             auxinput5_end_h, auxinput5_end_m , auxinput5_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxinput5_end_mo, D=auxinput5_end_d, &
                                     H=auxinput5_end_h, M=auxinput5_end_m, S=auxinput5_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput5_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXINPUT5_ALARM, interval, begin_time, end_time )

!TBH:  Should be OK to remove the "#else" section and the code it contains 
!TBH:  because later code overwrites grid%alarms( AUXINPUT5_ALARM )...  
!TBH:  In fact, by setting namelist values for auxinput5 correctly, it ought 
!TBH:  to be possible to get rid of all "#ifdef WRF_CHEM" bits in this file...  
#ifndef WRF_CHEM
   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT5_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(AUXINPUT5_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
#else
   CALL WRFU_AlarmEnable( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
   CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
#endif


   CALL domain_alarm_create( grid, BOUNDARY_ALARM )

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

#ifdef WRF_CHEM
! TBH:  NOTE:  Proper setting of namelist variables for auxinput5 ought to 
! TBH:         make this hard-coded bit unnecessary.  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! add for wrf_chem emiss input
   CALL WRFU_AlarmEnable( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
   CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
! end for wrf chem emiss input
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#endif

! AUXINPUT6_ INTERVAL
! auxinput6_interval is left there (and means minutes) for consistency, but
! auxinput6_interval_m will take precedence if specified
   CALL nl_get_auxinput6_interval( grid%id, auxinput6_interval )   ! same as minutes
   CALL nl_get_auxinput6_interval_mo( grid%id, auxinput6_interval_mo )
   CALL nl_get_auxinput6_interval_d( grid%id, auxinput6_interval_d )
   CALL nl_get_auxinput6_interval_h( grid%id, auxinput6_interval_h )
   CALL nl_get_auxinput6_interval_m( grid%id, auxinput6_interval_m )
   CALL nl_get_auxinput6_interval_s( grid%id, auxinput6_interval_s )
   IF ( auxinput6_interval_m .EQ. 0 ) auxinput6_interval_m = auxinput6_interval

   IF ( MAX( auxinput6_interval_mo, auxinput6_interval_d,   &
             auxinput6_interval_h, auxinput6_interval_m , auxinput6_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxinput6_interval_mo, D=auxinput6_interval_d, &
                                        H=auxinput6_interval_h, M=auxinput6_interval_m, S=auxinput6_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput6_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxinput6_begin_y( grid%id, auxinput6_begin_y )
   CALL nl_get_auxinput6_begin_mo( grid%id, auxinput6_begin_mo )
   CALL nl_get_auxinput6_begin_d( grid%id, auxinput6_begin_d )
   CALL nl_get_auxinput6_begin_h( grid%id, auxinput6_begin_h )
   CALL nl_get_auxinput6_begin_m( grid%id, auxinput6_begin_m )
   CALL nl_get_auxinput6_begin_s( grid%id, auxinput6_begin_s )
   IF ( MAX( auxinput6_begin_y, auxinput6_begin_mo, auxinput6_begin_d,   &
             auxinput6_begin_h, auxinput6_begin_m , auxinput6_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxinput6_begin_mo, D=auxinput6_begin_d, &
                                      H=auxinput6_begin_h, M=auxinput6_begin_m, S=auxinput6_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput6_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxinput6_end_y( grid%id, auxinput6_end_y )
   CALL nl_get_auxinput6_end_mo( grid%id, auxinput6_end_mo )
   CALL nl_get_auxinput6_end_d( grid%id, auxinput6_end_d )
   CALL nl_get_auxinput6_end_h( grid%id, auxinput6_end_h )
   CALL nl_get_auxinput6_end_m( grid%id, auxinput6_end_m )
   CALL nl_get_auxinput6_end_s( grid%id, auxinput6_end_s )
   IF ( MAX( auxinput6_end_y, auxinput6_end_mo, auxinput6_end_d,   &
             auxinput6_end_h, auxinput6_end_m , auxinput6_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxinput6_end_mo, D=auxinput6_end_d, &
                                     H=auxinput6_end_h, M=auxinput6_end_m, S=auxinput6_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput6_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXINPUT6_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT6_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(AUXINPUT6_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF


! AUXINPUT7_ INTERVAL
! auxinput7_interval is left there (and means minutes) for consistency, but
! auxinput7_interval_m will take precedence if specified
   CALL nl_get_auxinput7_interval( grid%id, auxinput7_interval )   ! same as minutes
   CALL nl_get_auxinput7_interval_mo( grid%id, auxinput7_interval_mo )
   CALL nl_get_auxinput7_interval_d( grid%id, auxinput7_interval_d )
   CALL nl_get_auxinput7_interval_h( grid%id, auxinput7_interval_h )
   CALL nl_get_auxinput7_interval_m( grid%id, auxinput7_interval_m )
   CALL nl_get_auxinput7_interval_s( grid%id, auxinput7_interval_s )
   IF ( auxinput7_interval_m .EQ. 0 ) auxinput7_interval_m = auxinput7_interval

   IF ( MAX( auxinput7_interval_mo, auxinput7_interval_d,   &
             auxinput7_interval_h, auxinput7_interval_m , auxinput7_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxinput7_interval_mo, D=auxinput7_interval_d, &
                                        H=auxinput7_interval_h, M=auxinput7_interval_m, S=auxinput7_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput7_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxinput7_begin_y( grid%id, auxinput7_begin_y )
   CALL nl_get_auxinput7_begin_mo( grid%id, auxinput7_begin_mo )
   CALL nl_get_auxinput7_begin_d( grid%id, auxinput7_begin_d )
   CALL nl_get_auxinput7_begin_h( grid%id, auxinput7_begin_h )
   CALL nl_get_auxinput7_begin_m( grid%id, auxinput7_begin_m )
   CALL nl_get_auxinput7_begin_s( grid%id, auxinput7_begin_s )
   IF ( MAX( auxinput7_begin_y, auxinput7_begin_mo, auxinput7_begin_d,   &
             auxinput7_begin_h, auxinput7_begin_m , auxinput7_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxinput7_begin_mo, D=auxinput7_begin_d, &
                                      H=auxinput7_begin_h, M=auxinput7_begin_m, S=auxinput7_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput7_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxinput7_end_y( grid%id, auxinput7_end_y )
   CALL nl_get_auxinput7_end_mo( grid%id, auxinput7_end_mo )
   CALL nl_get_auxinput7_end_d( grid%id, auxinput7_end_d )
   CALL nl_get_auxinput7_end_h( grid%id, auxinput7_end_h )
   CALL nl_get_auxinput7_end_m( grid%id, auxinput7_end_m )
   CALL nl_get_auxinput7_end_s( grid%id, auxinput7_end_s )
   IF ( MAX( auxinput7_end_y, auxinput7_end_mo, auxinput7_end_d,   &
             auxinput7_end_h, auxinput7_end_m , auxinput7_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxinput7_end_mo, D=auxinput7_end_d, &
                                     H=auxinput7_end_h, M=auxinput7_end_m, S=auxinput7_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput7_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXINPUT7_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT7_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(AUXINPUT7_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF



! AUXINPUT8_ INTERVAL
! auxinput8_interval is left there (and means minutes) for consistency, but
! auxinput8_interval_m will take precedence if specified
   CALL nl_get_auxinput8_interval( grid%id, auxinput8_interval )   ! same as minutes
   CALL nl_get_auxinput8_interval_mo( grid%id, auxinput8_interval_mo )
   CALL nl_get_auxinput8_interval_d( grid%id, auxinput8_interval_d )
   CALL nl_get_auxinput8_interval_h( grid%id, auxinput8_interval_h )
   CALL nl_get_auxinput8_interval_m( grid%id, auxinput8_interval_m )
   CALL nl_get_auxinput8_interval_s( grid%id, auxinput8_interval_s )
   IF ( auxinput8_interval_m .EQ. 0 ) auxinput8_interval_m = auxinput8_interval

   IF ( MAX( auxinput8_interval_mo, auxinput8_interval_d,   &
             auxinput8_interval_h, auxinput8_interval_m , auxinput8_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxinput8_interval_mo, D=auxinput8_interval_d, &
                                        H=auxinput8_interval_h, M=auxinput8_interval_m, S=auxinput8_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput8_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxinput8_begin_y( grid%id, auxinput8_begin_y )
   CALL nl_get_auxinput8_begin_mo( grid%id, auxinput8_begin_mo )
   CALL nl_get_auxinput8_begin_d( grid%id, auxinput8_begin_d )
   CALL nl_get_auxinput8_begin_h( grid%id, auxinput8_begin_h )
   CALL nl_get_auxinput8_begin_m( grid%id, auxinput8_begin_m )
   CALL nl_get_auxinput8_begin_s( grid%id, auxinput8_begin_s )
   IF ( MAX( auxinput8_begin_y, auxinput8_begin_mo, auxinput8_begin_d,   &
             auxinput8_begin_h, auxinput8_begin_m , auxinput8_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxinput8_begin_mo, D=auxinput8_begin_d, &
                                      H=auxinput8_begin_h, M=auxinput8_begin_m, S=auxinput8_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput8_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxinput8_end_y( grid%id, auxinput8_end_y )
   CALL nl_get_auxinput8_end_mo( grid%id, auxinput8_end_mo )
   CALL nl_get_auxinput8_end_d( grid%id, auxinput8_end_d )
   CALL nl_get_auxinput8_end_h( grid%id, auxinput8_end_h )
   CALL nl_get_auxinput8_end_m( grid%id, auxinput8_end_m )
   CALL nl_get_auxinput8_end_s( grid%id, auxinput8_end_s )
   IF ( MAX( auxinput8_end_y, auxinput8_end_mo, auxinput8_end_d,   &
             auxinput8_end_h, auxinput8_end_m , auxinput8_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxinput8_end_mo, D=auxinput8_end_d, &
                                     H=auxinput8_end_h, M=auxinput8_end_m, S=auxinput8_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput8_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXINPUT8_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT8_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(AUXINPUT8_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF

! AUXINPUT9_ INTERVAL
! auxinput9_interval is left there (and means minutes) for consistency, but
! auxinput9_interval_m will take precedence if specified
   CALL nl_get_auxinput9_interval( grid%id, auxinput9_interval )   ! same as minutes
   CALL nl_get_auxinput9_interval_mo( grid%id, auxinput9_interval_mo )
   CALL nl_get_auxinput9_interval_d( grid%id, auxinput9_interval_d )
   CALL nl_get_auxinput9_interval_h( grid%id, auxinput9_interval_h )
   CALL nl_get_auxinput9_interval_m( grid%id, auxinput9_interval_m )
   CALL nl_get_auxinput9_interval_s( grid%id, auxinput9_interval_s )
   IF ( auxinput9_interval_m .EQ. 0 ) auxinput9_interval_m = auxinput9_interval

   IF ( MAX( auxinput9_interval_mo, auxinput9_interval_d,   &
             auxinput9_interval_h, auxinput9_interval_m , auxinput9_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxinput9_interval_mo, D=auxinput9_interval_d, &
                                        H=auxinput9_interval_h, M=auxinput9_interval_m, S=auxinput9_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput9_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxinput9_begin_y( grid%id, auxinput9_begin_y )
   CALL nl_get_auxinput9_begin_mo( grid%id, auxinput9_begin_mo )
   CALL nl_get_auxinput9_begin_d( grid%id, auxinput9_begin_d )
   CALL nl_get_auxinput9_begin_h( grid%id, auxinput9_begin_h )
   CALL nl_get_auxinput9_begin_m( grid%id, auxinput9_begin_m )
   CALL nl_get_auxinput9_begin_s( grid%id, auxinput9_begin_s )
   IF ( MAX( auxinput9_begin_y, auxinput9_begin_mo, auxinput9_begin_d,   &
             auxinput9_begin_h, auxinput9_begin_m , auxinput9_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxinput9_begin_mo, D=auxinput9_begin_d, &
                                      H=auxinput9_begin_h, M=auxinput9_begin_m, S=auxinput9_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput9_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxinput9_end_y( grid%id, auxinput9_end_y )
   CALL nl_get_auxinput9_end_mo( grid%id, auxinput9_end_mo )
   CALL nl_get_auxinput9_end_d( grid%id, auxinput9_end_d )
   CALL nl_get_auxinput9_end_h( grid%id, auxinput9_end_h )
   CALL nl_get_auxinput9_end_m( grid%id, auxinput9_end_m )
   CALL nl_get_auxinput9_end_s( grid%id, auxinput9_end_s )
   IF ( MAX( auxinput9_end_y, auxinput9_end_mo, auxinput9_end_d,   &
             auxinput9_end_h, auxinput9_end_m , auxinput9_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxinput9_end_mo, D=auxinput9_end_d, &
                                     H=auxinput9_end_h, M=auxinput9_end_m, S=auxinput9_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput9_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXINPUT9_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT9_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(AUXINPUT9_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF

#if (EM_CORE == 1)
  CALL nl_get_grid_fdda( grid%id, grid_fdda )
#endif

! AUXINPUT10_ INTERVAL (GFDDA)
! gfdda_interval is left there (and means minutes) for consistency, but
! gfdda_interval_m will take precedence if specified
   CALL nl_get_gfdda_interval( grid%id, gfdda_interval )   ! same as minutes
   CALL nl_get_gfdda_interval_mo( grid%id, gfdda_interval_mo )
   CALL nl_get_gfdda_interval_d( grid%id, gfdda_interval_d )
   CALL nl_get_gfdda_interval_h( grid%id, gfdda_interval_h )
   CALL nl_get_gfdda_interval_m( grid%id, gfdda_interval_m )
   CALL nl_get_gfdda_interval_s( grid%id, gfdda_interval_s )
   IF ( gfdda_interval_m .EQ. 0 ) gfdda_interval_m = gfdda_interval

   IF ( MAX( gfdda_interval_mo, gfdda_interval_d,   &
             gfdda_interval_h, gfdda_interval_m , gfdda_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=gfdda_interval_mo, D=gfdda_interval_d, &
                                        H=gfdda_interval_h, M=gfdda_interval_m, S=gfdda_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(gfdda_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF
#if (EM_CORE == 1)
   IF( grid_fdda == 0 ) interval = run_length + padding_interval
#endif

   CALL nl_get_gfdda_begin_y( grid%id, gfdda_begin_y )
   CALL nl_get_gfdda_begin_mo( grid%id, gfdda_begin_mo )
   CALL nl_get_gfdda_begin_d( grid%id, gfdda_begin_d )
   CALL nl_get_gfdda_begin_h( grid%id, gfdda_begin_h )
   CALL nl_get_gfdda_begin_m( grid%id, gfdda_begin_m )
   CALL nl_get_gfdda_begin_s( grid%id, gfdda_begin_s )
   IF ( MAX( gfdda_begin_y, gfdda_begin_mo, gfdda_begin_d,   &
             gfdda_begin_h, gfdda_begin_m , gfdda_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=gfdda_begin_mo, D=gfdda_begin_d, &
                                      H=gfdda_begin_h, M=gfdda_begin_m, S=gfdda_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(gfdda_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_gfdda_end_y( grid%id, gfdda_end_y )
   CALL nl_get_gfdda_end_mo( grid%id, gfdda_end_mo )
   CALL nl_get_gfdda_end_d( grid%id, gfdda_end_d )
   CALL nl_get_gfdda_end_h( grid%id, gfdda_end_h )
#if (EM_CORE == 1)
   IF( grid_fdda == 1 ) gfdda_end_h = gfdda_end_h - NINT( gfdda_interval_m/60.0 )
#endif
   CALL nl_get_gfdda_end_m( grid%id, gfdda_end_m )
   CALL nl_get_gfdda_end_s( grid%id, gfdda_end_s )
   IF ( MAX( gfdda_end_y, gfdda_end_mo, gfdda_end_d,   &
             gfdda_end_h, gfdda_end_m , gfdda_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=gfdda_end_mo, D=gfdda_end_d, &
                                     H=gfdda_end_h, M=gfdda_end_m, S=gfdda_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(gfdda_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
#if (EM_CORE == 1)
   IF( grid_fdda == 0 ) end_time = run_length + padding_interval
#endif

   CALL domain_alarm_create( grid, AUXINPUT10_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT10_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(AUXINPUT10_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF

! AUXINPUT11_ INTERVAL
! auxinput11_interval is left there (and means minutes) for consistency, but
! auxinput11_interval_m will take precedence if specified
   CALL nl_get_auxinput11_interval( grid%id, auxinput11_interval )   ! same as minutes
   CALL nl_get_auxinput11_interval_mo( grid%id, auxinput11_interval_mo )
   CALL nl_get_auxinput11_interval_d( grid%id, auxinput11_interval_d )
   CALL nl_get_auxinput11_interval_h( grid%id, auxinput11_interval_h )
   CALL nl_get_auxinput11_interval_m( grid%id, auxinput11_interval_m )
   CALL nl_get_auxinput11_interval_s( grid%id, auxinput11_interval_s )
   IF ( auxinput11_interval_m .EQ. 0 ) auxinput11_interval_m = auxinput11_interval

   IF ( MAX( auxinput11_interval_mo, auxinput11_interval_d,   &
             auxinput11_interval_h, auxinput11_interval_m , auxinput11_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, MM=auxinput11_interval_mo, D=auxinput11_interval_d, &
                                        H=auxinput11_interval_h, M=auxinput11_interval_m, S=auxinput11_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput11_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_auxinput11_begin_y( grid%id, auxinput11_begin_y )
   CALL nl_get_auxinput11_begin_mo( grid%id, auxinput11_begin_mo )
   CALL nl_get_auxinput11_begin_d( grid%id, auxinput11_begin_d )
   CALL nl_get_auxinput11_begin_h( grid%id, auxinput11_begin_h )
   CALL nl_get_auxinput11_begin_m( grid%id, auxinput11_begin_m )
   CALL nl_get_auxinput11_begin_s( grid%id, auxinput11_begin_s )
   IF ( MAX( auxinput11_begin_y, auxinput11_begin_mo, auxinput11_begin_d,   &
             auxinput11_begin_h, auxinput11_begin_m , auxinput11_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , MM=auxinput11_begin_mo, D=auxinput11_begin_d, &
                                      H=auxinput11_begin_h, M=auxinput11_begin_m, S=auxinput11_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput11_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_auxinput11_end_y( grid%id, auxinput11_end_y )
   CALL nl_get_auxinput11_end_mo( grid%id, auxinput11_end_mo )
   CALL nl_get_auxinput11_end_d( grid%id, auxinput11_end_d )
   CALL nl_get_auxinput11_end_h( grid%id, auxinput11_end_h )
   CALL nl_get_auxinput11_end_m( grid%id, auxinput11_end_m )
   CALL nl_get_auxinput11_end_s( grid%id, auxinput11_end_s )
   IF ( MAX( auxinput11_end_y, auxinput11_end_mo, auxinput11_end_d,   &
             auxinput11_end_h, auxinput11_end_m , auxinput11_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , MM=auxinput11_end_mo, D=auxinput11_end_d, &
                                     H=auxinput11_end_h, M=auxinput11_end_m, S=auxinput11_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput11_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, AUXINPUT11_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( AUXINPUT11_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(AUXINPUT11_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF

#ifdef MOVE_NESTS
! This is the interval at which the code in time_for_move in share/mediation_integrate.F
! will recompute the center of the Vortex.  Other times, it will use the last position.
!
   CALL nl_get_vortex_interval ( grid%id , vortex_interval ) 
   CALL WRFU_TimeIntervalSet( interval, M=vortex_interval, rc=rc )
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(interval) for computing vortex center FAILED', &
                           __FILE__ , &
                           __LINE__  )
   CALL domain_alarm_create( grid,  COMPUTE_VORTEX_CENTER_ALARM, interval  )
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


