define( ALARM,
`! $1$2_ INTERVAL
! $1$2_interval is left there (and means minutes) for consistency, but
! $1$2_interval_m will take precedence if specified
   CALL nl_get_$1$2_interval( grid%id, $1$2_interval )   ! same as minutes
   CALL nl_get_$1$2_interval_d( grid%id, $1$2_interval_d )
   CALL nl_get_$1$2_interval_h( grid%id, $1$2_interval_h )
   CALL nl_get_$1$2_interval_m( grid%id, $1$2_interval_m )
   CALL nl_get_$1$2_interval_s( grid%id, $1$2_interval_s )
   IF ( $1$2_interval_m .EQ. 0 ) $1$2_interval_m = $1$2_interval

   IF ( MAX( $1$2_interval_d,   &
             $1$2_interval_h, $1$2_interval_m , $1$2_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=$1$2_interval_d, &
                                        H=$1$2_interval_h, M=$1$2_interval_m, S=$1$2_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           "WRFU_TimeIntervalSet($1$2_interval) FAILED", &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval = run_length + padding_interval
   ENDIF

   CALL nl_get_$1$2_begin_y( grid%id, $1$2_begin_y )
   CALL nl_get_$1$2_begin_d( grid%id, $1$2_begin_d )
   CALL nl_get_$1$2_begin_h( grid%id, $1$2_begin_h )
   CALL nl_get_$1$2_begin_m( grid%id, $1$2_begin_m )
   CALL nl_get_$1$2_begin_s( grid%id, $1$2_begin_s )
   IF ( MAX( $1$2_begin_y, $1$2_begin_d,   &
             $1$2_begin_h, $1$2_begin_m , $1$2_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=$1$2_begin_d, &
                                      H=$1$2_begin_h, M=$1$2_begin_m, S=$1$2_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            "WRFU_TimeIntervalSet($1$2_begin) FAILED", &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF

   CALL nl_get_$1$2_end_y( grid%id, $1$2_end_y )
   CALL nl_get_$1$2_end_d( grid%id, $1$2_end_d )
   CALL nl_get_$1$2_end_h( grid%id, $1$2_end_h )
   CALL nl_get_$1$2_end_m( grid%id, $1$2_end_m )
   CALL nl_get_$1$2_end_s( grid%id, $1$2_end_s )
   IF ( MAX( $1$2_end_y, $1$2_end_d,   &
             $1$2_end_h, $1$2_end_m , $1$2_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=$1$2_end_d, &
                                     H=$1$2_end_h, M=$1$2_end_m, S=$1$2_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            "WRFU_TimeIntervalSet($1$2_end) FAILED", &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF

   CALL domain_alarm_create( grid, $1$2_ALARM, interval, begin_time, end_time )

   IF ( interval .NE. run_length + padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( $1$2_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           "WRFU_AlarmRingerOn($1$2_ALARM) FAILED", &
                           __FILE__ , &
                           __LINE__  )
   ENDIF'
)

ALARM(history,)

ALARM(auxinput,1)
ALARM(auxinput,2)
ALARM(auxinput,3)
ALARM(auxinput,4)
#ifndef WRF_CHEM
ALARM(auxinput,5)
#endif
ALARM(auxinput,6)
ALARM(auxinput,7)
ALARM(auxinput,8)
ALARM(auxinput,9)
ALARM(auxinput,10)
ALARM(auxinput,11)

ALARM(auxhist,1)
ALARM(auxhist,2)
ALARM(auxhist,3)
ALARM(auxhist,4)
ALARM(auxhist,5)
ALARM(auxhist,6)
ALARM(auxhist,7)
ALARM(auxhist,8)
ALARM(auxhist,9)
ALARM(auxhist,10)
ALARM(auxhist,11)


