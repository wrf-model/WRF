  program ttest
  USE module_utility
IMPLICIT NONE
  INTEGER rc, e, f 
  TYPE (WRFU_Time)         :: start, stop, current
  TYPE (WRFU_Calendar)     :: gregorianCalendar
  TYPE (WRFU_TimeInterval) :: timestep, ri1, ri2, run_length
  TYPE (WRFU_Alarm), pointer       :: alarms(:)
  TYPE (WRFU_Clock)        :: clock
  TYPE(WRFU_VM)       :: vm
  CHARACTER (len=WRFU_MAXSTR)  :: str,s2
  INTEGER ms
  INTEGER :: year, month, day, hour, minute, second

!  CALL WRFU_Initialize( vm=vm, defaultCalendar=WRFU_CAL_GREGORIAN, rc=rc )

  print *,'start == YY=2002, MM=12, DD=27 , H=3, Sn=3, Sd=6'
  print *,'stop  == YY=2002, MM=12, DD=27 , H=3, Sn=2, Sd=5'

  CALL WRFU_TimeSet( start, YY=2002, MM=12, DD=27 , H=3, Sn=3, Sd=6, rc=rc )
  CALL WRFU_TimeSet( stop,  YY=2002, MM=12, DD=27 , H=3, Sn=2, Sd=5, rc=rc )

  CALL WRFU_TimeGet( start, YY=year, MM=month, DD=day, H=hour, M=minute, S=second, rc=rc )
  print*,'year, month, day, hour, minute, second = ', &
          year, month, day, hour, minute, second
  CALL WRFU_TimeGet( start, YY=year, MM=month, DD=day, S=second, rc=rc )
  print*,'year, month, day, second = ', &
          year, month, day, second

  print*,'eq',(start .eq. stop)
  print*,'lt',(start .lt. stop)
  print*,'le',(start .le. stop)
  print*,'gt',(start .gt. stop)
  print*,'ge',(start .ge. stop)

  print *, 'start == YY=2000, MM=03, DD=02, H=09,         Sn=0, Sd=6'
  print *, 'stop  == YY=2000, MM=03, DD=02, H=12, S=1800, Sn=0, Sd=5'

  CALL WRFU_TimeSet( start, YY=2000, MM=03, DD=02 , H=09, Sn=0, Sd=6, &
                     cal=gregorianCalendar, rc=rc )
  CALL WRFU_TimeSet( stop, YY=2000, MM=03, DD=02 , H=12, S=1800,Sn=0, &
                     Sd=5, cal=gregorianCalendar, rc=rc )

  run_length = stop - start

  year = -1
  month = -1
  day = -1
  second = -1
  CALL WRFU_TimeIntervalGet( run_length, YY=year, MO=month, D=day, &
                             S=second, rc=rc )
  print *, 'run_length = stop - start'
  write(6,*)'run_length YY = ',year
  write(6,*)'run_length MO = ',month
  write(6,*)'run_length  D = ',day
  write(6,*)'run_length  S = ',second
  write(6,*)'rc = ',rc

!  CALL WRFU_Finalize( rc=rc )

  STOP

  END
  

! OLD tests...  
!  CALL WRFU_CalendarSet(gregorianCalendar, WRFU_CAL_GREGORIAN, rc)
!  PRINT*,rc,WRFU_SUCCESS
!  CALL WRFU_TimeSet( start, YY=2002, MM=12, DD=31 , H=23, S=3599, Sn=7, Sd=3, cal=gregorianCalendar, rc=rc )
!  CALL WRFU_TimeGetString( start, str, rc ) 
!  s2 = str(1:10)//"_"//str(12:24)
!  PRINT*,TRIM(s2)
!  STOP


! More OLD tests...  
!
!  CALL WRFU_TimeIntervalSet(timestep, S=333, Sn=1, Sd=3, rc=rc)
!  timestep = timestep/3
!  CALL print_a_time(timestep)
!  STOP
!
!  CALL WRFU_TimeIntervalSet(ri1, H=1, rc=rc)
!  CALL WRFU_TimeIntervalSet(ri2, M=30, rc=rc)
!
!  CALL WRFU_ClockSet( clock, TimeStep=timestep, StartTime=start, StopTime=stop, rc=rc )
!  CALL WRFU_ClockGetAlarmList( clock, alarms, rc )
!  CALL WRFU_AlarmSet( alarms(1), RingInterval=ri1, rc=rc )
!  CALL WRFU_AlarmSet( alarms(2), RingInterval=ri2, rc=rc )
!  CALL WRFU_ClockAddAlarm( clock, alarms(1), rc=rc )
!  CALL WRFU_ClockAddAlarm( clock, alarms(2), rc=rc )
!
!  current = start
!
!  DO WHILE ( .NOT. WRFU_ClockIsStopTime(clock,rc) )
!    CALL WRFU_ClockGetAlarmList( clock, alarms, rc )
!    IF ( WRFU_AlarmIsRinging(alarms(1),rc )) THEN
!      write(0,*)' alarm 1 --------------------------------------'
!      CALL WRFU_AlarmRingerOff(alarms(1),rc )
!    ENDIF
!    IF ( WRFU_AlarmIsRinging(alarms(2),rc )) THEN
!      write(0,*)' alarm 2 --------------------------------------'
!      CALL WRFU_AlarmRingerOff(alarms(2),rc )
!    ENDIF
!    CALL WRFU_TimeGetString( clock%CurrTime, str, rc ) 
!    CALL WRFU_TimeGet(clock%CurrTime,MS=ms) 
!    s2 = str(1:10)//"_"//str(12:19)//".0000"
!    write(s2(20:24),'("."I4.4)')ms*10
!    PRINT*,TRIM(s2)
!    current = current + timestep
!    CALL WRFU_ClockAdvance( clock, rc=rc ) 
!  ENDDO
!
!print*, 'reverse'
!  current = stop
!
!  DO WHILE ( current >= start ) 
!    CALL WRFU_TimeGetString( current, str, rc ) 
!    CALL WRFU_TimeGet(current,MS=ms) 
!    s2 = str(1:10)//"_"//str(12:19)//".0000"
!    write(s2(20:24),'("."I4.4)')ms*10
!    PRINT*,TRIM(s2)
!    current = current - timestep
!  ENDDO

