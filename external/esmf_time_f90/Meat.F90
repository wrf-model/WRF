Module Meat
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
  INTEGER, DIMENSION(12),  PARAMETER :: mday        = (/31,28,31,30,31,30,31,31,30,31,30,31/)
  INTEGER, DIMENSION(12),  PARAMETER :: mdayleap    = (/31,29,31,30,31,30,31,31,30,31,30,31/)
  INTEGER, DIMENSION(0:12)             :: mdaycum
  INTEGER, DIMENSION(0:12)             :: mdayleapcum
  INTEGER, DIMENSION(365) :: daym     
  INTEGER, DIMENSION(366) :: daymleap 
END MODULE Meat


SUBROUTINE normalize_time( time )
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
  USE meat
IMPLICIT NONE
      INTEGER nfeb
      type(ESMF_Time), intent(INOUT) :: time
      integer DD_prev

      ! handle any rollovers or rollunders of whole seconds

      DD_prev = time%DD

      IF      ( time%basetime%S .GE. 3600*24 ) THEN
        time%DD = time%DD + time%basetime%S/(3600*24)
        time%basetime%S = mod(time%basetime%S,(3600*24))
      ELSE IF ( time%basetime%S .LT. 0 ) THEN
        time%DD = time%DD + time%basetime%S/(3600*24) - 1
        time%basetime%S = 3600*24+mod(time%basetime%S,(3600*24))
      ENDIF

      IF      ( nfeb(time%YR) .EQ. 28 .AND. time%DD .LT. 1 ) THEN
        time%DD = time%DD + mday(mod(time%MM-1+12-1,12)+1)
        time%MM = time%MM - 1
      ELSE IF ( nfeb(time%YR) .EQ. 29 .AND. time%DD .LT. 1 ) THEN
        time%DD = time%DD + mdayleap(mod(time%MM-1+12-1,12)+1)
        time%MM = time%MM - 1
      ENDIF
      IF      ( nfeb(time%YR) .EQ. 28 .AND. time%DD .GT. mday(time%MM) ) THEN
        time%DD = time%DD - mday(time%MM)
        time%MM = time%MM + 1
      ELSE IF ( nfeb(time%YR) .EQ. 29 .AND. time%DD .GT. mdayleap(time%MM) ) THEN
        time%DD = time%DD - mdayleap(time%MM)
        time%MM = time%MM + 1
      ENDIF
      IF ( DD_prev .NE. 0 ) THEN
      IF ( time%MM .GT. 12 ) THEN
	time%YR = time%YR + 1
	time%MM = mod( time%MM-1, 12 ) + 1
      ENDIF
      IF ( time%MM .LT. 1 ) THEN
	time%YR = time%YR - 1
	time%MM = 12 + mod( time%MM-1, 12 ) + 1
      ENDIF
      ENDIF
      
END SUBROUTINE normalize_time


! added from share/module_date_time in WRF.
FUNCTION nfeb ( year ) RESULT (num_days)
      ! Compute the number of days in February for the given year
      IMPLICIT NONE
      INTEGER :: year
      INTEGER :: num_days
      num_days = 28 ! By default, February has 28 days ...
      IF (MOD(year,4).eq.0) THEN
         num_days = 29  ! But every four years, it has 29 days ...
         IF (MOD(year,100).eq.0) THEN
            num_days = 28  ! Except every 100 years, when it has 28 days ...
            IF (MOD(year,400).eq.0) THEN
               num_days = 29  ! Except every 400 years, when it has 29 days.
            END IF
         END IF
      END IF
END FUNCTION nfeb

SUBROUTINE initdaym 
USE meat
IMPLICIT NONE
      INTEGER i,j,m
      m = 1
      mdaycum(0) = 0
      DO i = 1,12
	DO j = 1,mday(i)
	  daym(m) = i
	  m = m + 1
	ENDDO
	mdaycum(i) = mdaycum(i-1) + mday(i)
      ENDDO
      m = 1
      mdayleapcum(0) = 0
      DO i = 1,12
	DO j = 1,mdayleap(i)
	  daymleap(m) = i
	  m = m + 1
	ENDDO
	mdayleapcum(i) = mdayleapcum(i-1) + mdayleap(i)
      ENDDO
END SUBROUTINE initdaym


SUBROUTINE compute_dayinyear(YR,MM,DD,dayinyear)
USE meat
IMPLICIT NONE
      INTEGER, INTENT(IN)  :: YR,MM,DD
      INTEGER, INTENT(OUT) :: dayinyear
      INTEGER i
      integer nfeb

      dayinyear = 0
      DO i = 1,MM-1
	if (i.eq.2) then
	  dayinyear = dayinyear + nfeb(YR)
	else
	  dayinyear = dayinyear + mday(i)
	endif
      ENDDO
      dayinyear = dayinyear + DD
END SUBROUTINE compute_dayinyear


SUBROUTINE timecmp(time1, time2, retval )
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
IMPLICIT NONE
      integer, intent(out) :: retval
!
! !ARGUMENTS:
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
! local
      integer :: t1, t2, lcd, d1, d2, n1, n2
      
      IF ( time1%YR .GT. time2%YR ) THEN ; retval = 1 ; RETURN ; ENDIF
      IF ( time1%YR .LT. time2%YR ) THEN ; retval = -1 ; RETURN ; ENDIF
      IF ( time1%MM .GT. time2%MM ) THEN ; retval = 1 ; RETURN ; ENDIF
      IF ( time1%MM .LT. time2%MM ) THEN ; retval = -1 ; RETURN ; ENDIF
      IF ( time1%DD .GT. time2%DD ) THEN ; retval = 1 ; RETURN ; ENDIF
      IF ( time1%DD .LT. time2%DD ) THEN ; retval = -1 ; RETURN ; ENDIF

      t1 = time1%basetime%S
      t2 = time2%basetime%S
      d1 = time1%basetime%Sd
      d2 = time2%basetime%Sd
      n1 = time1%basetime%Sn
      n2 = time2%basetime%Sn
      if ( n1 .gt. 0 .or. n2 .gt. 0 ) then
	 CALL compute_lcd( d1, d2, lcd )
	 n1 = n1 * ( lcd / d1 )
	 n2 = n2 * ( lcd / d2 )
      endif

      if ( t1 .GT. t2 ) retval = 1
      if ( t1 .LT. t2 ) retval = -1
      if ( t1 .EQ. t2 ) THEN
	IF (n1 .GT. n2) retval = 1
	IF (n1 .LT. n2) retval = -1
        IF (n1 .EQ. n2) retval = 0
      ENDIF
END SUBROUTINE timecmp

SUBROUTINE c_esmc_basetimeeq (time1, time2, ESMF_TimeEQ)
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
IMPLICIT NONE
      logical, intent(OUT) :: ESMF_TimeEQ
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
      integer res 
      CALL timecmp(time1,time2,res)
      ESMF_TimeEQ = (res .EQ. 0)
END SUBROUTINE c_esmc_basetimeeq
SUBROUTINE c_esmc_basetimege(time1, time2, ESMF_TimeEQ)
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
      logical, intent(OUT) :: ESMF_TimeEQ
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
      integer res 
      CALL timecmp(time1,time2,res)
      ESMF_TimeEQ = (res .EQ. 1 .OR. res .EQ. 0)
END SUBROUTINE c_esmc_basetimege
SUBROUTINE c_esmc_basetimegt(time1, time2, ESMF_TimeEQ)
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
IMPLICIT NONE
      logical, intent(OUT) :: ESMF_TimeEQ
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
      integer res 
      CALL timecmp(time1,time2,res)
      ESMF_TimeEQ = (res .EQ. 1)
END SUBROUTINE c_esmc_basetimegt
SUBROUTINE c_esmc_basetimele(time1, time2, ESMF_TimeEQ)
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
IMPLICIT NONE
      logical, intent(OUT) :: ESMF_TimeEQ
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
      integer res 
      CALL timecmp(time1,time2,res)
      ESMF_TimeEQ = (res .EQ. -1 .OR. res .EQ. 0)
END SUBROUTINE c_esmc_basetimele
SUBROUTINE c_esmc_basetimelt(time1, time2, ESMF_TimeEQ)
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
IMPLICIT NONE
      logical, intent(OUT) :: ESMF_TimeEQ
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
      integer res 
      CALL timecmp(time1,time2,res)
      ESMF_TimeEQ = (res .EQ. -1)
END SUBROUTINE c_esmc_basetimelt
SUBROUTINE c_esmc_basetimene(time1, time2, ESMF_TimeEQ)
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
IMPLICIT NONE
      logical, intent(OUT) :: ESMF_TimeEQ
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
      integer res 
      CALL timecmp(time1,time2,res)
      ESMF_TimeEQ = (res .NE. 0)
END SUBROUTINE c_esmc_basetimene

SUBROUTINE compute_lcd( e1, e2, lcd )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: e1, e2
      INTEGER, INTENT(OUT) :: lcd
      INTEGER, PARAMETER ::  nprimes = 9
      INTEGER, DIMENSION(nprimes), PARAMETER :: primes = (/2,3,5,7,11,13,17,19,23/)
      INTEGER i, p
      INTEGER d1, d2

      d1 = e1 ; d2 = e2
      IF ( d1 .EQ. 0 .AND. d2 .EQ. 0 ) THEN ; lcd = 1 ; RETURN ; ENDIF
      IF ( d1 .EQ. 0 ) d1 = d2 
      IF ( d2 .EQ. 0 ) d2 = d1 
      IF ( d1 .EQ. d2 ) THEN ; lcd = d1 ; RETURN ; ENDIF
      lcd = d1 * d2
      DO i = 1, nprimes
	p = primes(i)
        DO WHILE (lcd/p .NE. 0 .AND. &
		  mod(lcd/p,d1) .EQ. 0 .AND. mod(lcd/p,d2) .EQ. 0) 
	  lcd = lcd / p 
	END DO
      ENDDO
END SUBROUTINE compute_lcd

SUBROUTINE simplify( ni, di, no, do ) 
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: ni, di
    INTEGER, INTENT(OUT) :: no, do
    INTEGER, PARAMETER ::  nprimes = 9
    INTEGER, DIMENSION(nprimes), PARAMETER :: primes = (/2,3,5,7,11,13,17,19,23/)
    INTEGER pr, d, n, np
    LOGICAL keepgoing
    IF ( ni .EQ. 0 ) THEN
      do = 1
      no = 0
      RETURN
    ENDIF
    IF ( mod( di , ni ) .EQ. 0 ) THEN
      do = di / ni
      no = 1
      RETURN
    ENDIF
    d = di
    n = ni
    DO np = 1, nprimes
      pr = primes(np)
      keepgoing = .TRUE.
      DO WHILE ( keepgoing )
        keepgoing = .FALSE.
        IF ( d/pr .NE. 0 .AND. n/pr .NE. 0 .AND. MOD(d,pr) .EQ. 0 .AND. MOD(n,pr) .EQ. 0 ) THEN
          d = d / pr
          n = n / pr
          keepgoing = .TRUE.
        ENDIF
      ENDDO
    ENDDO
    do = d
    no = n
    RETURN
END SUBROUTINE simplify


! Beware:  This routine is duplicated with different argument type signatures 
!          in several places in this file!  Be sure to change all of them 
!          together.  Search for "c_esmc_basetimesum".  
SUBROUTINE c_esmc_basetimesum( time1, timeinterval, timeOut )
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
  USE meat
IMPLICIT NONE
      type(ESMF_Time), intent(in) :: time1
      TYPE (ESMF_TimeInterval), intent(in) :: timeinterval
      type(ESMF_Time), intent(out) :: timeOut
      integer diy, daysapart, iSd, iSn, tSd, tSn, lcd
      integer nfeb
      TYPE (ESMF_TimeInterval) :: tempInt

      CALL initdaym

      IF ( timeinterval%basetime%S .LT. 0 ) THEN
        tempInt = timeinterval
        tempInt%basetime%S = -tempInt%basetime%S
        CALL c_esmc_basetimedec( time1, tempInt, timeOut )
        RETURN
      ENDIF

      timeOut%basetime%S = time1%basetime%S + timeinterval%basetime%S

      iSd = timeinterval%basetime%Sd
      iSn = timeinterval%basetime%Sn
      tSd = time1%basetime%Sd
      tSn = time1%basetime%Sn
      IF ( iSd .NE. 0 ) THEN
        CALL compute_lcd( tSd , iSd , lcd )
        if ( tSd .EQ. 0 ) tSd = 1
        timeOut%basetime%Sd = lcd
        timeOut%basetime%Sn = (tSn * lcd / tSd) + (iSn * lcd / iSd)

        IF ( timeOut%basetime%Sn >= timeOut%basetime%Sd ) THEN
	  timeOut%basetime%S = timeOut%basetime%S + timeOut%basetime%Sn / timeOut%basetime%Sd
	  timeOut%basetime%Sn = mod( timeOut%basetime%Sn , timeOut%basetime%Sd )
        ENDIF
        timeOut%basetime%MS = NINT( timeOut%basetime%Sn*1.0D0 / timeOut%basetime%Sd*1.0D0  * 1000 )
      ELSE IF ( timeinterval%basetime%MS .NE. 0 ) THEN
! this is not right, does not cover all cases of some operands being in ms and 
! others in fraction
        timeOut%basetime%MS = time1%basetime%MS + timeinterval%basetime%MS
        IF ( timeOut%basetime%MS >= 1000 ) THEN
	  timeOut%basetime%S = timeOut%basetime%S + timeOut%basetime%MS / 1000
	  timeOut%basetime%MS = mod( timeOut%basetime%MS , 1000 )
	ENDIF
      ENDIF

      IF ( time1%instant ) THEN
        CALL compute_dayinyear(time1%YR,time1%MM,time1%DD,diy)
        diy = diy + timeinterval%DD
        DO WHILE ( timeOut%basetime%S .GE. 3600*24 )
          timeOut%basetime%S = timeOut%basetime%S - 3600*24
	  diy = diy + 1
        ENDDO
        IF ( nfeb(time1%YR) .NE. 29 ) THEN
	  timeOut%YR = time1%YR
	  IF ( diy .gt. 365 ) THEN
	    diy = diy - 365
	    timeOut%YR = timeOut%YR + 1
	  ENDIF
	  timeOut%MM = daym( diy )
	  timeOut%DD = diy - mdaycum(timeOut%MM-1)
        ELSE
	  timeOut%YR = time1%YR
	  IF ( diy .gt. 366 ) THEN
	    diy = diy - 366
	    timeOut%YR = timeOut%YR + 1
	  ENDIF
	  timeOut%MM = daymleap( diy )
	  timeOut%DD = diy - mdayleapcum(timeOut%MM-1)
        ENDIF
      ELSE
        timeOut%DD = time1%DD + timeinterval%DD
        timeOut%MM = time1%MM + timeinterval%MM
      ENDIF

END SUBROUTINE c_esmc_basetimesum


! This is a hideous duplication of c_esmc_basetimesum.  Refactor...  
SUBROUTINE c_esmc_basetimeplus( time1, time2, timeIntOut )
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
  USE meat
IMPLICIT NONE
      type(ESMF_Time), intent(in) :: time1
      TYPE (ESMF_Time), intent(in) :: time2
      type(ESMF_TimeInterval), intent(out) :: timeIntOut
      integer diy, daysapart, iSd, iSn, tSd, tSn, lcd
      integer nfeb
      TYPE (ESMF_Time) :: temp

      CALL initdaym

      IF ( time2%basetime%S .LT. 0 ) THEN
        temp = time2
        temp%basetime%S = -temp%basetime%S
        CALL c_esmc_basetimediff( time1, temp, timeIntOut )
        RETURN
      ENDIF

      timeIntOut%basetime%S = time1%basetime%S + time2%basetime%S

      iSd = time2%basetime%Sd
      iSn = time2%basetime%Sn
      tSd = time1%basetime%Sd
      tSn = time1%basetime%Sn
      IF ( iSd .NE. 0 ) THEN
        CALL compute_lcd( tSd , iSd , lcd )
        if ( tSd .EQ. 0 ) tSd = 1
        timeIntOut%basetime%Sd = lcd
        timeIntOut%basetime%Sn = (tSn * lcd / tSd) + (iSn * lcd / iSd)

        IF ( timeIntOut%basetime%Sn >= timeIntOut%basetime%Sd ) THEN
	  timeIntOut%basetime%S = timeIntOut%basetime%S + timeIntOut%basetime%Sn / timeIntOut%basetime%Sd
	  timeIntOut%basetime%Sn = mod( timeIntOut%basetime%Sn , timeIntOut%basetime%Sd )
        ENDIF
        timeIntOut%basetime%MS = NINT( timeIntOut%basetime%Sn*1.0D0 / timeIntOut%basetime%Sd*1.0D0  * 1000 )
      ELSE IF ( time2%basetime%MS .NE. 0 ) THEN
! this is not right, does not cover all cases of some operands being in ms and 
! others in fraction
        timeIntOut%basetime%MS = time1%basetime%MS + time2%basetime%MS
        IF ( timeIntOut%basetime%MS >= 1000 ) THEN
	  timeIntOut%basetime%S = timeIntOut%basetime%S + timeIntOut%basetime%MS / 1000
	  timeIntOut%basetime%MS = mod( timeIntOut%basetime%MS , 1000 )
	ENDIF
      ENDIF

      IF ( time1%instant ) THEN
        CALL compute_dayinyear(time1%YR,time1%MM,time1%DD,diy)
        diy = diy + time2%DD
        DO WHILE ( timeIntOut%basetime%S .GE. 3600*24 )
          timeIntOut%basetime%S = timeIntOut%basetime%S - 3600*24
	  diy = diy + 1
        ENDDO
        IF ( nfeb(time1%YR) .NE. 29 ) THEN
	  timeIntOut%YR = time1%YR
	  IF ( diy .gt. 365 ) THEN
	    diy = diy - 365
	    timeIntOut%YR = timeIntOut%YR + 1
	  ENDIF
	  timeIntOut%MM = daym( diy )
	  timeIntOut%DD = diy - mdaycum(timeIntOut%MM-1)
        ELSE
	  timeIntOut%YR = time1%YR
	  IF ( diy .gt. 366 ) THEN
	    diy = diy - 366
	    timeIntOut%YR = timeIntOut%YR + 1
	  ENDIF
	  timeIntOut%MM = daymleap( diy )
	  timeIntOut%DD = diy - mdayleapcum(timeIntOut%MM-1)
        ENDIF
      ELSE
        timeIntOut%DD = time1%DD + time2%DD
        timeIntOut%MM = time1%MM + time2%MM
      ENDIF

END SUBROUTINE c_esmc_basetimeplus


! This is a hideous duplication of c_esmc_basetimesum.  Refactor...  
SUBROUTINE c_esmc_basetimeintervalsum( timeInt1, timeInt2, timeIntOut )
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
  USE meat
IMPLICIT NONE
      type(ESMF_TimeInterval), intent(in) :: timeInt1
      TYPE (ESMF_TimeInterval), intent(in) :: timeInt2
      type(ESMF_TimeInterval), intent(out) :: timeIntOut
      integer diy, daysapart, iSd, iSn, tSd, tSn, lcd
      integer nfeb
      TYPE (ESMF_TimeInterval) :: tempInt

      CALL initdaym

      IF ( timeInt2%basetime%S .LT. 0 ) THEN
        tempInt = timeInt2
        tempInt%basetime%S = -tempInt%basetime%S
        CALL c_esmc_basetimeintervaldiff( timeInt1, tempInt, timeIntOut )
        RETURN
      ENDIF

      timeIntOut%basetime%S = timeInt1%basetime%S + timeInt2%basetime%S

      iSd = timeInt2%basetime%Sd
      iSn = timeInt2%basetime%Sn
      tSd = timeInt1%basetime%Sd
      tSn = timeInt1%basetime%Sn
      IF ( iSd .NE. 0 ) THEN
        CALL compute_lcd( tSd , iSd , lcd )
        if ( tSd .EQ. 0 ) tSd = 1
        timeIntOut%basetime%Sd = lcd
        timeIntOut%basetime%Sn = (tSn * lcd / tSd) + (iSn * lcd / iSd)

        IF ( timeIntOut%basetime%Sn >= timeIntOut%basetime%Sd ) THEN
	  timeIntOut%basetime%S = timeIntOut%basetime%S + timeIntOut%basetime%Sn / timeIntOut%basetime%Sd
	  timeIntOut%basetime%Sn = mod( timeIntOut%basetime%Sn , timeIntOut%basetime%Sd )
        ENDIF
        timeIntOut%basetime%MS = NINT( timeIntOut%basetime%Sn*1.0D0 / timeIntOut%basetime%Sd*1.0D0  * 1000 )
      ELSE IF ( timeInt2%basetime%MS .NE. 0 ) THEN
! this is not right, does not cover all cases of some operands being in ms and 
! others in fraction
        timeIntOut%basetime%MS = timeInt1%basetime%MS + timeInt2%basetime%MS
        IF ( timeIntOut%basetime%MS >= 1000 ) THEN
	  timeIntOut%basetime%S = timeIntOut%basetime%S + timeIntOut%basetime%MS / 1000
	  timeIntOut%basetime%MS = mod( timeIntOut%basetime%MS , 1000 )
	ENDIF
      ENDIF

      IF ( timeInt1%instant ) THEN
        CALL compute_dayinyear(timeInt1%YR,timeInt1%MM,timeInt1%DD,diy)
        diy = diy + timeInt2%DD
        DO WHILE ( timeIntOut%basetime%S .GE. 3600*24 )
          timeIntOut%basetime%S = timeIntOut%basetime%S - 3600*24
	  diy = diy + 1
        ENDDO
        IF ( nfeb(timeInt1%YR) .NE. 29 ) THEN
	  timeIntOut%YR = timeInt1%YR
	  IF ( diy .gt. 365 ) THEN
	    diy = diy - 365
	    timeIntOut%YR = timeIntOut%YR + 1
	  ENDIF
	  timeIntOut%MM = daym( diy )
	  timeIntOut%DD = diy - mdaycum(timeIntOut%MM-1)
        ELSE
	  timeIntOut%YR = timeInt1%YR
	  IF ( diy .gt. 366 ) THEN
	    diy = diy - 366
	    timeIntOut%YR = timeIntOut%YR + 1
	  ENDIF
	  timeIntOut%MM = daymleap( diy )
	  timeIntOut%DD = diy - mdayleapcum(timeIntOut%MM-1)
        ENDIF
      ELSE
        timeIntOut%DD = timeInt1%DD + timeInt2%DD
        timeIntOut%MM = timeInt1%MM + timeInt2%MM
      ENDIF

END SUBROUTINE c_esmc_basetimeintervalsum


! Beware:  This routine is duplicated with different argument type signatures 
!          in several places in this file!  Be sure to change all of them 
!          together.  Search for "c_esmc_basetimedec".  
SUBROUTINE c_esmc_basetimedec( time1, timeinterval, timeOut )
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
  USE meat
IMPLICIT NONE
      type(ESMF_Time), intent(in) :: time1
      TYPE (ESMF_TimeInterval), intent(in) :: timeinterval
      type(ESMF_Time), intent(out) :: timeOut
      integer diy, daysapart, iSd, iSn, tSd, tSn,lcd
      integer nfeb
      TYPE (ESMF_TimeInterval)  :: tempInt 

      CALL initdaym

      IF ( timeinterval%basetime%S .LT. 0 ) THEN
        tempInt = timeinterval
        tempInt%basetime%S = -tempInt%basetime%S
        CALL c_esmc_basetimesum( time1, tempInt, timeOut )
        RETURN
      ENDIF

      iSd = timeinterval%basetime%Sd
      iSn = timeinterval%basetime%Sn
      tSd = time1%basetime%Sd
      tSn = time1%basetime%Sn
      IF ( iSd .NE. 0 ) THEN
        CALL compute_lcd( tSd , iSd , lcd )
	if ( tSd .EQ. 0 ) tSd = 1
        timeOut%basetime%Sd = lcd
        timeOut%basetime%Sn = (tSn * lcd / tSd) - (iSn * lcd / iSd)
        IF ( timeOut%basetime%Sn < 0 ) THEN
          IF ( mod( -timeOut%basetime%Sn, timeOut%basetime%Sd) .EQ. 0 ) THEN
            timeOut%basetime%S = timeOut%basetime%S - mod( -timeOut%basetime%Sn, timeOut%basetime%Sd)
          ELSE
            timeOut%basetime%S = timeOut%basetime%S + timeOut%basetime%Sn / timeOut%basetime%Sd - 1
          ENDIF
          timeOut%basetime%Sn = timeOut%basetime%Sd + mod( timeOut%basetime%Sn , timeOut%basetime%Sd )
        ENDIF
        timeOut%basetime%MS = NINT( timeOut%basetime%Sn*1.0D0 / timeOut%basetime%Sd*1.0D0  * 1000 )
      ELSE IF ( timeinterval%basetime%MS .NE. 0 ) THEN
        timeOut%basetime%MS = time1%basetime%MS - timeinterval%basetime%MS
        IF ( timeOut%basetime%MS < 0 ) THEN
          timeOut%basetime%S = timeOut%basetime%S + timeOut%basetime%MS / 1000 - 1
          timeOut%basetime%MS = 1000 + mod( timeOut%basetime%MS , 1000 )
        ENDIF
      ENDIF

      IF ( time1%instant ) THEN
        CALL compute_dayinyear(time1%YR,time1%MM,time1%DD,diy)

        timeOut%basetime%S = time1%basetime%S - timeinterval%basetime%S

        diy = diy - timeinterval%DD
        IF ( timeOut%basetime%S .LT. 0 ) THEN
          diy = diy - 1
          timeOut%basetime%S = timeOut%basetime%S + 3600*24
        ENDIF
        IF ( nfeb(time1%YR) .NE. 29 ) THEN
          timeOut%YR = time1%YR
          IF ( diy .lt. 1 ) THEN
            diy = diy + 365
            timeOut%YR = timeOut%YR - 1
          ENDIF
          timeOut%MM = daym( diy )
          timeOut%DD = diy - mdaycum(timeOut%MM-1)
        ELSE
          timeOut%YR = time1%YR
          IF ( diy .lt. 1 ) THEN
            diy = diy + 366
            timeOut%YR = timeOut%YR - 1
          ENDIF
          timeOut%MM = daym( diy )
          timeOut%DD = diy - mdayleapcum(timeOut%MM-1)
        ENDIF
        timeOut%basetime%S = mod( timeOut%basetime%S, 3600*24 )
      ELSE
        timeOut%DD = time1%DD - timeinterval%DD
        timeOut%MM = time1%MM - timeinterval%MM
      ENDIF

END SUBROUTINE c_esmc_basetimedec


! This is a hideous duplication of c_esmc_basetimedec.  Refactor...  
SUBROUTINE c_esmc_basetimediff( time1, time2, timeIntOut )
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
  USE meat
IMPLICIT NONE
      type(ESMF_Time), intent(in) :: time1
      TYPE (ESMF_Time), intent(in) :: time2
      type(ESMF_TimeInterval), intent(out) :: timeIntOut
      integer diy, daysapart, iSd, iSn, tSd, tSn,lcd
      integer nfeb
      TYPE (ESMF_Time)  :: temp 

      CALL initdaym

      IF ( time2%basetime%S .LT. 0 ) THEN
        temp = time2
        temp%basetime%S = -temp%basetime%S
        CALL c_esmc_basetimeplus( time1, temp, timeIntOut )
        RETURN
      ENDIF

      iSd = time2%basetime%Sd
      iSn = time2%basetime%Sn
      tSd = time1%basetime%Sd
      tSn = time1%basetime%Sn
      IF ( iSd .NE. 0 ) THEN
        CALL compute_lcd( tSd , iSd , lcd )
	if ( tSd .EQ. 0 ) tSd = 1
        timeIntOut%basetime%Sd = lcd
        timeIntOut%basetime%Sn = (tSn * lcd / tSd) - (iSn * lcd / iSd)
        IF ( timeIntOut%basetime%Sn < 0 ) THEN
          IF ( mod( -timeIntOut%basetime%Sn, timeIntOut%basetime%Sd) .EQ. 0 ) THEN
            timeIntOut%basetime%S = timeIntOut%basetime%S - mod( -timeIntOut%basetime%Sn, timeIntOut%basetime%Sd)
          ELSE
            timeIntOut%basetime%S = timeIntOut%basetime%S + timeIntOut%basetime%Sn / timeIntOut%basetime%Sd - 1
          ENDIF
          timeIntOut%basetime%Sn = timeIntOut%basetime%Sd + mod( timeIntOut%basetime%Sn , timeIntOut%basetime%Sd )
        ENDIF
        timeIntOut%basetime%MS = NINT( timeIntOut%basetime%Sn*1.0D0 / timeIntOut%basetime%Sd*1.0D0  * 1000 )
      ELSE IF ( time2%basetime%MS .NE. 0 ) THEN
        timeIntOut%basetime%MS = time1%basetime%MS - time2%basetime%MS
        IF ( timeIntOut%basetime%MS < 0 ) THEN
          timeIntOut%basetime%S = timeIntOut%basetime%S + timeIntOut%basetime%MS / 1000 - 1
          timeIntOut%basetime%MS = 1000 + mod( timeIntOut%basetime%MS , 1000 )
        ENDIF
      ENDIF

      IF ( time1%instant ) THEN
        CALL compute_dayinyear(time1%YR,time1%MM,time1%DD,diy)

        timeIntOut%basetime%S = time1%basetime%S - time2%basetime%S

        diy = diy - time2%DD
        IF ( timeIntOut%basetime%S .LT. 0 ) THEN
          diy = diy - 1
          timeIntOut%basetime%S = timeIntOut%basetime%S + 3600*24
        ENDIF
        IF ( nfeb(time1%YR) .NE. 29 ) THEN
          timeIntOut%YR = time1%YR
          IF ( diy .lt. 1 ) THEN
            diy = diy + 365
            timeIntOut%YR = timeIntOut%YR - 1
          ENDIF
          timeIntOut%MM = daym( diy )
          timeIntOut%DD = diy - mdaycum(timeIntOut%MM-1)
        ELSE
          timeIntOut%YR = time1%YR
          IF ( diy .lt. 1 ) THEN
            diy = diy + 366
            timeIntOut%YR = timeIntOut%YR - 1
          ENDIF
          timeIntOut%MM = daym( diy )
          timeIntOut%DD = diy - mdayleapcum(timeIntOut%MM-1)
        ENDIF
        timeIntOut%basetime%S = mod( timeIntOut%basetime%S, 3600*24 )
      ELSE
        timeIntOut%DD = time1%DD - time2%DD
        timeIntOut%MM = time1%MM - time2%MM
      ENDIF

END SUBROUTINE c_esmc_basetimediff


! This is a hideous duplication of c_esmc_basetimedec.  Refactor...  
SUBROUTINE c_esmc_basetimeintervaldiff( timeInt1, timeInt2, timeIntOut )
  USE esmf_alarmmod
  USE esmf_basemod
  USE esmf_basetimemod
  USE esmf_calendarmod
  USE esmf_clockmod
  USE esmf_fractionmod
  USE esmf_timeintervalmod
  USE esmf_timemod
  USE meat
IMPLICIT NONE
      type(ESMF_TimeInterval), intent(in) :: timeInt1
      TYPE (ESMF_TimeInterval), intent(in) :: timeInt2
      type(ESMF_TimeInterval), intent(out) :: timeIntOut
      integer diy, daysapart, iSd, iSn, tSd, tSn,lcd
      integer nfeb
      TYPE (ESMF_TimeInterval)  :: tempInt

      CALL initdaym

      IF ( timeInt2%basetime%S .LT. 0 ) THEN
        tempInt = timeInt2
        tempInt%basetime%S = -tempInt%basetime%S
        CALL c_esmc_basetimeintervalsum( timeInt1, tempInt, timeIntOut )
        RETURN
      ENDIF

      iSd = timeInt2%basetime%Sd
      iSn = timeInt2%basetime%Sn
      tSd = timeInt1%basetime%Sd
      tSn = timeInt1%basetime%Sn
      IF ( iSd .NE. 0 ) THEN
        CALL compute_lcd( tSd , iSd , lcd )
	if ( tSd .EQ. 0 ) tSd = 1
        timeIntOut%basetime%Sd = lcd
        timeIntOut%basetime%Sn = (tSn * lcd / tSd) - (iSn * lcd / iSd)
        IF ( timeIntOut%basetime%Sn < 0 ) THEN
          IF ( mod( -timeIntOut%basetime%Sn, timeIntOut%basetime%Sd) .EQ. 0 ) THEN
            timeIntOut%basetime%S = timeIntOut%basetime%S - mod( -timeIntOut%basetime%Sn, timeIntOut%basetime%Sd)
          ELSE
            timeIntOut%basetime%S = timeIntOut%basetime%S + timeIntOut%basetime%Sn / timeIntOut%basetime%Sd - 1
          ENDIF
          timeIntOut%basetime%Sn = timeIntOut%basetime%Sd + mod( timeIntOut%basetime%Sn , timeIntOut%basetime%Sd )
        ENDIF
        timeIntOut%basetime%MS = NINT( timeIntOut%basetime%Sn*1.0D0 / timeIntOut%basetime%Sd*1.0D0  * 1000 )
      ELSE IF ( timeInt2%basetime%MS .NE. 0 ) THEN
        timeIntOut%basetime%MS = timeInt1%basetime%MS - timeInt2%basetime%MS
        IF ( timeIntOut%basetime%MS < 0 ) THEN
          timeIntOut%basetime%S = timeIntOut%basetime%S + timeIntOut%basetime%MS / 1000 - 1
          timeIntOut%basetime%MS = 1000 + mod( timeIntOut%basetime%MS , 1000 )
        ENDIF
      ENDIF

      IF ( timeInt1%instant ) THEN
        CALL compute_dayinyear(timeInt1%YR,timeInt1%MM,timeInt1%DD,diy)

        timeIntOut%basetime%S = timeInt1%basetime%S - timeInt2%basetime%S

        diy = diy - timeInt2%DD
        IF ( timeIntOut%basetime%S .LT. 0 ) THEN
          diy = diy - 1
          timeIntOut%basetime%S = timeIntOut%basetime%S + 3600*24
        ENDIF
        IF ( nfeb(timeInt1%YR) .NE. 29 ) THEN
          timeIntOut%YR = timeInt1%YR
          IF ( diy .lt. 1 ) THEN
            diy = diy + 365
            timeIntOut%YR = timeIntOut%YR - 1
          ENDIF
          timeIntOut%MM = daym( diy )
          timeIntOut%DD = diy - mdaycum(timeIntOut%MM-1)
        ELSE
          timeIntOut%YR = timeInt1%YR
          IF ( diy .lt. 1 ) THEN
            diy = diy + 366
            timeIntOut%YR = timeIntOut%YR - 1
          ENDIF
          timeIntOut%MM = daym( diy )
          timeIntOut%DD = diy - mdayleapcum(timeIntOut%MM-1)
        ENDIF
        timeIntOut%basetime%S = mod( timeIntOut%basetime%S, 3600*24 )
      ELSE
        timeIntOut%DD = timeInt1%DD - timeInt2%DD
        timeIntOut%MM = timeInt1%MM - timeInt2%MM
      ENDIF

END SUBROUTINE c_esmc_basetimeintervaldiff


SUBROUTINE c_esmc_calendarprint
END SUBROUTINE c_esmc_calendarprint
SUBROUTINE c_esmc_calendarread
END SUBROUTINE c_esmc_calendarread
SUBROUTINE c_esmc_calendarset
END SUBROUTINE c_esmc_calendarset
SUBROUTINE c_esmc_calendarsetgeneric
END SUBROUTINE c_esmc_calendarsetgeneric
SUBROUTINE c_esmc_calendarvalidate
END SUBROUTINE c_esmc_calendarvalidate
SUBROUTINE c_esmc_calendarwrite
END SUBROUTINE c_esmc_calendarwrite
SUBROUTINE c_esmc_timeget
END SUBROUTINE c_esmc_timeget
SUBROUTINE c_esmc_timegetcalendarcopy
END SUBROUTINE c_esmc_timegetcalendarcopy
SUBROUTINE c_esmc_timegetcalendarptr
END SUBROUTINE c_esmc_timegetcalendarptr
SUBROUTINE c_esmc_timegetdayofmonth
END SUBROUTINE c_esmc_timegetdayofmonth
SUBROUTINE c_esmc_timegetdayofweek
END SUBROUTINE c_esmc_timegetdayofweek
SUBROUTINE c_esmc_timegetdayofyeardouble
END SUBROUTINE c_esmc_timegetdayofyeardouble
SUBROUTINE c_esmc_timegetdayofyearinteger
END SUBROUTINE c_esmc_timegetdayofyearinteger
SUBROUTINE c_esmc_timegetdayofyeartimeint
END SUBROUTINE c_esmc_timegetdayofyeartimeint
SUBROUTINE c_esmc_timegetmidmonth
END SUBROUTINE c_esmc_timegetmidmonth
SUBROUTINE c_esmc_timegetrealtime
END SUBROUTINE c_esmc_timegetrealtime
SUBROUTINE c_esmc_timegetstring
END SUBROUTINE c_esmc_timegetstring
SUBROUTINE c_esmc_timegettimezone
END SUBROUTINE c_esmc_timegettimezone
SUBROUTINE c_esmc_timeintervalabsvalue
END SUBROUTINE c_esmc_timeintervalabsvalue
SUBROUTINE c_esmc_timeintervalfquot
END SUBROUTINE c_esmc_timeintervalfquot
SUBROUTINE c_esmc_timeintervalget
END SUBROUTINE c_esmc_timeintervalget
SUBROUTINE c_esmc_timeintervalgetstring
END SUBROUTINE c_esmc_timeintervalgetstring
SUBROUTINE c_esmc_timeintervalnegabsvalue
END SUBROUTINE c_esmc_timeintervalnegabsvalue
SUBROUTINE c_esmc_timeintervalprint
END SUBROUTINE c_esmc_timeintervalprint
SUBROUTINE c_esmc_timeintervalprodf
END SUBROUTINE c_esmc_timeintervalprodf
SUBROUTINE c_esmc_timeintervalprodi
END SUBROUTINE c_esmc_timeintervalprodi
SUBROUTINE c_esmc_timeintervalprodr
END SUBROUTINE c_esmc_timeintervalprodr
SUBROUTINE c_esmc_timeintervalquoti
END SUBROUTINE c_esmc_timeintervalquoti
SUBROUTINE c_esmc_timeintervalquotr
END SUBROUTINE c_esmc_timeintervalquotr
SUBROUTINE c_esmc_timeintervalread
END SUBROUTINE c_esmc_timeintervalread
SUBROUTINE c_esmc_timeintervalrquot
END SUBROUTINE c_esmc_timeintervalrquot
SUBROUTINE c_esmc_timeintervalset
END SUBROUTINE c_esmc_timeintervalset
SUBROUTINE c_esmc_timeintervalvalidate
END SUBROUTINE c_esmc_timeintervalvalidate
SUBROUTINE c_esmc_timeintervalwrite
END SUBROUTINE c_esmc_timeintervalwrite
SUBROUTINE c_esmc_timeissamecal
END SUBROUTINE c_esmc_timeissamecal
SUBROUTINE c_esmc_timeprint
END SUBROUTINE c_esmc_timeprint
SUBROUTINE c_esmc_timeread
END SUBROUTINE c_esmc_timeread
SUBROUTINE c_esmc_timeset
END SUBROUTINE c_esmc_timeset
SUBROUTINE c_esmc_timesetcalendarptr
END SUBROUTINE c_esmc_timesetcalendarptr
SUBROUTINE c_esmc_timesetcalendarptrptr
END SUBROUTINE c_esmc_timesetcalendarptrptr
SUBROUTINE c_esmc_timesettimezone
END SUBROUTINE c_esmc_timesettimezone
SUBROUTINE c_esmc_timevalidate
END SUBROUTINE c_esmc_timevalidate
SUBROUTINE c_esmc_timewrite
END SUBROUTINE c_esmc_timewrite

! some extra wrf stuff

SUBROUTINE print_a_time( time )
   use ESMF_basemod
   use ESMF_Timemod
   type(ESMF_Time) time
   character*128 :: s
   integer rc
   CALL ESMFold_TimeGetString( time, s, rc )
   print *,'Print a time|',TRIM(s),'|'
   write(0,*)'Print a time|',TRIM(s),'|'
   return
END SUBROUTINE print_a_time

SUBROUTINE print_a_timeinterval( time )
   use ESMF_basemod
   use ESMF_TimeIntervalmod
   type(ESMF_TimeInterval) time
   character*128 :: s
   integer rc
   CALL ESMFold_TimeIntervalGetString( time, s, rc )
   print *,'Print a time interval|',TRIM(s),'|'
   write(0,*)'Print a time interval|',TRIM(s),'|'
   return
END SUBROUTINE print_a_timeinterval

