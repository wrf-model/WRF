!WRF:MODEL_LAYER:UTIL
!
MODULE module_date_time

  USE module_wrf_error
  USE module_configure
  USE module_model_constants

  CHARACTER* 24 ::   start_date = '                        '
  CHARACTER* 24 ::   current_date
  INTEGER , PARAMETER :: len_current_date  = 24
  REAL , PRIVATE :: xtime

!  1.  geth_idts (ndate, odate, idts)
!  Get the time period between two dates.

!  2. geth_newdate ( ndate, odate, idts)
!  Get the new date based on the old date and a time difference.

!  3. split_date_char ( date , century_year , month , day , hour , minute , second , ten_thousandth)
!  Given the date, return the integer components.

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE get_julgmt(date_str,julyr,julday,gmt)
     IMPLICIT NONE
! Arguments
     CHARACTER (LEN=24) , INTENT(IN) :: date_str
     INTEGER, INTENT(OUT  ) :: julyr
     INTEGER, INTENT(OUT  ) :: julday
     REAL   , INTENT(OUT  ) :: gmt
! Local
     INTEGER :: ny , nm , nd , nh , ni , ns , nt
     INTEGER :: my1, my2, my3, monss
     INTEGER, DIMENSION(12) :: mmd
     DATA MMD/31,28,31,30,31,30,31,31,30,31,30,31/
     CALL split_date_char ( date_str , ny , nm , nd , nh , ni , ns , nt )
#ifdef PLANET
     GMT=nh+FLOAT(ni)/60.+(FLOAT(ns)+FLOAT(nt)/1.e6)/3600.
     JULDAY=nd
     JULYR=ny
#else
     GMT=nh+FLOAT(ni)/60.+FLOAT(ns)/3600.
     MY1=MOD(ny,4)
     MY2=MOD(ny,100)
     MY3=MOD(ny,400)
     IF(MY1.EQ.0.AND.MY2.NE.0.OR.MY3.EQ.0)MMD(2)=29
     JULDAY=nd
     JULYR=ny
     DO MONSS=1,nm-1
       JULDAY=JULDAY+MMD(MONSS)
     ENDDO
#endif
   END SUBROUTINE get_julgmt


   SUBROUTINE geth_julgmt(julyr,julday, gmt)
     IMPLICIT NONE
! Arguments
     INTEGER, INTENT(OUT  ) :: julyr
     INTEGER, INTENT(OUT  ) :: julday
     REAL   , INTENT(OUT  ) :: gmt
! Local
     INTEGER :: ny , nm , nd , nh , ni , ns , nt
     INTEGER :: my1, my2, my3, monss
     INTEGER, DIMENSION(12) :: mmd
     DATA MMD/31,28,31,30,31,30,31,31,30,31,30,31/
     CALL split_date_char ( current_date , ny , nm , nd , nh , ni , ns , nt )
#ifdef PLANET
     GMT=nh+FLOAT(ni)/60.+(FLOAT(ns)+FLOAT(nt)/1.e6)/3600.
     JULDAY=nd
     JULYR=ny
#else
     GMT=nh+FLOAT(ni)/60.+FLOAT(ns)/3600.
     MY1=MOD(ny,4)
     MY2=MOD(ny,100)
     MY3=MOD(ny,400)
     IF(MY1.EQ.0.AND.MY2.NE.0.OR.MY3.EQ.0)MMD(2)=29
     JULDAY=nd
     JULYR=ny
     DO MONSS=1,nm-1
       JULDAY=JULDAY+MMD(MONSS)
     ENDDO
#endif
   END SUBROUTINE geth_julgmt

   SUBROUTINE calc_current_date (id, time)
! This subroutines calculates current_date and xtime
   IMPLICIT NONE
! Arguments
   INTEGER, INTENT(IN   ) :: id ! grid id
   REAL, INTENT(IN   ) :: time ! time in seconds since start time
! Local
   INTEGER :: julyr, julday, idt
   CHARACTER*19  new_date
   CHARACTER*24  base_date
   CHARACTER*128 mess
   REAL :: gmt

    xtime = time/60.
    CALL nl_get_gmt (id, gmt)
    CALL nl_get_julyr (id, julyr)
    CALL nl_get_julday (id, julday)
    idt        = 86400*(julday-1)+nint(3600*gmt)
    write (mess,*) 'calc_current_date called: time = ',time,' idt = ',idt
    CALL wrf_debug(300,TRIM(mess))
    write (mess,*) 'calc_current_date called: gmt  = ',gmt
    CALL wrf_debug(300,TRIM(mess))
    write (mess,*) 'calc_current_date called: julyr  = ',julyr
    CALL wrf_debug(300,TRIM(mess))
    write (mess,*) 'calc_current_date called: julday = ',julday
    CALL wrf_debug(300,TRIM(mess))
#ifdef PLANET
    base_date  = '0000-00001_00:00:00.0000'
#else
    base_date  = '0000-01-01_00:00:00.0000'
#endif
    write(base_date(1:4),'(I4.4)')julyr
    CALL geth_newdate (start_date(1:19), base_date(1:19), idt)
    CALL geth_newdate (new_date, start_date(1:19), nint(time))
    write (current_date(1:24),fmt=340)new_date
    340 format(a19, '.0000')
    write (mess,*) current_date,gmt,julday,julyr,'=current_date,gmt,julday,julyr: calc_current_date'
    CALL wrf_debug(300,TRIM(mess))
   END SUBROUTINE calc_current_date

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE geth_idts (ndate, odate, idts)
   
      IMPLICIT NONE
      
      !  From 2 input mdates ('YYYY-MM-DD HH:MM:SS.ffff'), 
      !                   or ('YYYY-DDDDD HH:MM:SS.ffff'),
      !  compute the time difference.
      
      !  on entry     -  ndate  -  the new hdate.
      !                  odate  -  the old hdate.
      
      !  on exit      -  idts    -  the change in time in seconds.
      
      CHARACTER (LEN=*) , INTENT(INOUT) :: ndate, odate
      INTEGER           , INTENT(OUT)   :: idts
      
      !  Local Variables
      
      !  yrnew    -  indicates the year associated with "ndate"
      !  yrold    -  indicates the year associated with "odate"
      !  monew    -  indicates the month associated with "ndate"
      !  moold    -  indicates the month associated with "odate"
      !  dynew    -  indicates the day associated with "ndate"
      !  dyold    -  indicates the day associated with "odate"
      !  hrnew    -  indicates the hour associated with "ndate"
      !  hrold    -  indicates the hour associated with "odate"
      !  minew    -  indicates the minute associated with "ndate"
      !  miold    -  indicates the minute associated with "odate"
      !  scnew    -  indicates the second associated with "ndate"
      !  scold    -  indicates the second associated with "odate"
      !  i        -  loop counter
      !  mday     -  a list assigning the number of days in each month
      
      CHARACTER (LEN=24) :: tdate
      INTEGER :: olen, nlen
      INTEGER :: yrnew, monew, dynew, hrnew, minew, scnew
      INTEGER :: yrold, moold, dyold, hrold, miold, scold
      INTEGER :: mday(12), i, newdys, olddys
      LOGICAL :: npass, opass
      INTEGER :: isign
      
      IF (odate.GT.ndate) THEN
         isign = -1
         tdate=ndate
         ndate=odate
         odate=tdate
      ELSE
         isign = 1
      END IF
      
      !  Assign the number of days in a months
      
      mday( 1) = 31
      mday( 2) = 28
      mday( 3) = 31
      mday( 4) = 30
      mday( 5) = 31
      mday( 6) = 30
      mday( 7) = 31
      mday( 8) = 31
      mday( 9) = 30
      mday(10) = 31
      mday(11) = 30
      mday(12) = 31
      
      !  Break down old hdate into parts
      
      hrold = 0
      miold = 0
      scold = 0
      olen = LEN(odate)
      
      READ(odate(1:4),  '(I4.4)') yrold
#ifdef PLANET
      READ(odate(6:10), '(I5)') dyold
      moold=0.
#else
      READ(odate(6:7),  '(I2)') moold
      READ(odate(9:10), '(I2)') dyold
#endif
      IF (olen.GE.13) THEN
         READ(odate(12:13),'(I2)') hrold
         IF (olen.GE.16) THEN
            READ(odate(15:16),'(I2)') miold
            IF (olen.GE.19) THEN
               READ(odate(18:19),'(I2)') scold
            END IF
         END IF
      END IF
      
      !  Break down new hdate into parts
      
      hrnew = 0
      minew = 0
      scnew = 0
      nlen = LEN(ndate)
      
      READ(ndate(1:4),  '(I4.4)') yrnew
#ifdef PLANET
      READ(ndate(6:10), '(I5)') dynew
      monew=0.
#else
      READ(ndate(6:7),  '(I2)') monew
      READ(ndate(9:10), '(I2)') dynew
#endif
      IF (nlen.GE.13) THEN
         READ(ndate(12:13),'(I2)') hrnew
         IF (nlen.GE.16) THEN
            READ(ndate(15:16),'(I2)') minew
            IF (nlen.GE.19) THEN
               READ(ndate(18:19),'(I2)') scnew
            END IF
         END IF
      END IF
      
      !  Check that the dates make sense.
      
      npass = .true.
      opass = .true.
      
#ifdef PLANET
      !  Check that the day of NDATE makes sense.
      
      IF ((dynew > PLANET_YEAR).or.(dynew < 1)) THEN
         PRINT*, 'GETH_IDTS:  Day of NDATE = ', dynew
         npass = .false.
      END IF

      !  Check that the day of ODATE makes sense.

      IF ((dyold > PLANET_YEAR).or.(dyold < 1)) THEN
         PRINT*, 'GETH_IDTS:  Day of ODATE = ', dyold
         opass = .false.
      END IF

#else
      !  Check that the month of NDATE makes sense.
      
      IF ((monew.GT.12).or.(monew.LT.1)) THEN
         PRINT*, 'GETH_IDTS:  Month of NDATE = ', monew
         npass = .false.
      END IF
      
      !  Check that the month of ODATE makes sense.
      
      IF ((moold.GT.12).or.(moold.LT.1)) THEN
         PRINT*, 'GETH_IDTS:  Month of ODATE = ', moold
         opass = .false.
      END IF
      
      !  Check that the day of NDATE makes sense.
      
      IF (monew.ne.2) THEN
      ! ...... For all months but February
         IF ((dynew.GT.mday(monew)).or.(dynew.LT.1)) THEN
            PRINT*, 'GETH_IDTS:  Day of NDATE = ', dynew
            npass = .false.
         END IF
      ELSE IF (monew.eq.2) THEN
      ! ...... For February
         IF ((dynew.GT.nfeb(yrnew)).OR.(dynew.LT.1)) THEN
            PRINT*, 'GETH_IDTS:  Day of NDATE = ', dynew
            npass = .false.
         END IF
      END IF
      
      !  Check that the day of ODATE makes sense.
      
      IF (moold.ne.2) THEN
      ! ...... For all months but February
         IF ((dyold.GT.mday(moold)).or.(dyold.LT.1)) THEN
            PRINT*, 'GETH_IDTS:  Day of ODATE = ', dyold
            opass = .false.
         END IF
      ELSE IF (moold.eq.2) THEN
      ! ....... For February
         IF ((dyold.GT.nfeb(yrold)).or.(dyold.LT.1)) THEN
            PRINT*, 'GETH_IDTS:  Day of ODATE = ', dyold
            opass = .false.
         END IF
      END IF
#endif
      !  Check that the hour of NDATE makes sense.
      
      IF ((hrnew.GT.23).or.(hrnew.LT.0)) THEN
         PRINT*, 'GETH_IDTS:  Hour of NDATE = ', hrnew
         npass = .false.
      END IF
      
      !  Check that the hour of ODATE makes sense.
      
      IF ((hrold.GT.23).or.(hrold.LT.0)) THEN
         PRINT*, 'GETH_IDTS:  Hour of ODATE = ', hrold
         opass = .false.
      END IF
      
      !  Check that the minute of NDATE makes sense.
      
      IF ((minew.GT.59).or.(minew.LT.0)) THEN
         PRINT*, 'GETH_IDTS:  Minute of NDATE = ', minew
         npass = .false.
      END IF
      
      !  Check that the minute of ODATE makes sense.
      
      IF ((miold.GT.59).or.(miold.LT.0)) THEN
         PRINT*, 'GETH_IDTS:  Minute of ODATE = ', miold
         opass = .false.
      END IF
      
      !  Check that the second of NDATE makes sense.
      
      IF ((scnew.GT.59).or.(scnew.LT.0)) THEN
         PRINT*, 'GETH_IDTS:  SECOND of NDATE = ', scnew
         npass = .false.
      END IF
      
      !  Check that the second of ODATE makes sense.
      
      IF ((scold.GT.59).or.(scold.LT.0)) THEN
         PRINT*, 'GETH_IDTS:  Second of ODATE = ', scold
         opass = .false.
      END IF
      
      IF (.not. npass) THEN
         WRITE( wrf_err_message , * ) 'module_date_time: geth_idts: Bad NDATE: ', ndate(1:nlen)
         CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      END IF
      
      IF (.not. opass) THEN
         WRITE( wrf_err_message , * ) 'module_date_time: geth_idts: Bad ODATE: ', odate(1:olen)
         CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      END IF
      
      !  Date Checks are completed.  Continue.
      
      !  Compute number of days from 1 January ODATE, 00:00:00 until ndate
      !  Compute number of hours from 1 January ODATE, 00:00:00 until ndate
      !  Compute number of minutes from 1 January ODATE, 00:00:00 until ndate
      
      newdys = 0
#ifdef PLANET
      DO i = yrold, yrnew - 1
         newdys = newdys + PLANET_YEAR
      END DO
#else
      DO i = yrold, yrnew - 1
         newdys = newdys + (365 + (nfeb(i)-28))
      END DO
      
      IF (monew .GT. 1) THEN
         mday(2) = nfeb(yrnew)
         DO i = 1, monew - 1
            newdys = newdys + mday(i)
         END DO
         mday(2) = 28
      END IF
#endif
      
      newdys = newdys + dynew-1
      
      !  Compute number of hours from 1 January ODATE, 00:00:00 until odate
      !  Compute number of minutes from 1 January ODATE, 00:00:00 until odate
      
      olddys = 0
      
#ifndef PLANET
      IF (moold .GT. 1) THEN
         mday(2) = nfeb(yrold)
         DO i = 1, moold - 1
            olddys = olddys + mday(i)
         END DO
         mday(2) = 28
      END IF
#endif
      
      olddys = olddys + dyold-1
      
      !  Determine the time difference in seconds
      
      idts = (newdys - olddys) * 86400
      idts = idts + (hrnew - hrold) * 3600
      idts = idts + (minew - miold) * 60
      idts = idts + (scnew - scold)
      
      IF (isign .eq. -1) THEN
         tdate=ndate
         ndate=odate
         odate=tdate
         idts = idts * isign
      END IF
   
   END SUBROUTINE geth_idts

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE geth_newdate (ndate, odate, idt)
   
      IMPLICIT NONE
      
      !  From old date ('YYYY-MM-DD HH:MM:SS.ffff') and 
      !            [or ('YYYY-DDDDD HH:MM:SS.ffff')]
      !  delta-time, compute the new date.
   
      !  on entry     -  odate  -  the old hdate.
      !                  idt    -  the change in time
   
      !  on exit      -  ndate  -  the new hdate.
      
      INTEGER , INTENT(IN)           :: idt
      CHARACTER (LEN=*) , INTENT(OUT) :: ndate
      CHARACTER (LEN=*) , INTENT(IN)  :: odate
      
       
      !  Local Variables
       
      !  yrold    -  indicates the year associated with "odate"
      !  moold    -  indicates the month associated with "odate"
      !  dyold    -  indicates the day associated with "odate"
      !  hrold    -  indicates the hour associated with "odate"
      !  miold    -  indicates the minute associated with "odate"
      !  scold    -  indicates the second associated with "odate"
       
      !  yrnew    -  indicates the year associated with "ndate"
      !  monew    -  indicates the month associated with "ndate"
      !  dynew    -  indicates the day associated with "ndate"
      !  hrnew    -  indicates the hour associated with "ndate"
      !  minew    -  indicates the minute associated with "ndate"
      !  scnew    -  indicates the second associated with "ndate"
       
      !  mday     -  a list assigning the number of days in each month
      
      !  i        -  loop counter
      !  nday     -  the integer number of days represented by "idt"
      !  nhour    -  the integer number of hours in "idt" after taking out
      !              all the whole days
      !  nmin     -  the integer number of minutes in "idt" after taking out
      !              all the whole days and whole hours.
      !  nsec     -  the integer number of minutes in "idt" after taking out
      !              all the whole days, whole hours, and whole minutes.
       
      INTEGER :: nlen, olen
      INTEGER :: yrnew, monew, dynew, hrnew, minew, scnew, frnew
      INTEGER :: yrold, moold, dyold, hrold, miold, scold, frold
      INTEGER :: mday(12), nday, nhour, nmin, nsec, nfrac, i, ifrc
      LOGICAL :: opass
      CHARACTER (LEN=10) :: hfrc
      CHARACTER (LEN=1) :: sp
      ! INTEGER, EXTERNAL :: nfeb  ! in the same module now
      
      !  Assign the number of days in a months
      
      mday( 1) = 31
      mday( 2) = 28
      mday( 3) = 31
      mday( 4) = 30
      mday( 5) = 31
      mday( 6) = 30
      mday( 7) = 31
      mday( 8) = 31
      mday( 9) = 30
      mday(10) = 31
      mday(11) = 30
      mday(12) = 31
      
      !  Break down old hdate into parts
      
      hrold = 0
      miold = 0
      scold = 0
      frold = 0
      olen = LEN(odate)
      IF (olen.GE.11) THEN
         sp = odate(11:11)
      else
         sp = ' '
      END IF
      
      !  Use internal READ statements to convert the CHARACTER string
      !  date into INTEGER components.
   
      READ(odate(1:4),  '(I4.4)') yrold
#ifdef PLANET
      READ(odate(6:10), '(I5)') dyold
      moold=0.
#else
      READ(odate(6:7),  '(I2)') moold
      READ(odate(9:10), '(I2)') dyold
#endif
      IF (olen.GE.13) THEN
         READ(odate(12:13),'(I2)') hrold
         IF (olen.GE.16) THEN
            READ(odate(15:16),'(I2)') miold
            IF (olen.GE.19) THEN
               READ(odate(18:19),'(I2)') scold
               IF (olen.GT.20) THEN
                  READ(odate(21:olen),'(I2)') frold
               END IF
            END IF
         END IF
      END IF
      
      !  Set the number of days in February for that year.
      
      mday(2) = nfeb(yrold)
      
      !  Check that ODATE makes sense.
      
      opass = .TRUE.
      
#ifdef PLANET
      !  Check that the day of ODATE makes sense.
      IF ((dyold.GT.PLANET_YEAR).or.(dyold.LT.1)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold
         opass = .FALSE.
      END IF
#else
      !  Check that the month of ODATE makes sense.
      
      IF ((moold.GT.12).or.(moold.LT.1)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Month of ODATE = ', moold
         opass = .FALSE.
      END IF
      
      !  Check that the day of ODATE makes sense.
      
      IF ((dyold.GT.mday(moold)).or.(dyold.LT.1)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold
         opass = .FALSE.
      END IF
#endif
      !  Check that the hour of ODATE makes sense.
      
      IF ((hrold.GT.23).or.(hrold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Hour of ODATE = ', hrold
         opass = .FALSE.
      END IF
      
      !  Check that the minute of ODATE makes sense.
      
      IF ((miold.GT.59).or.(miold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Minute of ODATE = ', miold
         opass = .FALSE.
      END IF
      
      !  Check that the second of ODATE makes sense.
      
      IF ((scold.GT.59).or.(scold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
         opass = .FALSE.
      END IF
      
      !  Check that the fractional part  of ODATE makes sense.
      
      
      IF (.not.opass) THEN
         WRITE( wrf_err_message , * ) 'module_date_time: GETH_NEWDATE: Bad ODATE: ', odate(1:olen), olen
         CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      END IF
      
      !  Date Checks are completed.  Continue.
      
      
      !  Compute the number of days, hours, minutes, and seconds in idt
      
      IF (olen.GT.20) THEN !idt should be in fractions of seconds
         ifrc = olen-20
         ifrc = 10**ifrc
         nday   = ABS(idt)/(86400*ifrc)
         nhour  = MOD(ABS(idt),86400*ifrc)/(3600*ifrc)
         nmin   = MOD(ABS(idt),3600*ifrc)/(60*ifrc)
         nsec   = MOD(ABS(idt),60*ifrc)/(ifrc)
         nfrac = MOD(ABS(idt), ifrc)
      ELSE IF (olen.eq.19) THEN  !idt should be in seconds
         ifrc = 1
         nday   = ABS(idt)/86400 ! Integer number of days in delta-time
         nhour  = MOD(ABS(idt),86400)/3600
         nmin   = MOD(ABS(idt),3600)/60
         nsec   = MOD(ABS(idt),60)
         nfrac  = 0
      ELSE IF (olen.eq.16) THEN !idt should be in minutes
         ifrc = 1
         nday   = ABS(idt)/1440 ! Integer number of days in delta-time
         nhour  = MOD(ABS(idt),1440)/60
         nmin   = MOD(ABS(idt),60)
         nsec   = 0
         nfrac  = 0
      ELSE IF (olen.eq.13) THEN !idt should be in hours
         ifrc = 1
         nday   = ABS(idt)/24 ! Integer number of days in delta-time
         nhour  = MOD(ABS(idt),24)
         nmin   = 0
         nsec   = 0
         nfrac  = 0
      ELSE IF (olen.eq.10) THEN !idt should be in days
         ifrc = 1
         nday   = ABS(idt)/24 ! Integer number of days in delta-time
         nhour  = 0
         nmin   = 0
         nsec   = 0
         nfrac  = 0
      ELSE
         WRITE( wrf_err_message , * ) 'module_date_time: GETH_NEWDATE: Strange length for ODATE: ',olen
         CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      END IF
      
      IF (idt.GE.0) THEN
      
         frnew = frold + nfrac
         IF (frnew.GE.ifrc) THEN
            frnew = frnew - ifrc
            nsec = nsec + 1
         END IF
      
         scnew = scold + nsec
         IF (scnew .GE. 60) THEN
            scnew = scnew - 60
            nmin  = nmin + 1
         END IF
      
         minew = miold + nmin
         IF (minew .GE. 60) THEN
            minew = minew - 60
            nhour  = nhour + 1
         END IF
      
         hrnew = hrold + nhour
         IF (hrnew .GE. 24) THEN
            hrnew = hrnew - 24
            nday  = nday + 1
         END IF
      
         dynew = dyold
         monew = moold
         yrnew = yrold
         DO i = 1, nday
            dynew = dynew + 1
#ifdef PLANET
            IF (dynew .GT. PLANET_YEAR) THEN
               dynew = dynew - PLANET_YEAR
               yrnew = yrnew + 1
            END IF
#else
            IF (dynew.GT.mday(monew)) THEN
               dynew = dynew - mday(monew)
               monew = monew + 1
               IF (monew .GT. 12) THEN
                  monew = 1
                  yrnew = yrnew + 1
                  ! If the year changes, recompute the number of days in February
                  mday(2) = nfeb(yrnew)
               END IF
            END IF
#endif
         END DO
      
      ELSE IF (idt.LT.0) THEN
      
         frnew = frold - nfrac
         IF (frnew .LT. 0) THEN
            frnew = frnew + ifrc
            nsec = nsec - 1
         END IF
      
         scnew = scold - nsec
         IF (scnew .LT. 00) THEN
            scnew = scnew + 60
            nmin  = nmin + 1
         END IF
      
         minew = miold - nmin
         IF (minew .LT. 00) THEN
            minew = minew + 60
            nhour  = nhour + 1
         END IF
      
         hrnew = hrold - nhour
         IF (hrnew .LT. 00) THEN
            hrnew = hrnew + 24
            nday  = nday + 1
         END IF
      
         dynew = dyold
         monew = moold
         yrnew = yrold
         DO i = 1, nday
            dynew = dynew - 1
#ifdef PLANET
            IF (dynew.eq.0) THEN
               dynew = PLANET_YEAR
               yrnew = yrnew - 1
            END IF
#else
            IF (dynew.eq.0) THEN
               monew = monew - 1
               IF (monew.eq.0) THEN
                  monew = 12
                  yrnew = yrnew - 1
                  ! If the year changes, recompute the number of days in February
                  mday(2) = nfeb(yrnew)
               END IF
               dynew = mday(monew)
            END IF
#endif
         END DO
      END IF
      
      !  Now construct the new mdate
      
      nlen = LEN(ndate)
      
#ifdef PLANET
      IF (nlen.GT.20) THEN
         WRITE(ndate(1:19),19) yrnew, dynew, hrnew, minew, scnew
         WRITE(hfrc,'(I10)') frnew+1000000000
         ndate = ndate(1:19)//'.'//hfrc(31-nlen:10)
      
      ELSE IF (nlen.eq.19.or.nlen.eq.20) THEN
         WRITE(ndate(1:19),19) yrnew, dynew, hrnew, minew, scnew
      19   format(I4.4,'-',I5.5,'_',I2.2,':',I2.2,':',I2.2)
         IF (nlen.eq.20) ndate = ndate(1:19)//'.'
      
      ELSE IF (nlen.eq.16) THEN
         WRITE(ndate,16) yrnew, dynew, hrnew, minew
      16   format(I4.4,'-',I5.5,'_',I2.2,':',I2.2)
      
      ELSE IF (nlen.eq.13) THEN
         WRITE(ndate,13) yrnew, dynew, hrnew
      13   format(I4.4,'-',I5.5,'_',I2.2)
      
      ELSE IF (nlen.eq.10) THEN
         WRITE(ndate,10) yrnew, dynew
      10   format(I4.4,'-',I5.5)
      
      END IF
      
      IF (olen.GE.11) ndate(11:11) = sp
#else
      IF (nlen.GT.20) THEN
         WRITE(ndate(1:19),19) yrnew, monew, dynew, hrnew, minew, scnew
         WRITE(hfrc,'(I10)') frnew+1000000000
         ndate = ndate(1:19)//'.'//hfrc(31-nlen:10)
      
      ELSE IF (nlen.eq.19.or.nlen.eq.20) THEN
         WRITE(ndate(1:19),19) yrnew, monew, dynew, hrnew, minew, scnew
      19   format(I4.4,'-',I2.2,'-',I2.2,'_',I2.2,':',I2.2,':',I2.2)
         IF (nlen.eq.20) ndate = ndate(1:19)//'.'
      
      ELSE IF (nlen.eq.16) THEN
         WRITE(ndate,16) yrnew, monew, dynew, hrnew, minew
      16   format(I4.4,'-',I2.2,'-',I2.2,'_',I2.2,':',I2.2)
      
      ELSE IF (nlen.eq.13) THEN
         WRITE(ndate,13) yrnew, monew, dynew, hrnew
      13   format(I4.4,'-',I2.2,'-',I2.2,'_',I2.2)
      
      ELSE IF (nlen.eq.10) THEN
         WRITE(ndate,10) yrnew, monew, dynew
      10   format(I4.4,'-',I2.2,'-',I2.2)
      
      END IF
      
      IF (olen.GE.11) ndate(11:11) = sp
#endif
   END SUBROUTINE geth_newdate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION nfeb ( year ) RESULT (num_days)
   
      ! Compute the number of days in February for the given year
   
      IMPLICIT NONE
   
      INTEGER :: year
      INTEGER :: num_days
   
#ifdef NO_LEAP_CALENDAR
      num_days = 28 ! By default, February has 28 days ...
#else
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
#endif
   
   END FUNCTION nfeb

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE split_date_char ( date , century_year , month , day , hour , minute , second , ten_thousandth)
     
      IMPLICIT NONE
   
      !  Input data.
   
      CHARACTER(LEN=24) , INTENT(IN) :: date 
   
      !  Output data.
   
      INTEGER , INTENT(OUT) :: century_year , month , day , hour , minute , second , ten_thousandth
      
      READ(date,FMT='(    I4.4)') century_year
#ifdef PLANET
      month = 0
      READ(date,FMT='( 5X,I5.5)') day
#else
      READ(date,FMT='( 5X,I2.2)') month
      READ(date,FMT='( 8X,I2.2)') day
#endif
      READ(date,FMT='(11X,I2.2)') hour
      READ(date,FMT='(14X,I2.2)') minute
      READ(date,FMT='(17X,I2.2)') second
      READ(date,FMT='(20X,I4.4)') ten_thousandth
   
   END SUBROUTINE split_date_char

   SUBROUTINE init_module_date_time
   END SUBROUTINE init_module_date_time

END MODULE module_date_time


   ! TBH:  NOTE:  
   ! TBH:  Linkers whine if these routines are placed inside the module.  Not 
   ! TBH:  sure if these should live here or inside an external package.  They 
   ! TBH:  have dependencies both on WRF (for the format of the WRF date-time 
   ! TBH:  strings) and on the time manager.  Currently, the format of the WRF 
   ! TBH:  date-time strings is a slight variant on ISO 8601 (ISO is 
   ! TBH:  "YYYY-MM-DDThh:mm:ss" while WRF is "YYYY-MM-DD_hh:mm:ss").  If we 
   ! TBH:  change the WRF format to match the standard, then we remove the 
   ! TBH:  WRF dependence...  

   ! Converts WRF date-time string into an WRFU_Time object.  
   ! The format of the WRF date-time strings is a slight variant on ISO 8601: 
   ! ISO is "YYYY-MM-DDThh:mm:ss" while WRF is "YYYY-MM-DD_hh:mm:ss".  
   SUBROUTINE wrf_atotime ( str, time )
      USE module_utility
      CHARACTER (LEN=*), INTENT(INOUT) :: str
      TYPE(WRFU_Time),   INTENT(OUT) :: time
      INTEGER yr, mm, dd, h, m, s, ms
      INTEGER rc
      IF ( LEN( str ) .GE. 20 ) THEN
        IF ( str(20:20) .EQ. '.' ) THEN
#ifdef PLANET
          READ(str,'(I4.4,1x,I5.5,1x,I2.2,1x,I2.2,1x,I2.2,1x,I4.4)') yr,dd,h,m,s,ms
          mm = 1
#else
          READ(str,34) yr,mm,dd,h,m,s,ms
#endif
          !  last four digits are ten-thousandths of a sec, convert to ms
          ms=nint(real(ms)/10)
        ELSE
#ifdef PLANET
          READ(str,'(I4.4,1x,I5.5,1x,I2.2,1x,I2.2,1x,I2.2)') yr,dd,h,m,s
          mm = 1
#else
          READ(str,33) yr,mm,dd,h,m,s
#endif
          ms = 0
        ENDIF
      ELSE
#ifdef PLANET
        READ(str,'(I4.4,1x,I5.5,1x,I2.2,1x,I2.2,1x,I2.2)') yr,dd,h,m,s
        mm = 1
#else
        READ(str,33) yr,mm,dd,h,m,s
#endif
        ms = 0
      ENDIF
      CALL WRFU_TimeSet( time, YY=yr, MM=mm, DD=dd, H=h, M=m, S=s, MS=ms, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeSet() in wrf_atotime() FAILED', &
                            __FILE__ , &
                            __LINE__  )
33 FORMAT (I4.4,1x,I2.2,1x,I2.2,1x,I2.2,1x,I2.2,1x,I2.2)
34 FORMAT (I4.4,1x,I2.2,1x,I2.2,1x,I2.2,1x,I2.2,1x,I2.2,1x,I4.4)
      RETURN
   END SUBROUTINE wrf_atotime

   ! Converts an WRFU_Time object into a WRF date-time string.  
   ! The format of the WRF date-time strings is a slight variant on ISO 8601: 
   ! ISO is "YYYY-MM-DDThh:mm:ss" while WRF is "YYYY-MM-DD_hh:mm:ss".  
   SUBROUTINE wrf_timetoa ( time, str )
      USE module_utility, ONLY : WRFU_Time, WRFU_TimeGet, WRFU_SUCCESS
      IMPLICIT NONE
      TYPE(WRFU_Time),   INTENT(INOUT) :: time
      CHARACTER (LEN=*), INTENT(OUT) :: str
      INTEGER strlen, rc
      CHARACTER (LEN=256) :: mess, tmpstr
      ! Assertion
      IF ( LEN(str) < 19 ) THEN
        CALL wrf_error_fatal( 'wrf_timetoa:  str is too short' )
      ENDIF
      tmpstr = ''
      CALL WRFU_TimeGet( time, timeString=tmpstr, rc=rc )
      WRITE(mess,*)'WRFU_TimeGet() returns ',rc,' in wrf_timetoa() FAILED: timeString >',TRIM(tmpstr),'<'
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            mess, &
                            __FILE__ , &
                            __LINE__  )
      ! change ISO 8601 'T' to WRF '_' and hack off fraction if str is not 
      ! big enough to hold it
      strlen = MIN( LEN(str), LEN_TRIM(tmpstr) )
      str = ''
      str(1:strlen) = tmpstr(1:strlen)
      str(11:11) = '_'
      WRITE (mess,*) 'DEBUG wrf_timetoa():  returning with str = [',TRIM(str),']'
      CALL wrf_debug ( 150 , TRIM(mess) )
      RETURN
   END SUBROUTINE wrf_timetoa

   ! Converts an WRFU_TimeInterval object into a time-interval string.  
   SUBROUTINE wrf_timeinttoa ( timeinterval, str )
      USE module_utility
      IMPLICIT NONE
      TYPE(WRFU_TimeInterval),   INTENT(INOUT) :: timeinterval
      CHARACTER (LEN=*), INTENT(OUT) :: str
      INTEGER rc
      CHARACTER (LEN=256) :: mess
      CALL WRFU_TimeIntervalGet( timeinterval, timeString=str, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalGet() in wrf_timeinttoa() FAILED', &
                            __FILE__ , &
                            __LINE__  )
      WRITE (mess,*) 'DEBUG wrf_timeinttoa():  returning with str = [',TRIM(str),']'
      CALL wrf_debug ( 150 , TRIM(mess) )
      RETURN
   END SUBROUTINE wrf_timeinttoa



   ! Debug routine to print key clock information.  
   ! Every printed line begins with pre_str.  
   SUBROUTINE wrf_clockprint ( level, clock, pre_str )
      USE module_utility
      INTEGER,           INTENT( IN) :: level
      TYPE(WRFU_Clock),  INTENT( IN) :: clock
      CHARACTER (LEN=*), INTENT( IN) :: pre_str
      INTEGER rc
      INTEGER :: debug_level
      TYPE(WRFU_Time) :: currTime, startTime, stopTime
      TYPE(WRFU_TimeInterval) :: timeStep
      CHARACTER (LEN=64) :: currTime_str, startTime_str, stopTime_str
      CHARACTER (LEN=64) :: timeStep_str
      CHARACTER (LEN=256) :: mess
      CALL get_wrf_debug_level( debug_level )
      IF ( level .LE. debug_level ) THEN
        CALL WRFU_ClockGet( clock, CurrTime=currTime, StartTime=startTime, &
                                   StopTime=stopTime, TimeStep=timeStep, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'wrf_clockprint:  WRFU_ClockGet() FAILED', &
                              __FILE__ , &
                              __LINE__  )
        CALL wrf_timetoa( currTime, currTime_str )
        CALL wrf_timetoa( startTime, startTime_str )
        CALL wrf_timetoa( stopTime, stopTime_str )
        CALL wrf_timeinttoa( timeStep, timeStep_str )
        WRITE (mess,*) TRIM(pre_str),'  clock start time = ',TRIM(startTime_str)
        CALL wrf_message(TRIM(mess))
        WRITE (mess,*) TRIM(pre_str),'  clock current time = ',TRIM(currTime_str)
        CALL wrf_message(TRIM(mess))
        WRITE (mess,*) TRIM(pre_str),'  clock stop time = ',TRIM(stopTime_str)
        CALL wrf_message(TRIM(mess))
        WRITE (mess,*) TRIM(pre_str),'  clock time step = ',TRIM(timeStep_str)
        CALL wrf_message(TRIM(mess))
      ENDIF
      RETURN
   END SUBROUTINE wrf_clockprint

