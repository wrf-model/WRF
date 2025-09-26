      subroutine geth_newdate (ndate, odate, idts)
      implicit none

!**********************************************************************
!
!  purpose      -  from old date ('YYYY-MM-DD*HH:MM:SS') and time in
!                  seconds, compute the new date.
!
!  on entry     -  odate  -  the old hdate.
!                  idts   -  the change in time in seconds.
!
!  on exit      -  ndate  -  the new hdate.
!                  idts    -  the change in time in seconds.
!
!**********************************************************************

      integer idts
      character*(*) ndate, odate
      integer nlen, olen

!
!  Local Variables
!
!  yrold    -  indicates the year associated with "odate"
!  moold    -  indicates the month associated with "odate"
!  dyold    -  indicates the day associated with "odate"
!  hrold    -  indicates the hour associated with "odate"
!  miold    -  indicates the minute associated with "odate"
!  scold    -  indicates the second associated with "odate"
!
!  yrnew    -  indicates the year associated with "ndate"
!  monew    -  indicates the month associated with "ndate"
!  dynew    -  indicates the day associated with "ndate"
!  hrnew    -  indicates the hour associated with "ndate"
!  minew    -  indicates the minute associated with "ndate"
!  scnew    -  indicates the second associated with "ndate"
!
!  mday     -  a list assigning the number of days in each month

!  dth      -  the number of hours represented by "idts"
!  i        -  loop counter
!  nday     -  the integer number of days represented by "idts"
!  nhour    -  the integer number of hours in "idts" after taking out
!              all the whole days
!  nmin     -  the integer number of minutes in "idts" after taking out
!              all the whole days and whole hours.
!  nsec     -  the integer number of minutes in "idts" after taking out
!              all the whole days, whole hours, and whole minutes.
!

      integer yrnew, monew, dynew, hrnew, minew, scnew
      integer yrold, moold, dyold, hrold, miold, scold
      integer mday(12), nday, nhour, nmin, nsec, i
      real    dth
      logical opass
      logical noLeapCalendar


!*************************  Subroutine Begin  *************************

!
!  Determine whether the routine should use a no-leap year calendar
!
      noLeapCalendar = .false.
     
#ifdef NO_LEAP_CALENDAR
      noLeapCalendar = .true.
#endif


!
!  Assign the number of days in a months
!

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

!
!  Break down old hdate into parts
!
      hrold = 0
      miold = 0
      scold = 0
      olen = len(odate)

      read(odate(1:4),  '(I4)') yrold
      read(odate(6:7),  '(I2)') moold
      read(odate(9:10), '(I2)') dyold
      if (olen.ge.13) then
         read(odate(12:13),'(I2)') hrold
         if (olen.ge.16) then
            read(odate(15:16),'(I2)') miold
            if (olen.ge.19) then
               read(odate(18:19),'(I2)') scold
            endif
         endif
      endif
!
!  Set the number of days in February for that year.
!
      mday(2) = 28
      if (.not. noLeapCalendar) then
         if (mod(yrold,4).eq.0) then
            mday(2) = 29
            if (mod(yrold,100).eq.0) then
               mday(2) = 28
               if (mod(yrold,400).eq.0) then
                  mday(2) = 29
               endif
            endif
         endif
      endif
!
!  Check that ODATE makes sense.
!
      opass = .TRUE.

!  Check that the month of ODATE makes sense.

      if ((moold.gt.12).or.(moold.lt.1)) then
         print*, 'GETH_NEWDATE:  Month of ODATE = ', moold
         opass = .FALSE.
      endif

!  Check that the day of ODATE makes sense.

      if ((dyold.gt.mday(moold)).or.(dyold.lt.1)) then

         !
         ! Send a custom message if any leap-day files if it is a No-Leap Calendar
         !
         if (noLeapCalendar .and. (moold .eq. 2) .and. (dyold .eq. 29)) then
            print*,'GET_NEWDATE:  Using a no-Leap Calendar, but data for 2/29 was found.'
         else
            print*, 'GET_NEWDATE:  Day of ODATE = ', dyold
         endif

            opass = .FALSE.
      endif

!  Check that the hour of ODATE makes sense.

      if ((hrold.gt.23).or.(hrold.lt.0)) then
         print*, 'GET_NEWDATE:  Hour of ODATE = ', hrold
         opass = .FALSE.
      endif

!  Check that the minute of ODATE makes sense.

      if ((miold.gt.59).or.(miold.lt.0)) then
         print*, 'GET_NEWDATE:  Minute of ODATE = ', miold
         opass = .FALSE.
      endif

!  Check that the second of ODATE makes sense.

      if ((scold.gt.59).or.(scold.lt.0)) then
         print*, 'GET_NEWDATE:  Second of ODATE = ', scold
         opass = .FALSE.
      endif

      if (.not.opass) then
         print*, 'Crazy ODATE: ', odate(1:olen), olen
         STOP 'Error_odate'
!        stop
      endif
!
!  Date Checks are completed.  Continue.
!

!
!  Compute the number of days, hours, minutes, and seconds in idts
!
      nday   = idts/86400  ! Integer number of days in delta-time
      nhour   = mod(idts,86400)/3600
      nmin   = mod(idts,3600)/60
      nsec   = mod(idts,60)

      scnew = scold + nsec
      if (scnew .ge. 60) then
         scnew = scnew - 60
         nmin  = nmin + 1
      end if
      minew = miold + nmin
      if (minew .ge. 60) then
         minew = minew - 60
         nhour  = nhour + 1
      end if
      hrnew = hrold + nhour
      if (hrnew .ge. 24) then
         hrnew = hrnew - 24
         nday  = nday + 1
      end if

      dynew = dyold
      monew = moold
      yrnew = yrold
      do i = 1, nday
         dynew = dynew + 1
         if (dynew.gt.mday(monew)) then
            dynew = dynew - mday(monew)
            monew = monew + 1
            if (monew .gt. 12) then
               monew = 1
               yrnew = yrnew + 1

               mday(2) = 28
               if (.not. noLeapCalendar) then
                  if (mod(yrnew,4).eq.0) then
                     mday(2) = 29
                     if (mod(yrnew,100).eq.0) then
                        mday(2) = 28
                        if (mod(yrnew,400).eq.0) then
                           mday(2) = 29
                        endif
                     endif
                  endif
               endif

            end if
         endif
      enddo

!
!  Now construct the new mdate
!
      nlen = len(ndate)

      if (nlen.ge.19) then
         write(ndate,19) yrnew, monew, dynew, hrnew, minew, scnew
 19      format(I4,'-',I2.2,'-',I2.2,'_',I2.2,':',I2.2,':',I2.2)

      else if (nlen.eq.16) then
         write(ndate,16) yrnew, monew, dynew, hrnew, minew
 16      format(I4,'-',I2.2,'-',I2.2,'_',I2.2,':',I2.2)

      else if (nlen.eq.13) then
         write(ndate,13) yrnew, monew, dynew, hrnew
 13      format(I4,'-',I2.2,'-',I2.2,'_',I2.2)

      else if (nlen.eq.10) then
         write(ndate,10) yrnew, monew, dynew
 10      format(I4,'-',I2.2,'-',I2.2)

      endif

!**************************  Subroutine End  **************************

      end
