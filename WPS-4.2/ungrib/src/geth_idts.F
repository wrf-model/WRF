      subroutine geth_idts (ndate, odate, idts)
      implicit none

!***********************************************************************
!
!  purpose      -  from 2 input mdates ('YYYY-MM-DD HH:MM:SS'), compute
!                  the time difference in seconds.
!
!  on entry     -  ndate  -  the new hdate.
!                  odate  -  the old hdate.
!
!  on exit      -  idts    -  the change in time in seconds.
!
!***********************************************************************

      character*(*) ndate, odate
      character*19 tdate
      integer idts
      integer olen, nlen
!
!  Local Variables
!
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
!  newhrs   -  the number of hours between "ndate" and 1901
!              whole 24 hour days
!  oldhrs   -  the number of hours between "odate" and 1901
!

      integer yrnew, monew, dynew, hrnew, minew, scnew
      integer yrold, moold, dyold, hrold, miold, scold
      integer mday(12), i, newdys, olddys
      logical npass, opass
      integer isign

! External function:
      integer, external :: nfeb

!*************************  Subroutine Begin  **************************

      if (odate.gt.ndate) then
         isign = -1
         tdate=ndate
         ndate=odate
         odate=tdate
      else
         isign = 1
      endif

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

      read(odate(1:4),  '(I4)', err=101) yrold
      read(odate(6:7),  '(I2)', err=101) moold
      read(odate(9:10), '(I2)', err=101) dyold
      if (olen.ge.13) then
         read(odate(12:13),'(I2)', err=101) hrold
         if (olen.ge.16) then
            read(odate(15:16),'(I2)', err=101) miold
            if (olen.ge.19) then
               read(odate(18:19),'(I2)', err=101) scold
            endif
         endif
      endif

!
!  Break down new hdate into parts
!
      hrnew = 0
      minew = 0
      scnew = 0
      nlen = len(ndate)

      read(ndate(1:4),  '(I4)', err=102) yrnew
      read(ndate(6:7),  '(I2)', err=102) monew
      read(ndate(9:10), '(I2)', err=102) dynew
      if (nlen.ge.13) then
         read(ndate(12:13),'(I2)', err=102) hrnew
         if (nlen.ge.16) then
            read(ndate(15:16),'(I2)', err=102) minew
            if (nlen.ge.19) then
               read(ndate(18:19),'(I2)', err=102) scnew
            endif
         endif
      endif

!
!  Check that the dates make sense.
!
      npass = .true.
      opass = .true.

!  Check that the month of NDATE makes sense.

      if ((monew.gt.12).or.(monew.lt.1)) then
         print*, 'GETH_IDTS:  Month of NDATE = ', monew
         npass = .false.
      endif

!  Check that the month of ODATE makes sense.

      if ((moold.gt.12).or.(moold.lt.1)) then
         print*, 'GETH_IDTS:  Month of ODATE = ', moold
         opass = .false.
      endif

!  Check that the day of NDATE makes sense.

      if (monew.ne.2) then
! ...... For all months but February
         if ((dynew.gt.mday(monew)).or.(dynew.lt.1)) then
            print*, 'GETH_IDTS:  Day of NDATE = ', dynew
            npass = .false.
         endif
      elseif (monew.eq.2) then
! ...... For February
         if ((dynew .gt. nfeb(yrnew)).or.(dynew.lt.1)) then
            print*, 'GETH_IDTS:  Day of NDATE = ', dynew
            npass = .false.
         endif
      endif

!  Check that the day of ODATE makes sense.

      if (moold.ne.2) then
! ...... For all months but February
         if ((dyold.gt.mday(moold)).or.(dyold.lt.1)) then
            print*, 'GETH_IDTS:  Day of ODATE = ', dyold
            opass = .false.
         endif
      elseif (moold.eq.2) then
! ....... For February
         if ((dyold .gt. nfeb(yrold)).or.(dyold .lt. 1)) then
            print*, 'GETH_IDTS:  Day of ODATE = ', dyold
            opass = .false.
         endif
      endif

!  Check that the hour of NDATE makes sense.

      if ((hrnew.gt.23).or.(hrnew.lt.0)) then
         print*, 'GETH_IDTS:  Hour of NDATE = ', hrnew
         npass = .false.
      endif

!  Check that the hour of ODATE makes sense.

      if ((hrold.gt.23).or.(hrold.lt.0)) then
         print*, 'GETH_IDTS:  Hour of ODATE = ', hrold
         opass = .false.
      endif

!  Check that the minute of NDATE makes sense.

      if ((minew.gt.59).or.(minew.lt.0)) then
         print*, 'GETH_IDTS:  Minute of NDATE = ', minew
         npass = .false.
      endif

!  Check that the minute of ODATE makes sense.

      if ((miold.gt.59).or.(miold.lt.0)) then
         print*, 'GETH_IDTS:  Minute of ODATE = ', miold
         opass = .false.
      endif
!
!  Check that the second of NDATE makes sense.
!
      if ((scnew.gt.59).or.(scnew.lt.0)) then
         print*, 'GETH_IDTS:  SECOND of NDATE = ', scnew
         npass = .false.
      endif

!  Check that the second of ODATE makes sense.

      if ((scold.gt.59).or.(scold.lt.0)) then
         print*, 'GETH_IDTS:  Second of ODATE = ', scold
         opass = .false.
      endif

      if (.not. npass) then
         print*, 'Screwy NDATE: ', ndate(1:nlen)
         STOP 'Error_ndate'
      endif

      if (.not. opass) then
         print*, 'Screwy ODATE: ', odate(1:olen)
         STOP 'Error_odate'
      endif
!
!  Date Checks are completed.  Continue.
!

!
!  Compute number of days from 1 January ODATE, 00:00:00 until ndate
!  Compute number of hours from 1 January ODATE, 00:00:00 until ndate
!  Compute number of minutes from 1 January ODATE, 00:00:00 until ndate
!

      newdys = 0
      do i = yrold, yrnew - 1
         newdys = newdys + (365 + (nfeb(i)-28))
      enddo

      if (monew .gt. 1) then
         mday(2) = nfeb(yrnew)
         do i = 1, monew - 1
            newdys = newdys + mday(i)
         enddo
         mday(2) = 28
      end if

      newdys = newdys + dynew-1
!
!  Compute number of hours from 1 January ODATE, 00:00:00 until odate
!  Compute number of minutes from 1 January ODATE, 00:00:00 until odate
!

      olddys = 0

      if (moold .gt. 1) then
         mday(2) = nfeb(yrold)
         do i = 1, moold - 1
            olddys = olddys + mday(i)
         enddo
         mday(2) = 28
      end if

      olddys = olddys + dyold-1
!
!  Determine the time difference in seconds
!
      idts = (newdys - olddys) * 86400
      idts = idts + (hrnew - hrold) * 3600
      idts = idts + (minew - miold) * 60
      idts = idts + (scnew - scold)

      if (isign .eq. -1) then
         tdate=ndate
         ndate=odate
         odate=tdate
         idts = idts * isign
      endif


      return
  101 write(6,*) 'Error reading odate. odate = ',odate
      write(6,*) 'Most likely an error in namelist.wps'
      stop 'geth_idts 101'
  102 write(6,*) 'Error reading ndate. ndate = ',ndate
      write(6,*) 'Most likely an error in namelist.wps'
      stop 'geth_idts 102'

!**************************  Subroutine End  ***************************

      end
      integer function nfeb(year)
!
! Compute the number of days in February for the given year.
!
        implicit none
        integer, intent(in) :: year ! Four-digit year

#ifdef NO_LEAP_CALENDAR
        nfeb = 28 ! February always has 28 days for No Leap Calendar ...
#else
        nfeb = 28 ! By default, February has 28 days ...
        if (mod(year,4).eq.0) then  
           nfeb = 29         ! But every four years, it has 29 days ...
           if (mod(year,100).eq.0) then
              nfeb = 28      ! Except every 100 years, when it has 28 days ...
              if (mod(year,400).eq.0) then
                 nfeb = 29   ! Except every 400 years, when it has 29 days ...
                 if (mod(year,3600).eq.0) then
                    nfeb = 28 ! Except every 3600 years, when it has 28 days.
                 endif
              endif
           endif
        endif
#endif

      end function nfeb
