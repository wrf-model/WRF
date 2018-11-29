!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

module kwm_date_utilities
contains
  subroutine geth_newdate (ndate, odate, idt)
    implicit none

!  From old date ('YYYY-MM-DD HH:MM:SS.ffff') and 
!  delta-time, compute the new date.

!  on entry     -  odate  -  the old hdate.
!                  idt    -  the change in time

!  on exit      -  ndate  -  the new hdate.

    integer, intent(in)           :: idt
    character (len=*), intent(out) :: ndate
    character (len=*), intent(in)  :: odate


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

    integer :: nlen, olen
    integer :: yrnew, monew, dynew, hrnew, minew, scnew, frnew
    integer :: yrold, moold, dyold, hrold, miold, scold, frold
    integer :: mday(12), nday, nhour, nmin, nsec, nfrac, i, ifrc
    logical :: opass
    character (len=10) :: hfrc
    character (len=1) :: sp

    logical :: punctuated
    logical :: idtdy, idthr, idtmin, idtsec, idtfrac

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

!  Determine if the date is "punctuated" or just a string of numbers.
    if ( odate(5:5) == "-") then
       punctuated = .TRUE.
    else
       punctuated = .FALSE.
    endif

!  Break down old hdate into parts

    hrold = 0
    miold = 0
    scold = 0
    frold = 0
    olen = len(odate)
    if (punctuated) then
       if (olen.ge.11) then
          sp = odate(11:11)
       else
          sp = ' '
       end if
    endif

!  Use internal READ statements to convert the CHARACTER string
!  date into INTEGER components.

    idtdy   = .FALSE.
    idthr   = .FALSE.
    idtmin  = .FALSE.
    idtsec  = .FALSE.
    idtfrac = .FALSE.
    read(odate(1:4),  '(i4)') yrold
    if (punctuated) then
       read(odate(6:7),  '(i2)') moold
       read(odate(9:10), '(i2)') dyold
       idtdy = .TRUE.
       if (olen.ge.13) then
          idthr = .TRUE.
          read(odate(12:13),'(i2)') hrold
          if (olen.ge.16) then
             idtmin = .TRUE.
             read(odate(15:16),'(i2)') miold
             if (olen.ge.19) then
                idtsec = .TRUE.
                read(odate(18:19),'(i2)') scold
                if (olen.gt.20) then
                   idtfrac = .TRUE.
                   read(odate(21:olen),*) frold
                end if
             end if
          end if
       end if
    else ! Not punctuated
       read(odate(5:6),  '(i2)') moold
       read(odate(7:8), '(i2)') dyold
       idtdy = .TRUE.
       if (olen.ge.10) then
          idthr = .TRUE.
          read(odate(9:10),'(i2)') hrold
          if (olen.ge.12) then
             idtmin = .TRUE.
             read(odate(11:12),'(i2)') miold
             if (olen.ge.14) then
                idtsec = .TRUE.
                read(odate(13:14),'(i2)') scold
                if (olen.ge.15) then
                   idtfrac = .TRUE.
                   read(odate(15:olen),*) frold
                end if
             end if
          end if
       end if
    endif

!  Set the number of days in February for that year.

    mday(2) = nfeb(yrold)

!  Check that ODATE makes sense.

    opass = .TRUE.

!  Check that the month of ODATE makes sense.

    if ((moold.gt.12).or.(moold.lt.1)) then
       write(*,*) 'GETH_NEWDATE:  Month of ODATE = ', moold
       opass = .FALSE.
    end if

!  Check that the day of ODATE makes sense.

    if ((dyold.gt.mday(moold)).or.(dyold.lt.1)) then
       write(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold
       opass = .FALSE.
    end if

!  Check that the hour of ODATE makes sense.

    if ((hrold.gt.23).or.(hrold.lt.0)) then
       write(*,*) 'GETH_NEWDATE:  Hour of ODATE = ', hrold
       opass = .FALSE.
    end if

!  Check that the minute of ODATE makes sense.

    if ((miold.gt.59).or.(miold.lt.0)) then
       write(*,*) 'GETH_NEWDATE:  Minute of ODATE = ', miold
       opass = .FALSE.
    end if

!  Check that the second of ODATE makes sense.

    if ((scold.gt.59).or.(scold.lt.0)) then
       write(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
       opass = .FALSE.
    end if

!  Check that the fractional part  of ODATE makes sense.

!KWM      IF ((scold.GT.59).or.(scold.LT.0)) THEN
!KWM         WRITE(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
!KWM         opass = .FALSE.
!KWM      END IF

    if (.not.opass) then
       write(*,*) 'FATAL ERROR: In kwm_date_utilities.F geth_newdate() - Crazy ODATE: ', odate(1:olen), olen
       call abort()
    end if

!  Date Checks are completed.  Continue.


!  Compute the number of days, hours, minutes, and seconds in idt

    if (idtfrac) then !idt should be in fractions of seconds
       if (punctuated) then
          ifrc = olen-14
       else
          ifrc = olen-20
       endif
       ifrc = 10**ifrc
       nday   = abs(idt)/(86400*ifrc)
       nhour  = mod(abs(idt),86400*ifrc)/(3600*ifrc)
       nmin   = mod(abs(idt),3600*ifrc)/(60*ifrc)
       nsec   = mod(abs(idt),60*ifrc)/(ifrc)
       nfrac = mod(abs(idt), ifrc)
    else if (idtsec) then  !idt should be in seconds
       ifrc = 1
       nday   = abs(idt)/86400 ! integer number of days in delta-time
       nhour  = mod(abs(idt),86400)/3600
       nmin   = mod(abs(idt),3600)/60
       nsec   = mod(abs(idt),60)
       nfrac  = 0
    else if (idtmin) then !idt should be in minutes
       ifrc = 1
       nday   = abs(idt)/1440 ! integer number of days in delta-time
       nhour  = mod(abs(idt),1440)/60
       nmin   = mod(abs(idt),60)
       nsec   = 0
       nfrac  = 0
    else if (idthr) then !idt should be in hours
       ifrc = 1
       nday   = abs(idt)/24 ! integer number of days in delta-time
       nhour  = mod(abs(idt),24)
       nmin   = 0
       nsec   = 0
       nfrac  = 0
    else if (idtdy) then !idt should be in days
       ifrc = 1
       nday   = abs(idt)    ! integer number of days in delta-time
       nhour  = 0
       nmin   = 0
       nsec   = 0
       nfrac  = 0
    else
       write(*,'(''FATAL ERROR: In kwm_date_utilities.F geth_newdate() - Strange length for ODATE: '', i3)') &
            olen
       write(*,*) odate(1:olen)
       call abort()
    end if

    if (idt.ge.0) then

       frnew = frold + nfrac
       if (frnew.ge.ifrc) then
          frnew = frnew - ifrc
          nsec = nsec + 1
       end if

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
                ! If the year changes, recompute the number of days in February
                mday(2) = nfeb(yrnew)
             end if
          end if
       end do

    else if (idt.lt.0) then

       frnew = frold - nfrac
       if (frnew .lt. 0) then
          frnew = frnew + ifrc
          nsec = nsec + 1
       end if

       scnew = scold - nsec
       if (scnew .lt. 00) then
          scnew = scnew + 60
          nmin  = nmin + 1
       end if

       minew = miold - nmin
       if (minew .lt. 00) then
          minew = minew + 60
          nhour  = nhour + 1
       end if

       hrnew = hrold - nhour
       if (hrnew .lt. 00) then
          hrnew = hrnew + 24
          nday  = nday + 1
       end if

       dynew = dyold
       monew = moold
       yrnew = yrold
       do i = 1, nday
          dynew = dynew - 1
          if (dynew.eq.0) then
             monew = monew - 1
             if (monew.eq.0) then
                monew = 12
                yrnew = yrnew - 1
                ! If the year changes, recompute the number of days in February
                mday(2) = nfeb(yrnew)
             end if
             dynew = mday(monew)
          end if
       end do
    end if

!  Now construct the new mdate

    nlen = LEN(ndate)

    if (punctuated) then

       if (nlen.gt.20) then
          write(ndate(1:19),19) yrnew, monew, dynew, hrnew, minew, scnew
          write(hfrc,'(i10)') frnew+1000000000
          ndate = ndate(1:19)//'.'//hfrc(31-nlen:10)

       else if (nlen.eq.19.or.nlen.eq.20) then
          write(ndate(1:19),19) yrnew, monew, dynew, hrnew, minew, scnew
19        format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2,':',i2.2)
          if (nlen.eq.20) ndate = ndate(1:19)//'.'

       else if (nlen.eq.16) then
          write(ndate,16) yrnew, monew, dynew, hrnew, minew
16        format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2)

       else if (nlen.eq.13) then
          write(ndate,13) yrnew, monew, dynew, hrnew
13        format(i4,'-',i2.2,'-',i2.2,'_',i2.2)
          
       else if (nlen.eq.10) then
          write(ndate,10) yrnew, monew, dynew
10        format(i4,'-',i2.2,'-',i2.2)

       end if

       if (olen.ge.11) ndate(11:11) = sp
       
    else

       if (nlen.gt.20) then
          write(ndate(1:14),14) yrnew, monew, dynew, hrnew, minew, scnew
          write(hfrc,'(i10)') frnew+1000000000
          ndate = ndate(1:18)//hfrc(31-nlen:10)

       else if (nlen.eq.14) then
          write(ndate(1:14),14) yrnew, monew, dynew, hrnew, minew, scnew
14        format(i4,i2.2,i2.2,i2.2,i2.2,i2.2)

       else if (nlen.eq.12) then
          write(ndate,12) yrnew, monew, dynew, hrnew, minew
12        format(i4,i2.2,i2.2,i2.2,i2.2)

       else if (nlen.eq.10) then
          write(ndate,210) yrnew, monew, dynew, hrnew
210       format(i4,i2.2,i2.2,i2.2)
          
       else if (nlen.eq.8) then
          write(ndate,8) yrnew, monew, dynew
8         format(i4,i2.2,i2.2)

       else
          stop "FATAL ERROR: In kwm_date_utilities.F geth_newdate() - DATELEN PROBLEM"
       end if
    endif

  end subroutine geth_newdate

  subroutine geth_idts (newdate, olddate, idt)
    implicit none

!  From 2 input mdates ('YYYY-MM-DD HH:MM:SS.ffff'), 
!  compute the time difference.

!  on entry     -  newdate  -  the new hdate.
!                  olddate  -  the old hdate.

!  on exit      -  idt    -  the change in time.
!                            Units depend on length of date strings.

    character (len=*) , intent(in) :: newdate, olddate
    integer           , intent(out)   :: idt

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

! ndate, odate: local values of newdate and olddate
    character(len=24) :: ndate, odate

    character (len=24) :: tdate
    integer :: olen, nlen
    integer :: yrnew, monew, dynew, hrnew, minew, scnew, frnew
    integer :: yrold, moold, dyold, hrold, miold, scold, frold
    integer :: mday(12), i, newdys, olddys
    logical :: npass, opass
    integer :: isign
    integer :: ifrc

    logical :: punctuated

    olen = len(olddate)
    nlen = len(newdate)
    if (nlen.ne.olen) then
       write(*,'("FATAL ERROR: In kwm_date_utilities.F geth_idts() - NLEN /= OLEN: ", A, 3x, A)') newdate(1:nlen), olddate(1:olen)
       call abort
    endif

    if (olddate.gt.newdate) then
       isign = -1

       ifrc = olen
       olen = nlen
       nlen = ifrc

       ndate = olddate
       odate = newdate
    else
       isign = 1
       ndate = newdate
       odate = olddate
    end if

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

!  Determine if the date is "punctuated" or just a string of numbers.
    if ( odate(5:5) == "-") then
       punctuated = .TRUE.
    else
       punctuated = .FALSE.
    endif


!  Break down old and new hdates into parts


    hrold = 0
    miold = 0
    scold = 0
    frold = 0

    hrnew = 0
    minew = 0
    scnew = 0
    frnew = 0

    read(odate(1:4),  '(i4)') yrold
    read(ndate(1:4),  '(i4)') yrnew

    if (punctuated) then

!  Break down old hdate into parts

       read(odate(6:7),  '(i2)') moold
       read(odate(9:10), '(i2)') dyold
       if (olen.ge.13) then
          read(odate(12:13),'(i2)') hrold
          if (olen.ge.16) then
             read(odate(15:16),'(i2)') miold
             if (olen.ge.19) then
                read(odate(18:19),'(i2)') scold
                if (olen.gt.20) then
                   if (olen.eq.21) then
                      read(odate(21:21),'(i1)') frold
                   else if (olen.eq.22) then
                      read(odate(21:22),'(i2)') frold
                   else if (olen.eq.23) then
                      read(odate(21:23),'(i3)') frold
                   else if (olen.eq.24) then
                      read(odate(21:24),'(i4)') frold
                   endif
                end if
             end if
          end if
       end if
       
!  Break down new hdate into parts

       read(ndate(6:7),  '(i2)') monew
       read(ndate(9:10), '(i2)') dynew
       if (nlen.ge.13) then
          read(ndate(12:13),'(i2)') hrnew
          if (nlen.ge.16) then
             read(ndate(15:16),'(i2)') minew
             if (nlen.ge.19) then
                read(ndate(18:19),'(i2)') scnew
                if (nlen.gt.20) then
                   read(ndate(21:nlen),*) frnew
                end if
             end if
          end if
       end if
    else

!  Break down old hdate into parts

       read(odate(5:6),  '(i2)') moold
       read(odate(7:8), '(i2)') dyold
       if (olen.ge.10) then
          read(odate(9:10),'(i2)') hrold
          if (olen.ge.12) then
             read(odate(11:12),'(i2)') miold
             if (olen.ge.14) then
                read(odate(13:14),'(i2)') scold
                if (olen.ge.15) then
                   read(odate(15:olen),*) frold
                end if
             end if
          end if
       end if
       
!  Break down new hdate into parts

       read(ndate(5:6),  '(i2)') monew
       read(ndate(7:8), '(i2)') dynew
       if (nlen.ge.10) then
          read(ndate(9:10),'(i2)') hrnew
          if (nlen.ge.12) then
             read(ndate(11:12),'(i2)') minew
             if (nlen.ge.14) then
                read(ndate(13:14),'(i2)') scnew
                if (nlen.ge.15) then
                   read(ndate(15:nlen),*) frnew
                end if
             end if
          end if
       end if
    endif

!  Check that the dates make sense.

    npass = .true.
    opass = .true.

!  Check that the month of NDATE makes sense.

    if ((monew.gt.12).or.(monew.lt.1)) then
       print*, 'GETH_IDTS:  Month of NDATE = ', monew
       npass = .false.
    end if

!  Check that the month of ODATE makes sense.

    if ((moold.gt.12).or.(moold.lt.1)) then
       print*, 'GETH_IDTS:  Month of ODATE = ', moold
       opass = .false.
    end if

!  Check that the day of NDATE makes sense.

    if (monew.ne.2) then
       ! ...... For all months but February
       if ((dynew.gt.mday(monew)).or.(dynew.lt.1)) then
          print*, 'GETH_IDTS:  Day of NDATE = ', dynew
          npass = .false.
       end if
    else if (monew.eq.2) then
       ! ...... For February
       if ((dynew > nfeb(yrnew)).or.(dynew < 1)) then
          print*, 'GETH_IDTS:  Day of NDATE = ', dynew
          npass = .false.
       end if
    endif

!  Check that the day of ODATE makes sense.

    if (moold.ne.2) then
       ! ...... For all months but February
       if ((dyold.gt.mday(moold)).or.(dyold.lt.1)) then
          print*, 'GETH_IDTS:  Day of ODATE = ', dyold
          opass = .false.
       end if
    else if (moold.eq.2) then
       ! ....... For February
       if ((dyold > nfeb(yrold)).or.(dyold < 1)) then
          print*, 'GETH_IDTS:  Day of ODATE = ', dyold
          opass = .false.
       end if
    end if

!  Check that the hour of NDATE makes sense.

    if ((hrnew.gt.23).or.(hrnew.lt.0)) then
       print*, 'GETH_IDTS:  Hour of NDATE = ', hrnew
       npass = .false.
    end if

!  Check that the hour of ODATE makes sense.

    if ((hrold.gt.23).or.(hrold.lt.0)) then
       print*, 'GETH_IDTS:  Hour of ODATE = ', hrold
       opass = .false.
    end if

!  Check that the minute of NDATE makes sense.

    if ((minew.gt.59).or.(minew.lt.0)) then
       print*, 'GETH_IDTS:  Minute of NDATE = ', minew
       npass = .false.
    end if

!  Check that the minute of ODATE makes sense.

    if ((miold.gt.59).or.(miold.lt.0)) then
       print*, 'GETH_IDTS:  Minute of ODATE = ', miold
       opass = .false.
    end if

!  Check that the second of NDATE makes sense.

    if ((scnew.gt.59).or.(scnew.lt.0)) then
       print*, 'GETH_IDTS:  SECOND of NDATE = ', scnew
       npass = .false.
    end if

!  Check that the second of ODATE makes sense.

    if ((scold.gt.59).or.(scold.lt.0)) then
       print*, 'GETH_IDTS:  Second of ODATE = ', scold
       opass = .false.
    end if

    if (.not. npass) then
       print*, 'FATAL ERROR: In kwm_data_utilities.F geth_idts() - Screwy NDATE: ', ndate(1:nlen)
       call abort()
    end if

    if (.not. opass) then
       print*, 'FATAL ERROR: In kwm_data_utilities.F geth_idts() - Screwy ODATE: ', odate(1:olen)
       call abort()
    end if

!  Date Checks are completed.  Continue.

!  Compute number of days from 1 January ODATE, 00:00:00 until ndate
!  Compute number of hours from 1 January ODATE, 00:00:00 until ndate
!  Compute number of minutes from 1 January ODATE, 00:00:00 until ndate

    newdys = 0
    do i = yrold, yrnew - 1
       newdys = newdys + 337 + nfeb(i)
    end do

    if (monew .gt. 1) then
       mday(2) = nfeb(yrnew)
       do i = 1, monew - 1
          newdys = newdys + mday(i)
       end do
       mday(2) = 28
    end if

    newdys = newdys + dynew - 1

!  Compute number of hours from 1 January ODATE, 00:00:00 until odate
!  Compute number of minutes from 1 January ODATE, 00:00:00 until odate

    olddys = 0

    if (moold .gt. 1) then
       mday(2) = nfeb(yrold)
       do i = 1, moold - 1
          olddys = olddys + mday(i)
       end do
       mday(2) = 28
    end if

    olddys = olddys + dyold -1

!  Determine the time difference

    idt = (newdys - olddys)
    if (punctuated) then
       if (olen.gt.10) then
          idt = idt*24 + (hrnew - hrold)
          if (olen.gt.13) then
             idt = idt*60 + (minew - miold)
             if (olen.gt.16) then
                idt = idt*60 + (scnew - scold)
                if (olen.gt.20) then
                   ifrc = olen-20
                   ifrc = 10**ifrc
                   idt = idt * ifrc + (frnew-frold)
                endif
             endif
          endif
       endif
    else
       if (olen.gt.8) then
          idt = idt*24 + (hrnew - hrold)
          if (olen.gt.10) then
             idt = idt*60 + (minew - miold)
             if (olen.gt.12) then
                idt = idt*60 + (scnew - scold)
                if (olen.gt.14) then
                   ifrc = olen-14
                   ifrc = 10**ifrc
                   idt = idt * ifrc + (frnew-frold)
                endif
             endif
          endif
       endif
    endif

    if (isign .eq. -1) then
       idt = idt * isign
    end if

  end subroutine geth_idts


  integer function nfeb(year)
!
! Compute the number of days in February for the given year.
!
    implicit none
    integer, intent(in) :: year ! Four-digit year

    nfeb = 28 ! By default, February has 28 days ...
    if (mod(year,4).eq.0) then  
       nfeb = 29  ! But every four years, it has 29 days ...
       if (mod(year,100).eq.0) then
          nfeb = 28  ! Except every 100 years, when it has 28 days ...
          if (mod(year,400).eq.0) then
             nfeb = 29  ! Except every 400 years, when it has 29 days ...
             if (mod(year,3600).eq.0) then
                nfeb = 28  ! Except every 3600 years, when it has 28 days.
             endif
          endif
       endif
    endif
  end function nfeb

  integer function nmdays(hdate)
!
! Compute the number of days in the month of given date hdate.
!
    implicit none
    character(len=*), intent(in) :: hdate

    integer :: year, month
    integer, dimension(12), parameter :: ndays = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

    if (hdate(5:5) == "-") then
       read(hdate(1:7), '(I4,1x,I2)') year, month
    else
       read(hdate(1:6), '(I4,I2)') year, month
    endif

    if (month == 2) then
       nmdays = nfeb(year)
    else
       nmdays = ndays(month)
    endif
  end function nmdays
end module kwm_date_utilities
