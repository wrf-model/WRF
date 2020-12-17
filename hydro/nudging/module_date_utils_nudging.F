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

module module_date_utils_nudging
contains

!===================================================================================================
! Subroutine Name: geth_newdate
! Author(s)/Contact(s): 
!   Wei Yu? or NoahMP developers?
!   James L McCreight, jamesmcc><ucar><edu
! Abstract: 
!   Calculate a time difference from two time strings: idts=newdate-olddate
! History Log: 
!   7/22/15 -Header Created, JLM
! Usage: 
!   call geth_newdate(ndate, odate, idt)
! Parameters: 
!   newdate : the resulting date: odate+idt
!   olddate : the start time for the sum: odate+id
!   idts    : the time increment in seconds: odate+id
! Input Files: 
!   None.
! Output Files: 
!   None.
! Condition codes: 
!   call hydro_stop("geth_newdate") is used. 
! User controllable options: 
!   None. 
! Notes: 
!   This routine was copied from ../LandModel/Utility_routines/module_date_utilities.F
!   There are some advantages to having a separate copy here, both compiling and 
!   tailored needs. 

subroutine geth_newdate (ndate, odate, idt)
implicit none

!  From old date ("YYYY-MM-DD HH:MM:SS.ffff" or "YYYYMMDDHHMMSSffff") and 
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

integer :: newlen, oldlen
integer :: yrnew, monew, dynew, hrnew, minew, scnew, frnew
integer :: yrold, moold, dyold, hrold, miold, scold, frold
integer :: nday, nhour, nmin, nsec, nfrac, i, ifrc
logical :: opass
character (len=10) :: hfrc
character (len=1) :: sp
logical :: punct
integer :: yrstart, yrend, mostart, moend, dystart, dyend
integer :: hrstart, hrend, mistart, miend, scstart, scend, frstart
integer :: units
integer, dimension(12) :: mday = (/31,28,31,30,31,30,31,31,30,31,30,31/)

! Determine if odate is "YYYY-MM-DD_HH ... " or "YYYYMMDDHH...."
if (odate(5:5) == "-") then
   punct = .TRUE.
else
   punct = .FALSE.
endif

!  Break down old hdate into parts

hrold = 0
miold = 0
scold = 0
frold = 0
oldlen = LEN(odate)
if (punct) then
   yrstart = 1
   yrend = 4
   mostart = 6
   moend = 7
   dystart = 9
   dyend = 10
   hrstart = 12
   hrend = 13
   mistart = 15
   miend = 16
   scstart = 18
   scend = 19
   frstart = 21
   select case (oldlen)
   case (10)
      ! Days
      units = 1
   case (13)
      ! Hours
      units = 2
   case (16)
      ! Minutes
      units = 3
   case (19)
      ! Seconds
      units = 4
   case (21)
      ! Tenths
      units = 5
   case (22)
      ! Hundredths
      units = 6
   case (23)
      ! Thousandths
      units = 7
   case (24)
      ! Ten thousandths
      units = 8
   case default

      write(*,*) 'FATAL ERROR: geth_newdate:  odd length: #'//trim(odate)//'#'
      call hydro_stop("geth_newdate")

   end select

   if (oldlen.ge.11) then
      sp = odate(11:11)
   else
      sp = ' '
   end if

else

   yrstart = 1
   yrend = 4
   mostart = 5
   moend = 6
   dystart = 7
   dyend = 8
   hrstart = 9
   hrend = 10
   mistart = 11
   miend = 12
   scstart = 13
   scend = 14
   frstart = 15

   select case (oldlen)
   case (8)
      ! Days
      units = 1
   case (10)
      ! Hours
      units = 2
   case (12)
      ! Minutes
      units = 3
   case (14)
      ! Seconds
      units = 4
   case (15)
      ! Tenths
      units = 5
   case (16)
      ! Hundredths
      units = 6
   case (17)
      ! Thousandths
      units = 7
   case (18)
      ! Ten thousandths
      units = 8
   case default

      write(*,*) 'FATAL ERROR: geth_newdate:  odd length: #'//trim(odate)//'#'
      call hydro_stop("geth_newdate")

   end select
endif

!  Use internal READ statements to convert the CHARACTER string
!  date into INTEGER components.

read(odate(yrstart:yrend),  '(i4)') yrold
read(odate(mostart:moend),  '(i2)') moold
read(odate(dystart:dyend), '(i2)') dyold
if (units.ge.2) then
   read(odate(hrstart:hrend),'(i2)') hrold
   if (units.ge.3) then
      read(odate(mistart:miend),'(i2)') miold
      if (units.ge.4) then
         read(odate(scstart:scend),'(i2)') scold
         if (units.ge.5) then
            read(odate(frstart:oldlen),*) frold
         end if
      end if
   end if
end if

!  Set the number of days in February for that year.

mday(2) = nfeb(yrold)

!  Check that ODATE makes sense.

opass = .TRUE.

!  Check that the month of ODATE makes sense.

if ((moold.gt.12).or.(moold.lt.1)) then
#ifdef HYDRO_D
   write(*,*) 'GETH_NEWDATE:  Month of ODATE = ', moold
#endif
   opass = .FALSE.
end if

!  Check that the day of ODATE makes sense.

if ((dyold.gt.mday(moold)).or.(dyold.lt.1)) then
#ifdef HYDRO_D
   write(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold
#endif
   opass = .FALSE.
end if

!  Check that the hour of ODATE makes sense.

if ((hrold.gt.23).or.(hrold.lt.0)) then
#ifdef HYDRO_D
   write(*,*) 'GETH_NEWDATE:  Hour of ODATE = ', hrold
#endif
   opass = .FALSE.
end if

!  Check that the minute of ODATE makes sense.

if ((miold.gt.59).or.(miold.lt.0)) then
#ifdef HYDRO_D
   write(*,*) 'GETH_NEWDATE:  Minute of ODATE = ', miold
#endif
   opass = .FALSE.
end if

!  Check that the second of ODATE makes sense.

if ((scold.gt.59).or.(scold.lt.0)) then
#ifdef HYDRO_D
   write(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
#endif
   opass = .FALSE.
end if

!  Check that the fractional part  of ODATE makes sense.


if (.not.opass) then

   write(*,*) 'FATAL ERROR: Crazy ODATE: ', odate(1:oldlen), oldlen
   stop

end if

!  Date Checks are completed.  Continue.


!  Compute the number of days, hours, minutes, and seconds in idt

if (units.ge.5) then !idt should be in fractions of seconds
   ifrc = oldlen-(frstart)+1
   ifrc = 10**ifrc
   nday   = abs(idt)/(86400*ifrc)
   nhour  = mod(abs(idt),86400*ifrc)/(3600*ifrc)
   nmin   = mod(abs(idt),3600*ifrc)/(60*ifrc)
   nsec   = mod(abs(idt),60*ifrc)/(ifrc)
   nfrac = mod(abs(idt), ifrc)
else if (units.eq.4) then  !idt should be in seconds
   ifrc = 1
   nday   = abs(idt)/86400 ! integer number of days in delta-time
   nhour  = mod(abs(idt),86400)/3600
   nmin   = mod(abs(idt),3600)/60
   nsec   = mod(abs(idt),60)
   nfrac  = 0
else if (units.eq.3) then !idt should be in minutes
   ifrc = 1
   nday   = abs(idt)/1440 ! integer number of days in delta-time
   nhour  = mod(abs(idt),1440)/60
   nmin   = mod(abs(idt),60)
   nsec   = 0
   nfrac  = 0
else if (units.eq.2) then !idt should be in hours
   ifrc = 1
   nday   = abs(idt)/24 ! integer number of days in delta-time
   nhour  = mod(abs(idt),24)
   nmin   = 0
   nsec   = 0
   nfrac  = 0
else if (units.eq.1) then !idt should be in days
   ifrc = 1
   nday   = abs(idt)    ! integer number of days in delta-time
   nhour  = 0
   nmin   = 0
   nsec   = 0
   nfrac  = 0
else

   write(*,'(''GETH_NEWDATE: Strange length for ODATE: '', i3)') &
        oldlen
   write(*,*) '#'//odate(1:oldlen)//'#'
   call hydro_stop("geth_newdate")

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

newlen = LEN(ndate)

if (punct) then

   if (newlen.gt.frstart) then
      write(ndate(1:scend),19) yrnew, monew, dynew, hrnew, minew, scnew
      write(hfrc,'(i10)') frnew+1000000000
      ndate = ndate(1:scend)//'.'//hfrc(31-newlen:10)

   else if (newlen.eq.scend) then
      write(ndate(1:scend),19) yrnew, monew, dynew, hrnew, minew, scnew
19    format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2,':',i2.2)

   else if (newlen.eq.miend) then
      write(ndate,16) yrnew, monew, dynew, hrnew, minew
16    format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2)

   else if (newlen.eq.hrend) then
      write(ndate,13) yrnew, monew, dynew, hrnew
13    format(i4,'-',i2.2,'-',i2.2,'_',i2.2)

   else if (newlen.eq.dyend) then
      write(ndate,10) yrnew, monew, dynew
10    format(i4,'-',i2.2,'-',i2.2)

   end if

else

   if (newlen.gt.frstart) then
      write(ndate(1:scend),119) yrnew, monew, dynew, hrnew, minew, scnew
      write(hfrc,'(i10)') frnew+1000000000
      ndate = ndate(1:scend)//'.'//hfrc(31-newlen:10)

   else if (newlen.eq.scend) then
      write(ndate(1:scend),119) yrnew, monew, dynew, hrnew, minew, scnew
119   format(i4,i2.2,i2.2,i2.2,i2.2,i2.2)

   else if (newlen.eq.miend) then
      write(ndate,116) yrnew, monew, dynew, hrnew, minew
116   format(i4,i2.2,i2.2,i2.2,i2.2)

   else if (newlen.eq.hrend) then
      write(ndate,113) yrnew, monew, dynew, hrnew
113   format(i4,i2.2,i2.2,i2.2)

   else if (newlen.eq.dyend) then
      write(ndate,110) yrnew, monew, dynew
110   format(i4,i2.2,i2.2)

   end if

endif

if (punct .and. (oldlen.ge.11) .and. (newlen.ge.11)) ndate(11:11) = sp

end subroutine geth_newdate



!===================================================================================================
! Subroutine Name: geth_idts
! Author(s)/Contact(s): 
!   Wei Yu? or NoahMP developers?
!   James L McCreight, jamesmcc><ucar><edu
! Abstract: 
!   Calculate a time difference from two time strings: idts=newdate-olddate
! History Log: 
!   7/22/15 -Header Created, JLM
! Usage: 
!   call geth_idts(newdate, olddate, idt)
! Parameters: 
!   newdate : first date in difference  (newdate-olddate).
!   olddate : second date in difference (newdate-olddate).
!   idts    : difference with units dependent on input resolution.
! Input Files: 
!   None.
! Output Files: 
!   None.
! Condition codes: 
!   Currently incomplete error handling. 
! User controllable options: 
!   None. 
! Notes: 
!   This routine was copied from ../LandModel/Utility_routines/module_date_utilities.F
!   There are some advantages to having a separate copy here, both compiling and 
!   tailored needs. 
subroutine geth_idts(newdate, olddate, idt)

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

integer :: oldlen, newlen
integer :: yrnew, monew, dynew, hrnew, minew, scnew, frnew
integer :: yrold, moold, dyold, hrold, miold, scold, frold
integer :: i, newdys, olddys
logical :: npass, opass
integer :: timesign
integer :: ifrc
integer, dimension(12) :: mday = (/31,28,31,30,31,30,31,31,30,31,30,31/)
logical :: punct
integer :: yrstart, yrend, mostart, moend, dystart, dyend
integer :: hrstart, hrend, mistart, miend, scstart, scend, frstart
integer :: units

oldlen = len(olddate)
newlen = len(newdate)
if (newlen.ne.oldlen) then
#ifdef HYDRO_D
   write(*,'("GETH_IDTS: NEWLEN /= OLDLEN: ", A, 3x, A)') newdate(1:newlen), olddate(1:oldlen)
   call hydro_stop("geth_newdate")
#endif
endif

if (olddate.gt.newdate) then
   timesign = -1

   ifrc = oldlen
   oldlen = newlen
   newlen = ifrc

   ndate = olddate
   odate = newdate
else
   timesign = 1
   ndate = newdate
   odate = olddate
end if

! Break down old hdate into parts

! Determine if olddate is punctuated or not
if (odate(5:5) == "-") then
   punct = .TRUE.
   if (ndate(5:5) /= "-") then

      write(*,'("GETH_IDTS: Dates appear to be different formats: ", A, 3x, A)') &
           ndate(1:newlen), odate(1:oldlen)
      call hydro_stop("geth_idts utils_nudging -1")

   endif
else
   punct = .FALSE.
   if (ndate(5:5) == "-") then

      write(*,'("GETH_IDTS: Dates appear to be different formats: ", A, 3x, A)') &
           ndate(1:newlen), odate(1:oldlen)
      call hydro_stop("geth_idts utils_nudging 0")

   endif
endif

if (punct) then
   yrstart = 1
   yrend = 4
   mostart = 6
   moend = 7
   dystart = 9
   dyend = 10
   hrstart = 12
   hrend = 13
   mistart = 15
   miend = 16
   scstart = 18
   scend = 19
   frstart = 21
   select case (oldlen)
   case (10)
      ! Days
      units = 1
   case (13)
      ! Hours
      units = 2
   case (16)
      ! Minutes
      units = 3
   case (19)
      ! Seconds
      units = 4
   case (21)
      ! Tenths
      units = 5
   case (22)
      ! Hundredths
      units = 6
   case (23)
      ! Thousandths
      units = 7
   case (24)
      ! Ten thousandths
      units = 8
   case default

      write(*,*) 'ERROR: geth_idts:  odd length: #'//trim(odate)//'#'
      call hydro_stop("geth_idts utils_nudging 1")

   end select
else

   yrstart = 1
   yrend = 4
   mostart = 5
   moend = 6
   dystart = 7
   dyend = 8
   hrstart = 9
   hrend = 10
   mistart = 11
   miend = 12
   scstart = 13
   scend = 14
   frstart = 15

   select case (oldlen)
   case (8)
      ! Days
      units = 1
   case (10)
      ! Hours
      units = 2
   case (12)
      ! Minutes
      units = 3
   case (14)
      ! Seconds
      units = 4
   case (15)
      ! Tenths
      units = 5
   case (16)
      ! Hundredths
      units = 6
   case (17)
      ! Thousandths
      units = 7
   case (18)
      ! Ten thousandths
      units = 8
   case default

      write(*,*) 'ERROR: geth_idts:  odd length: #'//trim(odate)//'#'
      call hydro_stop("geth_idts nudging_utils 2")

   end select
endif


hrold = 0
miold = 0
scold = 0
frold = 0

read(odate(yrstart:yrend), '(i4)') yrold
read(odate(mostart:moend), '(i2)') moold
read(odate(dystart:dyend), '(i2)') dyold
if (units.ge.2) then
   read(odate(hrstart:hrend),'(i2)') hrold
   if (units.ge.3) then
      read(odate(mistart:miend),'(i2)') miold
      if (units.ge.4) then
         read(odate(scstart:scend),'(i2)') scold
         if (units.ge.5) then
            read(odate(frstart:oldlen),*) frold
         end if
      end if
   end if
end if

!  Break down new hdate into parts

hrnew = 0
minew = 0
scnew = 0
frnew = 0

read(ndate(yrstart:yrend), '(i4)') yrnew
read(ndate(mostart:moend), '(i2)') monew
read(ndate(dystart:dyend), '(i2)') dynew
if (units.ge.2) then
   read(ndate(hrstart:hrend),'(i2)') hrnew
   if (units.ge.3) then
      read(ndate(mistart:miend),'(i2)') minew
      if (units.ge.4) then
         read(ndate(scstart:scend),'(i2)') scnew
         if (units.ge.5) then
            read(ndate(frstart:newlen),*) frnew
         end if
      end if
   end if
end if

!  Check that the dates make sense.

npass = .true.
opass = .true.

!  Check that the month of NDATE makes sense.

if ((monew.gt.12).or.(monew.lt.1)) then
#ifdef HYDRO_D
   write(*,*) 'GETH_IDTS:  Month of NDATE = ', monew
#endif
   npass = .false.
end if

!  Check that the month of ODATE makes sense.

if ((moold.gt.12).or.(moold.lt.1)) then
#ifdef HYDRO_D
   print*, 'GETH_IDTS:  Month of ODATE = ', moold
#endif
   opass = .false.
end if

!  Check that the day of NDATE makes sense.

if (monew.ne.2) then
   ! ...... For all months but February
   if ((dynew.gt.mday(monew)).or.(dynew.lt.1)) then
#ifdef HYDRO_D
      print*, 'GETH_IDTS:  Day of NDATE = ', dynew
#endif
      npass = .false.
   end if
else if (monew.eq.2) then
   ! ...... For February
   if ((dynew > nfeb(yrnew)).or.(dynew < 1)) then
#ifdef HYDRO_D
      print*, 'GETH_IDTS:  Day of NDATE = ', dynew
#endif
      npass = .false.
   end if
endif

!  Check that the day of ODATE makes sense.

if (moold.ne.2) then
   ! ...... For all months but February
   if ((dyold.gt.mday(moold)).or.(dyold.lt.1)) then
#ifdef HYDRO_D
      print*, 'GETH_IDTS:  Day of ODATE = ', dyold
#endif
      opass = .false.
   end if
else if (moold.eq.2) then
   ! ....... For February
   if ((dyold > nfeb(yrold)).or.(dyold < 1)) then
#ifdef HYDRO_D
      print*, 'GETH_IDTS:  Day of ODATE = ', dyold
#endif
      opass = .false.
   end if
end if

!  Check that the hour of NDATE makes sense.

if ((hrnew.gt.23).or.(hrnew.lt.0)) then
#ifdef HYDRO_D
   print*, 'GETH_IDTS:  Hour of NDATE = ', hrnew
#endif
   npass = .false.
end if

!  Check that the hour of ODATE makes sense.

if ((hrold.gt.23).or.(hrold.lt.0)) then
#ifdef HYDRO_D
   print*, 'GETH_IDTS:  Hour of ODATE = ', hrold
#endif
   opass = .false.
end if

!  Check that the minute of NDATE makes sense.

if ((minew.gt.59).or.(minew.lt.0)) then
#ifdef HYDRO_D
   print*, 'GETH_IDTS:  Minute of NDATE = ', minew
#endif
   npass = .false.
end if

!  Check that the minute of ODATE makes sense.

if ((miold.gt.59).or.(miold.lt.0)) then
#ifdef HYDRO_D
   print*, 'GETH_IDTS:  Minute of ODATE = ', miold
#endif
   opass = .false.
end if

!  Check that the second of NDATE makes sense.

if ((scnew.gt.59).or.(scnew.lt.0)) then
#ifdef HYDRO_D
   print*, 'GETH_IDTS:  SECOND of NDATE = ', scnew
#endif
   npass = .false.
end if

!  Check that the second of ODATE makes sense.

if ((scold.gt.59).or.(scold.lt.0)) then
#ifdef HYDRO_D
   print*, 'GETH_IDTS:  Second of ODATE = ', scold
#endif
   opass = .false.
end if

if (.not. npass) then
#ifdef HYDRO_D
   print*, 'Screwy NDATE: ', ndate(1:newlen)
   call hydro_stop("geth_idts nudging_utils 3")
#endif
end if

if (.not. opass) then
#ifdef HYDRO_D
   print*, 'FATAL ERROR: Screwy ODATE: ', odate(1:oldlen)
   call hydro_stop("geth_idts nudging_utils 4")
#endif
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
if (units.ge.2) then
   idt = idt*24 + (hrnew - hrold)
   if (units.ge.3) then
      idt = idt*60 + (minew - miold)
      if (units.ge.4) then
         idt = idt*60 + (scnew - scold)
         if (units.ge.5) then
            ifrc = oldlen-(frstart-1)
            ifrc = 10**ifrc
            idt = idt * ifrc + (frnew-frold)
         endif
      endif
   endif
endif

if (timesign .eq. -1) then
   idt = idt * timesign
end if

end subroutine geth_idts


!===================================================================================================
! Function Name: get_idts_elemental
! Author(s)/Contact(s): 
!   James L McCreight, jamesmcc><ucar><edu
! Abstract: 
!   get_idts on a vector.
!
! History Log: 
!   10/11/16 -Header Created, JLM
! Usage: 
!
! Parameters: 
!
! Input Files: 
!   None.
! Output Files: 
!   None.
! Condition codes: 
!
! User controllable options: 
!   None. 
! Notes: 
!elemental subroutine geth_idts_elemental(newdate, olddate, idt)
!end



!===================================================================================================
! Function Name: nfeb
! Author(s)/Contact(s): 
!   Wei Yu? or NoahMP developers?
!   James L McCreight, jamesmcc><ucar><edu
! Abstract: 
!
! History Log: 
!   7/22/15 -Header Created, JLM
! Usage: 
!
! Parameters: 
!
! Input Files: 
!   None.
! Output Files: 
!   None.
! Condition codes: 
!
! User controllable options: 
!   None. 
! Notes: 
!   This routine was copied from ../LandModel/Utility_routines/module_date_utilities.F
!   There are some advantages to having a separate copy here, both compiling and 
!   tailored needs. 

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

!===================================================================================================
! Function Name: nmdays
! Author(s)/Contact(s): 
!   Wei Yu? or NoahMP developers?
!   James L McCreight, jamesmcc><ucar><edu
! Abstract: 
!
! History Log: 
!   7/22/15 -Header Created, JLM
! Usage: 
!
! Parameters: 
!
! Input Files: 
!   None.
! Output Files: 
!   None.
! Condition codes: 
!
! User controllable options: 
!   None. 
! Notes: 
!   This routine was copied from ../LandModel/Utility_routines/module_date_utilities.F
!   There are some advantages to having a separate copy here, both compiling and 
!   tailored needs. 

integer function nmdays(hdate)
!
! Compute the number of days in the month of given date hdate.
!
implicit none
character(len=*), intent(in) :: hdate

integer :: year, month
integer, dimension(12), parameter :: ndays = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

read(hdate(1:7), '(I4,1x,I2)') year, month

if (month == 2) then
   nmdays = nfeb(year)
else
   nmdays = ndays(month)
endif
end function nmdays

!===================================================================================================
! Function Name: monthabbr_to_mm
! Author(s)/Contact(s): 
!   Wei Yu? or NoahMP developers?
!   James L McCreight, jamesmcc><ucar><edu
! Abstract: 
!
! History Log: 
!   7/22/15 -Header Created, JLM
! Usage: 
!
! Parameters: 
!
! Input Files: 
!   None.
! Output Files: 
!   None.
! Condition codes: 
!
! User controllable options: 
!   None. 
! Notes: 
!   This routine was copied from ../LandModel/Utility_routines/module_date_utilities.F
!   There are some advantages to having a separate copy here, both compiling and 
!   tailored needs. 

function monthabbr_to_mm(mon) result(mm)
implicit none

character(len=3), intent(in) :: mon

integer :: mm

if (mon == "Jan") then
   mm = 1
elseif (mon == "Feb") then
   mm = 2
elseif (mon == "Mar") then
   mm = 3
elseif (mon == "Apr") then
   mm = 4
elseif (mon == "May") then
   mm = 5
elseif (mon == "Jun") then
   mm = 6
elseif (mon == "Jul") then
   mm = 7
elseif (mon == "Aug") then
   mm = 8
elseif (mon == "Sep") then
   mm = 9
elseif (mon == "Oct") then
   mm = 10
elseif (mon == "Nov") then
   mm = 11
elseif (mon == "Dec") then
   mm = 12
else

   write(*, '("Function monthabbr_to_mm:  mon = <",A,">")') mon
   print*,  "FATAL ERROR: Function monthabbr_to_mm:  Unrecognized mon"
   call hydro_stop("monthabbr_to_mm")

endif
end function monthabbr_to_mm

!===================================================================================================
! Subroutine Name: swap_date_format
! Author(s)/Contact(s): 
!   Wei Yu? or NoahMP developers?
!   James L McCreight, jamesmcc><ucar><edu
! Abstract: 
!
! History Log: 
!   7/22/15 -Header Created, JLM
! Usage: 
!
! Parameters: 
!
! Input Files: 
!   None.
! Output Files: 
!   None.
! Condition codes: 
!
! User controllable options: 
!   None. 
! Notes: 
!   This routine was copied from ../LandModel/Utility_routines/module_date_utilities.F
!   There are some advantages to having a separate copy here, both compiling and 
!   tailored needs. 

subroutine swap_date_format(indate, outdate)
implicit none
character(len=*), intent(in)  :: indate
character(len=*), intent(out) :: outdate
integer :: inlen

inlen = len(indate)
if (indate(5:5) == "-") then
   select case (inlen)
   case (10)
      ! YYYY-MM-DD
      outdate = indate(1:4)//indate(6:7)//indate(9:10)
   case (13)
      ! YYYY-MM-DD_HH
      outdate = indate(1:4)//indate(6:7)//indate(9:10)//indate(12:13)
   case (16)
      ! YYYY-MM-DD_HH:mm
      outdate = indate(1:4)//indate(6:7)//indate(9:10)//indate(12:13)//indate(15:16)
   case (19)
      ! YYYY-MM-DD_HH:mm:ss
      outdate = indate(1:4)//indate(6:7)//indate(9:10)//indate(12:13)//indate(15:16)//&
           indate(18:19)
   case (21,22,23,24)
      ! YYYY-MM-DD_HH:mm:ss.f[f[f[f]]]
      outdate = indate(1:4)//indate(6:7)//indate(9:10)//indate(12:13)//indate(15:16)//&
           indate(18:19)//indate(21:inlen)
   case default
#ifdef HYDRO_D
      write(*,'("Unrecognized length: <", A,">")') indate
      call hydro_stop("swap_date_format")
#endif
   end select
else
   select case (inlen)
   case (8)
      ! YYYYMMDD
      outdate = indate(1:4)//"-"//indate(5:6)//"-"//indate(7:8)
   case (10)
      ! YYYYMMDDHH
      outdate = indate(1:4)//"-"//indate(5:6)//"-"//indate(7:8)//"_"//&
           indate(9:10)
   case (12)
      ! YYYYMMDDHHmm
      outdate = indate(1:4)//"-"//indate(5:6)//"-"//indate(7:8)//"_"//&
           indate(9:10)//":"//indate(11:12)
   case (14)
      ! YYYYMMDDHHmmss
      outdate = indate(1:4)//"-"//indate(5:6)//"-"//indate(7:8)//"_"//&
           indate(9:10)//":"//indate(11:12)//":"//indate(13:14)
   case (15,16,17,18)
      ! YYYYMMDDHHmmssf[f[f[f]]]
      outdate = indate(1:4)//"-"//indate(5:6)//"-"//indate(7:8)//"_"//&
           indate(9:10)//":"//indate(11:12)//":"//indate(13:14)//"."//indate(15:inlen)
   case default

      write(*,'("FATAL ERROR: Unrecognized length: <", A,">")') indate
      call hydro_stop("swap_date_format")

   end select
endif

end subroutine swap_date_format

!===================================================================================================
! Function Name: mm_to_monthabbr
! Author(s)/Contact(s): 
!   Wei Yu? or NoahMP developers?
!   James L McCreight, jamesmcc><ucar><edu
! Abstract: 
!
! History Log: 
!   7/22/15 -Header Created, JLM
! Usage: 
!
! Parameters: 
!
! Input Files: 
!   None.
! Output Files: 
!   None.
! Condition codes: 
!
! User controllable options: 
!   None. 
! Notes: 
!   This routine was copied from ../LandModel/Utility_routines/module_date_utilities.F
!   There are some advantages to having a separate copy here, both compiling and 
!   tailored needs. 

character(len=3) function mm_to_monthabbr(ii) result(mon)
implicit none
integer, intent(in) :: ii
character(len=3), parameter, dimension(12) :: month = (/ &
     "Jan", "Feb", "Mar", "Apr", "May", "Jun", &
     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" /)
if (ii > 0 .and. ii < 13 ) then
   mon = month(ii)
else

   print*, "FATAL ERROR: mm_to_monthabbr"
   call hydro_stop("mm_to_monthabbr")

endif
end function mm_to_monthabbr

!===================================================================================================
! Function Name: round_resolution_minute
! Author(s)/Contact(s): 
!   James L McCreight, jamesmcc><ucar><edu
! Abstract: Round a time to some time resolution specified in minutes:
!           1) the resolution evenly divides 60
!           2) the first time chunk in a hour is centered on 0 minutes and
!              subesquent chunks are centered on N*resoluton (mod 60).
! History Log: 
!   7/29/15 -Header Created, JLM
! Usage: 
!
! Parameters: 
!
! Input Files: 
!   None.
! Output Files: 
!   None.
! Condition codes: 
!   None.
! User controllable options: 
!   None. 
! Notes: Some tests:
! print*,round_resolution_minute('2003-02-01_01:00:00', 5)
! print*,round_resolution_minute('2003-02-01_01:02:29', 5)
! print*,round_resolution_minute('2003-02-01_01:02:30', 5)
! print*,round_resolution_minute('2003-02-01_01:57:29', 5)
! print*,round_resolution_minute('2003-02-01_01:57:30', 5)
! print*,''
! print*,round_resolution_minute('2003-02-01_01:00:00', 15)
! print*,round_resolution_minute('2003-02-01_01:07:29', 15)
! print*,round_resolution_minute('2003-02-01_01:07:30', 15)
! print*,round_resolution_minute('2003-02-01_01:52:29', 15)
! print*,round_resolution_minute('2003-02-01_01:52:30', 15)

function round_resolution_minute(time, resolution)
implicit none
character(len=19)             :: round_resolution_minute ! return value
character(len=19), intent(in) :: time         ! time to round
integer                       :: resolution   ! the time resolution in minutes

integer :: diffMin
real    :: resolutionInv, theMinute, theSecond, resMin
character(len=19) :: roundTime, timeCopy

! nearest defines a time resolution
resolutionInv = 1./resolution
!1234567890123456789
!YYYY-MM-DD_HH:mm:ss
read(time(15:16),*) theMinute
read(time(18:19),*) theSecond
theMinute = theMinute + (theSecond/60.)
! What fraction of the time resolution is the current time?
resMin = (theMinute * resolutionInv) 
! Round to the time resolution.
! nint doesnt document its rounding rule on gfortran! boo.
if(resMin - floor(resMin) .ge. .5) resMin=ceiling(resMin)
if(resMin - floor(resMin) .lt. .5) resMin=floor(resMin)
! Convert back to regular minutes.
resMin = resMin / resolutionInv
! To get the exact right time, some floor/ceil operations are helpful.
timeCopy = time
timeCopy(18:19) = '00' ! Floor of theMinute in string form
! nint is needed b/c resMin is real and often slightly off integer, 
! and this is exacerbated by *60
diffMin = (nint(resMin) - floor(theMinute))*60
!print*, nint(resMin), floor(theMinute), timeCopy, diffMin
call geth_newdate(round_resolution_minute, timeCopy, diffMin)

end function round_resolution_minute



end module module_date_utils_nudging
