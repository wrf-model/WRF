program da_advance_time

   !   modified from da_advance_cymdh, 
   !   - has accuracy down to second,
   !   - can use day/hour/minute/second (with/without +/- sign) to advance time,
   !   - can digest various input date format if it still has the right order (ie. cc yy mm dd hh nn ss)
   !   - can digest flexible time increment 
   !   - can output in wrf date format (ccyy-mm-dd_hh:nn:ss)
   !   - can specify output date format
   !   - can output Julian day
   !   - can output Gregorian days and seconds (since year 1601)
   !
   !   eg.: da_advance_time 20070730      12             # advance 12 h 
   !        da_advance_time 2007073012   -1d2h30m30s     # back 1 day 2 hours 30 minutes and 30 seconds
   !        da_advance_time 2007073012    1s-3h30m       # back 3 hours 30 minutes less 1 second
   !        da_advance_time 200707301200  2d1s -w        # advance 2 days and 1 second, output in wrf date format
   !        da_advance_time 2007-07-30_12:00:00 2d1s -w               # same as previous example
   !        da_advance_time 200707301200  2d1s -f ccyy-mm-dd_hh:nn:ss # same as previous example
   !        da_advance_time 2007073006    120 -j         # advance 120 h, and print year and Julian day
   !        da_advance_time 2007073006    120 -J         # advance 120 h, print year, Julian day, hour, minute and second
   !        da_advance_time 2007073006    0 -g           # print Gregorian day and second (since year 1601)
   !

#ifdef crayx1
#define iargc ipxfargc
#endif

   implicit none

!  interface
!     integer function iargc()
!     end function iargc
!  end interface
   integer, external :: iargc

   integer :: ccyy, mm, dd, hh, nn, ss, dday, dh, dn, ds, gday, gsec

   integer :: nargum, i, n

   character(len=80), dimension(10) :: argum

   character(len=14) :: ccyymmddhhnnss

   character(len=80) :: out_date_format, dtime

   integer :: datelen
   
   integer, parameter :: stdout=6

   nargum=iargc()

   if ( nargum < 2 ) then
      write(unit=stdout, fmt='(a)') &
         'Usage:   da_advance_time ccyymmddhh[nnss] [+|-]dt[d|h|m|s] [-w|-W|-wrf|-WRF] [-f|-F date_format] [-j|-J] [-g|-G]'
      write(unit=stdout, fmt='(a)') &
         'Option:  -w|-W|-wrf|-WRF  output in wrf date format as ccyy-mm-dd_hh:nn:ss'
      write(unit=stdout, fmt='(a)') &
         '         -f|-F  specify output date format, such as ccyy-mm-dd_hh:nn:ss, or ''ccyy/mm/dd  hh:nn:ss'''
      write(unit=stdout, fmt='(a)') &
         '         -j|-J  print Julian day'
      write(unit=stdout, fmt='(a)') &
         '         -g|-G  print Gregorian days and seconds (since year 1601)'
      write(unit=stdout, fmt='(a)') &
         'Example: da_advance_time 20070730      12            # advance 12 h'
      write(unit=stdout, fmt='(a)') &
         '         da_advance_time 2007073012   -1d2h30m30s    # back 1 day 2 hours 30 min and 30 sec'
      write(unit=stdout, fmt='(a)') &
         '         da_advance_time 2007073012    1s-3h30m      # back 3 hours 30 minutes less 1 second'
      write(unit=stdout, fmt='(a)') &
         '         da_advance_time 200707301200  1d1s -w       # advance 1 day 1 sec, output in wrf date format'
      write(unit=stdout, fmt='(a)') &
         '         da_advance_time 2007-07-30_12:00:00 2d1s -w               # same as previous example'
      write(unit=stdout, fmt='(a)') &
         '         da_advance_time 200707301200  2d1s -f ccyy-mm-dd_hh:nn:ss # same as previous' 
      write(unit=stdout, fmt='(a)') &
         '         da_advance_time 2007073006    120 -j         # advance 120 h, and print year and Julian day'
      write(unit=stdout, fmt='(a)') &
         '         da_advance_time 2007073006    120 -J         # advance 120 h, print year, Julian day, hour, minute and second'
      write(unit=stdout, fmt='(a)') &
         '         da_advance_time 2007073006    0 -g           # print Gregorian day and second (since year 1601)'
      write(unit=stdout, fmt='(a)') ''
      stop 'try again.'
   end if

   do i=1,nargum
      do n=1,80
         argum(i)(n:n)=' '
      end do
      call getarg(i,argum(i))
   end do

   ccyymmddhhnnss = parsedate(argum(1))
   datelen = len_trim(ccyymmddhhnnss)

   if (datelen == 8) then
      read(ccyymmddhhnnss(1:10), fmt='(i4, 2i2)')  ccyy, mm, dd
      hh = 0
      nn = 0
      ss = 0
   else if (datelen == 10) then
      read(ccyymmddhhnnss(1:10), fmt='(i4, 3i2)')  ccyy, mm, dd, hh
      nn = 0
      ss = 0
   else if (datelen == 12) then
      read(ccyymmddhhnnss(1:12), fmt='(i4, 4i2)')  ccyy, mm, dd, hh, nn
      ss = 0
   else if (datelen == 14) then
      read(ccyymmddhhnnss(1:14), fmt='(i4, 5i2)')  ccyy, mm, dd, hh, nn, ss
   else
      stop 'wrong input date'
   endif

   if (.not. validdate(ccyy,mm,dd,hh,nn,ss)) then
      stop 'Start date is not valid, or has wrong format'
   endif

   i = 0

   dtime = trim(argum(2))
   call parsedt(dtime,dday,dh,dn,ds)

   hh = hh + dh
   nn = nn + dn
   ss = ss + ds

   ! advance minute according to second
   do while (ss < 0) 
      ss = ss + 60
      nn = nn - 1
   end do
   do while (ss > 59) 
      ss = ss - 60
      nn = nn + 1
   end do

   ! advance hour according to minute
   do while (nn < 0) 
      nn = nn + 60
      hh = hh - 1
   end do
   do while (nn > 59) 
      nn = nn - 60
      hh = hh + 1
   end do

   ! advance day according to hour
   do while (hh < 0) 
      hh = hh + 24
      dday = dday - 1
   end do

   do while (hh > 23) 
      hh = hh - 24
      dday = dday + 1
   end do

   ! advance day if dday /= 0
   if (dday /= 0) call change_date ( ccyy, mm, dd, dday)

   write(ccyymmddhhnnss(1:14), fmt='(i4, 5i2.2)')  ccyy, mm, dd, hh, nn, ss
   if ( nargum == 2 ) then
      if (datelen<14) then
         if(nn /= 0) datelen=12
         if(ss /= 0) datelen=14
      endif
      write(unit=stdout, fmt='(a)') ccyymmddhhnnss(1:datelen)
   else if ( nargum > 2 ) then
      i = 3
      do while (i <= nargum)
        select case ( trim(argum(i)) )
           case ('-w', '-W', '-wrf','-WRF')
              out_date_format = 'ccyy-mm-dd_hh:nn:ss'
              write(unit=stdout, fmt='(a)') trim(formatdate(ccyymmddhhnnss, out_date_format))
              i = i+1
           case ('-f', '-F')
              out_date_format = trim(argum(i+1))
              write(unit=stdout, fmt='(a)') trim(formatdate(ccyymmddhhnnss, out_date_format))
              i = i+2
           case ('-j')
              write(unit=stdout, fmt='(I4,I4)') ccyy, julian_day(ccyy,mm,dd)
              i = i+1
           case ('-J')
              write(unit=stdout, fmt='(I4,I4,I3,I3,I3)') ccyy, julian_day(ccyy,mm,dd),hh,nn,ss
              i = i+1
           case ('-g','-G')
              call gregorian_day_sec(ccyy,mm,dd,hh,nn,ss,gday,gsec)
              write(unit=stdout, fmt='(I8,I8)') gday, gsec
              i = i+1
           case default
              i = i+1
        end select
      end do
   end if

contains

subroutine change_date( ccyy, mm, dd, delta )

   implicit none

   integer, intent(inout) :: ccyy, mm, dd
   integer, intent(in)    :: delta

   integer                :: dday, direction

   dday = abs(delta)
   direction = sign(1,delta)

   do while (dday > 0) 

      dd = dd + direction

      if (dd == 0) then
         mm = mm - 1

         if (mm == 0) then
            mm = 12
            ccyy = ccyy - 1
         end if

         dd = getmmday(ccyy,mm)

      elseif ( dd > getmmday(ccyy,mm)) then
         dd = 1
         mm = mm + 1
         if(mm > 12 ) then
            mm = 1
            ccyy = ccyy + 1
         end if
      end if

      dday = dday - 1

   end do
   return
end subroutine change_date

#ifdef crayx1

   subroutine getarg(i, harg)
      implicit none
      character(len=*) :: harg
      integer :: ierr, ilen, i

      call pxfgetarg(i, harg, ilen, ierr)
      return
   end subroutine getarg
#endif

function parsedate(datein)
   character(len=80), intent(in) :: datein

   character(len=14) :: parsedate
   character(len=1 ) :: ch
   integer :: n, i
   parsedate = '00000000000000'
   i=0
   do n = 1, len_trim(datein)
      ch = datein(n:n)
      if (ch >= '0' .and. ch <= '9') then
         i=i+1
         parsedate(i:i)=ch
      end if
   end do
   if (parsedate(11:14) == '0000') then
      parsedate(11:14) = ''
   else if(parsedate(13:14) == '00') then
      parsedate(13:14) = ''
   end if
   return 
end function parsedate

subroutine parsedt(dt,dday,dh,dn,ds)
   character(len=80), intent(in) :: dt
   integer,           intent(inout) :: dday, dh, dn, ds

   character(len=1 ) :: ch
   integer :: n,i,d,s,nounit
   ! initialize time and sign
   nounit=1
   dday=0
   dh=0
   dn=0
   ds=0
   d=0
   s=1
   do n = 1, len_trim(dt)
      ch = dt(n:n)
      select case (ch)
         case ('0':'9')
           read(ch,fmt='(i1)') i
           d=d*10+i
         case ('-')
           s=-1
         case ('+')
           s=1
         case ('d')
           nounit=0
           dday=dday+d*s
           d=0
         case ('h')
           nounit=0
           dh=dh+d*s
           d=0
         case ('n','m')
           nounit=0
           dn=dn+d*s
           d=0
         case ('s')
           nounit=0
           ds=ds+d*s
           d=0
         case ('.')
           stop "Decimal values not allowed"
         case default
      end select
   end do
   if (nounit==1) dh=d*s
end subroutine parsedt

function formatdate(datein,dateform)
   character(len=14), intent(in) :: datein
   character(len=80), intent(in) :: dateform
   character(len=80) :: formatdate
   integer :: ic,iy,im,id,ih,in,is
   ic=index(dateform,'cc')
   iy=index(dateform,'yy')
   im=index(dateform,'mm')
   id=index(dateform,'dd')
   ih=index(dateform,'hh')
   in=index(dateform,'nn')
   is=index(dateform,'ss')
   formatdate=trim(dateform)
   if (ic /= 0) formatdate(ic:ic+1) = datein(1:2)
   if (iy /= 0) formatdate(iy:iy+1) = datein(3:4)
   if (im /= 0) formatdate(im:im+1) = datein(5:6)
   if (id /= 0) formatdate(id:id+1) = datein(7:8)
   if (ih /= 0) formatdate(ih:ih+1) = datein(9:10)
   if (in /= 0) formatdate(in:in+1) = datein(11:12)
   if (is /= 0) formatdate(is:is+1) = datein(13:14)
   return
end function formatdate

function julian_day(ccyy,mm,dd)
   integer, intent(in) :: ccyy,mm,dd
   integer :: julian_day
   integer, parameter, dimension( 13) :: &
      bgn_day = (/ 0,  31,  59,  90, 120, 151, &
                 181, 212, 243, 273, 304, 334, 365 /), &
      bgn_day_ly = (/ 0,  31,  60,  91, 121, 152, &
                    182, 213, 244, 274, 305, 335, 366 /)
   if (isleapyear(ccyy)) then
      julian_day = bgn_day_ly(mm)+dd
   else
      julian_day = bgn_day(mm)+dd
   end if
end function julian_day

function isleapyear(year)
   ! check if year is leapyear
   integer,intent(in) :: year
   logical :: isleapyear
   if( mod(year,4) .ne. 0 ) then
     isleapyear=.FALSE.
   else
     isleapyear=.TRUE.
     if ( mod(year,100) == 0 .and. mod(year,400) .ne. 0 ) isleapyear=.FALSE.
   endif
end function isleapyear

subroutine gregorian_day_sec(year,month,day,hours,minutes,seconds,gday,gsec)
   integer, intent(in)  :: day, month, year, hours, minutes, seconds
   integer, intent(out) :: gday, gsec

   integer :: ndays, m, nleapyr
   integer :: base_year = 1601
   integer :: days_per_month(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)

   if( year < base_year ) stop "Year can not be before 1601!"

   ! compute number of leap years fully past since base_year
   nleapyr = (year - base_year) / 4 - (year - base_year) / 100 + (year - base_year) / 400
   ! Count up days in this year
   ndays = 0
   do m=1,month-1
      ndays = ndays + days_per_month(m)
      if(isleapyear(year) .and. m == 2) ndays = ndays + 1
   enddo
   gsec = seconds + 60*(minutes + 60*hours)
   gday = day - 1 + ndays + 365*(year - base_year - nleapyr) + 366*(nleapyr)
   return
end subroutine gregorian_day_sec

function validdate(ccyy,mm,dd,hh,nn,ss)
   integer, intent(in) :: ccyy,mm,dd,hh,nn,ss

   logical :: validdate

   validdate = .true.

   if(ss > 59 .or. ss < 0 .or. &
      nn > 59 .or. nn < 0 .or. &
      hh > 23 .or. hh < 0 .or. &
                   dd < 1 .or. &
      mm > 12 .or. mm < 1 ) validdate = .false.

   if (mm == 2 .and. ( dd > 29 .or. &
                     ((.not. isleapyear(ccyy)) .and. dd > 28))) &
      validdate = .false.
end function validdate

function getmmday(ccyy,mm)
   integer, intent(in) :: ccyy,mm

   integer :: getmmday
   integer, dimension(12) :: mmday, mmday_ly

   mmday = (/31,28,31,30,31,30,31,31,30,31,30,31/)
   mmday_ly = (/31,29,31,30,31,30,31,31,30,31,30,31/)

   if(isleapyear(ccyy)) then
     getmmday=mmday_ly(mm)
   else
     getmmday=mmday(mm)
   endif

end function getmmday

end program da_advance_time
