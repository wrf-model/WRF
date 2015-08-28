subroutine da_atotime(date_char, st)
 
   !-----------------------------------------------------------------------
   ! Purpose: Input a date character string in WRF format (CCYY-MM-DD_hh:mm:ss)
   !          Output the number of seconds since Dec 31, 1999, 00:00:00
   !-----------------------------------------------------------------------

   implicit none

   character(len=*), intent(in) :: date_char
   real,              intent(out) :: st
  
   integer                :: ccyy,mo,dd,hh,mi,ss,i
   integer, dimension(12) :: mmday
   integer                :: dayssince2000
 
   mmday=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  
   read(date_char(1:19),'(i4,1x,4(i2,1x),i2)') &
        ccyy, mo, dd, hh, mi, ss
  
   if (mod(ccyy,4) == 0) then
      mmday(2) = 29
      if (mod(ccyy,400) == 0) then
         mmday(2) = 29
      else if (mod(ccyy,100) == 0) then
         mmday(2) = 28
      end if
   end if

   dayssince2000 = 0;

   ! This set of if statements sets "dayssince2000" to the number of days from the beginning of
   ! the year 2000 to the beginning of the current year (for example, 2000 returns "0", 
   ! 2001 returns 366, 2012 returns 4018, 1999 returns -365, etc.)
   if (ccyy < 2000) then
      do i=ccyy,1999
         dayssince2000 = dayssince2000 - 365

         !If statements to cover leap year cases
         if (mod(i,4) == 0) then
            dayssince2000 = dayssince2000 - 1
            if (mod(i,100) == 0) then
               dayssince2000 = dayssince2000 + 1
               if (mod(i,400) == 0) then
                  dayssince2000 = dayssince2000 - 1
               end if
            end if
         end if
      end do
   else if (ccyy > 2000) then
      do i=2000,ccyy-1
         dayssince2000 = dayssince2000 + 365

         !If statements to cover leap year cases
         if (mod(i,4) == 0) then
            dayssince2000 = dayssince2000 + 1
            if (mod(i,100) == 0) then
               dayssince2000 = dayssince2000 - 1
               if (mod(i,400) == 0) then
                  dayssince2000 = dayssince2000 + 1
               end if
            end if
         end if
      end do
   end if

   dd=dd+dayssince2000

   do i=1,mo-1
      dd=dd+mmday(i)
   end do
  
   st = real(ss) + 60.0*(real(mi) + 60.0*(real(hh) + 24.0* real(dd)))

end subroutine da_atotime


