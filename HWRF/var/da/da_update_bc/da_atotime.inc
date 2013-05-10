subroutine da_atotime(date_char, st)
 
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   character(len=*), intent(in) :: date_char
   real,              intent(out) :: st
  
   integer                :: ccyy,mo,dd,hh,mi,ss,i
   integer, dimension(12) :: mmday
 
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
  
   dd=dd+365*(ccyy-2000)

   do i=1,mo-1
      dd=dd+mmday(i)
   end do
  
   st = real(ss) &
      + 60.0*(real(mi) &
      + 60.0*(real(hh) &
      + 24.0* real(dd)))

end subroutine da_atotime


