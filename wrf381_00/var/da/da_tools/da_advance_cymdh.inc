subroutine da_advance_cymdh (start_date, dh, end_date)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   character (len=10), intent(in)  :: start_date ! In date (ccyymmddhh).
   integer, intent(in)             :: dh         ! Period to advance (-ve for past).
   character (len=10), intent(out) :: end_date   ! Out date (ccyymmddhh).

   integer :: ccyy, mm, dd, hh

   read(start_date(1:10), fmt='(i4, 3i2)')  ccyy, mm, dd, hh

   hh = hh + dh

   do while (hh < 0) 
      hh = hh + 24
      call da_change_date  (ccyy, mm, dd, -1)
   end do

   do while (hh > 23) 
      hh = hh - 24
      call da_change_date  (ccyy, mm, dd, 1)
   end do

   write(unit=end_date(1:10), fmt='(i4.4, 3i2.2)')  ccyy, mm, dd, hh

end subroutine da_advance_cymdh


