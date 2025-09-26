subroutine da_wdt(h,w,terr,dt)

   !----------------------------------------------------------------------
   ! Purpose: Calculate DT
   !----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: h(kts:kte)
   real, intent(out) :: dt(kts:kte)
   real, intent(in)  :: w(kts:kte+1)
   real, intent(in)  :: terr

   integer :: k

   if (trace_use) call da_trace_entry("da_wdt")

   do k=kte,kts+1,-1
      if (w(k) >= 0.1) then
         dt(k)=(h(k)-h(k-1))/w(k)
      else
         dt(k)=0.0
      end if
   end do

   if (w(kts) >= 0.1) then
      dt(kts)=(h(kts)-terr)/w(kts)
   else
      dt(kts)=0.0
   end if

   if (trace_use) call da_trace_exit("da_wdt")

end subroutine da_wdt


