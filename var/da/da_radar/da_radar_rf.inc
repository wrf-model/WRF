subroutine da_radar_rf (ra,rs,rg,tc1,ro1,rze)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)    :: ra, rs, rg, tc1, ro1
   real, intent(inout) :: rze

   real                :: zrr, zds, zws, zgr
   real                :: zerr, zews, zeds, zegr
   real                :: czr

   if (trace_use) call da_trace_entry("da_radar_rf")

   ! ro1 use wrfda value, do not need to scale by 0.001

   zrr = 3.63*1.00e+9  ! rainwater
   zws = 4.26*1.00e+11 ! wet snow
   zds = 9.80*1.00e+8  ! dry snow
   zgr = 4.33*1.00e+10 ! graupel

   zerr = zrr*(ro1*ra)**1.75
   zews = zws*(ro1*rs)**1.75
   zeds = zds*(ro1*rs)**1.75
   zegr = zgr*(ro1*rg)**1.75

   if (tc1.ge.5.0) then
      rze = zerr
   elseif (tc1.le.5.0 .and. tc1 .ge.-5.0) then
      czr = (tc1+5.0)/10.0
      if (tc1.le.0.0) then
         rze = czr*zerr + (1.0-czr)*(zeds+zegr)
      else
         rze = czr*zerr + (1.0-czr)*(zews+zegr)
      end if
   elseif (tc1.lt.-5.0) then
       rze = zeds + zegr
   end if

   if (rze.lt.1.0e-20) rze=1.0e-20

   if (trace_use) call da_trace_exit("da_radar_rf")

end subroutine da_radar_rf

