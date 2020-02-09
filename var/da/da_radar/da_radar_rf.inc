subroutine da_radar_rf (ra,rs,rg,tc1,ro1,rze)

   !-----------------------------------------------------------------------
   ! Purpose: calculate background/model radar equivalent reflectivity factor
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)    :: ra, rs, rg, tc1, ro1
   real, intent(inout) :: rze

   real                :: zrr, zds, zws, zgr
   real                :: zerr, zews, zeds, zegr

   if (trace_use) call da_trace_entry("da_radar_rf")

   ! ro1 use wrfda value, do not need to scale by 0.001

   zrr = 3.63*1.00e+9  ! rainwater
   zws = 4.26*1.00e+11 ! wet snow
   zds = 9.80*1.00e+8  ! dry snow
   zgr = 4.33*1.00e+10 ! graupel

   zerr = zrr*(ro1*max(0.0,ra))**1.75
   zews = zws*(ro1*max(0.0,rs))**1.75
   zeds = zds*(ro1*max(0.0,rs))**1.75
   zegr = zgr*(ro1*max(0.0,rg))**1.75

   if (tc1.lt.0.0) then
      rze = zerr + zeds + zegr
   else
      rze = zerr + zews + zegr
   end if

   if (rze.lt.1.0e-20) rze=1.0e-20

   if (trace_use) call da_trace_exit("da_radar_rf")

end subroutine da_radar_rf


