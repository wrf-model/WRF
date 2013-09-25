   subroutine da_radar_rf (ra,rs,rg,tc1,ro1,rze, dbz)
   ! ro1 use wrfda value, doenot need to scale by 0.001
   implicit none
   real*8    :: ra, rs, rg, tc1, ro1, rze, dbz
   real*8    :: zrr, zds, zws, zgr
   real*8    :: zerr, zews, zeds, zegr
   real*8    :: czr
   ! Ze=zv*(ro*v)**1.75
   ! Zdb=10*log10(Ze)
   zrr = 3.63*1.00e+9  ! rainwater
   zws = 4.26*1.00e+11 ! wet snow
   zds = 9.80*1.00e+8  ! dry snow
   zgr = 4.33*1.00e+10 ! grauple

   zerr = zrr*(ro1*ra)**1.75
   zews = zws*(ro1*rs)**1.75
   zeds = zds*(ro1*rs)**1.75
   zegr = zgr*(ro1*rg)**1.75

   !print*,ra,10.0*log10(zerr),10.0*log10(zews),10.0*log10(zeds),10.0*log10(zegr)
   if (tc1.ge.5.0) then
      rze = zerr
   elseif (tc1.le.5.0 .and. tc1 .ge.-5.0) then
       czr = (tc1+5.0)/10.0
       rze = czr*zerr + (1.0-czr)*(zews+zegr)
   elseif (tc1.lt.-5.0) then
       rze = zeds + zegr
   end if
   !rze = amax1(1.0e-20,rze)
   if (rze.lt.1.0e-20) rze=1.0e-20
   dbz = 10.0*log10(rze)
  end subroutine da_radar_rf

