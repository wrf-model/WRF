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
       if (tc1.le.0.0) then
       rze = czr*zerr + (1.0-czr)*(zeds+zegr)
       else
       rze = czr*zerr + (1.0-czr)*(zews+zegr)
       end if
   elseif (tc1.lt.-5.0) then
       rze = zeds + zegr
   end if
   !rze = amax1(1.0e-20,rze)
   if (rze.lt.1.0e-20) rze=1.0e-20
   dbz = 10.0*log10(rze)
  end subroutine da_radar_rf

   subroutine da_radar_ze (ra,rs,rg,tc1,ro1,rze)
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
       if (tc1.le.0.0) then
       rze = czr*zerr + (1.0-czr)*(zeds+zegr)
       else
       rze = czr*zerr + (1.0-czr)*(zews+zegr)
       end if
   elseif (tc1.lt.-5.0) then
       rze = zeds + zegr
   end if
  end subroutine da_radar_ze

SUBROUTINE DA_RADAR_ZE_D(ra, rad, rs, rsd, rg, rgd, tc1, ro1, rzed) 
  IMPLICIT NONE
  REAL*8 :: ra, rs, rg, tc1, ro1, rze, dbz
  REAL*8 :: rad, rsd, rgd, rzed, dbzd
  REAL*8 :: zrr, zds, zws, zgr
  REAL*8 :: zerr, zews, zeds, zegr
  REAL*8 :: zerrd, zewsd, zedsd, zegrd
  REAL*8 :: czr
  INTRINSIC LOG10
! Ze=zv*(ro*v)**1.75
! Zdb=10*log10(Ze)
! rainwater
  zrr = 3.63*1.00e+9
! wet snow
  zws = 4.26*1.00e+11
! dry snow
  zds = 9.80*1.00e+8
! grauple
  zgr = 4.33*1.00e+10
  zerrd = zrr*1.75*(ro1*ra)**0.75*ro1*rad
  zewsd = zws*1.75*(ro1*rs)**0.75*ro1*rsd
  zedsd = zds*1.75*(ro1*rs)**0.75*ro1*rsd
  zegrd = zgr*1.75*(ro1*rg)**0.75*ro1*rgd
  IF (tc1 .GE. 5.0) THEN
    rzed = zerrd
  ELSE IF (tc1 .LE. 5.0 .AND. tc1 .GE. -5.0) THEN
    czr = (tc1+5.0)/10.0
       if (tc1.le.0.0) then
       rzed = czr*zerrd + (1.0-czr)*(zedsd+zegrd)
       else
       rzed = czr*zerrd + (1.0-czr)*(zewsd+zegrd)
       end if
  ELSE IF (tc1 .LT. -5.0) THEN
    rzed = zedsd + zegrd
  END IF
END SUBROUTINE DA_RADAR_ZE_D

SUBROUTINE DA_RADAR_ZE_B(ra, rab, rs, rsb, rg, rgb, tc1, ro1, rzeb)
  IMPLICIT NONE
  REAL*8 :: ra, rs, rg, tc1, ro1, rze, dbz
  REAL*8 :: rab, rsb, rgb, rzeb, dbzb
  REAL*8 :: zrr, zds, zws, zgr
  REAL*8 :: zerr, zews, zeds, zegr
  REAL*8 :: zerrb, zewsb, zedsb, zegrb
  REAL*8 :: czr
  INTEGER :: branch
  INTRINSIC LOG10
! Ze=zv*(ro*v)**1.75
! Zdb=10*log10(Ze)
! rainwater
  zrr = 3.63*1.00e+9
! wet snow
  zws = 4.26*1.00e+11
! dry snow
  zds = 9.80*1.00e+8
! grauple
  zgr = 4.33*1.00e+10

  zerrb = 0.0
  zewsb = 0.0
  zedsb = 0.0
  zegrb = 0.0

  IF (tc1 .GE. 5.0) THEN
     zerrb = zerrb + rzeb 
  ELSE IF (tc1 .LE. 5.0 .AND. tc1 .GE. -5.0) THEN
    czr = (tc1+5.0)/10.0
       if (tc1.le.0.0) then
         zerrb = zerrb + czr * rzeb
         zedsb = zedsb + (1.0-czr) * rzeb
         zegrb = zegrb + (1.0-czr) * rzeb
       else
         zerrb = zerrb + czr * rzeb
         zewsb = zewsb + (1.0-czr) * rzeb
         zegrb = zegrb + (1.0-czr) * rzeb
       end if
  ELSE IF (tc1 .LT. -5.0) THEN
    rze = zeds + zegr
    zedsb = zedsb + rzeb
    zegrb = zegrb + rzeb
  END IF

  rgb = 1.75*(ro1*rg)**0.75*zgr*ro1*zegrb
  rsb = 1.75*(ro1*rs)**0.75*zws*ro1*zewsb + 1.75*(ro1*rs)**0.75*zds*ro1*zedsb
  rab = 1.75*(ro1*ra)**0.75*zrr*ro1*zerrb
END SUBROUTINE DA_RADAR_ZE_B
