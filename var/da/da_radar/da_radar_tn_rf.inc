SUBROUTINE DA_RADAR_RF_D(ra, rad, rs, rsd, rg, rgd, tc1, ro1, rze, rzed&
&  , dbz, dbzd)
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
  zerr = zrr*(ro1*ra)**1.75
  zewsd = zws*1.75*(ro1*rs)**0.75*ro1*rsd
  zews = zws*(ro1*rs)**1.75
  zedsd = zds*1.75*(ro1*rs)**0.75*ro1*rsd
  zeds = zds*(ro1*rs)**1.75
  zegrd = zgr*1.75*(ro1*rg)**0.75*ro1*rgd
  zegr = zgr*(ro1*rg)**1.75
!print*,ra,10.0*log10(zerr),10.0*log10(zews),10.0*log10(zeds),10.0*log10(zegr)
  IF (tc1 .GE. 5.0) THEN
    rzed = zerrd
    rze = zerr
  ELSE IF (tc1 .LE. 5.0 .AND. tc1 .GE. -5.0) THEN
    czr = (tc1+5.0)/10.0
    rzed = czr*zerrd + (1.0-czr)*(zewsd+zegrd)
    rze = czr*zerr + (1.0-czr)*(zews+zegr)
  ELSE IF (tc1 .LT. -5.0) THEN
    rzed = zedsd + zegrd
    rze = zeds + zegr
  ELSE
    rzed = 0.0_8
  END IF
!rze = amax1(1.0e-20,rze)
  IF (rze .LT. 1.0e-20) THEN
    rze = 1.0e-20
    rzed = 0.0_8
  END IF
  dbzd = 10.0*rzed/(rze*LOG(10.0))
  dbz = 10.0*LOG10(rze)
END SUBROUTINE DA_RADAR_RF_D

SUBROUTINE DA_RADAR_RF_B(ra, rab, rs, rsb, rg, rgb, tc1, ro1, rze, rzeb&
&  , dbz, dbzb)
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
  zerr = zrr*(ro1*ra)**1.75
  zews = zws*(ro1*rs)**1.75
  zeds = zds*(ro1*rs)**1.75
  zegr = zgr*(ro1*rg)**1.75
!print*,ra,10.0*log10(zerr),10.0*log10(zews),10.0*log10(zeds),10.0*log10(zegr)
  IF (tc1 .GE. 5.0) THEN
    rze = zerr
    CALL PUSHCONTROL2B(0)
  ELSE IF (tc1 .LE. 5.0 .AND. tc1 .GE. -5.0) THEN
    czr = (tc1+5.0)/10.0
    rze = czr*zerr + (1.0-czr)*(zews+zegr)
    CALL PUSHCONTROL2B(1)
  ELSE IF (tc1 .LT. -5.0) THEN
    rze = zeds + zegr
    CALL PUSHCONTROL2B(2)
  ELSE
    CALL PUSHCONTROL2B(3)
  END IF
!rze = amax1(1.0e-20,rze)
  IF (rze .LT. 1.0e-20) THEN
    rze = 1.0e-20
    CALL PUSHCONTROL1B(0)
  ELSE
    CALL PUSHCONTROL1B(1)
  END IF
  rzeb = rzeb + 10.0*dbzb/(rze*LOG(10.0))
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) rzeb = 0.0_8
  CALL POPCONTROL2B(branch)
  IF (branch .LT. 2) THEN
    IF (branch .EQ. 0) THEN
      zerrb = rzeb
      rzeb = 0.0_8
      zewsb = 0.0_8
      zegrb = 0.0_8
      zedsb = 0.0_8
    ELSE
      zerrb = czr*rzeb
      zewsb = (1.0-czr)*rzeb
      zegrb = (1.0-czr)*rzeb
      rzeb = 0.0_8
      zedsb = 0.0_8
    END IF
  ELSE
    IF (branch .EQ. 2) THEN
      zedsb = rzeb
      zegrb = rzeb
      rzeb = 0.0_8
    ELSE
      zegrb = 0.0_8
      zedsb = 0.0_8
    END IF
    zerrb = 0.0_8
    zewsb = 0.0_8
  END IF
  rgb = 1.75*(ro1*rg)**0.75*zgr*ro1*zegrb
  rsb = 1.75*(ro1*rs)**0.75*zws*ro1*zewsb + 1.75*(ro1*rs)**0.75*zds*ro1*&
&    zedsb
  rab = 1.75*(ro1*ra)**0.75*zrr*ro1*zerrb
  dbzb = 0.0_8
END SUBROUTINE DA_RADAR_RF_B
