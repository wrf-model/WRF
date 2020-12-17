MODULE MODULE_MP_MKESSLER
  IMPLICIT NONE

CONTAINS
!
  SUBROUTINE MKESSLER(t, qv, qc, qr, rho, p, pii, dt_in, z, xlv, cp, ep2&
&    , svp1, svp2, svp3, svpt0, rhowater, dz8w, rainnc, rainncv, ids, ide&
&    , jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, &
&    jte, kts, kte)
    IMPLICIT NONE
!----------------------------------------------------------------
! Restructered from WRF Kessler Warm rain process
! H.L. Wang Aug. 1 2009
!----------------------------------------------------------------
    REAL, PARAMETER :: c1=.001
    REAL, PARAMETER :: c2=.001
    REAL, PARAMETER :: c3=2.2
    REAL, PARAMETER :: c4=.875
!----------------------------------------------------------------
    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, ims, ime, jms, &
&    jme, kms, kme, its, ite, jts, jte, kts, kte
    REAL, INTENT(IN) :: xlv, cp
    REAL, INTENT(IN) :: ep2, svp1, svp2, svp3, svpt0
    REAL, INTENT(IN) :: rhowater
    REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: t, qv, &
&    qc, qr
    REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: rho, p, &
&    pii, dz8w
    REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: z
    REAL, INTENT(IN) :: dt_in
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: rainnc, rainncv
! local variables
    REAL :: qrprod, ern, gam, rcgs, rcgsi
    REAL, DIMENSION(its:ite, kts:kte, jts:jte) :: prod
    REAL, DIMENSION(kts:kte) :: vt, prodk, vtden, rdzk, rhok, piik, &
&    factor, rdzw
    INTEGER :: i, j, k
    INTEGER :: nfall, n, nfall_new
    REAL :: qrr, pressure, temp, es, qvs, dz, dt
    REAL :: f5, dtfall, rdz, product
    REAL :: vtmax, crmax, factorn
    REAL :: qcr, factorr, ppt
    REAL, PARAMETER :: max_cr_sedimentation=0.75
!----------------------------------------------------------------
    INTEGER :: imax, kmax
! whl
    REAL, DIMENSION(kts:kte) :: qv1d, qc1d, qr1d, t1d, p1d
    REAL :: dtleft, rainncv0, max_cr
    INTEGER :: kvts, kvte, kn
    dt = dt_in
!  print*,'begin'
!   print*,its,ite,jts,jte
!   print*,ims,ime,jms,jme
!   print*,ids,ide,jds,jde
    f5 = svp2*(svpt0-svp3)*xlv/cp
!   print*,its,ite,jts,jte
!   print*,ims,ime,jms,jme
!   print*,ids,ide,jds,jde
    rdzk = 0.0
    rdzw = 0.0
    DO j=jts,jte
      DO i=its,ite
        DO k=1,kte-1
          rdzk(k) = 1./(z(i, k+1, j)-z(i, k, j))
        END DO
        rdzk(kte) = 1./(z(i, kte, j)-z(i, kte-1, j))
      END DO
    END DO
    DO j=jts,jte
      DO i=its,ite
        DO k=1,kte
          qv1d(k) = qv(i, k, j)
          qc1d(k) = qc(i, k, j)
          qr1d(k) = qr(i, k, j)
          t1d(k) = t(i, k, j)
          p1d(k) = p(i, k, j)
          rhok(k) = rho(i, k, j)
          piik(k) = pii(i, k, j)
          rdzw(k) = 1./dz8w(i, k, j)
        END DO
!   print*,i,j
        kvts = kts
        kvte = kte
        max_cr = max_cr_sedimentation
        dtleft = dt
        CALL SMALLSTEP(qr1d, rdzk, rdzw, rhok, max_cr, dtleft, nfall, &
&                 kvts, kvte)
        dtleft = dt/nfall
        rainncv0 = 0.0
        rainncv(i, j) = 0.0
        DO kn=1,nfall
          CALL RFALL(qr1d, rdzk, rdzw, rhok, rainncv0, rhowater, max_cr&
&               , dtleft, kvts, kvte)
          rainncv(i, j) = rainncv(i, j) + rainncv0
        END DO
!    print*,rainncv0
!autoca(qc1d,qr1d, kvts,kvte,c1,c2,c3,c4,dt )
!autoca(qc1d,qr1d, kvts,kvte,c1,c2,c3,c4,dt )
        rainnc(i, j) = rainnc(i, j) + rainncv(i, j)
!autoca(qc1d,qr1d, kvts,kvte,c1,c2,c3,c4,dt )
        CALL AUTOCA(qc1d, qr1d, kvts, kvte, c1, c2, c3, c4, dt)
!satadj(qv,qc,qr, tmp, pii,rho,  kvts,kvte,xlv, cp,EP2,SVP1,SVP2,SVP3,SVPT0)
        CALL SATADJ(qv1d, qc1d, qr1d, t1d, p1d, piik, rhok, kvts, kvte, &
&              xlv, dt, cp, ep2, svp1, svp2, svp3, svpt0)
        DO k=1,kte
          qv(i, k, j) = qv1d(k)
          qc(i, k, j) = qc1d(k)
          qr(i, k, j) = qr1d(k)
          t(i, k, j) = t1d(k)
        END DO
      END DO
    END DO
! print*,rainncv
    RETURN
  END SUBROUTINE MKESSLER

  SUBROUTINE SMALLSTEP(prodk, rdzk, rdzw, rhok, max_cr, dtleft, nstep, &
&    kvts, kvte)
    IMPLICIT NONE
    INTEGER :: nstep, k, kvts, kvte
    REAL, DIMENSION(kvts:kvte) :: vtden, vt, prodk, factor, rdzk, rdzw, &
&    rhok
    REAL :: max_cr, ppt, dtleft, crmax, qrr
    REAL :: arg1
    INTRINSIC AMAX1
!    INTRINSIC NINT
    INTRINSIC SQRT
    INTRINSIC NINT
    crmax = 0.0
    DO k=kvts,kvte-1
      qrr = prodk(k)*0.001*rhok(k)
      arg1 = rhok(1)/rhok(k)
      vtden(k) = SQRT(arg1)
      IF (qrr/(0.001*rhok(k)) .GE. 1d-5) THEN
        vt(k) = 36.34*qrr**0.1364*vtden(k)
      ELSE
        vt(k) = 0.0
      END IF
      IF (vt(k)*dtleft*rdzw(k) .LT. crmax) THEN
        crmax = crmax
      ELSE
        crmax = vt(k)*dtleft*rdzw(k)
      END IF
    END DO
!    nstep = NINT(0.5 + crmax/max_cr)
    nstep = NINT(0.5 + crmax/0.75)
  END SUBROUTINE SMALLSTEP

  SUBROUTINE RFALL(prodk, rdzk, rdzw, rhok, rainncv0, rhowat, max_cr, &
&    dtfall, kvts, kvte)
    IMPLICIT NONE
    INTEGER :: k, kvts, kvte
    REAL, DIMENSION(kvts:kvte) :: vtden, vt, prodk, factor, rdzk, rdzw, &
&    rhok
    REAL :: rainncv0, rhowat, max_cr, ppt, dtleft
    REAL :: qrr, dtfall
    REAL :: arg1
    INTRINSIC SQRT
    DO k=kvts,kvte
      IF (prodk(k) .LT. 0) prodk(k) = 0.0
    END DO
    DO k=kvts,kvte
      qrr = prodk(k)*0.001*rhok(k)
      arg1 = rhok(1)/rhok(k)
      vtden(k) = SQRT(arg1)
      IF (qrr/( 0.001*rhok(k) ) .GE. 1d-5) THEN
        vt(k) = 36.34*qrr**0.1364*vtden(k)
      ELSE
        vt(k) = 0.0
      END IF
    END DO
    DO k=kvts,kvte-1
      factor(k) = dtfall*rdzk(k)/rhok(k)
    END DO
    factor(kvte) = dtfall*rdzk(kvte)
    ppt = 0.
    k = 1
    ppt = rhok(k)*prodk(k)*vt(k)*dtfall/rhowat
!mm
    rainncv0 = ppt*1000.
!      print*,rainncv0
!------------------------------------------------------------------------------
! Time split loop, Fallout done with flux upstream
!------------------------------------------------------------------------------
    DO k=kvts,kvte-1
      prodk(k) = prodk(k) - factor(k)*(rhok(k)*prodk(k)*vt(k)-rhok(k+1)*&
&        prodk(k+1)*vt(k+1))
    END DO
    k = kvte
    prodk(k) = prodk(k) - factor(k)*prodk(k)*vt(k)
    DO k=kvts,kvte
      IF (prodk(k) .LT. 0) prodk(k) = 0.0
    END DO
  END SUBROUTINE RFALL

  SUBROUTINE AUTOCA(qc1d, qr1d, kvts, kvte, c1, c2, c3, c4, dt)
    IMPLICIT NONE
!     print*,k,qrprod
    INTEGER :: kvts, kvte, k
    REAL, DIMENSION(kvts:kvte) :: qc1d, qr1d
    REAL :: c1, c2, c3, c4
    REAL :: qrrc, dt, factorn, qrprod, qrprod2
    REAL :: pwr1
    qrrc = 1.0e-5
    DO k=kvts,kvte
      IF (qr1d(k) .LT. 0.0) qr1d(k) = 0.0
      IF (qc1d(k) .LT. 0.0) qc1d(k) = 0.0
      IF (qr1d(k) .GE. qrrc) THEN
        pwr1 = qr1d(k)**c4
        factorn = 1.0/(1.+c3*dt*pwr1)
      ELSE
        factorn = 1.0
      END IF
      qrprod = qc1d(k)*(1.0-factorn)
      qrprod2 = 0.0
      IF (qc1d(k) - c2 .GT. 0) THEN
        qrprod2 = factorn*c1*dt*(qc1d(k)-c2)
        IF (qrprod2 .GT. qc1d(k) - c2) qrprod2 = qc1d(k) - c2
      END IF
!        print*,k,qrprod2
      qrprod = qrprod + qrprod2
      IF (qc1d(k) - qrprod .GT. 0) THEN
        qc1d(k) = qc1d(k) - qrprod
        qr1d(k) = qr1d(k) + qrprod
      ELSE
        qc1d(k) = 0.0
        qrprod = qc1d(k)
        qr1d(k) = qr1d(k) + qrprod
      END IF
    END DO
  END SUBROUTINE AUTOCA

  SUBROUTINE SATADJ(qv, qc, qr, tmp, p1d, pii, rhok, kvts, kvte, xlv, dt&
&    , cp, ep2, svp1, svp2, svp3, svpt0)
    IMPLICIT NONE
    INTEGER :: kvts, kvte, k
    REAL, DIMENSION(kvts:kvte) :: qv, qc, qr, tmp, p1d, pii, rhok
    REAL, DIMENSION(kvts:kvte) :: rcgs, pressure, temp, es, qvs
    REAL, DIMENSION(kvts:kvte) :: ern, qv2cl, rn2qv
! local var
    REAL :: svp1, svp2, svp3, svpt0, ep2, xlv, cp, dt, f5
    REAL :: ernmax, product
    REAL :: arg1
    INTRINSIC EXP
    f5 = svp2*(svpt0-svp3)*xlv/cp
    DO k=kvts,kvte
!constant
      rcgs(k) = 0.001*rhok(k)
      pressure(k) = p1d(k)
      temp(k) = pii(k)*tmp(k)
      arg1 = svp2*(temp(k)-svpt0)/(temp(k)-svp3)
      es(k) = 1000.*svp1*EXP(arg1)
      qvs(k) = ep2*es(k)/(pressure(k)-es(k))
      IF (qr(k) .LT. 0) qr(k) = 0.0
      IF (qv(k) .LT. 0) qv(k) = 0.0
      IF (qc(k) .LT. 0) qc(k) = 0.0
    END DO
    DO k=kvts,kvte
!not related to time; maximum transform qv to cl (sat) or cl to qv (sub sat)
      qv2cl(k) = (qv(k)-qvs(k))/(1.+pressure(k)/(pressure(k)-es(k))*qvs(&
&        k)*f5/(temp(k)-svp3)**2)
! sub sat rain evaperate
      rn2qv(k) = 0.0
      ern(k) = 0.0
      IF (qvs(k) .GT. qv(k)) THEN
        IF (qr(k) .GE. 1d-5) THEN
          rn2qv(k) = dt*((1.6+124.9*(rcgs(k)*qr(k))**.2046)*(rcgs(k)*qr(&
&            k))**.525/(2.55e8/(pressure(k)*qvs(k))+5.4e5))*((qvs(k)-qv(k&
&            ))/(rcgs(k)*qvs(k)))
        ELSE
          rn2qv(k) = 0.0
        END IF
        IF (rn2qv(k) .GT. qr(k)) rn2qv(k) = qr(k)
        ernmax = 0.0
        IF (-qv2cl(k) - qc(k) .GT. 0.0) ernmax = -qv2cl(k) - qc(k)
!        ern(k)  = amin1(rn2qv(k), ernmax)
        ern(k) = rn2qv(k)
        IF (rn2qv(k) .GT. ernmax) ern(k) = ernmax
      END IF
! Update all variables
!       product = amax1(qv2cl(k),-qc(k))
      product = qv2cl(k)
      IF (qv2cl(k) .LT. -qc(k)) product = -qc(k)
!       qv(k) = amax1(qv(k) - product + ern(k),0.)
      qv(k) = qv(k) - product + ern(k)
      IF (qv(k) .LT. 0) qv(k) = 0.0
      qc(k) = qc(k) + product
      qr(k) = qr(k) - ern(k)
      temp(k) = temp(k) + xlv/cp*(product-ern(k))
      tmp(k) = temp(k)/pii(k)
    END DO
  END SUBROUTINE SATADJ

END MODULE MODULE_MP_MKESSLER
