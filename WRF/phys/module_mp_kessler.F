!WRF:MODEL_LAYER:PHYSICS
!

MODULE module_mp_kessler

CONTAINS
!----------------------------------------------------------------
   SUBROUTINE kessler( t, qv, qc, qr, rho, pii                  &
                      ,dt_in, z, xlv, cp                        &
                      ,EP2,SVP1,SVP2,SVP3,SVPT0,rhowater        &
                      ,dz8w                                     &
                      ,RAINNC, RAINNCV                          &
                      ,ids,ide, jds,jde, kds,kde                & ! domain dims
                      ,ims,ime, jms,jme, kms,kme                & ! memory dims
                      ,its,ite, jts,jte, kts,kte                & ! tile   dims
                                                                )
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------
   !  taken from the COMMAS code - WCS 10 May 1999.
   !  converted from FORTRAN 77 to 90, tiled, WCS 10 May 1999.
!----------------------------------------------------------------
   REAL    , PARAMETER ::  c1 = .001 
   REAL    , PARAMETER ::  c2 = .001 
   REAL    , PARAMETER ::  c3 = 2.2 
   REAL    , PARAMETER ::  c4 = .875 
   REAL    , PARAMETER ::  fudge = 1.0 
   REAL    , PARAMETER ::  mxfall = 10.0 
!----------------------------------------------------------------
   INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde, &
                                     ims,ime, jms,jme, kms,kme, &
                                     its,ite, jts,jte, kts,kte
   REAL   ,      INTENT(IN   )    :: xlv, cp
   REAL   ,      INTENT(IN   )    :: EP2,SVP1,SVP2,SVP3,SVPT0
   REAL   ,      INTENT(IN   )    :: rhowater

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(INOUT) ::                                       &
                                                            t , &
                                                            qv, &
                                                            qc, &
                                                            qr

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(IN   ) ::                                       &
                                                           rho, &
                                                           pii, &
                                                          dz8w 

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(IN   ) ::                                    z

   REAL, INTENT(IN   ) :: dt_in

   REAL, DIMENSION( ims:ime , jms:jme ),                        &
         INTENT(INOUT) ::                               RAINNC, &
                                                       RAINNCV



   ! local variables

   REAL :: qrprod, ern, gam, rcgs, rcgsi
   REAL, DIMENSION( its:ite , kts:kte, jts:jte ) ::     prod
   REAL, DIMENSION(kts:kte) :: vt, prodk, vtden,rdzk,rhok,factor,rdzw
   INTEGER :: i,j,k
   INTEGER :: nfall, n, nfall_new
   REAL    :: qrr, pressure, temp, es, qvs, dz, dt
   REAL    :: f5, dtfall, rdz, product
   REAL    :: max_heating, max_condense, max_rain, maxqrp
   REAL    :: vtmax, ernmax, crmax, factorn, time_sediment
   REAL    :: qcr, factorr, ppt
   REAL, PARAMETER :: max_cr_sedimentation = 0.75
!----------------------------------------------------------------

   INTEGER :: imax, kmax

    dt = dt_in

!   f5 = 237.3 * 17.27 * 2.5e6 / cp 
    f5 = svp2*(svpt0-svp3)*xlv/cp
    ernmax = 0.
    maxqrp = -100.

!------------------------------------------------------------------------------
! parameters for the time split terminal advection
!------------------------------------------------------------------------------

      max_heating = 0.
      max_condense = 0.
      max_rain = 0.

!-----------------------------------------------------------------------------
! outer J loop for entire microphysics, outer i loop for sedimentation
!-----------------------------------------------------------------------------

  microphysics_outer_j_loop: DO j = jts, jte

  sedimentation_outer_i_loop: DO i = its,ite

!   vtmax = 0.
   crmax = 0.


!------------------------------------------------------------------------------
! Terminal velocity calculation and advection, set up coefficients and
! compute stable timestep
!------------------------------------------------------------------------------

   DO k = 1, kte
     prodk(k)   = qr(i,k,j)
     rhok(k) = rho(i,k,j)
     qrr = amax1(0.,qr(i,k,j)*0.001*rhok(k))
     vtden(k) = sqrt(rhok(1)/rhok(k))
     vt(k) = 36.34*(qrr**0.1364) * vtden(k)
!     vtmax = amax1(vt(k), vtmax)
     rdzw(k) = 1./dz8w(i,k,j)
     crmax = amax1(vt(k)*dt*rdzw(k),crmax)
   ENDDO
   DO k = 1, kte-1
     rdzk(k) = 1./(z(i,k+1,j) - z(i,k,j))
   ENDDO
   rdzk(kte) = 1./(z(i,kte,j) - z(i,kte-1,j))

   nfall = max(1,nint(0.5+crmax/max_cr_sedimentation))  ! courant number for big timestep.
   dtfall = dt / float(nfall)                           ! splitting so courant number for sedimentation
   time_sediment = dt                                   ! is stable

!------------------------------------------------------------------------------
! Terminal velocity calculation and advection
! Do a time split loop on this for stability.
!------------------------------------------------------------------------------

   column_sedimentation: DO WHILE ( nfall > 0 )

   time_sediment = time_sediment - dtfall
   DO k = 1, kte-1
     factor(k) = dtfall*rdzk(k)/rhok(k)
   ENDDO
   factor(kte) = dtfall*rdzk(kte)

   ppt=0.

      k = 1
      ppt=rhok(k)*prodk(k)*vt(k)*dtfall/rhowater
      RAINNCV(i,j)=ppt*1000.
      RAINNC(i,j)=RAINNC(i,j)+ppt*1000.  ! unit = mm
 
!------------------------------------------------------------------------------
! Time split loop, Fallout done with flux upstream
!------------------------------------------------------------------------------

      DO k = kts, kte-1
        prodk(k) = prodk(k) - factor(k)           &
                  * (rhok(k)*prodk(k)*vt(k)       &
                    -rhok(k+1)*prodk(k+1)*vt(k+1))
      ENDDO

      k = kte
      prodk(k) = prodk(k) - factor(k)*prodk(k)*vt(k)

!------------------------------------------------------------------------------
! compute new sedimentation velocity, and check/recompute new 
! sedimentation timestep if this isn't the last split step.
!------------------------------------------------------------------------------

      IF( nfall > 1 ) THEN ! this wasn't the last split sedimentation timestep

        nfall = nfall - 1
        crmax = 0.
        DO k = kts, kte 
          qrr = amax1(0.,prodk(k)*0.001*rhok(k))
          vt(k) = 36.34*(qrr**0.1364) * vtden(k)
!          vtmax = amax1(vt(k), vtmax)
          crmax = amax1(vt(k)*time_sediment*rdzw(k),crmax)
        ENDDO

        nfall_new = max(1,nint(0.5+crmax/max_cr_sedimentation))
        if (nfall_new /= nfall ) then
          nfall = nfall_new
          dtfall = time_sediment/nfall
        end if

      ELSE  ! this was the last timestep

        DO k=kts,kte
          prod(i,k,j) = prodk(k)
        ENDDO
        nfall = 0  ! exit condition for sedimentation loop

      END IF

   ENDDO column_sedimentation

   ENDDO sedimentation_outer_i_loop

!------------------------------------------------------------------------------
! Production of rain and deletion of qc
! Production of qc from supersaturation
! Evaporation of QR
!------------------------------------------------------------------------------

     DO k = kts, kte
     DO i = its, ite
       factorn = 1.0 / (1.+c3*dt*amax1(0.,qr(i,k,j))**c4)
       qrprod = qc(i,k,j) * (1.0 - factorn)           &
             + factorn*c1*dt*amax1(qc(i,k,j)-c2,0.)      
       rcgs = 0.001*rho(i,k,j)

       qc(i,k,j) = amax1(qc(i,k,j) - qrprod,0.)
       qr(i,k,j) = (qr(i,k,j) + prod(i,k,j)-qr(i,k,j))
       qr(i,k,j) = amax1(qr(i,k,j) + qrprod,0.)

       temp      = pii(i,k,j)*t(i,k,j)
       pressure = 1.000e+05 * (pii(i,k,j)**(1004./287.))
       gam = 2.5e+06/(1004.*pii(i,k,j))
!      qvs       = 380.*exp(17.27*(temp-273.)/(temp- 36.))/pressure
       es        = 1000.*svp1*exp(svp2*(temp-svpt0)/(temp-svp3))
       qvs       = ep2*es/(pressure-es)
!      prod(i,k,j) = (qv(i,k,j)-qvs) / (1.+qvs*f5/(temp-36.)**2)
       prod(i,k,j) = (qv(i,k,j)-qvs) / (1.+pressure/(pressure-es)*qvs*f5/(temp-svp3)**2)
       ern  = amin1(dt*(((1.6+124.9*(rcgs*qr(i,k,j))**.2046)   &
          *(rcgs*qr(i,k,j))**.525)/(2.55e8/(pressure*qvs)       &
          +5.4e5))*(dim(qvs,qv(i,k,j))/(rcgs*qvs)),             &
          amax1(-prod(i,k,j)-qc(i,k,j),0.),qr(i,k,j))

! Update all variables

       product = amax1(prod(i,k,j),-qc(i,k,j))
       t (i,k,j) = t(i,k,j) + gam*(product - ern)
       qv(i,k,j) = amax1(qv(i,k,j) - product + ern,0.)
       qc(i,k,j) =       qc(i,k,j) + product
       qr(i,k,j) = qr(i,k,j) - ern

     ENDDO
     ENDDO

  ENDDO  microphysics_outer_j_loop

  RETURN

  END SUBROUTINE kessler

END MODULE module_mp_kessler
