!WRF:MODEL_LAYER:DYNAMICS
!
MODULE module_damping_em

  USE module_wrf_error

CONTAINS

!------------------------------------------------------------------------------

  SUBROUTINE held_suarez_damp( ru_tend, rv_tend, ru, rv, p, pb,  &
                               ids,ide, jds,jde, kds,kde, &
                               ims,ime, jms,jme, kms,kme, &
                               its,ite, jts,jte, kts,kte )

    IMPLICIT NONE

    INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
    INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
    INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

    REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),          &
          INTENT(INOUT) ::                         ru_tend, &
                                                   rv_tend

    REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),          &
          INTENT(IN) ::                            ru, rv, p, pb

    integer :: i,j,k

    REAL :: delty,delthez,sigb,kka,kkf
    REAL :: sig,sigterm,kkt,kkv,daylensec

    sigb=0.7
    daylensec=60.0*60.0*24.0
    kkf=1.0/daylensec

!  fixed limits so no divide by zero, WCS 070509

    DO j=max(jds+1,jts),min(jde-1,jte)
    DO k=kts,MIN(kte,kde-1)
    DO i=its,ite

       sig=    (p(i,k,j-1)+pb(i,k,j-1)+p(i,k,j)+pb(i,k,j))/     &
               (p(i,1,j-1)+pb(i,1,j-1)+p(i,1,j)+pb(i,1,j))
       sigterm=max(0.0,(sig-sigb)/(1.0-sigb))
       kkv=kkf*sigterm
       rv_tend(i,k,j)=rv_tend(i,k,j)-kkv*rv(i,k,j)
 
    END DO
    END DO
    END DO

    DO j=jts,min(jde-1,jte)
    DO k=kts,MIN(kte,kde-1)
    DO i=its,ite

       sig=    (p(i-1,k,j)+pb(i-1,k,j)+p(i,k,j)+pb(i,k,j))/     &
               (p(i-1,1,j)+pb(i-1,1,j)+p(i,1,j)+pb(i,1,j))
       sigterm=max(0.0,(sig-sigb)/(1.0-sigb))
       kkv=kkf*sigterm
       ru_tend(i,k,j)=ru_tend(i,k,j)-kkv*ru(i,k,j)
 
    END DO
    END DO
    END DO

  END SUBROUTINE held_suarez_damp

!------------------------------------------------------------------------------

END MODULE module_damping_em
