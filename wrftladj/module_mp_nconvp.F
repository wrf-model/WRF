!WRF:MODEL_LAYER:PHYSICS
!

MODULE module_mp_nconvp

CONTAINS
!----------------------------------------------------------------
   SUBROUTINE lscond( th, p, qv, rho, pii                       &
                      ,r_v, xlv, cp                             &
                      ,EP2,SVP1,SVP2,SVP3,SVPT0                 &
                      ,dz8w                                     &
                      ,RAINNC, RAINNCV                          &
                      ,ids,ide, jds,jde, kds,kde                & ! domain dims
                      ,ims,ime, jms,jme, kms,kme                & ! memory dims
                      ,its,ite, jts,jte, kts,kte                & ! tile   dims
                                                                )
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------
   !  based on MM5 code (JD November 2006)
!----------------------------------------------------------------
   INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde, &
                                     ims,ime, jms,jme, kms,kme, &
                                     its,ite, jts,jte, kts,kte
   REAL   ,      INTENT(IN   )    :: r_v, xlv, cp
   REAL   ,      INTENT(IN   )    :: EP2,SVP1,SVP2,SVP3,SVPT0

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(INOUT) ::                                       &
                                                            th, &
                                                            qv

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(IN   ) ::                                       &
                                                           rho, &
                                                           pii, &
                                                           p  , &
                                                          dz8w 


   REAL, DIMENSION( ims:ime , jms:jme ),                        &
         INTENT(INOUT) ::                               RAINNC, &
                                                       RAINNCV

   ! local variables

   REAL :: TTEMP, ES1, QS1, DQV, COND, R1
   INTEGER :: i,j,k
!----------------------------------------------------------------

   DO j = jts, jte
   DO k = kts, kte
   DO i = its,ite
      TTEMP = PII(I,K,J)*TH(I,K,J)
      ES1 = 1000.*SVP1*EXP(SVP2*(TTEMP-SVPT0)/(TTEMP-SVP3))
      QS1 = EP2*ES1/(P(I,K,J)-ES1)
      DQV = QV(I,K,J) - QS1
      IF(DQV.GT.0.0)THEN
        R1 = 1. + XLV*XLV/(R_V*CP)*QS1/(TTEMP*TTEMP)
        COND = DQV/R1
        QV(I,K,J) = QV(I,K,J) - COND
        TTEMP = TTEMP + XLV/CP*COND
        TH(I,K,J) = TTEMP/PII(I,K,J)
        RAINNCV(I,J) = RHO(I,K,J)*COND*DZ8W(I,K,J)
        RAINNC(I,J) = RAINNC(I,J) + RHO(I,K,J)*COND*DZ8W(I,K,J)
      ENDIF
   ENDDO
   ENDDO
   ENDDO
  RETURN

  END SUBROUTINE lscond


END MODULE module_mp_nconvp
