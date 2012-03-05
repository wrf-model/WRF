!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_sf_sfcdiags

CONTAINS

   SUBROUTINE SFCDIAGS(HFX,QFX,TSK,QSFC,CHS2,CQS2,T2,TH2,Q2,       &
                     PSFC,CP,R_d,ROVCP,                            &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN)                  ::                HFX, &
                                                              QFX, &
                                                              TSK, &
                                                             QSFC
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)               ::                Q2, &
                                                             TH2, &
                                                              T2
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN)                  ::               PSFC, &
                                                             CHS2, &
                                                             CQS2
      REAL,     INTENT(IN   )               ::       CP,R_d,ROVCP
! LOCAL VARS
      INTEGER ::  I,J
      REAL    ::  RHO

      DO J=jts,jte
        DO I=its,ite
          RHO = PSFC(I,J)/(R_d * TSK(I,J))
          if(CQS2(I,J).lt.1.E-5) then
             Q2(I,J)=QSFC(I,J)
          else
             Q2(I,J) = QSFC(I,J) - QFX(I,J)/(RHO*CQS2(I,J))
          endif
          if(CHS2(I,J).lt.1.E-5) then
             T2(I,J) = TSK(I,J) 
          else
             T2(I,J) = TSK(I,J) - HFX(I,J)/(RHO*CP*CHS2(I,J))
          endif
          TH2(I,J) = T2(I,J)*(1.E5/PSFC(I,J))**ROVCP
        ENDDO
      ENDDO

  END SUBROUTINE SFCDIAGS

END MODULE module_sf_sfcdiags
