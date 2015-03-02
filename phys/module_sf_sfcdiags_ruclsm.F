!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_sf_sfcdiags_ruclsm

CONTAINS

   SUBROUTINE SFCDIAGS_RUCLSM(HFX,QFX,TSK,QSFC,CHS2,CQS2,T2,TH2,Q2,       &
                     T3D,QV3D,RHO3D,P3D,                           &
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
                INTENT(INOUT)               ::                 Q2, &
                                                              TH2, &
                                                              T2
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN)                  ::               PSFC, &
                                                             CHS2, &
                                                             CQS2
      REAL,    DIMENSION( ims:ime, kms:kme, jms:jme )            , &
               INTENT(IN   )    ::                           QV3D, &
                                                              T3D, &
                                                              P3D, &
                                                            rho3D

      REAL,     INTENT(IN   )               ::       CP,R_d,ROVCP
! LOCAL VARS
      INTEGER ::  I,J
      REAL    ::  RHO, x2m, qlev1, tempc, qsat, p2m


      DO J=jts,jte
        DO I=its,ite
!          RHO = PSFC(I,J)/(R_d * TSK(I,J))
          RHO = RHO3D(i,1,j)
          P2m = PSFC(I,J)*EXP(-0.068283/t3d(i,1,j))

          if(CHS2(I,J).lt.1.E-5) then
!             TH2(I,J) = TSK(I,J)*(1.E5/PSFC(I,J))**ROVCP 
             TH2(I,J) = t3d(i,1,j)*(1.E5/P2m)**ROVCP 
          else
             TH2(I,J) = TSK(I,J)*(1.E5/PSFC(I,J))**ROVCP - HFX(I,J)/(RHO*CP*CHS2(I,J))
!tgs             T2(I,J) = TSK(I,J) - HFX(I,J)/(RHO*CP*CHS2(I,J))
          endif
!tgs             TH2(I,J) = T2(I,J)*(1.E5/PSFC(I,J))**ROVCP
             T2(I,J) = TH2(I,J)*(1.E-5*P2m)**ROVCP
!tgs check that T2 values lie in the range between TSK and T at the 1st level
             x2m     = MAX(MIN(tsk(i,j),t3d(i,1,j)) , t2(i,j))
             t2(i,j) = MIN(MAX(tsk(i,j),t3d(i,1,j)) , x2m)

             TH2(I,J) = T2(I,J)*(1.E5/P2m)**ROVCP

!tgs check that Q2 values in the lie between QSFC and Q at the 1st level
             qlev1 = qv3d(i,1,j)
!tgs saturation check
             tempc=t3d(i,1,j)-273.15
           if (tempc .le. 0.0) then
! qsat - mixing ratio
             qsat = rsif(p3d(i,1,j), t3d(i,1,j))
           else
             qsat = rslf(p3d(i,1,j), t3d(i,1,j))
           endif
             qlev1 = min(qsat, qlev1)

          if(CQS2(I,J).lt.1.E-5) then
!tgs - here Q2 is 2-m water vapor mixing ratio
             Q2(I,J)=qlev1
          else
             x2m = QSFC(I,J) - QFX(I,J)/(RHO*CQS2(I,J))
             Q2(I,J)=x2m/(1.-x2m)
          endif

             x2m     = MAX(MIN(qsfc(i,j)/(1.-qsfc(i,j)),qlev1) , q2(i,j))
             q2(i,j) = MIN(MAX(qsfc(i,j)/(1.-qsfc(i,j)),qlev1) , x2m)
!tgs saturation check
             tempc=t2(i,j)-273.15
           if (tempc .le. 0.0) then
! qsat - mixing ratio
             qsat = rsif(psfc(i,j), t2(i,j))
           else
             qsat = rslf(psfc(i,j), t2(i,j))
           endif
            
             q2(i,j) = min(qsat, q2(i,j))

        ENDDO
      ENDDO

  END SUBROUTINE SFCDIAGS_RUCLSM

!tgs - saturation functions are from Thompson microphysics scheme
      REAL FUNCTION RSLF(P,T)

      IMPLICIT NONE
      REAL, INTENT(IN):: P, T
      REAL:: ESL,X
      REAL, PARAMETER:: C0= .611583699E03
      REAL, PARAMETER:: C1= .444606896E02
      REAL, PARAMETER:: C2= .143177157E01
      REAL, PARAMETER:: C3= .264224321E-1
      REAL, PARAMETER:: C4= .299291081E-3
      REAL, PARAMETER:: C5= .203154182E-5
      REAL, PARAMETER:: C6= .702620698E-8
      REAL, PARAMETER:: C7= .379534310E-11
      REAL, PARAMETER:: C8=-.321582393E-13

      X=MAX(-80.,T-273.16)

!      ESL=612.2*EXP(17.67*X/(T-29.65))
      ESL=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))
      RSLF=.622*ESL/(P-ESL)

      END FUNCTION RSLF
!
!    ALTERNATIVE
!  ; Source: Murphy and Koop, Review of the vapour pressure of ice and
!             supercooled water for atmospheric applications, Q. J. R.
!             Meteorol. Soc (2005), 131, pp. 1539-1565.
!    Psat = EXP(54.842763 - 6763.22 / T - 4.210 * ALOG(T) + 0.000367 * T
!         + TANH(0.0415 * (T - 218.8)) * (53.878 - 1331.22
!         / T - 9.44523 * ALOG(T) + 0.014025 * T))
!
!+---+-----------------------------------------------------------------+
! THIS FUNCTION CALCULATES THE ICE SATURATION VAPOR MIXING RATIO AS A
! FUNCTION OF TEMPERATURE AND PRESSURE
!
      REAL FUNCTION RSIF(P,T)

      IMPLICIT NONE
      REAL, INTENT(IN):: P, T
      REAL:: ESI,X
      REAL, PARAMETER:: C0= .609868993E03
      REAL, PARAMETER:: C1= .499320233E02
      REAL, PARAMETER:: C2= .184672631E01
      REAL, PARAMETER:: C3= .402737184E-1
      REAL, PARAMETER:: C4= .565392987E-3
      REAL, PARAMETER:: C5= .521693933E-5
      REAL, PARAMETER:: C6= .307839583E-7
      REAL, PARAMETER:: C7= .105785160E-9
      REAL, PARAMETER:: C8= .161444444E-12

      X=MAX(-80.,T-273.16)
      ESI=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))
      RSIF=.622*ESI/(P-ESI)

      END FUNCTION RSIF

END MODULE module_sf_sfcdiags_ruclsm
