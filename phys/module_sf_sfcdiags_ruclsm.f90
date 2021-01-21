

MODULE module_sf_sfcdiags_ruclsm

CONTAINS

   SUBROUTINE SFCDIAGS_RUCLSM(HFX,QFX,TSK,QSFC,CQS,CQS2,CHS,CHS2,T2,TH2,Q2,  &
                     T3D,QV3D,RHO3D,P3D,PSFC2D,SNOW,                         &
                     CP,R_d,ROVCP,                                           &
                     ids,ide, jds,jde, kds,kde,                              &
                     ims,ime, jms,jme, kms,kme,                              &        
                     its,ite, jts,jte, kts,kte                     )

      IMPLICIT NONE

      INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN)                  ::                HFX, &
                                                              QFX, &
                                                             SNOW, &
                                                              TSK, &
                                                             QSFC
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)               ::                 Q2, &
                                                              TH2, &
                                                               T2
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN)                  ::                     &
                                                           PSFC2D, &
                                                              CHS, &
                                                              CQS, &
                                                             CHS2, &
                                                             CQS2
      REAL,    DIMENSION( ims:ime, kms:kme, jms:jme )            , &
               INTENT(IN   )    ::                           QV3D, &
                                                              T3D, &
                                                              P3D, &
                                                            rho3D

      REAL,     INTENT(IN   )               ::       CP,R_d,ROVCP

      INTEGER ::  I,J
      REAL    ::  RHO, x2m, qlev1, tempc, qsat, p2m, qsfcprox, qsfcmr, &
                  psfc, dT, dQ, fh, fac, dz1

      LOGICAL :: FLUX


      flux = .false.

      DO J=jts,jte
        DO I=its,ite
          RHO = RHO3D(i,1,j)


          PSFC = PSFC2D(I,J)


    if ( flux ) then

           if(CHS2(I,J).lt.1.E-5) then




             TH2(I,J) = t3d(i,1,j)*(1.E5/PSFC)**ROVCP 
          else
             TH2(I,J) = TSK(I,J)*(1.E5/PSFC)**ROVCP - HFX(I,J)/(RHO*CP*CHS2(I,J))

          endif

             T2(I,J) = TH2(I,J)*(1.E-5*PSFC)**ROVCP

             x2m     = MAX(MIN(tsk(i,j),t3d(i,1,j)) , t2(i,j))
             t2(i,j) = MIN(MAX(tsk(i,j),t3d(i,1,j)) , x2m)
             TH2(I,J) = T2(I,J)*(1.E5/PSFC)**ROVCP
    else
             fac=(1.E5/PSFC)**ROVCP
             TH2(I,J) = tsk(i,j)*fac - CHS(I,J)/CHS2(I,J)*(tsk(i,j) - t3d(i,1,j))*fac
             T2(I,J) = TH2(I,J)*(1.E-5*PSFC)**ROVCP
    endif 



             qlev1 = qv3d(i,1,j)

             tempc=t3d(i,1,j)-273.15
           if (tempc .le. 0.0) then

             qsat = rsif(p3d(i,1,j), t3d(i,1,j))
           else
             qsat = rslf(p3d(i,1,j), t3d(i,1,j))
           endif

             qlev1 = min(qsat, qlev1)




             qsfcprox=qlev1+QFX(I,J)/(RHO*CQS(I,J))
             qsfcmr = qsfc(i,j)/(1.-qsfc(i,j))







    if ( flux ) then
          if(CQS2(I,J).lt.1.E-5) then

             Q2(I,J)=qlev1
          else

             x2m = QSFCprox - QFX(I,J)/(RHO*CQS2(I,J))
             q2(i,j) = x2m
          endif
    else

            Q2(I,J) = qsfcmr - CQS(I,J)/CQS2(I,J)*(qsfcmr - qlev1)
    endif  


             x2m     = MAX(MIN(qsfcmr,qlev1) , q2(i,j))
             q2(i,j) = MIN(MAX(qsfcmr,qlev1) , x2m)


             tempc=t2(i,j)-273.15
           if (tempc .le. 0.0) then

             qsat = rsif(psfc, t2(i,j))
           else

             qsat = rslf(psfc, t2(i,j))
           endif
            
             q2(i,j) = min(qsat, q2(i,j))







        ENDDO
      ENDDO

  END SUBROUTINE SFCDIAGS_RUCLSM


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


      ESL=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))
      RSLF=.622*ESL/(P-ESL)

      END FUNCTION RSLF













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
