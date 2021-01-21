

MODULE module_sf_pxsfclay

 REAL    , PARAMETER ::  RICRIT = 0.25            
 REAL    , PARAMETER ::  BETAH  = 5.0    
 REAL    , PARAMETER ::  BETAM  = 5.0    
 REAL    , PARAMETER ::  BM     = 13.0
 REAL    , PARAMETER ::  BH     = 15.7
 REAL    , PARAMETER ::  GAMAM  = 19.3
 REAL    , PARAMETER ::  GAMAH  = 11.6
 REAL    , PARAMETER ::  PR0    = 0.95
 REAL    , PARAMETER ::  CZO    = 0.032
 REAL    , PARAMETER ::  OZO    = 1.E-4
 REAL    , PARAMETER ::  VCONVC = 1.0


CONTAINS


   SUBROUTINE PXSFCLAY(U3D,V3D,T3D,TH3D,QV3D,P3D,dz8w,             &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,                                      &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,          &
                     itimestep,                                    &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

      IMPLICIT NONE






























































































      INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte

      INTEGER,  INTENT(IN )   ::        ISFFLX, ITIMESTEP
      REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
      REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                           dz8w
                                        
      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                           QV3D, &
                                                              P3D, &
                                                              T3D, &
                                                             TH3D

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )               ::             MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(OUT  )               ::                U10, &
                                                              V10
                                                             

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)               ::             REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                               LH, &
                                                        MOL,RMOL,QSFC


      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                        PSIM,PSIH

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                            U3D, &
                                                              V3D
                                        
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )               ::               PSFC

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                            ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                             CHS2, &
                                                             CQS2, &
                                                              CHS

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                      FLHC,FLQC

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                                 &
                                                              QGH


                                    
      REAL,     INTENT(IN   )                  ::   CP,G,ROVCP,R,XLV,DX
 


      REAL,     DIMENSION( its:ite ) ::                       U1D, &
                                                              V1D, &
                                                             QV1D, &
                                                              P1D, &
                                                              T1D, &
                                                             TH1D

      REAL,     DIMENSION( its:ite ) ::                    dz8w1d

      INTEGER ::  I,J

      DO J=jts,jte
   
        DO i=its,ite
          dz8w1d(i) =dz8w(i,1,j)
          U1D(i)    =U3D(i,1,j)
          V1D(i)    =V3D(i,1,j)
          QV1D(i)   =QV3D(i,1,j)
          P1D(i)    =P3D(i,1,j)
          T1D(i)    =T3D(i,1,j)
          TH1D(i)   =TH3D(i,1,j)
        ENDDO
        
        
        
        
        CALL PXSFCLAY1D(J,U1D,V1D,T1D,TH1D,QV1D,P1D,dz8w1d,          &
                CP,G,ROVCP,R,XLV,PSFC(ims,j),CHS(ims,j),CHS2(ims,j), &
                CQS2(ims,j),CPM(ims,j),PBLH(ims,j), RMOL(ims,j),   &
                ZNT(ims,j),UST(ims,j),MAVAIL(ims,j),ZOL(ims,j),    &
                MOL(ims,j),REGIME(ims,j),PSIM(ims,j),PSIH(ims,j),  &
                XLAND(ims,j),HFX(ims,j),QFX(ims,j),TSK(ims,j),     &
                U10(ims,j),V10(ims,j),                             &
                FLHC(ims,j),FLQC(ims,j),QGH(ims,j),                &
                QSFC(ims,j),LH(ims,j),                             &
                GZ1OZ0(ims,j),WSPD(ims,j),BR(ims,j),ISFFLX,DX,     &
                SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,               &
                itimestep,                                         &
                ids,ide, jds,jde, kds,kde,                         &
                ims,ime, jms,jme, kms,kme,                         &
                its,ite, jts,jte, kts,kte                          )
      ENDDO


   END SUBROUTINE PXSFCLAY

   SUBROUTINE PXSFCLAY1D(J,US,VS,T1D,THETA1,QV1D,P1D,dz8w1d,                &
                     CP,G,ROVCP,R,XLV,PSFCPA,CHS,CHS2,CQS2,CPM,PBLH,RMOL, &
                     ZNT,UST,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH,      &
                     XLAND,HFX,QFX,TG,                             &
                     U10,V10,FLHC,FLQC,QGH,                        &
                     QSFC,LH,GZ1OZ0,WSPD,BR,ISFFLX,DX,             &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,          &
                     itimestep,                                    &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

      IMPLICIT NONE

      REAL,     PARAMETER     ::        XKA=2.4E-5
      REAL,     PARAMETER     ::        PRT=1.

      INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, &
                                        J

      INTEGER,  INTENT(IN )   ::        ISFFLX, ITIMESTEP
      REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
      REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN


      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(IN   )               ::             MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TG

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(IN   )               ::             PSFCPA

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)               ::             REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                         MOL,RMOL


      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                        PSIM,PSIH

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                            ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                             CHS2, &
                                                             CQS2, &
                                                              CHS

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                      FLHC,FLQC

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                                 &
                                                              QGH,QSFC

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(OUT)     ::                        U10,V10, &
                                                          LH
                                    
      REAL,     INTENT(IN   )                    ::   CP,G,ROVCP,XLV,DX,R


      REAL,     DIMENSION( its:ite ),  INTENT(IN   )   ::  dz8w1d

      REAL,     DIMENSION( its:ite ),  INTENT(IN   )   ::      US, &
                                                               VS, &
                                                             QV1D, &
                                                              P1D, &
                                                              T1D, &
                                                           THETA1
 


      REAL,     DIMENSION( its:ite )        ::                 ZA, &
                                                              TH0, &
                                                           THETAG, &
                                                               WS, &
                                                            RICUT, &
                                                             USTM, &
                                                               RA, &
                                                          THETAV1, &
                                                         MOLENGTH

      REAL,     DIMENSION( its:ite )        ::                     &
                                                      RHOX,GOVRTH  

      REAL,     DIMENSION( its:ite )        ::               PSFC

      INTEGER                               ::                 KL

      INTEGER ::  N,I,K,KK,L,NZOL,NK,NZOL2,NZOL10

      REAL    ::  PL,THCON,TVCON,E1
      REAL    ::  ZL,TSKV,DTHVDZ,DTHVM,VCONV,RZOL,RZOL2,RZOL10,ZOL2,ZOL10
      REAL    ::  DTG,PSIX,DTTHX,PSIX10,PSIT,PSIT2,PSIQ,PSIQ2
      REAL    ::  FLUXC,VSGD
      REAL    ::  XMOL,ZOBOL,Z10OL,ZNTOL,YNT,YOB,X1,X2
      REAL    ::  G2OZ0,G10OZ0,RA2,ZOLL
      REAL    ::  TV0,CPOT,RICRITI,AM,AH,SQLNZZ0,RBH,RBW,TSTV
      REAL    ::  PSIH2, PSIM2, PSIH10, PSIM10, CQS,TMPVTCON,TST,QST



      DO i = its,ite

        PSFC(I)   = PSFCPA(I)/1000.
        TVCON     = 1.0 + EP1 * QV1D(I)
        THETAV1(I)= THETA1(I) * TVCON
        RHOX(I)   = PSFCPA(I)/(R*T1D(I)*TVCON)
      ENDDO




      DO I=its,ite
        IF (TG(I) .LT. 273.15) THEN
           
           E1=SVP1*EXP(4648*(1./273.15 - 1./TG(I)) - &
           & 11.64*LOG(273.15/TG(I)) + 0.02265*(273.15 - TG(I)))
        ELSE
           
           E1=SVP1*EXP( SVP2*(TG(I)-SVPT0)/(TG(I)-SVP3) )  
        ENDIF

        IF (xland(i).gt.1.5 .or. QSFC(i).le.0.0.or.itimestep.eq.1) THEN                     
           QSFC(I)=EP2*E1/(PSFC(I)-E1)
        ENDIF
        


        E1=SVP1*EXP(SVP2*(T1D(I)-SVPT0)/(T1D(I)-SVP3))  
        PL = P1D(I)/1000.                     
        QGH(I)=EP2*E1/(PL-E1)                                                 
        CPM(I)=CP*(1.+0.8*QV1D(I))                                   
      ENDDO                                                                   


      DO I = its, ite

        TV0       = TG(I) * (1.0 + EP1 * QSFC(I))
        CPOT      = (100./PSFC(I))**ROVCP 
        TH0(I)    = TV0 * CPOT
        THETAG(I) = CPOT * TG(I)
      ENDDO






      DO I = its,ite
        ZA(I) = 0.5 * DZ8W1D(I)                                
        WS(I)   = SQRT(US(I) * US(I) + VS(I) * VS(I))
      ENDDO                                                                   




        RICRITI = 1.0 / RICRIT

      DO i = its,ite
        GZ1OZ0(I) = ALOG(ZA(I) / ZNT(I))
        DTHVDZ      = THETAV1(I) - TH0(I)
        fluxc = max(hfx(i)/rhox(i)/cp                    &
              + ep1*TH0(I)*qfx(i)/rhox(i),0.)
        VCONV = vconvc*(g/tg(i)*pblh(i)*fluxc)**.33
        VSGD = 0.32 * (max(dx/5000.-1.,0.))**.33
        WSPD(I)=SQRT(WS(I)*WS(I)+VCONV*VCONV+vsgd*vsgd)
        WSPD(I)   = AMAX1(WSPD(I),0.1)
        GOVRTH(I)   = G / THETA1(I)
        BR(I)     = GOVRTH(I) * ZA(I) * DTHVDZ / (WSPD(I) * WSPD(I))
        RICUT(I)    = 1.0 / (RICRITI + GZ1OZ0(I))
      ENDDO

      DO I = its,ite

        ZOLL = 0.0
        IF (BR(I) .GE. RICUT(I)) THEN

            REGIME(I) = 1.0
            ZOLL      = BR(I) * GZ1OZ0(I) / (1.0 - RICRITI * RICUT(I))
            PSIM(I)   = 1.0 - BETAM - ZOLL
            PSIH(I)   = 1.0 - BETAH - ZOLL

        ELSE IF (BR(I) .GE. 0.0) THEN

            REGIME(I) = 2.0
            ZOLL      = BR(I) * GZ1OZ0(I) / (1.0 - RICRITI * BR(I))
            PSIM(I)   = -BETAM * ZOLL
            PSIH(I)   = -BETAH * ZOLL

        ELSE


            REGIME(I) = 3.0   
            AM          = 0.031 + 0.276 * ALOG(GZ1OZ0(I))
            AH          = 0.04 + 0.355 * ALOG(GZ1OZ0(I))
            SQLNZZ0     = SQRT(GZ1OZ0(I))
            PSIM(I)   = AM * ALOG(1.0 - BM * SQLNZZ0 * BR(I))
            PSIH(I)   = AH * ALOG(1.0 - BH * SQLNZZ0 * BR(I))

        ENDIF
      ENDDO


      DO I = its,ite
        DTG       = THETA1(I) - THETAG(I)
        PSIX      = GZ1OZ0(I) - PSIM(I)
        UST(I)=0.5*UST(I)+0.5*KARMAN*WSPD(I)/PSIX                                             
        USTM(I) = UST(I)



        IF ((XLAND(I)-1.5) .GE. 0.0) THEN
          ZNT(I) = CZO * USTM(I) * USTM(I) / G + OZO
          GZ1OZ0(I) = ALOG(ZA(I) / ZNT(I))
          PSIX      = GZ1OZ0(I) - PSIM(I)
          UST(I)  = KARMAN * WSPD(I) / PSIX
          USTM(I) = UST(I)
        ENDIF 

        RA(I)  = PR0 * (GZ1OZ0(I) - PSIH(I)) / (KARMAN * UST(I))
        RBH    = 5.0 / UST(I)                

        RBW    = 4.503/UST(I)                                       
        CHS(I) = 1./(RA(I) + RBH)
        CQS    = 1./(RA(I) + RBW)
        MOL(I) = DTG * CHS(I) / UST(I)
        TMPVTCON  = 1.0 + EP1 * QV1D(i)
        TST = DTG * CHS(I)/UST(i)
        TSTV  = (THETAV1(I) - TH0(I)) * CHS(I) / UST(I) 
        IF (ABS(TSTV) .LT. 1.E-5)  TSTV = 1.E-5 
        MOLENGTH(I) = THETAV1(I) * UST(I) * UST(I) / (KARMAN * G * TSTV)


        XMOL = MOLENGTH(I)
        IF(MOLENGTH(I).GT.0.0) XMOL = AMAX1(MOLENGTH(I),2.0)       
        RMOL(I) = 1/XMOL                                           
        ZOL(I)   = ZA(I)*RMOL(I)                                                        
        ZOBOL = 1.5*RMOL(I)  
        Z10OL = 10.0*RMOL(I)                                                      
        ZNTOL = ZNT(I)*RMOL(I)                                                  
        IF(XMOL.LT.0.0) THEN
          YNT = ( 1.0 - GAMAH * ZNTOL )**0.5
          YOB = ( 1.0 - GAMAH * ZOBOL )**0.5
          PSIH2 =  2. * ALOG((YOB+1.0)/(YNT+1.0))
          x1   = (1.0 - gamam * z10ol)**0.25
          x2   = (1.0 - gamam * zntol)**0.25
          psim10 = 2.0 * ALOG( (1.0+x1) / (1.0+x2) ) +        &
                         ALOG( (1.0+x1*x1) / (1.0+x2*x2)) -   &
                         2.0 * ATAN(x1) + 2.0 * ATAN(x2)
        ELSE 
          IF((ZOBOL-ZNTOL).LE.1.0) THEN                                       
            PSIH2 = -BETAH*(ZOBOL-ZNTOL)                                     
          ELSE                                                                
            PSIH2 = 1.-BETAH-(ZOBOL-ZNTOL)                                   
          ENDIF                                                               
          IF((Z10OL-ZNTOL).LE.1.0) THEN                                       
            PSIM10 = -BETAM*(Z10OL-ZNTOL)                                     
          ELSE                                                                
            PSIM10 = 1.-BETAM-(Z10OL-ZNTOL)                                   
          ENDIF                                                               
        ENDIF 
        G2OZ0  = ALOG(1.5 / ZNT(I))
        G10OZ0 = ALOG(10.0 / ZNT(I))
        RA2  = PR0 * (G2OZ0 - PSIH2) / (KARMAN * UST(I))
        CHS2(I) = 1.0/(RA2 + RBH)
        CQS2(I) = 1.0/(RA2 + RBW) 
        U10(I)  = US(I)*(G10OZ0-PSIM10)/PSIX                                    
        V10(I)  = VS(I)*(G10OZ0-PSIM10)/PSIX                                            


        FLHC(i)=CPM(I)*RHOX(I)*CHS(I)
        FLQC(i)=RHOX(I)*CQS*MAVAIL(I)
        QFX(I)=FLQC(I)*(QSFC(I)-QV1D(I))                                     
        QFX(I)=AMAX1(QFX(I),0.)                                            
        LH(I)=XLV*QFX(I)
        IF(XLAND(I)-1.5.GT.0.)THEN                                           
          HFX(I)=-FLHC(I)*DTG                               
        ELSEIF(XLAND(I)-1.5.LT.0.)THEN                                       
          HFX(I)=-FLHC(I)*DTG                               
          HFX(I)=AMAX1(HFX(I),-250.)                                       
        ENDIF      
      ENDDO                                           


   END SUBROUTINE PXSFCLAY1D


   SUBROUTINE pxsfclayinit( allowed_to_read )         

   LOGICAL , INTENT(IN)      ::      allowed_to_read
   INTEGER                   ::      N
   REAL                      ::      ZOLN,X,Y


   END SUBROUTINE pxsfclayinit



END MODULE module_sf_pxsfclay
