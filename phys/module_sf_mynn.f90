

MODULE module_sf_mynn
























































  USE module_model_constants, only: &
       &p1000mb, ep_2


  IMPLICIT NONE













  REAL, PARAMETER :: ep_3=1.-ep_2 
  REAL, PARAMETER :: wmin=0.1    
  REAL, PARAMETER :: VCONVC=1.25
  REAL, PARAMETER :: SNOWZ0=0.011
  REAL, PARAMETER :: COARE_OPT=3.0  
  
  LOGICAL, PARAMETER :: debug_code = .false.

  REAL,   DIMENSION(0:1000 ),SAVE :: psim_stab,psim_unstab, &
                                     psih_stab,psih_unstab

CONTAINS


  SUBROUTINE mynn_sf_init_driver(allowed_to_read)

    LOGICAL, INTENT(in) :: allowed_to_read

    
    
    
    
    

     CALL psi_init

  END SUBROUTINE mynn_sf_init_driver


   SUBROUTINE SFCLAY_mynn(                                  &
              U3D,V3D,T3D,QV3D,P3D,dz8w,                    &
              CP,G,ROVCP,R,XLV,PSFCPA,CHS,CHS2,CQS2,CPM,    &
              ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
              XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
              U10,V10,TH2,T2,Q2,SNOWH,                      &
              GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
              SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
              KARMAN,itimestep,ch,th3d,pi3d,qc3d,rho3d,qcg, &
              spp_pbl,pattern_spp_pbl,                      &
              ids,ide, jds,jde, kds,kde,                    &
              ims,ime, jms,jme, kms,kme,                    &
              its,ite, jts,jte, kts,kte,                    &
              ustm,ck,cka,cd,cda,isftcflx,iz0tlnd           )

      IMPLICIT NONE



























































































      INTEGER,  INTENT(IN)   ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte
      INTEGER,  INTENT(IN)   ::        itimestep
      REAL,     INTENT(IN)   ::        SVP1,SVP2,SVP3,SVPT0
      REAL,     INTENT(IN)   ::        EP1,EP2,KARMAN
      REAL,     INTENT(IN)   ::        CP,G,ROVCP,R,XLV,DX

      INTEGER,  INTENT(IN)   ::        ISFFLX
      INTEGER,  OPTIONAL,  INTENT(IN)   ::     ISFTCFLX, IZ0TLND
      INTEGER,  OPTIONAL,  INTENT(IN)   ::     spp_pbl




      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                           dz8w, &
                                                             QV3D, &
                                                              P3D, &
                                                              T3D, &
                                                             QC3D, &
                                                          U3D,V3D, &
                                                  RHO3D,th3d,pi3d

      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL,      &
                INTENT(IN) ::                      pattern_spp_pbl



      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )               ::             MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK, &
                                                              QCG, &
                                                           PSFCPA ,&
                                                            SNOWH


      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(OUT  )               ::            U10,V10, &
                                                        TH2,T2,Q2

      REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme )              , &
                INTENT(OUT)     ::              ck,cka,cd,cda,ustm

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)               ::             REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                               LH, &
                                                         MOL,RMOL, &
                                                        QSFC, QGH, &
                                                              ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                             CHS2, &
                                                             CQS2, &
                                                              CHS, &
                                                               CH, &
                                                        FLHC,FLQC, &
                                                   GZ1OZ0,WSPD,BR, &
                                                        PSIM,PSIH


      REAL,     DIMENSION( ims:ime, jms:jme )    ::   wstar,qstar



      REAL,     DIMENSION( its:ite ) ::                       U1D, &
                                                              V1D, &
                                                        U1D2,V1D2, & 
                                                             QV1D, &
                                                              P1D, &
                                                         T1D,QC1D, &
                                                            RHO1D, &
                                                           dz8w1d, & 
                                                           dz2w1d    


      REAL,     DIMENSION( its:ite ) ::                  rstoch1D

      INTEGER ::  I,J,K,itf,jtf,ktf


      itf=MIN0(ite,ide-1)
      jtf=MIN0(jte,jde-1)
      ktf=MIN0(kte,kde-1)

      DO J=jts,jte
        DO i=its,ite
           dz8w1d(I) = dz8w(i,kts,j)
           dz2w1d(I) = dz8w(i,kts+1,j)
           U1D(i) =U3D(i,kts,j)
           V1D(i) =V3D(i,kts,j)
           
           U1D2(i) =U3D(i,kts+1,j)
           V1D2(i) =V3D(i,kts+1,j)
           QV1D(i)=QV3D(i,kts,j)
           QC1D(i)=QC3D(i,kts,j)
           P1D(i) =P3D(i,kts,j)
           T1D(i) =T3D(i,kts,j)
           RHO1D(i)=RHO3D(i,kts,j)
           if (spp_pbl==1) then
               rstoch1D(i)=pattern_spp_pbl(i,kts,j)
           else
               rstoch1D(i)=0.0
           endif
        ENDDO

        IF (itimestep==1) THEN
           DO i=its,ite
              UST(i,j)=MAX(0.04*SQRT(U1D(i)*U1D(i) + V1D(i)*V1D(i)),0.001)
              MOL(i,j)=0.     
              QSFC(i,j)=QV3D(i,kts,j)/(1.+QV3D(i,kts,j))
              qstar(i,j)=0.0
           ENDDO
        ENDIF

        CALL SFCLAY1D_mynn(                                        &
                J,U1D,V1D,T1D,QV1D,P1D,dz8w1d,rho1d,               &
                U1D2,V1D2,dz2w1d,                                  &
                CP,G,ROVCP,R,XLV,PSFCPA(ims,j),CHS(ims,j),CHS2(ims,j),&
                CQS2(ims,j),CPM(ims,j),PBLH(ims,j), RMOL(ims,j),   &
                ZNT(ims,j),UST(ims,j),MAVAIL(ims,j),ZOL(ims,j),    &
                MOL(ims,j),REGIME(ims,j),PSIM(ims,j),PSIH(ims,j),  &
                XLAND(ims,j),HFX(ims,j),QFX(ims,j),TSK(ims,j),     &
                U10(ims,j),V10(ims,j),TH2(ims,j),T2(ims,j),        &
                Q2(ims,j),FLHC(ims,j),FLQC(ims,j),SNOWH(ims,j),    &
                QGH(ims,j),QSFC(ims,j),LH(ims,j),                  &
                GZ1OZ0(ims,j),WSPD(ims,j),BR(ims,j),ISFFLX,DX,     &
                SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,               &
                ch(ims,j),qc1d,qcg(ims,j),                         &
                itimestep,                                         &

                wstar(ims,j),qstar(ims,j),                         &

                spp_pbl,rstoch1D,                                  &
                ids,ide, jds,jde, kds,kde,                         &
                ims,ime, jms,jme, kms,kme,                         &
                its,ite, jts,jte, kts,kte                          &
                ,isftcflx,iz0tlnd,                                 &
                USTM(ims,j),CK(ims,j),CKA(ims,j),                  &
                CD(ims,j),CDA(ims,j)                               &
                                                                   )

      ENDDO

    END SUBROUTINE SFCLAY_MYNN


   SUBROUTINE SFCLAY1D_mynn(                                       &
                     J,U1D,V1D,T1D,QV1D,P1D,dz8w1d,rho1d,          &
                     U1D2,V1D2,dz2w1d,                             &
                     CP,G,ROVCP,R,XLV,PSFCPA,CHS,CHS2,CQS2,CPM,    &
                     PBLH,RMOL,ZNT,UST,MAVAIL,ZOL,MOL,REGIME,      &
                     PSIM,PSIH,XLAND,HFX,QFX,TSK,                  &
                     U10,V10,TH2,T2,Q2,FLHC,FLQC,SNOWH,QGH,        &
                     QSFC,LH,GZ1OZ0,WSPD,BR,ISFFLX,DX,             &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                     KARMAN,ch,qc1d,qcg,                           &
                     itimestep,                                    &

                     wstar,qstar,                                  &

                     spp_pbl,rstoch1D,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     &
                     ,isftcflx, iz0tlnd,                           &
                     ustm,ck,cka,cd,cda                            &
                     )


      IMPLICIT NONE



      INTEGER,  INTENT(IN) ::        ids,ide, jds,jde, kds,kde, &
                                     ims,ime, jms,jme, kms,kme, &
                                     its,ite, jts,jte, kts,kte, &
                                     J, itimestep

      REAL,     PARAMETER  :: XKA=2.4E-5   
      REAL,     PARAMETER  :: PRT=1.       
      REAL,     INTENT(IN) :: SVP1,SVP2,SVP3,SVPT0,EP1,EP2
      REAL,     INTENT(IN) :: KARMAN,CP,G,ROVCP,R,XLV,DX




      INTEGER,  INTENT(IN) :: ISFFLX
      INTEGER,  OPTIONAL,  INTENT(IN )   ::     ISFTCFLX, IZ0TLND
      INTEGER,    INTENT(IN)             ::     spp_pbl




      REAL,     DIMENSION( ims:ime ), INTENT(IN)    ::     MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK, &
                                                           PSFCPA, &
                                                              QCG, &
                                                            SNOWH

      REAL,     DIMENSION( its:ite ), INTENT(IN)   ::     U1D,V1D, &
                                                        U1D2,V1D2, &
                                                         QV1D,P1D, &
                                                         T1D,QC1d, &
                                                    dz8w1d,dz2w1d, &
                                                            RHO1D

      REAL,     DIMENSION( ims:ime ), INTENT(INOUT) ::     REGIME, &
                                                       HFX,QFX,LH, &
                                                         MOL,RMOL, &
                                                         QGH,QSFC, &
                                                              ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                        CHS2,CQS2, &
                                                           CHS,CH, &
                                                        FLHC,FLQC, &
                                                           GZ1OZ0, &
                                                             WSPD, &
                                                               BR, &
                                                        PSIM,PSIH
      REAL,     DIMENSION( its:ite ), INTENT(IN)   ::     rstoch1D

      
      REAL,     DIMENSION( ims:ime ), INTENT(OUT)   ::    U10,V10, &
                                                        TH2,T2,Q2

      REAL, OPTIONAL, DIMENSION( ims:ime )                       , &
                INTENT(OUT)     ::              ck,cka,cd,cda,ustm


      REAL,     DIMENSION( ims:ime ) ::                wstar,qstar




      REAL, DIMENSION(its:ite) :: &
                 ZA, &    
                ZA2, &    
              THV1D, &    
               TH1D, &    
               TC1D, &    
               TV1D, &    
               QVSH, &    
              PSIH2, &    
             PSIM10, &    
             PSIH10, &    
              WSPDI, & 
            z_t,z_q, &    
           ZNTstoch, &
             GOVRTH, &    
               THGB, &    
              THVGB, &    
               PSFC, &    
             QSFCMR, &    
             GZ2OZ0, &    
            GZ10OZ0, &    
             GZ2OZt, &    
            GZ10OZt, &    
             GZ1OZt, &    
             zratio       

      INTEGER ::  N,I,K,L,yesno

      REAL    ::  PL,THCON,TVCON,E1
      REAL    ::  DTHVDZ,DTHVM,VCONV,ZOL2,ZOL10,ZOLZA,ZOLZ0
      REAL    ::  DTG,PSIX,DTTHX,DTHDZ,PSIX10,PSIT,PSIT2, &
                  PSIQ,PSIQ2,PSIQ10
      REAL    ::  FLUXC,VSGD
      REAL    ::  restar,VISC,DQG,OLDUST,OLDTST



      DO I=its,ite
         
         
         PSFC(I)=PSFCPA(I)/1000.
         THGB(I)=TSK(I)*(100./PSFC(I))**ROVCP   
         
         PL=P1D(I)/1000.                                                   
         THCON=(100./PL)**ROVCP                                                 
         TH1D(I)=T1D(I)*THCON                   
         TC1D(I)=T1D(I)-273.15                  

         
         QVSH(I)=QV1D(I)/(1.+QV1D(I))        
         TVCON=(1.+EP1*QVSH(I))
         THV1D(I)=TH1D(I)*TVCON                 
         TV1D(I)=T1D(I)*TVCON   

         
         ZA(I)=0.5*dz8w1d(I)             
         ZA2(I)=dz8w1d(I) + 0.5*dz2w1d(I)    
         GOVRTH(I)=G/TH1D(I)
      ENDDO

      DO I=its,ite
         IF (TSK(I) .LT. 273.15) THEN
            
            E1=SVP1*EXP(4648*(1./273.15 - 1./TSK(I)) - &
            & 11.64*LOG(273.15/TSK(I)) + 0.02265*(273.15 - TSK(I)))
         ELSE
            
            E1=SVP1*EXP(SVP2*(TSK(I)-SVPT0)/(TSK(I)-SVP3))
         ENDIF
         
         IF (xland(i).gt.1.5 .or. QSFC(i).le.0.0) THEN   
            QSFC(I)=EP2*E1/(PSFC(I)-ep_3*E1)             
            QSFCMR(I)=EP2*E1/(PSFC(I)-E1)                
         ELSE                                            
            QSFCMR(I)=QSFC(I)/(1.-QSFC(I))
         ENDIF

         
         
         IF (TSK(I) .LT. 273.15) THEN
            
            E1=SVP1*EXP(4648*(1./273.15 - 1./T1D(I)) - &
            &  11.64*LOG(273.15/T1D(I)) + 0.02265*(273.15 - T1D(I)))
         ELSE
            
            E1=SVP1*EXP(SVP2*(T1D(I)-SVPT0)/(T1D(I)-SVP3))
         ENDIF
         PL=P1D(I)/1000.
         
         QGH(I)=EP2*E1/(PL-E1)          
         CPM(I)=CP*(1.+0.84*QV1D(I))
      ENDDO

      DO I=its,ite
         WSPD(I)=SQRT(U1D(I)*U1D(I)+V1D(I)*V1D(I))     

         
         THVGB(I)=THGB(I)*(1.+EP1*QSFC(I))

         DTHDZ=(TH1D(I)-THGB(I))
         DTHVDZ=(THV1D(I)-THVGB(I))

         
         
         
         
         
         
         fluxc = max(hfx(i)/RHO1D(i)/cp                    &
         &    + ep1*THVGB(I)*qfx(i)/RHO1D(i),0.)
         
         IF (xland(i).gt.1.5 .or. QSFC(i).le.0.0) THEN   
            WSTAR(I) = vconvc*(g/TSK(i)*pblh(i)*fluxc)**.33
         ELSE                                            
            
            
            WSTAR(I) = vconvc*(g/TSK(i)*MIN(1.5*pblh(i),4000.)*fluxc)**.33
         ENDIF
         
         
         
         
         VSGD = 0.32 * (max(dx/5000.-1.,0.))**.33
         WSPD(I)=SQRT(WSPD(I)*WSPD(I)+WSTAR(I)*WSTAR(I)+vsgd*vsgd)
         WSPD(I)=MAX(WSPD(I),wmin)

         
         
         
         
         BR(I)=GOVRTH(I)*ZA(I)*DTHVDZ/(WSPD(I)*WSPD(I))
         IF (ITIMESTEP == 1) THEN
            
            BR(I)=MAX(BR(I),-2.0)
            BR(I)=MIN(BR(I),2.0)
         ELSE
           BR(I)=MAX(BR(I),-50.0)
           BR(I)=MIN(BR(I), 50.0)
         ENDIF

         
         
         
         
     
      ENDDO

 1006   format(A,F7.3,A,f9.4,A,f9.5,A,f9.4)
 1007   format(A,F2.0,A,f6.2,A,f7.3,A,f7.2)







 DO I=its,ite

    
    
    VISC=1.326e-5*(1. + 6.542e-3*TC1D(I) + 8.301e-6*TC1D(I)*TC1D(I) &
                      - 4.84e-9*TC1D(I)*TC1D(I)*TC1D(I))

    IF ((XLAND(I)-1.5).GE.0) THEN
       
       
       
       
       
       IF ( PRESENT(ISFTCFLX) ) THEN
          IF ( ISFTCFLX .EQ. 0 ) THEN
             IF (COARE_OPT .EQ. 3.0) THEN
                
                CALL charnock_1955(ZNT(i),UST(i),WSPD(i),visc,ZA(I))
             ELSE
                
                CALL edson_etal_2013(ZNT(i),UST(i),WSPD(i),visc,ZA(I))
             ENDIF
          ELSEIF ( ISFTCFLX .EQ. 1 .OR. ISFTCFLX .EQ. 2 ) THEN
             CALL davis_etal_2008(ZNT(i),UST(i))
          ELSEIF ( ISFTCFLX .EQ. 3 ) THEN
             CALL Taylor_Yelland_2001(ZNT(i),UST(i),WSPD(i))
          ELSEIF ( ISFTCFLX .EQ. 4 ) THEN
             IF (COARE_OPT .EQ. 3.0) THEN
                
                CALL charnock_1955(ZNT(i),UST(i),WSPD(i),visc,ZA(I))
             ELSE
                
                CALL edson_etal_2013(ZNT(i),UST(i),WSPD(i),visc,ZA(I))
             ENDIF
          ENDIF
       ELSE
          
          IF (COARE_OPT .EQ. 3.0) THEN
             
             CALL charnock_1955(ZNT(i),UST(i),WSPD(i),visc,ZA(I))
          ELSE
             
             CALL edson_etal_2013(ZNT(i),UST(i),WSPD(i),visc,ZA(I))
          ENDIF
       ENDIF

       
       if (spp_pbl==1) then
          ZNTstoch(I)  = MAX(ZNT(I) + ZNT(I)*1.0*rstoch1D(i), 1e-6)
       else
          ZNTstoch(I)  = ZNT(I)
       endif

       
       
       
       
       restar=MAX(ust(i)*ZNTstoch(i)/visc, 0.1)

       
       
       
       IF ( PRESENT(ISFTCFLX) ) THEN
          IF ( ISFTCFLX .EQ. 0 ) THEN
             IF (COARE_OPT .EQ. 3.0) THEN
                CALL fairall_etal_2003(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
             ELSE

                CALL fairall_etal_2014(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
             ENDIF
          ELSEIF ( ISFTCFLX .EQ. 1 ) THEN
             IF (COARE_OPT .EQ. 3.0) THEN
                CALL fairall_etal_2003(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
             ELSE
                CALL fairall_etal_2014(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
             ENDIF
          ELSEIF ( ISFTCFLX .EQ. 2 ) THEN
             CALL garratt_1992(z_t(i),z_q(i),ZNTstoch(i),restar,XLAND(I))
          ELSEIF ( ISFTCFLX .EQ. 3 ) THEN
             IF (COARE_OPT .EQ. 3.0) THEN
                CALL fairall_etal_2003(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
             ELSE
                CALL fairall_etal_2014(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
             ENDIF
          ENDIF
       ELSE
          
          IF (COARE_OPT .EQ. 3.0) THEN
             CALL fairall_etal_2003(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
          ELSE
             CALL fairall_etal_2014(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
          ENDIF
       ENDIF
 
    ELSE

       
       if (spp_pbl==1) then
          ZNTstoch(I)  = MAX(ZNT(I) + ZNT(I)*1.0*rstoch1D(i), 1e-6)
       else
          ZNTstoch(I)  = ZNT(I)
       endif

       
       
       
       
       restar=MAX(ust(i)*ZNTstoch(i)/visc, 0.1)

       
       
       
       
       IF ( SNOWH(i) .GE. 0.1) THEN
          CALL Andreas_2002(ZNTSTOCH(i),visc,ust(i),z_t(i),z_q(i))
       ELSE
          IF ( PRESENT(IZ0TLND) ) THEN
             IF ( IZ0TLND .LE. 1 ) THEN
                CALL zilitinkevich_1995(ZNTSTOCH(i),z_t(i),z_q(i),restar,&
                           UST(I),KARMAN,XLAND(I),IZ0TLND,spp_pbl,rstoch1D(i))
             ELSEIF ( IZ0TLND .EQ. 2 ) THEN
                CALL Yang_2008(ZNTSTOCH(i),z_t(i),z_q(i),UST(i),MOL(I),&
                               qstar(I),restar,visc,XLAND(I))
             ELSEIF ( IZ0TLND .EQ. 3 ) THEN
                
                CALL garratt_1992(z_t(i),z_q(i),ZNTSTOCH(i),restar,XLAND(I))
             ENDIF
          ELSE
             
             CALL zilitinkevich_1995(ZNTSTOCH(i),z_t(i),z_q(i),restar,&
                           UST(I),KARMAN,XLAND(I),0,spp_pbl,rstoch1D(i))
          ENDIF
       ENDIF

    ENDIF
    zratio(i)=ZNTstoch(I)/z_t(I)   

    GZ1OZ0(I)= LOG((ZA(I)+ZNTstoch(I))/ZNTstoch(I))
    GZ1OZt(I)= LOG((ZA(I)+z_t(i))/z_t(i))           
    GZ2OZ0(I)= LOG((2.0+ZNTstoch(I))/ZNTstoch(I))                                        
    GZ2OZt(I)= LOG((2.0+z_t(i))/z_t(i))                                        
    GZ10OZ0(I)=LOG((10.+ZNTstoch(I))/ZNTstoch(I)) 
    GZ10OZt(I)=LOG((10.+z_t(i))/z_t(i)) 

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    IF (BR(I) .GT. 0.0) THEN
       IF (BR(I) .GT. 0.2) THEN        
          
          REGIME(I)=1.
       ELSE
          
          REGIME(I)=2.
       ENDIF

       
       IF (itimestep .LE. 1) THEN
          CALL Li_etal_2010(ZOL(I),BR(I),ZA(I)/ZNTstoch(I),zratio(I))
       ELSE
          ZOL(I)=ZA(I)*KARMAN*G*MOL(I)/(TH1D(I)*MAX(UST(I)*UST(I),0.0001))
          ZOL(I)=MAX(ZOL(I),0.0)
          ZOL(I)=MIN(ZOL(I),50.)
       ENDIF

       
       zol(I)=zolri(br(I),ZA(I),ZNTstoch(I),z_t(I),ZOL(I))
       ZOL(I)=MAX(ZOL(I),0.0)
       ZOL(I)=MIN(ZOL(I),50.)

       zolz0 = zol(I)*ZNTstoch(I)/ZA(I)          
       zolza = zol(I)*(za(I)+ZNTstoch(I))/za(I)  
       zol10 = zol(I)*(10.+ZNTstoch(I))/za(I)    
       zol2  = zol(I)*(2.+ZNTstoch(I))/za(I)     

       
       IF ((XLAND(I)-1.5).GE.0) THEN                                            
          
          
          
          
          
          
          
          psim(I)=psim_stable(zolza)-psim_stable(zolz0)
          psih(I)=psih_stable(zolza)-psih_stable(zolz0)
          psim10(I)=psim_stable(zol10)-psim_stable(zolz0)
          psih10(I)=psih_stable(zol10)-psih_stable(zolz0)
          psih2(I)=psih_stable(zol2)-psih_stable(zolz0)
       ELSE
          
          
          
          
          
          
          
          psim(I)=psim_stable(zolza)-psim_stable(zolz0)
          psih(I)=psih_stable(zolza)-psih_stable(zolz0)
          psim10(I)=psim_stable(zol10)-psim_stable(zolz0)
          psih10(I)=psih_stable(zol10)-psih_stable(zolz0)
          psih2(I)=psih_stable(zol2)-psih_stable(zolz0)
       ENDIF

       
       
       
       

       
       RMOL(I)= ZOL(I)/ZA(I)

    ELSEIF(BR(I) .EQ. 0.) THEN                  
       
       
       
       REGIME(I)=3.

       PSIM(I)=0.0
       PSIH(I)=PSIM(I)
       PSIM10(I)=0.
       PSIH10(I)=0.
       PSIH2(I)=0.

       
       IF (UST(I) .LT. 0.01) THEN
          ZOL(I)=BR(I)*GZ1OZ0(I)
       ELSE
          ZOL(I)=KARMAN*GOVRTH(I)*ZA(I)*MOL(I)/(MAX(UST(I)*UST(I),0.001))
       ENDIF
       RMOL(I) = ZOL(I)/ZA(I)

    ELSEIF(BR(I) .LT. 0.)THEN
       
       
       
       REGIME(I)=4.

       
       IF (itimestep .LE. 1) THEN
          CALL Li_etal_2010(ZOL(I),BR(I),ZA(I)/ZNTstoch(I),zratio(I))
       ELSE
          ZOL(I)=ZA(I)*KARMAN*G*MOL(I)/(TH1D(I)*MAX(UST(I)*UST(I),0.001))
          ZOL(I)=MAX(ZOL(I),-50.0)
          ZOL(I)=MIN(ZOL(I),0.0)
       ENDIF

       
       zol(I)=zolri(br(I),ZA(I),ZNTstoch(I),z_t(I),ZOL(I))
       ZOL(I)=MAX(ZOL(I),-50.0)
       ZOL(I)=MIN(ZOL(I),0.0)

       zolz0 = zol(I)*ZNTstoch(I)/ZA(I)           
       zolza = zol(I)*(za(I)+ZNTstoch(I))/za(I)   
       zol10 = zol(I)*(10.+ZNTstoch(I))/za(I)     
       zol2  = zol(I)*(2.+ZNTstoch(I))/za(I)      

       
       IF ((XLAND(I)-1.5).GE.0) THEN                                            
          
          
          
          
          
          
          psim(I)=psim_unstable(zolza)-psim_unstable(zolz0)
          psih(I)=psih_unstable(zolza)-psih_unstable(zolz0)
          psim10(I)=psim_unstable(zol10)-psim_unstable(zolz0)
          psih10(I)=psih_unstable(zol10)-psih_unstable(zolz0)
          psih2(I)=psih_unstable(zol2)-psih_unstable(zolz0)
       ELSE           
          
          
          
          
          
          psim(I)=psim_unstable(zolza)-psim_unstable(zolz0)
          psih(I)=psih_unstable(zolza)-psih_unstable(zolz0)
          psim10(I)=psim_unstable(zol10)-psim_unstable(zolz0)
          psih10(I)=psih_unstable(zol10)-psih_unstable(zolz0)
          psih2(I)=psih_unstable(zol2)-psih_unstable(zolz0)
       ENDIF              

       
       

       
       
       
       PSIH(I)=MIN(PSIH(I),0.9*GZ1OZt(I))
       PSIM(I)=MIN(PSIM(I),0.9*GZ1OZ0(I))
       PSIH2(I)=MIN(PSIH2(I),0.9*GZ2OZt(I))
       PSIM10(I)=MIN(PSIM10(I),0.9*GZ10OZ0(I))
       PSIH10(I)=MIN(PSIH10(I),0.9*GZ10OZt(I))

       RMOL(I) = ZOL(I)/ZA(I)  

    ENDIF

    
    
    
    
    PSIX=GZ1OZ0(I)-PSIM(I)
    PSIX10=GZ10OZ0(I)-PSIM10(I)
    
    OLDUST = UST(I)
    UST(I)=0.5*UST(I)+0.5*KARMAN*WSPD(I)/PSIX 
    
     
    
    WSPDI(I)=MAX(SQRT(U1D(I)*U1D(I)+V1D(I)*V1D(I)), wmin)
    IF ( PRESENT(USTM) ) THEN
       USTM(I)=0.5*USTM(I)+0.5*KARMAN*WSPDI(I)/PSIX
    ENDIF

    IF ((XLAND(I)-1.5).LT.0.) THEN        
       UST(I)=MAX(UST(I),0.005)  
       
       IF ( PRESENT(USTM) ) USTM(I)=UST(I)
    ENDIF

    
    
    
    
    
    GZ1OZt(I)= LOG((ZA(I)+z_t(i))/z_t(i))
    GZ2OZt(I)= LOG((2.0+z_t(i))/z_t(i))                                        

    PSIT =MAX(GZ1OZt(I)-PSIH(I) ,1.)
    PSIT2=MAX(GZ2OZt(I)-PSIH2(I),1.)

    PSIQ=MAX(LOG((ZA(I)+z_q(i))/z_q(I))-PSIH(I) ,1.0)
    PSIQ2=MAX(LOG((2.0+z_q(i))/z_q(I))-PSIH2(I) ,1.0)
    PSIQ10=MAX(LOG((10.0+z_q(i))/z_q(I))-PSIH10(I) ,1.0)

    
    
    
    DTG=THV1D(I)-THVGB(I)
    OLDTST=MOL(I)
    MOL(I)=KARMAN*DTG/PSIT/PRT
    
    
    
    
    DQG=(QVSH(i)-qsfc(i))*1000.   
    qstar(I)=KARMAN*DQG/PSIQ/PRT

    
        
        
        
        
        
        
        
    

 ENDDO     

 1000   format(A,F6.1, A,f6.1, A,f5.1, A,f7.1)
 1001   format(A,F2.0, A,f10.4,A,f5.3, A,f11.5)
 1002   format(A,f7.2, A,f7.2, A,f7.2, A,f10.3)
 1003   format(A,f7.2, A,f7.2, A,f10.3,A,f10.3)
 1004   format(A,f11.3,A,f9.7, A,f9.7, A,f6.2, A,f10.3)
 1005   format(A,f9.2,A,f6.4,A,f7.4,A,f7.4)

      
      
      
 DO I=its,ite

    
    
    PSIX=GZ1OZ0(I)-PSIM(I)
    PSIX10=GZ10OZ0(I)-PSIM10(I)

    PSIT =MAX(GZ1OZt(I)-PSIH(I), 1.0)
    PSIT2=MAX(GZ2OZt(I)-PSIH2(I),1.0)
  
    PSIQ=MAX(LOG((ZA(I)+z_q(i))/z_q(I))-PSIH(I) ,1.0)
    PSIQ2=MAX(LOG((2.0+z_q(i))/z_q(I))-PSIH2(I) ,1.0)
    PSIQ10=MAX(LOG((10.0+z_q(i))/z_q(I))-PSIH10(I) ,1.0)

    IF (ISFFLX .LT. 1) THEN                            

       QFX(i)  = 0.                                                              
       HFX(i)  = 0.    
       FLHC(I) = 0.                                                             
       FLQC(I) = 0.                                                             
       LH(I)   = 0.                                                             
       CHS(I)  = 0.                                                             
       CH(I)   = 0.                                                             
       CHS2(i) = 0.                                                              
       CQS2(i) = 0.                                                              
       IF(PRESENT(ck)  .and. PRESENT(cd) .and. &
         &PRESENT(cka) .and. PRESENT(cda)) THEN
           Ck(I) = 0.
           Cd(I) = 0.
           Cka(I)= 0.
           Cda(I)= 0.
       ENDIF
    ELSE

      
      
      
      
      FLQC(I)=RHO1D(I)*MAVAIL(I)*UST(I)*KARMAN/PSIQ
      FLHC(I)=RHO1D(I)*CPM(I)*UST(I)*KARMAN/PSIT

      
      
      
      QFX(I)=FLQC(I)*(QSFCMR(I)-QV1D(I))
      
      QFX(I)=MAX(QFX(I),-0.02)      
      LH(I)=XLV*QFX(I)

      
      
      
      IF(XLAND(I)-1.5.GT.0.)THEN      
         HFX(I)=FLHC(I)*(THGB(I)-TH1D(I))                                
         IF ( PRESENT(ISFTCFLX) ) THEN
            IF ( ISFTCFLX.NE.0 ) THEN
               
               HFX(I)=HFX(I)+RHO1D(I)*USTM(I)*USTM(I)*WSPDI(I)
            ENDIF
         ENDIF
      ELSEIF(XLAND(I)-1.5.LT.0.)THEN  
         HFX(I)=FLHC(I)*(THGB(I)-TH1D(I))                                
         HFX(I)=MAX(HFX(I),-250.)                                       
      ENDIF

      
      

      CHS(I)=UST(I)*KARMAN/PSIT

      
      

      
      ch(i)=flhc(i)/( cpm(i)*RHO1D(i) )

      
      CQS2(I)=UST(I)*KARMAN/PSIQ2
      CHS2(I)=UST(I)*KARMAN/PSIT2

      IF(PRESENT(ck)  .and. PRESENT(cd) .and. &
        &PRESENT(cka) .and. PRESENT(cda)) THEN
         Ck(I)=(karman/psix10)*(karman/psiq10)
         Cd(I)=(karman/psix10)*(karman/psix10)
         Cka(I)=(karman/psix)*(karman/psiq)
         Cda(I)=(karman/psix)*(karman/psix)
      ENDIF

   ENDIF 

   
   
   
   
   
   
   
   if (ZA(i) .le. 7.0) then
      
      if(ZA2(i) .gt. 7.0 .and. ZA2(i) .lt. 13.0) then
         
         U10(I)=U1D2(I)
         V10(I)=V1D2(I)
      else
         U10(I)=U1D(I)*log(10./ZNTstoch(I))/log(ZA(I)/ZNTstoch(I))
         V10(I)=V1D(I)*log(10./ZNTstoch(I))/log(ZA(I)/ZNTstoch(I))
      endif
   elseif(ZA(i) .gt. 7.0 .and. ZA(i) .lt. 13.0) then
      
      
      
      
      U10(I)=U1D(I)*log(10./ZNTstoch(I))/log(ZA(I)/ZNTstoch(I))
      V10(I)=V1D(I)*log(10./ZNTstoch(I))/log(ZA(I)/ZNTstoch(I))
   else
      
      U10(I)=U1D(I)*PSIX10/PSIX
      V10(I)=V1D(I)*PSIX10/PSIX
   endif

   
   
   
   
   DTG=TH1D(I)-THGB(I) 
   TH2(I)=THGB(I)+DTG*PSIT2/PSIT
   
   
   IF ((TH1D(I)>THGB(I) .AND. (TH2(I)<THGB(I) .OR. TH2(I)>TH1D(I))) .OR. &
       (TH1D(I)<THGB(I) .AND. (TH2(I)>THGB(I) .OR. TH2(I)<TH1D(I)))) THEN
       TH2(I)=THGB(I) + 2.*(TH1D(I)-THGB(I))/ZA(I)
   ENDIF
   T2(I)=TH2(I)*(PSFC(I)/100.)**ROVCP

   Q2(I)=QSFCMR(I)+(QV1D(I)-QSFCMR(I))*PSIQ2/PSIQ
   Q2(I)= MAX(Q2(I), MIN(QSFCMR(I), QV1D(I)))
   Q2(I)= MIN(Q2(I), 1.05*QV1D(I))

   IF ( debug_code ) THEN
      yesno = 0
      IF (HFX(I) > 1200. .OR. HFX(I) < -700.)THEN
            print*,"SUSPICIOUS VALUES IN MYNN SFCLAYER",&
            I,J, "HFX: ",HFX(I)
            yesno = 1
      ENDIF
      IF (LH(I)  > 1200. .OR. LH(I)  < -700.)THEN
            print*,"SUSPICIOUS VALUES IN MYNN SFCLAYER",&
            I,J, "LH: ",LH(I)
            yesno = 1
      ENDIF
      IF (UST(I) < 0.0 .OR. UST(I) > 4.0 )THEN
            print*,"SUSPICIOUS VALUES IN MYNN SFCLAYER",&
            I,J, "UST: ",UST(I)
            yesno = 1
      ENDIF
      IF (WSTAR(I)<0.0 .OR. WSTAR(I) > 6.0)THEN
            print*,"SUSPICIOUS VALUES IN MYNN SFCLAYER",&
            I,J, "WSTAR: ",WSTAR(I)
            yesno = 1
      ENDIF
      IF (RHO1D(I)<0.0 .OR. RHO1D(I) > 1.6 )THEN
            print*,"SUSPICIOUS VALUES IN MYNN SFCLAYER",&
            I,J, "rho: ",RHO1D(I)
            yesno = 1
      ENDIF
      IF (QSFC(I)*1000. <0.0 .OR. QSFC(I)*1000. >40.)THEN
            print*,"SUSPICIOUS VALUES IN MYNN SFCLAYER",&
            I,J, "QSFC: ",QSFC(I)
            yesno = 1
      ENDIF
      IF (PBLH(I)<0. .OR. PBLH(I)>6000.)THEN
            print*,"SUSPICIOUS VALUES IN MYNN SFCLAYER",&
            I,J, "PBLH: ",PBLH(I)
            yesno = 1
      ENDIF

      IF (yesno == 1) THEN
        print*," OTHER INFO:"
        write(*,1001)"REGIME:",REGIME(I)," z/L:",ZOL(I)," U*:",UST(I),&
              " Tstar:",MOL(I)
        write(*,1002)"PSIM:",PSIM(I)," PSIH:",PSIH(I)," W*:",WSTAR(I),&
              " DTHV:",THV1D(I)-THVGB(I)
        write(*,1003)"CPM:",CPM(I)," RHO1D:",RHO1D(I)," L:",&
              ZOL(I)/ZA(I)," DTH:",TH1D(I)-THGB(I)
        write(*,*)" Z0:",ZNTstoch(I)," Zt:",z_t(I)," za:",za(I)
        write(*,1005)"Re:",restar," MAVAIL:",MAVAIL(I)," QSFC(I):",&
              QSFC(I)," QVSH(I):",QVSH(I)
        print*,"PSIX=",PSIX," Z0:",ZNTstoch(I)," T1D(i):",T1D(i)
        write(*,*)"============================================="
      ENDIF
   ENDIF

 ENDDO 

END SUBROUTINE SFCLAY1D_mynn

   SUBROUTINE zilitinkevich_1995(Z_0,Zt,Zq,restar,ustar,KARMAN,&
       & landsea,IZ0TLND2,spp_pbl,rstoch)

       
       
       
       
       
       
       
       

       IMPLICIT NONE
       REAL, INTENT(IN) :: Z_0,restar,ustar,KARMAN,landsea
       INTEGER, OPTIONAL, INTENT(IN)::  IZ0TLND2
       REAL, INTENT(OUT) :: Zt,Zq
       REAL :: CZIL  
                     
                     
       INTEGER,  INTENT(IN)  ::    spp_pbl
       REAL,     INTENT(IN)  ::    rstoch


       IF (landsea-1.5 .GT. 0) THEN    

          
          
          IF (restar .LT. 0.1) THEN
             Zt = Z_0*EXP(KARMAN*2.0)
             Zt = MIN( Zt, 6.0e-5)
             Zt = MAX( Zt, 2.0e-9)
             Zq = Z_0*EXP(KARMAN*3.0)
             Zq = MIN( Zq, 6.0e-5)
             Zq = MAX( Zq, 2.0e-9)
          ELSE
             Zt = Z_0*EXP(-KARMAN*(4.0*SQRT(restar)-3.2))
             Zt = MIN( Zt, 6.0e-5)
             Zt = MAX( Zt, 2.0e-9)
             Zq = Z_0*EXP(-KARMAN*(4.0*SQRT(restar)-4.2))
             Zq = MIN( Zt, 6.0e-5)
             Zq = MAX( Zt, 2.0e-9)
          ENDIF

       ELSE                             

          
          IF ( IZ0TLND2 .EQ. 1 ) THEN
             CZIL = 10.0 ** ( -0.40 * ( Z_0 / 0.07 ) )
          ELSE
             CZIL = 0.085 
          END IF

          Zt = Z_0*EXP(-KARMAN*CZIL*SQRT(restar))
          Zt = MIN( Zt, 0.75*Z_0)

          Zq = Z_0*EXP(-KARMAN*CZIL*SQRT(restar))
          Zq = MIN( Zq, 0.75*Z_0)



          if (spp_pbl==1) then
             Zt = Zt + Zt * 0.5 * rstoch
             Zt = MAX(Zt, 0.0001)
             Zq = Zt
          endif

       ENDIF
                   
       return

   END SUBROUTINE zilitinkevich_1995

   SUBROUTINE davis_etal_2008(Z_0,ustar)

    
    
    
    
    

       IMPLICIT NONE
       REAL, INTENT(IN)  :: ustar
       REAL, INTENT(OUT)  :: Z_0
       REAL :: ZW, ZN1, ZN2
       REAL, PARAMETER :: G=9.81, OZO=1.59E-5

       
       

       ZW  = MIN((ustar/1.06)**(0.3),1.0)
       ZN1 = 0.011*ustar*ustar/G + OZO
       ZN2 = 10.*exp(-9.5*ustar**(-.3333)) + &
             0.11*1.5E-5/AMAX1(ustar,0.01)
       Z_0 = (1.0-ZW) * ZN1 + ZW * ZN2

       Z_0 = MAX( Z_0, 1.27e-7)  
       Z_0 = MIN( Z_0, 2.85e-3)  
                   
       return

   END SUBROUTINE davis_etal_2008

   SUBROUTINE Taylor_Yelland_2001(Z_0,ustar,wsp10)

    
    

       IMPLICIT NONE
       REAL, INTENT(IN)  :: ustar,wsp10
       REAL, INTENT(OUT) :: Z_0
       REAL, parameter  :: g=9.81, pi=3.14159265
       REAL :: hs, Tp, Lp

       
        hs = 0.0248*(wsp10**2.)
       
        Tp = 0.729*MAX(wsp10,0.1)
       
        Lp = g*Tp**2/(2*pi)

       Z_0 = 1200.*hs*(hs/Lp)**4.5
       Z_0 = MAX( Z_0, 1.27e-7)  
       Z_0 = MIN( Z_0, 2.85e-3)  
                   
       return

   END SUBROUTINE Taylor_Yelland_2001

   SUBROUTINE charnock_1955(Z_0,ustar,wsp10,visc,zu)
 

    
    
    

       IMPLICIT NONE
       REAL, INTENT(IN)  :: ustar, visc, wsp10, zu
       REAL, INTENT(OUT) :: Z_0
       REAL, PARAMETER   :: G=9.81, CZO2=0.011
       REAL              :: CZC    
       REAL              :: wsp10m 

       wsp10m = wsp10*log(10./1e-4)/log(zu/1e-4)
       CZC = CZO2 + 0.007*MIN(MAX((wsp10m-10.)/8., 0.), 1.0)

       Z_0 = CZC*ustar*ustar/G + (0.11*visc/MAX(ustar,0.05))
       Z_0 = MAX( Z_0, 1.27e-7)  
       Z_0 = MIN( Z_0, 2.85e-3)  

       return

   END SUBROUTINE charnock_1955

   SUBROUTINE edson_etal_2013(Z_0,ustar,wsp10,visc,zu)


     
     
     

       IMPLICIT NONE
       REAL, INTENT(IN)  :: ustar, visc, wsp10, zu
       REAL, INTENT(OUT) :: Z_0
       REAL, PARAMETER   :: G=9.81
       REAL, PARAMETER   :: m=0.017, b=-0.005
       REAL              :: CZC    
       REAL              :: wsp10m 

       wsp10m = wsp10*log(10/1e-4)/log(zu/1e-4)
       wsp10m = MIN(19., wsp10m)
       CZC    = m*wsp10m + b
       CZC    = MAX(CZC, 0.0)

       Z_0 = CZC*ustar*ustar/G + (0.11*visc/MAX(ustar,0.07))
       Z_0 = MAX( Z_0, 1.27e-7)  
       Z_0 = MIN( Z_0, 2.85e-3)  

       return

   END SUBROUTINE edson_etal_2013

   SUBROUTINE garratt_1992(Zt,Zq,Z_0,Ren,landsea)

    
    
    
    
    
    

       IMPLICIT NONE
       REAL, INTENT(IN)  :: Ren, Z_0,landsea
       REAL, INTENT(OUT) :: Zt,Zq
       REAL :: Rq
       REAL, PARAMETER  :: e=2.71828183

       IF (landsea-1.5 .GT. 0) THEN    

          Zt = Z_0*EXP(2.0 - (2.48*(Ren**0.25)))
          Zq = Z_0*EXP(2.0 - (2.28*(Ren**0.25)))

          Zq = MIN( Zq, 5.5e-5)
          Zq = MAX( Zq, 2.0e-9)
          Zt = MIN( Zt, 5.5e-5)
          Zt = MAX( Zt, 2.0e-9) 
       ELSE                            
          Zq = Z_0/(e**2.)      
          Zt = Zq
       ENDIF
                   
       return

    END SUBROUTINE garratt_1992

    SUBROUTINE fairall_etal_2003(Zt,Zq,Ren,ustar,visc,rstoch,spp_pbl)

    
    
    
    
    
    
    
    
    

       IMPLICIT NONE
       REAL, INTENT(IN)   :: Ren,ustar,visc,rstoch
       INTEGER, INTENT(IN):: spp_pbl
       REAL, INTENT(OUT)  :: Zt,Zq

       IF (Ren .le. 2.) then

          Zt = (5.5e-5)*(Ren**(-0.60))
          Zq = Zt
          
          
          

       ELSE

          
          Zt = (5.5e-5)*(Ren**(-0.60))
          Zq = Zt

       ENDIF

       if (spp_pbl==1) then
          Zt = Zt + Zt * 0.5 * rstoch
          Zq = Zt
       endif

       Zt = MIN(Zt,1.0e-4)
       Zt = MAX(Zt,2.0e-9)

       Zq = MIN(Zt,1.0e-4)
       Zq = MAX(Zt,2.0e-9)

       return

    END SUBROUTINE fairall_etal_2003

    SUBROUTINE fairall_etal_2014(Zt,Zq,Ren,ustar,visc,rstoch,spp_pbl)

    
    
    
    
    

       IMPLICIT NONE
       REAL, INTENT(IN)  :: Ren,ustar,visc,rstoch
       INTEGER, INTENT(IN):: spp_pbl
       REAL, INTENT(OUT) :: Zt,Zq

       
       Zt = MIN(1.6E-4, 5.8E-5/(Ren**0.72))
       Zq = Zt

       IF (spp_pbl ==1) THEN
          Zt = MAX(Zt + Zt*0.5*rstoch,2.0e-9)
          Zq = MAX(Zt + Zt*0.5*rstoch,2.0e-9)
       ELSE
          Zt = MAX(Zt,2.0e-9)
          Zq = MAX(Zt,2.0e-9)
       ENDIF

       return

    END SUBROUTINE fairall_etal_2014

    SUBROUTINE Yang_2008(Z_0,Zt,Zq,ustar,tstar,qst,Ren,visc,landsea)

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

       IMPLICIT NONE
       REAL, INTENT(IN)  :: Z_0, Ren, ustar, tstar, qst, visc, landsea
       REAL              :: ht,     &
                            tstar2, &
                            qstar2, &
                            Z_02,   &
                            Renc2    
       REAL, INTENT(OUT) :: Zt,Zq
       REAL, PARAMETER  :: Renc=300., & 
                           beta=1.5,  & 
                           m=170.,    & 
                           b=691.       

       Z_02 = MIN(Z_0,0.5)
       Z_02 = MAX(Z_02,0.04)
       Renc2= b + m*log(Z_02)
       ht     = Renc2*visc/MAX(ustar,0.01)
       tstar2 = MIN(tstar, 0.0)
       qstar2 = MIN(qst,0.0)

       Zt     = ht * EXP(-beta*(ustar**0.5)*(ABS(tstar2)**1.0))
       Zq     = ht * EXP(-beta*(ustar**0.5)*(ABS(qstar2)**1.0))
       

       Zt = MIN(Zt, Z_0/2.0)
       Zq = MIN(Zq, Z_0/2.0)

       return

    END SUBROUTINE Yang_2008

    SUBROUTINE Andreas_2002(Z_0,bvisc,ustar,Zt,Zq)

    
    
    
    

       IMPLICIT NONE
       REAL, INTENT(IN)  :: Z_0, bvisc, ustar
       REAL, INTENT(OUT) :: Zt, Zq
       REAL :: Ren2, zntsno

       REAL, PARAMETER  :: bt0_s=1.25,  bt0_t=0.149,  bt0_r=0.317,  &
                           bt1_s=0.0,   bt1_t=-0.55,  bt1_r=-0.565, &
                           bt2_s=0.0,   bt2_t=0.0,    bt2_r=-0.183

       REAL, PARAMETER  :: bq0_s=1.61,  bq0_t=0.351,  bq0_r=0.396,  &
                           bq1_s=0.0,   bq1_t=-0.628, bq1_r=-0.512, &
                           bq2_s=0.0,   bq2_t=0.0,    bq2_r=-0.180

      
       zntsno = 0.135*bvisc/ustar + &
               (0.035*(ustar*ustar)/9.8) * &
               (5.*exp(-1.*(((ustar - 0.18)/0.1)*((ustar - 0.18)/0.1))) + 1.)                                                
       Ren2 = ustar*zntsno/bvisc

       
       
       IF (Ren2 .gt. 1000.) Ren2 = 1000. 

       IF (Ren2 .le. 0.135) then

          Zt = zntsno*EXP(bt0_s + bt1_s*LOG(Ren2) + bt2_s*LOG(Ren2)**2)
          Zq = zntsno*EXP(bq0_s + bq1_s*LOG(Ren2) + bq2_s*LOG(Ren2)**2)

       ELSE IF (Ren2 .gt. 0.135 .AND. Ren2 .lt. 2.5) then

          Zt = zntsno*EXP(bt0_t + bt1_t*LOG(Ren2) + bt2_t*LOG(Ren2)**2)
          Zq = zntsno*EXP(bq0_t + bq1_t*LOG(Ren2) + bq2_t*LOG(Ren2)**2)

       ELSE

          Zt = zntsno*EXP(bt0_r + bt1_r*LOG(Ren2) + bt2_r*LOG(Ren2)**2)
          Zq = zntsno*EXP(bq0_r + bq1_r*LOG(Ren2) + bq2_r*LOG(Ren2)**2)

       ENDIF

       return

    END SUBROUTINE Andreas_2002

    SUBROUTINE PSI_Hogstrom_1996(psi_m, psi_h, zL, Zt, Z_0, Za)

    
    

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL, Zt, Z_0, Za
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL  :: x, x0, y, y0, zmL, zhL

       zmL = Z_0*zL/Za  
       zhL = Zt*zL/Za

       IF (zL .gt. 0.) THEN  

          psi_m = -5.3*(zL - zmL)
          psi_h = -8.0*(zL - zhL)
 
       ELSE                 

          x = (1.-19.0*zL)**0.25
          x0= (1.-19.0*zmL)**0.25
          y = (1.-11.6*zL)**0.5
          y0= (1.-11.6*zhL)**0.5

          psi_m = 2.*LOG((1.+x)/(1.+x0)) + &
                    &LOG((1.+x**2.)/(1.+x0**2.)) - &
                    &2.0*ATAN(x) + 2.0*ATAN(x0)
          psi_h = 2.*LOG((1.+y)/(1.+y0))

       ENDIF
                   
       return

    END SUBROUTINE PSI_Hogstrom_1996

    SUBROUTINE PSI_DyerHicks(psi_m, psi_h, zL, Zt, Z_0, Za)

    
    
    
    

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL, Zt, Z_0, Za
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL  :: x, x0, y, y0, zmL, zhL

       zmL = Z_0*zL/Za  
       zhL = Zt*zL/Za   

       IF (zL .gt. 0.) THEN  

          psi_m = -5.0*(zL - zmL)
          psi_h = -5.0*(zL - zhL)
 
       ELSE                 

          x = (1.-16.*zL)**0.25
          x0= (1.-16.*zmL)**0.25

          y = (1.-16.*zL)**0.5
          y0= (1.-16.*zhL)**0.5

          psi_m = 2.*LOG((1.+x)/(1.+x0)) + &
                    &LOG((1.+x**2.)/(1.+x0**2.)) - & 
                    &2.0*ATAN(x) + 2.0*ATAN(x0)
          psi_h = 2.*LOG((1.+y)/(1.+y0))

       ENDIF
                   
       return

    END SUBROUTINE PSI_DyerHicks

    SUBROUTINE PSI_Beljaars_Holtslag_1991(psi_m, psi_h, zL)

    
    
    

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL, PARAMETER  :: a=1., b=0.666, c=5., d=0.35

       IF (zL .lt. 0.) THEN  

          WRITE(*,*)"WARNING: Universal stability functions from"
          WRITE(*,*)"        Beljaars and Holtslag (1991) should only"
          WRITE(*,*)"        be used in the stable regime!"
          psi_m = 0.
          psi_h = 0.
 
       ELSE                 

          psi_m = -(a*zL + b*(zL -(c/d))*exp(-d*zL) + (b*c/d))
          psi_h = -((1.+.666*a*zL)**1.5 + &
                  b*(zL - (c/d))*exp(-d*zL) + (b*c/d) -1.)

       ENDIF
                   
       return

    END SUBROUTINE PSI_Beljaars_Holtslag_1991

    SUBROUTINE PSI_Zilitinkevich_Esau_2007(psi_m, psi_h, zL)

    
    
    
    

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL, PARAMETER  :: Cm=3.0, Ct=2.5

       IF (zL .lt. 0.) THEN  

          WRITE(*,*)"WARNING: Universal stability function from"
          WRITE(*,*)"        Zilitinkevich and Esau (2007) should only"
          WRITE(*,*)"        be used in the stable regime!"
          psi_m = 0.
          psi_h = 0.
 
       ELSE                 

          psi_m = -Cm*(zL**(5./6.))
          psi_h = -Ct*(zL**(4./5.))

       ENDIF
                   
       return

    END SUBROUTINE PSI_Zilitinkevich_Esau_2007

    SUBROUTINE PSI_Businger_1971(psi_m, psi_h, zL)

    
    

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL  :: x, y
       REAL, PARAMETER  ::  Pi180 = 3.14159265/180.

       IF (zL .lt. 0.) THEN  

          x = (1. - 15.0*zL)**0.25
          y = (1. - 9.0*zL)**0.5

          psi_m = LOG(((1.+x)/2.)**2.) + &
                 &LOG((1.+x**2.)/2.) - &
                 &2.0*ATAN(x) + Pi180*90.
          psi_h = 2.*LOG((1.+y)/2.)

       ELSE                 

          psi_m = -4.7*zL
          psi_h = -(4.7/0.74)*zL

       ENDIF
                   
       return

    END SUBROUTINE PSI_Businger_1971

    SUBROUTINE PSI_Suselj_Sood_2010(psi_m, psi_h, zL)

    
    
    
    
    
    

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL, PARAMETER  :: Rfc=0.19, Ric=0.183, PHIT=0.8

       IF (zL .gt. 0.) THEN  

          psi_m = -(zL/Rfc + 1.1223*EXP(1.-1.6666/zL))
          
          
          
          psi_h = -(zL*Ric/((Rfc**2.)*5.) + 7.09*(zL**1.1091))
 
       ELSE                 

          psi_m = 0.9904*LOG(1. - 14.264*zL)
          psi_h = 1.0103*LOG(1. - 16.3066*zL)

       ENDIF
                   
       return

    END SUBROUTINE PSI_Suselj_Sood_2010

    SUBROUTINE PSI_CB2005(psim1,psih1,zL,z0L)

    
    
    

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL,z0L
       REAL, INTENT(OUT) :: psim1,psih1

       psim1 = -6.1*LOG(zL  + (1.+ zL **2.5)**0.4) &
               -6.1*LOG(z0L + (1.+ z0L**2.5)**0.4)
       psih1 = -5.5*LOG(zL  + (1.+ zL **1.1)**0.90909090909) &
               -5.5*LOG(z0L + (1.+ z0L**1.1)**0.90909090909)

       return

    END SUBROUTINE PSI_CB2005

    SUBROUTINE Li_etal_2010(zL, Rib, zaz0, z0zt)

    
    
    

       IMPLICIT NONE
       REAL, INTENT(OUT)  :: zL
       REAL, INTENT(IN) :: Rib, zaz0, z0zt
       REAL :: alfa, beta, zaz02, z0zt2
       REAL, PARAMETER  :: au11=0.045, bu11=0.003, bu12=0.0059, &
                          &bu21=-0.0828, bu22=0.8845, bu31=0.1739, &
                          &bu32=-0.9213, bu33=-0.1057
       REAL, PARAMETER  :: aw11=0.5738, aw12=-0.4399, aw21=-4.901,&
                          &aw22=52.50, bw11=-0.0539, bw12=1.540, &
                          &bw21=-0.669, bw22=-3.282
       REAL, PARAMETER  :: as11=0.7529, as21=14.94, bs11=0.1569,&
                          &bs21=-0.3091, bs22=-1.303
          
       
       zaz02=zaz0
       IF (zaz0 .lt. 100.0) zaz02=100.
       IF (zaz0 .gt. 100000.0) zaz02=100000.

       
       z0zt2=z0zt
       IF (z0zt .lt. 0.5) z0zt2=0.5
       IF (z0zt .gt. 100.0) z0zt2=100.

       alfa = LOG(zaz02)
       beta = LOG(z0zt2)

       IF (Rib .le. 0.0) THEN
          zL = au11*alfa*Rib**2 + (                   &
               &  (bu11*beta + bu12)*alfa**2 +        &
               &  (bu21*beta + bu22)*alfa    +        &
               &  (bu31*beta**2 + bu32*beta + bu33))*Rib
          
          zL = MAX(zL,-15.) 
          zL = MIN(zL,0.)   
       ELSEIF (Rib .gt. 0.0 .AND. Rib .le. 0.2) THEN
          zL = ((aw11*beta + aw12)*alfa +             &
             &  (aw21*beta + aw22))*Rib**2 +          &
             & ((bw11*beta + bw12)*alfa +             &
             &  (bw21*beta + bw22))*Rib
          
          zL = MIN(zL,4.) 
          zL = MAX(zL,0.) 
       ELSE
          zL = (as11*alfa + as21)*Rib + bs11*alfa +   &
             &  bs21*beta + bs22
          
          zL = MIN(zL,20.) 
                           
          zL = MAX(zL,1.)
       ENDIF

       return

    END SUBROUTINE Li_etal_2010

      REAL function zolri(ri,za,z0,zt,zol1)

      
      
      
      
      

      IMPLICIT NONE
      REAL, INTENT(IN) :: ri,za,z0,zt,zol1
      REAL :: x1,x2,fx1,fx2
      INTEGER :: n

      if (ri.lt.0.)then
         x1=zol1 - 0.02  
         x2=0.
      else
         x1=0.
         x2=zol1 + 0.02 
      endif

      n=0
      fx1=zolri2(x1,ri,za,z0,zt)
      fx2=zolri2(x2,ri,za,z0,zt)
      Do While (abs(x1 - x2) > 0.01 .and. n < 5)
        if(abs(fx2).lt.abs(fx1))then
          x1=x1-fx1/(fx2-fx1)*(x2-x1)
          fx1=zolri2(x1,ri,za,z0,zt)
          zolri=x1
        else
          x2=x2-fx2/(fx2-fx1)*(x2-x1)
          fx2=zolri2(x2,ri,za,z0,zt)
          zolri=x2
        endif
        n=n+1
        
      enddo

      if (n==5 .and. abs(x1 - x2) >= 0.01) then
         
         
         
         if (ri.lt.0.)then
            zolri=ri*5.
         else
            zolri=ri*8.
         endif
      
      
      endif

      return
      end function

      REAL function zolri2(zol2,ri2,za,z0,zt)

      
      
      
      
      
      
      
      

      IMPLICIT NONE
      REAL, INTENT(IN) :: ri2,za,z0,zt
      REAL, INTENT(INOUT) :: zol2
      REAL :: zol20,zol3,psim1,psih1,psix2,psit2

      if(zol2*ri2 .lt. 0.)zol2=0.  

      zol20=zol2*z0/za 
      zol3=zol2+zol20  

      if (ri2.lt.0) then
         
         psix2=log((za+z0)/z0)-(psim_unstable(zol3)-psim_unstable(zol20))
         psit2=log((za+zt)/zt)-(psih_unstable(zol3)-psih_unstable(zol20))
         
         
      else
         
         
         psix2=log((za+z0)/z0)-(psim_stable(zol3)-psim_stable(zol20))
         psit2=log((za+zt)/zt)-(psih_stable(zol3)-psih_stable(zol20))
         
         
      endif

      zolri2=zol2*psit2/psix2**2 - ri2

      return
      end function

   SUBROUTINE psi_init

    INTEGER                   ::      N
    REAL                      ::      zolf

    DO N=0,1000
       
       zolf = float(n)*0.01
       psim_stab(n)=psim_stable_full(zolf)
       psih_stab(n)=psih_stable_full(zolf)

       
       zolf = -float(n)*0.01
       psim_unstab(n)=psim_unstable_full(zolf)
       psih_unstab(n)=psih_unstable_full(zolf)
    ENDDO

   END SUBROUTINE psi_init



   REAL function psim_stable_full(zolf)
        REAL :: zolf   

        psim_stable_full=-6.1*log(zolf+(1+zolf**2.5)**(1./2.5))

        return
   end function

   REAL function psih_stable_full(zolf)
        REAL :: zolf

        psih_stable_full=-5.3*log(zolf+(1+zolf**1.1)**(1./1.1))

        return
   end function

   REAL function psim_unstable_full(zolf)
        REAL :: zolf,x,ym,psimc,psimk

        x=(1.-16.*zolf)**.25
        psimk=2*ALOG(0.5*(1+X))+ALOG(0.5*(1+X*X))-2.*ATAN(X)+2.*ATAN(1.)

        ym=(1.-10.*zolf)**0.33
        psimc=(3./2.)*log((ym**2.+ym+1.)/3.)-sqrt(3.)*ATAN((2.*ym+1)/sqrt(3.))+4.*ATAN(1.)/sqrt(3.)

        psim_unstable_full=(psimk+zolf**2*(psimc))/(1+zolf**2.)

        return
   end function

   REAL function psih_unstable_full(zolf)
        REAL :: zolf,y,yh,psihc,psihk

        y=(1.-16.*zolf)**.5
        psihk=2.*log((1+y)/2.)

        yh=(1.-34.*zolf)**0.33
        psihc=(3./2.)*log((yh**2.+yh+1.)/3.)-sqrt(3.)*ATAN((2.*yh+1)/sqrt(3.))+4.*ATAN(1.)/sqrt(3.)

        psih_unstable_full=(psihk+zolf**2*(psihc))/(1+zolf**2.)

        return
   end function



   REAL function psim_stable(zolf)
        integer :: nzol
        real    :: rzol,zolf

        nzol = int(zolf*100.)
        rzol = zolf*100. - nzol
        if(nzol+1 .le. 1000)then
           psim_stable = psim_stab(nzol) + rzol*(psim_stab(nzol+1)-psim_stab(nzol))
        else
           psim_stable = psim_stable_full(zolf)
        endif

      return
   end function

   REAL function psih_stable(zolf)
        integer :: nzol
        real    :: rzol,zolf

        nzol = int(zolf*100.)
        rzol = zolf*100. - nzol
        if(nzol+1 .le. 1000)then
           psih_stable = psih_stab(nzol) + rzol*(psih_stab(nzol+1)-psih_stab(nzol))
        else
           psih_stable = psih_stable_full(zolf)
        endif

      return
   end function

   REAL function psim_unstable(zolf)
        integer :: nzol
        real    :: rzol,zolf

        nzol = int(-zolf*100.)
        rzol = -zolf*100. - nzol
        if(nzol+1 .le. 1000)then
           psim_unstable = psim_unstab(nzol) + rzol*(psim_unstab(nzol+1)-psim_unstab(nzol))
        else
           psim_unstable = psim_unstable_full(zolf)
        endif

      return
   end function

   REAL function psih_unstable(zolf)
        integer :: nzol
        real    :: rzol,zolf

        nzol = int(-zolf*100.)
        rzol = -zolf*100. - nzol
        if(nzol+1 .le. 1000)then
           psih_unstable = psih_unstab(nzol) + rzol*(psih_unstab(nzol+1)-psih_unstab(nzol))
        else
           psih_unstable = psih_unstable_full(zolf)
        endif

      return
   end function


END MODULE module_sf_mynn

