!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_sf_sfclay

 REAL    , PARAMETER ::  VCONVC=1.
 REAL    , PARAMETER ::  CZO=0.032
 REAL    , PARAMETER ::  OZO=1.E-4

 REAL,   DIMENSION(0:1000 ),SAVE          :: PSIMTB,PSIHTB

CONTAINS

!-------------------------------------------------------------------
   SUBROUTINE SFCLAY(U3D,V3D,T3D,QV3D,P3D,dz8w,                    &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,TH2,T2,Q2,                            &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                     KARMAN,EOMEG,STBOLT,                          &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
!-- U3D         3D u-velocity interpolated to theta points (m/s)
!-- V3D         3D v-velocity interpolated to theta points (m/s)
!-- T3D         temperature (K)
!-- QV3D        3D water vapor mixing ratio (Kg/Kg)
!-- P3D         3D pressure (Pa)
!-- dz8w        dz between full levels (m)
!-- CP          heat capacity at constant pressure for dry air (J/kg/K)
!-- G           acceleration due to gravity (m/s^2)
!-- ROVCP       R/CP
!-- R           gas constant for dry air (J/kg/K)
!-- XLV         latent heat of vaporization for water (J/kg)
!-- PSFC        surface pressure (Pa)
!-- ZNT         roughness length (m)
!-- UST         u* in similarity theory (m/s)
!-- PBLH        PBL height from previous time (m)
!-- MAVAIL      surface moisture availability (between 0 and 1)
!-- ZOL         z/L height over Monin-Obukhov length
!-- MOL         T* (similarity theory) (K)
!-- REGIME      flag indicating PBL regime (stable, unstable, etc.)
!-- PSIM        similarity stability function for momentum
!-- PSIH        similarity stability function for heat
!-- XLAND       land mask (1 for land, 2 for water)
!-- HFX         upward heat flux at the surface (W/m^2)
!-- QFX         upward moisture flux at the surface (kg/m^2/s)
!-- LH          net upward latent heat flux at surface (W/m^2)
!-- TSK         surface temperature (K)
!-- FLHC        exchange coefficient for heat (m/s)
!-- FLQC        exchange coefficient for moisture (m/s)
!-- QGH         lowest-level saturated mixing ratio
!-- U10         diagnostic 10m u wind
!-- V10         diagnostic 10m v wind
!-- TH2         diagnostic 2m theta (K)
!-- T2          diagnostic 2m temperature (K)
!-- Q2          diagnostic 2m mixing ratio (kg/kg)
!-- GZ1OZ0      log(z/z0) where z0 is roughness length
!-- WSPD        wind speed at lowest model level (m/s)
!-- BR          bulk Richardson number in surface layer
!-- ISFFLX      isfflx=1 for surface heat and moisture fluxes
!-- DX          horizontal grid size (m)
!-- SVP1        constant for saturation vapor pressure (kPa)
!-- SVP2        constant for saturation vapor pressure (dimensionless)
!-- SVP3        constant for saturation vapor pressure (K)
!-- SVPT0       constant for saturation vapor pressure (K)
!-- EP1         constant for virtual temperature (R_v/R_d - 1) (dimensionless)
!-- EP2         constant for specific humidity calculation 
!               (R_d/R_v) (dimensionless)
!-- KARMAN      Von Karman constant
!-- EOMEG       angular velocity of earth's rotation (rad/s)
!-- STBOLT      Stefan-Boltzmann constant (W/m^2/K^4)
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!-------------------------------------------------------------------
      INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte
!                                                               
      INTEGER,  INTENT(IN )   ::        ISFFLX
      REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
      REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN,EOMEG,STBOLT
!
      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                           dz8w
                                        
      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                           QV3D, &
                                                              P3D, &
                                                              T3D

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )               ::             MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(OUT  )               ::                U10, &
                                                              V10, &
                                                              TH2, &
                                                               T2, &
                                                               Q2, &
                                                             QSFC
!
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)               ::             REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                               LH, &
                                                          MOL,RMOL
!m the following 5 are change to memory size
!
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


                                    
      REAL,     INTENT(IN   )               ::   CP,G,ROVCP,R,XLV,DX
 
! LOCAL VARS

      REAL,     DIMENSION( its:ite ) ::                       U1D, &
                                                              V1D, &
                                                             QV1D, &
                                                              P1D, &
                                                              T1D

      REAL,     DIMENSION( its:ite ) ::                    dz8w1d

      INTEGER ::  I,J

      DO J=jts,jte
        DO i=its,ite
          dz8w1d(I) = dz8w(i,1,j)
        ENDDO
   
        DO i=its,ite
           U1D(i) =U3D(i,1,j)
           V1D(i) =V3D(i,1,j)
           QV1D(i)=QV3D(i,1,j)
           P1D(i) =P3D(i,1,j)
           T1D(i) =T3D(i,1,j)
        ENDDO

        CALL SFCLAY1D(J,U1D,V1D,T1D,QV1D,P1D,dz8w1d,               &
                CP,G,ROVCP,R,XLV,PSFC(ims,j),CHS(ims,j),CHS2(ims,j),&
                CQS2(ims,j),CPM(ims,j),PBLH(ims,j), RMOL(ims,j),   &
                ZNT(ims,j),UST(ims,j),MAVAIL(ims,j),ZOL(ims,j),    &
                MOL(ims,j),REGIME(ims,j),PSIM(ims,j),PSIH(ims,j),  &
                XLAND(ims,j),HFX(ims,j),QFX(ims,j),TSK(ims,j),     &
                U10(ims,j),V10(ims,j),TH2(ims,j),T2(ims,j),        &
                Q2(ims,j),FLHC(ims,j),FLQC(ims,j),QGH(ims,j),      &
                QSFC(ims,j),LH(ims,j),                             &
                GZ1OZ0(ims,j),WSPD(ims,j),BR(ims,j),ISFFLX,DX,     &
                SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,EOMEG,STBOLT,  &
                ids,ide, jds,jde, kds,kde,                         &
                ims,ime, jms,jme, kms,kme,                         &
                its,ite, jts,jte, kts,kte                          )
      ENDDO


   END SUBROUTINE SFCLAY


!-------------------------------------------------------------------
   SUBROUTINE SFCLAY1D(J,UX,VX,T1D,QV1D,P1D,dz8w1d,                &
                     CP,G,ROVCP,R,XLV,PSFCPA,CHS,CHS2,CQS2,CPM,PBLH,RMOL, &
                     ZNT,UST,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH,      &
                     XLAND,HFX,QFX,TSK,                            &
                     U10,V10,TH2,T2,Q2,FLHC,FLQC,QGH,              &
                     QSFC,LH,GZ1OZ0,WSPD,BR,ISFFLX,DX,             &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                     KARMAN,EOMEG,STBOLT,                          &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      REAL,     PARAMETER     ::        XKA=2.4E-5
      REAL,     PARAMETER     ::        PRT=1.

      INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, &
                                        J
!                                                               
      INTEGER,  INTENT(IN )   ::        ISFFLX
      REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
      REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN,EOMEG,STBOLT

!
      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(IN   )               ::             MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK
!
      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(IN   )               ::             PSFCPA

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)               ::             REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                         MOL,RMOL
!m the following 5 are changed to memory size---
!
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
                                                              QGH

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(OUT)     ::                        U10,V10, &
                                                TH2,T2,Q2,QSFC,LH
                                    
      REAL,     INTENT(IN   )               ::   CP,G,ROVCP,R,XLV,DX

! MODULE-LOCAL VARIABLES, DEFINED IN SUBROUTINE SFCLAY
      REAL,     DIMENSION( its:ite ),  INTENT(IN   )   ::  dz8w1d

      REAL,     DIMENSION( its:ite ),  INTENT(IN   )   ::      UX, &
                                                               VX, &
                                                             QV1D, &
                                                              P1D, &
                                                              T1D
 
! LOCAL VARS

      REAL,     DIMENSION( its:ite )        ::                 ZA, &
                                                        THVX,ZQKL, &
                                                           ZQKLP1, &
                                                           THX,QX, &
                                                            PSIH2, &
                                                            PSIM2, &
                                                            PSIH10, &
                                                            PSIM10, &
                                                           GZ2OZ0, &
                                                           GZ10OZ0
!
      REAL,     DIMENSION( its:ite )        ::                     &
                                                      RHOX,GOVRTH, &
                                                            TGDSA
!
      REAL,     DIMENSION( its:ite)         ::          SCR3,SCR4
      REAL,     DIMENSION( its:ite )        ::         THGB, PSFC
!
      INTEGER                               ::                 KL

      INTEGER ::  N,I,K,KK,L,NZOL,NK,NZOL2,NZOL10

      REAL    ::  PL,THCON,TVCON,E1
      REAL    ::  ZL,TSKV,DTHVDZ,DTHVM,VCONV,RZOL,RZOL2,RZOL10,ZOL2,ZOL10
      REAL    ::  DTG,PSIX,USTM,DTTHX,PSIX10,PSIT,PSIT2,PSIQ,PSIQ2
      REAL    ::  FLUXC,VSGD
!-------------------------------------------------------------------
      KL=kte

      DO i=its,ite
! PSFC cmb
         PSFC(I)=PSFCPA(I)/1000.
      ENDDO
!                                                      
!----CONVERT GROUND TEMPERATURE TO POTENTIAL TEMPERATURE:  
!                                                            
      DO 5 I=its,ite                                   
        TGDSA(I)=TSK(I)                                    
! PSFC cmb
        THGB(I)=TSK(I)*(100./PSFC(I))**ROVCP                
    5 CONTINUE                                               
!                                                            
!-----DECOUPLE FLUX-FORM VARIABLES TO GIVE U,V,T,THETA,THETA-VIR.,
!     T-VIR., QV, AND QC AT CROSS POINTS AND AT KTAU-1.  
!                                                                 
!     *** NOTE ***                                           
!         THE BOUNDARY WINDS MAY NOT BE ADEQUATELY AFFECTED BY FRICTION,         
!         SO USE ONLY INTERIOR VALUES OF UX AND VX TO CALCULATE 
!         TENDENCIES.                             
!                                                           
   10 CONTINUE                                                     

!     DO 24 I=its,ite
!        UX(I)=U1D(I)
!        VX(I)=V1D(I)
!  24 CONTINUE                                             
                                                             
   26 CONTINUE                                               
                                                   
!.....SCR3(I,K) STORE TEMPERATURE,                           
!     SCR4(I,K) STORE VIRTUAL TEMPERATURE.                                       
                                                                                 
      DO 30 I=its,ite
! PL cmb
         PL=P1D(I)/1000.
         SCR3(I)=T1D(I)                                                   
         THCON=(100./PL)**ROVCP                                                 
         THX(I)=SCR3(I)*THCON                                               
         SCR4(I)=SCR3(I)                                                    
         THVX(I)=THX(I)                                                     
         QX(I)=0.                                                             
   30 CONTINUE                                                                 
!                                                                                
      DO I=its,ite
         QGH(I)=0.                                                                
         FLHC(I)=0.                                                               
         FLQC(I)=0.                                                               
         CPM(I)=CP                                                                
      ENDDO
!                                                                                
!     IF(IDRY.EQ.1)GOTO 80                                                   
      DO 50 I=its,ite
         QX(I)=QV1D(I)                                                    
         TVCON=(1.+EP1*QX(I))                                      
         THVX(I)=THX(I)*TVCON                                               
         SCR4(I)=SCR3(I)*TVCON                                              
   50 CONTINUE                                                                 
!                                                                                
      DO 60 I=its,ite
        E1=SVP1*EXP(SVP2*(TGDSA(I)-SVPT0)/(TGDSA(I)-SVP3))                       
        QSFC(I)=EP2*E1/(PSFC(I)-E1)                                                 
! QGH CHANGED TO USE LOWEST-LEVEL AIR TEMP CONSISTENT WITH MYJSFC CHANGE
! Q2SAT = QGH IN LSM
        E1=SVP1*EXP(SVP2*(T1D(I)-SVPT0)/(T1D(I)-SVP3))                       
        QGH(I)=EP2*E1/(PSFC(I)-E1)                                                 
        CPM(I)=CP*(1.+0.8*QX(I))                                   
   60 CONTINUE                                                                   
   80 CONTINUE
                                                                                 
!-----COMPUTE THE HEIGHT OF FULL- AND HALF-SIGMA LEVELS ABOVE GROUND             
!     LEVEL, AND THE LAYER THICKNESSES.                                          
                                                                                 
      DO 90 I=its,ite
        ZQKLP1(I)=0.
        RHOX(I)=PSFC(I)*1000./(R*SCR4(I))                                       
   90 CONTINUE                                                                   
!                                                                                
      DO 110 I=its,ite                                                   
           ZQKL(I)=dz8w1d(I)+ZQKLP1(I)
  110 CONTINUE                                                                 
!                                                                                
      DO 120 I=its,ite
         ZA(I)=0.5*(ZQKL(I)+ZQKLP1(I))                                        
  120 CONTINUE                                                                 
!                                                                                
      DO 160 I=its,ite
        GOVRTH(I)=G/THX(I)                                                    
  160 CONTINUE                                                                   
                                                                                 
!-----CALCULATE BULK RICHARDSON NO. OF SURFACE LAYER, ACCORDING TO               
!     AKB(1976), EQ(12).                                                         
                   
      DO 260 I=its,ite
        GZ1OZ0(I)=ALOG(ZA(I)/ZNT(I))                                        
        GZ2OZ0(I)=ALOG(2./ZNT(I))                                        
        GZ10OZ0(I)=ALOG(10./ZNT(I))                                        
        IF((XLAND(I)-1.5).GE.0)THEN                                            
          ZL=ZNT(I)                                                            
        ELSE                                                                     
          ZL=0.01                                                                
        ENDIF                                                                    
        WSPD(I)=SQRT(UX(I)*UX(I)+VX(I)*VX(I))                        

        TSKV=THGB(I)*(1.+EP1*QSFC(I)*MAVAIL(I))                     
        DTHVDZ=(THVX(I)-TSKV)                                                 
!  Convective velocity scale Vc and subgrid-scale velocity Vsg
!  following Beljaars (1995, QJRMS) and Mahrt and Sun (1995, MWR)
!                                ... HONG Aug. 2001
!
!       VCONV = 0.25*sqrt(g/tskv*pblh(i)*dthvm)
        fluxc = max(hfx(i)/rhox(i)/cp                    &
              + ep1*tskv*qfx(i)/rhox(i),0.)
        VCONV = vconvc*(g/tgdsa(i)*pblh(i)*fluxc)**.33
!       IF(-DTHVDZ.GE.0)THEN
!         DTHVM=-DTHVDZ
!       ELSE
!         DTHVM=0.
!       ENDIF
!       VCONV = max(vconv,VCONVC*SQRT(DTHVM))
! VCONV comes from Beljaars only
        VSGD = 0.32 * (max(dx/5000.-1.,0.))**.33
        WSPD(I)=SQRT(WSPD(I)*WSPD(I)+VCONV*VCONV+vsgd*vsgd)
        WSPD(I)=AMAX1(WSPD(I),0.1)
        BR(I)=GOVRTH(I)*ZA(I)*DTHVDZ/(WSPD(I)*WSPD(I))                        
!  IF PREVIOUSLY UNSTABLE, DO NOT LET INTO REGIMES 1 AND 2
        IF(MOL(I).LT.0.)BR(I)=AMIN1(BR(I),0.0)
  260 CONTINUE                                                                   

!                                                                                
!-----DIAGNOSE BASIC PARAMETERS FOR THE APPROPRIATED STABILITY CLASS:            
!                                                                                
!                                                                                
!     THE STABILITY CLASSES ARE DETERMINED BY BR (BULK RICHARDSON NO.)           
!     AND HOL (HEIGHT OF PBL/MONIN-OBUKHOV LENGTH).                              
!                                                                                
!     CRITERIA FOR THE CLASSES ARE AS FOLLOWS:                                   
!                                                                                
!        1. BR .GE. 0.2;                                                         
!               REPRESENTS NIGHTTIME STABLE CONDITIONS (REGIME=1),               
!                                                                                
!        2. BR .LT. 0.2 .AND. BR .GT. 0.0;                                       
!               REPRESENTS DAMPED MECHANICAL TURBULENT CONDITIONS                
!               (REGIME=2),                                                      
!                                                                                
!        3. BR .EQ. 0.0                                                          
!               REPRESENTS FORCED CONVECTION CONDITIONS (REGIME=3),              
!                                                                                
!        4. BR .LT. 0.0                                                          
!               REPRESENTS FREE CONVECTION CONDITIONS (REGIME=4).                
!                                                                                
!CCCCC                                                                           

      DO 320 I=its,ite
!CCCCC                                                                           
!CC     REMOVE REGIME 3 DEPENDENCE ON PBL HEIGHT                                 
!CC          IF(BR(I).LT.0..AND.HOL(I,J).GT.1.5)GOTO 310                         
        IF(BR(I).LT.0.)GOTO 310                                                  
!                                                                                
!-----CLASS 1; STABLE (NIGHTTIME) CONDITIONS:                                    
!                                                                                
        IF(BR(I).LT.0.2)GOTO 270                                                 
        REGIME(I)=1.                                                           
        PSIM(I)=-10.*GZ1OZ0(I)                                                   
!    LOWER LIMIT ON PSI IN STABLE CONDITIONS                                     
        PSIM(I)=AMAX1(PSIM(I),-10.)                                              
        PSIH(I)=PSIM(I)                                                          
        PSIM10(I)=10./ZA(I)*PSIM(I)
        PSIM10(I)=AMAX1(PSIM10(I),-10.)                               
        PSIH10(I)=PSIM10(I)                                          
        PSIM2(I)=2./ZA(I)*PSIM(I)
        PSIM2(I)=AMAX1(PSIM2(I),-10.)                              
        PSIH2(I)=PSIM2(I)                                         

        GOTO 320                                                                 
!                                                                                
!-----CLASS 2; DAMPED MECHANICAL TURBULENCE:                                     
!                                                                                
  270   IF(BR(I).EQ.0.0)GOTO 280                                                 
        REGIME(I)=2.                                                           
        PSIM(I)=-5.0*BR(I)*GZ1OZ0(I)/(1.1-5.0*BR(I))                             
!    LOWER LIMIT ON PSI IN STABLE CONDITIONS                                     
        PSIM(I)=AMAX1(PSIM(I),-10.)                                              
!.....AKB(1976), EQ(16).                                                         
        PSIH(I)=PSIM(I)                                                          
        PSIM10(I)=10./ZA(I)*PSIM(I)
        PSIM10(I)=AMAX1(PSIM10(I),-10.)                               
        PSIH10(I)=PSIM10(I)                                          
        PSIM2(I)=2./ZA(I)*PSIM(I)
        PSIM2(I)=AMAX1(PSIM2(I),-10.)                              
        PSIH2(I)=PSIM2(I)                                         
        GOTO 320                                                                 
!                                                                                
!-----CLASS 3; FORCED CONVECTION:                                                
!                                                                                
  280   REGIME(I)=3.                                                           
        PSIM(I)=0.0                                                              
        PSIH(I)=PSIM(I)                                                          
        PSIM10(I)=0.                                                   
        PSIH10(I)=PSIM10(I)                                           
        PSIM2(I)=0.                                                  
        PSIH2(I)=PSIM2(I)                                           

                                                                                 
        IF(UST(I).LT.0.01)THEN                                                 
          ZOL(I)=BR(I)*GZ1OZ0(I)                                               
        ELSE                                                                     
          ZOL(I)=KARMAN*GOVRTH(I)*ZA(I)*MOL(I)/(UST(I)*UST(I)) 
        ENDIF                                                                    

        GOTO 320                                                                 
!                                                                                
!-----CLASS 4; FREE CONVECTION:                                                  
!                                                                                
  310   CONTINUE                                                                 
        REGIME(I)=4.                                                           
        IF(UST(I).LT.0.01)THEN                                                 
          ZOL(I)=BR(I)*GZ1OZ0(I)                                               
        ELSE                                                                     
          ZOL(I)=KARMAN*GOVRTH(I)*ZA(I)*MOL(I)/(UST(I)*UST(I))
        ENDIF                                                                    
        ZOL10=10./ZA(I)*ZOL(I)                                    
        ZOL2=2./ZA(I)*ZOL(I)                                     
        ZOL(I)=AMIN1(ZOL(I),0.)                                              
        ZOL(I)=AMAX1(ZOL(I),-9.9999)                                         
        ZOL10=AMIN1(ZOL10,0.)                                          
        ZOL10=AMAX1(ZOL10,-9.9999)                                    
        ZOL2=AMIN1(ZOL2,0.)                                          
        ZOL2=AMAX1(ZOL2,-9.9999)                                    
        NZOL=INT(-ZOL(I)*100.)                                                 
        RZOL=-ZOL(I)*100.-NZOL                                                 
        NZOL10=INT(-ZOL10*100.)                                        
        RZOL10=-ZOL10*100.-NZOL10                                     
        NZOL2=INT(-ZOL2*100.)                                        
        RZOL2=-ZOL2*100.-NZOL2                                      
        PSIM(I)=PSIMTB(NZOL)+RZOL*(PSIMTB(NZOL+1)-PSIMTB(NZOL))                  
        PSIH(I)=PSIHTB(NZOL)+RZOL*(PSIHTB(NZOL+1)-PSIHTB(NZOL))                  
        PSIM10(I)=PSIMTB(NZOL10)+RZOL10*(PSIMTB(NZOL10+1)-PSIMTB(NZOL10))                                                    
        PSIH10(I)=PSIHTB(NZOL10)+RZOL10*(PSIHTB(NZOL10+1)-PSIHTB(NZOL10))
        PSIM2(I)=PSIMTB(NZOL2)+RZOL2*(PSIMTB(NZOL2+1)-PSIMTB(NZOL2))    
        PSIH2(I)=PSIHTB(NZOL2)+RZOL2*(PSIHTB(NZOL2+1)-PSIHTB(NZOL2))   

!---LIMIT PSIH AND PSIM IN THE CASE OF THIN LAYERS AND HIGH ROUGHNESS            
!---  THIS PREVENTS DENOMINATOR IN FLUXES FROM GETTING TOO SMALL                 
        PSIH(I)=AMIN1(PSIH(I),0.9*GZ1OZ0(I))                                     
        PSIM(I)=AMIN1(PSIM(I),0.9*GZ1OZ0(I))                                     
        PSIH2(I)=AMIN1(PSIH2(I),0.9*GZ2OZ0(I))                                     
        PSIM10(I)=AMIN1(PSIM10(I),0.9*GZ10OZ0(I))                                     
  320 CONTINUE                                                                   
!                                                                                
!-----COMPUTE THE FRICTIONAL VELOCITY:                                           
!     ZA(1982) EQS(2.60),(2.61).                                                 
!                                                                                
      DO 330 I=its,ite
        DTG=THX(I)-THGB(I)                                                   
        PSIX=GZ1OZ0(I)-PSIM(I)                                                   
        PSIX10=GZ10OZ0(I)-PSIM10(I)
!     LOWER LIMIT ADDED TO PREVENT LARGE FLHC IN SOIL MODEL
!     ACTIVATES IN UNSTABLE CONDITIONS WITH THIN LAYERS OR HIGH Z0
        PSIT=AMAX1(GZ1OZ0(I)-PSIH(I),2.)

        IF((XLAND(I)-1.5).GE.0)THEN                                            
          ZL=ZNT(I)                                                            
        ELSE                                                                     
          ZL=0.01                                                                
        ENDIF                                                                    
        PSIQ=ALOG(KARMAN*UST(I)*ZA(I)/XKA+ZA(I)/ZL)-PSIH(I)   
        PSIT2=GZ2OZ0(I)-PSIH2(I)                                     
        PSIQ2=ALOG(KARMAN*UST(I)*2./XKA+2./ZL)-PSIH2(I)                                   
! TO PREVENT OSCILLATIONS AVERAGE WITH OLD VALUE 
        UST(I)=0.5*UST(I)+0.5*KARMAN*WSPD(I)/PSIX                                             
        U10(I)=UX(I)*PSIX10/PSIX                                    
        V10(I)=VX(I)*PSIX10/PSIX                                   
        TH2(I)=THGB(I)+DTG*PSIT2/PSIT                                
        Q2(I)=QSFC(I)+(QX(I)-QSFC(I))*PSIQ2/PSIQ                   
        T2(I) = TH2(I)*(PSFC(I)/100.)**ROVCP                     
!       LATER Q2 WILL BE OVERWRITTEN FOR LAND POINTS IN SURFCE     
!       QA2(I,J) = Q2(I)                                         
!       UA10(I,J) = U10(I)                                      
!       VA10(I,J) = V10(I)                                     
!       write(*,1002)UST(I),KARMAN*WSPD(I),PSIX,KARMAN*WSPD(I)/PSIX
!                                                                                
        USTM=AMAX1(UST(I),0.1)                                                 
        IF((XLAND(I)-1.5).GE.0)THEN                                            
          UST(I)=UST(I)                                                      
        ELSE                                                                     
          UST(I)=USTM                                                          
        ENDIF                                                                    
!       write(*,1002)UST(I),USTM,I,J
 1002   format(f15.12,2x,f15.12,2x,f15.12,2x,f15.12,2x,f15.12)
        MOL(I)=KARMAN*DTG/PSIT/PRT                              
  330 CONTINUE                                                                   
!                                                                                
  335 CONTINUE                                                                   
                                                                                  
!-----COMPUTE THE SURFACE SENSIBLE AND LATENT HEAT FLUXES:                       
                                                                                 
      DO i=its,ite
        QFX(i)=0.                                                              
        HFX(i)=0.                                                              
      ENDDO

      IF (ISFFLX.EQ.0) GOTO 410                                                
                                                                                 
!-----OVER WATER, ALTER ROUGHNESS LENGTH (ZNT) ACCORDING TO WIND (UST).          
                                                                                 
      DO 360 I=its,ite
        IF((XLAND(I)-1.5).GE.0)THEN                                            
          ZNT(I)=CZO*UST(I)*UST(I)/G+OZO                                   
        ENDIF                                                                    
        IF((XLAND(I)-1.5).GE.0)THEN                                            
          ZL=ZNT(I)                                                            
        ELSE                                                                     
          ZL=0.01                                                                
        ENDIF                                                                    
        FLQC(I)=RHOX(I)*MAVAIL(I)*UST(I)*KARMAN/(   & 
                ALOG(KARMAN*UST(I)*ZA(I)/XKA+ZA(I)/ZL)-PSIH(I))          
        DTTHX=ABS(THX(I)-THGB(I))                                            
        IF(DTTHX.GT.1.E-5)THEN                                                   
          FLHC(I)=CPM(I)*RHOX(I)*UST(I)*MOL(I)/(THX(I)-THGB(I))          
!         write(*,1001)FLHC(I),CPM(I),RHOX(I),UST(I),MOL(I),THX(I),THGB(I),I
 1001   format(f8.5,2x,f12.7,2x,f12.10,2x,f12.10,2x,f13.10,2x,f12.8,f12.8,2x,i3)
        ELSE                                                                     
          FLHC(I)=0.                                                             
        ENDIF                                                                    
  360 CONTINUE                                                                   

!                                                                                
!-----COMPUTE SURFACE MOIST FLUX:                                                
!                                                                                
!     IF(IDRY.EQ.1)GOTO 390                                                
!                                                                                
      DO 370 I=its,ite
        QFX(I)=FLQC(I)*(QSFC(I)-QX(I))                                     
        QFX(I)=AMAX1(QFX(I),0.)                                            
        LH(I)=XLV*QFX(I)
  370 CONTINUE                                                                 
                                                                                
!-----COMPUTE SURFACE HEAT FLUX:                                                 
!                                                                                
  390 CONTINUE                                                                 
      DO 400 I=its,ite
        IF(XLAND(I)-1.5.GT.0.)THEN                                           
          HFX(I)=FLHC(I)*(THGB(I)-THX(I))                                
        ELSEIF(XLAND(I)-1.5.LT.0.)THEN                                       
          HFX(I)=FLHC(I)*(THGB(I)-THX(I))                                
          HFX(I)=AMAX1(HFX(I),-250.)                                       
        ENDIF                                                                  
  400 CONTINUE                                                                 
         
      DO I=its,ite
         IF((XLAND(I)-1.5).GE.0)THEN
           ZL=ZNT(I)
         ELSE
           ZL=0.01
         ENDIF
         CHS(I)=UST(I)*KARMAN/(ALOG(KARMAN*UST(I)*ZA(I) &
                /XKA+ZA(I)/ZL)-PSIH(I))
!        GZ2OZ0(I)=ALOG(2./ZNT(I))
!        PSIM2(I)=-10.*GZ2OZ0(I)
!        PSIM2(I)=AMAX1(PSIM2(I),-10.)
!        PSIH2(I)=PSIM2(I)
         CQS2(I)=UST(I)*KARMAN/(ALOG(KARMAN*UST(I)*2.0  &
               /XKA+2.0/ZL)-PSIH2(I))
         CHS2(I)=UST(I)*KARMAN/(GZ2OZ0(I)-PSIH2(I))
      ENDDO
                                                                        
  410 CONTINUE                                                                   
  
!                                                                                
   END SUBROUTINE SFCLAY1D

!====================================================================
   SUBROUTINE sfclayinit( allowed_to_read )         

   LOGICAL , INTENT(IN)      ::      allowed_to_read
   INTEGER                   ::      N
   REAL                      ::      ZOLN,X,Y

   DO N=0,1000
      ZOLN=-FLOAT(N)*0.01
      X=(1-16.*ZOLN)**0.25
      PSIMTB(N)=2*ALOG(0.5*(1+X))+ALOG(0.5*(1+X*X))- &
                2.*ATAN(X)+2.*ATAN(1.)
      Y=(1-16*ZOLN)**0.5
      PSIHTB(N)=2*ALOG(0.5*(1+Y))
   ENDDO

   END SUBROUTINE sfclayinit

!-------------------------------------------------------------------          

END MODULE module_sf_sfclay
