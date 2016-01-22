!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_bl_mrf

CONTAINS

!-------------------------------------------------------------------          
   SUBROUTINE MRF(U3D,V3D,TH3D,T3D,QV3D,QC3D,P3D,PI3D,             &
                  RUBLTEN,RVBLTEN,RTHBLTEN,                        &
                  RQVBLTEN,RQCBLTEN,                               &
                  CP,G,ROVCP,R,ROVG,                               &
                  dz8w,z,XLV,RV,PSFC,                              &
                  p1000mb,                                         &
                  ZNT,UST,ZOL,HOL,PBL,PSIM,PSIH,                   &
                  XLAND,HFX,QFX,TSK,GZ1OZ0,WSPD,BR,                &
                  DT,DTMIN,KPBL2D,                                 &
                  SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,EOMEG,STBOLT,&
                  flag_qi,                                         &
                  ids,ide, jds,jde, kds,kde,                       &
                  ims,ime, jms,jme, kms,kme,                       &
                  its,ite, jts,jte, kts,kte,                       &
               ! Optional
                  QI3D,RQIBLTEN,                                   & 
                  regime                                           )
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
!-- U3D         3D u-velocity interpolated to theta points (m/s)
!-- V3D         3D v-velocity interpolated to theta points (m/s)
!-- TH3D        3D potential temperature (K)
!-- T3D         temperature (K)
!-- QV3D        3D water vapor mixing ratio (Kg/Kg)
!-- QC3D        3D cloud mixing ratio (Kg/Kg)
!-- QI3D        3D ice mixing ratio (Kg/Kg)
!-- P3D         3D pressure (Pa)
!-- PI3D        3D exner function (dimensionless)
!-- rr3D        3D dry air density (kg/m^3)
!-- RUBLTEN     U tendency due to
!               PBL parameterization (m/s^2)
!-- RVBLTEN     V tendency due to
!               PBL parameterization (m/s^2)
!-- RTHBLTEN    Theta tendency due to
!               PBL parameterization (K/s)
!-- RQVBLTEN    Qv tendency due to
!               PBL parameterization (kg/kg/s)
!-- RQCBLTEN    Qc tendency due to
!               PBL parameterization (kg/kg/s)
!-- RQIBLTEN    Qi tendency due to
!               PBL parameterization (kg/kg/s)
!-- CP          heat capacity at constant pressure for dry air (J/kg/K)
!-- G           acceleration due to gravity (m/s^2)
!-- ROVCP       R/CP
!-- R           gas constant for dry air (J/kg/K)
!-- ROVG        R/G
!-- dz8w        dz between full levels (m)
!-- z           height above sea level (m)
!-- XLV         latent heat of vaporization (J/kg)
!-- RV          gas constant for water vapor (J/kg/K)
!-- PSFC        pressure at the surface (Pa)
!-- ZNT         roughness length (m)
!-- UST         u* in similarity theory (m/s)
!-- ZOL         z/L height over Monin-Obukhov length
!-- HOL         PBL height over Monin-Obukhov length
!-- PBL         PBL height (m)
!-- REGIME      flag indicating PBL regime (stable, unstable, etc.)
!-- PSIM        similarity stability function for momentum
!-- PSIH        similarity stability function for heat
!-- XLAND       land mask (1 for land, 2 for water)
!-- HFX         upward heat flux at the surface (W/m^2)
!-- QFX         upward moisture flux at the surface (kg/m^2/s)
!-- TSK         surface temperature (K)
!-- GZ1OZ0      log(z/z0) where z0 is roughness length
!-- WSPD        wind speed at lowest model level (m/s)
!-- BR          bulk Richardson number in surface layer
!-- DT          time step (s)
!-- DTMIN       time step (minute)
!-- rvovrd      R_v divided by R_d (dimensionless)
!-- SVP1        constant for saturation vapor pressure (kPa)
!-- SVP2        constant for saturation vapor pressure (dimensionless)
!-- SVP3        constant for saturation vapor pressure (K)
!-- SVPT0       constant for saturation vapor pressure (K)
!-- EP1         constant for virtual temperature (R_v/R_d - 1) (dimensionless)
!-- EP2         constant for specific humidity calculation
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

      INTEGER,  INTENT(IN   )   ::      ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte

!
      REAL,     INTENT(IN   )   ::      P1000mb
      REAL,     INTENT(IN   )   ::      DT,DTMIN,CP,G,ROVCP,       &
                                        ROVG,R,XLV,RV             

      REAL,     INTENT(IN )     ::     SVP1,SVP2,SVP3,SVPT0 
      REAL,     INTENT(IN )     ::     EP1,EP2,KARMAN,EOMEG,STBOLT

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                           QV3D, &
                                                             QC3D, &
                                                              P3D, &
                                                             PI3D, &
                                                             TH3D, &
                                                              T3D, &
                                                             dz8w, &
                                                                z   
!
      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(INOUT)   ::                        RUBLTEN, &
                                                          RVBLTEN, &
                                                         RTHBLTEN, &
                                                         RQVBLTEN, &
                                                         RQCBLTEN

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )   ::                          XLAND, &
                                                              HFX, &
                                                              QFX
 
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                            HOL, &
                                                              UST, &
                                                              PBL, &
                                                              ZNT

      LOGICAL,  INTENT(IN)      ::                        FLAG_QI
!
!m The following 5 variables are changed to memory size from tile size--
!
     REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(IN   )  ::    &
                                                             PSIM, &
                                                             PSIH

     REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)   ::   &
                                                             WSPD

     REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(IN   )   ::   &
                                                           GZ1OZ0, &
                                                               BR

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )               ::               PSFC

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )   ::                            TSK

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                            ZOL
                                                                      
      INTEGER,  DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(OUT  )   ::                         KPBL2D 

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                            U3D, &
                                                              V3D
!
! Optional
!
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                OPTIONAL                                         , &
                INTENT(INOUT)   ::                         REGIME

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(INOUT)   ::                       RQIBLTEN

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                OPTIONAL                                         , &
                INTENT(IN   )   ::                           QI3D

! LOCAL VARS
   REAL,       DIMENSION( its:ite, kts:kte )          ::   dz8w2d, & 
                                                              z2d
                                                           

   INTEGER ::  I,J,K,NK

!
   DO J=jts,jte
      DO k=kts,kte
      NK=kme-k
      DO i=its,ite
         dz8w2d(I,K) = dz8w(i,NK,j)
         z2d(I,K) = z(i,NK,j)
      ENDDO
      ENDDO


      CALL MRF2D(J,U3D(ims,kms,j),V3D(ims,kms,j),T3D(ims,kms,j),    &
               QV3D(ims,kms,j),QC3D(ims,kms,j),                     &
               P3D(ims,kms,j),RUBLTEN(ims,kms,j),RVBLTEN(ims,kms,j),&
               RTHBLTEN(ims,kms,j),RQVBLTEN(ims,kms,j),             &
               RQCBLTEN(ims,kms,j),                                 &
               p1000mb,                                             &
               CP,G,ROVCP,R,ROVG,                                   &
               dz8w2d,z2d,XLV,Rv,                                   &
               PSFC(ims,j),ZNT(ims,j),                              &
               UST(ims,j),ZOL(ims,j),                               &
               HOL(ims,j),PBL(ims,j),PSIM(ims,j),                   &
               PSIH(ims,j),XLAND(ims,j),HFX(ims,j),QFX(ims,j),      &
               TSK(ims,j),GZ1OZ0(ims,j),WSPD(ims,j),BR(ims,j),      &
               DT,DTMIN,KPBL2D(ims,j),                              &
               SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,EOMEG,STBOLT,    &
               flag_qi,                                             &
               ids,ide, jds,jde, kds,kde,                           &
               ims,ime, jms,jme, kms,kme,                           &
               its,ite, jts,jte, kts,kte,                           &
            !optional
               QI2DTEN=RQIBLTEN(ims,kms,j),                         &
               REGIME=REGIME(ims,j),QI2D=QI3D(ims,kms,j)            )


      DO k=kts,kte
      DO i=its,ite
         RTHBLTEN(I,K,J)=RTHBLTEN(I,K,J)/PI3D(I,K,J)
      ENDDO
      ENDDO

    ENDDO

   END SUBROUTINE MRF

!-------------------------------------------------------------------          
   SUBROUTINE MRF2D(J,U2D,V2D,T2D,QV2D,QC2D,     P2D,              &
                  U2DTEN,V2DTEN,T2DTEN,                            &
                  QV2DTEN,QC2DTEN,                                 & 
                  p1000mb,                                         &
                  CP,G,ROVCP,R,ROVG,                               &
                  dz8w2d,z2d,XLV,RV,PSFCPA,                        &
                  ZNT,UST,ZOL,HOL,PBL,PSIM,PSIH,                   &
                  XLAND,HFX,QFX,TSK,GZ1OZ0,WSPD,BR,                &
                  DT,DTMIN,KPBL1D,                                 &
                  SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,EOMEG,STBOLT,&
                  flag_qi,                                         &
                  ids,ide, jds,jde, kds,kde,                       &
                  ims,ime, jms,jme, kms,kme,                       &
                  its,ite, jts,jte, kts,kte,                       &
               ! optional
                  regime, qi2d, QI2DTEN                            )
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
!     BASED ON THE "COUNTERGRADIENT" TRANSPORT TERM OF TROEN                     
!     AND MAHRT (1986) FOR THE UNSTABLE PBL.                                     
!     THIS ROUTINE USES AN IMPLICIT APPROACH FOR VERTICAL FLUX                   
!     DIVERGENCE AND DOES NOT REQUIRE "MITER" TIMESTEPS.                         
!     IT INCLUDES VERTICAL DIFFUSION IN THE STABLE ATMOSPHERE                    
!     AND MOIST VERTICAL DIFFUSION IN CLOUDS.                                    
!     SURFACE FLUXES CALCULATED AS IN HIRPBL.                                    
!     5-LAYER SOIL MODEL OPTION REQUIRED IN SLAB DUE TO LONG TIMESTEP            
!                                                                                
!     CODED BY SONG-YOU HONG (NCEP), IMPLEMENTED BY JIMY DUDHIA (NCAR)           
!     FALL 1996                                                                  
!                                                                                
!     REFERENCES:                                                                
!                                                                                
!        HONG AND PAN (1996), MON. WEA. REV.                                     
!        TROEN AND MAHRT (1986), BOUNDARY LAYER MET.                             
!                                                                                
!     CHANGES:                                                                   
!        INCREASE RLAM FROM 30 TO 150, AND CHANGE FREE ATMOSPHERE                
!        STABILITY FUNCTION TO INCREASE VERTICAL DIFFUSION                       
!        (HONG, JUNE 1997)                                                       
!                                                                                
!        PUT LOWER LIMIT ON PSI FOR STABLE CONDITIONS. THIS WILL                 
!        PREVENT FLUXES FROM BECOMING TOO SMALL (DUDHIA, OCTOBER 1997)           
!                                                                                
!        CORRECTION TO REGIME CALCULATION. THIS WILL ALLOW POINTS IN             
!        REGIME 4 MUCH MORE FREQUENTLY GIVING LARGER SURFACE FLUXES              
!        REGIME 3 NO LONGER USES HOL < 1.5 OR THVX LAPSE-RATE CHECK              
!        IN MRF SCHEME. THIS WILL MAKE REGIME 3 MUCH LESS FREQUENT.              
!                                                                                
!        ADD SURFACE PRESSURE, PS(I), ARRAY FOR EFFICIENCY                       
!                                                                                
!        FIX FOR PROBLEM WITH THIN LAYERS AND HIGH ROUGHNESS                     
!                                                                                
!        CHARNOCK CONSTANT NOW COMES FROM NAMELIST (DEFAULT SAME)                
!                                                                                
!-------------------------------------------------------------------          

      REAL      RLAM,PRMIN,PRMAX,XKZMIN,XKZMAX,RIMIN,BRCR,         &
                CFAC,PFAC,SFCFRAC,CKZ,ZFMIN,APHI5,APHI16,GAMCRT,   &
                GAMCRQ,XKA,PRT

      PARAMETER (RLAM=150.,PRMIN=0.5,PRMAX=4.)                       
      PARAMETER (XKZMIN=0.01,XKZMAX=1000.,RIMIN=-100.)                           
      PARAMETER (BRCR=0.5,CFAC=7.8,PFAC=2.0,SFCFRAC=0.1)                         
      PARAMETER (CKZ=0.001,ZFMIN=1.E-8,APHI5=5.,APHI16=16.)                      
      PARAMETER (GAMCRT=3.,GAMCRQ=2.E-3)                                         
      PARAMETER (XKA=2.4E-5)                                                     
      PARAMETER (PRT=1.)                                                         
!
      INTEGER,  INTENT(IN   )   ::      ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, &
                                        J
!
      LOGICAL,  INTENT(IN)      ::                        FLAG_QI
!
      REAL,     INTENT(IN   )   ::      P1000mb
      REAL,     INTENT(IN   )   ::      DT,DTMIN,CP,G,ROVCP,       &
                                        ROVG,R,XLV,RV

      REAL,     INTENT(IN )     ::     SVP1,SVP2,SVP3,SVPT0 
      REAL,     INTENT(IN )     ::     EP1,EP2,KARMAN,EOMEG,STBOLT

      REAL,     DIMENSION( ims:ime, kms:kme )                    , &
                INTENT(IN   )   ::                           QV2D, &
                                                             QC2D, &
                                                              P2D, &
                                                              T2D
!
      REAL,     DIMENSION( ims:ime, kms:kme )                    , &
                INTENT(INOUT)   ::                         U2DTEN, &
                                                           V2DTEN, &
                                                           T2DTEN, &
                                                          QV2DTEN, &
                                                          QC2DTEN
                                                                  
      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                            HOL, &
                                                              UST, &
                                                              PBL, &
                                                              ZNT

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(IN   )   ::                          XLAND, &
                                                              HFX, &
                                                              QFX
!
!m The following 5 are changed to memory size---
!
     REAL,     DIMENSION( ims:ime ), INTENT(IN   )   ::      PSIM, &
                                                             PSIH

     REAL,     DIMENSION( ims:ime ), INTENT(INOUT)   ::      WSPD

     REAL,     DIMENSION( ims:ime ), INTENT(IN   )   ::    GZ1OZ0, &
                                                               BR

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(IN   )               ::             PSFCPA

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(IN   )   ::                            TSK

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                            ZOL

      INTEGER,  DIMENSION( ims:ime )                             , &
                INTENT(OUT  )   ::                         KPBL1D

      REAL,     DIMENSION( ims:ime, kms:kme )                    , &
                INTENT(IN   )   ::                            U2D, &
                                                              V2D
                                                                      
! MODULE-LOCAL VARIABLES (DEFINED IN SUBROUTINE MRF)
!
      REAL,     DIMENSION( its:ite, kts:kte ) ,                    &
                INTENT(IN)      ::                         dz8w2d, &
                                                              z2d
!                                                                                
! 
! Optional
!
      REAL,     DIMENSION( ims:ime )                             , &
                OPTIONAL                                         , &
                INTENT(INOUT)   ::                         REGIME

      REAL,     DIMENSION( ims:ime, kms:kme )                    , &
                OPTIONAL                                         , &
                INTENT(IN   )   ::                           QI2D

      REAL,     DIMENSION( ims:ime, kms:kme )                    , &
                OPTIONAL                                         , &
                INTENT(INOUT)   ::                        QI2DTEN
    
! LOCAL VARS

      REAL,     DIMENSION( its:ite, kts:kte+1 ) ::             ZQ

      REAL,     DIMENSION( its:ite, kts:kte )   ::                 &
                                                         UX,VX,QX, &
                                                     QCX,THX,THVX, & 
                                                          DZQ,DZA, & 
                                                        TTNP,QTNP, &
                                                         QCTNP,ZA, &
                                                          UXS,VXS, &
                                                         THXS,QXS, &
                                                         QCXS,QIX, &
                                                       QITNP,QIXS, &
                                                        UTNP,VTNP
!
      REAL,    DIMENSION( its:ite )             ::     QIXSV,RHOX, &
                                                     WSPD1,GOVRTH, & 
                                                       PBL0,THXSV, &
                                                        UXSV,VXSV, &             
                                                       QXSV,QCXSV, & 
                                                     QGH,TGDSA,PS

      INTEGER                                   ::   ILXM,JLXM,KL, &
                                                   KLM,KLP1,KLPBL
!
      INTEGER, DIMENSION( its:ite )             ::     KPBL,KPBL0
!
      REAL,    DIMENSION( its:ite, kts:kte )    ::      SCR3,SCR4
!
      REAL,    DIMENSION( its:ite )             ::           DUM1, &
                                                           XKZMKL
!
      REAL,    DIMENSION( its:ite )             ::    ZL1,THERMAL, &
                                                     WSCALE,HGAMT, &
                                                       HGAMQ,BRDN, &
                                                        BRUP,PHIM, &
                                                         PHIH,CPM, &
                                                      DUSFC,DVSFC, &
                                                      DTSFC,DQSFC
                
!
      REAL,    DIMENSION( its:ite, kts:kte )    ::      XKZM,XKZH, &
                                                            A1,A2, &  
                                                            AD,AU, &
                                                               TX
!
      REAL,    DIMENSION( its:ite, kts:kte )  ::               AL
!
      LOGICAL, DIMENSION( its:ite )             ::         PBLFLG, &
                                                           SFCFLG, &
                                                           STABLE
!                                                         
      REAL, DIMENSION( its:ite )                ::           THGB

      INTEGER ::  N,I,K,KK,L,NZOL,IMVDIF

      INTEGER ::  JBGN,JEND,IBGN,IEND,NK

      REAL    ::  ZOLN,X,Y,CONT,CONQ,CONW,PL,THCON,TVCON,E1,DTSTEP
      REAL    ::  ZL,TSKV,DTHVDZ,DTHVM,VCONV,RZOL
      REAL    ::  DTTHX,PSIX,DTG,PSIQ,USTM
      REAL    ::  DT4,RDT,SPDK2,FM,FH,HOL1,GAMFAC,VPERT,PRNUM
      REAL    ::  ZFAC,XKZO,SS,RI,QMEAN,TMEAN,ALPH,CHI,ZK,RL2,DK,SRI
      REAL    ::  BRINT,DTODSD,DSIG,RDZ,DSDZT,DSDZQ,DSDZ2,TTEND,QTEND
      REAL    ::  UTEND,VTEND,QCTEND,QITEND,TGC,DTODSU

!----------------------------------------------------------------------

      KLPBL=1
      KL=kte
      ILXM=ite-1
      JLXM=jte-1
      KLM=kte-1
      KLP1=kte+1
!                                                                                
      CONT=1000.*CP/G                                                            
      CONQ=1000.*XLV/G                                                           
      CONW=1000./G                                                               

!-- IMVDIF      imvdif=1 for moist adiabat vertical diffusion

      IMVDIF=1

!     DO i=its,ite
!!PS PSFC cmb
!        PSFC(I)=PSFCPA(I)/1000.
!     ENDDO


!                                                                                
!----CONVERT GROUND TEMPERATURE TO POTENTIAL TEMPERATURE:                        
!                                                                                
      DO 5 I=its,ite                                                       
        TGDSA(I)=TSK(I)                                                        
! PS PSFC cmb
        PS(I)=PSFCPA(I)/1000.
!        THGB(I)=TSK(I)*(100./PS(I))**ROVCP   
        THGB(I)=TSK(I)*(P1000mb/PSFCPA(I))**ROVCP   
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
      DO 24 K=kts,kte                                                      
        NK=kme-K
        DO 24 I=its,ite
          UX(I,K)=U2D(I,NK)
          VX(I,K)=V2D(I,NK)
   24 CONTINUE                                                                 
!                                                                                
!.....SCR3(I,K) STORE TEMPERATURE,                                               
!     SCR4(I,K) STORE VIRTUAL TEMPERATURE.                                       
!                                                                                
      DO 30 K=kts,kte
        NK=kme-K
        DO 30 I=its,ite
! PL cmb
          PL=P2D(I,NK)/1000.
          SCR3(I,K)=T2D(I,NK)
!          THCON=(100./PL)**ROVCP                                                 
          THCON=(P1000mb/(PL*1000.))**ROVCP                                                 
          THX(I,K)=SCR3(I,K)*THCON                                               
          TX(I,K)=SCR3(I,K)                                                      
          SCR4(I,K)=SCR3(I,K)                                                    
          THVX(I,K)=THX(I,K)                                                     
          QX(I,K)=0.                                                             
   30 CONTINUE                                                                 
!                                                                                
      DO I=its,ite
         QGH(i)=0.                                                                
         CPM(i)=CP                                                                
      ENDDO
!                                                                                
!     IF(IDRY.EQ.1)GOTO 80                                                   
      DO 50 K=kts,kte
        NK=kme-K
        DO 50 I=its,ite
          QX(I,K)=QV2D(I,NK)
          TVCON=(1.+EP1*QX(I,K))                                      
          THVX(I,K)=THX(I,K)*TVCON                                               
          SCR4(I,K)=SCR3(I,K)*TVCON                                              
   50 CONTINUE                                                                 
!                                                                                
      DO 60 I=its,ite
        E1=SVP1*EXP(SVP2*(TGDSA(I)-SVPT0)/(TGDSA(I)-SVP3))                       
        QGH(I)=EP2*E1/(PS(I)-E1)                                                 
        CPM(I)=CP*(1.+0.8*QX(I,KL))                                   
   60 CONTINUE                                                                   
!                                                                                
!     IF(IMOIST.EQ.1)GOTO 80
      DO 70 K=kts,kte
        NK=kme-K
        DO 70 I=its,ite
          QCX(I,K)=QC2D(I,NK)
   70 CONTINUE

      IF (flag_QI .AND. PRESENT( QI2D ) ) THEN
         DO K=kts,kte
            NK=kme-K
            DO I=its,ite
               QIX(I,K)=QI2D(I,NK)
            ENDDO
         ENDDO
      ELSE
         DO K=kts,kte
            NK=kme-K
            DO I=its,ite
               QIX(I,K)=0.
            ENDDO
         ENDDO
      ENDIF

   80 CONTINUE

!                                                                                
!-----COMPUTE THE HEIGHT OF FULL- AND HALF-SIGMA LEVELS ABOVE GROUND             
!     LEVEL, AND THE LAYER THICKNESSES.                                          
!                                                                                
      DO 90 I=its,ite
        ZQ(I,KLP1)=0.                                                            
        RHOX(I)=PS(I)*1000./(R*SCR4(I,KL))                                       
   90 CONTINUE                                                                   
!                                                                                
      DO 110 KK=kts,kte
        K=kme-KK
        DO 100 I=its,ite                                                   
          DUM1(I)=ZQ(I,K+1)                                                      
  100   CONTINUE                                                                 
!                                                                                
        DO 110 I=its,ite                                                   
           ZQ(I,K)=dz8w2d(I,K)+DUM1(I)
  110   CONTINUE                                                                 
!                                                                                
      DO 120 K=kts,kte
        DO 120 I=its,ite
          ZA(I,K)=0.5*(ZQ(I,K)+ZQ(I,K+1))                                        
          DZQ(I,K)=ZQ(I,K)-ZQ(I,K+1)                                             
  120 CONTINUE                                                                 
!                                                                                
      DO 130 K=kts,kte-1
        DO 130 I=its,ite
          DZA(I,K)=ZA(I,K)-ZA(I,K+1)                                             
  130 CONTINUE                                                                 
               
      DTSTEP=DT                                                                  
!                                                                                
      DO 160 I=its,ite
        GOVRTH(I)=G/THX(I,KL)                                                    
  160 CONTINUE                                                                   
!                                                                                
!-----INITIALIZE VERTICAL TENDENCIES AND                                         
!                                                                                
      DO I=its,ite
      DO K=kts,kte
         UTNP(i,k)=0.                                                           
         VTNP(i,k)=0.                                                           
         TTNP(i,k)=0.                                                           
      ENDDO
      ENDDO
!                                                                                
!     IF(IDRY.EQ.1)GOTO 250                                                  
      DO 230 K=kts,kte
        DO 230 I=its,ite
          QTNP(I,K)=0.                                                           
  230 CONTINUE                                                                 
!                                                                                
!     IF(IMOIST.EQ.1)GOTO 250                                                
      DO 240 K=kts,kte
        DO 240 I=its,ite
          QCTNP(I,K)=0.                                                          
          QITNP(I,K)=0.                                                          
  240 CONTINUE                                                                 
                                                                                 
  250 CONTINUE                                                                   
!                                                                                
!-----CALCULATE BULK RICHARDSON NO. OF SURFACE LAYER, ACCORDING TO               
!     AKB(1976), EQ(12).                                                         
                                                                                 
!     DO 260 I=its,ite
!       GZ1OZ0(I)=ALOG(ZA(I,KL)/ZNT(I))                                        
!       IF((XLAND(I)-1.5).GE.0)THEN                                            
!         ZL=ZNT(I)                                                            
!       ELSE                                                                     
!         ZL=0.01                                                                
!       ENDIF                                                                    
!       WSPD(I)=SQRT(UX(I,KL)*UX(I,KL)+VX(I,KL)*VX(I,KL))                        
!       TSKV=THGB(I)*(1.+EP1*QGH(I)*MAVAIL(I))                     
!       DTHVDZ=(THVX(I,KL)-TSKV)                                                 
!       IF(-DTHVDZ.GE.0)THEN                                                     
!         DTHVM=-DTHVDZ                                                          
!       ELSE                                                                     
!         DTHVM=0.                                                               
!       ENDIF                                                                    
!       VCONV=VCONVC*SQRT(DTHVM)                                                 
!       WSPD(I)=SQRT(WSPD(I)*WSPD(I)+VCONV*VCONV)                                
!       WSPD(I)=AMAX1(WSPD(I),1.)                                                
!       BR(I)=GOVRTH(I)*ZA(I,KL)*DTHVDZ/(WSPD(I)*WSPD(I))                        
! 260 CONTINUE                                                                   
                                                                                 
!!-----DIAGNOSE BASIC PARAMETERS FOR THE APPROPRIATED STABILITY CLASS:            
!!                                                                                
!!     THE STABILITY CLASSES ARE DETERMINED BY BR (BULK RICHARDSON NO.)           
!!     AND HOL (HEIGHT OF PBL/MONIN-OBUKHOV LENGTH).                              
!!                                                                                
!!     CRITERIA FOR THE CLASSES ARE AS FOLLOWS:                                   
!!                                                                                
!!        1. BR .GE. 0.2;                                                         
!!               REPRESENTS NIGHTTIME STABLE CONDITIONS (REGIME=1),               
!!                                                                                
!!        2. BR .LT. 0.2 .AND. BR .GT. 0.0;                                       
!!               REPRESENTS DAMPED MECHANICAL TURBULENT CONDITIONS                
!!               (REGIME=2),                                                      
!!                                                                                
!!        3. BR .EQ. 0.0                                                          
!!               REPRESENTS FORCED CONVECTION CONDITIONS (REGIME=3),              
!!                                                                                
!!        4. BR .LT. 0.0                                                          
!!               REPRESENTS FREE CONVECTION CONDITIONS (REGIME=4).                
!!                                                                                
!!-----                                                                           
!
!      DO 320 I=its,ite
!!----                                                                            
!!--     REMOVE REGIME 3 DEPENDENCE ON PBL HEIGHT                                 
!!--          IF(BR(I).LT.0..AND.HOL(I).GT.1.5)GOTO 310                         
!
!        IF(BR(I).LT.0.)GOTO 310                                                  
!!                                                                                
!!-----CLASS 1; STABLE (NIGHTTIME) CONDITIONS:                                    
!!                                                                                
!        IF(BR(I).LT.0.2)GOTO 270                                                 
!        REGIME(I)=1.                                                           
!        PSIM(I)=-10.*GZ1OZ0(I)                                                   
!!    LOWER LIMIT ON PSI IN STABLE CONDITIONS                                     
!        PSIM(I)=AMAX1(PSIM(I),-10.)                                              
!        PSIH(I)=PSIM(I)                                                          
!        HOL(I)=0.0
!        PBL(I)=0.0                                                             
!        GOTO 320                                                                 
!!                                                                                
!!-----CLASS 2; DAMPED MECHANICAL TURBULENCE:                                     
!!                                                                                
!  270   IF(BR(I).EQ.0.0)GOTO 280                                                 
!        REGIME(I)=2.                                                           
!        PSIM(I)=-5.0*BR(I)*GZ1OZ0(I)/(1.1-5.0*BR(I))                             
!!    LOWER LIMIT ON PSI IN STABLE CONDITIONS                                     
!        PSIM(I)=AMAX1(PSIM(I),-10.)                                              
!!.....AKB(1976), EQ(16).                                                         
!        PSIH(I)=PSIM(I)                                                          
!        HOL(I)=0.0
!        PBL(I)=0.0                                                             
!        GOTO 320                                                                 
!!                                                                                
!!-----CLASS 3; FORCED CONVECTION:                                                
!!                                                                                
!  280   REGIME(I)=3.                                                           
!        PSIM(I)=0.0                                                              
!        PSIH(I)=PSIM(I)                                                          
!                                                                                 
!! special use kte instead of kme
!
!        DO 290 KK=kts,kte-1
!          K=kte-KK
!          IF(THVX(I,K).GT.THVX(I,KL))GOTO 300                                    
!  290   CONTINUE                                                                 
!        STOP 290                                                                 
!  300   PBL(I)=ZQ(I,K+1)                                                       
!        IF(UST(I).LT.0.01)THEN                                                 
!          ZOL(I)=BR(I)*GZ1OZ0(I)                                               
!        ELSE                                                                     
!          ZOL(I)=KARMAN*GOVRTH(I)*ZA(I,KL)*MOL(I,J)/(UST(I)*UST(I)) 
!        ENDIF                                                                    
!        HOL(I)=-ZOL(I)*PBL(I)/ZA(I,KL)
!        GOTO 320                                                                 
!                                                                                 
!!-----CLASS 4; FREE CONVECTION:                                                  
!                                                                                 
!! 310     IF(THVX(I,KLM).GT.THVX(I,KL))GOTO 280                                  
!
!  310   CONTINUE                                                                 
!        REGIME(I)=4.                                                           
!        IF(UST(I).LT.0.01)THEN                                                 
!          ZOL(I)=BR(I)*GZ1OZ0(I)                                               
!        ELSE                                                                     
!          ZOL(I)=KARMAN*GOVRTH(I)*ZA(I,KL)*MOL(I,J)/(UST(I)*UST(I))
!        ENDIF                                                                    
!        ZOL(I)=AMIN1(ZOL(I),0.)                                              
!        ZOL(I)=AMAX1(ZOL(I),-9.9999)                                         
!        NZOL=INT(-ZOL(I)*100.)                                                 
!        RZOL=-ZOL(I)*100.-NZOL                                                 
!        PSIM(I)=PSIMTB(NZOL)+RZOL*(PSIMTB(NZOL+1)-PSIMTB(NZOL))                  
!        PSIH(I)=PSIHTB(NZOL)+RZOL*(PSIHTB(NZOL+1)-PSIHTB(NZOL))                  
!!---LIMIT PSIH AND PSIM IN THE CASE OF THIN LAYERS AND HIGH ROUGHNESS            
!!---  THIS PREVENTS DENOMINATOR IN FLUXES FROM GETTING TOO SMALL                 
!        PSIH(I)=AMIN1(PSIH(I),0.9*GZ1OZ0(I))                                     
!        PSIM(I)=AMIN1(PSIM(I),0.9*GZ1OZ0(I))                                     
!  320 CONTINUE                                                                   

!-----COMPUTE THE FRICTIONAL VELOCITY:        
!     ZA(1982) EQS(2.60),(2.61).     

      DO 330 I=its,ite
        DTG=THX(I,KL)-THGB(I)        
        PSIX=GZ1OZ0(I)-PSIM(I)        
        IF((XLAND(I)-1.5).GE.0)THEN        
          ZL=ZNT(I)        
        ELSE        
          ZL=0.01        
        ENDIF        
        PSIQ=ALOG(KARMAN*UST(I)*ZA(I,KL)/XKA+ZA(I,KL)/ZL)-PSIH(I)        
        UST(I)=KARMAN*WSPD(I)/PSIX        
!        
        USTM=AMAX1(UST(I),0.1)        
        IF((XLAND(I)-1.5).GE.0)THEN        
          UST(I)=UST(I)        
        ELSE                     
          UST(I)=USTM          
        ENDIF                    
!       MOL(I,J)=KARMAN*DTG/(GZ1OZ0(I)-PSIH(I))/PRT
  330 CONTINUE                   
!                                                                                
      DO 420 I=its,ite
        WSPD1(I)=SQRT(UX(I,KL)*UX(I,KL)+VX(I,KL)*VX(I,KL))+1.E-9                  
  420 CONTINUE                                                                   
!                                                                                
!---- COMPUTE VERTICAL DIFFUSION                                                 
!                                                                                
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -          
!     COMPUTE PRELIMINARY VARIABLES                                              
!                                                                                
!                                                                                
      DT4=2.*DTSTEP                                                              
      RDT=1./DT4                                                                 
!                                                                                
      DO I=its,ite
        HGAMT(I)=0.                                                              
        HGAMQ(I)=0.                                                              
        WSCALE(I)=0.                                                             
        KPBL(I)=KL                                                               
        PBL(I)=ZQ(I,KL)                                                        
        KPBL0(I)=KL                                                    
        PBL0(I)=ZQ(I,KL)                                              
        PBLFLG(I)=.TRUE.                                                         
        SFCFLG(I)=.TRUE.                                                         
        IF(BR(I).GT.0.0)SFCFLG(I)=.FALSE.                                        
        ZL1(I)=ZA(I,KL)                                                          
        THERMAL(I)=THVX(I,KL)                                                    
      ENDDO                                                                      
                                                                                 
!     COMPUTE THE FIRST GUESS OF PBL HEIGHT                                      
                                                                                 
      DO I=its,ite
        STABLE(I)=.FALSE.                                                        
        BRUP(I)=BR(I)                                                            
      ENDDO                                                                      
      DO K=KLM,KLPBL,-1                                                          
        DO I=its,ite
          IF(.NOT.STABLE(I))THEN                                                 
            BRDN(I)=BRUP(I)                                                      
            SPDK2=MAX(UX(I,K)**2+VX(I,K)**2,1.)                                  
            BRUP(I)=(THVX(I,K)-THERMAL(I))*(G*ZA(I,K)/THVX(I,KL))/SPDK2          
            KPBL(I)=K                                                            
            STABLE(I)=BRUP(I).GT.BRCR                                            
          ENDIF                                                                  
        ENDDO                                                                    
      ENDDO                                                                      
!                                                                                
      DO I=its,ite
        K=KPBL(I)                                                                
        IF(BRDN(I).GE.BRCR)THEN                                                  
          BRINT=0.                                                               
        ELSEIF(BRUP(I).LE.BRCR)THEN                                              
          BRINT=1.                                                               
        ELSE                                                                     
          BRINT=(BRCR-BRDN(I))/(BRUP(I)-BRDN(I))                                 
        ENDIF                                                                    
        PBL(I)=ZA(I,K+1)+BRINT*(ZA(I,K)-ZA(I,K+1))                             
        IF(PBL(I).LT.ZQ(I,KPBL(I)+1))KPBL(I)=KPBL(I)+1                         
      ENDDO                                                                      
!                                                                                
      DO I=its,ite
        FM=GZ1OZ0(I)-PSIM(I)                                                     
        FH=GZ1OZ0(I)-PSIH(I)                                                     
        HOL(I)=MAX(BR(I)*FM*FM/FH,RIMIN)                                       
        IF(SFCFLG(I))THEN                                                        
          HOL(I)=MIN(HOL(I),-ZFMIN)                                          
        ELSE                                                                     
          HOL(I)=MAX(HOL(I),ZFMIN)                                           
        ENDIF                                                                    
!                                                                                
        HOL1=HOL(I)*PBL(I)/ZL1(I)*SFCFRAC                                    
        HOL(I)=-HOL(I)*PBL(I)/ZL1(I)                                       
        IF(SFCFLG(I))THEN                                                        
          PHIM(I)=(1.-APHI16*HOL1)**(-1./4.)                                     
          PHIH(I)=(1.-APHI16*HOL1)**(-1./2.)                                     
        ELSE                                                                     
          PHIM(I)=(1.+APHI5*HOL1)                                                
          PHIH(I)=PHIM(I)                                                        
        ENDIF                                                                    
        WSCALE(I)=UST(I)/PHIM(I)                                               
        WSCALE(I)=MIN(WSCALE(I),UST(I)*APHI16)                                 
        WSCALE(I)=MAX(WSCALE(I),UST(I)/APHI5)                                  
      ENDDO                                                                      
                                                                                 
!     COMPUTE THE SURFACE VARIABLES FOR PBL HEIGHT ESTIMATION                    
!     UNDER UNSTABLE CONDITIONS                                                  
                                                                                 
      DO I=its,ite
        IF(SFCFLG(I))THEN                                                        
          GAMFAC=CFAC/RHOX(I)/WSCALE(I)                                          
          HGAMT(I)=MIN(GAMFAC*HFX(I)/CPM(I),GAMCRT)                            
          HGAMQ(I)=MIN(GAMFAC*QFX(I),GAMCRQ)                                   
          IF((XLAND(I)-1.5).GE.0)HGAMQ(I)=0.                                   
          VPERT=HGAMT(I)+EP1*THX(I,KL)*HGAMQ(I)                                  
          VPERT=MIN(VPERT,GAMCRT)                                                
          THERMAL(I)=THERMAL(I)+MAX(VPERT,0.)                                    
          HGAMT(I)=MAX(HGAMT(I),0.0)                                             
          HGAMQ(I)=MAX(HGAMQ(I),0.0)                                             
        ELSE                                                                     
          PBLFLG(I)=.FALSE.                                                      
        ENDIF                                                                    
      ENDDO                                                                      
!                                                                                
      DO I=its,ite
        IF(PBLFLG(I))THEN                                                        
          KPBL(I)=KL                                                             
          PBL(I)=ZQ(I,KL)                                                      
        ENDIF                                                                    
      ENDDO                                                                      
!                                                                                
!     ENHANCE THE PBL HEIGHT BY CONSIDERING THE THERMAL                          
!                                                                                
      DO I=its,ite
        IF(PBLFLG(I))THEN                                                        
          STABLE(I)=.FALSE.                                                      
          BRUP(I)=BR(I)                                                          
        ENDIF                                                                    
      ENDDO                                                                      
      DO K=KLM,KLPBL,-1                                                          
        DO I=its,ite
          IF(.NOT.STABLE(I).AND.PBLFLG(I))THEN                                   
            BRDN(I)=BRUP(I)                                                      
            SPDK2=MAX((UX(I,K)**2+VX(I,K)**2),1.)                                
            BRUP(I)=(THVX(I,K)-THERMAL(I))*(G*ZA(I,K)/THVX(I,KL))/SPDK2          
            KPBL(I)=K                                                            
            STABLE(I)=BRUP(I).GT.BRCR                                            
          ENDIF                                                                  
        ENDDO                                                                    
      ENDDO                                                                      
!                                                                                
      DO I=its,ite
        IF(PBLFLG(I))THEN                                                        
          K=KPBL(I)                                                              
          IF(BRDN(I).GE.BRCR)THEN                                                
            BRINT=0.                                                             
          ELSEIF(BRUP(I).LE.BRCR)THEN                                            
            BRINT=1.                                                             
          ELSE                                                                   
            BRINT=(BRCR-BRDN(I))/(BRUP(I)-BRDN(I))                               
          ENDIF                                                                  
          PBL(I)=ZA(I,K+1)+BRINT*(ZA(I,K)-ZA(I,K+1))                           
          IF(PBL(I).LT.ZQ(I,KPBL(I)+1))KPBL(I)=KPBL(I)+1                       
          IF(KPBL(I).LE.1)PBLFLG(I)=.FALSE.                                      
        ENDIF                                                                    
      ENDDO                                                                      
!                                                                                
!     DIAGNOSTIC PBL HEIGHT WITH BRCR EFFECTIVELY ZERO (PBL0)                    
!                                                                                
      DO I=its,ite
        IF(PBLFLG(I))THEN                                                        
          STABLE(I)=.FALSE.                                                      
          BRUP(I)=BR(I)                                                          
        ENDIF                                                                    
      ENDDO                                                                      
      DO K=KLM,KLPBL,-1                                                          
        DO I=its,ite
          IF(.NOT.STABLE(I).AND.PBLFLG(I))THEN                                   
            BRDN(I)=BRUP(I)                                                      
            SPDK2=MAX((UX(I,K)**2+VX(I,K)**2),1.)                                
            BRUP(I)=(THVX(I,K)-THERMAL(I))*(G*ZA(I,K)/THVX(I,KL))/SPDK2          
            KPBL0(I)=K                                                           
            STABLE(I)=BRUP(I).GT.0.0                                             
          ENDIF                                                                  
                                                                                 
        ENDDO                                                                    
      ENDDO                                                                      
!                                                                                
      DO I=its,ite
        IF(PBLFLG(I))THEN                                                        
          K=KPBL0(I)                                                             
          IF(BRDN(I).GE.0.0)THEN                                                 
            BRINT=0.                                                             
          ELSEIF(BRUP(I).LE.0.0)THEN                                             
            BRINT=1.                                                             
          ELSE                                                                   
            BRINT=(0.0-BRDN(I))/(BRUP(I)-BRDN(I))                                
          ENDIF                                                                  
          PBL0(I)=ZA(I,K+1)+BRINT*(ZA(I,K)-ZA(I,K+1))                            
          IF(PBL0(I).LT.ZQ(I,KPBL0(I)+1))KPBL0(I)=KPBL0(I)+1                     
          IF(KPBL0(I).LE.1)PBLFLG(I)=.FALSE.                                     
        ENDIF                                                                    
      ENDDO                                                                      

!                                                                                
!     COMPUTE DIFFUSION COEFFICIENTS BELOW PBL                                   
!                                                                                
      DO K=kte,KLPBL,-1 
        DO I=its,ite
          IF(KPBL(I).LT.K)THEN                                                   
            PRNUM=(PHIH(I)/PHIM(I)+CFAC*KARMAN*SFCFRAC)                          
            PRNUM=MIN(PRNUM,PRMAX)                                               
            PRNUM=MAX(PRNUM,PRMIN)                                               
            ZFAC=MAX((1.-(ZQ(I,K)-ZL1(I))/(PBL(I)-ZL1(I))),ZFMIN)              
            XKZO=CKZ*DZA(I,K-1)                                                    
            XKZM(I,K)=XKZO+WSCALE(I)*KARMAN*ZQ(I,K)*ZFAC**PFAC                   
            XKZH(I,K)=XKZM(I,K)/PRNUM                                            
            XKZM(I,K)=MIN(XKZM(I,K),XKZMAX)                                      
            XKZM(I,K)=MAX(XKZM(I,K),XKZMIN)                                      
            XKZH(I,K)=MIN(XKZH(I,K),XKZMAX)                                      
            XKZH(I,K)=MAX(XKZH(I,K),XKZMIN)                                      
          ENDIF                                                                  
        ENDDO                                                                    
      ENDDO                                                                      
!                                                                                
!     COMPUTE DIFFUSION COEFFICIENTS OVER PBL (FREE ATMOSPHERE)                  
!                                                                                
      DO K=kts+1,kte
        DO I=its,ite
          XKZO=CKZ*DZA(I,K-1)                                                      
          IF(K.LE.KPBL(I))THEN                                                   
            SS=((UX(I,K-1)-UX(I,K))*(UX(I,K-1)-UX(I,K))+(VX(I,K-1)-   & 
               VX(I,K))*(VX(I,K-1)-VX(I,K)))/(DZA(I,K-1)*DZA(I,K-1))+ &          
               1.E-9                                                             
            RI=GOVRTH(I)*(THVX(I,K-1)-THVX(I,K))/(SS*DZA(I,K-1))                 
            IF(IMVDIF.EQ.1)THEN                              
              IF((QCX(I,K)+QIX(I,K)).GT.0.01E-3.AND.(QCX(I,K-1)+      & 
                QIX(I,K-1)).GT.0.01E-3)THEN                                      
!      IN CLOUD                                                                  
                QMEAN=0.5*(QX(I,K)+QX(I,K-1))                                    
                TMEAN=0.5*(SCR3(I,K)+SCR3(I,K-1))                                
                ALPH=XLV*QMEAN/R/TMEAN                                           
                CHI=XLV*XLV*QMEAN/CP/RV/TMEAN/TMEAN                              
                RI=(1.+ALPH)*(RI-G*G/SS/TMEAN/CP*((CHI-ALPH)/(1.+CHI)))          
              ENDIF                                                              
            ENDIF                                                                
            ZK=KARMAN*ZQ(I,K)                                                    
            RL2=(ZK*RLAM/(RLAM+ZK))**2                                           
            DK=RL2*SQRT(SS)                                                      
            IF(RI.LT.0.)THEN                                                     
! UNSTABLE REGIME                                                                
              SRI=SQRT(-RI)                                                      
              XKZM(I,K)=XKZO+DK*(1+8.*(-RI)/(1+1.746*SRI))                       
              XKZH(I,K)=XKZO+DK*(1+8.*(-RI)/(1+1.286*SRI))                       
            ELSE                                                                 
! STABLE REGIME                                                                  
              XKZH(I,K)=XKZO+DK/(1+5.*RI)**2                                     
              PRNUM=1.0+2.1*RI                                                   
              PRNUM=MIN(PRNUM,PRMAX)                                             
              XKZM(I,K)=(XKZH(I,K)-XKZO)*PRNUM+XKZO                              
            ENDIF                                                                
!                                                                                
            XKZM(I,K)=MIN(XKZM(I,K),XKZMAX)                                      
            XKZM(I,K)=MAX(XKZM(I,K),XKZMIN)                                      
            XKZH(I,K)=MIN(XKZH(I,K),XKZMAX)                                      
            XKZH(I,K)=MAX(XKZH(I,K),XKZMIN)                                      
          ENDIF                                                                  
!                                                                                
        ENDDO                                                                    
      ENDDO                                                                      
                                                                                 
!     COMPUTE TRIDIAGONAL MATRIX ELEMENTS FOR HEAT AND MOISTURE                  
                                                                                 
      DO I=its,ite
      DO K=kts,kte
         AU(i,k)=0.
         AL(i,k)=0.
         AD(i,k)=0.
         A1(i,k)=0.
         A2(i,k)=0.
      ENDDO
      ENDDO
 
      DO I=its,ite
        AD(I,1)=1.                                                               
        A1(I,1)=SCR3(I,KL)+HFX(I)/(RHOX(I)*CPM(I))/ZQ(I,KL)*DT4                
        A2(I,1)=QX(I,KL)+QFX(I)/(RHOX(I))/ZQ(I,KL)*DT4                         
      ENDDO                                                                      
!                                                                                
      DO K=kte,kts+1,-1
        KK=kme-K                                                                
        DO I=its,ite
          DTODSD=DT4/dz8w2d(I,K)                                                   
          DTODSU=DT4/dz8w2d(I,K-1)                                                 
          DSIG=z2d(I,K)-z2d(I,K-1)                                                 
          DSIG=-DSIG
          RDZ=1./DZA(I,K-1)                                                      
          IF(PBLFLG(I).AND.KPBL(I).LT.K)THEN                                     
            DSDZT=DSIG*XKZH(I,K)*RDZ*(G/CP-HGAMT(I)/PBL(I))                    
            DSDZQ=DSIG*XKZH(I,K)*RDZ*(-HGAMQ(I)/PBL(I))                        
            A2(I,KK)=A2(I,KK)+DTODSD*DSDZQ                                       
            A2(I,KK+1)=QX(I,K-1)-DTODSU*DSDZQ                                    
          ELSE                                                                   
            DSDZT=DSIG*XKZH(I,K)*RDZ*(G/CP)                                      
            A2(I,KK+1)=QX(I,K-1)                                                 
          ENDIF                                                                  
          DSDZ2=DSIG*XKZH(I,K)*RDZ*RDZ                                           
          AU(I,KK)=-DTODSD*DSDZ2                                                 
          AL(I,KK)=-DTODSU*DSDZ2                                                 
          AD(I,KK)=AD(I,KK)-AU(I,KK)                                             
          AD(I,KK+1)=1.-AL(I,KK)                                                 
          A1(I,KK)=A1(I,KK)+DTODSD*DSDZT                                         
          A1(I,KK+1)=SCR3(I,K-1)-DTODSU*DSDZT                                    
        ENDDO                                                                    
      ENDDO                                                                      
                                                                                 
!     SOLVE TRIDIAGONAL PROBLEM FOR HEAT AND MOISTURE                            
      
      CALL TRIDI2(AL,AD,AU,A1,A2,AU,A1,A2,                 &
                  its,ite,kts,kte                          )

!     RECOVER TENDENCIES OF HEAT AND MOISTURE                                    
                                                                                 
      DO K=kte,kts,-1
        KK=kme-K
        DO I=its,ite
          TTEND=(A1(I,KK)-SCR3(I,K))*RDT                                         
          QTEND=(A2(I,KK)-QX(I,K))*RDT                                           
          TTNP(I,K)=TTNP(I,K)+TTEND                                              
          QTNP(I,K)=QTNP(I,K)+QTEND                                              
        ENDDO                                                                    
      ENDDO                                                                      
                                                                                 
!     COMPUTE TRIDIAGONAL MATRIX ELEMENTS FOR MOMENTUM                           
                                                                                 
      DO I=its,ite
      DO K=kts,kte
         AU(i,k)=0.
         AL(i,k)=0.
         AD(i,k)=0.
         A1(i,k)=0.
         A2(i,k)=0.
      ENDDO
      ENDDO
 
      DO I=its,ite
        AD(I,1)=1.                                                               
        A1(I,1)=UX(I,KL)-UX(I,KL)/WSPD1(I)*UST(I)*UST(I)/ZQ(I,KL)  &
                *DT4*(WSPD1(I)/WSPD(I))**2                            
        A2(I,1)=VX(I,KL)-VX(I,KL)/WSPD1(I)*UST(I)*UST(I)/ZQ(I,KL)  &
                *DT4*(WSPD1(I)/WSPD(I))**2                          
      ENDDO                                                                      
!                                                                                
      DO K=kte,kts+1,-1
        KK=kme-K
        DO I=its,ite
          DTODSD=DT4/dz8w2d(I,K)                                                   
          DTODSU=DT4/dz8w2d(I,K-1)                                                 
          DSIG=z2d(I,K)-z2d(I,K-1)                                                 
          DSIG=-DSIG
          RDZ=1./DZA(I,K-1)                                                      
          DSDZ2=DSIG*XKZM(I,K)*RDZ*RDZ                                           
          AU(I,KK)=-DTODSD*DSDZ2                                                 
          AL(I,KK)=-DTODSU*DSDZ2                                                 
          AD(I,KK)=AD(I,KK)-AU(I,KK)                                             
          AD(I,KK+1)=1.-AL(I,KK)                                                 
          A1(I,KK+1)=UX(I,K-1)                                                   
          A2(I,KK+1)=VX(I,K-1)                                                   
        ENDDO                                                                    
      ENDDO                                                                      
                                                                                 
!     SOLVE TRIDIAGONAL PROBLEM FOR MOMENTUM                                     
                                                                                 
      CALL TRIDI2(AL,AD,AU,A1,A2,AU,A1,A2,                 &
                  its,ite,kts,kte                          )
                                                                                 
!     RECOVER TENDENCIES OF MOMENTUM                                             
                                                                                 
      DO K=kte,kts,-1 
        KK=kme-K
        DO I=its,ite
          UTEND=(A1(I,KK)-UX(I,K))*RDT                                           
          VTEND=(A2(I,KK)-VX(I,K))*RDT                                           
          UTNP(I,K)=UTNP(I,K)+UTEND                                              
          VTNP(I,K)=VTNP(I,K)+VTEND                                              
        ENDDO                                                                    
      ENDDO                                                                      
                                                                                 
!     COMPUTE TRIDIAGONAL MATRIX ELEMENTS FOR CLOUD                              
                                                                                 
      DO I=its,ite
      DO K=kts,kte
         AU(i,k)=0.
         AL(i,k)=0.
         AD(i,k)=0.
         A1(i,k)=0.
         A2(i,k)=0.
      ENDDO
      ENDDO
 
!     IF(IMOIST.EQ.1)GOTO 690                                                
      DO I=its,ite
        AD(I,1)=1.                                                               
        A1(I,1)=QCX(I,KL)                                                        
        A2(I,1)=QIX(I,KL)                                                        
      ENDDO                                                                      
!                                                                                
      DO K=kte,kts+1,-1
        KK=kme-K
        DO I=its,ite
          DTODSD=DT4/dz8w2d(I,K)                                                   
          DTODSU=DT4/dz8w2d(I,K-1)                                                 
          DSIG=z2d(I,K)-z2d(I,K-1)                                                 
          DSIG=-DSIG
          RDZ=1./DZA(I,K-1)                                                      
          A1(I,KK+1)=QCX(I,K-1)                                                  
          A2(I,KK+1)=QIX(I,K-1)                                                  
          DSDZ2=DSIG*XKZH(I,K)*RDZ*RDZ                                           
          AU(I,KK)=-DTODSD*DSDZ2                                                 
          AL(I,KK)=-DTODSU*DSDZ2                                                 
          AD(I,KK)=AD(I,KK)-AU(I,KK)                                             
          AD(I,KK+1)=1.-AL(I,KK)                                                 
        ENDDO                                                                    
      ENDDO                                                                      
                                                                                 
!     SOLVE TRIDIAGONAL PROBLEM FOR CLOUD                                        
                                                                                 
      CALL TRIDI2(AL,AD,AU,A1,A2,AU,A1,A2,                 &
                  its,ite,kts,kte                          )
!
      DO K=kte,kts,-1
        KK=kme-K                                                                
        DO I=its,ite
          QCTEND=(A1(I,KK)-QCX(I,K))*RDT                                         
          QITEND=(A2(I,KK)-QIX(I,K))*RDT                                         
          QCTNP(I,K)=QCTNP(I,K)+QCTEND                                           
          QITNP(I,K)=QITNP(I,K)+QITEND                                           
        ENDDO                                                                    
      ENDDO                                                                      
!                                                                                
!---- END OF VERTICAL DIFFUSION                                                  
!                                                                                
  690 CONTINUE                                                                   
!                                                                                
!-----CALCULATION OF NEW VALUES DUE TO VERTICAL EXCHANGE PROCESSES IS            
!     COMPLETED. THE FINAL STEP IS TO ADD THE TENDENCIES CALCULATED              
!     IN HIRPBL TO THOSE OF MM4.                                                 
                                                                                 
      DO 820 K=kts,kte
        NK=kme-K
        DO 820 I=its,ite
          U2DTEN(I,NK)=UTNP(I,K)
          V2DTEN(I,NK)=VTNP(I,K)
  820   CONTINUE                                                                 
!                                                                                
!     IF(J.EQ.1.AND.IN.GT.1)GOTO 860                                             
!SUE  JBGN=3                                                                     
!SUE  JEND=JLXM-1                                                                

! change when nest
!SUE  JBGN=2                                                                   
!SUE  JEND=JLXM                                                                

      JBGN=jts
      JEND=jte
      IBGN=its                                                                   
      IEND=ite

!     IF(J.LT.JBGN.OR.J.GT.JEND)GOTO 860                                         
!SUE  IBGN=3                                                                     
!SUE  IEND=ILXM-1                                                                

! change when nest
!SUE  IBGN=2                                                                   
!SUE  IEND=ILXM                                                                

      DO 830 K=kts,kte
        NK=kme-K
        DO 830 I=IBGN,IEND                                                       
          T2DTEN(I,NK)=TTNP(I,K)
  830   CONTINUE                                                                 
!                                                                                
!     IF(IDRY.EQ.1)GOTO 860                                                  
      DO 840 K=kts,kte
        NK=kme-K
        DO 840 I=IBGN,IEND                                                       
          QV2DTEN(I,NK)=QTNP(I,K) 
  840   CONTINUE                                                                 
                                                                                 
!     IF(IMOIST.EQ.1)GOTO 860
      DO 850 K=kts,kte
        NK=kme-K
        DO 850 I=IBGN,IEND
           QC2DTEN(I,NK)=QCTNP(I,K)
  850   CONTINUE

      IF(flag_QI .AND. PRESENT( QI2DTEN ) ) THEN
        DO K=kts,kte
        NK=kme-K
          DO I=IBGN,IEND
             QI2DTEN(I,NK)=QITNP(I,K)
          ENDDO
        ENDDO
      ENDIF

  860 CONTINUE                                                                   
!                                                                                
!-----APPLY ASSELIN FILTER TO TGD FOR LARGE TIME STEP:                           
!                                                                                
!     DO 885 I=its,ite
!       TSK(I)=TSK(I)*(PS(I)/100.)**ROVCP                                    
! 885 CONTINUE                                                                   
!                                                                                
  940 CONTINUE                                                                   
!                                                                                
! KPBL IS NEEDED FOR THE FDDA, AND SINCE THERE IS NO LONGER JUST ONE             
! LARGE "J LOOP" IT MUST BE STORED AS (I,J)...                                   
!                                                                                
! USE NEW DIAGNOSED PBL DEPTH (CALCULATED WITH brcr=0.0)
! PBL IS USED FOR OUTPUT AND NEXT-TIME-STEP BELJAARS CALC IN SFCLAY
      DO 950 I=its,ite
        KPBL1D(I)=KPBL0(I)                                                      
        PBL(I)=PBL0(I)
  950 CONTINUE                                                                   

   END SUBROUTINE MRF2D

!================================================================          
   SUBROUTINE TRIDI2(CL,CM,CU,R1,R2,AU,A1,A2,                   &
                     its,ite,kts,kte                            )
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------

   INTEGER, INTENT(IN )      ::     its,ite, kts,kte
                                   
   REAL, DIMENSION( its:ite, kts+1:kte+1 )                    , &
         INTENT(IN   )  ::                                  CL

   REAL, DIMENSION( its:ite, kts:kte )                        , &
         INTENT(IN   )  ::                                  CM, &
                                                            R1, &
                                                            R2
   REAL, DIMENSION( its:ite, kts:kte )                        , &
         INTENT(INOUT)  ::                                  AU, &
                                                            CU, &
                                                            A1, &
                                                            A2

   REAL    :: FK
   INTEGER :: I,K,L,N

!----------------------------------------------------------------         

   L=ite 
   N=kte

   DO I=its,L
     FK=1./CM(I,1)                                                            
     AU(I,1)=FK*CU(I,1)                                                       
     A1(I,1)=FK*R1(I,1)                                                       
     A2(I,1)=FK*R2(I,1)                                                       
   ENDDO                                                                      
   DO K=2,N-1
     DO I=its,L
       FK=1./(CM(I,K)-CL(I,K)*AU(I,K-1))                                      
       AU(I,K)=FK*CU(I,K)                                                     
       A1(I,K)=FK*(R1(I,K)-CL(I,K)*A1(I,K-1))                                 
       A2(I,K)=FK*(R2(I,K)-CL(I,K)*A2(I,K-1))                                 
     ENDDO                                                                    
   ENDDO                                                                      
   DO I=its,L
     FK=1./(CM(I,N)-CL(I,N)*AU(I,N-1))                                        
     A1(I,N)=FK*(R1(I,N)-CL(I,N)*A1(I,N-1))                                   
     A2(I,N)=FK*(R2(I,N)-CL(I,N)*A2(I,N-1))                                   

   ENDDO                                                                      
   DO K=N-1,kts,-1                                                              
     DO I=its,L
       A1(I,K)=A1(I,K)-AU(I,K)*A1(I,K+1)                                      
       A2(I,K)=A2(I,K)-AU(I,K)*A2(I,K+1)                                      
     ENDDO                                                                    
   ENDDO                                                                      

   END SUBROUTINE TRIDI2
      
!===================================================================
   SUBROUTINE mrfinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,           &
                      RQCBLTEN,RQIBLTEN,P_QI,P_FIRST_SCALAR,       &
                      restart, allowed_to_read ,                   &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )
!-------------------------------------------------------------------          
   IMPLICIT NONE
!-------------------------------------------------------------------          
   LOGICAL , INTENT(IN)          :: restart , allowed_to_read
   INTEGER , INTENT(IN)          ::  ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)          ::  P_QI,P_FIRST_SCALAR

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::         &
                                                         RUBLTEN, &
                                                         RVBLTEN, &
                                                         RTHBLTEN, &
                                                         RQVBLTEN, &
                                                         RQCBLTEN, & 
                                                         RQIBLTEN
   INTEGER :: i, j, k, itf, jtf, ktf

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

   IF(.not.restart)THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
        RUBLTEN(i,k,j)=0.
        RVBLTEN(i,k,j)=0.
        RTHBLTEN(i,k,j)=0.
        RQVBLTEN(i,k,j)=0.
        RQCBLTEN(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO
   ENDIF

   IF (P_QI .ge. P_FIRST_SCALAR .and. .not.restart) THEN
      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         RQIBLTEN(i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO
   ENDIF

   END SUBROUTINE mrfinit

!-------------------------------------------------------------------          

END MODULE module_bl_mrf

