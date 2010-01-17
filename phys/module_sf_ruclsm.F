#define LSMRUC_DBG_LVL 3000
!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_sf_ruclsm

  USE module_model_constants
  USE module_wrf_error

! VEGETATION PARAMETERS
        INTEGER :: LUCATS , BARE, NATURAL
        integer, PARAMETER :: NLUS=50
        CHARACTER*8 LUTYPE
        INTEGER, DIMENSION(1:NLUS) :: NROTBL
        real, dimension(1:NLUS) ::  SNUPTBL, RSTBL, RGLTBL, HSTBL, LAITBL,         &
                                    ALBTBL, Z0TBL, LEMITBL, PCTBL, SHDTBL, MAXALB
        REAL ::   TOPT_DATA,CMCMAX_DATA,CFACTR_DATA,RSMAX_DATA

! SOIL PARAMETERS
        INTEGER :: SLCATS
        INTEGER, PARAMETER :: NSLTYPE=30
        CHARACTER*8 SLTYPE
        REAL, DIMENSION (1:NSLTYPE) :: BB,DRYSMC,HC,                           &
        MAXSMC, REFSMC,SATPSI,SATDK,SATDW, WLTSMC,QTZ

! LSM GENERAL PARAMETERS
        INTEGER :: SLPCATS
        INTEGER, PARAMETER :: NSLOPE=30
        REAL, DIMENSION (1:NSLOPE) :: SLOPE_DATA
        REAL ::  SBETA_DATA,FXEXP_DATA,CSOIL_DATA,SALP_DATA,REFDK_DATA,           &
                 REFKDT_DATA,FRZK_DATA,ZBOT_DATA,  SMLOW_DATA,SMHIGH_DATA,        &
                        CZIL_DATA

        CHARACTER*256  :: err_message


CONTAINS

!-----------------------------------------------------------------
    SUBROUTINE LSMRUC(                                           &
                   DT,KTAU,NSL,ZS,                               &
                   RAINBL,SNOW,SNOWH,SNOWC,FRZFRAC,frpcpn,       &
                   Z3D,P8W,T3D,QV3D,QC3D,RHO3D,                  & !p8W in [PA]
                   GLW,GSW,EMISS,CHKLOWQ, CHS,                   & 
                   FLQC,FLHC,MAVAIL,CANWAT,VEGFRA,ALB,ZNT,       &
                   Z0,SNOALB,ALBBCK,                            &  !new
                   QSFC,QSG,QVG,QCG,DEW,SOILT1,TSNAV,            &
                   TBOT,IVGTYP,ISLTYP,XLAND,                     &
                   ISICE,XICE,XICE_THRESHOLD,                    &
                   CP,ROVCP,G0,LV,STBOLT,                        &
                   SOILMOIS,SH2O,SMAVAIL,SMMAX,                  &
                   TSO,SOILT,HFX,QFX,LH,                         &
                   SFCRUNOFF,UDRUNOFF,SFCEXC,                    &
                   SFCEVP,GRDFLX,ACSNOW,SNOM,                    &
                   SMFR3D,KEEPFR3DFLAG,                          &
                   myj,                                          &
                   ids,ide, jds,jde, kds,kde,                    &
                   ims,ime, jms,jme, kms,kme,                    &
                   its,ite, jts,jte, kts,kte                     )
!-----------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------
!
! The RUC LSM model is described in:
!  Smirnova, T.G., J.M. Brown, and S.G. Benjamin, 1997: 
!     Performance of different soil model configurations in simulating 
!     ground surface temperature and surface fluxes. 
!     Mon. Wea. Rev. 125, 1870-1884.
!  Smirnova, T.G., J.M. Brown, and D. Kim, 2000: Parameterization of 
!     cold-season processes in the MAPS land-surface scheme. 
!     J. Geophys. Res. 105, 4077-4086.
!-----------------------------------------------------------------
!-- DT            time step (second)
!        ktau - number of time step
!        NSL  - number of soil layers
!        NZS  - number of levels in soil
!        ZS   - depth of soil levels (m)
!-- RAINBL    - accumulated rain in [mm] between the PBL calls
!-- RAINNCV         one time step grid scale precipitation (mm/step)
!        SNOW - snow water equivalent [mm]
!        FRAZFRAC - fraction of frozen precipitation
!-- SNOWC       flag indicating snow coverage (1 for snow cover)
!-- Z3D         heights (m)
!-- P8W         3D pressure (Pa)
!-- T3D         temperature (K)
!-- QV3D        3D water vapor mixing ratio (Kg/Kg)
!        QC3D - 3D cloud water mixing ratio (Kg/Kg)
!       RHO3D - 3D air density (kg/m^3)
!-- GLW         downward long wave flux at ground surface (W/m^2)
!-- GSW         absorbed short wave flux at ground surface (W/m^2)
!-- EMISS       surface emissivity (between 0 and 1)
!        FLQC - surface exchange coefficient for moisture (kg/m^2/s)
!        FLHC - surface exchange coefficient for heat [W/m^2/s/degreeK]     
!      SFCEXC - surface exchange coefficient for heat [m/s]
!      CANWAT - CANOPY MOISTURE CONTENT (mm)
!      VEGFRA - vegetation fraction (between 0 and 1)
!         ALB - surface albedo (between 0 and 1)
!      SNOALB - maximum snow albedo (between 0 and 1)
!      ALBBCK - snow-free albedo (between 0 and 1)
!         ZNT - roughness length [m]
!-- TBOT        soil temperature at lower boundary (K)
!      IVGTYP - USGS vegetation type (24 classes)
!      ISLTYP - STASGO soil type (16 classes)
!-- XLAND       land mask (1 for land, 2 for water)
!-- CP          heat capacity at constant pressure for dry air (J/kg/K)
!-- G0          acceleration due to gravity (m/s^2)
!-- LV          latent heat of melting (J/kg)
!-- STBOLT      Stefan-Boltzmann constant (W/m^2/K^4)
!    SOILMOIS - soil moisture content (volumetric fraction)
!         TSO - soil temp (K)
!-- SOILT       surface temperature (K)
!-- HFX         upward heat flux at the surface (W/m^2)
!-- QFX         upward moisture flux at the surface (kg/m^2/s)
!-- LH          upward latent heat flux (W/m^2)
!   SFCRUNOFF - ground surface runoff [mm]
!    UDRUNOFF - underground runoff [mm]
!      SFCEVP - total evaporation in [kg/m^2]
!      GRDFLX - soil heat flux (W/m^2: negative, if downward from surface)
!      ACSNOW - accumulation of snow water [m]   
!-- CHKLOWQ - is either 0 or 1 (so far set equal to 1).
!--           used only in MYJPBL. 
!-- tice - sea ice temperture (C)
!-- rhosice - sea ice density (kg m^-3)
!-- capice - sea ice volumetric heat capacity (J/m^3/K)
!-- thdifice - sea ice thermal diffusivity (m^2/s)
!--
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-------------------------------------------------------------------------
!   INTEGER,     PARAMETER            ::     nzss=5
!   INTEGER,     PARAMETER            ::     nddzs=2*(nzss-2)

   INTEGER,     PARAMETER            ::     nvegclas=24+3

   REAL,       INTENT(IN   )    ::     DT
   LOGICAL,    INTENT(IN   )    ::     myj,frpcpn
   INTEGER,    INTENT(IN   )    ::     ktau, nsl, isice,         &
                                       ims,ime, jms,jme, kms,kme, &
                                       ids,ide, jds,jde, kds,kde, &
                                       its,ite, jts,jte, kts,kte

   REAL,    DIMENSION( ims:ime, kms:kme, jms:jme )            , &
            INTENT(IN   )    ::                           QV3D, &
                                                          QC3D, &
                                                           p8w, &
                                                         rho3D, &
                                                           T3D, &
                                                           z3D

   REAL,       DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(IN   )    ::                       RAINBL, &
                                                            GLW, &
                                                            GSW, &
                                                           FLHC, &
                                                           FLQC, &
                                                           CHS , &
                                                          EMISS, &
                                                           XICE, &
                                                          XLAND, &
                                                         ALBBCK, &
                                                         VEGFRA, &
                                                           TBOT

   REAL,       DIMENSION( 1:nsl), INTENT(IN   )      ::      ZS

   REAL,       DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(INOUT)    ::                               &
                                                           SNOW, & !new
                                                          SNOWH, &
                                                          SNOWC, &
                                                         CANWAT, & ! new
                                                         SNOALB, &
                                                            ALB, &
                                                         MAVAIL, & 
                                                         SFCEXC, &
                                                            Z0 , &
                                                            ZNT

   REAL,       DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(IN   )    ::                               &
                                                        FRZFRAC

   INTEGER,    DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(IN   )    ::                       IVGTYP, &
                                                         ISLTYP

   REAL, INTENT(IN   )          ::         CP,ROVCP,G0,LV,STBOLT,XICE_threshold
 
   REAL,       DIMENSION( ims:ime , 1:nsl, jms:jme )           , &
               INTENT(INOUT)    ::                 SOILMOIS,SH2O,TSO

   REAL,       DIMENSION( ims:ime, jms:jme )                   , &
               INTENT(INOUT)    ::                        SOILT, &
                                                            HFX, &
                                                            QFX, &
                                                             LH, &
                                                         SFCEVP, &
                                                      SFCRUNOFF, &
                                                       UDRUNOFF, &
                                                         GRDFLX, &
                                                         ACSNOW, &
                                                           SNOM, &
                                                            QVG, &
                                                            QCG, &
                                                            DEW, &
                                                           QSFC, &
                                                            QSG, &
                                                        CHKLOWQ, &
                                                         SOILT1, &
                                                          TSNAV

   REAL,       DIMENSION( ims:ime, jms:jme )                   , & 
               INTENT(INOUT)    ::                      SMAVAIL, &
                                                          SMMAX

   REAL,       DIMENSION( its:ite, jts:jte )    ::               &
                                                             PC, &
                                                        RUNOFF1, &
                                                        RUNOFF2, &
                                                         EMISSL, &
                                                           ZNTL, &
                                                        LMAVAIL, &
                                                          SMELT, &
                                                           SNOH, &
                                                          SNFLX, &
                                                           EDIR, &
                                                             EC, &
                                                            ETT, &
                                                         SUBLIM, &
                                                           sflx, &
                                                          EVAPL, &
                                                          PRCPL, &
                                                         SEAICE, &
                                                        INFILTR

!--- soil/snow properties
   REAL,       DIMENSION( ims:ime, 1:nsl, jms:jme)               &
                                             ::    KEEPFR3DFLAG, &
                                                         SMFR3D

   REAL                                                          &
                             ::                           RHOCS, &
                                                          RHOSN, &
                                                       RHONEWSN, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                           WILT, &
                                                        CANWATR, &
                                                       SNOWFRAC, &
                                                          SNHEI, &
                                                           SNWE

   REAL                                      ::              CN, &
                                                         SAT,CW, &
                                                           C1SN, &
                                                           C2SN, &
                                                         KQWRTZ, &
                                                           KICE, &
                                                            KWT


   REAL,     DIMENSION(1:NSL)                ::          ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2

   REAL,     DIMENSION(1:2*(nsl-2))          ::           DTDZS

   REAL,     DIMENSION(1:4001)               ::             TBQ


   REAL,     DIMENSION( 1:nsl )              ::         SOILM1D, & 
                                                          TSO1D, &
                                                        SOILICE, &
                                                        SOILIQW, &
                                                       SMFRKEEP

   REAL,     DIMENSION( 1:nsl )              ::          KEEPFR
                                                

   REAL                           ::                        RSM, &
                                                      SNWEPRINT, &
                                                     SNHEIPRINT

   REAL                           ::                     PRCPMS, &
                                                        NEWSNMS, &
                                                           PATM, &
                                                          PATMB, &
                                                           TABS, &
                                                          QVATM, &
                                                          QCATM, &
                                                          Q2SAT, &
                                                         CONFLX, &
                                                            RHO, &
                                                           QKMS, &
                                                           TKMS, &
                                                       INFILTRP
   REAL      ::  cq,r61,r273,arp,brp,x,evs,eis

   REAL      ::  meltfactor
   INTEGER   ::  NROOT
   INTEGER   ::  ILAND,ISOIL
 
   INTEGER, DIMENSION( 1:(nvegclas) )   ::  IFOREST

   INTEGER   ::  I,J,K,NZS,NZS1,NDDZS
   INTEGER   ::  k1,l,k2,kp,km
   CHARACTER (LEN=132) :: message

!-----------------------------------------------------------------

         NZS=NSL
         NDDZS=2*(nzs-2)

!---- table TBQ is for resolution of balance equation in VILKA
        CQ=173.15-.05
        R273=1./273.15
        R61=6.1153*0.62198
        ARP=77455.*41.9/461.525
        BRP=64.*41.9/461.525

        DO K=1,4001
          CQ=CQ+.05
!          TBQ(K)=R61*EXP(ARP*(R273-1./CQ)-BRP*LOG(CQ*R273))
        EVS=EXP(17.67*(CQ-273.15)/(CQ-29.65))
        EIS=EXP(22.514-6.15E3/CQ)
        if(CQ.ge.273.15) then
! tbq is in mb
        tbq(k) = R61*evs
        else
        tbq(k) = R61*eis
        endif

        END DO

!--- Initialize soil/vegetation parameters
!--- This is temporary until SI is added to mass coordinate ---!!!!!

#if ( NMM_CORE == 1 )
     if(ktau+1.eq.1) then
#else
     if(ktau.eq.1) then
#endif
     DO J=jts,jte
         DO i=its,ite
            do k=1,nsl
!       smfr3d  (i,k,j)=soilmois(i,k,j)/900.*1.e3
!       sh2o    (i,k,j)=soilmois(i,k,j)-smfr3d(i,k,j)/1.e3*900.
       keepfr3dflag(i,k,j)=0.
            enddo
!--- initializing to zero snow fraction
           snowc(i,j) = min(1.,snowh(i,j)/0.05)
!--- initializing inside snow temp if it is not defined
        IF((soilt1(i,j) .LT. 170.) .or. (soilt1(i,j) .GT.400.)) THEN
            IF(snowc(i,j).gt.0.1) THEN
           soilt1(i,j)=0.5*(soilt(i,j)+tso(i,1,j))
    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
        WRITE ( message , FMT='(A,F8.3,2I6)' ) &
       'Temperature inside snow is initialized in RUCLSM ', soilt1(i,j),i,j
        CALL wrf_debug ( 0 , message )
    ENDIF
            ELSE
           soilt1(i,j) = soilt(i,j)
            ENDIF
        ENDIF
           tsnav(i,j) =0.5*(soilt(i,j)+tso(i,1,j))-273.
           qcg  (i,j) =0.
           patmb=P8w(i,kms,j)*1.e-2
           QSG  (i,j) = QSN(SOILT(i,j),TBQ)/PATMB
           qvg  (i,j) = QSG(i,j)*mavail(i,j)
!           qvg  (i,j) =qv3d(i,1,j)
!           qsfc(i,j) = qsg(i,j)/(1.+qsg(i,j))
           qsfc(i,j) = qvg(i,j)/(1.+qvg(i,j))
           SMELT(i,j) = 0.
           SNOM (i,j) = 0.
           SNFLX(i,j) = 0.
           DEW  (i,j) = 0.
           PC   (i,j) = 0.
           zntl (i,j) = 0.
           RUNOFF1(i,j) = 0.
           RUNOFF2(i,j) = 0.
           SFCRUNOFF(i,j) = 0.
           UDRUNOFF(i,j) = 0.
           emissl (i,j) = 0.
! Temporarily!!!
            canwat(i,j)=0.

! For RUC LSM CHKLOWQ needed for MYJPBL should 
! 1 because is actual specific humidity at the surface, and
! not the saturation value
           chklowq(i,j) = 1.
           infiltr(i,j) = 0.
           snoh  (i,j) = 0.
           edir  (i,j) = 0.
           ec    (i,j) = 0.
           ett   (i,j) = 0.
           sublim(i,j) = 0.
           sflx  (i,j) = 0.
           evapl (i,j) = 0.
           prcpl (i,j) = 0.
         ENDDO
     ENDDO

        do k=1,nsl
           soilice(k)=0.
           soiliqw(k)=0.
        enddo
     endif

!-----------------------------------------------------------------

        PRCPMS = 0.


   DO J=jts,jte

      DO i=its,ite

    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
      print *,' IN LSMRUC ','ims,ime,jms,jme,its,ite,jts,jte,nzs', &
                ims,ime,jms,jme,its,ite,jts,jte,nzs
      print *,' IVGTYP, ISLTYP ', ivgtyp(i,j),isltyp(i,j)
      print *,' MAVAIL ', mavail(i,j)
      print *,' SOILT,QVG,P8w',soilt(i,j),qvg(i,j),p8w(i,1,j)
      print *, 'LSMRUC, I,J,xland, QFX,HFX from SFCLAY',i,j,xland(i,j), &
                  qfx(i,j),hfx(i,j)
      print *, ' GSW, GLW =',gsw(i,j),glw(i,j)
      print *, 'SOILT, TSO start of time step =',soilt(i,j),(tso(i,k,j),k=1,nsl)
      print *, 'SOILMOIS start of time step =',(soilmois(i,k,j),k=1,nsl)
      print *, 'SMFROZEN start of time step =',(smfr3d(i,k,j),k=1,nsl)
      print *, ' I,J=, after SFCLAY CHS,FLHC ',i,j,chs(i,j),flhc(i,j)
      print *, 'LSMRUC, IVGTYP,ISLTYP,ZNT,ALB = ', ivgtyp(i,j),isltyp(i,j),znt(i,j),alb(i,j),i,j
      print *, 'LSMRUC  I,J,DT,RAINBL =',I,J,dt,RAINBL(i,j)
      print *, 'XLAND ---->, ivgtype,isoiltyp,i,j',xland(i,j),ivgtyp(i,j),isltyp(i,j),i,j
    ENDIF


         ILAND     = IVGTYP(i,j)
         ISOIL     = ISLTYP(I,J)
         TABS      = T3D(i,kms,j)
         QVATM     = QV3D(i,kms,j)
         QCATM     = QC3D(i,kms,j)
         PATM      = P8w(i,kms,j)*1.e-5
!-- Z3D(1) is thickness between first full sigma level and the surface, 
!-- but first mass level is at the half of the first sigma level 
!-- (u and v are also at the half of first sigma level)
         CONFLX    = Z3D(i,kms,j)*0.5
         RHO       = RHO3D(I,kms,J)
!--- 1*e-3 is to convert from mm/s to m/s
       IF(FRPCPN) THEN
         PRCPMS    = (RAINBL(i,j)/DT*1.e-3)*(1-FRZFRAC(I,J))
         NEWSNMS  = (RAINBL(i,j)/DT*1.e-3)*FRZFRAC(I,J)
       ELSE
          if (tabs.le.273.15) then
         PRCPMS    = 0.
         NEWSNMS   = RAINBL(i,j)/DT*1.e-3
          else
         PRCPMS    = RAINBL(i,j)/DT*1.e-3
         NEWSNMS   = 0.
          endif
       ENDIF
!        if   (myj)   then
         QKMS=CHS(i,j)
         TKMS=CHS(i,j)
!        else
!--- convert exchange coeff QKMS to [m/s]
!         QKMS=FLQC(I,J)/RHO/MAVAIL(I,J)
!         TKMS=FLHC(I,J)/RHO/CP
!        endif
!--- convert incoming snow and canwat from mm to m
         SNWE=SNOW(I,J)*1.E-3
         SNHEI=SNOWH(I,J)
         CANWATR=CANWAT(I,J)*1.E-3
         SNOWFRAC=SNOWC(I,J)

!-----
             zsmain(1)=0.
             zshalf(1)=0.
          do k=2,nzs
             zsmain(k)= zs(k)
             zshalf(k)=0.5*(zsmain(k-1) + zsmain(k))
          enddo


!------------------------------------------------------------
!-----  DDZS and DSDZ1 are for implicit solution of soil eqns.
!-------------------------------------------------------------
        NZS1=NZS-1
!-----
    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
         print *,' DT,NZS1, ZSMAIN, ZSHALF --->', dt,nzs1,zsmain,zshalf
    ENDIF

        DO  K=2,NZS1
          K1=2*K-3
          K2=K1+1
          X=DT/2./(ZSHALF(K+1)-ZSHALF(K))
          DTDZS(K1)=X/(ZSMAIN(K)-ZSMAIN(K-1))
          DTDZS2(K-1)=X
          DTDZS(K2)=X/(ZSMAIN(K+1)-ZSMAIN(K))
        END DO


        CN=0.5     ! exponent
        SAT=0.0004   ! canopy water saturated

        CW =4.183E6


!--- Constants used in Johansen soil thermal
!--- conductivity method

        KQWRTZ=7.7
        KICE=2.2
        KWT=0.57

!***********************************************************************
!--- Constants for snow density calculations C1SN and C2SN

        c1sn=0.026
!        c1sn=0.01
        c2sn=21.

!***********************************************************************

        NROOT= 4
!           ! rooting depth

        RHONEWSN = 200.
       if(SNOWH(i,j).gt.0.) then
        RHOSN = SNOW(i,j)/SNOWH(i,j)
       else
        RHOSN = 300.
       endif

!--- initializing soil and surface properties
     CALL SOILVEGIN  ( ILAND,ISOIL,MYJ,IFOREST,                     &
                     EMISSL(I,J),PC(i,j),ZNT(I,J),QWRTZ,       &
!                     EMISSL(I,J),PC(i,j),ZNTL(I,J),QWRTZ,       &
                     RHOCS,BCLH,DQM,KSAT,PSIS,QMIN,REF,WILT     )

!-- definition of number of soil levels in the rooting zone
     IF(iforest(ivgtyp(i,j)).ne.1) THEN
!---- all vegetation types except evergreen and mixed forests
!18apr08 - define meltfactor for Egglston melting limit:
! for open areas factor is 2, and for forests - factor is 1.5
! This will make limit on snow melting smaller and let snow stay 
! longer in the forests.
         meltfactor = 2.0

         do k=2,nzs
         if(zsmain(k).ge.0.4) then
            NROOT=K
            goto  111
         endif
         enddo
     ELSE
!---- evergreen and mixed forests
!18apr08 - define meltfactor
         meltfactor = 1.5

         do k=2,nzs
         if(zsmain(k).ge.1.1) then
            NROOT=K
            goto  111
         endif
         enddo
     ENDIF
 111   continue

!-----
    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
         print *,' ZS, ZSMAIN, ZSHALF, CONFLX --->', zs,zsmain,zshalf,conflx
         print *,'NROOT, iforest, ivgtyp, i,j ', nroot,iforest(ivgtyp(i,j)),ivgtyp(I,J),I,J
    ENDIF

!*** SET ZERO-VALUE FOR SOME OUTPUT DIAGNOSTIC ARRAYS

        IF((XLAND(I,J)-1.5).GE.0.)THEN
!-- Water 
           SMAVAIL(I,J)=1.0
             SMMAX(I,J)=1.0
             SNOW(I,J)=0.0
           LMAVAIL(I,J)=1.0

           ILAND=16
           ISOIL=14

           patmb=P8w(i,1,j)*1.e-2
           qvg  (i,j) = QSN(SOILT(i,j),TBQ)/PATMB
           qsfc(i,j) = qvg(i,j)/(1.+qvg(i,j))
           CHKLOWQ(I,J)=1.
           Q2SAT=QSN(TABS,TBQ)/PATMB

            DO K=1,NZS
              SOILMOIS(I,K,J)=1.0
              SH2O    (I,K,J)=1.0 
              TSO(I,K,J)= SOILT(I,J)
            ENDDO

    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
              PRINT*,'  water point, I=',I,                      &
              'J=',J, 'SOILT=', SOILT(i,j)
    ENDIF

           ELSE

! LAND POINT OR SEA ICE
       if(xice(i,j).ge.xice_threshold) then
!       if(IVGTYP(i,j).eq.isice) then
           SEAICE(i,j)=1.
       else
           SEAICE(i,j)=0.
       endif

         IF(SEAICE(I,J).GT.0.5)THEN
!-- Sea-ice case
    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
              PRINT*,' sea-ice at water point, I=',I,            &
              'J=',J
    ENDIF
            ILAND = 24
            ISOIL = 16
            ZNT(I,J) = 0.011
            snoalb(i,j) = 0.75
            dqm = 1.
            ref = 1.
            qmin = 0.
            wilt = 0.
            emissl(i,j) = 1.0 

            DO K=1,NZS
               soilmois(i,k,j) = 1.
               smfr3d(i,k,j)   = 1.
               sh2o(i,k,j)     = 0.
               keepfr3dflag(i,k,j) = 0.
            ENDDO
          ENDIF

!  Attention!!!!  RUC LSM uses soil moisture content minus residual (minimum
!  or dry soil moisture content for a given soil type) as a state variable.

           DO k=1,nzs
! soilm1d - soil moisture content minus residual [m**3/m**3]
              soilm1d (k) = min(max(0.,soilmois(i,k,j)),dqm)
              tso1d   (k) = tso(i,k,j)
              soiliqw (k) = sh2o(i,k,j)
           ENDDO 

           do k=1,nzs
              smfrkeep(k) = smfr3d(i,k,j)
              keepfr  (k) = keepfr3dflag(i,k,j)
           enddo

!              LMAVAIL(I,J)=max(0.00001,min(1.,soilmois(i,1,j)/(REF-QMIN)))
              LMAVAIL(I,J)=max(0.00001,min(1.,soilmois(i,1,j)/dqm))

! extract dew from the cloud water at the surface
              QCG(I,J)=QCG(I,J)-DEW(I,J)

    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
   print *,'LAND, i,j,tso1d,soilm1d,PATM,TABS,QVATM,QCATM,RHO',  &
                  i,j,tso1d,soilm1d,PATM,TABS,QVATM,QCATM,RHO
   print *,'CONFLX =',CONFLX 
   print *,'SMFRKEEP,KEEPFR   ',SMFRKEEP,KEEPFR
    ENDIF

!-----------------------------------------------------------------
         CALL SFCTMP (dt,ktau,conflx,i,j,                        &
!--- input variables
                nzs,nddzs,nroot,meltfactor,                      &   !added meltfactor
                iland,isoil,xland(i,j),ivgtyp(i,j),PRCPMS,       &
                NEWSNMS,SNWE,SNHEI,SNOWFRAC,RHOSN,RHONEWSN,      &
                PATM,TABS,QVATM,QCATM,RHO,                       &
                GLW(I,J),GSW(I,J),EMISSL(I,J),                   &
                QKMS,TKMS,PC(I,J),LMAVAIL(I,J),                  &
                canwatr,vegfra(I,J),alb(I,J),znt(I,J),           &
                snoalb(i,j),albbck(i,j),                         &   !new
                myj,seaice(i,j),                                 &
!--- soil fixed fields
                QWRTZ,                                           &
                rhocs,dqm,qmin,ref,                              &
                wilt,psis,bclh,ksat,                             &
                sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,           &
!--- constants
                cp,rovcp,g0,lv,stbolt,cw,c1sn,c2sn,              &
                KQWRTZ,KICE,KWT,                                 &
!--- output variables
                snweprint,snheiprint,rsm,                        &
                soilm1d,tso1d,smfrkeep,keepfr,                   &
                soilt(I,J),soilt1(i,j),tsnav(i,j),dew(I,J),      &
                qvg(I,J),qsg(I,J),qcg(I,J),SMELT(I,J),           &
                SNOH(I,J),SNFLX(I,J),SNOM(I,J),ACSNOW(I,J),      &
                edir(I,J),ec(I,J),ett(I,J),qfx(I,J),          &
                lh(I,J),hfx(I,J),sflx(I,J),sublim(I,J),          &
                evapl(I,J),prcpl(I,J),runoff1(I,J),              &
                runoff2(I,J),soilice,soiliqw,infiltrp)
!-----------------------------------------------------------------

!***  DIAGNOSTICS
!--- available and maximum soil moisture content in the soil
!--- domain

        smavail(i,j) = 0.
        smmax (i,j)  = 0.  

      do k=1,nzs-1
        smavail(i,j)=smavail(i,j)+(qmin+soilm1d(k))*             &
                    (zshalf(k+1)-zshalf(k))
        smmax (i,j) =smmax (i,j)+(qmin+dqm)*                     &
                    (zshalf(k+1)-zshalf(k))
      enddo

        smavail(i,j)=smavail(i,j)+(qmin+soilm1d(nzs))*           &
                    (zsmain(nzs)-zshalf(nzs))
        smmax (i,j) =smmax (i,j)+(qmin+dqm)*                     &
                    (zsmain(nzs)-zshalf(nzs))

!--- Convert the water unit into mm
        SFCRUNOFF(I,J) = SFCRUNOFF(I,J)+RUNOFF1(I,J)*DT*1000.0
        UDRUNOFF (I,J) = UDRUNOFF(I,J)+RUNOFF2(I,J)*1000.0
        SMAVAIL  (I,J) = SMAVAIL(I,J) * 1000.
        SMMAX    (I,J) = SMMAX(I,J) * 1000.

        do k=1,nzs

             soilmois(i,k,j) = soilm1d(k)
             sh2o    (i,k,j) = soiliqw(k)
                  tso(i,k,j) = tso1d(k)
        enddo

        tso(i,nzs,j) = tbot(i,j)

        do k=1,nzs
             smfr3d(i,k,j) = smfrkeep(k)
           keepfr3dflag(i,k,j) = keepfr (k)
        enddo

!tgs add together dew and cloud at the ground surface
        qcg(i,j)=qcg(i,j)+dew(i,j)

        Z0       (I,J) = ZNT (I,J)
        SFCEXC   (I,J) = TKMS
        patmb=P8w(i,1,j)*1.e-2
        Q2SAT=QSN(TABS,TBQ)/PATMB
        QSFC(I,J) = QVG(I,J)/(1.+QVG(I,J))
! for MYJ surface and PBL scheme
!      if (myj) then
! MYJSFC expects QSFC as actual specific humidity at the surface
        IF((QVATM.GE.Q2SAT*0.95).AND.QVATM.LT.qvg(I,J))THEN
          CHKLOWQ(I,J)=0.
        ELSE
          CHKLOWQ(I,J)=1.
        ENDIF
!      else
!          CHKLOWQ(I,J)=1.
!      endif

    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
      if(CHKLOWQ(I,J).eq.0.) then
   print *,'i,j,CHKLOWQ',  &
                  i,j,CHKLOWQ(I,J)
      endif
    ENDIF

! SNOW is in [mm], SNWE is in [m]; CANWAT is in mm, CANWATR is in m
        SNOW   (i,j) = SNWE*1000.
        SNOWH  (I,J) = SNHEI 
        CANWAT (I,J) = CANWATR*1000.
        INFILTR(I,J) = INFILTRP

        MAVAIL (i,j) = LMAVAIL(I,J)  
    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
       print *,' LAND, I=,J=, QFX, HFX after SFCTMP', i,j,lh(i,j),hfx(i,j)
    ENDIF
!!!        QFX    (I,J) = LH(I,J)/LV
        SFCEVP (I,J) = SFCEVP (I,J) + QFX (I,J) * DT
        GRDFLX (I,J) = -1. * sflx(I,J)

    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
       print *,' QFX after change, LH ', i,j, QFX(i,j),LH(I,J)
    ENDIF
!--- SNOWC snow cover flag
       if(snowfrac > 0. .and. (xice(i,j).ge.xice_threshold .and. xice(i,j) < 1.)) then
           SNOWFRAC = SNOWFRAC*XICE(I,J)
       endif

       SNOWC(I,J)=SNOWFRAC


!--- get 3d soil fields
    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
   print *,'LAND, i,j,tso1d,soilm1d - end of time step',         &
                  i,j,tso1d,soilm1d
    ENDIF

!--- end of a land or sea ice point
        ENDIF

      ENDDO

   ENDDO

!-----------------------------------------------------------------
   END SUBROUTINE LSMRUC
!-----------------------------------------------------------------



   SUBROUTINE SFCTMP (delt,ktau,conflx,i,j,                      &
!--- input variables
                nzs,nddzs,nroot,meltfactor,                      &
                ILAND,ISOIL,XLAND,IVGTYP,PRCPMS,                 &
                NEWSNMS,SNWE,SNHEI,SNOWFRAC,RHOSN,RHONEWSN,      &
                PATM,TABS,QVATM,QCATM,rho,                       &
                GLW,GSW,EMISS,QKMS,TKMS,PC,                      &
                MAVAIL,CST,VEGFRA,ALB,ZNT,                       &
                ALB_SNOW,ALB_SNOW_FREE,                          &
                MYJ,SEAICE,                                      &
!--- soil fixed fields
                QWRTZ,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,    &
                sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,           &
!--- constants
                cp,rovcp,g0,lv,stbolt,cw,c1sn,c2sn,              &
                KQWRTZ,KICE,KWT,                                 &
!--- output variables
                snweprint,snheiprint,rsm,                        &
                soilm1d,ts1d,smfrkeep,keepfr,soilt,soilt1,       &
                tsnav,dew,qvg,qsg,qcg,                           &
                SMELT,SNOH,SNFLX,SNOM,ACSNOW,                    &
                edir1,ec1,ett1,eeta,qfx,hfx,s,sublim,            &
                evapl,prcpl,runoff1,runoff2,soilice,             &
                soiliqw,infiltr)
!-----------------------------------------------------------------
       IMPLICIT NONE
!-----------------------------------------------------------------

!--- input variables

   INTEGER,  INTENT(IN   )   ::  i,j,nroot,ktau,nzs ,            &
                                 nddzs                             !nddzs=2*(nzs-2)

   REAL,     INTENT(IN   )   ::  DELT,CONFLX,meltfactor
   REAL,     INTENT(IN   )   ::  C1SN,C2SN
   LOGICAL,    INTENT(IN   )    ::     myj
!--- 3-D Atmospheric variables
   REAL                                                        , &
            INTENT(IN   )    ::                            PATM, &
                                                           TABS, &
                                                          QVATM, &
                                                          QCATM
   REAL                                                        , &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                             PC, &
                                                         VEGFRA, &
                                                  ALB_SNOW_FREE, &
                                                         SEAICE, &
                                                          XLAND, &
                                                            RHO, &
                                                           QKMS, &
                                                           TKMS
                                                             
   INTEGER,   INTENT(IN   )  ::                          IVGTYP
!--- 2-D variables
   REAL                                                        , &
            INTENT(INOUT)    ::                           EMISS, &
                                                         MAVAIL, &
                                                       SNOWFRAC, &
                                                       ALB_SNOW, &
                                                            ALB, &
                                                            CST

!--- soil properties
   REAL                      ::                                  &
                                                          RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                            SAT, &
                                                           WILT

   REAL,     INTENT(IN   )   ::                              CN, &
                                                             CW, &
                                                             CP, &
                                                          ROVCP, &
                                                             G0, &
                                                             LV, &
                                                         STBOLT, &
                                                         KQWRTZ, &
                                                           KICE, &
                                                            KWT

   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2 


   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:4001), INTENT(IN)  ::              TBQ


!--- input/output variables
!-------- 3-d soil moisture and temperature
   REAL,     DIMENSION( 1:nzs )                                , &
             INTENT(INOUT)   ::                            TS1D, & 
                                                        SOILM1D, &
                                                       SMFRKEEP
   REAL,  DIMENSION( 1:nzs )                                   , &
             INTENT(INOUT)   ::                          KEEPFR
          

   INTEGER, INTENT(INOUT)    ::                     ILAND,ISOIL

!-------- 2-d variables
   REAL                                                        , &
             INTENT(INOUT)   ::                             DEW, &
                                                          EDIR1, &
                                                            EC1, &
                                                           ETT1, &
                                                           EETA, &
                                                          EVAPL, &
                                                        INFILTR, &
                                                          RHOSN, & 
                                                       RHONEWSN, &
                                                         SUBLIM, &
                                                          PRCPL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                            QFX, &
                                                            HFX, &
                                                              S, &  
                                                        RUNOFF1, &
                                                        RUNOFF2, &
                                                         ACSNOW, &
                                                           SNWE, &
                                                          SNHEI, &
                                                          SMELT, &
                                                           SNOM, &
                                                           SNOH, &
                                                          SNFLX, &
                                                          SOILT, &
                                                         SOILT1, &
                                                          TSNAV, &
                                                            ZNT

   REAL,     DIMENSION(1:NZS)              ::                    &
                                                           tice, &
                                                        rhosice, &
                                                         capice, &
                                                       thdifice
!-------- 1-d variables
   REAL,     DIMENSION(1:NZS), INTENT(OUT)  ::          SOILICE, &
                                                        SOILIQW
            
                     

   REAL,     INTENT(OUT)                    ::              RSM, &  
                                                      SNWEPRINT, &
                                                     SNHEIPRINT
!--- Local variables

   INTEGER ::  K,ILNB

   REAL    ::  BSN, XSN                                        , &
               RAINF, SNTH, NEWSN, PRCPMS, NEWSNMS             , &
               T3, UPFLUX, XINET
   REAL    ::  snhei_crit, keep_snow_albedo

   REAL    ::  RNET,GSWNEW,EMISSN,ZNTSN
   REAL    ::  VEGFRAC
   real    ::  cice, albice, albsn

!-----------------------------------------------------------------
        integer,   parameter      ::      ilsnow=99 
        
    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
        print *,' in SFCTMP',i,j,nzs,nddzs,nroot,                 &
                 SNWE,RHOSN,SNOM,SMELT,TS1D
    ENDIF

        NEWSN=0.
        RAINF = 0.
        RSM=0.
        INFILTR=0.
        VEGFRAC=0.01*VEGFRA
       if(VEGFRAC.le.0.01) VEGFRAC=0.
!---initialize local arrays for sea ice
          do k=1,nzs
            tice(k) = 0.
            rhosice(k) = 0. 
            cice = 0.
            capice(k) = 0.
            thdifice(k) = 0.
          enddo

        GSWnew=GSW
        ALBice=ALB_SNOW_FREE
!--- sea ice properties
!--- N.N Zubov "Arctic Ice"
!--- no salinity dependence because we consider the ice pack
!--- to be old and to have low salinity (0.0002)
       if(SEAICE.ge.0.5) then
          do k=1,nzs
            tice(k) = ts1d(k) - 273.15
            rhosice(k) = 917.6/(1-0.000165*tice(k))
            cice = 2115.85 +7.7948*tice(k)
            capice(k) = cice*rhosice(k)
            thdifice(k) = 2.260872/capice(k)
           enddo
!-- SEA ICE ALB dependence on ice temperature. When ice temperature is
!-- below critical value of -10C - no change to albedo.
!-- If temperature is higher that -10C then albedo is decreasing.
!-- The minimum albedo at t=0C for ice is 0.1 less.
         GSWNEW=GSW/(1.-ALB)
       ALBice = MIN(ALB_SNOW_FREE,MAX(ALB_SNOW_FREE - 0.1,   &
               ALB_SNOW_FREE - 0.1*(tice(1)+10.)/10. ))
         GSWNEW=GSW*(1.-ALBice)
       endif

    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
        print *,'I,J,KTAU,QKMS,TKMS', i,j,ktau,qkms,tkms
        print *,'GSW,GSWnew,GLW,SOILT,EMISS,ALB,ALBice,SNWE',&
                 GSW,GSWnew,GLW,SOILT,EMISS,ALB,ALBice,SNWE
    ENDIF

         SNHEI   = SNWE * 1000. / RHOSN
!--------------
         T3      = STBOLT*SOILT*SOILT*SOILT
         UPFLUX  = T3 *SOILT
         XINET   = EMISS*(GLW-UPFLUX)
         RNET    = GSWnew + XINET

!Calculate the amount (m) of fresh snow

	if(snhei.gt.0.0081*1.e3/rhosn) then
!*** Correct snow density for current temperature (Koren et al. 1999)
        BSN=delt/3600.*c1sn*exp(0.08*tsnav-c2sn*rhosn*1.e-3)
       if(bsn*snwe*100..lt.1.e-4) goto 777
        XSN=rhosn*(exp(bsn*snwe*100.)-1.)/(bsn*snwe*100.)
        rhosn=MIN(MAX(100.,XSN),400.)
!        rhosn=MIN(MAX(50.,XSN),400.)
 777   continue

      else
        rhosn     =200.
        rhonewsn  =200.
      endif

           newsn=newsnms*delt
!---- ACSNOW - accumulation of snow water [m]
           acsnow=acsnow+newsn

       IF(NEWSN.GE.1.E-8) THEN
!*** Calculate fresh snow density (t > -15C, else MIN value)
!*** Eq. 10 from Koren et al. (1999)

    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
      print *, 'THERE IS NEW SNOW, newsn', newsn
    ENDIF
        if(tabs.lt.258.15) then
!          rhonewsn=50.
          rhonewsn=100.

        else
          rhonewsn=1.e3*max((0.10+0.0017*(Tabs-273.15+15.)**1.5) &
                                 , 0.10)
!          rhonewsn=1.e3*max((0.05+0.0017*(Tabs-273.15+15.)**1.5) &
!                                 , 0.05)
          rhonewsn=MIN(rhonewsn,400.)
!          rhonewsn=100.
        endif


!*** Define average snow density of the snow pack considering
!*** the amount of fresh snow (eq. 9 in Koren et al.(1999) 
!*** without snow melt )
         xsn=(rhosn*snwe+rhonewsn*newsn)/                         &
             (snwe+newsn)
         rhosn=MIN(MAX(100.,XSN),400.)
!         rhosn=MIN(MAX(50.,XSN),400.)

         snwe=snwe+newsn
         snhei=snwe*1.E3/rhosn
         NEWSN=NEWSN*1.E3/rhosn
        endif

       IF(PRCPMS.NE.0.) THEN

! PRCPMS is liquid precipitation rate
! RAINF is a flag used for calculation of rain water
! heat content contribution into heat budget equation. Rain's temperature
! is set equal to air temperature at the first atmospheric
! level.  

           RAINF=1.
       ENDIF

        IF(SNHEI.GT.0.0) THEN
!--- Set of surface parameters should be changed to snow values for grid
!--- points where the snow cover exceeds snow threshold of 2 cm

         SNHEI_CRIT=0.01601*1.e3/rhosn
         SNOWFRAC=MIN(1.,SNHEI/(2.*SNHEI_CRIT))
!tgs NEW - low limit on snow fraction
       if(SNOWFRAC.lt.0.01) snowfrac=0.
!---        EMISS = 0.91 for snow
         EMISS = EMISS*(1.-snowfrac)+0.91*snowfrac

         KEEP_SNOW_ALBEDO = 0.
      IF (NEWSN.GT.0.) KEEP_SNOW_ALBEDO = 1.

!---  GSW in-coming solar
         GSWNEW=GSW/(1.-ALB)

    IF(SEAICE .LT. 0.5) THEN
!----- SNOW on soil
!-- ALB dependence on snow depth
       ALBsn   = MAX(keep_snow_albedo*alb_snow,                 &
            MIN((alb_snow_free +                                &
           (alb_snow - alb_snow_free) *                         &
           (snhei/(2.*SNHEI_CRIT))), alb_snow))

!-- ALB dependence on snow temperature. When snow temperature is
!-- below critical value of -10C - no change to albedo.
!-- If temperature is higher that -10C then albedo is decreasing.
!-- The minimum albedo at t=0C for snow on land is 15% less than
!-- albedo of temperatures below -10C.
      if(albsn.lt.alb_snow)then
       ALB=ALBsn
      else
       ALB = MIN(ALBSN,MAX(ALBSN - 0.15*(soilt - 263.15)/       &
                (273.15-263.15), ALBSN - 0.15))
      endif
    ELSE
!----- SNOW on ice
       ALBsn   = MAX(keep_snow_albedo*alb_snow,                 &
            MIN((albice + (alb_snow - albice) *                 &
           (snhei/(2.*SNHEI_CRIT))), alb_snow))

!-- ALB dependence on snow temperature. When snow temperature is
!-- below critical value of -10C - no change to albedo.
!-- If temperature is higher that -10C then albedo is decreasing.
!-- The minimum albedo at t=0C for snow on ice is 0.15 less.
      if(albsn.lt.alb_snow)then
       ALB=ALBsn
      else
       ALB = MIN(ALBSN,MAX(ALBSN - 0.15*(soilt - 263.15)/       &
                (273.15-263.15), ALBSN - 0.15))
      endif

    ENDIF

!--- recompute absorbed solar radiation and net radiation
!--- for new value of albedo
         gswnew=gswnew*(1.-alb)

        XINET   = EMISS*(GLW-UPFLUX)
        RNET    = GSWnew + XINET

    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
        print *,'I,J,GSW,GSWnew,GLW,UPFLUX,ALB',&
                 i,j,GSW,GSWnew,GLW,UPFLUX,ALB
    ENDIF


      if (SEAICE .LT. 0.5) then
! LAND
         CALL SNOWSOIL (                                        & !--- input variables
            i,j,isoil,delt,ktau,conflx,nzs,nddzs,nroot,         &
            meltfactor,rhonewsn,                                &  ! new
            ILAND,PRCPMS,RAINF,NEWSN,snhei,SNWE,snowfrac,       &
            RHOSN,PATM,QVATM,QCATM,                             &
            GLW,GSWnew,EMISS,RNET,IVGTYP,                       &
            QKMS,TKMS,PC,CST,                                   &
            RHO,VEGFRAC,ALB,ZNT,                                &
            MYJ,                                                &
!--- soil fixed fields
            QWRTZ,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,       &
            sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,              & 
!--- constants
            lv,CP,rovcp,G0,cw,stbolt,tabs,                      &
            KQWRTZ,KICE,KWT,                                    &
!--- output variables
            ilnb,snweprint,snheiprint,rsm,                      &
            soilm1d,ts1d,smfrkeep,keepfr,                       &
            dew,soilt,soilt1,tsnav,qvg,qsg,qcg,                 &
            SMELT,SNOH,SNFLX,SNOM,edir1,ec1,ett1,eeta,          &
            qfx,hfx,s,sublim,prcpl,runoff1,runoff2,             &
            mavail,soilice,soiliqw,infiltr                      )
       else
! SEA ICE
         CALL SNOWSEAICE (                                      &
            i,j,isoil,delt,ktau,conflx,nzs,nddzs,               &    
            meltfactor,rhonewsn,                                &  ! new
            ILAND,PRCPMS,RAINF,NEWSN,snhei,SNWE,snowfrac,       &    
            RHOSN,PATM,QVATM,QCATM,                             &    
            GLW,GSWnew,EMISS,RNET,                              &    
            QKMS,TKMS,RHO,                                      &    
!--- sea ice parameters
            ALB,ZNT,                                            &
            tice,rhosice,capice,thdifice,                       &    
            zsmain,zshalf,DTDZS,DTDZS2,tbq,                     &    
!--- constants
            lv,CP,rovcp,cw,stbolt,tabs,                         &    
!--- output variables
            ilnb,snweprint,snheiprint,rsm,ts1d,                 &    
            dew,soilt,soilt1,tsnav,qvg,qsg,qcg,                 &    
            SMELT,SNOH,SNFLX,SNOM,eeta,                         &    
            qfx,hfx,s,sublim,prcpl                              &    
                                                                )    
           edir1 = eeta
           ec1 = 0.
           ett1 = 0.
           runoff1 = smelt
           runoff2 = 0.
           mavail = 1.
           infiltr=0.
           cst=0.
            do k=1,nzs
               soilm1d(k)=1.
               soiliqw(k)=0.
               soilice(k)=1.
               smfrkeep(k)=1.
               keepfr(k)=0.
            enddo
       endif

         if(snhei.eq.0.) then
!--- all snow is melted
         alb=alb_snow_free
         endif

        ELSE
!--- no snow
           snheiprint=0.
           snweprint=0.

       if(SEAICE .LT. 0.5) then
!  LAND
         CALL SOIL(                                             &
!--- input variables
            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,   &
            PRCPMS,RAINF,PATM,QVATM,QCATM,GLW,GSWnew,           &
            EMISS,RNET,QKMS,TKMS,PC,cst,rho,vegfrac,            &
!--- soil fixed fields 
            QWRTZ,rhocs,dqm,qmin,ref,wilt,                      &
            psis,bclh,ksat,sat,cn,                              &
            zsmain,zshalf,DTDZS,DTDZS2,tbq,                     &
!--- constants
            lv,CP,rovcp,G0,cw,stbolt,tabs,                      &
            KQWRTZ,KICE,KWT,                                    &
!--- output variables
            soilm1d,ts1d,smfrkeep,keepfr,                       &
            dew,soilt,qvg,qsg,qcg,edir1,ec1,                    &
            ett1,eeta,qfx,hfx,s,evapl,prcpl,runoff1,            &
            runoff2,mavail,soilice,soiliqw,                     &
            infiltr)
        else
! SEA ICE
          CALL SICE(                                            &
!--- input variables
            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,   &
            PRCPMS,RAINF,PATM,QVATM,QCATM,GLW,GSWnew,           &
            EMISS,RNET,QKMS,TKMS,rho,                           &
!--- sea ice parameters
            tice,rhosice,capice,thdifice,                       &
            zsmain,zshalf,DTDZS,DTDZS2,tbq,                     &
!--- constants
            lv,CP,rovcp,cw,stbolt,tabs,                         &
!--- output variables
            ts1d,dew,soilt,qvg,qsg,qcg,                         &
            eeta,qfx,hfx,s,evapl,prcpl                          &
                                                                )
           edir1 = eeta
           ec1 = 0.
           ett1 = 0.
           runoff1 = prcpms
           runoff2 = 0.
           mavail = 1.
           infiltr=0.
           cst=0.
            do k=1,nzs
               soilm1d(k)=1.
               soiliqw(k)=0.
               soilice(k)=1.
               smfrkeep(k)=1.
               keepfr(k)=0.
            enddo
        endif

        ENDIF
!      ENDIF

!

!      RETURN
!       END
!---------------------------------------------------------------
   END SUBROUTINE SFCTMP
!---------------------------------------------------------------


       FUNCTION QSN(TN,T)
!****************************************************************
   REAL,     DIMENSION(1:4001),  INTENT(IN   )   ::  T
   REAL,     INTENT(IN  )   ::  TN

      REAL    QSN, R,R1,R2
      INTEGER I

       R=(TN-173.15)/.05+1.
       I=INT(R)
       IF(I.GE.1) goto 10
       I=1
       R=1.
  10   IF(I.LE.4000) GOTO 20
       I=4000
       R=4001.
  20   R1=T(I)
       R2=R-I
       QSN=(T(I+1)-R1)*R2 + R1
!       print *,' in QSN, I,R,R1,R2,T(I+1),TN, QSN', I,R,r1,r2,t(i+1),tn,QSN
!       RETURN
!       END
!-----------------------------------------------------------------------
  END FUNCTION QSN
!------------------------------------------------------------------------


        SUBROUTINE SOIL (                                    &
!--- input variables
            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,&
            PRCPMS,RAINF,PATM,QVATM,QCATM,                   &
            GLW,GSW,EMISS,RNET,                              &
            QKMS,TKMS,PC,cst,rho,vegfrac,                    &
!--- soil fixed fields
            QWRTZ,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,    &
            sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,           &
!--- constants
            xlv,CP,rovcp,G0_P,cw,stbolt,TABS,                &
            KQWRTZ,KICE,KWT,                                 &
!--- output variables
            soilmois,tso,smfrkeep,keepfr,                    &
            dew,soilt,qvg,qsg,qcg,                           &
            edir1,ec1,ett1,eeta,qfx,hfx,s,evapl,             &
            prcpl,runoff1,runoff2,mavail,soilice,            &
            soiliqw,infiltrp)

!*************************************************************
!   Energy and moisture budget for vegetated surfaces 
!   without snow, heat diffusion and Richards eqns. in
!   soil
!
!     DELT - time step (s)
!     ktau - numver of time step
!     CONFLX - depth of constant flux layer (m)
!     J,I - the location of grid point
!     IME, JME, KME, NZS - dimensions of the domain
!     NROOT - number of levels within the root zone
!     PRCPMS - precipitation rate in m/s
!     PATM - pressure [bar]
!     QVATM,QCATM - cloud and water vapor mixing ratio (kg/kg)
!                   at the first atm. level
!     GLW, GSW - incoming longwave and absorbed shortwave
!                radiation at the surface (W/m^2)
!     EMISS,RNET - emissivity of the ground surface (0-1) and net
!                  radiation at the surface (W/m^2)
!     QKMS - exchange coefficient for water vapor in the
!              surface layer (m/s)
!     TKMS - exchange coefficient for heat in the surface
!              layer (m/s)
!     PC - plant coefficient (resistance) (0-1)
!     RHO - density of atmosphere near sueface (kg/m^3)
!     VEGFRAC - greeness fraction
!     RHOCS - volumetric heat capacity of dry soil
!     DQM, QMIN - porosity minus residual soil moisture QMIN (m^3/m^3)
!     REF, WILT - field capacity soil moisture and the
!                 wilting point (m^3/m^3)
!     PSIS - matrix potential at saturation (m)
!     BCLH - exponent for Clapp-Hornberger parameterization
!     KSAT - saturated hydraulic conductivity (m/s)
!     SAT - maximum value of water intercepted by canopy (m)
!     CN - exponent for calculation of canopy water
!     ZSMAIN - main levels in soil (m)
!     ZSHALF - middle of the soil layers (m)
!     DTDZS,DTDZS2 - dt/(2.*dzshalf*dzmain) and dt/dzshalf in soil
!     TBQ - table to define saturated mixing ration
!           of water vapor for given temperature and pressure
!     SOILMOIS,TSO - soil moisture (m^3/m^3) and temperature (K)
!     DEW -  dew in kg/m^2s
!     SOILT - skin temperature (K)
!     QSG,QVG,QCG - saturated mixing ratio, mixing ratio of
!                   water vapor and cloud at the ground
!                   surface, respectively (kg/kg)
!     EDIR1, EC1, ETT1, EETA - direct evaporation, evaporation of
!            canopy water, transpiration in kg m-2 s-1 and total
!            evaporation in m s-1.
!     QFX, HFX - latent and sensible heat fluxes (W/m^2)
!     S - soil heat flux in the top layer (W/m^2)
!     RUNOFF - surface runoff (m/s)
!     RUNOFF2 - underground runoff (m)
!     MAVAIL - moisture availability in the top soil layer (0-1)
!     INFILTRP - infiltration flux from the top of soil domain (m/s)
!
!*****************************************************************
        IMPLICIT NONE
!-----------------------------------------------------------------

!--- input variables

   INTEGER,  INTENT(IN   )   ::  nroot,ktau,nzs                , &
                                 nddzs                    !nddzs=2*(nzs-2)
   INTEGER,  INTENT(IN   )   ::  i,j,iland,isoil
   REAL,     INTENT(IN   )   ::  DELT,CONFLX
!--- 3-D Atmospheric variables
   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM
!--- 2-D variables
   REAL,                                                         &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                          EMISS, &
                                                            RHO, &
                                                             PC, &
                                                        VEGFRAC, &
                                                           QKMS, &
                                                           TKMS

!--- soil properties
   REAL,                                                         &
            INTENT(IN   )    ::                           RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                           WILT

   REAL,     INTENT(IN   )   ::                              CN, &
                                                             CW, &
                                                         KQWRTZ, &
                                                           KICE, &
                                                            KWT, &
                                                            XLV, &
                                                            g0_p


   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:4001), INTENT(IN)  ::              TBQ


!--- input/output variables
!-------- 3-d soil moisture and temperature
   REAL,     DIMENSION( 1:nzs )                                , &
             INTENT(INOUT)   ::                             TSO, &
                                                       SOILMOIS, &
                                                       SMFRKEEP

   REAL,     DIMENSION( 1:nzs )                                , &
             INTENT(INOUT)   ::                          KEEPFR

!-------- 2-d variables
   REAL,                                                         &
             INTENT(INOUT)   ::                             DEW, &
                                                            CST, &
                                                          EDIR1, &
                                                            EC1, &
                                                           ETT1, &
                                                           EETA, &
                                                          EVAPL, &
                                                          PRCPL, &
                                                         MAVAIL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                           RNET, &
                                                            QFX, &
                                                            HFX, &
                                                              S, &
                                                            SAT, &
                                                        RUNOFF1, &
                                                        RUNOFF2, &
                                                          SOILT

!-------- 1-d variables
   REAL,     DIMENSION(1:NZS), INTENT(OUT)  ::          SOILICE, &
                                                        SOILIQW

!--- Local variables

   REAL    ::  INFILTRP, transum                               , &
               RAINF,  PRCPMS                                  , &
               TABS, T3, UPFLUX, XINET
   REAL    ::  CP,rovcp,G0,LV,STBOLT,xlmelt,dzstop             , &
               can,epot,fac,fltot,ft,fq,hft                    , &
               q1,ras,rhoice,sph                               , &
               trans,zn,ci,cvw,tln,tavln,pi                    , &
               DD1,CMC2MS,DRYCAN,WETCAN                        , &
               INFMAX,RIW
   REAL,     DIMENSION(1:NZS)  ::  transp,cap,diffu,hydro      , &
                                   thdif,tranf,tav,soilmoism   , &
                                   soilicem,soiliqwm,detal     , &
                                   fwsat,lwsat,told,smold

   REAL                        ::  drip

   INTEGER ::  nzs1,nzs2,k

!-----------------------------------------------------------------

!-- define constants
!        STBOLT=5.670151E-8
        RHOICE=900.
        CI=RHOICE*2100.
        XLMELT=3.35E+5
        cvw=cw

        SAT=0.0004
        prcpl=prcpms

!--- Initializing local arrays
        DO K=1,NZS
          TRANSP   (K)=0.
          soilmoism(k)=0.
          soilice  (k)=0.
          soiliqw  (k)=0.
          soilicem (k)=0.
          soiliqwm (k)=0.
          lwsat    (k)=0.
          fwsat    (k)=0.
          tav      (k)=0.
          cap      (k)=0.
          thdif    (k)=0.
          diffu    (k)=0.
          hydro    (k)=0.   
          tranf    (k)=0.
          detal    (k)=0.
          told     (k)=0.
          smold    (k)=0.
        ENDDO

          NZS1=NZS-1
          NZS2=NZS-2
        dzstop=1./(zsmain(2)-zsmain(1))
        RAS=RHO*1.E-3
        RIW=rhoice*1.e-3

!--- Computation of volumetric content of ice in soil 

         DO K=1,NZS
!- main levels
         tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(XLMELT*                        &
         (tso(k)-273.15)/tso(k)/9.81/psis)                       &
          **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/RIW

!---- melting and freezing is balanced, soil ice cannot increase
       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*riw)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif

          ENDDO

          DO K=1,NZS1
!- middle of soil layers
         tav(k)=0.5*(tso(k)+tso(k+1))
         soilmoism(k)=0.5*(soilmois(k)+soilmois(k+1))
         tavln=log(tav(k)/273.15)

         if(tavln.lt.0.) then
           soiliqwm(k)=(dqm+qmin)*(XLMELT*                       &
         (tav(k)-273.15)/tav(k)/9.81/psis)                       &
          **(-1./bclh)-qmin
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
           soiliqwm(k)=max(0.,soiliqwm(k))
           soiliqwm(k)=min(soiliqwm(k), soilmoism(k))
           soilicem(k)=(soilmoism(k)-soiliqwm(k))/riw
!---- melting and freezing is balanced, soil ice cannot increase
       if(keepfr(k).eq.1.) then
           soilicem(k)=min(soilicem(k),                          &
                   0.5*(smfrkeep(k)+smfrkeep(k+1)))
           soiliqwm(k)=max(0.,soilmoism(k)-soilicem(k)*riw)
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
       endif

         else
           soilicem(k)=0.
           soiliqwm(k)=soilmoism(k)
           lwsat(k)=dqm+qmin
           fwsat(k)=0.
         endif

          ENDDO

          do k=1,nzs
           if(soilice(k).gt.0.) then
             smfrkeep(k)=soilice(k)
           else
             smfrkeep(k)=soilmois(k)/riw
           endif
          enddo

!******************************************************************
! SOILPROP computes thermal diffusivity, and diffusional and
!          hydraulic condeuctivities
!******************************************************************
          CALL SOILPROP(                                          &
!--- input variables
               nzs,fwsat,lwsat,tav,keepfr,                        &
               soilmois,soiliqw,soilice,                          &
               soilmoism,soiliqwm,soilicem,                       &
!--- soil fixed fields
               QWRTZ,rhocs,dqm,qmin,psis,bclh,ksat,               &
!--- constants
               riw,xlmelt,CP,G0_P,cvw,ci,                         &
               kqwrtz,kice,kwt,                                   &
!--- output variables
               thdif,diffu,hydro,cap)

!********************************************************************
!--- CALCULATION OF CANOPY WATER (Smirnova et al., 1996, EQ.16) AND DEW 
 
        DRIP=0.
        DD1=0.

        FQ=QKMS

        Q1=-QKMS*RAS*(QVATM - QSG)

        DEW=0.
        IF(QVATM.GE.QSG)THEN
          DEW=FQ*(QVATM-QSG)
        ENDIF
        IF(DEW.NE.0.)THEN
          DD1=CST+DELT*(PRCPMS +DEW*RAS)*vegfrac
        ELSE
          DD1=CST+                                                 &
            DELT*(PRCPMS+RAS*FQ*(QVATM-QSG)                        &
           *(CST/SAT)**CN)*vegfrac
        ENDIF

        IF(DD1.LT.0.) DD1=0.
        if(vegfrac.eq.0.)then
          cst=0.
          drip=0.
        endif
        IF (vegfrac.GT.0.) THEN
          CST=DD1
        IF(CST.GT.SAT) THEN
          CST=SAT
          DRIP=DD1-SAT
        ENDIF
        ENDIF

!--- WETCAN is the fraction of vegetated area covered by canopy
!--- water, and DRYCAN is the fraction of vegetated area where
!--- transpiration may take place.

          WETCAN=(CST/SAT)**CN
          DRYCAN=1.-WETCAN

!**************************************************************
!  TRANSF computes transpiration function
!**************************************************************
           CALL TRANSF(                                       &
!--- input variables
              nzs,nroot,soiliqw,tabs,                         &
!--- soil fixed fields
              dqm,qmin,ref,wilt,zshalf,                       &
!--- output variables
              tranf,transum)


!--- Save soil temp and moisture from the beginning of time step
          do k=1,nzs
           told(k)=tso(k)
           smold(k)=soilmois(k)
          enddo

!**************************************************************
!  SOILTEMP soilves heat budget and diffusion eqn. in soil
!**************************************************************

        CALL SOILTEMP(                                        &
!--- input variables
             i,j,iland,isoil,                                 &
             delt,ktau,conflx,nzs,nddzs,nroot,                &
             PRCPMS,RAINF,                                    &
             PATM,TABS,QVATM,QCATM,EMISS,RNET,                &
             QKMS,TKMS,PC,rho,vegfrac,                        &
             thdif,cap,drycan,wetcan,                         & 
             transum,dew,mavail,                              &
!--- soil fixed fields
             dqm,qmin,bclh,zsmain,zshalf,DTDZS,tbq,           &
!--- constants
             xlv,CP,G0_P,cvw,stbolt,                          &
!--- output variables
             tso,soilt,qvg,qsg,qcg)

!************************************************************************

!--- CALCULATION OF DEW USING NEW VALUE OF QSG OR TRANSP IF NO DEW
        ETT1=0.
        DEW=0.

        IF(QVATM.GE.QSG)THEN
          DEW=QKMS*(QVATM-QSG)
          DO K=1,NZS
            TRANSP(K)=0.
          ENDDO
        ELSE
          DO K=1,NROOT
            TRANSP(K)=VEGFRAC*RAS*QKMS*                       &
                    (QVATM-QSG)*                              &
                     PC*TRANF(K)*DRYCAN/ZSHALF(NROOT+1)
               IF(TRANSP(K).GT.0.) TRANSP(K)=0.
            ETT1=ETT1-TRANSP(K)
          ENDDO
          DO k=nroot+1,nzs
            transp(k)=0.
          enddo
        ENDIF

!-- Recalculating of volumetric content of frozen water in soil
         DO K=1,NZS
!- main levels
           tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(XLMELT*                     &
          (tso(k)-273.15)/tso(k)/9.81/psis)                   & 
           **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/riw
!---- melting and freezing is balanced, soil ice cannot increase
       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*riw)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif
         ENDDO

!*************************************************************************
! SOILMOIST solves moisture budget (Smirnova et al., 1996, EQ.22,28) 
!           and Richards eqn.
!*************************************************************************
          CALL SOILMOIST (                                     &
!-- input
               delt,nzs,nddzs,DTDZS,DTDZS2,RIW,                &
               zsmain,zshalf,diffu,hydro,                      &
               QSG,QVG,QCG,QCATM,QVATM,-PRCPMS,                &
               QKMS,TRANSP,DRIP,DEW,0.,SOILICE,VEGFRAC,        &
!               QKMS,TRANSP,DRIP,DEW,0.,SOILICE,VEGFRAC,        &
!-- soil properties
               DQM,QMIN,REF,KSAT,RAS,INFMAX,                   &
!-- output
               SOILMOIS,SOILIQW,MAVAIL,RUNOFF1,                &
               RUNOFF2,INFILTRP)
        
!--- KEEPFR is 1 when the temperature and moisture in soil
!--- are both increasing. In this case soil ice should not
!--- be increasing according to the freezing curve.
!--- Some part of ice is melted, but additional water is
!--- getting frozen. Thus, only structure of frozen soil is
!--- changed, and phase changes are not affecting the heat
!--- transfer. This situation may happen when it rains on the
!--- frozen soil.
 
        do k=1,nzs
       if (soilice(k).gt.0.) then
          if(tso(k).gt.told(k).and.soilmois(k).gt.smold(k)) then
              keepfr(k)=1.
          else
              keepfr(k)=0.
          endif
       endif
        enddo
!--- THE DIAGNOSTICS OF SURFACE FLUXES 

          T3      = STBOLT*SOILT*SOILT*SOILT
          UPFLUX  = T3 *SOILT
          XINET   = EMISS*(GLW-UPFLUX)
          RNET    = GSW + XINET
          HFT=-TKMS*CP*RHO*(TABS-SOILT)                        &
               *(P1000mb*0.00001/Patm)**ROVCP
          Q1=-QKMS*RAS*(QVATM - QSG)
        IF (Q1.LE.0.) THEN
! ---  condensation
          EC1=0.
          EDIR1=0.
          ETT1=0.
!-- moisture flux for coupling with PBL
          EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QSG/(1.+QSG))*1.E3
          QFX= XLV*EETA
!-- actual moisture flux from RUC LSM
          EETA= RHO*DEW
        ELSE
! ---  evaporation
          EDIR1 =-(1.-vegfrac)*QKMS*RAS*                       &
                  (QVATM-QVG)
          EC1 = Q1 * WETCAN
          CMC2MS=CST/DELT
         if(EC1.gt.CMC2MS) cst=0.
          EC1=MIN(CMC2MS,EC1)*vegfrac
!-- moisture flux for coupling with PBL
          EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QVG/(1.+QVG))*1.E3
! to convert from kg m-2 s-1 to m s-1: 1/rho water=1.e-3************
          QFX= XLV * EETA
!-- actual moisture flux from RUC LSM
          EETA = (EDIR1 + EC1 + ETT1)*1.E3
        ENDIF
          EVAPL=EETA
          S=THDIF(1)*CAP(1)*DZSTOP*(TSO(1)-TSO(2))
          HFX=HFT
          FLTOT=RNET-HFT-QFX-S

 222    CONTINUE

 1123    FORMAT(I5,8F12.3)
 1133    FORMAT(I7,8E12.4)
  123   format(i6,f6.2,7f8.1)
  122   FORMAT(1X,2I3,6F8.1,F8.3,F8.2)


!      RETURN                                                                 
!      END                                                                    
!-------------------------------------------------------------------
   END SUBROUTINE SOIL
!-------------------------------------------------------------------

        SUBROUTINE SICE (                                       &
!--- input variables
            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,   &
            PRCPMS,RAINF,PATM,QVATM,QCATM,GLW,GSW,              &
            EMISS,RNET,QKMS,TKMS,rho,                           &
!--- sea ice parameters
            tice,rhosice,capice,thdifice,                       &
            zsmain,zshalf,DTDZS,DTDZS2,tbq,                     &
!--- constants
            xlv,CP,rovcp,cw,stbolt,tabs,                        &
!--- output variables
            tso,dew,soilt,qvg,qsg,qcg,                          &
            eeta,qfx,hfx,s,evapl,prcpl                          &
                                                                )

!*****************************************************************
!   Energy budget and  heat diffusion eqns. for
!   sea ice
!*************************************************************

        IMPLICIT NONE
!-----------------------------------------------------------------

!--- input variables

   INTEGER,  INTENT(IN   )   ::  nroot,ktau,nzs                , &
                                 nddzs                    !nddzs=2*(nzs-2)
   INTEGER,  INTENT(IN   )   ::  i,j,iland,isoil
   REAL,     INTENT(IN   )   ::  DELT,CONFLX
!--- 3-D Atmospheric variables
   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM
!--- 2-D variables
   REAL,                                                         &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                          EMISS, &
                                                            RHO, &
                                                           QKMS, &
                                                           TKMS
!--- sea ice properties
   REAL,    DIMENSION(1:NZS)                                   , &
            INTENT(IN   )    ::                                  &
                                                           tice, &
                                                        rhosice, &
                                                         capice, &
                                                       thdifice


   REAL,     INTENT(IN   )   ::                                  &
                                                             CW, &
                                                            XLV


   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:4001), INTENT(IN)  ::              TBQ


!--- input/output variables
!----soil temperature
   REAL,     DIMENSION( 1:nzs ),  INTENT(INOUT)   ::        TSO
!-------- 2-d variables
   REAL,                                                         &
             INTENT(INOUT)   ::                             DEW, &
                                                           EETA, &
                                                          EVAPL, &
                                                          PRCPL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                           RNET, &
                                                            QFX, &
                                                            HFX, &
                                                              S, &
                                                          SOILT

!--- Local variables
   REAL    ::  x,x1,x2,x4,tn,denom
   REAL    ::  RAINF,  PRCPMS                                  , &
               TABS, T3, UPFLUX, XINET

   REAL    ::  CP,rovcp,G0,LV,STBOLT,xlmelt,dzstop             , &
               epot,fltot,ft,fq,hft,ras,cvw                    

   REAL    ::  FKT,D1,D2,D9,D10,DID,R211,R21,R22,R6,R7,D11     , &
               PI,H,FKQ,R210,AA,BB,PP,Q1,QS1,TS1,TQ2,TX2       , &
               TDENOM

   REAL    ::  AA1,RHCS


   REAL,     DIMENSION(1:NZS)  ::   cotso,rhtso

   INTEGER ::  nzs1,nzs2,k,k1,kn,kk

!-----------------------------------------------------------------

!-- define constants
!        STBOLT=5.670151E-8
        XLMELT=3.35E+5
        cvw=cw

        prcpl=prcpms

          NZS1=NZS-1
          NZS2=NZS-2
        dzstop=1./(zsmain(2)-zsmain(1))
        RAS=RHO*1.E-3

        do k=1,nzs
           cotso(k)=0.
           rhtso(k)=0.
        enddo

        cotso(1)=0.
        rhtso(1)=TSO(NZS)

        DO 33 K=1,NZS2
          KN=NZS-K
          K1=2*KN-3
          X1=DTDZS(K1)*THDIFICE(KN-1)
          X2=DTDZS(K1+1)*THDIFICE(KN)
          FT=TSO(KN)+X1*(TSO(KN-1)-TSO(KN))                             &
             -X2*(TSO(KN)-TSO(KN+1))
          DENOM=1.+X1+X2-X2*cotso(K)
          cotso(K+1)=X1/DENOM
          rhtso(K+1)=(FT+X2*rhtso(K))/DENOM
   33  CONTINUE

!************************************************************************
!--- THE HEAT BALANCE EQUATION (Smirnova et al., 1996, EQ. 21,26)
        RHCS=CAPICE(1)
        H=1.
        FKT=TKMS
        D1=cotso(NZS1)
        D2=rhtso(NZS1)
        TN=SOILT
        D9=THDIFICE(1)*RHCS*dzstop
        D10=TKMS*CP*RHO
        R211=.5*CONFLX/DELT
        R21=R211*CP*RHO
        R22=.5/(THDIFICE(1)*DELT*dzstop**2)
        R6=EMISS *STBOLT*.5*TN**4
        R7=R6/TN
        D11=RNET+R6
        TDENOM=D9*(1.-D1+R22)+D10+R21+R7                              &
              +RAINF*CVW*PRCPMS
        FKQ=QKMS*RHO
        R210=R211*RHO
        AA=XLS*(FKQ+R210)/TDENOM
        BB=(D10*TABS+R21*TN+XLS*(QVATM*FKQ                            &
        +R210*QVG)+D11+D9*(D2+R22*TN)                                 &
        +RAINF*CVW*PRCPMS*max(273.15,TABS)                            &
         )/TDENOM
        AA1=AA
        PP=PATM*1.E3
        AA1=AA1/PP
    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
        PRINT *,' VILKA-SEAICE1'
        print *,'D10,TABS,R21,TN,QVATM,FKQ',                          &
                 D10,TABS,R21,TN,QVATM,FKQ
        print *,'RNET, EMISS, STBOLT, SOILT',RNET, EMISS, STBOLT, SOILT
        print *,'R210,QVG,D11,D9,D2,R22,RAINF,CVW,PRCPMS,TDENOM',     &
                 R210,QVG,D11,D9,D2,R22,RAINF,CVW,PRCPMS,TDENOM
        print *,'tn,aa1,bb,pp,fkq,r210',                              &
                 tn,aa1,bb,pp,fkq,r210
    ENDIF
        CALL VILKA(TN,AA1,BB,PP,QS1,TS1,TBQ,KTAU,i,j,iland,isoil)
!--- it is saturation over sea ice
        QVG=QS1
        QSG=QS1
        TSO(1)=min(271.4,TS1)
        QCG=0.
!--- sea ice melting is not included in this simple approach
!--- SOILT - skin temperature
          SOILT=TSO(1)
!---- Final solution for soil temperature - TSO
          DO K=2,NZS
            KK=NZS-K+1
            TSO(K)=min(271.4,rhtso(KK)+cotso(KK)*TSO(K-1))
          END DO
!--- CALCULATION OF DEW USING NEW VALUE OF QSG OR TRANSP IF NO DEW
        DEW=0.

!--- THE DIAGNOSTICS OF SURFACE FLUXES 
          T3      = STBOLT*SOILT*SOILT*SOILT
          UPFLUX  = T3 *SOILT
          XINET   = EMISS*(GLW-UPFLUX)
          RNET    = GSW + XINET
          HFT=-TKMS*CP*RHO*(TABS-SOILT)                        &
               *(P1000mb*0.00001/Patm)**ROVCP
          Q1=-QKMS*RAS*(QVATM - QSG)
        IF (Q1.LE.0.) THEN
! ---  condensation
!-- moisture flux for coupling with PBL
          EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QSG/(1.+QSG))*1.E3
          QFX= XLS*EETA
!-- actual moisture flux from RUC LSM
          DEW=QKMS*(QVATM-QSG)
          EETA= RHO*DEW
        ELSE
! ---  evaporation
!-- moisture flux for coupling with PBL
          EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QVG/(1.+QVG))*1.E3
!          EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QSG/(1.+QSG))*1.E3
! to convert from kg m-2 s-1 to m s-1: 1/rho water=1.e-3************
          QFX= XLS * EETA
!-- actual moisture flux from RUC LSM
          EETA = Q1*1.E3
        ENDIF
          EVAPL=EETA
          S=THDIFICE(1)*CAPICE(1)*DZSTOP*(TSO(1)-TSO(2))
          HFX=HFT
          FLTOT=RNET-HFT-QFX-S

!-------------------------------------------------------------------
   END SUBROUTINE SICE
!-------------------------------------------------------------------



        SUBROUTINE SNOWSOIL (                                  &
!--- input variables
             i,j,isoil,delt,ktau,conflx,nzs,nddzs,nroot,       &
             meltfactor,rhonewsn,                              & ! new
             ILAND,PRCPMS,RAINF,NEWSNOW,snhei,SNWE,SNOWFRAC,   &
             RHOSN,                                            &
             PATM,QVATM,QCATM,                                 &
             GLW,GSW,EMISS,RNET,IVGTYP,                        &
             QKMS,TKMS,PC,cst,rho,vegfrac,alb,znt,             & 
             MYJ,                                              &
!--- soil fixed fields
             QWRTZ,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,     &
             sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,            &
!--- constants
             xlv,CP,rovcp,G0_P,cw,stbolt,TABS,                 &
             KQWRTZ,KICE,KWT,                                  &
!--- output variables
             ilnb,snweprint,snheiprint,rsm,                    &
             soilmois,tso,smfrkeep,keepfr,                     &
             dew,soilt,soilt1,tsnav,                           &
             qvg,qsg,qcg,SMELT,SNOH,SNFLX,SNOM,                &
             edir1,ec1,ett1,eeta,qfx,hfx,s,sublim,             &
             prcpl,runoff1,runoff2,mavail,soilice,             &
             soiliqw,infiltrp                                  )

!***************************************************************
!   Energy and moisture budget for snow, heat diffusion eqns.
!   in snow and soil, Richards eqn. for soil covered with snow
!
!     DELT - time step (s)
!     ktau - numver of time step
!     CONFLX - depth of constant flux layer (m)
!     J,I - the location of grid point
!     IME, JME,  NZS - dimensions of the domain
!     NROOT - number of levels within the root zone
!     PRCPMS - precipitation rate in m/s
!     NEWSNOW - pcpn in soilid form (m)
!     SNHEI, SNWE - snow height and snow water equivalent (m)
!     RHOSN - snow density (kg/m-3)
!     PATM - pressure (bar)
!     QVATM,QCATM - cloud and water vapor mixing ratio
!                   at the first atm. level (kg/kg)
!     GLW, GSW - incoming longwave and absorbed shortwave
!                radiation at the surface (W/m^2)
!     EMISS,RNET - emissivity (0-1) of the ground surface and net
!                  radiation at the surface (W/m^2)
!     QKMS - exchange coefficient for water vapor in the
!              surface layer (m/s)
!     TKMS - exchange coefficient for heat in the surface
!              layer (m/s)
!     PC - plant coefficient (resistance) (0-1)
!     RHO - density of atmosphere near surface (kg/m^3)
!     VEGFRAC - greeness fraction (0-1)
!     RHOCS - volumetric heat capacity of dry soil (J/m^3/K)
!     DQM, QMIN - porosity minus residual soil moisture QMIN (m^3/m^3)
!     REF, WILT - field capacity soil moisture and the
!                 wilting point (m^3/m^3)
!     PSIS - matrix potential at saturation (m)
!     BCLH - exponent for Clapp-Hornberger parameterization
!     KSAT - saturated hydraulic conductivity (m/s)
!     SAT - maximum value of water intercepted by canopy (m)
!     CN - exponent for calculation of canopy water
!     ZSMAIN - main levels in soil (m)
!     ZSHALF - middle of the soil layers (m)
!     DTDZS,DTDZS2 - dt/(2.*dzshalf*dzmain) and dt/dzshalf in soil
!     TBQ - table to define saturated mixing ration
!           of water vapor for given temperature and pressure
!     ilnb - number of layers in snow
!     rsm - liquid water inside snow pack (m)
!     SOILMOIS,TSO - soil moisture (m^3/m^3) and temperature (K)
!     DEW -  dew in (kg/m^2 s)
!     SOILT - skin temperature (K)
!     SOILT1 - snow temperature at 7.5 cm depth (K)
!     TSNAV - average temperature of snow pack (C)
!     QSG,QVG,QCG - saturated mixing ratio, mixing ratio of
!                   water vapor and cloud at the ground
!                   surface, respectively (kg/kg)
!     EDIR1, EC1, ETT1, EETA - direct evaporation, evaporation of
!            canopy water, transpiration (kg m-2 s-1) and total
!            evaporation in (m s-1).
!     QFX, HFX - latent and sensible heat fluxes (W/m^2)
!     S - soil heat flux in the top layer (W/m^2)
!     SUBLIM - snow sublimation (kg/m^2/s)
!     RUNOFF1 - surface runoff (m/s)
!     RUNOFF2 - underground runoff (m)
!     MAVAIL - moisture availability in the top soil layer (0-1)
!     SOILICE - content of soil ice in soil layers (m^3/m^3)
!     SOILIQW - lliquid water in soil layers (m^3/m^3)
!     INFILTRP - infiltration flux from the top of soil domain (m/s)
!     XINET - net long-wave radiation (W/m^2)
!
!*******************************************************************

        IMPLICIT NONE
!-------------------------------------------------------------------
!--- input variables

   INTEGER,  INTENT(IN   )   ::  nroot,ktau,nzs     ,            &
                                 nddzs                         !nddzs=2*(nzs-2)
   INTEGER,  INTENT(IN   )   ::  i,j,isoil

   REAL,     INTENT(IN   )   ::  DELT,CONFLX,PRCPMS            , &
                                 RAINF,NEWSNOW,RHONEWSN,meltfactor

   LOGICAL,    INTENT(IN   )    ::     myj

!--- 3-D Atmospheric variables
   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM
!--- 2-D variables
   REAL                                                        , &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                            RHO, &
                                                             PC, &
                                                        VEGFRAC, &
                                                           QKMS, &
                                                           TKMS

   INTEGER,  INTENT(IN   )   ::                          IVGTYP
!--- soil properties
   REAL                                                        , &
            INTENT(IN   )    ::                           RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                            SAT, &
                                                           WILT

   REAL,     INTENT(IN   )   ::                              CN, &
                                                             CW, &
                                                            XLV, &
                                                           G0_P, & 
                                                         KQWRTZ, &
                                                           KICE, &
                                                            KWT 


   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:4001), INTENT(IN)  ::              TBQ


!--- input/output variables
!-------- 3-d soil moisture and temperature
   REAL,     DIMENSION(  1:nzs )                               , &
             INTENT(INOUT)   ::                             TSO, &
                                                       SOILMOIS, &
                                                       SMFRKEEP

   REAL,  DIMENSION( 1:nzs )                                   , &
             INTENT(INOUT)   ::                          KEEPFR


   INTEGER,  INTENT(INOUT)    ::                           ILAND


!-------- 2-d variables
   REAL                                                        , &
             INTENT(INOUT)   ::                             DEW, &
                                                            CST, &
                                                          EDIR1, &
                                                            EC1, &
                                                           ETT1, &
                                                           EETA, &
                                                          RHOSN, &
                                                         SUBLIM, &
                                                          PRCPL, &
                                                            ALB, &
                                                          EMISS, &
                                                            ZNT, &
                                                         MAVAIL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                            QFX, &
                                                            HFX, &
                                                              S, &
                                                        RUNOFF1, &
                                                        RUNOFF2, &
                                                           SNWE, &
                                                          SNHEI, &
                                                          SMELT, &
                                                           SNOM, &
                                                           SNOH, &
                                                          SNFLX, &
                                                          SOILT, &
                                                         SOILT1, &
                                                       SNOWFRAC, &
                                                          TSNAV

   INTEGER, INTENT(INOUT)    ::                            ILNB

!-------- 1-d variables
   REAL,     DIMENSION(1:NZS), INTENT(OUT)  ::          SOILICE, &
                                                        SOILIQW

   REAL,     INTENT(OUT)                    ::              RSM, &
                                                      SNWEPRINT, &
                                                     SNHEIPRINT
!--- Local variables


   INTEGER ::  nzs1,nzs2,k

   REAL    ::  INFILTRP, TRANSUM                               , &
               SNTH, NEWSN                                     , &
               TABS, T3, UPFLUX, XINET                         , &
               BETA, SNWEPR,EPDT,PP
   REAL    ::  CP,rovcp,G0,LV,xlvm,STBOLT,xlmelt,dzstop        , &
               can,epot,fac,fltot,ft,fq,hft                    , &
               q1,ras,rhoice,sph                               , &
               trans,zn,ci,cvw,tln,tavln,pi                    , &
               DD1,CMC2MS,DRYCAN,WETCAN                        , &
               INFMAX,RIW,DELTSN,H,UMVEG

   REAL,     DIMENSION(1:NZS)  ::  transp,cap,diffu,hydro      , &
                                   thdif,tranf,tav,soilmoism   , &
                                   soilicem,soiliqwm,detal     , &
                                   fwsat,lwsat,told,smold
   REAL                                     ::             drip

   REAL                                     ::             RNET

!-----------------------------------------------------------------

        cvw=cw
        XLMELT=3.35E+5
!-- the next line calculates heat of sublimation of water vapor
        XLVm=XLV+XLMELT
!        STBOLT=5.670151E-8

!--- SNOW flag -- 99
         ILAND=99

!--- DELTSN - is the threshold for splitting the snow layer into 2 layers.
!--- With snow density 400 kg/m^3, this threshold is equal to 7.5 cm,
!--- equivalent to 0.03 m SNWE. For other snow densities the threshold is
!--- computed using SNWE=0.03 m and current snow density.
!--- SNTH - the threshold below which the snow layer is combined with
!--- the top soil layer. SNTH is computed using snwe=0.016 m, and
!--- equals 4 cm for snow density 400 kg/m^3.

           DELTSN=0.0301*1.e3/rhosn
           snth=0.01601*1.e3/rhosn

!tgs when the snow depth is marginlly higher than DELTSN,
! reset DELTSN to half of snow depth.
        IF(SNHEI.GE.DELTSN+SNTH) THEN
! 2-layer model
          if(snhei-deltsn-snth.lt.snth) deltsn=0.5*(snhei-snth)
        ENDIF 

        RHOICE=900.
        CI=RHOICE*2100.
        RAS=RHO*1.E-3
        RIW=rhoice*1.e-3
        MAVAIL=1.
        RSM=0.

        DO K=1,NZS
          TRANSP     (K)=0.
          soilmoism  (k)=0.
          soiliqwm   (k)=0.
          soilice    (k)=0.
          soilicem   (k)=0.
          lwsat      (k)=0.
          fwsat      (k)=0.
          tav        (k)=0.
          cap        (k)=0.
          diffu      (k)=0.
          hydro      (k)=0.
          thdif      (k)=0.  
          tranf      (k)=0.
          detal      (k)=0.
          told       (k)=0.
          smold      (k)=0. 
        ENDDO

        snweprint=0.
        snheiprint=0.
        prcpl=prcpms

!*** DELTSN is the depth of the top layer of snow where
!*** there is a temperature gradient, the rest of the snow layer
!*** is considered to have constant temperature


          NZS1=NZS-1
          NZS2=NZS-2
        DZSTOP=1./(zsmain(2)-zsmain(1))

!----- THE CALCULATION OF THERMAL DIFFUSIVITY, DIFFUSIONAL AND ---
!----- HYDRAULIC CONDUCTIVITY (SMIRNOVA ET AL. 1996, EQ.2,5,6) ---
!tgs - the following loop is added to define the amount of frozen
!tgs - water in soil if there is any
         DO K=1,NZS

         tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(XLMELT*                          &
         (tso(k)-273.15)/tso(k)/9.81/psis)                         &
          **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/riw

!---- melting and freezing is balanced, soil ice cannot increase
       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*rhoice*1.e-3)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif

          ENDDO

          DO K=1,NZS1

         tav(k)=0.5*(tso(k)+tso(k+1))
         soilmoism(k)=0.5*(soilmois(k)+soilmois(k+1))
         tavln=log(tav(k)/273.15)

         if(tavln.lt.0.) then
           soiliqwm(k)=(dqm+qmin)*(XLMELT*                         &
         (tav(k)-273.15)/tav(k)/9.81/psis)                         &
          **(-1./bclh)-qmin
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
           soiliqwm(k)=max(0.,soiliqwm(k))
           soiliqwm(k)=min(soiliqwm(k), soilmoism(k))
           soilicem(k)=(soilmoism(k)-soiliqwm(k))/riw
!---- melting and freezing is balanced, soil ice cannot increase
       if(keepfr(k).eq.1.) then
           soilicem(k)=min(soilicem(k),                            &
                    0.5*(smfrkeep(k)+smfrkeep(k+1)))
           soiliqwm(k)=max(0.,soilmoism(k)-soilicem(k)*riw)
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
       endif

         else
           soilicem(k)=0.
           soiliqwm(k)=soilmoism(k)
           lwsat(k)=dqm+qmin
           fwsat(k)=0.

         endif
          ENDDO

          do k=1,nzs
           if(soilice(k).gt.0.) then
             smfrkeep(k)=soilice(k)
           else
             smfrkeep(k)=soilmois(k)/riw
           endif
          enddo

!******************************************************************
! SOILPROP computes thermal diffusivity, and diffusional and
!          hydraulic condeuctivities
!******************************************************************
          CALL SOILPROP(                                         &
!--- input variables
               nzs,fwsat,lwsat,tav,keepfr,                       &
               soilmois,soiliqw,soilice,                         &
               soilmoism,soiliqwm,soilicem,                      &
!--- soil fixed fields
               QWRTZ,rhocs,dqm,qmin,psis,bclh,ksat,              & 
!--- constants
               riw,xlmelt,CP,G0_P,cvw,ci,                        &
               kqwrtz,kice,kwt,                                  &
!--- output variables
               thdif,diffu,hydro,cap)

!******************************************************************** 
!--- CALCULATION OF CANOPY WATER (Smirnova et al., 1996, EQ.16) AND DEW 
 
        DRIP=0.
        SMELT=0.
        DD1=0.
        H=1.

        FQ=QKMS


!--- If vegfrac.ne.0. then part of falling snow can be
!--- intercepted by the canopy. 

        DEW=0.
        UMVEG=1.-vegfrac
        EPOT = -FQ*(QVATM-QSG) 

      IF(vegfrac.EQ.0.) then
           cst=0.
           drip=0.
      ELSE
       IF(EPOT.GE.0.) THEN
! Evaporation
!         DD1=CST+(NEWSNOW*RHOSN*1.E-3                         &
         DD1=CST+(NEWSNOW*RHOnewSN*1.E-3                         &
!-- this change will not let liquid waer be intercepted by the canopy....
              -DELT*(RAS*EPOT                                 &
!              -DELT*(-PRCPMS+RAS*EPOT                            &
              *(CST/SAT)**CN)) *vegfrac
        ELSE
! Sublimation
         DEW = - EPOT
!         DD1=CST+(NEWSNOW*RHOSN*1.E-3+delt*(                  &
         DD1=CST+(NEWSNOW*RHOnewSN*1.E-3+delt*(               &
!         DD1=CST+(NEWSNOW*RHOSN*1.E-3+delt*(PRCPMS               &
                     +DEW*RAS)) *vegfrac
       ENDIF

        IF(DD1.LT.0.) DD1=0.
      IF (vegfrac.GT.0.) THEN
          CST=DD1
        IF(CST.GT.SAT*vegfrac) THEN
          CST=SAT*vegfrac
          DRIP=DD1-SAT*vegfrac
        ENDIF
      ENDIF


!--- In SFCTMP NEWSNOW is added to SNHEI as if there is no vegetation
!--- With vegetation part of NEWSNOW can be intercepted by canopy until
!--- the saturation is reached. After the canopy saturation is reached
!--- DRIP in the solid form will be added to SNOW cover.

   SNWE=(SNHEI-vegfrac*NEWSNOW)*RHOSN*1.E-3                      &
                  + DRIP                                         

       ENDIF
 
        DRIP=0.
        SNHEI=SNWE*1.e3/RHOSN
          SNWEPR=SNWE

!  check if all snow can evaporate during DT
         BETA=1.
         EPDT = EPOT * RAS *DELT*UMVEG
         IF(SNWEPR.LE.EPDT) THEN 
            BETA=SNWEPR/max(1.e-8,EPDT)
            SNWE=0.
            SNHEI=0.
         ENDIF

          WETCAN=(CST/SAT)**CN
          DRYCAN=1.-WETCAN

!**************************************************************
!  TRANSF computes transpiration function
!**************************************************************
           CALL TRANSF(                                       &
!--- input variables
              nzs,nroot,soiliqw,tabs,                         &
!--- soil fixed fields
              dqm,qmin,ref,wilt,zshalf,                       & 
!--- output variables
              tranf,transum)

!--- Save soil temp and moisture from the beginning of time step
          do k=1,nzs
           told(k)=tso(k)
           smold(k)=soilmois(k)
          enddo

!**************************************************************
! SNOWTEMP solves heat budget and diffusion eqn. in soil
!**************************************************************

    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
print *, 'TSO before calling SNOWTEMP: ', tso
    ENDIF
        CALL SNOWTEMP(                                        &
!--- input variables
             i,j,iland,isoil,                                 &
             delt,ktau,conflx,nzs,nddzs,nroot,                &
             snwe,snwepr,snhei,newsnow,snowfrac,              &
             beta,deltsn,snth,rhosn,rhonewsn,meltfactor,      &  ! add meltfactor
             PRCPMS,RAINF,                                    &
             PATM,TABS,QVATM,QCATM,                           &
             GLW,GSW,EMISS,RNET,                              &
             QKMS,TKMS,PC,rho,vegfrac,                        &
             thdif,cap,drycan,wetcan,cst,                     &
             tranf,transum,dew,mavail,                        &
!--- soil fixed fields
             dqm,qmin,psis,bclh,                              &
             zsmain,zshalf,DTDZS,tbq,                         &
!--- constants
             xlvm,CP,rovcp,G0_P,cvw,stbolt,                   &
!--- output variables
             snweprint,snheiprint,rsm,                        &
             tso,soilt,soilt1,tsnav,qvg,qsg,qcg,              &
             smelt,snoh,snflx,ilnb)

!************************************************************************
!--- RECALCULATION OF DEW USING NEW VALUE OF QSG OR TRANSP IF NO DEW
         DEW=0.
         ETT1=0.
         PP=PATM*1.E3
         QSG= QSN(SOILT,TBQ)/PP
         EPOT = -FQ*(QVATM-QSG)
       IF(EPOT.GE.0.) THEN
! Evaporation
          DO K=1,NROOT
            TRANSP(K)=vegfrac*RAS*FQ*(QVATM-QSG)              &
                     *PC*tranf(K)*DRYCAN/zshalf(NROOT+1)
           IF(TRANSP(K).GT.0.) TRANSP(K)=0.
            ETT1=ETT1-TRANSP(K)
          ENDDO
          DO k=nroot+1,nzs
            transp(k)=0.
          enddo

        ELSE
! Sublimation
          DEW=-EPOT
          DO K=1,NZS
            TRANSP(K)=0.
          ENDDO
        ETT1=0.
        ENDIF

!-- recalculating of frozen water in soil
         DO K=1,NZS
         tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(XLMELT*                    &
         (tso(k)-273.15)/tso(k)/9.81/psis)                   &
          **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/riw
!---- melting and freezing is balanced, soil ice cannot increase
       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*riw)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif
         ENDDO

!*************************************************************************
!--- TQCAN FOR SOLUTION OF MOISTURE BALANCE (Smirnova et al. 1996, EQ.22,28)
!    AND TSO,ETA PROFILES
!*************************************************************************
                CALL SOILMOIST (                                   &
!-- input
               delt,nzs,nddzs,DTDZS,DTDZS2,RIW,                    &
               zsmain,zshalf,diffu,hydro,                          &
               QSG,QVG,QCG,QCATM,QVATM,-PRCPMS,                    &
               0.,TRANSP,0.,                                       &
               0.,SMELT,soilice,vegfrac,                           &
!-- soil properties
               DQM,QMIN,REF,KSAT,RAS,INFMAX,                       &
!-- output
               SOILMOIS,SOILIQW,MAVAIL,RUNOFF1,                    &
               RUNOFF2,infiltrp) 
 
! 4 Nov 07 - update CST for snow melt
        if(snwe.ne.0.) then
         CST=(1.-min(1.,smelt/snwe))*CST
        else
         CST=0.
        endif

!-- Restore land-use parameters if snow is less than threshold
         IF(SNHEI.EQ.0.)  then
          tsnav=soilt-273.15
          CALL SNOWFREE(ivgtyp,myj,emiss,                          & 
                        znt,iland)
          smelt=smelt+snwe/delt
          rsm=0.
!          snwe=0.
         ENDIF

! 21apr2009
! SNOM goes into the passed-in ACSNOM variable in the grid derived type
        SNOM=SNOM+SMELT*DELT*1.e3

!--- KEEPFR is 1 when the temperature and moisture in soil
!--- are both increasing. In this case soil ice should not
!--- be increasing according to the freezing curve.
!--- Some part of ice is melted, but additional water is
!--- getting frozen. Thus, only structure of frozen soil is
!--- changed, and phase changes are not affecting the heat
!--- transfer. This situation may happen when it rains on the
!--- frozen soil.

        do k=1,nzs
       if (soilice(k).gt.0.) then
          if(tso(k).gt.told(k).and.soilmois(k).gt.smold(k)) then
              keepfr(k)=1.
          else
              keepfr(k)=0.
          endif
       endif
        enddo
!--- THE DIAGNOSTICS OF SURFACE FLUXES

        T3      = STBOLT*SOILT*SOILT*SOILT
        UPFLUX  = T3 *SOILT
        XINET   = EMISS*(GLW-UPFLUX)   
        RNET    = GSW + XINET
        HFT=-TKMS*CP*RHO*(TABS-SOILT)                        &
               *(P1000mb*0.00001/Patm)**ROVCP
        Q1 = - FQ*RAS* (QVATM - QSG)

        IF (Q1.LT.0.) THEN
! ---  condensation
        EDIR1=0.
        EC1=0.
        ETT1=0.
! ---  condensation
!-- moisture flux for coupling with PBL
          EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QSG/(1.+QSG))*1.E3
          QFX= XLVm*EETA
!-- actual moisture flux from RUC LSM
          DEW=QKMS*(QVATM-QSG)
          EETA= RHO*DEW
        ELSE
! ---  evaporation
        EDIR1 = Q1*UMVEG *BETA
        EC1 = Q1 * WETCAN
        CMC2MS=CST/DELT
       if(EC1.gt.CMC2MS) cst=0.
        EC1=MIN(CMC2MS,EC1)*vegfrac
!-- moisture flux for coupling with PBL
        EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QVG/(1.+QVG))*1.E3
!        EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QSG/(1.+QSG))*1.E3
! to convert from kg m-2 s-1 to m s-1: 1/rho water=1.e-3************
        QFX= XLVm * EETA
!-- actual moisture flux from RUC LSM
        EETA = (EDIR1 + EC1 + ETT1)*1.E3
       ENDIF
        s=THDIF(1)*CAP(1)*dzstop*(tso(1)-tso(2))
        HFX=HFT
        FLTOT=RNET-HFT-QFX-S

 222     CONTINUE

 1123    FORMAT(I5,8F12.3)
 1133    FORMAT(I7,8E12.4)
  123   format(i6,f6.2,7f8.1)
 122    FORMAT(1X,2I3,6F8.1,F8.3,F8.2)


!      RETURN                                                                 
!      END                                                                    
!-------------------------------------------------------------------
   END SUBROUTINE SNOWSOIL
!-------------------------------------------------------------------

           SUBROUTINE SNOWSEAICE(                               &
            i,j,isoil,delt,ktau,conflx,nzs,nddzs,               &
            meltfactor,rhonewsn,                                &  ! new
            ILAND,PRCPMS,RAINF,NEWSNOW,snhei,SNWE,snowfrac,     &
            RHOSN,PATM,QVATM,QCATM,                             &
            GLW,GSW,EMISS,RNET,                                 &
            QKMS,TKMS,RHO,                                      &
!--- sea ice parameters
            ALB,ZNT,                                            &
            tice,rhosice,capice,thdifice,                       &
            zsmain,zshalf,DTDZS,DTDZS2,tbq,                     &
!--- constants
            xlv,CP,rovcp,cw,stbolt,tabs,                        &
!--- output variables
            ilnb,snweprint,snheiprint,rsm,tso,                  &
            dew,soilt,soilt1,tsnav,qvg,qsg,qcg,                 &
            SMELT,SNOH,SNFLX,SNOM,eeta,                         &
            qfx,hfx,s,sublim,prcpl                              &
                                                                )
!***************************************************************
!   Solving energy budget for snow on sea ice and heat diffusion 
!   eqns. in snow and sea ice
!***************************************************************


        IMPLICIT NONE
!-------------------------------------------------------------------
!--- input variables

   INTEGER,  INTENT(IN   )   ::  ktau,nzs     ,                  &
                                 nddzs                         !nddzs=2*(nzs-2)
   INTEGER,  INTENT(IN   )   ::  i,j,isoil

   REAL,     INTENT(IN   )   ::  DELT,CONFLX,PRCPMS            , &
                                 RAINF,NEWSNOW,RHONEWSN,meltfactor
   real                      ::  rhonewcsn

!--- 3-D Atmospheric variables
   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM
!--- 2-D variables
   REAL                                                        , &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                            RHO, &
                                                           QKMS, &
                                                           TKMS

!--- sea ice properties
   REAL,     DIMENSION(1:NZS)                                  , &
            INTENT(IN   )    ::                                  &
                                                           tice, &
                                                        rhosice, &
                                                         capice, &
                                                       thdifice

   REAL,     INTENT(IN   )   ::                                  &
                                                             CW, &
                                                            XLV

   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:4001), INTENT(IN)  ::              TBQ

!--- input/output variables
!-------- 3-d soil moisture and temperature
   REAL,     DIMENSION(  1:nzs )                               , &
             INTENT(INOUT)   ::                             TSO

   INTEGER,  INTENT(INOUT)    ::                           ILAND


!-------- 2-d variables
   REAL                                                        , &
             INTENT(INOUT)   ::                             DEW, &
                                                           EETA, &
                                                          RHOSN, &
                                                         SUBLIM, &
                                                          PRCPL, &
                                                            ALB, &
                                                          EMISS, &
                                                            ZNT, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                            QFX, &
                                                            HFX, &
                                                              S, &
                                                           SNWE, &
                                                          SNHEI, &
                                                          SMELT, &
                                                           SNOM, &
                                                           SNOH, &
                                                          SNFLX, &
                                                          SOILT, &
                                                         SOILT1, &
                                                       SNOWFRAC, &
                                                          TSNAV

   INTEGER, INTENT(INOUT)    ::                            ILNB

   REAL,     INTENT(OUT)                    ::              RSM, &
                                                      SNWEPRINT, &
                                                     SNHEIPRINT
!--- Local variables


   INTEGER ::  nzs1,nzs2,k,k1,kn,kk
   REAL    ::  x,x1,x2,dzstop,ft,tn,denom

   REAL    ::  SNTH, NEWSN                                     , &
               TABS, T3, UPFLUX, XINET                         , &
               BETA, SNWEPR,EPDT,PP
   REAL    ::  CP,rovcp,G0,LV,xlvm,STBOLT,xlmelt               , &
               epot,fltot,fq,hft,q1,ras,rhoice,ci,cvw          , &
               RIW,DELTSN,H

   REAL    ::  rhocsn,thdifsn,                                   &
               xsn,ddzsn,x1sn,d1sn,d2sn,d9sn,r22sn

   REAL    ::  cotsn,rhtsn,xsn1,ddzsn1,x1sn1,ftsnow,denomsn
   REAL    ::  fso,fsn,                                          &
               FKT,D1,D2,D9,D10,DID,R211,R21,R22,R6,R7,D11,      &
               FKQ,R210,AA,BB,QS1,TS1,TQ2,TX2,                   &
               TDENOM,AA1,RHCS,H1,TSOB, SNPRIM,                  &
               SNODIF,SOH,TNOLD,QGOLD,SNOHGNEW
   REAL,     DIMENSION(1:NZS)  ::  cotso,rhtso

   REAL                        :: RNET,rsmfrac,soiltfrac,hsn
   integer                     ::      nmelt


!-----------------------------------------------------------------
        XLMELT=3.35E+5
!-- the next line calculates heat of sublimation of water vapor
        XLVm=XLV+XLMELT
!        STBOLT=5.670151E-8

!--- SNOW flag -- 99
         ILAND=99

!--- DELTSN - is the threshold for splitting the snow layer into 2 layers.
!--- With snow density 400 kg/m^3, this threshold is equal to 7.5 cm,
!--- equivalent to 0.03 m SNWE. For other snow densities the threshold is
!--- computed using SNWE=0.03 m and current snow density.
!--- SNTH - the threshold below which the snow layer is combined with
!--- the top sea ice layer. SNTH is computed using snwe=0.016 m, and
!--- equals 4 cm for snow density 400 kg/m^3.

           DELTSN=0.0301*1.e3/rhosn
           snth=0.01601*1.e3/rhosn

!tgs when the snow depth is marginlly higher than DELTSN,
! reset DELTSN to half of snow depth.
        IF(SNHEI.GE.DELTSN+SNTH) THEN
! 2-layer model
          if(snhei-deltsn-snth.lt.snth) deltsn=0.5*(snhei-snth)
        ENDIF


        RHOICE=900.
        CI=RHOICE*2100.
        RAS=RHO*1.E-3
        RIW=rhoice*1.e-3
        RSM=0.

        XLMELT=3.35E+5
        RHOCSN=2090.* RHOSN
!18apr08 - add rhonewcsn
        RHOnewCSN=2090.* RHOnewSN
        THDIFSN = 0.265/RHOCSN
        RAS=RHO*1.E-3

        SOILTFRAC=SOILT

        SMELT=0.
        SOH=0.
        SNODIF=0.
        SNOH=0.
        SNOHGNEW=0.
        RSM = 0.
        RSMFRAC = 0.
        fsn=1.
        fso=0.
        hsn=snhei

          NZS1=NZS-1
          NZS2=NZS-2

        QGOLD=QVG
        TNOLD=SOILT
        DZSTOP=1./(ZSMAIN(2)-ZSMAIN(1))

        snweprint=0.
        snheiprint=0.
        prcpl=prcpms

!*** DELTSN is the depth of the top layer of snow where
!*** there is a temperature gradient, the rest of the snow layer
!*** is considered to have constant temperature


        H=1.
        SMELT=0.

        FQ=QKMS
        SNHEI=SNWE*1.e3/RHOSN
          SNWEPR=SNWE

!  check if all snow can evaporate during DT
         BETA=1.
         EPDT = EPOT * RAS *DELT
         IF(SNWEPR.LE.EPDT) THEN
            BETA=SNWEPR/max(1.e-8,EPDT)
            SNWE=0.
            SNHEI=0.
         ENDIF

!******************************************************************************
!       COEFFICIENTS FOR THOMAS ALGORITHM FOR TSO
!******************************************************************************

        cotso(1)=0.
        rhtso(1)=TSO(NZS)
        DO 33 K=1,NZS2
          KN=NZS-K
          K1=2*KN-3
          X1=DTDZS(K1)*THDIFICE(KN-1)
          X2=DTDZS(K1+1)*THDIFICE(KN)
          FT=TSO(KN)+X1*(TSO(KN-1)-TSO(KN))                           &
             -X2*(TSO(KN)-TSO(KN+1))
          DENOM=1.+X1+X2-X2*cotso(K)
          cotso(K+1)=X1/DENOM
          rhtso(K+1)=(FT+X2*rhtso(K))/DENOM
   33  CONTINUE
!--- THE NZS element in COTSO and RHTSO will be for snow
!--- There will be 2 layers in snow if it is deeper than DELTSN+SNTH
       IF(SNHEI.GE.SNTH) then
        if(snhei.le.DELTSN+SNTH) then
!-- 1-layer snow model
         ilnb=1
         snprim=snhei
         soilt1=tso(1)
         tsob=tso(1)
         XSN = DELT/2./(zshalf(2)+0.5*SNPRIM)
         DDZSN = XSN / SNPRIM
         X1SN = DDZSN * thdifsn
         X2 = DTDZS(1)*THDIFICE(1)
         FT = TSO(1)+X1SN*(SOILT-TSO(1))                              &
              -X2*(TSO(1)-TSO(2))
         DENOM = 1. + X1SN + X2 -X2*cotso(NZS1)
         cotso(NZS)=X1SN/DENOM
         rhtso(NZS)=(FT+X2*rhtso(NZS1))/DENOM
         cotsn=cotso(NZS)
         rhtsn=rhtso(NZS)
!*** Average temperature of snow pack (C)
         tsnav=0.5*(soilt+tso(1))                                     &
                     -273.15

        else
!-- 2 layers in snow, SOILT1 is temperasture at DELTSN depth
         ilnb=2
         snprim=deltsn
         tsob=soilt1
         XSN = DELT/2./(0.5*SNHEI)
         XSN1= DELT/2./(zshalf(2)+0.5*(SNHEI-DELTSN))
         DDZSN = XSN / DELTSN
         DDZSN1 = XSN1 / (SNHEI-DELTSN)
         X1SN = DDZSN * thdifsn
         X1SN1 = DDZSN1 * thdifsn
         X2 = DTDZS(1)*THDIFICE(1)
         FT = TSO(1)+X1SN1*(SOILT1-TSO(1))                            &
              -X2*(TSO(1)-TSO(2))
         DENOM = 1. + X1SN1 + X2 - X2*cotso(NZS1)
         cotso(nzs)=x1sn1/denom
         rhtso(nzs)=(ft+x2*rhtso(nzs1))/denom
         ftsnow = soilt1+x1sn*(soilt-soilt1)                          &
               -x1sn1*(soilt1-tso(1))
         denomsn = 1. + X1SN + X1SN1 - X1SN1*cotso(NZS)
         cotsn=x1sn/denomsn
         rhtsn=(ftsnow+X1SN1*rhtso(NZS))/denomsn
!*** Average temperature of snow pack (C)
         tsnav=0.5/snhei*((soilt+soilt1)*deltsn                       &
                     +(soilt1+tso(1))*(SNHEI-DELTSN))                 &
                     -273.15
        endif
       ENDIF

       IF(SNHEI.LT.SNTH.AND.SNHEI.GT.0.) then
!--- snow is too thin to be treated separately, therefore it
!--- is combined with the first sea ice layer.
         fsn=SNHEI/(SNHEI+zsmain(2))
         fso=1.-fsn
         soilt1=tso(1)
         tsob=tso(2)
         snprim=SNHEI+zsmain(2)
         XSN = DELT/2./((zshalf(3)-zsmain(2))+0.5*snprim)
         DDZSN = XSN /snprim
         X1SN = DDZSN * (fsn*thdifsn+fso*thdifice(1))
         X2=DTDZS(2)*THDIFICE(2)
         FT=TSO(2)+X1SN*(SOILT-TSO(2))-                              &
                       X2*(TSO(2)-TSO(3))
         denom = 1. + x1sn + x2 - x2*cotso(nzs-2)
         cotso(nzs1) = x1sn/denom
         rhtso(nzs1)=(FT+X2*rhtso(NZS-2))/denom
         tsnav=0.5*(soilt+tso(1))                                    &
                     -273.15
       ENDIF

!************************************************************************
!--- THE HEAT BALANCE EQUATION 
!18apr08 nmelt is the flag for melting, and SNOH is heat of snow phase changes
       nmelt=0
       SNOH=0.

        EPOT=-QKMS*(QVATM-QSG)
        RHCS=CAPICE(1)
        H=1.
        FKT=TKMS
        D1=cotso(NZS1)
        D2=rhtso(NZS1)
        TN=SOILT
        D9=THDIFICE(1)*RHCS*dzstop
        D10=TKMS*CP*RHO
        R211=.5*CONFLX/DELT
        R21=R211*CP*RHO
        R22=.5/(THDIFICE(1)*DELT*dzstop**2)
        R6=EMISS *STBOLT*.5*TN**4
        R7=R6/TN
        D11=RNET+R6

      IF(SNHEI.GE.SNTH) THEN 
        if(snhei.le.DELTSN+SNTH) then
!--- 1-layer snow
          D1SN = cotso(NZS)
          D2SN = rhtso(NZS)
        else
!--- 2-layer snow
          D1SN = cotsn
          D2SN = rhtsn
        endif
        D9SN= THDIFSN*RHOCSN / SNPRIM
        R22SN = SNPRIM*SNPRIM*0.5/(THDIFSN*DELT)
      ENDIF

       IF(SNHEI.LT.SNTH.AND.SNHEI.GT.0.) then
!--- thin snow is combined with sea ice
         D1SN = D1
         D2SN = D2
         D9SN = (fsn*THDIFSN*RHOCSN+fso*THDIFICE(1)*RHCS)/           &
                 snprim
         R22SN = snprim*snprim*0.5                                   &
                 /((fsn*THDIFSN+fso*THDIFICE(1))*delt)
      ENDIF

      IF(SNHEI.eq.0.)then
!--- all snow is sublimated
        D9SN = D9
        R22SN = R22
        D1SN = D1
        D2SN = D2
    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
        print *,' SNHEI = 0, D9SN,R22SN,D1SN,D2SN: ',D9SN,R22SN,D1SN,D2SN
    ENDIF
      ENDIF

!---- TDENOM for snow
!18apr08  - the iteration start point
 212    continue
        TDENOM = D9SN*(1.-D1SN +R22SN)+D10+R21+R7                    &
              +RAINF*CVW*PRCPMS                                      &
              +RHOnewCSN*NEWSNOW/DELT

        FKQ=QKMS*RHO
        R210=R211*RHO
        AA=XLVM*(BETA*FKQ+R210)/TDENOM
        BB=(D10*TABS+R21*TN+XLVM*(QVATM*                             &
        (BETA*FKQ)                                                   &
        +R210*QVG)+D11+D9SN*(D2SN+R22SN*TN)                          &
        +RAINF*CVW*PRCPMS*max(273.15,TABS)                           &
        + RHOnewCSN*NEWSNOW/DELT*min(273.15,TABS)                    &
!18apr08 - add heat of snow phase change
        -SNOH                                                        &
         )/TDENOM
        AA1=AA
        PP=PATM*1.E3
        AA1=AA1/PP
    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
        print *,'VILKA-SNOW on SEAICE'
        print *,'tn,aa1,bb,pp,fkq,r210',                             &
                 tn,aa1,bb,pp,fkq,r210
    ENDIF

        CALL VILKA(TN,AA1,BB,PP,QS1,TS1,TBQ,KTAU,i,j,iland,isoil)
!--- it is saturation over snow
        QVG=QS1
        QSG=QS1
        QCG=0.

!--- SOILT - skin temperature
        SOILT=TS1

    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
        print *,' AFTER VILKA-SNOW on SEAICE'
        print *,' TS1,QS1: ', ts1,qs1
    ENDIF
! Solution for temperature at 7.5 cm depth and snow-seaice interface
       IF(SNHEI.GE.SNTH) THEN
        if(snhei.gt.DELTSN+SNTH) then
!-- 2-layer snow model
          SOILT1=rhtsn+cotsn*SOILT
          TSO(1)=min(271.4,(rhtso(NZS)+cotso(NZS)*SOILT1))
          tsob=soilt1
        else
!-- 1 layer in snow
          TSO(1)=min(271.4,(rhtso(NZS)+cotso(NZS)*SOILT))
          SOILT1=TSO(1)
          tsob=tso(1)
        endif
       ELSE
         TSO(1)=SOILT
         SOILT1=SOILT
         tsob=SOILT
       ENDIF
!---- Final solution for TSO in sea ice
          DO K=2,NZS
            KK=NZS-K+1
            TSO(K)=min(271.4,(rhtso(KK)+cotso(KK)*TSO(K-1)))
          END DO
!--- For thin snow layer combined with the top sea ice layer
!--- TSO is computed by linear inmterpolation between SOILT
!--- and TSO(2)

      if(nmelt.eq.1) go to 220

!--- IF SOILT > 273.15 F then melting of snow can happen
   IF(SOILT.GT.273.15.AND.SNHEI.GT.0.) THEN
        nmelt = 1
        soiltfrac=snowfrac*273.15+(1.-snowfrac)*SOILT
         QSG= QSN(soiltfrac,TBQ)/PP
         QVG=QSG
        T3      = STBOLT*SOILTfrac*SOILTfrac*SOILTfrac
        UPFLUX  = T3 * SOILTfrac
        XINET   = EMISS*(GLW-UPFLUX)
        RNET = GSW + XINET
         EPOT = -QKMS*(QVATM-QSG)
         Q1=EPOT*RAS

        IF (Q1.LE.0.) THEN
! ---  condensation
          DEW=-EPOT

        QFX= XLVM*RHO*DEW
        EETA=QFX/XLVM
       ELSE
! ---  evaporation
        EETA = Q1 * BETA *1.E3
! to convert from kg m-2 s-1 to m s-1: 1/rho water=1.e-3************
        QFX= - XLVM * EETA
       ENDIF

         HFX=D10*(TABS-soiltfrac)

       IF(SNHEI.GE.SNTH)then
         SOH=thdifsn*RHOCSN*(soiltfrac-TSOB)/SNPRIM
         SNFLX=SOH
       ELSE
         SOH=(fsn*thdifsn*rhocsn+fso*thdifice(1)*rhcs)*                &
              (soiltfrac-TSOB)/snprim
         SNFLX=SOH
       ENDIF
         X= (R21+D9SN*R22SN)*(soiltfrac-TNOLD) +                        &
            XLVM*R210*(QSG-QGOLD)
!-- SNOH is energy flux of snow phase change
        SNOH=RNET+QFX +HFX                                              &
                  +RHOnewCSN*NEWSNOW/DELT*(min(273.15,TABS)-TN)         &
                  -SOH-X+RAINF*CVW*PRCPMS*                              &
                  (max(273.15,TABS)-TN)
        SNOH=AMAX1(0.,SNOH)
!-- SMELT is speed of melting in M/S
        SMELT= SNOH /XLMELT*1.E-3
        SMELT=AMIN1(SMELT,SNWEPR/DELT-BETA*EPOT*RAS)
        SMELT=AMAX1(0.,SMELT)

!18apr08 - Egglston limit
        SMELT= amin1 (smelt, 5.6E-7*meltfactor*max(1.,(soilt-273.15)))
!        SMELT= amin1 (smelt, 5.6E-8*meltfactor*max(1.,(soilt-273.15)))
!*** From Koren et al. (1999) 13% of snow melt stays in the snow pack
!!!        rsm=0.13*smelt*delt
       if(snwepr.gt.0.) then
         rsmfrac=min(0.18,(max(0.08,0.10/snwepr*0.13)))
!       else
!         rsmfrac=0.13
       endif

         rsm=rsmfrac*smelt*delt
!18apr08 rsm is part of melted water that stays in snow as liquid
        SMELT=AMAX1(0.,SMELT-rsm/delt)

        SNOHGNEW=SMELT*XLMELT*1.E3
        SNODIF=AMAX1(0.,(SNOH-SNOHGNEW))

        SNOH=SNOHGNEW


!18apr08 - if snow melt occurred then go into iteration for energy budget
!          solution
!-- correction of liquid equivalent of snow depth
!-- due to evaporation and snow melt
        SNWE = AMAX1(0.,(SNWEPR-                                      &
                    (SMELT+BETA*EPOT*RAS)*DELT                        &
                                         ) )

!--- If all snow melts, then 13% of snow melt we kept in the
!--- snow pack should be added back to snow melt and infiltrate
!--- into soil.
        if(snwe.le.rsm) then
           smelt=smelt+rsm/delt
           snwe=0.
           rsm=0.
        else
!*** Correct snow density on effect of snow melt, melted
!*** from the top of the snow. 13% of melted water
!*** remains in the pack and changes its density.
!*** Eq. 9 (with my correction) in Koren et al. (1999)

          if(snwe.gt.0.) then
         xsn=(rhosn*(snwe-rsm)+1.e3*rsm)/                            &
             snwe
         rhosn=MIN(XSN,400.)

        RHOCSN=2090.* RHOSN
        thdifsn = 0.265/RHOCSN
          endif

        endif

!--- If there is no snow melting then just evaporation
!--- or condensation cxhanges SNWE
      ELSE
               EPOT=-QKMS*(QVATM-QSG)
               SNWE = AMAX1(0.,(SNWEPR-                               &
                    BETA*EPOT*RAS*DELT))

      ENDIF
!*** Correct snow density on effect of snow melt, melted
!*** from the top of the snow. 13% of melted water
!*** remains in the pack and changes its density.
!*** Eq. 9 (with my correction) in Koren et al. (1999)

        SNHEI=SNWE *1.E3 / RHOSN

        snweprint=snwe
!                                              &
!--- if VEGFRAC.ne.0. then some snow stays on the canopy
!--- and should be added to SNWE for water conservation
! 4 Nov 07                    +VEGFRAC*cst
        snheiprint=snweprint*1.E3 / RHOSN

      if(nmelt.eq.1) goto 212
 220  continue

    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
print *, 'snweprint : ',snweprint
print *, 'D9SN,SOILT,TSOB : ', D9SN,SOILT,TSOB
    ENDIF
!--- Compute flux in the top snow layer
      SNFLX=D9SN*(SOILT-TSOB)
      IF(SNHEI.GT.0.) THEN
        if(ilnb.gt.1) then
          tsnav=0.5/snhei*((soilt+soilt1)*deltsn                     &
                    +(soilt1+tso(1))*(SNHEI-DELTSN))                 &
                       -273.15
        else
          tsnav=0.5*(soilt+tso(1)) - 273.15
        endif
      ENDIF
!--- RECALCULATION OF DEW USING NEW VALUE OF QSG 
         DEW=0.
         PP=PATM*1.E3
         QSG= QSN(SOILT,TBQ)/PP
         EPOT = -FQ*(QVATM-QSG)
       IF(EPOT.LT.0.) THEN
! Sublimation
          DEW=-EPOT
        ENDIF
!-- Restore sea-ice parameters if snow is less than threshold
         IF(SNHEI.EQ.0.)  then
          tsnav=soilt-273.15
          smelt=smelt+snwe/delt
          rsm=0.
          emiss=1.
          znt=0.011
          alb=0.55
         ENDIF

        SNOM=SNOM+SMELT*DELT*1.e3

!--- THE DIAGNOSTICS OF SURFACE FLUXES

        T3      = STBOLT*SOILT*SOILT*SOILT
        UPFLUX  = T3 *SOILT
        XINET   = EMISS*(GLW-UPFLUX)
        RNET    = GSW + XINET
        HFT=-TKMS*CP*RHO*(TABS-SOILT)                        &
               *(P1000mb*0.00001/Patm)**ROVCP
        Q1 = - FQ*RAS* (QVATM - QSG)
        IF (Q1.LT.0.) THEN
! ---  condensation
!-- moisture flux for coupling with PBL
          EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QSG/(1.+QSG))*1.E3
          QFX= XLVm*EETA
!-- actual moisture flux from RUC LSM
          DEW=QKMS*(QVATM-QSG)
          EETA= RHO*DEW
          sublim = EETA
        ELSE
! ---  evaporation
!-- moisture flux for coupling with PBL
          EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QVG/(1.+QVG))*1.E3
!          EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QSG/(1.+QSG))*1.E3
! to convert from kg m-2 s-1 to m s-1: 1/rho water=1.e-3************
          QFX= XLVm * EETA
!-- actual moisture flux from RUC LSM
          EETA = Q1*1.E3
          sublim = EETA
        ENDIF

        s=THDIFICE(1)*CAPICE(1)*dzstop*(tso(1)-tso(2))
!        s=D9SN*(SOILT-TSOB)
        HFX=HFT
        FLTOT=RNET-HFT-QFX-S
!------------------------------------------------------------------------
!------------------------------------------------------------------------
   END SUBROUTINE SNOWSEAICE
!------------------------------------------------------------------------


           SUBROUTINE SOILTEMP(                             &
!--- input variables
           i,j,iland,isoil,                                 &
           delt,ktau,conflx,nzs,nddzs,nroot,                &
           PRCPMS,RAINF,PATM,TABS,QVATM,QCATM,              &
           EMISS,RNET,                                      &
           QKMS,TKMS,PC,RHO,VEGFRAC,                        &
           THDIF,CAP,DRYCAN,WETCAN,                         &
           TRANSUM,DEW,MAVAIL,                              &
!--- soil fixed fields
           DQM,QMIN,BCLH,                                   &
           ZSMAIN,ZSHALF,DTDZS,TBQ,                         &
!--- constants
           XLV,CP,G0_P,CVW,STBOLT,                          &
!--- output variables
           TSO,SOILT,QVG,QSG,QCG)

!*************************************************************
!   Energy budget equation and heat diffusion eqn are 
!   solved here and
!
!     DELT - time step (s)
!     ktau - numver of time step
!     CONFLX - depth of constant flux layer (m)
!     IME, JME, KME, NZS - dimensions of the domain 
!     NROOT - number of levels within the root zone
!     PRCPMS - precipitation rate in m/s
!     COTSO, RHTSO - coefficients for implicit solution of
!                     heat diffusion equation
!     THDIF - thermal diffusivity (m^2/s)
!     QSG,QVG,QCG - saturated mixing ratio, mixing ratio of
!                   water vapor and cloud at the ground
!                   surface, respectively (kg/kg)
!     PATM -  pressure [bar]
!     QC3D,QV3D - cloud and water vapor mixing ratio
!                   at the first atm. level (kg/kg)
!     EMISS,RNET - emissivity (0-1) of the ground surface and net
!                  radiation at the surface (W/m^2)
!     QKMS - exchange coefficient for water vapor in the
!              surface layer (m/s)
!     TKMS - exchange coefficient for heat in the surface
!              layer (m/s)
!     PC - plant coefficient (resistance)
!     RHO - density of atmosphere near surface (kg/m^3)
!     VEGFRAC - greeness fraction (0-1)
!     CAP - volumetric heat capacity (J/m^3/K)
!     DRYCAN - dry fraction of vegetated area where
!              transpiration may take place (0-1)
!     WETCAN - fraction of vegetated area covered by canopy
!              water (0-1)
!     TRANSUM - transpiration function integrated over the 
!               rooting zone (m)
!     DEW -  dew in kg/m^2s
!     MAVAIL - fraction of maximum soil moisture in the top
!               layer (0-1)
!     ZSMAIN - main levels in soil (m)
!     ZSHALF - middle of the soil layers (m)
!     DTDZS - dt/(2.*dzshalf*dzmain)
!     TBQ - table to define saturated mixing ration
!           of water vapor for given temperature and pressure
!     TSO - soil temperature (K)
!     SOILT - skin temperature (K)
!
!****************************************************************

        IMPLICIT NONE
!-----------------------------------------------------------------

!--- input variables

   INTEGER,  INTENT(IN   )   ::  nroot,ktau,nzs                , &
                                 nddzs                         !nddzs=2*(nzs-2)
   INTEGER,  INTENT(IN   )   ::  i,j,iland,isoil
   REAL,     INTENT(IN   )   ::  DELT,CONFLX,PRCPMS, RAINF
   REAL,     INTENT(INOUT)   ::  DRYCAN,WETCAN,TRANSUM
!--- 3-D Atmospheric variables
   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM
!--- 2-D variables
   REAL                                                        , &
            INTENT(IN   )    ::                                  &
                                                          EMISS, &
                                                            RHO, &
                                                           RNET, &  
                                                             PC, &
                                                        VEGFRAC, &
                                                            DEW, & 
                                                           QKMS, &
                                                           TKMS

!--- soil properties
   REAL                                                        , &
            INTENT(IN   )    ::                                  &
                                                           BCLH, &
                                                            DQM, &
                                                           QMIN

   REAL,     INTENT(IN   )   ::                              CP, &
                                                            CVW, &
                                                            XLV, &
                                                         STBOLT, &
                                                           TABS, &
                                                           G0_P


   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                          THDIF, &
                                                            CAP

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:4001), INTENT(IN)  ::              TBQ


!--- input/output variables
!-------- 3-d soil moisture and temperature
   REAL,     DIMENSION( 1:nzs )                                , &
             INTENT(INOUT)   ::                             TSO

!-------- 2-d variables
   REAL                                                        , &
             INTENT(INOUT)   ::                                  &
                                                         MAVAIL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                          SOILT


!--- Local variables

   REAL    ::  x,x1,x2,x4,dzstop,can,ft,sph                    , &
               tn,trans,umveg,denom

   REAL    ::  FKT,D1,D2,D9,D10,DID,R211,R21,R22,R6,R7,D11     , &
               PI,H,FKQ,R210,AA,BB,PP,Q1,QS1,TS1,TQ2,TX2       , &
               TDENOM

   REAL    ::  C,CC,AA1,RHCS,H1

   REAL,     DIMENSION(1:NZS)  ::                   cotso,rhtso

   INTEGER ::  nzs1,nzs2,k,k1,kn,kk

!-----------------------------------------------------------------


          NZS1=NZS-1
          NZS2=NZS-2
        dzstop=1./(ZSMAIN(2)-ZSMAIN(1))

        do k=1,nzs
           cotso(k)=0.
           rhtso(k)=0.
        enddo
!******************************************************************************
!       COEFFICIENTS FOR THOMAS ALGORITHM FOR TSO
!******************************************************************************
!         did=2.*(ZSMAIN(nzs)-ZSHALF(nzs))
!         h1=DTDZS(8)*THDIF(nzs-1)*(ZSHALF(nzs)-ZSHALF(nzs-1))/did
!         cotso(1)=h1/(1.+h1)
!         rhtso(1)=(tso(nzs)+h1*(tso(nzs-1)-tso(nzs)))/
!     1         (1.+h1)
        cotso(1)=0.
        rhtso(1)=TSO(NZS)
        DO 33 K=1,NZS2
          KN=NZS-K
          K1=2*KN-3
          X1=DTDZS(K1)*THDIF(KN-1)
          X2=DTDZS(K1+1)*THDIF(KN)
          FT=TSO(KN)+X1*(TSO(KN-1)-TSO(KN))                             &
             -X2*(TSO(KN)-TSO(KN+1))
          DENOM=1.+X1+X2-X2*cotso(K)
          cotso(K+1)=X1/DENOM
          rhtso(K+1)=(FT+X2*rhtso(K))/DENOM
   33  CONTINUE

!************************************************************************
!--- THE HEAT BALANCE EQUATION (Smirnova et al., 1996, EQ. 21,26)

        RHCS=CAP(1)
        H=MAVAIL
        IF(DEW.NE.0.)THEN
          DRYCAN=0.
          WETCAN=1.
        ENDIf
        TRANS=PC*TRANSUM*DRYCAN/ZSHALF(NROOT+1)
        CAN=WETCAN+TRANS
        UMVEG=1.-VEGFRAC
        FKT=TKMS
        D1=cotso(NZS1)
        D2=rhtso(NZS1)
        TN=SOILT
        D9=THDIF(1)*RHCS*dzstop
        D10=TKMS*CP*RHO
        R211=.5*CONFLX/DELT
        R21=R211*CP*RHO
        R22=.5/(THDIF(1)*DELT*dzstop**2)
        R6=EMISS *STBOLT*.5*TN**4
        R7=R6/TN
        D11=RNET+R6
        TDENOM=D9*(1.-D1+R22)+D10+R21+R7                              &
              +RAINF*CVW*PRCPMS
        FKQ=QKMS*RHO
        R210=R211*RHO
        C=VEGFRAC*FKQ*CAN
        CC=C*XLV/TDENOM
        AA=XLV*(FKQ*UMVEG+R210)/TDENOM
        BB=(D10*TABS+R21*TN+XLV*(QVATM*                               &
        (FKQ*UMVEG+C)                                                 & 
        +R210*QVG)+D11+D9*(D2+R22*TN)                                 &
        +RAINF*CVW*PRCPMS*max(273.15,TABS)                            &
         )/TDENOM
        AA1=AA+CC
        PP=PATM*1.E3
        AA1=AA1/PP
    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
        PRINT *,' VILKA-1'
        print *,'D10,TABS,R21,TN,QVATM,FKQ,UMVEG,VEGFRAC,CAN',        &
                 D10,TABS,R21,TN,QVATM,FKQ,UMVEG,VEGFRAC,CAN
        print *,'RNET, EMISS, STBOLT, SOILT',RNET, EMISS, STBOLT, SOILT
        print *,'R210,QVG,D11,D9,D2,R22,RAINF,CVW,PRCPMS,TDENOM',     &
                 R210,QVG,D11,D9,D2,R22,RAINF,CVW,PRCPMS,TDENOM
        print *,'tn,aa1,bb,pp,umveg,fkq,r210,vegfrac',                &
                 tn,aa1,bb,pp,umveg,fkq,r210,vegfrac
    ENDIF
        CALL VILKA(TN,AA1,BB,PP,QS1,TS1,TBQ,KTAU,i,j,iland,isoil)
        TQ2=QVATM
        TX2=TQ2*(1.-H)
        Q1=TX2+H*QS1
        IF(Q1.LT.QS1) GOTO 100
!--- if no saturation - goto 100
!--- if saturation - goto 90
   90   QVG=QS1
        QSG=QS1
        TSO(1)=TS1
        QCG=max(0.,Q1-QS1)
        GOTO 200
  100   BB=BB-AA*TX2
        AA=(AA*H+CC)/PP
    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
        PRINT *,' VILKA-2'
        print *,'D10,TABS,R21,TN,QVATM,FKQ,UMVEG,VEGFRAC,CAN',        &
                 D10,TABS,R21,TN,QVATM,FKQ,UMVEG,VEGFRAC,CAN
        print *,'R210,QVG,D11,D9,D2,R22,RAINF,CVW,PRCPMS,TDENOM',     &
                 R210,QVG,D11,D9,D2,R22,RAINF,CVW,PRCPMS,TDENOM

        print *,'tn,aa1,bb,pp,umveg,fkq,r210,vegfrac',                &
                 tn,aa1,bb,pp,umveg,fkq,r210,vegfrac
    ENDIF

        CALL VILKA(TN,AA,BB,PP,QS1,TS1,TBQ,KTAU,i,j,iland,isoil)
        Q1=TX2+H*QS1
        IF(Q1.GT.QS1) GOTO 90
        QSG=QS1
        QVG=Q1
        TSO(1)=TS1
        QCG=0.
  200   CONTINUE

!--- SOILT - skin temperature
          SOILT=TS1

!---- Final solution for soil temperature - TSO
          DO K=2,NZS
            KK=NZS-K+1
            TSO(K)=rhtso(KK)+cotso(KK)*TSO(K-1)
          END DO

!--------------------------------------------------------------------
   END SUBROUTINE SOILTEMP
!--------------------------------------------------------------------


           SUBROUTINE SNOWTEMP(                                    & 
!--- input variables
           i,j,iland,isoil,                                        &
           delt,ktau,conflx,nzs,nddzs,nroot,                       &
           snwe,snwepr,snhei,newsnow,snowfrac,                     &
           beta,deltsn,snth,rhosn,rhonewsn,meltfactor,             &  ! add meltfactor
           PRCPMS,RAINF,                                           &
           PATM,TABS,QVATM,QCATM,                                  &
           GLW,GSW,EMISS,RNET,                                     &
           QKMS,TKMS,PC,RHO,VEGFRAC,                               &
           THDIF,CAP,DRYCAN,WETCAN,CST,                            &
           TRANF,TRANSUM,DEW,MAVAIL,                               &
!--- soil fixed fields
           DQM,QMIN,PSIS,BCLH,                                     &
           ZSMAIN,ZSHALF,DTDZS,TBQ,                                &
!--- constants
           XLVM,CP,rovcp,G0_P,CVW,STBOLT,                          &
!--- output variables
           SNWEPRINT,SNHEIPRINT,RSM,                               &
           TSO,SOILT,SOILT1,TSNAV,QVG,QSG,QCG,                     &
           SMELT,SNOH,SNFLX,ILNB)

!********************************************************************
!   Energy budget equation and heat diffusion eqn are 
!   solved here to obtain snow and soil temperatures
!
!     DELT - time step (s)
!     ktau - numver of time step
!     CONFLX - depth of constant flux layer (m)
!     IME, JME, KME, NZS - dimensions of the domain 
!     NROOT - number of levels within the root zone
!     PRCPMS - precipitation rate in m/s
!     COTSO, RHTSO - coefficients for implicit solution of
!                     heat diffusion equation
!     THDIF - thermal diffusivity (W/m/K)
!     QSG,QVG,QCG - saturated mixing ratio, mixing ratio of
!                   water vapor and cloud at the ground
!                   surface, respectively (kg/kg)
!     PATM - pressure [bar]
!     QCATM,QVATM - cloud and water vapor mixing ratio
!                   at the first atm. level (kg/kg)
!     EMISS,RNET - emissivity (0-1) of the ground surface and net
!                  radiation at the surface (W/m^2)
!     QKMS - exchange coefficient for water vapor in the
!              surface layer (m/s)
!     TKMS - exchange coefficient for heat in the surface
!              layer (m/s)
!     PC - plant coefficient (resistance)
!     RHO - density of atmosphere near surface (kg/m^3)
!     VEGFRAC - greeness fraction (0-1)
!     CAP - volumetric heat capacity (J/m^3/K)
!     DRYCAN - dry fraction of vegetated area where
!              transpiration may take place (0-1) 
!     WETCAN - fraction of vegetated area covered by canopy
!              water (0-1)
!     TRANSUM - transpiration function integrated over the 
!               rooting zone (m)
!     DEW -  dew in kg/m^2/s
!     MAVAIL - fraction of maximum soil moisture in the top
!               layer (0-1)
!     ZSMAIN - main levels in soil (m)
!     ZSHALF - middle of the soil layers (m)
!     DTDZS - dt/(2.*dzshalf*dzmain)
!     TBQ - table to define saturated mixing ration
!           of water vapor for given temperature and pressure
!     TSO - soil temperature (K)
!     SOILT - skin temperature (K)
!
!*********************************************************************

        IMPLICIT NONE
!---------------------------------------------------------------------
!--- input variables

   INTEGER,  INTENT(IN   )   ::  nroot,ktau,nzs                , &
                                 nddzs                             !nddzs=2*(nzs-2)

   INTEGER,  INTENT(IN   )   ::  i,j,iland,isoil
   REAL,     INTENT(IN   )   ::  DELT,CONFLX,PRCPMS            , &
                                 RAINF,NEWSNOW,DELTSN,SNTH     , &
                                 TABS,TRANSUM,SNWEPR           , &
                                 rhonewsn,meltfactor
   real                      ::  rhonewcsn

!--- 3-D Atmospheric variables
   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM
!--- 2-D variables
   REAL                                                        , &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                            RHO, &
                                                             PC, &
                                                        VEGFRAC, &
                                                           QKMS, &
                                                           TKMS

!--- soil properties
   REAL                                                        , &
            INTENT(IN   )    ::                                  &
                                                           BCLH, &
                                                            DQM, &
                                                           PSIS, &
                                                           QMIN

   REAL,     INTENT(IN   )   ::                              CP, &
                                                          ROVCP, &
                                                            CVW, &
                                                         STBOLT, &
                                                           XLVM, &
                                                            G0_P


   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                          THDIF, &
                                                            CAP, &
                                                          TRANF 

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:4001), INTENT(IN)  ::              TBQ


!--- input/output variables
!-------- 3-d soil moisture and temperature
   REAL,     DIMENSION(  1:nzs )                               , &
             INTENT(INOUT)   ::                             TSO


!-------- 2-d variables
   REAL                                                        , &
             INTENT(INOUT)   ::                             DEW, &
                                                            CST, &
                                                          RHOSN, &
                                                          EMISS, &
                                                         MAVAIL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                           SNWE, &
                                                          SNHEI, &
                                                       SNOWFRAC, &
                                                          SMELT, &
                                                           SNOH, &
                                                          SNFLX, &
                                                          SOILT, &
                                                         SOILT1, &
                                                          TSNAV

   REAL,     INTENT(INOUT)                  ::   DRYCAN, WETCAN           

   REAL,     INTENT(OUT)                    ::              RSM, &
                                                      SNWEPRINT, &
                                                     SNHEIPRINT
   INTEGER,  INTENT(OUT)                    ::             ilnb
!--- Local variables


   INTEGER ::  nzs1,nzs2,k,k1,kn,kk

   REAL    ::  x,x1,x2,x4,dzstop,can,ft,sph,                     &
               tn,trans,umveg,denom

   REAL    ::  cotsn,rhtsn,xsn1,ddzsn1,x1sn1,ftsnow,denomsn

   REAL    ::  t3,upflux,xinet,ras,                              &
               xlmelt,rhocsn,thdifsn,                            &
               beta,epot,xsn,ddzsn,x1sn,d1sn,d2sn,d9sn,r22sn

   REAL    ::  fso,fsn,                                          &
               FKT,D1,D2,D9,D10,DID,R211,R21,R22,R6,R7,D11,      &
               PI,H,FKQ,R210,AA,BB,PP,Q1,QS1,TS1,TQ2,TX2,        &
               TDENOM,C,CC,AA1,RHCS,H1,                          &
               tsob, snprim, sh1, sh2,                           &
               smeltg,snohg,snodif,soh,                          &
               CMC2MS,TNOLD,QGOLD,SNOHGNEW                            

   REAL,     DIMENSION(1:NZS)  ::  transp,cotso,rhtso
   REAL                        ::                         edir1, &
                                                            ec1, &
                                                           ett1, &
                                                           eeta, &
                                                              s, &
                                                            qfx, &
                                                            hfx

   REAL                        :: RNET,rsmfrac,soiltfrac,hsn
   integer                     ::      nmelt

!-----------------------------------------------------------------

       do k=1,nzs
          transp   (k)=0.
          cotso    (k)=0.
          rhtso    (k)=0.
       enddo

    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
print *, 'SNOWTEMP: SNHEI,SNTH,SOILT1: ',SNHEI,SNTH,SOILT1,soilt 
    ENDIF
        XLMELT=3.35E+5
        RHOCSN=2090.* RHOSN
!18apr08 - add rhonewcsn
        RHOnewCSN=2090.* RHOnewSN
        THDIFSN = 0.265/RHOCSN
        RAS=RHO*1.E-3

        SOILTFRAC=SOILT

        SMELT=0.
        SOH=0.
        SMELTG=0.
        SNOHG=0.
        SNODIF=0.
        RSM = 0.
        RSMFRAC = 0.
        fsn=1.
        fso=0.
        hsn=snhei

          NZS1=NZS-1
          NZS2=NZS-2

        QGOLD=QVG
        TNOLD=SOILT
        DZSTOP=1./(ZSMAIN(2)-ZSMAIN(1))

!******************************************************************************
!       COEFFICIENTS FOR THOMAS ALGORITHM FOR TSO
!******************************************************************************
!         did=2.*(ZSMAIN(nzs)-ZSHALF(nzs))
!         h1=DTDZS(8)*THDIF(nzs-1)*(ZSHALF(nzs)-ZSHALF(nzs-1))/did
!         cotso(1)=h1/(1.+h1)
!         rhtso(1)=(tso(nzs)+h1*(tso(nzs-1)-tso(nzs)))/
!     1         (1.+h1)

        cotso(1)=0.
        rhtso(1)=TSO(NZS)
        DO 33 K=1,NZS2
          KN=NZS-K
          K1=2*KN-3
          X1=DTDZS(K1)*THDIF(KN-1)
          X2=DTDZS(K1+1)*THDIF(KN)
          FT=TSO(KN)+X1*(TSO(KN-1)-TSO(KN))                           &
             -X2*(TSO(KN)-TSO(KN+1))
          DENOM=1.+X1+X2-X2*cotso(K)
          cotso(K+1)=X1/DENOM
          rhtso(K+1)=(FT+X2*rhtso(K))/DENOM
   33  CONTINUE
!--- THE NZS element in COTSO and RHTSO will be for snow
!--- There will be 2 layers in snow if it is deeper than DELTSN+SNTH
       IF(SNHEI.GE.SNTH) then
        if(snhei.le.DELTSN+SNTH) then
!-- 1-layer snow model
         ilnb=1
         snprim=snhei
         tsob=tso(1)
         XSN = DELT/2./(zshalf(2)+0.5*SNPRIM)
         DDZSN = XSN / SNPRIM
         X1SN = DDZSN * thdifsn
         X2 = DTDZS(1)*THDIF(1)
         FT = TSO(1)+X1SN*(SOILT-TSO(1))                              &
              -X2*(TSO(1)-TSO(2))
         DENOM = 1. + X1SN + X2 -X2*cotso(NZS1)
         cotso(NZS)=X1SN/DENOM
         rhtso(NZS)=(FT+X2*rhtso(NZS1))/DENOM
         cotsn=cotso(NZS)
         rhtsn=rhtso(NZS)
!*** Average temperature of snow pack (C)
         tsnav=0.5*(soilt+tso(1))                                     &
                     -273.15

        else
!-- 2 layers in snow, SOILT1 is temperasture at DELTSN depth
         ilnb=2
         snprim=deltsn
         tsob=soilt1
         XSN = DELT/2./(0.5*SNPRIM)
         XSN1= DELT/2./(zshalf(2)+0.5*(SNHEI-DELTSN))
         DDZSN = XSN / DELTSN
         DDZSN1 = XSN1 / (SNHEI-DELTSN)
         X1SN = DDZSN * thdifsn
         X1SN1 = DDZSN1 * thdifsn
         X2 = DTDZS(1)*THDIF(1)
         FT = TSO(1)+X1SN1*(SOILT1-TSO(1))                            &
              -X2*(TSO(1)-TSO(2))
         DENOM = 1. + X1SN1 + X2 - X2*cotso(NZS1)
         cotso(nzs)=x1sn1/denom
         rhtso(nzs)=(ft+x2*rhtso(nzs1))/denom
         ftsnow = soilt1+x1sn*(soilt-soilt1)                          &
               -x1sn1*(soilt1-tso(1))
         denomsn = 1. + X1SN + X1SN1 - X1SN1*cotso(NZS)
         cotsn=x1sn/denomsn
         rhtsn=(ftsnow+X1SN1*rhtso(NZS))/denomsn
!*** Average temperature of snow pack (C)
         tsnav=0.5/snhei*((soilt+soilt1)*deltsn                       &
                     +(soilt1+tso(1))*(SNHEI-DELTSN))                 &
                     -273.15
        endif
       ENDIF

       IF(SNHEI.LT.SNTH.AND.SNHEI.GT.0.) then
!--- snow is too thin to be treated separately, therefore it
!--- is combined with the first soil layer.
         fsn=SNHEI/(SNHEI+zsmain(2))
         fso=1.-fsn
         soilt1=tso(1)
         tsob=tso(2)
         snprim=SNHEI+zsmain(2)
         XSN = DELT/2./((zshalf(3)-zsmain(2))+0.5*snprim)
         DDZSN = XSN /snprim
         X1SN = DDZSN * (fsn*thdifsn+fso*thdif(1))
         X2=DTDZS(2)*THDIF(2)
         FT=TSO(2)+X1SN*(SOILT-TSO(2))-                              &
                       X2*(TSO(2)-TSO(3))
         denom = 1. + x1sn + x2 - x2*cotso(nzs-2)
         cotso(nzs1) = x1sn/denom
         rhtso(nzs1)=(FT+X2*rhtso(NZS-2))/denom
         tsnav=0.5*(soilt+tso(1))                                    &
                     -273.15
       ENDIF

!************************************************************************
!--- THE HEAT BALANCE EQUATION (Smirnova et al. 1996, EQ. 21,26)
!18apr08 nmelt is the flag for melting, and SNOH is heat of snow phase changes
       nmelt=0
       SNOH=0.


        ETT1=0.
        EPOT=-QKMS*(QVATM-QSG)
        RHCS=CAP(1)
        H=MAVAIL
        IF(DEW.NE.0.)THEN
          DRYCAN=0.
          WETCAN=1.
        ENDIF
        TRANS=PC*TRANSUM*DRYCAN/ZSHALF(NROOT+1)
        CAN=WETCAN+TRANS
        UMVEG=1.-VEGFRAC
        FKT=TKMS
        D1=cotso(NZS1)
        D2=rhtso(NZS1)
        TN=SOILT
        D9=THDIF(1)*RHCS*dzstop
        D10=TKMS*CP*RHO
        R211=.5*CONFLX/DELT
        R21=R211*CP*RHO
        R22=.5/(THDIF(1)*DELT*dzstop**2)
        R6=EMISS *STBOLT*.5*TN**4
        R7=R6/TN
        D11=RNET+R6

      IF(SNHEI.GE.SNTH) THEN
        if(snhei.le.DELTSN+SNTH) then
!--- 1-layer snow
          D1SN = cotso(NZS)
          D2SN = rhtso(NZS)
        else
!--- 2-layer snow
          D1SN = cotsn
          D2SN = rhtsn
        endif
        D9SN= THDIFSN*RHOCSN / SNPRIM
        R22SN = SNPRIM*SNPRIM*0.5/(THDIFSN*DELT)
      ENDIF

       IF(SNHEI.LT.SNTH.AND.SNHEI.GT.0.) then
!--- thin snow is combined with soil
         D1SN = D1
         D2SN = D2
         D9SN = (fsn*THDIFSN*RHOCSN+fso*THDIF(1)*RHCS)/              &
                 snprim
         R22SN = snprim*snprim*0.5                                   &
                 /((fsn*THDIFSN+fso*THDIF(1))*delt)
      ENDIF

      IF(SNHEI.eq.0.)then
!--- all snow is sublimated
        D9SN = D9
        R22SN = R22
        D1SN = D1
        D2SN = D2
    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
        print *,' SNHEI = 0, D9SN,R22SN,D1SN,D2SN: ',D9SN,R22SN,D1SN,D2SN
    ENDIF
      ENDIF

!---- TDENOM for snow
!18apr08  - the iteration start point
 212    continue
        TDENOM = D9SN*(1.-D1SN +R22SN)+D10+R21+R7                    &
              +RAINF*CVW*PRCPMS                                      &
              +RHOnewCSN*NEWSNOW/DELT

        FKQ=QKMS*RHO
        R210=R211*RHO
        C=VEGFRAC*FKQ*CAN
        CC=C*XLVM/TDENOM
        AA=XLVM*(BETA*FKQ*UMVEG+R210)/TDENOM
        BB=(D10*TABS+R21*TN+XLVM*(QVATM*                             &
        (BETA*FKQ*UMVEG+C)                                           &
        +R210*QVG)+D11+D9SN*(D2SN+R22SN*TN)                          &
        +RAINF*CVW*PRCPMS*max(273.15,TABS)                           &
        + RHOnewCSN*NEWSNOW/DELT*min(273.15,TABS)                    &
!18apr08 - added heat of snow phase change computed in the first iteration
        -SNOH                                                        &
         )/TDENOM
        AA1=AA+CC
        PP=PATM*1.E3
        AA1=AA1/PP
    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
        print *,'VILKA-SNOW'
        print *,'tn,aa1,bb,pp,umveg,fkq,r210,vegfrac',               &
                 tn,aa1,bb,pp,umveg,fkq,r210,vegfrac
    ENDIF

        CALL VILKA(TN,AA1,BB,PP,QS1,TS1,TBQ,KTAU,i,j,iland,isoil)
!--- it is saturation over snow 
        QVG=QS1
        QSG=QS1
        QCG=0.
!--- SOILT - skin temperature
        SOILT=TS1

    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
        print *,' AFTER VILKA-SNOW'
        print *,' TS1,QS1: ', ts1,qs1
    ENDIF

! Solution for temperature at 7.5 cm depth and snow-soil interface
       IF(SNHEI.GE.SNTH) THEN
        if(snhei.gt.DELTSN+SNTH) then
!-- 2-layer snow model
          SOILT1=rhtsn+cotsn*SOILT
          TSO(1)=rhtso(NZS)+cotso(NZS)*SOILT1
          tsob=soilt1
        else
!-- 1 layer in snow
          TSO(1)=rhtso(NZS)+cotso(NZS)*SOILT
          SOILT1=TSO(1)
          tsob=tso(1)
        endif
       ELSE
         TSO(1)=SOILT
         SOILT1=SOILT
         tsob=SOILT
       ENDIF

!---- Final solution for TSO
          DO K=2,NZS
            KK=NZS-K+1
            TSO(K)=rhtso(KK)+cotso(KK)*TSO(K-1)
          END DO
!--- For thin snow layer combined with the top soil layer
!--- TSO is computed by linear inmterpolation between SOILT
!--- and TSO(2)

       if(SNHEI.LT.SNTH.AND.SNHEI.GT.0.)then
          tso(1)=tso(2)+(soilt-tso(2))*fso
          SOILT1=TSO(1)
          tsob=tso(2)
!!!          tsob=tso(1)
       endif

     if(nmelt.eq.1) go to 220

!--- IF SOILT > 273.15 F then melting of snow can happen
   IF(SOILT.GT.273.15.AND.SNHEI.GT.0.) THEN
        nmelt = 1
        soiltfrac=snowfrac*273.15+(1.-snowfrac)*SOILT
         QSG= QSN(soiltfrac,TBQ)/PP
         QVG=QSG
        T3      = STBOLT*SOILTfrac*SOILTfrac*SOILTfrac
        UPFLUX  = T3 * SOILTfrac
        XINET   = EMISS*(GLW-UPFLUX)
        RNET = GSW + XINET
         EPOT = -QKMS*(QVATM-QSG)
         Q1=EPOT*RAS

        IF (Q1.LE.0.) THEN
! ---  condensation
          DEW=-EPOT
          DO K=1,NZS
            TRANSP(K)=0.
          ENDDO

        QFX= XLVM*RHO*DEW
        EETA=QFX/XLVM
       ELSE
! ---  evaporation
          DO K=1,NROOT
            TRANSP(K)=-VEGFRAC*q1                                     &
                      *PC*TRANF(K)*DRYCAN/zshalf(NROOT+1)
           IF(TRANSP(K).GT.0.) TRANSP(K)=0.
            ETT1=ETT1-TRANSP(K)
          ENDDO
          DO k=nroot+1,nzs
            transp(k)=0.
          enddo

        EDIR1 = Q1*UMVEG * BETA
        EC1 = Q1 * WETCAN *VEGFRAC
        CMC2MS=CST/DELT
        EC1=MIN(CMC2MS,EC1)
        EETA = (EDIR1 + EC1 + ETT1)*1.E3
! to convert from kg m-2 s-1 to m s-1: 1/rho water=1.e-3************ 
        QFX= - XLVM * EETA
       ENDIF

         HFX=D10*(TABS-soiltfrac)

       IF(SNHEI.GE.SNTH)then
         SOH=thdifsn*RHOCSN*(soiltfrac-TSOB)/SNPRIM
         SNFLX=SOH
       ELSE
         SOH=(fsn*thdifsn*rhocsn+fso*thdif(1)*rhcs)*                   &
              (soiltfrac-TSOB)/snprim
         SNFLX=SOH
       ENDIF

         X= (R21+D9SN*R22SN)*(soiltfrac-TNOLD) +                        &
            XLVM*R210*(QSG-QGOLD)
!-- SNOH is energy flux of snow phase change
        SNOH=RNET+QFX +HFX                                              & 
                  +RHOnewCSN*NEWSNOW/DELT*(min(273.15,TABS)-TN)         &
                  -SOH-X+RAINF*CVW*PRCPMS*                              &
                  (max(273.15,TABS)-TN) 
        SNOH=AMAX1(0.,SNOH)
!-- SMELT is speed of melting in M/S
        SMELT= SNOH /XLMELT*1.E-3
!        SMELT=AMIN1(SMELT,SNWEPR/DELT-BETA*EPOT*RAS*UMVEG)
        SMELT=AMIN1(SMELT,SNWEPR/DELT-BETA*EPOT*RAS)
        SMELT=AMAX1(0.,SMELT)

!18apr08 - Egglston limit
        SMELT= amin1 (smelt, 5.6E-7*meltfactor*max(1.,(soilt-273.15)))
!        SMELT= amin1 (smelt, 5.6E-8*meltfactor*max(1.,(soilt-273.15)))

!*** From Koren et al. (1999) 13% of snow melt stays in the snow pack
!!!        rsm=0.13*smelt*delt
       if(snwepr.gt.0.) then
         rsmfrac=min(0.18,(max(0.08,0.10/snwepr*0.13)))
!       else
!         rsmfrac=0.13
       endif

         rsm=rsmfrac*smelt*delt
!18apr08 rsm is part of melted water that stays in snow as liquid
        SMELT=AMAX1(0.,SMELT-rsm/delt)

        SNOHGNEW=SMELT*XLMELT*1.E3
        SNODIF=AMAX1(0.,(SNOH-SNOHGNEW))

        SNOH=SNOHGNEW

!-- correction of liquid equivalent of snow depth
!-- due to evaporation and snow melt
        SNWE = AMAX1(0.,(SNWEPR-                                      &
                    (SMELT+BETA*EPOT*RAS)*DELT                        &
!                    (SMELT+BETA*EPOT*RAS*UMVEG)*DELT                 &
                                         ) )

!--- If all snow melts, then 13% of snow melt we kept in the
!--- snow pack should be added back to snow melt and infiltrate
!--- into soil.
        if(snwe.le.rsm) then
           smelt=smelt+rsm/delt
           snwe=0.
           rsm=0.
        else
!*** Correct snow density on effect of snow melt, melted
!*** from the top of the snow. 13% of melted water
!*** remains in the pack and changes its density.
!*** Eq. 9 (with my correction) in Koren et al. (1999)
          if(snwe.gt.0.) then
         xsn=(rhosn*(snwe-rsm)+1.e3*rsm)/                            &
             snwe
         rhosn=MIN(XSN,400.)

        RHOCSN=2090.* RHOSN
        thdifsn = 0.265/RHOCSN
          endif  

        endif

!--- If there is no snow melting then just evaporation
!--- or condensation cxhanges SNWE
      ELSE
               EPOT=-QKMS*(QVATM-QSG)
               SNWE = AMAX1(0.,(SNWEPR-                               &
                    BETA*EPOT*RAS*DELT))
!                    BETA*EPOT*RAS*UMVEG*DELT))

      ENDIF
!*** Correct snow density on effect of snow melt, melted
!*** from the top of the snow. 13% of melted water
!*** remains in the pack and changes its density.
!*** Eq. 9 (with my correction) in Koren et al. (1999)

        SNHEI=SNWE *1.E3 / RHOSN

!18apr08 - if snow melt occurred then go into iteration for energy budget 
!         solution 
     if(nmelt.eq.1) goto 212
 220  continue
!--  Snow melt from the top is done. But if ground surface temperature
!--  is above freezing snow can melt from the bottom. The following
!--  piece of code will check if bottom melting is possible.

        IF(TSO(1).GT.273.15.AND.SNHEI.GT.0.) THEN
          if (snhei.GE.deltsn+snth) then
              hsn = snhei - deltsn
          else
              hsn = snhei
          endif

         soiltfrac=snowfrac*273.15+(1.-snowfrac)*TSO(1)

        SNOHG=(TSO(1)-soiltfrac)*(RHCS*zshalf(2)+                       &
               RHOCSN*0.5*hsn) / DELT
        SNOHG=AMAX1(0.,SNOHG)
        SNODIF=0. 
        SMELTG=SNOHG/XLMELT*1.E-3
! Egglston - empirical limit on snow melt from the bottom of snow pack
        SMELTG=AMIN1(SMELTG, 5.8e-9)

       if(SNWE-SMELTG*DELT.ge.rsm) then
        SNWE = AMAX1(0.,SNWE-SMELTG*DELT)
       else
           smeltg=snwe/delt
           snwe=0.
           rsm=0.
           hsn=0.
       endif

        SNOHGNEW=SMELTG*XLMELT*1.e3
        SNODIF=AMAX1(0.,(SNOHG-SNOHGNEW))
        TSO(1)=soiltfrac                  &
                + SNODIF/(RHCS*zshalf(2)+ RHOCSN*0.5*hsn)* DELT
        SMELT=SMELT+SMELTG
        SNOH=SNOH+SNOHGNEW

       ENDIF

        SNHEI=SNWE *1.E3 / RHOSN

        snweprint=snwe
!                                              &
!--- if VEGFRAC.ne.0. then some snow stays on the canopy
!--- and should be added to SNWE for water conservation
! 4 Nov 07                    +VEGFRAC*cst
        snheiprint=snweprint*1.E3 / RHOSN

    IF ( wrf_at_debug_level(LSMRUC_DBG_LVL) ) THEN
print *, 'snweprint : ',snweprint
print *, 'D9SN,SOILT,TSOB : ', D9SN,SOILT,TSOB
    ENDIF
!--- Compute flux in the top snow layer
      SNFLX=D9SN*(SOILT-TSOB)
      IF(SNHEI.GT.0.) THEN
        if(ilnb.gt.1) then
          tsnav=0.5/snhei*((soilt+soilt1)*deltsn                     &
                    +(soilt1+tso(1))*(SNHEI-DELTSN))                 &
                       -273.15
        else
          tsnav=0.5*(soilt+tso(1)) - 273.15
        endif
      ENDIF

!        return
!        end
!------------------------------------------------------------------------
   END SUBROUTINE SNOWTEMP
!------------------------------------------------------------------------


        SUBROUTINE SOILMOIST (                                  &
!--input parameters
              DELT,NZS,NDDZS,DTDZS,DTDZS2,RIW,                  &
              ZSMAIN,ZSHALF,DIFFU,HYDRO,                        &
              QSG,QVG,QCG,QCATM,QVATM,PRCP,                     &
              QKMS,TRANSP,DRIP,                                 &
              DEW,SMELT,SOILICE,VEGFRAC,                        &
!--soil properties
              DQM,QMIN,REF,KSAT,RAS,INFMAX,                     &
!--output
              SOILMOIS,SOILIQW,MAVAIL,RUNOFF,RUNOFF2,INFILTRP)
!*************************************************************************
!   moisture balance equation and Richards eqn.
!   are solved here 
!   
!     DELT - time step (s)
!     IME,JME,NZS - dimensions of soil domain
!     ZSMAIN - main levels in soil (m)
!     ZSHALF - middle of the soil layers (m)
!     DTDZS -  dt/(2.*dzshalf*dzmain)
!     DTDZS2 - dt/(2.*dzshalf)
!     DIFFU - diffusional conductivity (m^2/s)
!     HYDRO - hydraulic conductivity (m/s)
!     QSG,QVG,QCG - saturated mixing ratio, mixing ratio of
!                   water vapor and cloud at the ground
!                   surface, respectively (kg/kg)
!     QCATM,QVATM - cloud and water vapor mixing ratio
!                   at the first atm. level (kg/kg)
!     PRCP - precipitation rate in m/s
!     QKMS - exchange coefficient for water vapor in the
!              surface layer (m/s)
!     TRANSP - transpiration from the soil layers (m/s)
!     DRIP - liquid water dripping from the canopy to soil (m)
!     DEW -  dew in kg/m^2s
!     SMELT - melting rate in m/s
!     SOILICE - volumetric content of ice in soil (m^3/m^3)
!     SOILIQW - volumetric content of liquid water in soil (m^3/m^3)
!     VEGFRAC - greeness fraction (0-1)
!     RAS - ration of air density to soil density
!     INFMAX - maximum infiltration rate (kg/m^2/s)
!    
!     SOILMOIS - volumetric soil moisture, 6 levels (m^3/m^3)
!     MAVAIL - fraction of maximum soil moisture in the top
!               layer (0-1)
!     RUNOFF - surface runoff (m/s)
!     RUNOFF2 - underground runoff (m)
!     INFILTRP - point infiltration flux into soil (m/s)
!            /(snow bottom runoff) (mm/s)
!
!     COSMC, RHSMC - coefficients for implicit solution of
!                     Richards equation
!******************************************************************
        IMPLICIT NONE
!------------------------------------------------------------------
!--- input variables
   REAL,     INTENT(IN   )   ::  DELT
   INTEGER,  INTENT(IN   )   ::  NZS,NDDZS

! input variables

   REAL,     DIMENSION(1:NZS), INTENT(IN   )  ::         ZSMAIN, &
                                                         ZSHALF, &
                                                          DIFFU, &
                                                          HYDRO, &
                                                         TRANSP, &
                                                        SOILICE, &
                                                         DTDZS2

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     INTENT(IN   )   ::    QSG,QVG,QCG,QCATM,QVATM     , &
                                   QKMS,VEGFRAC,DRIP,PRCP      , &
                                   DEW,SMELT                   , &
                                   DQM,QMIN,REF,KSAT,RAS,RIW
                         
! output

   REAL,     DIMENSION(  1:nzs )                               , &

             INTENT(INOUT)   ::                SOILMOIS,SOILIQW
                                                  
   REAL,     INTENT(INOUT)   ::  MAVAIL,RUNOFF,RUNOFF2,INFILTRP, &
                                                        INFMAX

! local variables

   REAL,     DIMENSION( 1:nzs )  ::  COSMC,RHSMC

   REAL    ::  DZS,R1,R2,R3,R4,R5,R6,R7,R8,R9,R10
   REAL    ::  REFKDT,REFDK,DELT1,F1MAX,F2MAX
   REAL    ::  F1,F2,FD,KDT,VAL,DDT,PX,FK,FKMAX
   REAL    ::  QQ,UMVEG,INFMAX1,TRANS
   REAL    ::  TOTLIQ,FLX,FLXSAT,QTOT
   REAL    ::  DID,X1,X2,X4,DENOM,Q2,Q4
   REAL    ::  dice,fcr,acrt,frzx,sum,cvfrz

   INTEGER ::  NZS1,NZS2,K,KK,K1,KN,ialp1,jj,jk

!******************************************************************************
!       COEFFICIENTS FOR THOMAS ALGORITHM FOR SOILMOIS
!******************************************************************************
          NZS1=NZS-1                                                            
          NZS2=NZS-2

 118      format(6(10Pf23.19))

           do k=1,nzs
            cosmc(k)=0.
            rhsmc(k)=0.
           enddo
 
        DID=(ZSMAIN(NZS)-ZSHALF(NZS))
        X1=ZSMAIN(NZS)-ZSMAIN(NZS1)

!7may09        DID=(ZSMAIN(NZS)-ZSHALF(NZS))*2.
!        DENOM=DID/DELT+DIFFU(NZS1)/X1
!        COSMC(1)=DIFFU(NZS1)/X1/DENOM
!        RHSMC(1)=(SOILMOIS(NZS)*DID/DELT
!     1   +TRANSP(NZS)-(HYDRO(NZS)*SOILMOIS(NZS)
!     1   -HYDRO(NZS1)*SOILMOIS(NZS1))*DID
!     1   /X1) /DENOM

        DENOM=(1.+DIFFU(nzs1)/X1/DID*DELT+HYDRO(NZS)/(2.*DID)*DELT)
        COSMC(1)=DELT*(DIFFU(nzs1)/DID/X1                                &
                    +HYDRO(NZS1)/2./DID)/DENOM
        RHSMC(1)=(SOILIQW(NZS)+TRANSP(NZS)*DELT/                         &
               DID)/DENOM

        DO 330 K=1,NZS2
          KN=NZS-K
          K1=2*KN-3
          X4=2.*DTDZS(K1)*DIFFU(KN-1)
          X2=2.*DTDZS(K1+1)*DIFFU(KN)
          Q4=X4+HYDRO(KN-1)*DTDZS2(KN-1)
          Q2=X2-HYDRO(KN+1)*DTDZS2(KN-1)
          DENOM=1.+X2+X4-Q2*COSMC(K)
          COSMC(K+1)=Q4/DENOM
 330      RHSMC(K+1)=(SOILIQW(KN)+Q2*RHSMC(K)                             &
                   +TRANSP(KN)                                            &
                   /(ZSHALF(KN+1)-ZSHALF(KN))                             &
                   *DELT)/DENOM

! --- MOISTURE BALANCE BEGINS HERE

          TRANS=TRANSP(1)
          UMVEG=1.-VEGFRAC

          RUNOFF=0.
          RUNOFF2=0.
          DZS=ZSMAIN(2)
          R1=COSMC(NZS1)
          R2= RHSMC(NZS1)
          R3=DIFFU(1)/DZS
          R4=R3+HYDRO(1)*.5          
          R5=R3-HYDRO(2)*.5
          R6=QKMS*RAS
!-- Total liquid water available on the top of soil domain
!-- Without snow - 3 sources of water: precipitation,
!--         water dripping from the canopy and dew 
!-- With snow - only one source of water - snow melt

  191   format (f23.19)

        TOTLIQ=UMVEG*PRCP-DRIP/DELT-UMVEG*DEW*RAS-SMELT


        FLX=TOTLIQ
        INFILTRP=TOTLIQ

! -----------     FROZEN GROUND VERSION    -------------------------
!   REFERENCE FROZEN GROUND PARAMETER, CVFRZ, IS A SHAPE PARAMETER OF
!   AREAL DISTRIBUTION FUNCTION OF SOIL ICE CONTENT WHICH EQUALS 1/CV.
!   CV IS A COEFFICIENT OF SPATIAL VARIATION OF SOIL ICE CONTENT.
!   BASED ON FIELD DATA CV DEPENDS ON AREAL MEAN OF FROZEN DEPTH, AND IT
!   CLOSE TO CONSTANT = 0.6 IF AREAL MEAN FROZEN DEPTH IS ABOVE 20 CM.
!   THAT IS WHY PARAMETER CVFRZ = 3 (INT{1/0.6*0.6})
!
!   Current logic doesn't allow CVFRZ be bigger than 3
         CVFRZ = 3.

!-- SCHAAKE/KOREN EXPRESSION for calculation of max infiltration
         REFKDT=3.
         REFDK=3.4341E-6
         DELT1=DELT/86400.
         F1MAX=DQM*ZSHALF(2)
         F2MAX=DQM*(ZSHALF(3)-ZSHALF(2))
         F1=F1MAX*(1.-SOILMOIS(1)/DQM)
         DICE=SOILICE(1)*ZSHALF(2)
         FD=F1
        do k=2,nzs1
         DICE=DICE+(ZSHALF(k+1)-ZSHALF(k))*SOILICE(K)
         FKMAX=DQM*(ZSHALF(k+1)-ZSHALF(k))
         FK=FKMAX*(1.-SOILMOIS(k)/DQM)
         FD=FD+FK
        enddo
         KDT=REFKDT*KSAT/REFDK
         VAL=(1.-EXP(-KDT*DELT1))
         DDT = FD*VAL
         PX= - TOTLIQ * DELT
         IF(PX.LT.0.0) PX = 0.0
         INFMAX1 = (PX*(DDT/(PX+DDT)))/DELT
!         print *,'INFMAX1=,ksat',infmax1,ksat,f1,f2,kdt,val,ddt,px
! -----------     FROZEN GROUND VERSION    --------------------------
!    REDUCTION OF INFILTRATION BASED ON FROZEN GROUND PARAMETERS
!
! ------------------------------------------------------------------

         FRZX= 0.15*((dqm+qmin)/ref) * (0.412 / 0.468)
         FCR = 1.
         IF ( DICE .GT. 1.E-2) THEN
           ACRT = CVFRZ * FRZX / DICE
           SUM = 1.
           IALP1 = CVFRZ - 1
           DO JK = 1,IALP1
              K = 1
              DO JJ = JK+1, IALP1
                K = K * JJ
              END DO
              SUM = SUM + (ACRT ** ( CVFRZ-JK)) / FLOAT (K)
           END DO
           FCR = 1. - EXP(-ACRT) * SUM
         END IF
!          print *,'FCR--------',fcr
         INFMAX1 = INFMAX1* FCR
! -------------------------------------------------------------------

         INFMAX = MAX(INFMAX1,HYDRO(1)*SOILIQW(1))
         INFMAX = MIN(INFMAX, -TOTLIQ)
        
!----
          IF (-TOTLIQ.GT.INFMAX)THEN
            RUNOFF=-TOTLIQ-INFMAX
            FLX=-INFMAX
          ENDIF
! INFILTRP is total infiltration flux in M/S
          INFILTRP=FLX
! Solution of moisture budget
          R7=.5*DZS/DELT
          R4=R4+R7
          FLX=FLX-SOILIQW(1)*R7
          R8=UMVEG*R6
          QTOT=QVATM+QCATM
          R9=TRANS
          R10=QTOT-QSG
!-- evaporation regime
          IF(R10.LE.0.) THEN
            QQ=(R5*R2-FLX+R9)/(R4-R5*R1-R10*R8/(REF-QMIN))
            FLXSAT=-DQM*(R4-R5*R1-R10*R8/(REF-QMIN))                &
                   +R5*R2+R9
          ELSE
!-- dew formation regime
            QQ=(R2*R5-FLX+R8*(QTOT-QCG-QVG)+R9)/(R4-R1*R5)
            FLXSAT=-DQM*(R4-R1*R5)+R2*R5+R8*(QTOT-QVG-QCG)+R9
          END IF

          IF(QQ.LT.0.) THEN
            SOILIQW (1)=1.e-8
            SOILMOIS(1)=1.e-8+soilice(1)*riw

          ELSE IF(QQ.GT.DQM) THEN
!-- saturation
            SOILIQW (1)=DQM
            SOILMOIS(1)=DQM
            RUNOFF2=(FLXSAT-FLX)*DELT
            RUNOFF=RUNOFF+(FLXSAT-FLX)
          ELSE
            SOILIQW (1)=min(dqm,max(1.e-8,QQ))
            SOILMOIS(1)=max(1.e-8,QQ)+soilice(1)*riw
          END IF

!--- FINAL SOLUTION FOR SOILMOIS 
          DO K=2,NZS
            KK=NZS-K+1
            QQ=COSMC(KK)*SOILIQW(K-1)+RHSMC(KK)

           IF (QQ.LT.0.) THEN
            SOILIQW(K) =1.e-8
            SOILMOIS(K)=1.e-8 + soilice(k)*riw

           ELSE IF(QQ.GT.DQM) THEN
!-- saturation
            SOILIQW (K)=DQM
            SOILMOIS(K)=DQM
             IF(K.EQ.NZS)THEN
               RUNOFF2=RUNOFF2+(QQ-DQM)*(ZSMAIN(K)-ZSHALF(K))
             ELSE
               RUNOFF2=RUNOFF2+(QQ-DQM)*(ZSHALF(K+1)-ZSHALF(K))
             ENDIF
           ELSE
            SOILIQW (K)=min(dqm,max(1.e-8,QQ))
            SOILMOIS(K)=max(1.e-8,QQ)+soilice(k)*riw
           END IF
          END DO
!           MAVAIL=min(1.,SOILMOIS(1)/(REF-QMIN))
           MAVAIL=min(1.,SOILMOIS(1)/DQM)
          if (MAVAIL.EQ.0.) MAVAIL=.00001

!        RETURN
!        END
!-------------------------------------------------------------------
    END SUBROUTINE SOILMOIST
!-------------------------------------------------------------------


            SUBROUTINE SOILPROP(                                  &
!--- input variables
         nzs,fwsat,lwsat,tav,keepfr,                              &
         soilmois,soiliqw,soilice,                                &
         soilmoism,soiliqwm,soilicem,                             &
!--- soil fixed fields
         QWRTZ,rhocs,dqm,qmin,psis,bclh,ksat,                     &
!--- constants
         riw,xlmelt,CP,G0_P,cvw,ci,                               & 
         kqwrtz,kice,kwt,                                         &
!--- output variables
         thdif,diffu,hydro,cap)

!******************************************************************
! SOILPROP computes thermal diffusivity, and diffusional and
!          hydraulic condeuctivities
!******************************************************************
! NX,NY,NZS - dimensions of soil domain
! FWSAT, LWSAT - volumetric content of frozen and liquid water
!                for saturated condition at given temperatures (m^3/m^3)
! TAV - temperature averaged for soil layers (K)
! SOILMOIS - volumetric soil moisture at the main soil levels (m^3/m^3)
! SOILMOISM - volumetric soil moisture averaged for layers (m^3/m^3)
! SOILIQWM - volumetric liquid soil moisture averaged for layers (m^3/m^3)
! SOILICEM - volumetric content of soil ice averaged for layers (m^3/m^3)
! THDIF - thermal diffusivity for soil layers (W/m/K)
! DIFFU - diffusional conductivity (m^2/s)
! HYDRO - hydraulic conductivity (m/s)
! CAP - volumetric heat capacity (J/m^3/K)
!
!******************************************************************

        IMPLICIT NONE
!-----------------------------------------------------------------

!--- soil properties
   INTEGER, INTENT(IN   )    ::                            NZS
   REAL                                                        , &
            INTENT(IN   )    ::                           RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                          QWRTZ, &  
                                                           QMIN

   REAL,    DIMENSION(  1:nzs )                                , &
            INTENT(IN   )    ::                        SOILMOIS, &
                                                         keepfr


   REAL,     INTENT(IN   )   ::                              CP, &
                                                            CVW, &
                                                            RIW, &  
                                                         kqwrtz, &
                                                           kice, &
                                                            kwt, &
                                                         XLMELT, &
                                                            G0_P



!--- output variables
   REAL,     DIMENSION(1:NZS)                                  , &
            INTENT(INOUT)  ::      cap,diffu,hydro             , &
                                   thdif,tav                   , &
                                   soilmoism                   , &
                                   soiliqw,soilice             , &
                                   soilicem,soiliqwm           , &
                                   fwsat,lwsat

!--- local variables
   REAL,     DIMENSION(1:NZS)  ::  hk,detal,kasat,kjpl

   REAL    ::  x,x1,x2,x4,ws,wd,fact,fach,facd,psif,ci
   REAL    ::  tln,tavln,tn,pf,a,am,ame,h
   INTEGER ::  nzs1,k

!-- for Johansen thermal conductivity
   REAL    ::  kzero,gamd,kdry,kas,x5,sr,ke       
               

         nzs1=nzs-1

!-- Constants for Johansen (1975) thermal conductivity
         kzero =2.       ! if qwrtz > 0.2


         do k=1,nzs
            detal (k)=0.
            kasat (k)=0.
            kjpl  (k)=0.
            hk    (k)=0.
         enddo

           ws=dqm+qmin
           x1=xlmelt/(g0_p*psis)
           x2=x1/bclh*ws
           x4=(bclh+1.)/bclh
!--- Next 3 lines are for Johansen thermal conduct.
           gamd=(1.-ws)*2700.
           kdry=(0.135*gamd+64.7)/(2700.-0.947*gamd)
           kas=kqwrtz**qwrtz*kzero**(1.-qwrtz)

         DO K=1,NZS1
           tn=tav(k) - 273.15
           wd=ws - riw*soilicem(k)
           psif=psis*100.*(wd/(soiliqwm(k)+qmin))**bclh            &
                * (ws/wd)**3.
!--- PSIF should be in [CM] to compute PF
           pf=log10(abs(psif))
           fact=1.+riw*soilicem(k)
!--- HK is for McCumber thermal conductivity
         IF(PF.LE.5.2) THEN
           HK(K)=420.*EXP(-(PF+2.7))*fact
         ELSE
           HK(K)=.1744*fact
         END IF

           IF(soilicem(k).NE.0.AND.TN.LT.0.) then
!--- DETAL is taking care of energy spent on freezing or released from 
!          melting of soil water

              DETAL(K)=273.15*X2/(TAV(K)*TAV(K))*                  &
                     (TAV(K)/(X1*TN))**X4

              if(keepfr(k).eq.1.) then
                 detal(k)=0.
              endif

           ENDIF

!--- Next 10 lines calculate Johansen thermal conductivity KJPL
           kasat(k)=kas**(1.-ws)*kice**fwsat(k)                    &
                    *kwt**lwsat(k)

           X5=(soilmoism(k)+qmin)/ws
         if(soilicem(k).eq.0.) then
           sr=max(0.101,x5)
           ke=log10(sr)+1.
!--- next 2 lines - for coarse soils
!           sr=max(0.0501,x5)
!           ke=0.7*log10(sr)+1.
         else
           ke=x5
         endif

           kjpl(k)=ke*(kasat(k)-kdry)+kdry

!--- CAP -volumetric heat capacity
            CAP(K)=(1.-WS)*RHOCS                                    &
                  + (soiliqwm(K)+qmin)*CVW                          &
                  + soilicem(K)*CI                                  &
                  + (dqm-soilmoism(k))*CP*1.2                       &
            - DETAL(K)*1.e3*xlmelt

           a=RIW*soilicem(K)

        if((ws-a).lt.0.12)then
           diffu(K)=0.
        else
           H=max(0.,(soilmoism(K)-a)/(max(1.e-8,(dqm-a))))
           facd=1.
        if(a.ne.0.)facd=1.-a/max(1.e-8,soilmoism(K))
          ame=max(1.e-8,dqm-riw*soilicem(K))
!--- DIFFU is diffusional conductivity of soil water
          diffu(K)=-BCLH*KSAT*PSIS/ame*                             &
                  (dqm/ame)**3.                                     &
                  *H**(BCLH+2.)*facd
         endif

!          diffu(K)=-BCLH*KSAT*PSIS/dqm                              &
!                 *H**(BCLH+2.)


!--- thdif - thermal diffusivity
!           thdif(K)=HK(K)/CAP(K)
!--- Use thermal conductivity from Johansen (1975)
            thdif(K)=KJPL(K)/CAP(K)

         END DO

         DO K=1,NZS

         if((ws-riw*soilice(k)).lt.0.12)then
            hydro(k)=0.
         else
            fach=1.
          if(soilice(k).ne.0.)                                     &
             fach=1.-riw*soilice(k)/max(1.e-8,soilmois(k))
         am=max(1.e-8,dqm-riw*soilice(k))
!--- HYDRO is hydraulic conductivity of soil water
          hydro(K)=KSAT/am*                                        & 
                  (soiliqw(K)/am)                                  &
                  **(2.*BCLH+2.)                                   &
                  * fach
         endif

       ENDDO

!       RETURN
!       END

!-----------------------------------------------------------------------
   END SUBROUTINE SOILPROP
!-----------------------------------------------------------------------


           SUBROUTINE TRANSF(                                    &
!--- input variables
              nzs,nroot,soiliqw,tabs,                            &
!--- soil fixed fields
              dqm,qmin,ref,wilt,zshalf,                          &
!--- output variables
              tranf,transum)

!-------------------------------------------------------------------
!--- TRANF(K) - THE TRANSPIRATION FUNCTION (Smirnova et al. 1996, EQ. 18,19)
!*******************************************************************
! NX,NY,NZS - dimensions of soil domain
! SOILIQW - volumetric liquid soil moisture at the main levels (m^3/m^3)
! TRANF - the transpiration function at levels (m)
! TRANSUM - transpiration function integrated over the rooting zone (m)
!
!*******************************************************************
        IMPLICIT NONE
!-------------------------------------------------------------------

!--- input variables

   INTEGER,  INTENT(IN   )   ::  nroot,nzs

   REAL                                                        , &
            INTENT(IN   )    ::                            TABS
!--- soil properties
   REAL                                                        , &
            INTENT(IN   )    ::                             DQM, &
                                                           QMIN, &
                                                            REF, &
                                                           WILT

   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::          soiliqw,  &
                                                         ZSHALF

!-- output 
   REAL,     DIMENSION(1:NZS), INTENT(OUT)  ::            TRANF
   REAL,     INTENT(OUT)  ::                            TRANSUM  

!-- local variables
   REAL    ::  totliq, did
   INTEGER ::  k

!-- for non-linear root distribution
   REAL    ::  gx,sm1,sm2,sm3,sm4,ap0,ap1,ap2,ap3,ap4
   REAL    ::  FTEM
   REAL,     DIMENSION(1:NZS)   ::           PART
!--------------------------------------------------------------------

        do k=1,nzs
           part(k)=0.
        enddo

        transum=0.
        totliq=soiliqw(1)+qmin
           sm1=totliq
           sm2=sm1*sm1
           sm3=sm2*sm1
           sm4=sm3*sm1
           ap0=0.299
           ap1=-8.152
           ap2=61.653
           ap3=-115.876
           ap4=59.656
           gx=ap0+ap1*sm1+ap2*sm2+ap3*sm3+ap4*sm4
          if(totliq.ge.ref) gx=1.
          if(totliq.le.0.) gx=0.
          if(gx.gt.1.) gx=1.
          if(gx.lt.0.) gx=0.
        DID=zshalf(2)
          part(1)=DID*gx
!--- air temperature function
!     Avissar (1985) and AX 7/95
        IF (TABS .LE. 302.15) THEN
          FTEM = 1.0 / (1.0 + EXP(-0.41 * (TABS - 282.05)))
        ELSE
          FTEM = 1.0 / (1.0 + EXP(0.5 * (TABS - 314.0)))
        ENDIF
!---
        IF(TOTLIQ.GT.REF) THEN
          TRANF(1)=DID
        ELSE IF(TOTLIQ.LE.WILT) THEN
          TRANF(1)=0.
        ELSE
          TRANF(1)=(TOTLIQ-WILT)/(REF-WILT)*DID
        ENDIF 
!-- uncomment next line for non-linear root distribution
!cc           TRANF(1)=part(1)
          TRANF(1)=TRANF(1)*FTEM

        DO K=2,NROOT
        totliq=soiliqw(k)+qmin
           sm1=totliq
           sm2=sm1*sm1
           sm3=sm2*sm1
           sm4=sm3*sm1
           gx=ap0+ap1*sm1+ap2*sm2+ap3*sm3+ap4*sm4
          if(totliq.ge.ref) gx=1.
          if(totliq.le.0.) gx=0.
          if(gx.gt.1.) gx=1.
          if(gx.lt.0.) gx=0.
          DID=zshalf(K+1)-zshalf(K)
          part(k)=did*gx
        IF(totliq.GE.REF) THEN
          TRANF(K)=DID
        ELSE IF(totliq.LE.WILT) THEN
          TRANF(K)=0.
        ELSE
          TRANF(K)=(totliq-WILT)                                &
                /(REF-WILT)*DID
        ENDIF
!-- uncomment next line for non-linear root distribution
!cc          TRANF(k)=part(k)
          TRANF(k)=TRANF(k)*FTEM
        END DO

!-- TRANSUM - total for the rooting zone
          transum=0.
        DO K=1,NROOT
          transum=transum+tranf(k)
        END DO

!        RETURN
!        END
!-----------------------------------------------------------------
   END SUBROUTINE TRANSF
!-----------------------------------------------------------------


       SUBROUTINE VILKA(TN,D1,D2,PP,QS,TS,TT,NSTEP,ii,j,iland,isoil)
!--------------------------------------------------------------
!--- VILKA finds the solution of energy budget at the surface
!--- using table T,QS computed from Clausius-Klapeiron
!--------------------------------------------------------------
   REAL,     DIMENSION(1:4001),  INTENT(IN   )   ::  TT
   REAL,     INTENT(IN  )   ::  TN,D1,D2,PP
   INTEGER,  INTENT(IN  )   ::  NSTEP,ii,j,iland,isoil

   REAL,     INTENT(OUT  )  ::  QS, TS

   REAL    ::  F1,T1,T2,RN
   INTEGER ::  I,I1
     
       I=(TN-1.7315E2)/.05+1
       T1=173.1+FLOAT(I)*.05
       F1=T1+D1*TT(I)-D2
       I1=I-F1/(.05+D1*(TT(I+1)-TT(I)))
       I=I1
       IF(I.GT.4000.OR.I.LT.1) GOTO 1
  10   I1=I
       T1=173.1+FLOAT(I)*.05
       F1=T1+D1*TT(I)-D2
       RN=F1/(.05+D1*(TT(I+1)-TT(I)))
       I=I-INT(RN)                      
       IF(I.GT.4000.OR.I.LT.1) GOTO 1
       IF(I1.NE.I) GOTO 10
       TS=T1-.05*RN
       QS=(TT(I)+(TT(I)-TT(I+1))*RN)/PP
       GOTO 20
   1   PRINT *,'     AVOST IN VILKA      '
!       WRITE(12,*)'AVOST',TN,D1,D2,PP,NSTEP
       PRINT *,TN,D1,D2,PP,NSTEP,I,TT(i),ii,j,iland,isoil
       CALL wrf_error_fatal ('     AVOST IN VILKA      ' )
   20  CONTINUE
!       RETURN
!       END
!-----------------------------------------------------------------------
   END SUBROUTINE VILKA
!-----------------------------------------------------------------------


     SUBROUTINE SOILVEGIN  ( IVGTYP,ISLTYP,MYJ,                         &
                     IFOREST,EMISS,PC,ZNT,QWRTZ,                        &
                     RHOCS,BCLH,DQM,KSAT,PSIS,QMIN,REF,WILT        )

!************************************************************************
!  Set-up soil and vegetation Parameters in the case when
!  snow disappears during the forecast and snow parameters
!  shold be replaced by surface parameters according to
!  soil and vegetation types in this point.
!
!        Output:
!
!
!             Soil parameters:
!               DQM: MAX soil moisture content - MIN (m^3/m^3)
!               REF:        Reference soil moisture (m^3/m^3)
!               WILT: Wilting PT soil moisture contents (m^3/m^3)
!               QMIN: Air dry soil moist content limits (m^3/m^3)
!               PSIS: SAT soil potential coefs. (m)
!               KSAT:  SAT soil diffusivity/conductivity coefs. (m/s)
!               BCLH: Soil diffusivity/conductivity exponent.
!
! ************************************************************************
   IMPLICIT NONE
!---------------------------------------------------------------------------
      integer,   parameter      ::      nsoilclas=19
      integer,   parameter      ::      nvegclas=24+3
      integer,   parameter      ::      iwater=16
      integer,   parameter      ::      ilsnow=99


!---    soiltyp classification according to STATSGO(nclasses=16)
!
!             1          SAND                  SAND
!             2          LOAMY SAND            LOAMY SAND
!             3          SANDY LOAM            SANDY LOAM
!             4          SILT LOAM             SILTY LOAM
!             5          SILT                  SILTY LOAM
!             6          LOAM                  LOAM
!             7          SANDY CLAY LOAM       SANDY CLAY LOAM
!             8          SILTY CLAY LOAM       SILTY CLAY LOAM
!             9          CLAY LOAM             CLAY LOAM
!            10          SANDY CLAY            SANDY CLAY
!            11          SILTY CLAY            SILTY CLAY
!            12          CLAY                  LIGHT CLAY
!            13          ORGANIC MATERIALS     LOAM
!            14          WATER
!            15          BEDROCK
!                        Bedrock is reclassified as class 14
!            16          OTHER (land-ice)
!            17          Playa
!            18          Lava
!            19          White Sand
!
!----------------------------------------------------------------------
         REAL  LQMA(nsoilclas),LRHC(nsoilclas),                       &
               LPSI(nsoilclas),LQMI(nsoilclas),                       &
               LBCL(nsoilclas),LKAS(nsoilclas),                       &
               LWIL(nsoilclas),LREF(nsoilclas),                       &
               DATQTZ(nsoilclas)
!-- LQMA Rawls et al.[1982]
!        DATA LQMA /0.417, 0.437, 0.453, 0.501, 0.486, 0.463, 0.398,
!     &  0.471, 0.464, 0.430, 0.479, 0.475, 0.439, 1.0, 0.20, 0.401/
!---
!-- Clapp, R. and G. Hornberger, 1978: Empirical equations for some soil
!   hydraulic properties, Water Resour. Res., 14, 601-604.

!-- Clapp et al. [1978]
     DATA LQMA /0.395, 0.410, 0.435, 0.485, 0.485, 0.451, 0.420,      &
                0.477, 0.476, 0.426, 0.492, 0.482, 0.451, 1.0,        &
                0.20,  0.435, 0.468, 0.200, 0.339/

!-- LREF Rawls et al.[1982]
!        DATA LREF /0.091, 0.125, 0.207, 0.330, 0.360, 0.270, 0.255,
!     &  0.366, 0.318, 0.339, 0.387, 0.396, 0.329, 1.0, 0.108, 0.283/

!-- Clapp et al. [1978]
        DATA LREF /0.174, 0.179, 0.249, 0.369, 0.369, 0.314, 0.299,   &
                   0.357, 0.391, 0.316, 0.409, 0.400, 0.314, 1.,      &
                   0.1,   0.249, 0.454, 0.17,  0.236/

!-- LWIL Rawls et al.[1982]
!        DATA LWIL/0.033, 0.055, 0.095, 0.133, 0.133, 0.117, 0.148,
!     &  0.208, 0.197, 0.239, 0.250, 0.272, 0.066, 0.0, 0.006, 0.029/

!-- Clapp et al. [1978]
        DATA LWIL/0.068, 0.075, 0.114, 0.179, 0.179, 0.155, 0.175,    &
                  0.218, 0.250, 0.219, 0.283, 0.286, 0.155, 0.0,      &
                  0.006, 0.114, 0.030, 0.006, 0.01/

!        DATA LQMI/0.010, 0.028, 0.047, 0.084, 0.084, 0.066, 0.067,
!     &  0.120, 0.103, 0.100, 0.126, 0.138, 0.066, 0.0, 0.006, 0.028/

!-- Carsel and Parrish [1988]
        DATA LQMI/0.045, 0.057, 0.065, 0.067, 0.034, 0.078, 0.10,     &
                  0.089, 0.095, 0.10,  0.070, 0.068, 0.078, 0.0,      &
                  0.004, 0.065, 0.020, 0.004, 0.008/

!-- LPSI Cosby et al[1984]
!        DATA LPSI/0.060, 0.036, 0.141, 0.759, 0.759, 0.355, 0.135,
!     &  0.617, 0.263, 0.098, 0.324, 0.468, 0.355, 0.0, 0.069, 0.036/
!     &  0.617, 0.263, 0.098, 0.324, 0.468, 0.355, 0.0, 0.069, 0.036/

!-- Clapp et al. [1978]
       DATA LPSI/0.121, 0.090, 0.218, 0.786, 0.786, 0.478, 0.299,     &
                 0.356, 0.630, 0.153, 0.490, 0.405, 0.478, 0.0,       &
                 0.121, 0.218, 0.468, 0.069, 0.069/

!-- LKAS Rawls et al.[1982]
!        DATA LKAS/5.83E-5, 1.70E-5, 7.19E-6, 1.89E-6, 1.89E-6,
!     &  3.67E-6, 1.19E-6, 4.17E-7, 6.39E-7, 3.33E-7, 2.50E-7,
!     &  1.67E-7, 3.38E-6, 0.0, 1.41E-4, 1.41E-5/

!-- Clapp et al. [1978]
        DATA LKAS/1.76E-4, 1.56E-4, 3.47E-5, 7.20E-6, 7.20E-6,         &
                  6.95E-6, 6.30E-6, 1.70E-6, 2.45E-6, 2.17E-6,         &
                  1.03E-6, 1.28E-6, 6.95E-6, 0.0,     1.41E-4,         &
                  3.47E-5, 1.28E-6, 1.41E-4, 1.76E-4/

!-- LBCL Cosby et al [1984]
!        DATA LBCL/2.79,  4.26,  4.74,  5.33,  5.33,  5.25,  6.66,
!     &  8.72,  8.17,  10.73, 10.39, 11.55, 5.25,  0.0, 2.79,  4.26/

!-- Clapp et al. [1978]
        DATA LBCL/4.05,  4.38,  4.90,  5.30,  5.30,  5.39,  7.12,      &
                  7.75,  8.52, 10.40, 10.40, 11.40,  5.39,  0.0,       &
                  4.05,  4.90, 11.55,  2.79,  2.79/

        DATA LRHC /1.47,1.41,1.34,1.27,1.27,1.21,1.18,1.32,1.23,       &
                   1.18,1.15,1.09,1.21,4.18,2.03,2.10,1.09,2.03,1.47/

        DATA DATQTZ/0.92,0.82,0.60,0.25,0.10,0.40,0.60,0.10,0.35,      &
                    0.52,0.10,0.25,0.00,0.,0.60,0.0,0.25,0.60,0.92/

!--------------------------------------------------------------------------
!
!     USGS Vegetation Types
!
!    1:   Urban and Built-Up Land
!    2:   Dryland Cropland and Pasture
!    3:   Irrigated Cropland and Pasture
!    4:   Mixed Dryland/Irrigated Cropland and Pasture
!    5:   Cropland/Grassland Mosaic
!    6:   Cropland/Woodland Mosaic
!    7:   Grassland
!    8:   Shrubland
!    9:   Mixed Shrubland/Grassland
!   10:   Savanna
!   11:   Deciduous Broadleaf Forest
!   12:   Deciduous Needleleaf Forest
!   13:   Evergreen Broadleaf Forest
!   14:   Evergreen Needleleaf Fores
!   15:   Mixed Forest
!   16:   Water Bodies
!   17:   Herbaceous Wetland
!   18:   Wooded Wetland
!   19:   Barren or Sparsely Vegetated
!   20:   Herbaceous Tundra
!   21:   Wooded Tundra
!   22:   Mixed Tundra
!   23:   Bare Ground Tundra
!   24:   Snow or Ice
!
!   25:   Playa
!   26:   Lava
!   27:   White Sand


!----  Below are the arrays for the vegetation parameters
         REAL LALB(nvegclas),LMOI(nvegclas),LEMI(nvegclas),            &
              LROU(nvegclas),LTHI(nvegclas),LSIG(nvegclas),            &
              LPC(nvegclas), NROTBL(nvegclas)

!************************************************************************
!----     vegetation parameters
!
!-- USGS model
!
        DATA  LALB/.18,.17,.18,.18,.18,.16,.19,.22,.20,.20,.16,.14,     &
                   .12,.12,.13,.08,.14,.14,.25,.15,.15,.15,.25,.55,     &
                   .30,.16,.60 /
        DATA LEMI/.88,4*.92,.93,.92,.88,.9,.92,.93,.94,                 &
                  .95,.95,.94,.98,.95,.95,.85,.92,.93,.92,.85,.95,      &
                  .85,.85,.90 /
!-- Roughness length is changed for forests and some others
!        DATA LROU/.5,.06,.075,.065,.05,.2,.075,.1,.11,.15,.8,.85,       &
!                  2.0,1.0,.563,.0001,.2,.4,.05,.1,.15,.1,.065,.05/
         DATA LROU/.5,.06,.075,.065,.05,.2,.075,.1,.11,.15,.5,.5,       & 
                   .5,.5,.5,.0001,.2,.4,.05,.1,.15,.1,.065,.05,         &
                   .01,.15,.01 /

        DATA LMOI/.1,.3,.5,.25,.25,.35,.15,.1,.15,.15,.3,.3,            &
                  .5,.3,.3,1.,.6,.35,.02,.5,.5,.5,.02,.95,.40,.50,.40/
!
!---- still needs to be corrected
!
!       DATA LPC/ 15*.8,0.,.8,.8,.5,.5,.5,.5,.5,.0/
       DATA LPC /0.4,0.3,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,5*0.55,0.,0.55,0.55,                   &
                 0.3,0.3,0.4,0.4,0.3,0.,.3,0.,0./

! used in RUC       DATA LPC /0.6,6*0.8,0.7,0.75,6*0.8,0.,0.8,0.8,                   &
!                 0.5,0.7,0.6,0.7,0.5,0./


!***************************************************************************


   INTEGER      ::                &
                                                         IVGTYP, &
                                                         ISLTYP

   LOGICAL,    INTENT(IN   )    ::     myj

   REAL                                                        , &
            INTENT (  OUT)            ::                     pc

   REAL                                                        , &
            INTENT (INOUT   )         ::                  emiss, &
                                                            znt
!--- soil properties
   REAL                                                        , &
            INTENT(  OUT)    ::                           RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                           WILT

   INTEGER, DIMENSION( 1:(nvegclas) )                          , &
            INTENT (  OUT)            ::                iforest



   INTEGER, DIMENSION( 1:(nvegclas) )   ::   if1
   INTEGER   ::   kstart, kfin, lstart, lfin
   INTEGER   ::   i,j,k

!***********************************************************************
!        DATA ZS1/0.0,0.05,0.20,0.40,1.6,3.0/   ! o -  levels in soil
!        DATA ZS2/0.0,0.025,0.125,0.30,1.,2.3/   ! x - levels in soil
        DATA IF1/12*0,1,1,1,12*0/

          do k=1,nvegclas
             iforest(k)=if1(k)
          enddo


!        EMISS = LEMI(IVGTYP)
        EMISS = LEMITBL(IVGTYP)
! When MYJ sfc scheme is used - better use recommended in MYJSFCINIT
! values of roughness length, and not redefine it here.
! The table in this routine is the one we use in RUC with RUC LSM.

!tgs 3 Oct 2007 - MYJSFCINIT roughness produced unrealistic fluxes,
!tgs - will not use it any more!
!tgs        if (.not. myj) then
!        ZNT   = LROU(IVGTYP)
        ZNT   = Z0TBL(IVGTYP)
!tgs        endif

!        PC     = LPC (IVGTYP)
        PC     = PCTBL(IVGTYP)
!        RHOCS  = LRHC(ISLTYP)*1.E6
        RHOCS  = HC(ISLTYP)*1.E6

! parameters from SOILPARM.TBL
          BCLH   = BB(ISLTYP)
          DQM    = MAXSMC(ISLTYP)-                               &
                   DRYSMC(ISLTYP)
          KSAT   = SATDK(ISLTYP)
          PSIS   = - SATPSI(ISLTYP)
          QMIN   = DRYSMC(ISLTYP)
          REF    = REFSMC(ISLTYP)
          WILT   = WLTSMC(ISLTYP)
          QWRTZ  = QTZ(ISLTYP)

! parameters from the look-up tables
!          BCLH   = LBCL(ISLTYP)
!          DQM    = LQMA(ISLTYP)-                               &
!                   LQMI(ISLTYP)
!          KSAT   = LKAS(ISLTYP)
!          PSIS   = - LPSI(ISLTYP)
!          QMIN   = LQMI(ISLTYP)
!          REF    = LREF(ISLTYP)
!          WILT   = LWIL(ISLTYP)
!          QWRTZ  = DATQTZ(ISLTYP)

!--------------------------------------------------------------------------
   END SUBROUTINE SOILVEGIN
!--------------------------------------------------------------------------


      SUBROUTINE SNOWFREE (ivgtyp,myj,emiss,znt,iland)
!************************************************************************
!  Set-up soil and vegetation Parameters in the case when
!  snow disappears during the forecast and snow parameters
!  shold be replaced by surface parameters according to
!  soil and vegetation types in this point.
!
!***************************************************************************
   IMPLICIT NONE
!---------------------------------------------------------------------------
   integer,   parameter      ::      nvegclas=24+3


   INTEGER                   ::      IVGTYP

   LOGICAL,    INTENT(IN   )    ::     myj

   REAL,     INTENT(INOUT)   ::                                 &
                                                         emiss, &
                                                           znt  
   INTEGER,  INTENT(INOUT)   ::      ILAND
 
!----  Below are the arrays for the vegetation parameters 
   REAL,    DIMENSION( 1:nvegclas )   ::                  LALB, &
                                                          LEMI, &
                                                       LROU_MYJ,&
                                                          LROU

!************************************************************************
!-- USGS model
!
        DATA  LALB/.18,.17,.18,.18,.18,.16,.19,.22,.20,.20,.16,.14,     &
                   .12,.12,.13,.08,.14,.14,.25,.15,.15,.15,.25,.55,     &
                   .30,.16,.60/
        DATA LEMI/.88,4*.92,.93,.92,.88,.9,.92,.93,.94,                 &
                  .95,.95,.94,.98,.95,.95,.85,.92,.93,.92,.85,.95,      &
                  .85,.85,.90 /
!-- Roughness length is changed for forests and some others
! next 2 lines - table from RUC
!        DATA LROU/.5,.06,.075,.065,.05,.2,.075,.1,.11,.15,.8,.85,       &
!                  2.0,1.0,.563,.0001,.2,.4,.05,.1,.15,.1,.065,.05/

         DATA LROU/.5,.06,.075,.065,.05,.2,.075,.1,.11,.15,.5,.5,        &
                   .5,.5,.5,.0001,.2,.4,.05,.1,.15,.1,.065,.05,          &
                   .01,.15,.01 /

! With MYJSFC better use the table from MYJSFCINIT
        DATA LROU_MYJ/1.0,.07,.07,.07,.07,.15,.08,.03,.05,.86,.8,.85,    &
                  2.65,1.09,.8,.001,.04,.05,.01,.04,.06,.05,.03,.001,    &
                  .01,.15,.01 /



!--------------------------------------------------------------------------

        EMISS  = LEMITBL(IVGTYP)
!tgs 3 Oct 2007  - LROU_MYJ gives unrealistic surface fluxes with RUC LSM with RUC LSM
!       if(myj) then
!        ZNT    = LROU_MYJ(IVGTYP)
!       else
        ZNT    = Z0TBL(IVGTYP)
!!!        ZNT    = LROU(IVGTYP)
!       endif
        ILAND  =      IVGTYP
! --- 

!        RETURN
!        END
!--------------------------------------------------------------------------
   END SUBROUTINE SNOWFREE

  SUBROUTINE RUCLSMINIT( SH2O,SMFR3D,TSLB,SMOIS,ISLTYP,IVGTYP,     &
                     XICE,mavail,nzs, iswater, isice, restart,     &
                     allowed_to_read ,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )
#ifdef WRF_CHEM
  USE module_data_gocart_dust
#endif
   IMPLICIT NONE


   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte,  &
                                    nzs, iswater, isice

   REAL, DIMENSION( ims:ime, 1:nzs, jms:jme )                    , &
            INTENT(IN)    ::                                 TSLB, &
                                                            SMOIS

   INTEGER, DIMENSION( ims:ime, jms:jme )                        , &
            INTENT(INOUT)    ::                     ISLTYP,IVGTYP

   REAL, DIMENSION( ims:ime, 1:nzs, jms:jme )                    , &
            INTENT(INOUT)    ::                            SMFR3D, &
                                                             SH2O

   REAL, DIMENSION( ims:ime, jms:jme )                           , &
            INTENT(INOUT)    ::                       XICE,MAVAIL

   REAL, DIMENSION ( 1:nzs )  ::                           SOILIQW

   LOGICAL , INTENT(IN) :: restart, allowed_to_read 

!
  INTEGER ::  I,J,L,itf,jtf
  REAL    ::  RIW,XLMELT,TLN,DQM,REF,PSIS,QMIN,BCLH

   INTEGER                   :: errflag

!   itf=min0(ite,ide-1)
!   jtf=min0(jte,jde-1)


        RIW=900.*1.e-3
        XLMELT=3.35E+5

! initialize three  LSM related tables
   IF ( allowed_to_read ) THEN
     CALL wrf_message( 'INITIALIZE THREE LSM RELATED TABLES' )
     CALL  RUCLSM_PARM_INIT
   ENDIF

#ifdef WRF_CHEM
!
! need this parameter for dust parameterization in wrf/chem
!
   do I=1,NSLTYPE
      porosity(i)=maxsmc(i)
   enddo
#endif

 IF(.not.restart)THEN

   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   errflag = 0
   DO j = jts,jtf
     DO i = its,itf
       IF ( ISLTYP( i,j ) .LT. 1 ) THEN
         errflag = 1
         WRITE(err_message,*)"module_sf_ruclsm.F: lsminit: out of range ISLTYP ",i,j,ISLTYP( i,j )
         CALL wrf_message(err_message)
       ENDIF
     ENDDO
   ENDDO
   IF ( errflag .EQ. 1 ) THEN
      CALL wrf_error_fatal( "module_sf_ruclsm.F: lsminit: out of range value "// &
                            "of ISLTYP. Is this field in the input?" )
   ENDIF

   DO J=jts,jtf
       DO I=its,itf

!     CALL SOILIN     ( ISLTYP(I,J), DQM, REF, PSIS, QMIN, BCLH )


!--- Computation of volumetric content of ice in soil
!--- and initialize MAVAIL
          DQM    = MAXSMC   (ISLTYP(I,J)) -                               &
                   DRYSMC   (ISLTYP(I,J))
          REF    = REFSMC   (ISLTYP(I,J))
          PSIS   = - SATPSI (ISLTYP(I,J))
          QMIN   = DRYSMC   (ISLTYP(I,J))
          BCLH   = BB       (ISLTYP(I,J))


!!!     IF (.not.restart) THEN

    IF(xice(i,j).gt.0.) THEN
!-- for ice
         DO L=1,NZS
           smfr3d(i,l,j)=1.
           sh2o(i,l,j)=0.
           mavail(i,j) = 1.
         ENDDO
    ELSE
       if(isltyp(i,j).ne.14 ) then
!-- land
           mavail(i,j) = max(0.00001,min(1.,smois(i,1,j)/dqm))
!           mavail(i,j) = max(0.00001,min(1.,smois(i,1,j)/(ref-qmin)))
         DO L=1,NZS
!-- for land points initialize soil ice
         tln=log(TSLB(i,l,j)/273.15)
          
          if(tln.lt.0.) then
           soiliqw(l)=(dqm+qmin)*(XLMELT*                        &
         (tslb(i,l,j)-273.15)/tslb(i,l,j)/9.81/psis)             &
          **(-1./bclh)-qmin
           soiliqw(l)=max(0.,soiliqw(l))
           soiliqw(l)=min(soiliqw(l),smois(i,l,j))
           sh2o(i,l,j)=soiliqw(l)
           smfr3d(i,l,j)=(smois(i,l,j)-soiliqw(l))/RIW
         
          else
           smfr3d(i,l,j)=0.
           sh2o(i,l,j)=smois(i,l,j)
          endif
         ENDDO
    
       else
!-- for water  ISLTYP=14
         DO L=1,NZS
           smfr3d(i,l,j)=0.
           sh2o(i,l,j)=1.
           mavail(i,j) = 1.
         ENDDO
       endif
    ENDIF

    ENDDO
   ENDDO

 ENDIF

  END SUBROUTINE ruclsminit
!
!-----------------------------------------------------------------
        SUBROUTINE RUCLSM_PARM_INIT
!-----------------------------------------------------------------

        character*8 :: MMINLU, MMINSL

        MMINLU='USGS-RUC'
        MMINSL='STAS-RUC'
        call RUCLSM_SOILVEGPARM( MMINLU, MMINSL)

!-----------------------------------------------------------------
        END SUBROUTINE RUCLSM_PARM_INIT
!-----------------------------------------------------------------

!-----------------------------------------------------------------
        SUBROUTINE RUCLSM_SOILVEGPARM( MMINLU, MMINSL)
!-----------------------------------------------------------------

        IMPLICIT NONE

        integer :: LUMATCH, IINDEX, LC, NUM_SLOPE
        integer :: ierr
        INTEGER , PARAMETER :: OPEN_OK = 0

        character*8 :: MMINLU, MMINSL
        character*128 :: mess , message, vege_parm_string
        logical, external :: wrf_dm_on_monitor


!-----SPECIFY VEGETATION RELATED CHARACTERISTICS :
!             ALBBCK: SFC albedo (in percentage)
!                 Z0: Roughness length (m)
!               LEMI: Emissivity
!                 PC: Plant coefficient for transpiration function
! -- the rest of the parameters are read in but not used currently
!             SHDFAC: Green vegetation fraction (in percentage)
!  Note: The ALBEDO, Z0, and SHDFAC values read from the following table
!          ALBEDO, amd Z0 are specified in LAND-USE TABLE; and SHDFAC is
!          the monthly green vegetation data
!             CMXTBL: MAX CNPY Capacity (m)
!             NROTBL: Rooting depth (layer)
!              RSMIN: Mimimum stomatal resistance (s m-1)
!              RSMAX: Max. stomatal resistance (s m-1)
!                RGL: Parameters used in radiation stress function
!                 HS: Parameter used in vapor pressure deficit functio
!               TOPT: Optimum transpiration air temperature. (K)
!             CMCMAX: Maximum canopy water capacity
!             CFACTR: Parameter used in the canopy inteception calculati
!               SNUP: Threshold snow depth (in water equivalent m) that
!                     implies 100% snow cover
!                LAI: Leaf area index (dimensionless)
!             MAXALB: Upper bound on maximum albedo over deep snow
!
!-----READ IN VEGETAION PROPERTIES FROM VEGPARM.TBL 
!                                                                       

       IF ( wrf_dm_on_monitor() ) THEN

        OPEN(19, FILE='VEGPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_ruclsm.F: soil_veg_gen_parm: failure opening VEGPARM.TBL'
          CALL wrf_error_fatal ( message )
        END IF

        WRITE ( mess, * ) 'INPUT VEGPARM FOR ',MMINLU
        CALL wrf_message( mess )

        LUMATCH=0

 2000   FORMAT (A8)
        READ (19,'(A)') vege_parm_string
        outer : DO 
           READ (19,2000,END=2002)LUTYPE
           READ (19,*)LUCATS,IINDEX

           WRITE( mess , * ) 'VEGPARM FOR ',LUTYPE,' FOUND', LUCATS,' CATEGORIES'
           CALL wrf_message( mess )

           IF(LUTYPE.NE.MMINLU)THEN    ! Skip over the undesired table
              write ( mess , * ) 'Skipping ', LUTYPE, ' table'
              CALL wrf_message( mess )
              DO LC=1,LUCATS
                 READ (19,*)
              ENDDO
              inner : DO               ! Find the next "Vegetation Parameters"
                 READ (19,'(A)',END=2002) vege_parm_string
                 IF (TRIM(vege_parm_string) .EQ. "Vegetation Parameters") THEN
                    EXIT inner
                 END IF
               ENDDO inner
           ELSE
              LUMATCH=1
              write ( mess , * ) 'Found ', LUTYPE, ' table'
              CALL wrf_message( mess )
              EXIT outer                ! Found the table, read the data
           END IF

        ENDDO outer

        IF (LUMATCH == 1) then
           write ( mess , * ) 'Reading ',LUTYPE,' table'
           CALL wrf_message( mess )
           DO LC=1,LUCATS
              READ (19,*)IINDEX,ALBTBL(LC),Z0TBL(LC),LEMITBL(LC),PCTBL(LC), &
                         SHDTBL(LC),NROTBL(LC),RSTBL(LC),RGLTBL(LC),         &
                         HSTBL(LC),SNUPTBL(LC),LAITBL(LC),MAXALB(LC)
           ENDDO
!
           READ (19,*)
           READ (19,*)TOPT_DATA
           READ (19,*)
           READ (19,*)CMCMAX_DATA
           READ (19,*)
           READ (19,*)CFACTR_DATA
           READ (19,*)
           READ (19,*)RSMAX_DATA
           READ (19,*)
           READ (19,*)BARE
           READ (19,*)
           READ (19,*)NATURAL
        ENDIF

 2002   CONTINUE
        CLOSE (19)

        IF (LUMATCH == 0) then
           CALL wrf_error_fatal ("Land Use Dataset '"//MMINLU//"' not found in VEGPARM.TBL.")
        ENDIF

      END IF

      CALL wrf_dm_bcast_string  ( LUTYPE  , 8 )
      CALL wrf_dm_bcast_integer ( LUCATS  , 1 )
      CALL wrf_dm_bcast_integer ( IINDEX  , 1 )
      CALL wrf_dm_bcast_integer ( LUMATCH , 1 )
      CALL wrf_dm_bcast_real    ( ALBTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( Z0TBL   , NLUS )
      CALL wrf_dm_bcast_real    ( LEMITBL , NLUS )
      CALL wrf_dm_bcast_real    ( PCTBL   , NLUS )
      CALL wrf_dm_bcast_real    ( SHDTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( NROTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( RSTBL   , NLUS )
      CALL wrf_dm_bcast_real    ( RGLTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( HSTBL   , NLUS )
      CALL wrf_dm_bcast_real    ( SNUPTBL , NLUS )
      CALL wrf_dm_bcast_real    ( LAITBL  , NLUS )
      CALL wrf_dm_bcast_real    ( MAXALB  , NLUS )
      CALL wrf_dm_bcast_real    ( TOPT_DATA    , 1 )
      CALL wrf_dm_bcast_real    ( CMCMAX_DATA  , 1 )
      CALL wrf_dm_bcast_real    ( CFACTR_DATA  , 1 )
      CALL wrf_dm_bcast_real    ( RSMAX_DATA  , 1 )
      CALL wrf_dm_bcast_integer ( BARE    , 1 )

!                                                                       
!-----READ IN SOIL PROPERTIES FROM SOILPARM.TBL
!                                                                       
      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(19, FILE='SOILPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_ruclsm.F: soil_veg_gen_parm: failure opening SOILPARM.TBL'
          CALL wrf_error_fatal ( message )
        END IF

        WRITE(mess,*) 'INPUT SOIL TEXTURE CLASSIFICAION = ',MMINSL
        CALL wrf_message( mess )

        LUMATCH=0

        READ (19,*)
        READ (19,2000,END=2003)SLTYPE
        READ (19,*)SLCATS,IINDEX
        IF(SLTYPE.NE.MMINSL)THEN
          DO LC=1,SLCATS
              READ (19,*) IINDEX,BB(LC),DRYSMC(LC),HC(LC),MAXSMC(LC),&
                        REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
                        WLTSMC(LC), QTZ(LC)
          ENDDO
        ENDIF
        READ (19,*)
        READ (19,2000,END=2003)SLTYPE
        READ (19,*)SLCATS,IINDEX
 
        IF(SLTYPE.EQ.MMINSL)THEN
            WRITE( mess , * ) 'SOIL TEXTURE CLASSIFICATION = ',SLTYPE,' FOUND', &
                  SLCATS,' CATEGORIES'
            CALL wrf_message ( mess )
          LUMATCH=1
        ENDIF
            IF(SLTYPE.EQ.MMINSL)THEN
          DO LC=1,SLCATS
              READ (19,*) IINDEX,BB(LC),DRYSMC(LC),HC(LC),MAXSMC(LC),&
                        REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
                        WLTSMC(LC), QTZ(LC)
          ENDDO
           ENDIF

 2003   CONTINUE

        CLOSE (19)
      ENDIF

      CALL wrf_dm_bcast_integer ( LUMATCH , 1 )
      CALL wrf_dm_bcast_string  ( SLTYPE  , 8 )
      CALL wrf_dm_bcast_string  ( MMINSL  , 8 )  ! since this is reset above, see oct2 ^
      CALL wrf_dm_bcast_integer ( SLCATS  , 1 )
      CALL wrf_dm_bcast_integer ( IINDEX  , 1 )
      CALL wrf_dm_bcast_real    ( BB      , NSLTYPE )
      CALL wrf_dm_bcast_real    ( DRYSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( HC      , NSLTYPE )
      CALL wrf_dm_bcast_real    ( MAXSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( REFSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATPSI  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATDK   , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATDW   , NSLTYPE )
      CALL wrf_dm_bcast_real    ( WLTSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( QTZ     , NSLTYPE )

      IF(LUMATCH.EQ.0)THEN
          CALL wrf_message( 'SOIl TEXTURE IN INPUT FILE DOES NOT ' )
          CALL wrf_message( 'MATCH SOILPARM TABLE'                 )
          CALL wrf_error_fatal ( 'INCONSISTENT OR MISSING SOILPARM FILE' )
      ENDIF

!
!-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL 
!                                                                       
      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(19, FILE='GENPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_ruclsm.F: soil_veg_gen_parm: failure opening GENPARM.TBL'
          CALL wrf_error_fatal ( message )
        END IF

        READ (19,*)
        READ (19,*)
        READ (19,*) NUM_SLOPE

          SLPCATS=NUM_SLOPE

          DO LC=1,SLPCATS
              READ (19,*)SLOPE_DATA(LC)
          ENDDO

          READ (19,*)
          READ (19,*)SBETA_DATA
          READ (19,*)
          READ (19,*)FXEXP_DATA
          READ (19,*)
          READ (19,*)CSOIL_DATA
          READ (19,*)
          READ (19,*)SALP_DATA
          READ (19,*)
          READ (19,*)REFDK_DATA
          READ (19,*)
          READ (19,*)REFKDT_DATA
          READ (19,*)
          READ (19,*)FRZK_DATA
          READ (19,*)
          READ (19,*)ZBOT_DATA
          READ (19,*)
          READ (19,*)CZIL_DATA
          READ (19,*)
          READ (19,*)SMLOW_DATA
          READ (19,*)
          READ (19,*)SMHIGH_DATA
        CLOSE (19)
      ENDIF

      CALL wrf_dm_bcast_integer ( NUM_SLOPE    ,  1 )
      CALL wrf_dm_bcast_integer ( SLPCATS      ,  1 )
      CALL wrf_dm_bcast_real    ( SLOPE_DATA   ,  NSLOPE )
      CALL wrf_dm_bcast_real    ( SBETA_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( FXEXP_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( CSOIL_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( SALP_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( REFDK_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( REFKDT_DATA  ,  1 )
      CALL wrf_dm_bcast_real    ( FRZK_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( ZBOT_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( CZIL_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( SMLOW_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( SMHIGH_DATA  ,  1 )


!-----------------------------------------------------------------
      END SUBROUTINE RUCLSM_SOILVEGPARM
!-----------------------------------------------------------------


  SUBROUTINE SOILIN (ISLTYP, DQM, REF, PSIS, QMIN, BCLH )

!---    soiltyp classification according to STATSGO(nclasses=16)
!
!             1          SAND                  SAND
!             2          LOAMY SAND            LOAMY SAND
!             3          SANDY LOAM            SANDY LOAM
!             4          SILT LOAM             SILTY LOAM
!             5          SILT                  SILTY LOAM
!             6          LOAM                  LOAM
!             7          SANDY CLAY LOAM       SANDY CLAY LOAM
!             8          SILTY CLAY LOAM       SILTY CLAY LOAM
!             9          CLAY LOAM             CLAY LOAM
!            10          SANDY CLAY            SANDY CLAY
!            11          SILTY CLAY            SILTY CLAY
!            12          CLAY                  LIGHT CLAY
!            13          ORGANIC MATERIALS     LOAM
!            14          WATER
!            15          BEDROCK
!                        Bedrock is reclassified as class 14
!            16          OTHER (land-ice)
! extra classes from Fei Chen
!            17          Playa
!            18          Lava
!            19          White Sand
!
!----------------------------------------------------------------------
         integer,   parameter      ::      nsoilclas=19

         integer, intent ( in)  ::                          isltyp
         real,    intent ( out) ::               dqm,ref,qmin,psis

         REAL  LQMA(nsoilclas),LREF(nsoilclas),LBCL(nsoilclas),       &
               LPSI(nsoilclas),LQMI(nsoilclas)

!-- LQMA Rawls et al.[1982]
!        DATA LQMA /0.417, 0.437, 0.453, 0.501, 0.486, 0.463, 0.398,
!     &  0.471, 0.464, 0.430, 0.479, 0.475, 0.439, 1.0, 0.20, 0.401/
!---
!-- Clapp, R. and G. Hornberger, Empirical equations for some soil
!   hydraulic properties, Water Resour. Res., 14,601-604,1978.
!-- Clapp et al. [1978]
     DATA LQMA /0.395, 0.410, 0.435, 0.485, 0.485, 0.451, 0.420,      &
                0.477, 0.476, 0.426, 0.492, 0.482, 0.451, 1.0,        &
                0.20,  0.435, 0.468, 0.200, 0.339/

!-- Clapp et al. [1978]
        DATA LREF /0.174, 0.179, 0.249, 0.369, 0.369, 0.314, 0.299,   &
                   0.357, 0.391, 0.316, 0.409, 0.400, 0.314, 1.,      &
                   0.1,   0.249, 0.454, 0.17,  0.236/

!-- Carsel and Parrish [1988]
        DATA LQMI/0.045, 0.057, 0.065, 0.067, 0.034, 0.078, 0.10,     &
                  0.089, 0.095, 0.10,  0.070, 0.068, 0.078, 0.0,      &
                  0.004, 0.065, 0.020, 0.004, 0.008/

!-- Clapp et al. [1978]
       DATA LPSI/0.121, 0.090, 0.218, 0.786, 0.786, 0.478, 0.299,     &
                 0.356, 0.630, 0.153, 0.490, 0.405, 0.478, 0.0,       &
                 0.121, 0.218, 0.468, 0.069, 0.069/

!-- Clapp et al. [1978]
        DATA LBCL/4.05,  4.38,  4.90,  5.30,  5.30,  5.39,  7.12,      &
                  7.75,  8.52, 10.40, 10.40, 11.40,  5.39,  0.0,       &
                  4.05,  4.90, 11.55,  2.79,  2.79/


          DQM    = LQMA(ISLTYP)-                               &
                   LQMI(ISLTYP)
          REF    = LREF(ISLTYP)
          PSIS   = - LPSI(ISLTYP)
          QMIN   = LQMI(ISLTYP)
          BCLH   = LBCL(ISLTYP)

  END SUBROUTINE SOILIN

END MODULE module_sf_ruclsm
