!-----------------------------------------------------------------------
!
!NCEP_MESO:MODEL_LAYER: PHYSICS
!
!-----------------------------------------------------------------------
#include "nmm_loop_basemacros.h"
#include "nmm_loop_macros.h"
!-----------------------------------------------------------------------
!
      MODULE MODULE_PHYSICS_CALLS
!
!-----------------------------------------------------------------------
      USE MODULE_DOMAIN
      USE MODULE_DM
      USE MODULE_CONFIGURE
      USE MODULE_TILES
      USE MODULE_STATE_DESCRIPTION,ONLY : P_QV,P_QC,P_QR,P_QI,P_QS,P_QG,P_QNI
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_RA_GFDLETA,ONLY : CAL_MON_DAY,ZENITH
      USE MODULE_RADIATION_DRIVER
      USE MODULE_SF_MYJSFC
      USE MODULE_SURFACE_DRIVER
      USE MODULE_PBL_DRIVER
      USE MODULE_CU_BMJ
      USE MODULE_CUMULUS_DRIVER
      USE MODULE_MP_ETANEW
      USE MODULE_MICROPHYSICS_DRIVER
      USE MODULE_MICROPHYSICS_ZERO_OUT
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE RADIATION(NTSD,DT,JULDAY,JULYR,XTIME,JULIAN            &
     &                    ,IHRST,NPHS,GLAT,GLON                         &
     &                    ,NRADS,NRADL                                  &
     &                    ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT   &
     &                    ,PD,RES,PINT,T,Q,MOIST,THS,ALBEDO,EPSR        &
     &                    ,F_ICE,F_RAIN                                 &
#ifdef WRF_CHEM
     &                    ,GD_CLOUD,GD_CLOUD2                           &
#endif
     &                    ,SM,HBM2,LMH,CLDFRA,N_MOIST,RESTRT            &
     &                    ,RLWTT,RSWTT,RLWIN,RSWIN,RSWINC,RSWOUT        &
     &                    ,RLWTOA,RSWTOA,CZMEAN                         &
     &                    ,CFRACL,CFRACM,CFRACH,SIGT4                   &
     &                    ,ACFRST,NCFRST,ACFRCV,NCFRCV                  &
     &                    ,CUPPT,VEGFRC,SNOW,HTOP,HBOT                  &
     &                    ,Z,SICE,NUM_AEROSOLC,NUM_OZMIXM               &
     &                    ,GRID,CONFIG_FLAGS                            &
     &                    ,RTHRATEN                                     &
#ifdef WRF_CHEM
     &                    ,PM2_5_DRY, PM2_5_WATER, PM2_5_DRY_EC         &
     &                    ,TAUAER1, TAUAER2, TAUAER3, TAUAER4           &
     &                    ,GAER1, GAER2, GAER3, GAER4                   &
     &                    ,WAER1, WAER2, WAER3, WAER4                   &
#endif
     &                    ,IDS,IDE,JDS,JDE,KDS,KDE                      &
     &                    ,IMS,IME,JMS,JME,KMS,KME                      &
     &                    ,ITS,ITE,JTS,JTE,KTS,KTE)
!***  NOTE ***
! RLWIN  - downward longwave at the surface (=TOTLWDN, now a local array)
! RSWIN  - downward shortwave at the surface (=TOTSWDN, now a local array)
! RSWINC - CLEAR-SKY downward shortwave at the surface (=TOTSWDNC, new for AQ)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    RADIATION   RADIATION OUTER DRIVER
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 2002-06-04       
!     
! ABSTRACT:
!     RADIATION SERVES AS THE INTERFACE BETWEEN THE NCEP NONHYDROSTATIC
!     MESOSCALE MODEL AND THE WRF RADIATION DRIVER.
!     
! PROGRAM HISTORY LOG:
!   02-06-04  BLACK      - ORIGINATOR
!   02-09-09  WOLFE      - CONVERTING TO GLOBAL INDEXING
!   04-11-18  BLACK      - THREADED
!     
! USAGE: CALL RADIATION FROM SOLVE_NMM      
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM 
!$$$  
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,IHRST,JULDAY,JULYR                          &
     &                     ,N_MOIST,NPHS,NRADL,NRADS,NTSD               &
     &                     ,NUM_AEROSOLC,NUM_OZMIXM
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: NCFRCV,NCFRST
!
      REAL,INTENT(IN) :: DT,PDTOP,PT,XTIME,JULIAN
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LMH
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: ALBEDO              &
     &                                             ,EPSR,GLAT,GLON      &
     &                                             ,HBM2                &
     &                                             ,PD,RES,SM           &
     &                                             ,SNOW,THS,VEGFRC,SICE
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: CUPPT            

!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: F_ICE       &
     &                                                     ,F_RAIN      &
     &                                                     ,Q,T,Z
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: RTHRATEN
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,N_MOIST),INTENT(INOUT) :: MOIST
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACFRCV,ACFRST    &
     &                                                ,RLWIN,RLWTOA     &
     &                                                ,RSWIN,RSWOUT     &
     &                                                ,HBOT,HTOP        &
     &                                                ,RSWINC,RSWTOA
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: PINT     &
     &                                                        ,RLWTT    &
     &                                                        ,RSWTT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: CFRACH,CFRACL      &
     &                                              ,CFRACM,CZMEAN      &
     &                                              ,SIGT4
#ifdef WRF_CHEM
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME ),INTENT(IN) ::            &
     &                              GAER1,GAER2,GAER3,GAER4,            &
     &                              GD_CLOUD,GD_CLOUD2,                 &
     &                              PM2_5_DRY,PM2_5_WATER,PM2_5_DRY_EC, &
     &                              TAUAER1,TAUAER2,TAUAER3,TAUAER4,    &
     &                              WAER1,WAER2,WAER3,WAER4
#endif
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: CLDFRA
!
      LOGICAL,INTENT(IN) :: RESTRT
!
      TYPE(DOMAIN),TARGET :: GRID
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
!-----------------------------------------------------------------------
      INTEGER :: I,ICLOUD,IENDX,II,J,JDAY,JMONTH,K,KMNTH,LMHIJ,NRAD
!
      INTEGER,DIMENSION(3) :: IDAT
      INTEGER,DIMENSION(12) :: MONTH=(/31,28,31,30,31,30,31,31          &
     &                                ,30,31,30,31/)
!
      REAL :: CAPA,DAYI,DPL,FICE,FRAIN,GMT,HOUR,PDSL,PLYR,PSFC          &
     &       ,QI,QR,QW,RADT,TIMES,WC,TDUM
!
      REAL,DIMENSION(KMS:KME-1) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: REXNSFC,SWNETDN                &
     &                                  ,TOT,TSFC,XLAND,XLAT,XLON       &
     &                                  ,TOTLWDN,TOTSWDN,TOTSWDNC,CZEN  &
     &                                  ,HBOTR,HTOPR,CUPPTR
!
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: DZ,P8W,P_PHY,PI_PHY    &
     &                                          ,RR,T8W                 &
     &                                          ,THRATENLW,THRATENSW    &
     &                                          ,TH_PHY,T_PHY,CLFR
!
!
!***  Different way to include cloud effects in radiation.
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: QC1R,QI1R
!
      LOGICAL :: WARM_RAIN
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!*****
!***** NOTE: THIS IS HARDWIRED FOR CALLS TO LONGWAVE AND SHORTWAVE
!*****       AT EQUAL INTERVALS
!*****
      NRAD=NRADS
      RADT=DT*NRADS/60.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      CAPA=R_D/CP
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(dpl,fice,frain,i,j,k,pdsl,plyr,ql,tl)
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
!
        PDSL=PD(I,J)*RES(I,J)
        P8W(I,KTE+1,J)=PT
        XLAT(I,J)=GLAT(I,J)/DEGRAD
        XLON(I,J)=GLON(I,J)/DEGRAD
        XLAND(I,J)=SM(I,J)+1.
        PSFC=PD(I,J)+PDTOP+PT
        REXNSFC(I,J)=(PSFC*1.E-5)**CAPA
        TSFC(I,J)=THS(I,J)*REXNSFC(I,J)
        T8W(I,1,J)=TSFC(I,J)
        P8W(I,KTS,J)=ETA1(KTS)*PDTOP+ETA2(KTS)*PDSL+PT
!
!-----------------------------------------------------------------------
!***  FILL THE SINGLE-COLUMN INPUT
!-----------------------------------------------------------------------
!
        DO K=KTS,KTE
          DPL=DETA1(K)*PDTOP+DETA2(K)*PDSL
          QL(K)=AMAX1(Q(I,K,J),EPSQ)
          PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL+PT
          TL(K)=T(I,K,J)
!
          RR(I,K,J)=PLYR/(R_D*TL(K)*(1.+P608*QL(K)))
          T_PHY(I,K,J)=TL(K)
          TH_PHY(I,K,J)=TL(K)*(1.E5/PLYR)**CAPA
          P8W(I,K+1,J)=ETA1(K+1)*PDTOP+ETA2(K+1)*PDSL+PT
          P_PHY(I,K,J)=PLYR
          PI_PHY(I,K,J)=(PLYR*1.E-5)**CAPA
          DZ(I,K,J)=TL(K)*(P608*QL(K)+1.)*R_D                           &
     &                 *(P8W(I,K,J)-P8W(I,K+1,J))                       &
     &                 /(P_PHY(I,K,J)*G)
!!!  &                 *ALOG(P8W(I,KFLIP,J)/P8W(I,KFLIP+1,J))/G         &
!!!  &                 *ALOG(PINT(I,K+1,J)/PINT(I,K,J))/G               &
!
          RTHRATEN(I,K,J)=0.
          THRATENLW(I,K,J)=0.
          THRATENSW(I,K,J)=0.
!         PM2_5_DRY(I,K,J)=0.
!         PM2_5_WATER(I,K,J)=0.

        ENDDO
!
        DO K=KTS+1,KTE
          T8W(I,K,J)=0.5*(TL(K-1)+TL(K))
        ENDDO
        T8W(I,KTE+1,J)=-1.E20
!
      ENDDO
      ENDDO
!
      ICLOUD=999
!
      GMT=REAL(IHRST)
!
!-----------------------------------------------------------------------
!
!***  CALL THE INNER DRIVER.
!
!-----------------------------------------------------------------------
!
      DO J=JMS,JME
      DO K=KMS,KME
      DO I=IMS,IME
        QC1R(I,K,J)=0.
        QI1R(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=MYJS2,MYJE2
        DO K=KTS,KTE
          DO I=MYIS1,MYIE1
            QC1R(I,K,J)=MOIST(I,K,J,P_QC)
            QI1R(I,K,J)=MOIST(I,K,J,P_QI)
          ENDDO
        ENDDO
      ENDDO
      DO J=JMS,JME
        DO K=KMS,KME
        DO I=IMS,IME
          CLDFRA(I,K,J)=0.
        ENDDO
        ENDDO
!
        DO I=IMS,IME
          CFRACH(I,J)=0.
          CFRACL(I,J)=0.
          CFRACM(I,J)=0.
          CZMEAN(I,J)=0.
          SIGT4(I,J)=0.
          TOTSWDN(I,J)=0.   ! TOTAL (clear+cloudy sky) shortwave down at the surface
          TOTSWDNC(I,J)=0.  ! CLEAR SKY shortwave down at the surface
          SWNETDN(I,J)=0.   ! Net (down - up) total (clear+cloudy sky) shortwave at the surface
          TOTLWDN(I,J)=0.   ! Total longwave down at the surface
          CUPPTR(I,J)=CUPPT(I,J)   ! Temporary array set to zero in radiation
!-- NOTE:  HBOTR, HTOPR are passed into radiation and set equal to HBOT, HTOP.  HBOT, HTOP are
!          reset to clear sky values to be used by the ARW.  At the bottom of this subroutine, 
!          HBOT, HTOP are re-defined again to values stored in HBOTR, HTOPR.  HBOT, HTOP are 
!          reset to clear sky values after the call to radiation and after the top of the hour
!          in subroutine CUCNVC below.
        ENDDO
      ENDDO
!
      CALL SET_TILES(GRID,IDS+1,IDE-1,JDS+2,JDE-2,ITS,ITE,JTS,JTE)
!
      CALL RADIATION_DRIVER(                                            &
     &                  IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE &
     &                 ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME &
     &                 ,I_START=GRID%I_START,I_END=GRID%I_END           &
     &                 ,J_START=GRID%J_START,J_END=GRID%J_END           &
     &                 ,KTS=KTS,KTE=KTE,NUM_TILES=GRID%NUM_TILES        &
     &                 ,ITIMESTEP=NTSD,DT=DT                            &
     &                 ,cu_rad_feedback=config_flags%cu_rad_feedback    &
#ifdef WRF_CHEM
     &        ,PM2_5_DRY=pm2_5_dry, PM2_5_WATER=pm2_5_water               &
     &        ,PM2_5_DRY_EC=pm2_5_dry_ec                                  &
     &        ,TAUAER300=tauaer1, TAUAER400=tauaer2, TAUAER600=tauaer3, TAUAER999=tauaer4 & ! jcb
     &        ,GAER300=gaer1, GAER400=gaer2, GAER600=gaer3, GAER999=gaer4 & ! jcb
     &        ,WAER300=waer1, WAER400=waer2, WAER600=waer3, WAER999=waer4 & ! jcb
     &        ,qc_adjust=GD_CLOUD,qi_adjust=GD_CLOUD2                   &
#endif
     &                 ,RTHRATENLW=THRATENLW,RTHRATENSW=THRATENSW       &
     &                 ,RTHRATEN=RTHRATEN                               &
     &                 ,GLW=TOTLWDN,GSW=SWNETDN,SWDOWN=TOTSWDN          &
     &                 ,XLAT=XLAT,XLONG=XLON,ALBEDO=ALBEDO,EMISS=EPSR   &
     &                 ,XICE=SICE,XLAND=XLAND,Z=Z,TSK=TSFC              &
     &                 ,N_AEROSOLC=NUM_AEROSOLC,PAERLEV=GRID%PAERLEV    &
     &                 ,CAM_ABS_DIM1=GRID%CAM_ABS_DIM1                  &
     &                 ,CAM_ABS_DIM2=GRID%CAM_ABS_DIM2                  &
     &                 ,CAM_ABS_FREQ_S=GRID%CAM_ABS_FREQ_S              &
     &                 ,LEVSIZ=GRID%LEVSIZ,N_OZMIXM=NUM_OZMIXM          &
     &                 ,HTOP=HTOP,HBOT=HBOT,CUPPT=CUPPTR                &
     &                 ,HTOPR=HTOPR,HBOTR=HBOTR                         &
     &                 ,VEGFRA=VEGFRC,SNOW=SNOW                         &
     &                 ,RHO=RR,P8W=P8W,P=P_PHY,PI=PI_PHY                &
     &                 ,DZ8W=DZ,T=T_PHY,T8W=T8W,GMT=GMT                 &
     &                 ,JULDAY=JULDAY,JULYR=JULYR,NPHS=NPHS             &
     &                 ,JULIAN=JULIAN,XTIME=XTIME                       &
     &                 ,LW_PHYSICS=CONFIG_FLAGS%RA_LW_PHYSICS           &
     &                 ,SW_PHYSICS=CONFIG_FLAGS%RA_SW_PHYSICS           &
     &                 ,RADT=RADT,RA_CALL_OFFSET=GRID%RA_CALL_OFFSET    &
     &                 ,STEPRA=NRAD,ICLOUD=ICLOUD                       &
     &                 ,WARM_RAIN=WARM_RAIN                             & 
     &                 ,SWDOWNC=TOTSWDNC,CLDFRA=CLFR                    &
     &                 ,RSWTOA=RSWTOA,RLWTOA=RLWTOA                     &
     &                 ,CZMEAN=CZMEAN,CFRACL=CFRACL                     &
     &                 ,CFRACM=CFRACM,CFRACH=CFRACH                     &
     &                 ,ACFRST=ACFRST,NCFRST=NCFRST                     &
     &                 ,ACFRCV=ACFRCV,NCFRCV=NCFRCV                     &
     &                 ,F_ICE_PHY=F_ICE,F_RAIN_PHY=F_RAIN               &
     &                 ,QV=MOIST(IMS,KMS,JMS,P_QV),F_QV=F_QV            &
     &                 ,QC=MOIST(IMS,KMS,JMS,P_QC),F_QC=F_QC                               &
     &                 ,QR=MOIST(IMS,KMS,JMS,P_QR),F_QR=F_QR            &
     &                 ,QI=MOIST(IMS,KMS,JMS,P_QI),F_QI=F_QI                               &
     &                 ,QS=MOIST(IMS,KMS,JMS,P_QS),F_QS=F_QS            &
     &                 ,QG=MOIST(IMS,KMS,JMS,P_QG),F_QG=F_QG          )

!
!-----------------------------------------------------------------------
!
!***  UPDATE FLUXES AND TEMPERATURE TENDENCIES.
!
!-----------------------------------------------------------------------
!***  SHORTWAVE
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
      IF(MOD(NTSD,NRADS)==0)THEN
!-----------------------------------------------------------------------
!
        IF(CONFIG_FLAGS%RA_SW_PHYSICS/=GFDLSWSCHEME)THEN
!
!-----------------------------------------------------------------------
!***  COMPUTE CZMEAN FOR NON-GFDL SHORTWAVE
!-----------------------------------------------------------------------
!
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            CZMEAN(I,J)=0.
            TOT(I,J)=0.
          ENDDO
          ENDDO
!
          CALL CAL_MON_DAY(JULDAY,JULYR,JMONTH,JDAY)
          IDAT(1)=JMONTH
          IDAT(2)=JDAY
          IDAT(3)=JULYR
!
          DO II=0,NRADS,NPHS
            TIMES=NTSD*DT+II*DT
            CALL ZENITH(TIMES,DAYI,HOUR,IDAT,IHRST,GLON,GLAT,CZEN       &
     &                 ,MYIS &
     &                 ,MYIE &
     &                 ,MYJS &
     &                 ,MYJE &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)
            DO J=MYJS,MYJE
            DO I=MYIS,MYIE
              IF(CZEN(I,J)>0.)THEN
                CZMEAN(I,J)=CZMEAN(I,J)+CZEN(I,J)
                TOT(I,J)=TOT(I,J)+1.
              ENDIF
            ENDDO
            ENDDO
!
          ENDDO
!
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            IF(TOT(I,J)>0.)CZMEAN(I,J)=CZMEAN(I,J)/TOT(I,J)
          ENDDO
          ENDDO
!
!-----------------------------------------------------------------------
!***  COMPUTE TOTAL SFC SHORTWAVE DOWN FOR NON-GFDL SCHEMES
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j)
          DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
!
            IF(HBM2(I,J)>0.5)THEN
              TOTSWDN(I,J)=SWNETDN(I,J)/(1.-ALBEDO(I,J))  
!--- No value currently available for clear-sky solar fluxes from
!    non GFDL schemes, though it's needed for air quality forecasts.
!    For the time being, set to the total downward solar fluxes.
              TOTSWDNC(I,J)=TOTSWDN(I,J)
            ENDIF
!
          ENDDO
          ENDDO
!
        ENDIF   !End non-GFDL block
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,k)
        DO J=MYJS2,MYJE2
          IENDX=MYIE1
          IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
          DO I=MYIS1,IENDX
!
            RSWIN(I,J)=TOTSWDN(I,J)
            RSWINC(I,J)=TOTSWDNC(I,J)
            RSWOUT(I,J)=TOTSWDN(I,J)-SWNETDN(I,J)
!
            DO K=KTS,KTE
              RSWTT(I,K,J)=THRATENSW(I,K,J)*PI_PHY(I,K,J)
            ENDDO
!
          ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  LONGWAVE
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,NRADL)==0)THEN
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,k,lmhij)
        DO J=MYJS2,MYJE2
          IENDX=MYIE1
          IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
          DO I=MYIS1,IENDX
!
            IF(HBM2(I,J)>0.5)THEN
              LMHIJ=KTE+1-LMH(I,J)
              TDUM=T(I,LMHIJ,J)
              SIGT4(I,J)=STBOLT*TDUM*TDUM*TDUM*TDUM
!
              DO K=KTS,KTE
                RLWTT(I,K,J)=THRATENLW(I,K,J)*PI_PHY(I,K,J)
              ENDDO
!
              RLWIN(I,J)=TOTLWDN(I,J)
            ENDIF
!
          ENDDO
        ENDDO
!
      ENDIF
!
!-- Store 3D cloud fractions & restore HBOT/HTOP arrays
!
      DO J=MYJS2,MYJE2
        IENDX=MYIE1
        IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
        DO K=KTS,KTE
          DO I=MYIS1,IENDX
            CLDFRA(I,K,J)=CLFR(I,K,J)
          ENDDO
        ENDDO
        DO I=MYIS1,IENDX
          HBOT(I,J)=HBOTR(I,J)
          HTOP(I,J)=HTOPR(I,J)
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
!***  ZERO OUT BOUNDARY ROWS.
!-----------------------------------------------------------------------
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        IF(HBM2(I,J)<0.5)THEN
          ACFRST(I,J)=0.
          ACFRCV(I,J)=0.
          CFRACL(I,J)=0.
          CFRACM(I,J)=0.
          CFRACH(I,J)=0.
          RSWTOA(I,J)=0.
          RLWTOA(I,J)=0.
        ENDIF
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!

      END SUBROUTINE RADIATION
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE TURBL(NTSD,DT,NPHS,RESTRT                              &
     &                ,N_MOIST,NSOIL,SLDPTH,DZSOIL                      &
     &                ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT       &
     &                ,SM,LMH,HTM,VTM,HBM2,VBM2,DX_ARRAY,DFRLG          &
     &                ,CZEN,CZMEAN,SIGT4,RLWIN,RSWIN,RADOT              &
!- RLWIN/RSWIN - downward longwave/shortwave at the surface (also TOTLWDN/TOTSWDN in RADIATION)
     &                ,PD,RES,PINT,T,Q,CWM,F_ICE,F_RAIN,SR              &
     &                ,Q2,U,V,THS,TSFC,SST,PREC,SNO,ZERO_3D             &
     &                ,FIS,Z0,Z0BASE,USTAR,PBLH,LPBL,EL_MYJ             &
     &                ,MOIST,RMOL                                       &
     &                ,EXCH_H,AKHS,AKMS,AKHS_OUT,AKMS_OUT               &
     &                ,THZ0,QZ0,UZ0,VZ0,QS,MAVAIL                       &
     &                ,STC,SMC,CMC,SMSTAV,SMSTOT,SSROFF,BGROFF          &
     &                ,IVGTYP,ISLTYP,VEGFRC,SHDMIN,SHDMAX,GRNFLX        &
     &                ,SFCEXC,ACSNOW,ACSNOM,SNOPCX,SICE,TG,SOILTB       &
     &                ,ALBASE,MXSNAL,ALBEDO,SH2O,SI,EPSR                &
     &                ,U10,V10,TH10,Q10,TSHLTR,QSHLTR,PSHLTR            &
     &                ,T2,QSG,QVG,QCG,SOILT1,TSNAV,SMFR3D,KEEPFR3DFLAG  &
     &                ,TWBS,QWBS,SFCSHX,SFCLHX,SFCEVP                   &
     &                ,POTEVP,POTFLX,SUBSHX                             &
     &                ,APHTIM,ARDSW,ARDLW,ASRFC                         &
     &                ,RSWOUT,RSWTOA,RLWTOA                             &
     &                ,ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA          &
     &                ,UZ0H,VZ0H,DUDT,DVDT                              & 
     &                ,RTHBLTEN,RQVBLTEN                                & 
     &                ,PCPFLG,DDATA                                     & ! PRECIP ASSIM
     &                ,GRID,CONFIG_FLAGS                                &
     &                ,IHE,IHW,IVE,IVW                                  &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    TURBL       TURBULENCE OUTER DRIVER
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 02-04-19       
!     
! ABSTRACT:
!     TURBL DRIVES THE TURBULENCE SCHEMES
!     
! PROGRAM HISTORY LOG (with changes to called routines) :
!   95-03-15  JANJIC     - ORIGINATOR OF THE SUBROUTINES CALLED
!   BLACK & JANJIC       - ORIGINATORS OF THE DRIVER
!   95-03-28  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   96-03-29  BLACK      - ADDED EXTERNAL EDGE; REMOVED SCRCH COMMON
!   96-07-19  MESINGER   - ADDED Z0 EFFECTIVE
!   98-??-??  TUCCILLO   - MODIFIED FOR CLASS VIII PARALLELISM
!   98-10-27  BLACK      - PARALLEL CHANGES INTO MOST RECENT CODE
!   02-01-10  JANJIC     - MOIST TURBULENCE (DRIVER, MIXLEN, VDIFH)
!   02-01-10  JANJIC     - VERT. DIF OF Q2 INCREASED (Grenier & Bretherton)
!   02-02-02  JANJIC     - NEW SFCDIF
!   02-04-19  BLACK      - ORIGINATOR OF THIS OUTER DRIVER FOR WRF
!   02-05-03  JANJIC     - REMOVAL OF SUPERSATURATION AT 2m AND 10m
!   04-11-18  BLACK      - THREADED
!     
! USAGE: CALL TURBL FROM SOLVE_NMM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM
!$$$  
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,N_MOIST,NPHS,NSOIL,NTSD
!
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: ISLTYP,IVGTYP    &
     &                                                ,LMH
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: LPBL
!
      REAL,INTENT(IN) :: DT,PDTOP,PT
!
      REAL,INTENT(INOUT) :: APHTIM,ARDSW,ARDLW,ASRFC
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: DFRLG,ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ALBASE,MXSNAL
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: CZEN,CZMEAN         &
     &                                             ,DX_ARRAY            &
     &                                             ,FIS,HBM2            &
     &                                             ,PD,RES              &
     &                                             ,RLWIN,RLWTOA        &
     &                                             ,RSWIN,RSWOUT,RSWTOA &
     &                                             ,SHDMIN,SHDMAX       &
!    &                                             ,SICE,SIGT4,SM,SR    & !Bandaid
     &                                             ,SICE,SIGT4          &
     &                                             ,SST,TG,VBM2,VEGFRC
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: SM,EPSR,SR         !Bandaid
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: GRNFLX,QWBS,RADOT  &
                                                    ,SFCEXC,SMSTAV      &
                                                    ,SOILTB,TWBS
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACSNOM,ACSNOW    &
     &                                                ,AKHS,AKMS        &
     &                                                ,ALBEDO           &
     &                                                ,MAVAIL           &
     &                                                ,BGROFF,CMC       &
     &                                                ,PBLH,POTEVP      &
     &                                                ,POTFLX,PREC      &
     &                                                ,QCG,QS,QSG       &
     &                                                ,QVG,QZ0          &
     &                                                ,SFCEVP           &
     &                                                ,SFCLHX,SFCSHX    &
     &                                                ,SI,SMSTOT        &
     &                                                ,SNO,SNOPCX       &
     &                                                ,SOILT1           &
     &                                                ,SSROFF,SUBSHX    &
     &                                                ,T2,THS,THZ0      &
     &                                                ,TSFC,TSNAV       &
     &                                                ,USTAR,UZ0,UZ0H   &
     &                                                ,VZ0,VZ0H         &
     &                                                ,Z0,Z0BASE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: AKHS_OUT,AKMS_OUT  &
     &                                              ,ALWIN,ALWOUT       &
     &                                              ,ALWTOA,ASWIN       &
     &                                              ,ASWOUT,ASWTOA      &
     &                                              ,PSHLTR,Q10,QSHLTR  &
     &                                              ,TH10,TSHLTR        &
     &                                              ,U10,V10
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: CWM      &
     &                                                        ,DUDT     &
     &                                                        ,DVDT     &
     &                                                        ,EXCH_H   &
     &                                                        ,F_ICE    &
     &                                                        ,F_RAIN   &
     &                                                        ,Q,Q2     &
     &                                                        ,T,U,V
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: RQVBLTEN,RTHBLTEn
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,N_MOIST),INTENT(INOUT) :: MOIST
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: HTM,VTM
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: PINT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: ZERO_3D
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: EL_MYJ
!
      REAL,DIMENSION(NSOIL),INTENT(IN) :: DZSOIL,SLDPTH
!
      REAL,DIMENSION(IMS:IME,NSOIL,JMS:JME),INTENT(INOUT) :: KEEPFR3DFLAG &
     &                                                      ,SH2O,SMC     &
     &                                                      ,SMFR3D,STC
!
      LOGICAL,INTENT(IN) :: RESTRT
!
      TYPE(DOMAIN),TARGET :: GRID
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!  For precip assimilation:
      LOGICAL,INTENT(IN) :: PCPFLG
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DDATA
!
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
!-----------------------------------------------------------------------
      INTEGER :: I,I_M,IDUMMY,IEND,ISFFLX,ISTR,J,K,KOUNT_ALL,LENGTH_ROW &
     &          ,LLIJ,LLMH,LLYR,N,SST_UPDATE
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME) :: KPBL,LOWLYR
!
      REAL :: TRESH=0.95
!
      REAL :: ALTITUDE,CWML,DQDT,DTDT,DTPHS,DX,DZHALF,FACTR,FACTRL      &
     &       ,G_INV,PDSL,PLYR,PSFC,QI,QL,QOLD,QR,QW,RATIOMX,RDTPHS      &
     &       ,ROG,RWMSK,SDEPTH,SNO_FACTR,TL,TLMH,TLMH4,TNEW,TSFC2       &
     &       ,U_FRAME,V_FRAME,WMSK,XLVRW
!
      REAL :: APES,CKLQ,FACTOR,FFS,PQ0X,Q2SAT,QFC1,QLOWX,RLIVWV         &
     &       ,THBOT
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: BR,CHKLOWQ,CT,CWMLOW,ELFLX     &
     &                                  ,EXNSFC,FACTRS,FLHC,FLQC,GZ1OZ0 &
     &                                  ,ONE,PLM,PSFC_OUT,PSIH,PSIM     &
     &                                  ,Q2X,QLOW,RAIN,RAINBL           &
     &                                  ,RLW_DN_SFC,RMOL,RSW_NET_SFC    &
     &                                  ,RSW_DN_SFC                     &
     &                                  ,SFCEVPX,SFCZ,SNOW,SNOWC,SNOWH  &
     &                                  ,TH2X,THLOW,TLOW,VGFRCK         &
     &                                  ,WSPD,XLAND,ZERO_2D,EMISS
!
      REAL,DIMENSION(IMS:IME,KMS:KME-1,JMS:JME) :: EXNER
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: DZ,P8W                 &
     &                                          ,P_PHY,PI_PHY           &
     &                                          ,RQCBLTEN,RQIBLTEN      &
     &                                          ,RR   &
!    &                                          ,RQVBLTEN,RR,RTHBLTEN   &
     &                                          ,T_PHY,TH_PHY,TKE       &
     &                                          ,U_PHY,V_PHY,Z
!
      REAL,DIMENSION(IMS:IME,NSOIL,JMS:JME) :: ZERO_SOIL
!
      LOGICAL :: E_BDY,WARM_RAIN
!
      INTEGER :: ucmcall
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
     ucmcall=config_flags%ucmcall
!
      DTPHS=NPHS*DT
      RDTPHS=1./DTPHS
      G_INV=1./G
      ROG=R_D*G_INV
      FACTOR=-XLV*RHOWATER/DTPHS
!
      U_FRAME=0.
      V_FRAME=0.
!
      IDUMMY=0
      ISFFLX=1
      DX=0.
      SST_UPDATE=0
!
      DO J=JMS,JME
      DO I=IMS,IME
        UZ0H(I,J)=0.
        VZ0H(I,J)=0.
        ONE(I,J)=1.
        RMOL(I,J)=0.  !Reciprocal of Monin-Obukhov length
        SFCEVPX(I,J)=0.  !Dummy for accumulated latent energy, not flux
      ENDDO
      ENDDO
!
      IF(MODEL_CONFIG_REC%SF_SURFACE_PHYSICS(GRID%ID)==99)THEN
        SNO_FACTR=1.
      ELSE
        SNO_FACTR=0.001
      ENDIF
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        LOWLYR(I,J)=1
        VGFRCK(I,J)=100.*VEGFRC(I,J)
        SNOW(I,J)=SNO(I,J)
        SNOWH(I,J)=SI(I,J)*SNO_FACTR
        XLAND(I,J)=SM(I,J)+1.
        T2(I,J)=TSFC(I,J)
        EMISS(I,J)=EPSR(I,J)
      ENDDO
      ENDDO
!
      IF(NTSD==0)THEN
!$omp parallel do                                                       &
!$omp& private(i,j)
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          Z0BASE(I,J)=Z0(I,J)
          IF(SM(I,J)>0.5.AND.SICE(I,J)>0.5)THEN  !Bandaid
            SM(I,J)=0.        
          ENDIF              
        ENDDO
        ENDDO
      ENDIF
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=MYJS,MYJE
      DO K=KTS,KTE+1
      DO I=MYIS,MYIE
        Z(I,K,J)=0.
        DZ(I,K,J)=0.
        EXCH_H(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!***  PREPARE NEEDED ARRAYS
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(cwml,factrl,i,j,k,llij,llmh,pdsl,plyr,psfc,qi,ql,qr,qw   &
!$omp&        ,tl,tlmh,tlmh4)
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
!
        LLMH=LMH(I,J)
        PDSL=PD(I,J)*RES(I,J)
!!!     PSFC=PD(I,J)+PDTOP+PT
!!!     P8W(I,KTS,J)=PSFC
        P8W(I,KTS,J)=PINT(I,KTS,J)
        PSFC=PINT(I,KTS,J)
        LOWLYR(I,J)=KTE+1-LLMH
        EXNSFC(I,J)=(1.E5/PSFC)**CAPA
        THS(I,J)=(SST(I,J)*EXNSFC(I,J))*SM(I,J)+THS(I,J)*(1.-SM(I,J))
        TSFC(I,J)=THS(I,J)/EXNSFC(I,J)
        SFCZ(I,J)=FIS(I,J)*G_INV
        ZERO_2D(I,J)=0.
!YL     RAIN(I,J)=PREC(I,J)*RHOWATER
        IF (PCPFLG.AND.DDATA(I,J)<100.)THEN
          RAIN(I,J)=DDATA(I,J)*RHOWATER
        ELSE
          RAIN(I,J)=PREC(I,J)*RHOWATER
        ENDIF
!YL
        RAINBL(I,J)=0.
        IF(SNO(I,J)>0.)SNOWC(I,J)=1.
        LLIJ=LOWLYR(I,J)
        PLM(I,J)=(PINT(I,LLIJ,J)+PINT(I,LLIJ+1,J))*0.5
        TH2X(I,J)=T(I,LLIJ,J)*(1.E5/PLM(I,J))**CAPA
        Q2X(I,J)=Q(I,LLIJ,J)
!
!-----------------------------------------------------------------------
!*** LONG AND SHORTWAVE FLUX AT GROUND SURFACE
!-----------------------------------------------------------------------
!
        IF(CZMEAN(I,J)>0.)THEN
          FACTRS(I,J)=CZEN(I,J)/CZMEAN(I,J)
        ELSE
          FACTRS(I,J)=0.
        ENDIF
!
        IF(SIGT4(I,J)>0.)THEN
          TLMH=T(I,LLIJ,J)
          FACTRL=STBOLT*TLMH*TLMH*TLMH*TLMH/SIGT4(I,J)
        ELSE
          FACTRL=0.
        ENDIF
!     
!- RLWIN/RSWIN - downward longwave/shortwave at the surface
!
        RLW_DN_SFC(I,J)=RLWIN(I,J)*HBM2(I,J)*FACTRL
        RSW_NET_SFC(I,J)=(RSWIN(I,J)-RSWOUT(I,J))*HBM2(I,J)*FACTRS(I,J)
!
!- Instant downward solar for nmm_lsm
!
        RSW_DN_SFC(I,J)=RSWIN(I,J)*HBM2(I,J)*FACTRS(I,J)
!
!-----------------------------------------------------------------------
!***  FILL THE ARRAYS FOR CALLING THE INNER DRIVER.
!-----------------------------------------------------------------------
!
        Z(I,KTS,J)=SFCZ(I,J)
!
        DO K=KTS,KTE
          Q2(I,K,J)=AMAX1(Q2(I,K,J)*HBM2(I,J),EPSQ2)
          QL=AMAX1(Q(I,K,J),EPSQ)
          PLYR=(PINT(I,K,J)+PINT(I,K+1,J))*0.5
!!!       PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL+PT
          TL=T(I,K,J)
          CWML=CWM(I,K,J)
!
          RR(I,K,J)=PLYR/(R_D*TL)
          T_PHY(I,K,J)=TL
!
          EXNER(I,K,J)=(1.E5/PLYR)**CAPA
          PI_PHY(I,K,J)=1./EXNER(I,K,J)
          TH_PHY(I,K,J)=TL*EXNER(I,K,J)
          P8W(I,K+1,J)=PINT(I,K+1,J)
!!!       P8W(I,K+1,J)=ETA1(K+1)*PDTOP+ETA2(K+1)*PDSL+PT
          P_PHY(I,K,J)=PLYR
          TKE(I,K,J)=0.5*Q2(I,K,J)
!
          RTHBLTEN(I,K,J)=0.
          RQVBLTEN(I,K,J)=0.
          RQCBLTEN(I,K,J)=0.
          RQIBLTEN(I,K,J)=0.
!
          Z(I,K+1,J)=Z(I,K,J)+TL/PLYR                                   &
     &                  *(DETA1(K)*PDTOP+DETA2(K)*PDSL)*ROG             &
                        *(Q(I,K,J)*P608-CWML+1.)
          Z(I,K+1,J)=(Z(I,K+1,J)-DFRLG(K+1))*HTM(I,K,J)+DFRLG(K+1)
!!!       FACTR=1.-HTM(I,K,J)
!!!       Z(I,K+1,J)=Z(I,K+1,J)*HTM(I,K,J)+FACTR*DFRLG(K+1)
          DZ(I,K,J)=Z(I,K+1,J)-Z(I,K,J)
        ENDDO
      ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,llyr,qlowx)
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        TWBS(I,J)=0.
        QWBS(I,J)=0.
        LLYR=LOWLYR(I,J)
        THLOW(I,J)=TH_PHY(I,LLYR,J)
        TLOW(I,J)=T_PHY(I,LLYR,J)
        QLOW(I,J)=MAX(Q(I,LLYR,J),EPSQ)
        QLOWX=QLOW(I,J)/(1.-QLOW(I,J))
        QLOW(I,J)=QLOWX/(1.+QLOWX)
        CWMLOW(I,J)=CWM(I,LLYR,J)
        PBLH(I,J)=MAX(PBLH(I,J),0.)
        PBLH(I,J)=MIN(PBLH(I,J),Z(I,KTE,J))
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!***  COMPUTE VELOCITY COMPONENTS AT MASS POINTS
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j,k,rwmsk,wmsk)
      DO J=MYJS1_P1,MYJE1_P1
!
        DO K=KTS,KTE
          DO I=MYIS_P1,MYIE_P1
            WMSK=VTM(I+IHE(J),K,J)+VTM(I+IHW(J),K,J)                    &
     &          +VTM(I,K,J+1)+VTM(I,K,J-1)
            IF(WMSK>0.)THEN
              RWMSK=1./WMSK
              U_PHY(I,K,J)=(U(I+IHE(J),K,J)*VTM(I+IHE(J),K,J)           &
     &                         +U(I+IHW(J),K,J)*VTM(I+IHW(J),K,J)       &
     &                         +U(I,K,J+1)*VTM(I,K,J+1)                 &
     &                         +U(I,K,J-1)*VTM(I,K,J-1))*RWMSK
              V_PHY(I,K,J)=(V(I+IHE(J),K,J)*VTM(I+IHE(J),K,J)           &
     &                         +V(I+IHW(J),K,J)*VTM(I+IHW(J),K,J)       &
     &                         +V(I,K,J+1)*VTM(I,K,J+1)                 &
     &                         +V(I,K,J-1)*VTM(I,K,J-1))*RWMSK
            ELSE
              U_PHY(I,K,J)=0.
              V_PHY(I,K,J)=0.
            ENDIF
          ENDDO
        ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,iend,istr,j)
      DO J=MYJS1_P1,MYJE1_P1
        IF(MOD(J,2)==0)THEN
          ISTR=MYIS_P1
          IEND=MIN(MYIE_P1,IDE-1)
        ELSE
          ISTR=MAX(MYIS_P1,IDS+1)
          IEND=MIN(MYIE_P1,IDE-1)
        ENDIF
!     
        DO I=ISTR,IEND
          UZ0H(I,J)=(UZ0(I+IHE(J),J)+UZ0(I+IHW(J),J)                    &
     &              +UZ0(I,J+1)+UZ0(I,J-1))*0.25
!!!  &              +UZ0(I,J+1)+UZ0(I,J-1))*HBM2(I,J)*0.25
          VZ0H(I,J)=(VZ0(I+IHE(J),J)+VZ0(I+IHW(J),J)                    &
     &              +VZ0(I,J+1)+VZ0(I,J-1))*0.25
!!!  &              +VZ0(I,J+1)+VZ0(I,J-1))*HBM2(I,J)*0.25
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!***  CALL SURFACE LAYER AND LAND SURFACE PHYSICS
!
!-----------------------------------------------------------------------
!
      CALL SET_TILES(GRID,IDS,IDE-1,JDS+1,JDE-1,ITS,ITE,JTS,JTE)
!
      DO J=JTS,JTE  !jm was JTS
      DO I=ITS,ITE
        IF(MODEL_CONFIG_REC%SF_SURFACE_PHYSICS(GRID%ID)==99)THEN
          ONE(I,J)=1.
        ELSE
!tgs  -  MAVAIL should not be equal to 1. for other LSMs
          ONE(I,J)=MAVAIL(I,J)
        ENDIF 
      ENDDO
      ENDDO
!
      CALL SURFACE_DRIVER(                                              &
     &           ACSNOM=ACSNOM,ACSNOW=ACSNOW,AKHS=AKHS,AKMS=AKMS        &
     &          ,ALBEDO=ALBEDO,BR=BR,CANWAT=CMC,CHKLOWQ=CHKLOWQ         &
     &          ,DT=DT,DX=DX,DZ8W=DZ,DZS=DZSOIL,GLW=RLW_DN_SFC          &
     &          ,GRDFLX=GRNFLX,GSW=RSW_NET_SFC,SWDOWN=RSW_DN_SFC        &
     &          ,GZ1OZ0=GZ1OZ0,HFX=TWBS                                 &
     &          ,HT=SFCZ,IFSNOW=IDUMMY,ISFFLX=ISFFLX,ISLTYP=ISLTYP      &
     &          ,ITIMESTEP=NTSD,IVGTYP=IVGTYP,LOWLYR=LOWLYR             &
     &          ,MAVAIL=ONE,RMOL=RMOL,NUM_SOIL_LAYERS=NSOIL,P8W=P8W &
     &          ,PBLH=PBLH,PI_PHY=PI_PHY,PSHLTR=PSHLTR,PSIH=PSIH        &
     &          ,PSIM=PSIM,P_PHY=P_PHY,Q10=Q10,Q2=Q2X,QFX=QWBS,QSFC=QS  &
     &          ,QSHLTR=QSHLTR,QZ0=QZ0,RAINCV=RAIN                      &
     &          ,RHO=RR,SFCEVP=SFCEVPX,SFCEXC=SFCEXC,SFCRUNOFF=SSROFF   &
     &          ,SMOIS=SMC,SMSTAV=SMSTAV,SMSTOT=SMSTOT,SNOALB=MXSNAL    &
     &          ,SNOW=SNOW,SNOWC=SNOWC,SNOWH=SNOWH,STEPBL=NPHS          &
     &          ,SST=SST,SST_UPDATE=SST_UPDATE                          &
     &          ,TH10=TH10,TH2=TH2X,T2=T2,THZ0=THZ0,TH_PHY=TH_PHY       &
     &          ,TMN=TG,TSHLTR=TSHLTR,TSK=TSFC,TSLB=STC,T_PHY=T_PHY     &
     &          ,U10=U10,UDRUNOFF=BGROFF,UST=USTAR,UZ0=UZ0H             &
     &          ,U_FRAME=U_FRAME,U_PHY=U_PHY,V10=V10,VEGFRA=VGFRCK      &
     &          ,VZ0=VZ0H,V_FRAME=V_FRAME,V_PHY=V_PHY                   &
     &          ,WARM_RAIN=WARM_RAIN,WSPD=WSPD,XICE=SICE                &
     &          ,XLAND=XLAND,Z=Z,ZNT=Z0,ZS=SLDPTH,CT=CT,TKE_MYJ=TKE     &
     &          ,ALBBCK=ALBASE,LH=ELFLX,SH2O=SH2O,SHDMAX=SHDMAX         &
     &          ,SHDMIN=SHDMIN,Z0=Z0BASE,FLQC=FLQC,FLHC=FLHC            &
     &          ,PSFC=PSFC_OUT,EMISS=EPSR                               &
     &          ,SF_SFCLAY_PHYSICS=CONFIG_FLAGS%SF_SFCLAY_PHYSICS       &
     &          ,SF_SURFACE_PHYSICS=CONFIG_FLAGS%SF_SURFACE_PHYSICS     &
     &          ,RA_LW_PHYSICS=CONFIG_FLAGS%RA_LW_PHYSICS               &
     &          ,UCMCALL=ucmcall                                        &
     &          ,IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE        &
     &          ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME        &
     &          ,I_START=GRID%I_START,I_END=GRID%I_END                  &
     &          ,J_START=GRID%J_START,J_END=GRID%J_END                  &
     &          ,KTS=KTS,KTE=KTE,NUM_TILES=GRID%NUM_TILES               &
           ! Optional args
     &          ,QV_CURR=MOIST(IMS,KMS,JMS,P_QV),F_QV=F_QV              &
     &          ,QC_CURR=MOIST(IMS,KMS,JMS,P_QC),F_QC=F_QC              &
     &          ,QR_CURR=MOIST(IMS,KMS,JMS,P_QR),F_QR=F_QR              &
     &          ,QI_CURR=MOIST(IMS,KMS,JMS,P_QI),F_QI=F_QI              &
     &          ,QS_CURR=MOIST(IMS,KMS,JMS,P_QS),F_QS=F_QS              & 
     &          ,QG_CURR=MOIST(IMS,KMS,JMS,P_QG),F_QG=F_QG              &
     &          ,RAINBL=RAINBL                                          &
! for RUCLSM
     &          ,QSG=QSG, QVG=QVG, QCG=QCG, SOILT1=SOILT1               &
     &          ,TSNAV=TSNAV, SMFR3D=SMFR3D, KEEPFR3DFLAG=KEEPFR3DFLAG  &
     &          ,POTEVP=POTEVP,SNOPCX=SNOPCX,SOILTB=SOILTB,SR=SR)
!
!-----------------------------------------------------------------------
!
!***  CALL FREE ATMOSPHERE TURBULENCE
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=JMS,JME
      DO K=KMS,KME
      DO I=IMS,IME
        DUDT(I,K,J)=0.
        DVDT(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
!***  THE SURFACE EXCHANGE COEFFICIENTS AKHS AND AKMS ARE ACTUALLY
!***  MULTIPLIED BY HALF THE DEPTH OF THE LOWEST LAYER.  WE MUST RETAIN
!***  THOSE VALUES FOR THE NEXT TIMESTEP SO USE AUXILLIARY ARRAYS FOR
!***  THE OUTPUT.
!
!$omp parallel do                                                       &
!$omp& private(dzhalf,i,j)
      DO J=JTS,JTE
      DO I=ITS,ITE
        DZHALF=0.5*DZ(I,KTS,J)
        AKHS_OUT(I,J)=AKHS(I,J)*DZHALF
        AKMS_OUT(I,J)=AKMS(I,J)*DZHALF
      ENDDO
      ENDDO
!
      CALL PBL_DRIVER(                                                &
     &                ITIMESTEP=NTSD,DT=DT                            &
     &               ,U_FRAME=U_FRAME,V_FRAME=V_FRAME                 &
     &               ,RUBLTEN=DUDT,RVBLTEN=DVDT,RTHBLTEN=RTHBLTEN     &
     &               ,RQVBLTEN=RQVBLTEN,RQCBLTEN=RQCBLTEN             &
     &               ,RQSBLTEN=RQIBLTEN                               &
     &               ,TSK=TSFC,XLAND=XLAND,ZNT=Z0,HT=SFCZ             &
     &               ,UST=USTAR, PBLH=PBLH                            &
     &               ,HFX=TWBS,QFX=QWBS,  GRDFLX=GRNFLX               &
     &               ,U_PHY=U_PHY,V_PHY=V_PHY,TH_PHY=TH_PHY,RHO=RR    &
     &               ,P_PHY=P_PHY,PI_PHY=PI_PHY,P8W=P8W,T_PHY=T_PHY   &
     &               ,DZ8W=DZ,Z=Z,TKE_MYJ=TKE,EL_MYJ=EL_MYJ           &
     &               ,EXCH_H=EXCH_H,AKHS=AKHS,AKMS=AKMS               &
     &               ,THZ0=THZ0,QZ0=QZ0,UZ0=UZ0H,VZ0=VZ0H             &
     &               ,QSFC=QS,LOWLYR=LOWLYR                           &
     &               ,PSIM=PSIM,PSIH=PSIH,GZ1OZ0=GZ1OZ0               &
     &               ,WSPD=WSPD,BR=BR,CHKLOWQ=CHKLOWQ                 &
     &               ,DX=DX,STEPBL=NPHS,WARM_RAIN=WARM_RAIN           &
     &               ,KPBL=KPBL,CT=CT,LH=ELFLX,SNOW=SNOW,XICE=SICE    &
     &               ,BL_PBL_PHYSICS=config_flags%bl_pbl_physics      &
     &               ,RA_LW_PHYSICS=config_flags%ra_lw_physics        &
     &               ,IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE &
     &               ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME &
     &               ,I_START=GRID%I_START,I_END=GRID%I_END           &
     &               ,J_START=GRID%J_START,J_END=GRID%J_END           &
     &               ,KTS=KTS,KTE=KTE,NUM_TILES=GRID%NUM_TILES        &
                ! Optional args
     &               ,QV_CURR=moist(IMS,KMS,JMS,P_QV) , F_QV=F_QV     &
     &               ,QC_CURR=moist(IMS,KMS,JMS,P_QC) , F_QC=F_QC     &
     &               ,QR_CURR=moist(IMS,KMS,JMS,P_QR) , F_QR=F_QR     &
     &               ,QI_CURR=moist(IMS,KMS,JMS,P_QI) , F_QI=F_QI     &
     &               ,QS_CURR=moist(IMS,KMS,JMS,P_QS) , F_QS=F_QS     &
     &               ,QG_CURR=moist(IMS,KMS,JMS,P_QG) , F_QG=F_QG   )
!
!***  NOTE THAT THE EXCHANGE COEFFICIENTS FOR HEAT EXCH_H COMING OUT OF
!***  PBL_DRIVER ARE DEFINED AT THE TOPS OF THE LAYERS KTS TO KTE-1
!***  IF MODULE_BL_MYJPBL WAS INVOKED.
!
!-----------------------------------------------------------------------
! UNCOMPUTED LOCATIONS MUST BE FILLED IN FOR THE POST-PROCESSOR
!-----------------------------------------------------------------------
!
!***  EASTERN GLOBAL BOUNDARY
!
      IF(MYIE==IDE)THEN
!$omp parallel do                                                       &
!$omp& private(i,j)
        DO J=JDS,JDE
        IF (J>=MYJS.AND.J<=MYJE)THEN
          TH10(MYIE,J)=TH10(MYIE-1,J)
          Q10(MYIE,J)=Q10(MYIE-1,J)
          U10(MYIE,J)=U10(MYIE-1,J)
          V10(MYIE,J)=V10(MYIE-1,J)
          TSHLTR(MYIE,J)=TSHLTR(MYIE-1,J)
          QSHLTR(MYIE,J)=QSHLTR(MYIE-1,J)
        ENDIF
        ENDDO
      ENDIF
!
!***  SOUTHERN GLOBAL BOUNDARY
!

      IF(MYJS==1)THEN
        DO J=1,2
        DO I=IDS,IDE
          IF (I>=MYIS.AND.I<=MYIE) THEN
            TH10(I,J)=TH10(I,MYJS+2)
            Q10(I,J)=Q10(I,MYJS+2)
            U10(I,J)=U10(I,MYJS+2)
            V10(I,J)=V10(I,MYJS+2)
            TSHLTR(I,J)=TSHLTR(I,MYJS+2)
            QSHLTR(I,J)=QSHLTR(I,MYJS+2)
          ENDIF
        ENDDO
        ENDDO
      ENDIF
!
!***  NORTHERN GLOBAL BOUNDARY
!
      IF(MYJE==JDE)THEN
!$omp parallel do                                                       &
!$omp& private(i,j)
        DO J=MYJE-1,MYJE
        DO I=IDS,IDE
          IF (I>=MYIS.AND.I<=MYIE) THEN
            TH10(I,J)=TH10(I,MYJE-2)
            Q10(I,J)=Q10(I,MYJE-2)
            U10(I,J)=U10(I,MYJE-2)
            V10(I,J)=V10(I,MYJE-2)
            TSHLTR(I,J)=TSHLTR(I,MYJE-2)
            QSHLTR(I,J)=QSHLTR(I,MYJE-2)
          ENDIF
        ENDDO
        ENDDO
      ENDIF
!
      IF(CONFIG_FLAGS%SF_SFCLAY_PHYSICS==1)THEN ! non-NMM package
!$omp parallel do                                                       &
!$omp& private(i,j)
        DO J=MYJS1,MYJE1
        DO I=MYIS,MYIE1
!         TSHLTR(I,J)=TSHLTR(I,J)*(1.E5/PSHLTR(I,J))**RCP
          IF(TSHLTR(I,J)<200..OR.TSHLTR(I,J)>350.)THEN
            WRITE(0,*)'Troublesome TSHLTR...I,J,TSHLTR,PSHLTR: ',       &
               I,J,TSHLTR(I,J),PSHLTR(I,J)
          ENDIF
	ENDDO
	ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!***  COMPUTE MODEL LAYER CONTAINING THE TOP OF THE BOUNDARY LAYER
!-----------------------------------------------------------------------
!
      IF(CONFIG_FLAGS%BL_PBL_PHYSICS/=MYJPBLSCHEME)THEN
        LENGTH_ROW=MYIE1-MYIS1+1
        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
          KPBL(I,J)=-1000
        ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(altitude,i,j,k,kount_all)
        DO J=MYJS2,MYJE2
          KOUNT_ALL=0
          find_kpbl : DO K=KTS,KTE
          DO I=MYIS1,MYIE1
            ALTITUDE=Z(I,K+1,J)-SFCZ(I,J)
            IF(PBLH(I,J)<=ALTITUDE.AND.KPBL(I,J)<0)THEN
              KPBL(I,J)=K
              KOUNT_ALL=KOUNT_ALL+1
            ENDIF
            IF(KOUNT_ALL==LENGTH_ROW)EXIT find_kpbl
          ENDDO
          ENDDO find_kpbl
        ENDDO
      ENDIF
!
      IF(MODEL_CONFIG_REC%SF_SURFACE_PHYSICS(GRID%ID)==99)THEN
        SNO_FACTR=1.
      ELSE
        SNO_FACTR=1000.
      ENDIF
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        SNO(I,J)=SNOW(I,J)
        SI(I,J)=SNOWH(I,J)*SNO_FACTR
        LPBL(I,J)=KTE-KPBL(I,J)+1
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  DIAGNOSTIC RADIATION ACCUMULATION
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j,tsfc2)
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
        ASWIN (I,J)=ASWIN (I,J)+RSWIN(I,J)*HBM2(I,J)*FACTRS(I,J)
        ASWOUT(I,J)=ASWOUT(I,J)-RSWOUT(I,J)*HBM2(I,J)*FACTRS(I,J)
        ASWTOA(I,J)=ASWTOA(I,J)+RSWTOA(I,J)*HBM2(I,J)*FACTRS(I,J)
        ALWIN (I,J)=ALWIN (I,J)+RLW_DN_SFC(I,J)
        ALWOUT(I,J)=ALWOUT(I,J)-RADOT (I,J)*HBM2(I,J)
        ALWTOA(I,J)=ALWTOA(I,J)+RLWTOA(I,J)*HBM2(I,J)
!
        TSFC2=TSFC(I,J)*TSFC(I,J)
        RADOT(I,J)=HBM2(I,J)*EPSR(I,J)*STBOLT*TSFC2*TSFC2
        THS(I,J)=TSFC(I,J)*EXNSFC(I,J)
        PREC(I,J)=0.
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  UPDATE TEMPERATURE, SPECIFIC HUMIDITY, CLOUD, AND TKE.
!-----------------------------------------------------------------------
!
      E_BDY=(ITE>=IDE)
!
!$omp parallel do                                                       &
!$omp& private(dqdt,dtdt,i,iend,j,k,qi,qold,qr,qw,ratiomx,i_m)
      DO J=MYJS2,MYJE2
        IEND=MYIE1
        IF(E_BDY.AND.MOD(J,2)==0)IEND=IEND-1
!
        DO K=KTS,KTE
        DO I=MYIS1,IEND
          DTDT=RTHBLTEN(I,K,J)*PI_PHY(I,K,J)
          DQDT=RQVBLTEN(I,K,J)         !Mixing ratio tendency
          T(I,K,J)=T(I,K,J)+DTDT*DTPHS
          QOLD=Q(I,K,J)
          RATIOMX=QOLD/(1.-QOLD)+DQDT*DTPHS
          Q(I,K,J)=RATIOMX/(1.+RATIOMX)
!         Q(I,K,J)=MAX(Q(I,K,J),EPSQ)
          QW=max(0.,MOIST(I,K,J,P_QC)+RQCBLTEN(I,K,J)*DTPHS )
          QI=max(0.,MOIST(I,K,J,P_QS)+RQIBLTEN(I,K,J)*DTPHS )
          QR=max(0.,MOIST(I,K,J,P_QR) )
!          CWM(I,K,J)=QW+QI+QR
          CWM(I,K,J)=0. 
!
          DO I_M=1,N_MOIST
             IF(I_M/=P_QV)THEN
               CWM(I,K,J)=CWM(I,K,J)+MOIST(I,K,J,I_M)
             ENDIF
             IF(I_M==P_QV)THEN
               MOIST(I,K,J,P_QV)=MAX(EPSQ,(MOIST(I,K,J,P_QV) + RQVBLTEN(I,K,J)*DTPHS) )
             ELSEIF (I_M==P_QC)THEN
                CWM(I,K,J)=MAX(0., (CWM(I,K,J) + RQCBLTEN(I,K,J)*DTPHS) )
             ELSEIF(I_M==P_QI)THEN
                CWM(I,K,J)=MAX(0., (CWM(I,K,J) + RQIBLTEN(I,K,J)*DTPHS) )
             ENDIF
          ENDDO
!
          MOIST(I,K,J,P_QC)=QW
          MOIST(I,K,J,P_QS)=QI
          MOIST(I,K,J,P_QR)=QR
!
          IF(CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW)THEN
            IF(QI<=EPSQ)THEN  
              F_ICE(I,K,J)=0.
            ELSE
              F_ICE(I,K,J)=MAX(0.,MIN(1.,QI/CWM(I,K,J)))
            ENDIF
!
            IF(QR<=EPSQ)THEN
              F_RAIN(I,K,J)=0.
            ELSE
              F_RAIN(I,K,J)=QR/(QW+QR)
            ENDIF
          ENDIF
!
          Q2(I,K,J)=2.*TKE(I,K,J)
        ENDDO
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!***
!***  SAVE SURFACE-RELATED FIELDS.
!***
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j,llij,xlvrw)
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        LLIJ=LOWLYR(I,J)
!
!-----------------------------------------------------------------------
!***  INSTANTANEOUS SENSIBLE AND LATENT HEAT FLUX
!-----------------------------------------------------------------------
!
        TWBS(I,J)=-TWBS(I,J)
        QWBS(I,J)=-QWBS(I,J)*XLV*CHKLOWQ(I,J)
!
!-----------------------------------------------------------------------
!***  ACCUMULATED QUANTITIES.
!***  IN OPNL LSM, SFCEVP APPEARS TO BE IN UNITS OF
!***  METERS OF LIQUID WATER.  IT IS COMING FROM
!***  WRF MODULE AS KG/M**2.
!-----------------------------------------------------------------------
!
        SFCSHX(I,J)=SFCSHX(I,J)+TWBS(I,J)
        SFCLHX(I,J)=SFCLHX(I,J)+QWBS(I,J)
        XLVRW=DTPHS/(XLV*RHOWATER)
        SFCEVP(I,J)=SFCEVP(I,J)-QWBS(I,J)*XLVRW
        POTEVP(I,J)=POTEVP(I,J)-QWBS(I,J)*SM(I,J)*XLVRW
        POTFLX(I,J)=POTEVP(I,J)*FACTOR
        SUBSHX(I,J)=SUBSHX(I,J)+GRNFLX(I,J)
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  COUNTERS
!-----------------------------------------------------------------------
!
      APHTIM=APHTIM+1.
      ARDSW =ARDSW +1.
      ARDLW =ARDLW +1.
      ASRFC =ASRFC +1.
!-----------------------------------------------------------------------
!
      END SUBROUTINE TURBL
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE UV_H_TO_V(NTSD,DT,NPHS,UZ0H,VZ0H,UZ0,VZ0               &
     &                    ,DUDT,DVDT,U,V,HBM2,VTM,IVE,IVW               & 
     &                    ,IDS,IDE,JDS,JDE,KDS,KDE                      &
     &                    ,IMS,IME,JMS,JME,KMS,KME                      &
     &                    ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    UV_H_TO_V   INTERPOLATE WINDS FROM H TO V POINTS
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 05-02-22       
!     
! ABSTRACT:
!     INTERPOLATE WINDS BACK TO V POINTS AFTER TURBULENCE
!     
! PROGRAM HISTORY LOG :
!   05-02-22  BLACK      - ORIGINATOR
!     
! USAGE: CALL TURBL FROM SOLVE_NMM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM
!$$$  
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,NPHS,NTSD
!
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IVE,IVW
!
      REAL,INTENT(IN) :: DT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: HBM2,UZ0H,VZ0H
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: DUDT,DVDT   &
     &                                                     ,VTM
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: UZ0,VZ0
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: U,V
!
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
!-----------------------------------------------------------------------
!
      INTEGER :: I,IEND,J,K
!
      REAL :: DTPHS
!
      LOGICAL :: E_BDY
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      DTPHS=NPHS*DT
      E_BDY=(ITE>=IDE)
!
!-----------------------------------------------------------------------
!***  RECONSTRUCT UZ0 AND VZ0 ON VELOCITY POINTS.
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
        UZ0(I,J)=(UZ0H(I+IVE(J),J)*HBM2(I+IVE(J),J)                     &
     &           +UZ0H(I+IVW(J),J)*HBM2(I+IVW(J),J)                     &
     &           +UZ0H(I,J+1)*HBM2(I,J+1)+UZ0H(I,J-1)*HBM2(I,J-1))*0.25
        VZ0(I,J)=(VZ0H(I+IVE(J),J)*HBM2(I+IVE(J),J)                     &
     &           +VZ0H(I+IVW(J),J)*HBM2(I+IVW(J),J)                     &
     &           +VZ0H(I,J+1)*HBM2(I,J+1)+VZ0H(I,J-1)*HBM2(I,J-1))*0.25
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  INTERPOLATE WIND TENDENCIES TO VELOCITY POINTS AND UPDATE WINDS.
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,iend,j,k)
      DO J=MYJS2,MYJE2
        IEND=MYIE1
        IF(E_BDY.AND.MOD(J,2)==1)IEND=IEND-1
!
        DO K=KTS,KTE
        DO I=MYIS1,IEND
          U(I,K,J)=(DUDT(I+IVE(J),K,J)+DUDT(I+IVW(J),K,J)               &
     &             +DUDT(I,K,J+1)+DUDT(I,K,J-1))*0.25*DTPHS             &
     &             *VTM(I,K,J)+U(I,K,J)
          V(I,K,J)=(DVDT(I+IVE(J),K,J)+DVDT(I+IVW(J),K,J)               &
     &             +DVDT(I,K,J+1)+DVDT(I,K,J-1))*0.25*DTPHS             &
     &             *VTM(I,K,J)+V(I,K,J)
        ENDDO
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
      END SUBROUTINE UV_H_TO_V
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE CUCNVC(NTSD,DT,NCNVC,NRADS,NRADL                       &
     &                 ,GPS,RESTRT,HYDRO                                &
     &                 ,CLDEFI,LMH,N_MOIST,ENSDIM                       &
     &                 ,MOIST                                           &
     &                 ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2               &
     &                 ,F_ICE,F_RAIN                                    &
!***  Changes for other cu-schemes, most for gd scheme
     &                 ,APR_GR,APR_W,APR_MC,TTEN,QTEN                   &
     &                 ,APR_ST,APR_AS,APR_CAPMA                         &
     &                 ,APR_CAPME          ,APR_CAPMI                   &
     &                 ,MASS_FLUX         ,XF_ENS                       &
     &                 ,PR_ENS,GSW                                      &
#ifdef WRF_CHEM
     &                 ,GD_CLOUD,GD_CLOUD2                              &
#endif
!
     &                 ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TCUCN              &
     &                 ,OMGALF,U,V,VTM,WINT,Z,FIS,W0AVG                 &
     &                 ,PREC,ACPREC,CUPREC,CUPPT,CPRATE                 &
     &                 ,SM,HBM2,LPBL,CNVBOT,CNVTOP                      &
     &                 ,HTOP,HBOT,HTOPD,HBOTD,HTOPS,HBOTS               &
     &                 ,RTHBLTEN,RQVBLTEN,RTHRATEN                      & 
     &                 ,AVCNVC,ACUTIM,ZERO_3D,IHE,IHW                   &
     &                 ,GRID,CONFIG_FLAGS                               &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CUCNVC      CONVECTIVE PRECIPITATION OUTER DRIVER
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 02-03-21       
!     
! ABSTRACT:
!     CUCVNC DRIVES THE WRF CONVECTION SCHEMES
!     
! PROGRAM HISTORY LOG:
!   02-03-21  BLACK      - ORIGINATOR
!   04-11-18  BLACK      - THREADED
!     
! USAGE: CALL CUCNVC FROM SOLVE_NMM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM 
!$$$  
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: ENSDIM                                      &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,N_MOIST,NCNVC,NTSD,NRADS,NRADL
!
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LMH,LPBL
!
      REAL,INTENT(IN) :: DT,GPS,PDTOP,PT
!
      REAL,INTENT(INOUT) :: ACUTIM,AVCNVC
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
      REAL,DIMENSION(KMS:KME  ),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: FIS,HBM2,PD,RES,SM
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACPREC,CLDEFI    &
     &                                                ,CNVBOT,CNVTOP    &
     &                                                ,CUPPT,CUPREC     &
     &                                                ,HBOT,HTOP        &
     &                                                ,HBOTD,HTOPD      &
     &                                                ,HBOTS,HTOPS      &
     &                 ,APR_GR,APR_W,APR_MC                             &
     &                 ,APR_ST,APR_AS,APR_CAPMA                         &
     &                 ,APR_CAPME          ,APR_CAPMI                   &
     &                 ,MASS_FLUX                                       &
     &                 ,GSW  ,PREC,CPRATE
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: F_ICE       &
     &                                                     ,F_RAIN
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: TTEN     &
     &                                                     ,QTEN        &
     &                                       ,RTHBLTEN,RQVBLTEN,RTHRATEN

!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: Q,T      &
     &                                                        ,CWM      &
     &                                                        ,TCUCN    &
     &                                                        ,W0AVG    &
     &                                                        ,WINT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: OMGALF      &
     &                                                     ,PINT,U,V    &
     &                                                     ,VTM,Z
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: ZERO_3D
      REAL,DIMENSION(IMS:IME,jMS:jME,1:ENSDIM),INTENT(INOUT) ::          &
     &                         XF_ENS                                    &
     &                        ,PR_ENS
      
!    
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,N_MOIST)                   &
     &                                           ,INTENT(INOUT) :: moist
#ifdef WRF_CHEM
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: GD_CLOUD,GD_CLOUD2
#endif
!
      LOGICAL,INTENT(IN) :: HYDRO,RESTRT
!
      TYPE(DOMAIN),TARGET :: GRID
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
!-----------------------------------------------------------------------
      INTEGER :: I,ICLDCK,IENDX,J,K,MNTO,NCUBOT,NCUTOP,NSTEP_CNV        &
     &          ,N_TIMSTPS_OUTPUT
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME) :: KPBL,LBOT,LOWLYR,LTOP
!
      REAL :: CAPA,CF_HI,DPL,DQDT,DTCNVC,DTDT,FICE,FRAIN,G_INV          &
     &       ,PCPCOL,PDSL,PLYR,QI,QL_K,QR,QW,RDTCNVC,RWMSK,WMSK,WC
!
      REAL,DIMENSION(KMS:KME-1) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CUBOT,CUTOP,NCA,RAINC,RAINCV   &
     &                                  ,SFCZ,XLAND
!
      REAL,DIMENSION(IMS:IME,KMS:KME) :: WMID
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: DZ,P8W,P_PHY,PI_PHY    &
     &                                          ,RQCCUTEN,RQRCUTEN      &
     &                                          ,RQICUTEN,RQSCUTEN      &
     &                                          ,RQVCUTEN,RR,RTHCUTEN   &
     &                                          ,T_PHY,TH_PHY           &
     &                                          ,U_PHY,V_PHY
!
      REAL,DIMENSION(IMS:IME,JMS:JME)        :: ZERO_2D
      REAL,DIMENSION(IMS:IME,JMS:JME,ENSDIM) :: ZERO_GD
!
      LOGICAL :: RESTART,WARM_RAIN
      LOGICAL,DIMENSION(IMS:IME,JMS:JME) :: CU_ACT_FLAG
!
!-----------------------------------------------------------------------
!***  FOR TEMPERATURE CHANGE CHECK ONLY.
!-----------------------------------------------------------------------
      INTEGER :: DTEMP_CHECK=1.0
      REAL :: TCHANGE
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!***  RESET THE HBOT/HTOP CONVECTIVE CLOUD BOTTOM (BASE) AND TOP ARRAYS
!***  USED IN RADIATION.  THEY STORE THE MAXIMUM VERTICAL LIMITS OF 
!***  CONVECTIVE CLOUD BETWEEN RADIATION CALLS.  CUPPT IS THE ACCUMULATED
!***  CONVECTIVE PRECIPITATION BETWEEN RADIATION CALLS.
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,NRADS)==0.OR.MOD(NTSD,NRADL)==0)THEN
         DO J=JMS,JME
         DO I=IMS,IME
           HTOP(I,J)=0.
           HBOT(I,J)=REAL(KTE+1)
           CUPPT(I,J)=0.
         ENDDO
         ENDDO
      ENDIF
!-----------------------------------------------------------------------
      IF(MOD(NTSD,NCNVC)/=0.AND.                                      &
     &   CONFIG_FLAGS%CU_PHYSICS==BMJSCHEME)RETURN
      IF(MOD(NTSD,NCNVC)/=0.AND.                                      &
     &   CONFIG_FLAGS%CU_PHYSICS==SASSCHEME)RETURN
!-----------------------------------------------------------------------
      NSTEP_CNV=NCNVC
!
      RESTART=RESTRT
!-----------------------------------------------------------------------
      IF(CONFIG_FLAGS%CU_PHYSICS==KFETASCHEME)THEN
!
        IF(.NOT.RESTART.AND.NTSD==0)THEN
!$omp parallel do                                                       &
!$omp& private(i,j,k)
          DO J=JTS,JTE
          DO K=KTS,KTE
          DO I=ITS,ITE
            W0AVG(I,K,J)=0.
          ENDDO
          ENDDO
          ENDDO
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  GENERAL PREPARATION 
!-----------------------------------------------------------------------
!
      AVCNVC=AVCNVC+1.
      ACUTIM=ACUTIM+1.
!
      DTCNVC=NCNVC*DT
      RDTCNVC=1./DTCNVC
      CAPA=R_D/CP
      G_INV=1./G
!
!$omp parallel do                                                       &
!$omp& private(dpl,fice,frain,i,j,k,pdsl,plyr,ql,tl)
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
!
        PDSL=PD(I,J)*RES(I,J)
        RAINCV(I,J)=0.
        RAINC(I,J)=0.
        P8W(I,KTS,J)=PD(I,J)+PDTOP+PT
        LOWLYR(I,J)=KTE+1-LMH(I,J)
        XLAND(I,J)=SM(I,J)+1.
        NCA(I,J)=0.
        SFCZ(I,J)=FIS(I,J)*G_INV
!tgs
          CUTOP(I,J)=HTOP(I,J)
          CUBOT(I,J)=HBOT(I,J)
!
!***  LPBL IS THE MODEL LAYER CONTAINING THE PBL TOP
!***  COUNTING DOWNWARD FROM THE TOP OF THE DOMAIN
!***  SO KPBL IS THE SAME LAYER COUNTING UPWARD FROM 
!***  THE GROUND.
!
        KPBL(I,J)=KTE-LPBL(I,J)+1
        ZERO_2D(I,J)=0
!
        DO K=KTS,KTE
          DPL=DETA1(K)*PDTOP+DETA2(K)*PDSL
          QL(K)=AMAX1(Q(I,K,J),EPSQ)
          PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL+PT
          TL(K)=T(I,K,J)
!
          RR(I,K,J)=PLYR/(R_D*TL(K)*(P608*QL(K)+1.))
          T_PHY(I,K,J)=TL(K)

          TH_PHY(I,K,J)=TL(K)*(1.E5/PLYR)**CAPA
!!!       P8W(I,KFLIP,J)=PINT(I,K+1,J)
          P8W(I,K+1,J)=ETA1(K+1)*PDTOP+ETA2(K+1)*PDSL+PT
          P_PHY(I,K,J)=PLYR
          PI_PHY(I,K,J)=(PLYR*1.E-5)**CAPA
!
          RTHCUTEN(I,K,J)=0.
          RQVCUTEN(I,K,J)=0.
          RQCCUTEN(I,K,J)=0.
          RQRCUTEN(I,K,J)=0.
          RQICUTEN(I,K,J)=0.
          RQSCUTEN(I,K,J)=0.
        ENDDO
!
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!

      IF(.NOT.HYDRO)THEN
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO J=MYJS2,MYJE2
        DO K=KTS,KTE
        DO I=MYIS1,MYIE1
          DZ(I,K,J)=Z(I,K+1,J)-Z(I,K,J)
        ENDDO
        ENDDO
        ENDDO
!
        IF(NTSD==0)THEN
!$omp parallel do                                                       &
!$omp& private(i,j,k)
          DO J=MYJS2,MYJE2
          DO K=KTS,KTE
          DO I=MYIS1,MYIE1
            WINT(I,K,J)=0.
          ENDDO
          ENDDO
          ENDDO
        ENDIF
      ELSE
        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
          WINT(I,1,J)=0.
          WINT(I,KTE+1,J)=0.
        ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k,plyr,wmid)
        DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
            WMID(I,KTS)=-OMGALF(I,KTS,J)*CP/(G*DT)
            PDSL=PD(I,J)*RES(I,J)
            PLYR=AETA1(KTS)*PDTOP+AETA2(KTS)*PDSL+PT
            DZ(I,KTS,J)=T(I,KTS,J)*(P608*Q(I,KTS,J)+1.)*R_D             &
     &                 *(P8W(I,KTS,J)-P8W(I,KTS+1,J))                   &
     &                 /(PLYR*G)
          ENDDO
!
          DO K=KTS+1,KTE
          DO I=MYIS1,MYIE1
            QL_K=AMAX1(Q(I,K,J),EPSQ)
            WMID(I,K)=-OMGALF(I,K,J)*CP/(G*DT)
            WINT(I,K,J)=0.5*(WMID(I,K-1)+WMID(I,K))
            DZ(I,K,J)=T_PHY(I,K,J)*(P608*QL_K+1.)*R_D                   &
     &               *(P8W(I,K,J)-P8W(I,K+1,J))                         &
     &               /(P_PHY(I,K,J)*G)
          ENDDO
          ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  COMPUTE VELOCITY COMPONENTS AT MASS POINTS
!-----------------------------------------------------------------------
!
      IF(CONFIG_FLAGS%CU_PHYSICS.NE.BMJSCHEME)THEN
!
!$omp parallel do                                                       &
!$omp& private(i,j,k,rwmsk,wmsk)
        DO J=MYJS1_P1,MYJE1_P1
!
          DO K=KTS,KTE
          DO I=MYIS_P1,MYIE_P1
            WMSK=VTM(I+IHE(J),K,J)+VTM(I+IHW(J),K,J)                    &
     &          +VTM(I,K,J+1)+VTM(I,K,J-1)
            IF(WMSK>0.)THEN
              RWMSK=1./WMSK
              U_PHY(I,K,J)=(U(I+IHE(J),K,J)*VTM(I+IHE(J),K,J)           &
     &                         +U(I+IHW(J),K,J)*VTM(I+IHW(J),K,J)       &
     &                         +U(I,K,J+1)*VTM(I,K,J+1)                 &
     &                         +U(I,K,J-1)*VTM(I,K,J-1))*RWMSK
              V_PHY(I,K,J)=(V(I+IHE(J),K,J)*VTM(I+IHE(J),K,J)           &
     &                         +V(I+IHW(J),K,J)*VTM(I+IHW(J),K,J)       &
     &                         +V(I,K,J+1)*VTM(I,K,J+1)                 &
     &                         +V(I,K,J-1)*VTM(I,K,J-1))*RWMSK
            ELSE
              U_PHY(I,K,J)=0.
              V_PHY(I,K,J)=0.
            ENDIF
          ENDDO
          ENDDO
!
        ENDDO
!
      ENDIF
!-----------------------------------------------------------------------
!
!***  SINGLE-COLUMN CONVECTION
!
!-----------------------------------------------------------------------
!
      CALL SET_TILES(GRID,IDS+1,IDE-1,JDS+2,JDE-2,ITS,ITE,JTS,JTE)
!
      CALL CUMULUS_DRIVER(                                              &
     &                  IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE &
     &                 ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME &
     &                 ,I_START=GRID%I_START,I_END=GRID%I_END           &
     &                 ,J_START=GRID%J_START,J_END=GRID%J_END           &
     &                 ,KTS=KTS,KTE=KTE,NUM_TILES=GRID%NUM_TILES        &
                  ! Prognostic
     &                 ,U=U_PHY,V=V_PHY,TH=TH_PHY,T=T_PHY,W=WINT        &
     &                 ,P=P_PHY,PI=PI_PHY,RHO=RR,W0AVG=W0AVG            &
                  ! Others
     &                 ,ITIMESTEP=NTSD,DT=DT,DX=GPS                     &
     &                 ,RAINC=RAINC,RAINCV=RAINCV,NCA=NCA               &
     &                 ,DZ8W=DZ,P8W=P8W,FORCET=TTEN,FORCEQ=QTEN         &
     &                 ,CLDEFI=cldefi,LOWLYR=lowlyr,XLAND=xland         &
     &                 ,CU_ACT_FLAG=cu_act_flag,WARM_RAIN=warm_rain     &
     &                 ,STEPCU=NSTEP_CNV,GSW=gsw                        &
     &                 ,HTOP=CUTOP,HBOT=CUBOT,KPBL=KPBL,HT=SFCZ         &   
     &                 ,APR_GR=apr_gr,APR_W=apr_w,APR_MC=apr_mc         &
     &                 ,APR_ST=apr_st,APR_AS=apr_as,APR_CAPMA=apr_capma &
     &                 ,APR_CAPME=apr_capme,APR_CAPMI=apr_capmi         &
     &                 ,MASS_FLUX=mass_flux,XF_ENS=xf_ens               &
     &                 ,PR_ENS=pr_ens                                   &
#ifdef WRF_CHEM
     &                 ,gd_cloud=gd_cloud,gd_cloud2=gd_cloud2           &
#endif

     &                 ,ENSDIM=ENSDIM,MAXIENS=1,MAXENS=3                &
     &                 ,MAXENS2=3,MAXENS3=16                            &
     &                 ,RTHCUTEN=RTHCUTEN ,RQVCUTEN=RQVCUTEN            &
     &                 ,RQCCUTEN=RQCCUTEN ,RQRCUTEN=RQRCUTEN            &
     &                 ,RQICUTEN=RQICUTEN ,RQSCUTEN=RQSCUTEN            &
     &                 ,RTHBLTEN=RTHBLTEN,RQVBLTEN=RQVBLTEN             & 
     &                 ,RTHRATEN=RTHRATEN                               & 
                  ! Selection argument
     &                 ,CU_PHYSICS=CONFIG_FLAGS%CU_PHYSICS              &
                  ! Moisture tracer arguments
     &                 ,QV_CURR=MOIST(IMS,KMS,JMS,P_QV),F_QV=F_QV       &
     &                 ,QC_CURR=MOIST(IMS,KMS,JMS,P_QC),F_QC=F_QC       &
     &                 ,QR_CURR=MOIST(IMS,KMS,JMS,P_QR),F_QR=F_QR       &
     &                 ,QI_CURR=MOIST(IMS,KMS,JMS,P_QI),F_QI=F_QI       &
     &                 ,QS_CURR=MOIST(IMS,KMS,JMS,P_QS),F_QS=F_QS       &
     &                 ,QG_CURR=MOIST(IMS,KMS,JMS,P_QG),F_QG=F_QG      )
!
!-----------------------------------------------------------------------
!
!***  CNVTOP/CNVBOT HOLD THE MAXIMUM VERTICAL LIMITS OF CONVECTIVE CLOUD 
!***  BETWEEN HISTORY OUTPUT TIMES.  HBOTS/HTOPS STORE SIMILIAR INFORMATION
!***  FOR SHALLOW (NONPRECIPITATING) CONVECTION, AND HBOTD/HTOPD ARE FOR
!***  DEEP (PRECIPITATING) CONVECTION.  
!
      CF_HI=CONFIG_FLAGS%HISTORY_INTERVAL
      N_TIMSTPS_OUTPUT=NINT(60.*CF_HI/DT)
      MNTO=MOD(NTSD,N_TIMSTPS_OUTPUT)
!
      IF(MNTO>0.AND.MNTO<=NCNVC)THEN
        DO J=MYJS2,MYJE2
        IENDX=MYIE1
        IF(MOD(J,2)==0.AND.ITE==IDE-1)IENDX=IENDX-1
        DO I=MYIS1,IENDX
          CNVBOT(I,J)=REAL(KTE+1.)
          CNVTOP(I,J)=0.
          HBOTD(I,J)=REAL(KTE+1.)
          HTOPD(I,J)=0.
          HBOTS(I,J)=REAL(KTE+1.)
          HTOPS(I,J)=0.
        ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(dqdt,dtdt,i,iendx,j,k,ncubot,ncutop,pcpcol               &
!$omp&        ,tchange                                                  &
!$omp&        )
      DO J=MYJS2,MYJE2
      IENDX=MYIE1
      IF(MOD(J,2)==0.AND.ITE==IDE-1)IENDX=IENDX-1
      DO I=MYIS1,IENDX
!
!***  UPDATE TEMPERATURE, SPECIFIC HUMIDITY, AND HEATING.
!***  THE FLIP IS BECAUSE RTHCUTEN AND RQVCUTEN REACH THIS POINT
!***  WITH LAYER 1 AT THE BOTTOM.
!
        DO K=KTS,KTE
!
!***  RQVCUTEN IN BMJDRV IS THE MIXING RATIO TENDENCY,
!***  SO RETRIEVE DQDT BY CONVERTING TO SPECIFIC HUMIDITY.
!
          DQDT=RQVCUTEN(I,K,J)/(1.+MOIST(I,K,J,P_QV))**2
!
!***  RTHCUTEN IN BMJDRV IS DTDT OVER PI.
!
          DTDT=RTHCUTEN(I,K,J)*PI_PHY(I,K,J)
          T(I,K,J)=T(I,K,J)+DTDT*DTCNVC
          Q(I,K,J)=Q(I,K,J)+DQDT*DTCNVC
          MOIST(I,K,J,P_QV)=Q(I,K,J)/(1.-Q(I,K,J))       !Convert to mixing ratio
!tgs - added next two lines
          cps_select: SELECT CASE(config_flags%cu_physics)
!
          CASE (KFSCHEME,KFETASCHEME,GDSCHEME,SASSCHEME)
             MOIST(I,K,J,P_QS)=MAX(0.,MOIST(I,K,J,P_QS)+RQICUTEN(I,K,J)*DTCNVC)
             MOIST(I,K,J,P_QC)=MAX(0.,MOIST(I,K,J,P_QC)+RQCCUTEN(I,K,J)*DTCNVC)
          END SELECT cps_select
!
          TCUCN(I,K,J)=TCUCN(I,K,J)+DTDT
!
          TCHANGE=DTDT*DTCNVC
	  IF(ABS(TCHANGE)>DTEMP_CHECK)THEN
            WRITE(0,*)'BIG T CHANGE BY CONVECTION:  I,J,K,NTSD',TCHANGE,I,J,K,NTSD
	  ENDIF
!
        ENDDO
!
!***  UPDATE PRECIPITATION
!
        PCPCOL=RAINCV(I,J)*1.E-3*NSTEP_CNV
        PREC(I,J)=PREC(I,J)+PCPCOL
        ACPREC(I,J)=ACPREC(I,J)+PCPCOL
        CUPREC(I,J)=CUPREC(I,J)+PCPCOL
        CUPPT(I,J)=CUPPT(I,J)+PCPCOL
        CPRATE(I,J)=PCPCOL
!
!***  SAVE CLOUD TOP AND BOTTOM FOR RADIATION (HTOP/HBOT) AND
!***  FOR OUTPUT (CNVTOP/CNVBOT, HTOPS/HBOTS, HTOPD/HBOTD) ARRAYS.
!***  MUST BE TREATED SEPARATELY FROM EACH OTHER.
!
        NCUTOP=NINT(CUTOP(I,J))
        NCUBOT=NINT(CUBOT(I,J))
!
        IF(NCUTOP>1.AND.NCUTOP<KDE)THEN
          HTOP(I,J)=MAX(CUTOP(I,J),HTOP(I,J))
          CNVTOP(I,J)=MAX(CUTOP(I,J),CNVTOP(I,J))
          IF(PCPCOL>0.)THEN
            HTOPD(I,J)=MAX(CUTOP(I,J),HTOPD(I,J))
          ELSE
            HTOPS(I,J)=MAX(CUTOP(I,J),HTOPS(I,J))
          ENDIF
        ENDIF
        IF(NCUBOT>0.AND.NCUBOT<KDE)THEN
          HBOT(I,J)=MIN(CUBOT(I,J),HBOT(I,J))
          CNVBOT(I,J)=MIN(CUBOT(I,J),CNVBOT(I,J))
          IF(PCPCOL>0.)THEN
            HBOTD(I,J)=MIN(CUBOT(I,J),HBOTD(I,J))
          ELSE
            HBOTS(I,J)=MIN(CUBOT(I,J),HBOTS(I,J))
          ENDIF
        ENDIF
!
      ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=JMS,JME
      DO K=KMS,KME
      DO I=IMS,IME
        ZERO_3D(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
      END SUBROUTINE CUCNVC
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE GSMDRIVE(NTSD,DT,NPHS,N_MOIST                          &
     &                   ,DX,DY,LMH,SM,HBM2,FIS                         &
     &                   ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2             &
     &                   ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TRAIN            &
     &                   ,MOIST,SCALAR,N_SCALAR                         &
     &                   ,F_ICE,F_RAIN,F_RIMEF,SR                       &
     &                   ,PREC,ACPREC,AVRAIN,ZERO_3D                    &
     &                   ,MP_RESTART_STATE                              &
     &                   ,TBPVS_STATE                                   &
     &                   ,TBPVS0_STATE                                  &
     &                   ,GRID,CONFIG_FLAGS                             &
     &                   ,IDS,IDE,JDS,JDE,KDS,KDE                       &
     &                   ,IMS,IME,JMS,JME,KMS,KME                       &
     &                   ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    GSMDRIVE    MICROPHYSICS OUTER DRIVER
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 02-03-26       
!     
! ABSTRACT:
!     GSMDRIVE DRIVES THE MICROPHYSICS SCHEMES
!     
! PROGRAM HISTORY LOG:
!   02-03-26  BLACK      - ORIGINATOR
!   04-11-18  BLACK      - THREADED
!     
! USAGE: CALL GSMDRIVE FROM SOLVE_NMM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM
!$$$  
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,N_MOIST,N_SCALAR,NPHS,NTSD
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LMH
!
      REAL,INTENT(IN) :: DT,DX,DY,PDTOP,PT
!
      REAL,INTENT(INOUT) :: AVRAIN
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: FIS,HBM2,PD,RES,SM
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: PINT
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: ZERO_3D
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACPREC,PREC
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: CWM,Q,T  &
     &                                                        ,TRAIN
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: F_ICE    &
     &                                                        ,F_RAIN   &
     &                                                        ,F_RIMEF

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,n_moist),INTENT(INOUT) :: MOIST
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,n_scalar),INTENT(INOUT) :: SCALAR
!
!***  State var for etampnew microphysics (JM, 2005 05 02)
!
      REAL,DIMENSION(:),INTENT(INOUT) ::               MP_RESTART_STATE &
     &                                                     ,TBPVS_STATE &
     &                                                    ,TBPVS0_STATE

!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: SR
!
      TYPE(DOMAIN),TARGET :: GRID
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
!-----------------------------------------------------------------------
      INTEGER :: I,I_M,IENDX,J,K,IJ
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME) :: LOWLYR
!
      REAL :: CAPA,DPL,DTPHS,PCPCOL,PDSL,PLYR,RDTPHS,RG,TNEW
!
      REAL,DIMENSION(KMS:KME-1) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CUBOT,CUTOP,RAINNC,RAINNCV,XLAND      &
     &                                  ,ZERO_2D
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: DZ,P8W,P_PHY,PI_PHY    &
     &                                          ,RR,T_PHY,TH_PHY
!
      LOGICAL :: E_BDY,F_QT,QT_PRESENT,WARM_RAIN
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      IF(CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW)THEN
        QT_PRESENT=.TRUE.
      ELSE
        QT_PRESENT=.FALSE.
      ENDIF
!
      DTPHS=NPHS*DT
      RDTPHS=1./DTPHS
      CAPA=R_D/CP
      RG=1./G
      AVRAIN=AVRAIN+1.
!
!-----------------------------------------------------------------------
!
!***  PREPARE NEEDED ARRAYS
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dpl,i,j,k,pdsl,plyr,ql,tl)
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
!
        PDSL=PD(I,J)*RES(I,J)
        P8W(I,KTE+1,J)=PT
        LOWLYR(I,J)=KTE+1-LMH(I,J)
        XLAND(I,J)=SM(I,J)+1.
        ZERO_2D(I,J)=0.
! FILL RAINNC WITH ZERO (NORMALLY CONTAINS THE NONCONVECTIVE 
!         ACCUMULATED RAIN BUT NOT YET USED BY NMM)
! COULD BE OBTAINED FROM ACPREC AND CUPREC (ACPREC-CUPREC) 
        RAINNC(I,J)=0.
!
!***  FILL THE SINGLE-COLUMN INPUT
!
        DO K=KTS,KTE
          DPL=DETA1(K)*PDTOP+DETA2(K)*PDSL
          QL(K)=AMAX1(Q(I,K,J),EPSQ)
!!!       PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL+PT
          PLYR=(PINT(I,K,J)+PINT(I,K+1,J))*0.5
          TL(K)=T(I,K,J)
!
          RR(I,K,J)=PLYR/(R_D*TL(K)*(P608*QL(K)+1.))
          T_PHY(I,K,J)=TL(K)
          PI_PHY(I,K,J)=(PLYR*1.E-5)**CAPA
          TH_PHY(I,K,J)=TL(K)/PI_PHY(I,K,J)
!!!       P8W(I,KFLIP,J)=PINT(I,K+1,J)
          P8W(I,K,J)=ETA1(K)*PDTOP+ETA2(K)*PDSL+PT
          P_PHY(I,K,J)=PLYR
          DZ(I,K,J)=DPL*RG/RR(I,K,J)
        ENDDO
!
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!***  CALL MICROPHYSICS
!
!-----------------------------------------------------------------------
!
      CALL SET_TILES(GRID,IDS+1,IDE-1,JDS+2,JDE-2,ITS,ITE,JTS,JTE)
!
      CALL MICROPHYSICS_DRIVER(                                         &
     &                  TH=TH_PHY,RHO=RR,PI_PHY=PI_PHY,P=P_PHY          &
     &                 ,RAINNC=RAINNC,RAINNCV=RAINNCV                   &
     &                 ,DZ8W=DZ,P8W=P8W,DT=DTPHS,DX=DX,DY=DY            &
     &                 ,MP_PHYSICS=CONFIG_FLAGS%MP_PHYSICS              &
     &                 ,SPECIFIED=CONFIG_FLAGS%SPECIFIED                &
     &                        .OR.CONFIG_FLAGS%NESTED                   &
     &                 ,SPEC_ZONE=0,WARM_RAIN=WARM_RAIN                 &
     &                 ,XLAND=XLAND,ITIMESTEP=NTSD-1                    &
     &                 ,F_ICE_PHY=F_ICE,F_RAIN_PHY=F_RAIN               &
     &                 ,F_RIMEF_PHY=F_RIMEF                             &
     &                 ,LOWLYR=LOWLYR,SR=SR                             &
     &                 ,QV_CURR=MOIST(IMS,KMS,JMS,P_QV),F_QV=F_QV       &
     &                 ,QC_CURR=MOIST(IMS,KMS,JMS,P_QC),F_QC=F_QC       &
     &                 ,QR_CURR=MOIST(IMS,KMS,JMS,P_QR),F_QR=F_QR       &
     &                 ,QI_CURR=MOIST(IMS,KMS,JMS,P_QI),F_QI=F_QI       &
     &                 ,QS_CURR=MOIST(IMS,KMS,JMS,P_QS),F_QS=F_QS       &
     &                 ,QG_CURR=MOIST(IMS,KMS,JMS,P_QG),F_QG=F_QG       &
     &                 ,QNI_CURR=SCALAR(IMS,KMS,JMS,P_QNI),F_QNI=F_QNI  &
     &                 ,QT_CURR=CWM,F_QT=qt_present                     &
     &                 ,MP_RESTART_STATE=MP_RESTART_STATE               &
     &                 ,TBPVS_STATE=TBPVS_STATE                         &
     &                 ,TBPVS0_STATE=TBPVS0_STATE                       &
     &                 ,IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE &
     &                 ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME &
     &                 ,I_START=GRID%I_START,I_END=GRID%I_END           &
     &                 ,J_START=GRID%J_START,J_END=GRID%J_END           &
     &                 ,KTS=KTS,KTE=KTE,NUM_TILES=GRID%NUM_TILES        &
                                                                        )

!$omp parallel do                                                       &
!$omp& private(ij)
      DO IJ=1,GRID%NUM_TILES
        CALL MICROPHYSICS_ZERO_OUT(                                     &
                     MOIST,N_MOIST,CONFIG_FLAGS                         &
                    ,IDS,IDE,JDS,JDE,KDS,KDE                            &
                    ,IMS,IME,JMS,JME,KMS,KME                            &
                    ,GRID%I_START(IJ),GRID%I_END(IJ)                    &
                    ,GRID%J_START(IJ),GRID%J_END(IJ)                    &
                    ,KTS,KTE                                       )
      ENDDO



!
!-----------------------------------------------------------------------
!
      E_BDY=(ITE>=IDE)
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,k,pcpcol,tnew,i_m)
      DO J=MYJS2,MYJE2
      IENDX=MYIE1
      IF(E_BDY.AND.MOD(J,2)==0)IENDX=IENDX-1
      DO I=MYIS1,IENDX
!
!***  UPDATE TEMPERATURE, SPECIFIC HUMIDITY, CLOUD WATER, AND HEATING.
!
        DO K=KTS,KTE
          TNEW=TH_PHY(I,K,J)*PI_PHY(I,K,J)
          TRAIN(I,K,J)=TRAIN(I,K,J)+(TNEW-T(I,K,J))*RDTPHS
          T(I,K,J)=TNEW
          Q(I,K,J)=MOIST(I,K,J,P_QV)/(1.+MOIST(I,K,J,P_QV)) !To s.h.
!         CWM(I,K,J)=0.
!         DO I_M=2,N_MOIST
!           IF(I_M/=P_QV)THEN
!             CWM(I,K,J)=CWM(I,K,J)+MOIST(I,K,J,I_M)
!           ENDIF
!         ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  UPDATE PRECIPITATION
!-----------------------------------------------------------------------
!
        PCPCOL=RAINNCV(I,J)*1.E-3
        PREC(I,J)=PREC(I,J)+PCPCOL
        ACPREC(I,J)=ACPREC(I,J)+PCPCOL
! NOTE: RAINNC IS ACCUMULATED INSIDE MICROPHYSICS BUT NMM ZEROES IT OUT ABOVE
!    SINCE IT IS ONLY A LOCAL ARRAY FOR NOW
!
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=JMS,JME
      DO K=KMS,KME
      DO I=IMS,IME
        ZERO_3D(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!-------------------------------------------------------------------
!
      END SUBROUTINE GSMDRIVE
!
!-------------------------------------------------------------------
!
      END MODULE MODULE_PHYSICS_CALLS
!
!-------------------------------------------------------------------
