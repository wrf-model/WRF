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
      USE MODULE_STATE_DESCRIPTION,ONLY : P_QV,P_QC,P_QR,P_QI,P_QS,P_QG,P_QNI,P_QNR
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_RA_GFDLETA,ONLY : CAL_MON_DAY,ZENITH
      USE MODULE_RADIATION_DRIVER
      USE MODULE_SF_MYJSFC
      USE MODULE_SURFACE_DRIVER
      USE MODULE_PBL_DRIVER
      USE MODULE_GWD
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
      SUBROUTINE QITEND_FER_HIRES_ADVECT(QI,QRIMEF,QITEND)
        IMPLICIT NONE
        REAL, INTENT(INOUT) :: QI, QRIMEF
        REAL, INTENT(IN) :: QITEND
        REAL :: F_RIMEF
        real, parameter :: max_f_rimef = 60.0, min_f_rimef=1.0

        ! For the advected Ferrier-Aligo, we have to handle the QRIMEF
        ! during tendency updates.

        ! Determine old rime factor from old QI and old QRIMEF:
        IF(QI<EPSQ) THEN
           F_RIMEF=1.
        ELSE
           F_RIMEF=max(min_f_rimef,min(max_f_rimef,QRIMEF/QI))
        ENDIF

        ! Decide new QI and new QRIMEF based on QC and RQIBLTEN:
        QI=QI+QITEND
        IF(QI<EPSQ) THEN
           QRIMEF=QI
        ELSE
           QRIMEF=QI*F_RIMEF
        ENDIF
      END SUBROUTINE QITEND_FER_HIRES_ADVECT

!***********************************************************************

      SUBROUTINE RADIATION(NTSD,DT,JULDAY,JULYR,XTIME,JULIAN            &
     &                    ,IHRST,NPHS,GLAT,GLON                         &
     &                    ,NRADS,NRADL                                  &
     &                    ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT   &
     &                    ,PD,RES,PINT,T,Q,MOIST,THS,ALBEDO,EPSR        &
     &                    ,F_ICE,F_RAIN                                 &
     &                    ,GD_CLOUD,GD_CLOUD2                           &
     &                    ,SM,HBM2,CLDFRA,N_MOIST,RESTRT                &
     &                    ,RLWTT,RSWTT,RLWIN,RSWIN,RSWINC,RSWOUT        &
     &                    ,RLWTOA,RSWTOA,CZMEAN                         &
     &                    ,CFRACL,CFRACM,CFRACH,SIGT4                   &
     &                    ,ACFRST,NCFRST,ACFRCV,NCFRCV                  &
     &                    ,CUPPT,VEGFRC,SNOW,HTOP,HBOT                  &
     &                    ,Z,SICE,NUM_AEROSOLC,NUM_OZMIXM,OZMIXM,PIN    &
     &                    ,LEVSIZ,GRID,CONFIG_FLAGS                     &
     &                    ,RTHRATEN                                     &
     &                    ,re_cloud,re_ice,re_snow                      & ! G. Thompson
     &                    ,has_reqc,has_reqi,has_reqs                   & ! G. Thompson
     &                    ,SWUPT,SWUPTC,SWDNT,SWDNTC                    &
     &                    ,SWUPB,SWUPBC,SWDNB,SWDNBC                    &
     &                    ,LWUPT,LWUPTC,LWDNT,LWDNTC                    &
     &                    ,LWUPB,LWUPBC,LWDNB,LWDNBC                    &
     &                    ,ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC            &
     &                    ,ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC            &
     &                    ,ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC            &
     &                    ,ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC            &
     &                    ,SWVISDIR ,SWVISDIF                           &  !ssib
     &                    ,SWNIRDIR, SWNIRDIF &                            !ssib
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
!   05-12-15  BLACK      - CONVERTED FROM IKJ TO IJK
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
     &                     ,NUM_AEROSOLC,NUM_OZMIXM,LEVSIZ
      REAL, INTENT(IN) :: OZMIXM(ims:ime,LEVSIZ,jms:jme,num_ozmixm), PIN(LEVSIZ)
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: NCFRCV,NCFRST
!
      REAL,INTENT(IN) :: DT,PDTOP,PT,XTIME,JULIAN
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: ALBEDO              &
     &                                             ,EPSR,GLAT,GLON      &
     &                                             ,HBM2                &
     &                                             ,PD,RES,SICE,SM      &
     &                                             ,SNOW,THS,VEGFRC
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: CUPPT

!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: Q,T,Z
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: F_ICE       &   !<--- Used only with physics (IKJ)
     &                                                     ,F_RAIN
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: RTHRATEN     !<--- Used only with physics (IKJ)
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,N_MOIST)                   &
                                                 ,INTENT(INOUT) :: MOIST
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACFRCV,ACFRST    &
     &                                                ,HBOT,HTOP        &
     &                                                ,RLWIN,RLWTOA     &
     &                                                ,RSWIN,RSWOUT     &
     &                                                ,RSWINC,RSWTOA
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: PINT     &
     &                                                        ,RLWTT    &
     &                                                        ,RSWTT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: CFRACH,CFRACL    &
     &                                                ,CFRACM,CZMEAN    &
     &                                                ,SIGT4

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME ),INTENT(IN) ::            &   !<--- Used only with physics (IKJ)
     &                              GD_CLOUD,GD_CLOUD2                  
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: CLDFRA

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::          &
                      ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC,          &
                      ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC,          &
                      ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC,          &
                      ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC

! TOA and surface, upward and downward, total and clear fluxes
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::          &
                        SWUPT,  SWUPTC,  SWDNT,  SWDNTC,          &
                        SWUPB,  SWUPBC,  SWDNB,  SWDNBC,          &
                        LWUPT,  LWUPTC,  LWDNT,  LWDNTC,          &
                        LWUPB,  LWUPBC,  LWDNB,  LWDNBC

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(OUT  )  ::                              SWVISDIR, &
                                                        SWVISDIF, &
                                                        SWNIRDIR, &
                                                        SWNIRDIF

!
!..Additions for coupling cloud physics effective radii and radiation.  G. Thompson
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT):: re_cloud, re_ice, re_snow
      INTEGER, INTENT(INOUT):: has_reqc, has_reqi, has_reqs
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
      INTEGER :: I,IENDX,II,ISTAT,J,JDAY,JMONTH,K,KMNTH,N,NRAD
!
      INTEGER,DIMENSION(3) :: IDAT
      INTEGER,DIMENSION(12) :: MONTH=(/31,28,31,30,31,30,31,31          &
     &                                ,30,31,30,31/)
!
      REAL :: CAPA,DAYI,DPL,FICE,FRAIN,GMT,HOUR,PLYR,PSFC               &
     &       ,QI,QR,QW,RADT,TIMES,WC,TDUM
!
      REAL,DIMENSION(KMS:KME-1) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CUPPTR,CZEN,HBOTR,HTOPR        &
     &                                  ,SWCF, LWCF                     &
     &                                  ,PDSL,REXNSFC,SWNETDN           &
     &                                  ,TOT,TOTLWDN,TOTSWDN,TOTSWDNC   &
     &                                  ,TSFC,XLAND,XLAT,XLON, HT
!
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: CLFR,DZ                &   !<--- Used only with physics (IKJ)
     &                                          ,P8W,P_PHY,PI_PHY       &
     &                                          ,RR,T8W                 &
     &                                          ,THRATENLW,THRATENSW    &
     &                                          ,TH_PHY,T_PHY,Z_PHY
!
      REAL,DIMENSION(:,:,:,:),ALLOCATABLE :: MOIST_TRANS
!
      LOGICAL :: WARM_RAIN
      LOGICAL :: IS_CAMMGMP_USED=.FALSE.

      REAL :: DXKM, DYKM

!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!*****
!***** NOTE: THIS IS HARDWIRED FOR CALLS TO LONGWAVE AND SHORTWAVE
!*****       AT EQUAL INTERVALS
!*****
!-----------------------------------------------------------------------
!
      NRAD=NRADS
      RADT=DT*NRADS/60.
!
!-----------------------------------------------------------------------
!
      ALLOCATE(MOIST_TRANS(IMS:IME,KMS:KME,JMS:JME,N_MOIST),STAT=ISTAT)
!
!-----------------------------------------------------------------------
!
      CAPA=R_D/CP

      DXKM=grid%dlmd*0.01745329*6371200. ! numbers from module_initialize_real.F
      DYKM=grid%dy_nmm
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
!
        PDSL(I,J)=PD(I,J)*RES(I,J)
        P8W(I,KTE+1,J)=PT
        XLAND(I,J)=SM(I,J)+1.
        PSFC=PD(I,J)+PDTOP+PT
        REXNSFC(I,J)=(PSFC*1.E-5)**CAPA
        TSFC(I,J)=THS(I,J)*REXNSFC(I,J)
        T8W(I,KTS,J)=TSFC(I,J)
        P8W(I,KTS,J)=ETA1(KTS)*PDTOP+ETA2(KTS)*PDSL(I,J)+PT
        Z_PHY(I,KTS,J)=Z(I,J,KTS)
        HT(I,J)=Z(I,J,KTS)
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  FILL THE SINGLE-COLUMN INPUT
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(dpl,i,j,k,plyr,ql,qr,tl)
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        DO K=KTS,KTE
          DPL=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
          QL(K)=MAX(Q(I,J,K),EPSQ)
          PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL(I,J)+PT
          TL(K)=T(I,J,K)
!
          RR(I,K,J)=PLYR/(R_D*TL(K)*(1.+P608*QL(K)))
          T_PHY(I,K,J)=TL(K)
          TH_PHY(I,K,J)=TL(K)*(1.E5/PLYR)**CAPA
          P8W(I,K+1,J)=ETA1(K+1)*PDTOP+ETA2(K+1)*PDSL(I,J)+PT
          P_PHY(I,K,J)=PLYR
          PI_PHY(I,K,J)=(PLYR*1.E-5)**CAPA
          DZ(I,K,J)=TL(K)*(P608*QL(K)+1.)*R_D                           &
     &                 *(P8W(I,K,J)-P8W(I,K+1,J))                       &
     &                 /(P_PHY(I,K,J)*G)
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
!        T8W(I,KTE+1,J)=-1.E20 
! For RRTM 
        T8W(I,KTE+1,J)=T8W(I,KTE,J) + 0.5*(T8W(I,KTE,J)-T8W(I,KTE-1,J))
!
      ENDDO
      ENDDO
!
      GMT=REAL(IHRST)
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
          CLDFRA(I,J,K)=0.
        ENDDO
        ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=JMS,JME
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
          HTOPR(I,J) =0.
          HBOTR(I,J) =0.
          SWCF(I,J) =0.
          LWCF(I,J) =0.
!
!-- NOTE:  HBOTR, HTOPR are passed into radiation and set equal to HBOT, HTOP.  HBOT, HTOP are
!          reset to clear sky values to be used by the ARW.  At the bottom of this subroutine,
!          HBOT, HTOP are re-defined again to values stored in HBOTR, HTOPR.  HBOT, HTOP are
!          reset to clear sky values after the call to radiation and after the top of the hour
!          in subroutine CUCNVC below.
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  TRANSPOSE THE MOIST ARRAY (IJK) FOR THE PHYSICS (IKJ).
!***  REMEMBER THAT MOIST AND MOIST_TRANS ARE ONLY USED WITH
!***  THE PHYSICS AND THUS THE P_QV SLOT (=2) IS MIXING RATIO,
!***  NOT SPECIFIC HUMIDITY.
!-----------------------------------------------------------------------
!
      DO N=1,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
          MOIST_TRANS(I,K,J,N)=MOIST(I,J,K,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!***  CALL THE INNER DRIVER.
!
!-----------------------------------------------------------------------
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
     &                 ,ICLOUD_CU=config_flags%ICLOUD_CU                &
     &                 ,QC_CU=GRID%QC_CU,QI_CU=GRID%QI_CU               &
     &                 ,DX=DXKM,DY=DYKM          &
     &                 ,DXKM=dxkm                                       &
! WRF-Solar variables
                       ,swint_opt=config_flags%swint_opt                &   
     &       ,SWDDIR=grid%swddir,SWDDNI=grid%swddni,SWDDIF=grid%swddif     & ! jararias
     &       ,SWDDIRC=grid%swddirc,SWDDNIC=grid%swddnic                    & ! clearsky sw diags
     &       ,Gx=grid%Gx,Bx=grid%Bx,gg=grid%gg,bb=grid%bb                  & ! for sza-interpolation
     &       ,swdown_ref=grid%swdown_ref,swddir_ref=grid%swddir_ref        & !
     &       ,coszen_ref=grid%coszen_ref                                   & 
     &       ,coszen=grid%coszen                                   & 
     &       ,hrang=grid%hrang                                   & 
     &       ,ht=ht                                   & 
     &       ,aer_type=config_flags%aer_type                                                        &
     &       ,aer_aod550_opt=config_flags%aer_aod550_opt,aer_aod550_val=config_flags%aer_aod550_val &
     &       ,aer_angexp_opt=config_flags%aer_angexp_opt,aer_angexp_val=config_flags%aer_angexp_val &
     &       ,aer_ssa_opt=config_flags%aer_ssa_opt,aer_ssa_val=config_flags%aer_ssa_val             &
     &       ,aer_asy_opt=config_flags%aer_asy_opt,aer_asy_val=config_flags%aer_asy_val             &
     &                 ,RTHRATENLW=THRATENLW,RTHRATENSW=THRATENSW       &
     &                 ,RTHRATEN=RTHRATEN                               &
     &                 ,CEN_LAT=grid%cen_lat                            &
     &                 ,GLW=TOTLWDN,GSW=SWNETDN,SWDOWN=TOTSWDN          &
     &                 ,XLAT=grid%HLAT,XLONG=grid%HLON,ALBEDO=ALBEDO,EMISS=EPSR   &
     &                 ,XICE=SICE,XLAND=XLAND,Z=Z,TSK=TSFC              &
     &                 ,N_AEROSOLC=NUM_AEROSOLC,PAERLEV=GRID%PAERLEV    &
     &                 ,ID=grid%id                                      &
     &                 ,CAM_ABS_DIM1=GRID%CAM_ABS_DIM1                  &
     &                 ,CAM_ABS_DIM2=GRID%CAM_ABS_DIM2                  &
     &                 ,CAM_ABS_FREQ_S=GRID%CAM_ABS_FREQ_S              &
     &                 ,ALEVSIZ=grid%alevsiz,no_src_types=grid%no_src_types   &
     &                 ,LEVSIZ=LEVSIZ,N_OZMIXM=NUM_OZMIXM               &
     &                 ,OZMIXM=OZMIXM,PIN=PIN                           &
     &                 ,HTOP=HTOP,HBOT=HBOT,CUPPT=CUPPTR                &
     &                 ,HTOPR=HTOPR,HBOTR=HBOTR                         &
     &                 ,VEGFRA=VEGFRC,SNOW=SNOW                         &
     &                 ,RHO=RR,P8W=P8W,P=P_PHY,PI=PI_PHY                &
     &                 ,DZ8W=DZ,T=T_PHY,T8W=T8W,GMT=GMT                 &
     &                 ,JULDAY=JULDAY,JULYR=JULYR,NPHS=NPHS             &
     &                 ,JULIAN=JULIAN,XTIME=XTIME                       &
     &                 ,YR=JULYR                                        &
     &                 ,LW_PHYSICS=CONFIG_FLAGS%RA_LW_PHYSICS           &
     &                 ,SW_PHYSICS=CONFIG_FLAGS%RA_SW_PHYSICS           &
     &                 ,RADT=RADT,RA_CALL_OFFSET=GRID%RA_CALL_OFFSET    &
     &                 ,STEPRA=NRAD,ICLOUD=config_flags%ICLOUD          &
     &                 ,WARM_RAIN=WARM_RAIN                             &
     &                 ,SWDOWNC=TOTSWDNC,CLDFRA=CLFR                    &
     &                 ,SWUPT=SWUPT                                     &
     &                 ,SWUPTC=SWUPTC                                   &
     &                 ,SWDNT=SWDNT                                     &
     &                 ,SWDNTC=SWDNTC                                   &
     &                 ,SWUPB=SWUPB                                     &
     &                 ,SWUPBC=SWUPBC                                   &
     &                 ,SWDNB=SWDNB                                     &
     &                 ,SWDNBC=SWDNBC                                   &
     &                 ,LWUPT=LWUPT                                     &
     &                 ,LWUPTC=LWUPTC                                   &
     &                 ,LWDNT=LWDNT                                     &
     &                 ,LWDNTC=LWDNTC                                   &
     &                 ,LWUPB=LWUPB                                     &
     &                 ,LWUPBC=LWUPBC                                   &
     &                 ,LWDNB=LWDNB                                     &
     &                 ,LWDNBC=LWDNBC                                   &
     &                 ,ACSWUPT=ACSWUPT                                 &
     &                 ,ACSWUPTC=ACSWUPTC                               &
     &                 ,ACSWDNT=ACSWDNT                                 &
     &                 ,ACSWDNTC=ACSWDNTC                               &
     &                 ,ACSWUPB=ACSWUPB                                 &
     &                 ,ACSWUPBC=ACSWUPBC                               &
     &                 ,ACSWDNB=ACSWDNB                                 &
     &                 ,ACSWDNBC=ACSWDNBC                               &
     &                 ,ACLWUPT=ACLWUPT                                 &
     &                 ,ACLWUPTC=ACLWUPTC                               &
     &                 ,ACLWDNT=ACLWDNT                                 &
     &                 ,ACLWDNTC=ACLWDNTC                               &
     &                 ,ACLWUPB=ACLWUPB                                 &
     &                 ,ACLWUPBC=ACLWUPBC                               &
     &                 ,ACLWDNB=ACLWDNB                                 &
     &                 ,ACLWDNBC=ACLWDNBC                               &
     &        ,SWVISDIR=swvisdir ,SWVISDIF=swvisdif                     &  !ssib
     &        ,SWNIRDIR=swnirdir ,SWNIRDIF=swnirdif                     &  !ssib
     &                 ,re_cloud=grid%re_cloud                          & ! G. Thompson
     &                 ,re_ice=grid%re_ice                              & ! G. Thompson
     &                 ,re_snow=grid%re_snow                            & ! G. Thompson
     &                 ,has_reqc=has_reqc                               & ! G. Thompson
     &                 ,has_reqi=has_reqi                               & ! G. Thompson
     &                 ,has_reqs=has_reqs                               & ! G. Thompson
     &                 ,RSWTOA=RSWTOA,RLWTOA=RLWTOA                     &
     &                 ,CZMEAN=CZMEAN,CFRACL=CFRACL                     &
     &                 ,CFRACM=CFRACM,CFRACH=CFRACH                     &
     &                 ,ACFRST=ACFRST,NCFRST=NCFRST                     &
     &                 ,ACFRCV=ACFRCV,NCFRCV=NCFRCV                     &
     &                 ,F_ICE_PHY=F_ICE,F_RAIN_PHY=F_RAIN               &
     &                 ,LWCF=LWCF,SWCF=SWCF                             &
     & ,O3INPUT=config_flags%O3INPUT,AER_OPT=config_flags%AER_OPT,O3RAD=grid%o3rad  &
     &                 ,SF_SURFACE_PHYSICS=CONFIG_FLAGS%SF_SURFACE_PHYSICS &
     &                 ,QV=MOIST_TRANS(IMS,KMS,JMS,P_QV),F_QV=F_QV      &
     &                 ,QC=MOIST_TRANS(IMS,KMS,JMS,P_QC),F_QC=F_QC      &
     &                 ,QR=MOIST_TRANS(IMS,KMS,JMS,P_QR),F_QR=F_QR      &
     &                 ,QI=MOIST_TRANS(IMS,KMS,JMS,P_QI),F_QI=F_QI      &
     &                 ,QS=MOIST_TRANS(IMS,KMS,JMS,P_QS),F_QS=F_QS      &
     &                 ,QG=MOIST_TRANS(IMS,KMS,JMS,P_QG),F_QG=F_QG      &
     &                 ,IS_CAMMGMP_USED=IS_CAMMGMP_USED                 &
     &                 ,EXPLICIT_CONVECTION=config_flags%cu_physics==0  &
     &                 ,AERCU_OPT=config_flags%aercu_opt                &
     &                 ,CU_PHYSICS=config_flags%cu_physics              &
     &                 ,MP_PHYSICS=CONFIG_FLAGS%MP_PHYSICS              &
     &                 ,CALC_CLEAN_ATM_DIAG=CONFIG_FLAGS%CALC_CLEAN_ATM_DIAG)


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
      nrads_block: IF(MOD(NTSD,NRADS)==0)THEN
!-----------------------------------------------------------------------
!
        IF(CONFIG_FLAGS%RA_SW_PHYSICS/=GFDLSWSCHEME)THEN
!
!-----------------------------------------------------------------------
!***  COMPUTE CZMEAN FOR NON-GFDL SHORTWAVE
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j)
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
     &                 ,MYIS,MYIE,MYJS,MYJE                             &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!$omp parallel do                                                       &
!$omp& private(i,j)
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
!$omp parallel do                                                       &
!$omp& private(i,j)
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
!
!--- No value currently available for clear-sky solar fluxes from
!    non GFDL schemes, though it's needed for air quality forecasts.
!    For the time being, set to the total downward solar fluxes.
!
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
!$omp& private(i,iendx,j)
        DO J=MYJS2,MYJE2
          IENDX=MYIE1
          IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
          DO I=MYIS1,IENDX
!
            RSWIN(I,J)=TOTSWDN(I,J)
            RSWINC(I,J)=TOTSWDNC(I,J)
            RSWOUT(I,J)=TOTSWDN(I,J)-SWNETDN(I,J)
!
          ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,k)
        DO J=MYJS2,MYJE2
          IENDX=MYIE1
          IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
          DO I=MYIS1,IENDX
            DO K=KTS,KTE
              RSWTT(I,J,K)=THRATENSW(I,K,J)*PI_PHY(I,K,J)
            ENDDO
!
          ENDDO
        ENDDO
!
      ENDIF nrads_block
!
!-----------------------------------------------------------------------
!***  LONGWAVE
!-----------------------------------------------------------------------
!
      nradl_block: IF(MOD(NTSD,NRADL)==0)THEN
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j)
        DO J=MYJS2,MYJE2
          IENDX=MYIE1
          IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
          DO I=MYIS1,IENDX
!
            IF(HBM2(I,J)>0.5)THEN
              TDUM=T(I,J,KTS)
              SIGT4(I,J)=STBOLT*TDUM*TDUM*TDUM*TDUM
              RLWIN(I,J)=TOTLWDN(I,J)
            ENDIF
!
          ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,k)
        DO J=MYJS2,MYJE2
          IENDX=MYIE1
          IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
!
          DO K=KTS,KTE
          DO I=MYIS1,IENDX
            IF(HBM2(I,J)>0.5)THEN
                RLWTT(I,J,K)=THRATENLW(I,K,J)*PI_PHY(I,K,J)
            ENDIF
          ENDDO
          ENDDO
!
        ENDDO
!
      ENDIF nradl_block
!
!-----------------------------------------------------------------------
!***  STORE 3D CLOUD FRACTIONS.
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,k)
      DO K=KTS,KTE
        DO J=MYJS2,MYJE2
          IENDX=MYIE1
          IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
          DO I=MYIS1,IENDX
            CLDFRA(I,J,K)=CLFR(I,K,J)
          ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  RESET THE DIAGNOSTIC CONVECTIVE CLOUD TOPS/BOTTOMS AFTER
!***  EACH RADIATION CALL.
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j)
!if (config_flags%ra_sw_physics/=hwrfswscheme.and.config_flags%ra_lw_physics/=hwrflwscheme)then
      DO J=MYJS2,MYJE2
        IENDX=MYIE1
        IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
        DO I=MYIS1,IENDX
          HBOT(I,J)=HBOTR(I,J)
          HTOP(I,J)=HTOPR(I,J)
          CUPPT(I,J)=CUPPTR(I,J)
        ENDDO
      ENDDO
!endif
!
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
!
!-----------------------------------------------------------------------
!***  UPDATE THE PROGNOSTIC MOIST ARRAY.
!-----------------------------------------------------------------------
!
      DO N=2,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO J=JMS,JME
        DO K=KMS,KME
        DO I=IMS,IME
          MOIST(I,J,K,N)=MOIST_TRANS(I,K,J,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
      DEALLOCATE(MOIST_TRANS,STAT=ISTAT)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE RADIATION
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      SUBROUTINE TURBL(NTSD,DT,NPHS,RESTRT                              &
     &                ,N_MOIST,N_SCALAR,NSOIL,SLDPTH,DZSOIL             &
     &                ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT       &
     &                ,SM,HBM2,VBM2,DX_ARRAY,DFRLG                      &
     &                ,CZEN,CZMEAN,SIGT4,RLWIN,RSWIN,RADOT              &
!- RLWIN/RSWIN - downward longwave/shortwave at the surface (also TOTLWDN/TOTSWDN in RADIATION)
     &                ,PD,RES,PINT,T,Q,CWM,F_ICE,F_RAIN,SR              &
     &                ,Q2,U,V,THS,TSFC,SST,PREC,SNO                     &
     &                ,SCURX,SCURY                                      &
     &                ,FIS,Z0,MZ0,Z0BASE,USTAR,MIXHT,PBLH,LPBL,EL_MYJ   &   !MZ0: MOMENTUM Z0 (KWON)
     &                ,MOIST,SCALAR,RMOL,MOL                            &
     &                ,EXCH_H,EXCH_M,F,AKHS,AKMS,AKHS_OUT,AKMS_OUT      &
     &                ,THZ0,QZ0,UZ0,VZ0,QS,MAVAIL                       &
     &                ,STC,SMC,CMC,SMSTAV,SMSTOT,SSROFF,BGROFF          &
     &                ,IVGTYP,ISLTYP,VEGFRC,SHDMIN,SHDMAX,GRNFLX        &
     &                ,SNOTIME                                          &
     &                ,SFCEXC,ACSNOW,ACSNOM,SNOPCX,SICE,TG,SOILTB       &
     &                ,ALBSI,ICEDEPTH,SNOWSI                            &
     &                ,ALBASE,MXSNAL,ALBEDO,SH2O,SI,EPSR,EMBCK          &
     &                ,U10,V10,UOCE,VOCE,TH10,Q10,TSHLTR,QSHLTR,PSHLTR  &
     &                ,T2,QSG,QVG,QCG,SOILT1,TSNAV,SMFR3D,KEEPFR3DFLAG  &
     &                ,TWBS,QWBS,TAUX,TAUY,SFCSHX,SFCLHX,SFCEVP,RTHRATEN&
     &                ,POTEVP,POTFLX,SUBSHX                             &
     &                ,APHTIM,ARDSW,ARDLW,ASRFC                         &
     &                ,RSWOUT,RSWTOA,RLWTOA                             &
     &                ,ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA          &
     &                ,UZ0H,VZ0H,DUDT,DVDT,UGWDsfc,VGWDsfc,SFENTH       & ! GWD
     &                ,RTHBLTEN,RQVBLTEN                                &
     &                ,PCPFLG,DDATA                                     & ! PRECIP ASSIM
     &                ,HSTDV,HCNVX,HASYW,HASYS,HASYSW,HASYNW,HLENW      & ! GWD
     &                ,HLENS,HLENSW,HLENNW,HANGL,HANIS,HSLOP,HZMAX      & ! GWD
     &                ,CROT,SROT                                        & ! GWD
     &                ,DEW                                              & ! RUC LSM
     &                ,RC_MF                                            & ! QNSE
     &                ,GRID,CONFIG_FLAGS                                &
     &                ,IHE,IHW,IVE,IVW                                  &
     &                ,DISHEAT,DKU3D,DKT3D                              &
     &                ,HPBL2D, EVAP2D, HEAT2D,RC2D                      &  !Kwon S&P
     &                ,SFCHEADRT,INFXSRT,SOLDRAIN                       &  !Hydrology, no-op right now
     &                ,cd_out,ch_out                                    &
     &                ,ulowl, vlowl                                     &
     &                ,zkmax, ribn                                      &
     &                ,charn, msang                                     &
     &                ,DUBLDT,DVBLDT,DTHBLDT,DQVBLDT                    & 
     &                ,hpbl_hold                                        & 
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,IPS,IPE,JPS,JPE,KPS,KPE                          &
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
!   05-12-15  BLACK      - CONVERTED FROM IKJ TO IJK
!   07-05-15  FERRIER    - ADDED GRAVITY WAVE DRAG (GWD) & MOUNTAIN BLOCKING
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
#if (NMM_CORE==1)    
      LOGICAL,INTENT(IN) ::  DISHEAT                       !  hwrf's doing
#endif
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,IPS,IPE,JPS,JPE,KPS,KPE                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,N_MOIST,NPHS,NSOIL,NTSD,N_SCALAR
!
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ISLTYP,IVGTYP
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) ::  HPBL2D, EVAP2D, HEAT2D , RC2D   !Kwon S&P
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: LPBL
!
      REAL,INTENT(IN) :: DT,PDTOP,PT
!
      REAL,INTENT(IN) :: SFENTH
      REAL,INTENT(INOUT) :: APHTIM,ARDSW,ARDLW,ASRFC
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: RTHRATEN     !Kwon S&P
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: DFRLG,ETA1,ETA2
!
      REAL,DIMENSION(NSOIL),INTENT(IN) :: DZSOIL,SLDPTH
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: CZEN,CZMEAN         &
     &                                             ,DX_ARRAY            &
     &                                             ,F,FIS,HBM2          &
     &                                             ,PD,RES              &
     &                                             ,RLWIN,RLWTOA        &
     &                                             ,RSWIN,RSWOUT,RSWTOA &
     &                                             ,SHDMIN,SHDMAX       &
!    &                                             ,SICE,SIGT4,SM,SR    & !Bandaid
     &                                             ,SIGT4               &
     &                ,HSTDV,HCNVX,HASYW,HASYS,HASYSW,HASYNW,HLENW      & ! GWD
     &                ,HLENS,HLENSW,HLENNW,HANGL,HANIS,HSLOP,HZMAX      & ! GWD
     &                ,CROT,SROT                                        & ! GWD
     &                                             ,VBM2,VEGFRC
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: SST
      REAL,DIMENSION(IMS:IME,JMS:JME)               :: ALBSI
      REAL,DIMENSION(IMS:IME,JMS:JME)               :: ICEDEPTH
      REAL,DIMENSION(IMS:IME,JMS:JME)               :: SNOWSI
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: SM,EPSR,SR       & !Bandaid
                                                      ,TG,SICE          &
                                                      ,EMBCK
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ALBASE,MXSNAL
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACSNOM,ACSNOW    &
     &                                                ,SNOTIME          &
     &                                                ,AKHS,AKMS        &
     &                                                ,ALBEDO           &
     &                                                ,BGROFF,CMC       &
     &                                                ,MAVAIL,MOL       &
     &                                                ,MIXHT            &
     &                                                ,PBLH,POTEVP      &
     &                                                ,POTFLX,PREC      &
     &                                                ,QCG,QS,QSG       &
     &                                                ,QVG,QZ0          &
     &                                                ,RMOL             &
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
     &                                                ,DEW              & !RUC LSM
     &                                                ,Z0,MZ0,Z0BASE      !MZ0 (KWON)
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: AKHS_OUT,AKMS_OUT  &
     &                                              ,ALWIN,ALWOUT       &
     &                                              ,ALWTOA,ASWIN       &
     &                                              ,ASWOUT,ASWTOA      &
     &                                              ,PSHLTR,Q10,QSHLTR  &
     &                                              ,TH10,TSHLTR        &
     &                                              ,U10,V10            & ! GWD
     &                                              ,UOCE,VOCE          &
     &                                              ,UGWDsfc,VGWDsfc      ! GWD
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: GRNFLX,QWBS,RADOT  &
                                                    ,SFCEXC,SMSTAV      &
                                                    ,SOILTB,TWBS
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN)  :: scurx, scury

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: cd_out,ch_out
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: &
                                                       DTHBLDT         &
                                                      ,DQVBLDT         &
                                                      ,DUBLDT          &
                                                      ,DVBLDT

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: taux, tauy
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: zkmax, ribn
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: charn, msang
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PINT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: CWM      &
     &                                                        ,DUDT     &
     &                                                        ,DVDT     &
     &                                                        ,Q,Q2     &
     &                                                        ,T,U,V
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: F_ICE    &   !<--- Used only in physics (IKJ)
     &                                                        ,F_RAIN   &
     &                                                        ,RQVBLTEN &
     &                                                        ,RTHBLTEN
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: EL_MYJ     &   !<--- Used only in physics (IKJ)
     &                                                      ,EXCH_H     &
     &                                                      ,EXCH_M
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(OUT) :: DKU3D,DKT3D   ! KWON
      REAL,DIMENSION(IMS:IME,NSOIL,JMS:JME),INTENT(INOUT) :: KEEPFR3DFLAG & !<--- Used only in physics (IKJ)
     &                                                      ,SH2O,SMC     &
     &                                                      ,SMFR3D,STC

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: RC_MF        ! QNSE only
!
      REAL,DIMENSION(IMS:IME,NSOIL,JMS:JME)               :: SMCREL
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,N_MOIST)                   &
     &                                           ,INTENT(INOUT) :: MOIST
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,N_SCALAR)                  &
     &                                           ,INTENT(INOUT) :: SCALAR
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

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT)  :: SFCHEADRT         ! Hydrology, no-op right now
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: INFXSRT,SOLDRAIN  ! Hydrology, no-op right now
!
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
!-----------------------------------------------------------------------
      INTEGER :: I,I_M,IDUMMY,IEND,ISFFLX,ISTAT,ISTR,J,K,KOUNT_ALL      &
     &          ,LENGTH_ROW,LLIJ,LLYR,N,SST_UPDATE,SF_URBAN_PHYSICS,NUM_URBAN_LAYERS
      INTEGER :: FASDAS
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME) :: KPBL,LOWLYR
!
      REAL :: TRESH=0.95
!
      REAL :: ALTITUDE,CWML,DQDT,DTDT,DTPHS,DX,DZHALF,FACTR,FACTRL      &
     &       ,G_INV,PLYR,PSFC,QI,QL,QOLD,QR,QW,RATIOMX,RDTPHS,DY        &
     &       ,ROG,RWMSK,SDEPTH,SNO_FACTR,TL,TLMH,TLMH4,TNEW,TSFC2       &
     &       ,U_FRAME,V_FRAME,XLVRW
!
      REAL :: APES,CAPA,CKLQ,EXNER,FACTOR,FFS,PQ0X,Q2SAT,QFC1,QLOWX     &
     &       ,RLIVWV,THBOT,DPL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: BR,CHKLOWQ,CT,CWMLOW,ELFLX     &
     &                                  ,EXNSFC,FACTRS,FLHC,FLQC,GZ1OZ0 &
     &                                  ,FH,FM,ZOL                      &
     &                                  ,ONE,PDSL,PLM,PSFC_OUT,PSIH     &
     &                                  ,PSIM,Q2X,QLOW,RAIN,RAINBL      &
     &                                  ,RLW_DN_SFC,RSW_NET_SFC         &
     &                                  ,RSW_DN_SFC                     &
     &                                  ,SFCEVPX,SFCZ,SNOW,SNOWC,SNOWH  &
     &                                  ,TH2X,THLOW,TLOW,VGFRCK         &
     &                                  ,RS,XLAIDYN                     &
     &                                  ,WSPD,XLAND,REGIME,HOL
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: DUDT_PHY,DVDT_PHY,DZ   &
     &                                          ,P_PHY,P8W,PI_PHY       &
     &                                          ,RQCBLTEN,RQIBLTEN      &
!BSF &                                     ,RQSBLTEN,RQRBLTEN,RQGBLTEN  &
     &                                          ,RR,DELP                & ! GWD
     &                                          ,T_PHY,TH_PHY,TKE       &
     &                                          ,DUDT_GWD,DVDT_GWD      & ! GWD
     &                                          ,U_PHY,V_PHY,Z
!
      REAL,DIMENSION(IMS:IME,JMS:JME), INTENT(OUT) :: ULOWL, VLOWL
!
      REAL,DIMENSION(:,:,:,:),ALLOCATABLE :: MOIST_TRANS
!
      REAL,DIMENSION(IMS:IME,NSOIL,JMS:JME) :: ZERO_SOIL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: wstar_ysu, delta_ysu
      REAL,DIMENSION(IMS:IME,JMS:JME) :: hpbl_hold
!
      LOGICAL :: E_BDY,WARM_RAIN
      LOGICAL :: IS_CAMMGMP_USED=.FALSE.
!
      INTEGER :: NUM_ROOF_LAYERS,NUM_WALL_LAYERS,NUM_ROAD_LAYERS   ! urban
      INTEGER :: FRACTIONAL_SEAICE
      INTEGER :: SEAICE_ALBEDO_OPT
      REAL    :: SEAICE_ALBEDO_DEFAULT
      INTEGER :: SEAICE_THICKNESS_OPT
      REAL    :: SEAICE_THICKNESS_DEFAULT
      INTEGER :: SEAICE_SNOWDEPTH_OPT
      REAL    :: SEAICE_SNOWDEPTH_MAX
      REAL    :: SEAICE_SNOWDEPTH_MIN
      INTEGER :: IFNDALBSI
      INTEGER :: IFNDICEDEPTH
      INTEGER :: IFNDSNOWSI
      REAL    :: WIND
      INTEGER :: IGS,IGE,JGS,JGE, PQ_I   !BSF
      LOGICAL :: FQ_I, ETAMP_PHYSICS,ETAMP_Regional            !BSF

      CHARACTER(len=255) :: message
#if HWRF==1
!dbg integer :: kpblmin,kpblmax,lpblmin,lpblmax    !dbg
#endif
!
!
    TYPE(WRFU_Time)                :: currentTime
    INTEGER                         :: yr, month, day, hr, minute, sec, rc
    CHARACTER*80                    :: mesg

    INTEGER  :: isurban
    CHARACTER(len=256) :: MMINLU
    REAL :: VAR_RIC,coef_ric_s,coef_ric_l                             !KWON for variable Ric (=1)
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      ALLOCATE(MOIST_TRANS(IMS:IME,KMS:KME,JMS:JME,N_MOIST),STAT=ISTAT)
!
      SF_URBAN_PHYSICS=CONFIG_FLAGS%SF_URBAN_PHYSICS
      FASDAS=0

      if ( config_flags%bl_pbl_physics == BOULACSCHEME ) then
         call wrf_error_fatal("Cannot use BOULAC PBL with NMM")
      endif
      
      FRACTIONAL_SEAICE = CONFIG_FLAGS%FRACTIONAL_SEAICE
      IF ( FRACTIONAL_SEAICE == 1 ) THEN
         CALL WRF_ERROR_FATAL("NMM cannot use FRACTIONAL_SEAICE = 1.")
      ENDIF

      SEAICE_ALBEDO_OPT = CONFIG_FLAGS%SEAICE_ALBEDO_OPT
      IF ( SEAICE_ALBEDO_OPT /= 0 ) THEN
         CALL WRF_ERROR_FATAL("NMM must use SEAICE_ALBEDO_OPT = 0")
      ENDIF

      IFNDALBSI = 0
      IFNDSNOWSI = 0
      IFNDICEDEPTH = 0
      BR=10

      SEAICE_ALBEDO_DEFAULT = CONFIG_FLAGS%SEAICE_ALBEDO_DEFAULT
      SEAICE_THICKNESS_OPT = CONFIG_FLAGS%SEAICE_THICKNESS_OPT
      SEAICE_THICKNESS_DEFAULT = CONFIG_FLAGS%SEAICE_THICKNESS_DEFAULT
      SEAICE_SNOWDEPTH_OPT = CONFIG_FLAGS%SEAICE_SNOWDEPTH_OPT
      SEAICE_SNOWDEPTH_MAX = CONFIG_FLAGS%SEAICE_SNOWDEPTH_MAX
      SEAICE_SNOWDEPTH_MIN = CONFIG_FLAGS%SEAICE_SNOWDEPTH_MIN
!
      DTPHS=NPHS*DT
      RDTPHS=1./DTPHS
      G_INV=1./G
      ROG=R_D*G_INV
      FACTOR=-XLV*RHOWATER/DTPHS
      CAPA=R_D/CP
!
      U_FRAME=0.
      V_FRAME=0.
!
      IDUMMY=0
      ISFFLX=1
      DX=0.
      DY=0.
      SST_UPDATE=config_flags%SST_UPDATE
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=JMS,JME
      DO I=IMS,IME
        UZ0H(I,J)=0.
        VZ0H(I,J)=0.
        ONE(I,J)=1.
        RMOL(I,J)=0.     !Reciprocal of Monin-Obukhov length
        SFCEVPX(I,J)=0.  !Dummy for accumulated latent energy, not flux
        ZOL(I,J)=0.
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
        EXCH_M(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!***  PREPARE NEEDED ARRAYS FOR CALLING THE INNER DRIVER.
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(factrl,i,j,llij,tlmh)
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
!
        PDSL(I,J)=PD(I,J)*RES(I,J)
!!!     PSFC=PD(I,J)+PDTOP+PT
!!!     P8W(I,KTS,J)=PSFC
        P8W(I,KTS,J)=PINT(I,J,KTS)
        PSFC=PINT(I,J,KTS)
        LOWLYR(I,J)=KTS     !<----  The lowest model layer counted from the bottom.
        EXNSFC(I,J)=(1.E5/PSFC)**CAPA
        THS(I,J)=(SST(I,J)*EXNSFC(I,J))*SM(I,J)+THS(I,J)*(1.-SM(I,J))
        TSFC(I,J)=THS(I,J)/EXNSFC(I,J)
        SFCZ(I,J)=FIS(I,J)*G_INV
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
        PLM(I,J)=(PINT(I,J,LLIJ)+PINT(I,J,LLIJ+1))*0.5
        TH2X(I,J)=T(I,J,LLIJ)*(1.E5/PLM(I,J))**CAPA
        Q2X(I,J)=Q(I,J,LLIJ)
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
          TLMH=T(I,J,LLIJ)
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
        Z(I,KTS,J)=SFCZ(I,J)
!
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  FILL THE ARRAYS FOR CALLING THE INNER DRIVER.
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(cwml,i,j,k,plyr,qi,ql,qr,qw,tl)
      DO J=MYJS,MYJE
        DO K=KTS,KTE
        DO I=MYIS,MYIE
          Q2(I,J,K)=MAX(Q2(I,J,K)*HBM2(I,J),EPSQ2)
          QL=MAX(Q(I,J,K),EPSQ)
          PLYR=(PINT(I,J,K)+PINT(I,J,K+1))*0.5
!!!       PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL(I,J)+PT
          TL=T(I,J,K)
          CWML=CWM(I,J,K)
!
          RR(I,K,J)=PLYR/(R_D*TL)
          T_PHY(I,K,J)=TL
!
          EXNER=(1.E5/PLYR)**CAPA
          PI_PHY(I,K,J)=1./EXNER
          TH_PHY(I,K,J)=TL*EXNER
          P8W(I,K+1,J)=PINT(I,J,K+1)
!!!       P8W(I,K+1,J)=ETA1(K+1)*PDTOP+ETA2(K+1)*PDSL(I,J)+PT
          P_PHY(I,K,J)=PLYR
          TKE(I,K,J)=0.5*Q2(I,J,K)
!
          RTHBLTEN(I,K,J)=0.
          RQVBLTEN(I,K,J)=0.
          RQCBLTEN(I,K,J)=0.
          RQIBLTEN(I,K,J)=0.
!BSF          RQSBLTEN(I,K,J)=0.
!BSF          RQRBLTEN(I,K,J)=0.
!BSF          RQGBLTEN(I,K,J)=0.
!
!-- Next 3 lines modified for GWD
!
          DPL=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
          Z(I,K+1,J)=Z(I,K,J)+TL/PLYR*DPL*ROG*(Q(I,J,K)*P608-CWML+1.)
          DELP(I,K,J)=DPL
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
        QLOW(I,J)=MAX(Q(I,J,LLYR),EPSQ)
        QLOWX=QLOW(I,J)/(1.-QLOW(I,J))
        QLOW(I,J)=QLOWX/(1.+QLOWX)
        CWMLOW(I,J)=CWM(I,J,LLYR)
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
!$omp& private(i,j,k)
      DO K=KTS,KTE
        DO J=MYJS1_P1,MYJE1_P1
          DO I=MYIS_P1,MYIE_P1
            U_PHY(I,K,J)=(U(I+IHE(J),J,K)+U(I+IHW(J),J,K)               &
     &                   +U(I,J+1,K)+U(I,J-1,K))                        &
     &                   *0.25
            V_PHY(I,K,J)=(V(I+IHE(J),J,K)+V(I+IHW(J),J,K)               &
     &                   +V(I,J+1,K)+V(I,J-1,K))                        &
     &                   *0.25
          IF ( K == KTS ) THEN
             ULOWL(I,J) = U_PHY(I,K,J)
             VLOWL(I,J) = V_PHY(I,K,J)
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
!
!-----------------------------------------------------------------------
!***  SET MAVAIL EQUAL TO 1. ONLY FOR NMM LSM
!-----------------------------------------------------------------------
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        IF(MODEL_CONFIG_REC%SF_SURFACE_PHYSICS(GRID%ID)==2.OR.          &
           MODEL_CONFIG_REC%SF_SURFACE_PHYSICS(GRID%ID)==99)THEN
          ONE(I,J)=1.
        ELSE
!***  MAVAIL should not be equal to 1. for other LSMs
          ONE(I,J)=MAVAIL(I,J)
        ENDIF
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  TRANSPOSE THE MOIST ARRAY (IJK) FOR THE PHYSICS (IKJ).
!-----------------------------------------------------------------------
!
      DO N=1,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
          MOIST_TRANS(I,K,J,N)=MOIST(I,J,K,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  URBAN RELATED VARIABLES ARE ADDED TO ARGUMENTS OF SURFACE_DRIVER
!-----------------------------------------------------------------------
!
      NUM_ROOF_LAYERS=GRID%NUM_SOIL_LAYERS   !urban
      NUM_WALL_LAYERS=GRID%NUM_SOIL_LAYERS   !urban
      NUM_ROAD_LAYERS=GRID%NUM_SOIL_LAYERS   !urban
      CALL nl_get_isurban(grid%id, isurban)
      call nl_get_mminlu(grid%id, mminlu)

     CALL domain_clock_get( grid, current_time=currentTime, &
                            current_timestr=mesg )
     CALL WRFU_TimeGet( currentTime, YY=yr, dayOfYear=day, H=hr, M=minute, S=sec, rc=rc)
         IF( rc/= WRFU_SUCCESS)THEN
         CALL wrf_error_fatal('WRFU_TimeGet failed')
         ENDIF


!
!-----------------------------------------------------------------------
!
!***  CALL SURFACE LAYER AND LAND SURFACE PHYSICS
!
!-----------------------------------------------------------------------
!
      CALL SET_TILES(GRID,IDS,IDE-1,JDS+1,JDE-1,ITS,ITE,JTS,JTE)

!
      CALL SURFACE_DRIVER(                                              &
     &           HYDRO_dt=-1.0,  SFCHEADRT=SFCHEADRT,                   &
     &           INFXSRT=INFXSRT,  SOLDRAIN=SOLDRAIN,                   &
     &           JULIAN_IN=grid%julian,                                 &
     &           ACSNOM=ACSNOM,ACSNOW=ACSNOW,AKHS=AKHS,AKMS=AKMS        &
     &          ,ALBEDO=ALBEDO,BR=BR,CANWAT=CMC,CHKLOWQ=CHKLOWQ         &
     &          ,DT=DT,DX=DX,DZ8W=DZ,DZS=DZSOIL,GLW=RLW_DN_SFC          &
     &          ,GRDFLX=GRNFLX,GSW=RSW_NET_SFC,SWDOWN=RSW_DN_SFC        &
     &          ,GZ1OZ0=GZ1OZ0,HFX=TWBS                                 &
     &          ,HT=SFCZ,IFSNOW=IDUMMY,ISFFLX=ISFFLX                    &
     &          ,FRACTIONAL_SEAICE=FRACTIONAL_SEAICE                    &
     &          ,SEAICE_ALBEDO_OPT=SEAICE_ALBEDO_OPT                    &
     &          ,SEAICE_ALBEDO_DEFAULT=SEAICE_ALBEDO_DEFAULT            &
     &          ,SEAICE_THICKNESS_OPT=SEAICE_THICKNESS_OPT              &
     &          ,SEAICE_THICKNESS_DEFAULT=SEAICE_THICKNESS_DEFAULT      &
     &          ,SEAICE_SNOWDEPTH_OPT=SEAICE_SNOWDEPTH_OPT              &
     &          ,SEAICE_SNOWDEPTH_MAX=SEAICE_SNOWDEPTH_MAX              &
     &          ,SEAICE_SNOWDEPTH_MIN=SEAICE_SNOWDEPTH_MIN              &
     &          ,TICE2TSK_IF2COLD=CONFIG_FLAGS%TICE2TSK_IF2COLD         &
     &          ,IFNDALBSI=IFNDALBSI,IFNDICEDEPTH=IFNDICEDEPTH          &
     &          ,IFNDSNOWSI=IFNDSNOWSI                                  &
     &          ,ISLTYP=ISLTYP                                          &
     &          ,ITIMESTEP=NTSD,IVGTYP=IVGTYP,LOWLYR=LOWLYR             &
     &          ,MAVAIL=ONE,RMOL=RMOL,MOL=MOL                           &
     &          ,NUM_SOIL_LAYERS=NSOIL,P8W=P8W                          &
     &          ,PBLH=PBLH,PI_PHY=PI_PHY,PSHLTR=PSHLTR,PSIH=PSIH        &
     &          ,PSIM=PSIM,P_PHY=P_PHY,Q10=Q10,Q2=Q2X,QFX=QWBS,TAUX=TAUX,TAUY=TAUY,QSFC=QS  &
     &          ,QSHLTR=QSHLTR,QZ0=QZ0                                  &
     &          ,ICOEF_SF=CONFIG_FLAGS%ICOEF_SF                         &
     &          ,LCURR_SF=CONFIG_FLAGS%LCURR_SF                         &!for gfdl-sf drag
     &          ,ZKMAX=ZKMAX,RIBN=RIBN                                  &
     &          ,CHARN=CHARN,MSANG=MSANG,SCURX=SCURX,SCURY=SCURY        &
     &          ,IWAVECPL=CONFIG_FLAGS%IWAVECPL                         &
#if (HWRF==1)
     &          ,pert_Cd=config_flags%pert_Cd                           &
     &          ,ens_random_seed=config_flags%ens_random_seed           &
     &          ,ens_Cdamp=config_flags%ens_Cdamp                       &
#endif
     &          ,cd_out=grid%cd_out,ch_out=grid%ch_out                  &
     &          ,RAINCV=RAIN                                            &
     &          ,RHO=RR,SFCEVP=SFCEVPX,SFCEXC=SFCEXC,SFCRUNOFF=SSROFF   &
     &          ,SMOIS=SMC,SMSTAV=SMSTAV,SMSTOT=SMSTOT,SNOALB=MXSNAL    &
     &          ,SNOW=SNOW,SNOWC=SNOWC,SNOWH=SNOWH,STEPBL=NPHS          &
     &          ,SMCREL=SMCREL                                          &
     &          ,SST=SST,SST_UPDATE=SST_UPDATE,MAX_EDOM=-1              &
     &          ,TH10=TH10,TH2=TH2X,T2=T2,THZ0=THZ0,TH_PHY=TH_PHY       &
     &          ,TMN=TG,TSHLTR=TSHLTR,TSK=TSFC,TSLB=STC,T_PHY=T_PHY     &
     &          ,U10=U10,UDRUNOFF=BGROFF,UST=USTAR,UZ0=UZ0H             &
     &          ,U_FRAME=U_FRAME,U_PHY=U_PHY,V10=V10,VEGFRA=VGFRCK      &
     &          ,UOCE=UOCE,VOCE=VOCE                                    &
     &          ,VZ0=VZ0H,V_FRAME=V_FRAME,V_PHY=V_PHY                   &
     &          ,WARM_RAIN=WARM_RAIN,WSPD=WSPD,XICE=SICE,XICEM=SICE     &
     &          ,ALBSI=albsi,ICEDEPTH=icedepth,SNOWSI=snowsi            &
     &          ,ISICE=GRID%LANDUSE_ISICE,ISWATER=GRID%ISWATER          &
     &          ,XLAND=XLAND,Z=Z,ZNT=Z0                                 &
#if ( HWRF == 1 )
     &          ,MZNT=MZ0                                               &
#endif
     &          ,ZS=SLDPTH,CT=CT,TKE_PBL=TKE,SFENTH=SFENTH              &   !KWON
     &          ,ALBBCK=ALBASE,LH=ELFLX,SH2O=SH2O,SHDMAX=SHDMAX         &
     &          ,SHDMIN=SHDMIN,Z0=Z0BASE,FLQC=FLQC,FLHC=FLHC            &
     &          ,FM=FM,FHH=FH,ZOL=ZOL                                   &
     &          ,PSFC=PSFC_OUT,EMISS=EPSR,EMBCK=EMBCK                   &
     &          ,SF_SFCLAY_PHYSICS=CONFIG_FLAGS%SF_SFCLAY_PHYSICS       &
     &          ,SF_SURFACE_PHYSICS=CONFIG_FLAGS%SF_SURFACE_PHYSICS     &
     &          ,RA_LW_PHYSICS=CONFIG_FLAGS%RA_LW_PHYSICS               &
     &          ,RA_SW_PHYSICS=CONFIG_FLAGS%RA_SW_PHYSICS               &
     &          ,LAI=GRID%LAI,IZ0TLND=CONFIG_FLAGS%IZ0TLND              &
     &          ,SF_URBAN_PHYSICS=SF_URBAN_PHYSICS                      &
!     &          ,GMT=GMT,XLAT=XLAT,XLONG=XLONG,JULDAY=JULDAY            &
     &          ,NUM_URBAN_LAYERS=NUM_URBAN_LAYERS                      &
     &          ,NUM_URBAN_HI=config_flags%num_urban_hi                 & !multi-layer urban
     &          ,IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE        &
     &          ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME        &
     &          ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe      & 
     &          ,I_START=GRID%I_START,I_END=GRID%I_END                  &
     &          ,J_START=GRID%J_START,J_END=GRID%J_END                  &
     &          ,KTS=KTS,KTE=KTE,NUM_TILES=GRID%NUM_TILES               &
           ! Optional args
     &          ,QV_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QV),F_QV=F_QV        &
     &          ,QC_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QC),F_QC=F_QC        &
     &          ,QR_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QR),F_QR=F_QR        &
     &          ,QI_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QI),F_QI=F_QI        &
     &          ,QS_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QS),F_QS=F_QS        &
     &          ,QG_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QG),F_QG=F_QG        &
     &          ,RAINBL=RAINBL                                          &
     &          ,LAGDAY=1                                               & ! tmn_update
! for RUCLSM
     &          ,QSG=QSG,QVG=QVG,QCG=QCG,SOILT1=SOILT1                  &
     &          ,TSNAV=TSNAV,SMFR3D=SMFR3D,KEEPFR3DFLAG=KEEPFR3DFLAG    &
     &          ,POTEVP=POTEVP,SNOPCX=SNOPCX,SOILTB=SOILTB,SR=SR        &
     &          ,DEW=DEW,ACRUNOFF=grid%ACRUNOFF                         &
     &          ,MOSAIC_LU=CONFIG_FLAGS%MOSAIC_LU                       &
     &          ,MOSAIC_SOIL=CONFIG_FLAGS%MOSAIC_SOIL                   &
     &          ,LANDUSEF=grid%landusef                                 &
     &          ,SOILCTOP=grid%soilctop                                 &
     &          ,rhosnf=grid%rhosnf,precipfr=grid%precipfr              &
     &          ,snowfallac=grid%snowfallac                             &
! for URBAN
     &          ,NUM_ROOF_LAYERS=NUM_ROOF_LAYERS                        & ! urban
     &          ,NUM_WALL_LAYERS=NUM_WALL_LAYERS                        & ! urban
     &          ,NUM_ROAD_LAYERS=NUM_ROAD_LAYERS                        & ! urban
! for YSU
     &          ,REGIME=REGIME                                          &
! for PX LSM
     &          ,NLCAT=grid%num_land_cat,  NSCAT=grid%num_soil_cat      & ! P-X LSM
     &        ,ISURBAN=isurban, MMINLU=TRIM(mminlu)                       &
     &        ,SNOTIME = grid%SNOTIME &
     &        ,RDLAI2D=config_flags%rdlai2d                                   &
     &        ,usemonalb=config_flags%usemonalb                           &
     &        ,NOAHRES=grid%noahres                                       &
     &        ,opt_thcnd=config_flags%opt_thcnd       &
! for Noah UA changes
     &        ,ua_phys=config_flags%ua_phys,flx4=grid%flx4,fvb=grid%fvb       &
     &        ,fbur=grid%fbur,fgsn=grid%fgsn                                  &
! vertical dimensions for ocean model, not used for NMM
     &        ,okms = 1, okme=2                                          &
! for NOAH-MP LSM
     &        ,YR=yr                                       &
     &        ,idveg=config_flags%dveg,         iopt_crs=config_flags%opt_crs &
     &        ,iopt_btr=config_flags%opt_btr,   iopt_run=config_flags%opt_run &
     &        ,iopt_sfc=config_flags%opt_sfc,   iopt_frz=config_flags%opt_frz &
     &        ,iopt_inf=config_flags%opt_inf,   iopt_rad=config_flags%opt_rad &
     &        ,iopt_alb=config_flags%opt_alb,   iopt_snf=config_flags%opt_snf &
     &        ,iopt_tbot=config_flags%opt_tbot, iopt_stc=config_flags%opt_stc & 
     &        ,iopt_gla=config_flags%opt_gla,   iopt_rsf=config_flags%opt_rsf & 
     &        ,iopt_soil=config_flags%opt_soil, iopt_pedo=config_flags%opt_pedo &
     &        ,iopt_crop=config_flags%opt_crop
     &        , isnowxy=grid%isnowxy  ,    tvxy=grid%tvxy    ,    tgxy=grid%tgxy     &
     &        ,canicexy=grid%canicexy ,canliqxy=grid%canliqxy,   eahxy=grid%eahxy    &
     &        ,   tahxy=grid%tahxy    ,    cmxy=grid%cmxy    ,    chxy=grid%chxy     &
     &        ,  fwetxy=grid%fwetxy   ,sneqvoxy=grid%sneqvoxy,alboldxy=grid%alboldxy &
     &        , qsnowxy=grid%qsnowxy  ,wslakexy=grid%wslakexy,   zwtxy=grid%zwtxy    &
     &        ,    waxy=grid%waxy     ,    wtxy=grid%wtxy    ,  tsnoxy=grid%tsnoxy   &
     &        , zsnsoxy=grid%zsnsoxy  , snicexy=grid%snicexy , snliqxy=grid%snliqxy  &
     &        ,lfmassxy=grid%lfmassxy ,rtmassxy=grid%rtmassxy,stmassxy=grid%stmassxy &
     &        ,  woodxy=grid%woodxy   ,stblcpxy=grid%stblcpxy,fastcpxy=grid%fastcpxy &
     &        , grainxy=grid%grainxy  ,   gddxy=grid%gddxy   ,   pgsxy=grid%pgsxy    &
     &        , cropcat=grid%cropcat                                                 &
     &        ,planting=grid%planting , harvest=grid%harvest ,season_gdd=grid%season_gdd  &
     &        , soilcomp=grid%soilcomp                                               &
     &        , soilcl1=grid%soilcl1  , soilcl2=grid%soilcl2                         &
     &        , soilcl3=grid%soilcl3  , soilcl4=grid%soilcl4                         &
     &        ,  xsaixy=grid%xsaixy   , taussxy=grid%taussxy                         &
     &        ,  t2mvxy=grid%t2mvxy   ,  t2mbxy=grid%t2mbxy                          &
     &        ,  q2mvxy=grid%q2mvxy   ,  q2mbxy=grid%q2mbxy                          & 
     &        ,  tradxy=grid%tradxy   ,   neexy=grid%neexy   ,   gppxy=grid%gppxy    &
     &        ,   nppxy=grid%nppxy    ,  fvegxy=grid%fvegxy  , runsfxy=grid%runsfxy  &
     &        , runsbxy=grid%runsbxy  ,  ecanxy=grid%ecanxy  ,  edirxy=grid%edirxy   &
     &        , etranxy=grid%etranxy  ,   fsaxy=grid%fsaxy   ,  firaxy=grid%firaxy   &
     &        ,  aparxy=grid%aparxy   ,   psnxy=grid%psnxy   ,   savxy=grid%savxy    &
     &        ,   sagxy=grid%sagxy    , rssunxy=grid%rssunxy , rsshaxy=grid%rsshaxy  &
     &        ,  bgapxy=grid%bgapxy   ,  wgapxy=grid%wgapxy  ,   tgvxy=grid%tgvxy    &
     &        ,   tgbxy=grid%tgbxy    ,   chvxy=grid%chvxy   ,   chbxy=grid%chbxy    &
     &        ,   shgxy=grid%shgxy    ,   shcxy=grid%shcxy   ,   shbxy=grid%shbxy    &
     &        ,   evgxy=grid%evgxy    ,   evbxy=grid%evbxy   ,   ghvxy=grid%ghvxy    &
     &        ,   ghbxy=grid%ghbxy    ,   irgxy=grid%irgxy   ,   ircxy=grid%ircxy    &
     &        ,   irbxy=grid%irbxy    ,    trxy=grid%trxy    ,   evcxy=grid%evcxy    &
     &        ,chleafxy=grid%chleafxy ,  chucxy=grid%chucxy                          &                       
     &        ,  chv2xy=grid%chv2xy   ,  chb2xy=grid%chb2xy , chstarxy=grid%chstarxy &                       
     &        ,  smoiseq=grid%smoiseq, smcwtdxy=grid%smcwtdxy, rechxy=grid%rechxy    &                       
     &        ,  deeprechxy=grid%deeprechxy                                          &                       
     &        ,gecros_state=grid%gecros_state                                        &   ! Optional gecros crop
     &        ,coszen=grid%czen,xlat_urb2d=grid%hlat                                 &
     &        ,rs=rs,xlaidyn=xlaidyn                                                 &
! mosaic tiling for Noah
     &        ,sf_surface_mosaic=config_flags%sf_surface_mosaic,  mosaic_cat=config_flags%mosaic_cat      &
! lake module
     &        ,lakedepth2d=grid%lakedepth2d, savedtke12d=grid%savedtke12d, snowdp2d=grid%snowdp2d, h2osno2d=grid%h2osno2d    &
     &        ,snl2d=grid%snl2d, t_grnd2d=grid%t_grnd2d, t_lake3d=grid%t_lake3d,   lake_icefrac3d=grid%lake_icefrac3d        &
     &        ,z_lake3d=grid%z_lake3d, dz_lake3d=grid%dz_lake3d, t_soisno3d=grid%t_soisno3d, h2osoi_ice3d=grid%h2osoi_ice3d  &
     &        ,h2osoi_liq3d=grid%h2osoi_liq3d, h2osoi_vol3d=grid%h2osoi_vol3d, z3d=grid%z3d, dz3d=grid%dz3d                  &
     &        ,zi3d=grid%zi3d, watsat3d=grid%watsat3d, csol3d=grid%csol3d, tkmg3d=grid%tkmg3d                                &
     &        ,tkdry3d=grid%tkdry3d,tksatu3d=grid%tksatu3d, LakeModel=grid%sf_lake_physics,  lake_min_elev=grid%lake_min_elev  &
! end lake module
     &        ,maxpatch=1,inest=1,history_interval=config_flags%history_interval  & !clm
     &        ,fasdas=fasdas
     &                                                          )
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
        DUDT_PHY(I,K,J)=0.
        DVDT_PHY(I,K,J)=0.
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
!-- ETAMP_Regional logical is true for regional NAM (ETAMPNEW) or HRW (ETAMPNEW) microphysics
!
      ETAMP_Regional=.FALSE.
      IF (CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW .OR.                        &
     &    CONFIG_FLAGS%MP_PHYSICS==FER_MP_HIRES) ETAMP_Regional=.TRUE.
!
      IF(ETAMP_Regional) THEN
!-- Logical FQ_I and index PQ_I are set to values associated with snow
!   for the regional (NAM, HWRF) versions of the microphysics
         FQ_I=F_QS
         PQ_I=P_QS
      ELSE
         FQ_I=F_QI
         PQ_I=P_QI
      ENDIF
#if HWRF==1
      CALL nl_get_var_ric(1, var_ric)
      CALL nl_get_coef_ric_s(1, coef_ric_s)
      CALL nl_get_coef_ric_l(1, coef_ric_l)
!      write(0,*) 'var_ric & coef_ric_s l from namelist Kwon  ',var_ric,coef_ric_s,coef_ric_l
#endif
!
      CALL PBL_DRIVER(                                                &
     &                ITIMESTEP=NTSD,DT=DT                            &
     &               ,U_FRAME=U_FRAME,V_FRAME=V_FRAME                 &
     &               ,RUBLTEN=DUDT_PHY,RVBLTEN=DVDT_PHY               &
     &               ,RTHBLTEN=RTHBLTEN,RQVBLTEN=RQVBLTEN             & !BSF
     &               ,RQCBLTEN=RQCBLTEN,RQIBLTEN=RQIBLTEN             & !BSF
!BSF &               ,RQRBLTEN=RQRBLTEN,RQSBLTEN=RQSBLTEN             & !BSF
!BSF &               ,RQGBLTEN=RQGBLTEN                               & !BSF
     &               ,TSK=TSFC,XLAND=XLAND,ZNT=Z0                     &
#if ( HWRF == 1 )
     &               ,MSANG=MSANG,SCURX=SCURX,SCURY=SCURY             &
     &               ,IWAVECPL=CONFIG_FLAGS%IWAVECPL                  &
     &               ,LCURR_SF=CONFIG_FLAGS%LCURR_SF                  &

     &               ,MZNT=MZ0                                        &
#endif
     &               ,HPBL_HOLD=HPBL_HOLD                             &   !for new KSAS
     &               ,HT=SFCZ                                         &   !KWON
     &               ,UST=USTAR,MIXHT=MIXHT,PBLH=PBLH                 &
     &               ,HFX=TWBS,QFX=QWBS,GRDFLX=GRNFLX                 &
     &               ,U_PHY=U_PHY,V_PHY=V_PHY,TH_PHY=TH_PHY,RHO=RR    &
     &               ,P_PHY=P_PHY,PI_PHY=PI_PHY,P8W=P8W,T_PHY=T_PHY   &
     &               ,DZ8W=DZ,Z=Z,TKE_PBL=TKE,EL_PBL=EL_MYJ,F=F       &
     &               ,EXCH_H=EXCH_H,EXCH_M=EXCH_M,AKHS=AKHS,AKMS=AKMS &
     &               ,FM=FM,FHH=FH                                    &
     &               ,YSU_TOPDOWN_PBLMIX=CONFIG_FLAGS%YSU_TOPDOWN_PBLMIX &
     &               ,SHINHONG_TKE_DIAG=CONFIG_FLAGS%SHINHONG_TKE_DIAG &
     &               ,THZ0=THZ0,QZ0=QZ0,UZ0=UZ0H,VZ0=VZ0H             &
     &               ,QSFC=QS,LOWLYR=LOWLYR                           &
     &               ,PSIM=PSIM,PSIH=PSIH,GZ1OZ0=GZ1OZ0               &
     &               ,U10=U10,V10=V10,UOCE=UOCE,VOCE=VOCE,T2=T2,WSPD=WSPD,BR=BR,CHKLOWQ=CHKLOWQ &   
     &               ,DX=DX,DY=DY,STEPBL=NPHS,WARM_RAIN=WARM_RAIN     &
     &               ,KPBL=KPBL,CT=CT,LH=ELFLX,SNOW=SNOW,XICE=SICE    &
     &               ,BL_PBL_PHYSICS=config_flags%bl_pbl_physics      &
     &               ,RA_LW_PHYSICS=config_flags%ra_lw_physics        &
     &               ,MFSHCONV=config_flags%mfshconv                  &  ! work with QNSE PBL
     &               ,rc_mf=rc_mf                                     &  ! QNSE
     &               ,IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE &
     &               ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME &
     &               ,I_START=GRID%I_START,I_END=GRID%I_END           &
     &               ,J_START=GRID%J_START,J_END=GRID%J_END           &
     &               ,KTS=KTS,KTE=KTE,NUM_TILES=GRID%NUM_TILES        &
     &               ,CTOPO=grid%ctopo,CTOPO2=grid%ctopo2             &
     &               ,WINDFARM_OPT=config_flags%windfarm_opt          &
     &               ,POWER=grid%POWER                                &
     &               ,NUM_SCALAR=1, NUM_TRACER=1                      &  ! parameters not used by NMM
#if (NMM_CORE==1)
     &               ,DISHEAT=DISHEAT                                 &
#endif
                ! Optional args
     &               ,RTHRATEN=RTHRATEN                               &
#if (HWRF==1)
     &               ,GFS_ALPHA=GRID%GFS_ALPHA                        &
#endif
     &               ,HPBL2D=HPBL2D, EVAP2D=EVAP2D, HEAT2D=HEAT2D     &    !Kwon S&P
     &               ,RC2D=RC2D                                       &    !KWON
#if HWRF==1
     &               ,VAR_RIC=VAR_RIC                                 &    !Kwon Ric
#endif
     &               ,DKU3D=DKU3D,DKT3D=DKT3D 
#if HWRF==1
     &               ,coef_ric_l=coef_ric_l,coef_ric_s=coef_ric_s     &    !Kwon for Ric
     &          ,pert_pbl=config_flags%pert_pbl                       &
     &          ,ens_random_seed=config_flags%ens_random_seed         &
     &          ,ens_pblamp=config_flags%ens_pblamp                   &
#endif
     &               ,QV_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QV),F_QV=F_QV &
     &               ,QC_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QC),F_QC=F_QC &
     &               ,QI_CURR=MOIST_TRANS(IMS,KMS,JMS,PQ_I),F_QI=FQ_I &
     &               ,QR_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QR),F_QR=F_QR &
     &               ,QS_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QS),F_QS=F_QS &
     &               ,QG_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QG),F_QG=F_QG &
     &               ,HOL=HOL,sf_sfclay_physics=CONFIG_FLAGS%SF_SFCLAY_PHYSICS &
     &               ,IS_CAMMGMP_USED=IS_CAMMGMP_USED                 &
     &               ,wstar=wstar_ysu,delta=delta_ysu                 &
     &               ,fasdas=fasdas                                   &
     &               ,sf_urban_physics=CONFIG_FLAGS%SF_URBAN_PHYSICS)
!
!output PBL tendency
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=JMS,JME
      DO K=KMS,KME
      DO I=IMS,IME
        DUBLDT(I,J,K)=DUDT_PHY(I,K,J)
        DVBLDT(I,J,K)=DVDT_PHY(I,K,J)
        DTHBLDT(I,J,K)=RTHBLTEN(I,K,J)
        DQVBLDT(I,J,K)=RQVBLTEN(I,K,J)
      ENDDO
      ENDDO
      ENDDO
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

      IF(MYJS==JDS)THEN
        DO J=JDS,JDS+1
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
#if ( HWRF == 1 )
      if(size(grid%windsq_swath)>1) then
         do j=max(jts,jds),min(jte,jde-1)
            do i=max(its,ids),min(ite,ide-1)
               if(grid%interesting(i,j)==0) cycle
               wind=u10(i,j)**2 + v10(i,j)**2
               if(wind>grid%windsq_swath(i,j)) grid%windsq_swath(i,j)=wind
            enddo
         enddo
      endif
#endif

      IF(CONFIG_FLAGS%SF_SFCLAY_PHYSICS==1)THEN ! non-NMM package
!$omp parallel do                                                       &
!$omp& private(i,j)
        DO J=MYJS1,MYJE1
        DO I=MYIS,MYIE1
!         TSHLTR(I,J)=TSHLTR(I,J)*(1.E5/PSHLTR(I,J))**RCP
          IF(TSHLTR(I,J)<200..OR.TSHLTR(I,J)>350.)THEN
            WRITE(message,*)'Troublesome TSHLTR...I,J,TSHLTR,PSHLTR: '        &
                      ,I,J,TSHLTR(I,J),PSHLTR(I,J)
            CALL wrf_message(trim(message))
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
!=======================================================================
!===  Begin gravity wave drag (GWD) and mountain blocking (MB)  ========
!=======================================================================
!
      IGS=MYIS1
      IGE=MYIE1
      JGS=MYJS2
      JGE=MYJE2
!dbg !dbg
!dbg kpblmin=100
!dbg kpblmax=-100
!dbg DO J=JGS,JGE
!dbg   DO I=IGS,IGE
!dbg     kpblmin=min(kpblmin,kpbl(i,j))
!dbg     kpblmax=max(kpblmax,kpbl(i,j))
!dbg   enddo
!dbg enddo
!dbg print *,'TURBL: IGS,IGE,JGS,JGE,kpblmin,kpblmax=',IGS,IGE,JGS,JGE,kpblmin,kpblmax
!dbg

!      print *,'grid%id  gwd_opt at module_PHYSICS ',grid%id, grid%gwd_opt
!      WRITE(0,*)'grid%id  gwd_opt at module_PHYSICS ',grid%id, grid%gwd_opt
!      WRITE(MESSAGE,125)grid%gwd_opt
!125   FORMAT(1X,'grid%id  module_PHYSICS.F :  gwd_opt  ',I2,2X,I2)


#if ( HWRF == 1 )
      IF (grid%gwd_opt .eq. 2 .AND. grid%id.eq.1) THEN                                        !Kwon's doing for parent only now
#else
      IF (grid%gwd_opt .eq. 2) THEN
#endif
!      print *,'==grid%id  gwd_opt at module_PHYSICS inside gwd ',grid%id, grid%gwd_opt        !because there is no data for nest domain

        CALL wrf_message("GWD usage currently may be problematic for some cases - use at own risk")

      CALL GWD_driver(U=U_PHY,V=V_PHY,T=T_PHY                           &
     &               ,Q=MOIST_TRANS(IMS,KMS,JMS,P_QV)                   &
     &               ,Z=Z,DP=DELP,PINT=P8W,PMID=P_PHY,EXNR=PI_PHY       &
     &               ,KPBL=KPBL,ITIME=NTSD                              &
     &               ,HSTDV=HSTDV,HCNVX=HCNVX,HASYW=HASYW,HASYS=HASYS   &
     &               ,HASYSW=HASYSW,HASYNW=HASYNW,HLENW=HLENW           &
     &               ,HLENS=HLENS,HLENSW=HLENSW,HLENNW=HLENNW           &
     &               ,HANGL=HANGL,HANIS=HANIS,HSLOP=HSLOP,HZMAX=HZMAX   &
     &               ,CROT=CROT,SROT=SROT                               &
     &               ,DUDT=DUDT_GWD,DVDT=DVDT_GWD                       &
     &               ,UGWDsfc=UGWDsfc,VGWDsfc=VGWDsfc,XLAND=XLAND       &    !ADDED BY KWON FOR OCEAN
     &               ,IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE   &
     &               ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME   &
     &               ,ITS=IGS,ITE=IGE,JTS=JGS,JTE=JGE,KTS=KTS,KTE=KTE )

!=======================================================================
!=====  End gravity wave drag (GWD) and mountain blocking (MB)  ========
!=======================================================================
!
!-----------------------------------------------------------------------
!***  TRANSFER THE WIND TENDENCIES.
!-----------------------------------------------------------------------
!
      DO K=KTS,KTE
      DO J=JTS,JTE
      DO I=ITS,ITE


!mp     temporary bandaid limiting GWD/MB wind tendencies

        IF (DUDT_GWD(I,K,J) .gt. 1.6) then
        write(message,*) 'BIG DUDT_GWD:: ', I,K,J, DUDT_GWD(I,K,J)
        CALL wrf_message(message)
        DUDT_GWD(I,K,J)=1.6
        ENDIF

        IF (DUDT_GWD(I,K,J) .lt. -1.6) then
        write(message,*) 'BIG DUDT_GWD:: ', I,K,J, DUDT_GWD(I,K,J)
        CALL wrf_message(message)
        DUDT_GWD(I,K,J)=-1.6
        ENDIF

        IF (DVDT_GWD(I,K,J) .gt. 1.6) then
        write(message,*) 'BIG DVDT_GWD:: ', I,K,J, DVDT_GWD(I,K,J)
        CALL wrf_message(message)
        DVDT_GWD(I,K,J)=1.6
        ENDIF

        IF (DVDT_GWD(I,K,J) .lt. -1.6) then
        write(message,*) 'BIG DVDT_GWD:: ', I,K,J, DVDT_GWD(I,K,J)
        CALL wrf_message(message)
        DVDT_GWD(I,K,J)=-1.6
        ENDIF

!mp     end temporary bandaid

        DUDT(I,J,K)=DUDT_PHY(I,K,J)+DUDT_GWD(I,K,J)
        DVDT(I,J,K)=DVDT_PHY(I,K,J)+DVDT_GWD(I,K,J)

      ENDDO
      ENDDO
      ENDDO

      ELSE  ! no GWD

      DO K=KTS,KTE
      DO J=JTS,JTE
      DO I=ITS,ITE
        DUDT(I,J,K)=DUDT_PHY(I,K,J)
        DVDT(I,J,K)=DVDT_PHY(I,K,J)
      ENDDO
      ENDDO
      ENDDO

      ENDIF ! gwd_opt
!
!-----------------------------------------------------------------------
!***  TRANSPOSE THE MOIST_TRANS ARRAY BACK TO THE PROGNOSTIC MOIST ARRAY.
!-----------------------------------------------------------------------
!
      DO N=1,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO J=JMS,JME
        DO K=KMS,KME
        DO I=IMS,IME
          MOIST(I,J,K,N)=MOIST_TRANS(I,K,J,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
      DEALLOCATE(MOIST_TRANS,STAT=ISTAT)
!
!-----------------------------------------------------------------------
!***  UPDATE TEMPERATURE, SPECIFIC HUMIDITY, CLOUD, AND TKE.
!-----------------------------------------------------------------------
!
      E_BDY=(ITE>=IDE)
!
      ETAMP_PHYSICS=ETAMP_Regional
      IF (CONFIG_FLAGS%MP_PHYSICS==ETAMP_HWRF) ETAMP_PHYSICS=.TRUE.
!
!$omp parallel do                                                       &
!$omp& private(dqdt,dtdt,i,iend,j,k,qi,qold,qr,qw,ratiomx,i_m)
      DO K=KTS,KTE
      DO J=MYJS2,MYJE2
        IEND=MYIE1
        IF(E_BDY.AND.MOD(J,2)==0)IEND=IEND-1
!
        DO I=MYIS1,IEND
          DTDT=RTHBLTEN(I,K,J)*PI_PHY(I,K,J)
          DQDT=RQVBLTEN(I,K,J)         !Mixing ratio tendency
          T(I,J,K)=T(I,J,K)+DTDT*DTPHS
          QOLD=Q(I,J,K)
          RATIOMX=QOLD/(1.-QOLD)+DQDT*DTPHS
          Q(I,J,K)=RATIOMX/(1.+RATIOMX)
!         Q(I,J,K)=MAX(Q(I,J,K),EPSQ)
          MOIST(I,J,K,P_QV)=MAX(EPSQ,(MOIST(I,J,K,P_QV)+RQVBLTEN(I,K,J)*DTPHS) )
          CWM(I,J,K)=0.
!
          pbl_check1:  IF(config_flags%mp_physics==fer_mp_hires_advect) then
             ! Update QI and QRIMEF:
             call QITEND_FER_HIRES_ADVECT( &
                  MOIST(I,J,K,P_QI), &
                  SCALAR(I,J,K,P_QRIMEF), &
                  RQIBLTEN(I,K,J)*DTPHS)

             ! Update QC:
             MOIST(I,J,K,P_QC)=MAX(EPSQ,(MOIST(I,J,K,P_QC)+RQCBLTEN(I,K,J)*DTPHS))
          ELSEIF (.NOT.ETAMP_PHYSICS) THEN

            DO I_M=1,N_MOIST
              IF(I_M==P_QC) THEN
                MOIST(I,J,K,I_M)=MAX(EPSQ,(MOIST(I,J,K,I_M)+RQCBLTEN(I,K,J)*DTPHS) )
              ELSE IF(I_M==P_QI) THEN
                MOIST(I,J,K,I_M)=MAX(EPSQ,(MOIST(I,J,K,I_M)+RQIBLTEN(I,K,J)*DTPHS) )
              ENDIF
              IF(I_M/=P_QV) CWM(I,J,K)=CWM(I,J,K)+MOIST(I,J,K,I_M)
            ENDDO
          ELSE pbl_check1
!
!-- Allow vertical mixing to modify cloud ice + snow for ETAMPNEW
!
            QW=MAX(0.,MOIST(I,J,K,P_QC)+RQCBLTEN(I,K,J)*DTPHS )
            pbl_check2:  IF (ETAMP_Regional) THEN
              QI=MAX(0.,MOIST(I,J,K,P_QS)+RQIBLTEN(I,K,J)*DTPHS )  !-- Total ice
!BSF              QR=MAX(0.,MOIST(I,J,K,P_QR)+RQRBLTEN(I,K,J)*DTPHS )
              QR=MAX(0.,MOIST(I,J,K,P_QR) )
              MOIST(I,J,K,P_QC)=QW
              MOIST(I,J,K,P_QS)=QI
              MOIST(I,J,K,P_QR)=QR
            ELSE IF (CONFIG_FLAGS%MP_PHYSICS==ETAMP_HWRF) THEN    !-- pbl_check2
              QI=MAX(0.,MOIST(I,J,K,P_QI)+RQIBLTEN(I,K,J)*DTPHS )  !-- Total ice
!BSF              QR=MAX(0.,MOIST(I,J,K,P_QR)+RQRBLTEN(I,K,J)*DTPHS )
              QR=MAX(0.,MOIST(I,J,K,P_QR) )
              MOIST(I,J,K,P_QC)=QW
              MOIST(I,J,K,P_QI)=QI
              MOIST(I,J,K,P_QR)=QR
            ENDIF pbl_check2
            CWM(I,J,K)=QW+QI+QR
!
            IF(QI<=EPSQ)THEN
              F_ICE(I,K,J)=0.
            ELSE
              F_ICE(I,K,J)=MAX(0.,MIN(1.,QI/CWM(I,J,K)))
            ENDIF
!
            IF(QR<=EPSQ)THEN
              F_RAIN(I,K,J)=0.
            ELSE
              F_RAIN(I,K,J)=QR/(QW+QR)
            ENDIF
!
          ENDIF pbl_check1
!
          Q2(I,J,K)=2.*TKE(I,K,J)
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
     &                    ,DUDT,DVDT,U,V,HBM2,IVE,IVW                   &
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
!   05-12-12  BLACK      - CONVERTED FROM IKJ TO IJK
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
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: HBM2,UZ0H,VZ0H
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: DUDT,DVDT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: UZ0,VZ0
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: U,V
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
      DO K=KTS,KTE
        DO J=MYJS2,MYJE2
          IEND=MYIE1
          IF(E_BDY.AND.MOD(J,2)==1)IEND=IEND-1
!
          DO I=MYIS1,IEND
            U(I,J,K)=(DUDT(I+IVE(J),J,K)+DUDT(I+IVW(J),J,K)             &
     &               +DUDT(I,J+1,K)+DUDT(I,J-1,K))*0.25*DTPHS           &
     &               +U(I,J,K)
            V(I,J,K)=(DVDT(I+IVE(J),J,K)+DVDT(I+IVW(J),J,K)             &
     &               +DVDT(I,J+1,K)+DVDT(I,J-1,K))*0.25*DTPHS           &
     &               +V(I,J,K)
          ENDDO
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
      END SUBROUTINE UV_H_TO_V
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      SUBROUTINE CUCNVC(NTSD,DT,NCNVC,NRADS,NRADL                       &
     &                 ,GPS,RESTRT,HYDRO                                &
     &                 ,CLDEFI,N_MOIST,N_SCALAR,ENSDIM                  &
     &                 ,MOIST,SCALAR                                    &
     &                 ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2               &
     &                 ,F_ICE,F_RAIN                                    &
!***  Changes for other cu-schemes, most for gd scheme
     &                 ,APR_GR,APR_W,APR_MC,TTEN,QTEN                   &
     &                 ,APR_ST,APR_AS,APR_CAPMA                         &
     &                 ,APR_CAPME          ,APR_CAPMI                   &
     &                 ,MASS_FLUX         ,XF_ENS                       &
     &                 ,PR_ENS,GSW                                      &
     &                 ,GD_CLOUD,GD_CLOUD2,KTOP_DEEP                    &
     &                 ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TCUCN              &
     &                 ,OMGALF,U,V,W,Z,FIS,W0AVG                        &
     &                 ,PREC,ACPREC,CUPREC,CUPPT,CPRATE                 &
     &                 ,SM,HBM2,PBLH,LPBL,CNVBOT,CNVTOP                 &
     &                 ,HTOP,HBOT,HTOPD,HBOTD,HTOPS,HBOTS               &
     &                 ,RTHBLTEN,RQVBLTEN,RTHRATEN                      &
#if (NMM_CORE==1)    
     &                 ,TWBS,QWBS                                       &
     &                 ,DUCUDT, DVCUDT, MOMMIX, store_rand              &                  
     &                 ,DTHCUDT,DQVCUDT,DQRCUDT,DQCCUDT,DQICUDT         &! added output CU tendency
     &                 ,DQSCUDT                                         &! added output CU tendency

#endif
     &                 ,HPBL2D,EVAP2D,HEAT2D                            &     !Kwon S&P
     &                 ,DX2D,DYNMM                                      & ! Scale-aware sas
     &                 ,SCALEFUN, SCALEFUN1                             & !scale functions
     &                 ,SIGMU, SIGMU1                                   & !updraft fraction
     &                 ,AVCNVC,ACUTIM,IHE,IHW                           &
     &                 ,GRID,CONFIG_FLAGS                               &
     &                 ,HPBL_HOLD                                       & ! for new KSAS
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,IPS,IPE,JPS,JPE,KPS,KPE                         &
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
!   05-12-15  BLACK      - CONVERTED FROM IKJ TO IJK
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
     &                     ,IPS,IPE,JPS,JPE,KPS,KPE                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,N_MOIST,NCNVC,NTSD,NRADS,NRADL,N_SCALAR
!
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LPBL
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DX2D ! scale-sware sas
      REAL,INTENT(IN) :: DYNMM                           ! Delt_Y 
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: SCALEFUN,SCALEFUN1 !  scale-sware sas
                                                                     ! scale functions
       REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: SIGMU,SIGMU1 ! updraft fractional area

!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: HPBL2D,EVAP2D,HEAT2D     !Kwon S&P

      REAL,DIMENSION(IMS:IME,JMS:JME)               :: SHALL
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
     &                                                ,PREC,CPRATE      &
     &                 ,APR_GR,APR_W,APR_MC                             &
     &                 ,APR_ST,APR_AS,APR_CAPMA                         &
     &                 ,APR_CAPME,APR_CAPMI                             &
     &                 ,GSW,MASS_FLUX
#if (NMM_CORE==1)
     REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: store_rand 
#endif
!Biswas
       REAL,DIMENSION(IMS:IME,JMS:JME) :: HFX,QFX,PBLH
       REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: TWBS,QWBS
       REAL,DIMENSION(KMS:KME) :: ZNU
       REAL, DIMENSION(IMS:IME, KMS:KME, JMS:JME) ::                    &
                                        RUCUTEN,                        &
                                        RVCUTEN,RQVFTEN

       REAL,DIMENSION(IMS:IME,JMS:JME) :: HPBL_HOLD
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: F_ICE       &
     &                                                     ,F_RAIN
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: QTEN     &
     &                                                        ,RQVBLTEN &
     &                                                        ,RTHBLTEN &
     &                                                        ,RTHRATEN &
     &                                                        ,TTEN
!
#if (NMM_CORE==1)
      REAL, INTENT(INOUT)::MOMMIX
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT):: DUCUDT, DVCUDT 
! 2015-12-14 added output tendency from CU
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT):: DTHCUDT   &
     &                                                      , DQVCUDT   &
     &                                                      , DQRCUDT   &
     &                                                      , DQCCUDT   &
     &                                                      , DQICUDT   &
     &                                                      , DQSCUDT
!
      REAL,DIMENSION(IDS:IDE,JDS:JDE)               :: DATA1 
      LOGICAL, EXTERNAL::wrf_dm_on_monitor
#endif
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: CWM      &
     &                                                        ,OMGALF   &
     &                                                        ,Q,T      &
     &                                                        ,TCUCN    &
     &                                                        ,U,V      &
     &                                                        ,W,Z
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PINT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: W0AVG
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:ENSDIM),INTENT(INOUT) :: PR_ENS  &
     &                                                         ,XF_ENS
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,N_MOIST)                   &
     &                                           ,INTENT(INOUT) :: MOIST
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,N_SCALAR)                  &
     &                                           ,INTENT(INOUT) :: SCALAR
!

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: GD_CLOUD &
     &                                                        ,GD_CLOUD2
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: KTOP_DEEP
!
      LOGICAL,INTENT(IN) :: HYDRO,RESTRT
      LOGICAL :: IS_CAMMGMP_USED=.FALSE.
!
      TYPE(DOMAIN),TARGET :: GRID
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER :: I,ICLDCK,IENDX,ISTAT,J,K,MNTO,N,N_TIMSTPS_OUTPUT       &
     &          ,NCUBOT,NCUTOP,NSTEP_CNV
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME) :: KPBL,LBOT,LOWLYR,LTOP
!
      REAL :: CAPA,CF_HI,DPL,DQDT,DTCNVC,DTDT,FICE,FRAIN,G_INV          &
     &       ,PCPCOL,PLYR,QI,QL_K,QR,QW,RDTCNVC,TL_K,WC,WMID,PLYRB

!
      REAL,DIMENSION(KMS:KME-1) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CUBOT,CUTOP,NCA,PDSL           &
     &                                  ,RAINC,SFCZ,XLAND
      REAL,DIMENSION(IMS:IME,JMS:JME) :: RAINCV
!
      REAL,DIMENSION(ITS:ITE,JTS:JTE) :: WMID_L
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: DZ,P8W,P_PHY,PI_PHY    &
     &                                          ,RQCCUTEN,RQRCUTEN      &
     &                                          ,RQICUTEN,RQSCUTEN      &
     &                                          ,RQVCUTEN,RR,RTHCUTEN   &
     &                                          ,T_PHY,TH_PHY           &
     &                                          ,U_PHY,V_PHY,WINT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,ENSDIM) :: ZERO_GD
!
      REAL,DIMENSION(:,:,:,:),ALLOCATABLE :: MOIST_TRANS
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: CLDFRA_DP, CLDFRA_SH
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: QC_CU, QI_CU
!
      LOGICAL :: RESTART,WARM_RAIN,ETAMP_Regional, have_tg_tp, have_swath
      LOGICAL,DIMENSION(IMS:IME,JMS:JME) :: CU_ACT_FLAG
!
      CHARACTER(LEN=255) :: message
!
!-----------------------------------------------------------------------
!***  FOR TEMPERATURE CHANGE CHECK ONLY.
!-----------------------------------------------------------------------
      INTEGER :: DTEMP_CHECK=2.0
      REAL :: TCHANGE
!  random number restart
      INTEGER, SAVE  :: nfirst
      data nfirst /0/
#if (NMM_CORE==1)
      INTEGER                    :: IDT                
      REAL,   DIMENSION(2)       :: RND1 
#endif
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  RESET THE HBOT/HTOP CONVECTIVE CLOUD BOTTOM (BASE) AND TOP ARRAYS
!***  USED IN RADIATION.  THEY STORE THE MAXIMUM VERTICAL LIMITS OF
!***  CONVECTIVE CLOUD BETWEEN RADIATION CALLS.  CUPPT IS THE ACCUMULATED
!***  CONVECTIVE PRECIPITATION BETWEEN RADIATION CALLS.
!-----------------------------------------------------------------------
!
! Biswas's doing
       DO J=MYJS,MYJE
       DO I=MYIS,MYIE
!       DO J=JMS,JME
!       DO I=IMS,IME
         HFX(I,J)=0.
         QFX(I,J)=0.
       ENDDO
       ENDDO
!End

      IF(MOD(NTSD,NRADS)==0.OR.MOD(NTSD,NRADL)==0)THEN
         DO J=JMS,JME
         DO I=IMS,IME
           HTOP(I,J)=0.
           HBOT(I,J)=REAL(KTE+1)
           CUTOP(I,J)=0.
           CUBOT(I,J)=REAL(KTE+1)
           CUPPT(I,J)=0.
         ENDDO
         ENDDO
      ENDIF
!-----------------------------------------------------------------------
      IF(MOD(NTSD,NCNVC)/=0.AND.                                      &
     &   CONFIG_FLAGS%CU_PHYSICS==BMJSCHEME)RETURN
      IF(MOD(NTSD,NCNVC)/=0.AND.                                      &
     &   CONFIG_FLAGS%CU_PHYSICS==KSASSCHEME)RETURN
      IF(MOD(NTSD,NCNVC)/=0.AND.                                      &
     &   CONFIG_FLAGS%CU_PHYSICS==NSASSCHEME)RETURN
      IF(MOD(NTSD,NCNVC)/=0.AND.                                      &
     &   CONFIG_FLAGS%CU_PHYSICS==TIEDTKESCHEME)RETURN
      IF(MOD(NTSD,NCNVC)/=0.AND.                                      &
     &   CONFIG_FLAGS%CU_PHYSICS==OSASSCHEME)RETURN
      IF(MOD(NTSD,NCNVC)/=0.AND.                                      &
     &   CONFIG_FLAGS%CU_PHYSICS==SASSCHEME)RETURN
      IF(MOD(NTSD,NCNVC)/=0.AND.                                      &
     &   CONFIG_FLAGS%CU_PHYSICS==SCALESASSCHEME)RETURN 
      
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
!$omp& private(I,J)
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
!
        PDSL(I,J)=PD(I,J)*RES(I,J)
        RAINCV(I,J)=0.
        RAINC(I,J)=0.
        P8W(I,KTS,J)=PD(I,J)+PDTOP+PT
        LOWLYR(I,J)=KTS        !<----  The lowest model layer counted from the bottom.
        XLAND(I,J)=SM(I,J)+1.
        NCA(I,J)=0.
        SFCZ(I,J)=FIS(I,J)*G_INV
!
!Biswas's doing
         HFX(I,J)=-TWBS(I,J)
         QFX(I,J)=-QWBS(I,J)/XLV
!End

        CUTOP(I,J)=HTOP(I,J)
        CUBOT(I,J)=HBOT(I,J)
!
!***  LPBL IS THE MODEL LAYER CONTAINING THE PBL TOP
!***  COUNTING DOWNWARD FROM THE TOP OF THE DOMAIN
!***  SO KPBL IS THE SAME LAYER COUNTING UPWARD FROM
!***  THE GROUND.
!
        KPBL(I,J)=KTE-LPBL(I,J)+1
      ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(dpl,fice,frain,i,j,k,plyr,qi,ql,qr,qw,wc)
      DO J=MYJS2,MYJE2
        DO K=KTS,KTE
        DO I=MYIS1,MYIE1
          DPL=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
          QL(K)=MAX(Q(I,J,K),EPSQ)
          PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL(I,J)+PT
          PLYRB=AETA1(K)*PDTOP+AETA2(K)*(1.E5-PDTOP)+PT
          ZNU(K)=(PLYRB-PT)/1.E5
          TL(K)=T(I,J,K)
!
          RR(I,K,J)=PLYR/(R_D*TL(K)*(P608*QL(K)+1.))
          T_PHY(I,K,J)=TL(K)

          TH_PHY(I,K,J)=TL(K)*(1.E5/PLYR)**CAPA
!!!       P8W(I,KFLIP,J)=PINT(I,J,K+1)
          P8W(I,K+1,J)=ETA1(K+1)*PDTOP+ETA2(K+1)*PDSL(I,J)+PT
          P_PHY(I,K,J)=PLYR
          PI_PHY(I,K,J)=(PLYR*1.E-5)**CAPA
!
        ENDDO
!
      ENDDO
      ENDDO

!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=JTS,JTE
        DO K=KTS,KTE
        DO I=ITS,ITE
          RTHCUTEN(I,K,J)=0.
          RQVCUTEN(I,K,J)=0.
          RQCCUTEN(I,K,J)=0.
          RQRCUTEN(I,K,J)=0.
          RQICUTEN(I,K,J)=0.
          RQSCUTEN(I,K,J)=0.
        ENDDO
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!

      IF(.NOT.HYDRO)THEN
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KTS,KTE
        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
          DZ(I,K,J)=Z(I,J,K+1)-Z(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        IF(NTSD==0)THEN
!$omp parallel do                                                       &
!$omp& private(i,j,k)
          DO J=MYJS2,MYJE2
          DO K=KTS,KTE+1   ! zero for all interfaces
          DO I=MYIS1,MYIE1
            WINT(I,K,J)=0.
          ENDDO
          ENDDO
          ENDDO

	ELSE  ! not NTSD=0

         DO J=MYJS2,MYJE2
           DO I=MYIS1,MYIE1
             WINT(I,KTS,J)=0.
             WINT(I,KTE+1,J)=0.
           ENDDO
         ENDDO

         DO J=MYJS2,MYJE2
          DO K=KTS+1,KTE
           DO I=MYIS1,MYIE1
             WINT(I,K,J)=0.5*(W(I,J,K)+W(I,J,K-1))
           ENDDO
          ENDDO
         ENDDO

        ENDIF
	
      ELSE   ! hydrostatic

        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
          WINT(I,KTS,J)=0.
          WINT(I,KTE+1,J)=0.
        ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k,plyr)
        DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
            WMID_L(I,J)=-OMGALF(I,J,KTS)*CP/(G*DT)
            PDSL(I,J)=PD(I,J)*RES(I,J)
            PLYR=AETA1(KTS)*PDTOP+AETA2(KTS)*PDSL(I,J)+PT
            DZ(I,KTS,J)=T(I,J,KTS)*(P608*Q(I,J,KTS)+1.)*R_D             &
     &                 *(P8W(I,KTS,J)-P8W(I,KTS+1,J))                   &
     &                 /(PLYR*G)
          ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k,ql_k,tl_k,wmid)
        DO J=MYJS2,MYJE2
          DO K=KTS+1,KTE
          DO I=MYIS1,MYIE1
            TL_K=T_PHY(I,K,J)
            QL_K=MAX(Q(I,J,K),EPSQ)
            WMID=-OMGALF(I,J,K)*CP/(G*DT)
            WINT(I,K,J)=0.5*(WMID_L(I,J)+WMID)
            WMID_L(I,J)=WMID
            DZ(I,K,J)=TL_K*(P608*QL_K+1.)*R_D                           &
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
      IF(CONFIG_FLAGS%CU_PHYSICS/=BMJSCHEME)THEN
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KTS,KTE
!
          DO J=MYJS1_P1,MYJE1_P1
          DO I=MYIS_P1,MYIE_P1
            U_PHY(I,K,J)=(U(I+IHE(J),J,K)+U(I+IHW(J),J,K)               &
     &                   +U(I,J+1,K)+U(I,J-1,K))                        &
     &                   *0.25
            V_PHY(I,K,J)=(V(I+IHE(J),J,K)+V(I+IHW(J),J,K)               &
     &                   +V(I,J+1,K)+V(I,J-1,K))                        &
     &                   *0.25
          ENDDO
          ENDDO
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  TRANSPOSE THE MOIST ARRAY (IJK) FOR THE PHYSICS (IKJ).
!-----------------------------------------------------------------------
!
      IF(.NOT.ALLOCATED(MOIST_TRANS))THEN
        ALLOCATE(MOIST_TRANS(IMS:IME,KMS:KME,JMS:JME,N_MOIST),STAT=ISTAT)
      ENDIF
!
      DO N=1,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
          MOIST_TRANS(I,K,J,N)=MOIST(I,J,K,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO


      CALL SET_TILES(GRID,IDS+1,IDE-1,JDS+2,JDE-2,ITS,ITE,JTS,JTE)
#if (NMM_CORE==1)
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
           DUCUDT(i,j,k)=0.0
           DVCUDT(i,j,k)=0.0
           DTHCUDT(i,j,k)=0.0
           DQVCUDT(i,j,k)=0.0
           DQRCUDT(i,j,k)=0.0
           DQCCUDT(i,j,k)=0.0
           DQICUDT(i,j,k)=0.0
           DQSCUDT(i,j,k)=0.0
        ENDDO
        ENDDO
        ENDDO
#endif
      CALL CUMULUS_DRIVER(GRID                                          &
     &                 ,IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE &
     &                 ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME &
     &                 ,IPS=ips,IPE=ipe,JPS=jps,JPE=jpe,KPS=kps,KPE=kpe &
     &                 ,I_START=GRID%I_START,I_END=GRID%I_END           &
     &                 ,J_START=GRID%J_START,J_END=GRID%J_END           &
     &                 ,KTS=KTS,KTE=KTE,NUM_TILES=GRID%NUM_TILES        &
                  ! Prognostic
     &                 ,U=U_PHY,V=V_PHY,TH=TH_PHY,T=T_PHY,W=WINT        &
     &                 ,P=P_PHY,PI=PI_PHY,RHO=RR,W0AVG=W0AVG            &
                  ! Others
     &                 ,ITIMESTEP=NTSD,DT=DT,DX=GPS                     &
     &                 ,RAINC=RAINC,RAINCV=RAINCV,NCA=NCA               &
     &                 ,CLDFRA_DP=CLDFRA_DP,CLDFRA_SH=CLDFRA_SH         &
     &                 ,QC_CU=QC_CU, QI_CU=QI_CU                        &
     &                 ,DZ8W=DZ,P8W=P8W,FORCET=TTEN,FORCEQ=QTEN         &
     &                 ,CLDEFI=CLDEFI,LOWLYR=LOWLYR,XLAND=XLAND         &
     &                 ,CU_ACT_FLAG=CU_ACT_FLAG,WARM_RAIN=WARM_RAIN     &
! kf_edrates
     &                 ,UDR_KF=grid%udr_kf,DDR_KF=grid%ddr_kf           &
     &                 ,UER_KF=grid%uer_kf,DER_KF=grid%der_kf           &
     &                 ,TIMEC_KF=grid%timec_kf                          &
     &                 ,KF_EDRATES=config_flags%kf_edrates              &
!Biswas
     &                 ,HFX=HFX,QFX=QFX,PBLH=PBLH                       &
     &                 ,HPBL_HOLD=HPBL_HOLD                             &
     &                 ,ZNU=ZNU                                         &
     &                 ,STEPCU=NSTEP_CNV,GSW=GSW                        &
     &                 ,PERIODIC_X=.FALSE.,PERIODIC_Y=.FALSE.           &
     &                 ,HTOP=CUTOP,HBOT=CUBOT,KPBL=KPBL,HT=SFCZ,Z=Z     &
     &                 ,APR_GR=APR_GR,APR_W=APR_W,APR_MC=APR_MC         &
     &                 ,APR_ST=APR_ST,APR_AS=APR_AS,APR_CAPMA=APR_CAPMA &
     &                 ,APR_CAPME=APR_CAPME,APR_CAPMI=APR_CAPMI         &
     &                 ,MASS_FLUX=MASS_FLUX,XF_ENS=XF_ENS               &
     &                 ,PR_ENS=PR_ENS                                   &
     &                 ,GD_CLOUD=GD_CLOUD,GD_CLOUD2=GD_CLOUD2           &
     &                 ,KTOP_DEEP=KTOP_DEEP                             &
     &                 ,ENSDIM=ENSDIM,MAXIENS=1,MAXENS=3                &
     &                 ,MAXENS2=3,MAXENS3=16                            &
     &                 ,RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN             &
     &                 ,RQCCUTEN=RQCCUTEN,RQRCUTEN=RQRCUTEN             &
     &                 ,RQICUTEN=RQICUTEN,RQSCUTEN=RQSCUTEN             &
     &                 ,RTHBLTEN=RTHBLTEN,RQVBLTEN=RQVBLTEN             &
     &                 ,RTHRATEN=RTHRATEN                               &
#if (NMM_CORE==1)
     &                 ,RUCUTEN=DUCUDT, RVCUTEN=DVCUDT, MOMMIX=MOMMIX   &
                       ,store_rand=store_rand                           &
#if (HWRF==1)
     &                 ,pert_sas=config_flags%pert_sas                  &
     &                 ,ens_random_seed=config_flags%ens_random_seed    &
     &                 ,ens_sasamp=config_flags%ens_sasamp              &
#endif
#endif
     &                 ,SHALL=grid%shall                                &
     &                 ,HPBL2D=HPBL2D,EVAP2D=EVAP2D,HEAT2D=HEAT2D       & 
     &                 ,DX2D=DX2D,DYNMM=DYNMM                           & ! scale-sware SAS 
     &                 ,SCALEFUN=SCALEFUN, SCALEFUN1=SCALEFUN1          & !  scale function
     &                 ,SIGMU=SIGMU, SIGMU1=SIGMU1                      & !  updraft fraction

     &                 ,pgcon=config_flags%sas_pgcon                    &
     &                 ,sas_mass_flux=config_flags%sas_mass_flux        &
     &                 ,shalconv=config_flags%sas_shal_conv             &
     &                 ,shal_pgcon=config_flags%sas_shal_pgcon          &
     &                 ,IS_CAMMGMP_USED=IS_CAMMGMP_USED                 &
                  ! Selection argument
     &                 ,CU_PHYSICS=CONFIG_FLAGS%CU_PHYSICS              &
     &                 ,MP_PHYSICS=CONFIG_FLAGS%MP_PHYSICS              &
                  ! Moisture tracer arguments
     &                 ,QV_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QV),F_QV=F_QV &
     &                 ,QC_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QC),F_QC=F_QC &
     &                 ,QR_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QR),F_QR=F_QR &
     &                 ,QI_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QI),F_QI=F_QI &
     &                 ,QS_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QS),F_QS=F_QS &
     &                 ,QG_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QG),F_QG=F_QG)
!
#if (NMM_CORE==1)
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
           DTHCUDT(i,j,k)=RTHCUTEN(i,k,j)
           DQVCUDT(i,j,k)=RQVCUTEN(i,k,j)
           DQRCUDT(i,j,k)=RQRCUTEN(i,k,j)
           DQCCUDT(i,j,k)=RQCCUTEN(i,k,j)
           DQICUDT(i,j,k)=RQICUTEN(i,k,j)
           DQSCUDT(i,j,k)=RQSCUTEN(i,k,j)
        ENDDO
        ENDDO
        ENDDO
#endif

!-----------------------------------------------------------------------
!
!***  CNVTOP/CNVBOT HOLD THE MAXIMUM VERTICAL LIMITS OF CONVECTIVE CLOUD
!***  BETWEEN HISTORY OUTPUT TIMES.  HBOTS/HTOPS STORE SIMILIAR INFORMATION
!***  FOR SHALLOW (NONPRECIPITATING) CONVECTION, AND HBOTD/HTOPD ARE FOR
!***  DEEP (PRECIPITATING) CONVECTION.
!
      CF_HI=CONFIG_FLAGS%HISTORY_INTERVAL

      if(CF_HI<1e-5) then
         CF_HI = config_flags%history_interval_s/60.   &
               + config_flags%history_interval_m       &
               + config_flags%history_interval_h*3600. &
               + config_flags%history_interval_d*3600.*24.
      endif

      N_TIMSTPS_OUTPUT=NINT(60.*CF_HI/DT)
      MNTO=MOD(NTSD,max(1,N_TIMSTPS_OUTPUT))
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

      have_tg_tp = .false.
      have_swath = .false.
      have_tg_tp = (size(grid%tg_total_precip)>1)
#if ( HWRF == 1 )
      have_swath = ( size(grid%precip_swath)>1 )
#endif
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,ncubot,ncutop,pcpcol)
      pcp_cloud: DO J=MYJS2,MYJE2
        IENDX=MYIE1
        IF(MOD(J,2)==0.AND.ITE==IDE-1)IENDX=IENDX-1
        DO I=MYIS1,IENDX
!
!***  UPDATE PRECIPITATION
!
          PCPCOL=RAINCV(I,J)*1.E-3*NSTEP_CNV
          PREC(I,J)=PREC(I,J)+PCPCOL
          ACPREC(I,J)=ACPREC(I,J)+PCPCOL
          CUPREC(I,J)=CUPREC(I,J)+PCPCOL
          CUPPT(I,J)=CUPPT(I,J)+PCPCOL
          CPRATE(I,J)=PCPCOL
          if(have_tg_tp) then
             grid%tg_total_precip(i,j) = grid%tg_total_precip(i,j) + PCPCOL
          endif
#if ( HWRF == 1 )
          if(have_swath) then
             if(grid%interesting(i,j)/=0) then
                grid%precip_swath(i,j) = grid%precip_swath(i,j) + PCPCOL
                if(size(grid%cuprecip_swath)>0) then
                   grid%cuprecip_swath(i,j) = grid%cuprecip_swath(i,j) + PCPCOL
                endif
             endif
          endif
#endif
!
!***  SAVE CLOUD TOP AND BOTTOM FOR RADIATION (HTOP/HBOT) AND
!***  FOR OUTPUT (CNVTOP/CNVBOT, HTOPS/HBOTS, HTOPD/HBOTD) ARRAYS.
!***  THEY MUST BE TREATED SEPARATELY FROM EACH OTHER.
!
          CUTOP(I,J)=MIN(CUTOP(I,J),REAL(KDE))
          CUTOP(I,J)=MAX(CUTOP(I,J),0.0)
          CUBOT(I,J)=MIN(CUBOT(I,J),REAL(KDE))
          CUBOT(I,J)=MAX(CUBOT(I,J),0.0)

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
!
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
      ENDDO pcp_cloud
!
!-----------------------------------------------------------------------
!***  UPDATE TEMPERATURE, SPECIFIC HUMIDITY, AND HEATING.
!-----------------------------------------------------------------------
!
!-- ETAMP_Regional logical is true for regional NAM (ETAMPNEW) or HRW (ETAMPNEW) microphysics
!
      ETAMP_Regional=.FALSE.
      IF (CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW .OR.                        &
     &    CONFIG_FLAGS%MP_PHYSICS==FER_MP_HIRES) ETAMP_Regional=.TRUE.
!
!$omp parallel do                                                       &
!$omp& private(dqdt,dtdt,i,iendx,j,k,tchange)
      DO K=KTS,KTE
      DO J=MYJS2,MYJE2
        IENDX=MYIE1
        IF(MOD(J,2)==0.AND.ITE==IDE-1)IENDX=IENDX-1
        DO I=MYIS1,IENDX
!
!***  RQVCUTEN IN BMJDRV IS THE MIXING RATIO TENDENCY,
!***  SO RETRIEVE DQDT BY CONVERTING TO SPECIFIC HUMIDITY.
!
          DQDT=RQVCUTEN(I,K,J)/(1.+MOIST_TRANS(I,K,J,P_QV))**2
!
!***  RTHCUTEN IN BMJDRV IS DTDT OVER PI.
!
          DTDT=RTHCUTEN(I,K,J)*PI_PHY(I,K,J)
          T(I,J,K)=T(I,J,K)+DTDT*DTCNVC
          Q(I,J,K)=Q(I,J,K)+DQDT*DTCNVC
          TCUCN(I,J,K)=TCUCN(I,J,K)+DTDT
          MOIST_TRANS(I,K,J,P_QV)=Q(I,J,K)/(1.-Q(I,J,K))       !Convert to mixing ratio
!
          cps_select: SELECT CASE(config_flags%cu_physics)
!
          CASE (KFSCHEME,KFETASCHEME,GDSCHEME,SASSCHEME,SCALESASSCHEME,OSASSCHEME,KSASSCHEME,NSASSCHEME,TIEDTKESCHEME)
           IF(config_flags%mp_physics==fer_mp_hires_advect) THEN
             ! Update QI and QRIMEF:
             call QITEND_FER_HIRES_ADVECT( &
                  MOIST_TRANS(I,K,J,P_QI), &
                  SCALAR(I,J,K,P_QRIMEF), &
                  RQICUTEN(I,K,J)*DTCNVC)
            ELSEIF (ETAMP_Regional) THEN
              MOIST_TRANS(I,K,J,P_QS)=MAX(0.,MOIST_TRANS(I,K,J,P_QS)+RQICUTEN(I,K,J)*DTCNVC+RQSCUTEN(I,K,J)*DTCNVC)
            ELSE
              MOIST_TRANS(I,K,J,P_QI)=MAX(0.,MOIST_TRANS(I,K,J,P_QI)+RQICUTEN(I,K,J)*DTCNVC)
              MOIST_TRANS(I,K,J,P_QS)=MAX(0.,MOIST_TRANS(I,K,J,P_QS)+RQSCUTEN(I,K,J)*DTCNVC)
            ENDIF
            MOIST_TRANS(I,K,J,P_QR)=MAX(0.,MOIST_TRANS(I,K,J,P_QR)+RQRCUTEN(I,K,J)*DTCNVC)
            MOIST_TRANS(I,K,J,P_QC)=MAX(0.,MOIST_TRANS(I,K,J,P_QC)+RQCCUTEN(I,K,J)*DTCNVC)
          END SELECT cps_select
!
          TCHANGE=DTDT*DTCNVC
	  IF(ABS(TCHANGE)>DTEMP_CHECK)THEN
            WRITE(message,*)'BIG T CHANGE BY CONVECTION=',TCHANGE             &
                     ,' AT (',I,',',J,',',K,') FOR NTSD=',NTSD
            CALL wrf_message(trim(message))
	  ENDIF
!
        ENDDO
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!***  REFILL THE MOIST ARRAY.
!-----------------------------------------------------------------------
!
      DO N=1,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO J=JMS,JME
        DO K=KMS,KME
        DO I=IMS,IME
          MOIST(I,J,K,N)=MOIST_TRANS(I,K,J,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(MOIST_TRANS,STAT=ISTAT)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE CUCNVC
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE GSMDRIVE(NTSD,DT,NPHS,N_MOIST                          &
     &                   ,DX,DY,SM,HBM2,FIS                             &
     &                   ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2             &
     &                   ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TRAIN            &
     &                   ,MOIST,SCALAR,N_SCALAR                         &
     &                   ,F_ICE,F_RAIN,F_RIMEF,SR                       &
     &                   ,PREC,ACPREC,AVRAIN                            &
     &                   ,MP_RESTART_STATE                              &
     &                   ,TBPVS_STATE                                   &
     &                   ,TBPVS0_STATE                                  &
     &                   ,GRID,CONFIG_FLAGS                             &
     &                   ,re_cloud,re_ice,re_snow                       & ! G. Thompson
     &                   ,has_reqc,has_reqi,has_reqs                    & ! G. Thompson
     &                   ,diag_flag                                     & 
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
!   05-12-19  BLACK      - CONVERTED FROM IKJ TO IJK
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
      REAL,INTENT(IN) :: DT,DX,DY,PDTOP,PT
!
      REAL,INTENT(INOUT) :: AVRAIN
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: FIS,HBM2,PD,RES,SM
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PINT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACPREC,PREC
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: CWM,Q    &
     &                                                        ,T,TRAIN
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: F_ICE    &   !<--- Used only with physics (IKJ)
     &                                                        ,F_RAIN   &
     &                                                        ,F_RIMEF

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,N_MOIST)                   &
     &                                           ,INTENT(INOUT) :: MOIST
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,N_SCALAR)                  &
     &                                          ,INTENT(INOUT) :: SCALAR
!
!***  State var for etampnew microphysics (JM, 2005 05 02)
!
      REAL,DIMENSION(:),INTENT(INOUT) :: MP_RESTART_STATE               &
     &                                  ,TBPVS_STATE,TBPVS0_STATE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: SR
!
!..Additions for coupling cloud physics effective radii and radiation.  G. Thompson
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT):: re_cloud, re_ice, re_snow
      INTEGER, INTENT(IN):: has_reqc, has_reqi, has_reqs
!
      TYPE(DOMAIN),TARGET :: GRID
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER :: I,IENDX,IJ,ISTAT,J,K,N
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME) :: LOWLYR
!
      REAL :: CAPA,DPL,DTPHS,PCPCOL,PLYR,RDTPHS,RG,TNEW
!
      REAL,DIMENSION(KMS:KME-1) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CUBOT,CUTOP,PDSL               &
     &                                  ,RAINNC,RAINNCV,XLAND
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: CWM_PHY,DZ             &
     &                                          ,P8W,P_PHY,PI_PHY       &
     &                                          ,RR,T_PHY,TH_PHY
!
      REAL,DIMENSION(:,:,:,:),ALLOCATABLE :: MOIST_TRANS
      REAL,DIMENSION(:,:,:,:),ALLOCATABLE :: SCALAR_TRANS
      REAL,DIMENSION(:,:,:),  ALLOCATABLE :: W_TRANS
!
      LOGICAL                        :: diag_flag
      LOGICAL :: E_BDY,F_QT,QT_PRESENT,WARM_RAIN, have_tg_tp, have_swath
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      ALLOCATE(MOIST_TRANS(IMS:IME,KMS:KME,JMS:JME,N_MOIST),STAT=ISTAT)
      ALLOCATE(SCALAR_TRANS(IMS:IME,KMS:KME,JMS:JME,N_SCALAR),STAT=ISTAT)
      ALLOCATE(W_TRANS(IMS:IME,KMS:KME,JMS:JME))
!
!-----------------------------------------------------------------------
!***  TRANSPOSE THE MOIST ARRAY (IJK) FOR THE PHYSICS (IKJ).
!-----------------------------------------------------------------------
!
      DO N=2,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
          MOIST_TRANS(I,K,J,N)=MOIST(I,J,K,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
      do k=kts,kte
      do j=jts,jte
      do i=its,ite
         w_trans(i,k,j)=max(grid%w(i,j,k),grid%w_tot(i,j,k))
      enddo
      enddo
      enddo
!
!-----------------------------------------------------------------------
!
!-- QT_PRESENT logical is true for regional NAM (ETAMPNEW), HRW (ETAMPNEW),
!   or HWRF (ETAMP_HWRF) microphysics
!
      QT_PRESENT=.FALSE.
      IF (CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW .OR.                        &
     &    CONFIG_FLAGS%MP_PHYSICS==FER_MP_HIRES .OR.                        &
     &    CONFIG_FLAGS%MP_PHYSICS==ETAMP_HWRF) QT_PRESENT=.TRUE.
!
      micro_check1: IF(.NOT.QT_PRESENT) THEN
        DO N=2,N_SCALAR
!$omp parallel do                                                       &
!$omp& private(i,j,k)
          DO K=KMS,KME
          DO J=JMS,JME
          DO I=IMS,IME
            SCALAR_TRANS(I,K,J,N)=SCALAR(I,J,K,N)
          ENDDO
          ENDDO
          ENDDO
        ENDDO
      ENDIF micro_check1
!
!-----------------------------------------------------------------------
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
!$omp& private(i,j)
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
!
        PDSL(I,J)=PD(I,J)*RES(I,J)
        P8W(I,KTE+1,J)=PT
        LOWLYR(I,J)=KTS        !<----  The lowest model layer counted from the bottom.
        XLAND(I,J)=SM(I,J)+1.
!-----------------------------------------------------------------------
!***  FILL RAINNC WITH ZERO (NORMALLY CONTAINS THE NONCONVECTIVE
!***  ACCUMULATED RAIN BUT NOT YET USED BY NMM).
!***  CAN BE OBTAINED FROM ACPREC AND CUPREC (ACPREC-CUPREC).
!-----------------------------------------------------------------------
        RAINNC(I,J)=0.
!
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  FILL THE SINGLE-COLUMN INPUT
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(dpl,i,j,k,plyr,ql,tl)
      DO J=MYJS2,MYJE2
        DO K=KTS,KTE
        DO I=MYIS1,MYIE1
          DPL=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
          QL(K)=MAX(Q(I,J,K),EPSQ)
!!!       PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL(I,J)+PT
          PLYR=(PINT(I,J,K)+PINT(I,J,K+1))*0.5
          TL(K)=T(I,J,K)
!
          RR(I,K,J)=PLYR/(R_D*TL(K)*(P608*QL(K)+1.))
          T_PHY(I,K,J)=TL(K)
          PI_PHY(I,K,J)=(PLYR*1.E-5)**CAPA
          TH_PHY(I,K,J)=TL(K)/PI_PHY(I,K,J)
!!!       P8W(I,KFLIP,J)=PINT(I,J,K+1)
          P8W(I,K,J)=ETA1(K)*PDTOP+ETA2(K)*PDSL(I,J)+PT
          P_PHY(I,K,J)=PLYR
          DZ(I,K,J)=DPL*RG/RR(I,K,J)
          CWM_PHY(I,K,J)=CWM(I,J,K)
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
     &                 ,W=w_trans                                       &
     &                 ,MP_PHYSICS=CONFIG_FLAGS%MP_PHYSICS              &
     &                 ,SPECIFIED=CONFIG_FLAGS%SPECIFIED                &
     &                        .OR.CONFIG_FLAGS%NESTED                   &
     &                 ,SPEC_ZONE=0,WARM_RAIN=WARM_RAIN                 &
     &                 ,XLAND=XLAND,ITIMESTEP=NTSD-1                    &
     &                 ,F_ICE_PHY=F_ICE,F_RAIN_PHY=F_RAIN               &
     &                 ,F_RIMEF_PHY=F_RIMEF                             &
     &                 ,LOWLYR=LOWLYR,SR=SR                             &
     &                 ,QV_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QV),F_QV=F_QV &
     &                 ,QC_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QC),F_QC=F_QC &
     &                 ,QR_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QR),F_QR=F_QR &
     &                 ,QI_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QI),F_QI=F_QI &
     &                 ,QS_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QS),F_QS=F_QS &
     &                 ,QG_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QG),F_QG=F_QG &
     &                 ,QH_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QH),F_QH=F_QH &
     &        , QNN_CURR=SCALAR_TRANS(ims,kms,jms,P_QNN), F_QNN=F_QNN          &
     &        , QNDROP_CURR=SCALAR_TRANS(ims,kms,jms,P_QNDROP), F_QNDROP=F_QNDROP &
     &        , QNI_CURR=SCALAR_TRANS(ims,kms,jms,P_QNI), F_QNI=F_QNI          &
     &        , QNC_CURR=SCALAR_TRANS(ims,kms,jms,P_QNC), F_QNC=F_QNC          &
     &        , QNR_CURR=SCALAR_TRANS(ims,kms,jms,P_QNR), F_QNR=F_QNR          &
     &        , QNS_CURR=SCALAR_TRANS(ims,kms,jms,P_QNS), F_QNS=F_QNS          &
     &        , QNG_CURR=SCALAR_TRANS(ims,kms,jms,P_QNG), F_QNG=F_QNG          &
     &        , QNH_CURR=SCALAR_TRANS(ims,kms,jms,P_QNH), F_QNH=F_QNH          & ! for milbrandt2mom and nssl_2mom
     &        , QVOLG_CURR=SCALAR_TRANS(ims,kms,jms,P_QVOLG), F_QVOLG=F_QVOLG    & ! for nssl_2mom
     &        , QVOLH_CURR=SCALAR_TRANS(ims,kms,jms,P_QVOLH), F_QVOLH=F_QVOLH    & ! for nssl_2mom
     &                 ,QRIMEF_CURR=SCALAR_TRANS(IMS,KMS,JMS,P_QRIMEF),F_QRIMEF=F_QRIMEF &
     &                 ,QT_CURR=CWM_PHY,F_QT=QT_PRESENT                 &
     &                 ,MP_RESTART_STATE=MP_RESTART_STATE               &
     &                 ,TBPVS_STATE=TBPVS_STATE                         &
     &                 ,TBPVS0_STATE=TBPVS0_STATE                       &
     &                 ,IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE &
     &                 ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME &
     &                 ,I_START=GRID%I_START,I_END=GRID%I_END           &
     &                 ,J_START=GRID%J_START,J_END=GRID%J_END           &
     &                 ,KTS=KTS,KTE=KTE,NUM_TILES=GRID%NUM_TILES        &
     &                 ,DO_RADAR_REF=config_flags%do_radar_ref          &
     &                 ,DIAGFLAG=diag_flag                              &
     &                 ,ID=grid%id                                      &
     &                 ,num_scalar=1                                      &  !mchen temporary
     &                 ,refl_10cm=grid%refl_10cm & ! to calc. radar reflectivity
     &                 ,re_cloud=grid%re_cloud                          & ! G. Thompson
     &                 ,re_ice=grid%re_ice                              & ! G. Thompson
     &                 ,re_snow=grid%re_snow                            & ! G. Thompson
     &                 ,has_reqc=has_reqc                               & ! G. Thompson
     &                 ,has_reqi=has_reqi                               & ! G. Thompson
     &                 ,has_reqs=has_reqs                               & ! G. Thompson
     &                 ,ccn_conc=config_flags%ccn_conc                  &
     &                 ,aercu_opt=config_flags%aercu_opt                &
                                                                        )

!$omp parallel do                                                       &
!$omp& private(ij)
      DO IJ=1,GRID%NUM_TILES
        CALL MICROPHYSICS_ZERO_OUTA(                                    &
                     MOIST_TRANS,N_MOIST,CONFIG_FLAGS                   &
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
!-----------------------------------------------------------------------
!***  UPDATE TEMPERATURE, SPECIFIC HUMIDITY, CLOUD WATER, AND HEATING.
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,k,tnew)
      DO K=KTS,KTE
        DO J=MYJS2,MYJE2
          IENDX=MYIE1
          IF(E_BDY.AND.MOD(J,2)==0)IENDX=IENDX-1
          DO I=MYIS1,IENDX
            TNEW=TH_PHY(I,K,J)*PI_PHY(I,K,J)
            TRAIN(I,J,K)=TRAIN(I,J,K)+(TNEW-T(I,J,K))*RDTPHS
            T(I,J,K)=TNEW
            Q(I,J,K)=MOIST_TRANS(I,K,J,P_QV)/(1.+MOIST_TRANS(I,K,J,P_QV))
            CWM(I,J,K)=CWM_PHY(I,K,J)
          ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  UPDATE PRECIPITATION.
!***  NOTE: RAINNC IS ACCUMULATED INSIDE MICROPHYSICS BUT NMM ZEROES IT
!***  OUT ABOVE SINCE IT IS ONLY A LOCAL ARRAY FOR NOW.
!-----------------------------------------------------------------------
!
#if ( HWRF == 1 )
      have_swath = ( size(grid%precip_swath)>1 )
#endif
      have_tg_tp = (size(grid%tg_total_precip)>1)
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,pcpcol)
      DO J=MYJS2,MYJE2
        IENDX=MYIE1
        IF(E_BDY.AND.MOD(J,2)==0)IENDX=IENDX-1
        DO I=MYIS1,IENDX
          PCPCOL=RAINNCV(I,J)*1.E-3
          PREC(I,J)=PREC(I,J)+PCPCOL
          ACPREC(I,J)=ACPREC(I,J)+PCPCOL
          if(have_tg_tp) then
             grid%tg_total_precip(i,j) = grid%tg_total_precip(i,j) + PCPCOL
          endif
#if ( HWRF == 1 )
          if(have_swath) then
             if(grid%interesting(i,j)/=0) &
                  grid%precip_swath(i,j) = grid%precip_swath(i,j) + PCPCOL
          endif
#endif
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  REFILL THE MOIST ARRAY.
!-----------------------------------------------------------------------
!
      DO N=2,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO J=JMS,JME
        DO K=KMS,KME
        DO I=IMS,IME
          MOIST(I,J,K,N)=MOIST_TRANS(I,K,J,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      micro_check2: IF (.NOT.QT_PRESENT) THEN
        DO N=2,N_SCALAR
!$omp parallel do                                                       &
!$omp& private(i,j,k)
          DO J=JMS,JME
          DO K=KMS,KME
          DO I=IMS,IME
            SCALAR(I,J,K,N)=SCALAR_TRANS(I,K,J,N)
          ENDDO
          ENDDO
          ENDDO
        ENDDO
      ENDIF micro_check2
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(MOIST_TRANS,STAT=ISTAT)
      DEALLOCATE(SCALAR_TRANS,STAT=ISTAT)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE GSMDRIVE
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE UPDATE_MOIST(MOIST,Q,CWM,F_ICE,F_RAIN,N_MOIST          &
     &                       ,IDS,IDE,JDS,JDE,KDS,KDE                   &
     &                       ,IMS,IME,JMS,JME,KMS,KME                   &
     &                       ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,N_MOIST
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: CWM,Q
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: F_ICE       &   !<--- Used only with physics (IKJ)
     &                                                     ,F_RAIN
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,N_MOIST),INTENT(OUT) :: MOIST
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER :: I,J,K
!
      REAL :: FICE,FRAIN,QI,QR,QW,WC
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      DO K=KTS,KTE
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        MOIST(I,J,K,P_QV)=Q(I,J,K)/(1.-Q(I,J,K))
        WC=CWM(I,J,K)
        QI=0.
        QR=0.
        QW=0.
        FICE=F_ICE(I,K,J)
        FRAIN=F_RAIN(I,K,J)
!
        IF(FICE>=1.)THEN
          QI=WC
        ELSEIF(FICE<=0.)THEN
          QW=WC
        ELSE
          QI=FICE*WC
          QW=WC-QI
        ENDIF
!
        IF(QW>0..AND.FRAIN>0.)THEN
          IF(FRAIN>=1.)THEN
            QR=QW
            QW=0.
          ELSE
            QR=FRAIN*QW
            QW=QW-QR
          ENDIF
        ENDIF
!
        MOIST(I,J,K,P_QC)=QW
        MOIST(I,J,K,P_QR)=QR
        MOIST(I,J,K,P_QI)=0.
        MOIST(I,J,K,P_QS)=QI
        MOIST(I,J,K,P_QG)=0.
      ENDDO
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE UPDATE_MOIST
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      END MODULE MODULE_PHYSICS_CALLS
!
!-------------------------------------------------------------------
