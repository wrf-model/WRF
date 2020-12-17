!WRF:MODEL_RA:RADIATION
!
!-----------------------------------------------------------------------
!--  Search for "!GFDL" for changes to improve coupling with microphysics
!-----------------------------------------------------------------------
MODULE MODULE_RA_HWRF
      USE MODULE_CONFIGURE,ONLY : GRID_CONFIG_REC_TYPE
      USE MODULE_MODEL_CONSTANTS
!GFDL      USE MODULE_MP_ETANEW, ONLY : RHGRD,FPVS
      USE MODULE_MP_HWRF, ONLY : RHGRD_in,RHGRD_out,FPVS   !GFDL
      INTEGER,PARAMETER :: NL=81
      INTEGER,PARAMETER :: NBLY=15
      REAL,PARAMETER :: RTHRESH=1.E-15

      INTEGER, SAVE, DIMENSION(3)     :: LTOP
      REAL   , SAVE, DIMENSION(37,NL) :: XDUO3N,XDO3N2,XDO3N3,XDO3N4
      REAL   , SAVE, DIMENSION(NL)    :: PRGFDL
      REAL   , SAVE                   :: AB15WD,SKO2D,SKC1R,SKO3R

      REAL   , SAVE :: EM1(28,180),EM1WDE(28,180),TABLE1(28,180),     &
                           TABLE2(28,180),TABLE3(28,180),EM3(28,180), &
                           SOURCE(28,NBLY), DSRCE(28,NBLY)

      REAL   ,SAVE, DIMENSION(5040):: T1,T2,T4,EM1V,EM1VW,EM3V
      REAL   ,SAVE                 :: R1
! Created by CO2 initialization
      REAL,   SAVE, ALLOCATABLE, DIMENSION(:,:) :: CO251,CDT51,CDT58,C2D51,&
                                           C2D58,CO258
      REAL,   SAVE, ALLOCATABLE, DIMENSION(:)   :: STEMP,GTEMP,CO231,CO238, &
                                           C2D31,C2D38,CDT31,CDT38, &
                                           CO271,CO278,C2D71,C2D78, &
                                           CDT71,CDT78
      REAL,   SAVE, ALLOCATABLE, DIMENSION(:)   :: CO2M51,CO2M58,CDTM51,CDTM58, &
                                           C2DM51,C2DM58
      CHARACTER(256) :: ERRMESS

! Used by CO2 initialization
!     COMMON/PRESS/PA(109)
!     COMMON/TRAN/ TRANSA(109,109)
!     COMMON/COEFS/XA(109),CA(109),ETA(109),SEXPV(109),CORE,UEXP,SEXP
      REAL   ,SAVE, DIMENSION(109) :: PA, XA, CA, ETA, SEXPV
      REAL   ,SAVE, DIMENSION(109,109) :: TRANSA
      REAL   ,SAVE  :: CORE,UEXP,SEXP

      EQUIVALENCE (EM1V(1),EM1(1,1)),(EM1VW(1),EM1WDE(1,1)) 
      EQUIVALENCE (EM3V(1),EM3(1,1))
      EQUIVALENCE (T1(1),TABLE1(1,1)),(T2(1),TABLE2(1,1)), &
                  (T4(1),TABLE3(1,1))
CONTAINS

!-----------------------------------------------------------------------
      SUBROUTINE HWRFRAINIT(SFULL,SHALF,PPTOP,JULYR,MONTH,IDAY,GMT,    &
     &                       CONFIG_FLAGS, ALLOWED_TO_READ,                           &
     &                       KDS,KDE,KMS,KME,KTS,KTE)
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      TYPE (GRID_CONFIG_REC_TYPE) :: CONFIG_FLAGS
      INTEGER,INTENT(IN) :: KDS,KDE,KMS,KME,KTS,KTE
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: SFULL, SHALF
      INTEGER,INTENT(IN) :: JULYR,MONTH,IDAY
      REAL,INTENT(IN) :: GMT,PPTOP
      LOGICAL,INTENT(IN) :: ALLOWED_TO_READ
      INTEGER :: IHRST,N
      REAL :: PCLD
      REAL :: SSLP=1013.25
      REAL :: PTOP_HI=150.,PTOP_MID=350.,PTOP_LO=642.
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!***  INITIALIZE DIAGNOSTIC LOW,MIDDLE,HIGH CLOUD LAYER PRESSURE LIMITS.
!
      LTOP(1)=0
      LTOP(2)=0
      LTOP(3)=0
!
!dule      DO N=1,KTE
      DO N=1,KTE-1
        PCLD=(SSLP-PPTOP*10.)*SHALF(N)+PPTOP*10.
        IF(PCLD>=PTOP_LO)LTOP(1)=N
        IF(PCLD>=PTOP_MID)LTOP(2)=N
        IF(PCLD>=PTOP_HI)LTOP(3)=N
!       PRINT *,N,PCLD,SHALF(N),PSTAR,PPTOP
      ENDDO
!
!***  USE CALL TO CONRAD FOR DIRECT READ OF CO2 FUNCTIONS
!***  OTHERWISE CALL CO2O3.
!
      IF(ALLOWED_TO_READ)THEN
#if (EM_CORE==1)
        CALL CO2O3(SFULL,SHALF,PPTOP,KME-KMS,KME-KMS+1,KME-KMS+2)
#endif
#if (NMM_CORE==1)
        IF(CONFIG_FLAGS%CO2TF==1)THEN
          CALL CO2O3(SFULL,SHALF,PPTOP,KME-KMS,KME-KMS+1,KME-KMS+2)
        ELSE
          CALL CONRAD(KDS,KDE,KMS,KME,KTS,KTE)
        ENDIF
#endif
!
        CALL O3CLIM
        CALL TABLE
        IHRST=NINT(GMT)
        CALL SOLARD(IHRST,IDAY,MONTH,JULYR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE HWRFRAINIT
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
      SUBROUTINE HWRFRA(explicit_convection, DT,THRATEN,THRATENLW,THRATENSW,PI3D &
     &                ,XLAND,P8W,DZ8W,RHO_PHY,P_PHY,T                   &
     &                ,QV,QW,QI                                         & 
     &                ,TSK2D,GLW,GSW                                    &
     &                ,TOTSWDN,TOTLWDN,RSWTOA,RLWTOA,CZMEAN             & 
     &                ,GLAT,GLON,HTOP,HBOT,htopr,hbotr,ALBEDO,CUPPT     &
     &                ,VEGFRA,SNOW,G,GMT                                &
     &                ,NSTEPRA,NPHS,ITIMESTEP                           &
     &                ,JULYR,JULDAY,GFDL_LW,GFDL_SW                     &
     &                ,CFRACL,CFRACM,CFRACH                             &
     &                ,ACFRST,NCFRST,ACFRCV,NCFRCV                      &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      LOGICAL, INTENT(IN) :: explicit_convection
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE,ITIMESTEP           &
     &                     ,NPHS,NSTEPRA
 
      INTEGER,INTENT(IN) :: julyr,julday   
      INTEGER,INTENT(INOUT),DIMENSION(ims:ime,jms:jme) :: NCFRST        & !Added
                                                         ,NCFRCV          !Added
      REAL,INTENT(IN) :: DT,GMT,G

      REAL,INTENT(INOUT),DIMENSION(ims:ime, kms:kme, jms:jme)::         &
                                         THRATEN,THRATENLW,THRATENSW
      REAL,INTENT(IN),DIMENSION(ims:ime, kms:kme, jms:jme)::p8w,dz8w,   &
     &                                                      rho_phy,    &
     &                                                      p_phy,      &
     &                                                      PI3D
      REAL, INTENT(IN), DIMENSION(ims:ime, jms:jme):: ALBEDO,SNOW,      &
     &                                                TSK2D,VEGFRA,     &
     &                                                XLAND
      REAL, INTENT(IN), DIMENSION(ims:ime, jms:jme):: GLAT,GLON
      REAL, INTENT(INOUT), DIMENSION(ims:ime, jms:jme):: HTOP,HBOT,htopr,hbotr,cuppt
      REAL, INTENT(INOUT), DIMENSION(ims:ime, jms:jme):: RSWTOA,        & !Added
     &                                                   RLWTOA,        & !Added
     &                                                   ACFRST,        & !Added
     &                                                   ACFRCV
      REAL,INTENT(INOUT),DIMENSION(ims:ime, jms:jme):: GLW,GSW
      REAL,INTENT(OUT),DIMENSION(ims:ime, jms:jme):: CZMEAN,            &
     &                                               TOTLWDN,TOTSWDN
      REAL,INTENT(OUT),DIMENSION(ims:ime, jms:jme):: CFRACL,CFRACM,     & !Added
     &                                               CFRACH               !Added
      REAL, INTENT(IN), DIMENSION(ims:ime, kms:kme, jms:jme):: QI,QV,   &
     &                                                         QW,T
!     REAL, INTENT(IN), DIMENSION(37*kte) :: RAD1,RAD2,RAD3,RAD4
      LOGICAL, INTENT(IN) :: gfdl_lw,gfdl_sw

      REAL, DIMENSION(its:ite, kms:kme, jts:jte):: PFLIP,QIFLIP,QFLIP,  &
     &                                             QWFLIP,TFLIP
      REAL, DIMENSION(its:ite, kms:kme, jts:jte)::P8WFLIP,PHYD
      REAL, DIMENSION(its:ite, kts:kte, jts:jte)::TENDS,TENDL

      INTEGER :: IDAT(3),Jmonth,Jday
      INTEGER :: I,J,K,KFLIP,IHRST
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      IF(GFDL_LW.AND.GFDL_SW )GO TO 100

!
! NEED HYDROSTATIC PRESSURE HERE (MONOTONIC CHANGE WITH HEIGHT)
      DO J=JTS,JTE
      DO I=ITS,ITE
        PHYD(I,KTS,J)=P8W(I,KTS,J) 
      ENDDO
      ENDDO
!
      DO J=JTS,JTE
        DO K=KTS,KTE
        DO I=ITS,ITE
          PHYD(I,K+1,J)=PHYD(I,K,J)-G*RHO_PHY(I,K,J)*DZ8W(I,K,J)
        ENDDO
        ENDDO
      ENDDO
!
      DO K=KMS,KME
         KFLIP=KME+1-K
         DO J=JTS,JTE
         DO I=ITS,ITE
           P8WFLIP(I,K,J)=PHYD(I,KFLIP,J)
         ENDDO
         ENDDO
      ENDDO
!
!- Note that the effects of rain are ignored in this radiation package (BSF 2005-01-25)
!
      DO K=KTS,KTE
        KFLIP=KTE+1-K
        DO J=JTS,JTE
        DO I=ITS,ITE
          TFLIP (I,K,J)=T(I,KFLIP,J)
          QFLIP (I,K,J)=MAX(0.,QV(I,KFLIP,J)/(1.+QV(I,KFLIP,J)))
          QWFLIP(I,K,J)=QW(I,KFLIP,J)      !Modified
          QIFLIP(I,K,J)=QI(I,KFLIP,J)      !Added QI
!         PFLIP (I,K,J)=P_PHY(I,KFLIP,J)
!
!***  USE MONOTONIC HYDROSTATIC PRESSURE INTERPOLATED TO MID-LEVEL
!
          PFLIP(I,K,J)=0.5*(P8WFLIP(I,K,J)+P8WFLIP(I,K+1,J))
        ENDDO
        ENDDO
      ENDDO
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        HBOTR(I,J)=HBOT(I,J)
        HTOPR(I,J)=HTOP(I,J)        
      ENDDO
      ENDDO
         
      DO J=JTS,JTE
      DO I=ITS,ITE
        HBOT(I,J)=KTE+1-HBOT(I,J)
        HTOP(I,J)=KTE+1-HTOP(I,J)
      ENDDO
      ENDDO
!
      CALL CAL_MON_DAY(JULDAY,JULYR,JMONTH,JDAY)     
!
      IDAT(1)=JMONTH
      IDAT(2)=JDAY
      IDAT(3)=JULYR
      IHRST  =NINT(GMT)

!     CALL SOLARD(R1,IHRST,IDAT)
!     CALL SOLARD(R1,IHRST,JULDAY)
!-----------------------------------------------------------------------
      CALL RADTN (DT,TFLIP,QFLIP,QWFLIP,QIFLIP,                         &
     &            PFLIP,P8WFLIP,XLAND,TSK2D,                            &
     &            GLAT,GLON,HTOP,HBOT,ALBEDO,cuppt,                     &
     &            .not.explicit_convection,                             &
     &            ACFRCV,NCFRCV,ACFRST,NCFRST,                          &
     &            VEGFRA,SNOW,GLW,GSW,                                  &
     &            TOTSWDN,TOTLWDN,                                      &
     &            IDAT,IHRST,                                           &
     &            NSTEPRA,NSTEPRA,NPHS,ITIMESTEP,                       &
     &            TENDS,TENDL,RSWTOA,RLWTOA,CZMEAN,                     &
     &            CFRACL,CFRACM,CFRACH,                                 &
     &            IDS,IDE,JDS,JDE,KDS,KDE,                              &
     &            IMS,IME,JMS,JME,KMS,KME,                              &
     &            ITS,ITE,JTS,JTE,KTS,KTE                              )
!-----------------------------------------------------------------------
!
      IF(GFDL_LW)THEN
        DO J=JTS,JTE
        DO K = KTS,KTE
          KFLIP=KTE+1-K
          DO I=ITS,ITE
            THRATENLW(I,K,J)=TENDL(I,KFLIP,J)/PI3D(I,K,J)
            THRATENSW(I,K,J)=TENDS(I,KFLIP,J)/PI3D(I,K,J) !Put in SW section
            THRATEN(I,K,J)  =THRATEN(I,K,J) + THRATENLW(I,K,J)
          ENDDO
        ENDDO
        ENDDO
      ENDIF
!
!*** THIS ASSUMES THAT LONGWAVE IS CALLED FIRST IN THE RADIATION_DRIVER.
!
      IF(GFDL_SW)THEN
        DO J=JTS,JTE
        DO K=KTS,KTE
          KFLIP=KTE+1-K
          DO I=ITS,ITE
            THRATENSW(I,K,J)=TENDS(I,KFLIP,J)/PI3D(I,K,J)
!!!         THRATEN(I,K,J)=THRATEN(I,K,J)+THRATENSW(I,K,J)  !Added
          ENDDO
        ENDDO
        ENDDO
      ENDIF

!
!***  RESET ACCUMULATED CONVECTIVE CLOUD TOP/BOT AND CONVECTIVE PRECIP
!***  FOR NEXT INTERVAL BETWEEN RADIATION CALLS
!
      DO J=JTS,JTE
      DO I=ITS,ITE
!!!!    HBOT(I,J)=KTE+1-HBOT(I,J)
!!!!    HTOP(I,J)=KTE+1-HTOP(I,J)
        HBOT(I,J)=REAL(KTE+1)
        HTOP(I,J)=0.
        CUPPT(I,J)=0.
      ENDDO
      ENDDO
         !
  100 IF(GFDL_SW)THEN
        DO J=JTS,JTE
        DO K=KTS,KTE
          KFLIP=KTE+1-K
          DO I=ITS,ITE
            THRATEN(I,K,J)=THRATEN(I,K,J)+THRATENSW(I,K,J)
          ENDDO
        ENDDO
        ENDDO
      ENDIF
!
  END SUBROUTINE HWRFRA
!
!-----------------------------------------------------------------------
      SUBROUTINE RADTN(DT,T,Q,QCW,QICE,                                 &
     &                 PFLIP,P8WFLIP,XLAND,TSK2D,                       &
     &                 GLAT,GLON,HTOP,HBOT,ALB,CUPPT,CNCLD,             &
!    &                 RAD1,RAD2,RAD3,RAD4,                             &
!    &                 TABLE1,TABLE2,TABLE3,EM1,EM1WDE,EM3,             &
     &                 ACFRCV,NCFRCV,ACFRST,NCFRST,                     &
     &                 VEGFRC,SNO,GLW,GSW,                              & 
     &                 RSWIN,RLWIN,                                     & !Added
!    &                 IDAT,LTOP,IHRST,PRGFDL,                          &
     &                 IDAT,IHRST,                                      &
     &                 NRADS,NRADL,NPHS,NTSD,                           &
!    &                 SKO3R,AB15WD,SKC1R,SKO2D,                        &
!#$  &                 TENDS,TENDL,                                     &
     &                 TENDS,TENDL,RSWTOA,RLWTOA,CZMEAN,                &
     &                 CFRACL,CFRACM,CFRACH,                            & !Added
     &                 ids,ide, jds,jde, kds,kde,                       &
     &                 ims,ime, jms,jme, kms,kme,                       &
     &                 its,ite, jts,jte, kts,kte                       )
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
! GLAT : geodetic latitude in radians of the mass points on the computational grid.

! CZEN : instantaneous cosine of the solar zenith angle.

! HTOP : (REAL) model layer number that is highest in the atmosphere
!        in which convective cloud occurred since the previous call to the
!        radiation driver.

! HBOT : (REAL) model layer number that is lowest in the atmosphere
!        in which convective cloud occurred since the previous call to the
!        radiation driver.

! ALB  : is no longer used in the operational radiation.  Prior to 24 July 2001
!        ALB was the climatological albedo that was modified within RADTN to
!        account for vegetation fraction and snow.
!
! ALB  : reintroduced as the dynamic albedo from LSM

! CUPPT: accumulated convective precipitation (meters) since the
!        last call to the radiation.

! THS : potential temperature of the ground surface.

! IHE and IHW are relative location indices needed to locate neighboring
!       points on the Eta's Arakawa E grid since arrays are indexed locally on
!       each MPI task rather than globally.  IHE refers to the adjacent grid
!       point (a V point) to the east of the mass point being considered.  IHW
!       is the adjacent grid point to the west of the given mass point.

! IRAD is a relic from older code that is no longer needed.

! ACFRCV : sum of the convective cloud fractions that were computed
!          during each call to the radiation between calls to the subroutines that
!          do the forecast output.

! NCFRCV : the total number of times in which the convective cloud
!          fraction was computed to be greater than zero in the radiation between
!          calls to the output routines.  In the post-processor, ACFRCV is divided
!          by NCFRCV to yield an average convective cloud fraction.

!          ACFRST and NCFRST are the analogs for stratiform cloud cover.

!          VEGFRC is the fraction of the gridbox with vegetation.

!          LVL holds the number of model layers that lie below the ground surface
!          at each point.  Clearly for sigma coordinates LVL is zero everywhere.

! CTHK  :  an assumed maximum thickness of stratiform clouds currently set
!          to 20000 Pascals.  I think this is relevant for computing "low",
!          "middle", and "high" cloud fractions which are post-processed but which
!          do not feed back into the integration.

! IDAT  : a 3-element integer array holding the month, day, and year,
!        respectively, of the date for the start time of the free forecast.

! ABCFF : holds coefficients for various absorption bands.  You can see
!         where they are set in GFDLRD.F.

! LTOP  : a 3-element integer array holding the model layer that is at or
!         immediately below the specified pressure levels for the tops 
!         of "high" (15000 Pa), "middle" (35000 Pa), and "low" (64200 Pa) 
!         stratiform clouds.  These are for the diagnostic cloud layers 
!         needed in the output but not in the integration.

! R1     : earth-sun distance in astronomical units.

! NRADS : integer number of fundamental timesteps (our smallest
!         timestep, i.e., the one for inertial gravity wave adjustment) 
!         between updates of the shortwave tendencies.  Currently we 
!         update the shortwave every hour.

! NRADL : integer number of fundamental timesteps between updates of
!         the longwave tendencies.  Currently we update the longwave 
!         every two hours.

! NTSD   : integer counter of the fundamental timesteps that have
!         elapsed since the start of the forecast.
!
!**********************************************************************
!****************************** NOTE **********************************
!**********************************************************************
!*** DUE TO THE RESETTING OF CONVECTIVE PRECIP AND CONVECTIVE CLOUD
!*** TOPS AND BOTTOMS, SHORTWAVE MUST NOT BE CALLED LESS FREQUENTLY
!*** THAN LONGWAVE.
!**********************************************************************
!****************************** NOTE **********************************
!**********************************************************************
!-----------------------------------------------------------------------
!     INTEGER, PARAMETER         :: NL=81
      LOGICAL, INTENT(IN)        :: cncld
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,         &
     &                              ims,ime, jms,jme, kms,kme ,         &
     &                              its,ite, jts,jte, kts,kte
      INTEGER, INTENT(IN)        :: NRADS,NRADL,NTSD,NPHS 
!     LOGICAL, INTENT(IN)        :: RESTRT
      REAL   , INTENT(IN)        :: DT
!     REAL   , INTENT(IN), DIMENSION(37,NL) :: XDUO3N,XDO3N2,XDO3N3,XDO3N4
      INTEGER, INTENT(IN), DIMENSION(3) :: IDAT
!-----------------------------------------------------------------------
      REAL,    PARAMETER :: CAPA=R_D/CP,DTR=3.1415926/180.
      INTEGER            :: LM1,LP1,LM
      INTEGER, INTENT(IN)               :: IHRST
!     REAL,    INTENT(IN), DIMENSION(NL)    :: PRGFDL
!
      REAL,    PARAMETER :: ALPHA0=100.,CLFRMIN=0.1,CUPRATE=24.*1000.,  &
     &                      EPS=R_D/R_V,EPSO3=1.E-10,                   &
     &                      EPSQ=1.E-12,EPSQ1=1.E-5,                    &
     &                      GAMMA=0.49,H0=0.,H1=1.,H69=-6.9,HPINC=1.E1, &
     &                      PBOT=10000.0,PEXP=0.25,                     &
     &                      QCLDMIN=EPSQ,RLAG=14.8125,                  &
     &                      STBOL=STBOLT,T_ICE=-10.
!
      INTEGER, PARAMETER :: NB=12,KSMUD=0
      INTEGER,PARAMETER :: K15=SELECTED_REAL_KIND(15)
      REAL (KIND=K15) :: DDX,EEX,PROD
!     REAL, INTENT(IN) :: SKO3R,AB15WD,SKC1R,SKO2D
!-----------------------------------------------------------------------
      LOGICAL :: SHORT,LONG
      LOGICAL :: BITX,BITY,BITZ,BITW,BIT1,BIT2,BITC,BITS,BITCP1,BITSP1
      LOGICAL :: NEW_CLOUD
!-----------------------------------------------------------------------
      REAL, INTENT(IN), DIMENSION(ims:ime,jms:jme) :: XLAND,TSK2D
      REAL, INTENT(IN), DIMENSION(its:ite, kms:kme, jts:jte):: Q,QCW,   &
     &                                                         QICE,T
      REAL, INTENT(IN), DIMENSION(its:ite, kms:kme, jts:jte):: PFLIP,   &
     &                                                         P8WFLIP

!     REAL, INTENT(IN), DIMENSION(28,180) :: TABLE1,TABLE2,TABLE3,EM3,EM1,EM1WDE
      REAL, INTENT(OUT), DIMENSION(ims:ime, jms:jme):: GLW,GSW,CZMEAN   &
     &                                                ,RSWIN,RLWIN      & !Added
     &                                                ,CFRACL,CFRACM    &
     &                                                ,CFRACH

!     REAL,   INTENT(IN), DIMENSION(kms:kme)   :: ETAD
!     REAL,   INTENT(IN), DIMENSION(kms:kme)   :: AETA
!-----------------------------------------------------------------------
      REAL,   INTENT(INOUT), DIMENSION(ims:ime,jms:jme)  :: HTOP,HBOT
      REAL,   INTENT(IN   ), DIMENSION(ims:ime,jms:jme)  :: ALB,SNO
      REAL,   INTENT(IN   ), DIMENSION(ims:ime,jms:jme)  :: GLAT,GLON
!-----------------------------------------------------------------------
      REAL,   DIMENSION(ims:ime,jms:jme)  :: CZEN
!#$   REAL,   DIMENSION(its:ite,jts:jte)  :: CZMEAN,SIGT4
      REAL,   DIMENSION(its:ite,jts:jte)  :: SIGT4
       INTEGER, DIMENSION(its:ite, jts:jte):: LMH
!-----------------------------------------------------------------------
      REAL,   INTENT(INOUT), DIMENSION(ims:ime,jms:jme) :: CUPPT
!     REAL,   DIMENSION(37*kte)          :: RAD1,RAD2,RAD3,RAD4
!-----------------------------------------------------------------------
!     INTEGER,INTENT(IN), DIMENSION(jms:jme) :: IHE,IHW
!-----------------------------------------------------------------------
      REAL,   INTENT(INOUT), DIMENSION(ims:ime,jms:jme) :: ACFRCV,ACFRST &
                                                          ,RSWTOA,RLWTOA
      INTEGER,INTENT(INOUT), DIMENSION(ims:ime,jms:jme) :: NCFRCV,NCFRST
!-----------------------------------------------------------------------
!#$   REAL,   DIMENSION(its:ite,jts:jte) :: RLWIN,RLWOUT
      REAL,   DIMENSION(its:ite,jts:jte) :: RLWOUT
!-----------------------------------------------------------------------
      REAL,   INTENT(IN),   DIMENSION(ims:ime,jms:jme) :: VEGFRC
      REAL,   INTENT(INOUT),DIMENSION(its:ite,kts:kte,jts:jte) :: TENDL,&
     &                                                            TENDS 
!#$   REAL,   DIMENSION(its:ite,jts:jte) :: RSWIN,RSWOUT,RSWTOA
!#$   REAL,   DIMENSION(its:ite,jts:jte) :: RSWIN,RSWOUT
      REAL,   DIMENSION(its:ite,jts:jte) :: RSWOUT
      REAL,   DIMENSION(its:ite,kts:kte,jts:jte):: RSWTT,RLWTT
!-----------------------------------------------------------------------
      REAL :: CTHK(3)
      DATA CTHK/20000.0,20000.0,20000.0/

      REAL    :: PTOPC(4)
      REAL,DIMENSION(10),SAVE :: CC,PPT
!-----------------------------------------------------------------------
      REAL,SAVE :: ABCFF(NB)
      INTEGER,DIMENSION(its:ite,jts:jte) :: LVL
      REAL,   DIMENSION(its:ite, jts:jte):: PDSL,FNE,FSE,TL
      REAL,   DIMENSION(  0:kte)  :: CLDAMT
      REAL,   DIMENSION(its:ite,3):: CLDCFR
      INTEGER,   DIMENSION(its:ite,3):: MBOT,MTOP
      REAL,   DIMENSION(its:ite)  :: PSFC,TSKN,ALBEDO,XLAT,COSZ,        &
     &                               SLMSK,FLWUP,                       &
     &                               FSWDN,FSWUP,FSWDNS,FSWUPS,FLWDNS,  &
     &                               FLWUPS

      REAL,   DIMENSION(its:ite,kts:kte) :: PMID,TMID
      REAL,   DIMENSION(its:ite,kts:kte) :: QMID,THMID,OZN,POZN
      REAL,   DIMENSION(its:ite,jts:jte) :: TOT 

      REAL,   DIMENSION(its:ite,kts:kte+1) :: PINT,EMIS,CAMT
      INTEGER,DIMENSION(its:ite,kts:kte+1) :: KBTM,KTOP
      INTEGER,DIMENSION(its:ite)   :: NCLDS,KCLD 
      REAL,   DIMENSION(its:ite)   :: TAUDAR
      REAL,   DIMENSION(its:ite,NB,kts:kte+1) ::RRCL,TTCL

      REAL,   DIMENSION(its:ite,kts:kte):: CSMID,CCMID,QWMID,QIMID
      REAL,SAVE :: PLOMD,PMDHI,PHITP,P400,PLBTM
      INTEGER,SAVE :: NFILE

!-----------------------------------------------------------------------
      REAL    :: CLSTP,TIME,DAYI,HOUR,ADDL,RANG,RSIN1,RCOS1,RCOS2
      REAL    :: TIMES,EXNER,APES,SNOFAC,CCLIMIT,CLIMIT,P1,P2,CC1,CC2
      REAL    :: PMOD,CLFR1,CTAU,WV,ARG,CLDMAX
      REAL    :: CL1,CL2,CR1,DPCL,QSUM,PRS1,PRS2,DELP,TCLD,DD,EE,AA,FF
      REAL    :: BB,GG,DENOM,FCTRA,FCTRB,PDSLIJ,CFRAVG,SNOMM
      REAL    :: TAUC,THICK,CONVPRATE,CLFR,ESAT,QSAT,RHUM,QCLD,RHGRID
      INTEGER :: I,J,MYJS,MYJE,MYIS,MYIE,NTSPH,NRADPP,ITIMSW,ITIMLW,    &
     &           JD,II
      INTEGER :: L,N,LML,LVLIJ,IR,KNTLYR,LL,NC,L400,NMOD,LTROP,IWKL
      INTEGER :: LCNVB,LCNVT
      INTEGER :: NLVL,MALVL,LLTOP,LLBOT,KBT2,KTH1,KBT1,KTH2,KTOP1,KFLIP
      INTEGER :: NBAND,NCLD,LBASE,NKTP,NBTM,KS,MYJS1,MYJS2,MYJE2,MYJE1

!     REAL,DIMENSION(5040):: T1,T2,T4,EM1V,EM1VW,EM3V

!     EQUIVALENCE (EM1V(1),EM1(1,1)),(EM1VW(1),EM1WDE(1,1)) 
!     EQUIVALENCE (EM3V(1),EM3(1,1))
!     EQUIVALENCE (T1(1),TABLE1(1,1)),(T2(1),TABLE2(1,1)), &
!                 (T4(1),TABLE3(1,1))

      DATA    PLOMD/64200./,PMDHI/35000./,PHITP/15000./,P400/40000./,   &
              PLBTM/105000./
      DATA    NFILE/14/
      DATA    CC/0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/
      DATA    PPT/0.,.14,.31,.70,1.6,3.4,7.7,17.,38.,85./
      DATA ABCFF/2*4.0E-5,.002,.035,.377,1.95,9.40,44.6,190.,989.,      &
     &           2706.,39011./

!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      MYJS=jts
      MYJE=jte
      MYIS=its
      MYIE=ite
      MYJS1=jts !????
      MYJE1=jte
      MYJS2=jts
      MYJE2=jte
      LM=kte
      LM1=LM-1
      LP1=LM+1
!     CNCLD=.TRUE.
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        LMH(I,J)=KME-1
        LVL(I,J)=0
      ENDDO
      ENDDO
!***  
!***  ASSIGN THE PRESSURES FOR CLOUD DOMAIN BOUNDARIES
!***
      PTOPC(1)=PLBTM
      PTOPC(2)=PLOMD
      PTOPC(3)=PMDHI
      PTOPC(4)=PHITP
!**********************************************************************
!***  THE FOLLOWING CODE IS EXECUTED EACH TIME THE RADIATION IS CALLED.
!**********************************************************************
!----------------------CONVECTION--------------------------------------
!  NRADPP IS THE NUMBER OF TIME STEPS TO ACCUMULATE CONVECTIVE PRECIP
!     FOR RADIATION
!   NOTE: THIS WILL NOT WORK IF NRADS AND NRADL ARE DIFFERENT UNLESS
!         THEY ARE INTEGER MULTIPLES OF EACH OTHER
!  CLSTP IS THE NUMBER OF HOURS OF THE ACCUMULATION PERIOD
!
      NTSPH=NINT(3600./DT)
      NRADPP=MIN(NRADS,NRADL)
      CLSTP=1.0*NRADPP/NTSPH
      CONVPRATE=CUPRATE/CLSTP
!GFDL      RHGRID=RHGRD
      RHGRID=RHgrd_in   !GFDL => simple right now w/o height-dependencies
!----------------------CONVECTION--------------------------------------
!***
!***  STATE WHETHER THE SHORT OR LONGWAVE COMPUTATIONS ARE TO BE DONE.
!***
      SHORT=.TRUE. 
      LONG=.TRUE. 
      ITIMSW=0
      ITIMLW=0
      IF(SHORT)ITIMSW=1
      IF(LONG) ITIMLW=1
!***
!***  FIND THE MEAN COSINE OF THE SOLAR ZENITH ANGLE 
!***  BETWEEN THE CURRENT TIME AND THE NEXT TIME RADIATION IS
!***  CALLED.  ONLY AVERAGE IF THE SUN IS ABOVE THE HORIZON.
!***
      TIME=NTSD*DT
!     CALL ZENITH(TIME,DAYI,HOUR)
!-----------------------------------------------------------------------
      CALL ZENITH(TIME,DAYI,HOUR,IDAT,IHRST,GLON,GLAT,CZEN,             &
     &            MYIS,MYIE,MYJS,MYJE,                                  &
     &            ids,ide, jds,jde, kds,kde,                            &
     &            ims,ime, jms,jme, kms,kme,                            &
     &            its,ite, jts,jte, kts,kte                         )
!-----------------------------------------------------------------------

      JD=INT(DAYI+0.50)
      ADDL=0.
      IF(MOD(IDAT(3),4).EQ.0)ADDL=1.
      RANG=PI2*(DAYI-RLAG)/(365.25+ADDL)
      RSIN1=SIN(RANG)
      RCOS1=COS(RANG)
      RCOS2=COS(2.*RANG)
!
!-----------------------------------------------------------------------
      IF(SHORT)THEN
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          CZMEAN(I,J)=0.
          TOT(I,J)=0.
        ENDDO
        ENDDO
!
        DO II=0,NRADS,NPHS
          TIMES=NTSD*DT+II*DT
          CALL ZENITH(TIMES,DAYI,HOUR,IDAT,IHRST,GLON,GLAT,CZEN,        &
     &                MYIS,MYIE,MYJS,MYJE,                              &
     &                ids,ide, jds,jde, kds,kde,                        &
     &                ims,ime, jms,jme, kms,kme,                        &
     &                its,ite, jts,jte, kts,kte                       )
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            IF(CZEN(I,J).GT.0.)THEN
              CZMEAN(I,J)=CZMEAN(I,J)+CZEN(I,J)
              TOT(I,J)=TOT(I,J)+1.
            ENDIF
!
          ENDDO
          ENDDO
        ENDDO
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          IF(TOT(I,J).GT.0.)CZMEAN(I,J)=CZMEAN(I,J)/TOT(I,J)
!
        ENDDO
        ENDDO
!
!***  MODIFY CZEN TO BE AT THE TOP OF THE HOUR.
!
!       TIMES=NTSD*DT
!       CALL ZENITH(TIMES,DAYI,HOUR,IDAT,IHRST,GLON,GLAT,CZEN,          &
!    &              MYIS,MYIE,MYJS,MYJE,                                &
!    &              ids,ide, jds,jde, kds,kde,                          &
!    &              ims,ime, jms,jme, kms,kme,                          &
!    &              its,ite, jts,jte, kts,kte                         )
      ENDIF
!-----------------------------------------------------------------------
!
!***********************************************************************
!***  THIS IS THE BEGINNING OF THE PRIMARY LOOP THROUGH THE DOMAIN
!***********************************************************************
!                        *********************
                         DO 700 J = MYJS, MYJE
!                        *********************
!
      DO 125 L=1,LM
      DO I=MYIS,MYIE
!       IR=IRAD(I)
        TMID(I,L)=T(I,1,J)
        QMID(I,L)=EPSQ
        QWMID(I,L)=0.
        QIMID(I,L)=0.
        CSMID(I,L)=0.
        CCMID(I,L)=0.
        OZN(I,L)=EPSO3
        TENDS(I,L,J)=0.
        TENDL(I,L,J)=0.
      ENDDO
  125 CONTINUE
!
      DO 140 N=1,3
      DO I=MYIS,MYIE
        CLDCFR(I,N)=0.
        MTOP(I,N)=0
        MBOT(I,N)=0
      ENDDO
  140 CONTINUE
!***
!***  FILL IN WORKING ARRAYS WHERE VALUES AT L=LM ARE THOSE THAT
!***  ARE ACTUALLY AT ETA LEVEL L=LMH.
!***
      DO 200 I=MYIS,MYIE
!     IR=IRAD(I)
      LML=LMH(I,J)
      LVLIJ=LVL(I,J)
!
      DO L=1,LML
        PMID(I,L+LVLIJ)=PFLIP(I,L,J)
        PINT(I,L+LVLIJ+1)=P8WFLIP(I,L+1,J)
        EXNER=(1.E5/PMID(I,L+LVLIJ))**CAPA
        TMID(I,L+LVLIJ)=T(I,L,J)
        THMID(I,L+LVLIJ)=T(I,L,J)*EXNER
        QMID(I,L+LVLIJ)=Q(I,L,J)
!--- Note that rain is ignored, only effects from cloud water and ice are considered
        QWMID(I,L+LVLIJ)=QCW(I,L,J)
        QIMID(I,L+LVLIJ)=QICE(I,L,J)
      ENDDO
!***
!***  FILL IN ARTIFICIAL VALUES ABOVE THE TOP OF THE DOMAIN.
!***  PRESSURE DEPTHS OF THESE LAYERS IS 1 HPA.
!***  TEMPERATURES ABOVE ARE ALREADY ISOTHERMAL WITH (TRUE) LAYER 1.
!***
      IF(LVLIJ.GT.0)THEN
        KNTLYR=0
!
        DO L=LVLIJ,1,-1
          KNTLYR=KNTLYR+1
          PMID(I,L)=P8WFLIP(I,1,J)-REAL(2*KNTLYR-1)*0.5*HPINC
          PINT(I,L+1)=PMID(I,L)+0.5*HPINC
          EXNER=(1.E5/PMID(I,L))**CAPA
          THMID(I,L)=TMID(I,L)*EXNER
        ENDDO
      ENDIF
!
      IF(LVLIJ.EQ.0) THEN
         PINT(I,1)=P8WFLIP(I,1,J)
      ELSE
         PINT(I,1)=PMID(I,1)-0.5*HPINC
      ENDIF
  200 CONTINUE
!***
!***  FILL IN THE SURFACE PRESSURE, SKIN TEMPERATURE, GEODETIC LATITUDE,
!***  ZENITH ANGLE, SEA MASK, AND ALBEDO.  THE SKIN TEMPERATURE IS
!***  NEGATIVE OVER WATER.
!***
      DO 250 I=MYIS,MYIE
      PSFC(I)=P8WFLIP(I,KME,J)
      APES=(PSFC(I)*1.E-5)**CAPA
!     TSKN(I)=THS(I,J)*APES*(1.-2.*SM(I,J))
      IF((XLAND(I,J)-1.5).GT.0.)THEN
        TSKN(I)=-TSK2D(I,J)
      ELSE
        TSKN(I)=TSK2D(I,J)
      ENDIF

!     TSKN(I)=THS(I,J)*APES*(1.-2.*(XLAND(I,J)-1.))
!     SLMSK(I)=SM(I,J)
      SLMSK(I)=XLAND(I,J)-1.
!
!     SNO(I,J)=AMAX1(SNO(I,J),0.)
      SNOMM=AMAX1(SNO(I,J),0.)
      SNOFAC=AMIN1(SNOMM/0.02, 1.0)
!!!!  ALBEDO(I)=ALB(I,J)+(1.0-0.01*VEGFRC(I,J))*SNOFAC*(SNOALB-ALB(I,J))
      ALBEDO(I)=ALB(I,J)
!
      XLAT(I)=GLAT(I,J)/DTR
      COSZ(I)=CZMEAN(I,J)
  250 CONTINUE
!-----------------------------------------------------------------------
!---  COMPUTE GRID-SCALE CLOUD COVER FOR RADIATION
!     (modified by Ferrier, Feb '02)
!
!---  Cloud fraction parameterization follows Randall, 1994
!     (see Hong et al., 1998)
!-----------------------------------------------------------------------
!
      DO I=MYIS,MYIE
        LML=LMH(I,J)
        LVLIJ=LVL(I,J)
        DO L=1,LML
          LL=L+LVLIJ
!
!--- Water vapor mixing ratio
!
          WV=QMID(I,LL)/(1.-QMID(I,LL))
!
!--- Saturation vapor pressure w/r/t water ( >=0C ) or ice ( <0C )
!
          ESAT=1000.*FPVS(TMID(I,LL))         !--- Saturation vapor pressure (Pa)
          QSAT=EPS*ESAT/(PMID(I,LL)-ESAT)     !--- Saturation mixing ratio
          RHUM=WV/QSAT                        !--- Relative humidity
!
!--- Total "cloud" mixing ratio, QCLD.  Rain is not part of cloud,
!    only cloud water + cloud ice + snow
!
          QCLD=QWMID(I,LL)+QIMID(I,LL)
!
!--- Determine cloud fraction (modified from original algorithm)
!
          IF (QCLD .LT. QCLDMIN) THEN
!
!--- Assume zero cloud fraction if there is no cloud mixing ratio
!
            CLFR=H0
          ELSEIF(RHUM.GE.RHGRID)THEN
!
!--- Assume cloud fraction of unity if near saturation and the cloud
!    mixing ratio is at or above the minimum threshold
!
            CLFR=H1
          ELSE
!
!--- Adaptation of original algorithm (Randall, 1994; Zhao, 1995)
!    modified based on assumed grid-scale saturation at RH=RHgrid.
!
            DENOM=(RHGRID*QSAT-WV)**GAMMA
            ARG=MAX(H69, -ALPHA0*QCLD/DENOM)    ! <-- EXP(-6.9)=.001
            CLFR=(RHUM/RHGRID)**PEXP*(1.-EXP(ARG))
!!              ARG=-1000*QCLD/(RHUM-RHGRID)
!!              ARG=MAX(ARG, ARGMIN)
!!              CLFR=(RHUM/RHGRID)*(1.-EXP(ARG))
            IF (CLFR .LT. .01) CLFR=0.
          ENDIF          !--- End IF (QCLD .LT. QCLDmin) ...
          CSMID(I,LL)=MIN(H1,CLFR)
        ENDDO            !--- End DO L ...
      ENDDO                !--- End DO I ...
!***********************************************************************
!********************END OF STRATIFORM CLOUD SECTION********************
!***********************************************************************
!
!-----------------------------------------------------------------------
!---  COMPUTE CONVECTIVE CLOUD COVER FOR RADIATION
!
!--- The parameterization of Slingo (1987, QJRMS, Table 1, p. 904) is
!    used for convective cloud fraction as a function of precipitation
!    rate.  Cloud fractions have been increased by 20% for each rainrate
!    interval so that shallow, nonprecipitating convection is ascribed a
!    constant cloud fraction of 0.1  (Ferrier, Feb '02).
!-----------------------------------------------------------------
!
      IF (CNCLD) THEN
        DO I=MYIS,MYIE
!
!***  CLOUD TOPS AND BOTTOMS COME FROM CUCNVC.
!***  CONVECTIVE CLOUDS NEED TO BE AT LEAST 2 MODEL LAYERS THICK.
!
          IF (HBOT(I,J)-HTOP(I,J) .GT. 1.0) THEN
!--- Compute convective cloud fractions if appropriate  (Ferrier, Feb '02)
            CLFR=CC(1)
            PMOD=CUPPT(I,J)*CONVPRATE
            IF (PMOD .GT. PPT(1)) THEN
              DO NC=1,10
                IF(PMOD.GT.PPT(NC)) NMOD=NC
              ENDDO
              IF (NMOD .GE. 10) THEN
                CLFR=CC(10)
              ELSE
                CC1=CC(NMOD)
                CC2=CC(NMOD+1)
                P1=PPT(NMOD)
                P2=PPT(NMOD+1)
                CLFR=CC1+(CC2-CC1)*(PMOD-P1)/(P2-P1)
              ENDIF      !--- End IF (NMOD .GE. 10) ...
              CLFR=MIN(H1, CLFR)
            ENDIF        !--- End IF (PMOD .GT. PPT(1)) ...
!
!***  ADD LVL TO BE CONSISTENT WITH OTHER WORKING ARRAYS
!
            LVLIJ=LVL(I,J)
            LCNVT=NINT(HTOP(I,J))+LVLIJ
            LCNVT=MIN(LM,LCNVT)
            LCNVB=NINT(HBOT(I,J))+LVLIJ
            LCNVB=MIN(LM,LCNVB)
!
            DO LL=LCNVT,LCNVB
              CCMID(I,LL)=CLFR
            ENDDO
          ENDIF    !--- IF (HBOT(I,J)-HTOP(I,J) .GT. 1.0) ...
        ENDDO      !--- End DO I loop
      ENDIF        !--- End IF (CNCLD) ...
!*********************************************************************
!*****************END OF CONVECTIVE CLOUD SECTION*****************
!*********************************************************************
!***
!***  DETERMINE THE FRACTIONAL CLOUD COVERAGE FOR HIGH, MID
!***  AND LOW OF CLOUDS FROM THE CLOUD COVERAGE AT EACH LEVEL
!***
!***  NOTE: THIS IS FOR DIAGNOSTICS ONLY!!!
!***
!***
       DO 500 I=MYIS,MYIE
!!
       DO L=0,LM
         CLDAMT(L)=0.
       ENDDO
!!  
!!***  NOW GOES LOW, MIDDLE, HIGH
!!
       DO 480 NLVL=1,3
       CLDMAX=0.
       MALVL=LM
       LLTOP=LM+1-LTOP(NLVL)+LVL(I,J)
!!***
!!***  GO TO THE NEXT CLOUD LAYER IF THE TOP OF THE CLOUD-TYPE IN
!!***  QUESTION IS BELOW GROUND OR IS IN THE LOWEST LAYER ABOVE GROUND.
!!***
       IF(LLTOP.GE.LM)GO TO 480
!!
       IF(NLVL.GT.1)THEN
         LLBOT=LM+1-LTOP(NLVL-1)-1+LVL(I,J)
         LLBOT=MIN(LLBOT,LM1)
       ELSE
         LLBOT=LM1
       ENDIF
!!
       DO 435 L=LLTOP,LLBOT
       CLDAMT(L)=AMAX1(CSMID(I,L),CCMID(I,L))
       IF(CLDAMT(L).GT.CLDMAX)THEN
         MALVL=L
         CLDMAX=CLDAMT(L)
       ENDIF
   435 CONTINUE
!!*********************************************************************
!! NOW, CALCULATE THE TOTAL CLOUD FRACTION IN THIS PRESSURE DOMAIN
!! USING THE METHOD DEVELOPED BY Y.H., K.A.C. AND A.K. (NOV., 1992).
!! IN THIS METHOD, IT IS ASSUMED THAT SEPERATED CLOUD LAYERS ARE
!! RADOMLY OVERLAPPED AND ADJACENT CLOUD LAYERS ARE MAXIMUM OVERLAPPED.
!! VERTICAL LOCATION OF EACH TYPE OF CLOUD IS DETERMINED BY THE THICKEST
!! CONTINUING CLOUD LAYERS IN THE DOMAIN.
!!*********************************************************************
       CL1=0.0
       CL2=0.0
       KBT1=LLBOT
       KBT2=LLBOT
       KTH1=0
       KTH2=0
!!
       DO 450 LL=LLTOP,LLBOT
       L=LLBOT-LL+LLTOP
       BIT1=.FALSE.
       CR1=CLDAMT(L)
       BITX=(PINT(I,L).GE.PTOPC(NLVL+1)).AND.                           &
      &     (PINT(I,L).LT.PTOPC(NLVL)).AND.                             &
      &     (CLDAMT(L).GT.0.0)
       BIT1=BIT1.OR.BITX
       IF(.NOT.BIT1)GO TO 450
!!***
!!***  BITY=T: FIRST CLOUD LAYER; BITZ=T:CONSECUTIVE CLOUD LAYER
!!***  NOTE:  WE ASSUME THAT THE THICKNESS OF EACH CLOUD LAYER IN THE
!!***         DOMAIN IS LESS THAN 200 MB TO AVOID TOO MUCH COOLING OR
!!***         HEATING. SO WE SET CTHK(NLVL)=200*E2. BUT THIS LIMIT MAY
!!***         WORK WELL FOR CONVECTIVE CLOUDS. MODIFICATION MAY BE
!!***         NEEDED IN THE FUTURE.
!!***
       BITY=BITX.AND.(KTH2.LE.0)
       BITZ=BITX.AND.(KTH2.GT.0)
!!
       IF(BITY)THEN
         KBT2=L
         KTH2=1
       ENDIF
!!
       IF(BITZ)THEN
         KTOP1=KBT2-KTH2+1
         DPCL=PMID(I,KBT2)-PMID(I,KTOP1)
         IF(DPCL.LT.CTHK(NLVL))THEN
           KTH2=KTH2+1
         ELSE
           KBT2=KBT2-1
         ENDIF
       ENDIF
       IF(BITX)CL2=AMAX1(CL2,CR1)
!!***
!!*** AT THE DOMAIN BOUNDARY OR SEPARATED CLD LAYERS, RANDOM OVERLAP.
!!*** CHOOSE THE THICKEST OR THE LARGEST FRACTION AMT AS THE CLD
!!*** LAYER IN THAT DOMAIN.
!!***
       BIT2=.FALSE.
       BITY=BITX.AND.(CLDAMT(L-1).LE.0.0.OR. &
            PINT(I,L-1).LT.PTOPC(NLVL+1))
       BITZ=BITY.AND.CL1.GT.0.0
       BITW=BITY.AND.CL1.LE.0.0
       BIT2=BIT2.OR.BITY
       IF(.NOT.BIT2)GO TO 450
!!
       IF(BITZ)THEN
         KBT1=INT((CL1*KBT1+CL2*KBT2)/(CL1+CL2))
         KTH1=INT((CL1*KTH1+CL2*KTH2)/(CL1+CL2))+1
         CL1=CL1+CL2-CL1*CL2
       ENDIF
!!
       IF(BITW)THEN
         KBT1=KBT2
         KTH1=KTH2
         CL1=CL2
       ENDIF
!!
       IF(BITY)THEN
         KBT2=LLBOT
         KTH2=0
         CL2=0.0
       ENDIF
   450 CONTINUE
!
       CLDCFR(I,NLVL)=AMIN1(1.0,CL1)
       MTOP(I,NLVL)=MIN(KBT1,KBT1-KTH1+1)
       MBOT(I,NLVL)=KBT1
   480 CONTINUE
   500 CONTINUE

!***
!***  SET THE UN-NEEDED TAUDAR TO ONE
!***
      DO I=MYIS,MYIE
        TAUDAR(I)=1.0
      ENDDO
!----------------------------------------------------------------------
! NOW, CALCULATE THE CLOUD RADIATIVE PROPERTIES AFTER DAVIS (1982),
! HARSHVARDHAN ET AL (1987) AND Y.H., K.A.C. AND A.K. (1993).
! 
! UPDATE: THE FOLLOWING PARTS ARE MODIFIED, AFTER Y.T.H. (1994), TO 
!         CALCULATE THE RADIATIVE PROPERTIES OF CLOUDS ON EACH MODEL
!         LAYER. BOTH CONVECTIVE AND STRATIFORM CLOUDS ARE USED
!         IN THIS CALCULATIONS.
!
!                                     QINGYUN ZHAO   95-3-22
!
!----------------------------------------------------------------------
!
!***
!*** INITIALIZE ARRAYS FOR USES LATER
!***

      DO 600 I=MYIS,MYIE
      LML=LMH(I,J)
      LVLIJ=LVL(I,J)
!
!***
!*** NOTE: LAYER=1 IS THE SURFACE, AND LAYER=2 IS THE FIRST CLOUD
!***       LAYER ABOVE THE SURFACE AND SO ON.
!***
      EMIS(I,1)=1.0
      KTOP(I,1)=LP1
      KBTM(I,1)=LP1
      CAMT(I,1)=1.0
      KCLD(I)=2
!
      DO NBAND=1,NB
        RRCL(I,NBAND,1)=0.0
        TTCL(I,NBAND,1)=1.0
      ENDDO
!
      DO 510 L=2,LP1
      CAMT(I,L)=0.0
      KTOP(I,L)=1
      KBTM(I,L)=1
      EMIS(I,L)=0.0
!
      DO NBAND=1,NB
        RRCL(I,NBAND,L)=0.0
        TTCL(I,NBAND,L)=1.0
      ENDDO
  510 CONTINUE
!***
!*** NOW CALCULATE THE AMOUNT, TOP, BOTTOM AND TYPE OF EACH CLOUD LAYER
!*** CLOUD TYPE=1: STRATIFORM CLOUD
!***       TYPE=2: CONVECTIVE CLOUD
!*** WHEN BOTH CONVECTIVE AND STRATIFORM CLOUDS EXIST AT THE SAME POINT,
!*** SELECT CONVECTIVE CLOUD WITH THE HIGHER CLOUD FRACTION.
!*** CLOUD LAYERS ARE SEPARATED BY TOTAL ABSENCE OF CLOUDINESS.
!*** NOTE: THERE IS ONLY ONE CONVECTIVE CLOUD LAYER IN ONE COLUMN.
!*** KTOP AND KBTM ARE THE TOP AND BOTTOM OF EACH CLOUD LAYER IN TERMS
!*** OF MODEL LEVEL.
!***
      NEW_CLOUD=.TRUE.
!
      DO L=2,LML
        LL=LML-L+1+LVLIJ                        !-- Model layer
        CLFR=MAX(CCMID(I,LL),CSMID(I,LL))       !-- Cloud fraction in layer
        CLFR1=MAX(CCMID(I,LL+1),CSMID(I,LL+1))  !-- Cloud fraction in lower layer
!-------------------
        IF (CLFR .GE. CLFRMIN) THEN
!--- Cloud present at level
          IF (NEW_CLOUD) THEN
!--- New cloud layer
            IF(L==2.AND.CLFR1>=CLFRmin)THEN
              KBTM(I,KCLD(I))=LL+1
              CAMT(I,KCLD(I))=CLFR1
            ELSE
              KBTM(I,KCLD(I))=LL
              CAMT(I,KCLD(I))=CLFR
            ENDIF
            NEW_CLOUD=.FALSE.
          ELSE
!--- Existing cloud layer
            CAMT(I,KCLD(I))=AMAX1(CAMT(I,KCLD(I)), CLFR)
          ENDIF        ! End IF (NEW_CLOUD .EQ. 0) ...
        ELSE IF (CLFR1 .GE. CLFRMIN) THEN
!--- Cloud is not present at level but did exist at lower level, then ...
          IF (L .EQ. 2) THEN
!--- For the case of ground fog
            KBTM(I,KCLD(I))=LL+1
            CAMT(I,KCLD(I))=CLFR1
          ENDIF
          KTOP(I,KCLD(I))=LL+1
          NEW_CLOUD=.TRUE.
          KCLD(I)=KCLD(I)+1
          CAMT(I,KCLD(I))=0.0
        ENDIF
!-------------------
      ENDDO      !--- End DO L loop
!***
!*** THE REAL NUMBER OF CLOUD LAYERS IS (THE FIRST IS THE GROUND;
!*** THE LAST IS THE SKY):
!***
      NCLDS(I)=KCLD(I)-2
      NCLD=NCLDS(I)
!***
!***  NOW CALCULATE CLOUD RADIATIVE PROPERTIES
!***
      IF(NCLD.GE.1)THEN
!***
!*** NOTE: THE FOLLOWING CALCULATIONS, THE UNIT FOR PRESSURE IS MB!!!
!***
        DO 580 NC=2,NCLD+1
!
        TAUC=0.0   ! Total optical depth for each cloud layer
        QSUM=0.0
        NKTP=LP1
        NBTM=0
        BITX=CAMT(I,NC).GE.CLFRMIN
        NKTP=MIN(NKTP,KTOP(I,NC))
        NBTM=MAX(NBTM,KBTM(I,NC))
!
        DO LL=NKTP,NBTM
          IF(LL.GE.KTOP(I,NC).AND.LL.LE.KBTM(I,NC).AND.BITX)THEN
            PRS1=PINT(I,LL)*0.01
            PRS2=PINT(I,LL+1)*0.01
            DELP=PRS2-PRS1
            TCLD=TMID(I,LL)-273.16
            QSUM=QSUM+QMID(I,LL)*DELP*(PRS1+PRS2)                       &     
     &           /(120.1612*SQRT(TMID(I,LL)))
!
!--- The simple optical depth parameterization from eq. (1) of Harshvardhan
!    et al. (1989, JAS, p. 1924; hereafter referred to as HRCD by authorship)
!    is used for convective cloud properties with some simple changes.
!
!--- The optical depth Tau is Tau=CTau*DELP, where values of CTau are
!    described below.
!
!--- For convection, assume simple optical depth coefficients of
!      1) CTau=0.16 for ice, assumed to be for T<=T_ICE (=-10C in GSMCOLUMN)
!         This was referenced as "optically thick anvil associated with
!         convection in
!      2) CTau=0.08 for water, assumed to be present for T>T_ICE
!
!--- For grid-scale processes in the absence of convection:
!      1) CTau=0.08*min(1., Qc/Q0) for cloud water, where
!         Q0 is assumed to be the threshold mixing ratio for "thick anvils",
!         as noted in the 2nd paragraph after eq. (1) in Harshvardhan et al.
!         (1989).  A value of Q0=0.1 g/kg is assumed based on experience w/
!         cloud observations, and it is intended only to be a crude scaling
!         factor for "order of magnitude" effects.  The functional dependence
!         on mixing ratio is based on Stephens (1978, JAS, p. 2124, eq. 7).
!      2) CTau=500*Qi for ice particles.  This is based on the optical depth
!         of snow.  Prof. Q. Fu (U. Washington) provided the following eq.:
!           Tau-1.5*SWP/(Res*RHOs)
!         SWP is snow water path, Res is the snow effective radius, RHOs is
!         the snow density.  Based on derivations using Petch (1998, JAS, 1846-
!         1858) as a starting point, Res=1.5*Ds with Ds being the mean diameter
!         of an exponential distribution of ice particles ("snow").  After some
!         manipulation,
!           Tau=CTau*DELP => CTau=CSTau*Qice, where
!         CSTau=100./(G*Ds*RHOs) ~ 500 based on values of Ds and RHOs in the
!         ice lookup tables (actually varies from 920 for Ds=.1 mm to ~520 for
!         Ds>0.5 mm), and units of DELP in mb (must convert from Pascals).
!         "Snow" (precipitating ice) is assumed because of it dominates over
!         cloud ice in the scheme.
!
            CTAU=0.
            IF (CCMID(I,LL) .GE. CLFRMIN) THEN
!-- Crude convective cloud properties
              IF (TCLD .GT. T_ICE) THEN
                CTAU=0.08      !--- Cloud water
              ELSE
                CTAU=0.16      !--- Ice
              ENDIF
            ENDIF
!-- Crude grid-scale cloud properties
            IF (CSMID(I,LL) .GE. CLFRMIN)                               &
     &        CTAU=CTAU+800.*QWMID(I,LL)+500.*QIMID(I,LL)
            TAUC=TAUC+DELP*CTAU
          ENDIF      !--- End IF(LL.GE.KTOP(I,NC) ....
        ENDDO        !--- End DO LL
!
        IF(BITX)EMIS(I,NC)=1.0-EXP(-0.75*TAUC)
!GFDL => should consider using this =>        IF(BITX)EMIS(I,NC)=1.0-EXP(-1.66*TAUC)
        IF(QSUM.GE.EPSQ1)THEN
!
          DO 570 NBAND=1,NB
          IF(BITX)THEN
            PROD=ABCFF(NBAND)*QSUM
            DDX=TAUC/(TAUC+PROD)
            EEX=1.0-DDX
            IF(ABS(EEX).GE.1.E-8)THEN
              DD=DDX
              EE=EEX
              FF=1.0-DD*0.85
              AA=MIN(50.0,SQRT(3.0*EE*FF)*TAUC)
              AA=EXP(-AA)
              BB=FF/EE
              GG=SQRT(BB)
              DD=(GG+1.0)*(GG+1.0)-(GG-1.0)*(GG-1.0)*AA*AA
              RRCL(I,NBAND,NC)=MAX(0.1E-5,(BB-1.0)*(1.0-AA*AA)/DD)
              TTCL(I,NBAND,NC)=AMAX1(0.1E-5,4.0*GG*AA/DD)
            ENDIF
          ENDIF
  570     CONTINUE
        ENDIF
  580   CONTINUE
!
      ENDIF
!
  600 CONTINUE
!*********************************************************************
!******************  COMPUTE OZONE AT MIDLAYERS  *********************
!*********************************************************************
!
!***  MODIFY PRESSURES SO THAT THE ENTIRE COLUMN OF OZONE (TO 0 MB)
!***  IS INCLUDED IN THE MODEL COLUMN EVEN WHEN PT > 0 MB
!***
      DO L=1,LM
      DO I=MYIS,MYIE
        DENOM=1./(PINT(I,LP1)-PINT(I,1))
        FCTRA=PINT(I,LP1)*DENOM
        FCTRB=-PINT(I,1)*PINT(I,LP1)*DENOM
        POZN(I,L)=PMID(I,L)*FCTRA+FCTRB
      ENDDO
      ENDDO
!
      CALL OZON2D(LM,POZN,XLAT,RSIN1,RCOS1,RCOS2,OZN,              &
!                 XDUO3N,XDO3N4,XDO3N2,XDO3N3,                     &
!                 PRGFDL,MYIS,MYIE,                                &
                  MYIS,MYIE,                                       &
                  ids,ide, jds,jde, kds,kde,                       &
                  ims,ime, jms,jme, kms,kme,                       &
                  its,ite, jts,jte, kts,kte                        )
!
!***  
!***  NOW THE VARIABLES REQUIRED BY RADFS HAVE BEEN CALCULATED.
!***
!----------------------------------------------------------------------
!***
!***  CALL THE GFDL RADIATION DRIVER
!***
!***
      CALL RADFS &
     &     (PSFC,PMID,PINT,QMID,TMID,OZN,TSKN,SLMSK,ALBEDO,XLAT         &
     &,     CAMT,KTOP,KBTM,NCLDS,EMIS,RRCL,TTCL                         &
     &,     COSZ,TAUDAR,1                                               &
     &,     1,0                                                         &
     &,     ITIMSW,ITIMLW,JD,HOUR                                       &
     &,     TENDS(ITS,KTS,J),TENDL(ITS,KTS,J)                           &
     &,     FLWUP,FSWUP,FSWDN,FSWDNS,FSWUPS,FLWDNS,FLWUPS               &
     &,     ids,ide, jds,jde, kds,kde                                   &
     &,     ims,ime, jms,jme, kms,kme                                   &
     &,     its,ite, jts,jte, kts,kte                           )
!----------------------------------------------------------------------
      IF(LONG)THEN
        DO I=MYIS,MYIE
          GLW(I,J)=FLWDNS(I)
        ENDDO
      ENDIF
!
      IF(SHORT)THEN
        DO I=MYIS,MYIE
          GSW(I,J)=FSWDNS(I)-FSWUPS(I)
        ENDDO
      ENDIF
!
      DO 650 I=MYIS,MYIE
      CFRACL(I,J)=CLDCFR(I,1)
      CFRACM(I,J)=CLDCFR(I,2)
      CFRACH(I,J)=CLDCFR(I,3)
!     
!***  ARRAYS ACFRST AND ACFRCV ACCUMULATE AVERAGE STRATIFORM AND
!***  CONVECTIVE CLOUD FRACTIONS, RESPECTIVELY. 
!***  ACCUMLATE THESE VARIABLES ONLY ONCE PER RADIATION CALL.
!
!***  ASSUME RANDOM OVERLAP BETWEEN LOW, MIDDLE, & HIGH LAYERS.
!
      CFRAVG=1.-(1.-CFRACL(I,J))*(1.-CFRACM(I,J))*(1.-CFRACH(I,J))
!
      IF(CNCLD)THEN
        IF(HBOT(I,J)-HTOP(I,J).GT.1.)THEN
!--- Count locations with convective cloudiness
          ACFRST(I,J)=ACFRST(I,J)+CFRAVG
          NCFRST(I,J)=NCFRST(I,J)+1
        ENDIF
        BITS=.FALSE.
        DO LL=1,LM
          BITS=CSMID(I,LL).GE.CLFRMIN  !--- Existence of grid-scale cloud in layer
          IF(BITS)EXIT
        ENDDO
        IF(BITS)THEN
!--- Count locations with grid-scale cloudiness
          ACFRST(I,J)=ACFRST(I,J)+CFRAVG
          NCFRST(I,J)=NCFRST(I,J)+1
        ENDIF
      ELSE
!--- Count only locations with grid-scale cloudiness
        ACFRCV(I,J)=ACFRCV(I,J)+CFRAVG
        NCFRCV(I,J)=NCFRCV(I,J)+1
      ENDIF
  650 CONTINUE
!***
!***  COLLECT ATMOSPHERIC TEMPERATURE TENDENCIES DUE TO RADIATION.
!***  ALSO COLLECT THE TOTAL SW AND INCOMING LW RADIATION (W/M**2)
!***  AND CONVERT TO FORM NEEDED FOR PREDICTION OF THS IN SURFCE.
!***
      DO 660 I=MYIS,MYIE
      DO L=1,LM
        LL=LVL(I,J)+L
        IF(SHORT)RSWTT(I,L,J)=TENDS(I,LL,J)
        IF(LONG) RLWTT(I,L,J)=TENDL(I,LL,J)
        IF(LL.EQ.LM)GO TO 660
      ENDDO
  660 CONTINUE
!***
!***  SUM THE LW INCOMING AND SW RADIATION (W/M**2) FOR RADIN.
!***
      DO 675 I=MYIS,MYIE
      IF(LONG)THEN
        SIGT4(I,J)=STBOL*TMID(I,LM)*TMID(I,LM)* &
                   TMID(I,LM)*TMID(I,LM)
      ENDIF
!     
!***  ACCUMULATE VARIOUS LW AND SW RADIATIVE FLUXES FOR POST
!***  PROCESSOR.  PASSED VIA COMMON ACMRDL AND ACMRDS.
!
      IF(LONG)THEN
        RLWIN(I,J) =FLWDNS(I)
        RLWOUT(I,J)=FLWUPS(I)
        RLWTOA(I,J)=FLWUP(I)
      ENDIF
      IF(SHORT)THEN
        RSWIN(I,J) =FSWDNS(I)
        RSWOUT(I,J)=FSWUPS(I)
        RSWTOA(I,J)=FSWUP(I)
      ENDIF
  675 CONTINUE
!***
!***  THIS ROW IS FINISHED. GO TO NEXT
!***
!                        *********************
  700                          CONTINUE
!                        *********************
!----------------------------------------------------------------------
!***
!***  CALLS TO RADIATION THIS TIME STEP ARE COMPLETE.
!***
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!***
!***  HORIZONTAL SMOOTHING OF TEMPERATURE TENDENCIES
!***
!----------------------------------------------------------------------
      IF(SHORT) THEN
        DO 800 L=1,LM
!       CALL ZERO2(TL,  &                  
!                  ids,ide, jds,jde, kds,kde,                         &
!                  ims,ime, jms,jme, kms,kme,                         &
!                  its,ite, jts,jte, kts,kte                          )
!       CALL ZERO2(FNE, &
!                  ids,ide, jds,jde, kds,kde,                         &
!                  ims,ime, jms,jme, kms,kme,                         &
!                  its,ite, jts,jte, kts,kte                          )
!       CALL ZERO2(FSE, &
!                  ids,ide, jds,jde, kds,kde,                         &
!                  ims,ime, jms,jme, kms,kme,                         &
!                  its,ite, jts,jte, kts,kte                          )
!
!        IF(KSMUD.GE.1)THEN
!          DO 750 KS=1,KSMUD
!!
!          DO J=MYJS,MYJE
!          DO I=MYIS,MYIE
!            TL(I,J)=RSWTT(I,L,J)
!!           TL(I,J)=RSWTT(I,L,J)*HTM(I,L,J)
!          ENDDO
!          ENDDO
!!
!          DO J=MYJS,MYJE
!          DO I=MYIS,MYIE
!            FNE(I,J)=(TL(I+IHE(J),J+1)-TL(I,J)) 
!!                    *HTM(I,L,J)*HTM(I+IHE(J),J+1,L)
!          ENDDO
!          ENDDO
!!
!          DO J=MYJS1,MYJE
!          DO I=MYIS,MYIE
!            FSE(I,J)=(TL(I+IHE(J),J-1)-TL(I,J)) 
!!                    *HTM(I+IHE(J),J-1,L)*HTM(I,L,J)
!          ENDDO
!          ENDDO
!!
!          DO J=MYJS2,MYJE2
!          DO I=MYIS,MYIE
!            TL(I,J)=(FNE(I,J)-FNE(I+IHW(J),J-1)  &
!                    +FSE(I,J)-FSE(I+IHW(J),J+1)) &
!                    *0.125+TL(I,J)
!!                   *HBM2(I,J)*0.125+TL(I,J)
!          ENDDO
!          ENDDO
!!
!          DO J=MYJS,MYJE
!          DO I=MYIS,MYIE
!            RSWTT(I,L,J)=TL(I,J)
!          ENDDO
!          ENDDO
!!
!  750     CONTINUE
!        ENDIF
!
  800   CONTINUE
      ENDIF
!----------------------------------------------------------------------
!
      IF(LONG)THEN
!
        DO 900 L=1,LM
!       CALL ZERO2(TL, &
!                  ids,ide, jds,jde, kds,kde,                         &
!                  ims,ime, jms,jme, kms,kme,                         &
!                  its,ite, jts,jte, kts,kte                          )
!       CALL ZERO2(FNE, &
!                  ids,ide, jds,jde, kds,kde,                         &
!                  ims,ime, jms,jme, kms,kme,                         &
!                  its,ite, jts,jte, kts,kte                          )
!       CALL ZERO2(FSE, &
!                  ids,ide, jds,jde, kds,kde,                         &
!                  ims,ime, jms,jme, kms,kme,                         &
!                  its,ite, jts,jte, kts,kte                          )
!
!        IF(KSMUD.GE.1)THEN
!          DO 850 KS=1,KSMUD
!!
!          DO J=MYJS,MYJE
!          DO I=MYIS,MYIE
!            TL(I,J)=RLWTT(I,L,J)
!!           TL(I,J)=RLWTT(I,L,J)*HTM(I,L,J)
!          ENDDO
!          ENDDO
!!
!          DO J=MYJS,MYJE1
!          DO I=MYIS,MYIE
!            FNE(I,J)=(TL(I+IHE(J),J+1)-TL(I,J))  
!!                    *HTM(I,L,J)*HTM(I+IHE(J),J+1,L)
!          ENDDO
!          ENDDO
!!
!          DO J=MYJS1,MYJE
!          DO I=MYIS,MYIE
!            FSE(I,J)=(TL(I+IHE(J),J-1)-TL(I,J))  
!!                    *HTM(I+IHE(J),J-1,L)*HTM(I,L,J)
!          ENDDO
!          ENDDO
!!
!          DO J=MYJS2,MYJE2
!          DO I=MYIS,MYIE
!            TL(I,J)=(FNE(I,J)-FNE(I+IHW(J),J-1) &
!                    +FSE(I,J)-FSE(I+IHW(J),J+1)) &
!                    *0.125+TL(I,J)
!!                   *HBM2(I,J)*0.125+TL(I,J)
!          ENDDO
!          ENDDO
!!
!          DO J=MYJS,MYJE
!          DO I=MYIS,MYIE
!            RLWTT(I,L,J)=TL(I,J)
!          ENDDO
!          ENDDO
!!
!  850     CONTINUE
!        ENDIF
  900   CONTINUE
      ENDIF
!----------------------------------------------------------------------

      END SUBROUTINE RADTN

!----------------------------------------------------------------------

      SUBROUTINE ZENITH(TIMES,DAYI,HOUR,IDAT,IHRST,GLON,GLAT,CZEN,     &
                        MYIS,MYIE,MYJS,MYJE,                           &
                        IDS,IDE, JDS,JDE, KDS,KDE,                     &
                        IMS,IME, JMS,JME, KMS,KME,                     &
                        ITS,ITE, JTS,JTE, KTS,KTE)
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER, INTENT(IN)        :: IDS,IDE, JDS,JDE, KDS,KDE ,        &
                                    IMS,IME, JMS,JME, KMS,KME ,        &
                                    ITS,ITE, JTS,JTE, KTS,KTE
      INTEGER, INTENT(IN)        :: MYJS,MYJE,MYIS,MYIE

      REAL,    INTENT(IN)        :: TIMES
      REAL,    INTENT(OUT)       :: HOUR,DAYI
      INTEGER, INTENT(IN)        :: IHRST

      INTEGER, INTENT(IN), DIMENSION(3) :: IDAT 
      REAL,    INTENT(IN), DIMENSION(IMS:IME,JMS:JME) :: GLAT,GLON
      REAL,    INTENT(OUT), DIMENSION(IMS:IME,JMS:JME) :: CZEN

      REAL,    PARAMETER :: GSTC1=24110.54841,GSTC2=8640184.812866,    &
                            GSTC3=9.3104E-2,GSTC4=-6.2E-6,             &
                            PI=3.1415926,PI2=2.*PI,PIH=0.5*PI,         &
!#$                         DEG2RD=1.745329E-2,OBLIQ=23.440*DEG2RD,    &
                            DEG2RD=3.1415926/180.,OBLIQ=23.440*DEG2RD, &
                            ZEROJD=2451545.0

      REAL    :: DAY,YFCTR,ADDDAY,STARTYR,DATJUL,DIFJD,SLONM,   &
                 ANOM,SLON,DEC,RA,DATJ0,TU,STIM0,SIDTIM,HRANG
      REAL    :: HRLCL,SINALT
      INTEGER :: KMNTH,KNT,IDIFYR,J,I
      LOGICAL :: LEAP
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      INTEGER :: MONTH (12)
!-----------------------------------------------------------------------
      DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/
!***********************************************************************
!     SAVE MONTH
      DAY=0.
      LEAP=.FALSE.
      IF(MOD(IDAT(3),4).EQ.0)THEN
        MONTH(2)=29
        LEAP=.TRUE.
      ENDIF
      IF(IDAT(1).GT.1)THEN
        KMNTH=IDAT(1)-1
        DO 10 KNT=1,KMNTH
        DAY=DAY+REAL(MONTH(KNT))
   10   CONTINUE
      ENDIF
!***
!***  CALCULATE EXACT NUMBER OF DAYS FROM BEGINNING OF YEAR TO
!***  FORECAST TIME OF INTEREST 
!***
      DAY=DAY+REAL(IDAT(2)-1)+(REAL(IHRST)+TIMES/3600.)/24.
      DAYI=REAL(INT(DAY)+1)
      HOUR=(DAY-DAYI+1.)*24.
      YFCTR=2000.-IDAT(3)
!-----------------------------------------------------------------------
!***
!***  FIND CELESTIAL LONGITUDE OF THE SUN THEN THE SOLAR DECLINATION AND
!***  RIGHT ASCENSION.
!***
!-----------------------------------------------------------------------
      IDIFYR=IDAT(3)-2000
!***
!***  FIND JULIAN DATE OF START OF THE RELEVANT YEAR
!***  ADDING IN LEAP DAYS AS NEEDED
!***
      IF(IDIFYR.LT.0)THEN
        ADDDAY=REAL(IDIFYR/4)
      ELSE
        ADDDAY=REAL((IDIFYR+3)/4)
      ENDIF
      STARTYR=ZEROJD+IDIFYR*365.+ADDDAY-0.5
!***
!***  THE JULIAN DATE OF THE TIME IN QUESTION
!***
      DATJUL=STARTYR+DAY
!
!***  DIFFERENCE OF ACTUAL JULIAN DATE FROM JULIAN DATE
!***  AT 00H 1 January 2000
!
      DIFJD=DATJUL-ZEROJD
!
!***  MEAN GEOMETRIC LONGITUDE OF THE SUN
!
      SLONM=(280.460+0.9856474*DIFJD)*DEG2RD+YFCTR*PI2
!
!***  THE MEAN ANOMOLY
!
      ANOM=(357.528+0.9856003*DIFJD)*DEG2RD
!
!***  APPARENT GEOMETRIC LONGITUDE OF THE SUN
!
      SLON=SLONM+(1.915*SIN(ANOM)+0.020*SIN(2.*ANOM))*DEG2RD
      IF(SLON.GT.PI2)SLON=SLON-PI2
!
!***  DECLINATION AND RIGHT ASCENSION
! 
      DEC=ASIN(SIN(SLON)*SIN(OBLIQ))
      RA=ACOS(COS(SLON)/COS(DEC))
      IF(SLON.GT.PI)RA=PI2-RA
!***
!***  FIND THE GREENWICH SIDEREAL TIME THEN THE LOCAL SOLAR
!***  HOUR ANGLE.
!***
      DATJ0=STARTYR+DAYI-1.
      TU=(DATJ0-2451545.)/36525.
      STIM0=GSTC1+GSTC2*TU+GSTC3*TU**2+GSTC4*TU**3
      SIDTIM=STIM0/3600.+YFCTR*24.+1.00273791*HOUR
      SIDTIM=SIDTIM*15.*DEG2RD
      IF(SIDTIM.LT.0.)SIDTIM=SIDTIM+PI2
      IF(SIDTIM.GT.PI2)SIDTIM=SIDTIM-PI2
      HRANG=SIDTIM-RA
!
      DO 100 J=MYJS,MYJE
      DO 100 I=MYIS,MYIE
!     HRLCL=HRANG-GLON(I,J)
      HRLCL=HRANG+GLON(I,J)+PI2
!***
!***  THE ZENITH ANGLE IS THE COMPLEMENT OF THE ALTITUDE THUS THE
!***  COSINE OF THE ZENITH ANGLE EQUALS THE SINE OF THE ALTITUDE.
!***
      SINALT=SIN(DEC)*SIN(GLAT(I,J))+COS(DEC)*COS(HRLCL)* &
       COS(GLAT(I,J))
      IF(SINALT.LT.0.)SINALT=0.
      CZEN(I,J)=SINALT
  100 CONTINUE
!***
!***  IF THE FORECAST IS IN A DIFFERENT YEAR THAN THE START TIME,
!***  RESET DAYI TO THE PROPER DAY OF THE NEW YEAR (IT MUST NOT BE
!***  RESET BEFORE THE SOLAR ZENITH ANGLE IS COMPUTED).
!***
      IF(DAYI.GT.365.)THEN
        IF(.NOT.LEAP)THEN
          DAYI=DAYI-365.
        ELSEIF(LEAP.AND.DAYI.GT.366.)THEN
          DAYI=DAYI-366.
        ENDIF
      ENDIF
!
      END SUBROUTINE ZENITH
!-----------------------------------------------------------------------

  SUBROUTINE OZON2D (LK,POZN,XLAT,RSIN1,RCOS1,RCOS2,QO3,              &
!                    XDUO3N,XDO3N4,XDO3N2,XDO3N3,                     &
!                    PRGFDL,MYIS,MYIE,                                &
                     MYIS,MYIE,                                       &
                     ids,ide, jds,jde, kds,kde,                       &
                     ims,ime, jms,jme, kms,kme,                       &
                     its,ite, jts,jte, kts,kte                        )
!----------------------------------------------------------------------
 IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte  
      INTEGER, INTENT(IN)        :: LK,MYIS,MYIE
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte) :: POZN
      REAL,    INTENT(IN), DIMENSION(its:ite)  :: XLAT
      REAL,    INTENT(INOUT), DIMENSION(its:ite,kts:kte) :: QO3
      REAL,    INTENT(IN)  :: RSIN1,RCOS1,RCOS2
!----------------------------------------------------------------------
      INTEGER, PARAMETER ::  NL=81,NLP1=NL+1,LNGTH=37*NL
      REAL, PARAMETER :: RTD=57.2957795

!     REAL,    INTENT(IN),  DIMENSION(37,NL) :: XDUO3N,XDO3N4,XDO3N2,XDO3N3
!     REAL,    INTENT(IN), DIMENSION(NL)    :: PRGFDL
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      INTEGER,DIMENSION(its:ite)    :: JJROW
      REAL,   DIMENSION(its:ite)    :: TTHAN
      REAL,   DIMENSION(its:ite,NL) :: QO3O3

      INTEGER :: I,K,NUMITR,ILOG,IT,NHALF
      REAL    :: TH2,DO3V,DO3VP,APHI,APLO
!----------------------------------------------------------------------
      DO I=MYIS,MYIE
        TH2=0.2*XLAT(I)
        JJROW(I)=19.001-TH2
        TTHAN(I)=(19-JJROW(I))-TH2
      ENDDO
!
!***  SEASONAL AND SPATIAL INTERPOLATION DONE BELOW.
!
      DO K=1,NL
      DO I=MYIS,MYIE
        DO3V=XDUO3N(JJROW(I),K)+RSIN1*XDO3N2(JJROW(I),K)  &
                   +RCOS1*XDO3N3(JJROW(I),K)  &
                   +RCOS2*XDO3N4(JJROW(I),K)
        DO3VP=XDUO3N(JJROW(I)+1,K)+RSIN1*XDO3N2(JJROW(I)+1,K) &
                    +RCOS1*XDO3N3(JJROW(I)+1,K) &
                    +RCOS2*XDO3N4(JJROW(I)+1,K)
!
!***  NOW LATITUDINAL INTERPOLATION
!***  AND CONVERT O3 INTO MASS MIXING RATIO (ORIG DATA MPY BY 1.E4)
! 
        QO3O3(I,K)=1.E-4*(DO3V+TTHAN(I)*(DO3VP-DO3V))
      ENDDO
      ENDDO
!***
!***  VERTICAL INTERPOLATION FOR EACH GRIDPOINT (LINEAR IN LN P)
!***
      NUMITR=0
      ILOG=NL
   20 CONTINUE
      ILOG=(ILOG+1)/2
        IF(ILOG.EQ.1)GO TO 25
        NUMITR=NUMITR+1
        GO TO 20
   25 CONTINUE
!
      DO 60 K=1,LK
!
      NHALF=(NL+1)/2
      DO I=MYIS,MYIE
        JJROW(I)=NHALF
      ENDDO
!
      DO 40 IT=1,NUMITR
      NHALF=(NHALF+1)/2
      DO I=MYIS,MYIE
        IF(POZN(I,K).LT.PRGFDL(JJROW(I)-1))THEN
          JJROW(I)=JJROW(I)-NHALF
        ELSEIF(POZN(I,K).GE.PRGFDL(JJROW(I)))THEN
          JJROW(I)=JJROW(I)+NHALF
        ENDIF
        JJROW(I)=MIN(JJROW(I),NL)
        JJROW(I)=MAX(JJROW(I),2)
      ENDDO
   40 CONTINUE
!
      DO 50 I=MYIS,MYIE
      IF(POZN(I,K).LT.PRGFDL(1))THEN
        QO3(I,K)=QO3O3(I,1)
      ELSE IF(POZN(I,K).GT.PRGFDL(NL))THEN
        QO3(I,K)=QO3O3(I,NL)
      ELSE
        APLO=ALOG(PRGFDL(JJROW(I)-1))
        APHI=ALOG(PRGFDL(JJROW(I)))
        QO3(I,K)=QO3O3(I,JJROW(I))+(ALOG(POZN(I,K))-APHI)/ &
                   (APLO-APHI)* &
                   (QO3O3(I,JJROW(I)-1)-QO3O3(I,JJROW(I)))
      ENDIF
   50 CONTINUE
!
   60 CONTINUE

  END SUBROUTINE OZON2D
!-----------------------------------------------------------------------

! SUBROUTINE ZERO2(ARRAY, &
!                  ids,ide, jds,jde, kds,kde,                         &
!                  ims,ime, jms,jme, kms,kme,                         &
!                  its,ite, jts,jte, kts,kte                          )
!----------------------------------------------------------------------
!IMPLICIT NONE
!----------------------------------------------------------------------
!     INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
!                                   ims,ime, jms,jme, kms,kme ,      &
!                                   its,ite, jts,jte, kts,kte
!     REAL, INTENT(INOUT), DIMENSION(its:ite,jts:jte) :: ARRAY
!     INTEGER :: I,J
!----------------------------------------------------------------------
!     DO J=jts,jte
!     DO I=its,ite
!       ARRAY(I,J)=0.
!     ENDDO
!     ENDDO

! END SUBROUTINE ZERO2

!----------------------------------------------------------------

      SUBROUTINE O3INT(PHALF,DDUO3N,DDO3N2,DDO3N3,DDO3N4, &
                 ids,ide, jds,jde, kds,kde,            &
                 ims,ime, jms,jme, kms,kme,            &
                 its,ite, jts,jte, kts,kte             )
!----------------------------------------------------------------------
 IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    O3INT       COMPUTE ZONAL MEAN OZONE FOR ETA LYRS
!   PRGMMR: KENNETH CAMPANA  ORG: W/NMC23    DATE: 89-07-07
!           MICHAEL BALDWIN  ORG: W/NMC22    DATE: 92-06-08
!
! ABSTRACT: THIS CODE WRITTEN AT GFDL...
!   CALCULATES SEASONAL ZONAL MEAN OZONE,EVERY 5 DEG OF LATITUDE,
!   FOR CURRENT MODEL VERTICAL COORDINATE. OUTPUT DATA IN G/G * 1.E4
!   CODE IS CALLED ONLY ONCE.
!
! PROGRAM HISTORY LOG:
!   84-01-01  FELS AND SCHWARZKOPF,GFDL.
!   89-07-07  K. CAMPANA - ADAPTED STAND-ALONE CODE FOR IN-LINE USE.
!   92-06-08  M. BALDWIN - UPDATE TO RUN IN ETA MODEL
!
! USAGE:    CALL O3INT(O3,SIGL) OLD
!   INPUT ARGUMENT LIST:
!     PHALF    - MID LAYER PRESSURE (K=LM+1 IS MODEL SURFACE)
!   OUTPUT ARGUMENT LIST:
!     DDUO3N   - ZONAL MEAN OZONE DATA IN ALL MODEL LAYERS (G/G*1.E4)
!     DDO3N2     DIMENSIONED(L,N),WHERE L(=37) IS LATITUDE BETWEEN
!     DDO3N3     N AND S POLES,N=NUM OF VERTICAL LYRS(K=1 IS TOP LYR)
!     DDO3N4     AND SEASON-WIN,SPR,SUM,FALL.
!        IN COMMON
!
!   OUTPUT FILES:
!     OUTPUT   - PRINT FILE.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 200.
!
!$$$
!....     PROGRAM O3INT FROM DAN SCHWARZKOPF-GETS ZONAL MEAN O3
!..    OUTPUT O3 IS WINTER,SPRING,SUMMER,FALL (NORTHERN HEMISPHERE)
!-----------------------------------------------------------------------
!      INCLUDE "parmeta"
!-----------------------------------------------------------------------
!     *********************************************************

      INTEGER :: N,NP,NP2,NM1

!     PARAMETER (N=LM,NP=N+1,NP2=N+2,NM1=N-1)
!     *********************************************************
!-----------------------------------------------------------------------
!***
!***  SEASONAL CLIMATOLOGIES OF O3 (OBTAINED FROM A PREVIOUSLY RUN
!***  CODE WHICH INTERPOLATES O3 TO USER VERTICAL COORDINATE).
!***  DEFINED AS 5 DEG LAT MEANS N.P.->S.P.
!***
      REAL, INTENT(OUT), DIMENSION(37,kte):: DDUO3N,DDO3N2,DDO3N3,DDO3N4

!                        C O M M O N /SAVMEM/
!       ...WINTER....  ...SPRING....  ...SUMMER....  ....FALL.....
!    1  DDUO3N(37,LM), DDO3N2(37,LM), DDO3N3(37,LM), DDO3N4(37,LM)
!          ..... K.CAMPANA   OCTOBER 1988
!CCC  DIMENSION T41(NP2,2),O3O3(37,N,4)
!     DIMENSION SIGL(N)
!     *********************************************************
      REAL ::   QI(82)
      REAL ::   DDUO3(19,kts:kte),RO31(10,41),RO32(10,41),DUO3N(19,41)
      REAL ::   TEMPN(19)
      REAL ::   O3HI(10,25),O3LO1(10,16),O3LO2(10,16),O3LO3(10,16), &
                O3LO4(10,16)
      REAL ::   O3HI1(10,16),O3HI2(10,9),PH1(45),PH2(37),P1(48),P2(33)
      REAL ::   O35DEG(37,kts:kte)
      REAL ::   RSTD(81),RO3(10,41),RO3M(10,40),RBAR(kts:kte),RDATA(81), &
                PHALF(kts:kte+1),P(81),PH(82)

      INTEGER :: NKK,NK,NKP,K,L,NCASE,ITAPE,IPLACE,NKMM,NKM,KI,KK,KQ,JJ,KEN
      REAL :: O3RD,O3TOT,O3DU

      EQUIVALENCE (O3HI1(1,1),O3HI(1,1)),(O3HI2(1,1),O3HI(1,17))
      EQUIVALENCE (PH1(1),PH(1)),(PH2(1),PH(46))
      EQUIVALENCE (P1(1),P(1)),(P2(1),P(49))
      DATA PH1/      0., &
           0.1027246E-04, 0.1239831E-04, 0.1491845E-04, 0.1788053E-04, &
           0.2135032E-04, 0.2540162E-04, 0.3011718E-04, 0.3558949E-04, &
           0.4192172E-04, 0.4922875E-04, 0.5763817E-04, 0.6729146E-04, &
           0.7834518E-04, 0.9097232E-04, 0.1053635E-03, 0.1217288E-03, &
           0.1402989E-03, 0.1613270E-03, 0.1850904E-03, 0.2119495E-03, &
           0.2423836E-03, 0.2768980E-03, 0.3160017E-03, 0.3602623E-03, &
           0.4103126E-03, 0.4668569E-03, 0.5306792E-03, 0.6026516E-03, &
           0.6839018E-03, 0.7759249E-03, 0.8803303E-03, 0.9987843E-03, &
           0.1133178E-02, 0.1285955E-02, 0.1460360E-02, 0.1660001E-02, &
           0.1888764E-02, 0.2151165E-02, 0.2452466E-02, 0.2798806E-02, &
           0.3197345E-02, 0.3656456E-02, 0.4185934E-02, 0.4797257E-02/
      DATA PH2/ &
           0.5503893E-02, 0.6321654E-02, 0.7269144E-02, 0.8368272E-02, &
           0.9644873E-02, 0.1112946E-01, 0.1285810E-01, 0.1487354E-01, &
           0.1722643E-01, 0.1997696E-01, 0.2319670E-01, 0.2697093E-01, &
           0.3140135E-01, 0.3660952E-01, 0.4274090E-01, 0.4996992E-01, &
           0.5848471E-01, 0.6847525E-01, 0.8017242E-01, 0.9386772E-01, &
           0.1099026E+00, 0.1286765E+00, 0.1506574E+00, 0.1763932E+00, &
           0.2065253E+00, 0.2415209E+00, 0.2814823E+00, 0.3266369E+00, &
           0.3774861E+00, 0.4345638E+00, 0.4984375E+00, 0.5697097E+00, &
           0.6490189E+00, 0.7370409E+00, 0.8344896E+00, 0.9421190E+00, &
           0.1000000E+01/
      DATA P1/ &
           0.9300000E-05, 0.1129521E-04, 0.1360915E-04, 0.1635370E-04, &
           0.1954990E-04, 0.2331653E-04, 0.2767314E-04, 0.3277707E-04, &
           0.3864321E-04, 0.4547839E-04, 0.5328839E-04, 0.6234301E-04, &
           0.7263268E-04, 0.8450696E-04, 0.9793231E-04, 0.1133587E-03, &
           0.1307170E-03, 0.1505832E-03, 0.1728373E-03, 0.1982122E-03, &
           0.2266389E-03, 0.2592220E-03, 0.2957792E-03, 0.3376068E-03, &
           0.3844381E-03, 0.4379281E-03, 0.4976965E-03, 0.5658476E-03, &
           0.6418494E-03, 0.7287094E-03, 0.8261995E-03, 0.9380076E-03, &
           0.1063498E-02, 0.1207423E-02, 0.1369594E-02, 0.1557141E-02, &
           0.1769657E-02, 0.2015887E-02, 0.2295520E-02, 0.2620143E-02, &
           0.2989651E-02, 0.3419469E-02, 0.3909867E-02, 0.4481491E-02, &
           0.5135272E-02, 0.5898971E-02, 0.6774619E-02, 0.7799763E-02/
      DATA P2/ &
           0.8978218E-02, 0.1036103E-01, 0.1195488E-01, 0.1382957E-01, &
           0.1599631E-01, 0.1855114E-01, 0.2151235E-01, 0.2501293E-01, &
           0.2908220E-01, 0.3390544E-01, 0.3952926E-01, 0.4621349E-01, &
           0.5403168E-01, 0.6330472E-01, 0.7406807E-01, 0.8677983E-01, &
           0.1015345E+00, 0.1189603E+00, 0.1391863E+00, 0.1630739E+00, &
           0.1908004E+00, 0.2235461E+00, 0.2609410E+00, 0.3036404E+00, &
           0.3513750E+00, 0.4055375E+00, 0.4656677E+00, 0.5335132E+00, &
           0.6083618E+00, 0.6923932E+00, 0.7845676E+00, 0.8875882E+00, &
           0.1000000E+01/
      DATA O3HI1/ &
       .55,.50,.45,.45,.40,.35,.35,.30,.30,.30, &
       .55,.51,.46,.47,.42,.38,.37,.36,.35,.35, &
       .55,.53,.48,.49,.44,.42,.41,.40,.38,.38, &
       .60,.55,.52,.52,.50,.47,.46,.44,.42,.41, &
       .65,.60,.55,.56,.53,.52,.50,.48,.45,.45, &
       .75,.65,.60,.60,.55,.55,.55,.50,.48,.47, &
       .80,.75,.75,.75,.70,.70,.65,.63,.60,.60, &
       .90,.85,.85,.80,.80,.75,.75,.74,.72,.71, &
       1.10,1.05,1.00,.90,.90,.90,.85,.83,.80,.80, &
       1.40,1.30,1.25,1.25,1.25,1.20,1.15,1.10,1.05,1.00, &
       1.7,1.7,1.6,1.6,1.6,1.6,1.6,1.6,1.5,1.5, &
       2.1,2.0,1.9,1.9,1.9,1.8,1.8,1.8,1.7,1.7, &
       2.4,2.3,2.2,2.2,2.2,2.1,2.1,2.1,2.0,2.0, &
       2.7,2.5,2.5,2.5,2.5,2.5,2.4,2.4,2.3,2.3, &
       2.9,2.8,2.7,2.7,2.7,2.7,2.7,2.7,2.6,2.6, &
       3.1,3.1,3.0,3.0,3.0,3.0,3.0,3.0,2.9,2.8/
      DATA O3HI2/ &
       3.3,3.4,3.4,3.6,3.7,3.9,4.0,4.1,4.0,3.8, &
       3.6,3.8,3.9,4.2,4.7,5.3,5.6,5.7,5.5,5.2, &
       4.1,4.3,4.7,5.2,6.0,6.7,7.0,6.8,6.4,6.2, &
       5.4,5.7,6.0,6.6,7.3,8.0,8.4,7.7,7.1,6.7, &
       6.7,6.8,7.0,7.6,8.3,10.0,9.6,8.2,7.5,7.2, &
       9.2,9.3,9.4,9.6,10.3,10.6,10.0,8.5,7.7,7.3, &
       12.6,12.1,12.0,12.1,11.7,11.0,10.0,8.6,7.8,7.4, &
       14.2,13.5,13.1,12.8,11.9,10.9,9.8,8.5,7.8,7.5, &
       14.3,14.0,13.4,12.7,11.6,10.6,9.3,8.4,7.6,7.3/
      DATA O3LO1/ &
       14.9,14.2,13.3,12.5,11.2,10.3,9.5,8.6,7.5,7.4, &
       14.5,14.1,13.0,11.8,10.5,9.8,9.2,7.9,7.4,7.4, &
       11.8,11.5,10.9,10.5,9.9,9.6,8.9,7.5,7.2,7.2, &
       7.3,7.7,7.8,8.4,8.4,8.5,7.9,7.4,7.1,7.1, &
       4.1,4.4,5.3,6.6,6.9,7.5,7.4,7.2,7.0,6.9, &
       1.8,1.9,2.5,3.3,4.5,5.8,6.3,6.3,6.4,6.1, &
       0.4,0.5,0.8,1.2,2.7,3.6,4.6,4.7,5.0,5.2, &
       .10,.15,.20,.50,1.4,2.1,3.0,3.2,3.5,3.9, &
       .07,.10,.12,.30,1.0,1.4,1.8,1.9,2.3,2.5, &
       .06,.08,.10,.15,.60,.80,1.4,1.5,1.5,1.6, &
       .05,.05,.06,.09,.20,.40,.70,.80,.90,.90, &
       .05,.05,.06,.08,.10,.13,.20,.25,.30,.40, &
       .05,.05,.05,.06,.07,.07,.08,.09,.10,.13, &
       .05,.05,.05,.05,.06,.06,.06,.06,.07,.07, &
       .05,.05,.05,.05,.05,.05,.05,.06,.06,.06, &
       .04,.04,.04,.04,.04,.04,.04,.05,.05,.05/
      DATA O3LO2/ &
       14.8,14.2,13.8,12.2,11.0,9.8,8.5,7.8,7.4,6.9, &
       13.2,13.0,12.5,11.3,10.4,9.0,7.8,7.5,7.0,6.6, &
       10.6,10.6,10.7,10.1,9.4,8.6,7.5,7.0,6.5,6.1, &
       7.0,7.3,7.5,7.5,7.5,7.3,6.7,6.4,6.0,5.8, &
       3.8,4.0,4.7,5.0,5.2,5.9,5.8,5.6,5.5,5.5, &
       1.4,1.6,2.4,3.0,3.7,4.1,4.6,4.8,5.1,5.0, &
       .40,.50,.90,1.2,2.0,2.7,3.2,3.6,4.3,4.1, &
       .07,.10,.20,.30,.80,1.4,2.1,2.4,2.7,3.0, &
       .06,.07,.09,.15,.30,.70,1.2,1.4,1.6,2.0, &
       .05,.05,.06,.12,.15,.30,.60,.70,.80,.80, &
       .04,.05,.06,.08,.09,.15,.30,.40,.40,.40, &
       .04,.04,.05,.055,.06,.09,.12,.13,.15,.15, &
       .03,.03,.045,.052,.055,.06,.07,.07,.06,.07, &
       .03,.03,.04,.051,.052,.052,.06,.06,.05,.05, &
       .02,.02,.03,.05,.05,.05,.04,.04,.04,.04, &
       .02,.02,.02,.04,.04,.04,.03,.03,.03,.03/
      DATA O3LO3/ &
       14.5,14.0,13.5,11.3,11.0,10.0,9.0,8.3,7.5,7.3, &
       13.5,13.2,12.5,11.1,10.4,9.7,8.2,7.8,7.4,6.8, &
       10.8,10.9,11.0,10.4,10.0,9.6,7.9,7.5,7.0,6.7, &
       7.3,7.5,7.8,8.5,9.0,8.5,7.7,7.4,6.9,6.5, &
       4.1,4.5,5.3,6.2,7.3,7.7,7.3,7.0,6.6,6.4, &
       1.8,2.0,2.2,3.8,4.3,5.6,6.2,6.2,6.4,6.2, &
       .30,.50,.60,1.5,2.8,3.7,4.5,4.7,5.5,5.6, &
       .09,.10,.15,.60,1.2,2.1,3.0,3.5,4.0,4.3, &
       .06,.08,.10,.30,.60,1.1,1.9,2.2,2.9,3.0, &
       .04,.05,.06,.15,.45,.60,1.1,1.3,1.6,1.8, &
       .04,.04,.04,.08,.20,.30,.55,.60,.75,.90, &
       .04,.04,.04,.05,.06,.10,.12,.15,.20,.25, &
       .04,.04,.03,.04,.05,.06,.07,.07,.07,.08, &
       .03,.03,.04,.05,.05,.05,.05,.05,.05,.05, &
       .03,.03,.03,.04,.04,.04,.05,.05,.04,.04, &
       .02,.02,.02,.04,.04,.04,.04,.04,.03,.03/
      DATA O3LO4/ &
       14.2,13.8,13.2,12.5,11.7,10.5,8.6,7.8,7.5,6.6, &
       12.5,12.4,12.2,11.7,10.8,9.8,7.8,7.2,6.5,6.1, &
       10.6,10.5,10.4,10.1,9.6,9.0,7.1,6.8,6.1,5.9, &
       7.0,7.4,7.9,7.8,7.6,7.3,6.2,6.1,5.8,5.6, &
       4.2,4.6,5.1,5.6,5.9,5.9,5.9,5.8,5.6,5.3, &
       2.1,2.3,2.6,2.9,3.5,4.3,4.8,4.9,5.1,5.1, &
       0.7,0.8,1.0,1.5,2.0,2.8,3.5,3.6,3.7,4.0, &
       .15,.20,.40,.50,.60,1.4,2.1,2.2,2.3,2.5, &
       .08,.10,.15,.25,.30,.90,1.2,1.3,1.4,1.6, &
       .07,.08,.10,.14,.20,.50,.70,.90,.90,.80, &
       .05,.06,.08,.12,.14,.20,.35,.40,.60,.50, &
       .05,.05,.08,.09,.09,.09,.11,.12,.15,.18, &
       .04,.05,.06,.07,.07,.08,.08,.08,.08,.08, &
       .04,.04,.05,.07,.07,.07,.07,.07,.06,.05, &
       .02,.02,.04,.05,.05,.05,.05,.05,.04,.04, &
       .02,.02,.03,.04,.04,.04,.04,.04,.03,.03/

!!!!!
!     PSS=101325.
!     PDIF=PSS-PT
!
!     DO L=1,LM1
!       PHALF(L+1)=AETA(L)*PDIF+PT
!     ENDDO
!
!     PHALF(1)=0.
!     PHALF(LP1)=PSS
!!!!
      N=kte;NP=N+1;NP2=N+2;NM1=N-1

      NKK=41
      NK=81
      NKP=NK+1
      DO 24 K=1,NP
!  24 PHALF(K)=PHALF(K)*1.0E 03
   24 PHALF(K)=PHALF(K)*0.01*1.0E+03
!  24 PSTD(K)=PSTD(K+1)*1.0E 03
      DO 25 K=1,NK
      PH(K)=PH(K)*1013250.
   25 P(K)=P(K)*1013250.
      PH(NKP)=PH(NKP)*1013250.
!KAC  WRITE (6,3) PH
!KAC  WRITE (6,3) P
!     WRITE (6,3) (PHALF(K),K=1,NP)
!     WRITE (6,3) (PSTD(K),K=1,NP)
!***LOAD ARRAYS RO31,RO32,AS IN DICKS PGM.
      DO 1010 K=1,25
      DO 1010 L=1,10
        RO31(L,K)=O3HI(L,K)
        RO32(L,K)=O3HI(L,K)
1010  CONTINUE
!
      DO 3000 NCASE=1,4
      ITAPE=NCASE+50
      IPLACE=2
      IF (NCASE.EQ.2) IPLACE=4
      IF (NCASE.EQ.3) IPLACE=1
      IF (NCASE.EQ.4) IPLACE=3
!***NCASE=1: SPRING (IN N.H.)
!***NCASE=2: FALL   (IN N.H.)
!***NCASE=3: WINTER (IN N.H.)
!***NCASE=4: SUMMER (IN N.H.)
      IF (NCASE.EQ.1.OR.NCASE.EQ.2) THEN
         DO 1011 K=26,41
         DO 1011 L=1,10
           RO31(L,K)=O3LO1(L,K-25)
           RO32(L,K)=O3LO2(L,K-25)
1011     CONTINUE
      ENDIF
      IF (NCASE.EQ.3.OR.NCASE.EQ.4) THEN
         DO 1031 K=26,41
         DO 1031 L=1,10
           RO31(L,K)=O3LO3(L,K-25)
           RO32(L,K)=O3LO4(L,K-25)
1031     CONTINUE
      ENDIF
      DO 30 KK=1,NKK
      DO 31 L=1,10
      DUO3N(L,KK)=RO31(11-L,KK)
   31 DUO3N(L+9,KK)=RO32(L,KK)
      DUO3N(10,KK)=.5*(RO31(1,KK)+RO32(1,KK))
   30 CONTINUE
!***FOR NCASE=2 OR NCASE=4,REVERSE LATITUDE ARRANGEMENT OF CORR. SEASON
      IF (NCASE.EQ.2.OR.NCASE.EQ.4) THEN
         DO 1024 KK=1,NKK
         DO 1025 L=1,19
           TEMPN(L)=DUO3N(20-L,KK)
1025     CONTINUE
         DO 1026 L=1,19
           DUO3N(L,KK)=TEMPN(L)
1026     CONTINUE
1024     CONTINUE
      ENDIF
!***DUO3N NOW IS O3 PROFILE FOR APPROPRIATE SEASON,AT STD. PRESSURE
!      LEVELS
!KAC  WRITE (6,800) DUO3N
!***BEGIN LATITUDE (10 DEG) LOOP
      DO 33 L=1,19
      DO 22 KK=1,NKK
   22 RSTD(KK)=DUO3N(L,KK)
      NKM=NK-1
      NKMM=NK-3
!     BESSELS HALF-POINT INTERPOLATION FORMULA
      DO 60 K=4,NKMM,2
      KI=K/2
   60 RDATA(K)=.5*(RSTD(KI)+RSTD(KI+1))-(RSTD(KI+2)-RSTD(KI+1)-RSTD(KI)+ &
      RSTD(KI-1))/16.
      RDATA(2)=.5*(RSTD(2)+RSTD(1))
      RDATA(NKM)=.5*(RSTD(NKK)+RSTD(NKK-1))
!     PUT UNCHANGED DATA INTO NEW ARRAY
      DO 61 K=1,NK,2
      KQ=(K+1)/2
   61 RDATA(K)=RSTD(KQ)
!---NOTE TO NMC: THIS WRITE IS COMMENTED OUT TO REDUCE PRINTOUT
!     WRITE (6,798) RDATA
!     CALCULATE LAYER-MEAN OZONE MIXING RATIO FOR EACH MODEL LEVEL
      DO 99 KK=1,N
      RBAR(KK)=0.
!     LOOP TO CALCULATE SUMS TO GET LAYER OZONE MEAN
      DO 98 K=1,NK
      IF(PH(K+1).LT.PHALF(KK)) GO TO 98
      IF(PH(K).GT.PHALF(KK+1)) GO TO 98
      IF(PH(K+1).LT.PHALF(KK+1).AND.PH(K).LT.PHALF(KK)) RBAR(KK)=RBAR(KK &
      )+RDATA(K)*(PH(K+1)-PHALF(KK))
      IF(PH(K+1).LT.PHALF(KK+1).AND.PH(K).GE.PHALF(KK)) RBAR(KK)=RBAR(KK &
      )+RDATA(K)*(PH(K+1)-PH(K))
      IF(PH(K+1).GT.PHALF(KK+1).AND.PH(K).GT.PHALF(KK)) RBAR(KK)=RBAR(KK &
      )+RDATA(K)*(PHALF(KK+1)-PH(K))
   98 CONTINUE
      RBAR(KK)=RBAR(KK)/(PHALF(KK+1)-PHALF(KK))
      IF(RBAR(KK).GT..0000) GO TO 99
!     CODE TO COVER CASE WHEN MODEL RESOLUTION IS SO FINE THAT NO VALUE
!     OF P(K) IN THE OZONE DATA ARRAY FALLS BETWEEN PHALF(KK+1) AND
!     PHALF(KK).   PROCEDURE IS TO SIMPLY GRAB THE NEAREST VALUE FROM
!     RDATA
      DO 29 K=1,NK
      IF(PH(K).LT.PHALF(KK).AND.PH(K+1).GE.PHALF(KK+1)) RBAR(KK)=RDATA(K)
   29 CONTINUE
   99 CONTINUE
!     CALCULATE TOTAL OZONE
      O3RD=0.
      DO 89 KK=1,80
   89 O3RD=O3RD+RDATA(KK)*(PH(KK+1)-PH(KK))
      O3RD=O3RD+RDATA(81)*(P(81)-PH(81))
      O3RD=O3RD/980.
      O3TOT=0.
      DO 88 KK=1,N
   88 O3TOT=O3TOT+RBAR(KK)*(PHALF(KK+1)-PHALF(KK))
      O3TOT=O3TOT/980.
!     UNITS ARE MICROGRAMS/CM**2
      O3DU=O3TOT/2.144
!     O3DU UNITS ARE DOBSON UNITS (10**-3 ATM-CM)
!--NOTE TO NMC: THIS IS COMMENTED OUT TO SAVE PRINTOUT
!     WRITE (6,796) O3RD,O3TOT,O3DU
      DO 23 KK=1,N
   23 DDUO3(L,KK)=RBAR(KK)*.01
   33 CONTINUE
!***END OF LATITUDE LOOP
!
!***CREATE 5 DEG OZONE QUANTITIES BY LINEAR INTERPOLATION OF
!      10 DEG VALUES
      DO 1060 KK=1,N
        DO 1061 L=1,19
          O35DEG(2*L-1,KK)=DDUO3(L,KK)
1061    CONTINUE
        DO 1062 L=1,18
          O35DEG(2*L,KK)=0.5*(DDUO3(L,KK)+DDUO3(L+1,KK))
1062    CONTINUE
1060  CONTINUE
!***OUTPUT TO UNIT (ITAPE) THE OZONE VALUES FOR LATER USE
!O222  ***************************************************
!C          WRITE (66) O35DEG
      IF (IPLACE.EQ.1) THEN
      DO 302 JJ=1,37
       DO 302 KEN=1,N
        DDUO3N(JJ,KEN) = O35DEG(JJ,KEN)
  302 CONTINUE
      ELSE IF (IPLACE.EQ.2) THEN
      DO 312 JJ=1,37
       DO 312 KEN=1,N
        DDO3N2(JJ,KEN) = O35DEG(JJ,KEN)
  312 CONTINUE
      ELSE IF (IPLACE.EQ.3) THEN
      DO 322 JJ=1,37
       DO 322 KEN=1,N
        DDO3N3(JJ,KEN) = O35DEG(JJ,KEN)
  322 CONTINUE
      ELSE IF (IPLACE.EQ.4) THEN
      DO 332 JJ=1,37
       DO 332 KEN=1,N
        DDO3N4(JJ,KEN) = O35DEG(JJ,KEN)
  332 CONTINUE
      END IF
!O222  ***************************************************
3000  CONTINUE
!***END OF LOOP OVER CASES
      RETURN
   1  FORMAT(10F4.2)
    2 FORMAT(10X,E14.7,1X,E14.7,1X,E14.7,1X,E14.7,1X)
   3  FORMAT(10E12.5)
  797 FORMAT(10F7.2)
  799 FORMAT(19F6.4)
  800 FORMAT(19F6.2)
  102 FORMAT(' O3 IPLACE=',I4)
 1033 FORMAT(19F6.5)
  101 FORMAT(5X,1H*,F6.5,1H,,F6.5,1H,,F6.5,1H,,F6.5,1H,,F6.5,1H,,F6.5, &
      1H,,F6.5,1H,,F6.5,1H,,F6.5,1H,)
      
      END SUBROUTINE O3INT
!----------------------------------------------------------------

  SUBROUTINE CLO89(CLDFAC,CAMT,NCLDS,KBTM,KTOP                  &
      ,          ids,ide, jds,jde, kds,kde                      &
      ,          ims,ime, jms,jme, kms,kme                      &
      ,          its,ite, jts,jte, kts,kte                      )
!----------------------------------------------------------------------
 IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte
!----------------------------------------------------------------------

!     ************************************************************
!     *                                                          *
!     * THIS SUBROUTINE WAS MODIFIED TO BE USED IN THE ETA MODEL *
!     *                                                          *
!     *                            Q. ZHAO    95-3-22            *
!     *                                                          *
!     ************************************************************

      REAL,    INTENT(OUT),DIMENSION(its:ite,kts:kte+1,kts:kte+1) :: CLDFAC
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: CAMT
      INTEGER, INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: KBTM,KTOP
      INTEGER, INTENT(IN), DIMENSION(its:ite)           :: NCLDS

      REAL,    DIMENSION(kts:kte+1,kts:kte+1,64) :: CLDIPT
      REAL,    DIMENSION(kts:kte+1) :: CLDROW
      INTEGER:: IQ,ITOP,I,J,JTOP,IR,IP,K1,K2,KB,K,KP,KT,NC
      REAL   :: XCLD

      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE

    !  DIMENSION CLDIPT(LP1,LP1, 64 )
    !  DIMENSION NCLDS(IDIM1:IDIM2),KTOP(IDIM1:IDIM2,LP1), &
    !            KBTM(IDIM1:IDIM2,LP1)
    !  DIMENSION CLDROW(LP1)
    !  DIMENSION CAMT(IDIM1:IDIM2,LP1),CLDFAC(IDIM1:IDIM2,LP1,LP1)

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3
      LM1=L-1;  LM2=L-2;  LM3=L-3
      MYIS=its; MYIE=ite

!
      DO 1 IQ=MYIS,MYIE,64
      ITOP=IQ+63
      IF(ITOP.GT.MYIE) ITOP=MYIE
      JTOP=ITOP-IQ+1
      DO 11 IP=1,JTOP
      IR=IQ+IP-1
      IF (NCLDS(IR).EQ.0) THEN
        DO 25 J=1,LP1
        DO 25 I=1,LP1
        CLDIPT(I,J,IP)=1.
25      CONTINUE
      ENDIF
      IF (NCLDS(IR).GE.1) THEN
          XCLD=1.-CAMT(IR,2)
           K1=KTOP(IR,2)+1
           K2=KBTM(IR,2)
          DO 27 J=1,LP1
              CLDROW(J)=1.
27        CONTINUE
          DO 29 J=1,K2
              CLDROW(J)=XCLD
29        CONTINUE
          KB=MAX(K1,K2+1)
          DO 33 K=KB,LP1
          DO 33 KP=1,LP1
               CLDIPT(KP,K,IP)=CLDROW(KP)
33        CONTINUE
          DO 37 J=1,LP1
              CLDROW(J)=1.
37        CONTINUE
          DO 39 J=K1,LP1
              CLDROW(J)=XCLD
39        CONTINUE
          KT=MIN(K1-1,K2)
          DO 43 K=1,KT
          DO 43 KP=1,LP1
              CLDIPT(KP,K,IP)=CLDROW(KP)
43        CONTINUE
          IF(K2+1.LE.K1-1) THEN
            DO 31 J=K2+1,K1-1
            DO 31 I=1,LP1
                CLDIPT(I,J,IP)=1.
31          CONTINUE
          ELSE IF(K1.LE.K2) THEN
            DO 32 J=K1,K2
            DO 32 I=1,LP1
                CLDIPT(I,J,IP)=XCLD
32          CONTINUE
          ENDIF
      ENDIF

      IF (NCLDS(IR).GE.2) THEN
        DO 21 NC=2,NCLDS(IR)
          XCLD=1.-CAMT(IR,NC+1)
           K1=KTOP(IR,NC+1)+1
           K2=KBTM(IR,NC+1)
          DO 47 J=1,LP1
              CLDROW(J)=1.
47        CONTINUE
          DO 49 J=1,K2
              CLDROW(J)=XCLD
49        CONTINUE
          KB=MAX(K1,K2+1)
          DO 53 K=KB,LP1
          DO 53 KP=1,LP1
               CLDIPT(KP,K,IP)=CLDIPT(KP,K,IP)*CLDROW(KP)
53        CONTINUE
          DO 57 J=1,LP1
              CLDROW(J)=1.
57        CONTINUE
          DO 59 J=K1,LP1
              CLDROW(J)=XCLD
59        CONTINUE
          KT=MIN(K1-1,K2)
          DO 63 K=1,KT
          DO 63 KP=1,LP1
              CLDIPT(KP,K,IP)=CLDIPT(KP,K,IP)*CLDROW(KP)
63        CONTINUE
          IF(K1.LE.K2) THEN
            DO 52 J=K1,K2
            DO 52 I=1,LP1
                CLDIPT(I,J,IP)=CLDIPT(I,J,IP)*XCLD
52          CONTINUE
          ENDIF
21        CONTINUE
      ENDIF
11    CONTINUE
      DO 71 J=1,LP1
      DO 71 I=1,LP1
      DO 71 IP=1,JTOP
      IR=IQ+IP-1
      CLDFAC(IR,I,J)=CLDIPT(I,J,IP)
71    CONTINUE
1     CONTINUE

  END SUBROUTINE CLO89
!----------------------------------------------------------------
!     SUBROUTINE LWR88(HEATRA,GRNFLX,TOPFLX,                         &
!                      PRESS,TEMP,RH2O,QO3,CLDFAC,                   &
!                      CAMT,NCLDS,KTOP,KBTM,                         &
!!                     BO3RND,AO3RND,T1,T2,T4,EM1V,EM1VW,EM3V,       &
!                      BO3RND,AO3RND, &
!                      APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
!                      ZERO,ONE,H18E3,P0INV,H6P08108,DIFFCTR,        &
!                      GINV,H3M4,BETINW,RATH2OMW,GP0INV,P0,P0XZP8,   &
!                      P0XZP2,H3M3,H1M3,H1M2,H25E2,B0,B2,B1,B3,HAF,  &
!                      TEN,HP1,FOUR,HM1EZ,SKO3R,                     &
!                      AB15WD,SKC1R,RADCON,QUARTR,TWO,               &
!                      HM6666M2,HMP66667,HMP5, HP166666,H41666M2,    &
!                      RADCON1,H16E1, H28E1,H44194M2,H1P41819,SKO2D, &
!                      ids,ide, jds,jde, kds,kde,                    &
!                      ims,ime, jms,jme, kms,kme,                    &
!                      its,ite, jts,jte, kts,kte                     )

      SUBROUTINE LWR88(HEATRA,GRNFLX,TOPFLX,                         &
                       PRESS,TEMP,RH2O,QO3,CLDFAC,                   &
                       CAMT,NCLDS,KTOP,KBTM,                         &
!                      BO3RND,AO3RND,T1,T2,T4,EM1V,EM1VW,EM3V,       &
                       BO3RND,AO3RND, &
                       APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
                       ZERO,ONE,H18E3,P0INV,H6P08108,DIFFCTR,        &
                       GINV,H3M4,BETINW,RATH2OMW,GP0INV,P0,P0XZP8,   &
                       P0XZP2,H3M3,H1M3,H1M2,H25E2,B0,B2,B1,B3,HAF,  &
                       TEN,HP1,FOUR,HM1EZ,                           &
                       RADCON,QUARTR,TWO,                            &
                       HM6666M2,HMP66667,HMP5, HP166666,H41666M2,    &
                       RADCON1,H16E1, H28E1,H44194M2,H1P41819,       &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       its,ite, jts,jte, kts,kte                     )
!---------------------------------------------------------------------
 IMPLICIT NONE
!----------------------------------------------------------------------
!     INTEGER, PARAMETER :: NBLY=15

      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte  
      REAL,    INTENT(IN)        :: ZERO,ONE,H18E3,P0INV,H6P08108,DIFFCTR
      REAL,    INTENT(IN)        :: GINV,H3M4,BETINW,RATH2OMW,GP0INV
      REAL,    INTENT(IN)        :: P0XZP8,P0XZP2,H3M3,P0,H1M3
      REAL,    INTENT(IN)        :: H1M2,H25E2,B0,B1,B2,B3,HAF
!     REAL,    INTENT(IN)        :: TEN,HP1,FOUR,HM1EZ,SKO3R
      REAL,    INTENT(IN)        :: TEN,HP1,FOUR,HM1EZ         
!     REAL,    INTENT(IN)        :: AB15WD,SKC1R,RADCON,QUARTR,TWO
      REAL,    INTENT(IN)        :: RADCON,QUARTR,TWO
      REAL,    INTENT(IN)        :: HM6666M2,HMP66667,HMP5, HP166666,H41666M2
!     REAL,    INTENT(IN) :: RADCON1,H16E1, H28E1,H44194M2,H1P41819,SKO2D
      REAL,    INTENT(IN) :: RADCON1,H16E1, H28E1,H44194M2,H1P41819
!----------------------------------------------------------------------
      REAL, INTENT(IN), DIMENSION(3) :: BO3RND,AO3RND
!     REAL,INTENT(IN),DIMENSION(5040):: T1,T2,T4,EM1V,EM1VW
!     REAL, INTENT(IN), DIMENSION(5040) :: EM3V
      REAL,INTENT(IN),DIMENSION(NBLY) :: APCM,BPCM,ATPCM,BTPCM,ACOMB, &
                                         BCOMB,BETACM

      REAL,    INTENT(IN),DIMENSION(its:ite,kts:kte+1,kts:kte+1) :: CLDFAC
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: CAMT
      INTEGER, INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: KBTM,KTOP
      INTEGER, INTENT(IN), DIMENSION(its:ite)           :: NCLDS
     
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: PRESS,TEMP
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte)   :: RH2O,QO3
      REAL,    INTENT(OUT), DIMENSION(its:ite,kts:kte)   :: HEATRA
      REAL,    INTENT(OUT), DIMENSION(its:ite)           :: GRNFLX,TOPFLX

!     REAL,    DIMENSION(kts:kte+1,kts:kte+1,64) :: CLDIPT

!     Include co2 data from a file, which needs to have exactly vertical
!     dimension of the model.
      

!!! ??? co2 table
!     REAL,    DIMENSION(kts:kte+1,kts:kte+1) :: CO251,CDT51,CDT58,C2D51,&
!                                                C2D58,CO258
!     REAL,    DIMENSION(kts:kte+1)           :: STEMP,GTEMP,CO231,CO238, &
!                                                C2D31,C2D38,CDT31,CDT38, &
!                                                CO271,CO278,C2D71,C2D78, &
!                                                CDT71,CDT78
!     REAL,    DIMENSION(kts:kte)             :: CO2M51,CO2M58,CDTM51,CDTM58, &
!                                                C2DM51,C2DM58
!!! end co2 table

!     REAL,    DIMENSION(kts:kte+1) :: CLDROW

      REAL,    DIMENSION(its:ite,kts:kte+1) :: TEXPSL,TOTPHI,TOTO3,CNTVAL,&
                                               TPHIO3,TOTVO2,TSTDAV,TDAV, & 
                                               VSUM3,CO2R1,D2CD21,DCO2D1, &
                                               CO2R2,D2CD22,DCO2D2,CO2SP1,&
                                               CO2SP2,CO2R,DCO2DT,D2CDT2, &
                                               TLSQU,DIFT
      REAL,    DIMENSION(its:ite,kts:kte)   :: DELP2,DELP,CO2NBL,&
                                               QH2O,VV,VAR1,VAR2,VAR3,VAR4
      REAL,    DIMENSION(its:ite,kts:kte+1) :: P,T
      REAL,    DIMENSION(its:ite,kts:kte)   :: CO2MR,CO2MD,CO2M2D
      REAL,    DIMENSION(its:ite,kts:kte*2+1):: EMPL

      REAL,    DIMENSION(its:ite)           :: EMX1,EMX2,VSUM1,VSUM2,A1,A2 
      REAL,    DIMENSION(its:ite,kts:kte+1,kts:kte+1) :: CO21

   !  COMMON/CO2BD3/CO251(LP1,LP1),CO258(LP1,LP1),CDT51(LP1,LP1),
   !  DIMENSION CO21(IDIM1:IDIM2,LP1,LP1),CO2NBL(IDIM1:IDIM2,L)
   !  DIMENSION CO2R(IDIM1:IDIM2,LP1),DIFT(IDIM1:IDIM2,LP1)
   ! 1   CO2M2D(IDIM1:IDIM2,L)
   !  DIMENSION CO2MR(IDIM1:IDIM2,L),CO2MD(IDIM1:IDIM2,L),
   ! 2 CO2M58(L),CDTM51(L),CDTM58(L),C2DM51(L),C2DM58(L),
   ! 1 CDT58(LP1,LP1),C2D51(LP1,LP1),C2D58(LP1,LP1),CO2M51(L),
   !  COMMON / CO2BD2 / CO231(LP1),CO238(LP1),CDT31(LP1),
   ! 1 CDT38(LP1),C2D31(LP1),C2D38(LP1)
   !  DIMENSION CO2R1(IDIM1:IDIM2,LP1),DCO2D1(IDIM1:IDIM2,LP1)
   !  DIMENSION D2CD21(IDIM1:IDIM2,LP1),D2CD22(IDIM1:IDIM2,LP1)
   ! 3 STEMP(LP1),GTEMP(LP1),B0,B1,B2,B3
   ! 1 VV(IDIM1:IDIM2,L),VSUM3(IDIM1:IDIM2,LP1),VSUM1(IDIM1:IDIM2),
   ! 2 VSUM2(IDIM1:IDIM2)
   !  DIMENSION TDAV(IDIM1:IDIM2,LP1),TSTDAV(IDIM1:IDIM2,LP1),
   !  LLP1=LL+1, LL = 2L
   !  EMX2(IDIM1:IDIM2),EMPL(IDIM1:IDIM2,LLP1)
   !  DIMENSION TPHIO3(IDIM1:IDIM2,LP1),
   !  DIMENSION TEXPSL(IDIM1:IDIM2,LP1)
   !  DIMENSION QH2O(IDIM1:IDIM2,L)
   !  DIMENSION DELP2(IDIM1:IDIM2,L)
   !  DIMENSION VAR1(IDIM1:IDIM2,L),VAR2(IDIM1:IDIM2,L),
   ! 1   VAR3(IDIM1:IDIM2,L),VAR4(IDIM1:IDIM2,L)
   ! 1 VV(IDIM1:IDIM2,L)
   !  DIMENSION CNTVAL(IDIM1:IDIM2,LP1)
   !  DIMENSION TOTO3(IDIM1:IDIM2,LP1)
   !  DIMENSION EMX1(IDIM1:IDIM2),

   !  DIMENSION PRESS(IDIM1:IDIM2,LP1),TEMP(IDIM1:IDIM2,LP1), &
   !     RH2O(IDIM1:IDIM2,L),QO3(IDIM1:IDIM2,L)
   !  DIMENSION HEATRA(IDIM1:IDIM2,L),GRNFLX(IDIM1:IDIM2),    &
   !     TOPFLX(IDIM1:IDIM2)

!
!
!****COMPUTE FLUX PRESSURES (P) AND DIFFERENCES (DELP2,DELP)
!****COMPUTE FLUX LEVEL TEMPERATURES (T) AND CONTINUUM TEMPERATURE
!    CORRECTIONS (TEXPSL)
    
      INTEGER :: K, I,KP
      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE,LLP1,LL

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3; LLP1 = 2*L + 1
      LM1=L-1;  LM2=L-2;  LM3=L-3; LL = 2*L
      MYIS=its; MYIE=ite


      DO 103 K=2,L
      DO 103 I=MYIS,MYIE
      P(I,K)=HAF*(PRESS(I,K-1)+PRESS(I,K))
      T(I,K)=HAF*(TEMP(I,K-1)+TEMP(I,K))
103   CONTINUE
      DO 105 I=MYIS,MYIE
      P(I,1)=ZERO
      P(I,LP1)=PRESS(I,LP1)
      T(I,1)=TEMP(I,1)
      T(I,LP1)=TEMP(I,LP1)
105   CONTINUE
      DO 107 K=1,L
      DO 107 I=MYIS,MYIE
      DELP2(I,K)=P(I,K+1)-P(I,K)
      DELP(I,K)=ONE/DELP2(I,K)
107   CONTINUE
!****COMPUTE ARGUMENT FOR CONT.TEMP.COEFF.
!    (THIS IS 1800.(1./TEMP-1./296.))
      DO 125 K=1,LP1
      DO 125 I=MYIS,MYIE
      TEXPSL(I,K)=H18E3/TEMP(I,K)-H6P08108
!...THEN TAKE EXPONENTIAL
      TEXPSL(I,K)=EXP(TEXPSL(I,K))
125   CONTINUE
!***COMPUTE OPTICAL PATHS FOR H2O AND O3, USING THE DIFFUSIVITY
!   APPROXIMATION FOR THE ANGULAR INTEGRATION (1.66). OBTAIN THE
!   UNWEIGHTED VALUES(VAR1,VAR3) AND THE WEIGHTED VALUES(VAR2,VAR4).
!   THE QUANTITIES H3M4(.0003) AND H3M3(.003) APPEARING IN THE VAR2 AND
!   VAR4 EXPRESSIONS ARE THE APPROXIMATE VOIGT CORRECTIONS FOR H2O AND
!   O3,RESPECTIVELY.
!
      DO 131 K=1,L
      DO 131 I=MYIS,MYIE
      QH2O(I,K)=RH2O(I,K)*DIFFCTR
!---VV IS THE LAYER-MEAN PRESSURE (IN ATM),WHICH IS NOT THE SAME AS
!   THE LEVEL PRESSURE (PRESS)
      VV(I,K)=HAF*(P(I,K+1)+P(I,K))*P0INV
      VAR1(I,K)=DELP2(I,K)*QH2O(I,K)*GINV
      VAR3(I,K)=DELP2(I,K)*QO3(I,K)*DIFFCTR*GINV
      VAR2(I,K)=VAR1(I,K)*(VV(I,K)+H3M4)
      VAR4(I,K)=VAR3(I,K)*(VV(I,K)+H3M3)
!  COMPUTE OPTICAL PATH FOR THE H2O CONTINUUM, USING ROBERTS COEFFS.
!  (BETINW),AND TEMP. CORRECTION (TEXPSL). THE DIFFUSIVITY FACTOR
!  (WHICH CANCELS OUT IN THIS EXPRESSION) IS ASSUMED TO BE 1.66. THE
!  USE OF THE DIFFUSIVITY FACTOR HAS BEEN SHOWN TO BE A SIGNIFICANT
!  SOURCE OF ERROR IN THE CONTINUUM CALCS.,BUT THE TIME PENALTY OF
!  AN ANGULAR INTEGRATION IS SEVERE.
!
      CNTVAL(I,K)=TEXPSL(I,K)*RH2O(I,K)*VAR2(I,K)*BETINW/ &
                   (RH2O(I,K)+RATH2OMW)
131   CONTINUE
!   COMPUTE SUMMED OPTICAL PATHS FOR H2O,O3 AND CONTINUUM
      DO 201 I=MYIS,MYIE
      TOTPHI(I,1)=ZERO
      TOTO3(I,1)=ZERO
      TPHIO3(I,1)=ZERO
      TOTVO2(I,1)=ZERO
201   CONTINUE
      DO 203 K=2,LP1
      DO 203 I=MYIS,MYIE
      TOTPHI(I,K)=TOTPHI(I,K-1)+VAR2(I,K-1)
      TOTO3(I,K)=TOTO3(I,K-1)+VAR3(I,K-1)
      TPHIO3(I,K)=TPHIO3(I,K-1)+VAR4(I,K-1)
      TOTVO2(I,K)=TOTVO2(I,K-1)+CNTVAL(I,K-1)
203   CONTINUE
!---EMX1 IS THE ADDITIONAL PRESSURE-SCALED MASS FROM PRESS(L) TO
!   P(L). IT IS USED IN NEARBY LAYER AND EMISS CALCULATIONS.
!---EMX2 IS THE ADDITIONAL PRESSURE-SCALED MASS FROM PRESS(L) TO
!   P(LP1). IT IS USED IN CALCULATIONS BETWEEN FLUX LEVELS L AND LP1.
!
      DO 801 I=MYIS,MYIE
      EMX1(I)=QH2O(I,L)*PRESS(I,L)*(PRESS(I,L)-P(I,L))*GP0INV
      EMX2(I)=QH2O(I,L)*PRESS(I,L)*(P(I,LP1)-PRESS(I,L))*GP0INV
801   CONTINUE
!---EMPL IS THE PRESSURE SCALED MASS FROM P(K) TO PRESS(K) (INDEX 2-LP1)
!   OR TO PRESS(K+1) (INDEX LP2-LL)
      DO 811 K=1,L
      DO 811 I=MYIS,MYIE
      EMPL(I,K+1)=QH2O(I,K)*P(I,K+1)*(P(I,K+1)-PRESS(I,K))*GP0INV
811   CONTINUE
      DO 812 K=1,LM1
      DO 812 I=MYIS,MYIE
      EMPL(I,LP2+K-1)=QH2O(I,K+1)*P(I,K+1)*(PRESS(I,K+1)-P(I,K+1)) &
                     *GP0INV
812   CONTINUE
      DO 821 I=MYIS,MYIE
      EMPL(I,1)=VAR2(I,L)
      EMPL(I,LLP1)=EMPL(I,LL)
821   CONTINUE
!***COMPUTE WEIGHTED TEMPERATURE (TDAV) AND PRESSURE (TSTDAV) INTEGRALS
!   FOR USE IN OBTAINING TEMP. DIFFERENCE BET. SOUNDING AND STD.
!   TEMP. SOUNDING (DIFT)
      DO 161 I=MYIS,MYIE
      TSTDAV(I,1)=ZERO
      TDAV(I,1)=ZERO
161   CONTINUE
      DO 162 K=1,LP1
      DO 162 I=MYIS,MYIE
      VSUM3(I,K)=TEMP(I,K)-STEMP(K)
162   CONTINUE
      DO 163 K=1,L
      DO 165 I=MYIS,MYIE
      VSUM2(I)=GTEMP(K)*DELP2(I,K)
      VSUM1(I)=VSUM2(I)*VSUM3(I,K)
      TSTDAV(I,K+1)=TSTDAV(I,K)+VSUM2(I)
      TDAV(I,K+1)=TDAV(I,K)+VSUM1(I)
165   CONTINUE
163   CONTINUE
!
!****EVALUATE COEFFICIENTS FOR CO2 PRESSURE INTERPOLATION (A1,A2)
      DO 171 I=MYIS,MYIE
      A1(I)=(PRESS(I,LP1)-P0XZP8)/P0XZP2
      A2(I)=(P0-PRESS(I,LP1))/P0XZP2
171   CONTINUE
!***PERFORM CO2 PRESSURE INTERPOLATION ON ALL INPUTTED TRANSMISSION
!   FUNCTIONS AND TEMP. DERIVATIVES
!---SUCCESSIVELY COMPUTING CO2R,DCO2DT AND D2CDT2 IS DONE TO SAVE
!   STORAGE (AT A SLIGHT LOSS IN COMPUTATION TIME)
      DO 184 K=1,LP1
      DO 184 I=MYIS,MYIE
        CO2R1(I,K)=A1(I)*CO231(K)+A2(I)*CO238(K)
        D2CD21(I,K)=H1M3*(A1(I)*C2D31(K)+A2(I)*C2D38(K))
        DCO2D1(I,K)=H1M2*(A1(I)*CDT31(K)+A2(I)*CDT38(K))
        CO2R2(I,K)=A1(I)*CO271(K)+A2(I)*CO278(K)
        D2CD22(I,K)=H1M3*(A1(I)*C2D71(K)+A2(I)*C2D78(K))
        DCO2D2(I,K)=H1M2*(A1(I)*CDT71(K)+A2(I)*CDT78(K))
184   CONTINUE
      DO 190 K=1,L
      DO 190 I=MYIS,MYIE
        CO2MR(I,K)=A1(I)*CO2M51(K)+A2(I)*CO2M58(K)
        CO2MD(I,K)=H1M2*(A1(I)*CDTM51(K)+A2(I)*CDTM58(K))
        CO2M2D(I,K)=H1M3*(A1(I)*C2DM51(K)+A2(I)*C2DM58(K))
190   CONTINUE
!***COMPUTE CO2 TEMPERATURE INTERPOLATIONS FOR ALL BANDS,USING DIFT
!
!   THE CASE WHERE K=1 IS HANDLED FIRST. WE ARE NOW REPLACING
!   3-DIMENSIONAL ARRAYS BY 2-D ARRAYS, TO SAVE SPACE. THUS THIS
!   CALCULATION IS FOR (I,KP,1)
      DO 211 KP=2,LP1
      DO 211 I=MYIS,MYIE
      DIFT(I,KP)=TDAV(I,KP)/TSTDAV(I,KP)
211   CONTINUE
      DO 212 I=MYIS,MYIE
      CO21(I,1,1)=1.0
      CO2SP1(I,1)=1.0
      CO2SP2(I,1)=1.0
212   CONTINUE
      DO 215 KP=2,LP1
      DO 215 I=MYIS,MYIE
!---CALCULATIONS FOR KP>1 FOR K=1
      CO2R(I,KP)=A1(I)*CO251(KP,1)+A2(I)*CO258(KP,1)
      DCO2DT(I,KP)=H1M2*(A1(I)*CDT51(KP,1)+A2(I)*CDT58(KP,1))
      D2CDT2(I,KP)=H1M3*(A1(I)*C2D51(KP,1)+A2(I)*C2D58(KP,1))
      CO21(I,KP,1)=CO2R(I,KP)+DIFT(I,KP)*(DCO2DT(I,KP)+ &
                   HAF*DIFT(I,KP)*D2CDT2(I,KP))
!---CALCULATIONS FOR (EFFECTIVELY) KP=1,K>KP. THESE USE THE
!   SAME VALUE OF DIFT DUE TO SYMMETRY
      CO2R(I,KP)=A1(I)*CO251(1,KP)+A2(I)*CO258(1,KP)
      DCO2DT(I,KP)=H1M2*(A1(I)*CDT51(1,KP)+A2(I)*CDT58(1,KP))
      D2CDT2(I,KP)=H1M3*(A1(I)*C2D51(1,KP)+A2(I)*C2D58(1,KP))
      CO21(I,1,KP)=CO2R(I,KP)+DIFT(I,KP)*(DCO2DT(I,KP)+ &
                   HAF*DIFT(I,KP)*D2CDT2(I,KP))
215   CONTINUE
!   THE TRANSMISSION FUNCTIONS USED IN SPA88 MAY BE COMPUTED NOW.
!---(IN THE 250 LOOP,DIFT REALLY SHOULD BE (I,1,K), BUT DIFT IS
!    INVARIANT WITH RESPECT TO K,KP,AND SO (I,1,K)=(I,K,1))
      DO 250 K=2,LP1
      DO 250 I=MYIS,MYIE
      CO2SP1(I,K)=CO2R1(I,K)+DIFT(I,K)*(DCO2D1(I,K)+HAF*DIFT(I,K)* &
       D2CD21(I,K))
      CO2SP2(I,K)=CO2R2(I,K)+DIFT(I,K)*(DCO2D2(I,K)+HAF*DIFT(I,K)* &
       D2CD22(I,K))
250   CONTINUE
!
!   NEXT THE CASE WHEN K=2...L
      DO 220 K=2,L
      DO 222 KP=K+1,LP1
      DO 222 I=MYIS,MYIE
      DIFT(I,KP)=(TDAV(I,KP)-TDAV(I,K))/ &
                    (TSTDAV(I,KP)-TSTDAV(I,K))
      CO2R(I,KP)=A1(I)*CO251(KP,K)+A2(I)*CO258(KP,K)
      DCO2DT(I,KP)=H1M2*(A1(I)*CDT51(KP,K)+A2(I)*CDT58(KP,K))
      D2CDT2(I,KP)=H1M3*(A1(I)*C2D51(KP,K)+A2(I)*C2D58(KP,K))
      CO21(I,KP,K)=CO2R(I,KP)+DIFT(I,KP)*(DCO2DT(I,KP)+ &
                   HAF*DIFT(I,KP)*D2CDT2(I,KP))
      CO2R(I,KP)=A1(I)*CO251(K,KP)+A2(I)*CO258(K,KP)
      DCO2DT(I,KP)=H1M2*(A1(I)*CDT51(K,KP)+A2(I)*CDT58(K,KP))
      D2CDT2(I,KP)=H1M3*(A1(I)*C2D51(K,KP)+A2(I)*C2D58(K,KP))
      CO21(I,K,KP)=CO2R(I,KP)+DIFT(I,KP)*(DCO2DT(I,KP)+ &
                   HAF*DIFT(I,KP)*D2CDT2(I,KP))
222   CONTINUE
220   CONTINUE
!   FINALLY THE CASE WHEN K=KP,K=2..LP1
      DO 206 K=2,LP1
      DO 206 I=MYIS,MYIE
      DIFT(I,K)=HAF*(VSUM3(I,K)+VSUM3(I,K-1))
      CO2R(I,K)=A1(I)*CO251(K,K)+A2(I)*CO258(K,K)
      DCO2DT(I,K)=H1M2*(A1(I)*CDT51(K,K)+A2(I)*CDT58(K,K))
      D2CDT2(I,K)=H1M3*(A1(I)*C2D51(K,K)+A2(I)*C2D58(K,K))
      CO21(I,K,K)=CO2R(I,K)+DIFT(I,K)*(DCO2DT(I,K)+ &
                   HAF*DIFT(I,K)*D2CDT2(I,K))
206   CONTINUE
!--- WE AREN'T DOING NBL TFS ON THE 100 CM-1 BANDS .
      DO 260 K=1,L
      DO 260 I=MYIS,MYIE
      CO2NBL(I,K)=CO2MR(I,K)+VSUM3(I,K)*(CO2MD(I,K)+HAF* &
       VSUM3(I,K)*CO2M2D(I,K))
260   CONTINUE
!***COMPUTE TEMP. COEFFICIENT BASED ON T(K) (SEE REF.2)
      DO 264 K=1,LP1
      DO 264 I=MYIS,MYIE
      IF (T(I,K).LE.H25E2) THEN
         TLSQU(I,K)=B0+(T(I,K)-H25E2)* &
                            (B1+(T(I,K)-H25E2)* &
                         (B2+B3*(T(I,K)-H25E2)))
      ELSE
         TLSQU(I,K)=B0
      ENDIF
264   CONTINUE
!***APPLY TO ALL CO2 TFS
      DO 280 K=1,LP1
      DO 282 KP=1,LP1
      DO 282 I=MYIS,MYIE
      CO21(I,KP,K)=CO21(I,KP,K)*(ONE-TLSQU(I,KP))+TLSQU(I,KP)
282   CONTINUE
280   CONTINUE
      DO 284 K=1,LP1
      DO 286 I=MYIS,MYIE
      CO2SP1(I,K)=CO2SP1(I,K)*(ONE-TLSQU(I,1))+TLSQU(I,1)
      CO2SP2(I,K)=CO2SP2(I,K)*(ONE-TLSQU(I,1))+TLSQU(I,1)
286   CONTINUE
284   CONTINUE
      DO 288 K=1,L
      DO 290 I=MYIS,MYIE
      CO2NBL(I,K)=CO2NBL(I,K)*(ONE-TLSQU(I,K))+TLSQU(I,K)
290   CONTINUE
288   CONTINUE
!     CALL FST88(HEATRA,GRNFLX,TOPFLX, &
!                QH2O,PRESS,P,DELP,DELP2,TEMP,T, &
!                CLDFAC,NCLDS,KTOP,KBTM,CAMT, &
!                CO21,CO2NBL,CO2SP1,CO2SP2, &
!                VAR1,VAR2,VAR3,VAR4,CNTVAL, &
!                TOTO3,TPHIO3,TOTPHI,TOTVO2, &
!                EMX1,EMX2,EMPL, &
!
!                BO3RND,AO3RND, &
!!               T1,T2,T4 , EM1V,EM1VW, EM3V, &
!                APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
!                TEN,HP1,HAF,ONE,FOUR,HM1EZ,SKO3R, &
!                AB15WD,SKC1R,RADCON,QUARTR,TWO, &
!                HM6666M2,HMP66667,HMP5, &
!                HP166666,H41666M2,RADCON1, &
!                H16E1, H28E1, H25E2, H44194M2,H1P41819, &
!                SKO2D,                                        &
!                ids,ide, jds,jde, kds,kde,                    &
!                ims,ime, jms,jme, kms,kme,                    &
!                its,ite, jts,jte, kts,kte                     )

      CALL FST88(HEATRA,GRNFLX,TOPFLX, &
                 QH2O,PRESS,P,DELP,DELP2,TEMP,T, &
                 CLDFAC,NCLDS,KTOP,KBTM,CAMT, &
                 CO21,CO2NBL,CO2SP1,CO2SP2, &
                 VAR1,VAR2,VAR3,VAR4,CNTVAL, &
                 TOTO3,TPHIO3,TOTPHI,TOTVO2, &
                 EMX1,EMX2,EMPL, &
!
                 BO3RND,AO3RND, &
!                T1,T2,T4 , EM1V,EM1VW, EM3V, &
                 APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
                 TEN,HP1,HAF,ONE,FOUR,HM1EZ,       &
                 RADCON,QUARTR,TWO,  &
                 HM6666M2,HMP66667,HMP5, &
                 HP166666,H41666M2,RADCON1, &
                 H16E1, H28E1, H25E2, H44194M2,H1P41819, &
                 ids,ide, jds,jde, kds,kde,                    &
                 ims,ime, jms,jme, kms,kme,                    &
                 its,ite, jts,jte, kts,kte                     )

  END SUBROUTINE LWR88
!---------------------------------------------------------------------
! SUBROUTINE FST88(HEATRA,GRNFLX,TOPFLX, &
!                      QH2O,PRESS,P,DELP,DELP2,TEMP,T, &
!                      CLDFAC,NCLDS,KTOP,KBTM,CAMT, &
!                      CO21,CO2NBL,CO2SP1,CO2SP2, &
!                      VAR1,VAR2,VAR3,VAR4,CNTVAL, &
!                      TOTO3,TPHIO3,TOTPHI,TOTVO2, &
!                      EMX1,EMX2,EMPL, &
!                      BO3RND,AO3RND, &
!!                     T1,T2,T4 , EM1V,EM1VW, EM3V, &
!                      APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
!                      TEN,HP1,HAF,ONE,FOUR,HM1EZ,SKO3R, &
!                      AB15WD,SKC1R,RADCON,QUARTR,TWO, &
!                      HM6666M2,HMP66667,HMP5, &
!                      HP166666,H41666M2,RADCON1, &
!                      H16E1, H28E1, H25E2, H44194M2,H1P41819, &
!                      SKO2D,                                        &
!                      ids,ide, jds,jde, kds,kde,                    &
!                      ims,ime, jms,jme, kms,kme,                    &
!                      its,ite, jts,jte, kts,kte                     )

  SUBROUTINE FST88(HEATRA,GRNFLX,TOPFLX, &
                       QH2O,PRESS,P,DELP,DELP2,TEMP,T, &
                       CLDFAC,NCLDS,KTOP,KBTM,CAMT, &
                       CO21,CO2NBL,CO2SP1,CO2SP2, &
                       VAR1,VAR2,VAR3,VAR4,CNTVAL, &
                       TOTO3,TPHIO3,TOTPHI,TOTVO2, &
                       EMX1,EMX2,EMPL, &
                       BO3RND,AO3RND, &
!                      T1,T2,T4 , EM1V,EM1VW, EM3V, &
                       APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
                       TEN,HP1,HAF,ONE,FOUR,HM1EZ,       &
                       RADCON,QUARTR,TWO, &
                       HM6666M2,HMP66667,HMP5, &
                       HP166666,H41666M2,RADCON1, &
                       H16E1, H28E1, H25E2, H44194M2,H1P41819, &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       its,ite, jts,jte, kts,kte                     )
!---------------------------------------------------------------------
 IMPLICIT NONE
!----------------------------------------------------------------------
!     INTEGER, PARAMETER :: NBLY=15

      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte

!     REAL,    INTENT(IN)        :: TEN,HP1,HAF,ONE,FOUR,HM1EZ,SKO3R
      REAL,    INTENT(IN)        :: TEN,HP1,HAF,ONE,FOUR,HM1EZ
!     REAL,    INTENT(IN)        :: AB15WD,SKC1R,RADCON,QUARTR,TWO
      REAL,    INTENT(IN)        :: RADCON,QUARTR,TWO
      REAL,    INTENT(IN)        :: HM6666M2,HMP66667,HMP5
      REAL,    INTENT(IN)        :: HP166666,H41666M2,RADCON1,H16E1, H28E1 
!     REAL,    INTENT(IN)        :: H25E2,H44194M2,H1P41819,SKO2D
      REAL,    INTENT(IN)        :: H25E2,H44194M2,H1P41819

      REAL,INTENT(IN),DIMENSION(NBLY) :: APCM,BPCM,ATPCM,BTPCM,ACOMB, &
                                         BCOMB,BETACM

!     REAL, INTENT(IN), DIMENSION(5040) :: T1,T2,T4,EM1V,EM1VW
!     REAL, INTENT(IN), DIMENSION(5040) :: EM3V
      REAL, INTENT(IN), DIMENSION(its:ite,kts:kte*2+1) :: EMPL
      REAL, INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: TOTO3,TPHIO3,TOTPHI,CNTVAL,&
                                                        CO2SP1,CO2SP2   

      REAL,    INTENT(IN),DIMENSION(its:ite,kts:kte+1,kts:kte+1) :: CLDFAC
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: CAMT,TOTVO2
      INTEGER, INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: KBTM,KTOP
      INTEGER, INTENT(IN), DIMENSION(its:ite)           :: NCLDS
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte)   :: QH2O
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: PRESS,TEMP
      REAL,    INTENT(OUT), DIMENSION(its:ite,kts:kte)  :: HEATRA
      REAL,    INTENT(OUT), DIMENSION(its:ite)          :: GRNFLX,TOPFLX
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: P,T
      REAL,    INTENT(INOUT), DIMENSION(its:ite,kts:kte+1,kts:kte+1) :: CO21
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte)   :: CO2NBL,DELP2, &
                                                           DELP,&
                                               VAR1,VAR2,VAR3,VAR4
      REAL, INTENT(IN), DIMENSION(3) :: BO3RND,AO3RND
      REAL, INTENT(IN), DIMENSION(its:ite)   :: EMX1,EMX2
      
      REAL, DIMENSION(its:ite,kts:kte*2+1) :: TPL,EMD,ALP,C,CSUB,CSUB2
      REAL, DIMENSION(its:ite,kts:kte*2+1) :: C2
      INTEGER, DIMENSION(its:ite,kts:kte+1) :: IXO
      REAL, DIMENSION(its:ite,kts:kte+1) :: VTMP3,FXO,DT,FXOE2,DTE2, &
                                            SS1,CSOUR,TC,OSS,CSS,DTC,SS2,&
                                            AVEPHI,E1CTS1,E1FLX,  &
                                            E1CTW1,DSORC,EMISS,FAC1,&
                                            TO3SP,OVER1D,CNTTAU,TOTEVV,&
                                            CO2SP,FLX,AVMO3, &
                                            AVPHO3,AVVO2,CONT1D,TO31D,EMISDG,&
                                            DELPR1
      REAL, DIMENSION(its:ite,kts:kte+1) :: EMISSB,DELPR2,CONTDG,TO3DG,HEATEM,&
                                            VSUM1,FLXNET,Z1

      REAL, DIMENSION(its:ite,kts:kte+1,NBLY) :: SORC
      REAL, DIMENSION(its:ite,kts:kte)   :: E1CTS2,E1CTW2,TO3SPC,RLOG,EXCTS,&
                                            CTSO3,CTS
      REAL, DIMENSION(its:ite)   :: GXCTS,FLX1E1
      REAL, DIMENSION(its:ite)   :: PTOP,PBOT,FTOP,FBOT,DELPTC
      REAL, DIMENSION(its:ite,2) :: FXOSP,DTSP,EMSPEC
!     REAL, DIMENSION(28,NBLY) :: SOURCE,DSRCE
      INTEGER :: K, I,KP,LLM2,J1,J3,KMAX,KMIN,KCLDS,ICNT,LLM1
      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE,LLP1,LL,KK,KLEN

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3; LLP1 = 2*L + 1
      LM1=L-1;  LM2=L-2;  LM3=L-3; LL = 2*L
      LLM2 = LL-2; LLM1=LL-1
      MYIS=its; MYIE=ite

!
      DO 101 K=1,LP1
      DO 101 I=MYIS,MYIE
!---TEMP. INDICES FOR E1,SOURCE
      VTMP3(I,K)=AINT(TEMP(I,K)*HP1)
      FXO(I,K)=VTMP3(I,K)-9.
      DT(I,K)=TEMP(I,K)-TEN*VTMP3(I,K)
!---INTEGER INDEX FOR SOURCE (USED IMMEDIATELY)
      IXO(I,K)=FXO(I,K)
101   CONTINUE
      DO 103 k=1,L
      DO 103 I=MYIS,MYIE
!---TEMP. INDICES FOR E2 (KP=1 LAYER NOT USED IN FLUX CALCULATIONS)
      VTMP3(I,K)=AINT(T(I,K+1)*HP1)
      FXOE2(I,K)=VTMP3(I,K)-9.
      DTE2(I,K)=T(I,K+1)-TEN*VTMP3(I,K)
103   CONTINUE
!---SPECIAL CASE TO HANDLE KP=LP1 LAYER AND SPECIAL E2 CALCS.
      DO 105 I=MYIS,MYIE
      FXOE2(I,LP1)=FXO(I,L)
      DTE2(I,LP1)=DT(I,L)
      FXOSP(I,1)=FXOE2(I,LM1)
      FXOSP(I,2)=FXO(I,LM1)
      DTSP(I,1)=DTE2(I,LM1)
      DTSP(I,2)=DT(I,LM1)
105   CONTINUE
!
!---SOURCE FUNCTION FOR COMBINED BAND 1
      DO 4114 I=MYIS,MYIE
      DO 4114 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),1)
        DSORC(I,K)=DSRCE(IXO(I,K),1)
4114   CONTINUE
      DO 4112 K=1,LP1
      DO 4112 I=MYIS,MYIE
      SORC(I,K,1)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4112   CONTINUE
!---SOURCE FUNCTION FOR COMBINED BAND 2
      DO 4214 I=MYIS,MYIE
      DO 4214 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),2)
        DSORC(I,K)=DSRCE(IXO(I,K),2)
4214   CONTINUE
      DO 4212 K=1,LP1
      DO 4212 I=MYIS,MYIE
      SORC(I,K,2)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4212   CONTINUE
!---SOURCE FUNCTION FOR COMBINED BAND 3
      DO 4314 I=MYIS,MYIE
      DO 4314 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),3)
        DSORC(I,K)=DSRCE(IXO(I,K),3)
4314   CONTINUE
      DO 4312 K=1,LP1
      DO 4312 I=MYIS,MYIE
      SORC(I,K,3)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4312   CONTINUE
!---SOURCE FUNCTION FOR COMBINED BAND 4
      DO 4414 I=MYIS,MYIE
      DO 4414 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),4)
        DSORC(I,K)=DSRCE(IXO(I,K),4)
4414   CONTINUE
      DO 4412 K=1,LP1
      DO 4412 I=MYIS,MYIE
      SORC(I,K,4)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4412   CONTINUE
!---SOURCE FUNCTION FOR COMBINED BAND 5
      DO 4514 I=MYIS,MYIE
      DO 4514 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),5)
        DSORC(I,K)=DSRCE(IXO(I,K),5)
4514   CONTINUE
      DO 4512 K=1,LP1
      DO 4512 I=MYIS,MYIE
      SORC(I,K,5)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4512   CONTINUE
!---SOURCE FUNCTION FOR COMBINED BAND 6
      DO 4614 I=MYIS,MYIE
      DO 4614 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),6)
        DSORC(I,K)=DSRCE(IXO(I,K),6)
4614   CONTINUE
      DO 4612 K=1,LP1
      DO 4612 I=MYIS,MYIE
      SORC(I,K,6)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4612   CONTINUE
!---SOURCE FUNCTION FOR COMBINED BAND 7
      DO 4714 I=MYIS,MYIE
      DO 4714 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),7)
        DSORC(I,K)=DSRCE(IXO(I,K),7)
4714   CONTINUE
      DO 4712 K=1,LP1
      DO 4712 I=MYIS,MYIE
      SORC(I,K,7)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4712   CONTINUE
!---SOURCE FUNCTION FOR COMBINED BAND 8
      DO 4814 I=MYIS,MYIE
      DO 4814 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),8)
        DSORC(I,K)=DSRCE(IXO(I,K),8)
4814   CONTINUE
      DO 4812 K=1,LP1
      DO 4812 I=MYIS,MYIE
      SORC(I,K,8)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4812   CONTINUE
!---SOURCE FUNCTION FOR BAND 9 (560-670 CM-1)
      DO 4914 I=MYIS,MYIE
      DO 4914 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),9)
        DSORC(I,K)=DSRCE(IXO(I,K),9)
4914   CONTINUE
      DO 4912 K=1,LP1
      DO 4912 I=MYIS,MYIE
      SORC(I,K,9)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4912   CONTINUE
!---SOURCE FUNCTION FOR BAND 10 (670-800 CM-1)
      DO 5014 I=MYIS,MYIE
      DO 5014 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),10)
        DSORC(I,K)=DSRCE(IXO(I,K),10)
5014  CONTINUE
      DO 5012 K=1,LP1
      DO 5012 I=MYIS,MYIE
      SORC(I,K,10)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
5012   CONTINUE
!---SOURCE FUNCTION FOR BAND 11 (800-900 CM-1)
      DO 5114 I=MYIS,MYIE
      DO 5114 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),11)
        DSORC(I,K)=DSRCE(IXO(I,K),11)
5114   CONTINUE
      DO 5112 K=1,LP1
      DO 5112 I=MYIS,MYIE
      SORC(I,K,11)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
5112   CONTINUE
!---SOURCE FUNCTION FOR BAND 12 (900-990 CM-1)
      DO 5214 I=MYIS,MYIE
      DO 5214 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),12)
        DSORC(I,K)=DSRCE(IXO(I,K),12)
5214   CONTINUE
      DO 5212 K=1,LP1
      DO 5212 I=MYIS,MYIE
      SORC(I,K,12)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
5212   CONTINUE
!---SOURCE FUNCTION FOR BAND 13 (990-1070 CM-1)
      DO 5314 I=MYIS,MYIE
      DO 5314 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),13)
        DSORC(I,K)=DSRCE(IXO(I,K),13)
5314   CONTINUE
      DO 5312 K=1,LP1
      DO 5312 I=MYIS,MYIE
      SORC(I,K,13)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
5312   CONTINUE
!---SOURCE FUNCTION FOR BAND 14 (1070-1200 CM-1)
      DO 5414 I=MYIS,MYIE
      DO 5414 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),14)
        DSORC(I,K)=DSRCE(IXO(I,K),14)
5414   CONTINUE
      DO 5412 K=1,LP1
      DO 5412 I=MYIS,MYIE
      SORC(I,K,14)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
5412   CONTINUE
!
!        THE FOLLOWING SUBROUTINE OBTAINS NLTE SOURCE FUNCTION FOR CO2
!
!
!     CALL NLTE
!
!
!---OBTAIN SPECIAL SOURCE FUNCTIONS FOR THE 15 UM BAND (CSOUR)
!   AND THE WINDOW REGION (SS1)
      DO 131 K=1,LP1
      DO 131 I=MYIS,MYIE
      SS1(I,K)=SORC(I,K,11)+SORC(I,K,12)+SORC(I,K,14)
131   CONTINUE
      DO 143 K=1,LP1
      DO 143 I=MYIS,MYIE
      CSOUR(I,K)=SORC(I,K,9)+SORC(I,K,10)
143   CONTINUE
!
!---COMPUTE TEMP**4 (TC) AND VERTICAL TEMPERATURE DIFFERENCES
!   (OSS,CSS,SS2,DTC). ALL THESE WILL BE USED LATER IN FLUX COMPUTA-
!   TIONS.
!
      DO 901 K=1,LP1
      DO 901 I=MYIS,MYIE
      TC(I,K)=(TEMP(I,K)*TEMP(I,K))**2
901   CONTINUE
      DO 903 K=1,L
      DO 903 I=MYIS,MYIE
      OSS(I,K+1)=SORC(I,K+1,13)-SORC(I,K,13)
      CSS(I,K+1)=CSOUR(I,K+1)-CSOUR(I,K)
      DTC(I,K+1)=TC(I,K+1)-TC(I,K)
      SS2(I,K+1)=SS1(I,K+1)-SS1(I,K)
903   CONTINUE
!
!
!---THE FOLLOWIMG IS A DRASTIC REWRITE OF THE RADIATION CODE TO
!    (LARGELY) ELIMINATE THREE-DIMENSIONAL ARRAYS. THE CODE WORKS
!    ON THE FOLLOWING PRINCIPLES:
!
!          LET K = FIXED FLUX LEVEL, KP = VARYING FLUX LEVEL
!          THEN FLUX(K)=SUM OVER KP : (DELTAB(KP)*TAU(KP,K))
!               OVER ALL KP'S, FROM 1 TO LP1.
!
!          WE CAN BREAK DOWN THE CALCULATIONS FOR ALL K'S AS FOLLOWS:
!
!          FOR ALL K'S K=1 TO LP1:
!              FLUX(K)=SUM OVER KP : (DELTAB(KP)*TAU(KP,K))  (1)
!                      OVER ALL KP'S, FROM K+1 TO LP1
!          AND
!              FOR KP FROM K+1 TO LP1:
!                 FLUX(KP) = DELTAB(K)*TAU(K,KP)              (2)
!
!          NOW IF TAU(K,KP)=TAU(KP,K) (SYMMETRICAL ARRAYS)
!          WE CAN COMPUTE A 1-DIMENSIONAL ARRAY TAU1D(KP) FROM
!          K+1 TO LP1, EACH TIME K IS INCREMENTED.
!          EQUATIONS (1) AND (2) THEN BECOME:
!
!             TAU1D(KP) = (VALUES FOR TAU(KP,K) AT THE PARTICULAR K)
!             FLUX(K) = SUM OVER KP : (DELTAB(KP)*TAU1D(KP))   (3)
!             FLUX(KP) = DELTAB(K)*TAU1D(KP)                   (4)
!
!         THE TERMS FOR TAU (K,K) AND OTHER SPECIAL TERMS (FOR
!         NEARBY LAYERS) MUST, OF COURSE, BE HANDLED SEPARATELY, AND
!         WITH CARE.
!
!      COMPUTE "UPPER TRIANGLE" TRANSMISSION FUNCTIONS FOR
!      THE 9.6 UM BAND (TO3SP) AND THE 15 UM BAND (OVER1D). ALSO,
!      THE
!      STAGE 1...COMPUTE O3 ,OVER TRANSMISSION FCTNS AND AVEPHI
!---DO K=1 CALCULATION (FROM FLUX LAYER KK TO THE TOP) SEPARATELY
!   AS VECTORIZATION IS IMPROVED,AND OZONE CTS TRANSMISSIVITY
!   MAY BE EXTRACTED HERE.
      DO 3021 K=1,L
      DO 3021 I=MYIS,MYIE
      AVEPHI(I,K)=TOTPHI(I,K+1)
3021  CONTINUE
!---IN ORDER TO PROPERLY EVALUATE EMISS INTEGRATED OVER THE (LP1)
!   LAYER, A SPECIAL EVALUATION OF EMISS IS DONE. THIS REQUIRES
!   A SPECIAL COMPUTATION OF AVEPHI, AND IT IS STORED IN THE
!   (OTHERWISE VACANT) LP1'TH POSITION
!
      DO 803 I=MYIS,MYIE
      AVEPHI(I,LP1)=AVEPHI(I,LM1)+EMX1(I)
803   CONTINUE
!   COMPUTE FLUXES FOR K=1
      CALL E1E290(E1CTS1,E1CTS2,E1FLX,E1CTW1,E1CTW2,EMISS, &
                  FXO,DT,FXOE2,DTE2,AVEPHI,TEMP,T,         &
!                 T1,T2,T4 ,EM1V,EM1VW,                    &
                  H16E1,TEN,HP1,H28E1,HAF,                 &
                  ids,ide, jds,jde, kds,kde,               &
                  ims,ime, jms,jme, kms,kme,               &
                  its,ite, jts,jte, kts,kte                )

      DO 302 K=1,L
      DO 302 I=MYIS,MYIE
      FAC1(I,K)=BO3RND(2)*TPHIO3(I,K+1)/TOTO3(I,K+1)
      TO3SPC(I,K)=HAF*(FAC1(I,K)* &
          (SQRT(ONE+(FOUR*AO3RND(2)*TOTO3(I,K+1))/FAC1(I,K))-ONE))
!   FOR K=1, TO3SP IS USED INSTEAD OF TO31D (THEY ARE EQUAL IN THIS
!   CASE); TO3SP IS PASSED TO SPA90, WHILE TO31D IS A WORK-ARRAY.
      TO3SP(I,K)=EXP(HM1EZ*(TO3SPC(I,K)+SKO3R*TOTVO2(I,K+1)))
      OVER1D(I,K)=EXP(HM1EZ*(SQRT(AB15WD*TOTPHI(I,K+1))+ &
                  SKC1R*TOTVO2(I,K+1)))
!---BECAUSE ALL CONTINUUM TRANSMISSIVITIES ARE OBTAINED FROM THE
!  2-D QUANTITY CNTTAU (AND ITS RECIPROCAL TOTEVV) WE STORE BOTH
!  OF THESE HERE. FOR K=1, CONT1D EQUALS CNTTAU
      CNTTAU(I,K)=EXP(HM1EZ*TOTVO2(I,K+1))
      TOTEVV(I,K)=1./CNTTAU(I,K)
302   CONTINUE
      DO 3022 K=1,L
      DO 3022 I=MYIS,MYIE
      CO2SP(I,K+1)=OVER1D(I,K)*CO21(I,1,K+1)
3022  CONTINUE
      DO 3023 K=1,L
      DO 3023 I=MYIS,MYIE
      CO21(I,K+1,1)=CO21(I,K+1,1)*OVER1D(I,K)
3023  CONTINUE
!---RLOG IS THE NBL AMOUNT FOR THE 15 UM BAND CALCULATION
      DO 1808 I=MYIS,MYIE
      RLOG(I,1)=OVER1D(I,1)*CO2NBL(I,1)
1808  CONTINUE
!---THE TERMS WHEN KP=1 FOR ALL K ARE THE PHOTON EXCHANGE WITH
!   THE TOP OF THE ATMOSPHERE, AND ARE OBTAINED DIFFERENTLY THAN
!   THE OTHER CALCULATIONS
      DO 305 K=2,LP1
      DO 305 I=MYIS,MYIE
      FLX(I,K)= (TC(I,1)*E1FLX(I,K) &
                +SS1(I,1)*CNTTAU(I,K-1) &
                +SORC(I,1,13)*TO3SP(I,K-1) &
                +CSOUR(I,1)*CO2SP(I,K)) &
                *CLDFAC(I,1,K)
305   CONTINUE
      DO 307 I=MYIS,MYIE
      FLX(I,1)= TC(I,1)*E1FLX(I,1)+SS1(I,1)+SORC(I,1,13) &
                +CSOUR(I,1)
307   CONTINUE
!---THE KP TERMS FOR K=1...
      DO 303 KP=2,LP1
      DO 303 I=MYIS,MYIE
      FLX(I,1)=FLX(I,1)+(OSS(I,KP)*TO3SP(I,KP-1) &
                        +SS2(I,KP)*CNTTAU(I,KP-1) &
                        +CSS(I,KP)*CO21(I,KP,1) &
                        +DTC(I,KP)*EMISS(I,KP-1))*CLDFAC(I,KP,1)
303   CONTINUE
!          SUBROUTINE SPA88 IS CALLED TO OBTAIN EXACT CTS FOR WATER
!     CO2 AND O3, AND APPROXIMATE CTS CO2 AND O3 CALCULATIONS.
!
      CALL SPA88(EXCTS,CTSO3,GXCTS,SORC,CSOUR, &
                 CLDFAC,TEMP,PRESS,VAR1,VAR2, &
                 P,DELP,DELP2,TOTVO2,TO3SP,TO3SPC, &
                 CO2SP1,CO2SP2,CO2SP,              &
                 APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
                 H25E2,ONE,H44194M2,H1P41819,HAF,HM1EZ,TWO,    &
!                SKO2D,RADCON,                                 &
                 RADCON,                                 &
                 ids,ide, jds,jde, kds,kde,                    &
                 ims,ime, jms,jme, kms,kme,                    &
                 its,ite, jts,jte, kts,kte                     )

!
!    THIS SECTION COMPUTES THE EMISSIVITY CTS HEATING RATES FOR 2
!    EMISSIVITY BANDS: THE 0-160,1200-2200 CM-1 BAND AND THE 800-
!    990,1070-1200 CM-1 BAND. THE REMAINING CTS COMTRIBUTIONS ARE
!    CONTAINED IN CTSO3, COMPUTED IN SPA88.
!
      DO 998 I=MYIS,MYIE
      VTMP3(I,1)=1.
998   CONTINUE
      DO 999 K=1,L
      DO 999 I=MYIS,MYIE
      VTMP3(I,K+1)=CNTTAU(I,K)*CLDFAC(I,K+1,1)
999   CONTINUE
      DO 1001 K=1,L
      DO 1001 I=MYIS,MYIE
      CTS(I,K)=RADCON*DELP(I,K)*(TC(I,K)* &
           (E1CTW2(I,K)*CLDFAC(I,K+1,1)-E1CTW1(I,K)*CLDFAC(I,K,1)) + &
            SS1(I,K)*(VTMP3(I,K+1)-VTMP3(I,K)))
1001  CONTINUE
!
      DO 1011 K=1,L
      DO 1011 I=MYIS,MYIE
      VTMP3(I,K)=TC(I,K)*(CLDFAC(I,K,1)*(E1CTS1(I,K)-E1CTW1(I,K)) - &
                        CLDFAC(I,K+1,1)*(E1CTS2(I,K)-E1CTW2(I,K)))
1011  CONTINUE
      DO 1012 I=MYIS,MYIE
      FLX1E1(I)=TC(I,LP1)*CLDFAC(I,LP1,1)* &
                (E1CTS1(I,LP1)-E1CTW1(I,LP1))
1012  CONTINUE
      DO 1014 K=1,L
      DO 1013 I=MYIS,MYIE
      FLX1E1(I)=FLX1E1(I)+VTMP3(I,K)
1013  CONTINUE
1014  CONTINUE
!
!---NOW REPEAT FLUX CALCULATIONS FOR THE K=2..LM1  CASES.
!   CALCULATIONS FOR FLUX LEVEL L AND LP1 ARE DONE SEPARATELY, AS ALL
!   EMISSIVITY AND CO2 CALCULATIONS ARE SPECIAL CASES OR NEARBY LAYERS.
!
      DO 321 K=2,LM1
      KLEN=K
!
      DO 3218 KK=1,LP1-K
      DO 3218 I=MYIS,MYIE
      AVEPHI(I,KK+K-1)=TOTPHI(I,KK+K)-TOTPHI(I,K)
3218  CONTINUE
      DO 1803 I=MYIS,MYIE
      AVEPHI(I,LP1)=AVEPHI(I,LM1)+EMX1(I)
1803   CONTINUE
!---COMPUTE EMISSIVITY FLUXES (E2) FOR THIS CASE. NOTE THAT
!   WE HAVE OMITTED THE NEARBY LATER CASE (EMISS(I,K,K)) AS WELL
!   AS ALL CASES WITH K=L OR LP1. BUT THESE CASES HAVE ALWAYS
!   BEEN HANDLED AS SPECIAL CASES, SO WE MAY AS WELL COMPUTE
!    THEIR FLUXES SEPARASTELY.
!
      CALL E290(EMISSB,EMISS,AVEPHI,KLEN,FXOE2,DTE2,  &
!                      T1,T2,T4,                      &
                       H16E1,HP1,H28E1,HAF,TEN,       &
                       ids,ide, jds,jde, kds,kde,     &
                       ims,ime, jms,jme, kms,kme,     &
                       its,ite, jts,jte, kts,kte      )

      DO 322 KK=1,LP1-K
      DO 322 I=MYIS,MYIE
      AVMO3(I,KK+K-1)=TOTO3(I,KK+K)-TOTO3(I,K)
      AVPHO3(I,KK+K-1)=TPHIO3(I,KK+K)-TPHIO3(I,K)
      AVVO2(I,KK+K-1)=TOTVO2(I,KK+K)-TOTVO2(I,K)
      CONT1D(I,KK+K-1)=CNTTAU(I,KK+K-1)*TOTEVV(I,K-1)
322   CONTINUE
!
      DO 3221 KK=1,LP1-K
      DO 3221 I=MYIS,MYIE
      FAC1(I,K+KK-1)=BO3RND(2)*AVPHO3(I,K+KK-1)/AVMO3(I,K+KK-1)
      VTMP3(I,K+KK-1)=HAF*(FAC1(I,K+KK-1)* &
        (SQRT(ONE+(FOUR*AO3RND(2)*AVMO3(I,K+KK-1))/ &
         FAC1(I,K+KK-1))-ONE))
      TO31D(I,K+KK-1)=EXP(HM1EZ*(VTMP3(I,K+KK-1) &
                         +SKO3R*AVVO2(I,K+KK-1)))
      OVER1D(I,K+KK-1)=EXP(HM1EZ*(SQRT(AB15WD*AVEPHI(I,K+KK-1))+ &
                  SKC1R*AVVO2(I,K+KK-1)))
      CO21(I,K+KK,K)=OVER1D(I,K+KK-1)*CO21(I,K+KK,K)
3221  CONTINUE
      DO 3223 KP=K+1,LP1
      DO 3223 I=MYIS,MYIE
      CO21(I,K,KP)=OVER1D(I,KP-1)*CO21(I,K,KP)
3223  CONTINUE
!---RLOG IS THE NBL AMOUNT FOR THE 15 UM BAND CALCULATION
      DO 1804 I=MYIS,MYIE
      RLOG(I,K)=OVER1D(I,K)*CO2NBL(I,K)
1804  CONTINUE
!---THE KP TERMS FOR ARBIRRARY K..
      DO 3423 KP=K+1,LP1
      DO 3423 I=MYIS,MYIE
      FLX(I,K)=FLX(I,K)+(OSS(I,KP)*TO31D(I,KP-1) &
                        +SS2(I,KP)*CONT1D(I,KP-1) &
                        +CSS(I,KP)*CO21(I,KP,K) &
                        +DTC(I,KP)*EMISS(I,KP-1))*CLDFAC(I,KP,K)
3423  CONTINUE
      DO 3425 KP=K+1,LP1
      DO 3425 I=MYIS,MYIE
      FLX(I,KP)=FLX(I,KP)+(OSS(I,K)*TO31D(I,KP-1) &
                         +SS2(I,K)*CONT1D(I,KP-1) &
                         +CSS(I,K)*CO21(I,K,KP) &
                         +DTC(I,K)*EMISSB(I,KP-1))*CLDFAC(I,K,KP)
3425  CONTINUE
321   CONTINUE
!
      DO 821 I=MYIS,MYIE
      TPL(I,1)=TEMP(I,L)
      TPL(I,LP1)=HAF*(T(I,LP1)+TEMP(I,L))
      TPL(I,LLP1)=HAF*(T(I,L)+TEMP(I,L))
821   CONTINUE
      DO 823 K=2,L
      DO 823 I=MYIS,MYIE
      TPL(I,K)=T(I,K)
      TPL(I,K+L)=T(I,K)
823   CONTINUE
!
!---E2 FUNCTIONS ARE REQUIRED IN THE NBL CALCULATIONS FOR 2 CASES,
!   DENOTED (IN OLD CODE) AS (L,LP1) AND (LP1,LP1)
      DO 833 I=MYIS,MYIE
      AVEPHI(I,1)=VAR2(I,L)
      AVEPHI(I,2)=VAR2(I,L)+EMPL(I,L)
833   CONTINUE
      CALL E2SPEC(EMISS,AVEPHI,FXOSP,DTSP,                          &
!                     T1,T2,T4, &
                      H16E1,TEN,H28E1,HP1,                          &
                      ids,ide, jds,jde, kds,kde,                    &
                      ims,ime, jms,jme, kms,kme,                    &
                      its,ite, jts,jte, kts,kte                     )

!
!     CALL E3V88 FOR NBL H2O TRANSMISSIVITIES
!          CALL E3V88(EMD,TPL,EMPL,EM3V, &
           CALL E3V88(EMD,TPL,EMPL, &
                      TEN,HP1,H28E1,H16E1,  &
                      ids,ide, jds,jde, kds,kde,                    &
                      ims,ime, jms,jme, kms,kme,                    &
                      its,ite, jts,jte, kts,kte                     )
!
!   COMPUTE NEARBY LAYER AND SPECIAL-CASE TRANSMISSIVITIES FOR EMISS
!    USING METHODS FOR H2O GIVEN IN REF. (4)
      DO 851 K=2,L
      DO 851 I=MYIS,MYIE
      EMISDG(I,K)=EMD(I,K+L)+EMD(I,K)
851   CONTINUE
!
!   NOTE THAT EMX1/2 (PRESSURE SCALED PATHS) ARE NOW COMPUTED IN
!   LWR88
      DO 861 I=MYIS,MYIE
      EMSPEC(I,1)=(EMD(I,1)*EMPL(I,1)-EMD(I,LP1)*EMPL(I,LP1))/ &
       EMX1(I) + QUARTR*(EMISS(I,1)+EMISS(I,2))
      EMISDG(I,LP1)=TWO*EMD(I,LP1)
      EMSPEC(I,2)=TWO*(EMD(I,1)*EMPL(I,1)-EMD(I,LLP1)*EMPL(I,LLP1))/ &
       EMX2(I)
861   CONTINUE
      DO 331 I=MYIS,MYIE
      FAC1(I,L)=BO3RND(2)*VAR4(I,L)/VAR3(I,L)
      VTMP3(I,L)=HAF*(FAC1(I,L)* &
          (SQRT(ONE+(FOUR*AO3RND(2)*VAR3(I,L))/FAC1(I,L))-ONE))
      TO31D(I,L)=EXP(HM1EZ*(VTMP3(I,L)+SKO3R*CNTVAL(I,L)))
      OVER1D(I,L)=EXP(HM1EZ*(SQRT(AB15WD*VAR2(I,L))+ &
                  SKC1R*CNTVAL(I,L)))
      CONT1D(I,L)=CNTTAU(I,L)*TOTEVV(I,LM1)
      RLOG(I,L)=OVER1D(I,L)*CO2NBL(I,L)
331   CONTINUE
      DO 618 K=1,L
      DO 618 I=MYIS,MYIE
      RLOG(I,K)=LOG(RLOG(I,K))
618   CONTINUE
      DO 601 K=1,LM1
      DO 601 I=MYIS,MYIE
      DELPR1(I,K+1)=DELP(I,K+1)*(PRESS(I,K+1)-P(I,K+1))
      ALP(I,LP1+K-1)=-SQRT(DELPR1(I,K+1))*RLOG(I,K+1)
601   CONTINUE
      DO 603 K=1,L
      DO 603 I=MYIS,MYIE
      DELPR2(I,K+1)=DELP(I,K)*(P(I,K+1)-PRESS(I,K))
      ALP(I,K)=-SQRT(DELPR2(I,K+1))*RLOG(I,K)
603   CONTINUE
      DO 625 I=MYIS,MYIE
      ALP(I,LL)=-RLOG(I,L)
      ALP(I,LLP1)=-RLOG(I,L)*SQRT(DELP(I,L)*(P(I,LP1)-PRESS(I,LM1)))
625   CONTINUE
!        THE FIRST COMPUTATION IS FOR THE 15 UM BAND,WITH THE
!     FOR THE COMBINED H2O AND CO2 TRANSMISSION FUNCTION.
!
!       PERFORM NBL COMPUTATIONS FOR THE 15 UM BAND
!***THE STATEMENT FUNCTION SF IN PREV. VERSIONS IS NOW EXPLICITLY
!   EVALUATED.
      DO 631 K=1,LLP1
      DO 631 I=MYIS,MYIE
      C(I,K)=ALP(I,K)*(HMP66667+ALP(I,K)*(QUARTR+ALP(I,K)*HM6666M2))
631   CONTINUE
      DO 641 I=MYIS,MYIE
      CO21(I,LP1,LP1)=ONE+C(I,L)
      CO21(I,LP1,L)=ONE+(DELP2(I,L)*C(I,LL)-(PRESS(I,L)-P(I,L))* &
       C(I,LLM1))/(P(I,LP1)-PRESS(I,L))
      CO21(I,L,LP1)=ONE+((P(I,LP1)-PRESS(I,LM1))*C(I,LLP1)- &
       (P(I,LP1)-PRESS(I,L))*C(I,L))/(PRESS(I,L)-PRESS(I,LM1))
641   CONTINUE
      DO 643 K=2,L
      DO 643 I=MYIS,MYIE
      CO21(I,K,K)=ONE+HAF*(C(I,LM1+K)+C(I,K-1))
643   CONTINUE
!
!    COMPUTE NEARBY-LAYER TRANSMISSIVITIES FOR THE O3 BAND AND FOR THE
!    ONE-BAND CONTINUUM BAND (TO3 AND EMISS2). THE SF2 FUNCTION IS
!    USED. THE METHOD IS THE SAME AS DESCRIBED FOR CO2 IN REF (4).
      DO 651 K=1,LM1
      DO 651 I=MYIS,MYIE
      CSUB(I,K+1)=CNTVAL(I,K+1)*DELPR1(I,K+1)
      CSUB(I,LP1+K-1)=CNTVAL(I,K)*DELPR2(I,K+1)
651   CONTINUE
!---THE SF2 FUNCTION IN PREV. VERSIONS IS NOW EXPLICITLY EVALUATED
      DO 655 K=1,LLM2
      DO 655 I=MYIS,MYIE
      CSUB2(I,K+1)=SKO3R*CSUB(I,K+1)
      C(I,K+1)=CSUB(I,K+1)*(HMP5+CSUB(I,K+1)* &
                (HP166666-CSUB(I,K+1)*H41666M2))
      C2(I,K+1)=CSUB2(I,K+1)*(HMP5+CSUB2(I,K+1)* &
                 (HP166666-CSUB2(I,K+1)*H41666M2))
655   CONTINUE
      DO 661 I=MYIS,MYIE
      CONTDG(I,LP1)=1.+C(I,LLM1)
      TO3DG(I,LP1)=1.+C2(I,LLM1)
661   CONTINUE
      DO 663 K=2,L
      DO 663 I=MYIS,MYIE
      CONTDG(I,K)=ONE+HAF*(C(I,K)+C(I,LM1+K))
      TO3DG(I,K)=ONE+HAF*(C2(I,K)+C2(I,LM1+K))
663   CONTINUE
!---NOW OBTAIN FLUXES
!
!    FOR THE DIAGONAL TERMS...
      DO 871 K=2,LP1
      DO 871 I=MYIS,MYIE
      FLX(I,K)=FLX(I,K)+(DTC(I,K)*EMISDG(I,K) &
                       +SS2(I,K)*CONTDG(I,K) &
                       +OSS(I,K)*TO3DG(I,K) &
                       +CSS(I,K)*CO21(I,K,K))*CLDFAC(I,K,K)
871   CONTINUE
!     FOR THE TWO OFF-DIAGONAL TERMS...
      DO 873 I=MYIS,MYIE
      FLX(I,L)=FLX(I,L)+(CSS(I,LP1)*CO21(I,LP1,L) &
                        +DTC(I,LP1)*EMSPEC(I,2) &
                        +OSS(I,LP1)*TO31D(I,L) &
                        +SS2(I,LP1)*CONT1D(I,L))*CLDFAC(I,LP1,L)
      FLX(I,LP1)=FLX(I,LP1)+(CSS(I,L)*CO21(I,L,LP1) &
                            +OSS(I,L)*TO31D(I,L) &
                            +SS2(I,L)*CONT1D(I,L) &
                            +DTC(I,L)*EMSPEC(I,1))*CLDFAC(I,L,LP1)
873   CONTINUE
!
!     FINAL SECTION OBTAINS EMISSIVITY HEATING RATES,
!     TOTAL HEATING RATES AND THE FLUX AT THE GROUND
!
!     .....CALCULATE THE EMISSIVITY HEATING RATES
      DO 1101 K=1,L
      DO 1101 I=MYIS,MYIE
      HEATEM(I,K)=RADCON*(FLX(I,K+1)-FLX(I,K))*DELP(I,K)
1101  CONTINUE
!     .....CALCULATE THE TOTAL HEATING RATES
      DO 1103 K=1,L
      DO 1103 I=MYIS,MYIE
      HEATRA(I,K)=HEATEM(I,K)-CTS(I,K)-CTSO3(I,K)+EXCTS(I,K)
1103  CONTINUE
!     .....CALCULATE THE FLUX AT EACH FLUX LEVEL USING THE FLUX AT THE
!    TOP (FLX1E1+GXCTS) AND THE INTEGRAL OF THE HEATING RATES (VSUM1)
      DO 1111 K=1,L
      DO 1111 I=MYIS,MYIE
      VSUM1(I,K)=HEATRA(I,K)*DELP2(I,K)*RADCON1
1111  CONTINUE
      DO 1115 I=MYIS,MYIE
      TOPFLX(I)=FLX1E1(I)+GXCTS(I)
      FLXNET(I,1)=TOPFLX(I)
1115  CONTINUE
!---ONLY THE SURFACE VALUE OF FLUX (GRNFLX) IS NEEDED UNLESS
!    THE THICK CLOUD SECTION IS INVOKED.
      DO 1123 K=2,LP1
      DO 1123 I=MYIS,MYIE
      FLXNET(I,K)=FLXNET(I,K-1)+VSUM1(I,K-1)
1123  CONTINUE
      DO 1125 I=MYIS,MYIE
      GRNFLX(I)=FLXNET(I,LP1)
1125  CONTINUE
!
!     THIS IS THE THICK CLOUD SECTION.OPTIONALLY,IF THICK CLOUD
!     FLUXES ARE TO BE "CONVECTIVELY ADJUSTED",IE,DF/DP IS CONSTANT,
!     FOR CLOUDY PART OF GRID POINT, THE FOLLOWING CODE IS EXECUTED.
!***FIRST,COUNT THE NUMBER OF CLOUDS ALONG THE LAT. ROW. SKIP THE
!   ENTIRE THICK CLOUD COMPUTATION OF THERE ARE NO CLOUDS.
      ICNT=0
      DO 1301 I=MYIS,MYIE
      ICNT=ICNT+NCLDS(I)
1301  CONTINUE
      IF (ICNT.EQ.0) GO TO 6999
!---FIND THE MAXIMUM NUMBER OF CLOUDS IN THE LATITUDE ROW
      KCLDS=NCLDS(MYIS)
      DO 2106 I=MYIS,MYIE
      KCLDS=MAX(NCLDS(I),KCLDS)
2106  CONTINUE
!
!
!***OBTAIN THE PRESSURES AND FLUXES OF THE TOP AND BOTTOM OF
!   THE NC'TH CLOUD (IT IS ASSUMED THAT ALL KTOP AND KBTM'S HAVE
!   BEEN DEFINED!).
      DO 1361 KK=1,KCLDS
      KMIN=LP1
      KMAX=0
      DO 1362 I=MYIS,MYIE
        J1=KTOP(I,KK+1)
!       IF (J1.EQ.1) GO TO 1362
        J3=KBTM(I,KK+1)
        IF (J3.GT.J1) THEN
          PTOP(I)=P(I,J1)
          PBOT(I)=P(I,J3+1)
          FTOP(I)=FLXNET(I,J1)
          FBOT(I)=FLXNET(I,J3+1)
!***OBTAIN THE "FLUX DERIVATIVE" DF/DP (DELPTC)
          DELPTC(I)=(FTOP(I)-FBOT(I))/(PTOP(I)-PBOT(I))
          KMIN=MIN(KMIN,J1)
          KMAX=MAX(KMAX,J3)
        ENDIF
1362  CONTINUE
      KMIN=KMIN+1
!***CALCULATE THE TOT. FLUX CHG. FROM THE TOP OF THE CLOUD, FOR
!   ALL LEVELS.
      DO 1365 K=KMIN,KMAX
      DO 1363 I=MYIS,MYIE
!       IF (KTOP(I,KK+1).EQ.1) GO TO 1363
        IF(KTOP(I,KK+1).LT.K .AND. K.LE.KBTM(I,KK+1)) THEN
          Z1(I,K)=(P(I,K)-PTOP(I))*DELPTC(I)+FTOP(I)
!ORIGINAL FLXNET(I,K)=FLXNET(I,K)*(ONE-CAMT(I,KK+1)) +
!ORIGINAL1            Z1(I,K)*CAMT(I,KK+1)
          FLXNET(I,K)=Z1(I,K)
        ENDIF
1363  CONTINUE
1365  CONTINUE
1361  CONTINUE
!***USING THIS FLUX CHG. IN THE CLOUDY PART OF THE GRID BOX, OBTAIN
!   THE NEW FLUXES, WEIGHTING THE CLEAR AND CLOUDY FLUXES:AGAIN, ONLY
!    THE FLUXES IN THICK-CLOUD LEVELS WILL EVENTUALLY BE USED.
!     DO 6051 K=1,LP1
!     DO 6051 I=MYIS,MYIE
!     FLXNET(I,K)=FLXNET(I,K)*(ONE-CAMT(I,NC)) +
!    1            Z1(I,K)*CAMT(I,NC)
!051  CONTINUE
!***MERGE FLXTHK INTO FLXNET FOR APPROPRIATE LEVELS.
!     DO 1401 K=1,LP1
!     DO 1401 I=MYIS,MYIE
!     IF (K.GT.ITOP(I) .AND. K.LE.IBOT(I)
!    1  .AND.  (NC-1).LE.NCLDS(I))  THEN
!          FLXNET(I,K)=FLXTHK(I,K)
!     ENDIF
!401  CONTINUE
!
!******END OF CLOUD LOOP*****
6001  CONTINUE
6999  CONTINUE
!***THE FINAL STEP IS TO RECOMPUTE THE HEATING RATES BASED ON THE
!   REVISED FLUXES:
      DO 6101 K=1,L
      DO 6101 I=MYIS,MYIE
      HEATRA(I,K)=RADCON*(FLXNET(I,K+1)-FLXNET(I,K))*DELP(I,K)
6101  CONTINUE
!     THE THICK CLOUD SECTION ENDS HERE.

  END SUBROUTINE FST88

!----------------------------------------------------------------------

  SUBROUTINE E1E290(G1,G2,G3,G4,G5,EMISS,FXOE1,DTE1,FXOE2,DTE2,      &
                       AVEPHI,TEMP,T,                                &
!                      T1,T2,T4,EM1V,EM1VW,                          &
                       H16E1,TEN,HP1,H28E1,HAF,                      &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       its,ite, jts,jte, kts,kte                     )
!---------------------------------------------------------------------
 IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte
      REAL,INTENT(IN) :: H16E1,TEN,HP1,H28E1,HAF

      REAL,INTENT(OUT),DIMENSION(its:ite,kts:kte+1) :: G1,G4,G3,EMISS
      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte+1) :: FXOE1,DTE1,FXOE2,DTE2
      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte+1) :: AVEPHI,TEMP,T
      REAL,INTENT(OUT),DIMENSION(its:ite,kts:kte)   :: G2,G5
!     REAL,INTENT(IN),DIMENSION(5040):: T1,T2,T4 ,EM1V,EM1VW

      REAL,DIMENSION(its:ite,kts:kte+1) :: TMP3,DU,FYO,WW1,WW2
      INTEGER,DIMENSION(its:ite,kts:kte*3+2)   :: IT1
      INTEGER,DIMENSION(its:ite,kts:kte+1) :: IVAL

!     REAL,DIMENSION(28,180):: EM1,EM1WDE,TABLE1,TABLE2, &
!                              TABLE3
!     EQUIVALENCE (EM1V(1),EM1(1,1)),(EM1VW(1),EM1WDE(1,1))
!     EQUIVALENCE (T1(1),TABLE1(1,1)),(T2(1),TABLE2(1,1)), &
!      (T4(1),TABLE3(1,1))

      INTEGER :: K, I,KP,LLM2,J1,J3,KMAX,KMIN,KCLDS,ICNT,LLM1
      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE,LLP1,LL,KK,KLEN

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3; LLP1 = 2*L + 1
      LM1=L-1;  LM2=L-2;  LM3=L-3; LL = 2*L
      LLM2 = LL-2; LLM1=LL-1
      MYIS=its; MYIE=ite

!---FIRST WE OBTAIN THE EMISSIVITIES AS A FUNCTION OF TEMPERATURE
!   (INDEX FXO) AND WATER AMOUNT (INDEX FYO). THIS PART OF THE CODE
!   THUS GENERATES THE E2 FUNCTION. THE FXO INDICES HAVE BEEN
!   OBTAINED IN FST88, FOR CONVENIENCE.
!
!---THIS SUBROUTINE EVALUATES THE K=1 CASE ONLY--
!
!---THIS LOOP REPLACES LOOPS GOING FROMI=1,IMAX AND KP=2,LP1 PLUS
!   THE SPECIAL CASE FOR THE LP1TH LAYER.

      DO 1322 K=1,LP1
      DO 1322 I=MYIS,MYIE
      TMP3(I,K)=LOG10(AVEPHI(I,K))+H16E1
      FYO(I,K)=AINT(TMP3(I,K)*TEN)
      DU(I,K)=TMP3(I,K)-HP1*FYO(I,K)
      FYO(I,K)=H28E1*FYO(I,K)
      IVAL(I,K)=FYO(I,K)+FXOE2(I,K)
      EMISS(I,K)=T1(IVAL(I,K))+DU(I,K)*T2(IVAL(I,K)) &
                              +DTE2(I,K)*T4(IVAL(I,K))
1322  CONTINUE
!
!---THE SPECIAL CASE EMISS(I,L) (LAYER KP) IS OBTAINED NOW
!   BY AVERAGING THE VALUES FOR L AND LP1:
      DO 1344 I=MYIS,MYIE
      EMISS(I,L)=HAF*(EMISS(I,L)+EMISS(I,LP1))
1344  CONTINUE
!
!   CALCULATIONS FOR THE KP=1 LAYER ARE NOT PERFORMED, AS
!   THE RADIATION CODE ASSUMES THAT THE TOP FLUX LAYER (ABOVE THE
!   TOP DATA LEVEL) IS ISOTHERMAL, AND HENCE CONTRIBUTES NOTHING
!   TO THE FLUXES AT OTHER LEVELS.
!
!***THE FOLLOWING IS THE CALCULATION FOR THE E1 FUNCTION, FORMERLY
!    DONE IN SUBROUTINE E1V88. THE MOVE TO E1E288 IS DUE TO THE
!    SAVINGS IN OBTAINING INDEX VALUES (THE TEMP. INDICES HAVE
!    BEEN OBTAINED IN FST88, WHILE THE U-INDICES ARE OBTAINED
!    IN THE E2 CALCS.,WITH K=1).
!
!
!   FOR TERMS INVOLVING TOP LAYER, DU IS NOT KNOWN; IN FACT, WE
!   USE INDEX 2 TO REPERSENT INDEX 1 IN PREV. CODE. THIS MEANS THAT
!    THE IT1 INDEX 1 AND LLP1 HAS TO BE CALCULATED SEPARATELY. THE
!   INDEX LLP2 GIVES THE SAME VALUE AS 1; IT CAN BE OMITTED.
      DO 208 I=MYIS,MYIE
      IT1(I,1)=FXOE1(I,1)
      WW1(I,1)=TEN-DTE1(I,1)
      WW2(I,1)=HP1
208   CONTINUE
      DO 209 K=1,L
      DO 209 I=MYIS,MYIE
      IT1(I,K+1)=FYO(I,K)+FXOE1(I,K+1)
      IT1(I,LP2+K-1)=FYO(I,K)+FXOE1(I,K)
      WW1(I,K+1)=TEN-DTE1(I,K+1)
      WW2(I,K+1)=HP1-DU(I,K)
209   CONTINUE
      DO 211 KP=1,L
      DO 211 I=MYIS,MYIE
      IT1(I,KP+LLP1)=FYO(I,KP)+FXOE1(I,1)
211   CONTINUE
!
!
!  G3(I,1) HAS THE SAME VALUES AS G1 (AND DID ALL ALONG)
      DO 230 I=MYIS,MYIE
      G1(I,1)=WW1(I,1)*WW2(I,1)*EM1V(IT1(I,1))+ &
              WW2(I,1)*DTE1(I,1)*EM1V(IT1(I,1)+1)
      G3(I,1)=G1(I,1)
230   CONTINUE
      DO 240 K=1,L
      DO 240 I=MYIS,MYIE
      G1(I,K+1)=WW1(I,K+1)*WW2(I,K+1)*EM1V(IT1(I,K+1))+ &
              WW2(I,K+1)*DTE1(I,K+1)*EM1V(IT1(I,K+1)+1)+ &
              WW1(I,K+1)*DU(I,K)*EM1V(IT1(I,K+1)+28)+ &
              DTE1(I,K+1)*DU(I,K)*EM1V(IT1(I,K+1)+29)
      G2(I,K)=WW1(I,K)*WW2(I,K+1)*EM1V(IT1(I,K+LP2-1))+ &
              WW2(I,K+1)*DTE1(I,K)*EM1V(IT1(I,K+LP2-1)+1)+ &
              WW1(I,K)*DU(I,K)*EM1V(IT1(I,K+LP2-1)+28)+ &
              DTE1(I,K)*DU(I,K)*EM1V(IT1(I,K+LP2-1)+29)
240   CONTINUE
      DO 241 KP=2,LP1
      DO 241 I=MYIS,MYIE
      G3(I,KP)=WW1(I,1)*WW2(I,KP)*EM1V(IT1(I,LL+KP))+ &
              WW2(I,KP)*DTE1(I,1)*EM1V(IT1(I,LL+KP)+1)+ &
              WW1(I,1)*DU(I,KP-1)*EM1V(IT1(I,LL+KP)+28)+ &
              DTE1(I,1)*DU(I,KP-1)*EM1V(IT1(I,LL+KP)+29)
241   CONTINUE
!
      DO 244 I=MYIS,MYIE
      G4(I,1)=WW1(I,1)*WW2(I,1)*EM1VW(IT1(I,1))+ &
              WW2(I,1)*DTE1(I,1)*EM1VW(IT1(I,1)+1)
244   CONTINUE
      DO 242 K=1,L
      DO 242 I=MYIS,MYIE
      G4(I,K+1)=WW1(I,K+1)*WW2(I,K+1)*EM1VW(IT1(I,K+1))+ &
              WW2(I,K+1)*DTE1(I,K+1)*EM1VW(IT1(I,K+1)+1)+ &
              WW1(I,K+1)*DU(I,K)*EM1VW(IT1(I,K+1)+28)+ &
              DTE1(I,K+1)*DU(I,K)*EM1VW(IT1(I,K+1)+29)
      G5(I,K)=WW1(I,K)*WW2(I,K+1)*EM1VW(IT1(I,K+LP2-1))+ &
              WW2(I,K+1)*DTE1(I,K)*EM1VW(IT1(I,K+LP2-1)+1)+ &
              WW1(I,K)*DU(I,K)*EM1VW(IT1(I,K+LP2-1)+28)+ &
              DTE1(I,K)*DU(I,K)*EM1VW(IT1(I,K+LP2-1)+29)
242   CONTINUE
!
  END SUBROUTINE E1E290

!----------------------------------------------------------------------

 SUBROUTINE SPA88(EXCTS,CTSO3,GXCTS,SORC,CSOUR,                      &
                       CLDFAC,TEMP,PRESS,VAR1,VAR2,                  &
                       P,DELP,DELP2,TOTVO2,TO3SP,TO3SPC,             &
                       CO2SP1,CO2SP2,CO2SP,                          &
                       APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
                       H25E2,ONE,H44194M2,H1P41819,HAF,HM1EZ,TWO,    &
!                      SKO2D,RADCON,                                 &
                       RADCON,                                 &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       its,ite, jts,jte, kts,kte                     )
!---------------------------------------------------------------------
 IMPLICIT NONE
!----------------------------------------------------------------------
!     INTEGER, PARAMETER :: NBLY=15
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte

      REAL,INTENT(IN) :: H25E2,ONE,H44194M2,H1P41819,HAF,HM1EZ,TWO, &
                         RADCON
!                        SKO2D,RADCON

      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte+1) :: CSOUR
      REAL,INTENT(OUT),DIMENSION(its:ite,kts:kte)  :: CTSO3
      REAL,INTENT(OUT),DIMENSION(its:ite,kts:kte)  :: EXCTS
      REAL,INTENT(OUT),DIMENSION(its:ite)          :: GXCTS
      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte+1,NBLY) :: SORC
      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte+1,kts:kte+1) :: CLDFAC
      REAL,INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: PRESS,TEMP

      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte) :: VAR1,VAR2 
      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte+1) :: P
      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte)   :: DELP,DELP2,TO3SPC
      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte+1) ::TOTVO2,TO3SP,CO2SP1,&
                                                     CO2SP2,CO2SP
      REAL,INTENT(IN),DIMENSION(NBLY) :: APCM,BPCM,ATPCM,BTPCM,ACOMB, &
                                         BCOMB,BETACM

      REAL,DIMENSION(its:ite,kts:kte+1) ::CTMP,CTMP2,CTMP3
      REAL,DIMENSION(its:ite,kts:kte)   ::X,Y,FAC1,FAC2,F,FF,AG,AGG, &
                                          PHITMP,PSITMP,TOPM,TOPPHI,TT

      INTEGER :: K, I,KP,LLM2,J1,J3,KMAX,KMIN,KCLDS,ICNT,LLM1
      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE,LLP1,LL,KK,KLEN

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3; LLP1 = 2*L + 1
      LM1=L-1;  LM2=L-2;  LM3=L-3; LL = 2*L
      LLM2 = LL-2; LLM1=LL-1
      MYIS=its; MYIE=ite

!--!COMPUTE TEMPERATURE QUANTITIES FOR USE IN PROGRAM

      DO 101 K=1,L
      DO 101 I=MYIS,MYIE
      X(I,K)=TEMP(I,K)-H25E2
      Y(I,K)=X(I,K)*X(I,K)
101   CONTINUE
!---INITIALIZE CTMP(I,1),CTMP2(I,1),CTMP3(I,1) TO UNITY; THESE ARE
!   TRANSMISSION FCTNS AT THE TOP.
      DO 345 I=MYIS,MYIE
      CTMP(I,1)=ONE
      CTMP2(I,1)=1.
      CTMP3(I,1)=1.
345   CONTINUE
!***BEGIN LOOP ON FREQUENCY BANDS (1)***
!
!---CALCULATION FOR BAND 1 (COMBINED BAND 1)
!
!---OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY
!   BY OPTICAL PATH (VAR1,VAR2) TO COMPUTE TEMPERATURE-CORRECTED
!   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
      DO 301 K=1,L
      DO 301 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(1)*X(I,K)+BPCM(1)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(1)*X(I,K)+BTPCM(1)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
301   CONTINUE
!---OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
!   P(K) (TOPM,TOPPHI)
      DO 315 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
315   CONTINUE
      DO 319 K=2,L
      DO 317 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
317   CONTINUE
319   CONTINUE
!---TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
      DO 321 K=1,L
      DO 321 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(1)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(1)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*FAC1(I,K)/SQRT(1.+FAC2(I,K)))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
321   CONTINUE
!---EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
      DO 353 K=1,L
      DO 353 I=MYIS,MYIE
      EXCTS(I,K)=SORC(I,K,1)*(CTMP(I,K+1)-CTMP(I,K))
353   CONTINUE
!---GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
      DO 361 I=MYIS,MYIE
      GXCTS(I)=CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,1)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,1)-SORC(I,L,1)))
361   CONTINUE
!
!
!-----CALCULATION FOR BAND 2 (COMBINED BAND 2)
!
!
!---OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY
!   BY OPTICAL PATH (VAR1,VAR2) TO COMPUTE TEMPERATURE-CORRECTED
!   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
      DO 401 K=1,L
      DO 401 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(2)*X(I,K)+BPCM(2)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(2)*X(I,K)+BTPCM(2)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
401   CONTINUE
!---OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
!   P(K) (TOPM,TOPPHI)
      DO 415 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
415   CONTINUE
      DO 419 K=2,L
      DO 417 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
417   CONTINUE
419   CONTINUE
!---TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
      DO 421 K=1,L
      DO 421 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(2)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(2)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*FAC1(I,K)/SQRT(1.+FAC2(I,K)))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
421   CONTINUE
!---EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
      DO 453 K=1,L
      DO 453 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,2)* & 
                   (CTMP(I,K+1)-CTMP(I,K))
453   CONTINUE
!---GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
      DO 461 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,2)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,2)-SORC(I,L,2)))
461   CONTINUE
!
!-----CALCULATION FOR BAND 3 (COMBINED BAND 3)
!
!
!---OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY
!   BY OPTICAL PATH (VAR1,VAR2) TO COMPUTE TEMPERATURE-CORRECTED
!   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
      DO 501 K=1,L
      DO 501 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(3)*X(I,K)+BPCM(3)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(3)*X(I,K)+BTPCM(3)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
501   CONTINUE
!---OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
!   P(K) (TOPM,TOPPHI)
      DO 515 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
515   CONTINUE
      DO 519 K=2,L
      DO 517 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
517   CONTINUE
519   CONTINUE
!---TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
      DO 521 K=1,L
      DO 521 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(3)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(3)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*FAC1(I,K)/SQRT(1.+FAC2(I,K)))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
521   CONTINUE
!---EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
      DO 553 K=1,L
      DO 553 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,3)* &
                   (CTMP(I,K+1)-CTMP(I,K))
553   CONTINUE
!---GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
      DO 561 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,3)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,3)-SORC(I,L,3)))
561   CONTINUE
!
!-----CALCULATION FOR BAND 4 (COMBINED BAND 4)
!
!
!---OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY
!   BY OPTICAL PATH (VAR1,VAR2) TO COMPUTE TEMPERATURE-CORRECTED
!   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
      DO 601 K=1,L
      DO 601 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(4)*X(I,K)+BPCM(4)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(4)*X(I,K)+BTPCM(4)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
601   CONTINUE
!---OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
!   P(K) (TOPM,TOPPHI)
      DO 615 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
615   CONTINUE
      DO 619 K=2,L
      DO 617 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
617   CONTINUE
619   CONTINUE
!---TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
      DO 621 K=1,L
      DO 621 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(4)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(4)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*FAC1(I,K)/SQRT(1.+FAC2(I,K)))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
621   CONTINUE
!---EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
      DO 653 K=1,L
      DO 653 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,4)* &
                   (CTMP(I,K+1)-CTMP(I,K))
653   CONTINUE
!---GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
      DO 661 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,4)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,4)-SORC(I,L,4)))
661   CONTINUE
!
!-----CALCULATION FOR BAND 5 (COMBINED BAND 5)
!
!
!---OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY
!   BY OPTICAL PATH (VAR1,VAR2) TO COMPUTE TEMPERATURE-CORRECTED
!   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
      DO 701 K=1,L
      DO 701 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(5)*X(I,K)+BPCM(5)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(5)*X(I,K)+BTPCM(5)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
701   CONTINUE
!---OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
!   P(K) (TOPM,TOPPHI)
      DO 715 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
715   CONTINUE
      DO 719 K=2,L
      DO 717 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
717   CONTINUE
719   CONTINUE
!---TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
      DO 721 K=1,L
      DO 721 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(5)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(5)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(5)*TOTVO2(I,K+1)*SKO2D))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
721   CONTINUE
!---EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
      DO 753 K=1,L
      DO 753 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,5)* &
                   (CTMP(I,K+1)-CTMP(I,K))
753   CONTINUE
!---GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
      DO 761 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,5)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,5)-SORC(I,L,5)))
761   CONTINUE
!
!-----CALCULATION FOR BAND 6 (COMBINED BAND 6)
!
!
!---OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY
!   BY OPTICAL PATH (VAR1,VAR2) TO COMPUTE TEMPERATURE-CORRECTED
!   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
      DO 801 K=1,L
      DO 801 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(6)*X(I,K)+BPCM(6)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(6)*X(I,K)+BTPCM(6)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
801   CONTINUE
!---OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
!   P(K) (TOPM,TOPPHI)
      DO 815 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
815   CONTINUE
      DO 819 K=2,L
      DO 817 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
817   CONTINUE
819   CONTINUE
!---TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
      DO 821 K=1,L
      DO 821 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(6)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(6)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(6)*TOTVO2(I,K+1)*SKO2D))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
821   CONTINUE
!---EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
      DO 853 K=1,L
      DO 853 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,6)* &
                   (CTMP(I,K+1)-CTMP(I,K))
853   CONTINUE
!---GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
      DO 861 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,6)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,6)-SORC(I,L,6)))
861   CONTINUE
!
!-----CALCULATION FOR BAND 7 (COMBINED BAND 7)
!
!
!---OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY
!   BY OPTICAL PATH (VAR1,VAR2) TO COMPUTE TEMPERATURE-CORRECTED
!   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
      DO 901 K=1,L
      DO 901 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(7)*X(I,K)+BPCM(7)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(7)*X(I,K)+BTPCM(7)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
901   CONTINUE
!---OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
!   P(K) (TOPM,TOPPHI)
      DO 915 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
915   CONTINUE
      DO 919 K=2,L
      DO 917 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
917   CONTINUE
919   CONTINUE
!---TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
      DO 921 K=1,L
      DO 921 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(7)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(7)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(7)*TOTVO2(I,K+1)*SKO2D))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
921   CONTINUE
!---EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
      DO 953 K=1,L
      DO 953 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,k,7)* &
                   (CTMP(I,K+1)-CTMP(I,K))
953   CONTINUE
!---GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
      DO 961 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,7)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,7)-SORC(I,L,7)))
961   CONTINUE
!
!-----CALCULATION FOR BAND 8 (COMBINED BAND 8)
!
!
!---OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY
!   BY OPTICAL PATH (VAR1,VAR2) TO COMPUTE TEMPERATURE-CORRECTED
!   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
      DO 1001 K=1,L
      DO 1001 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(8)*X(I,K)+BPCM(8)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(8)*X(I,K)+BTPCM(8)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
1001  CONTINUE
!---OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
!   P(K) (TOPM,TOPPHI)
      DO 1015 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
1015  CONTINUE
      DO 1019 K=2,L
      DO 1017 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
1017  CONTINUE
1019  CONTINUE
!---TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
      DO 1021 K=1,L
      DO 1021 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(8)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(8)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(8)*TOTVO2(I,K+1)*SKO2D))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
1021  CONTINUE
!---EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
      DO 1053 K=1,L
      DO 1053 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,8)* &
                   (CTMP(I,K+1)-CTMP(I,K))
1053  CONTINUE
!---GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
      DO 1061 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,8)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,8)-SORC(I,L,8)))
1061  CONTINUE
!
!-----CALCULATION FOR BAND 9 ( 560-670 CM-1; INCLUDES CO2)
!
!
!---OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY
!   BY OPTICAL PATH (VAR1,VAR2) TO COMPUTE TEMPERATURE-CORRECTED
!   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
      DO 1101 K=1,L
      DO 1101 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(9)*X(I,K)+BPCM(9)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(9)*X(I,K)+BTPCM(9)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
1101  CONTINUE
!---OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
!   P(K) (TOPM,TOPPHI)
      DO 1115 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
1115  CONTINUE
      DO 1119 K=2,L
      DO 1117 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
1117  CONTINUE
1119  CONTINUE
!---TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
      DO 1121 K=1,L
      DO 1121 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(9)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(9)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(9)*TOTVO2(I,K+1)*SKO2D))*CO2SP1(I,K+1)
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
1121  CONTINUE
!---EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
      DO 1153 K=1,L
      DO 1153 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,9)* &
                   (CTMP(I,K+1)-CTMP(I,K))
1153  CONTINUE
!---GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
      DO 1161 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,9)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,9)-SORC(I,L,9)))
1161  CONTINUE
!
!-----CALCULATION FOR BAND 10 (670-800 CM-1; INCLUDES CO2)
!
!
!---OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY
!   BY OPTICAL PATH (VAR1,VAR2) TO COMPUTE TEMPERATURE-CORRECTED
!   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
      DO 1201 K=1,L
      DO 1201 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(10)*X(I,K)+BPCM(10)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(10)*X(I,K)+BTPCM(10)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
1201  CONTINUE
!---OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
!   P(K) (TOPM,TOPPHI)
      DO 1215 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
1215  CONTINUE
      DO 1219 K=2,L
      DO 1217 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
1217  CONTINUE
1219  CONTINUE
!---TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
      DO 1221 K=1,L
      DO 1221 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(10)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(10)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(10)*TOTVO2(I,K+1)*SKO2D))*CO2SP2(I,K+1)
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
1221  CONTINUE
!---EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
      DO 1253 K=1,L
      DO 1253 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,10)* &
                   (CTMP(I,K+1)-CTMP(I,K))
1253  CONTINUE
!---GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
      DO 1261 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,10)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,10)-SORC(I,L,10)))
1261  CONTINUE
!
!-----CALCULATION FOR BAND 11 (800-900 CM-1)
!
!
!---OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY
!   BY OPTICAL PATH (VAR1,VAR2) TO COMPUTE TEMPERATURE-CORRECTED
!   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
      DO 1301 K=1,L
      DO 1301 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(11)*X(I,K)+BPCM(11)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(11)*X(I,K)+BTPCM(11)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
1301  CONTINUE
!---OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
!   P(K) (TOPM,TOPPHI)
      DO 1315 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
1315  CONTINUE
      DO 1319 K=2,L
      DO 1317 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
1317  CONTINUE
1319  CONTINUE
!---TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
      DO 1321 K=1,L
      DO 1321 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(11)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(11)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(11)*TOTVO2(I,K+1)*SKO2D))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
1321  CONTINUE
!---EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
      DO 1353 K=1,L
      DO 1353 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,11)* &
                   (CTMP(I,K+1)-CTMP(I,K))
1353  CONTINUE
!---GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
      DO 1361 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,11)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,11)-SORC(I,L,11)))
1361  CONTINUE
!
!-----CALCULATION FOR BAND 12 (900-990 CM-1)
!
!
!---OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY
!   BY OPTICAL PATH (VAR1,VAR2) TO COMPUTE TEMPERATURE-CORRECTED
!   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
      DO 1401 K=1,L
      DO 1401 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(12)*X(I,K)+BPCM(12)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(12)*X(I,K)+BTPCM(12)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
1401  CONTINUE
!---OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
!   P(K) (TOPM,TOPPHI)
      DO 1415 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
1415  CONTINUE
      DO 1419 K=2,L
      DO 1417 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
1417  CONTINUE
1419  CONTINUE
!---TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
      DO 1421 K=1,L
      DO 1421 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(12)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(12)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(12)*TOTVO2(I,K+1)*SKO2D))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
1421  CONTINUE
!---EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
      DO 1453 K=1,L
      DO 1453 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,12)* &
                   (CTMP(I,K+1)-CTMP(I,K))
1453  CONTINUE
!---GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
      DO 1461 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,12)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,12)-SORC(I,L,12)))
1461  CONTINUE
!
!-----CALCULATION FOR BAND 13 (990-1070 CM-1; INCLUDES O3))
!
!
!---OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY
!   BY OPTICAL PATH (VAR1,VAR2) TO COMPUTE TEMPERATURE-CORRECTED
!   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
      DO 1501 K=1,L
      DO 1501 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(13)*X(I,K)+BPCM(13)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(13)*X(I,K)+BTPCM(13)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
1501  CONTINUE
!---OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
!   P(K) (TOPM,TOPPHI)
      DO 1515 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
1515  CONTINUE
      DO 1519 K=2,L
      DO 1517 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
1517  CONTINUE
1519  CONTINUE
!---TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
      DO 1521 K=1,L
      DO 1521 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(13)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(13)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(13)*TOTVO2(I,K+1)*SKO2D+TO3SPC(I,K)))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
1521  CONTINUE
!---EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
      DO 1553 K=1,L
      DO 1553 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,13)* &
                   (CTMP(I,K+1)-CTMP(I,K))
1553  CONTINUE
!---GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
      DO 1561 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,13)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,13)-SORC(I,L,13)))
1561  CONTINUE
!
!-----CALCULATION FOR BAND 14 (1070-1200 CM-1)
!
!
!---OBTAIN TEMPERATURE CORRECTION (CAPPHI,CAPPSI),THEN MULTIPLY
!   BY OPTICAL PATH (VAR1,VAR2) TO COMPUTE TEMPERATURE-CORRECTED
!   OPTICAL PATH AND MEAN PRESSURE FOR A LAYER (PHITMP,PSITMP)
      DO 1601 K=1,L
      DO 1601 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(14)*X(I,K)+BPCM(14)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(14)*X(I,K)+BTPCM(14)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
1601  CONTINUE
!---OBTAIN OPTICAL PATH,MEAN PRESSURE FROM THE TOP TO THE PRESSURE
!   P(K) (TOPM,TOPPHI)
      DO 1615 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
1615  CONTINUE
      DO 1619 K=2,L
      DO 1617 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
1617  CONTINUE
1619  CONTINUE
!---TT IS THE CLOUD-FREE CTS TRANSMISSION FUNCTION
      DO 1621 K=1,L
      DO 1621 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(14)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(14)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(14)*TOTVO2(I,K+1)*SKO2D))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
1621  CONTINUE
!---EXCTS IS THE CTS COOLING RATE ACCUMULATED OVER FREQUENCY BANDS
      DO 1653 K=1,L
      DO 1653 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,14)* &
                   (CTMP(I,K+1)-CTMP(I,K))
1653  CONTINUE
!---GXCTS IS THE EXACT CTS TOP FLUX ACCUMULATED OVER FREQUENCY BANDS
      DO 1661 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,14)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,14)-SORC(I,L,14)))
1661  CONTINUE
!
!
!   OBTAIN CTS FLUX AT THE TOP BY INTEGRATION OF HEATING RATES AND
!   USING CTS FLUX AT THE BOTTOM (CURRENT VALUE OF GXCTS). NOTE
!   THAT THE PRESSURE QUANTITIES AND CONVERSION FACTORS HAVE NOT
!   BEEN INCLUDED EITHER IN EXCTS OR IN GXCTS. THESE CANCEL OUT, THUS
!   REDUCING COMPUTATIONS!
      DO 1731 K=1,L
      DO 1731 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)-EXCTS(I,K)
1731  CONTINUE
!
!   NOW SCALE THE COOLING RATE (EXCTS) BY INCLUDING THE PRESSURE
!   FACTOR (DELP) AND THE CONVERSION FACTOR (RADCON)
      DO 1741 K=1,L
      DO 1741 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)*RADCON*DELP(I,K)
1741  CONTINUE
!---THIS IS THE END OF THE EXACT CTS COMPUTATIONS; AT THIS POINT
!   EXCTS HAS ITS APPROPRIATE VALUE.
!
!*** COMPUTE APPROXIMATE CTS HEATING RATES FOR 15UM AND 9.6 UM BANDS
!     (CTSO3)
      DO 1711 K=1,L
      DO 1711 I=MYIS,MYIE
      CTMP2(I,K+1)=CO2SP(I,K+1)*CLDFAC(I,K+1,1)
      CTMP3(I,K+1)=TO3SP(I,K)*CLDFAC(I,K+1,1)
1711  CONTINUE
      DO 1701 K=1,L
      DO 1701 I=MYIS,MYIE
      CTSO3(I,K)=RADCON*DELP(I,K)* &
           (CSOUR(I,K)*(CTMP2(I,K+1)-CTMP2(I,K)) + &
            SORC(I,K,13)*(CTMP3(I,K+1)-CTMP3(I,K)))
1701  CONTINUE

 END SUBROUTINE SPA88
!----------------------------------------------------------------------

 SUBROUTINE E290(EMISSB,EMISS,AVEPHI,KLEN,FXOE2,DTE2, &
!                      T1,T2,T4,                                     &
                       H16E1,HP1,H28E1,HAF,TEN,                      &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       its,ite, jts,jte, kts,kte                     )
!---------------------------------------------------------------------
 IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte
      INTEGER, INTENT(IN)        :: KLEN
      REAL, INTENT(IN) :: H16E1,HP1,H28E1,HAF ,TEN
      REAL, INTENT(OUT),DIMENSION(its:ite,kts:kte+1) :: EMISSB
      REAL, INTENT(IN ),DIMENSION(its:ite,kts:kte+1) :: AVEPHI,FXOE2,DTE2

!     REAL, INTENT(IN ), DIMENSION(5040) :: T1,T2,T4

      REAL, INTENT(INOUT), DIMENSION(its:ite,kts:kte+1) :: EMISS

      REAL, DIMENSION(its:ite,kts:kte+1) :: TMP3,DT,FYO,DU
      INTEGER, DIMENSION(its:ite,kts:kte+1) :: IVAL

!     REAL,    DIMENSION(28,180) :: TABLE1,TABLE2,TABLE3
!     EQUIVALENCE (T1(1),TABLE1(1,1)),(T2(1),TABLE2(1,1)), &
!                 (T4(1),TABLE3(1,1))
!     EQUIVALENCE (TMP3,DT)

      INTEGER :: K, I,KP,LLM2,J1,J3,KMAX,KMIN,KCLDS,ICNT,LLM1
      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE,LLP1,LL,KK

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3; LLP1 = 2*L + 1
      LM1=L-1;  LM2=L-2;  LM3=L-3; LL = 2*L
      LLM2 = LL-2; LLM1=LL-1
      MYIS=its; MYIE=ite


!---FIRST WE OBTAIN THE EMISSIVITIES AS A FUNCTION OF TEMPERATURE
!   (INDEX FXO) AND WATER AMOUNT (INDEX FYO). THIS PART OF THE CODE
!   THUS GENERATES THE E2 FUNCTION.
!
!---CALCULATIONS FOR VARYING KP (FROM KP=K+1 TO LP1, INCLUDING SPECIAL
!   CASE: RESULTS ARE IN EMISS



      DO 132 K=1,LP2-KLEN
      DO 132 I=MYIS,MYIE
      TMP3(I,K)=LOG10(AVEPHI(I,KLEN+K-1))+H16E1
      FYO(I,K)=AINT(TMP3(I,K)*TEN)
      DU(I,K)=TMP3(I,K)-HP1*FYO(I,K)
      FYO(I,K)=H28E1*FYO(I,K)
      IVAL(I,K)=FYO(I,K)+FXOE2(I,KLEN+K-1)
      EMISS(I,KLEN+K-1)=T1(IVAL(I,K))+DU(I,K)*T2(IVAL(I,K)) & 
                                 +DTE2(I,KLEN+K-1)*T4(IVAL(I,K))
132   CONTINUE
!---THE SPECIAL CASE EMISS(I,L) (LAYER KP) IS OBTAINED NOW
!   BY AVERAGING THE VALUES FOR L AND LP1:
      DO 1344 I=MYIS,MYIE
      EMISS(I,L)=HAF*(EMISS(I,L)+EMISS(I,LP1))
1344  CONTINUE
!---NOTE THAT EMISS(I,LP1) IS NOT USEFUL AFTER THIS POINT.
!
!---CALCULATIONS FOR KP=KLEN AND VARYING K; RESULTS ARE IN EMISSB.
!  IN THIS CASE, THE TEMPERATURE INDEX IS UNCHANGED, ALWAYS BEING
!  FXO(I,KLEN-1); THE WATER INDEX CHANGES, BUT IS SYMMETRICAL WITH
!  THAT FOR THE VARYING KP CASE.NOTE THAT THE SPECIAL CASE IS NOT
!  INVOLVED HERE.
!     (FIXED LEVEL) K VARIES FROM (KLEN+1) TO LP1; RESULTS ARE IN
!   EMISSB(I,(KLEN) TO L)
      DO 142 K=1,LP1-KLEN
      DO 142 I=MYIS,MYIE
      DT(I,K)=DTE2(I,KLEN-1)
      IVAL(I,K)=FYO(I,K)+FXOE2(I,KLEN-1)
142   CONTINUE
!
      DO 234 K=1,LP1-KLEN
      DO 234 I=MYIS,MYIE
      EMISSB(I,KLEN+K-1)=T1(IVAL(I,K))+DU(I,K)*T2(IVAL(I,K)) &
                                      +DT(I,K)*T4(IVAL(I,K))
234   CONTINUE

 END SUBROUTINE E290

!---------------------------------------------------------------------

  SUBROUTINE E2SPEC(EMISS,AVEPHI,FXOSP,DTSP,                         &
!                      T1,T2,T4,                                     &
                       H16E1,TEN,H28E1,HP1,                          &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       its,ite, jts,jte, kts,kte                     )
!---------------------------------------------------------------------
 IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte
      REAL,INTENT(IN ) :: H16E1,TEN,H28E1,HP1  
      REAL,INTENT(INOUT),DIMENSION(its:ite,kts:kte+1) :: EMISS
      REAL,INTENT(IN ),DIMENSION(its:ite,kts:kte+1) :: AVEPHI
      REAL,INTENT(IN ),DIMENSION(its:ite,2) :: FXOSP,DTSP

!     REAL, INTENT(IN ),DIMENSION(5040) :: T1,T2,T4

!     REAL, DIMENSION(28,180) :: TABLE1,TABLE2,TABLE3
!     EQUIVALENCE (T1(1),TABLE1(1,1)),(T2(1),TABLE2(1,1)), &
!                 (T4(1),TABLE3(1,1))

      INTEGER :: K,I,MYIS,MYIE

      REAL,    DIMENSION(its:ite,kts:kte+1) :: TMP3,FYO,DU
      INTEGER, DIMENSION(its:ite,kts:kte+1) :: IVAL

      MYIS=its
      MYIE=ite

      DO 132 K=1,2
      DO 132 I=MYIS,MYIE
      TMP3(I,K)=LOG10(AVEPHI(I,K))+H16E1
      FYO(I,K)=AINT(TMP3(I,K)*TEN)
      DU(I,K)=TMP3(I,K)-HP1*FYO(I,K)
      IVAL(I,K)=H28E1*FYO(I,K)+FXOSP(I,K)
      EMISS(I,K)=T1(IVAL(I,K))+DU(I,K)*T2(IVAL(I,K))+ &
                               DTSP(I,K)*T4(IVAL(I,K))
132   CONTINUE

  END SUBROUTINE E2SPEC

!---------------------------------------------------------------------

! SUBROUTINE E3V88(EMV,TV,AV,EM3V,            &
  SUBROUTINE E3V88(EMV,TV,AV, &
                       TEN,HP1,H28E1,H16E1,  &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       its,ite, jts,jte, kts,kte                     )
!---------------------------------------------------------------------
 IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte
      REAL,    INTENT(IN)  :: TEN,HP1,H28E1,H16E1 
!-----------------------------------------------------------------------
      REAL, INTENT(OUT), DIMENSION(its:ite,kts:kte*2+1) :: EMV
      REAL, INTENT(IN),  DIMENSION(its:ite,kts:kte*2+1) :: TV,AV
!     REAL, INTENT(IN),  DIMENSION(5040) :: EM3V

      REAL,DIMENSION(its:ite,kts:kte*2+1) ::FXO,TMP3,DT,WW1,WW2,DU,&
                                            FYO
!     REAL, DIMENSION(5040) :: EM3V

!     EQUIVALENCE (EM3V(1),EM3(1,1))

      INTEGER,DIMENSION(its:ite,kts:kte*2+1) ::IT

      INTEGER :: LLP1,I,K,MYIS,MYIE ,L
      L = kte
      LLP1 = 2*L + 1
      MYIS=its; MYIE=ite

!---THE FOLLOWING LOOP REPLACES A DOUBLE LOOP OVER I (1-IMAX) AND
!   K (1-LLP1)

      DO 203 K=1,LLP1
      DO 203 I=MYIS,MYIE
        FXO(I,K)=AINT(TV(I,K)*HP1)
        TMP3(I,K)=LOG10(AV(I,K))+H16E1
        DT(I,K)=TV(I,K)-TEN*FXO(I,K)
        FYO(I,K)=AINT(TMP3(I,K)*TEN)
        DU(I,K)=TMP3(I,K)-HP1*FYO(I,K)
!---OBTAIN INDEX FOR TABLE LOOKUP; THIS VALUE WILL HAVE TO BE
!   DECREMENTED BY 9 TO ACCOUNT FOR TABLE TEMPS STARTING AT 100K.
        IT(I,K)=FXO(I,K)+FYO(I,K)*H28E1
        WW1(I,K)=TEN-DT(I,K)
        WW2(I,K)=HP1-DU(I,K)
        EMV(I,K)=WW1(I,K)*WW2(I,K)*EM3V(IT(I,K)-9)+ &
                 WW2(I,K)*DT(I,K)*EM3V(IT(I,K)-8)+ & 
                 WW1(I,K)*DU(I,K)*EM3V(IT(I,K)+19)+ & 
                 DT(I,K)*DU(I,K)*EM3V(IT(I,K)+20)
203   CONTINUE

  END SUBROUTINE E3V88
!-----------------------------------------------------------------------

  SUBROUTINE SWR93(FSWC,HSWC,UFSWC,DFSWC,FSWL,HSWL,UFSWL,             &
                       DFSWL,                                         &
                       PRESS,COSZRO,TAUDAR,RH2O,RRCO2,SSOLAR,QO3,     &
                       NCLDS,KTOPSW,KBTMSW,CAMT,CRR,CTT,              &
                       ALVB,ALNB,ALVD,ALND,GDFVB,GDFNB,GDFVD,GDFND,   &
!                      UCO2,UO3,TUCO2,TUO3,TDO3,TDCO2,                &
                       ABCFF,PWTS,                                    &
                       H35E1,H1224E3,ONE,ZERO,HAF,H69766E5,HP219,     &
                       HP816,RRAYAV,GINV,CFCO2,CFO3,                  &
                       TWO,H235M3,HP26,H129M2,H75826M4,H1036E2,       &
                       H1P082,HMP805,H1386E2,H658M2,H2118M2,H42M2,    &
                       H323M4,HM1EZ,DIFFCTR,O3DIFCTR,FIFTY,RADCON,    &
                       ids,ide, jds,jde, kds,kde,                     &
                       ims,ime, jms,jme, kms,kme,                     &
                       its,ite, jts,jte, kts,kte                      )
!----------------------------------------------------------------------
 IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte
      REAL,INTENT(IN) :: RRCO2,SSOLAR
      REAL,INTENT(IN) :: H35E1,H1224E3,ONE,ZERO,HAF,H69766E5,HP219,HP816,RRAYAV,&
                         GINV,CFCO2,CFO3
      REAL,INTENT(IN) :: TWO,H235M3,HP26,H129M2,H75826M4,H1036E2  
      REAL,INTENT(IN) :: H1P082,HMP805,H1386E2,H658M2,H2118M2,H42M2,H323M4,HM1EZ
      REAL,INTENT(IN) :: DIFFCTR,O3DIFCTR,FIFTY,RADCON
!----------------------------------------------------------------------
      INTEGER, PARAMETER :: NB=12
      REAL,    INTENT(IN ),DIMENSION(its:ite,kts:kte+1) :: PRESS,CAMT
      REAL,    INTENT(IN ),DIMENSION(its:ite,kts:kte) :: RH2O,QO3
      REAL,    INTENT(IN ),DIMENSION(its:ite) :: COSZRO,TAUDAR,ALVB,ALVD,ALNB,ALND
      INTEGER, INTENT(IN ),DIMENSION(its:ite) :: NCLDS
      INTEGER, INTENT(IN ),DIMENSION(its:ite,kts:kte+1) ::KTOPSW,KBTMSW
      REAL, INTENT(IN ),DIMENSION(its:ite,NB,kts:kte+1) ::CRR,CTT
           
      REAL, INTENT(OUT),DIMENSION(its:ite,kts:kte+1) ::     &
                                       FSWC,HSWC,UFSWC,DFSWC,FSWL,HSWL,UFSWL,DFSWL
      REAL, INTENT(OUT),DIMENSION(its:ite) :: GDFVB,GDFVD,GDFNB,GDFND
      REAL, INTENT(IN), DIMENSION(NB) :: ABCFF,PWTS

!     REAL, INTENT(IN), DIMENSION(its:ite,kts:kte*2+2) :: UCO2,UO3
!     REAL, INTENT(IN), DIMENSION(its:ite,kts:kte+1)   :: TUCO2,TUO3,TDO3,TDCO2

      REAL, DIMENSION(its:ite,kts:kte*2+2) :: UCO2,UO3
      REAL, DIMENSION(its:ite,kts:kte+1)   :: TUCO2,TUO3,TDO3,TDCO2

      REAL, DIMENSION(its:ite,kts:kte*2+2) :: TCO2,TO3
      REAL, DIMENSION(its:ite,kts:kte+1) :: PP,DP,PR2,DU,DUCO2,DUO3,UD,TTD
      REAL, DIMENSION(its:ite,kts:kte+1) :: UDCO2,UDO3,UR,URCO2,URO3,TTU
      REAL, DIMENSION(its:ite,kts:kte+1) :: DFN,UFN
      REAL, DIMENSION(its:ite,kts:kte+1) :: XAMT,FF,FFCO2,FFO3,CR,CT
      REAL, DIMENSION(its:ite,kts:kte+1) :: PPTOP,DPCLD,TTDB1,TTUB1
      REAL, DIMENSION(its:ite,kts:kte+1) :: TDCL1,TUCL1,TDCL2,DFNTRN,  &
                                            UFNTRN,TCLU,TCLD,ALFA,ALFAU, &
                                            UFNCLU,DFNCLU

      REAL, DIMENSION(its:ite,NB) :: DFNTOP
      REAL, DIMENSION(its:ite) :: SECZ,TMP1,RRAY,REFL,REFL2,CCMAX

!                    EQUIVALENCE &
!       (UDO3,UO3(its,1),DFNCLU), (URO3,UO3(its,kte+2), UFNCLU) &
!     , (UDCO2,UCO2(its,1),TCLD), (URCO2,UCO2(its,kte+2), TCLU) &
!     , (TDO3 ,TO3(its,1),DFNTRN),(TUO3,TO3(its,kte+2), UFNTRN) &
!     , (TDCO2,TCO2(its,1)      ),(TUCO2,TCO2(its,kte+2)        ) &
!     , (FF   , ALFA ),   (FFCO2 , ALFAU ),   (FFO3  , TTDB1 ) &
!     , (DU   , TTUB1),   (DUCO2 , TUCL1 ),   (DUO3  , TDCL1 ) &
!     , (PR2  , TDCL2)

!                    EQUIVALENCE &
!       (UDO3,DFNCLU), (URO3,UFNCLU) &
!     , (UDCO2,TCLD ), (URCO2,TCLU) &
!     , (TDO3 ,DFNTRN),(TUO3,UFNTRN) &
!!    , (TDCO2,TCO2(its,1)      ),(TUCO2,TCO2(its,kte+2)        ) &
!     , (FF   , ALFA ),   (FFCO2 , ALFAU ),   (FFO3  , TTDB1 ) &
!     , (DU   , TTUB1),   (DUCO2 , TUCL1 ),   (DUO3  , TDCL1 ) &
!     , (PR2  , TDCL2)

      INTEGER :: K,I,KP,N,IP,MYIS1,KCLDS,NNCLDS,JTOP,KK,J2,J3,J1
      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE,LLP1,LL
      REAL    :: DENOM,HTEMP,TEMPF,TEMPG

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3; LLP1 = 2*L + 1
      LM1=L-1;  LM2=L-2;  LM3=L-3; LL = 2*L
      MYIS=its; MYIE=ite
      MYIS1=MYIS+1    ! ??

      DO 100 I=MYIS,MYIE
        SECZ(I) = H35E1/SQRT(H1224E3*COSZRO(I)*COSZRO(I)+ONE)
        PP(I,1)   = ZERO
        PP(I,LP1) = PRESS(I,LP1)
        TMP1(I)  = ONE/PRESS(I,LP1)
100   CONTINUE
      DO 110 K=1,LM1
      DO 110 I=MYIS,MYIE
        PP(I,K+1) = HAF*(PRESS(I,K+1)+PRESS(I,K))
110   CONTINUE
      DO 120 K=1,L
      DO 120 I=MYIS,MYIE
        DP (I,K) = PP(I,K+1)-PP(I,K)
        PR2(I,K) = HAF*(PP(I,K)+PP(I,K+1))
120   CONTINUE
      DO 130 K=1,L
      DO 130 I=MYIS,MYIE
        PR2(I,K) = PR2(I,K)*TMP1(I)
130   CONTINUE
!     CALCULATE ENTERING FLUX AT THE TOP FOR EACH BAND(IN CGS UNITS)
      DO 140 N=1,NB
      DO 140 IP=MYIS,MYIE
        DFNTOP(IP,N) = SSOLAR*H69766E5*COSZRO(IP)*TAUDAR(IP)*PWTS(N)
140   CONTINUE
!     EXECUTE THE LACIS-HANSEN REFLECTIVITY PARAMETERIZATION
!     FOR THE VISIBLE BAND
      DO 150 I=MYIS,MYIE
        RRAY(I) = HP219/(ONE+HP816*COSZRO(I))
        REFL(I) = RRAY(I) + (ONE-RRAY(I))*(ONE-RRAYAV)*ALVB(I)/ &
                  (ONE-ALVD(I)*RRAYAV)
150   CONTINUE
      DO 155 I=MYIS,MYIE
        RRAY(I) = 0.104/(ONE+4.8*COSZRO(I))
        REFL2(I)= RRAY(I) + (ONE-RRAY(I))*(ONE-0.093)*ALVB(I)/ &
                  (ONE-ALVD(I)*0.093)
155   CONTINUE
!     CALCULATE PRESSURE-WEIGHTED OPTICAL PATHS FOR EACH LAYER
!     IN UNITS OF CM-ATM. PRESSURE WEIGHTING IS USING PR2.
!     DU= VALUE FOR H2O;DUCO2 FOR CO2;DUO3 FOR O3.
      DO 160 K=1,L
      DO 160 I=MYIS,MYIE
        DU   (I,K) = GINV*RH2O(I,K)*DP(I,K)*PR2(I,K)
        DUCO2(I,K) = (RRCO2*GINV*CFCO2)*DP(I,K)*PR2(I,K)
        DUO3 (I,K) = (GINV*CFO3)*QO3(I,K)*DP(I,K)
160   CONTINUE
!
!                 CALCULATE CLEAR SKY SW FLUX
!
!     OBTAIN THE OPTICAL PATH FROM THE TOP OF THE ATMOSPHERE TO THE
!     FLUX PRESSURE. ANGULAR FACTORS ARE NOW INCLUDED. UD=DOWNWARD
!     PATH FOR H2O,WIGTH UR THE UPWARD PATH FOR H2O. CORRESPONDING
!     QUANTITIES FOR CO2,O3 ARE UDCO2/URCO2 AND UDO3/URO3.
      DO 200 IP=MYIS,MYIE
        UD   (IP,1) = ZERO
        UDCO2(IP,1) = ZERO
        UDO3 (IP,1) = ZERO
! SH
        UO3  (IP,1) = UDO3 (IP,1)
        UCO2 (IP,1) = UDCO2(IP,1)

200   CONTINUE
      DO 210 K=2,LP1
      DO 210 I=MYIS,MYIE
        UD   (I,K) = UD   (I,K-1)+DU   (I,K-1)*SECZ(I)
        UDCO2(I,K) = UDCO2(I,K-1)+DUCO2(I,K-1)*SECZ(I)
        UDO3 (I,K) = UDO3 (I,K-1)+DUO3 (I,K-1)*SECZ(I)
! SH
        UO3  (I,K) = UDO3 (I,K)
        UCO2 (I,K) = UDCO2(I,K)

210   CONTINUE
      DO 220 IP=MYIS,MYIE
        UR   (IP,LP1) = UD   (IP,LP1)
        URCO2(IP,LP1) = UDCO2(IP,LP1)
        URO3 (IP,LP1) = UDO3 (IP,LP1)
! SH
        UO3  (IP,LP1+LP1) = URO3 (IP,LP1) 
        UCO2 (IP,LP1+LP1) = URCO2(IP,LP1)

220   CONTINUE
      DO 230 K=L,1,-1
      DO 230 IP=MYIS,MYIE
        UR   (IP,K) = UR   (IP,K+1)+DU   (IP,K)*DIFFCTR
        URCO2(IP,K) = URCO2(IP,K+1)+DUCO2(IP,K)*DIFFCTR
        URO3 (IP,K) = URO3 (IP,K+1)+DUO3 (IP,K)*O3DIFCTR
! SH
        UO3 (IP,LP1+K) = URO3 (IP,K)
        UCO2(IP,LP1+K) = URCO2(IP,K)

230   CONTINUE
!     CALCULATE CO2 ABSORPTIONS . THEY WILL BE USED IN NEAR INFRARED
!     BANDS.SINCE THE ABSORPTION AMOUNT IS GIVEN (IN THE FORMULA USED
!     BELOW, DERIVED FROM SASAMORI) IN TERMS OF THE TOTAL SOLAR FLUX,
!     AND THE ABSORPTION IS ONLY INCLUDED IN THE NEAR IR (50 PERCENT
!     OF THE SOLAR SPECTRUM), THE ABSORPTIONS ARE MULTIPLIED BY 2.
!       SINCE CODE ACTUALLY REQUIRES TRANSMISSIONS, THESE ARE THE
!     VALUES ACTUALLY STORED IN TCO2.
      DO 240 K=1,LL
      DO 240 I=MYIS,MYIE
       TCO2(I,K+1)=ONE-TWO*(H235M3*EXP(HP26*LOG(UCO2(I,K+1)+H129M2)) &
                             -H75826M4)
240   CONTINUE

! SH
      DO 241 K=1,L
      DO 241 I=MYIS,MYIE
        TDCO2(I,K+1)=TCO2(I,K+1)
241   CONTINUE
      DO 242 K=1,L
      DO 242 I=MYIS,MYIE
        TUCO2(I,K)=TCO2(I,LP1+K)
242   CONTINUE

!     NOW CALCULATE OZONE ABSORPTIONS. THESE WILL BE USED IN
!     THE VISIBLE BAND.JUST AS IN THE CO2 CASE, SINCE THIS BAND IS
!     50 PERCENT OF THE SOLAR SPECTRUM,THE ABSORPTIONS ARE MULTIPLIED
!     BY 2. THE TRANSMISSIONS ARE STORED IN TO3.
      HTEMP = H1036E2*H1036E2*H1036E2
      DO 250 K=1,LL
      DO 250 I=MYIS,MYIE
        TO3(I,K+1)=ONE-TWO*UO3(I,K+1)* &
                  (H1P082*EXP(HMP805*LOG(ONE+H1386E2*UO3(I,K+1)))+ &
                  H658M2/(ONE+HTEMP*UO3(I,K+1)*UO3(I,K+1)*UO3(I,K+1))+ &
                  H2118M2/(ONE+UO3(I,K+1)*(H42M2+H323M4*UO3(I,K+1))))
250   CONTINUE

! SH
      DO 251 K=1,L
      DO 251 I=MYIS,MYIE
        TDO3(I,K+1)=TO3(I,K+1)
251   CONTINUE
      DO 252 K=1,L
      DO 252 I=MYIS,MYIE
        TUO3(I,K)=TO3(I,LP1+K)
252   CONTINUE


!   START FREQUENCY LOOP (ON N) HERE
!
!--- BAND 1 (VISIBLE) INCLUDES O3 AND H2O ABSORPTION
      DO 260 K=1,L
      DO 260 I=MYIS,MYIE
        TTD(I,K+1) = EXP(HM1EZ*MIN(FIFTY,ABCFF(1)*UD(I,K+1)))
        TTU(I,K) = EXP(HM1EZ*MIN(FIFTY,ABCFF(1)*UR(I,K)))
        DFN(I,K+1) = TTD(I,K+1)*TDO3(I,K+1)
        UFN(I,K) = TTU(I,K)*TUO3(I,K)
260   CONTINUE
      DO 270 I=MYIS,MYIE
        DFN(I,1)   = ONE
        UFN(I,LP1) = DFN(I,LP1)
270   CONTINUE
!     SCALE VISIBLE BAND FLUXES BY SOLAR FLUX AT THE TOP OF THE
!     ATMOSPHERE (DFNTOP(I,1))
!     DFSW/UFSW WILL BE THE FLUXES, SUMMED OVER ALL BANDS
      DO 280  K=1,LP1
      DO 280  I=MYIS,MYIE
        DFSWL(I,K) =         DFN(I,K)*DFNTOP(I,1)
        UFSWL(I,K) = REFL(I)*UFN(I,K)*DFNTOP(I,1)
280   CONTINUE
      DO 285 I=MYIS,MYIE
        GDFVB(I) = DFSWL(I,LP1)*EXP(-0.15746*SECZ(I))
        GDFVD(I) = ((ONE-REFL2(I))*DFSWL(I,LP1) - &
                    (ONE-ALVB(I)) *GDFVB(I)) / (ONE-ALVD(I))
        GDFNB(I) = ZERO
        GDFND(I) = ZERO
285   CONTINUE
!---NOW OBTAIN FLUXES FOR THE NEAR IR BANDS. THE METHODS ARE THE SAME
!   AS FOR THE VISIBLE BAND, EXCEPT THAT THE REFLECTION AND
!   TRANSMISSION COEFFICIENTS (OBTAINED BELOW) ARE DIFFERENT, AS
!   RAYLEIGH SCATTERING NEED NOT BE CONSIDERED.
      DO 350 N=2,NB
        IF (N.EQ.2) THEN
!   THE WATER VAPOR TRANSMISSION FUNCTION FOR BAND 2 IS EQUAL TO
!   THAT OF BAND 1 (SAVED AS TTD,TTU)
!--- BAND 2-9 (NEAR-IR) INCLUDES O3, CO2 AND H2O ABSORPTION
          DO 290 K=1,L
          DO 290 I=MYIS,MYIE
            DFN(I,K+1) = TTD(I,K+1)*TDCO2(I,K+1)
            UFN(I,K) = TTU(I,K)*TUCO2(I,K)
290       CONTINUE
        ELSE
!   CALCULATE WATER VAPOR TRANSMISSION FUNCTIONS FOR NEAR INFRARED
!   BANDS. INCLUDE CO2 TRANSMISSION (TDCO2/TUCO2), WHICH
!   IS THE SAME FOR ALL INFRARED BANDS.
          DO 300 K=1,L
          DO 300 I=MYIS,MYIE
            DFN(I,K+1)=EXP(HM1EZ*MIN(FIFTY,ABCFF(N)*UD(I,K+1))) &
                       *TDCO2(I,K+1)
            UFN(I,K)=EXP(HM1EZ*MIN(FIFTY,ABCFF(N)*UR(I,K))) &
                     *TUCO2(I,K)
300       CONTINUE
        ENDIF
!---AT THIS POINT,INCLUDE DFN(1),UFN(LP1), NOTING THAT DFN(1)=1 FOR
!   ALL BANDS, AND THAT UFN(LP1)=DFN(LP1) FOR ALL BANDS.
        DO 310 I=MYIS,MYIE
          DFN(I,1)   = ONE
          UFN(I,LP1) = DFN(I,LP1)
310     CONTINUE
!     SCALE THE PREVIOUSLY COMPUTED FLUXES BY THE FLUX AT THE TOP
!     AND SUM OVER BANDS
        DO 320 K=1,LP1
        DO 320 I=MYIS,MYIE
          DFSWL(I,K) = DFSWL(I,K) +         DFN(I,K)*DFNTOP(I,N)
          UFSWL(I,K) = UFSWL(I,K) + ALNB(I)*UFN(I,K)*DFNTOP(I,N)
320     CONTINUE
        DO 330 I=MYIS,MYIE
          GDFNB(I) = GDFNB(I) + DFN(I,LP1)*DFNTOP(I,N)
330     CONTINUE
350   CONTINUE
      DO 360 K=1,LP1
      DO 360 I=MYIS,MYIE
        FSWL(I,K) = UFSWL(I,K)-DFSWL(I,K)
360   CONTINUE
      DO 370 K=1,L
      DO 370 I=MYIS,MYIE
        HSWL(I,K)=RADCON*(FSWL(I,K+1)-FSWL(I,K))/DP(I,K)
370   CONTINUE
!
!---END OF FREQUENCY LOOP (OVER N)
!
!                 CALCULATE CLOUDY SKY SW FLUX
!
      KCLDS=NCLDS(MYIS)
      DO 400 I=MYIS1,MYIE
        KCLDS=MAX(NCLDS(I),KCLDS)
400   CONTINUE
        DO 410 K=1,LP1
        DO 410 I=MYIS,MYIE
          DFSWC(I,K) = DFSWL(I,K)
          UFSWC(I,K) = UFSWL(I,K)
          FSWC (I,K) = FSWL (I,K)
410     CONTINUE
        DO 420 K=1,L
        DO 420 I=MYIS,MYIE
          HSWC(I,K) = HSWL(I,K)
420     CONTINUE
!*******************************************************************
      IF (KCLDS .EQ. 0)  RETURN
!*******************************************************************
      DO 430 K=1,LP1
      DO 430 I=MYIS,MYIE
        XAMT(I,K) = CAMT(I,K)
430   CONTINUE
      DO 470 I=MYIS,MYIE
        NNCLDS   = NCLDS(I)
        CCMAX(I) = ZERO
        IF (NNCLDS .LE. 0) GO TO 470
        CCMAX(I) = ONE
        DO 450 K=1,NNCLDS
          CCMAX(I) = CCMAX(I) * (ONE - CAMT(I,K+1))
450     CONTINUE
        CCMAX(I) = ONE - CCMAX(I)
        IF (CCMAX(I) .GT. ZERO) THEN
          DO 460 K=1,NNCLDS
            XAMT(I,K+1) = CAMT(I,K+1)/CCMAX(I)
460       CONTINUE
        END IF
470   CONTINUE
      DO 480 K=1,LP1
      DO 480 I=MYIS,MYIE
        FF   (I,K) = DIFFCTR
        FFCO2(I,K) = DIFFCTR
        FFO3 (I,K) = O3DIFCTR
480   CONTINUE
      DO 490 IP=MYIS,MYIE
        JTOP = KTOPSW(IP,NCLDS(IP)+1)
      DO 490 K=1,JTOP
        FF   (IP,K) = SECZ(IP)
        FFCO2(IP,K) = SECZ(IP)
        FFO3 (IP,K) = SECZ(IP)
490   CONTINUE
      DO 500 I=MYIS,MYIE
        RRAY(I) = HP219/(ONE+HP816*COSZRO(I))
        REFL(I) = RRAY(I) + (ONE-RRAY(I))*(ONE-RRAYAV)*ALVD(I)/ &
                  (ONE-ALVD(I)*RRAYAV)
500   CONTINUE
      DO 510 IP=MYIS,MYIE
        UD   (IP,1) = ZERO
        UDCO2(IP,1) = ZERO
        UDO3 (IP,1) = ZERO
! SH
        UO3  (IP,1) = UDO3 (IP,1)
        UCO2 (IP,1) = UDCO2(IP,1)

510   CONTINUE
      DO 520 K=2,LP1
      DO 520 I=MYIS,MYIE
        UD   (I,K) = UD   (I,K-1)+DU   (I,K-1)*FF   (I,K)
        UDCO2(I,K) = UDCO2(I,K-1)+DUCO2(I,K-1)*FFCO2(I,K)
        UDO3 (I,K) = UDO3 (I,K-1)+DUO3 (I,K-1)*FFO3 (I,K)
! SH
        UO3 (I,K)  = UDO3 (I,K)
        UCO2(I,K)  = UDCO2(I,K)

520   CONTINUE
      DO 530 IP=MYIS,MYIE
        UR   (IP,LP1) = UD   (IP,LP1)
        URCO2(IP,LP1) = UDCO2(IP,LP1)
        URO3 (IP,LP1) = UDO3 (IP,LP1)
! SH
        UO3  (IP,LP1+LP1) = URO3 (IP,LP1)
        UCO2 (IP,LP1+LP1) = URCO2(IP,LP1)

530   CONTINUE
      DO 540 K=L,1,-1
      DO 540 IP=MYIS,MYIE
        UR   (IP,K) = UR   (IP,K+1)+DU   (IP,K)*DIFFCTR
        URCO2(IP,K) = URCO2(IP,K+1)+DUCO2(IP,K)*DIFFCTR
        URO3 (IP,K) = URO3 (IP,K+1)+DUO3 (IP,K)*O3DIFCTR
! SH
        UO3 (IP,LP1+K) = URO3 (IP,K)
        UCO2(IP,LP1+K) = URCO2(IP,K)

540   CONTINUE
      DO 550 K=1,LL
      DO 550 I=MYIS,MYIE
        TCO2(I,K+1)=ONE-TWO*(H235M3*EXP(HP26*LOG(UCO2(I,K+1)+H129M2)) &
                              -H75826M4)
550   CONTINUE
! SH
      DO 551 K=1,L
      DO 551 I=MYIS,MYIE
        TDCO2(I,K+1)=TCO2(I,K+1)
551   CONTINUE
      DO 552 K=1,L
      DO 552 I=MYIS,MYIE
        TUCO2(I,K)=TCO2(I,LP1+K)
552   CONTINUE

      DO 560 K=1,LL
      DO 560 I=MYIS,MYIE
        TO3(I,K+1)=ONE-TWO*UO3(I,K+1)* &
                 (H1P082*EXP(HMP805*LOG(ONE+H1386E2*UO3(I,K+1)))+ &
                H658M2/(ONE+HTEMP*UO3(I,K+1)*UO3(I,K+1)*UO3(I,K+1))+ &
                H2118M2/(ONE+UO3(I,K+1)*(H42M2+H323M4*UO3(I,K+1))))
560   CONTINUE
! SH
      DO 561 K=1,L
      DO 561 I=MYIS,MYIE
        TDO3(I,K+1)=TO3(I,K+1)
561   CONTINUE
      DO 562 K=1,L
      DO 562 I=MYIS,MYIE
        TUO3(I,K)=TO3(I,LP1+K)
562   CONTINUE

!********************************************************************
!---THE FIRST CLOUD IS THE GROUND; ITS PROPERTIES ARE GIVEN
!   BY REFL (THE TRANSMISSION (0) IS IRRELEVANT FOR NOW!).
!********************************************************************
      DO 570 I=MYIS,MYIE
        CR(I,1) = REFL(I)
570   CONTINUE
!***OBTAIN CLOUD REFLECTION AND TRANSMISSION COEFFICIENTS FOR
!   REMAINING CLOUDS (IF ANY) IN THE VISIBLE BAND
!---THE MAXIMUM NO OF CLOUDS IN THE ROW (KCLDS) IS USED. THIS CREATES
!   EXTRA WORK (MAY BE REMOVED IN A SUBSEQUENT UPDATE).
      DO 581 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 581
      DO 580 KK=2,KCLDS+1
        CR(I,KK) = CRR(I,1,KK)*XAMT(I,KK)
        CT(I,KK) = ONE - (ONE-CTT(I,1,KK))*XAMT(I,KK)
580   CONTINUE
581   CONTINUE
!---OBTAIN THE PRESSURE AT THE TOP,BOTTOM AND THE THICKNESS OF
!   "THICK" CLOUDS (THOSE AT LEAST 2 LAYERS THICK). THIS IS USED
!   LATER IS OBTAINING FLUXES INSIDE THE THICK CLOUDS, FOR ALL
!   FREQUENCY BANDS.
      DO 591 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 591
      DO 590 KK=1,KCLDS
        IF ((KBTMSW(I,KK+1)-1).GT.KTOPSW(I,KK+1)) THEN
           PPTOP(I,KK)=PP(I,KTOPSW(I,KK+1))
           DPCLD(I,KK)=ONE/(PPTOP(I,KK)-PP(I,KBTMSW(I,KK+1)))
        ENDIF
590   CONTINUE
591   CONTINUE
      DO 600 K=1,L
      DO 600 I=MYIS,MYIE
        TTDB1(I,K+1) = EXP(HM1EZ*MIN(FIFTY,ABCFF(1)*UD(I,K+1)))
        TTUB1(I,K) = EXP(HM1EZ*MIN(FIFTY,ABCFF(1)*UR(I,K)))
        TTD  (I,K+1) = TTDB1(I,K+1)*TDO3(I,K+1)
        TTU  (I,K) = TTUB1(I,K)*TUO3(I,K)
600   CONTINUE
      DO 610 I=MYIS,MYIE
        TTD(I,1)   = ONE
        TTU(I,LP1) = TTD(I,LP1)
610   CONTINUE
!***FOR EXECUTION OF THE CLOUD LOOP, IT IS NECESSARY TO SEPARATE OUT
!   TRANSMISSION FCTNS AT THE TOP AND BOTTOM OF THE CLOUDS, FOR
!   EACH BAND N. THE REQUIRED QUANTITIES ARE:
!      TTD(I,KTOPSW(I,K),N)  K RUNS FROM 1 TO NCLDS(I)+1:
!      TTU(I,KTOPSW(I,K),N)  K RUNS FROM 1 TO NCLDS(I)+1:
!      TTD(I,KBTMSW(I,K),N)  K RUNS FROM 1 TO NCLDS(I)+1:
!      AND INVERSES OF THE FIRST TWO. THE ABOVE QUANTITIES ARE
!      STORED IN TDCL1,TUCL1,TDCL2, AND DFNTRN,UFNTRN, RESPECTIVELY,
!      AS THEY HAVE MULTIPLE USE IN THE PGM.
!---FOR FIRST CLOUD LAYER (GROUND) TDCL1,TUCL1 ARE KNOWN:
      DO 620 I=MYIS,MYIE
        TDCL1 (I,1) = TTD(I,LP1)
        TUCL1 (I,1) = TTU(I,LP1)
        TDCL2 (I,1) = TDCL1(I,1)
        DFNTRN(I,1) = ONE/TDCL1(I,1)
        UFNTRN(I,1) = DFNTRN(I,1)
620   CONTINUE
      DO 631 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 631
      DO 630 KK=2,KCLDS+1
        TDCL1(I,KK) = TTD(I,KTOPSW(I,KK))
        TUCL1(I,KK) = TTU(I,KTOPSW(I,KK))
        TDCL2(I,KK) = TTD(I,KBTMSW(I,KK))
630   CONTINUE
631   CONTINUE
!---COMPUTE INVERSES
      DO 641 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 641
! SH
      DO 640 KK=2,KCLDS+1
        DFNTRN(I,KK) = ONE/TDCL1(I,KK)
        UFNTRN(I,KK) = ONE/TUCL1(I,KK)
640   CONTINUE
641   CONTINUE
!---COMPUTE THE TRANSMISSIVITY FROM THE TOP OF CLOUD (K+1) TO THE
!   TOP OF CLOUD (K). THE CLOUD TRANSMISSION (CT) IS INCLUDED. THIS
!   QUANTITY IS CALLED TCLU (INDEX K). ALSO, OBTAIN THE TRANSMISSIVITY
!   FROM THE BOTTOM OF CLOUD (K+1) TO THE TOP OF CLOUD (K)(A PATH
!   ENTIRELY OUTSIDE CLOUDS). THIS QUANTITY IS CALLED TCLD (INDEX K).
      DO 651 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 651
      DO 650 KK=1,KCLDS
        TCLU(I,KK) = TDCL1(I,KK)*DFNTRN(I,KK+1)*CT(I,KK+1)
        TCLD(I,KK) = TDCL1(I,KK)/TDCL2(I,KK+1)
650   CONTINUE
651   CONTINUE
!***THE FOLLOWING IS THE RECURSION RELATION FOR ALFA: THE REFLECTION
!   COEFFICIENT FOR A SYSTEM INCLUDING THE CLOUD IN QUESTION AND THE
!   FLUX COMING OUT OF THE CLOUD SYSTEM INCLUDING ALL CLOUDS BELOW
!   THE CLOUD IN QUESTION.
!---ALFAU IS ALFA WITHOUT THE REFLECTION OF THE CLOUD IN QUESTION
      DO 660 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 660
        ALFA (I,1)=CR(I,1)
        ALFAU(I,1)=ZERO
660   CONTINUE
!---AGAIN,EXCESSIVE CALCULATIONS-MAY BE CHANGED LATER!
      DO 671 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 671
      DO 670 KK=2,KCLDS+1
        ALFAU(I,KK)= TCLU(I,KK-1)*TCLU(I,KK-1)*ALFA(I,KK-1)/ &
              (ONE - TCLD(I,KK-1)*TCLD(I,KK-1)*ALFA(I,KK-1)*CR(I,KK))
        ALFA (I,KK)= ALFAU(I,KK)+CR(I,KK)
670   CONTINUE
671   CONTINUE
!     CALCULATE UFN AT CLOUD TOPS AND DFN AT CLOUD BOTTOMS
!---NOTE THAT UFNCLU(I,KCLDS+1) GIVES THE UPWARD FLUX AT THE TOP
!   OF THE HIGHEST REAL CLOUD (IF NCLDS(I)=KCLDS). IT GIVES THE FLUX
!   AT THE TOP OF THE ATMOSPHERE IF NCLDS(I) < KCLDS. IN THE FIRST
!   CASE, TDCL1 EQUALS THE TRANSMISSION FCTN TO THE TOP OF THE
!   HIGHEST CLOUD, AS WE WANT. IN THE SECOND CASE, TDCL1=1, SO UFNCLU
!   EQUALS ALFA. THIS IS ALSO CORRECT.
      DO 680 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 680
        UFNCLU(I,KCLDS+1) = ALFA(I,KCLDS+1)*TDCL1(I,KCLDS+1)
        DFNCLU(I,KCLDS+1) = TDCL1(I,KCLDS+1)
680   CONTINUE
!---THIS CALCULATION IS THE REVERSE OF THE RECURSION RELATION USED
!  ABOVE
      DO 691 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 691
      DO 690 KK=KCLDS,1,-1
        UFNCLU(I,KK) = UFNCLU(I,KK+1)*ALFAU(I,KK+1)/(ALFA(I,KK+1)* &
                       TCLU(I,KK))
        DFNCLU(I,KK) = UFNCLU(I,KK)/ALFA(I,KK)
690   CONTINUE
691   CONTINUE
      DO 701 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 701
      DO 700 KK=1,KCLDS+1
        UFNTRN(I,KK) = UFNCLU(I,KK)*UFNTRN(I,KK)
        DFNTRN(I,KK) = DFNCLU(I,KK)*DFNTRN(I,KK)
700   CONTINUE
701   CONTINUE
!---CASE OF KK=1( FROM THE GROUND TO THE BOTTOM OF THE LOWEST CLOUD)
      DO 720 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 720
        J2=KBTMSW(I,2)
        DO 710 K=J2,LP1
          UFN(I,K) = UFNTRN(I,1)*TTU(I,K)
          DFN(I,K) = DFNTRN(I,1)*TTD(I,K)
710     CONTINUE
720   CONTINUE
!---REMAINING LEVELS (IF ANY!)
      DO 760 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 760
      DO 755 KK=2,KCLDS+1
        J1=KTOPSW(I,KK)
        J2=KBTMSW(I,KK+1)
        IF (J1.EQ.1) GO TO 755
        DO 730 K=J2,J1
          UFN(I,K) = UFNTRN(I,KK)*TTU(I,K)
          DFN(I,K) = DFNTRN(I,KK)*TTD(I,K)
730     CONTINUE
!---FOR THE THICK CLOUDS, THE FLUX DIVERGENCE THROUGH THE CLOUD
!   LAYER IS ASSUMED TO BE CONSTANT. THE FLUX DERIVATIVE IS GIVEN BY
!   TEMPF (FOR THE UPWARD FLUX) AND TEMPG (FOR THE DOWNWARD FLUX).
        J3=KBTMSW(I,KK)
        IF ((J3-J1).GT.1) THEN
          TEMPF = (UFNCLU(I,KK)-UFN(I,J3))*DPCLD(I,KK-1)
          TEMPG = (DFNCLU(I,KK)-DFN(I,J3))*DPCLD(I,KK-1)
          DO 740 K=J1+1,J3-1
            UFN(I,K) = UFNCLU(I,KK)+TEMPF*(PP(I,K)-PPTOP(I,KK-1))
            DFN(I,K) = DFNCLU(I,KK)+TEMPG*(PP(I,K)-PPTOP(I,KK-1))
740       CONTINUE
        ENDIF
755   CONTINUE
760   CONTINUE
      DO 770 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 770
      DO 771 K=1,LP1
        DFSWC(I,K) = DFN(I,K)*DFNTOP(I,1)
        UFSWC(I,K) = UFN(I,K)*DFNTOP(I,1)
771   CONTINUE
770   CONTINUE
      DO 780 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 780
        TMP1(I) = ONE - CCMAX(I)
        GDFVB(I) = TMP1(I)*GDFVB(I)
        GDFNB(I) = TMP1(I)*GDFNB(I)
        GDFVD(I) = TMP1(I)*GDFVD(I) + CCMAX(I)*DFSWC(I,LP1)
780   CONTINUE
!---NOW OBTAIN FLUXES FOR THE NEAR IR BANDS. THE METHODS ARE THE SAME
!   AS FOR THE VISIBLE BAND, EXCEPT THAT THE REFLECTION AND
!   TRANSMISSION COEFFICIENTS ARE DIFFERENT, AS
!   RAYLEIGH SCATTERING NEED NOT BE CONSIDERED.
!
      DO 1000 N=2,NB
!YH93
        DO 791 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 791
        DO 790 K=1,KCLDS+1
          CR(I,K) = CRR(I,N,K)*XAMT(I,K)
          CT(I,K) = ONE - (ONE-CTT(I,N,K))*XAMT(I,K)
790     CONTINUE
791     CONTINUE
!YH93
        IF (N.EQ.2) THEN
!   THE WATER VAPOR TRANSMISSION FUNCTION FOR BAND 2 IS EQUAL TO
!   THAT OF BAND 1 (SAVED AS TTDB1,TTUB1)
          DO 800 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 800
        DO 801 KK=2,LP1
            TTD(I,KK) = TTDB1(I,KK)*TDCO2(I,KK)
801     CONTINUE
        DO 802 KK=1,L
            TTU(I,KK) = TTUB1(I,KK)*TUCO2(I,KK)
802     CONTINUE
800       CONTINUE
        ELSE
          DO 810 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 810
        DO 811 KK=2,LP1
            TTD(I,KK) = EXP(HM1EZ*MIN(FIFTY,ABCFF(N)*UD(I,KK))) &
                     * TDCO2(I,KK)
811     CONTINUE
        DO 812 KK=1,L
            TTU(I,KK) = EXP(HM1EZ*MIN(FIFTY,ABCFF(N)*UR(I,KK))) &
                     * TUCO2(I,KK)
812     CONTINUE
810       CONTINUE
        ENDIF
!---AT THIS POINT,INCLUDE TTD(1),TTU(LP1), NOTING THAT TTD(1)=1 FOR
!   ALL BANDS, AND THAT TTU(LP1)=TTD(LP1) FOR ALL BANDS.
        DO 820 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 820
          TTU(I,LP1) = TTD(I,LP1)
          TTD(I,1)   = ONE
820     CONTINUE
!***FOR EXECUTION OF THE CLOUD LOOP, IT IS NECESSARY TO SEPARATE OUT
!   TRANSMISSION FCTNS AT THE TOP AND BOTTOM OF THE CLOUDS, FOR
!   EACH BAND N. THE REQUIRED QUANTITIES ARE:
!      TTD(I,KTOPSW(I,K),N)  K RUNS FROM 1 TO NCLDS(I)+1:
!      TTD(I,KBTMSW(I,K),N)  K RUNS FROM 2 TO NCLDS(I)+1:
!      TTU(I,KTOPSW(I,K),N)  K RUNS FROM 1 TO NCLDS(I)+1:
!      AND INVERSES OF THE ABOVE. THE ABOVE QUANTITIES ARE STORED
!      IN TDCL1,TDCL2,TUCL1,AND DFNTRN,UFNTRN,RESPECTIVELY, AS
!      THEY HAVE MULTIPLE USE IN THE PGM.
!---FOR FIRST CLOUD LAYER (GROUND) TDCL1,TUCL1 ARE KNOWN:
        DO 830 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 830
          TDCL1 (I,1) = TTD(I,LP1)
          TUCL1 (I,1) = TTU(I,LP1)
          TDCL2 (I,1) = TDCL1(I,1)
          DFNTRN(I,1) = ONE/TDCL1(I,1)
          UFNTRN(I,1) = DFNTRN(I,1)
830     CONTINUE
        DO 841 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 841
        DO 840 KK=2,KCLDS+1
          TDCL1(I,KK) = TTD(I,KTOPSW(I,KK))
          TUCL1(I,KK) = TTU(I,KTOPSW(I,KK))
          TDCL2(I,KK) = TTD(I,KBTMSW(I,KK))
840     CONTINUE
841     CONTINUE
        DO 851 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 851
        DO 850 KK=2,KCLDS+1
          DFNTRN(I,KK) = ONE/TDCL1(I,KK)
          UFNTRN(I,KK) = ONE/TUCL1(I,KK)
850     CONTINUE
851     CONTINUE
        DO 861 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 861
        DO 860 KK=1,KCLDS
          TCLU(I,KK) = TDCL1(I,KK)*DFNTRN(I,KK+1)*CT(I,KK+1)
          TCLD(I,KK) = TDCL1(I,KK)/TDCL2(I,KK+1)
860     CONTINUE
861     CONTINUE
!***THE FOLLOWING IS THE RECURSION RELATION FOR ALFA: THE REFLECTION
!   COEFFICIENT FOR A SYSTEM INCLUDING THE CLOUD IN QUESTION AND THE
!   FLUX COMING OUT OF THE CLOUD SYSTEM INCLUDING ALL CLOUDS BELOW
!   THE CLOUD IN QUESTION.
        DO 870 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 870
          ALFA (I,1) = CR(I,1)
          ALFAU(I,1) = ZERO
870     CONTINUE
!---AGAIN,EXCESSIVE CALCULATIONS-MAY BE CHANGED LATER!
        DO 881 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 881
        DO 880 KK=2,KCLDS+1
          ALFAU(I,KK) = TCLU(I,KK-1)*TCLU(I,KK-1)*ALFA(I,KK-1)/(ONE - &
                   TCLD(I,KK-1)*TCLD(I,KK-1)*ALFA(I,KK-1)*CR(I,KK))
          ALFA (I,KK) = ALFAU(I,KK)+CR(I,KK)
880     CONTINUE
881     CONTINUE
!     CALCULATE UFN AT CLOUD TOPS AND DFN AT CLOUD BOTTOMS
!---NOTE THAT UFNCLU(I,KCLDS+1) GIVES THE UPWARD FLUX AT THE TOP
!   OF THE HIGHEST REAL CLOUD (IF NCLDS(I)=KCLDS). IT GIVES THE FLUX
!   AT THE TOP OF THE ATMOSPHERE IF NCLDS(I) < KCLDS. IT THE FIRST
!   CASE, TDCL1 EQUALS THE TRANSMISSION FCTN TO THE TOP OF THE
!   HIGHEST CLOUD, AS WE WANT. IN THE SECOND CASE, TDCL1=1, SO UFNCLU
!   EQUALS ALFA. THIS IS ALSO CORRECT.
        DO 890 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 890
          UFNCLU(I,KCLDS+1) = ALFA(I,KCLDS+1)*TDCL1(I,KCLDS+1)
          DFNCLU(I,KCLDS+1) = TDCL1(I,KCLDS+1)
890     CONTINUE
        DO 901 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 901
        DO 900 KK=KCLDS,1,-1
!
!***  ACCOUNT FOR UNREALISTICALLY SMALL CLOUD AMOUNT
!
        DENOM=ALFA(I,KK+1)*TCLU(I,KK)
        IF(DENOM.GT.RTHRESH)THEN
          UFNCLU(I,KK)=UFNCLU(I,KK+1)*ALFAU(I,KK+1)/DENOM
        ELSE
          UFNCLU(I,KK)=0.
        ENDIF
        IF(ALFA(I,KK).GT.RTHRESH)THEN
          DFNCLU(I,KK)=UFNCLU(I,KK)/ALFA(I,KK)
        ELSE
          DFNCLU(I,KK)=0.
        ENDIF
900     CONTINUE
901     CONTINUE
!     NOW OBTAIN DFN AND UFN FOR LEVELS BETWEEN THE CLOUDS
        DO 911 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 911
        DO 910 KK=1,KCLDS+1
          UFNTRN(I,KK) = UFNCLU(I,KK)*UFNTRN(I,KK)
          DFNTRN(I,KK) = DFNCLU(I,KK)*DFNTRN(I,KK)
910     CONTINUE
911     CONTINUE
        DO 930 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 930
          J2=KBTMSW(I,2)
          DO 920 K=J2,LP1
            UFN(I,K) = UFNTRN(I,1)*TTU(I,K)
            DFN(I,K) = DFNTRN(I,1)*TTD(I,K)
920       CONTINUE
930     CONTINUE
        DO 970  I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 970
        DO 965  KK=2,KCLDS+1
          J1 = KTOPSW(I,KK)
          J2 = KBTMSW(I,KK+1)
          IF (J1.EQ.1) GO TO 965
          DO 940 K=J2,J1
            UFN(I,K) = UFNTRN(I,KK)*TTU(I,K)
            DFN(I,K) = DFNTRN(I,KK)*TTD(I,K)
940       CONTINUE
          J3 = KBTMSW(I,KK)
          IF ((J3-J1).GT.1) THEN
            TEMPF = (UFNCLU(I,KK)-UFN(I,J3))*DPCLD(I,KK-1)
            TEMPG = (DFNCLU(I,KK)-DFN(I,J3))*DPCLD(I,KK-1)
            DO 950 K=J1+1,J3-1
              UFN(I,K) = UFNCLU(I,KK)+TEMPF*(PP(I,K)-PPTOP(I,KK-1))
              DFN(I,K) = DFNCLU(I,KK)+TEMPG*(PP(I,K)-PPTOP(I,KK-1))
950         CONTINUE
          ENDIF
965     CONTINUE
970     CONTINUE
        DO 980 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 980
        DO 981 K=1,LP1
          DFSWC(I,K) = DFSWC(I,K) + DFN(I,K)*DFNTOP(I,N)
          UFSWC(I,K) = UFSWC(I,K) + UFN(I,K)*DFNTOP(I,N)
981     CONTINUE
980     CONTINUE
        DO 990 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 990
          GDFND(I) = GDFND(I) + CCMAX(I)*DFN(I,LP1)*DFNTOP(I,N)
990     CONTINUE
1000  CONTINUE
      DO 1100 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 1100
      DO 1101 K=1,LP1
        DFSWC(I,K) = TMP1(I)*DFSWL(I,K) + CCMAX(I)*DFSWC(I,K)
        UFSWC(I,K) = TMP1(I)*UFSWL(I,K) + CCMAX(I)*UFSWC(I,K)
1101  CONTINUE
1100  CONTINUE
      DO 1200 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 1200
        DO 1201 KK=1,LP1
        FSWC(I,KK) = UFSWC(I,KK)-DFSWC(I,KK)
1201    CONTINUE
1200  CONTINUE
      DO 1250 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 1250
        DO 1251 KK=1, L
        HSWC(I,KK) = RADCON*(FSWC(I,KK+1)-FSWC(I,KK))/DP(I,KK)
1251    CONTINUE
1250  CONTINUE

  END SUBROUTINE SWR93
!-----------------------------------------------------------------------

  SUBROUTINE RADFS & 

!     *****************************************************************
!     *                                                               *
!     *   THE INTERNAL DRIVE FOR GFDL RADIATION                       *
!     *   THIS SUBROUTINE WAS FROM Y.H AND K.A.C (1993)               *
!     *   AND MODIFIED BY Q. ZHAO FOR USE IN THE ETA MODEL            *
!     *                   NOV. 18,  1993                              *
!     *                                                               *
!     * UPDATE: THIS SUBROUTINE WAS MODIFIED TO USE CLOUD FRACTION    *
!     *         ON EACH MODEL LAYER.                                  *
!     *                                QINGYUN  ZHAO   95-3-22        *
!     *                                                               *
!     * UPDATE: R1 HAS BEEN ADDED TO THE INPUTS FROM RADTN TO         *
!     *         COMPUTE THE VARIATION OF SOLAR CONSTANT AT THE TOP    *
!     *         OF ATMOSPHERE WITH JULIAN DAY IN A YEAR.              *
!     *                                QINGYUN  ZHAO   96-7-23        *
!     *****************************************************************
!***
!***  REQUIRED INPUT:
!***
                (QS,PP,PPI,QQH2O,TT,O3QO3,TSFC,SLMSK,ALBEDO,XLAT &
      ,          CAMT,KTOP,KBTM,NCLDS,EMCLD,RRCL,TTCL &
      ,          COSZRO,TAUDAR,IBEG &
      ,          KO3,KALB &
      ,          ITIMSW,ITIMLW &
!***************************************************************************
!*              IX IS THE LENGTH OF A ROW IN THE DOMAIN
!
!*   QS(IX):		THE SURFACE PRESSURE (PA)
!*   PP(IX,L):		THE MIDLAYER PRESSURES (PA)  (L IS THE VERT. DIMEN.)
!*   PPI(IX,LP1)	THE INTERFACE PRESSURES (PA)
!*   QQH2O(IX,L):	THE MIDLAYER WATER VAPOR MIXING RATIO (KG/KG)
!*   TT(IX,L):		THE MIDLAYER TEMPERATURE (K)
!*   O3QO3(IX,L):	THE MIDLAYER OZONE MIXING RATIO
!*   TSFC(IX):		THE SKIN TEMP. (K); NEGATIVE OVER WATER
!*   SLMSK(IX):		THE SEA MASK (LAND=0,SEA=1)
!*   ALBEDO(IX):	THE SURFACE ALBEDO (EXPRESSED AS A FRACTION)
!*   XLAT(IX):		THE GEODETIC LATITUDES OF EACH COLUMN IN DEGREES
!*				(N.H.> 0)
!* THE FOLLOWING ARE CLOUD INFORMATION FOR EACH CLOUD LAYER
!*                      LAYER=1:SURFACE
!*                      LAYER=2:FIRST LAYER ABOVE GROUND, AND SO ON
!*   CAMT(IX,LP1):      CLOUD FRACTION OF EACH CLOUD LAYER
!*   ITYP(IX,LP1):      CLOUD TYPE(=1: STRATIFORM, =2:CONVECTIVE)
!*   KTOP(IX,LP1):      HEIGHT OF CLOUD TOP OF EACH CLOUD LAYER (IN ETA LEVEL)
!*   KBTM(IX,LP1):      BOTTOM OF EACH CLOUD LAYER
!*   NCLDS(IX):         NUMBER OF CLOUD LAYERS
!*   EMCLD(IX,LP1):     CLOUD EMISSIVITY
!*   RRCL(IX,NB,LP1)    CLOUD REFLECTTANCES FOR SW SPECTRAL BANDS
!*   TTCL(IX,NB,LP1)    CLOUD TRANSMITANCES FOR SW SPECTRAL BANDS
!* THE ABOVE ARE CLOUD INFORMATION FOR EACH CLOUD LAYER
!*
!*   COSZRO(IX):	THE COSINE OF THE SOLAR ZENITH ANGLE
!*   TAUDAR:		=1.0
!*   IBEG:		=1
!*   KO3:		=1 ( READ IN THE QZONE DATA)
!*   KALB:		=0
!*   SLMRF(LP1):	THE INTERFACE'S ETA (LP1=L+1)
!*   SLYMRF(L):		THE MIDLAYER ETA 
!*   ITIMSW:		=1/0 (SHORTWAVE CALC. ARE DESIRED/NOT DESIRED)
!*   ITIMLW:		=1/0 (LONGWAVE CALC. ARE DESIRED/NOT DESIRED)
!************************************************************************
!***
!*** THE FOLLOWING ARE ADDITIONAL FOR ETA MODEL
!***
      ,          JD,GMT &
!**************************************************************************
!*   JD: JULIAN DAY IN A YEAR
!*   R1: THE NON-DIMENSIONAL SUN-EARTH DISTANCE
!*   GMT:HOUR
!**************************************************************************
!***
!*** GENERATED OUTPUT REQUIRED BY THE ETA MODEL
!***
      ,          SWH,HLW &
!     ,          T1,T2,T4,EM1V,EM1VW,EM3V                       &
      ,          FLWUP,FSWUP,FSWDN,FSWDNS,FSWUPS,FLWDNS,FLWUPS  &
!     ,          DDUO3N,DDO3N2,DDO3N3,DDO3N4                    &
!     ,          SKO3R,AB15WD,SKC1R,SKO2D                       &
      ,          ids,ide, jds,jde, kds,kde                      &
      ,          ims,ime, jms,jme, kms,kme                      &
      ,          its,ite, jts,jte, kts,kte                      )
!************************************************************************
!*    SWH: ATMOSPHERIC SHORTWAVE HEATING RATES IN K/S.
!*         SWH IS A REAL ARRAY DIMENSIONED (NCOL X LM).
!*    HLW: ATMOSPHERIC LONGWAVE HEATING RATES IN K/S.
!*         HLW IS A REAL ARRAY DIMENSIONED (NCOL X LM).
!*  FLWUP: UPWARD LONGWAVE FLUX AT TOP OF THE ATMOSPHERE IN W/M**2.
!*         FLWUP IS A REAL ARRAY DIMENSIONED (NCOL).
!*  FSWUP: UPWARD SHORTWAVE FLUX AT TOP OF THE ATMOSPHERE IN W/M**2.
!*         FSWUP IS A REAL ARRAY DIMENSIONED (NCOL).
!*  FSWDN: DOWNWARD SHORTWAVE FLUX AT TOP OF THE ATMOSPHERE IN W/M**2.
!*         FSWDN IS A REAL ARRAY DIMENSIONED (NCOL).
!* FSWDNS: DOWNWARD SHORTWAVE FLUX AT THE SURFACE IN W/M**2.
!*         FSWDNS IS A REAL ARRAY DIMENSIONED (NCOL).
!* FSWUPS: UPWARD SHORTWAVE FLUX AT THE SURFACE IN W/M**2.
!*         FSWUPS IS A REAL ARRAY DIMENSIONED (NCOL).
!* FLWDNS: DOWNWARD LONGWAVE FLUX AT THE SURFACE IN W/M**2.
!*         FLWDNS IS A REAL ARRAY DIMENSIONED (NCOL).
!* FLWUPS: UPWARD LONGWAVE FLUX AT THE SURFACE IN W/M**2.
!*         FLWUPS IS A REAL ARRAY DIMENSIONED (NCOL).
!************************************************************************
!***
!*** THE FOLLOWING OUTPUTS ARE NOT REQUIRED BY THE ETA MODEL
!***
!----------------------------------------------------------------------
 IMPLICIT NONE
!----------------------------------------------------------------------
!INTEGER, PARAMETER :: NBLY=15
 INTEGER, PARAMETER :: NB=12
 INTEGER, PARAMETER :: NBLX=47
 INTEGER , PARAMETER:: NBLW = 163

 REAL,PARAMETER ::      AMOLWT=28.9644
 REAL,PARAMETER ::      CSUBP=1.00484E7
 REAL,PARAMETER ::      DIFFCTR=1.66
 REAL,PARAMETER ::      G=980.665
 REAL,PARAMETER ::      GINV=1./G
 REAL,PARAMETER ::      GRAVDR=980.0
 REAL,PARAMETER ::      O3DIFCTR=1.90
 REAL,PARAMETER ::      P0=1013250.
 REAL,PARAMETER ::      P0INV=1./P0
 REAL,PARAMETER ::      GP0INV=GINV*P0INV
 REAL,PARAMETER ::      P0XZP2=202649.902
 REAL,PARAMETER ::      P0XZP8=810600.098
 REAL,PARAMETER ::      P0X2=2.*1013250.
 REAL,PARAMETER ::      RADCON=8.427
 REAL,PARAMETER ::      RADCON1=1./8.427
 REAL,PARAMETER ::      RATCO2MW=1.519449738
 REAL,PARAMETER ::      RATH2OMW=.622
 REAL,PARAMETER ::      RGAS=8.3142E7
 REAL,PARAMETER ::      RGASSP=8.31432E7
 REAL,PARAMETER ::      SECPDA=8.64E4
!
!******THE FOLLOWING ARE MATHEMATICAL CONSTANTS*******
!        ARRANGED IN DECREASING ORDER
 REAL,PARAMETER ::      HUNDRED=100.
 REAL,PARAMETER ::      HNINETY=90.
 REAL,PARAMETER ::      HNINE=9.0
 REAL,PARAMETER ::      SIXTY=60.
 REAL,PARAMETER ::      FIFTY=50.
 REAL,PARAMETER ::      TEN=10.
 REAL,PARAMETER ::      EIGHT=8.
 REAL,PARAMETER ::      FIVE=5.
 REAL,PARAMETER ::      FOUR=4.
 REAL,PARAMETER ::      THREE=3.
 REAL,PARAMETER ::      TWO=2.
 REAL,PARAMETER ::      ONE=1.
 REAL,PARAMETER ::      HAF=0.5
 REAL,PARAMETER ::      QUARTR=0.25
 REAL,PARAMETER ::      ZERO=0.
!
!******FOLLOWING ARE POSITIVE FLOATING POINT CONSTANTS(H'S)
!       ARRANGED IN DECREASING ORDER
 REAL,PARAMETER ::      H83E26=8.3E26
 REAL,PARAMETER ::      H71E26=7.1E26
 REAL,PARAMETER ::      H1E15=1.E15
 REAL,PARAMETER ::      H1E13=1.E13
 REAL,PARAMETER ::      H1E11=1.E11
 REAL,PARAMETER ::      H1E8=1.E8
 REAL,PARAMETER ::      H2E6=2.0E6
 REAL,PARAMETER ::      H1E6=1.0E6
 REAL,PARAMETER ::      H69766E5=6.97667E5
 REAL,PARAMETER ::      H4E5=4.E5
 REAL,PARAMETER ::      H165E5=1.65E5
 REAL,PARAMETER ::      H5725E4=57250.
 REAL,PARAMETER ::      H488E4=48800.
 REAL,PARAMETER ::      H1E4=1.E4
 REAL,PARAMETER ::      H24E3=2400.
 REAL,PARAMETER ::      H20788E3=2078.8
 REAL,PARAMETER ::      H2075E3=2075.
 REAL,PARAMETER ::      H18E3=1800.
 REAL,PARAMETER ::      H1224E3=1224.
 REAL,PARAMETER ::      H67390E2=673.9057
 REAL,PARAMETER ::      H5E2=500.
 REAL,PARAMETER ::      H3082E2=308.2
 REAL,PARAMETER ::      H3E2=300.
 REAL,PARAMETER ::      H2945E2=294.5
 REAL,PARAMETER ::      H29316E2=293.16
 REAL,PARAMETER ::      H26E2=260.0
 REAL,PARAMETER ::      H25E2=250.
 REAL,PARAMETER ::      H23E2=230.
 REAL,PARAMETER ::      H2E2=200.0
 REAL,PARAMETER ::      H15E2=150.
 REAL,PARAMETER ::      H1386E2=138.6
 REAL,PARAMETER ::      H1036E2=103.6
 REAL,PARAMETER ::      H8121E1=81.21
 REAL,PARAMETER ::      H35E1=35.
 REAL,PARAMETER ::      H3116E1=31.16
 REAL,PARAMETER ::      H28E1=28.
 REAL,PARAMETER ::      H181E1=18.1
 REAL,PARAMETER ::      H18E1=18.
 REAL,PARAMETER ::      H161E1=16.1
 REAL,PARAMETER ::      H16E1=16.
 REAL,PARAMETER ::      H1226E1=12.26
 REAL,PARAMETER ::      H9P94=9.94
 REAL,PARAMETER ::      H6P08108=6.081081081
 REAL,PARAMETER ::      H3P6=3.6
 REAL,PARAMETER ::      H3P5=3.5
 REAL,PARAMETER ::      H2P9=2.9
 REAL,PARAMETER ::      H2P8=2.8
 REAL,PARAMETER ::      H2P5=2.5
 REAL,PARAMETER ::      H1P8=1.8
 REAL,PARAMETER ::      H1P4387=1.4387
 REAL,PARAMETER ::      H1P41819=1.418191
 REAL,PARAMETER ::      H1P4=1.4
 REAL,PARAMETER ::      H1P25892=1.258925411
 REAL,PARAMETER ::      H1P082=1.082
 REAL,PARAMETER ::      HP816=0.816
 REAL,PARAMETER ::      HP805=0.805
 REAL,PARAMETER ::      HP8=0.8
 REAL,PARAMETER ::      HP60241=0.60241
 REAL,PARAMETER ::      HP602409=0.60240964
 REAL,PARAMETER ::      HP6=0.6
 REAL,PARAMETER ::      HP526315=0.52631579
 REAL,PARAMETER ::      HP518=0.518
 REAL,PARAMETER ::      HP5048=0.5048
 REAL,PARAMETER ::      HP3795=0.3795
 REAL,PARAMETER ::      HP369=0.369
 REAL,PARAMETER ::      HP26=0.26
 REAL,PARAMETER ::      HP228=0.228
 REAL,PARAMETER ::      HP219=0.219
 REAL,PARAMETER ::      HP166666=.166666
 REAL,PARAMETER ::      HP144=0.144
 REAL,PARAMETER ::      HP118666=0.118666192
 REAL,PARAMETER ::      HP1=0.1
!        (NEGATIVE EXPONENTIALS BEGIN HERE)
 REAL,PARAMETER ::      H658M2=0.0658
 REAL,PARAMETER ::      H625M2=0.0625
 REAL,PARAMETER ::      H44871M2=4.4871E-2
 REAL,PARAMETER ::      H44194M2=.044194
 REAL,PARAMETER ::      H42M2=0.042
 REAL,PARAMETER ::      H41666M2=0.0416666
 REAL,PARAMETER ::      H28571M2=.02857142857
 REAL,PARAMETER ::      H2118M2=0.02118
 REAL,PARAMETER ::      H129M2=0.0129
 REAL,PARAMETER ::      H1M2=.01
 REAL,PARAMETER ::      H559M3=5.59E-3
 REAL,PARAMETER ::      H3M3=0.003
 REAL,PARAMETER ::      H235M3=2.35E-3
 REAL,PARAMETER ::      H1M3=1.0E-3
 REAL,PARAMETER ::      H987M4=9.87E-4
 REAL,PARAMETER ::      H323M4=0.000323
 REAL,PARAMETER ::      H3M4=0.0003
 REAL,PARAMETER ::      H285M4=2.85E-4
 REAL,PARAMETER ::      H1M4=0.0001
 REAL,PARAMETER ::      H75826M4=7.58265E-4
 REAL,PARAMETER ::      H6938M5=6.938E-5
 REAL,PARAMETER ::      H394M5=3.94E-5
 REAL,PARAMETER ::      H37412M5=3.7412E-5
 REAL,PARAMETER ::      H15M5=1.5E-5
 REAL,PARAMETER ::      H1439M5=1.439E-5
 REAL,PARAMETER ::      H128M5=1.28E-5
 REAL,PARAMETER ::      H102M5=1.02E-5
 REAL,PARAMETER ::      H1M5=1.0E-5
 REAL,PARAMETER ::      H7M6=7.E-6
 REAL,PARAMETER ::      H4999M6=4.999E-6
 REAL,PARAMETER ::      H451M6=4.51E-6
 REAL,PARAMETER ::      H25452M6=2.5452E-6
 REAL,PARAMETER ::      H1M6=1.E-6
 REAL,PARAMETER ::      H391M7=3.91E-7
 REAL,PARAMETER ::      H1174M7=1.174E-7
 REAL,PARAMETER ::      H8725M8=8.725E-8
 REAL,PARAMETER ::      H327M8=3.27E-8
 REAL,PARAMETER ::      H257M8=2.57E-8
 REAL,PARAMETER ::      H1M8=1.0E-8
 REAL,PARAMETER ::      H23M10=2.3E-10
 REAL,PARAMETER ::      H14M10=1.4E-10
 REAL,PARAMETER ::      H11M10=1.1E-10
 REAL,PARAMETER ::      H1M10=1.E-10
 REAL,PARAMETER ::      H83M11=8.3E-11
 REAL,PARAMETER ::      H82M11=8.2E-11
 REAL,PARAMETER ::      H8M11=8.E-11
 REAL,PARAMETER ::      H77M11=7.7E-11
 REAL,PARAMETER ::      H72M11=7.2E-11
 REAL,PARAMETER ::      H53M11=5.3E-11
 REAL,PARAMETER ::      H48M11=4.8E-11
 REAL,PARAMETER ::      H44M11=4.4E-11
 REAL,PARAMETER ::      H42M11=4.2E-11
 REAL,PARAMETER ::      H37M11=3.7E-11
 REAL,PARAMETER ::      H35M11=3.5E-11
 REAL,PARAMETER ::      H32M11=3.2E-11
 REAL,PARAMETER ::      H3M11=3.0E-11
 REAL,PARAMETER ::      H28M11=2.8E-11
 REAL,PARAMETER ::      H24M11=2.4E-11
 REAL,PARAMETER ::      H23M11=2.3E-11
 REAL,PARAMETER ::      H2M11=2.E-11
 REAL,PARAMETER ::      H18M11=1.8E-11
 REAL,PARAMETER ::      H15M11=1.5E-11
 REAL,PARAMETER ::      H14M11=1.4E-11
 REAL,PARAMETER ::      H114M11=1.14E-11
 REAL,PARAMETER ::      H11M11=1.1E-11
 REAL,PARAMETER ::      H1M11=1.E-11
 REAL,PARAMETER ::      H96M12=9.6E-12
 REAL,PARAMETER ::      H93M12=9.3E-12
 REAL,PARAMETER ::      H77M12=7.7E-12
 REAL,PARAMETER ::      H74M12=7.4E-12
 REAL,PARAMETER ::      H65M12=6.5E-12
 REAL,PARAMETER ::      H62M12=6.2E-12
 REAL,PARAMETER ::      H6M12=6.E-12
 REAL,PARAMETER ::      H45M12=4.5E-12
 REAL,PARAMETER ::      H44M12=4.4E-12
 REAL,PARAMETER ::      H4M12=4.E-12
 REAL,PARAMETER ::      H38M12=3.8E-12
 REAL,PARAMETER ::      H37M12=3.7E-12
 REAL,PARAMETER ::      H3M12=3.E-12
 REAL,PARAMETER ::      H29M12=2.9E-12
 REAL,PARAMETER ::      H28M12=2.8E-12
 REAL,PARAMETER ::      H24M12=2.4E-12
 REAL,PARAMETER ::      H21M12=2.1E-12
 REAL,PARAMETER ::      H16M12=1.6E-12
 REAL,PARAMETER ::      H14M12=1.4E-12
 REAL,PARAMETER ::      H12M12=1.2E-12
 REAL,PARAMETER ::      H8M13=8.E-13
 REAL,PARAMETER ::      H46M13=4.6E-13
 REAL,PARAMETER ::      H36M13=3.6E-13
 REAL,PARAMETER ::      H135M13=1.35E-13
 REAL,PARAMETER ::      H12M13=1.2E-13
 REAL,PARAMETER ::      H1M13=1.E-13
 REAL,PARAMETER ::      H3M14=3.E-14
 REAL,PARAMETER ::      H15M14=1.5E-14
 REAL,PARAMETER ::      H14M14=1.4E-14
!
!******FOLLOWING ARE NEGATIVE FLOATING POINT CONSTANTS (HM'S)
!          ARRANGED IN DESCENDING ORDER
 REAL,PARAMETER ::      HM2M2=-.02
 REAL,PARAMETER ::      HM6666M2=-.066667
 REAL,PARAMETER ::      HMP5=-0.5
 REAL,PARAMETER ::      HMP575=-0.575
 REAL,PARAMETER ::      HMP66667=-.66667
 REAL,PARAMETER ::      HMP805=-0.805
 REAL,PARAMETER ::      HM1EZ=-1.
 REAL,PARAMETER ::      HM13EZ=-1.3
 REAL,PARAMETER ::      HM19EZ=-1.9
 REAL,PARAMETER ::      HM1E1=-10.
 REAL,PARAMETER ::      HM1597E1=-15.97469413
 REAL,PARAMETER ::      HM161E1=-16.1
 REAL,PARAMETER ::      HM1797E1=-17.97469413
 REAL,PARAMETER ::      HM181E1=-18.1
 REAL,PARAMETER ::      HM8E1=-80.
 REAL,PARAMETER ::      HM1E2=-100.
!
 REAL,PARAMETER ::      H1M16=1.0E-16
 REAL,PARAMETER ::      H1M20=1.E-20
 REAL,PARAMETER ::      HP98=0.98
 REAL,PARAMETER ::      Q19001=19.001
 REAL,PARAMETER ::      DAYSEC=1.1574E-5
 REAL,PARAMETER ::      HSIGMA=5.673E-5
 REAL,PARAMETER ::      TWENTY=20.0
 REAL,PARAMETER ::      HP537=0.537
 REAL,PARAMETER ::      HP2=0.2
 REAL,PARAMETER ::      RCO2=3.3E-4
 REAL,PARAMETER ::      Q14330=1.43306E-6
 REAL,PARAMETER ::      H3M6=3.0E-6
 REAL,PARAMETER ::      PI=3.1415927
 REAL,PARAMETER ::      DEGRAD=180.0/PI
 REAL,PARAMETER ::      H74E1=74.0
 REAL,PARAMETER ::      H15E1=15.0

 REAL, PARAMETER:: B0 = -.51926410E-4
 REAL, PARAMETER:: B1 = -.18113332E-3
 REAL, PARAMETER:: B2 = -.10680132E-5
 REAL, PARAMETER:: B3 = -.67303519E-7
 REAL, PARAMETER:: AWIDE = 0.309801E+01
 REAL, PARAMETER:: BWIDE = 0.495357E-01
 REAL, PARAMETER:: BETAWD = 0.347839E+02
 REAL, PARAMETER:: BETINW = 0.766811E+01


      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte
      INTEGER, INTENT(IN)        :: IBEG,KO3,KALB,ITIMSW,ITIMLW,JD
      REAL,    INTENT(IN)        :: GMT
!----------------------------------------------------------------------
!      ****************************************************************
!      *  GENERALIZED FOR PLUG-COMPATIBILITY -                        *
!      *    ORIGINAL CODE WAS CLEANED-UP GFDL CODE...K.CAMPANA MAR89..*
!......*  EXAMPLE FOR MRF:                                            *
!      *    KO3  =0  AND O3QO3=DUMMY ARRAY.   (GFDL CLIMO O3 USED)    *
!      *    KEMIS=0  AND HI CLD EMIS COMPUTED HERE (CEMIS=DUMMY INPUT)*
!      *    KALB =0  AND SFC ALBEDO OVER OPEN WATER COMPUTED BELOW... *
!      *    KCCO2=0,CO2 OBTAINED FROM BLOCK DATA                      *
!      *         =1,CO2 COMPUTED IN HERE --- NOT AVAILABLE YET...     *
!      *    SLMRF = INTERFACE (LEVELS) SIGMA                          *
!      *    SLYMRF= LAYER SIGMA                                       *
!      *  UPDATED FOR YUTAI HOU SIB SW RADIATION....KAC 6 MAR 92      *
!      *    OCEAN ALBEDO FOR BEAM SET TO BULK SFCALB, SINCE           *
!      *       COSINE ZENITH ANGLE EFFECTS ALREADY THERE(REF:PAYNE)   *
!      *       SLMSK = 0.                                             *
!      *    SNOW ICE ALBEDO FOR BEAM NOT ENHANCED VIA COSINE ZENITH   *
!      *       ANGLE EITHER CAUSE VALU ALREADY HIGH (WE SEE POLAR     *
!      *       COOLING IF WE DO BEAM CALCULATION)....KAC 17MAR92      *
!      *       ALBEDO GE .5                                           *
!      *   UPDATED TO OBTAIN CLEAR SKY FLUXES "ON THE FLY" FOR        *
!      *       CLOUD FORCING DIAGNOSTICS ELSEWHERE...KAC 7AUG92       *
!      *       SEE ##CLR LINES...RADFS,LWR88,FST88,SPA88 .......      *
!      *  UPDATED FOR USE NEW CLD SCHEME      ......YH  DEC 92        *
!      *    INPUT CLD MAY BE AS ORIGINAL IN 3 DOMAIN (CLD,MTOP,MBOT)  *
!      *       OR IN A VERTICAL ARRAY OF 18 MDL LAYERS (CLDARY)       *
!      *    IEMIS=0  USE THE ORG. CLD EMIS SCHEME                     *
!      *         =1  USE TEMP DEP. CLD EMIS SCHEME                    *
!      *  UPDATED TO COMPUTE CLD LAYER REFLECTTANCE AND TRANSMITTANCE *
!      *    INPUT CLD EMISSIVITY AND OPTICAL THICKNESS 'EMIS0,TAUC0'  *
!      *                                      ......YH FEB 93         *
!      ****************************************************************
!--------------------------------
!     INTEGER, PARAMETER:: LNGTH=37*kte
!--------------------------------
     
!     REAL, INTENT(IN) :: SKO3R,AB15WD,SKC1R,SKO2D

      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte):: PP,TT
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte):: QQH2O
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1):: PPI,CAMT,EMCLD
      REAL,    INTENT(IN), DIMENSION(its:ite):: QS,TSFC,SLMSK,ALBEDO,XLAT
      REAL,    INTENT(IN), DIMENSION(its:ite):: COSZRO,TAUDAR
      REAL,    INTENT(OUT), DIMENSION(its:ite):: FLWUPS
      INTEGER, INTENT(IN), DIMENSION(its:ite):: NCLDS
      INTEGER, INTENT(IN), DIMENSION(its:ite,kts:kte+1):: KTOP,KBTM
      REAL,    INTENT(INOUT), DIMENSION(its:ite,NB,kts:kte+1):: TTCL,RRCL
      REAL, intent(IN), DIMENSION(its:ite,kts:kte):: O3QO3
!     REAL,  INTENT(IN),  DIMENSION(5040):: T1,T2,T4,EM1V,EM1VW
!     REAL,  INTENT(IN),  DIMENSION(5040) :: EM3V

!     REAL, DIMENSION(its:ite)::ALVBR,ALNBR, ALVDR,ALNDR

! TABLE ???

      REAL,  DIMENSION(3) :: BO3RND,AO3RND
      REAL,  DIMENSION(NBLY) :: APCM,BPCM,ATPCM,BTPCM,ACOMB, &
                                BCOMB,BETACM

      DATA AO3RND / 0.543368E+02,  0.234676E+04,  0.384881E+02/ 
      DATA BO3RND / 0.526064E+01,  0.922424E+01,  0.496515E+01/

      DATA ACOMB  / &
         0.152070E+05,  0.332194E+04,  0.527177E+03,  0.163124E+03, &
         0.268808E+03,  0.534591E+02,  0.268071E+02,  0.123133E+02, &
         0.600199E+01,  0.640803E+00,  0.501549E-01,  0.167961E-01, &
         0.178110E-01,  0.170166E+00,  0.537083E-02/
      DATA BCOMB  / &
         0.152538E+00,  0.118677E+00,  0.103660E+00,  0.100119E+00, &
         0.127518E+00,  0.118409E+00,  0.904061E-01,  0.642011E-01, &
         0.629660E-01,  0.643346E-01,  0.717082E-01,  0.629730E-01, &
         0.875182E-01,  0.857907E-01,  0.214005E+00/
      DATA APCM   / &
        -0.671879E-03,  0.654345E-02,  0.143657E-01,  0.923593E-02, &
         0.117022E-01,  0.159596E-01,  0.181600E-01,  0.145013E-01, &
         0.170062E-01,  0.233303E-01,  0.256735E-01,  0.274745E-01, &
         0.279259E-01,  0.197002E-01,  0.349782E-01/
      DATA BPCM   / &
        -0.113520E-04, -0.323965E-04, -0.448417E-04, -0.230779E-04, &
        -0.361981E-04, -0.145117E-04,  0.198349E-04, -0.486529E-04, &
        -0.550050E-04, -0.684057E-04, -0.447093E-04, -0.778390E-04, &
        -0.982953E-04, -0.772497E-04, -0.748263E-04/
      DATA ATPCM  / &
        -0.106346E-02,  0.641531E-02,  0.137362E-01,  0.922513E-02, &
         0.136162E-01,  0.169791E-01,  0.206959E-01,  0.166223E-01, &
         0.171776E-01,  0.229724E-01,  0.275530E-01,  0.302731E-01, &
         0.281662E-01,  0.199525E-01,  0.370962E-01/
      DATA BTPCM  / &
        -0.735731E-05, -0.294149E-04, -0.505592E-04, -0.280894E-04, &
        -0.492972E-04, -0.341508E-04, -0.362947E-04, -0.250487E-04, &
        -0.521369E-04, -0.746260E-04, -0.744124E-04, -0.881905E-04, &
        -0.933645E-04, -0.664045E-04, -0.115290E-03/
      DATA BETACM / &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.188625E+03,  0.144293E+03,  0.174098E+03,  0.909366E+02, &
         0.497489E+02,  0.221212E+02,  0.113124E+02,  0.754174E+01, &
         0.589554E+01,  0.495227E+01,  0.000000E+00/


!        *********************************************
!====>   *   OUTPUT TO CALLING PROGRAM               *
!        *********************************************

       REAL, INTENT(INOUT),DIMENSION(its:ite,kts:kte)::SWH,HLW
       REAL, INTENT(OUT), DIMENSION(its:ite):: FSWUP,FSWUPS,FSWDN, &
                           FSWDNS,FLWUP,FLWDNS
      
!        *********************************************
!====>   *   POSSIBLE OUTPUT TO CALLING PROGRAM      *
!        *********************************************

      REAL, DIMENSION(its:ite):: GDFVBR,GDFNBR,GDFVDR,GDFNDR

!        ************************************************************
!====>   *   ARRAYS NEEDED BY SWR91SIB..FOR CLEAR SKY DATA(EG.FSWL) *
!        ************************************************************

      REAL, DIMENSION(its:ite,kts:kte+1)::FSWL,HSWL,UFL,DFL

!        ******************************************************
!====>   *   ARRAYS NEEDED BY CLO88, LWR88, SWR89 OR SWR91SIB *
!        ******************************************************

       REAL, DIMENSION(its:ite,kts:kte+1,kts:kte+1)::CLDFAC
       REAL, DIMENSION(its:ite,kts:kte+1)::EQCMT,PRESS,TEMP,FSW,HSW,UF,DF
       REAL, DIMENSION(its:ite,kts:kte)::RH2O,QO3,HEATRA
       REAL, DIMENSION(its:ite):: COSZEN,TAUDA,GRNFLX,TOPFLX,GRDFLX
       REAL, DIMENSION(kts:kte+1)::PHALF
!..... ADD PRESSURE INTERFACE

       REAL,    DIMENSION(NB) :: ABCFF,PWTS

       DATA ABCFF/2*4.0E-5,.002,.035,.377,1.95,9.40,44.6,190., &
                  989.,2706.,39011./
       DATA PWTS/.5000,.121416,.0698,.1558,.0631,.0362,.0243,.0158,.0087, &
                 .001467,.002342,.001075/

       REAL     :: CFCO2,CFO3,REFLO3,RRAYAV

       DATA CFCO2,CFO3/508.96,466.64/
       DATA REFLO3/1.9/
       DATA RRAYAV/0.144/

!        *********************************************
!====>   *   VECTOR TEMPORARIES FOR CLOUD CALC.      *
!        *********************************************

       REAL,    DIMENSION(its:ite):: TTHAN
       REAL,    DIMENSION(its:ite,kts:kte):: DO3V,DO3VP
       INTEGER, DIMENSION(its:ite):: JJROW

!====>    **************************************************************
!--     SEASONAL CLIMATOLOGIES OF O3 (OBTAINED FROM A PREVIOUSLY RUN
!             CODE WHICH INTERPOLATES O3 TO USER VERTICAL COORDINATE).
!         DEFINED AS 5 DEG LAT MEANS N.P.->S.P.
!         COMMON /SAVMEM/ &
!-       ...WINTER....  ...SPRING....  ...SUMMER....  ....FALL.....
!        DDUO3N(37,L), DDO3N2(37,L), DDO3N3(37,L), DDO3N4(37,L)

       REAL, DIMENSION(37,kte) :: DDUO3N,DDO3N2,DDO3N3,DDO3N4

!     DIMENSION RAD1(37*kte), RAD2(37*kte), RAD3(37*kte), RAD4(37*kte)
!     EQUIVALENCE (RAD1(1),DDUO3N(1,1)),(RAD2(1),DDO3N2(1,1))
!     EQUIVALENCE (RAD3(1),DDO3N3(1,1)),(RAD4(1),DDO3N4(1,1))
!====>    **************************************************************
!
      REAL,   DIMENSION(21,20) :: ALBD
      REAL,   DIMENSION(20)    :: ZA
      REAL,   DIMENSION(21)    :: TRN
      REAL,   DIMENSION(19)    :: DZA

      REAL    :: YEAR,RLAG,TPI,SC,SSOLAR,DATE,RANG,TH2,ZEN,DZEN,ALB1,ALB2
      INTEGER :: IR,IQ,JX
      DATA TRN/.00,.05,.10,.15,.20,.25,.30,.35,.40,.45,.50,.55,.60,.65, &
               .70,.75,.80,.85,.90,.95,1.00/

      REAL ::  ALB11(21,7),ALB22(21,7),ALB33(21,6)

      EQUIVALENCE (ALB11(1,1),ALBD(1,1)),(ALB22(1,1),ALBD(1,8)), &
                  (ALB33(1,1),ALBD(1,15))
      DATA ALB11/ .061,.062,.072,.087,.115,.163,.235,.318,.395,.472,.542, &
       .604,.655,.693,.719,.732,.730,.681,.581,.453,.425,.061,.062,.070, &
       .083,.108,.145,.198,.263,.336,.415,.487,.547,.595,.631,.656,.670, &
       .652,.602,.494,.398,.370,.061,.061,.068,.079,.098,.130,.174,.228, &
       .290,.357,.424,.498,.556,.588,.603,.592,.556,.488,.393,.342,.325, &
       .061,.061,.065,.073,.086,.110,.150,.192,.248,.306,.360,.407,.444, &
       .469,.480,.474,.444,.386,.333,.301,.290,.061,.061,.065,.070,.082, &
       .101,.131,.168,.208,.252,.295,.331,.358,.375,.385,.377,.356,.320, &
       .288,.266,.255,.061,.061,.063,.068,.077,.092,.114,.143,.176,.210, &
       .242,.272,.288,.296,.300,.291,.273,.252,.237,.266,.220,.061,.061, &
       .062,.066,.072,.084,.103,.127,.151,.176,.198,.219,.236,.245,.250, &
       .246,.235,.222,.211,.205,.200/
      DATA ALB22/ .061,.061,.061,.065,.071,.079,.094,.113,.134,.154,.173, &
       .185,.190,.193,.193,.190,.188,.185,.182,.180,.178,.061,.061,.061, &
       .064,.067,.072,.083,.099,.117,.135,.150,.160,.164,.165,.164,.162, &
       .160,.159,.158,.157,.157,.061,.061,.061,.062,.065,.068,.074,.084, &
       .097,.111,.121,.127,.130,.131,.131,.130,.129,.127,.126,.125,.122, &
       .061,.061,.061,.061,.062,.064,.070,.076,.085,.094,.101,.105,.107, &
       .106,.103,.100,.097,.096,.095,.095,.095,.061,.061,.061,.060,.061, &
       .062,.065,.070,.075,.081,.086,.089,.090,.088,.084,.080,.077,.075, &
       .074,.074,.074,.061,.061,.060,.060,.060,.061,.063,.065,.068,.072, &
       .076,.077,.076,.074,.071,.067,.064,.062,.061,.061,.061,.061,.061, &
       .060,.060,.060,.060,.061,.062,.065,.068,.069,.069,.068,.065,.061, &
       .058,.055,.054,.053,.052,.052/
      DATA ALB33/ .061,.061,.060,.060,.060,.060,.060,.060,.062,.065,.065, &
       .063,.060,.057,.054,.050,.047,.046,.045,.044,.044,.061,.061,.060, &
       .060,.060,.059,.059,.059,.059,.059,.058,.055,.051,.047,.043,.039, &
       .035,.033,.032,.031,.031,.061,.061,.060,.060,.060,.059,.059,.058, &
       .057,.056,.054,.051,.047,.043,.039,.036,.033,.030,.028,.027,.026, &
       .061,.061,.060,.060,.060,.059,.059,.058,.057,.055,.052,.049,.045, &
       .040,.036,.032,.029,.027,.026,.025,.025,.061,.061,.060,.060,.060, &
       .059,.059,.058,.056,.053,.050,.046,.042,.038,.034,.031,.028,.026, &
       .025,.025,.025,.061,.061,.060,.060,.059,.058,.058,.057,.055,.053, &
       .050,.046,.042,.038,.034,.030,.028,.029,.025,.025,.025/
      DATA ZA/90.,88.,86.,84.,82.,80.,78.,76.,74.,70.,66.,62.,58.,54., &
              50.,40.,30.,20.,10.,0.0/
      DATA DZA/8*2.0,6*4.0,5*10.0/

!    ***********************************************************
!

       REAL,    DIMENSION(its:ite)        :: ALVB,ALNB,ALVD,ALND, &
                                             GDFVB,   &
                                             GDFNB,GDFVD,GDFND,   &
                                             SFCALB
       REAL :: SOLC,RSIN1,RCOS1,RCOS2

       REAL    :: ALBD0,ALVD1,ALND1,RRVCO2,RRCO2
       INTEGER :: N

!====>    BEGIN HERE             .......................
!
!         SOLC,THE SOLAR CONSTANT IS SCALED TO A MORE CURRENT VALUE.
!          I.E. IF SOLC=2.0 LY/MIN THEN SSOLAR=1.96 LY/MIN.
!..     RE-COMPUTED CAUSE SSOLAR OVERWRITTEN AS PART OF SCRATCH COMMON
!


      INTEGER :: K, I,KP,LLM2,J1,J3,KMAX,KMIN,KCLDS,ICNT,LLM1
      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE,LLP1,LL,KK,KLEN

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3; LLP1 = 2*L + 1
      LM1=L-1;  LM2=L-2;  LM3=L-3; LL = 2*L
      LLM2 = LL-2; LLM1=LL-1
      MYIS=its; MYIE=ite

!******ZHAO
!  NOTE: XLAT IS IN DEGREE HERE
!*****ZHAO
      YEAR=365.25
      RLAG=14.8125
      TPI=6.283185308
      SC=2.
      SOLC=SC/(R1*R1)
!*****************************
! Special note: The solar constant is reduced extra 3 percent to account
!               for the lack of aerosols in the shortwave radiation
!               parameterization.       Q. Zhao    96-7-23
!****************************
      SSOLAR=SOLC*HP98
      SSOLAR=SSOLAR*0.97
      DATE=JD+GMT/24.0
      RANG=TPI*(DATE-RLAG)/YEAR
      RSIN1=SIN(RANG)
      RCOS1=COS(RANG)
      RCOS2=COS(2.0*RANG)
      DO 40 I=MYIS,MYIE
        IR = I + IBEG - 1
        TH2=HP2*XLAT(IR)
        JJROW(I)=Q19001-TH2
        TTHAN(I)=(19-JJROW(I))-TH2
!.....  NOTE THAT THE NMC VARIABLES ARE IN MKS (THUS PRESSURE IS IN
!          CENTIBARS)WHILE ALL GFDL VARIABLES ARE IN CGS UNITS
        SFCALB(I) = ALBEDO(IR)
!.....  NOW PUT SFC TEMP,PRESSURES, ZENITH ANGLE INTO SW COMMON BLOCK...
!***ZHAO
!  NOTE: ALL PRESSURES INPUT FROM THE ETA MODEL ARE IN PA
!        THE UNIT FOR PRESS IS MICRO BAR 
!        SURFACE TEMPERATURE ARE NEGATIVE OVER OCEANS IN THE ETA MODEL
!***ZHAO
        PRESS(I,LP1)=QS(IR)*10.0
        TEMP(I,LP1)=ABS(TSFC(IR))
        COSZEN(I) = COSZRO(IR)
        TAUDA(I) = TAUDAR(IR)
   40 CONTINUE
!***ZHAO
!.....  ALL GFDL VARIABLES HAVE K=1 AT THE TOP OF THE ATMOSPHERE.NMC
!       ETA MODEL HAS THE SAME STRUCTURE
!***ZHAO
      DO 50 K=1,L
       DO 50 I=MYIS,MYIE
        IR = I + IBEG - 1
!.....  NOW PUT TEMP,PRESSURES, INTO SW COMMON BLOCK..........
        TEMP(I,K) = TT(IR,K)
        PRESS(I,K) = 10.0 * PP(IR,K)
!.... STORE LYR MOISTURE AND ADD TO SW COMMON BLOCK
        RH2O(I,K)=QQH2O(IR,K)
        IF(RH2O(I,K).LT.H3M6) RH2O(I,K)=H3M6
   50 CONTINUE
!...    *************************
      IF (KO3.EQ.0) GO TO 65
!...    *************************
      DO 60 K=1,L
       DO 60 I=MYIS,MYIE
        QO3(I,K) = O3QO3(I+IBEG-1,K)
   60 CONTINUE
   65 CONTINUE
!...   ************************************
      IF (KALB.GT.0) GO TO 110
!...   ************************************
!..... THE FOLLOWING CODE GETS ALBEDO FROM PAYNE,1972 TABLES IF
!         1) OPEN SEA POINT (SLMSK=1);2) KALB=0
      IQ=INT(TWENTY*HP537+ONE)
      DO 105 I=MYIS,MYIE
         IF(COSZEN(I).GT.0.0 .AND. SLMSK(I+IBEG-1).GT.0.5) THEN
           ZEN=DEGRAD*ACOS(MAX(COSZEN(I),0.0))
           IF(ZEN.GE.H74E1) JX=INT(HAF*(HNINETY-ZEN)+ONE)
           IF(ZEN.LT.H74E1.AND.ZEN.GE.FIFTY) &
              JX=INT(QUARTR*(H74E1-ZEN)+HNINE)
           IF(ZEN.LT.FIFTY) JX=INT(HP1*(FIFTY-ZEN)+H15E1)
           DZEN=-(ZEN-ZA(JX))/DZA(JX)
           ALB1=ALBD(IQ,JX)+DZEN*(ALBD(IQ,JX+1)-ALBD(IQ,JX))
           ALB2=ALBD(IQ+1,JX)+DZEN*(ALBD(IQ+1,JX+1)-ALBD(IQ+1,JX))
           SFCALB(I)=ALB1+TWENTY*(ALB2-ALB1)*(HP537-TRN(IQ))
         ENDIF
  105 CONTINUE
  110 CONTINUE
!        **********************************
      IF (KO3.GT.0) GO TO 135
!        **********************************
!.... COMPUTE CLIMATOLOGICAL ZONAL MEAN OZONE,
!....   SEASONAL AND SPATIAL INTERPOLATION DONE BELOW.
      DO 125 I=MYIS,MYIE

         PHALF(1)=0.
         PHALF(LP1)=PPI(I,kme)
         DO L=1,LM1
            PHALF(L+1)=PP(I,L) !  AETA(L)*PDIF+PT
         ENDDO

         CALL O3INT(PHALF,DDUO3N,DDO3N2,DDO3N3,DDO3N4, &
                 ids,ide, jds,jde, kds,kde,            &
                 ims,ime, jms,jme, kms,kme,            &
                 its,ite, jts,jte, kts,kte             )

         DO 130 K=1,L
          DO3V(I,K)  = DDUO3N(JJROW(I),K) + RSIN1*DDO3N2(JJROW(I),K) &
                      +RCOS1*DDO3N3(JJROW(I),K) &
                      +RCOS2*DDO3N4(JJROW(I),K)
          DO3VP(I,K) = DDUO3N(JJROW(I)+1,K) + RSIN1*DDO3N2(JJROW(I)+1,K) &
                     +RCOS1*DDO3N3(JJROW(I)+1,K) &
                     +RCOS2*DDO3N4(JJROW(I)+1,K)
!...   NOW LATITUDINAL INTERPOLATION, AND
!          CONVERT O3 INTO MASS MIXING RATIO(ORIGINAL DATA MPY BY 1.E4)
          QO3(I,K) = H1M4 * (DO3V(I,K)+TTHAN(I)*(DO3VP(I,K)-DO3V(I,K)))
  130   CONTINUE
  125 CONTINUE
  135 CONTINUE
!.............
      DO 195 I=MYIS,MYIE
!.....     VISIBLE AND NEAR IR DIFFUSE ALBEDO
        ALVD(I) = SFCALB(I)
        ALND(I) = SFCALB(I)
!.....     VISIBLE AND NEAR IR DIRECT BEAM ALBEDO
        ALVB(I) = SFCALB(I)
        ALNB(I) = SFCALB(I)
!.....     VISIBLE AND NEAR IR DIRECT BEAM ALBEDO,IF NOT OCEAN NOR SNOW
!            ..FUNCTION OF COSINE SOLAR ZENITH ANGLE..
        IF (SLMSK(I+IBEG-1).LT.0.5) THEN
         IF (SFCALB(I).LE.0.5) THEN
          ALBD0 = -18.0 * (0.5 - ACOS(COSZEN(I))/PI)
          ALBD0 = EXP (ALBD0)
          ALVD1 = (ALVD(I) - 0.054313) / 0.945687
          ALND1 = (ALND(I) - 0.054313) / 0.945687
          ALVB(I) = ALVD1 + (1.0 - ALVD1) * ALBD0
          ALNB(I) = ALND1 + (1.0 - ALND1) * ALBD0
         END IF
        END IF
  195 CONTINUE
!.....SURFACE VALUES OF RRCL AND TTCL
      DO 200 N=1,2
        DO 200 I=MYIS,MYIE
      RRCL(I,N,1)=ALVD(I)
      TTCL(I,N,1)=ZERO
  200 CONTINUE
      DO 220 N=3,NB
      DO 220 I=MYIS,MYIE
         RRCL(I,N,1)=ALND(I)
         TTCL(I,N,1)=ZERO
  220 CONTINUE
!...     **************************
!...     *  END OF CLOUD SECTION  *
!...     **************************
!... THE FOLLOWING CODE CONVERTS RRVCO2,THE VOLUME MIXING RATIO OF CO2
!   INTO RRCO2,THE MASS MIXING RATIO.
      RRVCO2=RCO2
      RRCO2=RRVCO2*RATCO2MW
  250 IF(ITIMLW .EQ. 0) GO TO 300
!
!             ***********************
!====>        * LONG WAVE RADIATION *
!             ***********************
!
!....     ACCOUNT FOR REDUCED EMISSIVITY OF ANY CLDS
      DO 240 K=1,LP1
      DO 240 I=MYIS,MYIE
        EQCMT(I,K)=CAMT(I,K)*EMCLD(I,K)
  240 CONTINUE
!....    GET CLD FACTOR FOR LW CALCULATIONS
!....

! shuhua

      CALL CLO89(CLDFAC,EQCMT,NCLDS,KBTM,KTOP, &
                 ids,ide, jds,jde, kds,kde,    &
                 ims,ime, jms,jme, kms,kme,    &
                 its,ite, jts,jte, kts,kte     )

! shuhua
!===>        LONG WAVE RADIATION
!     CALL LWR88(HEATRA,GRNFLX,TOPFLX,         &
!                PRESS,TEMP,RH2O,QO3,CLDFAC,   &
!                EQCMT,NCLDS,KTOP,KBTM,        &
!
!!               BO3RND,AO3RND,T1,T2,T4,EM1V,EM1VW,EM3V, &
!                BO3RND,AO3RND, &
!                APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
!                ZERO,ONE,H18E3,P0INV,H6P08108,DIFFCTR,        &
!                GINV,H3M4,BETINW,RATH2OMW,GP0INV,P0,P0XZP8,   &
!                P0XZP2,H3M3,H1M3,H1M2,H25E2,B0,B2,B1,B3,HAF,  &
!                TEN,HP1,FOUR,HM1EZ,SKO3R,                     &
!                AB15WD,SKC1R,RADCON,QUARTR,TWO,               &
!                HM6666M2,HMP66667,HMP5, HP166666,H41666M2,    &
!                RADCON1,H16E1, H28E1,H44194M2,H1P41819,SKO2D, &
!                ids,ide, jds,jde, kds,kde,                    &
!                ims,ime, jms,jme, kms,kme,                    &
!                its,ite, jts,jte, kts,kte                    )

      CALL LWR88(HEATRA,GRNFLX,TOPFLX,         &
                 PRESS,TEMP,RH2O,QO3,CLDFAC,   &
                 EQCMT,NCLDS,KTOP,KBTM,        &
!
!                BO3RND,AO3RND,T1,T2,T4,EM1V,EM1VW,EM3V, &
                 BO3RND,AO3RND, &
                 APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
                 ZERO,ONE,H18E3,P0INV,H6P08108,DIFFCTR,        &
                 GINV,H3M4,BETINW,RATH2OMW,GP0INV,P0,P0XZP8,   &
                 P0XZP2,H3M3,H1M3,H1M2,H25E2,B0,B2,B1,B3,HAF,  &
                 TEN,HP1,FOUR,HM1EZ,                           &
                 RADCON,QUARTR,TWO,                            &
                 HM6666M2,HMP66667,HMP5, HP166666,H41666M2,    &
                 RADCON1,H16E1, H28E1,H44194M2,H1P41819,       &
                 ids,ide, jds,jde, kds,kde,                    &
                 ims,ime, jms,jme, kms,kme,                    &
                  its,ite, jts,jte, kts,kte                    )

!....
      DO 280 I=MYIS,MYIE
        IR = I + IBEG - 1
        FLWUP(IR) = TOPFLX(I) * .001E0
        GRNFLX(I)=Q14330*(HSIGMA*TEMP(I,LP1)**4-GRNFLX(I))
!.... GET LW FLUX DOWN AND UP AT GROUND(WATTS/M**2) - GRNFLX=LW DOWN.
        FLWDNS(IR)=GRNFLX(I)/(1.43306E-06*1000.E0)
        FLWUPS(IR)=HSIGMA*.001E0 * TEMP(I,LP1)**4
  280 CONTINUE
!....      CONVERT HEATING RATES TO DEG/SEC
      DO 290 K=1,L
        DO 290 I=MYIS,MYIE
          HLW(I+IBEG-1,K)=HEATRA(I,K)*DAYSEC
  290 CONTINUE
  300 CONTINUE
      IF(ITIMSW .EQ. 0) GO TO 350
!SW
      CALL SWR93(FSW,HSW,UF,DF,FSWL,HSWL,UFL,DFL, &
                 PRESS,COSZEN,TAUDA,RH2O,RRCO2,SSOLAR,QO3, &
                 NCLDS,KTOP,KBTM,CAMT,RRCL,TTCL, &
                 ALVB,ALNB,ALVD,ALND,GDFVB,GDFNB,GDFVD,GDFND, &
!
!                UCO2,UO3,TUCO2,TUO3,TDO3,TDCO2,                &
                 ABCFF,PWTS,                                    &
                 H35E1,H1224E3,ONE,ZERO,HAF,H69766E5,HP219,     &
                 HP816,RRAYAV,GINV,CFCO2,CFO3,                  &
                 TWO,H235M3,HP26,H129M2,H75826M4,H1036E2,       &
                 H1P082,HMP805,H1386E2,H658M2,H2118M2,H42M2,    &
                 H323M4,HM1EZ,DIFFCTR,O3DIFCTR,FIFTY,RADCON,    &
                 ids,ide, jds,jde, kds,kde,                     &
                 ims,ime, jms,jme, kms,kme,                     &
                 its,ite, jts,jte, kts,kte                      )

!SW
!
!.....    GET SW FLUXES IN WATTS/M**2
      DO 320 I=MYIS,MYIE
       IR = I + IBEG - 1
       FSWUP(IR) = UF(I,1) * 1.E-3
       FSWDN(IR) = DF(I,1) * 1.E-3
       FSWUPS(IR) = UF(I,LP1) * 1.E-3
!C..COUPLE W/M2 DIFF, IF FSWDNS(IR)=DF(I,LP1)*1.#E-3
       FSWDNS(IR) = (GDFVB(I)+GDFNB(I)+GDFVD(I)+GDFND(I)) * 1.E-3
!...    DOWNWARD SFC FLUX FOR THE SIB PARAMETERATION
!.....     VISIBLE AND NEAR IR DIFFUSE
       GDFVDR(IR) = GDFVD(I) * 1.E-3
       GDFNDR(IR) = GDFND(I) * 1.E-3
!.....     VISIBLE AND NEAR IR DIRECT BEAM
       GDFVBR(IR) = GDFVB(I) * 1.E-3
       GDFNBR(IR) = GDFNB(I) * 1.E-3
  320 CONTINUE
!....      CONVERT HEATING RATES TO DEG/SEC
      DO 330 K=1,L
        DO 330 I=MYIS,MYIE
          SWH(I+IBEG-1,K)=HSW(I,K)*DAYSEC
  330 CONTINUE
  350 CONTINUE
      RETURN
 1000 FORMAT(1H ,' YOU ARE CALLING GFDL RADIATION CODE FOR',I5,' PTS', &
                 'AND',I4,' LYRS,WITH KDAPRX,KO3,KCZ,KEMIS,KALB = ',5I2)
 
  END SUBROUTINE RADFS 

!-----------------------------------------------------------------------
    SUBROUTINE O3CLIM
!                (XDUO3N,XDO3N2,XDO3N3,XDO3N4,PRGFDL,         &
!                ids,ide, jds,jde, kds,kde,                   &
!                ims,ime, jms,jme, kms,kme,                   &
!                its,ite, jts,jte, kts,kte                    )
!----------------------------------------------------------------------
 IMPLICIT NONE
!----------------------------------------------------------------------
!     INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
!                                   ims,ime, jms,jme, kms,kme ,      &
!                                   its,ite, jts,jte, kts,kte

!     ******************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    O3CLIM      GENERATE SEASONAL OZONE DISTRIBUTION
!   PRGRMMR: GFDL/CAMPANA    ORG: W/NP22     DATE: ??-??-??
!     
! ABSTRACT:
!     O3CLIM COMPUTES THE SEASONAL CLIMATOLOGY OF OZONE USING
!     81-LAYER DATA FROM GFDL.
!     
! PROGRAM HISTORY LOG:
!   ??-??-??  GFDL/KC    - ORIGINATOR
!   96-07-26  BLACK      - MODIFIED FOR ETA MODEL
!     
! USAGE: CALL O3CLIM FROM SUBROUTINE RADTN
!   INPUT ARGUMENT LIST:
!     NONE     
!  
!   OUTPUT ARGUMENT LIST: 
!     NONE
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!  
!     UNIQUE:
!        NONE
!  
!     LIBRARY:
!        NONE
!  
!   COMMON BLOCKS: SEASO3
!                  O3DATA
!   
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$  
!----------------------------------------------------------------------
!      INTEGER   :: NL,NLP1,NLGTH,NKK,NK,NKP
       INTEGER, PARAMETER :: NL=81,NLP1=NL+1,NLGTH=37*NL,NKK=41,NK=81,NKP=NK+1
!----------------------------------------------------------------------
!     INCLUDE "SEASO3.comm"
!---------------------------------------------------------------------
!     REAL, INTENT(OUT), DIMENSION(37,NL) :: XDUO3N,XDO3N2,XDO3N3,XDO3N4
!     REAL, INTENT(OUT), DIMENSION(NL)    :: PRGFDL

!      COMMON /SEASO3/
!      ...WINTER....  ...SPRING....  ...SUMMER....  ....FALL.....
!    & XDUO3N(37,NL), XDO3N2(37,NL), XDO3N3(37,NL), XDO3N4(37,NL)
!
!    &,PRGFDL(NL)
!---------------------------------------------------------------------
       REAL :: PH1(45),PH2(37),P1(48),P2(33),O3HI1(10,16),O3HI2(10,9) &
              ,O3LO1(10,16),O3LO2(10,16),O3LO3(10,16),O3LO4(10,16)
!----------------------------------------------------------------------
       REAL    :: AVG,A1,B1,B2
       INTEGER :: K,N,NCASE,IPLACE,KK,NKM,NKMM,KI,KQ,JJ,KEN,I,iindex,jindex
!----------------------------------------------------------------------
       REAL :: PSTD(NL),TEMPN(19),O3O3(37,NL,4),O35DEG(37,NL) &
      ,XRAD1(NLGTH),XRAD2(NLGTH),XRAD3(NLGTH),XRAD4(NLGTH) &
      ,DDUO3N(19,NL),DUO3N(19,41) &
      ,RO3(10,41),RO3M(10,40),RO31(10,41),RO32(10,41) &
      ,O3HI(10,25) &
      ,RSTD(81),RBAR(NL),RDATA(81) &
      ,PHALF(NL),P(81),PH(82)

       REAL   :: PXX(81),PYY(82)                       !  fix for nesting: gopal's doing

!----------------------------------------------------------------------
!                           EQUIVALENCE &
!       (O3HI1(1,1),O3HI(1,1)),(O3HI2(1,1),O3HI(1,17)) &
!      ,(PH1(1),PH(1)),(PH2(1),PH(46)) &
!      ,(P1(1),P(1)),(P2(1),P(49))

                           EQUIVALENCE &                 
       (O3HI1(1,1),O3HI(1,1)),(O3HI2(1,1),O3HI(1,17)) & 
      ,(PH1(1),PYY(1)),(PH2(1),PYY(46)) &               ! fix for nesting: gopal's doing 
      ,(P1(1),PXX(1)),(P2(1),PXX(49))                   ! fix for nesting: gopal's doing 


!----------------------------------------------------------------------
!                          EQUIVALENCE &
!      (XRAD1(1),XDUO3N(1,1),O3O3(1,1,1)) &
!     ,(XRAD2(1),XDO3N2(1,1)) &
!     ,(XRAD3(1),XDO3N3(1,1)),(XRAD4(1),XDO3N4(1,1),)
                           EQUIVALENCE &
       (XRAD1(1),O3O3(1,1,1)) &
      ,(XRAD2(1),O3O3(1,1,2)) &
      ,(XRAD3(1),O3O3(1,1,3)),(XRAD4(1),O3O3(1,1,4))
!----------------------------------------------------------------------
!---------------------------------------------------------------------
      DATA PH1/      0.,     &
           0.1027246E-04, 0.1239831E-04, 0.1491845E-04, 0.1788053E-04,     &
           0.2135032E-04, 0.2540162E-04, 0.3011718E-04, 0.3558949E-04,     &
           0.4192172E-04, 0.4922875E-04, 0.5763817E-04, 0.6729146E-04,     &
           0.7834518E-04, 0.9097232E-04, 0.1053635E-03, 0.1217288E-03,     &
           0.1402989E-03, 0.1613270E-03, 0.1850904E-03, 0.2119495E-03,     &
           0.2423836E-03, 0.2768980E-03, 0.3160017E-03, 0.3602623E-03,     &
           0.4103126E-03, 0.4668569E-03, 0.5306792E-03, 0.6026516E-03,     &
           0.6839018E-03, 0.7759249E-03, 0.8803303E-03, 0.9987843E-03,     &
           0.1133178E-02, 0.1285955E-02, 0.1460360E-02, 0.1660001E-02,     &
           0.1888764E-02, 0.2151165E-02, 0.2452466E-02, 0.2798806E-02,     &
           0.3197345E-02, 0.3656456E-02, 0.4185934E-02, 0.4797257E-02/     
      DATA PH2/     &
           0.5503893E-02, 0.6321654E-02, 0.7269144E-02, 0.8368272E-02,     &
           0.9644873E-02, 0.1112946E-01, 0.1285810E-01, 0.1487354E-01,     &
           0.1722643E-01, 0.1997696E-01, 0.2319670E-01, 0.2697093E-01,     &
           0.3140135E-01, 0.3660952E-01, 0.4274090E-01, 0.4996992E-01,     &
           0.5848471E-01, 0.6847525E-01, 0.8017242E-01, 0.9386772E-01,     &
           0.1099026E+00, 0.1286765E+00, 0.1506574E+00, 0.1763932E+00,     &
           0.2065253E+00, 0.2415209E+00, 0.2814823E+00, 0.3266369E+00,     &
           0.3774861E+00, 0.4345638E+00, 0.4984375E+00, 0.5697097E+00,     &
           0.6490189E+00, 0.7370409E+00, 0.8344896E+00, 0.9421190E+00,     &
           0.1000000E+01/     
      DATA P1/     &
           0.9300000E-05, 0.1129521E-04, 0.1360915E-04, 0.1635370E-04,     &
           0.1954990E-04, 0.2331653E-04, 0.2767314E-04, 0.3277707E-04,     &
           0.3864321E-04, 0.4547839E-04, 0.5328839E-04, 0.6234301E-04,     &
           0.7263268E-04, 0.8450696E-04, 0.9793231E-04, 0.1133587E-03,     &
           0.1307170E-03, 0.1505832E-03, 0.1728373E-03, 0.1982122E-03,     &
           0.2266389E-03, 0.2592220E-03, 0.2957792E-03, 0.3376068E-03,     &
           0.3844381E-03, 0.4379281E-03, 0.4976965E-03, 0.5658476E-03,     &
           0.6418494E-03, 0.7287094E-03, 0.8261995E-03, 0.9380076E-03,     &
           0.1063498E-02, 0.1207423E-02, 0.1369594E-02, 0.1557141E-02,     &
           0.1769657E-02, 0.2015887E-02, 0.2295520E-02, 0.2620143E-02,     &
           0.2989651E-02, 0.3419469E-02, 0.3909867E-02, 0.4481491E-02,     &
           0.5135272E-02, 0.5898971E-02, 0.6774619E-02, 0.7799763E-02/     
      DATA P2/     &
           0.8978218E-02, 0.1036103E-01, 0.1195488E-01, 0.1382957E-01,     &
           0.1599631E-01, 0.1855114E-01, 0.2151235E-01, 0.2501293E-01,     &
           0.2908220E-01, 0.3390544E-01, 0.3952926E-01, 0.4621349E-01,     &
           0.5403168E-01, 0.6330472E-01, 0.7406807E-01, 0.8677983E-01,     &
           0.1015345E+00, 0.1189603E+00, 0.1391863E+00, 0.1630739E+00,     &
           0.1908004E+00, 0.2235461E+00, 0.2609410E+00, 0.3036404E+00,     &
           0.3513750E+00, 0.4055375E+00, 0.4656677E+00, 0.5335132E+00,     &
           0.6083618E+00, 0.6923932E+00, 0.7845676E+00, 0.8875882E+00,     &
           0.1000000E+01/     
      DATA O3HI1/     &
       .55,.50,.45,.45,.40,.35,.35,.30,.30,.30,     &
       .55,.51,.46,.47,.42,.38,.37,.36,.35,.35,     &
       .55,.53,.48,.49,.44,.42,.41,.40,.38,.38,     &
       .60,.55,.52,.52,.50,.47,.46,.44,.42,.41,     &
       .65,.60,.55,.56,.53,.52,.50,.48,.45,.45,     &
       .75,.65,.60,.60,.55,.55,.55,.50,.48,.47,     &
       .80,.75,.75,.75,.70,.70,.65,.63,.60,.60,     &
       .90,.85,.85,.80,.80,.75,.75,.74,.72,.71,     &
       1.10,1.05,1.00,.90,.90,.90,.85,.83,.80,.80,        &
       1.40,1.30,1.25,1.25,1.25,1.20,1.15,1.10,1.05,1.00, &
       1.7,1.7,1.6,1.6,1.6,1.6,1.6,1.6,1.5,1.5,     &
       2.1,2.0,1.9,1.9,1.9,1.8,1.8,1.8,1.7,1.7,     &
       2.4,2.3,2.2,2.2,2.2,2.1,2.1,2.1,2.0,2.0,     &
       2.7,2.5,2.5,2.5,2.5,2.5,2.4,2.4,2.3,2.3,     &
       2.9,2.8,2.7,2.7,2.7,2.7,2.7,2.7,2.6,2.6,     &
       3.1,3.1,3.0,3.0,3.0,3.0,3.0,3.0,2.9,2.8/     
      DATA O3HI2/     &
       3.3,3.4,3.4,3.6,3.7,3.9,4.0,4.1,4.0,3.8,     &
       3.6,3.8,3.9,4.2,4.7,5.3,5.6,5.7,5.5,5.2,     &
       4.1,4.3,4.7,5.2,6.0,6.7,7.0,6.8,6.4,6.2,     &
       5.4,5.7,6.0,6.6,7.3,8.0,8.4,7.7,7.1,6.7,     &
       6.7,6.8,7.0,7.6,8.3,10.0,9.6,8.2,7.5,7.2,     &
       9.2,9.3,9.4,9.6,10.3,10.6,10.0,8.5,7.7,7.3,     &
       12.6,12.1,12.0,12.1,11.7,11.0,10.0,8.6,7.8,7.4, &
       14.2,13.5,13.1,12.8,11.9,10.9,9.8,8.5,7.8,7.5,  &
       14.3,14.0,13.4,12.7,11.6,10.6,9.3,8.4,7.6,7.3/     
      DATA O3LO1/     &
       14.9,14.2,13.3,12.5,11.2,10.3,9.5,8.6,7.5,7.4,  &
       14.5,14.1,13.0,11.8,10.5,9.8,9.2,7.9,7.4,7.4,   &
       11.8,11.5,10.9,10.5,9.9,9.6,8.9,7.5,7.2,7.2,    &
       7.3,7.7,7.8,8.4,8.4,8.5,7.9,7.4,7.1,7.1,     &
       4.1,4.4,5.3,6.6,6.9,7.5,7.4,7.2,7.0,6.9,     &
       1.8,1.9,2.5,3.3,4.5,5.8,6.3,6.3,6.4,6.1,     &
       0.4,0.5,0.8,1.2,2.7,3.6,4.6,4.7,5.0,5.2,     &
       .10,.15,.20,.50,1.4,2.1,3.0,3.2,3.5,3.9,     &
       .07,.10,.12,.30,1.0,1.4,1.8,1.9,2.3,2.5,     &
       .06,.08,.10,.15,.60,.80,1.4,1.5,1.5,1.6,     &
       .05,.05,.06,.09,.20,.40,.70,.80,.90,.90,     &
       .05,.05,.06,.08,.10,.13,.20,.25,.30,.40,     &
       .05,.05,.05,.06,.07,.07,.08,.09,.10,.13,     &
       .05,.05,.05,.05,.06,.06,.06,.06,.07,.07,     &
       .05,.05,.05,.05,.05,.05,.05,.06,.06,.06,     &
       .04,.04,.04,.04,.04,.04,.04,.05,.05,.05/     
      DATA O3LO2/     &
       14.8,14.2,13.8,12.2,11.0,9.8,8.5,7.8,7.4,6.9,   &
       13.2,13.0,12.5,11.3,10.4,9.0,7.8,7.5,7.0,6.6,   &
       10.6,10.6,10.7,10.1,9.4,8.6,7.5,7.0,6.5,6.1,    &
       7.0,7.3,7.5,7.5,7.5,7.3,6.7,6.4,6.0,5.8,     &
       3.8,4.0,4.7,5.0,5.2,5.9,5.8,5.6,5.5,5.5,     &
       1.4,1.6,2.4,3.0,3.7,4.1,4.6,4.8,5.1,5.0,     &
       .40,.50,.90,1.2,2.0,2.7,3.2,3.6,4.3,4.1,     &
       .07,.10,.20,.30,.80,1.4,2.1,2.4,2.7,3.0,     &
       .06,.07,.09,.15,.30,.70,1.2,1.4,1.6,2.0,     &
       .05,.05,.06,.12,.15,.30,.60,.70,.80,.80,     &
       .04,.05,.06,.08,.09,.15,.30,.40,.40,.40,     &
       .04,.04,.05,.055,.06,.09,.12,.13,.15,.15,    &
       .03,.03,.045,.052,.055,.06,.07,.07,.06,.07,  &
       .03,.03,.04,.051,.052,.052,.06,.06,.05,.05,  &
       .02,.02,.03,.05,.05,.05,.04,.04,.04,.04,     &
       .02,.02,.02,.04,.04,.04,.03,.03,.03,.03/     
      DATA O3LO3/     &
       14.5,14.0,13.5,11.3,11.0,10.0,9.0,8.3,7.5,7.3,    &
       13.5,13.2,12.5,11.1,10.4,9.7,8.2,7.8,7.4,6.8,     &
       10.8,10.9,11.0,10.4,10.0,9.6,7.9,7.5,7.0,6.7,     &
       7.3,7.5,7.8,8.5,9.0,8.5,7.7,7.4,6.9,6.5,     &
       4.1,4.5,5.3,6.2,7.3,7.7,7.3,7.0,6.6,6.4,     &
       1.8,2.0,2.2,3.8,4.3,5.6,6.2,6.2,6.4,6.2,     &
       .30,.50,.60,1.5,2.8,3.7,4.5,4.7,5.5,5.6,     &
       .09,.10,.15,.60,1.2,2.1,3.0,3.5,4.0,4.3,     &
       .06,.08,.10,.30,.60,1.1,1.9,2.2,2.9,3.0,     &
       .04,.05,.06,.15,.45,.60,1.1,1.3,1.6,1.8,     &
       .04,.04,.04,.08,.20,.30,.55,.60,.75,.90,     &
       .04,.04,.04,.05,.06,.10,.12,.15,.20,.25,     &
       .04,.04,.03,.04,.05,.06,.07,.07,.07,.08,     &
       .03,.03,.04,.05,.05,.05,.05,.05,.05,.05,     &
       .03,.03,.03,.04,.04,.04,.05,.05,.04,.04,     &
       .02,.02,.02,.04,.04,.04,.04,.04,.03,.03/      
      DATA O3LO4/     &
       14.2,13.8,13.2,12.5,11.7,10.5,8.6,7.8,7.5,6.6,  &
       12.5,12.4,12.2,11.7,10.8,9.8,7.8,7.2,6.5,6.1,   &
       10.6,10.5,10.4,10.1,9.6,9.0,7.1,6.8,6.1,5.9,    &
       7.0,7.4,7.9,7.8,7.6,7.3,6.2,6.1,5.8,5.6,     &
       4.2,4.6,5.1,5.6,5.9,5.9,5.9,5.8,5.6,5.3,     &
       2.1,2.3,2.6,2.9,3.5,4.3,4.8,4.9,5.1,5.1,     &
       0.7,0.8,1.0,1.5,2.0,2.8,3.5,3.6,3.7,4.0,     &
       .15,.20,.40,.50,.60,1.4,2.1,2.2,2.3,2.5,     &
       .08,.10,.15,.25,.30,.90,1.2,1.3,1.4,1.6,     &
       .07,.08,.10,.14,.20,.50,.70,.90,.90,.80,     &
       .05,.06,.08,.12,.14,.20,.35,.40,.60,.50,     &
       .05,.05,.08,.09,.09,.09,.11,.12,.15,.18,     &
       .04,.05,.06,.07,.07,.08,.08,.08,.08,.08,     &
       .04,.04,.05,.07,.07,.07,.07,.07,.06,.05,     &
       .02,.02,.04,.05,.05,.05,.05,.05,.04,.04,     &
       .02,.02,.03,.04,.04,.04,.04,.04,.03,.03/     
!----------------------------------------------------------------------
!***
!***  COMPUTE DETAILED O3 PROFILE FROM THE ORIGINAL GFDL PRESSURES
!***  WHERE OUTPUT FROM O3INT (PSTD) IS TOP DOWN IN MB*1.E3
!***  AND PSFC=1013.25 MB    ......K.A.C. DEC94
!***

      DO K=1,NK
        PH(K)=PYY(K)*1013250.         ! fix for nesting: gopal's doing 
        P(K)=PXX(K)*1013250.
      ENDDO
!
      PH(NKP)=PYY(NKP)*1013250.        ! fixed; dusan
!
      DO K=1,NL
        PSTD(K)=P(K)
      ENDDO
!
      DO K=1,25
      DO N=1,10
        RO31(N,K)=O3HI(N,K)
        RO32(N,K)=O3HI(N,K)
      ENDDO
      ENDDO
!----------------------------------------------------------------------
      DO 100 NCASE=1,4
!
!***  NCASE=1: SPRING (IN N.H.)
!***  NCASE=2: FALL   (IN N.H.)
!***  NCASE=3: WINTER (IN N.H.)
!***  NCASE=4: SUMMER (IN N.H.)
!
      IPLACE=2
      IF(NCASE.EQ.2)IPLACE=4
      IF(NCASE.EQ.3)IPLACE=1
      IF(NCASE.EQ.4)IPLACE=3
!
      IF(NCASE.EQ.1.OR.NCASE.EQ.2)THEN
        DO K=26,41
        DO N=1,10
          RO31(N,K)=O3LO1(N,K-25)
          RO32(N,K)=O3LO2(N,K-25)
        ENDDO
        ENDDO
      ENDIF
!
      IF(NCASE.EQ.3.OR.NCASE.EQ.4)THEN
        DO K=26,41
        DO N=1,10
          RO31(N,K)=O3LO3(N,K-25)
          RO32(N,K)=O3LO4(N,K-25)
        ENDDO
        ENDDO
      ENDIF
!
      DO 25 KK=1,NKK
      DO N=1,10
        DUO3N(N,KK)=RO31(11-N,KK)
        DUO3N(N+9,KK)=RO32(N,KK)
      ENDDO
      DUO3N(10,KK)=0.5*(RO31(1,KK)+RO32(1,KK))
   25 CONTINUE
!
!***FOR NCASE=2 OR NCASE=4,REVERSE LATITUDE ARRANGEMENT OF CORR. SEASON
!
      IF(NCASE.EQ.2.OR.NCASE.EQ.4)THEN
        DO 50 KK=1,NKK
        DO N=1,19
          TEMPN(N)=DUO3N(20-N,KK)
        ENDDO
         DO N=1,19
           DUO3N(N,KK)=TEMPN(N)
         ENDDO
   50   CONTINUE
      ENDIF
!
!***  DUO3N NOW IS O3 PROFILE FOR APPROPRIATE SEASON AT STD PRESSURE
!***  LEVELS
!
!***  BEGIN LATITUDE (10 DEG) LOOP
!
      DO 75 N=1,19
!
      DO KK=1,NKK
        RSTD(KK)=DUO3N(N,KK)
      ENDDO
!
      NKM=NK-1
      NKMM=NK-3
!***
!***  BESSELS HALF-POINT INTERPOLATION FORMULA
!***
      DO K=4,NKMM,2
        KI=K/2
        RDATA(K)=0.5*(RSTD(KI)+RSTD(KI+1))-(RSTD(KI+2)-RSTD(KI+1) &
                                           -RSTD(KI)+RSTD(KI-1))/16.
      ENDDO
!
      RDATA(2)=0.5*(RSTD(2)+RSTD(1))
      RDATA(NKM)=0.5*(RSTD(NKK)+RSTD(NKK-1))
!
!***  PUT UNCHANGED DATA INTO NEW ARRAY
!
      DO K=1,NK,2
        KQ=(K+1)/2
        RDATA(K)=RSTD(KQ)
      ENDDO
!
      DO KK=1,NL
        DDUO3N(N,KK)=RDATA(KK)*.01
      ENDDO
!
   75 CONTINUE
!
!***  END OF LATITUDE LOOP
!
!----------------------------------------------------------------------
!***
!***  CREATE 5 DEG OZONE QUANTITIES BY LINEAR INTERPOLATION OF
!***  10 DEG VALUES
!***
      DO 90 KK=1,NL
!
      DO N=1,19
        O35DEG(2*N-1,KK)=DDUO3N(N,KK)
      ENDDO
!
      DO N=1,18
        O35DEG(2*N,KK)=0.5*(DDUO3N(N,KK)+DDUO3N(N+1,KK))
      ENDDO
!
   90 CONTINUE
!
      DO JJ=1,37
      DO KEN=1,NL
        O3O3(JJ,KEN,IPLACE)=O35DEG(JJ,KEN)
      ENDDO
      ENDDO
!
  100 CONTINUE
!----------------------------------------------------------------------
!***  END OF LOOP OVER CASES
!----------------------------------------------------------------------
!***
!***  AVERAGE CLIMATOLOGICAL VALUS OF O3 FROM 5 DEG LAT MEANS, SO THAT
!***  TIME AND SPACE INTERPOLATION WILL WORK (SEE SUBR OZON2D)
!***
      DO I=1,NLGTH
        AVG=0.25*(XRAD1(I)+XRAD2(I)+XRAD3(I)+XRAD4(I))
        A1=0.5*(XRAD2(I)-XRAD4(I))
        B1=0.5*(XRAD1(I)-XRAD3(I))
        B2=0.25*((XRAD1(I)+XRAD3(I))-(XRAD2(I)+XRAD4(I)))

!       XRAD1(I)=AVG
!       XRAD2(I)=A1
!       XRAD3(I)=B1
!       XRAD4(I)=B2

        iindex = 1+mod((I-1),37)
        jindex = 1+(I-1)/37
        XDUO3N(iindex,jindex)=AVG
        XDO3N2(iindex,jindex)=A1
        XDO3N3(iindex,jindex)=B1
        XDO3N4(iindex,jindex)=B2
      ENDDO
!***
!***  CONVERT GFDL PRESSURE (MICROBARS) TO PA 
!***
      DO N=1,NL
        PRGFDL(N)=PSTD(N)*1.E-1
      ENDDO
!
    END SUBROUTINE O3CLIM

!---------------------------------------------------------------------
      SUBROUTINE TABLE 
!                     (TABLE1,TABLE2,TABLE3,EM1,EM1WDE,EM3,          &
!                      SOURCE,DSRCE                                  )
!---------------------------------------------------------------------
 IMPLICIT NONE
!----------------------------------------------------------------------

!INTEGER, PARAMETER :: NBLY=15
 INTEGER, PARAMETER :: NB=12
 INTEGER, PARAMETER :: NBLX=47
 INTEGER , PARAMETER:: NBLW = 163

 REAL,PARAMETER ::      AMOLWT=28.9644
 REAL,PARAMETER ::      CSUBP=1.00484E7
 REAL,PARAMETER ::      DIFFCTR=1.66
 REAL,PARAMETER ::      G=980.665
 REAL,PARAMETER ::      GINV=1./G
 REAL,PARAMETER ::      GRAVDR=980.0
 REAL,PARAMETER ::      O3DIFCTR=1.90
 REAL,PARAMETER ::      P0=1013250.
 REAL,PARAMETER ::      P0INV=1./P0
 REAL,PARAMETER ::      GP0INV=GINV*P0INV
 REAL,PARAMETER ::      P0XZP2=202649.902
 REAL,PARAMETER ::      P0XZP8=810600.098
 REAL,PARAMETER ::      P0X2=2.*1013250.
 REAL,PARAMETER ::      RADCON=8.427
 REAL,PARAMETER ::      RADCON1=1./8.427
 REAL,PARAMETER ::      RATCO2MW=1.519449738
 REAL,PARAMETER ::      RATH2OMW=.622
 REAL,PARAMETER ::      RGAS=8.3142E7
 REAL,PARAMETER ::      RGASSP=8.31432E7
 REAL,PARAMETER ::      SECPDA=8.64E4
!
!******THE FOLLOWING ARE MATHEMATICAL CONSTANTS*******
!        ARRANGED IN DECREASING ORDER
 REAL,PARAMETER ::      HUNDRED=100.
 REAL,PARAMETER ::      HNINETY=90.
 REAL,PARAMETER ::      HNINE=9.0
 REAL,PARAMETER ::      SIXTY=60.
 REAL,PARAMETER ::      FIFTY=50.
 REAL,PARAMETER ::      TEN=10.
 REAL,PARAMETER ::      EIGHT=8.
 REAL,PARAMETER ::      FIVE=5.
 REAL,PARAMETER ::      FOUR=4.
 REAL,PARAMETER ::      THREE=3.
 REAL,PARAMETER ::      TWO=2.
 REAL,PARAMETER ::      ONE=1.
 REAL,PARAMETER ::      HAF=0.5
 REAL,PARAMETER ::      QUARTR=0.25
 REAL,PARAMETER ::      ZERO=0.
!
!******FOLLOWING ARE POSITIVE FLOATING POINT CONSTANTS(H'S)
!       ARRANGED IN DECREASING ORDER
 REAL,PARAMETER ::      H83E26=8.3E26
 REAL,PARAMETER ::      H71E26=7.1E26
 REAL,PARAMETER ::      H1E15=1.E15
 REAL,PARAMETER ::      H1E13=1.E13
 REAL,PARAMETER ::      H1E11=1.E11
 REAL,PARAMETER ::      H1E8=1.E8
 REAL,PARAMETER ::      H2E6=2.0E6
 REAL,PARAMETER ::      H1E6=1.0E6
 REAL,PARAMETER ::      H69766E5=6.97667E5
 REAL,PARAMETER ::      H4E5=4.E5
 REAL,PARAMETER ::      H165E5=1.65E5
 REAL,PARAMETER ::      H5725E4=57250.
 REAL,PARAMETER ::      H488E4=48800.
 REAL,PARAMETER ::      H1E4=1.E4
 REAL,PARAMETER ::      H24E3=2400.
 REAL,PARAMETER ::      H20788E3=2078.8
 REAL,PARAMETER ::      H2075E3=2075.
 REAL,PARAMETER ::      H18E3=1800.
 REAL,PARAMETER ::      H1224E3=1224.
 REAL,PARAMETER ::      H67390E2=673.9057
 REAL,PARAMETER ::      H5E2=500.
 REAL,PARAMETER ::      H3082E2=308.2
 REAL,PARAMETER ::      H3E2=300.
 REAL,PARAMETER ::      H2945E2=294.5
 REAL,PARAMETER ::      H29316E2=293.16
 REAL,PARAMETER ::      H26E2=260.0
 REAL,PARAMETER ::      H25E2=250.
 REAL,PARAMETER ::      H23E2=230.
 REAL,PARAMETER ::      H2E2=200.0
 REAL,PARAMETER ::      H15E2=150.
 REAL,PARAMETER ::      H1386E2=138.6
 REAL,PARAMETER ::      H1036E2=103.6
 REAL,PARAMETER ::      H8121E1=81.21
 REAL,PARAMETER ::      H35E1=35.
 REAL,PARAMETER ::      H3116E1=31.16
 REAL,PARAMETER ::      H28E1=28.
 REAL,PARAMETER ::      H181E1=18.1
 REAL,PARAMETER ::      H18E1=18.
 REAL,PARAMETER ::      H161E1=16.1
 REAL,PARAMETER ::      H16E1=16.
 REAL,PARAMETER ::      H1226E1=12.26
 REAL,PARAMETER ::      H9P94=9.94
 REAL,PARAMETER ::      H6P08108=6.081081081
 REAL,PARAMETER ::      H3P6=3.6
 REAL,PARAMETER ::      H3P5=3.5
 REAL,PARAMETER ::      H2P9=2.9
 REAL,PARAMETER ::      H2P8=2.8
 REAL,PARAMETER ::      H2P5=2.5
 REAL,PARAMETER ::      H1P8=1.8
 REAL,PARAMETER ::      H1P4387=1.4387
 REAL,PARAMETER ::      H1P41819=1.418191
 REAL,PARAMETER ::      H1P4=1.4
 REAL,PARAMETER ::      H1P25892=1.258925411
 REAL,PARAMETER ::      H1P082=1.082
 REAL,PARAMETER ::      HP816=0.816
 REAL,PARAMETER ::      HP805=0.805
 REAL,PARAMETER ::      HP8=0.8
 REAL,PARAMETER ::      HP60241=0.60241
 REAL,PARAMETER ::      HP602409=0.60240964
 REAL,PARAMETER ::      HP6=0.6
 REAL,PARAMETER ::      HP526315=0.52631579
 REAL,PARAMETER ::      HP518=0.518
 REAL,PARAMETER ::      HP5048=0.5048
 REAL,PARAMETER ::      HP3795=0.3795
 REAL,PARAMETER ::      HP369=0.369
 REAL,PARAMETER ::      HP26=0.26
 REAL,PARAMETER ::      HP228=0.228
 REAL,PARAMETER ::      HP219=0.219
 REAL,PARAMETER ::      HP166666=.166666
 REAL,PARAMETER ::      HP144=0.144
 REAL,PARAMETER ::      HP118666=0.118666192
 REAL,PARAMETER ::      HP1=0.1
!        (NEGATIVE EXPONENTIALS BEGIN HERE)
 REAL,PARAMETER ::      H658M2=0.0658
 REAL,PARAMETER ::      H625M2=0.0625
 REAL,PARAMETER ::      H44871M2=4.4871E-2
 REAL,PARAMETER ::      H44194M2=.044194
 REAL,PARAMETER ::      H42M2=0.042
 REAL,PARAMETER ::      H41666M2=0.0416666
 REAL,PARAMETER ::      H28571M2=.02857142857
 REAL,PARAMETER ::      H2118M2=0.02118
 REAL,PARAMETER ::      H129M2=0.0129
 REAL,PARAMETER ::      H1M2=.01
 REAL,PARAMETER ::      H559M3=5.59E-3
 REAL,PARAMETER ::      H3M3=0.003
 REAL,PARAMETER ::      H235M3=2.35E-3
 REAL,PARAMETER ::      H1M3=1.0E-3
 REAL,PARAMETER ::      H987M4=9.87E-4
 REAL,PARAMETER ::      H323M4=0.000323
 REAL,PARAMETER ::      H3M4=0.0003
 REAL,PARAMETER ::      H285M4=2.85E-4
 REAL,PARAMETER ::      H1M4=0.0001
 REAL,PARAMETER ::      H75826M4=7.58265E-4
 REAL,PARAMETER ::      H6938M5=6.938E-5
 REAL,PARAMETER ::      H394M5=3.94E-5
 REAL,PARAMETER ::      H37412M5=3.7412E-5
 REAL,PARAMETER ::      H15M5=1.5E-5
 REAL,PARAMETER ::      H1439M5=1.439E-5
 REAL,PARAMETER ::      H128M5=1.28E-5
 REAL,PARAMETER ::      H102M5=1.02E-5
 REAL,PARAMETER ::      H1M5=1.0E-5
 REAL,PARAMETER ::      H7M6=7.E-6
 REAL,PARAMETER ::      H4999M6=4.999E-6
 REAL,PARAMETER ::      H451M6=4.51E-6
 REAL,PARAMETER ::      H25452M6=2.5452E-6
 REAL,PARAMETER ::      H1M6=1.E-6
 REAL,PARAMETER ::      H391M7=3.91E-7
 REAL,PARAMETER ::      H1174M7=1.174E-7
 REAL,PARAMETER ::      H8725M8=8.725E-8
 REAL,PARAMETER ::      H327M8=3.27E-8
 REAL,PARAMETER ::      H257M8=2.57E-8
 REAL,PARAMETER ::      H1M8=1.0E-8
 REAL,PARAMETER ::      H23M10=2.3E-10
 REAL,PARAMETER ::      H14M10=1.4E-10
 REAL,PARAMETER ::      H11M10=1.1E-10
 REAL,PARAMETER ::      H1M10=1.E-10
 REAL,PARAMETER ::      H83M11=8.3E-11
 REAL,PARAMETER ::      H82M11=8.2E-11
 REAL,PARAMETER ::      H8M11=8.E-11
 REAL,PARAMETER ::      H77M11=7.7E-11
 REAL,PARAMETER ::      H72M11=7.2E-11
 REAL,PARAMETER ::      H53M11=5.3E-11
 REAL,PARAMETER ::      H48M11=4.8E-11
 REAL,PARAMETER ::      H44M11=4.4E-11
 REAL,PARAMETER ::      H42M11=4.2E-11
 REAL,PARAMETER ::      H37M11=3.7E-11
 REAL,PARAMETER ::      H35M11=3.5E-11
 REAL,PARAMETER ::      H32M11=3.2E-11
 REAL,PARAMETER ::      H3M11=3.0E-11
 REAL,PARAMETER ::      H28M11=2.8E-11
 REAL,PARAMETER ::      H24M11=2.4E-11
 REAL,PARAMETER ::      H23M11=2.3E-11
 REAL,PARAMETER ::      H2M11=2.E-11
 REAL,PARAMETER ::      H18M11=1.8E-11
 REAL,PARAMETER ::      H15M11=1.5E-11
 REAL,PARAMETER ::      H14M11=1.4E-11
 REAL,PARAMETER ::      H114M11=1.14E-11
 REAL,PARAMETER ::      H11M11=1.1E-11
 REAL,PARAMETER ::      H1M11=1.E-11
 REAL,PARAMETER ::      H96M12=9.6E-12
 REAL,PARAMETER ::      H93M12=9.3E-12
 REAL,PARAMETER ::      H77M12=7.7E-12
 REAL,PARAMETER ::      H74M12=7.4E-12
 REAL,PARAMETER ::      H65M12=6.5E-12
 REAL,PARAMETER ::      H62M12=6.2E-12
 REAL,PARAMETER ::      H6M12=6.E-12
 REAL,PARAMETER ::      H45M12=4.5E-12
 REAL,PARAMETER ::      H44M12=4.4E-12
 REAL,PARAMETER ::      H4M12=4.E-12
 REAL,PARAMETER ::      H38M12=3.8E-12
 REAL,PARAMETER ::      H37M12=3.7E-12
 REAL,PARAMETER ::      H3M12=3.E-12
 REAL,PARAMETER ::      H29M12=2.9E-12
 REAL,PARAMETER ::      H28M12=2.8E-12
 REAL,PARAMETER ::      H24M12=2.4E-12
 REAL,PARAMETER ::      H21M12=2.1E-12
 REAL,PARAMETER ::      H16M12=1.6E-12
 REAL,PARAMETER ::      H14M12=1.4E-12
 REAL,PARAMETER ::      H12M12=1.2E-12
 REAL,PARAMETER ::      H8M13=8.E-13
 REAL,PARAMETER ::      H46M13=4.6E-13
 REAL,PARAMETER ::      H36M13=3.6E-13
 REAL,PARAMETER ::      H135M13=1.35E-13
 REAL,PARAMETER ::      H12M13=1.2E-13
 REAL,PARAMETER ::      H1M13=1.E-13
 REAL,PARAMETER ::      H3M14=3.E-14
 REAL,PARAMETER ::      H15M14=1.5E-14
 REAL,PARAMETER ::      H14M14=1.4E-14
!
!******FOLLOWING ARE NEGATIVE FLOATING POINT CONSTANTS (HM'S)
!          ARRANGED IN DESCENDING ORDER
 REAL,PARAMETER ::      HM2M2=-.02
 REAL,PARAMETER ::      HM6666M2=-.066667
 REAL,PARAMETER ::      HMP5=-0.5
 REAL,PARAMETER ::      HMP575=-0.575
 REAL,PARAMETER ::      HMP66667=-.66667
 REAL,PARAMETER ::      HMP805=-0.805
 REAL,PARAMETER ::      HM1EZ=-1.
 REAL,PARAMETER ::      HM13EZ=-1.3
 REAL,PARAMETER ::      HM19EZ=-1.9
 REAL,PARAMETER ::      HM1E1=-10.
 REAL,PARAMETER ::      HM1597E1=-15.97469413
 REAL,PARAMETER ::      HM161E1=-16.1
 REAL,PARAMETER ::      HM1797E1=-17.97469413
 REAL,PARAMETER ::      HM181E1=-18.1
 REAL,PARAMETER ::      HM8E1=-80.
 REAL,PARAMETER ::      HM1E2=-100.
!
 REAL,PARAMETER ::      H1M16=1.0E-16
 REAL,PARAMETER ::      H1M20=1.E-20
 REAL,PARAMETER ::      HP98=0.98
 REAL,PARAMETER ::      Q19001=19.001
 REAL,PARAMETER ::      DAYSEC=1.1574E-5
 REAL,PARAMETER ::      HSIGMA=5.673E-5
 REAL,PARAMETER ::      TWENTY=20.0
 REAL,PARAMETER ::      HP537=0.537
 REAL,PARAMETER ::      HP2=0.2
 REAL,PARAMETER ::      RCO2=3.3E-4
 REAL,PARAMETER ::      Q14330=1.43306E-6
 REAL,PARAMETER ::      H3M6=3.0E-6
 REAL,PARAMETER ::      PI=3.1415927
 REAL,PARAMETER ::      DEGRAD=180.0/PI
 REAL,PARAMETER ::      H74E1=74.0
 REAL,PARAMETER ::      H15E1=15.0

 REAL, PARAMETER:: B0 = -.51926410E-4
 REAL, PARAMETER:: B1 = -.18113332E-3
 REAL, PARAMETER:: B2 = -.10680132E-5
 REAL, PARAMETER:: B3 = -.67303519E-7
 REAL, PARAMETER:: AWIDE = 0.309801E+01
 REAL, PARAMETER:: BWIDE = 0.495357E-01
 REAL, PARAMETER:: BETAWD = 0.347839E+02
 REAL, PARAMETER:: BETINW = 0.766811E+01


!     REAL, INTENT(OUT) :: EM1(28,180),EM1WDE(28,180),TABLE1(28,180), &
!                          TABLE2(28,180),TABLE3(28,180),EM3(28,180), &
!                          SOURCE(28,NBLY), DSRCE(28,NBLY)

!
      REAL :: ARNDM(NBLW),BRNDM(NBLW),BETAD(NBLW)
      REAL :: BANDLO(NBLW),BANDHI(NBLW)

      INTEGER :: IBAND(40)

      REAL :: BANDL1(64),BANDL2(64),BANDL3(35)
      REAL :: BANDH1(64),BANDH2(64),BANDH3(35) 
!     REAL :: AB15WD,SKO2D,SKC1R,SKO3R

!     REAL :: AWIDE,BWIDE,BETAWD,BETINW

!     DATA AWIDE  / 0.309801E+01/
!     DATA BWIDE  / 0.495357E-01/
!     DATA BETAWD / 0.347839E+02/
!     DATA BETINW / 0.766811E+01/

!
!% #NPADL = #PAGE*#NPAGE -  4*28*180  -  2*181 - 7*28 - 180 ;
!% #NPADL = #NPADL       -  11*28  - 2*180 - 2*30 ;

!     PARAMETER (NPADL = #NPADL - 28*NBLX - 2*28*NBLW - 7*NBLW)

      REAL ::  &
               SUM(28,180),PERTSM(28,180),SUM3(28,180),       &
               SUMWDE(28,180),SRCWD(28,NBLX),SRC1NB(28,NBLW), &
               DBDTNB(28,NBLW)
      REAL ::  &
               ZMASS(181),ZROOT(181),SC(28),DSC(28),XTEMV(28), &
               TFOUR(28),FORTCU(28),X(28),X1(28),X2(180),SRCS(28), &
               SUM4(28),SUM6(28),SUM7(28),SUM8(28),SUM4WD(28),     &
               R1T(28),R2(28),S2(28),T3(28),R1WD(28)
      REAL ::  EXPO(180),FAC(180)
      REAL ::  CNUSB(30),DNUSB(30)
      REAL ::  ALFANB(NBLW),AROTNB(NBLW)
      REAL ::  ANB(NBLW),BNB(NBLW),CENTNB(NBLW),DELNB(NBLW), &
               BETANB(NBLW)

      REAL ::  AB15(2)

      REAL ::   ARNDM1(64),ARNDM2(64),ARNDM3(35)
      REAL ::   BRNDM1(64),BRNDM2(64),BRNDM3(35)
      REAL ::   BETAD1(64),BETAD2(64),BETAD3(35)

      EQUIVALENCE (ARNDM1(1),ARNDM(1)),(ARNDM2(1),ARNDM(65)), &
                  (ARNDM3(1),ARNDM(129))
      EQUIVALENCE (BRNDM1(1),BRNDM(1)),(BRNDM2(1),BRNDM(65)), &
                  (BRNDM3(1),BRNDM(129))
      EQUIVALENCE (BETAD1(1),BETAD(1)),(BETAD2(1),BETAD(65)), &
                  (BETAD3(1),BETAD(129))

!---------------------------------------------------------------
      REAL    :: CENT,DEL,BDLO,BDHI,C1,ANU,tmp
      INTEGER :: N,I,ICNT,I1,I2E,I2
      INTEGER :: J,JP,NSUBDS,NSB,IA

!---------------------------------------------------------------

      DATA IBAND  / &
          2,   1,   2,   2,   1,   2,   1,   3,   2,   2, &
          3,   2,   2,   4,   2,   4,   2,   3,   3,   2, &
          4,   3,   4,   3,   7,   5,   6,   7,   6,   5, &
          7,   6,   7,   8,   6,   6,   8,   8,   8,   8/

      DATA BANDL1 / &
         0.000000E+00,  0.100000E+02,  0.200000E+02,  0.300000E+02, &
         0.400000E+02,  0.500000E+02,  0.600000E+02,  0.700000E+02, &
         0.800000E+02,  0.900000E+02,  0.100000E+03,  0.110000E+03, &
         0.120000E+03,  0.130000E+03,  0.140000E+03,  0.150000E+03, &
         0.160000E+03,  0.170000E+03,  0.180000E+03,  0.190000E+03, &
         0.200000E+03,  0.210000E+03,  0.220000E+03,  0.230000E+03, &
         0.240000E+03,  0.250000E+03,  0.260000E+03,  0.270000E+03, &
         0.280000E+03,  0.290000E+03,  0.300000E+03,  0.310000E+03, &
         0.320000E+03,  0.330000E+03,  0.340000E+03,  0.350000E+03, &
         0.360000E+03,  0.370000E+03,  0.380000E+03,  0.390000E+03, &
         0.400000E+03,  0.410000E+03,  0.420000E+03,  0.430000E+03, &
         0.440000E+03,  0.450000E+03,  0.460000E+03,  0.470000E+03, &
         0.480000E+03,  0.490000E+03,  0.500000E+03,  0.510000E+03, &
         0.520000E+03,  0.530000E+03,  0.540000E+03,  0.550000E+03, &
         0.560000E+03,  0.670000E+03,  0.800000E+03,  0.900000E+03, &
         0.990000E+03,  0.107000E+04,  0.120000E+04,  0.121000E+04/
      DATA BANDL2 / &
         0.122000E+04,  0.123000E+04,  0.124000E+04,  0.125000E+04, &
         0.126000E+04,  0.127000E+04,  0.128000E+04,  0.129000E+04, &
         0.130000E+04,  0.131000E+04,  0.132000E+04,  0.133000E+04, &
         0.134000E+04,  0.135000E+04,  0.136000E+04,  0.137000E+04, &
         0.138000E+04,  0.139000E+04,  0.140000E+04,  0.141000E+04, &
         0.142000E+04,  0.143000E+04,  0.144000E+04,  0.145000E+04, &
         0.146000E+04,  0.147000E+04,  0.148000E+04,  0.149000E+04, &
         0.150000E+04,  0.151000E+04,  0.152000E+04,  0.153000E+04, &
         0.154000E+04,  0.155000E+04,  0.156000E+04,  0.157000E+04, &
         0.158000E+04,  0.159000E+04,  0.160000E+04,  0.161000E+04, &
         0.162000E+04,  0.163000E+04,  0.164000E+04,  0.165000E+04, &
         0.166000E+04,  0.167000E+04,  0.168000E+04,  0.169000E+04, &
         0.170000E+04,  0.171000E+04,  0.172000E+04,  0.173000E+04, &
         0.174000E+04,  0.175000E+04,  0.176000E+04,  0.177000E+04, &
         0.178000E+04,  0.179000E+04,  0.180000E+04,  0.181000E+04, &
         0.182000E+04,  0.183000E+04,  0.184000E+04,  0.185000E+04/
      DATA BANDL3 / &
         0.186000E+04,  0.187000E+04,  0.188000E+04,  0.189000E+04, &
         0.190000E+04,  0.191000E+04,  0.192000E+04,  0.193000E+04, &
         0.194000E+04,  0.195000E+04,  0.196000E+04,  0.197000E+04, &
         0.198000E+04,  0.199000E+04,  0.200000E+04,  0.201000E+04, &
         0.202000E+04,  0.203000E+04,  0.204000E+04,  0.205000E+04, &
         0.206000E+04,  0.207000E+04,  0.208000E+04,  0.209000E+04, &
         0.210000E+04,  0.211000E+04,  0.212000E+04,  0.213000E+04, &
         0.214000E+04,  0.215000E+04,  0.216000E+04,  0.217000E+04, &
         0.218000E+04,  0.219000E+04,  0.227000E+04/

      DATA BANDH1 / &
         0.100000E+02,  0.200000E+02,  0.300000E+02,  0.400000E+02, &
         0.500000E+02,  0.600000E+02,  0.700000E+02,  0.800000E+02, &
         0.900000E+02,  0.100000E+03,  0.110000E+03,  0.120000E+03, &
         0.130000E+03,  0.140000E+03,  0.150000E+03,  0.160000E+03, &
         0.170000E+03,  0.180000E+03,  0.190000E+03,  0.200000E+03, &
         0.210000E+03,  0.220000E+03,  0.230000E+03,  0.240000E+03, &
         0.250000E+03,  0.260000E+03,  0.270000E+03,  0.280000E+03, &
         0.290000E+03,  0.300000E+03,  0.310000E+03,  0.320000E+03, &
         0.330000E+03,  0.340000E+03,  0.350000E+03,  0.360000E+03, &
         0.370000E+03,  0.380000E+03,  0.390000E+03,  0.400000E+03, &
         0.410000E+03,  0.420000E+03,  0.430000E+03,  0.440000E+03, &
         0.450000E+03,  0.460000E+03,  0.470000E+03,  0.480000E+03, &
         0.490000E+03,  0.500000E+03,  0.510000E+03,  0.520000E+03, &
         0.530000E+03,  0.540000E+03,  0.550000E+03,  0.560000E+03, &
         0.670000E+03,  0.800000E+03,  0.900000E+03,  0.990000E+03, &
         0.107000E+04,  0.120000E+04,  0.121000E+04,  0.122000E+04/
      DATA BANDH2 / &
         0.123000E+04,  0.124000E+04,  0.125000E+04,  0.126000E+04, &
         0.127000E+04,  0.128000E+04,  0.129000E+04,  0.130000E+04, &
         0.131000E+04,  0.132000E+04,  0.133000E+04,  0.134000E+04, &
         0.135000E+04,  0.136000E+04,  0.137000E+04,  0.138000E+04, &
         0.139000E+04,  0.140000E+04,  0.141000E+04,  0.142000E+04, &
         0.143000E+04,  0.144000E+04,  0.145000E+04,  0.146000E+04, &
         0.147000E+04,  0.148000E+04,  0.149000E+04,  0.150000E+04, &
         0.151000E+04,  0.152000E+04,  0.153000E+04,  0.154000E+04, &
         0.155000E+04,  0.156000E+04,  0.157000E+04,  0.158000E+04, &
         0.159000E+04,  0.160000E+04,  0.161000E+04,  0.162000E+04, &
         0.163000E+04,  0.164000E+04,  0.165000E+04,  0.166000E+04, &
         0.167000E+04,  0.168000E+04,  0.169000E+04,  0.170000E+04, &
         0.171000E+04,  0.172000E+04,  0.173000E+04,  0.174000E+04, &
         0.175000E+04,  0.176000E+04,  0.177000E+04,  0.178000E+04, &
         0.179000E+04,  0.180000E+04,  0.181000E+04,  0.182000E+04, &
         0.183000E+04,  0.184000E+04,  0.185000E+04,  0.186000E+04/
      DATA BANDH3 / &
         0.187000E+04,  0.188000E+04,  0.189000E+04,  0.190000E+04, &
         0.191000E+04,  0.192000E+04,  0.193000E+04,  0.194000E+04, &
         0.195000E+04,  0.196000E+04,  0.197000E+04,  0.198000E+04, &
         0.199000E+04,  0.200000E+04,  0.201000E+04,  0.202000E+04, &
         0.203000E+04,  0.204000E+04,  0.205000E+04,  0.206000E+04, &
         0.207000E+04,  0.208000E+04,  0.209000E+04,  0.210000E+04, &
         0.211000E+04,  0.212000E+04,  0.213000E+04,  0.214000E+04, &
         0.215000E+04,  0.216000E+04,  0.217000E+04,  0.218000E+04, &
         0.219000E+04,  0.220000E+04,  0.238000E+04/

!
!***THE FOLLOWING DATA STATEMENTS ARE BAND PARAMETERS OBTAINED USING
!   THE 1982 AFGL CATALOG ON THE SPECIFIED BANDS
      DATA ARNDM1  / &
         0.354693E+00,  0.269857E+03,  0.167062E+03,  0.201314E+04, &
         0.964533E+03,  0.547971E+04,  0.152933E+04,  0.599429E+04, &
         0.699329E+04,  0.856721E+04,  0.962489E+04,  0.233348E+04, &
         0.127091E+05,  0.104383E+05,  0.504249E+04,  0.181227E+05, &
         0.856480E+03,  0.136354E+05,  0.288635E+04,  0.170200E+04, &
         0.209761E+05,  0.126797E+04,  0.110096E+05,  0.336436E+03, &
         0.491663E+04,  0.863701E+04,  0.540389E+03,  0.439786E+04, &
         0.347836E+04,  0.130557E+03,  0.465332E+04,  0.253086E+03, &
         0.257387E+04,  0.488041E+03,  0.892991E+03,  0.117148E+04, &
         0.125880E+03,  0.458852E+03,  0.142975E+03,  0.446355E+03, &
         0.302887E+02,  0.394451E+03,  0.438112E+02,  0.348811E+02, &
         0.615503E+02,  0.143165E+03,  0.103958E+02,  0.725108E+02, &
         0.316628E+02,  0.946456E+01,  0.542675E+02,  0.351557E+02, &
         0.301797E+02,  0.381010E+01,  0.126319E+02,  0.548010E+01, &
         0.600199E+01,  0.640803E+00,  0.501549E-01,  0.167961E-01, &
         0.178110E-01,  0.170166E+00,  0.273514E-01,  0.983767E+00/
      DATA ARNDM2  / &
         0.753946E+00,  0.941763E-01,  0.970547E+00,  0.268862E+00, &
         0.564373E+01,  0.389794E+01,  0.310955E+01,  0.128235E+01, &
         0.196414E+01,  0.247113E+02,  0.593435E+01,  0.377552E+02, &
         0.305173E+02,  0.852479E+01,  0.116780E+03,  0.101490E+03, &
         0.138939E+03,  0.324228E+03,  0.683729E+02,  0.471304E+03, &
         0.159684E+03,  0.427101E+03,  0.114716E+03,  0.106190E+04, &
         0.294607E+03,  0.762948E+03,  0.333199E+03,  0.830645E+03, &
         0.162512E+04,  0.525676E+03,  0.137739E+04,  0.136252E+04, &
         0.147164E+04,  0.187196E+04,  0.131118E+04,  0.103975E+04, &
         0.621637E+01,  0.399459E+02,  0.950648E+02,  0.943161E+03, &
         0.526821E+03,  0.104150E+04,  0.905610E+03,  0.228142E+04, &
         0.806270E+03,  0.691845E+03,  0.155237E+04,  0.192241E+04, &
         0.991871E+03,  0.123907E+04,  0.457289E+02,  0.146146E+04, &
         0.319382E+03,  0.436074E+03,  0.374214E+03,  0.778217E+03, &
         0.140227E+03,  0.562540E+03,  0.682685E+02,  0.820292E+02, &
         0.178779E+03,  0.186150E+03,  0.383864E+03,  0.567416E+01/ 
      DATA ARNDM3  / &
         0.225129E+03,  0.473099E+01,  0.753149E+02,  0.233689E+02, &
         0.339802E+02,  0.108855E+03,  0.380016E+02,  0.151039E+01, &
         0.660346E+02,  0.370165E+01,  0.234169E+02,  0.440206E+00, &
         0.615283E+01,  0.304077E+02,  0.117769E+01,  0.125248E+02, &
         0.142652E+01,  0.241831E+00,  0.483721E+01,  0.226357E-01, &
         0.549835E+01,  0.597067E+00,  0.404553E+00,  0.143584E+01, &
         0.294291E+00,  0.466273E+00,  0.156048E+00,  0.656185E+00, &
         0.172727E+00,  0.118349E+00,  0.141598E+00,  0.588581E-01, &
         0.919409E-01,  0.155521E-01,  0.537083E-02/
      DATA BRNDM1  / &
         0.789571E-01,  0.920256E-01,  0.696960E-01,  0.245544E+00, &
         0.188503E+00,  0.266127E+00,  0.271371E+00,  0.330917E+00, &
         0.190424E+00,  0.224498E+00,  0.282517E+00,  0.130675E+00, &
         0.212579E+00,  0.227298E+00,  0.138585E+00,  0.187106E+00, &
         0.194527E+00,  0.177034E+00,  0.115902E+00,  0.118499E+00, &
         0.142848E+00,  0.216869E+00,  0.149848E+00,  0.971585E-01, &
         0.151532E+00,  0.865628E-01,  0.764246E-01,  0.100035E+00, &
         0.171133E+00,  0.134737E+00,  0.105173E+00,  0.860832E-01, &
         0.148921E+00,  0.869234E-01,  0.106018E+00,  0.184865E+00, &
         0.767454E-01,  0.108981E+00,  0.123094E+00,  0.177287E+00, &
         0.848146E-01,  0.119356E+00,  0.133829E+00,  0.954505E-01, &
         0.155405E+00,  0.164167E+00,  0.161390E+00,  0.113287E+00, &
         0.714720E-01,  0.741598E-01,  0.719590E-01,  0.140616E+00, &
         0.355356E-01,  0.832779E-01,  0.128680E+00,  0.983013E-01, &
         0.629660E-01,  0.643346E-01,  0.717082E-01,  0.629730E-01, &
         0.875182E-01,  0.857907E-01,  0.358808E+00,  0.178840E+00/
      DATA BRNDM2  / &
         0.254265E+00,  0.297901E+00,  0.153916E+00,  0.537774E+00, &
         0.267906E+00,  0.104254E+00,  0.400723E+00,  0.389670E+00, &
         0.263701E+00,  0.338116E+00,  0.351528E+00,  0.267764E+00, &
         0.186419E+00,  0.238237E+00,  0.210408E+00,  0.176869E+00, &
         0.114715E+00,  0.173299E+00,  0.967770E-01,  0.172565E+00, &
         0.162085E+00,  0.157782E+00,  0.886832E-01,  0.242999E+00, &
         0.760298E-01,  0.164248E+00,  0.221428E+00,  0.166799E+00, &
         0.312514E+00,  0.380600E+00,  0.353828E+00,  0.269500E+00, &
         0.254759E+00,  0.285408E+00,  0.159764E+00,  0.721058E-01, &
         0.170528E+00,  0.231595E+00,  0.307184E+00,  0.564136E-01, &
         0.159884E+00,  0.147907E+00,  0.185666E+00,  0.183567E+00, &
         0.182482E+00,  0.230650E+00,  0.175348E+00,  0.195978E+00, &
         0.255323E+00,  0.198517E+00,  0.195500E+00,  0.208356E+00, &
         0.309603E+00,  0.112011E+00,  0.102570E+00,  0.128276E+00, &
         0.168100E+00,  0.177836E+00,  0.105533E+00,  0.903330E-01, &
         0.126036E+00,  0.101430E+00,  0.124546E+00,  0.221406E+00/ 
      DATA BRNDM3  / &
         0.137509E+00,  0.911365E-01,  0.724508E-01,  0.795788E-01, &
         0.137411E+00,  0.549175E-01,  0.787714E-01,  0.165544E+00, &
         0.136484E+00,  0.146729E+00,  0.820496E-01,  0.846211E-01, &
         0.785821E-01,  0.122527E+00,  0.125359E+00,  0.101589E+00, &
         0.155756E+00,  0.189239E+00,  0.999086E-01,  0.480993E+00, &
         0.100233E+00,  0.153754E+00,  0.130780E+00,  0.136136E+00, &
         0.159353E+00,  0.156634E+00,  0.272265E+00,  0.186874E+00, &
         0.192090E+00,  0.135397E+00,  0.131497E+00,  0.127463E+00, &
         0.227233E+00,  0.190562E+00,  0.214005E+00/ 
      DATA BETAD1  / &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.234879E+03,  0.217419E+03,  0.201281E+03,  0.186364E+03, &
         0.172576E+03,  0.159831E+03,  0.148051E+03,  0.137163E+03, &
         0.127099E+03,  0.117796E+03,  0.109197E+03,  0.101249E+03, &
         0.939031E+02,  0.871127E+02,  0.808363E+02,  0.750349E+02, &
         0.497489E+02,  0.221212E+02,  0.113124E+02,  0.754174E+01, &
         0.589554E+01,  0.495227E+01,  0.000000E+00,  0.000000E+00/ 
      DATA BETAD2  / &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00/ 
      DATA BETAD3  / &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00/ 
!---------------------------------------------------------------
!     EQUIVALENCE (BANDL1(1),BANDLO(1)),(BANDL2(1),BANDLO(65)), &
!                 (BANDL3(1),BANDLO(129))

!     L     = kme-1
!     LP1   = L+1
!     LP1V  = LP1*(1+2*L/2)
!     IMAX  = ite
!     LP2   = L + 2
 
      DO I = 1,64
         BANDLO(I)=BANDL1(I)
      ENDDO

      DO I = 65,128
         BANDLO(I)=BANDL2(I-64)
      ENDDO

      DO I = 129,163
         BANDLO(I)=BANDL3(I-128)
      ENDDO

      DO I = 1,64
         BANDHI(I)=BANDH1(I)
      ENDDO

      DO I = 65,128
         BANDHI(I)=BANDH2(I-64)
      ENDDO

      DO I = 129,163
         BANDHI(I)=BANDH3(I-128)
      ENDDO

!****************************************
!***COMPUTE LOCAL QUANTITIES AND AO3,BO3,AB15
!....FOR NARROW-BANDS...
      DO 101 N=1,NBLW
      ANB(N)=ARNDM(N)
      BNB(N)=BRNDM(N)
      CENTNB(N)=HAF*(BANDLO(N)+BANDHI(N))
      DELNB(N)=BANDHI(N)-BANDLO(N)
      BETANB(N)=BETAD(N)
101   CONTINUE
      AB15(1)=ANB(57)*BNB(57)
      AB15(2)=ANB(58)*BNB(58)
!....FOR WIDE BANDS...
      AB15WD=AWIDE*BWIDE
!
!***COMPUTE INDICES: IND,INDX2,KMAXV
!SH   ICNT=0
!SH   DO 113 I1=1,L
!SH     I2E=LP1-I1
!SH     DO 115 I2=1,I2E
!SH       ICNT=ICNT+1
!SH       INDX2(ICNT)=LP1*(I2-1)+LP2*I1
!SH115     CONTINUE
!SH113   CONTINUE
!SH   KMAXV(1)=1
!SH   DO 117 I=2,L
!SH   KMAXV(I)=KMAXV(I-1)+(LP2-I)
117   CONTINUE
!SH   KMAXVM=KMAXV(L)
!***COMPUTE RATIOS OF CONT. COEFFS
      SKC1R=BETAWD/BETINW
      SKO3R=BETAD(61)/BETINW
      SKO2D=ONE/BETINW
!
!****BEGIN TABLE COMPUTATIONS HERE***
!***COMPUTE TEMPS, MASSES FOR TABLE ENTRIES
!---NOTE: THE DIMENSIONING AND INITIALIZATION OF XTEMV AND OTHER ARRAYS
!   WITH DIMENSION OF 28 IMPLY A RESTRICTION OF MODEL TEMPERATURES FROM
!   100K TO 370K.
!---THE DIMENSIONING OF ZMASS,ZROOT AND OTHER ARRAYS WITH DIMENSION OF
!   180 IMPLY A RESTRICTION OF MODEL H2O AMOUNTS SUCH THAT OPTICAL PATHS
!   ARE BETWEEN 10**-16 AND 10**2, IN CGS UNITS.
      ZMASS(1)=H1M16
      DO 201 J=1,180
      JP=J+1
      ZROOT(J)=SQRT(ZMASS(J))
      ZMASS(JP)=ZMASS(J)*H1P25892
201   CONTINUE
      DO 203 I=1,28
      XTEMV(I)=HNINETY+TEN*I
      TFOUR(I)=XTEMV(I)*XTEMV(I)*XTEMV(I)*XTEMV(I)
      FORTCU(I)=FOUR*XTEMV(I)*XTEMV(I)*XTEMV(I)
203   CONTINUE
!******THE COMPUTATION OF SOURCE,DSRCE IS  NEEDED ONLY
!   FOR THE COMBINED WIDE-BAND CASE.TO OBTAIN THEM,THE SOURCE
!   MUST BE COMPUTED FOR EACH OF THE (NBLX) WIDE BANDS(=SRCWD)
!   THEN COMBINED (USING IBAND) INTO SOURCE.
      DO 205 N=1,NBLY
      DO 205 I=1,28
      SOURCE(I,N)=ZERO
205   CONTINUE
      DO 207 N=1,NBLX
      DO 207 I=1,28
      SRCWD(I,N)=ZERO
207   CONTINUE
!---BEGIN FREQ. LOOP (ON N)
      DO 211 N=1,NBLX
        IF (N.LE.46) THEN
!***THE 160-1200 BAND CASES
          CENT=CENTNB(N+16)
          DEL=DELNB(N+16)
          BDLO=BANDLO(N+16)
          BDHI=BANDHI(N+16)
        ENDIF
        IF (N.EQ.NBLX) THEN
!***THE 2270-2380 BAND CASE
          CENT=CENTNB(NBLW)
          DEL=DELNB(NBLW)
          BDLO=BANDLO(NBLW)
          BDHI=BANDHI(NBLW)
        ENDIF
!***FOR PURPOSES OF ACCURACY, ALL EVALUATIONS OF PLANCK FCTNS ARE MADE
!  ON 10 CM-1 INTERVALS, THEN SUMMED INTO THE (NBLX) WIDE BANDS.
      NSUBDS=(DEL-H1M3)/10+1
      DO 213 NSB=1,NSUBDS
      IF (NSB.NE.NSUBDS) THEN
        CNUSB(NSB)=TEN*(NSB-1)+BDLO+FIVE
        DNUSB(NSB)=TEN
      ELSE
        CNUSB(NSB)=HAF*(TEN*(NSB-1)+BDLO+BDHI)
        DNUSB(NSB)=BDHI-(TEN*(NSB-1)+BDLO)
      ENDIF
      C1=(H37412M5)*CNUSB(NSB)**3
!---BEGIN TEMP. LOOP (ON I)
      DO 215 I=1,28
      X(I)=H1P4387*CNUSB(NSB)/XTEMV(I)
      X1(I)=EXP(X(I))
      SRCS(I)=C1/(X1(I)-ONE)
      SRCWD(I,N)=SRCWD(I,N)+SRCS(I)*DNUSB(NSB)
215   CONTINUE
213   CONTINUE
211   CONTINUE
!***THE FOLLOWING LOOPS CREATE THE COMBINED WIDE BAND QUANTITIES SOURCE
!   AND DSRCE
      DO 221 N=1,40
      DO 221 I=1,28
      SOURCE(I,IBAND(N))=SOURCE(I,IBAND(N))+SRCWD(I,N)
221   CONTINUE
      DO 223 N=9,NBLY
      DO 223 I=1,28
      SOURCE(I,N)=SRCWD(I,N+32)
223   CONTINUE
      DO 225 N=1,NBLY
      DO 225 I=1,27
      DSRCE(I,N)=(SOURCE(I+1,N)-SOURCE(I,N))*HP1
225   CONTINUE
      DO 231 N=1,NBLW
      ALFANB(N)=BNB(N)*ANB(N)
      AROTNB(N)=SQRT(ALFANB(N))
231   CONTINUE
!***FIRST COMPUTE PLANCK FCTNS (SRC1NB) AND DERIVATIVES (DBDTNB) FOR
!   USE IN TABLE EVALUATIONS. THESE ARE DIFFERENT FROM SOURCE,DSRCE
!   BECAUSE DIFFERENT FREQUENCY PTS ARE USED IN EVALUATION, THE FREQ.
!   RANGES ARE DIFFERENT, AND THE DERIVATIVE ALGORITHM IS DIFFERENT.
!
      DO 301 N=1,NBLW
      CENT=CENTNB(N)
      DEL=DELNB(N)
!---NOTE: AT PRESENT, THE IA LOOP IS ONLY USED FOR IA=2. THE LOOP STRUCT
!   IS KEPT SO THAT IN THE FUTURE, WE MAY USE A QUADRATURE SCHEME FOR
!   THE PLANCK FCTN EVALUATION, RATHER THAN USE THE MID-BAND FREQUENCY.
#if 0
      DO 303 IA=1,3
#else
!jm -- getting floating point exceptions for IA=1, since 2 is only
!      used anyway, I disabled the looping.
      DO 303 IA=2,2
#endif
      ANU=CENT+HAF*(IA-2)*DEL
      C1=(H37412M5)*ANU*ANU*ANU+H1M20
!---TEMPERATURE LOOP---
      DO 305 I=1,28
         X(I)=H1P4387*ANU/XTEMV(I)
         X1(I)=EXP(X(I))
!#$      tmp=max((X1(I)-ONE),H1M20)
!#$      SC(I)=C1/tmp
         SC(I)=C1/((X1(I)-ONE)+H1M20)
!#$      DSC(I)=X(I)*SC(I)*SC(I)*X1(I)/(XTEMV(I)*C1)
         DSC(I)=SC(I)*SC(I)*X(I)*X1(I)/(XTEMV(I)*C1)
305      CONTINUE
      IF (IA.EQ.2) THEN
         DO 307 I=1,28
         SRC1NB(I,N)=DEL*SC(I)
         DBDTNB(I,N)=DEL*DSC(I)
307      CONTINUE
      ENDIF
303   CONTINUE
301   CONTINUE
!***NEXT COMPUTE R1T,R2,S2,AND T3- COEFFICIENTS USED FOR E3 FUNCTION
!   WHEN THE OPTICAL PATH IS LESS THAN 10-4. IN THIS CASE, WE ASSUME A
!   DIFFERENT DEPENDENCE ON (ZMASS).
!---ALSO OBTAIN R1WD, WHICH IS R1T SUMMED OVER THE 160-560 CM-1 RANGE
      DO 311 I=1,28
      SUM4(I)=ZERO
      SUM6(I)=ZERO
      SUM7(I)=ZERO
      SUM8(I)=ZERO
      SUM4WD(I)=ZERO
311   CONTINUE
      DO 313 N=1,NBLW
      CENT=CENTNB(N)
!***PERFORM SUMMATIONS FOR FREQ. RANGES OF 0-560,1200-2200 CM-1 FOR SUM4
!   SUM6,SUM7,SUM8
      IF (CENT.LT.560. .OR. CENT.GT.1200..AND.CENT.LE.2200.) THEN
         DO 315 I=1,28
         SUM4(I)=SUM4(I)+SRC1NB(I,N)
         SUM6(I)=SUM6(I)+DBDTNB(I,N)
         SUM7(I)=SUM7(I)+DBDTNB(I,N)*AROTNB(N)
         SUM8(I)=SUM8(I)+DBDTNB(I,N)*ALFANB(N)
315      CONTINUE
      ENDIF
!***PERFORM SUMMATIONS OVER 160-560 CM-1 FREQ RANGE FOR E1 CALCS (SUM4WD
      IF (CENT.GT.160. .AND. CENT.LT.560.) THEN
         DO 316 I=1,28
         SUM4WD(I)=SUM4WD(I)+SRC1NB(I,N)
316      CONTINUE
      ENDIF
313   CONTINUE
      DO 317 I=1,28
      R1T(I)=SUM4(I)/TFOUR(I)
      R2(I)=SUM6(I)/FORTCU(I)
      S2(I)=SUM7(I)/FORTCU(I)
      T3(I)=SUM8(I)/FORTCU(I)
      R1WD(I)=SUM4WD(I)/TFOUR(I)
317   CONTINUE
      DO 401 J=1,180
      DO 401 I=1,28
      SUM(I,J)=ZERO
      PERTSM(I,J)=ZERO
      SUM3(I,J)=ZERO
      SUMWDE(I,J)=ZERO
401   CONTINUE
!---FREQUENCY LOOP BEGINS---
      DO 411 N=1,NBLW
      CENT=CENTNB(N)
!***PERFORM CALCULATIONS FOR FREQ. RANGES OF 0-560,1200-2200 CM-1
      IF (CENT.LT.560. .OR. CENT.GT.1200..AND.CENT.LE.2200.) THEN
         DO 413 J=1,180
         X2(J)=AROTNB(N)*ZROOT(J)
         EXPO(J)=EXP(-X2(J))
413      CONTINUE
         DO 415 J=1,180
         IF (X2(J).GE.HUNDRED) THEN
              EXPO(J)=ZERO
         ENDIF
415      CONTINUE
         DO 417 J=121,180
         FAC(J)=ZMASS(J)*(ONE-(ONE+X2(J))*EXPO(J))/(X2(J)*X2(J))
417      CONTINUE
         DO 419 J=1,180
         DO 419 I=1,28
         SUM(I,J)=SUM(I,J)+SRC1NB(I,N)*EXPO(J)
         PERTSM(I,J)=PERTSM(I,J)+DBDTNB(I,N)*EXPO(J)
419      CONTINUE
         DO 421 J=121,180
         DO 421 I=1,28
         SUM3(I,J)=SUM3(I,J)+DBDTNB(I,N)*FAC(J)
421      CONTINUE
      ENDIF
!---COMPUTE SUM OVER 160-560 CM-1 RANGE FOR USE IN E1 CALCS (SUMWDE)
      IF (CENT.GT.160. .AND. CENT.LT.560.) THEN
         DO 420 J=1,180
         DO 420 I=1,28
         SUMWDE(I,J)=SUMWDE(I,J)+SRC1NB(I,N)*EXPO(J)
420      CONTINUE
      ENDIF
411   CONTINUE
      DO 431 J=1,180
      DO 431 I=1,28
      EM1(I,J)=SUM(I,J)/TFOUR(I)
      TABLE1(I,J)=PERTSM(I,J)/FORTCU(I)
431   CONTINUE
      DO 433 J=121,180
      DO 433 I=1,28
      EM3(I,J)=SUM3(I,J)/FORTCU(I)
433   CONTINUE
      DO 441 J=1,179
      DO 441 I=1,28
      TABLE2(I,J)=(TABLE1(I,J+1)-TABLE1(I,J))*TEN
441   CONTINUE
      DO 443 J=1,180
      DO 443 I=1,27
      TABLE3(I,J)=(TABLE1(I+1,J)-TABLE1(I,J))*HP1
443   CONTINUE
      DO 445 I=1,28
      TABLE2(I,180)=ZERO
445   CONTINUE
      DO 447 J=1,180
      TABLE3(28,J)=ZERO
447   CONTINUE
      DO 449 J=1,2
      DO 449 I=1,28
      EM1(I,J)=R1T(I)
449   CONTINUE
      DO 451 J=1,120
      DO 451 I=1,28
      EM3(I,J)=R2(I)/TWO-S2(I)*SQRT(ZMASS(J))/THREE+T3(I)*ZMASS(J)/EIGHT
451   CONTINUE
      DO 453 J=121,180
      DO 453 I=1,28
      EM3(I,J)=EM3(I,J)/ZMASS(J)
453   CONTINUE
!***NOW COMPUTE E1 TABLES FOR 160-560 CM-1 BANDS ONLY.
!   WE USE R1WD AND SUMWDE OBTAINED ABOVE.
      DO 501 J=1,180
      DO 501 I=1,28
      EM1WDE(I,J)=SUMWDE(I,J)/TFOUR(I)
501   CONTINUE
      DO 503 J=1,2
      DO 503 I=1,28
      EM1WDE(I,J)=R1WD(I)
503   CONTINUE
   
      END SUBROUTINE TABLE

!---------------------------------------------------------------------
    SUBROUTINE SOLARD(IHRST,IDAY,MONTH,JULYR)
!---------------------------------------------------------------------
    IMPLICIT NONE
!---------------------------------------------------------------------
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                               .
! SUBPROGRAM:    SOLARD      COMPUTE THE SOLAR-EARTH DISTANCE
!   PRGRMMR: Q.ZHAO           ORG: W/NMC2     DATE: 96-7-23       
!     
! ABSTRACT:
!     SOLARD CALCULATES THE SOLAR-EARTH DISTANCE ON EACH DAY
!     FOR USE IN SHORT-WAVE RADIATION.
!     
! PROGRAM HISTORY LOG:
!   96-07-23  Q.ZHAO      - ORIGINATOR
!   98-10-09  Q.ZHAO      - CHANGED TO USE IW3JDN IN W3LIB TO
!                           CALCULATE JD.
!     
! USAGE: CALL SOLARD FROM SUBROUTINE INIT
!
!   INPUT ARGUMENT LIST:
!       NONE
!  
!   OUTPUT ARGUMENT LIST: 
!       R1   - THE NON-DIMENSIONAL DISTANCE BETWEEN SUN AND THE EARTH
!              (LESS THAN 1.0 IN SUMMER AND LARGER THAN 1.0 IN WINTER).
!     
!   INPUT FILES:
!     NONE
!        
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!  
!     UNIQUE: NONE
!  
!     LIBRARY: IW3JDN
!  
!   COMMON BLOCKS: CTLBLK
!   
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!***********************************************************************
     REAL, PARAMETER :: PI=3.1415926,PI2=2.*PI
!-----------------------------------------------------------------------
!     INTEGER, INTENT(IN ) :: IHRST,IDAT(3)
      INTEGER, INTENT(IN ) :: IHRST,IDAY,MONTH,JULYR
!     REAL   , INTENT(OUT) :: R1
!-----------------------------------------------------------------------
      INTEGER :: NDM(12),JYR19,JMN
      REAL    :: CCR

      DATA JYR19/1900/, JMN/0/, CCR/1.3E-6/
      DATA NDM/0,31,59,90,120,151,181,212,243,273,304,334/
 
!.....TPP = DAYS BETWEEN EPOCH AND PERIHELION PASSAGE OF 1900
!.....JDOR1 = JD OF DECEMBER 30, 1899 AT 12 HOURS UT
!.....JDOR2 = JD OF EPOCH WHICH IS JANUARY 0, 1990 AT 12 HOURS UT
!
      REAL    :: TPP
      DATA TPP/1.55/

      INTEGER :: JDOR2,JDOR1
      DATA JDOR2/2415020/, JDOR1/2415019/

      REAL    :: DAYINC,DAT,T,YEAR,DATE,EM,E,EC,EP,CR,FJD,FJD1
      INTEGER :: JYR,JMNTH,JDAY,JHRJD,JHR,JD,ITER
!
!     LIBRARY: IW3JDN
!
!    --------------------------------------------------------------------
!     COMPUTES JULIAN DAY AND FRACTION FROM YEAR, MONTH, DAY AND TIME UT
!     ACCURATE ONLY BETWEEN MARCH 1, 1900 AND FEBRUARY 28, 2100
!     BASED ON JULIAN CALENDAR CORRECTED TO CORRESPOND TO GREGORIAN
!     CALENDAR DURING THIS PERIOD
!    --------------------------------------------------------------------

!     JYR=IDAT(3)
!     JMNTH=IDAT(1)
!     JDAY=IDAT(2)
      JHR=IHRST
!
      JD=IDAY-32075                                                     &
             +1461*(JULYR+4800+(MONTH-14)/12)/4                         &
             +367*(MONTH-2-(MONTH-14)/12*12)/12                         &
             -3*((JULYR+4900+(MONTH-14)/12)/100)/4
      IF(JHR.LT.12)THEN
        JD=JD-1
        FJD=.5+.041666667*REAL(JHR)+.00069444444*REAL(JMN)
      ELSE
  7     FJD=.041666667E0*FLOAT(JHR-12)+.00069444444E0*FLOAT(JMN)
      END IF
      DAYINC=JHR/24.0
      FJD1=JD+FJD+DAYINC
      JD=FJD1
      FJD=FJD1-JD
!***
!*** CALCULATE THE SOLAR-EARTH DISTANCE
!***
      DAT=REAL(JD-JDOR2)-TPP+FJD
!***
!    COMPUTES TIME IN JULIAN CENTURIES AFTER EPOCH
!***
      T=FLOAT(JD-JDOR2)/36525.E0
!***
!    COMPUTES LENGTH OF ANOMALISTIC AND TROPICAL YEARS (MINUS 365 DAYS)
!***
      YEAR=.25964134E0+.304E-5*T
!***
!    COMPUTES ORBIT ECCENTRICITY FROM T
!***
      EC=.01675104E0-(.418E-4+.126E-6*T)*T
      YEAR=YEAR+365.E0
!***
!    DATE=DAYS SINCE LAST PERIHELION PASSAGE
!***
      DATE = MOD(DAT,YEAR)
!***
!    SOLVE ORBIT EQUATIONS BY NEWTON'S METHOD
!***
      EM=PI2*DATE/YEAR
      E=1.E0
      ITER = 0
 31   EP=E-(E-EC*SIN(E)-EM)/(1.E0-EC*COS(E))
      CR=ABS(E-EP)
      E=EP
      ITER = ITER + 1
      IF(ITER.GT.10) GOTO 1031
      IF(CR.GT.CCR) GO TO 31
 1031 CONTINUE
      R1=1.E0-EC*COS(E)
!
!     WRITE(6,1000)JYR,JMNTH,JDAY,JHR,R1
!     WRITE(6,*)JHR,R1
 1000 FORMAT('SUN-EARTH DISTANCE CALCULATION FINISHED IN SOLARD'/ &
             'YEAR=',I5,'  MONTH=',I3,'  DAY=',I3,' HOUR=' &
      ,      I3,' R1=',F9.4)
!***
!    RETURN TO RADTN
!***
    END SUBROUTINE SOLARD
!---------------------------------------------------------------------
    SUBROUTINE CAL_MON_DAY(JULDAY,julyr,Jmonth,Jday)     
!---------------------------------------------------------------------
    IMPLICIT NONE
!-----------------------------------------------------------------------
    INTEGER, INTENT(IN) :: JULDAY,julyr
    INTEGER, INTENT(OUT) :: Jmonth,Jday
    LOGICAL :: LEAP,NOT_FIND_DATE
    INTEGER :: MONTH (12),itmpday,itmpmon,i
!-----------------------------------------------------------------------
    DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/
!***********************************************************************
    NOT_FIND_DATE = .true.

    itmpday = JULDAY
    itmpmon = 1
    LEAP=.FALSE.
    IF(MOD(julyr,4).EQ.0)THEN
      MONTH(2)=29
      LEAP=.TRUE.
    ENDIF

    i = 1
    DO WHILE (NOT_FIND_DATE)
       IF(itmpday.GT.MONTH(i))THEN
         itmpday=itmpday-MONTH(i)
       ELSE
         Jday=itmpday
         Jmonth=i
         NOT_FIND_DATE = .false.
       ENDIF
       i = i+1
    END DO

    END SUBROUTINE CAL_MON_DAY
!!================================================================================
! CO2 initialization code

      FUNCTION ANTEMP(L,Z)
      REAL :: ZB(10,7),C(11,7),DELTA(10,7),TSTAR(7)
! ************** TROPICAL SOUNDING **************************
      DATA (ZB(N,1),N=1,10)/  2.0,   3.0,   16.5,  21.5,  45.0, &
                              51.0,  70.0,  100.,  200.,  300./
      DATA (C(N,1),N=1,11)/ -6.0,  -4.0,  -6.7,   4.0,   2.2,   &
                         1.0,  -2.8,  -.27,   0.0,   0.0,  0.0/
      DATA (DELTA(N,1),N=1,10)/.5,    .5,    .3,    .5,    1.0, &
                              1.0,   1.0,   1.0,   1.0,    1.0/
! ************** SUB-TROPICAL SUMMER ************************
      DATA (ZB(N,2),N=1,10)/ 1.5,   6.5,  13.0,  18.0,  26.0, &
                              36.0,  48.0,  50.0, 70.0,  100./
      DATA (C(N,2),N=1,11)/ -4.0,  -6.0,  -6.5,   0.0,   1.2, &
                        2.2,   2.5,   0.0,  -3.0,  -0.25,  0.0/
      DATA (DELTA(N,2),N=1,10)/ .5,  1.0,    .5,    .5,   1.0, &
                              1.0,  2.5,    .5,   1.0,   1.0/
! ************** SUB-TROPICAL WINTER ************************
      DATA (ZB(N,3),N=1,10)/ 3.0,  10.0,  19.0,  25.0,  32.0, &
                              44.5, 50.0,  71.0,  98.0,  200.0/
      DATA (C(N,3),N=1,11)/ -3.5,  -6.0,  -0.5,  0.0,   0.4, &
                              3.2,   1.6,  -1.8, -0.7,   0.0,   0.0/
      DATA (DELTA(N,3),N=1,10)/ .5,   .5,  1.0,   1.0,   1.0, &
                              1.0,  1.0,  1.0,   1.0,   1.0/
! *************  SUB-ARCTIC SUMMER *************************
      DATA (ZB(N,4),N=1,10)/ 4.7, 10.0,  23.0,  31.8,  44.0, &
                              50.2, 69.2, 100.0, 102.0, 103.0/
      DATA (C(N,4),N=1,11)/ -5.3, -7.0,   0.0,  1.4,   3.0, &
                               0.7, -3.3,  -0.2,  0.0,   0.0,  0.0/
      DATA (DELTA(N,4),N=1,10)/ .5,   .3,  1.0,   1.0,   2.0, &
                              1.0,  1.5,  1.0,   1.0,   1.0/
! ************ SUB-ARCTIC WINTER *****************************
      DATA (ZB(N,5),N=1,10)/ 1.0,   3.2,   8.5,   15.5,   25.0, &
                              30.0,  35.0,  50.0,  70.0,  100.0/
      DATA (C(N,5),N=1,11)/ 3.0,  -3.2,  -6.8,  0.0,  -0.6, &
                              1.0,   1.2,   2.5, -0.7,  -1.2,  0.0/
      DATA (DELTA(N,5),N=1,10)/ .4,   1.5,    .3 ,   .5,   1.0, &
                              1.0,   1.0,   1.0,   1.0,   1.0/
! ************ US STANDARD 1976 ******************************
      DATA (ZB(N,6),N=1,10)/ 11.0,  20.0,  32.0,  47.0,  51.0, & 
                             71.0,  84.8520,  90.0,  91.0,  92.0/
      DATA (C(N,6),N=1,11)/ -6.5,   0.0,   1.0,   2.80,  0.0, &
                             -2.80, -2.00,  0.0,   0.0,   0.0,  0.0/
      DATA (DELTA(N,6),N=1,10)/ 0.3,   1.0,   1.0,   1.0,   1.0, &
                              1.0,   1.0,   1.0,   1.0,   1.0/
!
! ************ ENLARGED US STANDARD 1976 **********************
      DATA (ZB(N,7),N=1,10)/ 11.0,  20.0,  32.0,  47.0,  51.0, &
                             71.0,  84.8520,  90.0,  91.0,  92.0/
      DATA (C(N,7),N=1,11)/ -6.5,   0.0,   1.0,   2.80,  0.0, &
                             -2.80, -2.00,  0.0,   0.0,   0.0,  0.0/
      DATA (DELTA(N,7),N=1,10)/ 0.3,   1.0,   1.0,   1.0,   1.0, &
                              1.0,   1.0,   1.0,   1.0,   1.0/
!
      DATA TSTAR/ 300.0,  294.0,  272.2,  287.0,  257.1, 2*288.15/
!
      NLAST=10
      TEMP=TSTAR(L)+C(1,L)*Z
      DO 20 N=1,NLAST
      EXPO=(Z-ZB(N,L))/DELTA(N,L)
      EXPP=ZB(N,L)/DELTA(N,L)
!JD single-precision change
!      FAC=EXP(EXPP)+EXP(-EXPP)
!mp	write(6,*) '.........................................'
!mp what in the hell does the next line do?
!mp	
!mp	apparently if statement <0 or =0 then 23, else 24
!mp     IF(ABS(EXPO)-100.0) 23,23,24
!
! changed to a more reasonable value for the workstation	
!
      IF(ABS(EXPO)-50.0) 23,23,24
   23 X=EXP(EXPO)
      Y=X+1.0/X
      ZLOG=ALOG(Y)
      GO TO 25
   24 ZLOG=ABS(EXPO)
!mp   25 IF(EXPP-100.0) 27,27,28
   25 IF(EXPP-50.0) 27,27,28
!JD single-precision change
   27 FAC=EXP(EXPP)+EXP(-EXPP)
      FACLOG=ALOG(FAC)
      GO TO 29
   28 FACLOG=EXPP
!     TEMP=TEMP+(C(N+1,L)-C(N,L))*0.5*(Z+DELTA(N,L)*
!    1     ALOG((EXP(EXPO)+EXP(-EXPO))/FAC))
   29 TEMP=TEMP+(C(N+1,L)-C(N,L))*0.5*(Z+DELTA(N,L)* &
           (ZLOG-FACLOG))
!mp	write(6,*) 'ANTEMP pieces (C,C,ZLOG,FACLOG)', C(N+1,L),C(N,L),
!mp     +	ZLOG,FACLOG
   20 CONTINUE
      ANTEMP=TEMP

      END FUNCTION ANTEMP

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc

      SUBROUTINE COEINT(RAT,IR)
! **********************************************************************
!
!
!            THE TRANSMISSION FUNCTION BETWEEN P1 AND P2 IS ASSUMED TO
!       THE  FUNCTIONAL FORM
!                     TAU(P1,P2)= 1.0-SQRT(C*LOG(1.0+X*PATH)),
!               WHERE
!                     PATH(P1,P2)=((P1-P2)**2)*(P1+P2+CORE)/
!                                 (ETA*(P1+P2+CORE)+(P1-P2))
!
!
!        THE PARAMETERS C AND X ARE FUNCTIONS OF P2, AND ARE TO BE DETER
!        WHILE CORE IS A PRESPECIFIED NUMBER.ETA IS A FUNCTION OF THE TH
!        PRODUCT (CX);IT IS OBTAITED ITERATIVELY. THE DERIVATION OF ALL
!        VALUES WILL BE EXPLAINED IN A FORTHCOMING PAPER.
!            SUBROUTINE COEINT DETERMINES C(I) AND X(I) BY USING THE ACT
!        VALUES OF TAU(P(I-2),P(I)) AND TAU(P(I-1),P(I)) AND THE PREVIOU
!        ITERATION VALUE OF ETA.
!             DEFINE:
!                PATHA=PATH(P(I),P(I-2),CORE,ETA)
!                PATHB=PATH(P(I),P(I-1),CORE,ETA);
!        THEN
!                R=(1-TAU(P(I),P(I-2)))/(1-TAU(P(I),P(I-1)))
!                 = SQRT(LOG(1+X*PATHA)/LOG(1+X*PATHB)),
!        SO THAT
!                R**2= LOG(1+X*PATHA)/LOG(1+X*PATHB).
!        THIS EQUATION CAN BE SOLVED BY NEWTON S METHOD FOR X AND THEN T
!        RESULT USED TO FIND C. THIS IS REPEATED FOR EACH VALUE OF I GRE
!        THAN 2 TO GIVE THE ARRAYS X(I) AND C(I).
!             NEWTON S METHOD FOR SOLVING THE EQUATION
!                 F(X)=0
!        MAKES USE OF THE LOOP XNEW= XOLD-F(XOLD)/FPRIME(XOLD).
!        THIS IS ITERATED 20 TIMES, WHICH IS PROBABLY EXCESSIVE.
!        THE FIRST GUESS FOR ETA IS 3.2E-4*EXP(-P(I)/1000),WHICH HAS
!        BEEN FOUND TO BE FAIRLY REALISTIC BY EXPERIMENT; WE ITERATE 5 T
!        (AGAIN,PROBABLY EXCESSIVELY) TO OBTAIN THE VALUES FOR C,X,ETA T
!        USED FOR INTERPOLATION.
!           THERE ARE SEVERAL POSSIBLE PITFALLS:
!              1) IN THE COURSE OF ITERATION, X MAY REACH A VALUE WHICH
!                 1+X*PATHA NEGATIVE; IN THIS CASE THE ITERATION IS STOP
!                 AND AN ERROR MESSAGE IS PRINTED OUT.
!              2) EVEN IF (1) DOES NOT OCCUR, IT IS STILL POSSIBLE THAT
!                 BE NEGATIVE AND LARGE ENOUGH TO MAKE 1+X*PATH(P(I),0,C
!                 NEGATIVE. THIS IS CHECKED FOR IN A FINAL LOOP, AND IF
!                 A WARNING IS PRINTED OUT.
!
!  *********************************************************************
!....
!     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!     COMMON/PRESS/PA(109)
      REAL RAT,SINV
!     REAL PA,CORE,TRANSA,PATH,UEXP,SEXP,ETA,SEXPV
      REAL PA2
!     COMMON/TRAN/ TRANSA(109,109)
!     COMMON/COEFS/XA(109),CA(109),ETA(109),SEXPV(109),CORE,UEXP,SEXP
      DIMENSION PATH0(109),ETAP(109),XAP(109),CAP(109)
      DIMENSION SINV(4)
      INTEGER :: IERR
      DATA SINV/2.74992,2.12731,4.38111,0.0832926/
!NOV89   DIMENSION SINV(3)
!NOV89   DATA SINV/2.74992,2.12731,4.38111/
!O222  OLD CODE USED 2.7528 RATHER THAN 2.74992 ---K.A.C. OCTOBER 1988
!O222   WHEN 2.7528 WAS USED,WE EXACTLY REPRODUCED THE MRF CO2 ARRAYS
      CORE=5.000
      UEXP=0.90
!      P0=0.7
      DO 902 I=1,109
      PA2=PA(I)*PA(I)
      SEXPV(I)=.505+2.0E-5*PA(I)+.035*(PA2-.25)/(PA2+.25)
902   CONTINUE
      DO 900 I=1,109
      ETA(I)=3.2E-4*EXP(-PA(I)/500.)
      ETAP(I)=ETA(I)
900   CONTINUE
      DO 1200 NP=1,10
      DO 1000 I=3,109
      SEXP=SEXPV(I)
      R=(1.0D0-TRANSA(I,I-2))/(1.0D0-TRANSA(I,I-1))
      REXP=R**(UEXP/SEXP)
      arg1=path(pa(i),pa(i-2),core,eta(i))
      arg2=path(pa(i),pa(i-1),core,eta(i))
      PATHA=(PATH(PA(I),PA(I-2),CORE,ETA(I)))**UEXP
      PATHB=(PATH(PA(I),PA(I-1),CORE,ETA(I)))**UEXP
      XX=2.0D0*(PATHB*REXP-PATHA)/(PATHB*PATHB*REXP-PATHA*PATHA)
      DO 1010 LL=1,20
      F1=DLOG(1.0D0+XX*PATHA)
      F2=DLOG(1.0D0+XX*PATHB)
      F=F1/F2-REXP
      FPRIME=(F2*PATHA/(1.0D0+XX*PATHA)-F1*PATHB/(1.0D0+XX*PATHB))/ &
          (F2*F2)
      XX=XX-F/FPRIME
      CHECK=1.0D0+XX*PATHA
!!!!  IF (CHECK) 1020,1020,1025
      IF(CHECK.LE.0.)THEN
        WRITE(errmess,360)I,LL,CHECK
        WRITE(errmess,*)' xx=',xx,' patha=',patha
  360   FORMAT(' ERROR,I=',I3,'LL=',I3,'CHECK=',F20.10)
        CALL wrf_error_fatal ( errmess )
      ENDIF
 1010 CONTINUE
      CA(I)=(1.0D0-TRANSA(I,I-2))**(UEXP/SEXP)/ &
       (DLOG(1.0D0+XX*PATHA)+1.0D-20)
      XA(I)=XX
1000  CONTINUE
      XA(2)=XA(3)
      XA(1)=XA(3)
      CA(2)=CA(3)
      CA(1)=CA(3)
      DO 1100 I=3,109
      PATH0(I)=(PATH(PA(I),0.,CORE,ETA(I)))**UEXP
      PATH0(I)=1.0D0+XA(I)*PATH0(I)
!+++  IF (PATH0(I).LT.0.) WRITE (6,361) I,PATH0(I),XA(I)
1100  CONTINUE
      DO 1035 I=1,109
      SEXP=SEXPV(I)
      ETAP(I)=ETA(I)
      ETA(I)=(SINV(IR)/RAT)**(1./SEXP)* &
        (CA(I)*XA(I))**(1./UEXP)
1035  CONTINUE
!
!     THE ETA FORMULATION IS DETAILED IN SCHWARZKOPF AND FELS(1985).
!        THE QUANTITY SINV=(G*DELTANU)/(RCO2*D*S)
!      IN CGS UNITS,WITH D,THE DIFFUSICITY FACTOR=2, AND
!      S,THE SUM OF CO2 LINE STRENGTHS OVER THE 15UM CO2 BAND
!       ALSO,THE DENOMINATOR IS MULTIPLIED BY
!      1000 TO PERMIT USE OF MB UNITS FOR PRESSURE.
!        S IS ACTUALLY WEIGHTED BY B(250) AT 10 CM-1 WIDE INTERVALS,IN
!      ORDER TO BE CONSISTENT WITH THE METHODS USED TO OBTAIN THE LBL
!      1-BAND CONSOLIDATED TRANCMISSION FUNCTIONS.
!      FOR THE 490-850 INTERVAL (DELTANU=360,IR=1) SINV=2.74992.
!      (SLIGHTLY DIFFERENT FROM 2.7528 USED IN EARLIER VERSIONS)
!      FOR THE 490-670 INTERVAL (IR=2) SINV=2.12731
!      FOR THE 670-850 INTERVAL (IR=3) SINV=4.38111
!      FOR THE 2270-2380 INTERVAL (IR=4) SINV=0.0832926
!      SINV HAS BEEN OBTAINED USING THE 1982 AFGL CATALOG FOR CO2
!        RAT IS THE ACTUAL CO2 MIXING RATIO IN UNITS OF 330 PPMV,
!      LETTING USE OF THIS FORMULATION FOR ANY CO2 CONCENTRATION.
!
!     WRITE (6,366) (NP,I,CA(I),XA(I),ETA(I),SEXPV(I),I=1,109)
!366   FORMAT (2I4,4E20.12)
1200  CONTINUE
 361  FORMAT (' **WARNING:** 1+XA*PATH(PA(I),0) IS NEGATIVE,I= ',I3,/ &
       20X,'PATH0(I)=',F16.6,' XA(I)=',F16.6)
      RETURN
      END SUBROUTINE COEINT

!--------------


!CCC  PROGRAM CO2INS
      SUBROUTINE CO2INS(T22,T23,T66,IQ,L,LP1,iflag)
!     *********************************************************
!       SAVE DATA ON PERMANENT DATA SET DENOTED BY CO222 ******
!          ..... K.CAMPANA   MARCH 1988,OCTOBER 1988...
!          ..... K.CAMPANA   DECEMBER 1988-CLEANED UP FOR LAUNCHER
!          ..... K.CAMPANA   NOVEMBER 1989-ALTERED FOR NEW RADIATION
!     *********************************************************
      DIMENSION T22(LP1,LP1,3),T23(LP1,LP1,3),T66(LP1,LP1,6)
      DIMENSION DCDT8(LP1,LP1),DCDT10(LP1,LP1),CO2PO(LP1,LP1), &
       CO2800(LP1,LP1),CO2PO1(LP1,LP1),CO2801(LP1,LP1),CO2PO2(LP1,LP1), &
       CO2802(LP1,LP1),N(LP1),D2CT8(LP1,LP1),D2CT10(LP1,LP1)
!CC   ITIN=22
!CC   ITIN1=23
!O222  LATEST CODE HAD  IQ=1
!CC      IQ=4
1011  FORMAT (4F20.14)
!CC      READ (ITIN,1011) ((CO2PO(I,J),I=1,LP1),J=1,LP1)
!CC      READ (ITIN1,1011) ((CO2800(I,J),I=1,LP1),J=1,LP1)
!CC      READ (ITIN,1011) ((CO2PO1(I,J),I=1,LP1),J=1,LP1)
!CC      READ (ITIN1,1011) ((CO2801(I,J),I=1,LP1),J=1,LP1)
!CC      READ (ITIN,1011) ((CO2PO2(I,J),I=1,LP1),J=1,LP1)
!CC      READ (ITIN1,1011) ((CO2802(I,J),I=1,LP1),J=1,LP1)
      DO 300 J=1,LP1
        DO 300 I=1,LP1
          CO2PO(I,J) = T22(I,J,1)
!NOV89
          IF (IQ.EQ.5) GO TO 300
!NOV89
          CO2PO1(I,J) = T22(I,J,2)
          CO2PO2(I,J) = T22(I,J,3)
  300 CONTINUE
      DO 301 J=1,LP1
        DO 301 I=1,LP1
          CO2800(I,J) = T23(I,J,1)
!NOV89
          IF (IQ.EQ.5) GO TO 301
!NOV89
          CO2801(I,J) = T23(I,J,2)
          CO2802(I,J) = T23(I,J,3)
  301 CONTINUE
!***THE FOLLOWING CODE IS REWRITTEN SO THAT THE RADIATIVE BANDS
!   ARE:
!        IQ=1    560-800     (CONSOL.=490-850)
!        IQ=2    560-670     (CONSOL.=490-670)
!        IQ=3    670-800     (CONSOL.=670-850)
!        IQ=4    560-760 (ORIGINAL CODE)   (CONSOL.=490-850)
!NOV89
!        IQ=5   2270-2380    (CONSOL.=2270-2380)
!NOV89
!  THE FOLLOWING LOOP OBTAINS TRANSMISSION FUNCTIONS FOR BANDS
!  USED IN RADIATIVE MODEL CALCULATIONS,WITH THE EQUIVALENT
!  WIDTHS KEPT FROM THE ORIGINAL CONSOLIDATED CO2 TF S.
!NOV89
!      NOTE: ALTHOUGH THE BAND TRANSMISSION FUNCTIONS ARE
!  COMPUTED FOR ALL RADIATIVE BANDS, AS OF 9/28/88, THEY
!  ARE WRITTEN OUT IN FULL ONLY FOR THE FULL 15 UM BAND CASES
!  (IQ=1,4).  IN OTHER CASES, THE TRANSMISSIVITIES (1,K) ARE
!  WRITTEN OUT, AS THESE ARE THE ONLY ONES NEEDED FOR CTS
!  CALCULATIONS.  ALSO, FOR THE 4.3 UM BAND (IQ=5) THE TEMP.
!  DERIVATIVE TERMS ARE NOT WRITTEN OUT, AS THEY ARE UNUSED.
!NOV89
      IF (IQ.EQ.1) THEN
         C1=1.5
         C2x=0.5
      ENDIF
      IF (IQ.EQ.2) THEN
        C1=18./11.
        C2x=7./11.
      ENDIF
      IF (IQ.EQ.3) THEN
        C1=18./13.
        C2x=5./13.
      ENDIF
      IF (IQ.EQ.4) THEN
        C1=1.8
        C2x=0.8
      ENDIF
!NOV89
      IF (IQ.EQ.5) THEN
        C1=1.0
        C2x=0.0
      ENDIF
!NOV89
      DO 1021 I=1,LP1
      DO 1021 J=1,LP1
      CO2PO(J,I)=C1*CO2PO(J,I)-C2x
      CO2800(J,I)=C1*CO2800(J,I)-C2x
!NOV89
      IF (IQ.EQ.5) GO TO 1021
!NOV89
      CO2PO1(J,I)=C1*CO2PO1(J,I)-C2x
      CO2801(J,I)=C1*CO2801(J,I)-C2x
      CO2PO2(J,I)=C1*CO2PO2(J,I)-C2x
      CO2802(J,I)=C1*CO2802(J,I)-C2x
1021  CONTINUE
!NOV89
      IF (IQ.GE.1.AND.IQ.LE.4) THEN
!NOV89
      DO 1 J=1,LP1
      DO 1 I=1,LP1
      DCDT8(I,J)=.02*(CO2801(I,J)-CO2802(I,J))*100.
      DCDT10(I,J)=.02*(CO2PO1(I,J)-CO2PO2(I,J))*100.
      D2CT8(I,J)=.0016*(CO2801(I,J)+CO2802(I,J)-2.*CO2800(I,J))*1000.
      D2CT10(I,J)=.0016*(CO2PO1(I,J)+CO2PO2(I,J)-2.*CO2PO(I,J))*1000.
1     CONTINUE
!NOV89
      ENDIF
!NOV89
!O222 *********************************************************
!CC       REWIND 66
!        SAVE CDT51,CO251,C2D51,CDT58,CO258,C2D58..ON TEMPO FILE
!CC       WRITE (66) DCDT10
!CC       WRITE (66) CO2PO
!CC       WRITE (66) D2CT10
!CC       WRITE (66) DCDT8
!CC       WRITE (66) CO2800
!CC       WRITE (66) D2CT8
!CC       REWIND 66
!NOV89
      IF (IQ.EQ.1.OR.IQ.EQ.4) THEN
!NOV89
      DO 400 J=1,LP1
       DO 400 I=1,LP1
        T66(I,J,1) = DCDT10(I,J)
        T66(I,J,2) = CO2PO(I,J)
        T66(I,J,3) = D2CT10(I,J)
        T66(I,J,4) = DCDT8(I,J)
        T66(I,J,5) = CO2800(I,J)
        T66(I,J,6) = D2CT8(I,J)
  400 CONTINUE
!NOV89
      ELSE
      DO 409 I=1,LP1
        T66(I,1,2) = CO2PO(1,I)
        T66(I,1,5) = CO2800(1,I)
        IF (IQ.EQ.5) GO TO 409
        T66(I,1,1) = DCDT10(1,I)
        T66(I,1,3) = D2CT10(1,I)
        T66(I,1,4) = DCDT8(1,I)
        T66(I,1,6) = D2CT8(1,I)
  409 CONTINUE
      ENDIF
!NOV89
!O222 *********************************************************
      RETURN
      END SUBROUTINE CO2INS
!O222 PROGRAM CO2INT(INPUT,TAPE5=INPUT)
!NOV89
      SUBROUTINE CO2INT(ITAPE,T15A,T15B,T22,RATIO,IR,NMETHD,NLEVLS,NLP1,NLP2)
!NOV89
!     *********************************************************
!       CHANGES TO DATA READ  AND FORMAT SEE CO222     ***
!          ..... K.CAMPANA   MARCH 1988,OCTOBER 1988
!       CHANGES TO PASS ITAPE,AND IF IR=4,READ 1 CO2 REC..KAC NOV89
!     *********************************************************
!       CO2INT INTERPOLATES CARBON DIOXIDE TRANSMISSION FUNCTIONS
!  FROM THE 109 LEVEL GRID,FOR WHICH THE TRANSMISSION FUNCTIONS
!  HAVE BEEN PRE-CALCULATED, TO THE GRID STRUCTURE SPECIFIED BY THE
!  USER.
!
!        METHOD:
!
!      CO2INT IS EMPLOYABLE FOR TWO PURPOSES: 1) TO OBTAIN TRANSMIS-
!  SIVITIES BETWEEN ANY 2 OF AN ARRAY OF USER-DEFINED PRESSURES; AND
!  2) TO OBTAIN LAYER-MEAN TRANSMISSIVITIES BETWEEN ANY 2 OF AN ARRAY
!  OF USER-DEFINED PRESSURE LAYERS.TO CLARIFY THESE TWO PURPOSES,SEE
!  THE DIAGRAM AND DISCUSSION BELOW.
!      CO2INT MAY BE USED TO EXECUTE ONLY ONE PURPOSE AT ONE TIME.
!
!     LET P BE AN ARRAY OF USER-DEFINED PRESSURES
!     AND PD BE USER-DEFINED PRESSURE LAYERS.
!
!       - - - - - - - - -   PD(I-1) ---
!                                     ^
!       -----------------   P(I)      ^  PRESSURE LAYER I  (PLM(I))
!                                     ^
!       - - - - - - - - -   PD(I)  ---
!                                     ^
!       -----------------   P(I+1)    ^  PRESSURE LAYER I+1 (PLM(I+1))
!                                     ^
!       - - - - - - - - -   PD(I+1)---
!            ...                          (THE NOTATION USED IS
!            ...                          CONSISTENT WITH THE CODE)
!            ...
!      - - - - - - - - -    PD(J-1)
!
!      -----------------    P(J)
!
!      - - - - - - - - -    PD(J)
!
!      PURPOSE 1:   THE TRANSMISSIVITY BETWEEN SPECIFIC PRESSURES
!      P(I) AND P(J) ,TAU(P(I),P(J))  IS COMPUTED BY THIS PROGRAM.
!      IN THIS MODE,THERE IS NO REFERENCE TO LAYER PRESSURES PD
!      (PD,PLM ARE NOT INPUTTED).
!
!      PURPOSE 2:   THE LAYER-MEAN TRANSMISSIVITY BETWEEN A LAYER-
!      MEAN PRESSURE PLM(J) AND PRESSURE LAYER I IS GIVEN BY
!         TAULM(PLM(I),PLM(J)). IT IS COMPUTED BY THE INTEGRAL
!
!                           PD(I)
!                           ----
!             1             ^
!        -------------  *   ^   TAU ( P',PLM(J) )  DP'
!        PD(I)-PD(I-1)      ^
!                        ----
!                        PD(I-1)
!
!           THE LAYER-MEAN PRESSURE PLM(I) IS SPECIFIED BY THE USER.
!        FOR MANY PURPOSES,PLM WILL BE CHOSEN TO BE THE AVERAGE
!        PRESSURE IN THE LAYER-IE,PLM(I)=0.5*(PD(I-1)+PD(I)).
!           FOR LAYER-MEAN TRANSMISSIVITIES,THE USER THUS INPUTS
!        A PRESSURE ARRAY (PD) DEFINING THE PRESSURE LAYERS AND AN
!        ARRAY (PLM) DEFINING THE LAYER-MEAN PRESSURES.THE CALCULATION
!        DOES NOT DEPEND ON THE P ARRAY USED FOR PURPOSE 1 (P IS NOT
!        INPUTTED).
!
!            THE FOLLOWING PARAGRAPHS DEPICT THE UTILIZATION OF THIS
!       CODE WHEN USED TO COMPUTE TRANSMISSIVITIES BETWEEN SPECIFIC
!       PRESSURES. LATER PARAGRAPHS DESCRIBE ADDITIONAL FEATURES NEEDED
!       FOR LAYER-MEAN TRANSMISSIVITIES.
!
!          FOR A GIVEN CO2 MIXING RATIO AND STANDARD TEMPERATURE
!      PROFILE,A TABLE OF TRANSMISSION FUNCTIONS FOR A FIXED GRID
!     OF ATMOSPHERIC PRESSURES HAS BEEN PRE-CALCULATED.
!      THE STANDARD TEMPERATURE PROFILE IS COMPUTED FROM THE US
!     STANDARD ATMOSPHERE (1977) TABLE.ADDITIONALLY, THE
!     SAME TRANSMISSION FUNCTIONS HAVE BEEN PRE-CALCULATED FOR A
!     TEMPERATURE PROFILE INCREASED AND DECREASED (AT ALL LEVELS)
!     BY 25 DEGREES.
!         THIS PROGRAM READS IN THE PRESPECIFIED TRANSMISSION FUNCTIONS
!     AND A USER-SUPPLIED PRESSURE GRID (P(I)) AND CALCULATES TRANS-
!     MISSION FUNCTIONS ,TAU(P(I),P(J)), FOR ALL P(I) S AND P(J) S.
!     A LOGARITHMIC INTERPOLATION SCHEME IS USED.
!         THIS METHOD IS REPEATED FOR THE THREE TEMPERATURE PROFILES
!     GIVEN ABOVE .THEREFORE OUTPUTS FROM THE PROGRAM ARE THREE TABLES
!     OF TRANSMISSION FUNCTIONS FOR THE USER-SUPPLIED PRESSURE GRID.
!     THE EXISTENCE OF THE THREE TABLES PERMITS SUBSEQUENT INTERPO-
!     LATION TO A USER-SUPPLIED TEMPERATURE PROFILE USING THE METHOD
!     DESCRIBED IN THE REFERENCE.SEE LIMITATIONS SECTION IF THE
!     USER DESIRES TO OBTAIN ONLY 1 TABLE OF TRANSMISSIVITIES.
!
!     MODIFICATIONS FOR LAYER-MEAN TRANSMISSIVITIES:
!          THE PRESSURES INPUTTED ARE THE LAYER-MEAN PRESSURES,PD,
!     AND THE LAYER-MEAN PRESSURES ,PLM. A SERIES OF TRANSMISSIVITIES
!     (TAU(P'',PLM(J)) ARE COMPUTED AND THE INTEGRAL GIVEN IN THE
!     DISCUSSION OF PURPOSE 2 IS COMPUTED.FOR PLM(I) NOT EQUAL TO
!     PLM(J) SIMPSON S RULE IS USED WITH 5 POINTS. IF PLM(I)=PLM(J)
!     (THE -NEARBY LAYER- CASE) A 49-POINT QUADRATURE IS USED FOR
!     GREATER ACCURACY.THE OUTPUT IS IN TAULM(PLM(I),PLM(J)).
!        NOTE:
!     TAULM IS NOT A SYMMETRICAL MATRIX. FOR THE ARRAY ELEMENT
!     TAULM(PLM(I),PLM(J)),THE INNER(FIRST,MOST RAPIDLY VARYING)
!     DIMENSION IS THE VARYING LAYER-MEAN PRESSURE,PLM(I);THE OUTER
!     (SECOND) DIMENSION IS THE FIXED LAYER-MEAN PRESSURE PLM(J).
!     THUS THE ELEMENT TAULM(2,3) IS THE TRANSMISSION FUNCTION BETWEEN
!     THE FIXED PRESSURE PLM(3)  AND THE PRESSURE LAYER HAVING AN AVERAG
!     PRESSURE OF PLM(2).
!         ALSO NOTE THAT NO QUADRATURE IS PERFORMED OVER THE LAYER
!     BETWEEN THE SMALLEST NONZERO PRESSURE AND ZERO PRESSURE;
!     TAULM IS TAULM(0,PLM(J)) IN THIS CASE,AND TAULM(0,0)=1.
!
!
!             REFERENCE:
!         S.B.FELS AND M.D.SCHWARZKOPF,-AN EFFICIENT ACCURATE
!     ALGORITHM FOR CALCULATING CO2 15 UM BAND COOLING RATES-,JOURNAL
!     OF GEOPHYSICAL RESEARCH,VOL.86,NO. C2, PP.1205-1232,1981.
!        MODIFICATIONS TO THE ALGORITHM HAVE BEEN MADE BY THE AUTHORS;
!     CONTACT S.B.F.OR M.D.S. FOR FURTHER DETAILS.A NOTE TO J.G.R.
!     IS PLANNED TO DOCUMENT THESE CHANGES.
!
!            AUTHOR:    M.DANIEL SCHWARZKOPF
!
!            DATE:      14 JULY 1983
!
!            ADDRESS:
!
!                      G.F.D.L.
!                      P.O.BOX 308
!                      PRINCETON,N.J.08540
!                      U.S.A.
!            TELEPHONE:  (609) 452-6521
!
!            INFORMATION ON TAPE: THIS SOURCE IS THE FIRST FILE
!        ON THIS TAPE.THE SIX FILES THAT FOLLOW ARE CO2 TRANS-
!        MISSIVITIES FOR THE 500-850 CM-1 INTERVAL FOR CO2
!        CONCENTRATIONS OF 330 PPMV (1X) ,660 PPMV (2X), AND
!        1320 PPMV (4X). THE FILES ARE ARRANGED AS FOLLOWS:
!          FILE 2   1X,CONSOLIDATED USING B(250) WEIGHTING FCTN.
!          FILE 3   1X,CONSOLIDATED WITH NO WEIGHTING FCTN.
!          FILE 4   2X,CONSOLIDATED USING B(250) WEIGHTING FCTN.
!          FILE 5   2X,CONSOLIDATED WITH NO WEIGHTING FCTN.
!          FILE 6   4X,CONSOLIDATED USING B(250) WEIGHTING FCTN.
!          FILE 7   4X,CONSOLIDATED WITH NO WEIGHTING FCTN.
!            FILES 2,4,6 ARE RECOMMENDED FOR USE IN OBTAINING
!        TRANSMISSION FUNCTIONS FOR USE IN HEATING RATE
!        COMPUTATIONS;THEY CORRESPOND TO THE TRANSMISSIVITIES
!        DISCUSSED IN THE 1980 PAPER.FILES 3,5,7 ARE PROVIDED
!        TO FACILITATE COMPARISON WITH OBSERVATION AND WITH OTHER
!        CALCULATIONS.
!
!            PROGRAM LANGUAGE: FORTRAN 1977,INCLUDING PARAMETER
!        AND PROGRAM STATEMENTS.THE PROGRAM IS WRITTEN ON A
!        CYBER 170-730.SEE THE SECTION ON LIMITATIONS FOR
!        ADAPTATIONS TO OTHER MACHINES.
!
!          INPUT UNITS,FORMATS AND FORMAT STATEMENT NOS:
!
!   UNIT NO    VARIABLES       FORMAT      STATEMENT NO.    TYPE
!      5        P (PURPOSE 1)  (5E16.9)        201         CARDS
!      5        PD (PURPOSE 2) (5E16.9)        201         CARDS
!      5        PLM(PURPOSE 2) (5E16.9)        201         CARDS
!      5        NMETHD         (I3)            202         CARDS
!      20       TRANSA         (4F20.14)       102          TAPE
!NOV89
!      ITAPE    TRANSA         (4F20.14)       102          TAPE
!NOV89
!
!         OUTPUT UNITS,FORMATS AND FORMAT STATEMENT NOS:
!
!   UNIT NO    VARIABLES       FORMAT     STATEMENT NO.
!      6         TRNFCT        (1X,8F15.8)     301         PRINT
!      22        TRNFCT        (4F20.14)       102          TAPE
!
!            PARAMETER INPUTS:
!     A) NLEVLS    : NLEVLS IS AN (INTEGER) PARAMETER DENOTING
!        THE NUMBER OF NONZERO PRESSURE LEVELS FOR PURPOSE 1
!        OR THE NUMBER OF NONZERO LAYER PRESSURES NEEDED TO
!        SPECIFY THE PRESSURE LAYERS(PURPOSE 2) IN THE OUTPUT
!        GRID. FOR EXAMPLE,IN PURPOSE 1,IF P=0,100,1000,NLEVLS=2.
!        IF,IN PURPOSE 2,PD=0,100,500,1000,THE NUMBER OF NONZERO
!        PRESSURE LAYERS=2,SO NLEVLS=2
!           IN THE CODE AS WRITTEN,NLEVLS=40; THE USER SHOULD
!        CHANGE THIS VALUE TO A USER-SPECIFIED VALUE.
!     B) NLP1,NLP2 : INTEGER PARAMETERS DEFINED AS: NLP1=NLEVLS+1;
!        NLP2=NLEVLS+2.
!           SEE LIMITATIONS FOR CODE MODIFICATIONS IF PARAMETER
!        STATEMENTS ARE NOT ALLOWED ON YOUR MACHINE.
!
!            INPUTS:
!
!     A) TRANSA    : THE 109X109 GRID OF TRANSMISSION FUNCTIONS
!            TRANSA IS A  DOUBLE PRECISION REAL ARRAY.
!
!           TRANSA  IS READ FROM FILE 20. THIS FILE CONTAINS 3
!     RECORDS,AS FOLLOWS:
!        1)   TRANSA, STANDARD TEMPERATURE PROFILE
!        3)   TRANSA, STANDARD TEMPERATURES + 25 DEG
!        5)   TRANSA, STANDARD TEMPERATURES - 25 DEG
!
!     B)   NMETHD: AN INTEGER WHOSE VALUE IS EITHER 1 (IF CO2INT IS
!       TO BE USED FOR PURPOSE 1) OR 2 (IF CO2INT IS TO BE USED FOR
!       PURPOSE 2).
!
!     C)     P,PD,PLM :
!          P IS A REAL ARRAY (LENGTH NLP1) SPECIFYING THE PRESSURE
!       GRID AT WHICH TRANSMISSION FUNCTIONS ARE TO BE COMPUTED FOR
!       PURPOSE 1.THE DIMENSION  OF P IS  IN MILLIBARS.THE
!       FOLLOWING LIMITATIONS WILL BE EXPLAINED MORE
!       IN THE SECTION ON LIMITATIONS: P(1) MUST BE ZERO; P(NLP1),THE
!       LARGEST PRESSURE, MUST NOT EXCEED 1165 MILLIBARS.
!         PD IS A REAL ARRAY (LENGTH NLP2) SPECIFYING THE PRESSURE
!       LAYERS FOR WHICH LAYER-AVERAGED TRANSMISSION FUNCTIONS ARE
!       TO BE COMPUTED.THE DIMENSION OF PD IS MILLIBARS.THE LIMITATIONS
!       FOR PD ARE THE SAME AS FOR P,AND ARE GIVEN IN THE SECTION ON
!       LIMITATIONS.
!         PLM IS A REAL ARRAY (LENGTH NLP2) SPECIFYING THE LAYER-MEAN
!       PRESSURES. THE DIMENSION OF PLM IS MILLIBARS. THE LIMITATIONS
!       FOR PLM ARE THE SAME AS FOR P,AND ARE GIVEN IN THE SECTION ON
!       LIMITATIONS.PD IS READ IN BEFORE PLM.
!
!          NOTE: AGAIN,WE NOTE THAT THE USER WILL INPUT EITHER P (FOR
!       PURPOSE 1) OR PD AND PLM(FOR PURPOSE 2) BUT NOT BOTH.
!
!
!
!
!           LIMITATIONS:
!     1)       P(1)=0.,PD(1)=0.,PLM(1)=0. THE TOP PRESSURE LEVEL
!       MUST BE ZERO,OR THE TOP PRESSURE LAYER MUST BE BOUNDED BY ZERO.
!       THE TOP LAYER-MEAN PRESSURE (PLM(1)) MUST BE ZERO; NO
!       QUADRATURE IS DONE ON THE TOP PRESSURE LAYER.EVEN IF ONE IS
!       NOT INTERESTED IN THE TRANSMISSION FUNCTION BETWEEN 0 AND P(J),
!       ONE MUST INCLUDE SUCH A LEVEL.
!     2)      PD(NLP2)=P(NLP1) IS LESS THAN OR EQUAL TO 1165 MB.
!       EXTRAPOLATION TO HIGHER PRESSURES IS NOT POSSIBLE.
!     3)      IF PROGRAM IS NOT PERMITTED ON YOUR COMPILER,
!       SIMPLY DELETE THE LINE.
!     4)      IF PARAMETER IS NOT PERMITTED,DO THE FOLLOWING:
!            1) DELETE ALL PARAMETER STATEMENTS IN CO2INT
!            2) AT THE POINT WHERE NMETHOD IS READ IN,ADD:
!                READ (5,202) NLEVLS
!                NLP1=NLEVLS+1
!                NLP2=NLEVLS+2
!            3) CHANGE DIMENSION AND/OR COMMON STATEMENTS DEFINING
!              ARRAYS TRNS,DELTA,P,PD,TRNFCT,PS,PDS,PLM IN CO2INT.
!              THE NUMERICAL VALUE OF (NLEVLS+1) SHOULD BE INSERTED
!              IN DIMENSION OR COMMON STATEMENTS FOR TRNS,DELTA,
!              P,TRNFCT,PS,PLM; THE NUMERICAL VALUE OF (NLEVLS+2)
!              IN DIMENSION OR COMMON STATEMENTS FOR PD,PDS.
!      5)    PARAMETER (NLEVLS=40) AND THE OTHER PARAMETER
!       STATEMENTS ARE WRITTEN IN CDC FORTRAN; ON OTHER MACHINES THE
!       SAME STATEMENT MAY BE WRITTEN DIFFERENTLY,FOR EXAMPLE AS
!       PARAMETER   NLEVLS=40
!      6) -DOUBLE PRECISION- IS USED INSTEAD OF -REAL*8- ,DUE TO
!       REQUIREMENTS OF CDC FORTAN.
!      7) THE STATEMENT -DO 400 KKK=1,3- CONTROLS THE NUMBER OF
!       TRANSMISSIVITY OUTPUT MATRICES PORDUCED BY THE PROGRAM.TO
!       PRODUCE 1 OUTPUT MATRIX,DELETE THIS STATEMENT.
!
!     OUTPUT:
!         A) TRNFCT IS AN (NLP1,NLP1) REAL ARRAY OF THE TRANSMISSION
!     FUNCTIONS APPROPRIATE TO YOUR ARRAY. IT IS TO BE SAVED ON FILE 22.
!     THE PROCEDURE FOR SAVING MAY BE MODIFIED; AS GIVEN HERE,THE
!     OUTPUT IS IN CARD IMAGE FORM WITH A FORMAT OF (4F20.14).
!
!         B)  PRINTED  OUTPUT IS A LISTING OF TRNFCT ON UNIT 6, IN
!     THE FORMAT (1X,8F15.8) (FORMAT STATEMENT 301). THE USER MAY
!     MODIFY OR ELIMINATE THIS AT WILL.
!
!      ************   FUNCTION INTERPOLATER ROUTINE  *****************
!
!
!     ******   THE FOLLOWING PARAMETER GIVES THE NUMBER OF     *******
!     ******           DATA LEVELS IN THE MODEL                *******
!     ****************************************************************
!     ****************************************************************
      COMMON/INPUT/P1,P2,TRNSLO,IA,JA,N
!     COMMON/PRESS/PA(109)
!     COMMON/TRAN/ TRANSA(109,109)
!     COMMON / OUTPUT / TRNS(NLP1,NLP1)
!     COMMON/INPUTP/P(NLP1),PD(NLP2)
      DIMENSION TRNS(NLP1,NLP1)
      DIMENSION P(NLP1),PD(NLP2)
      DIMENSION PS(NLP1),PDS(NLP2),PLM(NLP1)
      DIMENSION NRTAB(3)
      DIMENSION T15A(NLP2,2),T15B(NLP1)
      DIMENSION T22(NLP1,NLP1,3)
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
      DATA NRTAB/1,2,4/
!***********************************
!   THE FOLLOWING ARE THE INPUT FORMATS
100   FORMAT (4F20.14)
743   FORMAT (F20.14)
201   FORMAT (5E16.9)
202   FORMAT (I3)
!O222   203   FORMAT (F12.6,I2)
203   FORMAT (F12.6)
!    THE FOLLOWING ARE THE OUTPUT FORMATS
102   FORMAT (4F20.14)
301   FORMAT (1X,8F15.8)
!
!CC   REWIND 15
!CC   REWIND 20
!NOV89
      REWIND ITAPE
!NOV89
!CC   REWIND 22
!
!     CALCULATION OF PA -THE -TABLE- OF 109 GRID PRESSURES
!     NOTE-THIS CODE MUST NOT BE CHANGED BY THE USER^^^^^^^^^
      PA(1)=0.
      FACT15=10.**(1./15.)
      FACT30=10.**(1./30.)
      PA(2)=1.0E-3
      DO 231 I=2,76
      PA(I+1)=PA(I)*FACT15
231   CONTINUE
      DO 232 I=77,108
      PA(I+1)=PA(I)*FACT30
232   CONTINUE
!
      N=25
      NLV=NLEVLS
      NLP1V=NLP1
      NLP2V=NLP2
!     READ IN THE CO2 MIXING RATIO(IN UNITS OF 330 PPMV),AND AN INDEX
!     GIVING THE FREQUENCY RANGE OF THE LBL DATA
!O222    READ (5,203) RATIO,IR
!CC         IR = 1
!CC         READ (5,203) RATIO
!O222   ***********************************
!***VALUES FOR IR*****
!          IR=1     CONSOL. LBL TRANS. =490-850
!          IR=2     CONSOL. LBL TRANS. =490-670
!          IR=3     CONSOL. LBL TRANS. =670-850
!          IR=4     CONSOL. LBL TRANS. =2270-2380
!*** IR MUST BE 1,2,3 OR 4 FOR THE PGM. TO WORK
!     ALSO READ IN THE METHOD NO.(1 OR 2)
!CC         READ (5,202) NMETHD
      IF (RATIO.EQ.1.0) GO TO 621
      CALL wrf_error_fatal( 'SUBROUTINE CO2INT: 8746' )
!NOV89  621   ITAP1=20
621   ITAP1=ITAPE
!NOV89
      NTAP=1
      IF (NMETHD.EQ.2) GO TO 502
!   *****CARDS FOR PURPOSE 1(NMETHD=1)
!CC         READ (15,201) (P(I),I=1,NLP1)
      DO 300 I=1,NLP1
        P(I)=T15B(I)
  300 CONTINUE
      DO 801 I=1,NLP1
      PS(I)=P(I)
801   CONTINUE
      GO TO 503
502   CONTINUE
!  *****CARDS FOR PURPOSE 2(NMETHD=2)
!CC         READ (15,201) (PD(I),I=1,NLP2)
!CC         READ (15,201) (PLM(I),I=1,NLP1)
      DO 303 I=1,NLP2
        PD(I)=T15A(I,1)
  303 CONTINUE
      DO 302 I=1,NLP1
        PLM(I)=T15A(I,2)
  302 CONTINUE
      DO 802 I=1,NLP1
      PDS(I)=PD(I+1)
      PS(I)=PLM(I)
802   CONTINUE
!
503   CONTINUE
!  *****DO LOOP CONTROLLING NUMBER OF OUTPUT MATRICES
!NOV89
!NOV89    DO 400 KKK=1,3
      ICLOOP = 3
      IF (IR.EQ.4) ICLOOP = 1
      DO 400 KKK=1,ICLOOP
!NOV89
!  **********************
      IF (NMETHD.EQ.2) GO TO 505
!   *****CARDS FOR PURPOSE 1(NMETHD=1)
      DO 803 I=1,NLP1
      P(I)=PS(I)
803   CONTINUE
      GO TO 506
505   CONTINUE
!  *****CARDS FOR PURPOSE 2(NMETHD=2)
      DO 804 I=1,NLP1
      PD(I)=PDS(I)
      P(I)=PS(I)
804   CONTINUE
!
506   CONTINUE
      IA=108
      IAP=IA+1
!NOV89   IF (NTAP.EQ.1) READ (20,100) ((TRANSA(I,J),I=1,109),J=1,109)
!mp       IF (NTAP.EQ.1) READ (ITAPE,100) ((TRANSA(I,J),I=1,109),J=1,109)
        IF (NTAP.EQ.1) THEN
           IF ( wrf_dm_on_monitor() ) READ (ITAPE,743) ((TRANSA(I,J),I=1,109),J=1,109)
           CALL wrf_dm_bcast_bytes ( TRANSA , size ( TRANSA ) * RWORDSIZE )
        ENDIF
!mp	IF (NTAP.EQ.1) READ (ITAPE,100) (tmp(I),I=1,11881
!mp
	do J=109,1,-6
!mp	write(6,697)(TRANSA(I,J),I=5,105,10)
	enddo
 697	format(11(f5.3,1x))
!mp
!NOV89
      DO 4 I=1,IAP
      TRANSA(I,I)=1.0
    4 CONTINUE
      CALL COEINT(RATIO,IR)
      DO 805 I=1,NLP1
      DO 805 J=1,NLP1
      TRNS(J,I)=1.00
805   CONTINUE
      DO 10 I=1,NLP1
      DO 20 J=1,I
      IF (I.EQ.J) GO TO 20
      P1=P(J)
      P2=P(I)
      CALL SINTR2
      TRNS(J,I)=TRNSLO
20    CONTINUE
10    CONTINUE
      DO 47 I=1,NLP1
      DO 47 J=I,NLP1
      TRNS(J,I)=TRNS(I,J)
47    CONTINUE
!  *****THIS IS THE END OF PURPOSE 1 CALCULATIONS
      IF (NMETHD.EQ.1) GO TO 2872
!
      DO 51 J=1,NLP1
      DO 52 I=2,NLP1
      IA=I
      JA=J
      N=25
      IF (I.NE.J) N=3
      CALL QUADSR(NLV,NLP1V,NLP2V,P,PD,TRNS)
52    CONTINUE
51    CONTINUE
!  *****THIS IS THE END OF PURPOSE 2 CALCULATIONS
2872  CONTINUE
!
!+++  WRITE (6,301) ((TRNS(I,J),I=1,NLP1),J=1,NLP1)
!CC         WRITE (22,102) ((TRNS(I,J),I=1,NLP1),J=1,NLP1)
      DO 304 J=1,NLP1
       DO 304 I=1,NLP1
        T22(I,J,KKK) = TRNS(I,J)
  304 CONTINUE
400   CONTINUE
      RETURN
      END SUBROUTINE CO2INT
!CCC  PROGRAM CO2IN1
      SUBROUTINE CO2IN1(T20,T21,T66,IQ,L,LP1)
!    CO2IN1=CO2INS FOR METHOD 1
!     *********************************************************
!       SAVE DATA ON PERMANENT DATA SET DENOTED BY CO222 ***
!          ..... K.CAMPANA   MARCH 1988,OCTOBER 1988
!          ..... K.CAMPANA   DECEMBER 88 CLEANED UP FOR LAUNCHER
!     *********************************************************
      DIMENSION T20(LP1,LP1,3),T21(LP1,LP1,3),T66(L,6)
      DIMENSION DCDT8(LP1,LP1),DCDT10(LP1,LP1),CO2PO(LP1,LP1), &
       CO2800(LP1,LP1),CO2PO1(LP1,LP1),CO2801(LP1,LP1),CO2PO2(LP1,LP1), &
       CO2802(LP1,LP1),N(LP1),D2CT8(LP1,LP1),D2CT10(LP1,LP1)
      ITIN=20
      ITIN1=21
!O222 LATEST CODE HAS IQ=1
!CC         IQ=4
1011  FORMAT (4F20.14)
!CC        READ (ITIN,1011) ((CO2PO(I,J),I=1,LP1),J=1,LP1)
!CC        READ (ITIN1,1011) ((CO2800(I,J),I=1,LP1),J=1,LP1)
!CC        READ (ITIN,1011) ((CO2PO1(I,J),I=1,LP1),J=1,LP1)
!CC        READ (ITIN1,1011) ((CO2801(I,J),I=1,LP1),J=1,LP1)
!CC        READ (ITIN,1011) ((CO2PO2(I,J),I=1,LP1),J=1,LP1)
!CC        READ (ITIN1,1011) ((CO2802(I,J),I=1,LP1),J=1,LP1)
      DO 300 J=1,LP1
        DO 300 I=1,LP1
          CO2PO(I,J) = T20(I,J,1)
!NOV89
          IF (IQ.EQ.5) GO TO 300
!NOV89
          CO2PO1(I,J) = T20(I,J,2)
          CO2PO2(I,J) = T20(I,J,3)
  300 CONTINUE
      DO 301 J=1,LP1
        DO 301 I=1,LP1
          CO2800(I,J) = T21(I,J,1)
!NOV89
          IF (IQ.EQ.5) GO TO 301
!NOV89
          CO2801(I,J) = T21(I,J,2)
          CO2802(I,J) = T21(I,J,3)
  301 CONTINUE
!***THE FOLLOWING CODE IS REWRITTEN SO THAT THE RADIATIVE BANDS
!   ARE:
!        IQ=1    560-800     (CONSOL.=490-850)
!        IQ=2    560-670     (CONSOL.=490-670)
!        IQ=3    670-800     (CONSOL.=670-850)
!        IQ=4    560-760 (ORIGINAL CODE)   (CONSOL.=490-850)
!NOV89
!        IQ=5   2270-2380    (CONSOL.=2270-2380)
!NOV89
!  THE FOLLOWING LOOP OBTAINS TRANSMISSION FUNCTIONS FOR BANDS
!  USED IN RADIATIVE MODEL CALCULATIONS,WITH THE EQUIVALENT
!  WIDTHS KEPT FROM THE ORIGINAL CONSOLIDATED CO2 TF S.
      IF (IQ.EQ.1) THEN
         C1=1.5
         C2x=0.5
      ENDIF
      IF (IQ.EQ.2) THEN
        C1=18./11.
        C2x=7./11.
      ENDIF
      IF (IQ.EQ.3) THEN
        C1=18./13.
        C2x=5./13.
      ENDIF
      IF (IQ.EQ.4) THEN
        C1=1.8
        C2x=0.8
      ENDIF
!NOV89
      IF (IQ.EQ.5) THEN
        C1=1.0
        C2x=0.0
      ENDIF
!NOV89
      DO 1021 I=1,LP1
      DO 1021 J=1,LP1
      CO2PO(J,I)=C1*CO2PO(J,I)-C2x
      CO2800(J,I)=C1*CO2800(J,I)-C2x
!NOV89
      IF (IQ.EQ.5) GO TO 1021
!NOV89
      CO2PO1(J,I)=C1*CO2PO1(J,I)-C2x
      CO2801(J,I)=C1*CO2801(J,I)-C2x
      CO2PO2(J,I)=C1*CO2PO2(J,I)-C2x
      CO2802(J,I)=C1*CO2802(J,I)-C2x
1021  CONTINUE
!NOV89
      IF (IQ.GE.1.AND.IQ.LE.4) THEN
!NOV89
      DO 1 J=1,LP1
      DO 1 I=1,LP1
      DCDT8(I,J)=.02*(CO2801(I,J)-CO2802(I,J))*100.
      DCDT10(I,J)=.02*(CO2PO1(I,J)-CO2PO2(I,J))*100.
      D2CT8(I,J)=.0016*(CO2801(I,J)+CO2802(I,J)-2.*CO2800(I,J))*1000.
      D2CT10(I,J)=.0016*(CO2PO1(I,J)+CO2PO2(I,J)-2.*CO2PO(I,J))*1000.
1     CONTINUE
!NOV89
      ENDIF
!NOV89
!O222 *********************************************************
!CC          REWIND 66
!        SAVE CDTM51,CO2M51,C2DM51,CDTM58,CO2M58,C2DM58..ON TEMPO FILE
!CC          WRITE (66) (DCDT10(I,I+1),I=1,L)
!CC          WRITE (66) (CO2PO(I,I+1),I=1,L)
!CC          WRITE (66) (D2CT10(I,I+1),I=1,L)
!CC          WRITE (66) (DCDT8(I,I+1),I=1,L)
!CC          WRITE (66) (CO2800(I,I+1),I=1,L)
!CC          WRITE (66) (D2CT8(I,I+1),I=1,L)
!CC          REWIND 66
!O222 *********************************************************
      DO 400 I=1,L
        T66(I,2) = CO2PO(I,I+1)
        T66(I,5) = CO2800(I,I+1)
!NOV89
        IF (IQ.EQ.5) GO TO 400
!NOV89
        T66(I,1) = DCDT10(I,I+1)
        T66(I,3) = D2CT10(I,I+1)
        T66(I,4) = DCDT8(I,I+1)
        T66(I,6) = D2CT8(I,I+1)
  400 CONTINUE
      RETURN
      END SUBROUTINE CO2IN1
!CCC  PROGRAM PTZ - COURTESY OF DAN SCHWARZKOPF,GFDL DEC 1987....
      SUBROUTINE CO2PTZ(SGTEMP,T41,T42,T43,T44,SGLVNU,SIGLNU, &
                        SFULL,SHALF,PPTOP,LREAD,NL,NLP,NLP2)
!
! **         THIS PROGRAM CALCULATES TEMPERATURES ,H2O MIXING RATIOS
! **         AND O3 MIXING RATIOS BY USING AN ANALYTICAL
! **         FUNCTION WHICH APPROXIMATES
! **         THE US STANDARD (1976).  THIS IS
! **         CALCULATED IN FUNCTION 'ANTEMP', WHICH IS CALLED BY THE
! **         MAIN PROGRAM.  THE FORM OF THE ANALYTICAL FUNCTION WAS
! **         SUGGESTED TO ME IN 1971 BY RICHARD S. LINDZEN.
! ******************************************************************
!         CODE TO SAVE STEMP,GTEMP ON DATA SET,BRACKETED BY CO222  **
!             ....K. CAMPANA MARCH 88,OCTOBER 88
      DIMENSION SGTEMP(NLP,2),T41(NLP2,2),T42(NLP), &
                T43(NLP2,2),T44(NLP)
      DIMENSION SGLVNU(NLP),SIGLNU(NL)
      DIMENSION SFULL(NLP),SHALF(NL)
! ******************************************************************
!
!*****THIS VERSION IS ONLY USABLE FOR 1976 US STD ATM AND OBTAINS
!     QUANTITIES FOR CO2 INTERPOLATION AND INSERTION INTO OPERA-
!     TIONAL RADIATION CODES
!
      CHARACTER*20 PROFIL
      DIMENSION PRESS(NLP),TEMP(NLP),ALT(NLP),WMIX(NLP),O3MIX(NLP)
      DIMENSION WMXINT(NLP,4),WMXOUT(NLP2),OMXINT(NLP,4),OMXOUT(NLP2)
      DIMENSION PD(NLP2),GTEMP(NLP)
      DIMENSION PRS(NLP),TEMPS(NLP),PRSINT(NLP),TMPINT(NLP,4),A(NLP,4)
      DIMENSION PROUT(NLP2),TMPOUT(NLP2),TMPFLX(NLP2),TMPMID(NLP2)
!
!
      DATA PROFIL/ &
         'US STANDARD 1976'/
      DATA PSMAX/1013.250/
!
! **         NTYPE IS AN INTEGER VARIABLE WHICH HAS THE FOLLOWING
! **        VALUES:    0 =SIGMA LEVELS ARE USED;   1= SKYHI L40 LEVELS
! **        ARE USED;   2 = SKYHI L80 LEVELS ARE USED. DEFAULT: 0
!
      NTYPE=0
!O222 READ (*,*) NTYPE
    5 NLEV=NL
      DELZAP=0.5
      R=8.31432
      G0=9.80665
      ZMASS=28.9644
      AA=6356.766
         ALT(1)=0.0
         TEMP(1)=ANTEMP(6,0.0)
!*******DETERMINE THE PRESSURES (PRESS)
      PSTAR=PSMAX
!
!***  LTOP COMPUTATION MOVED FROM MODEL INITIALIZATION
!
      LTOP(1)=0
      LTOP(2)=0
      LTOP(3)=0
      DO 30 N=1,NL
        PCLD=(PSTAR-PPTOP*10.)*SHALF(N)+PPTOP*10.
        IF(PCLD.GE.642.)LTOP(1)=N
        IF(PCLD.GE.350.)LTOP(2)=N
        IF(PCLD.GE.150.)LTOP(3)=N
!       PRINT *,N,PCLD,SHALF(N),PSTAR,PPTOP
   30 CONTINUE
!
!O222 IF (NTYPE.EQ.1) CALL SKYP(PSTAR,PD,GTEMP)
!O222 IF (NTYPE.EQ.2) CALL SKY80P(PSTAR,PD,GTEMP)
!O222 IF (NTYPE.EQ.0) CALL SIGP(PSTAR,PD,GTEMP)
!CC----      CALL SIGP(PSTAR,PD,GTEMP)
      NLM=NL-1
      CALL SIGP(PSTAR,PD,GTEMP,T41,T42,T43,T44,SGLVNU,SIGLNU, &
                SFULL,SHALF,PPTOP,LREAD,NL,NLP,NLM,NLP2)
      PD(NLP2)=PSTAR
      DO 40 N=1,NLP
      PRSINT(N)=PD(NLP2+1-N)
 40   CONTINUE
!    *** CALCULATE TEMPS FOR SEVERAL PRESSURES TO DO QUADRATURE
      DO 504 NQ=1,4
      DO 505 N=2,NLP
 505  PRESS(N)=PRSINT(N)+0.25*(NQ-1)*(PRSINT(N-1)-PRSINT(N))
      PRESS(1)=PRSINT(1)
!*********************
      DO 100 N=1,NLEV
!
! **         ESTABLISH COMPUTATATIONAL LEVELS BETWEEN USER LEVELS AT
! **         INTERVALS OF APPROXIMATELY 'DELZAP' KM.
!
      DLOGP=7.0*ALOG(PRESS(N)/PRESS(N+1))
      NINT=DLOGP/DELZAP
      NINT=NINT+1
      ZNINT=NINT
!     G=G0
      DZ=R*DLOGP/(7.0*ZMASS*G0*ZNINT)
      HT=ALT(N)
!
! **         CALCULATE HEIGHT AT NEXT USER LEVEL BY MEANS OF
! **                   RUNGE-KUTTA INTEGRATION.
!
      DO 200 M=1,NINT
      RK1=ANTEMP(6,HT)*DZ
      RK2=ANTEMP(6,HT+0.5*RK1)*DZ
      RK3=ANTEMP(6,HT+0.5*RK2)*DZ
      RK4=ANTEMP(6,HT+RK3)*DZ
!mp	write(6,*) 'RK values,DZ ', RK1,RK2,RK3,RK4,DZ
      HT=HT+0.16666667*(RK1+RK2+RK2+RK3+RK3+RK4)
  200 CONTINUE
      ALT(N+1)=HT
      TEMP(N+1)=ANTEMP(6,HT)
  100 CONTINUE
      DO 506 N=1,NLP
      TMPINT(N,NQ)=TEMP(N)
      A(N,NQ)=ALT(N)
506   CONTINUE
504   CONTINUE
!O222   *****************************************************
!***OUTPUT TEMPERATURES
!O222   *****************************************************
      DO 901 N=1,NLP
        SGTEMP(N,1) = TMPINT(NLP2-N,1)
  901 CONTINUE
!O222   *****************************************************
!***OUTPUT GTEMP
!O222   *****************************************************
      DO 902 N=1,NLP
        SGTEMP(N,2) = GTEMP(N)
  902 CONTINUE
!O222   *****************************************************
      RETURN
      END SUBROUTINE CO2PTZ
      FUNCTION PATH(A,B,C,E)
!....
!     DOUBLE PRECISION XA,CA
!     COMMON/COEFS/XA(109),CA(109),ETA(109),SEXPV(109),CORE,UEXP,SEXP
      PEXP=1./SEXP
      PATH=((A-B)**PEXP*(A+B+C))/(E*(A+B+C)+(A-B)**(PEXP-1.))
      RETURN
      END FUNCTION PATH
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE QINTRP(XM,X0,XP,FM,F0,FP,X,F)
!....
!     DOUBLE PRECISION FM,F0,FP,F,D1,D2,B,A,DEL
      D1=(FP-F0)/(XP-X0)
      D2=(FM-F0)/(XM-X0)
      B=(D1-D2)/(XP-XM)
      A=D1-B*(XP-X0)
      DEL=(X-X0)
      F=F0+DEL*(A+DEL*B)
      RETURN
      END SUBROUTINE QINTRP
      SUBROUTINE QUADSR(NLV,NLP1V,NLP2V,P,PD,TRNS)
      COMMON/INPUT/P1,P2,TRNSLO,IA,JA,N
      DIMENSION P(NLP1V),PD(NLP2V),TRNS(NLP1V,NLP1V)
      DIMENSION WT(101)
      N2=2*N
      N2P=2*N+1
!  *****WEIGHTS ARE CALCULATED
      WT(1)=1.
      DO 21 I=1,N
      WT(2*I)=4.
      WT(2*I+1)=1.
21    CONTINUE
      IF (N.EQ.1) GO TO 25
      DO 22 I=2,N
      WT(2*I-1)=2.
22    CONTINUE
25    CONTINUE
      TRNSNB=0.
      DP=(PD(IA)-PD(IA-1))/N2
      PFIX=P(JA)
      DO 1 KK=1,N2P
      PVARY=PD(IA-1)+(KK-1)*DP
      IF (PVARY.GE.PFIX) P2=PVARY
      IF (PVARY.GE.PFIX) P1=PFIX
      IF (PVARY.LT.PFIX) P1=PVARY
      IF (PVARY.LT.PFIX) P2=PFIX
      CALL SINTR2
      TRNSNB=TRNSNB+TRNSLO*WT(KK)
1     CONTINUE
      TRNS(IA,JA)=TRNSNB*DP/(3.*(PD(IA)-PD(IA-1)))
      RETURN
      END SUBROUTINE QUADSR
!---------------------------------------------------------------------
      SUBROUTINE SIGP(PSTAR,PD,GTEMP,T41,T42,T43,T44,SGLVNU,SIGLNU, &
                      SIGLV,SIGLY,PPTOP,LREAD,KD,KP,KM,KP2)
      DIMENSION Q(KD),QMH(KP),PD(KP2),PLM(KP),GTEMP(KP),PDT(KP2)
      DIMENSION SIGLY(KD),SIGLV(KP)
      DIMENSION CI(KP),SGLVNU(KP),DEL(KD),SIGLNU(KD),CL(KD),RPI(KM)
      DIMENSION IDATE(4)
      DIMENSION T41(KP2,2),T42(KP), &
                T43(KP2,2),T44(KP)
!     integer :: retval
!     character(50) :: prsmid='prsmid'
!CC   18 LEVEL SIGMAS FOR NMC MRF(NEW) MODEL
!CC   DATA Q/.021,.074,.124,.175,.225,.275,.325,.375,.425,.497, &
!CC          .594,.688,.777,.856,.920,.960,.981,.995/
!     FOR SIGMA MODELS,Q=SIGMA,QMH=0.5(Q(I)+Q(I+1),
!     PD=Q*PSS,PLM=QMH*PSS.PSS=SURFACE PRESSURE(SPEC.)
!
!.....   GET NMC SIGMA STRUCTURE
!CC   IF (LREAD.GT.0) GO TO 914
!---   PPTOP IS MODEL TOP PRESSURE IN CB....
!        SIGMA DATA IS BOTTOM OF ATMOSPHERE TO T.O.A.....
!cccc PPTOP=5.0
!     READ(11,PPTOP,END=12321)
12321 CONTINUE
!     WRITE(6,88221)PPTOP,KD,KP
!88221 FORMAT(' ENTER SIGP PPTOP=',E12.5,' KD=',I2,' KP=',I2)
!     open(unit=23,file='fort.23',form='unformatted' &
!     ,    access='sequential')
!     REWIND 23
!     READ(23)SIGLY
!     DO KKK=1,KD
!      SIGLY(KKK)=1.-(FLOAT(KKK)-0.5)/KD
!     END DO
!     WRITE(6,88222)
!88222 FORMAT(' READ AETA')
!     DO 37821 LLL=1,KD
!     WRITE(6,37820)LLL,SIGLY(LLL)
!37820 FORMAT(' L=',I2,' AETA=',E12.5)
!37821 CONTINUE
!     READ(23)SIGLV
!     DO KKK=1,KP
!      SIGLV(KKK)=1.-(FLOAT(KKK-1))/KD
!     END DO
!     WRITE(6,88223)
!88223 FORMAT(' READ ETA')
!     PRINT 704,(SIGLY(K),K=1,KD)
!     PRINT 704,(SIGLV(K),K=1,KP)
!      DO 37823 LLL=1,KP
!      WRITE(6,37822)LLL,SIGLV(LLL)
!37822 FORMAT(' L=',I2,' ETA=',E12.5)
!37823 CONTINUE
  701 FORMAT(F6.2)
  702 FORMAT(7F10.6)
      IF (PPTOP.LE.0.) GO TO 708
      PSFC=100.
!--- IF PTOP NOT EQUAL TO ZERO ADJUST SIGMA SO AS TO GET PROPER STD ATM
!      VERTICAL LOCATION
      DO 706 K=1,KD
       SIGLY(K) = (SIGLY(K)*(PSFC-PPTOP)+PPTOP)/PSFC
  706 CONTINUE
      DO 707 K=1,KP
       SIGLV(K) = (SIGLV(K)*(PSFC-PPTOP)+PPTOP)/PSFC
  707 CONTINUE
  708 CONTINUE
!     PRINT 703,PPTOP
!     PRINT 704,(SIGLY(K),K=1,KD)
!     PRINT 704,(SIGLV(K),K=1,KP)
  703 FORMAT(1H ,'PTOP =',F6.2)
  704 FORMAT(1H ,7F10.6)
      DO 913 K=1,KP
       SGLVNU(K) = SIGLV(K)
       IF (K.LE.KD) SIGLNU(K) = SIGLY(K)
  913 CONTINUE
      DO 77 K=1,KD
         Q(K) = SIGLNU(KD+1-K)
   77 CONTINUE
      PSS=    1013250.
      QMH(1)=0.
      QMH(KP)=1.
      DO 1 K=2,KD
      QMH(K)=0.5*(Q(K-1)+Q(K))
1     CONTINUE
      PD(1)=0.
      PD(KP2)=PSS
      DO 2 K=2,KP
      PD(K)=Q(K-1)*PSS
2     CONTINUE
!       call int_get_fresh_handle(retval)
!       close(retval)
!       write(0,*)' before open in CO2O3'
!       open(unit=retval,file=prsmid,form='UNFORMATTED',iostat=ier)
!       write(0,*)' after open1'
!       do k=1,62
!         write(retval)pd(k)
!       enddo
!       close(retval)
      PLM(1)=0.
      DO 3 K=1,KM
      PLM(K+1)=0.5*(PD(K+1)+PD(K+2))
3     CONTINUE
      PLM(KP)=PSS
      DO 4 K=1,KD
      GTEMP(K)=PD(K+1)**0.2*(1.+PD(K+1)/30000.)**0.8/1013250.
4     CONTINUE
      GTEMP(KP)=0.
!+++  WRITE (6,100) (GTEMP(K),K=1,KD)
!+++  WRITE (6,100) (PD(K),K=1,KP2)
!+++  WRITE (6,100) (PLM(K),K=1,KP)
!***TAPES 41,42 ARE OUTPUT TO THE CO2 INTERPOLATION PROGRAM (PS=1013MB)
!  THE FOLLOWING PUTS P-DATA INTO MB
      DO 11 I=1,KP
      PD(I)=PD(I)*1.0E-3
      PLM(I)=PLM(I)*1.0E-3
11    CONTINUE
      PD(KP2)=PD(KP2)*1.0E-3
!CC         WRITE (41,101) (PD(K),K=1,KP2)
!CC         WRITE (41,101) (PLM(K),K=1,KP)
!CC         WRITE (42,101) (PLM(K),K=1,KP)
      DO 300 K=1,KP2
       T41(K,1) = PD(K)
  300 CONTINUE
      DO 301 K=1,KP
       T41(K,2) = PLM(K)
       T42(K) = PLM(K)
  301 CONTINUE
!***STORE AS PDT,SO THAT RIGHT PD IS RETURNED TO PTZ
      DO 12 I=1,KP2
      PDT(I)=PD(I)
12    CONTINUE
!***SECOND PASS: PSS=810MB,GTEMP NOT COMPUTED
      PSS=0.8*1013250.
      QMH(1)=0.
      QMH(KP)=1.
      DO 201 K=2,KD
      QMH(K)=0.5*(Q(K-1)+Q(K))
201   CONTINUE
      PD(1)=0.
      PD(KP2)=PSS
      DO 202 K=2,KP
      PD(K)=Q(K-1)*PSS
202   CONTINUE
      PLM(1)=0.
      DO 203 K=1,KM
      PLM(K+1)=0.5*(PD(K+1)+PD(K+2))
203   CONTINUE
      PLM(KP)=PSS
!+++  WRITE (6,100) (PD(K),K=1,KP2)
!+++  WRITE (6,100) (PLM(K),K=1,KP)
!***TAPES 43,44 ARE OUTPUT TO THE CO2 INTERPOLATION PROGRAM(PS=810 MB)
!  THE FOLLOWING PUTS P-DATA INTO MB
      DO 211 I=1,KP
      PD(I)=PD(I)*1.0E-3
      PLM(I)=PLM(I)*1.0E-3
211   CONTINUE
      PD(KP2)=PD(KP2)*1.0E-3
!CC       WRITE (43,101) (PD(K),K=1,KP2)
!CC       WRITE (43,101) (PLM(K),K=1,KP)
!CC       WRITE (44,101) (PLM(K),K=1,KP)
      DO 302 K=1,KP2
       T43(K,1) = PD(K)
  302 CONTINUE
      DO 303 K=1,KP
       T43(K,2) = PLM(K)
       T44(K) = PLM(K)
  303 CONTINUE
!***RESTORE PD
      DO 212 I=1,KP2
      PD(I)=PDT(I)
212   CONTINUE
100   FORMAT (1X,5E20.13)
101   FORMAT (5E16.9)
      RETURN
      END SUBROUTINE SIGP
!---------------------------------------------------------------------
      SUBROUTINE SINTR2
!....
!     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!     REAL P1,P2,PA,TRNSLO,CORE,TRANSA,PATH,UEXP,SEXP,ETA,SEXPV
      COMMON/INPUT/P1,P2,TRNSLO,IA,JA,N
!     COMMON/PRESS/ PA(109)
!     COMMON/TRAN/ TRANSA(109,109)
!     COMMON/COEFS/XA(109),CA(109),ETA(109),SEXPV(109),CORE,UEXP,SEXP
      DO 70 L=1,109
      IP1=L
      IF (P2-PA(L)) 65,65,70
   70 CONTINUE
   65 I=IP1-1
      IF (IP1.EQ.1) IP1=2
      IF (I.EQ.0) I=1
      DO 80 L=1,109
      JP1=L
      IF (P1-PA(L)) 75,75,80
   80 CONTINUE
   75 J=JP1-1
      IF (JP1.EQ.1) JP1=2
      IF (J.EQ.0) J=1
      JJJ=J
      III=I
      J=JJJ
      JP1=J+1
      I=III
      IP1=I+1
!  DETERMINE ETAP,THE VALUE OF ETA TO USE BY LINEAR INTERPOLATION
!    FOR PETA(=0.5*(P1+P2))
      PETA=P2
      DO 90 L=1,109
      IETAP1=L
      IF (PETA-PA(L)) 85,85,90
90    CONTINUE
85    IETA=IETAP1-1
      IF (IETAP1.EQ.1) IETAP1=2
      IF (IETA.EQ.0) IETA=1
      ETAP=ETA(IETA)+(PETA-PA(IETA))*(ETA(IETAP1)-ETA(IETA))/ &
       (PA(IETAP1)-PA(IETA))
      SEXP=SEXPV(IETA)+(PETA-PA(IETA))*(SEXPV(IETAP1)- &
       SEXPV(IETA))/ (PA(IETAP1)-PA(IETA))
      PIPMPI=PA(IP1)-PA(I)
      UP2P1=(PATH(P2,P1,CORE,ETAP))**UEXP
      IF (I-J) 126,126,127
  126 CONTINUE
      TRIP=(CA(IP1)*DLOG(1.0D0+XA(IP1)*UP2P1))**(SEXP/UEXP)
      TRI=(CA(I)*DLOG(1.0D0+XA(I)*UP2P1))**(SEXP/UEXP)
      TRNSLO=1.0D0-((PA(IP1)-P2)*TRI+(P2-PA(I))*TRIP)/PIPMPI
      GO TO 128
  127 TIJ=TRANSA(I,J)
      TIPJ=TRANSA(I+1,J)
      TIJP=TRANSA(I,J+1)
      TIPJP=TRANSA(I+1,J+1)
      UIJ=(PATH(PA(I),PA(J),CORE,ETAP))**UEXP
      UIPJ=(PATH(PA(I+1),PA(J),CORE,ETAP))**UEXP
      UIJP=(PATH(PA(I),PA(J+1),CORE,ETAP))**UEXP
      UIPJP=(PATH(PA(I+1),PA(J+1),CORE,ETAP))**UEXP
      PRODI=CA(I)*XA(I)
      PRODIP=CA(I+1)*XA(I+1)
      PROD=((PA(I+1)-P2)*PRODI+(P2-PA(I))*PRODIP)/PIPMPI
      XINT=((PA(I+1)-P2)*XA(I)+(P2-PA(I))*XA(I+1))/PIPMPI
      CINT=PROD/XINT
      AIJ=(CINT*DLOG(1.0D0+XINT*UIJ))**(SEXP/UEXP)
      AIJP=(CINT*DLOG(1.0D0+XINT*UIJP))**(SEXP/UEXP)
      AIPJ=(CINT*DLOG(1.0D0+XINT*UIPJ))**(SEXP/UEXP)
      AIPJP=(CINT*DLOG(1.0D0+XINT*UIPJP))**(SEXP/UEXP)
      EIJ=TIJ+AIJ
      EIPJ=TIPJ+AIPJ
      EIJP=TIJP+AIJP
      EIPJP=TIPJP+AIPJP
      DTDJ=(EIJP-EIJ)/(PA(J+1)-PA(J))
      DTDPJ=(EIPJP-EIPJ)/(PA(J+1)-PA(J))
      EPIP1=EIJ+DTDJ*(P1-PA(J))
      EPIPP1=EIPJ+DTDPJ*(P1-PA(J))
      EPP2P1=((PA(I+1)-P2)*EPIP1+(P2-PA(I))*EPIPP1)/PIPMPI
      TRNSLO=EPP2P1-(CINT*DLOG(1.0D0+XINT*UP2P1))**(SEXP/UEXP)
      IF (I.GE.108.OR.J.GE.108) GO TO 350
      IF (I-J-2) 350,350,355
355   CONTINUE
      TIP2J=TRANSA(I+2,J)
      TIP2JP=TRANSA(I+2,J+1)
      TI2J2=TRANSA(I+2,J+2)
      TIJP2=TRANSA(I,J+2)
      TIPJP2=TRANSA(I+1,J+2)
      UIP2J=(PATH(PA(I+2),PA(J),CORE,ETAP))**UEXP
      UIJP2=(PATH(PA(I),PA(J+2),CORE,ETAP))**UEXP
      UIPJP2=(PATH(PA(I+1),PA(J+2),CORE,ETAP))**UEXP
      UI2J2=(PATH(PA(I+2),PA(J+2),CORE,ETAP))**UEXP
      UIP2JP=(PATH(PA(I+2),PA(J+1),CORE,ETAP))**UEXP
      AIJP2=(CINT*DLOG(1.0D0+XINT*UIJP2))**(SEXP/UEXP)
      AIPJP2=(CINT*DLOG(1.0D0+XINT*UIPJP2))**(SEXP/UEXP)
      AIP2J=(CINT*DLOG(1.0D0+XINT*UIP2J))**(SEXP/UEXP)
      AIP2JP=(CINT*DLOG(1.0D0+XINT*UIP2JP))**(SEXP/UEXP)
      AI2J2=(CINT*DLOG(1.0D0+XINT*UI2J2))**(SEXP/UEXP)
      EIP2J=TIP2J+AIP2J
      EIP2JP=TIP2JP+AIP2JP
      EIJP2=TIJP2+AIJP2
      EIPJP2=TIPJP2+AIPJP2
      EI2J2=TI2J2+AI2J2
      CALL QINTRP(PA(J),PA(J+1),PA(J+2),EIJ,EIJP,EIJP2,P1,EI)
      CALL QINTRP(PA(J),PA(J+1),PA(J+2),EIPJ,EIPJP,EIPJP2,P1,EP)
      CALL QINTRP(PA(J),PA(J+1),PA(J+2),EIP2J,EIP2JP,EI2J2,P1,EP2)
      CALL QINTRP(PA(I),PA(I+1),PA(I+2),EI,EP,EP2,P2,EPSIL)
      TRNSLO=EPSIL-(CINT*DLOG(1.0D0+XINT*UP2P1))**(SEXP/UEXP)
  350 CONTINUE
  128 CONTINUE
  205 CONTINUE
      RETURN
      END SUBROUTINE SINTR2
      SUBROUTINE CO2O3(SFULL,SHALF,PPTOP,L,LP1,LP2)
!CCC  PROGRAM CO2O3 = CONSOLIDATION OF A NUMBER OF DAN SCHWARZKOPF,GFDL
!                     CODES TO PRODUCE A FILE OF CO2 HGT DATA
!                     FOR ANY VERTICAL COORDINATE (READ BY SUBROUTINE
!                     CONRAD IN THE GFDL RADIATION CODES)-K.A.C. JUN89.
!NOV89--UPDATED (NOV 89) FOR LATEST GFDL LW RADIATION.....K.A.C.
      LOGICAL                 :: opened
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
      CHARACTER*80 errmess
!     integer :: retval,kk,ka,kb
!     character(50) :: co2='co2'
      INTEGER etarad_unit61, etarad_unit62, etarad_unit63,IERROR
      DIMENSION SGTEMP(LP1,2),CO2D1D(L,6),CO2D2D(LP1,LP1,6)
!NOV89
      DIMENSION CO2IQ2(LP1,LP1,6),CO2IQ3(LP1,LP1,6),CO2IQ5(LP1,LP1,6)
!NOV89
      DIMENSION T41(LP2,2),T42(LP1), &
                T43(LP2,2),T44(LP1)
      DIMENSION T20(LP1,LP1,3),T21(LP1,LP1,3)
      DIMENSION T22(LP1,LP1,3),T23(LP1,LP1,3)
      DIMENSION SGLVNU(LP1),SIGLNU(L)
      DIMENSION SFULL(LP1),SHALF(L)
!     DIMENSION STEMP(LP1),GTEMP(LP1)
!     DIMENSION CDTM51(L),CO2M51(L),C2DM51(L)
!     DIMENSION CDTM58(L),CO2M58(L),C2DM58(L)
!     DIMENSION CDT51(LP1,LP1),CO251(LP1,LP1),C2D51(LP1,LP1)
!     DIMENSION CDT58(LP1,LP1),CO258(LP1,LP1),C2D58(LP1,LP1)
!NOV89
!     DIMENSION CDT31(LP1),CO231(LP1),C2D31(LP1)
!     DIMENSION CDT38(LP1),CO238(LP1),C2D38(LP1)
!     DIMENSION CDT71(LP1),CO271(LP1),C2D71(LP1)
!     DIMENSION CDT78(LP1),CO278(LP1),C2D78(LP1)
!     DIMENSION CO211(LP1),CO218(LP1)
!     EQUIVALENCE (CDT31(1),CO2IQ2(1,1,1)),(CO231(1),CO2IQ2(1,1,2))
!     EQUIVALENCE (C2D31(1),CO2IQ2(1,1,3)),(CDT38(1),CO2IQ2(1,1,4))
!     EQUIVALENCE (CO238(1),CO2IQ2(1,1,5)),(C2D38(1),CO2IQ2(1,1,6))
!     EQUIVALENCE (CDT71(1),CO2IQ3(1,1,1)),(CO271(1),CO2IQ3(1,1,2))
!     EQUIVALENCE (C2D71(1),CO2IQ3(1,1,3)),(CDT78(1),CO2IQ3(1,1,4))
!     EQUIVALENCE (CO278(1),CO2IQ3(1,1,5)),(C2D78(1),CO2IQ3(1,1,6))
!     EQUIVALENCE (CO211(1),CO2IQ5(1,1,2)),(CO218(1),CO2IQ5(1,1,5))
!NOV89
!     EQUIVALENCE (STEMP(1),SGTEMP(1,1)),(GTEMP(1),SGTEMP(1,2))
!     EQUIVALENCE (CDTM51(1),CO2D1D(1,1)),(CO2M51(1),CO2D1D(1,2))
!     EQUIVALENCE (C2DM51(1),CO2D1D(1,3)),(CDTM58(1),CO2D1D(1,4))
!     EQUIVALENCE (CO2M58(1),CO2D1D(1,5)),(C2DM58(1),CO2D1D(1,6))
!     EQUIVALENCE (CDT51(1,1),CO2D2D(1,1,1)),(CO251(1,1),CO2D2D(1,1,2))
!     EQUIVALENCE (C2D51(1,1),CO2D2D(1,1,3)),(CDT58(1,1),CO2D2D(1,1,4))
!     EQUIVALENCE (CO258(1,1),CO2D2D(1,1,5)),(C2D58(1,1),CO2D2D(1,1,6))

!    Deallocate before reading. This is required for nested domain init.
!
      IF(ALLOCATED (CO251))DEALLOCATE(CO251)
      IF(ALLOCATED (CDT51))DEALLOCATE(CDT51)
      IF(ALLOCATED (C2D51))DEALLOCATE(C2D51)
      IF(ALLOCATED (CO258))DEALLOCATE(CO258)
      IF(ALLOCATED (CDT58))DEALLOCATE(CDT58)
      IF(ALLOCATED (C2D58))DEALLOCATE(C2D58)
      IF(ALLOCATED (STEMP))DEALLOCATE(STEMP)
      IF(ALLOCATED (GTEMP))DEALLOCATE(GTEMP)
      IF(ALLOCATED (CO231))DEALLOCATE(CO231)
      IF(ALLOCATED (CDT31))DEALLOCATE(CDT31)
      IF(ALLOCATED (C2D31))DEALLOCATE(C2D31)
      IF(ALLOCATED (CO238))DEALLOCATE(CO238)
      IF(ALLOCATED (CDT38))DEALLOCATE(CDT38)
      IF(ALLOCATED (C2D38))DEALLOCATE(C2D38)
      IF(ALLOCATED (CO271))DEALLOCATE(CO271)
      IF(ALLOCATED (CDT71))DEALLOCATE(CDT71)
      IF(ALLOCATED (C2D71))DEALLOCATE(C2D71)
      IF(ALLOCATED (CO278))DEALLOCATE(CO278)
      IF(ALLOCATED (CDT78))DEALLOCATE(CDT78)
      IF(ALLOCATED (C2D78))DEALLOCATE(C2D78)
      IF(ALLOCATED (CO2M51))DEALLOCATE(CO2M51)
      IF(ALLOCATED (CDTM51))DEALLOCATE(CDTM51)
      IF(ALLOCATED (C2DM51))DEALLOCATE(C2DM51)
      IF(ALLOCATED (CO2M58))DEALLOCATE(CO2M58)
      IF(ALLOCATED (CDTM58))DEALLOCATE(CDTM58)
      IF(ALLOCATED (C2DM58))DEALLOCATE(C2DM58)

      ALLOCATE(CO251(LP1,LP1))
      ALLOCATE(CDT51(LP1,LP1))
      ALLOCATE(C2D51(LP1,LP1))
      ALLOCATE(CO258(LP1,LP1))
      ALLOCATE(CDT58(LP1,LP1))
      ALLOCATE(C2D58(LP1,LP1))
      ALLOCATE(STEMP(LP1))
      ALLOCATE(GTEMP(LP1))
      ALLOCATE(CO231(LP1))
      ALLOCATE(CDT31(LP1))
      ALLOCATE(C2D31(LP1))
      ALLOCATE(CO238(LP1))
      ALLOCATE(CDT38(LP1))
      ALLOCATE(C2D38(LP1))
      ALLOCATE(CO271(LP1))
      ALLOCATE(CDT71(LP1))
      ALLOCATE(C2D71(LP1))
      ALLOCATE(CO278(LP1))
      ALLOCATE(CDT78(LP1))
      ALLOCATE(C2D78(LP1))
      ALLOCATE(CO2M51(L))
      ALLOCATE(CDTM51(L))
      ALLOCATE(C2DM51(L))
      ALLOCATE(CO2M58(L))
      ALLOCATE(CDTM58(L))
      ALLOCATE(C2DM58(L))
      IF ( wrf_dm_on_monitor() ) THEN
        DO i = 61,99
          INQUIRE ( i , OPENED = opened )
          IF ( .NOT. opened ) THEN
            etarad_unit61 = i
            GOTO 2061
          ENDIF
        ENDDO
        etarad_unit61 = -1
 2061   CONTINUE
        DO i = 62,99
          INQUIRE ( i , OPENED = opened )
          IF ( .NOT. opened ) THEN
            etarad_unit62 = i
            GOTO 2062
          ENDIF
        ENDDO
        etarad_unit62 = -1
 2062   CONTINUE
        DO i = 63,99
          INQUIRE ( i , OPENED = opened )
          IF ( .NOT. opened ) THEN
            etarad_unit63 = i
            GOTO 2063
          ENDIF
        ENDDO
        etarad_unit63 = -1
 2063   CONTINUE
      ENDIF
      CALL wrf_dm_bcast_bytes ( etarad_unit61 , IWORDSIZE )
      IF ( etarad_unit61 < 0 ) THEN
        CALL wrf_error_fatal ( 'module_ra_hwrf: co2o3: Can not find unused fortran unit to read in lookup table.' )
      ENDIF
      CALL wrf_dm_bcast_bytes ( etarad_unit62 , IWORDSIZE )
      IF ( etarad_unit62 < 0 ) THEN
        CALL wrf_error_fatal ( 'module_ra_hwrf: co2o3: Can not find unused fortran unit to read in lookup table.' )
      ENDIF
      CALL wrf_dm_bcast_bytes ( etarad_unit63 , IWORDSIZE )
      IF ( etarad_unit63 < 0 ) THEN
        CALL wrf_error_fatal ( 'module_ra_hwrf: co2o3: Can not find unused fortran unit to read in lookup table.' )
      ENDIF
        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(etarad_unit61,FILE='tr49t85',                  &
               FORM='FORMATTED',STATUS='OLD',ERR=9061,IOSTAT=IERROR)
        ENDIF
        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(etarad_unit62,FILE='tr49t67',                  &
               FORM='FORMATTED',STATUS='OLD',ERR=9062,IOSTAT=IERROR)
        ENDIF
        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(etarad_unit63,FILE='tr67t85',                  &
               FORM='FORMATTED',STATUS='OLD',ERR=9063,IOSTAT=IERROR)
        ENDIF

!===>  GET SGTEMP AND OUTPUT WHICH USED TO BE ON UNITS 41,42,43,44....
      LREAD = 0
!     DO KKK=1,L
!JD      READ(23)SIGLNU(KKK)
!      SIGLNU(KKK)=1.-FLOAT(KKK)/LP1
!     END DO
      CALL CO2PTZ(SGTEMP,T41,T42,T43,T44,SGLVNU,SIGLNU, &
                  SFULL,SHALF,PPTOP,LREAD,L,LP1,LP2)
!       call int_get_fresh_handle(retval)
!       close(retval)
!       open(unit=retval,file=co2,form='UNFORMATTED',iostat=ier)
!       do kk=1,2
!         write(retval)(sgtemp(k,kk),k=1,61)
!       enddo
      DO K=1,LP1
        STEMP(K)=SGTEMP(K,1)
        GTEMP(K)=SGTEMP(K,2)
      ENDDO
!===>  INTERPOLATE DESIRED CO2 DATA FROM THE DETAILED(109,109) GRID..
!         IR=1,IQ=1 IS FOR COMMON /CO2BD3/ IN RADIATION CODE...
!           FOR THE CONSOLIDATED 490-850 CM-1 BAND...
!NOV89
!     ICO2TP=61
      ICO2TP=etarad_unit61
!NOV89
      IR = 1
      RATIO = 1.0
      NMETHD = 2
      CALL CO2INT(ICO2TP,T41,T42,T22,RATIO,IR,NMETHD,L,LP1,LP2)
      IR = 1
      RATIO = 1.0
      NMETHD = 1
      CALL CO2INT(ICO2TP,T41,T42,T20,RATIO,IR,NMETHD,L,LP1,LP2)
      IR = 1
      RATIO = 1.0
      NMETHD = 2
      CALL CO2INT(ICO2TP,T43,T44,T23,RATIO,IR,NMETHD,L,LP1,LP2)
      IR = 1
      RATIO = 1.0
      NMETHD = 1
      CALL CO2INT(ICO2TP,T43,T44,T21,RATIO,IR,NMETHD,L,LP1,LP2)
!===>    FILL UP THE CO2D1D ARRAY
!       THE FOLLOWING GETS CO2 TRANSMISSION FUNCTIONS AND
!         THEIR DERIVATIVES FOR TAU(I,I+1),I=1,LEVS,
!         WHERE THE VALUES ARE NOT OBTAINED BY QUADRATURE BUT ARE THE
!         ACTUAL TRANSMISSIVITIES,ETC,BETWEEN A PAIR OF PRESSURES. THESE
!         ARE USED ONLY FOR NEARBY LAYER CALCULATIONS INCLUDING H2O..
!
      IQ = 1
      CALL CO2IN1(T20,T21,CO2D1D,IQ,L,LP1)
!       do kk=1,6
!         write(retval)(co2d1d(k,kk),k=1,60)
!       enddo
      DO K=1,L
        CDTM51(K)=CO2D1D(K,1)
        CO2M51(K)=CO2D1D(K,2)
        C2DM51(K)=CO2D1D(K,3)
        CDTM58(K)=CO2D1D(K,4)
        CO2M58(K)=CO2D1D(K,5)
        C2DM58(K)=CO2D1D(K,6)
      ENDDO
!
!===>    FILL UP THE CO2D2D ARRAY
!    THE FOLLOWING GETS CO2 TRANSMISSION FUNCTIONS AND THEIR DERIVATIVES
!        FROM 109-LEVEL LINE-BY-LINE CALCULATIONS MADE USING THE 1982
!        MCCLATCHY TAPE (12511 LINES),CONSOLIDATED,INTERPOLATED
!        TO THE MRF VERTICAL COORDINATE,AND RE-CONSOLIDATED TO A
!        200 CM-1 BANDWIDTH. THE INTERPOLATION METHOD IS DESCRIBED IN
!        SCHWARZKOPF AND FELS (J.G.R.,1985).
!
      CALL CO2INS(T22,T23,CO2D2D,IQ,L,LP1,1)
!       do kk=1,6
!         write(retval)((co2d2d(ka,kb,kk),ka=1,61),kb=1,61)
!       enddo
      DO K1=1,LP1
      DO K2=1,LP1
        CDT51(K1,K2)=CO2D2D(K1,K2,1)
        CO251(K1,K2)=CO2D2D(K1,K2,2)
        C2D51(K1,K2)=CO2D2D(K1,K2,3)
        CDT58(K1,K2)=CO2D2D(K1,K2,4)
        CO258(K1,K2)=CO2D2D(K1,K2,5)
        C2D58(K1,K2)=CO2D2D(K1,K2,6)
      ENDDO
      ENDDO
!
!NOV89
!===>  INTERPOLATE DESIRED CO2 DATA FROM THE DETAILED(109,109) GRID..
!         IR=2,IQ=2 IS FOR COMMON /CO2BD2/ IN RADIATION CODE...
!           FOR THE CONSOLIDATED 490-670 CM-1 BAND...
!     ICO2TP=62
      ICO2TP=etarad_unit62
      IR = 2
      RATIO = 1.0
      NMETHD = 2
      CALL CO2INT(ICO2TP,T41,T42,T22,RATIO,IR,NMETHD,L,LP1,LP2)
      CALL CO2INT(ICO2TP,T43,T44,T23,RATIO,IR,NMETHD,L,LP1,LP2)
      IQ = 2
      CALL CO2INS(T22,T23,CO2IQ2,IQ,L,LP1,2)
!       do kk=1,6
!         write(retval)(co2iq2(k,1,kk),k=1,61)
!       enddo
      DO K=1,LP1
        CDT31(K)=CO2IQ2(K,1,1)
        CO231(K)=CO2IQ2(K,1,2)
        C2D31(K)=CO2IQ2(K,1,3)
        CDT38(K)=CO2IQ2(K,1,4)
        CO238(K)=CO2IQ2(K,1,5)
        C2D38(K)=CO2IQ2(K,1,6)
      ENDDO
!===>  INTERPOLATE DESIRED CO2 DATA FROM THE DETAILED(109,109) GRID..
!         IR=3,IQ=3 IS FOR COMMON /CO2BD4/ IN RADIATION CODE...
!           FOR THE CONSOLIDATED 670-850 CM-1 BAND...
!     ICO2TP=63
      ICO2TP=etarad_unit63
      IR = 3
      RATIO = 1.0
      NMETHD = 2
      CALL CO2INT(ICO2TP,T41,T42,T22,RATIO,IR,NMETHD,L,LP1,LP2)
      CALL CO2INT(ICO2TP,T43,T44,T23,RATIO,IR,NMETHD,L,LP1,LP2)
      IQ = 3
      CALL CO2INS(T22,T23,CO2IQ3,IQ,L,LP1,3)
!       do kk=1,6
!         write(retval)(co2iq3(k,1,kk),k=1,61)
!       enddo
!       close(retval)
      DO K=1,LP1
        CDT71(K)=CO2IQ3(K,1,1)
        CO271(K)=CO2IQ3(K,1,2)
        C2D71(K)=CO2IQ3(K,1,3)
        CDT78(K)=CO2IQ3(K,1,4)
        CO278(K)=CO2IQ3(K,1,5)
        C2D78(K)=CO2IQ3(K,1,6)
      ENDDO
!---      FOLLOWING CODE NOT WORKING AND NOT NEEDED YET
!===>  INTERPOLATE DESIRED CO2 DATA FROM THE DETAILED(109,109) GRID..
!         IR=4,IQ=5 IS FOR COMMON /CO2BD5/ IN RADIATION CODE...
!           FOR THE 4.3 MICRON BAND...
! NOT USED YET      ICO2TP=65
! NOT USED YET      IR = 4
! NOT USED YET      RATIO = 1.0
! DAN SCHWARZ --- USE 300PPMV  RATIO = 0.9091   (NOT TESTED YET).....
! NOT USED YET      NMETHD = 2
! NOT USED YET      CALL CO2INT(ICO2TP,T41,T42,T22,RATIO,IR,NMETHD)
! NOT USED YET      CALL CO2INT(ICO2TP,T43,T44,T23,RATIO,IR,NMETHD)
! NOT USED YET      IQ = 5
! NOT USED YET      CALL CO2INS(T22,T23,CO2IQ5,IQ)
!NOV89
!...     WRITE DATA TO DISK..
!            ...SINCE THESE CODES ARE COMPILED WITH AUTODBL,THE CO2 DATA
!               IS CONVERTED TO SINGLE PRECISION IN A LATER JOB STEP..

! NOT USED YET      WRITE(66) CO211
! NOT USED YET      WRITE(66) CO218
!NOV89
         IF ( wrf_dm_on_monitor() ) THEN
           CLOSE (etarad_unit61)
           CLOSE (etarad_unit62)
           CLOSE (etarad_unit63)
         ENDIF

      RETURN
9061 CONTINUE
     WRITE( errmess , '(A49,I4)' ) 'module_ra_hwrf: error reading tr49t85 on unit ',etarad_unit61
     write(0,*)' IERROR=',IERROR
     CALL wrf_error_fatal(errmess)
9062 CONTINUE
     WRITE( errmess , '(A49,I4)' ) 'module_ra_hwrf: error reading tr49t67 on unit ',etarad_unit62
     write(0,*)' IERROR=',IERROR
     CALL wrf_error_fatal(errmess)
9063 CONTINUE
     WRITE( errmess , '(A49,I4)' ) 'module_ra_hwrf: error reading tr67t85 on unit ',etarad_unit63
     write(0,*)' IERROR=',IERROR
     CALL wrf_error_fatal(errmess)
      END SUBROUTINE CO2O3


!!================================================================================
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      SUBROUTINE CONRAD(KDS,KDE,KMS,KME,KTS,KTE)
!----------------------------------------------------------------------
!    *******************************************************************
!    *                           C O N R A D                           *
!    *    READ CO2 TRANSMISSION DATA FROM UNIT(NFILE)FOR NEW VERTICAL  *
!    *      COORDINATE TESTS      ...                                  *
!    *    THESE ARRAYS USED TO BE IN BLOCK DATA    ...K.CAMPANA-MAR 90 *
!    *******************************************************************
!
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: KDS,KDE,KMS,KME,KTS,KTE
!----------------------------------------------------------------------
!
      INTEGER :: I,I1,I2,IERROR,IRTN,J,K,KK,L,LP1,N,NUNIT_CO2,RSIZE
      INTEGER,DIMENSION(3) :: RSZE
!
      REAL,DIMENSION(KMS:KME-1,6) :: CO21D
      REAL,DIMENSION(KMS:KME,2) :: SGTMP
      REAL,DIMENSION(KMS:KME,6) :: CO21D3,CO21D7
      REAL,DIMENSION(KMS:KME,KMS:KME,6) :: CO22D
      REAL,DIMENSION((KME-KMS+1)*(KME-KMS+1)) :: DATA2
      LOGICAL :: OPENED
      LOGICAL,EXTERNAL :: wrf_dm_on_monitor
      CHARACTER*80 errmess
!
!----------------------------------------------------------------------
!
!                 CO2 DATA TABLES FOR USER'S VERTICAL COORDINATE
!
!   THE FOLLOWING COMMON BLOCKS CONTAIN PRETABULATED CO2 TRANSMISSION
!       FUNCTIONS, EVALUATED USING THE METHODS OF FELS AND
!       SCHWARZKOPF (1981) AND SCHWARZKOPF AND FELS (1985),
!-----  THE 2-DIMENSIONAL ARRAYS ARE
!                    CO2 TRANSMISSION FUNCTIONS AND THEIR DERIVATIVES
!        FROM 109-LEVEL LINE-BY-LINE CALCULATIONS MADE USING THE 1982
!        MCCLATCHY TAPE (12511 LINES),CONSOLIDATED,INTERPOLATED
!        TO THE NMC MRF VERTICAL COORDINATTE,AND RE-CONSOLIDATED TO A
!        200 CM-1 BANDWIDTH. THE INTERPOLATION METHOD IS DESCRIBED IN
!        SCHWARZKOPF AND FELS (J.G.R.,1985).
!-----  THE 1-DIM ARRAYS ARE
!                  CO2 TRANSMISSION FUNCTIONS AND THEIR DERIVATIVES
!          FOR TAU(I,I+1),I=1,L,
!            WHERE THE VALUES ARE NOT OBTAINED BY QUADRATURE,BUT ARE THE
!            ACTUAL TRANSMISSIVITIES,ETC,BETWEEN A PAIR OF PRESSURES.
!          THESE USED ONLY FOR NEARBY LAYER CALCULATIONS INCLUDING QH2O.
!-----  THE WEIGHTING FUNCTION GTEMP=P(K)**0.2*(1.+P(K)/30000.)**0.8/
!         1013250.,WHERE P(K)=PRESSURE,NMC MRF(NEW)  L18 DATA LEVELS FOR
!         PSTAR=1013250.
!-----  STEMP IS US STANDARD ATMOSPHERES,1976,AT DATA PRESSURE LEVELS
!        USING NMC MRF SIGMAS,WHERE PSTAR=1013.25 MB (PTZ PROGRAM)
!
!***CO2 TRANSMISSION FUNCTIONS AND TEMPERATURE
!   AND PRESSURE DERIVATIVES FOR THE 560-800 CM-1 BAND. ALSO INCLUDED
!   ARE THE STANDARD TEMPERATURES AND THE WEIGHTING FUNCTION. THESE
!   DATA ARE IN BLOCK DATA BD3:
!         CO251    =  TRANSMISSION FCTNS FOR T0 (STD. PROFILE)
!                       WITH P(SFC)=1013.25 MB
!         CO258    =  TRANSMISSION FCTNS. FOR T0 (STD. PROFILE)
!                       WITH P(SFC)= 810 MB
!         CDT51    =  FIRST TEMPERATURE DERIVATIVE OF CO251
!         CDT58    =  FIRST TEMPERATURE DERIVATIVE OF CO258
!         C2D51    =  SECOND TEMPERATURE DERIVATIVE OF CO251
!         C2D58    =  SECOND TEMPERATURE DERIVATIVE OF CO251
!         CO2M51   =  TRANSMISSION FCTNS FOR T0 FOR ADJACENT PRESSURE
!                        LEVELS, WITH NO PRESSURE QUADRATURE. USED FOR
!                        NEARBY LAYER COMPUTATIONS. P(SFC)=1013.25 MB
!         CO2M58   =  SAME AS CO2M51,WITH P(SFC)= 810 MB
!         CDTM51   =  FIRST TEMPERATURE DERIVATIVE OF CO2M51
!         CDTM58   =  FIRST TEMPERATURE DERIVATIVE OF CO2M58
!         C2DM51   =  SECOND TEMPERATURE DERIVATIVE OF CO2M51
!         C2DM58   =  SECOND TEMPERATURE DERIVATIVE OF CO2M58
!         STEMP    =  STANDARD TEMPERATURES FOR MODEL PRESSURE LEVEL
!                        STRUCTURE WITH P(SFC)=1013.25 MB
!         GTEMP    =  WEIGHTING FUNCTION FOR MODEL PRESSURE LEVEL
!                        STRUCTURE WITH P(SFC)=1013.25 MB.
!-----       THE FOLLOWING ARE STILL IN BLOCK DATA
!         B0       =  TEMP. COEFFICIENT USED FOR CO2 TRANS. FCTN.
!                        CORRECTION FOR T(K). (SEE REF. 4 AND BD3)
!         B1       =  TEMP. COEFFICIENT, USED ALONG WITH B0
!         B2       =  TEMP. COEFFICIENT, USED ALONG WITH B0
!         B3       =  TEMP. COEFFICIENT, USED ALONG WITH B0
!
!***CO2 TRANSMISSION FUNCTIONS AND TEMPERATURE
!   AND PRESSURE DERIVATIVES FOR THE 560-670 CM-1 PART OF THE 15 UM
!   CO2 BAND.  THESE DATA ARE IN BLOCK DATA BD2.
!     FOR THE 560-670 CM-1 BAND,ONLY THE (1,I) VALUES ARE USED , SINCE
!     THESE ARE USED FOR CTS COMPUTATIONS.
!         CO231    =  TRANSMISSION FCTNS FOR T0 (STD. PROFILE)
!                       WITH P(SFC)=1013.25 MB
!         CO238    =  TRANSMISSION FCTNS. FOR T0 (STD. PROFILE)
!                       WITH P(SFC)= 810 MB
!         CDT31    =  FIRST TEMPERATURE DERIVATIVE OF CO231
!         CDT38    =  FIRST TEMPERATURE DERIVATIVE OF CO238
!         C2D31    =  SECOND TEMPERATURE DERIVATIVE OF CO231
!         C2D38    =  SECOND TEMPERATURE DERIVATIVE OF CO231
!
!***CO2 TRANSMISSION FUNCTIONS AND TEMPERATURE
!   AND PRESSURE DERIVATIVES FOR THE 670-800 CM-1 PART OF THE 15 UM
!   CO2 BAND.  THESE DATA ARE IN BLOCK DATA BD4.
!         CO271    =  TRANSMISSION FCTNS FOR T0 (STD. PROFILE)
!                       WITH P(SFC)=1013.25 MB
!         CO278    =  TRANSMISSION FCTNS. FOR T0 (STD. PROFILE)
!                       WITH P(SFC)= 810 MB
!         CDT71    =  FIRST TEMPERATURE DERIVATIVE OF CO271
!         CDT78    =  FIRST TEMPERATURE DERIVATIVE OF CO278
!         C2D71    =  SECOND TEMPERATURE DERIVATIVE OF CO271
!         C2D78    =  SECOND TEMPERATURE DERIVATIVE OF CO271
!
! *****THE FOLLOWING NOT USED IN CURRENT VERSION OF RADIATION *******
!
! --CO2 TRANSMISSION FUNCTIONS FOR THE 2270-
!       2380 PART OF THE 4.3 UM CO2 BAND.
!              THESE DATA ARE IN BLOCK DATA BD5.
!         CO211    =  TRANSMISSION FCTNS FOR T0 (STD. PROFILE)
!                        WITH P(SFC)=1013.25 MB
!         CO218    =  TRANSMISSION FCTNS. FOR T0 (STD. PROFILE)
!                       WITH P(SFC)= 810 MB
!
! *****THE ABOVE NOT USED IN CURRENT VERSION OF RADIATION ***********
!----------------------------------------------------------------------
!
      L=KME-KMS
      LP1=KME-KMS+1
!
!----------------------------------------------------------------------
      IF ( wrf_dm_on_monitor() ) THEN
        DO i = 14,99
          INQUIRE ( i , OPENED = opened )
          IF ( .NOT. opened ) THEN
            nunit_co2 = i
            GOTO 2014
          ENDIF
        ENDDO
        nunit_co2 = -1
 2014   CONTINUE
      ENDIF
        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(nunit_co2,FILE='co2_trans',                  &
               FORM='UNFORMATTED',STATUS='OLD',ERR=9014,IOSTAT=IERROR)
          REWIND NUNIT_CO2
        ENDIF


!----------------------------------------------------------------------
!
!***  READ IN PRE-COMPUTED CO2 TRANSMISSION DATA.
!
      RSZE(1) = LP1
      RSZE(2) = L
      RSZE(3) = LP1*LP1
!----------------------------------------------------------------------
!
      RSIZE = RSZE(1)
!
      DO KK=1,2
        IF( wrf_dm_on_monitor() )READ(NUNIT_CO2)(SGTMP(I,KK),I=1,RSIZE)
        CALL wrf_dm_bcast_real( SGTMP(1,KK), RSIZE )
      ENDDO
!
!----------------------------------------------------------------------
!
      RSIZE = RSZE(2)
!
      DO KK=1,6
        IF( wrf_dm_on_monitor() )READ(NUNIT_CO2)(CO21D(I,KK),I=1,RSIZE)
        CALL wrf_dm_bcast_real( CO21D(1,KK), RSIZE )
      ENDDO
!
!----------------------------------------------------------------------
!
      RSIZE = RSZE(3)
!
      DO KK=1,6
        IF( wrf_dm_on_monitor() )READ(NUNIT_CO2)(DATA2(I),I=1,RSIZE)
        CALL wrf_dm_bcast_real( DATA2(1), RSIZE )
        N=0
!
        DO I1=1,LP1
        DO I2=1,LP1
          N=N+1
          CO22D(I1,I2,KK)=DATA2(N)
        ENDDO
        ENDDO
!
      ENDDO

!
!    Deallocate before reading. This is required for nested domain init.
!    This is gopal's doing
!
      IF(ALLOCATED (CO251))DEALLOCATE(CO251)
      IF(ALLOCATED (CDT51))DEALLOCATE(CDT51)
      IF(ALLOCATED (C2D51))DEALLOCATE(C2D51)
      IF(ALLOCATED (CO258))DEALLOCATE(CO258)
      IF(ALLOCATED (CDT58))DEALLOCATE(CDT58)
      IF(ALLOCATED (C2D58))DEALLOCATE(C2D58)
      IF(ALLOCATED (STEMP))DEALLOCATE(STEMP)
      IF(ALLOCATED (GTEMP))DEALLOCATE(GTEMP)
      IF(ALLOCATED (CO231))DEALLOCATE(CO231)
      IF(ALLOCATED (CDT31))DEALLOCATE(CDT31)
      IF(ALLOCATED (C2D31))DEALLOCATE(C2D31)
      IF(ALLOCATED (CO238))DEALLOCATE(CO238)
      IF(ALLOCATED (CDT38))DEALLOCATE(CDT38)
      IF(ALLOCATED (C2D38))DEALLOCATE(C2D38)
      IF(ALLOCATED (CO271))DEALLOCATE(CO271)
      IF(ALLOCATED (CDT71))DEALLOCATE(CDT71)
      IF(ALLOCATED (C2D71))DEALLOCATE(C2D71)
      IF(ALLOCATED (CO278))DEALLOCATE(CO278)
      IF(ALLOCATED (CDT78))DEALLOCATE(CDT78)
      IF(ALLOCATED (C2D78))DEALLOCATE(C2D78)
      IF(ALLOCATED (CO2M51))DEALLOCATE(CO2M51)
      IF(ALLOCATED (CDTM51))DEALLOCATE(CDTM51)
      IF(ALLOCATED (C2DM51))DEALLOCATE(C2DM51)
      IF(ALLOCATED (CO2M58))DEALLOCATE(CO2M58)
      IF(ALLOCATED (CDTM58))DEALLOCATE(CDTM58)
      IF(ALLOCATED (C2DM58))DEALLOCATE(C2DM58)
!
!----------------------------------------------------------------------
!
      RSIZE = RSZE(1)
!
      DO KK=1,6
        IF( wrf_dm_on_monitor() )READ(NUNIT_CO2)(CO21D3(I,KK),I=1,RSIZE)
        CALL wrf_dm_bcast_real( CO21D3(1,KK), RSIZE )
      ENDDO
!
!----------------------------------------------------------------------
!
      DO KK=1,6
        IF( wrf_dm_on_monitor() )READ(NUNIT_CO2)(CO21D7(I,KK),I=1,RSIZE)
        CALL wrf_dm_bcast_real ( CO21D7(1,KK), RSIZE )
      ENDDO
!
!----------------------------------------------------------------------
      ALLOCATE(CO251(LP1,LP1))
      ALLOCATE(CDT51(LP1,LP1))
      ALLOCATE(C2D51(LP1,LP1))
      ALLOCATE(CO258(LP1,LP1))
      ALLOCATE(CDT58(LP1,LP1))
      ALLOCATE(C2D58(LP1,LP1))
      ALLOCATE(STEMP(LP1))
      ALLOCATE(GTEMP(LP1))
      ALLOCATE(CO231(LP1))
      ALLOCATE(CDT31(LP1))
      ALLOCATE(C2D31(LP1))
      ALLOCATE(CO238(LP1))
      ALLOCATE(CDT38(LP1))
      ALLOCATE(C2D38(LP1))
      ALLOCATE(CO271(LP1))
      ALLOCATE(CDT71(LP1))
      ALLOCATE(C2D71(LP1))
      ALLOCATE(CO278(LP1))
      ALLOCATE(CDT78(LP1))
      ALLOCATE(C2D78(LP1))
      ALLOCATE(CO2M51(L))
      ALLOCATE(CDTM51(L))
      ALLOCATE(C2DM51(L))
      ALLOCATE(CO2M58(L))
      ALLOCATE(CDTM58(L))
      ALLOCATE(C2DM58(L))
!----------------------------------------------------------------------
!
      DO K=1,LP1
        STEMP(K) = SGTMP(K,1)
        GTEMP(K) = SGTMP(K,2)
      ENDDO
!
      DO K=1,L
        CDTM51(K) = CO21D(K,1)
        CO2M51(K) = CO21D(K,2)
        C2DM51(K) = CO21D(K,3)
        CDTM58(K) = CO21D(K,4)
        CO2M58(K) = CO21D(K,5)
        C2DM58(K) = CO21D(K,6)
      ENDDO
!
      DO J=1,LP1
      DO I=1,LP1
        CDT51(I,J) = CO22D(I,J,1)
        CO251(I,J) = CO22D(I,J,2)
        C2D51(I,J) = CO22D(I,J,3)
        CDT58(I,J) = CO22D(I,J,4)
        CO258(I,J) = CO22D(I,J,5)
        C2D58(I,J) = CO22D(I,J,6)
      ENDDO
      ENDDO
!
      DO K=1,LP1
        CDT31(K) = CO21D3(K,1)
        CO231(K) = CO21D3(K,2)
        C2D31(K) = CO21D3(K,3)
        CDT38(K) = CO21D3(K,4)
        CO238(K) = CO21D3(K,5)
        C2D38(K) = CO21D3(K,6)
      ENDDO
!
      DO K=1,LP1
        CDT71(K) = CO21D7(K,1)
        CO271(K) = CO21D7(K,2)
        C2D71(K) = CO21D7(K,3)
        CDT78(K) = CO21D7(K,4)
        CO278(K) = CO21D7(K,5)
        C2D78(K) = CO21D7(K,6)
      ENDDO
!
!----------------------------------------------------------------------
      IF(wrf_dm_on_monitor())WRITE(0,66)NUNIT_CO2
   66 FORMAT('----READ CO2 TRANSMISSION FUNCTIONS FROM UNIT ',I2)
!----------------------------------------------------------------------
      IF( wrf_dm_on_monitor() )THEN
        CLOSE(nunit_co2)
      ENDIF
      RETURN
!
9014 CONTINUE
     WRITE(errmess,'(A51,I4)')'module_ra_hwrf: error reading co2_trans on unit ',nunit_co2
     CALL wrf_error_fatal(errmess)
!----------------------------------------------------------------------
      END SUBROUTINE CONRAD
!----------------------------------------------------------------------
!
      END MODULE module_RA_HWRF
!
!----------------------------------------------------------------------

