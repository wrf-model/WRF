!-----------------------------------------------------------------------
!
!NCEP_MESO:MODEL_LAYER: ACCUMULATION BUCKETS
!
!-----------------------------------------------------------------------
      SUBROUTINE BUCKETS(NTSD,NPREC,NSRFC,NRDSW,NRDLW                   &
     &                  ,RESTART,TSTART                                 &
     &                  ,NCLOD,NHEAT,NPHS,TSPH                          &
     &                  ,ACPREC,CUPREC,ACSNOW,ACSNOM,SSROFF,BGROFF      &
     &                  ,SFCEVP,POTEVP,SFCSHX,SFCLHX,SUBSHX,SNOPCX      &
     &                  ,SFCUVX,POTFLX                                  &
     &                  ,ARDSW,ASWIN,ASWOUT,ASWTOA                      &
     &                  ,ARDLW,ALWIN,ALWOUT,ALWTOA                      &
     &                  ,ACFRST,NCFRST,ACFRCV,NCFRCV                    &
     &                  ,AVCNVC,AVRAIN,TCUCN,TRAIN                      &
     &                  ,ASRFC                                          &
     &                  ,T,TLMAX,TLMIN,TSHLTR,PSHLTR,QSHLTR             &
     &                  ,T02_MAX,T02_MIN,RH02_MAX,RH02_MIN              &
     &                  ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    BUCKETS     EMPTY ACCUMULATION BUCKETS WHEN NEEDED
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 04-08-18
!     
! ABSTRACT:
!     VARIOUS ACCUMULATING QUANTITIES NEED TO BE RESET TO ZERO AT
!     SPECIFIED INTERVALS.
!     
! USAGE: CALL BUCKETS FROM SOLVE_NMM
!   INPUT ARGUMENT LIST:
!        NTSD - CURRENT TIMESTEP
!       NPREC - NUMBER OF TIMESTEPS BETWEEN EMPTYING BUCKETS FOR PRECIP
!       NHEAT - NUMBER OF TIMESTEPS BETWEEN EMPTYING BUCKETS FOR 
!               LATENT HEATING
!       NCNVC - NUMBER OF TIMESTEPS BETWEEN CALLS TO CONVECTION
!       TSPH - NUMBER OF DYNAMICS TIMESTEPS PER HOUR
!      ACPREC - ACCUMULATED TOTAL PRECIPITATION (M)
!      CUPREC - ACCUMULATED CONVECTIVE PRECIPITATION (M)
!      ACSNOW - ACCUMULATED SNOWFALL (M)
!      ACSNOM - ACCUMULATED SNOWMELT (M)
!      SSROFF - ACCUMULATED SURFACE RUNOFF
!      BGROFF - ACCUMULATED BELOW GROUND RUNOFF
!      SFCEVP - ACCUMULATED SURFACE EVAPORATION
!      POTEVP - ACCUMULATED POTENTIAL EVAPORATION
!           T - TEMPERATURE
!       TLMAX - MAX TEMPERATURE EACH HOUR IN LOWEST LAYER
!       TLMIN - MIN TEMPERATURE EACH HOUR IN LOWEST LAYER
!      TSHLTR - SHELTER LEVEL (2m) POTENTIAL TEMPERATURE (K)
!      PSHLTR - SHELTER LEVEL (2m) PRESSURE (Pa)
!      QSHLTR - SHELTER LEVEL (2m) SPECIFIC HUMIDITY (kg/kg)
!     T02_MAX - 2m HOURLY MAX TEMPERATURE (K)
!     T02_MIN - 2m HOURLY MIN TEMPERATURE (K)
!    RH02_MAX - 2m HOURLY MAX RELATIVE HUMIDITY (fraction)
!    RH02_MIN - 2m HOURLY MIN RELATIVE HUMIDITY (fraction)
!  
!   OUTPUT ARGUMENT LIST:  THE ACCUMULATED QUANTITIES
!     
!   OUTPUT FILES:  NONE
!     
!   SUBPROGRAMS CALLED:  NONE
!  
!   UNIQUE: NONE
!  
!   LIBRARY: NONE
!  
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM 
!$$$  
!-----------------------------------------------------------------------
!
      USE MODULE_MODEL_CONSTANTS,ONLY: CP,CPV,R_D,R_V,RCP
      USE MODULE_MP_ETANEW,ONLY: C1XPVS,C1XPVS0,C2XPVS,C2XPVS0          &
                                ,FPVS,FPVS0,NX,TBPVS,TBPVS0             &
                                ,GPVS
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!*** ARGUMENTS
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: NCLOD,NHEAT,NPHS,NPREC,NRDLW,NRDSW          &
                           ,NSRFC,NTSD                                  &
                           ,IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE 
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: NCFRST,NCFRCV
!
      REAL,INTENT(IN) :: TSPH,TSTART
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: PSHLTR,QSHLTR,TSHLTR
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: T
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: TLMAX,TLMIN
!
      REAL,INTENT(OUT) :: ARDLW,ARDSW,ASRFC,AVCNVC,AVRAIN
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: ACPREC,ACSNOM      &
     &                                              ,ACSNOW,ALWIN       &
     &                                              ,ACFRST,ACFRCV      &
     &                                              ,ALWOUT,ALWTOA      &
     &                                              ,ASWIN,ASWOUT       &
     &                                              ,ASWTOA,BGROFF      &
     &                                              ,CUPREC,POTEVP      &
     &                                              ,POTFLX,SFCEVP      &
     &                                              ,RH02_MAX,RH02_MIN  &
     &                                              ,SFCLHX,SFCSHX      &
     &                                              ,SFCUVX,SNOPCX      &
     &                                              ,SSROFF,SUBSHX      &
     &                                              ,T02_MAX,T02_MIN
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(OUT) :: TCUCN      &
     &                                                      ,TRAIN
!
      LOGICAL,INTENT(IN) :: RESTART
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES 
!-----------------------------------------------------------------------
!
      INTEGER :: I,J,K,NTSD_BUCKET,NTSPH
      LOGICAL ::  FIRST_PASS=.TRUE.
      LOGICAL ::  WRF_DM_ON_MONITOR
      EXTERNAL WRF_DM_ON_MONITOR
!
      REAL :: CAPPA_MOIST,RH02,SAT_VAPOR_PRESS,VAPOR_PRESS
      REAL,SAVE :: CP_FACTOR,EPSILON,ONE_MINUS_EPSILON,R_FACTOR
      REAL,SAVE :: P00_INV=1.E-5
!
      REAL,DIMENSION(ITS:ITE,JTS:JTE) :: T02
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!***  COMPUTE AND SAVE THE FACTORS IN R AND CP TO ACCOUNT FOR
!***  WATER VAPOR IN THE AIR.
!*** 
!***  RECALL: R  = Rd * (1. + Q * (1./EPSILON - 1.))
!***          CP = CPd * (1. + Q * (CPv/CPd - 1.))
!
      IF(FIRST_PASS)THEN
        FIRST_PASS=.FALSE.
!
        EPSILON=R_D/R_V
        ONE_MINUS_EPSILON=1.-EPSILON
        R_FACTOR=1./EPSILON-1.
        CP_FACTOR=CPV/CP-1.
! Make sure saturation vapor pressure lookup table is initialized
        CALL GPVS 
      ENDIF
!
!-----------------------------------------------------------------------
!
      NTSD_BUCKET=NTSD
!
!-----------------------------------------------------------------------
!***  TOTAL AND CONVECTIVE PRECIPITATION ARRAYS.
!***  TOTAL SNOW AND SNOW MELT ARRAYS.
!***  STORM SURFACE AND BASE GROUND RUN OFF ARRAYS.
!***  EVAPORATION ARRAYS.
!-----------------------------------------------------------------------
!
!     IF(MOD(NTSD,NPREC)<NPHS)THEN
      IF(MOD(NTSD_BUCKET,NPREC)==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          ACPREC(I,J)=0.
          CUPREC(I,J)=0.
          ACSNOW(I,J)=0.
          ACSNOM(I,J)=0.
          SSROFF(I,J)=0.
          BGROFF(I,J)=0.
          SFCEVP(I,J)=0.
          POTEVP(I,J)=0.
        ENDDO
        ENDDO
!
        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('ZEROED OUT PRECIP/RUNOFF ARRAYS')
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  SFC FLUX ARRAYS.
!-----------------------------------------------------------------------
!
!     IF(MOD(NTSD,NSRFC)<NPHS)THEN
      IF(MOD(NTSD_BUCKET,NSRFC)==0)THEN
        ASRFC=0.
        DO J=JTS,JTE
        DO I=ITS,ITE
          SFCSHX(I,J)=0.
          SFCLHX(I,J)=0.
          SUBSHX(I,J)=0.
          SNOPCX(I,J)=0.
          SFCUVX(I,J)=0.
          POTFLX(I,J)=0.
        ENDDO
        ENDDO
!
        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('ZEROED OUT SFC EVAP/FLUX ARRAYS')
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  SHORTWAVE FLUX ACCUMULATION ARRAYS.
!-----------------------------------------------------------------------
!
!     IF(MOD(NTSD,NRDSW)<NPHS)THEN
      IF(MOD(NTSD_BUCKET,NRDSW)==0)THEN
        ARDSW=0.
        DO J=JTS,JTE
        DO I=ITS,ITE
          ASWIN(I,J) =0.
          ASWOUT(I,J)=0.
          ASWTOA(I,J)=0.
        ENDDO
        ENDDO
!
        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('ZEROED OUT ACCUMULATED SHORTWAVE FLUX ARRAYS')
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  LONGWAVE FLUX ACCUMULATION ARRAYS.
!-----------------------------------------------------------------------
!
!     IF(MOD(NTSD,NRDLW)<NPHS)THEN
      IF(MOD(NTSD_BUCKET,NRDLW)==0)THEN
        ARDLW=0.
        DO J=JTS,JTE
        DO I=ITS,ITE
          ALWIN(I,J) =0.
          ALWOUT(I,J)=0.
          ALWTOA(I,J)=0.
        ENDDO
        ENDDO
!
        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('ZEROED OUT ACCUMULATED LONGWAVE FLUX ARRAYS')
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  TIME-AVERAGED CLOUD FRACTION ARRAYS.
!-----------------------------------------------------------------------
!
!     IF(MOD(NTSD,NCLOD)<NPHS)THEN
      IF(MOD(NTSD_BUCKET,NCLOD)==0)THEN
!***
  !--- Ferrier 11/2/05:  Right now no accumulator variable is used (e.g.,
  !    "ACLOD"), but instead the 2D arrays NCFRST & NCFRCV are used.  These
  !    can be removed later to streamline the code.
!***
        DO J=JTS,JTE
        DO I=ITS,ITE
          ACFRCV(I,J)=0.
          ACFRST(I,J)=0.
          NCFRCV(I,J)=0
          NCFRST(I,J)=0
        ENDDO
        ENDDO
!
        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('ZEROED OUT ACCUMULATED CLOUD FRACTION ARRAYS')
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  GRID-SCALE AND CONVECTIVE (LATENT) HEATING ARRAYS.
!-----------------------------------------------------------------------
!
!     IF(MOD(NTSD,NHEAT)<NPHS)THEN
      IF(MOD(NTSD_BUCKET,NHEAT)==0)THEN
        AVCNVC=0.
        AVRAIN=0.
!
        DO K=KTS,KTE
        DO J=JTS,JTE
        DO I=ITS,ITE
          TRAIN(I,J,K)=0.
          TCUCN(I,J,K)=0.
        ENDDO
        ENDDO
        ENDDO
!
        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('ZEROED OUT ACCUMULATED LATENT HEATING ARRAYS')
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  MAX/MIN TEMPERATURES
!-----------------------------------------------------------------------
!
      NTSPH=NINT(TSPH)
      IF(MOD(NTSD_BUCKET,NTSPH)==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TLMAX(I,J)=-999.
          TLMIN(I,J)=999.
          T02_MAX(I,J)=-999.
          T02_MIN(I,J)=999.
        ENDDO
        ENDDO
!
        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('RESET MAX/MIN TEMPERATURES')
        ENDIF
      ENDIF
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        TLMAX(I,J)=MAX(TLMAX(I,J),T(I,J,1))         !<--- Hourly max lowest layer T
        TLMIN(I,J)=MIN(TLMIN(I,J),T(I,J,1))         !<--- Hourly min lowest layer T
!
        CAPPA_MOIST=RCP*(1.+QSHLTR(I,J)*R_FACTOR)/(1.+QSHLTR(I,J)*CP_FACTOR)
        T02(I,J)=TSHLTR(I,J)*(P00_INV*PSHLTR(I,J))**CAPPA_MOIST
!
        IF(NTSD>0)THEN
          T02_MAX(I,J)=MAX(T02_MAX(I,J),T02(I,J))     !<--- Hourly max shelter T
          T02_MIN(I,J)=MIN(T02_MIN(I,J),T02(I,J))     !<--- Hourly min shelter T
        ENDIF
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  MAX/MIN RELATIVE HUMIDITY
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD_BUCKET,NTSPH)==0.OR.NTSD==1)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          RH02_MAX(I,J)=-999.
          RH02_MIN(I,J)=999.
        ENDDO
        ENDDO
!
        IF ( WRF_DM_ON_MONITOR() ) THEN
          CALL WRF_MESSAGE('RESET MAX/MIN RH')
        ENDIF
      ENDIF
!
      IF(NTSD>0)THEN
!
        DO J=JTS,JTE
        DO I=ITS,ITE
          VAPOR_PRESS=PSHLTR(I,J)*QSHLTR(I,J)/                          &
                     (EPSILON+QSHLTR(I,J)*ONE_MINUS_EPSILON)
!
!         IF(T02(I,J)>273.15)THEN
            SAT_VAPOR_PRESS=1.E3*FPVS0(T02(I,J))
!         ELSE
!           SAT_VAPOR_PRESS=1.E3*FPVS(T02(I,J))
!         ENDIF
!
          RH02=MIN(VAPOR_PRESS/SAT_VAPOR_PRESS,0.99)
!
          RH02_MAX(I,J)=MAX(RH02_MAX(I,J),RH02)     !<--- Hourly max shelter RH
          RH02_MIN(I,J)=MIN(RH02_MIN(I,J),RH02)     !<--- Hourly min shelter RH
        ENDDO
        ENDDO
!
      ELSE                         !<-- If timestep is 0, simply set max/min to zero.
        DO J=JTS,JTE
        DO I=ITS,ITE
          RH02_MAX(I,J)=0.
          RH02_MIN(I,J)=0.
        ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE BUCKETS
!
!-----------------------------------------------------------------------
