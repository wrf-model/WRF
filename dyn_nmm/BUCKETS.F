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
     &                  ,T,TLMAX,TLMIN                                  &
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
!   MACHINE : IBM SP
!$$$  
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
      REAL,INTENT(IN) :: TSPH,TSTART
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
     &                                              ,SFCLHX,SFCSHX      &
     &                                              ,SFCUVX,SNOPCX      &
     &                                              ,SSROFF,SUBSHX
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: NCFRST,NCFRCV
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: TLMAX,TLMIN
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: T
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: TCUCN      &
     &                                                      ,TRAIN
!
      LOGICAL,INTENT(IN) :: RESTART
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES 
!-----------------------------------------------------------------------
!
      INTEGER :: I,J,K,NTSD_BUCKET,NTSPH
      LOGICAL  WRF_DM_ON_MONITOR
      EXTERNAL WRF_DM_ON_MONITOR
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      NTSD_BUCKET=NTSD
      IF(RESTART.AND.TSTART>0.)THEN
        NTSD_BUCKET=NTSD-1
      ENDIF
!
!-----------------------------------------------------------------------
!***  TOTAL AND CONVECTIVE PRECIPITATION ARRAYS.
!***  TOTAL SNOW AND SNOW MELT ARRAYS.
!***  STORM SURFACE AND BASE GROUND RUN OFF ARRAYS.
!***  EVAPORATION ARRAYS.
!-----------------------------------------------------------------------
!
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
      IF(MOD(NTSD_BUCKET,NHEAT)==0)THEN
        AVCNVC=0.
        AVRAIN=0.
!
        DO J=JTS,JTE
        DO K=KTS,KTE
        DO I=ITS,ITE
          TRAIN(I,K,J)=0.
          TCUCN(I,K,J)=0.
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
        ENDDO
        ENDDO
!
        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('RESET MAX/MIN TEMPERTURES')
        ENDIF
      ENDIF
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        TLMAX(I,J)=MAX(TLMAX(I,J),T(I,1,J))
        TLMIN(I,J)=MIN(TLMIN(I,J),T(I,1,J))
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE BUCKETS
!
!-----------------------------------------------------------------------
