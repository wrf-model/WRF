!-----------------------------------------------------------------------
!
!NCEP_MESO:MODEL_LAYER: ACCUMULATION BUCKETS
!
!-----------------------------------------------------------------------
      SUBROUTINE BUCKETS(NTSD,NPREC,NSRFC,NRDSW,NRDLW,NHEAT,NPHS        &
     &                  ,ACPREC,CUPREC,ACSNOW,ACSNOM,SSROFF,BGROFF      &
     &                  ,SFCEVP,POTEVP,SFCSHX,SFCLHX,SUBSHX,SNOPCX      &
     &                  ,SFCUVX,POTFLX                                  &
     &                  ,ARDSW,ASWIN,ASWOUT,ASWTOA                      &
     &                  ,ARDLW,ALWIN,ALWOUT,ALWTOA                      &
     &                  ,AVCNVC,AVRAIN,TCUCN,TRAIN                      &
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
!      ACPREC - ACCUMULATED TOTAL PRECIPITATION (M)
!      CUPREC - ACCUMULATED CONVECTIVE PRECIPITATION (M)
!      ACSNOW - ACCUMULATED SNOWFALL (M)
!      ACSNOM - ACCUMULATED SNOWMELT (M)
!      SSROFF - ACCUMULATED SURFACE RUNOFF
!      BGROFF - ACCUMULATED BELOW GROUND RUNOFF
!      SFCEVP - ACCUMULATED SURFACE EVAPORATION
!      POTEVP - ACCUMULATED POTENTIAL EVAPORATION
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
      INTEGER,INTENT(IN) :: NHEAT,NPHS,NPREC,NRDLW,NRDSW,NSRFC,NTSD     &
                           ,IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE 
!
      REAL,INTENT(OUT) :: ARDLW,ARDSW,AVCNVC,AVRAIN
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: ACPREC,ACSNOM      &
     &                                              ,ACSNOW,ALWIN       &
     &                                              ,ALWOUT,ALWTOA      &
     &                                              ,ASWIN,ASWOUT       &
     &                                              ,ASWTOA,BGROFF      &
     &                                              ,CUPREC,POTEVP      &
     &                                              ,POTFLX,SFCEVP      &
     &                                              ,SFCLHX,SFCSHX      &
     &                                              ,SFCUVX,SNOPCX      &
     &                                              ,SSROFF,SUBSHX
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: TCUCN      &
     &                                                      ,TRAIN
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES 
!-----------------------------------------------------------------------
!
      INTEGER :: I,J,K
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!***  TOTAL AND CONVECTIVE PRECIPITATION ARRAYS.
!***  TOTAL SNOW AND SNOW MELT ARRAYS.
!***  STORM SURFACE AND BASE GROUND RUN OFF ARRAYS.
!
!     IF(MOD(NTSD,NPREC)<NPHS)THEN
      IF(MOD(NTSD,NPREC)==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          ACPREC(I,J)=0.
          CUPREC(I,J)=0.
          ACSNOW(I,J)=0.
          ACSNOM(I,J)=0.
          SSROFF(I,J)=0.
          BGROFF(I,J)=0.
        ENDDO
        ENDDO
!
        CALL WRF_MESSAGE('ZEROED OUT PRECIP/RUNOFF ARRAYS')
!
      ENDIF
!
!***  EVAPORATION ARRAYS.
!***  SFC FLUX ARRAYS.
!
!     IF(MOD(NTSD,NSRFC)<NPHS)THEN
      IF(MOD(NTSD,NSRFC)==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          SFCEVP(I,J)=0.
          POTEVP(I,J)=0.
          SFCSHX(I,J)=0.
          SFCLHX(I,J)=0.
          SUBSHX(I,J)=0.
          SNOPCX(I,J)=0.
          SFCUVX(I,J)=0.
          POTFLX(I,J)=0.
        ENDDO
        ENDDO
!
        CALL WRF_MESSAGE('ZEROED OUT SFC EVAP/FLUX ARRAYS')
!
      ENDIF
!
!***  SHORTWAVE FLUX ACCUMULATION ARRAYS.
!
!     IF(MOD(NTSD,NRDSW)<NPHS)THEN
      IF(MOD(NTSD,NRDSW)==0)THEN
        ARDSW=0.
        DO J=JTS,JTE
        DO I=ITS,ITE
          ASWIN(I,J) =0.
          ASWOUT(I,J)=0.
          ASWTOA(I,J)=0.
        ENDDO
        ENDDO
!
        CALL WRF_MESSAGE('ZEROED OUT ACCUMULATED SHORTWAVE FLUX ARRAYS')
!
      ENDIF
!
!***  LONGWAVE FLUX ACCUMULATION ARRAYS.
!
!     IF(MOD(NTSD,NRDLW)<NPHS)THEN
      IF(MOD(NTSD,NRDLW)==0)THEN
        ARDLW=0.
        DO J=JTS,JTE
        DO I=ITS,ITE
          ALWIN(I,J) =0.
          ALWOUT(I,J)=0.
          ALWTOA(I,J)=0.
        ENDDO
        ENDDO
!
        CALL WRF_MESSAGE('ZEROED OUT ACCUMULATED LONGWAVE FLUX ARRAYS')
!
      ENDIF
!
!***  GRID-SCALE AND CONVECTIVE (LATENT) HEATING ARRAYS.
!
!     IF(MOD(NTSD,NHEAT)<NPHS)THEN
      IF(MOD(NTSD,NHEAT)==0)THEN
      write(0,*)' buckets ntsd=',ntsd,' nheat=',nheat
      write(0,*)' zeroing train and tcucn'
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
        CALL WRF_MESSAGE('ZEROED OUT ACCUMULATED LATENT HEATING ARRAYS')
!
      ENDIF
!-----------------------------------------------------------------------
!
      END SUBROUTINE BUCKETS
!
!-----------------------------------------------------------------------
