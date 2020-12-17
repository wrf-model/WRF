!-----------------------------------------------------------------------
!
!NCEP_MESO:MODEL_LAYER: PHYSICS
!
!----------------------------------------------------------------------
#include "nmm_loop_basemacros.h"
#include "nmm_loop_macros.h"
!-----------------------------------------------------------------------
!
      MODULE MODULE_PRECIP_ADJUST
!
! This module contains 3 subroutines:
!     READPCP
!     CHKSNOW
!     ADJPPT
!-----------------------------------------------------------------------
!***
!***  Specify the diagnostic point here: (i,j) and the processor number.
!***  Remember that in WRF, local and global (i,j) are the same, so don't
!***  use the "local(i,j)" output from glb2loc.f; use the GLOBAL (I,J)
!***  and the PE_WRF.
!***
!
      INTEGER :: ITEST=346,JTEST=256,TESTPE=53
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
      SUBROUTINE READPCP(PPTDAT,DDATA,LSPA                              &
     &  ,IDS,IDE,JDS,JDE,KDS,KDE                                        &
     &  ,IMS,IME,JMS,JME,KMS,KME                                        &
     &  ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!     ****************************************************************
!     *                                                              *
!     *   PRECIPITATION ASSIMILATION INITIALIZATION.                 *
!     *    READ IN PRECIP ANALYSIS AND DATA MASK AND SET UP ALL      *
!     *    APPROPRIATE VARIABLES.                                    *
!     *                   MIKE BALDWIN, MARCH 1994                   *
!     *                   Adapted to 2-D code, Ying Lin, Mar 1996    *
!     *                   For WRF/NMM: Y.Lin Mar 2005                *
!     *                                                              *
!     ****************************************************************
!-----------------------------------------------------------------------
!
! READ THE BINARY VERSION OF THE PRECIP ANALYSIS.
!
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE
      REAL,DIMENSION(IDS:IDE,JDS:JDE) :: TEMPG
      REAL,DIMENSION(IMS:IME,JMS:JME) :: TEMPL
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: DDATA, LSPA
      REAL,DIMENSION(IMS:IME,JMS:JME,3),INTENT(OUT) :: PPTDAT
      INTEGER :: I, J, IHR
      INTEGER :: MYPE
      CHARACTER*256 :: MESSAGE
!
! Get the value of MYPE:
!
      CALL WRF_GET_MYPROC(MYPE)
!
      TEMPG=999.
!
      DO IHR=1,3
        IF(MYPE==0)THEN
          READ(40+IHR) ((TEMPG(I,J),I=IDS,IDE-1),J=JDS,JDE-1)
          WRITE(MESSAGE,*) 'IHR=', IHR, ' FINISHED READING PCP TO TEMPG'
          CALL WRF_MESSAGE(MESSAGE)
          CLOSE(40+IHR)
!
          DO J=JDS,JDE-1
            DO I=IDS,IDE-1
! In the binary version of the precip data, missing data are denoted as '999.'
! Convert the valid data from mm to m:
              IF (TEMPG(I,J).LT.900.) TEMPG(I,J)=TEMPG(I,J)*0.001
            ENDDO
          ENDDO
        ENDIF
!
! Distribute to local temp array:
        CALL DSTRB(TEMPG,TEMPL,1,1,1,1,1                                &
     &,                IDS,IDE,JDS,JDE,KDS,KDE                          &
     &,                IMS,IME,JMS,JME,KMS,KME                          &
     &,                ITS,ITE,JTS,JTE,KTS,KTE)
!
! Place into correct hour slot in PPTDAT:
        DO J=JMS,JME
          DO I=IMS,IME
            PPTDAT(I,J,IHR)=TEMPL(I,J)
          ENDDO
        ENDDO
!
        IF(MYPE==TESTPE)THEN
          WRITE(MESSAGE,*) 'ADJPPT-READPCP, IHR',IHR, 'PPTDAT=',        &
     &      PPTDAT(ITEST,JTEST,IHR)
          CALL WRF_MESSAGE(MESSAGE)
        ENDIF

      ENDDO
!
! Give DDATA (hourly precipitation analysis partitioned into each physics
! timestep; partitioning done in ADJPPT) an initial value of 999, because
! TURBL/SURFCE is called before ADJPPT.  Also initialize LSPA to zero.
!
      DDATA=999.
      LSPA=0.
!
      RETURN
      END SUBROUTINE READPCP
!
      SUBROUTINE CHKSNOW(NTSD,DT,NPHS,SR,PPTDAT                         &
     &  ,IDS,IDE,JDS,JDE,KDS,KDE                                        &
     &  ,IMS,IME,JMS,JME,KMS,KME                                        &
     &  ,ITS,ITE,JTS,JTE,KTS,KTE)
!
! AT THE FIRST PHYSICS TIME STEP AFTER THE TOP OF EACH HOUR, CHECK THE SNOW
! ARRAY AGAINST THE SR (SNOW/TOTAL PRECIP RATIO).  IF SR .GE. 0.9, SET THIS
! POINT TO MISSING (SO WE WON'T DO SNOW ADJUSTMENT HERE).
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: NTSD,NPHS
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: SR
      REAL,DIMENSION(IMS:IME,JMS:JME,3),INTENT(INOUT) :: PPTDAT
      REAL,INTENT(IN) :: DT
      REAL :: TIMES
      INTEGER :: I, J, IHR
      INTEGER :: MYPE
      CHARACTER*256 :: MESSAGE
!-----------------------------------------------------------------------
      TIMES=NTSD*DT
      IF (MOD(TIMES,3600.) < NPHS*DT) THEN
        IHR=INT(TIMES)/3600+1
        IF (IHR > 3) go to 10
        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
          IF (SR(I,J) >= 0.9) PPTDAT(I,J,IHR) = 999.
        ENDDO
        ENDDO
!
! Get the value of MYPE:
!
        CALL WRF_GET_MYPROC(MYPE)
!
        IF (MYPE==TESTPE) THEN
          WRITE(MESSAGE,1010) TIMES,SR(ITEST,JTEST)
 1010     FORMAT('ADJPPT-CHKSNOW: TIMES, SR=',F6.0,1X,F6.4)
          CALL WRF_MESSAGE(MESSAGE)
        ENDIF
      ENDIF
 10   CONTINUE
      RETURN
      END SUBROUTINE CHKSNOW
!
      SUBROUTINE ADJPPT(NTSD,DT,NPHS,PREC,LSPA,PPTDAT,DDATA             &
     &  ,IDS,IDE,JDS,JDE,KDS,KDE                                        &
     &  ,IMS,IME,JMS,JME,KMS,KME                                        &
     &  ,ITS,ITE,JTS,JTE,KTS,KTE)

!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    ADJPPT    PRECIPITATION/CLOUD ADJUSTMENT
!    PRGRMMR:    Y. LIN    ORG: W/NP22     DATE: 2005/03/30
!     
! ABSTRACT:
!     ADJPPT  MAKES ADJUSTMENT TO MODEL'S TEMPERATURE, MOISTURE, HYDROMETEOR
!     FIELDS TO BE MORE CONSISTENT WITH THE OBSERVED PRECIPITATION AND CLOUD
!     TOP PRESSURE
!     
!     FOR NOW, AS A FIRST STEP, JUST PARTITION THE INPUT HOURLY PRECIPITATION
!     OBSERVATION INTO TIME STEPS, AND FEED IT INTO THE SOIL.
! PROGRAM HISTORY LOG:
!
!   2005/03/30  LIN      - BAREBONES PRECIPITATION PARTITION/FEEDING TO GROUND
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM 
!$$$  
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: NPHS, NTSD
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE
      REAL,INTENT(IN) :: DT
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: PREC
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: DDATA, LSPA
      REAL,DIMENSION(IMS:IME,JMS:JME,3),INTENT(OUT) :: PPTDAT
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
!-----------------------------------------------------------------------
      REAL :: DTPHS, FRACT, FRACT1, FRACT2, TIMES, TPHS1, TPHS2
      INTEGER :: I, J, IHR, IHR1, IHR2, NTSP
      INTEGER :: MYPE
      CHARACTER*256 :: MESSAGE
!
! Get the value of MYPE:
!
      CALL WRF_GET_MYPROC(MYPE)
!
      TIMES=NTSD*DT
      IHR=INT(TIMES)/3600+1
! Size of physics time step:
      DTPHS=NPHS*DT
!
! Compute the beginning and ending time of the current physics time step,
! TPHS1 and TPHS2:
!  
      NTSP=NTSD/NPHS+1
      TPHS1=(NTSP-1)*DTPHS
      TPHS2=NTSP*DTPHS
!
      IHR1=INT(TPHS1)/3600+1
      IHR2=INT(TPHS2)/3600+1
!
! Fraction of an hour that falls into IHR1 and IHR2.  Note that IHR1 and IHR2
! might be identical.
      IF (IHR1 > 3) THEN 
        GO TO 200
      ELSEIF (IHR2 > 3) THEN
        IHR2=3
        FRACT1=(3600.- MOD(INT(TPHS1),3600))/3600.
        FRACT2=0.
      ELSEIF (IHR1 .EQ. IHR2) THEN
         FRACT1=0.5*DTPHS/3600.
         FRACT2=FRACT1
      ELSE
         FRACT1=(3600.- MOD(INT(TPHS1),3600))/3600.
         FRACT2=FLOAT(MOD(INT(TPHS2),3600))/3600.
      ENDIF
!
      FRACT=FRACT1 + FRACT2
!
      IF (MYPE==TESTPE) THEN
         WRITE(MESSAGE,1010) NTSD,NTSP,TIMES,IHR1,IHR2,TPHS1,TPHS2,      &
      &    FRACT1,FRACT2
 1010    FORMAT('ADJPPT: NTSD,NTSP,TIMES=',I4,1X,I4,1X,F6.0,' IHR1,IHR2=' &
      &   ,I1,1X,I1,' TPHS1,TPHS2=',F6.0,1X,F6.0,' FRACT1,FRACT2='        &
      &   ,2(1X,F6.4))
        CALL WRF_MESSAGE(MESSAGE)
      ENDIF
!
!-----------------------------------------------------------------------
!   FRACT1/2 IS THE FRACTION OF IHR1/2'S PRECIP THAT WE WANT FOR
!   THIS ADJUSTMENT (assuming that the physics time step spans over
!   IHR1 and IHR2.  If not, then IHR1=IHR2).
!-----------------------------------------------------------------------
!   SET UP OBSERVED PRECIP FOR THIS TIMESTEP IN DDATA
!-----------------------------------------------------------------------
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
! Note sometimes IHR1=IHR2.  
        IF (PPTDAT(I,J,IHR1).GT.900..OR.PPTDAT(I,J,IHR2).GT.900.) THEN
          DDATA(I,J) = 999.
          LSPA(I,J) = LSPA(I,J) + PREC(I,J)
          GO TO 100
        ELSE
          IF (IHR2 .LE. 3) then
            DDATA(I,J) = PPTDAT(I,J,IHR1)*FRACT1                        &
     &                 + PPTDAT(I,J,IHR2)*FRACT2
          ELSE
            DDATA(I,J) = PPTDAT(I,J,IHR1)*FRACT1 
          ENDIF
!
           LSPA(I,J) = LSPA(I,J) + DDATA(I,J)
        ENDIF
        IF (I.EQ.ITEST .AND. J.EQ.JTEST .AND. MYPE.EQ.TESTPE) THEN
          WRITE(MESSAGE,1020) DDATA(I,J), PREC(I,J), LSPA(I,J)
 1020     FORMAT('ADJPPT: DDATA=',E12.6, ' PREC=',E12.6,' LSPA=',E12.6)
          CALL WRF_MESSAGE(MESSAGE)
        ENDIF
!
 100    CONTINUE
      ENDDO
      ENDDO
!
 200  CONTINUE

      RETURN
      END SUBROUTINE ADJPPT
END MODULE module_PRECIP_ADJUST
