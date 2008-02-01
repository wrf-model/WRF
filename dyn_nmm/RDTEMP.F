!
!NCEP_MESO:MODEL_LAYER: PHYSICS
!
!***********************************************************************
      SUBROUTINE RDTEMP(NTSD,DT,JULDAY,JULYR,XTIME,IHRST,GLAT,GLON      &
     &                 ,CZEN,CZMEAN,T,RSWTT,RLWTT,HBM2                  &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    RDTEMP      RADIATIVE TEMPERATURE CHANGE
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 93-12-29
!     
! ABSTRACT:
!     RDTEMP APPLIES THE TEMPERATURE TENDENCIES DUE TO
!     RADIATION AT ALL LAYERS AT EACH ADJUSTMENT TIME STEP
!     
! PROGRAM HISTORY LOG:
!   87-09-??  BLACK      - ORIGINATOR
!   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   95-11-20  ABELES     - PARALLEL OPTIMIZATION
!   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
!   02-06-07  BLACK      - WRF CODING STANDARDS
!   02-09-09  WOLFE      - CONVERTING TO GLOBAL INDEXING
!   05-12-19  BLACK      - CONVERTED FROM IKJ TO IJK
!     
! USAGE: CALL RDTEMP FROM SUBROUTINE SOLVE_RUNSTREAM
!  
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$  
!-----------------------------------------------------------------------
      USE MODULE_MPP
      USE MODULE_RA_GFDLETA,ONLY : CAL_MON_DAY,ZENITH
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: IHRST,JULDAY,JULYR,NTSD
!
      REAL,INTENT(IN) :: DT,XTIME
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: CZMEAN,GLAT,GLON    &
     &                                             ,HBM2
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: RLWTT       &
     &                                                     ,RSWTT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: T
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: CZEN
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER :: I,J,JDAY,JMONTH,K
!
      INTEGER,DIMENSION(3) :: IDAT
!
      REAL :: DAYI,HOUR,TIMES,TTNDKL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CZEN2,XLAT2,XLON2
!
      REAL,DIMENSION(ITS:ITE,JTS:JTE) :: FACTR
!
      REAL :: DEGRAD=3.1415926/180.
      real :: xlat1,xlon1
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      MYIS=MAX(IDS,ITS)
      MYIE=MIN(IDE,ITE)
      MYJS=MAX(JDS,JTS)
      MYJE=MIN(JDE,JTE)
!-----------------------------------------------------------------------
!
!***  GET CURRENT VALUE OF COS(ZENITH ANGLE)
!
!      TIMES=NTSD*DT
      TIMES=XTIME*60.
!
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        XLAT2(I,J)=GLAT(I,J)
        XLON2(I,J)=GLON(I,J)
!!!!!!!!!!!!Remove the following lines after bit-correct answers
!!!!!!!!!!!!are established with the control
!       xlat1=glat(i,j)/degrad
!       xlat2(i,j)=xlat1*degrad
!       xlon1=glon(i,j)/degrad
!       xlon2(i,j)=xlon1*degrad
!!!!!!!!!!!!
!!!!!!!!!!!!
      ENDDO
      ENDDO
!
      CALL CAL_MON_DAY(JULDAY,JULYR,JMONTH,JDAY)

      IDAT(1)=JMONTH
      IDAT(2)=JDAY
      IDAT(3)=JULYR
!
      CALL ZENITH(TIMES,DAYI,HOUR,IDAT,IHRST,XLON2,XLAT2,CZEN2          &
     &           ,MYIS,MYIE,MYJS,MYJE                                   &
     &           ,IDS,IDE,JDS,JDE,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        CZEN(I,J)=CZEN2(I,J)
        IF(CZMEAN(I,J)>0.)THEN 
          FACTR(I,J)=CZEN(I,J)/CZMEAN(I,J)
        ELSE
          FACTR(I,J)=0.
        ENDIF
      ENDDO
      ENDDO
!
      DO K=KTS,KTE
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          TTNDKL=RSWTT(I,J,K)*FACTR(I,J)+RLWTT(I,J,K)
          T(I,J,K)=T(I,J,K)+TTNDKL*DT*HBM2(I,J)
        ENDDO
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
      END SUBROUTINE RDTEMP
!-----------------------------------------------------------------------
