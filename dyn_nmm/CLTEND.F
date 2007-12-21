!
!NCEP_MESO:MODEL_LAYER: PHYSICS
!
!**********************************************************************
      SUBROUTINE CLTEND (ICLTEND,NPHS,T,T_OLD,T_ADJ                    &
                        ,IDS,IDE,JDS,JDE,KDS,KDE                       &
                        ,IMS,IME,JMS,JME,KMS,KME                       &
                        ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CLTEND      TEMPERATURE CHANGE BY CLOUD PROCESSES
!   PRGRMMR: FERRIER         ORG: W/NP22     DATE: 01-09-26
!     
! ABSTRACT:
!     CLTEND GRADUALLY UPDATES TEMPERATURE TENDENCIES FROM CONVECTION 
!     GRID-SCALE MICROPHYSICS, AND PRECIPITATION ASSIMILATION.
!     
! USAGE: CALL CLTEND FROM SOLVE_RUNSTEAM
!   INPUT ARGUMENT LIST:
!     ICLTEND - FLAG SET TO -1 PRIOR TO PHYSICS CALLS, 0 AFTER PHYSICS
!               CALLS, AND 1 FOR UPDATING TEMPERATURES EVERY TIME STEP
!  
!   OUTPUT ARGUMENT LIST:  NONE
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
!----------------------------------------------------------------------
      USE module_MPP
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: ICLTEND                                    &
                           ,IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE                    &
                           ,NPHS
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: T       &
                                                              ,T_ADJ   &
                                                              ,T_OLD
!
!***  LOCAL VARIABLES 
!
      INTEGER :: I,J,K
!
      REAL :: DELTPH
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      IF(ICLTEND<0)THEN
        DO K=KTS,KTE
        DO J=JTS,JTE
        DO I=ITS,ITE
          T_OLD(I,J,K)=T(I,J,K)
        ENDDO
        ENDDO
        ENDDO
      ELSEIF(ICLTEND==0)THEN
        DO K=KTS,KTE
        DO J=JTS,JTE
        DO I=ITS,ITE
          T_ADJ(I,J,K)=T(I,J,K)-T_OLD(I,J,K)
          T(I,J,K)=T_OLD(I,J,K)
        ENDDO
        ENDDO
        ENDDO
      ELSE
        DELTPH=1./REAL(NPHS)
        DO K=KTS,KTE
        DO J=JTS,JTE
        DO I=ITS,ITE
          T(I,J,K)=T(I,J,K)+DELTPH*T_ADJ(I,J,K)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!----------------------------------------------------------------------
!
      END SUBROUTINE CLTEND
!
!----------------------------------------------------------------------
