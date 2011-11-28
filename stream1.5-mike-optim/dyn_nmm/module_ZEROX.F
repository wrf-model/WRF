      MODULE MODULE_ZEROX
!----------------------------------------------------------------------
      CONTAINS
!----------------------------------------------------------------------
!**********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    ZEROx       ZERO OUT MULTI-DIMENSIONAL ARRAY
!   PRGRMMR: BLACK           ORG: W/NP2      DATE: 01-03-20
!
! ABSTRACT:
!     SUBROUTINE ZEROx FILLS REAL ARRAYS WITH ZEROES
!
!     CURRENT INTERFACES: ZERO2, ZERO3
!
! PROGRAM HISTORY LOG:
!   01-03-20  BLACK      - ORIGINATOR
!
! USAGE: CALL ZERO WHERE NEEDED
!   INPUT ARGUMENT LIST:
!      ARR2 - THE ARRAY TO BE FILLED
!       IMS - THE STARTING I VALUE FOR LOCAL MEMORY
!       IME - THE ENDING I VALUE FOR LOCAL MEMORY
!       JMS - THE STARTING J VALUE FOR LOCAL MEMORY
!       JME - THE ENDING J VALUE FOR LOCAL MEMORY
!
!   OUTPUT ARGUMENT LIST:
!      ARR2
!
!   OUTPUT FILES:
!     NONE
!
!   SUBPROGRAMS CALLED:
!
!     UNIQUE: NONE
!
!     LIBRARY: NONE
!
!   COMMON BLOCKS: NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$
!**********************************************************************
      SUBROUTINE ZERO2(ARR2,IS,IE,JS,JE)
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IE,IS,JE,JS
      REAL,DIMENSION(IS:IE,JS:JE),INTENT(INOUT) :: ARR2
!
      INTEGER :: I,J
!**********************************************************************
!----------------------------------------------------------------------
      DO J=JS,JE
      DO I=IS,IE
        ARR2(I,J)=0.
      ENDDO
      ENDDO
!----------------------------------------------------------------------
      END SUBROUTINE ZERO2
!**********************************************************************
      SUBROUTINE ZERO3(ARR2,IS,IE,JS,JE,KS,KE)
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IE,IS,JE,JS,KE,KS
      REAL,DIMENSION(IS:IE,KS:KE,JS:JE),INTENT(INOUT) :: ARR2
!
      INTEGER :: I,J,K
!**********************************************************************
!----------------------------------------------------------------------
      DO J=JS,JE
      DO K=KS,KE
      DO I=IS,IE
        ARR2(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!----------------------------------------------------------------------
      END SUBROUTINE ZERO3
!**********************************************************************
!----------------------------------------------------------------------
      END MODULE MODULE_ZEROX
