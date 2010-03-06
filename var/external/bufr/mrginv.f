      SUBROUTINE MRGINV

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    MRGINV
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1996-10-09
C
C ABSTRACT: THIS SUBROUTINE PRINTS A SUMMARY OF MERGE ACTIVITY.
C
C PROGRAM HISTORY LOG:
C 1996-10-09  J. WOOLLEN -- ORIGINAL AUTHOR (ENTRY POINT IN INVMRG)
C 2002-05-14  J. WOOLLEN -- CHANGED FROM AN ENTRY POINT TO INCREASE
C                           PORTABILITY TO OTHER PLATFORMS
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C                           DOCUMENTATION (INCLUDING HISTORY)
C
C USAGE:    CALL MRGINV
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: None
C                               Normally called only by application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      COMMON /MRGCOM/ NRPL,NMRG,NAMB,NTOT
      COMMON /QUIET / IPRT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IF(IPRT.GE.0) THEN
      PRINT*,'+++++++++++++++++++++++BUFRLIB+++++++++++++++++++++++++'
      PRINT*,'-------------------------------------------------------'
      PRINT*,'INVENTORY FROM MERGE PROCESS IN BUFRLIB ROUTINE INVMRG '
      PRINT*,'-------------------------------------------------------'
      PRINT*,'NUMBER OF DRB EXPANSIONS  = ',NRPL
      PRINT*,'NUMBER OF MERGES          = ',NMRG
      PRINT*,'NUMBER THAT ARE AMBIGUOUS = ',NAMB
      PRINT*,'-------------------------------------------------------'
      PRINT*,'TOTAL NUMBER OF VISITS    = ',NTOT
      PRINT*,'-------------------------------------------------------'
      PRINT*,'+++++++++++++++++++++++BUFRLIB+++++++++++++++++++++++++'
      ENDIF

      RETURN
      END
