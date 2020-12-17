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
C 2009-04-21  J. ATOR    -- USE ERRWRT
C
C USAGE:    CALL MRGINV
C
C REMARKS:
C    THIS ROUTINE CALLS:        ERRWRT
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

      CHARACTER*128 ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++BUFRLIB+++++++++++++++++++++++')
      CALL ERRWRT('---------------------------------------------------')
      CALL ERRWRT('INVENTORY FROM MERGE PROCESS IN SUBROUTINE INVMRG:')
      CALL ERRWRT('---------------------------------------------------')
      WRITE ( UNIT=ERRSTR, FMT='(A,I8)' )
     .  'NUMBER OF DRB EXPANSIONS  = ', NRPL
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8)' )
     .  'NUMBER OF MERGES          = ', NMRG
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8)' )
     .  'NUMBER THAT ARE AMBIGUOUS = ', NAMB
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('---------------------------------------------------')
      WRITE ( UNIT=ERRSTR, FMT='(A,I9)' )
     .  'TOTAL NUMBER OF VISITS    = ', NTOT
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('---------------------------------------------------')
      CALL ERRWRT('+++++++++++++++++++++BUFRLIB+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

      RETURN
      END
