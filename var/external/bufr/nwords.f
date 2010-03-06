      FUNCTION NWORDS(N,LUN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    NWORDS (docblock incomplete)
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1996-10-09
C
C ABSTRACT: THIS FUNCTION ....
C
C PROGRAM HISTORY LOG:
C 1996-10-09  J. WOOLLEN -- ORIGINAL AUTHOR
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED DOCUMENTATION (INCLUDING
C                           HISTORY) (INCOMPLETE)
C
C USAGE:    NWORDS (N, LUN)
C   INPUT ARGUMENT LIST:
C     N        - INTEGER: BYTE COUNT INDEX FOR BUFR MESSAGE
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C   OUTPUT ARGUMENT LIST:
C     NWORDS   - INTEGER: ....
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: INVMRG
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /USRINT/ NVAL(NFILES),INV(MAXJL,NFILES),VAL(MAXJL,NFILES)

      REAL*8 VAL

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      NWORDS = 0

      DO K=1,NINT(VAL(N,LUN))
      NWORDS = NWORDS + NINT(VAL(NWORDS+N+1,LUN))
      ENDDO

      RETURN
      END
