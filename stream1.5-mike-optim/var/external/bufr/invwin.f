      FUNCTION INVWIN(NODE,LUN,INV1,INV2)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    INVWIN (docblock incomplete)
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS FUNCTION ....
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
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
C                           HISTORY) (INCOMPLETE); OUTPUTS MORE
C                           COMPLETE DIAGNOSTIC INFO WHEN UNUSUAL
C                           THINGS HAPPEN
C
C USAGE:    INVWIN (NODE, LUN, INV1, INV2)
C   INPUT ARGUMENT LIST:
C     NODE     - INTEGER: ....
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     INV1     - INTEGER: ....
C     INV2     - INTEGER: ....
C
C   OUTPUT ARGUMENT LIST:
C     INVWIN   - INTEGER: ....
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: CONWIN   DRSTPL   GETWIN   NEVN
C                               TRYBUMP  UFBGET   UFBRW    UFBSEQ
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
      COMMON /QUIET/  IPRT

      REAL*8 VAL

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      INVWIN = 0
      IF(NODE.EQ.0) GOTO 200

C  SEARCH BETWEEN INV1 AND INV2
C  ----------------------------

10    DO INVWIN=INV1,INV2
      IF(INV(INVWIN,LUN).EQ.NODE) GOTO 100
      ENDDO

      INVWIN = 0

 200  IF(IPRT.GE.2) THEN
      PRINT*
      PRINT*,'+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++++'
         PRINT*, 'BUFRLIB: INVWIN - INVWIN RETURNING WITH VALUE OF 0'
      PRINT*,'+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++++'
      PRINT*
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
