      FUNCTION NVNWIN(NODE,LUN,INV1,INV2,INVN,NMAX)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    NVNWIN (docblock incomplete)
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS FUNCTION ....
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
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
C                           COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C                           TERMINATES ABNORMALLY OR UNUSUAL THINGS
C                           HAPPEN
C
C USAGE:    NVNWIN (NODE, LUN, INV1, INV2, INVN, NMAX)
C   INPUT ARGUMENT LIST:
C     NODE     - INTEGER: ....
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     INV1     - INTEGER: ....
C     INV2     - INTEGER: ....
C     NMAX     - INTEGER: LENGTH OF INVN
C
C   OUTPUT ARGUMENT LIST:
C     INVN     - INTEGER: NMAX-WORD ARRAY ....
C     NVNWIN   - INTEGER: ....
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT
C    THIS ROUTINE IS CALLED BY: UFBEVN
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
      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR
      DIMENSION     INVN(NMAX)
      REAL*8        VAL

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IF(NODE.EQ.0) THEN
         IF(IPRT.GE.1) THEN
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
         PRINT*, 'BUFRLIB: NVNWIN - NODE=0, IMMEDIATE RETURN'
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*
         ENDIF
         GOTO 100
      ENDIF

c  .... DK: Shouldn't this be before RETURN above?
      NVNWIN = 0

      DO I=1,NMAX
!     INVN(I) = BMISS ! comment out for gfortran
      INVN(I) = -9999999
      ENDDO

C  SEARCH BETWEEN INV1 AND INV2
C  ----------------------------

      DO N=INV1,INV2
      IF(INV(N,LUN).EQ.NODE) THEN
         NVNWIN = NVNWIN+1
         INVN(NVNWIN) = N
      ENDIF
      ENDDO

c  .... DK: Shouldn't this check be moved into do loop above to
c            prevent array overflow in INVN ????
      IF(NVNWIN.GT.NMAX) GOTO 900

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: NVNWIN - THE NUMBER OF EVENTS, '//
     . 'NVNWIN (",I5,") EXCEEDS THE LIMIT, NMAX (",I5,")")') NVNWIN,NMAX
      CALL BORT(BORT_STR)
      END
