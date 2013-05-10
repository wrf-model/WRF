      SUBROUTINE STRNUM(STR,NUM)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    STRNUM
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE DECODES AN INTEGER FROM A CHARACTER STRING.
C   THE INPUT STRING SHOULD CONTAIN ONLY DIGITS AND (OPTIONAL) TRAILING
C   BLANKS AND SHOULD NOT CONTAIN ANY SIGN CHARACTERS (E.G. '+', '-')
C   NOR LEADING BLANKS NOR EMBEDDED BLANKS.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION
C
C USAGE:    CALL STRNUM (STR, NUM)
C   INPUT ARGUMENT LIST:
C     STR      - CHARACTER*(*): STRING CONTAINING ENCODED INTEGER VALUE
C
C   OUTPUT ARGUMENT LIST:
C     NUM      - INTEGER: DECODED VALUE
C                      -1 = decode was unsuccessful
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C REMARKS:
C    THIS ROUTINE CALLS:        STRSUC
C    THIS ROUTINE IS CALLED BY: JSTNUM   PARUTG   SEQSDX
C                               Normally not called by any application
C                               programs but it could be.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      CHARACTER*(*) STR
      CHARACTER*20  STR2

      COMMON /QUIET / IPRT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      NUM = 0
      K = 0

C     Note that, in the following call to subroutine STRSUC, the output
C     string STR2 is not used anywhere else in this routine.  In fact,
C     the only reason that subroutine STRSUC is being called here is to
C     determine NUM, which, owing to the fact that the input string STR
C     cannot contain any leading blanks, is equal to the number of
C     digits to be decoded from the beginning of STR.
c  .... DK: Should we have a contingency for NUM returned as -1? (guess
c           it can't ever happen, right)

      CALL STRSUC(STR,STR2,NUM)

      DO I=1,NUM
      READ(STR(I:I),'(I1)',ERR=99) J
      IF(J.EQ.0 .AND. STR(I:I).NE.'0') GOTO 99
      K = K*10+J
      ENDDO

      NUM = K
      GOTO 100

C     Note that NUM = -1 unambiguously indicates a bad decode since
C     the input string cannot contain sign characters; thus, NUM is
C     always positive if the decode is successful.

99    NUM = -1
      IF(IPRT.GE.0) THEN
      PRINT*
      PRINT*,'+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++++'
      PRINT*,'BUFRLIB: STRNUM - BAD DECODE -  RETURN WITH NUM = -1'
      PRINT*,'+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++++'
      PRINT*
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
