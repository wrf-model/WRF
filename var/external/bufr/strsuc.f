      SUBROUTINE STRSUC(STR1,STR2,LENS)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    STRSUC
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE REMOVES LEADING AND TRAILING BLANKS FROM A
C   STRING.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; ADDED MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN UNUSUAL THINGS HAPPEN
C
C USAGE:    CALL STRSUC (STR1, STR2, LENS)
C   INPUT ARGUMENT LIST:
C     STR1     - CHARACTER*(*): STRING
C
C   OUTPUT ARGUMENT LIST:
C     STR2     - CHARACTER*(*): COPY OF STR1 WITH LEADING AND TRAILING
C                BLANKS REMOVED
C     LENS     - INTEGER: LENGTH OF STR2:
C                      -1 = STR1 contained embedded blanks
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: DXDUMP   STRNUM
C                               Normally not called by any application
C                               programs but it could be.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      CHARACTER*(*) STR1,STR2

      COMMON /QUIET / IPRT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      LENS = 0
      LSTR = LEN(STR1)

C  FIND THE FIRST NON-BLANK IN THE INPUT STRING
C  --------------------------------------------

      DO I=1,LSTR
      IF(STR1(I:I).NE.' ') GOTO 2
      ENDDO
      GOTO 100

C     Now, starting with the first non-blank in the input string,
C     copy characters from the input string into the output string
C     until reaching the next blank in the input string.

2     DO J=I,LSTR
      IF(STR1(J:J).EQ.' ') GOTO 3
      LENS = LENS+1
      STR2(LENS:LENS) = STR1(J:J)
      ENDDO
      GOTO 100

C     Now, continuing on within the input string, make sure that
C     there are no more non-blank characters.  If there are, then
C     the blank at which we stopped copying from the input string
C     into the output string was an embedded blank.

3     DO I=J,LSTR
      IF(STR1(I:I).NE.' ') LENS = -1
      ENDDO

      IF(LENS.EQ.-1 .AND. IPRT.GE.0)  THEN
      PRINT*
      PRINT*,'+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++++'
         PRINT*,'BUFRLIB: STRSUC - INPUT STRING ',STR1,' CONTAINS ',
     .    '1 OR MORE EMBEDDED BLANKS -  RETURN WITH LENS = -1'
      PRINT*,'+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++++'
      PRINT*
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
