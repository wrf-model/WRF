      FUNCTION NEMOCK(NEMO)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    NEMOCK
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS FUNCTION CHECKS A MNEMONIC TO VERIFY THAT IT HAS A
C   LENGTH OF BETWEEN ONE AND EIGHT CHARACTERS AND THAT IT ONLY
C   CONTAINS CHARACTERS FROM THE ALLOWABLE CHARACTER SET.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- SPLIT NON-ZERO RETURN INTO -1 FOR LENGTH
C                           NOT 1-8 CHARACTERS AND -2 FOR INVALID
C                           CHARACTERS (RETURN ONLY -1 BEFORE FOR ALL
C                           PROBLEMATIC CASES); UNIFIED/PORTABLE FOR
C                           WRF; ADDED HISTORY DOCUMENTATION
C
C USAGE:    NEMOCK (NEMO)
C   INPUT ARGUMENT LIST:
C     NEMO     - CHARACTER*(*): MNEMONIC TO BE CHECKED
C
C   OUTPUT ARGUMENT LIST:
C     NEMOCK   - INTEGER: INDICATOR AS TO WHETHER NEMO IS VALID:
C                       0 = yes
C                      -1 = no, length not between 1 and 8 characters
C                      -2 = no, it does not contain characters from the
C                           allowable character set
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: RDUSDX   SEQSDX   SNTBBE   SNTBDE
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      CHARACTER*(*) NEMO
      CHARACTER*38  CHRSET

      DATA CHRSET /'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.'/
      DATA NCHR   /38/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  GET THE LENGTH OF NEMO
C  ----------------------

      LNEMO = 0

      DO I=LEN(NEMO),1,-1
      IF(NEMO(I:I).NE.' ') THEN
         LNEMO = I
         GOTO 1
      ENDIF
      ENDDO

1     IF(LNEMO.LT.1 .OR. LNEMO.GT.8) THEN
         NEMOCK = -1
         GOTO 100
      ENDIF

C  SCAN NEMO FOR ALLOWABLE CHARACTERS
C  ----------------------------------

      DO 10 I=1,LNEMO
      DO J=1,NCHR
      IF(NEMO(I:I).EQ.CHRSET(J:J)) GOTO 10
      ENDDO
      NEMOCK = -2
      GOTO 100
10    ENDDO

      NEMOCK = 0

C  EXIT
C  ----

100   RETURN
      END
