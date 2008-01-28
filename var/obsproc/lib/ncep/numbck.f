      FUNCTION NUMBCK(NUMB)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    NUMBCK
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS FUNCTION CHECKS THE CHARACTER STRING REPRESENTING AN
C   FXY (DESCRIPTOR) VALUE THAT WAS READ VIA BUFR ARCHIVE LIBRARY
C   SUBROUTINE RDUSDX (I.E., READ FROM A USER-SUPPLIED BUFR DICTIONARY
C   TABLE IN CHARACTER FORMAT) TO VERIFY THAT THE FIRST CHARACTER (THE
C   "F" VALUE) IS 'A', '0' OR '3'; THAT THE REMAINING CHARACTERS (2-6)
C   (THE "X" AND "Y" VALUES) ARE ALL NUMERIC; THAT CHARACTERS 2-3 (THE
C   "X" VALUE) ARE BETWEEN '00' AND '63'; AND THAT CHARACTERS 4-6 (THE
C   "Y" VALUE) ARE BETWEEN '000' AND '255'.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- SPLIT NON-ZERO RETURN INTO -1 FOR INVALID
C                           CHARACTER IN POSITION 1, -2 FOR INVALID
C                           CHARACTERS IN POSITIONS 2 THROUGH 6, -3 FOR
C                           INVALID CHARACTERS IN POSITIONS 2 AND 3 DUE
C                           TO BEING OUT OF RANGE, AND -4 FOR INVALID
C                           CHARACTERS IN POSITIONS 4 THROUGH 6 DUE TO
C                           BEING OUT OF RANGE (RETURN ONLY -1 BEFORE
C                           FOR ALL PROBLEMATIC CASES); UNIFIED/
C                           PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION
C
C USAGE:   NUMBCK (NUMB)
C   INPUT ARGUMENT LIST:
C     NUMB     - CHARACTER*6: FXY VALUE TO BE CHECKED
C
C   OUTPUT ARGUMENT LIST:
C     NUMBCK   - INTEGER: INDICATOR AS TO WHETHER NUMB IS VALID:
C                       0 = yes
C                      -1 = no, first character ("F"value) is not 'A',
C                           '0' OR '3'
C                      -2 = no, remaining characters (2-6) ("X" and "Y"
C                           values) are not all numeric
C                      -3 = no, characters 2-3 ("X" value) are not
C                           between '00' and '63'
C                      -4 = no, characters 4-6 ("Y" value) are not
C                           between '000' and '255'
C
C REMARKS:
C    THIS ROUTINE CALLS:        DIGIT
C    THIS ROUTINE IS CALLED BY: RDUSDX
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      CHARACTER*10 CHRSET
      CHARACTER*6  NUMB
      CHARACTER*1  FC
      LOGICAL      DIGIT

      DATA CHRSET /'0123456789'/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      NUMBCK = 0
      LNUMB  = 0
      FC     = NUMB(1:1)

C  CHECK THE FIRST CHARACTER OF NUMB
C  ---------------------------------

      IF(.NOT.(FC.EQ.'A' .OR. FC.EQ.'0' .OR. FC.EQ.'3')) THEN
         NUMBCK = -1
         GOTO 100
      ENDIF

C  CHECK THE REST OF NUMB
C  ----------------------

      DO 10 I=2,6
      DO J=1,10
      IF(NUMB(I:I).EQ.CHRSET(J:J)) GOTO 10
      ENDDO
      NUMBCK = -2
      GOTO 100
10    ENDDO

C  CHECK FOR A VALID DESCRIPTOR
C  ----------------------------

      IF(DIGIT(NUMB(2:6))) THEN
         READ(NUMB,'(1X,I2,I3)') IX,IY
      ELSE
         NUMBCK = -2
         GOTO 100
      ENDIF

      IF(IX.LT.0 .OR. IX.GT. 63) THEN
         NUMBCK = -3
         GOTO 100
      ELSE IF(IY.LT.0 .OR. IY.GT.255) THEN
         NUMBCK = -4
         GOTO 100
      ENDIF

      NUMBCK = 0

C  EXIT
C  ----

100   RETURN
      END
