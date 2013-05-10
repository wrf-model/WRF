      FUNCTION NUMBCK(NUMB)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    NUMBCK
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS FUNCTION CHECKS THE INPUT CHARACTER STRING TO DETERMINE
C   WHETHER IT CONTAINS A VALID FXY (DESCRIPTOR) VALUE.
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
C 2007-01-19  J. ATOR    -- CLEANED UP AND SIMPLIFIED LOGIC
C
C USAGE:   NUMBCK (NUMB)
C   INPUT ARGUMENT LIST:
C     NUMB     - CHARACTER*6: FXY VALUE TO BE CHECKED
C
C   OUTPUT ARGUMENT LIST:
C     NUMBCK   - INTEGER: INDICATOR AS TO WHETHER NUMB IS VALID:
C                       0 = YES
C                      -1 = NO - first character ("F" value) is not '0',
C                           '1', '2' OR '3'
C                      -2 = NO - remaining characters (2-6) ("X" and "Y"
C                           values) are not all numeric
C                      -3 = NO - characters 2-3 ("X" value) are not
C                           between '00' and '63'
C                      -4 = NO - characters 4-6 ("Y" value) are not
C                           between '000' and '255'
C
C REMARKS:
C    THIS ROUTINE CALLS:        DIGIT
C    THIS ROUTINE IS CALLED BY: IGETFXY  RDUSDX
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      CHARACTER*6  NUMB
      LOGICAL      DIGIT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK THE FIRST CHARACTER OF NUMB
C  ---------------------------------

      IF( LLT(NUMB(1:1),'0') .OR. LGT(NUMB(1:1),'3') ) THEN
         NUMBCK = -1
         RETURN
      ENDIF

C  CHECK FOR A VALID DESCRIPTOR
C  ----------------------------

      IF(DIGIT(NUMB(2:6))) THEN
         READ(NUMB,'(1X,I2,I3)') IX,IY
      ELSE
         NUMBCK = -2
         RETURN
      ENDIF

      IF(IX.LT.0 .OR. IX.GT. 63) THEN
         NUMBCK = -3
         RETURN
      ELSE IF(IY.LT.0 .OR. IY.GT.255) THEN
         NUMBCK = -4
         RETURN
      ENDIF

      NUMBCK = 0

      RETURN
      END
