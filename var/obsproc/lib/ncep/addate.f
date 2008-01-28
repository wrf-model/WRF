      SUBROUTINE ADDATE(IDATE,JH,JDATE)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    ADDATE
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE UPDATES AN EIGHT (YYMMDDHH) OR TEN
C   (YYYYMMDDHH) DIGIT INTEGER DATE BY A SPECIFIED NUMBER OF HOURS.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1996-12-11  J. WOOLLEN -- NEW DATE ARITHMETIC ROUTINE ADDED
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C                           DOCUMENTATION (INCLUDING HISTORY)
C 2004-08-18  J. ATOR    -- FIX BUG FOR YEARS THAT ARE MULTIPLE
C                           OF 100 BUT NOT OF 400
C
C USAGE:    CALL ADDATE (IDATE, JH, JDATE)
C   INPUT ARGUMENT LIST:
C     IDATE    - INTEGER: EIGHT- OR TEN-DIGIT DATE
C     JH       - INTEGER: NUMBER OF HOURS (+ OR -) BY WHICH IDATE
C                SHOULD BE UPDATED
C
C   OUTPUT ARGUMENT LIST:
C     JDATE    - INTEGER: EIGHT- OR TEN-DIGIT UPDATED DATE
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: None
C                               Normally called only by application
C                               programs (W3LIB routine W3MOVDAT is
C                               much better).
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      DIMENSION   MON(12)

      DATA MON/31,28,31,30,31,30,31,31,30,31,30,31/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IY = IDATE/1000000
      IM = MOD(IDATE/10000  ,100)
      ID = MOD(IDATE/100    ,100)
      IH = MOD(IDATE        ,100)
      IH = IH+JH

      IF(MOD(IY,4).EQ.0) MON(2) = 29
      IF(MOD(IY,100).EQ.0) MON(2) = 28
      IF(MOD(IY,400).EQ.0) MON(2) = 29

1     IF(IH.LT.0) THEN
         IH = IH+24
         ID = ID-1
         IF(ID.EQ.0) THEN
            IM = IM-1
            IF(IM.EQ.0) THEN
               IM = 12
               IY = IY-1
               IF(IY.LT.0) IY = 99
            ENDIF
            ID = MON(IM)
         ENDIF
         GOTO 1
      ELSEIF(IH.GE.24) THEN
         IH = IH-24
         ID = ID+1
         IF(ID.GT.MON(IM)) THEN
            ID = 1
            IM = IM+1
            IF(IM.GT.12) THEN
               IM = 1
               IY = IY+1
               IF(IY.EQ.100) IY = 00
            ENDIF
         ENDIF
         GOTO 1
      ENDIF

      JDATE = IY*1000000 + IM*10000 + ID*100 + IH

      RETURN
      END
