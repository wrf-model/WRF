      SUBROUTINE DXMINI(LUN,MBAY,MBYT,MB4,MBA,MBB,MBD)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    DXMINI
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE INITIALIZES A BUFR TABLE (DICTIONARY)
C   MESSAGE, WRITING ALL THE PRELIMINARY INFORMATION INTO SECTIONS 0,
C   1, 3, 4.  BUFR ARCHIVE LIBRARY SUBROUTINE WRDXTB WILL WRITE THE
C   ACTUAL TABLE INFORMATION INTO THE MESSAGE.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1997-07-29  J. WOOLLEN -- MODIFIED TO UPDATE THE CURRENT BUFR VERSION
C                           WRITTEN IN SECTION 0 FROM 2 TO 3
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           10,000 TO 20,000 BYTES
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
C                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C                           TERMINATES ABNORMALLY
C 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           20,000 TO 50,000 BYTES
C 2005-11-29  J. ATOR    -- CHANGED DEFAULT MASTER TABLE VERSION TO 12
C 2009-05-07  J. ATOR    -- CHANGED DEFAULT MASTER TABLE VERSION TO 13
C
C USAGE:    CALL DXMINI (LUN, MBAY, MBYT, MB4, MBA, MBB, MBD)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C   OUTPUT ARGUMENT LIST:
C     MBAY     - INTEGER: (MXMSGLD4)-WORD PACKED BINARY ARRAY
C                CONTAINING BUFR MESSAGE
C     MBYT     - INTEGER: LENGTH OF BUFR MESSAGE (BYTES)
C     MB4      - INTEGER: BYTE NUMBER IN MESSAGE OF FIRST BYTE IN
C                SECTION 4
C     MBA      - INTEGER: BYTE NUMBER IN MESSAGE OF FOURTH BYTE IN
C                SECTION 4
C     MBB      - INTEGER: BYTE NUMBER IN MESSAGE OF FIFTH BYTE IN
C                SECTION 4
C     MBD      - INTEGER: BYTE NUMBER IN MESSAGE OF SIXTH BYTE IN
C                SECTION 4
C
C REMARKS:
C    ARGUMENT LUN IS NOT REFERENCED IN THIS SUBROUTINE.  IT IS LEFT
C    HERE IN CASE AN APPLICATION PROGRAM CALLS THIS SUBROUTINE.
C
C    THIS ROUTINE CALLS:        BORT     IUPM     PKB      PKC
C    THIS ROUTINE IS CALLED BY: WRDXTB
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /DXTAB / MAXDX,IDXV,NXSTR(10),LDXA(10),LDXB(10),LDXD(10),
     .                LD30(10),DXSTR(10)

      CHARACTER*128 BORT_STR
      CHARACTER*56  DXSTR
      DIMENSION     MBAY(MXMSGLD4)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

c  .... The local message subtype is set to the version number of the
c       local tables (here = 1)
      MSBT = IDXV

C  INITIALIZE THE MESSAGE
C  ----------------------

      MBIT = 0
      DO I=1,MXMSGLD4
      MBAY(I) = 0
      ENDDO

C  For dictionary messages, the Section 1 date is simply zeroed out.
C  (Note that there is logic in function IDXMSG which relies on this!)

      IH   = 0
      ID   = 0
      IM   = 0
      IY   = 0

c  Dictionary messages get type 11 (see WMO Table A)
      MTYP = 11
      NSUB = 1

      IDXS = IDXV+1
      LDXS = NXSTR(IDXS)

      NBY0 = 8
      NBY1 = 18
      NBY2 = 0
      NBY3 = 7 + NXSTR(IDXS) + 1
      NBY4 = 7
      NBY5 = 4
      MBYT = NBY0+NBY1+NBY2+NBY3+NBY4+NBY5

      IF(MOD(NBY3,2).NE.0) GOTO 900

C  SECTION 0
C  ---------

      CALL PKC('BUFR' ,  4 , MBAY,MBIT)
      CALL PKB(  MBYT , 24 , MBAY,MBIT)
      CALL PKB(     3 ,  8 , MBAY,MBIT)

C  SECTION 1
C  ---------

      CALL PKB(  NBY1 , 24 , MBAY,MBIT)
      CALL PKB(     0 ,  8 , MBAY,MBIT)
      CALL PKB(     3 ,  8 , MBAY,MBIT)
      CALL PKB(     7 ,  8 , MBAY,MBIT)
      CALL PKB(     0 ,  8 , MBAY,MBIT)
      CALL PKB(     0 ,  8 , MBAY,MBIT)
      CALL PKB(  MTYP ,  8 , MBAY,MBIT)
      CALL PKB(  MSBT ,  8 , MBAY,MBIT)
      CALL PKB(    13 ,  8 , MBAY,MBIT)
      CALL PKB(  IDXV ,  8 , MBAY,MBIT)
      CALL PKB(    IY ,  8 , MBAY,MBIT)
      CALL PKB(    IM ,  8 , MBAY,MBIT)
      CALL PKB(    ID ,  8 , MBAY,MBIT)
      CALL PKB(    IH ,  8 , MBAY,MBIT)
      CALL PKB(     0 ,  8 , MBAY,MBIT)
      CALL PKB(     0 ,  8 , MBAY,MBIT)

C  SECTION 3
C  ---------

      CALL PKB(       NBY3 ,   24 , MBAY,MBIT)
      CALL PKB(          0 ,    8 , MBAY,MBIT)
      CALL PKB(          1 ,   16 , MBAY,MBIT)
      CALL PKB(       2**7 ,    8 , MBAY,MBIT)
      DO I=1,LDXS
      CALL PKB(IUPM(DXSTR(IDXS)(I:I),8),8,MBAY,MBIT)
      ENDDO
      CALL PKB(          0 ,    8 , MBAY,MBIT)

C  SECTION 4
C  ---------

      MB4 = MBIT/8+1
      CALL PKB(NBY4 , 24 , MBAY,MBIT)
      CALL PKB(   0 ,  8 , MBAY,MBIT)
      MBA = MBIT/8+1
      CALL PKB(   0 ,  8 , MBAY,MBIT)
      MBB = MBIT/8+1
      CALL PKB(   0 ,  8 , MBAY,MBIT)
      MBD = MBIT/8+1
      CALL PKB(   0 ,  8 , MBAY,MBIT)

      IF(MBIT/8+NBY5.NE.MBYT) GOTO 901

C  EXITS
C  -----

      RETURN
900   CALL BORT
     . ('BUFRLIB: DXMINI - LENGTH OF SECTION 3 IS NOT A MULTIPLE OF 2')
901   WRITE(BORT_STR,'("BUFRLIB: DXMINI - NUMBER OF BYTES STORED FOR '//
     . 'A MESSAGE (",I6,") IS NOT THE SAME AS FIRST CALCULATED, MBYT '//
     . '(",I6)') MBIT/8+NBY5,MBYT
      CALL BORT(BORT_STR)
      END
