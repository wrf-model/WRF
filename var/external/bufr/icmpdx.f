      INTEGER FUNCTION ICMPDX(LUD,LUN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    ICMPDX
C   PRGMMR: J. ATOR          ORG: NP20       DATE: 2009-06-18
C
C ABSTRACT: THIS FUNCTION DETERMINES WHETHER LOGICAL UNIT IOLUN(LUN)
C   HAS THE SAME INTERNAL TABLE INFORMATION AS LOGICAL UNIT IOLUN(LUD).
C   NOTE THAT THIS DOES NOT NECESSARILY MEAN THAT IOLUN(LUN) AND
C   IOLUN(LUD) ARE SHARING TABLE INFORMATION, SINCE TWO LOGICAL UNITS
C   CAN HAVE THE SAME INTERNAL TABLE INFORMATION WITHOUT SHARING IT.
C
C PROGRAM HISTORY LOG:
C 2009-06-18  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    ICMPDX (LUD, LUN)
C   INPUT ARGUMENT LIST:
C     LUD      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C                FOR FIRST LOGICAL UNIT
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C                FOR SECOND LOGICAL UNIT
C
C   OUTPUT ARGUMENT LIST:
C     ICMPDX   - INTEGER: RETURN CODE INDICATING WHETHER IOLUN(LUN)
C                HAS THE SAME INTERNAL TABLE INFORMATION AS IOLUN(LUD):
C                  0 - NO
C                  1 - YES
C
C REMARKS:
C    THIS ROUTINE CALLS:        ISHRDX
C    THIS ROUTINE IS CALLED BY: IOK2CPY  MAKESTAB
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /TABABD/ NTBA(0:NFILES),NTBB(0:NFILES),NTBD(0:NFILES),
     .                MTAB(MAXTBA,NFILES),IDNA(MAXTBA,NFILES,2),
     .                IDNB(MAXTBB,NFILES),IDND(MAXTBD,NFILES),
     .                TABA(MAXTBA,NFILES),TABB(MAXTBB,NFILES),
     .                TABD(MAXTBD,NFILES)

      CHARACTER*600 TABD
      CHARACTER*128 TABB
      CHARACTER*128 TABA

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     First, check whether the two units are actually sharing tables.
C     If so, then they obviously have the same table information.

      ICMPDX = ISHRDX(LUD,LUN)
      IF ( ICMPDX .EQ. 1 ) RETURN

C     Otherwise, check whether the internal Table A, B and D entries are
C     all identical between the two units.

      IF ( ( NTBA(LUD) .EQ. 0 ) .OR.
     .		( NTBA(LUN) .NE. NTBA(LUD) ) ) RETURN
      DO I = 1, NTBA(LUD)
	IF ( IDNA(I,LUN,1) .NE. IDNA(I,LUD,1) ) RETURN
	IF ( IDNA(I,LUN,2) .NE. IDNA(I,LUD,2) ) RETURN
	IF ( TABA(I,LUN) .NE. TABA(I,LUD) ) RETURN
      ENDDO

	IF ( ( NTBB(LUD) .EQ. 0 ) .OR.
     .		( NTBB(LUN) .NE. NTBB(LUD) ) ) RETURN
      DO I = 1, NTBB(LUD)
	IF ( IDNB(I,LUN) .NE. IDNB(I,LUD) ) RETURN
	IF ( TABB(I,LUN) .NE. TABB(I,LUD) ) RETURN
      ENDDO

      IF ( ( NTBD(LUD) .EQ. 0 ) .OR.
     .		( NTBD(LUN) .NE. NTBD(LUD) ) ) RETURN
      DO I = 1, NTBD(LUD)
	IF ( IDND(I,LUN) .NE. IDND(I,LUD) ) RETURN
	IF ( TABD(I,LUN) .NE. TABD(I,LUD) ) RETURN
      ENDDO

      ICMPDX = 1

      RETURN
      END
