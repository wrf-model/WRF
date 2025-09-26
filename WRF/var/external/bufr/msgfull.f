      LOGICAL FUNCTION MSGFULL(MSIZ,ITOADD,MXSIZ)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    MSGFULL
C   PRGMMR: J. ATOR          ORG: NP12       DATE: 2009-03-23
C
C ABSTRACT: THIS LOGICAL FUNCTION DETERMINES WHETHER THE CURRENT SUBSET
C   (OF LENGTH ITOADD BYTES) WILL FIT WITHIN THE CURRENT BUFR MESSAGE.
C   A FINITE AMOUNT OF "WIGGLE ROOM" IS ALLOWED FOR AS SHOWN BELOW.
C
C PROGRAM HISTORY LOG:
C 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    MSGFULL (MSIZ,ITOADD,MXSIZ)
C   INPUT ARGUMENT LIST:
C     MSIZ     - INTEGER: SIZE OF CURRENT MESSAGE (IN BYTES)
C     ITOADD   - INTEGER: SIZE OF SUBSET TO BE ADDED (IN BYTES)
C     MXSIZ    - INTEGER: MAXIMUM SIZE OF A BUFR MESSAGE
C
C   OUTPUT ARGUMENT LIST:
C     MSGFULL  - LOGICAL: FALSE IF SUBSET WILL FIT; TRUE OTHERWISE
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: CPYUPD   MSGUPD   WRCMPS   WRDXTB
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /MSGSTD/ CSMF
      COMMON /TNKRCP/ ITRYR,ITRMO,ITRDY,ITRHR,ITRMI,CTRT

      CHARACTER*1 CSMF
      CHARACTER*1 CTRT

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C     Allow for at least 11 additional bytes of "wiggle room" in the
C     message, because subroutine MSGWRT may do any or all of the
C     following:
C        3 bytes may be added by a call to subroutine CNVED4
C      + 1 byte (at most) of padding may be added to Section 4
C      + 7 bytes (at most) of padding may be added up to the next
C          word boundary after Section 5
C     ----
C       11

      IWGBYT = 11

C     But subroutine MSGWRT may also do any of all of the following:

C        6 bytes may be added by a call to subroutine ATRCPT

         IF(CTRT.EQ.'Y') IWGBYT = IWGBYT + 6

C        (MAXNC*2) bytes (at most) may be added by a call to
C        subroutine STNDRD

         IF(CSMF.EQ.'Y') IWGBYT = IWGBYT + (MAXNC*2)

C     Determine whether the subset will fit.

      IF ( ( MSIZ + ITOADD + IWGBYT ) .GT. MXSIZ ) THEN
	MSGFULL = .TRUE.
      ELSE
	MSGFULL = .FALSE.
      ENDIF

      RETURN
      END
