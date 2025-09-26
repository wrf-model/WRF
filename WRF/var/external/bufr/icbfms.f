	INTEGER FUNCTION ICBFMS ( STR, LSTR )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    ICBFMS
C   PRGMMR: J. ATOR          ORG: NP12       DATE: 2012-06-07
C
C ABSTRACT: THIS FUNCTION TESTS WHETHER THE INPUT CHARACTER STRING
C   IS "MISSING" BY CHECKING IF ALL OF THE EQUIVALENT BITS ARE SET TO 1.
C   IT IS SIMILAR TO BUFR ARCHIVE LIBRARY FUNCTION IBFMS, EXCEPT THAT
C   IBFMS TESTS REAL*8 VALUES FOR EQUIVALENCE TO THE PARAMETER BMISS,
C   WHEREAS ICBFMS CHECKS THAT ALL EQUIVALENT BITS ARE SET TO 1 AND IS
C   THEREFORE A MORE PORTABLE AND RELIABLE TEST FOR USE WITH CHARACTER
C   STRINGS.
C
C PROGRAM HISTORY LOG:
C 2012-06-07  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    ICBFMS ( STR, LSTR )
C   INPUT ARGUMENT LIST:
C     STR      - CHARACTER*(*): STRING TO BE TESTED
C     LSTR     - INTEGER: NUMBER OF CHARACTERS TO BE TESTED WITHIN STR
C
C   OUTPUT ARGUMENT LIST:
C     ICBFMS   - INTEGER: RETURN CODE:
C                0 - STR IS NOT "MISSING"
C                1 - STR IS "MISSING"
C
C REMARKS:
C    THIS ROUTINE CALLS:        IUPM
C    THIS ROUTINE IS CALLED BY: RDCMPS   RDTREE   UFDUMP
C                               Also called by application programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

        INCLUDE 'bufrlib.prm'

	CHARACTER*(*)	STR

C-----------------------------------------------------------------------

	ICBFMS = 0

	NUMCHR = MIN(LSTR,LEN(STR))

C*	Beginning with version 10.2.0 of the BUFRLIB, "missing" strings
C*	are explicitly encoded with all bits set to 1.  However, this
C*	wasn't the case for strings encoded with earlier versions of
C*	BUFRLIB, so the following block can help identify "missing"
C*	strings encoded with these earlier versions.

	IF ( (NUMCHR.GE.4) .AND. ( STR(1:4).EQ.'B7Hv')) THEN
	    ICBFMS = 1
	    RETURN
	END IF

C*	Otherwise, the logic below will handle cases encoded using
C*	BUFRLIB version 10.2.0 or later.

	DO I=1,NUMCHR
	   IF ( IUPM(STR(I:I),8).NE.255 ) RETURN
	ENDDO

	ICBFMS = 1

	RETURN
	END
