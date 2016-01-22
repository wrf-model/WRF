	SUBROUTINE GETNTBE ( LUNT, IFXYN, LINE, IRET )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    GETNTBE
C   PRGMMR: ATOR            ORG: NP12       DATE: 2007-01-19
C
C ABSTRACT:  THIS SUBROUTINE GETS THE FIRST LINE OF THE NEXT ENTRY IN
C   THE SPECIFIED ASCII MASTER TABLE B OR MASTER TABLE D FILE.  THIS
C   LINE CONTAINS, AMONG OTHER THINGS, THE FXY NUMBER CORRESPONDING TO
C   THIS ENTRY.
C
C PROGRAM HISTORY LOG:
C 2007-01-19  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL GETNTBE ( LUNT, IFXYN, LINE, IRET )
C   INPUT ARGUMENT LIST:
C     LUNT     - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE
C                CONTAINING MASTER TABLE B OR MASTER TABLE D INFORMATION
C
C   OUTPUT ARGUMENT LIST:
C     IFXYN    - INTEGER: BIT-WISE REPRESENTATION OF FXY NUMBER FOR
C                NEXT TABLE ENTRY
C     LINE     - CHARACTER*(*): FIRST LINE OF NEXT TABLE ENTRY
C     IRET     - INTEGER: RETURN CODE:
C                       0 = normal return
C                      -1 = end-of-file encountered while reading
C                           from LUNT
C                      -2 = I/O error encountered while reading
C                           from LUNT
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT2    IGETNTBL IGETFXY  IFXY
C                               PARSTR
C    THIS ROUTINE IS CALLED BY: RDMTBB   RDMTBD
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	CHARACTER*(*)	LINE
	CHARACTER*128	BORT_STR1, BORT_STR2
	CHARACTER*20	TAGS(4)
	CHARACTER*6	ADSC

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Get the first line of the next entry in the file.

	IRET = IGETNTBL ( LUNT, LINE )
	IF ( IRET .EQ. 0 ) THEN

C	    The first field within this line should contain the
C	    FXY number.

	    CALL PARSTR ( LINE(1:20), TAGS, 4, NTAG, '|', .FALSE. )
	    IF ( NTAG .LT. 1 ) GOTO 900
	    IF ( IGETFXY ( TAGS(1), ADSC ) .NE. 0 ) GOTO 900

C	    Store the bit-wise representation of the FXY number.

	    IFXYN = IFXY ( ADSC )
	ENDIF

	RETURN

 900    BORT_STR1 = 'BUFRLIB: GETNTBE - CARD BEGINNING WITH: ' //
     .     LINE(1:20)
        BORT_STR2 = '                  HAS BAD OR MISSING FXY NUMBER'
	CALL BORT2(BORT_STR1,BORT_STR2)

	END
