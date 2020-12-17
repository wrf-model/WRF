	FUNCTION IDXMSG( MESG )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    IDXMSG
C   PRGMMR: ATOR             ORG: NP12       DATE: 2009-03-23
C
C ABSTRACT:  THIS FUNCTION DETERMINES WHETHER THE GIVEN BUFR MESSAGE
C   IS A DX DICTIONARY MESSAGE THAT WAS CREATED BY THE BUFR ARCHIVE
C   LIBRARY SOFTWARE.
C
C PROGRAM HISTORY LOG:
C 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    IDXMSG( MESG )
C   INPUT ARGUMENT LIST:
C     MESG     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING
C                BUFR MESSAGE
C
C   OUTPUT ARGUMENT LIST:
C     IDXMSG  - INTEGER: RETURN VALUE:
C		   0 - MESG IS NOT A DX DICTIONARY MESSAGE
C		   1 - MESG IS A DX DICTIONARY MESSAGE
C
C REMARKS:
C    THIS ROUTINE CALLS:        IUPBS01
C    THIS ROUTINE IS CALLED BY: CPDXMM   DATEBF   DUMPBF   MESGBC
C                               MESGBF   MSGWRT   RDBFDX   READMG
C                               POSAPX   READERME UFBMEM
C                               Normally not called by application
C                               programs but it could be.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	DIMENSION MESG(*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Note that the following test relies upon logic within subroutine
C	DXMINI which zeroes out the Section 1 date of all DX dictionary 
C	messages.

	IF ( (IUPBS01(MESG,'MTYP').EQ.11) .AND.
     .       (IUPBS01(MESG,'MNTH').EQ.0) .AND.
     .       (IUPBS01(MESG,'DAYS').EQ.0) .AND.
     .       (IUPBS01(MESG,'HOUR').EQ.0) ) THEN
	   IDXMSG = 1
	ELSE
	   IDXMSG = 0
	END IF

	RETURN
	END
