	SUBROUTINE STNTBIA ( N, LUN, NUMB, NEMO, CELSQ )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    STNTBIA
C   PRGMMR: ATOR            ORG: NP12       DATE: 2009-03-23
C
C ABSTRACT: THIS SUBROUTINE STORES A NEW ENTRY WITHIN INTERNAL BUFR
C   TABLE A.
C
C PROGRAM HISTORY LOG:
C 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL STNTBIA ( N, LUN, NUMB, NEMO, CELSQ )
C   INPUT ARGUMENT LIST:
C       N      - INTEGER: STORAGE INDEX INTO INTERNAL TABLE A
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL TABLE A
C    NUMB      - CHARACTER*6: FXY NUMBER FOR NEW TABLE A ENTRY (IN
C                FORMAT FXXYYY)
C    NEMO      - CHARACTER*8: MNEMONIC CORRESPONDING TO NUMB
C   CELSQ      - CHARACTER*55: SEQUENCE DESCRIPTION CORRESPONDING
C                TO NUMB
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     DIGIT
C    THIS ROUTINE IS CALLED BY: RDUSDX   READS3   STBFDX
C                               Not normally called by application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	INCLUDE 'bufrlib.prm'

	COMMON /TABABD/ NTBA(0:NFILES),NTBB(0:NFILES),NTBD(0:NFILES),
     .			MTAB(MAXTBA,NFILES),IDNA(MAXTBA,NFILES,2),
     .			IDNB(MAXTBB,NFILES),IDND(MAXTBD,NFILES),
     .			TABA(MAXTBA,NFILES),TABB(MAXTBB,NFILES),
     .			TABD(MAXTBD,NFILES)

	CHARACTER*600 TABD
	CHARACTER*128 TABA, TABB
	CHARACTER*128 BORT_STR

	CHARACTER*(*) NUMB, NEMO, CELSQ

	LOGICAL DIGIT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Confirm that neither NEMO nor NUMB has already been defined
C	within the internal BUFR Table A (in COMMON /TABABD/) for
C	the given LUN.

	DO N=1,NTBA(LUN)
	  IF(NUMB(4:6).EQ.TABA(N,LUN)(1: 3)) GOTO 900
 	  IF(NEMO(1:8).EQ.TABA(N,LUN)(4:11)) GOTO 901
	ENDDO

C	Store the values within the internal BUFR Table A.

	TABA(N,LUN)( 1: 3) = NUMB(4:6)
	TABA(N,LUN)( 4:11) = NEMO(1:8)
	TABA(N,LUN)(13:67) = CELSQ(1:55)

C	Decode and store the message type and subtype.

	IF ( DIGIT ( NEMO(3:8) ) ) THEN
c  ....   Message type & subtype obtained directly from Table A mnemonic
	    READ ( NEMO,'(2X,2I3)') MTYP, MSBT
	    IDNA(N,LUN,1) = MTYP
	    IDNA(N,LUN,2) = MSBT
	ELSE
c  ....   Message type obtained from Y value of Table A seq. descriptor
	    READ ( NUMB(4:6),'(I3)') IDNA(N,LUN,1)
c  ....   Message subtype hardwired to ZERO
	    IDNA(N,LUN,2) = 0
	ENDIF

C	Update the count of internal Table A entries.

        NTBA(LUN) = N

	RETURN
900	WRITE(BORT_STR,'("BUFRLIB: STNTBIA - TABLE A FXY VALUE (",A,") '
     .     //'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NUMB
	CALL BORT(BORT_STR)
901	WRITE(BORT_STR,'("BUFRLIB: STNTBIA - TABLE A MNEMONIC (",A,") '
     .     //'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NEMO
	CALL BORT(BORT_STR)
	END
