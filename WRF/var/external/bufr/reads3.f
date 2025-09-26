	SUBROUTINE READS3 ( LUN )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    READS3
C   PRGMMR: ATOR             ORG: NP12       DATE: 2009-03-23
C
C ABSTRACT: THIS SUBROUTINE READS THE SECTION 3 DESCRIPTORS FROM THE
C   BUFR MESSAGE IN MBAY(1,LUN).  IT THEN USES THE BUFR MASTER TABLES
C   TO GENERATE THE NECESSARY INFORMATION FOR THESE DESCRIPTORS WITHIN
C   THE INTERNAL BUFR TABLE ARRAYS.
C
C PROGRAM HISTORY LOG:
C 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL READS3 (LUN)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C REMARKS:
C    THIS ROUTINE CALLS:        ADN30    BORT     DXINIT   ERRWRT
C                               IGETNTBI IGETTDI  ISTDESC  IUPBS01
C                               MAKESTAB READMT   STNTBIA  STSEQ
C                               UPDS3
C    THIS ROUTINE IS CALLED BY: READERME READMG
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	INCLUDE 'bufrlib.prm'

	COMMON /QUIET/  IPRT
	COMMON /SC3BFR/ ISC3(NFILES),TAMNEM(NFILES),IRDMT
	COMMON /BITBUF/	MAXBYT,IBIT,IBAY(MXMSGLD4),MBYT(NFILES),
     .			MBAY(MXMSGLD4,NFILES)
	COMMON /DSCACH/	NCNEM,CNEM(MXCNEM),NDC(MXCNEM),
     .                  IDCACH(MXCNEM,MAXNC)

	DIMENSION	IDS3(MAXNC)
	CHARACTER*6	CDS3(MAXNC),NUMB,ADN30

	CHARACTER*8	CNEM,TAMNEM
	CHARACTER*55	CSEQ

	CHARACTER*128	ERRSTR

	LOGICAL		INCACH, ALLSTD

C*	Initializing the following value ensures that new master tables
C*	are read during the first call to this subroutine.

	DATA	LMT /-99/

	SAVE	LMT, LMTV, LOGCE, LMTVL, IREPCT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C*	Unpack some Section 1 information from the message.

	IMT  = IUPBS01 ( MBAY(1,LUN), 'BMT' )
	IMTV = IUPBS01 ( MBAY(1,LUN), 'MTV' )
	IOGCE = IUPBS01 ( MBAY(1,LUN), 'OGCE' )
	IMTVL = IUPBS01 ( MBAY(1,LUN), 'MTVL' )

C*	Unpack the list of Section 3 descriptors from the message.

	CALL UPDS3 ( MBAY(1,LUN), MAXNC, CDS3, NCDS3 )
	DO II = 1, NCDS3
	  IDS3(II) = IFXY( CDS3(II) )
	ENDDO

C*	Compare the master table and master table version numbers from
C*	this message to those from the message that was processed during
C*	the previous call to this subroutine.

	IF (  ( IMT .NE. LMT )
     .		.OR.
     .	    ( ( IMT .NE. 0 ) .AND. ( IMTV .NE. LMTV ) )
     .		.OR.
     .	    ( ( IMT .EQ. 0 ) .AND. ( IMTV .NE. LMTV ) .AND.
     .	      ( ( IMTV .GT. 13 ) .OR. ( LMTV .GT. 13 ) ) )  )
     .	  THEN

C*	  Either the master table number has changed
C*	        .OR.
C*	  The master table number hasn't changed, but it isn't 0, and
C*	  the table version number has changed
C*	        .OR.
C*	  The master table number hasn't changed and is 0, but the table
C*	  version number has changed, and at least one of the table
C*	  version numbers (i.e. the current or the previous) is greater
C*	  than 13 (which is the last version that was a superset of all
C*	  earlier versions of master table 0!)

C*	  In any of these cases, we need to read in new tables and reset
C*	  the internal tables and local descriptor cache, since the
C*	  meanings of one or more Section 3 descriptors may have changed.

	  CALL READMT ( IMT, IMTV, IOGCE, IMTVL )
	  LMT  = IMT
	  LMTV = IMTV
	  LOGCE = IOGCE
	  LMTVL = IMTVL
	  CALL DXINIT ( LUN, 0 )
	  IREPCT = 0
	  NCNEM = 0
	ELSE

C*	  Check whether all of the Section 3 descriptors are standard.
C*	  If so, then the originating center and local table version
C*	  numbers are irrelevant as far as Section 3 is concerned.

	  II = 1
	  ALLSTD = .TRUE.
	  DO WHILE ( (ALLSTD) .AND. (II.LE.NCDS3) )
	    IF ( ISTDESC(IDS3(II)) .EQ. 0 ) THEN
	      ALLSTD = .FALSE.
	    ELSE
	      II = II + 1
	    ENDIF
	  ENDDO
	  IF ( .NOT. ALLSTD ) THEN

C*	    There was at least one local (i.e. non-standard) descriptor,
C*	    so check whether the originating center and/or local table
C*	    version number are different than those from the message
C*	    that was processed during the previous call to this
C*	    subroutine.  If so, then read in new tables and reset the
C*	    internal tables and local descriptor cache, since the
C*	    meanings of one or more local descriptors in Section 3 may
C*	    have changed.

	    IF ( ( IOGCE .NE. LOGCE ) .OR. ( IMTVL .NE. LMTVL ) )  THEN
	      CALL READMT ( IMT, IMTV, IOGCE, IMTVL )
	      LMT  = IMT
	      LMTV = IMTV
	      LOGCE = IOGCE
	      LMTVL = IMTVL
	      CALL DXINIT ( LUN, 0 )
	      IREPCT = 0
	      NCNEM = 0
	    ENDIF
	  ENDIF
	ENDIF

C*	Is the list of Section 3 descriptors already in the cache?

C*	The cache is a performance-enhancing device which saves
C*	time when the same descriptor sequences are encountered
C*	over and over within the calling program.  Time is saved
C*	because the below calls to subroutines STSEQ and MAKESTAB
C*	are bypassed whenever a list is already in the cache.

	INCACH = .FALSE.
	IF ( NCNEM .GT. 0 ) THEN
	  II = 1
	  DO WHILE ( (.NOT.INCACH) .AND. (II.LE.NCNEM) )
	    IF ( NCDS3 .EQ. NDC(II) ) THEN
	      JJ = 1
	      INCACH = .TRUE.
	      DO WHILE ( (INCACH) .AND. (JJ.LE.NCDS3) )
		IF ( IDS3(JJ) .EQ. IDCACH(II,JJ) ) THEN
		  JJ = JJ + 1
		ELSE
		  INCACH = .FALSE.
		ENDIF
	      ENDDO
	      IF (INCACH) THEN

C*		The list is already in the cache, so store the
C*		corresponding Table A mnemonic into COMMON /SC3BFR/
C*		and return.

		IF ( IPRT .GE. 2 ) THEN
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
	ERRSTR = 'BUFRLIB: READS3 - RE-USED CACHE LIST FOR ' // CNEM(II)
	CALL ERRWRT(ERRSTR)
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        CALL ERRWRT(' ')
		ENDIF
		TAMNEM(LUN) = CNEM(II)
		RETURN
	      ENDIF
	    ENDIF
	    II = II + 1
	  ENDDO
	ENDIF

C*	Get the next available index within the internal Table A.

	N = IGETNTBI ( LUN, 'A' )

C*	Generate a Table A mnemonic and sequence description.

	WRITE ( TAMNEM(LUN), '(A5,I3.3)') 'MSTTB', N
	CSEQ = 'TABLE A MNEMONIC ' // TAMNEM(LUN)

C*	Store the Table A mnemonic and sequence into the cache.

	NCNEM = NCNEM + 1
	IF ( NCNEM .GT. MXCNEM ) GOTO 900
	CNEM(NCNEM) = TAMNEM(LUN)
	NDC(NCNEM) = NCDS3
	DO JJ = 1, NCDS3
	  IDCACH(NCNEM,JJ) = IDS3(JJ)
	ENDDO
	IF ( IPRT .GE. 2 ) THEN
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
	ERRSTR = 'BUFRLIB: READS3 - STORED CACHE LIST FOR ' //
     .    CNEM(NCNEM)
	CALL ERRWRT(ERRSTR)
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        CALL ERRWRT(' ')
	ENDIF

C*	Get an FXY value to use with this Table A mnemonic.
  
	IDN = IGETTDI ( LUN )
	NUMB = ADN30 ( IDN, 6 )

C*	Store all of the information for this mnemonic within the
C*	internal Table A.

	CALL STNTBIA ( N, LUN, NUMB, TAMNEM(LUN), CSEQ )

C*	Store all of the information for this sequence within the
C*	internal Tables B and D.

	CALL STSEQ ( LUN, IREPCT, IDN, TAMNEM(LUN), CSEQ, IDS3, NCDS3 )

C*	Update the jump/link table.

	CALL MAKESTAB

	RETURN
900	CALL BORT('BUFRLIB: READS3 - MXCNEM OVERFLOW')
	END
