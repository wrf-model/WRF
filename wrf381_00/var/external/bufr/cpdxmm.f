	SUBROUTINE CPDXMM( LUNIT )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CPDXMM
C   PRGMMR: J. ATOR          ORG: NP12       DATE: 2009-03-23
C
C ABSTRACT: BEGINNING AT THE CURRENT FILE POINTER LOCATION WITHIN LUNIT,
C   THIS SUBROUTINE READS A COMPLETE DICTIONARY TABLE (I.E. ONE OR MORE
C   ADJACENT BUFR DX (DICTIONARY) MESSAGES) INTO COMMON /MSGMEM/.
C
C PROGRAM HISTORY LOG:
C 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR
C 2012-09-15  J. WOOLLEN -- MODIFIED FOR C/I/O/BUFR INTERFACE;
C                           REPLACED FORTRAN BACKSPACE WITH C BACKBUFR
C
C USAGE:    CALL CPDXMM (LUNIT)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C
C REMARKS:
C
C    THE FOLLOWING VALUES ARE STORED WITHIN COMMON /MSGMEM/ BY THIS
C    SUBROUTINE:
C
C      LDXM = number of array words filled within MDX
C
C      MDX(I=1,LDXM) = DX dictionary messages for use in decoding
C                      data messages stored within MSGS array (in
C                      COMMON /MSGMEM/)
C
C      NDXM = number of DX dictionary messages within MDX
C
C      IPDXM(I=1,NDXM) = pointer to first word of (I)th message
C                        within MDX
C
C      NDXTS = number of DX dictionary tables represented by
C              messages within MDX
C
C      IFDXTS(J=1,NDXTS) = sequential number of first message 
C                          within MDX which is part of (J)th
C                          dictionary table
C
C      ICDXTS(J=1,NDXTS) = count of consecutive messages within MDX
C                          (beginning with IFDXTS(J)) which
C                          constitute (J)th dictionary table
C
C      IPMSGS(J=1,NDXTS) = sequential number of first data message
C                          within MSGS array (in COMMON /MSGMEM/)
C                          to which (J)th dictionary table applies
C
C      LDXTS = current dictionary table that is in scope
C              (i.e. a number between 1 and NDXTS)
C
C    THIS ROUTINE CALLS:        BORT     ERRWRT   IDXMSG   IUPBS3
C                               NMWRD    RDMSGW
C    THIS ROUTINE IS CALLED BY: UFBMEM
C                               Not normally called by application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	INCLUDE	'bufrlib.prm'

	COMMON /QUIET/  IPRT
	COMMON /MSGMEM/ MUNIT,MLAST,MSGP(0:MAXMSG),MSGS(MAXMEM),
     .			MDX(MXDXW),IPDXM(MXDXM),LDXM,NDXM,LDXTS,NDXTS,
     .			IFDXTS(MXDXTS),ICDXTS(MXDXTS),IPMSGS(MXDXTS)

	DIMENSION MBAY(MXMSGLD4)

	CHARACTER*128	ERRSTR

	LOGICAL DONE

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	IF ( NDXTS .GE. MXDXTS ) GOTO 900

	ICT = 0
	DONE = .FALSE.
        call status(lunit,lun,il,im)

C	Read a complete dictionary table from LUNIT, as a set of one or
C	more DX dictionary messages.

	DO WHILE ( .NOT. DONE )
          CALL RDMSGW ( LUNIT, MBAY, IER )
          IF ( IER .EQ. -1 ) THEN

C	    Don't abort for an end-of-file condition, since it may be
C	    possible for a file to end with dictionary messages.
C	    Instead, backspace the file pointer and let the calling
C	    routine diagnose the end-of-file condition and deal with
C	    it as it sees fit.

	    call backbufr(lun) 
	    DONE = .TRUE.
          ELSE IF ( IER .EQ. -2 ) THEN
	    GOTO 901
	  ELSE IF ( IDXMSG(MBAY) .NE. 1 ) THEN

C	    This is a non-DX dictionary message.  Assume we've reached
C	    the end of the dictionary table, and backspace LUNIT so that
C	    the next read (e.g. in the calling routine) will get this
C	    same message.

	    call backbufr(lun) 
	    DONE = .TRUE.
	  ELSE IF ( IUPBS3(MBAY,'NSUB') .EQ. 0 ) THEN

C	    This is a DX dictionary message, but it doesn't contain any
C	    actual dictionary information.  Assume we've reached the end
C	    of the dictionary table.

	    DONE = .TRUE.
	  ELSE

C	    Store this message into COMMON /MSGMEM/.

            ICT = ICT + 1
	    IF ( ( NDXM + ICT ) .GT. MXDXM ) GOTO 902
            IPDXM(NDXM+ICT) = LDXM + 1
            LMEM = NMWRD(MBAY)
	    IF ( ( LDXM + LMEM ) .GT. MXDXW ) GOTO 903
            DO J = 1, LMEM
              MDX(LDXM+J) = MBAY(J)
            ENDDO
            LDXM = LDXM + LMEM
          ENDIF
	ENDDO

C	Update the table information within COMMON /MSGMEM/.

	IF ( ICT .GT. 0 ) THEN
          IFDXTS(NDXTS+1) = NDXM + 1
          ICDXTS(NDXTS+1) = ICT
          IPMSGS(NDXTS+1) = MSGP(0) + 1
          NDXM = NDXM + ICT
          NDXTS = NDXTS + 1
	  IF ( IPRT .GE. 2 ) THEN
	    CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++')
	    WRITE ( UNIT=ERRSTR, FMT='(A,I3,A,I3,A)') 
     .		'BUFRLIB: CPDXMM - STORED NEW DX TABLE #', NDXTS,
     .		' CONSISTING OF ', ICT, ' MESSAGES'
	    CALL ERRWRT(ERRSTR)
	    CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++')
            CALL ERRWRT(' ')
	  ENDIF
	ENDIF

	RETURN
 900	CALL BORT('BUFRLIB: CPDXMM - MXDXTS OVERFLOW')
 901	CALL BORT('BUFRLIB: CPDXMM - UNEXPECTED READ ERROR')
 902	CALL BORT('BUFRLIB: CPDXMM - MXDXM OVERFLOW')
 903	CALL BORT('BUFRLIB: CPDXMM - MXDXW OVERFLOW')
	END
