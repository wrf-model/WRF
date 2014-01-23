      SUBROUTINE UFBMEM(LUNIT,INEW,IRET,IUNIT)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    UFBMEM
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE OPENS A BUFR FILE FOR INPUT, READS EACH
C   MESSAGE AND TRANSFERS THEM ONE-BY-ONE TO INTERNAL MEMORY (ARRAY
C   MSGS IN COMMON BLOCK /MSGMEM/).  IF MESSAGES ARE APPENDED TO
C   EXISTING MESSAGES IN INTERNAL MEMORY, THE BUFR FILE READ HERE IS
C   CLOSED PRIOR TO RETURNING TO THE CALLING PROGRAM.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 1999-11-18  J. WOOLLEN -- THE MAXIMUM NUMBER OF BYTES REQUIRED TO
C                           STORE ALL MESSAGES INTERNALLY WAS INCREASED
C                           FROM 4 MBYTES TO 8 MBYTES
C 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           10,000 TO 20,000 BYTES
C 2001-08-15  D. KEYSER  -- PARAMETER MAXMEM (THE MAXIMUM NUMBER OF
C                           BYTES REQUIRED TO STORE ALL MESSAGES
C                           INTERNALLY) WAS INCREASED FROM 8 MBYTES TO
C                           16 MBYTES
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- PARAMETER MAXMSG (THE MAXIMUM NUMBER OF
C                           BUFR MESSAGES WHICH CAN BE STORED
C                           INTERNALLY) INCREASED FROM 50000 TO 200000;
C                           UNIFIED/PORTABLE FOR WRF; ADDED
C                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
C                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C                           TERMINATES ABNORMALLY
C 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           20,000 TO 50,000 BYTES
C 2004-11-15  D. KEYSER  -- MODIFIED TO NOT ABORT WHEN THERE ARE EITHER
C                           TOO MANY MESSAGES READ IN (I.E., .GT.
C                           MAXMSG) OR TOO MANY BYTES READ IN (I.E.,
C                           .GT. MAXMEM), BUT RATHER JUST STORE MAXMSG
C                           MESSAGES OR MAXMEM BYTES AND PRINT A
C                           DIAGNOSTIC; PARAMETER MAXMEM (THE MAXIMUM
C                           NUMBER OF BYTES REQUIRED TO STORE ALL
C                           MESSAGES INTERNALLY) WAS INCREASED FROM 16
C                           MBYTES TO 50 MBYTES
C 2005-11-29  J. ATOR    -- USE RDMSGW AND NMWRD
C 2009-03-23  J. ATOR    -- MODIFIED TO HANDLE EMBEDDED BUFR TABLE
C                           (DICTIONARY) MESSAGES
C 2012-09-15  J. WOOLLEN -- MODIFIED FOR C/I/O/BUFR INTERFACE;
C                           CALL STATUS TO GET LUN; REPLACE FORTRAN
C                           REWIND AND BACKSPACE WITH C ROUTINES CEWIND
C                           AND BACKBUFR
C
C USAGE:    CALL UFBMEM (LUNIT, INEW, IRET, IUNIT)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C     INEW     - INTEGER: SWITCH:
C                       0 = initialize internal arrays prior to
C                           transferring messages here
C                    else = append the messages transferred here to
C                           internal memory arrays
C
C   OUTPUT ARGUMENT LIST:
C     IRET     - INTEGER: NUMBER OF MESSAGES TRANSFERRED
C     IUNIT    - INTEGER: RETURN CODE:
C                       0 = no messages were read from LUNIT, file is
C                           empty
C                   LUNIT = INEW input as 0
C                    else = FORTRAN logical unit for BUFR file
C                           associated with initial message transferred
C                           to internal memory
C
C   INPUT FILES:
C     UNIT "LUNIT" - BUFR FILE
C
C REMARKS:
C    NOTE THAT IREADMM, RDMEMM, READMM, UFBMMS, UFBMNS, UFBRMS, UFBTAB
C    OR UFBTAM CAN BE CALLED AFTER THIS TO READ SPECIFIC BUFR MESSAGES
C    FROM INTERNAL MEMORY.
C
C    THIS ROUTINE CALLS:        BORT     CLOSBF   CPDXMM   ERRWRT
C                               IDXMSG   NMWRD    OPENBF   RDMSGW
C                               STATUS   CEWIND   BACKBUFR 
C    THIS ROUTINE IS CALLED BY: None
C                               Normally called only by application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /MSGMEM/ MUNIT,MLAST,MSGP(0:MAXMSG),MSGS(MAXMEM),
     .                MDX(MXDXW),IPDXM(MXDXM),LDXM,NDXM,LDXTS,NDXTS,
     .                IFDXTS(MXDXTS),ICDXTS(MXDXTS),IPMSGS(MXDXTS)

      CHARACTER*128 BORT_STR,ERRSTR
      DIMENSION     MBAY(MXMSGLD4)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  TRY TO OPEN BUFR FILE AND SET TO INITIALIZE OR CONCATENATE
C  ----------------------------------------------------------

      CALL OPENBF(LUNIT,'IN',LUNIT)

      IF(INEW.EQ.0) THEN
         MSGP(0) = 0
         MUNIT = 0
         MLAST = 0
         NDXTS = 0
         LDXTS = 0
         NDXM = 0
         LDXM = 0
      ENDIF

      NMSG = MSGP(0)
      IRET = 0
      IFLG = 0
      ITIM = 0

C     Copy any BUFR dictionary table messages from the beginning of
C     LUNIT into COMMON /MSGMEM/ for possible later use.  Note that
C     such a table (if one exists) is already now in scope due to the
C     prior call to subroutine OPENBF, which in turn would have
C     automatically called subroutines READDX, RDBFDX and MAKESTAB
C     for this table.

      ITEMP = NDXTS
      CALL STATUS(LUNIT,LUN,IL,IM)
      CALL CEWIND(LUN)   
      CALL CPDXMM(LUNIT)

C     If a table was indeed present at the beginning of the file,
C     then set the flag to indicate that this table is now in scope.

      IF ((ITEMP+1).EQ.NDXTS) LDXTS = NDXTS

C  TRANSFER MESSAGES FROM FILE TO MEMORY - SET MESSAGE POINTERS
C  ------------------------------------------------------------

1     CALL RDMSGW(LUNIT,MBAY,IER)
      IF(IER.EQ.-1) GOTO 100
      IF(IER.EQ.-2) GOTO 900

      IF(IDXMSG(MBAY).EQ.1) THEN

C	New "embedded" BUFR dictionary table messages have been found in
C	this file.  Copy them into COMMON /MSGMEM/ for later use.

	call backbufr(lun) !BACKSPACE LUNIT
	CALL CPDXMM(LUNIT)
	GOTO 1
      ENDIF

      NMSG = NMSG+1
      IF(NMSG      .GT.MAXMSG) IFLG = 1
      LMEM = NMWRD(MBAY)
      IF(LMEM+MLAST.GT.MAXMEM) IFLG = 2

      IF(IFLG.EQ.0) THEN
         IRET = IRET+1
         DO I=1,LMEM
            MSGS(MLAST+I) = MBAY(I)
         ENDDO
         MSGP(0)    = NMSG
         MSGP(NMSG) = MLAST+1
      ELSE
         IF(ITIM.EQ.0) THEN
            MLAST0 = MLAST
            ITIM=1
         ENDIF
      ENDIF
      MLAST = MLAST+LMEM
      GOTO 1

C  EXITS
C  -----

100   IF(IFLG.EQ.1) THEN

C  EMERGENCY ROOM TREATMENT FOR MAXMSG ARRAY OVERFLOW
C  --------------------------------------------------

      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,A,I8,A)' )
     . 'BUFRLIB: UFBMEM - THE NO. OF MESSAGES REQUIRED TO STORE ',
     . 'ALL MESSAGES INTERNALLY EXCEEDS MAXIMUM (', MAXMSG, 
     . ') - INCOMPLETE READ'
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8,A,I8,A)' )
     . '>>>UFBMEM STORED ', MSGP(0), ' MESSAGES OUT OF ', NMSG, '<<<'
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8,A,I8,A)' )
     . '>>>UFBMEM STORED ', MLAST0, ' BYTES OUT OF ', MLAST, '<<<'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF
      MLAST=MLAST0
      ENDIF

      IF(IFLG.EQ.2) THEN

C  EMERGENCY ROOM TREATMENT FOR MAXMEM ARRAY OVERFLOW
C  --------------------------------------------------

      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,A,I8,A)' )
     . 'BUFRLIB: UFBMEM - THE NO. OF BYTES REQUIRED TO STORE ',
     . 'ALL MESSAGES INTERNALLY EXCEEDS MAXIMUM (', MAXMEM, 
     . ') - INCOMPLETE READ'
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8,A,I8,A)' )
     . '>>>UFBMEM STORED ', MLAST0, ' BYTES OUT OF ', MLAST, '<<<'
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8,A,I8,A)' )
     . '>>>UFBMEM STORED ', MSGP(0), ' MESSAGES OUT OF ', NMSG, '<<<'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF
      MLAST=MLAST0
      ENDIF

      IF(IRET.EQ.0) THEN
         CALL CLOSBF(LUNIT)
      ELSE
         IF(MUNIT.NE.0) CALL CLOSBF(LUNIT)
         IF(MUNIT.EQ.0) MUNIT = LUNIT
      ENDIF
      IUNIT = MUNIT

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: UFBMEM - ERROR READING MESSAGE '//
     . 'NUMBER",I5," INTO MEMORY FROM UNIT",I3)') NMSG+1,LUNIT
      CALL BORT(BORT_STR)
      END
