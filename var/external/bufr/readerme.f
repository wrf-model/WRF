      SUBROUTINE READERME(MESG,LUNIT,SUBSET,JDATE,IRET)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    READERME
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1995-06-28
C
C ABSTRACT: THIS SUBROUTINE READS INFORMATION FROM A BUFR DATA MESSAGE
C   ALREADY IN MEMORY, PASSED IN AS AN INPUT ARGUMENT.  IT IS SIMILAR
C   TO BUFR ARCHIVE LIBRARY SUBROUTINE READMG EXCEPT, INSTEAD OF
C   READING BUFR MESSAGES DIRECTLY FROM A BUFR FILE THAT IS PHYSICALLY
C   STORED ON THE LOCAL SYSTEM AND INTERFACED TO THE SOFTWARE VIA A
C   LOGICAL UNIT NUMBER, IT READS BUFR MESSAGES DIRECTLY FROM A MEMORY
C   ARRAY WITHIN THE APPLICATION PROGRAM ITSELF.  THIS PROVIDES USERS
C   WITH GREATER FLEXIBILITY FROM AN INPUT/OUTPUT PERSPECTIVE.
C   READERME CAN BE USED IN ANY CONTEXT IN WHICH READMG MIGHT OTHERWISE
C   BE USED.  IF THIS MESSAGE IS NOT A BUFR MESSAGE, THEN AN
C   APPROPRIATE CALL IS MADE TO BUFR ARCHIVE LIBRARY SUBROUTINE BORT.
C
C PROGRAM HISTORY LOG:
C 1995-06-28  J. WOOLLEN -- ORIGINAL AUTHOR (FOR ERS DATA)
C 1997-07-29  J. WOOLLEN -- MODIFIED TO PROCESS GOES SOUNDINGS FROM
C                           NESDIS
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"; MODIFIED TO MAKE Y2K
C                           COMPLIANT; IMPROVED MACHINE PORTABILITY
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI); INCREASED THE
C                           MAXIMUM NUMBER OF POSSIBLE DESCRIPTORS IN A
C                           SUBSET FROM 1000 TO 3000
C 2000-09-19  J. WOOLLEN -- REMOVED MESSAGE DECODING LOGIC THAT HAD
C                           BEEN REPLICATED IN THIS AND OTHER READ
C                           ROUTINES AND CONSOLIDATED IT INTO A NEW
C                           ROUTINE CKTABA, CALLED HERE, WHICH IS
C                           ENHANCED TO ALLOW COMPRESSED AND STANDARD
C                           BUFR MESSAGES TO BE READ (ROUTINE UNCMPS,
C                           WHICH HAD BEEN CALLED BY THIS AND OTHER
C                           ROUTINES IS NOW OBSOLETE AND HAS BEEN
C                           REMOVED FROM THE BUFRLIB; MAXIMUM MESSAGE
C                           LENGTH INCREASED FROM 10,000 TO 20,000
C                           BYTES
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
C                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C                           TERMINATES ABNORMALLY
C 2004-08-18  J. ATOR    -- MODIFIED 'BUFR' STRING TEST FOR PORTABILITY
C                           TO EBCDIC MACHINES; MAXIMUM MESSAGE LENGTH
C                           INCREASED FROM 20,000 TO 50,000 BYTES
C 2005-11-29  J. ATOR    -- USE ICHKSTR
C 2009-03-23  D. KEYSER  -- CALL BORT IN CASE OF MBAY OVERFLOW
C 2009-03-23  J. ATOR    -- ADD LOGIC TO ALLOW SECTION 3 DECODING;
C                           ADD LOGIC TO PROCESS DICTIONARY MESSAGES
C 2012-06-07  J. ATOR    -- DON'T RESPOND TO DX TABLE MESSAGES IF
C                           SECTION 3 DECODING IS BEING USED
C
C USAGE:    CALL READERME (MESG, LUNIT, SUBSET, JDATE, IRET)
C   INPUT ARGUMENT LIST:
C     MESG     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING BUFR
C                MESSAGE
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C
C   OUTPUT ARGUMENT LIST:
C     SUBSET   - CHARACTER*8: TABLE A MNEMONIC FOR TYPE OF BUFR MESSAGE
C                BEING READ
C     JDATE    - INTEGER: DATE-TIME STORED WITHIN SECTION 1 OF BUFR
C                MESSAGE BEING READ, IN FORMAT OF EITHER YYMMDDHH OR
C                YYYYMMDDHH, DEPENDING ON DATELEN() VALUE
C     IRET     - INTEGER: RETURN CODE:
C                       0 = normal return
C                      -1 = unrecognized Table A message type
C                      11 = this is a BUFR table (dictionary) message
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     CKTABA   DXINIT   ERRWRT
C                               ICHKSTR  IDXMSG   IUPBS3   LMSG
C                               MAKESTAB READS3   STATUS   STBFDX
C                               WTSTAT
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

      COMMON /SC3BFR/ ISC3(NFILES),TAMNEM(NFILES)
      COMMON /BITBUF/ MAXBYT,IBIT,IBAY(MXMSGLD4),MBYT(NFILES),
     .                MBAY(MXMSGLD4,NFILES)
      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)
      COMMON /QUIET/  IPRT

      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*8 SUBSET,SEC0,TAMNEM
      CHARACTER*1 CEC0(8)

      DIMENSION   MESG(*),IEC0(2)

      DIMENSION   IDRDM(NFILES)

      LOGICAL ENDTBL

      EQUIVALENCE (SEC0,IEC0,CEC0)

      DATA IDRDM/NFILES*0/
      SAVE IDRDM

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      CALL WTSTAT(LUNIT,LUN,IL, 1)

C  COPY THE INPUT MESSAGE INTO THE INTERNAL MESSAGE BUFFER
C  -------------------------------------------------------

      IEC0(1) = MESG(1)
      IEC0(2) = MESG(2)
      LNMSG = LMSG(SEC0)
      IF(LNMSG*NBYTW.GT.MXMSGL) GOTO 902
      DO I=1,LNMSG
        MBAY(I,LUN) = MESG(I)
      ENDDO

C     Confirm that the first 4 bytes of SEC0 contain 'BUFR' encoded in
C     CCITT IA5 (i.e. ASCII).

      IF(ICHKSTR('BUFR',CEC0,4).NE.0) GOTO 903

C  PARSE THE MESSAGE SECTION CONTENTS
C  ----------------------------------

      IF(ISC3(LUN).NE.0) CALL READS3(LUN)

      CALL CKTABA(LUN,SUBSET,JDATE,IRET)

      IF(ISC3(LUN).NE.0) RETURN

C  CHECK FOR A DX DICTIONARY MESSAGE
C  ---------------------------------

C     A new DX dictionary table can be passed in as a consecutive set of
C     DX dictionary messages.  Each message should be passed in one at a
C     time, via input argument MESG during consecutive calls to this
C     subroutine, and will be processed as a single dictionary table up
C     until the next message is passed in which either contains no data
C     subsets or else is a non-DX dictionary message.

      ENDTBL = .FALSE.

      IF(IDXMSG(MBAY(1,LUN)).EQ.1) THEN

C	This is a DX dictionary message that was generated by the
C	BUFRLIB archive library software.

	IF(IUPBS3(MBAY(1,LUN),'NSUB').EQ.0) THEN

C	  But it doesn't contain any actual dictionary information, so
C	  assume we've reached the end of the dictionary table.

	  IF(IDRDM(LUN).GT.0) THEN
	    ENDTBL = .TRUE.
          ENDIF
	ELSE
	  IF(IDRDM(LUN).EQ.0) THEN

C	    This is the first DX dictionary message that is part of a
C	    new dictionary table.

	    CALL DXINIT(LUN,0)
	  ENDIF
	  IDRDM(LUN) = IDRDM(LUN) + 1
	  CALL STBFDX(LUN,MBAY(1,LUN))
	ENDIF
      ELSE IF(IDRDM(LUN).GT.0) THEN

C	This is the first non-DX dictionary message received following a
C       string of DX dictionary messages, so assume we've reached the
C	end of the dictionary table.

	ENDTBL = .TRUE.
      ENDIF

      IF(ENDTBL) THEN
	IF ( IPRT .GE. 2 ) THEN
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
	WRITE ( UNIT=ERRSTR, FMT='(A,I3,A)' )
     .    'BUFRLIB: READERME - STORED NEW DX TABLE CONSISTING OF (',
     .    IDRDM(LUN), ') MESSAGES;'
	CALL ERRWRT(ERRSTR)
	ERRSTR = 'WILL APPLY THIS TABLE TO ALL SUBSEQUENT DATA '//
     .    'MESSAGES UNTIL NEXT DX TABLE IS PASSED IN'
	CALL ERRWRT(ERRSTR)
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        CALL ERRWRT(' ')
	ENDIF
	IDRDM(LUN) = 0
	CALL MAKESTAB
      ENDIF

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: READERME - INPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: READERME - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   WRITE(BORT_STR,'("BUFRLIB: READERME - INPUT BUFR MESSAGE LENGTH",
     . 1X,I6," BYTES) IS LARGER THAN LIMIT OF ",I6," BYTES")')
     . LNMSG*NBYTW,MXMSGL
      CALL BORT(BORT_STR)
903   CALL BORT('BUFRLIB: READERME - FIRST 4 BYTES READ FROM RECORD'//
     . ' NOT "BUFR", DOES NOT CONTAIN BUFR DATA')
      END
