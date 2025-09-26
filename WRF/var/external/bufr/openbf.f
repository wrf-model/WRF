      SUBROUTINE OPENBF(LUNIT,IO,LUNDX)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    OPENBF
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE NORMALLY (I.E. EXCEPT WHEN INPUT ARGUMENT
C   IO IS 'QUIET') IDENTIFIES A NEW LOGICAL UNIT TO THE BUFR ARCHIVE
C   LIBRARY SOFTWARE FOR INPUT OR OUTPUT OPERATIONS.  HOWEVER, THE
C   FIRST TIME IT IS CALLED, IT ALSO FIGURES OUT SOME IMPORTANT
C   INFORMATION ABOUT THE LOCAL MACHINE ON WHICH THE SOFTWARE IS BEING
C   RUN (VIA A CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE WRDLEN), AND IT
C   ALSO INITIALIZES ARRAYS IN MANY BUFR ARCHIVE LIBRARY COMMON BLOCKS
C   (VIA A CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE BFRINI). UP TO 32
C   LOGICAL UNITS CAN BE CONNECTED TO THE BUFR ARCHIVE LIBRARY SOFTWARE
C   AT ANY ONE TIME.
C
C   NOTE: IF IO IS PASSED IN AS 'QUIET', THEN OPENBF PERFORMS ONLY ONE
C   FUNCTION - IT SIMPLY SETS THE "DEGREE OF PRINTOUT" SWITCH IPRT (IN
C   COMMON BLOCK /QUIET/) TO THE VALUE OF INPUT ARGUMENT LUNDX,
C   OVERRIDING ITS PREVIOUS VALUE.  A DEFAULT IPRT VALUE OF 0 (I.E.
C   "LIMITED PRINTOUT") IS SET DURING THE FIRST CALL TO THIS ROUTINE,
C   BUT THIS OR ANY OTHER IPRT VALUE MAY BE SET AND RESET AS OFTEN AS
C   DESIRED VIA SUCCESSIVE CALLS TO OPENBF WITH IO = 'QUIET'.
C   IN ALL SUCH CASES, OPENBF SIMPLY (RE)SETS IPRT AND THEN RETURNS
C   WITHOUT ACTUALLY OPENING ANY FILES.  THE DEGREE OF PRINTOUT
C   INCREASES AS IPRT INCREASES FROM "-1" TO "0" TO "1" TO "2".
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2003-11-04  J. ATOR    -- ADDED IO='NUL' OPTION IN ORDER TO PREVENT
C                           LATER WRITING TO BUFR FILE IN LUNIT (WAS IN
C                           DECODER VERSION); ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C                           ABNORMALLY, UNUSUAL THINGS HAPPEN OR FOR
C                           INFORMATIONAL PURPOSES
C 2004-08-18  J. ATOR    -- ADDED SAVE FOR IFIRST FLAG AND IO="NODX"
C                           OPTION 
C 2005-11-29  J. ATOR    -- ADDED COMMON /MSGFMT/ AND ICHKSTR CALL
C 2009-03-23  J. ATOR    -- ADDED IO='SEC3' OPTION; REMOVED CALL TO
C                           POSAPN; CLARIFIED COMMENTS; USE ERRWRT
C 2010-05-11  J. ATOR    -- ADDED COMMON /STCODE/
C 2012-06-18  J. ATOR    -- ADDED IO='INUL' OPTION
C 2012-09-15  J. WOOLLEN -- MODIFIED FOR C/I/O/BUFR INTERFACE;
C                           USE INQUIRE TO OBTAIN THE FILENAME;
C                           CALL C ROUTINES OPENRB, OPENWB, AND 
C                           OPENAB TO CONNECT BUFR FILES TO C;
C                           ADDED IO TYPE 'INX' TO ENABLE OPEN AND
C                           CLOSE FOR C FILE WITHOUT CLOSING FORTRAN
C                           FILE; ADD IO TYPE 'FIRST' TO SUPPORT CALLS   
C                           TO BFRINI AND WRDLEN PRIOR TO USER RESET
C                           OF BUFRLIB PARAMETERS FOUND IN NEW ROUTINES 
C                           SETBMISS AND SETBLOCK
C
C USAGE:    CALL OPENBF (LUNIT, IO, LUNDX)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C                (UNLESS IO IS 'QUIET', THEN A DUMMY)
C     IO       - CHARACTER*(*): FLAG INDICATING HOW LUNIT IS TO BE
C                USED BY THE SOFTWARE:
C                    'IN' = input operations with table processing
C                   'INX' = input  operations w/o table processing
C                   'OUX' = output operations w/o table processing
C                   'OUT' = output operations with table processing
C                  'SEC3' = same as 'IN', except use Section 3 of input 
C                           messages for decoding rather than dictionary
C                           table information from LUNDX; in this case
C                           LUNDX is ignored, and user must provide 
C                           appropriate BUFR master tables within
C                           directory specified by a subsequent call
C                           to subroutine MTINFO
C                  'NODX' = same as 'OUT', except don't write dictionary
C                           (i.e. DX) table messages to LUNIT
C                   'APN' = same as 'NODX', except begin writing at end
C                           of file ("append")
C                   'APX' = same as 'APN', except backspace before
C                           appending
C                   'NUL' = same as 'OUT', except don't write any
C                           messages whatsoever to LUNIT (e.g. when
C                           subroutine WRITSA is to be used)
C                  'INUL' = same as 'IN', except don't read any
C                           messages whatsoever from LUNIT (e.g. when
C                           subroutine READERME is to be used)
C                 'QUIET' = LUNIT is ignored, this is an indicator
C                           that the value for IPRT in COMMON block
C                           /QUIET/ is being reset (see LUNDX)
C                 'FIRST' = calls bfrini and wrdlen as a prelude to user 
c                           resetting of bufrlib parameters such as
c                           missing value or output block type
C     LUNDX    - INTEGER: IF IO IS NOT 'QUIET':
C                            FORTRAN logical unit number containing 
C                            dictionary table information to be used in
C                            reading/writing from/to LUNIT (depending
C                            on the case); may be set equal to LUNIT if
C                            dictionary table information is already
C                            embedded in LUNIT
C                         IF IO IS 'QUIET':
C                            Indicator for degree of printout:
C                              -1 = NO printout except for ABORT
C                                   messages
C                               0 = LIMITED printout (default)
C                               1 = ALL warning messages are printed
C                                   out
C                               2 = ALL warning AND informational
C                                   messages are printed out
C                            (Note: this does not change until OPENBF
C                                   is again called with IO equal to
C                                   'QUIET')
C
C   INPUT FILES:
C     UNIT "LUNIT" - BUFR FILE
C
C REMARKS:
C    THIS ROUTINE CALLS:        BFRINI   BORT     DXINIT   ERRWRT
C                               POSAPX   READDX   STATUS   WRDLEN
C                               WRITDX   WTSTAT   OPENRB   OPENWB
C                               OPENAB
C    THIS ROUTINE IS CALLED BY: COPYBF   GETBMISS MESGBC   MESGBF
C                               RDMGSB   UFBINX   UFBMEM   UFBMEX
C                               UFBTAB   SETBMISS SETBLOCK
C                               Also called by application programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /MSGCWD/ NMSG(NFILES),NSUB(NFILES),MSUB(NFILES),
     .                INODE(NFILES),IDATE(NFILES)
      COMMON /STBFR / IOLUN(NFILES),IOMSG(NFILES)
      COMMON /NULBFR/ NULL(NFILES)
      COMMON /SC3BFR/ ISC3(NFILES),TAMNEM(NFILES)
      COMMON /LUSHR/  LUS(NFILES)
      COMMON /STCODE/ ISCODES(NFILES)
      COMMON /QUIET / IPRT

      CHARACTER*(*) IO
      CHARACTER*255 filename,fileacc   
      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*28  CPRINT(0:3)
      CHARACTER*8   TAMNEM
      CHARACTER*1   BSTR(4)

      DATA IFIRST/0/
      DATA          CPRINT/
     . ' (only ABORTs)              ',
     . ' (limited - default)        ',
     . ' (all warnings)             ',
     . ' (all warning+informational)'/

      SAVE IFIRST

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     If this is the first call to this subroutine, initialize
C     IPRT in /QUIET/ as 0 (limited printout - except for abort
C     messages)

      IF(IFIRST.EQ.0) IPRT = 0

      IF(IO.EQ.'QUIET') THEN
c  .... override previous IPRT value (printout indicator)
         IF(LUNDX.LT.-1)  LUNDX = -1
         IF(LUNDX.GT. 2)  LUNDX =  2
         IF(LUNDX.GE.0) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,I3,A,A,I3,A)' )
     . 'BUFRLIB: OPENBF - DEGREE OF MESSAGE PRINT INDICATOR '//
     . 'CHNGED FROM',IPRT,CPRINT(IPRT+1),' TO',LUNDX,CPRINT(LUNDX+1)
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         IPRT = LUNDX
      ENDIF

      IF(IFIRST.EQ.0) THEN

C        If this is the first call to this subroutine, then call WRDLEN
C        to figure out some important information about the local
C        machine and call BFRINI to initialize some global variables.

C        NOTE: WRDLEN must be called prior to calling BFRINI!  

         CALL WRDLEN
         CALL BFRINI
         IFIRST = 1
      ENDIF

      IF(IO.EQ.'FIRST') GOTO 100
      IF(IO.EQ.'QUIET') GOTO 100

C  SEE IF A FILE CAN BE OPENED
C  ---------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(LUN.EQ.0) GOTO 900
      IF(IL .NE.0) GOTO 901
      NULL(LUN) = 0
      ISC3(LUN) = 0
      ISCODES(LUN) = 0
      LUS(LUN) = 0

C  USE INQUIRE TO OBTAIN THE FILENAME ASSOCIATED WITH UNIT LUNIT
C  -------------------------------------------------------------

      IF (IO.NE.'NUL' .AND. IO.NE.'INUL') THEN
         inquire(lunit,access=fileacc)
         if(fileacc=='UNDEFINED') open(lunit)
         inquire(lunit,name=filename)
         filename=trim(filename)//char(0)
      ENDIF

C  SET INITIAL OPEN DEFAULTS (CLEAR OUT A MSG CONTROL WORD PARTITION)
C  ------------------------------------------------------------------

      NMSG (LUN) = 0
      NSUB (LUN) = 0
      MSUB (LUN) = 0
      INODE(LUN) = 0
      IDATE(LUN) = 0

C  DECIDE HOW TO OPEN THE FILE AND SETUP THE DICTIONARY
C  ----------------------------------------------------

      IF(IO.EQ.'IN') THEN
         call openrb(lun,filename)
         CALL WTSTAT(LUNIT,LUN,-1,0)
         CALL READDX(LUNIT,LUN,LUNDX)
      ELSE IF(IO.EQ.'INUL') THEN
         CALL WTSTAT(LUNIT,LUN,-1,0)
         IF(LUNIT.NE.LUNDX) CALL READDX(LUNIT,LUN,LUNDX)
         NULL(LUN) = 1
      ELSE IF(IO.EQ.'NUL') THEN
         CALL WTSTAT(LUNIT,LUN, 1,0)
         IF(LUNIT.NE.LUNDX) CALL READDX(LUNIT,LUN,LUNDX)
         NULL(LUN) = 1
      ELSE IF(IO.EQ.'INX') THEN
         call openrb(lun,filename)
         CALL WTSTAT(LUNIT,LUN,-1,0)
         NULL(LUN) = 1
      ELSE IF(IO.EQ.'OUX') THEN
         call openwb(lun,filename)
         CALL WTSTAT(LUNIT,LUN, 1,0)
      ELSE IF(IO.EQ.'SEC3') THEN
         call openrb(lun,filename)
         CALL WTSTAT(LUNIT,LUN,-1,0)
         ISC3(LUN) = 1
      ELSE IF(IO.EQ.'OUT') THEN
         call openwb(lun,filename)
         CALL WTSTAT(LUNIT,LUN, 1,0)
         CALL WRITDX(LUNIT,LUN,LUNDX)
      ELSE IF(IO.EQ.'NODX') THEN
         call openwb(lun,filename)
         CALL WTSTAT(LUNIT,LUN, 1,0)
         CALL READDX(LUNIT,LUN,LUNDX)
      ELSE IF(IO.EQ.'APN' .OR. IO.EQ.'APX') THEN
         call openab(lun,filename)
         CALL WTSTAT(LUNIT,LUN, 1,0)
         IF(LUNIT.NE.LUNDX) CALL READDX(LUNIT,LUN,LUNDX)
         CALL POSAPX(LUNIT)
      ELSE
         GOTO 904
      ENDIF

      GOTO 100

C     FILE OPENED FOR INPUT IS EMPTY - LET READMG OR READERME GIVE
C     THE BAD NEWS LATER

200   REWIND LUNIT
      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,I3,A)' )
     .  'BUFRLIB: OPENBF - INPUT BUFR FILE IN UNIT ', LUNIT,
     .  ' IS EMPTY'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF
      CALL WTSTAT(LUNIT,LUN,-1,0)

C  INITIALIZE THE DICTIONARY TABLE PARTITION
C  -----------------------------------------

      CALL DXINIT(LUN,0)

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: OPENBF - THERE ARE ALREADY",I3,'//
     . '" BUFR FILES OPENED, CANNOT OPEN FILE CONNECTED TO UNIT",I4)')
     . NFILES,LUNIT
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: OPENBF - THE FILE CONNECTED TO UNIT"'//
     . ',I5," IS ALREADY OPEN")') LUNIT
      CALL BORT(BORT_STR)
904   CALL BORT('BUFRLIB: OPENBF - SECOND (INPUT) ARGUMENT MUST BE'//
     . ' "IN", "OUT", "NODX", "NUL", "APN", "APX", "SEC3"'//
     . ' OR "QUIET"')
      END
