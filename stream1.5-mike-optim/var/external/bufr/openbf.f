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
C
C USAGE:    CALL OPENBF (LUNIT, IO, LUNDX)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C                (UNLESS IO IS 'QUIET', THEN A DUMMY)
C     IO       - CHARACTER*(*): FLAG INDICATING HOW LUNIT IS TO BE
C                USED BY THE SOFTWARE:
C                    'IN' = input operations
C                   'OUT' = output operations
C                   'APN' = same as 'OUT', except begin writing at end
C                           of file ("append")
C                   'APX' = same as 'APN', except backspace before
C                           appending
C                  'NODX' = same as 'OUT', except don't write dictionary
C                           (i.e. DX) table messages to LUNIT
C                   'NUL' = same as 'OUT', except don't write any
C                           messages whatsoever to LUNIT (e.g. when
C                           subroutine WRITSA is to be used)
C                 'QUIET' = LUNIT is ignored, this is an indicator
C                           that the value for IPRT in COMMON block
C                           /QUIET/ is being reset (see LUNDX)
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
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C REMARKS:
C    THIS ROUTINE CALLS:        BFRINI   BORT     DXINIT   ICHKSTR
C                               POSAPN   POSAPX   READDX   STATUS   
C                               WRDLEN   WRITDX   WTSTAT
C    THIS ROUTINE IS CALLED BY: RDMGSB   UFBINX   UFBMEM   UFBTAB
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
      COMMON /MSGFMT/ MGWRDS(NFILES)
      COMMON /QUIET / IPRT

      CHARACTER*(*) IO
      CHARACTER*128 BORT_STR
      CHARACTER*54  CPRINT(0:3)
      CHARACTER*1   BSTR(4)

      DATA IFIRST/0/
      DATA          CPRINT/
     . 'No printout except for ABORT messages',
     . 'Limited printout (default)',
     . 'All warning messages are printed out',
     . 'All warning and informational messages are printed out'/

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
      PRINT*
      PRINT*,'+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++++'
      PRINT 101, IPRT,CPRINT(IPRT+1),LUNDX,CPRINT(LUNDX+1)
101   FORMAT(' BUFRLIB: OPENBF - THE DEGREE OF PRINTOUT INDICATOR IS ',
     . 'BEING CHANGED FROM:'/15X,I3,' - ',A/25X,'to'/15X,I3,' - ',A)
      PRINT*,'+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++++'
      PRINT*
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

      IF(IO.EQ.'QUIET') GOTO 100

C  SEE IF A FILE CAN BE OPENED
C  ---------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(LUN.EQ.0) GOTO 900
      IF(IL .NE.0) GOTO 901
      NULL(LUN) = 0
      MGWRDS(LUN) = 0

C  CHECK FOR NO BUFR DATA OR NO DATA AT ALL IN AN "IN" FILE
C  --------------------------------------------------------

C  Note that we only want to do this check if LUNIT=LUNDX, in order
C  to allow for the possibility that input BUFR messages may be passed
C  to the BUFR ARCHIVE LIBRARY software via an alternative method (e.g.
C  a future call to subroutine READERME) rather than read directly from
C  LUNIT, which is the usual method.

      IF(IO.EQ.'IN' .AND. LUNIT.EQ.LUNDX) THEN
         REWIND LUNIT
         READ(LUNIT,END=200,ERR=902) (BSTR(I),I=1,4)

C        Confirm that the BSTR array contains 'BUFR' encoded in
C        CCITT IA5 (i.e. ASCII).

         IF(ICHKSTR('BUFR',BSTR,4).NE.0) GOTO 903
      ENDIF

C  SET INITIAL OPEN DEFAULTS (CLEAR OUT A MSG CONTROL WORD PARTITION)
C  ------------------------------------------------------------------

      IF(IO.NE.'NUL') THEN
        REWIND LUNIT
      ENDIF
      NMSG (LUN) = 0
      NSUB (LUN) = 0
      MSUB (LUN) = 0
      INODE(LUN) = 0
      IDATE(LUN) = 0

C  DECIDE HOW TO SETUP THE DICTIONARY
C  ----------------------------------

      IF(IO.EQ.'IN') THEN
         CALL WTSTAT(LUNIT,LUN,-1,0)
         CALL READDX(LUNIT,LUN,LUNDX)
      ELSE IF(IO.EQ.'OUT') THEN
         CALL WTSTAT(LUNIT,LUN, 1,0)
         CALL WRITDX(LUNIT,LUN,LUNDX)
      ELSE IF(IO.EQ.'APN' .OR. IO.EQ.'APX'
     .   .OR. IO.EQ.'NODX'.OR. IO.EQ.'NUL') THEN
         CALL WTSTAT(LUNIT,LUN, 1,0)
         CALL READDX(LUNIT,LUN,LUNDX)
         IF(IO.EQ.'APN') THEN
           CALL POSAPN(LUNIT)
         ELSE IF(IO.EQ.'APX') THEN
           CALL POSAPX(LUNIT)
         ELSE IF(IO.EQ.'NUL') THEN
           NULL(LUN) = 1
         ENDIF
      ELSE
         GOTO 904
      ENDIF

      GOTO 100

C     FILE OPENED FOR INPUT IS EMPTY - LET READMG OR READERME GIVE
C     THE BAD NEWS LATER

200   REWIND LUNIT
      IF(IPRT.GE.0) THEN
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*, 'BUFRLIB: OPENBF - INPUT BUFR FILE IN UNIT ',LUNIT,
     . ' IS EMPTY'
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*
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
902   WRITE(BORT_STR,'("BUFRLIB: OPENBF - ERROR READING INPUT FILE '//
     . 'CONNECTED TO UNIT",I4," WHEN CHECKING FOR ''BUFR'' IN FIRST 4'//
     . ' BYTES OF RECORD")') LUNIT
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: OPENBF - FIRST 4 BYTES READ FROM '//
     . 'RECORD IN INPUT FILE CONNECTED TO UNIT",I4," NOT ''BUFR'', '//
     . 'DOES NOT CONTAIN BUFR DATA")') LUNIT
      CALL BORT(BORT_STR)
904   CALL BORT('BUFRLIB: OPENBF - SECOND (INPUT) ARGUMENT IS NOT ONE'//
     . ' OF THE FOLLOWING: "IN", "OUT", "NODX", "NUL", "APN", "APX"'//
     . ' OR "QUIET"')
      END
