      SUBROUTINE OPENBF(LUNIT,IO,LUNDX)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    OPENBF
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE NORMALLY (I.E. EXCEPT WHEN INPUT ARGUMENT
!   IO IS 'QUIET') IDENTIFIES A NEW LOGICAL UNIT TO THE BUFR ARCHIVE
!   LIBRARY SOFTWARE FOR INPUT OR OUTPUT OPERATIONS.  HOWEVER, THE
!   FIRST TIME IT IS CALLED, IT ALSO FIGURES OUT SOME IMPORTANT
!   INFORMATION ABOUT THE LOCAL MACHINE ON WHICH THE SOFTWARE IS BEING
!   RUN (VIA A CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE WRDLEN), AND IT
!   ALSO INITIALIZES ARRAYS IN MANY BUFR ARCHIVE LIBRARY COMMON BLOCKS
!   (VIA A CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE BFRINI). UP TO 32
!   LOGICAL UNITS CAN BE CONNECTED TO THE BUFR ARCHIVE LIBRARY SOFTWARE
!   AT ANY ONE TIME.
!
!   NOTE: IF IO IS PASSED IN AS 'QUIET', THEN OPENBF PERFORMS ONLY ONE
!   FUNCTION - IT SIMPLY SETS THE "DEGREE OF PRINTOUT" SWITCH IPRT (IN
!   COMMON BLOCK /QUIET/) TO THE VALUE OF INPUT ARGUMENT LUNDX,
!   OVERRIDING ITS PREVIOUS VALUE.  A DEFAULT IPRT VALUE OF 0 (I.E.
!   "LIMITED PRINTOUT") IS SET DURING THE FIRST CALL TO THIS ROUTINE,
!   BUT THIS OR ANY OTHER IPRT VALUE MAY BE SET AND RESET AS OFTEN AS
!   DESIRED VIA SUCCESSIVE CALLS TO OPENBF WITH IO = 'QUIET'.
!   IN ALL SUCH CASES, OPENBF SIMPLY (RE)SETS IPRT AND THEN RETURNS
!   WITHOUT ACTUALLY OPENING ANY FILES.  THE DEGREE OF PRINTOUT
!   INCREASES AS IPRT INCREASES FROM "-1" TO "0" TO "1" TO "2".
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
!                           ROUTINE "BORT"
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!                           BUFR FILES UNDER THE MPI)
! 2003-11-04  J. ATOR    -- ADDED IO='NUL' OPTION IN ORDER TO PREVENT
!                           LATER WRITING TO BUFR FILE IN LUNIT (WAS IN
!                           DECODER VERSION); ADDED DOCUMENTATION
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
!                           ABNORMALLY, UNUSUAL THINGS HAPPEN OR FOR
!                           INFORMATIONAL PURPOSES
! 2004-08-18  J. ATOR    -- ADDED SAVE FOR IFIRST FLAG AND IO="NODX"
!                           OPTION 
! 2005-11-29  J. ATOR    -- ADDED COMMON /MSGFMT/ AND ICHKSTR CALL
!
! USAGE:    CALL OPENBF (LUNIT, IO, LUNDX)
!   INPUT ARGUMENT LIST:
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
!                (UNLESS IO IS 'QUIET', THEN A DUMMY)
!     IO       - CHARACTER*(*): FLAG INDICATING HOW LUNIT IS TO BE
!                USED BY THE SOFTWARE:
!                    'IN' = input operations
!                   'OUT' = output operations
!                   'APN' = same as 'OUT', except begin writing at end
!                           of file ("append")
!                   'APX' = same as 'APN', except backspace before
!                           appending
!                  'NODX' = same as 'OUT', except don't write dictionary
!                           (i.e. DX) table messages to LUNIT
!                   'NUL' = same as 'OUT', except don't write any
!                           messages whatsoever to LUNIT (e.g. when
!                           subroutine WRITSA is to be used)
!                 'QUIET' = LUNIT is ignored, this is an indicator
!                           that the value for IPRT in COMMON block
!                           /QUIET/ is being reset (see LUNDX)
!     LUNDX    - INTEGER: IF IO IS NOT 'QUIET':
!                            FORTRAN logical unit number containing 
!                            dictionary table information to be used in
!                            reading/writing from/to LUNIT (depending
!                            on the case); may be set equal to LUNIT if
!                            dictionary table information is already
!                            embedded in LUNIT
!                         IF IO IS 'QUIET':
!                            Indicator for degree of printout:
!                              -1 = NO printout except for ABORT
!                                   messages
!                               0 = LIMITED printout (default)
!                               1 = ALL warning messages are printed
!                                   out
!                               2 = ALL warning AND informational
!                                   messages are printed out
!                            (Note: this does not change until OPENBF
!                                   is again called with IO equal to
!                                   'QUIET')
!
!   INPUT FILES:
!     UNIT "LUNIT" - BUFR FILE
!
!   OUTPUT FILES:
!     UNIT 06  - STANDARD OUTPUT PRINT
!
! REMARKS:
!    THIS ROUTINE CALLS:        BFRINI   BORT     DXINIT   ICHKSTR
!                               POSAPN   POSAPX   READDX   STATUS   
!                               WRDLEN   WRITDX   WTSTAT
!    THIS ROUTINE IS CALLED BY: RDMGSB   UFBINX   UFBMEM   UFBTAB
!                               Also called by application programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      INCLUDE 'bufrlib.prm'

      COMMON /MSGCWD/ NMSG(NFILES),NSUB(NFILES),MSUB(NFILES), &
                      INODE(NFILES),IDATE(NFILES)
      COMMON /STBFR / IOLUN(NFILES),IOMSG(NFILES)
      COMMON /NULBFR/ NULL(NFILES)
      COMMON /MSGFMT/ MGWRDS(NFILES)
      COMMON /QUIET / IPRT

      CHARACTER*(*) IO
      CHARACTER*128 BORT_STR
      CHARACTER*54  CPRINT(0:3)
      CHARACTER*1   BSTR(4)

      DATA IFIRST/0/
      DATA          CPRINT/ &
       'No printout except for ABORT messages', &
       'Limited printout (default)', &
       'All warning messages are printed out', &
       'All warning and informational messages are printed out'/

      SAVE IFIRST

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!     If this is the first call to this subroutine, initialize
!     IPRT in /QUIET/ as 0 (limited printout - except for abort
!     messages)

      IF(IFIRST.EQ.0) IPRT = 0

      IF(IO.EQ.'QUIET') THEN
!  .... override previous IPRT value (printout indicator)
         IF(LUNDX.LT.-1)  LUNDX = -1
         IF(LUNDX.GT. 2)  LUNDX =  2
         IF(LUNDX.GE.0) THEN
      PRINT*
      PRINT*,'+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++++'
      PRINT 101, IPRT,CPRINT(IPRT+1),LUNDX,CPRINT(LUNDX+1)
101   FORMAT(' BUFRLIB: OPENBF - THE DEGREE OF PRINTOUT INDICATOR IS ', &
       'BEING CHANGED FROM:'/15X,I3,' - ',A/25X,'to'/15X,I3,' - ',A)
      PRINT*,'+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++++'
      PRINT*
         ENDIF
         IPRT = LUNDX
      ENDIF

      IF(IFIRST.EQ.0) THEN

!        If this is the first call to this subroutine, then call WRDLEN
!        to figure out some important information about the local
!        machine and call BFRINI to initialize some global variables.

!        NOTE: WRDLEN must be called prior to calling BFRINI!  

         CALL WRDLEN
         CALL BFRINI
         IFIRST = 1
      ENDIF

      IF(IO.EQ.'QUIET') GOTO 100

!  SEE IF A FILE CAN BE OPENED
!  ---------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(LUN.EQ.0) GOTO 900
      IF(IL .NE.0) GOTO 901
      NULL(LUN) = 0
      MGWRDS(LUN) = 0

!  CHECK FOR NO BUFR DATA OR NO DATA AT ALL IN AN "IN" FILE
!  --------------------------------------------------------

!  Note that we only want to do this check if LUNIT=LUNDX, in order
!  to allow for the possibility that input BUFR messages may be passed
!  to the BUFR ARCHIVE LIBRARY software via an alternative method (e.g.
!  a future call to subroutine READERME) rather than read directly from
!  LUNIT, which is the usual method.

      IF(IO.EQ.'IN' .AND. LUNIT.EQ.LUNDX) THEN
         REWIND LUNIT
         READ(LUNIT,END=200,ERR=902) (BSTR(I),I=1,4)

!        Confirm that the BSTR array contains 'BUFR' encoded in
!        CCITT IA5 (i.e. ASCII).

         IF(ICHKSTR('BUFR',BSTR,4).NE.0) GOTO 903
      ENDIF

!  SET INITIAL OPEN DEFAULTS (CLEAR OUT A MSG CONTROL WORD PARTITION)
!  ------------------------------------------------------------------

      IF(IO.NE.'NUL') THEN
        REWIND LUNIT
      ENDIF
      NMSG (LUN) = 0
      NSUB (LUN) = 0
      MSUB (LUN) = 0
      INODE(LUN) = 0
      IDATE(LUN) = 0

!  DECIDE HOW TO SETUP THE DICTIONARY
!  ----------------------------------

      IF(IO.EQ.'IN') THEN
         CALL WTSTAT(LUNIT,LUN,-1,0)
         CALL READDX(LUNIT,LUN,LUNDX)
      ELSE IF(IO.EQ.'OUT') THEN
         CALL WTSTAT(LUNIT,LUN, 1,0)
         CALL WRITDX(LUNIT,LUN,LUNDX)
      ELSE IF(IO.EQ.'APN' .OR. IO.EQ.'APX' &
         .OR. IO.EQ.'NODX'.OR. IO.EQ.'NUL') THEN
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

!     FILE OPENED FOR INPUT IS EMPTY - LET READMG OR READERME GIVE
!     THE BAD NEWS LATER

200   REWIND LUNIT
      IF(IPRT.GE.0) THEN
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*, 'BUFRLIB: OPENBF - INPUT BUFR FILE IN UNIT ',LUNIT, &
       ' IS EMPTY'
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*
      ENDIF
      CALL WTSTAT(LUNIT,LUN,-1,0)

!  INITIALIZE THE DICTIONARY TABLE PARTITION
!  -----------------------------------------

      CALL DXINIT(LUN,0)

!  EXITS
!  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: OPENBF - THERE ARE ALREADY",I3,'// &
       '" BUFR FILES OPENED, CANNOT OPEN FILE CONNECTED TO UNIT",I4)') &
       NFILES,LUNIT
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: OPENBF - THE FILE CONNECTED TO UNIT"'// &
       ',I5," IS ALREADY OPEN")') LUNIT
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: OPENBF - ERROR READING INPUT FILE '// &
       'CONNECTED TO UNIT",I4," WHEN CHECKING FOR ''BUFR'' IN FIRST 4'// &
       ' BYTES OF RECORD")') LUNIT
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: OPENBF - FIRST 4 BYTES READ FROM '// &
       'RECORD IN INPUT FILE CONNECTED TO UNIT",I4," NOT ''BUFR'', '// &
       'DOES NOT CONTAIN BUFR DATA")') LUNIT
      CALL BORT(BORT_STR)
904   CALL BORT('BUFRLIB: OPENBF - SECOND (INPUT) ARGUMENT IS NOT ONE'// &
       ' OF THE FOLLOWING: "IN", "OUT", "NODX", "NUL", "APN", "APX"'// &
       ' OR "QUIET"')
      END SUBROUTINE OPENBF
