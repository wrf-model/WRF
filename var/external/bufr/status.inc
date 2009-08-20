      SUBROUTINE STATUS(LUNIT,LUN,IL,IM)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    STATUS
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE CHECKS WHETHER LOGICAL UNIT NUMBER LUNIT
!   (AND ITS ASSOCIATED BUFR FILE) IS CURRENTLY CONNECTED TO THE
!   BUFR ARCHIVE LIBRARY SOFTWARE.  IF SO, IT RETURNS THE I/O STREAM
!   INDEX (LUN) ASSOCIATED WITH THE LOGICAL UNIT NUMBER, THE LOGICAL
!   UNIT STATUS INDICATOR (IL), AND THE BUFR MESSAGE STATUS INDICATOR
!   (IM) FOR THAT I/O STREAM INDEX.  OTHERWISE, IT CHECKS WHETHER THERE
!   IS SPACE FOR A NEW I/O STREAM INDEX AND, IF SO, RETURNS THE NEXT
!   AVAILABLE I/O STREAM INDEX IN LUN IN ORDER TO DEFINE LUNIT (IL AND
!   IM ARE RETURNED AS ZERO, THEY ARE LATER DEFINED VIA CALLS TO BUFR
!   ARCHIVE LIBRARY SUBROUTINE WTSTAT IN THIS CASE).  IF THERE IS NO
!   SPACE FOR A NEW I/O STREAM INDEX, LUN IS RETURNED AS ZERO (AS WELL
!   AS IL AND IM) MEANING LUNIT COULD NOT BE CONNECTED TO THE BUFR
!   ARCHIVE LIBRARY SOFTWARE.  LUN IS USED TO IDENTIFY UP TO "NFILES"
!   UNIQUE BUFR FILES IN THE VARIOUS INTERNAL ARRAYS.
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1996-12-11  J. WOOLLEN -- FIXED A LONG STANDING BUG WHICH OCCURS IN
!                           UNUSUAL SITUATIONS, VERY LOW IMPACT
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
!                           ROUTINE "BORT"
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!                           BUFR FILES UNDER THE MPI)
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
!                           ABNORMALLY
!
! USAGE:    CALL STATUS ( LUNIT, LUN, IL, IM )
!   INPUT ARGUMENT LIST:
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
!
!   OUTPUT ARGUMENT LIST:
!     LUN      - INTEGER: I/O STREAM INDEX ASSOCIATED WITH LOGICAL UNIT
!                LUNIT
!                       0 = LUNIT is not currently connected to the
!                           BUFR Archive Library software and there is
!                           no space for a new I/O stream index
!     IL       - INTEGER: LOGICAL UNIT STATUS INDICATOR:
!                       0 = LUNIT is not currently connected to the
!                           BUFR Archive Library software or it was
!                           just connected in this call to STATUS
!                       1 = LUNIT is connected to the BUFR Archive
!                           Library software as an output file
!                      -1 = LUNIT is connected to the BUFR Archive
!                           Library software as an input file
!     IM       - INTEGER: INDICATOR AS TO WHETHER THERE IS A BUFR
!                MESSAGE CURRENTLY OPEN WITHIN MEMORY FOR THIS LUNIT:
!                       0 = no or LUNIT was just connected to the
!                           BUFR Archive Library software in this call
!                           to STATUS
!                       1 = yes
!
! REMARKS:
!    THIS ROUTINE CALLS:        BORT
!    THIS ROUTINE IS CALLED BY: CLOSBF   CLOSMG   COPYBF   COPYMG
!                               COPYSB   CPYMEM   DATEBF   DRFINI
!                               DUMPBF   DXDUMP   GETABDB  IFBGET
!                               INVMRG   IUPVS01  IUPVS1   MESGBC
!                               MINIMG   MSGWRT   NMSUB    OPENBF
!                               OPENMB   OPENMG   RDMEMM   RDMEMS
!                               RDMGSB   READDX   READERME READLC
!                               READMG   READMM   READNS   READSB
!                               REWNBF   STNDRD   UFBCNT   UFBCPY
!                               UFBCUP   UFBDMP   UFBEVN   UFBGET
!                               UFBIN3   UFBINT   UFBINX   UFBMMS
!                               UFBOVR   UFBPOS   UFBQCD   UFBQCP
!                               UFBREP   UFBRMS   UFBSEQ   UFBSTP
!                               UFBTAB   UFBTAM   UFDUMP   UPFTBV
!                               WRCMPS   WRITLC   WRITSA   WRITSB
!                               Also called by application programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      INCLUDE 'bufrlib.prm'

      COMMON /STBFR/ IOLUN(NFILES),IOMSG(NFILES)

      CHARACTER*128 BORT_STR

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      IF(LUNIT.LE.0 .OR. LUNIT.GT.99) GOTO 900

!  CLEAR THE STATUS INDICATORS
!  ---------------------------

      LUN = 0
      IL  = 0
      IM  = 0

!  SEE IF UNIT IS ALREADY CONNECTED TO BUFR ARCHIVE LIBRARY SOFTWARE
!  -----------------------------------------------------------------

      DO I=1,NFILES
      IF(ABS(IOLUN(I)).EQ.LUNIT) LUN = I
      ENDDO

!  IF NOT, TRY TO DEFINE IT SO AS TO CONNECT IT TO BUFR ARCHIVE LIBRARY
!  SOFTWARE
!  --------------------------------------------------------------------

      IF(LUN.EQ.0) THEN
         DO I=1,NFILES
         IF(IOLUN(I).EQ.0) THEN

!  File space is available, return with LUN > 0, IL and IM remain 0
!  ----------------------------------------------------------------

            LUN = I
            GOTO 100
         ENDIF
         ENDDO

!  File space is NOT available, return with LUN, IL and IM all 0
!  -------------------------------------------------------------

         GOTO 100
      ENDIF

!  IF THE UNIT WAS ALREADY CONNECTED TO THE BUFR ARCHIVE LIBRARY
!   SOFTWARE PRIOR TO THIS CALL, RETURN STATUSES
!  -------------------------------------------------------------

      IL = SIGN(1,IOLUN(LUN))
      IM = IOMSG(LUN)

!  EXITS
!  ----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: STATUS - INPUT UNIT NUMBER (",I3,") '// &
       'OUTSIDE LEGAL RANGE OF 1-99")') LUNIT
      CALL BORT(BORT_STR)
      END SUBROUTINE STATUS
