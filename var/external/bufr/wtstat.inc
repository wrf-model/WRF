      SUBROUTINE WTSTAT(LUNIT,LUN,IL,IM)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    WTSTAT
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE EITHER DISCONNECTS THE INPUT LOGICAL UNIT
!   NUMBER LUNIT (AND ITS ASSOCIATED BUFR FILE) FROM THE BUFR ARCHIVE
!   LIBRARY SOFTWARE OR IT CONNECTS IT AS EITHER AN INPUT OR OUPUT FILE
!   AND DEFINES A BUFR MESSAGE AS BEING EITHER OPENED OR CLOSED IN
!   MEMORY FOR THE BUFR FILE IN LUNIT.  THIS INFORMATION IS STORED IN
!   THE INTERNAL ARRAYS IOLUN AND IOMSG IN COMMON BLOCK /STBFR/.
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
! 2003-11-04  J. ATOR    -- CORRECTED A "TYPO" IN TEST FOR VALID VALUE
!                           FOR "IM"; ADDED DOCUMENTATION
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
!                           ABNORMALLY
!
! USAGE:    CALL WTSTAT (LUNIT, LUN, IL, IM)
!   INPUT ARGUMENT LIST:
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
!     LUN      - INTEGER: I/O STREAM INDEX ASSOCIATED WITH LOGICAL UNIT
!                LUNIT
!     IL       - INTEGER: LOGICAL UNIT STATUS INDICATOR:
!                       0 = disconnect LUNIT w.r.t. BUFR Archive
!                           Library software (all information
!                           associated with LUNIT is deleted from
!                           within internal arrays)
!                       1 = connect LUNIT as an output file w.r.t. to
!                           BUFR Archive Library software
!                      -1 = connect LUNIT as an input file w.r.t. to 
!                           BUFR Archive Library software
!     IM       - INTEGER: DEFINES WHETHER THERE IS A BUFR MESSAGE
!                CURRENTLY OPEN WITHIN MEMORY FOR THIS LUNIT (IF IT IS
!                CONNECTED, I.E., IL .NE. ZERO):
!                       0 = no
!                       1 = yes
!
! REMARKS:
!    THIS ROUTINE CALLS:        BORT
!    THIS ROUTINE IS CALLED BY: CLOSBF   CLOSMG   OPENBF   OPENMB
!                               OPENMG   RDMEMM   READERME REWNBF
!                               READMG   READMM
!                               Normally not called by any application
!                               programs.
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

!  CHECK ON THE ARGUMENTS
!  ----------------------

      IF(LUNIT.LE.0)            GOTO 900
      IF(LUN  .LE.0)            GOTO 901
      IF(IL.LT.-1 .OR. IL.GT.1) GOTO 902
      IF(IM.LT. 0 .OR. IM.GT.1) GOTO 903

!  CHECK ON LUNIT-LUN COMBINATION
!  ------------------------------

      IF(ABS(IOLUN(LUN)).NE.LUNIT) THEN
         IF(IOLUN(LUN).NE.0) GOTO 905
      ENDIF

!  RESET THE FILE STATUSES
!  -----------------------

      IF(IL.NE.0) THEN
         IOLUN(LUN) = SIGN(LUNIT,IL)
         IOMSG(LUN) = IM
      ELSE
         IOLUN(LUN) = 0
         IOMSG(LUN) = 0
      ENDIF

!  EXITS
!  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - INVALID UNIT NUMBER PASSED '// &
       ' INTO FIRST ARGUMENT (INPUT) (=",I3,")")') LUNIT
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - INVALID I/O STREAM INDEX '// &
       'PASSED INTO SECOND ARGUMENT (INPUT) (=",I3,")")') LUN
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - INVALID LOGICAL UNIT STATUS'// &
       ' INDICATOR PASSED INTO THIRD ARGUMENT (INPUT) (=",I4,")")') IL
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - INVALID BUFR MESSAGE STATUS'// &
       ' INDICATOR PASSED INTO FOURTH ARGUMENT (INPUT) (=",I4,")")') IM
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - ATTEMPTING TO REDEFINE '// &
       'EXISTING FILE UNIT (LOGICAL UNIT NUMBER ",I3,")")') IOLUN(LUN)
      CALL BORT(BORT_STR)
      END SUBROUTINE WTSTAT
