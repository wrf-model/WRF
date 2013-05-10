      SUBROUTINE WTSTAT(LUNIT,LUN,IL,IM)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    WTSTAT
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE EITHER DISCONNECTS THE INPUT LOGICAL UNIT
C   NUMBER LUNIT (AND ITS ASSOCIATED BUFR FILE) FROM THE BUFR ARCHIVE
C   LIBRARY SOFTWARE OR IT CONNECTS IT AS EITHER AN INPUT OR OUPUT FILE
C   AND DEFINES A BUFR MESSAGE AS BEING EITHER OPENED OR CLOSED IN
C   MEMORY FOR THE BUFR FILE IN LUNIT.  THIS INFORMATION IS STORED IN
C   THE INTERNAL ARRAYS IOLUN AND IOMSG IN COMMON BLOCK /STBFR/.
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
C 2003-11-04  J. ATOR    -- CORRECTED A "TYPO" IN TEST FOR VALID VALUE
C                           FOR "IM"; ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C                           ABNORMALLY
C
C USAGE:    CALL WTSTAT (LUNIT, LUN, IL, IM)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C     LUN      - INTEGER: I/O STREAM INDEX ASSOCIATED WITH LOGICAL UNIT
C                LUNIT
C     IL       - INTEGER: LOGICAL UNIT STATUS INDICATOR:
C                       0 = disconnect LUNIT w.r.t. BUFR Archive
C                           Library software (all information
C                           associated with LUNIT is deleted from
C                           within internal arrays)
C                       1 = connect LUNIT as an output file w.r.t. to
C                           BUFR Archive Library software
C                      -1 = connect LUNIT as an input file w.r.t. to 
C                           BUFR Archive Library software
C     IM       - INTEGER: DEFINES WHETHER THERE IS A BUFR MESSAGE
C                CURRENTLY OPEN WITHIN MEMORY FOR THIS LUNIT (IF IT IS
C                CONNECTED, I.E., IL .NE. ZERO):
C                       0 = no
C                       1 = yes
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT
C    THIS ROUTINE IS CALLED BY: CLOSBF   CLOSMG   OPENBF   OPENMB
C                               OPENMG   RDMEMM   READERME REWNBF
C                               READMG   READMM
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /STBFR/ IOLUN(NFILES),IOMSG(NFILES)

      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK ON THE ARGUMENTS
C  ----------------------

      IF(LUNIT.LE.0)            GOTO 900
      IF(LUN  .LE.0)            GOTO 901
      IF(IL.LT.-1 .OR. IL.GT.1) GOTO 902
      IF(IM.LT. 0 .OR. IM.GT.1) GOTO 903

C  CHECK ON LUNIT-LUN COMBINATION
C  ------------------------------

      IF(ABS(IOLUN(LUN)).NE.LUNIT) THEN
         IF(IOLUN(LUN).NE.0) GOTO 905
      ENDIF

C  RESET THE FILE STATUSES
C  -----------------------

      IF(IL.NE.0) THEN
         IOLUN(LUN) = SIGN(LUNIT,IL)
         IOMSG(LUN) = IM
      ELSE
         IOLUN(LUN) = 0
         IOMSG(LUN) = 0
      ENDIF

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - INVALID UNIT NUMBER PASSED '//
     . ' INTO FIRST ARGUMENT (INPUT) (=",I3,")")') LUNIT
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - INVALID I/O STREAM INDEX '//
     . 'PASSED INTO SECOND ARGUMENT (INPUT) (=",I3,")")') LUN
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - INVALID LOGICAL UNIT STATUS'//
     . ' INDICATOR PASSED INTO THIRD ARGUMENT (INPUT) (=",I4,")")') IL
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - INVALID BUFR MESSAGE STATUS'//
     . ' INDICATOR PASSED INTO FOURTH ARGUMENT (INPUT) (=",I4,")")') IM
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - ATTEMPTING TO REDEFINE '//
     . 'EXISTING FILE UNIT (LOGICAL UNIT NUMBER ",I3,")")') IOLUN(LUN)
      CALL BORT(BORT_STR)
      END
