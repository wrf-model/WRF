      SUBROUTINE POSAPX(LUNIT)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    POSAPX
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE READS TO THE END OF THE FILE AND BACKSPACES
!   IN ORDER TO POSITION FOR APPEND.
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
!                           ROUTINE "BORT"
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
!                           10,000 TO 20,000 BYTES
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
!                           TERMINATES ABNORMALLY
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
!                           20,000 TO 50,000 BYTES
!
! USAGE:    CALL POSAPX (LUNIT)
!   INPUT ARGUMENT LIST:
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
!
!   INPUT FILES:
!     UNIT "LUNIT" - BUFR FILE
!
! REMARKS:
!    THIS ROUTINE CALLS:        BORT     RDMSGW
!    THIS ROUTINE IS CALLED BY: OPENBF
!                               Normally not called by any application
!                               programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      INCLUDE 'bufrlib.prm'

      DIMENSION   MBAY(MXMSGLD4)

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      REWIND LUNIT
      IREC = 0

!  TRY TO READ TO THE END OF THE FILE
!  ----------------------------------

1     CALL RDMSGW(LUNIT,MBAY,IER)
      IF(IER.EQ.-1) GOTO 2
      IF(IER.EQ.-2) GOTO 3
      IREC = IREC+1
      GOTO 1

!  IF SUCCESSFUL, BACKSPACE FOR APPENDING AND RETURN
!  -------------------------------------------------

2     BACKSPACE LUNIT
      GOTO 100

!  IF AN I/O ERROR IS ENCOUNTERED, THEN REREAD THE GOOD RECORDS,
!  BACKSPACE FOR APPENDING AND RETURN
!  -----------------------------------------------------------------

3     REWIND LUNIT
      DO J=1,IREC
        CALL RDMSGW(LUNIT,MBAY,IER)
        IF(IER.EQ.-1) GOTO 2
        IF(IER.EQ.-2) GOTO 900
      ENDDO

!  EXITS
!  -----

100   RETURN
900   CALL BORT('BUFRLIB: POSAPX - ERROR READING A BUFR MESSAGE')
      END SUBROUTINE POSAPX
