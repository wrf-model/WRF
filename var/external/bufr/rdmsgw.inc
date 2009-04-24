      SUBROUTINE RDMSGW(LUNIT,MESG,IRET)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    RDMSGW
!   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
!
! ABSTRACT: THIS SUBROUTINE READS THE NEXT BUFR MESSAGE FROM LOGICAL
!   UNIT LUNIT AS AN ARRAY OF INTEGER WORDS.
!
! PROGRAM HISTORY LOG:
! 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
!
! USAGE:    CALL RDMSGW (LUNIT, MESG, IRET)
!   INPUT ARGUMENT LIST:
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
!
!   OUTPUT ARGUMENT LIST:
!     MESG     - *-WORD ARRAY CONTAINING BUFR MESSAGE READ FROM LUNIT
!     IRET     - INTEGER: RETURN CODE:
!                       0 = normal return
!                      -1 = end-of-file encountered while reading
!                           from LUNIT
!                      -2 = I/O error encountered while reading
!                           from LUNIT
!
!   INPUT FILES:
!     UNIT "LUNIT" - BUFR FILE
!
! REMARKS:
!    THIS ROUTINE CALLS:        ICHKSTR  LMSG
!    THIS ROUTINE IS CALLED BY: COPYBF   DATEBF   DUMPBF   MESGBC
!                               MESGBF   POSAPN   POSAPX   RDBFDX
!                               READMG   REWNBF   UFBMEM   UFBPOS
!                               Normally not called by any application
!                               programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      COMMON /HRDWRD/ NBYTW,NBITW,NREV,IORD(8)

      DIMENSION   MESG(*)

      CHARACTER*8 SEC0
      CHARACTER*1 CEC0(8)
      DIMENSION   IEC0(2)

      EQUIVALENCE (SEC0,IEC0,CEC0)

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      IMSG = 8/NBYTW+1

      SEC0 = ' '
      READ(LUNIT,END=100,ERR=200) SEC0,(MESG(I),I=IMSG,LMSG(SEC0))

!     Copy SEC0 into the front of MESG so that MESG now contains the
!     entire BUFR message.

      DO I=1,IMSG-1
        MESG(I) = IEC0(I)
      ENDDO

!     Confirm that the first 4 bytes of CEC0 contain 'BUFR' encoded in
!     CCITT IA5 (i.e. ASCII).

      IF(ICHKSTR('BUFR',CEC0,4).NE.0) GOTO 200

      IRET = 0
      RETURN

 100  IRET = -1
      RETURN

 200  IRET = -2
      RETURN

      END SUBROUTINE RDMSGW
