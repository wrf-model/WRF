      SUBROUTINE RDMSGB(LUNIT,MESG,IRET)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RDMSGB
C   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
C
C ABSTRACT: THIS SUBROUTINE READS THE NEXT BUFR MESSAGE FROM LOGICAL
C   UNIT LUNIT AS AN ARRAY OF BYTES, WHICH ARE THEN TRANSFERRED TO
C   AN ARRAY OF INTEGER WORDS FOR OUTPUT.
C
C PROGRAM HISTORY LOG:
C 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL RDMSGB (LUNIT, MESG, IRET)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C
C   OUTPUT ARGUMENT LIST:
C     MESG     - *-WORD ARRAY CONTAINING BUFR MESSAGE READ FROM LUNIT
C     IRET     - INTEGER: RETURN CODE:
C                       0 = normal return
C                      -1 = end-of-file encountered while reading
C                           from LUNIT
C                      -2 = I/O error encountered while reading
C                           from LUNIT
C
C   INPUT FILES:
C     UNIT "LUNIT" - BUFR FILE
C
C REMARKS:
C    THIS ROUTINE CALLS:        ICHKSTR  IUPBS01  LMSG
C    THIS ROUTINE IS CALLED BY: READMG
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      DIMENSION   MESG(*)

      CHARACTER*8 SEC0
      CHARACTER*1 CBAY(8*MXMSGLD4)
      DIMENSION   JBAY(MXMSGLD4)

      EQUIVALENCE (CBAY(1),JBAY(1),SEC0)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      SEC0 = ' '
      READ(LUNIT,END=100,ERR=200)
     .     SEC0,(CBAY(I),I=9,IUPBS01(JBAY,'LENM'))

C     Confirm that the first 4 bytes of CBAY contain 'BUFR' encoded in
C     CCITT IA5 (i.e. ASCII).  Note that, since CBAY(1) is equivalenced
C     to SEC0, then CBAY already contains the entire BUFR message!

      IF(ICHKSTR('BUFR',CBAY,4).NE.0) GOTO 200

C     Transfer the message from CBAY into MESG.

      DO I=1,LMSG(SEC0)
        MESG(I) = JBAY(I)
      ENDDO

      IRET = 0
      RETURN

 100  IRET = -1
      RETURN

 200  IRET = -2
      RETURN

      END
