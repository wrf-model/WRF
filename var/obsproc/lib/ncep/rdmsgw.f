      SUBROUTINE RDMSGW(LUNIT,MESG,IRET)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RDMSGW
C   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
C
C ABSTRACT: THIS SUBROUTINE READS THE NEXT BUFR MESSAGE FROM LOGICAL
C   UNIT LUNIT AS AN ARRAY OF INTEGER WORDS.
C
C PROGRAM HISTORY LOG:
C 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL RDMSGW (LUNIT, MESG, IRET)
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
C    THIS ROUTINE CALLS:        ICHKSTR  LMSG
C    THIS ROUTINE IS CALLED BY: COPYBF   DATEBF   DUMPBF   MESGBC
C                               MESGBF   POSAPN   POSAPX   RDBFDX
C                               READMG   REWNBF   UFBMEM   UFBPOS
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      COMMON /HRDWRD/ NBYTW,NBITW,NREV,IORD(8)

      DIMENSION   MESG(*)

      CHARACTER*8 SEC0
      CHARACTER*1 CEC0(8)
      DIMENSION   IEC0(2)

      EQUIVALENCE (SEC0,IEC0,CEC0)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IMSG = 8/NBYTW+1

      SEC0 = ' '
      READ(LUNIT,END=100,ERR=200) SEC0,(MESG(I),I=IMSG,LMSG(SEC0))

C     Copy SEC0 into the front of MESG so that MESG now contains the
C     entire BUFR message.

      DO I=1,IMSG-1
        MESG(I) = IEC0(I)
      ENDDO

C     Confirm that the first 4 bytes of CEC0 contain 'BUFR' encoded in
C     CCITT IA5 (i.e. ASCII).

      IF(ICHKSTR('BUFR',CEC0,4).NE.0) GOTO 200

      IRET = 0
      RETURN

 100  IRET = -1
      RETURN

 200  IRET = -2
      RETURN

      END
