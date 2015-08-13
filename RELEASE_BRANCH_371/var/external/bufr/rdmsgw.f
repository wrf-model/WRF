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
C 2009-03-23  D. KEYSER  -- CALL BORT IN CASE OF MESG OVERFLOW
C 2012-09-15  J. WOOLLEN -- CONVERT TO C LANGUAGE I/O INTERFACE;
C                           USE C ROUTINE CRDBUFR TO OBTAIN BUFR 
C                           MESSAGE; REMOVE CODE WHICH CHECKS SEC0
C                           AND MESSAGE LENGTH AS CRDBUFR DOES THAT
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
C
C   INPUT FILES:
C     UNIT "LUNIT" - BUFR FILE
C
C REMARKS:
C    THIS ROUTINE CALLS:        CRDBUFR  ERRWRT   STATUS
C    THIS ROUTINE IS CALLED BY: COPYBF   CPDXMM   DATEBF   DUMPBF
C                               MESGBC   MESGBF   POSAPX   RDBFDX
C                               READMG   UFBMEM   UFBMEX
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      DIMENSION   MESG(*)

      CHARACTER*128 BORT_STR
      integer crdbufr

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
1     IRET=CRDBUFR(LUN,MESG,MXMSGL)
      IF(IRET.eq.-3)
     +   CALL ERRWRT('BUFRLIB: RDMSGW - SKIPPING OVERLARGE MESSAGE')
      IF(IRET.eq.-2)
     +   CALL ERRWRT('BUFRLIB: RDMSGW - SKIPPING CORRUPTED MESSAGE')
      if(iret.lt.-1) goto 1
      RETURN
      END

