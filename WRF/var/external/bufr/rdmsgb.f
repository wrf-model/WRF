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
C 2009-03-23  D. KEYSER  -- CALLS BORT IN CASE OF MESG OVERFLOW
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
C    THIS ROUTINE CALLS:        BORT     ICHKSTR  IUPBS01  LMSG
C    THIS ROUTINE IS CALLED BY: None
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
      CHARACTER*8 SEC0
      CHARACTER*1 CBAY(8*MXMSGLD4)
      DIMENSION   JBAY(MXMSGLD4)

      EQUIVALENCE (CBAY(1),JBAY(1),SEC0)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      SEC0 = ' '

C     Read Section 0 from the next message in the file.

      READ(LUNIT,END=100,ERR=200) SEC0

C     Confirm that the first 4 bytes contain 'BUFR' encoded in
C     CCITT IA5 (i.e. ASCII).

      IF(ICHKSTR('BUFR',CBAY,4).NE.0) GOTO 200
      
C     Check the length of the next message to make sure it will fit
C     within the output array.

      LNMSG = LMSG(SEC0)
      IF(LNMSG*NBYTW.GT.MXMSGL) GOTO 900

C     Read the rest of the message as an array of bytes.

      READ(LUNIT,END=100,ERR=200) (CBAY(I),I=9,IUPBS01(JBAY,'LENM'))

C     Transfer the message to the output array.

      DO I=1,LNMSG
        MESG(I) = JBAY(I)
      ENDDO

C  EXITS
C  -----

      IRET = 0
      RETURN

100   IRET = -1
      RETURN

200   IRET = -2
      RETURN

900   WRITE(BORT_STR,'("BUFRLIB: RDMSGB - INPUT BUFR MESSAGE LENGTH (",
     . I6," BYTES) IS LARGER THAN LIMIT OF ",I6," BYTES")')
     . LNMSG*NBYTW,MXMSGL
      CALL BORT(BORT_STR)
      END
