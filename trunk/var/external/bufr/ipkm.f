      SUBROUTINE IPKM(CBAY,NBYT,N)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    IPKM
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE PACKS AN INTEGER N INTO A CHARACTER STRING
C   CBAY OF LENGTH NBYT BYTES.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  J. WOOLLEN -- BIG-ENDIAN/LITTLE-ENDIAN INDEPENDENT (WAS
C                           IN DECODER VERSION)
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C                           ABNORMALLY
C
C USAGE:    CALL IPKM (CBAY, NBYT, N)
C   INPUT ARGUMENT LIST:
C     NBYT     - INTEGER: NUMBER OF BYTES INTO WHICH TO PACK N (LENGTH
C                OF STRING)
C     N        - INTEGER: INTEGER TO BE PACKED
C
C   OUTPUT ARGUMENT LIST:
C     CBAY     - CHARACTER*8: STRING OF LENGTH NBYT BYTES CONTAINING
C                PACKED INTEGER N 
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     IREV
C    THIS ROUTINE IS CALLED BY: BFRINI   CHRTRNA  CRBMG    PKC
C                               PKTDD    UPC      WRITDX
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      COMMON /HRDWRD/ NBYTW,NBITW,NREV,IORD(8)

      CHARACTER*128 BORT_STR
      CHARACTER*8   CBAY,CINT
      EQUIVALENCE   (CINT,INT)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IF(NBYT.GT.NBYTW) GOTO 900

C     Note that the widths of input variable N and local variable INT
C     will both be equal to the default size of an integer (= NBYTW),
C     since they aren't specifically declared otherwise.

      INT = IREV(ISHFT(N,(NBYTW-NBYT)*8))
      DO I=1,NBYT
      CBAY(I:I) = CINT(I:I)
      ENDDO

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: IPKM - NUMBER OF BYTES BEING PACKED '//
     . ', NBYT (",I4,"), IS > THE INTEGER WORD LENGTH ON THIS '//
     . 'MACHINE, NBYTW (",I3,")")') NBYT,NBYTW
      CALL BORT(BORT_STR)
      END
