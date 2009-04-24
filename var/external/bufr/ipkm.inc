      SUBROUTINE IPKM(CBAY,NBYT,N)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    IPKM
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE PACKS AN INTEGER N INTO A CHARACTER STRING
!   CBAY OF LENGTH NBYT BYTES.
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
!                           ROUTINE "BORT"
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
! 2003-11-04  J. WOOLLEN -- BIG-ENDIAN/LITTLE-ENDIAN INDEPENDENT (WAS
!                           IN DECODER VERSION)
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
!                           ABNORMALLY
!
! USAGE:    CALL IPKM (CBAY, NBYT, N)
!   INPUT ARGUMENT LIST:
!     NBYT     - INTEGER: NUMBER OF BYTES INTO WHICH TO PACK N (LENGTH
!                OF STRING)
!     N        - INTEGER: INTEGER TO BE PACKED
!
!   OUTPUT ARGUMENT LIST:
!     CBAY     - CHARACTER*8: STRING OF LENGTH NBYT BYTES CONTAINING
!                PACKED INTEGER N 
!
! REMARKS:
!    THIS ROUTINE CALLS:        BORT     IREV
!    THIS ROUTINE IS CALLED BY: BFRINI   CHRTRNA  CRBMG    PKC
!                               PKTDD    UPC      WRITDX
!                               Normally not called by any application
!                               programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      COMMON /HRDWRD/ NBYTW,NBITW,NREV,IORD(8)

      CHARACTER*128 BORT_STR
      CHARACTER*1   CBAY(8)
      CHARACTER*8   CINT
      EQUIVALENCE   (CINT,INT)

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      IF(NBYT.GT.NBYTW) GOTO 900

!     Note that the widths of input variable N and local variable INT
!     will both be equal to the default size of an integer (= NBYTW),
!     since they aren't specifically declared otherwise.

      INT = IREV(ISHFT(N,(NBYTW-NBYT)*8))
      DO I=1,NBYT
      CBAY(I:I) = CINT(I:I)
      ENDDO

!  EXITS
!  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: IPKM - NUMBER OF BYTES BEING PACKED '// &
       ', NBYT (",I4,"), IS > THE INTEGER WORD LENGTH ON THIS '// &
       'MACHINE, NBYTW (",I3,")")') NBYT,NBYTW
      CALL BORT(BORT_STR)
      END SUBROUTINE IPKM
