      FUNCTION IFXY(ADSC)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    IFXY
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS FUNCTION RETURNS THE INTEGER CORRESPONDING TO THE
!   BIT-WISE REPRESENTATION OF AN INPUT CHARACTER FXY VALUE OF LENGTH
!   SIX.
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!                           DOCUMENTATION
!
! USAGE:    IFXY (ADSC)
!   INPUT ARGUMENT LIST:
!     ADSC     - CHARACTER*6: CHARACTER FORM OF DESCRIPTOR (FXY VALUE)
!
!   OUTPUT ARGUMENT LIST:
!     IFXY     - INTEGER: BIT-WISE REPRESENTATION OF DESCRIPTOR (FXY)
!                VALUE
!
! REMARKS:
!
!      EXAMPLE:
!
!      If ADSC = '063022', then IFXY = 16150 since:
!
!      0       63           22
!
!      F |     X     |       Y
!        |           |
!     0 0 1 1 1 1 1 1 0 0 0 1 0 1 1 0  =
!
!      ( 2**13 + 2**12 + 2**11 + 2**10 +
!              2**9 + 2**8 + 2**4 + 2**2 + 2**1 )  = 16150
!
!
!    THIS ROUTINE CALLS:        None
!    THIS ROUTINE IS CALLED BY: BFRINI   DXINIT   GETNTBE  IDN30
!                               NEMTAB   NEMTBB   NEMTBD   RDBFDX
!                               RDUSDX   RESTD    SNTBDE   UFBQCP
!                               Normally not called by any application
!                               programs but it could be.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      CHARACTER*6 ADSC

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      READ(ADSC,'(I1,I2,I3)') IF,IX,IY
      IFXY = IF*2**14 + IX*2**8 + IY
      RETURN
      END FUNCTION IFXY
