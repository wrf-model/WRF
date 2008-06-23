  subroutine init_constants_derived
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_constants_derived          set derived constants
!     prgmmr:    treadon          org: np23           date: 2004-12-02
!
! abstract:  This routine sets derived constants
!
! program history log:
!   2004-12-02  treadon
!   2005-03-03  treadon - add implicit none
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

!   Trigonometric constants
    pi      = acos(-one)
    deg2rad = pi/180.0_r_kind
    rad2deg = one/deg2rad
    tiny_r_kind = tiny(zero)
    huge_r_kind = huge(zero)
    tiny_single = tiny(zero_single)
    huge_single = huge(zero_single)

!   Geophysical parameters used in conversion of geopotential to
!   geometric height
    eccentricity_linear = sqrt(semi_major_axis**2 - semi_minor_axis**2)
    eccentricity = eccentricity_linear / semi_major_axis

    return
  end subroutine init_constants_derived
