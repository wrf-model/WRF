! This code is part of Radiative Transfer for Energetics (RTE)
!
! Contacts: Robert Pincus and Eli Mlawer
! email:  rrtmgp@aer.com
!
! Copyright 2015-  Atmospheric and Environmental Research,
!    Regents of the University of Colorado,
!    Trustees of Columbia University in the City of New York
! All right reserved.
!
! Use and duplication is permitted under the terms of the
!    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause
! -------------------------------------------------------------------------------------------------
module mo_rte_util_array
  use mo_rte_kind,      only: wp, wl
  implicit none
  public :: zero_array

  !-------------------------------------------------------------------------------------------------
  ! Initializing arrays to 0
  !-------------------------------------------------------------------------------------------------
  interface zero_array
    subroutine zero_array_1D(ni, array) bind(C, name="zero_array_1D")
      use mo_rte_kind,      only: wp, wl
      integer,                 intent(in ) :: ni
      real(wp), dimension(ni), intent(out) :: array
    end subroutine zero_array_1D
    ! ----------------------------------------------------------
    subroutine zero_array_2D(ni, nj, array) bind(C, name="zero_array_2D")
      use mo_rte_kind,      only: wp, wl
      integer,                     intent(in ) :: ni, nj
      real(wp), dimension(ni, nj), intent(out) :: array
    end subroutine zero_array_2D
    ! ----------------------------------------------------------
    subroutine zero_array_3D(ni, nj, nk, array) bind(C, name="zero_array_3D")
      use mo_rte_kind,      only: wp, wl
      integer,                         intent(in ) :: ni, nj, nk
      real(wp), dimension(ni, nj, nk), intent(out) :: array
    end subroutine zero_array_3D
    ! ----------------------------------------------------------
    subroutine zero_array_4D(ni, nj, nk, nl, array) bind(C, name="zero_array_4D")
    use mo_rte_kind,      only: wp, wl
    integer,                             intent(in ) :: ni, nj, nk, nl
      real(wp), dimension(ni, nj, nk, nl), intent(out) :: array
    end subroutine zero_array_4D
  end interface zero_array
end module mo_rte_util_array
