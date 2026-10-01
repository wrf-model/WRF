! This code is part of RRTM for GCM Applications - Parallel (RRTMGP)
!
! Contacts: Robert Pincus and Eli Mlawer
! email:  rrtmgp@aer.com
!
! Copyright 2020-  Atmospheric and Environmental Research,
!    Regents of the University of Colorado,
!    Trustees of Columbia University in the City of New York
! All right reserved.
!
! Use and duplication is permitted under the terms of the
!    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause

!> -------------------------------------------------------------------------------------------------
!>
!> ## Control input sanitization in Fortan front-end
!>   Provides public access to two proteced module variables
!>
!> -------------------------------------------------------------------------------------------------
module mo_rte_config
  use mo_rte_kind, only: wl
  implicit none
  private

  logical(wl), protected, public :: check_extents = .true.
  logical(wl), protected, public :: check_values  = .true.

  !> Specify checking of extents and values individually, or all checks together
  interface rte_config_checks
    module procedure rte_config_checks_each, rte_config_checks_all
  end interface
  public :: rte_config_checks
contains
  ! --------------------------------------------------------------
  !> Do extents and/or values checks within RTE+RRTMGP Fortran classes
  subroutine rte_config_checks_each(extents, values)
    logical(wl), intent(in) :: extents, values

    check_extents = extents
    check_values  = values
  end subroutine rte_config_checks_each
  ! --------------------------------------------------------------
  !> Do all checks within RTE+RRTMGP Fortran classes
  subroutine rte_config_checks_all(do_checks)
    logical(wl), intent(in) :: do_checks

    check_extents = do_checks
    check_values  = do_checks
  end subroutine rte_config_checks_all
  ! --------------------------------------------------------------
end module mo_rte_config
