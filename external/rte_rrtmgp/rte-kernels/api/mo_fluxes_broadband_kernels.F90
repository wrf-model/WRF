! This code is part of Radiative Transfer for Energetics (RTE)
!
! Contacts: Robert Pincus and Eli Mlawer
! email:  rrtmgp@aer.com
!
! Copyright 2015-,  Atmospheric and Environmental Research,
! Regents of the University of Colorado, Trustees of Columbia University.  All right reserved.
!
! Use and duplication is permitted under the terms of the
!    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause
! -------------------------------------------------------------------------------------------------
!>
!> ## Kernels for computing broadband fluxes
!>
! -------------------------------------------------------------------------------------------------
module mo_fluxes_broadband_kernels
  use, intrinsic :: iso_c_binding
  use mo_rte_kind, only: wp
  implicit none
  private
  public :: sum_broadband, net_broadband

  ! ----------------------------------------------------------------------------
  !>
  !> Spectral reduction over all points
  !>
  interface
    subroutine sum_broadband(ncol, nlev, ngpt, spectral_flux, broadband_flux) bind(C, name="rte_sum_broadband")
      use mo_rte_kind, only: wp
      integer,                               intent(in ) :: ncol, nlev, ngpt
        !! Array sizes
      real(wp), dimension(ncol, nlev, ngpt), intent(in ) :: spectral_flux
        !! Spectrally-resolved flux
      real(wp), dimension(ncol, nlev),       intent(out) :: broadband_flux
        !! Sum of spectrally-resolved flux over `ngpt`
    end subroutine sum_broadband
  end interface
  ! ----------------------------------------------------------------------------
  !>
  !> Spectral reduction over all points for net flux
  !>   Overloaded - which routine is called depends on arguments 
  !> 
  interface net_broadband
    ! ----------------------------------------------------------------------------
    !>
    !> Net flux from g-point fluxes up and down
    !>
    subroutine net_broadband_full(ncol, nlev, ngpt, spectral_flux_dn, spectral_flux_up, broadband_flux_net) &
      bind(C, name="rte_net_broadband_full")
      use mo_rte_kind, only: wp
      integer,                               intent(in ) :: ncol, nlev, ngpt
        !! Array sizes
      real(wp), dimension(ncol, nlev, ngpt), intent(in ) :: spectral_flux_dn, spectral_flux_up
        !! Spectrally-resolved flux up and down
      real(wp), dimension(ncol, nlev),       intent(out) :: broadband_flux_net
        !! Net (down minus up) summed over `ngpt`
     end subroutine net_broadband_full
    ! ----------------------------------------------------------------------------
    !>
    !> Net flux when bradband flux up and down are already available
    !>
    subroutine net_broadband_precalc(ncol, nlev, flux_dn, flux_up, broadband_flux_net) &
      bind(C, name="rte_net_broadband_precalc")
      use mo_rte_kind, only: wp
      integer,                         intent(in ) :: ncol, nlev
        !! Array sizes
      real(wp), dimension(ncol, nlev), intent(in ) :: flux_dn, flux_up
        !! Broadband downward and upward fluxes
      real(wp), dimension(ncol, nlev), intent(out) :: broadband_flux_net
        !! Net (down minus up)
     end subroutine net_broadband_precalc
  end interface net_broadband
  ! ----------------------------------------------------------------------------
end module mo_fluxes_broadband_kernels
