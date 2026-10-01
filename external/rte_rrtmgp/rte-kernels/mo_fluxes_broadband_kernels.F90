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

  interface net_broadband
    !! Interface for computing net flux
    module procedure net_broadband_full, net_broadband_precalc
  end interface net_broadband
contains
  ! ----------------------------------------------------------------------------
  !>
  !> Spectral reduction over all points
  !>
  subroutine sum_broadband(ncol, nlev, ngpt, spectral_flux, broadband_flux) bind(C, name="rte_sum_broadband")
    integer,                               intent(in ) :: ncol, nlev, ngpt
      !! Array sizes
    real(wp), dimension(ncol, nlev, ngpt), intent(in ) :: spectral_flux
      !! Spectrally-resolved flux
    real(wp), dimension(ncol, nlev),       intent(out) :: broadband_flux
      !! Sum of spectrally-resolved flux over `ngpt`

    integer  :: icol, ilev, igpt
    real(wp) :: bb_flux_s ! local scalar version

    !$acc enter data copyin(spectral_flux) create(broadband_flux)
    !$omp target enter data map(to:spectral_flux) map(alloc:broadband_flux)
    !$acc parallel loop gang vector collapse(2)
    !$omp target teams distribute parallel do simd collapse(2)
    do ilev = 1, nlev
      do icol = 1, ncol

        bb_flux_s = 0.0_wp

        do igpt = 1, ngpt
          bb_flux_s = bb_flux_s + spectral_flux(icol, ilev, igpt)
        end do

        broadband_flux(icol, ilev) = bb_flux_s
      end do
    end do
    !$acc exit data delete(spectral_flux) copyout(broadband_flux)
    !$omp target exit data map(release:spectral_flux) map(from:broadband_flux)
  end subroutine sum_broadband
  ! ----------------------------------------------------------------------------
  !>
  !> Spectral reduction over all points for net flux
  !>
  subroutine net_broadband_full(ncol, nlev, ngpt, spectral_flux_dn, spectral_flux_up, broadband_flux_net) &
    bind(C, name="rte_net_broadband_full")
    integer,                               intent(in ) :: ncol, nlev, ngpt
      !! Array sizes
    real(wp), dimension(ncol, nlev, ngpt), intent(in ) :: spectral_flux_dn, spectral_flux_up
      !! Spectrally-resolved flux up and down
    real(wp), dimension(ncol, nlev),       intent(out) :: broadband_flux_net
      !! Net (down minus up) summed over `ngpt`

    integer  :: icol, ilev, igpt
    real(wp) :: diff

    !$acc enter data copyin(spectral_flux_dn, spectral_flux_up) create(broadband_flux_net)
    !$omp target enter data map(to:spectral_flux_dn, spectral_flux_up) map(alloc:broadband_flux_net)
    !$acc parallel loop collapse(2)
    !$omp target teams distribute parallel do simd collapse(2)
    do ilev = 1, nlev
      do icol = 1, ncol
        diff = spectral_flux_dn(icol, ilev, 1   ) - spectral_flux_up(icol, ilev,     1)
        broadband_flux_net(icol, ilev) = diff
      end do
    end do
    !$acc parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3)
    do igpt = 2, ngpt
      do ilev = 1, nlev
        do icol = 1, ncol
          diff = spectral_flux_dn(icol, ilev, igpt) - spectral_flux_up(icol, ilev, igpt)
          !$acc atomic update
          !$omp atomic update
          broadband_flux_net(icol, ilev) = broadband_flux_net(icol, ilev) + diff
        end do
      end do
    end do
    !$acc exit data delete(spectral_flux_dn, spectral_flux_up) copyout(broadband_flux_net)
    !$omp target exit data map(release:spectral_flux_dn, spectral_flux_up) map(from:broadband_flux_net)
  end subroutine net_broadband_full
  ! ----------------------------------------------------------------------------
  !>
  !> Net flux when bradband flux up and down are already available
  !>
  subroutine net_broadband_precalc(ncol, nlev, flux_dn, flux_up, broadband_flux_net) &
    bind(C, name="rte_net_broadband_precalc")
    integer,                         intent(in ) :: ncol, nlev
      !! Array sizes
    real(wp), dimension(ncol, nlev), intent(in ) :: flux_dn, flux_up
      !! Broadband downward and upward fluxes
    real(wp), dimension(ncol, nlev), intent(out) :: broadband_flux_net
      !! Net (down minus up)

    integer  :: icol, ilev
    !$acc enter data copyin(flux_dn, flux_up) create(broadband_flux_net)
    !$omp target enter data map(to:flux_dn, flux_up) map(alloc:broadband_flux_net)
    !$acc parallel loop collapse(2)
    !$omp target teams distribute parallel do simd collapse(2)
    do ilev = 1, nlev
      do icol = 1, ncol
         broadband_flux_net(icol,ilev) = flux_dn(icol,ilev) - flux_up(icol,ilev)
       end do
    end do
    !$acc exit data delete(flux_dn, flux_up) copyout(broadband_flux_net)
    !$omp target exit data map(release:flux_dn, flux_up) map(from:broadband_flux_net)
  end subroutine net_broadband_precalc
  ! ----------------------------------------------------------------------------
end module mo_fluxes_broadband_kernels
