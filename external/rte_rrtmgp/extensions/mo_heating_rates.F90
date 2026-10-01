!
! Eli Mlawer and Robert Pincus
! Andre Wehe and Jennifer Delamere
! email:  rrtmgp@aer.com
!
! Copyright 2016-  Atmospheric and Environmental Research,
!    Regents of the University of Colorado,
!    Trustees of Columbia University in the City of New York
! All right reserved.
!
! Use and duplication is permitted under the terms of the
!    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause
!
! Description:  Heating rate calculation

module mo_heating_rates
  use mo_rte_kind,         only: wp, wl
  use mo_rte_config,       only: check_extents
  use mo_rte_util_array_validation, & 
                           only: extents_are, any_vals_less_than
  use mo_gas_optics_constants, & 
                           only: cp_dry, grav ! Only needed for heating rate calculation
  implicit none
  private
  interface compute_heating_rate
      module procedure compute_heating_rate_general, compute_heating_rate_solar_varmu0
  end interface compute_heating_rate
  public ::  compute_heating_rate
contains
  ! Compute heating rate from fluxes
  ! heating rate H [K/sec] = 1/(rho cp) d f_net/d z
  ! Here use hydrostatic equation for density and heat capacity of dry air
  ! --------------------------------------------------------------------------------------------------
  function compute_heating_rate_general(flux_up, flux_dn, p_lev, heating_rate) result(error_msg)
    real(wp), dimension(:,:), intent(in ) :: flux_up, flux_dn, & !< fluxes at interfaces [W/m2]
                                             p_lev               !< pressure at interfaces [Pa]
    real(wp), dimension(:,:), intent(out) :: heating_rate        !< heating rate within layer [K/sec]
    character(len=128)                    :: error_msg
    ! ---------
    integer :: ncol, nlay, ilay, icol
    ! ---------
    error_msg = ""
    ncol = size(flux_up, 1)
    nlay = size(flux_up, 2) - 1

    if(check_extents) then
      if(.not. extents_are(flux_dn,      ncol, nlay+1)) &
        error_msg = "heating_rate: flux_dn array inconsistently sized."
      if(.not. extents_are(p_lev,        ncol, nlay+1)) &
        error_msg = "heating_rate: p_lev array inconsistently sized."
      if(.not. extents_are(heating_rate, ncol, nlay)) &
        error_msg = "heating_rate: heating_rate array inconsistently sized."
      if(error_msg /= "") return
    end if

    do ilay = 1, nlay
      do icol = 1, ncol
        heating_rate(icol,ilay) = (flux_up(icol,ilay+1) - flux_up(icol,ilay) - &
                                   flux_dn(icol,ilay+1) + flux_dn(icol,ilay)) * &
                                  grav / (cp_dry * (p_lev(icol,ilay+1) - p_lev(icol,ilay)))
      end do
    end do
  end function compute_heating_rate_general
  ! --------------------------------------------------------------------------------------------------
  function compute_heating_rate_solar_varmu0(flux_up, flux_dn, flux_dir, p_lev, mu0, heating_rate) result(error_msg)
    real(wp), dimension(:,:), intent(in ) :: flux_up, flux_dn, flux_dir, & !< fluxes at interfaces [W/m2]
                                             p_lev                !< pressure at interfaces [Pa]
    real(wp), dimension(:,:), intent(in ) :: mu0                 !< solar zenith angle at layer center
    real(wp), dimension(:,:), intent(out) :: heating_rate        !< heating rate within layer [K/sec]
    character(len=128)                    :: error_msg
    ! ---------
    integer :: ncol, nlay, icol, ilay
    integer :: last_sunlight_layer(size(mu0, 1))
    logical(wl) :: top_at_1
    ! ---------
    error_msg = ""
    !
    ! Start with the baseline calculation
    !
    error_msg = compute_heating_rate_general(flux_up, flux_dn, p_lev, heating_rate)
    !
    ! If there was an error, or if the sun is everywhere over the horizon, no need to proceed
    !
    if(error_msg /= "" .or. &
       .not. any_vals_less_than(mu0, epsilon(mu0))) return
    ncol = size(flux_up, 1)
    nlay = size(flux_up, 2) - 1

    if(check_extents) then
      if(.not. extents_are(flux_dir,     ncol, nlay+1)) &
        error_msg = "heating_rate: flux_dir array inconsistently sized."
      if(.not. extents_are(mu0,          ncol, nlay)) &
        error_msg = "heating_rate: mu0 array inconsistently sized."
    end if
    !
    ! In each column: find the layer in which mu0 transitions from non-zero to zero;
    !   compute heating rates in this layer from the divergence of diffuse flux
    !   (total - direct)
    !
    if (any_vals_less_than(mu0(:,nlay), epsilon(mu0))) then
      ! Zero mu0 values in highest index implies top at 1
      last_sunlight_layer = minloc(mu0, mask=mu0>0._wp, dim=2)+1
    else
      last_sunlight_layer = maxloc(mu0, mask=mu0>0._wp, dim=2)-1
    end if
    do icol = 1, ncol
      ilay = last_sunlight_layer(icol)
      if(ilay > 1 .and. last_sunlight_layer(icol) < nlay) then
        !
        ! Heating rate with diffuse (total minus dir) net (up minus down) flux
        !
        heating_rate(icol,ilay) = (flux_up (icol,ilay+1) - flux_up (icol,ilay) - &
                                   flux_dn (icol,ilay+1) + flux_dn (icol,ilay) + &
                                   flux_dir(icol,ilay+1) - flux_dir(icol,ilay)) * &
                                  grav / (cp_dry * (p_lev(icol,ilay+1) - p_lev(icol,ilay)))
      end if
    end do
  end function compute_heating_rate_solar_varmu0
  ! --------------------------------------------------------------------------------------------------
end module mo_heating_rates
