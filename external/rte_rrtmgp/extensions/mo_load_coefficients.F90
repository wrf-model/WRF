! This code is part of RRTM for GCM Applications - Parallel (RRTMGP)
!
! Contacts: Robert Pincus and Eli Mlawer
! email:  rrtmgp@aer.com
!
! Copyright 2015-2018,  Atmospheric and Environmental Research and
! Regents of the University of Colorado.  All right reserved.
!
! Use and duplication is permitted under the terms of the
!    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause
! -------------------------------------------------------------------------------------------------
!
! The gas optics class used by RRMTGP needs to be initialized with data stored in a netCDF file.
!    RRTMGP itself doesn't include methods for reading the data so we don't conflict with users'
!    local environment. This module provides a straight-forward implementation of reading the data
!    and calling gas_optics%load().
!
! -------------------------------------------------------------------------------------------------
module mo_load_coefficients
  !
  ! Modules for working with rte and rrtmgp
  !
  use mo_rte_kind,           only: wp, wl
  use mo_gas_concentrations, only: ty_gas_concs
  use mo_gas_optics_rrtmgp,  only: ty_gas_optics_rrtmgp
  ! --------------------------------------------------
  use mo_simple_netcdf, only: read_field, read_char_vec, read_logical_vec, var_exists, get_dim_size
  use netcdf
  implicit none
  private
  public :: load_and_init

contains
  subroutine stop_on_err(msg)
    use iso_fortran_env, only : error_unit
    character(len=*), intent(in) :: msg


    if(msg /= "") then
      write(error_unit, *) msg
      error stop 1
    end if
  end subroutine
  !--------------------------------------------------------------------------------------------------------------------
  ! read optical coefficients from NetCDF file
  subroutine load_and_init(kdist, filename, available_gases)
    class(ty_gas_optics_rrtmgp), intent(inout) :: kdist
    character(len=*),     intent(in   ) :: filename
    class(ty_gas_concs),  intent(in   ) :: available_gases ! Which gases does the host model have available?
    ! --------------------------------------------------
    !
    ! Variables that will be passed to gas_optics%load()
    !
    character(len=32), dimension(:), allocatable :: gas_names
    integer,  dimension(:,:,:),      allocatable :: key_species
    integer,  dimension(:,:  ),      allocatable :: band2gpt
    real(wp), dimension(:,:  ),      allocatable :: band_lims
    real(wp)                                     :: press_ref_trop, temp_ref_p, temp_ref_t
    real(wp), dimension(:      ),    allocatable :: press_ref
    real(wp), dimension(:      ),    allocatable :: temp_ref
    real(wp), dimension(:,:,:  ),    allocatable :: vmr_ref
    real(wp), dimension(:,:,:,:),    allocatable :: kmajor

    character(len=32), dimension(:),  allocatable :: gas_minor, identifier_minor
    character(len=32), dimension(:),  allocatable :: minor_gases_lower,               minor_gases_upper
    integer, dimension(:,:),          allocatable :: minor_limits_gpt_lower,          minor_limits_gpt_upper
    logical(wl), dimension(:),        allocatable :: minor_scales_with_density_lower, minor_scales_with_density_upper
    character(len=32), dimension(:),  allocatable :: scaling_gas_lower,               scaling_gas_upper
    logical(wl), dimension(:),        allocatable :: scale_by_complement_lower,       scale_by_complement_upper
    integer, dimension(:),            allocatable :: kminor_start_lower,              kminor_start_upper
    real(wp), dimension(:,:,:),       allocatable :: kminor_lower,                    kminor_upper

    real(wp), dimension(:,:,:  ), allocatable :: rayl_lower, rayl_upper
    real(wp), dimension(:      ), allocatable :: solar_quiet, solar_facular, solar_sunspot
    real(wp)                                  :: tsi_default, mg_default, sb_default
    real(wp), dimension(:,:    ), allocatable :: totplnk
    real(wp), dimension(:,:,:,:), allocatable :: planck_frac
    real(wp), dimension(:,:)    , allocatable :: optimal_angle_fit

    ! -----------------
    !
    ! Book-keeping variables
    !
    integer :: ncid
    integer :: ntemps,          &
               npress,          &
               nabsorbers,      &
               nextabsorbers,   &
               nminorabsorbers, &
               nmixingfracs,    &
               nlayers,         &
               nbnds,           &
               ngpts,           &
               npairs,          &
               nminor_absorber_intervals_lower, &
               nminor_absorber_intervals_upper, &
               ncontributors_lower, &
               ncontributors_upper, &
               ninternalSourcetemps, &
               nfit_coeffs
    ! --------------------------------------------------
    !
    ! How big are the various arrays?
    !
    if(nf90_open(trim(fileName), NF90_NOWRITE, ncid) /= NF90_NOERR) &
      call stop_on_err("load_and_init(): can't open file " // trim(fileName))
    ntemps            = get_dim_size(ncid,'temperature')
    npress            = get_dim_size(ncid,'pressure')
    nabsorbers        = get_dim_size(ncid,'absorber')
    nminorabsorbers   = get_dim_size(ncid,'minor_absorber')
    nextabsorbers     = get_dim_size(ncid,'absorber_ext')
    nmixingfracs      = get_dim_size(ncid,'mixing_fraction')
    nlayers           = get_dim_size(ncid,'atmos_layer')
    nbnds             = get_dim_size(ncid,'bnd')
    ngpts             = get_dim_size(ncid,'gpt')
    npairs            = get_dim_size(ncid,'pair')
    nminor_absorber_intervals_lower &
                      = get_dim_size(ncid,'minor_absorber_intervals_lower')
    nminor_absorber_intervals_upper  &
                      = get_dim_size(ncid,'minor_absorber_intervals_upper')
    ninternalSourcetemps &
                      = get_dim_size(ncid,'temperature_Planck')
    ncontributors_lower = get_dim_size(ncid,'contributors_lower')
    ncontributors_upper = get_dim_size(ncid,'contributors_upper')
    nfit_coeffs         = get_dim_size(ncid,'fit_coeffs') ! Will be 0 for SW

    ! -----------------
    !
    ! Read the many arrays
    !
    gas_names         = read_char_vec(ncid, 'gas_names', nabsorbers)
    key_species       = int(read_field(ncid, 'key_species',  2, nlayers, nbnds))
    band_lims         = read_field(ncid, 'bnd_limits_wavenumber', 2, nbnds)
    band2gpt          = int(read_field(ncid, 'bnd_limits_gpt', 2, nbnds))
    press_ref         = read_field(ncid, 'press_ref', npress)
    temp_ref          = read_field(ncid, 'temp_ref',  ntemps)
    temp_ref_p        = read_field(ncid, 'absorption_coefficient_ref_P')
    temp_ref_t        = read_field(ncid, 'absorption_coefficient_ref_T')
    press_ref_trop    = read_field(ncid, 'press_ref_trop')
    kminor_lower      = read_field(ncid, 'kminor_lower', &
        ncontributors_lower, nmixingfracs, ntemps)
    kminor_upper      = read_field(ncid, 'kminor_upper', &
        ncontributors_upper, nmixingfracs, ntemps)
    gas_minor = read_char_vec(ncid, 'gas_minor', nminorabsorbers)
    identifier_minor = read_char_vec(ncid, 'identifier_minor', nminorabsorbers)
    minor_gases_lower = read_char_vec(ncid, 'minor_gases_lower', nminor_absorber_intervals_lower)
    minor_gases_upper = read_char_vec(ncid, 'minor_gases_upper', nminor_absorber_intervals_upper)
    minor_limits_gpt_lower &
                      = int(read_field(ncid, 'minor_limits_gpt_lower', npairs,nminor_absorber_intervals_lower))
    minor_limits_gpt_upper &
                      = int(read_field(ncid, 'minor_limits_gpt_upper', npairs,nminor_absorber_intervals_upper))
    minor_scales_with_density_lower &
                      = read_logical_vec(ncid, 'minor_scales_with_density_lower', nminor_absorber_intervals_lower)
    minor_scales_with_density_upper &
                      = read_logical_vec(ncid, 'minor_scales_with_density_upper', nminor_absorber_intervals_upper)
    scale_by_complement_lower &
                      = read_logical_vec(ncid, 'scale_by_complement_lower', nminor_absorber_intervals_lower)
    scale_by_complement_upper &
                      = read_logical_vec(ncid, 'scale_by_complement_upper', nminor_absorber_intervals_upper)
    scaling_gas_lower &
                      = read_char_vec(ncid, 'scaling_gas_lower', nminor_absorber_intervals_lower)
    scaling_gas_upper &
                      = read_char_vec(ncid, 'scaling_gas_upper', nminor_absorber_intervals_upper)
    kminor_start_lower &
                      = int(read_field(ncid, 'kminor_start_lower', nminor_absorber_intervals_lower))
    kminor_start_upper &
                      = int(read_field(ncid, 'kminor_start_upper', nminor_absorber_intervals_upper))
    vmr_ref           = read_field(ncid, 'vmr_ref', nlayers, nextabsorbers, ntemps)

    kmajor            = read_field(ncid, 'kmajor',  ngpts, nmixingfracs,  npress+1, ntemps)
    if(var_exists(ncid, 'rayl_lower')) then
      rayl_lower = read_field(ncid, 'rayl_lower',   ngpts, nmixingfracs,            ntemps)
      rayl_upper = read_field(ncid, 'rayl_upper',   ngpts, nmixingfracs,            ntemps)
    end if
    ! --------------------------------------------------
    !
    ! Initialize the gas optics class with data. The calls look slightly different depending
    !   on whether the radiation sources are internal to the atmosphere (longwave) or external (shortwave)
    ! gas_optics%load() returns a string; a non-empty string indicates an error.
    !
    if(var_exists(ncid, 'totplnk')) then
      !
      ! If there's a totplnk variable in the file it's a longwave (internal sources) type
      !
      totplnk     = read_field(ncid, 'totplnk', ninternalSourcetemps, nbnds)
      planck_frac = read_field(ncid, 'plank_fraction', ngpts, nmixingfracs, npress+1, ntemps)
      optimal_angle_fit = read_field(ncid, 'optimal_angle_fit', nfit_coeffs, nbnds)
      call stop_on_err(kdist%load(available_gases, &
                                  gas_names,   &
                                  key_species, &
                                  band2gpt,    &
                                  band_lims,   &
                                  press_ref,   &
                                  press_ref_trop, &
                                  temp_ref,    &
                                  temp_ref_p, temp_ref_t,     &
                                  vmr_ref, kmajor,            &
                                  kminor_lower, kminor_upper, &
                                  gas_minor,identifier_minor, &
                                  minor_gases_lower, minor_gases_upper, &
                                  minor_limits_gpt_lower, &
                                  minor_limits_gpt_upper, &
                                  minor_scales_with_density_lower, &
                                  minor_scales_with_density_upper, &
                                  scaling_gas_lower, scaling_gas_upper, &
                                  scale_by_complement_lower, &
                                  scale_by_complement_upper, &
                                  kminor_start_lower, &
                                  kminor_start_upper, &
                                  totplnk, planck_frac,       &
                                  rayl_lower, rayl_upper, &
                                  optimal_angle_fit))
    else
      !
      ! Solar source doesn't have an dependencies yet
      !
      solar_quiet   = read_field(ncid, 'solar_source_quiet', ngpts)
      solar_facular = read_field(ncid, 'solar_source_facular', ngpts)
      solar_sunspot = read_field(ncid, 'solar_source_sunspot', ngpts)
      tsi_default   = read_field(ncid, 'tsi_default')
      mg_default    = read_field(ncid, 'mg_default')
      sb_default    = read_field(ncid, 'sb_default')
      call stop_on_err(kdist%load(available_gases, &
                                  gas_names,   &
                                  key_species, &
                                  band2gpt,    &
                                  band_lims,   &
                                  press_ref,   &
                                  press_ref_trop, &
                                  temp_ref,    &
                                  temp_ref_p, temp_ref_t,     &
                                  vmr_ref, kmajor,            &
                                  kminor_lower, kminor_upper, &
                                  gas_minor,identifier_minor,&
                                  minor_gases_lower, minor_gases_upper, &
                                  minor_limits_gpt_lower, &
                                  minor_limits_gpt_upper, &
                                  minor_scales_with_density_lower, &
                                  minor_scales_with_density_upper, &
                                  scaling_gas_lower, scaling_gas_upper, &
                                  scale_by_complement_lower, &
                                  scale_by_complement_upper, &
                                  kminor_start_lower, &
                                  kminor_start_upper, &
                                  solar_quiet, solar_facular, solar_sunspot, &
                                  tsi_default, mg_default, sb_default, &
                                  rayl_lower, rayl_upper))
    end if
    ! --------------------------------------------------
    ncid = nf90_close(ncid)
  end subroutine load_and_init
end module
