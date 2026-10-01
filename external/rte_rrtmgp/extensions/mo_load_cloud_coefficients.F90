module mo_load_cloud_coefficients
  use mo_rte_kind,      only: wp
  use mo_optical_props, only: ty_optical_props,      &
                              ty_optical_props_arry, &
                              ty_optical_props_1scl, &
                              ty_optical_props_2str, &
                              ty_optical_props_nstr
  use mo_cloud_optics_rrtmgp, & 
                        only: ty_cloud_optics_rrtmgp
  use mo_simple_netcdf, only: read_field, read_string, var_exists, get_dim_size, &
                              write_field, create_dim, create_var
  use netcdf

  implicit none
  private
  public :: load_cld_lutcoeff, load_cld_padecoeff
  ! ----------------------------------------------------------------------------------

contains
  !--------------------------------------------------------------------------------------------------------------------
  !
  ! read cloud optical property LUT coefficients from NetCDF file
  !
  subroutine load_cld_lutcoeff(cloud_spec, cld_coeff_file)
    class(ty_cloud_optics_rrtmgp),     intent(inout) :: cloud_spec
    character(len=*),           intent(in   ) :: cld_coeff_file
    ! -----------------
    ! Local variables
    integer :: ncid, nband, nrghice, nsize_liq, nsize_ice

    real(wp), dimension(:,:), allocatable                :: band_lims_wvn
    ! Lookup table interpolation constants
    real(wp) :: radliq_lwr          ! liquid particle size lower bound for interpolation
    real(wp) :: radliq_upr          ! liquid particle size upper bound for interpolation
    real(wp) :: radliq_fac          ! constant for calculating interpolation indices for liquid
    real(wp) :: radice_lwr          ! ice particle size lower bound for interpolation
    real(wp) :: radice_upr          ! ice particle size upper bound for interpolation
    real(wp) :: radice_fac          ! constant for calculating interpolation indices for ice
    ! LUT coefficients
    real(wp), dimension(:,:),   allocatable :: lut_extliq   ! extinction: liquid
    real(wp), dimension(:,:),   allocatable :: lut_ssaliq   ! single scattering albedo: liquid
    real(wp), dimension(:,:),   allocatable :: lut_asyliq   ! asymmetry parameter: liquid
    real(wp), dimension(:,:,:), allocatable :: lut_extice   ! extinction: ice
    real(wp), dimension(:,:,:), allocatable :: lut_ssaice   ! single scattering albedo: ice
    real(wp), dimension(:,:,:), allocatable :: lut_asyice   ! asymmetry parameter: ice
    ! -----------------
    ! Open cloud optical property coefficient file
    if(nf90_open(trim(cld_coeff_file), NF90_NOWRITE, ncid) /= NF90_NOERR) &
       call stop_on_err("load_cld_lutcoeff(): can't open file " // trim(cld_coeff_file))

    ! Read LUT coefficient dimensions
    nband     = get_dim_size(ncid,'nband')
    nrghice   = get_dim_size(ncid,'nrghice')
    nsize_liq = get_dim_size(ncid,'nsize_liq')
    nsize_ice = get_dim_size(ncid,'nsize_ice')

    allocate(band_lims_wvn(2, nband))
    band_lims_wvn = read_field(ncid, 'bnd_limits_wavenumber', 2, nband)

    ! Read LUT constants
    radliq_lwr = read_field(ncid, 'radliq_lwr')
    radliq_upr = read_field(ncid, 'radliq_upr')
    radice_lwr = read_field(ncid, 'diamice_lwr')
    radice_upr = read_field(ncid, 'diamice_upr')

    ! Allocate cloud property lookup table input arrays
    allocate(lut_extliq(nsize_liq, nband), &
             lut_ssaliq(nsize_liq, nband), &
             lut_asyliq(nsize_liq, nband), &
             lut_extice(nsize_ice, nband, nrghice), &
             lut_ssaice(nsize_ice, nband, nrghice), &
             lut_asyice(nsize_ice, nband, nrghice))

    ! Read LUT coefficients
    lut_extliq = read_field(ncid, 'extliq',  nsize_liq, nband)
    lut_ssaliq = read_field(ncid, 'ssaliq',  nsize_liq, nband)
    lut_asyliq = read_field(ncid, 'asyliq',  nsize_liq, nband)
    lut_extice = read_field(ncid, 'extice',  nsize_ice, nband, nrghice)
    lut_ssaice = read_field(ncid, 'ssaice',  nsize_ice, nband, nrghice)
    lut_asyice = read_field(ncid, 'asyice',  nsize_ice, nband, nrghice)

    ncid = nf90_close(ncid)
    call stop_on_err(cloud_spec%load(band_lims_wvn,                      &
                                     radliq_lwr, radliq_upr,             &
                                     radice_lwr, radice_upr,             &
                                     lut_extliq, lut_ssaliq, lut_asyliq, &
                                     lut_extice, lut_ssaice, lut_asyice))
  end subroutine load_cld_lutcoeff
  !--------------------------------------------------------------------------------------------------------------------
  ! read cloud optical property Pade coefficients from NetCDF file
  !
  subroutine load_cld_padecoeff(cloud_spec, cld_coeff_file)
    class(ty_cloud_optics_rrtmgp),       intent(inout) :: cloud_spec
    character(len=*),             intent(in   ) :: cld_coeff_file
    ! -----------------
    ! Local variables
    integer :: ncid, nband, nrghice, nsizereg, ncoeff_ext, ncoeff_ssa_g, nbound

    ! Spectral discretization
    real(wp), dimension(:,:), allocatable :: band_lims_wvn

    ! Pade coefficients
    real(wp), dimension(:,:,:),   allocatable :: pade_extliq   ! extinction: liquid
    real(wp), dimension(:,:,:),   allocatable :: pade_ssaliq   ! single scattering albedo: liquid
    real(wp), dimension(:,:,:),   allocatable :: pade_asyliq   ! asymmetry parameter: liquid
    real(wp), dimension(:,:,:,:), allocatable :: pade_extice   ! extinction: ice
    real(wp), dimension(:,:,:,:), allocatable :: pade_ssaice   ! single scattering albedo: ice
    real(wp), dimension(:,:,:,:), allocatable :: pade_asyice   ! asymmetry parameter: ice

    ! Pade particle size regime boundaries
    real(wp),  dimension(:),       allocatable :: pade_sizreg_extliq
    real(wp),  dimension(:),       allocatable :: pade_sizreg_ssaliq
    real(wp),  dimension(:),       allocatable :: pade_sizreg_asyliq
    real(wp),  dimension(:),       allocatable :: pade_sizreg_extice
    real(wp),  dimension(:),       allocatable :: pade_sizreg_ssaice
    real(wp),  dimension(:),       allocatable :: pade_sizreg_asyice
    ! -----------------
    ! Open cloud optical property coefficient file
    if(nf90_open(trim(cld_coeff_file), NF90_NOWRITE, ncid) /= NF90_NOERR) &
       call stop_on_err("load_cld_padecoeff(): can't open file " // trim(cld_coeff_file))

    ! Read Pade coefficient dimensions
    nband        = get_dim_size(ncid,'nband')
    nrghice      = get_dim_size(ncid,'nrghice')
    nsizereg     = get_dim_size(ncid,'nsizereg')
    ncoeff_ext   = get_dim_size(ncid,'ncoeff_ext')
    ncoeff_ssa_g = get_dim_size(ncid,'ncoeff_ssa_g')
    nbound       = get_dim_size(ncid,'nbound')

    !
    allocate(band_lims_wvn(2, nband))
    band_lims_wvn = read_field(ncid, 'bnd_limits_wavenumber', 2, nband)

    ! Allocate cloud property Pade coefficient input arrays
    allocate(pade_extliq(nband, nsizereg, ncoeff_ext),   &
             pade_ssaliq(nband, nsizereg, ncoeff_ssa_g), &
             pade_asyliq(nband, nsizereg, ncoeff_ssa_g), &
             pade_extice(nband, nsizereg, ncoeff_ext,   nrghice), &
             pade_ssaice(nband, nsizereg, ncoeff_ssa_g, nrghice), &
             pade_asyice(nband, nsizereg, ncoeff_ssa_g, nrghice))

    pade_extliq  = read_field(ncid, 'pade_extliq', nband, nsizereg, ncoeff_ext)
    pade_ssaliq  = read_field(ncid, 'pade_ssaliq', nband, nsizereg, ncoeff_ssa_g)
    pade_asyliq  = read_field(ncid, 'pade_asyliq', nband, nsizereg, ncoeff_ssa_g)
    pade_extice  = read_field(ncid, 'pade_extice', nband, nsizereg, ncoeff_ext, nrghice)
    pade_ssaice  = read_field(ncid, 'pade_ssaice', nband, nsizereg, ncoeff_ssa_g, nrghice)
    pade_asyice  = read_field(ncid, 'pade_asyice', nband, nsizereg, ncoeff_ssa_g, nrghice)

    ! Allocate cloud property Pade coefficient particle size boundary input arrays
    allocate(pade_sizreg_extliq(nbound), &
             pade_sizreg_ssaliq(nbound), &
             pade_sizreg_asyliq(nbound), &
             pade_sizreg_extice(nbound), &
             pade_sizreg_ssaice(nbound), &
             pade_sizreg_asyice(nbound))

    pade_sizreg_extliq = read_field(ncid, 'pade_sizreg_extliq', nbound)
    pade_sizreg_ssaliq = read_field(ncid, 'pade_sizreg_ssaliq', nbound)
    pade_sizreg_asyliq = read_field(ncid, 'pade_sizreg_asyliq', nbound)
    pade_sizreg_extice = read_field(ncid, 'pade_sizreg_extice', nbound)
    pade_sizreg_ssaice = read_field(ncid, 'pade_sizreg_ssaice', nbound)
    pade_sizreg_asyice = read_field(ncid, 'pade_sizreg_asyice', nbound)

    ncid = nf90_close(ncid)

    call stop_on_err(cloud_spec%load(band_lims_wvn, &
                                     pade_extliq, pade_ssaliq, pade_asyliq, &
                                     pade_extice, pade_ssaice, pade_asyice, &
                                     pade_sizreg_extliq, pade_sizreg_ssaliq, pade_sizreg_asyliq, &
                                     pade_sizreg_extice, pade_sizreg_ssaice, pade_sizreg_asyice))
  end subroutine load_cld_padecoeff

  ! -----------------------------------------------------------------------------------
    subroutine stop_on_err(msg)
      !
      ! Print error message and stop
      !
      use iso_fortran_env, only : error_unit
      character(len=*), intent(in) :: msg
      if(len_trim(msg) > 0) then
        write (error_unit,*) trim(msg)
        error stop 1
      end if
    end subroutine

end module
