module mo_gas_optics_rrtmgp_kernels
  use mo_rte_kind,      only : wp, wl
  use mo_rte_util_array,only : zero_array
  implicit none
  private
  public :: interpolation, compute_tau_absorption, compute_tau_rayleigh, compute_Planck_source
  ! ------------------------------------------------------------------------------------------------------------------
  interface 
    subroutine interpolation( &
                ncol,nlay,ngas,nflav,neta, npres, ntemp, &
                flavor,                                  &
                press_ref_log, temp_ref,press_ref_log_delta,    &
                temp_ref_min,temp_ref_delta,press_ref_trop_log, &
                vmr_ref,                                        &
                play,tlay,col_gas,                              &
                jtemp,fmajor,fminor,col_mix,tropo,jeta,jpress) bind(C, name="rrtmgp_interpolation")
      use mo_rte_kind,      only : wp, wl
      ! input dimensions
      integer,                            intent(in) :: ncol,nlay
        !! physical domain size
      integer,                            intent(in) :: ngas,nflav,neta,npres,ntemp
        !! k-distribution table dimensions 
      integer,     dimension(2,nflav),    intent(in) :: flavor
        !! index into vmr_ref of major gases for each flavor
      real(wp),    dimension(npres),      intent(in) :: press_ref_log
        !! log of pressure dimension in RRTMGP tables 
      real(wp),    dimension(ntemp),      intent(in) :: temp_ref
        !! temperature dimension in RRTMGP tables 
      real(wp),                           intent(in) :: press_ref_log_delta, &
                                                        temp_ref_min, temp_ref_delta, &
                                                        press_ref_trop_log
        !! constants related to RRTMGP tables
      real(wp),    dimension(2,0:ngas,ntemp), intent(in) :: vmr_ref
        !! reference volume mixing ratios used in compute "binary species parameter" eta

      ! inputs from profile or parent function
      real(wp),    dimension(ncol,nlay),        intent(in) :: play, tlay
        !! input pressure (Pa?) and temperature (K)
      real(wp),    dimension(ncol,nlay,0:ngas), intent(in) :: col_gas
        !! input column gas amount - molecules/cm^2 
      ! outputs
      integer,     dimension(ncol,nlay), intent(out) :: jtemp, jpress
        !! temperature and pressure interpolation indexes 
      logical(wl), dimension(ncol,nlay), intent(out) :: tropo
        !! use lower (or upper) atmosphere tables 
      integer,     dimension(2,    ncol,nlay,nflav), intent(out) :: jeta
        !! Index for binary species interpolation 
#if !defined(__INTEL_LLVM_COMPILER) && __INTEL_COMPILER >= 2021
    ! A performance-hitting workaround for the vectorization problem reported in
    ! https://github.com/earth-system-radiation/rte-rrtmgp/issues/159
    ! The known affected compilers are Intel Fortran Compiler Classic
    ! 2021.4, 2021.5 and 2022.1. We do not limit the workaround to these
    ! versions because it is not clear when the compiler bug will be fixed, see
    ! https://community.intel.com/t5/Intel-Fortran-Compiler/Compiler-vectorization-bug/m-p/1362591.
    ! We, however, limit the workaround to the Classic versions only since the
    ! problem is not confirmed for the Intel Fortran Compiler oneAPI (a.k.a
    ! 'ifx'), which does not mean there is none though.
    real(wp),    dimension(:,       :,   :,    :), intent(out) :: col_mix
#else
      real(wp),    dimension(2,    ncol,nlay,nflav), intent(out) :: col_mix
        !! combination of major species's column amounts (first index is strat/trop)
#endif
      real(wp),    dimension(2,2,2,ncol,nlay,nflav), intent(out) :: fmajor
        !! Interpolation weights in pressure, eta, strat/trop 
      real(wp),    dimension(2,2,  ncol,nlay,nflav), intent(out) :: fminor
        !! Interpolation fraction in eta, strat/trop 
    end subroutine interpolation
  end interface 
  ! ------------------------------------------------------------------------------------------------------------------
  interface
    subroutine compute_tau_absorption(                &
                  ncol,nlay,nbnd,ngpt,                &  ! dimensions
                  ngas,nflav,neta,npres,ntemp,        &
                  nminorlower, nminorklower,          & ! number of minor contributors, total num absorption coeffs
                  nminorupper, nminorkupper,          &
                  idx_h2o,                            &
                  gpoint_flavor,                      &
                  band_lims_gpt,                      &
                  kmajor,                             &
                  kminor_lower,                       &
                  kminor_upper,                       &
                  minor_limits_gpt_lower,             &
                  minor_limits_gpt_upper,             &
                  minor_scales_with_density_lower,    &
                  minor_scales_with_density_upper,    &
                  scale_by_complement_lower,          &
                  scale_by_complement_upper,          &
                  idx_minor_lower,                    &
                  idx_minor_upper,                    &
                  idx_minor_scaling_lower,            &
                  idx_minor_scaling_upper,            &
                  kminor_start_lower,                 &
                  kminor_start_upper,                 &
                  tropo,                              &
                  col_mix,fmajor,fminor,              &
                  play,tlay,col_gas,                  &
                  jeta,jtemp,jpress,                  &
                  tau) bind(C, name="rrtmgp_compute_tau_absorption")
      ! ---------------------
      use mo_rte_kind,      only : wp, wl
      ! input dimensions
      integer,                                intent(in) :: ncol,nlay,nbnd,ngpt         !! array sizes 
      integer,                                intent(in) :: ngas,nflav,neta,npres,ntemp !! tables sizes 
      integer,                                intent(in) :: nminorlower, nminorklower,nminorupper, nminorkupper
                                                            !! table sizes
      integer,                                intent(in) :: idx_h2o                     !! index of water vapor in col_gas
      ! ---------------------
      ! inputs from object
      integer,     dimension(2,ngpt),                  intent(in) :: gpoint_flavor
        !! major gas flavor (pair) by upper/lower, g-point
      integer,     dimension(2,nbnd),                  intent(in) :: band_lims_gpt
        !! beginning and ending g-point for each band 
      real(wp),    dimension(ntemp,neta,npres+1,ngpt), intent(in) :: kmajor
        !! absorption coefficient table - major gases 
      real(wp),    dimension(ntemp,neta,nminorklower), intent(in) :: kminor_lower
        !! absorption coefficient table - minor gases, lower atmosphere 
      real(wp),    dimension(ntemp,neta,nminorkupper), intent(in) :: kminor_upper
        !! absorption coefficient table - minor gases, upper atmosphere 
      integer,     dimension(2,nminorlower),           intent(in) :: minor_limits_gpt_lower
        !! beginning and ending g-point for each minor gas 
      integer,     dimension(2,nminorupper),           intent(in) :: minor_limits_gpt_upper
      logical(wl), dimension(  nminorlower),           intent(in) :: minor_scales_with_density_lower
        !! generic treatment of minor gases - scales with density (e.g. continuum, collision-induced absorption)?
      logical(wl), dimension(  nminorupper),           intent(in) :: minor_scales_with_density_upper
      logical(wl), dimension(  nminorlower),           intent(in) :: scale_by_complement_lower
        !! generic treatment of minor gases - scale by density (e.g. self-continuum) or complement?  
      logical(wl), dimension(  nminorupper),           intent(in) :: scale_by_complement_upper
      integer,     dimension(  nminorlower),           intent(in) :: idx_minor_lower  
        !! index of each minor gas in col_gas
      integer,     dimension(  nminorupper),           intent(in) :: idx_minor_upper
      integer,     dimension(  nminorlower),           intent(in) :: idx_minor_scaling_lower 
        !! for this minor gas, index of the "scaling gas" in col_gas 
      integer,     dimension(  nminorupper),           intent(in) :: idx_minor_scaling_upper
      integer,     dimension(  nminorlower),           intent(in) :: kminor_start_lower 
        !! starting g-point index in minor gas absorption table
      integer,     dimension(  nminorupper),           intent(in) :: kminor_start_upper
      logical(wl), dimension(ncol,nlay),               intent(in) :: tropo
        !! use upper- or lower-atmospheric tables? 
      ! ---------------------
      ! inputs from profile or parent function
      real(wp), dimension(2,    ncol,nlay,nflav       ), intent(in) :: col_mix
        !! combination of major species's column amounts - computed in interpolation() 
      real(wp), dimension(2,2,2,ncol,nlay,nflav       ), intent(in) :: fmajor
        !! interpolation weights for major gases - computed in interpolation() 
      real(wp), dimension(2,2,  ncol,nlay,nflav       ), intent(in) :: fminor
        !! interpolation weights for minor gases - computed in interpolation() 
      real(wp), dimension(            ncol,nlay       ), intent(in) :: play, tlay 
        !! input temperature and pressure 
      real(wp), dimension(            ncol,nlay,0:ngas), intent(in) :: col_gas
        !! input column gas amount (molecules/cm^2) 
      integer,  dimension(2,    ncol,nlay,nflav       ), intent(in) :: jeta
        !! interpolation indexes in eta - computed in interpolation() 
      integer,  dimension(            ncol,nlay       ), intent(in) :: jtemp
        !! interpolation indexes in temperature - computed in interpolation() 
      integer,  dimension(            ncol,nlay       ), intent(in) :: jpress
        !! interpolation indexes in pressure  - computed in interpolation() 
      ! ---------------------
      ! output - optical depth
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau !! aborption optional depth 
    end subroutine compute_tau_absorption
  end interface 
  ! ------------------------------------------------------------------------------------------------------------------
  interface
    subroutine compute_tau_rayleigh(ncol,nlay,nbnd,ngpt,         &
                                    ngas,nflav,neta,npres,ntemp, &
                                    gpoint_flavor,band_lims_gpt, &
                                    krayl,                       &
                                    idx_h2o, col_dry,col_gas,    &
                                    fminor,jeta,tropo,jtemp,     &
                                    tau_rayleigh) bind(C, name="rrtmgp_compute_tau_rayleigh")
      use mo_rte_kind,      only : wp, wl
      integer,                                     intent(in ) :: ncol,nlay,nbnd,ngpt
        !! input dimensions 
      integer,                                     intent(in ) :: ngas,nflav,neta,npres,ntemp
        !! table dimensions 
      integer,     dimension(2,ngpt),              intent(in ) :: gpoint_flavor 
        !! major gas flavor (pair) by upper/lower, g-point
      integer,     dimension(2,nbnd),              intent(in ) :: band_lims_gpt
        !! start and end g-point for each band
      real(wp),    dimension(ntemp,neta,ngpt,2),   intent(in ) :: krayl
        !! Rayleigh scattering coefficients 
      integer,                                     intent(in ) :: idx_h2o
        !! index of water vapor in col_gas
      real(wp),    dimension(ncol,nlay),           intent(in ) :: col_dry
        !! column amount of dry air 
      real(wp),    dimension(ncol,nlay,0:ngas),    intent(in ) :: col_gas
        !! input column gas amount  (molecules/cm^2)
      real(wp),    dimension(2,2,ncol,nlay,nflav), intent(in ) :: fminor
        !! interpolation weights for major gases - computed in interpolation() 
      integer,     dimension(2,  ncol,nlay,nflav), intent(in ) :: jeta
        !! interpolation indexes in eta - computed in interpolation() 
      logical(wl), dimension(ncol,nlay),           intent(in ) :: tropo
        !! use upper- or lower-atmospheric tables? 
      integer,     dimension(ncol,nlay),           intent(in ) :: jtemp
        !! interpolation indexes in temperature - computed in interpolation() 
      ! outputs
      real(wp),    dimension(ncol,nlay,ngpt),      intent(out) :: tau_rayleigh
        !! Rayleigh optical depth 
    end subroutine compute_tau_rayleigh
  end interface
  ! ------------------------------------------------------------------------------------------------------------------
  interface 
    subroutine compute_Planck_source(                        &
                      ncol, nlay, nbnd, ngpt,                &
                      nflav, neta, npres, ntemp, nPlanckTemp,&
                      tlay, tlev, tsfc, sfc_lay,             &
                      fmajor, jeta, tropo, jtemp, jpress,    &
                      gpoint_bands, band_lims_gpt,           &
                      pfracin, temp_ref_min, totplnk_delta, totplnk, gpoint_flavor, &
                      sfc_src, lay_src, lev_src, sfc_source_Jac) bind(C, name="rrtmgp_compute_Planck_source")
      use mo_rte_kind,      only : wp, wl
      integer,                                    intent(in) :: ncol, nlay, nbnd, ngpt
        !! input dimensions 
      integer,                                    intent(in) :: nflav, neta, npres, ntemp, nPlanckTemp
        !! table dimensions 
      real(wp),    dimension(ncol,nlay  ),        intent(in) :: tlay !! temperature at layer centers (K)
      real(wp),    dimension(ncol,nlay+1),        intent(in) :: tlev !! temperature at interfaces (K)
      real(wp),    dimension(ncol       ),        intent(in) :: tsfc !! surface temperture 
      integer,                                    intent(in) :: sfc_lay !! index into surface layer 
      ! Interpolation variables
      real(wp),    dimension(2,2,2,ncol,nlay,nflav), intent(in) :: fmajor
        !! interpolation weights for major gases - computed in interpolation() 
      integer,     dimension(2,    ncol,nlay,nflav), intent(in) :: jeta
        !! interpolation indexes in eta - computed in interpolation() 
      logical(wl), dimension(            ncol,nlay), intent(in) :: tropo
        !! use upper- or lower-atmospheric tables? 
      integer,     dimension(            ncol,nlay), intent(in) :: jtemp, jpress
        !! interpolation indexes in temperature and pressure - computed in interpolation() 
      ! Table-specific
      integer, dimension(ngpt),                     intent(in) :: gpoint_bands  !! band to which each g-point belongs
      integer, dimension(2, nbnd),                  intent(in) :: band_lims_gpt !! start and end g-point for each band
      real(wp), dimension(ntemp,neta,npres+1,ngpt), intent(in) :: pfracin       !! Fraction of the Planck function in each g-point
      real(wp),                                     intent(in) :: temp_ref_min, totplnk_delta !! interpolation constants
      real(wp), dimension(nPlanckTemp,nbnd),        intent(in) :: totplnk       !! Total Planck function by band at each temperature 
      integer,  dimension(2,ngpt),                  intent(in) :: gpoint_flavor !! major gas flavor (pair) by upper/lower, g-point

      real(wp), dimension(ncol,       ngpt), intent(out) :: sfc_src  !! Planck emssion from the surface 
      real(wp), dimension(ncol,nlay,  ngpt), intent(out) :: lay_src  !! Planck emssion from layer centers
      real(wp), dimension(ncol,nlay+1,ngpt), intent(out) :: lev_src  !! Planck emission at layer boundaries
      real(wp), dimension(ncol,       ngpt), intent(out) :: sfc_source_Jac 
        !! Jacobian (derivative) of the surface Planck source with respect to surface temperature 
    end subroutine compute_Planck_source
  end interface
  ! ------------------------------------------------------------------------------------------------------------------
end module mo_gas_optics_rrtmgp_kernels
