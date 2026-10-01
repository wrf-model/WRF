! This code is part of
! RRTM for GCM Applications - Parallel (RRTMGP)
!
! Eli Mlawer and Robert Pincus
! Andre Wehe and Jennifer Delamere
! email:  rrtmgp@aer.com
!
! Copyright 2015-,  Atmospheric and Environmental Research,
! Regents of the University of Colorado, Trustees of Columbia University.  All right reserved.
!
! Use and duplication is permitted under the terms of the
!    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause
! -------------------------------------------------------------------------------------------------
!>
!> ## Numeric calculations for gas optics. Absorption and Rayleigh optical depths, Planck source functions.
!>
!>   - Interpolation coefficients are computed, then used in subsequent routines. 
!>   - All applications will call compute_tau_absorption(); 
!>     compute_tau_rayleigh() and/or compute_Planck_source() will be called depending on the 
!>     configuration of the k-distribution. 
!>   - The details of the interpolation scheme are not particaulrly important as long as arrays including 
!>     tables are passed consisently between kernels. 
!>
! -------------------------------------------------------------------------------------------------

module mo_gas_optics_rrtmgp_kernels
  use mo_rte_kind,      only : wp, wl
  use mo_rte_util_array,only : zero_array
  implicit none
  private
  public :: interpolation, compute_tau_absorption, compute_tau_rayleigh, compute_Planck_source
contains
  ! --------------------------------------------------------------------------------------
  !> Compute interpolation coefficients
  !> for calculations of major optical depths, minor optical depths, Rayleigh,
  !> and Planck fractions
  subroutine interpolation( &
                ncol,nlay,ngas,nflav,neta, npres, ntemp, &
                flavor,                                  &
                press_ref_log, temp_ref,press_ref_log_delta,    &
                temp_ref_min,temp_ref_delta,press_ref_trop_log, &
                vmr_ref,                                        &
                play,tlay,col_gas,                              &
                jtemp,fmajor,fminor,col_mix,tropo,jeta,jpress) bind(C, name="rrtmgp_interpolation")
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
#if !defined(__INTEL_LLVM_COMPILER) && __INTEL_COMPILER >= 1910
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
    ! -----------------
    ! local
    real(wp), dimension(ncol,nlay) :: ftemp, fpress ! interpolation fraction for temperature, pressure
    real(wp) :: locpress       ! needed to find location in pressure grid
    real(wp) :: ratio_eta_half ! ratio of vmrs of major species that defines eta=0.5
                               ! for given flavor and reference temperature level
    real(wp) :: eta, feta      ! binary_species_parameter, interpolation variable for eta
    real(wp) :: loceta         ! needed to find location in eta grid
    real(wp) :: ftemp_term
    ! -----------------
    ! local indexes
    integer :: icol, ilay, iflav, igases(2), itropo, itemp

    do ilay = 1, nlay
      do icol = 1, ncol
        ! index and factor for temperature interpolation
        jtemp(icol,ilay) = int((tlay(icol,ilay) - (temp_ref_min - temp_ref_delta)) / temp_ref_delta)
        jtemp(icol,ilay) = min(ntemp - 1, max(1, jtemp(icol,ilay))) ! limit the index range
        ftemp(icol,ilay) = (tlay(icol,ilay) - temp_ref(jtemp(icol,ilay))) / temp_ref_delta

        ! index and factor for pressure interpolation
        locpress = 1._wp + (log(play(icol,ilay)) - press_ref_log(1)) / press_ref_log_delta
        jpress(icol,ilay) = min(npres-1, max(1, int(locpress)))
        fpress(icol,ilay) = locpress - float(jpress(icol,ilay))

        ! determine if in lower or upper part of atmosphere
        tropo(icol,ilay) = log(play(icol,ilay)) > press_ref_trop_log
      end do
    end do

    do iflav = 1, nflav
      igases(:) = flavor(:,iflav)
      do ilay = 1, nlay
        do icol = 1, ncol
        ! itropo = 1 lower atmosphere; itropo = 2 upper atmosphere
        itropo = merge(1,2,tropo(icol,ilay))
        ! loop over implemented combinations of major species
          do itemp = 1, 2
            ! compute interpolation fractions needed for lower, then upper reference temperature level
            ! compute binary species parameter (eta) for flavor and temperature and
            !  associated interpolation index and factors
            ratio_eta_half = vmr_ref(itropo,igases(1),(jtemp(icol,ilay)+itemp-1)) / &
                             vmr_ref(itropo,igases(2),(jtemp(icol,ilay)+itemp-1))
            col_mix(itemp,icol,ilay,iflav) = col_gas(icol,ilay,igases(1)) + ratio_eta_half * col_gas(icol,ilay,igases(2))
            ! Keep this commented lines. Fortran does allow for
            ! substantial optimizations and in this merge cases may
            ! happen that all expressions are evaluated and so create
            ! a division by zero. In the if construct this should be
            ! save. Merge is the way to do it in general inside of
            ! loops, but sometimes it may not work.
            !
            ! eta = merge(col_gas(icol,ilay,igases(1)) / col_mix(itemp,icol,ilay,iflav), 0.5_wp, &
            !             col_mix(itemp,icol,ilay,iflav) > 2._wp * tiny(col_mix))
            !
            ! In essence: do not turn it back to merge(...)!
            if (col_mix(itemp,icol,ilay,iflav) > 2._wp * tiny(col_mix)) then
              eta = col_gas(icol,ilay,igases(1)) / col_mix(itemp,icol,ilay,iflav)
            else
              eta = 0.5_wp
            endif
            loceta = eta * float(neta-1)
            jeta(itemp,icol,ilay,iflav) = min(int(loceta)+1, neta-1)
            feta = mod(loceta, 1.0_wp)
            ! compute interpolation fractions needed for minor species
            ! ftemp_term = (1._wp-ftemp(icol,ilay)) for itemp = 1, ftemp(icol,ilay) for itemp=2
            ftemp_term = (real(2-itemp, wp) + real(2*itemp-3, wp) * ftemp(icol,ilay))
            fminor(1,itemp,icol,ilay,iflav) = (1._wp-feta) * ftemp_term
            fminor(2,itemp,icol,ilay,iflav) =        feta  * ftemp_term
            ! compute interpolation fractions needed for major species
            fmajor(1,1,itemp,icol,ilay,iflav) = (1._wp-fpress(icol,ilay)) * fminor(1,itemp,icol,ilay,iflav)
            fmajor(2,1,itemp,icol,ilay,iflav) = (1._wp-fpress(icol,ilay)) * fminor(2,itemp,icol,ilay,iflav)
            fmajor(1,2,itemp,icol,ilay,iflav) =        fpress(icol,ilay)  * fminor(1,itemp,icol,ilay,iflav)
            fmajor(2,2,itemp,icol,ilay,iflav) =        fpress(icol,ilay)  * fminor(2,itemp,icol,ilay,iflav)
          end do ! reference temperatures
        end do ! icol
      end do ! ilay
    end do ! iflav

  end subroutine interpolation
  ! --------------------------------------------------------------------------------------
  !
  !> Compute minor and major species optical depth using pre-computed interpolation coefficients
  !>   (jeta,jtemp,jpress) and weights (fmajor, fminor)
  !
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
    ! ---------------------
    ! Local variables
    !
    logical                    :: top_at_1
    integer, dimension(ncol,2) :: itropo_lower, itropo_upper
    ! ----------------------------------------------------------------

    ! ---------------------
    ! Layer limits of upper, lower atmospheres
    ! ---------------------
    top_at_1 = play(1,1) < play(1, nlay)
    if(top_at_1) then
      itropo_lower(:, 1) = minloc(play, dim=2, mask=tropo)
      itropo_lower(:, 2) = nlay
      itropo_upper(:, 1) = 1
      itropo_upper(:, 2) = maxloc(play, dim=2, mask=(.not. tropo))
    else
      itropo_lower(:, 1) = 1
      itropo_lower(:, 2) = minloc(play, dim=2, mask= tropo)
      itropo_upper(:, 1) = maxloc(play, dim=2, mask=(.not. tropo))
      itropo_upper(:, 2) = nlay
    end if
    ! ---------------------
    ! Major Species
    ! ---------------------
    call gas_optical_depths_major(   &
          ncol,nlay,nbnd,ngpt,       & ! dimensions
          nflav,neta,npres,ntemp,    &
          gpoint_flavor,             &
          band_lims_gpt,             &
          kmajor,                    &
          col_mix,fmajor,            &
          jeta,tropo,jtemp,jpress,   &
          tau)
    ! ---------------------
    ! Minor Species - lower
    ! ---------------------
    call gas_optical_depths_minor(     &
           ncol,nlay,ngpt,             & ! dimensions
           ngas,nflav,ntemp,neta,      &
           nminorlower,nminorklower,   &
           idx_h2o,                    &
           gpoint_flavor(1,:),         &
           kminor_lower,               &
           minor_limits_gpt_lower,     &
           minor_scales_with_density_lower, &
           scale_by_complement_lower,  &
           idx_minor_lower,            &
           idx_minor_scaling_lower,    &
           kminor_start_lower,         &
           play, tlay,                 &
           col_gas,fminor,jeta,        &
           itropo_lower,jtemp,         &
           tau)
    ! ---------------------
    ! Minor Species - upper
    ! ---------------------
    call gas_optical_depths_minor(     &
           ncol,nlay,ngpt,             & ! dimensions
           ngas,nflav,ntemp,neta,      &
           nminorupper,nminorkupper,   &
           idx_h2o,                    &
           gpoint_flavor(2,:),         &
           kminor_upper,               &
           minor_limits_gpt_upper,     &
           minor_scales_with_density_upper, &
           scale_by_complement_upper,  &
           idx_minor_upper,            &
           idx_minor_scaling_upper,    &
           kminor_start_upper,         &
           play, tlay,                 &
           col_gas,fminor,jeta,        &
           itropo_upper,jtemp,         &
           tau)
  end subroutine compute_tau_absorption
  ! --------------------------------------------------------------------------------------

  ! --------------------------------------------------------------------------------------
  !
  ! compute minor species optical depths
  !
  subroutine gas_optical_depths_major(ncol,nlay,nbnd,ngpt,&
                                      nflav,neta,npres,ntemp,      & ! dimensions
                                      gpoint_flavor, band_lims_gpt,   & ! inputs from object
                                      kmajor,                         &
                                      col_mix,fmajor,                 &
                                      jeta,tropo,jtemp,jpress,        & ! local input
                                      tau)
    ! input dimensions
    integer, intent(in) :: ncol, nlay, nbnd, ngpt, nflav,neta,npres,ntemp  ! dimensions

    ! inputs from object
    integer,  dimension(2,ngpt),  intent(in) :: gpoint_flavor
    integer,  dimension(2,nbnd),  intent(in) :: band_lims_gpt ! start and end g-point for each band
    real(wp), dimension(ntemp,neta,npres+1,ngpt), intent(in) :: kmajor

    ! inputs from profile or parent function
    real(wp),    dimension(2,    ncol,nlay,nflav), intent(in) :: col_mix
    real(wp),    dimension(2,2,2,ncol,nlay,nflav), intent(in) :: fmajor
    integer,     dimension(2,    ncol,nlay,nflav), intent(in) :: jeta
    logical(wl), dimension(ncol,nlay), intent(in) :: tropo
    integer,     dimension(ncol,nlay), intent(in) :: jtemp, jpress

    ! outputs
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau
    ! -----------------
    ! local variables
    real(wp) :: tau_major(ngpt) ! major species optical depth
    ! local index
    integer :: icol, ilay, iflav, ibnd, itropo
    integer :: gptS, gptE

    ! optical depth calculation for major species
    do ibnd = 1, nbnd
      gptS = band_lims_gpt(1, ibnd)
      gptE = band_lims_gpt(2, ibnd)
      do ilay = 1, nlay
        do icol = 1, ncol
          ! itropo = 1 lower atmosphere; itropo = 2 upper atmosphere
          itropo = merge(1,2,tropo(icol,ilay))
          iflav = gpoint_flavor(itropo, gptS) !eta interpolation depends on band's flavor
          tau_major(gptS:gptE) = &
            ! interpolation in temperature, pressure, and eta
            interpolate3D_byflav(col_mix(:,icol,ilay,iflav),                                     &
                                 fmajor(:,:,:,icol,ilay,iflav), kmajor,                          &
                                 band_lims_gpt(1, ibnd), band_lims_gpt(2, ibnd),                 &
                                 jeta(:,icol,ilay,iflav), jtemp(icol,ilay),jpress(icol,ilay)+itropo)
            tau(icol,ilay,gptS:gptE) = tau(icol,ilay,gptS:gptE) + tau_major(gptS:gptE)
        end do
      end do
    end do

  end subroutine gas_optical_depths_major

  ! ----------------------------------------------------------
  !
  ! compute minor species optical depths
  !
  subroutine gas_optical_depths_minor(ncol,nlay,ngpt,        &
                                      ngas,nflav,ntemp,neta, &
                                      nminor,nminork,        &
                                      idx_h2o,               &
                                      gpt_flv,               &
                                      kminor,                &
                                      minor_limits_gpt,      &
                                      minor_scales_with_density,    &
                                      scale_by_complement,   &
                                      idx_minor, idx_minor_scaling, &
                                      kminor_start,          &
                                      play, tlay,            &
                                      col_gas,fminor,jeta,   &
                                      layer_limits,jtemp,    &
                                      tau)
    integer,                                     intent(in   ) :: ncol,nlay,ngpt
    integer,                                     intent(in   ) :: ngas,nflav
    integer,                                     intent(in   ) :: ntemp,neta,nminor,nminork
    integer,                                     intent(in   ) :: idx_h2o
    integer,     dimension(ngpt),                intent(in   ) :: gpt_flv
    real(wp),    dimension(ntemp,neta,nminork),  intent(in   ) :: kminor
    integer,     dimension(2,nminor),            intent(in   ) :: minor_limits_gpt
    logical(wl), dimension(  nminor),            intent(in   ) :: minor_scales_with_density
    logical(wl), dimension(  nminor),            intent(in   ) :: scale_by_complement
    integer,     dimension(  nminor),            intent(in   ) :: kminor_start
    integer,     dimension(  nminor),            intent(in   ) :: idx_minor, idx_minor_scaling
    real(wp),    dimension(ncol,nlay),           intent(in   ) :: play, tlay
    real(wp),    dimension(ncol,nlay,0:ngas),    intent(in   ) :: col_gas
    real(wp),    dimension(2,2,ncol,nlay,nflav), intent(in   ) :: fminor
    integer,     dimension(2,  ncol,nlay,nflav), intent(in   ) :: jeta
    integer,     dimension(ncol, 2),             intent(in   ) :: layer_limits
    integer,     dimension(ncol,nlay),           intent(in   ) :: jtemp
    real(wp),    dimension(ncol,nlay,ngpt),      intent(inout) :: tau
    ! -----------------
    ! local variables
    real(wp), parameter :: PaTohPa = 0.01_wp
    real(wp) :: vmr_fact, dry_fact             ! conversion from column abundance to dry vol. mixing ratio;
    real(wp) :: scaling                        ! optical depth
    integer  :: icol, ilay, iflav, imnr
    integer  :: gptS, gptE
    real(wp), dimension(ngpt) :: tau_minor
    ! -----------------
    !
    ! Guard against layer limits being 0 -- that means don't do anything i.e. there are no
    !   layers with pressures in the upper or lower atmosphere respectively
    ! First check skips the routine entirely if all columns are out of bounds...
    !

    if(any(layer_limits(:,1) > 0)) then
      do imnr = 1, size(scale_by_complement,dim=1) ! loop over minor absorbers in each band
        do icol = 1, ncol
          !
          ! This check skips individual columns with no pressures in range
          !
          if(layer_limits(icol,1) > 0) then
            do ilay = layer_limits(icol,1), layer_limits(icol,2)
              !
              ! Scaling of minor gas absortion coefficient begins with column amount of minor gas
              !
              scaling = col_gas(icol,ilay,idx_minor(imnr))
              !
              ! Density scaling (e.g. for h2o continuum, collision-induced absorption)
              !
              if (minor_scales_with_density(imnr)) then
                !
                ! NOTE: P needed in hPa to properly handle density scaling.
                !
                scaling = scaling * (PaTohPa*play(icol,ilay)/tlay(icol,ilay))
                if(idx_minor_scaling(imnr) > 0) then  ! there is a second gas that affects this gas's absorption
                  vmr_fact = 1._wp / col_gas(icol,ilay,0)
                  dry_fact = 1._wp / (1._wp + col_gas(icol,ilay,idx_h2o) * vmr_fact)
                  ! scale by density of special gas
                  if (scale_by_complement(imnr)) then ! scale by densities of all gases but the special one
                    scaling = scaling * (1._wp - col_gas(icol,ilay,idx_minor_scaling(imnr)) * vmr_fact * dry_fact)
                  else
                    scaling = scaling *         (col_gas(icol,ilay,idx_minor_scaling(imnr)) * vmr_fact * dry_fact)
                  endif
                endif
              endif
              !
              ! Interpolation of absorption coefficient and calculation of optical depth
              !
              ! Which gpoint range does this minor gas affect?
              gptS = minor_limits_gpt(1,imnr)
              gptE = minor_limits_gpt(2,imnr)
              iflav = gpt_flv(gptS)
              tau_minor(gptS:gptE) = scaling *                   &
                                      interpolate2D_byflav(fminor(:,:,icol,ilay,iflav), &
                                                           kminor, &
                                                           kminor_start(imnr), kminor_start(imnr)+(gptE-gptS), &
                                                           jeta(:,icol,ilay,iflav), jtemp(icol,ilay))
              tau(icol,ilay,gptS:gptE) = tau(icol,ilay,gptS:gptE) + tau_minor(gptS:gptE)
            enddo
          end if
        enddo
      enddo
    end if

  end subroutine gas_optical_depths_minor
  ! ----------------------------------------------------------
  !
  ! compute Rayleigh scattering optical depths
  !
  subroutine compute_tau_rayleigh(ncol,nlay,nbnd,ngpt,         &
                                  ngas,nflav,neta,npres,ntemp, &
                                  gpoint_flavor,band_lims_gpt, &
                                  krayl,                       &
                                  idx_h2o, col_dry,col_gas,    &
                                  fminor,jeta,tropo,jtemp,     &
                                  tau_rayleigh) bind(C, name="rrtmgp_compute_tau_rayleigh")
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
    ! -----------------
    ! local variables
    real(wp) :: k(ngpt) ! rayleigh scattering coefficient
    integer  :: icol, ilay, iflav, ibnd, gptS, gptE
    integer  :: itropo
    ! -----------------

    do ibnd = 1, nbnd
      gptS = band_lims_gpt(1, ibnd)
      gptE = band_lims_gpt(2, ibnd)
      do ilay = 1, nlay
        do icol = 1, ncol
          itropo = merge(1,2,tropo(icol,ilay)) ! itropo = 1 lower atmosphere;itropo = 2 upper atmosphere
          iflav = gpoint_flavor(itropo, gptS) !eta interpolation depends on band's flavor
          k(gptS:gptE) = interpolate2D_byflav(fminor(:,:,icol,ilay,iflav), &
                                              krayl(:,:,:,itropo),      &
                                              gptS, gptE, jeta(:,icol,ilay,iflav), jtemp(icol,ilay))
          tau_rayleigh(icol,ilay,gptS:gptE) = k(gptS:gptE) * &
                                              (col_gas(icol,ilay,idx_h2o)+col_dry(icol,ilay))
        end do
      end do
    end do

  end subroutine compute_tau_rayleigh

  ! ----------------------------------------------------------
  subroutine compute_Planck_source(                        &
                    ncol, nlay, nbnd, ngpt,                &
                    nflav, neta, npres, ntemp, nPlanckTemp,&
                    tlay, tlev, tsfc, sfc_lay,             &
                    fmajor, jeta, tropo, jtemp, jpress,    &
                    gpoint_bands, band_lims_gpt,           &
                    pfracin, temp_ref_min, totplnk_delta, totplnk, gpoint_flavor, &
                    sfc_src, lay_src, lev_src, sfc_source_Jac) bind(C, name="rrtmgp_compute_Planck_source")
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
    real(wp),                                     intent(in) :: temp_ref_min, totplnk_delta !! interpolation constants
    real(wp), dimension(ntemp,neta,npres+1,ngpt), intent(in) :: pfracin       !! Fraction of the Planck function in each g-point
    real(wp), dimension(nPlanckTemp,nbnd),        intent(in) :: totplnk       !! Total Planck function by band at each temperature 
    integer,  dimension(2,ngpt),                  intent(in) :: gpoint_flavor !! major gas flavor (pair) by upper/lower, g-point

    real(wp), dimension(ncol,       ngpt), intent(out) :: sfc_src  !! Planck emission from the surface 
    real(wp), dimension(ncol,nlay,  ngpt), intent(out) :: lay_src  !! Planck emission from layer centers
    real(wp), dimension(ncol,nlay+1,ngpt), intent(out) :: lev_src  !! Planck emission from layer boundaries
    real(wp), dimension(ncol,       ngpt), intent(out) :: sfc_source_Jac 
      !! Jacobian (derivative) of the surface Planck source with respect to surface temperature 
    ! -----------------
    ! local
    real(wp), parameter                             :: delta_Tsurf = 1.0_wp

    integer  :: ilay, icol, igpt, ibnd, itropo, iflav
    integer  :: gptS, gptE
    real(wp), dimension(2), parameter :: one = [1._wp, 1._wp]
    real(wp) :: pfrac          (ncol,nlay  ,ngpt)
    real(wp) :: planck_function(ncol,nlay+1,nbnd)
    ! -----------------

    ! Calculation of fraction of band's Planck irradiance associated with each g-point
    do ibnd = 1, nbnd
      gptS = band_lims_gpt(1, ibnd)
      gptE = band_lims_gpt(2, ibnd)
      do ilay = 1, nlay
        do icol = 1, ncol
          ! itropo = 1 lower atmosphere; itropo = 2 upper atmosphere
          itropo = merge(1,2,tropo(icol,ilay))
          iflav = gpoint_flavor(itropo, gptS) !eta interpolation depends on band's flavor
          pfrac(icol,ilay,gptS:gptE) = &
            ! interpolation in temperature, pressure, and eta
            interpolate3D_byflav(one, fmajor(:,:,:,icol,ilay,iflav), pfracin, &
                          band_lims_gpt(1, ibnd), band_lims_gpt(2, ibnd),                 &
                          jeta(:,icol,ilay,iflav), jtemp(icol,ilay),jpress(icol,ilay)+itropo)
        end do ! column
      end do   ! layer
    end do     ! band

    !
    ! Planck function by band for the surface
    ! Compute surface source irradiance for g-point, equals band irradiance x fraction for g-point
    !
    do icol = 1, ncol
      planck_function(icol,1,1:nbnd) = interpolate1D(tsfc(icol), temp_ref_min, totplnk_delta, totplnk)
      planck_function(icol,2,1:nbnd) = interpolate1D(tsfc(icol) + delta_Tsurf, temp_ref_min, totplnk_delta, totplnk)
      !
      ! Map to g-points
      !
      do ibnd = 1, nbnd
        gptS = band_lims_gpt(1, ibnd)
        gptE = band_lims_gpt(2, ibnd)
        do igpt = gptS, gptE
            sfc_src(icol,igpt) = pfrac(icol,sfc_lay,igpt) * planck_function(icol,1,ibnd)
            sfc_source_Jac(icol, igpt) = pfrac(icol,sfc_lay,igpt) * &
                                (planck_function(icol, 2, ibnd) - planck_function(icol,1,ibnd))
        end do
      end do
    end do !icol

    do ilay = 1, nlay
      do icol = 1, ncol
        ! Compute layer source irradiance for g-point, equals band irradiance x fraction for g-point
        planck_function(icol,ilay,1:nbnd) = interpolate1D(tlay(icol,ilay), temp_ref_min, totplnk_delta, totplnk)
      end do
    end do

    !
    ! Map to g-points
    !
    do ibnd = 1, nbnd
      gptS = band_lims_gpt(1, ibnd)
      gptE = band_lims_gpt(2, ibnd)
      do igpt = gptS, gptE
        do ilay = 1, nlay
          do icol = 1, ncol
            lay_src(icol,ilay,igpt) = pfrac(icol,ilay,igpt) * planck_function(icol,ilay,ibnd)
          end do
        end do
      end do
    end do

    ! compute level source irradiances for each g-point
    do icol = 1, ncol
      planck_function  (icol,     1,1:nbnd) = interpolate1D(tlev(icol,     1),temp_ref_min, totplnk_delta, totplnk)
    end do
    do ilay = 1, nlay
      do icol = 1, ncol
        planck_function(icol,ilay+1,1:nbnd) = interpolate1D(tlev(icol,ilay+1),temp_ref_min, totplnk_delta, totplnk)
      end do
    end do

    !
    ! Map to g-points
    !
    do ibnd = 1, nbnd
      gptS = band_lims_gpt(1, ibnd)
      gptE = band_lims_gpt(2, ibnd)
      do igpt = gptS, gptE
        do icol = 1, ncol
          lev_src(icol,     1,igpt) = pfrac(icol,   1,igpt) * planck_function(icol,     1,ibnd)
        end do
        do ilay = 2, nlay
          do icol = 1, ncol
            lev_src(icol,ilay,igpt) = sqrt(pfrac(icol,ilay-1, igpt) *  & 
                                           pfrac(icol,ilay,   igpt)) & 
                                                            * planck_function(icol,ilay,  ibnd)
          end do
        end do
        do icol = 1, ncol
          lev_src(icol,nlay+1,igpt) = pfrac(icol,nlay,igpt) * planck_function(icol,nlay+1,ibnd)
        end do
      end do
    end do

  end subroutine compute_Planck_source
  ! ----------------------------------------------------------
  !
  ! One dimensional interpolation -- return all values along second table dimension
  !
  pure function interpolate1D(val, offset, delta, table) result(res)
    ! input
    real(wp), intent(in) :: val,    & ! axis value at which to evaluate table
                            offset, & ! minimum of table axis
                            delta     ! step size of table axis
    real(wp), dimension(:,:), &
              intent(in) :: table ! dimensions (axis, values)
    ! output
    real(wp), dimension(size(table,dim=2)) :: res

    ! local
    real(wp) :: val0 ! fraction index adjusted by offset and delta
    integer :: index ! index term
    real(wp) :: frac ! fractional term
    ! -------------------------------------
    val0 = (val - offset) / delta
    frac = val0 - int(val0) ! get fractional part
    index = min(size(table,dim=1)-1, max(1, int(val0)+1)) ! limit the index range
    res(:) = table(index,:) + frac * (table(index+1,:) - table(index,:))
  end function interpolate1D
  ! ----------------------------------------------------------
  !   This function returns a range of values from a subset (in gpoint) of the k table
  !
  pure function interpolate2D_byflav(fminor, k, gptS, gptE, jeta, jtemp) result(res)
    real(wp), dimension(2,2), intent(in) :: fminor ! interpolation fractions for minor species
                                       ! index(1) : reference eta level (temperature dependent)
                                       ! index(2) : reference temperature level
    real(wp), dimension(:,:,:), intent(in) :: k ! (g-point, eta, temp)
    integer,                    intent(in) :: gptS, gptE, jtemp ! interpolation index for temperature
    integer, dimension(2),      intent(in) :: jeta ! interpolation index for binary species parameter (eta)
    real(wp), dimension(gptE-gptS+1)       :: res ! the result

    ! Local variable
    integer :: igpt
    ! each code block is for a different reference temperature

    do igpt = 1, gptE-gptS+1
      res(igpt) = fminor(1,1) * k(jtemp  , jeta(1)  , gptS+igpt-1) + &
                  fminor(2,1) * k(jtemp  , jeta(1)+1, gptS+igpt-1) + &
                  fminor(1,2) * k(jtemp+1, jeta(2)  , gptS+igpt-1) + &
                  fminor(2,2) * k(jtemp+1, jeta(2)+1, gptS+igpt-1)
    end do

  end function interpolate2D_byflav
  ! ----------------------------------------------------------
  pure function interpolate3D_byflav(scaling, fmajor, k, gptS, gptE, jeta, jtemp, jpress) result(res)
    real(wp), dimension(2),     intent(in) :: scaling
    real(wp), dimension(2,2,2), intent(in) :: fmajor ! interpolation fractions for major species
                                                     ! index(1) : reference eta level (temperature dependent)
                                                     ! index(2) : reference pressure level
                                                     ! index(3) : reference temperature level
    real(wp), dimension(:,:,:,:),intent(in) :: k ! (temp,eta,press,gpt)
    integer,                     intent(in) :: gptS, gptE
    integer, dimension(2),       intent(in) :: jeta ! interpolation index for binary species parameter (eta)
    integer,                     intent(in) :: jtemp ! interpolation index for temperature
    integer,                     intent(in) :: jpress ! interpolation index for pressure
    real(wp), dimension(gptS:gptE)          :: res ! the result

    ! Local variable
    integer :: igpt
    ! each code block is for a different reference temperature
    do igpt = gptS, gptE
      res(igpt) =  &
        scaling(1) * &
        ( fmajor(1,1,1) * k(jtemp, jeta(1)  , jpress-1, igpt) + &
          fmajor(2,1,1) * k(jtemp, jeta(1)+1, jpress-1, igpt) + &
          fmajor(1,2,1) * k(jtemp, jeta(1)  , jpress  , igpt) + &
          fmajor(2,2,1) * k(jtemp, jeta(1)+1, jpress  , igpt) ) + &
        scaling(2) * &
        ( fmajor(1,1,2) * k(jtemp+1, jeta(2)  , jpress-1, igpt) + &
          fmajor(2,1,2) * k(jtemp+1, jeta(2)+1, jpress-1, igpt) + &
          fmajor(1,2,2) * k(jtemp+1, jeta(2)  , jpress  , igpt) + &
          fmajor(2,2,2) * k(jtemp+1, jeta(2)+1, jpress  , igpt) )
    end do
  end function interpolate3D_byflav

end module mo_gas_optics_rrtmgp_kernels
