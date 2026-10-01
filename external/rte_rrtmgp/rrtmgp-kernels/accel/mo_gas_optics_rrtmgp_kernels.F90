! This code is part of
! RRTM for GCM Applications - Parallel (RRTMGP)
!
! Eli Mlawer and Robert Pincus
! Andre Wehe and Jennifer Delamere
! email:  rrtmgp@aer.com
!
! Copyright 2015,  Atmospheric and Environmental Research and
! Regents of the University of Colorado.  All right reserved.
!
! Use and duplication is permitted under the terms of the
!    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause
!
! Description: Numeric calculations for gas optics. Absorption and Rayleigh optical depths,
!   source functions.

module mo_gas_optics_rrtmgp_kernels
  use mo_rte_kind,      only: wp, wl
  use mo_rte_util_array,only: zero_array
  implicit none
  private
  public :: interpolation, compute_tau_absorption, compute_tau_rayleigh, compute_Planck_source
contains
  ! --------------------------------------------------------------------------------------
  ! Compute interpolation coefficients
  ! for calculations of major optical depths, minor optical depths, Rayleigh,
  ! and Planck fractions
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
    integer,                            intent(in) :: ngas,nflav,neta,npres,ntemp
    integer,     dimension(2,nflav),    intent(in) :: flavor
    real(wp),    dimension(npres),      intent(in) :: press_ref_log
    real(wp),    dimension(ntemp),      intent(in) :: temp_ref
    real(wp),                           intent(in) :: press_ref_log_delta, &
                                                      temp_ref_min, temp_ref_delta, &
                                                      press_ref_trop_log
    real(wp),    dimension(2,0:ngas,ntemp), intent(in) :: vmr_ref

    ! inputs from profile or parent function
    real(wp),    dimension(ncol,nlay),        intent(in) :: play, tlay
    real(wp),    dimension(ncol,nlay,0:ngas), intent(in) :: col_gas

    ! outputs
    integer,     dimension(ncol,nlay), intent(out) :: jtemp, jpress
    logical(wl), dimension(ncol,nlay), intent(out) :: tropo
    integer,     dimension(2,    ncol,nlay,nflav), intent(out) :: jeta
    real(wp),    dimension(2,    ncol,nlay,nflav), intent(out) :: col_mix
    real(wp),    dimension(2,2,2,ncol,nlay,nflav), intent(out) :: fmajor
    real(wp),    dimension(2,2,  ncol,nlay,nflav), intent(out) :: fminor
    ! -----------------
    ! local
    real(wp), dimension(ncol,nlay) :: ftemp, fpress ! interpolation fraction for temperature, pressure
    real(wp) :: locpress ! needed to find location in pressure grid
    real(wp) :: ratio_eta_half ! ratio of vmrs of major species that defines eta=0.5
                               ! for given flavor and reference temperature level
    real(wp) :: eta, feta      ! binary_species_parameter, interpolation variable for eta
    real(wp) :: loceta         ! needed to find location in eta grid
    real(wp) :: ftemp_term
    ! -----------------
    ! local indexes
    integer :: icol, ilay, iflav, igases(2), itropo, itemp

    !$acc data copyin(flavor,press_ref_log,temp_ref,vmr_ref,play,tlay,col_gas) &
    !$acc      copyout(jtemp,jpress,tropo,jeta,col_mix,fmajor,fminor) &
    !$acc      create(ftemp,fpress)
    !$omp target data map(to:flavor, press_ref_log, temp_ref, vmr_ref, play, tlay, col_gas) &
    !$omp             map(alloc:jtemp, jpress, tropo, jeta, col_mix, fmajor, fminor) &
    !$omp             map(alloc:ftemp, fpress)

    !$acc parallel loop gang vector collapse(2) default(present)
    !$omp target teams distribute parallel do simd collapse(2)
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

    ! loop over implemented combinations of major species
    ! PGI BUG WORKAROUND: if present(vmr_ref) isn't there, OpenACC runtime
    ! thinks it isn't present.
    !$acc parallel loop gang vector collapse(4) default(present) private(igases)
    !$omp target teams distribute parallel do simd collapse(4) private(igases)
    do iflav = 1, nflav
      do ilay = 1, nlay
        ! loop over implemented combinations of major species
        do icol = 1, ncol
          do itemp = 1, 2
            igases(:) = flavor(:,iflav)
            ! itropo = 1 lower atmosphere; itropo = 2 upper atmosphere
            itropo = merge(1,2,tropo(icol,ilay))
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

    !$acc end data
    !$omp end target data

  end subroutine interpolation
  ! --------------------------------------------------------------------------------------
  !
  ! Compute minor and major species opitcal depth from pre-computed interpolation coefficients
  !   (jeta,jtemp,jpress)
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
    integer,                                intent(in) :: ncol,nlay,nbnd,ngpt
    integer,                                intent(in) :: ngas,nflav,neta,npres,ntemp
    integer,                                intent(in) :: nminorlower, nminorklower,nminorupper, nminorkupper
    integer,                                intent(in) :: idx_h2o
    ! ---------------------
    ! inputs from object
    integer,     dimension(2,ngpt),                  intent(in) :: gpoint_flavor
    integer,     dimension(2,nbnd),                  intent(in) :: band_lims_gpt
    real(wp),    dimension(ntemp,neta,npres+1,ngpt), intent(in) :: kmajor
    real(wp),    dimension(ntemp,neta,nminorklower), intent(in) :: kminor_lower
    real(wp),    dimension(ntemp,neta,nminorkupper), intent(in) :: kminor_upper
    integer,     dimension(2,nminorlower),           intent(in) :: minor_limits_gpt_lower
    integer,     dimension(2,nminorupper),           intent(in) :: minor_limits_gpt_upper
    logical(wl), dimension(  nminorlower),           intent(in) :: minor_scales_with_density_lower
    logical(wl), dimension(  nminorupper),           intent(in) :: minor_scales_with_density_upper
    logical(wl), dimension(  nminorlower),           intent(in) :: scale_by_complement_lower
    logical(wl), dimension(  nminorupper),           intent(in) :: scale_by_complement_upper
    integer,     dimension(  nminorlower),           intent(in) :: idx_minor_lower
    integer,     dimension(  nminorupper),           intent(in) :: idx_minor_upper
    integer,     dimension(  nminorlower),           intent(in) :: idx_minor_scaling_lower
    integer,     dimension(  nminorupper),           intent(in) :: idx_minor_scaling_upper
    integer,     dimension(  nminorlower),           intent(in) :: kminor_start_lower
    integer,     dimension(  nminorupper),           intent(in) :: kminor_start_upper
    logical(wl), dimension(ncol,nlay),               intent(in) :: tropo
    ! ---------------------
    ! inputs from profile or parent function
    real(wp), dimension(2,    ncol,nlay,nflav       ), intent(in) :: col_mix
    real(wp), dimension(2,2,2,ncol,nlay,nflav       ), intent(in) :: fmajor
    real(wp), dimension(2,2,  ncol,nlay,nflav       ), intent(in) :: fminor
    real(wp), dimension(            ncol,nlay       ), intent(in) :: play, tlay      ! pressure and temperature
    real(wp), dimension(            ncol,nlay,0:ngas), intent(in) :: col_gas
    integer,  dimension(2,    ncol,nlay,nflav       ), intent(in) :: jeta
    integer,  dimension(            ncol,nlay       ), intent(in) :: jtemp
    integer,  dimension(            ncol,nlay       ), intent(in) :: jpress
    ! ---------------------
    ! output - optical depth
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau
    ! ---------------------
    ! Local variables
    !
    logical(wl)                :: top_at_1
    integer, dimension(ncol,2) :: itropo_lower, itropo_upper
    integer                    :: icol, idx_tropo

    ! ----------------------------------------------------------------

    !$acc enter data create(itropo_lower, itropo_upper)
    !$omp target enter data map(alloc:itropo_lower, itropo_upper)
    !$acc enter data copyin(play, tlay, tropo, gpoint_flavor, jeta, jtemp, col_gas, fminor, tau)
    !$omp target enter data map(to:play, tlay, tropo, gpoint_flavor, jeta, jtemp, col_gas, fminor, tau)

    ! ---------------------
    ! Layer limits of upper, lower atmospheres
    ! ---------------------

    !$acc kernels copyout(top_at_1)
    !$omp target map(from:top_at_1)
    top_at_1 = play(1,1) < play(1, nlay)
    !$acc end kernels
    !$omp end target

    if(top_at_1) then
      !$acc parallel loop
      !$omp target teams distribute parallel do simd
      do icol = 1,ncol
        itropo_lower(icol,2) = nlay
#if ( defined(_CRAYFTN) && _RELEASE_MAJOR <= 14 ) || ( defined(_OPENMP) && defined(__NVCOMPILER) )
        itropo_upper(icol,1) = 1
        call minmaxloc(icol, tropo, play, itropo_lower(icol,1), itropo_upper(icol,2))
#else
        itropo_lower(icol,1) = minloc(play(icol,:), dim=1, mask=tropo(icol,:))
        itropo_upper(icol,1) = 1
        itropo_upper(icol,2) = maxloc(play(icol,:), dim=1, mask=(.not. tropo(icol,:)))
#endif
      end do
    else
      !$acc parallel loop
      !$omp target teams distribute parallel do simd
      do icol = 1,ncol
        itropo_lower(icol,1) = 1
#if ( defined(_CRAYFTN) && _RELEASE_MAJOR <= 14 ) || ( defined(_OPENMP) && defined(__NVCOMPILER) )
        itropo_upper(icol,2) = nlay
        call minmaxloc(icol, tropo, play, itropo_lower(icol,2), itropo_upper(icol,1))
#else
        itropo_lower(icol,2) = minloc(play(icol,:), dim=1, mask=tropo(icol,:))
        itropo_upper(icol,2) = nlay
        itropo_upper(icol,1) = maxloc(play(icol,:), dim=1, mask=(.not.tropo(icol,:)))
#endif
      end do
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
    idx_tropo = 1
    call gas_optical_depths_minor(     &
           ncol,nlay,ngpt,             & ! dimensions
           ngas,nflav,ntemp,neta,      &
           nminorlower,nminorklower,   &
           idx_h2o,idx_tropo,          &
           gpoint_flavor,              &
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
    idx_tropo = 2
    call gas_optical_depths_minor(     &
           ncol,nlay,ngpt,             & ! dimensions
           ngas,nflav,ntemp,neta,      &
           nminorupper,nminorkupper,   &
           idx_h2o,idx_tropo,          &
           gpoint_flavor,              &
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

    !$acc exit data delete(itropo_lower,itropo_upper)
    !$omp target exit data map(release:itropo_lower, itropo_upper)
    !$acc exit data delete(play, tlay, tropo, gpoint_flavor, jeta, jtemp, col_gas, fminor)
    !$omp target exit data map(release:play, tlay, tropo, gpoint_flavor, jeta, jtemp, col_gas, fminor)
    !$acc exit data copyout(tau)
    !$omp target exit data map(from:tau)

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
    real(wp) :: tau_major ! major species optical depth
    ! local index
    integer :: icol, ilay, iflav, igpt, itropo

    ! -----------------

    ! -----------------

    ! optical depth calculation for major species
    !$acc parallel loop collapse(2)
    !$omp target teams distribute parallel do simd collapse(2)
    do ilay = 1, nlay
      do icol = 1, ncol

        !$acc loop seq
        do igpt = 1, ngpt
          ! itropo = 1 lower atmosphere; itropo = 2 upper atmosphere
          itropo = merge(1,2,tropo(icol,ilay))  ! WS: moved inside innermost loop

          ! binary species parameter (eta) and col_mix depend on band flavor
          iflav = gpoint_flavor(itropo, igpt)
          tau_major = &
            ! interpolation in temperature, pressure, and eta
            interpolate3D(col_mix(:,icol,ilay,iflav), &
                          fmajor(:,:,:,icol,ilay,iflav), kmajor, &
                          igpt, jeta(:,icol,ilay,iflav), jtemp(icol,ilay),jpress(icol,ilay)+itropo)
          tau(icol,ilay,igpt) = tau(icol,ilay,igpt) + tau_major
        end do ! igpt

      end do
    end do ! ilay
  end subroutine gas_optical_depths_major

  ! ----------------------------------------------------------
  !
  ! compute minor species optical depths
  !
  subroutine gas_optical_depths_minor(ncol,nlay,ngpt,        &
                                      ngas,nflav,ntemp,neta, &
                                      nminor,nminork,        &
                                      idx_h2o,idx_tropo,     &
                                      gpt_flv,               &
                                      kminor,                &
                                      minor_limits_gpt,      &
                                      minor_scales_with_density,    &
                                      scale_by_complement,   &
                                      idx_minor, idx_minor_scaling, &
                                      kminor_start,        &
                                      play, tlay,          &
                                      col_gas,fminor,jeta, &
                                      layer_limits,jtemp,  &
                                      tau)
    integer,                                     intent(in   ) :: ncol,nlay,ngpt
    integer,                                     intent(in   ) :: ngas,nflav
    integer,                                     intent(in   ) :: ntemp,neta,nminor,nminork
    integer,                                     intent(in   ) :: idx_h2o, idx_tropo
    integer,     dimension(2, ngpt),             intent(in   ) :: gpt_flv
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
    real(wp) :: scaling, kminor_loc, tau_minor ! minor species absorption coefficient, optical depth
    integer  :: icol, ilay, iflav, igpt, imnr
    integer  :: minor_start, minor_loc, extent

    real(wp) :: myplay, mytlay, mycol_gas_h2o, mycol_gas_imnr, mycol_gas_0
    real(wp) :: myfminor(2,2)
    integer  :: myjtemp, myjeta(2)
    ! -----------------

    extent = size(scale_by_complement,dim=1)

    !$acc parallel loop gang vector collapse(2)
    !$omp target teams distribute parallel do simd collapse(2)
    do ilay = 1 , nlay
      do icol = 1, ncol
        !
        ! This check skips individual columns with no pressures in range
        !
        if ( layer_limits(icol,1) <= 0 .or. ilay < layer_limits(icol,1) .or. ilay > layer_limits(icol,2) ) cycle

        myplay  = play (icol,ilay)
        mytlay  = tlay (icol,ilay)
        myjtemp = jtemp(icol,ilay)
        mycol_gas_h2o = col_gas(icol,ilay,idx_h2o)
        mycol_gas_0   = col_gas(icol,ilay,0)

        !$acc loop seq
        do imnr = 1, extent
          ! What is the starting point in the stored array of minor absorption coefficients?
          minor_start = kminor_start(imnr)

          !$acc loop seq
          do igpt = minor_limits_gpt(1,imnr), minor_limits_gpt(2,imnr)

            scaling = col_gas(icol,ilay,idx_minor(imnr))
            if (minor_scales_with_density(imnr)) then
              !
              ! NOTE: P needed in hPa to properly handle density scaling.
              !
              scaling = scaling * (PaTohPa * myplay/mytlay)

              if(idx_minor_scaling(imnr) > 0) then  ! there is a second gas that affects this gas's absorption
                mycol_gas_imnr = col_gas(icol,ilay,idx_minor_scaling(imnr))
                vmr_fact = 1._wp / mycol_gas_0
                dry_fact = 1._wp / (1._wp + mycol_gas_h2o * vmr_fact)
                ! scale by density of special gas
                if (scale_by_complement(imnr)) then ! scale by densities of all gases but the special one
                  scaling = scaling * (1._wp - mycol_gas_imnr * vmr_fact * dry_fact)
                else
                  scaling = scaling *         (mycol_gas_imnr * vmr_fact * dry_fact)
                endif
              endif
            endif

            !
            ! Interpolation of absorption coefficient and calculation of optical depth
            !
            tau_minor = 0._wp
            iflav = gpt_flv(idx_tropo,igpt) ! eta interpolation depends on flavor
            minor_loc = minor_start + (igpt - minor_limits_gpt(1,imnr)) ! add offset to starting point
            kminor_loc = interpolate2D(fminor(:,:,icol,ilay,iflav), kminor, minor_loc, &
                                        jeta(:,icol,ilay,iflav), myjtemp)
            tau_minor = kminor_loc * scaling
            tau(icol,ilay,igpt) = tau(icol,ilay,igpt) + tau_minor
          enddo
        enddo

      enddo
    enddo

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
    integer,                                     intent(in ) :: ngas,nflav,neta,npres,ntemp
    integer,     dimension(2,ngpt),              intent(in ) :: gpoint_flavor
    integer,     dimension(2,nbnd),              intent(in ) :: band_lims_gpt ! start and end g-point for each band
    real(wp),    dimension(ntemp,neta,ngpt,2),   intent(in ) :: krayl
    integer,                                     intent(in ) :: idx_h2o
    real(wp),    dimension(ncol,nlay),           intent(in ) :: col_dry
    real(wp),    dimension(ncol,nlay,0:ngas),    intent(in ) :: col_gas
    real(wp),    dimension(2,2,ncol,nlay,nflav), intent(in ) :: fminor
    integer,     dimension(2,  ncol,nlay,nflav), intent(in ) :: jeta
    logical(wl), dimension(ncol,nlay),           intent(in ) :: tropo
    integer,     dimension(ncol,nlay),           intent(in ) :: jtemp
    ! outputs
    real(wp),    dimension(ncol,nlay,ngpt),      intent(out) :: tau_rayleigh
    ! -----------------
    ! local variables
    real(wp) :: k ! rayleigh scattering coefficient
    integer  :: icol, ilay, iflav, igpt
    integer  :: itropo
    ! -----------------

    !$acc parallel loop collapse(2)
    !$omp target teams distribute parallel do simd collapse(2)
    do ilay = 1, nlay
      do icol = 1, ncol
        !$acc loop seq
        do igpt = 1, ngpt
          itropo = merge(1,2,tropo(icol,ilay)) ! itropo = 1 lower atmosphere; itropo = 2 upper atmosphere
          iflav = gpoint_flavor(itropo, igpt)
          k = interpolate2D(fminor(:,:,icol,ilay,iflav), &
                            krayl(:,:,:,itropo),      &
                            igpt, jeta(:,icol,ilay,iflav), jtemp(icol,ilay))
          tau_rayleigh(icol,ilay,igpt) =  k * (col_gas(icol,ilay,idx_h2o)+col_dry(icol,ilay))
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
    integer,                                    intent(in) :: nflav, neta, npres, ntemp, nPlanckTemp
    real(wp),    dimension(ncol,nlay  ),        intent(in) :: tlay
    real(wp),    dimension(ncol,nlay+1),        intent(in) :: tlev
    real(wp),    dimension(ncol       ),        intent(in) :: tsfc
    integer,                                    intent(in) :: sfc_lay
    ! Interpolation variables
    real(wp),    dimension(2,2,2,ncol,nlay,nflav), intent(in) :: fmajor
    integer,     dimension(2,    ncol,nlay,nflav), intent(in) :: jeta
    logical(wl), dimension(            ncol,nlay), intent(in) :: tropo
    integer,     dimension(            ncol,nlay), intent(in) :: jtemp, jpress
    ! Table-specific
    integer, dimension(ngpt),                     intent(in) :: gpoint_bands ! start and end g-point for each band
    integer, dimension(2, nbnd),                  intent(in) :: band_lims_gpt ! start and end g-point for each band
    real(wp),                                     intent(in) :: temp_ref_min, totplnk_delta
    real(wp), dimension(ntemp,neta,npres+1,ngpt), intent(in) :: pfracin
    real(wp), dimension(nPlanckTemp,nbnd),        intent(in) :: totplnk
    integer,  dimension(2,ngpt),                  intent(in) :: gpoint_flavor

    real(wp), dimension(ncol,       ngpt), intent(out) :: sfc_src
    real(wp), dimension(ncol,nlay,  ngpt), intent(out) :: lay_src
    real(wp), dimension(ncol,nlay+1,ngpt), intent(out) :: lev_src
    real(wp), dimension(ncol,       ngpt), intent(out) :: sfc_source_Jac
    ! -----------------
    ! local
    real(wp), parameter                             :: delta_Tsurf = 1.0_wp

    integer  :: ilay, icol, igpt, ibnd, itropo, iflav
    integer  :: gptS, gptE
    real(wp), dimension(2), parameter :: one = [1._wp, 1._wp]
    real(wp) :: pfrac, pfrac_m1 ! Planck fraction in this layer and the one below 
    real(wp) :: planck_function_1, planck_function_2
    ! -----------------

    !$acc        data copyin(   tlay,tlev,tsfc,fmajor,jeta,tropo,jtemp,jpress,gpoint_bands,pfracin,totplnk,gpoint_flavor) &
    !$acc             copyout(  sfc_src,lay_src,lev_src,sfc_source_Jac)
    !$omp target data map(   to:tlay,tlev,tsfc,fmajor,jeta,tropo,jtemp,jpress,gpoint_bands,pfracin,totplnk,gpoint_flavor) &
    !$omp             map(from: sfc_src,lay_src,lev_src,sfc_source_Jac)

    ! Calculation of fraction of band's Planck irradiance associated with each g-point
    !$acc parallel loop tile(128,2)
    !$omp target teams distribute parallel do simd collapse(2)
    do ilay = 1, nlay
      do icol = 1, ncol

        !$acc loop seq
        do igpt = 1, ngpt
          ibnd = gpoint_bands(igpt)
          ! itropo = 1 lower atmosphere; itropo = 2 upper atmosphere
          itropo = merge(1,2,tropo(icol,ilay))  !WS moved itropo inside loop for GPU
          iflav  = gpoint_flavor(itropo, igpt) !eta interpolation depends on band's flavor
          ! interpolation in temperature, pressure, and eta
          pfrac = &
            interpolate3D(one, fmajor(:,:,:,icol,ilay,iflav), pfracin, &
                          igpt, jeta(:,icol,ilay,iflav), jtemp(icol,ilay),jpress(icol,ilay)+itropo)

          ! Compute layer source irradiance for g-point, equals band irradiance x fraction for g-point
          planck_function_1 = interpolate1D(tlay(icol,ilay), temp_ref_min, totplnk_delta, totplnk(:,ibnd))
          lay_src  (icol,ilay,igpt) = pfrac * planck_function_1

          ! Compute level source irradiance for g-point
          planck_function_1 = interpolate1D(tlev(icol,ilay), temp_ref_min, totplnk_delta, totplnk(:,ibnd))
          if (ilay == 1) then 
            lev_src(icol,ilay,  igpt) = pfrac * planck_function_1
          else
            itropo = merge(1,2,tropo(icol,ilay-1))  !WS moved itropo inside loop for GPU
            iflav  = gpoint_flavor(itropo, igpt) !eta interpolation depends on band's flavor
            pfrac_m1 = &
              interpolate3D(one, fmajor(:,:,:,icol,ilay-1,iflav), pfracin, &
                            igpt, jeta(:,icol,ilay-1,iflav), jtemp(icol,ilay-1),jpress(icol,ilay-1)+itropo)
            lev_src(icol,ilay,  igpt) = sqrt(pfrac * pfrac_m1) * planck_function_1
          end if 
          if (ilay == nlay) then
            planck_function_1 = interpolate1D(tlev(icol,nlay+1), temp_ref_min, totplnk_delta, totplnk(:,ibnd))
            lev_src(icol,nlay+1,igpt) = pfrac * planck_function_1
          end if
          if (ilay == sfc_lay) then
            planck_function_1 = interpolate1D(tsfc(icol)              , temp_ref_min, totplnk_delta, totplnk(:,ibnd))
            planck_function_2 = interpolate1D(tsfc(icol) + delta_Tsurf, temp_ref_min, totplnk_delta, totplnk(:,ibnd))
            sfc_src       (icol,igpt) = pfrac * planck_function_1
            sfc_source_Jac(icol,igpt) = pfrac * (planck_function_2 - planck_function_1)
          end if
        end do ! igpt
      end do ! icol
    end do ! ilay
    !$acc end        data
    !$omp end target data
  end subroutine compute_Planck_source
  ! ----------------------------------------------------------
  !
  ! One dimensional interpolation
  !
  function interpolate1D(val, offset, delta, table) result(res)
    !$acc routine seq
    !$omp declare target
    ! input
    real(wp), intent(in) :: val,    & ! axis value at which to evaluate table
                            offset, & ! minimum of table axis
                            delta     ! step size of table axis
    real(wp), dimension(:), &
              intent(in) :: table ! dimensions (axis, values)
    ! output
    real(wp)             :: res

    ! local
    real(wp) :: val0 ! fraction index adjusted by offset and delta
    integer :: index ! index term
    real(wp) :: frac ! fractional term
    ! -------------------------------------
    val0 = (val - offset) / delta
    frac = val0 - int(val0) ! get fractional part
    index = min(size(table,dim=1)-1, max(1, int(val0)+1)) ! limit the index range
    res = table(index) + frac * (table(index+1) - table(index))
  end function interpolate1D
  ! ------------
  !   This function returns a single value from a subset (in gpoint) of the k table
  !
  function interpolate2D(fminor, k, igpt, jeta, jtemp) result(res)
  !$acc routine seq
  !$omp declare target
    real(wp), dimension(2,2), intent(in) :: fminor ! interpolation fractions for minor species
                                       ! index(1) : reference eta level (temperature dependent)
                                       ! index(2) : reference temperature level
    real(wp), dimension(:,:,:), intent(in) :: k ! (g-point, eta, temp)
    integer,                    intent(in) :: igpt, jtemp ! interpolation index for temperature
    integer, dimension(2),      intent(in) :: jeta ! interpolation index for binary species parameter (eta)
    real(wp)                             :: res ! the result

    res =  &
      fminor(1,1) * k(jtemp  , jeta(1)  , igpt) + &
      fminor(2,1) * k(jtemp  , jeta(1)+1, igpt) + &
      fminor(1,2) * k(jtemp+1, jeta(2)  , igpt) + &
      fminor(2,2) * k(jtemp+1, jeta(2)+1, igpt)
  end function interpolate2D

  ! ----------------------------------------------------------
  ! interpolation in temperature, pressure, and eta
  function interpolate3D(scaling, fmajor, k, igpt, jeta, jtemp, jpress) result(res)
  !$acc routine seq
  !$omp declare target
    real(wp), dimension(2),     intent(in) :: scaling
    real(wp), dimension(2,2,2), intent(in) :: fmajor ! interpolation fractions for major species
                                                     ! index(1) : reference eta level (temperature dependent)
                                                     ! index(2) : reference pressure level
                                                     ! index(3) : reference temperature level
    real(wp), dimension(:,:,:,:),intent(in) :: k ! (gpt, eta,temp,press)
    integer,                     intent(in) :: igpt
    integer, dimension(2),       intent(in) :: jeta ! interpolation index for binary species parameter (eta)
    integer,                     intent(in) :: jtemp ! interpolation index for temperature
    integer,                     intent(in) :: jpress ! interpolation index for pressure
    real(wp)                                :: res ! the result
    ! each code block is for a different reference temperature
    res =  &
      scaling(1) * &
      ( fmajor(1,1,1) * k(jtemp, jeta(1)  , jpress-1, igpt  ) + &
        fmajor(2,1,1) * k(jtemp, jeta(1)+1, jpress-1, igpt  ) + &
        fmajor(1,2,1) * k(jtemp, jeta(1)  , jpress  , igpt  ) + &
        fmajor(2,2,1) * k(jtemp, jeta(1)+1, jpress  , igpt  ) ) + &
      scaling(2) * &
      ( fmajor(1,1,2) * k(jtemp+1, jeta(2)  , jpress-1, igpt) + &
        fmajor(2,1,2) * k(jtemp+1, jeta(2)+1, jpress-1, igpt) + &
        fmajor(1,2,2) * k(jtemp+1, jeta(2)  , jpress  , igpt) + &
        fmajor(2,2,2) * k(jtemp+1, jeta(2)+1, jpress  , igpt) )
  end function interpolate3D
  ! ----------------------------------------------------------
  !
  ! In-house subroutine for handling minloc and maxloc for
  ! compilers which do not support GPU versions
  !
  subroutine minmaxloc(i, mask, a, minl, maxl)
    implicit none
    !$acc routine seq
    !$omp declare target
    integer :: i, minl, maxl
    logical(wl) :: mask(:,:)
    real(wp) :: a(:,:)
    integer :: j, n
    real(wp) :: aij, amax, amin
    n = size(a,2)
    amax = -huge(amax)
    amin = huge(amin)
    do j = 1, n
      aij = a(i,j)
      if (mask(i,j)) then
        if (aij.lt.amin) then
          amin = aij
          minl = j
        end if
      else
        if (aij.gt.amax) then
          amax = aij
          maxl = j
        end if
      end if
    end do
  end subroutine
  ! ----------------------------------------------------------

end module mo_gas_optics_rrtmgp_kernels
