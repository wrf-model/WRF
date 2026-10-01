! This code is part of Radiative Transfer for Energetics (RTE)
!
! Contacts: Robert Pincus and Eli Mlawer
! email:  rrtmgp@aer.com
!
! Copyright 2015-2021,  Atmospheric and Environmental Research,
! Regents of the University of Colorado, Trustees of Columbia University.  All right reserved.
!
! Use and duplication is permitted under the terms of the
!    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause
! -------------------------------------------------------------------------------------------------
!
! Numeric calculations for radiative transfer solvers.
!   Emission/absorption (no-scattering) calculations
!     solver for multi-angle Gaussian quadrature
!     solver for a single angle, calling
!       source function computation (linear-in-tau)
!       transport
!   Extinction-only calculation (direct solar beam)
!   Two-stream calculations
!     solvers for LW and SW with different boundary conditions and source functions
!       source function calculation for LW, SW
!       two-stream calculations for LW, SW (using different assumtions about phase function)
!       transport (adding)
!   Application of boundary conditions
!
! -------------------------------------------------------------------------------------------------
module mo_rte_solver_kernels
  use,  intrinsic :: iso_c_binding
  use mo_rte_kind,       only: wp, wl
  use mo_rte_util_array, only: zero_array
  implicit none
  private

  public :: lw_solver_noscat, lw_solver_2stream, &
            sw_solver_noscat, sw_solver_2stream

  interface add_arrays
    module procedure add_arrays_2D, add_arrays_3D
  end interface

  real(wp), parameter :: pi = acos(-1._wp)
contains
  ! -------------------------------------------------------------------------------------------------
  !
  ! Top-level longwave kernels
  !
  ! -------------------------------------------------------------------------------------------------
  !
  ! LW fluxes, no scattering, mu (cosine of integration angle) specified by column
  !   Does radiation calculation at user-supplied angles; converts radiances to flux
  !   using user-supplied weights
  !
  ! ---------------------------------------------------------------
  subroutine lw_solver_noscat_oneangle(ncol, nlay, ngpt, top_at_1, D, weight, &
                              tau, lay_source, lev_source, sfc_emis, sfc_src, &
                              incident_flux,    &
                              flux_up, flux_dn, &
                              do_broadband, broadband_up, broadband_dn, &
                              do_Jacobians, sfc_srcJac, flux_upJac,               &
                              do_rescaling, ssa, g)
    integer,                               intent(in   ) :: ncol, nlay, ngpt ! Number of columns, layers, g-points
    logical(wl),                           intent(in   ) :: top_at_1
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: D            ! secant of propagation angle  []
    real(wp),                              intent(in   ) :: weight       ! quadrature weight
    real(wp), dimension(ncol,nlay,  ngpt), intent(in   ) :: tau          ! Absorption optical thickness []
    real(wp), dimension(ncol,nlay,  ngpt), intent(in   ) :: lay_source   ! Planck source at layer average temperature [W/m2]
    ! Planck source at layer edge for radiation in increasing/decreasing ilay direction
    ! lev_source_dec applies the mapping in layer i to the Planck function at layer i
    ! lev_source_inc applies the mapping in layer i to the Planck function at layer i+1
    real(wp), dimension(ncol,nlay+1,ngpt), target, &
                                           intent(in   ) :: lev_source
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_emis     ! Surface emissivity      []
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_src      ! Surface source function [W/m2]
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: incident_flux! Boundary condition for flux [W/m2]
    real(wp), dimension(ncol,nlay+1,ngpt), target, &
                                           intent(  out) :: flux_up, flux_dn
                                                                         ! Fluxes [W/m2]

    !
    ! Optional variables - arrays aren't referenced if corresponding logical  == False
    !
    logical(wl),                           intent(in   ) :: do_broadband
    real(wp), dimension(ncol,nlay+1     ), intent(  out) :: broadband_up, broadband_dn ! Spectrally-integrated fluxes [W/m2]
    logical(wl),                           intent(in   ) :: do_Jacobians
    real(wp), dimension(ncol       ,ngpt), intent(in   ) :: sfc_srcJac    ! surface temperature Jacobian of surface source function [W/m2/K]
    real(wp), dimension(ncol,nlay+1     ), intent(  out) :: flux_upJac    ! surface temperature Jacobian of Radiances [W/m2-str / K]
    logical(wl),                           intent(in   ) :: do_rescaling
    real(wp), dimension(ncol,nlay  ,ngpt), intent(in   ) :: ssa, g    ! single-scattering albedo, asymmetry parameter
    ! -------------------------------------------------------------------------------------------------
    ! Local variables
    integer :: icol, ilay, ilev, igpt
    integer :: top_level, sfc_level
    real(wp), dimension(ncol,nlay,ngpt) :: tau_loc, &  ! path length (tau/mu)
                                           trans       ! transmissivity  = exp(-tau)
    real(wp), dimension(ncol,nlay,ngpt) :: source_dn, source_up

    real(wp), parameter :: pi = acos(-1._wp)

    !
    ! For Jacobians
    !
    real(wp), dimension(ncol,nlay+1,ngpt) :: gpt_Jac
    !
    ! Used when approximating scattering
    !
    real(wp)                            :: ssal, wb, scaleTau, scaling
    real(wp), dimension(ncol,nlay,ngpt) :: An, Cn
    ! ------------------------------------
    ! Which way is up?
    ! Level Planck sources for upward and downward radiation
    if(top_at_1) then
      top_level = 1
      sfc_level = nlay+1
    else
      top_level = nlay+1
      sfc_level = 1
    end if

    !$acc        data create(   tau_loc,trans,source_dn,source_up ) &
    !$acc             copyin(   D, tau, lev_source)
    !$omp target data map(alloc:tau_loc,trans,source_dn,source_up ) &
    !$omp             map(to:   D, tau, lev_source)

    !$acc        enter data create(   flux_dn,flux_up)
    !$omp target enter data map(alloc:flux_dn,flux_up)

    !$acc                         parallel loop    collapse(2)
    !$omp target teams distribute parallel do simd collapse(2)
    do igpt = 1, ngpt
      do icol = 1, ncol
        !
        ! Transport is for intensity
        !   convert flux at top of domain to intensity assuming azimuthal isotropy
        !
        flux_dn(icol,top_level,igpt) = incident_flux(icol,igpt)/(pi * weight)
      end do
    end do

    !$acc        data create(   An, Cn)  copyin(g)          if(do_rescaling)
    !$omp target data map(alloc:An, Cn)  map(to:g)          if(do_rescaling)
    !$acc        data copyin(sfc_srcJac) create(   gpt_Jac) if(do_Jacobians)
    !$omp target data map(to:sfc_srcJac) map(alloc:gpt_Jac) if(do_Jacobians)

    !$acc parallel loop no_create(An, Cn, gpt_Jac, g) collapse(3)
    !$omp target teams distribute parallel do simd collapse(3)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          !
          ! The wb and scaleTau terms are independent of propagation
          !   angle D and could be pre-computed if several values of D are used
          ! We re-compute them here to keep not have to localize memory use
          !
          if(do_rescaling) then
            ssal = ssa(icol, ilay, igpt)
            wb = ssal*(1._wp - g(icol, ilay, igpt)) * 0.5_wp
            scaleTau = (1._wp - ssal + wb)
            ! here wb/scaleTau is parameter wb/(1-w(1-b)) of Eq.21 of the Tang paper
            ! actually it is in line of parameter rescaling defined in Eq.7
            ! potentialy if g=ssa=1  then  wb/scaleTau = NaN
            ! it should not happen because g is never 1 in atmospheres
            ! explanation of factor 0.4 note A of Table
            Cn(icol,ilay,igpt) = 0.4_wp*wb/scaleTau
            ! Eq.15 of the paper, multiplied by path length
            tau_loc(icol,ilay,igpt) = tau(icol,ilay,igpt)*D(icol,igpt)*scaleTau
            trans  (icol,ilay,igpt) = exp(-tau_loc(icol,ilay,igpt))
            An     (icol,ilay,igpt) = (1._wp-trans(icol,ilay,igpt)**2)
          else
            !
            ! Optical path and transmission, used in source function and transport calculations
            !
            tau_loc(icol,ilay,igpt) = tau(icol,ilay,igpt)*D(icol,igpt)
            trans  (icol,ilay,igpt) = exp(-tau_loc(icol,ilay,igpt))
          end if
          call lw_source_noscat(top_at_1, & 
                                lay_source(icol,ilay,igpt), lev_source(icol,ilay,igpt), & 
                                lev_source(icol,ilay+1,igpt),                           &
                                tau_loc   (icol,ilay,igpt), trans     (icol,ilay,igpt), &
                                source_dn (icol,ilay,igpt), source_up (icol,ilay,igpt))
        end do
      end do
    end do
    !
    ! Transport down
    !
    call lw_transport_noscat_dn(ncol, nlay, ngpt, top_at_1, trans, source_dn, flux_dn)
    !
    ! Surface reflection and emission
    !
    !$acc                         parallel loop    collapse(2) no_create(gpt_Jac, sfc_srcJac)
    !$omp target teams distribute parallel do simd collapse(2)
    do igpt = 1, ngpt
      do icol = 1, ncol
        !
        ! Surface albedo, surface source function
        !
        flux_up  (icol,sfc_level,igpt) = flux_dn   (icol,sfc_level,igpt)*(1._wp - sfc_emis(icol,igpt)) + &
                                         sfc_src   (icol,          igpt)*         sfc_emis(icol,igpt)
        if(do_Jacobians) &
          gpt_Jac(icol,sfc_level,igpt) = sfc_srcJac(icol,          igpt)*         sfc_emis(icol,igpt)
      end do
    end do
    !
    ! Transport up, or up and down again if using rescaling
    !
    if(do_rescaling) then
      call lw_transport_1rescl(ncol, nlay, ngpt, top_at_1, trans, &
                               source_dn, source_up,              &
                               flux_up, flux_dn, An, Cn,          &
                               do_Jacobians, gpt_Jac)
    else
      call lw_transport_noscat_up(ncol, nlay, ngpt, top_at_1, trans, source_up, flux_up, &
                                  do_Jacobians, gpt_Jac)
    end if

    if(do_broadband) then
      !
      ! Broadband reduction including
      !   conversion from intensity to flux assuming azimuthal isotropy and quadrature weight
      !
      call sum_broadband_factor(ncol, nlay+1, ngpt, pi * weight, flux_dn, broadband_dn)
      call sum_broadband_factor(ncol, nlay+1, ngpt, pi * weight, flux_up, broadband_up)
      !$acc        exit data delete(     flux_dn,flux_up)
      !$omp target exit data map(release:flux_dn,flux_up)
    else
      !
      ! Convert intensity to flux assuming azimuthal isotropy and quadrature weight
      !
      call apply_factor_3D(ncol, nlay+1, ngpt, pi * weight, flux_dn)
      call apply_factor_3D(ncol, nlay+1, ngpt, pi * weight, flux_up)
      !$acc        exit data copyout( flux_dn,flux_up)
      !$omp target exit data map(from:flux_dn,flux_up)
    end if
    !
    ! Only broadband-integrated Jacobians are provided
    !
    if (do_Jacobians) then
      call sum_broadband_factor(ncol, nlay+1, ngpt, pi * weight, gpt_Jac, flux_upJac)
    end if

    !$acc        end data
    !$omp end target data
    !$acc        end data
    !$omp end target data
    !$acc        end data
    !$omp end target data
  end subroutine lw_solver_noscat_oneangle
  ! ---------------------------------------------------------------
  !
  ! LW transport, no scattering, multi-angle quadrature
  !   Users provide a set of weights and quadrature angles
  !   Routine sums over single-angle solutions for each sets of angles/weights
  !
  ! ---------------------------------------------------------------
  subroutine lw_solver_noscat(ncol, nlay, ngpt, top_at_1, &
                                        nmus, Ds, weights,          &
                                        tau,                        &
                                        lay_source, lev_source,     &
                                        sfc_emis, sfc_src,          &
                                        inc_flux,                   &
                                        flux_up, flux_dn,           &
                                        do_broadband, broadband_up, broadband_dn, &
                                        do_Jacobians, sfc_srcJac, flux_upJac,               &
                                        do_rescaling, ssa, g) bind(C, name="rte_lw_solver_noscat")
    integer,                               intent(in   ) :: ncol, nlay, ngpt ! Number of columns, layers, g-points
    logical(wl),                           intent(in   ) :: top_at_1
    integer,                               intent(in   ) :: nmus       ! number of quadrature angles
    real(wp), dimension (ncol,      ngpt, &
                                    nmus), intent(in   ) :: Ds
    real(wp), dimension(nmus),             intent(in   ) :: weights    ! quadrature secants, weights
    real(wp), dimension(ncol,nlay,  ngpt), intent(in   ) :: tau        ! Absorption optical thickness []
    real(wp), dimension(ncol,nlay,  ngpt), intent(in   ) :: lay_source ! Planck source at layer average temperature [W/m2]
    real(wp), dimension(ncol,nlay+1,ngpt), intent(in   ) :: lev_source ! Planck source at layer edge [W/m2]
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_emis   ! Surface emissivity      []
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_src    ! Surface source function [W/m2]
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: inc_flux   ! Incident diffuse flux, probably 0 [W/m2]
    real(wp), dimension(ncol,nlay+1,ngpt), target, &
                                           intent(  out) :: flux_up, flux_dn ! Fluxes [W/m2]
    !
    ! Optional variables - arrays aren't referenced if corresponding logical  == False
    !
    logical(wl),                           intent(in   ) :: do_broadband
    real(wp), dimension(ncol,nlay+1     ), target, &
                                           intent(inout) :: broadband_up, broadband_dn
                                                            ! Spectrally-integrated fluxes [W/m2]
    logical(wl),                           intent(in   ) :: do_Jacobians
    real(wp), dimension(ncol       ,ngpt), intent(in   ) :: sfc_srcJac
                                                            ! surface temperature Jacobian of surface source function [W/m2/K]
    real(wp), dimension(ncol,nlay+1     ), target, &
                                           intent(  out) :: flux_upJac
                                                            ! surface temperature Jacobian of Radiances [W/m2-str / K]
    logical(wl),                           intent(in   ) :: do_rescaling
    real(wp), dimension(ncol,nlay  ,ngpt), intent(in   ) :: ssa, g    ! single-scattering albedo, asymmetry parameter
    ! ------------------------------------
    !
    ! Local variables
    !
    real(wp), dimension(:,:,:), pointer :: this_flux_up,      this_flux_dn
    real(wp), dimension(:,:),   pointer :: this_broadband_up, this_broadband_dn, this_flux_upJac
    integer :: icol, ilev, igpt, imu
    ! ------------------------------------

    !$acc        data copyin(Ds, tau, lay_source, lev_source, sfc_emis, sfc_src)
    !$omp target data map(to:Ds, tau, lay_source, lev_source, sfc_emis, sfc_src)
    !$acc        data copyout( flux_up, flux_dn)             if (.not. do_broadband)
    !$omp target data map(from:flux_up, flux_dn)             if (.not. do_broadband)
    !$acc        data copyout( broadband_up, broadband_dn)   if (      do_broadband)
    !$omp target data map(from:broadband_up, broadband_dn)   if (      do_broadband)
    !$acc        data copyin(sfc_srcJac)   copyout(flux_upJac) if (do_Jacobians)
    !$omp target data map(to:sfc_srcJac), map(from:flux_upJac) if (do_Jacobians)

    if(do_broadband) then
      this_broadband_up => broadband_up
      this_broadband_dn => broadband_dn
      allocate(this_flux_up(ncol, nlay+1, ngpt), this_flux_dn(ncol, nlay+1, ngpt))
    else
      this_flux_up => flux_up
      this_flux_dn => flux_dn
      ! Spectrally-integrated fluxes won't be filled in
      allocate(this_broadband_up(ncol,nlay+1), this_broadband_dn(ncol,nlay+1))
    end if

    !$acc        data create(   this_broadband_up, this_broadband_dn, this_flux_up, this_flux_dn)
    !$omp target data map(alloc:this_broadband_up, this_broadband_dn, this_flux_up, this_flux_dn)
    call lw_solver_noscat_oneangle(ncol, nlay, ngpt, &
                          top_at_1, Ds(:,:,1), weights(1), tau, &
                          lay_source, lev_source, sfc_emis, sfc_src, &
                          inc_flux,         &
                          this_flux_up, this_flux_dn, &
                          do_broadband, this_broadband_up, this_broadband_dn, &
                          do_Jacobians, sfc_srcJac, flux_upJac,     &
                          do_rescaling, ssa, g)
    !$acc end data
    !$omp end target data

    if(nmus > 1) then
      !
      ! For more than one angle use local arrays
      !
      if(do_broadband) then
        nullify( this_broadband_up,              this_broadband_dn)
        allocate(this_broadband_up(ncol,nlay+1), this_broadband_dn(ncol,nlay+1))
      else
        nullify( this_flux_up,                   this_flux_dn)
        allocate(this_flux_up(ncol,nlay+1,ngpt), this_flux_dn(ncol,nlay+1,ngpt))
      end if

      if(do_Jacobians) then
        allocate(this_flux_upJac(ncol,nlay+1))
      else
        this_flux_upJac => flux_upJac
      end if
      !
      ! For more than one angle use local arrays
      !
      !$acc        data create(   this_broadband_up, this_broadband_dn, this_flux_up, this_flux_dn)
      !$omp target data map(alloc:this_broadband_up, this_broadband_dn, this_flux_up, this_flux_dn)
      do imu = 2, nmus
        call lw_solver_noscat_oneangle(ncol, nlay, ngpt, &
                              top_at_1, Ds(:,:,imu), weights(imu), tau, &
                              lay_source, lev_source, sfc_emis, sfc_src, &
                              inc_flux,         &
                              this_flux_up,  this_flux_dn, &
                              do_broadband, this_broadband_up, this_broadband_dn, &
                              do_Jacobians, sfc_srcJac, this_flux_upJac,         &
                              do_rescaling, ssa, g)
        if(do_broadband) then
          call add_arrays(ncol, nlay+1, this_broadband_up, broadband_up)
          call add_arrays(ncol, nlay+1, this_broadband_dn, broadband_dn)
        else
          call add_arrays(ncol, nlay+1, ngpt, flux_up, this_flux_up)
          call add_arrays(ncol, nlay+1, ngpt, flux_dn, this_flux_dn)
        end if
        if (do_Jacobians) then
          call add_arrays(ncol, nlay+1, this_flux_upJac, flux_upJac)
        end if
      end do
      !$acc end data
      !$omp end target data
    end if

    !$acc end data
    !$omp end target data
    !$acc end data
    !$omp end target data
    !$acc end data
    !$omp end target data
    !$acc end data
    !$omp end target data

    ! Cleanup
    if (.not. associated(this_broadband_up, broadband_up)) then
      deallocate(this_broadband_up, this_broadband_dn)
    end if
    if (.not. associated(this_flux_up, flux_up)) then
      deallocate(this_flux_up, this_flux_dn)
    end if
    if (nmus > 1 .and. do_Jacobians) then
      deallocate(this_flux_upJac)
    end if
  end subroutine lw_solver_noscat
  ! -------------------------------------------------------------------------------------------------
  !
  ! Longwave two-stream calculation:
  !   combine RRTMGP-specific sources at levels
  !   compute layer reflectance, transmittance
  !   compute total source function at levels using linear-in-tau
  !   transport
  !
  ! -------------------------------------------------------------------------------------------------
  subroutine lw_solver_2stream (ncol, nlay, ngpt, top_at_1, &
                                tau, ssa, g,                &
                                lay_source, lev_source, sfc_emis, sfc_src, &
                                inc_flux,                   &
                                flux_up, flux_dn) bind(C, name="rte_lw_solver_2stream")
   integer,                               intent(in   ) :: ncol, nlay, ngpt ! Number of columns, layers, g-points
   logical(wl),                           intent(in   ) :: top_at_1
   real(wp), dimension(ncol,nlay,  ngpt), intent(in   ) :: tau, &     ! Optical thickness,
                                                           ssa, &     ! single-scattering albedo,
                                                           g          ! asymmetry parameter []
   real(wp), dimension(ncol,nlay,  ngpt), intent(in   ) :: lay_source ! Planck source at layer average temperature [W/m2]
   real(wp), dimension(ncol,nlay+1,ngpt), intent(in   ) :: lev_source ! Planck source at layer edge  [W/m2]
   real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_emis   ! Surface emissivity      []
   real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_src    ! Surface source function [W/m2]
   real(wp), dimension(ncol,       ngpt), intent(in   ) :: inc_flux   ! Incident diffuse flux, probably 0 [W/m2]
   real(wp), dimension(ncol,nlay+1,ngpt), intent(  out) :: flux_up, flux_dn ! Fluxes [W/m2]
    ! ----------------------------------------------------------------------
    integer :: icol, igpt, top_level
    real(wp), dimension(ncol,nlay  ,ngpt) :: Rdif, Tdif, gamma1, gamma2
    real(wp), dimension(ncol       ,ngpt) :: sfc_albedo
    real(wp), dimension(ncol,nlay  ,ngpt) :: source_dn, source_up
    real(wp), dimension(ncol       ,ngpt) :: source_sfc
    ! ------------------------------------
    ! ------------------------------------
    !$acc enter        data copyin(tau, ssa, g, lay_source, lev_source, sfc_emis, sfc_src, flux_dn)
    !$omp target enter data map(to:tau, ssa, g, lay_source, lev_source, sfc_emis, sfc_src, flux_dn)
    !$acc enter        data create(   flux_up, Rdif, Tdif, gamma1, gamma2, sfc_albedo, source_dn, source_up, source_sfc)
    !$omp target enter data map(alloc:flux_up, Rdif, Tdif, gamma1, gamma2, sfc_albedo, source_dn, source_up, source_sfc)
    !
    ! RRTMGP provides source functions at each level using the spectral mapping
    !   of each adjacent layer. Combine these for two-stream calculations
    !
    top_level = nlay+1
    if(top_at_1) top_level = 1
    !
    ! Cell properties: reflection, transmission for diffuse radiation
    !   Coupling coefficients needed for source function
    !
    call lw_two_stream(ncol, nlay, ngpt, &
                       tau , ssa, g,     &
                       gamma1, gamma2, Rdif, Tdif)
    !
    ! Source function for diffuse radiation
    !
    call lw_source_2str(ncol, nlay, ngpt, top_at_1, &
                        sfc_emis, sfc_src, &
                        lay_source, lev_source, &
                        gamma1, gamma2, Rdif, Tdif, tau, &
                        source_dn, source_up, source_sfc)

    !$acc                         parallel loop    collapse(2)
    !$omp target teams distribute parallel do simd collapse(2)
    do igpt = 1, ngpt
      do icol = 1, ncol
        sfc_albedo(icol,          igpt) = 1._wp - sfc_emis(icol,igpt)
        flux_dn   (icol,top_level,igpt) = inc_flux(icol,igpt)
      end do
    end do
    !
    ! Transport
    !
    call adding(ncol, nlay, ngpt, top_at_1,        &
                sfc_albedo,                        &
                Rdif, Tdif,                        &
                source_dn, source_up, source_sfc,  &
                flux_up, flux_dn)
    !$acc        exit data delete(     tau, ssa, g, lay_source, lev_source, sfc_emis, sfc_src)
    !$omp target exit data map(release:tau, ssa, g, lay_source, lev_source, sfc_emis, sfc_src)
    !$acc        exit data delete(     Rdif, Tdif, gamma1, gamma2, sfc_albedo, source_dn, source_up, source_sfc)
    !$omp target exit data map(release:Rdif, Tdif, gamma1, gamma2, sfc_albedo, source_dn, source_up, source_sfc)
    !$acc exit data copyout(flux_up, flux_dn)
    !$omp target exit data map(from:flux_up, flux_dn)
  end subroutine lw_solver_2stream
  ! -------------------------------------------------------------------------------------------------
  !
  !   Top-level shortwave kernels
  !
  ! -------------------------------------------------------------------------------------------------
  !
  !   Extinction-only i.e. solar direct beam
  !
  ! -------------------------------------------------------------------------------------------------
  subroutine sw_solver_noscat(ncol, nlay, ngpt, top_at_1, &
                              tau, mu0, inc_flux_dir, flux_dir) bind(C, name="rte_sw_solver_noscat")
    integer,                               intent(in ) :: ncol, nlay, ngpt ! Number of columns, layers, g-points
    logical(wl),                           intent(in ) :: top_at_1
    real(wp), dimension(ncol,nlay,  ngpt), intent(in ) :: tau          ! Absorption optical thickness []
    real(wp), dimension(ncol,nlay       ), intent(in ) :: mu0          ! cosine of solar zenith angle
    real(wp), dimension(ncol,       ngpt), intent(in ) :: inc_flux_dir ! Direct beam incident flux
    real(wp), dimension(ncol,nlay+1,ngpt), intent(out) :: flux_dir     ! Direct-beam flux, spectral [W/m2]

    integer :: icol, ilev, igpt
    ! ------------------------------------
    ! ------------------------------------
    !$acc enter data copyin(tau, mu0) create(flux_dir)
    !$omp target enter data map(to:tau, mu0) map(alloc:flux_dir)
    ! Indexing into arrays for upward and downward propagation depends on the vertical
    !   orientation of the arrays (whether the domain top is at the first or last index)
    ! We write the loops out explicitly so compilers will have no trouble optimizing them.

    ! Downward propagation
    if(top_at_1) then
      ! For the flux at this level, what was the previous level, and which layer has the
      !   radiation just passed through?
      ! layer index = level index - 1
      ! previous level is up (-1)
      !$acc parallel loop collapse(2)
      !$omp target teams distribute parallel do simd collapse(2)
      do igpt = 1, ngpt
        do icol = 1, ncol
          flux_dir(icol,    1,igpt) = inc_flux_dir(icol,   igpt) * mu0(icol, 1)
          do ilev = 2, nlay+1
            flux_dir(icol,ilev,igpt) = flux_dir(icol,ilev-1,igpt) * exp(-tau(icol,ilev,igpt)/mu0(icol, ilev-1))
          end do
        end do
      end do
    else
      ! layer index = level index
      ! previous level is up (+1)
      !$acc parallel loop collapse(2)
      !$omp target teams distribute parallel do simd collapse(2)
      do igpt = 1, ngpt
        do icol = 1, ncol
          flux_dir(icol,nlay+1,igpt) = inc_flux_dir(icol, igpt) * mu0(icol, nlay)
          do ilev = nlay, 1, -1
            flux_dir(icol,ilev,igpt) = flux_dir(icol,ilev+1,igpt) * exp(-tau(icol,ilev,igpt)/mu0(icol, ilev))
          end do
        end do
      end do
    end if
    !$acc exit data delete(tau, mu0) copyout(flux_dir)
    !$omp target exit data map(release:tau, mu0) map(from:flux_dir)
  end subroutine sw_solver_noscat
  ! -------------------------------------------------------------------------------------------------
  !
  ! Shortwave two-stream calculation:
  !   compute layer reflectance, transmittance
  !   compute solar source function for diffuse radiation
  !   transport
  !
  ! -------------------------------------------------------------------------------------------------
  subroutine sw_solver_2stream (ncol, nlay, ngpt, top_at_1,  &
                                tau, ssa, g, mu0,           &
                                sfc_alb_dir, sfc_alb_dif,   &
                                            inc_flux_dir,   &
                                flux_up, flux_dn, flux_dir, &
                                has_dif_bc, inc_flux_dif,   &
                                do_broadband, broadband_up, &
                                broadband_dn, broadband_dir) bind(C, name="rte_sw_solver_2stream")
    integer,                               intent(in ) :: ncol, nlay, ngpt ! Number of columns, layers, g-points
    logical(wl),                           intent(in ) :: top_at_1
    real(wp), dimension(ncol,nlay,  ngpt), intent(in ) :: tau, &  ! Optical thickness,
                                                          ssa, &  ! single-scattering albedo,
                                                          g       ! asymmetry parameter []
    real(wp), dimension(ncol,nlay       ), intent(in ) :: mu0     ! cosine of solar zenith angle
                                                            ! Spectral albedo of surface to direct and diffuse radiation
    real(wp), dimension(ncol,       ngpt), intent(in ) :: sfc_alb_dir, sfc_alb_dif, &
                                                          inc_flux_dir ! Direct beam incident flux
    real(wp), dimension(ncol,nlay+1,ngpt), target, &
                                           intent(out) :: flux_up, flux_dn, flux_dir! Fluxes [W/m2]
    logical(wl),                           intent(in ) :: has_dif_bc   ! Is a boundary condition for diffuse flux supplied?
    real(wp), dimension(ncol,       ngpt), intent(in ) :: inc_flux_dif ! Boundary condition for diffuse flux
    logical(wl),                           intent(in ) :: do_broadband ! Provide broadband-integrated, not spectrally-resolved, fluxes?
    real(wp), dimension(ncol,nlay+1     ), intent(out) :: broadband_up, broadband_dn, broadband_dir
    ! -------------------------------------------
    integer  :: icol, ilay, igpt, top_level, top_layer
    real(wp) :: bb_flux_s, bb_dir_s
    real(wp), dimension(ncol,nlay,ngpt) :: Rdif, Tdif
    real(wp), dimension(ncol,nlay,ngpt) :: source_up, source_dn
    real(wp), dimension(ncol     ,ngpt) :: source_srf
    real(wp), dimension(:,:,:), pointer :: gpt_flux_up, gpt_flux_dn, gpt_flux_dir
    ! ------------------------------------
    if(do_broadband) then
      allocate(gpt_flux_up (ncol,nlay+1,ngpt), &
               gpt_flux_dn (ncol,nlay+1,ngpt), &
               gpt_flux_dir(ncol,nlay+1,ngpt))
    else
      gpt_flux_up  => flux_up
      gpt_flux_dn  => flux_dn
      gpt_flux_dir => flux_dir
    end if
    if(top_at_1) then
      top_level = 1
      top_layer = 1
    else
      top_level = nlay+1
      top_layer = nlay
    end if
    !
    ! Boundary conditions direct beam...
    !
    !$acc        data create(   gpt_flux_up, gpt_flux_dn, gpt_flux_dir) &
    !$acc             copyin(   mu0)
    !$omp target data map(alloc:gpt_flux_up, gpt_flux_dn, gpt_flux_dir) &
    !$omp             map(to:   mu0)

    !$acc        data copyout(flux_up, flux_dn, flux_dir) if (.not. do_broadband)
    !$omp target data map(to: flux_up, flux_dn, flux_dir) if (.not. do_broadband)

    !$acc  parallel loop collapse(2)
    !$omp target teams distribute parallel do simd collapse(2)
    do igpt = 1, ngpt
      do icol = 1, ncol
        gpt_flux_dir(icol, top_level, igpt)  = inc_flux_dir(icol,igpt) * mu0(icol, top_layer)
      end do
    end do

    !
    ! ... and diffuse field, using 0 if no BC is provided
    !
    if(has_dif_bc) then
      !$acc                         parallel loop    collapse(2)
      !$omp target teams distribute parallel do simd collapse(2)
      do igpt = 1, ngpt
        do icol = 1, ncol
          gpt_flux_dn(icol, top_level, igpt)  = inc_flux_dif(icol,igpt)
        end do
      end do
    else
      !$acc                         parallel loop    collapse(2)
      !$omp target teams distribute parallel do simd collapse(2)
      do igpt = 1, ngpt
        do icol = 1, ncol
          gpt_flux_dn(icol, top_level, igpt)  = 0._wp
        end do
      end do
    end if
    !
    ! Cell properties: transmittance and reflectance for diffuse radiation
    ! Direct-beam radiation and source for diffuse radiation
    !
    !$acc        data create(   Rdif, Tdif, source_up, source_dn, source_srf)
    !$omp target data map(alloc:Rdif, Tdif, source_up, source_dn, source_srf)
    call sw_dif_and_source(ncol, nlay, ngpt, top_at_1, mu0, sfc_alb_dir, &
                           tau, ssa, g,                                  &
                           Rdif, Tdif, source_dn, source_up, source_srf, gpt_flux_dir)

    call adding(ncol, nlay, ngpt, top_at_1,   &
                sfc_alb_dif, Rdif, Tdif,      &
                source_dn, source_up, source_srf, gpt_flux_up, gpt_flux_dn)
    !$acc        end data
    !$omp end target data

    if(do_broadband) then
      !
      ! Broadband integration
      !
      !$acc        data copyout( broadband_up, broadband_dn, broadband_dir)
      !$omp target data map(from:broadband_up, broadband_dn, broadband_dir)
      call sum_broadband_factor(ncol, nlay+1, ngpt, 1._wp, gpt_flux_up,  broadband_up)
      call sum_broadband_factor(ncol, nlay+1, ngpt, 1._wp, gpt_flux_dn,  broadband_dn)
      call sum_broadband_factor(ncol, nlay+1, ngpt, 1._wp, gpt_flux_dir, broadband_dir)
      !
      ! adding computes only diffuse flux; flux_dn is total
      !
      call add_arrays          (ncol, nlay+1, broadband_dir, broadband_dn)
      !$acc        end data
      !$omp end target data
    else
      !
      ! adding computes only diffuse flux; flux_dn is total
      !
      call add_arrays(ncol, nlay+1, ngpt, flux_dir, flux_dn)
    end if

    !$acc        end data
    !$omp end target data
    !$acc        end data
    !$omp end target data

    if (do_broadband) then
      deallocate(gpt_flux_up, gpt_flux_dn, gpt_flux_dir)
    end if
  end subroutine sw_solver_2stream
  ! -------------------------------------------------------------------------------------------------
  !
  !   Lower-level longwave kernels
  !
  ! ---------------------------------------------------------------
  !
  ! Compute LW source function for upward and downward emission at levels using linear-in-tau assumption
  ! See Clough et al., 1992, doi: 10.1029/92JD01419, Eq 13
  ! This routine implements point-wise stencil, and has to be called in a loop
  !
  ! ---------------------------------------------------------------
  subroutine lw_source_noscat(top_at_1, lay_source, lev_source, levp1_source, tau, trans, &
                              source_dn, source_up)
    !$acc routine seq
    !$omp declare target
    !
    logical(wl), intent(in) :: top_at_1
    real(wp),    intent(in) :: lay_source,   & ! Planck source at layer center
                               lev_source,   & ! Planck source at levels (layer edges),
                               levp1_source, & ! Planck source at level +1 (layer edges),
                               tau,          & ! Optical path (tau/mu)
                               trans           ! Transmissivity (exp(-tau))
    real(wp), intent(inout):: source_dn, source_up
                                               ! Source function at layer edges
                                               ! Down at the bottom of the layer, up at the top
    ! --------------------------------
    real(wp), parameter  :: tau_thresh = sqrt(sqrt(epsilon(tau)))
    real(wp)             :: fact, source_inc, source_dec 
    ! ---------------------------------------------------------------
    !
    ! Weighting factor. Use 3rd order series expansion when rounding error (~tau^2)
    !   is of order epsilon (smallest difference from 1. in working precision)
    !   Thanks to Peter Blossey (UW) for the idea and Dmitry Alexeev (Nvidia) for suggesting 3rd order
    !
    if(tau > tau_thresh) then
      fact = (1._wp - trans)/tau - trans
    else
      fact = tau * (0.5_wp + tau * (-1._wp/3._wp + tau * 1._wp/8._wp) )
    end if
    !
    ! Equation below is developed in Clough et al., 1992, doi:10.1029/92JD01419, Eq 13
    !
    source_inc = (1._wp - trans) * levp1_source + 2._wp * fact * (lay_source - levp1_source)
    source_dec = (1._wp - trans) * lev_source   + 2._wp * fact * (lay_source - lev_source)
    if (top_at_1) then 
      source_dn = source_inc
      source_up = source_dec
    else
      source_up = source_inc
      source_dn = source_dec
    end if 
  end subroutine lw_source_noscat
  ! ---------------------------------------------------------------
  !
  ! Longwave no-scattering transport
  !
  ! ---------------------------------------------------------------
  subroutine lw_transport_noscat_dn(ncol, nlay, ngpt, top_at_1, &
                                    trans, source_dn,radn_dn)
    !dir$ optimize(-O0)
    integer,                               intent(in   ) :: ncol, nlay, ngpt ! Number of columns, layers, g-points
    logical(wl),                           intent(in   ) :: top_at_1   !
    real(wp), dimension(ncol,nlay  ,ngpt), intent(in   ) :: trans      ! transmissivity = exp(-tau)
    real(wp), dimension(ncol,nlay  ,ngpt), intent(in   ) :: source_dn  ! Diffuse radiation emitted by the layer
    real(wp), dimension(ncol,nlay+1,ngpt), intent(inout) :: radn_dn    ! Radiances [W/m2-str]
                                                                       ! Top level must contain incident flux boundary condition
    ! Local variables
    integer :: igpt, ilev, icol
    ! ---------------------------------------------------
    ! ---------------------------------------------------
    if(top_at_1) then
      !
      ! Top of domain is index 1
      !
      !$acc  parallel loop collapse(2)
      !$omp target teams distribute parallel do simd collapse(2)
      do igpt = 1, ngpt
        do icol = 1, ncol
          do ilev = 2, nlay+1
            radn_dn(icol,ilev,igpt) = trans(icol,ilev-1,igpt)*radn_dn(icol,ilev-1,igpt) + source_dn(icol,ilev-1,igpt)
          end do
        end do
      end do
    else
      !
      ! Top of domain is index nlay+1
      !
      !$acc  parallel loop collapse(2)
      !$omp target teams distribute parallel do simd collapse(2)
      do igpt = 1, ngpt
        do icol = 1, ncol
          do ilev = nlay, 1, -1
            radn_dn(icol,ilev,igpt) = trans(icol,ilev  ,igpt)*radn_dn(icol,ilev+1,igpt) + source_dn(icol,ilev,igpt)
          end do
        end do
      end do
    end if

  end subroutine lw_transport_noscat_dn
  ! -------------------------------------------------------------------------------------------------
  subroutine lw_transport_noscat_up(ncol, nlay, ngpt, &
                                    top_at_1, trans, source_up, radn_up, do_Jacobians, radn_upJac)
    !dir$ optimize(-O0)
    integer,                               intent(in   ) :: ncol, nlay, ngpt ! Number of columns, layers, g-points
    logical(wl),                           intent(in   ) :: top_at_1   !
    real(wp), dimension(ncol,nlay  ,ngpt), intent(in   ) :: trans      ! transmissivity = exp(-tau)
    real(wp), dimension(ncol,nlay  ,ngpt), intent(in   ) :: source_up  ! Diffuse radiation emitted by the layer
    real(wp), dimension(ncol,nlay+1,ngpt), intent(inout) :: radn_up    ! Radiances [W/m2-str]
    logical(wl),                           intent(in   ) :: do_Jacobians
    real(wp), dimension(ncol,nlay+1,ngpt), intent(inout) :: radn_upJac    ! surface temperature Jacobian of Radiances [W/m2-str / K]
    ! Local variables
    integer :: igpt, ilev, icol
    ! ---------------------------------------------------
    ! ---------------------------------------------------
    if(top_at_1) then
      !
      ! Top of domain is index 1
      !
      !$acc  parallel loop collapse(2) no_create(radn_upJac)
      !$omp target teams distribute parallel do simd collapse(2)
      do igpt = 1, ngpt
        do icol = 1, ncol
          do ilev = nlay, 1, -1
            radn_up     (icol,ilev,igpt) = trans(icol,ilev,igpt)*radn_up   (icol,ilev+1,igpt) + source_up(icol,ilev,igpt)
          end do
          if(do_Jacobians) then
            do ilev = nlay, 1, -1
              radn_upJac(icol,ilev,igpt) = trans(icol,ilev,igpt)*radn_upJac(icol,ilev+1,igpt)
            end do
          end if
        end do
      end do

    else
      !
      ! Top of domain is index nlay+1
      !
      !$acc  parallel loop collapse(2) no_create(radn_upJac)
      !$omp target teams distribute parallel do simd collapse(2)
      do igpt = 1, ngpt
        do icol = 1, ncol
          do ilev = 2, nlay+1
            radn_up     (icol,ilev,igpt) = trans(icol,ilev-1,igpt) * radn_up   (icol,ilev-1,igpt) +  source_up(icol,ilev-1,igpt)
          end do
          if(do_Jacobians) then
            do ilev = nlay, 1, -1
              radn_upJac(icol,ilev,igpt) = trans(icol,ilev-1,igpt) * radn_upJac(icol,ilev-1,igpt)
            end do
          end if
        end do
      end do
    end if

  end subroutine lw_transport_noscat_up
  ! -------------------------------------------------------------------------------------------------
  !
  ! Longwave two-stream solutions to diffuse reflectance and transmittance for a layer
  !    with optical depth tau, single scattering albedo w0, and asymmetery parameter g.
  !
  ! Equations are developed in Meador and Weaver, 1980,
  !    doi:10.1175/1520-0469(1980)037<0630:TSATRT>2.0.CO;2
  !
  subroutine lw_two_stream(ncol, nlay, ngpt, tau, w0, g, &
                                gamma1, gamma2, Rdif, Tdif)
    integer,                             intent(in)  :: ncol, nlay, ngpt
    real(wp), dimension(ncol,nlay,ngpt), intent(in)  :: tau, w0, g
    real(wp), dimension(ncol,nlay,ngpt), intent(out) :: gamma1, gamma2, Rdif, Tdif

    ! -----------------------
    integer  :: icol, ilay, igpt

    ! Variables used in Meador and Weaver
    real(wp) :: k

    ! Ancillary variables
    real(wp) :: RT_term
    real(wp) :: exp_minusktau, exp_minus2ktau

    real(wp), parameter :: LW_diff_sec = 1.66  ! 1./cos(diffusivity angle)
    ! ---------------------------------
    ! ---------------------------------
    !$acc enter data copyin(tau, w0, g)
    !$omp target enter data map(to:tau, w0, g)
    !$acc enter data create(gamma1, gamma2, Rdif, Tdif)
    !$omp target enter data map(alloc:gamma1, gamma2, Rdif, Tdif)

    !$acc  parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          !
          ! Coefficients differ from SW implementation because the phase function is more isotropic
          !   Here we follow Fu et al. 1997, doi:10.1175/1520-0469(1997)054<2799:MSPITI>2.0.CO;2
          !   and use a diffusivity sec of 1.66
          !
          gamma1(icol,ilay,igpt)= LW_diff_sec * (1._wp - 0.5_wp * w0(icol,ilay,igpt) * (1._wp + g(icol,ilay,igpt))) ! Fu et al. Eq 2.9
          gamma2(icol,ilay,igpt)= LW_diff_sec *          0.5_wp * w0(icol,ilay,igpt) * (1._wp - g(icol,ilay,igpt))  ! Fu et al. Eq 2.10

          ! Written to encourage vectorization of exponential, square root
          ! Eq 18;  k = SQRT(gamma1**2 - gamma2**2), limited below to avoid div by 0.
          !   k = 0 for isotropic, conservative scattering; this lower limit on k
          !   gives relative error with respect to conservative solution
          !   of < 0.1% in Rdif down to tau = 10^-9
          k = sqrt(max((gamma1(icol,ilay,igpt) - gamma2(icol,ilay,igpt)) * &
                       (gamma1(icol,ilay,igpt) + gamma2(icol,ilay,igpt)),  &
                       1.e-12_wp))
          exp_minusktau = exp(-tau(icol,ilay,igpt)*k)

          !
          ! Diffuse reflection and transmission
          !
          exp_minus2ktau = exp_minusktau * exp_minusktau

          ! Refactored to avoid rounding errors when k, gamma1 are of very different magnitudes
          RT_term = 1._wp / (k * (1._wp + exp_minus2ktau)  + &
                    gamma1(icol,ilay,igpt) * (1._wp - exp_minus2ktau) )

          ! Equation 25
          Rdif(icol,ilay,igpt) = RT_term * gamma2(icol,ilay,igpt) * (1._wp - exp_minus2ktau)

          ! Equation 26
          Tdif(icol,ilay,igpt) = RT_term * 2._wp * k * exp_minusktau
        end do
      end do
    end do
    !$acc exit data delete (tau, w0, g)
    !$omp target exit data map(release:tau, w0, g)
    !$acc exit data copyout(gamma1, gamma2, Rdif, Tdif)
    !$omp target exit data map(from:gamma1, gamma2, Rdif, Tdif)
  end subroutine lw_two_stream
  ! ---------------------------------------------------------------
  !
  ! Compute LW source function for upward and downward emission at levels using linear-in-tau assumption
  !   This version straight from ECRAD
  !   Source is provided as W/m2-str; factor of pi converts to flux units
  !
  ! ---------------------------------------------------------------
  subroutine lw_source_2str(ncol, nlay, ngpt, top_at_1,   &
                            sfc_emis, sfc_src,      &
                            lay_source, lev_source, &
                            gamma1, gamma2, rdif, tdif, tau, source_dn, source_up, source_sfc) &
                            bind (C, name="rte_lw_source_2str")
    integer,                         intent(in) :: ncol, nlay, ngpt
    logical(wl),                     intent(in) :: top_at_1
    real(wp), dimension(ncol      , ngpt), intent(in) :: sfc_emis, sfc_src
    real(wp), dimension(ncol, nlay, ngpt), intent(in) :: lay_source,    & ! Planck source at layer center
                                                   tau,           & ! Optical depth (tau)
                                                   gamma1, gamma2,& ! Coupling coefficients
                                                   rdif, tdif       ! Layer reflectance and transmittance
    real(wp), dimension(ncol, nlay+1, ngpt), target, &
                                     intent(in)  :: lev_source       ! Planck source at layer edges
    real(wp), dimension(ncol, nlay, ngpt), intent(out) :: source_dn, source_up
    real(wp), dimension(ncol      , ngpt), intent(out) :: source_sfc      ! Source function for upward radation at surface

    integer             :: icol, ilay, igpt
    real(wp)            :: Z, Zup_top, Zup_bottom, Zdn_top, Zdn_bottom
    real(wp)            :: lev_source_bot, lev_source_top
    ! ---------------------------------------------------------------
    ! ---------------------------------
    !$acc enter data copyin(sfc_emis, sfc_src, lay_source, tau, gamma1, gamma2, rdif, tdif, lev_source)
    !$omp target enter data map(to:sfc_emis, sfc_src, lay_source, tau, gamma1, gamma2, rdif, tdif, lev_source)
    !$acc enter data create(source_dn, source_up, source_sfc)
    !$omp target enter data map(alloc:source_dn, source_up, source_sfc)

    !$acc parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          if (tau(icol,ilay,igpt) > 1.0e-8_wp) then
            if(top_at_1) then
              lev_source_top = lev_source(icol,ilay  ,igpt)
              lev_source_bot = lev_source(icol,ilay+1,igpt)
            else
              lev_source_top = lev_source(icol,ilay+1,igpt)
              lev_source_bot = lev_source(icol,ilay  ,igpt)
            end if
            !
            ! Toon et al. (JGR 1989) Eqs 26-27
            !
            Z = (lev_source_bot-lev_source_top) / (tau(icol,ilay,igpt)*(gamma1(icol,ilay,igpt)+gamma2(icol,ilay,igpt)))
            Zup_top        =  Z + lev_source_top
            Zup_bottom     =  Z + lev_source_bot
            Zdn_top        = -Z + lev_source_top
            Zdn_bottom     = -Z + lev_source_bot
            source_up(icol,ilay,igpt) = pi * (Zup_top    - rdif(icol,ilay,igpt) * Zdn_top    - tdif(icol,ilay,igpt) * Zup_bottom)
            source_dn(icol,ilay,igpt) = pi * (Zdn_bottom - rdif(icol,ilay,igpt) * Zup_bottom - tdif(icol,ilay,igpt) * Zdn_top)
          else
            source_up(icol,ilay,igpt) = 0._wp
            source_dn(icol,ilay,igpt) = 0._wp
          end if
          if(ilay == 1) source_sfc(icol,igpt) = pi * sfc_emis(icol,igpt) * sfc_src(icol,igpt)
        end do
      end do
    end do
    !$acc exit data delete(sfc_emis, sfc_src, lay_source, tau, gamma1, gamma2, rdif, tdif, lev_source)
    !$omp target exit data map(release:sfc_emis, sfc_src, lay_source, tau, gamma1, gamma2, rdif, tdif, lev_source)
    !$acc exit data copyout(source_dn, source_up, source_sfc)
    !$omp target exit data map(from:source_dn, source_up, source_sfc)

  end subroutine lw_source_2str
  ! -------------------------------------------------------------------------------------------------
  !
  !   Lower-level shortwave kernels
  !
  ! -------------------------------------------------------------------------------------------------
  ! -------------------------------------------------------------------------------------------------
  !
  ! Two-stream solutions to direct and diffuse reflectance and transmittance for a layer
  !    with optical depth tau, single scattering albedo w0, and asymmetery parameter g.
  !
  ! Equations are developed in Meador and Weaver, 1980,
  !    doi:10.1175/1520-0469(1980)037<0630:TSATRT>2.0.CO;2
  !
  ! ---------------------------------------------------------------
  !
  ! Direct beam source for diffuse radiation in layers and at surface;
  !   report direct beam as a byproduct
  !
  ! -------------------------------------------------------------------------------------------------
  subroutine sw_dif_and_source(ncol, nlay, ngpt, top_at_1, mu0, sfc_albedo, &
                                tau, w0, g,                                      &
                                Rdif, Tdif, source_dn, source_up, source_sfc,    &
                                flux_dn_dir) bind (C, name="rte_sw_source_dir")
    integer,                               intent(in   ) :: ncol, nlay, ngpt
    logical(wl),                           intent(in   ) :: top_at_1
    real(wp), dimension(ncol,nlay       ), intent(in   ) :: mu0
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_albedo          ! surface albedo for direct radiation
    real(wp), dimension(ncol,nlay,  ngpt), intent(in   ) :: tau, w0, g
    real(wp), dimension(ncol,nlay,  ngpt), target, &
                                           intent(  out) :: Rdif, Tdif, source_dn, source_up
    real(wp), dimension(ncol,       ngpt), intent(  out) :: source_sfc ! Source function for upward radation at surface
    real(wp), dimension(ncol,nlay+1,ngpt), target, &
                                           intent(inout) :: flux_dn_dir ! Direct beam flux

    ! -----------------------
    integer  :: icol, ilay, igpt

    ! Variables used in Meador and Weaver
    real(wp) :: gamma1, gamma2, gamma3, gamma4, alpha1, alpha2


    ! Ancillary variables
    real(wp), parameter :: min_k = 1.e4_wp * epsilon(1._wp) ! Suggestion from Chiel van Heerwaarden
    real(wp) :: k, exp_minusktau, k_mu, k_gamma3, k_gamma4
    real(wp) :: RT_term, exp_minus2ktau
    real(wp) :: Rdir, Tdir, Tnoscat, inc_flux
    integer  :: lay_index, inc_index, trans_index
    real(wp) :: tau_s, w0_s, g_s, mu0_s
    ! ---------------------------------
    !$acc  parallel loop collapse(2)
    !$omp target teams distribute parallel do simd collapse(2)
    do igpt = 1, ngpt
      do icol = 1, ncol
        do ilay = 1, nlay
          if(top_at_1) then
            lay_index   = ilay
            inc_index   = lay_index
            trans_index = lay_index+1
          else
            lay_index   = nlay-ilay+1
            inc_index   = lay_index+1
            trans_index = lay_index
          end if
          inc_flux = flux_dn_dir(icol,inc_index,igpt)
          !
          ! Scalars
          !
          tau_s = tau(icol,lay_index,igpt)
          w0_s  = w0 (icol,lay_index,igpt)
          g_s   = g  (icol,lay_index,igpt)
          mu0_s = mu0(icol,lay_index)
          !
          ! Zdunkowski Practical Improved Flux Method "PIFM"
          !  (Zdunkowski et al., 1980;  Contributions to Atmospheric Physics 53, 147-66)
          !
          gamma1 = (8._wp - w0_s * (5._wp + 3._wp * g_s)) * .25_wp
          gamma2 =  3._wp *(w0_s * (1._wp -         g_s)) * .25_wp
          !
          ! Direct reflect and transmission
          !
          ! Eq 18;  k = SQRT(gamma1**2 - gamma2**2), limited below to avoid div by 0.
          !   k = 0 for isotropic, conservative scattering; this lower limit on k
          !   gives relative error with respect to conservative solution
          !   of < 0.1% in Rdif down to tau = 10^-9
          k = sqrt(max((gamma1 - gamma2) * (gamma1 + gamma2), min_k))
          k_mu     = k * mu0_s
          exp_minusktau = exp(-tau_s*k)
          exp_minus2ktau = exp_minusktau * exp_minusktau

          ! Refactored to avoid rounding errors when k, gamma1 are of very different magnitudes
          RT_term = 1._wp / (k      * (1._wp + exp_minus2ktau)  + &
                             gamma1 * (1._wp - exp_minus2ktau) )
          ! Equation 25
          Rdif(icol,lay_index,igpt) = RT_term * gamma2 * (1._wp - exp_minus2ktau)

          ! Equation 26
          Tdif(icol,lay_index,igpt) = RT_term * 2._wp * k * exp_minusktau

          !
          ! On a round earth, where mu0 can increase with depth in the atmosphere,
          !   levels with mu0 <= 0 have no direct beam and hence no source for diffuse light
          !
          if(mu0_s > 0._wp) then
            !
            ! Equation 14, multiplying top and bottom by exp(-k*tau)
            !   and rearranging to avoid div by 0.
            !
            RT_term =  w0_s * RT_term/merge(1._wp - k_mu*k_mu, &
                                            epsilon(1._wp),    &
                                            abs(1._wp - k_mu*k_mu) >= epsilon(1._wp))
            !
            ! Zdunkowski Practical Improved Flux Method "PIFM"
            !  (Zdunkowski et al., 1980;  Contributions to Atmospheric Physics 53, 147-66)
            !
            gamma3 = (2._wp - 3._wp * mu0_s *         g_s ) * .25_wp
            gamma4 =  1._wp - gamma3
            alpha1 = gamma1 * gamma4 + gamma2 * gamma3           ! Eq. 16
            alpha2 = gamma1 * gamma3 + gamma2 * gamma4           ! Eq. 17

            !
            ! Transmittance of direct, unscattered beam.
            !
            k_gamma3 = k * gamma3
            k_gamma4 = k * gamma4
            Tnoscat = exp(-tau_s/mu0_s)
            Rdir = RT_term  *                                            &
                ((1._wp - k_mu) * (alpha2 + k_gamma3)                  - &
                 (1._wp + k_mu) * (alpha2 - k_gamma3) * exp_minus2ktau - &
                 2.0_wp * (k_gamma3 - alpha2 * k_mu)  * exp_minusktau * Tnoscat)
            !
            ! Equation 15, multiplying top and bottom by exp(-k*tau),
            !   multiplying through by exp(-tau/mu0) to
            !   prefer underflow to overflow
            ! Omitting direct transmittance
            !
            Tdir = -RT_term *                                                             &
                  ((1._wp + k_mu) * (alpha1 + k_gamma4)                  * Tnoscat - &
                   (1._wp - k_mu) * (alpha1 - k_gamma4) * exp_minus2ktau * Tnoscat - &
                   2.0_wp * (k_gamma4 + alpha1 * k_mu)  * exp_minusktau)
            ! Final check that energy is not spuriously created, by recognizing that
            ! the beam can either be reflected, penetrate unscattered to the base of a layer, 
            ! or penetrate through but be scattered on the way - the rest is absorbed
            ! Makes the equations safer in single precision. Credit: Robin Hogan, Peter Ukkonen
            Rdir    = max(0.0_wp, min(Rdir, (1.0_wp - Tnoscat       ) ))
            Tdir    = max(0.0_wp, min(Tdir, (1.0_wp - Tnoscat - Rdir) ))
            source_up  (icol,lay_index,  igpt) =    Rdir * inc_flux
            source_dn  (icol,lay_index,  igpt) =    Tdir * inc_flux
            flux_dn_dir(icol,trans_index,igpt) = Tnoscat * inc_flux
          else
            source_up  (icol,lay_index,  igpt) = 0._wp
            source_dn  (icol,lay_index,  igpt) = 0._wp
            flux_dn_dir(icol,trans_index,igpt) = 0._wp
          end if
        end do
        source_sfc(icol,igpt) = flux_dn_dir(icol,trans_index,igpt)*sfc_albedo(icol,igpt)
      end do
    end do
  end subroutine sw_dif_and_source
! ---------------------------------------------------------------
!
! Transport of diffuse radiation through a vertically layered atmosphere.
!   Equations are after Shonk and Hogan 2008, doi:10.1175/2007JCLI1940.1 (SH08)
!   This routine is shared by longwave and shortwave
!
! -------------------------------------------------------------------------------------------------
  subroutine adding(ncol, nlay, ngpt, top_at_1, &
                    albedo_sfc,           &
                    rdif, tdif,           &
                    src_dn, src_up, src_sfc, &
                    flux_up, flux_dn)
    !dir$ optimize(-O0)
    integer,                               intent(in   ) :: ncol, nlay, ngpt
    logical(wl),                           intent(in   ) :: top_at_1
    real(wp), dimension(ncol       ,ngpt), intent(in   ) :: albedo_sfc
    real(wp), dimension(ncol,nlay  ,ngpt), intent(in   ) :: rdif, tdif
    real(wp), dimension(ncol,nlay  ,ngpt), intent(in   ) :: src_dn, src_up
    real(wp), dimension(ncol       ,ngpt), intent(in   ) :: src_sfc
    real(wp), dimension(ncol,nlay+1,ngpt), intent(  out) :: flux_up
    ! intent(inout) because top layer includes incident flux
    real(wp), dimension(ncol,nlay+1,ngpt), intent(inout) :: flux_dn
    ! ------------------
    integer :: icol, ilev, igpt

    ! These arrays could be private per thread in OpenACC, with 1 dimension of size nlay (or nlay+1)
    ! However, current PGI (19.4) has a bug preventing it from properly handling such private arrays.
    ! So we explicitly create the temporary arrays of size nlay(+1) per each of the ncol*ngpt elements
    !
    real(wp), dimension(ncol,nlay+1,ngpt) :: albedo, &  ! reflectivity to diffuse radiation below this level
                                              ! alpha in SH08
                                   src        ! source of diffuse upwelling radiation from emission or
                                              ! scattering of direct beam
                                              ! G in SH08
    real(wp), dimension(ncol,nlay  ,ngpt) :: denom      ! beta in SH08
    ! ------------------
    ! ---------------------------------
    !
    ! Indexing into arrays for upward and downward propagation depends on the vertical
    !   orientation of the arrays (whether the domain top is at the first or last index)
    ! We write the loops out explicitly so compilers will have no trouble optimizing them.
    !
    !$acc enter data copyin(albedo_sfc, rdif, tdif, src_dn, src_up, src_sfc, flux_dn)
    !$omp target enter data map(to:albedo_sfc, rdif, tdif, src_dn, src_up, src_sfc, flux_dn)
    !$acc enter data create(flux_up, albedo, src, denom)
    !$omp target enter data map(alloc:flux_up, albedo, src, denom)

    if(top_at_1) then
      !$acc parallel loop gang vector collapse(2)
      !$omp target teams distribute parallel do simd collapse(2)
      do igpt = 1, ngpt
        do icol = 1, ncol
          ilev = nlay + 1
          ! Albedo of lowest level is the surface albedo...
          albedo(icol,ilev,igpt)  = albedo_sfc(icol,igpt)
          ! ... and source of diffuse radiation is surface emission
          src(icol,ilev,igpt) = src_sfc(icol,igpt)

          !
          ! From bottom to top of atmosphere --
          !   compute albedo and source of upward radiation
          !
          do ilev = nlay, 1, -1
            denom(icol,ilev,igpt) = 1._wp/(1._wp - rdif(icol,ilev,igpt)*albedo(icol,ilev+1,igpt))    ! Eq 10
            albedo(icol,ilev,igpt) = rdif(icol,ilev,igpt) + &
                  tdif(icol,ilev,igpt)*tdif(icol,ilev,igpt) * albedo(icol,ilev+1,igpt) * denom(icol,ilev,igpt) ! Equation 9
            !
            ! Equation 11 -- source is emitted upward radiation at top of layer plus
            !   radiation emitted at bottom of layer,
            !   transmitted through the layer and reflected from layers below (tdiff*src*albedo)
            !
            src(icol,ilev,igpt) =  src_up(icol, ilev, igpt) + &
                           tdif(icol,ilev,igpt) * denom(icol,ilev,igpt) *       &
                             (src(icol,ilev+1,igpt) + albedo(icol,ilev+1,igpt)*src_dn(icol,ilev,igpt))
          end do

          ! Eq 12, at the top of the domain upwelling diffuse is due to ...
          ilev = 1
          flux_up(icol,ilev,igpt) = flux_dn(icol,ilev,igpt) * albedo(icol,ilev,igpt) + & ! ... reflection of incident diffuse and
                                    src(icol,ilev,igpt)                                  ! emission from below

          !
          ! From the top of the atmosphere downward -- compute fluxes
          !
          do ilev = 2, nlay+1
            flux_dn(icol,ilev,igpt) = (tdif(icol,ilev-1,igpt)*flux_dn(icol,ilev-1,igpt) + &  ! Equation 13
                               rdif(icol,ilev-1,igpt)*src(icol,ilev,igpt) +       &
                               src_dn(icol,ilev-1,igpt)) * denom(icol,ilev-1,igpt)
            flux_up(icol,ilev,igpt) = flux_dn(icol,ilev,igpt) * albedo(icol,ilev,igpt) + & ! Equation 12
                              src(icol,ilev,igpt)
          end do
        end do
      end do

    else

      !$acc parallel loop collapse(2)
      !$omp target teams distribute parallel do simd collapse(2)
      do igpt = 1, ngpt
        do icol = 1, ncol
          ilev = 1
          ! Albedo of lowest level is the surface albedo...
          albedo(icol,ilev,igpt)  = albedo_sfc(icol,igpt)
          ! ... and source of diffuse radiation is surface emission
          src(icol,ilev,igpt) = src_sfc(icol,igpt)

          !
          ! From bottom to top of atmosphere --
          !   compute albedo and source of upward radiation
          !
          do ilev = 1, nlay
            denom (icol,ilev  ,igpt) = 1._wp/(1._wp - rdif(icol,ilev,igpt)*albedo(icol,ilev,igpt))                ! Eq 10
            albedo(icol,ilev+1,igpt) = rdif(icol,ilev,igpt) + &
                               tdif(icol,ilev,igpt)*tdif(icol,ilev,igpt) * albedo(icol,ilev,igpt) * denom(icol,ilev,igpt) ! Equation 9
            !
            ! Equation 11 -- source is emitted upward radiation at top of layer plus
            !   radiation emitted at bottom of layer,
            !   transmitted through the layer and reflected from layers below (tdiff*src*albedo)
            !
            src(icol,ilev+1,igpt) =  src_up(icol, ilev, igpt) +  &
                             tdif(icol,ilev,igpt) * denom(icol,ilev,igpt) *       &
                             (src(icol,ilev,igpt) + albedo(icol,ilev,igpt)*src_dn(icol,ilev,igpt))
          end do

          ! Eq 12, at the top of the domain upwelling diffuse is due to ...
          ilev = nlay+1
          flux_up(icol,ilev,igpt) = flux_dn(icol,ilev,igpt) * albedo(icol,ilev,igpt) + & ! ... reflection of incident diffuse and
                            src(icol,ilev,igpt)                          ! scattering by the direct beam below

          !
          ! From the top of the atmosphere downward -- compute fluxes
          !
          do ilev = nlay, 1, -1
            flux_dn(icol,ilev,igpt) = (tdif(icol,ilev,igpt)*flux_dn(icol,ilev+1,igpt) + &  ! Equation 13
                               rdif(icol,ilev,igpt)*src(icol,ilev,igpt) + &
                               src_dn(icol, ilev, igpt)) * denom(icol,ilev,igpt)
            flux_up(icol,ilev,igpt) = flux_dn(icol,ilev,igpt) * albedo(icol,ilev,igpt) + & ! Equation 12
                              src(icol,ilev,igpt)

          end do
        end do
      end do
    end if
    !$acc exit data delete(albedo_sfc, rdif, tdif, src_dn, src_up, src_sfc, albedo, src, denom)
    !$omp target exit data map(release:albedo_sfc, rdif, tdif, src_dn, src_up, src_sfc, albedo, src, denom)
    !$acc exit data copyout(flux_up, flux_dn)
    !$omp target exit data map(from:flux_up, flux_dn)
  end subroutine adding
! -------------------------------------------------------------------------------------------------
!
! Similar to Longwave no-scattering tarnsport  (lw_transport_noscat)
!   a) adds adjustment factor based on cloud properties
!
!   implementation notice:
!       the adjustmentFactor computation can be skipped where Cn <= epsilon
!
! -------------------------------------------------------------------------------------------------
subroutine lw_transport_1rescl(ncol, nlay, ngpt, top_at_1, &
                               trans, source_dn, source_up, &
                               radn_up, radn_dn, An, Cn,    &
                               do_Jacobians, radn_up_Jac)
    integer,                               intent(in   ) :: ncol, nlay, ngpt ! Number of columns, layers, g-points
    logical(wl),                           intent(in   ) :: top_at_1   !
    real(wp), dimension(ncol,nlay  ,ngpt), intent(in   ) :: trans      ! transmissivity = exp(-tau)
    real(wp), dimension(ncol,nlay  ,ngpt), intent(in   ) :: source_dn, &
                                                            source_up  ! Diffuse radiation emitted by the layer
    real(wp), dimension(ncol,nlay+1,ngpt), intent(inout) :: radn_up    ! Radiances [W/m2-str]
    real(wp), dimension(ncol,nlay+1,ngpt), intent(inout) :: radn_dn    !Top level must contain incident flux boundary condition
    real(wp), dimension(ncol,nlay  ,ngpt), intent(in   ) :: An, Cn
    logical(wl),                           intent(in   ) :: do_Jacobians
    real(wp), dimension(ncol,nlay+1,ngpt), intent(inout) :: radn_up_Jac ! Radiances [W/m2-str]
    ! ---------------------------------------------------
    ! Local variables
    integer :: ilev, icol, igpt
    real(wp) :: adjustmentFactor
    ! ---------------------------------------------------
    if(top_at_1) then
      !
      ! Top of domain is index 1
      !
      ! Downward propagation
      !$acc                         parallel loop    collapse(2) no_create(radn_up_Jac)
      !$omp target teams distribute parallel do simd collapse(2)
      do igpt = 1, ngpt
        do icol = 1, ncol
          ! Upward propagation
          do ilev = nlay, 1, -1
            adjustmentFactor = Cn(icol,ilev,igpt) * &
                   ( An(icol,ilev,igpt)*radn_dn(icol,ilev,igpt) - &
                     source_dn(icol,ilev,igpt)*trans(icol,ilev,igpt ) - &
                     source_up(icol,ilev,igpt))
            radn_up(icol,ilev,igpt)       = trans(icol,ilev,igpt)*radn_up    (icol,ilev+1,igpt) + &
                                            source_up(icol,ilev,igpt) + adjustmentFactor
          enddo
          if(do_Jacobians) then
            do ilev = nlay, 1, -1
              radn_up_Jac(icol,ilev,igpt) = trans(icol,ilev,igpt)*radn_up_Jac(icol,ilev+1,igpt)
            end do
          end if

          ! radn_dn_Jac(icol,1,igpt) = 0._wp
          ! 2nd Downward propagation
          do ilev = 1, nlay
            ! radn_dn_Jac(icol,ilev+1,igpt) = trans(icol,ilev,igpt)*radn_dn_Jac(icol,ilev,igpt)
            adjustmentFactor = Cn(icol,ilev,igpt)*( &
                An(icol,ilev,igpt)*radn_up(icol,ilev,igpt) - &
                source_up(icol,ilev,igpt)*trans(icol,ilev,igpt) - &
                source_dn(icol,ilev,igpt) )
            radn_dn(icol,ilev+1,igpt)     = trans(icol,ilev,igpt)*radn_dn   (icol,ilev,  igpt) + &
                                            source_dn(icol,ilev,igpt) + adjustmentFactor
            ! adjustmentFactor             = Cn(icol,ilev,igpt)*An(icol,ilev,igpt)*radn_up_Jac(icol,ilev,igpt)
            ! radn_dn_Jac(icol,ilev+1,igpt) = radn_dn_Jac(icol,ilev+1,igpt) + adjustmentFactor
          enddo
        enddo
      enddo
    else
      !$acc  parallel loop collapse(2) no_create(radn_up_Jac)
      !$omp target teams distribute parallel do simd collapse(2)
      do igpt = 1, ngpt
        do icol = 1, ncol
          ! Upward propagation
          do ilev = 1, nlay
            adjustmentFactor = Cn(icol,ilev,igpt)*&
                   ( An(icol,ilev,igpt)*radn_dn(icol,ilev+1,igpt) - &
                     source_dn(icol,ilev,igpt) *trans(icol,ilev ,igpt) - &
                     source_up(icol,ilev,igpt))
            radn_up(icol,ilev+1,igpt)       = trans(icol,ilev,igpt)*radn_up   (icol,ilev,igpt) + &
                                              source_up(icol,ilev,igpt) + adjustmentFactor
          end do
          if(do_Jacobians) then
            do ilev = 1, nlay
              radn_up_Jac(icol,ilev+1,igpt) = trans(icol,ilev,igpt)*radn_up_Jac(icol,ilev,igpt)
            end do
          end if

          ! 2st Downward propagation
          ! radn_dn_Jac(icol,nlay+1,igpt) = 0._wp
          do ilev = nlay, 1, -1
            ! radn_dn_Jac(icol,ilev,igpt) = trans(icol,ilev,igpt)*radn_dn_Jac(icol,ilev+1,igpt)
            adjustmentFactor = Cn(icol,ilev,igpt)*( &
                    An(icol,ilev,igpt)*radn_up(icol,ilev,igpt) - &
                    source_up(icol,ilev,igpt)*trans(icol,ilev ,igpt ) - &
                    source_dn(icol,ilev,igpt) )
            radn_dn(icol,ilev,igpt)         = trans(icol,ilev,igpt)*radn_dn   (icol,ilev+1,igpt) + &
                                              source_dn(icol,ilev,igpt) + adjustmentFactor
            ! adjustmentFactor           = Cn(icol,ilev,igpt)*An(icol,ilev,igpt)*radn_up_Jac(icol,ilev,igpt)
            ! radn_dn_Jac(icol,ilev,igpt) = radn_dn_Jac(icol,ilev,igpt) + adjustmentFactor
          end do
        enddo
      enddo
    end if
  end subroutine lw_transport_1rescl
  ! -------------------------------------------------------------------------------------------------
  !
  ! Spectral reduction over all points
  !
  subroutine sum_broadband_factor(ncol, nlev, ngpt, factor, spectral_flux, broadband_flux)
  integer,                               intent(in ) :: ncol, nlev, ngpt
  real(wp),                              intent(in ) :: factor
  real(wp), dimension(ncol, nlev, ngpt), intent(in ) :: spectral_flux
  real(wp), dimension(ncol, nlev),       intent(out) :: broadband_flux

  integer  :: icol, ilev, igpt
  real(wp) :: scalar ! local scalar version

  !$acc                         parallel loop gang vector collapse(2)
  !$omp target teams distribute parallel do simd          collapse(2)
  do ilev = 1, nlev
    do icol = 1, ncol

      scalar = 0.0_wp

      do igpt = 1, ngpt
        scalar = scalar + spectral_flux(icol, ilev, igpt)
      end do

      broadband_flux(icol, ilev) = factor * scalar
    end do
  end do
  end subroutine sum_broadband_factor
  ! -------------------------------------------------------------------------------------------------
  !
  ! Apply a scalar weight to every element of an array
  !
  subroutine apply_factor_3D(ncol, nlev, ngpt, factor, array)
    integer,                               intent(in   ) :: ncol, nlev, ngpt
    real(wp),                              intent(in   ) :: factor
    real(wp), dimension(ncol, nlev, ngpt), intent(inout) :: array

    integer  :: icol, ilev, igpt

    !$acc                         parallel loop gang vector collapse(3)
    !$omp target teams distribute parallel do simd          collapse(3)
    do igpt = 1, ngpt
      do ilev = 1, nlev
        do icol = 1, ncol
          array(icol, ilev, igpt) = factor * array(icol, ilev, igpt)
        end do
      end do
    end do
  end subroutine apply_factor_3D
  ! -------------------------------------------------------------------------------------------------
  !
  ! Add an array to an existing array
  !
  subroutine add_arrays_3D(ncol, nlev, ngpt, increment, array)
    integer,                               intent(in   ) :: ncol, nlev, ngpt
    real(wp), dimension(ncol, nlev, ngpt), intent(in   ) :: increment
    real(wp), dimension(ncol, nlev, ngpt), intent(inout) :: array

    integer  :: icol, ilev, igpt

    !$acc                         parallel loop gang vector collapse(3)
    !$omp target teams distribute parallel do simd          collapse(3)
    do igpt = 1, ngpt
      do ilev = 1, nlev
        do icol = 1, ncol
          array(icol, ilev, igpt) = array(icol, ilev, igpt) + increment(icol, ilev, igpt)
        end do
      end do
    end do
  end subroutine add_arrays_3D
  ! -------------------------------------------------------------------------------------------------
  subroutine add_arrays_2D(ncol, nlev, increment, array)
    integer,                         intent(in   ) :: ncol, nlev
    real(wp), dimension(ncol, nlev), intent(in   ) :: increment
    real(wp), dimension(ncol, nlev), intent(inout) :: array

    integer  :: icol, ilev

    !$acc                         parallel loop gang vector collapse(2)
    !$omp target teams distribute parallel do simd          collapse(2)
    do ilev = 1, nlev
      do icol = 1, ncol
        array(icol, ilev) = array(icol, ilev) + increment(icol, ilev)
      end do
    end do
  end subroutine add_arrays_2D
  ! -------------------------------------------------------------------------------------------------
end module mo_rte_solver_kernels
