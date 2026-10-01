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
!
!>## Numeric calculations for radiative transfer solvers
!>  - Emission/absorption (no-scattering) calculations
!>  - solver for multi-angle Gaussian quadrature
!>  - solver for a single angle, calling
!>      - source function computation (linear-in-tau)
!>      -  transport
!>  - Extinction-only calculation (direct solar beam)
!>  - Two-stream calculations:
!>    solvers for LW and SW with different boundary conditions and source functions
!>      - source function calculation for LW, SW
!>      - two-stream calculations for LW, SW (using different assumtions about phase function)
!>      - transport (adding)
!>  - Application of boundary conditions
!
! -------------------------------------------------------------------------------------------------
module mo_rte_solver_kernels
  use,  intrinsic :: iso_c_binding
  use mo_rte_kind,      only: wp, wl
  use mo_rte_util_array,only: zero_array
  implicit none
  private

  public :: lw_solver_noscat, lw_solver_2stream, &
            sw_solver_noscat, sw_solver_2stream

  real(wp), parameter :: pi = acos(-1._wp)
contains
  ! -------------------------------------------------------------------------------------------------
  !
  ! Top-level longwave kernels
  !
  ! -------------------------------------------------------------------------------------------------
  !
  !>  LW fluxes, no scattering, mu (cosine of integration angle) specified by column
  !>    Does radiation calculation at user-supplied angles; converts radiances to flux
  !>    using user-supplied weights
  !
  ! ---------------------------------------------------------------
  subroutine lw_solver_noscat_oneangle(ncol, nlay, ngpt, top_at_1, D, weight, &
                              tau, lay_source, lev_source, sfc_emis, sfc_src, &
                              incident_flux,    &
                              flux_up, flux_dn, &
                              do_broadband, broadband_up, broadband_dn, &
                              do_Jacobians, sfc_srcJac, flux_upJac,     &
                              do_rescaling, ssa, g)
    integer,                               intent(in   ) :: ncol, nlay, ngpt ! Number of columns, layers, g-points
    logical(wl),                           intent(in   ) :: top_at_1
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: D            ! secant of propagation angle  []
    real(wp),                              intent(in   ) :: weight       ! quadrature weight
    real(wp), dimension(ncol,nlay,  ngpt), intent(in   ) :: tau          ! Absorption optical thickness []
    real(wp), dimension(ncol,nlay,  ngpt), intent(in   ) :: lay_source   ! Planck source at layer average temperature [W/m2]
    real(wp), dimension(ncol,nlay+1,ngpt), intent(in   ) :: lev_source   ! Planck source at layer edge  [W/m2]
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_emis     ! Surface emissivity      []
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_src      ! Surface source function [W/m2]
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: incident_flux! Boundary condition for flux [W/m2]
    real(wp), dimension(ncol,nlay+1,ngpt), target, &                     ! Fluxes [W/m2]
                                           intent(  out) :: flux_up, flux_dn
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
    ! ------------------------------------
    ! Local variables, no g-point dependency
    !
    integer                        :: icol, ilay, igpt
    integer                        :: top_level, sfc_level
    real(wp), dimension(ncol,nlay) :: tau_loc, &  ! path length (tau/mu)
                                      trans       ! transmissivity  = exp(-tau)
    real(wp), dimension(ncol,nlay) :: source_dn, source_up
    real(wp), dimension(ncol     ) :: sfc_albedo

    real(wp), parameter :: pi = acos(-1._wp)
    ! loc_fluxes hold a single g-point flux if fluxes are being integrated instead of returned
    !   with spectral detail
    real(wp), dimension(ncol,nlay+1), &
                              target  :: loc_flux_up, loc_flux_dn
    ! gpt_fluxes point to calculations for the current g-point
    real(wp), dimension(:,:), pointer :: gpt_flux_up, gpt_flux_dn
    ! -------------------------------------------------------------------------------------------------
    ! Optionally, use an approximate treatment of scattering using rescaling
    !   Implemented based on the paper
    !   Tang G, et al, 2018: https://doi.org/10.1175/JAS-D-18-0014.1
    !   a) relies on rescaling of the optical parameters based on asymetry factor and single scattering albedo
    !       scaling can be computed  by scaling_1rescl
    !   b) adds adustment term based on cloud properties (lw_transport_1rescl)
    !      adustment terms is computed based on solution of the Tang equations
    !      for "linear-in-tau" internal source (not in the paper)
    !
    ! Used when approximating scattering
    !
    real(wp)                         :: ssal, wb, scaleTau
    real(wp), dimension(ncol,nlay  ) :: An, Cn
    real(wp), dimension(ncol,nlay+1) :: gpt_flux_Jac
    ! ------------------------------------
    ! Which way is up?
    if(top_at_1) then
      top_level = 1
      sfc_level = nlay+1
    else
      top_level = nlay+1
      sfc_level = 1
    end if

    !
    ! Integrated fluxes need zeroing
    !
    if(do_broadband) then
      call zero_array(ncol, nlay+1, broadband_up )
      call zero_array(ncol, nlay+1, broadband_dn )
    end if
    if(do_Jacobians) &
      call zero_array(ncol, nlay+1, flux_upJac )

    do igpt = 1, ngpt
      if(do_broadband) then
        gpt_flux_up  => loc_flux_up
        gpt_flux_dn  => loc_flux_dn
      else
        gpt_flux_up  => flux_up (:,:,igpt)
        gpt_flux_dn  => flux_dn (:,:,igpt)
      end if
      !
      ! Transport is for intensity
      !   convert flux at top of domain to intensity assuming azimuthal isotropy
      !
      gpt_flux_dn(:,top_level) = incident_flux(:,igpt)/(pi * weight)
      !
      ! Optical path and transmission, used in source function and transport calculations
      !
      if (do_rescaling) then
        !
        ! The scaling and scaleTau terms are independent of propagation
        !   angle D and could be pre-computed if several values of D are used
        ! We re-compute them here to keep not have to localize memory use
        !
        do ilay = 1, nlay
          do icol = 1, ncol
            ssal = ssa(icol, ilay, igpt)

            ! w is the layer single scattering albedo
            ! b is phase function parameter (Eq.13 of the paper)
            ! for the similarity principle scaling scheme
            ! b = (1-g)/2 (where g is phase function avergae cosine)
            wb = ssal*(1._wp - g(icol, ilay, igpt)) * 0.5_wp

            ! scaleTau=1-w(1-b) is a scaling factor of the optical thickness representing
            ! the radiative transfer equation in a nonscattering form Eq(14) of the paper
            scaleTau = (1._wp - ssal + wb)

            ! Cn = 0.5*wb/(1-w(1-b)) is parameter of Eq.21-22 of the Tang paper
            ! Tang paper, p.2222 advises to replace 0.5 with 0.4 based on simulations
            Cn(icol,ilay) = 0.4_wp*wb/scaleTau

            ! Eqs.15, 18ab and 19 of the paper,
            ! rescaling of the optical depth multiplied by path length
            tau_loc(icol,ilay) = tau(icol,ilay,igpt)*D(icol,igpt)*scaleTau
          end do
          trans  (:,ilay) = exp(-tau_loc(:,ilay))
          An(:,ilay) = (1._wp-trans(:,ilay)**2)
        end do
      else
        do ilay = 1, nlay
          tau_loc(:,ilay) = tau(:,ilay,igpt)*D(:,igpt)
          trans  (:,ilay) = exp(-tau_loc(:,ilay))
        end do
      end if
      !
      ! Source function for diffuse radiation
      !
      call lw_source_noscat(ncol, nlay, top_at_1, &
                            lay_source(:,:,igpt), lev_source(:,:,igpt), &
                            tau_loc, trans, source_dn, source_up)
      !
      ! Transport down
      !
      call lw_transport_noscat_dn(ncol, nlay, top_at_1, trans, source_dn, gpt_flux_dn)
      !
      ! Surface albedo, surface source function, reflection and emission
      !
      sfc_albedo(:)    = 1._wp - sfc_emis(:,igpt)
      gpt_flux_up   (:,sfc_level) = gpt_flux_dn(:,sfc_level)*sfc_albedo(:) + &
                                    sfc_emis(:,igpt) * sfc_src   (:,igpt)
      if(do_Jacobians) &
        gpt_flux_Jac(:,sfc_level)  = sfc_emis(:,igpt) * sfc_srcJac(:,igpt)
      !
      ! Transport up, or up and down again if using rescaling
      !
      if(do_rescaling) then
        call lw_transport_1rescl(ncol, nlay, top_at_1, trans,                  &
                                 source_dn, source_up,                         &
                                 gpt_flux_up, gpt_flux_dn, An, Cn, &
                                 do_Jacobians, gpt_flux_Jac) ! Standing in for Jacobian, i.e. rad_up_Jac(:,:,igpt), rad_dn_Jac(:,:,igpt))
      else
        call lw_transport_noscat_up(ncol, nlay, top_at_1, trans, source_up, gpt_flux_up, &
                                    do_Jacobians, gpt_flux_Jac)
      end if

      if(do_broadband) then
        broadband_up(:,:) = broadband_up(:,:) + gpt_flux_up(:,:)
        broadband_dn(:,:) = broadband_dn(:,:) + gpt_flux_dn(:,:)
      else
        !
        ! Convert intensity to flux assuming azimuthal isotropy and quadrature weight
        !
        gpt_flux_dn(:,:)  = pi * weight * gpt_flux_dn(:,:)
        gpt_flux_up(:,:)  = pi * weight * gpt_flux_up(:,:)
      end if
      !
      ! Only broadband-integrated Jacobians are provided
      !
      if(do_Jacobians) &
          flux_upJac(:,:) =  flux_upJac(:,:) + gpt_flux_Jac(:,:)
    end do  ! g point loop

    if(do_broadband) then
      broadband_up(:,:) = pi * weight* broadband_up(:,:)
      broadband_dn(:,:) = pi * weight* broadband_dn(:,:)
    end if
    if(do_Jacobians) &
      flux_upJac(:,:)   = pi * weight * flux_upJac(:,:)

  end subroutine lw_solver_noscat_oneangle
  ! -------------------------------------------------------------------------------------------------
  !
  !> LW transport, no scattering, multi-angle quadrature
  !>   Users provide a set of weights and quadrature angles
  !>   Routine sums over single-angle solutions for each sets of angles/weights
  !
  ! ---------------------------------------------------------------
  subroutine lw_solver_noscat(ncol, nlay, ngpt, top_at_1, &
                              nmus, Ds, weights,          &
                              tau,                        &
                              lay_source, lev_source,     &
                              sfc_emis, sfc_src,          &
                              inc_flux,                   &
                              flux_up, flux_dn,           &
                              do_broadband, broadband_up, broadband_dn,   &
                              do_Jacobians, sfc_srcJac, flux_upJac,       &
                              do_rescaling, ssa, g) bind(C, name="rte_lw_solver_noscat")
    integer,                               intent(in   ) :: ncol, nlay, ngpt
                                                            !! Number of columns, layers, g-points
    logical(wl),                           intent(in   ) :: top_at_1
                                                            !! ilay = 1 is the top of the atmosphere?
    integer,                               intent(in   ) :: nmus
                                                            !! number of quadrature angles
    real(wp), dimension (ncol,      ngpt, &
                                    nmus), intent(in   ) :: Ds
                                                            !! quadrature secants
    real(wp), dimension(nmus),             intent(in   ) :: weights
                                                            !! quadrature weights
    real(wp), dimension(ncol,nlay,  ngpt), intent(in   ) :: tau
                                                            !! Absorption optical thickness []
    real(wp), dimension(ncol,nlay,  ngpt), intent(in   ) :: lay_source
                                                            !! Planck source at layer average temperature [W/m2]
    real(wp), dimension(ncol,nlay+1,ngpt), intent(in   ) :: lev_source
                                                            !! Planck source at layer edge for radiation[W/m2]
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_emis
                                                            !! Surface emissivity      []
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_src
                                                            !! Surface source function [W/m2]
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: inc_flux
                                                            !! Incident diffuse flux, probably 0 [W/m2]
    real(wp), dimension(ncol,nlay+1,ngpt), target, &
                                           intent(  out) :: flux_up, flux_dn
                                                            !! Fluxes [W/m2]
    !
    ! Optional variables - arrays aren't referenced if corresponding logical  == False
    !
    logical(wl),                           intent(in   ) :: do_broadband
    real(wp), dimension(ncol,nlay+1     ), target, &
                                           intent(  out) :: broadband_up, broadband_dn
                                                            !! Spectrally-integrated fluxes [W/m2]
    logical(wl),                           intent(in   ) :: do_Jacobians
                                                            !! compute Jacobian with respect to surface temeprature?
    real(wp), dimension(ncol       ,ngpt), intent(in   ) :: sfc_srcJac
                                                            !! surface temperature Jacobian of surface source function [W/m2/K]
    real(wp), dimension(ncol,nlay+1     ), target, &
                                           intent(  out) :: flux_upJac
                                                            !! surface temperature Jacobian of Radiances [W/m2-str / K]
    logical(wl),                           intent(in   ) :: do_rescaling
                                                            !! Approximate treatment of scattering (10.1175/JAS-D-18-0014.1)
    real(wp), dimension(ncol,nlay  ,ngpt), intent(in   ) :: ssa, g
                                                            !! single-scattering albedo, asymmetry parameter
    ! ------------------------------------
    !
    ! Local variables - used for a single quadrature angle
    !
    real(wp), dimension(:,:,:), pointer :: this_flux_up,      this_flux_dn
    real(wp), dimension(:,:),   pointer :: this_broadband_up, this_broadband_dn, this_flux_upJac

    integer :: imu
    ! ------------------------------------
    !
    ! For the first angle output arrays store total flux
    !
    call lw_solver_noscat_oneangle(ncol, nlay, ngpt, &
                          top_at_1, Ds(:,:,1), weights(1), tau,      &
                          lay_source, lev_source, sfc_emis, sfc_src, &
                          inc_flux,         &
                          flux_up, flux_dn, &
                          do_broadband, broadband_up, broadband_dn, &
                          do_Jacobians, sfc_srcJac, flux_upJac,     &
                          do_rescaling, ssa, g)
    !
    ! For more than one angle use local arrays
    !
    if(nmus > 1) then
      if(do_broadband) then
        allocate(this_broadband_up(ncol,nlay+1), this_broadband_dn(ncol,nlay+1))
        ! Spectrally-resolved fluxes won't be filled in so can point to caller-supplied memory
        this_flux_up => flux_up
        this_flux_dn => flux_dn
      else
        allocate(this_flux_up(ncol,nlay+1,ngpt), this_flux_dn(ncol,nlay+1,ngpt))
        ! Spectrally-integrated fluxes won't be filled in so can point to caller-supplied memory
        this_broadband_up => broadband_up
        this_broadband_dn => broadband_dn
      end if
      if(do_Jacobians) then
        allocate(this_flux_upJac(ncol,nlay+1))
      else
        this_flux_upJac => flux_upJac
      end if
    end if
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
        broadband_up(:,:) = broadband_up(:,:) + this_broadband_up(:,:)
        broadband_dn(:,:) = broadband_dn(:,:) + this_broadband_dn(:,:)
      else
        flux_up   (:,:,:) = flux_up   (:,:,:) + this_flux_up   (:,:,:)
        flux_dn   (:,:,:) = flux_dn   (:,:,:) + this_flux_dn   (:,:,:)
      end if
      if (do_Jacobians) &
        flux_upJac(:,:)  = flux_upJac(:,:  ) + this_flux_upJac(:,:  )
    end do
    if(nmus > 1) then
      if(      do_broadband) deallocate(this_broadband_up, this_broadband_dn)
      if(.not. do_broadband) deallocate(this_flux_up,        this_flux_dn)
      if(      do_Jacobians) deallocate(this_flux_upJac)
    end if
  end subroutine lw_solver_noscat
  ! -------------------------------------------------------------------------------------------------
  !
  !> Longwave two-stream calculation:
  !>   - combine RRTMGP-specific sources at levels
  !>   - compute layer reflectance, transmittance
  !>   - compute total source function at levels using linear-in-tau
  !>   - transport
  !
  ! -------------------------------------------------------------------------------------------------
   subroutine lw_solver_2stream (ncol, nlay, ngpt, top_at_1, &
                                 tau, ssa, g,                &
                                 lay_source, lev_source, sfc_emis, sfc_src, &
                                 inc_flux,                   &
                                 flux_up, flux_dn) bind(C, name="rte_lw_solver_2stream")
    integer,                               intent(in   ) :: ncol, nlay, ngpt
                                                            !! Number of columns, layers, g-points
    logical(wl),                           intent(in   ) :: top_at_1
                                                            !! ilay = 1 is the top of the atmosphere?
    real(wp), dimension(ncol,nlay,  ngpt), intent(in   ) :: tau, ssa, g
                                                            !! Optical thickness, single-scattering albedo, asymmetry parameter []
    real(wp), dimension(ncol,nlay,  ngpt),   intent(in   ) :: lay_source
                                                            !! Planck source at layer average temperature [W/m2]
    real(wp), dimension(ncol,nlay+1,ngpt), intent(in   ) :: lev_source
                                                            !! Planck source at layer edge temperature  [W/m2]
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_emis
                                                            !! Surface emissivity      []
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_src
                                                            !! Surface source function [W/m2]
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: inc_flux
                                                            !! Incident diffuse flux, probably 0 [W/m2]
    real(wp), dimension(ncol,nlay+1,ngpt), intent(  out) :: flux_up, flux_dn
                                                            !! Fluxes [W/m2]
    ! ----------------------------------------------------------------------
    integer :: igpt, top_level
    real(wp), dimension(ncol,nlay  ) :: Rdif, Tdif, gamma1, gamma2
    real(wp), dimension(ncol       ) :: sfc_albedo
    real(wp), dimension(ncol,nlay  ) :: source_dn, source_up
    real(wp), dimension(ncol       ) :: source_sfc
    ! ------------------------------------
    top_level = nlay+1
    if(top_at_1) top_level = 1
    do igpt = 1, ngpt
      !
      ! Cell properties: reflection, transmission for diffuse radiation
      !   Coupling coefficients needed for source function
      !
      call lw_two_stream(ncol, nlay,                                 &
                         tau (:,:,igpt), ssa(:,:,igpt), g(:,:,igpt), &
                         gamma1, gamma2, Rdif, Tdif)
      !
      ! Source function for diffuse radiation
      !
      call lw_source_2str(ncol, nlay, top_at_1, &
                          sfc_emis(:,igpt), sfc_src(:,igpt), &
                          lay_source(:,:,igpt), lev_source, &
                          gamma1, gamma2, Rdif, Tdif, tau(:,:,igpt), &
                          source_dn, source_up, source_sfc)
      !
      ! Transport
      !
      sfc_albedo(1:ncol) = 1._wp - sfc_emis(:,igpt)
      !
      ! Boundary condition
      !
      flux_dn(:,top_level,igpt) = inc_flux(:,igpt)
      call adding(ncol, nlay, top_at_1,              &
                  sfc_albedo,                        &
                  Rdif, Tdif,                        &
                  source_dn, source_up, source_sfc,  &
                  flux_up(:,:,igpt), flux_dn(:,:,igpt))
    end do

  end subroutine lw_solver_2stream
  ! -------------------------------------------------------------------------------------------------
  !
  !   Top-level shortwave kernels
  !
  ! -------------------------------------------------------------------------------------------------
  !
  !  !> Extinction-only shortwave solver i.e. solar direct beam
  !
  ! -------------------------------------------------------------------------------------------------
  pure subroutine sw_solver_noscat(ncol, nlay, ngpt, top_at_1, &
                                   tau, mu0, inc_flux_dir, flux_dir) bind(C, name="rte_sw_solver_noscat")
    integer,                               intent(in ) :: ncol, nlay, ngpt ! Number of columns, layers, g-points
                                                          !! Number of columns, layers, g-points
    logical(wl),                           intent(in ) :: top_at_1
                                                          !! ilay = 1 is the top of the atmosphere?
    real(wp), dimension(ncol,nlay,  ngpt), intent(in ) :: tau
                                                          !! Absorption optical thickness []
    real(wp), dimension(ncol,nlay       ), intent(in ) :: mu0
                                                          !! cosine of solar zenith angle
    real(wp), dimension(ncol,       ngpt), intent(in ) :: inc_flux_dir
                                                          !! Direct beam incident flux [W/m2]
    real(wp), dimension(ncol,nlay+1,ngpt), intent(out) :: flux_dir
                                                          !! Direct-beam flux, spectral [W/m2]

    integer :: ilev, igpt

    ! ------------------------------------
    ! Indexing into arrays for upward and downward propagation depends on the vertical
    !   orientation of the arrays (whether the domain top is at the first or last index)
    ! We write the loops out explicitly so compilers will have no trouble optimizing them.

    ! Downward propagation
    if(top_at_1) then
      ! For the flux at this level, what was the previous level, and which layer has the
      !   radiation just passed through?
      ! layer index = level index - 1
      ! previous level is up (-1)
      do igpt = 1, ngpt
        flux_dir(:,    1,igpt) = inc_flux_dir(:,igpt) * mu0(:,1)
        do ilev = 2, nlay+1
          flux_dir(:,ilev,igpt) = flux_dir(:,ilev-1,igpt) * exp(-tau(:,ilev-1,igpt)/mu0(:,ilev-1))
        end do
      end do
    else
      ! layer index = level index
      ! previous level is up (+1)
      do igpt = 1, ngpt
        flux_dir(:,nlay+1,igpt) = inc_flux_dir(:,igpt) * mu0(:,nlay)
        do ilev = nlay, 1, -1
          flux_dir(:,ilev,igpt) = flux_dir(:,ilev+1,igpt) * exp(-tau(:,ilev,igpt)/mu0(:,ilev))
        end do
      end do
    end if
  end subroutine sw_solver_noscat
  ! -------------------------------------------------------------------------------------------------
  !
  !> Shortwave two-stream calculation:
  !>   compute layer reflectance, transmittance
  !>   compute solar source function for diffuse radiation
  !>   transport
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
    integer,                               intent(in   ) :: ncol, nlay, ngpt
                                                            !! Number of columns, layers, g-points
    logical(wl),                           intent(in   ) :: top_at_1
                                                            !! ilay = 1 is the top of the atmosphere?
    real(wp), dimension(ncol,nlay,  ngpt), intent(in   ) :: tau, ssa, g
                                                            !! Optical thickness, single-scattering albedo, asymmetry parameter []
    real(wp), dimension(ncol,nlay       ), intent(in   ) :: mu0
                                                            !! cosine of solar zenith angle
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_alb_dir, sfc_alb_dif
                                                            !! Spectral surface albedo for direct and diffuse radiation
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: inc_flux_dir
                                                            !! Direct beam incident flux
    real(wp), dimension(ncol,nlay+1,ngpt), target, &
                                           intent(  out) :: flux_up, flux_dn, flux_dir
                                                            !! Fluxes [W/m2]
    logical(wl),                           intent(in   ) :: has_dif_bc
                                                            !! Is a boundary condition for diffuse flux supplied?
    real(wp), dimension(ncol,       ngpt), intent(in   ) :: inc_flux_dif
                                                            !! Boundary condition for diffuse flux [W/m2]
    logical(wl),                           intent(in   ) :: do_broadband
                                                            !! Provide broadband-integrated, not spectrally-resolved, fluxes?
    real(wp), dimension(ncol,nlay+1     ), intent(  out) :: broadband_up, broadband_dn, broadband_dir
                                                            !! Broadband integrated fluxes
    ! -------------------------------------------
    integer :: igpt, top_level, top_layer
    real(wp), dimension(ncol,nlay  )  :: Rdif, Tdif
    real(wp), dimension(ncol,nlay  )  :: source_up, source_dn
    real(wp), dimension(ncol       )  :: source_srf
    ! loc_fluxes hold a single g-point flux if fluxes are being integrated instead of returned
    !   with spectral detail
    real(wp), dimension(ncol,nlay+1), &
                              target  :: loc_flux_up, loc_flux_dn, loc_flux_dir
    ! gpt_fluxes point to calculations for the current g-point
    real(wp), dimension(:,:), pointer :: gpt_flux_up, gpt_flux_dn, gpt_flux_dir
    ! ------------------------------------
    if(top_at_1) then
      top_level = 1
      top_layer = 1
    else
      top_level  = nlay+1
      top_layer  = nlay
    end if
    !
    ! Integrated fluxes need zeroing
    !
    if(do_broadband) then
      call zero_array(ncol, nlay+1, broadband_up )
      call zero_array(ncol, nlay+1, broadband_dn )
      call zero_array(ncol, nlay+1, broadband_dir)
    end if

    do igpt = 1, ngpt
      if(do_broadband) then
        gpt_flux_up  => loc_flux_up
        gpt_flux_dn  => loc_flux_dn
        gpt_flux_dir => loc_flux_dir
      else
        gpt_flux_up  => flux_up (:,:,igpt)
        gpt_flux_dn  => flux_dn (:,:,igpt)
        gpt_flux_dir => flux_dir(:,:,igpt)
      end if
      !
      ! Boundary conditions direct beam...
      !
      gpt_flux_dir(:,top_level) = inc_flux_dir(:,igpt) * mu0(:,top_layer)
      !
      ! ... and diffuse field, using 0 if no BC is provided
      !
      if(has_dif_bc) then
        gpt_flux_dn(:,top_level) = inc_flux_dif(:,igpt)
      else
        gpt_flux_dn(:,top_level) = 0._wp
      end if
      !
      ! Cell properties: transmittance and reflectance for diffuse radiation
      !   Direct-beam and source for diffuse radiation
      !
      call sw_dif_and_source(ncol, nlay, top_at_1, mu0, sfc_alb_dir(:,igpt), &
                             tau(:,:,igpt), ssa(:,:,igpt), g(:,:,igpt),      &
                             Rdif, Tdif, source_dn, source_up, source_srf,  &
                             gpt_flux_dir)
      !
      ! Transport
      !
      call adding(ncol, nlay, top_at_1,            &
                  sfc_alb_dif(:,igpt), Rdif, Tdif, &
                  source_dn, source_up, source_srf, gpt_flux_up, gpt_flux_dn)
      !
      ! adding() computes only diffuse flux; flux_dn is total
      !
      if(do_broadband) then
        broadband_up (:,:) = broadband_up (:,:) + gpt_flux_up (:,:)
        broadband_dn (:,:) = broadband_dn (:,:) + gpt_flux_dn (:,:) + gpt_flux_dir(:,:)
        broadband_dir(:,:) = broadband_dir(:,:)                     + gpt_flux_dir(:,:)
      else
        gpt_flux_dn(:,:) =                        gpt_flux_dn (:,:) + gpt_flux_dir(:,:)
      end if
    end do
  end subroutine sw_solver_2stream
  ! -------------------------------------------------------------------------------------------------
  !
  !   Lower-level longwave kernels
  !
  ! -------------------------------------------------------------------------------------------------
  !
  ! Compute LW source function for upward and downward emission at levels using linear-in-tau assumption
  ! See Clough et al., 1992, doi: 10.1029/92JD01419, Eq 13
  !
  ! ---------------------------------------------------------------
  subroutine lw_source_noscat(ncol, nlay, top_at_1, lay_source, lev_source, tau, trans, &
                              source_dn, source_up)
    integer,                           intent(in) :: ncol, nlay
    logical(wl),                       intent(in) :: top_at_1
    real(wp), dimension(ncol, nlay  ), intent(in) :: lay_source, & ! Planck source at layer center
                                                     tau,        & ! Optical path (tau/mu)
                                                     trans         ! Transmissivity (exp(-tau))
    real(wp), dimension(ncol, nlay+1), intent(in) :: lev_source    ! Planck source at levels (layer edges)
    real(wp), dimension(ncol, nlay  ), target, & 
                                       intent(out):: source_dn, source_up
                                                                   ! Source function at layer edges
                                                                   ! Down at the bottom of the layer, up at the top
    ! --------------------------------
    real(wp), dimension(:,:), pointer :: source_inc, source_dec 
    integer             :: icol, ilay
    real(wp)            :: fact
    real(wp), parameter :: tau_thresh = sqrt(sqrt(epsilon(tau)))
    ! ---------------------------------------------------------------
    if (top_at_1) then 
      source_inc => source_dn 
      source_dec => source_up
    else
      source_inc => source_up
      source_dec => source_dn
    end if 
    do ilay = 1, nlay
      do icol = 1, ncol
      !
      ! Weighting factor. Use 3rd order series expansion when rounding error (~tau^2)
      !   is of order epsilon (smallest difference from 1. in working precision)
      !   Thanks to Peter Blossey (UW) for the idea and Dmitry Alexeev (Nvidia) for suggesting 3rd order
      !
      if(tau(icol, ilay) > tau_thresh) then
        fact = (1._wp - trans(icol,ilay))/tau(icol,ilay) - trans(icol,ilay)
      else
        fact = tau(icol, ilay) * (0.5_wp + tau(icol, ilay) * (- 1._wp/3._wp + tau(icol, ilay) * 1._wp/8._wp ) )
      end if
      !
      ! Equation below is developed in Clough et al., 1992, doi:10.1029/92JD01419, Eq 13
      !
      source_inc(icol,ilay) = (1._wp - trans(icol,ilay)) * lev_source(icol,ilay+1) + &
                              2._wp * fact * (lay_source(icol,ilay) - lev_source(icol,ilay+1))
      source_dec(icol,ilay) = (1._wp - trans(icol,ilay)) * lev_source(icol,ilay ) + &
                              2._wp * fact * (lay_source(icol,ilay) - lev_source(icol,ilay  ))
      !
      ! Even better - omit the layer Planck source (not working so well)
      !
      if(.false.) then 
        source_inc(icol,ilay) = (1._wp - trans(icol,ilay)) * lev_source(icol,ilay+1) + &
                                fact * (lev_source(icol,ilay  ) - lev_source(icol,ilay+1))
        source_dec(icol,ilay) = (1._wp - trans(icol,ilay)) * lev_source(icol,ilay ) + &
                                fact * (lev_source(icol,ilay+1) - lev_source(icol,ilay  ))
      end if 
      end do 
    end do
  end subroutine lw_source_noscat
  ! -------------------------------------------------------------------------------------------------
  !
  ! Longwave no-scattering transport - separate routines for up and down
  !
  ! -------------------------------------------------------------------------------------------------
  subroutine lw_transport_noscat_dn(ncol, nlay, top_at_1,     &
                                   trans, source_dn, radn_dn)
    integer,                          intent(in   ) :: ncol, nlay ! Number of columns, layers, g-points
    logical(wl),                      intent(in   ) :: top_at_1   !
    real(wp), dimension(ncol,nlay  ), intent(in   ) :: trans      ! transmissivity = exp(-tau)
    real(wp), dimension(ncol,nlay  ), intent(in   ) :: source_dn  ! Diffuse radiation emitted by the layer
    real(wp), dimension(ncol,nlay+1), intent(inout) :: radn_dn    ! Radiances [W/m2-str] Top level must contain incident flux boundary condition

    ! ---------------------------------------------------
    ! Local variables
    integer :: ilev
    ! ---------------------------------------------------
    if(top_at_1) then
      !
      ! Top of domain is index 1
      !
      do ilev = 2, nlay+1
        radn_dn(:,ilev) = trans(:,ilev-1)*radn_dn(:,ilev-1) + source_dn(:,ilev-1)
      end do
    else
      !
      ! Top of domain is index nlay+1
      !
      do ilev = nlay, 1, -1
        radn_dn(:,ilev) = trans(:,ilev  )*radn_dn(:,ilev+1) + source_dn(:,ilev)
      end do
    end if
  end subroutine lw_transport_noscat_dn
  ! -------------------------------------------------------------------------------------------------
  subroutine lw_transport_noscat_up(ncol, nlay, top_at_1,     &
                                   trans, source_up, radn_up, do_Jacobians, radn_upJac)
    integer,                          intent(in   ) :: ncol, nlay ! Number of columns, layers, g-points
    logical(wl),                      intent(in   ) :: top_at_1   !
    real(wp), dimension(ncol,nlay  ), intent(in   ) :: trans      ! transmissivity = exp(-tau)
    real(wp), dimension(ncol,nlay  ), intent(in   ) :: source_up  ! Diffuse radiation emitted by the layer
    real(wp), dimension(ncol,nlay+1), intent(inout) :: radn_up    ! Radiances [W/m2-str] Top level must contain incident flux boundary condition
    logical(wl),                      intent(in   ) :: do_Jacobians
    real(wp), dimension(ncol,nlay+1), intent(inout) :: radn_upJac       ! surface temperature Jacobian of Radiances [W/m2-str / K]

    ! ---------------------------------------------------
    ! Local variables
    integer :: ilev
    ! ---------------------------------------------------
    if(top_at_1) then
      !
      ! Top of domain is index 1
      !
      ! Upward propagation
      do ilev = nlay, 1, -1
        radn_up     (:,ilev) = trans(:,ilev  )*radn_up   (:,ilev+1) + source_up(:,ilev)
        if(do_Jacobians) &
          radn_upJac(:,ilev) = trans(:,ilev  )*radn_upJac(:,ilev+1)
      end do
    else
      !
      ! Top of domain is index nlay+1
      !
      ! Upward propagation
      do ilev = 2, nlay+1
        radn_up     (:,ilev) = trans(:,ilev-1) * radn_up   (:,ilev-1) +  source_up(:,ilev-1)
        if(do_Jacobians) &
          radn_upJac(:,ilev) = trans(:,ilev-1) * radn_upJac(:,ilev-1)
      end do
    end if
  end subroutine lw_transport_noscat_up
  ! -------------------------------------------------------------------------------------------------
  ! Upward and (second) downward transport for re-scaled longwave solution
  !   adds adjustment factor based on cloud properties
  !
  !   implementation notice:
  !       the adjustmentFactor computation can be skipped where Cn <= epsilon
  ! -------------------------------------------------------------------------------------------------
  subroutine lw_transport_1rescl(ncol, nlay, top_at_1, &
                                 trans, source_dn, source_up, &
                                 radn_up, radn_dn, An, Cn,&
                                 do_Jacobians, radn_up_Jac)
    integer,                          intent(in   ) :: ncol, nlay ! Number of columns, layers, g-points
    logical(wl),                      intent(in   ) :: top_at_1   !
    real(wp), dimension(ncol,nlay  ), intent(in   ) :: trans      ! transmissivity = exp(-tau)
    real(wp), dimension(ncol,nlay  ), intent(in   ) :: source_dn, &
                                                       source_up  ! Diffuse radiation emitted by the layer
    real(wp), dimension(ncol,nlay+1), intent(inout) :: radn_up    ! Radiances [W/m2-str]
    real(wp), dimension(ncol,nlay+1), intent(inout) :: radn_dn    !Top level must contain incident flux boundary condition
    real(wp), dimension(ncol,nlay),   intent(in   ) :: An, Cn
    logical(wl),                      intent(in   ) :: do_Jacobians
    real(wp), dimension(ncol,nlay+1), intent(inout) :: radn_up_Jac ! Surface temperature Jacobians [W/m2-str/K]
    !
    ! We could in principle compute a downwelling Jacobian too, but it's small
    !   (only a small proportion of LW is scattered) and it complicates code and the API,
    !   so we will not
    !

    ! Local variables
    integer :: ilev, icol
    ! ---------------------------------------------------
    real(wp) :: adjustmentFactor
    if(top_at_1) then
      !
      ! Top of domain is index 1
      !
      ! Upward propagation
      ! adjustment factor is obtained as a solution of 18b of the Tang paper
      ! eqvivalent to Eq.20 of the Tang paper but for linear-in-tau source
      do ilev = nlay, 1, -1
        do icol=1,ncol
          adjustmentFactor = Cn(icol,ilev)*( An(icol,ilev)*radn_dn(icol,ilev) - &
                   trans(icol,ilev)*source_dn(icol,ilev) - source_up(icol,ilev) )
          radn_up (icol,ilev) = trans(icol,ilev)*radn_up(icol,ilev+1) + source_up(icol,ilev) + &
                                adjustmentFactor
        end do
        if(do_Jacobians) &
          radn_up_Jac(:,ilev) = trans(:,ilev)*radn_up_Jac(:,ilev+1)
      end do
      ! Downward propagation
      ! radn_dn_Jac(:,1) = 0._wp
      ! adjustment factor is obtained as a solution of 19 of the Tang paper
      ! eqvivalent to Eq.21 of the Tang paper but for linear-in-tau source
      do ilev = 1, nlay
        ! radn_dn_Jac(:,ilev+1) = trans(:,ilev)*radn_dn_Jac(:,ilev)
        do icol=1,ncol
            adjustmentFactor = Cn(icol,ilev)*( An(icol,ilev)*radn_up(icol,ilev) - &
                     trans(icol,ilev)*source_up(icol,ilev) - source_dn(icol,ilev) )
            radn_dn(icol,ilev+1) = trans(icol,ilev)*radn_dn(icol,ilev) + source_dn(icol,ilev) + &
                                   adjustmentFactor
            ! adjustmentFactor         = Cn(icol,ilev)*An(icol,ilev)*radn_up_Jac(icol,ilev)
            ! radn_dn_Jac(icol,ilev+1) = radn_dn_Jac(icol,ilev+1) + adjustmentFactor
        enddo
      end do
    else
      !
      ! Top of domain is index nlay+1
      !
      ! Upward propagation
      ! adjustment factor is obtained as a solution of 18b of the Tang paper
      ! eqvivalent to Eq.20 of the Tang paper but for linear-in-tau source
      do ilev = 1, nlay
        radn_up      (:,ilev+1) = trans(:,ilev) * radn_up    (:,ilev) +  source_up(:,ilev)
        do icol=1,ncol
            adjustmentFactor = Cn(icol,ilev)*( An(icol,ilev)*radn_dn(icol,ilev+1) - &
                     trans(icol,ilev)*source_dn(icol,ilev) - source_up(icol,ilev) )
            radn_up(icol,ilev+1) = trans(icol,ilev)*radn_up(icol,ilev) +  source_up(icol,ilev) + &
                                   adjustmentFactor
        enddo
        if(do_Jacobians) &
          radn_up_Jac(:,ilev+1) = trans(:,ilev) * radn_up_Jac(:,ilev)
      end do

      ! Downward propagation
      ! adjustment factor is obtained as a solution of 19 of the Tang paper
      ! eqvivalent to Eq.21 of the Tang paper but for linear-in-tau source
      ! radn_dn_Jac(:,nlay+1) = 0._wp
      do ilev = nlay, 1, -1
        ! radn_dn_Jac(:,ilev) = trans(:,ilev)*radn_dn_Jac(:,ilev+1)
        do icol=1,ncol
            adjustmentFactor = Cn(icol,ilev)*( An(icol,ilev)*radn_up(icol,ilev) - &
                     trans(icol,ilev)*source_up(icol,ilev) - source_dn(icol,ilev) )
            radn_dn(icol,ilev)  = trans(icol,ilev)*radn_dn(icol,ilev+1) + source_dn(icol,ilev) + &
                                  adjustmentFactor
            ! adjustmentFactor    = Cn(icol,ilev)*An(icol,ilev)*radn_up_Jac(icol,ilev)
            ! radn_dn_Jac(icol,ilev) = radn_dn_Jac(icol,ilev) + adjustmentFactor
        enddo
      end do
    end if
  end subroutine lw_transport_1rescl
! -------------------------------------------------------------------------------------------------
  !
  ! Longwave two-stream solutions to diffuse reflectance and transmittance for a layer
  !    with optical depth tau, single scattering albedo w0, and asymmetery parameter g.
  !
  ! Equations are developed in Meador and Weaver, 1980,
  !    doi:10.1175/1520-0469(1980)037<0630:TSATRT>2.0.CO;2
  !
  ! -------------------------------------------------------------------------------------------------
  pure subroutine lw_two_stream(ncol, nlay, tau, w0, g, &
                                gamma1, gamma2, Rdif, Tdif)
    integer,                        intent(in)  :: ncol, nlay
    real(wp), dimension(ncol,nlay), intent(in)  :: tau, w0, g
    real(wp), dimension(ncol,nlay), intent(out) :: gamma1, gamma2, Rdif, Tdif

    ! -----------------------
    integer  :: i, j

    ! Variables used in Meador and Weaver
    real(wp) :: k(ncol)

    ! Ancillary variables
    real(wp) :: RT_term(ncol)
    real(wp) :: exp_minusktau(ncol), exp_minus2ktau(ncol)

    real(wp), parameter :: LW_diff_sec = 1.66  ! 1./cos(diffusivity angle)
    ! ---------------------------------
    do j = 1, nlay
      do i = 1, ncol
        !
        ! Coefficients differ from SW implementation because the phase function is more isotropic
        !   Here we follow Fu et al. 1997, doi:10.1175/1520-0469(1997)054<2799:MSPITI>2.0.CO;2
        !   and use a diffusivity sec of 1.66
        !
        gamma1(i,j)= LW_diff_sec * (1._wp - 0.5_wp * w0(i,j) * (1._wp + g(i,j))) ! Fu et al. Eq 2.9
        gamma2(i,j)= LW_diff_sec *          0.5_wp * w0(i,j) * (1._wp - g(i,j))  ! Fu et al. Eq 2.10
        ! Eq 18;  k = SQRT(gamma1**2 - gamma2**2), limited below to avoid div by 0.
        !   k = 0 for isotropic, conservative scattering; this lower limit on k
        !   gives relative error with respect to conservative solution
        !   of < 0.1% in Rdif down to tau = 10^-9
        k(i) = sqrt(max((gamma1(i,j) - gamma2(i,j)) * (gamma1(i,j) + gamma2(i,j)), 1.e-12_wp))
      end do

      ! Written to encourage vectorization of exponential
      exp_minusktau(1:ncol) = exp(-tau(1:ncol,j)*k(1:ncol))

      !
      ! Diffuse reflection and transmission
      !
      do i = 1, ncol
        exp_minus2ktau(i) = exp_minusktau(i) * exp_minusktau(i)

        ! Refactored to avoid rounding errors when k, gamma1 are of very different magnitudes
        RT_term(i) = 1._wp / (k     (i  ) * (1._wp + exp_minus2ktau(i))  + &
                              gamma1(i,j) * (1._wp - exp_minus2ktau(i)) )

        ! Equation 25
        Rdif(i,j) = RT_term(i) * gamma2(i,j) * (1._wp - exp_minus2ktau(i))

        ! Equation 26
        Tdif(i,j) = RT_term(i) * 2._wp * k(i) * exp_minusktau(i)
      end do

    end do
  end subroutine lw_two_stream
  ! ---------------------------------------------------------------
  !
  ! Compute LW source function for upward and downward emission at levels using linear-in-tau assumption
  !   This version straight from ECRAD
  !   Source is provided as W/m2-str; factor of pi converts to flux units
  !
  ! ---------------------------------------------------------------
  subroutine lw_source_2str(ncol, nlay, top_at_1,   &
                            sfc_emis, sfc_src,      &
                            lay_source, lev_source, &
                            gamma1, gamma2, rdif, tdif, tau, source_dn, source_up, source_sfc) &
                            bind (C, name="rte_lw_source_2str")
    integer,                         intent(in) :: ncol, nlay
    logical(wl),                     intent(in) :: top_at_1
    real(wp), dimension(ncol      ), intent(in) :: sfc_emis, sfc_src
    real(wp), dimension(ncol, nlay), intent(in) :: lay_source,    & ! Planck source at layer center
                                                   tau,           & ! Optical depth (tau)
                                                   gamma1, gamma2,& ! Coupling coefficients
                                                   rdif, tdif       ! Layer reflectance and transmittance
    real(wp), dimension(ncol, nlay+1), target, &
                                     intent(in)  :: lev_source       ! Planck source at layer edges
    real(wp), dimension(ncol, nlay), intent(out) :: source_dn, source_up
    real(wp), dimension(ncol      ), intent(out) :: source_sfc      ! Source function for upward radation at surface

    integer             :: icol, ilay
    real(wp)            :: Z, Zup_top, Zup_bottom, Zdn_top, Zdn_bottom
    real(wp), dimension(:), pointer :: lev_source_bot, lev_source_top
    ! ---------------------------------------------------------------
    do ilay = 1, nlay
      if(top_at_1) then
        lev_source_top => lev_source(:,ilay)
        lev_source_bot => lev_source(:,ilay+1)
      else
        lev_source_top => lev_source(:,ilay+1)
        lev_source_bot => lev_source(:,ilay)
      end if
      do icol = 1, ncol
        if (tau(icol,ilay) > 1.0e-8_wp) then
          !
          ! Toon et al. (JGR 1989) Eqs 26-27
          !
          Z = (lev_source_bot(icol)-lev_source_top(icol)) / (tau(icol,ilay)*(gamma1(icol,ilay)+gamma2(icol,ilay)))
          Zup_top        =  Z + lev_source_top(icol)
          Zup_bottom     =  Z + lev_source_bot(icol)
          Zdn_top        = -Z + lev_source_top(icol)
          Zdn_bottom     = -Z + lev_source_bot(icol)
          source_up(icol,ilay) = pi * (Zup_top    - rdif(icol,ilay) * Zdn_top    - tdif(icol,ilay) * Zup_bottom)
          source_dn(icol,ilay) = pi * (Zdn_bottom - rdif(icol,ilay) * Zup_bottom - tdif(icol,ilay) * Zdn_top)
        else
          source_up(icol,ilay) = 0._wp
          source_dn(icol,ilay) = 0._wp
        end if
      end do
    end do
    do icol = 1, ncol
      source_sfc(icol) = pi * sfc_emis(icol) * sfc_src(icol)
    end do
  end subroutine lw_source_2str
  ! -------------------------------------------------------------------------------------------------
  !
  !   Lower-level shortwave kernels
  !
  ! -------------------------------------------------------------------------------------------------
  !
  ! Two-stream solutions to diffuse reflectance and transmittance for a layer
  !    with optical depth tau, single scattering albedo w0, and asymmetery parameter g.
  ! Direct reflectance and transmittance used to compute direct beam source for diffuse radiation
  !   in layers and at surface; report direct beam as a byproduct
  ! Computing the direct-beam source for diffuse radiation at the same time as R and T for
  !   direct radiation reduces memory traffic and use.
  !
  ! Equations are developed in Meador and Weaver, 1980,
  !    doi:10.1175/1520-0469(1980)037<0630:TSATRT>2.0.CO;2
  !
  ! -------------------------------------------------------------------------------------------------
  pure subroutine sw_dif_and_source(ncol, nlay, top_at_1, mu0, sfc_albedo, &
                                    tau, w0, g,  &
                                    Rdif, Tdif, source_dn, source_up, source_sfc, flux_dn_dir) bind (C, name="rte_sw_source_dir")
    integer,                          intent(in   ) :: ncol, nlay
    logical(wl),                      intent(in   ) :: top_at_1
    real(wp), dimension(ncol       ), intent(in   ) :: sfc_albedo          ! surface albedo for direct radiation
    real(wp), dimension(ncol,nlay  ), intent(in   ) :: tau, w0, g, mu0
    real(wp), dimension(ncol,nlay  ), intent(  out) :: Rdif, Tdif, source_dn, source_up
    real(wp), dimension(ncol       ), intent(  out) :: source_sfc ! Source function for upward radation at surface
    real(wp), dimension(ncol,nlay+1), target, &
                                      intent(inout) :: flux_dn_dir ! Direct beam flux

    ! -----------------------
    integer  :: i, j

    ! Variables used in Meador and Weaver
    real(wp) :: gamma1, gamma2, gamma3, gamma4, alpha1, alpha2


    ! Ancillary variables
    real(wp), parameter :: min_k = 1.e4_wp * epsilon(1._wp) ! Suggestion from Chiel van Heerwaarden
    real(wp), parameter :: min_mu0 = sqrt(epsilon(1._wp))
    real(wp) :: k, exp_minusktau, k_mu, k_gamma3, k_gamma4
    real(wp) :: RT_term, exp_minus2ktau
    real(wp) :: Rdir, Tdir, Tnoscat
    real(wp), pointer, dimension(:) :: dir_flux_inc, dir_flux_trans
    integer  :: lay_index
    real(wp) :: tau_s, w0_s, g_s, mu0_s
    ! ---------------------------------

    do j = 1, nlay
      if(top_at_1) then
        lay_index      =  j
        dir_flux_inc   => flux_dn_dir(:,lay_index  )
        dir_flux_trans => flux_dn_dir(:,lay_index+1)
      else
        lay_index      =  nlay-j+1
        dir_flux_inc   => flux_dn_dir(:,lay_index+1)
        dir_flux_trans => flux_dn_dir(:,lay_index  )
      end if

      !$OMP SIMD
      do i = 1, ncol
        !
        ! Scalars
        !
        tau_s = tau(i, lay_index)
        w0_s  = w0 (i, lay_index)
        g_s   = g  (i, lay_index)
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
        exp_minusktau = exp(-tau_s*k)
        exp_minus2ktau = exp_minusktau * exp_minusktau

        ! Refactored to avoid rounding errors when k, gamma1 are of very different magnitudes
        RT_term = 1._wp / (k      * (1._wp + exp_minus2ktau)  + &
                           gamma1 * (1._wp - exp_minus2ktau) )
        ! Equation 25
        Rdif(i,lay_index) = RT_term * gamma2 * (1._wp - exp_minus2ktau)

        ! Equation 26
        Tdif(i,lay_index) = RT_term * 2._wp * k * exp_minusktau

        !
        ! On a round earth, where mu0 can increase with depth in the atmosphere,
        !   levels with mu0 <= 0 have no direct beam and hence no source for diffuse light
        !   Compute transmission and reflection using a nominal value but mask out later 
        !
        mu0_s = max(min_mu0, mu0(i, lay_index)) 
        k_mu     = k * mu0_s
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

        source_up(i,lay_index) =    Rdir * dir_flux_inc(i)
        source_dn(i,lay_index) =    Tdir * dir_flux_inc(i)
        dir_flux_trans(i)      = Tnoscat * dir_flux_inc(i)
      end do
    end do
    !
    ! T and R for the direct beam are computed using nominal values even when the 
    !   sun is below the horizon (mu0 < 0); set those values back to zero
    ! This won't be efficient if many nighttime columns are passed
    !
    source_sfc(:) = merge(dir_flux_trans(:)*sfc_albedo(:), & 
                          0._wp, mu0(:,lay_index) > 0._wp) 
    where(mu0(:,:) <= 0._wp) 
      source_up(:,:) = 0._wp
      source_dn(:,:) = 0._wp
    end where 

  end subroutine sw_dif_and_source
! ---------------------------------------------------------------
!
! Transport of diffuse radiation through a vertically layered atmosphere.
!   Equations are after Shonk and Hogan 2008, doi:10.1175/2007JCLI1940.1 (SH08)
!   This routine is shared by longwave and shortwave
!
! -------------------------------------------------------------------------------------------------
subroutine adding(ncol, nlay, top_at_1, &
                  albedo_sfc,           &
                  rdif, tdif,           &
                  src_dn, src_up, src_sfc, &
                  flux_up, flux_dn)
  integer,                          intent(in   ) :: ncol, nlay
  logical(wl),                      intent(in   ) :: top_at_1
  real(wp), dimension(ncol       ), intent(in   ) :: albedo_sfc
  real(wp), dimension(ncol,nlay  ), intent(in   ) :: rdif, tdif
  real(wp), dimension(ncol,nlay  ), intent(in   ) :: src_dn, src_up
  real(wp), dimension(ncol       ), intent(in   ) :: src_sfc
  real(wp), dimension(ncol,nlay+1), intent(  out) :: flux_up
  ! intent(inout) because top layer includes incident flux
  real(wp), dimension(ncol,nlay+1), intent(inout) :: flux_dn
  ! ------------------
  integer :: ilev
  real(wp), dimension(ncol,nlay+1)  :: albedo, &  ! reflectivity to diffuse radiation below this level
                                                  ! alpha in SH08
                                       src        ! source of diffuse upwelling radiation from emission or
                                                  ! scattering of direct beam
                                                  ! G in SH08
  real(wp), dimension(ncol,nlay  )  :: denom      ! beta in SH08
  ! ------------------
  !
  ! Indexing into arrays for upward and downward propagation depends on the vertical
  !   orientation of the arrays (whether the domain top is at the first or last index)
  ! We write the loops out explicitly so compilers will have no trouble optimizing them.
  !
  if(top_at_1) then
    ilev = nlay + 1
    ! Albedo of lowest level is the surface albedo...
    albedo(:,ilev)  = albedo_sfc(:)
    ! ... and source of diffuse radiation is surface emission
    src(:,ilev) = src_sfc(:)

    !
    ! From bottom to top of atmosphere --
    !   compute albedo and source of upward radiation
    !
    do ilev = nlay, 1, -1
      denom(:, ilev) = 1._wp/(1._wp - rdif(:,ilev)*albedo(:,ilev+1))                 ! Eq 10
      albedo(:,ilev) = rdif(:,ilev) + &
                       tdif(:,ilev)*tdif(:,ilev) * albedo(:,ilev+1) * denom(:,ilev) ! Equation 9
      !
      ! Equation 11 -- source is emitted upward radiation at top of layer plus
      !   radiation emitted at bottom of layer,
      !   transmitted through the layer and reflected from layers below (tdiff*src*albedo)
      !
      src(:,ilev) =  src_up(:, ilev) + &
                     tdif(:,ilev) * denom(:,ilev) *       &
                       (src(:,ilev+1) + albedo(:,ilev+1)*src_dn(:,ilev))
    end do

    ! Eq 12, at the top of the domain upwelling diffuse is due to ...
    ilev = 1
    flux_up(:,ilev) = flux_dn(:,ilev) * albedo(:,ilev) + & ! ... reflection of incident diffuse and
                      src(:,ilev)                          ! emission from below

    !
    ! From the top of the atmosphere downward -- compute fluxes
    !
    do ilev = 2, nlay+1
      flux_dn(:,ilev) = (tdif(:,ilev-1)*flux_dn(:,ilev-1) + &  ! Equation 13
                         rdif(:,ilev-1)*src(:,ilev) +       &
                         src_dn(:,ilev-1)) * denom(:,ilev-1)
      flux_up(:,ilev) = flux_dn(:,ilev) * albedo(:,ilev) + & ! Equation 12
                        src(:,ilev)
    end do
  else
    ilev = 1
    ! Albedo of lowest level is the surface albedo...
    albedo(:,ilev)  = albedo_sfc(:)
    ! ... and source of diffuse radiation is surface emission
    src(:,ilev) = src_sfc(:)

    !
    ! From bottom to top of atmosphere --
    !   compute albedo and source of upward radiation
    !
    do ilev = 1, nlay
      denom(:, ilev  ) = 1._wp/(1._wp - rdif(:,ilev)*albedo(:,ilev))                ! Eq 10
      albedo(:,ilev+1) = rdif(:,ilev) + &
                         tdif(:,ilev)*tdif(:,ilev) * albedo(:,ilev) * denom(:,ilev) ! Equation 9
      !
      ! Equation 11 -- source is emitted upward radiation at top of layer plus
      !   radiation emitted at bottom of layer,
      !   transmitted through the layer and reflected from layers below (tdiff*src*albedo)
      !
      src(:,ilev+1) =  src_up(:, ilev) +  &
                       tdif(:,ilev) * denom(:,ilev) *       &
                       (src(:,ilev) + albedo(:,ilev)*src_dn(:,ilev))
    end do

    ! Eq 12, at the top of the domain upwelling diffuse is due to ...
    ilev = nlay+1
    flux_up(:,ilev) = flux_dn(:,ilev) * albedo(:,ilev) + & ! ... reflection of incident diffuse and
                      src(:,ilev)                          ! scattering by the direct beam below

    !
    ! From the top of the atmosphere downward -- compute fluxes
    !
    do ilev = nlay, 1, -1
      flux_dn(:,ilev) = (tdif(:,ilev)*flux_dn(:,ilev+1) + &  ! Equation 13
                         rdif(:,ilev)*src(:,ilev) + &
                         src_dn(:, ilev)) * denom(:,ilev)
      flux_up(:,ilev) = flux_dn(:,ilev) * albedo(:,ilev) + & ! Equation 12
                        src(:,ilev)

    end do
  end if
end subroutine adding
end module mo_rte_solver_kernels
