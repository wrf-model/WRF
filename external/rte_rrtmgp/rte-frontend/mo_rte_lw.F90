! This code is part of Radiative Transfer for Energetics (RTE)
!
! Contacts: Robert Pincus and Eli Mlawer
! email:  rrtmgp@aer.com
!
! Copyright 2015-  Atmospheric and Environmental Research,
!    Regents of the University of Colorado,
!    Trustees of Columbia University in the City of New York
! All right reserved.
!
! Use and duplication is permitted under the terms of the
!    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause
! -------------------------------------------------------------------------------------------------
!
!> Compute longwave radiative fluxes
!>
!>  Contains a single routine to compute direct and diffuse fluxes of solar radiation given
!>
!> - atmospheric optical properties, spectrally-resolved via one of the sub-classes of
!>     [[mo_optical_props(module):ty_optical_props_arry(type)]] in module [[mo_optical_props]]
!      (ty_optical_props_arry in module mo_optical_props)
!> - information about vertical ordering
!> - internal Planck source functions, defined per g-point on the same spectral grid at the atmosphere,
!>     via [[mo_source_functions(module):ty_source_func_lw(type)]] in module [[mo_source_functions]]
!      (ty_source_func_lw in module mo_source_functions)
!> -  boundary conditions: surface emissivity defined per band
!> -  optionally, a boundary condition for incident diffuse radiation
!> -  optionally, an integer number of angles at which to do Gaussian quadrature if scattering is neglected
!>
!> If optical properties are supplied via class ty_optical_props_1scl (absorption optical thickenss only)
!>    ([[mo_optical_props(module):ty_optical_props_1scl(type)]] in module [[mo_optical_props]])
!>    then an emission/absorption solver is called.
!>    If optical properties are supplied via class ty_optical_props_2str
!>    ([[mo_optical_props(module):ty_optical_props_2str(type)]] in module [[mo_optical_props]])
!>    fluxes are computed via a rescaling by default or, optionally, using two-stream calculations and adding.
!>
!> Users must ensure that emissivity is on the same spectral grid as the optical properties.
!>
!> Final output is via user-extensible ty_fluxes
!> ([[mo_fluxes(module):ty_fluxes(type)]] in module [[mo_fluxes]])
!> which must reduce the detailed spectral fluxes to whatever summary the user needs
!
!> The routine does error checking and choses which lower-level kernel to invoke based on
!>   what kinds of optical properties are supplied
!
! -------------------------------------------------------------------------------------------------
module mo_rte_lw
  use mo_rte_kind,      only: wp, wl
  use mo_rte_config,    only: check_extents, check_values
  use mo_rte_util_array,only: zero_array
  use mo_rte_util_array_validation, & 
                        only: any_vals_less_than, any_vals_outside, extents_are
  use mo_optical_props, only: ty_optical_props, &
                              ty_optical_props_arry, ty_optical_props_1scl, ty_optical_props_2str, ty_optical_props_nstr
  use mo_source_functions,   &
                        only: ty_source_func_lw
  use mo_fluxes,        only: ty_fluxes, ty_fluxes_broadband
  use mo_rte_solver_kernels, &
                        only: lw_solver_noscat, lw_solver_2stream
  implicit none
  private

  public :: rte_lw
contains
  ! --------------------------------------------------
  !
  ! Interface using only optical properties and source functions as inputs; fluxes as outputs.
  !
  ! --------------------------------------------------
  function rte_lw(optical_props, top_at_1, &
                  sources, sfc_emis,       &
                  fluxes,                  &
                  inc_flux, n_gauss_angles, use_2stream, &
                  lw_Ds, flux_up_Jac) result(error_msg)
    class(ty_optical_props_arry), intent(in   ) :: optical_props
      !! Set of optical properties as one or more arrays
    logical,                      intent(in   ) :: top_at_1
      !! Is the top of the domain at index 1? (if not, ordering is bottom-to-top)
    type(ty_source_func_lw),      intent(in   ) :: sources
      !! Derived type with Planck source functions
    real(wp), dimension(:,:),     intent(in   ) :: sfc_emis
      !! emissivity at surface [] (nband, ncol)
    class(ty_fluxes),             intent(inout) :: fluxes
      !! Dervied type for computing spectral integrals from g-point fluxes.
      !! Default computes broadband fluxes at all levels if output arrays are defined. Can be extended per user desires.
    real(wp), dimension(:,:),   &
                target, optional, intent(in   ) :: inc_flux
      !! incident flux at domain top [W/m2] (ncol, ngpts)
    integer,            optional, intent(in   ) :: n_gauss_angles
      !! Number of angles used in Gaussian quadrature (max 3, no-scattering solution)
    logical,            optional, intent(in   ) :: use_2stream
      !! When 2-stream parameters (tau/ssa/g) are provided, use 2-stream methods
      !! Default is to use re-scaled longwave transport
    real(wp), dimension(:,:),   &
                      optional,   intent(in   ) :: lw_Ds
      !! User-specifed 1/cos of transport angle per col, g-point
    real(wp), dimension(:,:), target,  &
                      optional,   intent(inout) :: flux_up_Jac
      !! surface temperature flux  Jacobian [W/m2/K] (ncol, nlay+1)
    character(len=128)                          :: error_msg
      !! If empty, calculation was successful
    ! --------------------------------
    !
    ! Local variables
    !
    integer      :: ncol, nlay, ngpt, nband
    integer      :: icol, ilev, igpt,       imu
    integer      :: n_quad_angs
    logical(wl)  :: using_2stream, do_Jacobians, do_broadband
    real(wp), dimension(:,:),   allocatable   :: sfc_emis_gpt
    real(wp), dimension(:,:,:), allocatable   :: secants
    real(wp), dimension(:,:),   pointer       :: jacobian
    real(wp), dimension(optical_props%get_ncol(),           &
                        optical_props%get_nlay()+1), target &
                                            :: decoy2D ! Used for optional outputs - needs to be full size.
    ! Memory needs to be allocated for the full g-point fluxes even if they aren't
    !    used later because a) the GPU kernels use this memory to work in parallel and
    !    b) the fluxes are intent(out) in the solvers
    ! Shortwave solver takes a different approach since three fields are needed
    real(wp), dimension(optical_props%get_ncol(),   &
                        optical_props%get_nlay()+1, &
                        optical_props%get_ngpt())   &
                                            :: gpt_flux_up, gpt_flux_dn
    real(wp), dimension(:,:),   pointer     :: flux_dn_loc, flux_up_loc
    real(wp), dimension(:,:),   pointer     :: inc_flux_diffuse
    ! --------------------------------------------------
    !
    ! Weights and angle secants for "Gauss-Jacobi-5" quadrature.
    !   Values from Table 1, R. J. Hogan 2023, doi:10.1002/qj.4598
    !
    integer,  parameter :: max_gauss_pts = 4
    real(wp), parameter,                         &
      dimension(max_gauss_pts, max_gauss_pts) :: &
        ! 
        ! Values provided are for mu = cos(theta); we require the inverse
        !
        gauss_Ds  = 1._wp / & 
                    RESHAPE([0.6096748751_wp, huge(1._wp)    , huge(1._wp)    , huge(1._wp),      &  
                             0.2509907356_wp, 0.7908473988_wp, huge(1._wp)    , huge(1._wp),      &
                             0.1024922169_wp, 0.4417960320_wp, 0.8633751621_wp, huge(1._wp),      &
                             0.0454586727_wp, 0.2322334416_wp, 0.5740198775_wp, 0.9030775973_wp], &
                            [max_gauss_pts, max_gauss_pts]),              &
        gauss_wts = RESHAPE([1._wp,           0._wp,           0._wp,           0._wp, &
                             0.2300253764_wp, 0.7699746236_wp, 0._wp,           0._wp, &
                             0.0437820218_wp, 0.3875796738_wp, 0.5686383044_wp, 0._wp, &
                             0.0092068785_wp, 0.1285704278_wp, 0.4323381850_wp, 0.4298845087_wp], &
                             [max_gauss_pts, max_gauss_pts])
    ! ------------------------------------------------------------------------------------
    ncol  = optical_props%get_ncol()
    nlay  = optical_props%get_nlay()
    ngpt  = optical_props%get_ngpt()
    nband = optical_props%get_nband()
    do_Jacobians = present(flux_up_Jac)
    error_msg = ""

    ! ------------------------------------------------------------------------------------
    !
    ! Error checking -- input consistency of sizes and validity of values

    if(.not. fluxes%are_desired()) &
      error_msg = "rte_lw: no space allocated for fluxes"

    if (do_Jacobians .and. check_extents) then
      if( .not. extents_are(flux_up_Jac, ncol, nlay+1)) &
        error_msg = "rte_lw: flux Jacobian inconsistently sized"
    endif

    if (check_extents) then
      !
      ! Source functions
      !
      if(any([sources%get_ncol(), sources%get_nlay(), sources%get_ngpt()]  /= [ncol, nlay, ngpt])) &
        error_msg = "rte_lw: sources and optical properties inconsistently sized"
      !
      ! Surface emissivity
      !
      if(.not. extents_are(sfc_emis, nband, ncol)) &
        error_msg = "rte_lw: sfc_emis inconsistently sized"
      !
      ! Incident flux, if present
      !
      if(present(inc_flux)) then
        if(.not. extents_are(inc_flux, ncol, ngpt)) &
          error_msg = "rte_lw: inc_flux inconsistently sized"
      end if
      if (present(lw_Ds)) then
        if(.not. extents_are(lw_Ds, ncol, ngpt)) &
          error_msg = "rte_lw: lw_Ds inconsistently sized"
      end if
    end if

    if(check_values) then
      if(any_vals_outside(sfc_emis, 0._wp, 1._wp)) &
        error_msg = "rte_lw: sfc_emis has values < 0 or > 1"

      if(present(inc_flux)) then
        if(any_vals_less_than(inc_flux, 0._wp)) &
          error_msg = "rte_lw: inc_flux has values < 0"
      end if

      if (present(lw_Ds)) then
        if(any_vals_less_than(lw_Ds, 1._wp)) &
          error_msg = "rte_lw: one or more values of lw_Ds < 1."
      end if

      if(present(n_gauss_angles)) then
        if(n_gauss_angles > max_gauss_pts) &
          error_msg = "rte_lw: asking for too many quadrature points for no-scattering calculation"
        if(n_gauss_angles < 1) &
          error_msg = "rte_lw: have to ask for at least one quadrature point for no-scattering calculation"
      end if
    end if
    if(len_trim(error_msg) > 0) return

    !
    ! Number of quadrature points for no-scattering calculation
    !
    n_quad_angs = 1
    if(present(n_gauss_angles)) n_quad_angs = n_gauss_angles
    !
    ! Optionally - use 2-stream methods when low-order scattering properties are provided?
    !
    using_2stream = .false.
    if(present(use_2stream)) using_2stream = use_2stream

    !
    ! Checking that optional arguments are consistent with one another and with optical properties
    !
    select type (optical_props)
      class is (ty_optical_props_1scl)
        if (using_2stream) &
          error_msg = "rte_lw: can't use two-stream methods with only absorption optical depth"
        if(present(lw_Ds) .and. n_quad_angs /= 1) &
            error_msg = "rte_lw: providing lw_Ds incompatible with specifying n_gauss_angles"
      class is (ty_optical_props_2str)
        if (present(lw_Ds)) &
          error_msg = "rte_lw: lw_Ds not valid when providing scattering optical properties"
        if (using_2stream .and. n_quad_angs /= 1) &
          error_msg = "rte_lw: using_2stream=true incompatible with specifying n_gauss_angles"
        if (using_2stream .and. do_Jacobians) &
          error_msg = "rte_lw: can't provide Jacobian of fluxes w.r.t surface temperature with 2-stream"
      class default
        error_msg =  "rte_lw: lw_solver(...ty_optical_props_nstr...) not yet implemented"
    end select

    if(len_trim(error_msg) > 0) then
      if(len_trim(optical_props%get_name()) > 0) &
        error_msg = trim(optical_props%get_name()) // ': ' // trim(error_msg)
      return
    end if

    ! ------------------------------------------------------------------------------------
    !  Boundary conditions
    !    Lower boundary condition -- expand surface emissivity by band to gpoints
    !
    allocate(sfc_emis_gpt(ncol,         ngpt))

    !   Upper boundary condition -  use values in optional arg or be set to 0
    !
    if (present(inc_flux)) then
      inc_flux_diffuse => inc_flux
      !$acc        enter data copyin(   inc_flux_diffuse)
      !$omp target enter data map(to:   inc_flux_diffuse)
    else
      allocate(inc_flux_diffuse(ncol, ngpt))
      !$acc        enter data create(   inc_flux_diffuse)
      !$omp target enter data map(alloc:inc_flux_diffuse)
      call zero_array(ncol, ngpt, inc_flux_diffuse)
    end if

    ! ------------------------------------------------------------------------------------
    if(do_Jacobians) then
      jacobian => flux_up_Jac
    else
      jacobian => decoy2D
    end if

    select type(fluxes)
      !
      ! Broadband fluxes are treated as a special case within the solvers; memory
      !   for both up and down fluxes needs to be available even if the user doesn't
      !   want one of them
      !
      type is (ty_fluxes_broadband)
        do_broadband = .true._wl
        !
        ! Broadband fluxes class has three possible outputs; allocate memory for local use
        !   if one or more haven't been requested
        !
        if(associated(fluxes%flux_up)) then
          flux_up_loc => fluxes%flux_up
        else
          allocate(flux_up_loc(ncol, nlay+1))
        end if
        if(associated(fluxes%flux_dn)) then
          flux_dn_loc => fluxes%flux_dn
        else
          allocate(flux_dn_loc(ncol, nlay+1))
        end if
        !$acc        enter data create(   flux_up_loc, flux_dn_loc)
        !$omp target enter data map(alloc:flux_up_loc, flux_dn_loc)
      class default
        !
        ! If broadband integrals aren't being computed, allocate working space
        !   and decoy addresses for spectrally-integrated fields
        !
        do_broadband = .false._wl
        flux_up_loc  => decoy2D
        flux_dn_loc  => decoy2D
    end select

    !
    ! Compute the radiative transfer...
    !
    !$acc        data create(   sfc_emis_gpt, flux_up_loc, flux_dn_loc, gpt_flux_up, gpt_flux_dn)
    !$omp target data map(alloc:sfc_emis_gpt, flux_up_loc, flux_dn_loc, gpt_flux_up, gpt_flux_dn)
    call expand_and_transpose(optical_props, sfc_emis, sfc_emis_gpt)
    if(check_values) error_msg =  optical_props%validate()
    if(len_trim(error_msg) == 0) then ! Can't do an early return within OpenACC/MP data regions
      select type (optical_props)
        class is (ty_optical_props_1scl)
          !
          ! No scattering two-stream calculation
          !
          !
          ! Secant of radiation angle - either user-supplied, one per g-point, or
          !   taken from first-order Gaussian quadrate and applied to all columns a g-points
          !
          allocate(secants(ncol, ngpt, n_quad_angs))
          !$acc        data create(   secants)
          !$omp target data map(alloc:secants)
          if (present(lw_Ds)) then
            !$acc                         parallel loop    collapse(2) copyin(lw_Ds)
            !$omp target teams distribute parallel do simd collapse(2)
            ! nmu is 1
            do igpt = 1, ngpt
              do icol = 1, ncol
                secants(icol,igpt,1) = lw_Ds(icol,igpt)
              end do
            end do
          else
            !
            !   Is there an alternative to making ncol x ngpt copies of each value?
            !
            !$acc                         parallel loop    collapse(3)
            !$omp target teams distribute parallel do simd collapse(3)
            do imu = 1, n_quad_angs
              do igpt = 1, ngpt
                do icol = 1, ncol
                  secants(icol,igpt,imu) = gauss_Ds(imu,n_quad_angs)
                end do
              end do
            end do
          end if
          call lw_solver_noscat(ncol, nlay, ngpt,                 &
                                logical(top_at_1, wl), n_quad_angs,         &
                                secants, gauss_wts(1:n_quad_angs,n_quad_angs), &
                                optical_props%tau,                 &
                                sources%lay_source,                &
                                sources%lev_source,                &
                                sfc_emis_gpt, sources%sfc_source,  &
                                inc_flux_diffuse,                  &
                                gpt_flux_up, gpt_flux_dn,          &
                                do_broadband, flux_up_loc, flux_dn_loc,     &
                                logical(do_Jacobians, wl), sources%sfc_source_Jac, jacobian, &
                                logical(.false., wl),  optical_props%tau, optical_props%tau)
                                                      ! The last two arguments won't be used since the
                                                      ! third-to-last is .false. but need valid addresses
          !$acc        end data
          !$omp end target data
        class is (ty_optical_props_2str)
          if (using_2stream) then
            !
            ! two-stream calculation with scattering
            !
            call lw_solver_2stream(ncol, nlay, ngpt, logical(top_at_1, wl), &
                                   optical_props%tau, optical_props%ssa, optical_props%g, &
                                   sources%lay_source, sources%lev_source,                &
                                   sfc_emis_gpt, sources%sfc_source,       &
                                   inc_flux_diffuse,                       &
                                   gpt_flux_up, gpt_flux_dn)
          else
            allocate(secants(ncol, ngpt, n_quad_angs))
            !$acc        data create(   secants)
            !$omp target data map(alloc:secants)
            !$acc                         parallel loop    collapse(3)
            !$omp target teams distribute parallel do simd collapse(3)
            do imu = 1, n_quad_angs
              do igpt = 1, ngpt
                do icol = 1, ncol
                  secants(icol,igpt,imu) = gauss_Ds(imu,n_quad_angs)
                end do
              end do
            end do
            !
            ! Re-scaled solution to account for scattering
            !
            call lw_solver_noscat(ncol, nlay, ngpt,                 &
                                  logical(top_at_1, wl), n_quad_angs,         &
                                  secants, gauss_wts(1:n_quad_angs,n_quad_angs), &
                                  optical_props%tau,                 &
                                  sources%lay_source,                &
                                  sources%lev_source,                &
                                  sfc_emis_gpt, sources%sfc_source,  &
                                  inc_flux_diffuse,                  &
                                  gpt_flux_up, gpt_flux_dn,          &
                                  do_broadband, flux_up_loc, flux_dn_loc,     &
                                  logical(do_Jacobians, wl), sources%sfc_source_Jac, jacobian, &
                                  logical(.true., wl),  optical_props%ssa, optical_props%g)
            !$acc        end data
            !$omp end target data
          endif
        class is (ty_optical_props_nstr)
          !
          ! n-stream calculation
          !
          error_msg = 'lw_solver(...ty_optical_props_nstr...) not yet implemented'
      end select

      select type(fluxes)
        !
        ! Tidy up memory for broadband fluxes on GPUs
        !
        type is (ty_fluxes_broadband)
          if(associated(fluxes%flux_net)) then
            !
            ! FIXME: Do we need the create/copyout here?
            !
            !$acc parallel loop    collapse(2) copyin(fluxes) copyout( fluxes%flux_net)
            !$omp target teams distribute parallel do simd collapse(2) map(from:fluxes%flux_net)
            do ilev = 1, nlay+1
              do icol = 1, ncol
                fluxes%flux_net(icol,ilev) = flux_dn_loc(icol,ilev) - flux_up_loc(icol,ilev)
              end do
            end do
          end if
        class default
          !
          ! ...or reduce spectral fluxes to desired output quantities
          !
          error_msg = fluxes%reduce(gpt_flux_up, gpt_flux_dn, optical_props, top_at_1)
      end select
    end if ! no error message from validation
    !$acc        end data
    !$omp end target data

    if(.not. present(inc_flux)) then
      !$acc        exit data delete(     inc_flux_diffuse)
      !$omp target exit data map(release:inc_flux_diffuse)
      deallocate(inc_flux_diffuse)
    end if
    select type(fluxes)
      type is (ty_fluxes_broadband)
        !$acc        exit data copyout( flux_up_loc, flux_dn_loc)
        !$omp target exit data map(from:flux_up_loc, flux_dn_loc)
        if(.not. associated(flux_up_loc, fluxes%flux_up)) deallocate(flux_up_loc)
        if(.not. associated(flux_dn_loc, fluxes%flux_dn)) deallocate(flux_dn_loc)
    end select
  end function rte_lw
  !--------------------------------------------------------------------------------------------------------------------
  !
  ! Expand from band to g-point dimension, transpose dimensions (nband, ncol) -> (ncol,ngpt)
  !
  subroutine expand_and_transpose(ops,arr_in,arr_out)
    class(ty_optical_props),  intent(in ) :: ops
    real(wp), dimension(:,:), intent(in ) :: arr_in  ! (nband, ncol)
    real(wp), dimension(:,:), intent(out) :: arr_out ! (ncol, igpt)
    ! -------------
    integer :: ncol, nband, ngpt
    integer :: icol, iband, igpt
    integer, dimension(2,ops%get_nband()) :: limits

    ncol  = size(arr_in, 2)
    nband = ops%get_nband()
    ngpt  = ops%get_ngpt()
    limits = ops%get_band_lims_gpoint()
    !$acc                         parallel loop    collapse(2) copyin(arr_in, limits)
    !$omp target teams distribute parallel do simd collapse(2) map(to:arr_in, limits)
    do iband = 1, nband
      do icol = 1, ncol
        do igpt = limits(1, iband), limits(2, iband)
          arr_out(icol, igpt) = arr_in(iband,icol)
        end do
      end do
    end do

  end subroutine expand_and_transpose
  !--------------------------------------------------------------------------------------------------------------------
end module mo_rte_lw
