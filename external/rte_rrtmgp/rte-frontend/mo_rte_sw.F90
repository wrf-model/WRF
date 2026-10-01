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
!> Compute shortwave radiative fluxes

!>  Contains a single routine to compute direct and diffuse fluxes of solar radiation given
!>
!>  - atmospheric optical properties on a spectral grid
!>  - information about vertical ordering
!>  - boundary conditions
!>    - solar zenith angle, spectrally-resolved incident colimated flux, surface albedos for direct and diffuse radiation
!>    - optionally, a boundary condition for incident diffuse radiation
!>
!> It is the user's responsibility to ensure that boundary conditions (incident fluxes, surface albedos) are on the same
!>   spectral grid as the optical properties.
!>
!> Final output is via user-extensible ty_fluxes
!> ([[mo_fluxes(module):ty_fluxes(type)]] in module [[mo_fluxes]])
!> which must reduce the detailed spectral fluxes to whatever summary the user needs
!>
!>
!> The routine does error checking and choses which lower-level kernel to invoke based on
!>   what kinds of optical properties are supplied
!
! -------------------------------------------------------------------------------------------------
module mo_rte_sw
  use mo_rte_kind,      only: wp, wl
  use mo_rte_config,    only: check_extents, check_values
  use mo_rte_util_array,only: zero_array
  use mo_rte_util_array_validation, & 
                        only: any_vals_less_than, any_vals_outside, extents_are
  use mo_optical_props, only: ty_optical_props, &
                              ty_optical_props_arry, ty_optical_props_1scl, ty_optical_props_2str, ty_optical_props_nstr
  use mo_fluxes,        only: ty_fluxes, ty_fluxes_broadband
  use mo_rte_solver_kernels, &
                        only: sw_solver_noscat, sw_solver_2stream
  implicit none
  private

  interface rte_sw
    module procedure rte_sw_mu0_bycol, rte_sw_mu0_full
  end interface rte_sw
  public :: rte_sw

contains
  ! -------------------------------------------------------------------------------------------------
  function rte_sw_mu0_bycol(atmos, top_at_1, &
                  mu0, inc_flux,             &
                  sfc_alb_dir, sfc_alb_dif,  &
                  fluxes, inc_flux_dif) result(error_msg)
    class(ty_optical_props_arry), intent(in   ) :: atmos
      !! Optical properties provided as arrays
    logical,                      intent(in   ) :: top_at_1
      !! Is the top of the domain at index 1? (if not, ordering is bottom-to-top)
    real(wp), dimension(:),       intent(in   ) :: mu0
      !! cosine of solar zenith angle (ncol) - will be assumed constant with height
    real(wp), dimension(:,:),     intent(in   ) :: inc_flux
      !! incident flux at top of domain [W/m2] (ncol, ngpt)
    real(wp), dimension(:,:),     intent(in   ) :: sfc_alb_dir
      !! surface albedo for direct and
    real(wp), dimension(:,:),     intent(in   ) :: sfc_alb_dif
      !! diffuse radiation (nband, ncol)
    class(ty_fluxes),             intent(inout) :: fluxes
      !! Class describing output calculations
    real(wp), dimension(:,:), optional, target, &
                                  intent(in   ) :: inc_flux_dif
      !! incident diffuse flux at top of domain [W/m2] (ncol, ngpt)
    character(len=128)                          :: error_msg
      !! If empty, calculation was successful
    ! --------------------------------
    real(wp), dimension(size(mu0), atmos%get_nlay()) :: mu0_bylay
    integer :: i, j, ncol, nlay

    ncol = size(mu0)
    nlay = atmos%get_nlay()
    ! Solar zenith angle cosine is constant with height
    !$acc        data copyin(mu0)    create(mu0_bylay)
    !$omp target data map(to:mu0) map(alloc:mu0_bylay)

    !$acc                         parallel loop    collapse(2)
    !$omp target teams distribute parallel do simd collapse(2)
    do j = 1, nlay
      do i = 1, ncol
        mu0_bylay(i,j) = mu0(i)
      end do
    end do

    error_msg = rte_sw_mu0_full(atmos, top_at_1, &
                    mu0_bylay, inc_flux,         &
                    sfc_alb_dir, sfc_alb_dif,    &
                    fluxes, inc_flux_dif)
    !$acc end data
    !$omp end target data
  end function rte_sw_mu0_bycol
  ! -------------------------------------------------------------------------------------------------
  function rte_sw_mu0_full(atmos, top_at_1, &
                  mu0, inc_flux,            &
                  sfc_alb_dir, sfc_alb_dif, &
                  fluxes, inc_flux_dif) result(error_msg)
    class(ty_optical_props_arry), intent(in   ) :: atmos
      !! Optical properties provided as arrays
    logical,                      intent(in   ) :: top_at_1
      !! Is the top of the domain at index 1? (if not, ordering is bottom-to-top)
    real(wp), dimension(:,:),     intent(in   ) :: mu0
      !! cosine of solar zenith angle (ncol, nlay)
    real(wp), dimension(:,:),     intent(in   ) :: inc_flux
      !! incident flux at top of domain [W/m2] (ncol, ngpt)
    real(wp), dimension(:,:),     intent(in   ) :: sfc_alb_dir
      !! surface albedo for direct and
    real(wp), dimension(:,:),     intent(in   ) :: sfc_alb_dif
      !! diffuse radiation (nband, ncol)
    class(ty_fluxes),             intent(inout) :: fluxes
      !! Class describing output calculations
    real(wp), dimension(:,:), optional, target, &
                                  intent(in   ) :: inc_flux_dif
      !! incident diffuse flux at top of domain [W/m2] (ncol, ngpt)
    character(len=128)                          :: error_msg
      !! If empty, calculation was successful
    ! --------------------------------
    !
    ! Local variables
    !
    integer     :: ncol, nlay, ngpt, nband
    integer     :: icol, ilev
    logical(wl) :: has_dif_bc, do_broadband

    real(wp), dimension(:,:,:), pointer             :: gpt_flux_up, gpt_flux_dn, gpt_flux_dir
    real(wp), dimension(:,:),   allocatable         :: sfc_alb_dir_gpt, sfc_alb_dif_gpt
    real(wp), dimension(:,:),   pointer             :: flux_dn_loc, flux_up_loc, flux_dir_loc
    real(wp), dimension(:,:),   pointer             :: inc_flux_diffuse
    real(wp), dimension(:,:,:), allocatable, target :: decoy3D
    real(wp), dimension(:,:),   allocatable, target :: decoy2D

    ! ------------------------------------------------------------------------------------
    ncol  = atmos%get_ncol()
    nlay  = atmos%get_nlay()
    ngpt  = atmos%get_ngpt()
    nband = atmos%get_nband()
    error_msg = ""
    ! ------------------------------------------------------------------------------------
    !
    ! Error checking -- consistency of sizes and validity of values
    !
    ! --------------------------------
    if(.not. fluxes%are_desired()) &
      error_msg = "rte_sw: no space allocated for fluxes"

    has_dif_bc = logical(present(inc_flux_dif), wl)

    !
    ! Sizes of input arrays
    !
    ! Copy variables whose sizes and values are checked to the GPU so the checks can happen there.
    !   No harm done if checks are not performed  (?)
    !$acc        data copyin(mu0, inc_flux, sfc_alb_dir, sfc_alb_dif)
    !$omp target data map(to:mu0, inc_flux, sfc_alb_dir, sfc_alb_dif)
    !$acc        data copyin(inc_flux_dif) if (has_dif_bc)
    !$omp target data map(to:inc_flux_dif) if (has_dif_bc)
    if(check_extents) then
      if(.not. extents_are(mu0, ncol, nlay)) &
        error_msg = "rte_sw: mu0 inconsistently sized"
      if(.not. extents_are(inc_flux, ncol, ngpt)) &
        error_msg = "rte_sw: inc_flux inconsistently sized"
      if(.not. extents_are(sfc_alb_dir, nband, ncol)) &
        error_msg = "rte_sw: sfc_alb_dir inconsistently sized"
      if(.not. extents_are(sfc_alb_dif, nband, ncol)) &
        error_msg = "rte_sw: sfc_alb_dif inconsistently sized"
      if(has_dif_bc) then
        if(.not. extents_are(inc_flux_dif, ncol, ngpt)) &
          error_msg = "rte_sw: inc_flux_dif inconsistently sized"
      end if
    end if
    !
    ! Values of input arrays
    !
    if(check_values) then
      if(any_vals_outside(mu0, -1._wp, 1._wp)) &
        error_msg = "rte_sw: one or more mu0 < -1 or > 1"
      if(any_vals_less_than(inc_flux, 0._wp)) &
        error_msg = "rte_sw: one or more inc_flux < 0"
      if(any_vals_outside(sfc_alb_dir,  0._wp, 1._wp)) &
        error_msg = "rte_sw: sfc_alb_dir out of bounds [0,1]"
      if(any_vals_outside(sfc_alb_dif,  0._wp, 1._wp)) &
        error_msg = "rte_sw: sfc_alb_dif out of bounds [0,1]"
      if(has_dif_bc) then
        if(any_vals_less_than(inc_flux_dif, 0._wp)) &
          error_msg = "rte_sw: one or more inc_flux_dif < 0"
      end if
    end if

    ! ------------------------------------------------------------------------------------
    select type(fluxes)
      type is (ty_fluxes_broadband)
        do_broadband = .true._wl
        !
        ! Solvers will integrate in place (one g-point at a time on CPUs)
        !   so won't need big working arrays
        !
        allocate(decoy3D(ncol, nlay+1, ngpt))
        gpt_flux_up  => decoy3D
        gpt_flux_dn  => decoy3D
        gpt_flux_dir => decoy3D
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
        if(associated(fluxes%flux_dn_dir)) then
          flux_dir_loc => fluxes%flux_dn_dir
        else
          allocate(flux_dir_loc(ncol, nlay+1))
        end if
        !$acc        enter data create(   flux_up_loc, flux_dn_loc, flux_dir_loc)
        !$omp target enter data map(alloc:flux_up_loc, flux_dn_loc, flux_dir_loc)
      class default
        !
        ! If broadband integrals aren't being computed, allocate working space
        !   and decoy addresses for spectrally-integrated fields
        !
        do_broadband = .false._wl
        allocate(decoy2D(ncol, nlay+1))
        flux_up_loc  => decoy2D
        flux_dn_loc  => decoy2D
        flux_dir_loc => decoy2D
        allocate(gpt_flux_up (ncol,nlay+1,ngpt), &
                 gpt_flux_dn (ncol,nlay+1,ngpt), &
                 gpt_flux_dir(ncol,nlay+1,ngpt))
    end select

    allocate(sfc_alb_dir_gpt(ncol, ngpt), sfc_alb_dif_gpt(ncol, ngpt))
    if(len_trim(error_msg) > 0) then
      if(len_trim(atmos%get_name()) > 0) &
        error_msg = trim(atmos%get_name()) // ': ' // trim(error_msg)
    end if

    ! Fluxes need to be copied out only if do_broadband is .true.
    !$acc        data copyin(   flux_up_loc,flux_dn_loc,flux_dir_loc) if (      do_broadband)
    !$omp target data map(to:   flux_up_loc,flux_dn_loc,flux_dir_loc) if (      do_broadband)
    !$acc        data create(   flux_up_loc,flux_dn_loc,flux_dir_loc) if (.not. do_broadband)
    !$omp target data map(alloc:flux_up_loc,flux_dn_loc,flux_dir_loc) if (.not. do_broadband)

    !$acc        data create(   gpt_flux_up,gpt_flux_dn,gpt_flux_dir) &
    !$acc             create(   sfc_alb_dir_gpt, sfc_alb_dif_gpt)
    !$omp target data map(alloc:gpt_flux_up,gpt_flux_dn,gpt_flux_dir) &
    !$omp             map(alloc:sfc_alb_dir_gpt, sfc_alb_dif_gpt)


    ! ------------------------------------------------------------------------------------
    ! Boundary conditions
    !   Lower boundary condition -- expand surface albedos by band to gpoints
    !     and switch dimension ordering
    call expand_and_transpose(atmos, sfc_alb_dir, sfc_alb_dir_gpt)
    call expand_and_transpose(atmos, sfc_alb_dif, sfc_alb_dif_gpt)
    !
    !   Diffuse flux boundary condition - will use values in optional arg or be set to 0
    !
    if (has_dif_bc) then
      inc_flux_diffuse => inc_flux_dif
      !$acc        enter data copyin(   inc_flux_diffuse)
      !$omp target enter data map(to:   inc_flux_diffuse)
    else
      allocate(inc_flux_diffuse(ncol, ngpt))
      !$acc        enter data create(   inc_flux_diffuse)
      !$omp target enter data map(alloc:inc_flux_diffuse)
      call zero_array(ncol, ngpt, inc_flux_diffuse)
    end if
    ! ------------------------------------------------------------------------------------
    if(check_values) error_msg =  atmos%validate()
    !
    ! Compute the radiative transfer...
    !
    if(len_trim(error_msg) == 0) then
      select type (atmos)
        class is (ty_optical_props_1scl)
          !
          ! Direct beam only - for completeness, unlikely to be used in practice
          !
          call sw_solver_noscat(ncol, nlay, ngpt, logical(top_at_1, wl), &
                                atmos%tau, mu0, inc_flux,                &
                                gpt_flux_dir)
          call zero_array(ncol, nlay+1, ngpt, gpt_flux_up)
          !
          !$acc kernels
          !$omp target
          gpt_flux_dn(:,:,:) = gpt_flux_dir(:,:,:)
          !$acc end kernels
          !$omp end target

        class is (ty_optical_props_2str)
          !
          ! two-stream calculation with scattering
          !
          call sw_solver_2stream(ncol, nlay, ngpt, logical(top_at_1, wl), &
                                 atmos%tau, atmos%ssa, atmos%g, mu0,      &
                                 sfc_alb_dir_gpt, sfc_alb_dif_gpt,        &
                                             inc_flux,                    &
                                 gpt_flux_up, gpt_flux_dn, gpt_flux_dir,  &
                                 has_dif_bc, inc_flux_diffuse,            &
                                 do_broadband, flux_up_loc, flux_dn_loc, flux_dir_loc)
        class is (ty_optical_props_nstr)
          !
          ! n-stream calculation
          !
          ! not yet implemented so fail
          !
          error_msg = 'sw_solver(...ty_optical_props_nstr...) not yet implemented'
      end select
      if(len_trim(error_msg) > 0) then
        if(len_trim(atmos%get_name()) > 0) &
          error_msg = trim(atmos%get_name()) // ': ' // trim(error_msg)
      end if
      !
      ! Flux reduction (summarizing for output)
      !
      select type(fluxes)
        !
        ! Tidy up memory for broadband fluxes
        !
        type is (ty_fluxes_broadband)
          if(associated(fluxes%flux_net)) then
            !$acc                         parallel loop    collapse(2) copyin(fluxes) copyout(fluxes%flux_net)
            !$omp target teams distribute parallel do simd collapse(2)
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
          error_msg = fluxes%reduce(gpt_flux_up, gpt_flux_dn, atmos, top_at_1, gpt_flux_dir)
      end select
    end if ! In case of an error we exit here

    !$acc        end data
    !$omp end target data
    !$acc        end data
    !$omp end target data
    !$acc        end data
    !$omp end target data
    !$acc        end data
    !$omp end target data
    !$acc        end data
    !$omp end target data

    !
    ! Deallocate any memory allocated locally to pointer variables
    !
    select type(fluxes)
      type is (ty_fluxes_broadband)
        !$acc        exit data copyout( flux_up_loc, flux_dn_loc, flux_dir_loc)
        !$omp target exit data map(from:flux_up_loc, flux_dn_loc, flux_dir_loc)
        if(.not. associated(fluxes%flux_up    )) deallocate(flux_up_loc)
        if(.not. associated(fluxes%flux_dn    )) deallocate(flux_dn_loc)
        if(.not. associated(fluxes%flux_dn_dir)) deallocate(flux_dir_loc)
      class default
        deallocate(gpt_flux_up, gpt_flux_dn, gpt_flux_dir)
    end select
    if(.not. has_dif_bc) then
      !$acc        exit data delete(     inc_flux_diffuse)
      !$omp target exit data map(release:inc_flux_diffuse)
      deallocate(inc_flux_diffuse)
    end if

  end function rte_sw_mu0_full
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
end module mo_rte_sw
