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
!
! This module is for packaging output quantities from RRTMGP based on spectral flux profiles
!    This implementation provides band-by-band flux profiles
!
module mo_fluxes_byband
  use mo_rte_kind,      only: wp
  use mo_rte_config,    only: check_extents
  use mo_rte_util_array_validation, & 
                        only: extents_are
  use mo_fluxes,        only: ty_fluxes, ty_fluxes_broadband
  use mo_optical_props, only: ty_optical_props
  implicit none

  ! Output from radiation calculations
  !   Data components are pointers so results can be written directly into memory
  !   reduce() function accepts spectral flux profiles
  type, extends(ty_fluxes_broadband) :: ty_fluxes_byband
    real(wp), dimension(:,:,:), pointer :: bnd_flux_up => NULL(), & ! Band-by-band fluxes
                                           bnd_flux_dn => NULL()    ! (ncol, nlev, nband)
    real(wp), dimension(:,:,:), pointer :: bnd_flux_net => NULL()   ! Net (down - up)
    real(wp), dimension(:,:,:), pointer :: bnd_flux_dn_dir => NULL() ! Direct flux down
  contains
    procedure :: reduce => reduce_byband
    procedure :: are_desired => are_desired_byband
  end type ty_fluxes_byband

  interface net_byband
    module procedure net_byband_full, net_byband_precalc
  end interface net_byband

contains
  ! --------------------------------------------------------------------------------------
  function reduce_byband(this, gpt_flux_up, gpt_flux_dn, spectral_disc, top_at_1, gpt_flux_dn_dir) result(error_msg)
    class(ty_fluxes_byband),           intent(inout) :: this
    real(kind=wp), dimension(:,:,:),   intent(in   ) :: gpt_flux_up ! Fluxes by gpoint [W/m2](ncol, nlay+1, ngpt)
    real(kind=wp), dimension(:,:,:),   intent(in   ) :: gpt_flux_dn ! Fluxes by gpoint [W/m2](ncol, nlay+1, ngpt)
    class(ty_optical_props),           intent(in   ) :: spectral_disc  !< derived type with spectral information
    logical,                           intent(in   ) :: top_at_1
    real(kind=wp), dimension(:,:,:), optional, &
                                       intent(in   ) :: gpt_flux_dn_dir! Direct flux down
    character(len=128)                               :: error_msg
    ! ------
    integer :: ncol, nlev, ngpt, nbnd
    integer, dimension(2, spectral_disc%get_nband()) :: band_lims
    ! ------
    ncol = size(gpt_flux_up, DIM=1)
    nlev = size(gpt_flux_up, DIM=2)
    ngpt = spectral_disc%get_ngpt()
    nbnd = spectral_disc%get_nband()
    band_lims(:,:) = spectral_disc%get_band_lims_gpoint()

    ! Compute broadband fluxes
    !   This also checks that input arrays are consistently sized
    !
    error_msg = this%ty_fluxes_broadband%reduce(gpt_flux_up, gpt_flux_dn, spectral_disc, top_at_1, gpt_flux_dn_dir)
    if(error_msg /= '') return

    if(size(gpt_flux_up, 3) /= ngpt) then
      error_msg = "reduce: spectral discretization and g-point flux arrays have differing number of g-points"
      return
    end if

    ! Check sizes of output arrays
    if(check_extents) then
      if(associated(this%bnd_flux_up)) then
        if(.not. extents_are(this%bnd_flux_up, ncol, nlev, nbnd)) &
          error_msg = "reduce: bnd_flux_up array incorrectly sized (can't compute net flux either)"
      end if
      if(associated(this%bnd_flux_dn)) then
        if(.not. extents_are(this%bnd_flux_dn, ncol, nlev, nbnd)) &
          error_msg = "reduce: bnd_flux_dn array incorrectly sized (can't compute net flux either)"
      end if
      if(associated(this%bnd_flux_dn_dir)) then
        if(.not. extents_are(this%bnd_flux_dn_dir, ncol, nlev, nbnd)) &
          error_msg = "reduce: bnd_flux_dn_dir array incorrectly sized"
      end if
      if(associated(this%bnd_flux_net)) then
        if(.not. extents_are(this%bnd_flux_net, ncol, nlev, nbnd)) &
          error_msg = "reduce: bnd_flux_net array incorrectly sized (can't compute net flux either)"
      end if
      if(error_msg /= "") return
    end if
    !
    ! Self-consistency -- shouldn't be asking for direct beam flux if it isn't supplied
    if(associated(this%bnd_flux_dn_dir) .and. .not. present(gpt_flux_dn_dir)) then
      error_msg = "reduce: requesting bnd_flux_dn_dir but direct flux hasn't been supplied"
      return
    end if

    ! -------
    !$acc enter data copyin(band_lims)
    !$omp target enter data map(to:band_lims)
    ! Band-by-band fluxes
    ! Up flux
    if(associated(this%bnd_flux_up)) then
      call sum_byband(ncol, nlev, ngpt, nbnd, band_lims, gpt_flux_up,     this%bnd_flux_up    )
    end if

    ! -------
    ! Down flux
    if(associated(this%bnd_flux_dn)) then
      call sum_byband(ncol, nlev, ngpt, nbnd, band_lims, gpt_flux_dn,     this%bnd_flux_dn    )
    end if

    if(associated(this%bnd_flux_dn_dir)) then
      call sum_byband(ncol, nlev, ngpt, nbnd, band_lims, gpt_flux_dn_dir, this%bnd_flux_dn_dir)
    end if

    ! -------
    ! Net flux
    !
    if(associated(this%bnd_flux_net)) then
      !
      !  Reuse down and up results if possible
      !
      if(associated(this%bnd_flux_dn) .and. associated(this%bnd_flux_up)) then
        call net_byband(ncol, nlev,       nbnd,                             this%bnd_flux_dn, this%bnd_flux_up, this%bnd_flux_net)
      else
        call net_byband(ncol, nlev, ngpt, nbnd, band_lims, gpt_flux_dn, gpt_flux_up, this%bnd_flux_net)
      end if
    end if
    !$acc exit data delete(band_lims)
    !$omp target exit data map(release:band_lims)
  end function reduce_byband
  ! --------------------------------------------------------------------------------------
  ! Are any fluxes desired from this set of g-point fluxes? We can tell because memory will
  !   be allocated for output
  !
  function are_desired_byband(this)
    class(ty_fluxes_byband), intent(in   ) :: this
    logical                                :: are_desired_byband

    are_desired_byband = any([associated(this%bnd_flux_up),     &
                              associated(this%bnd_flux_dn),     &
                              associated(this%bnd_flux_dn_dir), &
                              associated(this%bnd_flux_net),    &
                              this%ty_fluxes_broadband%are_desired()])
  end function are_desired_byband

  ! ----------------------------------------------------------------------------
  ! Kernels (private to this module) 
  ! ----------------------------------------------------------------------------
  !
  ! Spectral reduction over all points
  !
  subroutine sum_byband(ncol, nlev, ngpt, nbnd, band_lims, spectral_flux, byband_flux) bind(C, name="rte_sum_byband")
    integer,                               intent(in ) :: ncol, nlev, ngpt, nbnd
    integer,  dimension(2,          nbnd), intent(in ) :: band_lims
    real(wp), dimension(ncol, nlev, ngpt), intent(in ) :: spectral_flux
    real(wp), dimension(ncol, nlev, nbnd), intent(out) :: byband_flux

    integer :: icol, ilev, igpt, ibnd
    !$acc parallel loop collapse(3) copyin(spectral_flux, band_lims) copyout(byband_flux)
    !$omp target teams distribute parallel do collapse(3) map(to:spectral_flux, band_lims) map(from:byband_flux)
    do ibnd = 1, nbnd
      do ilev = 1, nlev
        do icol = 1, ncol
          byband_flux(icol, ilev, ibnd) =  spectral_flux(icol, ilev, band_lims(1, ibnd))
          do igpt = band_lims(1,ibnd)+1, band_lims(2,ibnd)
            byband_flux(icol, ilev, ibnd) = byband_flux(icol, ilev, ibnd) + &
                                            spectral_flux(icol, ilev, igpt)
          end do
        end do
      end do
    enddo
  end subroutine sum_byband
  ! ----------------------------------------------------------------------------
  !
  ! Net flux: Spectral reduction over all points
  !
  subroutine net_byband_full(ncol, nlev, ngpt, nbnd, band_lims, spectral_flux_dn, spectral_flux_up, byband_flux_net) bind(C, name="rte_net_byband_full")
    integer,                               intent(in ) :: ncol, nlev, ngpt, nbnd
    integer,  dimension(2,          nbnd), intent(in ) :: band_lims
    real(wp), dimension(ncol, nlev, ngpt), intent(in ) :: spectral_flux_dn, spectral_flux_up
    real(wp), dimension(ncol, nlev, nbnd), intent(out) :: byband_flux_net

    integer :: icol, ilev, igpt, ibnd

    !$acc parallel loop collapse(3) copyin(spectral_flux_dn, spectral_flux_up, band_lims) copyout(byband_flux_net)
    !$omp target teams distribute parallel do collapse(3) map(to:spectral_flux_dn, spectral_flux_up, band_lims) map(from:byband_flux_net)
    do ibnd = 1, nbnd
      do ilev = 1, nlev
        do icol = 1, ncol
          igpt = band_lims(1,ibnd)
          byband_flux_net(icol, ilev, ibnd) = spectral_flux_dn(icol, ilev, igpt) - &
                                              spectral_flux_up(icol, ilev, igpt)
          do igpt = band_lims(1,ibnd)+1, band_lims(2,ibnd)
            byband_flux_net(icol, ilev, ibnd) = byband_flux_net(icol, ilev, ibnd) + &
                                                spectral_flux_dn(icol, ilev, igpt) - &
                                                spectral_flux_up(icol, ilev, igpt)
          end do
        end do
      end do
    end do
  end subroutine net_byband_full
  ! ----------------------------------------------------------------------------
  subroutine net_byband_precalc(ncol, nlev, nbnd, byband_flux_dn, byband_flux_up, byband_flux_net) bind (C)
    integer,                               intent(in ) :: ncol, nlev, nbnd
    real(wp), dimension(ncol, nlev, nbnd), intent(in ) :: byband_flux_dn, byband_flux_up
    real(wp), dimension(ncol, nlev, nbnd), intent(out) :: byband_flux_net

    byband_flux_net(1:ncol,1:nlev,1:nbnd) = byband_flux_dn(1:ncol,1:nlev,1:nbnd) - byband_flux_up(1:ncol,1:nlev,1:nbnd)
  end subroutine net_byband_precalc
  ! ----------------------------------------------------------------------------

end module mo_fluxes_byband
