! This code is part of Radiative Transfer for Energetics (RTE)
!
! Contacts: Robert Pincus and Eli Mlawer
! email:  rrtmgp@aer.com
!
! Copyright 2015-  Atmospheric and Environmental Research and
! Regents of the University of Colorado.  All right reserved.
!
! Use and duplication is permitted under the terms of the
!    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause
! -------------------------------------------------------------------------------------------------
!
!> ## Compute output quantities from spectrally-resolved flux profiles
!>
!>    This module contains an abstract class and a broadband implmentation that sums over all spectral points
!>    The abstract base class defines the routines that extenstions must implement: `reduce()` and `are_desired()`
!>    The intent is for users to extend it as required, using mo_flxues_broadband as an example
!
! -------------------------------------------------------------------------------------------------
module mo_fluxes
  use mo_rte_kind,       only: wp
  use mo_rte_config,     only: check_extents
  use mo_rte_util_array_validation, only: extents_are
  use mo_optical_props,  only: ty_optical_props
  use mo_fluxes_broadband_kernels, &
                         only: sum_broadband, net_broadband
  implicit none
  private
  ! -----------------------------------------------------------------------------------------------
  !
  !> Abstract base class:
  !>   `reduce()` function accepts spectral flux profiles, computes desired outputs
  !>   `are_desired()` returns a logical
  !
  ! -----------------------------------------------------------------------------------------------
  type, abstract, public :: ty_fluxes
  contains
    procedure(reduce_abstract),      deferred, public :: reduce
    procedure(are_desired_abstract), deferred, public :: are_desired
  end type ty_fluxes
  ! -----------------------------------------------------------------------------------------------
  !
  !> Class implementing broadband integration for the complete flux profile.
  !>   Data components are pointers so results can be written directly into memory
  !
  ! -----------------------------------------------------------------------------------------------
  type, extends(ty_fluxes), public :: ty_fluxes_broadband
    real(wp), dimension(:,:), pointer :: flux_up => NULL(), flux_dn => NULL()
    real(wp), dimension(:,:), pointer :: flux_net => NULL()    ! Net (down - up)
    real(wp), dimension(:,:), pointer :: flux_dn_dir => NULL() ! Direct flux down
  contains
    procedure, public :: reduce      => reduce_broadband
    procedure, public :: are_desired => are_desired_broadband
  end type ty_fluxes_broadband
  ! -----------------------------------------------------------------------------------------------

  ! -----------------------------------------------------------------------------------------------
  !
  ! Abstract interfaces: any implemntation has to provide routines with these interfaces
  !
  abstract interface
    ! -------------------
    !
    !> This routine takes the fully resolved calculation (detailed in spectral and vertical dimensions) and
    !>   computes desired outputs. Output values will normally be data components of the derived type.
    !
    function reduce_abstract(this, gpt_flux_up, gpt_flux_dn, spectral_disc, top_at_1, gpt_flux_dn_dir) result(error_msg)
      import ty_fluxes, ty_optical_props
      import wp
      class(ty_fluxes),                  intent(inout) :: this
      real(kind=wp), dimension(:,:,:),   intent(in   ) :: gpt_flux_up ! Fluxes by gpoint [W/m2](ncol, nlay+1, ngpt)
      real(kind=wp), dimension(:,:,:),   intent(in   ) :: gpt_flux_dn ! Fluxes by gpoint [W/m2](ncol, nlay+1, ngpt)
      class(ty_optical_props),           intent(in   ) :: spectral_disc  !< derived type with spectral information
      logical,                           intent(in   ) :: top_at_1
      real(kind=wp), dimension(:,:,:), optional, &
                                         intent(in   ) :: gpt_flux_dn_dir! Direct flux down
      character(len=128)                               :: error_msg
    end function reduce_abstract
    ! -------------------
    !
    ! This routine determines if the reduction should proceed - it's useful in ensuring
    !   that space has been allocated for the results, for example.
    !
    function are_desired_abstract(this)
      import ty_fluxes
      class(ty_fluxes), intent(in   ) :: this
      logical                         :: are_desired_abstract
    end function are_desired_abstract
    ! ----------------------
  end interface
contains
  ! --------------------------------------------------------------------------------------
  !
  !> Broadband fluxes -- simply sum over the spectral dimension and report the whole profile
  !
  ! --------------------------------------------------------------------------------------
  function reduce_broadband(this, gpt_flux_up, gpt_flux_dn, spectral_disc, top_at_1, gpt_flux_dn_dir) result(error_msg)
    class(ty_fluxes_broadband),        intent(inout) :: this
    real(kind=wp), dimension(:,:,:),   intent(in   ) :: gpt_flux_up ! Fluxes by gpoint [W/m2](ncol, nlay+1, ngpt)
    real(kind=wp), dimension(:,:,:),   intent(in   ) :: gpt_flux_dn ! Fluxes by gpoint [W/m2](ncol, nlay+1, ngpt)
    class(ty_optical_props),           intent(in   ) :: spectral_disc  !< derived type with spectral information
    logical,                           intent(in   ) :: top_at_1
    real(kind=wp), dimension(:,:,:), optional, &
                                       intent(in   ) :: gpt_flux_dn_dir! Direct flux down
    character(len=128)                               :: error_msg
    ! ------
    integer :: ncol, nlev, ngpt

    ! ------
    ncol = size(gpt_flux_up, DIM=1)
    nlev = size(gpt_flux_up, DIM=2)
    ngpt = size(gpt_flux_up, DIM=3)
    error_msg = ""

    if(check_extents) then
      !
      ! Check array sizes
      !  Input arrays
      !
      if(.not. extents_are(gpt_flux_dn, ncol, nlev, ngpt)) &
        error_msg = "reduce: gpt_flux_dn array incorrectly sized"

      if(present(gpt_flux_dn_dir)) then
        if(.not. extents_are(gpt_flux_dn_dir, ncol, nlev, ngpt)) &
          error_msg = "reduce: gpt_flux_dn_dir array incorrectly sized"
      end if
      !
      ! Output arrays
      !
      if(associated(this%flux_up)) then
        if(.not. extents_are(this%flux_up, ncol, nlev)) &
          error_msg = 'reduce: flux_up array incorrectly sized'
      end if
      if(associated(this%flux_dn)) then
        if(.not. extents_are(this%flux_dn, ncol, nlev)) &
          error_msg = 'reduce: flux_dn array incorrectly sized'
      end if
      if(associated(this%flux_net)) then
        if(.not. extents_are(this%flux_net, ncol, nlev)) &
          error_msg = 'reduce: flux_net array incorrectly sized'
      end if
      if(associated(this%flux_dn_dir)) then
        if(.not. extents_are(this%flux_dn_dir, ncol, nlev)) &
          error_msg = 'reduce: flux_dn_dir array incorrectly sized'
      end if

      if(error_msg /= "") return
    end if
    !
    ! Self-consistency -- shouldn't be asking for direct beam flux if it isn't supplied
    !
    if(associated(this%flux_dn_dir) .and. .not. present(gpt_flux_dn_dir)) then
      error_msg = "reduce: requesting direct downward flux but this hasn't been supplied"
      return
    end if

    !
    ! Broadband fluxes - call the kernels
    !
    if(associated(this%flux_up    )) &
      call sum_broadband(ncol, nlev, ngpt, gpt_flux_up,     this%flux_up)
    if(associated(this%flux_dn    )) &
      call sum_broadband(ncol, nlev, ngpt, gpt_flux_dn,     this%flux_dn)
    if(associated(this%flux_dn_dir)) &
      call sum_broadband(ncol, nlev, ngpt, gpt_flux_dn_dir, this%flux_dn_dir)

    if(associated(this%flux_net)) then
      !
      !  Reuse down and up results if possible
      !
      if(associated(this%flux_dn) .and. associated(this%flux_up)) then
        call net_broadband(ncol, nlev,      this%flux_dn, this%flux_up, this%flux_net)
      else
        call net_broadband(ncol, nlev, ngpt, gpt_flux_dn,  gpt_flux_up, this%flux_net)
      end if
    end if
  end function reduce_broadband
  ! --------------------------------------------------------------------------------------
  !
  !> Are any fluxes desired from this set of g-point fluxes? We can tell because memory will
  !>   be allocated for output
  !
  ! --------------------------------------------------------------------------------------
  function are_desired_broadband(this)
    class(ty_fluxes_broadband), intent(in   ) :: this
    logical                                   :: are_desired_broadband

    are_desired_broadband = any( [associated(this%flux_up),     &
                                  associated(this%flux_dn),     &
                                  associated(this%flux_dn_dir), &
                                  associated(this%flux_net)] )
  end function are_desired_broadband
  ! --------------------------------------------------------------------------------------
end module mo_fluxes
