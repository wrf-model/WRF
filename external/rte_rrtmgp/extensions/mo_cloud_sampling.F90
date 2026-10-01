! This code is part of RRTM for GCM Applications - Parallel (RRTMGP)
!
! Contacts: Robert Pincus and Eli Mlawer
! email:  rrtmgp@aer.com
!
! Copyright 2015-2019,  Atmospheric and Environmental Research and
! Regents of the University of Colorado.  All right reserved.
!
! Use and duplication is permitted under the terms of the
!    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause
! -------------------------------------------------------------------------------------------------
!
! This module provides a simple implementation of sampling for the
!   Monte Carlo Independent Pixel Approximation (McICA, doi:10.1029/2002jd003322)
! Cloud optical properties, defined by band and assumed homogenous within each cell (column/layer),
!   are randomly sampled to preserve the mean cloud fraction and one of several possible overlap assumptions
! Users supply random numbers with order ngpt,nlay,ncol
!   These are only accessed if cloud_fraction(icol,ilay) > 0 so many values don't need to be filled in
!
! -------------------------------------------------------------------------------------------------
module mo_cloud_sampling
  use mo_rte_kind,      only: wp, wl
  use mo_optical_props, only: ty_optical_props_arry, &
                              ty_optical_props_1scl, &
                              ty_optical_props_2str, &
                              ty_optical_props_nstr
  implicit none
  private
  public :: draw_samples, sampled_mask_max_ran, sampled_mask_exp_ran
contains
  ! -------------------------------------------------------------------------------------------------
  !
  ! Apply a T/F sampled cloud mask to cloud optical properties defined by band to produce
  !   McICA-sampled cloud optical properties
  !
  function draw_samples(cloud_mask,clouds,clouds_sampled) result(error_msg)
    logical, dimension(:,:,:),      intent(in   ) :: cloud_mask     ! Dimensions ncol,nlay,ngpt
    class(ty_optical_props_arry),   intent(in   ) :: clouds         ! Defined by band
    class(ty_optical_props_arry),   intent(inout) :: clouds_sampled ! Defined by g-point
    character(len=128)                            :: error_msg
    ! ------------------------
    integer :: ncol,nlay,nbnd,ngpt
    integer :: imom
    ! ------------------------
    !
    ! Error checking
    !
    error_msg = ""
    if(.not. clouds%is_initialized()) then
      error_msg = "draw_samples: cloud optical properties are not initialized"
      return
    end if
    if(.not. clouds_sampled%is_initialized()) then
      error_msg = "draw_samples: sampled cloud optical properties are not initialized"
      return
    end if

    !
    ! Variables clouds and clouds_sampled have to be of the same type (have the same set of fields)
    !   nstr isn't supported
    !   2str is checked at assignment
    !
    select type(clouds)
    type is (ty_optical_props_1scl)
      select type(clouds_sampled)
      type is (ty_optical_props_2str)
        error_msg = "draw_samples: by-band and sampled cloud properties need to be the same variable type"
        return
      type is (ty_optical_props_nstr)
        error_msg = "draw_samples: by-band and sampled cloud properties need to be the same variable type"
        return
      end select
    type is (ty_optical_props_nstr)
      error_msg = "draw_samples: sampling isn't implemented yet for ty_optical_props_nstr"
      return
    end select

    !
    ! Spectral discretization
    !
    if(.not. clouds%bands_are_equal(clouds_sampled)) then
      error_msg = "draw_samples: by-band and sampled cloud properties spectral structure is different"
      return
    end if

    !
    ! Array extents
    !
    ncol = clouds%get_ncol()
    nlay = clouds%get_nlay()
    nbnd = clouds%get_nband()
    ngpt = clouds_sampled%get_ngpt()
    if (any([size(cloud_mask,1), size(cloud_mask,2), size(cloud_mask,3)] /= [ncol,nlay,ngpt])) then
      error_msg = "draw_samples: cloud mask and cloud optical properties have different ncol, nlay and/or ngpt"
      return
    end if
    if (any([clouds_sampled%get_ncol(), clouds_sampled%get_nlay()] /= [ncol,nlay])) then
      error_msg = "draw_samples: sampled/unsampled cloud optical properties have different ncol and/or nlay"
      return
    end if
    ! ------------------------
    !
    ! Finally - sample fields according to the cloud mask
    !
    ! Optical depth assignment works for 1scl, 2str (also nstr)
    call apply_cloud_mask(ncol,nlay,nbnd,ngpt,clouds_sampled%get_band_lims_gpoint(),cloud_mask,clouds%tau,clouds_sampled%tau)
    !
    ! For 2-stream
    !
    select type(clouds)
    type is (ty_optical_props_2str)
      select type(clouds_sampled)
      type is (ty_optical_props_2str)
        call apply_cloud_mask(ncol,nlay,nbnd,ngpt,clouds_sampled%get_band_lims_gpoint(),cloud_mask,clouds%ssa,clouds_sampled%ssa)
        call apply_cloud_mask(ncol,nlay,nbnd,ngpt,clouds_sampled%get_band_lims_gpoint(),cloud_mask,clouds%g,  clouds_sampled%g  )
      class default
          error_msg = "draw_samples: by-band and sampled cloud properties need to be the same variable type"
      end select
    end select
  end function draw_samples
  ! -------------------------------------------------------------------------------------------------
  !
  ! Generate a McICA-sampled cloud mask for maximum-random overlap
  !
  function sampled_mask_max_ran(randoms,cloud_frac,cloud_mask) result(error_msg)
    real(wp), dimension(:,:,:),    intent(in ) :: randoms    ! ngpt,nlay,ncol
    real(wp), dimension(:,:),      intent(in ) :: cloud_frac ! ncol,nlay
    logical,  dimension(:,:,:),    intent(out) :: cloud_mask ! ncol,nlay,ngpt
    character(len=128)                         :: error_msg
  ! ------------------------
    integer                              :: ncol, nlay, ngpt, icol, ilay, igpt
    integer                              :: cloud_lay_fst, cloud_lay_lst
    real(wp), dimension(size(randoms,1)) :: local_rands
    logical,  dimension(size(randoms,2)) :: cloud_mask_layer
    ! ------------------------
    !
    ! Error checking
    !
    error_msg = ""
    ncol = size(randoms, 3)
    nlay = size(randoms, 2)
    ngpt = size(randoms, 1)
    if(any([ncol,nlay] /= [size(cloud_frac, 1),size(cloud_frac, 2)]))  then
      error_msg = "sampled_mask_max_ran: sizes of randoms(ngpt,nlay,ncol) and cloud_frac(ncol,nlay) are inconsistent"
      return
    end if
    if(any([ncol,nlay,ngpt] /= [size(cloud_mask, 1),size(cloud_mask, 2), size(cloud_mask,3)]))  then
      error_msg = "sampled_mask_max_ran: sizes of randoms(ngpt,nlay,ncol) and cloud_mask(ncol,nlay,ngpt) are inconsistent"
      return
    end if
    if(any(cloud_frac > 1._wp) .or. any(cloud_frac < 0._wp)) then
      error_msg = "sampled_mask_max_ran: cloud fraction values out of range [0,1]"
      return
    end if
    !
    ! We chould check the random numbers but that would be computationally heavy
    !
    ! ------------------------
    !
    ! Construct the cloud mask for each column
    !
    do icol = 1, ncol
      cloud_mask_layer(1:nlay) = cloud_frac(icol,1:nlay) > 0._wp
      if(.not. any(cloud_mask_layer)) then
        cloud_mask(icol,1:nlay,1:ngpt) = .false.
        cycle
      end if
      cloud_lay_fst = findloc(cloud_mask_layer, .true., dim=1)
      cloud_lay_lst = findloc(cloud_mask_layer, .true., dim=1, back = .true.)
      cloud_mask(icol,1:cloud_lay_fst-1,1:ngpt) = .false.

      ilay = cloud_lay_fst
      local_rands(1:ngpt) = randoms(1:ngpt,ilay,icol)
      cloud_mask(icol,ilay,1:ngpt) = local_rands(1:ngpt) > (1._wp - cloud_frac(icol,ilay))
      do ilay = cloud_lay_fst+1, cloud_lay_lst
        if(cloud_mask_layer(ilay)) then
          !
          ! Max-random overlap:
          !   new  random deviates if the adjacent layer isn't cloudy
          !   same random deviates if the adjacent layer is    cloudy
          !
          if(.not. cloud_mask_layer(ilay-1)) local_rands(1:ngpt) = randoms(1:ngpt,ilay,icol)
          cloud_mask(icol,ilay,1:ngpt) = local_rands(1:ngpt) > (1._wp - cloud_frac(icol,ilay))
        else
          cloud_mask(icol,ilay,1:ngpt) = .false.
        end if
      end do

      cloud_mask(icol,cloud_lay_lst+1:nlay,1:ngpt) = .false.
    end do

  end function sampled_mask_max_ran
  ! -------------------------------------------------------------------------------------------------
  !
  ! Generate a McICA-sampled cloud mask for exponential-random overlap.
  !   The overlap parameter overlap_param is defined between pairs of layers.
  !   For layer i, overlap_param(i) describes the overlap between cloud_frac(i) and cloud_frac(i+1).
  !   It is a correlation coefficient in [-1,1]. E.g., 
  !     +1 gives perfect correlation or maximum cloud overlap between layers i & i+1;
  !      0 gives no correlation or random cloud overlap between layers i & i+1;
  !     -1 gives perfect anticorrelation or minimum cloud overlap between layers i & i+1.
  !   By skipping layers with zero cloud fraction the code effectively forces overlap_param(i) = 0
  !     for cloud_frac(i) = 0, leading to random cloud overlap across clear layers.
  !
  function sampled_mask_exp_ran(randoms,cloud_frac,overlap_param,cloud_mask) result(error_msg)
    real(wp), dimension(:,:,:), intent(in ) :: randoms       ! ngpt,nlay,ncol
    real(wp), dimension(:,:),   intent(in ) :: cloud_frac    ! ncol,nlay
    real(wp), dimension(:,:),   intent(in ) :: overlap_param ! ncol,nlay-1
    logical,  dimension(:,:,:), intent(out) :: cloud_mask    ! ncol,nlay,ngpt
    character(len=128)                      :: error_msg
    ! ------------------------
    integer                              :: ncol, nlay, ngpt, icol, ilay, igpt
    integer                              :: cloud_lay_fst, cloud_lay_lst
    real(wp)                             :: rho ! correlation coefficient
    real(wp), dimension(size(randoms,1)) :: local_rands
    logical,  dimension(size(randoms,2)) :: cloud_mask_layer
    ! ------------------------
    !
    ! Error checking
    !
    error_msg = ""
    ncol = size(randoms, 3)
    nlay = size(randoms, 2)
    ngpt = size(randoms, 1)
    if(any([ncol,nlay] /= [size(cloud_frac, 1),size(cloud_frac, 2)]))  then
      error_msg = "sampled_mask_exp_ran: sizes of randoms(ngpt,nlay,ncol) and cloud_frac(ncol,nlay) are inconsistent"
      return
    end if
    if(any([ncol,nlay-1] /= [size(overlap_param, 1),size(overlap_param, 2)]))  then
      error_msg = "sampled_mask_exp_ran: sizes of randoms(ngpt,nlay,ncol) and overlap_param(ncol,nlay-1) are inconsistent"
      return
    end if
    if(any([ncol,nlay,ngpt] /= [size(cloud_mask, 1),size(cloud_mask, 2), size(cloud_mask,3)]))  then
      error_msg = "sampled_mask_exp_ran: sizes of randoms(ngpt,nlay,ncol) and cloud_mask(ncol,nlay,ngpt) are inconsistent"
      return
    end if

    if(any(cloud_frac > 1._wp) .or. any(cloud_frac < 0._wp)) then
      error_msg = "sampled_mask_exp_ran: cloud fraction values out of range [0,1]"
      return
    end if
    if(any(overlap_param > 1._wp) .or. any(overlap_param < -1._wp)) then
      error_msg = "sampled_mask_exp_ran: overlap_param values out of range [-1,1]"
      return
    end if
    !
    ! We chould check the random numbers but that would be computationally heavy
    !
    ! ------------------------
    ! Construct the cloud mask for each column
    !
    do icol = 1, ncol
      cloud_mask_layer(1:nlay) = cloud_frac(icol,1:nlay) > 0._wp
      if(.not. any(cloud_mask_layer)) then
        cloud_mask(icol,1:nlay,1:ngpt) = .false.
        cycle
      end if
      cloud_lay_fst = findloc(cloud_mask_layer, .true., dim=1)
      cloud_lay_lst = findloc(cloud_mask_layer, .true., dim=1, back = .true.)
      cloud_mask(icol,1:cloud_lay_fst-1,1:ngpt) = .false.

      ilay = cloud_lay_fst
      local_rands(1:ngpt) = randoms(1:ngpt,ilay,icol)
      cloud_mask(icol,ilay,1:ngpt) = local_rands(1:ngpt) > (1._wp - cloud_frac(icol,ilay))
      do ilay = cloud_lay_fst+1, cloud_lay_lst
        if(cloud_mask_layer(ilay)) then
          !
          ! Exponential-random overlap:
          !   new  random deviates if the adjacent layer isn't cloudy
          !   correlated  deviates if the adjacent layer is    cloudy
          !
          if(cloud_mask_layer(ilay-1)) then
            !
            ! Create random deviates correlated between this layer and the previous layer
            !    (have to remove mean value before enforcing correlation)
            !
            rho = overlap_param(icol,ilay-1)
            local_rands(1:ngpt) =  rho*(local_rands(1:ngpt)      -0.5_wp) + &
                   sqrt(1._wp-rho*rho)*(randoms(1:ngpt,ilay,icol)-0.5_wp) + 0.5_wp
          else
            local_rands(1:ngpt) = randoms(1:ngpt,ilay,icol)
          end if
          cloud_mask(icol,ilay,1:ngpt) = local_rands(1:ngpt) > (1._wp - cloud_frac(icol,ilay))
        else
          cloud_mask(icol,ilay,1:ngpt) = .false.
        end if
      end do

      cloud_mask(icol,cloud_lay_lst+1:nlay,1:ngpt) = .false.
    end do

  end function sampled_mask_exp_ran
  ! -------------------------------------------------------------------------------------------------
  !
  ! Apply a true/false cloud mask to a homogeneous field
  !   This could be a kernel
  !
  subroutine apply_cloud_mask(ncol,nlay,nbnd,ngpt,band_lims_gpt,cloud_mask,input_field,sampled_field)
    integer,                                intent(in ) :: ncol,nlay,nbnd,ngpt
    integer,     dimension(2,nbnd),         intent(in ) :: band_lims_gpt
    logical,     dimension(ncol,nlay,ngpt), intent(in ) :: cloud_mask
    real(wp),    dimension(ncol,nlay,nbnd), intent(in ) :: input_field
    real(wp),    dimension(ncol,nlay,ngpt), intent(out) :: sampled_field

    integer :: icol,ilay,ibnd,igpt

    do ibnd = 1, nbnd
      do igpt = band_lims_gpt(1,ibnd), band_lims_gpt(2,ibnd)
        do ilay = 1, nlay
          sampled_field(1:ncol,ilay,igpt) = merge(input_field(1:ncol,ilay,ibnd), 0._wp, cloud_mask(1:ncol,ilay,igpt))
        end do
      end do
    end do
  end subroutine apply_cloud_mask
end module mo_cloud_sampling
