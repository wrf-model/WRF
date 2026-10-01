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
!> ## Kernels for arrays of optical properties:
!>     - delta-scaling
!>     - adding two sets of properties
!>     - extracting subsets along the column dimension
!
! -------------------------------------------------------------------------------------------------

module mo_optical_props_kernels
  use, intrinsic :: iso_c_binding
  use mo_rte_kind, only: wp, wl
  implicit none

  public

  !> Delta-scale two-stream optical properties
  interface delta_scale_2str_kernel
    module procedure delta_scale_2str_f_k, delta_scale_2str_k
  end interface

  !> Subsetting, meaning extracting some portion of the 3D domain
  interface extract_subset
    module procedure extract_subset_dim1_3d, extract_subset_dim2_4d
    module procedure extract_subset_absorption_tau
  end interface extract_subset

  real(wp), parameter, private :: eps = 3.0_wp*tiny(1.0_wp)
contains
  ! -------------------------------------------------------------------------------------------------
  !
  ! Delta-scaling is provided only for two-stream properties at present
  !
  ! -------------------------------------------------------------------------------------------------
  !> Delta-scale two-stream optical properties given user-provided value of \(f\) (forward scattering)
  !
  pure subroutine delta_scale_2str_f_k(ncol, nlay, ngpt, tau, ssa, g, f) &
      bind(C, name="rte_delta_scale_2str_f_k")
    integer,                               intent(in   ) :: ncol, nlay, ngpt
      !! Array sizes
    real(wp), dimension(ncol, nlay, ngpt), intent(inout) ::  tau, ssa, g
      !! Optical depth, single-scattering albedo, asymmetry parameter
    real(wp), dimension(ncol, nlay, ngpt), intent(in   ) ::  f
      !! User-provided forward-scattering fraction

    real(wp) :: wf
    integer  :: icol, ilay, igpt

    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          wf = ssa(icol,ilay,igpt) * f(icol,ilay,igpt)
          tau(icol,ilay,igpt) = (1._wp - wf) * tau(icol,ilay,igpt)
          ssa(icol,ilay,igpt) = (ssa(icol,ilay,igpt) - wf) /  max(eps,(1.0_wp - wf))
          g  (icol,ilay,igpt) = (g  (icol,ilay,igpt) - f(icol,ilay,igpt)) / &
                                        max(eps,(1._wp - f(icol,ilay,igpt)))
        end do
      end do
    end do

  end subroutine delta_scale_2str_f_k
  ! ---------------------------------
  !> Delta-scale assuming forward-scatternig fraction is the square of the asymmetry parameter
  !>    i.e. \(f = g^2\)
  !
  pure subroutine delta_scale_2str_k(ncol, nlay, ngpt, tau, ssa, g) &
      bind(C, name="rte_delta_scale_2str_k")
    integer,                               intent(in   ) :: ncol, nlay, ngpt
      !! Array sizes
    real(wp), dimension(ncol, nlay, ngpt), intent(inout) ::  tau, ssa, g
      !! Optical depth, single-scattering albedo, asymmetry parameter

    real(wp) :: f, wf
    integer  :: icol, ilay, igpt

    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          f  = g  (icol,ilay,igpt) * g  (icol,ilay,igpt)
          wf = ssa(icol,ilay,igpt) * f
          tau(icol,ilay,igpt) = (1._wp - wf) * tau(icol,ilay,igpt)
          ssa(icol,ilay,igpt) = (ssa(icol,ilay,igpt) - wf) /  max(eps,(1.0_wp - wf))
          g  (icol,ilay,igpt) = (g  (icol,ilay,igpt) -  f) /  max(eps,(1.0_wp -  f))
        end do
      end do
    end do

  end subroutine delta_scale_2str_k
  ! -------------------------------------------------------------------------------------------------
  !
  ! Addition of optical properties: the first set are incremented by the second set.
  !
  !   There are three possible representations of optical properties (scalar = optical depth only;
  !   two-stream = tau, single-scattering albedo, and asymmetry factor g, and
  !   n-stream = tau, ssa, and phase function moments p.) Thus we need nine routines, three for
  !   each choice of representation on the left hand side times three representations of the
  !   optical properties to be added.
  !
  !   There are two sets of these nine routines. In the first the two sets of optical
  !   properties are defined at the same spectral resolution. There is also a set of routines
  !   to add properties defined at lower spectral resolution to a set defined at higher spectral
  !   resolution (adding properties defined by band to those defined by g-point)
  !
  ! -------------------------------------------------------------------------------------------------
  !> increase one absorption optical depth by a second value
  pure subroutine increment_1scalar_by_1scalar(ncol, nlay, ngpt, &
                                               tau1,             &
                                               tau2) bind(C, name="rte_increment_1scalar_by_1scalar")
    integer,                             intent(in   ) :: ncol, nlay, ngpt !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1             !! optical properties to be modified
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2             !! optical properties to be added to original

    integer  :: icol, ilay, igpt

    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          tau1(icol,ilay,igpt) = tau1(icol,ilay,igpt) + tau2(icol,ilay,igpt)
        end do
      end do
    end do
  end subroutine increment_1scalar_by_1scalar
  ! ---------------------------------
  !> increase absorption optical depth with extinction optical depth (2-stream form)
  pure subroutine increment_1scalar_by_2stream(ncol, nlay, ngpt, &
                                               tau1,             &
                                               tau2, ssa2) bind(C, name="rte_increment_1scalar_by_2stream")
    integer,                             intent(in   ) :: ncol, nlay, ngpt !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1             !! optical properties to be modified
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2       !! optical properties to be added to original

    integer  :: icol, ilay, igpt

    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          tau1(icol,ilay,igpt) = tau1(icol,ilay,igpt) + &
                                 tau2(icol,ilay,igpt) * (1._wp - ssa2(icol,ilay,igpt))
        end do
      end do
    end do
  end subroutine increment_1scalar_by_2stream
  ! ---------------------------------
  !> increase absorption optical depth with extinction optical depth (n-stream form)
  pure subroutine increment_1scalar_by_nstream(ncol, nlay, ngpt, &
                                               tau1,             &
                                               tau2, ssa2) bind(C, name="rte_increment_1scalar_by_nstream")
    integer,                             intent(in   ) :: ncol, nlay, ngpt !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1             !! optical properties to be modified
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2       !! optical properties to be added to original

    integer  :: icol, ilay, igpt

    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          tau1(icol,ilay,igpt) = tau1(icol,ilay,igpt) + &
                                 tau2(icol,ilay,igpt) * (1._wp - ssa2(icol,ilay,igpt))
        end do
      end do
    end do
  end subroutine increment_1scalar_by_nstream
  ! ---------------------------------
  ! ---------------------------------
  !> increment two-stream optical properties \(\tau, \omega_0, g\) with absorption optical depth
  pure subroutine increment_2stream_by_1scalar(ncol, nlay, ngpt, &
                                               tau1, ssa1,       &
                                               tau2) bind(C, name="rte_increment_2stream_by_1scalar")
    integer,                             intent(in   ) :: ncol, nlay, ngpt !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1       !! optical properties to be modified
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2             !! optical properties to be added to original

    integer  :: icol, ilay, igpt
    real(wp) :: tau12

    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,igpt)
          ssa1(icol,ilay,igpt) = tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) / max(eps,tau12)
          tau1(icol,ilay,igpt) = tau12
          ! g is unchanged
        end do
      end do
    end do
  end subroutine increment_2stream_by_1scalar
  ! ---------------------------------
  !> increment two-stream optical properties \(\tau, \omega_0, g\) with a second set
  pure subroutine increment_2stream_by_2stream(ncol, nlay, ngpt, &
                                               tau1, ssa1, g1,   &
                                               tau2, ssa2, g2) bind(C, name="rte_increment_2stream_by_2stream")
    integer,                             intent(in   ) :: ncol, nlay, ngpt !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1, g1   !! optical properties to be modified
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2, g2   !! optical properties to be added to original

    integer :: icol, ilay, igpt
    real(wp) :: tau12, tauscat12

    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          ! t=tau1 + tau2
          tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,igpt)
          ! w=(tau1*ssa1 + tau2*ssa2) / t
          tauscat12 = tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) + &
                      tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt)
          g1(icol,ilay,igpt) = &
            (tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) * g1(icol,ilay,igpt) + &
             tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt) * g2(icol,ilay,igpt)) &
              / max(eps,tauscat12)
          ssa1(icol,ilay,igpt) = tauscat12 / max(eps,tau12)
          tau1(icol,ilay,igpt) = tau12
        end do
      end do
    end do
  end subroutine increment_2stream_by_2stream
  ! ---------------------------------
  !> increment two-stream optical properties \(\tau, \omega_0, g\) with _n_-stream
  pure subroutine increment_2stream_by_nstream(ncol, nlay, ngpt, nmom2, &
                                               tau1, ssa1, g1,          &
                                               tau2, ssa2, p2) bind(C, name="rte_increment_2stream_by_nstream")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nmom2  !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1, g1           !! optical properties to be modified
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2               !! optical properties to be added to original
    real(wp), dimension(nmom2, &
                        ncol,nlay,ngpt), intent(in   ) :: p2                       !! moments of the phase function to be added

    integer  :: icol, ilay, igpt
    real(wp) :: tau12, tauscat12

    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          ! t=tau1 + tau2
          tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,igpt)
          ! w=(tau1*ssa1 + tau2*ssa2) / t
          tauscat12 = &
             tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) + &
             tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt)
          g1(icol,ilay,igpt) = &
            (tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) * g1(   icol,ilay,igpt)+ &
             tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt) * p2(1, icol,ilay,igpt)) / max(eps,tauscat12)
          ssa1(icol,ilay,igpt) = tauscat12 / max(eps,tau12)
          tau1(icol,ilay,igpt) = tau12
        end do
      end do
    end do
  end subroutine increment_2stream_by_nstream
  ! ---------------------------------
  ! ---------------------------------
  !> increment _n_-stream optical properties \(\tau, \omega_0, p\) with absorption optical depth
  pure subroutine increment_nstream_by_1scalar(ncol, nlay, ngpt, &
                                               tau1, ssa1,       &
                                               tau2) bind(C, name="rte_increment_nstream_by_1scalar")
    integer,                             intent(in   ) :: ncol, nlay, ngpt  !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1        !! optical properties to be modified
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2              !! optical properties to be added to original

    integer  :: icol, ilay, igpt
    real(wp) :: tau12

    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,igpt)
          ssa1(icol,ilay,igpt) = tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) / max(eps,tau12)
          tau1(icol,ilay,igpt) = tau12
          ! p is unchanged
        end do
      end do
    end do
  end subroutine increment_nstream_by_1scalar
  ! ---------------------------------
  !> increment _n_-stream optical properties \(\tau, \omega_0, p\) with two-stream values
  pure subroutine increment_nstream_by_2stream(ncol, nlay, ngpt, nmom1, &
                                               tau1, ssa1, p1,          &
                                               tau2, ssa2, g2) bind(C, name="rte_increment_nstream_by_2stream")
    integer,                              intent(in   ) :: ncol, nlay, ngpt, nmom1  !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1                !! optical properties to be modified
    real(wp), dimension(nmom1, &
                        ncol,nlay,ngpt), intent(inout) :: p1                        !! moments of the phase function be modified
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2, g2            !! optical properties to be added to original

    integer  :: icol, ilay, igpt
    real(wp) :: tau12, tauscat12
    real(wp), dimension(nmom1) :: temp_moms ! TK
    integer  :: imom  !TK

    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,igpt)
          tauscat12 = &
             tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) + &
             tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt)
          !
          ! Here assume Henyey-Greenstein
          !
          temp_moms(1) = g2(icol,ilay,igpt)
          do imom = 2, nmom1
            temp_moms(imom) = temp_moms(imom-1) * g2(icol,ilay,igpt)
          end do
          p1(1:nmom1, icol,ilay,igpt) = &
              (tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) * p1(1:nmom1, icol,ilay,igpt) + &
               tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt) * temp_moms(1:nmom1)  ) / max(eps,tauscat12)
          ssa1(icol,ilay,igpt) = tauscat12 / max(eps,tau12)
          tau1(icol,ilay,igpt) = tau12
        end do
      end do
    end do
  end subroutine increment_nstream_by_2stream
  ! ---------------------------------
  !> increment one set of _n_-stream optical properties with another set
  pure subroutine increment_nstream_by_nstream(ncol, nlay, ngpt, nmom1, nmom2, &
                                               tau1, ssa1, p1,                 &
                                               tau2, ssa2, p2) bind(C, name="rte_increment_nstream_by_nstream")
    integer,                              intent(in   ) :: ncol, nlay, ngpt, nmom1, nmom2  !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1   !! optical properties to be modified
    real(wp), dimension(nmom1, &
                        ncol,nlay,ngpt), intent(inout) :: p1           !! moments of the phase function be modified
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2   !! optical properties to be added to original
    real(wp), dimension(nmom2, &
                        ncol,nlay,ngpt), intent(in   ) :: p2           !! moments of the phase function to be added

    integer  :: icol, ilay, igpt, mom_lim
    real(wp) :: tau12, tauscat12

    mom_lim = min(nmom1, nmom2)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,igpt)
          tauscat12 = &
             tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) + &
             tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt)
          !
          ! If op2 has more moments than op1 these are ignored;
          !   if it has fewer moments the higher orders are assumed to be 0
          !
          p1(1:mom_lim, icol,ilay,igpt) = &
              (tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) * p1(1:mom_lim, icol,ilay,igpt) + &
               tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt) * p2(1:mom_lim, icol,ilay,igpt)) / max(eps,tauscat12)
          ssa1(icol,ilay,igpt) = tauscat12 / max(eps,tau12)
          tau1(icol,ilay,igpt) = tau12
        end do
      end do
    end do
  end subroutine increment_nstream_by_nstream
  ! -------------------------------------------------------------------------------------------------
  !
  ! Incrementing when the second set of optical properties is defined at lower spectral resolution
  !   (e.g. by band instead of by gpoint)
  !
  ! -------------------------------------------------------------------------------------------------
  !> increase one absorption optical depth defined on g-points by a second value defined on bands
  pure subroutine inc_1scalar_by_1scalar_bybnd(ncol, nlay, ngpt, &
                                               tau1,             &
                                               tau2,             &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_1scalar_by_1scalar_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd  !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1     !! optical properties to be modified (defined on g-points)
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2     !! optical properties to be added to original (defined on bands)
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims !! Starting and ending gpoint for each band

    integer :: ibnd, igpt

    do ibnd = 1, nbnd
      do igpt = gpt_lims(1, ibnd), gpt_lims(2, ibnd)
        tau1(:,:,igpt) = tau1(:,:,igpt) + tau2(:,:,ibnd)
      end do
    end do
  end subroutine inc_1scalar_by_1scalar_bybnd
  ! ---------------------------------
  !> increase absorption optical depth defined on g-points  with extinction optical depth (2-stream form) defined on bands
  pure subroutine inc_1scalar_by_2stream_bybnd(ncol, nlay, ngpt, &
                                               tau1,             &
                                               tau2, ssa2,       &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_1scalar_by_2stream_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd  !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1        !! optical properties to be modified (defined on g-points)
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2  !! optical properties to be added to original (defined on bands)
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims    !! Starting and ending gpoint for each band

    integer :: ibnd, igpt

    do ibnd = 1, nbnd
      do igpt = gpt_lims(1, ibnd), gpt_lims(2, ibnd)
        tau1(:,:,igpt) = tau1(:,:,igpt) + tau2(:,:,ibnd) * (1._wp - ssa2(:,:,ibnd))
      end do
    end do
  end subroutine inc_1scalar_by_2stream_bybnd
  ! ---------------------------------
  !> increase absorption optical depth defined on g-points  with extinction optical depth (n-stream form) defined on bands
  pure subroutine inc_1scalar_by_nstream_bybnd(ncol, nlay, ngpt, &
                                               tau1,             &
                                               tau2, ssa2,       &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_1scalar_by_nstream_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd  !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1       !! optical properties to be modified (defined on g-points)
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2 !! optical properties to be added to original (defined on bands)
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims   !! Starting and ending gpoint for each band

    integer :: ibnd, igpt

    do ibnd = 1, nbnd
      do igpt = gpt_lims(1, ibnd), gpt_lims(2, ibnd)
        tau1(:,:,igpt) = tau1(:,:,igpt) + tau2(:,:,ibnd) * (1._wp - ssa2(:,:,ibnd))
      end do
    end do
  end subroutine inc_1scalar_by_nstream_bybnd

    ! ---------------------------------
  !> increment two-stream optical properties \(\tau, \omega_0, g\) defined on g-points with absorption optical depth defined on bands
  pure subroutine inc_2stream_by_1scalar_bybnd(ncol, nlay, ngpt, &
                                               tau1, ssa1,       &
                                               tau2,             &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_2stream_by_1scalar_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd  !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1 !! optical properties to be modified (defined on g-points)
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2       !! optical properties to be added to original (defined on bands)
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims   !! Starting and ending gpoint for each band

    integer  :: icol, ilay, igpt, ibnd
    real(wp) :: tau12

    do ibnd = 1, nbnd
      do igpt = gpt_lims(1, ibnd), gpt_lims(2, ibnd)
        do ilay = 1, nlay
          do icol = 1, ncol
            tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,ibnd)
            ssa1(icol,ilay,igpt) = tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) / max(eps,tau12)
            tau1(icol,ilay,igpt) = tau12
            ! g is unchanged
          end do
        end do
      end do
    end do
  end subroutine inc_2stream_by_1scalar_bybnd
  ! ---------------------------------
  !> increment 2-stream optical properties defined on g-points with another set defined on bands
  pure subroutine inc_2stream_by_2stream_bybnd(ncol, nlay, ngpt, &
                                               tau1, ssa1, g1,   &
                                               tau2, ssa2, g2,   &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_2stream_by_2stream_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd  !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1, g1 !! optical properties to be modified (defined on g-points)
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2, g2 !! optical properties to be added to original (defined on bands)
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims       !! Starting and ending gpoint for each band

    integer  :: icol, ilay, igpt, ibnd
    real(wp) :: tau12, tauscat12

    do ibnd = 1, nbnd
      do igpt = gpt_lims(1, ibnd), gpt_lims(2, ibnd)
        do ilay = 1, nlay
          do icol = 1, ncol
            ! t=tau1 + tau2
            tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,ibnd)
            ! w=(tau1*ssa1 + tau2*ssa2) / t
            tauscat12 = &
               tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) + &
               tau2(icol,ilay,ibnd) * ssa2(icol,ilay,ibnd)
            g1(icol,ilay,igpt) = &
              (tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) * g1(icol,ilay,igpt) + &
               tau2(icol,ilay,ibnd) * ssa2(icol,ilay,ibnd) * g2(icol,ilay,ibnd)) / max(eps,tauscat12)
            ssa1(icol,ilay,igpt) = tauscat12 / max(eps,tau12)
            tau1(icol,ilay,igpt) = tau12
          end do
        end do
      end do
    end do
  end subroutine inc_2stream_by_2stream_bybnd
  ! ---------------------------------
  !> increment 2-stream optical properties defined on g-points with _n_-stream properties set defined on bands
  pure subroutine inc_2stream_by_nstream_bybnd(ncol, nlay, ngpt, nmom2, &
                                               tau1, ssa1, g1,          &
                                               tau2, ssa2, p2,          &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_2stream_by_nstream_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nmom2, nbnd  !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1, g1 !! optical properties to be modified (defined on g-points)
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2     !! optical properties to be added to original (defined on bands)
    real(wp), dimension(nmom2, &
                        ncol,nlay,nbnd), intent(in   ) :: p2             !! moments of the phase function to be added
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims       !! Starting and ending gpoint for each band

    integer  :: icol, ilay, igpt, ibnd
    real(wp) :: tau12, tauscat12

    do ibnd = 1, nbnd
      do igpt = gpt_lims(1, ibnd), gpt_lims(2, ibnd)
        do ilay = 1, nlay
          do icol = 1, ncol
            ! t=tau1 + tau2
            tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,ibnd)
            ! w=(tau1*ssa1 + tau2*ssa2) / t
            tauscat12 = &
               tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) + &
               tau2(icol,ilay,ibnd) * ssa2(icol,ilay,ibnd)
            g1(icol,ilay,igpt) = &
              (tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) * g1(   icol,ilay,igpt)+ &
               tau2(icol,ilay,ibnd) * ssa2(icol,ilay,ibnd) * p2(1, icol,ilay,ibnd)) / max(eps,tauscat12)
            ssa1(icol,ilay,igpt) = tauscat12 / max(eps,tau12)
            tau1(icol,ilay,igpt) = tau12
          end do
        end do
      end do
    end do
  end subroutine inc_2stream_by_nstream_bybnd
  ! ---------------------------------
  ! ---------------------------------
  !> increment _n_-stream optical properties defined on g-points with absorption optical depth defined on bands
  pure subroutine inc_nstream_by_1scalar_bybnd(ncol, nlay, ngpt, &
                                               tau1, ssa1,       &
                                               tau2,             &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_nstream_by_1scalar_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd  !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1 !! optical properties to be modified (defined on g-points)
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2       !! optical properties to be added to original (defined on bands)
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims   !! Starting and ending gpoint for each band

    integer  :: icol, ilay, igpt, ibnd
    real(wp) :: tau12

    do ibnd = 1, nbnd
      do igpt = gpt_lims(1, ibnd), gpt_lims(2, ibnd)
        do ilay = 1, nlay
          do icol = 1, ncol
            tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,ibnd)
            ssa1(icol,ilay,igpt) = tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) / max(eps,tau12)
            tau1(icol,ilay,igpt) = tau12
            ! p is unchanged
          end do
        end do
      end do
    end do
  end subroutine inc_nstream_by_1scalar_bybnd
  ! ---------------------------------
  !> increment n-stream optical properties defined on g-points with 2-stream properties set defined on bands
  pure subroutine inc_nstream_by_2stream_bybnd(ncol, nlay, ngpt, nmom1, &
                                               tau1, ssa1, p1,          &
                                               tau2, ssa2, g2,          &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_nstream_by_2stream_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nmom1, nbnd  !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1     !! optical properties to be modified (defined on g-points)
    real(wp), dimension(nmom1, &
                        ncol,nlay,ngpt), intent(inout) :: p1             !! moments of the phase function be modified
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2, g2 !! optical properties to be added to original (defined on bands)
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims       !! Starting and ending gpoint for each band

    integer  :: icol, ilay, igpt, ibnd
    real(wp) :: tau12, tauscat12
    real(wp), dimension(nmom1) :: temp_moms ! TK
    integer  :: imom  !TK

    do ibnd = 1, nbnd
      do igpt = gpt_lims(1, ibnd), gpt_lims(2, ibnd)
        do ilay = 1, nlay
          do icol = 1, ncol
            tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,ibnd)
            tauscat12 = &
               tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) + &
               tau2(icol,ilay,ibnd) * ssa2(icol,ilay,ibnd)
            !
            ! Here assume Henyey-Greenstein
            !
            temp_moms(1) = g2(icol,ilay,ibnd)
            do imom = 2, nmom1
              temp_moms(imom) = temp_moms(imom-1) * g2(icol,ilay,ibnd)
            end do
            p1(1:nmom1, icol,ilay,igpt) = &
                (tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) * p1(1:nmom1, icol,ilay,igpt) + &
                 tau2(icol,ilay,ibnd) * ssa2(icol,ilay,ibnd) * temp_moms(1:nmom1)  ) / max(eps,tauscat12)
            ssa1(icol,ilay,igpt) = tauscat12 / max(eps,tau12)
            tau1(icol,ilay,igpt) = tau12
          end do
        end do
      end do
    end do
  end subroutine inc_nstream_by_2stream_bybnd
  ! ---------------------------------
  !> increment _n_-stream optical properties defined on g-points with a second set defined on bands
  pure subroutine inc_nstream_by_nstream_bybnd(ncol, nlay, ngpt, nmom1, nmom2, &
                                               tau1, ssa1, p1,                 &
                                               tau2, ssa2, p2,                 &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_nstream_by_nstream_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nmom1, nmom2, nbnd  !! array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1 !! optical properties to be modified (defined on g-points)
    real(wp), dimension(nmom1, &
                        ncol,nlay,ngpt), intent(inout) :: p1         !! moments of the phase function be modified
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2 !! optical properties to be added to original (defined on bands)
    real(wp), dimension(nmom2, &
                        ncol,nlay,nbnd), intent(in   ) :: p2         !! moments of the phase function to be added
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims   !! Starting and ending gpoint for each band

    integer  :: icol, ilay, igpt, ibnd, mom_lim
    real(wp) :: tau12, tauscat12

    mom_lim = min(nmom1, nmom2)
    do ibnd = 1, nbnd
      do igpt = gpt_lims(1, ibnd), gpt_lims(2, ibnd)
        do ilay = 1, nlay
          do icol = 1, ncol
            tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,ibnd)
            tauscat12 = &
               tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) + &
               tau2(icol,ilay,ibnd) * ssa2(icol,ilay,ibnd)
            !
            ! If op2 has more moments than op1 these are ignored;
            !   if it has fewer moments the higher orders are assumed to be 0
            !
            p1(1:mom_lim, icol,ilay,igpt) = &
                (tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) * p1(1:mom_lim, icol,ilay,igpt) + &
                 tau2(icol,ilay,ibnd) * ssa2(icol,ilay,ibnd) * p2(1:mom_lim, icol,ilay,ibnd)) / max(eps,tauscat12)
            ssa1(icol,ilay,igpt) = tauscat12 / max(eps,tau12)
            tau1(icol,ilay,igpt) = tau12
          end do
        end do
      end do
    end do
  end subroutine inc_nstream_by_nstream_bybnd
  ! -------------------------------------------------------------------------------------------------
  !
  ! Subsetting, meaning extracting some portion of the 3D domain
  !
  ! -------------------------------------------------------------------------------------------------
  !>
  !> Extract a subset from the first dimension (normally columns) of a 3D field.
  !>   Applicable to most variables e.g. tau, ssa, g
  !>
  pure subroutine extract_subset_dim1_3d(ncol, nlay, ngpt, array_in, colS, colE, array_out) &
    bind (C, name="rte_extract_subset_dim1_3d")
    integer,                             intent(in ) :: ncol, nlay, ngpt !! Array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(in ) :: array_in         !! Array to subset
    integer,                             intent(in ) :: colS, colE       !! Starting and ending index
    real(wp), dimension(colE-colS+1,&
                             nlay,ngpt), intent(out) :: array_out        !! subset of the input array

    integer :: icol, ilay, igpt
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = colS, colE
          array_out(icol-colS+1, ilay, igpt) = array_in(icol, ilay, igpt)
        end do
      end do
    end do

  end subroutine extract_subset_dim1_3d
  ! ---------------------------------
  !> Extract a subset from the second dimension (normally columns) of a 4D field.
  !>   Applicable to phase function moments, where the first dimension is the moment
  pure subroutine extract_subset_dim2_4d(nmom, ncol, nlay, ngpt, array_in, colS, colE, array_out) &
    bind (C, name="rte_extract_subset_dim2_4d")
    integer,                                  intent(in ) :: nmom, ncol, nlay, ngpt !! Array sizes
    real(wp), dimension(nmom,ncol,nlay,ngpt), intent(in ) :: array_in               !! Array to subset
    integer,                                  intent(in ) :: colS, colE             !! Starting and ending index
    real(wp), dimension(nmom,colE-colS+1,&
                                  nlay,ngpt), intent(out) :: array_out              !! subset of the input array

    integer :: icol, ilay, igpt, imom

    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = colS, colE
          do imom = 1, nmom
            array_out(imom, icol-colS+1, ilay, igpt) = array_in(imom, icol, ilay, igpt)
          end do
        end do
      end do
    end do

  end subroutine extract_subset_dim2_4d
  ! ---------------------------------
  !
  !> Extract the absorption optical thickness \(\tau_{abs} = 1 - \omega_0 \tau_{ext}\)
  !
  pure subroutine extract_subset_absorption_tau(ncol, nlay, ngpt, tau_in, ssa_in, &
                                                colS, colE, tau_out)              &
    bind (C, name="rte_extract_subset_absorption_tau")
    integer,                             intent(in ) :: ncol, nlay, ngpt !! Array sizes
    real(wp), dimension(ncol,nlay,ngpt), intent(in ) :: tau_in, ssa_in   !! Optical thickness, single scattering albedo
    integer,                             intent(in ) :: colS, colE       !! Starting and ending index
    real(wp), dimension(colE-colS+1,&
                             nlay,ngpt), intent(out) :: tau_out          !! absorption optical thickness subset

    integer :: icol, ilay, igpt

    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = colS, colE
          tau_out(icol-colS+1, ilay, igpt) = &
            tau_in(icol, ilay, igpt) * (1._wp - ssa_in(icol, ilay, igpt))
        end do
      end do
    end do

  end subroutine extract_subset_absorption_tau
end module mo_optical_props_kernels
