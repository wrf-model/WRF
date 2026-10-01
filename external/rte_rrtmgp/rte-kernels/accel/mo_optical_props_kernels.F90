! This code is part of
! RRTM for GCM Applications - Parallel (RRTMGP)
!
! Eli Mlawer and Robert Pincus
! Andre Wehe and Jennifer Delamere
! email:  rrtmgp@aer.com
!
! Copyright 2015-2016,  Atmospheric and Environmental Research and
! Regents of the University of Colorado.  All right reserved.
!
! Use and duplication is permitted under the terms of the
!    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause
! -------------------------------------------------------------------------------------------------
!
! Description:  Addition of optical properties -- the first set are incremented by the second set.
!   There are three possible representations of optical properties (scalar = optical depth only;
!   two-stream = tau, single-scattering albedo, and asymmetry factor g, and
!   n-stream = tau, ssa, and phase function moments p.) Thus we need nine routines, three for
!   each choice of representation on the left hand side times three representations of the
!   optical properties to be added.
!   There are two sets of these nine routines. In the first the two sets of optical
!   properties are defined at the same spectral resolution. There is also a set of routines
!   to add properties defined at lower spectral resolution to a set defined at higher spectral
!   resolution (adding properties defined by band to those defined by g-point)

module mo_optical_props_kernels
  use, intrinsic :: iso_c_binding
  use mo_rte_kind, only: wp, wl
  implicit none

  public
  interface delta_scale_2str_kernel
    module procedure delta_scale_2str_f_k, delta_scale_2str_k
  end interface

  interface extract_subset
    module procedure extract_subset_dim1_3d, extract_subset_dim2_4d
    module procedure extract_subset_absorption_tau
  end interface extract_subset

  real(wp), parameter, private :: eps = 3.0_wp*tiny(1.0_wp)
contains
  ! -------------------------------------------------------------------------------------------------
  !
  ! Delta-scaling, provided only for two-stream properties at present
  !
  ! -------------------------------------------------------------------------------------------------
  ! Delta-scale
  !   user-provided value of f (forward scattering)
  !
  subroutine delta_scale_2str_f_k(ncol, nlay, ngpt, tau, ssa, g, f) &
      bind(C, name="rte_delta_scale_2str_f_k")
    integer,                               intent(in   ) :: ncol, nlay, ngpt
    real(wp), dimension(ncol, nlay, ngpt), intent(inout) ::  tau, ssa, g
    real(wp), dimension(ncol, nlay, ngpt), intent(in   ) ::  f

    real(wp) :: wf
    integer  :: icol, ilay, igpt
    ! --------------
    ! --------------

    !$acc  parallel loop collapse(3) &
    !$acc&     copy(ssa(:ncol,:nlay,:ngpt),tau(:ncol,:nlay,:ngpt)) &
    !$acc&     copyin(f(:ncol,:nlay,:ngpt)) &
    !$acc&     copy(g(:ncol,:nlay,:ngpt))
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(tofrom:ssa, tau) &
    !$omp& map(to:f) &
    !$omp& map(tofrom:g)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          if(tau(icol,ilay,igpt) > eps) then
            wf = ssa(icol,ilay,igpt) * f(icol,ilay,igpt)
            tau(icol,ilay,igpt) = (1._wp - wf) * tau(icol,ilay,igpt)
            ssa(icol,ilay,igpt) = (ssa(icol,ilay,igpt) - wf) /  (1.0_wp - wf)
            g  (icol,ilay,igpt) = (g  (icol,ilay,igpt) - f(icol,ilay,igpt)) / (1._wp - f(icol,ilay,igpt))
          end if
        end do
      end do
    end do
  end subroutine delta_scale_2str_f_k
  ! ---------------------------------
  ! Delta-scale
  !   f = g*g
  !
  subroutine delta_scale_2str_k(ncol, nlay, ngpt, tau, ssa, g) &
      bind(C, name="rte_delta_scale_2str_k")
    integer,                               intent(in   ) :: ncol, nlay, ngpt
    real(wp), dimension(ncol, nlay, ngpt), intent(inout) ::  tau, ssa, g

    real(wp) :: f, wf
    integer  :: icol, ilay, igpt
    ! --------------
    ! --------------

    !$acc  parallel loop collapse(3) &
    !$acc&     copy(tau(:ncol,:nlay,:ngpt),ssa(:ncol,:nlay,:ngpt),g(:ncol,:nlay,:ngpt))
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(tofrom:tau, ssa, g)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          if(tau(icol,ilay,igpt) > eps) then
            f  = g  (icol,ilay,igpt) * g  (icol,ilay,igpt)
            wf = ssa(icol,ilay,igpt) * f
            tau(icol,ilay,igpt) = (1._wp - wf) * tau(icol,ilay,igpt)
            ssa(icol,ilay,igpt) = (ssa(icol,ilay,igpt) - wf) / (1.0_wp - wf)
            g  (icol,ilay,igpt) = (g  (icol,ilay,igpt) -  f) / (1.0_wp -  f)
          end if
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
  subroutine increment_1scalar_by_1scalar(ncol, nlay, ngpt, &
                                               tau1,             &
                                               tau2) bind(C, name="rte_increment_1scalar_by_1scalar")
    integer,                              intent(in  ) :: ncol, nlay, ngpt
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2

    integer  :: icol, ilay, igpt
    ! --------------
    ! --------------

    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1)
    !$acc data copyin(tau2)

    !$acc  parallel loop collapse(3) 
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(to:tau2) &
    !$omp& map(tofrom:tau1)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          tau1(icol,ilay,igpt) = tau1(icol,ilay,igpt) + tau2(icol,ilay,igpt)
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine increment_1scalar_by_1scalar
  ! ---------------------------------
  ! increment 1scalar by 2stream
  subroutine increment_1scalar_by_2stream(ncol, nlay, ngpt, &
                                               tau1,             &
                                               tau2, ssa2) bind(C, name="rte_increment_1scalar_by_2stream")
    integer,                              intent(in   ) :: ncol, nlay, ngpt
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2

    integer  :: icol, ilay, igpt
    ! --------------
    ! --------------

    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1)
    !$acc data copyin(tau2, ssa2)

    !$acc  parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(to:tau2) &
    !$omp& map(tofrom:tau1) &
    !$omp& map(to:ssa2)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          tau1(icol,ilay,igpt) = tau1(icol,ilay,igpt) + &
                                 tau2(icol,ilay,igpt) * (1._wp - ssa2(icol,ilay,igpt))
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine increment_1scalar_by_2stream
  ! ---------------------------------
  ! increment 1scalar by nstream
  subroutine increment_1scalar_by_nstream(ncol, nlay, ngpt, &
                                               tau1,             &
                                               tau2, ssa2) bind(C, name="rte_increment_1scalar_by_nstream")
    integer,                              intent(in   ) :: ncol, nlay, ngpt
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2

    integer  :: icol, ilay, igpt
    ! --------------
    ! --------------
    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1)
    !$acc data copyin(tau2, ssa2)

    !$acc  parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(to:tau2) &
    !$omp& map(tofrom:tau1) &
    !$omp& map(to:ssa2)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          tau1(icol,ilay,igpt) = tau1(icol,ilay,igpt) + &
                                 tau2(icol,ilay,igpt) * (1._wp - ssa2(icol,ilay,igpt))
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine increment_1scalar_by_nstream
  ! ---------------------------------
  ! ---------------------------------
  ! increment 2stream by 1scalar
  subroutine increment_2stream_by_1scalar(ncol, nlay, ngpt, &
                                               tau1, ssa1,       &
                                               tau2) bind(C, name="rte_increment_2stream_by_1scalar")
    integer,                              intent(in   ) :: ncol, nlay, ngpt
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2

    integer  :: icol, ilay, igpt
    real(wp) :: tau12
    ! --------------
    ! --------------
    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1, ssa1)
    !$acc data copyin(tau2)

    !$acc  parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(tofrom:ssa1) &
    !$omp& map(to:tau2) &
    !$omp& map(tofrom:tau1)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,igpt)
          if(tau12 > eps) then
            ssa1(icol,ilay,igpt) = tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) / tau12
            tau1(icol,ilay,igpt) = tau12
            ! g is unchanged
          end if
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine increment_2stream_by_1scalar
  ! ---------------------------------
  ! increment 2stream by 2stream
  subroutine increment_2stream_by_2stream(ncol, nlay, ngpt, &
                                               tau1, ssa1, g1,   &
                                               tau2, ssa2, g2) bind(C, name="rte_increment_2stream_by_2stream")
    integer,                              intent(in   ) :: ncol, nlay, ngpt
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1, g1
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2, g2

    integer :: icol, ilay, igpt
    real(wp) :: tau12, tauscat12
    ! --------------
    ! --------------
    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1, ssa1, g1)
    !$acc data copyin(tau2, ssa2, g2)

    !$acc  parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(to:g2) &
    !$omp& map(tofrom:ssa1) &
    !$omp& map(to:ssa2, tau2) &
    !$omp& map(tofrom:tau1, g1)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          ! t=tau1 + tau2
          tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,igpt)
          ! w=(tau1*ssa1 + tau2*ssa2) / t
          tauscat12 = tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) + &
                      tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt)
          if(tauscat12 > eps) then
            g1(icol,ilay,igpt) = &
              (tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) * g1(icol,ilay,igpt) + &
               tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt) * g2(icol,ilay,igpt)) &
                / tauscat12
            ssa1(icol,ilay,igpt) = tauscat12 / tau12
            tau1(icol,ilay,igpt) = tau12
          end if
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine increment_2stream_by_2stream
  ! ---------------------------------
  ! increment 2stream by nstream
  subroutine increment_2stream_by_nstream(ncol, nlay, ngpt, nmom2, &
                                               tau1, ssa1, g1,          &
                                               tau2, ssa2, p2) bind(C, name="rte_increment_2stream_by_nstream")
    integer,                              intent(in   ) :: ncol, nlay, ngpt, nmom2
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1, g1
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2
    real(wp), dimension(nmom2, &
                        ncol,nlay,ngpt), intent(in   ) :: p2

    integer  :: icol, ilay, igpt
    real(wp) :: tau12, tauscat12
    ! --------------
    ! --------------
    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1, ssa1, g1)
    !$acc data copyin(tau2, ssa2, p2)

    !$acc  parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(to:p2) &
    !$omp& map(tofrom:ssa1) &
    !$omp& map(to:ssa2, tau2) &
    !$omp& map(tofrom:tau1, g1)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          ! t=tau1 + tau2
          tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,igpt)
          ! w=(tau1*ssa1 + tau2*ssa2) / t
          tauscat12 = &
             tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) + &
             tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt)
          if(tauscat12 > eps) then
            g1(icol,ilay,igpt) = &
              (tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) * g1(   icol,ilay,igpt)+ &
               tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt) * p2(1, icol,ilay,igpt)) / tauscat12
            ssa1(icol,ilay,igpt) = tauscat12 / tau12
            tau1(icol,ilay,igpt) = tau12
          end if
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine increment_2stream_by_nstream
  ! ---------------------------------
  ! ---------------------------------
  ! increment nstream by 1scalar
  subroutine increment_nstream_by_1scalar(ncol, nlay, ngpt, &
                                               tau1, ssa1,       &
                                               tau2) bind(C, name="rte_increment_nstream_by_1scalar")
    integer,                              intent(in   ) :: ncol, nlay, ngpt
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2

    integer  :: icol, ilay, igpt
    real(wp) :: tau12
    ! --------------
    ! --------------
    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1, ssa1)
    !$acc data copyin(tau2)

    !$acc  parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(tofrom:ssa1) &
    !$omp& map(to:tau2) &
    !$omp& map(tofrom:tau1)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,igpt)
          if(tau12 > eps) then
            ssa1(icol,ilay,igpt) = tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) / tau12
            tau1(icol,ilay,igpt) = tau12
            ! p is unchanged
          end if
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine increment_nstream_by_1scalar
  ! ---------------------------------
  ! increment nstream by 2stream
  subroutine increment_nstream_by_2stream(ncol, nlay, ngpt, nmom1, &
                                               tau1, ssa1, p1,          &
                                               tau2, ssa2, g2) bind(C, name="rte_increment_nstream_by_2stream")
    integer,                              intent(in   ) :: ncol, nlay, ngpt, nmom1
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1
    real(wp), dimension(nmom1, &
                        ncol,nlay,ngpt), intent(inout) :: p1
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2, g2

    integer  :: icol, ilay, igpt
    real(wp) :: tau12, tauscat12
    real(wp) :: temp_mom ! TK
    integer  :: imom  !TK
    ! --------------
    ! --------------
    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1, ssa1, p1)
    !$acc data copyin(tau2, ssa2, g2)

    !$acc parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(tofrom:p1, ssa1) &
    !$omp& map(to:ssa2) &
    !$omp& map(tofrom:tau1) &
    !$omp& map(to:g2) &
    !$omp& map(to:tau2)
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
          if(tauscat12 > eps) then
            temp_mom = g2(icol,ilay,igpt)
            do imom = 1, nmom1
               p1(imom, icol,ilay,igpt) = &
                    (tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) * p1(imom, icol,ilay,igpt) + &
                    tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt) * temp_mom) / tauscat12
               temp_mom = temp_mom * g2(icol,ilay,igpt)
            end do
            ssa1(icol,ilay,igpt) = tauscat12 / tau12
            tau1(icol,ilay,igpt) = tau12
         end if
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine increment_nstream_by_2stream
  ! ---------------------------------
  ! increment nstream by nstream
  subroutine increment_nstream_by_nstream(ncol, nlay, ngpt, nmom1, nmom2, &
                                               tau1, ssa1, p1,                 &
                                               tau2, ssa2, p2) bind(C, name="rte_increment_nstream_by_nstream")
    integer,                              intent(in   ) :: ncol, nlay, ngpt, nmom1, nmom2
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1
    real(wp), dimension(nmom1, &
                        ncol,nlay,ngpt), intent(inout) :: p1
    real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2
    real(wp), dimension(nmom2, &
                        ncol,nlay,ngpt), intent(in   ) :: p2

    integer  :: icol, ilay, igpt, mom_lim
    real(wp) :: tau12, tauscat12
    ! --------------
    ! --------------
    mom_lim = min(nmom1, nmom2)
    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1, ssa1, p1)
    !$acc data copyin(tau2, ssa2, p2)

    !$acc  parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(to:p2) &
    !$omp& map(tofrom:ssa1) &
    !$omp& map(to:ssa2, tau2) &
    !$omp& map(tofrom:tau1, p1)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,igpt)
          tauscat12 = &
             tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) + &
             tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt)
          if(tauscat12 > eps) then
            !
            ! If op2 has more moments than op1 these are ignored;
            !   if it has fewer moments the higher orders are assumed to be 0
            !
            p1(1:mom_lim, icol,ilay,igpt) = &
                (tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) * p1(1:mom_lim, icol,ilay,igpt) + &
                 tau2(icol,ilay,igpt) * ssa2(icol,ilay,igpt) * p2(1:mom_lim, icol,ilay,igpt)) / max(eps,tauscat12)
            ssa1(icol,ilay,igpt) = tauscat12 / max(eps,tau12)
            tau1(icol,ilay,igpt) = tau12
          end if
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine increment_nstream_by_nstream
  ! ---------------------------------
  !
  ! Incrementing when the second set of optical properties is defined at lower spectral resolution
  !   (e.g. by band instead of by gpoint)
  !
  ! ---------------------------------
  subroutine inc_1scalar_by_1scalar_bybnd(ncol, nlay, ngpt, &
                                               tau1,             &
                                               tau2,             &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_1scalar_by_1scalar_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims ! Starting and ending gpoint for each band
    integer :: ibnd, igpt, icol, ilay

    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1)
    !$acc data copyin(tau2, gpt_lims)

    !$acc parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(to:tau2) &
    !$omp& map(tofrom:tau1) &
    !$omp& map(to:gpt_lims)
    do igpt = 1 , ngpt
      do ilay = 1 , nlay
        do icol = 1 , ncol
          do ibnd = 1, nbnd
            if (igpt >= gpt_lims(1, ibnd) .and. igpt <= gpt_lims(2, ibnd) ) then
              tau1(icol,ilay,igpt) = tau1(icol,ilay,igpt) + tau2(icol,ilay,ibnd)
            endif
          end do
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine inc_1scalar_by_1scalar_bybnd
  ! ---------------------------------
  ! increment 1scalar by 2stream
  subroutine inc_1scalar_by_2stream_bybnd(ncol, nlay, ngpt, &
                                               tau1,             &
                                               tau2, ssa2,       &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_1scalar_by_2stream_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims ! Starting and ending gpoint for each band
    integer :: ibnd, igpt, icol, ilay

    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1)
    !$acc data copyin(tau2, ssa2, gpt_lims)

    !$acc parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(to:tau2, ssa2) &
    !$omp& map(tofrom:tau1) &
    !$omp& map(to:gpt_lims)
    do igpt = 1 , ngpt
      do ilay = 1 , nlay
        do icol = 1 , ncol
          do ibnd = 1, nbnd
            if (igpt >= gpt_lims(1, ibnd) .and. igpt <= gpt_lims(2, ibnd) ) then
              tau1(icol,ilay,igpt) = tau1(icol,ilay,igpt) + tau2(icol,ilay,ibnd) * (1._wp - ssa2(icol,ilay,ibnd))
            endif
          end do
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine inc_1scalar_by_2stream_bybnd
  ! ---------------------------------
  ! increment 1scalar by nstream
  subroutine inc_1scalar_by_nstream_bybnd(ncol, nlay, ngpt, &
                                               tau1,             &
                                               tau2, ssa2,       &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_1scalar_by_nstream_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims ! Starting and ending gpoint for each band
    integer :: ibnd, igpt, icol, ilay

    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1)
    !$acc data copyin(tau2, ssa2, gpt_lims)

    !$acc parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(to:gpt_lims, tau2) &
    !$omp& map(tofrom:tau1) &
    !$omp& map(to:ssa2)
    do igpt = 1 , ngpt
      do ilay = 1 , nlay
        do icol = 1 , ncol
          do ibnd = 1, nbnd
            if (igpt >= gpt_lims(1, ibnd) .and. igpt <= gpt_lims(2, ibnd) ) then
              tau1(icol,ilay,igpt) = tau1(icol,ilay,igpt) + tau2(icol,ilay,ibnd) * (1._wp - ssa2(icol,ilay,ibnd))
            endif
          end do
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine inc_1scalar_by_nstream_bybnd

    ! ---------------------------------
  ! increment 2stream by 1scalar
  subroutine inc_2stream_by_1scalar_bybnd(ncol, nlay, ngpt, &
                                               tau1, ssa1,       &
                                               tau2,             &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_2stream_by_1scalar_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims ! Starting and ending gpoint for each band

    integer  :: icol, ilay, igpt, ibnd
    real(wp) :: tau12

    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1, ssa1)
    !$acc data copyin(tau2, gpt_lims)

    !$acc parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(tofrom:tau1) &
    !$omp& map(to:tau2) &
    !$omp& map(tofrom:ssa1) &
    !$omp& map(to:gpt_lims)
    do igpt = 1 , ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          do ibnd = 1, nbnd
            if (igpt >= gpt_lims(1, ibnd) .and. igpt <= gpt_lims(2, ibnd) ) then
              tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,ibnd)
              ssa1(icol,ilay,igpt) = tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) / max(eps,tau12)
              tau1(icol,ilay,igpt) = tau12
              ! g is unchanged
            endif
          end do
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine inc_2stream_by_1scalar_bybnd
  ! ---------------------------------
  ! increment 2stream by 2stream
  subroutine inc_2stream_by_2stream_bybnd(ncol, nlay, ngpt, &
                                               tau1, ssa1, g1,   &
                                               tau2, ssa2, g2,   &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_2stream_by_2stream_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1, g1
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2, g2
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims ! Starting and ending gpoint for each band
    integer  :: icol, ilay, igpt, ibnd
    real(wp) :: tau12, tauscat12

    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1, ssa1, g1)
    !$acc data copyin(tau2, ssa2, g2, gpt_lims)

    !$acc parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(tofrom:tau1) &
    !$omp& map(to:tau2, ssa2) &
    !$omp& map(tofrom:ssa1) &
    !$omp& map(to:gpt_lims) &
    !$omp& map(tofrom:g1) &
    !$omp& map(to:g2)
    do igpt = 1 , ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          do ibnd = 1, nbnd
            if (igpt >= gpt_lims(1, ibnd) .and. igpt <= gpt_lims(2, ibnd) ) then
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
            endif
          end do
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine inc_2stream_by_2stream_bybnd
  ! ---------------------------------
  ! increment 2stream by nstream
  subroutine inc_2stream_by_nstream_bybnd(ncol, nlay, ngpt, nmom2, &
                                               tau1, ssa1, g1,          &
                                               tau2, ssa2, p2,          &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_2stream_by_nstream_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nmom2, nbnd
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1, g1
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2
    real(wp), dimension(nmom2, &
                        ncol,nlay,nbnd), intent(in   ) :: p2
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims ! Starting and ending gpoint for each band

    integer  :: icol, ilay, igpt, ibnd
    real(wp) :: tau12, tauscat12

    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1, ssa1, g1)
    !$acc data copyin(tau2, ssa2, p2, gpt_lims)

    !$acc parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(tofrom:tau1) &
    !$omp& map(to:tau2, ssa2) &
    !$omp& map(tofrom:ssa1) &
    !$omp& map(to:p2, gpt_lims) &
    !$omp& map(tofrom:g1)
    do igpt = 1 , ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          do ibnd = 1, nbnd
            if (igpt >= gpt_lims(1, ibnd) .and. igpt <= gpt_lims(2, ibnd) ) then
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
            endif
          end do
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine inc_2stream_by_nstream_bybnd
  ! ---------------------------------
  ! ---------------------------------
  ! increment nstream by 1scalar
  subroutine inc_nstream_by_1scalar_bybnd(ncol, nlay, ngpt, &
                                               tau1, ssa1,       &
                                               tau2,             &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_nstream_by_1scalar_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims ! Starting and ending gpoint for each band

    integer  :: icol, ilay, igpt, ibnd
    real(wp) :: tau12

    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1, ssa1)
    !$acc data copyin(tau2, gpt_lims)

    !$acc parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(tofrom:tau1) &
    !$omp& map(to:tau2) &
    !$omp& map(tofrom:ssa1) &
    !$omp& map(to:gpt_lims)
    do igpt = 1 , ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          do ibnd = 1, nbnd
            if (igpt >= gpt_lims(1, ibnd) .and. igpt <= gpt_lims(2, ibnd) ) then
              tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,ibnd)
              ssa1(icol,ilay,igpt) = tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) / max(eps,tau12)
              tau1(icol,ilay,igpt) = tau12
              ! p is unchanged
            endif
          end do
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine inc_nstream_by_1scalar_bybnd
  ! ---------------------------------
  ! increment nstream by 2stream
  subroutine inc_nstream_by_2stream_bybnd(ncol, nlay, ngpt, nmom1, &
                                               tau1, ssa1, p1,          &
                                               tau2, ssa2, g2,          &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_nstream_by_2stream_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nmom1, nbnd
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1
    real(wp), dimension(nmom1, &
                        ncol,nlay,ngpt), intent(inout) :: p1
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2, g2
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims ! Starting and ending gpoint for each band

    integer  :: icol, ilay, igpt, ibnd
    real(wp) :: tau12, tauscat12
    real(wp) :: temp_mom ! TK
    integer  :: imom  !TK

    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1, ssa1, p1)
    !$acc data copyin(tau2, ssa2, g2, gpt_lims)

    !$acc parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(tofrom:tau1) &
    !$omp& map(to:ssa2) &
    !$omp& map(tofrom:ssa1, p1) &
    !$omp& map(to:tau2) &
    !$omp& map(to:gpt_lims, g2)
    do igpt = 1 , ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          do ibnd = 1, nbnd
            if (igpt >= gpt_lims(1, ibnd) .and. igpt <= gpt_lims(2, ibnd) ) then
              tau12 = tau1(icol,ilay,igpt) + tau2(icol,ilay,ibnd)
              tauscat12 = &
                 tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) + &
                 tau2(icol,ilay,ibnd) * ssa2(icol,ilay,ibnd)
              !
              ! Here assume Henyey-Greenstein
              !
              temp_mom = g2(icol,ilay,ibnd)
              do imom = 1, nmom1
                 p1(imom, icol,ilay,igpt) = &
                      (tau1(icol,ilay,igpt) * ssa1(icol,ilay,igpt) * p1(imom, icol,ilay,igpt) + &
                      tau2(icol,ilay,ibnd) * ssa2(icol,ilay,ibnd) * temp_mom  ) / max(eps,tauscat12)
                 temp_mom = temp_mom * g2(icol,ilay,igpt)
              end do
              ssa1(icol,ilay,igpt) = tauscat12 / max(eps,tau12)
              tau1(icol,ilay,igpt) = tau12
            endif
          end do
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine inc_nstream_by_2stream_bybnd
  ! ---------------------------------
  ! increment nstream by nstream
  subroutine inc_nstream_by_nstream_bybnd(ncol, nlay, ngpt, nmom1, nmom2, &
                                               tau1, ssa1, p1,                 &
                                               tau2, ssa2, p2,                 &
                                               nbnd, gpt_lims) bind(C, name="rte_inc_nstream_by_nstream_bybnd")
    integer,                             intent(in   ) :: ncol, nlay, ngpt, nmom1, nmom2, nbnd
    real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1
    real(wp), dimension(nmom1, &
                        ncol,nlay,ngpt), intent(inout) :: p1
    real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2
    real(wp), dimension(nmom2, &
                        ncol,nlay,nbnd), intent(in   ) :: p2
    integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims ! Starting and ending gpoint for each band

    integer  :: icol, ilay, igpt, ibnd, mom_lim
    real(wp) :: tau12, tauscat12

    mom_lim = min(nmom1, nmom2)
    ! tau1 and tau2 might be the same array, thus we need to perform copy and copyin
    ! in separate steps. Otherwise, at the time of copyout of tau1 runtime may see
    ! it as present (as present counter of tau2, having the same memory address) 
    ! is not necessarily decrement yet, and copyout action is not carried out
    ! (present_or_copyout semantic)
    !$acc data copy(tau1, ssa1, p1)
    !$acc data copyin(tau2, ssa2, p2, gpt_lims)

    !$acc parallel loop collapse(3)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(to:p2) &
    !$omp& map(tofrom:ssa1) &
    !$omp& map(to:ssa2) &
    !$omp& map(tofrom:tau1) &
    !$omp& map(to:tau2) &
    !$omp& map(tofrom:p1) &
    !$omp& map(to:gpt_lims)
    do igpt = 1 , ngpt
      do ilay = 1, nlay
        do icol = 1, ncol
          do ibnd = 1, nbnd
            if (igpt >= gpt_lims(1, ibnd) .and. igpt <= gpt_lims(2, ibnd) ) then
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
            endif
          end do
        end do
      end do
    end do

  !$acc end data
  !$acc end data
  end subroutine inc_nstream_by_nstream_bybnd
  ! ---------------------------------
  ! -------------------------------------------------------------------------------------------------
  !
  ! Subsetting, meaning extracting some portion of the 3D domain
  !
  ! -------------------------------------------------------------------------------------------------
  subroutine extract_subset_dim1_3d(ncol, nlay, ngpt, array_in, colS, colE, array_out) &
    bind (C, name="rte_extract_subset_dim1_3d")
    integer,                             intent(in ) :: ncol, nlay, ngpt
    real(wp), dimension(ncol,nlay,ngpt), intent(in ) :: array_in
    integer,                             intent(in ) :: colS, colE
    real(wp), dimension(colE-colS+1,&
                             nlay,ngpt), intent(out) :: array_out
    integer :: icol, ilay, igpt

    !$acc parallel loop collapse(3) &
    !$acc&     copyout(array_out) &
    !$acc&     copyin(array_in)
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(from:array_out) &
    !$omp& map(to:array_in)
    do igpt = 1, ngpt
      do ilay = 1, nlay
        do icol = colS, colE
          array_out(icol-colS+1, ilay, igpt) = array_in(icol, ilay, igpt)
        end do
      end do
    end do

  end subroutine extract_subset_dim1_3d
  ! ---------------------------------
  subroutine extract_subset_dim2_4d(nmom, ncol, nlay, ngpt, array_in, colS, colE, array_out) &
    bind (C, name="rte_extract_subset_dim2_4d")
    integer,                                  intent(in ) :: nmom, ncol, nlay, ngpt
    real(wp), dimension(nmom,ncol,nlay,ngpt), intent(in ) :: array_in
    integer,                                  intent(in ) :: colS, colE
    real(wp), dimension(nmom,colE-colS+1,&
                                  nlay,ngpt), intent(out) :: array_out

    integer :: icol, ilay, igpt, imom

    !$acc parallel loop collapse(4) &
    !$acc&     copyout(array_out(:nmom,:cole-cols+1,:nlay,:ngpt)) &
    !$acc&     copyin(array_in(:nmom,cols:cole,:nlay,:ngpt))
    !$omp target teams distribute parallel do simd collapse(4) &
    !$omp& map(from:array_out) &
    !$omp& map(to:array_in)
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
  ! Extract the absorption optical thickness which requires mulitplying by 1 - ssa
  !
  subroutine extract_subset_absorption_tau(ncol, nlay, ngpt, tau_in, ssa_in, &
                                                colS, colE, tau_out)              &
    bind (C, name="rte_extract_subset_absorption_tau")
    integer,                             intent(in ) :: ncol, nlay, ngpt
    real(wp), dimension(ncol,nlay,ngpt), intent(in ) :: tau_in, ssa_in
    integer,                             intent(in ) :: colS, colE
    real(wp), dimension(colE-colS+1,&
                             nlay,ngpt), intent(out) :: tau_out

    integer :: icol, ilay, igpt

    !$acc parallel loop collapse(3) &
    !$acc&     copyin(ssa_in(cols:cole,:nlay,:ngpt)) &
    !$acc&     copyout(tau_out(:cole-cols+1,:nlay,:ngpt)) &
    !$acc&     copyin(tau_in(cols:cole,:nlay,:ngpt))
    !$omp target teams distribute parallel do simd collapse(3) &
    !$omp& map(to:ssa_in) &
    !$omp& map(from:tau_out) &
    !$omp& map(to:tau_in)
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
