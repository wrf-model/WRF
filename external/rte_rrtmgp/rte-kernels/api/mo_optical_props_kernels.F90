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

  ! -------------------------------------------------------------------------------------------------
  !
  ! Delta-scaling is provided only for two-stream properties at present
  !
  interface delta_scale_2str_kernel
    ! -------------------------------------------------------------------------------------------------
    !> Delta-scale two-stream optical properties given user-provided value of \(f\) (forward scattering)
    !
    pure subroutine delta_scale_2str_f_k(ncol, nlay, ngpt, tau, ssa, g, f) &
        bind(C, name="rte_delta_scale_2str_f_k")
      use mo_rte_kind, only: wp, wl
      integer,                               intent(in   ) :: ncol, nlay, ngpt
        !! Array sizes
      real(wp), dimension(ncol, nlay, ngpt), intent(inout) ::  tau, ssa, g
        !! Optical depth, single-scattering albedo, asymmetry parameter
      real(wp), dimension(ncol, nlay, ngpt), intent(in   ) ::  f
        !! User-provided forward-scattering fraction
     end subroutine delta_scale_2str_f_k
    ! ---------------------------------
    !> Delta-scale assuming forward-scatternig fraction is the square of the asymmetry parameter
    !>    i.e. \(f = g^2\)
    !
    pure subroutine delta_scale_2str_k(ncol, nlay, ngpt, tau, ssa, g) &
        bind(C, name="rte_delta_scale_2str_k")
      use mo_rte_kind, only: wp, wl
      integer,                               intent(in   ) :: ncol, nlay, ngpt
        !! Array sizes
      real(wp), dimension(ncol, nlay, ngpt), intent(inout) ::  tau, ssa, g
        !! Optical depth, single-scattering albedo, asymmetry parameter
    end subroutine delta_scale_2str_k
  end interface delta_scale_2str_kernel
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
  interface
    pure subroutine increment_1scalar_by_1scalar(ncol, nlay, ngpt, &
                                                 tau1,             &
                                                 tau2) bind(C, name="rte_increment_1scalar_by_1scalar")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1             !! optical properties to be modified
      real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2             !! optical properties to be added to original
    end subroutine increment_1scalar_by_1scalar
  end interface 
  ! ---------------------------------
  !> increase absorption optical depth with extinction optical depth (2-stream form)
  interface
    pure subroutine increment_1scalar_by_2stream(ncol, nlay, ngpt, &
                                                 tau1,             &
                                                 tau2, ssa2) bind(C, name="rte_increment_1scalar_by_2stream")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1             !! optical properties to be modified
      real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2       !! optical properties to be added to original
    end subroutine increment_1scalar_by_2stream
  end interface 
  ! ---------------------------------
  !> increase absorption optical depth with extinction optical depth (n-stream form)
  interface
    pure subroutine increment_1scalar_by_nstream(ncol, nlay, ngpt, &
                                                 tau1,             &
                                                 tau2, ssa2) bind(C, name="rte_increment_1scalar_by_nstream")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1             !! optical properties to be modified
      real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2       !! optical properties to be added to original
    end subroutine increment_1scalar_by_nstream
  end interface 
  ! ---------------------------------
  ! ---------------------------------
  !> increment two-stream optical properties \(\tau, \omega_0, g\) with absorption optical depth
  interface
    pure subroutine increment_2stream_by_1scalar(ncol, nlay, ngpt, &
                                                 tau1, ssa1,       &
                                                 tau2) bind(C, name="rte_increment_2stream_by_1scalar")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1       !! optical properties to be modified
      real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2             !! optical properties to be added to original
    end subroutine increment_2stream_by_1scalar
  end interface 
  ! ---------------------------------
  !> increment two-stream optical properties \(\tau, \omega_0, g\) with a second set
  interface
    pure subroutine increment_2stream_by_2stream(ncol, nlay, ngpt, &
                                                 tau1, ssa1, g1,   &
                                                 tau2, ssa2, g2) bind(C, name="rte_increment_2stream_by_2stream")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1, g1   !! optical properties to be modified
      real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2, g2   !! optical properties to be added to original
    end subroutine increment_2stream_by_2stream
  end interface 
  ! ---------------------------------
  !> increment two-stream optical properties \(\tau, \omega_0, g\) with _n_-stream
  interface
    pure subroutine increment_2stream_by_nstream(ncol, nlay, ngpt, nmom2, &
                                                 tau1, ssa1, g1,          &
                                                 tau2, ssa2, p2) bind(C, name="rte_increment_2stream_by_nstream")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt, nmom2  !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1, g1           !! optical properties to be modified
      real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2               !! optical properties to be added to original
      real(wp), dimension(nmom2, &
                          ncol,nlay,ngpt), intent(in   ) :: p2                       !! moments of the phase function to be added
    end subroutine increment_2stream_by_nstream
  end interface 
  ! ---------------------------------
  ! ---------------------------------
  !> increment _n_-stream optical properties \(\tau, \omega_0, p\) with absorption optical depth
  interface
    pure subroutine increment_nstream_by_1scalar(ncol, nlay, ngpt, &
                                                 tau1, ssa1,       &
                                                 tau2) bind(C, name="rte_increment_nstream_by_1scalar")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt  !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1        !! optical properties to be modified
      real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2              !! optical properties to be added to original
    end subroutine increment_nstream_by_1scalar
  end interface 
  ! ---------------------------------
  !> increment _n_-stream optical properties \(\tau, \omega_0, p\) with two-stream values
  interface
    pure subroutine increment_nstream_by_2stream(ncol, nlay, ngpt, nmom1, &
                                                 tau1, ssa1, p1,          &
                                                 tau2, ssa2, g2) bind(C, name="rte_increment_nstream_by_2stream")
      use mo_rte_kind, only: wp, wl
      integer,                              intent(in   ) :: ncol, nlay, ngpt, nmom1  !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1                !! optical properties to be modified
      real(wp), dimension(nmom1, &
                          ncol,nlay,ngpt), intent(inout) :: p1                        !! moments of the phase function be modified
      real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2, g2            !! optical properties to be added to original
    end subroutine increment_nstream_by_2stream
  end interface 
  ! ---------------------------------
  !> increment one set of _n_-stream optical properties with another set
  interface
    pure subroutine increment_nstream_by_nstream(ncol, nlay, ngpt, nmom1, nmom2, &
                                                 tau1, ssa1, p1,                 &
                                                 tau2, ssa2, p2) bind(C, name="rte_increment_nstream_by_nstream")
      use mo_rte_kind, only: wp, wl
      integer,                              intent(in   ) :: ncol, nlay, ngpt, nmom1, nmom2  !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1   !! optical properties to be modified
      real(wp), dimension(nmom1, &
                          ncol,nlay,ngpt), intent(inout) :: p1           !! moments of the phase function be modified
      real(wp), dimension(ncol,nlay,ngpt), intent(in   ) :: tau2, ssa2   !! optical properties to be added to original
      real(wp), dimension(nmom2, &
                          ncol,nlay,ngpt), intent(in   ) :: p2           !! moments of the phase function to be added
    end subroutine increment_nstream_by_nstream
  end interface 
  ! -------------------------------------------------------------------------------------------------
  !
  ! Incrementing when the second set of optical properties is defined at lower spectral resolution
  !   (e.g. by band instead of by gpoint)
  !
  ! -------------------------------------------------------------------------------------------------
  !> increase one absorption optical depth defined on g-points by a second value defined on bands
  interface
    pure subroutine inc_1scalar_by_1scalar_bybnd(ncol, nlay, ngpt, &
                                                 tau1,             &
                                                 tau2,             &
                                                 nbnd, gpt_lims) bind(C, name="rte_inc_1scalar_by_1scalar_bybnd")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd  !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1     !! optical properties to be modified (defined on g-points)
      real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2     !! optical properties to be added to original (defined on bands)
      integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims !! Starting and ending gpoint for each band
     end subroutine inc_1scalar_by_1scalar_bybnd
  end interface 
  ! ---------------------------------
  !> increase absorption optical depth defined on g-points  with extinction optical depth (2-stream form) defined on bands
  interface
    pure subroutine inc_1scalar_by_2stream_bybnd(ncol, nlay, ngpt, &
                                                 tau1,             &
                                                 tau2, ssa2,       &
                                                 nbnd, gpt_lims) bind(C, name="rte_inc_1scalar_by_2stream_bybnd")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd  !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1        !! optical properties to be modified (defined on g-points)
      real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2  !! optical properties to be added to original (defined on bands)
      integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims    !! Starting and ending gpoint for each band
    end subroutine inc_1scalar_by_2stream_bybnd
  end interface 
  ! ---------------------------------
  !> increase absorption optical depth defined on g-points  with extinction optical depth (n-stream form) defined on bands
  interface
    pure subroutine inc_1scalar_by_nstream_bybnd(ncol, nlay, ngpt, &
                                                 tau1,             &
                                                 tau2, ssa2,       &
                                                 nbnd, gpt_lims) bind(C, name="rte_inc_1scalar_by_nstream_bybnd")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd  !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1       !! optical properties to be modified (defined on g-points)
      real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2 !! optical properties to be added to original (defined on bands)
      integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims   !! Starting and ending gpoint for each band
    end subroutine inc_1scalar_by_nstream_bybnd
  end interface 
  ! ---------------------------------
  !> increment two-stream optical properties \(\tau, \omega_0, g\) defined on g-points with absorption optical depth defined on bands
  interface
    pure subroutine inc_2stream_by_1scalar_bybnd(ncol, nlay, ngpt, &
                                                 tau1, ssa1,       &
                                                 tau2,             &
                                                 nbnd, gpt_lims) bind(C, name="rte_inc_2stream_by_1scalar_bybnd")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd  !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1 !! optical properties to be modified (defined on g-points)
      real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2       !! optical properties to be added to original (defined on bands)
      integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims   !! Starting and ending gpoint for each band
    end subroutine inc_2stream_by_1scalar_bybnd
  end interface 
  ! ---------------------------------
  !> increment 2-stream optical properties defined on g-points with another set defined on bands
  interface
    pure subroutine inc_2stream_by_2stream_bybnd(ncol, nlay, ngpt, &
                                                 tau1, ssa1, g1,   &
                                                 tau2, ssa2, g2,   &
                                                 nbnd, gpt_lims) bind(C, name="rte_inc_2stream_by_2stream_bybnd")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd  !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1, g1 !! optical properties to be modified (defined on g-points)
      real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2, g2 !! optical properties to be added to original (defined on bands)
      integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims       !! Starting and ending gpoint for each band
    end subroutine inc_2stream_by_2stream_bybnd
  end interface 
  ! ---------------------------------
  !> increment 2-stream optical properties defined on g-points with _n_-stream properties set defined on bands
  interface
    pure subroutine inc_2stream_by_nstream_bybnd(ncol, nlay, ngpt, nmom2, &
                                                 tau1, ssa1, g1,          &
                                                 tau2, ssa2, p2,          &
                                                 nbnd, gpt_lims) bind(C, name="rte_inc_2stream_by_nstream_bybnd")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt, nmom2, nbnd  !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1, g1 !! optical properties to be modified (defined on g-points)
      real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2     !! optical properties to be added to original (defined on bands)
      real(wp), dimension(nmom2, &
                          ncol,nlay,nbnd), intent(in   ) :: p2             !! moments of the phase function to be added
      integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims       !! Starting and ending gpoint for each band
    end subroutine inc_2stream_by_nstream_bybnd
  end interface 
  ! ---------------------------------
  ! ---------------------------------
  !> increment _n_-stream optical properties defined on g-points with absorption optical depth defined on bands
  interface
    pure subroutine inc_nstream_by_1scalar_bybnd(ncol, nlay, ngpt, &
                                                 tau1, ssa1,       &
                                                 tau2,             &
                                                 nbnd, gpt_lims) bind(C, name="rte_inc_nstream_by_1scalar_bybnd")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt, nbnd  !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1 !! optical properties to be modified (defined on g-points)
      real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2       !! optical properties to be added to original (defined on bands)
      integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims   !! Starting and ending gpoint for each band
    end subroutine inc_nstream_by_1scalar_bybnd
  end interface 
  ! ---------------------------------
  !> increment n-stream optical properties defined on g-points with 2-stream properties set defined on bands
  interface
    pure subroutine inc_nstream_by_2stream_bybnd(ncol, nlay, ngpt, nmom1, &
                                                 tau1, ssa1, p1,          &
                                                 tau2, ssa2, g2,          &
                                                 nbnd, gpt_lims) bind(C, name="rte_inc_nstream_by_2stream_bybnd")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt, nmom1, nbnd  !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1     !! optical properties to be modified (defined on g-points)
      real(wp), dimension(nmom1, &
                          ncol,nlay,ngpt), intent(inout) :: p1             !! moments of the phase function be modified
      real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2, g2 !! optical properties to be added to original (defined on bands)
      integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims       !! Starting and ending gpoint for each band
    end subroutine inc_nstream_by_2stream_bybnd
  end interface 
  ! ---------------------------------
  !> increment _n_-stream optical properties defined on g-points with a second set defined on bands
  interface
    pure subroutine inc_nstream_by_nstream_bybnd(ncol, nlay, ngpt, nmom1, nmom2, &
                                                 tau1, ssa1, p1,                 &
                                                 tau2, ssa2, p2,                 &
                                                 nbnd, gpt_lims) bind(C, name="rte_inc_nstream_by_nstream_bybnd")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in   ) :: ncol, nlay, ngpt, nmom1, nmom2, nbnd  !! array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(inout) :: tau1, ssa1 !! optical properties to be modified (defined on g-points)
      real(wp), dimension(nmom1, &
                          ncol,nlay,ngpt), intent(inout) :: p1         !! moments of the phase function be modified
      real(wp), dimension(ncol,nlay,nbnd), intent(in   ) :: tau2, ssa2 !! optical properties to be added to original (defined on bands)
      real(wp), dimension(nmom2, &
                          ncol,nlay,nbnd), intent(in   ) :: p2         !! moments of the phase function to be added
      integer,  dimension(2,nbnd),         intent(in   ) :: gpt_lims   !! Starting and ending gpoint for each band
    end subroutine inc_nstream_by_nstream_bybnd
  end interface 
  ! -------------------------------------------------------------------------------------------------
  !
  ! Subsetting, meaning extracting some portion of the 3D domain
  !
  ! -------------------------------------------------------------------------------------------------
  !>
  !> Extract a subset from the first dimension (normally columns) of a 3D field.
  !>   Applicable to most variables e.g. tau, ssa, g
  !>
  interface extract_subset
    pure subroutine extract_subset_dim1_3d(ncol, nlay, ngpt, array_in, colS, colE, array_out) &
      bind (C, name="rte_extract_subset_dim1_3d")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in ) :: ncol, nlay, ngpt !! Array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(in ) :: array_in         !! Array to subset
      integer,                             intent(in ) :: colS, colE       !! Starting and ending index
      real(wp), dimension(colE-colS+1,&
                               nlay,ngpt), intent(out) :: array_out        !! subset of the input array
    end subroutine extract_subset_dim1_3d
    ! ---------------------------------
    !> Extract a subset from the second dimension (normally columns) of a 4D field.
    !>   Applicable to phase function moments, where the first dimension is the moment
    pure subroutine extract_subset_dim2_4d(nmom, ncol, nlay, ngpt, array_in, colS, colE, array_out) &
      bind (C, name="rte_extract_subset_dim2_4d")
      use mo_rte_kind, only: wp, wl
      integer,                                  intent(in ) :: nmom, ncol, nlay, ngpt !! Array sizes
      real(wp), dimension(nmom,ncol,nlay,ngpt), intent(in ) :: array_in               !! Array to subset
      integer,                                  intent(in ) :: colS, colE             !! Starting and ending index
      real(wp), dimension(nmom,colE-colS+1,&
                                    nlay,ngpt), intent(out) :: array_out              !! subset of the input array
    end subroutine extract_subset_dim2_4d
    ! ---------------------------------
    !
    !> Extract the absorption optical thickness \(\tau_{abs} = 1 - \omega_0 \tau_{ext}\)
    !
    pure subroutine extract_subset_absorption_tau(ncol, nlay, ngpt, tau_in, ssa_in, &
                                                  colS, colE, tau_out)              &
      bind (C, name="rte_extract_subset_absorption_tau")
      use mo_rte_kind, only: wp, wl
      integer,                             intent(in ) :: ncol, nlay, ngpt !! Array sizes
      real(wp), dimension(ncol,nlay,ngpt), intent(in ) :: tau_in, ssa_in   !! Optical thickness, single scattering albedo
      integer,                             intent(in ) :: colS, colE       !! Starting and ending index
      real(wp), dimension(colE-colS+1,&
                               nlay,ngpt), intent(out) :: tau_out          !! absorption optical thickness subset
    end subroutine extract_subset_absorption_tau
  end interface extract_subset
end module mo_optical_props_kernels
