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
!>  - Extinction-only calculation (direct solar beam)
!>  - Two-stream calculations:
!>    solvers for LW and SW with different boundary conditions and source functions
!
! -------------------------------------------------------------------------------------------------
module mo_rte_solver_kernels
  use,  intrinsic :: iso_c_binding
  use mo_rte_kind,      only: wp, wl
  implicit none
  private

  public :: lw_solver_noscat, lw_solver_2stream, &
            sw_solver_noscat, sw_solver_2stream
  ! -------------------------------------------------------------------------------------------------
  !
  ! Top-level longwave kernels
  !
  ! -------------------------------------------------------------------------------------------------
  !
  !> LW transport, no scattering, multi-angle quadrature
  !>   Users provide a set of weights and quadrature angles
  !>   Routine sums over single-angle solutions for each sets of angles/weights
  !
  ! ---------------------------------------------------------------
  interface
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
      use mo_rte_kind,      only: wp, wl
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
                                                              !! Planck source at layer edge for radiation [W/m2]
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
    end subroutine lw_solver_noscat
  end interface
  ! -------------------------------------------------------------------------------------------------
  !
  !> Longwave two-stream calculation:
  !>   - combine RRTMGP-specific sources at levels
  !>   - compute layer reflectance, transmittance
  !>   - compute total source function at levels using linear-in-tau
  !>   - transport
  !
  ! -------------------------------------------------------------------------------------------------
  interface
    subroutine lw_solver_2stream (ncol, nlay, ngpt, top_at_1, &
                                  tau, ssa, g,                &
                                  lay_source, lev_source, sfc_emis, sfc_src, &
                                  inc_flux,                   &
                                  flux_up, flux_dn) bind(C, name="rte_lw_solver_2stream")
      use mo_rte_kind,      only: wp, wl
      integer,                               intent(in   ) :: ncol, nlay, ngpt
                                                              !! Number of columns, layers, g-points
      logical(wl),                           intent(in   ) :: top_at_1
                                                              !! ilay = 1 is the top of the atmosphere?
      real(wp), dimension(ncol,nlay,  ngpt), intent(in   ) :: tau, ssa, g
                                                              !! Optical thickness, single-scattering albedo, asymmetry parameter []
      real(wp), dimension(ncol,nlay,  ngpt),   intent(in   ) :: lay_source
                                                              !! Planck source at layer average temperature [W/m2]
      real(wp), dimension(ncol,nlay+1,ngpt), intent(in   ) :: lev_source
                                                              !! Planck source at layer edge for radiation [W/m2]
      real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_emis
                                                              !! Surface emissivity      []
      real(wp), dimension(ncol,       ngpt), intent(in   ) :: sfc_src
                                                              !! Surface source function [W/m2]
      real(wp), dimension(ncol,       ngpt), intent(in   ) :: inc_flux
                                                              !! Incident diffuse flux, probably 0 [W/m2]
      real(wp), dimension(ncol,nlay+1,ngpt), intent(  out) :: flux_up, flux_dn
                                                              !! Fluxes [W/m2]
    end subroutine lw_solver_2stream
  end interface
  ! -------------------------------------------------------------------------------------------------
  !
  !   Top-level shortwave kernels
  !
  ! -------------------------------------------------------------------------------------------------
  !
  !  !> Extinction-only shortwave solver i.e. solar direct beam
  !
  ! -------------------------------------------------------------------------------------------------
  interface
    pure subroutine sw_solver_noscat(ncol, nlay, ngpt, top_at_1, &
                                     tau, mu0, inc_flux_dir, flux_dir) bind(C, name="rte_sw_solver_noscat")
      use mo_rte_kind,      only: wp, wl
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
    end subroutine sw_solver_noscat
  end interface
  ! -------------------------------------------------------------------------------------------------
  !
  !> Shortwave two-stream calculation:
  !>   compute layer reflectance, transmittance
  !>   compute solar source function for diffuse radiation
  !>   transport
  !
  ! -------------------------------------------------------------------------------------------------
  interface
    subroutine sw_solver_2stream (ncol, nlay, ngpt, top_at_1,  &
                                  tau, ssa, g, mu0,           &
                                  sfc_alb_dir, sfc_alb_dif,   &
                                              inc_flux_dir,   &
                                  flux_up, flux_dn, flux_dir, &
                                  has_dif_bc, inc_flux_dif,   &
                                  do_broadband, broadband_up, &
                                  broadband_dn, broadband_dir) bind(C, name="rte_sw_solver_2stream")
      use mo_rte_kind,      only: wp, wl
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
    end subroutine sw_solver_2stream
  end interface
end module mo_rte_solver_kernels
