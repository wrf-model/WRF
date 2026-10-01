! This code is part of RRTM for GCM Applications - Parallel (RRTMGP)
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
!>
!> ## Generic Fortran interface for gas optics  
!> 
!> Defines an interface for gas optics parameterizations: 
!>
!>   - inquiry functions for the pressure and temperature limits 
!>   - inquiry functions for the source of radiation (internal or planetary radiation vs. 
!>      external or stellar radiation)
!>   - Method for computing gas optical optical properties and incident stellar radiation 
!>     given pressure, temperature, and gas concentrations 
!>   - Method for computing gas optical optical properties and internal Planck sources 
!>     given pressure, temperature, and gas concentrations 
!> 
!> This (abstract) class is a sub-classes of `ty_optical_props` in the RTE module `mo_optical_props`
!>   and inherits the procedures related to spectral discratization from that class. 
!> Optical properties are returned in any variable of `ty_optical_props_arry` (that is, 
!>    an array of values with dimensions ncol, nlay, ngpt) in the same module. 
!> Internal sources of radiation are provided in a variable of type 
!>   `ty_source_func_lw` in RTE module `ty_source_func_lw`. 
!> The module also makes use of [[mo_gas_concentrations(module):ty_gas_concs(type)]] from 
!>   module [[mo_gas_concentrations]]. 
!
! -------------------------------------------------------------------------------------------------
module mo_gas_optics
  use mo_rte_kind,           only: wp
  use mo_source_functions,   only: ty_source_func_lw
  use mo_gas_concentrations, only: ty_gas_concs
  use mo_optical_props,      only: ty_optical_props, ty_optical_props_arry

  type, abstract, extends(ty_optical_props), public :: ty_gas_optics
  contains
    generic,   public :: gas_optics => gas_optics_int, gas_optics_ext
    !
    ! Deferred procedures -- each must be implemented in each child class with
    !   arguments following the abstract interface (defined below)
    !
    ! gas_optics_int and gas_optics_ext should be declared private in concrete classes
    !    but need to be visible in the abstract type or the interfaces won't be inherited
    ! See https://software.intel.com/en-us/forums/intel-fortran-compiler-for-linux-and-mac-os-x/topic/681705
    !
    procedure(gas_optics_int_abstract), deferred  :: gas_optics_int
    procedure(gas_optics_ext_abstract), deferred  :: gas_optics_ext

    procedure(logical_abstract), deferred, public :: source_is_internal
    procedure(logical_abstract), deferred, public :: source_is_external
    procedure(real_abstract),    deferred, public :: get_press_min
    procedure(real_abstract),    deferred, public :: get_press_max
    procedure(real_abstract),    deferred, public :: get_temp_min
    procedure(real_abstract),    deferred, public :: get_temp_max
  end type ty_gas_optics
  !
  ! Interfaces for the methods to be implemented
  !
  abstract interface
    !--------------------------------------------------------------------------------------------------------------------
    !
    ! Compute gas optical depth given temperature, pressure, and composition
    !
    function gas_optics_ext_abstract(this,                         &
                                     play, plev, tlay, gas_desc,   & ! mandatory inputs
                                     optical_props, toa_src,       & ! mandatory outputs
                                     col_dry) result(error_msg)      ! optional input
      import ty_gas_optics, wp, ty_gas_concs, ty_optical_props_arry
      class(ty_gas_optics), intent(in) :: this
      real(wp), dimension(:,:), intent(in   ) :: play, &   !! layer pressures [Pa, mb]; (ncol,nlay)
                                                 plev, &   !! level pressures [Pa, mb]; (ncol,nlay+1)
                                                 tlay      !! layer temperatures [K]; (ncol,nlay)
      type(ty_gas_concs),       intent(in   ) :: gas_desc  !! Gas volume mixing ratios
      class(ty_optical_props_arry),  &
                                intent(inout) :: optical_props !! 
      real(wp), dimension(:,:), intent(  out) :: toa_src   !! Incoming solar irradiance(ncol,ngpt)
      character(len=128)                      :: error_msg !! Empty if successful
      ! Optional inputs
      real(wp), dimension(:,:), intent(in   ), &
                             optional, target :: col_dry !! Column dry amount (molecules/cm^2); dim(ncol,nlay)
    end function gas_optics_ext_abstract
    !--------------------------------------------------------------------------------------------------------------------
    !
    ! Compute gas optical depth and Planck source functions,
    !  given temperature, pressure, and composition
    !
    function gas_optics_int_abstract(this,                             &
                                     play, plev, tlay, tsfc, gas_desc, &
                                     optical_props, sources,           &
                                     col_dry, tlev) result(error_msg)
      import ty_gas_optics, wp, ty_gas_concs, ty_optical_props_arry, ty_source_func_lw
      class(ty_gas_optics),     intent(in   ) :: this
      real(wp), dimension(:,:), intent(in   ) :: play, &   !! layer pressures [Pa, mb]; (ncol,nlay)
                                                 plev, &   !! level pressures [Pa, mb]; (ncol,nlay+1)
                                                 tlay      !! layer temperatures [K]; (ncol,nlay)
      real(wp), dimension(:),   intent(in   ) :: tsfc      !! surface skin temperatures [K]; (ncol)
      type(ty_gas_concs),       intent(in   ) :: gas_desc  !! Gas volume mixing ratios
      class(ty_optical_props_arry),  &
                                intent(inout) :: optical_props !! Optical properties
      class(ty_source_func_lw    ),  &
                                intent(inout) :: sources    !! Planck sources
      character(len=128)                      :: error_msg  !! Empty if successful 
      real(wp), dimension(:,:), intent(in   ), &
                            optional, target :: col_dry, &  !! Column dry amount (molecules/cm^2); dim(ncol,nlay)
                                                   tlev     !! level temperatures [K]l (ncol,nlay+1)
    end function gas_optics_int_abstract
    !--------------------------------------------------------------------------------------------------------------------
    function logical_abstract(this)
      import ty_gas_optics
      class(ty_gas_optics),     intent(in   ) :: this
      logical                                 :: logical_abstract
    end function logical_abstract
    !--------------------------------------------------------------------------------------------------------------------
    function real_abstract(this)
      import ty_gas_optics, wp
      class(ty_gas_optics),     intent(in   ) :: this
      real(wp)                                :: real_abstract
    end function real_abstract
    !--------------------------------------------------------------------------------------------------------------------
  end interface
end module mo_gas_optics
