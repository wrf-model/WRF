! This code is part of Radiative Transfer for Energetics (RTE)
!
! Contacts: Robert Pincus and Eli Mlawer
! email:  rrtmgp@aer.com
!
! Copyright 2015-2018,  Atmospheric and Environmental Research and
! Regents of the University of Colorado.  All right reserved.
!
! Use and duplication is permitted under the terms of the
!    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause
! -------------------------------------------------------------------------------------------------
! Provides aerosol optical properties as a function of aerosol size (radius), aerosol mass,
! and relative humidity for the RRTMGP spectral bands.
!   Based on climatoligical aerosol optical properties used in MERRA2 as derived from the
!     GOCART model for 15 aerosol types, including dust and sea salt each for five size bins,
!     one sulfate type, and both hydrophobic and hydrophilic black carbon and organic carbon.
!   Input aerosol optical data are stored in look-up tables.
!
!   References for the gocart interactive aerosols:   
!     Chin et al., jgr, 2000 (https://doi.org/10.1029/2000jd900384)
!     Chin et al., jas, 2002 (https://doi.org/10.1175/1520-0469(2002)059<0461:TAOTFT>2.0.CO;2)
!     Colarco et al., jgr, 2010 (https://doi.org/10.1029/2009jd012820)
!                                                                      
!   References for merra2 aerosol reanalysis:                          
!     Randles et al., j. clim., 2017 (https://doi.org/10.1175/jcli-d-16-0609.1) 
!     Buchard et al., j. clim., 2017 (https://doi.org/10.1175/jcli-d-16-0613.1)
!                                                                      
! The class can be used as-is but is also intended as an example of how to extend the RTE framework
! -------------------------------------------------------------------------------------------------

module mo_aerosol_optics_rrtmgp_merra
  use mo_rte_kind,      only: wp, wl
  use mo_rte_config,    only: check_extents, check_values
  use mo_rte_util_array_validation,& 
                        only: extents_are, any_vals_outside
  use mo_optical_props, only: ty_optical_props,      &
                              ty_optical_props_arry, &
                              ty_optical_props_1scl, &
                              ty_optical_props_2str, &
                              ty_optical_props_nstr
  implicit none

  ! MERRA2/GOCART aerosol types
  integer, parameter, public :: merra_ntype = 7          ! Number of MERRA aerosol types
  integer, parameter, public :: merra_aero_none = 0      ! no aerosal
  integer, parameter, public :: merra_aero_dust = 1      ! dust
  integer, parameter, public :: merra_aero_salt = 2      ! Salt
  integer, parameter, public :: merra_aero_sulf = 3      ! sulfate
  integer, parameter, public :: merra_aero_bcar_rh = 4   ! black carbon, hydrophilic
  integer, parameter, public :: merra_aero_bcar = 5      ! black carbon, hydrophobic
  integer, parameter, public :: merra_aero_ocar_rh = 6   ! organic carbon, hydrophilic
  integer, parameter, public :: merra_aero_ocar = 7      ! organic carbon, hydrophobic

  ! index identifiers for aerosol optical property tables
  integer, parameter, private :: ext = 1                 ! extinction
  integer, parameter, private :: ssa = 2                 ! single scattering albedo
  integer, parameter, private :: g   = 3                 ! asymmetry parameter

  private
  ! -----------------------------------------------------------------------------------
  type, extends(ty_optical_props), public :: ty_aerosol_optics_rrtmgp_merra
    private
    !
    ! Lookup table information
    !
    ! Table upper and lower aerosol size (radius) bin limits (microns)
    real(wp),dimension(:,:), allocatable :: merra_aero_bin_lims     ! Dimensions (pair,nbin)
    ! Table relative humidity values
    real(wp),dimension(:),   allocatable :: aero_rh(:)
    !
    ! The aerosol tables themselves.
    ! extinction (m2/kg)
    ! single scattering albedo (unitless)
    ! asymmetry parameter (unitless)
    !
    real(wp), dimension(:,:,:  ), allocatable :: aero_dust_tbl      ! ext, ssa, g (nval, nbin, nbnd)
    real(wp), dimension(:,:,:,:), allocatable :: aero_salt_tbl      ! ext, ssa, g (nval, nrh, nbin, nbnd)
    real(wp), dimension(:,:,:  ), allocatable :: aero_sulf_tbl      ! ext, ssa, g (nval, nrh, nbnd)
    real(wp), dimension(:,:    ), allocatable :: aero_bcar_tbl      ! ext, ssa, g (nval, nbnd)
    real(wp), dimension(:,:,:  ), allocatable :: aero_bcar_rh_tbl   ! ext, ssa, g (nval, nrh, nbnd)
    real(wp), dimension(:,:    ), allocatable :: aero_ocar_tbl      ! ext, ssa, g (nval, nbnd)
    real(wp), dimension(:,:,:  ), allocatable :: aero_ocar_rh_tbl   ! ext, ssa, g (nval, nrh, nbnd)
    !
    ! -----
  contains
    generic,   public  :: load  => load_lut
    procedure, public  :: finalize
    procedure, public  :: aerosol_optics
    ! Internal procedures
    procedure, private :: load_lut
  end type ty_aerosol_optics_rrtmgp_merra

contains
  ! ------------------------------------------------------------------------------
  !
  ! Routines to load data needed for aerosol optics calculations from lookup-tables.
  !
  ! ------------------------------------------------------------------------------
  function load_lut(this, band_lims_wvn, &
                    merra_aero_bin_lims, aero_rh, &
                    aero_dust_tbl, aero_salt_tbl, aero_sulf_tbl, &
                    aero_bcar_tbl, aero_bcar_rh_tbl, &
                    aero_ocar_tbl, aero_ocar_rh_tbl) &
                    result(error_msg)

    class(ty_aerosol_optics_rrtmgp_merra),   & 
                                intent(inout) :: this
    real(wp), dimension(:,:),   intent(in   ) :: band_lims_wvn ! spectral discretization
    ! Lookup table interpolation constants
    real(wp), dimension(:,:),   intent(in   ) :: merra_aero_bin_lims ! aerosol lut size bin limiits (pair,nbin)
    real(wp), dimension(:),     intent(in   ) :: aero_rh       ! relative humidity LUT dimension values
    ! LUT coefficients
    ! Extinction, single-scattering albedo, and asymmetry parameter for aerosol types
    real(wp), dimension(:,:,:),   intent(in)  :: aero_dust_tbl
    real(wp), dimension(:,:,:,:), intent(in)  :: aero_salt_tbl
    real(wp), dimension(:,:,:),   intent(in)  :: aero_sulf_tbl
    real(wp), dimension(:,:),     intent(in)  :: aero_bcar_tbl
    real(wp), dimension(:,:,:),   intent(in)  :: aero_bcar_rh_tbl
    real(wp), dimension(:,:),     intent(in)  :: aero_ocar_tbl
    real(wp), dimension(:,:,:),   intent(in)  :: aero_ocar_rh_tbl
    character(len=128)    :: error_msg
    ! -------
    !
    ! Local variables
    !
    integer               :: npair, nval, nrh, nbin, nband

    error_msg = this%init(band_lims_wvn, name="RRTMGP aerosol optics")
    !
    ! LUT coefficient dimensions
    !
    npair = size(merra_aero_bin_lims,dim=1)
    nval  = size(aero_salt_tbl,dim=1)
    nrh   = size(aero_salt_tbl,dim=2)
    nbin  = size(aero_salt_tbl,dim=3)
    nband = size(aero_salt_tbl,dim=4)
    !
    ! Error checking
    !
    if (check_extents) then
      error_msg = ''
      if(.not. extents_are(aero_dust_tbl, nval, nbin, nband)) &
        error_msg = "aerosol_optics%load_lut(): array aero_dust_tbl isn't consistently sized"
      if(.not. extents_are(aero_salt_tbl, nval, nrh, nbin, nband)) &
        error_msg = "aerosol_optics%load_lut(): array aero_salt_tbl isn't consistently sized"
      if(.not. extents_are(aero_sulf_tbl, nval, nrh, nband)) &
        error_msg = "aerosol_optics%load_lut(): array aero_sulf_tbl isn't consistently sized"
      if(.not. extents_are(aero_bcar_rh_tbl, nval, nrh, nband)) &
        error_msg = "aerosol_optics%load_lut(): array aero_bcar_rh_tbl isn't consistently sized"
      if(.not. extents_are(aero_bcar_tbl, nval, nband)) &
        error_msg = "aerosol_optics%load_lut(): array aero_bcar_tbl isn't consistently sized"
      if(.not. extents_are(aero_ocar_rh_tbl, nval, nrh, nband)) &
        error_msg = "aerosol_optics%load_lut(): array aero_ocar_rh_tbl isn't consistently sized"
      if(.not. extents_are(aero_ocar_tbl, nval, nband)) &
        error_msg = "aerosol_optics%load_lut(): array aero_ocar_tbl isn't consistently sized"
      if(error_msg /= "") return
    endif

    ! Allocate LUT parameters
    allocate(this%merra_aero_bin_lims(npair,nbin))
    allocate(this%aero_rh(nrh))
    ! Allocate LUT coefficients
    allocate(this%aero_dust_tbl(nval, nbin, nband), &
             this%aero_salt_tbl(nrh, nval, nbin, nband), &
             this%aero_sulf_tbl(nrh, nval, nband), &
             this%aero_bcar_tbl(nval, nband), &
             this%aero_bcar_rh_tbl(nrh, nval, nband), &
             this%aero_ocar_tbl(nval, nband), &
             this%aero_ocar_rh_tbl(nrh, nval, nband))
    
    ! Copy LUT coefficients
    this%merra_aero_bin_lims = merra_aero_bin_lims
    this%aero_rh             = aero_rh
    this%aero_dust_tbl = aero_dust_tbl
    this%aero_bcar_tbl = aero_bcar_tbl
    this%aero_ocar_tbl = aero_ocar_tbl

    this%aero_salt_tbl    = reshape( aero_salt_tbl,    shape=(/nrh, nval, nbin, nband/), order=(/2,1,3,4/) )
    this%aero_sulf_tbl    = reshape( aero_sulf_tbl,    shape=(/nrh, nval,       nband/), order=(/2,1,3/) )
    this%aero_bcar_rh_tbl = reshape( aero_bcar_rh_tbl, shape=(/nrh, nval,       nband/), order=(/2,1,3/) )
    this%aero_ocar_rh_tbl = reshape( aero_ocar_rh_tbl, shape=(/nrh, nval,       nband/), order=(/2,1,3/) )

    !$acc enter data create(this)                                               &
    !$acc            copyin(this%aero_dust_tbl, this%aero_salt_tbl, this%aero_sulf_tbl)  &
    !$acc            copyin(this%aero_bcar_tbl, this%aero_bcar_rh_tbl)          &
    !$acc            copyin(this%aero_ocar_tbl, this%aero_ocar_rh_tbl)          &
    !$acc            copyin(this%merra_aero_bin_lims, this%aero_rh)
    !$omp target enter data &
    !$omp map(to:this%aero_dust_tbl, this%aero_salt_tbl, this%aero_sulf_tbl) &
    !$omp map(to:this%aero_bcar_tbl, this%aero_bcar_rh_tbl)                  &
    !$omp map(to:this%aero_ocar_tbl, this%aero_ocar_rh_tbl)                  &
    !$omp map(to:this%merra_aero_bin_lims, this%aero_rh)

  end function load_lut
  !--------------------------------------------------------------------------------------------------------------------
  !
  ! Finalize
  !
  !--------------------------------------------------------------------------------------------------------------------
  subroutine finalize(this)
    class(ty_aerosol_optics_rrtmgp_merra), intent(inout) :: this

    ! Lookup table aerosol optics interpolation arrays
    if(allocated(this%merra_aero_bin_lims)) then
      deallocate(this%merra_aero_bin_lims, this%aero_rh)
      !$acc        exit data delete(     this%merra_aero_bin_lims, this%aero_rh) 
      !$omp target exit data map(release:this%merra_aero_bin_lims, this%aero_rh)
    end if

    ! Lookup table aerosol optics coefficients
    if(allocated(this%aero_dust_tbl)) then
      !$acc exit data delete(this%aero_dust_tbl, this%aero_salt_tbl, this%aero_sulf_tbl)  &
      !$acc           delete(this%aero_bcar_tbl, this%aero_bcar_rh_tbl) &
      !$acc           delete(this%aero_ocar_tbl, this%aero_ocar_rh_tbl) &
      !$acc           delete(this)
      !$omp target exit data map(release:this%aero_dust_tbl, this%aero_salt_tbl, this%aero_sulf_tbl) &
      !$omp                  map(release:this%aero_bcar_tbl, this%aero_bcar_rh_tbl)                  & 
      !$omp                  map(release:this%aero_ocar_tbl, this%aero_ocar_rh_tbl) 
      deallocate(this%aero_dust_tbl, this%aero_salt_tbl, this%aero_sulf_tbl, &
                 this%aero_bcar_tbl, this%aero_bcar_rh_tbl, &
                 this%aero_ocar_tbl, this%aero_ocar_rh_tbl)
    end if

  end subroutine finalize
  ! ------------------------------------------------------------------------------
  !
  ! Derive aerosol optical properties from provided aerosol input properties
  !
  ! ------------------------------------------------------------------------------
  !
  ! Compute single-scattering properties
  !
  function aerosol_optics(this, aero_type, aero_size, aero_mass, relhum, &
                          optical_props) result(error_msg)

    class(ty_aerosol_optics_rrtmgp_merra), &
              intent(in  ) :: this
    integer,  intent(in  ) :: aero_type(:,:)   ! MERRA2/GOCART aerosol type 
                                               ! Dimensions: (ncol,nlay)
                                               ! 1 = merra_aero_dust    (dust)
                                               ! 2 = merra_aero_salt    (salt)
                                               ! 3 = merra_aero_sulf    (sulfate)
                                               ! 4 = merra_aero_bcar_rh (black carbon, hydrophilic)
                                               ! 5 = merra_aero_bcar    (black carbon, hydrophobic)
                                               ! 6 = merra_aero_ocar_rh (organic carbon, hydrophilic)
                                               ! 7 = merra_aero_ocar    (organic carbon, hydrophobic)
    real(wp), intent(in  ) :: aero_size(:,:)   ! aerosol size (radius) for dust and sea-salt (microns)
                                               ! Dimensions: (ncol,nlay)
    real(wp), intent(in  ) :: aero_mass(:,:)   ! aerosol mass column (kg/m2)
                                               ! Dimensions: (ncol,nlay)
    real(wp), intent(in  ) :: relhum(:,:)      ! relative humidity (fraction, 0-1)
                                               ! Dimensions: (ncol,nlay)

    class(ty_optical_props_arry), &
              intent(inout) :: optical_props
                                               ! Dimensions: (ncol,nlay,nbnd)

    character(len=128)      :: error_msg
    ! ------- Local -------
    logical(wl), dimension(size(aero_type,1), size(aero_type,2)) :: aeromsk
    real(wp),    dimension(size(aero_type,1), size(aero_type,2), size(this%aero_dust_tbl,3)) :: &
                 atau, ataussa, ataussag
    integer  :: ncol, nlay, npair, nbin, nrh, nval, nbnd
    integer  :: icol, ilay, ibnd, ibin
    ! scalars for total tau, tau*ssa
    real(wp) :: tau, taussa
    ! Scalars to work around OpenACC/OMP issues
    real(wp) :: minSize,  maxSize

    ! ----------------------------------------
    !
    ! Error checking
    !
    ! ----------------------------------------
    error_msg = ''
    if(.not.(allocated(this%aero_dust_tbl))) then
      error_msg = 'aerosol optics: no data has been initialized'
      return
    end if

    ncol = size(aero_type,1)
    nlay = size(aero_type,2)
    npair= size(this%merra_aero_bin_lims,1)
    nbin = size(this%merra_aero_bin_lims,2)
    nrh  = size(this%aero_rh,1)
    nval = size(this%aero_dust_tbl,1)
    nbnd = size(this%aero_dust_tbl,3)

    !$acc        update host(this%merra_aero_bin_lims)
    !$omp target update from(this%merra_aero_bin_lims)
    minSize = this%merra_aero_bin_lims(1,1)
    maxSize = this%merra_aero_bin_lims(2,nbin)

    !
    ! Array sizes
    !
    if (check_extents) then
      error_msg = ''
      if(.not. extents_are(aero_type, ncol, nlay)) &
        error_msg = "aerosol optics: aero_type isn't consistenly sized"
      if(.not. extents_are(aero_size, ncol, nlay)) &
        error_msg = "aerosol optics: aero_size isn't consistenly sized"
      if(.not. extents_are(aero_mass, ncol, nlay)) &
        error_msg = "aerosol optics: aero_mass isn't consistenly sized"
      if(.not. extents_are(relhum, ncol, nlay)) &
        error_msg = "aerosol optics: relhum isn't consistenly sized"
      if(optical_props%get_ncol() /= ncol .or. optical_props%get_nlay() /= nlay) &
        error_msg = "aerosol optics: optical_props have wrong extents"
      if(error_msg /= "") return
    end if

    !
    ! Spectral consistency
    !
    if(check_values) then
      if(.not. this%bands_are_equal(optical_props)) &
        error_msg = "aerosol optics: optical properties don't have the same band structure"
      if(optical_props%get_nband() /= optical_props%get_ngpt() ) &
        error_msg = "aerosol optics: optical properties must be requested by band not g-points"
      if(any_int_vals_outside_2D(aero_type, merra_aero_none, merra_ntype)) &
        error_msg = 'aerosol optics: aerosol type is out of bounds'
      if(error_msg /= "") return
    end if

    !$acc data        copyin(aero_type, aero_size, aero_mass, relhum) 
    !$omp target data map(to:aero_type, aero_size, aero_mass, relhum) 
    !
    ! Aerosol mask; don't need aerosol optics if there's no aerosol
    !
    !$acc data           create(aeromsk)
    !$omp target data map(alloc:aeromsk) 
    !$acc              parallel loop default(present) collapse(2)
    !$omp target teams distribute parallel do simd collapse(2)
    do ilay = 1, nlay
      do icol = 1, ncol
        aeromsk(icol,ilay) = aero_type(icol,ilay) > 0
      end do
    end do

    !
    ! Aerosol size, relative humidity
    !
    if(check_values) then
      if(any_vals_outside(aero_size, aeromsk, minSize, maxSize)) &
        error_msg = 'aerosol optics: requested aerosol size is out of bounds'
      if(any_vals_outside(relhum,    aeromsk, 0._wp, 1._wp)) &
        error_msg = 'aerosol optics: relative humidity fraction is out of bounds'
    end if
    ! Release aerosol mask 
    !$acc end data
    !$omp end target data 

    if(error_msg == "") then
      !$acc data           create(atau, ataussa, ataussag)
      !$omp target data map(alloc:atau, ataussa, ataussag) 
      !
      !
      ! ----------------------------------------
      !
      ! The lookup tables determining extinction coefficient, single-scattering albedo,
      !   and asymmetry parameter g as a function of aerosol type, aerosol size and
      !   relative humidity.
      ! We compute the optical depth tau (= exintinction coeff * aerosol mass ) and the
      !    products tau*ssa and tau*ssa*g separately for each aerosol type requested.
      ! These are used to determine the aerosol optical properties.
      !
      if (allocated(this%aero_dust_tbl)) then
        !
        ! Aerosol
        !
        call compute_all_from_table(ncol, nlay, npair, nval, nrh, nbin, nbnd,                   &
                                    aero_type, aero_size, aero_mass, relhum,                    &
                                    this%merra_aero_bin_lims, this%aero_rh,                     &
                                    this%aero_dust_tbl, this%aero_salt_tbl, this%aero_sulf_tbl, &
                                    this%aero_bcar_rh_tbl, this%aero_bcar_tbl,                  &
                                    this%aero_ocar_rh_tbl, this%aero_ocar_tbl,                  &
                                    atau, ataussa, ataussag)

      endif

      !
      ! Derive total aerosol optical properties
      !   See also the increment routines in mo_optical_props_kernels
      !
      select type(optical_props)
      type is (ty_optical_props_1scl)
        !$acc parallel loop gang vector default(present) collapse(3) &
        !$acc               copyin(optical_props) copyout(optical_props%tau)
        !$omp target teams distribute parallel do simd collapse(3) &
        !$omp map(from:optical_props%tau)
        do ibnd = 1, nbnd
          do ilay = 1, nlay
            do icol = 1, ncol
              ! Absorption optical depth  = (1-ssa) * tau = tau - taussa
              optical_props%tau(icol,ilay,ibnd) = (atau(icol,ilay,ibnd) - ataussa(icol,ilay,ibnd))
            end do
          end do
        end do
      type is (ty_optical_props_2str)
        !$acc parallel loop gang vector default(present) collapse(3) &
        !$acc               copyin(optical_props) copyout(optical_props%tau, optical_props%ssa, optical_props%g)
        !$omp target teams distribute parallel do simd collapse(3) &
        !$omp map(from:optical_props%tau, optical_props%ssa, optical_props%g)
        do ibnd = 1, nbnd
          do ilay = 1, nlay
            do icol = 1, ncol
              tau    = atau   (icol,ilay,ibnd)
              taussa = ataussa(icol,ilay,ibnd)
              optical_props%tau(icol,ilay,ibnd) = tau
              optical_props%ssa(icol,ilay,ibnd) = taussa / max(epsilon(tau), tau)
              optical_props%g  (icol,ilay,ibnd) = (ataussag(icol,ilay,ibnd)) &
                                                         / max(epsilon(tau), taussa)
            end do
          end do
        end do
      type is (ty_optical_props_nstr)
        error_msg = "aerosol optics: n-stream calculations not yet supported"
      end select
      !$acc end data
      !$omp end target data
    end if 
    !$acc end data
    !$omp end target data 
  end function aerosol_optics
  !--------------------------------------------------------------------------------------------------------------------
  !
  ! Ancillary functions
  !
  !--------------------------------------------------------------------------------------------------------------------
  !
  ! For size dimension, select size bin appropriate for the requested aerosol size.
  ! For rh dimension, linearly interpolate values from a lookup table with "nrh" 
  !   unevenly-spaced elements "aero_rh". The last dimension for all tables is band.
  ! Returns zero where no aerosol is present.
  !
  subroutine compute_all_from_table(ncol, nlay, npair, nval, nrh, nbin, nbnd,          &
                                    type, size, mass, rh,                              &
                                    merra_aero_bin_lims, aero_rh,                      &
                                    aero_dust_tbl, aero_salt_tbl, aero_sulf_tbl,       &
                                    aero_bcar_rh_tbl, aero_bcar_tbl,                   &
                                    aero_ocar_rh_tbl, aero_ocar_tbl,                   &
                                    tau, taussa, taussag)

    integer,                               intent(in) :: ncol, nlay, npair, nval, nrh, nbin, nbnd
    integer,     dimension(ncol,nlay),     intent(in) :: type
    real(wp),    dimension(ncol,nlay),     intent(in) :: size, mass, rh

    real(wp),    dimension(npair,nbin),    intent(in) :: merra_aero_bin_lims
    real(wp),    dimension(nrh),           intent(in) :: aero_rh

    real(wp),    dimension(nval,    nbin,nbnd), intent(in) :: aero_dust_tbl
    real(wp),    dimension(nrh,nval,nbin,nbnd), intent(in) :: aero_salt_tbl
    real(wp),    dimension(nrh,nval,     nbnd), intent(in) :: aero_sulf_tbl
    real(wp),    dimension(nrh,nval,     nbnd), intent(in) :: aero_bcar_rh_tbl
    real(wp),    dimension(nval,         nbnd), intent(in) :: aero_bcar_tbl
    real(wp),    dimension(nrh,nval,     nbnd), intent(in) :: aero_ocar_rh_tbl
    real(wp),    dimension(nval,         nbnd), intent(in) :: aero_ocar_tbl

    real(wp),    dimension(ncol,nlay,nbnd), intent(out) :: tau, taussa, taussag
    ! ---------------------------
    integer  :: icol, ilay, ibnd, ibin, i
    integer  :: itype, irh1, irh2
    real(wp) :: drh0, drh1, rdrh
    real(wp) :: t, ts, tsg  ! tau, tau*ssa, tau*ssa*g
    ! ---------------------------
    !$acc parallel loop gang vector default(present) collapse(3)
    !$omp target teams distribute parallel do simd collapse(3)
    do ibnd = 1, nbnd
      do ilay = 1,nlay
        do icol = 1, ncol
          ! Sequential loop to find size bin
          do i=1,nbin 
             if (size(icol,ilay) .ge. merra_aero_bin_lims(1,i) .and. &
                 size(icol,ilay) .le. merra_aero_bin_lims(2,i)) then
                ibin = i
             endif
          enddo
          itype = type(icol,ilay)
          ! relative humidity linear interpolation coefficients
          if (itype .ne. merra_aero_none) then
             irh2 = 1
             do while (rh(icol,ilay) .gt. aero_rh(irh2))
                irh2 = irh2 + 1
                if (irh2 .gt. nrh) exit
             enddo
             irh1 = max(1, irh2-1)
             irh2 = min(nrh, irh2)
             drh0 = aero_rh(irh2) - aero_rh(irh1)
             drh1 = rh(icol,ilay) - aero_rh(irh1)
             if (irh1 == irh2) then
                rdrh = 0._wp
             else
                rdrh = drh1 / drh0
             endif
          endif

          ! Set aerosol optical properties where aerosol present. Use aerosol type array as the mask. 
          select case (itype)

             ! dust
             case(merra_aero_dust)
               tau    (icol,ilay,ibnd) = mass  (icol,ilay)      * aero_dust_tbl(ext,ibin,ibnd)
               taussa (icol,ilay,ibnd) = tau   (icol,ilay,ibnd) * aero_dust_tbl(ssa,ibin,ibnd)
               taussag(icol,ilay,ibnd) = taussa(icol,ilay,ibnd) * aero_dust_tbl(g,ibin,ibnd)
             ! sea-salt
             case(merra_aero_salt)
               tau    (icol,ilay,ibnd) = mass  (icol,ilay) * &
                                         linear_interp_aero_table(aero_salt_tbl(:,ext,ibin,ibnd),irh1,irh2,rdrh)
               taussa (icol,ilay,ibnd) = tau   (icol,ilay,ibnd) * &
                                         linear_interp_aero_table(aero_salt_tbl(:,ssa,ibin,ibnd),irh1,irh2,rdrh)
               taussag(icol,ilay,ibnd) = taussa(icol,ilay,ibnd) * &
                                         linear_interp_aero_table(aero_salt_tbl(:,g,  ibin,ibnd),irh1,irh2,rdrh)

             ! sulfate
             case(merra_aero_sulf)
               tau    (icol,ilay,ibnd) = mass  (icol,ilay) * &
                                         linear_interp_aero_table(aero_sulf_tbl(:,ext,ibnd),irh1,irh2,rdrh)
               taussa (icol,ilay,ibnd) = tau   (icol,ilay,ibnd) * &
                                         linear_interp_aero_table(aero_sulf_tbl(:,ssa,ibnd),irh1,irh2,rdrh)
               taussag(icol,ilay,ibnd) = taussa(icol,ilay,ibnd) * &
                                         linear_interp_aero_table(aero_sulf_tbl(:,g,  ibnd),irh1,irh2,rdrh)
             ! black carbon - hydrophilic
             case(merra_aero_bcar_rh)
               tau    (icol,ilay,ibnd) = mass  (icol,ilay) * &
                                         linear_interp_aero_table(aero_bcar_rh_tbl(:,ext,ibnd),irh1,irh2,rdrh)
               taussa (icol,ilay,ibnd) = tau   (icol,ilay,ibnd) * &
                                         linear_interp_aero_table(aero_bcar_rh_tbl(:,ssa,ibnd),irh1,irh2,rdrh)
               taussag(icol,ilay,ibnd) = taussa(icol,ilay,ibnd) * &
                                         linear_interp_aero_table(aero_bcar_rh_tbl(:,g,  ibnd),irh1,irh2,rdrh)
             ! black carbon - hydrophobic
             case(merra_aero_bcar)
               tau    (icol,ilay,ibnd) = mass  (icol,ilay)      * aero_bcar_tbl(ext,ibnd)
               taussa (icol,ilay,ibnd) = tau   (icol,ilay,ibnd) * aero_bcar_tbl(ssa,ibnd)
               taussag(icol,ilay,ibnd) = taussa(icol,ilay,ibnd) * aero_bcar_tbl(g,  ibnd)
             ! organic carbon - hydrophilic
             case(merra_aero_ocar_rh)
               tau    (icol,ilay,ibnd) = mass  (icol,ilay) * &
                                         linear_interp_aero_table(aero_ocar_rh_tbl(:,ext,ibnd),irh1,irh2,rdrh)
               taussa (icol,ilay,ibnd) = tau   (icol,ilay,ibnd) * &
                                         linear_interp_aero_table(aero_ocar_rh_tbl(:,ssa,ibnd),irh1,irh2,rdrh)
               taussag(icol,ilay,ibnd) = taussa(icol,ilay,ibnd) * &
                                         linear_interp_aero_table(aero_ocar_rh_tbl(:,g,  ibnd),irh1,irh2,rdrh)
             ! organic carbon - hydrophobic
             case(merra_aero_ocar)
               tau    (icol,ilay,ibnd) = mass  (icol,ilay)      * aero_ocar_tbl(ext,ibnd)
               taussa (icol,ilay,ibnd) = tau   (icol,ilay,ibnd) * aero_ocar_tbl(ssa,ibnd)
               taussag(icol,ilay,ibnd) = taussa(icol,ilay,ibnd) * aero_ocar_tbl(g,  ibnd)
             ! no aerosol
             case default
               tau    (icol,ilay,ibnd) = 0._wp
               taussa (icol,ilay,ibnd) = 0._wp
               taussag(icol,ilay,ibnd) = 0._wp

          end select

        end do
      end do
    end do
  end subroutine compute_all_from_table
  !--------------------------------------------------------------------------------------------------------------------
  !
  ! Function for linearly interpolating MERRA aerosol optics tables in the rh dimension for
  ! a single parameter, aerosol type, spectral band, and size bin. Interpolation is performed
  ! only where aerosol in present using aerosol type as the mask. 
  !
  function linear_interp_aero_table(table, index1, index2, weight) result(value)
    !$acc routine seq
    !$omp declare target

    integer,                intent(in) :: index1, index2
    real(wp),               intent(in) :: weight
    real(wp), dimension(:), intent(in) :: table

    real(wp) :: value

    value = table(index1) + weight * (table(index2) - table(index1))
     
  end function linear_interp_aero_table
! ----------------------------------------------------------
  logical function any_int_vals_outside_2D(array, checkMin, checkMax)
    integer, dimension(:,:), intent(in) :: array
    integer,                 intent(in) :: checkMin, checkMax

    integer :: minValue, maxValue

    !$acc kernels copyin(array)
    !$omp target map(to:array) map(from:minValue, maxValue)
    minValue = minval(array)
    maxValue = maxval(array)
    !$acc end kernels
    !$omp end target
    any_int_vals_outside_2D = minValue < checkMin .or. maxValue > checkMax

  end function any_int_vals_outside_2D

end module mo_aerosol_optics_rrtmgp_merra
