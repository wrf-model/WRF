!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
! ------------------ Goddard Radiation Code   ---------------------------------
!
! Comments : 
!   This radiation scheme is originally developed for NASA Global Modeling (GEOS)
!   by Ming-Dah Chou and Max Suarez. When the radiation code was incorporated into 
!   mesoscale model, Toshi Matsui optimized the code (1D driver) for high-resolution modeling, 
!   and eventually single-scattering properties of clouds and aerosols throuth 
!   the NASA-Unified WRF project. Radiation code update radiative heating rate and 
!   surface and TOA radiation fluxes. 
!
!   (Note module_ra_gsfcsw.F is similar SW version of goddard scheme, but it is
!    OLD version so that some of gas molecular absorptions are missing or
!    does not account for effect of solar zenith angle.)
!
!
! Method (Calling sequence) :
!
! init_goddardrad( initialize LUT )
!     !-- read_lut_nc (read single-scattering LUT in netcdf format )
!
!
! goddardrad(preparation for radiative transfer)
!     !-- sounding_interp (interpolate sounding climatology)
!     |-- ozone_interp (intepolate ozone profile)
!     |-- opt_cloud( derive cloud optiacl properties )
!     |-- aero_opt( derive aerosol optiacl properties ) from module_gocart_coupling.F
!     |-- swrad(compute SW radiative transfer)
!     |     |-- sw_uvpar(compute SW radiative transfer for UV band)
!     |     |     |-cloud_scale(scale cloud optical thickness for diffuse/direct radiation)
!     |     |     |-delta_eddington(delta-eddington approximation for the bulk scattering properties)
!     |     |     |-twostream_adding(compute updown fluxes using two-stream adding method)
!     |     |-- sw_ir(compute SW radiative transfer for IR band)
!     |     |     |-cloud_scale(scale cloud optical thickness for diffuse/direct radiation)
!     |     |     |-delta_eddington(delta-eddington approximation for the bulk scattering properties)
!     |     |     |-twostream_adding(compute updown fluxes using two-stream adding method)
!     |     |-- reduce_flux(compute the reduction of clear-sky downward solar flux)
!     |
!     |-- lwrad(compute LW radiative transfer)
!           |-- column(compute column-integrated amount of absorber)
!           |-- h2oexps(compute exponentials for water vapor line absorption)
!           |-- conexps(compute exponentials for continuum absorption)
!           |-- co2exps(compute co2 exponentials)
!           |-- ch4exps(compute ch4 exponentials)
!           |-- comexps(compute co2-minor exponentials)
!           |-- cfcexps(compute cfc exponentials)
!           |-- b10exps(compute band3a exponentials)
!           |-- tablup(compute water vapor, co2 and o3 transmittances)
!           |-- h2okdis(compute water vapor transmittance using kdistirubion method)
!           |-- co2kdis(compute co2 transmittances using k-distribution method)
!           |-- n2okdis(compute n2o transmittances using k-distribution method)
!           |-- ch4kdis(compute ch4 transmittances using k-distribution method)
!           |-- comkdis(compute co2-minor transmittances using k-distribution method)
!           |-- cfckdis(compute cfc transmittances using k-distribution method)
!           |-- b10kdis(compute band3a transmittances using k-distribution method)
!           |-- cldovlp(compute the fractional clear line-of-sight)
!
!
! History :
!   Nov  2017    , Toshi Matsui : scattering LUT is unified for single NetCDF file. 
!   Nov  2017    , M.D. Chou    : Upgraded gas absorption from HITRAN 1996 to 2012. 
!   Oct  2016    , Jarno Mielikainen : Additional code optimization by adding vectorization (x1.17 for SW, x 1.7 for LW)
!   July 2015    , Toshi Matsui : mcdat_int is io parameter now for running hybrid MPI/OMP
!   Feb  2014    , Toshi Matsui : Optimization of new single scattering routine is done. 
!                                Also modified new IO parameters (re3d, aod, and cod). 
!                                Added additional SW flux paarameters for WRF v3.5.1 radiation driver.
!   Aug  2013    , Toshi Matsui : Completely revisied single-scattering tables that is now 
!                                identical to G-SDSU (satellite simulator). For this change, 
!                                original hydrometero single-scattering values are removed, and now 
!                                WRF must call init_goddardrad for initializing single-scattering LUT. 
!                                So I removed cwc and reff inputs for swrad/lwrad subroutine. 
!                                So far this only apply to cloud species, and aerosol are yet testing. 
!   Feb  2011    , Toshi Matsui : New optimization (skip nighttime completely for SW routine).
!                                Now wrapper routine are single precision, while
!                                routine under swrad and lwrad are double precision.
!                                Stratosphere option was removed.
!   Feb  2011    , Toshi Matsui&Roger Shi : Slight modification for online-gocart coupling. 
!   Oct  2008    , Toshi Matsui : All real parameters becomes double precision for stability. 
!   Jun  2008    , Toshi Matsui : Sounding interp is called only for each j_loop
!   Jun  2008    , Toshi Matsui : Vector loop (m) was completely removed, now radiation code is 
!                                compltely 1dimensionalized (-> 30~40% speed up with O3 optimization)
!   Apr  2008    , Toshi Matsui : Add stratosphere layrs options 
!   Aug  2007    , Toshi Matsui : One-dimensinalized swrad to skip swrad for nighttime.
!   Jun  2007    , Toshi Matsui ; Revise broadband flux reduction to avoid negative SW heating.
!   May  2007    , Toshi Matsui ; Made driver to plug WRF, GOCCART module, and F90 revision.
!   Apr  2007    , Toshi Matsui ; SW radiation routine is optimized (add fast_overcast option for CRM).
!      ~ 2002    , Ming-Dah Chou and Max Suarez ; initial development. 
!
!
! References (Radiation Physics Scheme):
!  Chou M.-D., and M. J. Suarez, 2001: A thermal infrared radiation parameterization for atmospheric
!      studies. NASA/TM-2001-104606, vol. 19, 55pp
!  Chou M.-D., and M. J. Suarez, 1999: A solar radiation parameterization for atmospheric studies. 
!      NASA Tech. Rep. NASA/TM-1999-10460, vol. 15, 38 pp
!
!
! References (Application): 
! Matsui, T., S. Q. Zhang, W.-K. Tao, S. Lang, C. Ichoku, and C. Peters-Lidard (2018), 
!      Impact of Radiation Frequency, Precipitation Radiative Forcing, and Radiation Column Aggregation 
!      on Convection-Permitting West African Monsoon Simulations, 
!      Climate Dynamics, 1-21, https://doi.org/10.1007/s00382-018-4187-2
! Shi, J. J., T. Matsui, W.-K. Tao, C. Peters-Lidard, M. Chin1, Q. Tan, and E. Kemp (2014), 
!      Implementation of an Aerosol-Cloud Microphysics-Radiation Coupling into the NASA Unified WRF:  
!      Simulation Results for the 6-7 August 2006 AMMA Special Observing Period, Quarterly Journal 
!      of the Royal Meteorological Society. doi: 10.1002/qj.2286
! Ma, P.-L., K. Zhang, J.-J. Shi, T. Matsui, A. Arking, (2012): Direct Radiative Effect of Mineral Dust 
!     on the Development of African Easterly Wave in Late Summer, 2003-2007, Journal of Applied Meteorology 
!     and Climatology, 51, 2090-2104.
!
! Shi, J. J., W.-K. Tao, T. Matsui, A. Hou, S. Lang, C. Peters-Lidard, G. Jackson, R. Cifelli, S. Rutledge,
!     and W. Petersen (2010), Microphysical Properties of the January 20-22 2007 Snow Events over Canada: 
!     Comparison with in-situ and Satellite Observations. Journal of Applied Meteorology and Climatology. 
!     49(11), 2246-2266
!
! Bug report: 
!      If you find any bug of Goddard Radiation in WRF, please report to
!      Toshi Matsui @ NASA GSFC, email: Toshihisa.Matsui-1@nasa.gov
!
!---------------------------------------------------------------------------------------------------------
 module module_ra_goddard
 use module_wrf_error
 use module_checkerror
#if (WRF_CHEM == 1)
 use module_gocart_coupling
#endif

 implicit none

  INTEGER, PARAMETER, PRIVATE:: chunk = 16  !size of vector (16 is optimized number for Haswell/Broadwell CPUs.

!
! encapsulation control
!
  private  !-> privatize all variables in this module excepting public parameter (subroutine) below. 
  public :: init_goddardrad, goddardrad  !only this subroutine can be called from other module

!integer,parameter :: fp_kind = SELECTED_REAL_KIND( 6, 37)  !single precision precision kind
 integer,parameter :: fp_kind = SELECTED_REAL_KIND(15,307)  !double precision precision kind


!
! If one, this logical parameter allow to check anomalous value of input parameters.
!
  logical,parameter :: NUWRF_DEBUG = .false.

!
! hydrometeor single scattering properties. If true (default), optical properties of 
! precipitating species (rain, snow, graupel, hail) are computed. If false, only non-precipitating
! hydrometeor (cloud , ice crystals) are accounted like GCM. 
!
 logical,parameter :: opt_account_precip = .true.


! (SW radition option)
! For overcast (=true) option, this option is always true, which  skip the clear-sky
! two-stream radiative transfer computation, and only compute cloudy ski option.
! Fclear/Fcloud is estimated from pre-computed look-up table for the below cloud
! flux reduction due to CO2 and O2.  So, this option make SW radiation faster 1.5 time.
! Difference in surface downwelling radiation betweeen true and false option is less than 1.W/m^2.
   logical :: fast_overcast  

! (LW radiation option)
!   if trace = .true., absorption due to n2o, ch4, cfcs, and the
!   two minor co2 bands in the LW window region is included.
!   if trace = .false., absorption in those minor bands is neglected.

   logical,parameter :: trace = .true.  !recommend true

   real(Kind=fp_kind),    parameter ::   co2    = 401.30e-6 !336.77e-6  ! co2 concentration [ppv] (Y1850 = 285.43e-6)
   real(Kind=fp_kind),    parameter ::   n2o    = 0.32e-6    ! n2o concentration [ppv] (Y1850 = 0.28e-6)
   real(Kind=fp_kind),    parameter ::   ch4    = 1.79e-6    ! ch4 concentration [ppv] (Y1850 = 0.86e-6)
   real(Kind=fp_kind),    parameter ::   cfc11  = 268.0e-12  ! cfc11 concentration [ppv] (Y1850 = 0.)
   real(Kind=fp_kind),    parameter ::   cfc12  = 503.0e-12  ! cfc12 concentration [ppv] (Y1850 = 0.)
   real(Kind=fp_kind),    parameter ::   cfc22  = 105.0e-12  ! cfc22 concentration [ppv] (Y1850 = 0.)


!  (Both SW and LW radiation option)
!  in a high spatial-resolution atmospheric model, fractional cloud cover
!  might be computed to be either 0 or 1.  in such a case, scaling of the
!  cloud optical thickness is not necessary, and the computation can be
!  made faster by setting overcast=.true.  otherwise, set the option
!  overcast=.false. (-> hardwire with WRF module)

   logical,parameter :: overcast = .true.  ! true for CRM or LES

!
! Radiation scheme here is 1D radiation same as common radiation scheme used in various 
! atmospheric model. In this scheme, when dx become a few km, many 1D assumption will be
! invalid. So, this skip option will aggregate evey NxN horizontal grid to compute averaged
! flux and heating ratio. This N size can be 1(no aggregation), 2(2x2=4grids), or 3(3x3=9grid)
! determined by N = ij_skip = NINT(6000/dx), when iskip (= goddardrad_skip) = 1
! Overall, this option (depending on your dx) will gain ~ 5x speed, suitable for high-res
! weather forecasting or regiona climate model. 
!
   logical :: radiation_skip   ! if on skip option will be activated (fast)

!
!  -LW radiation option-
!  if high = .true., transmission functions in the co2, o3, and the
!  three water vapor bands with strong absorption are computed using
!  table look-up.  cooling rates are computed accurately from the
!  surface up to 0.01 mb. But slightly computationally expensive.
!
!  if high = .false., transmission functions in LW radiation are computed using the
!  k-distribution method with linear pressure scaling for all spectral
!  bands and gases.  cooling rates are not accurately calculated for
!  pressures less than 10 mb. But, the computationally cheap for LW radiation.
!
   ! NUWRF EMK 2 March 2015...Changed to true per instructions from Toshi.
   logical,parameter :: high = .false.    ! about a few W/m2 difference in typical WRF run
                                          ! so just false for a bit faster simulation.


!
! Threshold values
!  - Do not change. These values are the lowest threshold, while avoiding numerical instability.
   real              , parameter :: cosz_min = 0.0001 ! threshold of minimum cos of solar zenith angle for SW rad
   real(Kind=fp_kind), parameter :: fcld_min = 0.01   ! threshold of minimum cloud fraction for account clooud
   real(Kind=fp_kind), parameter :: taux_min = 0.0001 ! threshold of minimum optical depth for accouting cloud (0.02) 
   real(Kind=fp_kind), parameter :: opt_min  = 1.e-6  ! threshold of optical properties to avoid numerical instability. 
   real(Kind=fp_kind), parameter :: ssa_min = 0.e0       !minimamum of single scattering albedo
   real(Kind=fp_kind), parameter :: ssa_max = 0.999999e0 !maximum of single scattering albedo
   real(Kind=fp_kind), parameter :: asy_min = -1.e0      !minimum of asymetry parameter
   real(Kind=fp_kind), parameter :: asy_max =  1.e0      !maximum of asymetry parameter
   real(Kind=fp_kind), parameter :: const_tiny =  tiny(1.)      !maximum of asymetry parameter

   real,parameter :: q_min_condensate = 1.0e-5   ! siginificant minimum hydrometeor mixing ratio [g/m3]
   real,parameter :: q_min_aerosol    = 1.0e-8  ! siginificant minimum aerosol mixing ratio [g/m3]
   real,parameter :: const_pi = 3.14159265e0

!
! New unified single scalttering properties. 
!
 logical,parameter :: new_method_aero = .false.  !defaut is false in march 2014 (Toshi Matsui)


!
! hydrometeor index
!
 integer,parameter :: id_hmax    = 6  ! total number of  hydrometeor species id maxima
 integer,parameter :: id_ice     = 1  
 integer,parameter :: id_cloud   = 2
 integer,parameter :: id_rain    = 3  
 integer,parameter :: id_snow    = 4  
 integer,parameter :: id_graupel = 5  
 integer,parameter :: id_hail    = 6  

!
! generalized 1- or 2-moment 6-class microphysics parameters with generalized gamma distribution 
!
 type particle_gen !gen paticle type
    real:: cloud,rain,ice,snow,graupel,hail
 end type particle_gen

!
! GOCART 1-moment aerosol microphysics parameters
!
 integer,parameter :: id_amax    = 14  ! total number of  hydrometeor species id maxima
 
 type particle_gocart !GOCART paticle type
    real :: so4,blc,ocn,och,ssa,ssc,du1,du2,du3,du4,du5,du6,du7,du8
 end type particle_gocart

 character(len=200) :: sdsu_io_file

!
! Single-scattering look-up tables (final dimension is 1-SW, 2-LW)
!
! NUWRF EMK...Change path to GODDARDRAD_SSLUT to immediate subdirectory, which
! can be symbolic link to actual location. Allows for more flexibility in
! where to run wrf.exe.
 character(len=200),parameter :: dir_lut = './' !directory of single scattering tables are now in running directory. 

  type ( particle_gen ),allocatable,dimension(:,:,:) :: & ! particle_gen is defined in module_simulater 
     kext_lut_gen,  & ! extinction coefficient [km-1 / (g/m3) ]! dimension( mxpts_re_visir, mxband, 2)
     salb_lut_gen,  & ! single scattering albedo [-]
     asym_lut_gen     ! asymetry parameter [-]

  real,allocatable,dimension(:,:,:) :: &  !single scattering properties for solar radiation
     kext_sw_cloud,  & ! extinction coefficient [km-1 / (g/m3) ]
     salb_sw_cloud,  & ! single scattering albedo [-]
     asym_sw_cloud     ! asymetry parameter [-]

  real,allocatable,dimension(:,:,:) :: &  !single scattering properties for thermal radiation
     kext_lw_cloud,  & ! extinction coefficient [km-1 / (g/m3) ]
     salb_lw_cloud,  & ! single scattering albedo [-]
     asym_lw_cloud     ! asymetry parameter [-]


  type ( particle_gocart ),allocatable,dimension(:,:,:) :: & ! particle_gocart is defined in module_simulater 
     kext_lut_gocart,  & ! extinction coefficient [km-1 / (g/m3) ]! dimension( mxpts_re_visir, mxband, 2)
     salb_lut_gocart,  & ! single scattering albedo [-]
     asym_lut_gocart     ! asymetry parameter [-]

  integer,parameter :: nband_sw = 11       !# of SW radiative band
  integer,parameter :: nband_lw = 10      ! # of LW radiation band
  integer,parameter :: mxib     = 2        ! # of separation of broad-band category (2 for goddardrad)

!
! LUT bins for visible-IR and lidar simulators
!
  integer, parameter :: mxpts_re_visir  = 109  ! LUT dimension for effective radius [micron] 
  real, parameter :: pts_re_visir(mxpts_re_visir) = & ! LUT re vector [micron] for frozen hydrometeors
              (/ 1.0e-2, 1.5e-2, 2.0e-2, 2.5e-2, 3.0e-2, 3.5e-2, 4.0e-2, 4.5e-2, 5.0e-2, &
                 5.5e-2, 6.0e-2, 6.5e-2, 7.0e-2, 7.5e-2, 8.0e-2, 8.5e-2, 9.0e-2, 9.5e-2, &
                 1.0e-1, 1.5e-1, 2.0e-1, 2.5e-1, 3.0e-1, 3.5e-1, 4.0e-1, 4.5e-1, 5.0e-1, &
                 5.5e-1, 6.0e-1, 6.5e-1, 7.0e-1, 7.5e-1, 8.0e-1, 8.5e-1, 9.0e-1, 9.5e-1, &
                 1.0e-0, 1.5e-0, 2.0e-0, 2.5e-0, 3.0e-0, 3.5e-0, 4.0e-0, 4.5e-0, 5.0e-0, &
                 5.5e-0, 6.0e-0, 6.5e-0, 7.0e-0, 7.5e-0, 8.0e-0, 8.5e-0, 9.0e-0, 9.5e-0, &
                 1.0e+1, 1.5e+1, 2.0e+1, 2.5e+1, 3.0e+1, 3.5e+1, 4.0e+1, 4.5e+1, 5.0e+1, &
                 5.5e+1, 6.0e+1, 6.5e+1, 7.0e+1, 7.5e+1, 8.0e+1, 8.5e+1, 9.0e+1, 9.5e+1, &
                 1.0e+2, 1.5e+2, 2.0e+2, 2.5e+2, 3.0e+2, 3.5e+2, 4.0e+2, 4.5e+2, 5.0e+2, &
                 5.5e+2, 6.0e+2, 6.5e+2, 7.0e+2, 7.5e+2, 8.0e+2, 8.5e+2, 9.0e+2, 9.5e+2, &
                 1.0e+3, 1.5e+3, 2.0e+3, 2.5e+3, 3.0e+3, 3.5e+3, 4.0e+3, 4.5e+3, 5.0e+3, &
                 5.5e+3, 6.0e+3, 6.5e+3, 7.0e+3, 7.5e+3, 8.0e+3, 8.5e+3, 9.0e+3, 9.5e+3, &
                 1e+4 /)


  integer, parameter :: mxpts_rh  = 36       ! LUT dimension for relative humidity [fraction] 
  real, parameter :: pts_rh(mxpts_rh) = & ! LUT rh vector for aerosol particles
              (/ 0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, &
                 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.81, 0.82, 0.83, &
                 0.84, 0.85, 0.86, 0.87, 0.88, 0.89, 0.90, 0.91, 0.92, 0.93, &
                 0.94, 0.95, 0.96, 0.97, 0.98, 0.99 /)




! bundled scattering array (mxpts_re_visir, max_spc, nband, mxisl )
 real,allocatable :: kext_q_unit_array (:,:,:,:) ! extinction cross section per unit mass [km-1 / (g/m3) ]
 real,allocatable :: omega_array       (:,:,:,:) ! signle scattering albedo [-]
 real,allocatable :: asym_array        (:,:,:,:) ! asymetry parameter (forward scattering fraction) [-]

 real,allocatable :: kext_q_unit_aero_array (:,:,:,:) ! extinction cross section per unit mass [km-1 / (g/m3) ]
 real,allocatable :: omega_aero_array       (:,:,:,:) ! signle scattering albedo [-]
 real,allocatable :: asym_aero_array        (:,:,:,:) ! asymetry parameter (forward scattering fraction) [-]



!
! ----------- Mclatchy Sounding climatology ------------------------
!

 integer :: ilev, ifld
 integer, parameter :: ilev_max = 33         !sounding vertical layer #
 real,dimension(ilev_max,10,6) :: mcdat      !Mclatchy Sounding climatology

!
! ifield      Parameters 
!    1      : Height (m)
!    2      : pressure (Pa)
!    3      : air temp (K)
!    4      : vapor density (kg/m3)
!    5      : ozone density (kg/m3)
!    6      : air density (kg/m3)
!
! -- Definition of latitude for each regime 
!  90  deg -- Arctic (1~2)
!  67.5deg -- Sub-Arctic (3~4)
!  45  deg -- Mid-latitude (5~6)
!  22.5deg -- Sub-Tropics (7~8)
!   0. deg -- Tropics (9~10)
!
!  arctic winter
data ((mcdat(ilev,1,ifld),ifld=1,6),ilev=1,11)/  &
       0., 101350.0,    249.1,  .1201E-02,  .4105E-07,  .1417E+01,  &
    1000.,  88416.0,    252.2,  .1190E-02,  .4067E-07,  .1221E+01,  &
    2000.,  77213.0,    250.9,  .1014E-02,  .4036E-07,  .1072E+01,  &
    3000.,  67274.0,    245.4,  .7333E-03,  .4221E-07,  .9549E+00,  &
    4000.,  58431.0,    239.9,  .4471E-03,  .4384E-07,  .8485E+00,  &
    5000.,  50583.0,    234.4,  .2254E-03,  .4527E-07,  .7518E+00,  &
    6000.,  43640.0,    228.9,  .9344E-04,  .4681E-07,  .6643E+00,  &
    7000.,  37520.0,    223.4,  .3123E-04,  .6740E-07,  .5852E+00,  &
    8000.,  32171.0,    217.9,  .1248E-04,  .8508E-07,  .5139E+00,  &
    9000.,  27435.0,    214.9,  .7875E-05,  .1505E-06,  .4448E+00,  &
   10000.,  23398.0,    214.4,  .5161E-05,  .2248E-06,  .3802E+00/
data ((mcdat(ilev,1,ifld),ifld=1,6),ilev=12,22)/  &
   11000.,  19951.0,    213.9,  .3533E-05,  .2983E-06,  .3249E+00,  &
   12000.,  17008.0,    213.2,  .2393E-05,  .3988E-06,  .2779E+00,  &
   13000.,  14490.0,    212.4,  .1538E-05,  .4330E-06,  .2376E+00,  &
   14000.,  12338.0,    211.6,  .1005E-05,  .4477E-06,  .2031E+00,  &
   15000.,  10499.0,    210.9,  .6644E-06,  .5076E-06,  .1735E+00,  &
   16000.,   8929.0,    210.1,  .5438E-06,  .5554E-06,  .1481E+00,  &
   17000.,   7590.0,    209.3,  .4610E-06,  .5497E-06,  .1264E+00,  &
   18000.,   6450.0,    208.4,  .3906E-06,  .5442E-06,  .1078E+00,  &
   19000.,   5475.0,    207.7,  .3307E-06,  .5208E-06,  .9185E-01,  &
   20000.,   4648.0,    207.6,  .2800E-06,  .4809E-06,  .7797E-01,  &
   21000.,   3945.0,    207.6,  .2373E-06,  .4337E-06,  .6619E-01/
data ((mcdat(ilev,1,ifld),ifld=1,6),ilev=23,33)/  &
   22000.,   3349.0,    207.6,  .2014E-06,  .3961E-06,  .5619E-01,  &
   23000.,   2843.0,    207.6,  .1705E-06,  .3594E-06,  .4770E-01,  &
   24000.,   2414.0,    207.6,  .1443E-06,  .2986E-06,  .4050E-01,  &
   25000.,   2050.0,    207.6,  .1226E-06,  .2633E-06,  .3439E-01,  &
   30000.,    905.1,    207.6,  .5169E-07,  .1178E-06,  .1519E-01,  &
   35000.,    417.1,    213.9,  .2317E-07,  .7227E-07,  .6804E-02,  &
   40000.,    199.0,    225.6,  .1045E-07,  .3221E-07,  .3075E-02,  &
   45000.,     98.8,    237.7,  .4933E-08,  .1021E-07,  .1449E-02,  &
   50000.,     50.8,    248.2,  .2412E-08,  .3378E-08,  .7094E-03,  &
   70000.,      3.5,    235.3,  .1791E-09,  .6756E-10,  .5259E-04,  &
  103000.,       .1,    201.2,  .1571E-11,  .3378E-13,  .4617E-06/
!  arctic summer
data ((mcdat(ilev,2,ifld),ifld=1,6),ilev=1,11)/  &
       0., 101250.0,    278.1,  .9164E-02,  .4935E-07,  .1265E+01,  &
    1000.,  89502.0,    275.5,  .5963E-02,  .5366E-07,  .1129E+01,  &
    2000.,  79020.0,    272.9,  .4173E-02,  .5564E-07,  .1007E+01,  &
    3000.,  69671.0,    268.4,  .2664E-02,  .5743E-07,  .9030E+00,  &
    4000.,  61250.0,    261.9,  .1630E-02,  .5926E-07,  .8141E+00,  &
    5000.,  53667.0,    255.4,  .9583E-03,  .6303E-07,  .7317E+00,  &
    6000.,  46862.0,    248.9,  .5328E-03,  .6966E-07,  .6558E+00,  &
    7000.,  40778.0,    242.4,  .2829E-03,  .7316E-07,  .5861E+00,  &
    8000.,  35349.0,    235.9,  .1262E-03,  .7668E-07,  .5221E+00,  &
    9000.,  30525.0,    229.4,  .4040E-04,  .1063E-06,  .4636E+00,  &
   10000.,  26261.0,    226.7,  .1681E-04,  .1249E-06,  .4036E+00/
data ((mcdat(ilev,2,ifld),ifld=1,6),ilev=12,22)/  &
   11000.,  22598.0,    227.7,  .8268E-05,  .1739E-06,  .3458E+00,  &
   12000.,  19460.0,    228.6,  .4072E-05,  .2036E-06,  .2965E+00,  &
   13000.,  16770.0,    229.6,  .2006E-05,  .2532E-06,  .2544E+00,  &
   14000.,  12469.0,    230.1,  .7442E-06,  .2043E-06,  .1887E+00,  &
   15000.,  10752.0,    230.1,  .5726E-06,  .2358E-06,  .1628E+00,  &
   16000.,   9273.0,    230.1,  .4935E-06,  .2508E-06,  .1404E+00,  &
   17000.,   7999.0,    230.1,  .4274E-06,  .2899E-06,  .1211E+00,  &
   18000.,   6898.0,    230.1,  .3693E-06,  .3065E-06,  .1044E+00,  &
   19000.,   5950.0,    230.1,  .3198E-06,  .3085E-06,  .9007E-01,  &
   20000.,   5232.0,    230.1,  .2823E-06,  .3016E-06,  .7769E-01,  &
   21000.,   4428.0,    230.1,  .2395E-06,  .2746E-06,  .6702E-01/
data ((mcdat(ilev,2,ifld),ifld=1,6),ilev=23,33)/  &
   22000.,   3819.0,    230.1,  .2071E-06,  .2454E-06,  .5781E-01,  &
   23000.,   3295.0,    230.7,  .1788E-06,  .2312E-06,  .4976E-01,  &
   24000.,   2845.0,    231.9,  .1538E-06,  .2176E-06,  .4274E-01,  &
   25000.,   2459.0,    233.1,  .1330E-06,  .2035E-06,  .3674E-01,  &
   30000.,   1198.0,    239.1,  .9439E-07,  .1662E-06,  .1746E-01,  &
   35000.,    591.0,    251.6,  .3335E-07,  .8225E-07,  .8631E-02,  &
   40000.,    304.0,    266.9,  .1618E-07,  .3666E-07,  .4442E-02,  &
   45000.,    161.8,    278.9,  .8288E-08,  .1162E-07,  .2371E-02,  &
   50000.,     88.2,    281.8,  .4443E-08,  .3844E-08,  .1288E-02,  &
   70000.,      6.3,    220.6,  .4068E-09,  .7689E-10,  .9227E-04,  &
  104000.,       .1,    213.1,  .1788E-11,  .3844E-13,  .6525E-06/
!  sub-arctic winter
data ((mcdat(ilev,3,ifld),ifld=1,6),ilev=1,11)/  &
       0., 101300.0,    257.1,  .1200E-02,  .4100E-07,  .1372E+01,  &
    1000.,  88780.0,    259.1,  .1200E-02,  .4100E-07,  .1193E+01,  &
    2000.,  77750.0,    256.4,  .1030E-02,  .4100E-07,  .1058E+01,  &
    3000.,  67980.0,    252.2,  .7470E-03,  .4300E-07,  .9366E+00,  &
    4000.,  59320.0,    246.8,  .4590E-03,  .4500E-07,  .8339E+00,  &
    5000.,  51580.0,    240.6,  .2340E-03,  .4700E-07,  .7457E+00,  &
    6000.,  44670.0,    233.9,  .9780E-04,  .4900E-07,  .6646E+00,  &
    7000.,  38530.0,    227.1,  .3290E-04,  .7100E-07,  .5904E+00,  &
    8000.,  33080.0,    220.4,  .1320E-04,  .9000E-07,  .5226E+00,  &
    9000.,  28290.0,    217.1,  .8370E-05,  .1600E-06,  .4538E+00,  &
   10000.,  24180.0,    217.1,  .5510E-05,  .2400E-06,  .3879E+00/
data ((mcdat(ilev,3,ifld),ifld=1,6),ilev=12,22)/  &
   11000.,  20670.0,    217.1,  .3790E-05,  .3200E-06,  .3315E+00,  &
   12000.,  17660.0,    217.1,  .2580E-05,  .4300E-06,  .2834E+00,  &
   13000.,  15100.0,    217.1,  .1670E-05,  .4700E-06,  .2422E+00,  &
   14000.,  12910.0,    217.1,  .1100E-05,  .4900E-06,  .2071E+00,  &
   15000.,  11030.0,    217.0,  .7330E-06,  .5600E-06,  .1770E+00,  &
   16000.,   9431.0,    216.7,  .6070E-06,  .6200E-06,  .1517E+00,  &
   17000.,   8058.0,    216.1,  .5200E-06,  .6200E-06,  .1300E+00,  &
   18000.,   6882.0,    215.5,  .4450E-06,  .6200E-06,  .1113E+00,  &
   19000.,   5875.0,    214.9,  .3810E-06,  .6000E-06,  .9529E-01,  &
   20000.,   5014.0,    214.3,  .3260E-06,  .5600E-06,  .8155E-01,  &
   21000.,   4277.0,    213.7,  .2790E-06,  .5100E-06,  .6976E-01/
data ((mcdat(ilev,3,ifld),ifld=1,6),ilev=23,33)/  &
   22000.,   3647.0,    213.1,  .2390E-06,  .4700E-06,  .5966E-01,  &
   23000.,   3109.0,    212.5,  .2040E-06,  .4300E-06,  .5100E-01,  &
   24000.,   2649.0,    212.0,  .1740E-06,  .3600E-06,  .4358E-01,  &
   25000.,   2256.0,    211.9,  .1490E-06,  .3200E-06,  .3722E-01,  &
   30000.,   1020.0,    216.6,  .6580E-07,  .1500E-06,  .1645E-01,  &
   35000.,    470.1,    223.1,  .2950E-07,  .9200E-07,  .7368E-02,  &
   40000.,    224.3,    235.3,  .1330E-07,  .4100E-07,  .3330E-02,  &
   45000.,    111.3,    247.9,  .6280E-08,  .1300E-07,  .1569E-02,  &
   50000.,     57.2,    258.9,  .3070E-08,  .4300E-08,  .7682E-03,  &
   70000.,      4.0,    245.4,  .2280E-09,  .8600E-10,  .5695E-04,  &
  103000.,       .1,    209.9,  .2000E-11,  .4300E-13,  .5000E-06/
!  sub-arctic summer
data ((mcdat(ilev,4,ifld),ifld=1,6),ilev=1,11)/  &
       0., 101000.0,    287.0,  .9100E-02,  .4900E-07,  .1220E+01,  &
    1000.,  89600.0,    281.7,  .6000E-02,  .5400E-07,  .1110E+01,  &
    2000.,  79290.0,    276.4,  .4200E-02,  .5600E-07,  .9971E+00,  &
    3000.,  70000.0,    271.1,  .2690E-02,  .5800E-07,  .8985E+00,  &
    4000.,  61600.0,    265.7,  .1650E-02,  .6000E-07,  .8077E+00,  &
    5000.,  54100.0,    259.8,  .9730E-03,  .6400E-07,  .7244E+00,  &
    6000.,  47300.0,    252.8,  .5430E-03,  .7100E-07,  .6519E+00,  &
    7000.,  41300.0,    245.8,  .2900E-03,  .7500E-07,  .5849E+00,  &
    8000.,  35900.0,    238.8,  .1300E-03,  .7900E-07,  .5231E+00,  &
    9000.,  31070.0,    231.8,  .4180E-04,  .1100E-06,  .4663E+00,  &
   10000.,  26770.0,    225.6,  .1750E-04,  .1300E-06,  .4142E+00/
data ((mcdat(ilev,4,ifld),ifld=1,6),ilev=12,22)/  &
   11000.,  23000.0,    225.0,  .8560E-05,  .1800E-06,  .3559E+00,  &
   12000.,  19770.0,    225.0,  .4200E-05,  .2100E-06,  .3059E+00,  &
   13000.,  17000.0,    225.0,  .2060E-05,  .2600E-06,  .2630E+00,  &
   14000.,  14600.0,    225.0,  .1020E-05,  .2800E-06,  .2260E+00,  &
   15000.,  12500.0,    225.0,  .7770E-06,  .3200E-06,  .1943E+00,  &
   16000.,  10801.0,    225.0,  .6690E-06,  .3400E-06,  .1671E+00,  &
   17000.,   9280.0,    225.0,  .5750E-06,  .3900E-06,  .1436E+00,  &
   18000.,   7980.0,    225.0,  .4940E-06,  .4100E-06,  .1235E+00,  &
   19000.,   6860.0,    225.0,  .4250E-06,  .4100E-06,  .1062E+00,  &
   20000.,   5890.0,    225.0,  .3650E-06,  .3900E-06,  .9128E-01,  &
   21000.,   5070.0,    225.0,  .3140E-06,  .3600E-06,  .7849E-01/
data ((mcdat(ilev,4,ifld),ifld=1,6),ilev=23,33)/  &
   22000.,   4360.0,    225.1,  .2700E-06,  .3200E-06,  .6750E-01,  &
   23000.,   3750.0,    225.5,  .2320E-06,  .3000E-06,  .5805E-01,  &
   24000.,   3227.0,    226.6,  .1980E-06,  .2800E-06,  .4963E-01,  &
   25000.,   2780.0,    227.9,  .1700E-06,  .2600E-06,  .4247E-01,  &
   30000.,   1340.0,    234.9,  .7950E-07,  .1400E-06,  .1338E-01,  &
   35000.,    661.0,    247.2,  .3730E-07,  .9200E-07,  .6614E-02,  &
   40000.,    340.0,    262.3,  .1810E-07,  .4100E-07,  .3404E-02,  &
   45000.,    181.0,    274.1,  .9270E-08,  .1300E-07,  .1817E-02,  &
   50000.,     98.7,    276.9,  .4970E-08,  .4300E-08,  .9868E-03,  &
   70000.,      7.1,    216.8,  .4550E-09,  .8600E-10,  .7071E-04,  &
  104000.,       .1,    209.4,  .2000E-11,  .4300E-13,  .5000E-06/
!  mid-latitude winter
data ((mcdat(ilev,5,ifld),ifld=1,6),ilev=1,11)/  &
       0., 101800.0,    272.2,  .3500E-02,  .6000E-07,  .1301E+01,  &
    1000.,  89730.0,    268.7,  .2500E-02,  .5400E-07,  .1162E+01,  &
    2000.,  78970.0,    265.2,  .1800E-02,  .4900E-07,  .1037E+01,  &
    3000.,  69380.0,    261.2,  .1160E-02,  .4900E-07,  .9230E+00,  &
    4000.,  60810.0,    255.7,  .6900E-03,  .4900E-07,  .8282E+00,  &
    5000.,  53130.0,    249.6,  .3780E-03,  .5800E-07,  .7411E+00,  &
    6000.,  46270.0,    243.6,  .1890E-03,  .6400E-07,  .6614E+00,  &
    7000.,  40160.0,    237.6,  .8570E-04,  .7700E-07,  .5886E+00,  &
    8000.,  34730.0,    231.6,  .3500E-04,  .9000E-07,  .5222E+00,  &
    9000.,  29920.0,    225.6,  .1600E-04,  .1200E-06,  .4619E+00,  &
   10000.,  25680.0,    220.6,  .7500E-05,  .1600E-06,  .4072E+00/
data ((mcdat(ilev,5,ifld),ifld=1,6),ilev=12,22)/  &
   11000.,  21990.0,    219.2,  .4440E-05,  .2100E-06,  .3496E+00,  &
   12000.,  18820.0,    218.7,  .2720E-05,  .2600E-06,  .2999E+00,  &
   13000.,  16100.0,    218.2,  .1720E-05,  .3000E-06,  .2572E+00,  &
   14000.,  13780.0,    217.7,  .1130E-05,  .3200E-06,  .2206E+00,  &
   15000.,  11780.0,    217.2,  .7640E-06,  .3400E-06,  .1890E+00,  &
   16000.,  10070.0,    216.7,  .6480E-06,  .3600E-06,  .1620E+00,  &
   17000.,   8610.0,    216.2,  .5550E-06,  .3900E-06,  .1388E+00,  &
   18000.,   7350.0,    215.7,  .4750E-06,  .4100E-06,  .1188E+00,  &
   19000.,   6280.0,    215.4,  .4060E-06,  .4300E-06,  .1017E+00,  &
   20000.,   5370.0,    215.2,  .3040E-06,  .4500E-06,  .8690E-01,  &
   21000.,   4580.0,    215.2,  .2970E-06,  .4300E-06,  .7421E-01/
data ((mcdat(ilev,5,ifld),ifld=1,6),ilev=23,33)/  &
   22000.,   3910.0,    215.2,  .2530E-06,  .4300E-06,  .6338E-01,  &
   23000.,   3340.0,    215.2,  .2160E-06,  .3900E-06,  .5415E-01,  &
   24000.,   2860.0,    215.2,  .1850E-06,  .3600E-06,  .4624E-01,  &
   25000.,   2430.0,    215.4,  .1570E-06,  .3400E-06,  .3950E-01,  &
   30000.,   1110.0,    217.3,  .7120E-07,  .1900E-06,  .1783E-01,  &
   35000.,    518.0,    227.9,  .3170E-07,  .9200E-07,  .7924E-02,  &
   40000.,    253.0,    244.0,  .1450E-07,  .4100E-07,  .3625E-02,  &
   45000.,    129.0,    258.9,  .6940E-08,  .1300E-07,  .1741E-02,  &
   50000.,     68.2,    265.6,  .3580E-08,  .4300E-08,  .8954E-03,  &
   70000.,      4.7,    230.9,  .2820E-09,  .8600E-10,  .7051E-04,  &
  103000.,       .1,    210.1,  .1990E-11,  .4300E-13,  .5000E-06/
!  mid-latitude summer
data ((mcdat(ilev,6,ifld),ifld=1,6),ilev=1,11)/  &
       0., 101300.0,    294.0,  .1400E-01,  .6000E-07,  .1191E+01,  &
    1000.,  90200.0,    290.0,  .9300E-02,  .6000E-07,  .1080E+01,  &
    2000.,  80200.0,    285.0,  .5850E-02,  .6000E-07,  .9757E+00,  &
    3000.,  71000.0,    279.0,  .3430E-02,  .6200E-07,  .8846E+00,  &
    4000.,  62800.0,    273.0,  .1890E-02,  .6400E-07,  .7998E+00,  &
    5000.,  55400.0,    267.1,  .1000E-02,  .6600E-07,  .7211E+00,  &
    6000.,  48700.0,    261.0,  .6090E-03,  .6900E-07,  .6487E+00,  &
    7000.,  42600.0,    254.7,  .3710E-03,  .7500E-07,  .5830E+00,  &
    8000.,  37200.0,    248.2,  .2100E-03,  .7900E-07,  .5225E+00,  &
    9000.,  32400.0,    241.7,  .1180E-03,  .8600E-07,  .4669E+00,  &
   10000.,  28100.0,    235.2,  .6430E-04,  .9000E-07,  .4159E+00/
data ((mcdat(ilev,6,ifld),ifld=1,6),ilev=12,22)/  &
   11000.,  24300.0,    228.8,  .2190E-04,  .1100E-06,  .3693E+00,  &
   12000.,  20900.0,    222.3,  .6460E-05,  .1200E-06,  .3269E+00,  &
   13000.,  17900.0,    216.9,  .1660E-05,  .1500E-06,  .2882E+00,  &
   14000.,  15300.0,    215.8,  .9950E-06,  .1800E-06,  .2464E+00,  &
   15000.,  13000.0,    215.8,  .8400E-06,  .1900E-06,  .2104E+00,  &
   16000.,  11000.0,    215.8,  .7100E-06,  .2100E-06,  .1797E+00,  &
   17000.,   9500.0,    215.8,  .6140E-06,  .2400E-06,  .1535E+00,  &
   18000.,   8120.0,    216.0,  .5240E-06,  .2800E-06,  .1305E+00,  &
   19000.,   6950.0,    217.0,  .4460E-06,  .3200E-06,  .1110E+00,  &
   20000.,   5950.0,    218.2,  .3800E-06,  .3400E-06,  .9453E-01,  &
   21000.,   5100.0,    219.4,  .3240E-06,  .3600E-06,  .8056E-01/
data ((mcdat(ilev,6,ifld),ifld=1,6),ilev=23,33)/  &
   22000.,   4370.0,    220.6,  .2760E-06,  .3600E-06,  .6872E-01,  &
   23000.,   3760.0,    221.8,  .2360E-06,  .3400E-06,  .5867E-01,  &
   24000.,   3220.0,    223.0,  .2010E-06,  .3200E-06,  .5014E-01,  &
   25000.,   2770.0,    224.2,  .1720E-06,  .3000E-06,  .4288E-01,  &
   30000.,   1320.0,    234.2,  .7850E-07,  .2000E-06,  .1322E-01,  &
   35000.,    652.0,    245.3,  .3700E-07,  .9200E-07,  .6519E-02,  &
   40000.,    333.0,    257.5,  .1800E-07,  .4100E-07,  .3330E-02,  &
   45000.,    176.0,    269.7,  .9090E-08,  .1300E-07,  .1757E-02,  &
   50000.,     95.1,    276.2,  .4800E-08,  .4300E-08,  .9512E-03,  &
   70000.,      6.7,    219.1,  .4270E-09,  .8600E-10,  .6706E-04,  &
  104000.,       .1,    209.9,  .1990E-11,  .4300E-13,  .5000E-06/
!  subtropical winter
data ((mcdat(ilev,7,ifld),ifld=1,6),ilev=1,11)/  &
       0., 102100.0,    287.1,  .1125E-01,  .5800E-07,  .1233E+01,  &
    1000.,  90659.0,    284.2,  .7750E-02,  .5500E-07,  .1107E+01,  &
    2000.,  80378.0,    281.2,  .5545E-02,  .5150E-07,  .9934E+00,  &
    3000.,  71125.0,    274.7,  .2930E-02,  .5000E-07,  .9006E+00,  &
    4000.,  62740.0,    268.2,  .1675E-02,  .4800E-07,  .8142E+00,  &
    5000.,  55176.0,    261.7,  .9540E-03,  .5150E-07,  .7340E+00,  &
    6000.,  48367.0,    255.2,  .5245E-03,  .5350E-07,  .6599E+00,  &
    7000.,  42254.0,    248.8,  .2783E-03,  .5900E-07,  .5916E+00,  &
    8000.,  36786.0,    242.3,  .1425E-03,  .6450E-07,  .5289E+00,  &
    9000.,  31906.0,    235.8,  .6850E-04,  .7950E-07,  .4713E+00,  &
   10000.,  27563.0,    229.3,  .2825E-04,  .9950E-07,  .4187E+00/
data ((mcdat(ilev,7,ifld),ifld=1,6),ilev=12,22)/  &
   11000.,  23716.0,    222.9,  .1117E-04,  .1255E-06,  .3707E+00,  &
   12000.,  20315.0,    216.4,  .4400E-05,  .1515E-06,  .3270E+00,  &
   13000.,  17344.0,    213.7,  .1755E-05,  .1725E-06,  .2828E+00,  &
   14000.,  14781.0,    211.1,  .1058E-05,  .1825E-06,  .2439E+00,  &
   15000.,  12557.0,    208.5,  .7605E-06,  .1935E-06,  .2101E+00,  &
   16000.,  10671.0,    205.9,  .6425E-06,  .2035E-06,  .1805E+00,  &
   17000.,   9041.0,    203.3,  .5485E-06,  .2295E-06,  .1549E+00,  &
   18000.,   7651.0,    203.1,  .4615E-06,  .2500E-06,  .1311E+00,  &
   19000.,   6480.0,    205.4,  .3880E-06,  .2850E-06,  .1099E+00,  &
   20000.,   5498.0,    207.9,  .3060E-06,  .3200E-06,  .9213E-01,  &
   21000.,   4676.0,    210.4,  .2770E-06,  .3350E-06,  .7743E-01/
data ((mcdat(ilev,7,ifld),ifld=1,6),ilev=23,33)/  &
   22000.,   3984.0,    212.9,  .2345E-06,  .3550E-06,  .6520E-01,  &
   23000.,   3401.0,    214.9,  .1995E-06,  .3550E-06,  .5512E-01,  &
   24000.,   2907.0,    216.9,  .1700E-06,  .3500E-06,  .4669E-01,  &
   25000.,   2489.0,    218.9,  .1440E-06,  .3400E-06,  .3961E-01,  &
   30000.,   1169.0,    228.8,  .6535E-07,  .2150E-06,  .1780E-01,  &
   35000.,    568.0,    239.8,  .2980E-07,  .9200E-07,  .8255E-02,  &
   40000.,    286.0,    251.6,  .1405E-07,  .4100E-07,  .3960E-02,  &
   45000.,    148.8,    263.4,  .6870E-08,  .1300E-07,  .1967E-02,  &
   50000.,     79.4,    269.1,  .3580E-08,  .4300E-08,  .1027E-02,  &
   70000.,      5.4,    221.7,  .2905E-09,  .8600E-10,  .8440E-04,  &
  103000.,       .2,    191.1,  .1805E-11,  .4300E-13,  .3422E-05/
!  subtropical summer
data ((mcdat(ilev,8,ifld),ifld=1,6),ilev=1,11)/  &
       0., 101350.0,    301.1,  .1650E-01,  .5800E-07,  .1159E+01,  &
    1000.,  90464.0,    293.7,  .1115E-01,  .5800E-07,  .1066E+01,  &
    2000.,  80504.0,    288.2,  .7570E-02,  .5700E-07,  .9686E+00,  &
    3000.,  71484.0,    282.7,  .4065E-02,  .5650E-07,  .8776E+00,  &
    4000.,  63311.0,    277.2,  .2275E-02,  .5550E-07,  .7937E+00,  &
    5000.,  55936.0,    271.7,  .1265E-02,  .5550E-07,  .7159E+00,  &
    6000.,  49292.0,    266.3,  .7345E-03,  .5600E-07,  .6443E+00,  &
    7000.,  43304.0,    259.3,  .4210E-03,  .5800E-07,  .5814E+00,  &
    8000.,  37913.0,    252.3,  .2300E-03,  .5900E-07,  .5233E+00,  &
    9000.,  33068.0,    245.3,  .1195E-03,  .6250E-07,  .4694E+00,  &
   10000.,  28729.0,    238.4,  .5665E-04,  .6450E-07,  .4198E+00/
data ((mcdat(ilev,8,ifld),ifld=1,6),ilev=12,22)/  &
   11000.,  24858.0,    231.4,  .1990E-04,  .7550E-07,  .3742E+00,  &
   12000.,  21414.0,    224.4,  .6270E-05,  .8150E-07,  .3324E+00,  &
   13000.,  18359.0,    217.5,  .1725E-05,  .9750E-07,  .2941E+00,  &
   14000.,  15665.0,    210.5,  .9905E-06,  .1125E-06,  .2953E+00,  &
   15000.,  13295.0,    203.5,  .7985E-06,  .1185E-06,  .2276E+00,  &
   16000.,  11248.0,    203.1,  .6735E-06,  .1285E-06,  .1929E+00,  &
   17000.,   9526.0,    205.2,  .5780E-06,  .1545E-06,  .1617E+00,  &
   18000.,   8081.0,    207.4,  .4860E-06,  .1850E-06,  .1358E+00,  &
   19000.,   6868.0,    209.6,  .4080E-06,  .2300E-06,  .1142E+00,  &
   20000.,   5846.0,    211.8,  .3440E-06,  .2650E-06,  .9618E-01,  &
   21000.,   4986.0,    213.9,  .2905E-06,  .3000E-06,  .8119E-01/
data ((mcdat(ilev,8,ifld),ifld=1,6),ilev=23,33)/  &
   22000.,   4258.0,    215.9,  .2460E-06,  .3200E-06,  .6870E-01,  &
   23000.,   3643.0,    217.9,  .2095E-06,  .3300E-06,  .5823E-01,  &
   24000.,   3121.0,    219.9,  .1780E-06,  .3300E-06,  .4944E-01,  &
   25000.,   2677.0,    221.9,  .1515E-06,  .3200E-06,  .4203E-01,  &
   30000.,   1270.0,    231.8,  .6900E-07,  .2200E-06,  .1909E-01,  &
   35000.,    622.9,    242.8,  .3245E-07,  .9200E-07,  .8939E-02,  &
   40000.,    316.2,    254.6,  .1580E-07,  .4100E-07,  .4327E-02,  &
   45000.,    165.7,    266.4,  .7945E-08,  .1300E-07,  .2167E-02,  &
   50000.,     89.1,    272.1,  .4190E-08,  .4300E-08,  .1140E-03,  &
   70000.,      6.1,    217.6,  .3630E-09,  .8600E-10,  .9739E-04,  &
  103000.,       .2,    180.1,  .1805E-11,  .4300E-13,  .3472E-05/
!  tropical
data ((mcdat(ilev,9,ifld),ifld=1,6),ilev=1,11)/  &
       0., 101300.0,    300.0,  .1900E-01,  .5600E-07,  .1167E+01,  &
    1000.,  90400.0,    294.1,  .1300E-01,  .5600E-07,  .1064E+01,  &
    2000.,  80500.0,    288.4,  .9290E-02,  .5400E-07,  .9689E+00,  &
    3000.,  71500.0,    283.6,  .4700E-02,  .5100E-07,  .8756E+00,  &
    4000.,  63300.0,    277.4,  .2660E-02,  .4700E-07,  .7951E+00,  &
    5000.,  55900.0,    270.7,  .1530E-02,  .4500E-07,  .7199E+00,  &
    6000.,  49200.0,    264.0,  .8600E-03,  .4300E-07,  .6501E+00,  &
    7000.,  43200.0,    257.3,  .4710E-03,  .4100E-07,  .5855E+00,  &
    8000.,  37800.0,    250.6,  .2500E-03,  .3900E-07,  .5258E+00,  &
    9000.,  32900.0,    243.8,  .1210E-03,  .3900E-07,  .4708E+00,  &
   10000.,  28600.0,    237.2,  .4900E-04,  .3900E-07,  .4202E+00/
data ((mcdat(ilev,9,ifld),ifld=1,6),ilev=12,22)/  &
   11000.,  24700.0,    230.4,  .1790E-04,  .4100E-07,  .3740E+00,  &
   12000.,  21300.0,    223.8,  .6080E-05,  .4300E-07,  .3316E+00,  &
   13000.,  18200.0,    217.0,  .1790E-05,  .4500E-07,  .2929E+00,  &
   14000.,  15600.0,    210.4,  .9860E-06,  .4500E-07,  .2578E+00,  &
   15000.,  13200.0,    203.6,  .7570E-06,  .4700E-07,  .2260E+00,  &
   16000.,  11100.0,    196.8,  .6370E-06,  .4700E-07,  .1972E+00,  &
   17000.,   9370.0,    195.6,  .5420E-06,  .6900E-07,  .1676E+00,  &
   18000.,   7890.0,    199.5,  .4480E-06,  .9000E-07,  .1382E+00,  &
   19000.,   6660.0,    203.6,  .3700E-06,  .1400E-06,  .1145E+00,  &
   20000.,   5650.0,    207.6,  .3080E-06,  .1900E-06,  .9515E-01,  &
   21000.,   4800.0,    211.5,  .2570E-06,  .2400E-06,  .7938E-01/
data ((mcdat(ilev,9,ifld),ifld=1,6),ilev=23,33)/  &
   22000.,   4090.0,    214.6,  .2160E-06,  .2800E-06,  .6645E-01,  &
   23000.,   3500.0,    216.9,  .1830E-06,  .3200E-06,  .5618E-01,  &
   24000.,   3000.0,    219.1,  .1550E-06,  .3400E-06,  .4763E-01,  &
   25000.,   2570.0,    221.3,  .1310E-06,  .3400E-06,  .4045E-01,  &
   30000.,   1220.0,    232.3,  .5950E-07,  .2400E-06,  .1831E-01,  &
   35000.,    600.0,    243.3,  .2790E-07,  .9200E-07,  .8600E-02,  &
   40000.,    305.0,    254.3,  .1360E-07,  .4100E-07,  .4181E-02,  &
   45000.,    159.0,    264.9,  .6800E-08,  .1300E-07,  .2097E-02,  &
   50000.,     85.4,    270.0,  .3580E-08,  .4300E-08,  .1101E-02,  &
   70000.,      5.8,    219.5,  .2990E-09,  .8600E-10,  .9210E-04,  &
  103000.,       .1,    209.9,  .1620E-11,  .4300E-13,  .5000E-06/
!  tropical (summer)
data ((mcdat(ilev,10,ifld),ifld=1,6),ilev=1,11)/  &
       0., 101300.0,    300.0,  .1900E-01,  .5600E-07,  .1167E+01,  &
    1000.,  90400.0,    294.1,  .1300E-01,  .5600E-07,  .1064E+01,  &
    2000.,  80500.0,    288.4,  .9290E-02,  .5400E-07,  .9689E+00,  &
    3000.,  71500.0,    283.6,  .4700E-02,  .5100E-07,  .8756E+00,  &
    4000.,  63300.0,    277.4,  .2660E-02,  .4700E-07,  .7951E+00,  &
    5000.,  55900.0,    270.7,  .1530E-02,  .4500E-07,  .7199E+00,  &
    6000.,  49200.0,    264.0,  .8600E-03,  .4300E-07,  .6501E+00,  &
    7000.,  43200.0,    257.3,  .4710E-03,  .4100E-07,  .5855E+00,  &
    8000.,  37800.0,    250.6,  .2500E-03,  .3900E-07,  .5258E+00,  &
    9000.,  32900.0,    243.8,  .1210E-03,  .3900E-07,  .4708E+00,  &
   10000.,  28600.0,    237.2,  .4900E-04,  .3900E-07,  .4202E+00/
data ((mcdat(ilev,10,ifld),ifld=1,6),ilev=12,22)/  &
   11000.,  24700.0,    230.4,  .1790E-04,  .4100E-07,  .3740E+00,  &
   12000.,  21300.0,    223.8,  .6080E-05,  .4300E-07,  .3316E+00,  &
   13000.,  18200.0,    217.0,  .1790E-05,  .4500E-07,  .2929E+00,  &
   14000.,  15600.0,    210.4,  .9860E-06,  .4500E-07,  .2578E+00,  &
   15000.,  13200.0,    203.6,  .7570E-06,  .4700E-07,  .2260E+00,  &
   16000.,  11100.0,    196.8,  .6370E-06,  .4700E-07,  .1972E+00,  &
   17000.,   9370.0,    195.6,  .5420E-06,  .6900E-07,  .1676E+00,  &
   18000.,   7890.0,    199.5,  .4480E-06,  .9000E-07,  .1382E+00,  &
   19000.,   6660.0,    203.6,  .3700E-06,  .1400E-06,  .1145E+00,  &
   20000.,   5650.0,    207.6,  .3080E-06,  .1900E-06,  .9515E-01,  &
   21000.,   4800.0,    211.5,  .2570E-06,  .2400E-06,  .7938E-01/
 data ((mcdat(ilev,10,ifld),ifld=1,6),ilev=23,33)/  &
   22000.,   4090.0,    214.6,  .2160E-06,  .2800E-06,  .6645E-01,  &
   23000.,   3500.0,    216.9,  .1830E-06,  .3200E-06,  .5618E-01,  &
   24000.,   3000.0,    219.1,  .1550E-06,  .3400E-06,  .4763E-01,  &
   25000.,   2570.0,    221.3,  .1310E-06,  .3400E-06,  .4045E-01,  &
   30000.,   1220.0,    232.3,  .5950E-07,  .2400E-06,  .1831E-01,  &
   35000.,    600.0,    243.3,  .2790E-07,  .9200E-07,  .8600E-02,  &
   40000.,    305.0,    254.3,  .1360E-07,  .4100E-07,  .4181E-02,  &
   45000.,    159.0,    264.9,  .6800E-08,  .1300E-07,  .2097E-02,  &
   50000.,     85.4,    270.0,  .3580E-08,  .4300E-08,  .1101E-02,  &
   70000.,      5.8,    219.5,  .2990E-09,  .8600E-10,  .9210E-04,  &
  103000.,       .1,    209.9,  .1620E-11,  .4300E-13,  .5000E-06/

 contains

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine init_goddardrad(allowed_to_read, &
                                 ids, ide, jds, jde, kds, kde,                &
                                 ims, ime, jms, jme, kms, kme,                &
                                 its, ite, jts, jte, kts, kte                 )
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
!  Initialize single scattering tables.  
!  This is one-time initialization, and single scattering tables must be stored in each CPU memory. 
! 
! History: 08/2013  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!---------------------------------------------------------------------------------------------------
 logical,intent(in) :: allowed_to_read ! this is the ID statement for MPI master core

 INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,  &
                                   ims, ime, jms, jme, kms, kme,  &
                                   its, ite, jts, jte, kts, kte

 integer :: mxband
!
! initialize goddard radiation (to open/read single scattering look-up table)
!
 print*,'initialize goddard radiation single scattering tables'

 mxband = max(nband_sw, nband_lw)  !maximum band number 

!
! read/initialize single-scattering look-up table
!

 if(allowed_to_read) call read_lut_nc(mxband)

#if (WRF_CHEM == 1)

!  if(allowed_to_read) call read_lut_aero_nc(mxband)

#endif


 end subroutine init_goddardrad

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine read_lut_nc(mxband)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
!   Read NetCDF LUT data.
!    
! History: 
!  10/2017  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!---------------------------------------------------------------------------------------------------

 integer,intent(in) :: mxband !maximum band

 integer :: i,j,k !domain loop index
 integer :: ib,n  !band loop index
 integer :: ierr  !allocation stat
 integer :: max_band
 integer :: mxisl, max_spc
 integer :: ncid, varid
 integer :: nspec, nre, nb, ns 
 integer :: reclen

!
! static indice
!
 mxisl = 2  !1-sw, 2-lw

!
! allocate array parameters (format from G-SDSU)
!
 if( .not. allocated(kext_q_unit_array) ) then

   allocate( kext_q_unit_array ( mxpts_re_visir, id_hmax, mxband, mxisl ), &
             omega_array       ( mxpts_re_visir, id_hmax, mxband, mxisl ), &
             asym_array        ( mxpts_re_visir, id_hmax, mxband, mxisl ), &
             stat=ierr )
   if (ierr /= 0) call wrf_error_fatal('MSG read_lut_nc: LUT parameter allocation error -> Terminate program. ')
   inquire(iolength=reclen) kext_q_unit_array
   open(11, file='BROADBAND_CLOUD_GODDARD.bin', form='unformatted', &
      status='old', access='direct', &
      recl=reclen)
   ! extinction cross section per unit mass [km-1 / (g/m3) ]
   read(11, rec=1) kext_q_unit_array
   ! signle scattering albedo [-] 
   read(11, rec=2) omega_array
   ! asymetry parameter (forward scattering fraction) [-]
   read(11, rec=3) asym_array
   close(11)

 endif


!
! convert into SW/LW LUT (array format for goddard radiation)
!
 if( .not. allocated(kext_sw_cloud) ) then
     allocate( &
     kext_sw_cloud(mxband, mxpts_re_visir, id_hmax), & !
     salb_sw_cloud(mxband, mxpts_re_visir, id_hmax), & !
     asym_sw_cloud(mxband, mxpts_re_visir, id_hmax), & !
     kext_lw_cloud(mxband, mxpts_re_visir, id_hmax), & !
     salb_lw_cloud(mxband, mxpts_re_visir, id_hmax), & !
     asym_lw_cloud(mxband, mxpts_re_visir, id_hmax), & !
     stat=ierr )
     if (ierr /= 0) call wrf_error_fatal('MSG read_lut_nc: LUT parameter allocation error -> Terminate program. ')

! integer,parameter :: id_ice     = 1
! integer,parameter :: id_cloud   = 2
! integer,parameter :: id_rain    = 3
! integer,parameter :: id_snow    = 4
! integer,parameter :: id_graupel = 5
! integer,parameter :: id_hail    = 6

   do nspec = 1, id_hmax ; do nre = 1, mxpts_re_visir ; do nb = 1, mxband
     if(nspec == 1 ) ns = 2 
     if(nspec == 2 ) ns = 3 
     if(nspec == 3 ) ns = 1  
     if(nspec == 4 ) ns = 4
     if(nspec == 5 ) ns = 5 
     if(nspec == 6 ) ns = 6
     kext_sw_cloud(nb,nre,ns) = kext_q_unit_array(nre,nspec,nb,1)
     salb_sw_cloud(nb,nre,ns) = omega_array      (nre,nspec,nb,1)
     asym_sw_cloud(nb,nre,ns) = asym_array       (nre,nspec,nb,1)
     kext_lw_cloud(nb,nre,ns) = kext_q_unit_array(nre,nspec,nb,2)
     salb_lw_cloud(nb,nre,ns) = omega_array      (nre,nspec,nb,2)
     asym_lw_cloud(nb,nre,ns) = asym_array       (nre,nspec,nb,2)
   enddo ; enddo ; enddo

  !
  ! Broadcast LUT parameters 
  !
  call wrf_dm_bcast_bytes (kext_sw_cloud,  size(kext_sw_cloud) * RWORDSIZE  )
  call wrf_dm_bcast_bytes (salb_sw_cloud,  size(salb_sw_cloud) * RWORDSIZE )
  call wrf_dm_bcast_bytes (asym_sw_cloud,  size(asym_sw_cloud) * RWORDSIZE )
  call wrf_dm_bcast_bytes (kext_lw_cloud,  size(kext_lw_cloud) * RWORDSIZE )
  call wrf_dm_bcast_bytes (salb_lw_cloud,  size(salb_lw_cloud) * RWORDSIZE )
  call wrf_dm_bcast_bytes (asym_lw_cloud,  size(asym_lw_cloud) * RWORDSIZE )

 endif


 end subroutine read_lut_nc

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

#if 0
 subroutine read_lut_aero_nc(mxband)
 use netcdf
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
!   Read NetCDF LUT data for aerosols.
!    
! History: 
!  10/2017  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!---------------------------------------------------------------------------------------------------
 integer,intent(in) :: mxband !maximum band

 integer :: i,j,k !domain loop index
 integer :: ib,n  !band loop index
 integer :: ierr  !allocation stat
 integer :: max_band
 integer :: mxisl, max_spc, nband
 integer :: ncid, varid

!
! static indice
!
 mxisl = 2  !1-sw, 2-lw



!
! static indice
!
 mxisl = 2  !1-sw, 2-lw

!
! allocate array parameters
!
 if( .not. allocated(kext_q_unit_array) ) then

   allocate( kext_q_unit_aero_array ( mxpts_rh, id_amax, mxband, mxisl ), &
             omega_aero_array       ( mxpts_rh, id_amax, mxband, mxisl ), &
             asym_aero_array        ( mxpts_rh, id_amax, mxband, mxisl ), &
             stat=ierr )
   if (ierr /= 0) call wrf_error_fatal('MSG read_lut_aero_nc: LUT parameter allocation error -> Terminate program. ')

   !
   ! open netcdf file
   !
   sdsu_io_file = trim(dir_lut)//'BROADBAND_AEROSOL_GODDARD.nc'
   print*, 'MSG read_lut_aero_nc: reading broadband LUT:',trim(sdsu_io_file)
   call check( nf90_open(trim(sdsu_io_file), nf90_nowrite, ncid) )

   !
   ! extinction cross section per unit mass [km-1 / (g/m3) ]
   !
   call check( nf90_inq_varid(ncid, 'kext_q_unit' , varid ) )
   call check( nf90_get_var(ncid, varid, kext_q_unit_aero_array ))

   !
   ! signle scattering albedo [-] 
   !
   call check( nf90_inq_varid(ncid, 'omega' , varid ) )
   call check( nf90_get_var(ncid, varid, omega_aero_array ))

   !
   ! asymetry parameter (forward scattering fraction) [-] 
   !
   call check( nf90_inq_varid(ncid, 'asym' , varid ) )
   call check( nf90_get_var(ncid, varid, asym_aero_array ))

   !
   ! close netCDF file
   !
   call check( nf90_close(ncid) )

 endif

 end subroutine read_lut_aero_nc

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine check(status,para,missing_para)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Check status of reading process of netCDF. 
!
! History:
! 05/2014  Toshi Matsui@NASA GSFC ; included optional statement for skiping
! 05/2007  Toshi Matsui@NASA GSFC ; Initial   
!           
! References: 
!---------------------------------------------------------------------------------------------------
  integer, intent (in) :: status
  character(len=*),intent(in),optional :: para  !optional parameter for missing, but skippable parameters.
  logical,intent(out),optional :: missing_para  !optional parameter for missing, but skippable parameters.

  if( present(missing_para) )  missing_para = .false.


  if(status /= nf90_noerr) then
    print *, trim(nf90_strerror(status))

    !
    !for optional parameters, keep going on program.
    !
    if( present(para) ) then
          print*,'Missing ',trim(para)
          if( present(missing_para) ) missing_para = .true.
          return
    endif

    !
    !for required parameter, stop program
    ! 
    call wrf_error_fatal('MSG check: missing parameter in netcdf file')

  end if
 end subroutine check
#endif

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

   subroutine goddardrad( sw_or_lw, dx    &
                   ,rthraten, gsf, xlat,xlong         &
                   ,dz8w,t8w,rho_phy                              &
                   ,alb,emiss,tsk,t3d,qv,qc,qr                    &
                   ,qi,qs,qg,qh                                   &
                   ,p3d,p8w3d,pi3d,cldfra3d                       &
                   ,gmt,cp,g,julday,xtime,declin,solcon           &
                   ,radfrq,degrad,cod2d_out,ctop2d_out,warm_rain  &
                   ,f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qh            &
                   ,rec3d,rei3d,rer3d,res3d,reg3d,reh3d           &
                   ,ids,ide, jds,jde, kds,kde                     & 
                   ,ims,ime, jms,jme, kms,kme                     &
                   ,its,ite, jts,jte, kts,kte                     & 
                   ,ERBE_out                                      &  
                   ,itimestep, dt_in                              &  
                   ,sflxd                                         & !optional for sw
                   ,swddir,swddni,swddif                          & !optinal for sw
                   ,coszen                                        & !optional for sw
#if (WRF_CHEM == 1)
                   ,aod2d_out,atop2d_out                          & !optional for sw  
                   ,aero                                          & 
                   ,chem_opt                                      &
                   ,gsfcrad_gocart_coupling                       &
#endif
                    )
#ifndef NO_IEEE_MODULE
 use, intrinsic :: ieee_arithmetic
#endif
 implicit none
!----------------------------------------------------------------------
!
! Comments for vertical levels: 
!
! Vetical layers are reversed in this routine. 
! in wrf, kms (smallest number) is the bottom level and kme (largest
! number) is the top level.  in your scheme, if 1 is at the top level,
! then you have to reverse the order in the k direction.
!
! And, vertical dimesion of the input parameter is defined as 
!      kms:kme, which is equivalent to kts:kte+1
!      e.g., if kme=61, kte = 60
!
!
! For example, 
!        Array in both full- and half-level parameters are defined (ims:ime,kms:kme,jms:jme)
!        There is no data of k=kme for half-level parameters. 
!
!         kme      -   half level (no data for half level)
!         kme    ----- full level  (top for full level)
!         kme-1    -   half level  (top for half level)
!         kme-1  ----- full level
!         .
!         .
!         .
!         kms+1    -   half level
!         kms+1  ----- full level
!         kms      -   half level  (bottom for half level)
!         kms    ----- full level  (bottom for full level)
!
! However, when reversed...(and one dimensionalized) in thir routine. 
!          Array in full-level parameters are defined (kts-1:kte)
!          Array in half-level parameters are defined (kts  :kte) 
!
!         kts-1    -   half level (no data for half level)
!         kts-1  ----- full level  (top for full level)
!         kts      -   half level  (top for half level)
!         kts    ----- full level
!         .
!         .
!         .
!         kte-1    -   half level
!         kte-1  ----- full level
!         kte      -   half level  (bottom for half level)
!         kte    ----- full level  (bottom for full level)
!
! For example, kms=1 kme=51, then
!             Layer Temp                Level Temp
!           1 t3d(i,k,j)   302.6994     t8w(i,k,j)   302.8955
!           2 t3d(i,k,j)   302.3034     t8w(i,k,j)   302.5020
!           3 t3d(i,k,j)   301.2647     t8w(i,k,j)   301.7841
!           .
!           .
!          49 t3d(i,k,j)   228.0565     t8w(i,k,j)   227.4076
!          50 t3d(i,k,j)   231.9670     t8w(i,k,j)   228.7616
!          51 t3d(i,k,j)  0.0000000E+00 t8w(i,k,j)   235.3769
!
!
!             Layer Pres                Level Pres 
!           1 p3d(i,k,j)   95409.39     p8w(i,k,j)   96129.95
!           2 p3d(i,k,j)   93964.12     p8w(i,k,j)   94688.82
!           3 p3d(i,k,j)   92514.72     p8w(i,k,j)   93239.42
!           .
!           .
!          49 p3d(i,k,j)   1485.196     p8w(i,k,j)   1563.812
!          50 p3d(i,k,j)   1049.190     p8w(i,k,j)   1406.580
!          51 p3d(i,k,j)  0.0000000E+00 p8w(i,k,j)   691.8000
! Toshi Matsui @ NASA GSFC
!
!-------------------------------------------------------------------------------

!------------------------- I / O variables -------------------------------------

 character(len=2), intent(in) :: &
   sw_or_lw   ! 'sw'=shortwave radiation,'lw'=longwave radiation

 real,intent(in) :: dx   !horizontal grid spacing [m]
 integer :: iskip ! 0 (no skip) 1 (skip)
 integer,    intent(in) :: &
    ids, &    ! start index for i in domain
    ide, &    ! end index for i in domain
    jds, &    ! start index for j in domain
    jde, &    ! end index for j in domain
    kds, &    ! start index for k in domain
    kde, &    ! end index for k in domain 
    ims, &    ! start index for i in memory
    ime, &    ! end index for i in memory
    jms, &    ! start index for j in memory
    jme, &    ! end index for j in memory
    kms, &    ! start index for k in memory
    kme, &    ! end index for k in memory
    its, &    ! start index for i in tile
    ite, &    ! end index for i in tile
    jts, &    ! start index for j in tile
    jte, &    ! end index for j in tile
    kts, &    ! start index for k in tile
    kte, &    ! end index for k in tile (kte = kms - 1 )
   julday     ! integer of julian date  

 logical,    intent(in) ::  &
   warm_rain  ! logics for warm_rain only or not

 REAL, INTENT(IN  )   ::       dt_in
 INTEGER, INTENT(IN  ) ::      itimestep
 LOGICAL, EXTERNAL :: wrf_dm_on_monitor

 real, intent(in)      ::  &
   radfrq, &  ! time for calling radiation (min)                
   degrad, &  ! conversion factor for degrees to radians (pi/180.) (rad/deg)
   xtime,  &  ! time since simulation start (min)
   declin, &  ! solar declination angle (rad)
   solcon, &  ! solar constant (W/m^2) 
      gmt, &  ! greenwich mean time hour of model start (hour)
       cp, &  ! heat capacity at constant pressure for dry air (J/kg/K)
        g     ! acceleration due to gravity (m/s^2)

 real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: &
       p3d, &  ! pressure (Pa) 
     p8w3d, &  ! pressure at full levels (Pa)
      pi3d, &  ! exner function (-)
       t3d, &  ! temperature (K)
       t8w, &  ! temperature at full levels (K)
      dz8w, &  ! dz between full levels (m)
   rho_phy, &  ! density (kg/m^3)
  cldfra3d     ! cloud fraction (0-1)

 real, dimension( ims:ime, jms:jme ), intent(in)  :: &
       xlat, &  ! latitude, south is negative (degree)
      xlong, &  ! longitude, west is negative (degree)
        alb, &  ! broadband surface albedo (between 0 and 1)
        tsk, &  ! surface skin temperature (k)
       emiss    ! broadband surface emissivity (between 0 and 1)

 real, dimension( ims:ime, kms:kme, jms:jme ),optional,intent(in) :: &
     qv,   & ! water vapor mixing ratio (kg/kg)  (in)
     qc,   & ! cloud water mixing ratio (kg/kg)  (in)
     qr,   & ! rain water mixing ratio (kg/kg)   (in)
     qi,   & ! cloud ice mixing ratio (kg/kg)    (in)
     qs,   & ! snow mixing ratio (kg/kg)         (in)
     qg,   & ! graupel mixing ration (kg/kg)     (in)
     qh      ! hail mixing ration (kg/kg)     (in)

 real, dimension( ims:ime, kms:kme, jms:jme ),optional,intent(in) :: &
     rec3d,   & ! cloud effective radius [micron]  (in)
     rer3d,   & ! rain effective radius [micron]   (in)
     rei3d,   & ! cloud ice effective radius [micron] (kg/kg)    (in)
     res3d,   & ! snow effective radius [micron]  (in)
     reg3d,   & ! graupel effective radius [micron]  (in)
     reh3d      ! hail effective radius [micron]  (in)


 real, dimension( ims:ime, kms:kme, jms:jme ) :: &
     qv3d,   & ! water vapor mixing ratio (kg/kg)  (in)
     qc3d,   & ! cloud water mixing ratio (kg/kg)  (in)
     qr3d,   & ! rain water mixing ratio (kg/kg)   (in)
     qi3d,   & ! cloud ice mixing ratio (kg/kg)    (in)
     qs3d,   & ! snow mixing ratio (kg/kg)         (in)
     qg3d,   & ! graupel mixing ration (kg/kg)     (in)
     qh3d      ! hail mixing ration (kg/kg)        (in)


 logical, optional, intent(in )  ::  f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qh  !hydrometeor flags 
!
 real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout)  :: &
  rthraten  ! theta tendency due to radiative heating (K/sec)



 real, dimension( ims:ime, jms:jme ), intent(inout)  ::  &
         gsf    ! (for SW) : net short wave flux at ground surface (W/m^2) 
                ! (for LW) : downward long wave flux at ground surface (W/m^2)

 real, dimension( ims:ime, jms:jme ), intent(out),optional  :: cod2d_out  !cloud optical thickness [-]
 real, dimension( ims:ime, jms:jme ), intent(out),optional  :: ctop2d_out  !cloud-top pressure [mb]


 real, dimension( ims:ime, jms:jme, 1:4 ), intent(out),optional  :: sflxd    !SW only
       ! 1-surface downward UV+VIS beam radiation [W/m2]
       ! 2-surface downward UV+VIS diffuse radiation [W/m2]
       ! 3-surface downward NIR beam radiation [W/m2]
       ! 4-surface downward NIR diffuse radiation [W/m2]

! 
! ERBE_out: Energy Budget (last dimension 1-TOA LW down, 2-TOA LW up, 3-surface LW down, 4-surface LW up)
!                                         5-TOA SW down, 6-TOA SW up, 7-surface SW down, 8-surface SW up)
!
  real, dimension(ims:ime, jms:jme, 1:8),intent(out) :: ERBE_out  !earth radiation budget (W/m2)

#if (WRF_CHEM == 1)
!
! Aerosol is assumed to be GOCART scheme  (or other similar scheme must be re-array in consistent manner)
! Note that num_go = 14 : aerosol type (see below), and aero must be in the following order.
!  #   
!  1 = SO4      ! sulfur and its precure
!  2 = BC1+BC2  ! black carbon (soot)
!  3 = OC1      ! non hygroscopic OC
!  4 = OC2      ! hygroscopic OC
!  5 = SS1      ! sea-salt accumulation mode
!  6 = SS2+SS3+SS4 !sea-salt coarse mode
!  7 = DU1      ! dust mode 1
!  8 = DU1      ! dust mode 2
!  9 = DU1      ! dust mode 3
! 10 = DU1      ! dust mode 4
! 11 = DU2      ! dust mode 5
! 12 = DU3      ! dust mode 6
! 13 = DU4      ! dust mode 7
! 14 = DU5      ! dust mode 8
!
  integer, parameter :: num_go = 14  ! # of gocart aerosol species (tgmx=14 defined in module_gocart_offline.F)
  real, dimension( ims:ime, kms:kme, jms:jme, num_go), intent(in) :: aero    ! aerosol mass conc [g/m3]            
  real, dimension(ims:ime, jms:jme),intent(out),optional :: aod2d_out        ! column Aeorosol Optical Depth (-)
                                                                             ! at visible band (0.4-0.7 micron)
  real, dimension(ims:ime,jms:jme),intent(out),optional :: atop2d_out        ! aerosol-top pressure [mb]
  integer,intent(in) :: chem_opt ! EMK
  integer,intent(in) :: gsfcrad_gocart_coupling ! EMK
#endif        

! NCAR WRF V3.5.1 modification
  real, optional, dimension(ims:ime, jms:jme), intent(out) :: &           
         swddir, & ! All-sky broadband surface direct horizontal irradiance
         swddni, & ! All-sky broadband surface direct normal irradiance
         swddif    ! All-sky braodband surface diffuse irradiance
  real, optional, dimension(ims:ime, jms:jme), intent(in) :: &        
         coszen    ! cosine of the solar zenith angle



!------- Local variables ----------------------------------------------

 integer :: i,j,k,nk,ib,n,kt,km ! loop indice
 integer :: dk_half    !maximum dimension for k (half level)
 integer :: i24h

 real, dimension( CHUNK )  :: &
    cosz, &  ! cosine of solar zenith angle (0~1)
  rsuvbm, &  ! surface albedo for direct UV-VIS radiation (-)
  rsuvdf, &  ! surface albedo for diffuse UV-VIS radiation (-)
  rsirbm, &  ! surface albedo for direct NIR radiation (-)
  rsirdf, &  ! surface albedo for diffuse NIR radiation (-)
    tsfc, &  ! surface air temperature (K)
   tskin, &  ! surface skin (radiative) temperature (K)
    p400, &  ! pressure criteria for upper
    p700     ! pressure criteir for middle

 real, dimension( CHUNK, nband_lw ) :: emis1d !IR emissivity (-)

 integer, dimension( CHUNK ) ::  &
   ict, & ! 400mb level indice
   icb    ! 700mb level indice

!
! Full-level 1D parameters
!
 real, dimension( CHUNK, kts-1:kte ) :: &
     p8w1d, &  ! pressure in full level (mb or hPa)
     t8w1d     ! temperature in full level (K)

 real, dimension( CHUNK, kts-1:kte )  :: &
   flx, & !flux fraction (-) or actual flux (W/m2) 
  flxd, & !donwelling flux fraction [-] (for shortwave) , but actual flux [W/m2] (for longwave)
  flxu    !upwelling flux fraction [-] (for shortwave) , but actual flux [W/m2] (for longwave) 

!
! Half-level 1D parameters
!
 real, dimension( CHUNK, kts:kte ) ::   &
     tten1d, &  ! temperature tendency (K/sec)
       sh1d, &  ! specific humidity (-)
        p1d, &  ! pressure (mb or hPa)
        t1d, &  ! temperature (K)
      rho1d, &  ! dry air density [kg/m3]
       dz1d, &  ! layer depth (m)
       o31d, &  ! ozone mixing ratio [kg/kg]
     fcld1d     ! cloud fraction

 real,  dimension( CHUNK, kts:kte, id_hmax ) :: &
         q1d ,& ! condensate mass concentration [g/m3]
         re1d   ! particle effective radius [um]

! type ( particle_gocart ),dimension(nband_sw) :: & !particle_gocart is defined in module_simulater
!     kext_gocart,  & ! extinction coefficient [km-1]
!     salb_gocart,  & ! single scattering albedo [-]
!     asym_gocart     ! asymetry parameter [-]

 real, dimension( CHUNK, kts:kte, nband_sw ) :: &
  taucl_sw, &  ! hydrometeor optical depth for SW bands 
  ssacl_sw, &  ! hydrometeor single scattering albedo for SW bands
  asycl_sw     ! hydrometeor asymetry factor for SW bands

 real, dimension( CHUNK, kts:kte, nband_lw ) ::  &
  taucl_lw, &  ! hydrometeor optical depth for LW bands
  ssacl_lw, &  ! hydrometeor single scattering albedo for LW bands
  asycl_lw     ! hydrometeor asymetry factor for LW bands

 real, dimension( CHUNK, kts:kte, nband_sw ) :: &
  taual_sw, &  ! aerosol optical depth for SW bands 
  ssaal_sw, &  ! aerosol single scattering albedo for SW bands
  asyal_sw     ! aerosol asymetry factor for SW bands

 real, dimension( CHUNK, kts:kte, nband_lw ) ::  &
  taual_lw, &  ! aerosol optical depth for LW bands
  ssaal_lw, &  ! aerosol single scattering albedo for LW bands
  asyal_lw     ! aerosol asymetry factor for LW bands

#if (WRF_CHEM == 1)
 real, dimension( CHUNK, kts:kte, num_go) :: aero1d  !aerosol mass conc [g/m3] 1D reverse k order  (half layer)         

! type(particle_gocart), dimension( kts:kte ) ::   &
!                     a1d  ! aerosol mass concentration [ ]

#endif

!
! surface 0D parameters
!
 real :: flxd_surf(CHUNK,1:4)  ! downward surface flux 
                         ! 1-direct UV+PAR, 2-diffuse UV+PAR, 3-direct NIR, 4-diffuse NIR

! misc parameter for calculations
 real  :: fac,xt24,tloctm,hrang,xxlat ! ,x

! integer :: max_band

! parameters for fast skip radiation 
 integer :: ij_skip
 integer,dimension(CHUNK) :: ie,je !block array indice

 INTEGER :: ip, ic, ii
 logical :: no_micro(CHUNK), lmask(CHUNK) ! No update of theta tendancy (rthraten) and energy budget due to solar heating

! now in local parameter for OMP purpose

 real,dimension( CHUNK,ilev_max,6)    :: mcdat_int  !interpolated Mclatchy Sounding climatology

!--------------------       PROGRAM START        ---------------------------------------

  i24h=nint(86400./dt_in)
!
! Make sure there is no negative value in qv and cloud species
! Negative value happen through dynamic core (advection).
!
   do j = jts,jte ; do k = kts,kte   ; do i = its, ite  !3D full level
      if ( present( f_qv ) .and. present( qv)  )  qv3d(i,k,j) = max(0.0e0, qv(i,k,j))
      if ( present( f_qc ) .and. present( qc)  )  qc3d(i,k,j) = max(0.0e0, qc(i,k,j))
      if ( present( f_qr ) .and. present( qr)  )  qr3d(i,k,j) = max(0.0e0, qr(i,k,j))
      if ( present( f_qi ) .and. present( qi)  )  qi3d(i,k,j) = max(0.0e0, qi(i,k,j))
      if ( present( f_qs ) .and. present( qs)  )  qs3d(i,k,j) = max(0.0e0, qs(i,k,j))
      if ( present( f_qg ) .and. present( qg)  )  qg3d(i,k,j) = max(0.0e0, qg(i,k,j))
      if ( present( f_qh ) .and. present( qh)  )  qh3d(i,k,j) = max(0.0e0, qh(i,k,j))
   enddo ; enddo ; enddo

!
! Check input errors 
!
 if( NUWRF_DEBUG ) then 
   do j = jts,jte ; do k = kts,kte+1   ; do i = its, ite  !3D full level
      call checkerror('goddardrad','temperature_K',i,k,j,t8w(i,k,j) )
      call checkerror('goddardrad','pressure_Pa'  ,i,k,j,p8w3d(i,k,j) )
   enddo ; enddo ; enddo

   do j = jts,jte ; do k = kts,kte ; do i = its, ite  !3D half level
      call checkerror('goddardrad','temperature_K',i,k,j,t3d(i,k,j) )
      call checkerror('goddardrad','pressure_Pa'  ,i,k,j,p3d(i,k,j) )
      if ( present( f_qc ) .and. present( qc)  ) & ! cloud
          call checkerror('goddardrad_qc3d','condensate_kg/kg',i,k,j,qc3d(i,k,j) )
      if ( present( f_qc ) .and. present( qi)  ) & ! ice
          call checkerror('goddardrad_qi3d','condensate_kg/kg',i,k,j,qi3d(i,k,j) )
      if ( present( f_qc ) .and. present( qr)  ) & ! rain
          call checkerror('goddardrad_qr3d','condensate_kg/kg',i,k,j,qr3d(i,k,j) )
      if ( present( f_qc ) .and. present( qs)  ) & ! snow
          call checkerror('goddardrad_qs3d','condensate_kg/kg',i,k,j,qs3d(i,k,j) )
      if ( present( f_qc ) .and. present( qg)  ) & ! graupel
          call checkerror('goddardrad_qg3d','condensate_kg/kg',i,k,j,qg3d(i,k,j) )
#if (WRF_CHEM == 1)
      if ( (chem_opt == 300 .or. chem_opt == 301 .or. &
            chem_opt == 302 .or. chem_opt == 303) .and. &
            (gsfcrad_gocart_coupling == 1) ) then
         do n = 1, num_go
            call checkerror('goddardrad','aerosol_g/m3'  ,i,k,j,aero(i,k,j,n) )
         enddo
      end if
#endif

   enddo ; enddo ; enddo

   do j = jts,jte ; do i = its, ite  !2D 
      call checkerror('goddardrad','albedo'    ,i,0,j,  alb(i,j) )
      call checkerror('goddardrad','emissivity',i,0,j,emiss(i,j) )
   enddo ; enddo

 endif

!
! Initialize
!
 dk_half = kte-kts+1   !maximum dimension for k (half level)

 if(sw_or_lw=='sw') ctop2d_out(its:ite, jts:jte) = -999.e0

#if (WRF_CHEM == 1)
 if(sw_or_lw=='sw') atop2d_out(its:ite, jts:jte) = -999.e0
#endif


!
! grid aggregation and skippiing option, applying grid spacing less than 4km (new in 2014 scheme)
!
 iskip = 0  !zero for vectorlized version. 

 if( iskip == 1 ) then
    !
    ! nearest integer of 6000m devided by horizontal space
    ! ranges from 1 ~ 3 (essentially become 2, when dx<=4000m)
    !
    ij_skip = min( 3,  max( 1, nint( 6000.0/dx ) ) ) !

 elseif( iskip == 0 ) then
    ij_skip = 1
 else
    call wrf_error_fatal('MSG: goddardrad: iskip(goddardrad_skip) must be 0 or 1 ')
 endif

!!!
! TEST
 if(ij_skip .ne. 1) then
    print *, ' ij_skip = ', ij_skip
    print *, ' setting ij_skip=1'
    ij_skip = 1
 endif
!!!

 if( ij_skip > 1 ) then
    radiation_skip = .true.
 else
    radiation_skip = .false.
 endif


 if( overcast ) then
  fast_overcast = .true.
 else
  fast_overcast = .false.
 endif

! Removing these OpenMP directives since this is already in a 
! threaded region from the radiation driver for the WRF model.
! Perhaps other models use these, so we will keep them around.
#if (NUWRF_GODDARD == 1)
!$OMP PARALLEL DO &
!$OMP PRIVATE ( ic, ii) &
!$OMP PRIVATE ( i,j,k,nk,ib,n,kt,km, i24h ) &
!$OMP PRIVATE ( cosz, rsuvbm, rsuvdf, rsirbm, rsirdf, tsfc, tskin, p400, p700 ) &
!$OMP PRIVATE ( ict, icb, p8w1d, t8w1d, flx, flxd, flxu ) &
!$OMP PRIVATE ( tten1d, sh1d, p1d, t1d, rho1d, dz1d, o31d, fcld1d ) &
!$OMP PRIVATE ( emis1d, q1d, re1d ) &
!$OMP PRIVATE ( taucl_sw, ssacl_sw, asycl_sw ) &
!$OMP PRIVATE ( taucl_lw, ssacl_lw, asycl_lw ) &
!$OMP PRIVATE ( taual_sw, ssaal_sw, asyal_sw ) &
!$OMP PRIVATE ( taual_lw, ssaal_lw, asyal_lw ) &
!$OMP PRIVATE ( flxd_surf ) &
!$OMP PRIVATE ( fac,xt24,tloctm,hrang,xxlat ) &
!$OMP PRIVATE ( ie,je,no_micro,lmask,mcdat_int ) &
#if (WRF_CHEM == 1)
!$OMP PRIVATE ( aero1d ) &
#endif
!$OMP SCHEDULE(dynamic,1)
#endif

  DO ip = 1,((1+(ite-its+1)/CHUNK)*CHUNK)*(jte-jts+1),CHUNK ! *ij_skip
  j  = jts+(ip-1)/((1+(ite-its+1)/CHUNK)*CHUNK) ! *ij_skip
  IF ( j .ge. jts .and. j .le. jte ) THEN
  ii = its+mod((ip-1),((1+(ite-its+1)/CHUNK)*CHUNK)) ! *ij_skip

!TEST
         q1d(:,:,:) = 0.

  if( radiation_skip ) then
   DO ic=1,min(CHUNK,ite-ii+1) ! /ij_skip
     i = ii+(ic-1) ! *ij_skip
     ie(ic) =  min(ite,i+ij_skip-1) ! skip block i end bound
     je(ic) =  min(jte,j+ij_skip-1) ! skip block j end bound
   ENDDO
  endif

!
! Mclatchy sounding climatologies are temporally and spatially interpolated at each grid's latitude and julday.
!
  if( radiation_skip ) then
   DO ic=1,min(CHUNK,ite-ii+1)
     i = ii+ic-1
     call sounding_interp( ave(xlat(i:ie(ic),j:je(ic))) , julday , mcdat_int(ic,:,:))  
   ENDDO
  else
   DO ic=1,min(CHUNK,ite-ii+1)
     i = ii+ic-1
     call sounding_interp( xlat(i,j) , julday , mcdat_int(ic,:,:) )
   ENDDO
  endif

!
! reverse vertical profile for full-level 1D parameters
!
  if( radiation_skip ) then
      do kt=kts-1,kte !full level ( kts -1 = TOA, kte = BOA )
        DO ic=1,min(CHUNK,ite-ii+1)
          i = ii+ic-1
!           km=kme-kt+kms-1  
           km=kte-kt+kts  
           p8w1d(ic,kt) = ave(p8w3d(i:ie(ic),km,j:je(ic))) *0.01e0   ! p8w1d is in mb (hPa)  <-- [Pa]
           t8w1d(ic,kt) = ave(t8w(i:ie(ic),km,j:je(ic)))           ! temperature at full level (K)
!       print*, kt, km, p8w1d(ic,kt), t8w1d(ic,kt)
        ENDDO
      enddo
  else
      do kt=kts-1,kte !full level ( kts -1 = TOA, kte = BOA )
        DO ic=1,min(CHUNK,ite-ii+1)
          i = ii+ic-1
!           km=kme-kt+kms-1
           km=kte-kt+kts
           p8w1d(ic,kt) = p8w3d(i,km,j) *0.01e0   ! p8w1d is in mb (hPa)  <-- [Pa]
           t8w1d(ic,kt) = t8w(i,km,j)           ! temperature at full level (K)
!       print*, kt, km, p8w1d(ic,kt), t8w1d(ic,kt)
        ENDDO
      enddo
  endif

! Now set up TOA pressure is zero, this is required ...
  DO ic=1,min(CHUNK,ite-ii+1)
    p8w1d(ic,kts-1) = 0.
  ENDDO

!
! reverse vertical profile for half-level parameters
!
  if( radiation_skip ) then

     do kt=kts,kte   ! half level (kts = TOA, kte = BOA)
      DO ic=1,min(CHUNK,ite-ii+1)
        i = ii+ic-1
!        km=kme-kt+kms-1
        km=kte-kt+kts
        tten1d(ic,kt)  = 0.e0
        t1d   (ic,kt)  = ave(t3d(i:ie(ic),km,j:je(ic)))
        sh1d  (ic,kt)  = max(0., ave(qv3d(i:ie(ic),km,j:je(ic)))/(1.+ave(qv3d(i:ie(ic),km,j:je(ic)))) )
        p1d   (ic,kt)  = ave(p3d(i:ie(ic),km,j:je(ic)))*0.01e0      ! p1d is in mb (hPa)
        rho1d (ic,kt)  = ave(rho_phy(i:ie(ic),km,j:je(ic)))    !dry air density [kg/m3]
        fcld1d(ic,kt)  = ave(cldfra3d(i:ie(ic),km,j:je(ic)))
        dz1d  (ic,kt)  = ave(dz8w(i:ie(ic),km,j:je(ic)))     !dz8w is fake full-level (actually dz must be half layer) (m) (double check)
!       print*,kt,km, dz1d  (ic,kt), p1d   (ic,kt), t1d   (ic,kt)
      ENDDO
     enddo

  else

     do kt=kts,kte   ! half level (kts = TOA, kte = BOA)
      DO ic=1,min(CHUNK,ite-ii+1)
        i = ii+ic-1
!        km=kme-kt+kms-1
        km=kte-kt+kts
        tten1d(ic,kt)  = 0.e0
        t1d   (ic,kt)  = t3d(i,km,j)
        sh1d  (ic,kt)  = max(0., qv3d(i,km,j)/(1.+qv3d(i,km,j)) )
        p1d   (ic,kt)  = p3d(i,km,j)*0.01e0      ! p1d is in mb (hPa)
        rho1d (ic,kt)  = rho_phy(i,km,j)    !dry air density [kg/m3]
        fcld1d(ic,kt)  = cldfra3d(i,km,j)
        dz1d  (ic,kt)  = dz8w(i,km,j)     !dz8w is actually half level (m) (double checked)
!       print*,kt,km, dz1d  (ic,kt), p1d   (ic,kt), t1d   (ic,kt)
      ENDDO
     enddo

  endif

!
! vertical profiles for ozone by pressure in half level.
!
  DO ic=1,min(CHUNK,ite-ii+1)
    call ozone_interp( mcdat_int(ic,:,:), dk_half, p1d(ic,:), o31d(ic,:) )
  ENDDO

    !
    ! Reverse vertical order, convert to column mass concentration and effective radius. 
    !

    if ( present( f_qi ) .and. present( qi )  ) then 
      do kt=kts,kte
       DO ic=1,min(CHUNK,ite-ii+1)
         i = ii+ic-1
!         km=kme-kt+kms-1
         km=kte-kt+kts
         if( f_qi ) then
           if( radiation_skip ) then
             q1d(ic,kt,id_ice) =  ave(qi3d(i:ie(ic),km,j:je(ic))) * rho1d(ic,kt) * 1.e+3  ! mass concentration [g/m3]
            re1d(ic,kt,id_ice) =  avew(rei3d(i:ie(ic),km,j:je(ic)),qi3d(i:ie(ic),km,j:je(ic))) ! effective radius [um] 
           else
             q1d(ic,kt,id_ice) =  qi3d(i,km,j) * rho1d(ic,kt) * 1.e+3  ! mass concentration [g/m3]
            re1d(ic,kt,id_ice) = rei3d(i,km,j)                      ! effective radius [um] 
           endif
           if( NUWRF_DEBUG ) call check_reff('ice',q1d(ic,kt,id_ice),re1d(ic,kt,id_ice),i,j,kt)
         else
           q1d(ic,kt,id_ice) = 0.e0 
          re1d(ic,kt,id_ice) = 0.e0
         endif
       ENDDO
      enddo
    endif

    if ( present( f_qc ) .and. present( qc )  ) then 
      do kt=kts,kte
       DO ic=1,min(CHUNK,ite-ii+1)
         i = ii+ic-1
!         km=kme-kt+kms-1
         km=kte-kt+kts
         if( f_qc ) then
           if( radiation_skip ) then 
              q1d(ic,kt,id_cloud) = ave(qc3d(i:ie(ic),km,j:je(ic))) * rho1d(ic,kt) * 1.e+3  ! mass concentration [g/m3]
             re1d(ic,kt,id_cloud) = avew(rec3d(i:ie(ic),km,j:je(ic)),qc3d(i:ie(ic),km,j:je(ic))) ! effective radius [um] 
           else
              q1d(ic,kt,id_cloud) =  qc3d(i,km,j) * rho1d(ic,kt) * 1.e+3  ! mass concentration [g/m3]
             re1d(ic,kt,id_cloud) = rec3d(i,km,j)                      ! effective radius [um] 
           endif
           if( NUWRF_DEBUG ) call check_reff('cloud',q1d(ic,kt,id_cloud),re1d(ic,kt,id_cloud),i,j,kt)
         else
            q1d(ic,kt,id_cloud) = 0.e0
           re1d(ic,kt,id_cloud) = 0.e0
         endif
       ENDDO
      enddo
    endif

    if ( present( f_qr ) .and. present( qr )  ) then 
      do kt=kts,kte
       DO ic=1,min(CHUNK,ite-ii+1)
         i = ii+ic-1
!         km=kme-kt+kms-1
         km=kte-kt+kts
         if( f_qr ) then
           if( radiation_skip ) then
              q1d(ic,kt,id_rain) = ave(qr3d(i:ie(ic),km,j:je(ic))) * rho1d(ic,kt) * 1.e+3  ! mass concentration [g/m3]
             re1d(ic,kt,id_rain) = avew(rer3d(i:ie(ic),km,j:je(ic)),qr3d(i:ie(ic),km,j:je(ic))) ! effective radius [um] 
           else
              q1d(ic,kt,id_rain) =  qr3d(i,km,j) * rho1d(ic,kt) * 1.e+3  ! mass concentration [g/m3]
             re1d(ic,kt,id_rain) = rer3d(i,km,j)                      ! effective radius [um] 
           endif
           if( .not. opt_account_precip ) then
               q1d(ic,kt,id_rain) = 0.e0
              re1d(ic,kt,id_rain) = 0.e0
           endif
           if( NUWRF_DEBUG ) call check_reff('rain',q1d(ic,kt,id_rain),re1d(ic,kt,id_rain),i,j,kt)
         else
            q1d(ic,kt,id_rain) = 0.e0
           re1d(ic,kt,id_rain) = 0.e0
         endif
       ENDDO
      enddo
    endif

    if ( present( f_qs ) .and. present( qs )  ) then 
      do kt=kts,kte
       DO ic=1,min(CHUNK,ite-ii+1)
         i = ii+ic-1
!         km=kme-kt+kms-1
         km=kte-kt+kts
         if( f_qs ) then
           if( radiation_skip ) then
              q1d(ic,kt,id_snow) = ave(qs3d(i:ie(ic),km,j:je(ic))) * rho1d(ic,kt) * 1.e+3   ! mass concentration [g/m3]
             re1d(ic,kt,id_snow) = avew(res3d(i:ie(ic),km,j:je(ic)), qs3d(i:ie(ic),km,j:je(ic))) ! effective radius [um] 
           else
              q1d(ic,kt,id_snow) =  qs3d(i,km,j) * rho1d(ic,kt) * 1.e+3  ! mass concentration [g/m3]
             re1d(ic,kt,id_snow) = res3d(i,km,j)                      ! effective radius [um] 
           endif
           if( NUWRF_DEBUG ) call check_reff('snow',q1d(ic,kt,id_snow),re1d(ic,kt,id_snow),i,j,kt)
           if( .not. opt_account_precip ) then
               q1d(ic,kt,id_snow) = 0.e0
              re1d(ic,kt,id_snow) = 0.e0
           endif
         else
            q1d(ic,kt,id_snow) = 0.e0
           re1d(ic,kt,id_snow) = 0.e0
         endif
       ENDDO
      enddo
    endif

    if ( present( f_qg ) .and. present( qg )  ) then 
      do kt=kts,kte
       DO ic=1,min(CHUNK,ite-ii+1)
         i = ii+ic-1
!         km=kme-kt+kms-1
         km=kte-kt+kts
         if( f_qg ) then
           if( radiation_skip ) then
             q1d(ic,kt,id_graupel) = ave(qg3d(i:ie(ic),km,j:je(ic))) * rho1d(ic,kt) * 1.e+3  ! mass concentration [g/m3]
            re1d(ic,kt,id_graupel) = avew(reg3d(i:ie(ic),km,j:je(ic)),qg3d(i:ie(ic),km,j:je(ic))) ! effective radius [um] 
           else
             q1d(ic,kt,id_graupel) =  qg3d(i,km,j) * rho1d(ic,kt) * 1.e+3  ! mass concentration [g/m3]
            re1d(ic,kt,id_graupel) = reg3d(i,km,j)                      ! effective radius [um] 
           endif
           if( .not. opt_account_precip ) then
               q1d(ic,kt,id_graupel) = 0.e0
              re1d(ic,kt,id_graupel) = 0.e0
           endif
           if( NUWRF_DEBUG ) call check_reff('graupel',q1d(ic,kt,id_graupel),re1d(ic,kt,id_graupel),i,j,kt)
         else
           q1d(ic,kt,id_graupel) = 0.e0
          re1d(ic,kt,id_graupel) = 0.e0
         endif
       ENDDO
      enddo
    endif

    if ( present( f_qh ) .and. present( qh )  ) then 
      do kt=kts,kte
       DO ic=1,min(CHUNK,ite-ii+1)
         i = ii+ic-1
!         km=kme-kt+kms-1
         km=kte-kt+kts
         if( f_qh ) then
           if( radiation_skip ) then
             q1d(ic,kt,id_hail) = ave(qh3d(i:ie(ic),km,j:je(ic))) * rho1d(ic,kt) * 1.e+3  ! mass concentration [g/m3]
            re1d(ic,kt,id_hail) = avew(reh3d(i:ie(ic),km,j:je(ic)),qh3d(i:ie(ic),km,j:je(ic))) ! effective radius [um] 
           else
             q1d(ic,kt,id_hail) =  qh3d(i,km,j) * rho1d(ic,kt) * 1.e+3  ! mass concentration [g/m3]
            re1d(ic,kt,id_hail) = reh3d(i,km,j)                      ! effective radius [um]
           endif
           if( .not. opt_account_precip ) then
               q1d(ic,kt,id_hail) = 0.e0
              re1d(ic,kt,id_hail) = 0.e0
           endif
           if( NUWRF_DEBUG ) call check_reff('hail',q1d(ic,kt,id_hail),re1d(ic,kt,id_hail),i,j,kt)
         else
           q1d(ic,kt,id_hail) = 0.e0
          re1d(ic,kt,id_hail) = 0.e0
         endif
       ENDDO
      enddo
    endif


!
! vertical-level indices separating high, middle and low clouds.
! Needed for cloud overlapping and twostream_adding routines. 
!
   p400(:) = 1.e5
   p700(:) = 1.e5
   do kt = kts,kte
     DO ic=1,min(CHUNK,ite-ii+1)
        if (abs(p1d(ic,kt) - 400.) .lt. p400(ic)) then
            p400(ic) = abs(p1d(ic,kt) - 400.)
            ict(ic) = kt
        endif
        if (abs(p1d(ic,kt) - 700.) .lt. p700(ic)) then
            p700(ic) = abs(p1d(ic,kt) - 700.)
            icb(ic) = kt
        endif
     ENDDO
   end do



 rad_select: select case(sw_or_lw)

! SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW 
! SW SW SW SW SW SW SW SW  Shortwave scheme SW SW SW SW SW SW SW SW SW 
! SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW 
     
  case ('sw')

    !
    ! Derive cloud single-scattering properties
    ! 
      call opt_cloud( sw_or_lw, dk_half ,nband_sw, dz1d, q1d, &
          re1d, taucl_sw, ssacl_sw, asycl_sw, min(CHUNK,ite-ii+1) )

    !
    ! column cloud optical depth for visible (PAR) band
    ! 
    if( radiation_skip ) then
     DO ic=1,min(CHUNK,ite-ii+1)
      i = ii+ic -1
      cod2d_out(i:ie(ic),j:je(ic)) = sum(taucl_sw(ic,kts:kte,8)) 
     ENDDO
    else
     DO ic=1,min(CHUNK,ite-ii+1)
      i = ii+ic -1
      cod2d_out(i,j) = sum(taucl_sw(ic,kts:kte,8))
     ENDDO
    endif

    !
    ! cloud-top pressure (threshold is optical depth 0.01)
    !
    DO ic=1,min(CHUNK,ite-ii+1)
     i = ii+ic -1
     ctop_loop: do k = kts,kte
       if( taucl_sw(ic,k,8) >= 0.01 ) then  
          if( radiation_skip ) then
            ctop2d_out(i:ie(ic),j:je(ic)) = p1d(ic,k) !cloud-top pressure [mb]
          else
            ctop2d_out(i,j) = p1d(ic,k) !cloud-top pressure [mb]
          endif
          exit ctop_loop
       endif
     enddo ctop_loop
    ENDDO

!
! compute aerosol sigle-scattering properties.  
!
    DO ic=1,min(CHUNK,ite-ii+1)
     taual_sw(ic,:,:) = 0.
     ssaal_sw(ic,:,:) = 0.
     asyal_sw(ic,:,:) = 0.
    ENDDO

#if (WRF_CHEM == 1)

     if ( (chem_opt == 300 .or. chem_opt == 301 .or. &
           chem_opt == 302 .or. chem_opt == 303) .and. &
           (gsfcrad_gocart_coupling == 1) ) then

        if( radiation_skip ) then
          do kt=kts,kte   ! half level (kts = TOA, kte = BOA)
!             km=kme-kt+kms-1
             km=kte-kt+kts
             do n = 1, num_go
               DO ic=1,min(CHUNK,ite-ii+1)
                 i = ii+ic-1
                 aero1d(ic,kt,n)=ave(aero(i:ie(ic),km,j:je(ic),n))
               ENDDO
             enddo
          enddo
        else
          do kt=kts,kte   ! half level (kts = TOA, kte = BOA)
!             km=kme-kt+kms-1
             km=kte-kt+kts
             DO ic=1,min(CHUNK,ite-ii+1)
               i = ii+ic-1
               aero1d(ic,kt,:)=aero(i,km,j,:)
             ENDDO
          enddo
        endif
        !
        ! Derive aerosol optical properties in 1D manner (must be here to reduce memory size)
        !

        DO ic=1,min(CHUNK,ite-ii+1)
          call aero_opt(sw_or_lw, dk_half, nband_sw, p1d(ic,:), t1d(ic,:), sh1d(ic,:), &
             dz1d(ic,:), aero1d(ic,:,:), taual_sw(ic,:,:), ssaal_sw(ic,:,:), asyal_sw(ic,:,:))
        ENDDO

        !
        ! Output AOD profile at PAR band for diagnostic purpose
        !
        if( radiation_skip ) then
         DO ic=1,min(CHUNK,ite-ii+1)
          i = ii+ic-1
          aod2d_out(i:ie(ic),j:je(ic))  = sum(taual_sw(ic,:,8))   ! 8 is PAR band (0.400-0.700 micron band)
         ENDDO
        else
         DO ic=1,min(CHUNK,ite-ii+1)
          i = ii+ic-1
          aod2d_out(i,j)  = sum(taual_sw(ic,:,8))   ! 8 is PAR band (0.400-0.700 micron band)
         ENDDO
        endif

        !
        ! Output aerosol-top pressure (useful for PBL)
        !
        DO ic=1,min(CHUNK,ite-ii+1)
         i = ii+ic-1
         atop_loop: do k = kts,kte
          if( taual_sw(ic,k,8) >= 0.01 ) then
            if( radiation_skip ) then
              atop2d_out(i:ie(ic),j:je(ic)) = p1d(ic,k) ! aerosol-top pressure [mb]
            else
              atop2d_out(i,j) = p1d(ic,k)       ! aerosol-top pressure [mb]
            endif
            exit atop_loop
          endif
         enddo atop_loop
        ENDDO

     else
      DO ic=1,min(CHUNK,ite-ii+1)
       i = ii+ic-1
       if( radiation_skip ) then
         aod2d_out(i:ie(ic),j:je(ic)) = 0.e0
         atop2d_out(i:ie(ic),j:je(ic)) = -999.e0
       else
         aod2d_out(i,j) = 0.e0
         atop2d_out(i,j) = -999.e0
       endif
      ENDDO
     endif


#endif

!
! if SW computation and nighttime skip all process. (new optimization)
!

    if (present(coszen)) then ! jararias, 14/08/2013
     DO ic=1,min(CHUNK,ite-ii+1)
      i = ii+ic-1
!      call wrf_debug(100,'using coszen from radiation driver')
      if( radiation_skip ) then
         cosz(ic)=ave(coszen(i:ie(ic),j:je(ic)))
      else
         cosz(ic)=coszen(i,j)
      endif
     ENDDO
    else

      !
      ! estimate cosin of solar zenith angle
      !
      DO ic=1,min(CHUNK,ite-ii+1)
       i = ii+ic-1
       if( radiation_skip ) then
         xt24 = mod(xtime + radfrq * 0.5e0, 1440.e0)
         tloctm = gmt + xt24 / 60.e0 + ave(xlong(i:ie(ic),j:je(ic))) / 15.e0
         hrang = 15.e0 * (tloctm - 12.e0) * degrad
         xxlat = ave(xlat(i:ie(ic),j:je(ic))) * degrad
         cosz(ic) = sin(xxlat) * sin(declin) + &
               cos(xxlat) * cos(declin) * cos(hrang)  !cosine of solar zenith angle [-]
       else
         xt24 = mod(xtime + radfrq * 0.5e0, 1440.e0)
         tloctm = gmt + xt24 / 60.e0 + xlong(i,j) / 15.e0
         hrang = 15.e0 * (tloctm - 12.e0) * degrad
         xxlat = xlat(i,j) * degrad
         cosz(ic) = sin(xxlat) * sin(declin) + &
               cos(xxlat) * cos(declin) * cos(hrang)  !cosine of solar zenith angle [-]

       endif
      ENDDO
    endif


!
! 1-dimension driver of shortwave radiative transfer scheme (all double precision)
! (only pressure is full level, other are half layer.)
!

    no_micro = .false.
    DO ic=1,min(CHUNK,ite-ii+1)
      i = ii+ic-1
      if (cosz(ic) <= cosz_min ) then !night time skip 

        !
        ! No update of theta tendancy (rthraten) and energy budget due to solar heating
        ! Note skip is located here in order to compute cloud/aerosol optical depth for 
        ! diagnostic output even during night. 
        !
        if( radiation_skip ) then 
          rthraten(i:ie(ic),k,j:je(ic))=ave(rthraten(i:ie(ic),k,j:je(ic)))  !no update of SW heating rate. 
          gsf(i:ie(ic),j:je(ic))       = 0.e0    !surface SW abs is zero
          if(present(sflxd)) sflxd(i:ie(ic),j:je(ic),1:4) = 0.e0    ! downward SW flux are all zero.
          if(present(swddni)) swddni(i:ie(ic),j:je(ic))    = 0.e0    !total SW direct radiation at normal direction [W/m2]
          if(present(swddir)) swddir(i:ie(ic),j:je(ic))    = 0.e0    !total SW direct radiation [W/m2]    
          if(present(swddif)) swddif(i:ie(ic),j:je(ic))    = 0.e0    !total SW diffuse radiation [W/m2]
          ERBE_out(i:ie(ic),j:je(ic),5:8) = 0.e0 !SW energy budget are all zeo. 
        else
          rthraten(i,k,j)=rthraten(i,k,j)  !no update of SW heating rate. 
          gsf(i,j)       = 0.e0    !surface SW abs is zero
          if(present(sflxd)) sflxd(i,j,1:4) = 0.e0    ! downward SW flux are all zero.
          if(present(swddni)) swddni(i,j)    = 0.e0    !total SW direct radiation at normal direction [W/m2]
          if(present(swddir)) swddir(i,j)    = 0.e0    !total SW direct radiation [W/m2]    
          if(present(swddif)) swddif(i,j)    = 0.e0    !total SW diffuse radiation [W/m2]
          ERBE_out(i,j,5:8) = 0.e0 !SW energy budget are all zeo. 
        endif
        no_micro(ic) = .true.

      endif
    ENDDO

    if (ALL(no_micro(1:min(CHUNK,ite-ii+1)))) cycle
    lmask = .true.
    lmask = lmask .and. (.not. no_micro)

!
! surface spectrum albedo for direct and diffuse radiation 
! (T. Matsui-> this should be modified to account for spectrum albedo. )
! For example Seller's canopy albedo model gives rsuvbm, rsuvdf, rsirbm, rsirdf
! independently. 
!
    DO ic=1,min(CHUNK,ite-ii+1)
    if(lmask(ic) .eqv. .true.) then
       i = ii+ic-1
       if( radiation_skip ) then 
         rsuvbm(ic) = ave(alb(i:ie(ic),j:je(ic)))
         rsuvdf(ic) = ave(alb(i:ie(ic),j:je(ic)))
         rsirbm(ic) = ave(alb(i:ie(ic),j:je(ic)))
         rsirdf(ic) = ave(alb(i:ie(ic),j:je(ic)))
       else
         rsuvbm(ic) = alb(i,j)
         rsuvdf(ic) = alb(i,j)
         rsirbm(ic) = alb(i,j)
         rsirdf(ic) = alb(i,j)
       endif
    endif
    ENDDO
!
! drive shortwave (solar) radiative transfer (1D driver)
!
      DO ic=1,min(CHUNK,ite-ii+1)
      if(lmask(ic) .eqv. .true.) then
        flx(ic,:)=0. ; flxd(ic,:)=0. ; flxu(ic,:)=0.
      endif
      ENDDO


#if (RWORDSIZE == 4)
      call swrad ( np=dk_half, icb=icb, ict=ict, fcld=dble(fcld1d), &
                   pl=dble(p8w1d), ta=dble(t1d), wa=dble(sh1d), oa=dble(o31d), &
                   taucl=dble(taucl_sw), ssacl=dble(ssacl_sw), asycl=dble(asycl_sw), &
                   taual=dble(taual_sw), ssaal=dble(ssaal_sw), asyal=dble(asyal_sw), &
                   cosz=dble(cosz), rsuvbm=dble(rsuvbm), rsuvdf=dble(rsuvdf), rsirbm=dble(rsirbm), rsirdf=dble(rsirdf),&
                   flx_out=flx, flxd_out=flxd,flxu_out=flxu, flxd_surf = flxd_surf, lmask=lmask, irestrict=min(CHUNK,ite-ii+1) )
#elif (RWORDSIZE == 8)
      call swrad ( np=dk_half, icb=icb, ict=ict, fcld=fcld1d, &
                   pl=p8w1d, ta=t1d, wa=sh1d, oa=o31d, &
                   taucl=taucl_sw, ssacl=ssacl_sw, asycl=asycl_sw, &
                   taual=taual_sw, ssaal=ssaal_sw, asyal=asyal_sw, &
                   cosz=cosz, rsuvbm=rsuvbm, rsuvdf=rsuvdf, rsirbm=rsirbm, rsirdf=rsirdf,&
                   flx_out=flx, flxd_out=flxd,flxu_out=flxu, flxd_surf = flxd_surf, lmask=lmask, irestrict=min(CHUNK,ite-ii+1) )
#endif

!
! convert the units of flx from flux fraction to W/m2 (full level)
!
      do kt=kts-1,kte !full level ( kts -1 = TOA, kte = BOA )
        DO ic=1,min(CHUNK,ite-ii+1)
        if(lmask(ic) .eqv. .true.) then
         flx(ic,kt) = flx(ic,kt) * solcon * cosz(ic)
        endif
        ENDDO
      enddo
 
!
! calculate heating rate (deg/sec)
!
      fac = .01 * g / cp

      do kt=kts,kte !half level ( kts = TOA, kte = BOA )
        DO ic=1,min(CHUNK,ite-ii+1)
        if(lmask(ic) .eqv. .true.) then
         i = ii+ic -1
         tten1d(ic,kt) = - fac * (flx(ic,kt-1) - flx(ic,kt)) / (p8w1d(ic,kt-1)-p8w1d(ic,kt))

#ifndef NO_IEEE_MODULE
         ! check error
         if( ieee_is_nan( tten1d(ic,kt) ) ) then
            print*,'MSG goddardrad SW: Found NaN in tten1d(k)',i,j,kt
            print*,'p8w1d',p8w1d(ic,:)
            print*,'t1d',t1d(ic,:)
            print*,'sh1d',sh1d(ic,:)
            print*,'o31d',o31d(ic,:)
            print*,'q1d',q1d(ic,:,:)
            print*,'emis1d',emis1d(ic,:)
            print*,'re1d',re1d(ic,:,:)
            print*,'taual_sw',taual_sw(ic,:,:)
            print*,'ssaal_sw',ssaal_sw(ic,:,:)
            print*,'asyal_sw',asyal_sw(ic,:,:)
            tten1d(ic,kt) = 0.  !brute force correction
         endif
#endif
        endif
        ENDDO
      enddo

!
! Update theta tendency due to radiative heating (K/sec)
!
      do kt=kts,kte  !half level
       DO ic=1,min(CHUNK,ite-ii+1)
       if(lmask(ic) .eqv. .true.) then
        i = ii+ic -1
!         km=kme-kt+kms-1
         km=kte-kt+kts

         if(tten1d(ic,kt) < 0. ) then
            print*,'MSG goddardrad : WARNING Negative SW heating =',&
                   tten1d(ic,kt)/pi3d(i,km,j)*3600.*24.,'[K/day] at point ikj',i,km,j
            print*,'cosz=',cosz(ic)
            tten1d(ic,kt) = 0. !brute force correction
         endif

         if( radiation_skip ) then
           rthraten(i:ie(ic),km,j:je(ic))=ave(rthraten(i:ie(ic),km,j:je(ic))) &
                          +tten1d(ic,kt)/ave(pi3d(i:ie(ic),km,j:je(ic)))  !update heating ratio [K/s]
         else
           rthraten(i,km,j)=rthraten(i,km,j)+tten1d(ic,kt)/pi3d(i,km,j)  !update heating ratio [K/s]
         endif
       endif
       ENDDO
      enddo

 if( radiation_skip ) then

!
! absorbed part in surface energy budget [W/m^2] (Note that flxd in SW is flux fraction)
!
     DO ic=1,min(CHUNK,ite-ii+1)
     if(lmask(ic) .eqv. .true.) then
       i = ii+ic-1
       gsf(i:ie(ic),j:je(ic)) = (1. - rsuvbm(ic)) * flxd(ic,kte) * solcon * cosz(ic)
     endif
     ENDDO

!
! More detailed surface flux for Seller's canoppy radiative transfer 
!
     do n = 1, 4
       DO ic=1,min(CHUNK,ite-ii+1)
       if(lmask(ic) .eqv. .true.) then
         i = ii+ic-1
         if(present(sflxd)) sflxd(i:ie(ic),j:je(ic),n) = flxd_surf(ic,n) * solcon * cosz(ic)  !surface dir/dif PAR/NIR downward radiation [W/m2]
       endif
       ENDDO
     enddo
!
! Extra output for NCAR WRF V3.5.1 (modified)
!
     DO ic=1,min(CHUNK,ite-ii+1)
     if(lmask(ic) .eqv. .true.) then
       i = ii+ic-1
       if(present(swddni)) swddni(i:ie(ic),j:je(ic)) = (flxd_surf(ic,1)+flxd_surf(ic,3)) * solcon !total SW direct radiation at normal direction [W/m2]
       if(present(swddir)) swddir(i:ie(ic),j:je(ic)) = swddni(i,j) * cosz(ic)                   !total SW direct radiation [W/m2]    
       if(present(swddif)) swddif(i:ie(ic),j:je(ic)) = (flxd_surf(ic,2)+flxd_surf(ic,4)) * solcon * cosz(ic) !total SW diffuse radiation [W/m2]

!
! extra output for earth radiation budget
!       
       ERBE_out(i:ie(ic),j:je(ic),5) = flxd(ic,kts-1) * solcon * cosz(ic)  ! TOA SW downwelling flux [W/m2] 
       ERBE_out(i:ie(ic),j:je(ic),6) = flxu(ic,kts-1) * solcon * cosz(ic)  ! TOA SW upwelling flux   [W/m2]
       ERBE_out(i:ie(ic),j:je(ic),7) = flxd(ic,kte)   * solcon * cosz(ic)  ! surface SW downwelling flux [W/m2]
       ERBE_out(i:ie(ic),j:je(ic),8) = flxu(ic,kte)   * solcon * cosz(ic)  ! surface SW upwelling flux   [W/m2]
     endif
     ENDDO

 else

!
! absorbed part in surface energy budget [W/m^2] (Note that flxd in SW is flux fraction)
!
     DO ic=1,min(CHUNK,ite-ii+1)
     if(lmask(ic) .eqv. .true.) then
       i = ii+ic-1

       gsf(i,j) = (1. - rsuvbm(ic)) * flxd(ic,kte) * solcon * cosz(ic)

!
! More detailed surface flux for Seller's canoppy radiative transfer 
!
       if(present(sflxd)) sflxd(i,j,1:4) = flxd_surf(ic,1:4) * solcon * cosz(ic)  !surface dir/dif PAR/NIR downward radiation [W/m2]

!
! Extra output for NCAR WRF V3.5.1 (modified)
!
       if(present(swddni)) swddni(i,j) = (flxd_surf(ic,1)+flxd_surf(ic,3)) * solcon !total SW direct radiation at normal direction [W/m2]
       if(present(swddir)) swddir(i,j) = swddni(i,j) * cosz(ic)                   !total SW direct radiation [W/m2]    
       if(present(swddif)) swddif(i,j) = (flxd_surf(ic,2)+flxd_surf(ic,4)) * solcon * cosz(ic) !total SW diffuse radiation [W/m2]

!
! extra output for earth radiation budget
!       
       ERBE_out(i,j,5) = flxd(ic,kts-1) * solcon * cosz(ic)  ! TOA SW downwelling flux [W/m2] 
       ERBE_out(i,j,6) = flxu(ic,kts-1) * solcon * cosz(ic)  ! TOA SW upwelling flux   [W/m2]
       ERBE_out(i,j,7) = flxd(ic,kte)   * solcon * cosz(ic)  ! surface SW downwelling flux [W/m2]
       ERBE_out(i,j,8) = flxu(ic,kte)   * solcon * cosz(ic)  ! surface SW upwelling flux   [W/m2]
     endif
     ENDDO

 endif

! LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW 
! LW LW LW LW LW LW LW LW  Longwave scheme  LW LW LW LW LW LW LW LW LW 
! LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW 

  case ('lw')


!
! compute condensate single-scattering properties. 
!
      call opt_cloud( sw_or_lw, dk_half ,nband_lw, dz1d, q1d, & 
       re1d, taucl_lw, ssacl_lw, asycl_lw, min(CHUNK,ite-ii+1) )

!
! initialize aerosol LW optical properties 
!
     taual_lw = 0.
     ssaal_lw = 0.
     asyal_lw = 0.

#if (WRF_CHEM == 1)

     if ( (chem_opt == 300 .or. chem_opt == 301 .or. &
           chem_opt == 302 .or. chem_opt == 303) .and. &
           (gsfcrad_gocart_coupling == 1) ) then

        if( radiation_skip ) then
          do kt=kts,kte   ! half level (kts = TOA, kte = BOA)
!             km=kme-kt+kms-1
             km=kte-kt+kts
             do n = 1, num_go
               DO ic=1,min(CHUNK,ite-ii+1)
                 i = ii+ic-1
                 aero1d(ic,kt,n)=ave(aero(i:ie(ic),km,j:je(ic),n))
               ENDDO
             enddo
          enddo
        else
          do kt=kts,kte   ! half level (kts = TOA, kte = BOA)
!             km=kme-kt+kms-1
             km=kte-kt+kts
             DO ic=1,min(CHUNK,ite-ii+1)
               i = ii+ic-1
               aero1d(ic,kt,:)=aero(i,km,j,:)
             ENDDO
          enddo
        endif

        !
        ! Derive aerosol LW optical properties in 1D manner (must be here to reduce memory size)
        !
        DO ic=1,min(CHUNK,ite-ii+1)
          call aero_opt(sw_or_lw, dk_half, nband_lw, p1d(ic,:), t1d(ic,:), sh1d(ic,:), &
             dz1d(ic,:), aero1d(ic,:,:), taual_lw(ic,:,:), ssaal_lw(ic,:,:), asyal_lw(ic,:,:))
        ENDDO

     else
        do kt=kts,kte   ! half level (kts = TOA, kte = BOA)    
!           km=kme-kt+kms-1
           km=kte-kt+kts
           DO ic=1,min(CHUNK,ite-ii+1)
             aero1d(ic,kt,:)=0
           ENDDO
        enddo
     end if

#endif

!
! surface parameters
!
     DO ic=1,min(CHUNK,ite-ii+1)
       i = ii+ic -1
       tsfc(ic)  = t8w1d(ic,kte)   !surface-layer air temperature [K]
       if( radiation_skip ) then
         tskin(ic) = ave(tsk(i:ie(ic),j:je(ic)))     !surface skin temperature [K]
         emis1d(ic,1:nband_lw) = ave(emiss(i:ie(ic),j:je(ic))) !(Toshi- this should be modified to account for spectrum emissivity)
       else
         tskin(ic) = tsk(i,j)     !surface skin temperature [K]
         emis1d(ic,1:nband_lw) = emiss(i,j)   !(Toshi- this should be modified to account for spectrum emissivity)
       endif
     ENDDO
!
! 1-dimension driver of longwave radiative transfer scheme
!

#if (RWORDSIZE == 4)
       call lwrad ( np=dk_half, tb=dble(tsfc), ts=dble(tskin), ict=ict, icb=icb,&
                    pl=dble(p8w1d), ta=dble(t1d), wa=dble(sh1d), oa=dble(o31d), &
                    emiss=dble(emis1d), fcld=dble(fcld1d), &
                    taucl=dble(taucl_lw), ssacl=dble(ssacl_lw), asycl=dble(asycl_lw),  &
                    taual=dble(taual_lw), ssaal=dble(ssaal_lw), asyal=dble(asyal_lw),  &
                    flx_out=flx, acflxd_out=flxd, acflxu_out=flxu, irestrict=min(CHUNK,ite-ii+1) )
#elif (RWORDSIZE == 8)
       call lwrad ( np=dk_half, tb=tsfc, ts=tskin, ict=ict, icb=icb,&
                    pl=p8w1d, ta=t1d, wa=sh1d, oa=o31d, &
                    emiss=emis1d, fcld=fcld1d, &
                    taucl=taucl_lw, ssacl=ssacl_lw, asycl=asycl_lw,  &
                    taual=taual_lw, ssaal=ssaal_lw, asyal=asyal_lw,  &
                    flx_out=flx, acflxd_out=flxd, acflxu_out=flxu, irestrict=min(CHUNK,ite-ii+1) )
#endif

!
! calculate heating rate (deg/sec)
!
     fac = .01 * g / cp

     do kt=kts,kte !half level ( kts = TOA, kte = BOA )
       DO ic=1,min(CHUNK,ite-ii+1)
        i = ii+ic -1
        tten1d(ic,kt) = - fac * (flx(ic,kt-1) - flx(ic,kt)) / (p8w1d(ic,kt-1)-p8w1d(ic,kt))

#ifndef NO_IEEE_MODULE
        if( ieee_is_nan( tten1d(ic,kt) ) ) then
            print*,'MSG goddardrad LW: Found NaN in tten1d(k)',i,j,kt
            print*,'tsfc',tsfc(ic)
            print*,'tskin',tskin(ic)
            print*,'p8w1d',p8w1d(ic,:)
            print*,'t1d',t1d(ic,:)
            print*,'sh1d',sh1d(ic,:)
            print*,'o31d',o31d(ic,:)
            print*,'q1d',q1d(ic,:,:)
            print*,'emis1d',emis1d(ic,:)
            print*,'re1d',re1d(ic,:,:)
            print*,'taual_lw',taual_lw(ic,:,:)
            print*,'ssaal_lw',ssaal_lw(ic,:,:)
            print*,'asyal_lw',asyal_lw(ic,:,:)
            tten1d(ic,k) = 0.  !brute force correction
        endif
#endif
       ENDDO
     enddo

!
! get theta tendency due to radiative heating (K/sec)
!
     do kt=kts,kte  !half level
      DO ic=1,min(CHUNK,ite-ii+1)
        i = ii+ic -1
!        km=kme-kt+kms-1
        km=kte-kt+kts
        if( radiation_skip ) then
          rthraten(i:ie(ic),km,j:je(ic))=ave(rthraten(i:ie(ic),km,j:je(ic)))&
                                 +tten1d(ic,kt)/ave(pi3d(i:ie(ic),km,j:je(ic))) !update heating ratio [K/s]
        else
          rthraten(i,km,j)=rthraten(i,km,j)+tten1d(ic,kt)/pi3d(i,km,j)  !update heating ratio [K/s]
        endif
       ENDDO
     enddo

   if( radiation_skip ) then

!
! downwelling/upwelling longwave radiation [W/m^2] (Note that flxd/flxu of LW is actual flux [W/m2])
!
     DO ic=1,min(CHUNK,ite-ii+1)
       i = ii+ic -1
       gsf(i:ie(ic),j:je(ic)) = flxd(ic,kte)  !surface downwelling LW radiation

!
! extra output for energy budget
!       
       ERBE_out(i:ie(ic),j:je(ic),1) = flxd(ic,kts-1)  ! TOA LW downwelling flux [W/m2] 
       ERBE_out(i:ie(ic),j:je(ic),2) = flxu(ic,kts-1)  ! TOA LW upwelling flux   [W/m2]
       ERBE_out(i:ie(ic),j:je(ic),3) = flxd(ic,kte)    ! surface LW downwelling flux [W/m2]
       ERBE_out(i:ie(ic),j:je(ic),4) = flxu(ic,kte)    ! surface LW upwelling flux   [W/m2]
     ENDDO

   else
!
! downwelling/upwelling longwave radiation [W/m^2] (Note that flxd/flxu of LW is actual flux [W/m2])
!
     DO ic=1,min(CHUNK,ite-ii+1)
       i = ii+ic -1
       gsf(i,j) = flxd(ic,kte)  !surface downwelling LW radiation

!
! extra output for energy budget
!       
       ERBE_out(i,j,1) = flxd(ic,kts-1)  ! TOA LW downwelling flux [W/m2] 
       ERBE_out(i,j,2) = flxu(ic,kts-1)  ! TOA LW upwelling flux   [W/m2]
       ERBE_out(i,j,3) = flxd(ic,kte)    ! surface LW downwelling flux [W/m2]
       ERBE_out(i,j,4) = flxu(ic,kte)    ! surface LW upwelling flux   [W/m2]
    ENDDO
  endif

  case default
      call wrf_error_fatal('MSG: goddardrad: option does not exist. check sw_or_lw ')
  end select rad_select

 ENDIF
 ENDDO

 end subroutine goddardrad

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine sounding_interp(lat,julday, mcdat_int)
 implicit none
!----------------------------------------------------------------------
! Purpose: This routines interpolate sounding climatology for a given 
!          julian date and latitude.  
!
! History:
! 09/2015  Toshi Matsui@NASA GSFC : mcdat_int is io parameter now.
! 06/2008  Toshi Matsui@NASA GSFC ; Initial 
!-----------------------------------------------------------------------
 integer,intent(in) :: julday         ! julian day
 real,intent(in) :: lat ! latitude (deg)
 real,dimension(ilev_max,6),intent(out)    :: mcdat_int  !interpolated Mclatchy Sounding climatology

!
! sounding related
!
  integer,parameter :: jul_winter = 30  !around end of Jan
  integer,parameter :: jul_summer = 212 !around end of Jun
  real :: wgt_winter !winter weight 
  real :: wgt_summer !summer weight
  real :: wgt_north  !latitudinal weight
  real :: wgt_south  !latitudinal weight
  integer :: juld    !julidan date
  integer :: inorth,isouth !latitudinal indice
  real :: xlat       !latitude
  real :: lat_north,lat_south !
  real,dimension(ilev_max,6)   :: mcdat_north,mcdat_south !interpolated mcdat


 wgt_winter = 0. ; wgt_summer = 0. 
 wgt_north  = 0. ; wgt_south  = 0. 

!
! Determine summer-winter weight based on julian day
!
 juld = julday

   if(juld < jul_winter) then
      wgt_summer = REAL(jul_winter - juld) / 183.
      wgt_winter = 1.-wgt_summer
   elseif(juld >= jul_winter .and. juld <= jul_summer) then
      wgt_summer = REAL(juld-jul_winter) / 183.
      wgt_winter = 1.- wgt_summer
   elseif(juld > jul_summer) then
      wgt_winter = REAL(juld-jul_summer) / 183.
      wgt_summer = 1.- wgt_winter
   endif

 if(lat < 0.) then !southern hemisphere (flip summer and winter weights)
    wgt_summer = wgt_winter
    wgt_winter = 1.-wgt_summer
 endif

!
! Latitudinal (climatology index) weight
!
   xlat = abs(lat)

   inorth = 2*INT((90.-xlat)/22.5) + 1
   isouth = 2*INT((90.-xlat)/22.5) + 3

   mcdat_north(:,:) = wgt_summer*mcdat(:,inorth,:) + wgt_winter*mcdat(:,inorth+1,:)
   mcdat_south(:,:) = wgt_summer*mcdat(:,isouth,:) + wgt_winter*mcdat(:,isouth+1,:)

   lat_north = 90.- REAL((inorth-1)/2)*22.5
   lat_south = lat_north - 22.5


   wgt_north = (xlat - lat_south) / 22.5
   wgt_south = 1.- wgt_north

!
! Interpolated sounding for a given latitude and julidan date 
! mcdat_int will be used in subroutines ozone_interp and sounding_strat.
!
   mcdat_int(:,:) = wgt_north*mcdat_north(:,:) + wgt_south*mcdat_south(:,:)  !this is module parameter

 end subroutine sounding_interp

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine ozone_interp( mcdat_int, mlev, pr, o3 )
 implicit none
!----------------------------------------------------------------------
! Comments: 
! This routines interpolate ozone mixing ratio for a givne pressure profile 
! using sounding climatology. 
!
! History:
! 09/2015  Toshi Matsui@NASA GSFC : mcdat_int is io parameter now.
! 03/2009  Toshi Matsui@NASA GSFC : bug in kk_loop was fixed.
! 04/2008  Toshi Matsui@NASA GSFC ; Initial 
!-----------------------------------------------------------------------
 real,dimension(ilev_max,6),intent(in) :: mcdat_int  !interpolated Mclatchy Sounding climatology
 integer,intent(in) :: mlev     ! vertical level
 real,intent(in)  :: pr(mlev)   ! WRF pressure (mb or hPa)
 real,intent(out) :: o3(mlev)   ! ozone mixing ratio (kg/kg)

 integer :: k,kk !looping
 real :: wgt       !interpolation weight
 real,dimension(mlev,1:6) :: mcdat_wrf

 logical :: found_pr  ! EMK TEST

!
! Interpolate sounding ozone value for a given pressure  mcdat_int(:,2) is pressure in Pa
!

  do k = 1,mlev !model level

     found_pr = .false.

     if    ( pr(k)*100. <= mcdat_int(ilev_max,2) ) then  
           mcdat_wrf(k,1:6) = mcdat_int(ilev_max,1:6)   !use highest level
           found_pr = .true. ! EMK TEST

     elseif( pr(k)*100. >= mcdat_int(1       ,2) ) then 
           mcdat_wrf(k,1:6) = mcdat_int(1,1:6)          !use lowest level
           found_pr = .true. ! EMK TEST
     else

        kk_loop : do kk = ilev_max-1, 1 , -1 !mcdat level loop
           if(pr(k)*100. >= mcdat_int(kk+1,2) .and. pr(k)*100. <= mcdat_int(kk,2) ) then
              wgt = ( pr(k)*100 - mcdat_int(kk+1,2) ) / ( mcdat_int(kk,2) - mcdat_int(kk+1,2) )
              mcdat_wrf(k,1:6) = mcdat_int(kk,1:6)*wgt + mcdat_int(kk+1,1:6)*(1.-wgt)
              found_pr = .true. ! EMK TEST
              exit kk_loop
           endif
        enddo kk_loop
     endif

   ! 
   ! EMK TEST
   !
   if (.not. found_pr) then
      print*,'EMK:  Warning: Could not find wrf level in ozone sounding.'
      print*,'EMK:  k, pr(k)*100 = ',k,pr(k)*100
      print*,'EMK:  mcdat_wrf(k,6) = ',mcdat_wrf(k,6)
      print*,'EMK:  pr(:)*100 = ',pr(:)*100
      print*,'EMK:  mcdat_int(:,2) = ',mcdat_int(:,2)
      call wrf_error_fatal('ERROR interpolating ozone in Goddard radiation')
   end if

!
! interpolated ozone mixing ratio for WRF layer (mcdat_wrf(k,6) is dry air density)
!

   o3(k)  = mcdat_wrf(k,5) / mcdat_wrf(k,6)  ! ozone mixing ratio [kg/kg] or [g/g]


 enddo

 end subroutine ozone_interp

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine sounding_strat(mcdat_int, mlev, pres, tmp, sh, o3 ) 
 implicit none
!----------------------------------------------------------------------
! Comments: 
! This routines interpolate temperature, humidity, and ozone mixing ration 
! for additional stratospheric layers. 
! This interpolation is a bit time comsuming, but create smooth transition of 
! stratospheric temperature for large-domain simulation. (Toshi)
!
! History:
! 09/2015  Toshi Matsui@NASA GSFC : mcdat_int is io parameter now.
! 03/2009  Toshi Matsui@NASA GSFC : bug in kk_loop was fixed.
! 04/2008  Toshi Matsui@NASA GSFC ; Initial 
!-----------------------------------------------------------------------
 real,dimension(ilev_max,6),intent(in) :: mcdat_int  !interpolated Mclatchy Sounding climatology
 integer,intent(in) :: mlev !vertical level
 real,intent(in)  :: pres(mlev) !pressure (mb or hPa)
 real,intent(out) :: tmp(mlev),sh(mlev),o3(mlev)  ! temperature, specific humidity, ozone for 
                                                  ! additional stratosphere layers

 integer :: k,kk !looping
 real :: wgt
 real,dimension(mlev,1:6) :: mcdat_strat

!
! Interpolate sounding value for a given stratosphere pressure 
!
  do k = 1,mlev !stratosphere model leve

     if(pres(k)*100. <= mcdat_int(ilev_max,2) ) then
         print*, 'model pressure is ',pres(k)*100. ,'(mb)', &
                 'McClatch sounding pressure is ',mcdat_int(ilev_max,2),'(mb)' 
!         stop 'MSG Goddard Radiation Subroutine sounding; reduce the number of alev_strat'
         call wrf_error_fatal('MSG Goddard Radiation Subroutine sounding; reduce the number of alev_strat')
     endif

     kk_loop : do kk = ilev_max-1, 1 , -1 !mcdat level loop
        if(pres(k)*100. >= mcdat_int(kk+1,2) .and. pres(k)*100. <= mcdat_int(kk,2) ) then
           wgt = ( pres(k)*100 - mcdat_int(kk+1,2) ) / ( mcdat_int(kk,2) - mcdat_int(kk+1,2) )
           mcdat_strat(k,1:6) = mcdat_int(kk,1:6)*wgt + mcdat_int(kk+1,1:6)*(1.-wgt) 
           exit kk_loop
        endif
     enddo kk_loop

!
! get parameters profiles for stratosphere
!
   tmp(k) = mcdat_strat(k,3)                     ! temperature [K]
   sh(k)  = mcdat_strat(k,4) / mcdat_strat(k,6)  ! specific humidity [kg/kg]
   o3(k)  = mcdat_strat(k,5) / mcdat_strat(k,6)  ! ozone mixing ratio [kg/kg]

 enddo 

 end subroutine sounding_strat

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD

 subroutine mie_lut_broad( nband, q , re, kext_lut, salb_lut, asym_lut, pts_ref, mxpts_ref, &
                           kext, salb, asym )
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! This subroutine find the extinction/scattering coefficients for
! given Re using Mie LUTs (per unit q), 
! and optal values are re-scaled to actual q (hydrometeor content).
! 
! History:
!   05/2009  Toshi Matsui@NASA GSFC ; Initial     
!           
! References: 
!---------------------------------------------------------------------------------------------------
 integer,intent(in) :: nband   !total band number
 real,intent(in) :: q  ! particle mass mixing ratio [g/m3]
 real :: re            ! particle effective radius [micron]
 real,intent(in) :: kext_lut(mxpts_ref,nband) ! extinction coefficient per unit mass [km-1 / (g/m3) ]
 real,intent(in) :: salb_lut(mxpts_ref,nband) ! single scattering albedo [-]
 real,intent(in) :: asym_lut(mxpts_ref,nband) ! asymetry parameter [-]
 real,intent(in) :: pts_ref(mxpts_ref)  ! LUT of re index [micron]
 integer,intent(in) :: mxpts_ref           ! dimension of re index
 real,intent(out) :: kext(nband)   ! extinction coefficient [km-1] 
 real,intent(out) :: salb(nband)   ! single scattering albedo [-]
 real,intent(out) :: asym(nband)   ! asymetry parameter [-]

!
! local parameter
!
 integer :: j !loop index
 real :: kext_q_unit(nband)   ! extinction coefficient per unit mass [km-1 / (g/m3) ]
 real :: wgt1,wgt2

!
!initialise
!
 kext = 0.d0 ; salb = 0.d0 ; asym = 0.d0

 if(q <= q_min_condensate) return

 if(re < pts_ref(1)        ) re = pts_ref(1)
 if(re > pts_ref(mxpts_ref)) re = pts_ref(mxpts_ref)

!
! Linear interpolation
!
 REF_LOOP: do j = 1,mxpts_ref-1

  if( re >= pts_ref(j) .and. re <= pts_ref(j+1) ) then
      wgt2 =  ( re - pts_ref(j) )    / ( pts_ref(j+1) - pts_ref(j) )
      wgt1 = 1.0 - wgt2

      kext_q_unit(:) = wgt1*kext_lut(j,:) + wgt2*kext_lut(j+1,:)
      salb(:)        = wgt1*salb_lut(j,:) + wgt2*salb_lut(j+1,:)
      asym(:)        = wgt1*asym_lut(j,:) + wgt2*asym_lut(j+1,:)

      exit REF_LOOP
  endif

 enddo REF_LOOP

!
! Re-scale kext from unit mass mixing ratio (1[g/m3]) to actual mass mixing ratio (q [g/m3])
! 
  kext(:) = kext_q_unit(:) * q

 end subroutine mie_lut_broad

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD

 subroutine init_gen(param,proc)
 implicit none
 type( particle_gen ) ,intent(inout) :: param
 character(len=*),intent(in) :: proc

 select case(trim(proc))
 case('zero')
   param%cloud    =0.0
   param%rain     =0.0
   param%ice      =0.0
   param%snow     =0.0
   param%graupel  =0.0
   param%hail     =0.0
 case('nan')

 case('inf')

 end select

 end subroutine init_gen

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD

 subroutine init_gocart(param,proc)
 implicit none
 type( particle_gocart ) ,intent(inout) :: param
 character(len=*),intent(in) :: proc

 select case(trim(proc))
 case('zero')
  param%so4  = 0.0
  param%blc  = 0.0
  param%ocn  = 0.0
  param%och  = 0.0
  param%ssa  = 0.0
  param%ssc  = 0.0
  param%du1  = 0.0
  param%du2  = 0.0
  param%du3  = 0.0
  param%du4  = 0.0
  param%du5  = 0.0
  param%du6  = 0.0
  param%du7  = 0.0
  param%du8  = 0.0
 case('nan')

 case('inf')

 end select

 end subroutine init_gocart

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD

#if ( WRF_CHEM == 1)

 subroutine  opt_gocart_broad( nband, ib, aero, pr, ta, sh, kext_gocart, salb_gocart, asym_gocart )
 implicit none

!---------------------------------------------------------------------------------------------------
! Comments:  
!  Compute single-scattering properties from atmospheric particles using LUTs-interpolation techniqu.
!  This routine is for GOCART microphysics.
!        
! History: 
!  10/2009  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!---------------------------------------------------------------------------------------------------
 integer,intent(in)::nband  !total band #
 integer,intent(in)::ib !radiation type
 real,intent(in) :: aero(tgmx)  ! aerosol mass concentration [g/m3]  (tgmx is defined in module_gocart_coupling)
 real, intent(in)    :: pr        !pressure [mb]
 real, intent(in)    :: ta        !temperature [K]
 real, intent(in)    :: sh        !specific humidity [g/g]
 type ( particle_gocart ),dimension(nband),intent(out) :: & ! particle_gocart is defined in module_simulater
     kext_gocart,  & ! extinction coefficient [km-1]
     salb_gocart,  & ! single scattering albedo [-]
     asym_gocart     ! asymetry parameter [-]

 type ( particle_gocart ) :: q   ! mixing ratio [g/m3]
 real :: rh ! relative humidity [0~1]  
 integer :: rhi         !RH index
 real :: es            !saturated vapor pressure [kPa]
 real :: eq             !vapor mixing pressure [kPa]
 real :: e             !vapor pressure [kPa]
 integer :: n

!
! derive relative humidity
!
 es = 0.611 * exp( 5423.*( 1./273.15 - 1./ta ) )  ! saturatd vapor pressure over flat water [kPa]
 eq = sh/(1.-sh)                                   ! vapor mixing pressure [kPa]
 e = (eq / (eq+0.622)) * pr * 0.1                   ! unsaturated vapor pressure [mb] -> [kPa]
 rh = min( max(1e-5, e/es) , 0.99)                ! relative humidity (set upper limit 99%) [-]

!
! convert to gocart type name. 
!
 q%so4  = aero(1)
 q%blc  = aero(2)
 q%ocn  = aero(3)
 q%och  = aero(4)
 q%ssa  = aero(5)
 q%ssc  = aero(6)
 q%du1  = aero(7)
 q%du2  = aero(8)
 q%du3  = aero(9)
 q%du4  = aero(10)
 q%du5  = aero(11)
 q%du6  = aero(12)
 q%du7  = aero(13)
 q%du8  = aero(14)


!
! initialize extinction
!
 do n = 1, nband
    call init_gocart( kext_gocart(n), 'zero')
    call init_gocart( salb_gocart(n), 'zero')
    call init_gocart( asym_gocart(n), 'zero')
 enddo

!
! go to LUT interpolation
!
 call mie_lut_broad_aerosol (nband, q%so4, rh ,  kext_lut_gocart(:,1:nband,ib)%so4 , &
                             salb_lut_gocart(:,1:nband,ib)%so4 , &
                             asym_lut_gocart(:,1:nband,ib)%so4 , pts_rh, mxpts_rh , &
                             kext_gocart%so4, salb_gocart%so4, asym_gocart%so4 )

 call mie_lut_broad_aerosol (nband, q%blc, rh ,  kext_lut_gocart(:,1:nband,ib)%blc , &
                             salb_lut_gocart(:,1:nband,ib)%blc , &
                             asym_lut_gocart(:,1:nband,ib)%blc , pts_rh, mxpts_rh , &
                             kext_gocart%blc, salb_gocart%blc, asym_gocart%blc )

 call mie_lut_broad_aerosol (nband, q%ocn, rh ,  kext_lut_gocart(:,1:nband,ib)%ocn , &
                             salb_lut_gocart(:,1:nband,ib)%ocn , &
                             asym_lut_gocart(:,1:nband,ib)%ocn , pts_rh, mxpts_rh , &
                             kext_gocart%ocn, salb_gocart%ocn, asym_gocart%ocn )

 call mie_lut_broad_aerosol (nband, q%och, rh ,  kext_lut_gocart(:,1:nband,ib)%och , &
                             salb_lut_gocart(:,1:nband,ib)%och , &
                             asym_lut_gocart(:,1:nband,ib)%och , pts_rh, mxpts_rh , &
                             kext_gocart%och, salb_gocart%och, asym_gocart%och )

 call mie_lut_broad_aerosol (nband, q%ssa, rh ,  kext_lut_gocart(:,1:nband,ib)%ssa , &
                             salb_lut_gocart(:,1:nband,ib)%ssa , &
                             asym_lut_gocart(:,1:nband,ib)%ssa , pts_rh, mxpts_rh , &
                             kext_gocart%ssa, salb_gocart%ssa, asym_gocart%ssa )

 call mie_lut_broad_aerosol (nband, q%ssc, rh ,  kext_lut_gocart(:,1:nband,ib)%ssc , &
                             salb_lut_gocart(:,1:nband,ib)%ssc , &
                             asym_lut_gocart(:,1:nband,ib)%ssc , pts_rh, mxpts_rh , &
                             kext_gocart%ssc, salb_gocart%ssc, asym_gocart%ssc )

 call mie_lut_broad_aerosol (nband, q%du1, rh ,  kext_lut_gocart(:,1:nband,ib)%du1 , &
                             salb_lut_gocart(:,1:nband,ib)%du1 , &
                             asym_lut_gocart(:,1:nband,ib)%du1 , pts_rh, mxpts_rh , &
                             kext_gocart%du1 , salb_gocart%du1 , asym_gocart%du1 )

 call mie_lut_broad_aerosol (nband, q%du2, rh ,  kext_lut_gocart(:,1:nband,ib)%du2 , &
                             salb_lut_gocart(:,1:nband,ib)%du2 , &
                             asym_lut_gocart(:,1:nband,ib)%du2 , pts_rh, mxpts_rh , &
                             kext_gocart%du2 , salb_gocart%du2 , asym_gocart%du2 )

 call mie_lut_broad_aerosol (nband, q%du3, rh ,  kext_lut_gocart(:,1:nband,ib)%du3 , &
                             salb_lut_gocart(:,1:nband,ib)%du3 , &
                             asym_lut_gocart(:,1:nband,ib)%du3 , pts_rh, mxpts_rh , &
                             kext_gocart%du3 , salb_gocart%du3 , asym_gocart%du3 )

 call mie_lut_broad_aerosol (nband, q%du4, rh ,  kext_lut_gocart(:,1:nband,ib)%du4 , &
                             salb_lut_gocart(:,1:nband,ib)%du4 , &
                             asym_lut_gocart(:,1:nband,ib)%du4 , pts_rh, mxpts_rh , &
                             kext_gocart%du4 , salb_gocart%du4 , asym_gocart%du4 )

 call mie_lut_broad_aerosol (nband, q%du5, rh ,  kext_lut_gocart(:,1:nband,ib)%du5 , &
                             salb_lut_gocart(:,1:nband,ib)%du5 , &
                             asym_lut_gocart(:,1:nband,ib)%du5, pts_rh, mxpts_rh , &
                             kext_gocart%du5 , salb_gocart%du5 , asym_gocart%du5 )

 call mie_lut_broad_aerosol (nband, q%du6, rh ,  kext_lut_gocart(:,1:nband,ib)%du6 , &
                             salb_lut_gocart(:,1:nband,ib)%du6 , &
                             asym_lut_gocart(:,1:nband,ib)%du6 , pts_rh, mxpts_rh , &
                             kext_gocart%du6 , salb_gocart%du6 , asym_gocart%du6 )

 call mie_lut_broad_aerosol (nband, q%du7, rh ,  kext_lut_gocart(:,1:nband,ib)%du7 , &
                             salb_lut_gocart(:,1:nband,ib)%du7 , &
                             asym_lut_gocart(:,1:nband,ib)%du7 , pts_rh, mxpts_rh , &
                             kext_gocart%du7 , salb_gocart%du7 , asym_gocart%du7 )

 call mie_lut_broad_aerosol (nband, q%du8, rh ,  kext_lut_gocart(:,1:nband,ib)%du8 , &
                             salb_lut_gocart(:,1:nband,ib)%du8 , &
                             asym_lut_gocart(:,1:nband,ib)%du8 , pts_rh, mxpts_rh , &
                             kext_gocart%du8 , salb_gocart%du8 , asym_gocart%du8 )

 end subroutine opt_gocart_broad

#endif

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine mie_lut_broad_aerosol( nband, q , rh_in, kext_lut, salb_lut, asym_lut, pts_rh, mxpts, &
                                  kext, salb, asym )
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! This subroutine find the extinction/scattering coefficients for
! given Re using Mie LUTs (per unit q), 
! and optal values are re-scaled to actual q (hydrometeor content).
! 
! History:
!   05/2009  Toshi Matsui@NASA GSFC ; Initial     
!           
! References: 
!---------------------------------------------------------------------------------------------------
 integer,intent(in) :: nband   !total band number
 real,intent(in) :: q  ! particle mass mixing ratio [g/m3]
 real,intent(in) :: rh_in ! relative humidity [-] (0~1) 
 real,intent(in) :: kext_lut(mxpts,nband) ! extinction coefficient per unit mass [km-1 / (g/m3) ]
 real,intent(in) :: salb_lut(mxpts,nband) ! single scattering albedo [-]
 real,intent(in) :: asym_lut(mxpts,nband) ! asymetry parameter [-]
 real,intent(in) :: pts_rh(mxpts)  ! LUT of re index [micron]
 integer,intent(in) :: mxpts          ! dimension of rh index
 real,intent(out) :: kext(nband)   ! extinction coefficient [km-1] 
 real,intent(out) :: salb(nband)   ! single scattering albedo [-]
 real,intent(out) :: asym(nband)   ! asymetry parameter [-]

! local parameter
 integer :: j                 ! loop index
 real :: kext_q_unit(nband)   ! extinction coefficient per unit mass mixing ratio [km-1 / (g/m3) ]
 real :: wgt1,wgt2            ! weighting function [-]
 real :: rh                   ! local relative humidity [-] (0~1) 

!
!initialise
!
 kext = 0.d0 ; salb = 0.d0 ; asym = 0.d0
 rh = rh_in

 if(q <= q_min_aerosol) return

 if(rh < pts_rh(1)    ) rh = pts_rh(1)
 if(rh > pts_rh(mxpts)) rh = pts_rh(mxpts_rh)


!
! Linear interpolation
!
 RH_LOOP: do j = 1,mxpts_rh-1

  if( rh >= pts_rh(j) .and. rh <= pts_rh(j+1) ) then
      wgt2 =  ( rh - pts_rh(j) )    / ( pts_rh(j+1) - pts_rh(j) )
      wgt1 = 1.0 - wgt2

      kext_q_unit(:) = wgt1*kext_lut(j,:) + wgt2*kext_lut(j+1,:)
      salb(:)        = wgt1*salb_lut(j,:) + wgt2*salb_lut(j+1,:)
      asym(:)        = wgt1*asym_lut(j,:) + wgt2*asym_lut(j+1,:)

      exit RH_LOOP
  endif

 enddo RH_LOOP

!
! Re-scale kext from unit mass mixing ratio (1[g/m3]) to actual mass mixing ratio (q [g/m3])
! 
  kext(:) = kext_q_unit(:) * q


 end subroutine mie_lut_broad_aerosol

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

#if (WRF_CHEM == 1)

 subroutine total_opt_gocart_broad( dz_km, nband, kext_gocart, salb_gocart, asym_gocart, &
                                    tautot, salbtot, asymtot )
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
!  Compute total single-scattering properties from atmospheric particles.
!  This routine is for GCE microphysics.
!  
! History: 
!  10/2009  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!---------------------------------------------------------------------------------------------------
 real,intent(in) :: dz_km  !layer depth [km]
 integer,intent(in) :: nband  !total band number 
 type ( particle_gocart ),dimension(nband),intent(in) :: & ! particle_gocart is defined in module_simulater
     kext_gocart,  & ! extinction coefficient [km-1]
     salb_gocart,  & ! single scattering albedo [-]
     asym_gocart     ! asymetry parameter [-]

 real,dimension(nband),intent(out) :: tautot, salbtot, asymtot !total optical parameters

 real :: kexttot(nband) ! extinction coefficient [km-1]

 integer :: n


!
! total extinction 
!
   kexttot(:)    = &
                kext_gocart(:)%so4  + &
                kext_gocart(:)%blc  + &
                kext_gocart(:)%ocn  + &
                kext_gocart(:)%och  + &
                kext_gocart(:)%ssa  + &
                kext_gocart(:)%ssc  + &
                kext_gocart(:)%du1  + &
                kext_gocart(:)%du2  + &
                kext_gocart(:)%du3  + &
                kext_gocart(:)%du4  + &
                kext_gocart(:)%du5  + &
                kext_gocart(:)%du6  + &
                kext_gocart(:)%du7  + &
                kext_gocart(:)%du8

!
! total optical depth
!
 do n = 1, nband
    tautot(n) = kexttot(n) * dz_km
 enddo

!
! total single scattering albedo
!
 do n = 1, nband

   if ( kexttot(n) <= 0.e0 ) then
        salbtot(n) = 0.e0
   else
       salbtot(n) = ( &
                  salb_gocart(n)%so4 * kext_gocart(n)%so4   + &
                  salb_gocart(n)%blc * kext_gocart(n)%blc   + &
                  salb_gocart(n)%ocn * kext_gocart(n)%ocn   + &
                  salb_gocart(n)%och * kext_gocart(n)%och   + &
                  salb_gocart(n)%ssa * kext_gocart(n)%ssa   + &
                  salb_gocart(n)%ssc * kext_gocart(n)%ssc   + &
                  salb_gocart(n)%du1 * kext_gocart(n)%du1   + &
                  salb_gocart(n)%du2 * kext_gocart(n)%du2   + &
                  salb_gocart(n)%du3 * kext_gocart(n)%du3   + &
                  salb_gocart(n)%du4 * kext_gocart(n)%du4   + &
                  salb_gocart(n)%du5 * kext_gocart(n)%du5   + &
                  salb_gocart(n)%du6 * kext_gocart(n)%du6   + &
                  salb_gocart(n)%du7 * kext_gocart(n)%du7   + &
                  salb_gocart(n)%du8 * kext_gocart(n)%du8     &
                 ) / kexttot(n)

   endif

enddo

!
! total asymetry parameter
!
 do n = 1, nband

   if ( salbtot(n) <= 0.e0 ) then
        asymtot(n) = 0.e0
   else
       asymtot(n) = ( &
                  asym_gocart(n)%so4  * salb_gocart(n)%so4  * kext_gocart(n)%so4   + &
                  asym_gocart(n)%blc  * salb_gocart(n)%blc  * kext_gocart(n)%blc   + &
                  asym_gocart(n)%ocn  * salb_gocart(n)%ocn  * kext_gocart(n)%ocn   + &
                  asym_gocart(n)%och  * salb_gocart(n)%och  * kext_gocart(n)%och   + &
                  asym_gocart(n)%ssa  * salb_gocart(n)%ssa  * kext_gocart(n)%ssa   + &
                  asym_gocart(n)%ssc  * salb_gocart(n)%ssc  * kext_gocart(n)%ssc   + &
                  asym_gocart(n)%du1  * salb_gocart(n)%du1  * kext_gocart(n)%du1   + &
                  asym_gocart(n)%du2  * salb_gocart(n)%du2  * kext_gocart(n)%du2   + &
                  asym_gocart(n)%du3  * salb_gocart(n)%du3  * kext_gocart(n)%du3   + &
                  asym_gocart(n)%du4  * salb_gocart(n)%du4  * kext_gocart(n)%du4   + &
                  asym_gocart(n)%du5  * salb_gocart(n)%du5  * kext_gocart(n)%du5   + &
                  asym_gocart(n)%du6  * salb_gocart(n)%du6  * kext_gocart(n)%du6   + &
                  asym_gocart(n)%du7  * salb_gocart(n)%du7  * kext_gocart(n)%du7   + &
                  asym_gocart(n)%du8  * salb_gocart(n)%du8  * kext_gocart(n)%du8     &
                 ) / ( salbtot(n) * kexttot(n) )
   endif

enddo

 end subroutine total_opt_gocart_broad

#endif

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

  subroutine swrad ( np,cosz, pl,ta,wa,oa, fcld,ict,icb, &
                     taucl,ssacl,asycl,taual,ssaal,asyal, &
                     rsuvbm,rsuvdf,rsirbm,rsirdf, &
                     flx_out,flxd_out,flxu_out , &
                     flxd_surf, lmask, irestrict) 

!------------     corrections for bugs      ----------------------------
!
!     a bug was found in "ntop=nctop", it has been corrected to be
!     "ntop=nctop(i)".   dated march 28, 2000.
!
!
!-----------------------------------------------------------------------
!
! following the nasa technical memorandum (nasa/tm-1999-104606, vol. 15)
!  of chou and suarez (1999), this routine computes solar fluxes due to
!  absorption by water vapor, ozone, co2, o2, clouds, and aerosols and
!  due to scattering by clouds, aerosols, and gases.
!
!
! cloud ice, liquid, and rain particles are allowed to co-exist in a layer.
!
! if no information is available for the effective particle size, reff,
!  default values of 10 micron for liquid water and 75 micron for ice may be
!  used.
!  the size of raindrops, reff(3), is irrelevant in this code. it can be
!  set to any values.
!  for a clear layer, reff can be set to any values except zero.
!
! the maximum-random assumption is applied for treating cloud
!  overlapping. clouds are grouped into high, middle, and low clouds
!  separated by the level indices ict and icb.  for detail, see
!  subroutine "cloud_scale".
!
! aerosol optical thickness, single-scattering albedo, and asymmetry
!  factor can be specified as functions of height and spectral band.
!
!----- input parameters:
!                                                   units      size
!  number of atmospheric layers (np)                n/d         1
!  level pressure (pl)                              mb       (np+1)
!  layer temperature (ta)                           k        np
!  layer specific humidity (wa)                     gm/gm    np
!  layer ozone concentration (oa)                   gm/gm    np
!  co2 mixing ratio by volume (co2)                 pppv        1
!  option for scaling cloud optical thickness       n/d         1
!        overcast="true" if scaling is not required
!        overcast="fasle" if scaling is required
!  cloud optical thickness (taucl)                  n/d       np*band
!  cloud single scattering albedo (ssacl)           n/d       np*band
!  cloud asymetry parameter (asycl)                 n/d       np*band
!  aerosol optical thickness (taual)                n/d       np*band
!  aerosol single scattering albedo (ssaal)         n/d       np*band
!  aerosol asymetry parameter (asyal)               n/d       np*band
!  cloud amount (fcld)                            fraction   np
!  level index separating high and middle           n/d         1
!        clouds (ict)
!  level index separating middle and low            n/d         1
!        clouds (icb)
!  aerosol optical thickness (taual)                n/d      np*11
!  aerosol single-scattering albedo (ssaal)         n/d      np*11
!  aerosol asymmetry factor (asyal)                 n/d      np*11
!        in the uv region :
!           index  1 for the 0.175-0.225 micron band
!           index  2 for the 0.225-0.245; 0.260-0.280 micron band
!           index  3 for the 0.245-0.260 micron band
!           index  4 for the 0.280-0.295 micron band
!           index  5 for the 0.295-0.310 micron band
!           index  6 for the 0.310-0.320 micron band
!           index  7 for the 0.325-0.400 micron band
!        in the par region :
!           index  8 for the 0.400-0.700 micron band
!        in the infrared region :
!           index  9 for the 0.700-1.220 micron band
!           index 10 for the 1.220-2.270 micron band
!           index 11 for the 2.270-10.00 micron band
!   cosine of solar zenith angle (cosz)              n/d     
!   uv+visible sfc albedo for beam radiation
!        for wavelengths<0.7 micron (rsuvbm)       fraction  
!   uv+visible sfc albedo for diffuse radiation
!        for wavelengths<0.7 micron (rsuvdf)       fraction  
!   ir sfc albedo for beam radiation
!        for wavelengths>0.7 micron  (rsirbm)      fraction  
!   ir sfc albedo for diffuse radiation (rsirdf)   fraction  
!
!----- output parameters
!
!   all-sky flux divergence level (downward minus upward) (flx)     fraction  (np+1)
!   clear-sky flux divergence level (downward minus upward) (flc)   fraction  (np+1)
! 
!   all-sky direct downward uv (0.175-0.4 micron)
!                flux at the surface (fdiruv)      fraction  
!   all-sky diffuse downward uv flux at
!                the surface (fdifuv)              fraction  
!   all-sky direct downward par (0.4-0.7 micron)
!                flux at the surface (fdirpar)     fraction  
!   all-sky diffuse downward par flux at
!                the surface (fdifpar)             fraction  
!   all-sky direct downward ir (0.7-10 micron)
!                flux at the surface (fdirir)      fraction  
!   all-sky diffuse downward ir flux at
!                the surface (fdifir)              fraction  
!
!----- notes:
!
!    (1) the unit of output fluxes (flx,flc,etc.) is fraction of the
!        insolation at the top of the atmosphere.  therefore, fluxes
!        are the output fluxes multiplied by the extra-terrestrial solar
!        flux and the cosine of the solar zenith angle.
!    (2) pl( ,1) is the pressure at the top of the model, and
!        pl( ,np+1) is the surface pressure.
!    (3) the pressure levels ict and icb correspond approximately
!        to 400 and 700 mb.
!
!-----if coding errors are found, please notify ming-dah chou at
!     chou@climate.gsfc.nasa.gov
!
!*************************************************************************
 implicit none
!-----IO parameters
 integer,intent(in) :: irestrict
 logical,dimension(CHUNK),intent(in) :: lmask
 integer,intent(in) :: np  ! # of vertical layer
 integer,dimension(CHUNK),intent(in) :: ict,icb !vertical index to separate high- middle- and low-clouds
 real(fp_kind),intent(in) :: pl(CHUNK,1:np+1)
 real(fp_kind),dimension(CHUNK,np),intent(in) :: ta,wa,oa,fcld
 real(fp_kind),dimension(CHUNK,np,nband_sw),intent(in) :: taucl,ssacl,asycl  ! condensate optical properties 
 real(fp_kind),dimension(CHUNK,np,nband_sw),intent(in) :: taual,ssaal,asyal  ! aerosol optical properties 
 real(fp_kind),dimension(CHUNK),intent(in) :: cosz                        ! cosine of solar zenith angle
 real(fp_kind),dimension(CHUNK),intent(in) :: rsuvbm,rsuvdf,rsirbm,rsirdf ! vis/nir albedo for beam and iffuse radiaiton

 real,intent(out) :: flx_out(CHUNK,1:np+1)    !flux divergence (down-up) []
 real,intent(out) :: flxd_out(CHUNK,1:np+1)   !downward flux fraction []
 real,intent(out) :: flxu_out(CHUNK,1:np+1)   !upwelling flux fraction []
 real,intent(out) :: flxd_surf(CHUNK,4)     !1-direct UV+PAR, 2-diffuse UV+PAR, 3-direct NIR, 4-diffuse NIR

!-----IO parameter used to be-----
 real(fp_kind) :: flc(CHUNK,1:np+1)
 real(fp_kind) :: flx(CHUNK,1:np+1)    !flux divergence (down-up) []
 real(fp_kind) :: flxd(CHUNK,1:np+1)   !downward flux fraction []
 real(fp_kind) :: flxu(CHUNK,1:np+1)   !upwelling flux fraction []
 real(fp_kind) :: fdiruv(CHUNK) ,fdifuv(CHUNK)  !UV 
 real(fp_kind) :: fdirpar(CHUNK),fdifpar(CHUNK) !PAR
 real(fp_kind) :: fdirir(CHUNK) ,fdifir(CHUNK)  !NIR
!-----temporary array
 integer i,j,k,ntop(CHUNK),ic
 integer :: nctop(CHUNK)
 real(fp_kind) x
 real(fp_kind) :: taux(CHUNK,np)     ! total condensates optical depth
 real(fp_kind) :: dp(CHUNK,np)
 real(fp_kind) :: wh(CHUNK,np)
 real(fp_kind) :: oh(CHUNK,np)
 real(fp_kind) :: scal(CHUNK,np)
 real(fp_kind) :: swu(CHUNK,np+1)
 real(fp_kind) :: swh(CHUNK,np+1)
 real(fp_kind) :: so2(CHUNK,np+1)    !scaled o2 conc
 real(fp_kind) :: df(CHUNK,np+1)     !integrated flux refuction rate []
 real(fp_kind) :: df_sub(CHUNK,np+1) !sub-layer flux reduction rate []
 real(fp_kind) :: df_cld(CHUNK,np+1) !integrated clear-sky flux reduction rate []
 real(fp_kind) :: df_clr(CHUNK,np+1) !integrated all-sky flux reduction rate []
 real(fp_kind) :: snt(CHUNK)         !inverse of cosz
 real(fp_kind) :: cnt(CHUNK)

! new look-up table for (Fclr/Fall) ratio (overcast_fast option)
  real(fp_kind) :: ratio, cld_alb
  integer :: i_cos, i_tau
  real(fp_kind) :: ratio_lut(10,10)
   data ((ratio_lut(i,j),i=1,10),j=1,10)/ &  !i cosin  j albedo
   0.796, 0.559, 0.523, 0.474, 0.439, 0.377, 0.298, 0.239, 0.154, 0.086, &
   0.845, 0.628, 0.566, 0.508, 0.457, 0.392, 0.315, 0.242, 0.156, 0.087, &
   0.894, 0.697, 0.609, 0.542, 0.475, 0.407, 0.332, 0.245, 0.158, 0.088, &
   0.924, 0.759, 0.662, 0.581, 0.511, 0.432, 0.350, 0.269, 0.173, 0.092, &
   0.944, 0.809, 0.713, 0.634, 0.552, 0.471, 0.381, 0.288, 0.183, 0.097, &
   0.961, 0.848, 0.760, 0.689, 0.602, 0.516, 0.425, 0.323, 0.208, 0.116, &
   0.971, 0.882, 0.808, 0.730, 0.650, 0.556, 0.456, 0.355, 0.233, 0.134, &
   0.978, 0.910, 0.844, 0.776, 0.695, 0.601, 0.499, 0.387, 0.256, 0.141, &
   0.984, 0.934, 0.876, 0.810, 0.731, 0.637, 0.533, 0.405, 0.275, 0.151, &
   0.988, 0.944, 0.897, 0.844, 0.773, 0.683, 0.561, 0.421, 0.277, 0.156/

!-----parameters for co2 transmission tables---------------------------
      integer nu,nw,nx2,ny2 ! cccshie 9/15/04
      parameter (nu=43,nw=37,nx2=62,ny2=101)
      real(fp_kind) w1,dw,u1,du,coa(nx2,ny2),cah(nu,nw)
!-----cah is the co2 absorptance in band 10
      data ((cah(i,j),i=1,43),j=  1,  1)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000053,  0.0000074, &
        0.0000104,  0.0000147,  0.0000206,  0.0000288,  0.0000402, &
        0.0000559,  0.0000772,  0.0001059,  0.0001439,  0.0001936, &
        0.0002575,  0.0003384,  0.0004400,  0.0005662,  0.0007219, &
        0.0009131,  0.0011470,  0.0014327,  0.0017806,  0.0022021, &
        0.0027093,  0.0033141,  0.0040280,  0.0048609,  0.0058217, &
        0.0069177,  0.0081559,  0.0095430/
      data ((cah(i,j),i=1,43),j=  2,  2)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000053,  0.0000074, &
        0.0000104,  0.0000147,  0.0000206,  0.0000288,  0.0000402, &
        0.0000559,  0.0000772,  0.0001059,  0.0001439,  0.0001936, &
        0.0002575,  0.0003384,  0.0004400,  0.0005662,  0.0007219, &
        0.0009130,  0.0011470,  0.0014326,  0.0017805,  0.0022020, &
        0.0027091,  0.0033139,  0.0040276,  0.0048605,  0.0058211, &
        0.0069170,  0.0081551,  0.0095420/
      data ((cah(i,j),i=1,43),j=  3,  3)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000053,  0.0000074, &
        0.0000104,  0.0000147,  0.0000206,  0.0000288,  0.0000402, &
        0.0000559,  0.0000772,  0.0001059,  0.0001439,  0.0001936, &
        0.0002574,  0.0003384,  0.0004399,  0.0005661,  0.0007218, &
        0.0009129,  0.0011468,  0.0014325,  0.0017803,  0.0022017, &
        0.0027088,  0.0033135,  0.0040271,  0.0048599,  0.0058204, &
        0.0069161,  0.0081539,  0.0095406/
      data ((cah(i,j),i=1,43),j=  4,  4)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000053,  0.0000074, &
        0.0000104,  0.0000147,  0.0000206,  0.0000288,  0.0000402, &
        0.0000559,  0.0000772,  0.0001059,  0.0001439,  0.0001936, &
        0.0002574,  0.0003384,  0.0004399,  0.0005661,  0.0007217, &
        0.0009128,  0.0011467,  0.0014323,  0.0017800,  0.0022014, &
        0.0027084,  0.0033130,  0.0040265,  0.0048591,  0.0058194, &
        0.0069148,  0.0081524,  0.0095387/
      data ((cah(i,j),i=1,43),j=  5,  5)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000053,  0.0000074, &
        0.0000104,  0.0000147,  0.0000206,  0.0000288,  0.0000402, &
        0.0000559,  0.0000772,  0.0001059,  0.0001439,  0.0001935, &
        0.0002574,  0.0003383,  0.0004398,  0.0005660,  0.0007216, &
        0.0009127,  0.0011465,  0.0014320,  0.0017797,  0.0022010, &
        0.0027078,  0.0033123,  0.0040256,  0.0048580,  0.0058180, &
        0.0069132,  0.0081503,  0.0095361/
      data ((cah(i,j),i=1,43),j=  6,  6)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000053,  0.0000074, &
        0.0000104,  0.0000147,  0.0000206,  0.0000288,  0.0000402, &
        0.0000559,  0.0000772,  0.0001059,  0.0001439,  0.0001935, &
        0.0002573,  0.0003383,  0.0004398,  0.0005659,  0.0007215, &
        0.0009125,  0.0011462,  0.0014317,  0.0017792,  0.0022004, &
        0.0027071,  0.0033113,  0.0040244,  0.0048565,  0.0058162, &
        0.0069109,  0.0081476,  0.0095328/
      data ((cah(i,j),i=1,43),j=  7,  7)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000053,  0.0000074, &
        0.0000104,  0.0000147,  0.0000206,  0.0000288,  0.0000402, &
        0.0000559,  0.0000772,  0.0001058,  0.0001438,  0.0001935, &
        0.0002573,  0.0003382,  0.0004396,  0.0005657,  0.0007213, &
        0.0009122,  0.0011459,  0.0014312,  0.0017786,  0.0021996, &
        0.0027061,  0.0033100,  0.0040228,  0.0048545,  0.0058137, &
        0.0069079,  0.0081439,  0.0095283/
      data ((cah(i,j),i=1,43),j=  8,  8)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000074, &
        0.0000104,  0.0000146,  0.0000206,  0.0000288,  0.0000402, &
        0.0000558,  0.0000772,  0.0001058,  0.0001438,  0.0001934, &
        0.0002572,  0.0003381,  0.0004395,  0.0005655,  0.0007210, &
        0.0009119,  0.0011454,  0.0014306,  0.0017778,  0.0021985, &
        0.0027047,  0.0033084,  0.0040207,  0.0048519,  0.0058105, &
        0.0069040,  0.0081391,  0.0095225/
      data ((cah(i,j),i=1,43),j=  9,  9)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000074, &
        0.0000104,  0.0000146,  0.0000206,  0.0000288,  0.0000402, &
        0.0000558,  0.0000771,  0.0001058,  0.0001437,  0.0001933, &
        0.0002571,  0.0003379,  0.0004393,  0.0005652,  0.0007206, &
        0.0009114,  0.0011447,  0.0014297,  0.0017767,  0.0021971, &
        0.0027030,  0.0033061,  0.0040180,  0.0048485,  0.0058064, &
        0.0068989,  0.0081329,  0.0095149/
      data ((cah(i,j),i=1,43),j= 10, 10)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000074, &
        0.0000104,  0.0000146,  0.0000205,  0.0000288,  0.0000402, &
        0.0000558,  0.0000771,  0.0001057,  0.0001437,  0.0001932, &
        0.0002569,  0.0003377,  0.0004390,  0.0005649,  0.0007201, &
        0.0009107,  0.0011439,  0.0014286,  0.0017753,  0.0021953, &
        0.0027006,  0.0033032,  0.0040144,  0.0048441,  0.0058009, &
        0.0068922,  0.0081248,  0.0095051/
      data ((cah(i,j),i=1,43),j= 11, 11)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000074, &
        0.0000104,  0.0000146,  0.0000205,  0.0000287,  0.0000401, &
        0.0000558,  0.0000770,  0.0001056,  0.0001436,  0.0001931, &
        0.0002567,  0.0003375,  0.0004387,  0.0005644,  0.0007195, &
        0.0009098,  0.0011428,  0.0014271,  0.0017734,  0.0021929, &
        0.0026976,  0.0032995,  0.0040097,  0.0048384,  0.0057939, &
        0.0068837,  0.0081145,  0.0094926/
      data ((cah(i,j),i=1,43),j= 12, 12)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000074, &
        0.0000104,  0.0000146,  0.0000205,  0.0000287,  0.0000401, &
        0.0000557,  0.0000770,  0.0001055,  0.0001434,  0.0001929, &
        0.0002565,  0.0003371,  0.0004382,  0.0005637,  0.0007186, &
        0.0009087,  0.0011413,  0.0014252,  0.0017709,  0.0021898, &
        0.0026937,  0.0032946,  0.0040038,  0.0048311,  0.0057850, &
        0.0068729,  0.0081013,  0.0094768/
      data ((cah(i,j),i=1,43),j= 13, 13)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000074, &
        0.0000104,  0.0000146,  0.0000205,  0.0000287,  0.0000400, &
        0.0000556,  0.0000769,  0.0001054,  0.0001432,  0.0001926, &
        0.0002561,  0.0003366,  0.0004376,  0.0005629,  0.0007175, &
        0.0009073,  0.0011394,  0.0014228,  0.0017678,  0.0021859, &
        0.0026888,  0.0032885,  0.0039963,  0.0048218,  0.0057738, &
        0.0068592,  0.0080849,  0.0094570/
      data ((cah(i,j),i=1,43),j= 14, 14)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000074, &
        0.0000104,  0.0000146,  0.0000205,  0.0000286,  0.0000400, &
        0.0000556,  0.0000767,  0.0001052,  0.0001430,  0.0001923, &
        0.0002557,  0.0003361,  0.0004368,  0.0005619,  0.0007161, &
        0.0009054,  0.0011370,  0.0014197,  0.0017639,  0.0021809, &
        0.0026826,  0.0032809,  0.0039869,  0.0048103,  0.0057597, &
        0.0068422,  0.0080643,  0.0094323/
      data ((cah(i,j),i=1,43),j= 15, 15)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000073, &
        0.0000103,  0.0000145,  0.0000204,  0.0000286,  0.0000399, &
        0.0000554,  0.0000766,  0.0001050,  0.0001427,  0.0001919, &
        0.0002552,  0.0003353,  0.0004358,  0.0005605,  0.0007144, &
        0.0009032,  0.0011340,  0.0014159,  0.0017590,  0.0021748, &
        0.0026750,  0.0032715,  0.0039752,  0.0047961,  0.0057424, &
        0.0068212,  0.0080389,  0.0094019/
      data ((cah(i,j),i=1,43),j= 16, 16)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000073, &
        0.0000103,  0.0000145,  0.0000204,  0.0000285,  0.0000398, &
        0.0000553,  0.0000764,  0.0001047,  0.0001423,  0.0001914, &
        0.0002545,  0.0003344,  0.0004345,  0.0005589,  0.0007122, &
        0.0009003,  0.0011304,  0.0014112,  0.0017531,  0.0021673, &
        0.0026656,  0.0032598,  0.0039609,  0.0047786,  0.0057211, &
        0.0067954,  0.0080078,  0.0093646/
      data ((cah(i,j),i=1,43),j= 17, 17)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000018,  0.0000026,  0.0000037,  0.0000052,  0.0000073, &
        0.0000103,  0.0000145,  0.0000203,  0.0000284,  0.0000397, &
        0.0000551,  0.0000761,  0.0001044,  0.0001419,  0.0001908, &
        0.0002536,  0.0003332,  0.0004330,  0.0005568,  0.0007095, &
        0.0008968,  0.0011259,  0.0014054,  0.0017458,  0.0021123, &
        0.0026542,  0.0032457,  0.0039435,  0.0047573,  0.0056951, &
        0.0067640,  0.0079700,  0.0093194/
      data ((cah(i,j),i=1,43),j= 18, 18)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000018,  0.0000026,  0.0000037,  0.0000052,  0.0000073, &
        0.0000102,  0.0000144,  0.0000202,  0.0000283,  0.0000395, &
        0.0000549,  0.0000758,  0.0001040,  0.0001413,  0.0001900, &
        0.0002525,  0.0003318,  0.0004311,  0.0005543,  0.0007063, &
        0.0008926,  0.0011204,  0.0013985,  0.0017370,  0.0021470, &
        0.0026404,  0.0032285,  0.0039224,  0.0047315,  0.0056637, &
        0.0067260,  0.0079245,  0.0092651/
      data ((cah(i,j),i=1,43),j= 19, 19)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000006,  0.0000009,  0.0000013, &
        0.0000018,  0.0000026,  0.0000036,  0.0000051,  0.0000072, &
        0.0000102,  0.0000143,  0.0000201,  0.0000282,  0.0000393, &
        0.0000546,  0.0000754,  0.0001034,  0.0001406,  0.0001890, &
        0.0002512,  0.0003300,  0.0004287,  0.0005513,  0.0007023, &
        0.0008875,  0.0011139,  0.0013901,  0.0017264,  0.0021337, &
        0.0026238,  0.0032080,  0.0038971,  0.0047005,  0.0056261, &
        0.0066806,  0.0078701,  0.0092003/
      data ((cah(i,j),i=1,43),j= 20, 20)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000006,  0.0000009,  0.0000013, &
        0.0000018,  0.0000026,  0.0000036,  0.0000051,  0.0000072, &
        0.0000101,  0.0000142,  0.0000200,  0.0000280,  0.0000391, &
        0.0000543,  0.0000750,  0.0001028,  0.0001397,  0.0001878, &
        0.0002496,  0.0003279,  0.0004259,  0.0005476,  0.0006975, &
        0.0008813,  0.0011060,  0.0013802,  0.0017138,  0.0021179, &
        0.0026040,  0.0031835,  0.0038670,  0.0046637,  0.0055814, &
        0.0066267,  0.0078055,  0.0091235/
      data ((cah(i,j),i=1,43),j= 21, 21)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000006,  0.0000009,  0.0000013, &
        0.0000018,  0.0000025,  0.0000036,  0.0000051,  0.0000071, &
        0.0000100,  0.0000141,  0.0000198,  0.0000278,  0.0000388, &
        0.0000539,  0.0000744,  0.0001020,  0.0001386,  0.0001863, &
        0.0002477,  0.0003253,  0.0004226,  0.0005432,  0.0006918, &
        0.0008740,  0.0010966,  0.0013683,  0.0016988,  0.0020991, &
        0.0025806,  0.0031545,  0.0038313,  0.0046201,  0.0055285, &
        0.0065630,  0.0077294,  0.0090332/
      data ((cah(i,j),i=1,43),j= 22, 22)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000009,  0.0000013, &
        0.0000018,  0.0000025,  0.0000036,  0.0000050,  0.0000071, &
        0.0000100,  0.0000140,  0.0000197,  0.0000275,  0.0000384, &
        0.0000534,  0.0000737,  0.0001011,  0.0001373,  0.0001846, &
        0.0002453,  0.0003222,  0.0004185,  0.0005265,  0.0006850, &
        0.0008652,  0.0010855,  0.0013541,  0.0016809,  0.0020768, &
        0.0025528,  0.0031202,  0.0037892,  0.0045688,  0.0054664, &
        0.0064883,  0.0076402,  0.0089277/
      data ((cah(i,j),i=1,43),j= 23, 23)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000009,  0.0000013, &
        0.0000018,  0.0000025,  0.0000035,  0.0000050,  0.0000070, &
        0.0000098,  0.0000138,  0.0000194,  0.0000272,  0.0000380, &
        0.0000528,  0.0000729,  0.0000999,  0.0001357,  0.0001825, &
        0.0002425,  0.0003185,  0.0004137,  0.0005316,  0.0006769, &
        0.0008548,  0.0010722,  0.0013373,  0.0016599,  0.0020504, &
        0.0025201,  0.0030799,  0.0037398,  0.0045087,  0.0053938, &
        0.0064013,  0.0075366,  0.0088053/
      data ((cah(i,j),i=1,43),j= 24, 24)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000009,  0.0000012, &
        0.0000017,  0.0000025,  0.0000035,  0.0000049,  0.0000069, &
        0.0000097,  0.0000137,  0.0000192,  0.0000268,  0.0000375, &
        0.0000520,  0.0000719,  0.0000986,  0.0001339,  0.0001800, &
        0.0002392,  0.0003142,  0.0004079,  0.0005242,  0.0006673, &
        0.0008426,  0.0010567,  0.0013177,  0.0016352,  0.0020196, &
        0.0024820,  0.0030330,  0.0036825,  0.0044391,  0.0053098, &
        0.0063007,  0.0074172,  0.0084815/
      data ((cah(i,j),i=1,43),j= 25, 25)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000009,  0.0000012, &
        0.0000017,  0.0000024,  0.0000034,  0.0000048,  0.0000068, &
        0.0000096,  0.0000134,  0.0000189,  0.0000264,  0.0000369, &
        0.0000512,  0.0000708,  0.0000970,  0.0001318,  0.0001772, &
        0.0002354,  0.0003091,  0.0004013,  0.0005156,  0.0006562, &
        0.0008284,  0.0010386,  0.0012949,  0.0016066,  0.0019840, &
        0.0024379,  0.0029788,  0.0036164,  0.0043590,  0.0052135, &
        0.0061857,  0.0072808,  0.0085042/
      data ((cah(i,j),i=1,43),j= 26, 26)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000008,  0.0000012, &
        0.0000017,  0.0000024,  0.0000034,  0.0000047,  0.0000067, &
        0.0000094,  0.0000132,  0.0000185,  0.0000259,  0.0000362, &
        0.0000503,  0.0000695,  0.0000952,  0.0001294,  0.0001739, &
        0.0002310,  0.0003033,  0.0003937,  0.0005057,  0.0006435, &
        0.0008121,  0.0010180,  0.0012688,  0.0015739,  0.0019434, &
        0.0023877,  0.0029172,  0.0035413,  0.0042681,  0.0051043, &
        0.0060554,  0.0071267,  0.0083234/
      data ((cah(i,j),i=1,43),j= 27, 27)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000008,  0.0000012, &
        0.0000016,  0.0000023,  0.0000033,  0.0000046,  0.0000065, &
        0.0000092,  0.0000129,  0.0000181,  0.0000254,  0.0000355, &
        0.0000493,  0.0000680,  0.0000933,  0.0001267,  0.0001702, &
        0.0002261,  0.0002968,  0.0003852,  0.0004946,  0.0006291, &
        0.0007937,  0.0009946,  0.0012394,  0.0015370,  0.0018975, &
        0.0023310,  0.0028478,  0.0034568,  0.0041660,  0.0049818, &
        0.0059096,  0.0069544,  0.0081215/
      data ((cah(i,j),i=1,43),j= 28, 28)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000008,  0.0000011, &
        0.0000016,  0.0000023,  0.0000032,  0.0000045,  0.0000064, &
        0.0000090,  0.0000126,  0.0000177,  0.0000248,  0.0000346, &
        0.0000481,  0.0000664,  0.0000910,  0.0001236,  0.0001661, &
        0.0002206,  0.0002895,  0.0003755,  0.0004821,  0.0006130, &
        0.0007731,  0.0009685,  0.0012065,  0.0014959,  0.0018463, &
        0.0022680,  0.0027705,  0.0033629,  0.0040526,  0.0048459, &
        0.0057480,  0.0067639,  0.0078987/
      data ((cah(i,j),i=1,43),j= 29, 29)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000008,  0.0000011, &
        0.0000016,  0.0000022,  0.0000031,  0.0000044,  0.0000062, &
        0.0000087,  0.0000123,  0.0000173,  0.0000242,  0.0000330, &
        0.0000468,  0.0000646,  0.0000886,  0.0001203,  0.0001616, &
        0.0002145,  0.0002814,  0.0003649,  0.0004682,  0.0005951, &
        0.0007503,  0.0009396,  0.0011701,  0.0014505,  0.0017900, &
        0.0021986,  0.0026857,  0.0032598,  0.0039283,  0.0046971, &
        0.0055713,  0.0065558,  0.0076557/
      data ((cah(i,j),i=1,43),j= 30, 30)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000003,  0.0000004,  0.0000005,  0.0000008,  0.0000011, &
        0.0000015,  0.0000021,  0.0000030,  0.0000043,  0.0000060, &
        0.0000085,  0.0000119,  0.0000167,  0.0000234,  0.0000327, &
        0.0000454,  0.0000627,  0.0000859,  0.0001166,  0.0001566, &
        0.0002078,  0.0002724,  0.0003531,  0.0004529,  0.0005755, &
        0.0007253,  0.0009079,  0.0011304,  0.0014010,  0.0017287, &
        0.0021232,  0.0025935,  0.0031480,  0.0037936,  0.0045361, &
        0.0053805,  0.0063314,  0.0073941/
      data ((cah(i,j),i=1,43),j= 31, 31)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000003,  0.0000004,  0.0000005,  0.0000007,  0.0000010, &
        0.0000015,  0.0000021,  0.0000029,  0.0000041,  0.0000058, &
        0.0000082,  0.0000115,  0.0000162,  0.0000226,  0.0000316, &
        0.0000438,  0.0000605,  0.0000829,  0.0001125,  0.0001510, &
        0.0002004,  0.0002626,  0.0003402,  0.0004362,  0.0005540, &
        0.0006980,  0.0008736,  0.0010874,  0.0013476,  0.0016627, &
        0.0020421,  0.0024947,  0.0030283,  0.0036497,  0.0043644, &
        0.0051772,  0.0060928,  0.0071164/
      data ((cah(i,j),i=1,43),j= 32, 32)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000003,  0.0000004,  0.0000005,  0.0000007,  0.0000010, &
        0.0000014,  0.0000020,  0.0000028,  0.0000040,  0.0000056, &
        0.0000079,  0.0000111,  0.0000155,  0.0000218,  0.0000303, &
        0.0000421,  0.0000582,  0.0000797,  0.0001081,  0.0001450, &
        0.0001923,  0.0002519,  0.0003262,  0.0004180,  0.0005308, &
        0.0006686,  0.0008367,  0.0010414,  0.0012905,  0.0015925, &
        0.0019561,  0.0023900,  0.0029017,  0.0034978,  0.0041836, &
        0.0049638,  0.0058430,  0.0068264/
      data ((cah(i,j),i=1,43),j= 33, 33)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000002,  0.0000003,  0.0000005,  0.0000007,  0.0000010, &
        0.0000014,  0.0000019,  0.0000027,  0.0000038,  0.0000053, &
        0.0000075,  0.0000106,  0.0000149,  0.0000208,  0.0000290, &
        0.0000403,  0.0000556,  0.0000761,  0.0001032,  0.0001384, &
        0.0001834,  0.0002402,  0.0003110,  0.0003985,  0.0005059, &
        0.0006372,  0.0007974,  0.0009926,  0.0012302,  0.0015185, &
        0.0018657,  0.0022803,  0.0027696,  0.0033398,  0.0039960, &
        0.0047430,  0.0055851,  0.0065278/
      data ((cah(i,j),i=1,43),j= 34, 34)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000002,  0.0000003,  0.0000005,  0.0000006,  0.0000009, &
        0.0000013,  0.0000018,  0.0000026,  0.0000036,  0.0000051, &
        0.0000071,  0.0000100,  0.0000141,  0.0000197,  0.0000275, &
        0.0000382,  0.0000527,  0.0000722,  0.0000979,  0.0001312, &
        0.0001739,  0.0002277,  0.0002947,  0.0003775,  0.0004793, &
        0.0006038,  0.0007558,  0.0009412,  0.0011671,  0.0014412, &
        0.0017717,  0.0021208,  0.0026329,  0.0031768,  0.0038033, &
        0.0045168,  0.0053220,  0.0062240/
      data ((cah(i,j),i=1,43),j= 35, 35)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000002,  0.0000003,  0.0000004,  0.0000006,  0.0000009, &
        0.0000012,  0.0000017,  0.0000024,  0.0000034,  0.0000048, &
        0.0000067,  0.0000095,  0.0000133,  0.0000186,  0.0000259, &
        0.0000360,  0.0000496,  0.0000679,  0.0000921,  0.0001235, &
        0.0001637,  0.0002143,  0.0002773,  0.0003554,  0.0004513, &
        0.0005688,  0.0007124,  0.0008876,  0.0011014,  0.0013610, &
        0.0016745,  0.0020493,  0.0024925,  0.0030099,  0.0036066, &
        0.0042868,  0.0050553,  0.0059171/
      data ((cah(i,j),i=1,43),j= 36, 36)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000001, &
        0.0000002,  0.0000003,  0.0000004,  0.0000006,  0.0000008, &
        0.0000011,  0.0000016,  0.0000022,  0.0000032,  0.0000045, &
        0.0000063,  0.0000088,  0.0000124,  0.0000173,  0.0000242, &
        0.0000336,  0.0000463,  0.0000634,  0.0000860,  0.0001153, &
        0.0001528,  0.0002001,  0.0002591,  0.0003322,  0.0004221, &
        0.0005323,  0.0006672,  0.0008322,  0.0010335,  0.0012785, &
        0.0015746,  0.0019293,  0.0023491,  0.0028399,  0.0034067, &
        0.0040539,  0.0047860,  0.0056083/
      data ((cah(i,j),i=1,43),j= 37, 37)/ &
        0.0000000,  0.0000000,  0.0000001,  0.0000001,  0.0000001, &
        0.0000002,  0.0000003,  0.0000004,  0.0000005,  0.0000007, &
        0.0000010,  0.0000015,  0.0000021,  0.0000029,  0.0000041, &
        0.0000058,  0.0000082,  0.0000114,  0.0000160,  0.0000223, &
        0.0000310,  0.0000428,  0.0000586,  0.0000795,  0.0001067, &
        0.0001414,  0.0001853,  0.0002401,  0.0003081,  0.0003918, &
        0.0004947,  0.0006208,  0.0007751,  0.0009639,  0.0011940, &
        0.0014726,  0.0018069,  0.0022032,  0.0026674,  0.0032043, &
        0.0038186,  0.0045147,  0.0052979/
!-----coa is the co2 absorptance in strong absorption regions of band 11
      data ((coa(i,j),i=1,62),j=  1,  1)/ &
        0.0000080,  0.0000089,  0.0000098,  0.0000106,  0.0000114, &
        0.0000121,  0.0000128,  0.0000134,  0.0000140,  0.0000146, &
        0.0000152,  0.0000158,  0.0000163,  0.0000168,  0.0000173, &
        0.0000178,  0.0000182,  0.0000186,  0.0000191,  0.0000195, &
        0.0000199,  0.0000202,  0.0000206,  0.0000210,  0.0000213, &
        0.0000217,  0.0000220,  0.0000223,  0.0000226,  0.0000229, &
        0.0000232,  0.0000235,  0.0000238,  0.0000241,  0.0000244, &
        0.0000246,  0.0000249,  0.0000252,  0.0000254,  0.0000257, &
        0.0000259,  0.0000261,  0.0000264,  0.0000266,  0.0000268, &
        0.0000271,  0.0000273,  0.0000275,  0.0000277,  0.0000279, &
        0.0000281,  0.0000283,  0.0000285,  0.0000287,  0.0000289, &
        0.0000291,  0.0000293,  0.0000295,  0.0000297,  0.0000298, &
        0.0000300,  0.0000302/
      data ((coa(i,j),i=1,62),j=  2,  2)/ &
        0.0000085,  0.0000095,  0.0000104,  0.0000113,  0.0000121, &
        0.0000128,  0.0000136,  0.0000143,  0.0000149,  0.0000155, &
        0.0000161,  0.0000167,  0.0000172,  0.0000178,  0.0000183, &
        0.0000187,  0.0000192,  0.0000196,  0.0000201,  0.0000205, &
        0.0000209,  0.0000213,  0.0000217,  0.0000220,  0.0000224, &
        0.0000227,  0.0000231,  0.0000234,  0.0000237,  0.0000240, &
        0.0000243,  0.0000246,  0.0000249,  0.0000252,  0.0000255, &
        0.0000258,  0.0000260,  0.0000263,  0.0000266,  0.0000268, &
        0.0000271,  0.0000273,  0.0000275,  0.0000278,  0.0000280, &
        0.0000282,  0.0000285,  0.0000287,  0.0000289,  0.0000291, &
        0.0000293,  0.0000295,  0.0000297,  0.0000299,  0.0000301, &
        0.0000303,  0.0000305,  0.0000307,  0.0000309,  0.0000311, &
        0.0000313,  0.0000314/
      data ((coa(i,j),i=1,62),j=  3,  3)/ &
        0.0000095,  0.0000106,  0.0000116,  0.0000125,  0.0000134, &
        0.0000143,  0.0000150,  0.0000158,  0.0000165,  0.0000171, &
        0.0000178,  0.0000184,  0.0000189,  0.0000195,  0.0000200, &
        0.0000205,  0.0000210,  0.0000215,  0.0000219,  0.0000223, &
        0.0000228,  0.0000232,  0.0000235,  0.0000239,  0.0000243, &
        0.0000247,  0.0000250,  0.0000253,  0.0000257,  0.0000260, &
        0.0000263,  0.0000266,  0.0000269,  0.0000272,  0.0000275, &
        0.0000278,  0.0000281,  0.0000283,  0.0000286,  0.0000289, &
        0.0000291,  0.0000294,  0.0000296,  0.0000299,  0.0000301, &
        0.0000303,  0.0000306,  0.0000308,  0.0000310,  0.0000312, &
        0.0000315,  0.0000317,  0.0000319,  0.0000321,  0.0000323, &
        0.0000325,  0.0000327,  0.0000329,  0.0000331,  0.0000333, &
        0.0000335,  0.0000329/
      data ((coa(i,j),i=1,62),j=  4,  4)/ &
        0.0000100,  0.0000111,  0.0000122,  0.0000131,  0.0000141, &
        0.0000149,  0.0000157,  0.0000165,  0.0000172,  0.0000179, &
        0.0000185,  0.0000191,  0.0000197,  0.0000203,  0.0000208, &
        0.0000213,  0.0000218,  0.0000223,  0.0000227,  0.0000232, &
        0.0000236,  0.0000240,  0.0000244,  0.0000248,  0.0000252, &
        0.0000255,  0.0000259,  0.0000262,  0.0000266,  0.0000269, &
        0.0000272,  0.0000275,  0.0000278,  0.0000281,  0.0000284, &
        0.0000287,  0.0000290,  0.0000293,  0.0000295,  0.0000298, &
        0.0000300,  0.0000303,  0.0000306,  0.0000308,  0.0000310, &
        0.0000313,  0.0000315,  0.0000317,  0.0000320,  0.0000322, &
        0.0000324,  0.0000326,  0.0000328,  0.0000331,  0.0000333, &
        0.0000335,  0.0000330,  0.0000339,  0.0000341,  0.0000343, &
        0.0000345,  0.0000346/
      data ((coa(i,j),i=1,62),j=  5,  5)/ &
        0.0000109,  0.0000121,  0.0000132,  0.0000143,  0.0000152, &
        0.0000161,  0.0000170,  0.0000178,  0.0000185,  0.0000192, &
        0.0000199,  0.0000205,  0.0000211,  0.0000217,  0.0000222, &
        0.0000228,  0.0000233,  0.0000238,  0.0000242,  0.0000247, &
        0.0000251,  0.0000255,  0.0000259,  0.0000263,  0.0000267, &
        0.0000271,  0.0000275,  0.0000278,  0.0000282,  0.0000285, &
        0.0000288,  0.0000291,  0.0000295,  0.0000298,  0.0000301, &
        0.0000304,  0.0000307,  0.0000309,  0.0000312,  0.0000315, &
        0.0000318,  0.0000320,  0.0000323,  0.0000325,  0.0000328, &
        0.0000330,  0.0000333,  0.0000335,  0.0000330,  0.0000340, &
        0.0000342,  0.0000344,  0.0000346,  0.0000348,  0.0000351, &
        0.0000353,  0.0000355,  0.0000357,  0.0000359,  0.0000361, &
        0.0000363,  0.0000365/
      data ((coa(i,j),i=1,62),j=  6,  6)/ &
        0.0000117,  0.0000130,  0.0000142,  0.0000153,  0.0000163, &
        0.0000173,  0.0000181,  0.0000190,  0.0000197,  0.0000204, &
        0.0000211,  0.0000218,  0.0000224,  0.0000230,  0.0000235, &
        0.0000241,  0.0000246,  0.0000251,  0.0000256,  0.0000260, &
        0.0000265,  0.0000269,  0.0000273,  0.0000277,  0.0000281, &
        0.0000285,  0.0000289,  0.0000293,  0.0000296,  0.0000299, &
        0.0000303,  0.0000306,  0.0000309,  0.0000313,  0.0000316, &
        0.0000319,  0.0000322,  0.0000324,  0.0000327,  0.0000330, &
        0.0000333,  0.0000336,  0.0000331,  0.0000341,  0.0000343, &
        0.0000346,  0.0000348,  0.0000351,  0.0000353,  0.0000355, &
        0.0000358,  0.0000360,  0.0000362,  0.0000365,  0.0000367, &
        0.0000369,  0.0000371,  0.0000373,  0.0000375,  0.0000377, &
        0.0000379,  0.0000381/
      data ((coa(i,j),i=1,62),j=  7,  7)/ &
        0.0000125,  0.0000139,  0.0000151,  0.0000163,  0.0000173, &
        0.0000183,  0.0000192,  0.0000200,  0.0000208,  0.0000216, &
        0.0000223,  0.0000229,  0.0000236,  0.0000242,  0.0000247, &
        0.0000253,  0.0000258,  0.0000263,  0.0000268,  0.0000273, &
        0.0000277,  0.0000282,  0.0000286,  0.0000290,  0.0000294, &
        0.0000298,  0.0000302,  0.0000306,  0.0000309,  0.0000313, &
        0.0000316,  0.0000320,  0.0000323,  0.0000326,  0.0000329, &
        0.0000332,  0.0000335,  0.0000331,  0.0000341,  0.0000344, &
        0.0000347,  0.0000350,  0.0000352,  0.0000355,  0.0000358, &
        0.0000360,  0.0000363,  0.0000365,  0.0000368,  0.0000370, &
        0.0000372,  0.0000375,  0.0000377,  0.0000379,  0.0000382, &
        0.0000384,  0.0000386,  0.0000388,  0.0000390,  0.0000392, &
        0.0000394,  0.0000396/
      data ((coa(i,j),i=1,62),j=  8,  8)/ &
        0.0000132,  0.0000147,  0.0000160,  0.0000172,  0.0000183, &
        0.0000193,  0.0000202,  0.0000210,  0.0000218,  0.0000226, &
        0.0000233,  0.0000240,  0.0000246,  0.0000252,  0.0000258, &
        0.0000264,  0.0000269,  0.0000274,  0.0000279,  0.0000284, &
        0.0000289,  0.0000293,  0.0000298,  0.0000302,  0.0000306, &
        0.0000310,  0.0000314,  0.0000318,  0.0000321,  0.0000325, &
        0.0000328,  0.0000332,  0.0000335,  0.0000331,  0.0000342, &
        0.0000345,  0.0000348,  0.0000351,  0.0000354,  0.0000357, &
        0.0000360,  0.0000363,  0.0000365,  0.0000368,  0.0000371, &
        0.0000373,  0.0000376,  0.0000378,  0.0000381,  0.0000383, &
        0.0000386,  0.0000388,  0.0000391,  0.0000393,  0.0000395, &
        0.0000397,  0.0000400,  0.0000402,  0.0000404,  0.0000406, &
        0.0000408,  0.0000411/
      data ((coa(i,j),i=1,62),j=  9,  9)/ &
        0.0000143,  0.0000158,  0.0000172,  0.0000184,  0.0000195, &
        0.0000206,  0.0000215,  0.0000224,  0.0000232,  0.0000240, &
        0.0000247,  0.0000254,  0.0000261,  0.0000267,  0.0000273, &
        0.0000279,  0.0000284,  0.0000290,  0.0000295,  0.0000300, &
        0.0000305,  0.0000309,  0.0000314,  0.0000318,  0.0000322, &
        0.0000326,  0.0000330,  0.0000334,  0.0000331,  0.0000342, &
        0.0000345,  0.0000349,  0.0000352,  0.0000356,  0.0000359, &
        0.0000362,  0.0000365,  0.0000368,  0.0000371,  0.0000374, &
        0.0000377,  0.0000380,  0.0000383,  0.0000386,  0.0000389, &
        0.0000391,  0.0000394,  0.0000397,  0.0000399,  0.0000402, &
        0.0000404,  0.0000407,  0.0000409,  0.0000412,  0.0000414, &
        0.0000416,  0.0000419,  0.0000421,  0.0000423,  0.0000426, &
        0.0000428,  0.0000430/
      data ((coa(i,j),i=1,62),j= 10, 10)/ &
        0.0000153,  0.0000169,  0.0000183,  0.0000196,  0.0000207, &
        0.0000218,  0.0000227,  0.0000236,  0.0000245,  0.0000253, &
        0.0000260,  0.0000267,  0.0000274,  0.0000281,  0.0000287, &
        0.0000293,  0.0000298,  0.0000304,  0.0000309,  0.0000314, &
        0.0000319,  0.0000324,  0.0000328,  0.0000333,  0.0000330, &
        0.0000341,  0.0000345,  0.0000349,  0.0000353,  0.0000357, &
        0.0000361,  0.0000364,  0.0000368,  0.0000371,  0.0000375, &
        0.0000378,  0.0000381,  0.0000384,  0.0000387,  0.0000391, &
        0.0000394,  0.0000397,  0.0000399,  0.0000402,  0.0000405, &
        0.0000408,  0.0000411,  0.0000413,  0.0000416,  0.0000419, &
        0.0000421,  0.0000424,  0.0000426,  0.0000429,  0.0000431, &
        0.0000434,  0.0000436,  0.0000439,  0.0000441,  0.0000443, &
        0.0000446,  0.0000448/
      data ((coa(i,j),i=1,62),j= 11, 11)/ &
        0.0000165,  0.0000182,  0.0000196,  0.0000209,  0.0000221, &
        0.0000232,  0.0000242,  0.0000251,  0.0000260,  0.0000268, &
        0.0000276,  0.0000283,  0.0000290,  0.0000297,  0.0000303, &
        0.0000309,  0.0000315,  0.0000321,  0.0000326,  0.0000331, &
        0.0000336,  0.0000341,  0.0000346,  0.0000350,  0.0000355, &
        0.0000359,  0.0000363,  0.0000367,  0.0000371,  0.0000375, &
        0.0000379,  0.0000383,  0.0000386,  0.0000390,  0.0000394, &
        0.0000397,  0.0000400,  0.0000404,  0.0000407,  0.0000410, &
        0.0000413,  0.0000416,  0.0000419,  0.0000422,  0.0000425, &
        0.0000428,  0.0000431,  0.0000434,  0.0000437,  0.0000439, &
        0.0000442,  0.0000445,  0.0000447,  0.0000450,  0.0000453, &
        0.0000455,  0.0000458,  0.0000460,  0.0000463,  0.0000465, &
        0.0000468,  0.0000470/
      data ((coa(i,j),i=1,62),j= 12, 12)/ &
        0.0000173,  0.0000190,  0.0000205,  0.0000219,  0.0000231, &
        0.0000242,  0.0000252,  0.0000262,  0.0000271,  0.0000279, &
        0.0000287,  0.0000294,  0.0000301,  0.0000308,  0.0000314, &
        0.0000320,  0.0000326,  0.0000332,  0.0000330,  0.0000343, &
        0.0000348,  0.0000353,  0.0000358,  0.0000362,  0.0000367, &
        0.0000371,  0.0000376,  0.0000380,  0.0000384,  0.0000388, &
        0.0000392,  0.0000396,  0.0000399,  0.0000403,  0.0000407, &
        0.0000410,  0.0000414,  0.0000417,  0.0000420,  0.0000424, &
        0.0000427,  0.0000430,  0.0000433,  0.0000436,  0.0000439, &
        0.0000442,  0.0000445,  0.0000448,  0.0000451,  0.0000454, &
        0.0000457,  0.0000459,  0.0000462,  0.0000465,  0.0000468, &
        0.0000470,  0.0000473,  0.0000475,  0.0000478,  0.0000481, &
        0.0000483,  0.0000486/
      data ((coa(i,j),i=1,62),j= 13, 13)/ &
        0.0000186,  0.0000204,  0.0000219,  0.0000233,  0.0000246, &
        0.0000257,  0.0000268,  0.0000277,  0.0000286,  0.0000295, &
        0.0000303,  0.0000311,  0.0000318,  0.0000325,  0.0000331, &
        0.0000331,  0.0000344,  0.0000350,  0.0000355,  0.0000361, &
        0.0000366,  0.0000371,  0.0000376,  0.0000381,  0.0000386, &
        0.0000390,  0.0000395,  0.0000399,  0.0000403,  0.0000407, &
        0.0000412,  0.0000416,  0.0000419,  0.0000423,  0.0000427, &
        0.0000431,  0.0000434,  0.0000438,  0.0000441,  0.0000445, &
        0.0000448,  0.0000451,  0.0000455,  0.0000458,  0.0000461, &
        0.0000464,  0.0000467,  0.0000470,  0.0000473,  0.0000476, &
        0.0000479,  0.0000482,  0.0000485,  0.0000488,  0.0000491, &
        0.0000494,  0.0000497,  0.0000499,  0.0000502,  0.0000505, &
        0.0000507,  0.0000510/
      data ((coa(i,j),i=1,62),j= 14, 14)/ &
        0.0000198,  0.0000216,  0.0000232,  0.0000246,  0.0000259, &
        0.0000271,  0.0000281,  0.0000291,  0.0000301,  0.0000310, &
        0.0000318,  0.0000326,  0.0000333,  0.0000340,  0.0000347, &
        0.0000354,  0.0000360,  0.0000366,  0.0000372,  0.0000377, &
        0.0000383,  0.0000388,  0.0000393,  0.0000398,  0.0000403, &
        0.0000408,  0.0000412,  0.0000417,  0.0000421,  0.0000425, &
        0.0000430,  0.0000434,  0.0000438,  0.0000442,  0.0000446, &
        0.0000449,  0.0000453,  0.0000457,  0.0000461,  0.0000464, &
        0.0000468,  0.0000471,  0.0000475,  0.0000478,  0.0000481, &
        0.0000485,  0.0000488,  0.0000491,  0.0000494,  0.0000498, &
        0.0000501,  0.0000504,  0.0000507,  0.0000510,  0.0000513, &
        0.0000516,  0.0000519,  0.0000522,  0.0000524,  0.0000527, &
        0.0000530,  0.0000533/
      data ((coa(i,j),i=1,62),j= 15, 15)/ &
        0.0000209,  0.0000228,  0.0000244,  0.0000258,  0.0000271, &
        0.0000283,  0.0000294,  0.0000305,  0.0000314,  0.0000323, &
        0.0000332,  0.0000340,  0.0000347,  0.0000354,  0.0000361, &
        0.0000368,  0.0000375,  0.0000381,  0.0000387,  0.0000392, &
        0.0000398,  0.0000404,  0.0000409,  0.0000414,  0.0000419, &
        0.0000424,  0.0000429,  0.0000433,  0.0000438,  0.0000442, &
        0.0000447,  0.0000451,  0.0000455,  0.0000459,  0.0000463, &
        0.0000467,  0.0000471,  0.0000475,  0.0000479,  0.0000483, &
        0.0000486,  0.0000490,  0.0000493,  0.0000497,  0.0000501, &
        0.0000504,  0.0000507,  0.0000511,  0.0000514,  0.0000518, &
        0.0000521,  0.0000524,  0.0000527,  0.0000530,  0.0000534, &
        0.0000537,  0.0000540,  0.0000543,  0.0000546,  0.0000549, &
        0.0000552,  0.0000555/
      data ((coa(i,j),i=1,62),j= 16, 16)/ &
        0.0000221,  0.0000240,  0.0000257,  0.0000272,  0.0000285, &
        0.0000297,  0.0000308,  0.0000319,  0.0000329,  0.0000331, &
        0.0000347,  0.0000355,  0.0000363,  0.0000370,  0.0000377, &
        0.0000384,  0.0000391,  0.0000397,  0.0000404,  0.0000409, &
        0.0000415,  0.0000421,  0.0000426,  0.0000432,  0.0000437, &
        0.0000442,  0.0000447,  0.0000452,  0.0000456,  0.0000461, &
        0.0000466,  0.0000470,  0.0000475,  0.0000479,  0.0000483, &
        0.0000487,  0.0000491,  0.0000496,  0.0000500,  0.0000503, &
        0.0000507,  0.0000511,  0.0000515,  0.0000519,  0.0000523, &
        0.0000526,  0.0000530,  0.0000533,  0.0000537,  0.0000540, &
        0.0000544,  0.0000547,  0.0000551,  0.0000554,  0.0000558, &
        0.0000561,  0.0000564,  0.0000567,  0.0000571,  0.0000574, &
        0.0000577,  0.0000580/
      data ((coa(i,j),i=1,62),j= 17, 17)/ &
        0.0000234,  0.0000254,  0.0000271,  0.0000286,  0.0000300, &
        0.0000312,  0.0000324,  0.0000335,  0.0000345,  0.0000354, &
        0.0000363,  0.0000372,  0.0000380,  0.0000387,  0.0000395, &
        0.0000402,  0.0000409,  0.0000415,  0.0000422,  0.0000428, &
        0.0000434,  0.0000440,  0.0000446,  0.0000451,  0.0000457, &
        0.0000462,  0.0000467,  0.0000472,  0.0000477,  0.0000482, &
        0.0000487,  0.0000492,  0.0000496,  0.0000501,  0.0000505, &
        0.0000510,  0.0000514,  0.0000518,  0.0000523,  0.0000527, &
        0.0000531,  0.0000535,  0.0000539,  0.0000543,  0.0000547, &
        0.0000551,  0.0000555,  0.0000559,  0.0000562,  0.0000566, &
        0.0000570,  0.0000573,  0.0000577,  0.0000581,  0.0000584, &
        0.0000588,  0.0000591,  0.0000595,  0.0000598,  0.0000602, &
        0.0000605,  0.0000608/
      data ((coa(i,j),i=1,62),j= 18, 18)/ &
        0.0000248,  0.0000268,  0.0000285,  0.0000301,  0.0000315, &
        0.0000328,  0.0000340,  0.0000351,  0.0000362,  0.0000371, &
        0.0000381,  0.0000389,  0.0000398,  0.0000406,  0.0000413, &
        0.0000421,  0.0000428,  0.0000435,  0.0000442,  0.0000448, &
        0.0000454,  0.0000460,  0.0000466,  0.0000472,  0.0000478, &
        0.0000484,  0.0000489,  0.0000494,  0.0000500,  0.0000505, &
        0.0000510,  0.0000515,  0.0000520,  0.0000525,  0.0000530, &
        0.0000534,  0.0000539,  0.0000544,  0.0000548,  0.0000553, &
        0.0000557,  0.0000561,  0.0000566,  0.0000570,  0.0000574, &
        0.0000578,  0.0000582,  0.0000586,  0.0000590,  0.0000594, &
        0.0000598,  0.0000602,  0.0000606,  0.0000610,  0.0000614, &
        0.0000618,  0.0000621,  0.0000625,  0.0000629,  0.0000633, &
        0.0000636,  0.0000640/
      data ((coa(i,j),i=1,62),j= 19, 19)/ &
        0.0000260,  0.0000281,  0.0000299,  0.0000315,  0.0000330, &
        0.0000343,  0.0000355,  0.0000367,  0.0000377,  0.0000388, &
        0.0000397,  0.0000406,  0.0000415,  0.0000423,  0.0000431, &
        0.0000439,  0.0000446,  0.0000453,  0.0000460,  0.0000467, &
        0.0000474,  0.0000480,  0.0000487,  0.0000493,  0.0000499, &
        0.0000505,  0.0000510,  0.0000516,  0.0000522,  0.0000527, &
        0.0000533,  0.0000538,  0.0000543,  0.0000548,  0.0000553, &
        0.0000558,  0.0000563,  0.0000568,  0.0000573,  0.0000578, &
        0.0000582,  0.0000587,  0.0000591,  0.0000596,  0.0000601, &
        0.0000605,  0.0000609,  0.0000614,  0.0000618,  0.0000622, &
        0.0000626,  0.0000631,  0.0000635,  0.0000639,  0.0000643, &
        0.0000647,  0.0000651,  0.0000655,  0.0000659,  0.0000663, &
        0.0000667,  0.0000670/
      data ((coa(i,j),i=1,62),j= 20, 20)/ &
        0.0000275,  0.0000296,  0.0000315,  0.0000332,  0.0000347, &
        0.0000360,  0.0000373,  0.0000385,  0.0000396,  0.0000407, &
        0.0000417,  0.0000426,  0.0000435,  0.0000444,  0.0000452, &
        0.0000460,  0.0000468,  0.0000476,  0.0000483,  0.0000490, &
        0.0000497,  0.0000504,  0.0000511,  0.0000517,  0.0000524, &
        0.0000530,  0.0000536,  0.0000542,  0.0000548,  0.0000554, &
        0.0000560,  0.0000566,  0.0000571,  0.0000577,  0.0000582, &
        0.0000587,  0.0000593,  0.0000598,  0.0000603,  0.0000608, &
        0.0000613,  0.0000618,  0.0000623,  0.0000628,  0.0000633, &
        0.0000638,  0.0000642,  0.0000647,  0.0000652,  0.0000656, &
        0.0000661,  0.0000665,  0.0000670,  0.0000674,  0.0000678, &
        0.0000683,  0.0000687,  0.0000691,  0.0000695,  0.0000700, &
        0.0000704,  0.0000708/
      data ((coa(i,j),i=1,62),j= 21, 21)/ &
        0.0000290,  0.0000312,  0.0000331,  0.0000349,  0.0000364, &
        0.0000379,  0.0000392,  0.0000404,  0.0000416,  0.0000427, &
        0.0000437,  0.0000447,  0.0000457,  0.0000466,  0.0000475, &
        0.0000483,  0.0000492,  0.0000500,  0.0000507,  0.0000515, &
        0.0000523,  0.0000530,  0.0000537,  0.0000544,  0.0000551, &
        0.0000558,  0.0000564,  0.0000571,  0.0000577,  0.0000583, &
        0.0000589,  0.0000596,  0.0000602,  0.0000607,  0.0000613, &
        0.0000619,  0.0000625,  0.0000630,  0.0000636,  0.0000641, &
        0.0000647,  0.0000652,  0.0000657,  0.0000663,  0.0000668, &
        0.0000673,  0.0000678,  0.0000683,  0.0000688,  0.0000693, &
        0.0000698,  0.0000702,  0.0000707,  0.0000712,  0.0000716, &
        0.0000721,  0.0000726,  0.0000730,  0.0000735,  0.0000739, &
        0.0000744,  0.0000748/
      data ((coa(i,j),i=1,62),j= 22, 22)/ &
        0.0000306,  0.0000329,  0.0000349,  0.0000366,  0.0000383, &
        0.0000398,  0.0000411,  0.0000424,  0.0000436,  0.0000448, &
        0.0000459,  0.0000469,  0.0000479,  0.0000489,  0.0000499, &
        0.0000508,  0.0000516,  0.0000525,  0.0000533,  0.0000542, &
        0.0000549,  0.0000557,  0.0000565,  0.0000572,  0.0000580, &
        0.0000587,  0.0000594,  0.0000601,  0.0000608,  0.0000615, &
        0.0000621,  0.0000628,  0.0000634,  0.0000640,  0.0000647, &
        0.0000653,  0.0000659,  0.0000665,  0.0000671,  0.0000677, &
        0.0000683,  0.0000688,  0.0000694,  0.0000700,  0.0000705, &
        0.0000711,  0.0000716,  0.0000721,  0.0000727,  0.0000732, &
        0.0000737,  0.0000742,  0.0000747,  0.0000752,  0.0000757, &
        0.0000762,  0.0000767,  0.0000772,  0.0000777,  0.0000782, &
        0.0000786,  0.0000791/
      data ((coa(i,j),i=1,62),j= 23, 23)/ &
        0.0000323,  0.0000347,  0.0000368,  0.0000386,  0.0000403, &
        0.0000419,  0.0000433,  0.0000447,  0.0000459,  0.0000472, &
        0.0000483,  0.0000494,  0.0000505,  0.0000516,  0.0000526, &
        0.0000535,  0.0000545,  0.0000554,  0.0000563,  0.0000572, &
        0.0000580,  0.0000589,  0.0000597,  0.0000605,  0.0000613, &
        0.0000621,  0.0000628,  0.0000636,  0.0000643,  0.0000650, &
        0.0000657,  0.0000664,  0.0000671,  0.0000678,  0.0000685, &
        0.0000692,  0.0000698,  0.0000705,  0.0000711,  0.0000717, &
        0.0000724,  0.0000730,  0.0000736,  0.0000742,  0.0000748, &
        0.0000754,  0.0000760,  0.0000765,  0.0000771,  0.0000777, &
        0.0000782,  0.0000788,  0.0000793,  0.0000799,  0.0000804, &
        0.0000809,  0.0000815,  0.0000820,  0.0000825,  0.0000830, &
        0.0000835,  0.0000840/
      data ((coa(i,j),i=1,62),j= 24, 24)/ &
        0.0000341,  0.0000365,  0.0000387,  0.0000406,  0.0000424, &
        0.0000440,  0.0000456,  0.0000470,  0.0000483,  0.0000496, &
        0.0000509,  0.0000521,  0.0000532,  0.0000543,  0.0000554, &
        0.0000564,  0.0000574,  0.0000584,  0.0000594,  0.0000603, &
        0.0000613,  0.0000622,  0.0000630,  0.0000639,  0.0000648, &
        0.0000656,  0.0000664,  0.0000672,  0.0000680,  0.0000688, &
        0.0000696,  0.0000703,  0.0000711,  0.0000718,  0.0000725, &
        0.0000732,  0.0000739,  0.0000746,  0.0000753,  0.0000760, &
        0.0000767,  0.0000773,  0.0000780,  0.0000786,  0.0000793, &
        0.0000799,  0.0000805,  0.0000811,  0.0000817,  0.0000823, &
        0.0000829,  0.0000835,  0.0000841,  0.0000847,  0.0000853, &
        0.0000858,  0.0000864,  0.0000870,  0.0000875,  0.0000881, &
        0.0000886,  0.0000892/
      data ((coa(i,j),i=1,62),j= 25, 25)/ &
        0.0000359,  0.0000385,  0.0000408,  0.0000428,  0.0000447, &
        0.0000464,  0.0000480,  0.0000495,  0.0000510,  0.0000524, &
        0.0000537,  0.0000550,  0.0000562,  0.0000574,  0.0000585, &
        0.0000597,  0.0000608,  0.0000618,  0.0000629,  0.0000639, &
        0.0000649,  0.0000658,  0.0000668,  0.0000677,  0.0000686, &
        0.0000695,  0.0000704,  0.0000713,  0.0000721,  0.0000730, &
        0.0000738,  0.0000746,  0.0000754,  0.0000762,  0.0000770, &
        0.0000777,  0.0000785,  0.0000792,  0.0000800,  0.0000807, &
        0.0000814,  0.0000821,  0.0000828,  0.0000835,  0.0000842, &
        0.0000849,  0.0000856,  0.0000862,  0.0000869,  0.0000875, &
        0.0000882,  0.0000888,  0.0000894,  0.0000900,  0.0000907, &
        0.0000913,  0.0000919,  0.0000925,  0.0000931,  0.0000936, &
        0.0000942,  0.0000948/
      data ((coa(i,j),i=1,62),j= 26, 26)/ &
        0.0000380,  0.0000407,  0.0000431,  0.0000453,  0.0000473, &
        0.0000491,  0.0000508,  0.0000525,  0.0000540,  0.0000555, &
        0.0000569,  0.0000583,  0.0000596,  0.0000609,  0.0000622, &
        0.0000634,  0.0000646,  0.0000657,  0.0000668,  0.0000679, &
        0.0000690,  0.0000700,  0.0000711,  0.0000721,  0.0000731, &
        0.0000740,  0.0000750,  0.0000759,  0.0000769,  0.0000778, &
        0.0000786,  0.0000795,  0.0000804,  0.0000812,  0.0000821, &
        0.0000829,  0.0000837,  0.0000845,  0.0000853,  0.0000861, &
        0.0000869,  0.0000876,  0.0000884,  0.0000891,  0.0000899, &
        0.0000906,  0.0000913,  0.0000920,  0.0000927,  0.0000934, &
        0.0000941,  0.0000948,  0.0000955,  0.0000961,  0.0000968, &
        0.0000974,  0.0000981,  0.0000987,  0.0000994,  0.0001000, &
        0.0001006,  0.0001012/
      data ((coa(i,j),i=1,62),j= 27, 27)/ &
        0.0000403,  0.0000431,  0.0000456,  0.0000479,  0.0000500, &
        0.0000520,  0.0000538,  0.0000556,  0.0000573,  0.0000589, &
        0.0000604,  0.0000619,  0.0000633,  0.0000647,  0.0000661, &
        0.0000674,  0.0000686,  0.0000699,  0.0000711,  0.0000723, &
        0.0000734,  0.0000746,  0.0000757,  0.0000768,  0.0000778, &
        0.0000789,  0.0000799,  0.0000809,  0.0000819,  0.0000829, &
        0.0000838,  0.0000848,  0.0000857,  0.0000866,  0.0000875, &
        0.0000884,  0.0000893,  0.0000902,  0.0000910,  0.0000919, &
        0.0000927,  0.0000935,  0.0000943,  0.0000951,  0.0000959, &
        0.0000967,  0.0000974,  0.0000982,  0.0000990,  0.0000997, &
        0.0001004,  0.0001012,  0.0001019,  0.0001026,  0.0001033, &
        0.0001040,  0.0001047,  0.0001054,  0.0001061,  0.0001067, &
        0.0001074,  0.0001080/
      data ((coa(i,j),i=1,62),j= 28, 28)/ &
        0.0000426,  0.0000456,  0.0000482,  0.0000507,  0.0000529, &
        0.0000550,  0.0000570,  0.0000589,  0.0000607,  0.0000624, &
        0.0000641,  0.0000657,  0.0000672,  0.0000687,  0.0000702, &
        0.0000716,  0.0000730,  0.0000743,  0.0000756,  0.0000769, &
        0.0000781,  0.0000794,  0.0000806,  0.0000817,  0.0000829, &
        0.0000840,  0.0000851,  0.0000862,  0.0000873,  0.0000883, &
        0.0000893,  0.0000904,  0.0000913,  0.0000923,  0.0000933, &
        0.0000943,  0.0000952,  0.0000961,  0.0000970,  0.0000979, &
        0.0000988,  0.0000997,  0.0001006,  0.0001014,  0.0001023, &
        0.0001031,  0.0001039,  0.0001047,  0.0001055,  0.0001063, &
        0.0001071,  0.0001079,  0.0001087,  0.0001094,  0.0001102, &
        0.0001109,  0.0001116,  0.0001124,  0.0001131,  0.0001138, &
        0.0001145,  0.0001152/
      data ((coa(i,j),i=1,62),j= 29, 29)/ &
        0.0000451,  0.0000482,  0.0000511,  0.0000537,  0.0000561, &
        0.0000584,  0.0000605,  0.0000626,  0.0000645,  0.0000664, &
        0.0000682,  0.0000699,  0.0000715,  0.0000732,  0.0000747, &
        0.0000763,  0.0000777,  0.0000792,  0.0000806,  0.0000820, &
        0.0000833,  0.0000846,  0.0000859,  0.0000872,  0.0000884, &
        0.0000896,  0.0000908,  0.0000920,  0.0000931,  0.0000942, &
        0.0000953,  0.0000964,  0.0000975,  0.0000986,  0.0000996, &
        0.0001006,  0.0001016,  0.0001026,  0.0001036,  0.0001046, &
        0.0001055,  0.0001064,  0.0001074,  0.0001083,  0.0001092, &
        0.0001101,  0.0001110,  0.0001118,  0.0001127,  0.0001135, &
        0.0001144,  0.0001152,  0.0001160,  0.0001168,  0.0001176, &
        0.0001184,  0.0001192,  0.0001200,  0.0001207,  0.0001215, &
        0.0001222,  0.0001230/
      data ((coa(i,j),i=1,62),j= 30, 30)/ &
        0.0000478,  0.0000512,  0.0000543,  0.0000571,  0.0000597, &
        0.0000621,  0.0000644,  0.0000666,  0.0000687,  0.0000708, &
        0.0000727,  0.0000746,  0.0000764,  0.0000781,  0.0000798, &
        0.0000814,  0.0000830,  0.0000846,  0.0000861,  0.0000876, &
        0.0000891,  0.0000905,  0.0000919,  0.0000932,  0.0000945, &
        0.0000958,  0.0000971,  0.0000984,  0.0000996,  0.0001008, &
        0.0001020,  0.0001032,  0.0001043,  0.0001055,  0.0001066, &
        0.0001077,  0.0001088,  0.0001098,  0.0001109,  0.0001119, &
        0.0001129,  0.0001139,  0.0001149,  0.0001159,  0.0001168, &
        0.0001178,  0.0001187,  0.0001197,  0.0001206,  0.0001215, &
        0.0001224,  0.0001233,  0.0001241,  0.0001250,  0.0001258, &
        0.0001267,  0.0001275,  0.0001283,  0.0001292,  0.0001300, &
        0.0001308,  0.0001316/
      data ((coa(i,j),i=1,62),j= 31, 31)/ &
        0.0000508,  0.0000544,  0.0000577,  0.0000607,  0.0000635, &
        0.0000661,  0.0000686,  0.0000710,  0.0000733,  0.0000754, &
        0.0000775,  0.0000795,  0.0000815,  0.0000834,  0.0000852, &
        0.0000870,  0.0000887,  0.0000904,  0.0000920,  0.0000936, &
        0.0000952,  0.0000967,  0.0000982,  0.0000996,  0.0001011, &
        0.0001025,  0.0001038,  0.0001052,  0.0001065,  0.0001078, &
        0.0001091,  0.0001103,  0.0001116,  0.0001128,  0.0001140, &
        0.0001151,  0.0001163,  0.0001174,  0.0001186,  0.0001197, &
        0.0001207,  0.0001218,  0.0001229,  0.0001239,  0.0001249, &
        0.0001260,  0.0001270,  0.0001279,  0.0001289,  0.0001299, &
        0.0001308,  0.0001318,  0.0001327,  0.0001336,  0.0001317, &
        0.0001325,  0.0001363,  0.0001372,  0.0001380,  0.0001389, &
        0.0001397,  0.0001406/
      data ((coa(i,j),i=1,62),j= 32, 32)/ &
        0.0000540,  0.0000579,  0.0000615,  0.0000647,  0.0000677, &
        0.0000706,  0.0000733,  0.0000758,  0.0000783,  0.0000806, &
        0.0000829,  0.0000851,  0.0000872,  0.0000892,  0.0000912, &
        0.0000931,  0.0000950,  0.0000968,  0.0000985,  0.0001003, &
        0.0001020,  0.0001036,  0.0001052,  0.0001068,  0.0001083, &
        0.0001098,  0.0001113,  0.0001127,  0.0001142,  0.0001156, &
        0.0001169,  0.0001183,  0.0001196,  0.0001209,  0.0001222, &
        0.0001234,  0.0001246,  0.0001259,  0.0001270,  0.0001282, &
        0.0001294,  0.0001305,  0.0001317,  0.0001328,  0.0001339, &
        0.0001321,  0.0001360,  0.0001371,  0.0001381,  0.0001391, &
        0.0001401,  0.0001411,  0.0001421,  0.0001431,  0.0001440, &
        0.0001450,  0.0001459,  0.0001469,  0.0001478,  0.0001487, &
        0.0001496,  0.0001505/
      data ((coa(i,j),i=1,62),j= 33, 33)/ &
        0.0000575,  0.0000617,  0.0000655,  0.0000690,  0.0000723, &
        0.0000754,  0.0000783,  0.0000810,  0.0000837,  0.0000862, &
        0.0000887,  0.0000910,  0.0000933,  0.0000955,  0.0000976, &
        0.0000997,  0.0001017,  0.0001036,  0.0001055,  0.0001074, &
        0.0001092,  0.0001110,  0.0001127,  0.0001144,  0.0001160, &
        0.0001176,  0.0001192,  0.0001208,  0.0001223,  0.0001238, &
        0.0001252,  0.0001267,  0.0001281,  0.0001295,  0.0001308, &
        0.0001322,  0.0001335,  0.0001319,  0.0001360,  0.0001373, &
        0.0001385,  0.0001397,  0.0001409,  0.0001421,  0.0001433, &
        0.0001444,  0.0001456,  0.0001467,  0.0001478,  0.0001489, &
        0.0001499,  0.0001510,  0.0001520,  0.0001531,  0.0001541, &
        0.0001551,  0.0001561,  0.0001571,  0.0001581,  0.0001590, &
        0.0001600,  0.0001609/
      data ((coa(i,j),i=1,62),j= 34, 34)/ &
        0.0000613,  0.0000659,  0.0000700,  0.0000738,  0.0000773, &
        0.0000806,  0.0000838,  0.0000868,  0.0000896,  0.0000924, &
        0.0000950,  0.0000976,  0.0001000,  0.0001024,  0.0001047, &
        0.0001069,  0.0001091,  0.0001112,  0.0001132,  0.0001152, &
        0.0001172,  0.0001191,  0.0001209,  0.0001227,  0.0001245, &
        0.0001262,  0.0001279,  0.0001296,  0.0001312,  0.0001328, &
        0.0001344,  0.0001359,  0.0001374,  0.0001389,  0.0001403, &
        0.0001417,  0.0001432,  0.0001445,  0.0001459,  0.0001472, &
        0.0001485,  0.0001498,  0.0001511,  0.0001524,  0.0001536, &
        0.0001548,  0.0001560,  0.0001572,  0.0001584,  0.0001595, &
        0.0001607,  0.0001618,  0.0001629,  0.0001640,  0.0001651, &
        0.0001661,  0.0001672,  0.0001682,  0.0001693,  0.0001703, &
        0.0001713,  0.0001723/
      data ((coa(i,j),i=1,62),j= 35, 35)/ &
        0.0000654,  0.0000703,  0.0000747,  0.0000789,  0.0000827, &
        0.0000863,  0.0000897,  0.0000929,  0.0000960,  0.0000990, &
        0.0001018,  0.0001046,  0.0001072,  0.0001098,  0.0001123, &
        0.0001147,  0.0001170,  0.0001193,  0.0001214,  0.0001236, &
        0.0001257,  0.0001277,  0.0001297,  0.0001316,  0.0001335, &
        0.0001325,  0.0001372,  0.0001389,  0.0001407,  0.0001424, &
        0.0001440,  0.0001457,  0.0001473,  0.0001488,  0.0001504, &
        0.0001519,  0.0001534,  0.0001548,  0.0001563,  0.0001577, &
        0.0001591,  0.0001605,  0.0001618,  0.0001631,  0.0001645, &
        0.0001658,  0.0001670,  0.0001683,  0.0001695,  0.0001707, &
        0.0001720,  0.0001732,  0.0001743,  0.0001755,  0.0001767, &
        0.0001778,  0.0001789,  0.0001800,  0.0001811,  0.0001822, &
        0.0001833,  0.0001844/
      data ((coa(i,j),i=1,62),j= 36, 36)/ &
        0.0000699,  0.0000752,  0.0000800,  0.0000844,  0.0000886, &
        0.0000925,  0.0000962,  0.0000997,  0.0001030,  0.0001062, &
        0.0001093,  0.0001123,  0.0001151,  0.0001179,  0.0001205, &
        0.0001231,  0.0001256,  0.0001280,  0.0001304,  0.0001327, &
        0.0001321,  0.0001371,  0.0001392,  0.0001413,  0.0001433, &
        0.0001453,  0.0001472,  0.0001491,  0.0001509,  0.0001527, &
        0.0001545,  0.0001562,  0.0001579,  0.0001596,  0.0001612, &
        0.0001629,  0.0001644,  0.0001660,  0.0001675,  0.0001690, &
        0.0001705,  0.0001720,  0.0001734,  0.0001749,  0.0001762, &
        0.0001776,  0.0001790,  0.0001803,  0.0001817,  0.0001830, &
        0.0001842,  0.0001855,  0.0001868,  0.0001880,  0.0001892, &
        0.0001905,  0.0001917,  0.0001928,  0.0001940,  0.0001952, &
        0.0001963,  0.0001975/
      data ((coa(i,j),i=1,62),j= 37, 37)/ &
        0.0000748,  0.0000805,  0.0000858,  0.0000906,  0.0000951, &
        0.0000993,  0.0001033,  0.0001071,  0.0001107,  0.0001142, &
        0.0001175,  0.0001207,  0.0001238,  0.0001267,  0.0001296, &
        0.0001323,  0.0001322,  0.0001376,  0.0001401,  0.0001426, &
        0.0001450,  0.0001473,  0.0001496,  0.0001518,  0.0001539, &
        0.0001560,  0.0001581,  0.0001601,  0.0001620,  0.0001640, &
        0.0001659,  0.0001677,  0.0001695,  0.0001713,  0.0001731, &
        0.0001748,  0.0001765,  0.0001781,  0.0001798,  0.0001814, &
        0.0001830,  0.0001845,  0.0001861,  0.0001876,  0.0001891, &
        0.0001905,  0.0001920,  0.0001934,  0.0001948,  0.0001962, &
        0.0001976,  0.0001990,  0.0002003,  0.0002017,  0.0002030, &
        0.0002043,  0.0002056,  0.0002068,  0.0002081,  0.0002093, &
        0.0002106,  0.0002118/
      data ((coa(i,j),i=1,62),j= 38, 38)/ &
        0.0000802,  0.0000863,  0.0000920,  0.0000972,  0.0001021, &
        0.0001067,  0.0001110,  0.0001151,  0.0001190,  0.0001227, &
        0.0001263,  0.0001297,  0.0001330,  0.0001362,  0.0001393, &
        0.0001422,  0.0001451,  0.0001479,  0.0001506,  0.0001532, &
        0.0001557,  0.0001582,  0.0001606,  0.0001630,  0.0001653, &
        0.0001675,  0.0001697,  0.0001719,  0.0001740,  0.0001760, &
        0.0001780,  0.0001800,  0.0001819,  0.0001839,  0.0001857, &
        0.0001876,  0.0001894,  0.0001911,  0.0001929,  0.0001946, &
        0.0001963,  0.0001980,  0.0001996,  0.0002012,  0.0002028, &
        0.0002044,  0.0002060,  0.0002075,  0.0002090,  0.0002105, &
        0.0002120,  0.0002135,  0.0002149,  0.0002164,  0.0002178, &
        0.0002192,  0.0002205,  0.0002219,  0.0002233,  0.0002246, &
        0.0002259,  0.0002273/
      data ((coa(i,j),i=1,62),j= 39, 39)/ &
        0.0000859,  0.0000926,  0.0000987,  0.0001044,  0.0001097, &
        0.0001146,  0.0001193,  0.0001237,  0.0001279,  0.0001319, &
        0.0001358,  0.0001395,  0.0001430,  0.0001464,  0.0001497, &
        0.0001528,  0.0001559,  0.0001589,  0.0001617,  0.0001645, &
        0.0001673,  0.0001699,  0.0001725,  0.0001750,  0.0001774, &
        0.0001798,  0.0001822,  0.0001845,  0.0001867,  0.0001889, &
        0.0001911,  0.0001932,  0.0001953,  0.0001973,  0.0001993, &
        0.0002013,  0.0002032,  0.0002051,  0.0002070,  0.0002088, &
        0.0002107,  0.0002124,  0.0002142,  0.0002160,  0.0002177, &
        0.0002194,  0.0002211,  0.0002227,  0.0002243,  0.0002260, &
        0.0002276,  0.0002291,  0.0002307,  0.0002322,  0.0002338, &
        0.0002353,  0.0002368,  0.0002382,  0.0002397,  0.0002412, &
        0.0002426,  0.0002440/
      data ((coa(i,j),i=1,62),j= 40, 40)/ &
        0.0000922,  0.0000995,  0.0001061,  0.0001122,  0.0001179, &
        0.0001233,  0.0001283,  0.0001331,  0.0001376,  0.0001419, &
        0.0001460,  0.0001500,  0.0001538,  0.0001574,  0.0001609, &
        0.0001643,  0.0001676,  0.0001707,  0.0001738,  0.0001768, &
        0.0001797,  0.0001825,  0.0001853,  0.0001880,  0.0001906, &
        0.0001932,  0.0001957,  0.0001981,  0.0002006,  0.0002029, &
        0.0002052,  0.0002075,  0.0002097,  0.0002119,  0.0002141, &
        0.0002162,  0.0002183,  0.0002203,  0.0002223,  0.0002243, &
        0.0002263,  0.0002282,  0.0002301,  0.0002320,  0.0002339, &
        0.0002357,  0.0002375,  0.0002393,  0.0002411,  0.0002428, &
        0.0002446,  0.0002463,  0.0002480,  0.0002496,  0.0002513, &
        0.0002529,  0.0002546,  0.0002562,  0.0002578,  0.0002593, &
        0.0002609,  0.0002625/
      data ((coa(i,j),i=1,62),j= 41, 41)/ &
        0.0000990,  0.0001069,  0.0001141,  0.0001207,  0.0001268, &
        0.0001326,  0.0001380,  0.0001431,  0.0001480,  0.0001526, &
        0.0001570,  0.0001612,  0.0001653,  0.0001692,  0.0001729, &
        0.0001766,  0.0001801,  0.0001835,  0.0001868,  0.0001900, &
        0.0001931,  0.0001961,  0.0001991,  0.0002019,  0.0002048, &
        0.0002075,  0.0002102,  0.0002129,  0.0002154,  0.0002180, &
        0.0002205,  0.0002229,  0.0002253,  0.0002277,  0.0002300, &
        0.0002323,  0.0002346,  0.0002368,  0.0002390,  0.0002411, &
        0.0002432,  0.0002453,  0.0002474,  0.0002494,  0.0002515, &
        0.0002535,  0.0002554,  0.0002574,  0.0002593,  0.0002612, &
        0.0002631,  0.0002649,  0.0002668,  0.0002686,  0.0002704, &
        0.0002722,  0.0002740,  0.0002757,  0.0002775,  0.0002792, &
        0.0002809,  0.0002826/
      data ((coa(i,j),i=1,62),j= 42, 42)/ &
        0.0001063,  0.0001148,  0.0001226,  0.0001297,  0.0001363, &
        0.0001425,  0.0001483,  0.0001538,  0.0001590,  0.0001639, &
        0.0001687,  0.0001732,  0.0001775,  0.0001817,  0.0001857, &
        0.0001896,  0.0001933,  0.0001970,  0.0002005,  0.0002039, &
        0.0002073,  0.0002105,  0.0002137,  0.0002168,  0.0002198, &
        0.0002228,  0.0002257,  0.0002286,  0.0002314,  0.0002341, &
        0.0002368,  0.0002394,  0.0002420,  0.0002446,  0.0002471, &
        0.0002496,  0.0002520,  0.0002544,  0.0002568,  0.0002591, &
        0.0002615,  0.0002637,  0.0002660,  0.0002682,  0.0002704, &
        0.0002726,  0.0002747,  0.0002768,  0.0002789,  0.0002810, &
        0.0002831,  0.0002851,  0.0002871,  0.0002891,  0.0002911, &
        0.0002930,  0.0002950,  0.0002969,  0.0002988,  0.0003007, &
        0.0003025,  0.0003044/
      data ((coa(i,j),i=1,62),j= 43, 43)/ &
        0.0001141,  0.0001233,  0.0001316,  0.0001393,  0.0001464, &
        0.0001531,  0.0001593,  0.0001652,  0.0001707,  0.0001760, &
        0.0001811,  0.0001859,  0.0001905,  0.0001950,  0.0001993, &
        0.0002035,  0.0002075,  0.0002114,  0.0002152,  0.0002189, &
        0.0002225,  0.0002260,  0.0002294,  0.0002328,  0.0002360, &
        0.0002393,  0.0002424,  0.0002455,  0.0002485,  0.0002515, &
        0.0002544,  0.0002573,  0.0002601,  0.0002629,  0.0002656, &
        0.0002683,  0.0002709,  0.0002736,  0.0002762,  0.0002787, &
        0.0002812,  0.0002837,  0.0002862,  0.0002886,  0.0002910, &
        0.0002934,  0.0002957,  0.0002980,  0.0003003,  0.0003026, &
        0.0003048,  0.0003071,  0.0003093,  0.0003114,  0.0003136, &
        0.0003157,  0.0003179,  0.0003200,  0.0003221,  0.0003241, &
        0.0003262,  0.0003282/
      data ((coa(i,j),i=1,62),j= 44, 44)/ &
        0.0001224,  0.0001323,  0.0001413,  0.0001496,  0.0001572, &
        0.0001643,  0.0001709,  0.0001772,  0.0001832,  0.0001888, &
        0.0001943,  0.0001994,  0.0002044,  0.0002092,  0.0002138, &
        0.0002183,  0.0002226,  0.0002269,  0.0002309,  0.0002349, &
        0.0002388,  0.0002426,  0.0002463,  0.0002499,  0.0002535, &
        0.0002570,  0.0002604,  0.0002637,  0.0002670,  0.0002702, &
        0.0002734,  0.0002765,  0.0002796,  0.0002826,  0.0002856, &
        0.0002886,  0.0002915,  0.0002943,  0.0002972,  0.0002999, &
        0.0003027,  0.0003054,  0.0003081,  0.0003108,  0.0003134, &
        0.0003160,  0.0003185,  0.0003211,  0.0003236,  0.0003261, &
        0.0003286,  0.0003310,  0.0003334,  0.0003358,  0.0003382, &
        0.0003405,  0.0003428,  0.0003451,  0.0003474,  0.0003497, &
        0.0003519,  0.0003542/
      data ((coa(i,j),i=1,62),j= 45, 45)/ &
        0.0001312,  0.0001419,  0.0001515,  0.0001603,  0.0001685, &
        0.0001761,  0.0001832,  0.0001899,  0.0001963,  0.0002024, &
        0.0002082,  0.0002138,  0.0002191,  0.0002243,  0.0002292, &
        0.0002341,  0.0002387,  0.0002433,  0.0002477,  0.0002520, &
        0.0002562,  0.0002603,  0.0002644,  0.0002683,  0.0002722, &
        0.0002759,  0.0002796,  0.0002833,  0.0002869,  0.0002904, &
        0.0002939,  0.0002973,  0.0003006,  0.0003039,  0.0003072, &
        0.0003104,  0.0003136,  0.0003167,  0.0003198,  0.0003228, &
        0.0003259,  0.0003288,  0.0003318,  0.0003347,  0.0003376, &
        0.0003404,  0.0003432,  0.0003460,  0.0003487,  0.0003515, &
        0.0003542,  0.0003568,  0.0003595,  0.0003621,  0.0003647, &
        0.0003673,  0.0003698,  0.0003724,  0.0003749,  0.0003773, &
        0.0003798,  0.0003822/
      data ((coa(i,j),i=1,62),j= 46, 46)/ &
        0.0001406,  0.0001520,  0.0001623,  0.0001718,  0.0001805, &
        0.0001886,  0.0001963,  0.0002035,  0.0002103,  0.0002168, &
        0.0002231,  0.0002291,  0.0002348,  0.0002404,  0.0002458, &
        0.0002510,  0.0002561,  0.0002610,  0.0002658,  0.0002705, &
        0.0002750,  0.0002795,  0.0002839,  0.0002882,  0.0002924, &
        0.0002965,  0.0003005,  0.0003045,  0.0003084,  0.0003123, &
        0.0003161,  0.0003198,  0.0003235,  0.0003271,  0.0003307, &
        0.0003342,  0.0003376,  0.0003411,  0.0003445,  0.0003478, &
        0.0003511,  0.0003544,  0.0003576,  0.0003608,  0.0003639, &
        0.0003670,  0.0003701,  0.0003731,  0.0003762,  0.0003791, &
        0.0003821,  0.0003850,  0.0003879,  0.0003908,  0.0003936, &
        0.0003965,  0.0003992,  0.0004020,  0.0004047,  0.0004075, &
        0.0004102,  0.0004128/
      data ((coa(i,j),i=1,62),j= 47, 47)/ &
        0.0001506,  0.0001628,  0.0001739,  0.0001840,  0.0001934, &
        0.0002021,  0.0002103,  0.0002180,  0.0002254,  0.0002324, &
        0.0002391,  0.0002456,  0.0002518,  0.0002579,  0.0002637, &
        0.0002694,  0.0002749,  0.0002802,  0.0002854,  0.0002905, &
        0.0002955,  0.0003004,  0.0003052,  0.0003099,  0.0003145, &
        0.0003190,  0.0003234,  0.0003278,  0.0003320,  0.0003362, &
        0.0003404,  0.0003445,  0.0003485,  0.0003524,  0.0003564, &
        0.0003602,  0.0003640,  0.0003678,  0.0003715,  0.0003751, &
        0.0003787,  0.0003823,  0.0003858,  0.0003893,  0.0003928, &
        0.0003962,  0.0003995,  0.0004029,  0.0004062,  0.0004094, &
        0.0004127,  0.0004159,  0.0004190,  0.0004222,  0.0004253, &
        0.0004283,  0.0004314,  0.0004344,  0.0004374,  0.0004404, &
        0.0004433,  0.0004462/
      data ((coa(i,j),i=1,62),j= 48, 48)/ &
        0.0001613,  0.0001744,  0.0001863,  0.0001971,  0.0002072, &
        0.0002165,  0.0002254,  0.0002337,  0.0002417,  0.0002493, &
        0.0002565,  0.0002636,  0.0002703,  0.0002769,  0.0002832, &
        0.0002894,  0.0002954,  0.0003013,  0.0003070,  0.0003125, &
        0.0003180,  0.0003233,  0.0003286,  0.0003337,  0.0003387, &
        0.0003436,  0.0003485,  0.0003533,  0.0003579,  0.0003625, &
        0.0003671,  0.0003716,  0.0003760,  0.0003803,  0.0003846, &
        0.0003888,  0.0003929,  0.0003971,  0.0004011,  0.0004051, &
        0.0004091,  0.0004130,  0.0004168,  0.0004206,  0.0004244, &
        0.0004281,  0.0004318,  0.0004354,  0.0004390,  0.0004426, &
        0.0004461,  0.0004496,  0.0004530,  0.0004565,  0.0004598, &
        0.0004632,  0.0004665,  0.0004698,  0.0004730,  0.0004763, &
        0.0004795,  0.0004826/
      data ((coa(i,j),i=1,62),j= 49, 49)/ &
        0.0001728,  0.0001868,  0.0001996,  0.0002112,  0.0002220, &
        0.0002321,  0.0002417,  0.0002507,  0.0002593,  0.0002676, &
        0.0002755,  0.0002831,  0.0002905,  0.0002977,  0.0003046, &
        0.0003113,  0.0003179,  0.0003243,  0.0003305,  0.0003366, &
        0.0003426,  0.0003484,  0.0003542,  0.0003598,  0.0003653, &
        0.0003707,  0.0003760,  0.0003812,  0.0003863,  0.0003914, &
        0.0003963,  0.0004012,  0.0004060,  0.0004108,  0.0004154, &
        0.0004201,  0.0004246,  0.0004291,  0.0004335,  0.0004379, &
        0.0004422,  0.0004464,  0.0004506,  0.0004548,  0.0004589, &
        0.0004629,  0.0004669,  0.0004709,  0.0004748,  0.0004787, &
        0.0004825,  0.0004863,  0.0004900,  0.0004937,  0.0004974, &
        0.0005010,  0.0005046,  0.0005082,  0.0005117,  0.0005152, &
        0.0005187,  0.0005221/
      data ((coa(i,j),i=1,62),j= 50, 50)/ &
        0.0001851,  0.0002003,  0.0002139,  0.0002265,  0.0002382, &
        0.0002491,  0.0002595,  0.0002693,  0.0002787,  0.0002877, &
        0.0002963,  0.0003047,  0.0003127,  0.0003205,  0.0003281, &
        0.0003355,  0.0003427,  0.0003497,  0.0003565,  0.0003632, &
        0.0003697,  0.0003761,  0.0003824,  0.0003885,  0.0003945, &
        0.0004004,  0.0004062,  0.0004119,  0.0004175,  0.0004230, &
        0.0004285,  0.0004338,  0.0004390,  0.0004442,  0.0004493, &
        0.0004543,  0.0004593,  0.0004641,  0.0004689,  0.0004737, &
        0.0004784,  0.0004830,  0.0004875,  0.0004920,  0.0004965, &
        0.0005009,  0.0005052,  0.0005095,  0.0005138,  0.0005179, &
        0.0005221,  0.0005262,  0.0005302,  0.0005342,  0.0005268, &
        0.0005421,  0.0005460,  0.0005499,  0.0005537,  0.0005574, &
        0.0005612,  0.0005649/
      data ((coa(i,j),i=1,62),j= 51, 51)/ &
        0.0001985,  0.0002149,  0.0002297,  0.0002433,  0.0002559, &
        0.0002679,  0.0002791,  0.0002898,  0.0003001,  0.0003099, &
        0.0003193,  0.0003285,  0.0003373,  0.0003459,  0.0003542, &
        0.0003622,  0.0003701,  0.0003778,  0.0003853,  0.0003926, &
        0.0003997,  0.0004067,  0.0004135,  0.0004202,  0.0004268, &
        0.0004333,  0.0004396,  0.0004458,  0.0004519,  0.0004579, &
        0.0004638,  0.0004696,  0.0004753,  0.0004809,  0.0004864, &
        0.0004919,  0.0004972,  0.0005025,  0.0005077,  0.0005129, &
        0.0005179,  0.0005229,  0.0005279,  0.0005327,  0.0005375, &
        0.0005423,  0.0005470,  0.0005516,  0.0005562,  0.0005607, &
        0.0005652,  0.0005696,  0.0005739,  0.0005782,  0.0005825, &
        0.0005867,  0.0005909,  0.0005951,  0.0005991,  0.0006032, &
        0.0006072,  0.0006112/
      data ((coa(i,j),i=1,62),j= 52, 52)/ &
        0.0002132,  0.0002309,  0.0002469,  0.0002617,  0.0002755, &
        0.0002885,  0.0003008,  0.0003125,  0.0003237,  0.0003345, &
        0.0003449,  0.0003549,  0.0003645,  0.0003739,  0.0003830, &
        0.0003918,  0.0004004,  0.0004088,  0.0004170,  0.0004250, &
        0.0004328,  0.0004404,  0.0004478,  0.0004551,  0.0004623, &
        0.0004693,  0.0004762,  0.0004829,  0.0004895,  0.0004960, &
        0.0005024,  0.0005087,  0.0005149,  0.0005210,  0.0005269, &
        0.0005328,  0.0005272,  0.0005443,  0.0005500,  0.0005555, &
        0.0005609,  0.0005663,  0.0005717,  0.0005769,  0.0005821, &
        0.0005872,  0.0005922,  0.0005972,  0.0006021,  0.0006070, &
        0.0006118,  0.0006165,  0.0006212,  0.0006258,  0.0006304, &
        0.0006349,  0.0006394,  0.0006438,  0.0006482,  0.0006525, &
        0.0006568,  0.0006611/
      data ((coa(i,j),i=1,62),j= 53, 53)/ &
        0.0002293,  0.0002485,  0.0002660,  0.0002821,  0.0002972, &
        0.0003114,  0.0003249,  0.0003377,  0.0003500,  0.0003618, &
        0.0003732,  0.0003841,  0.0003947,  0.0004049,  0.0004149, &
        0.0004245,  0.0004339,  0.0004430,  0.0004520,  0.0004606, &
        0.0004691,  0.0004774,  0.0004855,  0.0004934,  0.0005012, &
        0.0005087,  0.0005162,  0.0005235,  0.0005306,  0.0005377, &
        0.0005446,  0.0005513,  0.0005580,  0.0005646,  0.0005710, &
        0.0005773,  0.0005836,  0.0005897,  0.0005957,  0.0006017, &
        0.0006076,  0.0006134,  0.0006191,  0.0006247,  0.0006302, &
        0.0006357,  0.0006411,  0.0006464,  0.0006517,  0.0006569, &
        0.0006620,  0.0006671,  0.0006721,  0.0006771,  0.0006820, &
        0.0006868,  0.0006916,  0.0006963,  0.0007010,  0.0007056, &
        0.0007102,  0.0007147/
      data ((coa(i,j),i=1,62),j= 54, 54)/ &
        0.0002471,  0.0002680,  0.0002871,  0.0003048,  0.0003214, &
        0.0003369,  0.0003517,  0.0003658,  0.0003792,  0.0003921, &
        0.0004045,  0.0004165,  0.0004281,  0.0004392,  0.0004501, &
        0.0004606,  0.0004708,  0.0004807,  0.0004903,  0.0004998, &
        0.0005089,  0.0005179,  0.0005267,  0.0005352,  0.0005436, &
        0.0005518,  0.0005598,  0.0005677,  0.0005754,  0.0005829, &
        0.0005903,  0.0005976,  0.0006048,  0.0006118,  0.0006187, &
        0.0006255,  0.0006322,  0.0006388,  0.0006453,  0.0006516, &
        0.0006579,  0.0006641,  0.0006702,  0.0006762,  0.0006822, &
        0.0006880,  0.0006938,  0.0006995,  0.0007051,  0.0007106, &
        0.0007161,  0.0007215,  0.0007269,  0.0007321,  0.0007374, &
        0.0007425,  0.0007476,  0.0007527,  0.0007576,  0.0007626, &
        0.0007674,  0.0007723/
      data ((coa(i,j),i=1,62),j= 55, 55)/ &
        0.0002669,  0.0002898,  0.0003107,  0.0003300,  0.0003482, &
        0.0003653,  0.0003815,  0.0003969,  0.0004116,  0.0004257, &
        0.0004392,  0.0004522,  0.0004648,  0.0004769,  0.0004887, &
        0.0005001,  0.0005111,  0.0005218,  0.0005323,  0.0005425, &
        0.0005524,  0.0005620,  0.0005714,  0.0005807,  0.0005897, &
        0.0005985,  0.0006071,  0.0006155,  0.0006238,  0.0006319, &
        0.0006398,  0.0006476,  0.0006553,  0.0006628,  0.0006702, &
        0.0006775,  0.0006846,  0.0006917,  0.0006986,  0.0007054, &
        0.0007121,  0.0007187,  0.0007252,  0.0007316,  0.0007379, &
        0.0007442,  0.0007503,  0.0007564,  0.0007624,  0.0007683, &
        0.0007741,  0.0007798,  0.0007855,  0.0007911,  0.0007967, &
        0.0008022,  0.0008076,  0.0008129,  0.0008182,  0.0008235, &
        0.0008286,  0.0008337/
      data ((coa(i,j),i=1,62),j= 56, 56)/ &
        0.0002889,  0.0003140,  0.0003369,  0.0003582,  0.0003780, &
        0.0003967,  0.0004144,  0.0004312,  0.0004473,  0.0004626, &
        0.0004773,  0.0004914,  0.0005050,  0.0005182,  0.0005309, &
        0.0005432,  0.0005551,  0.0005666,  0.0005779,  0.0005888, &
        0.0005995,  0.0006098,  0.0006200,  0.0006298,  0.0006395, &
        0.0006489,  0.0006581,  0.0006672,  0.0006760,  0.0006847, &
        0.0006932,  0.0007015,  0.0007097,  0.0007177,  0.0007256, &
        0.0007333,  0.0007409,  0.0007484,  0.0007558,  0.0007630, &
        0.0007702,  0.0007772,  0.0007841,  0.0007909,  0.0007976, &
        0.0008043,  0.0008108,  0.0008172,  0.0008236,  0.0008298, &
        0.0008360,  0.0008421,  0.0008481,  0.0008541,  0.0008600, &
        0.0008658,  0.0008715,  0.0008772,  0.0008828,  0.0008883, &
        0.0008938,  0.0008992/
      data ((coa(i,j),i=1,62),j= 57, 57)/ &
        0.0003135,  0.0003410,  0.0003662,  0.0003895,  0.0004112, &
        0.0004316,  0.0004509,  0.0004692,  0.0004866,  0.0005032, &
        0.0005191,  0.0005344,  0.0005491,  0.0005632,  0.0005769, &
        0.0005901,  0.0006029,  0.0006153,  0.0006274,  0.0006391, &
        0.0006505,  0.0006616,  0.0006725,  0.0006830,  0.0006933, &
        0.0007034,  0.0007132,  0.0007229,  0.0007323,  0.0007415, &
        0.0007506,  0.0007595,  0.0007682,  0.0007767,  0.0007851, &
        0.0007933,  0.0008014,  0.0008093,  0.0008172,  0.0008249, &
        0.0008324,  0.0008399,  0.0008472,  0.0008544,  0.0008615, &
        0.0008685,  0.0008755,  0.0008823,  0.0008890,  0.0008956, &
        0.0009021,  0.0009086,  0.0009149,  0.0009212,  0.0009274, &
        0.0009335,  0.0009396,  0.0009455,  0.0009514,  0.0009573, &
        0.0009630,  0.0009687/
      data ((coa(i,j),i=1,62),j= 58, 58)/ &
        0.0003409,  0.0003711,  0.0003987,  0.0004241,  0.0004478, &
        0.0004700,  0.0004909,  0.0005107,  0.0005295,  0.0005474, &
        0.0005645,  0.0005810,  0.0005968,  0.0006120,  0.0006267, &
        0.0006408,  0.0006545,  0.0006678,  0.0006807,  0.0006932, &
        0.0007054,  0.0007173,  0.0007288,  0.0007401,  0.0007510, &
        0.0007618,  0.0007722,  0.0007825,  0.0007925,  0.0008023, &
        0.0008119,  0.0008213,  0.0008306,  0.0008396,  0.0008485, &
        0.0008572,  0.0008658,  0.0008742,  0.0008824,  0.0008906, &
        0.0008986,  0.0009064,  0.0009142,  0.0009218,  0.0009293, &
        0.0009367,  0.0009439,  0.0009511,  0.0009582,  0.0009652, &
        0.0009720,  0.0009788,  0.0009855,  0.0009921,  0.0009986, &
        0.0010050,  0.0010113,  0.0010176,  0.0010238,  0.0010299, &
        0.0010359,  0.0010419/
      data ((coa(i,j),i=1,62),j= 59, 59)/ &
        0.0003715,  0.0004046,  0.0004346,  0.0004623,  0.0004880, &
        0.0005120,  0.0005346,  0.0005560,  0.0005762,  0.0005955, &
        0.0006139,  0.0006315,  0.0006485,  0.0006648,  0.0006804, &
        0.0006956,  0.0007102,  0.0007244,  0.0007382,  0.0007515, &
        0.0007645,  0.0007771,  0.0007893,  0.0008012,  0.0008129, &
        0.0008243,  0.0008354,  0.0008463,  0.0008569,  0.0008673, &
        0.0008774,  0.0008874,  0.0008971,  0.0009067,  0.0009160, &
        0.0009252,  0.0009342,  0.0009431,  0.0009518,  0.0009604, &
        0.0009688,  0.0009770,  0.0009851,  0.0009931,  0.0010010, &
        0.0010088,  0.0010164,  0.0010239,  0.0010313,  0.0010386, &
        0.0010458,  0.0010529,  0.0010598,  0.0010667,  0.0010735, &
        0.0010802,  0.0010869,  0.0010934,  0.0010998,  0.0011062, &
        0.0011125,  0.0011187/
      data ((coa(i,j),i=1,62),j= 60, 60)/ &
        0.0004055,  0.0004415,  0.0004742,  0.0005042,  0.0005320, &
        0.0005579,  0.0005822,  0.0006052,  0.0006269,  0.0006476, &
        0.0006673,  0.0006862,  0.0007042,  0.0007216,  0.0007383, &
        0.0007545,  0.0007701,  0.0007851,  0.0007997,  0.0008139, &
        0.0008276,  0.0008410,  0.0008540,  0.0008666,  0.0008789, &
        0.0008910,  0.0009027,  0.0009141,  0.0009253,  0.0009362, &
        0.0009469,  0.0009574,  0.0009676,  0.0009777,  0.0009875, &
        0.0009972,  0.0010066,  0.0010159,  0.0010250,  0.0010339, &
        0.0010427,  0.0010514,  0.0010599,  0.0010682,  0.0010764, &
        0.0010845,  0.0010925,  0.0011003,  0.0011080,  0.0011156, &
        0.0011231,  0.0011304,  0.0011377,  0.0011449,  0.0011519, &
        0.0011589,  0.0011658,  0.0011726,  0.0011793,  0.0011859, &
        0.0011924,  0.0011989/
      data ((coa(i,j),i=1,62),j= 61, 61)/ &
        0.0004429,  0.0004821,  0.0005175,  0.0005499,  0.0005798, &
        0.0006076,  0.0006337,  0.0006583,  0.0006816,  0.0007037, &
        0.0007247,  0.0007448,  0.0007640,  0.0007825,  0.0008003, &
        0.0008174,  0.0008339,  0.0008499,  0.0008653,  0.0008803, &
        0.0008948,  0.0009089,  0.0009226,  0.0009359,  0.0009488, &
        0.0009615,  0.0009738,  0.0009858,  0.0009975,  0.0010089, &
        0.0010201,  0.0010311,  0.0010418,  0.0010523,  0.0010625, &
        0.0010726,  0.0010825,  0.0010921,  0.0011016,  0.0011109, &
        0.0011201,  0.0011291,  0.0011379,  0.0011466,  0.0011551, &
        0.0011635,  0.0011718,  0.0011799,  0.0011879,  0.0011958, &
        0.0012035,  0.0012112,  0.0012187,  0.0012261,  0.0012335, &
        0.0012407,  0.0012478,  0.0012548,  0.0012618,  0.0012686, &
        0.0012754,  0.0012821/
      data ((coa(i,j),i=1,62),j= 62, 62)/ &
        0.0004840,  0.0005264,  0.0005646,  0.0005994,  0.0006316, &
        0.0006614,  0.0006893,  0.0007155,  0.0007403,  0.0007638, &
        0.0007862,  0.0008075,  0.0008279,  0.0008475,  0.0008663, &
        0.0008844,  0.0009018,  0.0009186,  0.0009349,  0.0009506, &
        0.0009658,  0.0009806,  0.0009950,  0.0010089,  0.0010225, &
        0.0010356,  0.0010485,  0.0010610,  0.0010733,  0.0010852, &
        0.0010968,  0.0011082,  0.0011194,  0.0011303,  0.0011409, &
        0.0011514,  0.0011616,  0.0011717,  0.0011815,  0.0011912, &
        0.0012007,  0.0012100,  0.0012191,  0.0012281,  0.0012370, &
        0.0012457,  0.0012542,  0.0012627,  0.0012709,  0.0012791, &
        0.0012871,  0.0012951,  0.0013029,  0.0013106,  0.0013181, &
        0.0013256,  0.0013330,  0.0013403,  0.0013475,  0.0013546, &
        0.0013616,  0.0013685/
      data ((coa(i,j),i=1,62),j= 63, 63)/ &
        0.0005290,  0.0005747,  0.0006157,  0.0006530,  0.0006874, &
        0.0007192,  0.0007490,  0.0007769,  0.0008032,  0.0008281, &
        0.0008518,  0.0008743,  0.0008959,  0.0009165,  0.0009362, &
        0.0009552,  0.0009735,  0.0009911,  0.0010082,  0.0010246, &
        0.0010405,  0.0010559,  0.0010709,  0.0010854,  0.0010995, &
        0.0011132,  0.0011266,  0.0011396,  0.0011523,  0.0011647, &
        0.0011768,  0.0011886,  0.0012002,  0.0012115,  0.0012225, &
        0.0012334,  0.0012440,  0.0012544,  0.0012646,  0.0012746, &
        0.0012844,  0.0012941,  0.0013036,  0.0013129,  0.0013220, &
        0.0013310,  0.0013399,  0.0013486,  0.0013572,  0.0013657, &
        0.0013740,  0.0013823,  0.0013904,  0.0013983,  0.0014062, &
        0.0014140,  0.0014217,  0.0014292,  0.0014367,  0.0014441, &
        0.0014514,  0.0014586/
      data ((coa(i,j),i=1,62),j= 64, 64)/ &
        0.0005778,  0.0006269,  0.0006708,  0.0007107,  0.0007473, &
        0.0007812,  0.0008127,  0.0008423,  0.0008701,  0.0008964, &
        0.0009213,  0.0009450,  0.0009676,  0.0009892,  0.0010099, &
        0.0010297,  0.0010488,  0.0010671,  0.0010848,  0.0011020, &
        0.0011185,  0.0011345,  0.0011500,  0.0011651,  0.0011798, &
        0.0011940,  0.0012078,  0.0012213,  0.0012345,  0.0012473, &
        0.0012599,  0.0012721,  0.0012841,  0.0012958,  0.0013073, &
        0.0013185,  0.0013295,  0.0013403,  0.0013509,  0.0013612, &
        0.0013714,  0.0013815,  0.0013913,  0.0014010,  0.0014105, &
        0.0014199,  0.0014291,  0.0014382,  0.0014471,  0.0014559, &
        0.0014646,  0.0014732,  0.0014816,  0.0014900,  0.0014982, &
        0.0015063,  0.0015143,  0.0015222,  0.0015300,  0.0015377, &
        0.0015453,  0.0015528/
      data ((coa(i,j),i=1,62),j= 65, 65)/ &
        0.0006307,  0.0006832,  0.0007301,  0.0007725,  0.0008114, &
        0.0008472,  0.0008805,  0.0009116,  0.0009409,  0.0009684, &
        0.0009945,  0.0010193,  0.0010428,  0.0010653,  0.0010868, &
        0.0011075,  0.0011273,  0.0011464,  0.0011647,  0.0011825, &
        0.0011996,  0.0012162,  0.0012323,  0.0012480,  0.0012631, &
        0.0012779,  0.0012922,  0.0013062,  0.0013199,  0.0013332, &
        0.0013462,  0.0013589,  0.0013713,  0.0013835,  0.0013954, &
        0.0014071,  0.0014186,  0.0014298,  0.0014408,  0.0014516, &
        0.0014623,  0.0014727,  0.0014830,  0.0014931,  0.0015030, &
        0.0015128,  0.0015225,  0.0015319,  0.0015413,  0.0015505, &
        0.0015596,  0.0015686,  0.0015774,  0.0015862,  0.0015948, &
        0.0016033,  0.0016117,  0.0016200,  0.0016282,  0.0016363, &
        0.0016443,  0.0016522/
      data ((coa(i,j),i=1,62),j= 66, 66)/ &
        0.0006876,  0.0007436,  0.0007934,  0.0008383,  0.0008793, &
        0.0009170,  0.0009520,  0.0009846,  0.0010150,  0.0010439, &
        0.0010710,  0.0010968,  0.0011213,  0.0011446,  0.0011669, &
        0.0011883,  0.0012089,  0.0012287,  0.0012477,  0.0012661, &
        0.0012839,  0.0013011,  0.0013178,  0.0013340,  0.0013498, &
        0.0013651,  0.0013800,  0.0013946,  0.0014088,  0.0014227, &
        0.0014362,  0.0014495,  0.0014624,  0.0014751,  0.0014876, &
        0.0014998,  0.0015117,  0.0015235,  0.0015350,  0.0015464, &
        0.0015575,  0.0015685,  0.0015792,  0.0015899,  0.0016003, &
        0.0016106,  0.0016207,  0.0016307,  0.0016405,  0.0016502, &
        0.0016598,  0.0016693,  0.0016786,  0.0016878,  0.0016969, &
        0.0017059,  0.0017147,  0.0017235,  0.0017322,  0.0017407, &
        0.0017492,  0.0017575/
      data ((coa(i,j),i=1,62),j= 67, 67)/ &
        0.0007485,  0.0008080,  0.0008606,  0.0009079,  0.0009509, &
        0.0009904,  0.0010269,  0.0010608,  0.0010926,  0.0011225, &
        0.0011507,  0.0011774,  0.0012028,  0.0012270,  0.0012501, &
        0.0012723,  0.0012936,  0.0013142,  0.0013339,  0.0013531, &
        0.0013716,  0.0013895,  0.0014069,  0.0014238,  0.0014402, &
        0.0014562,  0.0014718,  0.0014870,  0.0015019,  0.0015164, &
        0.0015306,  0.0015445,  0.0015581,  0.0015714,  0.0015845, &
        0.0015973,  0.0016099,  0.0016223,  0.0016344,  0.0016464, &
        0.0016581,  0.0016697,  0.0016811,  0.0016922,  0.0017033, &
        0.0017141,  0.0017249,  0.0017354,  0.0017458,  0.0017561, &
        0.0017662,  0.0017762,  0.0017861,  0.0017959,  0.0018055, &
        0.0018150,  0.0018244,  0.0018337,  0.0018429,  0.0018520, &
        0.0018610,  0.0018698/
      data ((coa(i,j),i=1,62),j= 68, 68)/ &
        0.0008135,  0.0008762,  0.0009315,  0.0009811,  0.0010259, &
        0.0010670,  0.0011050,  0.0011402,  0.0011732,  0.0012042, &
        0.0012334,  0.0012611,  0.0012874,  0.0013126,  0.0013366, &
        0.0013597,  0.0013819,  0.0014033,  0.0014239,  0.0014439, &
        0.0014632,  0.0014820,  0.0015002,  0.0015179,  0.0015351, &
        0.0015519,  0.0015683,  0.0015843,  0.0016000,  0.0016153, &
        0.0016302,  0.0016449,  0.0016592,  0.0016733,  0.0016871, &
        0.0017007,  0.0017140,  0.0017271,  0.0017400,  0.0017526, &
        0.0017651,  0.0017773,  0.0017894,  0.0018012,  0.0018129, &
        0.0018245,  0.0018358,  0.0018471,  0.0018581,  0.0018690, &
        0.0018798,  0.0018904,  0.0019009,  0.0019113,  0.0019215, &
        0.0019316,  0.0019416,  0.0019515,  0.0019613,  0.0019709, &
        0.0019805,  0.0019899/
      data ((coa(i,j),i=1,62),j= 69, 69)/ &
        0.0008823,  0.0009481,  0.0010059,  0.0010575,  0.0011042, &
        0.0011468,  0.0011862,  0.0012227,  0.0012569,  0.0012891, &
        0.0013195,  0.0013483,  0.0013757,  0.0014020,  0.0014271, &
        0.0014512,  0.0014744,  0.0014968,  0.0015185,  0.0015395, &
        0.0015598,  0.0015795,  0.0015987,  0.0016174,  0.0016356, &
        0.0016533,  0.0016707,  0.0016876,  0.0017041,  0.0017203, &
        0.0017362,  0.0017517,  0.0017669,  0.0017819,  0.0017965, &
        0.0018109,  0.0018251,  0.0018390,  0.0018527,  0.0018661, &
        0.0018793,  0.0018924,  0.0019052,  0.0019178,  0.0019303, &
        0.0019425,  0.0019546,  0.0019665,  0.0019783,  0.0019899, &
        0.0020014,  0.0020127,  0.0020238,  0.0020348,  0.0020457, &
        0.0020565,  0.0020671,  0.0020776,  0.0020880,  0.0020983, &
        0.0021084,  0.0021185/
      data ((coa(i,j),i=1,62),j= 70, 70)/ &
        0.0009546,  0.0010233,  0.0010834,  0.0011370,  0.0011854, &
        0.0012297,  0.0012705,  0.0013085,  0.0013441,  0.0013776, &
        0.0014093,  0.0014395,  0.0014682,  0.0014957,  0.0015221, &
        0.0015475,  0.0015720,  0.0015956,  0.0016185,  0.0016406, &
        0.0016621,  0.0016830,  0.0017033,  0.0017231,  0.0017424, &
        0.0017613,  0.0017797,  0.0017976,  0.0018152,  0.0018324, &
        0.0018493,  0.0018658,  0.0018820,  0.0018979,  0.0019135, &
        0.0019288,  0.0019439,  0.0019587,  0.0019732,  0.0019875, &
        0.0020016,  0.0020155,  0.0020291,  0.0020425,  0.0020558, &
        0.0020688,  0.0020817,  0.0020944,  0.0021069,  0.0021192, &
        0.0021314,  0.0021434,  0.0021095,  0.0021670,  0.0021786, &
        0.0021900,  0.0022013,  0.0022125,  0.0022235,  0.0022344, &
        0.0022452,  0.0022558/
      data ((coa(i,j),i=1,62),j= 71, 71)/ &
        0.0010302,  0.0011016,  0.0011640,  0.0012195,  0.0012698, &
        0.0013159,  0.0013584,  0.0013981,  0.0014354,  0.0014705, &
        0.0015038,  0.0015355,  0.0015658,  0.0015948,  0.0016227, &
        0.0016496,  0.0016755,  0.0017005,  0.0017248,  0.0017483, &
        0.0017712,  0.0017934,  0.0018150,  0.0018360,  0.0018566, &
        0.0018766,  0.0018962,  0.0019153,  0.0019340,  0.0019524, &
        0.0019703,  0.0019879,  0.0020052,  0.0020221,  0.0020387, &
        0.0020550,  0.0020710,  0.0020867,  0.0021022,  0.0021174, &
        0.0021324,  0.0021471,  0.0021159,  0.0021759,  0.0021900, &
        0.0022039,  0.0022175,  0.0022310,  0.0022443,  0.0022574, &
        0.0022703,  0.0022830,  0.0022956,  0.0023080,  0.0023203, &
        0.0023324,  0.0023443,  0.0023562,  0.0023678,  0.0023794, &
        0.0023908,  0.0024020/
      data ((coa(i,j),i=1,62),j= 72, 72)/ &
        0.0011087,  0.0011828,  0.0012476,  0.0013054,  0.0013579, &
        0.0014060,  0.0014506,  0.0014923,  0.0015315,  0.0015685, &
        0.0016038,  0.0016373,  0.0016694,  0.0017002,  0.0017298, &
        0.0017583,  0.0017859,  0.0018125,  0.0018384,  0.0018634, &
        0.0018877,  0.0019114,  0.0019344,  0.0019568,  0.0019787, &
        0.0020000,  0.0020209,  0.0020412,  0.0020612,  0.0020807, &
        0.0020998,  0.0021185,  0.0021368,  0.0021090,  0.0021725, &
        0.0021898,  0.0022068,  0.0022235,  0.0022399,  0.0022561, &
        0.0022720,  0.0022876,  0.0023029,  0.0023181,  0.0023330, &
        0.0023477,  0.0023621,  0.0023764,  0.0023904,  0.0024042, &
        0.0024179,  0.0024314,  0.0024446,  0.0024577,  0.0024707, &
        0.0024834,  0.0024960,  0.0025085,  0.0025208,  0.0025329, &
        0.0025449,  0.0025568/
      data ((coa(i,j),i=1,62),j= 73, 73)/ &
        0.0011902,  0.0012672,  0.0013347,  0.0013952,  0.0014502, &
        0.0015008,  0.0015478,  0.0015919,  0.0016334,  0.0016727, &
        0.0017101,  0.0017457,  0.0017799,  0.0018126,  0.0018442, &
        0.0018746,  0.0019039,  0.0019323,  0.0019598,  0.0019865, &
        0.0020124,  0.0020376,  0.0020621,  0.0020859,  0.0021092, &
        0.0021319,  0.0021083,  0.0021757,  0.0021969,  0.0022176, &
        0.0022379,  0.0022577,  0.0022772,  0.0022962,  0.0023149, &
        0.0023333,  0.0023513,  0.0023690,  0.0023863,  0.0024034, &
        0.0024202,  0.0024366,  0.0024529,  0.0024688,  0.0024845, &
        0.0025000,  0.0025152,  0.0025302,  0.0025450,  0.0025595, &
        0.0025739,  0.0025880,  0.0026019,  0.0026157,  0.0026293, &
        0.0026426,  0.0026557,  0.0026689,  0.0026817,  0.0026944, &
        0.0027070,  0.0027194/
      data ((coa(i,j),i=1,62),j= 74, 74)/ &
        0.0012749,  0.0013552,  0.0014259,  0.0014895,  0.0015475, &
        0.0016010,  0.0016509,  0.0016977,  0.0017418,  0.0017836, &
        0.0018234,  0.0018614,  0.0018978,  0.0019328,  0.0019664, &
        0.0019987,  0.0020300,  0.0020602,  0.0020895,  0.0021179, &
        0.0021454,  0.0021722,  0.0021982,  0.0022235,  0.0022482, &
        0.0022723,  0.0022958,  0.0023187,  0.0023411,  0.0023630, &
        0.0023844,  0.0024054,  0.0024259,  0.0024460,  0.0024658, &
        0.0024851,  0.0025040,  0.0025226,  0.0025409,  0.0025588, &
        0.0025764,  0.0025937,  0.0026107,  0.0026275,  0.0026439, &
        0.0026601,  0.0026760,  0.0026917,  0.0027071,  0.0027223, &
        0.0027373,  0.0027520,  0.0027665,  0.0027808,  0.0027950, &
        0.0028089,  0.0028226,  0.0028361,  0.0028495,  0.0028627, &
        0.0028757,  0.0028885/
      data ((coa(i,j),i=1,62),j= 75, 75)/ &
        0.0013631,  0.0014474,  0.0015220,  0.0015892,  0.0016507, &
        0.0017076,  0.0017607,  0.0018105,  0.0018575,  0.0019021, &
        0.0019445,  0.0019850,  0.0020238,  0.0020610,  0.0020967, &
        0.0021312,  0.0021186,  0.0021965,  0.0022276,  0.0022577, &
        0.0022868,  0.0023152,  0.0023427,  0.0023695,  0.0023956, &
        0.0024210,  0.0024457,  0.0024699,  0.0024935,  0.0025165, &
        0.0025390,  0.0025610,  0.0025826,  0.0026037,  0.0026243, &
        0.0026445,  0.0026643,  0.0026838,  0.0027028,  0.0027215, &
        0.0027399,  0.0027579,  0.0027756,  0.0027930,  0.0028101, &
        0.0028269,  0.0028434,  0.0028596,  0.0028756,  0.0028913, &
        0.0029068,  0.0029220,  0.0029370,  0.0029518,  0.0029664, &
        0.0029807,  0.0029949,  0.0030088,  0.0030225,  0.0030361, &
        0.0030494,  0.0030626/
      data ((coa(i,j),i=1,62),j= 76, 76)/ &
        0.0014557,  0.0015446,  0.0016236,  0.0016950,  0.0017605, &
        0.0018211,  0.0018777,  0.0019308,  0.0019809,  0.0020284, &
        0.0020736,  0.0021167,  0.0021121,  0.0021973,  0.0022353, &
        0.0022718,  0.0023069,  0.0023409,  0.0023737,  0.0024055, &
        0.0024362,  0.0024661,  0.0024950,  0.0025232,  0.0025506, &
        0.0025772,  0.0026031,  0.0026284,  0.0026531,  0.0026771, &
        0.0027006,  0.0027235,  0.0027460,  0.0027679,  0.0027893, &
        0.0028103,  0.0028309,  0.0028510,  0.0028707,  0.0028900, &
        0.0029090,  0.0029276,  0.0029458,  0.0029637,  0.0029813, &
        0.0029986,  0.0030156,  0.0030322,  0.0030486,  0.0030647, &
        0.0030806,  0.0030962,  0.0031115,  0.0031266,  0.0031414, &
        0.0031560,  0.0031704,  0.0031846,  0.0031986,  0.0032123, &
        0.0032259,  0.0032392/
      data ((coa(i,j),i=1,62),j= 77, 77)/ &
        0.0015532,  0.0016476,  0.0017317,  0.0018078,  0.0018775, &
        0.0019422,  0.0020024,  0.0020590,  0.0021123,  0.0021169, &
        0.0022106,  0.0022563,  0.0022999,  0.0023416,  0.0023817, &
        0.0024202,  0.0024572,  0.0024929,  0.0025273,  0.0025607, &
        0.0025929,  0.0026241,  0.0026543,  0.0026837,  0.0027122, &
        0.0027399,  0.0027668,  0.0027931,  0.0028186,  0.0028435, &
        0.0028678,  0.0028915,  0.0029146,  0.0029372,  0.0029592, &
        0.0029808,  0.0030019,  0.0030225,  0.0030427,  0.0030625, &
        0.0030819,  0.0031009,  0.0031195,  0.0031377,  0.0031556, &
        0.0031732,  0.0031904,  0.0032073,  0.0032239,  0.0032402, &
        0.0032563,  0.0032720,  0.0032875,  0.0033027,  0.0033177, &
        0.0033324,  0.0033468,  0.0033611,  0.0033751,  0.0033889, &
        0.0034025,  0.0034159/
      data ((coa(i,j),i=1,62),j= 78, 78)/ &
        0.0016566,  0.0017571,  0.0018467,  0.0019278,  0.0020021, &
        0.0020708,  0.0021349,  0.0021949,  0.0022514,  0.0023047, &
        0.0023554,  0.0024035,  0.0024494,  0.0024932,  0.0025352, &
        0.0025755,  0.0026142,  0.0026515,  0.0026874,  0.0027220, &
        0.0027555,  0.0027878,  0.0028191,  0.0028495,  0.0028789, &
        0.0029075,  0.0029352,  0.0029621,  0.0029883,  0.0030138, &
        0.0030387,  0.0030629,  0.0030864,  0.0031094,  0.0031319, &
        0.0031538,  0.0031752,  0.0031961,  0.0032166,  0.0032365, &
        0.0032561,  0.0032752,  0.0032940,  0.0033123,  0.0033303, &
        0.0033479,  0.0033652,  0.0033821,  0.0033988,  0.0034151, &
        0.0034310,  0.0034467,  0.0034622,  0.0034773,  0.0034922, &
        0.0035068,  0.0035211,  0.0035353,  0.0035491,  0.0035628, &
        0.0035762,  0.0035894/
      data ((coa(i,j),i=1,62),j= 79, 79)/ &
        0.0017664,  0.0018736,  0.0019689,  0.0020552,  0.0021342, &
        0.0022071,  0.0022749,  0.0023383,  0.0023978,  0.0024539, &
        0.0025070,  0.0025574,  0.0026054,  0.0026511,  0.0026948, &
        0.0027366,  0.0027767,  0.0028153,  0.0028523,  0.0028880, &
        0.0029224,  0.0029556,  0.0029877,  0.0030187,  0.0030487, &
        0.0030778,  0.0031060,  0.0031334,  0.0031599,  0.0031857, &
        0.0032108,  0.0032352,  0.0032590,  0.0032821,  0.0033047, &
        0.0033267,  0.0033481,  0.0033690,  0.0033894,  0.0034094, &
        0.0034289,  0.0034479,  0.0034665,  0.0034847,  0.0035025, &
        0.0035200,  0.0035371,  0.0035538,  0.0035702,  0.0035862, &
        0.0036020,  0.0036174,  0.0036325,  0.0036474,  0.0036620, &
        0.0036763,  0.0036903,  0.0037041,  0.0037177,  0.0037310, &
        0.0037441,  0.0037569/
      data ((coa(i,j),i=1,62),j= 80, 80)/ &
        0.0018832,  0.0019973,  0.0020987,  0.0021902,  0.0022737, &
        0.0023505,  0.0024220,  0.0024885,  0.0025508,  0.0026093, &
        0.0026646,  0.0027169,  0.0027666,  0.0028138,  0.0028589, &
        0.0029018,  0.0029430,  0.0029824,  0.0030202,  0.0030565, &
        0.0030915,  0.0031252,  0.0031576,  0.0031890,  0.0032192, &
        0.0032485,  0.0032768,  0.0033042,  0.0033308,  0.0033566, &
        0.0033816,  0.0034059,  0.0034295,  0.0034525,  0.0034748, &
        0.0034966,  0.0035177,  0.0035384,  0.0035585,  0.0035781, &
        0.0035972,  0.0036159,  0.0036341,  0.0036520,  0.0036694, &
        0.0036864,  0.0037031,  0.0037193,  0.0037353,  0.0037509, &
        0.0037662,  0.0037811,  0.0037958,  0.0038102,  0.0038243, &
        0.0038381,  0.0038517,  0.0038650,  0.0038780,  0.0038908, &
        0.0039034,  0.0039158/
      data ((coa(i,j),i=1,62),j= 81, 81)/ &
        0.0020072,  0.0021282,  0.0022356,  0.0023321,  0.0024200, &
        0.0025006,  0.0025751,  0.0026443,  0.0027089,  0.0027695, &
        0.0028265,  0.0028803,  0.0029311,  0.0029794,  0.0030252, &
        0.0030689,  0.0031106,  0.0031504,  0.0031886,  0.0032251, &
        0.0032602,  0.0032939,  0.0033263,  0.0033575,  0.0033876, &
        0.0034166,  0.0034447,  0.0034718,  0.0034980,  0.0035234, &
        0.0035480,  0.0035719,  0.0035950,  0.0036175,  0.0036393, &
        0.0036605,  0.0036811,  0.0037012,  0.0037207,  0.0037398, &
        0.0037583,  0.0037764,  0.0037940,  0.0038112,  0.0038280, &
        0.0038444,  0.0038604,  0.0038761,  0.0038914,  0.0039063, &
        0.0039210,  0.0039353,  0.0039494,  0.0039631,  0.0039766, &
        0.0039898,  0.0040027,  0.0040154,  0.0040278,  0.0040400, &
        0.0040520,  0.0040638/
      data ((coa(i,j),i=1,62),j= 82, 82)/ &
        0.0021381,  0.0022661,  0.0023791,  0.0024803,  0.0025719, &
        0.0026557,  0.0027328,  0.0028041,  0.0028705,  0.0029325, &
        0.0029905,  0.0030451,  0.0030966,  0.0031453,  0.0031914, &
        0.0032353,  0.0032769,  0.0033166,  0.0033545,  0.0033908, &
        0.0034255,  0.0034588,  0.0034907,  0.0035214,  0.0035509, &
        0.0035793,  0.0036067,  0.0036331,  0.0036586,  0.0036833, &
        0.0037071,  0.0037302,  0.0037526,  0.0037743,  0.0037953, &
        0.0038157,  0.0038356,  0.0038548,  0.0038736,  0.0038916, &
        0.0039095,  0.0039268,  0.0039436,  0.0039600,  0.0039761, &
        0.0039917,  0.0040069,  0.0040218,  0.0040364,  0.0040506, &
        0.0040645,  0.0040781,  0.0040914,  0.0041044,  0.0041172, &
        0.0041296,  0.0041419,  0.0041539,  0.0041656,  0.0041772, &
        0.0041885,  0.0041996/
      data ((coa(i,j),i=1,62),j= 83, 83)/ &
        0.0022756,  0.0024100,  0.0025280,  0.0026332,  0.0027280, &
        0.0028142,  0.0028931,  0.0029658,  0.0030331,  0.0030957, &
        0.0031541,  0.0032089,  0.0032603,  0.0033088,  0.0033545, &
        0.0033978,  0.0034389,  0.0034780,  0.0035151,  0.0035506, &
        0.0035844,  0.0036168,  0.0036478,  0.0036775,  0.0037061, &
        0.0037335,  0.0037599,  0.0037853,  0.0038098,  0.0038335, &
        0.0038563,  0.0038784,  0.0038998,  0.0039205,  0.0039405, &
        0.0039599,  0.0039788,  0.0039971,  0.0040149,  0.0040322, &
        0.0040490,  0.0040653,  0.0040813,  0.0040968,  0.0041119, &
        0.0041267,  0.0041411,  0.0041552,  0.0041689,  0.0041823, &
        0.0041954,  0.0042082,  0.0042208,  0.0042331,  0.0042451, &
        0.0042569,  0.0042684,  0.0042797,  0.0042908,  0.0043017, &
        0.0043123,  0.0043228/
      data ((coa(i,j),i=1,62),j= 84, 84)/ &
        0.0024185,  0.0025586,  0.0026808,  0.0027890,  0.0028859, &
        0.0029735,  0.0030533,  0.0031264,  0.0031938,  0.0032562, &
        0.0033142,  0.0033683,  0.0034190,  0.0034665,  0.0035113, &
        0.0035535,  0.0035934,  0.0036313,  0.0036672,  0.0037014, &
        0.0037340,  0.0037651,  0.0037948,  0.0038232,  0.0038505, &
        0.0038767,  0.0039018,  0.0039260,  0.0039493,  0.0039717, &
        0.0039934,  0.0040143,  0.0040345,  0.0040540,  0.0040730, &
        0.0040913,  0.0041091,  0.0041264,  0.0041432,  0.0041595, &
        0.0041753,  0.0041907,  0.0042057,  0.0042204,  0.0042346, &
        0.0042485,  0.0042621,  0.0042754,  0.0042883,  0.0043009, &
        0.0043133,  0.0043254,  0.0043372,  0.0043488,  0.0043601, &
        0.0043712,  0.0043821,  0.0043928,  0.0044032,  0.0044135, &
        0.0044236,  0.0044335/
      data ((coa(i,j),i=1,62),j= 85, 85)/ &
        0.0025653,  0.0027099,  0.0028350,  0.0029450,  0.0030428, &
        0.0031307,  0.0032102,  0.0032828,  0.0033493,  0.0034106, &
        0.0034673,  0.0035200,  0.0035692,  0.0036152,  0.0036583, &
        0.0036990,  0.0037373,  0.0037735,  0.0038078,  0.0038404, &
        0.0038714,  0.0039009,  0.0039291,  0.0039560,  0.0039818, &
        0.0040065,  0.0040303,  0.0040531,  0.0040751,  0.0040962, &
        0.0041166,  0.0041363,  0.0041553,  0.0041738,  0.0041916, &
        0.0042089,  0.0042256,  0.0042419,  0.0042577,  0.0042730, &
        0.0042879,  0.0043025,  0.0043166,  0.0043304,  0.0043439, &
        0.0043570,  0.0043698,  0.0043823,  0.0043945,  0.0044064, &
        0.0044181,  0.0044296,  0.0044407,  0.0044517,  0.0044624, &
        0.0044730,  0.0044833,  0.0044934,  0.0045033,  0.0045130, &
        0.0045226,  0.0045320/
      data ((coa(i,j),i=1,62),j= 86, 86)/ &
        0.0027124,  0.0028597,  0.0029861,  0.0030963,  0.0031936, &
        0.0032805,  0.0033586,  0.0034295,  0.0034941,  0.0035534, &
        0.0036081,  0.0036587,  0.0037058,  0.0037497,  0.0037908, &
        0.0038294,  0.0038657,  0.0039000,  0.0039324,  0.0039631, &
        0.0039923,  0.0040201,  0.0040467,  0.0040720,  0.0040963, &
        0.0041195,  0.0041418,  0.0041633,  0.0041839,  0.0042038, &
        0.0042230,  0.0042415,  0.0042594,  0.0042768,  0.0042935, &
        0.0043098,  0.0043256,  0.0043409,  0.0043558,  0.0043703, &
        0.0043844,  0.0043982,  0.0044115,  0.0044246,  0.0044373, &
        0.0044497,  0.0044619,  0.0044737,  0.0044853,  0.0044966, &
        0.0045077,  0.0045186,  0.0045292,  0.0045397,  0.0045499, &
        0.0045599,  0.0045697,  0.0045793,  0.0045888,  0.0045981, &
        0.0046072,  0.0046162/
      data ((coa(i,j),i=1,62),j= 87, 87)/ &
        0.0028481,  0.0029956,  0.0031209,  0.0032292,  0.0033241, &
        0.0034083,  0.0034836,  0.0035515,  0.0036132,  0.0036696, &
        0.0037214,  0.0037692,  0.0038136,  0.0038548,  0.0038934, &
        0.0039296,  0.0039636,  0.0039956,  0.0040260,  0.0040547, &
        0.0040820,  0.0041080,  0.0041328,  0.0041565,  0.0041792, &
        0.0042010,  0.0042219,  0.0042419,  0.0042613,  0.0042800, &
        0.0042980,  0.0043154,  0.0043322,  0.0043485,  0.0043643, &
        0.0043796,  0.0043945,  0.0044090,  0.0044231,  0.0044367, &
        0.0044501,  0.0044631,  0.0044757,  0.0044881,  0.0045001, &
        0.0045119,  0.0045234,  0.0045347,  0.0045457,  0.0045564, &
        0.0045670,  0.0045773,  0.0045874,  0.0045973,  0.0046070, &
        0.0046166,  0.0046259,  0.0046351,  0.0046441,  0.0046530, &
        0.0046616,  0.0046702/
      data ((coa(i,j),i=1,62),j= 88, 88)/ &
        0.0029341,  0.0030768,  0.0031968,  0.0032997,  0.0033892, &
        0.0034681,  0.0035383,  0.0036014,  0.0036584,  0.0037104, &
        0.0037581,  0.0038020,  0.0038427,  0.0038805,  0.0039159, &
        0.0039490,  0.0039801,  0.0040095,  0.0040373,  0.0040637, &
        0.0040888,  0.0041127,  0.0041355,  0.0041573,  0.0041782, &
        0.0041983,  0.0042175,  0.0042361,  0.0042540,  0.0042713, &
        0.0042880,  0.0043041,  0.0043197,  0.0043349,  0.0043495, &
        0.0043638,  0.0043777,  0.0043911,  0.0044042,  0.0044170, &
        0.0044294,  0.0044416,  0.0044534,  0.0044649,  0.0044762, &
        0.0044872,  0.0044980,  0.0045085,  0.0045188,  0.0045289, &
        0.0045387,  0.0045484,  0.0045579,  0.0045672,  0.0045763, &
        0.0045852,  0.0045940,  0.0046026,  0.0046110,  0.0046193, &
        0.0046275,  0.0046355/
      data ((coa(i,j),i=1,62),j= 89, 89)/ &
        0.0029122,  0.0030427,  0.0031513,  0.0032438,  0.0033237, &
        0.0033938,  0.0034559,  0.0035116,  0.0035619,  0.0036076, &
        0.0036495,  0.0036882,  0.0037238,  0.0037573,  0.0037884, &
        0.0038176,  0.0038451,  0.0038711,  0.0038957,  0.0039191, &
        0.0039413,  0.0039626,  0.0039829,  0.0040023,  0.0040209, &
        0.0040389,  0.0040561,  0.0040727,  0.0040887,  0.0041042, &
        0.0041192,  0.0041337,  0.0041477,  0.0041613,  0.0041745, &
        0.0041874,  0.0041998,  0.0042120,  0.0042238,  0.0042353, &
        0.0042465,  0.0042574,  0.0042681,  0.0042785,  0.0042887, &
        0.0042986,  0.0043084,  0.0043179,  0.0043272,  0.0043363, &
        0.0043452,  0.0043539,  0.0043625,  0.0043709,  0.0043791, &
        0.0043872,  0.0043951,  0.0044029,  0.0044105,  0.0044180, &
        0.0044254,  0.0044326/
      data ((coa(i,j),i=1,62),j= 90, 90)/ &
        0.0027405,  0.0028512,  0.0029426,  0.0030199,  0.0030864, &
        0.0031447,  0.0031962,  0.0032424,  0.0032841,  0.0033221, &
        0.0033569,  0.0033891,  0.0034190,  0.0034468,  0.0034729, &
        0.0034974,  0.0035206,  0.0035424,  0.0035632,  0.0035830, &
        0.0036018,  0.0036198,  0.0036370,  0.0036535,  0.0036694, &
        0.0036847,  0.0036993,  0.0037135,  0.0037272,  0.0037404, &
        0.0037532,  0.0037656,  0.0037776,  0.0037892,  0.0038005, &
        0.0038115,  0.0038222,  0.0038326,  0.0038427,  0.0038526, &
        0.0038622,  0.0038716,  0.0038808,  0.0038897,  0.0038984, &
        0.0039070,  0.0039153,  0.0039235,  0.0039314,  0.0039393, &
        0.0039469,  0.0039544,  0.0039618,  0.0039690,  0.0039761, &
        0.0039830,  0.0039898,  0.0039965,  0.0040030,  0.0040095, &
        0.0040158,  0.0040220/
      data ((coa(i,j),i=1,62),j= 91, 91)/ &
        0.0024633,  0.0025514,  0.0026239,  0.0026851,  0.0027377, &
        0.0027838,  0.0028247,  0.0028613,  0.0028946,  0.0029249, &
        0.0029529,  0.0029787,  0.0030028,  0.0030253,  0.0030464, &
        0.0030663,  0.0030851,  0.0031029,  0.0031199,  0.0031360, &
        0.0031514,  0.0031661,  0.0031803,  0.0031938,  0.0032068, &
        0.0032194,  0.0032314,  0.0032431,  0.0032544,  0.0032652, &
        0.0032758,  0.0032860,  0.0032959,  0.0033055,  0.0033148, &
        0.0033239,  0.0033327,  0.0033413,  0.0033497,  0.0033578, &
        0.0033658,  0.0033735,  0.0033811,  0.0033885,  0.0033957, &
        0.0034028,  0.0034097,  0.0034164,  0.0034230,  0.0034295, &
        0.0034359,  0.0034421,  0.0034482,  0.0034541,  0.0034600, &
        0.0034657,  0.0034714,  0.0034769,  0.0034823,  0.0034877, &
        0.0034929,  0.0034981/
      data ((coa(i,j),i=1,62),j= 92, 92)/ &
        0.0021142,  0.0022278,  0.0022837,  0.0023309,  0.0023717, &
        0.0024075,  0.0024394,  0.0024681,  0.0024943,  0.0025182, &
        0.0025404,  0.0025609,  0.0025801,  0.0025980,  0.0026149, &
        0.0026308,  0.0026459,  0.0026602,  0.0026738,  0.0026868, &
        0.0026992,  0.0027111,  0.0027225,  0.0027334,  0.0027439, &
        0.0027541,  0.0027638,  0.0027733,  0.0027824,  0.0027912, &
        0.0027997,  0.0028080,  0.0028160,  0.0028238,  0.0028314, &
        0.0028387,  0.0028459,  0.0028529,  0.0028597,  0.0028663, &
        0.0028727,  0.0028791,  0.0028852,  0.0028912,  0.0028971, &
        0.0029028,  0.0029084,  0.0029139,  0.0029193,  0.0029246, &
        0.0029297,  0.0029348,  0.0029398,  0.0029446,  0.0029494, &
        0.0029541,  0.0029587,  0.0029632,  0.0029676,  0.0029720, &
        0.0029761,  0.0029805/
      data ((coa(i,j),i=1,62),j= 93, 93)/ &
        0.0018726,  0.0019238,  0.0019660,  0.0020019,  0.0020331, &
        0.0020606,  0.0020852,  0.0021074,  0.0021278,  0.0021464, &
        0.0021179,  0.0021798,  0.0021948,  0.0022089,  0.0022221, &
        0.0022347,  0.0022466,  0.0022578,  0.0022686,  0.0022789, &
        0.0022887,  0.0022981,  0.0023071,  0.0023158,  0.0023241, &
        0.0023321,  0.0023399,  0.0023474,  0.0023546,  0.0023616, &
        0.0023684,  0.0023750,  0.0023814,  0.0023876,  0.0023936, &
        0.0023995,  0.0024052,  0.0024108,  0.0024162,  0.0024215, &
        0.0024266,  0.0024317,  0.0024366,  0.0024414,  0.0024461, &
        0.0024507,  0.0024552,  0.0024596,  0.0024639,  0.0024681, &
        0.0024722,  0.0024763,  0.0024802,  0.0024841,  0.0024880, &
        0.0024917,  0.0024954,  0.0024990,  0.0025026,  0.0025060, &
        0.0025095,  0.0025129/
      data ((coa(i,j),i=1,62),j= 94, 94)/ &
        0.0016337,  0.0016718,  0.0017033,  0.0017303,  0.0017537, &
        0.0017745,  0.0017931,  0.0018100,  0.0018254,  0.0018397, &
        0.0018529,  0.0018651,  0.0018766,  0.0018874,  0.0018976, &
        0.0019073,  0.0019164,  0.0019251,  0.0019334,  0.0019413, &
        0.0019489,  0.0019562,  0.0019631,  0.0019698,  0.0019763, &
        0.0019825,  0.0019885,  0.0019944,  0.0020000,  0.0020054, &
        0.0020107,  0.0020158,  0.0020208,  0.0020256,  0.0020303, &
        0.0020349,  0.0020394,  0.0020437,  0.0020479,  0.0020521, &
        0.0020561,  0.0020600,  0.0020639,  0.0020676,  0.0020713, &
        0.0020749,  0.0020784,  0.0020819,  0.0020852,  0.0020886, &
        0.0020918,  0.0020950,  0.0020981,  0.0021011,  0.0021041, &
        0.0021071,  0.0021100,  0.0021128,  0.0021156,  0.0021183, &
        0.0021210,  0.0021236/
      data ((coa(i,j),i=1,62),j= 95, 95)/ &
        0.0014740,  0.0015024,  0.0015259,  0.0015460,  0.0015636, &
        0.0015791,  0.0015931,  0.0016058,  0.0016174,  0.0016282, &
        0.0016381,  0.0016474,  0.0016561,  0.0016643,  0.0016720, &
        0.0016793,  0.0016863,  0.0016929,  0.0016992,  0.0017052, &
        0.0017110,  0.0017165,  0.0017219,  0.0017270,  0.0017319, &
        0.0017367,  0.0017413,  0.0017458,  0.0017501,  0.0017543, &
        0.0017584,  0.0017623,  0.0017661,  0.0017699,  0.0017735, &
        0.0017770,  0.0017804,  0.0017838,  0.0017870,  0.0017902, &
        0.0017933,  0.0017964,  0.0017993,  0.0018023,  0.0018051, &
        0.0018079,  0.0018106,  0.0018132,  0.0018158,  0.0018184, &
        0.0018209,  0.0018233,  0.0018258,  0.0018281,  0.0018304, &
        0.0018327,  0.0018349,  0.0018371,  0.0018393,  0.0018414, &
        0.0018434,  0.0018455/
      data ((coa(i,j),i=1,62),j= 96, 96)/ &
        0.0013895,  0.0014110,  0.0014289,  0.0014441,  0.0014574, &
        0.0014692,  0.0014798,  0.0014894,  0.0014982,  0.0015064, &
        0.0015139,  0.0015210,  0.0015277,  0.0015338,  0.0015398, &
        0.0015454,  0.0015508,  0.0015558,  0.0015607,  0.0015653, &
        0.0015698,  0.0015740,  0.0015782,  0.0015821,  0.0015859, &
        0.0015896,  0.0015932,  0.0015966,  0.0016000,  0.0016032, &
        0.0016064,  0.0016094,  0.0016124,  0.0016153,  0.0016181, &
        0.0016208,  0.0016235,  0.0016261,  0.0016286,  0.0016311, &
        0.0016335,  0.0016358,  0.0016381,  0.0016404,  0.0016426, &
        0.0016447,  0.0016468,  0.0016489,  0.0016509,  0.0016529, &
        0.0016548,  0.0016567,  0.0016586,  0.0016604,  0.0016622, &
        0.0016639,  0.0016657,  0.0016673,  0.0016690,  0.0016706, &
        0.0016722,  0.0016738/
      data ((coa(i,j),i=1,62),j= 97, 97)/ &
        0.0013502,  0.0013669,  0.0013807,  0.0013924,  0.0014027, &
        0.0014118,  0.0014200,  0.0014274,  0.0014343,  0.0014406, &
        0.0014465,  0.0014520,  0.0014571,  0.0014620,  0.0014666, &
        0.0014710,  0.0014751,  0.0014791,  0.0014829,  0.0014865, &
        0.0014900,  0.0014933,  0.0014966,  0.0014997,  0.0015027, &
        0.0015055,  0.0015083,  0.0015109,  0.0015136,  0.0015162, &
        0.0015186,  0.0015210,  0.0015234,  0.0015256,  0.0015278, &
        0.0015299,  0.0015320,  0.0015340,  0.0015360,  0.0015380, &
        0.0015398,  0.0015417,  0.0015435,  0.0015452,  0.0015469, &
        0.0015486,  0.0015503,  0.0015519,  0.0015534,  0.0015550, &
        0.0015565,  0.0015580,  0.0015594,  0.0015608,  0.0015622, &
        0.0015636,  0.0015649,  0.0015663,  0.0015676,  0.0015688, &
        0.0015701,  0.0015713/
      data ((coa(i,j),i=1,62),j= 98, 98)/ &
        0.0013341,  0.0013476,  0.0013588,  0.0013683,  0.0013766, &
        0.0013840,  0.0013907,  0.0013967,  0.0014023,  0.0014074, &
        0.0014122,  0.0014167,  0.0014209,  0.0014248,  0.0014286, &
        0.0014321,  0.0014355,  0.0014387,  0.0014418,  0.0014447, &
        0.0014476,  0.0014503,  0.0014529,  0.0014554,  0.0014578, &
        0.0014602,  0.0014624,  0.0014646,  0.0014667,  0.0014688, &
        0.0014708,  0.0014727,  0.0014746,  0.0014764,  0.0014782, &
        0.0014799,  0.0014816,  0.0014833,  0.0014849,  0.0014864, &
        0.0014879,  0.0014894,  0.0014909,  0.0014923,  0.0014937, &
        0.0014950,  0.0014964,  0.0014977,  0.0014989,  0.0015002, &
        0.0015014,  0.0015026,  0.0015038,  0.0015049,  0.0015060, &
        0.0015071,  0.0015082,  0.0015093,  0.0015103,  0.0015114, &
        0.0015124,  0.0015134/
      data ((coa(i,j),i=1,62),j= 99, 99)/ &
        0.0013255,  0.0013373,  0.0013470,  0.0013554,  0.0013626, &
        0.0013691,  0.0013749,  0.0013803,  0.0013851,  0.0013896, &
        0.0013938,  0.0013977,  0.0014014,  0.0014049,  0.0014082, &
        0.0014113,  0.0014142,  0.0014171,  0.0014197,  0.0014223, &
        0.0014248,  0.0014272,  0.0014294,  0.0014316,  0.0014337, &
        0.0014358,  0.0014378,  0.0014397,  0.0014415,  0.0014433, &
        0.0014450,  0.0014467,  0.0014483,  0.0014499,  0.0014515, &
        0.0014530,  0.0014544,  0.0014559,  0.0014573,  0.0014586, &
        0.0014599,  0.0014612,  0.0014625,  0.0014637,  0.0014649, &
        0.0014661,  0.0014672,  0.0014684,  0.0014695,  0.0014705, &
        0.0014716,  0.0014726,  0.0014736,  0.0014746,  0.0014756, &
        0.0014766,  0.0014775,  0.0014784,  0.0014793,  0.0014802, &
        0.0014811,  0.0014820/
      data ((coa(i,j),i=1,62),j=100,100)/ &
        0.0013126,  0.0013234,  0.0013324,  0.0013401,  0.0013469, &
        0.0013529,  0.0013583,  0.0013632,  0.0013677,  0.0013719, &
        0.0013758,  0.0013795,  0.0013829,  0.0013861,  0.0013891, &
        0.0013920,  0.0013947,  0.0013974,  0.0013998,  0.0014022, &
        0.0014045,  0.0014067,  0.0014088,  0.0014108,  0.0014127, &
        0.0014146,  0.0014164,  0.0014181,  0.0014198,  0.0014215, &
        0.0014230,  0.0014246,  0.0014261,  0.0014275,  0.0014289, &
        0.0014303,  0.0014316,  0.0014329,  0.0014341,  0.0014354, &
        0.0014366,  0.0014377,  0.0014389,  0.0014400,  0.0014411, &
        0.0014421,  0.0014432,  0.0014442,  0.0014452,  0.0014462, &
        0.0014471,  0.0014480,  0.0014490,  0.0014499,  0.0014507, &
        0.0014516,  0.0014525,  0.0014533,  0.0014541,  0.0014549, &
        0.0014557,  0.0014565/
      data ((coa(i,j),i=1,62),j=101,101)/ &
        0.0012882,  0.0012983,  0.0013066,  0.0013138,  0.0013202, &
        0.0013258,  0.0013309,  0.0013355,  0.0013398,  0.0013437, &
        0.0013473,  0.0013507,  0.0013539,  0.0013569,  0.0013598, &
        0.0013625,  0.0013650,  0.0013674,  0.0013697,  0.0013719, &
        0.0013740,  0.0013760,  0.0013780,  0.0013798,  0.0013816, &
        0.0013833,  0.0013850,  0.0013865,  0.0013881,  0.0013896, &
        0.0013910,  0.0013924,  0.0013938,  0.0013951,  0.0013963, &
        0.0013976,  0.0013988,  0.0013999,  0.0014011,  0.0014022, &
        0.0014033,  0.0014043,  0.0014054,  0.0014064,  0.0014073, &
        0.0014083,  0.0014092,  0.0014101,  0.0014110,  0.0014119, &
        0.0014128,  0.0014136,  0.0014145,  0.0014153,  0.0014161, &
        0.0014168,  0.0014176,  0.0014183,  0.0014191,  0.0014198, &
        0.0014205,  0.0014212/

!-----


!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
   swh(ic,1)=0.
   so2(ic,1)=0.
!-----snt is the secant of the solar zenith angle
   snt(ic)=1.0  /cosz(ic)
 endif
 ENDDO

 do k=1,np
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then

!
!-----compute layer thickness. indices for the surface level and
!     surface layer are np+1 and np, respectively.
    dp(ic,k)=pl(ic,k+1)-pl(ic,k)


!
!-----compute scaled water vapor amount following eqs. (3.3) and (3.5)
!     unit is g/cm**2
!
    scal(ic,k)= dp(ic,k)*(.5*(pl(ic,k)+pl(ic,k+1))/300.)**.8  
    wh(ic,k)=1.02*wa(ic,k)*scal(ic,k) &
          *(1.+0.00135*(ta(ic,k)-240.)) +1.e-11
    swh(ic,k+1)=swh(ic,k)+wh(ic,k)
!-----compute ozone amount, unit is (cm-atm)stp
!     the number 466.7 is the unit conversion factor
!     from g/cm**2 to (cm-atm)stp

    oh(ic,k)=1.02*oa(ic,k)*dp(ic,k)*466.7 +1.e-11

 endif
 ENDDO
 enddo


!-----initialize fluxes for all-sky (flx), clear-sky (flc), and
!     flux reduction (df)
 flx    = 0.
 flc    = 0.
 flxu   = 0.
 flxd   = 0.
 df     = 0.
 df_sub = 0.
 df_cld = 0.
 df_clr = 0.

!-----compute solar uv and par fluxes
!ccshie 9/18/04

 call sw_uvpar (np,wh,oh,dp, &
                ict,icb,fcld,cosz, &
                taucl,ssacl,asycl,taual,ssaal,asyal,taux,rsuvbm,rsuvdf, &
                flx,flc,flxd,fdiruv,fdifuv,fdirpar,fdifpar,lmask,irestrict)

!-----compute and update solar ir fluxes
 call sw_ir (np,wh,dp, &
             ict,icb,fcld,cosz, &
             taucl,ssacl,asycl,taual,ssaal,asyal,rsirbm,rsirdf, &
             flx,flc,flxd,fdirir,fdifir,lmask,irestrict)


!-----compute pressure-scaled o2 amount following eq. (3.5) with
!     f=1. unit is (cm-atm)stp.
!     the constant 165.22 equals (1000/980)*23.14%*(22400/32)
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
   cnt(ic)=165.22*snt(ic)
 endif
 ENDDO

 do k=1,np
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
      so2(ic,k+1)=so2(ic,k)+scal(ic,k)*cnt(ic)
 endif
 ENDDO
 enddo

!-----compute flux reduction due to o2 following eq. (3.18)
!     the constant 0.0633 is the fraction of insolation contained
!     in the oxygen bands 
 do k= 2, np+1
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
       x=so2(ic,k)
       df(ic,k)=0.0633*(1.-exp(-0.000145*sqrt(x)))
 endif
 ENDDO
 enddo

!-----for solar heating due to co2
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
   cnt(ic)=co2*snt(ic)
 endif
 ENDDO
!-----scale co2 amounts following eq. (3.5) with f=1.
!     unit is (cm-atm)stp.
!     the constant 789 equals (1000/980)*(44/28.97)*(22400/44)
 do k=1,np
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
    x=789.*cnt(ic)
    so2(ic,k+1)=so2(ic,k)+x*scal(ic,k)+1.e-11
 endif
 ENDDO
 enddo
!-----for co2 absorption in band 10 where absorption due to
!     water vapor and co2 are both moderate
 u1=-3.0
 du=0.15
 w1=-4.0
 dw=0.15
!-----so2 and swh are the co2 and water vapor amounts integrated
!     from the top of the atmosphere
 do k= 2, np+1
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
    swu(ic,k)=log10(so2(ic,k))
    swh(ic,k)=log10(swh(ic,k)*snt(ic))
 endif
 ENDDO
 enddo
!-----df is the updated flux reduction given by the second term on the
!     right-hand-side of eq. (3.24) divided by so
 call reduce_flux(np,swu,u1,du,nu,swh,w1,dw,nw,cah,df,lmask,irestrict)
!-----for co2 absorption in band 11 where the co2 absorption has
!     a large impact on the heating of middle atmosphere.
 u1=0.000250
 du=0.000050
 w1=-2.0
 dw=0.05
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
   swu(ic,1)=co2*snt(ic)
 endif
 ENDDO
!-----co2 mixing ratio is independent of space (spatially homogeneous)
 do k= 2, np+1
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
    swu(ic,k)=swu(ic,1)
 endif
 ENDDO
 enddo
!-----swh is the logarithm of pressure
 do k= 2, np+1
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
       swh(ic,k)=log10(pl(ic,k))
 endif
 ENDDO
 enddo
!-----df is the updated flux reduction derived from the table given by
!     eq. (3.19)
 call reduce_flux(np,swu,u1,du,nx2,swh,w1,dw,ny2,coa,df,lmask,irestrict)

! compute layer sub df (also filter negative values)
 do k = 2, np+1
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
       df_sub(ic,k) = max(df(ic,k) - df(ic,k-1), 0.)  !df for each layer (remove negative df_sub) 
 endif
 ENDDO
 enddo

! compute clear-sky df
 do k = 2, np+1
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
    df_clr(ic,k) = df_clr(ic,k-1)+df_sub(ic,k)  
 endif
 ENDDO
 enddo

!-----adjustment for the effect of o2 cnd co2 on clear-sky fluxes.
!     both flc and df_clr are positive quantities
 do k=1,np+1
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
    flc(ic,k)=max(flc(ic,k)-df_clr(ic,k),0.)  !this filter is for small cosine zenith angle.
 endif
 ENDDO
 enddo

!-----identify top cloud-layer

!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
   nctop(ic)=np+1
 endif
 ENDDO

 do k=1,np
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
    if (fcld(ic,k).gt.fcld_min .and. nctop(ic).eq.np+1) then
        nctop(ic)=k
    endif
 endif
 ENDDO
 enddo

! adjust df_sub for below cloud
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
   ntop(ic)=nctop(ic)
   cld_alb=0.
   if(overcast .and. fast_overcast) then !compute cloud albedo
    cld_alb = sum(taux(ic,ntop(ic)+1:np))/(6.7+sum(taux(ic,ntop(ic)+1:np)))
   endif
   if (ntop(ic).lt.np+1) then
    do k= ntop(ic)+1,np+1 !cloud top -> surface
       if(overcast .and. fast_overcast) then  !use ratio in LUT
           i_cos = min(max(int(cosz(ic)*10.)+1,   1),10) !1~10
           i_tau = min(max(int(cld_alb*10.)+1,1),10) !1~10
           ratio = ratio_lut(i_tau,i_cos)
       else !use computed clear and cloudy flux ratio (not fast_overcast)
           ratio = max(0.01, min(1.,(flx(ic,k)/flc(ic,k))))
       endif
       df_sub(ic,k)  = df_sub(ic,k)*ratio  !compute cloudy-sky df_sub
    enddo !k
   endif
 endif
 ENDDO

!update df for cloudy-sky
 do k = 2, np+1
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
      df_cld(ic,k) = df_cld(ic,k-1)+df_sub(ic,k) 
 endif
 ENDDO
 enddo

!-----adjustment for the effect of o2 cnd co2 on all-sky fluxes.
!      max statement filter negative value in flx for small cosz (for df_cld > flx)
 do k = 1, np+1
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
    flx(ic,k)  = max(flx(ic,k)-df_cld(ic,k) , 0.)  !this max is for small cosz
    flxd(ic,k) = max(flxd(ic,k)-df_cld(ic,k), 0.)  !this max is for small cosz
    flxu(ic,k) = flx(ic,k)-flxd(ic,k)
 endif
 ENDDO
 enddo 

!-----adjustment for the direct downward flux
!
! flux reduction all goes to direct NIR radiation.
! Correctd by toshi. In cloudy case fdirir often become zero. 
! So, it must also reduce fdifir to make sure energy budgets are equal between
! flxd,flx and flxd_surf.
!
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then

   if (  (fdirir(ic)-df_cld(ic,np+1)) >= 0. ) then ! normal
    fdirir(ic)=fdirir(ic)-df_cld(ic,np+1)  ! updated 
   else ! if negative, it also reduces diffuse component.
    fdifir(ic) = MAX(0., fdifir(ic) + (fdirir(ic)-df_cld(ic,np+1)) )
    fdirir(ic)=0.
   endif
!
! Convert double to single precision
!
   flx_out(ic,:)  = REAL(flx(ic,:) )  !flux fraction divergence []
   flxd_out(ic,:) = REAL(flxd(ic,:))  !flux fraction downward []
   flxu_out(ic,:) = REAL(flxu(ic,:))  !flux fraction upward []

!
! Beam/diffuse radiation at UV-VIS/NIR bands for Land-Surface Models
!
   flxd_surf(ic,1) = REAL( fdiruv(ic)+fdirpar(ic) ) !direct UV+PAR
   flxd_surf(ic,2) = REAL( fdifuv(ic)+fdifpar(ic) ) !diffuse UV+PAR
   flxd_surf(ic,3) = REAL( fdirir(ic)         ) !direct NIR
   flxd_surf(ic,4) = REAL( fdifir(ic)         ) !diffuse NIR
 endif
 ENDDO

 end subroutine swrad

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine sw_uvpar (np,wh,oh,dp, &
                ict,icb,fcld,cosz, &
                taucl,ssacl,asycl,taual,ssaal,asyal,taux,rsuvbm,rsuvdf, &
                flx,flc,flxd,fdiruv,fdifuv,fdirpar,fdifpar,lmask,irestrict)
 implicit none

!******************************************************************
!  compute solar fluxes in the uv+par region. the spectrum is
!  grouped into 8 bands:
!
!              band     micrometer
!
!       uv-c    1.     .175 - .225
!               2.     .225 - .245
!                      .260 - .280
!               3.     .245 - .260
!
!       uv-b    4.     .280 - .295
!               5.     .295 - .310
!               6.     .310 - .320
!
!       uv-a    7.     .320 - .400
!
!       par     8.     .400 - .700
!
!----- input parameters:                            units      size
!
!  number of atmospheric layers (np)                n/d         1
!  layer scaled-water vapor content (wh)          gm/cm^2      np
!  layer ozone content (oh)                      (cm-atm)stp   np
!  layer pressure thickness (dp)                    mb         np
!  option for scaling cloud optical thickness       n/d         1
!        overcast="true" if scaling is not required
!        overcast="fasle" if scaling is required
!  cloud water amount (cwp)                        gm/m**2     np*5
!        index 1 for ice particles
!        index 2 for liquid drops
!        index 3 for rain drops
!        index 4 for snow
!        index 5 for graupel
!  cloud optical thickness (taucld)                 n/d        np*5
!       index 1 for ice particles
!       index 2 for liquid drops
!       index 3 for rain drops
!  effective cloud-particle size (reff)          micrometer    np*5
!       index 1 for ice paticles
!       index 2 for liquid drops
!       index 3 for rain drops
!       index 4 for snow
!       index 5 for graupel
!  level index separating high and                  n/d       1 
!       middle clouds (ict)
!  level indiex separating middle and               n/d       1 
!       low clouds (icb)
!  cloud amount (fcld)                            fraction     np
!  cosine of solar zenith angle (cosz)              n/d        1
!  aerosol optical thickness (taual)                n/d        np*11
!  aerosol single-scattering albedo (ssaal)         n/d        np*11
!  aerosol asymmetry factor (asyal)                 n/d        np*11
!  uv+par surface albedo for beam                 fraction     1 
!       radiation (rsuvbm)
!  uv+par surface albedo for diffuse              fraction     1 
!       radiation (rsuvdf)
!
!---- temporary array
!
!  scaled cloud optical thickness                   n/d        np
!       for beam radiation (tauclb)
!  scaled cloud optical thickness                   n/d        np
!       for diffuse radiation  (tauclf)
!
!----- output (updated) parameters:
!
!  all-sky flux divergence (downward-upward) (flx)               fraction      (np+1)
!  clear-sky flux divergence (downward-upward) (flc)             fraction      (np+1)
!  all-sky direct downward uv flux at
!       the surface (fdiruv)                     fraction     1 
!  all-sky diffuse downward uv flux at
!       the surface (fdifuv)                     fraction     1 
!  all-sky direct downward par flux at
!       the surface (fdirpar)                    fraction     1 
!  all-sky diffuse downward par flux at
!       the surface (fdifpar)                    fraction     1 
!
!***********************************************************************
!-----input parameters
 integer,intent(in) :: irestrict
 logical,dimension(CHUNK),intent(in) :: lmask
 integer np,ict(CHUNK),icb(CHUNK)
 real(fp_kind) fcld(CHUNK,np)
 real(fp_kind) wh(CHUNK,np),oh(CHUNK,np),dp(CHUNK,np)
 real(fp_kind) taucl(CHUNK,np,nband_sw),ssacl(CHUNK,np,nband_sw),asycl(CHUNK,np,nband_sw)
 real(fp_kind) taual(CHUNK,np,nband_sw),ssaal(CHUNK,np,nband_sw),asyal(CHUNK,np,nband_sw)
 real(fp_kind) rsuvbm(CHUNK),rsuvdf(CHUNK),cosz(CHUNK)
!-----output (updated) parameter
 real(fp_kind) flx(CHUNK,np+1),flc(CHUNK,np+1)
 real(fp_kind) flxd(CHUNK,np+1)
 real(fp_kind) fdiruv(CHUNK) ,fdifuv(CHUNK)
 real(fp_kind) fdirpar(CHUNK),fdifpar(CHUNK)
 real(fp_kind) taux(CHUNK,np)

!-----static parameters
 integer nband
 parameter (nband=8)
 real(fp_kind) hk(nband),wk(nband),zk(nband),ry(nband)
 real(fp_kind) aig(3),awg(3),arg(3)
 real(fp_kind) aib(2),awb(2),arb(2)
!-----temporary array
 integer :: k,ib,ic
 integer ::  ih1,ih2,im1,im2,is1,is2
 real(fp_kind) :: taurs(CHUNK,np),tauoz(CHUNK,np),tauwv(CHUNK,np) ! TODO: scalarize
! real(fp_kind) :: g(max_spc) !asymetry factors
 real(fp_kind) :: dsm
 real(fp_kind) :: tauclb(CHUNK,np,nband_sw)
 real(fp_kind) :: tauclf(CHUNK,np,nband_sw)
 real(fp_kind) :: tausto(CHUNK)
 real(fp_kind) :: ssatau(CHUNK)
 real(fp_kind) :: asysto(CHUNK)
 real(fp_kind) :: tautob(CHUNK)
 real(fp_kind) :: ssatob(CHUNK)
 real(fp_kind) :: asytob(CHUNK)
 real(fp_kind) :: tautof(CHUNK)
 real(fp_kind) :: ssatof(CHUNK)
 real(fp_kind) :: asytof(CHUNK)
 real(fp_kind) :: rr(CHUNK,np+1,2)
 real(fp_kind) :: tt(CHUNK,np+1,2)
 real(fp_kind) :: td(CHUNK,np+1,2)
 real(fp_kind) :: rs(CHUNK,np+1,2)
 real(fp_kind) :: ts(CHUNK,np+1,2)
 real(fp_kind) :: fall(CHUNK,np+1)
 real(fp_kind) :: falld(CHUNK,np+1)
 real(fp_kind) :: fclr(CHUNK,np+1)
 real(fp_kind) :: fsdir(CHUNK)
 real(fp_kind) :: fsdif(CHUNK)
 real(fp_kind) :: asyclt(CHUNK)
 real(fp_kind) :: cc(CHUNK,3)
 real(fp_kind) :: rrt(CHUNK,np)
 real(fp_kind) :: ttt(CHUNK,np)
 real(fp_kind) :: tdt(CHUNK,np)
 real(fp_kind) :: rst(CHUNK,np)
 real(fp_kind) :: tst(CHUNK,np)
 real(fp_kind) :: dum1(CHUNK,np+1)
 real(fp_kind) :: dum2(CHUNK)
 real(fp_kind) :: dum3(CHUNK)
 real(fp_kind) :: dum(CHUNK,np)
!-----hk is the fractional extra-terrestrial solar flux in each
!     of the 8 bands. the sum of hk is 0.47074. (table 3)
      data hk/.00057, .00367, .00083, .00417, &
              .00600, .00556, .05913, .39081/
!-----zk is the ozone absorption coefficient. unit: /(cm-atm)stp
!     (table 3)
      data zk /30.47, 187.2,  301.9,   42.83, &
               7.09,  1.25,   0.0345,  0.0572/
!-----wk is the water vapor absorption coefficient. unit: cm**2/g
!     (table 3)
      data wk /7*0.0, 0.00075/
!-----ry is the extinction coefficient for rayleigh scattering.
!     unit: /mb. (table 3)
      data ry /.00604, .00170, .00222, .00132, &
               .00107, .00091, .00055, .00012/
!-----coefficients for computing the extinction coefficients of ice,
!     water, and rain particles, independent of spectral band. (table 4)
      data aib/ 3.33e-4,2.52/
      data awb/-6.59e-3,1.65/
      data arb/ 3.07e-3,0.00/
!-----coefficients for computing the asymmetry factor of ice, water,
!     and rain particles, independent of spectral band. (table 6)
      data aig/.74625,.0010541,-.00000264/
      data awg/.82562,.0052900,-.00014866/
      data arg/.883,0.0,0.0/
!-----initialize fdiruv, fdifuv, surface reflectances and transmittances.
!     the reflectance and transmittance of the clear and cloudy portions
!     of a layer are denoted by 1 and 2, respectively.
!     cc is the maximum cloud cover in each of the high, middle, and low
!     cloud groups.
!     1/dsm=1/cos(53) = 1.66

 dsm=0.602e0
!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
   fdiruv=0.e0
   fdifuv=0.e0
   cc(ic,1:3)=0.e0

!
! surface boundary conditions
!
   rr(ic,np+1,1)=rsuvbm(ic)
   rr(ic,np+1,2)=rsuvbm(ic)
   rs(ic,np+1,1)=rsuvdf(ic)
   rs(ic,np+1,2)=rsuvdf(ic)

   td(ic,np+1,1:2)=0.e0
   tt(ic,np+1,1:2)=0.e0
   ts(ic,np+1,1:2)=0.e0
 endif
 ENDDO

!-----options for scaling cloud optical thickness

 if (overcast) then  !CRM or LES

!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
     tauclb(ic,:,:) = taucl(ic,:,:)
     tauclf(ic,:,:) = taucl(ic,:,:)
     cc(ic,1:3)=1.e0  !cc is the maximum cloud cover in each of the high, middle, and low cloud gruops
 endif
 ENDDO

 else  !for GCM (toshii work on lator)

!-----scale cloud optical thickness in each layer from taucld (with
!     cloud amount fcld) to tauclb and tauclf (with cloud amount cc).
!     tauclb is the scaled optical thickness for beam radiation and
!     tauclf is for diffuse radiation (see section 7).
!     So this gives different cloud optical depths for beam and diffuse radiation...

     do ib=1,nband
!     DO ic=1,irestrict
!     if(lmask(ic) .eqv. .true.) then
        call cloud_scale ( np,cosz,fcld,taucl,ict,icb, &
                           cc,tauclb,tauclf,lmask,irestrict )
!     endif
!     ENDDO
     enddo

 endif

!
! uv-vis band integration
!
 BAND_LOOP: do ib=1,nband  !nband is 8 in sw_uvpar 

!-----compute reflectance and transmittance of the clear portion of a layer

   LEVEL_LOOP: do k=1,np

!dir$ vector aligned
   DO ic=1,irestrict
   if(lmask(ic) .eqv. .true.) then

!-----compute rayleigh, ozone and water vapor optical thicknesses
      taurs(ic,k)=ry(ib)*dp(ic,k) 
      tauoz(ic,k)=zk(ib)*oh(ic,k)
      tauwv(ic,k)=wk(ib)*wh(ic,k)

!
! total optical properties without cloud
!
      tausto(ic)=max(taurs(ic,k)+tauoz(ic,k)+tauwv(ic,k)+taual(ic,k,ib),opt_min)
      ssatau(ic)=ssaal(ic,k,ib)*taual(ic,k,ib)+taurs(ic,k)
      asysto(ic)=asyal(ic,k,ib)*ssaal(ic,k,ib)*taual(ic,k,ib)


!-----compute clear-sky optical thickness, single scattering albedo,
!     and asymmetry factor (eqs. 6.2-6.4)

   endif
   enddo

      IF_CLEAR: if (overcast .and. fast_overcast ) then 
!dir$ vector aligned
   DO ic=1,irestrict
   if(lmask(ic) .eqv. .true.) then

           rr(ic,k,1)=0.e0
           tt(ic,k,1)=0.e0
           td(ic,k,1)=0.e0
           rs(ic,k,1)=0.e0
           ts(ic,k,1)=0.e0

   endif
   enddo

      else !GCM or overcast (need clear-sky portion of optical properties)
!dir$ vector aligned
   DO ic=1,irestrict
   if(lmask(ic) .eqv. .true.) then

          tautob(ic)=tausto(ic)
          ssatob(ic)=min(max(ssatau(ic)/tautob(ic),ssa_min), ssa_max)
          asytob(ic)=min(max(asysto(ic)/(ssatob(ic)*tautob(ic)),asy_min), asy_max)

!-----Compute delta-eddington approximation of scattering properties
!     for direct incident radiation

   endif
   enddo
!dir$ vector aligned
!dir$ SIMD
   DO ic=1,irestrict
   if(lmask(ic) .eqv. .true.) then

          call delta_eddington(tautob(ic), ssatob(ic), asytob(ic), cosz(ic) , &
                               rrt(ic,k),ttt(ic,k), tdt(ic,k) )

!     
!-----diffuse incident radiation is approximated by beam radiation with
!     an incident angle of 53 degrees, eqs. (6.5) and (6.6)

          call delta_eddington(tautob(ic), ssatob(ic), asytob(ic), dsm , &
                               rst(ic,k),tst(ic,k), dum(ic,k) )

   endif
   enddo
!dir$ vector aligned
   DO ic=1,irestrict
   if(lmask(ic) .eqv. .true.) then

           rr(ic,k,1)=rrt(ic,k)
           tt(ic,k,1)=ttt(ic,k)
           td(ic,k,1)=tdt(ic,k)
           rs(ic,k,1)=rst(ic,k)
           ts(ic,k,1)=tst(ic,k)

   endif
   enddo

      endif IF_CLEAR


   DO ic=1,irestrict
   if(lmask(ic) .eqv. .true.) then

!-----compute reflectance and transmittance of the cloudy portion of a layer

!-----for direct incident radiation
!     the effective layer optical properties. eqs. (6.2)-(6.4)  !modify max & min
           tautob(ic)=tausto(ic) + max(tauclb(ic,k,ib),0.e0)
           ssatob(ic)=min(max((ssatau(ic)+ssacl(ic,k,ib)*tauclb(ic,k,ib))/tautob(ic) , ssa_min), ssa_max)  !SSA of cloud is unity
           asytob(ic)=min(max((asysto(ic)+asycl(ic,k,ib)*ssacl(ic,k,ib)*tauclb(ic,k,ib)) &
                         /(   max(ssatob(ic)*tautob(ic),const_tiny)   ),asy_min), asy_max)

!-----for diffuse incident radiation (same values)
        if(overcast) then  !CRM case direct and diffuse component are same
!           tautof(ic)=tautob(ic)
!           ssatof(ic)=ssatob(ic)     
!           asytof(ic)=asytob(ic)
        else               !GCM case overlappsing scheme generate different beam and diffuse component.
!           tautof(ic)=tausto(ic) + max(tauclf(ic,k,ib),0.e0)
!           ssatof(ic)=min(max((ssatau(ic)+ssacl(ic,k,ib)*tauclf(ic,k,ib))/tautob(ic) , ssa_min), ssa_max)  !SSA of cloud is unity
!           asytof(ic)=min(max((asysto(ic)+asycl(ic,k,ib)*ssacl(ic,k,ib)*tauclf(ic,k,ib)) &
!                         /(   max(ssatob(ic)*tautob(ic),const_tiny)    ),asy_min), asy_max)
        endif

!-----Compute delta-eddington approximation of scattering properties
!     for direct incident radiation
!     note that the cloud optical thickness is scaled differently for direct
!     and diffuse insolation, eqs. (7.3) and (7.4).

   endif
   enddo
!dir$ vector aligned
!dir$ SIMD
   DO ic=1,irestrict
   if(lmask(ic) .eqv. .true.) then

           call delta_eddington(tautob(ic), ssatob(ic), asytob(ic), cosz(ic) , &
                                rrt(ic,k),ttt(ic,k), tdt(ic,k) )

!-----diffuse incident radiation is approximated by beam radiation with
!     an incident angle of 53 degrees, eqs. (6.5) and (6.6)

           call delta_eddington(tautob(ic), ssatob(ic), asytob(ic), dsm , &
                                rst(ic,k),tst(ic,k), dum(ic,k) )


   endif
   enddo
   DO ic=1,irestrict
   if(lmask(ic) .eqv. .true.) then

           rr(ic,k,2)=rrt(ic,k)
           tt(ic,k,2)=ttt(ic,k)
           td(ic,k,2)=tdt(ic,k)
           rs(ic,k,2)=rst(ic,k)
           ts(ic,k,2)=tst(ic,k)

   endif
   ENDDO
   enddo LEVEL_LOOP


!-----flux calculations

   if (overcast) then   !cloud fraction 0 or 1 -> CRM 

       if( .not. fast_overcast ) then
!-----for clear-sky fluxes only (This is needed for equation 6.18 flux redtion due to CO2 and O2 below cloud)
         ih1=1 ; im1=1 ; is1=1
         ih2=1 ; im2=1 ; is2=1 
         call twostream_adding (np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                      cc,rr,tt,td,rs,ts,fclr,dum1,falld,dum2,dum3,lmask,irestrict) 
       endif

!-----for cloudy-sky fluxes only
         ih1=2 ; im1=2 ; is1=2
         ih2=2 ; im2=2 ; is2=2     
         call twostream_adding (np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                      cc,rr,tt,td,rs,ts,dum1,fall,falld,fsdir,fsdif,lmask,irestrict) 
   else  ! non overcast (cloud fraction 0.~1. GCM or RCM ) 

!-----for clear- and all-sky fluxes the all-sky flux, fall is the summation inside the brackets of eq. (7.11)
         ih1=1 ; im1=1 ; is1=1
         ih2=2 ; im2=2 ; is2=2   
         call twostream_adding (np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                      cc,rr,tt,td,rs,ts,fclr,fall,falld,fsdir,fsdif,lmask,irestrict) 
   endif  ! overcast  


!-----flux integration, eq. (6.1)

   do k=1,np+1
!dir$ vector aligned
   DO ic=1,irestrict
   if(lmask(ic) .eqv. .true.) then
      flx(ic,k)=flx(ic,k)+fall(ic,k)*hk(ib)
      flxd(ic,k)=flxd(ic,k)+falld(ic,k)*hk(ib)

      if(overcast .and. fast_overcast) then 
         !do nothing
      else 
         flc(ic,k)=flc(ic,k)+fclr(ic,k)*hk(ib)
      endif
   endif
   ENDDO
   enddo

!-----compute direct and diffuse downward surface fluxes in the uv
!     and par regions
!dir$ vector aligned
   DO ic=1,irestrict
   if(lmask(ic) .eqv. .true.) then
   if(ib.lt.8) then  !UV
       fdiruv(ic)=fdiruv(ic)+fsdir(ic)*hk(ib)  !beam 
       fdifuv(ic)=fdifuv(ic)+fsdif(ic)*hk(ib)  !diffuse
   else              !VIS
       fdirpar(ic)=fsdir(ic)*hk(ib)  !beam
       fdifpar(ic)=fsdif(ic)*hk(ib)  !diffuse
   endif
   endif
   ENDDO

 enddo BAND_LOOP

 end subroutine sw_uvpar

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine sw_ir (np,wh,dp, &
                   ict,icb,fcld,cosz, &
                   taucl,ssacl,asycl,taual,ssaal,asyal, &
                   rsirbm,rsirdf,flx,flc,flxd,fdirir,fdifir,lmask,irestrict)
 implicit none
!************************************************************************
!  compute solar flux in the infrared region. the spectrum is divided
!   into three bands:
!
!          band   wavenumber(/cm)  wavelength (micron)
!          1( 9)    14280-8200         0.70-1.22
!          2(10)     8200-4400         1.22-2.27
!          3(11)     4400-1000         2.27-10.0
!
!----- input parameters:                            units      size
!
!  number of atmospheric layers (np)                n/d         1
!  layer scaled-water vapor content (wh)          gm/cm^2      np
!  option for scaling cloud optical thickness       n/d         1
!        overcast="true" if scaling is not required
!        overcast="fasle" if scaling is required
!  cloud optical thickness (taucl)                  n/d       np*band
!  cloud single scattering albedo (ssacl)           n/d       np*band
!  cloud asymetry parameter (asycl)                 n/d       np*band
!  aerosol optical thickness (taual)                n/d       np*band
!  aerosol single scattering albedo (ssaal)         n/d       np*band
!  aerosol asymetry parameter (asyal)               n/d       np*band
!      
!  level index separating high and                  n/d       1 
!        middle clouds (ict)
!  level index separating middle and                n/d       1 
!        low clouds (icb)
!  cloud amount (fcld)                            fraction     np
!  aerosol optical thickness (taual)                n/d        np*11
!  aerosol single-scattering albedo (ssaal)         n/d        np*11
!  aerosol asymmetry factor (asyal)                 n/d        np*11
!  near ir surface albedo for beam                fraction     1 
!        radiation (rsirbm)
!  near ir surface albedo for diffuse             fraction     1 
!        radiation (rsirdf)
!
!---- temporary array
!
!  scaled cloud optical thickness                   n/d        np
!          for beam radiation (tauclb)
!  scaled cloud optical thickness                   n/d        np
!          for diffuse radiation  (tauclf)
!
!----- output (updated) parameters:
!
!  all-sky flux divergence (downward-upward) (flx)           fraction     (np+1)
!  clear-sky flux divergence (downward-upward) (flc)         fraction     (np+1)
!  all-sky direct downward ir flux at
!          the surface (fdirir)                   fraction     1 
!  all-sky diffuse downward ir flux at
!          the surface (fdifir)                   fraction     1 
!
!**********************************************************************
!-----input parameters
 logical lmask(CHUNK)
 integer irestrict
 integer np,ict(CHUNK),icb(CHUNK)
 integer ih1,ih2,im1,im2,is1,is2
 real(fp_kind) fcld(CHUNK,np),cosz(CHUNK)
 real(fp_kind) rsirbm(CHUNK),rsirdf(CHUNK)
 real(fp_kind) taucl(CHUNK,np,nband_sw),ssacl(CHUNK,np,nband_sw),asycl(CHUNK,np,nband_sw)
 real(fp_kind) taual(CHUNK,np,nband_sw),ssaal(CHUNK,np,nband_sw),asyal(CHUNK,np,nband_sw)
 real(fp_kind) dp(CHUNK,np),wh(CHUNK,np)
!-----output (updated) parameters
 real(fp_kind) flx(CHUNK,np+1),flc(CHUNK,np+1)
 real(fp_kind) flxd(CHUNK,np+1)
 real(fp_kind) fdirir(CHUNK),fdifir(CHUNK)
!-----static parameters
 integer nk,nband
 parameter (nk=10,nband=3)
 real(fp_kind) :: taux
! real(fp_kind) :: w(max_spc) !expansion stuff
! real(fp_kind) :: g(max_spc) !asymetry stuff
 real(fp_kind) hk(nband,nk),xk(nk),ry(nband)
 real(fp_kind) aib(nband,2),awb(nband,2),arb(nband,2)
 real(fp_kind) aia(nband,3),awa(nband,3),ara(nband,3)
 real(fp_kind) aig(nband,3),awg(nband,3),arg(nband,3)
!-----temporary array
 integer ib,iv,ik,k,ic
 real(fp_kind) taurs,tauwv
 real(fp_kind) :: dsm
 real(fp_kind) :: tauclb(CHUNK,np)
 real(fp_kind) :: tauclf(CHUNK,np)
 real(fp_kind) :: cc(CHUNK,3)
 real(fp_kind) :: rr(CHUNK,np+1,2)
 real(fp_kind) :: tt(CHUNK,np+1,2)
 real(fp_kind) :: td(CHUNK,np+1,2)
 real(fp_kind) :: rs(CHUNK,np+1,2)
 real(fp_kind) :: ts(CHUNK,np+1,2)
 real(fp_kind) :: fall(CHUNK,np+1)
 real(fp_kind) :: falld(CHUNK,np+1)
 real(fp_kind) :: fclr(CHUNK,np+1)
 real(fp_kind) :: fsdir(CHUNK)
 real(fp_kind) :: fsdif(CHUNK)
 real(fp_kind) :: tausto(CHUNK,np)
 real(fp_kind) :: ssatau(CHUNK,np)
 real(fp_kind) :: asysto(CHUNK,np)
 real(fp_kind) :: tautob(CHUNK,np)
 real(fp_kind) :: ssatob(CHUNK,np)
 real(fp_kind) :: asytob(CHUNK,np)
 real(fp_kind) :: tautof(CHUNK,np)
 real(fp_kind) :: ssatof(CHUNK,np)
 real(fp_kind) :: asytof(CHUNK,np)
! real(fp_kind) :: ssaclt(CHUNK)
! real(fp_kind) :: asyclt(CHUNK)
 real(fp_kind) :: rrt(CHUNK,np)
 real(fp_kind) :: ttt(CHUNK,np)
 real(fp_kind) :: tdt(CHUNK,np)
 real(fp_kind) :: rst(CHUNK,np)
 real(fp_kind) :: tst(CHUNK,np)
 real(fp_kind) :: dum1(CHUNK,np+1)
 real(fp_kind) :: dum2(CHUNK)
 real(fp_kind) :: dum3(CHUNK)
 real(fp_kind) :: dum(CHUNK,np)
!-----water vapor absorption coefficient for 10 k-intervals.
!     unit: cm^2/gm (table 2)
      data xk/ &
        0.0010, 0.0133, 0.0422, 0.1334, 0.4217, &
        1.334,  5.623,  31.62,  177.8,  1000.0/
!-----water vapor k-distribution function,
!     the sum of hk is 0.52926. unit: fraction (table 2) 
! old
!      data hk/ &
!       .20673,.08236,.01074,  .03497,.01157,.00360, &
!       .03011,.01133,.00411,  .02260,.01143,.00421, &
!       .01336,.01240,.00389,  .00696,.01258,.00326, &
!       .00441,.01381,.00499,  .00115,.00650,.00465, &
!       .00026,.00244,.00245,  .00000,.00094,.00145/
! new (HITRAN2012) from M.D. Chou
      data hk/ &
       .19943,.07968,.01042, .03830,.01247,.00353, &
       .03144,.01131,.00384, .02377,.01174,.00418, &
       .01416,.01230,.00387, .00747,.01321,.00325, &
       .00489,.01459,.00476, .00128,.00673,.00480, &
       .00031,.00249,.00247, .00001,.00102,.00153/

!-----ry is the extinction coefficient for rayleigh scattering.
!     unit: /mb (table 3)
      data ry /.0000156, .0000018, .000000/
!-----coefficients for computing the extinction coefficients of
!     ice, water, and rain particles (table 4)
      data aib/ &
        .000333, .000333, .000333, &
           2.52,    2.52,    2.52/
      data awb/ &
        -0.0101, -0.0166, -0.0339, &
           1.72,    1.85,    2.16/
      data arb/ &
         0.00307, 0.00307, 0.00307, &
         0.0    , 0.0    , 0.0    /
!-----coefficients for computing the single-scattering co-albedo of
!     ice, water, and rain particles (table 5)
      data aia/ &
       -.00000260, .00215346, .08938331, &
        .00000746, .00073709, .00299387, &
        .00000000,-.00000134,-.00001038/
      data awa/ &
        .00000007,-.00019934, .01209318, &
        .00000845, .00088757, .01784739, &
       -.00000004,-.00000650,-.00036910/
      data ara/ &
        .029,      .342,      .466, &
        .0000,     .000,      .000, &
        .0000,     .000,      .000/
!-----coefficients for computing the asymmetry factor of
!     ice, water, and rain particles (table 6)
      data aig/ &
        .74935228, .76098937, .84090400, &
        .00119715, .00141864, .00126222, &
       -.00000367,-.00000396,-.00000385/
      data awg/ &
        .79375035, .74513197, .83530748, &
        .00832441, .01370071, .00257181, &
       -.00023263,-.00038203, .00005519/
      data arg/ &
        .891,      .948,      .971, &
        .0000,     .000,      .000, &
        .0000,     .000,      .000/
!-----initialize surface fluxes, reflectances, and transmittances.
!     the reflectance and transmittance of the clear and cloudy portions
!     of a layer are denoted by 1 and 2, respectively.
!     cc is the maximum cloud cover in each of the high, middle, and low
!     cloud groups.
!     1/dsm=1/cos(53)=1.66
 dsm=0.602e0

!dir$ vector aligned
 DO ic=1,irestrict
 if(lmask(ic) .eqv. .true.) then
   fdirir(ic)=0.e0
   fdifir(ic)=0.e0
   cc(ic,1:3)=0.e0

!
! surface boundary condition
!
   rr(ic,np+1,1)=rsirbm(ic)
   rr(ic,np+1,2)=rsirbm(ic)
   rs(ic,np+1,1)=rsirdf(ic)
   rs(ic,np+1,2)=rsirdf(ic)
   td(ic,np+1,1:2)=0.e0
   tt(ic,np+1,1:2)=0.e0
   ts(ic,np+1,1:2)=0.e0
  endif
  ENDDO

!-----integration over spectral bands
 BAND_LOOP: do ib=1,nband  !nband is 3 for IR band
   iv=ib+8  !this index must be used 

!-----options for scaling cloud optical thickness

   if (overcast) then
!dir$ vector aligned
   DO ic=1,irestrict
   if(lmask(ic) .eqv. .true.) then
       tauclb(ic,:)=taucl(ic,:,iv)
       tauclf(ic,:)=taucl(ic,:,iv)
       cc(ic,1:3)=1.0
   endif
   ENDDO
   else
!-----scale cloud optical thickness in each layer from taucld (with
!     cloud amount fcld) to tauclb and tauclf (with cloud amount cc).
!     tauclb is the scaled optical thickness for beam radiation and
!     tauclf is for diffuse radiation.
!dir$ vector aligned
   DO ic=1,irestrict
   if(lmask(ic) .eqv. .true.) then
       call cloud_scale (np,cosz,fcld,taucl,ict,icb, &
                    cc,tauclb,tauclf,lmask,irestrict)
   endif
   ENDDO
   endif


!-----integration over the k-distribution function
   KD_LOOP: do ik=1,nk !nk is 10 so total IR routine has 30 loop

!-----compute clear-sky optical thickness, single scattering albedo,
!     and asymmetry factor. eqs.(6.2)-(6.4)
      VERTICAL_LOOP: do k=1,np

!dir$ vector aligned
   DO ic=1,irestrict
!   do concurrent ( ic = 1:min(CHUNK,irestrict) )
   if(lmask(ic) .eqv. .true.) then

           taurs=ry(ib)*dp(ic,k) 
           tauwv=xk(ik)*wh(ic,k)

           tausto(ic,k)=max(taurs+tauwv+taual(ic,k,iv), opt_min)
           ssatau(ic,k)=ssaal(ic,k,iv)*taual(ic,k,iv)+taurs
           asysto(ic,k)=asyal(ic,k,iv)*ssaal(ic,k,iv)*taual(ic,k,iv)

   endif
   enddo

           IF_CLEAR: if (overcast .and. fast_overcast ) then

!dir$ vector aligned
   DO ic=1,irestrict
!   do concurrent ( ic = 1:min(CHUNK,irestrict) )
   if(lmask(ic) .eqv. .true.) then

             rr(ic,k,1)=0.e0
             tt(ic,k,1)=0.e0
             td(ic,k,1)=0.e0
             rs(ic,k,1)=0.e0
             ts(ic,k,1)=0.e0

   endif
   enddo

           else

!dir$ vector aligned
   DO ic=1,irestrict
!   do concurrent ( ic = 1:min(CHUNK,irestrict) )
   if(lmask(ic) .eqv. .true.) then

!-----compute reflectance and transmittance of the clear portion of a layer
             tautob(ic,k)=tausto(ic,k)
             ssatob(ic,k)=min(max(ssatau(ic,k)/tautob(ic,k),ssa_min),ssa_max)
             asytob(ic,k)=min(max(asysto(ic,k)/(ssatob(ic,k)*tautob(ic,k)),asy_min),asy_max)

!     delta-eddington approximation for optical propeties
!-----for direct incident radiation

   endif
   enddo
!dir$ vector aligned
!dir$ SIMD
   DO ic=1,irestrict
   if(lmask(ic) .eqv. .true.) then

             call delta_eddington(tautob(ic,k), ssatob(ic,k), asytob(ic,k), cosz(ic) , &
                                  rrt(ic,k),ttt(ic,k), tdt(ic,k) )


!-----diffuse incident radiation is approximated by beam radiation with
!     an incident angle of 53 degrees, eqs. (6.5) and (6.6)

             call delta_eddington(tautob(ic,k), ssatob(ic,k), asytob(ic,k), dsm , &
                                  rst(ic,k),tst(ic,k), dum(ic,k) )

   endif
   enddo
!dir$ vector aligned
   DO ic=1,irestrict
!   do concurrent ( ic = 1:min(CHUNK,irestrict) )
   if(lmask(ic) .eqv. .true.) then

             rr(ic,k,1)=rrt(ic,k)
             tt(ic,k,1)=ttt(ic,k)
             td(ic,k,1)=tdt(ic,k)
             rs(ic,k,1)=rst(ic,k)
             ts(ic,k,1)=tst(ic,k)

   endif
   enddo

           endif IF_CLEAR

!dir$ vector aligned
   DO ic=1,irestrict
!   do concurrent ( ic = 1:min(CHUNK,irestrict) )
   if(lmask(ic) .eqv. .true.) then

!-----compute reflectance and transmittance of the cloudy portion of a layer

!-----for direct incident radiation. eqs.(6.2)-(6.4)
           tautob(ic,k)=tausto(ic,k)+max(tauclb(ic,k),0.e0)
           ssatob(ic,k)=min(max((ssatau(ic,k)+ssacl(ic,k,iv)*tauclb(ic,k))/tautob(ic,k),ssa_min),ssa_max)
           asytob(ic,k)=min(max((asysto(ic,k)+asycl(ic,k,iv)*ssacl(ic,k,iv)*tauclb(ic,k)) &
                      /(  max(ssatob(ic,k)*tautob(ic,k),const_tiny)  ),ssa_min),ssa_max)

!-----for diffuse incident radiation
           if(overcast) then
             tautof(ic,k)=tautob(ic,k)
             ssatof(ic,k)=ssatob(ic,k)
             asytof(ic,k)=asytob(ic,k)
           else
             tautof(ic,k)=tausto(ic,k)+max(tauclf(ic,k),0.e0)
             ssatof(ic,k)=min(max((ssatau(ic,k)+ssacl(ic,k,iv)*tauclf(ic,k))/tautof(ic,k),ssa_min),ssa_max) 
             asytof(ic,k)=min(max((asysto(ic,k)+asycl(ic,k,iv)*ssacl(ic,k,iv)*tauclf(ic,k)) &
                        /(  max(ssatof(ic,k)*tautof(ic,k),const_tiny)  ),asy_min),asy_max)
           endif

!     delta-eddington approximation for optical propeties
!-----for direct incident radiation

   endif
   enddo
!dir$ vector aligned
!dir$ SIMD
   DO ic=1,irestrict
   if(lmask(ic) .eqv. .true.) then

           call delta_eddington(tautob(ic,k), ssatob(ic,k), asytob(ic,k), cosz(ic) , &
                       rrt(ic,k),ttt(ic,k), tdt(ic,k) )

!-----diffuse incident radiation is approximated by beam radiation with
!     an incident angle of 53 degrees, eqs.(6.5) and (6.6)

          call delta_eddington(tautob(ic,k), ssatob(ic,k), asytob(ic,k), dsm , &
                       rst(ic,k),tst(ic,k), dum(ic,k) )

   endif
   enddo


!dir$ vector aligned
   DO ic=1,irestrict
!   do concurrent ( ic = 1:min(CHUNK,irestrict) )
   if(lmask(ic) .eqv. .true.) then

          rr(ic,k,2)=rrt(ic,k)
          tt(ic,k,2)=ttt(ic,k)
          td(ic,k,2)=tdt(ic,k)
          rs(ic,k,2)=rst(ic,k)
          ts(ic,k,2)=tst(ic,k)

      endif
      ENDDO
      enddo VERTICAL_LOOP


!-----flux calculations
      IF_OVERCAST: if (overcast) then  ! overcast  (LES or CRM)

         if( .not. fast_overcast ) then
!-----for clear-sky fluxes only
           ih1=1 ; im1=1 ; is1=1
           ih2=1 ; im2=1 ; is2=1
           call twostream_adding (np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                                  cc,rr,tt,td,rs,ts,fclr,dum1,falld,dum2,dum3,lmask,irestrict) 
         endif

!-----for cloudy-sky fluxes only
         ih1=2 ; im1=2 ; is1=2
         ih2=2 ; im2=2 ; is2=2
         call twostream_adding (np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                               cc,rr,tt,td,rs,ts,dum1,fall,falld,fsdir,fsdif,lmask,irestrict) 
      else  !GCM or RCM

!-----for clear- and all-sky fluxes
!     the all-sky flux, fall is the summation inside the brackets
!     of eq. (7.11)
         ih1=1 ; im1=1 ; is1=1
         ih2=2 ; im2=2 ; is2=2
         call twostream_adding (np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                      cc,rr,tt,td,rs,ts,fclr,fall,falld,fsdir,fsdif,lmask,irestrict) 
      endif IF_OVERCAST 


!-----flux integration following eq. (6.1)
      do k=1,np+1
!dir$ vector aligned
      DO ic=1,irestrict
      if(lmask(ic) .eqv. .true.) then
          flx(ic,k) = flx(ic,k)+fall(ic,k)*hk(ib,ik)
          flxd(ic,k) = flxd(ic,k)+falld(ic,k)*hk(ib,ik)
          if(overcast .and. fast_overcast) then 
            !do nothing flc(ic,k) = 0.e0
          else
            flc(ic,k) = flc(ic,k)+fclr(ic,k)*hk(ib,ik)
          endif
      endif
      ENDDO
      enddo
!-----compute downward surface fluxes in the ir region
!dir$ vector aligned
      DO ic=1,irestrict
      if(lmask(ic) .eqv. .true.) then
        fdirir(ic) = fdirir(ic)+fsdir(ic)*hk(ib,ik) !beam
        fdifir(ic) = fdifir(ic)+fsdif(ic)*hk(ib,ik) !diffuse
      endif
      ENDDO

   enddo KD_LOOP 

 enddo BAND_LOOP 

 end subroutine sw_ir

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine cloud_scale (np,cosz,fcld,taucld,ict,icb, &
                           cc,tauclb,tauclf,lmask,irestrict)
!********************************************************************
!
!   this subroutine computes the high, middle, and low cloud
!    amounts and scales the cloud optical thickness (section 7)
!
!   to simplify calculations in a cloudy atmosphere, clouds are
!    grouped into high, middle and low clouds separated by the levels
!    ict and icb (level 1 is the top of the model atmosphere).
!
!   within each of the three groups, clouds are assumed maximally
!    overlapped, and the cloud cover (cc) of a group is the maximum
!    cloud cover of all the layers in the group.  the optical thickness
!    (taucld) of a given layer is then scaled to new values (tauclb and
!    tauclf) so that the layer reflectance corresponding to the cloud
!    cover cc is the same as the original reflectance with optical
!    thickness taucld and cloud cover fcld.
!
!---input parameters
!
!    number of atmospheric layers (np)
!    cosine of the solar zenith angle (cosz)
!    fractional cloud cover (fcld)
!    cloud optical thickness (taucld)
!    index separating high and middle clouds (ict)
!    index separating middle and low clouds (icb)
!
!---output parameters
!
!    fractional cover of high, middle, and low cloud groups (cc)
!    scaled cloud optical thickness for direct  radiation (tauclb)
!    scaled cloud optical thickness for diffuse radiation (tauclf)
!
!********************************************************************
      implicit none
!-----input parameters
      logical lmask(CHUNK)
      integer irestrict
      integer np,ict(CHUNK),icb(CHUNK)
      real(fp_kind) cosz(CHUNK),fcld(CHUNK,np),taucld(CHUNK,np)
!-----output parameters
      real(fp_kind) cc(CHUNK,3),tauclb(CHUNK,np),tauclf(CHUNK,np)
!-----temporary variables
      integer i,j,k,im,it,ia,kk,ic
      real(fp_kind)  fm,ft,fa,xai,taux
!-----pre-computed table
!     size of cosz-interval:         dm
!     size of taucld-interval:       dt
!     size of cloud amount-interval: da
      integer   nm,nt,na
      parameter (nm=11,nt=9,na=11)
      real(fp_kind)  dm,dt,da,t1,caib(nm,nt,na),caif(nt,na)
      parameter (dm=0.1,dt=0.30103,da=0.1,t1=-0.9031)
!-----include the pre-computed table of mcai for scaling the cloud optical
!     thickness under the assumption that clouds are maximally overlapped
!
!     caib is for scaling the cloud optical thickness for direct radiation
!     caif is for scaling the cloud optical thickness for diffuse radiation
!     include "mcai.data"
      data ((caib(1,i,j),j=1,11),i=1,9)/ &
       .000,0.068,0.140,0.216,0.298,0.385,0.481,0.586,0.705,0.840,1.000, &
       .000,0.052,0.106,0.166,0.230,0.302,0.383,0.478,0.595,0.752,1.000, &
       .000,0.038,0.078,0.120,0.166,0.218,0.276,0.346,0.438,0.582,1.000, &
       .000,0.030,0.060,0.092,0.126,0.164,0.206,0.255,0.322,0.442,1.000, &
       .000,0.025,0.051,0.078,0.106,0.136,0.170,0.209,0.266,0.462,1.000, &
       .000,0.023,0.046,0.070,0.095,0.122,0.150,0.187,0.278,0.577,1.000, &
       .000,0.022,0.043,0.066,0.089,0.114,0.141,0.187,0.354,0.603,1.000, &
       .000,0.021,0.042,0.063,0.086,0.108,0.135,0.214,0.349,0.565,1.000, &
       .000,0.021,0.041,0.062,0.083,0.105,0.134,0.202,0.302,0.479,1.000/
      data ((caib(2,i,j),j=1,11),i=1,9)/ &
       .000,0.088,0.179,0.272,0.367,0.465,0.566,0.669,0.776,0.886,1.000, &
       .000,0.079,0.161,0.247,0.337,0.431,0.531,0.637,0.749,0.870,1.000, &
       .000,0.065,0.134,0.207,0.286,0.372,0.466,0.572,0.692,0.831,1.000, &
       .000,0.049,0.102,0.158,0.221,0.290,0.370,0.465,0.583,0.745,1.000, &
       .000,0.037,0.076,0.118,0.165,0.217,0.278,0.354,0.459,0.638,1.000, &
       .000,0.030,0.061,0.094,0.130,0.171,0.221,0.286,0.398,0.631,1.000, &
       .000,0.026,0.052,0.081,0.111,0.146,0.189,0.259,0.407,0.643,1.000, &
       .000,0.023,0.047,0.072,0.098,0.129,0.170,0.250,0.387,0.598,1.000, &
       .000,0.022,0.044,0.066,0.090,0.118,0.156,0.224,0.328,0.508,1.000/
      data ((caib(3,i,j),j=1,11),i=1,9)/ &
       .000,0.094,0.189,0.285,0.383,0.482,0.582,0.685,0.788,0.894,1.000, &
       .000,0.088,0.178,0.271,0.366,0.465,0.565,0.669,0.776,0.886,1.000, &
       .000,0.079,0.161,0.247,0.337,0.431,0.531,0.637,0.750,0.870,1.000, &
       .000,0.066,0.134,0.209,0.289,0.375,0.470,0.577,0.697,0.835,1.000, &
       .000,0.050,0.104,0.163,0.227,0.300,0.383,0.483,0.606,0.770,1.000, &
       .000,0.038,0.080,0.125,0.175,0.233,0.302,0.391,0.518,0.710,1.000, &
       .000,0.031,0.064,0.100,0.141,0.188,0.249,0.336,0.476,0.689,1.000, &
       .000,0.026,0.054,0.084,0.118,0.158,0.213,0.298,0.433,0.638,1.000, &
       .000,0.023,0.048,0.074,0.102,0.136,0.182,0.254,0.360,0.542,1.000/
      data ((caib(4,i,j),j=1,11),i=1,9)/ &
       .000,0.096,0.193,0.290,0.389,0.488,0.589,0.690,0.792,0.896,1.000, &
       .000,0.092,0.186,0.281,0.378,0.477,0.578,0.680,0.785,0.891,1.000, &
       .000,0.086,0.174,0.264,0.358,0.455,0.556,0.660,0.769,0.882,1.000, &
       .000,0.074,0.153,0.235,0.323,0.416,0.514,0.622,0.737,0.862,1.000, &
       .000,0.061,0.126,0.195,0.271,0.355,0.449,0.555,0.678,0.823,1.000, &
       .000,0.047,0.098,0.153,0.215,0.286,0.370,0.471,0.600,0.770,1.000, &
       .000,0.037,0.077,0.120,0.170,0.230,0.303,0.401,0.537,0.729,1.000, &
       .000,0.030,0.062,0.098,0.138,0.187,0.252,0.343,0.476,0.673,1.000, &
       .000,0.026,0.053,0.082,0.114,0.154,0.207,0.282,0.391,0.574,1.000/
      data ((caib(5,i,j),j=1,11),i=1,9)/ &
       .000,0.097,0.194,0.293,0.392,0.492,0.592,0.693,0.794,0.897,1.000, &
       .000,0.094,0.190,0.286,0.384,0.483,0.584,0.686,0.789,0.894,1.000, &
       .000,0.090,0.181,0.274,0.370,0.468,0.569,0.672,0.778,0.887,1.000, &
       .000,0.081,0.165,0.252,0.343,0.439,0.539,0.645,0.757,0.874,1.000, &
       .000,0.069,0.142,0.218,0.302,0.392,0.490,0.598,0.717,0.850,1.000, &
       .000,0.054,0.114,0.178,0.250,0.330,0.422,0.529,0.656,0.810,1.000, &
       .000,0.042,0.090,0.141,0.200,0.269,0.351,0.455,0.589,0.764,1.000, &
       .000,0.034,0.070,0.112,0.159,0.217,0.289,0.384,0.515,0.703,1.000, &
       .000,0.028,0.058,0.090,0.128,0.174,0.231,0.309,0.420,0.602,1.000/
      data ((caib(6,i,j),j=1,11),i=1,9)/ &
       .000,0.098,0.196,0.295,0.394,0.494,0.594,0.695,0.796,0.898,1.000, &
       .000,0.096,0.193,0.290,0.389,0.488,0.588,0.690,0.792,0.895,1.000, &
       .000,0.092,0.186,0.281,0.378,0.477,0.577,0.680,0.784,0.891,1.000, &
       .000,0.086,0.174,0.264,0.358,0.455,0.556,0.661,0.769,0.882,1.000, &
       .000,0.075,0.154,0.237,0.325,0.419,0.518,0.626,0.741,0.865,1.000, &
       .000,0.062,0.129,0.201,0.279,0.366,0.462,0.571,0.694,0.836,1.000, &
       .000,0.049,0.102,0.162,0.229,0.305,0.394,0.501,0.631,0.793,1.000, &
       .000,0.038,0.080,0.127,0.182,0.245,0.323,0.422,0.550,0.730,1.000, &
       .000,0.030,0.064,0.100,0.142,0.192,0.254,0.334,0.448,0.627,1.000/
      data ((caib(7,i,j),j=1,11),i=1,9)/ &
       .000,0.098,0.198,0.296,0.396,0.496,0.596,0.696,0.797,0.898,1.000, &
       .000,0.097,0.194,0.293,0.392,0.491,0.591,0.693,0.794,0.897,1.000, &
       .000,0.094,0.190,0.286,0.384,0.483,0.583,0.686,0.789,0.894,1.000, &
       .000,0.089,0.180,0.274,0.369,0.467,0.568,0.672,0.778,0.887,1.000, &
       .000,0.081,0.165,0.252,0.344,0.440,0.541,0.646,0.758,0.875,1.000, &
       .000,0.069,0.142,0.221,0.306,0.397,0.496,0.604,0.722,0.854,1.000, &
       .000,0.056,0.116,0.182,0.256,0.338,0.432,0.540,0.666,0.816,1.000, &
       .000,0.043,0.090,0.143,0.203,0.273,0.355,0.455,0.583,0.754,1.000, &
       .000,0.034,0.070,0.111,0.157,0.210,0.276,0.359,0.474,0.650,1.000/
      data ((caib(8,i,j),j=1,11),i=1,9)/ &
       .000,0.099,0.198,0.298,0.398,0.497,0.598,0.698,0.798,0.899,1.000, &
       .000,0.098,0.196,0.295,0.394,0.494,0.594,0.695,0.796,0.898,1.000, &
       .000,0.096,0.193,0.290,0.390,0.489,0.589,0.690,0.793,0.896,1.000, &
       .000,0.093,0.186,0.282,0.379,0.478,0.578,0.681,0.786,0.892,1.000, &
       .000,0.086,0.175,0.266,0.361,0.458,0.558,0.663,0.771,0.883,1.000, &
       .000,0.076,0.156,0.240,0.330,0.423,0.523,0.630,0.744,0.867,1.000, &
       .000,0.063,0.130,0.203,0.282,0.369,0.465,0.572,0.694,0.834,1.000, &
       .000,0.049,0.102,0.161,0.226,0.299,0.385,0.486,0.611,0.774,1.000, &
       .000,0.038,0.078,0.122,0.172,0.229,0.297,0.382,0.498,0.672,1.000/
      data ((caib(9,i,j),j=1,11),i=1,9)/ &
       .000,0.099,0.199,0.298,0.398,0.498,0.598,0.699,0.799,0.899,1.000, &
       .000,0.099,0.198,0.298,0.398,0.497,0.598,0.698,0.798,0.899,1.000, &
       .000,0.098,0.196,0.295,0.394,0.494,0.594,0.695,0.796,0.898,1.000, &
       .000,0.096,0.193,0.290,0.389,0.488,0.588,0.690,0.792,0.895,1.000, &
       .000,0.092,0.185,0.280,0.376,0.474,0.575,0.678,0.782,0.890,1.000, &
       .000,0.084,0.170,0.259,0.351,0.447,0.547,0.652,0.762,0.878,1.000, &
       .000,0.071,0.146,0.224,0.308,0.398,0.494,0.601,0.718,0.850,1.000, &
       .000,0.056,0.114,0.178,0.248,0.325,0.412,0.514,0.638,0.793,1.000, &
       .000,0.042,0.086,0.134,0.186,0.246,0.318,0.405,0.521,0.691,1.000/
      data ((caib(10,i,j),j=1,11),i=1,9)/ &
       .000,0.100,0.200,0.300,0.400,0.500,0.600,0.700,0.800,0.900,1.000, &
       .000,0.100,0.200,0.300,0.400,0.500,0.600,0.700,0.800,0.900,1.000, &
       .000,0.100,0.200,0.300,0.400,0.500,0.600,0.700,0.800,0.900,1.000, &
       .000,0.100,0.199,0.298,0.398,0.498,0.598,0.698,0.798,0.899,1.000, &
       .000,0.098,0.196,0.294,0.392,0.491,0.590,0.691,0.793,0.896,1.000, &
       .000,0.092,0.185,0.278,0.374,0.470,0.570,0.671,0.777,0.886,1.000, &
       .000,0.081,0.162,0.246,0.333,0.424,0.521,0.625,0.738,0.862,1.000, &
       .000,0.063,0.128,0.196,0.270,0.349,0.438,0.540,0.661,0.809,1.000, &
       .000,0.046,0.094,0.146,0.202,0.264,0.337,0.426,0.542,0.710,1.000/
      data ((caib(11,i,j),j=1,11),i=1,9)/ &
       .000,0.101,0.202,0.302,0.402,0.502,0.602,0.702,0.802,0.901,1.000, &
       .000,0.102,0.202,0.303,0.404,0.504,0.604,0.703,0.802,0.902,1.000, &
       .000,0.102,0.205,0.306,0.406,0.506,0.606,0.706,0.804,0.902,1.000, &
       .000,0.104,0.207,0.309,0.410,0.510,0.609,0.707,0.805,0.902,1.000, &
       .000,0.106,0.208,0.309,0.409,0.508,0.606,0.705,0.803,0.902,1.000, &
       .000,0.102,0.202,0.298,0.395,0.493,0.590,0.690,0.790,0.894,1.000, &
       .000,0.091,0.179,0.267,0.357,0.449,0.545,0.647,0.755,0.872,1.000, &
       .000,0.073,0.142,0.214,0.290,0.372,0.462,0.563,0.681,0.822,1.000, &
       .000,0.053,0.104,0.158,0.217,0.281,0.356,0.446,0.562,0.726,1.000/
      data ((caif(i,j),j=1,11),i=1,9)/ &
       .000,0.099,0.198,0.297,0.397,0.496,0.597,0.697,0.798,0.899,1.000, &
       .000,0.098,0.196,0.294,0.394,0.494,0.594,0.694,0.796,0.898,1.000, &
       .000,0.096,0.192,0.290,0.388,0.487,0.587,0.689,0.792,0.895,1.000, &
       .000,0.092,0.185,0.280,0.376,0.476,0.576,0.678,0.783,0.890,1.000, &
       .000,0.085,0.173,0.263,0.357,0.454,0.555,0.659,0.768,0.881,1.000, &
       .000,0.076,0.154,0.237,0.324,0.418,0.517,0.624,0.738,0.864,1.000, &
       .000,0.063,0.131,0.203,0.281,0.366,0.461,0.567,0.688,0.830,1.000, &
       .000,0.052,0.107,0.166,0.232,0.305,0.389,0.488,0.610,0.770,1.000, &
       .000,0.043,0.088,0.136,0.189,0.248,0.317,0.400,0.510,0.675,1.000/
!-----clouds within each of the high, middle, and low clouds are assumed
!     to be maximally overlapped, and the cloud cover (cc) for a group
!     (high, middle, or low) is the maximum cloud cover of all the layers
!     within a group 

!dir$ vector aligned
       DO ic=1,irestrict
       if(lmask(ic) .eqv. .true.) then
         cc(ic,1)=0.0
         cc(ic,2)=0.0
         cc(ic,3)=0.0
       endif
       ENDDO
!dir$ vector aligned
       DO ic=1,irestrict
       do k=1,ict(ic)-1
       if(lmask(ic) .eqv. .true.) then
          cc(ic,1)=max(cc(ic,1),fcld(ic,k))
       endif
       enddo
       ENDDO
!dir$ vector aligned
       DO ic=1,irestrict
       do k=ict(ic),icb(ic)-1
       if(lmask(ic) .eqv. .true.) then
          cc(ic,2)=max(cc(ic,2),fcld(ic,k))
       endif
       enddo
       ENDDO
!dir$ vector aligned
       DO ic=1,irestrict
       do k=icb(ic),np
       if(lmask(ic) .eqv. .true.) then
          cc(ic,3)=max(cc(ic,3),fcld(ic,k))
       endif
       enddo
       ENDDO
!-----scale the cloud optical thickness.
       do k=1,np
!dir$ vector aligned
       DO ic=1,irestrict
       if(lmask(ic) .eqv. .true.) then
         if(k.lt.ict(ic)) then
            kk=1
         elseif(k.ge.ict(ic) .and. k.lt.icb(ic)) then
            kk=2
         else
            kk=3
         endif
         tauclb(ic,k) = 0.0
         tauclf(ic,k) = 0.0
         taux= taucld(ic,k) 
         if (taux.gt.taux_min .and. fcld(ic,k).gt.fcld_min) then
!-----normalize cloud cover following eq. (7.8)
           fa=fcld(ic,k)/cc(ic,kk)
!-----table look-up
           taux=min(taux,32.)
           fm=cosz(ic)/dm
           ft=(log10(taux)-t1)/dt
           fa=fa/da
           im=int(fm+1.5)
           it=int(ft+1.5)
           ia=int(fa+1.5)
           im=max(im,2)
           it=max(it,2)
           ia=max(ia,2)
           im=min(im,nm-1)
           it=min(it,nt-1)
           ia=min(ia,na-1)
           fm=fm-float(im-1)
           ft=ft-float(it-1)
           fa=fa-float(ia-1)
!-----scale cloud optical thickness for beam radiation following eq. (7.3)
!     the scaling factor, xai, is a function of the solar zenith
!     angle, optical thickness, and cloud cover.
           xai=    (-caib(im-1,it,ia)*(1.-fm)+ &
            caib(im+1,it,ia)*(1.+fm))*fm*.5+caib(im,it,ia)*(1.-fm*fm)
           xai=xai+(-caib(im,it-1,ia)*(1.-ft)+ &
            caib(im,it+1,ia)*(1.+ft))*ft*.5+caib(im,it,ia)*(1.-ft*ft)
           xai=xai+(-caib(im,it,ia-1)*(1.-fa)+ &
           caib(im,it,ia+1)*(1.+fa))*fa*.5+caib(im,it,ia)*(1.-fa*fa)
           xai= xai-2.*caib(im,it,ia)
           xai=max(xai,0.0)
           xai=min(xai,1.0)
           tauclb(ic,k) = taux*xai
!-----scale cloud optical thickness for diffuse radiation following eq. (7.4)
!     the scaling factor, xai, is a function of the cloud optical
!     thickness and cover but not the solar zenith angle.
           xai=    (-caif(it-1,ia)*(1.-ft)+ &
            caif(it+1,ia)*(1.+ft))*ft*.5+caif(it,ia)*(1.-ft*ft)
           xai=xai+(-caif(it,ia-1)*(1.-fa)+ &
            caif(it,ia+1)*(1.+fa))*fa*.5+caif(it,ia)*(1.-fa*fa)
           xai= xai-caif(it,ia)
           xai=max(xai,0.0)
           xai=min(xai,1.0)
           tauclf(ic,k) = taux*xai
         endif
       endif
       ENDDO
       enddo

      end subroutine cloud_scale

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

subroutine delta_eddington(tau,ssc,g0,cza,rr,tt,td)

!*********************************************************************
!
!-----uses the delta-eddington approximation to compute the
!     bulk scattering properties of a single layer
!     coded following king and harshvardhan (jas, 1986)
!
!  inputs:
!
!     tau: the effective optical thickness
!     ssc: the effective single scattering albedo
!     g0:  the effective asymmetry factor
!     cza: cosine of solar zenith angle
!
!  outputs:
!
!     rr: the layer reflection of the direct beam
!     tt: the layer diffuse transmission of the direct beam
!     td: the layer direct transmission of the direct beam
!
!*********************************************************************
      implicit none
!*********************************************************************

      real(fp_kind) zero,one,two,three,four,fourth,seven,thresh
      parameter (one =1., three=3.)
      parameter (two =2., seven=7.)
      parameter (four=4., fourth=.25)
      parameter (zero=0., thresh=1.e-8)

!-----input parameters
      real(fp_kind), intent(in) :: tau,ssc,g0,cza

!-----output parameters
      real(fp_kind), intent(out) :: rr,tt,td

!-----temporary parameters

      real(fp_kind) zth,ff,xx,taup,sscp,gp,gm1,gm2,gm3,akk,alf1,alf2, &
           all,bll,st7,st8,cll,dll,fll,ell,st1,st2,st3,st4
       real(fp_kind) taupdzth,akkdtaup

!---------------------------------------------------------------------

                zth = cza 

!  delta-eddington scaling of single scattering albedo,
!  optical thickness, and asymmetry factor,
!  k & h eqs(27-29)

                ff  = g0*g0
                xx  = one-ff*ssc
                taup= tau*xx
                sscp= ssc*(one-ff)/xx
                gp  = g0/(one+g0)

!  gamma1, gamma2, and gamma3. see table 2 and eq(26) k & h
!  ssc and gp are the d-s single scattering
!  albedo and asymmetry factor.

                xx  =  three*gp
                gm1 =  (seven - sscp*(four+xx))*fourth
                gm2 = -(one   - sscp*(four-xx))*fourth

!  akk is k as defined in eq(25) of k & h

                akk = sqrt((gm1+gm2)*(gm1-gm2))

                xx  = akk * zth
           if (abs((one-xx)*(one+xx)) .lt. thresh) then
               zth = zth + 0.001
               xx  = akk * zth
           endif
                st7 = one - xx
                st8 = one + xx
                st3 = st7 * st8
!                if (abs(st3) .lt. thresh) then
!                    zth = zth + 0.001
!                    xx  = akk * zth
!                    st7 = one - xx
!                    st8 = one + xx
!                    st3 = st7 * st8
!                endif

!  extinction of the direct beam transmission
                td=0.
                taupdzth=taup/zth
                if (taupdzth .lt. 40. ) td  = exp(-taup/zth)
!  alf1 and alf2 are alpha1 and alpha2 from eqs (23) & (24) of k & h

                gm3  = (two - zth*three*gp)*fourth
                xx   = gm1 - gm2
                alf1 = gm1 - gm3 * xx
                alf2 = gm2 + gm3 * xx

!  all is last term in eq(21) of k & h
!  bll is last term in eq(22) of k & h

                xx  = akk * two
                all = (gm3 - alf2 * zth    )*xx*td
                bll = (one - gm3 + alf1*zth)*xx

                xx  = akk * gm3
                cll = (alf2 + xx) * st7
                dll = (alf2 - xx) * st8

                xx  = akk * (one-gm3)
                fll = (alf1 + xx) * st8
                ell = (alf1 - xx) * st7

                st2=0.
                akkdtaup=akk*taup
                if (akkdtaup.lt.40.) st2 = exp(-akkdtaup)
                st4 = st2 * st2

                st1 =  sscp / ((akk+gm1 + (akk-gm1)*st4) * st3)

!  rr is r-hat of eq(21) of k & h
!  tt is diffuse part of t-hat of eq(22) of k & h

                rr =   ( cll-dll*st4    -all*st2)*st1
                tt = - ((fll-ell*st4)*td-bll*st2)*st1

                rr = max(rr,zero)
                tt = max(tt,zero)
                tt = tt+td
      end subroutine delta_eddington

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 


      subroutine twostream_adding (np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                 cc,rr,tt,td,rs,ts,fclr,fall,falld,fsdir,fsdif,lmask,irestrict)

!*******************************************************************
!  compute upward and downward fluxes using a two-stream adding method
!  following equations (6.9)-(6.16).
!
!  clouds are grouped into high, middle, and low clouds which are assumed
!  randomly overlapped. it involves a maximum of 8 sets of calculations.
!  in each set of calculations, each atmospheric layer is homogeneous,
!  either totally filled with clouds or without clouds.
!  input parameters:
!
!   np:  number of atmospheric layers
!   ict: the level separating high and middle clouds
!   icb: the level separating middle and low clouds
!   ih1,ih2,im1,im2,is1,is2: indices for three group of clouds
!   cc:  effective cloud covers for high, middle and low clouds
!   rr:  reflection of a layer illuminated by beam radiation
!   tt:  total (direct+diffuse) transmission of a layer illuminated
!        by beam radiation
!   td:  direct beam transmission
!   rs:  reflection of a layer illuminated by diffuse radiation
!   ts:  transmission of a layer illuminated by diffuse radiation
!
!  output parameters:
!
!     fclr:  clear-sky flux divergence (downward minus upward)
!     fall:  all-sky flux divergence (downward minus upward)
!     fsdir: surface direct downward flux
!     fsdif: surface diffuse downward flux
!
!*********************************************************************c
!ccshie 8/19/04
     implicit none
!-----input parameters
      logical lmask(CHUNK)
      integer irestrict
      integer np,ict(CHUNK),icb(CHUNK),ih1,ih2,im1,im2,is1,is2
      real(fp_kind) rr(CHUNK,np+1,2),tt(CHUNK,np+1,2),td(CHUNK,np+1,2)
      real(fp_kind) rs(CHUNK,np+1,2),ts(CHUNK,np+1,2)
      real(fp_kind) cc(CHUNK,3)
!-----temporary array
      integer k,ih,im,is
      real(fp_kind) denm,xx,yy
      real(fp_kind) fupdif
      real(fp_kind) :: rra(CHUNK,np+1,2,2)
      real(fp_kind) :: tta(CHUNK,np+1,2,2)
      real(fp_kind) :: tda(CHUNK,np+1,2,2)
      real(fp_kind) :: rsa(CHUNK,np+1,2,2)
      real(fp_kind) :: rxa(CHUNK,np+1,2,2)
      real(fp_kind) :: ch(CHUNK)
      real(fp_kind) :: cm(CHUNK)
      real(fp_kind) :: ct(CHUNK)
      real(fp_kind) :: flxdn(CHUNK,np+1)
      real(fp_kind) :: fdndir(CHUNK)
      real(fp_kind) :: fdndif(CHUNK)
      real(fp_kind) flxdnu(CHUNK,np+1),flxdnd(CHUNK,np+1)

!-----output parameters
      real(fp_kind) fclr(CHUNK,np+1),fall(CHUNK,np+1)
      real(fp_kind) falld(CHUNK,np+1)
      real(fp_kind) fsdir(CHUNK),fsdif(CHUNK)

      integer :: ic

!-----initialize all-sky flux (fall) and surface downward fluxes
      do k=1,np+1
!dir$ vector aligned
      DO ic=1,irestrict
      if(lmask(ic) .eqv. .true.) then
           fclr(ic,k)=0.0
           fall(ic,k)=0.0
           falld(ic,k)=0.0
      endif
      ENDDO
      enddo

      fsdir=0.0
      fsdif=0.0

!-----compute transmittances and reflectances for a composite of
!     layers. layers are added one at a time, going down from the top.
!     tda is the composite direct transmittance illuminated by beam radiation
!     tta is the composite total transmittance illuminated by
!         beam radiation
!     rsa is the composite reflectance illuminated from below
!         by diffuse radiation
!     tta and rsa are computed from eqs. (6.10) and (6.12)
!-----for high clouds
!     ih=1 for clear-sky condition, ih=2 for cloudy-sky condition

      do ih=ih1,ih2
!dir$ vector aligned
      DO ic=1,irestrict
      if(lmask(ic) .eqv. .true.) then
          tda(ic,1,ih,1)=td(ic,1,ih)
          tta(ic,1,ih,1)=tt(ic,1,ih)
          rsa(ic,1,ih,1)=rs(ic,1,ih)
          tda(ic,1,ih,2)=td(ic,1,ih)
          tta(ic,1,ih,2)=tt(ic,1,ih)
          rsa(ic,1,ih,2)=rs(ic,1,ih)
      endif
      ENDDO

!         do k=2,ict-1
         do k=2,np
         DO ic=1,irestrict
         if(k .le. ict(ic)-1 .and. lmask(ic) .eqv. .true.) then

          denm = ts(ic,k,ih)/( 1.-rsa(ic,k-1,ih,1)*rs(ic,k,ih))
          tda(ic,k,ih,1)= tda(ic,k-1,ih,1)*td(ic,k,ih)
          tta(ic,k,ih,1)= tda(ic,k-1,ih,1)*tt(ic,k,ih) &
                +(tda(ic,k-1,ih,1)*rsa(ic,k-1,ih,1)*rr(ic,k,ih) &
                +tta(ic,k-1,ih,1)-tda(ic,k-1,ih,1))*denm    !additional -tda(ic,k-1,ih,1)
          rsa(ic,k,ih,1)= rs(ic,k,ih)+ts(ic,k,ih) &
                        *rsa(ic,k-1,ih,1)*denm
          if(tda(ic,k,ih,1).lt.1.e-10) tda(ic,k,ih,1)=0. !!
          if(tta(ic,k,ih,1).lt.1.e-10) tta(ic,k,ih,1)=0. !!
          tda(ic,k,ih,2)= tda(ic,k,ih,1)
          tta(ic,k,ih,2)= tta(ic,k,ih,1)
          rsa(ic,k,ih,2)= rsa(ic,k,ih,1)
         endif
         ENDDO
         enddo

!-----for middle clouds
!     im=1 for clear-sky condition, im=2 for cloudy-sky condition

      do im=im1,im2
!        do k=ict,icb-1
        do k=1,np
!dir$ vector aligned
         DO ic=1,irestrict
         if(k .ge. ict(ic) .and. k .le. icb(ic)-1 .and. lmask(ic) .eqv. .true.) then
          denm = ts(ic,k,im)/( 1.-rsa(ic,k-1,ih,im)*rs(ic,k,im))
          tda(ic,k,ih,im)= tda(ic,k-1,ih,im)*td(ic,k,im)
          tta(ic,k,ih,im)= tda(ic,k-1,ih,im)*tt(ic,k,im) &
               +(tda(ic,k-1,ih,im)*rsa(ic,k-1,ih,im)*rr(ic,k,im) &
               +tta(ic,k-1,ih,im)-tda(ic,k-1,ih,im))*denm   !additional -tda(k-1,ih,im)
          rsa(ic,k,ih,im)= rs(ic,k,im)+ts(ic,k,im) &
                        *rsa(ic,k-1,ih,im)*denm
          if(tda(ic,k,ih,im).lt.1.e-10) tda(ic,k,ih,im)=0. !!
          if(tta(ic,k,ih,im).lt.1.e-10) tta(ic,k,ih,im)=0. !!
         endif
         ENDDO
        enddo
      enddo                 ! end im loop

      enddo                 ! end ih loop

!-----layers are added one at a time, going up from the surface.
!     rra is the composite reflectance illuminated by beam radiation
!     rxa is the composite reflectance illuminated from above
!         by diffuse radiation
!     rra and rxa are computed from eqs. (6.9) and (6.11)
!-----for the low clouds
!     is=1 for clear-sky condition, is=2 for cloudy-sky condition

      do is=is1,is2
!dir$ vector aligned
         DO ic=1,irestrict
         if(lmask(ic) .eqv. .true.) then
           rra(ic,np+1,1,is)=rr(ic,np+1,is)
           rxa(ic,np+1,1,is)=rs(ic,np+1,is)
           rra(ic,np+1,2,is)=rr(ic,np+1,is)
           rxa(ic,np+1,2,is)=rs(ic,np+1,is)
         endif
         ENDDO

!         do k=np,icb,-1
         do k=np,1,-1
!dir$ vector aligned
         DO ic=1,irestrict
         if(k .ge. icb(ic)  .and.  lmask(ic) .eqv. .true.) then
          denm=ts(ic,k,is)/( 1.-rs(ic,k,is)*rxa(ic,k+1,1,is) )
          rra(ic,k,1,is)=rr(ic,k,is)+(td(ic,k,is)*rra(ic,k+1,1,is) &
              +(tt(ic,k,is)-td(ic,k,is))*rxa(ic,k+1,1,is))*denm  !additional -td(k,is)
          rxa(ic,k,1,is)= rs(ic,k,is)+ts(ic,k,is) &
              *rxa(ic,k+1,1,is)*denm
          rra(ic,k,2,is)=rra(ic,k,1,is)
          rxa(ic,k,2,is)=rxa(ic,k,1,is)
        endif
        ENDDO
        enddo

!-----for middle clouds
      do im=im1,im2
!        do k=icb-1,ict,-1
        do k=np,1,-1
!dir$ vector aligned
        DO ic=1,irestrict
        if(k .ge. ict(ic)-1 .and. k .le. icb(ic)-1 .and. lmask(ic) .eqv. .true.) then
          denm=ts(ic,k,im)/( 1.-rs(ic,k,im)*rxa(ic,k+1,im,is) )
          rra(ic,k,im,is)= rr(ic,k,im)+(td(ic,k,im)*rra(ic,k+1,im,is) &
              +(tt(ic,k,im)-td(ic,k,im))*rxa(ic,k+1,im,is))*denm   !additiona -td(k,im)
          rxa(ic,k,im,is)= rs(ic,k,im)+ts(ic,k,im) &
              *rxa(ic,k+1,im,is)*denm
        endif
        ENDDO
        enddo
      enddo                 ! end im loop

      enddo                 ! end is loop

!-----integration over eight sky situations.
!     ih, im, is denotes high, middle and low cloud groups.

      do ih=ih1,ih2

!-----clear portion
!dir$ vector aligned
        DO ic=1,irestrict
        if(lmask(ic) .eqv. .true.) then
          if(ih.eq.1) then
             ch(ic)=1.0-cc(ic,1)
          else
!-----cloudy portion
             ch(ic)=cc(ic,1)
          endif
        endif
        ENDDO

      do im=im1,im2

!-----clear portion
!dir$ vector aligned
        DO ic=1,irestrict
        if(lmask(ic) .eqv. .true.) then
         if(im.eq.1) then
              cm(ic)=ch(ic)*(1.0-cc(ic,2))
         else
!-----cloudy portion
              cm(ic)=ch(ic)*cc(ic,2)
         endif
        endif
        ENDDO

      do is=is1,is2
!dir$ vector aligned
        DO ic=1,irestrict
        if(lmask(ic) .eqv. .true.) then
!-----clear portion
         if(is.eq.1) then
             ct(ic)=cm(ic)*(1.0-cc(ic,3))
         else
!-----cloudy portion
             ct(ic)=cm(ic)*cc(ic,3)
         endif
        endif
        ENDDO

!-----add one layer at a time, going down.
!        do k=icb,np
        do k=1,np
!dir$ vector aligned
        DO ic=1,irestrict
        if(k .ge. icb(ic) .and. lmask(ic) .eqv. .true.) then
          denm = ts(ic,k,is)/( 1.-rsa(ic,k-1,ih,im)*rs(ic,k,is) )
          tda(ic,k,ih,im)= tda(ic,k-1,ih,im)*td(ic,k,is)
          tta(ic,k,ih,im)=  tda(ic,k-1,ih,im)*tt(ic,k,is) &
               +(tda(ic,k-1,ih,im)*rr(ic,k,is) &
               *rsa(ic,k-1,ih,im)+tta(ic,k-1,ih,im)-tda(ic,k-1,ih,im))*denm   !additional -tda(k-1,ih,im)
          rsa(ic,k,ih,im)= rs(ic,k,is)+ts(ic,k,is) &
               *rsa(ic,k-1,ih,im)*denm
          if(tda(ic,k,ih,im).lt.1.e-10) tda(ic,k,ih,im)=0.  !!
          if(tta(ic,k,ih,im).lt.1.e-10) tta(ic,k,ih,im)=0.  !!
        endif
        ENDDO
        enddo
!-----add one layer at a time, going up.
!        do k=ict-1,1,-1
        do k=np,1,-1
!dir$ vector aligned
        DO ic=1,irestrict
        if(k .le. ict(ic)-1 .and. lmask(ic) .eqv. .true.) then
          denm =ts(ic,k,ih)/(1.-rs(ic,k,ih)*rxa(ic,k+1,im,is))
          rra(ic,k,im,is)= rr(ic,k,ih)+(td(ic,k,ih)*rra(ic,k+1,im,is) &
              +(tt(ic,k,ih)-td(ic,k,ih))*rxa(ic,k+1,im,is))*denm   !addittional -td(k,ih)
          rxa(ic,k,im,is)= rs(ic,k,ih)+ts(ic,k,ih) &
              *rxa(ic,k+1,im,is)*denm
        endif
        ENDDO
        enddo
!-----compute fluxes following eq. (6.15) for fupdif and
!     eq. (6.16) for (fdndir+fdndif)
!     fdndir is the direct  downward flux
!     fdndif is the diffuse downward flux
!     fupdif is the diffuse upward flux
      do k=2,np+1
!dir$ vector aligned
      DO ic=1,irestrict
      if(lmask(ic) .eqv. .true.) then
         denm= 1./(1.-rsa(ic,k-1,ih,im)*rxa(ic,k,im,is))
         fdndir(ic)= tda(ic,k-1,ih,im)
         xx= tda(ic,k-1,ih,im)*rra(ic,k,im,is)
         yy= tta(ic,k-1,ih,im)-tda(ic,k-1,ih,im)    !additional -tda(k-1,ih,im)
         fdndif(ic)= (xx*rsa(ic,k-1,ih,im)+yy)*denm
         fupdif= (xx+yy*rxa(ic,k,im,is))*denm
         flxdn(ic,k)= fdndir(ic)+fdndif(ic)-fupdif
         flxdnu(ic,k)=-fupdif
         flxdnd(ic,k)=fdndir(ic)+fdndif(ic)
      endif
      ENDDO
      enddo
!dir$ vector aligned
      DO ic=1,irestrict
      if(lmask(ic) .eqv. .true.) then
         flxdn(ic,1)=1.0-rra(ic,1,im,is)
         flxdnu(ic,1)=-rra(ic,1,im,is)
         flxdnd(ic,1)=1.0
      endif
      ENDDO
!-----summation of fluxes over all sky situations;
!     the term in the brackets of eq. (7.11)
      do k=1,np+1
!dir$ vector aligned
      DO ic=1,irestrict
      if(lmask(ic) .eqv. .true.) then
           if(ih.eq.1 .and. im.eq.1 .and. is.eq.1) then
             fclr(ic,k)=flxdn(ic,k)
           endif
             fall(ic,k)=fall(ic,k)+flxdn(ic,k)*ct(ic)
             falld(ic,k)=falld(ic,k)+flxdnd(ic,k)*ct(ic)
      endif
      ENDDO
      enddo
!dir$ vector aligned
      DO ic=1,irestrict
      if(lmask(ic) .eqv. .true.) then
            fsdir(ic)=fsdir(ic)+fdndir(ic)*ct(ic)
            fsdif(ic)=fsdif(ic)+fdndif(ic)*ct(ic)
      endif
      ENDDO

       enddo                 ! end is loop

     enddo                 ! end im loop

   enddo                 ! end ih loop


      end subroutine twostream_adding 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine reduce_flux (np,swc,u1,du,nu,swh,w1,dw,nw,tbl,df,lmask,irestrict)
!*****************************************************************
!-----compute the reduction of clear-sky downward solar flux
!     due to co2 absorption.
      implicit none
!-----input parameters
      logical lmask(CHUNK)
      integer irestrict
      integer np,nu,nw
      real(fp_kind) u1,du,w1,dw
      real(fp_kind) swc(CHUNK,np+1),swh(CHUNK,np+1),tbl(nu,nw)
!-----output (undated) parameter
      real(fp_kind) df(CHUNK,np+1)
!-----temporary array
      integer k,ic,iw,i
      real(fp_kind) clog,wlog,dc,dd,x0,x1,x2,y0,y1,y2
!-----table look-up for the reduction of clear-sky solar
         x0=u1+float(nu)*du
         y0=w1+float(nw)*dw
         x1=u1-0.5*du
         y1=w1-0.5*dw

      do k= 2, np+1
      DO i=1,min(CHUNK,irestrict)
      if(lmask(i) .eqv. .true.) then
          clog=min(swc(i,k),x0)
          clog=max(swc(i,k),x1)
          wlog=min(swh(i,k),y0)
          wlog=max(swh(i,k),y1)
          ic=int( (clog-x1)/du+1.)
          iw=int( (wlog-y1)/dw+1.)
          if(ic.lt.2)ic=2
          if(iw.lt.2)iw=2
          if(ic.gt.nu)ic=nu
          if(iw.gt.nw)iw=nw
          dc=clog-float(ic-2)*du-u1
          dd=wlog-float(iw-2)*dw-w1
          x2=tbl(ic-1,iw-1)+(tbl(ic-1,iw)-tbl(ic-1,iw-1))/dw*dd
          y2=x2+(tbl(ic,iw-1)-tbl(ic-1,iw-1))/du*dc
          df(i,k)=df(i,k)+y2
      endif
      ENDDO
      enddo

      end subroutine reduce_flux

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
 subroutine lwrad ( np, emiss, tb, ts, ict, icb,&
                    pl, ta, wa, oa, fcld, &
                    taucl, ssacl, asycl, &
                    taual, ssaal, asyal,  &
                    flx_out,acflxd_out,acflxu_out, irestrict )
 implicit none
!***********************************************************************
! this routine computes ir fluxes due to water vapor, co2, o3,
!   trace gases (n2o, ch4, cfc11, cfc12, cfc22, co2-minor),
!   clouds, and aerosols.
!
! some detailed descriptions of the radiation routine are given in
!   chou and suarez (1994).
!
! ice and liquid cloud particles are allowed to co-exist in any of the
!  np layers.
!
! if no information is available for the effective cloud particle size,
!  reff, default values of 10 micron for liquid water and 75 micron
!  for ice can be used.
!
! the maximum-random assumption is applied for cloud overlapping.
!  clouds are grouped into high, middle, and low clouds separated by the
!  level indices ict and icb.  within each of the three groups, clouds
!  are assumed maximally overlapped, and the cloud cover of a group is
!  the maximum cloud cover of all the layers in the group.  clouds among
!  the three groups are assumed randomly overlapped. the indices ict and
!  icb correpond approximately to the 400 mb and 700 mb levels.
!
! aerosols are allowed to be in any of the np layers. aerosol optical
!  properties can be specified as functions of height and spectral band.
!
! the ir spectrum is divided into nine bands:
!
!   band     wavenumber (/cm)   absorber
!
!    1           0 - 340           h2o
!    2         340 - 540           h2o
!    3         540 - 800       h2o,cont,co2
!    4         800 - 980       h2o,cont
!                              co2,f11,f12,f22
!    5         980 - 1100      h2o,cont,o3
!                              co2,f11
!    6        1100 - 1215      h2o,cont
!                              n2o,ch4,f12,f22
!    7        1215 - 1380      h2o,cont
!                              n2o,ch4
!    8        1380 - 1900          h2o
!    9        1900 - 3000          h2o
!
! in addition, a narrow band in the 17 micrometer region is added to
!    compute flux reduction due to n2o
!
!    10        540 - 620       h2o,cont,co2,n2o
!
! band 3 (540-800/cm) is further divided into 3 sub-bands :
!
!   subband   wavenumber (/cm)
!
!    1          540 - 620
!    2          620 - 720
!    3          720 - 800
!
!---- input parameters                               units    size
!
!   number of atmospheric layers (np)                  --      1
!   level pressure (pl)                               mb      (np+1)
!   layer temperature (ta)                            k       np
!   layer specific humidity (wa)                      g/g     np
!   layer ozone mixing ratio by mass (oa)             g/g     np
!   surface air temperature (tb)                      k       1 
!   surface temperature (ts)                          k       1 
!   surface emissivity (emiss)                      fraction   10
!   input option for cloud fractional cover            --      1
!      (overcast)   (see explanation above)
!   input option for cloud optical thickness           --      1
!  cloud optical thickness (taucl)                  n/d       np*band
!  cloud single scattering albedo (ssacl)           n/d       np*band
!  cloud asymetry parameter (asycl)                 n/d       np*band
!  aerosol optical thickness (taual)                n/d       np*band
!  aerosol single scattering albedo (ssaal)         n/d       np*band
!  aerosol asymetry parameter (asyal)               n/d       np*band
!   cloud amount (fcld)                             fraction   np
!   level index separating high and middle             --      1
!       clouds (ict)
!   level index separating middle and low              --      1
!       clouds (icb)
!   aerosol optical thickness (taual)                  --    np*10
!   aerosol single-scattering albedo (ssaal)           --    np*10
!   aerosol asymmetry factor (asyal)                   --    np*10
!   high (see explanation above)                       --      1
!   trace (see explanation above)                      --      1
!
! data used in table look-up for transmittance calculations:
!
!   c1 , c2, c3: for co2 (band 3)
!   o1 , o2, o3: for  o3 (band 5)
!   h11,h12,h13: for h2o (band 1)
!   h21,h22,h23: for h2o (band 2)
!   h81,h82,h83: for h2o (band 8)
!
!---- output parameters
!
!   net downward flux, all-sky   (flx)             w/m**2   (np+1)
!   net downward flux, clear-sky (flc)             w/m**2   (np+1)
!   sensitivity of net downward flux
!       to surface temperature (dfdts)            w/m**2/k  (np+1)
!   emission by the surface (sfcem)                 w/m**2     1 
!
! notes:
!
!   (1) water vapor continuum absorption is included in 540-1380 /cm.
!   (2) scattering is parameterized for clouds and aerosols.
!   (3) diffuse cloud and aerosol transmissions are computed
!       from exp(-1.66*tau).
!   (4) if there are no clouds, flx=flc.
!   (5) plevel(1) is the pressure at the top of the model atmosphere,
!        and plevel(np+1) is the surface pressure.
!   (6) downward flux is positive and upward flux is negative.
!   (7) sfcem and dfdts are negative because upward flux is defined as negative.
!   (8) for questions and coding errors, plaese contact ming-dah chou,
!       code 913, nasa/goddard space flight center, greenbelt, md 20771.
!       phone: 301-614-6192, fax: 301-614-6307,
!       e-mail: chou@climate.gsfc.nasa.gov
!
!***************************************************************************
!---- input parameters ------
 integer ,intent(in) ::  np,ict(CHUNK),icb(CHUNK)
 real(fp_kind) ,intent(in) :: pl(CHUNK, np+1),ta(CHUNK, np),wa(CHUNK, np),oa(CHUNK, np), &
                     tb(CHUNK), ts(CHUNK), emiss(CHUNK, nband_lw)
 real(fp_kind) ,intent(in)  :: fcld(CHUNK, np)
 real(fp_kind) , intent(in) :: taucl(CHUNK, np,nband_lw),ssacl(CHUNK, np,nband_lw),asycl(CHUNK, np,nband_lw)
 real(fp_kind) , intent(in) :: taual(CHUNK, np,nband_lw),ssaal(CHUNK, np,nband_lw),asyal(CHUNK, np,nband_lw)
 integer :: irestrict
!---- output parameters ------
 real,intent(out) :: flx_out(CHUNK, np+1)
 real,intent(out) :: acflxu_out(CHUNK, np+1)
 real,intent(out) :: acflxd_out(CHUNK, np+1)  !upwelling and downwelling broadband LW flux [W/m2]

 real(fp_kind) :: flx(CHUNK, np+1)
 real(fp_kind) :: acflxu(CHUNK, np+1),acflxd(CHUNK, np+1)  !upwelling and downwelling broadband LW flux [W/m2]

 real(fp_kind) :: flc(CHUNK, np+1),dfdts(CHUNK, np+1), sfcem(CHUNK)

!---- static data -----
 real(fp_kind) :: cb(6,10),xkw(9),xke(9),aw(9),bw(9),pm(9),fkw(6,9),gkw(6,3)
 real(fp_kind) :: aib(3,10),awb(4,10),aiw(4,10),aww(4,10),aig(4,10),awg(4,10)
 integer :: ne(9),mw(9)

!-----parameters defining the size of the pre-computed tables for
!     transmittance using table look-up.
!c    "nx" is the number of intervals in pressure
!     "nx2" is the number of intervals in pressure
!     "no" is the number of intervals in o3 amount
!     "nc" is the number of intervals in co2 amount
!     "nh" is the number of intervals in h2o amount
 integer,parameter :: nx2=26,no=21,nc=30,nh=31 ! cccshie 9/15/04
 real(fp_kind) :: c1 (nx2,nc),c2 (nx2,nc),c3 (nx2,nc)
 real(fp_kind) :: o1 (nx2,no),o2 (nx2,no),o3 (nx2,no)
 real(fp_kind) :: h11(nx2,nh),h12(nx2,nh),h13(nx2,nh)
 real(fp_kind) :: h21(nx2,nh),h22(nx2,nh),h23(nx2,nh)
 real(fp_kind) :: h71(nx2,nh),h72(nx2,nh),h73(nx2,nh)
 real(fp_kind) :: h81(nx2,nh),h82(nx2,nh),h83(nx2,nh)
!---- temporary arrays -----
 real(fp_kind) :: pa(CHUNK, np),dt(CHUNK, np)
 real(fp_kind) :: sh2o(CHUNK, np+1),swpre(CHUNK, np+1),swtem(CHUNK, np+1)
 real(fp_kind) :: sco3(CHUNK, np+1),scopre(CHUNK, np+1),scotem(CHUNK, np+1)
 real(fp_kind) :: dh2o(CHUNK, np),dcont(CHUNK, np),dco2(CHUNK, np),do3(CHUNK, np)
 real(fp_kind) :: dn2o(CHUNK, np),dch4(CHUNK, np)
 real(fp_kind) :: df11(CHUNK, np),df12(CHUNK, np),df22(CHUNK, np)
 real(fp_kind) :: th2o(CHUNK, 6),tcon(CHUNK, 3),tco2(CHUNK, 6,2)
 real(fp_kind) :: tn2o(CHUNK, 4),tch4(CHUNK, 4),tcom(CHUNK, 6)
 real(fp_kind) :: tf11(CHUNK),tf12(CHUNK),tf22(CHUNK)
 real(fp_kind) :: h2oexp(CHUNK, np,6),conexp(CHUNK, np,3),co2exp(CHUNK, np,6,2)
 real(fp_kind) :: n2oexp(CHUNK, np,4),ch4exp(CHUNK, np,4),comexp(CHUNK, np,6)
 real(fp_kind) :: f11exp(CHUNK, np),f12exp(CHUNK, np),f22exp(CHUNK, np)
 real(fp_kind) :: blayer(CHUNK, 0:np+1),blevel(CHUNK, np+1),dblayr(CHUNK, np+1),dbs(CHUNK)
 real(fp_kind) :: dp(CHUNK, np)
 real(fp_kind) :: trant(CHUNK),tranal(CHUNK),transfc(CHUNK, np+1),trantcr(CHUNK, np+1)
 real(fp_kind) :: flxu(CHUNK, np+1),flxd(CHUNK, np+1),flcu(CHUNK, np+1),flcd(CHUNK, np+1)
 real(fp_kind) :: rflx(CHUNK, np+1),rflc(CHUNK, np+1)
 integer :: it,im,ib
 real(fp_kind) :: cldhi(CHUNK),cldmd(CHUNK),cldlw(CHUNK),tcldlyr(CHUNK, np),fclr(CHUNK)
 real(fp_kind) :: taerlyr(CHUNK, np)

 integer :: j,k,ip,iw,ibn,ik,iq,isb,k1,k2,ic
 real(fp_kind) :: xx,yy,p1,dwe,dpe,a1,b1,fk1,a2,b2,fk2,bu,bd
! real(fp_kind) :: w(CHUNK, max_spc),g(CHUNK, max_spc)
 real(fp_kind) :: w1,ww,gg,ff,taux
 real(fp_kind) :: tauxa

 logical :: oznbnd,co2bnd,h2otbl,conbnd,n2obnd
 logical :: ch4bnd,combnd,f11bnd,f12bnd,f22bnd,b10bnd

!-----the following coefficients are given in table 2 for computing
!     spectrally integrated planck fluxes using eq. (3.11)
       data cb/ &
            5.3443e+0,  -2.0617e-1,   2.5333e-3, &
           -6.8633e-6,   1.0115e-8,  -6.2672e-12, &
            2.7148e+1,  -5.4038e-1,   2.9501e-3, &
            2.7228e-7,  -9.3384e-9,   9.9677e-12, &
           -3.4860e+1,   1.1132e+0,  -1.3006e-2, &
            6.4955e-5,  -1.1815e-7,   8.0424e-11, &
           -6.0513e+1,   1.4087e+0,  -1.2077e-2, &
            4.4050e-5,  -5.6735e-8,   2.5660e-11, &
           -2.6689e+1,   5.2828e-1,  -3.4453e-3, &
            6.0715e-6,   1.2523e-8,  -2.1550e-11, &
           -6.7274e+0,   4.2256e-2,   1.0441e-3, &
           -1.2917e-5,   4.7396e-8,  -4.4855e-11, &
            1.8786e+1,  -5.8359e-1,   6.9674e-3, &
           -3.9391e-5,   1.0120e-7,  -8.2301e-11, &
            1.0344e+2,  -2.5134e+0,   2.3748e-2, &
           -1.0692e-4,   2.1841e-7,  -1.3704e-10, &
           -1.0482e+1,   3.8213e-1,  -5.2267e-3, &
            3.4412e-5,  -1.1075e-7,   1.4092e-10, &
            1.6769e+0,   6.5397e-2,  -1.8125e-3, &
            1.2912e-5,  -2.6715e-8,   1.9792e-11/
!-----xkw is the absorption coefficient are given in table 4 for the
!     first k-distribution interval due to water vapor line absorption.
!     units are cm**2/g
! old (HITRAN1996)
!      data xkw / 29.55  , 4.167e-1, 1.328e-2, 5.250e-4,   &
!                 5.25e-4, 9.369e-3, 4.719e-2, 1.320e-0, 5.250e-4/
! new (HITRAN2012)
      data xkw / 39.453  , 4.306e-1, 1.263e-2, 4.065e-4,   &
                 5.225e-4, 5.464e-3, 4.084e-2, 2.449e+0, 7.981e-3/

!-----xke is the absorption coefficient given in table 9 for the first
!     k-distribution function due to water vapor continuum absorption
!     units are cm**2/g
! old
!      data xke /  0.00,   0.00,   27.40,   15.8, &
!                  9.40,   7.75,    8.78,    0.0,   0.0/
!-----CKD (Version 2.3) continuum k data
      data xke /  0.0,    271.,    25.00,   16.8,   &
                  8.31,   6.52,    12.7,    0.0,  0.0/

!-----mw is the ratio between neighboring absorption coefficients
!     for water vapor line absorption (table 4).
      data mw /6,6,8,6,6,8,9,6,16/
!-----aw and bw (table 3) are the coefficients for temperature scaling
!     in eq. (4.2).
! old (HITRAN1996)
!      data aw/ 0.0021, 0.0140, 0.0167, 0.0302,    &
!               0.0307, 0.0195, 0.0152, 0.0008, 0.0096/
!      data bw/ -1.01e-5, 5.57e-5, 8.54e-5, 2.96e-4,    &
!                2.86e-4, 1.108e-4, 7.608e-5, -3.52e-6, 1.64e-5/
! new (HITRAN2012)
      data aw/ 0.00367, 0.01481, 0.01821, 0.03138,    &
               0.03192, 0.01899, 0.01494, 0.00211, 0.01266/
      data bw/ -2.18e-5, 7.30e-5, 9.46e-5, 3.112e-4,    &
                3.088e-4, 1.074e-4, 7.64e-5, -1.66e-5, 4.88e-5/

!-----pm is the pressure-scaling parameter for water vapor absorption
!     eq. (4.1) and table 3.
      data pm/ 1.0, 1.0, 1.0, 1.0, 1.0, 0.77, 0.5, 1.0, 1.0/
!-----fkw is the planck-weighted k-distribution function due to h2o
!     line absorption given in table 4.
!     the k-distribution function for the third band, fkw(*,3),
!     is not used (see the parameter gkw below).
! old (HITRAN1996)
!      data fkw / 0.2747,0.2717,0.2752,0.1177,0.0352,0.0255, &
!                 0.1521,0.3974,0.1778,0.1826,0.0374,0.0527, &
!                 6*1.00, &
!                 0.4654,0.2991,0.1343,0.0646,0.0226,0.0140, &
!                 0.5543,0.2723,0.1131,0.0443,0.0160,0.0000, &
!                 0.5955,0.2693,0.0953,0.0335,0.0064,0.0000, &
!                 0.1958,0.3469,0.3147,0.1013,0.0365,0.0048, &
!                 0.0740,0.1636,0.4174,0.1783,0.1101,0.0566, &
!                 0.1437,0.2197,0.3185,0.2351,0.0647,0.0183/
! new (HITRAN2012)
      data fkw / 0.2703,0.3113,0.2564,0.1042,0.0428,0.0150,  &
                 0.1338,0.3653,0.2544,0.1298,0.0780,0.0387,  &
                 6*1.00,  &
                 0.2824,0.3696,0.2211,0.0819,0.0332,0.0118,  &
                 0.4442,0.3455,0.1361,0.0536,0.0175,0.0031,  &
                 0.4480,0.3744,0.1219,0.0447,0.0101,0.0009,  &
                 0.2257,0.3316,0.2951,0.1053,0.0348,0.0075,  &
                 0.0750,0.2814,0.3496,0.1769,0.0817,0.0354,  &
                 0.3361,0.3207,0.2459,0.0780,0.0174,0.0019/

!-----gkw is the planck-weighted k-distribution function due to h2o
!     line absorption in the 3 subbands (800-720,620-720,540-620 /cm)
!     of band 3 given in table 10.  note that the order of the sub-bands
!     is reversed.
! old(HITRAN1996)
!      data gkw/  0.1782,0.0593,0.0215,0.0068,0.0022,0.0000, &
!                 0.0923,0.1675,0.0923,0.0187,0.0178,0.0000, &
!                 0.0000,0.1083,0.1581,0.0455,0.0274,0.0041/
! new *(HITRAN2012)
      data gkw/   0.0000,0.0729,0.1761,0.0627,0.0232,0.0084,  &
                  0.0631,0.1712,0.1050,0.0346,0.0131,0.0014,  &
                  0.1568,0.0741,0.0255,0.0101,0.0018,0.0000/

!-----ne is the number of terms used in each band to compute water vapor
!     continuum transmittance (table 9).
      data ne /0,0,3,1,1,1,1,0,0/
!
!-----coefficients for computing the extinction coefficient
!     for cloud ice particles (table 11a, eq. 6.4a).
!
      data aib /  -0.44171,    0.62951,   0.06465, &
                  -0.13727,    0.61291,   0.28962, &
                  -0.01878,    1.67680,   0.79080, &
                  -0.01896,    1.06510,   0.69493, &
                  -0.04788,    0.88178,   0.54492, &
                  -0.02265,    1.57390,   0.76161, &
                  -0.01038,    2.15640,   0.89045, &
                  -0.00450,    2.51370,   0.95989, &
                  -0.00044,    3.15050,   1.03750, &
                  -0.02956,    1.44680,   0.71283/
!
!-----coefficients for computing the extinction coefficient
!     for cloud liquid drops. (table 11b, eq. 6.4b)
!
      data awb /   0.08641,    0.01769,    -1.5572e-3,   3.4896e-5, &
                   0.22027,    0.00997,    -1.8719e-3,   5.3112e-5, &
                   0.38074,   -0.03027,     1.0154e-3,  -1.1849e-5, &
                   0.15587,    0.00371,    -7.7705e-4,   2.0547e-5, &
                   0.05518,    0.04544,    -4.2067e-3,   1.0184e-4, &
                   0.12724,    0.04751,    -5.2037e-3,   1.3711e-4, &
                   0.30390,    0.01656,    -3.5271e-3,   1.0828e-4, &
                   0.63617,   -0.06287,     2.2350e-3,  -2.3177e-5, &
                   1.15470,   -0.19282,     1.2084e-2,  -2.5612e-4, &
                   0.34021,   -0.02805,     1.0654e-3,  -1.5443e-5/
!
!-----coefficients for computing the single-scattering albedo
!     for cloud ice particles. (table 12a, eq. 6.5)
!
      data aiw/    0.17201,    1.2229e-2,  -1.4837e-4,   5.8020e-7, &
                   0.81470,   -2.7293e-3,   9.7816e-8,   5.7650e-8, &
                   0.54859,   -4.8273e-4,   5.4353e-6,  -1.5679e-8, &
                   0.39218,    4.1717e-3, - 4.8869e-5,   1.9144e-7, &
                   0.71773,   -3.3640e-3,   1.9713e-5,  -3.3189e-8, &
                   0.77345,   -5.5228e-3,   4.8379e-5,  -1.5151e-7, &
                   0.74975,   -5.6604e-3,   5.6475e-5,  -1.9664e-7, &
                   0.69011,   -4.5348e-3,   4.9322e-5,  -1.8255e-7, &
                   0.83963,   -6.7253e-3,   6.1900e-5,  -2.0862e-7, &
                   0.64860,   -2.8692e-3,   2.7656e-5,  -8.9680e-8/
!
!-----coefficients for computing the single-scattering albedo
!     for cloud liquid drops. (table 12b, eq. 6.5)
!
      data aww/   -7.8566e-2,  8.0875e-2,  -4.3403e-3,   8.1341e-5, &
                  -1.3384e-2,  9.3134e-2,  -6.0491e-3,   1.3059e-4, &
                   3.7096e-2,  7.3211e-2,  -4.4211e-3,   9.2448e-5, &
                  -3.7600e-3,  9.3344e-2,  -5.6561e-3,   1.1387e-4, &
                   0.40212,    7.8083e-2,  -5.9583e-3,   1.2883e-4, &
                   0.57928,    5.9094e-2,  -5.4425e-3,   1.2725e-4, &
                   0.68974,    4.2334e-2,  -4.9469e-3,   1.2863e-4, &
                   0.80122,    9.4578e-3,  -2.8508e-3,   9.0078e-5, &
                   1.02340,   -2.6204e-2,   4.2552e-4,   3.2160e-6, &
                   0.05092,    7.5409e-2,  -4.7305e-3,   1.0121e-4/
!
!-----coefficients for computing the asymmetry factor for cloud ice
!     particles. (table 13a, eq. 6.6)
!
      data aig /   0.57867,    1.0135e-2,  -1.1142e-4,   4.1537e-7, &
                   0.72259,    3.1149e-3,  -1.9927e-5,   5.6024e-8, &
                   0.76109,    4.5449e-3,  -4.6199e-5,   1.6446e-7, &
                   0.86934,    2.7474e-3,  -3.1301e-5,   1.1959e-7, &
                   0.89103,    1.8513e-3,  -1.6551e-5,   5.5193e-8, &
                   0.86325,    2.1408e-3,  -1.6846e-5,   4.9473e-8, &
                   0.85064,    2.5028e-3,  -2.0812e-5,   6.3427e-8, &
                   0.86945,    2.4615e-3,  -2.3882e-5,   8.2431e-8, &
                   0.80122,    3.1906e-3,  -2.4856e-5,   7.2411e-8, &
                   0.73290,    4.8034e-3,  -4.4425e-5,   1.4839e-7/
!
!-----coefficients for computing the asymmetry factor for cloud liquid
!     drops. (table 13b, eq. 6.6)
!
      data awg /  -0.51930,    0.20290,    -1.1747e-2,   2.3868e-4, &
                  -0.22151,    0.19708,    -1.2462e-2,   2.6646e-4, &
                   0.14157,    0.14705,    -9.5802e-3,   2.0819e-4, &
                   0.41590,    0.10482,    -6.9118e-3,   1.5115e-4, &
                   0.55338,    7.7016e-2,  -5.2218e-3,   1.1587e-4, &
                   0.61384,    6.4402e-2,  -4.6241e-3,   1.0746e-4, &
                   0.67891,    4.8698e-2,  -3.7021e-3,   9.1966e-5, &
                   0.78169,    2.0803e-2,  -1.4749e-3,   3.9362e-5, &
                   0.93218,   -3.3425e-2,   2.9632e-3,  -6.9362e-5, &
                   0.01649,    0.16561,    -1.0723e-2,   2.3220e-4/
!
!-----include tables used in the table look-up for co2 (band 3),
!     o3 (band 5), and h2o (bands 1, 2, and 7) transmission functions.
!     "co2.tran4" is the new co2 transmission table applicable to a large
!     range of co2 amount (up to 100 times of the present-time value).
!     include 'h2o.tran3'
!     include 'co2.tran4'
!     include 'o3.tran3'
      data ((h11(ip,iw),iw=1,31), ip= 1, 1)/ &
        0.99993813,  0.99990332,  0.99985558,  0.99979532,  0.99972224, &
        0.99963796,  0.99954617,  0.99944353,  0.99932158,  0.99916780, &
        0.99896526,  0.99869478,  0.99833935,  0.99788177,  0.99729526, &
        0.99653482,  0.99553120,  0.99419260,  0.99240279,  0.99000663, &
        0.98677677,  0.98239183,  0.97641385,  0.96825922,  0.95715666, &
        0.94209653,  0.92173046,  0.89425236,  0.85741836,  0.80881417, &
        0.74626487/
      data ((h12(ip,iw),iw=1,31), ip= 1, 1)/ &
       -0.1374E-06, -0.2708E-06, -0.4751E-06, -0.7528E-06, -0.1099E-05, &
       -0.1483E-05, -0.1891E-05, -0.2361E-05, -0.2959E-05, -0.3751E-05, &
       -0.4799E-05, -0.6115E-05, -0.7662E-05, -0.9412E-05, -0.1143E-04, &
       -0.1378E-04, -0.1658E-04, -0.1990E-04, -0.2375E-04, -0.2818E-04, &
       -0.3337E-04, -0.3952E-04, -0.4697E-04, -0.5621E-04, -0.6779E-04, &
       -0.8221E-04, -0.1002E-03, -0.1233E-03, -0.1539E-03, -0.1946E-03, &
       -0.2470E-03/
      data ((h13(ip,iw),iw=1,31), ip= 1, 1)/ &
        0.3566E-09,  0.4603E-09,  0.5960E-09,  0.6227E-09,  0.7610E-09, &
        0.9207E-09,  0.6174E-09, -0.2368E-09, -0.1897E-08, -0.4880E-08, &
       -0.8568E-08, -0.1181E-07, -0.1402E-07, -0.1578E-07, -0.1755E-07, &
       -0.1971E-07, -0.2167E-07, -0.2299E-07, -0.2341E-07, -0.2370E-07, &
       -0.2437E-07, -0.2530E-07, -0.2620E-07, -0.2813E-07, -0.3265E-07, &
       -0.3921E-07, -0.4360E-07, -0.3767E-07, -0.1222E-07,  0.4307E-07, &
        0.1446E-06/
      data ((h11(ip,iw),iw=1,31), ip= 2, 2)/ &
        0.99993813,  0.99990314,  0.99985522,  0.99979478,  0.99972117, &
        0.99963617,  0.99954265,  0.99943727,  0.99931049,  0.99914831, &
        0.99893194,  0.99863851,  0.99824709,  0.99773568,  0.99706858, &
        0.99618745,  0.99500763,  0.99341935,  0.99127823,  0.98838830, &
        0.98446715,  0.97911924,  0.97180730,  0.96182114,  0.94823337, &
        0.92982620,  0.90495598,  0.87150788,  0.82708740,  0.76937377, &
        0.69672096/
      data ((h12(ip,iw),iw=1,31), ip= 2, 2)/ &
       -0.1367E-06, -0.2697E-06, -0.4737E-06, -0.7504E-06, -0.1093E-05, &
       -0.1473E-05, -0.1874E-05, -0.2335E-05, -0.2916E-05, -0.3688E-05, &
       -0.4705E-05, -0.5984E-05, -0.7493E-05, -0.9212E-05, -0.1123E-04, &
       -0.1363E-04, -0.1652E-04, -0.1998E-04, -0.2400E-04, -0.2864E-04, &
       -0.3409E-04, -0.4067E-04, -0.4882E-04, -0.5915E-04, -0.7225E-04, &
       -0.8880E-04, -0.1099E-03, -0.1377E-03, -0.1747E-03, -0.2233E-03, &
       -0.2834E-03/
      data ((h13(ip,iw),iw=1,31), ip= 2, 2)/ &
        0.3486E-09,  0.4524E-09,  0.5881E-09,  0.5960E-09,  0.7211E-09, &
        0.8648E-09,  0.5641E-09, -0.3432E-09, -0.2022E-08, -0.4965E-08, &
       -0.8629E-08, -0.1170E-07, -0.1374E-07, -0.1544E-07, -0.1719E-07, &
       -0.1906E-07, -0.2055E-07, -0.2111E-07, -0.2052E-07, -0.1940E-07, &
       -0.1896E-07, -0.1908E-07, -0.1923E-07, -0.2091E-07, -0.2455E-07, &
       -0.2895E-07, -0.2714E-07, -0.9139E-08,  0.3458E-07,  0.1185E-06, &
        0.2589E-06/
      data ((h11(ip,iw),iw=1,31), ip= 3, 3)/ &
        0.99993807,  0.99990296,  0.99985474,  0.99979377,  0.99971932, &
        0.99963313,  0.99953723,  0.99942762,  0.99929363,  0.99911934, &
        0.99888295,  0.99855793,  0.99811894,  0.99753672,  0.99676406, &
        0.99572688,  0.99432445,  0.99242115,  0.98983938,  0.98632908, &
        0.98154306,  0.97499502,  0.96603107,  0.95379549,  0.93717366, &
        0.91468084,  0.88435274,  0.84385335,  0.79079306,  0.72320402, &
        0.64039373/
      data ((h12(ip,iw),iw=1,31), ip= 3, 3)/ &
       -0.1355E-06, -0.2684E-06, -0.4700E-06, -0.7442E-06, -0.1085E-05, &
       -0.1459E-05, -0.1848E-05, -0.2300E-05, -0.2862E-05, -0.3604E-05, &
       -0.4590E-05, -0.5834E-05, -0.7305E-05, -0.9014E-05, -0.1106E-04, &
       -0.1354E-04, -0.1657E-04, -0.2018E-04, -0.2440E-04, -0.2930E-04, &
       -0.3512E-04, -0.4229E-04, -0.5141E-04, -0.6313E-04, -0.7817E-04, &
       -0.9750E-04, -0.1228E-03, -0.1564E-03, -0.2011E-03, -0.2578E-03, &
       -0.3238E-03/
      data ((h13(ip,iw),iw=1,31), ip= 3, 3)/ &
        0.3379E-09,  0.4391E-09,  0.5748E-09,  0.5801E-09,  0.6972E-09, &
        0.7717E-09,  0.4471E-09, -0.4470E-09, -0.2150E-08, -0.5040E-08, &
       -0.8507E-08, -0.1138E-07, -0.1336E-07, -0.1498E-07, -0.1669E-07, &
       -0.1817E-07, -0.1904E-07, -0.1853E-07, -0.1671E-07, -0.1422E-07, &
       -0.1269E-07, -0.1200E-07, -0.1198E-07, -0.1331E-07, -0.1561E-07, &
       -0.1564E-07, -0.4129E-08,  0.3015E-07,  0.9862E-07,  0.2180E-06, &
        0.4007E-06/
      data ((h11(ip,iw),iw=1,31), ip= 4, 4)/ &
        0.99993783,  0.99990243,  0.99985403,  0.99979228,  0.99971670, &
        0.99962842,  0.99952894,  0.99941313,  0.99926841,  0.99907714, &
        0.99881303,  0.99844551,  0.99794370,  0.99726915,  0.99635881, &
        0.99512327,  0.99343932,  0.99114186,  0.98800439,  0.98371774, &
        0.97785223,  0.96981639,  0.95881772,  0.94383150,  0.92350686, &
        0.89604330,  0.85920161,  0.81056869,  0.74796361,  0.67015076, &
        0.57788986/
      data ((h12(ip,iw),iw=1,31), ip= 4, 4)/ &
       -0.1349E-06, -0.2669E-06, -0.4655E-06, -0.7354E-06, -0.1070E-05, &
       -0.1438E-05, -0.1817E-05, -0.2249E-05, -0.2790E-05, -0.3505E-05, &
       -0.4458E-05, -0.5669E-05, -0.7117E-05, -0.8833E-05, -0.1094E-04, &
       -0.1355E-04, -0.1673E-04, -0.2055E-04, -0.2501E-04, -0.3023E-04, &
       -0.3656E-04, -0.4457E-04, -0.5495E-04, -0.6843E-04, -0.8596E-04, &
       -0.1089E-03, -0.1396E-03, -0.1804E-03, -0.2333E-03, -0.2970E-03, &
       -0.3659E-03/
      data ((h13(ip,iw),iw=1,31), ip= 4, 4)/ &
        0.3486E-09,  0.4550E-09,  0.5482E-09,  0.5508E-09,  0.6200E-09, &
        0.6546E-09,  0.3246E-09, -0.5881E-09, -0.2182E-08, -0.5045E-08, &
       -0.8305E-08, -0.1096E-07, -0.1284E-07, -0.1455E-07, -0.1597E-07, &
       -0.1701E-07, -0.1692E-07, -0.1515E-07, -0.1179E-07, -0.8057E-08, &
       -0.5678E-08, -0.4646E-08, -0.4521E-08, -0.5170E-08, -0.4770E-08, &
        0.2814E-08,  0.2832E-07,  0.8408E-07,  0.1846E-06,  0.3442E-06, &
        0.5701E-06/
      data ((h11(ip,iw),iw=1,31), ip= 5, 5)/ &
        0.99993753,  0.99990189,  0.99985290,  0.99979007,  0.99971253, &
        0.99962103,  0.99951631,  0.99939132,  0.99923170,  0.99901599, &
        0.99871421,  0.99829102,  0.99770778,  0.99691248,  0.99582618, &
        0.99433988,  0.99230248,  0.98950773,  0.98567337,  0.98041713, &
        0.97321093,  0.96333665,  0.94984657,  0.93150383,  0.90666318, &
        0.87321484,  0.82876885,  0.77100623,  0.69827306,  0.61053538, &
        0.51051188/
      data ((h12(ip,iw),iw=1,31), ip= 5, 5)/ &
       -0.1329E-06, -0.2618E-06, -0.4579E-06, -0.7235E-06, -0.1051E-05, &
       -0.1408E-05, -0.1775E-05, -0.2189E-05, -0.2706E-05, -0.3395E-05, &
       -0.4319E-05, -0.5497E-05, -0.6940E-05, -0.8701E-05, -0.1091E-04, &
       -0.1367E-04, -0.1705E-04, -0.2110E-04, -0.2585E-04, -0.3153E-04, &
       -0.3858E-04, -0.4771E-04, -0.5970E-04, -0.7542E-04, -0.9620E-04, &
       -0.1240E-03, -0.1612E-03, -0.2101E-03, -0.2709E-03, -0.3392E-03, &
       -0.4077E-03/
      data ((h13(ip,iw),iw=1,31), ip= 5, 5)/ &
        0.3512E-09,  0.4124E-09,  0.5029E-09,  0.4763E-09,  0.5481E-09, &
        0.5881E-09,  0.1889E-09, -0.6732E-09, -0.2256E-08, -0.4875E-08, &
       -0.7882E-08, -0.1041E-07, -0.1229E-07, -0.1391E-07, -0.1515E-07, &
       -0.1539E-07, -0.1407E-07, -0.1072E-07, -0.5888E-08, -0.1194E-08, &
        0.1940E-08,  0.3026E-08,  0.3204E-08,  0.4126E-08,  0.1027E-07, &
        0.2961E-07,  0.7341E-07,  0.1571E-06,  0.2954E-06,  0.4979E-06, &
        0.7612E-06/
      data ((h11(ip,iw),iw=1,31), ip= 6, 6)/ &
        0.99993718,  0.99990106,  0.99985105,  0.99978644,  0.99970627, &
        0.99961007,  0.99949741,  0.99935919,  0.99917841,  0.99892938, &
        0.99857765,  0.99808246,  0.99739319,  0.99644214,  0.99513316, &
        0.99333090,  0.99084938,  0.98742902,  0.98272377,  0.97626024, &
        0.96739453,  0.95526421,  0.93873185,  0.91629297,  0.88598102, &
        0.84546542,  0.79236054,  0.72470009,  0.64178503,  0.54529846, &
        0.44021600/
      data ((h12(ip,iw),iw=1,31), ip= 6, 6)/ &
       -0.1301E-06, -0.2574E-06, -0.4472E-06, -0.7059E-06, -0.1023E-05, &
       -0.1367E-05, -0.1719E-05, -0.2116E-05, -0.2612E-05, -0.3275E-05, &
       -0.4169E-05, -0.5332E-05, -0.6795E-05, -0.8638E-05, -0.1099E-04, &
       -0.1394E-04, -0.1755E-04, -0.2188E-04, -0.2703E-04, -0.3333E-04, &
       -0.4138E-04, -0.5196E-04, -0.6596E-04, -0.8460E-04, -0.1096E-03, &
       -0.1435E-03, -0.1884E-03, -0.2457E-03, -0.3126E-03, -0.3822E-03, &
       -0.4480E-03/
      data ((h13(ip,iw),iw=1,31), ip= 6, 6)/ &
        0.3326E-09,  0.3858E-09,  0.4816E-09,  0.4231E-09,  0.4311E-09, &
        0.4364E-09,  0.6119E-10, -0.7397E-09, -0.2145E-08, -0.4473E-08, &
       -0.7238E-08, -0.9707E-08, -0.1161E-07, -0.1314E-07, -0.1397E-07, &
       -0.1319E-07, -0.1028E-07, -0.5231E-08,  0.8217E-09,  0.6399E-08, &
        0.9764E-08,  0.1062E-07,  0.1157E-07,  0.1639E-07,  0.3185E-07, &
        0.6721E-07,  0.1355E-06,  0.2539E-06,  0.4333E-06,  0.6767E-06, &
        0.9654E-06/
      data ((h11(ip,iw),iw=1,31), ip= 7, 7)/ &
        0.99993646,  0.99989963,  0.99984819,  0.99978095,  0.99969679, &
        0.99959368,  0.99946970,  0.99931306,  0.99910289,  0.99880952, &
        0.99839330,  0.99780440,  0.99697787,  0.99582863,  0.99423826, &
        0.99203920,  0.98899806,  0.98479503,  0.97900313,  0.97104371, &
        0.96013606,  0.94524759,  0.92500275,  0.89758098,  0.86074132, &
        0.81207508,  0.74940544,  0.67149895,  0.57911086,  0.47608691, &
        0.36935675/
      data ((h12(ip,iw),iw=1,31), ip= 7, 7)/ &
       -0.1259E-06, -0.2487E-06, -0.4333E-06, -0.6814E-06, -0.9866E-06, &
       -0.1320E-05, -0.1654E-05, -0.2035E-05, -0.2510E-05, -0.3150E-05, &
       -0.4022E-05, -0.5191E-05, -0.6710E-05, -0.8678E-05, -0.1121E-04, &
       -0.1439E-04, -0.1827E-04, -0.2296E-04, -0.2866E-04, -0.3583E-04, &
       -0.4518E-04, -0.5758E-04, -0.7419E-04, -0.9662E-04, -0.1271E-03, &
       -0.1682E-03, -0.2216E-03, -0.2861E-03, -0.3562E-03, -0.4242E-03, &
       -0.4861E-03/
      data ((h13(ip,iw),iw=1,31), ip= 7, 7)/ &
        0.3273E-09,  0.3592E-09,  0.4204E-09,  0.3646E-09,  0.3060E-09, &
        0.2954E-09, -0.5854E-10, -0.7584E-09, -0.1932E-08, -0.3941E-08, &
       -0.6493E-08, -0.8930E-08, -0.1081E-07, -0.1215E-07, -0.1224E-07, &
       -0.1007E-07, -0.5417E-08,  0.1187E-08,  0.8212E-08,  0.1434E-07, &
        0.1746E-07,  0.1858E-07,  0.2183E-07,  0.3396E-07,  0.6291E-07, &
        0.1194E-06,  0.2191E-06,  0.3766E-06,  0.5984E-06,  0.8738E-06, &
        0.1181E-05/
      data ((h11(ip,iw),iw=1,31), ip= 8, 8)/ &
        0.99993581,  0.99989760,  0.99984390,  0.99977297,  0.99968249, &
        0.99956971,  0.99942952,  0.99924731,  0.99899793,  0.99864626, &
        0.99814731,  0.99743760,  0.99643582,  0.99503660,  0.99309152, &
        0.99039197,  0.98664820,  0.98146874,  0.97432804,  0.96452290, &
        0.95111614,  0.93286294,  0.90809101,  0.87467122,  0.83021170, &
        0.77239782,  0.69957948,  0.61172736,  0.51155847,  0.40512204, &
        0.30019701/
      data ((h12(ip,iw),iw=1,31), ip= 8, 8)/ &
       -0.1203E-06, -0.2378E-06, -0.4132E-06, -0.6492E-06, -0.9431E-06, &
       -0.1259E-05, -0.1582E-05, -0.1948E-05, -0.2410E-05, -0.3031E-05, &
       -0.3898E-05, -0.5094E-05, -0.6713E-05, -0.8847E-05, -0.1161E-04, &
       -0.1505E-04, -0.1927E-04, -0.2445E-04, -0.3091E-04, -0.3923E-04, &
       -0.5023E-04, -0.6496E-04, -0.8494E-04, -0.1123E-03, -0.1495E-03, &
       -0.1989E-03, -0.2603E-03, -0.3297E-03, -0.3995E-03, -0.4642E-03, &
       -0.5203E-03/
      data ((h13(ip,iw),iw=1,31), ip= 8, 8)/ &
        0.2634E-09,  0.3033E-09,  0.3592E-09,  0.2900E-09,  0.2528E-09, &
        0.2155E-09, -0.4258E-10, -0.6040E-09, -0.1469E-08, -0.3124E-08, &
       -0.5564E-08, -0.8001E-08, -0.9962E-08, -0.1090E-07, -0.9829E-08, &
       -0.5867E-08,  0.5805E-09,  0.8262E-08,  0.1622E-07,  0.2218E-07, &
        0.2516E-07,  0.2746E-07,  0.3629E-07,  0.5981E-07,  0.1066E-06, &
        0.1904E-06,  0.3272E-06,  0.5263E-06,  0.7851E-06,  0.1080E-05, &
        0.1425E-05/
      data ((h11(ip,iw),iw=1,31), ip= 9, 9)/ &
        0.99993432,  0.99989462,  0.99983770,  0.99976140,  0.99966222, &
        0.99953526,  0.99937302,  0.99915594,  0.99885464,  0.99842918, &
        0.99782312,  0.99695855,  0.99573493,  0.99401993,  0.99162894, &
        0.98830014,  0.98367906,  0.97728425,  0.96847647,  0.95640767, &
        0.93995035,  0.91759145,  0.88733667,  0.84683520,  0.79370028, &
        0.72597051,  0.64295048,  0.54633045,  0.44109100,  0.33477455, &
        0.23450381/
      data ((h12(ip,iw),iw=1,31), ip= 9, 9)/ &
       -0.1112E-06, -0.2231E-06, -0.3897E-06, -0.6124E-06, -0.8909E-06, &
       -0.1194E-05, -0.1507E-05, -0.1863E-05, -0.2316E-05, -0.2927E-05, &
       -0.3807E-05, -0.5078E-05, -0.6846E-05, -0.9185E-05, -0.1220E-04, &
       -0.1598E-04, -0.2064E-04, -0.2648E-04, -0.3397E-04, -0.4378E-04, &
       -0.5687E-04, -0.7461E-04, -0.9898E-04, -0.1325E-03, -0.1776E-03, &
       -0.2354E-03, -0.3032E-03, -0.3740E-03, -0.4413E-03, -0.5014E-03, &
       -0.5464E-03/
      data ((h13(ip,iw),iw=1,31), ip= 9, 9)/ &
        0.2342E-09,  0.2421E-09,  0.2847E-09,  0.1916E-09,  0.1464E-09, &
        0.1597E-09,  0.2662E-10, -0.2741E-09, -0.8009E-09, -0.2238E-08, &
       -0.4534E-08, -0.7041E-08, -0.8877E-08, -0.9047E-08, -0.6429E-08, &
       -0.5614E-09,  0.7273E-08,  0.1607E-07,  0.2426E-07,  0.2989E-07, &
        0.3348E-07,  0.3947E-07,  0.5740E-07,  0.9656E-07,  0.1671E-06, &
        0.2848E-06,  0.4616E-06,  0.7007E-06,  0.9846E-06,  0.1305E-05, &
        0.1719E-05/
      data ((h11(ip,iw),iw=1,31), ip=10,10)/ &
        0.99993247,  0.99989015,  0.99982893,  0.99974501,  0.99963343, &
        0.99948746,  0.99929446,  0.99903160,  0.99866378,  0.99814367, &
        0.99740094,  0.99633998,  0.99483567,  0.99272412,  0.98977107, &
        0.98565489,  0.97994047,  0.97204179,  0.96118468,  0.94635040, &
        0.92617261,  0.89882028,  0.86202478,  0.81335741,  0.75064027, &
        0.67264533,  0.58013344,  0.47696161,  0.37007034,  0.26708907, &
        0.17368871/
      data ((h12(ip,iw),iw=1,31), ip=10,10)/ &
       -0.1013E-06, -0.2034E-06, -0.3576E-06, -0.5686E-06, -0.8351E-06, &
       -0.1128E-05, -0.1435E-05, -0.1790E-05, -0.2238E-05, -0.2853E-05, &
       -0.3777E-05, -0.5171E-05, -0.7123E-05, -0.9722E-05, -0.1306E-04, &
       -0.1725E-04, -0.2250E-04, -0.2924E-04, -0.3808E-04, -0.4976E-04, &
       -0.6553E-04, -0.8717E-04, -0.1171E-03, -0.1580E-03, -0.2117E-03, &
       -0.2770E-03, -0.3481E-03, -0.4174E-03, -0.4808E-03, -0.5329E-03, &
       -0.5565E-03/
      data ((h13(ip,iw),iw=1,31), ip=10,10)/ &
        0.2076E-09,  0.2262E-09,  0.2182E-09,  0.1543E-09,  0.1650E-09, &
        0.1916E-09,  0.2688E-09,  0.2342E-09,  0.3996E-10, -0.1211E-08, &
       -0.3462E-08, -0.6038E-08, -0.7379E-08, -0.6376E-08, -0.1883E-08, &
        0.5692E-08,  0.1466E-07,  0.2415E-07,  0.3214E-07,  0.3774E-07, &
        0.4336E-07,  0.5669E-07,  0.8802E-07,  0.1476E-06,  0.2487E-06, &
        0.4046E-06,  0.6228E-06,  0.8933E-06,  0.1196E-05,  0.1569E-05, &
        0.2059E-05/
      data ((h11(ip,iw),iw=1,31), ip=11,11)/ &
        0.99993002,  0.99988443,  0.99981701,  0.99972272,  0.99959445, &
        0.99942166,  0.99918830,  0.99886608,  0.99841398,  0.99777257, &
        0.99685609,  0.99554735,  0.99369043,  0.99107867,  0.98742169, &
        0.98232460,  0.97525448,  0.96550310,  0.95214033,  0.93393362, &
        0.90921962,  0.87585801,  0.83142829,  0.77359635,  0.70071238, &
        0.61275256,  0.51244217,  0.40584934,  0.30076474,  0.20359278, &
        0.11942053/
      data ((h12(ip,iw),iw=1,31), ip=11,11)/ &
       -0.9120E-07, -0.1840E-06, -0.3247E-06, -0.5275E-06, -0.7826E-06, &
       -0.1069E-05, -0.1380E-05, -0.1738E-05, -0.2186E-05, -0.2831E-05, &
       -0.3847E-05, -0.5406E-05, -0.7598E-05, -0.1050E-04, -0.1422E-04, &
       -0.1896E-04, -0.2502E-04, -0.3296E-04, -0.4348E-04, -0.5756E-04, &
       -0.7679E-04, -0.1035E-03, -0.1403E-03, -0.1896E-03, -0.2515E-03, &
       -0.3217E-03, -0.3927E-03, -0.4588E-03, -0.5163E-03, -0.5529E-03, &
       -0.5392E-03/
      data ((h13(ip,iw),iw=1,31), ip=11,11)/ &
        0.1756E-09,  0.1943E-09,  0.2315E-09,  0.1863E-09,  0.2289E-09, &
        0.4204E-09,  0.6866E-09,  0.9180E-09,  0.9738E-09,  0.2615E-11, &
       -0.2326E-08, -0.4755E-08, -0.5239E-08, -0.2568E-08,  0.3666E-08, &
        0.1248E-07,  0.2263E-07,  0.3208E-07,  0.3976E-07,  0.4653E-07, &
        0.5765E-07,  0.8231E-07,  0.1312E-06,  0.2174E-06,  0.3545E-06, &
        0.5510E-06,  0.8056E-06,  0.1095E-05,  0.1436E-05,  0.1889E-05, &
        0.2378E-05/
      data ((h11(ip,iw),iw=1,31), ip=12,12)/ &
        0.99992687,  0.99987698,  0.99980158,  0.99969339,  0.99954218, &
        0.99933404,  0.99904799,  0.99865049,  0.99809140,  0.99729615, &
        0.99616045,  0.99453890,  0.99224001,  0.98900169,  0.98446733, &
        0.97815216,  0.96940953,  0.95738846,  0.94096935,  0.91864955, &
        0.88844192,  0.84798515,  0.79486394,  0.72709715,  0.64399153, &
        0.54724288,  0.44184703,  0.33536971,  0.23494369,  0.14573598, &
        0.07395160/
      data ((h12(ip,iw),iw=1,31), ip=12,12)/ &
       -0.7928E-07, -0.1639E-06, -0.2965E-06, -0.4862E-06, -0.7376E-06, &
       -0.1025E-05, -0.1346E-05, -0.1712E-05, -0.2179E-05, -0.2892E-05, &
       -0.4051E-05, -0.5820E-05, -0.8295E-05, -0.1157E-04, -0.1578E-04, &
       -0.2124E-04, -0.2840E-04, -0.3787E-04, -0.5051E-04, -0.6766E-04, &
       -0.9138E-04, -0.1243E-03, -0.1691E-03, -0.2270E-03, -0.2954E-03, &
       -0.3672E-03, -0.4357E-03, -0.4972E-03, -0.5437E-03, -0.5514E-03, &
       -0.4828E-03/
      data ((h13(ip,iw),iw=1,31), ip=12,12)/ &
        0.1703E-09,  0.1809E-09,  0.2288E-09,  0.2209E-09,  0.4417E-09, &
        0.8063E-09,  0.1264E-08,  0.1786E-08,  0.2033E-08,  0.1171E-08, &
       -0.1000E-08, -0.2810E-08, -0.2134E-08,  0.2190E-08,  0.9878E-08, &
        0.1981E-07,  0.3034E-07,  0.3958E-07,  0.4771E-07,  0.5816E-07, &
        0.7848E-07,  0.1187E-06,  0.1909E-06,  0.3103E-06,  0.4864E-06, &
        0.7222E-06,  0.1001E-05,  0.1316E-05,  0.1726E-05,  0.2231E-05, &
        0.2554E-05/
      data ((h11(ip,iw),iw=1,31), ip=13,13)/ &
        0.99992329,  0.99986815,  0.99978262,  0.99965620,  0.99947554, &
        0.99922138,  0.99886787,  0.99837518,  0.99767971,  0.99669021, &
        0.99527812,  0.99326670,  0.99041331,  0.98639458,  0.98077095, &
        0.97295225,  0.96215779,  0.94736737,  0.92722213,  0.89990038, &
        0.86313921,  0.81449950,  0.75177604,  0.67372197,  0.58109993, &
        0.47777659,  0.37071592,  0.26757157,  0.17401850,  0.09552824, &
        0.03950775/
      data ((h12(ip,iw),iw=1,31), ip=13,13)/ &
       -0.7227E-07, -0.1493E-06, -0.2728E-06, -0.4566E-06, -0.7047E-06, &
       -0.1007E-05, -0.1345E-05, -0.1730E-05, -0.2248E-05, -0.3078E-05, &
       -0.4417E-05, -0.6439E-05, -0.9257E-05, -0.1298E-04, -0.1785E-04, &
       -0.2429E-04, -0.3284E-04, -0.4422E-04, -0.5958E-04, -0.8071E-04, &
       -0.1101E-03, -0.1506E-03, -0.2041E-03, -0.2696E-03, -0.3414E-03, &
       -0.4118E-03, -0.4764E-03, -0.5299E-03, -0.5548E-03, -0.5160E-03, &
       -0.3857E-03/
      data ((h13(ip,iw),iw=1,31), ip=13,13)/ &
        0.1836E-09,  0.1863E-09,  0.2794E-09,  0.4045E-09,  0.7264E-09, &
        0.1338E-08,  0.2086E-08,  0.2746E-08,  0.3257E-08,  0.2645E-08, &
        0.9448E-09, -0.1853E-10,  0.2065E-08,  0.7783E-08,  0.1645E-07, &
        0.2703E-07,  0.3746E-07,  0.4680E-07,  0.5701E-07,  0.7468E-07, &
        0.1085E-06,  0.1693E-06,  0.2717E-06,  0.4286E-06,  0.6452E-06, &
        0.9119E-06,  0.1209E-05,  0.1577E-05,  0.2063E-05,  0.2494E-05, &
        0.2469E-05/
      data ((h11(ip,iw),iw=1,31), ip=14,14)/ &
        0.99991977,  0.99985856,  0.99976057,  0.99961144,  0.99939293, &
        0.99908102,  0.99864274,  0.99802899,  0.99716157,  0.99592775, &
        0.99416977,  0.99166948,  0.98812658,  0.98314106,  0.97617304, &
        0.96650791,  0.95320976,  0.93504637,  0.91035986,  0.87701780, &
        0.83260548,  0.77477765,  0.70185912,  0.61381042,  0.51335931, &
        0.40658849,  0.30131996,  0.20398021,  0.11965722,  0.05530477, &
        0.01708663/
      data ((h12(ip,iw),iw=1,31), ip=14,14)/ &
       -0.6855E-07, -0.1384E-06, -0.2600E-06, -0.4393E-06, -0.6962E-06, &
       -0.1021E-05, -0.1392E-05, -0.1819E-05, -0.2428E-05, -0.3415E-05, &
       -0.4978E-05, -0.7293E-05, -0.1051E-04, -0.1483E-04, -0.2057E-04, &
       -0.2826E-04, -0.3856E-04, -0.5237E-04, -0.7122E-04, -0.9747E-04, &
       -0.1339E-03, -0.1829E-03, -0.2447E-03, -0.3153E-03, -0.3870E-03, &
       -0.4542E-03, -0.5130E-03, -0.5509E-03, -0.5383E-03, -0.4388E-03, &
       -0.2635E-03/
      data ((h13(ip,iw),iw=1,31), ip=14,14)/ &
        0.1756E-09,  0.2049E-09,  0.3858E-09,  0.6227E-09,  0.1150E-08, &
        0.1977E-08,  0.3036E-08,  0.3978E-08,  0.4649E-08,  0.4476E-08, &
        0.3773E-08,  0.4130E-08,  0.7338E-08,  0.1390E-07,  0.2331E-07, &
        0.3377E-07,  0.4386E-07,  0.5410E-07,  0.6971E-07,  0.9880E-07, &
        0.1503E-06,  0.2381E-06,  0.3766E-06,  0.5735E-06,  0.8258E-06, &
        0.1111E-05,  0.1446E-05,  0.1895E-05,  0.2381E-05,  0.2551E-05, &
        0.2071E-05/
      data ((h11(ip,iw),iw=1,31), ip=15,15)/ &
        0.99991643,  0.99984878,  0.99973679,  0.99956077,  0.99929637, &
        0.99891269,  0.99836808,  0.99760133,  0.99651724,  0.99497712, &
        0.99278837,  0.98967999,  0.98528343,  0.97910392,  0.97048581, &
        0.95856780,  0.94222379,  0.91995072,  0.88976622,  0.84931463, &
        0.79618502,  0.72838539,  0.64520276,  0.54832244,  0.44274497, &
        0.33605832,  0.23542911,  0.14604336,  0.07411176,  0.02684504, &
        0.00543809/
      data ((h12(ip,iw),iw=1,31), ip=15,15)/ &
       -0.6810E-07, -0.1377E-06, -0.2556E-06, -0.4445E-06, -0.7154E-06, &
       -0.1075E-05, -0.1499E-05, -0.2014E-05, -0.2759E-05, -0.3932E-05, &
       -0.5740E-05, -0.8407E-05, -0.1213E-04, -0.1719E-04, -0.2405E-04, &
       -0.3332E-04, -0.4580E-04, -0.6272E-04, -0.8612E-04, -0.1188E-03, &
       -0.1633E-03, -0.2210E-03, -0.2895E-03, -0.3617E-03, -0.4311E-03, &
       -0.4937E-03, -0.5413E-03, -0.5503E-03, -0.4826E-03, -0.3261E-03, &
       -0.1467E-03/
      data ((h13(ip,iw),iw=1,31), ip=15,15)/ &
        0.1783E-09,  0.2661E-09,  0.4763E-09,  0.8488E-09,  0.1562E-08, &
        0.2685E-08,  0.4127E-08,  0.5465E-08,  0.6514E-08,  0.7110E-08, &
        0.7674E-08,  0.9292E-08,  0.1364E-07,  0.2094E-07,  0.3012E-07, &
        0.3983E-07,  0.4977E-07,  0.6293E-07,  0.8733E-07,  0.1321E-06, &
        0.2078E-06,  0.3297E-06,  0.5076E-06,  0.7436E-06,  0.1020E-05, &
        0.1330E-05,  0.1735E-05,  0.2236E-05,  0.2557E-05,  0.2309E-05, &
        0.1429E-05/
      data ((h11(ip,iw),iw=1,31), ip=16,16)/ &
        0.99991357,  0.99983984,  0.99971312,  0.99950707,  0.99918896, &
        0.99871838,  0.99804187,  0.99708277,  0.99572664,  0.99380535, &
        0.99107963,  0.98721790,  0.98176694,  0.97412360,  0.96349198, &
        0.94883859,  0.92879146,  0.90152681,  0.86478388,  0.81612718, &
        0.75335342,  0.67520702,  0.58244193,  0.47892416,  0.37162650, &
        0.26823258,  0.17444962,  0.09576660,  0.03960651,  0.01011747, &
        0.00102341/
      data ((h12(ip,iw),iw=1,31), ip=16,16)/ &
       -0.7123E-07, -0.1392E-06, -0.2623E-06, -0.4614E-06, -0.7631E-06, &
       -0.1175E-05, -0.1687E-05, -0.2338E-05, -0.3251E-05, -0.4638E-05, &
       -0.6743E-05, -0.9820E-05, -0.1416E-04, -0.2017E-04, -0.2841E-04, &
       -0.3961E-04, -0.5484E-04, -0.7580E-04, -0.1050E-03, -0.1453E-03, &
       -0.1987E-03, -0.2642E-03, -0.3362E-03, -0.4071E-03, -0.4727E-03, &
       -0.5273E-03, -0.5534E-03, -0.5157E-03, -0.3859E-03, -0.2025E-03, &
       -0.6303E-04/
      data ((h13(ip,iw),iw=1,31), ip=16,16)/ &
        0.1650E-09,  0.2714E-09,  0.5588E-09,  0.1048E-08,  0.1950E-08, &
        0.3403E-08,  0.5359E-08,  0.7328E-08,  0.9023E-08,  0.1066E-07, &
        0.1267E-07,  0.1573E-07,  0.2106E-07,  0.2879E-07,  0.3714E-07, &
        0.4545E-07,  0.5583E-07,  0.7454E-07,  0.1117E-06,  0.1777E-06, &
        0.2851E-06,  0.4461E-06,  0.6655E-06,  0.9322E-06,  0.1227E-05, &
        0.1590E-05,  0.2072E-05,  0.2499E-05,  0.2472E-05,  0.1771E-05, &
        0.7569E-06/
      data ((h11(ip,iw),iw=1,31), ip=17,17)/ &
        0.99991143,  0.99983215,  0.99969172,  0.99945456,  0.99907631, &
        0.99850315,  0.99766576,  0.99646872,  0.99477261,  0.99237478, &
        0.98898262,  0.98418909,  0.97744209,  0.96801037,  0.95493895, &
        0.93697095,  0.91242504,  0.87915903,  0.83475697,  0.77687484, &
        0.70383525,  0.61560035,  0.51490510,  0.40784776,  0.30227286, &
        0.20463216,  0.12004054,  0.05548018,  0.01713908,  0.00259376, &
        0.00004685/
      data ((h12(ip,iw),iw=1,31), ip=17,17)/ &
       -0.7317E-07, -0.1447E-06, -0.2733E-06, -0.4910E-06, -0.8335E-06, &
       -0.1321E-05, -0.1957E-05, -0.2790E-05, -0.3920E-05, -0.5563E-05, &
       -0.8004E-05, -0.1158E-04, -0.1668E-04, -0.2382E-04, -0.3369E-04, &
       -0.4728E-04, -0.6605E-04, -0.9223E-04, -0.1286E-03, -0.1776E-03, &
       -0.2395E-03, -0.3103E-03, -0.3824E-03, -0.4503E-03, -0.5101E-03, &
       -0.5494E-03, -0.5380E-03, -0.4392E-03, -0.2639E-03, -0.9981E-04, &
       -0.1906E-04/
      data ((h13(ip,iw),iw=1,31), ip=17,17)/ &
        0.1091E-09,  0.2634E-09,  0.5215E-09,  0.1136E-08,  0.2259E-08, &
        0.4055E-08,  0.6583E-08,  0.9470E-08,  0.1237E-07,  0.1527E-07, &
        0.1871E-07,  0.2347E-07,  0.3031E-07,  0.3802E-07,  0.4478E-07, &
        0.5135E-07,  0.6383E-07,  0.9137E-07,  0.1453E-06,  0.2389E-06, &
        0.3848E-06,  0.5883E-06,  0.8449E-06,  0.1131E-05,  0.1463E-05, &
        0.1908E-05,  0.2391E-05,  0.2557E-05,  0.2076E-05,  0.1082E-05, &
        0.2792E-06/
      data ((h11(ip,iw),iw=1,31), ip=18,18)/ &
        0.99990946,  0.99982589,  0.99967337,  0.99940664,  0.99896580, &
        0.99827647,  0.99724770,  0.99575895,  0.99364209,  0.99064904, &
        0.98642915,  0.98048556,  0.97215050,  0.96054798,  0.94452488, &
        0.92253780,  0.89256215,  0.85221606,  0.79907858,  0.73115838, &
        0.64774114,  0.55052960,  0.44456220,  0.33746827,  0.23644471, &
        0.14668953,  0.07444155,  0.02696061,  0.00545985,  0.00031263, &
       -0.00000322/
      data ((h12(ip,iw),iw=1,31), ip=18,18)/ &
       -0.7570E-07, -0.1469E-06, -0.2858E-06, -0.5222E-06, -0.9127E-06, &
       -0.1498E-05, -0.2307E-05, -0.3365E-05, -0.4779E-05, -0.6756E-05, &
       -0.9608E-05, -0.1377E-04, -0.1974E-04, -0.2818E-04, -0.3999E-04, &
       -0.5657E-04, -0.7991E-04, -0.1126E-03, -0.1574E-03, -0.2154E-03, &
       -0.2842E-03, -0.3569E-03, -0.4269E-03, -0.4905E-03, -0.5395E-03, &
       -0.5500E-03, -0.4833E-03, -0.3269E-03, -0.1471E-03, -0.3641E-04, &
       -0.3430E-05/
      data ((h13(ip,iw),iw=1,31), ip=18,18)/ &
        0.1064E-09,  0.2501E-09,  0.5003E-09,  0.1160E-08,  0.2339E-08, &
        0.4481E-08,  0.7802E-08,  0.1189E-07,  0.1626E-07,  0.2106E-07, &
        0.2641E-07,  0.3327E-07,  0.4151E-07,  0.4900E-07,  0.5392E-07, &
        0.5960E-07,  0.7616E-07,  0.1158E-06,  0.1920E-06,  0.3203E-06, &
        0.5085E-06,  0.7546E-06,  0.1037E-05,  0.1348E-05,  0.1751E-05, &
        0.2249E-05,  0.2568E-05,  0.2318E-05,  0.1434E-05,  0.4850E-06, &
        0.6147E-07/
      data ((h11(ip,iw),iw=1,31), ip=19,19)/ &
        0.99990809,  0.99982101,  0.99965847,  0.99936569,  0.99886382, &
        0.99805009,  0.99680185,  0.99496508,  0.99232858,  0.98859704, &
        0.98334807,  0.97598833,  0.96571696,  0.95148486,  0.93188584, &
        0.90503371,  0.86859643,  0.82008219,  0.75725996,  0.67887831, &
        0.58570331,  0.48164380,  0.37376148,  0.26981497,  0.17552531, &
        0.09638405,  0.03986549,  0.01018095,  0.00102937, -0.00001919, &
        0.00002277/
      data ((h12(ip,iw),iw=1,31), ip=19,19)/ &
       -0.7674E-07, -0.1508E-06, -0.2928E-06, -0.5484E-06, -0.9875E-06, &
       -0.1686E-05, -0.2688E-05, -0.4039E-05, -0.5848E-05, -0.8267E-05, &
       -0.1163E-04, -0.1646E-04, -0.2340E-04, -0.3331E-04, -0.4747E-04, &
       -0.6780E-04, -0.9689E-04, -0.1375E-03, -0.1914E-03, -0.2577E-03, &
       -0.3305E-03, -0.4023E-03, -0.4689E-03, -0.5251E-03, -0.5529E-03, &
       -0.5167E-03, -0.3875E-03, -0.2035E-03, -0.6335E-04, -0.8814E-05, &
       -0.8413E-07/
      data ((h13(ip,iw),iw=1,31), ip=19,19)/ &
        0.8781E-10,  0.2288E-09,  0.5029E-09,  0.1080E-08,  0.2270E-08, &
        0.4718E-08,  0.8643E-08,  0.1400E-07,  0.2073E-07,  0.2828E-07, &
        0.3633E-07,  0.4565E-07,  0.5519E-07,  0.6210E-07,  0.6573E-07, &
        0.7294E-07,  0.9672E-07,  0.1524E-06,  0.2575E-06,  0.4244E-06, &
        0.6566E-06,  0.9373E-06,  0.1241E-05,  0.1605E-05,  0.2086E-05, &
        0.2513E-05,  0.2486E-05,  0.1781E-05,  0.7609E-06,  0.1439E-06, &
        0.4409E-08/
      data ((h11(ip,iw),iw=1,31), ip=20,20)/ &
        0.99990726,  0.99981779,  0.99964726,  0.99933219,  0.99877435, &
        0.99783629,  0.99634784,  0.99410510,  0.99084204,  0.98620075, &
        0.97967792,  0.97057611,  0.95794863,  0.94052535,  0.91658264, &
        0.88388997,  0.83991259,  0.78220361,  0.70903510,  0.62037933, &
        0.51901436,  0.41113460,  0.30474180,  0.20637417,  0.12113369, &
        0.05601943,  0.01731062,  0.00261897,  0.00004733,  0.00001419, &
        0.00001651/
      data ((h12(ip,iw),iw=1,31), ip=20,20)/ &
       -0.7704E-07, -0.1520E-06, -0.2986E-06, -0.5707E-06, -0.1053E-05, &
       -0.1857E-05, -0.3076E-05, -0.4799E-05, -0.7103E-05, -0.1014E-04, &
       -0.1418E-04, -0.1979E-04, -0.2779E-04, -0.3940E-04, -0.5643E-04, &
       -0.8143E-04, -0.1174E-03, -0.1669E-03, -0.2299E-03, -0.3022E-03, &
       -0.3760E-03, -0.4455E-03, -0.5071E-03, -0.5484E-03, -0.5390E-03, &
       -0.4416E-03, -0.2659E-03, -0.1006E-03, -0.1920E-04, -0.9391E-06, &
        0.1401E-06/
      data ((h13(ip,iw),iw=1,31), ip=20,20)/ &
        0.8781E-10,  0.1756E-09,  0.4257E-09,  0.1011E-08,  0.2142E-08, &
        0.4641E-08,  0.9167E-08,  0.1590E-07,  0.2528E-07,  0.3651E-07, &
        0.4899E-07,  0.6118E-07,  0.7186E-07,  0.7842E-07,  0.8338E-07, &
        0.9591E-07,  0.1307E-06,  0.2076E-06,  0.3457E-06,  0.5529E-06, &
        0.8238E-06,  0.1127E-05,  0.1470E-05,  0.1918E-05,  0.2403E-05, &
        0.2573E-05,  0.2091E-05,  0.1091E-05,  0.2815E-06,  0.2033E-07, &
       -0.4366E-10/
      data ((h11(ip,iw),iw=1,31), ip=21,21)/ &
        0.99990672,  0.99981523,  0.99963874,  0.99930596,  0.99869990, &
        0.99764448,  0.99590999,  0.99321270,  0.98921347,  0.98346788, &
        0.97538018,  0.96413982,  0.94863576,  0.92732108,  0.89811796, &
        0.85852122,  0.80592799,  0.73817724,  0.65448236,  0.55657035, &
        0.44957256,  0.34131473,  0.23920792,  0.14851642,  0.07546103, &
        0.02736473,  0.00554627,  0.00031799, -0.00000286,  0.00002152, &
        0.00000876/
      data ((h12(ip,iw),iw=1,31), ip=21,21)/ &
       -0.7734E-07, -0.1536E-06, -0.3040E-06, -0.5882E-06, -0.1108E-05, &
       -0.2001E-05, -0.3437E-05, -0.5570E-05, -0.8502E-05, -0.1235E-04, &
       -0.1733E-04, -0.2393E-04, -0.3315E-04, -0.4675E-04, -0.6728E-04, &
       -0.9789E-04, -0.1417E-03, -0.2003E-03, -0.2711E-03, -0.3466E-03, &
       -0.4196E-03, -0.4860E-03, -0.5378E-03, -0.5508E-03, -0.4862E-03, &
       -0.3302E-03, -0.1489E-03, -0.3686E-04, -0.3463E-05,  0.1353E-06, &
        0.1039E-06/
      data ((h13(ip,iw),iw=1,31), ip=21,21)/ &
        0.6652E-10,  0.1943E-09,  0.4151E-09,  0.9287E-09,  0.1993E-08, &
        0.4430E-08,  0.9050E-08,  0.1718E-07,  0.2926E-07,  0.4521E-07, &
        0.6353E-07,  0.8050E-07,  0.9282E-07,  0.1011E-06,  0.1107E-06, &
        0.1327E-06,  0.1841E-06,  0.2877E-06,  0.4600E-06,  0.7053E-06, &
        0.1002E-05,  0.1332E-05,  0.1749E-05,  0.2255E-05,  0.2581E-05, &
        0.2336E-05,  0.1450E-05,  0.4908E-06,  0.6208E-07,  0.2183E-09, &
        0.1746E-09/
      data ((h11(ip,iw),iw=1,31), ip=22,22)/ &
        0.99990630,  0.99981350,  0.99963254,  0.99928600,  0.99864006, &
        0.99748278,  0.99550837,  0.99233055,  0.98749363,  0.98044109, &
        0.97045708,  0.95660043,  0.93756276,  0.91149014,  0.87598008, &
        0.82836479,  0.76615584,  0.68785858,  0.59415340,  0.48899502, &
        0.37962717,  0.27413034,  0.17846024,  0.09814817,  0.04069227, &
        0.01041925,  0.00105643, -0.00001931,  0.00002301,  0.00001258, &
        0.00000381/
      data ((h12(ip,iw),iw=1,31), ip=22,22)/ &
       -0.7615E-07, -0.1541E-06, -0.3086E-06, -0.6044E-06, -0.1145E-05, &
       -0.2113E-05, -0.3753E-05, -0.6295E-05, -0.9939E-05, -0.1483E-04, &
       -0.2110E-04, -0.2906E-04, -0.3987E-04, -0.5588E-04, -0.8054E-04, &
       -0.1176E-03, -0.1697E-03, -0.2366E-03, -0.3127E-03, -0.3893E-03, &
       -0.4608E-03, -0.5213E-03, -0.5530E-03, -0.5198E-03, -0.3918E-03, &
       -0.2067E-03, -0.6449E-04, -0.8964E-05, -0.8318E-07,  0.1249E-06, &
        0.6531E-07/
      data ((h13(ip,iw),iw=1,31), ip=22,22)/ &
        0.8781E-10,  0.1597E-09,  0.3805E-09,  0.8302E-09,  0.1876E-08, &
        0.3994E-08,  0.8661E-08,  0.1762E-07,  0.3209E-07,  0.5307E-07, &
        0.7873E-07,  0.1030E-06,  0.1203E-06,  0.1335E-06,  0.1518E-06, &
        0.1882E-06,  0.2634E-06,  0.3975E-06,  0.6023E-06,  0.8749E-06, &
        0.1190E-05,  0.1576E-05,  0.2077E-05,  0.2517E-05,  0.2502E-05, &
        0.1802E-05,  0.7732E-06,  0.1464E-06,  0.4540E-08,  0.5821E-10, &
        0.2765E-09/
      data ((h11(ip,iw),iw=1,31), ip=23,23)/ &
        0.99990612,  0.99981230,  0.99962813,  0.99927127,  0.99859446, &
        0.99735069,  0.99515831,  0.99150169,  0.98575759,  0.97720367, &
        0.96496516,  0.94792449,  0.92453283,  0.89265805,  0.84968978, &
        0.79293245,  0.72031248,  0.63150871,  0.52920425,  0.41969413, &
        0.31129104,  0.21094686,  0.12399805,  0.05750990,  0.01784873, &
        0.00271523,  0.00004989,  0.00001466,  0.00001687,  0.00000602, &
        0.00000149/
      data ((h12(ip,iw),iw=1,31), ip=23,23)/ &
       -0.7510E-07, -0.1554E-06, -0.3119E-06, -0.6119E-06, -0.1173E-05, &
       -0.2207E-05, -0.4008E-05, -0.6933E-05, -0.1132E-04, -0.1743E-04, &
       -0.2536E-04, -0.3530E-04, -0.4838E-04, -0.6748E-04, -0.9692E-04, &
       -0.1409E-03, -0.2009E-03, -0.2740E-03, -0.3528E-03, -0.4297E-03, &
       -0.4985E-03, -0.5459E-03, -0.5414E-03, -0.4466E-03, -0.2707E-03, &
       -0.1029E-03, -0.1967E-04, -0.9582E-06,  0.1435E-06,  0.8580E-07, &
        0.3337E-07/
      data ((h13(ip,iw),iw=1,31), ip=23,23)/ &
        0.1064E-09,  0.1623E-09,  0.3539E-09,  0.7823E-09,  0.1660E-08, &
        0.3667E-08,  0.8103E-08,  0.1715E-07,  0.3355E-07,  0.5870E-07, &
        0.9224E-07,  0.1272E-06,  0.1560E-06,  0.1799E-06,  0.2109E-06, &
        0.2682E-06,  0.3729E-06,  0.5382E-06,  0.7697E-06,  0.1055E-05, &
        0.1407E-05,  0.1882E-05,  0.2393E-05,  0.2581E-05,  0.2113E-05, &
        0.1111E-05,  0.2881E-06,  0.2072E-07,  0.0000E+00,  0.2619E-09, &
        0.1892E-09/
      data ((h11(ip,iw),iw=1,31), ip=24,24)/ &
        0.99990606,  0.99981177,  0.99962461,  0.99926001,  0.99856067, &
        0.99724674,  0.99486804,  0.99076360,  0.98409128,  0.97387761, &
        0.95901638,  0.93813980,  0.90942395,  0.87054116,  0.81886047, &
        0.75190943,  0.66845155,  0.56990325,  0.46134251,  0.35079926, &
        0.24611366,  0.15298080,  0.07791620,  0.02838999,  0.00579512, &
        0.00033593, -0.00000381,  0.00002211,  0.00000912,  0.00000226, &
        0.00000042/
      data ((h12(ip,iw),iw=1,31), ip=24,24)/ &
       -0.7555E-07, -0.1559E-06, -0.3164E-06, -0.6169E-06, -0.1193E-05, &
       -0.2283E-05, -0.4214E-05, -0.7461E-05, -0.1256E-04, -0.1997E-04, &
       -0.2988E-04, -0.4252E-04, -0.5899E-04, -0.8230E-04, -0.1172E-03, &
       -0.1679E-03, -0.2343E-03, -0.3111E-03, -0.3912E-03, -0.4678E-03, &
       -0.5294E-03, -0.5504E-03, -0.4910E-03, -0.3362E-03, -0.1527E-03, &
       -0.3800E-04, -0.3577E-05,  0.1377E-06,  0.1068E-06,  0.4910E-07, &
        0.1382E-07/
      data ((h13(ip,iw),iw=1,31), ip=24,24)/ &
        0.1038E-09,  0.1277E-09,  0.4125E-09,  0.7451E-09,  0.1450E-08, &
        0.3356E-08,  0.7456E-08,  0.1617E-07,  0.3282E-07,  0.6125E-07, &
        0.1022E-06,  0.1504E-06,  0.1982E-06,  0.2422E-06,  0.2936E-06, &
        0.3789E-06,  0.5157E-06,  0.7110E-06,  0.9579E-06,  0.1260E-05, &
        0.1686E-05,  0.2227E-05,  0.2583E-05,  0.2355E-05,  0.1476E-05, &
        0.5046E-06,  0.6422E-07,  0.2183E-09,  0.1892E-09,  0.2910E-09, &
        0.1164E-09/
      data ((h11(ip,iw),iw=1,31), ip=25,25)/ &
        0.99990606,  0.99981093,  0.99962217,  0.99925297,  0.99853528, &
        0.99716783,  0.99463624,  0.99013960,  0.98257101,  0.97060472, &
        0.95276862,  0.92735082,  0.89223903,  0.84502023,  0.78329539, &
        0.70526958,  0.61107880,  0.50439918,  0.39259118,  0.28406858, &
        0.18521482,  0.10203111,  0.04244447,  0.01094002,  0.00112236, &
       -0.00002146,  0.00002360,  0.00001270,  0.00000411,  0.00000089, &
        0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=25,25)/ &
       -0.7704E-07, -0.1603E-06, -0.3171E-06, -0.6175E-06, -0.1218E-05, &
       -0.2343E-05, -0.4376E-05, -0.7897E-05, -0.1364E-04, -0.2227E-04, &
       -0.3436E-04, -0.5040E-04, -0.7169E-04, -0.1009E-03, -0.1423E-03, &
       -0.1989E-03, -0.2693E-03, -0.3478E-03, -0.4288E-03, -0.5023E-03, &
       -0.5464E-03, -0.5224E-03, -0.3985E-03, -0.2120E-03, -0.6665E-04, &
       -0.9332E-05, -0.9177E-07,  0.1277E-06,  0.6769E-07,  0.2288E-07, &
        0.4290E-08/
      data ((h13(ip,iw),iw=1,31), ip=25,25)/ &
        0.1144E-09,  0.1703E-09,  0.3832E-09,  0.6280E-09,  0.1424E-08, &
        0.3124E-08,  0.6708E-08,  0.1467E-07,  0.3071E-07,  0.6037E-07, &
        0.1074E-06,  0.1704E-06,  0.2433E-06,  0.3199E-06,  0.4057E-06, &
        0.5253E-06,  0.6960E-06,  0.9149E-06,  0.1173E-05,  0.1524E-05, &
        0.2039E-05,  0.2519E-05,  0.2526E-05,  0.1833E-05,  0.7949E-06, &
        0.1526E-06,  0.4729E-08,  0.1164E-09,  0.2619E-09,  0.1455E-09, &
        0.5821E-10/
      data ((h11(ip,iw),iw=1,31), ip=26,26)/ &
        0.99990594,  0.99981046,  0.99962056,  0.99924737,  0.99851662, &
        0.99710888,  0.99445754,  0.98962957,  0.98124003,  0.96750849, &
        0.94641417,  0.91576242,  0.87315178,  0.81620890,  0.74311233, &
        0.65339649,  0.54924417,  0.43682033,  0.32487178,  0.22071731, &
        0.13000739,  0.06041300,  0.01882440,  0.00288731,  0.00005370, &
        0.00001419,  0.00001746,  0.00000632,  0.00000137,  0.00000006, &
       -0.00000012/
      data ((h12(ip,iw),iw=1,31), ip=26,26)/ &
       -0.7689E-07, -0.1593E-06, -0.3161E-06, -0.6281E-06, -0.1239E-05, &
       -0.2391E-05, -0.4505E-05, -0.8258E-05, -0.1454E-04, -0.2433E-04, &
       -0.3865E-04, -0.5869E-04, -0.8621E-04, -0.1235E-03, -0.1726E-03, &
       -0.2342E-03, -0.3066E-03, -0.3861E-03, -0.4664E-03, -0.5295E-03, &
       -0.5394E-03, -0.4532E-03, -0.2778E-03, -0.1064E-03, -0.2051E-04, &
       -0.1033E-05,  0.1473E-06,  0.8866E-07,  0.3480E-07,  0.8103E-08, &
        0.9529E-09/
      data ((h13(ip,iw),iw=1,31), ip=26,26)/ &
        0.1011E-09,  0.2209E-09,  0.3699E-09,  0.6360E-09,  0.1354E-08, &
        0.2815E-08,  0.6195E-08,  0.1309E-07,  0.2811E-07,  0.5739E-07, &
        0.1090E-06,  0.1874E-06,  0.2891E-06,  0.4110E-06,  0.5510E-06, &
        0.7154E-06,  0.9190E-06,  0.1150E-05,  0.1436E-05,  0.1878E-05, &
        0.2409E-05,  0.2625E-05,  0.2158E-05,  0.1142E-05,  0.3004E-06, &
        0.2224E-07, -0.7276E-10,  0.2328E-09,  0.2328E-09,  0.8731E-10, &
        0.4366E-10/
      data ((h21(ip,iw),iw=1,31), ip= 1, 1)/ &
        0.99999648,  0.99999303,  0.99998641,  0.99997419,  0.99995244, &
        0.99991620,  0.99986106,  0.99978638,  0.99969482,  0.99958766, &
        0.99946332,  0.99931884,  0.99915075,  0.99895716,  0.99873149, &
        0.99845850,  0.99811709,  0.99768430,  0.99713957,  0.99645668, &
        0.99560016,  0.99451423,  0.99310941,  0.99126315,  0.98881638, &
        0.98555976,  0.98121214,  0.97538328,  0.96752322,  0.95687479, &
        0.94245565/
      data ((h22(ip,iw),iw=1,31), ip= 1, 1)/ &
       -0.4753E-07, -0.9491E-07, -0.1790E-06, -0.3272E-06, -0.5677E-06, &
       -0.9265E-06, -0.1394E-05, -0.1949E-05, -0.2586E-05, -0.3328E-05, &
       -0.4201E-05, -0.5223E-05, -0.6410E-05, -0.7839E-05, -0.9638E-05, &
       -0.1196E-04, -0.1494E-04, -0.1863E-04, -0.2308E-04, -0.2844E-04, &
       -0.3494E-04, -0.4296E-04, -0.5306E-04, -0.6601E-04, -0.8270E-04, &
       -0.1043E-03, -0.1323E-03, -0.1693E-03, -0.2183E-03, -0.2833E-03, &
       -0.3683E-03/
      data ((h23(ip,iw),iw=1,31), ip= 1, 1)/ &
       -0.1996E-09, -0.3752E-09, -0.6466E-09, -0.1096E-08, -0.1634E-08, &
       -0.2123E-08, -0.2222E-08, -0.2086E-08, -0.2493E-08, -0.3858E-08, &
       -0.5729E-08, -0.7400E-08, -0.9058E-08, -0.1205E-07, -0.1729E-07, &
       -0.2405E-07, -0.3157E-07, -0.3950E-07, -0.4812E-07, -0.5742E-07, &
       -0.6745E-07, -0.7800E-07, -0.8757E-07, -0.9509E-07, -0.1011E-06, &
       -0.1071E-06, -0.1165E-06, -0.1326E-06, -0.1556E-06, -0.1798E-06, &
       -0.1961E-06/
      data ((h21(ip,iw),iw=1,31), ip= 2, 2)/ &
        0.99999648,  0.99999303,  0.99998641,  0.99997419,  0.99995244, &
        0.99991614,  0.99986106,  0.99978626,  0.99969459,  0.99958706, &
        0.99946231,  0.99931693,  0.99914742,  0.99895126,  0.99872077, &
        0.99843901,  0.99808180,  0.99762255,  0.99703413,  0.99628311, &
        0.99532354,  0.99408531,  0.99245912,  0.99029195,  0.98738796, &
        0.98349059,  0.97824448,  0.97115874,  0.96155798,  0.94853783, &
        0.93095934/
      data ((h22(ip,iw),iw=1,31), ip= 2, 2)/ &
       -0.4753E-07, -0.9491E-07, -0.1790E-06, -0.3272E-06, -0.5677E-06, &
       -0.9265E-06, -0.1395E-05, -0.1951E-05, -0.2588E-05, -0.3330E-05, &
       -0.4208E-05, -0.5236E-05, -0.6432E-05, -0.7880E-05, -0.9713E-05, &
       -0.1210E-04, -0.1518E-04, -0.1905E-04, -0.2377E-04, -0.2955E-04, &
       -0.3669E-04, -0.4564E-04, -0.5713E-04, -0.7205E-04, -0.9151E-04, &
       -0.1170E-03, -0.1504E-03, -0.1947E-03, -0.2535E-03, -0.3311E-03, &
       -0.4319E-03/
      data ((h23(ip,iw),iw=1,31), ip= 2, 2)/ &
       -0.1996E-09, -0.3752E-09, -0.6466E-09, -0.1096E-08, -0.1634E-08, &
       -0.2113E-08, -0.2227E-08, -0.2089E-08, -0.2491E-08, -0.3853E-08, &
       -0.5732E-08, -0.7379E-08, -0.9036E-08, -0.1205E-07, -0.1733E-07, &
       -0.2407E-07, -0.3154E-07, -0.3943E-07, -0.4797E-07, -0.5722E-07, &
       -0.6733E-07, -0.7812E-07, -0.8799E-07, -0.9548E-07, -0.1010E-06, &
       -0.1072E-06, -0.1168E-06, -0.1326E-06, -0.1535E-06, -0.1719E-06, &
       -0.1776E-06/
      data ((h21(ip,iw),iw=1,31), ip= 3, 3)/ &
        0.99999648,  0.99999303,  0.99998641,  0.99997419,  0.99995250, &
        0.99991614,  0.99986094,  0.99978608,  0.99969417,  0.99958628, &
        0.99946100,  0.99931419,  0.99914253,  0.99894208,  0.99870420, &
        0.99840903,  0.99802876,  0.99753171,  0.99688333,  0.99604201, &
        0.99494857,  0.99351460,  0.99160337,  0.98902792,  0.98555082, &
        0.98084849,  0.97447640,  0.96582508,  0.95407653,  0.93817306, &
        0.91681957/
      data ((h22(ip,iw),iw=1,31), ip= 3, 3)/ &
       -0.4753E-07, -0.9491E-07, -0.1790E-06, -0.3275E-06, -0.5677E-06, &
       -0.9271E-06, -0.1396E-05, -0.1952E-05, -0.2591E-05, -0.3335E-05, &
       -0.4219E-05, -0.5255E-05, -0.6467E-05, -0.7944E-05, -0.9828E-05, &
       -0.1230E-04, -0.1554E-04, -0.1964E-04, -0.2474E-04, -0.3107E-04, &
       -0.3903E-04, -0.4920E-04, -0.6245E-04, -0.7987E-04, -0.1028E-03, &
       -0.1331E-03, -0.1731E-03, -0.2264E-03, -0.2969E-03, -0.3894E-03, &
       -0.5080E-03/
      data ((h23(ip,iw),iw=1,31), ip= 3, 3)/ &
       -0.1996E-09, -0.3752E-09, -0.6466E-09, -0.1102E-08, -0.1644E-08, &
       -0.2123E-08, -0.2219E-08, -0.2097E-08, -0.2496E-08, -0.3845E-08, &
       -0.5750E-08, -0.7376E-08, -0.9092E-08, -0.1209E-07, -0.1734E-07, &
       -0.2409E-07, -0.3143E-07, -0.3929E-07, -0.4763E-07, -0.5693E-07, &
       -0.6739E-07, -0.7864E-07, -0.8851E-07, -0.9552E-07, -0.1012E-06, &
       -0.1075E-06, -0.1173E-06, -0.1320E-06, -0.1485E-06, -0.1582E-06, &
       -0.1486E-06/
      data ((h21(ip,iw),iw=1,31), ip= 4, 4)/ &
        0.99999648,  0.99999303,  0.99998635,  0.99997413,  0.99995244, &
        0.99991614,  0.99986082,  0.99978578,  0.99969357,  0.99958509, &
        0.99945861,  0.99930990,  0.99913460,  0.99892777,  0.99867845, &
        0.99836367,  0.99795061,  0.99740112,  0.99667352,  0.99571395, &
        0.99444747,  0.99276096,  0.99048626,  0.98739773,  0.98319960, &
        0.97748899,  0.96971005,  0.95911890,  0.94474679,  0.92537642, &
        0.89957643/
      data ((h22(ip,iw),iw=1,31), ip= 4, 4)/ &
       -0.4753E-07, -0.9491E-07, -0.1790E-06, -0.3280E-06, -0.5680E-06, &
       -0.9276E-06, -0.1398E-05, -0.1955E-05, -0.2597E-05, -0.3345E-05, &
       -0.4235E-05, -0.5284E-05, -0.6523E-05, -0.8043E-05, -0.1000E-04, &
       -0.1261E-04, -0.1605E-04, -0.2048E-04, -0.2606E-04, -0.3312E-04, &
       -0.4216E-04, -0.5390E-04, -0.6938E-04, -0.8992E-04, -0.1172E-03, &
       -0.1534E-03, -0.2016E-03, -0.2657E-03, -0.3500E-03, -0.4594E-03, &
       -0.5976E-03/
      data ((h23(ip,iw),iw=1,31), ip= 4, 4)/ &
       -0.1996E-09, -0.3752E-09, -0.6413E-09, -0.1094E-08, -0.1639E-08, &
       -0.2131E-08, -0.2227E-08, -0.2105E-08, -0.2493E-08, -0.3872E-08, &
       -0.5758E-08, -0.7389E-08, -0.9106E-08, -0.1213E-07, -0.1736E-07, &
       -0.2402E-07, -0.3126E-07, -0.3893E-07, -0.4733E-07, -0.5693E-07, &
       -0.6786E-07, -0.7916E-07, -0.8873E-07, -0.9564E-07, -0.1015E-06, &
       -0.1083E-06, -0.1176E-06, -0.1292E-06, -0.1387E-06, -0.1360E-06, &
       -0.1038E-06/
      data ((h21(ip,iw),iw=1,31), ip= 5, 5)/ &
        0.99999648,  0.99999303,  0.99998635,  0.99997419,  0.99995244, &
        0.99991602,  0.99986064,  0.99978524,  0.99969250,  0.99958307, &
        0.99945492,  0.99930298,  0.99912232,  0.99890554,  0.99863952, &
        0.99829626,  0.99783719,  0.99721783,  0.99638581,  0.99527395, &
        0.99378401,  0.99177408,  0.98903996,  0.98530644,  0.98020452, &
        0.97323179,  0.96370512,  0.95073187,  0.93318701,  0.90970320, &
        0.87876439/
      data ((h22(ip,iw),iw=1,31), ip= 5, 5)/ &
       -0.4753E-07, -0.9491E-07, -0.1790E-06, -0.3274E-06, -0.5683E-06, &
       -0.9282E-06, -0.1399E-05, -0.1960E-05, -0.2603E-05, -0.3360E-05, &
       -0.4261E-05, -0.5332E-05, -0.6607E-05, -0.8193E-05, -0.1027E-04, &
       -0.1305E-04, -0.1679E-04, -0.2164E-04, -0.2786E-04, -0.3587E-04, &
       -0.4629E-04, -0.6004E-04, -0.7832E-04, -0.1028E-03, -0.1354E-03, &
       -0.1790E-03, -0.2371E-03, -0.3140E-03, -0.4143E-03, -0.5425E-03, &
       -0.7012E-03/
      data ((h23(ip,iw),iw=1,31), ip= 5, 5)/ &
       -0.1996E-09, -0.3752E-09, -0.6413E-09, -0.1104E-08, -0.1634E-08, &
       -0.2115E-08, -0.2240E-08, -0.2107E-08, -0.2480E-08, -0.3866E-08, &
       -0.5795E-08, -0.7432E-08, -0.9138E-08, -0.1213E-07, -0.1734E-07, &
       -0.2391E-07, -0.3090E-07, -0.3848E-07, -0.4710E-07, -0.5731E-07, &
       -0.6849E-07, -0.7959E-07, -0.8877E-07, -0.9590E-07, -0.1021E-06, &
       -0.1090E-06, -0.1167E-06, -0.1228E-06, -0.1222E-06, -0.1012E-06, &
       -0.3644E-07/
      data ((h21(ip,iw),iw=1,31), ip= 6, 6)/ &
        0.99999648,  0.99999303,  0.99998641,  0.99997413,  0.99995238, &
        0.99991590,  0.99986023,  0.99978453,  0.99969083,  0.99957997, &
        0.99944901,  0.99929231,  0.99910325,  0.99887222,  0.99858135, &
        0.99819821,  0.99767721,  0.99696583,  0.99599892,  0.99468935, &
        0.99291271,  0.99049187,  0.98718077,  0.98263699,  0.97640336, &
        0.96785825,  0.95617527,  0.94030702,  0.91897225,  0.89068717, &
        0.85394704/
      data ((h22(ip,iw),iw=1,31), ip= 6, 6)/ &
       -0.4753E-07, -0.9491E-07, -0.1790E-06, -0.3280E-06, -0.5690E-06, &
       -0.9295E-06, -0.1403E-05, -0.1966E-05, -0.2616E-05, -0.3382E-05, &
       -0.4303E-05, -0.5405E-05, -0.6736E-05, -0.8420E-05, -0.1065E-04, &
       -0.1370E-04, -0.1780E-04, -0.2321E-04, -0.3027E-04, -0.3951E-04, &
       -0.5172E-04, -0.6798E-04, -0.8977E-04, -0.1191E-03, -0.1584E-03, &
       -0.2110E-03, -0.2810E-03, -0.3728E-03, -0.4911E-03, -0.6396E-03, &
       -0.8183E-03/
      data ((h23(ip,iw),iw=1,31), ip= 6, 6)/ &
       -0.1996E-09, -0.3752E-09, -0.6466E-09, -0.1110E-08, -0.1636E-08, &
       -0.2139E-08, -0.2251E-08, -0.2107E-08, -0.2493E-08, -0.3885E-08, &
       -0.5790E-08, -0.7464E-08, -0.9188E-08, -0.1225E-07, -0.1736E-07, &
       -0.2372E-07, -0.3045E-07, -0.3811E-07, -0.4728E-07, -0.5796E-07, &
       -0.6918E-07, -0.7976E-07, -0.8909E-07, -0.9641E-07, -0.1029E-06, &
       -0.1093E-06, -0.1130E-06, -0.1109E-06, -0.9583E-07, -0.4794E-07, &
        0.5993E-07/
      data ((h21(ip,iw),iw=1,31), ip= 7, 7)/ &
        0.99999654,  0.99999303,  0.99998629,  0.99997413,  0.99995220, &
        0.99991560,  0.99985969,  0.99978322,  0.99968827,  0.99957514, &
        0.99944001,  0.99927598,  0.99907416,  0.99882185,  0.99849612, &
        0.99805862,  0.99745625,  0.99662507,  0.99548388,  0.99392039, &
        0.99177766,  0.98884010,  0.98480314,  0.97924423,  0.97159582, &
        0.96110350,  0.94678706,  0.92743945,  0.90164268,  0.86787140, &
        0.82476485/
      data ((h22(ip,iw),iw=1,31), ip= 7, 7)/ &
       -0.4723E-07, -0.9491E-07, -0.1798E-06, -0.3280E-06, -0.5701E-06, &
       -0.9323E-06, -0.1409E-05, -0.1978E-05, -0.2635E-05, -0.3418E-05, &
       -0.4367E-05, -0.5518E-05, -0.6930E-05, -0.8753E-05, -0.1121E-04, &
       -0.1458E-04, -0.1919E-04, -0.2533E-04, -0.3347E-04, -0.4430E-04, &
       -0.5877E-04, -0.7818E-04, -0.1043E-03, -0.1397E-03, -0.1872E-03, &
       -0.2507E-03, -0.3346E-03, -0.4435E-03, -0.5816E-03, -0.7506E-03, &
       -0.9473E-03/
      data ((h23(ip,iw),iw=1,31), ip= 7, 7)/ &
       -0.2049E-09, -0.3752E-09, -0.6466E-09, -0.1104E-08, -0.1623E-08, &
       -0.2126E-08, -0.2243E-08, -0.2099E-08, -0.2475E-08, -0.3888E-08, &
       -0.5827E-08, -0.7498E-08, -0.9209E-08, -0.1224E-07, -0.1718E-07, &
       -0.2327E-07, -0.2999E-07, -0.3801E-07, -0.4790E-07, -0.5886E-07, &
       -0.6964E-07, -0.8007E-07, -0.8958E-07, -0.9726E-07, -0.1035E-06, &
       -0.1072E-06, -0.1047E-06, -0.9110E-07, -0.5435E-07,  0.3110E-07, &
        0.1879E-06/
      data ((h21(ip,iw),iw=1,31), ip= 8, 8)/ &
        0.99999648,  0.99999303,  0.99998629,  0.99997407,  0.99995214, &
        0.99991530,  0.99985880,  0.99978131,  0.99968427,  0.99956757, &
        0.99942595,  0.99925089,  0.99903029,  0.99874794,  0.99837416, &
        0.99786478,  0.99715632,  0.99617100,  0.99480504,  0.99291641, &
        0.99031132,  0.98672378,  0.98177612,  0.97494662,  0.96554160, &
        0.95265889,  0.93515933,  0.91168392,  0.88073176,  0.84085232, &
        0.79097253/
      data ((h22(ip,iw),iw=1,31), ip= 8, 8)/ &
       -0.4753E-07, -0.9491E-07, -0.1798E-06, -0.3284E-06, -0.5731E-06, &
       -0.9359E-06, -0.1417E-05, -0.1995E-05, -0.2666E-05, -0.3474E-05, &
       -0.4466E-05, -0.5689E-05, -0.7221E-05, -0.9235E-05, -0.1199E-04, &
       -0.1580E-04, -0.2105E-04, -0.2816E-04, -0.3771E-04, -0.5056E-04, &
       -0.6786E-04, -0.9121E-04, -0.1228E-03, -0.1656E-03, -0.2231E-03, &
       -0.2995E-03, -0.3994E-03, -0.5274E-03, -0.6861E-03, -0.8744E-03, &
       -0.1086E-02/
      data ((h23(ip,iw),iw=1,31), ip= 8, 8)/ &
       -0.1996E-09, -0.3752E-09, -0.6466E-09, -0.1096E-08, -0.1644E-08, &
       -0.2137E-08, -0.2251E-08, -0.2110E-08, -0.2475E-08, -0.3920E-08, &
       -0.5873E-08, -0.7546E-08, -0.9215E-08, -0.1222E-07, -0.1688E-07, &
       -0.2277E-07, -0.2967E-07, -0.3849E-07, -0.4889E-07, -0.5966E-07, &
       -0.7002E-07, -0.8063E-07, -0.9037E-07, -0.9801E-07, -0.1026E-06, &
       -0.1016E-06, -0.8995E-07, -0.5920E-07,  0.9117E-08,  0.1409E-06, &
        0.3365E-06/
      data ((h21(ip,iw),iw=1,31), ip= 9, 9)/ &
        0.99999642,  0.99999303,  0.99998623,  0.99997389,  0.99995190, &
        0.99991465,  0.99985743,  0.99977821,  0.99967796,  0.99955577, &
        0.99940449,  0.99921286,  0.99896592,  0.99864131,  0.99820441, &
        0.99760073,  0.99675626,  0.99557197,  0.99391770,  0.99161607, &
        0.98842955,  0.98402578,  0.97793722,  0.96952480,  0.95795447, &
        0.94216752,  0.92086565,  0.89257455,  0.85580462,  0.80932784, &
        0.75241166/
      data ((h22(ip,iw),iw=1,31), ip= 9, 9)/ &
       -0.4843E-07, -0.9581E-07, -0.1804E-06, -0.3297E-06, -0.5752E-06, &
       -0.9416E-06, -0.1430E-05, -0.2021E-05, -0.2717E-05, -0.3559E-05, &
       -0.4614E-05, -0.5946E-05, -0.7641E-05, -0.9914E-05, -0.1305E-04, &
       -0.1745E-04, -0.2355E-04, -0.3191E-04, -0.4326E-04, -0.5865E-04, &
       -0.7950E-04, -0.1078E-03, -0.1461E-03, -0.1980E-03, -0.2674E-03, &
       -0.3588E-03, -0.4770E-03, -0.6253E-03, -0.8043E-03, -0.1009E-02, &
       -0.1233E-02/
      data ((h23(ip,iw),iw=1,31), ip= 9, 9)/ &
       -0.1623E-09, -0.3752E-09, -0.6466E-09, -0.1094E-08, -0.1676E-08, &
       -0.2131E-08, -0.2248E-08, -0.2102E-08, -0.2440E-08, -0.3941E-08, &
       -0.5913E-08, -0.7530E-08, -0.9207E-08, -0.1200E-07, -0.1646E-07, &
       -0.2222E-07, -0.2990E-07, -0.3952E-07, -0.5004E-07, -0.6039E-07, &
       -0.7090E-07, -0.8171E-07, -0.9142E-07, -0.9799E-07, -0.9897E-07, &
       -0.9060E-07, -0.6537E-07, -0.9170E-08,  0.1011E-06,  0.2785E-06, &
        0.4786E-06/
      data ((h21(ip,iw),iw=1,31), ip=10,10)/ &
        0.99999642,  0.99999303,  0.99998623,  0.99997383,  0.99995154, &
        0.99991393,  0.99985534,  0.99977356,  0.99966878,  0.99953789, &
        0.99937212,  0.99915701,  0.99887317,  0.99849242,  0.99797267, &
        0.99724811,  0.99622810,  0.99478859,  0.99276686,  0.98994553, &
        0.98602796,  0.98060095,  0.97308666,  0.96271574,  0.94850242, &
        0.92922503,  0.90344882,  0.86965692,  0.82650536,  0.77310455, &
        0.70888364/
      data ((h22(ip,iw),iw=1,31), ip=10,10)/ &
       -0.4902E-07, -0.9610E-07, -0.1818E-06, -0.3318E-06, -0.5793E-06, &
       -0.9508E-06, -0.1451E-05, -0.2061E-05, -0.2790E-05, -0.3690E-05, &
       -0.4841E-05, -0.6317E-05, -0.8240E-05, -0.1085E-04, -0.1450E-04, &
       -0.1965E-04, -0.2686E-04, -0.3682E-04, -0.5045E-04, -0.6903E-04, &
       -0.9430E-04, -0.1287E-03, -0.1753E-03, -0.2381E-03, -0.3215E-03, &
       -0.4302E-03, -0.5682E-03, -0.7372E-03, -0.9345E-03, -0.1153E-02, &
       -0.1387E-02/
      data ((h23(ip,iw),iw=1,31), ip=10,10)/ &
       -0.1730E-09, -0.3805E-09, -0.6493E-09, -0.1110E-08, -0.1682E-08, &
       -0.2174E-08, -0.2259E-08, -0.2107E-08, -0.2512E-08, -0.3949E-08, &
       -0.5913E-08, -0.7512E-08, -0.9071E-08, -0.1165E-07, -0.1597E-07, &
       -0.2214E-07, -0.3079E-07, -0.4090E-07, -0.5119E-07, -0.6148E-07, &
       -0.7225E-07, -0.8328E-07, -0.9212E-07, -0.9568E-07, -0.9071E-07, &
       -0.7143E-07, -0.2598E-07,  0.6630E-07,  0.2241E-06,  0.4256E-06, &
        0.5851E-06/
      data ((h21(ip,iw),iw=1,31), ip=11,11)/ &
        0.99999642,  0.99999303,  0.99998617,  0.99997371,  0.99995112, &
        0.99991250,  0.99985224,  0.99976677,  0.99965459,  0.99951136, &
        0.99932498,  0.99907678,  0.99874300,  0.99828905,  0.99766219, &
        0.99678314,  0.99553728,  0.99377203,  0.99128604,  0.98781121, &
        0.98297691,  0.97626954,  0.96698701,  0.95421380,  0.93680686, &
        0.91339087,  0.88244677,  0.84253281,  0.79259300,  0.73202771, &
        0.66004348/
      data ((h22(ip,iw),iw=1,31), ip=11,11)/ &
       -0.4932E-07, -0.9760E-07, -0.1833E-06, -0.3341E-06, -0.5857E-06, &
       -0.9661E-06, -0.1485E-05, -0.2126E-05, -0.2905E-05, -0.3887E-05, &
       -0.5173E-05, -0.6847E-05, -0.9066E-05, -0.1212E-04, -0.1643E-04, &
       -0.2256E-04, -0.3120E-04, -0.4320E-04, -0.5970E-04, -0.8227E-04, &
       -0.1131E-03, -0.1550E-03, -0.2116E-03, -0.2874E-03, -0.3870E-03, &
       -0.5148E-03, -0.6736E-03, -0.8622E-03, -0.1075E-02, -0.1304E-02, &
       -0.1547E-02/
      data ((h23(ip,iw),iw=1,31), ip=11,11)/ &
       -0.1676E-09, -0.3752E-09, -0.6546E-09, -0.1118E-08, -0.1700E-08, &
       -0.2193E-08, -0.2331E-08, -0.2187E-08, -0.2509E-08, -0.3954E-08, &
       -0.5931E-08, -0.7411E-08, -0.8717E-08, -0.1116E-07, -0.1561E-07, &
       -0.2272E-07, -0.3217E-07, -0.4240E-07, -0.5241E-07, -0.6315E-07, &
       -0.7433E-07, -0.8486E-07, -0.9147E-07, -0.8991E-07, -0.7605E-07, &
       -0.4043E-07,  0.3517E-07,  0.1721E-06,  0.3674E-06,  0.5493E-06, &
        0.6604E-06/
      data ((h21(ip,iw),iw=1,31), ip=12,12)/ &
        0.99999636,  0.99999297,  0.99998611,  0.99997342,  0.99995035, &
        0.99991083,  0.99984789,  0.99975663,  0.99963427,  0.99947309, &
        0.99925774,  0.99896514,  0.99856573,  0.99801725,  0.99725395, &
        0.99617636,  0.99464226,  0.99246347,  0.98939490,  0.98509979, &
        0.97911763,  0.97081727,  0.95935690,  0.94366837,  0.92244881, &
        0.89420575,  0.85743225,  0.81090724,  0.75391734,  0.68583673, &
        0.60547698/
      data ((h22(ip,iw),iw=1,31), ip=12,12)/ &
       -0.4917E-07, -0.9789E-07, -0.1846E-06, -0.3387E-06, -0.5948E-06, &
       -0.9898E-06, -0.1532E-05, -0.2218E-05, -0.3075E-05, -0.4181E-05, &
       -0.5647E-05, -0.7589E-05, -0.1019E-04, -0.1382E-04, -0.1901E-04, &
       -0.2641E-04, -0.3686E-04, -0.5143E-04, -0.7152E-04, -0.9907E-04, &
       -0.1367E-03, -0.1878E-03, -0.2564E-03, -0.3473E-03, -0.4652E-03, &
       -0.6135E-03, -0.7927E-03, -0.9982E-03, -0.1223E-02, -0.1462E-02, &
       -0.1710E-02/
      data ((h23(ip,iw),iw=1,31), ip=12,12)/ &
       -0.1597E-09, -0.3752E-09, -0.6679E-09, -0.1131E-08, -0.1708E-08, &
       -0.2243E-08, -0.2440E-08, -0.2227E-08, -0.2573E-08, -0.3997E-08, &
       -0.5902E-08, -0.7208E-08, -0.8220E-08, -0.1064E-07, -0.1580E-07, &
       -0.2405E-07, -0.3396E-07, -0.4384E-07, -0.5434E-07, -0.6555E-07, &
       -0.7680E-07, -0.8582E-07, -0.8796E-07, -0.7900E-07, -0.5180E-07, &
        0.8584E-08,  0.1245E-06,  0.3056E-06,  0.5039E-06,  0.6330E-06, &
        0.7692E-06/
      data ((h21(ip,iw),iw=1,31), ip=13,13)/ &
        0.99999636,  0.99999279,  0.99998599,  0.99997294,  0.99994928, &
        0.99990821,  0.99984163,  0.99974293,  0.99960572,  0.99941981, &
        0.99916512,  0.99881363,  0.99832994,  0.99766099,  0.99672300, &
        0.99539179,  0.99349195,  0.99079424,  0.98699373,  0.98167247, &
        0.97425884,  0.96399003,  0.94987535,  0.93068647,  0.90498149, &
        0.87122715,  0.82806110,  0.77459973,  0.71028948,  0.63413304, &
        0.54501963/
      data ((h22(ip,iw),iw=1,31), ip=13,13)/ &
       -0.4917E-07, -0.9745E-07, -0.1865E-06, -0.3432E-06, -0.6093E-06, &
       -0.1022E-05, -0.1603E-05, -0.2359E-05, -0.3325E-05, -0.4605E-05, &
       -0.6316E-05, -0.8598E-05, -0.1170E-04, -0.1609E-04, -0.2239E-04, &
       -0.3141E-04, -0.4416E-04, -0.6194E-04, -0.8652E-04, -0.1203E-03, &
       -0.1663E-03, -0.2284E-03, -0.3111E-03, -0.4193E-03, -0.5572E-03, &
       -0.7263E-03, -0.9240E-03, -0.1143E-02, -0.1378E-02, -0.1625E-02, &
       -0.1865E-02/
      data ((h23(ip,iw),iw=1,31), ip=13,13)/ &
       -0.1543E-09, -0.3619E-09, -0.6652E-09, -0.1136E-08, -0.1743E-08, &
       -0.2350E-08, -0.2578E-08, -0.2461E-08, -0.2727E-08, -0.4066E-08, &
       -0.5756E-08, -0.6690E-08, -0.7512E-08, -0.1041E-07, -0.1666E-07, &
       -0.2579E-07, -0.3570E-07, -0.4578E-07, -0.5684E-07, -0.6871E-07, &
       -0.7925E-07, -0.8473E-07, -0.8045E-07, -0.6059E-07, -0.1305E-07, &
        0.8252E-07,  0.2444E-06,  0.4481E-06,  0.6046E-06,  0.7101E-06, &
        0.1018E-05/
      data ((h21(ip,iw),iw=1,31), ip=14,14)/ &
        0.99999630,  0.99999273,  0.99998581,  0.99997246,  0.99994791, &
        0.99990481,  0.99983346,  0.99972439,  0.99956763,  0.99934852, &
        0.99904162,  0.99861497,  0.99802417,  0.99720109,  0.99604040, &
        0.99438727,  0.99202752,  0.98867983,  0.98396397,  0.97736186, &
        0.96817619,  0.95549798,  0.93818021,  0.91484344,  0.88396150, &
        0.84407526,  0.79411077,  0.73347151,  0.66137868,  0.57658100, &
        0.47910506/
      data ((h22(ip,iw),iw=1,31), ip=14,14)/ &
       -0.5036E-07, -0.9909E-07, -0.1888E-06, -0.3521E-06, -0.6282E-06, &
       -0.1066E-05, -0.1700E-05, -0.2555E-05, -0.3683E-05, -0.5196E-05, &
       -0.7229E-05, -0.9953E-05, -0.1371E-04, -0.1908E-04, -0.2681E-04, &
       -0.3786E-04, -0.5347E-04, -0.7529E-04, -0.1055E-03, -0.1468E-03, &
       -0.2030E-03, -0.2782E-03, -0.3772E-03, -0.5047E-03, -0.6634E-03, &
       -0.8522E-03, -0.1065E-02, -0.1295E-02, -0.1540E-02, -0.1787E-02, &
       -0.1996E-02/
      data ((h23(ip,iw),iw=1,31), ip=14,14)/ &
       -0.1650E-09, -0.3699E-09, -0.6839E-09, -0.1168E-08, -0.1799E-08, &
       -0.2477E-08, -0.2810E-08, -0.2757E-08, -0.3002E-08, -0.4172E-08, &
       -0.5399E-08, -0.5987E-08, -0.6990E-08, -0.1068E-07, -0.1805E-07, &
       -0.2764E-07, -0.3760E-07, -0.4845E-07, -0.6028E-07, -0.7216E-07, &
       -0.8063E-07, -0.8073E-07, -0.6733E-07, -0.3102E-07,  0.4654E-07, &
        0.1860E-06,  0.3840E-06,  0.5667E-06,  0.6739E-06,  0.8714E-06, &
        0.1516E-05/
      data ((h21(ip,iw),iw=1,31), ip=15,15)/ &
        0.99999636,  0.99999261,  0.99998546,  0.99997187,  0.99994642, &
        0.99990076,  0.99982363,  0.99970156,  0.99951971,  0.99925733, &
        0.99888444,  0.99836206,  0.99763465,  0.99661559,  0.99517184, &
        0.99311298,  0.99017996,  0.98602027,  0.98016363,  0.97197348, &
        0.96061146,  0.94501376,  0.92387742,  0.89570683,  0.85898691, &
        0.81248021,  0.75545335,  0.68728173,  0.60678530,  0.51328194, &
        0.40901369/
      data ((h22(ip,iw),iw=1,31), ip=15,15)/ &
       -0.5185E-07, -0.9968E-07, -0.1933E-06, -0.3603E-06, -0.6523E-06, &
       -0.1125E-05, -0.1831E-05, -0.2821E-05, -0.4170E-05, -0.5999E-05, &
       -0.8450E-05, -0.1176E-04, -0.1636E-04, -0.2297E-04, -0.3250E-04, &
       -0.4609E-04, -0.6529E-04, -0.9211E-04, -0.1292E-03, -0.1798E-03, &
       -0.2481E-03, -0.3385E-03, -0.4560E-03, -0.6041E-03, -0.7833E-03, &
       -0.9892E-03, -0.1214E-02, -0.1455E-02, -0.1704E-02, -0.1933E-02, &
       -0.2084E-02/
      data ((h23(ip,iw),iw=1,31), ip=15,15)/ &
       -0.1916E-09, -0.3752E-09, -0.6839E-09, -0.1192E-08, -0.1916E-08, &
       -0.2664E-08, -0.3198E-08, -0.3313E-08, -0.3566E-08, -0.4305E-08, &
       -0.4944E-08, -0.5242E-08, -0.6727E-08, -0.1150E-07, -0.1959E-07, &
       -0.2938E-07, -0.4016E-07, -0.5184E-07, -0.6435E-07, -0.7544E-07, &
       -0.7990E-07, -0.7239E-07, -0.4576E-07,  0.1525E-07,  0.1330E-06, &
        0.3159E-06,  0.5160E-06,  0.6449E-06,  0.7769E-06,  0.1232E-05, &
        0.2320E-05/
      data ((h21(ip,iw),iw=1,31), ip=16,16)/ &
        0.99999624,  0.99999249,  0.99998522,  0.99997109,  0.99994451, &
        0.99989629,  0.99981260,  0.99967539,  0.99946308,  0.99914724, &
        0.99869245,  0.99804932,  0.99714786,  0.99587929,  0.99407870, &
        0.99151409,  0.98786569,  0.98269808,  0.97542733,  0.96528339, &
        0.95127523,  0.93217921,  0.90655577,  0.87286794,  0.82974762, &
        0.77629298,  0.71193099,  0.63565648,  0.54636526,  0.44509035, &
        0.33688110/
      data ((h22(ip,iw),iw=1,31), ip=16,16)/ &
       -0.5260E-07, -0.1019E-06, -0.1974E-06, -0.3724E-06, -0.6824E-06, &
       -0.1194E-05, -0.1996E-05, -0.3163E-05, -0.4803E-05, -0.7042E-05, &
       -0.1005E-04, -0.1413E-04, -0.1982E-04, -0.2799E-04, -0.3975E-04, &
       -0.5650E-04, -0.8013E-04, -0.1131E-03, -0.1587E-03, -0.2205E-03, &
       -0.3030E-03, -0.4109E-03, -0.5485E-03, -0.7176E-03, -0.9153E-03, &
       -0.1135E-02, -0.1371E-02, -0.1619E-02, -0.1861E-02, -0.2046E-02, &
       -0.2109E-02/
      data ((h23(ip,iw),iw=1,31), ip=16,16)/ &
       -0.1836E-09, -0.3672E-09, -0.7105E-09, -0.1227E-08, -0.2012E-08, &
       -0.2964E-08, -0.3802E-08, -0.4263E-08, -0.4571E-08, -0.4697E-08, &
       -0.4755E-08, -0.4848E-08, -0.6804E-08, -0.1225E-07, -0.2080E-07, &
       -0.3111E-07, -0.4302E-07, -0.5604E-07, -0.6883E-07, -0.7751E-07, &
       -0.7602E-07, -0.5781E-07, -0.1111E-07,  0.8486E-07,  0.2480E-06, &
        0.4530E-06,  0.6104E-06,  0.7152E-06,  0.1020E-05,  0.1877E-05, &
        0.3424E-05/
      data ((h21(ip,iw),iw=1,31), ip=17,17)/ &
        0.99999613,  0.99999231,  0.99998498,  0.99997044,  0.99994290, &
        0.99989182,  0.99980116,  0.99964774,  0.99940079,  0.99902284, &
        0.99846709,  0.99767250,  0.99655056,  0.99496633,  0.99271864, &
        0.98952401,  0.98498827,  0.97857380,  0.96956515,  0.95703954, &
        0.93984765,  0.91661447,  0.88581598,  0.84599125,  0.79606229, &
        0.73541570,  0.66324592,  0.57828486,  0.48056805,  0.37372464, &
        0.26559973/
      data ((h22(ip,iw),iw=1,31), ip=17,17)/ &
       -0.5304E-07, -0.1036E-06, -0.2022E-06, -0.3850E-06, -0.7154E-06, &
       -0.1274E-05, -0.2183E-05, -0.3568E-05, -0.5572E-05, -0.8348E-05, &
       -0.1210E-04, -0.1720E-04, -0.2429E-04, -0.3441E-04, -0.4891E-04, &
       -0.6954E-04, -0.9864E-04, -0.1392E-03, -0.1949E-03, -0.2699E-03, &
       -0.3689E-03, -0.4963E-03, -0.6550E-03, -0.8439E-03, -0.1057E-02, &
       -0.1288E-02, -0.1534E-02, -0.1782E-02, -0.1994E-02, -0.2106E-02, &
       -0.2053E-02/
      data ((h23(ip,iw),iw=1,31), ip=17,17)/ &
       -0.1703E-09, -0.3858E-09, -0.7318E-09, -0.1283E-08, -0.2201E-08, &
       -0.3345E-08, -0.4614E-08, -0.5657E-08, -0.6173E-08, -0.5987E-08, &
       -0.5287E-08, -0.5037E-08, -0.7030E-08, -0.1263E-07, -0.2138E-07, &
       -0.3278E-07, -0.4617E-07, -0.6060E-07, -0.7291E-07, -0.7732E-07, &
       -0.6744E-07, -0.3340E-07,  0.4259E-07,  0.1817E-06,  0.3801E-06, &
        0.5630E-06,  0.6702E-06,  0.8678E-06,  0.1511E-05,  0.2833E-05, &
        0.4741E-05/
      data ((h21(ip,iw),iw=1,31), ip=18,18)/ &
        0.99999613,  0.99999225,  0.99998474,  0.99997002,  0.99994147, &
        0.99988788,  0.99979049,  0.99962020,  0.99933702,  0.99888939, &
        0.99821389,  0.99723196,  0.99583292,  0.99385166,  0.99104559, &
        0.98706627,  0.98143232,  0.97348487,  0.96235722,  0.94696146, &
        0.92598587,  0.89793938,  0.86130989,  0.81485945,  0.75785357, &
        0.68964934,  0.60902810,  0.51528323,  0.41067511,  0.30170137, &
        0.19860673/
      data ((h22(ip,iw),iw=1,31), ip=18,18)/ &
       -0.5304E-07, -0.1044E-06, -0.2061E-06, -0.3965E-06, -0.7459E-06, &
       -0.1357E-05, -0.2386E-05, -0.4017E-05, -0.6449E-05, -0.9910E-05, &
       -0.1465E-04, -0.2110E-04, -0.3000E-04, -0.4253E-04, -0.6040E-04, &
       -0.8579E-04, -0.1215E-03, -0.1712E-03, -0.2389E-03, -0.3293E-03, &
       -0.4469E-03, -0.5953E-03, -0.7747E-03, -0.9809E-03, -0.1207E-02, &
       -0.1448E-02, -0.1699E-02, -0.1931E-02, -0.2084E-02, -0.2093E-02, &
       -0.1907E-02/
      data ((h23(ip,iw),iw=1,31), ip=18,18)/ &
       -0.1703E-09, -0.3858E-09, -0.7424E-09, -0.1370E-08, -0.2379E-08, &
       -0.3821E-08, -0.5670E-08, -0.7445E-08, -0.8587E-08, -0.8552E-08, &
       -0.7352E-08, -0.6325E-08, -0.7467E-08, -0.1232E-07, -0.2127E-07, &
       -0.3357E-07, -0.4875E-07, -0.6440E-07, -0.7483E-07, -0.7309E-07, &
       -0.5083E-07,  0.6847E-08,  0.1213E-06,  0.3018E-06,  0.5002E-06, &
        0.6273E-06,  0.7589E-06,  0.1216E-05,  0.2306E-05,  0.4058E-05, &
        0.6072E-05/
      data ((h21(ip,iw),iw=1,31), ip=19,19)/ &
        0.99999619,  0.99999231,  0.99998468,  0.99996972,  0.99994040, &
        0.99988461,  0.99978089,  0.99959469,  0.99927568,  0.99875391, &
        0.99794221,  0.99673504,  0.99499243,  0.99251539,  0.98901063, &
        0.98405677,  0.97706568,  0.96724236,  0.95355600,  0.93474090, &
        0.90933526,  0.87580550,  0.83278608,  0.77937931,  0.71501046, &
        0.63865173,  0.54914534,  0.44749933,  0.33880121,  0.23220938, &
        0.13937765/
      data ((h22(ip,iw),iw=1,31), ip=19,19)/ &
       -0.5304E-07, -0.1064E-06, -0.2104E-06, -0.4047E-06, -0.7741E-06, &
       -0.1439E-05, -0.2589E-05, -0.4474E-05, -0.7399E-05, -0.1169E-04, &
       -0.1770E-04, -0.2591E-04, -0.3715E-04, -0.5276E-04, -0.7478E-04, &
       -0.1059E-03, -0.1497E-03, -0.2101E-03, -0.2919E-03, -0.3998E-03, &
       -0.5378E-03, -0.7075E-03, -0.9059E-03, -0.1126E-02, -0.1363E-02, &
       -0.1613E-02, -0.1858E-02, -0.2046E-02, -0.2112E-02, -0.1994E-02, &
       -0.1678E-02/
      data ((h23(ip,iw),iw=1,31), ip=19,19)/ &
       -0.1756E-09, -0.3991E-09, -0.7557E-09, -0.1421E-08, -0.2568E-08, &
       -0.4361E-08, -0.6815E-08, -0.9569E-08, -0.1197E-07, -0.1290E-07, &
       -0.1181E-07, -0.9694E-08, -0.8794E-08, -0.1168E-07, -0.1969E-07, &
       -0.3278E-07, -0.4934E-07, -0.6493E-07, -0.7206E-07, -0.6150E-07, &
       -0.2053E-07,  0.6998E-07,  0.2274E-06,  0.4265E-06,  0.5780E-06, &
        0.6774E-06,  0.9818E-06,  0.1844E-05,  0.3399E-05,  0.5411E-05, &
        0.7121E-05/
      data ((h21(ip,iw),iw=1,31), ip=20,20)/ &
        0.99999624,  0.99999231,  0.99998474,  0.99996942,  0.99993980, &
        0.99988210,  0.99977279,  0.99957287,  0.99921966,  0.99862283, &
        0.99766326,  0.99619573,  0.99403816,  0.99094629,  0.98657119, &
        0.98040581,  0.97174209,  0.95963550,  0.94288456,  0.92004675, &
        0.88955516,  0.84993827,  0.80012286,  0.73950386,  0.66727221, &
        0.58212024,  0.48402858,  0.37661588,  0.26779234,  0.16875094, &
        0.09066099/
      data ((h22(ip,iw),iw=1,31), ip=20,20)/ &
       -0.5290E-07, -0.1064E-06, -0.2119E-06, -0.4114E-06, -0.7982E-06, &
       -0.1509E-05, -0.2775E-05, -0.4919E-05, -0.8370E-05, -0.1362E-04, &
       -0.2118E-04, -0.3167E-04, -0.4595E-04, -0.6552E-04, -0.9271E-04, &
       -0.1309E-03, -0.1841E-03, -0.2571E-03, -0.3549E-03, -0.4821E-03, &
       -0.6415E-03, -0.8315E-03, -0.1046E-02, -0.1278E-02, -0.1526E-02, &
       -0.1777E-02, -0.1993E-02, -0.2109E-02, -0.2059E-02, -0.1807E-02, &
       -0.1386E-02/
      data ((h23(ip,iw),iw=1,31), ip=20,20)/ &
       -0.1889E-09, -0.3779E-09, -0.7876E-09, -0.1466E-08, -0.2775E-08, &
       -0.4854E-08, -0.7938E-08, -0.1197E-07, -0.1622E-07, -0.1911E-07, &
       -0.1924E-07, -0.1651E-07, -0.1270E-07, -0.1165E-07, -0.1684E-07, &
       -0.2884E-07, -0.4505E-07, -0.5873E-07, -0.6063E-07, -0.3693E-07, &
        0.3092E-07,  0.1620E-06,  0.3510E-06,  0.5227E-06,  0.6166E-06, &
        0.8039E-06,  0.1446E-05,  0.2779E-05,  0.4704E-05,  0.6641E-05, &
        0.7588E-05/
      data ((h21(ip,iw),iw=1,31), ip=21,21)/ &
        0.99999624,  0.99999231,  0.99998480,  0.99996942,  0.99993926, &
        0.99988008,  0.99976671,  0.99955463,  0.99917024,  0.99850142, &
        0.99739027,  0.99563473,  0.99299157,  0.98915201,  0.98369783, &
        0.97603029,  0.96531236,  0.95044154,  0.93004721,  0.90254754, &
        0.86634612,  0.82017386,  0.76329756,  0.69507873,  0.61428088, &
        0.52014816,  0.41490984,  0.30508935,  0.20102721,  0.11452556, &
        0.05384004/
      data ((h22(ip,iw),iw=1,31), ip=21,21)/ &
       -0.5349E-07, -0.1064E-06, -0.2128E-06, -0.4178E-06, -0.8167E-06, &
       -0.1568E-05, -0.2933E-05, -0.5332E-05, -0.9319E-05, -0.1561E-04, &
       -0.2499E-04, -0.3829E-04, -0.5651E-04, -0.8120E-04, -0.1150E-03, &
       -0.1616E-03, -0.2260E-03, -0.3134E-03, -0.4291E-03, -0.5768E-03, &
       -0.7570E-03, -0.9648E-03, -0.1193E-02, -0.1437E-02, -0.1691E-02, &
       -0.1927E-02, -0.2087E-02, -0.2101E-02, -0.1918E-02, -0.1545E-02, &
       -0.1069E-02/
      data ((h23(ip,iw),iw=1,31), ip=21,21)/ &
       -0.1783E-09, -0.3832E-09, -0.8036E-09, -0.1522E-08, -0.2924E-08, &
       -0.5277E-08, -0.9052E-08, -0.1446E-07, -0.2101E-07, -0.2708E-07, &
       -0.3022E-07, -0.2818E-07, -0.2166E-07, -0.1463E-07, -0.1331E-07, &
       -0.2062E-07, -0.3282E-07, -0.4140E-07, -0.3400E-07,  0.9002E-08, &
        0.1124E-06,  0.2822E-06,  0.4659E-06,  0.5729E-06,  0.6815E-06, &
        0.1123E-05,  0.2212E-05,  0.3980E-05,  0.6026E-05,  0.7434E-05, &
        0.7294E-05/
      data ((h21(ip,iw),iw=1,31), ip=22,22)/ &
        0.99999630,  0.99999255,  0.99998492,  0.99996948,  0.99993891, &
        0.99987864,  0.99976200,  0.99953991,  0.99912888,  0.99839354, &
        0.99713373,  0.99507743,  0.99188846,  0.98716390,  0.98039114, &
        0.97087091,  0.95763975,  0.93944019,  0.91475397,  0.88193613, &
        0.83949566,  0.78646111,  0.72224021,  0.64578426,  0.55588573, &
        0.45352978,  0.34383565,  0.23603255,  0.14191669,  0.07163429, &
        0.02856171/
      data ((h22(ip,iw),iw=1,31), ip=22,22)/ &
       -0.5349E-07, -0.1068E-06, -0.2146E-06, -0.4223E-06, -0.8317E-06, &
       -0.1611E-05, -0.3067E-05, -0.5691E-05, -0.1020E-04, -0.1758E-04, &
       -0.2897E-04, -0.4561E-04, -0.6878E-04, -0.1001E-03, -0.1423E-03, &
       -0.1994E-03, -0.2768E-03, -0.3805E-03, -0.5154E-03, -0.6837E-03, &
       -0.8826E-03, -0.1105E-02, -0.1345E-02, -0.1600E-02, -0.1850E-02, &
       -0.2046E-02, -0.2119E-02, -0.2007E-02, -0.1693E-02, -0.1237E-02, &
       -0.7623E-03/
      data ((h23(ip,iw),iw=1,31), ip=22,22)/ &
       -0.1889E-09, -0.4018E-09, -0.8302E-09, -0.1591E-08, -0.3049E-08, &
       -0.5604E-08, -0.1004E-07, -0.1681E-07, -0.2608E-07, -0.3634E-07, &
       -0.4447E-07, -0.4604E-07, -0.3862E-07, -0.2469E-07, -0.1217E-07, &
       -0.7498E-08, -0.8834E-08, -0.6474E-08,  0.1720E-07,  0.8812E-07, &
        0.2295E-06,  0.4139E-06,  0.5426E-06,  0.6091E-06,  0.8817E-06, &
        0.1725E-05,  0.3279E-05,  0.5314E-05,  0.7076E-05,  0.7544E-05, &
        0.6291E-05/
      data ((h21(ip,iw),iw=1,31), ip=23,23)/ &
        0.99999630,  0.99999261,  0.99998510,  0.99996954,  0.99993867, &
        0.99987787,  0.99975872,  0.99952865,  0.99909520,  0.99830168, &
        0.99690276,  0.99454683,  0.99077523,  0.98504108,  0.97669667, &
        0.96492028,  0.94863147,  0.92644799,  0.89676428,  0.85799170, &
        0.80890477,  0.74876618,  0.67669797,  0.59129578,  0.49246222, &
        0.38387454,  0.27356315,  0.17285836,  0.09315068,  0.04061204, &
        0.01303118/
      data ((h22(ip,iw),iw=1,31), ip=23,23)/ &
       -0.5170E-07, -0.1068E-06, -0.2144E-06, -0.4252E-06, -0.8411E-06, &
       -0.1644E-05, -0.3175E-05, -0.5994E-05, -0.1099E-04, -0.1943E-04, &
       -0.3294E-04, -0.5335E-04, -0.8251E-04, -0.1223E-03, -0.1753E-03, &
       -0.2454E-03, -0.3382E-03, -0.4598E-03, -0.6145E-03, -0.8020E-03, &
       -0.1016E-02, -0.1251E-02, -0.1504E-02, -0.1763E-02, -0.1988E-02, &
       -0.2114E-02, -0.2073E-02, -0.1826E-02, -0.1406E-02, -0.9217E-03, &
       -0.4992E-03/
      data ((h23(ip,iw),iw=1,31), ip=23,23)/ &
       -0.1676E-09, -0.4178E-09, -0.8488E-09, -0.1613E-08, -0.3127E-08, &
       -0.5899E-08, -0.1090E-07, -0.1895E-07, -0.3101E-07, -0.4637E-07, &
       -0.6159E-07, -0.7060E-07, -0.6640E-07, -0.4683E-07, -0.1875E-07, &
        0.7974E-08,  0.2920E-07,  0.5387E-07,  0.1050E-06,  0.2110E-06, &
        0.3760E-06,  0.5244E-06,  0.5825E-06,  0.7228E-06,  0.1328E-05, &
        0.2641E-05,  0.4566E-05,  0.6539E-05,  0.7561E-05,  0.6889E-05, &
        0.4852E-05/
      data ((h21(ip,iw),iw=1,31), ip=24,24)/ &
        0.99999654,  0.99999279,  0.99998516,  0.99996966,  0.99993861, &
        0.99987739,  0.99975634,  0.99952048,  0.99906945,  0.99822646, &
        0.99670368,  0.99406314,  0.98970103,  0.98287082,  0.97271711, &
        0.95825428,  0.93828648,  0.91137356,  0.87595028,  0.83064175, &
        0.77455646,  0.70689976,  0.62624615,  0.53160882,  0.42513669, &
        0.31351697,  0.20733559,  0.11866122,  0.05608100,  0.02031803, &
        0.00467783/
      data ((h22(ip,iw),iw=1,31), ip=24,24)/ &
       -0.5126E-07, -0.1061E-06, -0.2135E-06, -0.4282E-06, -0.8481E-06, &
       -0.1671E-05, -0.3258E-05, -0.6234E-05, -0.1165E-04, -0.2110E-04, &
       -0.3675E-04, -0.6120E-04, -0.9723E-04, -0.1473E-03, -0.2140E-03, &
       -0.3006E-03, -0.4118E-03, -0.5529E-03, -0.7269E-03, -0.9307E-03, &
       -0.1158E-02, -0.1404E-02, -0.1665E-02, -0.1913E-02, -0.2086E-02, &
       -0.2113E-02, -0.1940E-02, -0.1571E-02, -0.1092E-02, -0.6336E-03, &
       -0.2972E-03/
      data ((h23(ip,iw),iw=1,31), ip=24,24)/ &
       -0.1969E-09, -0.3885E-09, -0.8435E-09, -0.1650E-08, -0.3145E-08, &
       -0.6147E-08, -0.1152E-07, -0.2078E-07, -0.3566E-07, -0.5644E-07, &
       -0.8076E-07, -0.1013E-06, -0.1065E-06, -0.8660E-07, -0.4128E-07, &
        0.1815E-07,  0.8048E-07,  0.1481E-06,  0.2421E-06,  0.3816E-06, &
        0.5306E-06,  0.5969E-06,  0.6485E-06,  0.1030E-05,  0.2081E-05, &
        0.3832E-05,  0.5890E-05,  0.7356E-05,  0.7306E-05,  0.5642E-05, &
        0.3349E-05/
      data ((h21(ip,iw),iw=1,31), ip=25,25)/ &
        0.99999666,  0.99999303,  0.99998528,  0.99996966,  0.99993879, &
        0.99987727,  0.99975473,  0.99951464,  0.99904978,  0.99816769, &
        0.99653906,  0.99364126,  0.98871136,  0.98075461,  0.96861500, &
        0.95105618,  0.92675209,  0.89430392,  0.85237980,  0.79998612, &
        0.73641622,  0.66047239,  0.57058001,  0.46732146,  0.35575873, &
        0.24537134,  0.14839041,  0.07545924,  0.03036642,  0.00851601, &
        0.00096470/
      data ((h22(ip,iw),iw=1,31), ip=25,25)/ &
       -0.5051E-07, -0.1044E-06, -0.2143E-06, -0.4293E-06, -0.8529E-06, &
       -0.1691E-05, -0.3317E-05, -0.6421E-05, -0.1220E-04, -0.2254E-04, &
       -0.4023E-04, -0.6882E-04, -0.1123E-03, -0.1744E-03, -0.2580E-03, &
       -0.3651E-03, -0.4987E-03, -0.6611E-03, -0.8527E-03, -0.1069E-02, &
       -0.1307E-02, -0.1564E-02, -0.1823E-02, -0.2034E-02, -0.2125E-02, &
       -0.2030E-02, -0.1725E-02, -0.1269E-02, -0.7863E-03, -0.3985E-03, &
       -0.1586E-03/
      data ((h23(ip,iw),iw=1,31), ip=25,25)/ &
       -0.1836E-09, -0.4124E-09, -0.8515E-09, -0.1615E-08, -0.3236E-08, &
       -0.6338E-08, -0.1201E-07, -0.2228E-07, -0.3958E-07, -0.6599E-07, &
       -0.1006E-06, -0.1367E-06, -0.1591E-06, -0.1478E-06, -0.8960E-07, &
        0.1091E-07,  0.1380E-06,  0.2793E-06,  0.4346E-06,  0.5879E-06, &
        0.6670E-06,  0.6630E-06,  0.8472E-06,  0.1618E-05,  0.3133E-05, &
        0.5168E-05,  0.6973E-05,  0.7523E-05,  0.6352E-05,  0.4136E-05, &
        0.2052E-05/
      data ((h21(ip,iw),iw=1,31), ip=26,26)/ &
        0.99999678,  0.99999321,  0.99998528,  0.99996996,  0.99993908, &
        0.99987698,  0.99975365,  0.99951053,  0.99903572,  0.99812329, &
        0.99640930,  0.99328965,  0.98784143,  0.97878826,  0.96458673, &
        0.94361532,  0.91436821,  0.87557977,  0.82637513,  0.76623750, &
        0.69435614,  0.60912937,  0.50989425,  0.39977902,  0.28677946, &
        0.18258661,  0.09928077,  0.04378778,  0.01428860,  0.00259024, &
       -0.00019467/
      data ((h22(ip,iw),iw=1,31), ip=26,26)/ &
       -0.4991E-07, -0.1043E-06, -0.2143E-06, -0.4269E-06, -0.8588E-06, &
       -0.1703E-05, -0.3360E-05, -0.6564E-05, -0.1261E-04, -0.2372E-04, &
       -0.4326E-04, -0.7587E-04, -0.1271E-03, -0.2024E-03, -0.3056E-03, &
       -0.4379E-03, -0.5984E-03, -0.7844E-03, -0.9924E-03, -0.1220E-02, &
       -0.1467E-02, -0.1728E-02, -0.1965E-02, -0.2110E-02, -0.2090E-02, &
       -0.1860E-02, -0.1447E-02, -0.9562E-03, -0.5215E-03, -0.2268E-03, &
       -0.7386E-04/
      data ((h23(ip,iw),iw=1,31), ip=26,26)/ &
       -0.1996E-09, -0.4417E-09, -0.7930E-09, -0.1668E-08, -0.3294E-08, &
       -0.6394E-08, -0.1237E-07, -0.2344E-07, -0.4278E-07, -0.7442E-07, &
       -0.1199E-06, -0.1745E-06, -0.2217E-06, -0.2310E-06, -0.1714E-06, &
       -0.2780E-07,  0.1869E-06,  0.4373E-06,  0.6698E-06,  0.8022E-06, &
        0.7859E-06,  0.8087E-06,  0.1284E-05,  0.2509E-05,  0.4402E-05, &
        0.6415E-05,  0.7530E-05,  0.6949E-05,  0.4957E-05,  0.2716E-05, &
        0.1081E-05/
      data ((h81(ip,iw),iw=1,31), ip= 1, 1)/ &
        0.99998581,  0.99997175,  0.99994498,  0.99989539,  0.99980557, &
        0.99965411,  0.99941576,  0.99907619,  0.99863428,  0.99810231, &
        0.99749488,  0.99681550,  0.99605590,  0.99518549,  0.99415886, &
        0.99292004,  0.99139947,  0.98950624,  0.98714000,  0.98420030, &
        0.98059285,  0.97621703,  0.97091669,  0.96443838,  0.95640814, &
        0.94630748,  0.93342716,  0.91680253,  0.89516866,  0.86697024, &
        0.83044368/
      data ((h82(ip,iw),iw=1,31), ip= 1, 1)/ &
       -0.1133E-07, -0.2653E-07, -0.6126E-07, -0.1332E-06, -0.2990E-06, &
       -0.6641E-06, -0.1374E-05, -0.2604E-05, -0.4427E-05, -0.6804E-05, &
       -0.9611E-05, -0.1276E-04, -0.1629E-04, -0.2042E-04, -0.2535E-04, &
       -0.3137E-04, -0.3882E-04, -0.4794E-04, -0.5875E-04, -0.7119E-04, &
       -0.8529E-04, -0.1012E-03, -0.1190E-03, -0.1390E-03, -0.1619E-03, &
       -0.1885E-03, -0.2197E-03, -0.2561E-03, -0.2984E-03, -0.3466E-03, &
       -0.4004E-03/
      data ((h83(ip,iw),iw=1,31), ip= 1, 1)/ &
        0.1118E-09,  0.3087E-09,  0.6253E-09,  0.1187E-08,  0.2278E-08, &
        0.4066E-08,  0.6932E-08,  0.1020E-07,  0.1334E-07,  0.1569E-07, &
        0.1687E-07,  0.1655E-07,  0.1419E-07,  0.1020E-07,  0.4393E-08, &
       -0.4963E-08, -0.1939E-07, -0.3654E-07, -0.5056E-07, -0.5584E-07, &
       -0.5170E-07, -0.4162E-07, -0.2922E-07, -0.1764E-07, -0.1103E-07, &
       -0.1168E-07, -0.1656E-07, -0.1255E-07,  0.2023E-07,  0.1052E-06, &
        0.2684E-06/
      data ((h81(ip,iw),iw=1,31), ip= 2, 2)/ &
        0.99998581,  0.99997175,  0.99994498,  0.99989539,  0.99980563, &
        0.99965405,  0.99941570,  0.99907601,  0.99863404,  0.99810159, &
        0.99749357,  0.99681306,  0.99605125,  0.99517709,  0.99414408, &
        0.99289298,  0.99134856,  0.98941231,  0.98696810,  0.98389256, &
        0.98005611,  0.97531378,  0.96945959,  0.96217942,  0.95302546, &
        0.94137722,  0.92637795,  0.90687048,  0.88135755,  0.84807086, &
        0.80509931/
      data ((h82(ip,iw),iw=1,31), ip= 2, 2)/ &
       -0.1133E-07, -0.2653E-07, -0.6126E-07, -0.1332E-06, -0.2988E-06, &
       -0.6636E-06, -0.1374E-05, -0.2603E-05, -0.4424E-05, -0.6800E-05, &
       -0.9609E-05, -0.1275E-04, -0.1628E-04, -0.2040E-04, -0.2531E-04, &
       -0.3129E-04, -0.3866E-04, -0.4767E-04, -0.5830E-04, -0.7050E-04, &
       -0.8435E-04, -0.1001E-03, -0.1180E-03, -0.1383E-03, -0.1619E-03, &
       -0.1894E-03, -0.2217E-03, -0.2597E-03, -0.3038E-03, -0.3542E-03, &
       -0.4103E-03/
      data ((h83(ip,iw),iw=1,31), ip= 2, 2)/ &
        0.1118E-09,  0.3087E-09,  0.6253E-09,  0.1187E-08,  0.2275E-08, &
        0.4074E-08,  0.6926E-08,  0.1019E-07,  0.1333E-07,  0.1572E-07, &
        0.1684E-07,  0.1657E-07,  0.1428E-07,  0.1028E-07,  0.4308E-08, &
       -0.5050E-08, -0.1943E-07, -0.3642E-07, -0.4992E-07, -0.5412E-07, &
       -0.4733E-07, -0.3265E-07, -0.1384E-07,  0.6059E-08,  0.2311E-07, &
        0.3506E-07,  0.4738E-07,  0.7484E-07,  0.1396E-06,  0.2661E-06, &
        0.4807E-06/
      data ((h81(ip,iw),iw=1,31), ip= 3, 3)/ &
        0.99998581,  0.99997175,  0.99994498,  0.99989539,  0.99980557, &
        0.99965400,  0.99941570,  0.99907571,  0.99863356,  0.99810058, &
        0.99749130,  0.99680942,  0.99604398,  0.99516451,  0.99412048, &
        0.99284983,  0.99126911,  0.98926747,  0.98670810,  0.98343539, &
        0.97928077,  0.97405070,  0.96748143,  0.95918971,  0.94863594, &
        0.93506783,  0.91744840,  0.89439797,  0.86418962,  0.82487863, &
        0.77447081/
      data ((h82(ip,iw),iw=1,31), ip= 3, 3)/ &
       -0.1133E-07, -0.2653E-07, -0.6126E-07, -0.1332E-06, -0.2987E-06, &
       -0.6636E-06, -0.1374E-05, -0.2601E-05, -0.4422E-05, -0.6798E-05, &
       -0.9603E-05, -0.1273E-04, -0.1626E-04, -0.2036E-04, -0.2523E-04, &
       -0.3116E-04, -0.3843E-04, -0.4728E-04, -0.5770E-04, -0.6965E-04, &
       -0.8331E-04, -0.9905E-04, -0.1172E-03, -0.1381E-03, -0.1625E-03, &
       -0.1912E-03, -0.2251E-03, -0.2649E-03, -0.3112E-03, -0.3643E-03, &
       -0.4232E-03/
      data ((h83(ip,iw),iw=1,31), ip= 3, 3)/ &
        0.1118E-09,  0.3087E-09,  0.6253E-09,  0.1187E-08,  0.2272E-08, &
        0.4085E-08,  0.6900E-08,  0.1021E-07,  0.1330E-07,  0.1569E-07, &
        0.1688E-07,  0.1650E-07,  0.1424E-07,  0.1019E-07,  0.4311E-08, &
       -0.5109E-08, -0.1943E-07, -0.3609E-07, -0.4868E-07, -0.5063E-07, &
       -0.3987E-07, -0.1953E-07,  0.6922E-08,  0.3656E-07,  0.6530E-07, &
        0.9259E-07,  0.1260E-06,  0.1819E-06,  0.2843E-06,  0.4583E-06, &
        0.7293E-06/
      data ((h81(ip,iw),iw=1,31), ip= 4, 4)/ &
        0.99998581,  0.99997175,  0.99994498,  0.99989545,  0.99980563, &
        0.99965394,  0.99941546,  0.99907529,  0.99863273,  0.99809867, &
        0.99748814,  0.99680293,  0.99603289,  0.99514413,  0.99408364, &
        0.99278188,  0.99114609,  0.98904693,  0.98631871,  0.98277009, &
        0.97818679,  0.97232127,  0.96484441,  0.95528841,  0.94299692, &
        0.92705190,  0.90620387,  0.87883735,  0.84302551,  0.79670012, &
        0.73788261/
      data ((h82(ip,iw),iw=1,31), ip= 4, 4)/ &
       -0.1133E-07, -0.2653E-07, -0.6126E-07, -0.1334E-06, -0.2985E-06, &
       -0.6633E-06, -0.1372E-05, -0.2600E-05, -0.4418E-05, -0.6790E-05, &
       -0.9591E-05, -0.1272E-04, -0.1622E-04, -0.2029E-04, -0.2512E-04, &
       -0.3096E-04, -0.3810E-04, -0.4675E-04, -0.5693E-04, -0.6868E-04, &
       -0.8228E-04, -0.9821E-04, -0.1168E-03, -0.1386E-03, -0.1641E-03, &
       -0.1942E-03, -0.2299E-03, -0.2720E-03, -0.3212E-03, -0.3774E-03, &
       -0.4391E-03/
      data ((h83(ip,iw),iw=1,31), ip= 4, 4)/ &
        0.1118E-09,  0.3087E-09,  0.6253E-09,  0.1184E-08,  0.2270E-08, &
        0.4090E-08,  0.6908E-08,  0.1021E-07,  0.1329E-07,  0.1568E-07, &
        0.1682E-07,  0.1662E-07,  0.1425E-07,  0.1019E-07,  0.4239E-08, &
       -0.5013E-08, -0.1921E-07, -0.3516E-07, -0.4591E-07, -0.4441E-07, &
       -0.2854E-07, -0.1304E-08,  0.3388E-07,  0.7441E-07,  0.1171E-06, &
        0.1631E-06,  0.2220E-06,  0.3120E-06,  0.4579E-06,  0.6851E-06, &
        0.1015E-05/
      data ((h81(ip,iw),iw=1,31), ip= 5, 5)/ &
        0.99998581,  0.99997175,  0.99994504,  0.99989545,  0.99980551, &
        0.99965388,  0.99941510,  0.99907458,  0.99863106,  0.99809599, &
        0.99748284,  0.99679315,  0.99601531,  0.99511212,  0.99402517, &
        0.99267668,  0.99095803,  0.98871505,  0.98574859,  0.98182374, &
        0.97667706,  0.96999919,  0.96138340,  0.95025587,  0.93580973, &
        0.91692734,  0.89212763,  0.85957503,  0.81718338,  0.76283979, &
        0.69476205/
      data ((h82(ip,iw),iw=1,31), ip= 5, 5)/ &
       -0.1133E-07, -0.2623E-07, -0.6096E-07, -0.1334E-06, -0.2982E-06, &
       -0.6626E-06, -0.1371E-05, -0.2596E-05, -0.4414E-05, -0.6781E-05, &
       -0.9572E-05, -0.1269E-04, -0.1617E-04, -0.2020E-04, -0.2495E-04, &
       -0.3068E-04, -0.3764E-04, -0.4608E-04, -0.5604E-04, -0.6768E-04, &
       -0.8140E-04, -0.9775E-04, -0.1171E-03, -0.1399E-03, -0.1668E-03, &
       -0.1988E-03, -0.2367E-03, -0.2816E-03, -0.3342E-03, -0.3940E-03, &
       -0.4579E-03/
      data ((h83(ip,iw),iw=1,31), ip= 5, 5)/ &
        0.1118E-09,  0.3033E-09,  0.6147E-09,  0.1184E-08,  0.2286E-08, &
        0.4082E-08,  0.6948E-08,  0.1020E-07,  0.1333E-07,  0.1566E-07, &
        0.1680E-07,  0.1656E-07,  0.1422E-07,  0.1020E-07,  0.4348E-08, &
       -0.4771E-08, -0.1849E-07, -0.3312E-07, -0.4106E-07, -0.3471E-07, &
       -0.1250E-07,  0.2260E-07,  0.6782E-07,  0.1209E-06,  0.1805E-06, &
        0.2497E-06,  0.3393E-06,  0.4686E-06,  0.6635E-06,  0.9481E-06, &
        0.1338E-05/
      data ((h81(ip,iw),iw=1,31), ip= 6, 6)/ &
        0.99998581,  0.99997175,  0.99994504,  0.99989545,  0.99980545, &
        0.99965370,  0.99941462,  0.99907362,  0.99862880,  0.99809134, &
        0.99747437,  0.99677783,  0.99598694,  0.99506181,  0.99393469, &
        0.99251598,  0.99067485,  0.98822564,  0.98493081,  0.98050761, &
        0.97463554,  0.96693331,  0.95689833,  0.94382089,  0.92670774, &
        0.90421730,  0.87463552,  0.83594227,  0.78595793,  0.72265798, &
        0.64477807/
      data ((h82(ip,iw),iw=1,31), ip= 6, 6)/ &
       -0.1192E-07, -0.2623E-07, -0.6126E-07, -0.1328E-06, -0.2979E-06, &
       -0.6612E-06, -0.1367E-05, -0.2591E-05, -0.4407E-05, -0.6767E-05, &
       -0.9544E-05, -0.1264E-04, -0.1609E-04, -0.2005E-04, -0.2471E-04, &
       -0.3028E-04, -0.3705E-04, -0.4527E-04, -0.5510E-04, -0.6679E-04, &
       -0.8085E-04, -0.9786E-04, -0.1182E-03, -0.1423E-03, -0.1709E-03, &
       -0.2051E-03, -0.2459E-03, -0.2943E-03, -0.3509E-03, -0.4141E-03, &
       -0.4788E-03/
      data ((h83(ip,iw),iw=1,31), ip= 6, 6)/ &
        0.1224E-09,  0.3033E-09,  0.6200E-09,  0.1173E-08,  0.2291E-08, &
        0.4079E-08,  0.6937E-08,  0.1017E-07,  0.1331E-07,  0.1565E-07, &
        0.1680E-07,  0.1651E-07,  0.1426E-07,  0.1027E-07,  0.4537E-08, &
       -0.4356E-08, -0.1703E-07, -0.2923E-07, -0.3288E-07, -0.2072E-07, &
        0.8817E-08,  0.5309E-07,  0.1097E-06,  0.1778E-06,  0.2583E-06, &
        0.3552E-06,  0.4808E-06,  0.6550E-06,  0.9040E-06,  0.1248E-05, &
        0.1691E-05/
      data ((h81(ip,iw),iw=1,31), ip= 7, 7)/ &
        0.99998581,  0.99997175,  0.99994504,  0.99989545,  0.99980539, &
        0.99965340,  0.99941415,  0.99907178,  0.99862510,  0.99808419, &
        0.99746084,  0.99675345,  0.99594355,  0.99498338,  0.99379563, &
        0.99227214,  0.99025416,  0.98751915,  0.98378420,  0.97871542, &
        0.97192460,  0.96294236,  0.95114350,  0.93564767,  0.91524792, &
        0.88836491,  0.85308057,  0.80723882,  0.74866933,  0.67568231, &
        0.58799136/
      data ((h82(ip,iw),iw=1,31), ip= 7, 7)/ &
       -0.1192E-07, -0.2623E-07, -0.6096E-07, -0.1328E-06, -0.2964E-06, &
       -0.6590E-06, -0.1365E-05, -0.2581E-05, -0.4391E-05, -0.6743E-05, &
       -0.9506E-05, -0.1257E-04, -0.1597E-04, -0.1985E-04, -0.2437E-04, &
       -0.2976E-04, -0.3632E-04, -0.4439E-04, -0.5422E-04, -0.6618E-04, &
       -0.8080E-04, -0.9869E-04, -0.1202E-03, -0.1460E-03, -0.1768E-03, &
       -0.2137E-03, -0.2580E-03, -0.3107E-03, -0.3715E-03, -0.4371E-03, &
       -0.5005E-03/
      data ((h83(ip,iw),iw=1,31), ip= 7, 7)/ &
        0.1224E-09,  0.3033E-09,  0.6094E-09,  0.1173E-08,  0.2286E-08, &
        0.4050E-08,  0.6868E-08,  0.1020E-07,  0.1332E-07,  0.1565E-07, &
        0.1683E-07,  0.1647E-07,  0.1427E-07,  0.1042E-07,  0.5045E-08, &
       -0.3148E-08, -0.1404E-07, -0.2277E-07, -0.2081E-07, -0.1665E-08, &
        0.3608E-07,  0.9067E-07,  0.1608E-06,  0.2478E-06,  0.3534E-06, &
        0.4833E-06,  0.6498E-06,  0.8741E-06,  0.1180E-05,  0.1581E-05, &
        0.2067E-05/
      data ((h81(ip,iw),iw=1,31), ip= 8, 8)/ &
        0.99998581,  0.99997175,  0.99994504,  0.99989539,  0.99980527, &
        0.99965310,  0.99941295,  0.99906909,  0.99861914,  0.99807310, &
        0.99743950,  0.99671561,  0.99587530,  0.99486279,  0.99358487, &
        0.99190885,  0.98964298,  0.98652112,  0.98221254,  0.97632098, &
        0.96837795,  0.95780253,  0.94381404,  0.92532998,  0.90090793, &
        0.86874938,  0.82677054,  0.77276111,  0.70474106,  0.62176353, &
        0.52501291/
      data ((h82(ip,iw),iw=1,31), ip= 8, 8)/ &
       -0.1192E-07, -0.2623E-07, -0.6096E-07, -0.1326E-06, -0.2964E-06, &
       -0.6560E-06, -0.1359E-05, -0.2570E-05, -0.4371E-05, -0.6707E-05, &
       -0.9442E-05, -0.1246E-04, -0.1579E-04, -0.1956E-04, -0.2392E-04, &
       -0.2910E-04, -0.3550E-04, -0.4353E-04, -0.5357E-04, -0.6600E-04, &
       -0.8143E-04, -0.1004E-03, -0.1235E-03, -0.1514E-03, -0.1849E-03, &
       -0.2252E-03, -0.2737E-03, -0.3311E-03, -0.3957E-03, -0.4619E-03, &
       -0.5212E-03/
      data ((h83(ip,iw),iw=1,31), ip= 8, 8)/ &
        0.1224E-09,  0.3033E-09,  0.6094E-09,  0.1176E-08,  0.2280E-08, &
        0.3997E-08,  0.6873E-08,  0.1018E-07,  0.1335E-07,  0.1563E-07, &
        0.1689E-07,  0.1648E-07,  0.1446E-07,  0.1083E-07,  0.6157E-08, &
       -0.5933E-09, -0.8722E-08, -0.1243E-07, -0.3970E-08,  0.2292E-07, &
        0.7000E-07,  0.1367E-06,  0.2237E-06,  0.3336E-06,  0.4695E-06, &
        0.6371E-06,  0.8496E-06,  0.1128E-05,  0.1492E-05,  0.1941E-05, &
        0.2452E-05/
      data ((h81(ip,iw),iw=1,31), ip= 9, 9)/ &
        0.99998581,  0.99997175,  0.99994504,  0.99989533,  0.99980509, &
        0.99965227,  0.99941087,  0.99906486,  0.99861032,  0.99805486, &
        0.99740648,  0.99665618,  0.99577004,  0.99467933,  0.99326950, &
        0.99137819,  0.98877430,  0.98514515,  0.98010147,  0.97317445, &
        0.96379381,  0.95123780,  0.93453717,  0.91238034,  0.88309479, &
        0.84469539,  0.79499590,  0.73186743,  0.65385056,  0.56123221, &
        0.45710266/
      data ((h82(ip,iw),iw=1,31), ip= 9, 9)/ &
       -0.1192E-07, -0.2564E-07, -0.6036E-07, -0.1317E-06, -0.2945E-06, &
       -0.6496E-06, -0.1347E-05, -0.2549E-05, -0.4338E-05, -0.6650E-05, &
       -0.9351E-05, -0.1230E-04, -0.1554E-04, -0.1916E-04, -0.2334E-04, &
       -0.2836E-04, -0.3469E-04, -0.4287E-04, -0.5330E-04, -0.6645E-04, &
       -0.8291E-04, -0.1034E-03, -0.1284E-03, -0.1588E-03, -0.1956E-03, &
       -0.2401E-03, -0.2936E-03, -0.3558E-03, -0.4228E-03, -0.4870E-03, &
       -0.5392E-03/
      data ((h83(ip,iw),iw=1,31), ip= 9, 9)/ &
        0.1224E-09,  0.3140E-09,  0.6200E-09,  0.1187E-08,  0.2262E-08, &
        0.4047E-08,  0.6900E-08,  0.1016E-07,  0.1328E-07,  0.1570E-07, &
        0.1689E-07,  0.1661E-07,  0.1483E-07,  0.1177E-07,  0.8323E-08, &
        0.3885E-08, -0.1278E-09,  0.1943E-08,  0.1790E-07,  0.5358E-07, &
        0.1115E-06,  0.1931E-06,  0.3007E-06,  0.4384E-06,  0.6095E-06, &
        0.8198E-06,  0.1083E-05,  0.1418E-05,  0.1834E-05,  0.2317E-05, &
        0.2835E-05/
      data ((h81(ip,iw),iw=1,31), ip=10,10)/ &
        0.99998581,  0.99997181,  0.99994504,  0.99989516,  0.99980468, &
        0.99965107,  0.99940813,  0.99905807,  0.99859601,  0.99802738, &
        0.99735516,  0.99656427,  0.99561012,  0.99440449,  0.99280733, &
        0.99062097,  0.98757029,  0.98328680,  0.97731358,  0.96909207, &
        0.95792198,  0.94290930,  0.92286408,  0.89623821,  0.86115694, &
        0.81549817,  0.75707543,  0.68409348,  0.59607393,  0.49501973, &
        0.38628352/
      data ((h82(ip,iw),iw=1,31), ip=10,10)/ &
       -0.1133E-07, -0.2623E-07, -0.6036E-07, -0.1304E-06, -0.2899E-06, &
       -0.6426E-06, -0.1333E-05, -0.2521E-05, -0.4285E-05, -0.6567E-05, &
       -0.9213E-05, -0.1209E-04, -0.1520E-04, -0.1866E-04, -0.2267E-04, &
       -0.2760E-04, -0.3402E-04, -0.4253E-04, -0.5359E-04, -0.6768E-04, &
       -0.8547E-04, -0.1077E-03, -0.1353E-03, -0.1688E-03, -0.2096E-03, &
       -0.2592E-03, -0.3181E-03, -0.3842E-03, -0.4512E-03, -0.5104E-03, &
       -0.5525E-03/
      data ((h83(ip,iw),iw=1,31), ip=10,10)/ &
        0.1330E-09,  0.2927E-09,  0.6200E-09,  0.1184E-08,  0.2248E-08, &
        0.4066E-08,  0.6839E-08,  0.1013E-07,  0.1324E-07,  0.1565E-07, &
        0.1700E-07,  0.1694E-07,  0.1554E-07,  0.1371E-07,  0.1232E-07, &
        0.1120E-07,  0.1218E-07,  0.2117E-07,  0.4557E-07,  0.9112E-07, &
        0.1623E-06,  0.2623E-06,  0.3951E-06,  0.5657E-06,  0.7766E-06, &
        0.1034E-05,  0.1351E-05,  0.1741E-05,  0.2198E-05,  0.2697E-05, &
        0.3192E-05/
      data ((h81(ip,iw),iw=1,31), ip=11,11)/ &
        0.99998581,  0.99997181,  0.99994504,  0.99989504,  0.99980414, &
        0.99964952,  0.99940366,  0.99904746,  0.99857390,  0.99798411, &
        0.99727577,  0.99642479,  0.99537003,  0.99400103,  0.99214578, &
        0.98956615,  0.98593515,  0.98082125,  0.97368443,  0.96384913, &
        0.95045704,  0.93240529,  0.90826738,  0.87627876,  0.83440095, &
        0.78045630,  0.71244276,  0.62930238,  0.53204596,  0.42478460, &
        0.31533009/
      data ((h82(ip,iw),iw=1,31), ip=11,11)/ &
       -0.1148E-07, -0.2623E-07, -0.5947E-07, -0.1292E-06, -0.2861E-06, &
       -0.6317E-06, -0.1309E-05, -0.2476E-05, -0.4212E-05, -0.6445E-05, &
       -0.9022E-05, -0.1179E-04, -0.1476E-04, -0.1808E-04, -0.2199E-04, &
       -0.2695E-04, -0.3364E-04, -0.4270E-04, -0.5462E-04, -0.6993E-04, &
       -0.8939E-04, -0.1140E-03, -0.1446E-03, -0.1820E-03, -0.2277E-03, &
       -0.2829E-03, -0.3469E-03, -0.4149E-03, -0.4791E-03, -0.5302E-03, &
       -0.5588E-03/
      data ((h83(ip,iw),iw=1,31), ip=11,11)/ &
        0.1357E-09,  0.2980E-09,  0.6147E-09,  0.1163E-08,  0.2214E-08, &
        0.4037E-08,  0.6809E-08,  0.1013E-07,  0.1326E-07,  0.1581E-07, &
        0.1738E-07,  0.1764E-07,  0.1725E-07,  0.1716E-07,  0.1870E-07, &
        0.2201E-07,  0.2902E-07,  0.4582E-07,  0.7962E-07,  0.1370E-06, &
        0.2244E-06,  0.3470E-06,  0.5103E-06,  0.7184E-06,  0.9739E-06, &
        0.1282E-05,  0.1654E-05,  0.2090E-05,  0.2571E-05,  0.3064E-05, &
        0.3493E-05/
      data ((h81(ip,iw),iw=1,31), ip=12,12)/ &
        0.99998581,  0.99997181,  0.99994498,  0.99989486,  0.99980325, &
        0.99964696,  0.99939692,  0.99903113,  0.99853987,  0.99791837, &
        0.99715620,  0.99621618,  0.99501866,  0.99342209,  0.99122083, &
        0.98812902,  0.98375845,  0.97760117,  0.96901232,  0.95717090, &
        0.94102478,  0.91923845,  0.89015573,  0.85182822,  0.80212283, &
        0.73893917,  0.66078889,  0.56784910,  0.46305943,  0.35295039, &
        0.24754596/
      data ((h82(ip,iw),iw=1,31), ip=12,12)/ &
       -0.1148E-07, -0.2504E-07, -0.5813E-07, -0.1262E-06, -0.2788E-06, &
       -0.6155E-06, -0.1276E-05, -0.2415E-05, -0.4103E-05, -0.6273E-05, &
       -0.8763E-05, -0.1142E-04, -0.1426E-04, -0.1747E-04, -0.2138E-04, &
       -0.2655E-04, -0.3371E-04, -0.4353E-04, -0.5657E-04, -0.7343E-04, &
       -0.9507E-04, -0.1226E-03, -0.1568E-03, -0.1990E-03, -0.2505E-03, &
       -0.3115E-03, -0.3791E-03, -0.4464E-03, -0.5045E-03, -0.5442E-03, &
       -0.5540E-03/
      data ((h83(ip,iw),iw=1,31), ip=12,12)/ &
        0.1357E-09,  0.2927E-09,  0.6014E-09,  0.1131E-08,  0.2201E-08, &
        0.3943E-08,  0.6729E-08,  0.1011E-07,  0.1333E-07,  0.1609E-07, &
        0.1802E-07,  0.1914E-07,  0.2016E-07,  0.2283E-07,  0.2826E-07, &
        0.3678E-07,  0.5093E-07,  0.7664E-07,  0.1213E-06,  0.1930E-06, &
        0.3004E-06,  0.4507E-06,  0.6494E-06,  0.9001E-06,  0.1204E-05, &
        0.1566E-05,  0.1987E-05,  0.2454E-05,  0.2941E-05,  0.3392E-05, &
        0.3693E-05/
      data ((h81(ip,iw),iw=1,31), ip=13,13)/ &
        0.99998581,  0.99997181,  0.99994481,  0.99989444,  0.99980199, &
        0.99964291,  0.99938649,  0.99900728,  0.99848855,  0.99781942, &
        0.99697864,  0.99591231,  0.99451441,  0.99261320,  0.98995894, &
        0.98621172,  0.98090869,  0.97344768,  0.96305251,  0.94872272, &
        0.92918164,  0.90285385,  0.86788404,  0.82219505,  0.76365697, &
        0.69050038,  0.60221273,  0.50069368,  0.39117759,  0.28263843, &
        0.18615139/
      data ((h82(ip,iw),iw=1,31), ip=13,13)/ &
       -0.1148E-07, -0.2504E-07, -0.5664E-07, -0.1240E-06, -0.2702E-06, &
       -0.5918E-06, -0.1226E-05, -0.2323E-05, -0.3951E-05, -0.6043E-05, &
       -0.8440E-05, -0.1098E-04, -0.1373E-04, -0.1693E-04, -0.2100E-04, &
       -0.2655E-04, -0.3439E-04, -0.4522E-04, -0.5969E-04, -0.7858E-04, &
       -0.1030E-03, -0.1340E-03, -0.1728E-03, -0.2207E-03, -0.2784E-03, &
       -0.3443E-03, -0.4131E-03, -0.4765E-03, -0.5254E-03, -0.5491E-03, &
       -0.5326E-03/
      data ((h83(ip,iw),iw=1,31), ip=13,13)/ &
        0.1357E-09,  0.2927E-09,  0.6173E-09,  0.1112E-08,  0.2201E-08, &
        0.3941E-08,  0.6708E-08,  0.1004E-07,  0.1358E-07,  0.1674E-07, &
        0.1928E-07,  0.2167E-07,  0.2505E-07,  0.3128E-07,  0.4149E-07, &
        0.5627E-07,  0.7850E-07,  0.1144E-06,  0.1719E-06,  0.2613E-06, &
        0.3933E-06,  0.5765E-06,  0.8156E-06,  0.1113E-05,  0.1469E-05, &
        0.1882E-05,  0.2341E-05,  0.2821E-05,  0.3285E-05,  0.3644E-05, &
        0.3739E-05/
      data ((h81(ip,iw),iw=1,31), ip=14,14)/ &
        0.99998587,  0.99997181,  0.99994475,  0.99989372,  0.99979997, &
        0.99963719,  0.99937135,  0.99897152,  0.99841356,  0.99767524, &
        0.99672168,  0.99547845,  0.99381119,  0.99150914,  0.98827392, &
        0.98369658,  0.97722799,  0.96814531,  0.95550716,  0.93810284, &
        0.91441429,  0.88264406,  0.84078050,  0.78670061,  0.71846342, &
        0.63503206,  0.53738129,  0.42953157,  0.31922323,  0.21723127, &
        0.13354182/
      data ((h82(ip,iw),iw=1,31), ip=14,14)/ &
       -0.1177E-07, -0.2474E-07, -0.5574E-07, -0.1176E-06, -0.2560E-06, &
       -0.5614E-06, -0.1159E-05, -0.2203E-05, -0.3761E-05, -0.5769E-05, &
       -0.8067E-05, -0.1053E-04, -0.1327E-04, -0.1659E-04, -0.2098E-04, &
       -0.2712E-04, -0.3584E-04, -0.4798E-04, -0.6433E-04, -0.8582E-04, &
       -0.1136E-03, -0.1489E-03, -0.1932E-03, -0.2475E-03, -0.3111E-03, &
       -0.3800E-03, -0.4470E-03, -0.5032E-03, -0.5388E-03, -0.5400E-03, &
       -0.4891E-03/
      data ((h83(ip,iw),iw=1,31), ip=14,14)/ &
        0.1251E-09,  0.2874E-09,  0.5907E-09,  0.1136E-08,  0.2182E-08, &
        0.3909E-08,  0.6735E-08,  0.1018E-07,  0.1399E-07,  0.1783E-07, &
        0.2163E-07,  0.2610E-07,  0.3246E-07,  0.4315E-07,  0.5899E-07, &
        0.8117E-07,  0.1128E-06,  0.1606E-06,  0.2341E-06,  0.3453E-06, &
        0.5068E-06,  0.7277E-06,  0.1012E-05,  0.1359E-05,  0.1767E-05, &
        0.2223E-05,  0.2703E-05,  0.3175E-05,  0.3574E-05,  0.3768E-05, &
        0.3601E-05/
      data ((h81(ip,iw),iw=1,31), ip=15,15)/ &
        0.99998581,  0.99997181,  0.99994457,  0.99989307,  0.99979734, &
        0.99962950,  0.99935025,  0.99892145,  0.99830657,  0.99747115, &
        0.99636191,  0.99487901,  0.99285656,  0.99003839,  0.98606563, &
        0.98044837,  0.97252828,  0.96143234,  0.94602317,  0.92485082, &
        0.89616144,  0.85797250,  0.80818051,  0.74475378,  0.66627097, &
        0.57292521,  0.46762335,  0.35682863,  0.25054556,  0.15963095, &
        0.09080571/
      data ((h82(ip,iw),iw=1,31), ip=15,15)/ &
       -0.1118E-07, -0.2415E-07, -0.5321E-07, -0.1133E-06, -0.2410E-06, &
       -0.5236E-06, -0.1073E-05, -0.2053E-05, -0.3528E-05, -0.5460E-05, &
       -0.7694E-05, -0.1015E-04, -0.1299E-04, -0.1658E-04, -0.2147E-04, &
       -0.2839E-04, -0.3828E-04, -0.5213E-04, -0.7091E-04, -0.9566E-04, &
       -0.1276E-03, -0.1682E-03, -0.2189E-03, -0.2797E-03, -0.3476E-03, &
       -0.4167E-03, -0.4786E-03, -0.5240E-03, -0.5406E-03, -0.5107E-03, &
       -0.4220E-03/
      data ((h83(ip,iw),iw=1,31), ip=15,15)/ &
        0.1251E-09,  0.2927E-09,  0.5721E-09,  0.1118E-08,  0.2142E-08, &
        0.3824E-08,  0.6708E-08,  0.1043E-07,  0.1488E-07,  0.1979E-07, &
        0.2543E-07,  0.3260E-07,  0.4308E-07,  0.5882E-07,  0.8153E-07, &
        0.1124E-06,  0.1551E-06,  0.2174E-06,  0.3102E-06,  0.4479E-06, &
        0.6439E-06,  0.9071E-06,  0.1240E-05,  0.1640E-05,  0.2095E-05, &
        0.2579E-05,  0.3060E-05,  0.3491E-05,  0.3764E-05,  0.3721E-05, &
        0.3292E-05/
      data ((h81(ip,iw),iw=1,31), ip=16,16)/ &
        0.99998587,  0.99997181,  0.99994439,  0.99989206,  0.99979407, &
        0.99961913,  0.99932277,  0.99885392,  0.99816269,  0.99719489, &
        0.99587458,  0.99407625,  0.99159390,  0.98812038,  0.98322064, &
        0.97630697,  0.96658587,  0.95300376,  0.93419170,  0.90845579, &
        0.87383258,  0.82820439,  0.76947904,  0.69596839,  0.60722947, &
        0.50518250,  0.39504176,  0.28574610,  0.18839723,  0.11161584, &
        0.05787951/
      data ((h82(ip,iw),iw=1,31), ip=16,16)/ &
       -0.1163E-07, -0.2355E-07, -0.5142E-07, -0.1075E-06, -0.2262E-06, &
       -0.4817E-06, -0.9818E-06, -0.1885E-05, -0.3285E-05, -0.5158E-05, &
       -0.7401E-05, -0.9958E-05, -0.1303E-04, -0.1705E-04, -0.2262E-04, &
       -0.3058E-04, -0.4198E-04, -0.5804E-04, -0.7992E-04, -0.1087E-03, &
       -0.1457E-03, -0.1926E-03, -0.2501E-03, -0.3165E-03, -0.3865E-03, &
       -0.4524E-03, -0.5056E-03, -0.5353E-03, -0.5249E-03, -0.4571E-03, &
       -0.3378E-03/
      data ((h83(ip,iw),iw=1,31), ip=16,16)/ &
        0.1171E-09,  0.2980E-09,  0.5402E-09,  0.1104E-08,  0.2091E-08, &
        0.3848E-08,  0.6769E-08,  0.1094E-07,  0.1632E-07,  0.2289E-07, &
        0.3108E-07,  0.4197E-07,  0.5722E-07,  0.7897E-07,  0.1097E-06, &
        0.1512E-06,  0.2073E-06,  0.2871E-06,  0.4038E-06,  0.5728E-06, &
        0.8079E-06,  0.1117E-05,  0.1502E-05,  0.1953E-05,  0.2443E-05, &
        0.2937E-05,  0.3394E-05,  0.3732E-05,  0.3804E-05,  0.3491E-05, &
        0.2856E-05/
      data ((h81(ip,iw),iw=1,31), ip=17,17)/ &
        0.99998587,  0.99997187,  0.99994391,  0.99989080,  0.99978989, &
        0.99960697,  0.99928856,  0.99877006,  0.99797958,  0.99683803, &
        0.99524260,  0.99303532,  0.98996651,  0.98566484,  0.97960740, &
        0.97108519,  0.95913982,  0.94250381,  0.91956043,  0.88838351, &
        0.84684128,  0.79275382,  0.72422367,  0.64030874,  0.54205990, &
        0.43354255,  0.32249713,  0.21969658,  0.13518941,  0.07362670, &
        0.03405166/
      data ((h82(ip,iw),iw=1,31), ip=17,17)/ &
       -0.1088E-07, -0.2191E-07, -0.5053E-07, -0.1018E-06, -0.2113E-06, &
       -0.4407E-06, -0.8942E-06, -0.1725E-05, -0.3064E-05, -0.4925E-05, &
       -0.7258E-05, -0.1005E-04, -0.1351E-04, -0.1814E-04, -0.2462E-04, &
       -0.3391E-04, -0.4726E-04, -0.6615E-04, -0.9188E-04, -0.1257E-03, &
       -0.1688E-03, -0.2227E-03, -0.2867E-03, -0.3567E-03, -0.4256E-03, &
       -0.4849E-03, -0.5251E-03, -0.5319E-03, -0.4859E-03, -0.3816E-03, &
       -0.2488E-03/
      data ((h83(ip,iw),iw=1,31), ip=17,17)/ &
        0.1251E-09,  0.2634E-09,  0.5774E-09,  0.1099E-08,  0.2107E-08, &
        0.3856E-08,  0.7067E-08,  0.1178E-07,  0.1845E-07,  0.2726E-07, &
        0.3877E-07,  0.5429E-07,  0.7531E-07,  0.1046E-06,  0.1451E-06, &
        0.1993E-06,  0.2721E-06,  0.3732E-06,  0.5183E-06,  0.7233E-06, &
        0.1001E-05,  0.1360E-05,  0.1797E-05,  0.2289E-05,  0.2798E-05, &
        0.3281E-05,  0.3673E-05,  0.3849E-05,  0.3662E-05,  0.3112E-05, &
        0.2335E-05/
      data ((h81(ip,iw),iw=1,31), ip=18,18)/ &
        0.99998593,  0.99997187,  0.99994367,  0.99988943,  0.99978596, &
        0.99959368,  0.99925119,  0.99867415,  0.99776298,  0.99640357, &
        0.99445862,  0.99173003,  0.98791820,  0.98257768,  0.97508043, &
        0.96456927,  0.94989133,  0.92953795,  0.90164959,  0.86409807, &
        0.81463462,  0.75114733,  0.67224920,  0.57824445,  0.47214776, &
        0.36050701,  0.25336337,  0.16159421,  0.09200191,  0.04520833, &
        0.01819801/
      data ((h82(ip,iw),iw=1,31), ip=18,18)/ &
       -0.1103E-07, -0.2206E-07, -0.4859E-07, -0.9792E-07, -0.1994E-06, &
       -0.4061E-06, -0.8228E-06, -0.1602E-05, -0.2904E-05, -0.4815E-05, &
       -0.7357E-05, -0.1053E-04, -0.1457E-04, -0.2000E-04, -0.2766E-04, &
       -0.3866E-04, -0.5450E-04, -0.7686E-04, -0.1073E-03, -0.1471E-03, &
       -0.1976E-03, -0.2587E-03, -0.3277E-03, -0.3985E-03, -0.4627E-03, &
       -0.5115E-03, -0.5326E-03, -0.5076E-03, -0.4222E-03, -0.2941E-03, &
       -0.1681E-03/
      data ((h83(ip,iw),iw=1,31), ip=18,18)/ &
        0.1224E-09,  0.2448E-09,  0.5748E-09,  0.1142E-08,  0.2123E-08, &
        0.3967E-08,  0.7416E-08,  0.1287E-07,  0.2113E-07,  0.3276E-07, &
        0.4854E-07,  0.6982E-07,  0.9833E-07,  0.1366E-06,  0.1888E-06, &
        0.2589E-06,  0.3522E-06,  0.4798E-06,  0.6576E-06,  0.9025E-06, &
        0.1227E-05,  0.1636E-05,  0.2118E-05,  0.2637E-05,  0.3148E-05, &
        0.3589E-05,  0.3856E-05,  0.3798E-05,  0.3352E-05,  0.2628E-05, &
        0.1767E-05/
      data ((h81(ip,iw),iw=1,31), ip=19,19)/ &
        0.99998593,  0.99997181,  0.99994355,  0.99988830,  0.99978203, &
        0.99958068,  0.99921370,  0.99857539,  0.99752712,  0.99590880, &
        0.99353248,  0.99015075,  0.98540407,  0.97876346,  0.96947908, &
        0.95651841,  0.93850368,  0.91368133,  0.87997156,  0.83508462, &
        0.77673703,  0.70312166,  0.61380792,  0.51084727,  0.39962953, &
        0.28925329,  0.19089335,  0.11320567,  0.05874455,  0.02542299, &
        0.00871336/
      data ((h82(ip,iw),iw=1,31), ip=19,19)/ &
       -0.1028E-07, -0.2236E-07, -0.4829E-07, -0.9345E-07, -0.1912E-06, &
       -0.3889E-06, -0.7758E-06, -0.1525E-05, -0.2827E-05, -0.4873E-05, &
       -0.7740E-05, -0.1148E-04, -0.1633E-04, -0.2283E-04, -0.3198E-04, &
       -0.4514E-04, -0.6403E-04, -0.9054E-04, -0.1266E-03, -0.1736E-03, &
       -0.2321E-03, -0.2999E-03, -0.3716E-03, -0.4398E-03, -0.4956E-03, &
       -0.5286E-03, -0.5222E-03, -0.4579E-03, -0.3401E-03, -0.2080E-03, &
       -0.1039E-03/
      data ((h83(ip,iw),iw=1,31), ip=19,19)/ &
        0.1304E-09,  0.2608E-09,  0.5535E-09,  0.1152E-08,  0.2142E-08, &
        0.4122E-08,  0.7799E-08,  0.1397E-07,  0.2408E-07,  0.3910E-07, &
        0.6004E-07,  0.8860E-07,  0.1265E-06,  0.1766E-06,  0.2432E-06, &
        0.3327E-06,  0.4518E-06,  0.6112E-06,  0.8261E-06,  0.1114E-05, &
        0.1486E-05,  0.1943E-05,  0.2458E-05,  0.2987E-05,  0.3474E-05, &
        0.3824E-05,  0.3892E-05,  0.3567E-05,  0.2915E-05,  0.2077E-05, &
        0.1211E-05/
      data ((h81(ip,iw),iw=1,31), ip=20,20)/ &
        0.99998593,  0.99997187,  0.99994338,  0.99988759,  0.99977875, &
        0.99956948,  0.99917996,  0.99848181,  0.99729222,  0.99538499, &
        0.99249852,  0.98831391,  0.98240149,  0.97413868,  0.96263647, &
        0.94666839,  0.92461574,  0.89449459,  0.85404098,  0.80086255, &
        0.73280096,  0.64872819,  0.54967648,  0.43987310,  0.32735765, &
        0.22317076,  0.13746834,  0.07493556,  0.03466660,  0.01290333, &
        0.00367731/
      data ((h82(ip,iw),iw=1,31), ip=20,20)/ &
       -0.1043E-07, -0.2102E-07, -0.4889E-07, -0.9315E-07, -0.1857E-06, &
       -0.3780E-06, -0.7543E-06, -0.1486E-05, -0.2823E-05, -0.5051E-05, &
       -0.8375E-05, -0.1290E-04, -0.1887E-04, -0.2681E-04, -0.3784E-04, &
       -0.5366E-04, -0.7619E-04, -0.1076E-03, -0.1501E-03, -0.2052E-03, &
       -0.2717E-03, -0.3446E-03, -0.4162E-03, -0.4783E-03, -0.5213E-03, &
       -0.5309E-03, -0.4878E-03, -0.3851E-03, -0.2521E-03, -0.1350E-03, &
       -0.5807E-04/
      data ((h83(ip,iw),iw=1,31), ip=20,20)/ &
        0.1384E-09,  0.2741E-09,  0.5748E-09,  0.1131E-08,  0.2161E-08, &
        0.4247E-08,  0.8188E-08,  0.1499E-07,  0.2674E-07,  0.4548E-07, &
        0.7259E-07,  0.1102E-06,  0.1602E-06,  0.2255E-06,  0.3108E-06, &
        0.4239E-06,  0.5746E-06,  0.7721E-06,  0.1029E-05,  0.1361E-05, &
        0.1781E-05,  0.2275E-05,  0.2807E-05,  0.3325E-05,  0.3749E-05, &
        0.3939E-05,  0.3748E-05,  0.3187E-05,  0.2391E-05,  0.1504E-05, &
        0.7362E-06/
      data ((h81(ip,iw),iw=1,31), ip=21,21)/ &
        0.99998617,  0.99997193,  0.99994338,  0.99988717,  0.99977612, &
        0.99956113,  0.99915254,  0.99840069,  0.99707633,  0.99486989, &
        0.99141109,  0.98627210,  0.97892439,  0.96864319,  0.95439541, &
        0.93474925,  0.90785789,  0.87153333,  0.82338101,  0.76101756, &
        0.68269420,  0.58842880,  0.48118341,  0.36774790,  0.25862980, &
        0.16510296,  0.09410405,  0.04627281,  0.01861316,  0.00583249, &
        0.00132960/
      data ((h82(ip,iw),iw=1,31), ip=21,21)/ &
       -0.1014E-07, -0.2176E-07, -0.4680E-07, -0.9211E-07, -0.1844E-06, &
       -0.3731E-06, -0.7439E-06, -0.1477E-05, -0.2852E-05, -0.5281E-05, &
       -0.9151E-05, -0.1469E-04, -0.2215E-04, -0.3205E-04, -0.4554E-04, &
       -0.6453E-04, -0.9130E-04, -0.1283E-03, -0.1781E-03, -0.2413E-03, &
       -0.3147E-03, -0.3905E-03, -0.4593E-03, -0.5116E-03, -0.5354E-03, &
       -0.5122E-03, -0.4277E-03, -0.2991E-03, -0.1715E-03, -0.7979E-04, &
       -0.2831E-04/
      data ((h83(ip,iw),iw=1,31), ip=21,21)/ &
        0.1224E-09,  0.3033E-09,  0.5588E-09,  0.1118E-08,  0.2206E-08, &
        0.4271E-08,  0.8374E-08,  0.1586E-07,  0.2918E-07,  0.5135E-07, &
        0.8524E-07,  0.1338E-06,  0.1993E-06,  0.2847E-06,  0.3940E-06, &
        0.5365E-06,  0.7246E-06,  0.9678E-06,  0.1272E-05,  0.1651E-05, &
        0.2111E-05,  0.2629E-05,  0.3160E-05,  0.3635E-05,  0.3934E-05, &
        0.3890E-05,  0.3441E-05,  0.2703E-05,  0.1820E-05,  0.9766E-06, &
        0.3839E-06/
      data ((h81(ip,iw),iw=1,31), ip=22,22)/ &
        0.99998623,  0.99997211,  0.99994355,  0.99988687,  0.99977422, &
        0.99955481,  0.99913073,  0.99833500,  0.99689233,  0.99439919, &
        0.99033678,  0.98411196,  0.97504222,  0.96227551,  0.94463825, &
        0.92052001,  0.88788027,  0.84437561,  0.78755522,  0.71526611, &
        0.62658077,  0.52313668,  0.41026467,  0.29745829,  0.19654948, &
        0.11670119,  0.06062329,  0.02623105,  0.00896722,  0.00229371, &
        0.00038755/
      data ((h82(ip,iw),iw=1,31), ip=22,22)/ &
       -0.9391E-08, -0.2221E-07, -0.4665E-07, -0.9226E-07, -0.1830E-06, &
       -0.3735E-06, -0.7406E-06, -0.1474E-05, -0.2892E-05, -0.5499E-05, &
       -0.9920E-05, -0.1666E-04, -0.2607E-04, -0.3859E-04, -0.5529E-04, &
       -0.7813E-04, -0.1098E-03, -0.1532E-03, -0.2105E-03, -0.2809E-03, &
       -0.3591E-03, -0.4353E-03, -0.4984E-03, -0.5362E-03, -0.5320E-03, &
       -0.4676E-03, -0.3482E-03, -0.2137E-03, -0.1069E-03, -0.4211E-04, &
       -0.1145E-04/
      data ((h83(ip,iw),iw=1,31), ip=22,22)/ &
        0.1623E-09,  0.3007E-09,  0.5721E-09,  0.1110E-08,  0.2203E-08, &
        0.4343E-08,  0.8640E-08,  0.1645E-07,  0.3095E-07,  0.5619E-07, &
        0.9716E-07,  0.1580E-06,  0.2425E-06,  0.3535E-06,  0.4947E-06, &
        0.6742E-06,  0.9064E-06,  0.1202E-05,  0.1563E-05,  0.1989E-05, &
        0.2478E-05,  0.3003E-05,  0.3509E-05,  0.3891E-05,  0.3985E-05, &
        0.3668E-05,  0.3010E-05,  0.2153E-05,  0.1258E-05,  0.5557E-06, &
        0.1624E-06/
      data ((h81(ip,iw),iw=1,31), ip=23,23)/ &
        0.99998647,  0.99997246,  0.99994379,  0.99988651,  0.99977303, &
        0.99954993,  0.99911475,  0.99828470,  0.99674338,  0.99399304, &
        0.98934013,  0.98194593,  0.97088563,  0.95511639,  0.93333685, &
        0.90381682,  0.86439717,  0.81265670,  0.74623829,  0.66356468, &
        0.56501788,  0.45429623,  0.33944970,  0.23212188,  0.14330000, &
        0.07824194,  0.03621233,  0.01344591,  0.00381404,  0.00075638, &
        0.00008333/
      data ((h82(ip,iw),iw=1,31), ip=23,23)/ &
       -0.8496E-08, -0.2117E-07, -0.4620E-07, -0.9330E-07, -0.1859E-06, &
       -0.3687E-06, -0.7355E-06, -0.1475E-05, -0.2920E-05, -0.5659E-05, &
       -0.1056E-04, -0.1855E-04, -0.3028E-04, -0.4621E-04, -0.6714E-04, &
       -0.9485E-04, -0.1323E-03, -0.1827E-03, -0.2471E-03, -0.3227E-03, &
       -0.4027E-03, -0.4763E-03, -0.5304E-03, -0.5469E-03, -0.5048E-03, &
       -0.3992E-03, -0.2618E-03, -0.1405E-03, -0.6043E-04, -0.1900E-04, &
       -0.3491E-05/
      data ((h83(ip,iw),iw=1,31), ip=23,23)/ &
        0.1410E-09,  0.2767E-09,  0.5588E-09,  0.1128E-08,  0.2238E-08, &
        0.4332E-08,  0.8672E-08,  0.1688E-07,  0.3235E-07,  0.6024E-07, &
        0.1075E-06,  0.1815E-06,  0.2882E-06,  0.4314E-06,  0.6134E-06, &
        0.8399E-06,  0.1124E-05,  0.1479E-05,  0.1902E-05,  0.2376E-05, &
        0.2880E-05,  0.3391E-05,  0.3833E-05,  0.4049E-05,  0.3874E-05, &
        0.3311E-05,  0.2500E-05,  0.1580E-05,  0.7735E-06,  0.2652E-06, &
        0.5064E-07/
      data ((h81(ip,iw),iw=1,31), ip=24,24)/ &
        0.99998665,  0.99997264,  0.99994391,  0.99988639,  0.99977225, &
        0.99954611,  0.99910295,  0.99824727,  0.99662870,  0.99366271, &
        0.98846823,  0.97989517,  0.96664554,  0.94737148,  0.92061591, &
        0.88462359,  0.83725178,  0.77615321,  0.69933873,  0.60623604, &
        0.49901956,  0.38394505,  0.27167684,  0.17430288,  0.09971440, &
        0.04912645,  0.01973170,  0.00614691,  0.00139344,  0.00019175, &
        0.00001121/
      data ((h82(ip,iw),iw=1,31), ip=24,24)/ &
       -0.8050E-08, -0.2102E-07, -0.4442E-07, -0.9465E-07, -0.1893E-06, &
       -0.3659E-06, -0.7344E-06, -0.1480E-05, -0.2933E-05, -0.5753E-05, &
       -0.1101E-04, -0.2013E-04, -0.3439E-04, -0.5447E-04, -0.8092E-04, &
       -0.1150E-03, -0.1594E-03, -0.2169E-03, -0.2871E-03, -0.3654E-03, &
       -0.4440E-03, -0.5117E-03, -0.5510E-03, -0.5365E-03, -0.4515E-03, &
       -0.3166E-03, -0.1818E-03, -0.8448E-04, -0.2990E-04, -0.6816E-05, &
       -0.7165E-06/
      data ((h83(ip,iw),iw=1,31), ip=24,24)/ &
        0.1490E-09,  0.3007E-09,  0.5694E-09,  0.1147E-08,  0.2219E-08, &
        0.4393E-08,  0.8805E-08,  0.1708E-07,  0.3326E-07,  0.6308E-07, &
        0.1159E-06,  0.2028E-06,  0.3338E-06,  0.5153E-06,  0.7484E-06, &
        0.1035E-05,  0.1382E-05,  0.1802E-05,  0.2288E-05,  0.2806E-05, &
        0.3317E-05,  0.3780E-05,  0.4091E-05,  0.4066E-05,  0.3619E-05, &
        0.2864E-05,  0.1944E-05,  0.1046E-05,  0.4099E-06,  0.9938E-07, &
        0.9939E-08/
      data ((h81(ip,iw),iw=1,31), ip=25,25)/ &
        0.99998695,  0.99997276,  0.99994379,  0.99988675,  0.99977160, &
        0.99954373,  0.99909455,  0.99822062,  0.99654251,  0.99340349, &
        0.98774564,  0.97806484,  0.96255350,  0.93937284,  0.90680546, &
        0.86315435,  0.80651176,  0.73489875,  0.64710438,  0.54404056, &
        0.43020850,  0.31471944,  0.20977807,  0.12548858,  0.06554347, &
        0.02840608,  0.00965798,  0.00244677,  0.00041175,  0.00003231, &
        0.00000018/
      data ((h82(ip,iw),iw=1,31), ip=25,25)/ &
       -0.7454E-08, -0.2146E-07, -0.4293E-07, -0.9375E-07, -0.1821E-06, &
       -0.3696E-06, -0.7405E-06, -0.1476E-05, -0.2937E-05, -0.5806E-05, &
       -0.1130E-04, -0.2129E-04, -0.3793E-04, -0.6274E-04, -0.9615E-04, &
       -0.1386E-03, -0.1914E-03, -0.2558E-03, -0.3298E-03, -0.4079E-03, &
       -0.4819E-03, -0.5388E-03, -0.5541E-03, -0.4995E-03, -0.3772E-03, &
       -0.2329E-03, -0.1164E-03, -0.4557E-04, -0.1236E-04, -0.1755E-05, &
       -0.5057E-07/
      data ((h83(ip,iw),iw=1,31), ip=25,25)/ &
        0.1809E-09,  0.3246E-09,  0.5960E-09,  0.1120E-08,  0.2246E-08, &
        0.4391E-08,  0.8829E-08,  0.1731E-07,  0.3393E-07,  0.6527E-07, &
        0.1223E-06,  0.2207E-06,  0.3764E-06,  0.6008E-06,  0.8961E-06, &
        0.1258E-05,  0.1683E-05,  0.2173E-05,  0.2716E-05,  0.3269E-05, &
        0.3771E-05,  0.4136E-05,  0.4230E-05,  0.3926E-05,  0.3258E-05, &
        0.2357E-05,  0.1385E-05,  0.6107E-06,  0.1776E-06,  0.2568E-07, &
        0.6839E-09/
      data ((h81(ip,iw),iw=1,31), ip=26,26)/ &
        0.99998742,  0.99997318,  0.99994421,  0.99988669,  0.99977106, &
        0.99954182,  0.99908882,  0.99820071,  0.99648011,  0.99320894, &
        0.98717427,  0.97652453,  0.95883882,  0.93154693,  0.89246184, &
        0.83993518,  0.77259064,  0.68930018,  0.59020877,  0.47830009, &
        0.36090946,  0.24956739,  0.15586823,  0.08599961,  0.04009032, &
        0.01487154,  0.00417018,  0.00081682,  0.00008935,  0.00000226, &
        0.00000000/
      data ((h82(ip,iw),iw=1,31), ip=26,26)/ &
       -0.9540E-08, -0.2102E-07, -0.4546E-07, -0.9330E-07, -0.1793E-06, &
       -0.3664E-06, -0.7400E-06, -0.1476E-05, -0.2941E-05, -0.5832E-05, &
       -0.1147E-04, -0.2206E-04, -0.4070E-04, -0.7030E-04, -0.1119E-03, &
       -0.1649E-03, -0.2279E-03, -0.2990E-03, -0.3744E-03, -0.4494E-03, &
       -0.5153E-03, -0.5537E-03, -0.5334E-03, -0.4368E-03, -0.2936E-03, &
       -0.1593E-03, -0.6801E-04, -0.2110E-04, -0.3891E-05, -0.2838E-06, &
       -0.2863E-08/
      data ((h83(ip,iw),iw=1,31), ip=26,26)/ &
        0.1437E-09,  0.2634E-09,  0.5721E-09,  0.1134E-08,  0.2169E-08, &
        0.4476E-08,  0.8895E-08,  0.1750E-07,  0.3438E-07,  0.6677E-07, &
        0.1272E-06,  0.2349E-06,  0.4139E-06,  0.6836E-06,  0.1050E-05, &
        0.1501E-05,  0.2020E-05,  0.2590E-05,  0.3184E-05,  0.3752E-05, &
        0.4204E-05,  0.4400E-05,  0.4212E-05,  0.3651E-05,  0.2814E-05, &
        0.1808E-05,  0.8844E-06,  0.2999E-06,  0.5742E-07,  0.3813E-08, &
        0.5821E-10/
      data ((c1(ip,iw),iw=1,30), ip= 1, 1)/ &
        0.99986166,  0.99977082,  0.99964607,  0.99948418,  0.99927646, &
        0.99899888,  0.99861085,  0.99806982,  0.99734622,  0.99642432, &
        0.99527818,  0.99384803,  0.99204856,  0.98977995,  0.98691165, &
        0.98327899,  0.97869217,  0.97294933,  0.96584505,  0.95713884, &
        0.94650167,  0.93346804,  0.91738296,  0.89743340,  0.87275159, &
        0.84253013,  0.80613744,  0.76331758,  0.71443796,  0.66071773/
      data ((c2(ip,iw),iw=1,30), ip= 1, 1)/ &
       -0.2292E-06, -0.5441E-06, -0.1141E-05, -0.2114E-05, -0.3578E-05, &
       -0.5719E-05, -0.8775E-05, -0.1292E-04, -0.1827E-04, -0.2504E-04, &
       -0.3377E-04, -0.4530E-04, -0.6053E-04, -0.8030E-04, -0.1054E-03, &
       -0.1368E-03, -0.1751E-03, -0.2213E-03, -0.2765E-03, -0.3421E-03, &
       -0.4203E-03, -0.5130E-03, -0.6215E-03, -0.7460E-03, -0.8855E-03, &
       -0.1037E-02, -0.1198E-02, -0.1364E-02, -0.1530E-02, -0.1694E-02/  
      data ((c3(ip,iw),iw=1,30), ip= 1, 1)/ &
        0.5694E-09,  0.8648E-09,  0.6918E-09, -0.5242E-09, -0.3169E-08, &
       -0.7496E-08, -0.1311E-07, -0.1994E-07, -0.2963E-07, -0.4599E-07, &
       -0.7262E-07, -0.1094E-06, -0.1551E-06, -0.2125E-06, -0.2868E-06, &
       -0.3813E-06, -0.4972E-06, -0.6346E-06, -0.7967E-06, -0.9906E-06, &
       -0.1225E-05, -0.1499E-05, -0.1801E-05, -0.2108E-05, -0.2397E-05, &
       -0.2646E-05, -0.2826E-05, -0.2908E-05, -0.2848E-05, -0.2574E-05/  
      data ((c1(ip,iw),iw=1,30), ip= 2, 2)/ &
        0.99986154,  0.99977070,  0.99964583,  0.99948364,  0.99927574, &
        0.99899709,  0.99860787,  0.99806446,  0.99733645,  0.99640572, &
        0.99524444,  0.99378759,  0.99194312,  0.98959959,  0.98661262, &
        0.98280418,  0.97796720,  0.97187805,  0.96429664,  0.95493060, &
        0.94338161,  0.92908537,  0.91127324,  0.88903010,  0.86143875, &
        0.82772064,  0.78741211,  0.74061924,  0.68828899,  0.63231009/  
      data ((c2(ip,iw),iw=1,30), ip= 2, 2)/ &
       -0.2289E-06, -0.5435E-06, -0.1140E-05, -0.2113E-05, -0.3574E-05, &
       -0.5713E-05, -0.8764E-05, -0.1290E-04, -0.1824E-04, -0.2497E-04, &
       -0.3366E-04, -0.4512E-04, -0.6025E-04, -0.7992E-04, -0.1050E-03, &
       -0.1365E-03, -0.1751E-03, -0.2218E-03, -0.2779E-03, -0.3448E-03, &
       -0.4246E-03, -0.5193E-03, -0.6301E-03, -0.7573E-03, -0.8997E-03, &
       -0.1054E-02, -0.1217E-02, -0.1384E-02, -0.1552E-02, -0.1720E-02/  
      data ((c3(ip,iw),iw=1,30), ip= 2, 2)/ &
        0.5854E-09,  0.8701E-09,  0.6839E-09, -0.5136E-09, -0.3198E-08, &
       -0.7485E-08, -0.1314E-07, -0.2000E-07, -0.2978E-07, -0.4609E-07, &
       -0.7287E-07, -0.1097E-06, -0.1556E-06, -0.2129E-06, -0.2863E-06, &
       -0.3798E-06, -0.4941E-06, -0.6301E-06, -0.7907E-06, -0.9828E-06, &
       -0.1214E-05, -0.1483E-05, -0.1774E-05, -0.2065E-05, -0.2330E-05, &
       -0.2542E-05, -0.2672E-05, -0.2685E-05, -0.2526E-05, -0.2117E-05/
      data ((c1(ip,iw),iw=1,30), ip= 3, 3)/ &
        0.99986154,  0.99977064,  0.99964547,  0.99948299,  0.99927431, &
        0.99899471,  0.99860352,  0.99805611,  0.99732059,  0.99637693, &
        0.99519277,  0.99369639,  0.99178660,  0.98933846,  0.98619473, &
        0.98215890,  0.97700489,  0.97047842,  0.96229303,  0.95209277, &
        0.93938899,  0.92350262,  0.90354991,  0.87854189,  0.84755611, &
        0.80992901,  0.76549649,  0.71489191,  0.65975630,  0.60250735/
      data ((c2(ip,iw),iw=1,30), ip= 3, 3)/ &
       -0.2286E-06, -0.5432E-06, -0.1138E-05, -0.2108E-05, -0.3567E-05, &
       -0.5703E-05, -0.8748E-05, -0.1288E-04, -0.1819E-04, -0.2489E-04, &
       -0.3351E-04, -0.4488E-04, -0.5991E-04, -0.7953E-04, -0.1047E-03, &
       -0.1364E-03, -0.1755E-03, -0.2231E-03, -0.2803E-03, -0.3489E-03, &
       -0.4307E-03, -0.5280E-03, -0.6419E-03, -0.7726E-03, -0.9187E-03, &
       -0.1077E-02, -0.1242E-02, -0.1412E-02, -0.1584E-02, -0.1756E-02/
      data ((c3(ip,iw),iw=1,30), ip= 3, 3)/ &
        0.5694E-09,  0.8595E-09,  0.6945E-09, -0.5375E-09, -0.3206E-08, &
       -0.7549E-08, -0.1322E-07, -0.2015E-07, -0.2989E-07, -0.4635E-07, &
       -0.7319E-07, -0.1101E-06, -0.1559E-06, -0.2126E-06, -0.2852E-06, &
       -0.3773E-06, -0.4904E-06, -0.6250E-06, -0.7842E-06, -0.9742E-06, &
       -0.1202E-05, -0.1463E-05, -0.1743E-05, -0.2014E-05, -0.2250E-05, &
       -0.2418E-05, -0.2489E-05, -0.2417E-05, -0.2135E-05, -0.1582E-05/
      data ((c1(ip,iw),iw=1,30), ip= 4, 4)/ &
        0.99986136,  0.99977046,  0.99964488,  0.99948180,  0.99927211, &
        0.99899107,  0.99859619,  0.99804264,  0.99729609,  0.99633241, &
        0.99511319,  0.99355984,  0.99155855,  0.98897028,  0.98562151, &
        0.98129672,  0.97574216,  0.96866274,  0.95971423,  0.94845927, &
        0.93429822,  0.91642857,  0.89386898,  0.86559713,  0.83075762, &
        0.78892154,  0.74038720,  0.68647277,  0.62947410,  0.57195354/
      data ((c2(ip,iw),iw=1,30), ip= 4, 4)/ &
       -0.2277E-06, -0.5415E-06, -0.1134E-05, -0.2101E-05, -0.3559E-05, &
       -0.5689E-05, -0.8723E-05, -0.1284E-04, -0.1812E-04, -0.2476E-04, &
       -0.3331E-04, -0.4459E-04, -0.5955E-04, -0.7921E-04, -0.1046E-03, &
       -0.1368E-03, -0.1766E-03, -0.2253E-03, -0.2842E-03, -0.3548E-03, &
       -0.4394E-03, -0.5399E-03, -0.6578E-03, -0.7931E-03, -0.9436E-03, &
       -0.1106E-02, -0.1275E-02, -0.1449E-02, -0.1626E-02, -0.1801E-02/
      data ((c3(ip,iw),iw=1,30), ip= 4, 4)/ &
        0.5801E-09,  0.8355E-09,  0.6812E-09, -0.5375E-09, -0.3238E-08, &
       -0.7647E-08, -0.1328E-07, -0.2021E-07, -0.3005E-07, -0.4652E-07, &
       -0.7339E-07, -0.1102E-06, -0.1557E-06, -0.2116E-06, -0.2832E-06, &
       -0.3740E-06, -0.4861E-06, -0.6196E-06, -0.7775E-06, -0.9650E-06, &
       -0.1187E-05, -0.1440E-05, -0.1705E-05, -0.1953E-05, -0.2153E-05, &
       -0.2272E-05, -0.2270E-05, -0.2090E-05, -0.1667E-05, -0.9819E-06/
      data ((c1(ip,iw),iw=1,30), ip= 5, 5)/ &
        0.99986118,  0.99977005,  0.99964374,  0.99948001,  0.99926865, &
        0.99898481,  0.99858499,  0.99802184,  0.99725801,  0.99626452, &
        0.99499524,  0.99336094,  0.99123430,  0.98846263,  0.98485070, &
        0.98015988,  0.97409874,  0.96632105,  0.95640951,  0.94382304, &
        0.92783535,  0.90752852,  0.88185948,  0.84983021,  0.81075883, &
        0.76460648,  0.71231532,  0.65593779,  0.59815186,  0.54105949/
      data ((c2(ip,iw),iw=1,30), ip= 5, 5)/ &
       -0.2258E-06, -0.5382E-06, -0.1128E-05, -0.2094E-05, -0.3545E-05, &
       -0.5666E-05, -0.8687E-05, -0.1278E-04, -0.1801E-04, -0.2460E-04, &
       -0.3306E-04, -0.4426E-04, -0.5924E-04, -0.7908E-04, -0.1049E-03, &
       -0.1377E-03, -0.1786E-03, -0.2289E-03, -0.2898E-03, -0.3632E-03, &
       -0.4512E-03, -0.5560E-03, -0.6790E-03, -0.8198E-03, -0.9757E-03, &
       -0.1143E-02, -0.1317E-02, -0.1497E-02, -0.1678E-02, -0.1854E-02/
      data ((c3(ip,iw),iw=1,30), ip= 5, 5)/ &
        0.5774E-09,  0.8302E-09,  0.7185E-09, -0.5854E-09, -0.3265E-08, &
       -0.7690E-08, -0.1341E-07, -0.2040E-07, -0.3027E-07, -0.4677E-07, &
       -0.7360E-07, -0.1101E-06, -0.1548E-06, -0.2099E-06, -0.2804E-06, &
       -0.3703E-06, -0.4815E-06, -0.6143E-06, -0.7707E-06, -0.9550E-06, &
       -0.1171E-05, -0.1413E-05, -0.1660E-05, -0.1880E-05, -0.2038E-05, &
       -0.2096E-05, -0.2001E-05, -0.1690E-05, -0.1126E-05, -0.3440E-06/
      data ((c1(ip,iw),iw=1,30), ip= 6, 6)/ &
        0.99986094,  0.99976933,  0.99964249,  0.99947715,  0.99926335, &
        0.99897510,  0.99856734,  0.99798959,  0.99719977,  0.99616224, &
        0.99482149,  0.99307603,  0.99078435,  0.98777461,  0.98382956, &
        0.97867453,  0.97197431,  0.96331739,  0.95219171,  0.93793094, &
        0.91968191,  0.89643741,  0.86714232,  0.83091533,  0.78739101, &
        0.73710698,  0.68177342,  0.62401903,  0.56637585,  0.50983685/
      data ((c2(ip,iw),iw=1,30), ip= 6, 6)/ &
       -0.2229E-06, -0.5348E-06, -0.1119E-05, -0.2079E-05, -0.3522E-05, &
       -0.5632E-05, -0.8631E-05, -0.1269E-04, -0.1788E-04, -0.2439E-04, &
       -0.3278E-04, -0.4397E-04, -0.5907E-04, -0.7926E-04, -0.1057E-03, &
       -0.1395E-03, -0.1819E-03, -0.2341E-03, -0.2978E-03, -0.3746E-03, &
       -0.4671E-03, -0.5774E-03, -0.7067E-03, -0.8539E-03, -0.1016E-02, &
       -0.1189E-02, -0.1370E-02, -0.1555E-02, -0.1739E-02, -0.1907E-02/
      data ((c3(ip,iw),iw=1,30), ip= 6, 6)/ &
        0.5748E-09,  0.8435E-09,  0.6679E-09, -0.6359E-09, -0.3345E-08, &
       -0.7804E-08, -0.1359E-07, -0.2062E-07, -0.3052E-07, -0.4688E-07, &
       -0.7340E-07, -0.1093E-06, -0.1531E-06, -0.2073E-06, -0.2771E-06, &
       -0.3662E-06, -0.4769E-06, -0.6093E-06, -0.7639E-06, -0.9439E-06, &
       -0.1152E-05, -0.1381E-05, -0.1606E-05, -0.1793E-05, -0.1901E-05, &
       -0.1881E-05, -0.1669E-05, -0.1216E-05, -0.5342E-06,  0.3075E-06/
      data ((c1(ip,iw),iw=1,30), ip= 7, 7)/ &
        0.99986053,  0.99976838,  0.99964023,  0.99947268,  0.99925500, &
        0.99895978,  0.99853981,  0.99794048,  0.99711221,  0.99601156, &
        0.99457222,  0.99267805,  0.99017203,  0.98685879,  0.98248965, &
        0.97675008,  0.96924561,  0.95948225,  0.94682872,  0.93048364, &
        0.90948170,  0.88277107,  0.84936363,  0.80862159,  0.76067388, &
        0.70681286,  0.64947200,  0.59142905,  0.53437781,  0.47803569/
      data ((c2(ip,iw),iw=1,30), ip= 7, 7)/ &
       -0.2205E-06, -0.5259E-06, -0.1106E-05, -0.2055E-05, -0.3488E-05, &
       -0.5580E-05, -0.8550E-05, -0.1257E-04, -0.1770E-04, -0.2415E-04, &
       -0.3252E-04, -0.4380E-04, -0.5918E-04, -0.7993E-04, -0.1073E-03, &
       -0.1425E-03, -0.1867E-03, -0.2416E-03, -0.3086E-03, -0.3900E-03, &
       -0.4882E-03, -0.6053E-03, -0.7420E-03, -0.8966E-03, -0.1066E-02, &
       -0.1246E-02, -0.1433E-02, -0.1623E-02, -0.1804E-02, -0.1957E-02/
      data ((c3(ip,iw),iw=1,30), ip= 7, 7)/ &
        0.5694E-09,  0.8009E-09,  0.6253E-09, -0.6945E-09, -0.3438E-08, &
       -0.7900E-08, -0.1377E-07, -0.2089E-07, -0.3073E-07, -0.4686E-07, &
       -0.7275E-07, -0.1077E-06, -0.1508E-06, -0.2043E-06, -0.2733E-06, &
       -0.3621E-06, -0.4728E-06, -0.6046E-06, -0.7569E-06, -0.9316E-06, &
       -0.1130E-05, -0.1343E-05, -0.1542E-05, -0.1689E-05, -0.1733E-05, &
       -0.1611E-05, -0.1264E-05, -0.6785E-06,  0.8145E-07,  0.9671E-06/
      data ((c1(ip,iw),iw=1,30), ip= 8, 8)/ &
        0.99985993,  0.99976683,  0.99963683,  0.99946576,  0.99924189, &
        0.99893636,  0.99849796,  0.99786597,  0.99698311,  0.99579459, &
        0.99422216,  0.99213505,  0.98935312,  0.98565400,  0.98074925, &
        0.97427416,  0.96576148,  0.95460701,  0.94004321,  0.92113876, &
        0.89685524,  0.86616063,  0.82824260,  0.78287470,  0.73088396, &
        0.67437506,  0.61619699,  0.55862093,  0.50198877,  0.44554055/
      data ((c2(ip,iw),iw=1,30), ip= 8, 8)/ &
       -0.2147E-06, -0.5157E-06, -0.1086E-05, -0.2021E-05, -0.3437E-05, &
       -0.5502E-05, -0.8439E-05, -0.1241E-04, -0.1749E-04, -0.2392E-04, &
       -0.3235E-04, -0.4387E-04, -0.5973E-04, -0.8130E-04, -0.1099E-03, &
       -0.1469E-03, -0.1937E-03, -0.2519E-03, -0.3233E-03, -0.4104E-03, &
       -0.5157E-03, -0.6410E-03, -0.7861E-03, -0.9490E-03, -0.1126E-02, &
       -0.1314E-02, -0.1507E-02, -0.1697E-02, -0.1866E-02, -0.2000E-02/
      data ((c3(ip,iw),iw=1,30), ip= 8, 8)/ &
        0.5508E-09,  0.7743E-09,  0.5295E-09, -0.8196E-09, -0.3579E-08, &
       -0.8113E-08, -0.1405E-07, -0.2119E-07, -0.3082E-07, -0.4624E-07, &
       -0.7128E-07, -0.1055E-06, -0.1478E-06, -0.2008E-06, -0.2694E-06, &
       -0.3585E-06, -0.4696E-06, -0.6006E-06, -0.7496E-06, -0.9178E-06, &
       -0.1105E-05, -0.1298E-05, -0.1466E-05, -0.1561E-05, -0.1520E-05, &
       -0.1274E-05, -0.7879E-06, -0.1038E-06,  0.7074E-06,  0.1625E-05/
      data ((c1(ip,iw),iw=1,30), ip= 9, 9)/ &
        0.99985909,  0.99976450,  0.99963140,  0.99945474,  0.99922204, &
        0.99890035,  0.99843478,  0.99775594,  0.99679625,  0.99548972, &
        0.99374318,  0.99140668,  0.98827362,  0.98408484,  0.97850770, &
        0.97111320,  0.96133631,  0.94844013,  0.93151331,  0.90952784, &
        0.88142335,  0.84628373,  0.80362570,  0.75383699,  0.69858503, &
        0.64060891,  0.58259541,  0.52561414,  0.46898127,  0.41247040/
      data ((c2(ip,iw),iw=1,30), ip= 9, 9)/ &
       -0.2065E-06, -0.4976E-06, -0.1056E-05, -0.1973E-05, -0.3362E-05, &
       -0.5395E-05, -0.8291E-05, -0.1222E-04, -0.1729E-04, -0.2377E-04, &
       -0.3240E-04, -0.4432E-04, -0.6091E-04, -0.8361E-04, -0.1139E-03, &
       -0.1532E-03, -0.2033E-03, -0.2657E-03, -0.3429E-03, -0.4371E-03, &
       -0.5510E-03, -0.6856E-03, -0.8402E-03, -0.1012E-02, -0.1197E-02, &
       -0.1392E-02, -0.1589E-02, -0.1771E-02, -0.1923E-02, -0.2034E-02/
      data ((c3(ip,iw),iw=1,30), ip= 9, 9)/ &
        0.5056E-09,  0.7211E-09,  0.4524E-09, -0.9286E-09, -0.3781E-08, &
       -0.8307E-08, -0.1430E-07, -0.2133E-07, -0.3047E-07, -0.4517E-07, &
       -0.6924E-07, -0.1028E-06, -0.1446E-06, -0.1969E-06, -0.2658E-06, &
       -0.3558E-06, -0.4676E-06, -0.5973E-06, -0.7420E-06, -0.9028E-06, &
       -0.1077E-05, -0.1246E-05, -0.1374E-05, -0.1399E-05, -0.1248E-05, &
       -0.8641E-06, -0.2613E-06,  0.4860E-06,  0.1340E-05,  0.2271E-05/
      data ((c1(ip,iw),iw=1,30), ip=10,10)/ &
        0.99985766,  0.99976081,  0.99962300,  0.99943829,  0.99919146, &
        0.99884665,  0.99834174,  0.99759775,  0.99653465,  0.99507225, &
        0.99310064,  0.99044478,  0.98686624,  0.98206371,  0.97564846, &
        0.96710497,  0.95574677,  0.94068575,  0.92088860,  0.89527357, &
        0.86284405,  0.82293105,  0.77557182,  0.72196519,  0.66457325, &
        0.60630083,  0.54892772,  0.49214739,  0.43535739,  0.37917084/
      data ((c2(ip,iw),iw=1,30), ip=10,10)/ &
       -0.1972E-06, -0.4736E-06, -0.1010E-05, -0.1900E-05, -0.3258E-05, &
       -0.5254E-05, -0.8114E-05, -0.1203E-04, -0.1714E-04, -0.2380E-04, &
       -0.3277E-04, -0.4534E-04, -0.6295E-04, -0.8719E-04, -0.1197E-03, &
       -0.1621E-03, -0.2162E-03, -0.2841E-03, -0.3684E-03, -0.4714E-03, &
       -0.5953E-03, -0.7402E-03, -0.9049E-03, -0.1086E-02, -0.1280E-02, &
       -0.1480E-02, -0.1673E-02, -0.1840E-02, -0.1971E-02, -0.2058E-02/
      data ((c3(ip,iw),iw=1,30), ip=10,10)/ &
        0.4923E-09,  0.6280E-09,  0.3646E-09, -0.1083E-08, -0.3954E-08, &
       -0.8542E-08, -0.1454E-07, -0.2138E-07, -0.2983E-07, -0.4350E-07, &
       -0.6668E-07, -0.9944E-07, -0.1408E-06, -0.1934E-06, -0.2634E-06, &
       -0.3549E-06, -0.4672E-06, -0.5948E-06, -0.7347E-06, -0.8865E-06, &
       -0.1044E-05, -0.1183E-05, -0.1255E-05, -0.1186E-05, -0.9045E-06, &
       -0.3919E-06,  0.2906E-06,  0.1087E-05,  0.1969E-05,  0.2883E-05/
      data ((c1(ip,iw),iw=1,30), ip=11,11)/ &
        0.99985558,  0.99975532,  0.99961084,  0.99941343,  0.99914658, &
        0.99876797,  0.99820811,  0.99737567,  0.99617624,  0.99451292, &
        0.99225265,  0.98919237,  0.98505485,  0.97948909,  0.97203231, &
        0.96205616,  0.94872671,  0.93101531,  0.90780544,  0.87802786, &
        0.84086448,  0.79606968,  0.74441928,  0.68799782,  0.62972331, &
        0.57195759,  0.51503146,  0.45810747,  0.40138185,  0.34614110/
      data ((c2(ip,iw),iw=1,30), ip=11,11)/ &
       -0.1814E-06, -0.4396E-06, -0.9485E-06, -0.1804E-05, -0.3121E-05, &
       -0.5079E-05, -0.7915E-05, -0.1186E-04, -0.1714E-04, -0.2411E-04, &
       -0.3366E-04, -0.4713E-04, -0.6612E-04, -0.9237E-04, -0.1277E-03, &
       -0.1739E-03, -0.2333E-03, -0.3082E-03, -0.4012E-03, -0.5146E-03, &
       -0.6495E-03, -0.8057E-03, -0.9809E-03, -0.1172E-02, -0.1373E-02, &
       -0.1573E-02, -0.1754E-02, -0.1902E-02, -0.2009E-02, -0.2069E-02/
      data ((c3(ip,iw),iw=1,30), ip=11,11)/ &
        0.4231E-09,  0.5375E-09,  0.1783E-09, -0.1216E-08, -0.4183E-08, &
       -0.8738E-08, -0.1465E-07, -0.2105E-07, -0.2864E-07, -0.4118E-07, &
       -0.6333E-07, -0.9556E-07, -0.1369E-06, -0.1906E-06, -0.2629E-06, &
       -0.3563E-06, -0.4683E-06, -0.5933E-06, -0.7281E-06, -0.8688E-06, &
       -0.1004E-05, -0.1101E-05, -0.1095E-05, -0.9073E-06, -0.4912E-06, &
        0.1190E-06,  0.8558E-06,  0.1691E-05,  0.2577E-05,  0.3435E-05/
      data ((c1(ip,iw),iw=1,30), ip=12,12)/ &
        0.99985254,  0.99974763,  0.99959290,  0.99937779,  0.99908137, &
        0.99865609,  0.99802142,  0.99707383,  0.99570006,  0.99378008, &
        0.99115396,  0.98758602,  0.98275584,  0.97624695,  0.96749693, &
        0.95573723,  0.93998092,  0.91909111,  0.89192092,  0.85751379, &
        0.81538570,  0.76593137,  0.71082634,  0.65285242,  0.59476137, &
        0.53763282,  0.48068869,  0.42364532,  0.36750680,  0.31402236/
      data ((c2(ip,iw),iw=1,30), ip=12,12)/ &
       -0.1608E-06, -0.3958E-06, -0.8650E-06, -0.1675E-05, -0.2950E-05, &
       -0.4877E-05, -0.7726E-05, -0.1179E-04, -0.1736E-04, -0.2485E-04, &
       -0.3523E-04, -0.4994E-04, -0.7074E-04, -0.9954E-04, -0.1384E-03, &
       -0.1896E-03, -0.2555E-03, -0.3390E-03, -0.4424E-03, -0.5675E-03, &
       -0.7146E-03, -0.8823E-03, -0.1068E-02, -0.1267E-02, -0.1472E-02, &
       -0.1664E-02, -0.1827E-02, -0.1953E-02, -0.2034E-02, -0.2066E-02/
      data ((c3(ip,iw),iw=1,30), ip=12,12)/ &
        0.3699E-09,  0.4098E-09,  0.8249E-10, -0.1368E-08, -0.4281E-08, &
       -0.8754E-08, -0.1438E-07, -0.2042E-07, -0.2719E-07, -0.3863E-07, &
       -0.5964E-07, -0.9135E-07, -0.1338E-06, -0.1899E-06, -0.2651E-06, &
       -0.3602E-06, -0.4714E-06, -0.5938E-06, -0.7223E-06, -0.8487E-06, &
       -0.9536E-06, -0.9879E-06, -0.8765E-06, -0.5557E-06, -0.2634E-07, &
        0.6473E-06,  0.1431E-05,  0.2287E-05,  0.3143E-05,  0.3888E-05/
      data ((c1(ip,iw),iw=1,30), ip=13,13)/ &
        0.99984854,  0.99973702,  0.99956822,  0.99932772,  0.99899036, &
        0.99850190,  0.99777073,  0.99667609,  0.99508202,  0.99283862, &
        0.98975527,  0.98555952,  0.97987813,  0.97220761,  0.96185511, &
        0.94789296,  0.92920458,  0.90459120,  0.87295771,  0.83359337, &
        0.78654921,  0.73307240,  0.67570245,  0.61741775,  0.56001049, &
        0.50309032,  0.44594115,  0.38916695,  0.33435279,  0.28346843/
      data ((c2(ip,iw),iw=1,30), ip=13,13)/ &
       -0.1378E-06, -0.3411E-06, -0.7620E-06, -0.1519E-05, -0.2750E-05, &
       -0.4663E-05, -0.7578E-05, -0.1188E-04, -0.1790E-04, -0.2617E-04, &
       -0.3769E-04, -0.5404E-04, -0.7712E-04, -0.1091E-03, -0.1524E-03, &
       -0.2097E-03, -0.2838E-03, -0.3775E-03, -0.4929E-03, -0.6308E-03, &
       -0.7906E-03, -0.9700E-03, -0.1166E-02, -0.1371E-02, -0.1571E-02, &
       -0.1748E-02, -0.1891E-02, -0.1992E-02, -0.2045E-02, -0.2052E-02/
      data ((c3(ip,iw),iw=1,30), ip=13,13)/ &
        0.3220E-09,  0.2900E-09, -0.5853E-10, -0.1453E-08, -0.4188E-08, &
       -0.8544E-08, -0.1401E-07, -0.1963E-07, -0.2570E-07, -0.3583E-07, &
       -0.5549E-07, -0.8717E-07, -0.1318E-06, -0.1917E-06, -0.2706E-06, &
       -0.3669E-06, -0.4771E-06, -0.5966E-06, -0.7171E-06, -0.8230E-06, &
       -0.8803E-06, -0.8243E-06, -0.5856E-06, -0.1399E-06,  0.4663E-06, &
        0.1190E-05,  0.2007E-05,  0.2854E-05,  0.3632E-05,  0.4201E-05/
      data ((c1(ip,iw),iw=1,30), ip=14,14)/ &
        0.99984354,  0.99972343,  0.99953604,  0.99926126,  0.99886900, &
        0.99829781,  0.99744409,  0.99616748,  0.99430037,  0.99165708, &
        0.98801166,  0.98305029,  0.97632855,  0.96722776,  0.95489699, &
        0.93826014,  0.91611183,  0.88725436,  0.85075951,  0.80634087, &
        0.75481313,  0.69836313,  0.64004612,  0.58229285,  0.52536106, &
        0.46826547,  0.41112047,  0.35525620,  0.30263388,  0.25491780/
      data ((c2(ip,iw),iw=1,30), ip=14,14)/ &
       -0.1107E-06, -0.2798E-06, -0.6492E-06, -0.1345E-05, -0.2534E-05, &
       -0.4458E-05, -0.7507E-05, -0.1215E-04, -0.1886E-04, -0.2821E-04, &
       -0.4127E-04, -0.5969E-04, -0.8556E-04, -0.1214E-03, -0.1702E-03, &
       -0.2351E-03, -0.3190E-03, -0.4246E-03, -0.5533E-03, -0.7047E-03, &
       -0.8773E-03, -0.1068E-02, -0.1271E-02, -0.1476E-02, -0.1665E-02, &
       -0.1823E-02, -0.1942E-02, -0.2016E-02, -0.2042E-02, -0.2030E-02/
      data ((c3(ip,iw),iw=1,30), ip=14,14)/ &
        0.2581E-09,  0.2555E-09, -0.9846E-10, -0.1307E-08, -0.3946E-08, &
       -0.8081E-08, -0.1346E-07, -0.1903E-07, -0.2451E-07, -0.3310E-07, &
       -0.5113E-07, -0.8340E-07, -0.1314E-06, -0.1963E-06, -0.2789E-06, &
       -0.3767E-06, -0.4866E-06, -0.6024E-06, -0.7109E-06, -0.7833E-06, &
       -0.7671E-06, -0.5930E-06, -0.2211E-06,  0.3183E-06,  0.9753E-06, &
        0.1739E-05,  0.2564E-05,  0.3364E-05,  0.3998E-05,  0.4378E-05/
      data ((c1(ip,iw),iw=1,30), ip=15,15)/ &
        0.99983788,  0.99970752,  0.99949718,  0.99917865,  0.99871457, &
        0.99803782,  0.99703240,  0.99553382,  0.99333793,  0.99021345, &
        0.98589009,  0.98000097,  0.97200888,  0.96114874,  0.94640887, &
        0.92659646,  0.90046513,  0.86691999,  0.82535470,  0.77612388, &
        0.72097731,  0.66286379,  0.60470819,  0.54761422,  0.49065191, &
        0.43337888,  0.37676656,  0.32264513,  0.27297002,  0.22841394/
      data ((c2(ip,iw),iw=1,30), ip=15,15)/ &
       -0.8598E-07, -0.2205E-06, -0.5318E-06, -0.1163E-05, -0.2310E-05, &
       -0.4269E-05, -0.7519E-05, -0.1265E-04, -0.2028E-04, -0.3108E-04, &
       -0.4612E-04, -0.6709E-04, -0.9635E-04, -0.1369E-03, -0.1924E-03, &
       -0.2662E-03, -0.3615E-03, -0.4804E-03, -0.6232E-03, -0.7887E-03, &
       -0.9739E-03, -0.1174E-02, -0.1381E-02, -0.1578E-02, -0.1749E-02, &
       -0.1886E-02, -0.1979E-02, -0.2025E-02, -0.2028E-02, -0.2005E-02/
      data ((c3(ip,iw),iw=1,30), ip=15,15)/ &
        0.2475E-09,  0.2501E-09, -0.2925E-10, -0.1078E-08, -0.3398E-08, &
       -0.7395E-08, -0.1271E-07, -0.1860E-07, -0.2411E-07, -0.3137E-07, &
       -0.4761E-07, -0.8047E-07, -0.1325E-06, -0.2027E-06, -0.2896E-06, &
       -0.3902E-06, -0.4998E-06, -0.6094E-06, -0.6971E-06, -0.7150E-06, &
       -0.5950E-06, -0.2854E-06,  0.2000E-06,  0.7992E-06,  0.1499E-05, &
        0.2282E-05,  0.3081E-05,  0.3770E-05,  0.4211E-05,  0.4499E-05/
      data ((c1(ip,iw),iw=1,30), ip=16,16)/ &
        0.99983191,  0.99969077,  0.99945426,  0.99908280,  0.99852955, &
        0.99772078,  0.99653023,  0.99476653,  0.99218494,  0.98849833, &
        0.98337215,  0.97636360,  0.96681708,  0.95381010,  0.93619323, &
        0.91269946,  0.88211763,  0.84358740,  0.79702663,  0.74364722, &
        0.68610674,  0.62760854,  0.57012022,  0.51321244,  0.45598596, &
        0.39891213,  0.34358257,  0.29206699,  0.24564439,  0.20362538/
      data ((c2(ip,iw),iw=1,30), ip=16,16)/ &
       -0.6855E-07, -0.1752E-06, -0.4312E-06, -0.9879E-06, -0.2093E-05, &
       -0.4104E-05, -0.7589E-05, -0.1331E-04, -0.2210E-04, -0.3475E-04, &
       -0.5227E-04, -0.7637E-04, -0.1097E-03, -0.1558E-03, -0.2190E-03, &
       -0.3032E-03, -0.4112E-03, -0.5443E-03, -0.7019E-03, -0.8814E-03, &
       -0.1079E-02, -0.1286E-02, -0.1489E-02, -0.1671E-02, -0.1822E-02, &
       -0.1934E-02, -0.2000E-02, -0.2020E-02, -0.2008E-02, -0.1984E-02/
      data ((c3(ip,iw),iw=1,30), ip=16,16)/ &
        0.2608E-09,  0.3033E-09,  0.1543E-09, -0.5960E-09, -0.2626E-08, &
       -0.6352E-08, -0.1194E-07, -0.1852E-07, -0.2487E-07, -0.3198E-07, &
       -0.4661E-07, -0.7875E-07, -0.1333E-06, -0.2085E-06, -0.3010E-06, &
       -0.4064E-06, -0.5157E-06, -0.6137E-06, -0.6644E-06, -0.5995E-06, &
       -0.3510E-06,  0.9010E-07,  0.6553E-06,  0.1302E-05,  0.2029E-05, &
        0.2800E-05,  0.3518E-05,  0.4027E-05,  0.4307E-05,  0.4711E-05/
      data ((c1(ip,iw),iw=1,30), ip=17,17)/ &
        0.99982625,  0.99967456,  0.99941117,  0.99898118,  0.99832189, &
        0.99735099,  0.99593443,  0.99386060,  0.99083877,  0.98651254, &
        0.98045152,  0.97209597,  0.96065331,  0.94506192,  0.92408115, &
        0.89642918,  0.86103737,  0.81745273,  0.76635075,  0.70990741, &
        0.65133941,  0.59332442,  0.53625399,  0.47907001,  0.42173553, &
        0.36549109,  0.31233364,  0.26400638,  0.22048974,  0.18009293/
      data ((c2(ip,iw),iw=1,30), ip=17,17)/ &
       -0.5514E-07, -0.1392E-06, -0.3493E-06, -0.8397E-06, -0.1893E-05, &
       -0.3946E-05, -0.7684E-05, -0.1406E-04, -0.2417E-04, -0.3898E-04, &
       -0.5950E-04, -0.8744E-04, -0.1257E-03, -0.1784E-03, -0.2502E-03, &
       -0.3456E-03, -0.4672E-03, -0.6152E-03, -0.7879E-03, -0.9812E-03, &
       -0.1189E-02, -0.1397E-02, -0.1589E-02, -0.1753E-02, -0.1880E-02, &
       -0.1965E-02, -0.2003E-02, -0.2005E-02, -0.1989E-02, -0.1964E-02/
      data ((c3(ip,iw),iw=1,30), ip=17,17)/ &
        0.3246E-09,  0.4151E-09,  0.4204E-09,  0.4528E-10, -0.1535E-08, &
       -0.4880E-08, -0.1063E-07, -0.1849E-07, -0.2720E-07, -0.3640E-07, &
       -0.5021E-07, -0.7908E-07, -0.1321E-06, -0.2101E-06, -0.3095E-06, &
       -0.4215E-06, -0.5301E-06, -0.6060E-06, -0.5943E-06, -0.4190E-06, &
       -0.3586E-07,  0.5081E-06,  0.1133E-05,  0.1821E-05,  0.2551E-05, &
        0.3263E-05,  0.3823E-05,  0.4126E-05,  0.4400E-05,  0.5155E-05/
      data ((c1(ip,iw),iw=1,30), ip=18,18)/ &
        0.99982196,  0.99966067,  0.99937201,  0.99888307,  0.99810678, &
        0.99694288,  0.99525315,  0.99281454,  0.98929566,  0.98425364, &
        0.97712040,  0.96716130,  0.95343208,  0.93478447,  0.90994793, &
        0.87772256,  0.83733451,  0.78894842,  0.73419034,  0.67604613, &
        0.61762655,  0.56022692,  0.50297821,  0.44541037,  0.38843226, &
        0.33384353,  0.28363961,  0.23849565,  0.19704032,  0.15752959/
      data ((c2(ip,iw),iw=1,30), ip=18,18)/ &
       -0.4665E-07, -0.1168E-06, -0.2973E-06, -0.7239E-06, -0.1721E-05, &
       -0.3791E-05, -0.7760E-05, -0.1477E-04, -0.2619E-04, -0.4332E-04, &
       -0.6735E-04, -0.1000E-03, -0.1443E-03, -0.2045E-03, -0.2859E-03, &
       -0.3930E-03, -0.5285E-03, -0.6917E-03, -0.8794E-03, -0.1086E-02, &
       -0.1299E-02, -0.1502E-02, -0.1678E-02, -0.1820E-02, -0.1922E-02, &
       -0.1978E-02, -0.1994E-02, -0.1987E-02, -0.1973E-02, -0.1944E-02/
      data ((c3(ip,iw),iw=1,30), ip=18,18)/ &
        0.3113E-09,  0.5322E-09,  0.7264E-09,  0.6706E-09, -0.2555E-09, &
       -0.2895E-08, -0.8462E-08, -0.1768E-07, -0.3017E-07, -0.4437E-07, &
       -0.6017E-07, -0.8378E-07, -0.1282E-06, -0.2039E-06, -0.3086E-06, &
       -0.4277E-06, -0.5329E-06, -0.5704E-06, -0.4687E-06, -0.1680E-06, &
        0.3269E-06,  0.9415E-06,  0.1629E-05,  0.2348E-05,  0.3046E-05, &
        0.3628E-05,  0.3963E-05,  0.4143E-05,  0.4644E-05,  0.5889E-05/
      data ((c1(ip,iw),iw=1,30), ip=19,19)/ &
        0.99981844,  0.99964958,  0.99933988,  0.99879652,  0.99790138, &
        0.99652147,  0.99450737,  0.99163669,  0.98754823,  0.98170340, &
        0.97335166,  0.96152353,  0.94510251,  0.92290431,  0.89373261, &
        0.85661834,  0.81128657,  0.75875717,  0.70160520,  0.64312267, &
        0.58543533,  0.52814835,  0.47036928,  0.41267300,  0.35673177, &
        0.30465996,  0.25773448,  0.21512479,  0.17481446,  0.13591540/
      data ((c2(ip,iw),iw=1,30), ip=19,19)/ &
       -0.4098E-07, -0.1007E-06, -0.2591E-06, -0.6488E-06, -0.1581E-05, &
       -0.3648E-05, -0.7796E-05, -0.1535E-04, -0.2790E-04, -0.4722E-04, &
       -0.7509E-04, -0.1135E-03, -0.1652E-03, -0.2342E-03, -0.3257E-03, &
       -0.4448E-03, -0.5940E-03, -0.7722E-03, -0.9744E-03, -0.1190E-02, &
       -0.1403E-02, -0.1594E-02, -0.1753E-02, -0.1873E-02, -0.1947E-02, &
       -0.1977E-02, -0.1979E-02, -0.1972E-02, -0.1958E-02, -0.1915E-02/
      data ((c3(ip,iw),iw=1,30), ip=19,19)/ &
        0.3486E-09,  0.5961E-09,  0.9233E-09,  0.1346E-08,  0.1269E-08, &
       -0.2634E-09, -0.5106E-08, -0.1521E-07, -0.3192E-07, -0.5389E-07, &
       -0.7585E-07, -0.9598E-07, -0.1266E-06, -0.1898E-06, -0.2923E-06, &
       -0.4140E-06, -0.5052E-06, -0.4857E-06, -0.2779E-06,  0.1387E-06, &
        0.7058E-06,  0.1381E-05,  0.2127E-05,  0.2859E-05,  0.3475E-05, &
        0.3846E-05,  0.3969E-05,  0.4224E-05,  0.5152E-05,  0.6873E-05/
      data ((c1(ip,iw),iw=1,30), ip=20,20)/ &
        0.99981600,  0.99964136,  0.99931455,  0.99872625,  0.99772161, &
        0.99612063,  0.99373823,  0.99035382,  0.98559308,  0.97882116, &
        0.96908391,  0.95513755,  0.93565118,  0.90942025,  0.87547016, &
        0.83328891,  0.78337395,  0.72776425,  0.66966605,  0.61183786, &
        0.55468166,  0.49698496,  0.43872380,  0.38142091,  0.32735240, &
        0.27838105,  0.23436898,  0.19330215,  0.15356398,  0.11541939/
      data ((c2(ip,iw),iw=1,30), ip=20,20)/ &
       -0.3756E-07, -0.9374E-07, -0.2378E-06, -0.5962E-06, -0.1485E-05, &
       -0.3532E-05, -0.7794E-05, -0.1573E-04, -0.2912E-04, -0.5028E-04, &
       -0.8196E-04, -0.1269E-03, -0.1874E-03, -0.2667E-03, -0.3695E-03, &
       -0.5007E-03, -0.6632E-03, -0.8557E-03, -0.1070E-02, -0.1290E-02, &
       -0.1496E-02, -0.1673E-02, -0.1813E-02, -0.1908E-02, -0.1955E-02, &
       -0.1966E-02, -0.1965E-02, -0.1962E-02, -0.1942E-02, -0.1874E-02/
      data ((c3(ip,iw),iw=1,30), ip=20,20)/ &
        0.3566E-09,  0.6998E-09,  0.1224E-08,  0.1972E-08,  0.2815E-08, &
        0.2709E-08, -0.5642E-09, -0.1074E-07, -0.3102E-07, -0.6097E-07, &
       -0.9344E-07, -0.1176E-06, -0.1362E-06, -0.1755E-06, -0.2590E-06, &
       -0.3654E-06, -0.4210E-06, -0.3288E-06, -0.2318E-07,  0.4684E-06, &
        0.1083E-05,  0.1812E-05,  0.2598E-05,  0.3312E-05,  0.3778E-05, &
        0.3903E-05,  0.3958E-05,  0.4525E-05,  0.5947E-05,  0.8016E-05/
      data ((c1(ip,iw),iw=1,30), ip=21,21)/ &
        0.99981397,  0.99963546,  0.99929672,  0.99867326,  0.99757719, &
        0.99577057,  0.99300259,  0.98902106,  0.98344505,  0.97556579, &
        0.96423388,  0.94793248,  0.92507470,  0.89441156,  0.85533464, &
        0.80810499,  0.75430608,  0.69694877,  0.63920856,  0.58230531, &
        0.52508599,  0.46681160,  0.40848309,  0.35235608,  0.30093586, &
        0.25503820,  0.21298867,  0.17258596,  0.13329828,  0.09623200/
      data ((c2(ip,iw),iw=1,30), ip=21,21)/ &
       -0.3458E-07, -0.8927E-07, -0.2276E-06, -0.5715E-06, -0.1427E-05, &
       -0.3448E-05, -0.7761E-05, -0.1589E-04, -0.2978E-04, -0.5225E-04, &
       -0.8727E-04, -0.1388E-03, -0.2093E-03, -0.3009E-03, -0.4167E-03, &
       -0.5609E-03, -0.7364E-03, -0.9410E-03, -0.1162E-02, -0.1379E-02, &
       -0.1573E-02, -0.1736E-02, -0.1856E-02, -0.1926E-02, -0.1951E-02, &
       -0.1956E-02, -0.1959E-02, -0.1956E-02, -0.1918E-02, -0.1818E-02/
      data ((c3(ip,iw),iw=1,30), ip=21,21)/ &
        0.4204E-09,  0.7264E-09,  0.1349E-08,  0.2456E-08,  0.4164E-08, &
        0.5665E-08,  0.4495E-08, -0.4390E-08, -0.2641E-07, -0.6297E-07, &
       -0.1081E-06, -0.1467E-06, -0.1647E-06, -0.1760E-06, -0.2159E-06, &
       -0.2696E-06, -0.2537E-06, -0.7936E-07,  0.2835E-06,  0.7986E-06, &
        0.1453E-05,  0.2220E-05,  0.3009E-05,  0.3640E-05,  0.3894E-05, &
        0.3856E-05,  0.4088E-05,  0.5125E-05,  0.6974E-05,  0.9239E-05/
      data ((c1(ip,iw),iw=1,30), ip=22,22)/ &
        0.99981296,  0.99963164,  0.99928445,  0.99863535,  0.99746948, &
        0.99548900,  0.99235290,  0.98771960,  0.98116148,  0.97192103, &
        0.95871633,  0.93980151,  0.91333646,  0.87801361,  0.83368826, &
        0.78171986,  0.72501707,  0.66722846,  0.61055648,  0.55416226, &
        0.49651819,  0.43790764,  0.38022524,  0.32622856,  0.27784550, &
        0.23429114,  0.19307172,  0.15282935,  0.11413610,  0.07851070/
      data ((c2(ip,iw),iw=1,30), ip=22,22)/ &
       -0.3607E-07, -0.9106E-07, -0.2201E-06, -0.5559E-06, -0.1391E-05, &
       -0.3381E-05, -0.7671E-05, -0.1583E-04, -0.2984E-04, -0.5305E-04, &
       -0.9054E-04, -0.1478E-03, -0.2287E-03, -0.3343E-03, -0.4658E-03, &
       -0.6255E-03, -0.8143E-03, -0.1027E-02, -0.1246E-02, -0.1452E-02, &
       -0.1633E-02, -0.1780E-02, -0.1879E-02, -0.1928E-02, -0.1943E-02, &
       -0.1952E-02, -0.1962E-02, -0.1949E-02, -0.1883E-02, -0.1749E-02/
      data ((c3(ip,iw),iw=1,30), ip=22,22)/ &
        0.3991E-09,  0.7797E-09,  0.1514E-08,  0.2890E-08,  0.5114E-08, &
        0.8004E-08,  0.9207E-08,  0.2751E-08, -0.1866E-07, -0.5876E-07, &
       -0.1158E-06, -0.1759E-06, -0.2100E-06, -0.2037E-06, -0.1777E-06, &
       -0.1260E-06,  0.1159E-07,  0.2717E-06,  0.6398E-06,  0.1142E-05, &
        0.1811E-05,  0.2581E-05,  0.3312E-05,  0.3761E-05,  0.3802E-05, &
        0.3819E-05,  0.4473E-05,  0.6011E-05,  0.8162E-05,  0.1049E-04/
      data ((c1(ip,iw),iw=1,30), ip=23,23)/ &
        0.99981195,  0.99962944,  0.99927652,  0.99860990,  0.99739289, &
        0.99527556,  0.99181646,  0.98653018,  0.97884804,  0.96793681, &
        0.95248020,  0.93061423,  0.90033394,  0.86034650,  0.81107712, &
        0.75512129,  0.69664025,  0.63929212,  0.58348066,  0.52703655, &
        0.46901560,  0.41065586,  0.35463405,  0.30363917,  0.25805390, &
        0.21567380,  0.17439228,  0.13414437,  0.09621924,  0.06238037/
      data ((c2(ip,iw),iw=1,30), ip=23,23)/ &
       -0.3741E-07, -0.8793E-07, -0.2127E-06, -0.5352E-06, -0.1342E-05, &
       -0.3272E-05, -0.7454E-05, -0.1544E-04, -0.2923E-04, -0.5256E-04, &
       -0.9141E-04, -0.1530E-03, -0.2432E-03, -0.3641E-03, -0.5150E-03, &
       -0.6942E-03, -0.8974E-03, -0.1111E-02, -0.1318E-02, -0.1509E-02, &
       -0.1677E-02, -0.1805E-02, -0.1882E-02, -0.1918E-02, -0.1938E-02, &
       -0.1960E-02, -0.1970E-02, -0.1938E-02, -0.1839E-02, -0.1671E-02/
      data ((c3(ip,iw),iw=1,30), ip=23,23)/ &
        0.4550E-09,  0.7983E-09,  0.1546E-08,  0.3031E-08,  0.5588E-08, &
        0.9353E-08,  0.1256E-07,  0.9064E-08, -0.9430E-08, -0.4907E-07, &
       -0.1139E-06, -0.1947E-06, -0.2559E-06, -0.2493E-06, -0.1525E-06, &
        0.4902E-07,  0.3608E-06,  0.7150E-06,  0.1069E-05,  0.1529E-05, &
        0.2160E-05,  0.2878E-05,  0.3447E-05,  0.3616E-05,  0.3553E-05, &
        0.3905E-05,  0.5129E-05,  0.7106E-05,  0.9457E-05,  0.1172E-04/
      data ((c1(ip,iw),iw=1,30), ip=24,24)/ &
        0.99981159,  0.99962759,  0.99927121,  0.99859214,  0.99733758, &
        0.99511474,  0.99138594,  0.98549122,  0.97662354,  0.96375191, &
        0.94555831,  0.92026752,  0.88592684,  0.84149605,  0.78813756, &
        0.72953093,  0.67042029,  0.61351800,  0.55762243,  0.50072914, &
        0.44270837,  0.38552511,  0.33234173,  0.28479803,  0.24121642, &
        0.19889826,  0.15705287,  0.11675125,  0.07971334,  0.04797637/
      data ((c2(ip,iw),iw=1,30), ip=24,24)/ &
       -0.3696E-07, -0.8674E-07, -0.2028E-06, -0.5040E-06, -0.1260E-05, &
       -0.3065E-05, -0.7012E-05, -0.1462E-04, -0.2786E-04, -0.5064E-04, &
       -0.8982E-04, -0.1541E-03, -0.2520E-03, -0.3885E-03, -0.5620E-03, &
       -0.7660E-03, -0.9842E-03, -0.1192E-02, -0.1381E-02, -0.1554E-02, &
       -0.1702E-02, -0.1807E-02, -0.1865E-02, -0.1902E-02, -0.1940E-02, &
       -0.1976E-02, -0.1980E-02, -0.1921E-02, -0.1788E-02, -0.1587E-02/
      data ((c3(ip,iw),iw=1,30), ip=24,24)/ &
        0.4417E-09,  0.8621E-09,  0.1541E-08,  0.3028E-08,  0.5617E-08, &
        0.9774E-08,  0.1396E-07,  0.1323E-07, -0.7108E-09, -0.3567E-07, &
       -0.1007E-06, -0.1922E-06, -0.2742E-06, -0.2729E-06, -0.1151E-06, &
        0.2385E-06,  0.7388E-06,  0.1217E-05,  0.1603E-05,  0.2001E-05, &
        0.2527E-05,  0.3095E-05,  0.3362E-05,  0.3222E-05,  0.3258E-05, &
        0.4158E-05,  0.5974E-05,  0.8329E-05,  0.1081E-04,  0.1289E-04/
      data ((c1(ip,iw),iw=1,30), ip=25,25)/ &
        0.99981159,  0.99962676,  0.99926704,  0.99857843,  0.99729389, &
        0.99498510,  0.99102783,  0.98458773,  0.97456259,  0.95956063, &
        0.93812776,  0.90879148,  0.87007952,  0.82162309,  0.76556253, &
        0.70621312,  0.64746487,  0.59013212,  0.53297842,  0.47532034, &
        0.41783774,  0.36303061,  0.31369632,  0.26940316,  0.22683442, &
        0.18390143,  0.14129812,  0.10090071,  0.06484979,  0.03544110/
      data ((c2(ip,iw),iw=1,30), ip=25,25)/ &
       -0.3592E-07, -0.8167E-07, -0.1906E-06, -0.4617E-06, -0.1141E-05, &
       -0.2757E-05, -0.6325E-05, -0.1333E-04, -0.2571E-04, -0.4738E-04, &
       -0.8602E-04, -0.1520E-03, -0.2569E-03, -0.4088E-03, -0.6073E-03, &
       -0.8389E-03, -0.1071E-02, -0.1271E-02, -0.1441E-02, -0.1591E-02, &
       -0.1710E-02, -0.1787E-02, -0.1835E-02, -0.1886E-02, -0.1949E-02, &
       -0.1996E-02, -0.1987E-02, -0.1900E-02, -0.1733E-02, -0.1504E-02/
      data ((c3(ip,iw),iw=1,30), ip=25,25)/ &
        0.4018E-09,  0.7557E-09,  0.1466E-08,  0.2932E-08,  0.5434E-08, &
        0.9412E-08,  0.1410E-07,  0.1542E-07,  0.6847E-08, -0.1963E-07, &
       -0.7469E-07, -0.1585E-06, -0.2342E-06, -0.2164E-06, -0.9240E-08, &
        0.4333E-06,  0.1064E-05,  0.1725E-05,  0.2236E-05,  0.2590E-05, &
        0.2947E-05,  0.3218E-05,  0.3056E-05,  0.2698E-05,  0.3035E-05, &
        0.4543E-05,  0.6899E-05,  0.9601E-05,  0.1216E-04,  0.1393E-04/
      data ((c1(ip,iw),iw=1,30), ip=26,26)/ &
        0.99981129,  0.99962574,  0.99926347,  0.99856603,  0.99725467, &
        0.99486899,  0.99070829,  0.98377496,  0.97267365,  0.95554590, &
        0.93054640,  0.89652336,  0.85311186,  0.80127764,  0.74432230, &
        0.68628454,  0.62840164,  0.56955659,  0.51003551,  0.45124298, &
        0.39495039,  0.34372914,  0.29863960,  0.25682771,  0.21449614, &
        0.17074752,  0.12734443,  0.08683419,  0.05188513,  0.02489018/
      data ((c2(ip,iw),iw=1,30), ip=26,26)/ &
       -0.3547E-07, -0.7571E-07, -0.1771E-06, -0.4121E-06, -0.9912E-06, &
       -0.2375E-05, -0.5472E-05, -0.1170E-04, -0.2303E-04, -0.4336E-04, &
       -0.8114E-04, -0.1492E-03, -0.2622E-03, -0.4312E-03, -0.6546E-03, &
       -0.9091E-03, -0.1150E-02, -0.1348E-02, -0.1505E-02, -0.1625E-02, &
       -0.1706E-02, -0.1752E-02, -0.1798E-02, -0.1874E-02, -0.1962E-02, &
       -0.2014E-02, -0.1990E-02, -0.1874E-02, -0.1677E-02, -0.1423E-02/
      data ((c3(ip,iw),iw=1,30), ip=26,26)/ &
        0.3619E-09,  0.7397E-09,  0.1448E-08,  0.2834E-08,  0.5234E-08, &
        0.8951E-08,  0.1360E-07,  0.1672E-07,  0.1367E-07, -0.6730E-09, &
       -0.3422E-07, -0.8585E-07, -0.1175E-06, -0.4731E-07,  0.1899E-06, &
        0.6128E-06,  0.1279E-05,  0.2158E-05,  0.2910E-05,  0.3301E-05, &
        0.3427E-05,  0.3227E-05,  0.2609E-05,  0.2215E-05,  0.2953E-05, &
        0.4987E-05,  0.7816E-05,  0.1084E-04,  0.1341E-04,  0.1477E-04/
      data ((o1(ip,iw),iw=1,21), ip= 1, 1)/ &
        0.99999380,  0.99998742,  0.99997437,  0.99994880,  0.99989700, &
        0.99979585,  0.99959809,  0.99921876,  0.99851090,  0.99724305, &
        0.99510759,  0.99179822,  0.98712689,  0.98100328,  0.97317654, &
        0.96302688,  0.94956261,  0.93159300,  0.90799314,  0.87798125, &
        0.84127617/
      data ((o2(ip,iw),iw=1,21), ip= 1, 1)/ &
        0.2533E-08,  0.4172E-08,  0.4619E-08,  0.1937E-08, -0.5367E-08, &
       -0.2668E-07, -0.9717E-07, -0.3201E-06, -0.9879E-06, -0.2866E-05, &
       -0.7657E-05, -0.1834E-04, -0.3846E-04, -0.7061E-04, -0.1170E-03, &
       -0.1816E-03, -0.2691E-03, -0.3826E-03, -0.5210E-03, -0.6817E-03, &
       -0.8626E-03/
      data ((o3(ip,iw),iw=1,21), ip= 1, 1)/ &
        0.7983E-11,  0.1064E-10,  0.3991E-10,  0.2395E-10,  0.1543E-09, &
        0.3273E-09,  0.7557E-09,  0.1943E-08,  0.4760E-08,  0.1135E-07, &
        0.2413E-07,  0.4171E-07,  0.5172E-07,  0.3416E-07, -0.2136E-07, &
       -0.1153E-06, -0.2399E-06, -0.3714E-06, -0.4697E-06, -0.5060E-06, &
       -0.4864E-06/
      data ((o1(ip,iw),iw=1,21), ip= 2, 2)/ &
        0.99999380,  0.99998742,  0.99997443,  0.99994868,  0.99989706, &
        0.99979585,  0.99959815,  0.99921870,  0.99851090,  0.99724287, &
        0.99510676,  0.99179584,  0.98712158,  0.98099238,  0.97315592, &
        0.96298945,  0.94949555,  0.93147409,  0.90778208,  0.87761009, &
        0.84063214/
      data ((o2(ip,iw),iw=1,21), ip= 2, 2)/ &
        0.2533E-08,  0.4172E-08,  0.4321E-08,  0.1340E-08, -0.5665E-08, &
       -0.2698E-07, -0.9672E-07, -0.3200E-06, -0.9882E-06, -0.2862E-05, &
       -0.7650E-05, -0.1833E-04, -0.3843E-04, -0.7056E-04, -0.1169E-03, &
       -0.1814E-03, -0.2689E-03, -0.3821E-03, -0.5202E-03, -0.6801E-03, &
       -0.8597E-03/
      data ((o3(ip,iw),iw=1,21), ip= 2, 2)/ &
        0.7983E-11,  0.1064E-10,  0.3459E-10,  0.5056E-10,  0.1490E-09, &
        0.3220E-09,  0.7584E-09,  0.1945E-08,  0.4734E-08,  0.1132E-07, &
        0.2410E-07,  0.4165E-07,  0.5164E-07,  0.3403E-07, -0.2161E-07, &
       -0.1158E-06, -0.2407E-06, -0.3730E-06, -0.4726E-06, -0.5111E-06, &
       -0.4952E-06/
      data ((o1(ip,iw),iw=1,21), ip= 3, 3)/ &
        0.99999380,  0.99998742,  0.99997437,  0.99994874,  0.99989700, &
        0.99979597,  0.99959815,  0.99921876,  0.99851084,  0.99724233, &
        0.99510533,  0.99179244,  0.98711336,  0.98097545,  0.97312361, &
        0.96293050,  0.94939035,  0.93128616,  0.90745074,  0.87702966, &
        0.83963144/
      data ((o2(ip,iw),iw=1,21), ip= 3, 3)/ &
        0.2831E-08,  0.3874E-08,  0.4619E-08,  0.1340E-08, -0.6411E-08, &
       -0.2653E-07, -0.9613E-07, -0.3201E-06, -0.9867E-06, -0.2857E-05, &
       -0.7640E-05, -0.1830E-04, -0.3839E-04, -0.7047E-04, -0.1168E-03, &
       -0.1812E-03, -0.2685E-03, -0.3813E-03, -0.5188E-03, -0.6777E-03, &
       -0.8554E-03/
      data ((o3(ip,iw),iw=1,21), ip= 3, 3)/ &
        0.1330E-10,  0.1597E-10,  0.3991E-10,  0.3459E-10,  0.1730E-09, &
        0.2980E-09,  0.7424E-09,  0.1943E-08,  0.4718E-08,  0.1132E-07, &
        0.2408E-07,  0.4155E-07,  0.5148E-07,  0.3375E-07, -0.2203E-07, &
       -0.1166E-06, -0.2421E-06, -0.3755E-06, -0.4770E-06, -0.5184E-06, &
       -0.5068E-06/
      data ((o1(ip,iw),iw=1,21), ip= 4, 4)/ &
        0.99999380,  0.99998742,  0.99997443,  0.99994880,  0.99989706, &
        0.99979591,  0.99959821,  0.99921870,  0.99851060,  0.99724168, &
        0.99510312,  0.99178684,  0.98710048,  0.98094857,  0.97307235, &
        0.96283722,  0.94922370,  0.93099111,  0.90693128,  0.87612689, &
        0.83809173/
      data ((o2(ip,iw),iw=1,21), ip= 4, 4)/ &
        0.2831E-08,  0.3874E-08,  0.4768E-08,  0.1638E-08, -0.6411E-08, &
       -0.2668E-07, -0.9598E-07, -0.3195E-06, -0.9837E-06, -0.2850E-05, &
       -0.7622E-05, -0.1826E-04, -0.3831E-04, -0.7034E-04, -0.1166E-03, &
       -0.1808E-03, -0.2678E-03, -0.3802E-03, -0.5168E-03, -0.6741E-03, &
       -0.8492E-03/
      data ((o3(ip,iw),iw=1,21), ip= 4, 4)/ &
        0.1330E-10,  0.1597E-10,  0.3725E-10,  0.5056E-10,  0.1623E-09, &
        0.3113E-09,  0.7451E-09,  0.1937E-08,  0.4723E-08,  0.1127E-07, &
        0.2401E-07,  0.4138E-07,  0.5121E-07,  0.3341E-07, -0.2266E-07, &
       -0.1177E-06, -0.2443E-06, -0.3792E-06, -0.4832E-06, -0.5283E-06, &
       -0.5212E-06/
      data ((o1(ip,iw),iw=1,21), ip= 5, 5)/ &
        0.99999380,  0.99998742,  0.99997443,  0.99994880,  0.99989706, &
        0.99979603,  0.99959826,  0.99921864,  0.99851030,  0.99724048, &
        0.99509972,  0.99177790,  0.98707968,  0.98090613,  0.97299159, &
        0.96269035,  0.94896168,  0.93052864,  0.90612280,  0.87473416, &
        0.83575100/
      data ((o2(ip,iw),iw=1,21), ip= 5, 5)/ &
        0.2831E-08,  0.3874E-08,  0.4917E-08,  0.1638E-08, -0.6411E-08, &
       -0.2713E-07, -0.9553E-07, -0.3173E-06, -0.9794E-06, -0.2840E-05, &
       -0.7593E-05, -0.1820E-04, -0.3819E-04, -0.7013E-04, -0.1162E-03, &
       -0.1802E-03, -0.2668E-03, -0.3784E-03, -0.5137E-03, -0.6689E-03, &
       -0.8407E-03/
      data ((o3(ip,iw),iw=1,21), ip= 5, 5)/ &
        0.1330E-10,  0.1597E-10,  0.2395E-10,  0.5056E-10,  0.1623E-09, &
        0.2980E-09,  0.7318E-09,  0.1919E-08,  0.4694E-08,  0.1122E-07, &
        0.2389E-07,  0.4114E-07,  0.5087E-07,  0.3278E-07, -0.2370E-07, &
       -0.1195E-06, -0.2475E-06, -0.3847E-06, -0.4917E-06, -0.5404E-06, &
       -0.5361E-06/
      data ((o1(ip,iw),iw=1,21), ip= 6, 6)/ &
        0.99999386,  0.99998742,  0.99997443,  0.99994880,  0.99989718, &
        0.99979603,  0.99959826,  0.99921852,  0.99850971,  0.99723876, &
        0.99509430,  0.99176359,  0.98704708,  0.98083913,  0.97286427, &
        0.96245909,  0.94855142,  0.92980820,  0.90487397,  0.87261194, &
        0.83225304/
      data ((o2(ip,iw),iw=1,21), ip= 6, 6)/ &
        0.2831E-08,  0.2682E-08,  0.4917E-08,  0.1638E-08, -0.6261E-08, &
       -0.2713E-07, -0.9523E-07, -0.3150E-06, -0.9742E-06, -0.2823E-05, &
       -0.7547E-05, -0.1810E-04, -0.3800E-04, -0.6981E-04, -0.1157E-03, &
       -0.1793E-03, -0.2652E-03, -0.3758E-03, -0.5094E-03, -0.6618E-03, &
       -0.8296E-03/
      data ((o3(ip,iw),iw=1,21), ip= 6, 6)/ &
        0.7983E-11,  0.2661E-10,  0.2395E-10,  0.5056E-10,  0.1490E-09, &
        0.2927E-09,  0.7318E-09,  0.1911E-08,  0.4670E-08,  0.1109E-07, &
        0.2369E-07,  0.4079E-07,  0.5027E-07,  0.3187E-07, -0.2516E-07, &
       -0.1220E-06, -0.2521E-06, -0.3920E-06, -0.5023E-06, -0.5532E-06, &
       -0.5476E-06/
      data ((o1(ip,iw),iw=1,21), ip= 7, 7)/ &
        0.99999386,  0.99998742,  0.99997449,  0.99994880,  0.99989712, &
        0.99979609,  0.99959826,  0.99921834,  0.99850875,  0.99723560, &
        0.99508584,  0.99174130,  0.98699600,  0.98073393,  0.97266501, &
        0.96209770,  0.94791204,  0.92869633,  0.90296900,  0.86943132, &
        0.82713652/
      data ((o2(ip,iw),iw=1,21), ip= 7, 7)/ &
        0.2831E-08,  0.2682E-08,  0.5215E-08,  0.1340E-08, -0.4921E-08, &
       -0.2683E-07, -0.9538E-07, -0.3136E-06, -0.9640E-06, -0.2796E-05, &
       -0.7478E-05, -0.1794E-04, -0.3770E-04, -0.6931E-04, -0.1148E-03, &
       -0.1779E-03, -0.2630E-03, -0.3721E-03, -0.5034E-03, -0.6525E-03, &
       -0.8161E-03/
      data ((o3(ip,iw),iw=1,21), ip= 7, 7)/ &
        0.7983E-11,  0.2661E-10,  0.1331E-10,  0.4523E-10,  0.1676E-09, &
        0.2874E-09,  0.7398E-09,  0.1884E-08,  0.4633E-08,  0.1102E-07, &
        0.2335E-07,  0.4021E-07,  0.4932E-07,  0.3046E-07, -0.2749E-07, &
       -0.1258E-06, -0.2580E-06, -0.4013E-06, -0.5137E-06, -0.5636E-06, &
       -0.5487E-06/
      data ((o1(ip,iw),iw=1,21), ip= 8, 8)/ &
        0.99999386,  0.99998748,  0.99997449,  0.99994886,  0.99989724, &
        0.99979615,  0.99959838,  0.99921793,  0.99850750,  0.99723095, &
        0.99507236,  0.99170661,  0.98691559,  0.98056912,  0.97235346, &
        0.96153617,  0.94692683,  0.92699933,  0.90010893,  0.86476213, &
        0.81984389/
      data ((o2(ip,iw),iw=1,21), ip= 8, 8)/ &
        0.2831E-08,  0.1788E-08,  0.4470E-08,  0.1042E-08, -0.4921E-08, &
       -0.2698E-07, -0.9344E-07, -0.3080E-06, -0.9500E-06, -0.2751E-05, &
       -0.7370E-05, -0.1770E-04, -0.3724E-04, -0.6854E-04, -0.1136E-03, &
       -0.1759E-03, -0.2597E-03, -0.3669E-03, -0.4956E-03, -0.6413E-03, &
       -0.8010E-03/
      data ((o3(ip,iw),iw=1,21), ip= 8, 8)/ &
        0.7983E-11,  0.3193E-10,  0.2661E-10,  0.3992E-10,  0.1730E-09, &
        0.2954E-09,  0.7052E-09,  0.1865E-08,  0.4521E-08,  0.1081E-07, &
        0.2286E-07,  0.3930E-07,  0.4798E-07,  0.2847E-07, -0.3065E-07, &
       -0.1308E-06, -0.2659E-06, -0.4115E-06, -0.5233E-06, -0.5654E-06, &
       -0.5317E-06/
      data ((o1(ip,iw),iw=1,21), ip= 9, 9)/ &
        0.99999386,  0.99998748,  0.99997455,  0.99994898,  0.99989742, &
        0.99979621,  0.99959826,  0.99921739,  0.99850500,  0.99722350, &
        0.99505132,  0.99165231,  0.98679137,  0.98031354,  0.97187132, &
        0.96067154,  0.94542491,  0.92445219,  0.89590424,  0.85808158, &
        0.80973923/
      data ((o2(ip,iw),iw=1,21), ip= 9, 9)/ &
        0.2533E-08,  0.1788E-08,  0.4470E-08,  0.1638E-08, -0.5218E-08, &
       -0.2698E-07, -0.9106E-07, -0.2995E-06, -0.9265E-06, -0.2685E-05, &
       -0.7201E-05, -0.1733E-04, -0.3655E-04, -0.6737E-04, -0.1117E-03, &
       -0.1729E-03, -0.2551E-03, -0.3601E-03, -0.4861E-03, -0.6288E-03, &
       -0.7857E-03/
      data ((o3(ip,iw),iw=1,21), ip= 9, 9)/ &
       -0.2661E-11,  0.2661E-10,  0.3725E-10,  0.3991E-10,  0.1570E-09, &
        0.2954E-09,  0.6945E-09,  0.1820E-08,  0.4425E-08,  0.1049E-07, &
        0.2213E-07,  0.3798E-07,  0.4588E-07,  0.2551E-07, -0.3499E-07, &
       -0.1371E-06, -0.2744E-06, -0.4203E-06, -0.5260E-06, -0.5519E-06, &
       -0.4904E-06/
      data ((o1(ip,iw),iw=1,21), ip=10,10)/ &
        0.99999392,  0.99998754,  0.99997467,  0.99994910,  0.99989754, &
        0.99979627,  0.99959797,  0.99921638,  0.99850142,  0.99721217, &
        0.99501920,  0.99156892,  0.98659974,  0.97992051,  0.97113293, &
        0.95935977,  0.94317663,  0.92071021,  0.88987815,  0.84879214, &
        0.79615003/
      data ((o2(ip,iw),iw=1,21), ip=10,10)/ &
        0.3576E-08,  0.3129E-08,  0.2980E-08,  0.7445E-09, -0.5069E-08, &
       -0.2623E-07, -0.8823E-07, -0.2908E-06, -0.8895E-06, -0.2586E-05, &
       -0.6945E-05, -0.1678E-04, -0.3551E-04, -0.6565E-04, -0.1091E-03, &
       -0.1689E-03, -0.2491E-03, -0.3519E-03, -0.4757E-03, -0.6166E-03, &
       -0.7724E-03/
      data ((o3(ip,iw),iw=1,21), ip=10,10)/ &
        0.2220E-15,  0.7983E-11,  0.1597E-10,  0.2927E-10,  0.1384E-09, &
        0.2927E-09,  0.6812E-09,  0.1754E-08,  0.4228E-08,  0.1002E-07, &
        0.2099E-07,  0.3603E-07,  0.4297E-07,  0.2159E-07, -0.4015E-07, &
       -0.1438E-06, -0.2816E-06, -0.4237E-06, -0.5159E-06, -0.5174E-06, &
       -0.4229E-06/
      data ((o1(ip,iw),iw=1,21), ip=11,11)/ &
        0.99999398,  0.99998754,  0.99997479,  0.99994916,  0.99989760, &
        0.99979627,  0.99959749,  0.99921471,  0.99849570,  0.99719518, &
        0.99497050,  0.99144322,  0.98630953,  0.97932506,  0.97002149, &
        0.95740712,  0.93988645,  0.91535580,  0.88149357,  0.83626372, &
        0.77838826/
      data ((o2(ip,iw),iw=1,21), ip=11,11)/ &
        0.3874E-08,  0.3427E-08,  0.1937E-08,  0.5953E-09, -0.4623E-08, &
       -0.2459E-07, -0.8465E-07, -0.2757E-06, -0.8402E-06, -0.2440E-05, &
       -0.6572E-05, -0.1596E-04, -0.3400E-04, -0.6320E-04, -0.1053E-03, &
       -0.1635E-03, -0.2419E-03, -0.3429E-03, -0.4657E-03, -0.6068E-03, &
       -0.7632E-03/
      data ((o3(ip,iw),iw=1,21), ip=11,11)/ &
       -0.5322E-11,  0.1330E-10,  0.2927E-10,  0.3725E-10,  0.1623E-09, &
        0.2741E-09,  0.6919E-09,  0.1655E-08,  0.4029E-08,  0.9337E-08, &
        0.1948E-07,  0.3328E-07,  0.3910E-07,  0.1693E-07, -0.4555E-07, &
       -0.1490E-06, -0.2840E-06, -0.4161E-06, -0.4881E-06, -0.4602E-06, &
       -0.3288E-06/
      data ((o1(ip,iw),iw=1,21), ip=12,12)/ &
        0.99999404,  0.99998772,  0.99997491,  0.99994934,  0.99989778, &
        0.99979621,  0.99959701,  0.99921221,  0.99848753,  0.99716997, &
        0.99489915,  0.99125737,  0.98587918,  0.97844428,  0.96838760, &
        0.95457482,  0.93520546,  0.90792793,  0.87019116,  0.81987017, &
        0.75581276/
      data ((o2(ip,iw),iw=1,21), ip=12,12)/ &
        0.3129E-08,  0.2533E-08,  0.1639E-08,  0.8934E-09, -0.4920E-08, &
       -0.2385E-07, -0.7944E-07, -0.2532E-06, -0.7685E-06, -0.2234E-05, &
       -0.6050E-05, -0.1482E-04, -0.3188E-04, -0.5984E-04, -0.1005E-03, &
       -0.1570E-03, -0.2338E-03, -0.3343E-03, -0.4581E-03, -0.6018E-03, &
       -0.7607E-03/
      data ((o3(ip,iw),iw=1,21), ip=12,12)/ &
        0.2661E-11,  0.2661E-11,  0.2395E-10,  0.3193E-10,  0.1410E-09, &
        0.2874E-09,  0.6360E-09,  0.1573E-08,  0.3675E-08,  0.8451E-08, &
        0.1747E-07,  0.2971E-07,  0.3447E-07,  0.1214E-07, -0.4940E-07, &
       -0.1499E-06, -0.2770E-06, -0.3938E-06, -0.4409E-06, -0.3812E-06, &
       -0.2072E-06/
      data ((o1(ip,iw),iw=1,21), ip=13,13)/ &
        0.99999410,  0.99998778,  0.99997491,  0.99994951,  0.99989790, &
        0.99979597,  0.99959606,  0.99920869,  0.99847609,  0.99713469, &
        0.99479783,  0.99099284,  0.98526359,  0.97718149,  0.96605861, &
        0.95059496,  0.92876536,  0.89796472,  0.85544366,  0.79905826, &
        0.72794402/
      data ((o2(ip,iw),iw=1,21), ip=13,13)/ &
        0.3129E-08,  0.3576E-08,  0.1787E-08,  0.1486E-09, -0.2983E-08, &
       -0.2117E-07, -0.6975E-07, -0.2243E-06, -0.6773E-06, -0.1965E-05, &
       -0.5362E-05, -0.1330E-04, -0.2909E-04, -0.5554E-04, -0.9464E-04, &
       -0.1497E-03, -0.2259E-03, -0.3276E-03, -0.4550E-03, -0.6038E-03, &
       -0.7671E-03/
      data ((o3(ip,iw),iw=1,21), ip=13,13)/ &
        0.1330E-10,  0.5322E-11,  0.5854E-10,  0.2395E-10,  0.1330E-09, &
        0.2874E-09,  0.6173E-09,  0.1456E-08,  0.3297E-08,  0.7381E-08, &
        0.1513E-07,  0.2559E-07,  0.2950E-07,  0.8483E-08, -0.4922E-07, &
       -0.1425E-06, -0.2574E-06, -0.3553E-06, -0.3765E-06, -0.2819E-06, &
       -0.5608E-07/
      data ((o1(ip,iw),iw=1,21), ip=14,14)/ &
        0.99999422,  0.99998790,  0.99997514,  0.99994957,  0.99989814, &
        0.99979585,  0.99959457,  0.99920410,  0.99846095,  0.99708819, &
        0.99466276,  0.99063551,  0.98442352,  0.97544628,  0.96286833, &
        0.94521070,  0.92022300,  0.88506019,  0.83682334,  0.77345133, &
        0.69464076/
      data ((o2(ip,iw),iw=1,21), ip=14,14)/ &
        0.1788E-08,  0.4172E-08,  0.2532E-08,  0.5952E-09, -0.3281E-08, &
       -0.2072E-07, -0.6200E-07, -0.1902E-06, -0.5675E-06, -0.1643E-05, &
       -0.4526E-05, -0.1143E-04, -0.2567E-04, -0.5038E-04, -0.8802E-04, &
       -0.1423E-03, -0.2194E-03, -0.3245E-03, -0.4583E-03, -0.6152E-03, &
       -0.7847E-03/
      data ((o3(ip,iw),iw=1,21), ip=14,14)/ &
        0.5322E-11,  0.1064E-10,  0.4524E-10,  0.4258E-10,  0.1171E-09, &
        0.2475E-09,  0.5748E-09,  0.1304E-08,  0.2858E-08,  0.6227E-08, &
        0.1264E-07,  0.2143E-07,  0.2528E-07,  0.8009E-08, -0.4213E-07, &
       -0.1241E-06, -0.2237E-06, -0.3024E-06, -0.2984E-06, -0.1640E-06, &
        0.1263E-06/
      data ((o1(ip,iw),iw=1,21), ip=15,15)/ &
        0.99999428,  0.99998802,  0.99997538,  0.99994981,  0.99989814, &
        0.99979544,  0.99959302,  0.99919838,  0.99844259,  0.99703109, &
        0.99449563,  0.99018627,  0.98334765,  0.97319114,  0.95870394, &
        0.93823278,  0.90932250,  0.86893308,  0.81409526,  0.74301666, &
        0.65632105/
      data ((o2(ip,iw),iw=1,21), ip=15,15)/ &
        0.2533E-08,  0.3725E-08,  0.3129E-08,  0.1341E-08, -0.3728E-08, &
       -0.1729E-07, -0.5351E-07, -0.1544E-06, -0.4486E-06, -0.1302E-05, &
       -0.3626E-05, -0.9383E-05, -0.2183E-04, -0.4468E-04, -0.8116E-04, &
       -0.1357E-03, -0.2151E-03, -0.3261E-03, -0.4694E-03, -0.6373E-03, &
       -0.8144E-03/
      data ((o3(ip,iw),iw=1,21), ip=15,15)/ &
        0.2395E-10,  0.2927E-10,  0.2927E-10,  0.2927E-10,  0.1197E-09, &
        0.2714E-09,  0.5508E-09,  0.1203E-08,  0.2517E-08,  0.5282E-08, &
        0.1044E-07,  0.1809E-07,  0.2306E-07,  0.1220E-07, -0.2683E-07, &
       -0.9422E-07, -0.1775E-06, -0.2387E-06, -0.2106E-06, -0.3034E-07, &
        0.3374E-06/
      data ((o1(ip,iw),iw=1,21), ip=16,16)/ &
        0.99999440,  0.99998826,  0.99997556,  0.99994981,  0.99989820, &
        0.99979490,  0.99959117,  0.99919206,  0.99842268,  0.99696815, &
        0.99430782,  0.98967069,  0.98207790,  0.97045571,  0.95356220, &
        0.92959517,  0.89595246,  0.84949458,  0.78733349,  0.70822150, &
        0.61410975/
      data ((o2(ip,iw),iw=1,21), ip=16,16)/ &
        0.4172E-08,  0.4768E-08,  0.2533E-08,  0.1340E-08, -0.3131E-08, &
       -0.1640E-07, -0.4278E-07, -0.1201E-06, -0.3446E-06, -0.9895E-06, &
       -0.2778E-05, -0.7390E-05, -0.1798E-04, -0.3888E-04, -0.7454E-04, &
       -0.1302E-03, -0.2137E-03, -0.3327E-03, -0.4884E-03, -0.6700E-03, &
       -0.8551E-03/
      data ((o3(ip,iw),iw=1,21), ip=16,16)/ &
        0.2661E-10,  0.1064E-10,  0.2395E-10,  0.5056E-10,  0.1038E-09, &
        0.2608E-09,  0.5295E-09,  0.1139E-08,  0.2320E-08,  0.4689E-08, &
        0.9127E-08,  0.1625E-07,  0.2345E-07,  0.2114E-07, -0.3140E-08, &
       -0.5377E-07, -0.1202E-06, -0.1666E-06, -0.1168E-06,  0.1124E-06, &
        0.5669E-06/
      data ((o1(ip,iw),iw=1,21), ip=17,17)/ &
        0.99999464,  0.99998838,  0.99997562,  0.99995005,  0.99989802, &
        0.99979442,  0.99958938,  0.99918658,  0.99840337,  0.99690610, &
        0.99411917,  0.98913842,  0.98071784,  0.96739835,  0.94761288, &
        0.91942477,  0.88021058,  0.82691038,  0.75699466,  0.67009914, &
        0.56980520/
      data ((o2(ip,iw),iw=1,21), ip=17,17)/ &
        0.3427E-08,  0.6109E-08,  0.3427E-08,  0.1042E-08, -0.2684E-08, &
       -0.1327E-07, -0.3622E-07, -0.9673E-07, -0.2617E-06, -0.7404E-06, &
       -0.2090E-05, -0.5699E-05, -0.1452E-04, -0.3342E-04, -0.6839E-04, &
       -0.1260E-03, -0.2148E-03, -0.3436E-03, -0.5136E-03, -0.7104E-03, &
       -0.9030E-03/
      data ((o3(ip,iw),iw=1,21), ip=17,17)/ &
        0.1863E-10,  0.2395E-10,  0.3459E-10,  0.6120E-10,  0.1171E-09, &
        0.2688E-09,  0.4869E-09,  0.1088E-08,  0.2225E-08,  0.4423E-08, &
        0.8651E-08,  0.1588E-07,  0.2586E-07,  0.3314E-07,  0.2578E-07, &
       -0.4755E-08, -0.5304E-07, -0.8497E-07, -0.1866E-07,  0.2541E-06, &
        0.7976E-06/
      data ((o1(ip,iw),iw=1,21), ip=18,18)/ &
        0.99999475,  0.99998856,  0.99997580,  0.99995005,  0.99989790, &
        0.99979395,  0.99958789,  0.99918169,  0.99838680,  0.99685186, &
        0.99395061,  0.98864597,  0.97940272,  0.96428061,  0.94121426, &
        0.90808922,  0.86244535,  0.80163682,  0.72390091,  0.63013542, &
        0.52561903/
      data ((o2(ip,iw),iw=1,21), ip=18,18)/ &
        0.2682E-08,  0.5364E-08,  0.3724E-08,  0.1638E-08, -0.4174E-08, &
       -0.1282E-07, -0.3234E-07, -0.7870E-07, -0.2052E-06, -0.5674E-06, &
       -0.1589E-05, -0.4406E-05, -0.1168E-04, -0.2855E-04, -0.6259E-04, &
       -0.1224E-03, -0.2174E-03, -0.3569E-03, -0.5417E-03, -0.7533E-03, &
       -0.9517E-03/
      data ((o3(ip,iw),iw=1,21), ip=18,18)/ &
        0.3193E-10,  0.2129E-10,  0.6652E-10,  0.6120E-10,  0.1118E-09, &
        0.2235E-09,  0.4816E-09,  0.1134E-08,  0.2200E-08,  0.4356E-08, &
        0.8621E-08,  0.1642E-07,  0.2916E-07,  0.4549E-07,  0.5671E-07, &
        0.5038E-07,  0.2591E-07,  0.1114E-07,  0.8870E-07,  0.3902E-06, &
        0.1013E-05/
      data ((o1(ip,iw),iw=1,21), ip=19,19)/ &
        0.99999493,  0.99998879,  0.99997604,  0.99995005,  0.99989778, &
        0.99979347,  0.99958694,  0.99917805,  0.99837393,  0.99680883, &
        0.99381369,  0.98823392,  0.97825152,  0.96138662,  0.93487167, &
        0.89621651,  0.84331334,  0.77443409,  0.68916380,  0.59004080, &
        0.48375273/
      data ((o2(ip,iw),iw=1,21), ip=19,19)/ &
        0.4470E-08,  0.5364E-08,  0.5512E-08,  0.1936E-08, -0.3727E-08, &
       -0.1058E-07, -0.3026E-07, -0.7021E-07, -0.1708E-06, -0.4531E-06, &
       -0.1246E-05, -0.3476E-05, -0.9479E-05, -0.2433E-04, -0.5688E-04, &
       -0.1184E-03, -0.2200E-03, -0.3705E-03, -0.5691E-03, -0.7929E-03, &
       -0.9945E-03/
      data ((o3(ip,iw),iw=1,21), ip=19,19)/ &
        0.2129E-10,  0.1064E-10,  0.5056E-10,  0.5588E-10,  0.1038E-09, &
        0.2634E-09,  0.5242E-09,  0.1099E-08,  0.2214E-08,  0.4380E-08, &
        0.8696E-08,  0.1695E-07,  0.3202E-07,  0.5589E-07,  0.8540E-07, &
        0.1086E-06,  0.1166E-06,  0.1280E-06,  0.2163E-06,  0.5286E-06, &
        0.1204E-05/
      data ((o1(ip,iw),iw=1,21), ip=20,20)/ &
        0.99999505,  0.99998903,  0.99997604,  0.99994999,  0.99989754, &
        0.99979341,  0.99958634,  0.99917579,  0.99836481,  0.99677712, &
        0.99371070,  0.98791438,  0.97732371,  0.95892221,  0.92909217, &
        0.88464081,  0.82380420,  0.74640268,  0.65413809,  0.55151534, &
        0.44598198/
      data ((o2(ip,iw),iw=1,21), ip=20,20)/ &
        0.3129E-08,  0.6407E-08,  0.5512E-08,  0.2384E-08, -0.4174E-08, &
       -0.1148E-07, -0.2981E-07, -0.6037E-07, -0.1497E-06, -0.3744E-06, &
       -0.1013E-05, -0.2820E-05, -0.7808E-05, -0.2077E-04, -0.5122E-04, &
       -0.1133E-03, -0.2212E-03, -0.3828E-03, -0.5935E-03, -0.8257E-03, &
       -0.1027E-02/
      data ((o3(ip,iw),iw=1,21), ip=20,20)/ &
        0.2927E-10,  0.1330E-10,  0.5588E-10,  0.3193E-10,  0.6918E-10, &
        0.1996E-09,  0.5322E-09,  0.1051E-08,  0.2209E-08,  0.4444E-08, &
        0.8818E-08,  0.1753E-07,  0.3417E-07,  0.6380E-07,  0.1095E-06, &
        0.1649E-06,  0.2161E-06,  0.2661E-06,  0.3740E-06,  0.6873E-06, &
        0.1384E-05/
      data ((o1(ip,iw),iw=1,21), ip=21,21)/ &
        0.99999535,  0.99998903,  0.99997604,  0.99994993,  0.99989718, &
        0.99979323,  0.99958587,  0.99917406,  0.99835837,  0.99675494, &
        0.99363708,  0.98768157,  0.97662324,  0.95697355,  0.92423612, &
        0.87421501,  0.80518728,  0.71900427,  0.62041485,  0.51613665, &
        0.41349036/
      data ((o2(ip,iw),iw=1,21), ip=21,21)/ &
        0.3576E-08,  0.7152E-08,  0.6109E-08,  0.2533E-08, -0.4473E-08, &
       -0.1520E-07, -0.2892E-07, -0.5843E-07, -0.1327E-06, -0.3234E-06, &
       -0.8557E-06, -0.2359E-05, -0.6565E-05, -0.1786E-04, -0.4587E-04, &
       -0.1070E-03, -0.2198E-03, -0.3928E-03, -0.6148E-03, -0.8513E-03, &
       -0.1048E-02/
      data ((o3(ip,iw),iw=1,21), ip=21,21)/ &
        0.2129E-10,  0.3193E-10,  0.2395E-10,  0.2395E-10,  0.1171E-09, &
        0.2129E-09,  0.5375E-09,  0.1102E-08,  0.2246E-08,  0.4497E-08, &
        0.8896E-08,  0.1776E-07,  0.3551E-07,  0.6931E-07,  0.1280E-06, &
        0.2145E-06,  0.3150E-06,  0.4167E-06,  0.5573E-06,  0.8769E-06, &
        0.1580E-05/
      data ((o1(ip,iw),iw=1,21), ip=22,22)/ &
        0.99999571,  0.99998921,  0.99997592,  0.99994940,  0.99989718, &
        0.99979347,  0.99958587,  0.99917299,  0.99835443,  0.99674016, &
        0.99358666,  0.98751831,  0.97612083,  0.95552617,  0.92044622, &
        0.86556542,  0.78875232,  0.69391191,  0.58969289,  0.48527473, &
        0.38690841/
      data ((o2(ip,iw),iw=1,21), ip=22,22)/ &
        0.4768E-08,  0.7003E-08,  0.5960E-08,  0.3724E-08, -0.4770E-08, &
       -0.1282E-07, -0.3115E-07, -0.5903E-07, -0.1242E-06, -0.2923E-06, &
       -0.7493E-06, -0.2036E-05, -0.5660E-05, -0.1560E-04, -0.4121E-04, &
       -0.1002E-03, -0.2157E-03, -0.3994E-03, -0.6334E-03, -0.8725E-03, &
       -0.1062E-02/
      data ((o3(ip,iw),iw=1,21), ip=22,22)/ &
       -0.5322E-11,  0.3459E-10,  0.3725E-10,  0.6120E-10,  0.9047E-10, &
        0.1969E-09,  0.5029E-09,  0.1123E-08,  0.2259E-08,  0.4462E-08, &
        0.8922E-08,  0.1803E-07,  0.3644E-07,  0.7294E-07,  0.1409E-06, &
        0.2527E-06,  0.4028E-06,  0.5655E-06,  0.7534E-06,  0.1096E-05, &
        0.1807E-05/
      data ((o1(ip,iw),iw=1,21), ip=23,23)/ &
        0.99999624,  0.99998933,  0.99997568,  0.99994916,  0.99989706, &
        0.99979329,  0.99958593,  0.99917239,  0.99835163,  0.99673033, &
        0.99355358,  0.98740870,  0.97577649,  0.95450813,  0.91767871, &
        0.85892558,  0.77540523,  0.67262524,  0.56352603,  0.46000499, &
        0.36636806/
      data ((o2(ip,iw),iw=1,21), ip=23,23)/ &
        0.5514E-08,  0.5365E-08,  0.4618E-08,  0.3575E-08, -0.4324E-08, &
       -0.1282E-07, -0.3026E-07, -0.5709E-07, -0.1179E-06, -0.2655E-06, &
       -0.6794E-06, -0.1817E-05, -0.5025E-05, -0.1392E-04, -0.3743E-04, &
       -0.9388E-04, -0.2100E-03, -0.4029E-03, -0.6501E-03, -0.8927E-03, &
       -0.1073E-02/
      data ((o3(ip,iw),iw=1,21), ip=23,23)/ &
       -0.5056E-10, -0.5322E-11,  0.5056E-10,  0.4790E-10,  0.1144E-09, &
        0.2235E-09,  0.4817E-09,  0.1136E-08,  0.2281E-08,  0.4441E-08, &
        0.8898E-08,  0.1800E-07,  0.3685E-07,  0.7492E-07,  0.1487E-06, &
        0.2793E-06,  0.4734E-06,  0.7017E-06,  0.9491E-06,  0.1327E-05, &
        0.2047E-05/
      data ((o1(ip,iw),iw=1,21), ip=24,24)/ &
        0.99999648,  0.99998945,  0.99997538,  0.99994904,  0.99989724, &
        0.99979335,  0.99958575,  0.99917179,  0.99835020,  0.99672443, &
        0.99353158,  0.98733693,  0.97555012,  0.95382470,  0.91576988, &
        0.85417569,  0.76542622,  0.65605462,  0.54297459,  0.44087917, &
        0.35146344/
      data ((o2(ip,iw),iw=1,21), ip=24,24)/ &
        0.6259E-08,  0.8643E-08,  0.4023E-08,  0.1339E-08, -0.2982E-08, &
       -0.1163E-07, -0.2937E-07, -0.5784E-07, -0.1134E-06, -0.2483E-06, &
       -0.6303E-06, -0.1666E-05, -0.4587E-05, -0.1272E-04, -0.3461E-04, &
       -0.8872E-04, -0.2046E-03, -0.4050E-03, -0.6660E-03, -0.9135E-03, &
       -0.1084E-02/
      data ((o3(ip,iw),iw=1,21), ip=24,24)/ &
       -0.2129E-10, -0.2129E-10,  0.2927E-10,  0.8249E-10,  0.1118E-09, &
        0.1543E-09,  0.5242E-09,  0.1150E-08,  0.2238E-08,  0.4396E-08, &
        0.8975E-08,  0.1803E-07,  0.3697E-07,  0.7580E-07,  0.1534E-06, &
        0.2973E-06,  0.5271E-06,  0.8204E-06,  0.1131E-05,  0.1538E-05, &
        0.2256E-05/
      data ((o1(ip,iw),iw=1,21), ip=25,25)/ &
        0.99999648,  0.99998963,  0.99997491,  0.99994934,  0.99989718, &
        0.99979317,  0.99958575,  0.99917185,  0.99834859,  0.99672103, &
        0.99351835,  0.98729140,  0.97540486,  0.95338494,  0.91452152, &
        0.85099351,  0.75854135,  0.64429641,  0.52830541,  0.42760271, &
        0.34112751/
      data ((o2(ip,iw),iw=1,21), ip=25,25)/ &
        0.5811E-08,  0.1118E-07,  0.7442E-09,  0.1486E-09, -0.3876E-08, &
       -0.1312E-07, -0.2877E-07, -0.5560E-07, -0.1114E-06, -0.2378E-06, &
       -0.6010E-06, -0.1575E-05, -0.4310E-05, -0.1195E-04, -0.3275E-04, &
       -0.8523E-04, -0.2009E-03, -0.4076E-03, -0.6813E-03, -0.9327E-03, &
       -0.1091E-02/
      data ((o3(ip,iw),iw=1,21), ip=25,25)/ &
        0.3991E-10, -0.5056E-10,  0.4524E-10,  0.1863E-10,  0.1064E-09, &
        0.1863E-09,  0.5349E-09,  0.1099E-08,  0.2334E-08,  0.4351E-08, &
        0.8887E-08,  0.1819E-07,  0.3731E-07,  0.7656E-07,  0.1566E-06, &
        0.3101E-06,  0.5681E-06,  0.9160E-06,  0.1278E-05,  0.1694E-05, &
        0.2405E-05/
      data ((o1(ip,iw),iw=1,21), ip=26,26)/ &
        0.99999672,  0.99998963,  0.99997467,  0.99994934,  0.99989712, &
        0.99979317,  0.99958575,  0.99917191,  0.99834746,  0.99671894, &
        0.99350989,  0.98726571,  0.97531629,  0.95311588,  0.91374940, &
        0.84899974,  0.75415814,  0.63668656,  0.51872611,  0.41882724, &
        0.33376986/
      data ((o2(ip,iw),iw=1,21), ip=26,26)/ &
        0.5960E-08,  0.1401E-07, -0.3428E-08,  0.4023E-08, -0.3429E-08, &
       -0.1252E-07, -0.3011E-07, -0.5501E-07, -0.1039E-06, -0.2331E-06, &
       -0.5801E-06, -0.1513E-05, -0.4143E-05, -0.1151E-04, -0.3169E-04, &
       -0.8333E-04, -0.1993E-03, -0.4107E-03, -0.6933E-03, -0.9453E-03, &
       -0.1093E-02/
      data ((o3(ip,iw),iw=1,21), ip=26,26)/ &
        0.2129E-10,  0.5322E-11,  0.1863E-10,  0.1330E-10,  0.1091E-09, &
        0.1863E-09,  0.5588E-09,  0.1072E-08,  0.2328E-08,  0.4369E-08, &
        0.8930E-08,  0.1798E-07,  0.3744E-07,  0.7708E-07,  0.1592E-06, &
        0.3195E-06,  0.5962E-06,  0.9783E-06,  0.1370E-05,  0.1785E-05, &
        0.2503E-05/




! Start programming
!-----compute layer pressure (pa) and layer temperature minus 250k (dt)
 do k=1,np
!dir$ vector aligned
 DO ic=1,irestrict
    pa(ic,k)=0.5*(pl(ic,k)+pl(ic,k+1))
    dt(ic,k)=ta(ic,k)-250.0
 ENDDO
 enddo

!-----compute layer absorber amount
!     dh2o : water vapor amount (g/cm**2)
!     dcont: scaled water vapor amount for continuum absorption
!            (g/cm**2)
!     dco2 : co2 amount (cm-atm)stp
!     do3  : o3 amount (cm-atm)stp
!     dn2o : n2o amount (cm-atm)stp
!     dch4 : ch4 amount (cm-atm)stp
!     df11 : cfc11 amount (cm-atm)stp
!     df12 : cfc12 amount (cm-atm)stp
!     df22 : cfc22 amount (cm-atm)stp
!     the factor 1.02 is equal to 1000/980
!     factors 789 and 476 are for unit conversion
!     the factor 0.001618 is equal to 1.02/(.622*1013.25)
!     the factor 6.081 is equal to 1800/296
 do k=1,np
!dir$ vector aligned
 DO ic=1,irestrict

!old
!    dp   (ic,k) = pl(ic,k+1)-pl(ic,k)
!    dh2o (ic,k) = 1.02*wa(ic,k)*dp(ic,k)+1.e-10
!    do3  (ic,k) = 476.*oa(ic,k)*dp(ic,k)+1.e-10
!    dco2 (ic,k) = 789.*co2*dp(ic,k)+1.e-10
!    dch4 (ic,k) = 789.*ch4*dp(ic,k)+1.e-10
!    dn2o (ic,k) = 789.*n2o*dp(ic,k)+1.e-10
!    df11 (ic,k) = 789.*cfc11*dp(ic,k)+1.e-10
!    df12 (ic,k) = 789.*cfc12*dp(ic,k)+1.e-10
!    df22 (ic,k) = 789.*cfc22*dp(ic,k)+1.e-10
!new
     dp   (ic,k) = pl(ic,k+1)-pl(ic,k)
     dh2o (ic,k) = 1.02*wa(ic,k)*dp(ic,k)
     dh2o (ic,k) = max(dh2o (ic,k),1.e-30)
     do3  (ic,k) = 476.*oa(ic,k)*dp(ic,k)
     do3  (ic,k) = max(do3 (ic,k),1.e-30)
     dco2 (ic,k) = 789.*co2*dp(ic,k)
     dco2 (ic,k) = max(dco2 (ic,k),1.e-30)
     dch4 (ic,k) = 789.*ch4*dp(ic,k)
     dn2o (ic,k) = 789.*n2o*dp(ic,k)
     df11 (ic,k) = 789.*cfc11*dp(ic,k)
     df12 (ic,k) = 789.*cfc12*dp(ic,k)
     df22 (ic,k) = 789.*cfc22*dp(ic,k)

!-----compute scaled water vapor amount for h2o continuum absorption
!     following eq. (4.21).
     xx=pa(ic,k)*0.001618*wa(ic,k)*wa(ic,k)*dp(ic,k)
     dcont(ic,k) = xx*exp(1800./ta(ic,k)-6.081)+1.e-10
 ENDDO
 enddo
!-----compute column-integrated h2o amoumt (sh2o), h2o-weighted pressure
!     (swpre) and temperature (swtem). it follows eqs. (4.13) and (4.14).

 if (high) then
     call column(np,pa,dt,dh2o,sh2o,swpre,swtem,irestrict)
 endif

!-----the surface (np+1) is treated as a layer filled with black clouds.
!     transfc is the transmttance between the surface and a pressure level.
!     trantcr is the clear-sky transmttance between the surface and a
!     pressure level.

!dir$ vector aligned
 DO ic=1,irestrict
   sfcem(ic)       =0.e0
   transfc(ic,np+1)=1.e0
   trantcr(ic,np+1)=1.e0

!- ----initialize fluxes
   flx(ic,:)  = 0.e0
   flc(ic,:)  = 0.e0
   dfdts(ic,:)= 0.e0
   rflx(ic,:) = 0.e0
   rflc(ic,:) = 0.e0
   acflxu(ic,:) = 0.e0
   acflxd(ic,:) = 0.e0
 ENDDO
!-----integration over spectral bands

 BAND_LOOP: do ibn=1,nband_lw

!  Assign logical parameters
!-----if h2otbl, compute h2o (line) transmittance using table look-up.
!     if conbnd, compute h2o (continuum) transmittance in bands 3-7.
!     if co2bnd, compute co2 transmittance in band 3.
!     if oznbnd, compute  o3 transmittance in band 5.
!     if n2obnd, compute n2o transmittance in bands 6 and 7.
!     if ch4bnd, compute ch4 transmittance in bands 6 and 7.
!     if combnd, compute co2-minor transmittance in bands 4 and 5.
!     if f11bnd, compute cfc11 transmittance in bands 4 and 5.
!     if f12bnd, compute cfc12 transmittance in bands 4 and 6.
!     if f22bnd, compute cfc22 transmittance in bands 4 and 6.
!     if b10bnd, compute flux reduction due to n2o in band 10.
    h2otbl=high.and.(ibn.eq.1.or.ibn.eq.2.or.ibn.eq.8)
    conbnd=ibn.ge.3.and.ibn.le.7
    co2bnd=ibn.eq.3
    oznbnd=ibn.eq.5
    n2obnd=ibn.eq.6.or.ibn.eq.7
    ch4bnd=ibn.eq.6.or.ibn.eq.7
    combnd=ibn.eq.4.or.ibn.eq.5
    f11bnd=ibn.eq.4.or.ibn.eq.5
    f12bnd=ibn.eq.4.or.ibn.eq.6
    f22bnd=ibn.eq.4.or.ibn.eq.6
    b10bnd=ibn.eq.10

    if (.not. b10bnd .or. trace) then          ! skip b10 and .not.trace
!-----blayer is the spectrally integrated planck flux of the mean layer
!     temperature derived from eq. (3.11)
!     the fitting for the planck flux is valid for the range 160-345 k.
       do k=1,np
!dir$ vector aligned
       DO ic=1,irestrict
          blayer(ic,k)=ta(ic,k)*(ta(ic,k)*(ta(ic,k)*(ta(ic,k) &
                     *(ta(ic,k)*cb(6,ibn)+cb(5,ibn))+cb(4,ibn)) &
                     +cb(3,ibn))+cb(2,ibn))+cb(1,ibn)
       ENDDO
       enddo
!-----the earth's surface, with index "np+1", is treated as a layer.
!     index "0" is the layer above the top of the atmosphere.
!dir$ vector aligned
       DO ic=1,irestrict
         blayer(ic,np+1)=(ts(ic)*(ts(ic)*(ts(ic)*(ts(ic) &
                       *(ts(ic)*cb(6,ibn)+cb(5,ibn))+cb(4,ibn)) &
                       +cb(3,ibn))+cb(2,ibn))+cb(1,ibn))*emiss(ic,ibn)   !surface skin 
         blayer(ic,0)   = 0.0
!-----dbs is the derivative of the surface emission with respect to
!     surface temperature eq. (3.12).
        dbs(ic)=(ts(ic)*(ts(ic)*(ts(ic)*(ts(ic)*5.*cb(6,ibn)+4.*cb(5,ibn)) &
              +3.*cb(4,ibn))+2.*cb(3,ibn))+cb(2,ibn))*emiss(ic,ibn)      !surface skin
       ENDDO
!-----difference in planck functions between adjacent layers.
       do k=1,np+1
!dir$ vector aligned
       DO ic=1,irestrict
         dblayr(ic,k)=blayer(ic,k-1)-blayer(ic,k)
       ENDDO
       enddo
!------interpolate planck function at model levels
       do k=2,np
!dir$ vector aligned
       DO ic=1,irestrict
         blevel(ic,k)=(blayer(ic,k-1)*dp(ic,k)+blayer(ic,k)*dp(ic,k-1))/ &
                     (dp(ic,k-1)+dp(ic,k))
       ENDDO
       enddo
!dir$ vector aligned
       DO ic=1,irestrict
         blevel(ic,1)=blayer(ic,1)+(blayer(ic,1)-blayer(ic,2))*dp(ic,1)/ &
                     (dp(ic,1)+dp(ic,2))                                 !TOA
         blevel(ic,np+1)=tb(ic)*(tb(ic)*(tb(ic)*(tb(ic) &
                       *(tb(ic)*cb(6,ibn)+cb(5,ibn))+cb(4,ibn)) &
                       +cb(3,ibn))+cb(2,ibn))+cb(1,ibn)            !surface air
      ENDDO
!-----compute column-integrated absorber amoumt, absorber-weighted
!     pressure and temperature for co2 (band 3) and o3 (band 5).
!     it follows eqs. (4.13) and (4.14).
!-----this is in the band loop to save storage

      if (high .and. co2bnd) then
        call column(np,pa,dt,dco2,sco3,scopre,scotem,irestrict)
      endif
      if (oznbnd) then
        call column(np,pa,dt,do3,sco3,scopre,scotem,irestrict)
      endif

!
! New single scatter properties was derived from G-SDSU.
!

       do k=1,np
!dir$ vector aligned
       DO ic=1,irestrict
           tcldlyr(ic,k) = 1.0
           taux=taucl(ic,k,ibn)  ! condensate optical depths
           if (taux.gt.taux_min .and. fcld(ic,k).gt.fcld_min) then  !pppp
              ww=ssaal(ic,k,ibn)  ! single-scattering albedo
              gg=asyal(ic,k,ibn)  ! asymetry parameter         
!-----parameterization of lw scattering following eqs. (6.8) and (6.9).
              ff=0.5+(0.3739+(0.0076+0.1185*gg)*gg)*gg
              taux=taux*(1.-ww*ff)
!-----compute cloud diffuse transmittance. it is approximated by using
!     a diffusivity factor of 1.66.
              tauxa=max(0.,1.66*taux)
              tcldlyr(ic,k)=0.
              if(tauxa.lt.80.) tcldlyr(ic,k)=exp(-tauxa)
           endif
      ENDDO
      enddo

!-----for aerosol diffuse transmittance
!     the same scaling of cloud optical thickness is applied to aerosols
       do k=1,np
!dir$ vector aligned
       DO ic=1,irestrict
           taerlyr(ic,k)=1.0
          if (taual(ic,k,ibn).gt.0.01) then
           ff=0.5+(0.3739+(0.0076+0.1185*asyal(ic,k,ibn)) &
            *asyal(ic,k,ibn))*asyal(ic,k,ibn)
           taux=taual(ic,k,ibn)*(1.-ssaal(ic,k,ibn)*ff)
           taerlyr(ic,k)=exp(-1.66*taux)
          endif
      ENDDO
      enddo
!-----compute the exponential terms (eq. 8.18) at each layer due to
!     water vapor line absorption when k-distribution is used
      if (.not.h2otbl .and. .not.b10bnd) then
        call h2oexps(ibn,np,dh2o,pa,dt,xkw,aw,bw,pm,mw,h2oexp,irestrict)
      endif
!-----compute the exponential terms (eq. 8.18) at each layer due to
!     water vapor continuum absorption
      if (conbnd) then
        call conexps(ibn,np,dcont,xke,conexp,irestrict)
      endif
!-----compute the exponential terms (eq. 8.18) at each layer due to
!     co2 absorption
      if (.not.high .and. co2bnd) then
        call co2exps(np,dco2,pa,dt,co2exp,irestrict)
      endif
!***** for trace gases *****
      if (trace) then
!-----compute the exponential terms at each layer due to n2o absorption
       if (n2obnd) then
        call n2oexps(ibn,np,dn2o,pa,dt,n2oexp,irestrict)
       endif
!-----compute the exponential terms at each layer due to ch4 absorption
       if (ch4bnd) then
        call ch4exps(ibn,np,dch4,pa,dt,ch4exp,irestrict)
       endif
!-----compute the exponential terms due to co2 minor absorption
       if (combnd) then
        call comexps(ibn,np,dco2,dt,comexp,irestrict)
       endif
!-----compute the exponential terms due to cfc11 absorption.
!     the values of the parameters are given in table 7.
       if (f11bnd) then
            a1  = 1.26610e-3
            b1  = 3.55940e-6
            fk1 = 1.89736e+1
            a2  = 8.19370e-4
            b2  = 4.67810e-6
            fk2 = 1.01487e+1
        call cfcexps(ibn,np,a1,b1,fk1,a2,b2,fk2,df11,dt,f11exp,irestrict)
       endif
!-----compute the exponential terms due to cfc12 absorption.
       if (f12bnd) then
            a1  = 8.77370e-4
            b1  =-5.88440e-6
            fk1 = 1.58104e+1
            a2  = 8.62000e-4
            b2  =-4.22500e-6
            fk2 = 3.70107e+1
        call cfcexps(ibn,np,a1,b1,fk1,a2,b2,fk2,df12,dt,f12exp,irestrict)
       endif
!-----compute the exponential terms due to cfc22 absorption.
       if (f22bnd) then
            a1  = 9.65130e-4
            b1  = 1.31280e-5
            fk1 = 6.18536e+0
            a2  =-3.00010e-5
            b2  = 5.25010e-7
            fk2 = 3.27912e+1
        call cfcexps(ibn,np,a1,b1,fk1,a2,b2,fk2,df22,dt,f22exp,irestrict)
       endif
!-----compute the exponential terms at each layer in band 10 due to
!     h2o line and continuum, co2, and n2o absorption
       if (b10bnd) then
        call b10exps(np,dh2o,dcont,dco2,dn2o,pa,dt &
                    ,h2oexp,conexp,co2exp,n2oexp,irestrict)
       endif
      endif
!-----compute transmittances for regions between levels k1 and k2
!     and update fluxes at the two levels.
!-----initialize fluxes
      do k=1,np+1
!dir$ vector aligned
      DO ic=1,irestrict
         flxu(ic,k) = 0.0
         flxd(ic,k) = 0.0
         flcu(ic,k) = 0.0
         flcd(ic,k) = 0.0
      ENDDO
      enddo


      do 2000 k1=1,np
!-----initialization
!
!     it, im, and ib are the numbers of cloudy layers in the high,
!     middle, and low cloud groups between levels k1 and k2.
!     cldlw, cldmd, and cldhi are the equivalent black-cloud fractions
!     of low, middle, and high troposphere.
!     tranal is the aerosol transmission function
          it = 0
          im = 0
          ib = 0
          cldlw(1:irestrict) = 0.0
          cldmd(1:irestrict) = 0.0
          cldhi(1:irestrict) = 0.0
          tranal(1:irestrict)= 1.0
!-----for h2o line transmission
      if (.not. h2otbl) then
        do ik=1,6
           th2o(1:irestrict,ik)=1.0
        enddo
      endif
!-----for h2o continuum transmission
         do iq=1,3
            tcon(1:irestrict,iq)=1.0
         enddo
!-----for co2 transmission using k-distribution method.
!     band 3 is divided into 3 sub-bands, but sub-bands 3a and 3c
!     are combined in computing the co2 transmittance.
       if (.not.high .and. co2bnd) then
         do isb=1,2
          do ik=1,6
             tco2(1:irestrict,ik,isb)=1.0
          enddo
         enddo
       endif
!***** for trace gases *****
      if (trace) then
!-----for n2o transmission using k-distribution method.
       if (n2obnd) then
          do ik=1,4
             tn2o(1:irestrict,ik)=1.0
          enddo
       endif
!-----for ch4 transmission using k-distribution method.
       if (ch4bnd) then
          do ik=1,4
             tch4(1:irestrict,ik)=1.0
          enddo
       endif
!-----for co2-minor transmission using k-distribution method.
       if (combnd) then
          do ik=1,6
             tcom(1:irestrict,ik)=1.0
          enddo
       endif
!-----for cfc-11 transmission using k-distribution method.
       if (f11bnd) then
             tf11(1:irestrict)=1.0
       endif
!-----for cfc-12 transmission using k-distribution method.
       if (f12bnd) then
             tf12(1:irestrict)=1.0
       endif
!-----for cfc-22 transmission when using k-distribution method.
       if (f22bnd) then
             tf22(1:irestrict)=1.0
       endif
!-----for the transmission in band 10 using k-distribution method.
       if (b10bnd) then
          do ik=1,5
              th2o(1:irestrict,ik)=1.0
          enddo
          do ik=1,6
              tco2(1:irestrict,ik,1)=1.0
          enddo
          tcon(1:irestrict,1)=1.0
          do ik=1,2
              tn2o(1:irestrict,ik)=1.0
          enddo
       endif
      endif
!***** end trace gases *****
      fclr(1:irestrict)=1.0
!-----loop over the bottom level of the region (k2)
      do 3000 k2=k1+1,np+1


!-----trant is the total transmittance between levels k1 and k2.
      trant(1:irestrict)=1.0
      if (h2otbl) then
!-----compute water vapor transmittance using table look-up.
!     the following values are taken from table 8.
          w1=-8.0
          p1=-2.0
          dwe=0.3
          dpe=0.2
          if (ibn.eq.1) then
           call tablup(k1,k2,np,nx2,nh,sh2o,swpre,swtem, &
                       w1,p1,dwe,dpe,h11,h12,h13,trant,irestrict)
          endif
          if (ibn.eq.2) then
           call tablup(k1,k2,np,nx2,nh,sh2o,swpre,swtem, &
                       w1,p1,dwe,dpe,h21,h22,h23,trant,irestrict)
          endif
          if (ibn.eq.8) then
           call tablup(k1,k2,np,nx2,nh,sh2o,swpre,swtem, &
                       w1,p1,dwe,dpe,h81,h82,h83,trant,irestrict)
          endif

      else
!-----compute water vapor transmittance using k-distribution
       if (.not.b10bnd) then
        call h2okdis(ibn,np,k2-1,fkw,gkw,ne,h2oexp,conexp, &
                     th2o,tcon,trant,irestrict)
       endif

      endif

      if (co2bnd) then
        if (high) then
!-----compute co2 transmittance using table look-up method.
!     the following values are taken from table 8.
          w1=-4.0
          p1=-2.0
          dwe=0.3
          dpe=0.2
          call tablup(k1,k2,np,nx2,nc,sco3,scopre,scotem, &
                      w1,p1,dwe,dpe,c1,c2,c3,trant,irestrict)
       else
!-----compute co2 transmittance using k-distribution method
          call co2kdis(np,k2-1,co2exp,tco2,trant,irestrict)
        endif
      endif
!-----always use table look-up to compute o3 transmittance.
!     the following values are taken from table 8.

      if (oznbnd) then
          w1=-6.0
          p1=-2.0
          dwe=0.3
          dpe=0.2
          call tablup(k1,k2,np,nx2,no,sco3,scopre,scotem, &
                      w1,p1,dwe,dpe,o1,o2,o3,trant,irestrict)
      endif


!***** for trace gases *****
      if (trace) then
!-----compute n2o transmittance using k-distribution method
       if (n2obnd) then
          call n2okdis(ibn,np,k2-1,n2oexp,tn2o,trant,irestrict)
       endif
!-----compute ch4 transmittance using k-distribution method
       if (ch4bnd) then
          call ch4kdis(ibn,np,k2-1,ch4exp,tch4,trant,irestrict)
       endif
!-----compute co2-minor transmittance using k-distribution method
       if (combnd) then
          call comkdis(ibn,np,k2-1,comexp,tcom,trant,irestrict)
       endif
!-----compute cfc11 transmittance using k-distribution method
       if (f11bnd) then
          call cfckdis(np,k2-1,f11exp,tf11,trant,irestrict)
       endif
!-----compute cfc12 transmittance using k-distribution method
       if (f12bnd) then
          call cfckdis(np,k2-1,f12exp,tf12,trant,irestrict)
       endif
!-----compute cfc22 transmittance using k-distribution method
       if (f22bnd) then
          call cfckdis(np,k2-1,f22exp,tf22,trant,irestrict)
       endif
!-----compute transmittance in band 10 using k-distribution method.
!     for band 10, trant is the change in transmittance due to n2o
!     absorption.
       if (b10bnd) then
          call b10kdis(np,k2-1,h2oexp,conexp,co2exp,n2oexp &
                      ,th2o,tcon,tco2,tn2o,trant,irestrict)
       endif
      endif
!*****   end trace gases  *****
!-----include aerosol effect
!dir$ vector aligned
        DO ic=1,irestrict
         tranal(ic)=tranal(ic)*taerlyr(ic,k2-1)
         trant(ic) =trant(ic) *tranal(ic)
        ENDDO
!***** cloud overlapping *****
      if (.not. overcast) then
!dir$ vector aligned
        DO ic=1,irestrict
        call cldovlp (np,k2,ict,icb,it,im,ib, &
                      cldhi,cldmd,cldlw,fcld,tcldlyr,fclr,irestrict)
       ENDDO
      else
!dir$ vector aligned
       DO ic=1,irestrict
        fclr(ic)=fclr(ic)*tcldlyr(ic,k2-1)
       ENDDO
      endif
!-----compute upward and downward fluxes (bands 1-9). it follows
!     eqs. (8.14) and (8.15). downward fluxes are positive.
      if (.not. b10bnd) then
!-----contribution from the "adjacent layer"
       if (k2 .eq. k1+1) then
!dir$ vector aligned
       DO ic=1,irestrict
         yy=min(0.999,trant(ic))
         yy=max(0.001,yy)
!-hmhj use log instead of alog for default intrinsic function
         xx=(blevel(ic,k1)-blevel(ic,k2))/ log(yy)
         bu=(blevel(ic,k1)-blevel(ic,k2)*yy)/(1.0-yy)+xx
         bd=(blevel(ic,k2)-blevel(ic,k1)*yy)/(1.0-yy)-xx
!                bu=blayer(ic,k1)
!                bd=blayer(ic,k1)
!-----for clear-sky situation
         flcu(ic,k1)=flcu(ic,k1)-bu+(bu-blayer(ic,k2))*trant(ic)
         flcd(ic,k2)=flcd(ic,k2)+bd-(bd-blayer(ic,k1-1))*trant(ic)
!-----for all-sky situation
         flxu(ic,k1)=flxu(ic,k1)-bu+(bu-blayer(ic,k2))*trant(ic)*fclr(ic)
         flxd(ic,k2)=flxd(ic,k2)+bd-(bd-blayer(ic,k1-1))*trant(ic)*fclr(ic)
       ENDDO
       else
!-----contribution from distant layers.
!dir$ vector aligned
       DO ic=1,irestrict
          xx=trant(ic)*dblayr(ic,k2)
          flcu(ic,k1) =flcu(ic,k1)+xx
          flxu(ic,k1) =flxu(ic,k1)+xx*fclr(ic)
          xx=trant(ic)*dblayr(ic,k1)
          flcd(ic,k2) =flcd(ic,k2)+xx
          flxd(ic,k2) =flxd(ic,k2)+xx*fclr(ic)
       ENDDO
       endif
      else
!-----flux reduction due to n2o in band 10 (eqs. 5.1 and 5.2)
!     trant is the transmittance change due to n2o absorption (eq. 5.3).
!dir$ vector aligned
       DO ic=1,irestrict
        rflx(ic,k1) = rflx(ic,k1)+trant(ic)*fclr(ic)*dblayr(ic,k2)
        rflx(ic,k2) = rflx(ic,k2)+trant(ic)*fclr(ic)*dblayr(ic,k1)
        rflc(ic,k1) = rflc(ic,k1)+trant(ic)*dblayr(ic,k2)
        rflc(ic,k2) = rflc(ic,k2)+trant(ic)*dblayr(ic,k1)
      ENDDO
      endif
 3000 continue
!-----here, fclr and trant are, respectively, the clear line-of-sight
!     and the transmittance between k1 and the surface.
!dir$ vector aligned
       DO ic=1,irestrict
         trantcr(ic,k1) =trant(ic)
         transfc(ic,k1) =trant(ic)*fclr(ic)
      ENDDO
!-----compute the partial derivative of fluxes with respect to
!     surface temperature (eq. 3.12). note: upward flux is negative.
!dir$ vector aligned
       DO ic=1,irestrict
         dfdts(ic,k1) =dfdts(ic,k1)-dbs(ic)*transfc(ic,k1)
       ENDDO
 2000 continue
      if (.not. b10bnd) then
!-----for surface emission.
!     note: blayer(np+1) and dbs include the surface emissivity effect.
!dir$ vector aligned
       DO ic=1,irestrict
          flcu(ic,np+1)=-blayer(ic,np+1)
          flxu(ic,np+1)=-blayer(ic,np+1)
          sfcem(ic)=sfcem(ic)-blayer(ic,np+1)
          dfdts(ic,np+1)=dfdts(ic,np+1)-dbs(ic)
       ENDDO
!-----add the flux reflected by the surface. (last term on the
!     rhs of eq. 3.10)
        do k=1,np+1
!dir$ vector aligned
       DO ic=1,irestrict
           flcu(ic,k)=flcu(ic,k)- &
                flcd(ic,np+1)*trantcr(ic,k)*(1.-emiss(ic,ibn))
           flxu(ic,k)=flxu(ic,k)- &
                flxd(ic,np+1)*transfc(ic,k)*(1.-emiss(ic,ibn))
       ENDDO
        enddo
      endif
!-----summation of fluxes over spectral bands
      do k=1,np+1
!dir$ vector aligned
       DO ic=1,irestrict
         flc(ic,k)=flc(ic,k)+flcd(ic,k)+flcu(ic,k)
         flx(ic,k)=flx(ic,k)+flxd(ic,k)+flxu(ic,k)
         acflxu(ic,k)=acflxu(ic,k)+flxu(ic,k)   ! (LW upward must hold negative values)
         acflxd(ic,k)=acflxd(ic,k)+flxd(ic,k)   ! (LW downward must hold postive values, and should=0 a
       ENDDO
      enddo

!-----adjustment due to n2o absorption in band 10. eqs. (5.4) and (5.5)
       if (b10bnd) then
        ! correction of n2o at TOA flux should be zero (brute force) toshii
!dir$ vector aligned
       DO ic=1,irestrict
        rflc(ic,1) = 0.e0
        rflx(ic,1) = 0.e0
       ENDDO
        do k=2,np+1  !correction (TOA Down Flux must be ZERO) (so ski k=1)
!dir$ vector aligned
       DO ic=1,irestrict
          flc(ic,k)=flc(ic,k)+rflc(ic,k)
          flx(ic,k)=flx(ic,k)+rflx(ic,k)
          if(rflx(ic,k).ge.0.0) acflxd(ic,k)=acflxd(ic,k)+rflx(ic,k)
          if(rflx(ic,k).lt.0.0) acflxu(ic,k)=acflxu(ic,k)+rflx(ic,k)
       ENDDO
        enddo
       endif
      endif                            ! endif (.not. b10bnd .or. trace)

 enddo BAND_LOOP

!
! output double to single precision
!
   flx_out    = REAL(flx)     ! broadband LW flux divergence [W/m2]
   acflxu_out = REAL(acflxu)  ! upwelling broadband LW flux  [W/m2]
   acflxd_out = REAL(acflxd)  ! downwelling broadband LW flux [W/m2]


  end subroutine lwrad

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine column (np,pa,dt,sabs0,sabs,spre,stem, irestrict)
!***********************************************************************
!-----compute column-integrated (from top of the model atmosphere)
!     absorber amount (sabs), absorber-weighted pressure (spre) and
!     temperature (stem).
!     computations follow eqs. (8.24) - (8.26).
!
!--- input parameters
!   number of atmospheric layers (np)
!   layer pressure (pa)
!   layer temperature minus 250k (dt)
!   layer absorber amount (sabs0)
!
!--- output parameters
!   column-integrated absorber amount (sabs)
!   column absorber-weighted pressure (spre)
!   column absorber-weighted temperature (stem)
!
!--- units of pa and dt are mb and k, respectively.
!    units of sabs are g/cm**2 for water vapor and (cm-atm)stp
!    for co2 and o3
!***********************************************************************
      implicit none
      integer np,k,irestrict,ic
!---- input parameters -----
      real(fp_kind) pa(CHUNK,np),dt(CHUNK,np),sabs0(CHUNK,np)
!---- output parameters -----
      real(fp_kind) sabs(CHUNK,np+1),spre(CHUNK,np+1),stem(CHUNK,np+1)
!*********************************************************************
!dir$ vector aligned
        DO ic=1,irestrict
          sabs(ic,1)=0.0
          spre(ic,1)=0.0
          stem(ic,1)=0.0
        ENDDO
        do k=1,np
!dir$ vector aligned
        DO ic=1,irestrict
           sabs(ic,k+1)=sabs(ic,k)+sabs0(ic,k)
           spre(ic,k+1)=spre(ic,k)+pa(ic,k)*sabs0(ic,k)
           stem(ic,k+1)=stem(ic,k)+dt(ic,k)*sabs0(ic,k)
        ENDDO
        enddo

       end subroutine column

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine h2oexps(ib,np,dh2o,pa,dt,xkw,aw,bw,pm,mw,h2oexp,irestrict)
!**********************************************************************
!   compute exponentials for water vapor line absorption
!   in individual layers using eqs. (8.18) and (8.19).
!
!---- input parameters
!  spectral band (ib)
!  number of layers (np)
!  layer water vapor amount for line absorption (dh2o)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!  absorption coefficients for the first k-distribution
!     function due to h2o line absorption (xkw)
!  coefficients for the temperature and pressure scaling (aw,bw,pm)
!  ratios between neighboring absorption coefficients for
!     h2o line absorption (mw)
!
!---- output parameters
!  6 exponentials for each layer  (h2oexp)
!**********************************************************************
      implicit none
      integer ib,np,k,ik,irestrict,ic
!---- input parameters ------
      real(fp_kind) dh2o(CHUNK,np),pa(CHUNK,np),dt(CHUNK,np)
!---- output parameters -----
      real(fp_kind) h2oexp(CHUNK,np,6)
!---- static data -----
      integer mw(9)
      real(fp_kind) xkw(9),aw(9),bw(9),pm(9)
!---- temporary arrays -----
      real(fp_kind) xh(CHUNK)
!**********************************************************************
!    note that the 3 sub-bands in band 3 use the same set of xkw, aw,
!    and bw,  therefore, h2oexp for these sub-bands are identical.
!**********************************************************************
        do k=1,np
!dir$ vector aligned
        DO ic=1,irestrict
!-----xh is the scaled water vapor amount for line absorption
!     computed from eq. (4.4).
           xh(ic) = dh2o(ic,k)*(pa(ic,k)/500.)**pm(ib) &
              * ( 1.+(aw(ib)+bw(ib)* dt(ic,k))*dt(ic,k) )
!-----h2oexp is the water vapor transmittance of the layer k
!     due to line absorption
           h2oexp(ic,k,1) = exp(-xh(ic)*xkw(ib))
        ENDDO
        enddo
!-----compute transmittances from eq. (8.19)
         if (mw(ib).eq.6) then
        do ik=2,6
          do k=1,np
!dir$ vector aligned
          DO ic=1,irestrict
             xh(ic) = h2oexp(ic,k,ik-1)*h2oexp(ic,k,ik-1)
             if(xh(ic).lt.1.e-4) xh(ic)=0.
             h2oexp(ic,k,ik) = xh(ic)*xh(ic)*xh(ic)
          ENDDO
          enddo
        enddo
        elseif (mw(ib).eq.8) then
        do ik=2,6
          do k=1,np
!dir$ vector aligned
          DO ic=1,irestrict
             xh(ic) = h2oexp(ic,k,ik-1)*h2oexp(ic,k,ik-1)
             if(xh(ic).lt.1.e-3) xh(ic)=0.
             xh(ic) = xh(ic)*xh(ic)
             h2oexp(ic,k,ik) = xh(ic)*xh(ic)
          ENDDO
          enddo
        enddo
        elseif (mw(ib).eq.9) then
        do ik=2,6
          do k=1,np
!dir$ vector aligned
          DO ic=1,irestrict
             xh(ic)=h2oexp(ic,k,ik-1)*h2oexp(ic,k,ik-1)*h2oexp(ic,k,ik-1)
             if(xh(ic).lt.1.e-4) xh(ic)=0.
             h2oexp(ic,k,ik) = xh(ic)*xh(ic)*xh(ic)
          enddo
        ENDDO
        enddo
        else
        do ik=2,6
          do k=1,np
!dir$ vector aligned
          DO ic=1,irestrict
             xh(ic) = h2oexp(ic,k,ik-1)*h2oexp(ic,k,ik-1)
             if(xh(ic).lt.2.e-2) xh(ic)=0.
             xh(ic) = xh(ic)*xh(ic)
             xh(ic) = xh(ic)*xh(ic)
             h2oexp(ic,k,ik) = xh(ic)*xh(ic)
          ENDDO
          enddo
        enddo
        endif

      end subroutine h2oexps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine conexps(ib,np,dcont,xke,conexp,irestrict)
!**********************************************************************
!   compute exponentials for continuum absorption in individual layers.
!
!---- input parameters
!  spectral band (ib)
!  number of layers (np)
!  layer scaled water vapor amount for continuum absorption (dcont)
!  absorption coefficients for the first k-distribution function
!     due to water vapor continuum absorption (xke)
!
!---- output parameters
!  1 or 3 exponentials for each layer (conexp)
!**********************************************************************
      implicit none
      integer ib,np,k,irestrict,ic
!---- input parameters ------
      real(fp_kind) dcont(CHUNK,np)
!---- updated parameters -----
      real(fp_kind) conexp(CHUNK,np,3)
!---- static data -----
      real(fp_kind) xke(9)
!****************************************************************
        do k=1,np
!dir$ vector aligned
        DO ic=1,irestrict
           conexp(ic,k,1) = exp(-dcont(ic,k)*xke(ib))
        ENDDO
        enddo
       if (ib .eq. 3) then
!-----the absorption coefficients for sub-bands 3b and 3a are, respectively,
!     two and four times the absorption coefficient for sub-band 3c (table 9).
!     note that conexp(k,3) is for sub-band 3a.
         do k=1,np
!dir$ vector aligned
         DO ic=1,irestrict
            conexp(ic,k,2) = conexp(ic,k,1) *conexp(ic,k,1)
            conexp(ic,k,3) = conexp(ic,k,2) *conexp(ic,k,2)
         ENDDO
         enddo
       endif

      end subroutine conexps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine co2exps(np,dco2,pa,dt,co2exp,irestrict)
!**********************************************************************
!   compute co2 exponentials for individual layers.
!
!---- input parameters
!  number of layers (np)
!  layer co2 amount (dco2)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  6 exponentials for each layer (co2exp)
!**********************************************************************
      implicit none
      integer np,k,irestrict,ic
!---- input parameters -----
      real(fp_kind) dco2(CHUNK,np),pa(CHUNK,np),dt(CHUNK,np)
!---- output parameters -----
      real(fp_kind) co2exp(CHUNK,np,6,2)
!---- temporary arrays -----
      real(fp_kind) xc
!**********************************************************************
        do k=1,np
!-----the scakubg oaraneters are given in table 3, and values of
!     the absorption coefficient are given in table 10.
!     scaled co2 amount for band-wings (sub-bands 3a and 3c)
!dir$ vector aligned
        DO ic=1,irestrict
           xc = dco2(ic,k)*(pa(ic,k)/300.0)**0.5 &
                   *(1.+(0.0182+1.07e-4*dt(ic,k))*dt(ic,k))
!-----six exponentials by powers of 8 (see eqs. 8.18, 8.19 and table 10).
           co2exp(ic,k,1,1)=exp(-xc*2.656e-5)
           xc=co2exp(ic,k,1,1)*co2exp(ic,k,1,1)
           xc=xc*xc
           co2exp(ic,k,2,1)=xc*xc
           xc=co2exp(ic,k,2,1)*co2exp(ic,k,2,1)
           xc=xc*xc
           co2exp(ic,k,3,1)=xc*xc
           xc=co2exp(ic,k,3,1)*co2exp(ic,k,3,1)
           xc=xc*xc
           co2exp(ic,k,4,1)=xc*xc
           xc=co2exp(ic,k,4,1)*co2exp(ic,k,4,1)
           xc=xc*xc
           co2exp(ic,k,5,1)=xc*xc
           xc=co2exp(ic,k,5,1)*co2exp(ic,k,5,1)
           xc=xc*xc
           co2exp(ic,k,6,1)=xc*xc
!-----for band-center region (sub-band 3b)
           xc = dco2(ic,k)*(pa(ic,k)/30.0)**0.85 &
                   *(1.+(0.0042+2.00e-5*dt(ic,k))*dt(ic,k))
           co2exp(ic,k,1,2)=exp(-xc*2.656e-3)
           xc=co2exp(ic,k,1,2)*co2exp(ic,k,1,2)
           xc=xc*xc
           co2exp(ic,k,2,2)=xc*xc
           xc=co2exp(ic,k,2,2)*co2exp(ic,k,2,2)
           xc=xc*xc
           co2exp(ic,k,3,2)=xc*xc
           xc=co2exp(ic,k,3,2)*co2exp(ic,k,3,2)
           xc=xc*xc
           co2exp(ic,k,4,2)=xc*xc
           xc=co2exp(ic,k,4,2)*co2exp(ic,k,4,2)
           xc=xc*xc
           co2exp(ic,k,5,2)=xc*xc
           xc=co2exp(ic,k,5,2)*co2exp(ic,k,5,2)
           xc=xc*xc
           co2exp(ic,k,6,2)=xc*xc
        ENDDO
        enddo

      end subroutine co2exps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine n2oexps(ib,np,dn2o,pa,dt,n2oexp,irestrict)
!**********************************************************************
!   compute n2o exponentials for individual layers
!
!---- input parameters
!  spectral band (ib)
!  number of layers (np)
!  layer n2o amount (dn2o)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  2 or 4 exponentials for each layer (n2oexp)
!**********************************************************************
      implicit none
      integer ib,np,k,irestrict,ic

!---- input parameters -----

      real(fp_kind) dn2o(CHUNK,np),pa(CHUNK,np),dt(CHUNK,np)

!---- output parameters -----

      real(fp_kind) n2oexp(CHUNK,np,4)

!---- temporary arrays -----

      real(fp_kind) xc,xc1,xc2

!-----scaling and absorpton data are given in table 5.
!     transmittances are computed using eqs. (8.18) and (8.19).

       do k=1,np
!dir$ vector aligned
       DO ic=1,irestrict

!-----four exponential by powers of 21 for band 6.

          if (ib.eq.6) then

           xc=dn2o(ic,k)*(1.+(1.9297e-3+4.3750e-6*dt(ic,k))*dt(ic,k))
           n2oexp(ic,k,1)=exp(-xc*6.31582e-2)

           xc=n2oexp(ic,k,1)*n2oexp(ic,k,1)*n2oexp(ic,k,1)
           xc1=xc*xc
           xc2=xc1*xc1
           n2oexp(ic,k,2)=xc*xc1*xc2

!-----four exponential by powers of 8 for band 7

          else

           xc=dn2o(ic,k)*(pa(ic,k)/500.0)**0.48 &
              *(1.+(1.3804e-3+7.4838e-6*dt(ic,k))*dt(ic,k))
           n2oexp(ic,k,1)=exp(-xc*5.35779e-2)

           xc=n2oexp(ic,k,1)*n2oexp(ic,k,1)
           xc=xc*xc
           n2oexp(ic,k,2)=xc*xc
           xc=n2oexp(ic,k,2)*n2oexp(ic,k,2)
           xc=xc*xc
           n2oexp(ic,k,3)=xc*xc
           xc=n2oexp(ic,k,3)*n2oexp(ic,k,3)
           xc=xc*xc
           n2oexp(ic,k,4)=xc*xc

          endif
       ENDDO
       enddo

      end subroutine n2oexps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine ch4exps(ib,np,dch4,pa,dt,ch4exp,irestrict)
!**********************************************************************
!   compute ch4 exponentials for individual layers
!
!---- input parameters
!  spectral band (ib)
!  number of layers (np)
!  layer ch4 amount (dch4)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  1 or 4 exponentials for each layer (ch4exp)
!**********************************************************************
      implicit none
      integer ib,np,k,irestrict,ic
!---- input parameters -----
      real(fp_kind) dch4(CHUNK,np),pa(CHUNK,np),dt(CHUNK,np)
!---- output parameters -----
      real(fp_kind) ch4exp(CHUNK,np,4)
!---- temporary arrays -----
      real(fp_kind) xc
!*****  scaling and absorpton data are given in table 5  *****
       do k=1,np
!dir$ vector aligned
       DO ic=1,irestrict

!-----four exponentials for band 6
          if (ib.eq.6) then
           xc=dch4(ic,k)*(1.+(1.7007e-2+1.5826e-4*dt(ic,k))*dt(ic,k))
           ch4exp(ic,k,1)=exp(-xc*5.80708e-3)
!-----four exponentials by powers of 12 for band 7
          else
           xc=dch4(ic,k)*(pa(ic,k)/500.0)**0.65 &
             *(1.+(5.9590e-4-2.2931e-6*dt(ic,k))*dt(ic,k))
           ch4exp(ic,k,1)=exp(-xc*6.29247e-2)
           xc=ch4exp(ic,k,1)*ch4exp(ic,k,1)*ch4exp(ic,k,1)
           xc=xc*xc
           ch4exp(ic,k,2)=xc*xc
           xc=ch4exp(ic,k,2)*ch4exp(ic,k,2)*ch4exp(ic,k,2)
           xc=xc*xc
           ch4exp(ic,k,3)=xc*xc
           xc=ch4exp(ic,k,3)*ch4exp(ic,k,3)*ch4exp(ic,k,3)
           xc=xc*xc
           ch4exp(ic,k,4)=xc*xc
          endif
       ENDDO
       enddo

      end subroutine ch4exps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine comexps(ib,np,dcom,dt,comexp,irestrict)
!**********************************************************************
!   compute co2-minor exponentials for individual layers using
!   eqs. (8.18) and (8.19).
!
!---- input parameters
!  spectral band (ib)
!  number of layers (np)
!  layer co2 amount (dcom)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  6 exponentials for each layer (comexp)
!**********************************************************************
      implicit none
      integer ib,np,k,ik,irestrict,ic
!---- input parameters -----
      real(fp_kind) dcom(CHUNK,np),dt(CHUNK,np)
!---- output parameters -----
      real(fp_kind) comexp(CHUNK,np,6)
!---- temporary arrays -----
      real(fp_kind) xc
!*****  scaling and absorpton data are given in table 6  *****
       do k=1,np
!dir$ vector aligned
       DO ic=1,irestrict
          if (ib.eq.4) then
           xc=dcom(ic,k)*(1.+(3.5775e-2+4.0447e-4*dt(ic,k))*dt(ic,k))
          endif
          if (ib.eq.5) then
           xc=dcom(ic,k)*(1.+(3.4268e-2+3.7401e-4*dt(ic,k))*dt(ic,k))
          endif
           comexp(ic,k,1)=exp(-xc*1.922e-7)
          do ik=2,6
           xc=comexp(ic,k,ik-1)*comexp(ic,k,ik-1)
           xc=xc*xc
           comexp(ic,k,ik)=xc*comexp(ic,k,ik-1)
          enddo
       ENDDO
       enddo

      end subroutine comexps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine cfcexps(ib,np,a1,b1,fk1,a2,b2,fk2,dcfc,dt,cfcexp,irestrict)
!**********************************************************************
!   compute cfc(-11, -12, -22) exponentials for individual layers.
!
!---- input parameters
!  spectral band (ib)
!  number of layers (np)
!  parameters for computing the scaled cfc amounts
!             for temperature scaling (a1,b1,a2,b2)
!  the absorption coefficients for the
!     first k-distribution function due to cfcs (fk1,fk2)
!  layer cfc amounts (dcfc)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  1 exponential for each layer (cfcexp)
!**********************************************************************
      implicit none
      integer ib,np,k,irestrict,ic
!---- input parameters -----
      real(fp_kind) dcfc(CHUNK,np),dt(CHUNK,np)
!---- output parameters -----
      real(fp_kind) cfcexp(CHUNK,np)
!---- static data -----
      real(fp_kind) a1,b1,fk1,a2,b2,fk2
!---- temporary arrays -----
      real(fp_kind) xf
!**********************************************************************
       do k=1,np
!-----compute the scaled cfc amount (xf) and exponential (cfcexp)
!dir$ vector aligned
       DO ic=1,irestrict
          if (ib.eq.4) then
           xf=dcfc(ic,k)*(1.+(a1+b1*dt(ic,k))*dt(ic,k))
           cfcexp(ic,k)=exp(-xf*fk1)
          else
           xf=dcfc(ic,k)*(1.+(a2+b2*dt(ic,k))*dt(ic,k))
           cfcexp(ic,k)=exp(-xf*fk2)
          endif
       ENDDO
       enddo

      end subroutine cfcexps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine b10exps(np,dh2o,dcont,dco2,dn2o,pa,dt &
                ,h2oexp,conexp,co2exp,n2oexp,irestrict)
!**********************************************************************
!   compute band3a exponentials for individual layers
!
!---- input parameters
!  number of layers (np)
!  layer h2o amount for line absorption (dh2o)
!  layer h2o amount for continuum absorption (dcont)
!  layer co2 amount (dco2)
!  layer n2o amount (dn2o)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!
!  exponentials for each layer (h2oexp,conexp,co2exp,n2oexp)
!**********************************************************************
      implicit none
      integer np,k,irestrict,ic
!---- input parameters -----
      real(fp_kind) dh2o(CHUNK,np),dcont(CHUNK,np),dn2o(CHUNK,np)
      real(fp_kind) dco2(CHUNK,np),pa(CHUNK,np),dt(CHUNK,np)
!---- output parameters -----
      real(fp_kind) h2oexp(CHUNK,np,6),conexp(CHUNK,np,3),co2exp(CHUNK,np,6,2) &
          ,n2oexp(CHUNK,np,4)
!---- temporary arrays -----
      real(fp_kind) xx,xx1,xx2,xx3
!**********************************************************************
        do k=1,np
!-----compute scaled h2o-line amount for band 10 (eq. 4.4 and table 3).
!dir$ vector aligned
       DO ic=1,irestrict
           xx=dh2o(ic,k)*(pa(ic,k)/500.0) &
                 *(1.+(0.0149+6.20e-5*dt(ic,k))*dt(ic,k))
!-----six exponentials by powers of 8
!     the constant 0.10624 is equal to 1.66*0.064
           h2oexp(ic,k,1)=exp(-xx*0.10624)
           xx=h2oexp(ic,k,1)*h2oexp(ic,k,1)
           xx=xx*xx
           h2oexp(ic,k,2)=xx*xx
           xx=h2oexp(ic,k,2)*h2oexp(ic,k,2)
           xx=xx*xx
           h2oexp(ic,k,3)=xx*xx
           xx=h2oexp(ic,k,3)*h2oexp(ic,k,3)
           xx=xx*xx
           h2oexp(ic,k,4)=xx*xx
           xx=h2oexp(ic,k,4)*h2oexp(ic,k,4)
           xx=xx*xx
           h2oexp(ic,k,5)=xx*xx
           xx=h2oexp(ic,k,5)*h2oexp(ic,k,5)
           xx=xx*xx
!          h2oexp(ic,k,6)=xx*xx
!-----compute scaled co2 amount for the band 10 (eq. 4.4 and table 6).
           xx=dco2(ic,k)*(pa(ic,k)/300.0)**0.5 &
                 *(1.+(0.0179+1.02e-4*dt(ic,k))*dt(ic,k))
!-----six exponentials by powers of 8
!     the constant 2.656e-5 is equal to 1.66*1.60e-5
           co2exp(ic,k,1,1)=exp(-xx*2.656e-5)
           xx=co2exp(ic,k,1,1)*co2exp(ic,k,1,1)
           xx=xx*xx
           co2exp(ic,k,2,1)=xx*xx
           xx=co2exp(ic,k,2,1)*co2exp(ic,k,2,1)
           xx=xx*xx
           co2exp(ic,k,3,1)=xx*xx
           xx=co2exp(ic,k,3,1)*co2exp(ic,k,3,1)
           xx=xx*xx
           co2exp(ic,k,4,1)=xx*xx
           xx=co2exp(ic,k,4,1)*co2exp(ic,k,4,1)
           xx=xx*xx
           co2exp(ic,k,5,1)=xx*xx
           xx=co2exp(ic,k,5,1)*co2exp(ic,k,5,1)
           xx=xx*xx
           co2exp(ic,k,6,1)=xx*xx
!-----one exponential of h2o continuum for sub-band 3a (table 9).
            conexp(ic,k,1)=exp(-dcont(ic,k)*1.04995e+2)
!-----compute the scaled n2o amount for band 10 (table 5).
           xx=dn2o(ic,k)*(1.+(1.4476e-3+3.6656e-6*dt(ic,k))*dt(ic,k))
!-----two exponentials by powers of 58
           n2oexp(ic,k,1)=exp(-xx*0.25238)
           xx=n2oexp(ic,k,1)*n2oexp(ic,k,1)
           xx1=xx*xx
           xx1=xx1*xx1
           xx2=xx1*xx1
           xx3=xx2*xx2
           n2oexp(ic,k,2)=xx*xx1*xx2*xx3
        ENDDO
        enddo

      end subroutine b10exps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine tablup(k1,k2,np,nx,nh,sabs,spre,stem,w1,p1, &
                        dwe,dpe,coef1,coef2,coef3,tran,irestrict)
!**********************************************************************
!   compute water vapor, co2 and o3 transmittances between level
!   k1 and and level k2 using table look-up.
!
!   calculations follow eq. (4.16).
!
!---- input ---------------------
!  indices for layer (k1) and level (k2)
!  number of atmospheric layers (np)
!  number of pressure intervals in the table (nx)
!  number of absorber amount intervals in the table (nh)
!  column-integrated absorber amount (sabs)           ! [g/cm2] or (cm-atm)stp
!  column absorber amount-weighted pressure (spre)    ![mb]
!  column absorber amount-weighted temperature (stem) ![K]
!
!  column-integrated h2o amoumt (sh2o), h2o-weighted pressure(swpre)
!  temperature (swtem). .
!
!  first value of absorber amount (log10) in the table (w1)
!  first value of pressure (log10) in the table (p1)
!  size of the interval of absorber amount (log10) in the table (dwe)
!  size of the interval of pressure (log10) in the table (dpe)
!  pre-computed coefficients (coef1, coef2, and coef3)
!
!---- updated ---------------------
!  transmittance (tran)
!
!  note:
!   (1) units of sabs are g/cm**2 for water vapor and
!       (cm-atm)stp for co2 and o3.
!   (2) units of spre and stem are, respectively, mb and k.
!
!**********************************************************************
      implicit none
!---- input parameters -----
      integer k1,k2,np,nx,nh,irestrict,ic
      real(fp_kind) w1,p1,dwe,dpe
      real(fp_kind) sabs(CHUNK,np+1),spre(CHUNK,np+1),stem(CHUNK,np+1)
      real(fp_kind) coef1(nx,nh),coef2(nx,nh),coef3(nx,nh)
!---- update parameter -----
      real(fp_kind) tran(CHUNK)
!---- temporary variables -----
      real(fp_kind) x1,x2,x3,we,pe,fw,fp,pa,pb,pc,ax,ba,bb,t1,ca,cb,t2
      integer iw,ip
!**********************************************************************
!dir$ vector aligned
      DO ic=1,irestrict
        x1=sabs(ic,k2)-sabs(ic,k1)
        we=(log10(x1)-w1)/dwe
       if (we .ge. (w1-2.)) then

        x2=(spre(ic,k2)-spre(ic,k1))/x1
        x3=(stem(ic,k2)-stem(ic,k1))/x1
!-----normalize we and pe
        pe=(log10(x2)-p1)/dpe
!-----restrict the magnitudes of the normalized we and pe.
        we=min(we,REAL(nh-1))
        pe=max(pe,0.0)
        pe=min(pe,REAL(nx-1))
!-----assign iw and ip and compute the distance of we and pe
!     from iw and ip.
        iw=int(we+1.0)
        iw=min(iw,nh-1)
        iw=max(iw, 2)
        fw=we-float(iw-1)

        ip=int(pe+1.0)
        ip=min(ip,nx-1)
        ip=max(ip, 1)
        fp=pe-float(ip-1)
!-----linear interpolation in pressure
        pa = coef1(ip,iw-1)*(1.-fp)+coef1(ip+1,iw-1)*fp
        pb = coef1(ip,  iw)*(1.-fp)+coef1(ip+1,  iw)*fp
        pc = coef1(ip,iw+1)*(1.-fp)+coef1(ip+1,iw+1)*fp

!-----quadratic interpolation in absorber amount for coef1
        ax = (-pa*(1.-fw)+pc*(1.+fw)) *fw*0.5 + pb*(1.-fw*fw)

!-----linear interpolation in absorber amount for coef2 and coef3
        ba = coef2(ip,  iw)*(1.-fp)+coef2(ip+1,  iw)*fp
        bb = coef2(ip,iw+1)*(1.-fp)+coef2(ip+1,iw+1)*fp
        t1 = ba*(1.-fw) + bb*fw

        ca = coef3(ip,  iw)*(1.-fp)+coef3(ip+1,  iw)*fp
        cb = coef3(ip,iw+1)*(1.-fp)+coef3(ip+1,iw+1)*fp
        t2 = ca*(1.-fw) + cb*fw
!-----update the total transmittance between levels k1 and k2
        tran(ic)= (ax + (t1+t2*x3) * x3)*tran(ic)
        tran(ic)=min(tran(ic),0.9999999)
        tran(ic)=max(tran(ic),0.0000001)
       else
        tran(ic)=0.9999999
       endif
      ENDDO

      end subroutine tablup

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine h2okdis(ib,np,k,fkw,gkw,ne,h2oexp,conexp, &
                         th2o,tcon,tran,irestrict)
!**********************************************************************
!   compute water vapor transmittance between levels k1 and k2
!   using the k-distribution method.
!
!---- input parameters
!  spectral band (ib)
!  number of levels (np)
!  current level (k)
!  planck-weighted k-distribution function due to
!    h2o line absorption (fkw)
!  planck-weighted k-distribution function due to
!    h2o continuum absorption (gkw)
!  number of terms used in each band to compute water vapor
!     continuum transmittance (ne)
!  exponentials for line absorption (h2oexp)
!  exponentials for continuum absorption (conexp)
!
!---- updated parameters
!  transmittance between levels k1 and k2 due to
!    water vapor line absorption (th2o)
!  transmittance between levels k1 and k2 due to
!    water vapor continuum absorption (tcon)
!  total transmittance (tran)
!
!**********************************************************************
      implicit none
      integer ib,np,k,irestrict,ic
!---- input parameters ------
      real(fp_kind) conexp(CHUNK,np,3),h2oexp(CHUNK,np,6)
      integer ne(9)
      real(fp_kind)  fkw(6,9),gkw(6,3)
!---- updated parameters -----
      real(fp_kind) th2o(CHUNK,6),tcon(CHUNK,3),tran(CHUNK)
!---- temporary arrays -----
      real(fp_kind) trnth2o
!-----tco2 are the six exp factors between levels k1 and k2
!     tran is the updated total transmittance between levels k1 and k2
!-----th2o is the 6 exp factors between levels k1 and k2 due to
!     h2o line absorption.
!-----tcon is the 3 exp factors between levels k1 and k2 due to
!     h2o continuum absorption.
!-----trnth2o is the total transmittance between levels k1 and k2 due
!     to both line and continuum absorption.
!-----comoute th2o following eq. (8.20).
!dir$ vector aligned
      DO ic=1,irestrict
           th2o(ic,1) = th2o(ic,1)*h2oexp(ic,k,1)
           th2o(ic,2) = th2o(ic,2)*h2oexp(ic,k,2)
           th2o(ic,3) = th2o(ic,3)*h2oexp(ic,k,3)
           th2o(ic,4) = th2o(ic,4)*h2oexp(ic,k,4)
           th2o(ic,5) = th2o(ic,5)*h2oexp(ic,k,5)
           th2o(ic,6) = th2o(ic,6)*h2oexp(ic,k,6)
      if (ne(ib).eq.0) then
!-----comoute trnh2o following eq. (8.22). fkw is given in table 4.
           trnth2o      =(fkw(1,ib)*th2o(ic,1) &
                        + fkw(2,ib)*th2o(ic,2) &
                        + fkw(3,ib)*th2o(ic,3) &
                        + fkw(4,ib)*th2o(ic,4) &
                        + fkw(5,ib)*th2o(ic,5) &
                        + fkw(6,ib)*th2o(ic,6))
          tran(ic)=tran(ic)*trnth2o
      elseif (ne(ib).eq.1) then
!-----comoute trnh2o following eq. (8.22) and (4.27).
           tcon(ic,1)= tcon(ic,1)*conexp(ic,k,1)
           trnth2o      =(fkw(1,ib)*th2o(ic,1) &
                        + fkw(2,ib)*th2o(ic,2) &
                        + fkw(3,ib)*th2o(ic,3) &
                        + fkw(4,ib)*th2o(ic,4) &
                        + fkw(5,ib)*th2o(ic,5) &
                        + fkw(6,ib)*th2o(ic,6))*tcon(ic,1)
          tran(ic)=tran(ic)*trnth2o
      else
!-----for band 3. this band is divided into 3 subbands.
           tcon(ic,1)= tcon(ic,1)*conexp(ic,k,1)
           tcon(ic,2)= tcon(ic,2)*conexp(ic,k,2)
           tcon(ic,3)= tcon(ic,3)*conexp(ic,k,3)
!-----comoute trnh2o following eq. (4.29).
           trnth2o      = (  gkw(1,1)*th2o(ic,1) &
                           + gkw(2,1)*th2o(ic,2) &
                           + gkw(3,1)*th2o(ic,3) &
                           + gkw(4,1)*th2o(ic,4) &
                           + gkw(5,1)*th2o(ic,5) &
                           + gkw(6,1)*th2o(ic,6) ) * tcon(ic,1) &
                        + (  gkw(1,2)*th2o(ic,1) &
                           + gkw(2,2)*th2o(ic,2) &
                           + gkw(3,2)*th2o(ic,3) &
                           + gkw(4,2)*th2o(ic,4) &
                           + gkw(5,2)*th2o(ic,5) &
                           + gkw(6,2)*th2o(ic,6) ) * tcon(ic,2) &
                        + (  gkw(1,3)*th2o(ic,1) &
                           + gkw(2,3)*th2o(ic,2) &
                           + gkw(3,3)*th2o(ic,3) &
                           + gkw(4,3)*th2o(ic,4) &
                           + gkw(5,3)*th2o(ic,5) &
                           + gkw(6,3)*th2o(ic,6) ) * tcon(ic,3)
          tran(ic)=tran(ic)*trnth2o
      endif
      ENDDO

      end subroutine h2okdis

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine co2kdis(np,k,co2exp,tco2,tran,irestrict)
!**********************************************************************
!   compute co2 transmittances between levels k1 and k2 
!    using the k-distribution method with linear
!    pressure scaling.
!
!---- input parameters
!   number of levels (np)
!   current level (k)
!   exponentials for co2 absorption (co2exp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to co2 absorption
!     for the various values of the absorption coefficient (tco2)
!   total transmittance (tran)
!
!**********************************************************************
      implicit none
      integer np,k,irestrict,ic
!---- input parameters -----
      real(fp_kind) co2exp(CHUNK,np,6,2)
!---- updated parameters -----
      real(fp_kind) tco2(CHUNK,6,2),tran(CHUNK)
!---- temporary arrays -----
      real(fp_kind) xc
!-----tco2 is the 6 exp factors between levels k1 and k2.
!     xc is the total co2 transmittance given by eq. (4.30).
!     the k-distribution functions are given in table 10.
!-----band-wings
!dir$ vector aligned
      DO ic=1,irestrict
           tco2(ic,1,1)=tco2(ic,1,1)*co2exp(ic,k,1,1)
           xc=   0.1395 *tco2(ic,1,1)
           tco2(ic,2,1)=tco2(ic,2,1)*co2exp(ic,k,2,1)
           xc=xc+0.1407 *tco2(ic,2,1)
           tco2(ic,3,1)=tco2(ic,3,1)*co2exp(ic,k,3,1)
           xc=xc+0.1549 *tco2(ic,3,1)
           tco2(ic,4,1)=tco2(ic,4,1)*co2exp(ic,k,4,1)
           xc=xc+0.1357 *tco2(ic,4,1)
           tco2(ic,5,1)=tco2(ic,5,1)*co2exp(ic,k,5,1)
           xc=xc+0.0182 *tco2(ic,5,1)
           tco2(ic,6,1)=tco2(ic,6,1)*co2exp(ic,k,6,1)
           xc=xc+0.0220 *tco2(ic,6,1)
!-----band-center region
           tco2(ic,1,2)=tco2(ic,1,2)*co2exp(ic,k,1,2)
           xc=xc+0.0766 *tco2(ic,1,2)
           tco2(ic,2,2)=tco2(ic,2,2)*co2exp(ic,k,2,2)
           xc=xc+0.1372 *tco2(ic,2,2)
           tco2(ic,3,2)=tco2(ic,3,2)*co2exp(ic,k,3,2)
           xc=xc+0.1189 *tco2(ic,3,2)
           tco2(ic,4,2)=tco2(ic,4,2)*co2exp(ic,k,4,2)
           xc=xc+0.0335 *tco2(ic,4,2)
           tco2(ic,5,2)=tco2(ic,5,2)*co2exp(ic,k,5,2)
           xc=xc+0.0169 *tco2(ic,5,2)
           tco2(ic,6,2)=tco2(ic,6,2)*co2exp(ic,k,6,2)
           xc=xc+0.0059 *tco2(ic,6,2)
           tran(ic)=tran(ic)*xc
      ENDDO

      end subroutine co2kdis

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine n2okdis(ib,np,k,n2oexp,tn2o,tran,irestrict)
!**********************************************************************
!   compute n2o transmittances between levels k1 and k2
!    using the k-distribution method with linear
!    pressure scaling.
!
!---- input parameters
!   spectral band (ib)
!   number of levels (np)
!   current level (k)
!   exponentials for n2o absorption (n2oexp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to n2o absorption
!     for the various values of the absorption coefficient (tn2o)
!   total transmittance (tran)
!
!**********************************************************************
      implicit none
      integer ib,np,k,irestrict,ic
!---- input parameters -----
      real(fp_kind) n2oexp(CHUNK,np,4)
!---- updated parameters -----
      real(fp_kind) tn2o(CHUNK,4),tran(CHUNK)
!---- temporary arrays -----
      real(fp_kind) xc
!-----tn2o is computed from eq. (8.20).
!     xc is the total n2o transmittance computed from (8.22)
!     the k-distribution functions are given in table 5.
!-----band 6
!dir$ vector aligned
      DO ic=1,irestrict
          if (ib.eq.6) then
           tn2o(ic,1)=tn2o(ic,1)*n2oexp(ic,k,1)
           xc=   0.940414*tn2o(ic,1)
           tn2o(ic,2)=tn2o(ic,2)*n2oexp(ic,k,2)
           xc=xc+0.059586*tn2o(ic,2)
!-----band 7
          else
           tn2o(ic,1)=tn2o(ic,1)*n2oexp(ic,k,1)
           xc=   0.561961*tn2o(ic,1)
           tn2o(ic,2)=tn2o(ic,2)*n2oexp(ic,k,2)
           xc=xc+0.138707*tn2o(ic,2)
           tn2o(ic,3)=tn2o(ic,3)*n2oexp(ic,k,3)
           xc=xc+0.240670*tn2o(ic,3)
           tn2o(ic,4)=tn2o(ic,4)*n2oexp(ic,k,4)
           xc=xc+0.058662*tn2o(ic,4)
          endif
           tran(ic)=tran(ic)*xc
      ENDDO

      end subroutine n2okdis

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine ch4kdis(ib,np,k,ch4exp,tch4,tran,irestrict)
!**********************************************************************
!   compute ch4 transmittances between levels k1 and k2
!    using the k-distribution method with
!    linear pressure scaling.
!
!---- input parameters
!   spectral band (ib)
!   number of levels (np)
!   current level (k)
!   exponentials for ch4 absorption (ch4exp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to ch4 absorption
!     for the various values of the absorption coefficient (tch4)
!   total transmittance (tran)
!
!**********************************************************************
      implicit none
      integer ib,np,k,irestrict,ic
!---- input parameters -----
      real(fp_kind) ch4exp(CHUNK,np,4)
!---- updated parameters -----
      real(fp_kind) tch4(CHUNK,4),tran(CHUNK)
!---- temporary arrays -----
      real(fp_kind) xc
!-----tch4 is computed from eq. (8.20).
!     xc is the total ch4 transmittance computed from (8.22)
!     the k-distribution functions are given in table 5.
!-----band 6
!dir$ vector aligned
      DO ic=1,irestrict
          if (ib.eq.6) then
           tch4(ic,1)=tch4(ic,1)*ch4exp(ic,k,1)
           xc= tch4(ic,1)
!-----band 7
          else
           tch4(ic,1)=tch4(ic,1)*ch4exp(ic,k,1)
           xc=   0.610650*tch4(ic,1)
           tch4(ic,2)=tch4(ic,2)*ch4exp(ic,k,2)
           xc=xc+0.280212*tch4(ic,2)
           tch4(ic,3)=tch4(ic,3)*ch4exp(ic,k,3)
           xc=xc+0.107349*tch4(ic,3)
           tch4(ic,4)=tch4(ic,4)*ch4exp(ic,k,4)
           xc=xc+0.001789*tch4(ic,4)
          endif
           tran(ic)=tran(ic)*xc
      ENDDO

      end subroutine ch4kdis

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine comkdis(ib,np,k,comexp,tcom,tran,irestrict)
!**********************************************************************
!  compute co2-minor transmittances between levels k1 and k2
!   using the k-distribution method
!   with linear pressure scaling.
!
!---- input parameters
!   spectral band (ib)
!   number of levels (np)
!   current level (k)
!   exponentials for co2-minor absorption (comexp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to co2-minor absorption
!     for the various values of the absorption coefficient (tcom)
!   total transmittance (tran)
!
!**********************************************************************
      implicit none
      integer ib,np,k,irestrict,ic
!---- input parameters -----
      real(fp_kind) comexp(CHUNK,np,6)
!---- updated parameters -----
      real(fp_kind) tcom(CHUNK,6),tran(CHUNK)
!---- temporary arrays -----
      real(fp_kind) xc
!-----tcom is computed from eq. (8.20).
!     xc is the total co2 transmittance computed from (8.22)
!     the k-distribution functions are given in table 6.
!-----band 4
!dir$ vector aligned
      DO ic=1,irestrict
           if (ib.eq.4) then
            tcom(ic,1)=tcom(ic,1)*comexp(ic,k,1)
            xc=   0.12159*tcom(ic,1)
            tcom(ic,2)=tcom(ic,2)*comexp(ic,k,2)
            xc=xc+0.24359*tcom(ic,2)
            tcom(ic,3)=tcom(ic,3)*comexp(ic,k,3)
            xc=xc+0.24981*tcom(ic,3)
            tcom(ic,4)=tcom(ic,4)*comexp(ic,k,4)
            xc=xc+0.26427*tcom(ic,4)
            tcom(ic,5)=tcom(ic,5)*comexp(ic,k,5)
            xc=xc+0.07807*tcom(ic,5)
            tcom(ic,6)=tcom(ic,6)*comexp(ic,k,6)
            xc=xc+0.04267*tcom(ic,6)
!-----band 5
           else
            tcom(ic,1)=tcom(ic,1)*comexp(ic,k,1)
            xc=   0.06869*tcom(ic,1)
            tcom(ic,2)=tcom(ic,2)*comexp(ic,k,2)
            xc=xc+0.14795*tcom(ic,2)
            tcom(ic,3)=tcom(ic,3)*comexp(ic,k,3)
            xc=xc+   0.19512*tcom(ic,3)
            tcom(ic,4)=tcom(ic,4)*comexp(ic,k,4)
            xc=xc+   0.33446*tcom(ic,4)
            tcom(ic,5)=tcom(ic,5)*comexp(ic,k,5)
            xc=xc+   0.17199*tcom(ic,5)
            tcom(ic,6)=tcom(ic,6)*comexp(ic,k,6)
            xc=xc+   0.08179*tcom(ic,6)
           endif
            tran(ic)=tran(ic)*xc
      ENDDO

      end subroutine comkdis

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine cfckdis(np,k,cfcexp,tcfc,tran,irestrict)
!**********************************************************************
!  compute cfc-(11,12,22) transmittances between levels k1 and k2
!   using the k-distribution method with
!   linear pressure scaling.
!
!---- input parameters
!   number of levels (np)
!   current level (k)
!   exponentials for cfc absorption (cfcexp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to cfc absorption
!     for the various values of the absorption coefficient (tcfc)
!   total transmittance (tran)
!
!**********************************************************************
      implicit none
      integer np,k,irestrict,ic
!---- input parameters -----
      real(fp_kind) cfcexp(CHUNK,np)
!---- updated parameters -----
      real(fp_kind) tcfc(CHUNK),tran(CHUNK)
!-----tcfc is the exp factors between levels k1 and k2.
!dir$ vector aligned
      DO ic=1,irestrict
            tcfc(ic)=tcfc(ic)*cfcexp(ic,k)
            tran(ic)=tran(ic)*tcfc(ic)
      ENDDO

      end subroutine cfckdis

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine b10kdis(np,k,h2oexp,conexp,co2exp,n2oexp &
                ,th2o,tcon,tco2,tn2o,tran,irestrict)
!**********************************************************************
!
!   compute h2o (line and continuum),co2,n2o transmittances between
!   levels k1 and k2 using the k-distribution
!   method with linear pressure scaling.
!
!---- input parameters
!   number of levels (np)
!   current level (k)
!   exponentials for h2o line absorption (h2oexp)
!   exponentials for h2o continuum absorption (conexp)
!   exponentials for co2 absorption (co2exp)
!   exponentials for n2o absorption (n2oexp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to h2o line absorption
!     for the various values of the absorption coefficient (th2o)
!   transmittance between levels k1 and k2 due to h2o continuum
!     absorption for the various values of the absorption
!     coefficient (tcon)
!   transmittance between levels k1 and k2 due to co2 absorption
!     for the various values of the absorption coefficient (tco2)
!   transmittance between levels k1 and k2 due to n2o absorption
!     for the various values of the absorption coefficient (tn2o)
!   total transmittance (tran)
!
!**********************************************************************
      implicit none
      integer np,k,irestrict,ic
!---- input parameters -----
      real(fp_kind) h2oexp(CHUNK,np,6),conexp(CHUNK,np,3),co2exp(CHUNK,np,6,2) &
          ,n2oexp(CHUNK,np,4)
!---- updated parameters -----
      real(fp_kind) th2o(CHUNK,6),tcon(CHUNK,3),tco2(CHUNK,6,2),tn2o(CHUNK,4) &
          ,tran(CHUNK)
!---- temporary arrays -----
      real(fp_kind) xx
!-----for h2o line. the k-distribution functions are given in table 4.
!dir$ vector aligned
      DO ic=1,irestrict
           th2o(ic,1)=th2o(ic,1)*h2oexp(ic,k,1)
           xx=   0.3153*th2o(ic,1)
           th2o(ic,2)=th2o(ic,2)*h2oexp(ic,k,2)
           xx=xx+0.4604*th2o(ic,2)
           th2o(ic,3)=th2o(ic,3)*h2oexp(ic,k,3)
           xx=xx+0.1326*th2o(ic,3)
           th2o(ic,4)=th2o(ic,4)*h2oexp(ic,k,4)
           xx=xx+0.0798*th2o(ic,4)
           th2o(ic,5)=th2o(ic,5)*h2oexp(ic,k,5)
           xx=xx+0.0119*th2o(ic,5)
           tran(ic)=xx
!-----for h2o continuum. note that conexp(k,3) is for subband 3a.
           tcon(ic,1)=tcon(ic,1)*conexp(ic,k,1)
           tran(ic)=tran(ic)*tcon(ic,1)
!-----for co2 (table 6)
           tco2(ic,1,1)=tco2(ic,1,1)*co2exp(ic,k,1,1)
           xx=    0.2673*tco2(ic,1,1)
           tco2(ic,2,1)=tco2(ic,2,1)*co2exp(ic,k,2,1)
           xx=xx+ 0.2201*tco2(ic,2,1)
           tco2(ic,3,1)=tco2(ic,3,1)*co2exp(ic,k,3,1)
           xx=xx+ 0.2106*tco2(ic,3,1)
           tco2(ic,4,1)=tco2(ic,4,1)*co2exp(ic,k,4,1)
           xx=xx+ 0.2409*tco2(ic,4,1)
           tco2(ic,5,1)=tco2(ic,5,1)*co2exp(ic,k,5,1)
           xx=xx+ 0.0196*tco2(ic,5,1)
           tco2(ic,6,1)=tco2(ic,6,1)*co2exp(ic,k,6,1)
           xx=xx+ 0.0415*tco2(ic,6,1)
           tran(ic)=tran(ic)*xx
!-----for n2o (table 5)
           tn2o(ic,1)=tn2o(ic,1)*n2oexp(ic,k,1)
           xx=   0.970831*tn2o(ic,1)
           tn2o(ic,2)=tn2o(ic,2)*n2oexp(ic,k,2)
           xx=xx+0.029169*tn2o(ic,2)
           tran(ic)=tran(ic)*(xx-1.0)
      ENDDO

      end subroutine b10kdis

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine cldovlp (np,k2,ict,icb,it,im,ib, &
                     cldhi,cldmd,cldlw,fcld,tcldlyr,fclr,irestrict)
!***********************************************************************
!     compute the fractional clear line-of-sight between levels k1
!     and k2
!
! input parameters
!
!  np:      number of layers
!  k2:      index for the level
!  ict:     the level separating high and middle clouds
!  icb:     the level separating middle and low clouds
!  it:      number of cloudy layers in the high-cloud group
!  im:      number of cloudy layers in the middle-cloud group
!  ib:      number of cloudy layers in the low-cloud group
!  fcld:    fractional cloud cover of a layer
!  tcldlyr: transmittance of a cloud layer
!
! output parameter
!
!  fclr:    clear line-of-sight between levels k1 and k2
!***********************************************************************
      implicit none
      integer np,k2,ict(CHUNK),icb(CHUNK),irestrict,ic
      integer j,k,ii,it,im,ib,itx(CHUNK,np),imx(CHUNK,np),ibx(CHUNK,np)
      real(fp_kind) cldhi(CHUNK),cldmd(CHUNK),cldlw(CHUNK)
      real(fp_kind) fcld(CHUNK,np),tcldlyr(CHUNK,np),fclr(CHUNK)
!***********************************************************************
!-----for high clouds
!     "it" is the number of high-cloud layers
!dir$ vector aligned
      DO ic=1,irestrict
        if (k2.le.ict(ic)) then
         if(fcld(ic,k2-1).gt.fcld_min) then
          it=it+1
          ii=it
          itx(ic,ii)=k2-1
         if (ii .eq. 1) go to 11
!-----rearrange the order of cloud layers with increasing cloud amount
         do k=1,ii-1
           j=itx(ic,k)
          if(fcld(ic,j).gt.fcld(ic,k2-1)) then
           do j=ii-1,k,-1
            itx(ic,j+1)=itx(ic,j)
           enddo
            itx(ic,k)=k2-1
            go to 11
          endif
         enddo
   11   continue
!-----compute equivalent black-body high cloud amount
           cldhi(ic)=0.0
          do k=1,ii
           j=itx(ic,k)
           cldhi(ic)=fcld(ic,j)-tcldlyr(ic,j)*(fcld(ic,j)-cldhi(ic))
          enddo
        endif
       endif
!-----for middle clouds
!     "im" is the number of middle-cloud layers
       if (k2.gt.ict(ic) .and. k2.le.icb(ic)) then
        if(fcld(ic,k2-1).gt.fcld_min) then
         im=im+1
         ii=im
         imx(ic,ii)=k2-1
        if (ii .eq. 1) go to 21
!-----rearrange the order of cloud layers with increasing cloud amount
         do k=1,ii-1
            j=imx(ic,k)
           if(fcld(ic,j).gt.fcld(ic,k2-1)) then
            do j=ii-1,k,-1
             imx(ic,j+1)=imx(ic,j)
            enddo
             imx(ic,k)=k2-1
             go to 21
           endif
          enddo
   21   continue
!-----compute equivalent black-body middle cloud amount
           cldmd(ic)=0.0
          do k=1,ii
           j=imx(ic,k)
           cldmd(ic)=fcld(ic,j)-tcldlyr(ic,j)*(fcld(ic,j)-cldmd(ic))
          enddo
        endif
       endif
!-----for low clouds
!     "ib" is the number of low-cloud layers
       if (k2.gt.icb(ic)) then
        if(fcld(ic,k2-1).gt.fcld_min) then
         ib=ib+1
         ii=ib
         ibx(ic,ii)=k2-1
        if (ii .eq. 1) go to 31
!-----rearrange the order of cloud layers with increasing cloud amount
         do k=1,ii-1
          j=ibx(ic,k)
           if(fcld(ic,j).gt.fcld(ic,k2-1)) then
            do j=ii-1,k,-1
             ibx(ic,j+1)=ibx(ic,j)
            enddo
             ibx(ic,k)=k2-1
             go to 31
           endif
          enddo
   31    continue
!-----compute equivalent black-body low cloud amount
           cldlw(ic)=0.0
          do k=1,ii
           j=ibx(ic,k)
           cldlw(ic)=fcld(ic,j)-tcldlyr(ic,j)*(fcld(ic,j)-cldlw(ic))
          enddo
        endif
       endif
!-----fclr is the equivalent clear fraction between levels k1 and k2
!     assuming the three cloud groups are randomly overlapped.
!     it follows eqs. (10) and (12).
        fclr(ic)=(1.0-cldhi(ic))*(1.0-cldmd(ic))*(1.0-cldlw(ic))
      ENDDO

      end subroutine cldovlp

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine opt_cloud( sw_or_lw, np ,nband, dz, q, re, tau, ssa, asy, irestrict )
 implicit none
!------------------------------------------------------------------------
! History;
! 02/2014  Toshi Matsui ; Initial.
!------------------------------------------------------------------------

!-----IO variables
 character(len=2), intent(in) :: sw_or_lw   ! character index that determines sw or lw radiation
 integer, intent(in) :: np             !model verical array size
 integer, intent(in) :: nband          !# of radiation bands
 real, intent(in)    :: dz(CHUNK,np)         !layer thickness [m]
 real, intent(in)    :: q(CHUNK,np ,id_hmax) !hydrometeor mass conc [g/m3]
 real, intent(inout)    :: re(CHUNK,np,id_hmax) !effective radius [um]
 real, intent(out)   :: tau(CHUNK,np,nband)  !total optical depth [-]
 real, intent(out)   :: ssa(CHUNK,np,nband)  !total single scattering albedo [-]
 real, intent(out)   :: asy(CHUNK,np,nband)  !total asymetry factor [-]
 integer, intent(in) :: irestrict 

!-----Local variables
! real :: ext           !mass extinction coef [m2/g]
 real :: tau_typ(CHUNK,nband,id_hmax) !optical depth for each aerosol type [-]
 real :: ssa_typ(CHUNK,nband,id_hmax) !single scattering albedo for each aerosol type [-]
 real :: asy_typ(CHUNK,nband,id_hmax) !asymetery factor for each aerosol type [-]
 real :: wgt1(CHUNK), wgt2(CHUNK)        !weight for pressure interpolation
 integer :: t,k,n,ib,ire,ic
 real :: ref(CHUNK)
 logical :: exit_re_loop(CHUNK)

!
! check all single scattering tables are allocated and initialized
!
 if( .not. allocated(kext_sw_cloud) ) then
    call wrf_error_fatal('MSG opt_cloud in module_ra_goddard_2014.F  you must call read_lut first.')
 endif

!
! intialize
!
!dir$ vector aligned
 DO ic=1,irestrict
   tau(ic,:,:) = 0.e0 ; ssa(ic,:,:) = 0.e0 ; asy(ic,:,:) = 0.e0
 ENDDO

!
! Compute total aerosol tau, ssa, & asy
! Algorithm: Total optical properties is computed as
! TAU = (tau1 + tau2 + tau3 +.... )
! SSA = (tau1*w1 + tau2*w2 + tau3*w2 + .... ) / TAU
! ASY = (tau1*w1*g1 + tau2*w2*g1 + tau3*w2*g1 + .... ) / (TAU*SSA)
!
 vertical_loop: do k = 1, np !vertical loop
!dir$ vector aligned
       DO ic=1,irestrict
         tau_typ(ic,:,:) = 0. ; ssa_typ(ic,:,:) = 0. ; asy_typ(ic,:,:) = 0.  !initialization
       ENDDO
       type_loop: do t = 1,id_hmax !hydrometero type loop
       DO ic=1,irestrict
          ref(ic) = re(ic,k,t)  !effective radius [um]
          if(re(ic,k,t) < pts_re_visir(1)             ) ref(ic) = pts_re_visir(1)
          if(re(ic,k,t) > pts_re_visir(mxpts_re_visir)) ref(ic) = pts_re_visir(mxpts_re_visir)
       ENDDO

          !
          ! Linear interpolation
          !
          exit_re_loop(1:irestrict) = .false.

          re_loop: do ire = 1,mxpts_re_visir-1

          if(ALL(exit_re_loop(1:irestrict))) exit re_loop

!dir$ SIMD
!dir$ vector aligned
          DO ic=1,irestrict
            if( .not. exit_re_loop(ic) .and. ref(ic) >= pts_re_visir(ire) .and. ref(ic) <= pts_re_visir(ire+1) ) then
                wgt2(ic) =  ( ref(ic) - pts_re_visir(ire) ) / ( pts_re_visir(ire+1) - pts_re_visir(ire) )
                wgt1(ic) = 1.0 - wgt2(ic)

              rad_select: select case(sw_or_lw)
              case ('sw')  !shortwave radiation
                tau_typ(ic,1:nband,t) = (wgt1(ic)*kext_sw_cloud(1:nband,ire,t) + wgt2(ic)*kext_sw_cloud(1:nband,ire+1,t)) &
                              * q(ic,k,t) * dz(ic,k) * 1.e-3  !optical depth [-]
                ssa_typ(ic,1:nband,t) =  wgt1(ic)*salb_sw_cloud(1:nband,ire,t) + wgt2(ic)*salb_sw_cloud(1:nband,ire+1,t)
                asy_typ(ic,1:nband,t) =  wgt1(ic)*asym_sw_cloud(1:nband,ire,t) + wgt2(ic)*asym_sw_cloud(1:nband,ire+1,t)

              case ('lw')  !longwave radiation
                tau_typ(ic,1:nband,t) = (wgt1(ic)*kext_lw_cloud(1:nband,ire,t) + wgt2(ic)*kext_lw_cloud(1:nband,ire+1,t)) &
                              * q(ic,k,t) * dz(ic,k) * 1.e-3  !optical depth [-]
                ssa_typ(ic,1:nband,t) =  wgt1(ic)*salb_lw_cloud(1:nband,ire,t) + wgt2(ic)*salb_lw_cloud(1:nband,ire+1,t)
                asy_typ(ic,1:nband,t) =  wgt1(ic)*asym_lw_cloud(1:nband,ire,t) + wgt2(ic)*asym_lw_cloud(1:nband,ire+1,t)

              case default
!                stop 'MSG opt_cloud: the option does not exist: sw_or_lw '
                call wrf_error_fatal('MSG opt_cloud: the option does not exist: sw_or_lw ')
              end select rad_select

              exit_re_loop(ic) = .true.
            endif
          ENDDO

          enddo re_loop

       enddo type_loop

    !
    ! compute total optical depth single scatterling albedo, asymetry parameters
    !
    band_loop: do n = 1,nband  !wavelengh loop
!dir$ vector aligned
    DO ic=1,irestrict
       tau(ic,k,n) = sum( tau_typ(ic,n,1:id_hmax) )
       ssa(ic,k,n) = sum( tau_typ(ic,n,1:id_hmax)*ssa_typ(ic,n,1:id_hmax) ) / max(tau(ic,k,n),1e-08)
       asy(ic,k,n) = sum( tau_typ(ic,n,1:id_hmax)*ssa_typ(ic,n,1:id_hmax)*asy_typ(ic,n,1:id_hmax) ) &
                       /  max(tau(ic,k,n)*ssa(ic,k,n),1e-08)
    ENDDO
    enddo band_loop

 enddo vertical_loop

 end subroutine opt_cloud

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine check_reff(spc,q,re,i,j,k)
#ifndef NO_IEEE_MODULE
 use, intrinsic :: ieee_arithmetic
#endif
 implicit none
 character(len=*) :: spc  !species 
 real,intent(inout) :: q !mass concentration [g/m2]
 real,intent(inout) :: re !effective radius [g/m2]
 integer,intent(in) :: i,j,k !array index


#ifndef NO_IEEE_MODULE
!
! Check NaN
!
 if( ieee_is_nan(re) ) then
  print*,'MSG check_reff: Warning '//trim(spc)//' re is NaN at (i,j,k)=',i,j,k
  re = 0.e0
 endif
#endif
!
! Check  too large (100m)
!
 if( re > 1.e8  ) then
  print*,'MSG check_reff: Warning '//trim(spc)//' re is too large (>100m) at (i,j,k)=',i,j,k
  re = 0.e0
 endif

!
! check negative re
!
 if( re < 0.e0 ) then
  print*,'MSG check_reff: Warning '//trim(spc)//' re is negative at (i,j,k)=',i,j,k
  re = 0.e0
 endif

!
! check inconsistency
!
 if( q > 0.e0 .and. re == 0.e0 ) then
!  print*,'MSG check_reff: Warning '//trim(spc)//' q is ',q,', but re is ',re
  re = 0.e0 ; q = 0.e0
 endif

 end subroutine check_reff

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 real function ave(var2d)
 !averaging
 implicit none
 real,intent(in) :: var2d(:,:)

 ave = sum( var2d(:,:) ) / real( size(var2d(:,:) ) )

 end function ave

 real function avew(var2d,wgt2d)
 !averaging
 implicit none
 real,intent(in) :: var2d(:,:)
 real,intent(in) :: wgt2d(:,:)

 if( sum(wgt2d(:,:)) == 0.e0 ) then
  avew = 0.e0
 else
  avew = sum( var2d(:,:)*wgt2d(:,:) ) / sum(wgt2d(:,:)) 
 endif

 end function avew


!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 end module module_ra_goddard

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD


