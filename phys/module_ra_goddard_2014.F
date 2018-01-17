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
! init_goddardrad_2014( initialize LUT )
!     !-- read_sslut (real single-scattering talbes )
!            |--open_lut_broad_gen (open LUT for hydrometeors)
!                 |--read_lut (reading LUT)
!                      |--open_oldfile(opening file carefully) 
!            |--open_lut_broad_gocart (open LUT for aerosols)
!                 |--read_lut (reading LUT)
!                      |--open_oldfile(opening file carefully) 
!
!
!
! goddardrad_2014(preparation for radiative transfer)
!     !-- sounding_interp (interpolate sounding climatology)
!     |-- ozone_interp (intepolate ozone profile)
!     |-- opt_cloud( derive cloud optiacl properties )
!     |-- aero_opt( derive aerosol optiacl properties )
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
!   Oct  2016   , Jarno Mielikainen : additional code optimization
!   Sep  2015   , Toshi Matsui  : re-scaleing molecular absorption 50% up in SW subroutine to adjust toward RRTMG.
!   July 2015    , Toshi Matsui : mcdat_int is io parameter now for running hybrid MPI/OMP
!   Feb 2014    , Toshi Matsui : Optimization of new single scattering routine is done. 
!                                Also modified new IO parameters (re3d, aod, and cod). 
!                                Added additional SW flux paarameters for WRF v3.5.1 radiation driver.
!   Aug 2013    , Toshi Matsui : Completely revisied single-scattering tables that is now 
!                                identical to G-SDSU (satellite simulator). For this change, 
!                                original hydrometero single-scattering values are removed, and now 
!                                WRF must call init_goddardrad for initializing single-scattering LUT. 
!                                So I removed cwc and reff inputs for swrad/lwrad subroutine. 
!                                So far this only apply to cloud species, and aerosol are yet testing. 
!   Feb 2011    , Toshi Matsui : New optimization (skip nighttime completely for SW routine).
!                                Now wrapper routine are single precision, while
!                                routine under swrad and lwrad are double precision.
!                                Stratosphere option was removed.
!   Feb 2011    , Toshi Matsui&Roger Shi : Slight modification for online-gocart coupling. 
!   Oct 2008    , Toshi Matsui : All real parameters becomes double precision for stability. 
!   Jun 2008    , Toshi Matsui : Sounding interp is called only for each j_loop
!   Jun 2008    , Toshi Matsui : Vector loop (m) was completely removed, now radiation code is 
!                                compltely 1dimensionalized (-> 30~40% speed up with O3 optimization)
!   Apr 2008    , Toshi Matsui : Add stratosphere layrs options 
!   Aug 2007    , Toshi Matsui : One-dimensinalized swrad to skip swrad for nighttime.
!   Jun 2007    , Toshi Matsui ; Revise broadband flux reduction to avoid negative SW heating.
!   May 2007    , Toshi Matsui ; Made driver to plug WRF, GOCCART module, and F90 revision.
!   Apr 2007    , Toshi Matsui ; SW radiation routine is optimized (add fast_overcast option for CRM).
!     ~ 2002    , Ming-Dah Chou and Max Suarez ; initial development. 
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
 module module_ra_goddard_2014
 use module_wrf_error
 use module_checkerror
#if (WRF_CHEM == 1)
 use module_gocart_coupling
#endif

 implicit none

  INTEGER, PARAMETER, PRIVATE:: CHUNK = 16

!
! encapsulation control
!
  private  !-> privatize all variables in this module excepting public parameter (subroutine) below. 
  public :: init_goddardrad_2014, goddardrad_2014  !only this subroutine can be called from other module

! integer,parameter :: fp_kind = SELECTED_REAL_KIND( 6, 37)  !single precision precision kind
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
! determined by N = ij_skip = NINT(6000/dx), when iskip (= goddardrad_2014_skip) = 1
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
!  pressures less than 10 mb. But, the computationally slightly cheap.
!
   ! NUWRF EMK 2 March 2015...Changed to true per instructions from Toshi.
   logical,parameter :: high = .true.    ! about a few W/m2 difference in typical WRF run
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
 type particle_gocart !GOCART paticle type
    real :: so4,blc,ocn,och,ssa,ssc,du1,du2,du3,du4,du5,du6,du7,du8
 end type particle_gocart

!
! Single-scattering look-up tables (final dimension is 1-SW, 2-LW)
!
! NUWRF EMK...Change path to GODDARDRAD_SSLUT to immediate subdirectory, which
! can be symbolic link to actual location. Allows for more flexibility in
! where to run wrf.exe.
! character(len=200),parameter :: dir_sslut = './../GODDARDRAD_SSLUT/' !directory of single scattering tables
 character(len=200),parameter :: dir_sslut = './GODDARDRAD_SSLUT/' !directory of single scattering tables

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

 subroutine init_goddardrad_2014(allowed_to_read, &
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
 if(allowed_to_read) call read_sslut(mxband)

 return
 end subroutine init_goddardrad_2014

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine read_sslut(mxband)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Allocate and initialize broad-band single scattering parameters. 
! 
! History: 08/2013  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!---------------------------------------------------------------------------------------------------
 integer,intent(in) :: mxband !maximum band

 integer :: i,j,k,r !domain loop index
 integer :: ib,n  !band loop index
 integer :: ierr  !allocation stat
 integer :: max_band
 
!
! Read Single-Scattering LUTs for one time only. 
!
 if( .not. allocated(kext_lut_gen) ) then
     allocate( &
     kext_lut_gen(mxpts_re_visir, mxband, mxib), & !
     salb_lut_gen(mxpts_re_visir, mxband, mxib), & !
     asym_lut_gen(mxpts_re_visir, mxband, mxib), & !
     stat=ierr )
!     if (ierr /= 0) stop 'MSG opt_broad: LUT parameter allocation error -> Terminate program. '
     if (ierr /= 0) call wrf_error_fatal('MSG opt_broad: LUT parameter allocation error -> Terminate program. ')


!
! initialize
!
     do ib = 1, mxib ; do n = 1, mxband ; do r = 1, mxpts_re_visir
        call init_gen(kext_lut_gen(r,n,ib),'zero') 
        call init_gen(salb_lut_gen(r,n,ib),'zero')
        call init_gen(asym_lut_gen(r,n,ib),'zero')
     enddo ; enddo ; enddo

     do ib = 1,mxib
        call open_lut_broad_gen( ib )  !read LUTs
     enddo


!
! convert into bundled LUT
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
!     if (ierr /= 0) stop 'MSG opt_broad: LUT parameter allocation error -> Terminate program. '
     if (ierr /= 0) call wrf_error_fatal('MSG opt_broad: LUT parameter allocation error -> Terminate program. ')

 endif

!
! bundle all single scattering properties
!
 call bundle_sslut()


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

#if (WRF_CHEM == 1)

 if( new_method_aero ) then !under test ....

 if( .not. allocated(kext_lut_gocart) ) then
      allocate( &
      kext_lut_gocart(mxpts_rh, mxband, mxib), & !
      salb_lut_gocart(mxpts_rh, mxband, mxib), & !
      asym_lut_gocart(mxpts_rh, mxband, mxib), & !
      stat=ierr )
!      if (ierr /= 0) stop 'MSG opt_broad: LUT parameter allocation error -> Terminate program. '
      if (ierr /= 0) call wrf_error_fatal('MSG opt_broad: LUT parameter allocation error -> Terminate program. ')

!
! initialize
!
     do ib = 1, mxib ; do n = 1, mxband ; do r = 1, mxpts_rh
        call init_gocart(kext_lut_gocart(r,n,ib),'zero')
        call init_gocart(salb_lut_gocart(r,n,ib),'zero')
        call init_gocart(asym_lut_gocart(r,n,ib),'zero')
     enddo ; enddo ; enddo

      do ib = 1,mxib
         call open_lut_broad_gocart( ib )  !read LUTs
      enddo
    endif

 endif

#endif

 return
 end subroutine read_sslut

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine open_lut_broad_gen(ib)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Collect the prepared broad-band Look-up table for GCE microphysics
! 
! History: 10/2009  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!---------------------------------------------------------------------------------------------------
 integer,intent(in) :: ib !1-SW, 2-LW

 integer :: i,j,n !loop indice
 integer,parameter :: io = 501
 integer :: max_band
 character(len=2) :: rad_typ
 character(len=2) :: char_ib
 character(len=50) :: fname
! character(len=20),parameter :: cloud_microphysics = 'GOD10'  !goddard microphysics
 character(len=20),parameter :: cloud_microphysics = 'GMP4ICE'  !goddard microphysics


 if(ib == 1) then !SW
    max_band = nband_sw
    rad_typ = 'SW'
 else              !LW
    max_band = nband_lw
    rad_typ = 'LW'
 endif


 BAND_LOOP: do n = 1, max_band

 write(char_ib,"(I2.2)") n  !from integer to character 

 fname = 'CW-'//trim(cloud_microphysics)//'-'//rad_typ//'band'//trim(char_ib)//'.dat'  ! cloud water
 call read_lut( fname, io, kext_lut_gen(:,n,ib)%cloud, salb_lut_gen(:,n,ib)%cloud, asym_lut_gen(:,n,ib)%cloud )

 fname = 'RR-'//trim(cloud_microphysics)//'-'//rad_typ//'band'//trim(char_ib)//'.dat'  ! rain
 call read_lut( fname, io, kext_lut_gen(:,n,ib)%rain, salb_lut_gen(:,n,ib)%rain, asym_lut_gen(:,n,ib)%rain )

 fname = 'CI-'//trim(cloud_microphysics)//'-'//rad_typ//'band'//trim(char_ib)//'.dat'  ! cloud ice
 call read_lut( fname, io, kext_lut_gen(:,n,ib)%ice, salb_lut_gen(:,n,ib)%ice, asym_lut_gen(:,n,ib)%ice )

 fname = 'SN-'//trim(cloud_microphysics)//'-'//rad_typ//'band'//trim(char_ib)//'.dat'  ! snow
 call read_lut( fname, io, kext_lut_gen(:,n,ib)%snow, salb_lut_gen(:,n,ib)%snow, asym_lut_gen(:,n,ib)%snow )

 fname = 'GR-'//trim(cloud_microphysics)//'-'//rad_typ//'band'//trim(char_ib)//'.dat'  !graupel
 call read_lut( fname, io, kext_lut_gen(:,n,ib)%graupel, salb_lut_gen(:,n,ib)%graupel, asym_lut_gen(:,n,ib)%graupel)

 fname = 'HL-'//trim(cloud_microphysics)//'-'//rad_typ//'band'//trim(char_ib)//'.dat'  !hail
 call read_lut( fname, io, kext_lut_gen(:,n,ib)%hail, salb_lut_gen(:,n,ib)%hail, asym_lut_gen(:,n,ib)%hail )

 enddo BAND_LOOP

 return
 end subroutine open_lut_broad_gen

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine open_lut_broad_gocart(ib)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Collect the prepared broad-band Look-up table for GOCART aerosol microphysics
! 
! History: 10/2009  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!---------------------------------------------------------------------------------------------------
 integer,intent(in) :: ib !1-SW, 2-LW

 integer :: i,j,n !loop indice
 integer,parameter :: io = 504
 integer :: max_band
 character(len=2) :: rad_typ
 character(len=2) :: char_ib
 character(len=50) :: fname


 if(ib == 1) then !SW
    max_band = nband_sw
     rad_typ = 'SW'
 else              !LW
    max_band = nband_lw
     rad_typ = 'LW'
 endif


 BAND_LOOP: do n = 1, max_band

 write(char_ib,"(I2.2)") n  !from integer to character 

 fname = 'SO4-GOCART-'//rad_typ//'band'//trim(char_ib)//'.dat' 
 call read_lut( fname, io, kext_lut_gocart(:,n,ib)%so4, salb_lut_gocart(:,n,ib)%so4, asym_lut_gocart(:,n,ib)%so4 )

 fname = 'BLC-GOCART-'//rad_typ//'band'//trim(char_ib)//'.dat' 
 call read_lut( fname, io, kext_lut_gocart(:,n,ib)%blc, salb_lut_gocart(:,n,ib)%blc, asym_lut_gocart(:,n,ib)%blc )

 fname = 'OCN-GOCART-'//rad_typ//'band'//trim(char_ib)//'.dat'
 call read_lut( fname, io, kext_lut_gocart(:,n,ib)%ocn, salb_lut_gocart(:,n,ib)%ocn, asym_lut_gocart(:,n,ib)%ocn )

 fname = 'SSA-GOCART-'//rad_typ//'band'//trim(char_ib)//'.dat' 
 call read_lut( fname, io, kext_lut_gocart(:,n,ib)%ssa, salb_lut_gocart(:,n,ib)%ssa, asym_lut_gocart(:,n,ib)%ssa )

 fname = 'SSC-GOCART-'//rad_typ//'band'//trim(char_ib)//'.dat' 
 call read_lut( fname, io, kext_lut_gocart(:,n,ib)%ssc, salb_lut_gocart(:,n,ib)%ssc, asym_lut_gocart(:,n,ib)%ssc )

 fname = 'DU1-GOCART-'//rad_typ//'band'//trim(char_ib)//'.dat'
 call read_lut( fname, io, kext_lut_gocart(:,n,ib)%du1, salb_lut_gocart(:,n,ib)%du1, asym_lut_gocart(:,n,ib)%du1 )

 fname = 'DU2-GOCART-'//rad_typ//'band'//trim(char_ib)//'.dat' 
 call read_lut( fname, io, kext_lut_gocart(:,n,ib)%du2, salb_lut_gocart(:,n,ib)%du2, asym_lut_gocart(:,n,ib)%du2 )

 fname = 'DU3-GOCART-'//rad_typ//'band'//trim(char_ib)//'.dat' 
 call read_lut( fname, io, kext_lut_gocart(:,n,ib)%du3, salb_lut_gocart(:,n,ib)%du3, asym_lut_gocart(:,n,ib)%du3 )

 fname = 'DU4-GOCART-'//rad_typ//'band'//trim(char_ib)//'.dat' 
 call read_lut( fname, io, kext_lut_gocart(:,n,ib)%du4, salb_lut_gocart(:,n,ib)%du4, asym_lut_gocart(:,n,ib)%du4 )

 fname = 'DU5-GOCART-'//rad_typ//'band'//trim(char_ib)//'.dat' 
 call read_lut( fname, io, kext_lut_gocart(:,n,ib)%du5, salb_lut_gocart(:,n,ib)%du5, asym_lut_gocart(:,n,ib)%du5 )

 fname = 'DU6-GOCART-'//rad_typ//'band'//trim(char_ib)//'.dat' 
 call read_lut( fname, io, kext_lut_gocart(:,n,ib)%du6, salb_lut_gocart(:,n,ib)%du6, asym_lut_gocart(:,n,ib)%du6 )

 fname = 'DU7-GOCART-'//rad_typ//'band'//trim(char_ib)//'.dat' 
 call read_lut( fname, io, kext_lut_gocart(:,n,ib)%du7, salb_lut_gocart(:,n,ib)%du7, asym_lut_gocart(:,n,ib)%du7 )

 fname = 'DU8-GOCART-'//rad_typ//'band'//trim(char_ib)//'.dat' 
 call read_lut( fname, io, kext_lut_gocart(:,n,ib)%du8, salb_lut_gocart(:,n,ib)%du8, asym_lut_gocart(:,n,ib)%du8 )

 enddo BAND_LOOP

 return
 end subroutine open_lut_broad_gocart

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine read_lut( fname, io, kext_lut, salb_lut, asym_lut)
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
! Open and read the prepared broad-band Look-up table. 
! 
! History: 10/2009  Toshi Matsui@NASA GSFC ; Initial
!           
! References: 
!---------------------------------------------------------------------------------------------------
 character(len=50),intent(in) :: fname
 integer,intent(in) :: io
 real,dimension(:),intent(inout) :: kext_lut, salb_lut, asym_lut  !single-scattering properties 
 integer :: j
 integer :: mxpts(1)

!
! Estimate memory bound 
!
  mxpts = UBOUND(kext_lut)

!
! open prepared LUTs 
!
 call open_oldfile(io, trim(dir_sslut)//trim(fname) )

 do j = 1, mxpts(1)
    read(io,90) kext_lut(j), salb_lut(j), asym_lut(j)
 enddo
 close (io)

 90  Format ( 1P3E14.6 )

 return
 end subroutine read_lut

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine open_oldfile(io_unit,io_file)
 implicit none
!--------------------------------------------------------------------------------------------
! Comments:  
!  This subroutine open existing file more carefully. It actually try 10 times before it getting failure. 
! 
! History:
! 09/2012  Toshi Matsui@NASA GSFC : Initial. 
!           
! References: 
!-----------------------------------------------------------------------------------------------------
 integer,intent(in) :: io_unit
 character(*),intent(in) :: io_file  !io file name and directory


 integer :: open_stat
 integer :: ntries  !number of tries to open the file
 character(len=200) :: error_msg

 ntries = 0   ! number of tries to open the file

 forever: do

    open (unit=io_unit, file=trim(io_file), status='old', iostat=open_stat)

    if (open_stat /= 0) then
       ntries = ntries + 1
       if (ntries > 10) then  !if more than 10 sec
           error_msg = 'MSG open_oldfile: Cannot open file '//trim(io_file)
!           stop 
          call wrf_error_fatal(trim(error_msg))
       endif
       call sleep(1)   ! Fail openiing a file,  so wait 1 second before retrying
    else
       exit forever   !success opening a file
    endif

 enddo forever

 return
 end subroutine open_oldfile

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine bundle_sslut
 implicit none
 integer :: n,ire,ib 

 ib = 1
 do n = 1, nband_sw ; do ire = 1, mxpts_re_visir

    kext_sw_cloud(n,ire,id_cloud) = kext_lut_gen(ire,n,ib)%cloud
    salb_sw_cloud(n,ire,id_cloud) = salb_lut_gen(ire,n,ib)%cloud 
    asym_sw_cloud(n,ire,id_cloud) = asym_lut_gen(ire,n,ib)%cloud

    kext_sw_cloud(n,ire,id_ice) = kext_lut_gen(ire,n,ib)%ice
    salb_sw_cloud(n,ire,id_ice) = salb_lut_gen(ire,n,ib)%ice
    asym_sw_cloud(n,ire,id_ice) = asym_lut_gen(ire,n,ib)%ice

    kext_sw_cloud(n,ire,id_rain) = kext_lut_gen(ire,n,ib)%rain
    salb_sw_cloud(n,ire,id_rain) = salb_lut_gen(ire,n,ib)%rain
    asym_sw_cloud(n,ire,id_rain) = asym_lut_gen(ire,n,ib)%rain

    kext_sw_cloud(n,ire,id_snow) = kext_lut_gen(ire,n,ib)%snow
    salb_sw_cloud(n,ire,id_snow) = salb_lut_gen(ire,n,ib)%snow
    asym_sw_cloud(n,ire,id_snow) = asym_lut_gen(ire,n,ib)%snow

    kext_sw_cloud(n,ire,id_graupel) = kext_lut_gen(ire,n,ib)%graupel
    salb_sw_cloud(n,ire,id_graupel) = salb_lut_gen(ire,n,ib)%graupel
    asym_sw_cloud(n,ire,id_graupel) = asym_lut_gen(ire,n,ib)%graupel

    kext_sw_cloud(n,ire,id_hail) = kext_lut_gen(ire,n,ib)%hail
    salb_sw_cloud(n,ire,id_hail) = salb_lut_gen(ire,n,ib)%hail
    asym_sw_cloud(n,ire,id_hail) = asym_lut_gen(ire,n,ib)%hail

 enddo ; enddo


 ib = 2
 do n = 1, nband_sw ; do ire = 1, mxpts_re_visir

    kext_lw_cloud(n,ire,id_cloud) = kext_lut_gen(ire,n,ib)%cloud
    salb_lw_cloud(n,ire,id_cloud) = salb_lut_gen(ire,n,ib)%cloud
    asym_lw_cloud(n,ire,id_cloud) = asym_lut_gen(ire,n,ib)%cloud

    kext_lw_cloud(n,ire,id_ice) = kext_lut_gen(ire,n,ib)%ice
    salb_lw_cloud(n,ire,id_ice) = salb_lut_gen(ire,n,ib)%ice
    asym_lw_cloud(n,ire,id_ice) = asym_lut_gen(ire,n,ib)%ice

    kext_lw_cloud(n,ire,id_rain) = kext_lut_gen(ire,n,ib)%rain
    salb_lw_cloud(n,ire,id_rain) = salb_lut_gen(ire,n,ib)%rain
    asym_lw_cloud(n,ire,id_rain) = asym_lut_gen(ire,n,ib)%rain

    kext_lw_cloud(n,ire,id_snow) = kext_lut_gen(ire,n,ib)%snow
    salb_lw_cloud(n,ire,id_snow) = salb_lut_gen(ire,n,ib)%snow
    asym_lw_cloud(n,ire,id_snow) = asym_lut_gen(ire,n,ib)%snow

    kext_lw_cloud(n,ire,id_graupel) = kext_lut_gen(ire,n,ib)%graupel
    salb_lw_cloud(n,ire,id_graupel) = salb_lut_gen(ire,n,ib)%graupel
    asym_lw_cloud(n,ire,id_graupel) = asym_lut_gen(ire,n,ib)%graupel

    kext_lw_cloud(n,ire,id_hail) = kext_lut_gen(ire,n,ib)%hail
    salb_lw_cloud(n,ire,id_hail) = salb_lut_gen(ire,n,ib)%hail
    asym_lw_cloud(n,ire,id_hail) = asym_lut_gen(ire,n,ib)%hail

 enddo ; enddo


 end subroutine bundle_sslut

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

   subroutine goddardrad_2014( dx, iskip, rthraten, gsf, xlat,xlong         &
                   ,dz8w,t8w,rho_phy,sw_or_lw                     &
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
 integer,intent(in) :: iskip ! 0 (no skip) 1 (skip)
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
!  if (mod(itimestep,i24h).eq.1) then
 if ( wrf_dm_on_monitor() .and. mod(itimestep,i24h).eq.1 ) then
    print *, 'in goddardrad1014 ', sw_or_lw
    print *, '   kts, kte = ', kts, kte
    print *, '   kms, kme = ', kms, kme
    print *, '   kds, kde = ', kds, kde
 endif

  if ( kme .ne. kte+1 ) then
     print *,'Something is strange, kde, kme, kte = ', kde, kme, kte 
     print *,'                      kds, kms, kts = ', kds, kms, kts 
     print *,' at timestep = ', itimestep
     print *,' at gmt, julday, xtime = ', gmt,julday,xtime
  endif

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
 if( iskip == 1 ) then
    !
    ! nearest integer of 6000m devided by horizontal space
    ! ranges from 1 ~ 3 (essentially become 2, when dx<=4000m)
    !
    ij_skip = min( 3,  max( 1, nint( 6000.0/dx ) ) ) !

 elseif( iskip == 0 ) then
    ij_skip = 1
 else
    call wrf_error_fatal('MSG: goddardrad_2014: iskip(goddardrad_2014_skip) must be 0 or 1 ')
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

      call swrad ( np=dk_half, icb=icb, ict=ict, fcld=dble(fcld1d), &
                   pl=dble(p8w1d), ta=dble(t1d), wa=dble(sh1d), oa=dble(o31d), &
                   taucl=dble(taucl_sw), ssacl=dble(ssacl_sw), asycl=dble(asycl_sw), &
                   taual=dble(taual_sw), ssaal=dble(ssaal_sw), asyal=dble(asyal_sw), &
                   cosz=dble(cosz), rsuvbm=dble(rsuvbm), rsuvdf=dble(rsuvdf), rsirbm=dble(rsirbm), rsirdf=dble(rsirdf),&
                   flx_out=flx, flxd_out=flxd,flxu_out=flxu, flxd_surf = flxd_surf, lmask=lmask, irestrict=min(CHUNK,ite-ii+1) )

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

         ! check error
         if( ISNAN( tten1d(ic,kt) ) ) then
            print*,'MSG goddardrad_2014 SW: Found NaN in tten1d(k)',i,j,kt
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
            print*,'MSG goddardrad_2014 : WARNING Negative SW heating =',&
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

       call lwrad ( np=dk_half, tb=dble(tsfc), ts=dble(tskin), ict=ict, icb=icb,&
                    pl=dble(p8w1d), ta=dble(t1d), wa=dble(sh1d), oa=dble(o31d), &
                    emiss=dble(emis1d), fcld=dble(fcld1d), &
                    taucl=dble(taucl_lw), ssacl=dble(ssacl_lw), asycl=dble(asycl_lw),  &
                    taual=dble(taual_lw), ssaal=dble(ssaal_lw), asyal=dble(asyal_lw),  &
                    flx_out=flx, acflxd_out=flxd, acflxu_out=flxu, irestrict=min(CHUNK,ite-ii+1) )

!
! calculate heating rate (deg/sec)
!
     fac = .01 * g / cp

     do kt=kts,kte !half level ( kts = TOA, kte = BOA )
       DO ic=1,min(CHUNK,ite-ii+1)
        i = ii+ic -1
        tten1d(ic,kt) = - fac * (flx(ic,kt-1) - flx(ic,kt)) / (p8w1d(ic,kt-1)-p8w1d(ic,kt))

        if( ISNAN( tten1d(ic,kt) ) ) then
            print*,'MSG goddardrad_2014 LW: Found NaN in tten1d(k)',i,j,kt
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
      call wrf_error_fatal('MSG: goddardrad_2014: option does not exist. check sw_or_lw ')
  end select rad_select

 ENDIF
 ENDDO

 return
 end subroutine goddardrad_2014

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

 return
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

 return
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

 return
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

 return
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

 return
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

 return
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

 return
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


 return
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

 return
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

!toshii
!print*,'k=',k,'dp(ic,k)=',dp(ic,k),'pl(ic,k+1)',pl(ic,k+1),'pl(ic,k)=',pl(ic,k),'oa(ic,k)=',oa(ic,k)

!
!-----compute scaled water vapor amount following eqs. (3.3) and (3.5)
!     unit is g/cm**2
!

!
! re-scaleing molecular absorption 50% up (factor of 1.5) due to sever underestimation of 
! clear-sky atmospheric abosrption in original CliRad.
!
    scal(ic,k)=1.5* dp(ic,k)*(.5*(pl(ic,k)+pl(ic,k+1))/300.)**.8  !50% increase (1.5 factor )--> toshi

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

!stop


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
   cnt(ic)=co2*snt(ic)
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

 return
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

 return
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
      data hk/ &
       .20673,.08236,.01074,  .03497,.01157,.00360, &
       .03011,.01133,.00411,  .02260,.01143,.00421, &
       .01336,.01240,.00389,  .00696,.01258,.00326, &
       .00441,.01381,.00499,  .00115,.00650,.00465, &
       .00026,.00244,.00245,  .00000,.00094,.00145/
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
!   DO ic=1,irestrict
   do concurrent ( ic = 1:min(CHUNK,irestrict) )
   if(lmask(ic) .eqv. .true.) then

           taurs=ry(ib)*dp(ic,k) 
           tauwv=xk(ik)*wh(ic,k)

           tausto(ic,k)=max(taurs+tauwv+taual(ic,k,iv), opt_min)
           ssatau(ic,k)=ssaal(ic,k,iv)*taual(ic,k,iv)+taurs
           asysto(ic,k)=asyal(ic,k,iv)*ssaal(ic,k,iv)*taual(ic,k,iv)

   endif
   enddo

           IF_CLEAR: if (overcast .and. fast_overcast ) then

   do concurrent ( ic = 1:min(CHUNK,irestrict) )
   if(lmask(ic) .eqv. .true.) then

             rr(ic,k,1)=0.e0
             tt(ic,k,1)=0.e0
             td(ic,k,1)=0.e0
             rs(ic,k,1)=0.e0
             ts(ic,k,1)=0.e0

   endif
   enddo

           else

   do concurrent ( ic = 1:min(CHUNK,irestrict) )
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
   do concurrent ( ic = 1:min(CHUNK,irestrict) )
   if(lmask(ic) .eqv. .true.) then

             rr(ic,k,1)=rrt(ic,k)
             tt(ic,k,1)=ttt(ic,k)
             td(ic,k,1)=tdt(ic,k)
             rs(ic,k,1)=rst(ic,k)
             ts(ic,k,1)=tst(ic,k)

   endif
   enddo

           endif IF_CLEAR

   do concurrent ( ic = 1:min(CHUNK,irestrict) )
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
   do concurrent ( ic = 1:min(CHUNK,irestrict) )
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

 return
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
      return
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


      return
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

      return
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
      data xkw / 29.55  , 4.167e-1, 1.328e-2, 5.250e-4, &
                 5.25e-4, 9.369e-3, 4.719e-2, 1.320e-0, 5.250e-4/
!-----xke is the absorption coefficient given in table 9 for the first
!     k-distribution function due to water vapor continuum absorption
!     units are cm**2/g
      data xke /  0.00,   0.00,   27.40,   15.8, &
                  9.40,   7.75,    8.78,    0.0,   0.0/
!-----mw is the ratio between neighboring absorption coefficients
!     for water vapor line absorption (table 4).
      data mw /6,6,8,6,6,8,9,6,16/
!-----aw and bw (table 3) are the coefficients for temperature scaling
!     in eq. (4.2).
      data aw/ 0.0021, 0.0140, 0.0167, 0.0302, &
               0.0307, 0.0195, 0.0152, 0.0008, 0.0096/
      data bw/ -1.01e-5, 5.57e-5, 8.54e-5, 2.96e-4, &
                2.86e-4, 1.108e-4, 7.608e-5, -3.52e-6, 1.64e-5/
!-----pm is the pressure-scaling parameter for water vapor absorption
!     eq. (4.1) and table 3.
      data pm/ 1.0, 1.0, 1.0, 1.0, 1.0, 0.77, 0.5, 1.0, 1.0/
!-----fkw is the planck-weighted k-distribution function due to h2o
!     line absorption given in table 4.
!     the k-distribution function for the third band, fkw(*,3),
!     is not used (see the parameter gkw below).
      data fkw / 0.2747,0.2717,0.2752,0.1177,0.0352,0.0255, &
                 0.1521,0.3974,0.1778,0.1826,0.0374,0.0527, &
                 6*1.00, &
                 0.4654,0.2991,0.1343,0.0646,0.0226,0.0140, &
                 0.5543,0.2723,0.1131,0.0443,0.0160,0.0000, &
                 0.5955,0.2693,0.0953,0.0335,0.0064,0.0000, &
                 0.1958,0.3469,0.3147,0.1013,0.0365,0.0048, &
                 0.0740,0.1636,0.4174,0.1783,0.1101,0.0566, &
                 0.1437,0.2197,0.3185,0.2351,0.0647,0.0183/
!-----gkw is the planck-weighted k-distribution function due to h2o
!     line absorption in the 3 subbands (800-720,620-720,540-620 /cm)
!     of band 3 given in table 10.  note that the order of the sub-bands
!     is reversed.
      data gkw/  0.1782,0.0593,0.0215,0.0068,0.0022,0.0000, &
                 0.0923,0.1675,0.0923,0.0187,0.0178,0.0000, &
                 0.0000,0.1083,0.1581,0.0455,0.0274,0.0041/
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
         0.99993843,  0.99990183,  0.99985260,  0.99979079,  0.99971771, &
         0.99963379,  0.99953848,  0.99942899,  0.99930018,  0.99914461, &
         0.99895102,  0.99870503,  0.99838799,  0.99797899,  0.99745202, &
         0.99677002,  0.99587703,  0.99469399,  0.99311298,  0.99097902, &
         0.98807001,  0.98409998,  0.97864997,  0.97114998,  0.96086001, &
         0.94682997,  0.92777002,  0.90200001,  0.86739999,  0.82169998, &
         0.76270002/
      data ((h12(ip,iw),iw=1,31), ip= 1, 1)/ &
        -0.2021e-06, -0.3628e-06, -0.5891e-06, -0.8735e-06, -0.1204e-05, &
        -0.1579e-05, -0.2002e-05, -0.2494e-05, -0.3093e-05, -0.3852e-05, &
        -0.4835e-05, -0.6082e-05, -0.7591e-05, -0.9332e-05, -0.1128e-04, &
        -0.1347e-04, -0.1596e-04, -0.1890e-04, -0.2241e-04, -0.2672e-04, &
        -0.3208e-04, -0.3884e-04, -0.4747e-04, -0.5854e-04, -0.7272e-04, &
        -0.9092e-04, -0.1146e-03, -0.1458e-03, -0.1877e-03, -0.2435e-03, &
        -0.3159e-03/
      data ((h13(ip,iw),iw=1,31), ip= 1, 1)/ &
         0.5907e-09,  0.8541e-09,  0.1095e-08,  0.1272e-08,  0.1297e-08, &
         0.1105e-08,  0.6788e-09, -0.5585e-10, -0.1147e-08, -0.2746e-08, &
        -0.5001e-08, -0.7715e-08, -0.1037e-07, -0.1227e-07, -0.1287e-07, &
        -0.1175e-07, -0.8517e-08, -0.2920e-08,  0.4786e-08,  0.1407e-07, &
         0.2476e-07,  0.3781e-07,  0.5633e-07,  0.8578e-07,  0.1322e-06, &
         0.2013e-06,  0.3006e-06,  0.4409e-06,  0.6343e-06,  0.8896e-06, &
         0.1216e-05/
      data ((h11(ip,iw),iw=1,31), ip= 2, 2)/ &
         0.99993837,  0.99990171,  0.99985230,  0.99979031,  0.99971670, &
         0.99963200,  0.99953520,  0.99942321,  0.99928987,  0.99912637, &
         0.99892002,  0.99865198,  0.99830002,  0.99783802,  0.99723297, &
         0.99643701,  0.99537897,  0.99396098,  0.99204701,  0.98944002, &
         0.98588002,  0.98098999,  0.97425997,  0.96502000,  0.95236999, &
         0.93515998,  0.91184998,  0.88040000,  0.83859998,  0.78429997, &
         0.71560001/
      data ((h12(ip,iw),iw=1,31), ip= 2, 2)/ &
        -0.2017e-06, -0.3620e-06, -0.5878e-06, -0.8713e-06, -0.1201e-05, &
        -0.1572e-05, -0.1991e-05, -0.2476e-05, -0.3063e-05, -0.3808e-05, &
        -0.4776e-05, -0.6011e-05, -0.7516e-05, -0.9272e-05, -0.1127e-04, &
        -0.1355e-04, -0.1620e-04, -0.1936e-04, -0.2321e-04, -0.2797e-04, &
        -0.3399e-04, -0.4171e-04, -0.5172e-04, -0.6471e-04, -0.8150e-04, &
        -0.1034e-03, -0.1321e-03, -0.1705e-03, -0.2217e-03, -0.2889e-03, &
        -0.3726e-03/
      data ((h13(ip,iw),iw=1,31), ip= 2, 2)/ &
         0.5894e-09,  0.8519e-09,  0.1092e-08,  0.1267e-08,  0.1289e-08, &
         0.1093e-08,  0.6601e-09, -0.7831e-10, -0.1167e-08, -0.2732e-08, &
        -0.4864e-08, -0.7334e-08, -0.9581e-08, -0.1097e-07, -0.1094e-07, &
        -0.8999e-08, -0.4669e-08,  0.2391e-08,  0.1215e-07,  0.2424e-07, &
         0.3877e-07,  0.5711e-07,  0.8295e-07,  0.1218e-06,  0.1793e-06, &
         0.2621e-06,  0.3812e-06,  0.5508e-06,  0.7824e-06,  0.1085e-05, &
         0.1462e-05/
      data ((h11(ip,iw),iw=1,31), ip= 3, 3)/ &
         0.99993825,  0.99990153,  0.99985188,  0.99978942,  0.99971509, &
         0.99962920,  0.99953020,  0.99941432,  0.99927431,  0.99909937, &
         0.99887401,  0.99857497,  0.99817699,  0.99764699,  0.99694097, &
         0.99599802,  0.99473000,  0.99301600,  0.99068397,  0.98749000, &
         0.98311001,  0.97707999,  0.96877003,  0.95738000,  0.94186002, &
         0.92079002,  0.89230001,  0.85420001,  0.80430001,  0.74049997, &
         0.66200000/
      data ((h12(ip,iw),iw=1,31), ip= 3, 3)/ &
        -0.2011e-06, -0.3609e-06, -0.5859e-06, -0.8680e-06, -0.1195e-05, &
        -0.1563e-05, -0.1975e-05, -0.2450e-05, -0.3024e-05, -0.3755e-05, &
        -0.4711e-05, -0.5941e-05, -0.7455e-05, -0.9248e-05, -0.1132e-04, &
        -0.1373e-04, -0.1659e-04, -0.2004e-04, -0.2431e-04, -0.2966e-04, &
        -0.3653e-04, -0.4549e-04, -0.5724e-04, -0.7259e-04, -0.9265e-04, &
        -0.1191e-03, -0.1543e-03, -0.2013e-03, -0.2633e-03, -0.3421e-03, &
        -0.4350e-03/
      data ((h13(ip,iw),iw=1,31), ip= 3, 3)/ &
         0.5872e-09,  0.8484e-09,  0.1087e-08,  0.1259e-08,  0.1279e-08, &
         0.1077e-08,  0.6413e-09, -0.9334e-10, -0.1161e-08, -0.2644e-08, &
        -0.4588e-08, -0.6709e-08, -0.8474e-08, -0.9263e-08, -0.8489e-08, &
        -0.5553e-08,  0.1203e-09,  0.9035e-08,  0.2135e-07,  0.3689e-07, &
         0.5610e-07,  0.8097e-07,  0.1155e-06,  0.1649e-06,  0.2350e-06, &
         0.3353e-06,  0.4806e-06,  0.6858e-06,  0.9617e-06,  0.1315e-05, &
         0.1741e-05/
      data ((h11(ip,iw),iw=1,31), ip= 4, 4)/ &
         0.99993813,  0.99990118,  0.99985123,  0.99978811,  0.99971271, &
         0.99962479,  0.99952239,  0.99940068,  0.99925101,  0.99905968, &
         0.99880803,  0.99846900,  0.99800998,  0.99738997,  0.99655402, &
         0.99542397,  0.99389100,  0.99180400,  0.98895001,  0.98501998, &
         0.97961003,  0.97215003,  0.96191001,  0.94791001,  0.92887998, &
         0.90311998,  0.86849999,  0.82270002,  0.76370001,  0.69000000, &
         0.60240000/
      data ((h12(ip,iw),iw=1,31), ip= 4, 4)/ &
        -0.2001e-06, -0.3592e-06, -0.5829e-06, -0.8631e-06, -0.1187e-05, &
        -0.1549e-05, -0.1953e-05, -0.2415e-05, -0.2975e-05, -0.3694e-05, &
        -0.4645e-05, -0.5882e-05, -0.7425e-05, -0.9279e-05, -0.1147e-04, &
        -0.1406e-04, -0.1717e-04, -0.2100e-04, -0.2580e-04, -0.3191e-04, &
        -0.3989e-04, -0.5042e-04, -0.6432e-04, -0.8261e-04, -0.1068e-03, &
        -0.1389e-03, -0.1820e-03, -0.2391e-03, -0.3127e-03, -0.4021e-03, &
        -0.5002e-03/
      data ((h13(ip,iw),iw=1,31), ip= 4, 4)/ &
         0.5838e-09,  0.8426e-09,  0.1081e-08,  0.1249e-08,  0.1267e-08, &
         0.1062e-08,  0.6313e-09, -0.8241e-10, -0.1094e-08, -0.2436e-08, &
        -0.4100e-08, -0.5786e-08, -0.6992e-08, -0.7083e-08, -0.5405e-08, &
        -0.1259e-08,  0.6099e-08,  0.1732e-07,  0.3276e-07,  0.5256e-07, &
         0.7756e-07,  0.1103e-06,  0.1547e-06,  0.2159e-06,  0.3016e-06, &
         0.4251e-06,  0.6033e-06,  0.8499e-06,  0.1175e-05,  0.1579e-05, &
         0.2044e-05/
      data ((h11(ip,iw),iw=1,31), ip= 5, 5)/ &
         0.99993789,  0.99990070,  0.99985009,  0.99978602,  0.99970889, &
         0.99961799,  0.99951053,  0.99938041,  0.99921662,  0.99900270, &
         0.99871498,  0.99832201,  0.99778402,  0.99704897,  0.99604702, &
         0.99468100,  0.99281400,  0.99025702,  0.98673999,  0.98189002, &
         0.97521001,  0.96600002,  0.95337999,  0.93620998,  0.91292000, &
         0.88150001,  0.83969998,  0.78530002,  0.71650004,  0.63330001, &
         0.53799999/
      data ((h12(ip,iw),iw=1,31), ip= 5, 5)/ &
        -0.1987e-06, -0.3565e-06, -0.5784e-06, -0.8557e-06, -0.1175e-05, &
        -0.1530e-05, -0.1923e-05, -0.2372e-05, -0.2919e-05, -0.3631e-05, &
        -0.4587e-05, -0.5848e-05, -0.7442e-05, -0.9391e-05, -0.1173e-04, &
        -0.1455e-04, -0.1801e-04, -0.2232e-04, -0.2779e-04, -0.3489e-04, &
        -0.4428e-04, -0.5678e-04, -0.7333e-04, -0.9530e-04, -0.1246e-03, &
        -0.1639e-03, -0.2164e-03, -0.2848e-03, -0.3697e-03, -0.4665e-03, &
        -0.5646e-03/
      data ((h13(ip,iw),iw=1,31), ip= 5, 5)/ &
         0.5785e-09,  0.8338e-09,  0.1071e-08,  0.1239e-08,  0.1256e-08, &
         0.1057e-08,  0.6480e-09, -0.1793e-10, -0.9278e-09, -0.2051e-08, &
        -0.3337e-08, -0.4514e-08, -0.5067e-08, -0.4328e-08, -0.1545e-08, &
         0.4100e-08,  0.1354e-07,  0.2762e-07,  0.4690e-07,  0.7190e-07, &
         0.1040e-06,  0.1459e-06,  0.2014e-06,  0.2764e-06,  0.3824e-06, &
         0.5359e-06,  0.7532e-06,  0.1047e-05,  0.1424e-05,  0.1873e-05, &
         0.2356e-05/
      data ((h11(ip,iw),iw=1,31), ip= 6, 6)/ &
         0.99993753,  0.99989992,  0.99984848,  0.99978292,  0.99970299, &
         0.99960762,  0.99949282,  0.99935049,  0.99916708,  0.99892199, &
         0.99858701,  0.99812400,  0.99748403,  0.99660099,  0.99538797, &
         0.99372399,  0.99143797,  0.98829001,  0.98395002,  0.97794998, &
         0.96968001,  0.95832998,  0.94283003,  0.92179000,  0.89330000, &
         0.85530001,  0.80519998,  0.74140000,  0.66280001,  0.57099998, &
         0.47049999/
      data ((h12(ip,iw),iw=1,31), ip= 6, 6)/ &
        -0.1964e-06, -0.3526e-06, -0.5717e-06, -0.8451e-06, -0.1158e-05, &
        -0.1504e-05, -0.1886e-05, -0.2322e-05, -0.2861e-05, -0.3576e-05, &
        -0.4552e-05, -0.5856e-05, -0.7529e-05, -0.9609e-05, -0.1216e-04, &
        -0.1528e-04, -0.1916e-04, -0.2408e-04, -0.3043e-04, -0.3880e-04, &
        -0.4997e-04, -0.6488e-04, -0.8474e-04, -0.1113e-03, -0.1471e-03, &
        -0.1950e-03, -0.2583e-03, -0.3384e-03, -0.4326e-03, -0.5319e-03, &
        -0.6244e-03/
      data ((h13(ip,iw),iw=1,31), ip= 6, 6)/ &
         0.5713e-09,  0.8263e-09,  0.1060e-08,  0.1226e-08,  0.1252e-08, &
         0.1076e-08,  0.7149e-09,  0.1379e-09, -0.6043e-09, -0.1417e-08, &
        -0.2241e-08, -0.2830e-08, -0.2627e-08, -0.8950e-09,  0.3231e-08, &
         0.1075e-07,  0.2278e-07,  0.4037e-07,  0.6439e-07,  0.9576e-07, &
         0.1363e-06,  0.1886e-06,  0.2567e-06,  0.3494e-06,  0.4821e-06, &
         0.6719e-06,  0.9343e-06,  0.1280e-05,  0.1705e-05,  0.2184e-05, &
         0.2651e-05/
      data ((h11(ip,iw),iw=1,31), ip= 7, 7)/ &
         0.99993700,  0.99989867,  0.99984592,  0.99977797,  0.99969423, &
         0.99959219,  0.99946660,  0.99930722,  0.99909681,  0.99880999, &
         0.99841303,  0.99786001,  0.99708802,  0.99601799,  0.99453998, &
         0.99250001,  0.98969001,  0.98580003,  0.98041999,  0.97299999, &
         0.96279001,  0.94881999,  0.92980999,  0.90407002,  0.86949998, &
         0.82370001,  0.76459998,  0.69089997,  0.60310000,  0.50479996, &
         0.40219998/
      data ((h12(ip,iw),iw=1,31), ip= 7, 7)/ &
        -0.1932e-06, -0.3467e-06, -0.5623e-06, -0.8306e-06, -0.1136e-05, &
        -0.1472e-05, -0.1842e-05, -0.2269e-05, -0.2807e-05, -0.3539e-05, &
        -0.4553e-05, -0.5925e-05, -0.7710e-05, -0.9968e-05, -0.1278e-04, &
        -0.1629e-04, -0.2073e-04, -0.2644e-04, -0.3392e-04, -0.4390e-04, &
        -0.5727e-04, -0.7516e-04, -0.9916e-04, -0.1315e-03, -0.1752e-03, &
        -0.2333e-03, -0.3082e-03, -0.3988e-03, -0.4982e-03, -0.5947e-03, &
        -0.6764e-03/
      data ((h13(ip,iw),iw=1,31), ip= 7, 7)/ &
         0.5612e-09,  0.8116e-09,  0.1048e-08,  0.1222e-08,  0.1270e-08, &
         0.1141e-08,  0.8732e-09,  0.4336e-09, -0.6548e-10, -0.4774e-09, &
        -0.7556e-09, -0.6577e-09,  0.4377e-09,  0.3359e-08,  0.9159e-08, &
         0.1901e-07,  0.3422e-07,  0.5616e-07,  0.8598e-07,  0.1251e-06, &
         0.1752e-06,  0.2392e-06,  0.3228e-06,  0.4389e-06,  0.6049e-06, &
         0.8370e-06,  0.1150e-05,  0.1547e-05,  0.2012e-05,  0.2493e-05, &
         0.2913e-05/
      data ((h11(ip,iw),iw=1,31), ip= 8, 8)/ &
         0.99993622,  0.99989682,  0.99984211,  0.99977070,  0.99968100, &
         0.99956948,  0.99942881,  0.99924588,  0.99899900,  0.99865800, &
         0.99818099,  0.99751103,  0.99657297,  0.99526602,  0.99345201, &
         0.99094099,  0.98746002,  0.98264998,  0.97599000,  0.96682000, &
         0.95423001,  0.93708003,  0.91380000,  0.88239998,  0.84060001, &
         0.78610003,  0.71730000,  0.63400000,  0.53859997,  0.43660003, &
         0.33510000/
      data ((h12(ip,iw),iw=1,31), ip= 8, 8)/ &
        -0.1885e-06, -0.3385e-06, -0.5493e-06, -0.8114e-06, -0.1109e-05, &
        -0.1436e-05, -0.1796e-05, -0.2219e-05, -0.2770e-05, -0.3535e-05, &
        -0.4609e-05, -0.6077e-05, -0.8016e-05, -0.1051e-04, -0.1367e-04, &
        -0.1768e-04, -0.2283e-04, -0.2955e-04, -0.3849e-04, -0.5046e-04, &
        -0.6653e-04, -0.8813e-04, -0.1173e-03, -0.1569e-03, -0.2100e-03, &
        -0.2794e-03, -0.3656e-03, -0.4637e-03, -0.5629e-03, -0.6512e-03, &
        -0.7167e-03/
      data ((h13(ip,iw),iw=1,31), ip= 8, 8)/ &
         0.5477e-09,  0.8000e-09,  0.1039e-08,  0.1234e-08,  0.1331e-08, &
         0.1295e-08,  0.1160e-08,  0.9178e-09,  0.7535e-09,  0.8301e-09, &
         0.1184e-08,  0.2082e-08,  0.4253e-08,  0.8646e-08,  0.1650e-07, &
         0.2920e-07,  0.4834e-07,  0.7564e-07,  0.1125e-06,  0.1606e-06, &
         0.2216e-06,  0.2992e-06,  0.4031e-06,  0.5493e-06,  0.7549e-06, &
         0.1035e-05,  0.1400e-05,  0.1843e-05,  0.2327e-05,  0.2774e-05, &
         0.3143e-05/
      data ((h11(ip,iw),iw=1,31), ip= 9, 9)/ &
         0.99993503,  0.99989408,  0.99983650,  0.99975997,  0.99966192, &
         0.99953687,  0.99937540,  0.99916059,  0.99886602,  0.99845397, &
         0.99787402,  0.99705601,  0.99590701,  0.99430102,  0.99206603, &
         0.98896003,  0.98465002,  0.97869003,  0.97044003,  0.95911002, &
         0.94363999,  0.92260998,  0.89419997,  0.85609996,  0.80610001, &
         0.74220002,  0.66359997,  0.57169998,  0.47100002,  0.36860001, &
         0.27079999/
      data ((h12(ip,iw),iw=1,31), ip= 9, 9)/ &
        -0.1822e-06, -0.3274e-06, -0.5325e-06, -0.7881e-06, -0.1079e-05, &
        -0.1398e-05, -0.1754e-05, -0.2184e-05, -0.2763e-05, -0.3581e-05, &
        -0.4739e-05, -0.6341e-05, -0.8484e-05, -0.1128e-04, -0.1490e-04, &
        -0.1955e-04, -0.2561e-04, -0.3364e-04, -0.4438e-04, -0.5881e-04, &
        -0.7822e-04, -0.1045e-03, -0.1401e-03, -0.1884e-03, -0.2523e-03, &
        -0.3335e-03, -0.4289e-03, -0.5296e-03, -0.6231e-03, -0.6980e-03, &
        -0.7406e-03/
      data ((h13(ip,iw),iw=1,31), ip= 9, 9)/ &
         0.5334e-09,  0.7859e-09,  0.1043e-08,  0.1279e-08,  0.1460e-08, &
         0.1560e-08,  0.1618e-08,  0.1657e-08,  0.1912e-08,  0.2569e-08, &
         0.3654e-08,  0.5509e-08,  0.8964e-08,  0.1518e-07,  0.2560e-07, &
         0.4178e-07,  0.6574e-07,  0.9958e-07,  0.1449e-06,  0.2031e-06, &
         0.2766e-06,  0.3718e-06,  0.5022e-06,  0.6849e-06,  0.9360e-06, &
         0.1268e-05,  0.1683e-05,  0.2157e-05,  0.2625e-05,  0.3020e-05, &
         0.3364e-05/
      data ((h11(ip,iw),iw=1,31), ip=10,10)/ &
         0.99993336,  0.99989021,  0.99982840,  0.99974459,  0.99963468, &
         0.99949121,  0.99930137,  0.99904430,  0.99868703,  0.99818403, &
         0.99747300,  0.99646801,  0.99505299,  0.99307102,  0.99030602, &
         0.98645997,  0.98111999,  0.97372001,  0.96353000,  0.94957000, &
         0.93058997,  0.90486002,  0.87029999,  0.82449996,  0.76530004, &
         0.69159997,  0.60380000,  0.50529999,  0.40259999,  0.30269998, &
         0.21020001/
      data ((h12(ip,iw),iw=1,31), ip=10,10)/ &
        -0.1742e-06, -0.3134e-06, -0.5121e-06, -0.7619e-06, -0.1048e-05, &
        -0.1364e-05, -0.1725e-05, -0.2177e-05, -0.2801e-05, -0.3694e-05, &
        -0.4969e-05, -0.6748e-05, -0.9161e-05, -0.1236e-04, -0.1655e-04, &
        -0.2203e-04, -0.2927e-04, -0.3894e-04, -0.5192e-04, -0.6936e-04, &
        -0.9294e-04, -0.1250e-03, -0.1686e-03, -0.2271e-03, -0.3027e-03, &
        -0.3944e-03, -0.4951e-03, -0.5928e-03, -0.6755e-03, -0.7309e-03, &
        -0.7417e-03/
      data ((h13(ip,iw),iw=1,31), ip=10,10)/ &
         0.5179e-09,  0.7789e-09,  0.1071e-08,  0.1382e-08,  0.1690e-08, &
         0.1979e-08,  0.2297e-08,  0.2704e-08,  0.3466e-08,  0.4794e-08, &
         0.6746e-08,  0.9739e-08,  0.1481e-07,  0.2331e-07,  0.3679e-07, &
         0.5726e-07,  0.8716e-07,  0.1289e-06,  0.1837e-06,  0.2534e-06, &
         0.3424e-06,  0.4609e-06,  0.6245e-06,  0.8495e-06,  0.1151e-05, &
         0.1536e-05,  0.1991e-05,  0.2468e-05,  0.2891e-05,  0.3245e-05, &
         0.3580e-05/
      data ((h11(ip,iw),iw=1,31), ip=11,11)/ &
         0.99993110,  0.99988490,  0.99981719,  0.99972337,  0.99959719, &
         0.99942869,  0.99920130,  0.99888903,  0.99845201,  0.99783301, &
         0.99695599,  0.99571502,  0.99396503,  0.99150997,  0.98808002, &
         0.98329997,  0.97667003,  0.96750998,  0.95494002,  0.93779999, &
         0.91453999,  0.88319999,  0.84130001,  0.78689998,  0.71799999, &
         0.63470000,  0.53909999,  0.43699998,  0.33550000,  0.24010003, &
         0.15420002/
      data ((h12(ip,iw),iw=1,31), ip=11,11)/ &
        -0.1647e-06, -0.2974e-06, -0.4900e-06, -0.7358e-06, -0.1022e-05, &
        -0.1344e-05, -0.1721e-05, -0.2212e-05, -0.2901e-05, -0.3896e-05, &
        -0.5327e-05, -0.7342e-05, -0.1011e-04, -0.1382e-04, -0.1875e-04, &
        -0.2530e-04, -0.3403e-04, -0.4573e-04, -0.6145e-04, -0.8264e-04, &
        -0.1114e-03, -0.1507e-03, -0.2039e-03, -0.2737e-03, -0.3607e-03, &
        -0.4599e-03, -0.5604e-03, -0.6497e-03, -0.7161e-03, -0.7443e-03, &
        -0.7133e-03/
      data ((h13(ip,iw),iw=1,31), ip=11,11)/ &
         0.5073e-09,  0.7906e-09,  0.1134e-08,  0.1560e-08,  0.2046e-08, &
         0.2589e-08,  0.3254e-08,  0.4107e-08,  0.5481e-08,  0.7602e-08, &
         0.1059e-07,  0.1501e-07,  0.2210e-07,  0.3334e-07,  0.5055e-07, &
         0.7629e-07,  0.1134e-06,  0.1642e-06,  0.2298e-06,  0.3133e-06, &
         0.4225e-06,  0.5709e-06,  0.7739e-06,  0.1047e-05,  0.1401e-05, &
         0.1833e-05,  0.2308e-05,  0.2753e-05,  0.3125e-05,  0.3467e-05, &
         0.3748e-05/
      data ((h11(ip,iw),iw=1,31), ip=12,12)/ &
         0.99992824,  0.99987793,  0.99980247,  0.99969512,  0.99954712, &
         0.99934530,  0.99906880,  0.99868500,  0.99814498,  0.99738002, &
         0.99629498,  0.99475700,  0.99258602,  0.98953998,  0.98527998, &
         0.97934997,  0.97112000,  0.95981002,  0.94433999,  0.92332000, &
         0.89490002,  0.85680002,  0.80680001,  0.74290001,  0.66420001, &
         0.57220000,  0.47149998,  0.36900002,  0.27109998,  0.18159997, &
         0.10460001/
      data ((h12(ip,iw),iw=1,31), ip=12,12)/ &
        -0.1548e-06, -0.2808e-06, -0.4683e-06, -0.7142e-06, -0.1008e-05, &
        -0.1347e-05, -0.1758e-05, -0.2306e-05, -0.3083e-05, -0.4214e-05, &
        -0.5851e-05, -0.8175e-05, -0.1140e-04, -0.1577e-04, -0.2166e-04, &
        -0.2955e-04, -0.4014e-04, -0.5434e-04, -0.7343e-04, -0.9931e-04, &
        -0.1346e-03, -0.1826e-03, -0.2467e-03, -0.3283e-03, -0.4246e-03, &
        -0.5264e-03, -0.6211e-03, -0.6970e-03, -0.7402e-03, -0.7316e-03, &
        -0.6486e-03/
      data ((h13(ip,iw),iw=1,31), ip=12,12)/ &
         0.5078e-09,  0.8244e-09,  0.1255e-08,  0.1826e-08,  0.2550e-08, &
         0.3438e-08,  0.4532e-08,  0.5949e-08,  0.8041e-08,  0.1110e-07, &
         0.1534e-07,  0.2157e-07,  0.3116e-07,  0.4570e-07,  0.6747e-07, &
         0.9961e-07,  0.1451e-06,  0.2061e-06,  0.2843e-06,  0.3855e-06, &
         0.5213e-06,  0.7060e-06,  0.9544e-06,  0.1280e-05,  0.1684e-05, &
         0.2148e-05,  0.2609e-05,  0.3002e-05,  0.3349e-05,  0.3670e-05, &
         0.3780e-05/
      data ((h11(ip,iw),iw=1,31), ip=13,13)/ &
         0.99992472,  0.99986941,  0.99978399,  0.99965900,  0.99948251, &
         0.99923742,  0.99889702,  0.99842298,  0.99775398,  0.99680400, &
         0.99545598,  0.99354500,  0.99084800,  0.98706001,  0.98176998, &
         0.97439998,  0.96423000,  0.95029002,  0.93129998,  0.90557003, &
         0.87099999,  0.82520002,  0.76600003,  0.69220001,  0.60440004, &
         0.50580001,  0.40310001,  0.30299997,  0.21039999,  0.12860000, &
         0.06360000/
      data ((h12(ip,iw),iw=1,31), ip=13,13)/ &
        -0.1461e-06, -0.2663e-06, -0.4512e-06, -0.7027e-06, -0.1014e-05, &
        -0.1387e-05, -0.1851e-05, -0.2478e-05, -0.3373e-05, -0.4682e-05, &
        -0.6588e-05, -0.9311e-05, -0.1311e-04, -0.1834e-04, -0.2544e-04, &
        -0.3502e-04, -0.4789e-04, -0.6515e-04, -0.8846e-04, -0.1202e-03, &
        -0.1635e-03, -0.2217e-03, -0.2975e-03, -0.3897e-03, -0.4913e-03, &
        -0.5902e-03, -0.6740e-03, -0.7302e-03, -0.7415e-03, -0.6858e-03, &
        -0.5447e-03/
      data ((h13(ip,iw),iw=1,31), ip=13,13)/ &
         0.5236e-09,  0.8873e-09,  0.1426e-08,  0.2193e-08,  0.3230e-08, &
         0.4555e-08,  0.6200e-08,  0.8298e-08,  0.1126e-07,  0.1544e-07, &
         0.2130e-07,  0.2978e-07,  0.4239e-07,  0.6096e-07,  0.8829e-07, &
         0.1280e-06,  0.1830e-06,  0.2555e-06,  0.3493e-06,  0.4740e-06, &
         0.6431e-06,  0.8701e-06,  0.1169e-05,  0.1547e-05,  0.1992e-05, &
         0.2460e-05,  0.2877e-05,  0.3230e-05,  0.3569e-05,  0.3782e-05, &
         0.3591e-05/
      data ((h11(ip,iw),iw=1,31), ip=14,14)/ &
         0.99992090,  0.99985969,  0.99976218,  0.99961531,  0.99940270, &
         0.99910218,  0.99868101,  0.99809098,  0.99725902,  0.99607700, &
         0.99440002,  0.99202299,  0.98866999,  0.98395997,  0.97737998, &
         0.96825999,  0.95570999,  0.93857002,  0.91531003,  0.88389999, &
         0.84210002,  0.78759998,  0.71869999,  0.63530004,  0.53970003, &
         0.43750000,  0.33590001,  0.24040002,  0.15439999,  0.08300000, &
         0.03299999/
      data ((h12(ip,iw),iw=1,31), ip=14,14)/ &
        -0.1402e-06, -0.2569e-06, -0.4428e-06, -0.7076e-06, -0.1051e-05, &
        -0.1478e-05, -0.2019e-05, -0.2752e-05, -0.3802e-05, -0.5343e-05, &
        -0.7594e-05, -0.1082e-04, -0.1536e-04, -0.2166e-04, -0.3028e-04, &
        -0.4195e-04, -0.5761e-04, -0.7867e-04, -0.1072e-03, -0.1462e-03, &
        -0.1990e-03, -0.2687e-03, -0.3559e-03, -0.4558e-03, -0.5572e-03, &
        -0.6476e-03, -0.7150e-03, -0.7439e-03, -0.7133e-03, -0.6015e-03, &
        -0.4089e-03/
      data ((h13(ip,iw),iw=1,31), ip=14,14)/ &
         0.5531e-09,  0.9757e-09,  0.1644e-08,  0.2650e-08,  0.4074e-08, &
         0.5957e-08,  0.8314e-08,  0.1128e-07,  0.1528e-07,  0.2087e-07, &
         0.2874e-07,  0.4002e-07,  0.5631e-07,  0.7981e-07,  0.1139e-06, &
         0.1621e-06,  0.2275e-06,  0.3136e-06,  0.4280e-06,  0.5829e-06, &
         0.7917e-06,  0.1067e-05,  0.1419e-05,  0.1844e-05,  0.2310e-05, &
         0.2747e-05,  0.3113e-05,  0.3455e-05,  0.3739e-05,  0.3715e-05, &
         0.3125e-05/
      data ((h11(ip,iw),iw=1,31), ip=15,15)/ &
         0.99991709,  0.99984968,  0.99973857,  0.99956548,  0.99930853, &
         0.99893898,  0.99841601,  0.99768001,  0.99664098,  0.99516898, &
         0.99308002,  0.99012297,  0.98594999,  0.98009998,  0.97194999, &
         0.96066999,  0.94523001,  0.92421001,  0.89579999,  0.85769999, &
         0.80760002,  0.74360001,  0.66490000,  0.57290000,  0.47200000, &
         0.36940002,  0.27139997,  0.18180001,  0.10479999,  0.04699999, &
         0.01359999/
      data ((h12(ip,iw),iw=1,31), ip=15,15)/ &
        -0.1378e-06, -0.2542e-06, -0.4461e-06, -0.7333e-06, -0.1125e-05, &
        -0.1630e-05, -0.2281e-05, -0.3159e-05, -0.4410e-05, -0.6246e-05, &
        -0.8933e-05, -0.1280e-04, -0.1826e-04, -0.2589e-04, -0.3639e-04, &
        -0.5059e-04, -0.6970e-04, -0.9552e-04, -0.1307e-03, -0.1784e-03, &
        -0.2422e-03, -0.3237e-03, -0.4203e-03, -0.5227e-03, -0.6184e-03, &
        -0.6953e-03, -0.7395e-03, -0.7315e-03, -0.6487e-03, -0.4799e-03, &
        -0.2625e-03/
      data ((h13(ip,iw),iw=1,31), ip=15,15)/ &
         0.5891e-09,  0.1074e-08,  0.1885e-08,  0.3167e-08,  0.5051e-08, &
         0.7631e-08,  0.1092e-07,  0.1500e-07,  0.2032e-07,  0.2769e-07, &
         0.3810e-07,  0.5279e-07,  0.7361e-07,  0.1032e-06,  0.1450e-06, &
         0.2026e-06,  0.2798e-06,  0.3832e-06,  0.5242e-06,  0.7159e-06, &
         0.9706e-06,  0.1299e-05,  0.1701e-05,  0.2159e-05,  0.2612e-05, &
         0.2998e-05,  0.3341e-05,  0.3661e-05,  0.3775e-05,  0.3393e-05, &
         0.2384e-05/
      data ((h11(ip,iw),iw=1,31), ip=16,16)/ &
         0.99991363,  0.99984020,  0.99971467,  0.99951237,  0.99920303, &
         0.99874902,  0.99809903,  0.99717999,  0.99588197,  0.99404502, &
         0.99144298,  0.98776001,  0.98258001,  0.97533000,  0.96524000, &
         0.95135999,  0.93241000,  0.90667999,  0.87199998,  0.82620001, &
         0.76700002,  0.69309998,  0.60510004,  0.50650001,  0.40359998, &
         0.30350000,  0.21069998,  0.12870002,  0.06370002,  0.02200001, &
         0.00389999/
      data ((h12(ip,iw),iw=1,31), ip=16,16)/ &
        -0.1383e-06, -0.2577e-06, -0.4608e-06, -0.7793e-06, -0.1237e-05, &
        -0.1850e-05, -0.2652e-05, -0.3728e-05, -0.5244e-05, -0.7451e-05, &
        -0.1067e-04, -0.1532e-04, -0.2193e-04, -0.3119e-04, -0.4395e-04, &
        -0.6126e-04, -0.8466e-04, -0.1164e-03, -0.1596e-03, -0.2177e-03, &
        -0.2933e-03, -0.3855e-03, -0.4874e-03, -0.5870e-03, -0.6718e-03, &
        -0.7290e-03, -0.7411e-03, -0.6859e-03, -0.5450e-03, -0.3353e-03, &
        -0.1363e-03/
      data ((h13(ip,iw),iw=1,31), ip=16,16)/ &
         0.6217e-09,  0.1165e-08,  0.2116e-08,  0.3685e-08,  0.6101e-08, &
         0.9523e-08,  0.1400e-07,  0.1959e-07,  0.2668e-07,  0.3629e-07, &
         0.4982e-07,  0.6876e-07,  0.9523e-07,  0.1321e-06,  0.1825e-06, &
         0.2505e-06,  0.3420e-06,  0.4677e-06,  0.6416e-06,  0.8760e-06, &
         0.1183e-05,  0.1565e-05,  0.2010e-05,  0.2472e-05,  0.2882e-05, &
         0.3229e-05,  0.3564e-05,  0.3777e-05,  0.3589e-05,  0.2786e-05, &
         0.1487e-05/
      data ((h11(ip,iw),iw=1,31), ip=17,17)/ &
         0.99991077,  0.99983180,  0.99969262,  0.99945968,  0.99909151, &
         0.99853700,  0.99773198,  0.99658400,  0.99496001,  0.99266702, &
         0.98943001,  0.98484999,  0.97842997,  0.96945000,  0.95703000, &
         0.93998998,  0.91676998,  0.88540000,  0.84350002,  0.78890002, &
         0.71990001,  0.63639998,  0.54060000,  0.43820000,  0.33639997, &
         0.24080002,  0.15460002,  0.08310002,  0.03310001,  0.00770003, &
         0.00050002/
      data ((h12(ip,iw),iw=1,31), ip=17,17)/ &
        -0.1405e-06, -0.2649e-06, -0.4829e-06, -0.8398e-06, -0.1379e-05, &
        -0.2132e-05, -0.3138e-05, -0.4487e-05, -0.6353e-05, -0.9026e-05, &
        -0.1290e-04, -0.1851e-04, -0.2650e-04, -0.3772e-04, -0.5319e-04, &
        -0.7431e-04, -0.1031e-03, -0.1422e-03, -0.1951e-03, -0.2648e-03, &
        -0.3519e-03, -0.4518e-03, -0.5537e-03, -0.6449e-03, -0.7133e-03, &
        -0.7432e-03, -0.7133e-03, -0.6018e-03, -0.4092e-03, -0.1951e-03, &
        -0.5345e-04/
      data ((h13(ip,iw),iw=1,31), ip=17,17)/ &
         0.6457e-09,  0.1235e-08,  0.2303e-08,  0.4149e-08,  0.7120e-08, &
         0.1152e-07,  0.1749e-07,  0.2508e-07,  0.3462e-07,  0.4718e-07, &
         0.6452e-07,  0.8874e-07,  0.1222e-06,  0.1675e-06,  0.2276e-06, &
         0.3076e-06,  0.4174e-06,  0.5714e-06,  0.7837e-06,  0.1067e-05, &
         0.1428e-05,  0.1859e-05,  0.2327e-05,  0.2760e-05,  0.3122e-05, &
         0.3458e-05,  0.3739e-05,  0.3715e-05,  0.3126e-05,  0.1942e-05, &
         0.6977e-06/
      data ((h11(ip,iw),iw=1,31), ip=18,18)/ &
         0.99990851,  0.99982500,  0.99967349,  0.99941093,  0.99897999, &
         0.99831200,  0.99732101,  0.99589097,  0.99386197,  0.99099803, &
         0.98695999,  0.98128998,  0.97333002,  0.96227002,  0.94700998, &
         0.92614001,  0.89779997,  0.85969996,  0.80949998,  0.74540001, &
         0.66649997,  0.57420003,  0.47310001,  0.37029999,  0.27200001, &
         0.18220001,  0.10500002,  0.04710001,  0.01359999,  0.00169998, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=18,18)/ &
        -0.1431e-06, -0.2731e-06, -0.5072e-06, -0.9057e-06, -0.1537e-05, &
        -0.2460e-05, -0.3733e-05, -0.5449e-05, -0.7786e-05, -0.1106e-04, &
        -0.1574e-04, -0.2249e-04, -0.3212e-04, -0.4564e-04, -0.6438e-04, &
        -0.9019e-04, -0.1256e-03, -0.1737e-03, -0.2378e-03, -0.3196e-03, &
        -0.4163e-03, -0.5191e-03, -0.6154e-03, -0.6931e-03, -0.7384e-03, &
        -0.7313e-03, -0.6492e-03, -0.4805e-03, -0.2629e-03, -0.8897e-04, &
        -0.1432e-04/
      data ((h13(ip,iw),iw=1,31), ip=18,18)/ &
         0.6607e-09,  0.1282e-08,  0.2441e-08,  0.4522e-08,  0.8027e-08, &
         0.1348e-07,  0.2122e-07,  0.3139e-07,  0.4435e-07,  0.6095e-07, &
         0.8319e-07,  0.1139e-06,  0.1557e-06,  0.2107e-06,  0.2819e-06, &
         0.3773e-06,  0.5107e-06,  0.6982e-06,  0.9542e-06,  0.1290e-05, &
         0.1703e-05,  0.2170e-05,  0.2628e-05,  0.3013e-05,  0.3352e-05, &
         0.3669e-05,  0.3780e-05,  0.3397e-05,  0.2386e-05,  0.1062e-05, &
         0.2216e-06/
      data ((h11(ip,iw),iw=1,31), ip=19,19)/ &
         0.99990678,  0.99981970,  0.99965781,  0.99936831,  0.99887598, &
         0.99808502,  0.99687898,  0.99510998,  0.99257898,  0.98900002, &
         0.98398000,  0.97693998,  0.96711999,  0.95353001,  0.93484998, &
         0.90934002,  0.87479997,  0.82900000,  0.76960003,  0.69550002, &
         0.60720003,  0.50819999,  0.40490001,  0.30440003,  0.21130002, &
         0.12910002,  0.06389999,  0.02200001,  0.00389999,  0.00010002, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=19,19)/ &
        -0.1454e-06, -0.2805e-06, -0.5296e-06, -0.9685e-06, -0.1695e-05, &
        -0.2812e-05, -0.4412e-05, -0.6606e-05, -0.9573e-05, -0.1363e-04, &
        -0.1932e-04, -0.2743e-04, -0.3897e-04, -0.5520e-04, -0.7787e-04, &
        -0.1094e-03, -0.1529e-03, -0.2117e-03, -0.2880e-03, -0.3809e-03, &
        -0.4834e-03, -0.5836e-03, -0.6692e-03, -0.7275e-03, -0.7408e-03, &
        -0.6865e-03, -0.5459e-03, -0.3360e-03, -0.1365e-03, -0.2935e-04, &
        -0.2173e-05/
      data ((h13(ip,iw),iw=1,31), ip=19,19)/ &
         0.6693e-09,  0.1312e-08,  0.2538e-08,  0.4802e-08,  0.8778e-08, &
         0.1528e-07,  0.2501e-07,  0.3836e-07,  0.5578e-07,  0.7806e-07, &
         0.1069e-06,  0.1456e-06,  0.1970e-06,  0.2631e-06,  0.3485e-06, &
         0.4642e-06,  0.6268e-06,  0.8526e-06,  0.1157e-05,  0.1545e-05, &
         0.2002e-05,  0.2478e-05,  0.2897e-05,  0.3245e-05,  0.3578e-05, &
         0.3789e-05,  0.3598e-05,  0.2792e-05,  0.1489e-05,  0.4160e-06, &
         0.3843e-07/
      data ((h11(ip,iw),iw=1,31), ip=20,20)/ &
         0.99990559,  0.99981570,  0.99964547,  0.99933308,  0.99878299, &
         0.99786699,  0.99642301,  0.99425799,  0.99111998,  0.98667002, &
         0.98041999,  0.97170001,  0.95960999,  0.94295001,  0.92012000, &
         0.88900000,  0.84740001,  0.79280001,  0.72360003,  0.63960004, &
         0.54330003,  0.44029999,  0.33800000,  0.24190003,  0.15530002, &
         0.08350003,  0.03320003,  0.00770003,  0.00050002,  0.00000000, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=20,20)/ &
        -0.1472e-06, -0.2866e-06, -0.5485e-06, -0.1024e-05, -0.1842e-05, &
        -0.3160e-05, -0.5136e-05, -0.7922e-05, -0.1171e-04, -0.1682e-04, &
        -0.2381e-04, -0.3355e-04, -0.4729e-04, -0.6673e-04, -0.9417e-04, &
        -0.1327e-03, -0.1858e-03, -0.2564e-03, -0.3449e-03, -0.4463e-03, &
        -0.5495e-03, -0.6420e-03, -0.7116e-03, -0.7427e-03, -0.7139e-03, &
        -0.6031e-03, -0.4104e-03, -0.1957e-03, -0.5358e-04, -0.6176e-05, &
        -0.1347e-06/
      data ((h13(ip,iw),iw=1,31), ip=20,20)/ &
         0.6750e-09,  0.1332e-08,  0.2602e-08,  0.5003e-08,  0.9367e-08, &
         0.1684e-07,  0.2863e-07,  0.4566e-07,  0.6865e-07,  0.9861e-07, &
         0.1368e-06,  0.1856e-06,  0.2479e-06,  0.3274e-06,  0.4315e-06, &
         0.5739e-06,  0.7710e-06,  0.1040e-05,  0.1394e-05,  0.1829e-05, &
         0.2309e-05,  0.2759e-05,  0.3131e-05,  0.3472e-05,  0.3755e-05, &
         0.3730e-05,  0.3138e-05,  0.1948e-05,  0.6994e-06,  0.1022e-06, &
         0.2459e-08/
      data ((h11(ip,iw),iw=1,31), ip=21,21)/ &
         0.99990469,  0.99981278,  0.99963617,  0.99930513,  0.99870503, &
         0.99766999,  0.99597800,  0.99336702,  0.98951000,  0.98399001, &
         0.97622001,  0.96543998,  0.95059001,  0.93019998,  0.90235001, &
         0.86470002,  0.81480002,  0.75059998,  0.67129999,  0.57840002, &
         0.47659999,  0.37279999,  0.27389997,  0.18339998,  0.10570002, &
         0.04740000,  0.01370001,  0.00169998,  0.00000000,  0.00000000, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=21,21)/ &
        -0.1487e-06, -0.2912e-06, -0.5636e-06, -0.1069e-05, -0.1969e-05, &
        -0.3483e-05, -0.5858e-05, -0.9334e-05, -0.1416e-04, -0.2067e-04, &
        -0.2936e-04, -0.4113e-04, -0.5750e-04, -0.8072e-04, -0.1139e-03, &
        -0.1606e-03, -0.2246e-03, -0.3076e-03, -0.4067e-03, -0.5121e-03, &
        -0.6110e-03, -0.6909e-03, -0.7378e-03, -0.7321e-03, -0.6509e-03, &
        -0.4825e-03, -0.2641e-03, -0.8936e-04, -0.1436e-04, -0.5966e-06, &
         0.0000e+00/
      data ((h13(ip,iw),iw=1,31), ip=21,21)/ &
         0.6777e-09,  0.1344e-08,  0.2643e-08,  0.5138e-08,  0.9798e-08, &
         0.1809e-07,  0.3185e-07,  0.5285e-07,  0.8249e-07,  0.1222e-06, &
         0.1730e-06,  0.2351e-06,  0.3111e-06,  0.4078e-06,  0.5366e-06, &
         0.7117e-06,  0.9495e-06,  0.1266e-05,  0.1667e-05,  0.2132e-05, &
         0.2600e-05,  0.3001e-05,  0.3354e-05,  0.3679e-05,  0.3796e-05, &
         0.3414e-05,  0.2399e-05,  0.1067e-05,  0.2222e-06,  0.1075e-07, &
         0.0000e+00/
      data ((h11(ip,iw),iw=1,31), ip=22,22)/ &
         0.99990410,  0.99981070,  0.99962938,  0.99928379,  0.99864298, &
         0.99750000,  0.99556601,  0.99247700,  0.98780000,  0.98100001, &
         0.97140002,  0.95810002,  0.93984997,  0.91491997,  0.88110000, &
         0.83570004,  0.77670002,  0.70239997,  0.61350000,  0.51349998, &
         0.40910000,  0.30750000,  0.21340001,  0.13040000,  0.06449997, &
         0.02219999,  0.00389999,  0.00010002,  0.00000000,  0.00000000, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=22,22)/ &
        -0.1496e-06, -0.2947e-06, -0.5749e-06, -0.1105e-05, -0.2074e-05, &
        -0.3763e-05, -0.6531e-05, -0.1076e-04, -0.1682e-04, -0.2509e-04, &
        -0.3605e-04, -0.5049e-04, -0.7012e-04, -0.9787e-04, -0.1378e-03, &
        -0.1939e-03, -0.2695e-03, -0.3641e-03, -0.4703e-03, -0.5750e-03, &
        -0.6648e-03, -0.7264e-03, -0.7419e-03, -0.6889e-03, -0.5488e-03, &
        -0.3382e-03, -0.1375e-03, -0.2951e-04, -0.2174e-05,  0.0000e+00, &
         0.0000e+00/
      data ((h13(ip,iw),iw=1,31), ip=22,22)/ &
         0.6798e-09,  0.1350e-08,  0.2667e-08,  0.5226e-08,  0.1010e-07, &
         0.1903e-07,  0.3455e-07,  0.5951e-07,  0.9658e-07,  0.1479e-06, &
         0.2146e-06,  0.2951e-06,  0.3903e-06,  0.5101e-06,  0.6693e-06, &
         0.8830e-06,  0.1168e-05,  0.1532e-05,  0.1968e-05,  0.2435e-05, &
         0.2859e-05,  0.3222e-05,  0.3572e-05,  0.3797e-05,  0.3615e-05, &
         0.2811e-05,  0.1500e-05,  0.4185e-06,  0.3850e-07,  0.0000e+00, &
         0.0000e+00/
      data ((h11(ip,iw),iw=1,31), ip=23,23)/ &
         0.99990374,  0.99980932,  0.99962449,  0.99926788,  0.99859399, &
         0.99736100,  0.99520397,  0.99163198,  0.98606002,  0.97779000, &
         0.96600002,  0.94963002,  0.92720997,  0.89670002,  0.85580003, &
         0.80170000,  0.73269999,  0.64840001,  0.55110002,  0.44669998, &
         0.34280002,  0.24529999,  0.15750003,  0.08469999,  0.03369999, &
         0.00779998,  0.00050002,  0.00000000,  0.00000000,  0.00000000, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=23,23)/ &
        -0.1503e-06, -0.2971e-06, -0.5832e-06, -0.1131e-05, -0.2154e-05, &
        -0.3992e-05, -0.7122e-05, -0.1211e-04, -0.1954e-04, -0.2995e-04, &
        -0.4380e-04, -0.6183e-04, -0.8577e-04, -0.1191e-03, -0.1668e-03, &
        -0.2333e-03, -0.3203e-03, -0.4237e-03, -0.5324e-03, -0.6318e-03, &
        -0.7075e-03, -0.7429e-03, -0.7168e-03, -0.6071e-03, -0.4139e-03, &
        -0.1976e-03, -0.5410e-04, -0.6215e-05, -0.1343e-06,  0.0000e+00, &
         0.0000e+00/
      data ((h13(ip,iw),iw=1,31), ip=23,23)/ &
         0.6809e-09,  0.1356e-08,  0.2683e-08,  0.5287e-08,  0.1030e-07, &
         0.1971e-07,  0.3665e-07,  0.6528e-07,  0.1100e-06,  0.1744e-06, &
         0.2599e-06,  0.3650e-06,  0.4887e-06,  0.6398e-06,  0.8358e-06, &
         0.1095e-05,  0.1429e-05,  0.1836e-05,  0.2286e-05,  0.2716e-05, &
         0.3088e-05,  0.3444e-05,  0.3748e-05,  0.3740e-05,  0.3157e-05, &
         0.1966e-05,  0.7064e-06,  0.1030e-06,  0.2456e-08,  0.0000e+00, &
         0.0000e+00/
      data ((h11(ip,iw),iw=1,31), ip=24,24)/ &
         0.99990344,  0.99980831,  0.99962109,  0.99925637,  0.99855798, &
         0.99725199,  0.99489999,  0.99087203,  0.98436999,  0.97447002, &
         0.96012998,  0.94006002,  0.91254002,  0.87540001,  0.82609999, &
         0.76240003,  0.68299997,  0.58930004,  0.48589998,  0.38020003, &
         0.27920002,  0.18699998,  0.10769999,  0.04830003,  0.01400000, &
         0.00169998,  0.00000000,  0.00000000,  0.00000000,  0.00000000, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=24,24)/ &
        -0.1508e-06, -0.2989e-06, -0.5892e-06, -0.1151e-05, -0.2216e-05, &
        -0.4175e-05, -0.7619e-05, -0.1333e-04, -0.2217e-04, -0.3497e-04, &
        -0.5238e-04, -0.7513e-04, -0.1049e-03, -0.1455e-03, -0.2021e-03, &
        -0.2790e-03, -0.3757e-03, -0.4839e-03, -0.5902e-03, -0.6794e-03, &
        -0.7344e-03, -0.7341e-03, -0.6557e-03, -0.4874e-03, -0.2674e-03, &
        -0.9059e-04, -0.1455e-04, -0.5986e-06,  0.0000e+00,  0.0000e+00, &
         0.0000e+00/
      data ((h13(ip,iw),iw=1,31), ip=24,24)/ &
         0.6812e-09,  0.1356e-08,  0.2693e-08,  0.5328e-08,  0.1045e-07, &
         0.2021e-07,  0.3826e-07,  0.6994e-07,  0.1218e-06,  0.1997e-06, &
         0.3069e-06,  0.4428e-06,  0.6064e-06,  0.8015e-06,  0.1043e-05, &
         0.1351e-05,  0.1733e-05,  0.2168e-05,  0.2598e-05,  0.2968e-05, &
         0.3316e-05,  0.3662e-05,  0.3801e-05,  0.3433e-05,  0.2422e-05, &
         0.1081e-05,  0.2256e-06,  0.1082e-07,  0.0000e+00,  0.0000e+00, &
         0.0000e+00/
      data ((h11(ip,iw),iw=1,31), ip=25,25)/ &
         0.99990326,  0.99980772,  0.99961871,  0.99924821,  0.99853098, &
         0.99716800,  0.99465698,  0.99022102,  0.98281002,  0.97118002, &
         0.95393997,  0.92948997,  0.89579999,  0.85070002,  0.79189998, &
         0.71759999,  0.62800002,  0.52639997,  0.41970003,  0.31559998, &
         0.21899998,  0.13370001,  0.06610000,  0.02280003,  0.00400001, &
         0.00010002,  0.00000000,  0.00000000,  0.00000000,  0.00000000, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=25,25)/ &
        -0.1511e-06, -0.3001e-06, -0.5934e-06, -0.1166e-05, -0.2263e-05, &
        -0.4319e-05, -0.8028e-05, -0.1438e-04, -0.2460e-04, -0.3991e-04, &
        -0.6138e-04, -0.9005e-04, -0.1278e-03, -0.1778e-03, -0.2447e-03, &
        -0.3313e-03, -0.4342e-03, -0.5424e-03, -0.6416e-03, -0.7146e-03, &
        -0.7399e-03, -0.6932e-03, -0.5551e-03, -0.3432e-03, -0.1398e-03, &
        -0.3010e-04, -0.2229e-05,  0.0000e+00,  0.0000e+00,  0.0000e+00, &
         0.0000e+00/
      data ((h13(ip,iw),iw=1,31), ip=25,25)/ &
         0.6815e-09,  0.1358e-08,  0.2698e-08,  0.5355e-08,  0.1054e-07, &
         0.2056e-07,  0.3942e-07,  0.7349e-07,  0.1315e-06,  0.2226e-06, &
         0.3537e-06,  0.5266e-06,  0.7407e-06,  0.9958e-06,  0.1296e-05, &
         0.1657e-05,  0.2077e-05,  0.2512e-05,  0.2893e-05,  0.3216e-05, &
         0.3562e-05,  0.3811e-05,  0.3644e-05,  0.2841e-05,  0.1524e-05, &
         0.4276e-06,  0.3960e-07,  0.0000e+00,  0.0000e+00,  0.0000e+00, &
         0.0000e+00/
      data ((h11(ip,iw),iw=1,31), ip=26,26)/ &
         0.99990320,  0.99980718,  0.99961710,  0.99924242,  0.99851102, &
         0.99710602,  0.99446702,  0.98969001,  0.98144001,  0.96805000, &
         0.94762999,  0.91812998,  0.87730002,  0.82290000,  0.75319999, &
         0.66789997,  0.56879997,  0.46160001,  0.35450000,  0.25370002, &
         0.16280001,  0.08740002,  0.03479999,  0.00809997,  0.00059998, &
         0.00000000,  0.00000000,  0.00000000,  0.00000000,  0.00000000, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=26,26)/ &
        -0.1513e-06, -0.3009e-06, -0.5966e-06, -0.1176e-05, -0.2299e-05, &
        -0.4430e-05, -0.8352e-05, -0.1526e-04, -0.2674e-04, -0.4454e-04, &
        -0.7042e-04, -0.1062e-03, -0.1540e-03, -0.2163e-03, -0.2951e-03, &
        -0.3899e-03, -0.4948e-03, -0.5983e-03, -0.6846e-03, -0.7332e-03, &
        -0.7182e-03, -0.6142e-03, -0.4209e-03, -0.2014e-03, -0.5530e-04, &
        -0.6418e-05, -0.1439e-06,  0.0000e+00,  0.0000e+00,  0.0000e+00, &
         0.0000e+00/
      data ((h13(ip,iw),iw=1,31), ip=26,26)/ &
         0.6817e-09,  0.1359e-08,  0.2702e-08,  0.5374e-08,  0.1061e-07, &
         0.2079e-07,  0.4022e-07,  0.7610e-07,  0.1392e-06,  0.2428e-06, &
         0.3992e-06,  0.6149e-06,  0.8893e-06,  0.1220e-05,  0.1599e-05, &
         0.2015e-05,  0.2453e-05,  0.2853e-05,  0.3173e-05,  0.3488e-05, &
         0.3792e-05,  0.3800e-05,  0.3210e-05,  0.2002e-05,  0.7234e-06, &
         0.1068e-06,  0.2646e-08,  0.0000e+00,  0.0000e+00,  0.0000e+00, &
         0.0000e+00/
      data ((h21(ip,iw),iw=1,31), ip= 1, 1)/ &
         0.99999607,  0.99999237,  0.99998546,  0.99997294,  0.99995142, &
         0.99991685,  0.99986511,  0.99979371,  0.99970162,  0.99958909, &
         0.99945778,  0.99931037,  0.99914628,  0.99895900,  0.99873799, &
         0.99846601,  0.99813002,  0.99771398,  0.99719697,  0.99655598, &
         0.99575800,  0.99475598,  0.99348903,  0.99186200,  0.98973000, &
         0.98688000,  0.98303002,  0.97777998,  0.97059000,  0.96077001, &
         0.94742000/
      data ((h22(ip,iw),iw=1,31), ip= 1, 1)/ &
        -0.5622e-07, -0.1071e-06, -0.1983e-06, -0.3533e-06, -0.5991e-06, &
        -0.9592e-06, -0.1444e-05, -0.2049e-05, -0.2764e-05, -0.3577e-05, &
        -0.4469e-05, -0.5467e-05, -0.6654e-05, -0.8137e-05, -0.1002e-04, &
        -0.1237e-04, -0.1528e-04, -0.1884e-04, -0.2310e-04, -0.2809e-04, &
        -0.3396e-04, -0.4098e-04, -0.4960e-04, -0.6058e-04, -0.7506e-04, &
        -0.9451e-04, -0.1207e-03, -0.1558e-03, -0.2026e-03, -0.2648e-03, &
        -0.3468e-03/
      data ((h23(ip,iw),iw=1,31), ip= 1, 1)/ &
        -0.2195e-09, -0.4031e-09, -0.7043e-09, -0.1153e-08, -0.1737e-08, &
        -0.2395e-08, -0.3020e-08, -0.3549e-08, -0.4034e-08, -0.4421e-08, &
        -0.4736e-08, -0.5681e-08, -0.8289e-08, -0.1287e-07, -0.1873e-07, &
        -0.2523e-07, -0.3223e-07, -0.3902e-07, -0.4409e-07, -0.4699e-07, &
        -0.4782e-07, -0.4705e-07, -0.4657e-07, -0.4885e-07, -0.5550e-07, &
        -0.6619e-07, -0.7656e-07, -0.8027e-07, -0.7261e-07, -0.4983e-07, &
        -0.1101e-07/
      data ((h21(ip,iw),iw=1,31), ip= 2, 2)/ &
         0.99999607,  0.99999237,  0.99998546,  0.99997294,  0.99995142, &
         0.99991679,  0.99986511,  0.99979353,  0.99970138,  0.99958861, &
         0.99945688,  0.99930882,  0.99914342,  0.99895400,  0.99872798, &
         0.99844801,  0.99809802,  0.99765801,  0.99710101,  0.99639499, &
         0.99549901,  0.99435198,  0.99287099,  0.99093699,  0.98837000, &
         0.98491001,  0.98019999,  0.97373998,  0.96490002,  0.95283002, &
         0.93649000/
      data ((h22(ip,iw),iw=1,31), ip= 2, 2)/ &
        -0.5622e-07, -0.1071e-06, -0.1983e-06, -0.3534e-06, -0.5992e-06, &
        -0.9594e-06, -0.1445e-05, -0.2050e-05, -0.2766e-05, -0.3580e-05, &
        -0.4476e-05, -0.5479e-05, -0.6677e-05, -0.8179e-05, -0.1009e-04, &
        -0.1251e-04, -0.1553e-04, -0.1928e-04, -0.2384e-04, -0.2930e-04, &
        -0.3588e-04, -0.4393e-04, -0.5403e-04, -0.6714e-04, -0.8458e-04, &
        -0.1082e-03, -0.1400e-03, -0.1829e-03, -0.2401e-03, -0.3157e-03, &
        -0.4147e-03/
      data ((h23(ip,iw),iw=1,31), ip= 2, 2)/ &
        -0.2195e-09, -0.4032e-09, -0.7046e-09, -0.1153e-08, -0.1738e-08, &
        -0.2395e-08, -0.3021e-08, -0.3550e-08, -0.4035e-08, -0.4423e-08, &
        -0.4740e-08, -0.5692e-08, -0.8314e-08, -0.1292e-07, -0.1882e-07, &
        -0.2536e-07, -0.3242e-07, -0.3927e-07, -0.4449e-07, -0.4767e-07, &
        -0.4889e-07, -0.4857e-07, -0.4860e-07, -0.5132e-07, -0.5847e-07, &
        -0.6968e-07, -0.8037e-07, -0.8400e-07, -0.7521e-07, -0.4830e-07, &
        -0.7562e-09/
      data ((h21(ip,iw),iw=1,31), ip= 3, 3)/ &
         0.99999607,  0.99999237,  0.99998546,  0.99997294,  0.99995142, &
         0.99991679,  0.99986500,  0.99979341,  0.99970102,  0.99958777, &
         0.99945557,  0.99930632,  0.99913889,  0.99894601,  0.99871302, &
         0.99842101,  0.99805099,  0.99757600,  0.99696302,  0.99617100, &
         0.99514598,  0.99381000,  0.99205798,  0.98974001,  0.98662001, &
         0.98238999,  0.97659999,  0.96866000,  0.95776999,  0.94296998, &
         0.92306000/
      data ((h22(ip,iw),iw=1,31), ip= 3, 3)/ &
        -0.5622e-07, -0.1071e-06, -0.1983e-06, -0.3535e-06, -0.5994e-06, &
        -0.9599e-06, -0.1446e-05, -0.2052e-05, -0.2769e-05, -0.3586e-05, &
        -0.4487e-05, -0.5499e-05, -0.6712e-05, -0.8244e-05, -0.1021e-04, &
        -0.1272e-04, -0.1591e-04, -0.1992e-04, -0.2489e-04, -0.3097e-04, &
        -0.3845e-04, -0.4782e-04, -0.5982e-04, -0.7558e-04, -0.9674e-04, &
        -0.1254e-03, -0.1644e-03, -0.2167e-03, -0.2863e-03, -0.3777e-03, &
        -0.4959e-03/
      data ((h23(ip,iw),iw=1,31), ip= 3, 3)/ &
        -0.2196e-09, -0.4033e-09, -0.7048e-09, -0.1154e-08, -0.1739e-08, &
        -0.2396e-08, -0.3022e-08, -0.3551e-08, -0.4036e-08, -0.4425e-08, &
        -0.4746e-08, -0.5710e-08, -0.8354e-08, -0.1300e-07, -0.1894e-07, &
        -0.2554e-07, -0.3265e-07, -0.3958e-07, -0.4502e-07, -0.4859e-07, &
        -0.5030e-07, -0.5053e-07, -0.5104e-07, -0.5427e-07, -0.6204e-07, &
        -0.7388e-07, -0.8477e-07, -0.8760e-07, -0.7545e-07, -0.4099e-07, &
         0.2046e-07/
      data ((h21(ip,iw),iw=1,31), ip= 4, 4)/ &
         0.99999607,  0.99999237,  0.99998546,  0.99997294,  0.99995142, &
         0.99991673,  0.99986482,  0.99979299,  0.99970031,  0.99958658, &
         0.99945343,  0.99930239,  0.99913180,  0.99893302,  0.99869001, &
         0.99838102,  0.99798000,  0.99745703,  0.99676800,  0.99586397, &
         0.99467200,  0.99309403,  0.99099600,  0.98817998,  0.98438001, &
         0.97918999,  0.97206002,  0.96227002,  0.94888997,  0.93080997, &
         0.90671003/
      data ((h22(ip,iw),iw=1,31), ip= 4, 4)/ &
        -0.5623e-07, -0.1071e-06, -0.1984e-06, -0.3536e-06, -0.5997e-06, &
        -0.9606e-06, -0.1447e-05, -0.2055e-05, -0.2775e-05, -0.3596e-05, &
        -0.4504e-05, -0.5529e-05, -0.6768e-05, -0.8345e-05, -0.1039e-04, &
        -0.1304e-04, -0.1645e-04, -0.2082e-04, -0.2633e-04, -0.3322e-04, &
        -0.4187e-04, -0.5292e-04, -0.6730e-04, -0.8640e-04, -0.1122e-03, &
        -0.1472e-03, -0.1948e-03, -0.2585e-03, -0.3428e-03, -0.4523e-03, &
        -0.5915e-03/
      data ((h23(ip,iw),iw=1,31), ip= 4, 4)/ &
        -0.2196e-09, -0.4034e-09, -0.7050e-09, -0.1154e-08, -0.1740e-08, &
        -0.2398e-08, -0.3024e-08, -0.3552e-08, -0.4037e-08, -0.4428e-08, &
        -0.4756e-08, -0.5741e-08, -0.8418e-08, -0.1310e-07, -0.1910e-07, &
        -0.2575e-07, -0.3293e-07, -0.3998e-07, -0.4572e-07, -0.4980e-07, &
        -0.5211e-07, -0.5287e-07, -0.5390e-07, -0.5782e-07, -0.6650e-07, &
        -0.7892e-07, -0.8940e-07, -0.8980e-07, -0.7119e-07, -0.2452e-07, &
         0.5823e-07/
      data ((h21(ip,iw),iw=1,31), ip= 5, 5)/ &
         0.99999607,  0.99999237,  0.99998546,  0.99997294,  0.99995136, &
         0.99991661,  0.99986458,  0.99979252,  0.99969929,  0.99958479, &
         0.99945003,  0.99929619,  0.99912071,  0.99891400,  0.99865502, &
         0.99831998,  0.99787700,  0.99728799,  0.99650002,  0.99544799, &
         0.99404198,  0.99215603,  0.98961997,  0.98619002,  0.98153001, &
         0.97513002,  0.96634001,  0.95428002,  0.93791002,  0.91593999, &
         0.88700002/
      data ((h22(ip,iw),iw=1,31), ip= 5, 5)/ &
        -0.5623e-07, -0.1071e-06, -0.1985e-06, -0.3538e-06, -0.6002e-06, &
        -0.9618e-06, -0.1450e-05, -0.2059e-05, -0.2783e-05, -0.3611e-05, &
        -0.4531e-05, -0.5577e-05, -0.6855e-05, -0.8499e-05, -0.1066e-04, &
        -0.1351e-04, -0.1723e-04, -0.2207e-04, -0.2829e-04, -0.3621e-04, &
        -0.4636e-04, -0.5954e-04, -0.7690e-04, -0.1002e-03, -0.1317e-03, &
        -0.1746e-03, -0.2326e-03, -0.3099e-03, -0.4111e-03, -0.5407e-03, &
        -0.7020e-03/
      data ((h23(ip,iw),iw=1,31), ip= 5, 5)/ &
        -0.2197e-09, -0.4037e-09, -0.7054e-09, -0.1155e-08, -0.1741e-08, &
        -0.2401e-08, -0.3027e-08, -0.3553e-08, -0.4039e-08, -0.4431e-08, &
        -0.4775e-08, -0.5784e-08, -0.8506e-08, -0.1326e-07, -0.1931e-07, &
        -0.2600e-07, -0.3324e-07, -0.4048e-07, -0.4666e-07, -0.5137e-07, &
        -0.5428e-07, -0.5558e-07, -0.5730e-07, -0.6228e-07, -0.7197e-07, &
        -0.8455e-07, -0.9347e-07, -0.8867e-07, -0.5945e-07,  0.5512e-08, &
         0.1209e-06/
      data ((h21(ip,iw),iw=1,31), ip= 6, 6)/ &
         0.99999607,  0.99999237,  0.99998546,  0.99997288,  0.99995130, &
         0.99991649,  0.99986428,  0.99979180,  0.99969780,  0.99958187, &
         0.99944460,  0.99928659,  0.99910372,  0.99888301,  0.99860299, &
         0.99822998,  0.99773002,  0.99705303,  0.99613500,  0.99489301, &
         0.99321300,  0.99093801,  0.98785001,  0.98365998,  0.97790998, &
         0.97000998,  0.95916998,  0.94437003,  0.92440999,  0.89789999, &
         0.86360002/
      data ((h22(ip,iw),iw=1,31), ip= 6, 6)/ &
        -0.5624e-07, -0.1072e-06, -0.1986e-06, -0.3541e-06, -0.6010e-06, &
        -0.9636e-06, -0.1453e-05, -0.2067e-05, -0.2796e-05, -0.3634e-05, &
        -0.4572e-05, -0.5652e-05, -0.6987e-05, -0.8733e-05, -0.1107e-04, &
        -0.1418e-04, -0.1832e-04, -0.2378e-04, -0.3092e-04, -0.4017e-04, &
        -0.5221e-04, -0.6806e-04, -0.8916e-04, -0.1176e-03, -0.1562e-03, &
        -0.2087e-03, -0.2793e-03, -0.3724e-03, -0.4928e-03, -0.6440e-03, &
        -0.8270e-03/
      data ((h23(ip,iw),iw=1,31), ip= 6, 6)/ &
        -0.2198e-09, -0.4040e-09, -0.7061e-09, -0.1156e-08, -0.1744e-08, &
        -0.2405e-08, -0.3032e-08, -0.3556e-08, -0.4040e-08, -0.4444e-08, &
        -0.4800e-08, -0.5848e-08, -0.8640e-08, -0.1346e-07, -0.1957e-07, &
        -0.2627e-07, -0.3357e-07, -0.4114e-07, -0.4793e-07, -0.5330e-07, &
        -0.5676e-07, -0.5873e-07, -0.6152e-07, -0.6783e-07, -0.7834e-07, &
        -0.9023e-07, -0.9530e-07, -0.8162e-07, -0.3634e-07,  0.5638e-07, &
         0.2189e-06/
      data ((h21(ip,iw),iw=1,31), ip= 7, 7)/ &
         0.99999607,  0.99999237,  0.99998546,  0.99997288,  0.99995124, &
         0.99991626,  0.99986368,  0.99979049,  0.99969530,  0.99957728, &
         0.99943632,  0.99927181,  0.99907762,  0.99883801,  0.99852502, &
         0.99810201,  0.99752498,  0.99673301,  0.99564600,  0.99416101, &
         0.99213398,  0.98936999,  0.98559999,  0.98043001,  0.97333997, &
         0.96359003,  0.95025003,  0.93216002,  0.90798998,  0.87639999, &
         0.83609998/
      data ((h22(ip,iw),iw=1,31), ip= 7, 7)/ &
        -0.5626e-07, -0.1072e-06, -0.1987e-06, -0.3545e-06, -0.6022e-06, &
        -0.9665e-06, -0.1460e-05, -0.2078e-05, -0.2817e-05, -0.3671e-05, &
        -0.4637e-05, -0.5767e-05, -0.7188e-05, -0.9080e-05, -0.1165e-04, &
        -0.1513e-04, -0.1981e-04, -0.2609e-04, -0.3441e-04, -0.4534e-04, &
        -0.5978e-04, -0.7897e-04, -0.1047e-03, -0.1396e-03, -0.1870e-03, &
        -0.2510e-03, -0.3363e-03, -0.4475e-03, -0.5888e-03, -0.7621e-03, &
        -0.9647e-03/
      data ((h23(ip,iw),iw=1,31), ip= 7, 7)/ &
        -0.2200e-09, -0.4045e-09, -0.7071e-09, -0.1159e-08, -0.1748e-08, &
        -0.2411e-08, -0.3040e-08, -0.3561e-08, -0.4046e-08, -0.4455e-08, &
        -0.4839e-08, -0.5941e-08, -0.8815e-08, -0.1371e-07, -0.1983e-07, &
        -0.2652e-07, -0.3400e-07, -0.4207e-07, -0.4955e-07, -0.5554e-07, &
        -0.5966e-07, -0.6261e-07, -0.6688e-07, -0.7454e-07, -0.8521e-07, &
        -0.9470e-07, -0.9275e-07, -0.6525e-07,  0.3686e-08,  0.1371e-06, &
         0.3623e-06/
      data ((h21(ip,iw),iw=1,31), ip= 8, 8)/ &
         0.99999607,  0.99999237,  0.99998540,  0.99997282,  0.99995112, &
         0.99991590,  0.99986279,  0.99978858,  0.99969149,  0.99957019, &
         0.99942350,  0.99924922,  0.99903822,  0.99877101,  0.99841398, &
         0.99792302,  0.99724299,  0.99630302,  0.99500000,  0.99320602, &
         0.99074000,  0.98736000,  0.98272002,  0.97635001,  0.96758002, &
         0.95555997,  0.93919998,  0.91722000,  0.88819999,  0.85089999, &
         0.80439997/
      data ((h22(ip,iw),iw=1,31), ip= 8, 8)/ &
        -0.5628e-07, -0.1073e-06, -0.1990e-06, -0.3553e-06, -0.6042e-06, &
        -0.9710e-06, -0.1469e-05, -0.2096e-05, -0.2849e-05, -0.3728e-05, &
        -0.4738e-05, -0.5942e-05, -0.7490e-05, -0.9586e-05, -0.1247e-04, &
        -0.1644e-04, -0.2184e-04, -0.2916e-04, -0.3898e-04, -0.5205e-04, &
        -0.6948e-04, -0.9285e-04, -0.1244e-03, -0.1672e-03, -0.2251e-03, &
        -0.3028e-03, -0.4051e-03, -0.5365e-03, -0.6998e-03, -0.8940e-03, &
        -0.1112e-02/
      data ((h23(ip,iw),iw=1,31), ip= 8, 8)/ &
        -0.2204e-09, -0.4052e-09, -0.7088e-09, -0.1162e-08, -0.1755e-08, &
        -0.2422e-08, -0.3053e-08, -0.3572e-08, -0.4052e-08, -0.4474e-08, &
        -0.4898e-08, -0.6082e-08, -0.9046e-08, -0.1400e-07, -0.2009e-07, &
        -0.2683e-07, -0.3463e-07, -0.4334e-07, -0.5153e-07, -0.5811e-07, &
        -0.6305e-07, -0.6749e-07, -0.7346e-07, -0.8208e-07, -0.9173e-07, &
        -0.9603e-07, -0.8264e-07, -0.3505e-07,  0.6878e-07,  0.2586e-06, &
         0.5530e-06/
      data ((h21(ip,iw),iw=1,31), ip= 9, 9)/ &
         0.99999607,  0.99999237,  0.99998540,  0.99997276,  0.99995089, &
         0.99991536,  0.99986148,  0.99978572,  0.99968570,  0.99955928, &
         0.99940401,  0.99921501,  0.99897999,  0.99867398,  0.99825603, &
         0.99767601,  0.99686497,  0.99573302,  0.99415499,  0.99196899, &
         0.98895001,  0.98479998,  0.97907001,  0.97119999,  0.96038002, &
         0.94559997,  0.92565000,  0.89910001,  0.86470002,  0.82130003, &
         0.76830000/
      data ((h22(ip,iw),iw=1,31), ip= 9, 9)/ &
        -0.5630e-07, -0.1074e-06, -0.1994e-06, -0.3564e-06, -0.6072e-06, &
        -0.9779e-06, -0.1484e-05, -0.2124e-05, -0.2900e-05, -0.3817e-05, &
        -0.4891e-05, -0.6205e-05, -0.7931e-05, -0.1031e-04, -0.1362e-04, &
        -0.1821e-04, -0.2454e-04, -0.3320e-04, -0.4493e-04, -0.6068e-04, &
        -0.8186e-04, -0.1104e-03, -0.1491e-03, -0.2016e-03, -0.2722e-03, &
        -0.3658e-03, -0.4873e-03, -0.6404e-03, -0.8253e-03, -0.1037e-02, &
        -0.1267e-02/
      data ((h23(ip,iw),iw=1,31), ip= 9, 9)/ &
        -0.2207e-09, -0.4061e-09, -0.7117e-09, -0.1169e-08, -0.1767e-08, &
        -0.2439e-08, -0.3074e-08, -0.3588e-08, -0.4062e-08, -0.4510e-08, &
        -0.4983e-08, -0.6261e-08, -0.9324e-08, -0.1430e-07, -0.2036e-07, &
        -0.2725e-07, -0.3561e-07, -0.4505e-07, -0.5384e-07, -0.6111e-07, &
        -0.6731e-07, -0.7355e-07, -0.8112e-07, -0.8978e-07, -0.9616e-07, &
        -0.9157e-07, -0.6114e-07,  0.1622e-07,  0.1694e-06,  0.4277e-06, &
         0.7751e-06/
      data ((h21(ip,iw),iw=1,31), ip=10,10)/ &
         0.99999607,  0.99999237,  0.99998540,  0.99997264,  0.99995059, &
         0.99991453,  0.99985939,  0.99978119,  0.99967682,  0.99954277, &
         0.99937469,  0.99916458,  0.99889499,  0.99853599,  0.99804002, &
         0.99734300,  0.99636298,  0.99498600,  0.99305803,  0.99037802, &
         0.98667002,  0.98154002,  0.97447002,  0.96473998,  0.95141000, &
         0.93333000,  0.90916002,  0.87750000,  0.83710003,  0.78729999, &
         0.72790003/
      data ((h22(ip,iw),iw=1,31), ip=10,10)/ &
        -0.5636e-07, -0.1076e-06, -0.2000e-06, -0.3582e-06, -0.6119e-06, &
        -0.9888e-06, -0.1507e-05, -0.2168e-05, -0.2978e-05, -0.3952e-05, &
        -0.5122e-05, -0.6592e-05, -0.8565e-05, -0.1132e-04, -0.1518e-04, &
        -0.2060e-04, -0.2811e-04, -0.3848e-04, -0.5261e-04, -0.7173e-04, &
        -0.9758e-04, -0.1326e-03, -0.1801e-03, -0.2442e-03, -0.3296e-03, &
        -0.4415e-03, -0.5840e-03, -0.7591e-03, -0.9636e-03, -0.1189e-02, &
        -0.1427e-02/
      data ((h23(ip,iw),iw=1,31), ip=10,10)/ &
        -0.2214e-09, -0.4080e-09, -0.7156e-09, -0.1178e-08, -0.1784e-08, &
        -0.2466e-08, -0.3105e-08, -0.3617e-08, -0.4087e-08, -0.4563e-08, &
        -0.5110e-08, -0.6492e-08, -0.9643e-08, -0.1461e-07, -0.2069e-07, &
        -0.2796e-07, -0.3702e-07, -0.4717e-07, -0.5662e-07, -0.6484e-07, &
        -0.7271e-07, -0.8079e-07, -0.8928e-07, -0.9634e-07, -0.9625e-07, &
        -0.7776e-07, -0.2242e-07,  0.9745e-07,  0.3152e-06,  0.6388e-06, &
         0.9992e-06/
      data ((h21(ip,iw),iw=1,31), ip=11,11)/ &
         0.99999607,  0.99999237,  0.99998534,  0.99997252,  0.99995011, &
         0.99991328,  0.99985629,  0.99977452,  0.99966347,  0.99951839, &
         0.99933177,  0.99909180,  0.99877602,  0.99834698,  0.99774700, &
         0.99690098,  0.99570400,  0.99401599,  0.99164802,  0.98834997, &
         0.98377001,  0.97742999,  0.96868002,  0.95666999,  0.94032001, &
         0.91833997,  0.88929999,  0.85189998,  0.80530000,  0.74909997, &
         0.68299997/
      data ((h22(ip,iw),iw=1,31), ip=11,11)/ &
        -0.5645e-07, -0.1079e-06, -0.2009e-06, -0.3610e-06, -0.6190e-06, &
        -0.1005e-05, -0.1541e-05, -0.2235e-05, -0.3096e-05, -0.4155e-05, &
        -0.5463e-05, -0.7150e-05, -0.9453e-05, -0.1269e-04, -0.1728e-04, &
        -0.2375e-04, -0.3279e-04, -0.4531e-04, -0.6246e-04, -0.8579e-04, &
        -0.1175e-03, -0.1604e-03, -0.2186e-03, -0.2964e-03, -0.3990e-03, &
        -0.5311e-03, -0.6957e-03, -0.8916e-03, -0.1112e-02, -0.1346e-02, &
        -0.1590e-02/
      data ((h23(ip,iw),iw=1,31), ip=11,11)/ &
        -0.2225e-09, -0.4104e-09, -0.7217e-09, -0.1192e-08, -0.1811e-08, &
        -0.2509e-08, -0.3155e-08, -0.3668e-08, -0.4138e-08, -0.4650e-08, &
        -0.5296e-08, -0.6785e-08, -0.9991e-08, -0.1494e-07, -0.2122e-07, &
        -0.2911e-07, -0.3895e-07, -0.4979e-07, -0.6002e-07, -0.6964e-07, &
        -0.7935e-07, -0.8887e-07, -0.9699e-07, -0.9967e-07, -0.8883e-07, &
        -0.4988e-07,  0.4156e-07,  0.2197e-06,  0.5081e-06,  0.8667e-06, &
         0.1212e-05/
      data ((h21(ip,iw),iw=1,31), ip=12,12)/ &
         0.99999607,  0.99999237,  0.99998528,  0.99997234,  0.99994951, &
         0.99991143,  0.99985188,  0.99976480,  0.99964428,  0.99948311, &
         0.99927050,  0.99899000,  0.99861199,  0.99809098,  0.99735999, &
         0.99632198,  0.99484903,  0.99276900,  0.98984998,  0.98576999, &
         0.98009998,  0.97224998,  0.96144003,  0.94667000,  0.92672002, &
         0.90020001,  0.86570001,  0.82220000,  0.76919997,  0.70640004, &
         0.63330001/
      data ((h22(ip,iw),iw=1,31), ip=12,12)/ &
        -0.5658e-07, -0.1083e-06, -0.2023e-06, -0.3650e-06, -0.6295e-06, &
        -0.1030e-05, -0.1593e-05, -0.2334e-05, -0.3273e-05, -0.4455e-05, &
        -0.5955e-05, -0.7935e-05, -0.1067e-04, -0.1455e-04, -0.2007e-04, &
        -0.2788e-04, -0.3885e-04, -0.5409e-04, -0.7503e-04, -0.1036e-03, &
        -0.1425e-03, -0.1951e-03, -0.2660e-03, -0.3598e-03, -0.4817e-03, &
        -0.6355e-03, -0.8218e-03, -0.1035e-02, -0.1267e-02, -0.1508e-02, &
        -0.1755e-02/
      data ((h23(ip,iw),iw=1,31), ip=12,12)/ &
        -0.2241e-09, -0.4142e-09, -0.7312e-09, -0.1214e-08, -0.1854e-08, &
        -0.2578e-08, -0.3250e-08, -0.3765e-08, -0.4238e-08, -0.4809e-08, &
        -0.5553e-08, -0.7132e-08, -0.1035e-07, -0.1538e-07, -0.2211e-07, &
        -0.3079e-07, -0.4142e-07, -0.5303e-07, -0.6437e-07, -0.7566e-07, &
        -0.8703e-07, -0.9700e-07, -0.1025e-06, -0.9718e-07, -0.6973e-07, &
        -0.5265e-09,  0.1413e-06,  0.3895e-06,  0.7321e-06,  0.1085e-05, &
         0.1449e-05/
      data ((h21(ip,iw),iw=1,31), ip=13,13)/ &
         0.99999607,  0.99999231,  0.99998522,  0.99997205,  0.99994856, &
         0.99990892,  0.99984580,  0.99975121,  0.99961728,  0.99943388, &
         0.99918568,  0.99884999,  0.99839199,  0.99775398,  0.99685299, &
         0.99557197,  0.99375200,  0.99117702,  0.98755997,  0.98250002, &
         0.97548002,  0.96575999,  0.95244002,  0.93436003,  0.91018999, &
         0.87849998,  0.83810002,  0.78820002,  0.72870004,  0.65910000, &
         0.57850003/
      data ((h22(ip,iw),iw=1,31), ip=13,13)/ &
        -0.5677e-07, -0.1090e-06, -0.2043e-06, -0.3709e-06, -0.6448e-06, &
        -0.1066e-05, -0.1669e-05, -0.2480e-05, -0.3532e-05, -0.4887e-05, &
        -0.6650e-05, -0.9017e-05, -0.1232e-04, -0.1701e-04, -0.2372e-04, &
        -0.3325e-04, -0.4664e-04, -0.6528e-04, -0.9095e-04, -0.1260e-03, &
        -0.1737e-03, -0.2381e-03, -0.3238e-03, -0.4359e-03, -0.5789e-03, &
        -0.7549e-03, -0.9607e-03, -0.1188e-02, -0.1427e-02, -0.1674e-02, &
        -0.1914e-02/
      data ((h23(ip,iw),iw=1,31), ip=13,13)/ &
        -0.2262e-09, -0.4191e-09, -0.7441e-09, -0.1244e-08, -0.1916e-08, &
        -0.2687e-08, -0.3407e-08, -0.3947e-08, -0.4432e-08, -0.5059e-08, &
        -0.5896e-08, -0.7538e-08, -0.1079e-07, -0.1606e-07, -0.2346e-07, &
        -0.3305e-07, -0.4459e-07, -0.5720e-07, -0.6999e-07, -0.8300e-07, &
        -0.9536e-07, -0.1039e-06, -0.1036e-06, -0.8526e-07, -0.3316e-07, &
         0.7909e-07,  0.2865e-06,  0.6013e-06,  0.9580e-06,  0.1303e-05, &
         0.1792e-05/
      data ((h21(ip,iw),iw=1,31), ip=14,14)/ &
         0.99999607,  0.99999231,  0.99998510,  0.99997163,  0.99994737, &
         0.99990571,  0.99983770,  0.99973333,  0.99958128,  0.99936771, &
         0.99907219,  0.99866599,  0.99810302,  0.99731499,  0.99620003, &
         0.99461198,  0.99235398,  0.98916000,  0.98466998,  0.97839999, &
         0.96969002,  0.95769000,  0.94133997,  0.91935998,  0.89029998, &
         0.85290003,  0.80620003,  0.74989998,  0.68369997,  0.60679996, &
         0.51899999/
      data ((h22(ip,iw),iw=1,31), ip=14,14)/ &
        -0.5703e-07, -0.1098e-06, -0.2071e-06, -0.3788e-06, -0.6657e-06, &
        -0.1116e-05, -0.1776e-05, -0.2687e-05, -0.3898e-05, -0.5493e-05, &
        -0.7607e-05, -0.1048e-04, -0.1450e-04, -0.2024e-04, -0.2845e-04, &
        -0.4014e-04, -0.5658e-04, -0.7947e-04, -0.1110e-03, -0.1541e-03, &
        -0.2125e-03, -0.2907e-03, -0.3936e-03, -0.5261e-03, -0.6912e-03, &
        -0.8880e-03, -0.1109e-02, -0.1346e-02, -0.1591e-02, -0.1837e-02, &
        -0.2054e-02/
      data ((h23(ip,iw),iw=1,31), ip=14,14)/ &
        -0.2288e-09, -0.4265e-09, -0.7627e-09, -0.1289e-08, -0.2011e-08, &
        -0.2861e-08, -0.3673e-08, -0.4288e-08, -0.4812e-08, -0.5475e-08, &
        -0.6365e-08, -0.8052e-08, -0.1142e-07, -0.1711e-07, -0.2533e-07, &
        -0.3597e-07, -0.4862e-07, -0.6259e-07, -0.7708e-07, -0.9150e-07, &
        -0.1035e-06, -0.1079e-06, -0.9742e-07, -0.5928e-07,  0.2892e-07, &
         0.1998e-06,  0.4789e-06,  0.8298e-06,  0.1172e-05,  0.1583e-05, &
         0.2329e-05/
      data ((h21(ip,iw),iw=1,31), ip=15,15)/ &
         0.99999607,  0.99999225,  0.99998498,  0.99997115,  0.99994600, &
         0.99990171,  0.99982780,  0.99971092,  0.99953562,  0.99928278, &
         0.99892598,  0.99842799,  0.99773300,  0.99675500,  0.99536800, &
         0.99339402,  0.99058902,  0.98662001,  0.98104000,  0.97325999, &
         0.96249002,  0.94773000,  0.92778003,  0.90125000,  0.86680001, &
         0.82319999,  0.77010000,  0.70730001,  0.63400000,  0.54960001, &
         0.45560002/
      data ((h22(ip,iw),iw=1,31), ip=15,15)/ &
        -0.5736e-07, -0.1109e-06, -0.2106e-06, -0.3890e-06, -0.6928e-06, &
        -0.1181e-05, -0.1917e-05, -0.2965e-05, -0.4396e-05, -0.6315e-05, &
        -0.8891e-05, -0.1242e-04, -0.1736e-04, -0.2442e-04, -0.3454e-04, &
        -0.4892e-04, -0.6916e-04, -0.9735e-04, -0.1362e-03, -0.1891e-03, &
        -0.2602e-03, -0.3545e-03, -0.4768e-03, -0.6310e-03, -0.8179e-03, &
        -0.1032e-02, -0.1265e-02, -0.1508e-02, -0.1757e-02, -0.1989e-02, &
        -0.2159e-02/
      data ((h23(ip,iw),iw=1,31), ip=15,15)/ &
        -0.2321e-09, -0.4350e-09, -0.7861e-09, -0.1347e-08, -0.2144e-08, &
        -0.3120e-08, -0.4107e-08, -0.4892e-08, -0.5511e-08, -0.6164e-08, &
        -0.7054e-08, -0.8811e-08, -0.1240e-07, -0.1862e-07, -0.2773e-07, &
        -0.3957e-07, -0.5371e-07, -0.6939e-07, -0.8564e-07, -0.1006e-06, &
        -0.1100e-06, -0.1066e-06, -0.8018e-07, -0.1228e-07,  0.1263e-06, &
         0.3678e-06,  0.7022e-06,  0.1049e-05,  0.1411e-05,  0.2015e-05, &
         0.3099e-05/
      data ((h21(ip,iw),iw=1,31), ip=16,16)/ &
         0.99999791,  0.99999589,  0.99999183,  0.99998391,  0.99996853, &
         0.99993920,  0.99988472,  0.99978709,  0.99961978,  0.99934620, &
         0.99892199,  0.99830103,  0.99742401,  0.99619502,  0.99445999, &
         0.99199599,  0.98851001,  0.98360002,  0.97671002,  0.96710002, &
         0.95378000,  0.93559003,  0.91112000,  0.87900001,  0.83780003, &
         0.78680003,  0.72549999,  0.65300000,  0.56830001,  0.47140002, &
         0.36650002/
      data ((h22(ip,iw),iw=1,31), ip=16,16)/ &
        -0.3122e-07, -0.6175e-07, -0.1214e-06, -0.2361e-06, -0.4518e-06, &
        -0.8438e-06, -0.1524e-05, -0.2643e-05, -0.4380e-05, -0.6922e-05, &
        -0.1042e-04, -0.1504e-04, -0.2125e-04, -0.2987e-04, -0.4200e-04, &
        -0.5923e-04, -0.8383e-04, -0.1186e-03, -0.1670e-03, -0.2328e-03, &
        -0.3204e-03, -0.4347e-03, -0.5802e-03, -0.7595e-03, -0.9703e-03, &
        -0.1205e-02, -0.1457e-02, -0.1720e-02, -0.1980e-02, -0.2191e-02, &
        -0.2290e-02/
      data ((h23(ip,iw),iw=1,31), ip=16,16)/ &
        -0.1376e-09, -0.2699e-09, -0.5220e-09, -0.9897e-09, -0.1819e-08, &
        -0.3186e-08, -0.5224e-08, -0.7896e-08, -0.1090e-07, -0.1349e-07, &
        -0.1443e-07, -0.1374e-07, -0.1386e-07, -0.1673e-07, -0.2237e-07, &
        -0.3248e-07, -0.5050e-07, -0.7743e-07, -0.1097e-06, -0.1369e-06, &
        -0.1463e-06, -0.1268e-06, -0.6424e-07,  0.5941e-07,  0.2742e-06, &
         0.5924e-06,  0.9445e-06,  0.1286e-05,  0.1819e-05,  0.2867e-05, &
         0.4527e-05/
      data ((h21(ip,iw),iw=1,31), ip=17,17)/ &
         0.99999756,  0.99999511,  0.99999028,  0.99998081,  0.99996233, &
         0.99992681,  0.99986011,  0.99973929,  0.99953061,  0.99918979, &
         0.99866599,  0.99790198,  0.99681997,  0.99528998,  0.99312103, &
         0.99004799,  0.98571002,  0.97961998,  0.97105998,  0.95915002, &
         0.94278002,  0.92061001,  0.89120001,  0.85320002,  0.80550003, &
         0.74759996,  0.67900002,  0.59829998,  0.50520003,  0.40219998, &
         0.29600000/
      data ((h22(ip,iw),iw=1,31), ip=17,17)/ &
        -0.3547e-07, -0.7029e-07, -0.1386e-06, -0.2709e-06, -0.5218e-06, &
        -0.9840e-06, -0.1799e-05, -0.3156e-05, -0.5272e-05, -0.8357e-05, &
        -0.1260e-04, -0.1827e-04, -0.2598e-04, -0.3667e-04, -0.5169e-04, &
        -0.7312e-04, -0.1037e-03, -0.1467e-03, -0.2060e-03, -0.2857e-03, &
        -0.3907e-03, -0.5257e-03, -0.6940e-03, -0.8954e-03, -0.1124e-02, &
        -0.1371e-02, -0.1632e-02, -0.1897e-02, -0.2131e-02, -0.2275e-02, &
        -0.2265e-02/
      data ((h23(ip,iw),iw=1,31), ip=17,17)/ &
        -0.1482e-09, -0.2910e-09, -0.5667e-09, -0.1081e-08, -0.2005e-08, &
        -0.3554e-08, -0.5902e-08, -0.8925e-08, -0.1209e-07, -0.1448e-07, &
        -0.1536e-07, -0.1565e-07, -0.1763e-07, -0.2088e-07, -0.2564e-07, &
        -0.3635e-07, -0.5791e-07, -0.8907e-07, -0.1213e-06, -0.1418e-06, &
        -0.1397e-06, -0.1000e-06, -0.4427e-08,  0.1713e-06,  0.4536e-06, &
         0.8086e-06,  0.1153e-05,  0.1588e-05,  0.2437e-05,  0.3905e-05, &
         0.5874e-05/
      data ((h21(ip,iw),iw=1,31), ip=18,18)/ &
         0.99999714,  0.99999428,  0.99998862,  0.99997741,  0.99995553, &
         0.99991333,  0.99983358,  0.99968803,  0.99943441,  0.99901879, &
         0.99837899,  0.99744099,  0.99609798,  0.99418801,  0.99147803, &
         0.98764998,  0.98227000,  0.97469997,  0.96410000,  0.94941998, &
         0.92940998,  0.90263999,  0.86769998,  0.82330000,  0.76880002, &
         0.70379996,  0.62720001,  0.53810000,  0.43769997,  0.33149999, &
         0.22839999/
      data ((h22(ip,iw),iw=1,31), ip=18,18)/ &
        -0.4064e-07, -0.8066e-07, -0.1593e-06, -0.3124e-06, -0.6049e-06, &
        -0.1148e-05, -0.2118e-05, -0.3751e-05, -0.6314e-05, -0.1006e-04, &
        -0.1526e-04, -0.2232e-04, -0.3196e-04, -0.4525e-04, -0.6394e-04, &
        -0.9058e-04, -0.1284e-03, -0.1812e-03, -0.2533e-03, -0.3493e-03, &
        -0.4740e-03, -0.6315e-03, -0.8228e-03, -0.1044e-02, -0.1286e-02, &
        -0.1544e-02, -0.1810e-02, -0.2061e-02, -0.2243e-02, -0.2291e-02, &
        -0.2152e-02/
      data ((h23(ip,iw),iw=1,31), ip=18,18)/ &
        -0.1630e-09, -0.3213e-09, -0.6266e-09, -0.1201e-08, -0.2248e-08, &
        -0.4030e-08, -0.6770e-08, -0.1033e-07, -0.1392e-07, -0.1640e-07, &
        -0.1768e-07, -0.1932e-07, -0.2229e-07, -0.2508e-07, -0.2940e-07, &
        -0.4200e-07, -0.6717e-07, -0.1002e-06, -0.1286e-06, -0.1402e-06, &
        -0.1216e-06, -0.5487e-07,  0.8418e-07,  0.3246e-06,  0.6610e-06, &
         0.1013e-05,  0.1394e-05,  0.2073e-05,  0.3337e-05,  0.5175e-05, &
         0.7255e-05/
      data ((h21(ip,iw),iw=1,31), ip=19,19)/ &
         0.99999672,  0.99999344,  0.99998701,  0.99997419,  0.99994916, &
         0.99990064,  0.99980861,  0.99963921,  0.99934143,  0.99884701, &
         0.99807602,  0.99692601,  0.99525797,  0.99287403,  0.98948997, &
         0.98474002,  0.97804999,  0.96867001,  0.95559001,  0.93761998, &
         0.91336000,  0.88139999,  0.84029996,  0.78920001,  0.72770000, &
         0.65499997,  0.56999999,  0.47280002,  0.36760002,  0.26220000, &
         0.16700000/
      data ((h22(ip,iw),iw=1,31), ip=19,19)/ &
        -0.4629e-07, -0.9195e-07, -0.1819e-06, -0.3572e-06, -0.6936e-06, &
        -0.1323e-05, -0.2456e-05, -0.4385e-05, -0.7453e-05, -0.1200e-04, &
        -0.1843e-04, -0.2731e-04, -0.3943e-04, -0.5606e-04, -0.7936e-04, &
        -0.1123e-03, -0.1588e-03, -0.2231e-03, -0.3101e-03, -0.4247e-03, &
        -0.5713e-03, -0.7522e-03, -0.9651e-03, -0.1202e-02, -0.1456e-02, &
        -0.1721e-02, -0.1983e-02, -0.2196e-02, -0.2296e-02, -0.2224e-02, &
        -0.1952e-02/
      data ((h23(ip,iw),iw=1,31), ip=19,19)/ &
        -0.1827e-09, -0.3607e-09, -0.7057e-09, -0.1359e-08, -0.2552e-08, &
        -0.4615e-08, -0.7854e-08, -0.1218e-07, -0.1670e-07, -0.2008e-07, &
        -0.2241e-07, -0.2516e-07, -0.2796e-07, -0.3015e-07, -0.3506e-07, &
        -0.4958e-07, -0.7627e-07, -0.1070e-06, -0.1289e-06, -0.1286e-06, &
        -0.8843e-07,  0.1492e-07,  0.2118e-06,  0.5155e-06,  0.8665e-06, &
         0.1220e-05,  0.1765e-05,  0.2825e-05,  0.4498e-05,  0.6563e-05, &
         0.8422e-05/
      data ((h21(ip,iw),iw=1,31), ip=20,20)/ &
         0.99999636,  0.99999279,  0.99998569,  0.99997163,  0.99994397, &
         0.99989033,  0.99978799,  0.99959832,  0.99926043,  0.99868900, &
         0.99777400,  0.99637598,  0.99431503,  0.99134803,  0.98714000, &
         0.98122001,  0.97290999,  0.96131998,  0.94528997,  0.92346001, &
         0.89429998,  0.85650003,  0.80879998,  0.75080001,  0.68190002, &
         0.60100001,  0.50740004,  0.40399998,  0.29729998,  0.19730002, &
         0.11479998/
      data ((h22(ip,iw),iw=1,31), ip=20,20)/ &
        -0.5164e-07, -0.1026e-06, -0.2031e-06, -0.3994e-06, -0.7771e-06, &
        -0.1488e-05, -0.2776e-05, -0.5001e-05, -0.8610e-05, -0.1411e-04, &
        -0.2209e-04, -0.3328e-04, -0.4860e-04, -0.6954e-04, -0.9861e-04, &
        -0.1393e-03, -0.1961e-03, -0.2738e-03, -0.3778e-03, -0.5132e-03, &
        -0.6831e-03, -0.8868e-03, -0.1118e-02, -0.1368e-02, -0.1632e-02, &
        -0.1899e-02, -0.2136e-02, -0.2282e-02, -0.2273e-02, -0.2067e-02, &
        -0.1679e-02/
      data ((h23(ip,iw),iw=1,31), ip=20,20)/ &
        -0.2058e-09, -0.4066e-09, -0.7967e-09, -0.1539e-08, -0.2904e-08, &
        -0.5293e-08, -0.9116e-08, -0.1447e-07, -0.2058e-07, -0.2608e-07, &
        -0.3053e-07, -0.3418e-07, -0.3619e-07, -0.3766e-07, -0.4313e-07, &
        -0.5817e-07, -0.8299e-07, -0.1072e-06, -0.1195e-06, -0.1031e-06, &
        -0.3275e-07,  0.1215e-06,  0.3835e-06,  0.7220e-06,  0.1062e-05, &
         0.1504e-05,  0.2367e-05,  0.3854e-05,  0.5842e-05,  0.7875e-05, &
         0.9082e-05/
      data ((h21(ip,iw),iw=1,31), ip=21,21)/ &
         0.99999619,  0.99999237,  0.99998480,  0.99996990,  0.99994045, &
         0.99988312,  0.99977320,  0.99956751,  0.99919540,  0.99855101, &
         0.99748802,  0.99581498,  0.99329299,  0.98961997,  0.98439002, &
         0.97702003,  0.96671999,  0.95245999,  0.93291998,  0.90660000, &
         0.87199998,  0.82780004,  0.77329999,  0.70819998,  0.63119996, &
         0.54159999,  0.44059998,  0.33380002,  0.23000002,  0.14029998, &
         0.07340002/
      data ((h22(ip,iw),iw=1,31), ip=21,21)/ &
        -0.5584e-07, -0.1110e-06, -0.2198e-06, -0.4329e-06, -0.8444e-06, &
        -0.1623e-05, -0.3049e-05, -0.5551e-05, -0.9714e-05, -0.1627e-04, &
        -0.2609e-04, -0.4015e-04, -0.5955e-04, -0.8603e-04, -0.1223e-03, &
        -0.1724e-03, -0.2413e-03, -0.3346e-03, -0.4578e-03, -0.6155e-03, &
        -0.8087e-03, -0.1033e-02, -0.1279e-02, -0.1540e-02, -0.1811e-02, &
        -0.2065e-02, -0.2251e-02, -0.2301e-02, -0.2163e-02, -0.1828e-02, &
        -0.1365e-02/
      data ((h23(ip,iw),iw=1,31), ip=21,21)/ &
        -0.2274e-09, -0.4498e-09, -0.8814e-09, -0.1708e-08, -0.3247e-08, &
        -0.5972e-08, -0.1045e-07, -0.1707e-07, -0.2545e-07, -0.3440e-07, &
        -0.4259e-07, -0.4822e-07, -0.5004e-07, -0.5061e-07, -0.5485e-07, &
        -0.6687e-07, -0.8483e-07, -0.9896e-07, -0.9646e-07, -0.5557e-07, &
         0.5765e-07,  0.2752e-06,  0.5870e-06,  0.9188e-06,  0.1291e-05, &
         0.1971e-05,  0.3251e-05,  0.5115e-05,  0.7221e-05,  0.8825e-05, &
         0.9032e-05/
      data ((h21(ip,iw),iw=1,31), ip=22,22)/ &
         0.99999607,  0.99999213,  0.99998438,  0.99996895,  0.99993849, &
         0.99987900,  0.99976391,  0.99954629,  0.99914569,  0.99843502, &
         0.99722600,  0.99526101,  0.99221897,  0.98771000,  0.98123002, &
         0.97209001,  0.95936000,  0.94187999,  0.91820002,  0.88679999, &
         0.84609997,  0.79530001,  0.73379999,  0.66090000,  0.57529998, &
         0.47740000,  0.37129998,  0.26490003,  0.16890001,  0.09340000, &
         0.04310000/
      data ((h22(ip,iw),iw=1,31), ip=22,22)/ &
        -0.5833e-07, -0.1160e-06, -0.2300e-06, -0.4540e-06, -0.8885e-06, &
        -0.1718e-05, -0.3256e-05, -0.6010e-05, -0.1072e-04, -0.1838e-04, &
        -0.3026e-04, -0.4772e-04, -0.7223e-04, -0.1057e-03, -0.1512e-03, &
        -0.2128e-03, -0.2961e-03, -0.4070e-03, -0.5514e-03, -0.7320e-03, &
        -0.9467e-03, -0.1187e-02, -0.1446e-02, -0.1717e-02, -0.1985e-02, &
        -0.2203e-02, -0.2307e-02, -0.2237e-02, -0.1965e-02, -0.1532e-02, &
        -0.1044e-02/
      data ((h23(ip,iw),iw=1,31), ip=22,22)/ &
        -0.2426e-09, -0.4805e-09, -0.9447e-09, -0.1841e-08, -0.3519e-08, &
        -0.6565e-08, -0.1172e-07, -0.1979e-07, -0.3095e-07, -0.4443e-07, &
        -0.5821e-07, -0.6868e-07, -0.7282e-07, -0.7208e-07, -0.7176e-07, &
        -0.7562e-07, -0.8110e-07, -0.7934e-07, -0.5365e-07,  0.2483e-07, &
         0.1959e-06,  0.4731e-06,  0.7954e-06,  0.1123e-05,  0.1652e-05, &
         0.2711e-05,  0.4402e-05,  0.6498e-05,  0.8392e-05,  0.9154e-05, &
         0.8261e-05/
      data ((h21(ip,iw),iw=1,31), ip=23,23)/ &
         0.99999601,  0.99999207,  0.99998420,  0.99996859,  0.99993771, &
         0.99987692,  0.99975860,  0.99953198,  0.99910772,  0.99833697, &
         0.99698901,  0.99473202,  0.99113101,  0.98566002,  0.97770000, &
         0.96640998,  0.95076001,  0.92943001,  0.90092003,  0.86370003, &
         0.81659997,  0.75889999,  0.69000000,  0.60870004,  0.51440001, &
         0.40990001,  0.30190003,  0.20060003,  0.11680001,  0.05760002, &
         0.02270001/
      data ((h22(ip,iw),iw=1,31), ip=23,23)/ &
        -0.5929e-07, -0.1180e-06, -0.2344e-06, -0.4638e-06, -0.9118e-06, &
        -0.1775e-05, -0.3401e-05, -0.6375e-05, -0.1160e-04, -0.2039e-04, &
        -0.3444e-04, -0.5575e-04, -0.8641e-04, -0.1287e-03, -0.1856e-03, &
        -0.2615e-03, -0.3618e-03, -0.4928e-03, -0.6594e-03, -0.8621e-03, &
        -0.1095e-02, -0.1349e-02, -0.1618e-02, -0.1894e-02, -0.2140e-02, &
        -0.2293e-02, -0.2290e-02, -0.2085e-02, -0.1696e-02, -0.1212e-02, &
        -0.7506e-03/
      data ((h23(ip,iw),iw=1,31), ip=23,23)/ &
        -0.2496e-09, -0.4954e-09, -0.9780e-09, -0.1915e-08, -0.3697e-08, &
        -0.6991e-08, -0.1279e-07, -0.2231e-07, -0.3653e-07, -0.5541e-07, &
        -0.7688e-07, -0.9614e-07, -0.1069e-06, -0.1065e-06, -0.9866e-07, &
        -0.8740e-07, -0.7192e-07, -0.4304e-07,  0.1982e-07,  0.1525e-06, &
         0.3873e-06,  0.6947e-06,  0.1000e-05,  0.1409e-05,  0.2253e-05, &
         0.3739e-05,  0.5744e-05,  0.7812e-05,  0.9067e-05,  0.8746e-05, &
         0.6940e-05/
      data ((h21(ip,iw),iw=1,31), ip=24,24)/ &
         0.99999601,  0.99999207,  0.99998420,  0.99996853,  0.99993742, &
         0.99987602,  0.99975550,  0.99952233,  0.99907869,  0.99825698, &
         0.99678302,  0.99424398,  0.99007100,  0.98356003,  0.97387999, &
         0.96004999,  0.94090003,  0.91503000,  0.88099998,  0.83740002, &
         0.78350002,  0.71869999,  0.64170003,  0.55149996,  0.44950002, &
         0.34100002,  0.23540002,  0.14380002,  0.07550001,  0.03240001, &
         0.01029998/
      data ((h22(ip,iw),iw=1,31), ip=24,24)/ &
        -0.5950e-07, -0.1185e-06, -0.2358e-06, -0.4678e-06, -0.9235e-06, &
        -0.1810e-05, -0.3503e-05, -0.6664e-05, -0.1236e-04, -0.2223e-04, &
        -0.3849e-04, -0.6396e-04, -0.1017e-03, -0.1545e-03, -0.2257e-03, &
        -0.3192e-03, -0.4399e-03, -0.5933e-03, -0.7824e-03, -0.1005e-02, &
        -0.1251e-02, -0.1516e-02, -0.1794e-02, -0.2060e-02, -0.2257e-02, &
        -0.2318e-02, -0.2186e-02, -0.1853e-02, -0.1386e-02, -0.9021e-03, &
        -0.5050e-03/
      data ((h23(ip,iw),iw=1,31), ip=24,24)/ &
        -0.2515e-09, -0.5001e-09, -0.9904e-09, -0.1951e-08, -0.3800e-08, &
        -0.7288e-08, -0.1362e-07, -0.2452e-07, -0.4184e-07, -0.6663e-07, &
        -0.9770e-07, -0.1299e-06, -0.1533e-06, -0.1584e-06, -0.1425e-06, &
        -0.1093e-06, -0.5972e-07,  0.1426e-07,  0.1347e-06,  0.3364e-06, &
         0.6209e-06,  0.9169e-06,  0.1243e-05,  0.1883e-05,  0.3145e-05, &
         0.5011e-05,  0.7136e-05,  0.8785e-05,  0.9048e-05,  0.7686e-05, &
         0.5368e-05/
      data ((h21(ip,iw),iw=1,31), ip=25,25)/ &
         0.99999601,  0.99999207,  0.99998420,  0.99996847,  0.99993724, &
         0.99987543,  0.99975342,  0.99951530,  0.99905682,  0.99819201, &
         0.99660802,  0.99381101,  0.98908001,  0.98148000,  0.96991003, &
         0.95317000,  0.92992002,  0.89880002,  0.85839999,  0.80799997, &
         0.74689996,  0.67420000,  0.58850002,  0.48979998,  0.38200003, &
         0.27329999,  0.17479998,  0.09700000,  0.04500002,  0.01620001, &
         0.00349998/
      data ((h22(ip,iw),iw=1,31), ip=25,25)/ &
        -0.5953e-07, -0.1187e-06, -0.2363e-06, -0.4697e-06, -0.9304e-06, &
        -0.1833e-05, -0.3578e-05, -0.6889e-05, -0.1299e-04, -0.2384e-04, &
        -0.4227e-04, -0.7201e-04, -0.1174e-03, -0.1825e-03, -0.2710e-03, &
        -0.3861e-03, -0.5313e-03, -0.7095e-03, -0.9203e-03, -0.1158e-02, &
        -0.1417e-02, -0.1691e-02, -0.1968e-02, -0.2200e-02, -0.2320e-02, &
        -0.2263e-02, -0.1998e-02, -0.1563e-02, -0.1068e-02, -0.6300e-03, &
        -0.3174e-03/
      data ((h23(ip,iw),iw=1,31), ip=25,25)/ &
        -0.2520e-09, -0.5016e-09, -0.9963e-09, -0.1971e-08, -0.3867e-08, &
        -0.7500e-08, -0.1427e-07, -0.2634e-07, -0.4656e-07, -0.7753e-07, &
        -0.1196e-06, -0.1683e-06, -0.2113e-06, -0.2309e-06, -0.2119e-06, &
        -0.1518e-06, -0.5200e-07,  0.9274e-07,  0.2973e-06,  0.5714e-06, &
         0.8687e-06,  0.1152e-05,  0.1621e-05,  0.2634e-05,  0.4310e-05, &
         0.6418e-05,  0.8347e-05,  0.9162e-05,  0.8319e-05,  0.6209e-05, &
         0.3844e-05/
      data ((h21(ip,iw),iw=1,31), ip=26,26)/ &
         0.99999601,  0.99999207,  0.99998420,  0.99996847,  0.99993718, &
         0.99987501,  0.99975210,  0.99951041,  0.99904078,  0.99814302, &
         0.99646801,  0.99344200,  0.98819000,  0.97952002,  0.96597999, &
         0.94600999,  0.91812003,  0.88099998,  0.83359998,  0.77569997, &
         0.70669997,  0.62529999,  0.53049999,  0.42449999,  0.31419998, &
         0.20969999,  0.12269998,  0.06089997,  0.02420002,  0.00660002, &
         0.00040001/
      data ((h22(ip,iw),iw=1,31), ip=26,26)/ &
        -0.5954e-07, -0.1187e-06, -0.2366e-06, -0.4709e-06, -0.9349e-06, &
        -0.1849e-05, -0.3632e-05, -0.7058e-05, -0.1350e-04, -0.2521e-04, &
        -0.4564e-04, -0.7958e-04, -0.1329e-03, -0.2114e-03, -0.3200e-03, &
        -0.4611e-03, -0.6353e-03, -0.8409e-03, -0.1072e-02, -0.1324e-02, &
        -0.1592e-02, -0.1871e-02, -0.2127e-02, -0.2297e-02, -0.2312e-02, &
        -0.2123e-02, -0.1738e-02, -0.1247e-02, -0.7744e-03, -0.4117e-03, &
        -0.1850e-03/
      data ((h23(ip,iw),iw=1,31), ip=26,26)/ &
        -0.2522e-09, -0.5025e-09, -0.9997e-09, -0.1983e-08, -0.3912e-08, &
        -0.7650e-08, -0.1474e-07, -0.2777e-07, -0.5055e-07, -0.8745e-07, &
        -0.1414e-06, -0.2095e-06, -0.2790e-06, -0.3241e-06, -0.3135e-06, &
        -0.2269e-06, -0.5896e-07,  0.1875e-06,  0.4996e-06,  0.8299e-06, &
         0.1115e-05,  0.1467e-05,  0.2236e-05,  0.3672e-05,  0.5668e-05, &
         0.7772e-05,  0.9094e-05,  0.8827e-05,  0.7041e-05,  0.4638e-05, &
         0.2539e-05/
      data ((h81(ip,iw),iw=1,31), ip= 1, 1)/ &
         0.99998659,  0.99997360,  0.99994862,  0.99990171,  0.99981678, &
         0.99967158,  0.99944150,  0.99910933,  0.99867302,  0.99814397, &
         0.99753898,  0.99686199,  0.99610198,  0.99523401,  0.99421698, &
         0.99299300,  0.99147898,  0.98958999,  0.98721999,  0.98430002, &
         0.98071998,  0.97639000,  0.97115999,  0.96480000,  0.95695001, &
         0.94713998,  0.93469000,  0.91873002,  0.89810002,  0.87129998, &
         0.83679998/
      data ((h82(ip,iw),iw=1,31), ip= 1, 1)/ &
        -0.5685e-08, -0.1331e-07, -0.3249e-07, -0.8137e-07, -0.2048e-06, &
        -0.4973e-06, -0.1118e-05, -0.2246e-05, -0.3982e-05, -0.6290e-05, &
        -0.9040e-05, -0.1215e-04, -0.1567e-04, -0.1970e-04, -0.2449e-04, &
        -0.3046e-04, -0.3798e-04, -0.4725e-04, -0.5831e-04, -0.7123e-04, &
        -0.8605e-04, -0.1028e-03, -0.1212e-03, -0.1413e-03, -0.1635e-03, &
        -0.1884e-03, -0.2160e-03, -0.2461e-03, -0.2778e-03, -0.3098e-03, &
        -0.3411e-03/
      data ((h83(ip,iw),iw=1,31), ip= 1, 1)/ &
         0.2169e-10,  0.5237e-10,  0.1296e-09,  0.3204e-09,  0.7665e-09, &
         0.1691e-08,  0.3222e-08,  0.5110e-08,  0.6779e-08,  0.7681e-08, &
         0.7378e-08,  0.5836e-08,  0.3191e-08, -0.1491e-08, -0.1022e-07, &
        -0.2359e-07, -0.3957e-07, -0.5553e-07, -0.6927e-07, -0.7849e-07, &
        -0.8139e-07, -0.7853e-07, -0.7368e-07, -0.7220e-07, -0.7780e-07, &
        -0.9091e-07, -0.1038e-06, -0.9929e-07, -0.5422e-07,  0.5379e-07, &
         0.2350e-06/
      data ((h81(ip,iw),iw=1,31), ip= 2, 2)/ &
         0.99998659,  0.99997360,  0.99994862,  0.99990171,  0.99981678, &
         0.99967158,  0.99944139,  0.99910921,  0.99867302,  0.99814397, &
         0.99753797,  0.99686003,  0.99609798,  0.99522603,  0.99420297, &
         0.99296701,  0.99142998,  0.98949999,  0.98706001,  0.98400998, &
         0.98021001,  0.97552001,  0.96976000,  0.96262002,  0.95367002, &
         0.94234002,  0.92781997,  0.90903997,  0.88459998,  0.85290003, &
         0.81200004/
      data ((h82(ip,iw),iw=1,31), ip= 2, 2)/ &
        -0.5684e-08, -0.1331e-07, -0.3248e-07, -0.8133e-07, -0.2047e-06, &
        -0.4971e-06, -0.1117e-05, -0.2245e-05, -0.3981e-05, -0.6287e-05, &
        -0.9035e-05, -0.1215e-04, -0.1565e-04, -0.1967e-04, -0.2444e-04, &
        -0.3036e-04, -0.3780e-04, -0.4694e-04, -0.5779e-04, -0.7042e-04, &
        -0.8491e-04, -0.1013e-03, -0.1196e-03, -0.1399e-03, -0.1625e-03, &
        -0.1879e-03, -0.2163e-03, -0.2474e-03, -0.2803e-03, -0.3140e-03, &
        -0.3478e-03/
      data ((h83(ip,iw),iw=1,31), ip= 2, 2)/ &
         0.2168e-10,  0.5242e-10,  0.1295e-09,  0.3201e-09,  0.7662e-09, &
         0.1690e-08,  0.3220e-08,  0.5106e-08,  0.6776e-08,  0.7673e-08, &
         0.7362e-08,  0.5808e-08,  0.3138e-08, -0.1595e-08, -0.1041e-07, &
        -0.2390e-07, -0.4010e-07, -0.5636e-07, -0.7045e-07, -0.7972e-07, &
        -0.8178e-07, -0.7677e-07, -0.6876e-07, -0.6381e-07, -0.6583e-07, &
        -0.7486e-07, -0.8229e-07, -0.7017e-07, -0.1497e-07,  0.1051e-06, &
         0.2990e-06/
      data ((h81(ip,iw),iw=1,31), ip= 3, 3)/ &
         0.99998659,  0.99997360,  0.99994862,  0.99990171,  0.99981678, &
         0.99967152,  0.99944133,  0.99910891,  0.99867201,  0.99814302, &
         0.99753499,  0.99685597,  0.99609101,  0.99521297,  0.99418002, &
         0.99292499,  0.99135399,  0.98935997,  0.98681003,  0.98356998, &
         0.97947001,  0.97430998,  0.96784997,  0.95972002,  0.94941002, &
         0.93620002,  0.91912001,  0.89690000,  0.86790001,  0.83020002, &
         0.78210002/
      data ((h82(ip,iw),iw=1,31), ip= 3, 3)/ &
        -0.5682e-08, -0.1330e-07, -0.3247e-07, -0.8129e-07, -0.2046e-06, &
        -0.4968e-06, -0.1117e-05, -0.2244e-05, -0.3978e-05, -0.6283e-05, &
        -0.9027e-05, -0.1213e-04, -0.1563e-04, -0.1963e-04, -0.2436e-04, &
        -0.3021e-04, -0.3754e-04, -0.4649e-04, -0.5709e-04, -0.6940e-04, &
        -0.8359e-04, -0.9986e-04, -0.1182e-03, -0.1388e-03, -0.1620e-03, &
        -0.1882e-03, -0.2175e-03, -0.2498e-03, -0.2843e-03, -0.3203e-03, &
        -0.3573e-03/
      data ((h83(ip,iw),iw=1,31), ip= 3, 3)/ &
         0.2167e-10,  0.5238e-10,  0.1294e-09,  0.3198e-09,  0.7656e-09, &
         0.1688e-08,  0.3217e-08,  0.5104e-08,  0.6767e-08,  0.7661e-08, &
         0.7337e-08,  0.5764e-08,  0.3051e-08, -0.1752e-08, -0.1068e-07, &
        -0.2436e-07, -0.4081e-07, -0.5740e-07, -0.7165e-07, -0.8046e-07, &
        -0.8082e-07, -0.7289e-07, -0.6141e-07, -0.5294e-07, -0.5134e-07, &
        -0.5552e-07, -0.5609e-07, -0.3464e-07,  0.3275e-07,  0.1669e-06, &
         0.3745e-06/
      data ((h81(ip,iw),iw=1,31), ip= 4, 4)/ &
         0.99998659,  0.99997360,  0.99994862,  0.99990171,  0.99981678, &
         0.99967140,  0.99944109,  0.99910849,  0.99867100,  0.99814099, &
         0.99753201,  0.99685001,  0.99607998,  0.99519402,  0.99414498, &
         0.99286002,  0.99123698,  0.98914999,  0.98644000,  0.98293000, &
         0.97842002,  0.97263998,  0.96529001,  0.95592999,  0.94392002, &
         0.92839003,  0.90815997,  0.88169998,  0.84720004,  0.80269998, &
         0.74629998/
      data ((h82(ip,iw),iw=1,31), ip= 4, 4)/ &
        -0.5680e-08, -0.1329e-07, -0.3243e-07, -0.8121e-07, -0.2044e-06, &
        -0.4963e-06, -0.1115e-05, -0.2242e-05, -0.3974e-05, -0.6276e-05, &
        -0.9015e-05, -0.1211e-04, -0.1559e-04, -0.1956e-04, -0.2423e-04, &
        -0.2999e-04, -0.3716e-04, -0.4588e-04, -0.5618e-04, -0.6818e-04, &
        -0.8218e-04, -0.9847e-04, -0.1171e-03, -0.1382e-03, -0.1621e-03, &
        -0.1892e-03, -0.2197e-03, -0.2535e-03, -0.2902e-03, -0.3293e-03, &
        -0.3700e-03/
      data ((h83(ip,iw),iw=1,31), ip= 4, 4)/ &
         0.2166e-10,  0.5229e-10,  0.1294e-09,  0.3193e-09,  0.7644e-09, &
         0.1686e-08,  0.3213e-08,  0.5092e-08,  0.6753e-08,  0.7640e-08, &
         0.7302e-08,  0.5696e-08,  0.2917e-08, -0.1984e-08, -0.1108e-07, &
        -0.2497e-07, -0.4171e-07, -0.5849e-07, -0.7254e-07, -0.8017e-07, &
        -0.7802e-07, -0.6662e-07, -0.5153e-07, -0.3961e-07, -0.3387e-07, &
        -0.3219e-07, -0.2426e-07,  0.8700e-08,  0.9027e-07,  0.2400e-06, &
         0.4623e-06/
      data ((h81(ip,iw),iw=1,31), ip= 5, 5)/ &
         0.99998659,  0.99997360,  0.99994862,  0.99990165,  0.99981672, &
         0.99967128,  0.99944091,  0.99910778,  0.99866998,  0.99813801, &
         0.99752700,  0.99684101,  0.99606299,  0.99516302,  0.99408901, &
         0.99276000,  0.99105698,  0.98882997,  0.98588997,  0.98202002, &
         0.97696000,  0.97039002,  0.96192998,  0.95104003,  0.93691999, &
         0.91851997,  0.89440000,  0.86290002,  0.82200003,  0.76969999, &
         0.70420003/
      data ((h82(ip,iw),iw=1,31), ip= 5, 5)/ &
        -0.5675e-08, -0.1328e-07, -0.3239e-07, -0.8110e-07, -0.2040e-06, &
        -0.4954e-06, -0.1114e-05, -0.2238e-05, -0.3968e-05, -0.6265e-05, &
        -0.8996e-05, -0.1208e-04, -0.1553e-04, -0.1945e-04, -0.2404e-04, &
        -0.2966e-04, -0.3663e-04, -0.4508e-04, -0.5508e-04, -0.6686e-04, &
        -0.8082e-04, -0.9732e-04, -0.1165e-03, -0.1382e-03, -0.1630e-03, &
        -0.1913e-03, -0.2234e-03, -0.2593e-03, -0.2989e-03, -0.3417e-03, &
        -0.3857e-03/
      data ((h83(ip,iw),iw=1,31), ip= 5, 5)/ &
         0.2163e-10,  0.5209e-10,  0.1291e-09,  0.3186e-09,  0.7626e-09, &
         0.1682e-08,  0.3203e-08,  0.5078e-08,  0.6730e-08,  0.7606e-08, &
         0.7246e-08,  0.5592e-08,  0.2735e-08, -0.2325e-08, -0.1162e-07, &
        -0.2576e-07, -0.4268e-07, -0.5938e-07, -0.7262e-07, -0.7827e-07, &
        -0.7297e-07, -0.5786e-07, -0.3930e-07, -0.2373e-07, -0.1295e-07, &
        -0.3728e-08,  0.1465e-07,  0.6114e-07,  0.1590e-06,  0.3257e-06, &
         0.5622e-06/
      data ((h81(ip,iw),iw=1,31), ip= 6, 6)/ &
         0.99998659,  0.99997360,  0.99994862,  0.99990165,  0.99981672, &
         0.99967122,  0.99944037,  0.99910682,  0.99866802,  0.99813402, &
         0.99751902,  0.99682599,  0.99603701,  0.99511498,  0.99400300, &
         0.99260598,  0.99078500,  0.98835999,  0.98510998,  0.98075998, &
         0.97499001,  0.96741998,  0.95757002,  0.94476998,  0.92804998, &
         0.90613002,  0.87739998,  0.83990002,  0.79159999,  0.73049998, &
         0.65540004/
      data ((h82(ip,iw),iw=1,31), ip= 6, 6)/ &
        -0.5671e-08, -0.1326e-07, -0.3234e-07, -0.8091e-07, -0.2035e-06, &
        -0.4941e-06, -0.1111e-05, -0.2232e-05, -0.3958e-05, -0.6247e-05, &
        -0.8966e-05, -0.1202e-04, -0.1544e-04, -0.1929e-04, -0.2377e-04, &
        -0.2921e-04, -0.3593e-04, -0.4409e-04, -0.5385e-04, -0.6555e-04, &
        -0.7965e-04, -0.9656e-04, -0.1163e-03, -0.1390e-03, -0.1649e-03, &
        -0.1947e-03, -0.2288e-03, -0.2675e-03, -0.3109e-03, -0.3575e-03, &
        -0.4039e-03/
      data ((h83(ip,iw),iw=1,31), ip= 6, 6)/ &
         0.2155e-10,  0.5188e-10,  0.1288e-09,  0.3175e-09,  0.7599e-09, &
         0.1675e-08,  0.3190e-08,  0.5059e-08,  0.6699e-08,  0.7551e-08, &
         0.7154e-08,  0.5435e-08,  0.2452e-08, -0.2802e-08, -0.1235e-07, &
        -0.2668e-07, -0.4353e-07, -0.5962e-07, -0.7134e-07, -0.7435e-07, &
        -0.6551e-07, -0.4676e-07, -0.2475e-07, -0.4876e-08,  0.1235e-07, &
         0.3092e-07,  0.6192e-07,  0.1243e-06,  0.2400e-06,  0.4247e-06, &
         0.6755e-06/
      data ((h81(ip,iw),iw=1,31), ip= 7, 7)/ &
         0.99998659,  0.99997360,  0.99994862,  0.99990165,  0.99981660, &
         0.99967092,  0.99943972,  0.99910510,  0.99866402,  0.99812698, &
         0.99750602,  0.99680197,  0.99599499,  0.99504000,  0.99387002, &
         0.99237198,  0.99038202,  0.98768002,  0.98400998,  0.97903001, &
         0.97236001,  0.96354002,  0.95196998,  0.93681002,  0.91688001, &
         0.89069998,  0.85640001,  0.81190002,  0.75520003,  0.68470001, &
         0.59990001/
      data ((h82(ip,iw),iw=1,31), ip= 7, 7)/ &
        -0.5665e-08, -0.1322e-07, -0.3224e-07, -0.8063e-07, -0.2027e-06, &
        -0.4921e-06, -0.1106e-05, -0.2223e-05, -0.3942e-05, -0.6220e-05, &
        -0.8920e-05, -0.1194e-04, -0.1530e-04, -0.1905e-04, -0.2337e-04, &
        -0.2860e-04, -0.3505e-04, -0.4296e-04, -0.5259e-04, -0.6439e-04, &
        -0.7884e-04, -0.9635e-04, -0.1170e-03, -0.1407e-03, -0.1681e-03, &
        -0.1998e-03, -0.2366e-03, -0.2790e-03, -0.3265e-03, -0.3763e-03, &
        -0.4235e-03/
      data ((h83(ip,iw),iw=1,31), ip= 7, 7)/ &
         0.2157e-10,  0.5178e-10,  0.1283e-09,  0.3162e-09,  0.7558e-09, &
         0.1665e-08,  0.3169e-08,  0.5027e-08,  0.6645e-08,  0.7472e-08, &
         0.7017e-08,  0.5212e-08,  0.2059e-08, -0.3443e-08, -0.1321e-07, &
        -0.2754e-07, -0.4389e-07, -0.5869e-07, -0.6825e-07, -0.6819e-07, &
        -0.5570e-07, -0.3343e-07, -0.7592e-08,  0.1778e-07,  0.4322e-07, &
         0.7330e-07,  0.1190e-06,  0.1993e-06,  0.3348e-06,  0.5376e-06, &
         0.8030e-06/
      data ((h81(ip,iw),iw=1,31), ip= 8, 8)/ &
         0.99998659,  0.99997360,  0.99994856,  0.99990159,  0.99981642, &
         0.99967051,  0.99943858,  0.99910247,  0.99865901,  0.99811602, &
         0.99748600,  0.99676597,  0.99592900,  0.99492502,  0.99366802, &
         0.99202400,  0.98979002,  0.98672003,  0.98249000,  0.97671002, &
         0.96890998,  0.95854002,  0.94483000,  0.92675000,  0.90289998, &
         0.87160003,  0.83069998,  0.77829999,  0.71239996,  0.63209999, &
         0.53839999/
      data ((h82(ip,iw),iw=1,31), ip= 8, 8)/ &
        -0.5652e-08, -0.1318e-07, -0.3210e-07, -0.8018e-07, -0.2014e-06, &
        -0.4888e-06, -0.1099e-05, -0.2210e-05, -0.3918e-05, -0.6179e-05, &
        -0.8849e-05, -0.1182e-04, -0.1509e-04, -0.1871e-04, -0.2284e-04, &
        -0.2782e-04, -0.3403e-04, -0.4177e-04, -0.5145e-04, -0.6354e-04, &
        -0.7853e-04, -0.9681e-04, -0.1185e-03, -0.1437e-03, -0.1729e-03, &
        -0.2072e-03, -0.2475e-03, -0.2942e-03, -0.3457e-03, -0.3973e-03, &
        -0.4434e-03/
      data ((h83(ip,iw),iw=1,31), ip= 8, 8)/ &
         0.2153e-10,  0.5151e-10,  0.1273e-09,  0.3136e-09,  0.7488e-09, &
         0.1649e-08,  0.3142e-08,  0.4980e-08,  0.6559e-08,  0.7346e-08, &
         0.6813e-08,  0.4884e-08,  0.1533e-08, -0.4209e-08, -0.1409e-07, &
        -0.2801e-07, -0.4320e-07, -0.5614e-07, -0.6312e-07, -0.5976e-07, &
        -0.4369e-07, -0.1775e-07,  0.1280e-07,  0.4534e-07,  0.8106e-07, &
         0.1246e-06,  0.1874e-06,  0.2873e-06,  0.4433e-06,  0.6651e-06, &
         0.9477e-06/
      data ((h81(ip,iw),iw=1,31), ip= 9, 9)/ &
         0.99998659,  0.99997360,  0.99994856,  0.99990153,  0.99981618, &
         0.99966979,  0.99943691,  0.99909842,  0.99865001,  0.99809903, &
         0.99745399,  0.99670798,  0.99582899,  0.99475002,  0.99336600, &
         0.99151403,  0.98896003,  0.98540002,  0.98044997,  0.97364998, &
         0.96445000,  0.95213997,  0.93579000,  0.91412002,  0.88550001, &
         0.84810001,  0.79970002,  0.73850000,  0.66280001,  0.57309997, &
         0.47189999/
      data ((h82(ip,iw),iw=1,31), ip= 9, 9)/ &
        -0.5629e-08, -0.1310e-07, -0.3186e-07, -0.7948e-07, -0.1995e-06, &
        -0.4837e-06, -0.1088e-05, -0.2188e-05, -0.3880e-05, -0.6115e-05, &
        -0.8743e-05, -0.1165e-04, -0.1480e-04, -0.1824e-04, -0.2216e-04, &
        -0.2691e-04, -0.3293e-04, -0.4067e-04, -0.5057e-04, -0.6314e-04, &
        -0.7885e-04, -0.9813e-04, -0.1212e-03, -0.1482e-03, -0.1799e-03, &
        -0.2175e-03, -0.2622e-03, -0.3135e-03, -0.3678e-03, -0.4193e-03, &
        -0.4627e-03/
      data ((h83(ip,iw),iw=1,31), ip= 9, 9)/ &
         0.2121e-10,  0.5076e-10,  0.1257e-09,  0.3091e-09,  0.7379e-09, &
         0.1623e-08,  0.3097e-08,  0.4904e-08,  0.6453e-08,  0.7168e-08, &
         0.6534e-08,  0.4458e-08,  0.8932e-09, -0.5026e-08, -0.1469e-07, &
        -0.2765e-07, -0.4103e-07, -0.5169e-07, -0.5585e-07, -0.4913e-07, &
        -0.2954e-07,  0.6372e-09,  0.3738e-07,  0.7896e-07,  0.1272e-06, &
         0.1867e-06,  0.2682e-06,  0.3895e-06,  0.5672e-06,  0.8091e-06, &
         0.1114e-05/
      data ((h81(ip,iw),iw=1,31), ip=10,10)/ &
         0.99998659,  0.99997360,  0.99994850,  0.99990141,  0.99981582, &
         0.99966878,  0.99943417,  0.99909198,  0.99863601,  0.99807203, &
         0.99740499,  0.99662101,  0.99567503,  0.99448699,  0.99292302, &
         0.99078500,  0.98780000,  0.98360002,  0.97773999,  0.96968001, &
         0.95872998,  0.94401997,  0.92440999,  0.89840001,  0.86409998, &
         0.81959999,  0.76279998,  0.69190001,  0.60650003,  0.50839996, &
         0.40249997/
      data ((h82(ip,iw),iw=1,31), ip=10,10)/ &
        -0.5597e-08, -0.1300e-07, -0.3148e-07, -0.7838e-07, -0.1964e-06, &
        -0.4759e-06, -0.1071e-05, -0.2155e-05, -0.3822e-05, -0.6019e-05, &
        -0.8586e-05, -0.1139e-04, -0.1439e-04, -0.1764e-04, -0.2134e-04, &
        -0.2591e-04, -0.3188e-04, -0.3978e-04, -0.5011e-04, -0.6334e-04, &
        -0.7998e-04, -0.1006e-03, -0.1253e-03, -0.1547e-03, -0.1895e-03, &
        -0.2315e-03, -0.2811e-03, -0.3363e-03, -0.3917e-03, -0.4413e-03, &
        -0.4809e-03/
      data ((h83(ip,iw),iw=1,31), ip=10,10)/ &
         0.2109e-10,  0.5017e-10,  0.1235e-09,  0.3021e-09,  0.7217e-09, &
         0.1585e-08,  0.3028e-08,  0.4796e-08,  0.6285e-08,  0.6910e-08, &
         0.6178e-08,  0.3945e-08,  0.2436e-09, -0.5632e-08, -0.1464e-07, &
        -0.2596e-07, -0.3707e-07, -0.4527e-07, -0.4651e-07, -0.3644e-07, &
        -0.1296e-07,  0.2250e-07,  0.6722e-07,  0.1202e-06,  0.1831e-06, &
         0.2605e-06,  0.3627e-06,  0.5062e-06,  0.7064e-06,  0.9725e-06, &
         0.1304e-05/
      data ((h81(ip,iw),iw=1,31), ip=11,11)/ &
         0.99998659,  0.99997354,  0.99994850,  0.99990124,  0.99981529, &
         0.99966723,  0.99942988,  0.99908209,  0.99861503,  0.99803102, &
         0.99732900,  0.99648702,  0.99544603,  0.99409997,  0.99228698, &
         0.98977000,  0.98620999,  0.98120999,  0.97421998,  0.96458000, &
         0.95146000,  0.93378001,  0.91017002,  0.87889999,  0.83810002, &
         0.78549999,  0.71930003,  0.63859999,  0.54409999,  0.43970001, &
         0.33249998/
      data ((h82(ip,iw),iw=1,31), ip=11,11)/ &
        -0.5538e-08, -0.1280e-07, -0.3089e-07, -0.7667e-07, -0.1917e-06, &
        -0.4642e-06, -0.1045e-05, -0.2106e-05, -0.3736e-05, -0.5878e-05, &
        -0.8363e-05, -0.1104e-04, -0.1387e-04, -0.1692e-04, -0.2044e-04, &
        -0.2493e-04, -0.3101e-04, -0.3926e-04, -0.5020e-04, -0.6429e-04, &
        -0.8213e-04, -0.1044e-03, -0.1314e-03, -0.1637e-03, -0.2027e-03, &
        -0.2498e-03, -0.3042e-03, -0.3617e-03, -0.4163e-03, -0.4625e-03, &
        -0.4969e-03/
      data ((h83(ip,iw),iw=1,31), ip=11,11)/ &
         0.2067e-10,  0.4903e-10,  0.1200e-09,  0.2917e-09,  0.6965e-09, &
         0.1532e-08,  0.2925e-08,  0.4632e-08,  0.6054e-08,  0.6590e-08, &
         0.5746e-08,  0.3436e-08, -0.2251e-09, -0.5703e-08, -0.1344e-07, &
        -0.2256e-07, -0.3120e-07, -0.3690e-07, -0.3520e-07, -0.2164e-07, &
         0.6510e-08,  0.4895e-07,  0.1037e-06,  0.1702e-06,  0.2502e-06, &
         0.3472e-06,  0.4710e-06,  0.6379e-06,  0.8633e-06,  0.1159e-05, &
         0.1514e-05/
      data ((h81(ip,iw),iw=1,31), ip=12,12)/ &
         0.99998659,  0.99997354,  0.99994838,  0.99990094,  0.99981439, &
         0.99966472,  0.99942350,  0.99906689,  0.99858302,  0.99796802, &
         0.99721497,  0.99628800,  0.99510801,  0.99354398,  0.99139601, &
         0.98838001,  0.98409998,  0.97807997,  0.96967000,  0.95806998, &
         0.94226003,  0.92093998,  0.89249998,  0.85510004,  0.80659997, &
         0.74510002,  0.66909999,  0.57870001,  0.47680002,  0.36919999, &
         0.26520002/
      data ((h82(ip,iw),iw=1,31), ip=12,12)/ &
        -0.5476e-08, -0.1257e-07, -0.3008e-07, -0.7418e-07, -0.1848e-06, &
        -0.4468e-06, -0.1006e-05, -0.2032e-05, -0.3611e-05, -0.5679e-05, &
        -0.8058e-05, -0.1059e-04, -0.1324e-04, -0.1612e-04, -0.1956e-04, &
        -0.2411e-04, -0.3046e-04, -0.3925e-04, -0.5098e-04, -0.6619e-04, &
        -0.8562e-04, -0.1100e-03, -0.1399e-03, -0.1761e-03, -0.2202e-03, &
        -0.2726e-03, -0.3306e-03, -0.3885e-03, -0.4404e-03, -0.4820e-03, &
        -0.5082e-03/
      data ((h83(ip,iw),iw=1,31), ip=12,12)/ &
         0.2041e-10,  0.4771e-10,  0.1149e-09,  0.2782e-09,  0.6614e-09, &
         0.1451e-08,  0.2778e-08,  0.4401e-08,  0.5736e-08,  0.6189e-08, &
         0.5315e-08,  0.3087e-08, -0.2518e-09, -0.4806e-08, -0.1071e-07, &
        -0.1731e-07, -0.2346e-07, -0.2659e-07, -0.2184e-07, -0.4261e-08, &
         0.2975e-07,  0.8112e-07,  0.1484e-06,  0.2308e-06,  0.3296e-06, &
         0.4475e-06,  0.5942e-06,  0.7859e-06,  0.1041e-05,  0.1369e-05, &
         0.1726e-05/
      data ((h81(ip,iw),iw=1,31), ip=13,13)/ &
         0.99998653,  0.99997348,  0.99994826,  0.99990052,  0.99981320, &
         0.99966109,  0.99941391,  0.99904412,  0.99853402,  0.99787498, &
         0.99704498,  0.99599600,  0.99462402,  0.99276501,  0.99017602, &
         0.98651999,  0.98133999,  0.97403997,  0.96386999,  0.94984001, &
         0.93071002,  0.90495998,  0.87080002,  0.82620001,  0.76910001, &
         0.69790000,  0.61199999,  0.51320004,  0.40640002,  0.29970002, &
         0.20359999/
      data ((h82(ip,iw),iw=1,31), ip=13,13)/ &
        -0.5362e-08, -0.1223e-07, -0.2895e-07, -0.7071e-07, -0.1748e-06, &
        -0.4219e-06, -0.9516e-06, -0.1928e-05, -0.3436e-05, -0.5409e-05, &
        -0.7666e-05, -0.1005e-04, -0.1254e-04, -0.1533e-04, -0.1880e-04, &
        -0.2358e-04, -0.3038e-04, -0.3988e-04, -0.5264e-04, -0.6934e-04, &
        -0.9083e-04, -0.1179e-03, -0.1515e-03, -0.1927e-03, -0.2424e-03, &
        -0.2994e-03, -0.3591e-03, -0.4155e-03, -0.4634e-03, -0.4982e-03, &
        -0.5096e-03/
      data ((h83(ip,iw),iw=1,31), ip=13,13)/ &
         0.1976e-10,  0.4551e-10,  0.1086e-09,  0.2601e-09,  0.6126e-09, &
         0.1345e-08,  0.2583e-08,  0.4112e-08,  0.5365e-08,  0.5796e-08, &
         0.5031e-08,  0.3182e-08,  0.5970e-09, -0.2547e-08, -0.6172e-08, &
        -0.1017e-07, -0.1388e-07, -0.1430e-07, -0.6118e-08,  0.1624e-07, &
         0.5791e-07,  0.1205e-06,  0.2025e-06,  0.3032e-06,  0.4225e-06, &
         0.5619e-06,  0.7322e-06,  0.9528e-06,  0.1243e-05,  0.1592e-05, &
         0.1904e-05/
      data ((h81(ip,iw),iw=1,31), ip=14,14)/ &
         0.99998653,  0.99997348,  0.99994808,  0.99989992,  0.99981129, &
         0.99965578,  0.99939990,  0.99901080,  0.99846399,  0.99773800, &
         0.99680001,  0.99558002,  0.99394703,  0.99169999,  0.98853999, &
         0.98408002,  0.97776002,  0.96888000,  0.95652002,  0.93949002, &
         0.91631001,  0.88529998,  0.84439999,  0.79159999,  0.72510004, &
         0.64390004,  0.54890001,  0.44379997,  0.33560002,  0.23449999, &
         0.15009999/
      data ((h82(ip,iw),iw=1,31), ip=14,14)/ &
        -0.5210e-08, -0.1172e-07, -0.2731e-07, -0.6598e-07, -0.1615e-06, &
        -0.3880e-06, -0.8769e-06, -0.1787e-05, -0.3204e-05, -0.5066e-05, &
        -0.7197e-05, -0.9451e-05, -0.1185e-04, -0.1465e-04, -0.1831e-04, &
        -0.2346e-04, -0.3088e-04, -0.4132e-04, -0.5545e-04, -0.7410e-04, &
        -0.9820e-04, -0.1288e-03, -0.1670e-03, -0.2140e-03, -0.2692e-03, &
        -0.3293e-03, -0.3886e-03, -0.4417e-03, -0.4840e-03, -0.5073e-03, &
        -0.4944e-03/
      data ((h83(ip,iw),iw=1,31), ip=14,14)/ &
         0.1880e-10,  0.4271e-10,  0.9966e-10,  0.2352e-09,  0.5497e-09, &
         0.1205e-08,  0.2334e-08,  0.3765e-08,  0.4993e-08,  0.5532e-08, &
         0.5148e-08,  0.4055e-08,  0.2650e-08,  0.1326e-08,  0.2019e-09, &
        -0.1124e-08, -0.2234e-08,  0.2827e-09,  0.1247e-07,  0.4102e-07, &
         0.9228e-07,  0.1682e-06,  0.2676e-06,  0.3885e-06,  0.5286e-06, &
         0.6904e-06,  0.8871e-06,  0.1142e-05,  0.1466e-05,  0.1800e-05, &
         0.2004e-05/
      data ((h81(ip,iw),iw=1,31), ip=15,15)/ &
         0.99998653,  0.99997336,  0.99994785,  0.99989909,  0.99980879, &
         0.99964851,  0.99938041,  0.99896401,  0.99836302,  0.99754399, &
         0.99645603,  0.99500400,  0.99302697,  0.99027801,  0.98640001, &
         0.98092002,  0.97319001,  0.96234000,  0.94727999,  0.92657000, &
         0.89850003,  0.86119998,  0.81260002,  0.75080001,  0.67429996, &
         0.58350003,  0.48089999,  0.37250000,  0.26760000,  0.17650002, &
         0.10610002/
      data ((h82(ip,iw),iw=1,31), ip=15,15)/ &
        -0.5045e-08, -0.1113e-07, -0.2540e-07, -0.6008e-07, -0.1449e-06, &
        -0.3457e-06, -0.7826e-06, -0.1609e-05, -0.2920e-05, -0.4665e-05, &
        -0.6691e-05, -0.8868e-05, -0.1127e-04, -0.1422e-04, -0.1820e-04, &
        -0.2389e-04, -0.3213e-04, -0.4380e-04, -0.5975e-04, -0.8092e-04, &
        -0.1083e-03, -0.1433e-03, -0.1873e-03, -0.2402e-03, -0.2997e-03, &
        -0.3607e-03, -0.4178e-03, -0.4662e-03, -0.4994e-03, -0.5028e-03, &
        -0.4563e-03/
      data ((h83(ip,iw),iw=1,31), ip=15,15)/ &
         0.1804e-10,  0.3983e-10,  0.9045e-10,  0.2080e-09,  0.4786e-09, &
         0.1046e-08,  0.2052e-08,  0.3413e-08,  0.4704e-08,  0.5565e-08, &
         0.5887e-08,  0.5981e-08,  0.6202e-08,  0.6998e-08,  0.8493e-08, &
         0.1002e-07,  0.1184e-07,  0.1780e-07,  0.3483e-07,  0.7122e-07, &
         0.1341e-06,  0.2259e-06,  0.3446e-06,  0.4866e-06,  0.6486e-06, &
         0.8343e-06,  0.1063e-05,  0.1356e-05,  0.1690e-05,  0.1951e-05, &
         0.2005e-05/
      data ((h81(ip,iw),iw=1,31), ip=16,16)/ &
         0.99998647,  0.99997330,  0.99994755,  0.99989808,  0.99980563, &
         0.99963909,  0.99935490,  0.99890202,  0.99822801,  0.99728203, &
         0.99599099,  0.99423301,  0.99181002,  0.98842001,  0.98364002, &
         0.97689003,  0.96740001,  0.95414001,  0.93575001,  0.91060001, &
         0.87680000,  0.83219999,  0.77490002,  0.70330000,  0.61689997, &
         0.51750004,  0.40990001,  0.30239999,  0.20539999,  0.12750000, &
         0.07150000/
      data ((h82(ip,iw),iw=1,31), ip=16,16)/ &
        -0.4850e-08, -0.1045e-07, -0.2334e-07, -0.5367e-07, -0.1265e-06, &
        -0.2980e-06, -0.6750e-06, -0.1406e-05, -0.2601e-05, -0.4239e-05, &
        -0.6201e-05, -0.8389e-05, -0.1091e-04, -0.1413e-04, -0.1859e-04, &
        -0.2500e-04, -0.3432e-04, -0.4761e-04, -0.6595e-04, -0.9030e-04, &
        -0.1219e-03, -0.1624e-03, -0.2126e-03, -0.2708e-03, -0.3327e-03, &
        -0.3926e-03, -0.4458e-03, -0.4871e-03, -0.5045e-03, -0.4777e-03, &
        -0.3954e-03/
      data ((h83(ip,iw),iw=1,31), ip=16,16)/ &
         0.1717e-10,  0.3723e-10,  0.8093e-10,  0.1817e-09,  0.4100e-09, &
         0.8932e-09,  0.1791e-08,  0.3126e-08,  0.4634e-08,  0.6095e-08, &
         0.7497e-08,  0.9170e-08,  0.1136e-07,  0.1453e-07,  0.1892e-07, &
         0.2369e-07,  0.2909e-07,  0.3922e-07,  0.6232e-07,  0.1083e-06, &
         0.1847e-06,  0.2943e-06,  0.4336e-06,  0.5970e-06,  0.7815e-06, &
         0.9959e-06,  0.1263e-05,  0.1583e-05,  0.1880e-05,  0.2009e-05, &
         0.1914e-05/
      data ((h81(ip,iw),iw=1,31), ip=17,17)/ &
         0.99998647,  0.99997318,  0.99994719,  0.99989688,  0.99980187, &
         0.99962789,  0.99932390,  0.99882400,  0.99805701,  0.99694502, &
         0.99538797,  0.99323398,  0.99023998,  0.98604000,  0.98013997, &
         0.97182000,  0.96016002,  0.94391000,  0.92149997,  0.89100003, &
         0.85049999,  0.79769999,  0.73089999,  0.64919996,  0.55350000, &
         0.44760001,  0.33870000,  0.23670000,  0.15149999,  0.08810002, &
         0.04570001/
      data ((h82(ip,iw),iw=1,31), ip=17,17)/ &
        -0.4673e-08, -0.9862e-08, -0.2135e-07, -0.4753e-07, -0.1087e-06, &
        -0.2512e-06, -0.5671e-06, -0.1199e-05, -0.2281e-05, -0.3842e-05, &
        -0.5804e-05, -0.8110e-05, -0.1088e-04, -0.1452e-04, -0.1961e-04, &
        -0.2696e-04, -0.3768e-04, -0.5311e-04, -0.7444e-04, -0.1028e-03, &
        -0.1397e-03, -0.1865e-03, -0.2427e-03, -0.3047e-03, -0.3667e-03, &
        -0.4237e-03, -0.4712e-03, -0.5003e-03, -0.4921e-03, -0.4286e-03, &
        -0.3188e-03/
      data ((h83(ip,iw),iw=1,31), ip=17,17)/ &
         0.1653e-10,  0.3436e-10,  0.7431e-10,  0.1605e-09,  0.3548e-09, &
         0.7723e-09,  0.1595e-08,  0.2966e-08,  0.4849e-08,  0.7169e-08, &
         0.1003e-07,  0.1366e-07,  0.1825e-07,  0.2419e-07,  0.3186e-07, &
         0.4068e-07,  0.5064e-07,  0.6618e-07,  0.9684e-07,  0.1536e-06, &
         0.2450e-06,  0.3730e-06,  0.5328e-06,  0.7184e-06,  0.9291e-06, &
         0.1180e-05,  0.1484e-05,  0.1798e-05,  0.1992e-05,  0.1968e-05, &
         0.1736e-05/
      data ((h81(ip,iw),iw=1,31), ip=18,18)/ &
         0.99998647,  0.99997312,  0.99994683,  0.99989569,  0.99979800, &
         0.99961591,  0.99928999,  0.99873698,  0.99785602,  0.99653602, &
         0.99464101,  0.99198103,  0.98825997,  0.98306000,  0.97574002, &
         0.96548998,  0.95117003,  0.93129998,  0.90407002,  0.86739999, &
         0.81910002,  0.75720000,  0.68040001,  0.58880001,  0.48530000, &
         0.37610000,  0.27029997,  0.17830002,  0.10720003,  0.05790001, &
         0.02740002/
      data ((h82(ip,iw),iw=1,31), ip=18,18)/ &
        -0.4532e-08, -0.9395e-08, -0.1978e-07, -0.4272e-07, -0.9442e-07, &
        -0.2124e-06, -0.4747e-06, -0.1017e-05, -0.2003e-05, -0.3524e-05, &
        -0.5567e-05, -0.8108e-05, -0.1127e-04, -0.1547e-04, -0.2138e-04, &
        -0.2996e-04, -0.4251e-04, -0.6059e-04, -0.8563e-04, -0.1190e-03, &
        -0.1623e-03, -0.2156e-03, -0.2767e-03, -0.3403e-03, -0.4006e-03, &
        -0.4530e-03, -0.4912e-03, -0.4995e-03, -0.4563e-03, -0.3592e-03, &
        -0.2383e-03/
      data ((h83(ip,iw),iw=1,31), ip=18,18)/ &
         0.1593e-10,  0.3276e-10,  0.6896e-10,  0.1476e-09,  0.3190e-09, &
         0.6944e-09,  0.1474e-08,  0.2935e-08,  0.5300e-08,  0.8697e-08, &
         0.1336e-07,  0.1946e-07,  0.2707e-07,  0.3637e-07,  0.4800e-07, &
         0.6187e-07,  0.7806e-07,  0.1008e-06,  0.1404e-06,  0.2089e-06, &
         0.3153e-06,  0.4613e-06,  0.6416e-06,  0.8506e-06,  0.1095e-05, &
         0.1387e-05,  0.1708e-05,  0.1956e-05,  0.2003e-05,  0.1836e-05, &
         0.1483e-05/
      data ((h81(ip,iw),iw=1,31), ip=19,19)/ &
         0.99998641,  0.99997300,  0.99994648,  0.99989462,  0.99979430, &
         0.99960452,  0.99925661,  0.99864697,  0.99763900,  0.99607199, &
         0.99376297,  0.99046898,  0.98584002,  0.97937000,  0.97031999, &
         0.95766997,  0.94010001,  0.91588002,  0.88300002,  0.83920002, &
         0.78230000,  0.71060002,  0.62360001,  0.52320004,  0.41450000, &
         0.30589998,  0.20789999,  0.12900001,  0.07239997,  0.03590000, &
         0.01539999/
      data ((h82(ip,iw),iw=1,31), ip=19,19)/ &
        -0.4448e-08, -0.9085e-08, -0.1877e-07, -0.3946e-07, -0.8472e-07, &
        -0.1852e-06, -0.4074e-06, -0.8791e-06, -0.1789e-05, -0.3314e-05, &
        -0.5521e-05, -0.8425e-05, -0.1215e-04, -0.1711e-04, -0.2407e-04, &
        -0.3421e-04, -0.4905e-04, -0.7032e-04, -0.9985e-04, -0.1394e-03, &
        -0.1897e-03, -0.2491e-03, -0.3132e-03, -0.3763e-03, -0.4332e-03, &
        -0.4786e-03, -0.5005e-03, -0.4775e-03, -0.3970e-03, -0.2794e-03, &
        -0.1652e-03/
      data ((h83(ip,iw),iw=1,31), ip=19,19)/ &
         0.1566e-10,  0.3219e-10,  0.6635e-10,  0.1400e-09,  0.2999e-09, &
         0.6513e-09,  0.1406e-08,  0.2953e-08,  0.5789e-08,  0.1037e-07, &
         0.1709e-07,  0.2623e-07,  0.3777e-07,  0.5159e-07,  0.6823e-07, &
         0.8864e-07,  0.1134e-06,  0.1461e-06,  0.1960e-06,  0.2761e-06, &
         0.3962e-06,  0.5583e-06,  0.7580e-06,  0.9957e-06,  0.1282e-05, &
         0.1607e-05,  0.1898e-05,  0.2020e-05,  0.1919e-05,  0.1623e-05, &
         0.1171e-05/
      data ((h81(ip,iw),iw=1,31), ip=20,20)/ &
         0.99998641,  0.99997294,  0.99994624,  0.99989372,  0.99979132, &
         0.99959481,  0.99922693,  0.99856299,  0.99742502,  0.99558598, &
         0.99278802,  0.98872000,  0.98295999,  0.97491002,  0.96368998, &
         0.94812000,  0.92662001,  0.89719999,  0.85780001,  0.80599999, &
         0.73969996,  0.65779996,  0.56130004,  0.45410001,  0.34369999, &
         0.24030000,  0.15390003,  0.08950001,  0.04640001,  0.02090001, &
         0.00800002/
      data ((h82(ip,iw),iw=1,31), ip=20,20)/ &
        -0.4403e-08, -0.8896e-08, -0.1818e-07, -0.3751e-07, -0.7880e-07, &
        -0.1683e-06, -0.3640e-06, -0.7852e-06, -0.1640e-05, -0.3191e-05, &
        -0.5634e-05, -0.9046e-05, -0.1355e-04, -0.1953e-04, -0.2786e-04, &
        -0.3995e-04, -0.5752e-04, -0.8256e-04, -0.1174e-03, -0.1638e-03, &
        -0.2211e-03, -0.2854e-03, -0.3507e-03, -0.4116e-03, -0.4633e-03, &
        -0.4966e-03, -0.4921e-03, -0.4309e-03, -0.3215e-03, -0.2016e-03, &
        -0.1061e-03/
      data ((h83(ip,iw),iw=1,31), ip=20,20)/ &
         0.1551e-10,  0.3147e-10,  0.6419e-10,  0.1356e-09,  0.2860e-09, &
         0.6178e-09,  0.1353e-08,  0.2934e-08,  0.6095e-08,  0.1174e-07, &
         0.2067e-07,  0.3346e-07,  0.5014e-07,  0.7024e-07,  0.9377e-07, &
         0.1226e-06,  0.1592e-06,  0.2056e-06,  0.2678e-06,  0.3584e-06, &
         0.4892e-06,  0.6651e-06,  0.8859e-06,  0.1160e-05,  0.1488e-05, &
         0.1814e-05,  0.2010e-05,  0.1984e-05,  0.1748e-05,  0.1338e-05, &
         0.8445e-06/
      data ((h81(ip,iw),iw=1,31), ip=21,21)/ &
         0.99998641,  0.99997288,  0.99994606,  0.99989301,  0.99978900, &
         0.99958712,  0.99920273,  0.99849200,  0.99723101,  0.99511403, &
         0.99177098,  0.98677999,  0.97962999,  0.96961999,  0.95573002, &
         0.93658000,  0.91036999,  0.87500000,  0.82800001,  0.76730001, &
         0.69110000,  0.59930003,  0.49479997,  0.38370001,  0.27590001, &
         0.18210000,  0.10949999,  0.05919999,  0.02800000,  0.01130003, &
         0.00389999/
      data ((h82(ip,iw),iw=1,31), ip=21,21)/ &
        -0.4379e-08, -0.8801e-08, -0.1782e-07, -0.3642e-07, -0.7536e-07, &
        -0.1581e-06, -0.3366e-06, -0.7227e-06, -0.1532e-05, -0.3106e-05, &
        -0.5810e-05, -0.9862e-05, -0.1540e-04, -0.2279e-04, -0.3292e-04, &
        -0.4738e-04, -0.6817e-04, -0.9765e-04, -0.1384e-03, -0.1918e-03, &
        -0.2551e-03, -0.3226e-03, -0.3876e-03, -0.4452e-03, -0.4883e-03, &
        -0.5005e-03, -0.4598e-03, -0.3633e-03, -0.2416e-03, -0.1349e-03, &
        -0.6278e-04/
      data ((h83(ip,iw),iw=1,31), ip=21,21)/ &
         0.1542e-10,  0.3111e-10,  0.6345e-10,  0.1310e-09,  0.2742e-09, &
         0.5902e-09,  0.1289e-08,  0.2826e-08,  0.6103e-08,  0.1250e-07, &
         0.2355e-07,  0.4041e-07,  0.6347e-07,  0.9217e-07,  0.1256e-06, &
         0.1658e-06,  0.2175e-06,  0.2824e-06,  0.3607e-06,  0.4614e-06, &
         0.6004e-06,  0.7880e-06,  0.1034e-05,  0.1349e-05,  0.1698e-05, &
         0.1965e-05,  0.2021e-05,  0.1857e-05,  0.1500e-05,  0.1015e-05, &
         0.5467e-06/
      data ((h81(ip,iw),iw=1,31), ip=22,22)/ &
         0.99998635,  0.99997288,  0.99994594,  0.99989259,  0.99978727, &
         0.99958128,  0.99918407,  0.99843502,  0.99706697,  0.99468601, &
         0.99077803,  0.98474997,  0.97593999,  0.96350998,  0.94633001, &
         0.92282999,  0.89100003,  0.84860003,  0.79330003,  0.72299999, &
         0.63670003,  0.53600001,  0.42580003,  0.31470001,  0.21410000, &
         0.13300002,  0.07470000,  0.03710002,  0.01580000,  0.00580001, &
         0.00169998/
      data ((h82(ip,iw),iw=1,31), ip=22,22)/ &
        -0.4366e-08, -0.8749e-08, -0.1761e-07, -0.3578e-07, -0.7322e-07, &
        -0.1517e-06, -0.3189e-06, -0.6785e-06, -0.1446e-05, -0.3014e-05, &
        -0.5933e-05, -0.1069e-04, -0.1755e-04, -0.2683e-04, -0.3936e-04, &
        -0.5675e-04, -0.8137e-04, -0.1160e-03, -0.1630e-03, -0.2223e-03, &
        -0.2899e-03, -0.3589e-03, -0.4230e-03, -0.4755e-03, -0.5031e-03, &
        -0.4834e-03, -0.4036e-03, -0.2849e-03, -0.1687e-03, -0.8356e-04, &
        -0.3388e-04/
      data ((h83(ip,iw),iw=1,31), ip=22,22)/ &
         0.1536e-10,  0.3086e-10,  0.6248e-10,  0.1288e-09,  0.2664e-09, &
         0.5637e-09,  0.1222e-08,  0.2680e-08,  0.5899e-08,  0.1262e-07, &
         0.2527e-07,  0.4621e-07,  0.7678e-07,  0.1165e-06,  0.1640e-06, &
         0.2199e-06,  0.2904e-06,  0.3783e-06,  0.4787e-06,  0.5925e-06, &
         0.7377e-06,  0.9389e-06,  0.1216e-05,  0.1560e-05,  0.1879e-05, &
         0.2025e-05,  0.1940e-05,  0.1650e-05,  0.1194e-05,  0.6981e-06, &
         0.3103e-06/
      data ((h81(ip,iw),iw=1,31), ip=23,23)/ &
         0.99998635,  0.99997282,  0.99994588,  0.99989229,  0.99978608, &
         0.99957722,  0.99917048,  0.99839097,  0.99693698,  0.99432403, &
         0.98987001,  0.98273998,  0.97201002,  0.95668000,  0.93548000, &
         0.90671998,  0.86830002,  0.81800002,  0.75330001,  0.67299998, &
         0.57720000,  0.46920002,  0.35659999,  0.25010002,  0.16049999, &
         0.09350002,  0.04850000,  0.02179998,  0.00840002,  0.00269997, &
         0.00070000/
      data ((h82(ip,iw),iw=1,31), ip=23,23)/ &
        -0.4359e-08, -0.8720e-08, -0.1749e-07, -0.3527e-07, -0.7175e-07, &
        -0.1473e-06, -0.3062e-06, -0.6451e-06, -0.1372e-05, -0.2902e-05, &
        -0.5936e-05, -0.1133e-04, -0.1971e-04, -0.3143e-04, -0.4715e-04, &
        -0.6833e-04, -0.9759e-04, -0.1379e-03, -0.1907e-03, -0.2542e-03, &
        -0.3239e-03, -0.3935e-03, -0.4559e-03, -0.4991e-03, -0.5009e-03, &
        -0.4414e-03, -0.3306e-03, -0.2077e-03, -0.1093e-03, -0.4754e-04, &
        -0.1642e-04/
      data ((h83(ip,iw),iw=1,31), ip=23,23)/ &
         0.1531e-10,  0.3070e-10,  0.6184e-10,  0.1257e-09,  0.2578e-09, &
         0.5451e-09,  0.1159e-08,  0.2526e-08,  0.5585e-08,  0.1225e-07, &
         0.2576e-07,  0.5017e-07,  0.8855e-07,  0.1417e-06,  0.2078e-06, &
         0.2858e-06,  0.3802e-06,  0.4946e-06,  0.6226e-06,  0.7572e-06, &
         0.9137e-06,  0.1133e-05,  0.1438e-05,  0.1772e-05,  0.1994e-05, &
         0.1994e-05,  0.1779e-05,  0.1375e-05,  0.8711e-06,  0.4273e-06, &
         0.1539e-06/
      data ((h81(ip,iw),iw=1,31), ip=24,24)/ &
         0.99998635,  0.99997282,  0.99994582,  0.99989212,  0.99978542, &
         0.99957442,  0.99916071,  0.99835902,  0.99683702,  0.99403203, &
         0.98908001,  0.98084998,  0.96805000,  0.94933999,  0.92330998, &
         0.88830000,  0.84219998,  0.78270000,  0.70809996,  0.61759996, &
         0.51330000,  0.40079999,  0.29000002,  0.19239998,  0.11619997, &
         0.06300002,  0.02980000,  0.01200002,  0.00410002,  0.00120002, &
         0.00019997/
      data ((h82(ip,iw),iw=1,31), ip=24,24)/ &
        -0.4354e-08, -0.8703e-08, -0.1742e-07, -0.3499e-07, -0.7074e-07, &
        -0.1441e-06, -0.2971e-06, -0.6195e-06, -0.1309e-05, -0.2780e-05, &
        -0.5823e-05, -0.1165e-04, -0.2152e-04, -0.3616e-04, -0.5604e-04, &
        -0.8230e-04, -0.1173e-03, -0.1635e-03, -0.2211e-03, -0.2868e-03, &
        -0.3567e-03, -0.4260e-03, -0.4844e-03, -0.5097e-03, -0.4750e-03, &
        -0.3779e-03, -0.2522e-03, -0.1409e-03, -0.6540e-04, -0.2449e-04, &
        -0.6948e-05/
      data ((h83(ip,iw),iw=1,31), ip=24,24)/ &
         0.1529e-10,  0.3060e-10,  0.6142e-10,  0.1241e-09,  0.2535e-09, &
         0.5259e-09,  0.1107e-08,  0.2383e-08,  0.5243e-08,  0.1161e-07, &
         0.2523e-07,  0.5188e-07,  0.9757e-07,  0.1657e-06,  0.2553e-06, &
         0.3629e-06,  0.4878e-06,  0.6323e-06,  0.7923e-06,  0.9575e-06, &
         0.1139e-05,  0.1381e-05,  0.1687e-05,  0.1952e-05,  0.2029e-05, &
         0.1890e-05,  0.1552e-05,  0.1062e-05,  0.5728e-06,  0.2280e-06, &
         0.6762e-07/
      data ((h81(ip,iw),iw=1,31), ip=25,25)/ &
         0.99998635,  0.99997282,  0.99994582,  0.99989200,  0.99978489, &
         0.99957252,  0.99915391,  0.99833602,  0.99676299,  0.99380499, &
         0.98843998,  0.97920001,  0.96427000,  0.94182003,  0.91018999, &
         0.86769998,  0.81260002,  0.74300003,  0.65770000,  0.55750000, &
         0.44660002,  0.33310002,  0.22860003,  0.14319998,  0.08090001, &
         0.04030001,  0.01719999,  0.00620002,  0.00190002,  0.00040001, &
         0.00000000/
      data ((h82(ip,iw),iw=1,31), ip=25,25)/ &
        -0.4352e-08, -0.8693e-08, -0.1738e-07, -0.3483e-07, -0.7006e-07, &
        -0.1423e-06, -0.2905e-06, -0.6008e-06, -0.1258e-05, -0.2663e-05, &
        -0.5638e-05, -0.1165e-04, -0.2270e-04, -0.4044e-04, -0.6554e-04, &
        -0.9855e-04, -0.1407e-03, -0.1928e-03, -0.2534e-03, -0.3197e-03, &
        -0.3890e-03, -0.4563e-03, -0.5040e-03, -0.4998e-03, -0.4249e-03, &
        -0.3025e-03, -0.1794e-03, -0.8860e-04, -0.3575e-04, -0.1122e-04, &
        -0.2506e-05/
      data ((h83(ip,iw),iw=1,31), ip=25,25)/ &
         0.1527e-10,  0.3053e-10,  0.6115e-10,  0.1230e-09,  0.2492e-09, &
         0.5149e-09,  0.1068e-08,  0.2268e-08,  0.4932e-08,  0.1089e-07, &
         0.2408e-07,  0.5156e-07,  0.1028e-06,  0.1859e-06,  0.3028e-06, &
         0.4476e-06,  0.6124e-06,  0.7932e-06,  0.9879e-06,  0.1194e-05, &
         0.1417e-05,  0.1673e-05,  0.1929e-05,  0.2064e-05,  0.1997e-05, &
         0.1725e-05,  0.1267e-05,  0.7464e-06,  0.3312e-06,  0.1066e-06, &
         0.2718e-07/
      data ((h81(ip,iw),iw=1,31), ip=26,26)/ &
         0.99998635,  0.99997282,  0.99994576,  0.99989188,  0.99978459, &
         0.99957132,  0.99914938,  0.99831998,  0.99670899,  0.99363601, &
         0.98794001,  0.97781998,  0.96087998,  0.93456000,  0.89670002, &
         0.84560001,  0.78020000,  0.69920003,  0.60299999,  0.49400002, &
         0.37910002,  0.26889998,  0.17460001,  0.10280001,  0.05379999, &
         0.02429998,  0.00929999,  0.00300002,  0.00080001,  0.00010002, &
         0.00000000/
      data ((h82(ip,iw),iw=1,31), ip=26,26)/ &
        -0.4351e-08, -0.8688e-08, -0.1736e-07, -0.3473e-07, -0.6966e-07, &
        -0.1405e-06, -0.2857e-06, -0.5867e-06, -0.1218e-05, -0.2563e-05, &
        -0.5435e-05, -0.1144e-04, -0.2321e-04, -0.4379e-04, -0.7487e-04, &
        -0.1163e-03, -0.1670e-03, -0.2250e-03, -0.2876e-03, -0.3535e-03, &
        -0.4215e-03, -0.4826e-03, -0.5082e-03, -0.4649e-03, -0.3564e-03, &
        -0.2264e-03, -0.1188e-03, -0.5128e-04, -0.1758e-04, -0.4431e-05, &
        -0.7275e-06/
      data ((h83(ip,iw),iw=1,31), ip=26,26)/ &
         0.1525e-10,  0.3048e-10,  0.6097e-10,  0.1223e-09,  0.2466e-09, &
         0.5021e-09,  0.1032e-08,  0.2195e-08,  0.4688e-08,  0.1027e-07, &
         0.2279e-07,  0.4999e-07,  0.1046e-06,  0.2009e-06,  0.3460e-06, &
         0.5335e-06,  0.7478e-06,  0.9767e-06,  0.1216e-05,  0.1469e-05, &
         0.1735e-05,  0.1977e-05,  0.2121e-05,  0.2103e-05,  0.1902e-05, &
         0.1495e-05,  0.9541e-06,  0.4681e-06,  0.1672e-06,  0.4496e-07, &
         0.9859e-08/
      data ((c1(ip,iw),iw=1,30), ip= 1, 1)/ &
         0.99985647,  0.99976432,  0.99963892,  0.99948031,  0.99927652, &
         0.99899602,  0.99860001,  0.99804801,  0.99732202,  0.99640399, &
         0.99526399,  0.99384302,  0.99204999,  0.98979002,  0.98694998, &
         0.98334998,  0.97878999,  0.97307003,  0.96592999,  0.95722002, &
         0.94660002,  0.93366003,  0.91777998,  0.89819998,  0.87419999, &
         0.84500003,  0.81029999,  0.76989996,  0.72440004,  0.67490000/
      data ((c2(ip,iw),iw=1,30), ip= 1, 1)/ &
        -0.1841e-06, -0.4666e-06, -0.1050e-05, -0.2069e-05, -0.3601e-05, &
        -0.5805e-05, -0.8863e-05, -0.1291e-04, -0.1806e-04, -0.2460e-04, &
        -0.3317e-04, -0.4452e-04, -0.5944e-04, -0.7884e-04, -0.1036e-03, &
        -0.1346e-03, -0.1727e-03, -0.2186e-03, -0.2728e-03, -0.3364e-03, &
        -0.4102e-03, -0.4948e-03, -0.5890e-03, -0.6900e-03, -0.7930e-03, &
        -0.8921e-03, -0.9823e-03, -0.1063e-02, -0.1138e-02, -0.1214e-02/
      data ((c3(ip,iw),iw=1,30), ip= 1, 1)/ &
         0.5821e-10,  0.5821e-10, -0.3201e-09, -0.1804e-08, -0.4336e-08, &
        -0.7829e-08, -0.1278e-07, -0.1847e-07, -0.2827e-07, -0.4495e-07, &
        -0.7126e-07, -0.1071e-06, -0.1524e-06, -0.2160e-06, -0.3014e-06, &
        -0.4097e-06, -0.5349e-06, -0.6718e-06, -0.8125e-06, -0.9755e-06, &
        -0.1157e-05, -0.1339e-05, -0.1492e-05, -0.1563e-05, -0.1485e-05, &
        -0.1210e-05, -0.7280e-06, -0.1107e-06,  0.5369e-06,  0.1154e-05/
      data ((c1(ip,iw),iw=1,30), ip= 2, 2)/ &
         0.99985647,  0.99976432,  0.99963868,  0.99947977,  0.99927580, &
         0.99899501,  0.99859601,  0.99804401,  0.99731201,  0.99638498, &
         0.99523097,  0.99378198,  0.99194402,  0.98961002,  0.98664999, &
         0.98286998,  0.97807002,  0.97200000,  0.96439999,  0.95503998, &
         0.94352001,  0.92931998,  0.91175002,  0.88989997,  0.86300004, &
         0.83039999,  0.79159999,  0.74710000,  0.69790000,  0.64579999/
      data ((c2(ip,iw),iw=1,30), ip= 2, 2)/ &
        -0.1831e-06, -0.4642e-06, -0.1048e-05, -0.2067e-05, -0.3596e-05, &
        -0.5797e-05, -0.8851e-05, -0.1289e-04, -0.1802e-04, -0.2454e-04, &
        -0.3307e-04, -0.4435e-04, -0.5916e-04, -0.7842e-04, -0.1031e-03, &
        -0.1342e-03, -0.1725e-03, -0.2189e-03, -0.2739e-03, -0.3386e-03, &
        -0.4138e-03, -0.5003e-03, -0.5968e-03, -0.7007e-03, -0.8076e-03, &
        -0.9113e-03, -0.1007e-02, -0.1096e-02, -0.1181e-02, -0.1271e-02/
      data ((c3(ip,iw),iw=1,30), ip= 2, 2)/ &
         0.5821e-10,  0.5821e-10, -0.3347e-09, -0.1746e-08, -0.4366e-08, &
        -0.7858e-08, -0.1262e-07, -0.1866e-07, -0.2849e-07, -0.4524e-07, &
        -0.7176e-07, -0.1077e-06, -0.1531e-06, -0.2166e-06, -0.3018e-06, &
        -0.4090e-06, -0.5327e-06, -0.6670e-06, -0.8088e-06, -0.9714e-06, &
        -0.1151e-05, -0.1333e-05, -0.1483e-05, -0.1548e-05, -0.1467e-05, &
        -0.1192e-05, -0.7159e-06, -0.1032e-06,  0.5571e-06,  0.1217e-05/
      data ((c1(ip,iw),iw=1,30), ip= 3, 3)/ &
         0.99985671,  0.99976432,  0.99963838,  0.99947912,  0.99927449, &
         0.99899203,  0.99859202,  0.99803501,  0.99729699,  0.99635702, &
         0.99518001,  0.99369103,  0.99178600,  0.98935002,  0.98623002, &
         0.98223001,  0.97711003,  0.97060001,  0.96243000,  0.95222998, &
         0.93957001,  0.92379999,  0.90411001,  0.87959999,  0.84930003, &
         0.81270003,  0.76980001,  0.72140002,  0.66909999,  0.61539996/
      data ((c2(ip,iw),iw=1,30), ip= 3, 3)/ &
        -0.1831e-06, -0.4623e-06, -0.1048e-05, -0.2065e-05, -0.3589e-05, &
        -0.5789e-05, -0.8833e-05, -0.1286e-04, -0.1797e-04, -0.2446e-04, &
        -0.3292e-04, -0.4412e-04, -0.5880e-04, -0.7795e-04, -0.1027e-03, &
        -0.1340e-03, -0.1728e-03, -0.2199e-03, -0.2759e-03, -0.3419e-03, &
        -0.4194e-03, -0.5081e-03, -0.6078e-03, -0.7156e-03, -0.8270e-03, &
        -0.9365e-03, -0.1040e-02, -0.1137e-02, -0.1235e-02, -0.1339e-02/
      data ((c3(ip,iw),iw=1,30), ip= 3, 3)/ &
         0.2910e-10,  0.5821e-10, -0.3201e-09, -0.1732e-08, -0.4307e-08, &
        -0.7843e-08, -0.1270e-07, -0.1882e-07, -0.2862e-07, -0.4571e-07, &
        -0.7225e-07, -0.1082e-06, -0.1535e-06, -0.2171e-06, -0.3021e-06, &
        -0.4084e-06, -0.5302e-06, -0.6615e-06, -0.8059e-06, -0.9668e-06, &
        -0.1146e-05, -0.1325e-05, -0.1468e-05, -0.1530e-05, -0.1448e-05, &
        -0.1168e-05, -0.6907e-06, -0.7148e-07,  0.6242e-06,  0.1357e-05/
      data ((c1(ip,iw),iw=1,30), ip= 4, 4)/ &
         0.99985629,  0.99976349,  0.99963838,  0.99947798,  0.99927282, &
         0.99898797,  0.99858499,  0.99802202,  0.99727303,  0.99631298, &
         0.99510002,  0.99355298,  0.99155599,  0.98898000,  0.98566002, &
         0.98136997,  0.97584999,  0.96880001,  0.95986998,  0.94862998, &
         0.93452001,  0.91681999,  0.89459997,  0.86680001,  0.83270001, &
         0.79189998,  0.74479997,  0.69290000,  0.63839996,  0.58410001/
      data ((c2(ip,iw),iw=1,30), ip= 4, 4)/ &
        -0.1808e-06, -0.4642e-06, -0.1045e-05, -0.2058e-05, -0.3581e-05, &
        -0.5776e-05, -0.8801e-05, -0.1281e-04, -0.1789e-04, -0.2433e-04, &
        -0.3273e-04, -0.4382e-04, -0.5840e-04, -0.7755e-04, -0.1024e-03, &
        -0.1342e-03, -0.1737e-03, -0.2217e-03, -0.2791e-03, -0.3473e-03, &
        -0.4272e-03, -0.5191e-03, -0.6227e-03, -0.7354e-03, -0.8526e-03, &
        -0.9688e-03, -0.1081e-02, -0.1189e-02, -0.1300e-02, -0.1417e-02/
      data ((c3(ip,iw),iw=1,30), ip= 4, 4)/ &
         0.1019e-09,  0.1601e-09, -0.4075e-09, -0.1746e-08, -0.4366e-08, &
        -0.7960e-08, -0.1294e-07, -0.1898e-07, -0.2899e-07, -0.4594e-07, &
        -0.7267e-07, -0.1088e-06, -0.1536e-06, -0.2164e-06, -0.3002e-06, &
        -0.4055e-06, -0.5260e-06, -0.6571e-06, -0.8022e-06, -0.9624e-06, &
        -0.1139e-05, -0.1315e-05, -0.1456e-05, -0.1512e-05, -0.1420e-05, &
        -0.1137e-05, -0.6483e-06,  0.6679e-08,  0.7652e-06,  0.1574e-05/
      data ((c1(ip,iw),iw=1,30), ip= 5, 5)/ &
         0.99985641,  0.99976403,  0.99963748,  0.99947661,  0.99926913, &
         0.99898303,  0.99857402,  0.99800003,  0.99723399,  0.99624503, &
         0.99498397,  0.99335301,  0.99123502,  0.98847997,  0.98488998, &
         0.98023999,  0.97421998,  0.96648002,  0.95659000,  0.94404000, &
         0.92815000,  0.90802002,  0.88270003,  0.85119998,  0.81290001, &
         0.76770002,  0.71679997,  0.66219997,  0.60670000,  0.55250001/
      data ((c2(ip,iw),iw=1,30), ip= 5, 5)/ &
        -0.1827e-06, -0.4608e-06, -0.1042e-05, -0.2053e-05, -0.3565e-05, &
        -0.5745e-05, -0.8758e-05, -0.1273e-04, -0.1778e-04, -0.2417e-04, &
        -0.3250e-04, -0.4347e-04, -0.5801e-04, -0.7729e-04, -0.1025e-03, &
        -0.1349e-03, -0.1755e-03, -0.2249e-03, -0.2842e-03, -0.3549e-03, &
        -0.4380e-03, -0.5340e-03, -0.6428e-03, -0.7613e-03, -0.8854e-03, &
        -0.1009e-02, -0.1131e-02, -0.1252e-02, -0.1376e-02, -0.1502e-02/
      data ((c3(ip,iw),iw=1,30), ip= 5, 5)/ &
         0.4366e-10, -0.1455e-10, -0.4075e-09, -0.1804e-08, -0.4293e-08, &
        -0.8178e-08, -0.1301e-07, -0.1915e-07, -0.2938e-07, -0.4664e-07, &
        -0.7365e-07, -0.1090e-06, -0.1539e-06, -0.2158e-06, -0.2992e-06, &
        -0.4033e-06, -0.5230e-06, -0.6537e-06, -0.7976e-06, -0.9601e-06, &
        -0.1135e-05, -0.1305e-05, -0.1440e-05, -0.1490e-05, -0.1389e-05, &
        -0.1087e-05, -0.5646e-06,  0.1475e-06,  0.9852e-06,  0.1853e-05/
      data ((c1(ip,iw),iw=1,30), ip= 6, 6)/ &
         0.99985617,  0.99976331,  0.99963629,  0.99947429,  0.99926388, &
         0.99897301,  0.99855602,  0.99796802,  0.99717802,  0.99614400, &
         0.99480897,  0.99306899,  0.99078500,  0.98778999,  0.98387998, &
         0.97876000,  0.97211999,  0.96350002,  0.95240998,  0.93821001, &
         0.92009002,  0.89709997,  0.86820000,  0.83249998,  0.78970003, &
         0.74039996,  0.68630004,  0.63010001,  0.57459998,  0.52069998/
      data ((c2(ip,iw),iw=1,30), ip= 6, 6)/ &
        -0.1798e-06, -0.4580e-06, -0.1033e-05, -0.2039e-05, -0.3544e-05, &
        -0.5709e-05, -0.8696e-05, -0.1264e-04, -0.1763e-04, -0.2395e-04, &
        -0.3220e-04, -0.4311e-04, -0.5777e-04, -0.7732e-04, -0.1032e-03, &
        -0.1365e-03, -0.1784e-03, -0.2295e-03, -0.2914e-03, -0.3653e-03, &
        -0.4527e-03, -0.5541e-03, -0.6689e-03, -0.7947e-03, -0.9265e-03, &
        -0.1060e-02, -0.1192e-02, -0.1326e-02, -0.1460e-02, -0.1586e-02/
      data ((c3(ip,iw),iw=1,30), ip= 6, 6)/ &
         0.8731e-10,  0.0000e+00, -0.3492e-09, -0.1892e-08, -0.4322e-08, &
        -0.8367e-08, -0.1318e-07, -0.1962e-07, -0.3024e-07, -0.4708e-07, &
        -0.7359e-07, -0.1087e-06, -0.1534e-06, -0.2152e-06, -0.2978e-06, &
        -0.4008e-06, -0.5207e-06, -0.6509e-06, -0.7968e-06, -0.9584e-06, &
        -0.1128e-05, -0.1297e-05, -0.1425e-05, -0.1461e-05, -0.1342e-05, &
        -0.1009e-05, -0.4283e-06,  0.3666e-06,  0.1272e-05,  0.2171e-05/
      data ((c1(ip,iw),iw=1,30), ip= 7, 7)/ &
         0.99985600,  0.99976230,  0.99963462,  0.99947017,  0.99925607, &
         0.99895698,  0.99852800,  0.99791902,  0.99709100,  0.99599499, &
         0.99456000,  0.99267203,  0.99017102,  0.98688000,  0.98255002, &
         0.97685999,  0.96941000,  0.95969999,  0.94709998,  0.93085998, &
         0.91001999,  0.88360000,  0.85060000,  0.81040001,  0.76319999, &
         0.71029997,  0.65400004,  0.59740001,  0.54229999,  0.48839998/
      data ((c2(ip,iw),iw=1,30), ip= 7, 7)/ &
        -0.1784e-06, -0.4551e-06, -0.1023e-05, -0.2019e-05, -0.3507e-05, &
        -0.5651e-05, -0.8608e-05, -0.1250e-04, -0.1744e-04, -0.2370e-04, &
        -0.3189e-04, -0.4289e-04, -0.5777e-04, -0.7787e-04, -0.1045e-03, &
        -0.1392e-03, -0.1828e-03, -0.2365e-03, -0.3015e-03, -0.3797e-03, &
        -0.4723e-03, -0.5803e-03, -0.7026e-03, -0.8365e-03, -0.9772e-03, &
        -0.1120e-02, -0.1265e-02, -0.1409e-02, -0.1547e-02, -0.1665e-02/
      data ((c3(ip,iw),iw=1,30), ip= 7, 7)/ &
         0.5821e-10,  0.8731e-10, -0.4366e-09, -0.1935e-08, -0.4555e-08, &
        -0.8455e-08, -0.1356e-07, -0.2024e-07, -0.3079e-07, -0.4758e-07, &
        -0.7352e-07, -0.1078e-06, -0.1520e-06, -0.2139e-06, -0.2964e-06, &
        -0.3997e-06, -0.5185e-06, -0.6493e-06, -0.7943e-06, -0.9568e-06, &
        -0.1127e-05, -0.1288e-05, -0.1405e-05, -0.1425e-05, -0.1275e-05, &
        -0.8809e-06, -0.2158e-06,  0.6597e-06,  0.1610e-05,  0.2524e-05/
      data ((c1(ip,iw),iw=1,30), ip= 8, 8)/ &
         0.99985582,  0.99976122,  0.99963123,  0.99946368,  0.99924308, &
         0.99893397,  0.99848598,  0.99784499,  0.99696398,  0.99577999, &
         0.99421299,  0.99212801,  0.98935997,  0.98569000,  0.98083001, &
         0.97442001,  0.96595001,  0.95486999,  0.94040000,  0.92163002, &
         0.89760000,  0.86720002,  0.82969999,  0.78499997,  0.73370004, &
         0.67799997,  0.62070000,  0.56439996,  0.50960004,  0.45539999/
      data ((c2(ip,iw),iw=1,30), ip= 8, 8)/ &
        -0.1760e-06, -0.4451e-06, -0.1004e-05, -0.1989e-05, -0.3457e-05, &
        -0.5574e-05, -0.8470e-05, -0.1230e-04, -0.1721e-04, -0.2344e-04, &
        -0.3168e-04, -0.4286e-04, -0.5815e-04, -0.7898e-04, -0.1070e-03, &
        -0.1434e-03, -0.1892e-03, -0.2460e-03, -0.3152e-03, -0.3985e-03, &
        -0.4981e-03, -0.6139e-03, -0.7448e-03, -0.8878e-03, -0.1038e-02, &
        -0.1193e-02, -0.1348e-02, -0.1499e-02, -0.1631e-02, -0.1735e-02/
      data ((c3(ip,iw),iw=1,30), ip= 8, 8)/ &
        -0.1455e-10,  0.4366e-10, -0.3929e-09, -0.2081e-08, -0.4700e-08, &
        -0.8804e-08, -0.1417e-07, -0.2068e-07, -0.3143e-07, -0.4777e-07, &
        -0.7336e-07, -0.1070e-06, -0.1517e-06, -0.2134e-06, -0.2967e-06, &
        -0.3991e-06, -0.5164e-06, -0.6510e-06, -0.7979e-06, -0.9575e-06, &
        -0.1123e-05, -0.1279e-05, -0.1382e-05, -0.1374e-05, -0.1166e-05, &
        -0.6893e-06,  0.7339e-07,  0.1013e-05,  0.1982e-05,  0.2896e-05/
      data ((c1(ip,iw),iw=1,30), ip= 9, 9)/ &
         0.99985498,  0.99975908,  0.99962622,  0.99945402,  0.99922228, &
         0.99889803,  0.99842203,  0.99773699,  0.99677801,  0.99547797, &
         0.99373603,  0.99140298,  0.98829001,  0.98413998,  0.97863001, &
         0.97127002,  0.96156001,  0.94875997,  0.93197000,  0.91017997, &
         0.88230002,  0.84749997,  0.80540001,  0.75620002,  0.70159996, &
         0.64429998,  0.58710003,  0.53130001,  0.47640002,  0.42189997/
      data ((c2(ip,iw),iw=1,30), ip= 9, 9)/ &
        -0.1717e-06, -0.4327e-06, -0.9759e-06, -0.1943e-05, -0.3391e-05, &
        -0.5454e-05, -0.8297e-05, -0.1209e-04, -0.1697e-04, -0.2322e-04, &
        -0.3163e-04, -0.4318e-04, -0.5910e-04, -0.8111e-04, -0.1108e-03, &
        -0.1493e-03, -0.1982e-03, -0.2588e-03, -0.3333e-03, -0.4237e-03, &
        -0.5312e-03, -0.6562e-03, -0.7968e-03, -0.9496e-03, -0.1110e-02, &
        -0.1276e-02, -0.1439e-02, -0.1588e-02, -0.1708e-02, -0.1796e-02/
      data ((c3(ip,iw),iw=1,30), ip= 9, 9)/ &
         0.0000e+00,  0.1455e-10, -0.3638e-09, -0.2299e-08, -0.4744e-08, &
        -0.9284e-08, -0.1445e-07, -0.2141e-07, -0.3162e-07, -0.4761e-07, &
        -0.7248e-07, -0.1065e-06, -0.1501e-06, -0.2140e-06, -0.2981e-06, &
        -0.3994e-06, -0.5201e-06, -0.6549e-06, -0.8009e-06, -0.9627e-06, &
        -0.1125e-05, -0.1266e-05, -0.1348e-05, -0.1292e-05, -0.1005e-05, &
        -0.4166e-06,  0.4279e-06,  0.1401e-05,  0.2379e-05,  0.3278e-05/
      data ((c1(ip,iw),iw=1,30), ip=10,10)/ &
         0.99985462,  0.99975640,  0.99961889,  0.99943668,  0.99919188, &
         0.99884301,  0.99832898,  0.99757999,  0.99651998,  0.99506402, &
         0.99309200,  0.99044400,  0.98689002,  0.98215997,  0.97579002, &
         0.96730000,  0.95603001,  0.94110000,  0.92149001,  0.89609998, &
         0.86399996,  0.82449996,  0.77759999,  0.72459996,  0.66769999, &
         0.61000001,  0.55340004,  0.49769998,  0.44250000,  0.38810003/
      data ((c2(ip,iw),iw=1,30), ip=10,10)/ &
        -0.1607e-06, -0.4160e-06, -0.9320e-06, -0.1872e-05, -0.3281e-05, &
        -0.5286e-05, -0.8097e-05, -0.1187e-04, -0.1677e-04, -0.2320e-04, &
        -0.3190e-04, -0.4402e-04, -0.6081e-04, -0.8441e-04, -0.1162e-03, &
        -0.1576e-03, -0.2102e-03, -0.2760e-03, -0.3571e-03, -0.4558e-03, &
        -0.5730e-03, -0.7082e-03, -0.8591e-03, -0.1022e-02, -0.1194e-02, &
        -0.1368e-02, -0.1533e-02, -0.1671e-02, -0.1775e-02, -0.1843e-02/
      data ((c3(ip,iw),iw=1,30), ip=10,10)/ &
        -0.1164e-09, -0.7276e-10, -0.5530e-09, -0.2270e-08, -0.5093e-08, &
        -0.9517e-08, -0.1502e-07, -0.2219e-07, -0.3171e-07, -0.4712e-07, &
        -0.7123e-07, -0.1042e-06, -0.1493e-06, -0.2156e-06, -0.2999e-06, &
        -0.4027e-06, -0.5243e-06, -0.6616e-06, -0.8125e-06, -0.9691e-06, &
        -0.1126e-05, -0.1251e-05, -0.1294e-05, -0.1163e-05, -0.7639e-06, &
        -0.7395e-07,  0.8279e-06,  0.1819e-05,  0.2795e-05,  0.3647e-05/
      data ((c1(ip,iw),iw=1,30), ip=11,11)/ &
         0.99985212,  0.99975210,  0.99960798,  0.99941242,  0.99914628, &
         0.99876302,  0.99819702,  0.99736100,  0.99616700,  0.99450397, &
         0.99225003,  0.98920000,  0.98510998,  0.97961998,  0.97220999, &
         0.96231002,  0.94909000,  0.93155003,  0.90856999,  0.87910002, &
         0.84219998,  0.79790002,  0.74669999,  0.69080001,  0.63300002, &
         0.57570004,  0.51950002,  0.46359998,  0.40829998,  0.35450000/
      data ((c2(ip,iw),iw=1,30), ip=11,11)/ &
        -0.1531e-06, -0.3864e-06, -0.8804e-06, -0.1776e-05, -0.3131e-05, &
        -0.5082e-05, -0.7849e-05, -0.1164e-04, -0.1669e-04, -0.2340e-04, &
        -0.3261e-04, -0.4546e-04, -0.6380e-04, -0.8932e-04, -0.1237e-03, &
        -0.1687e-03, -0.2262e-03, -0.2984e-03, -0.3880e-03, -0.4964e-03, &
        -0.6244e-03, -0.7705e-03, -0.9325e-03, -0.1107e-02, -0.1288e-02, &
        -0.1466e-02, -0.1623e-02, -0.1746e-02, -0.1831e-02, -0.1875e-02/
      data ((c3(ip,iw),iw=1,30), ip=11,11)/ &
         0.1019e-09, -0.2037e-09, -0.8004e-09, -0.2387e-08, -0.5326e-08, &
        -0.9764e-08, -0.1576e-07, -0.2256e-07, -0.3180e-07, -0.4616e-07, &
        -0.7026e-07, -0.1031e-06, -0.1520e-06, -0.2181e-06, -0.3037e-06, &
        -0.4109e-06, -0.5354e-06, -0.6740e-06, -0.8241e-06, -0.9810e-06, &
        -0.1126e-05, -0.1221e-05, -0.1200e-05, -0.9678e-06, -0.4500e-06, &
         0.3236e-06,  0.1256e-05,  0.2259e-05,  0.3206e-05,  0.3978e-05/
      data ((c1(ip,iw),iw=1,30), ip=12,12)/ &
         0.99985027,  0.99974507,  0.99959022,  0.99937689,  0.99907988, &
         0.99865198,  0.99801201,  0.99706602,  0.99569201,  0.99377203, &
         0.99115402,  0.98762000,  0.98286003,  0.97640002,  0.96771997, &
         0.95604998,  0.94045001,  0.91979003,  0.89289999,  0.85879999, &
         0.81700003,  0.76800001,  0.71340001,  0.65579998,  0.59810001, &
         0.54139996,  0.48519999,  0.42909998,  0.37410003,  0.32190001/
      data ((c2(ip,iw),iw=1,30), ip=12,12)/ &
        -0.1340e-06, -0.3478e-06, -0.8189e-06, -0.1653e-05, -0.2944e-05, &
        -0.4852e-05, -0.7603e-05, -0.1150e-04, -0.1682e-04, -0.2400e-04, &
        -0.3390e-04, -0.4799e-04, -0.6807e-04, -0.9596e-04, -0.1338e-03, &
        -0.1833e-03, -0.2471e-03, -0.3275e-03, -0.4268e-03, -0.5466e-03, &
        -0.6862e-03, -0.8439e-03, -0.1017e-02, -0.1201e-02, -0.1389e-02, &
        -0.1563e-02, -0.1706e-02, -0.1809e-02, -0.1872e-02, -0.1890e-02/
      data ((c3(ip,iw),iw=1,30), ip=12,12)/ &
        -0.1455e-10, -0.1892e-09, -0.8295e-09, -0.2547e-08, -0.5544e-08, &
        -0.1014e-07, -0.1605e-07, -0.2341e-07, -0.3156e-07, -0.4547e-07, &
        -0.6749e-07, -0.1034e-06, -0.1550e-06, -0.2230e-06, -0.3130e-06, &
        -0.4219e-06, -0.5469e-06, -0.6922e-06, -0.8448e-06, -0.9937e-06, &
        -0.1118e-05, -0.1166e-05, -0.1054e-05, -0.6926e-06, -0.7180e-07, &
         0.7515e-06,  0.1709e-05,  0.2703e-05,  0.3593e-05,  0.4232e-05/
      data ((c1(ip,iw),iw=1,30), ip=13,13)/ &
         0.99984729,  0.99973530,  0.99956691,  0.99932659,  0.99898797, &
         0.99849701,  0.99776399,  0.99667102,  0.99507397,  0.99283201, &
         0.98977000,  0.98563999,  0.98001999,  0.97241002,  0.96213001, &
         0.94830000,  0.92980999,  0.90546000,  0.87409997,  0.83510000, &
         0.78850001,  0.73549998,  0.67850000,  0.62049997,  0.56340003, &
         0.50699997,  0.45050001,  0.39450002,  0.34060001,  0.29079998/
      data ((c2(ip,iw),iw=1,30), ip=13,13)/ &
        -0.1163e-06, -0.3048e-06, -0.7186e-06, -0.1495e-05, -0.2726e-05, &
        -0.4588e-05, -0.7396e-05, -0.1152e-04, -0.1725e-04, -0.2514e-04, &
        -0.3599e-04, -0.5172e-04, -0.7403e-04, -0.1051e-03, -0.1469e-03, &
        -0.2023e-03, -0.2735e-03, -0.3637e-03, -0.4746e-03, -0.6067e-03, &
        -0.7586e-03, -0.9281e-03, -0.1112e-02, -0.1304e-02, -0.1491e-02, &
        -0.1653e-02, -0.1777e-02, -0.1860e-02, -0.1896e-02, -0.1891e-02/
      data ((c3(ip,iw),iw=1,30), ip=13,13)/ &
        -0.1455e-09, -0.2765e-09, -0.9750e-09, -0.2794e-08, -0.5413e-08, &
        -0.1048e-07, -0.1625e-07, -0.2344e-07, -0.3105e-07, -0.4304e-07, &
        -0.6608e-07, -0.1057e-06, -0.1587e-06, -0.2308e-06, -0.3235e-06, &
        -0.4373e-06, -0.5687e-06, -0.7156e-06, -0.8684e-06, -0.1007e-05, &
        -0.1094e-05, -0.1062e-05, -0.8273e-06, -0.3485e-06,  0.3463e-06, &
         0.1206e-05,  0.2173e-05,  0.3132e-05,  0.3919e-05,  0.4370e-05/
      data ((c1(ip,iw),iw=1,30), ip=14,14)/ &
         0.99984348,  0.99972272,  0.99953479,  0.99926043,  0.99886698, &
         0.99829400,  0.99744201,  0.99615997,  0.99429500,  0.99166000, &
         0.98806000,  0.98316997,  0.97649997,  0.96748000,  0.95525998, &
         0.93878001,  0.91687000,  0.88830000,  0.85220003,  0.80820000, &
         0.75699997,  0.70099998,  0.64300001,  0.58550000,  0.52890003, &
         0.47219998,  0.41560000,  0.36040002,  0.30849999,  0.26169997/
      data ((c2(ip,iw),iw=1,30), ip=14,14)/ &
        -0.8581e-07, -0.2557e-06, -0.6103e-06, -0.1305e-05, -0.2472e-05, &
        -0.4334e-05, -0.7233e-05, -0.1167e-04, -0.1806e-04, -0.2679e-04, &
        -0.3933e-04, -0.5705e-04, -0.8194e-04, -0.1165e-03, -0.1637e-03, &
        -0.2259e-03, -0.3068e-03, -0.4082e-03, -0.5318e-03, -0.6769e-03, &
        -0.8415e-03, -0.1023e-02, -0.1216e-02, -0.1410e-02, -0.1588e-02, &
        -0.1733e-02, -0.1837e-02, -0.1894e-02, -0.1904e-02, -0.1881e-02/
      data ((c3(ip,iw),iw=1,30), ip=14,14)/ &
        -0.2037e-09, -0.4220e-09, -0.1091e-08, -0.2896e-08, -0.5821e-08, &
        -0.1052e-07, -0.1687e-07, -0.2353e-07, -0.3193e-07, -0.4254e-07, &
        -0.6685e-07, -0.1072e-06, -0.1638e-06, -0.2427e-06, -0.3421e-06, &
        -0.4600e-06, -0.5946e-06, -0.7472e-06, -0.8958e-06, -0.1009e-05, &
        -0.1032e-05, -0.8919e-06, -0.5224e-06,  0.5218e-07,  0.7886e-06, &
         0.1672e-05,  0.2626e-05,  0.3513e-05,  0.4138e-05,  0.4379e-05/
      data ((c1(ip,iw),iw=1,30), ip=15,15)/ &
         0.99983788,  0.99970680,  0.99949580,  0.99917668,  0.99871200, &
         0.99803603,  0.99703097,  0.99552703,  0.99333203,  0.99023402, &
         0.98597997,  0.98013997,  0.97223002,  0.96145999,  0.94686002, &
         0.92727000,  0.90142000,  0.86820000,  0.82700002,  0.77820003, &
         0.72350001,  0.66569996,  0.60769999,  0.55089998,  0.49430001, &
         0.43739998,  0.38110000,  0.32749999,  0.27840000,  0.23479998/
      data ((c2(ip,iw),iw=1,30), ip=15,15)/ &
        -0.8246e-07, -0.2070e-06, -0.4895e-06, -0.1106e-05, -0.2216e-05, &
        -0.4077e-05, -0.7150e-05, -0.1202e-04, -0.1920e-04, -0.2938e-04, &
        -0.4380e-04, -0.6390e-04, -0.9209e-04, -0.1310e-03, -0.1843e-03, &
        -0.2554e-03, -0.3468e-03, -0.4611e-03, -0.5982e-03, -0.7568e-03, &
        -0.9340e-03, -0.1126e-02, -0.1324e-02, -0.1514e-02, -0.1676e-02, &
        -0.1801e-02, -0.1881e-02, -0.1911e-02, -0.1900e-02, -0.1867e-02/
      data ((c3(ip,iw),iw=1,30), ip=15,15)/ &
        -0.1601e-09, -0.3492e-09, -0.1019e-08, -0.2634e-08, -0.5632e-08, &
        -0.1065e-07, -0.1746e-07, -0.2542e-07, -0.3206e-07, -0.4390e-07, &
        -0.6956e-07, -0.1093e-06, -0.1729e-06, -0.2573e-06, -0.3612e-06, &
        -0.4904e-06, -0.6342e-06, -0.7834e-06, -0.9175e-06, -0.9869e-06, &
        -0.9164e-06, -0.6386e-06, -0.1544e-06,  0.4798e-06,  0.1252e-05, &
         0.2137e-05,  0.3043e-05,  0.3796e-05,  0.4211e-05,  0.4332e-05/
      data ((c1(ip,iw),iw=1,30), ip=16,16)/ &
         0.99983227,  0.99968958,  0.99945217,  0.99907941,  0.99852598, &
         0.99772000,  0.99652398,  0.99475902,  0.99218899,  0.98856002, &
         0.98348999,  0.97653997,  0.96708000,  0.95420998,  0.93677002, &
         0.91352999,  0.88330001,  0.84509999,  0.79900002,  0.74599999, &
         0.68879998,  0.63049996,  0.57319999,  0.51660001,  0.45969999, &
         0.40289998,  0.34780002,  0.29650003,  0.25070000,  0.20959997/
      data ((c2(ip,iw),iw=1,30), ip=16,16)/ &
        -0.7004e-07, -0.1592e-06, -0.3936e-06, -0.9145e-06, -0.1958e-05, &
        -0.3850e-05, -0.7093e-05, -0.1252e-04, -0.2066e-04, -0.3271e-04, &
        -0.4951e-04, -0.7268e-04, -0.1045e-03, -0.1487e-03, -0.2092e-03, &
        -0.2899e-03, -0.3936e-03, -0.5215e-03, -0.6729e-03, -0.8454e-03, &
        -0.1035e-02, -0.1235e-02, -0.1432e-02, -0.1608e-02, -0.1751e-02, &
        -0.1854e-02, -0.1907e-02, -0.1913e-02, -0.1888e-02, -0.1857e-02/
      data ((c3(ip,iw),iw=1,30), ip=16,16)/ &
        -0.2328e-09, -0.3347e-09, -0.9750e-09, -0.2314e-08, -0.5166e-08, &
        -0.1052e-07, -0.1726e-07, -0.2605e-07, -0.3532e-07, -0.4949e-07, &
        -0.7229e-07, -0.1133e-06, -0.1799e-06, -0.2725e-06, -0.3881e-06, &
        -0.5249e-06, -0.6763e-06, -0.8227e-06, -0.9279e-06, -0.9205e-06, &
        -0.7228e-06, -0.3109e-06,  0.2583e-06,  0.9390e-06,  0.1726e-05, &
         0.2579e-05,  0.3376e-05,  0.3931e-05,  0.4161e-05,  0.4369e-05/
      data ((c1(ip,iw),iw=1,30), ip=17,17)/ &
         0.99982637,  0.99967217,  0.99940813,  0.99897701,  0.99831802, &
         0.99734300,  0.99592501,  0.99385202,  0.99086499,  0.98659998, &
         0.98057997,  0.97229999,  0.96098000,  0.94555002,  0.92479002, &
         0.89740002,  0.86240000,  0.81919998,  0.76859999,  0.71249998, &
         0.65419996,  0.59630001,  0.53950000,  0.48259997,  0.42549998, &
         0.36940002,  0.31629997,  0.26810002,  0.22520000,  0.18580002/
      data ((c2(ip,iw),iw=1,30), ip=17,17)/ &
        -0.6526e-07, -0.1282e-06, -0.3076e-06, -0.7454e-06, -0.1685e-05, &
        -0.3600e-05, -0.7071e-05, -0.1292e-04, -0.2250e-04, -0.3665e-04, &
        -0.5623e-04, -0.8295e-04, -0.1195e-03, -0.1696e-03, -0.2385e-03, &
        -0.3298e-03, -0.4465e-03, -0.5887e-03, -0.7546e-03, -0.9408e-03, &
        -0.1141e-02, -0.1345e-02, -0.1533e-02, -0.1691e-02, -0.1813e-02, &
        -0.1889e-02, -0.1916e-02, -0.1904e-02, -0.1877e-02, -0.1850e-02/
      data ((c3(ip,iw),iw=1,30), ip=17,17)/ &
        -0.1746e-09, -0.2037e-09, -0.8149e-09, -0.2095e-08, -0.4889e-08, &
        -0.9517e-08, -0.1759e-07, -0.2740e-07, -0.4147e-07, -0.5774e-07, &
        -0.7909e-07, -0.1199e-06, -0.1877e-06, -0.2859e-06, -0.4137e-06, &
        -0.5649e-06, -0.7218e-06, -0.8516e-06, -0.9022e-06, -0.7905e-06, &
        -0.4531e-06,  0.6917e-07,  0.7009e-06,  0.1416e-05,  0.2194e-05, &
         0.2963e-05,  0.3578e-05,  0.3900e-05,  0.4094e-05,  0.4642e-05/
      data ((c1(ip,iw),iw=1,30), ip=18,18)/ &
         0.99982101,  0.99965781,  0.99936712,  0.99887502,  0.99809802, &
         0.99692702,  0.99523401,  0.99281400,  0.98935997,  0.98435003, &
         0.97728002,  0.96740997,  0.95381999,  0.93539000,  0.91082001, &
         0.87889999,  0.83889997,  0.79100001,  0.73660004,  0.67879999, &
         0.62049997,  0.56330001,  0.50629997,  0.44900000,  0.39209998, &
         0.33749998,  0.28729999,  0.24229997,  0.20150000,  0.16280001/
      data ((c2(ip,iw),iw=1,30), ip=18,18)/ &
        -0.6477e-07, -0.1243e-06, -0.2536e-06, -0.6173e-06, -0.1495e-05, &
        -0.3353e-05, -0.6919e-05, -0.1337e-04, -0.2418e-04, -0.4049e-04, &
        -0.6354e-04, -0.9455e-04, -0.1367e-03, -0.1942e-03, -0.2717e-03, &
        -0.3744e-03, -0.5042e-03, -0.6609e-03, -0.8416e-03, -0.1041e-02, &
        -0.1249e-02, -0.1448e-02, -0.1622e-02, -0.1760e-02, -0.1857e-02, &
        -0.1906e-02, -0.1911e-02, -0.1892e-02, -0.1870e-02, -0.1844e-02/
      data ((c3(ip,iw),iw=1,30), ip=18,18)/ &
        -0.5821e-10, -0.2328e-09, -0.6985e-09, -0.1368e-08, -0.4351e-08, &
        -0.8993e-08, -0.1579e-07, -0.2916e-07, -0.4904e-07, -0.7010e-07, &
        -0.9623e-07, -0.1332e-06, -0.1928e-06, -0.2977e-06, -0.4371e-06, &
        -0.5992e-06, -0.7586e-06, -0.8580e-06, -0.8238e-06, -0.5811e-06, &
        -0.1298e-06,  0.4702e-06,  0.1162e-05,  0.1905e-05,  0.2632e-05, &
         0.3247e-05,  0.3609e-05,  0.3772e-05,  0.4166e-05,  0.5232e-05/
      data ((c1(ip,iw),iw=1,30), ip=19,19)/ &
         0.99981648,  0.99964571,  0.99933147,  0.99878597,  0.99787998, &
         0.99649400,  0.99448699,  0.99166602,  0.98762000,  0.98181999, &
         0.97352999,  0.96183002,  0.94558001,  0.92363000,  0.89480001, &
         0.85799998,  0.81309998,  0.76100004,  0.70420003,  0.64590001, &
         0.58840001,  0.53139997,  0.47380000,  0.41619998,  0.36030000, &
         0.30809999,  0.26109999,  0.21880001,  0.17909998,  0.14080000/
      data ((c2(ip,iw),iw=1,30), ip=19,19)/ &
        -0.7906e-07, -0.1291e-06, -0.2430e-06, -0.5145e-06, -0.1327e-05, &
        -0.3103e-05, -0.6710e-05, -0.1371e-04, -0.2561e-04, -0.4405e-04, &
        -0.7051e-04, -0.1070e-03, -0.1560e-03, -0.2217e-03, -0.3090e-03, &
        -0.4228e-03, -0.5657e-03, -0.7371e-03, -0.9322e-03, -0.1142e-02, &
        -0.1352e-02, -0.1541e-02, -0.1697e-02, -0.1813e-02, -0.1883e-02, &
        -0.1906e-02, -0.1898e-02, -0.1882e-02, -0.1866e-02, -0.1832e-02/
      data ((c3(ip,iw),iw=1,30), ip=19,19)/ &
         0.2910e-10,  0.1455e-10, -0.2765e-09, -0.1426e-08, -0.2576e-08, &
        -0.5923e-08, -0.1429e-07, -0.3159e-07, -0.5441e-07, -0.8367e-07, &
        -0.1161e-06, -0.1526e-06, -0.2060e-06, -0.3007e-06, -0.4450e-06, &
        -0.6182e-06, -0.7683e-06, -0.8170e-06, -0.6754e-06, -0.3122e-06, &
         0.2234e-06,  0.8828e-06,  0.1632e-05,  0.2373e-05,  0.3002e-05, &
         0.3384e-05,  0.3499e-05,  0.3697e-05,  0.4517e-05,  0.6117e-05/
      data ((c1(ip,iw),iw=1,30), ip=20,20)/ &
         0.99981302,  0.99963689,  0.99930489,  0.99870700,  0.99768901, &
         0.99608499,  0.99373102,  0.99039900,  0.98566997,  0.97895002, &
         0.96930999,  0.95548999,  0.93621999,  0.91029000,  0.87669998, &
         0.83490002,  0.78549999,  0.73019999,  0.67240000,  0.61469996, &
         0.55779999,  0.50029999,  0.44220001,  0.38489997,  0.33069998, &
         0.28149998,  0.23760003,  0.19690001,  0.15759999,  0.11989999/
      data ((c2(ip,iw),iw=1,30), ip=20,20)/ &
        -0.7762e-07, -0.1319e-06, -0.2315e-06, -0.4780e-06, -0.1187e-05, &
        -0.2750e-05, -0.6545e-05, -0.1393e-04, -0.2645e-04, -0.4652e-04, &
        -0.7657e-04, -0.1190e-03, -0.1766e-03, -0.2520e-03, -0.3499e-03, &
        -0.4751e-03, -0.6307e-03, -0.8160e-03, -0.1024e-02, -0.1240e-02, &
        -0.1443e-02, -0.1619e-02, -0.1757e-02, -0.1849e-02, -0.1892e-02, &
        -0.1896e-02, -0.1886e-02, -0.1878e-02, -0.1861e-02, -0.1807e-02/
      data ((c3(ip,iw),iw=1,30), ip=25,25)/ &
        -0.6257e-09, -0.1382e-08,  0.2095e-08,  0.1863e-08, -0.1834e-08, &
        -0.2125e-08, -0.6985e-08, -0.1634e-07, -0.4128e-07, -0.9924e-07, &
        -0.1938e-06, -0.3275e-06, -0.4556e-06, -0.5046e-06, -0.3633e-06, &
         0.2484e-07,  0.6195e-06,  0.1249e-05,  0.1731e-05,  0.2053e-05, &
         0.2358e-05,  0.2569e-05,  0.2342e-05,  0.1883e-05,  0.2103e-05, &
         0.3570e-05,  0.5973e-05,  0.8752e-05,  0.1140e-04,  0.1328e-04/
      data ((c1(ip,iw),iw=1,30), ip=26,26)/ &
         0.99980712,  0.99958581,  0.99919039,  0.99854302,  0.99724799, &
         0.99486500,  0.99071401,  0.98379999,  0.97279000,  0.95585001, &
         0.93112999,  0.89749998,  0.85460001,  0.80320001,  0.74660003, &
         0.68869996,  0.63100004,  0.57249999,  0.51320004,  0.45450002, &
         0.39810002,  0.34649998,  0.30119997,  0.25950003,  0.21740001, &
         0.17379999,  0.13029999,  0.08950001,  0.05400002,  0.02640003/
      data ((c2(ip,iw),iw=1,30), ip=26,26)/ &
        -0.1257e-06, -0.2495e-06, -0.1334e-06, -0.8414e-07, -0.1698e-06, &
        -0.1346e-05, -0.3692e-05, -0.8625e-05, -0.1750e-04, -0.3483e-04, &
        -0.6843e-04, -0.1305e-03, -0.2362e-03, -0.3971e-03, -0.6127e-03, &
        -0.8621e-03, -0.1101e-02, -0.1297e-02, -0.1452e-02, -0.1570e-02, &
        -0.1647e-02, -0.1688e-02, -0.1727e-02, -0.1797e-02, -0.1887e-02, &
        -0.1947e-02, -0.1935e-02, -0.1833e-02, -0.1647e-02, -0.1401e-02/
      data ((c3(ip,iw),iw=1,30), ip=26,26)/ &
        -0.1222e-08, -0.1164e-09,  0.2285e-08,  0.2037e-09,  0.5675e-09, &
        -0.5239e-08, -0.9211e-08, -0.1483e-07, -0.3981e-07, -0.9641e-07, &
        -0.1717e-06, -0.2796e-06, -0.3800e-06, -0.3762e-06, -0.1936e-06, &
         0.1920e-06,  0.8335e-06,  0.1691e-05,  0.2415e-05,  0.2767e-05, &
         0.2823e-05,  0.2551e-05,  0.1839e-05,  0.1314e-05,  0.1960e-05, &
         0.4003e-05,  0.6909e-05,  0.1004e-04,  0.1273e-04,  0.1423e-04/
      data ((o1(ip,iw),iw=1,21), ip= 1, 1)/ &
         0.99999344,  0.99998689,  0.99997336,  0.99994606,  0.99989170, &
         0.99978632,  0.99957907,  0.99918377,  0.99844402,  0.99712098, &
         0.99489498,  0.99144602,  0.98655999,  0.98008001,  0.97165000, &
         0.96043998,  0.94527000,  0.92462999,  0.89709997,  0.86180001, &
         0.81800002/
      data ((o2(ip,iw),iw=1,21), ip= 1, 1)/ &
         0.6531e-10,  0.5926e-10, -0.1646e-09, -0.1454e-08, -0.7376e-08, &
        -0.2968e-07, -0.1071e-06, -0.3584e-06, -0.1125e-05, -0.3289e-05, &
        -0.8760e-05, -0.2070e-04, -0.4259e-04, -0.7691e-04, -0.1264e-03, &
        -0.1957e-03, -0.2895e-03, -0.4107e-03, -0.5588e-03, -0.7300e-03, &
        -0.9199e-03/
      data ((o3(ip,iw),iw=1,21), ip= 1, 1)/ &
        -0.2438e-10, -0.4826e-10, -0.9474e-10, -0.1828e-09, -0.3406e-09, &
        -0.6223e-09, -0.1008e-08, -0.1412e-08, -0.1244e-08,  0.8485e-09, &
         0.6343e-08,  0.1201e-07,  0.2838e-08, -0.4024e-07, -0.1257e-06, &
        -0.2566e-06, -0.4298e-06, -0.6184e-06, -0.7657e-06, -0.8153e-06, &
        -0.7552e-06/
      data ((o1(ip,iw),iw=1,21), ip= 2, 2)/ &
         0.99999344,  0.99998689,  0.99997348,  0.99994606,  0.99989170, &
         0.99978632,  0.99957907,  0.99918377,  0.99844402,  0.99712098, &
         0.99489498,  0.99144298,  0.98654997,  0.98006999,  0.97162998, &
         0.96042001,  0.94520003,  0.92449999,  0.89690000,  0.86140001, &
         0.81739998/
      data ((o2(ip,iw),iw=1,21), ip= 2, 2)/ &
         0.6193e-10,  0.5262e-10, -0.1774e-09, -0.1478e-08, -0.7416e-08, &
        -0.2985e-07, -0.1071e-06, -0.3584e-06, -0.1124e-05, -0.3287e-05, &
        -0.8753e-05, -0.2069e-04, -0.4256e-04, -0.7686e-04, -0.1264e-03, &
        -0.1956e-03, -0.2893e-03, -0.4103e-03, -0.5580e-03, -0.7285e-03, &
        -0.9171e-03/
      data ((o3(ip,iw),iw=1,21), ip= 2, 2)/ &
        -0.2436e-10, -0.4822e-10, -0.9466e-10, -0.1827e-09, -0.3404e-09, &
        -0.6220e-09, -0.1008e-08, -0.1414e-08, -0.1247e-08,  0.8360e-09, &
         0.6312e-08,  0.1194e-07,  0.2753e-08, -0.4040e-07, -0.1260e-06, &
        -0.2571e-06, -0.4307e-06, -0.6202e-06, -0.7687e-06, -0.8204e-06, &
        -0.7636e-06/
      data ((o1(ip,iw),iw=1,21), ip= 3, 3)/ &
         0.99999344,  0.99998689,  0.99997348,  0.99994606,  0.99989170, &
         0.99978632,  0.99957907,  0.99918377,  0.99844402,  0.99712098, &
         0.99489301,  0.99143898,  0.98654997,  0.98005998,  0.97158998, &
         0.96035999,  0.94509000,  0.92431998,  0.89660001,  0.86080003, &
         0.81639999/
      data ((o2(ip,iw),iw=1,21), ip= 3, 3)/ &
         0.5658e-10,  0.4212e-10, -0.1977e-09, -0.1516e-08, -0.7481e-08, &
        -0.2995e-07, -0.1072e-06, -0.3583e-06, -0.1123e-05, -0.3283e-05, &
        -0.8744e-05, -0.2067e-04, -0.4252e-04, -0.7679e-04, -0.1262e-03, &
        -0.1953e-03, -0.2889e-03, -0.4096e-03, -0.5567e-03, -0.7263e-03, &
        -0.9130e-03/
      data ((o3(ip,iw),iw=1,21), ip= 3, 3)/ &
        -0.2433e-10, -0.4815e-10, -0.9453e-10, -0.1825e-09, -0.3400e-09, &
        -0.6215e-09, -0.1007e-08, -0.1415e-08, -0.1253e-08,  0.8143e-09, &
         0.6269e-08,  0.1186e-07,  0.2604e-08, -0.4067e-07, -0.1264e-06, &
        -0.2579e-06, -0.4321e-06, -0.6229e-06, -0.7732e-06, -0.8277e-06, &
        -0.7752e-06/
      data ((o1(ip,iw),iw=1,21), ip= 4, 4)/ &
         0.99999344,  0.99998689,  0.99997348,  0.99994606,  0.99989200, &
         0.99978632,  0.99957907,  0.99918377,  0.99844402,  0.99711901, &
         0.99489301,  0.99143499,  0.98653001,  0.98003000,  0.97153997, &
         0.96026999,  0.94493997,  0.92404002,  0.89609998,  0.85990000, &
         0.81480002/
      data ((o2(ip,iw),iw=1,21), ip= 4, 4)/ &
         0.4814e-10,  0.2552e-10, -0.2298e-09, -0.1576e-08, -0.7579e-08, &
        -0.3009e-07, -0.1074e-06, -0.3581e-06, -0.1122e-05, -0.3278e-05, &
        -0.8729e-05, -0.2063e-04, -0.4245e-04, -0.7667e-04, -0.1260e-03, &
        -0.1950e-03, -0.2883e-03, -0.4086e-03, -0.5549e-03, -0.7229e-03, &
        -0.9071e-03/
      data ((o3(ip,iw),iw=1,21), ip= 4, 4)/ &
        -0.2428e-10, -0.4805e-10, -0.9433e-10, -0.1821e-09, -0.3394e-09, &
        -0.6206e-09, -0.1008e-08, -0.1416e-08, -0.1261e-08,  0.7860e-09, &
         0.6188e-08,  0.1171e-07,  0.2389e-08, -0.4109e-07, -0.1271e-06, &
        -0.2591e-06, -0.4344e-06, -0.6267e-06, -0.7797e-06, -0.8378e-06, &
        -0.7901e-06/
      data ((o1(ip,iw),iw=1,21), ip= 5, 5)/ &
         0.99999344,  0.99998689,  0.99997348,  0.99994606,  0.99989200, &
         0.99978638,  0.99957907,  0.99918377,  0.99844402,  0.99711901, &
         0.99488801,  0.99142599,  0.98650998,  0.97999001,  0.97148001, &
         0.96011001,  0.94467002,  0.92356998,  0.89530003,  0.85860002, &
         0.81250000/
      data ((o2(ip,iw),iw=1,21), ip= 5, 5)/ &
         0.3482e-10, -0.6492e-12, -0.2805e-09, -0.1671e-08, -0.7740e-08, &
        -0.3032e-07, -0.1076e-06, -0.3582e-06, -0.1120e-05, -0.3270e-05, &
        -0.8704e-05, -0.2058e-04, -0.4235e-04, -0.7649e-04, -0.1257e-03, &
        -0.1945e-03, -0.2874e-03, -0.4070e-03, -0.5521e-03, -0.7181e-03, &
        -0.8990e-03/
      data ((o3(ip,iw),iw=1,21), ip= 5, 5)/ &
        -0.2419e-10, -0.4788e-10, -0.9401e-10, -0.1815e-09, -0.3385e-09, &
        -0.6192e-09, -0.1006e-08, -0.1417e-08, -0.1273e-08,  0.7404e-09, &
         0.6068e-08,  0.1148e-07,  0.2021e-08, -0.4165e-07, -0.1281e-06, &
        -0.2609e-06, -0.4375e-06, -0.6323e-06, -0.7887e-06, -0.8508e-06, &
        -0.8067e-06/
      data ((o1(ip,iw),iw=1,21), ip= 6, 6)/ &
         0.99999344,  0.99998689,  0.99997348,  0.99994606,  0.99989200, &
         0.99978638,  0.99957931,  0.99918377,  0.99844301,  0.99711698, &
         0.99488401,  0.99141300,  0.98648000,  0.97992003,  0.97135001, &
         0.95989001,  0.94428003,  0.92286998,  0.89410001,  0.85640001, &
         0.80890000/
      data ((o2(ip,iw),iw=1,21), ip= 6, 6)/ &
         0.1388e-10, -0.4180e-10, -0.3601e-09, -0.1820e-08, -0.7993e-08, &
        -0.3068e-07, -0.1081e-06, -0.3580e-06, -0.1117e-05, -0.3257e-05, &
        -0.8667e-05, -0.2049e-04, -0.4218e-04, -0.7620e-04, -0.1253e-03, &
        -0.1937e-03, -0.2860e-03, -0.4047e-03, -0.5481e-03, -0.7115e-03, &
        -0.8885e-03/
      data ((o3(ip,iw),iw=1,21), ip= 6, 6)/ &
        -0.2406e-10, -0.4762e-10, -0.9351e-10, -0.1806e-09, -0.3370e-09, &
        -0.6170e-09, -0.1004e-08, -0.1417e-08, -0.1297e-08,  0.6738e-09, &
         0.5895e-08,  0.1113e-07,  0.1466e-08, -0.4265e-07, -0.1298e-06, &
        -0.2636e-06, -0.4423e-06, -0.6402e-06, -0.8005e-06, -0.8658e-06, &
        -0.8222e-06/
      data ((o1(ip,iw),iw=1,21), ip= 7, 7)/ &
         0.99999344,  0.99998689,  0.99997348,  0.99994630,  0.99989200, &
         0.99978638,  0.99957931,  0.99918360,  0.99844301,  0.99711502, &
         0.99487501,  0.99138802,  0.98642999,  0.97982001,  0.97114998, &
         0.95954001,  0.94363999,  0.92176998,  0.89219999,  0.85329998, &
         0.80379999/
      data ((o2(ip,iw),iw=1,21), ip= 7, 7)/ &
        -0.1889e-10, -0.1062e-09, -0.4847e-09, -0.2053e-08, -0.8389e-08, &
        -0.3140e-07, -0.1089e-06, -0.3577e-06, -0.1112e-05, -0.3236e-05, &
        -0.8607e-05, -0.2035e-04, -0.4192e-04, -0.7576e-04, -0.1245e-03, &
        -0.1925e-03, -0.2840e-03, -0.4013e-03, -0.5427e-03, -0.7029e-03, &
        -0.8756e-03/
      data ((o3(ip,iw),iw=1,21), ip= 7, 7)/ &
        -0.2385e-10, -0.4722e-10, -0.9273e-10, -0.1791e-09, -0.3348e-09, &
        -0.6121e-09, -0.9974e-09, -0.1422e-08, -0.1326e-08,  0.5603e-09, &
         0.5604e-08,  0.1061e-07,  0.6106e-09, -0.4398e-07, -0.1321e-06, &
        -0.2676e-06, -0.4490e-06, -0.6507e-06, -0.8145e-06, -0.8801e-06, &
        -0.8311e-06/
      data ((o1(ip,iw),iw=1,21), ip= 8, 8)/ &
         0.99999344,  0.99998689,  0.99997348,  0.99994630,  0.99989229, &
         0.99978650,  0.99957931,  0.99918288,  0.99844098,  0.99711001, &
         0.99486202,  0.99135500,  0.98635000,  0.97965997,  0.97083998, &
         0.95898998,  0.94266999,  0.92009997,  0.88929999,  0.84860003, &
         0.79640001/
      data ((o2(ip,iw),iw=1,21), ip= 8, 8)/ &
        -0.6983e-10, -0.2063e-09, -0.6785e-09, -0.2416e-08, -0.9000e-08, &
        -0.3243e-07, -0.1100e-06, -0.3574e-06, -0.1104e-05, -0.3205e-05, &
        -0.8516e-05, -0.2014e-04, -0.4151e-04, -0.7508e-04, -0.1234e-03, &
        -0.1907e-03, -0.2811e-03, -0.3966e-03, -0.5355e-03, -0.6924e-03, &
        -0.8613e-03/
      data ((o3(ip,iw),iw=1,21), ip= 8, 8)/ &
        -0.2353e-10, -0.4659e-10, -0.9153e-10, -0.1769e-09, -0.3313e-09, &
        -0.6054e-09, -0.9899e-09, -0.1430e-08, -0.1375e-08,  0.3874e-09, &
         0.5171e-08,  0.9807e-08, -0.7345e-09, -0.4604e-07, -0.1356e-06, &
        -0.2731e-06, -0.4577e-06, -0.6632e-06, -0.8284e-06, -0.8894e-06, &
        -0.8267e-06/
      data ((o1(ip,iw),iw=1,21), ip= 9, 9)/ &
         0.99999344,  0.99998689,  0.99997360,  0.99994630,  0.99989229, &
         0.99978650,  0.99957961,  0.99918252,  0.99843901,  0.99710202, &
         0.99484003,  0.99130303,  0.98623002,  0.97940999,  0.97038001, &
         0.95815003,  0.94119000,  0.91755998,  0.88510001,  0.84189999, &
         0.78610003/
      data ((o2(ip,iw),iw=1,21), ip= 9, 9)/ &
        -0.1481e-09, -0.3601e-09, -0.9762e-09, -0.2973e-08, -0.1014e-07, &
        -0.3421e-07, -0.1121e-06, -0.3569e-06, -0.1092e-05, -0.3156e-05, &
        -0.8375e-05, -0.1981e-04, -0.4090e-04, -0.7405e-04, -0.1218e-03, &
        -0.1881e-03, -0.2770e-03, -0.3906e-03, -0.5269e-03, -0.6810e-03, &
        -0.8471e-03/
      data ((o3(ip,iw),iw=1,21), ip= 9, 9)/ &
        -0.2304e-10, -0.4564e-10, -0.8969e-10, -0.1735e-09, -0.3224e-09, &
        -0.5933e-09, -0.9756e-09, -0.1428e-08, -0.1446e-08,  0.1156e-09, &
         0.4499e-08,  0.8469e-08, -0.2720e-08, -0.4904e-07, -0.1401e-06, &
        -0.2801e-06, -0.4681e-06, -0.6761e-06, -0.8387e-06, -0.8879e-06, &
        -0.8040e-06/
      data ((o1(ip,iw),iw=1,21), ip=10,10)/ &
         0.99999344,  0.99998689,  0.99997360,  0.99994630,  0.99989259, &
         0.99978650,  0.99957931,  0.99918163,  0.99843597,  0.99709100, &
         0.99480897,  0.99122101,  0.98604000,  0.97902000,  0.96965003, &
         0.95684999,  0.93896997,  0.91386002,  0.87910002,  0.83249998, &
         0.77200001/
      data ((o2(ip,iw),iw=1,21), ip=10,10)/ &
        -0.2661e-09, -0.5923e-09, -0.1426e-08, -0.3816e-08, -0.1159e-07, &
        -0.3654e-07, -0.1143e-06, -0.3559e-06, -0.1074e-05, -0.3083e-05, &
        -0.8159e-05, -0.1932e-04, -0.3998e-04, -0.7253e-04, -0.1194e-03, &
        -0.1845e-03, -0.2718e-03, -0.3833e-03, -0.5176e-03, -0.6701e-03, &
        -0.8354e-03/
      data ((o3(ip,iw),iw=1,21), ip=10,10)/ &
        -0.2232e-10, -0.4421e-10, -0.8695e-10, -0.1684e-09, -0.3141e-09, &
        -0.5765e-09, -0.9606e-09, -0.1434e-08, -0.1551e-08, -0.2663e-09, &
         0.3515e-08,  0.6549e-08, -0.5479e-08, -0.5312e-07, -0.1460e-06, &
        -0.2883e-06, -0.4787e-06, -0.6863e-06, -0.8399e-06, -0.8703e-06, &
        -0.7602e-06/
      data ((o1(ip,iw),iw=1,21), ip=11,11)/ &
         0.99999356,  0.99998701,  0.99997360,  0.99994630,  0.99989289, &
         0.99978679,  0.99957907,  0.99917960,  0.99843001,  0.99707502, &
         0.99475998,  0.99109501,  0.98575002,  0.97843999,  0.96855003, &
         0.95494002,  0.93572998,  0.90853000,  0.87070000,  0.81970000, &
         0.75380003/
      data ((o2(ip,iw),iw=1,21), ip=11,11)/ &
        -0.4394e-09, -0.9330e-09, -0.2086e-08, -0.5054e-08, -0.1373e-07, &
        -0.3971e-07, -0.1178e-06, -0.3546e-06, -0.1049e-05, -0.2976e-05, &
        -0.7847e-05, -0.1860e-04, -0.3864e-04, -0.7038e-04, -0.1162e-03, &
        -0.1798e-03, -0.2654e-03, -0.3754e-03, -0.5091e-03, -0.6621e-03, &
        -0.8286e-03/
      data ((o3(ip,iw),iw=1,21), ip=11,11)/ &
        -0.2127e-10, -0.4216e-10, -0.8300e-10, -0.1611e-09, -0.3019e-09, &
        -0.5597e-09, -0.9431e-09, -0.1450e-08, -0.1694e-08, -0.7913e-09, &
         0.2144e-08,  0.3990e-08, -0.9282e-08, -0.5810e-07, -0.1525e-06, &
        -0.2965e-06, -0.4869e-06, -0.6894e-06, -0.8281e-06, -0.8350e-06, &
        -0.6956e-06/
      data ((o1(ip,iw),iw=1,21), ip=12,12)/ &
         0.99999368,  0.99998701,  0.99997377,  0.99994630,  0.99989259, &
         0.99978709,  0.99957848,  0.99917740,  0.99842203,  0.99704897, &
         0.99468797,  0.99090999,  0.98532999,  0.97758001,  0.96693999, &
         0.95213997,  0.93109000,  0.90110999,  0.85930002,  0.80290002, &
         0.73019999/
      data ((o2(ip,iw),iw=1,21), ip=12,12)/ &
        -0.6829e-09, -0.1412e-08, -0.3014e-08, -0.6799e-08, -0.1675e-07, &
        -0.4450e-07, -0.1235e-06, -0.3538e-06, -0.1014e-05, -0.2827e-05, &
        -0.7407e-05, -0.1759e-04, -0.3676e-04, -0.6744e-04, -0.1120e-03, &
        -0.1742e-03, -0.2585e-03, -0.3683e-03, -0.5034e-03, -0.6594e-03, &
        -0.8290e-03/
      data ((o3(ip,iw),iw=1,21), ip=12,12)/ &
        -0.1985e-10, -0.3937e-10, -0.7761e-10, -0.1511e-09, -0.2855e-09, &
        -0.5313e-09, -0.9251e-09, -0.1470e-08, -0.1898e-08, -0.1519e-08, &
         0.2914e-09,  0.5675e-09, -0.1405e-07, -0.6359e-07, -0.1584e-06, &
        -0.3020e-06, -0.4893e-06, -0.6821e-06, -0.8021e-06, -0.7834e-06, &
        -0.6105e-06/
      data ((o1(ip,iw),iw=1,21), ip=13,13)/ &
         0.99999368,  0.99998701,  0.99997389,  0.99994695,  0.99989289, &
         0.99978721,  0.99957782,  0.99917412,  0.99840999,  0.99701297, &
         0.99458599,  0.99064600,  0.98471999,  0.97632003,  0.96464998, &
         0.94819999,  0.92467999,  0.89109999,  0.84430003,  0.78139997, &
         0.70070004/
      data ((o2(ip,iw),iw=1,21), ip=13,13)/ &
        -0.1004e-08, -0.2043e-08, -0.4239e-08, -0.9104e-08, -0.2075e-07, &
        -0.5096e-07, -0.1307e-06, -0.3520e-06, -0.9671e-06, -0.2630e-05, &
        -0.6825e-05, -0.1624e-04, -0.3429e-04, -0.6369e-04, -0.1069e-03, &
        -0.1680e-03, -0.2520e-03, -0.3635e-03, -0.5029e-03, -0.6647e-03, &
        -0.8390e-03/
      data ((o3(ip,iw),iw=1,21), ip=13,13)/ &
        -0.1807e-10, -0.3587e-10, -0.7085e-10, -0.1385e-09, -0.2648e-09, &
        -0.4958e-09, -0.8900e-09, -0.1473e-08, -0.2112e-08, -0.2399e-08, &
        -0.2002e-08, -0.3646e-08, -0.1931e-07, -0.6852e-07, -0.1618e-06, &
        -0.3021e-06, -0.4828e-06, -0.6634e-06, -0.7643e-06, -0.7177e-06, &
        -0.5054e-06/
      data ((o1(ip,iw),iw=1,21), ip=14,14)/ &
         0.99999368,  0.99998713,  0.99997389,  0.99994725,  0.99989289, &
         0.99978679,  0.99957597,  0.99916971,  0.99839503,  0.99696702, &
         0.99444997,  0.99028301,  0.98387003,  0.97457999,  0.96148002, &
         0.94284999,  0.91613001,  0.87809998,  0.82520002,  0.75489998, &
         0.66520000/
      data ((o2(ip,iw),iw=1,21), ip=14,14)/ &
        -0.1387e-08, -0.2798e-08, -0.5706e-08, -0.1187e-07, -0.2564e-07, &
        -0.5866e-07, -0.1398e-06, -0.3516e-06, -0.9148e-06, -0.2398e-05, &
        -0.6122e-05, -0.1459e-04, -0.3125e-04, -0.5923e-04, -0.1013e-03, &
        -0.1620e-03, -0.2473e-03, -0.3631e-03, -0.5098e-03, -0.6800e-03, &
        -0.8603e-03/
      data ((o3(ip,iw),iw=1,21), ip=14,14)/ &
        -0.1610e-10, -0.3200e-10, -0.6337e-10, -0.1245e-09, -0.2408e-09, &
        -0.4533e-09, -0.8405e-09, -0.1464e-08, -0.2337e-08, -0.3341e-08, &
        -0.4467e-08, -0.8154e-08, -0.2436e-07, -0.7128e-07, -0.1604e-06, &
        -0.2945e-06, -0.4666e-06, -0.6357e-06, -0.7187e-06, -0.6419e-06, &
        -0.3795e-06/
      data ((o1(ip,iw),iw=1,21), ip=15,15)/ &
         0.99999410,  0.99998724,  0.99997455,  0.99994725,  0.99989331, &
         0.99978632,  0.99957472,  0.99916393,  0.99837703,  0.99690801, &
         0.99427801,  0.98982000,  0.98277998,  0.97232002,  0.95731997, &
         0.93585998,  0.90521002,  0.86180001,  0.80190003,  0.72290003, &
         0.62380004/
      data ((o2(ip,iw),iw=1,21), ip=15,15)/ &
        -0.1788e-08, -0.3588e-08, -0.7244e-08, -0.1479e-07, -0.3083e-07, &
        -0.6671e-07, -0.1497e-06, -0.3519e-06, -0.8607e-06, -0.2154e-05, &
        -0.5364e-05, -0.1276e-04, -0.2785e-04, -0.5435e-04, -0.9573e-04, &
        -0.1570e-03, -0.2455e-03, -0.3682e-03, -0.5253e-03, -0.7065e-03, &
        -0.8938e-03/
      data ((o3(ip,iw),iw=1,21), ip=15,15)/ &
        -0.1429e-10, -0.2843e-10, -0.5645e-10, -0.1115e-09, -0.2181e-09, &
        -0.4200e-09, -0.7916e-09, -0.1460e-08, -0.2542e-08, -0.4168e-08, &
        -0.6703e-08, -0.1215e-07, -0.2821e-07, -0.7073e-07, -0.1530e-06, &
        -0.2791e-06, -0.4426e-06, -0.6027e-06, -0.6707e-06, -0.5591e-06, &
        -0.2328e-06/
      data ((o1(ip,iw),iw=1,21), ip=16,16)/ &
         0.99999434,  0.99998778,  0.99997467,  0.99994761,  0.99989331, &
         0.99978602,  0.99957269,  0.99915779,  0.99835497,  0.99684399, &
         0.99408400,  0.98929000,  0.98148000,  0.96954000,  0.95212001, &
         0.92719001,  0.89170003,  0.84200001,  0.77420002,  0.68620002, &
         0.57780004/
      data ((o2(ip,iw),iw=1,21), ip=16,16)/ &
        -0.2141e-08, -0.4286e-08, -0.8603e-08, -0.1737e-07, -0.3548e-07, &
        -0.7410e-07, -0.1590e-06, -0.3537e-06, -0.8142e-06, -0.1935e-05, &
        -0.4658e-05, -0.1099e-04, -0.2444e-04, -0.4948e-04, -0.9067e-04, &
        -0.1538e-03, -0.2474e-03, -0.3793e-03, -0.5495e-03, -0.7439e-03, &
        -0.9383e-03/
      data ((o3(ip,iw),iw=1,21), ip=16,16)/ &
        -0.1295e-10, -0.2581e-10, -0.5136e-10, -0.1019e-09, -0.2011e-09, &
        -0.3916e-09, -0.7585e-09, -0.1439e-08, -0.2648e-08, -0.4747e-08, &
        -0.8301e-08, -0.1499e-07, -0.3024e-07, -0.6702e-07, -0.1399e-06, &
        -0.2564e-06, -0.4117e-06, -0.5669e-06, -0.6239e-06, -0.4748e-06, &
        -0.7013e-07/
      data ((o1(ip,iw),iw=1,21), ip=17,17)/ &
         0.99999434,  0.99998778,  0.99997479,  0.99994791,  0.99989331, &
         0.99978608,  0.99957120,  0.99915212,  0.99833500,  0.99677801, &
         0.99388403,  0.98873001,  0.98005998,  0.96639001,  0.94606000, &
         0.91689998,  0.87580001,  0.81889999,  0.74280000,  0.64559996, &
         0.52869999/
      data ((o2(ip,iw),iw=1,21), ip=17,17)/ &
        -0.2400e-08, -0.4796e-08, -0.9599e-08, -0.1927e-07, -0.3892e-07, &
        -0.7954e-07, -0.1661e-06, -0.3540e-06, -0.7780e-06, -0.1763e-05, &
        -0.4092e-05, -0.9512e-05, -0.2142e-04, -0.4502e-04, -0.8640e-04, &
        -0.1525e-03, -0.2526e-03, -0.3955e-03, -0.5805e-03, -0.7897e-03, &
        -0.9899e-03/
      data ((o3(ip,iw),iw=1,21), ip=17,17)/ &
        -0.1220e-10, -0.2432e-10, -0.4845e-10, -0.9640e-10, -0.1912e-09, &
        -0.3771e-09, -0.7392e-09, -0.1420e-08, -0.2702e-08, -0.5049e-08, &
        -0.9214e-08, -0.1659e-07, -0.3101e-07, -0.6162e-07, -0.1235e-06, &
        -0.2287e-06, -0.3755e-06, -0.5274e-06, -0.5790e-06, -0.3947e-06, &
         0.1003e-06/
      data ((o1(ip,iw),iw=1,21), ip=18,18)/ &
         0.99999464,  0.99998808,  0.99997497,  0.99994791,  0.99989331, &
         0.99978518,  0.99957031,  0.99914658,  0.99831802,  0.99671799, &
         0.99370098,  0.98821002,  0.97867000,  0.96313000,  0.93948001, &
         0.90534002,  0.85769999,  0.79310000,  0.70840001,  0.60290003, &
         0.47930002/
      data ((o2(ip,iw),iw=1,21), ip=18,18)/ &
        -0.2557e-08, -0.5106e-08, -0.1020e-07, -0.2043e-07, -0.4103e-07, &
        -0.8293e-07, -0.1697e-06, -0.3531e-06, -0.7531e-06, -0.1645e-05, &
        -0.3690e-05, -0.8411e-05, -0.1902e-04, -0.4118e-04, -0.8276e-04, &
        -0.1525e-03, -0.2601e-03, -0.4147e-03, -0.6149e-03, -0.8384e-03, &
        -0.1042e-02/
      data ((o3(ip,iw),iw=1,21), ip=18,18)/ &
        -0.1189e-10, -0.2372e-10, -0.4729e-10, -0.9421e-10, -0.1873e-09, &
        -0.3713e-09, -0.7317e-09, -0.1437e-08, -0.2764e-08, -0.5243e-08, &
        -0.9691e-08, -0.1751e-07, -0.3122e-07, -0.5693e-07, -0.1076e-06, &
        -0.1981e-06, -0.3324e-06, -0.4785e-06, -0.5280e-06, -0.3174e-06, &
         0.2672e-06/
      data ((o1(ip,iw),iw=1,21), ip=19,19)/ &
         0.99999464,  0.99998820,  0.99997509,  0.99994779,  0.99989331, &
         0.99978518,  0.99956989,  0.99914283,  0.99830401,  0.99667197, &
         0.99355298,  0.98776001,  0.97741997,  0.96007001,  0.93285000, &
         0.89310002,  0.83819997,  0.76520002,  0.67250001,  0.56000000, &
         0.43199998/
      data ((o2(ip,iw),iw=1,21), ip=19,19)/ &
        -0.2630e-08, -0.5249e-08, -0.1048e-07, -0.2096e-07, -0.4198e-07, &
        -0.8440e-07, -0.1710e-06, -0.3513e-06, -0.7326e-06, -0.1562e-05, &
        -0.3416e-05, -0.7637e-05, -0.1719e-04, -0.3795e-04, -0.7926e-04, &
        -0.1524e-03, -0.2680e-03, -0.4344e-03, -0.6486e-03, -0.8838e-03, &
        -0.1089e-02/
      data ((o3(ip,iw),iw=1,21), ip=19,19)/ &
        -0.1188e-10, -0.2369e-10, -0.4725e-10, -0.9417e-10, -0.1875e-09, &
        -0.3725e-09, -0.7365e-09, -0.1445e-08, -0.2814e-08, -0.5384e-08, &
        -0.1008e-07, -0.1816e-07, -0.3179e-07, -0.5453e-07, -0.9500e-07, &
        -0.1679e-06, -0.2819e-06, -0.4109e-06, -0.4555e-06, -0.2283e-06, &
         0.4283e-06/
      data ((o1(ip,iw),iw=1,21), ip=20,20)/ &
         0.99999487,  0.99998832,  0.99997520,  0.99994791,  0.99989331, &
         0.99978459,  0.99956900,  0.99913990,  0.99829400,  0.99663699, &
         0.99344099,  0.98741001,  0.97643000,  0.95743001,  0.92672002, &
         0.88099998,  0.81809998,  0.73660004,  0.63620001,  0.51880002, &
         0.38880002/
      data ((o2(ip,iw),iw=1,21), ip=20,20)/ &
        -0.2651e-08, -0.5291e-08, -0.1056e-07, -0.2110e-07, -0.4221e-07, &
        -0.8462e-07, -0.1705e-06, -0.3466e-06, -0.7155e-06, -0.1501e-05, &
        -0.3223e-05, -0.7079e-05, -0.1581e-04, -0.3517e-04, -0.7553e-04, &
        -0.1510e-03, -0.2746e-03, -0.4528e-03, -0.6789e-03, -0.9214e-03, &
        -0.1124e-02/
      data ((o3(ip,iw),iw=1,21), ip=20,20)/ &
        -0.1193e-10, -0.2380e-10, -0.4748e-10, -0.9465e-10, -0.1886e-09, &
        -0.3751e-09, -0.7436e-09, -0.1466e-08, -0.2872e-08, -0.5508e-08, &
        -0.1038e-07, -0.1891e-07, -0.3279e-07, -0.5420e-07, -0.8711e-07, &
        -0.1403e-06, -0.2248e-06, -0.3221e-06, -0.3459e-06, -0.1066e-06, &
         0.5938e-06/
      data ((o1(ip,iw),iw=1,21), ip=21,21)/ &
         0.99999487,  0.99998873,  0.99997509,  0.99994779,  0.99989349, &
         0.99978501,  0.99956918,  0.99913877,  0.99828798,  0.99661303, &
         0.99335998,  0.98715001,  0.97566003,  0.95530999,  0.92153001, &
         0.87000000,  0.79869998,  0.70819998,  0.60109997,  0.48110002, &
         0.35140002/
      data ((o2(ip,iw),iw=1,21), ip=21,21)/ &
        -0.2654e-08, -0.5296e-08, -0.1057e-07, -0.2111e-07, -0.4219e-07, &
        -0.8445e-07, -0.1696e-06, -0.3428e-06, -0.7013e-06, -0.1458e-05, &
        -0.3084e-05, -0.6678e-05, -0.1476e-04, -0.3284e-04, -0.7173e-04, &
        -0.1481e-03, -0.2786e-03, -0.4688e-03, -0.7052e-03, -0.9506e-03, &
        -0.1148e-02/
      data ((o3(ip,iw),iw=1,21), ip=21,21)/ &
        -0.1195e-10, -0.2384e-10, -0.4755e-10, -0.9482e-10, -0.1890e-09, &
        -0.3761e-09, -0.7469e-09, -0.1476e-08, -0.2892e-08, -0.5603e-08, &
        -0.1060e-07, -0.1942e-07, -0.3393e-07, -0.5508e-07, -0.8290e-07, &
        -0.1182e-06, -0.1657e-06, -0.2170e-06, -0.1997e-06,  0.6227e-07, &
         0.7847e-06/
      data ((o1(ip,iw),iw=1,21), ip=22,22)/ &
         0.99999541,  0.99998873,  0.99997497,  0.99994737,  0.99989349, &
         0.99978501,  0.99956882,  0.99913770,  0.99828303,  0.99659699, &
         0.99330199,  0.98697001,  0.97510999,  0.95372999,  0.91742998, &
         0.86080003,  0.78139997,  0.68220001,  0.56920004,  0.44809997, &
         0.32080001/
      data ((o2(ip,iw),iw=1,21), ip=22,22)/ &
        -0.2653e-08, -0.5295e-08, -0.1057e-07, -0.2110e-07, -0.4215e-07, &
        -0.8430e-07, -0.1690e-06, -0.3403e-06, -0.6919e-06, -0.1427e-05, &
        -0.2991e-05, -0.6399e-05, -0.1398e-04, -0.3099e-04, -0.6824e-04, &
        -0.1441e-03, -0.2795e-03, -0.4814e-03, -0.7282e-03, -0.9739e-03, &
        -0.1163e-02/
      data ((o3(ip,iw),iw=1,21), ip=22,22)/ &
        -0.1195e-10, -0.2384e-10, -0.4756e-10, -0.9485e-10, -0.1891e-09, &
        -0.3765e-09, -0.7483e-09, -0.1481e-08, -0.2908e-08, -0.5660e-08, &
        -0.1075e-07, -0.1980e-07, -0.3472e-07, -0.5626e-07, -0.8149e-07, &
        -0.1027e-06, -0.1136e-06, -0.1071e-06, -0.2991e-07,  0.2743e-06, &
         0.1017e-05/
      data ((o1(ip,iw),iw=1,21), ip=23,23)/ &
         0.99999595,  0.99998885,  0.99997479,  0.99994725,  0.99989331, &
         0.99978518,  0.99956882,  0.99913692,  0.99827999,  0.99658602, &
         0.99326497,  0.98685002,  0.97474003,  0.95260000,  0.91441000, &
         0.85360003,  0.76719999,  0.65990001,  0.54190004,  0.42119998, &
         0.29699999/
      data ((o2(ip,iw),iw=1,21), ip=23,23)/ &
        -0.2653e-08, -0.5294e-08, -0.1057e-07, -0.2109e-07, -0.4212e-07, &
        -0.8420e-07, -0.1686e-06, -0.3388e-06, -0.6858e-06, -0.1406e-05, &
        -0.2928e-05, -0.6206e-05, -0.1344e-04, -0.2961e-04, -0.6533e-04, &
        -0.1399e-03, -0.2780e-03, -0.4904e-03, -0.7488e-03, -0.9953e-03, &
        -0.1175e-02/
      data ((o3(ip,iw),iw=1,21), ip=23,23)/ &
        -0.1195e-10, -0.2384e-10, -0.4756e-10, -0.9485e-10, -0.1891e-09, &
        -0.3767e-09, -0.7492e-09, -0.1485e-08, -0.2924e-08, -0.5671e-08, &
        -0.1084e-07, -0.2009e-07, -0.3549e-07, -0.5773e-07, -0.8208e-07, &
        -0.9394e-07, -0.7270e-07, -0.3947e-08,  0.1456e-06,  0.5083e-06, &
         0.1270e-05/
      data ((o1(ip,iw),iw=1,21), ip=24,24)/ &
         0.99999630,  0.99998873,  0.99997401,  0.99994725,  0.99989349, &
         0.99978501,  0.99956959,  0.99913663,  0.99827701,  0.99658000, &
         0.99324101,  0.98676997,  0.97447002,  0.95185000,  0.91232002, &
         0.84850001,  0.75660002,  0.64230001,  0.52030003,  0.40090001, &
         0.27980000/
      data ((o2(ip,iw),iw=1,21), ip=24,24)/ &
        -0.2653e-08, -0.5294e-08, -0.1056e-07, -0.2109e-07, -0.4210e-07, &
        -0.8413e-07, -0.1684e-06, -0.3379e-06, -0.6820e-06, -0.1393e-05, &
        -0.2889e-05, -0.6080e-05, -0.1307e-04, -0.2861e-04, -0.6310e-04, &
        -0.1363e-03, -0.2758e-03, -0.4969e-03, -0.7681e-03, -0.1017e-02, &
        -0.1186e-02/
      data ((o3(ip,iw),iw=1,21), ip=24,24)/ &
        -0.1195e-10, -0.2384e-10, -0.4756e-10, -0.9485e-10, -0.1891e-09, &
        -0.3768e-09, -0.7497e-09, -0.1487e-08, -0.2933e-08, -0.5710e-08, &
        -0.1089e-07, -0.2037e-07, -0.3616e-07, -0.5907e-07, -0.8351e-07, &
        -0.8925e-07, -0.4122e-07,  0.8779e-07,  0.3143e-06,  0.7281e-06, &
         0.1500e-05/
      data ((o1(ip,iw),iw=1,21), ip=25,25)/ &
         0.99999648,  0.99998897,  0.99997377,  0.99994749,  0.99989331, &
         0.99978501,  0.99956989,  0.99913692,  0.99827600,  0.99657297, &
         0.99322498,  0.98672003,  0.97431999,  0.95137000,  0.91095001, &
         0.84500003,  0.74909997,  0.62979996,  0.50510001,  0.38679999, &
         0.26789999/
      data ((o2(ip,iw),iw=1,21), ip=25,25)/ &
        -0.2653e-08, -0.5293e-08, -0.1056e-07, -0.2108e-07, -0.4209e-07, &
        -0.8409e-07, -0.1682e-06, -0.3373e-06, -0.6797e-06, -0.1383e-05, &
        -0.2862e-05, -0.5993e-05, -0.1283e-04, -0.2795e-04, -0.6158e-04, &
        -0.1338e-03, -0.2743e-03, -0.5030e-03, -0.7863e-03, -0.1038e-02, &
        -0.1196e-02/
      data ((o3(ip,iw),iw=1,21), ip=25,25)/ &
        -0.1195e-10, -0.2383e-10, -0.4755e-10, -0.9484e-10, -0.1891e-09, &
        -0.3768e-09, -0.7499e-09, -0.1489e-08, -0.2939e-08, -0.5741e-08, &
        -0.1100e-07, -0.2066e-07, -0.3660e-07, -0.6002e-07, -0.8431e-07, &
        -0.8556e-07, -0.1674e-07,  0.1638e-06,  0.4525e-06,  0.8949e-06, &
         0.1669e-05/
      data ((o1(ip,iw),iw=1,21), ip=26,26)/ &
         0.99999672,  0.99998909,  0.99997377,  0.99994695,  0.99989349, &
         0.99978518,  0.99956989,  0.99913692,  0.99827498,  0.99657100, &
         0.99321902,  0.98668998,  0.97421002,  0.95106000,  0.91009998, &
         0.84280002,  0.74430001,  0.62180001,  0.49519998,  0.37800002, &
         0.25999999/
      data ((o2(ip,iw),iw=1,21), ip=26,26)/ &
        -0.2652e-08, -0.5292e-08, -0.1056e-07, -0.2108e-07, -0.4208e-07, &
        -0.8406e-07, -0.1681e-06, -0.3369e-06, -0.6784e-06, -0.1378e-05, &
        -0.2843e-05, -0.5944e-05, -0.1269e-04, -0.2759e-04, -0.6078e-04, &
        -0.1326e-03, -0.2742e-03, -0.5088e-03, -0.8013e-03, -0.1054e-02, &
        -0.1202e-02/
      data ((o3(ip,iw),iw=1,21), ip=26,26)/ &
        -0.1194e-10, -0.2383e-10, -0.4754e-10, -0.9482e-10, -0.1891e-09, &
        -0.3768e-09, -0.7499e-09, -0.1489e-08, -0.2941e-08, -0.5752e-08, &
        -0.1104e-07, -0.2069e-07, -0.3661e-07, -0.6012e-07, -0.8399e-07, &
        -0.8183e-07,  0.1930e-08,  0.2167e-06,  0.5434e-06,  0.9990e-06, &
         0.1787e-05/

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
    dp   (ic,k) = pl(ic,k+1)-pl(ic,k)
    dh2o (ic,k) = 1.02*wa(ic,k)*dp(ic,k)+1.e-10
    do3  (ic,k) = 476.*oa(ic,k)*dp(ic,k)+1.e-10
    dco2 (ic,k) = 789.*co2*dp(ic,k)+1.e-10
    dch4 (ic,k) = 789.*ch4*dp(ic,k)+1.e-10
    dn2o (ic,k) = 789.*n2o*dp(ic,k)+1.e-10
    df11 (ic,k) = 789.*cfc11*dp(ic,k)+1.e-10
    df12 (ic,k) = 789.*cfc12*dp(ic,k)+1.e-10
    df22 (ic,k) = 789.*cfc22*dp(ic,k)+1.e-10
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


    return
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
       return
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
      return
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
      return
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
      return
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

      return
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
      return
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
      return
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
      return
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
      return
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
      return
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
      return
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
      return
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
      return
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
      return
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
      return
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
      return
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
      return
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
      return
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
    call wrf_error_fatal('MSG opt_cloud in module_ra_goddard_2014.F  you must call read_sslut first.')
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

 return
 end subroutine opt_cloud

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

 subroutine check_reff(spc,q,re,i,j,k)
 implicit none
 character(len=*) :: spc  !species 
 real,intent(inout) :: q !mass concentration [g/m2]
 real,intent(inout) :: re !effective radius [g/m2]
 integer,intent(in) :: i,j,k !array index


!
! Check NaN
!
 if( isnan(re) ) then
  print*,'MSG check_reff: Warning '//trim(spc)//' re is NaN at (i,j,k)=',i,j,k
  re = 0.e0
 endif
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


 return
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

 end module module_ra_goddard_2014

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD
