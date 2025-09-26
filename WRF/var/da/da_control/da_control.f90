module da_control

   !--------------------------------------------------------------------------
   ! Purpose: Common reference point for WRFVAR control.
   !--------------------------------------------------------------------------

   use module_driver_constants, only : max_domains, max_eta, max_moves, max_bogus, &
                                       max_outer_iterations, max_instruments, max_plevs, &
                                       max_ocean, num_ob_indexes

   implicit none

#include "namelist_defines.inc"

   ! switches set from other namelist options
   logical :: use_obsgts
   logical :: use_rad

   !---------------------------------------------------------------------------
   ! [0.0] WRF hybrid coordinate variables
   !---------------------------------------------------------------------------
   real, allocatable :: c1f(:), c2f(:), c3f(:), c4f(:)
   real, allocatable :: c1h(:), c2h(:), c3h(:), c4h(:)

   !---------------------------------------------------------------------------
   ! [1.0] Physical parameter constants (all NIST standard values):
   !---------------------------------------------------------------------------

   ! Fundamental constants:
   real, parameter    :: pi = 3.1415926           ! Value used in WRF.
   real, parameter    :: radian = pi / 180.0
   real, parameter    :: gas_constant = 287.0     ! Value used in WRF.
   real, parameter    :: gas_constant_v = 461.6   ! Value used in WRF.
   real, parameter    :: cp = 7.0*gas_constant/2.0 ! Value used in WRF.
   real, parameter    :: t_kelvin = 273.15
   real, parameter    :: t_triple = 273.16 ! triple point of water
   ! The imported code for ssmi and radiance uses 273.0 in a way that suggests 
   ! it may not be a lazy definition of the melting point of water, so keep the
   ! value separate for the moment
   real, parameter    :: t_roughem = 273.0
   real, parameter    :: t_landem = 273.0

   real, parameter    :: kappa = gas_constant / cp
   real, parameter    :: rd_over_rv = gas_constant / gas_constant_v
   real, parameter    :: rd_over_rv1 = 1.0 - rd_over_rv
   real, parameter    :: L_over_Rv = 5418.12

   real, parameter    :: gamma = 1.4

   ! Earth constants:
   real, parameter    :: gravity = 9.81        ! m/s - value used in WRF.
   ! real, parameter    :: earth_radius = 6378.15
   real, parameter    :: earth_radius      = 6370.0          ! Be consistant with WRF
   real, parameter    :: satellite_height  = 35800.0         ! used by da_get_satzen
   ! real, parameter    :: earth_omega  = 2.0*pi/86400.0  ! Omega
   real, parameter    :: earth_omega  = 0.000072921     ! Omega 7.2921*10**-5

   ! Saturation Vapour Pressure Constants(Rogers & Yau, 1989) 
   real, parameter    :: es_alpha = 611.2
   real, parameter    :: es_beta = 17.67
   real, parameter    :: es_gamma = 243.5
   real, parameter    :: es_gammabeta = es_gamma * es_beta
   real, parameter    :: es_gammakelvin = es_gamma - t_kelvin

   ! Explicit moist constants:
   real, parameter    :: SVP1=0.6112, SVP2=17.67, SVP3=29.65
   real, parameter    :: SVPT0=t_kelvin, TO=t_kelvin
   real, parameter    :: N0R=8.0E6, N0S=2.0E7, RHOS=0.1
   real, parameter    :: AVT=841.99667, BVT=0.8, BVT2=2.5+0.5*BVT, BVT3=3.0+BVT
   real, parameter    :: PPI=1.0/(pi*N0R), PPIS=1.0/(pi*N0S*RHOS)
   real, parameter    :: XLV1=2370.0, XLF0=0.3337E6, XLV0=3.15E6
   real, parameter    :: XLS=XLV0-XLV1*t_triple+XLF0

   ! Planetary boundary physics constants
   real, parameter         :: k_kar = 0.4    ! Von Karman constant

   ! Zenith Total Delay: 
   !  Hydrostatic delay: 
   real, parameter    :: zdk1 = 2.2768e-5 
   real, parameter    :: zdk2 = 2.66e-3 
   real, parameter    :: zdk3 = 2.8e-7 
   !  Wet delay: 
   real, parameter    :: wdk1 = 2.21e-7 
   real, parameter    :: wdk2 = 3.73e-3 
 
   !  GPS Refractivity constant   
   real, parameter    :: a_ew = 0.622 
   real, parameter    :: b_ew = 0.378  

   ! GPS Refractivity constant  
   real, parameter    :: coeff = (wdk2*1.e8) / 77.6

   ! GPS Excess Phase parameter
   !hcl-note: is 5km a universal applicable setting?
   !hcl-note: should this be a namelist option?
   real, parameter    :: gps_ray_path_step = 5.0 !5km
   !hcl-note: 2000 is derived from 20km (50hPa) top with 0.01km interval
   !hcl-note: should the top and interval be namelist options?
   integer, parameter :: interpolate_level = 2000

#if RWORDSIZE==8
   real, parameter :: da_zero = 0D0
#else
   real, parameter :: da_zero = 0.0
#endif

   complex, parameter :: da_zero_complex = (da_zero,da_zero)
   
   !---------------------------------------------------------------------------
   ! [2.0] WRF-Var parameter constants:
   !---------------------------------------------------------------------------

   ! Missing values and the index number of the quality control

   integer, parameter ::  missing       = -888888
   real   , parameter ::  missing_r     = -888888.0
   real   , parameter ::  xmiss         = -88.0
!   real   , parameter ::  Max_StHeight_Diff = 100.0 !became a namelist variable

   integer, parameter :: cv_options_hum_specific_humidity = 1
   integer, parameter :: cv_options_hum_relative_humidity = 2

   ! No-one explains what these options means anywhere
   integer, parameter :: vert_corr_1 = 1
   integer, parameter :: vert_corr_2 = 2

   integer, parameter :: vertical_ip_0            = 0
   integer, parameter :: vertical_ip_sqrt_delta_p = 1
   integer, parameter :: vertical_ip_delta_p      = 2

   integer, parameter :: vert_evalue_global = 1
   integer, parameter :: vert_evalue_local  = 2

   integer, parameter :: alphacv_method_vp = 1
   integer, parameter :: alphacv_method_xa = 2

   integer, parameter :: sfc_assi_options_1 = 1
   integer, parameter :: sfc_assi_options_2 = 2

   integer, parameter :: check_rh_simple = 1
   integer, parameter :: check_rh_tpw    = 2

   logical :: anal_type_verify=.false.
   logical :: anal_type_randomcv=.false.
   logical :: anal_type_qcobs=.false.
   logical :: anal_type_hybrid_dual_res=.false.

   integer,parameter :: monitor_on  = 1
   integer,parameter :: monitor_off = 0

   integer,parameter :: qc_good       =  1
   integer,parameter :: qc_bad        = -1
   integer,parameter :: qc_varbc_bad  = -1

   integer, parameter :: bufr_satellite_id   = 1
   integer, parameter :: bufr_ifov           = 2
   integer, parameter :: bufr_year           = 3
   integer, parameter :: bufr_month          = 4
   integer, parameter :: bufr_day            = 5
   integer, parameter :: bufr_hour           = 6
   integer, parameter :: bufr_minute         = 7
   integer, parameter :: bufr_second         = 8
   integer, parameter :: bufr_lat            = 9
   integer, parameter :: bufr_lon            = 10
   integer, parameter :: bufr_satzen         = 11
   integer, parameter :: bufr_solzen         = 12
   integer, parameter :: bufr_station_height = 13
   integer, parameter :: bufr_landsea_mask   = 14
   integer, parameter :: bufr_solazi         = 15     !RTTOV9_3

   integer, parameter :: nchan_amsua = 15
   integer, parameter :: nchan_amsub = 5
   integer, parameter :: nchan_mhs   = 5
   integer, parameter :: nchan_msu   = 4
   integer, parameter :: nchan_hirs2 = 19
   integer, parameter :: nchan_hirs3 = 19
   integer, parameter :: nchan_hirs4 = 19
   integer, parameter :: nchan_ssmis = 24
   integer, parameter :: nchan_airs  = 281

   ! WRFVAR Minimisation:

   integer            :: iter
   integer            :: cv_size
   integer, parameter :: MP = 6
   integer, parameter :: LP = 6
   integer, parameter :: MAXFEV = 10
   real, parameter    :: FTOL = 1.0E-4
   real, parameter    :: GTOL = 0.9
   real, parameter    :: XTOL = 1.0E-17
   real, parameter    :: STPMIN = 1.0E-20
   real, parameter    :: STPMAX = 1.0E+20
   
   ! Background errors:
   real, parameter    :: pplow = 1.0e-8       ! Machine lowest number?
   real, parameter    :: pp_umin = 1.0e-2     ! Minimum u back. error (m/s).
   real, parameter    :: pp_vmin = 1.0e-2     ! Minimum v back. error (m/s).
   real, parameter    :: pp_tmin = 1.0e-2     ! Minimum t back. error (K).
   real, parameter    :: pp_qmin = 1.0e-6     ! Minimum q back. error (kg/kg)
   real, parameter    :: pp_pmin= 1.0e+1      ! Minimum pp back. error (Pa).

   ! FFTs:
   integer, parameter :: Forward_FFT     = -1 ! Grid to spectral
   integer, parameter :: Inverse_FFT     =  1 ! Spectral to grid.
   integer, parameter :: num_fft_factors = 10 ! Max number of factors.
   integer, parameter :: nrange          =1000! Range to search for efficient FFT.
 
   ! Balance:
   integer, parameter :: balance_geo = 1      ! Geostrophic balance.
   integer, parameter :: balance_cyc = 2      ! Cyclostrophic balance.
   integer, parameter :: balance_geocyc = 3   ! Geo/cyclostrophic balance.

   ! Adjoint tests:
   real, parameter    :: typical_u_rms = 2.0     ! m/s
   real, parameter    :: typical_v_rms = 2.0     ! m/s
   real, parameter    :: typical_speed_rms = 2.0 ! m/s
   real, parameter    :: typical_tb19v_rms = 1.0 ! K
   real, parameter    :: typical_tb19h_rms = 1.0 ! K
   real, parameter    :: typical_tb22v_rms = 1.0 ! K
   real, parameter    :: typical_tb37v_rms = 1.0 ! K
   real, parameter    :: typical_tb37h_rms = 1.0 ! K
   real, parameter    :: typical_tb85v_rms = 1.0 ! K
   real, parameter    :: typical_tb85h_rms = 1.0 ! K
   real, parameter    :: typical_t_rms = 1.0     ! K
   real, parameter    :: typical_p_rms = 100.0   ! Pa
   real, parameter    :: typical_q_rms = 0.00001 ! g/kg
   real, parameter    :: typical_rho_rms = 0.01  ! kg/m^3
   real, parameter    :: typical_tpw_rms = 0.2   ! cm
   real, parameter    :: typical_ref_rms = 5.0   ! N unit
   real, parameter    :: typical_eph_rms =1000.0 ! km
   real, parameter    :: typical_rh_rms = 20.0   ! %
   real, parameter    :: typical_thickness_rms = 50.0   ! m
   real, parameter    :: typical_qrn_rms = 0.00001 ! g/kg
   real, parameter    :: typical_qcw_rms = 0.00001 ! g/kg
   real, parameter    :: typical_qci_rms = 0.00001 ! g/kg
   real, parameter    :: typical_qsn_rms = 0.00001 ! g/kg
   real, parameter    :: typical_qgr_rms = 0.00001 ! g/kg
   real, parameter    :: typical_w_rms = 0.1     ! m/s
   real, parameter    :: typical_rv_rms = 1.0    ! m/s
   real, parameter    :: typical_rf_rms = 1.0    ! dBZ
   real, parameter    :: typical_rain_rms = 1.0   ! mm  

   ! The following typical mean squared values depend on control variable. They   
   ! are calculated in da_setup_background_errors and used in the VvToVp adjoint 
   ! test:

   real, parameter    :: inv_typ_vp1_sumsq = 0.00001 ! 1/sum(psi**2)
   real, parameter    :: inv_typ_vp2_sumsq = 0.00001 ! 1/sum(chi**2)
   real, parameter    :: inv_typ_vp3_sumsq = 0.00001 ! 1/sum(phi_u**2)
   real, parameter    :: inv_typ_vp4_sumsq = 10000.0 ! 1/sum(q**2)
   real, parameter    :: inv_typ_vp5_sumsq = 0.00001 ! 1/sum(?**2)
   real, parameter    :: inv_typ_vpalpha_sumsq = 1.0 ! 1/sum(?**2)

#include "version_decl"

   integer, parameter :: fg_format_wrf_arw_regional = 1
   integer, parameter :: fg_format_wrf_nmm_regional = 2
   integer, parameter :: fg_format_wrf_arw_global   = 3
   integer, parameter :: fg_format_kma_global = 4

   integer, parameter :: ob_format_bufr = 1
   integer, parameter :: ob_format_ascii = 2
   integer, parameter :: ob_format_madis = 3

   integer, parameter :: convert_fd2uv = 1
   integer, parameter :: convert_uv2fd = -1

   ! Fortran unit  parameters:

   ! stdout, stderr, trace_unit all controlled from namelist

   ! Units 9,10 are used for reading and writing namelist.input/output in WRF

   ! Do not use get_unit/free_unit because tracing is too low level
   integer, parameter :: trace_csv_unit = 8

   integer :: y_unit, yp_unit, cost_unit, grad_unit, stats_unit, jo_unit
   integer :: check_max_iv_unit, check_buddy_unit, rand_unit, omb_unit, &
              filtered_obs_unit
   integer :: biasprep_unit, qcstat_conv_unit
   integer :: varbc_tamdar_unit

   integer,parameter :: filename_len = 200

   integer, parameter :: num_alpha_corr_types = 3

   integer, parameter :: alpha_corr_type_exp      = 1
   integer, parameter :: alpha_corr_type_soar     = 2
   integer, parameter :: alpha_corr_type_gaussian = 3

   integer :: alpha_corr_unit1(num_alpha_corr_types)
   integer :: alpha_corr_unit2(num_alpha_corr_types)

   integer, parameter :: max_num_of_var = 200 ! Maximum # of stored fields.

   integer, parameter :: unit_start = 20
   integer, parameter :: unit_end = 500
   logical :: unit_used(unit_start:unit_end) = .false.

   ! grid properties

   character(len=3), parameter :: grid_ordering = "xyz"
   character(len=3), parameter :: grid_stagger  = "xyz"

   !---------------------------------------------------------------------------
   ! [3.0] Variables used in MM5 part of code:
   !---------------------------------------------------------------------------

   integer            :: map_projection       !1=LamConf/2=PolarSte/3=Mercator
                                              !0=CylEqui/6=Cassini
   real               :: ycntr
   integer            :: coarse_ix            ! coarse domain dim in i direction.
   integer            :: coarse_jy            ! coarse domain dim in y direction.
   real               :: coarse_ds            ! Coarse domain gridlength (km)
   real               :: start_x              ! i posn. of (1,1) in coarse domain.
   real               :: start_y              ! j posn. of (1,1) in coarse domain.
   real               :: start_lat            ! Latitude coresponds to start_(x,y)
   real               :: start_lon            ! Longitude coresponds to start_(x,y)
   real               :: delt_lat             ! Latitude increments for global grids
   real               :: delt_lon             ! Longitude increments for global grids

   real               :: phic                 ! coarse domain central lat(degree)
   real               :: xlonc                ! coarse domain central lon(degree)
   real               :: cone_factor          ! Cone Factor
   real               :: truelat1_3dv         ! True latitude 1 (degrees)
   real               :: truelat2_3dv         ! True latitude 2 (degrees)
   real               :: pole                 ! Pole latitude (degrees)
   real               :: dsm                  ! Current domain gridlength (km)
   real               :: psi1                 ! ?
   real               :: c2                   ! earth_radius * COS(psi1)

   real               :: ptop
   real, parameter    :: t0 = 300.0

   !------------------------------------------------------------------------------
   ! 4.0 vertical interpolation options
   !------------------------------------------------------------------------------

   integer, parameter :: v_interp_not_specified = missing, &
                         v_interp_p             = 1, &
                         v_interp_h             = 2

   !------------------------------------------------------------------------------
   ! WRFVAR scalar constants:
   !------------------------------------------------------------------------------

   integer                :: Anal_Space  ! Space of analysis
                                         ! ( 1 = Full model,
                                         !   2 = Transformed grid,
                                         !   3 = Ob space (PSAS) )

   integer                :: mix         ! 1st dimension of analysis grid.
   integer                :: mjy         ! 2nd dimension of analysis grid.
   integer                :: mkz         ! 3rd dimension of analysis grid.

   ! Recursive filter:

   real, allocatable      :: rf_turnconds(:) ! RF turning conditions.

   integer, parameter     :: max_ob_levels = 1001 ! Maximum levels for single ob
   integer, parameter     :: max_fgat_time = 100  ! Maximum levels for FGAT.

   integer                :: time

   logical       :: gaussian_lats  


   integer       :: cv_size_domain_jb    ! Total jb cv size.
   integer       :: cv_size_domain_je    ! Total je cv size.
   integer       :: cv_size_domain_jp    ! Total jp cv size.
   integer       :: cv_size_domain_js    ! Total js cv size.
   integer       :: cv_size_domain_jl    ! Total jl cv size.
   integer       :: cv_size_domain_jt    ! Total jt cv size.
   integer       :: cv_size_domain       ! Total cv size.    

   ! Hybrid:
   real          :: sigma_alpha          ! Alpha standard deviation.
   real          :: jb_factor            ! Weighting for Background Error Cov.

   ! Namelist variables in future?:
   real, parameter :: maximum_rh = 100.0
   real, parameter :: minimum_rh =  10.0

   real, parameter :: qlimit = 1.0e-12   ! imposed minimum mixing ratio

   ! other

   integer, parameter :: jperr = 6

   ! NCEP errors (U in m/s, V in m/s, T in K, H in %, P in Pa)
   ! rh has been divided by 2

   real, parameter :: err_k(0:jperr+1) = &
                      (/200000.0, 100100.0,70000.0,50000.0,30000.0,10000.0,5000.0, 1.0/)
   real, parameter :: err_u(0:jperr+1) = &
                      (/ 1.4, 1.4,   2.4,   2.8,   3.4,   2.5,  2.7,  2.7/)
   real, parameter :: err_v(0:jperr+1) = &
                      (/ 1.4, 1.4,   2.4,   2.8,   3.4,   2.5,  2.7 , 2.7 /)
   real, parameter :: err_t(0:jperr+1) = &
                      (/ 1.8, 1.8,   1.3,   1.3,   2.0,   3.1,  4.0 , 4.0 /)
   real, parameter :: err_rh(0:jperr+1) = &
                      (/ 10.0, 10.0,  10.0,  10.0,  10.0,  10.0, 10.0,  10.0/)
   real, parameter :: err_p(0:jperr+1) = &
                      (/ 100.0,100.0, 100.0, 100.0, 100.0, 100.0,100.0,100.0 /)

   ! Buddy check parameters (YRG, 10/3/2008):

   real, parameter :: max_buddy_t             =     8.0, &
                      max_buddy_uv            =     8.0, &
                      max_buddy_z             =     8.0, &
                      max_buddy_rh            =    40.0, &
                      max_buddy_p             =   350.0, &
                      buddy_weight            =     1.0, &
                      bin_p_width             =  5000.0, &
                      bin_z_width             =   500.0 

   ! Define various ways for bad data to be flagged.  

   integer, parameter ::  &
      missing_data            = -88, &     ! Data is missing with the value of 
                                           ! missing_r
      outside_of_domain       = -77, &     ! Data outside horizontal domain 
                                           ! or time window, data set to missing_r
      fail_varbc_aircraft     = -55, &     ! Data fail VarBC of aircraft 
                                           ! => no action
      wrong_direction         = -15, &     ! Wind vector direction <0 or> 360 
                                           ! => direction set to missing_r
      negative_spd            = -14, &     ! Wind vector norm is negative 
                                           ! => norm set to missing_r
      zero_spd                = -13, &     ! Wind vector norm is zero 
                                           ! => norm set to missing_r
      wrong_wind_data         = -12, &     ! Spike in wind profile 
                                           ! =>direction and norm set to missing_r 
      zero_t_td               = -11, &     ! t or td = 0 => t or td, rh and qv 
                                           ! are set to missing_r, 
      t_fail_supa_inver       = -10, &     ! superadiabatic temperature
                                           ! 
      wrong_t_sign            = - 9, &     ! Spike in Temperature profile 
                                           ! 
      above_model_lid         = - 8, &     ! heigh above model lid
                                           ! => no action
      far_below_model_surface = - 7, &     ! heigh far below model surface
                                           ! => no action
      below_model_surface     = - 6, &     ! height below model surface
                                           ! => no action
      standard_atmosphere     = - 5, &     ! Missing h, p or t
                                           ! =>Datum interpolated from standard atm
      from_background         = - 4, &     ! Missing h, p or t
                                           ! =>Datum interpolated from model
      fails_error_max         = - 3, &     ! Datum Fails error max check
                                           ! => no action
      fails_buddy_check       = - 2, &     ! Datum Fails buddy check
                                           ! => no action
      no_buddies              = - 1, &     ! Datum has no buddies
                                           ! => no action
      good_quality            =   0, &     ! OBS datum has good quality
                                           !
      convective_adjustment   =   1, &     ! convective adjustement check
                                           ! =>apply correction on t, td, rh and qv
      surface_correction      =   2, &     ! Surface datum
                                           ! => apply correction on datum
      Hydrostatic_recover     =   3, &     ! Height from hydrostaic assumption with
                                           ! the OBS data calibration
      Reference_OBS_recover   =   4, &     ! Height from reference state with OBS
                                           ! data calibration
      Other_check             =  88        ! passed other quality check

   ! Observations:

   integer                :: num_procs            ! Number of total processors.
   integer                :: myproc               ! My processor ID.
   integer, parameter     :: root = 0             ! Number of root processor
   logical                :: rootproc             ! Am I the root processor

   integer, parameter :: rtm_option_rttov = 1
   integer, parameter :: rtm_option_crtm = 2

   ! rtm_init setup parameter

   integer, parameter            :: maxsensor = 30

   integer, parameter :: npres_print = 12


   ! Tracing

   integer :: trace_start_points=0   ! Number of routines to initiate trace

   integer, parameter :: sound     = 1
   integer, parameter :: synop     = 2
   integer, parameter :: pilot     = 3
   integer, parameter :: satem     = 4
   integer, parameter :: geoamv    = 5
   integer, parameter :: polaramv  = 6
   integer, parameter :: airep     = 7
   integer, parameter :: gpspw     = 8
   integer, parameter :: gpsref    = 9
   integer, parameter :: metar     = 10
   integer, parameter :: ships     = 11
   integer, parameter :: ssmi_rv   = 12
   integer, parameter :: ssmi_tb   = 13
   integer, parameter :: ssmt1     = 14
   integer, parameter :: ssmt2     = 15
   integer, parameter :: qscat     = 16
   integer, parameter :: profiler  = 17
   integer, parameter :: buoy      = 18
   integer, parameter :: bogus     = 19
   integer, parameter :: pseudo    = 20
   integer, parameter :: radar     = 21
   integer, parameter :: radiance  = 22
   integer, parameter :: airsr     = 23
   integer, parameter :: sonde_sfc = 24
   integer, parameter :: mtgirs    = 25
   integer, parameter :: tamdar    = 26
   integer, parameter :: tamdar_sfc = 27
   integer, parameter :: rain      = 28
   integer, parameter :: gpseph    = 29

   character(len=14), parameter :: obs_names(num_ob_indexes) = (/ &
      "sound         ", &
      "synop         ", &
      "pilot         ", &
      "satem         ", &
      "geoamv        ", &
      "polaramv      ", &
      "airep         ", &
      "gpspw         ", &
      "gpsrf         ", &
      "metar         ", &
      "ships         ", &
      "ssmi_rv       ", &
      "ssmi_tb       ", &
      "ssmt1         ", &
      "ssmt2         ", &
      "qscat         ", &
      "profiler      ", &
      "buoy          ", &
      "bogus         ", &
      "pseudo        ", &
      "radar         ", &
      "radiance      ", &
      "airs retrieval", &
      "sonde_sfc     ", &
      "mtgirs        ", &
      "tamdar        ", &
      "tamdar_sfc    ", &
      "rain          ", &
      "gpseph        "  &
   /)

   logical :: pseudo_tpw
   logical :: pseudo_ztd
   logical :: pseudo_ref
   logical :: pseudo_uvtpq

   integer, parameter :: max_no_fm = 290

   integer, parameter :: num_ob_vars=10

   logical, parameter :: in_report(num_ob_vars,2) = reshape((/&
     .false.,.false.,.false.,.false.,.false.,.false.,.false.,.false.,.false., & ! sound
     .true.,.true.,.true.,.true.,.true.,.true.,.false.,.false.,.false.,.false.,.false./), &
     (/num_ob_vars,2/))

   integer, parameter :: report_h   = 1
   integer, parameter :: report_u   = 2
   integer, parameter :: report_v   = 3
   integer, parameter :: report_t   = 4
   integer, parameter :: report_q   = 5
   integer, parameter :: report_p   = 6
   integer, parameter :: report_rh  = 7
   integer, parameter :: report_slp = 8
   integer, parameter :: report_zk  = 9

   logical :: obs_use(num_ob_indexes) = .false.

   ! Special cases

   integer, parameter :: fm_satem = 86
   integer, parameter :: fm_amv   = 88

   integer, parameter :: fm_index(max_no_fm) = (/ &
      0,0,0,0,0,0,0,0,0,0,                                & ! 1-10
      0,Synop,Ships,0,Metar,            & ! 11-15
      Metar,Ships,buoy,buoy,0,    & ! 16-20
      0,0,0,0,0,0,0,0,0,0,                                & ! 21-30
      0,pilot,pilot,pilot,sound,  & ! 31-35
      sound,sound,sound,0,0,            & ! 36-40
      0,airep,0,0,0,0,0,0,0,0,                      & ! 41-50
      0,0,0,0,0,0,0,0,0,0,                                & ! 51-60
      0,0,0,0,0,0,0,0,0,0,                                & ! 61-70
      0,0,0,0,0,0,0,0,0,0,                                & ! 71-80
      0,0,0,0,0,satem,0,geoamv,0,0,           & ! 81-90
      0,0,0,0,0,airep,airep,0,0,0,            & ! 91-100
      tamdar,0,0,0,0,0,0,0,0,0,                                & ! 101-110
      gpspw,0,0,gpspw,0,gpsref,0,gpseph,0,0,  & ! 111-120
      ssmt1,ssmt2,0,0,ssmi_rv,0,0,0,0,0,            & ! 121-130
      0,profiler,airsr,0,bogus,0,0,0,0,0, & ! 131-140
      0,0,0,0,0,0,0,0,0,0,                                & ! 141-150
      0,0,0,0,0,0,0,0,0,0,                                & ! 151-160
      mtgirs,0,0,0,0,0,0,0,0,0,                            & ! 161-170
      0,0,0,0,0,0,0,0,0,0,                                & ! 171-180
      0,0,0,0,0,0,0,0,0,0,                                & ! 181-190
      0,0,0,0,0,0,0,0,0,0,                                & ! 191-200
      0,0,0,0,0,0,0,0,0,0,                                & ! 201-210
      0,0,0,0,0,0,0,0,0,0,                                & ! 211-220
      0,0,0,0,0,0,0,0,0,0,                                & ! 231-230
      0,0,0,0,0,0,0,0,0,0,                                & ! 231-240
      0,0,0,0,0,0,0,0,0,0,                                & ! 241-250
      0,0,0,0,0,0,0,0,0,0,                                & ! 251-260
      0,0,0,0,0,0,0,0,0,0,                                & ! 261-270
      0,0,0,0,0,0,0,0,0,0,                                & ! 271-280
      qscat,0,0,0,0,0,0,0,0,0 /)                      ! 281-290

   character(len=120)  :: fmt_info ='(a12,1x,a19,1x,a40,1x,i6,3(f12.3,11x),6x,a5)'
   character(len=120)  :: fmt_srfc = '(7(:,f12.3,i4,f7.2))'
!   character(len=120)  :: fmt_srfc = '(f12.3,i4,f7.2,F12.3,I4,F7.3)'
   character(len=120)  :: fmt_each = &
      '(3(f12.3,i4,f7.2),11x,3(f12.3,i4,f7.2),11x,3(f12.3,i4,f7.2))'

   ! lat/long information calculated in da_setup_firstguess_wrf

   real, parameter :: deg_to_rad = pi/180.0
   real, parameter :: rad_to_deg = 1.0/deg_to_rad
  
   real, allocatable :: cos_xls(:)
   real, allocatable :: sin_xls(:)
   real, allocatable :: cos_xle(:)
   real, allocatable :: sin_xle(:)

   integer :: ierr ! General error code
   integer :: comm ! MPI communicator

   integer :: ids,ide,jds,jde,kds,kde
   integer :: ims,ime,jms,jme,kms,kme
   integer :: its,ite,jts,jte,kts,kte
   integer :: ips,ipe,jps,jpe,kps,kpe
   integer :: itsy,itey,jtsy,jtey,ktsy,ktey
   integer :: itsx,itex,jtsx,jtex,ktsx,ktex

   integer :: ide_ens,jde_ens,kde_ens

   integer :: its_int,ite_int,jts_int,jte_int,kts_int,kte_int
   integer :: ids_int,ide_int,jds_int,jde_int,kds_int,kde_int
   integer :: ims_int,ime_int,jms_int,jme_int,kms_int,kme_int
   integer :: ips_int,ipe_int,jps_int,jpe_int,kps_int,kpe_int

   character (len=filename_len) :: input_file_ens = 'fg_ens'

   TYPE dual_res_type
         integer :: i
         integer :: j
         real    :: dx
         real    :: dy
         real    :: dxm
         real    :: dym
   END TYPE dual_res_type
   TYPE(dual_res_type), allocatable :: aens_locs(:,:)

   integer :: num_qcstat_conv(2,num_ob_indexes,num_ob_vars,npres_print+1)
   character*4, parameter :: ob_vars(num_ob_vars) = (/'U   ','V   ','T   ',&
                                                      'Q   ','Ps  ','Spd ',&
                                                      'Tpw ','GpsR','Thic','Rain'/)
   real, parameter :: pptop(1:npres_print) = (/ 1000.0, 900.0, 800.0, 600.0, 400.0, 300.0,  &
                      250.0,  200.0, 150.0, 100.0, 50.0, 0./)

   real, parameter :: ppbot(npres_print) = (/ 1200.0, 999.9, 899.9, 799.0, 599.9, 399.9,  &
                      299.9,  249.9, 199.9, 149.9, 99.9, 49.9/)

   real*8, allocatable :: time_slots(:)
   integer             :: ifgat_ana !index of First Guess at Appropriate Time of analysis

   logical :: global

   logical, allocatable :: fgat_rain_flags(:)

end module da_control
