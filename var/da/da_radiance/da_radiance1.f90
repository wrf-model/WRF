module da_radiance1

   !---------------------------------------------------------------------------
   ! Purpose: module for radiance data assimilation. 
   !---------------------------------------------------------------------------

#if defined(RTTOV) || defined(CRTM)
   use module_radiance, only : satinfo,q2ppmv,rttov_inst_name
#ifdef CRTM
   use module_radiance, only : CRTM_Planck_Radiance, CRTM_Planck_Temperature
#endif
#ifdef RTTOV
   use module_radiance, only : coefs
#endif

   use da_control, only : trace_use,missing_r, rootproc, &
      stdout,myproc,qc_good,num_fgat_time,qc_bad, &
      use_error_factor_rad,biasprep_unit,obs_qc_pointer, filename_len, &
      print_detail_rad, rtm_option, trace_use_dull, &
      rtm_option_rttov,rtm_option_crtm, radiance, only_sea_rad, &
      global, gas_constant, gravity, monitor_on,kts,kte,use_rttov_kmatrix, &
      use_pseudo_rad, pi, t_triple, crtm_cloud, DT_cloud_model,write_jacobian, &
      use_crtm_kmatrix,use_clddet_mmr, use_satcv, cv_size_domain, &
      cv_size_domain_js, calc_weightfunc, use_clddet_ecmwf, deg_to_rad, rad_to_deg
   use da_define_structures, only : info_type,model_loc_type,maxmin_type, &
      iv_type, y_type, jo_type,bad_data_type,bad_data_type,number_type, &
      be_type
   use module_dm, only : wrf_dm_sum_real, wrf_dm_sum_integer
   use da_par_util, only : da_proc_stats_combine
   use da_par_util1, only : da_proc_sum_int,da_proc_sum_ints
   use da_reporting, only : da_error, message
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_residual_new, da_eof_decomposition
   use da_tools_serial, only : da_free_unit, da_get_unit
   use da_tracing, only : da_trace_entry, da_trace_exit, da_trace_int_sort

#if defined(RTTOV) || defined(CRTM)
   use da_control, only : rtminit_sensor,write_profile,num_procs,tovs_min_transfer
   use da_reporting, only : da_warning, da_message
   use da_tracing, only : da_trace
#endif

   implicit none
   
   type datalink_type

      type (info_type)        :: info
      type (model_loc_type)   :: loc

      integer   ::  ifgat, landsea_mask, rain_flag
      integer   ::  scanline, scanpos
      real      ::  satzen, satazi, solzen, solazi  !  satellite and solar angles
      ! channels' bright temperature
      real,    pointer   ::   emiss(:)
      ! logical, pointer   ::   calcemis(:)
      integer, pointer   ::   cloud_flag(:)
      real,    pointer   ::   t(:), mr(:), zk(:)
      real,    pointer   ::   pm(:), tm(:), qm(:), qrn(:), qcw(:),qci(:),qsn(:),qgr(:)
      real               ::   ps,ts,t2m,mr2m,u10,v10, clwp
      real               ::   smois, tslb, snowh, elevation,soiltyp,vegtyp,vegfra
      real               ::   clw
      integer            ::   isflg
      integer            ::   cloudflag

!      real,    pointer   ::   tb_xb(:)
      real, pointer             :: tb_ob(:)
      real, pointer             :: tb_inv(:)
      real, pointer             :: tb_qc(:)
      real, pointer             :: tb_error(:)
      integer                   :: sensor_index
      type (datalink_type), pointer  :: next ! pointer to next data
   end type datalink_type

   type con_vars_type
      integer            ::  nlevels
      real   ,  pointer  ::  t(:)
      real   ,  pointer  ::  q(:)
      real               ::  ps
      real   ,  pointer  ::  t_jac(:,:) => null()
      real   ,  pointer  ::  q_jac(:,:) => null()
      real   ,  pointer  ::  ps_jac(:)  => null()
   end type con_vars_type

   type con_cld_vars_type
      integer            ::  nwp_levels
      real   ,  pointer  ::  p(:)
      real   ,  pointer  ::  ph(:)
      real   ,  pointer  ::  t(:)
      real   ,  pointer  ::  cc(:)
      real   ,  pointer  ::  clw(:)   ! kg/kg
      real   ,  pointer  ::  ciw(:)   ! kg/kg
      real   ,  pointer  ::  rain(:)  ! kg/m2/s
      real   ,  pointer  ::  sp(:)    ! kg/m2/s
     integer ,  pointer  ::  cloudflag(:)
   end type con_cld_vars_type

   type aux_vars_type
      integer            ::  surftype
      real               ::  surft, t2m, q2m, u10, v10
      real               ::  satzen, satazi  !!, fastem(5)
      real               ::  solzen, solazi
      real               ::  elevation ,rlat
   end type aux_vars_type

   type maxmin_rad_stats_type
      type (maxmin_type)         :: maximum, minimum
      real                       :: ave, rms
      integer                    :: num
   end type maxmin_rad_stats_type

   type stats_rad_type
      type (maxmin_rad_stats_type), pointer  :: ichan(:)
   end type stats_rad_type

   type rad_header_type                       ! innovation file header
      character (LEN = 19) :: date_char       ! YYYY-MM-DD-HH (assimilation window date)
      integer              :: assim_win       ! assimilation window hours, e.g., 6 hours
      character(LEN=20)    :: rttovid_string  ! e.g., noaa-16-amsua 
      integer              :: platform_id     ! e.g., 1  for noaa (see RTTOV UG)
      integer              :: satellite_id    ! e.g., 16 for noaa-16
      integer              :: sensor_id       ! e.g., 3  for amsua
      integer              :: num_rad         ! pixel number in file
      integer              :: nchan           ! channel number of each pixel
      integer ,  pointer   :: ichan(:)        ! index of nchan channels
      integer              :: nemis           ! emissivity number of each pixel
                                              ! may be different with nchan
                                              ! due to polarisation in microwave
      integer              :: nlevel_fix      ! fixed pressure level number for RTM 
                                              ! e.g., 43 for RTTOV8_5 
      real   ,   pointer   :: pres(:)         ! pressure with nlevel_fix
      integer              :: nlevel_cld      ! cloud profile level number for RTM
   end type rad_header_type

   type rad_data_type                       ! innovation file pixel data

      ! part from Observation
      integer            :: landmask      ! 1:land; 0:sea
      integer            :: scanline      ! number of scan line
      integer            :: scanpos       ! number of scan position
      real               :: lat           ! Latitude in degree
      real               :: lon           ! Longitude in degree
      real               :: elv           ! Elevation in m
      real               :: satzen        ! satellite zenith angle in degree
      real               :: satazi        ! satellite azimuth angle in degree
      real               :: solzen        ! solar zenith angle in degree
      real               :: solazi        ! solar azimuth angle in degree
      real,    pointer   :: tb(:)         ! observed brightness temperatures in Kelvin
      real,    pointer   :: inv(:)        ! innovation (obs - background) in Kelvin
      real,    pointer   :: bias(:)       ! bias correction values in Kelvin
      real,    pointer   :: err(:)        ! std of observation error in Kelvin
      real,    pointer   :: qc(:)         ! quality control flag
                                          ! 0:good; <0:rejected; other:suspected
      real,    pointer   :: emiss(:)      ! surface emissivity

      !  part from background field
      integer            :: surftype      ! surface type
                                          ! 0:sea     1:sea-ice     2:land     3:snow
                                          ! 4:mix-sea 5:mix-sea-ice 6:mix-land 7:mix-snow
      integer            :: terrain       ! model terrain in m
      integer            :: soiltyp       ! soil type (MM5/WRF USGS 24 catagories)
      integer            :: vegtyp        ! vegetation type (MM5/WRF 16 catagories)
      real               :: vegfra        ! vegetation fraction
      real               :: soilm         ! soil moisture
      real               :: soilt         ! soil temperature
      real               :: snowh         ! snow depth
      real               :: ps            ! surface pressure in hPa
      real               :: ts            ! surface skin temperature in Kelvin
      real               :: t2m           ! T in Kelvin at 2m 
      real               :: mr2m          ! volume mixture ratio in ppmv at 2m
      real               :: u10,v10       ! u/v wind in m/s at 10m
      real,    pointer   :: t(:)          ! temperatures at fixed pressure levels
      real,    pointer   :: mr(:)         ! volume mixture ratio in ppmv at fixed pressure levels
      real,    pointer   :: zk(:)         ! vertical interpolation weight from model level to fixed pressure levels
      real,    pointer   :: pm(:)         ! full-level pressure at model levels
      real,    pointer   :: phm(:)        ! half-level pressure at model levels
      real,    pointer   :: tm(:)         ! temperatures at model levels
      real,    pointer   :: cc(:)         ! cloud cover at model levels
      real,    pointer   :: rain(:)       ! rainfall rate in kg/m2/s
      real,    pointer   :: solidp(:)     ! solid precipitation rate in kg/m2/s
      real,    pointer   :: clw(:)        ! cloud liquid water (kg/kg)
      real,    pointer   :: ciw(:)        ! cloud ice water    (kg/kg)
      integer, pointer   :: cloudflag(:)  ! cloud flag

   end type rad_data_type

   type bias_type
      integer :: nchan     ! number of channels
      integer :: npred     ! number of predictors
      integer :: platform_id,satellite_id,sensor_id
      integer :: year, month, day, hour, min, sec
      integer :: scanline,scanpos
      integer :: landmask
      integer, pointer :: qc_flag(:) ! 1/0:good/bad
      integer, pointer :: cloud_flag(:) ! 1/0:no-cloud/cloud
      integer :: surf_flag  ! surface type
      real    :: elevation,lat,lon,ps, t2m, q2m, tsk, clwp
      real, pointer  :: tb(:), omb(:), bias(:)
      real, pointer  :: pred(:)
   end type bias_type

   integer, allocatable :: num_tovs_before(:,:)
   integer, allocatable :: num_tovs_after(:,:)
   integer, allocatable :: tovs_send_pe(:,:)
   integer, allocatable :: tovs_send_start(:,:)
   integer, allocatable :: tovs_send_count(:,:)
   integer, allocatable :: tovs_recv_pe(:,:)
   integer, allocatable :: tovs_recv_start(:,:)
   integer, allocatable :: tovs_copy_count(:)

contains

#include "da_jo_and_grady_rad.inc"
#include "da_residual_rad.inc"
#include "da_biascorr.inc"
#include "da_read_biascoef.inc"
#include "da_biasprep.inc"
#include "da_write_biasprep.inc"
#ifdef RTTOV
#include "da_predictor_rttov.inc"
#endif
#ifdef CRTM
#include "da_predictor_crtm.inc"
#include "da_qc_crtm.inc"
#endif
#include "da_cloud_sim.inc"
#include "da_cloud_detect_airs.inc"
#include "da_cloud_detect_iasi.inc"
#include "da_qc_airs.inc"
#include "da_qc_amsua.inc"
#include "da_qc_amsub.inc"
#include "da_qc_hirs.inc"
#include "da_qc_ssmis.inc"
#include "da_qc_iasi.inc"
#include "da_qc_mhs.inc"
#include "da_qc_mwts.inc"
#include "da_qc_mwhs.inc"
#include "da_qc_mwhs2.inc"
#include "da_qc_atms.inc"
#include "da_qc_seviri.inc"
#include "da_qc_amsr2.inc"
#include "da_qc_ahi.inc"
#include "da_qc_goesimg.inc"
#include "da_write_iv_rad_ascii.inc"
#include "da_write_oa_rad_ascii.inc"
#include "da_detsurtyp.inc"
#include "da_cld_eff_radius.inc"
#include "da_ao_stats_rad.inc"
#include "da_oi_stats_rad.inc"
#include "da_print_stats_rad.inc"
#include "da_qc_rad.inc"
#include "da_setup_satcv.inc"
#include "da_mspps_emis.inc"
#include "da_mspps_ts.inc"

#endif

end module da_radiance1

