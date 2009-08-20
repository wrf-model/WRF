module module_radiance

   !---------------------------------------------------------------------------
   ! Purpose: module for radiance data assimilation. 
   !---------------------------------------------------------------------------

   use da_control, only : pi, use_landem, t_landem, t_kelvin
   use da_reporting, only : da_error,message

#ifdef RTTOV
   use rttov_const,  only : &
            errorstatus_success, &
            errorstatus_fatal,   &
            platform_name      , &
            inst_name          , &
            gas_id_watervapour  ,&
            gas_unit_specconc   ,&
            gas_unit_ppmv, sensor_id_mw
   use rttov_types
#endif

#ifdef CRTM
  ! -- Modules to define CRTM constants etc.
   USE CRTM_Parameters, only : INVALID_WMO_SENSOR_ID
   !USE Type_Kinds
   !USE Error_Handler
   !USE CRTM_Utility

  ! -- CRTM RT_models modules
   USE CRTM_Module, only : graupel_cloud, rain_cloud, snow_cloud,crtm_adjoint, &
      crtm_allocate_atmosphere, crtm_allocate_surface, crtm_assign_atmosphere, &
      crtm_assign_surface,crtm_destroy_atmosphere,crtm_destroy_surface, &
      crtm_forward,crtm_init,crtm_k_matrix, &
      crtm_tangent_linear, grass_soil, h2o_id,hail_cloud,ice_cloud,new_snow, &
      o3_id, water_cloud, crtm_rtsolution_type, crtm_channelinfo_type, &
      crtm_atmosphere_type, crtm_surface_type, crtm_geometryinfo_type, &
      crtm_zero_surface, crtm_zero_atmosphere, crtm_destroy, &
      crtm_destroy_channelinfo, climatology_model_name, &
      crtm_options_type, crtm_allocate_options, crtm_destroy_options

   USE CRTM_SensorInfo
   USE CRTM_Planck_Functions, only : CRTM_Planck_Temperature, &
      CRTM_Planck_Radiance, CRTM_Planck_Temperature_TL, &
      CRTM_Planck_Temperature_AD
#endif

   use gsi_kinds      ,  only : r_kind,r_double,i_kind,r_single
   use gsi_constants  ,  only : deg2rad, rad2deg,       &
                            init_constants_derived, &
                            one, three, zero, half, &
                            one_tenth, two, four

   ! use irsse_model, only: forward_irsse
   implicit none
   
   real, parameter             :: q2ppmv = 1.60771704e+6

  Character (len=8), Parameter :: rttov_platform_name(1:20) = &
       (/ 'noaa    ', 'dmsp    ', 'meteosat', 'goes    ', 'gms     ', &
          'fy2     ', 'trmm    ', 'ers     ', 'eos     ', 'metop   ', &
          'envisat ', 'msg     ', 'fy1     ', 'adeos   ', 'mtsat   ', &
          'coriolis', 'npoess  ', 'gifts   ', 'xxxxxxxx', 'xxxxxxxx'/)

  ! List of instruments  !!!! HIRS is number 0
  Character (len=8), Dimension(0:34) :: rttov_inst_name  =                &
       & (/ 'hirs    ', 'msu     ', 'ssu     ', 'amsua   ', 'amsub   ',  &
       &    'avhrr   ', 'ssmi    ', 'vtpr1   ', 'vtpr2   ', 'tmi     ',  &
       &    'ssmis   ', 'airs    ', 'hsb     ', 'modis   ', 'atsr    ',  &
       &    'mhs     ', 'iasi    ', 'amsr    ', 'imager  ', 'atms    ',  &
       &    'mviri   ', 'seviri  ', 'imager  ', 'sounder ', 'imager  ',  &
       &    'vissr   ', 'mvisr   ', 'cris    ', 'cmis    ', 'viirs   ',  &
       &    'windsat ', 'gifts   ', 'xxxxxxxx', 'airs    ', 'xxxxxxxx'   /)

! n=noaa; f=dmsp; g=goes; c=npoess; eos-1/2=aqua/terra;
   character(len=8), parameter :: crtm_platform_name(1:20) = &
       (/ 'n       ', 'f       ', 'meteosat', 'g       ', 'gms     ', &
          'fy2     ', 'trmm    ', 'ers     ', 'eos     ', 'metop   ', &
          'envisat ', 'msg     ', 'fy1     ', 'adeos   ', 'mtsat   ', &
          'coriolis', 'c       ', 'gifts   ', 'xxxxxxxx', 'xxxxxxxx'/)

! List of instruments  !!!! HIRS is number 0
  Character (len=8), Dimension(0:34) :: crtm_sensor_name  =                &
       & (/ 'hirs    ', 'msu     ', 'ssu     ', 'amsua   ', 'amsub   ',  &
       &    'avhrr   ', 'ssmi    ', 'vtpr1   ', 'vtpr2   ', 'tmi     ',  &
       &    'ssmis   ', 'airs    ', 'hsb     ', 'modis   ', 'atsr    ',  &
       &    'mhs     ', 'iasi    ', 'amsre   ', 'imager  ', 'atms    ',  &
       &    'mviri   ', 'seviri  ', 'imgr    ', 'sndr    ', 'imager  ',  &
       &    'vissr   ', 'mvisr   ', 'cris    ', 'cmis    ', 'viirs   ',  &
       &    'windsat ', 'gifts   ', 'amsre   ', 'xxxxxxxx', 'xxxxxxxx'   /)

   integer                     :: n_scatt_coef
   character(len=5), pointer   :: coefs_scatt_instname(:)
#ifdef RTTOV
   type( rttov_coef ), pointer :: coefs(:)         ! RTTOV8_5 coefficients
   type( rttov_scatt_coef ), pointer :: coefs_scatt(:)
#endif

   type satinfo_type
      integer, pointer   :: ichan(:)      ! channel index
      integer, pointer   :: iuse (:)      ! usage flag (-1: not use) from GSI info file
      real   , pointer   :: error(:)      ! error Standard Deviation from GSI info file
      real   , pointer   :: polar(:)      ! polarisation (0:ver; 1:hori) from GSI info file
      real   , pointer   :: error_factor(:) ! error tuning factor ! from error tuning file
     ! new air mass bias correction coefs.
      real   , pointer   :: scanbias(:,:) ! scan bias without latitude band variation
      real   , pointer   :: scanbias_b(:,:,:) ! scan bias with latitude band variation
      real   , pointer   :: bcoef(:,:)   ! airmass predictor bias coefficients
      real   , pointer   :: bcoef0(:)    ! airmass constant coefficient
      real   , pointer   :: error_std(:) ! error standard deviation
   end type satinfo_type

   type (satinfo_type), pointer :: satinfo(:)

   CHARACTER( 80 ), allocatable, save :: Sensor_Descriptor(:)

contains

#include "gsi_emiss.inc"
#include "emiss_ssmi.inc"
#include "iceem_amsu.inc"
#include "siem_ats.inc"
#include "siem_bts.inc"
#include "siem_interpolate.inc"
#include "landem.inc"
#include "snwem_amsu.inc"
#include "seaem.inc"
#include "ossmem.inc"

end module module_radiance

