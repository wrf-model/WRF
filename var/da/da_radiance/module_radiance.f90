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
            gas_id_watervapour,  &
            sensor_id_ir,        &
            sensor_id_mw,        &
            sensor_id_hi
   use rttov_types, only :  &
         rttov_options,     &
         rttov_opts_rt_ir,  &
         rttov_coefs,       &
         profile_type,      &
         transmission_type, &
         radiance_type,     &
         rttov_chanprof,    &
         rttov_emissivity
   use parkind1, only : jpim, jprb
#endif

#ifdef CRTM
  ! -- Modules to define CRTM constants etc.
   !USE Type_Kinds
   !USE Error_Handler
   !USE CRTM_Utility

  ! -- CRTM RT_models modules
   USE CRTM_Module, only : graupel_cloud, rain_cloud, snow_cloud,crtm_adjoint, &
      crtm_atmosphere_create, crtm_surface_create, &
      crtm_atmosphere_destroy, crtm_surface_destroy, &
      crtm_forward,crtm_init,crtm_k_matrix, &
      crtm_tangent_linear, grass_soil, h2o_id,hail_cloud,ice_cloud,new_snow, &
      o3_id, water_cloud, crtm_rtsolution_type, crtm_channelinfo_type, &
      crtm_atmosphere_type, crtm_surface_type, crtm_geometry_type, &
      crtm_surface_zero, crtm_atmosphere_zero, crtm_destroy, &
      climatology_model_name, &
      crtm_options_type, crtm_options_create, crtm_options_destroy, &
      crtm_rtsolution_create, crtm_rtsolution_destroy, crtm_rtsolution_associated
   USE CRTM_Atmosphere_Define, only: crtm_atmosphere_associated, &
      MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS
   USE CRTM_Surface_Define, only: crtm_surface_associated
   USE CRTM_Options_Define, only: crtm_options_associated

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
   
   real, parameter             :: q2ppmv = 1.60771704e+6   ! q_mixratio_to_ppmv

  Character (len=8), Parameter :: rttov_platform_name(1:27) =         &
     & (/ 'noaa    ', 'dmsp    ', 'meteosat', 'goes    ', 'gms     ', &
        & 'fy2     ', 'trmm    ', 'ers     ', 'eos     ', 'metop   ', &
        & 'envisat ', 'msg     ', 'fy1     ', 'adeos   ', 'mtsat   ', &
        & 'coriolis', 'jpss    ', 'gifts   ', 'tiros   ', 'meghatr ', &
        & 'kalpana ', 'insat_3d', 'fy3     ', 'coms    ', 'meteor-m', &
        & 'gosat   ', 'calipso '/)

  ! List of instruments  !!!! HIRS is number 0
  Character (len=8), Dimension(0:49) :: rttov_inst_name  =             &
     & (/ 'hirs    ', 'msu     ', 'ssu     ', 'amsua   ', 'amsub   ',  &
        & 'avhrr   ', 'ssmi    ', 'vtpr1   ', 'vtpr2   ', 'tmi     ',  &
        & 'ssmis   ', 'airs    ', 'hsb     ', 'modis   ', 'atsr    ',  &
        & 'mhs     ', 'iasi    ', 'amsr    ', 'imager  ', 'atms    ',  &
        & 'mviri   ', 'seviri  ', 'imager  ', 'sounder ', 'imager  ',  &
        & 'vissr   ', 'mvisr   ', 'cris    ', 'cmis    ', 'viirs   ',  &
        & 'windsat ', 'gifts   ', 'ssmt1   ', 'ssmt2   ', 'saphir  ',  &
        & 'madras  ', 'ssmisz  ', 'kavhrr  ', 'iimager ', 'isoundr ',  &
        & 'mwts    ', 'mwhs    ', 'iras    ', 'mwri    ', 'abi     ',  &
        & 'mi      ', 'msumr   ', 'tansofts', 'iir     ', 'mwr     '/)

! n=noaa; f=dmsp; g=goes; c=npoess/npp; eos-1/2=aqua/terra;
   character(len=8), parameter :: crtm_platform_name(1:23) = &
       (/ 'n       ', 'f       ', 'm       ', 'g       ', 'gms     ', &
          'fy2     ', 'trmm    ', 'ers     ', 'eos     ', 'metop   ', &
          'envisat ', 'msg     ', 'fy1     ', 'adeos   ', 'mtsat   ', &
          'coriolis', 'c       ', 'gifts   ', 'tiros   ', 'xxxxxxxx', &
          'xxxxxxxx', 'xxxxxxxx', 'fy3     '/)

! List of instruments  !!!! HIRS is number 0
  Character (len=8), Dimension(0:41) :: crtm_sensor_name  =                &
       & (/ 'hirs    ', 'msu     ', 'ssu     ', 'amsua   ', 'amsub   ',  &
       &    'avhrr   ', 'ssmi    ', 'vtpr1   ', 'vtpr2   ', 'tmi     ',  &
       &    'ssmis   ', 'airs    ', 'hsb     ', 'modis   ', 'atsr    ',  &
       &    'mhs     ', 'iasi    ', 'amsre   ', 'imager  ', 'atms    ',  &
       &    'mviri   ', 'seviri  ', 'imgr    ', 'sndr    ', 'imager  ',  &
       &    'vissr   ', 'mvisr   ', 'cris    ', 'cmis    ', 'viirs   ',  &
       &    'windsat ', 'gifts   ', 'amsre   ', 'xxxxxxxx', 'xxxxxxxx',  &
       &    'xxxxxxxx', 'xxxxxxxx', 'xxxxxxxx', 'xxxxxxxx', 'xxxxxxxx',  &
       &    'mwts    ', 'mwhs    '/)

#ifdef RTTOV
   type (rttov_coefs), allocatable   :: coefs(:)     ! coefficients structure
   type (rttov_options), allocatable :: opts(:)      ! options structure
   type (rttov_opts_rt_ir), allocatable :: opts_rt_ir(:) ! options structure
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

end module module_radiance

