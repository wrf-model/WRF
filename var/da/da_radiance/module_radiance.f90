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
      crtm_tangent_linear, h2o_id,hail_cloud,ice_cloud, &
      o3_id, water_cloud, crtm_rtsolution_type, crtm_channelinfo_type, &
      crtm_atmosphere_type, crtm_surface_type, crtm_geometry_type, &
      crtm_surface_zero, crtm_atmosphere_zero, crtm_destroy, &
      climatology_model_name, &
      crtm_options_type, crtm_options_create, crtm_options_destroy, &
      crtm_rtsolution_create, crtm_rtsolution_destroy, crtm_rtsolution_associated, &
      crtm_irlandcoeff_classification
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

  ! cf. RTTOV-11 Users Guide Table 2
  ! index 19 is sentinel3 in Table 2, here we keep it as tiros for 
  ! WRFDA backward compatibility
  Character (len=8), Parameter :: rttov_platform_name(1:35) =          &
     & (/ 'noaa    ', 'dmsp    ', 'meteosat', 'goes    ', 'gms     ',  &
        & 'fy2     ', 'trmm    ', 'ers     ', 'eos     ', 'metop   ',  &
        & 'envisat ', 'msg     ', 'fy1     ', 'adeos   ', 'mtsat   ',  &
        & 'coriolis', 'jpss    ', 'gifts   ', 'tiros   ', 'meghatr ',  &
        & 'kalpana ', 'reserved', 'fy3     ', 'coms    ', 'meteor-m',  &
        & 'gosat   ', 'calipso ', 'reserved', 'gcom-w  ', 'nimbus  ',  &
        & 'himawari', 'mtg     ', 'saral   ', 'metop-ng', 'landsat '/)

  ! cf. RTTOV-11 Users Guide Table 3
  ! List of instruments  !!!! HIRS is number 0
  Character (len=8), Dimension(0:65) :: rttov_inst_name  =             &
     & (/ 'hirs    ', 'msu     ', 'ssu     ', 'amsua   ', 'amsub   ',  &
        & 'avhrr   ', 'ssmi    ', 'vtpr1   ', 'spare   ', 'tmi     ',  &
        & 'ssmis   ', 'airs    ', 'hsb     ', 'modis   ', 'atsr    ',  &
        & 'mhs     ', 'iasi    ', 'amsre   ', 'imager  ', 'atms    ',  &
        & 'mviri   ', 'seviri  ', 'imager  ', 'sounder ', 'imager  ',  &
        & 'vissr   ', 'mvisr   ', 'cris    ', 'spare   ', 'viirs   ',  &
        & 'windsat ', 'gifts   ', 'ssmt1   ', 'ssmt2   ', 'saphir  ',  &
        & 'madras  ', 'spare   ', 'imager  ', 'reserved', 'reserved',  &
        & 'mwts    ', 'mwhs    ', 'iras    ', 'mwri    ', 'abi     ',  &
        & 'mi      ', 'msumr   ', 'reserved', 'iir     ', 'mwr     ',  &
        & 'reserved', 'reserved', 'reserved', 'reserved', 'scams   ',  &
        & 'smmr    ', 'ahi     ', 'irs     ', 'altika  ', 'iasing  ',  &
        & 'tm      ', 'fci     ', 'amsr1   ', 'amsr2   ', 'vissr   ',  &
        & 'slstr   '/)

  ! cf. rttov_platform_name above and CRTM: v2.1.3 User Guide Table B.1
  ! n=noaa; f=dmsp; g=goes; eos-2/1=aqua/terra;
  ! xxxxxxxx means crtm does not have corresponding coefficient file.
  ! For satellite names that can not be directly mapped here to names
  ! used in crtm coeff names, they will be re-set in
  ! da_crtm_sensor_descriptor.inc
  Character (len=8), Parameter :: crtm_platform_name(1:35) =           &
     & (/ 'n       ', 'f       ', 'm       ', 'g       ', 'gms     ',  &
        & 'xxxxxxxx', 'trmm    ', 'ers     ', 'eos     ', 'metop   ',  &
        & 'envisat ', 'msg     ', 'xxxxxxxx', 'xxxxxxxx', 'mt      ',  &
        & 'coriolis', 'npp     ', 'gifts   ', 'tiros   ', 'meghat  ',  &
        & 'kalpana ', 'tiros   ', 'fy3     ', 'coms    ', 'xxxxxxxx',  &
        & 'xxxxxxxx', 'xxxxxxxx', 'reserved', 'gcom-w  ', 'xxxxxxxx',  &
        & 'xxxxxxxx', 'xxxxxxxx', 'xxxxxxxx', 'xxxxxxxx', 'xxxxxxxx'/)

  ! cf. rttov_inst_name above and CRTM: v2.1.3 User Guide Table B.1
  ! List of instruments  !!!! HIRS is number 0
  ! xxxxxxxx means crtm does not have corresponding coefficient file.
  ! For instrument names that can not be directly mapped here to names
  ! used in crtm coeff names, they will be re-set in
  ! da_crtm_sensor_descriptor.inc
  Character (len=8), Dimension(0:65) :: crtm_sensor_name  =            &
     & (/ 'hirs    ', 'msu     ', 'ssu     ', 'amsua   ', 'amsub   ',  &
        & 'avhrr   ', 'ssmi    ', 'xxxxxxxx', 'spare   ', 'tmi     ',  &
        & 'ssmis   ', 'airs    ', 'hsb     ', 'modis   ', 'atsr    ',  &
        & 'mhs     ', 'iasi    ', 'amsre   ', 'imgr    ', 'atms    ',  &
        & 'mviri   ', 'seviri  ', 'imgr    ', 'sndr    ', 'imgr    ',  &
        & 'vissr   ', 'xxxxxxxx', 'cris    ', 'spare   ', 'viirs   ',  &
        & 'windsat ', 'xxxxxxxx', 'ssmt1   ', 'ssmt2   ', 'saphir  ',  &
        & 'madras  ', 'spare   ', 'imgr    ', 'reserved', 'reserved',  &
        & 'mwts    ', 'mwhs    ', 'iras    ', 'mwri    ', 'abi     ',  &
        & 'xxxxxxxx', 'xxxxxxxx', 'reserved', 'xxxxxxxx', 'xxxxxxxx',  &
        & 'reserved', 'reserved', 'reserved', 'reserved', 'xxxxxxxx',  &
        & 'xxxxxxxx', 'xxxxxxxx', 'xxxxxxxx', 'xxxxxxxx', 'xxxxxxxx',  &
        & 'xxxxxxxx', 'xxxxxxxx', 'xxxxxxxx', 'amsr2   ', 'vissr   ',  &
        & 'xxxxxxxx'/)

#ifdef RTTOV
   type (rttov_coefs), allocatable   :: coefs(:)     ! coefficients structure
   type (rttov_options), allocatable :: opts(:)      ! options structure
   type (rttov_opts_rt_ir), allocatable :: opts_rt_ir(:) ! options structure
#endif

   type satinfo_type
      integer, pointer   :: ichan(:)      ! channel index
      integer, pointer   :: iuse (:)      ! usage flag (-1: not use) from radiance info file
      real   , pointer   :: error(:)      ! error Standard Deviation from radiance info file
      real   , pointer   :: error_cld(:)  ! error Standard Deviation for cloudy radiance from radiance info file
      real   , pointer   :: polar(:)      ! polarisation (0:ver; 1:hori) from radiance info file
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

