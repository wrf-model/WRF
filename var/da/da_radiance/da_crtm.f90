module da_crtm

   !---------------------------------------------------------------------------
   ! Purpose: module for CRTM radiance data assimilation. 
   !---------------------------------------------------------------------------

#ifdef CRTM

   use module_domain, only : x_type, domain
   use da_define_structures, only : y_type, iv_type

   use module_radiance, only : CRTM_RTSolution_type,CRTM_ChannelInfo_type, &
      CRTM_Atmosphere_type, CRTM_Surface_type,CRTM_Geometry_type, &
      CRTM_Adjoint,CRTM_Forward,CRTM_Tangent_Linear, &
      CRTM_K_Matrix, CRTM_Planck_Temperature, CRTM_Planck_Temperature_TL, &
      CRTM_Planck_Temperature_AD, CRTM_Planck_Radiance, &
      CRTM_Atmosphere_Create,H2O_ID,GRAUPEL_CLOUD,ICE_CLOUD,HAIL_CLOUD, &
      NEW_SNOW,rain_cloud,snow_cloud,O3_ID, GRASS_SOIL, &
      WATER_CLOUD, Sensor_Descriptor, MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS, &
      crtm_atmosphere_destroy, crtm_sensor_name, &
      crtm_surface_create,crtm_surface_destroy, &
      crtm_surface_zero, CRTM_Atmosphere_zero, satinfo, &
      crtm_platform_name, crtm_init, &
      rttov_inst_name,rttov_platform_name, climatology_model_name, &
      crtm_options_type, crtm_options_create, crtm_options_destroy, &
      crtm_atmosphere_associated, crtm_surface_associated, &
      crtm_options_associated, &
      crtm_rtsolution_create, crtm_rtsolution_destroy, crtm_rtsolution_associated

   use da_control, only : trace_use, crtm_cloud, gravity,stdout, biascorr, &
      biasprep, qc_rad,missing_r,rtminit_sensor,rtminit_nsensor, filename_len, &
      use_error_factor_rad,read_biascoef, analysis_date,time_window_max, &
      time_window_min,num_fgat_time,rtminit_platform, print_detail_rad, &
      rtminit_satid, global,kms,kme,ims,ime,jms,jme,kts,kte,use_clddet_mmr, &
      use_crtm_kmatrix, use_varbc, freeze_varbc, use_pseudo_rad, &
      use_antcorr, time_slots, use_satcv, use_simulated_rad, simulated_rad_io, &
      simulated_rad_ngrid, interp_option, use_mspps_emis, use_mspps_ts, calc_weightfunc, &
	  use_clddet_ecmwf,its,ite,jts,jte	  
   use da_interpolation, only : da_interp_lin_2d_partial,da_interp_lin_2d_adj_partial, &
      da_interp_2d_partial
   use module_dm, only : wrf_dm_sum_real, wrf_dm_sum_reals
   use da_radiance1, only : da_biasprep,da_detsurtyp,da_biascorr, &
       da_biasprep,da_cld_eff_radius, da_mspps_emis, da_mspps_ts

   use da_reporting, only : da_error, message, da_warning, da_message
   use da_tools_serial, only : da_free_unit, da_get_unit
   use da_tools, only: da_get_time_slots, da_eof_decomposition
   use da_tracing, only : da_trace_entry, da_trace_exit

   TYPE (CRTM_ChannelInfo_type), allocatable, save :: ChannelInfo(:)

contains

#include "da_transform_xtoy_crtm.inc"
#include "da_transform_xtoy_crtm_adj.inc"
#include "da_get_innov_vector_crtm.inc"
#include "da_crtm_tl.inc"
#include "da_crtm_k.inc"
#include "da_crtm_direct.inc"
#include "da_crtm_ad.inc"
#include "da_crtm_init.inc"
#include "da_crtm_sensor_descriptor.inc"
#include "da_det_crtm_climat.inc"

#endif

end module da_crtm

