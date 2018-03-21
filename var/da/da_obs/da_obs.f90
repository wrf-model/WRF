module da_obs

   use da_define_structures, only : multi_level_type, y_type, iv_type, infa_type, &
      field_type, each_level_type,da_allocate_y, da_random_seed,da_allocate_y_rain, &
      da_allocate_y_radar
   use module_domain, only : domain, x_type

   use da_airep, only : da_transform_xtoy_airep, da_transform_xtoy_airep_adj 
   use da_airsr, only : da_transform_xtoy_airsr, da_transform_xtoy_airsr_adj 
   use da_bogus, only : da_transform_xtoy_bogus, da_transform_xtoy_bogus_adj
   use da_buoy, only : da_transform_xtoy_buoy,da_transform_xtoy_buoy_adj
   use da_control, only : use_shipsobs, use_synopobs, use_ssmt2obs, &
      use_soundobs,use_mtgirsobs,use_satemobs, use_profilerobs, use_pilotobs, &
      use_qscatobs,use_metarobs, use_polaramvobs, use_geoamvobs, &
      use_bogusobs,use_buoyobs, use_airsretobs, use_tamdarobs, trace_use, num_procs, &
      xmiss, missing_r, missing, use_airepobs,use_gpspwobs,use_gpsztdobs,use_gpsrefobs, &
      use_ssmt1obs,filtered_obs_unit,fmt_each,fmt_info,fmt_srfc, ide, jde, &
      pseudo_x, fg_format, fg_format_kma_global, fg_format_wrf_arw_regional,fg_format_wrf_nmm_regional, &
      missing_data, pseudo_var, pseudo_val,stdout, num_pseudo, pseudo_y, pseudo_z, &
      pseudo_err,obs_qc_pointer,myproc,rtm_option,rtm_option_rttov, &
      rtm_option_crtm,use_rad, base_temp, base_lapse, base_pres, &
      ob_format,ob_format_ascii,filename_len, trace_use_dull, &
      sound, mtgirs, synop, profiler, gpsref, gpseph, gpspw, polaramv, geoamv, ships, metar, &
      satem, radar, ssmi_rv, ssmi_tb, ssmt1, ssmt2, airsr, pilot, airep, sonde_sfc,rain, &
      bogus, buoy, qscat, tamdar, tamdar_sfc, pseudo, num_ob_indexes, its,ite,jds,jts,jte,ids, &
      write_mod_filtered_obs, radiance, use_varbc, obs_names, q_error_options, kts,kte,kds,kde, &
      use_gpsephobs
   ! use_crtm_kmatrix,use_crtm_kmatrix_fast
   use da_control, only : pseudo_tpw, pseudo_ztd, pseudo_ref, pseudo_uvtpq
   use da_define_structures, only : da_allocate_obs_info
#ifdef CRTM
   use da_crtm, only : da_transform_xtoy_crtm, da_transform_xtoy_crtm_adj
      !da_transform_xtoy_crtmk,da_transform_xtoy_crtmk_adj
      !da_transform_xtoy_crtmk_f, da_transform_xtoy_crtmk_f_adj
#endif
   use da_geoamv,    only : da_transform_xtoy_geoamv, da_transform_xtoy_geoamv_adj
   use da_gpspw,     only : da_transform_xtoy_gpspw,da_transform_xtoy_gpspw_adj, &
                            da_transform_xtoy_gpsztd,da_transform_xtoy_gpsztd_adj
   use da_gpsref,    only : da_transform_xtoy_gpsref,da_transform_xtoy_gpsref_adj
   use da_gpseph,    only : da_transform_xtoy_gpseph,da_transform_xtoy_gpseph_adj, &
      global_adj_ref, global_h_mean, global_h, global_xa_ref, global_ref, gps_rays
   use da_metar,     only : da_transform_xtoy_metar, da_transform_xtoy_metar_adj
   use da_physics,   only : da_tp_to_qs,da_get_q_error
   use da_pilot,     only : da_transform_xtoy_pilot,da_transform_xtoy_pilot_adj
   use da_polaramv,  only : da_transform_xtoy_polaramv, da_transform_xtoy_polaramv_adj
   use da_profiler,  only : da_transform_xtoy_profiler, da_transform_xtoy_profiler_adj
   use da_pseudo,    only : da_transform_xtoy_pseudo, da_transform_xtoy_pseudo_adj
   use da_qscat,     only : da_transform_xtoy_qscat,da_transform_xtoy_qscat_adj
   use da_radar,     only : da_transform_xtoy_radar,da_transform_xtoy_radar_adj
   use da_rain,      only : da_transform_xtoy_rain,da_transform_xtoy_rain_adj
   use da_reporting, only : da_error, message, da_warning, da_message
#ifdef RTTOV
   use da_rttov,     only : da_transform_xtoy_rttov,da_transform_xtoy_rttov_adj
#endif
   use da_satem,     only : da_transform_xtoy_satem, da_transform_xtoy_satem_adj
   use da_ships,     only : da_transform_xtoy_ships, da_transform_xtoy_ships_adj
   use da_sound,     only : da_transform_xtoy_sound, da_transform_xtoy_sonde_sfc, &
      da_transform_xtoy_sound_adj, da_transform_xtoy_sonde_sfc_adj
   use da_mtgirs,    only : da_transform_xtoy_mtgirs, da_transform_xtoy_mtgirs_adj
  use da_tamdar,    only : da_transform_xtoy_tamdar, da_transform_xtoy_tamdar_adj, &
                            da_transform_xtoy_tamdar_sfc, da_transform_xtoy_tamdar_sfc_adj
   use da_ssmi,      only : da_transform_xtoy_ssmt1, da_transform_xtoy_ssmt2, &
      da_transform_xtoy_ssmi_tb, da_transform_xtoy_ssmi_rv, &
      da_transform_xtoy_ssmi_tb_adj, da_transform_xtoy_ssmi_rv_adj, &
      da_transform_xtoy_ssmt1_adj, da_transform_xtoy_ssmt2_adj
   use da_synop,     only : da_transform_xtoy_synop,da_transform_xtoy_synop_adj
   use da_tools_serial,    only : da_free_unit, da_get_unit
   use da_tools,     only : da_add_noise, da_add_noise_new,da_random_omb, &
                            da_geo2msl1, da_msl2geo1
   use da_tracing,   only : da_trace_entry, da_trace_exit 
   use module_dm,    only : wrf_dm_sum_real, wrf_dm_sum_reals

   implicit none

contains

#include "da_obs_proc_station.inc"
#include "da_transform_xtoy.inc"
#include "da_transform_xtoy_adj.inc"
#include "da_add_noise_to_ob.inc"
#include "da_check_missing.inc"
#include "da_fill_obs_structures.inc"
#include "da_fill_obs_structures_radar.inc"
#include "da_fill_obs_structures_rain.inc"
#include "da_random_omb_all.inc"
#include "da_store_obs_grid_info.inc"
#include "da_store_obs_grid_info_rad.inc"
#include "da_count_filtered_obs.inc"
#include "da_obs_sensitivity.inc"
#include "da_set_obs_missing.inc"
#include "da_set_3d_obs_missing.inc"

end module da_obs
