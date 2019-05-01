module da_obs_io

   use module_domain, only : domain

   use da_control, only : xmiss, missing_r, fmt_each, fmt_info, trace_use, &
      fmt_srfc, filtered_obs_unit, num_procs,missing, ierr,comm, rand_unit, &
      obs_qc_pointer, rootproc, omb_unit,omb_add_noise,use_airepobs, &
      use_airepobs,use_bogusobs,use_gpspwobs,use_gpsztdobs,use_gpsrefobs,use_geoamvobs, &
      use_metarobs,use_profilerobs,use_pilotobs,use_buoyobs,use_shipsobs,use_rainobs, &
      use_synopobs,use_soundobs,use_mtgirsobs,use_tamdarobs,use_qscatobs,use_radarobs, &
      test_transforms, use_ssmiretrievalobs, report_start, &
      report_end, global, print_detail_obs, stdout, t_kelvin, stderr, &
      max_ob_levels, missing_data, max_bogus_input, myproc, convert_uv2fd, convert_fd2uv, &
      fails_error_max,standard_atmosphere,zero_t_td,print_detail_f_obs, &
      print_detail_radar,use_satemobs,use_polaramvobs,use_ssmt1obs, &
      use_ssmt2obs, use_airsretobs,convert_fd2uv,anal_type_qcobs,gravity,gas_constant,cp, &
      filename_len, t0, max_airep_input, max_bogus_input, max_ssmi_rv_input, &
      max_buoy_input, max_gpsref_input, max_gpspw_input, max_geoamv_input, &
      max_airsr_input, max_polaramv_input, max_radar_input, &
      max_profiler_input, max_sound_input, max_mtgirs_input, max_tamdar_input, max_ships_input, &
      max_satem_input,max_pilot_input, max_metar_input, max_ssmt1_input, &
      max_synop_input,max_ssmt2_input,  max_qscat_input, &
      obs_names, num_ob_indexes, fm_index, ids,ide, ite, jte, &
      sound, mtgirs,synop, pilot, satem, geoamv, polaramv, airep, gpspw, gpsref, &
      tamdar, tamdar_sfc, metar, ships, ssmi_rv, ssmi_tb, ssmt1, ssmt2, qscat, profiler, buoy, bogus, pseudo, &
      radar, radiance, airsr, sonde_sfc, trace_use_dull, num_fgat_time, time_slots, myproc, &
      qmarker_retain, anal_type_verify, top_km_gpsro, bot_km_gpsro, thin_rainobs, &
      sfc_assi_options, sfc_assi_options_1, sfc_assi_options_2,print_detail_rain,max_rain_input,rain, &
      pi, ob_format_gpsro, ob_format_ascii, analysis_date, kms,kme, v_interp_h,v_interp_p, &
      wind_sd,wind_sd_synop,wind_sd_tamdar,wind_sd_mtgirs,wind_sd_profiler,wind_sd_geoamv,wind_sd_polaramv, &
      wind_sd_airep,wind_sd_sound,wind_sd_metar,wind_sd_ships,wind_sd_qscat,wind_sd_buoy,wind_sd_pilot,wind_stats_sd,&
      thin_conv, thin_conv_ascii, lsac_nh_step, lsac_nv_step, lsac_nv_start, lsac_print_details, &
      lsac_use_u, lsac_use_v, lsac_use_t, lsac_use_q, lsac_u_error, lsac_v_error, lsac_t_error, lsac_q_error, &
      gpsro_drift, max_gpseph_input, use_gpsephobs, gpseph, gpseph_loadbalance, kds, kde, kts, kte, &
      use_radar_rhv, use_radar_rqv

   use da_define_structures, only : iv_type, multi_level_type, multi_level_type_BUFR, &
      radar_multi_level_type, y_type, field_type, each_level_type, &
      radar_each_level_type, info_type, model_loc_type,gpsref_type, rain_single_level_type, rain_each_type, &
      gpseph_type
   use da_grid_definitions, only : da_ffdduv,da_ffdduv_model,da_ffdduv_diagnose
   use da_obs, only : da_count_filtered_obs,da_check_missing,da_obs_proc_station, da_set_obs_missing, da_set_3d_obs_missing
   use da_par_util1, only : da_proc_sum_int
   use da_physics, only : da_tp_to_qs
   use da_reporting, only : da_warning, message, da_error
   use da_tools, only : da_llxy, da_get_julian_time, da_geo2msl1, da_msl2geo1
   use da_tools_serial, only : da_free_unit, da_get_unit, da_advance_time
   use da_tracing, only : da_trace_entry, da_trace_exit

   use module_radiance, only : deg2rad, i_kind
   use gsi_thinning, only : map2grids, map2grids_conv, cleangrids_conv, thinning_grid, &
                            map2tgrid, thinning_grid_conv
#ifdef DM_PARALLEL
   use da_control, only : root
!  use mpi, only : mpi_min
   use da_par_util, only : true_mpi_real
#endif
#ifdef BUFR
   use da_grid_definitions, only : da_earth_2_model_wind
#endif
   use da_reporting, only : message, da_message
   use da_interpolation, only : da_to_zk
   use da_netcdf_interface, only : da_get_var_3d_real_cdf, da_get_dims_cdf, &
      da_get_var_2d_real_cdf
   use da_gpseph, only: da_gpseph_create_ob, ob_in_mean_h, global_h_mean

   implicit none

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

contains

#include "da_read_obs_ascii.inc"
#include "da_scan_obs_ascii.inc"
#include "da_read_obs_radar.inc"
#include "da_scan_obs_radar.inc"
#include "da_scan_obs_rain.inc" 
#include "da_read_obs_rain.inc"
#include "da_read_errfac.inc"
#include "da_use_obs_errfac.inc"
#include "da_write_obs.inc"
#include "da_write_iv_for_multi_inc.inc"
#include "da_read_iv_for_multi_inc.inc"
#include "da_search_obs.inc"
#include "da_write_obs_etkf.inc"
#include "da_write_filtered_obs.inc"
#include "da_write_modified_filtered_obs.inc"
#include "da_write_y.inc"
#include "da_read_obs_bufr.inc"
#include "da_read_obs_bufrgpsro.inc"
#include "da_read_obs_bufrgpsro_eph.inc"
#include "da_final_write_obs.inc"
#include "da_final_write_y.inc"
#include "da_read_y_unit.inc"
#include "da_read_rand_unit.inc"
#include "da_read_omb_tmp.inc"
#include "da_write_noise_to_ob.inc"
#include "da_final_write_filtered_obs.inc"
#include "da_final_write_modified_filtered_obs.inc"
#include "da_read_lsac_util.inc"
#include "da_read_obs_lsac.inc"
#include "da_scan_obs_lsac.inc"

end module da_obs_io
