module da_obs_io

   use module_domain, only : domain

   use da_control, only : missing_r, fmt_each, fmt_info, trace_use, &
      fmt_srfc, filtered_obs_unit, num_procs,missing, ierr,comm, rand_unit, &
      obs_qc_pointer, rootproc, omb_unit,omb_add_noise,use_airepobs, &
      use_airepobs,use_bogusobs,use_gpspwobs,use_gpsztdobs,use_gpsrefobs,use_geoamvobs, &
      use_metarobs,use_profilerobs,use_pilotobs,use_buoyobs,use_shipsobs, &
      use_synopobs,use_soundobs,use_mtgirsobs,use_tamdarobs,use_qscatobs,test_transforms, use_ssmiretrievalobs, report_start, &
      report_end, global, print_detail_obs, stdout, t_kelvin, stderr, &
      max_ob_levels, missing_data, max_bogus_input, myproc,convert_uv2fd, &
      fails_error_max,standard_atmosphere,zero_t_td,print_detail_f_obs, &
      print_detail_radar,use_satemobs,use_polaramvobs,use_ssmt1obs, &
      use_ssmt2obs, use_airsretobs,convert_fd2uv,anal_type_qcobs,gravity, &
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
      qmarker_retain, anal_type_verify

   use da_define_structures, only : iv_type, multi_level_type, multi_level_type_BUFR, &
      radar_multi_level_type, y_type, field_type, each_level_type, &
      radar_each_level_type, info_type, model_loc_type,gpsref_type
   use da_grid_definitions, only : da_ffdduv
   use da_obs, only : da_count_filtered_obs,da_check_missing,da_obs_proc_station
   use da_par_util1, only : da_proc_sum_int
   use da_physics, only : da_tp_to_qs
   use da_reporting, only : da_warning, message, da_error
   use da_tools, only : da_llxy, da_get_julian_time
   use da_tools_serial, only : da_free_unit, da_get_unit, da_advance_time
   use da_tracing, only : da_trace_entry, da_trace_exit

#ifdef BUFR
   use da_control, only : thin_conv
   use da_grid_definitions, only : da_earth_2_model_wind
   use module_radiance, only : deg2rad, i_kind
   use gsi_thinning, only : map2grids, map2grids_conv, cleangrids_conv, thinning_grid_conv
   use da_obs, only : da_set_obs_missing
#ifdef DM_PARALLEL
   use da_control, only : root
!  use mpi, only : mpi_min
   use da_par_util, only : true_mpi_real
#endif
#endif
   use da_reporting, only : message, da_message

   implicit none

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

contains

#include "da_read_obs_ascii.inc"
#include "da_scan_obs_ascii.inc"
#include "da_read_obs_radar.inc"
#include "da_scan_obs_radar.inc"
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
#include "da_final_write_obs.inc"
#include "da_final_write_y.inc"
#include "da_read_y_unit.inc"
#include "da_read_rand_unit.inc"
#include "da_read_omb_tmp.inc"
#include "da_write_noise_to_ob.inc"
#include "da_final_write_filtered_obs.inc"
#include "da_final_write_modified_filtered_obs.inc"

end module da_obs_io
