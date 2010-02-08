module da_setup_structures

   !---------------------------------------------------------------------------
   ! Purpose: Sets up various structures.
   !---------------------------------------------------------------------------

   use module_domain, only : xb_type, ep_type, domain

   use da_define_structures, only : xbx_type,be_subtype, be_type, y_type, j_type, &
      iv_type,da_allocate_background_errors,da_allocate_observations, &
      multi_level_type,each_level_type
   use da_wrf_interfaces, only : wrf_debug
   use da_control, only : trace_use,vert_evalue,stdout,rootproc, &
      analysis_date,coarse_ix,coarse_ds,map_projection,coarse_jy, c2,dsm,phic, &
      pole, cone_factor, start_x,base_pres,ptop,psi1,start_y, base_lapse,base_temp,truelat2_3dv, &
      truelat1_3dv,xlonc,t0,num_fft_factors,pi,print_detail_spectral, global, print_detail_obs, &
      use_radar_rf, num_ob_indexes,kts, kte, time_window_max, time_window_min, &
      max_fgat_time, num_fgat_time, dt_cloud_model, &
      use_ssmiretrievalobs,use_radarobs,use_ssmitbobs,use_qscatobs, num_procs, &
      num_pseudo, missing, ob_format, ob_format_bufr,ob_format_ascii, ob_format_madis, &
      use_airepobs, use_tamdarobs, test_dm_exact, use_amsuaobs, use_amsubobs, &
      use_airsobs, use_bogusobs, sfc_assi_options, use_eos_amsuaobs, &
      use_filtered_rad, use_gpsrefobs, use_hirs2obs, &
      use_hsbobs,use_hirs3obs, use_gpspwobs, use_gpsztdobs, use_metarobs, use_msuobs, &
      use_kma1dvar,use_pilotobs, use_polaramvobs, use_rad, crtm_cloud, use_soundobs,use_mtgirsobs, &
      use_ssmt1obs,use_ssmt2obs, use_shipsobs, use_satemobs, use_synopobs, &
      use_radar_rv,use_profilerobs, use_obsgts, use_geoamvobs, use_buoyobs, &
      jb_factor, je_factor, alphacv_method,its,ite,jts,jte,cv_size_domain_jb, &
      cv_size_domain_je, cv_size_domain,ensdim_alpha, alpha_vertloc, alpha_hydrometeors, &
      lat_stats_option,alpha_std_dev,sigma_alpha,alpha_corr_scale,len_scaling1, &
      len_scaling2,len_scaling3,len_scaling4,len_scaling5,max_vert_var1, &
      max_vert_var2,max_vert_var3,max_vert_var4,max_vert_var_alpha,print_detail_be, &
      test_statistics, var_scaling1,var_scaling2,var_scaling3,var_scaling4, &
      var_scaling5,vert_corr,max_vert_var5,power_truncation,alpha_truncation, &
      print_detail_regression,gas_constant, use_airsretobs, &
      filename_len, use_ssmisobs, gravity, t_triple, use_hirs4obs, use_mhsobs, &
      vert_corr_2, alphacv_method_xa, vert_evalue_global, &
      vert_evalue_local, obs_names, num_ob_indexes, &
      sound, mtgirs,tamdar, synop, profiler, gpsref, gpspw, polaramv, geoamv, ships, metar, &
      satem, radar, ssmi_rv, ssmi_tb, ssmt1, ssmt2, airsr, pilot, airep, &
      bogus, buoy, qscat, radiance, pseudo, trace_use_dull, kts,kte, &
      use_simulated_rad, use_pseudo_rad, pseudo_rad_platid, pseudo_rad_satid, &
      pseudo_rad_senid, rtminit_nsensor, rtminit_platform, rtminit_satid, &
      rtminit_sensor, thinning, qc_rad,& 
      num_pseudo,pseudo_x, pseudo_y, pseudo_z, pseudo_var,pseudo_val, pseudo_err,&
      fg_format, fg_format_wrf_arw_regional,fg_format_wrf_nmm_regional, &
      fg_format_wrf_arw_global, fg_format_kma_global, deg_to_rad, rad_to_deg, &
      sonde_sfc, missing_data, missing_r, qc_good, thin_mesh_conv, time_slots, &
      cv_options, cv_size, as1, as2, as3, as4, as5, &
      ids,ide,jds,jde,kds,kde, ims,ime,jms,jme,kms,kme, &
      its,ite,jts,jte,kts,kte, ips,ipe,jps,jpe,kps,kpe, root, comm, ierr, &
      fmt_info, fmt_srfc, fmt_each, unit_end, max_ext_its  

   use da_obs, only : da_fill_obs_structures, da_store_obs_grid_info, da_store_obs_grid_info_bufr
   use da_obs_io, only : da_read_obs_bufr,da_read_obs_radar, &
      da_scan_obs_radar,da_scan_obs_ascii,da_read_obs_ascii, &
      da_read_obs_bufrgpsro
   use da_par_util, only : da_patch_to_global
#if defined(RTTOV) || defined(CRTM)
   use da_radiance, only : da_setup_radiance_structures
#endif
   use da_reporting, only : da_error,message, da_warning, da_message
   use da_recursive_filter, only : da_calculate_rf_factors
   use da_spectral, only : da_initialize_h,da_calc_power_spectrum
   use da_ssmi, only : da_read_obs_ssmi,da_scan_obs_ssmi
   use da_tools_serial, only : da_get_unit, da_free_unit, da_array_print, da_find_fft_factors, &
      da_find_fft_trig_funcs
   use da_tools, only: da_get_time_slots
   use da_tracing, only : da_trace_entry, da_trace_exit
   use da_vtox_transforms, only : da_check_eof_decomposition
   use da_rfz_cv3, only : da_rfz0
   use da_rf_cv3, only : RFDPAR1, RFDPAR2, RFDPARV
#ifdef BUFR
   use da_control, only : thin_conv
   use module_radiance, only : init_constants_derived
   use gsi_thinning, only : r999,r360,rlat_min,rlat_max,rlon_min,rlon_max, &
                            dlat_grid,dlon_grid,thinning_grid_conv, &
                            make3grids, destroygrids_conv
#ifdef DM_PARALLEL
!  use mpi, only : mpi_min, mpi_max
   use da_par_util, only : true_mpi_real
#endif
#endif

   implicit none

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

contains

#include "da_get_vertical_truncation.inc"
#include "da_interpolate_regcoeff.inc"
#include "da_rescale_background_errors.inc"
#include "da_scale_background_errors.inc"
#include "da_setup_background_errors.inc"
#include "da_setup_be_global.inc"
#include "da_setup_be_ncep_gfs.inc"
#include "da_setup_be_regional.inc"
#include "da_setup_be_nmm_regional.inc"
#include "da_setup_cv.inc"
#include "da_chgvres.inc"
#include "da_setup_flow_predictors.inc"
#include "da_setup_obs_structures.inc"
#include "da_setup_obs_structures_ascii.inc"
#include "da_setup_obs_structures_bufr.inc"
#include "da_setup_obs_structures_madis.inc"
#include "da_setup_obs_interp_wts.inc"
#include "da_setup_runconstants.inc"
#include "da_cloud_model.inc"
#include "da_lcl.inc"
#include "da_cumulus.inc"
#include "da_qfrmrh.inc"
#include "da_write_increments.inc"
#include "da_write_increments_for_wrf_nmm_regional.inc"
#include "da_write_kma_increments.inc"
#include "da_get_bins_info.inc"
#include "da_truncate_spectra.inc"

end module da_setup_structures
