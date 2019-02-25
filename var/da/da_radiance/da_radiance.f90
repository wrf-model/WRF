module da_radiance

   !---------------------------------------------------------------------------
   ! Purpose: module for radiance data assimilation. 
   !---------------------------------------------------------------------------

#if defined(HDF5)
   use hdf5
#endif

#if defined(RTTOV) || defined(CRTM)

   use module_domain, only : xb_type, domain
   use module_radiance, only : satinfo, &
      i_kind,r_kind, r_double, &
       one, zero, three,deg2rad,rad2deg, &
      q2ppmv, &
      init_constants_derived, &
      rttov_platform_name, rttov_inst_name, crtm_sensor_name  ! names used by both RTTOV and CRTM
#ifdef RTTOV
   use module_radiance, only : coefs, rttov_coefs, rttov_profile, rttov_radiance, &
      rttov_transmission,errorstatus_success,gas_id_watervapour,rttov_emissivity
#endif
#ifdef CRTM
   use module_radiance, only : crtm_channelinfo_type, crtm_platform_name, crtm_init, &
   CRTM_Planck_Radiance, CRTM_Planck_Temperature   
#endif

#ifdef DM_PARALLEL
!  use mpi, only : mpi_integer, mpi_status_size, mpi_min, mpi_max, mpi_minloc, &
!      mpi_2double_precision
#endif

   use da_control, only : max_ob_levels,missing_r, &
      v_interp_p, v_interp_h, trace_use_dull, &
      missing, max_error_uv, max_error_t, rootproc, &
      max_error_p,max_error_q, radiance, &
      max_stheight_diff,missing_data,max_error_bq,max_error_slp, &
      max_error_bt, max_error_buv, rtminit_platform,rtminit_satid, &
      rtminit_nsensor,rtminit_sensor,filename_len,read_biascoef,analysis_date, &
      time_window_max,time_window_min,print_detail_obs,use_hsbobs,use_msuobs, &
      use_amsubobs,use_eos_amsuaobs,use_amsuaobs,use_hirs2obs,rtm_option, &
      rtm_option_rttov,rtm_option_crtm,use_airsobs,use_kma1dvar,use_hirs3obs, &
      use_ssmisobs,use_iasiobs,use_seviriobs,use_filtered_rad,print_detail_rad,stderr, mw_emis_sea, &
      rtminit_print, rttov_scatt,comm,root,ierr,biasprep, qc_rad, num_procs, &
      tovs_min_transfer,use_error_factor_rad,num_fgat_time,stdout,trace_use, &
      qc_good, qc_bad,myproc,biascorr,thinning,thinning_mesh, &
      rad_monitoring, monitor_on, kts, kte, kms, kme, calc_weightfunc, &
      use_mwtsobs, use_mwhsobs, use_mwhs2obs, use_atmsobs, use_amsr2obs, use_ahiobs, &
      use_hirs4obs, use_mhsobs,bufr_year, bufr_month,bufr_day,bufr_hour, &
      bufr_minute, bufr_second,bufr_solzen, bufr_station_height, &
      bufr_landsea_mask,bufr_solazi,tovs_end, max_tovs_input, bufr_satzen, nchan_mhs, &
      nchan_msu, nchan_amsua,nchan_hirs2, nchan_hirs3, nchan_hirs4, nchan_airs, &
      bufr_lon, bufr_satellite_id, bufr_ifov,nchan_amsub, tovs_start, bufr_lat, &
      use_pseudo_rad, pseudo_rad_platid,pseudo_rad_satid, pseudo_rad_senid, &
      pseudo_rad_ichan, pseudo_rad_inv, pseudo_rad_lat,pseudo_rad_lon, &
      pseudo_rad_err, use_simulated_rad,use_rttov_kmatrix, use_crtm_kmatrix , &
      use_rad,crtm_cloud, DT_cloud_model, global, use_varbc, freeze_varbc, &
      airs_warmest_fov, time_slots, interp_option, ids, ide, jds, jde, &
      ips, ipe, jps, jpe, simulated_rad_ngrid, obs_qc_pointer, use_blacklist_rad, use_satcv, &
      use_goesimgobs, pi, earth_radius, satellite_height
 
#ifdef CRTM
   use da_crtm, only : da_crtm_init, da_get_innov_vector_crtm
#endif
   use da_define_structures, only : maxmin_type, iv_type, y_type, jo_type, j_type, &
      bad_data_type, x_type, number_type, bad_data_type, &
      airsr_type,info_type, model_loc_type, varbc_info_type, varbc_type
   use da_interpolation, only : da_to_zk, da_to_zk_new
   use da_tools_serial, only : da_get_unit, da_free_unit
   use da_par_util1, only : da_proc_sum_int,da_proc_sum_ints
#ifdef DM_PARALLEL
   use da_par_util, only :  da_proc_stats_combine, true_mpi_real
#else
   use da_par_util, only :  da_proc_stats_combine
#endif
   use da_physics, only : da_sfc_pre, da_transform_xtopsfc, &
      da_transform_xtopsfc_adj,da_tpq_to_slp_lin,da_tpq_to_slp_adj
   use da_radiance1, only : num_tovs_before,num_tovs_after,tovs_copy_count, &
      tovs_send_pe, tovs_recv_pe, tovs_send_start, tovs_send_count, &
      tovs_recv_start,con_vars_type,aux_vars_type, datalink_type,da_qc_amsub, &
      da_qc_amsua,da_biascorr, da_detsurtyp,da_biasprep, &
      da_qc_rad, da_cld_eff_radius, da_read_biascoef
   use da_reporting, only : da_message, da_warning, message, da_error
#ifdef RTTOV
   use da_rttov, only : da_rttov_init, da_get_innov_vector_rttov 
#endif
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_residual, da_obs_sfc_correction, &
      da_llxy, da_llxy_new, da_togrid_new, da_get_julian_time, da_get_time_slots, &
      da_xyll, map_info
   use da_tracing, only : da_trace_entry, da_trace_exit, da_trace, &
      da_trace_int_sort
   use da_varbc, only : da_varbc_direct,da_varbc_coldstart,da_varbc_precond, &
      da_varbc_pred
   use da_wrf_interfaces, only : wrf_dm_bcast_integer
   use gsi_thinning, only : r999,r360,rlat_min,rlat_max,rlon_min,rlon_max, &
                            dlat_grid,dlon_grid,thinning_grid, &
                            makegrids,map2grids, &
                            destroygrids
                            
   implicit none

   include 'netcdf.inc'

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif
   
contains

#include "da_calculate_grady_rad.inc"
#include "da_read_filtered_rad.inc"
#include "da_read_simulated_rad.inc"
#include "da_write_filtered_rad.inc"
#include "da_read_obs_bufrtovs.inc"
#include "da_read_obs_fy3.inc"
#include "da_read_obs_hdf5mwhs2.inc"
#include "da_read_obs_bufratms.inc"
#include "ATMS_Spatial_Average.inc"
#include "da_read_obs_bufrairs.inc"
#include "da_read_obs_bufrssmis.inc"
#include "da_read_obs_bufriasi.inc"
#include "da_read_obs_bufrseviri.inc"
#include "da_read_obs_hdf5amsr2.inc"
#include "da_read_obs_hdf5ahi.inc"
#include "da_read_obs_netcdf4ahi_geocat.inc"
#include "da_read_obs_netcdf4ahi_jaxa.inc"
#include "da_read_obs_ncgoesimg.inc"
#include "da_get_satzen.inc"
#include "da_allocate_rad_iv.inc"
#include "da_initialize_rad_iv.inc"
#include "da_read_kma1dvar.inc"
#include "da_sort_rad.inc"
#include "da_setup_radiance_structures.inc"
#include "da_radiance_init.inc"
#include "da_get_innov_vector_radiance.inc"
#include "da_read_pseudo_rad.inc"
#include "da_blacklist_rad.inc"
#include "da_deallocate_radiance.inc"

#endif

end module da_radiance

