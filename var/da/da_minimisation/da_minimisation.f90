module da_minimisation

   !---------------------------------------------------------------------------
   ! Purpose: Collection of routines associated with minimisation. 
   !---------------------------------------------------------------------------

   use module_configure, only : grid_config_rec_type
   use module_dm, only : wrf_dm_sum_real, wrf_dm_sum_integer
   use module_domain, only : domain, ep_type, vp_type
   use module_state_description, only : dyn_em,dyn_em_tl,dyn_em_ad,p_g_qv

!#ifdef DM_PARALLEL
!   use mpi, only : mpi_barrier
!#endif

   use da_airep, only : da_calculate_grady_airep, da_ao_stats_airep, &
      da_oi_stats_airep, da_get_innov_vector_airep, da_residual_airep, &
      da_jo_and_grady_airep
   use da_airsr , only : da_calculate_grady_airsr, da_ao_stats_airsr, &
      da_oi_stats_airsr, da_get_innov_vector_airsr, da_residual_airsr, &
      da_jo_and_grady_airsr
   use da_bogus, only : da_calculate_grady_bogus, da_ao_stats_bogus, &
      da_oi_stats_bogus, da_get_innov_vector_bogus, da_residual_bogus, &
      da_jo_and_grady_bogus
   use da_buoy , only : da_calculate_grady_buoy, da_ao_stats_buoy, &
      da_oi_stats_buoy,da_get_innov_vector_buoy, da_residual_buoy, &
      da_jo_and_grady_buoy
   use da_control, only : trace_use,var4d_coupling_disk_simul, &
      var4d, rootproc,jcdfi_use,jcdfi_io,var4d_coupling,ierr,comm,num_fgat_time, &
      stdout, eps, stats_unit, test_dm_exact, global, multi_inc, &
      calculate_cg_cost_fn,anal_type_randomcv,cv_size_domain,je_factor, &
      jb_factor,ntmax,omb_add_noise,write_iv_rad_ascii,use_obs_errfac, &
      rtm_option,rtm_option_rttov, rtm_option_crtm, anal_type_verify, &
      write_filtered_rad,omb_set_rand,use_rad,var_scaling2,var_scaling1, &
      var_scaling4,var_scaling5,var_scaling3, jo_unit, &
      print_detail_grad,omb_set_rand,grad_unit,cost_unit, num_pseudo, cv_options, &
      cv_size_domain_je,cv_size_domain_jb, cv_size_domain_jp, cv_size_domain_js, &
      sound, mtgirs, sonde_sfc, synop, profiler, gpsref, gpspw, polaramv, geoamv, ships, metar, &
      satem, radar, ssmi_rv, ssmi_tb, ssmt1, ssmt2, airsr, pilot, airep,tamdar, tamdar_sfc, &
      bogus, buoy, qscat,pseudo, radiance, monitor_on, max_ext_its, use_crtm_kmatrix, &
      precondition_cg, precondition_factor, use_varbc, varbc_factor, &
      num_procs, myproc, use_gpspwobs, use_gpsztdobs, pseudo_var, num_pseudo, &
      num_ob_indexes, num_ob_vars, npres_print, pptop, ppbot, qcstat_conv_unit, &
      orthonorm_gradient, its, ite, jts, jte, kte, ids, ide, jds, jde, &
      use_satcv, sensitivity_option, print_detail_outerloop, adj_sens, filename_len
   use da_define_structures, only : iv_type, y_type, j_type, be_type, &
      xbx_type, jo_type, da_allocate_y,da_zero_x,da_zero_y,da_deallocate_y, &
      da_zero_vp_type
   use da_obs, only : da_transform_xtoy_adj,da_transform_xtoy, &
      da_add_noise_to_ob,da_random_omb_all, da_obs_sensitivity
   use da_geoamv, only : da_calculate_grady_geoamv, da_ao_stats_geoamv, &
      da_oi_stats_geoamv, da_get_innov_vector_geoamv,da_residual_geoamv, &
      da_jo_and_grady_geoamv
   use da_gpspw, only : da_calculate_grady_gpspw, da_ao_stats_gpspw, &
      da_oi_stats_gpspw, da_get_innov_vector_gpspw, da_residual_gpspw, &
      da_jo_and_grady_gpspw, da_get_innov_vector_gpsztd
   use da_gpsref, only : da_calculate_grady_gpsref, da_ao_stats_gpsref, &
      da_oi_stats_gpsref, da_get_innov_vector_gpsref, da_residual_gpsref, &
      da_jo_and_grady_gpsref
   use da_obs_io, only : da_final_write_y, da_write_y, da_final_write_obs, &
      da_write_obs,da_write_obs_etkf,da_write_noise_to_ob, da_use_obs_errfac, &
      da_write_iv_for_multi_inc, da_read_iv_for_multi_inc
   use da_metar, only : da_calculate_grady_metar, da_ao_stats_metar, &
      da_oi_stats_metar, da_get_innov_vector_metar, da_residual_metar, &
      da_jo_and_grady_metar
   use da_pilot, only : da_calculate_grady_pilot, da_ao_stats_pilot, &
      da_oi_stats_pilot, da_get_innov_vector_pilot, da_residual_pilot, &
      da_jo_and_grady_pilot
   use da_par_util, only : da_system,da_cv_to_global
   use da_par_util1, only : da_proc_sum_real,da_proc_sum_ints
   use da_polaramv, only : da_calculate_grady_polaramv, da_ao_stats_polaramv, &
      da_oi_stats_polaramv, da_get_innov_vector_polaramv, da_residual_polaramv, &
      da_jo_and_grady_polaramv
   use da_profiler, only : da_calculate_grady_profiler, da_ao_stats_profiler, &
      da_oi_stats_profiler,da_get_innov_vector_profiler, da_residual_profiler, &
      da_jo_and_grady_profiler
   use da_pseudo, only : da_calculate_grady_pseudo, da_ao_stats_pseudo, &
      da_oi_stats_pseudo, da_get_innov_vector_pseudo, da_residual_pseudo, &
      da_jo_and_grady_pseudo
   use da_qscat, only : da_calculate_grady_qscat, da_ao_stats_qscat, &
      da_oi_stats_qscat, da_get_innov_vector_qscat, da_residual_qscat, &
      da_jo_and_grady_qscat
   use da_mtgirs, only : da_calculate_grady_mtgirs, &
      da_ao_stats_mtgirs, da_oi_stats_mtgirs,da_oi_stats_mtgirs, &
      da_get_innov_vector_mtgirs, &
      da_jo_and_grady_mtgirs, da_residual_mtgirs
   use da_tamdar, only : da_calculate_grady_tamdar, &
      da_ao_stats_tamdar, da_oi_stats_tamdar,da_oi_stats_tamdar, &
      da_get_innov_vector_tamdar, &
      da_jo_and_grady_tamdar, da_residual_tamdar, &
      da_calculate_grady_tamdar_sfc, &
      da_ao_stats_tamdar_sfc, da_oi_stats_tamdar_sfc,da_oi_stats_tamdar_sfc, &
      da_get_innov_vector_tamdar_sfc, &
      da_jo_and_grady_tamdar_sfc, da_residual_tamdar_sfc

#if defined(RTTOV) || defined(CRTM)
   use da_radiance, only : da_calculate_grady_rad, da_write_filtered_rad, &
      da_get_innov_vector_radiance, satinfo
   use da_radiance1, only : da_ao_stats_rad,da_oi_stats_rad, &
      da_write_iv_rad_ascii,da_residual_rad,da_jo_and_grady_rad
#endif
   use da_radar, only :  da_calculate_grady_radar, da_ao_stats_radar, &
      da_oi_stats_radar, da_get_innov_vector_radar, da_residual_radar, &
      da_jo_and_grady_radar
   use da_reporting, only : da_message, da_warning, da_error
   use da_satem, only : da_calculate_grady_satem, da_ao_stats_satem, &
      da_oi_stats_satem, da_get_innov_vector_satem, da_residual_satem, &
      da_jo_and_grady_satem
   use da_ships, only : da_calculate_grady_ships, da_ao_stats_ships, &
      da_oi_stats_ships, da_get_innov_vector_ships, da_residual_ships, &
      da_jo_and_grady_ships
   use da_sound, only : da_calculate_grady_sound,da_calculate_grady_sonde_sfc, &
      da_ao_stats_sound, da_oi_stats_sound,da_oi_stats_sound, &
      da_oi_stats_sonde_sfc,da_ao_stats_sonde_sfc,da_get_innov_vector_sound, &
      da_get_innov_vector_sonde_sfc,da_jo_and_grady_sound, da_residual_sound, &
      da_jo_and_grady_sound,da_jo_and_grady_sonde_sfc,da_residual_sonde_sfc
   use da_ssmi, only : da_calculate_grady_ssmi_tb,da_calculate_grady_ssmi_rv,da_calculate_grady_ssmt1, &
      da_calculate_grady_ssmt2, da_ao_stats_ssmi_tb ,da_ao_stats_ssmt2, &
      da_ao_stats_ssmt2, da_oi_stats_ssmt1, da_oi_stats_ssmt2, &
      da_oi_stats_ssmi_tb,da_oi_stats_ssmi_rv,da_ao_stats_ssmt1,da_get_innov_vector_ssmi_tb, &
      da_get_innov_vector_ssmi_rv, da_residual_ssmi_rv, da_residual_ssmi_tb, &
      da_get_innov_vector_ssmt1,da_get_innov_vector_ssmt2, &
      da_jo_and_grady_ssmt1, da_jo_and_grady_ssmt2,da_jo_and_grady_ssmi_tb, &
      da_jo_and_grady_ssmi_rv, &
      da_residual_ssmt1,da_residual_ssmt2, da_ao_stats_ssmi_rv
   use da_synop, only : da_calculate_grady_synop, da_ao_stats_synop, &
      da_oi_stats_synop, da_get_innov_vector_synop, da_residual_synop, &
      da_jo_and_grady_synop
   use da_statistics, only : da_analysis_stats, da_print_qcstat
   use da_tools_serial, only : da_get_unit,da_free_unit
   use da_tracing, only : da_trace_entry, da_trace_exit,da_trace
   use da_transfer_model, only : da_transfer_wrftltoxa,da_transfer_xatowrftl, &
      da_transfer_xatowrftl_adj,da_setup_firstguess,da_transfer_wrftltoxa_adj
#if defined(RTTOV) || defined(CRTM)
   use da_varbc, only : da_varbc_tl,da_varbc_adj
#endif
   use da_vtox_transforms, only : da_transform_vtox,da_transform_vtox_adj,da_transform_xtoxa,da_transform_xtoxa_adj
   use da_wrf_interfaces, only : wrf_dm_bcast_real, wrf_get_dm_communicator
   use da_wrfvar_io, only : da_med_initialdata_input
   use module_symbols_util, only : wrfu_finalize
   use da_lapack, only : dsteqr

   implicit none

   private :: da_dot, da_dot_cv

contains
      
#include "da_calculate_j.inc"
#include "da_calculate_gradj.inc"
#include "da_jo_and_grady.inc"
#include "da_calculate_residual.inc"
#include "da_get_var_diagnostics.inc"
#include "da_get_innov_vector.inc"
#include "da_dot.inc"
#include "da_dot_cv.inc"
#include "da_write_diagnostics.inc"
#include "da_minimise_cg.inc"
#include "da_minimise_lz.inc"
#include "da_calculate_grady.inc"
#include "da_transform_vtoy.inc"
#include "da_transform_vtoy_adj.inc"
#include "da_adjoint_sensitivity.inc"
#include "da_sensitivity.inc"
end module da_minimisation
