module da_radar

   use module_domain, only : domain

   use da_control, only : obs_qc_pointer,max_ob_levels,missing_r, &
      v_interp_p, v_interp_h, check_max_iv_print, trace_use, &
      missing, max_error_uv, max_error_t, rootproc, &
      max_error_p,max_error_q, check_max_iv_unit,check_max_iv,  &
      max_stheight_diff,missing_data,max_error_bq,max_error_slp, &
      max_error_bt, max_error_buv, radar,fails_error_max, &
      use_radar_rv, use_radar_rf,radar_rf_opt,radar_rf_rscl,radar_rv_rscl,rf_noice,rfmin, rf_qthres, use_radar_rhv, use_radar_rqv, &
      below_model_surface,mkz,above_model_lid,&
      fg_format,fg_format_wrf_arw_regional,fg_format_wrf_nmm_regional,fg_format_wrf_arw_global,&
      fg_format_kma_global,max_error_rv,max_error_rf, &
      far_below_model_surface,kms,kme,kts,kte, trace_use_dull,filename_len,&
      myproc, analysis_date, num_procs , ierr, comm, es_beta, es_gamma, a_ew
   use da_control, only : its, ite, jts, jte, ids, ide, jds, jde, ims, ime, jms, jme
   use da_control, only : cloudbase_calc_opt, &
      radar_non_precip_rf, radar_non_precip_opt, radar_rqv_thresh1, radar_rqv_thresh2, &
      radar_rqv_rh1, radar_rqv_rh2, radar_non_precip_rh_w, radar_non_precip_rh_i, &
      radar_rqv_h_lbound, radar_rqv_h_ubound, radar_saturated_rf, cloud_cv_options, &
      radar_rhv_err_opt, radar_rhv_rrn_err, radar_rhv_rsn_err, radar_rhv_rgr_err
   use da_define_structures, only : maxmin_type, iv_type, y_type, jo_type, &
      bad_data_type, x_type, number_type, bad_data_type, &
      infa_type, field_type
   use da_interpolation, only : da_to_zk, da_interp_lin_3d,da_interp_lin_3d_adj
   use da_par_util, only :da_proc_stats_combine, da_patch_to_global
   use da_par_util1, only : da_proc_sum_int
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_residual, map_info, da_llxy_wrf, da_llxy_default, da_convert_zk
   use da_tracing, only : da_trace_entry, da_trace_exit
   use da_reporting, only : da_error, da_warning, da_message, message
   use da_tools_serial, only : da_get_unit, da_free_unit

   ! The "stats_radar_type" is ONLY used locally in da_radar:

   type residual_radar1_type
      real                    :: rv
      real                    :: rf
      real                    :: rrn
      real                    :: rsn
      real                    :: rgr
      real                    :: rqv
   end type residual_radar1_type

   type maxmin_radar_stats_type
      type (maxmin_type)         :: rv       ! Radial velocity
      type (maxmin_type)         :: rf       ! Reflectivity
      type (maxmin_type)         :: rrn
      type (maxmin_type)         :: rsn
      type (maxmin_type)         :: rgr
      type (maxmin_type)         :: rqv
   end type maxmin_radar_stats_type

   type stats_radar_type
      type (maxmin_radar_stats_type)  :: maximum, minimum
      type (residual_radar1_type)     :: average, rms_err
   end type stats_radar_type

   real, parameter :: leh1=43.1
   real, parameter :: leh2=17.5

   real :: zlcl_mean  !model grid mean LCL

contains

#include "da_ao_stats_radar.inc"
#include "da_jo_and_grady_radar.inc"
#include "da_residual_radar.inc"
#include "da_oi_stats_radar.inc"
#include "da_print_stats_radar.inc"
#include "da_transform_xtoy_radar.inc"
#include "da_transform_xtoy_radar_adj.inc"
#include "da_check_max_iv_radar.inc"
#include "da_get_innov_vector_radar.inc"
#include "da_radial_velocity.inc"
#include "da_radial_velocity_lin.inc"
#include "da_radial_velocity_adj.inc"
#include "da_calculate_grady_radar.inc"
#include "da_max_error_qc_radar.inc"
#include "da_write_oa_radar_ascii.inc"
#include "da_radar_rf.inc"

#include "da_radzicevar_calc_ice_abc.inc"
#include "da_radzicevar_pkx.inc"
#include "da_radzicevar_upper_f.inc"
#include "da_radzicevar_cal_tl_fw4wetice.inc"
#include "da_radzicevar_prepare_interceptpara.inc"
#include "da_radzicevar_virtual.inc"
#include "da_radzicevar_parameter_zrx.inc"
#include "da_radzicevar_prepare_mixingratios.inc"
#include "da_radzicevar_waterfraction.inc"
#include "da_radzicevar_parameter_zxx.inc"
#include "da_radzicevar_prepare_zmm_adj.inc"
#include "da_radzicevar_dryice_adj.inc"
#include "da_radzicevar_pxabk.inc"
#include "da_radzicevar_wetice_adj.inc"
#include "da_radzicevar_dryice_tl.inc"
#include "da_radzicevar_rain_adj.inc"
#include "da_radzicevar_wetice_tl.inc"
#include "da_radzicevar_rain_tl.inc"
#include "da_radzicevar_rhoair_tl.inc"
#include "da_radzicevar.inc"
#include "da_radzicevar_tl.inc"
#include "da_radzicevar_adj.inc"
#include "da_radzicevar_sigma_in_abc.inc"

end module da_radar

