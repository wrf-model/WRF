module da_geoamv

   use module_domain, only : domain
   
   use da_control, only : obs_qc_pointer,max_ob_levels,missing_r, &
      v_interp_p, v_interp_h, check_max_iv_print, &
      missing, max_error_uv, max_error_t, rootproc, kms,kme,kts,kte, &
      max_error_p,max_error_q, check_max_iv_unit,check_max_iv, trace_use_dull, &
      max_stheight_diff,missing_data,max_error_bq,max_error_slp,fails_error_max, &
      max_error_bt, max_error_buv, geoamv, anal_type_verify,qcstat_conv_unit,ob_vars, &
      convert_fd2uv, convert_uv2fd, max_error_spd, max_error_dir, max_omb_spd, max_omb_dir, pi, qc_rej_both,&
      wind_sd_geoamv, wind_stats_sd
   use da_grid_definitions, only : da_ffdduv, da_ffdduv_model,da_ffdduv_diagnose
   use da_physics, only : da_uv_to_sd_lin, da_uv_to_sd_adj
   use da_define_structures, only : maxmin_type, iv_type, y_type, jo_type, &
      bad_data_type, x_type, number_type, bad_data_type, &
      maxmin_type
  use da_interpolation, only : da_interp_lin_3d, da_to_zk, &
      da_interp_lin_3d_adj
   use da_par_util, only : da_proc_stats_combine
   use da_par_util1, only : da_proc_sum_int
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_max_error_qc, da_residual, da_convert_zk, da_get_print_lvl
   use da_tracing, only : da_trace_entry, da_trace_exit

   ! The "stats_geoamv_type" is ONLY used locally in da_geoamv:

   type residual_geoamv1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
   end type residual_geoamv1_type

   type maxmin_geoamv_stats_type
      type (maxmin_type)         :: u, v, t, q
   end type maxmin_geoamv_stats_type

   type stats_geoamv_type
      type (maxmin_geoamv_stats_type)  :: maximum, minimum
      type (residual_geoamv1_type)     :: average, rms_err
   end type stats_geoamv_type

contains

#include "da_ao_stats_geoamv.inc"
#include "da_jo_and_grady_geoamv.inc"
#include "da_residual_geoamv.inc"
#include "da_oi_stats_geoamv.inc"
#include "da_print_stats_geoamv.inc"
#include "da_transform_xtoy_geoamv.inc"
#include "da_transform_xtoy_geoamv_adj.inc"
#include "da_check_max_iv_geoamv.inc"
#include "da_get_innov_vector_geoamv.inc"
#include "da_calculate_grady_geoamv.inc"

end module da_geoamv     

