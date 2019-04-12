module da_gpspw

   use module_dm, only : wrf_dm_sum_real
   use module_domain, only : domain

   use da_control, only : obs_qc_pointer,max_ob_levels,missing_r, &
      v_interp_p, v_interp_h, check_max_iv_print,kts,kte, &
      missing, max_error_uv, max_error_t, rootproc, gpspw, &
      max_error_p,max_error_q, check_max_iv_unit,check_max_iv,  &
      max_stheight_diff_ztd,missing_data,max_error_bq,max_error_slp, &
      max_error_bt, max_error_buv, gpspw,max_error_thickness, &
      pseudo_var, num_pseudo, use_gpspwobs, use_gpsztdobs, max_error_pw,fails_error_max, &
      fails_error_max,pseudo_err,pseudo_x, pseudo_y, stdout, &
      pseudo_z,pseudo_val,max_error_ref, trace_use_dull, pseudo, its,ite,jts,jte,&
      ob_vars,qcstat_conv_unit, write_rej_obs_conv
   use da_control, only : pseudo_tpw, pseudo_ztd, myproc, num_fgat_time, write_iv_gpsztd
   use da_define_structures, only : maxmin_type, iv_type, y_type, jo_type, &
      bad_data_type, x_type, number_type, bad_data_type, &
      maxmin_type, da_allocate_observations
   use da_par_util, only : da_proc_stats_combine
   use da_par_util1, only : da_proc_sum_int
   use da_reporting, only : da_error, da_message, message
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_max_error_qc, da_residual,da_get_print_lvl
   use da_tools_serial, only : da_get_unit, da_free_unit
   use da_tracing, only : da_trace_entry, da_trace_exit

   ! The "stats_gpspw_type" is ONLY used locally in da_gpspw:

   type residual_gpspw1_type
      real          :: tpw                      ! Precipitable water
   end type residual_gpspw1_type

   type maxmin_gpspw_stats_type
      type (maxmin_type)         :: tpw
   end type maxmin_gpspw_stats_type

   type stats_gpspw_type
      type (maxmin_gpspw_stats_type)  :: maximum, minimum
      type (residual_gpspw1_type)     :: average, rms_err
   end type stats_gpspw_type

contains

#include "da_ao_stats_gpspw.inc"
#include "da_jo_and_grady_gpspw.inc"
#include "da_residual_gpspw.inc"
#include "da_oi_stats_gpspw.inc"
#include "da_print_stats_gpspw.inc"
#include "da_transform_xtoy_gpspw.inc"
#include "da_transform_xtoy_gpspw_adj.inc"
#include "da_transform_xtoy_gpsztd.inc"
#include "da_transform_xtoy_gpsztd_adj.inc"
#include "da_check_max_iv_gpspw.inc"
#include "da_get_innov_vector_gpspw.inc"
#include "da_get_innov_vector_gpsztd.inc"
#include "da_calculate_grady_gpspw.inc"


end module da_gpspw

