module da_satem


   use module_domain, only : domain
   
   use da_control, only : obs_qc_pointer,max_ob_levels,missing_r, &
      v_interp_p, v_interp_h, check_max_iv_print, trace_use_dull, &
      missing, max_error_uv, max_error_t, rootproc, kts,kte, fails_error_max, &
      max_error_p,max_error_q, check_max_iv_unit,check_max_iv,  &
      max_stheight_diff,missing_data,max_error_bq,max_error_slp, &
      max_error_bt, max_error_buv, satem,max_error_thickness, above_model_lid,&
      ob_vars,qcstat_conv_unit

   use da_define_structures, only : maxmin_type, iv_type, y_type, jo_type, &
      bad_data_type, x_type, number_type, bad_data_type, &
      maxmin_type
   use da_par_util, only : da_proc_stats_combine
   use da_par_util1, only : da_proc_sum_int
   use da_physics, only : da_tv_profile, da_thickness, da_find_layer, da_thickness_adj, &
      da_find_layer_adj, da_tv_profile_adj, da_find_layer_tl, &
      da_thickness_tl, da_tv_profile_tl
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_max_error_qc, da_residual,da_get_print_lvl
   use da_tracing, only : da_trace_entry, da_trace_exit

   ! The "stats_satem_type" is ONLY used locally in da_satem:

   type residual_satem1_type
      real          :: thickness                ! satem thickness
   end type residual_satem1_type

   type maxmin_satem_stats_type
      type (maxmin_type)         :: thickness
   end type maxmin_satem_stats_type

   type stats_satem_type
      type (maxmin_satem_stats_type)  :: maximum, minimum
      type (residual_satem1_type)     :: average, rms_err
   end type stats_satem_type


contains

#include "da_ao_stats_satem.inc"
#include "da_jo_and_grady_satem.inc"
#include "da_residual_satem.inc"
#include "da_oi_stats_satem.inc"
#include "da_print_stats_satem.inc"
#include "da_transform_xtoy_satem.inc"
#include "da_transform_xtoy_satem_adj.inc"
#include "da_check_max_iv_satem.inc"
#include "da_get_innov_vector_satem.inc"
#include "da_calculate_grady_satem.inc"

end module da_satem

