module da_airsr

   use module_domain, only : domain
   
   use da_control, only : obs_qc_pointer,max_ob_levels,missing_r, &
      check_max_iv_print, check_max_iv_unit, v_interp_p, v_interp_h, &
      check_max_iv, missing, max_error_uv, max_error_t, rootproc, &
      airsr, max_error_p,max_error_q, trace_use_dull,fails_error_max, &
      max_stheight_diff,missing_data,max_error_bq,max_error_slp, &
      max_error_bt, max_error_buv, anal_type_verify, kms,kme,kts,kte, &
      ob_vars, qcstat_conv_unit, fails_error_max
   use da_define_structures, only : maxmin_type, iv_type, y_type, jo_type, &
      bad_data_type, x_type, number_type, bad_data_type
   use da_interpolation, only : da_interp_lin_3d, da_to_zk, &
      da_interp_lin_3d_adj
   use da_par_util1, only : da_proc_sum_int
   use da_par_util, only : da_proc_stats_combine
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_max_error_qc, da_residual, da_convert_zk, da_get_print_lvl
   use da_tracing, only : da_trace_entry, da_trace_exit

   ! The "stats_airsr_type" is ONLY used locally in da_airsr:

   type residual_airsr1_type
      real          :: t                        ! temperature
      real          :: q                        ! specific humidity
   end type residual_airsr1_type

   type maxmin_airsr_stats_type
      type (maxmin_type)         :: t, q
   end type maxmin_airsr_stats_type

   type stats_airsr_type
      type (maxmin_airsr_stats_type)  :: maximum, minimum
      type (residual_airsr1_type)     :: average, rms_err
   end type stats_airsr_type

contains

#include "da_ao_stats_airsr.inc"
#include "da_jo_and_grady_airsr.inc"
#include "da_jo_airsr_tq.inc"
#include "da_residual_airsr.inc"
#include "da_oi_stats_airsr.inc"
#include "da_print_stats_airsr.inc"
#include "da_transform_xtoy_airsr.inc"
#include "da_transform_xtoy_airsr_adj.inc"
#include "da_check_max_iv_airsr.inc"
#include "da_get_innov_vector_airsr.inc"
#include "da_calculate_grady_airsr.inc"

end module da_airsr

