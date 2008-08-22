module da_bogus

   use module_domain, only : domain

   use da_control, only : obs_qc_pointer,max_ob_levels,missing_r, &
      check_max_iv_print, check_max_iv_unit, v_interp_p, v_interp_h, &
      check_max_iv, missing, max_error_uv, max_error_t, rootproc, &
      bogus, max_error_p,max_error_q, trace_use_dull,fails_error_max, &
      max_stheight_diff,missing_data,max_error_bq,max_error_slp, &
      max_error_bt, max_error_buv, anal_type_verify, kms,kme,kts,kte, &
      ob_vars,qcstat_conv_unit
   use da_define_structures, only : maxmin_type, iv_type, y_type, jo_type, &
      bad_data_type, x_type, number_type, bad_data_type
   use da_interpolation, only : da_interp_lin_3d, da_to_zk, &
      da_interp_lin_3d_adj
   use da_par_util, only : da_proc_stats_combine
   use da_par_util1, only : da_proc_sum_int
   use da_physics, only : da_tpq_to_slp_adj,da_tpq_to_slp_lin
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_max_error_qc, da_residual, da_convert_zk,da_get_print_lvl
   use da_tracing, only : da_trace_entry, da_trace_exit

   ! The "stats_bogus_type" is ONLY used locally in da_bogus:

   type residual_bogus1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: q                        ! specific humidity
      real          :: slp                      ! sea level pressure (hPa)
   end type residual_bogus1_type

   type maxmin_bogus_stats_type
      type (maxmin_type)         :: u, v, t, q, slp 
   end type maxmin_bogus_stats_type

   type stats_bogus_type
      type (maxmin_bogus_stats_type)  :: maximum, minimum
      type (residual_bogus1_type)     :: average, rms_err
   end type stats_bogus_type

contains

#include "da_ao_stats_bogus.inc"
#include "da_jo_and_grady_bogus.inc"
#include "da_residual_bogus.inc"
#include "da_oi_stats_bogus.inc"
#include "da_print_stats_bogus.inc"
#include "da_transform_xtoy_bogus.inc"
#include "da_transform_xtoy_bogus_adj.inc"
#include "da_check_max_iv_bogus.inc"
#include "da_get_innov_vector_bogus.inc"
#include "da_calculate_grady_bogus.inc"

end module da_bogus

