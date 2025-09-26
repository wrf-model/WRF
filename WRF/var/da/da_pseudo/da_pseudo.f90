module da_pseudo

   use da_control, only : obs_qc_pointer,max_ob_levels,missing_r, &
      v_interp_p, v_interp_h, trace_use_dull, &
      missing, max_error_uv, max_error_t, rootproc, &
      max_error_p,max_error_q, pseudo, &
      max_stheight_diff,missing_data,max_error_bq,max_error_slp, &
      max_error_bt, max_error_buv, pseudo_var
   use da_define_structures, only : maxmin_type, iv_type, y_type, jo_type, &
      bad_data_type, x_type, number_type, bad_data_type
   use module_domain, only : domain
   use da_interpolation, only : da_interp_lin_3d,da_interp_lin_3d_adj
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_residual, da_convert_zk
   use da_par_util, only : da_proc_stats_combine
   use da_par_util1, only : da_proc_sum_int
   use da_tracing, only : da_trace_entry, da_trace_exit

   ! The "stats_pseudo_type" is ONLY used locally in da_pseudo:

   type residual_pseudo1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: p                        ! pressure
      real          :: q                        ! specific humidity
   end type residual_pseudo1_type

   type maxmin_pseudo_stats_type
      type (maxmin_type)         :: u, v, t, p, q
   end type maxmin_pseudo_stats_type

   type stats_pseudo_type
      type (maxmin_pseudo_stats_type)  :: maximum, minimum
      type (residual_pseudo1_type)     :: average, rms_err
   end type stats_pseudo_type

contains

#include "da_jo_and_grady_pseudo.inc"
#include "da_residual_pseudo.inc"
#include "da_get_innov_vector_pseudo.inc"
#include "da_ao_stats_pseudo.inc"
#include "da_oi_stats_pseudo.inc"
#include "da_print_stats_pseudo.inc"
#include "da_transform_xtoy_pseudo.inc"
#include "da_transform_xtoy_pseudo_adj.inc"
#include "da_calculate_grady_pseudo.inc"

end module da_pseudo

