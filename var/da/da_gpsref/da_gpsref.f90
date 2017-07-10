module da_gpsref

   use module_domain, only : domain
   use module_dm, only : wrf_dm_sum_real

   use da_control, only : obs_qc_pointer,max_ob_levels,missing_r, &
      v_interp_p, v_interp_h, check_max_iv_print, radian, &
      missing, max_error_uv, max_error_t, rootproc,fails_error_max, &
      max_error_p,max_error_q, check_max_iv_unit,check_max_iv, qcstat_conv_unit, &
      max_stheight_diff,missing_data,max_error_bq,max_error_slp, ob_vars, &
      max_error_bt, max_error_buv, gpsref,max_error_thickness, &
! t_iwabuchi 20121216 use ms,ime,jms,jme for interpolation of log(N)
!      pseudo_var, num_pseudo, kms,kme,kts,kte, trace_use_dull, &
      pseudo_var, num_pseudo, ims,ime,jms,jme, kms,kme,kts,kte, trace_use_dull, &
! t_iwabuchi END
      anal_type_verify,fails_error_max,pseudo_err,pseudo_x, pseudo_y, stdout, &
      use_gpsrefobs, gpsref_thinning, pseudo_z,pseudo_val,max_error_ref, pseudo, &
      jts, jte,its,ite, npres_print, pptop
   use da_control, only : pseudo_ref
   use da_define_structures, only : maxmin_type, iv_type, y_type, jo_type, &
      bad_data_type, x_type, number_type, bad_data_type, &
      maxmin_type, da_allocate_observations
   use da_interpolation, only : da_interp_lin_3d,da_interp_lin_3d_adj, &
      da_to_zk
   use da_par_util, only : da_proc_stats_combine
   use da_par_util1, only : da_proc_sum_int
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_max_error_qc, da_residual, da_convert_zk,da_get_print_lvl
   use da_tracing, only : da_trace_entry, da_trace_exit

   ! The "stats_gpsref_type" is ONLY used locally in da_gpsref:

   type residual_gpsref1_type
      real :: ref                   ! GPS Refractivity
      real ::   p                   ! Retrieved from GPS Refractivity
      real ::   t                   ! Retrieved from GPS Refractivity
      real ::   q                   ! Used in GPS Refra. retrieval.
   end type residual_gpsref1_type

   type maxmin_gpsref_stats_type
      type (maxmin_type)         :: ref          ! GPS Refractivity
   end type maxmin_gpsref_stats_type

   type stats_gpsref_type
      type (maxmin_gpsref_stats_type)  :: maximum, minimum
      type (residual_gpsref1_type)     :: average, rms_err
   end type stats_gpsref_type

contains

#include "da_ao_stats_gpsref.inc"
#include "da_calculate_grady_gpsref.inc"
#include "da_jo_and_grady_gpsref.inc"
#include "da_residual_gpsref.inc"
#include "da_oi_stats_gpsref.inc"
#include "da_print_stats_gpsref.inc"
#include "da_transform_xtoy_gpsref.inc"
#include "da_transform_xtoy_gpsref_adj.inc"
#include "da_check_max_iv_gpsref.inc"
#include "da_get_innov_vector_gpsref.inc"

end module da_gpsref

