module da_tamdar

   use da_control, only : obs_qc_pointer,max_ob_levels,missing_r, &
      check_max_iv_print, check_max_iv_unit, v_interp_p, v_interp_h, &
      check_max_iv, missing_data, max_error_uv, max_error_t, rootproc, &
      max_error_p,max_error_q, sfc_assi_options, &
!, no_buddies, fails_error_max, &
!      fails_buddy_check, check_buddy, check_buddy_print, check_buddy_unit, &
!      buddy_weight , max_buddy_uv, max_buddy_t, max_buddy_p, max_buddy_rh, &
      max_stheight_diff,test_dm_exact, anal_type_verify, &
      kms,kme,kts,kte,sfc_assi_options_1,sfc_assi_options_2, num_procs, comm, &
      trace_use_dull, tamdar, tamdar_sfc, position_lev_dependant, max_ext_its, &
      qcstat_conv_unit,ob_vars, fails_error_max, &
      convert_fd2uv,convert_uv2fd,max_error_spd,max_error_dir,max_omb_spd,max_omb_dir,pi,qc_rej_both, &
      wind_sd_tamdar, wind_stats_sd
   use da_grid_definitions, only : da_ffdduv,da_ffdduv_model, da_ffdduv_diagnose
   use da_define_structures, only : maxmin_type, iv_type, y_type, jo_type, &
      bad_data_type, x_type, number_type, bad_data_type
   use module_domain, only : domain
   use da_interpolation, only : da_to_zk, da_interp_lin_3d, &
      da_interp_lin_3d_adj, da_interp_lin_2d, da_interp_lin_2d_adj, da_interp_lin_2d_partial
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_max_error_qc, da_residual, da_obs_sfc_correction,da_convert_zk, &
                        da_buddy_qc, da_get_print_lvl

   use da_par_util, only : da_proc_stats_combine
   use da_par_util1, only : da_proc_sum_int

   use da_physics, only : da_sfc_pre, da_transform_xtopsfc, &
      da_transform_xtopsfc_adj, da_uv_to_sd_lin, da_uv_to_sd_adj

   use da_par_util, only : da_proc_stats_combine
   use da_par_util1, only : da_proc_sum_int
   use da_tracing, only : da_trace_entry, da_trace_exit

   ! The "stats_tamdar_type" is ONLY used locally in da_tamdar:

   type residual_tamdar1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: q                        ! specific humidity
   end type residual_tamdar1_type

   type maxmin_tamdar_stats_type
      type (maxmin_type)         :: u, v, t, q
   end type maxmin_tamdar_stats_type

   type stats_tamdar_type
      type (maxmin_tamdar_stats_type)  :: maximum, minimum
      type (residual_tamdar1_type)     :: average, rms_err
   end type stats_tamdar_type

   ! The "stats_tamdar_sfc_type" is ONLY used locally in da_tamdar_sfc:

   type residual_tamdar_sfc1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: p                        ! pressure
      real          :: q                        ! specific humidity
   end type residual_tamdar_sfc1_type

   type maxmin_tamdar_sfc_stats_type
      type (maxmin_type)         :: u, v, t, p, q
   end type maxmin_tamdar_sfc_stats_type

   type stats_tamdar_sfc_type
      type (maxmin_tamdar_sfc_stats_type)  :: maximum, minimum
      type (residual_tamdar_sfc1_type)     :: average, rms_err
   end type stats_tamdar_sfc_type

contains

#include "da_ao_stats_tamdar.inc"
#include "da_jo_and_grady_tamdar.inc"
#include "da_jo_tamdar_uvtq.inc"
#include "da_residual_tamdar.inc"
#include "da_oi_stats_tamdar.inc"
#include "da_print_stats_tamdar.inc"
#include "da_transform_xtoy_tamdar.inc"
#include "da_transform_xtoy_tamdar_adj.inc"
#include "da_check_max_iv_tamdar.inc"
#include "da_get_innov_vector_tamdar.inc"
#include "da_calculate_grady_tamdar.inc"
!#include "da_check_buddy_tamdar.inc"

#include "da_ao_stats_tamdar_sfc.inc"
#include "da_jo_and_grady_tamdar_sfc.inc"
#include "da_jo_tamdar_sfc_uvtq.inc"
#include "da_residual_tamdar_sfc.inc"
#include "da_oi_stats_tamdar_sfc.inc"
#include "da_print_stats_tamdar_sfc.inc"
#include "da_transform_xtoy_tamdar_sfc.inc"
#include "da_transform_xtoy_tamdar_sfc_adj.inc"
#include "da_get_innov_vector_tamdar_sfc.inc"
#include "da_check_max_iv_tamdar_sfc.inc"
#include "da_calculate_grady_tamdar_sfc.inc"

end module da_tamdar

