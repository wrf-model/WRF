module da_lightning

   use module_domain, only : domain

   use da_control, only : stdout, obs_qc_pointer,max_ob_levels,missing_r, &
      v_interp_p, v_interp_h, check_max_iv_print, trace_use, &
      missing, max_error_uv, max_error_t, rootproc, &
      max_error_p,max_error_q, check_max_iv_unit,check_max_iv,  &
      max_stheight_diff,missing_data,max_error_bq,max_error_slp, &
      max_error_bt, max_error_buv, lightning, qcstat_conv_unit, fails_error_max, &
      use_lightning_w, use_lightning_qv, use_lightning_div, &
      fg_format,fg_format_wrf_arw_regional,fg_format_wrf_nmm_regional,fg_format_wrf_arw_global,&
      fg_format_kma_global,max_error_lda_w,max_error_lda_qv, max_error_lda_div, &
      far_below_model_surface,kms,kme,kts,kte, trace_use_dull,filename_len,&
      myproc, analysis_date, num_procs , ierr, comm
 
   use da_control, only : its, ite, jts, jte, ids, ide, jds, jde, ims, ime, jms, jme 
   use da_control, only : cloudbase_calc_opt
   use da_define_structures, only : maxmin_type, iv_type, y_type, jo_type, &
      bad_data_type, x_type, number_type, bad_data_type, &
      infa_type, field_type
   use da_interpolation, only : da_to_zk, da_interp_lin_3d,da_interp_lin_3d_adj
   use da_par_util, only :da_proc_stats_combine, da_patch_to_global
   use da_par_util1, only : da_proc_sum_int
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_max_error_qc, da_residual, map_info, da_llxy_wrf, da_llxy_default, da_convert_zk
   use da_tracing, only : da_trace_entry, da_trace_exit
   use da_reporting, only : da_error, da_warning, da_message, message
   use da_tools_serial, only : da_get_unit, da_free_unit

   ! The "stats_lightning_type" is ONLY used locally in da_lightning:

   type residual_lightning1_type
      real                    :: w
      real                    :: div
      real                    :: qv
   end type residual_lightning1_type

   type maxmin_lightning_stats_type
      type (maxmin_type)         :: w        ! vertical velocity
      type (maxmin_type)         :: div      ! divgerence 
      type (maxmin_type)         :: qv       ! water vapor
   end type maxmin_lightning_stats_type

   type stats_lightning_type
      type (maxmin_lightning_stats_type)  :: maximum, minimum
      type (residual_lightning1_type)     :: average, rms_err
   end type stats_lightning_type

contains

#include "da_ao_stats_lightning.inc"
#include "da_jo_and_grady_lightning.inc"
#include "da_residual_lightning.inc"
#include "da_oi_stats_lightning.inc"
#include "da_print_stats_lightning.inc"
#include "da_transform_xtoy_lightning.inc"
#include "da_transform_xtoy_lightning_adj.inc"
#include "da_check_max_iv_lightning.inc"
#include "da_get_innov_vector_lightning.inc"
#include "da_calculate_grady_lightning.inc"
#include "da_div_profile.inc"
#include "da_div_profile_tl.inc"
#include "da_div_profile_adj.inc"

end module da_lightning

