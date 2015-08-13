module da_sound

   use da_control, only : obs_qc_pointer,max_ob_levels,missing_r, missing_data, &
      check_max_iv_print, check_max_iv_unit, v_interp_p, v_interp_h, &
      check_max_iv, missing, max_error_uv, max_error_t, rootproc, &
      max_error_p,max_error_q, sfc_assi_options, no_buddies, fails_error_max, &
      fails_buddy_check, check_buddy, check_buddy_print, check_buddy_unit, &
      buddy_weight , max_buddy_uv, max_buddy_t, max_buddy_p, max_buddy_rh, &
      max_stheight_diff,test_dm_exact, anal_type_verify, &
      kms,kme,kts,kte,sfc_assi_options_1,sfc_assi_options_2, num_procs, comm, &
      trace_use_dull, sound, sonde_sfc, position_lev_dependant, max_ext_its,qcstat_conv_unit,ob_vars, &
      convert_fd2uv,convert_uv2fd,max_error_spd,max_error_dir,max_omb_spd,max_omb_dir,pi,qc_rej_both, &
      wind_sd_sound, wind_stats_sd
   use da_grid_definitions, only : da_ffdduv,da_ffdduv_model, da_ffdduv_diagnose

#ifdef DM_PARALLEL
!  use mpi, only : mpi_integer, mpi_real8, mpi_max
#endif
   use da_define_structures, only : maxmin_type, iv_type, y_type, jo_type, &
      bad_data_type, x_type, number_type, bad_data_type
   use module_domain, only : domain
   use da_interpolation, only : da_to_zk, da_interp_lin_3d, &
      da_interp_lin_3d_adj, da_interp_lin_2d, da_interp_lin_2d_adj, da_interp_lin_2d_partial
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_max_error_qc, da_residual, da_obs_sfc_correction, da_convert_zk,&
                        da_buddy_qc, da_get_print_lvl
   use da_par_util, only : da_proc_stats_combine, &
      da_deallocate_global_sound, da_to_global_sound, da_to_global_sonde_sfc, &
      da_deallocate_global_sonde_sfc
   use da_par_util1, only : da_proc_sum_int 
   use da_physics, only : da_sfc_pre, da_transform_xtopsfc, &
      da_transform_xtopsfc_adj, da_uv_to_sd_lin, da_uv_to_sd_adj
   use da_tracing, only : da_trace_entry, da_trace_exit

   ! The "stats_sound_type" is ONLY used locally in da_sound:

   type residual_sound1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: q                        ! specific humidity
   end type residual_sound1_type

   type maxmin_sound_stats_type
      type (maxmin_type)         :: u, v, t, q
   end type maxmin_sound_stats_type

   type stats_sound_type
      type (maxmin_sound_stats_type)  :: maximum, minimum
      type (residual_sound1_type)     :: average, rms_err
   end type stats_sound_type

   ! The "stats_sonde_sfc_type" is ONLY used locally in da_sonde_sfc:

   type residual_sonde_sfc1_type
      real          :: u                        ! u-wind.
      real          :: v                        ! v-wind.
      real          :: t                        ! temperature
      real          :: p                        ! pressure
      real          :: q                        ! specific humidity
   end type residual_sonde_sfc1_type

   type maxmin_sonde_sfc_stats_type
      type (maxmin_type)         :: u, v, t, p, q
   end type maxmin_sonde_sfc_stats_type

   type stats_sonde_sfc_type
      type (maxmin_sonde_sfc_stats_type)  :: maximum, minimum
      type (residual_sonde_sfc1_type)     :: average, rms_err
   end type stats_sonde_sfc_type

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

contains

#include "da_ao_stats_sound.inc"
#include "da_jo_and_grady_sound.inc"
#include "da_jo_sound_uvtq.inc"
#include "da_residual_sound.inc"
#include "da_oi_stats_sound.inc"
#include "da_print_stats_sound.inc"
#include "da_transform_xtoy_sound.inc"
#include "da_transform_xtoy_sound_adj.inc"
#include "da_check_max_iv_sound.inc"
#include "da_get_innov_vector_sound.inc"
#include "da_calculate_grady_sound.inc"
#include "da_check_buddy_sound.inc"

#include "da_ao_stats_sonde_sfc.inc"
#include "da_jo_and_grady_sonde_sfc.inc"
#include "da_jo_sonde_sfc_uvtq.inc"
#include "da_residual_sonde_sfc.inc"
#include "da_oi_stats_sonde_sfc.inc"
#include "da_print_stats_sonde_sfc.inc"
#include "da_transform_xtoy_sonde_sfc.inc"
#include "da_transform_xtoy_sonde_sfc_adj.inc"
#include "da_get_innov_vector_sonde_sfc.inc"
#include "da_check_max_iv_sonde_sfc.inc"
#include "da_calculate_grady_sonde_sfc.inc"

end module da_sound

