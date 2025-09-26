module da_rain 

   use module_domain, only : domain
   
#ifdef DM_PARALLEL
   use module_dm, only : local_communicator, mytask, ntasks, ntasks_x, &
      ntasks_y
   use module_comm_dm, only : halo_em_rain_sub
#endif

   use da_control, only : obs_qc_pointer,missing_r, &
      check_max_iv_print, check_max_iv_unit, v_interp_p, v_interp_h, &
      check_max_iv, missing, rootproc, max_error_rain, &
      rain, trace_use,fails_error_max, &
      max_stheight_diff,missing_data,anal_type_verify, &
      anal_type_verify,max_ext_its,qcstat_conv_unit,ob_vars, &
      ids,ide,jds,jde,kds,kde, ims,ime,jms,jme,kms,kme, &
      ips,ipe,jps,jpe,kps,kpe,num_fgat_time, write_rej_obs_conv

   use da_define_structures, only : maxmin_type, iv_type, y_type, jo_type, &
      bad_data_type, x_type, number_type, bad_data_type
   use da_interpolation, only : da_to_zk, &
      da_interp_lin_3d,da_interp_lin_3d_adj, &
      da_interp_lin_2d, da_interp_lin_2d_adj, da_interp_lin_2d_partial
   use da_par_util1, only : da_proc_sum_int
   use da_par_util, only : da_proc_stats_combine
   use da_statistics, only : da_stats_calculate
   use da_tools, only : da_max_error_qc, da_residual, da_obs_sfc_correction, &
       da_convert_zk,map_info,da_llxy_wrf, da_llxy_default
   use da_tracing, only : da_trace_entry, da_trace_exit

   ! The "stats_rain_type" is ONLY used locally in da_rain:

   type residual_rain1_type
      real          :: rain     
   end type residual_rain1_type

   type maxmin_rain_stats_type
      type (maxmin_type)         :: rain
   end type maxmin_rain_stats_type

   type stats_rain_type
      type (maxmin_rain_stats_type)  :: maximum, minimum
      type (residual_rain1_type)     :: average, rms_err
   end type stats_rain_type

contains

#include "da_ao_stats_rain.inc"
#include "da_jo_and_grady_rain.inc"
#include "da_residual_rain.inc"
#include "da_oi_stats_rain.inc"
#include "da_print_stats_rain.inc"
#include "da_transform_xtoy_rain.inc"
#include "da_transform_xtoy_rain_adj.inc"
#include "da_check_max_iv_rain.inc"
#include "da_get_hr_rain.inc"
#include "da_get_innov_vector_rain.inc"
#include "da_calculate_grady_rain.inc"


end module da_rain 

