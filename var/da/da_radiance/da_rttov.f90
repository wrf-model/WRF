module da_rttov


   !---------------------------------------------------------------------------
   ! Purpose: module for radiance data assimilation. 
   !---------------------------------------------------------------------------

#ifdef RTTOV
   use da_define_structures, only : iv_type, y_type, x_type
   use module_domain, only : domain
   use da_reporting, only : da_error

   use module_radiance, only : satinfo, coefs_scatt_instname, &
       i_kind,r_kind, r_double, &
       one, zero, three,deg2rad, n_scatt_coef,q2ppmv, gsi_emiss
   use module_radiance, only : coefs,coefs_scatt,profile_type,radiance_type, &
      rttov_coef,platform_name,rttov_inst_name,transmission_type, &
      errorstatus_success,gas_id_watervapour,errorstatus_fatal

#ifdef DM_PARALLEL
!  use mpi, only : mpi_integer, mpi_status_size
#endif

   use da_control, only : max_ob_levels,missing_r, &
      v_interp_p, v_interp_h, tovs_batch, gravity, &
      missing, max_error_uv, max_error_t, max_error_p,max_error_q,  &
      max_stheight_diff,missing_data,max_error_bq,max_error_slp, &
      max_error_bt, max_error_buv, rtminit_platform,rtminit_satid, &
      rtminit_nsensor,rtminit_sensor,filename_len,read_biascoef,analysis_date, &
      time_window_max,time_window_min, kts,kte,kms,kme, &
      rtm_option_rttov,rtm_option_crtm, gravity, &
      print_detail_rad,stderr, mw_emis_sea, &
      rtminit_print, rttov_scatt,comm,ierr,biasprep, qc_rad, &
      num_fgat_time,stdout,trace_use, use_error_factor_rad, &
      qc_good, qc_bad,myproc,biascorr, global,ims,ime,jms,jme, &
      use_airs_mmr, time_slots
   use da_interpolation, only : da_to_zk_new, &
      da_interp_lin_2d, da_interp_lin_3d, da_interp_lin_3d_adj, da_interp_lin_2d_adj
   use da_tools_serial, only : da_get_unit, da_free_unit
#ifdef DM_PARALLEL
   use da_par_util, only :  true_mpi_real
#endif
   use da_radiance1, only : num_tovs_after,tovs_copy_count, &
      tovs_send_pe, tovs_recv_pe, tovs_send_start, tovs_send_count, &
      tovs_recv_start,con_vars_type,aux_vars_type, &
      da_biascorr, da_detsurtyp,da_biasprep
   use da_reporting, only : da_message, message, da_warning
   use da_tools, only : da_convert_zk, da_get_time_slots
   use da_tracing, only : da_trace_entry, da_trace_exit, da_trace
   use da_wrf_interfaces, only : wrf_dm_bcast_integer
   use da_reporting, only : da_warning

   implicit none

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

#include "rttov_setupchan.interface"
#include "rttov_setupindex.interface"
   
contains

#include "da_get_innov_vector_rttov.inc"
#include "da_transform_xtoy_rttov.inc"
#include "da_transform_xtoy_rttov_adj.inc"

#include "da_rttov_init.inc"
#include "da_rttov_direct.inc"
#include "da_rttov_tl.inc"
#include "da_rttov_ad.inc"

#endif

end module da_rttov

