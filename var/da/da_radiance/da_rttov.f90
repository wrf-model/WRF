module da_rttov

   !---------------------------------------------------------------------------
   ! Purpose: module for radiance data assimilation. 
   !---------------------------------------------------------------------------

#ifdef RTTOV
   use da_define_structures, only : iv_type, y_type, x_type
   use module_domain, only : domain

   use module_radiance, only : satinfo, &
       i_kind,r_kind, r_double, &
       one, zero, three,deg2rad, q2ppmv, &
       coefs, opts,opts_rt_ir, rttov_inst_name
   use module_radiance, only : rttov_options, rttov_opts_rt_ir, rttov_coefs, rttov_profile, &
       rttov_transmission, rttov_radiance, rttov_chanprof, &
       jpim, jprb, errorstatus_success, errorstatus_fatal, gas_id_watervapour, &
       atlas, atlas_type, atlas_id, atlas_type_ir, atlas_type_mw, &
       sensor_id_ir, sensor_id_mw, sensor_id_hi, sensor_id_po, rttov_emissivity

   use da_control, only : max_ob_levels,missing_r, &
      v_interp_p, v_interp_h, tovs_batch, gravity, &
      missing, max_error_uv, max_error_t, max_error_p,max_error_q,  &
      max_stheight_diff,missing_data,max_error_bq,max_error_slp, &
      max_error_bt, max_error_buv, rtminit_platform,rtminit_satid, &
      rtminit_nsensor,rtminit_sensor,filename_len,read_biascoef,analysis_date, &
      time_window_max,time_window_min, kts,kte,kms,kme, &
      rtm_option_rttov,use_rttov_kmatrix,rtm_option_crtm, gravity, &
      print_detail_rad,stderr, mw_emis_sea, &
      rtminit_print, rttov_scatt,comm,ierr,biasprep, qc_rad, &
      num_fgat_time,stdout,trace_use, use_error_factor_rad, &
      qc_good, qc_bad,myproc,biascorr, global,ims,ime,jms,jme, &
      use_clddet, time_slots, rttov_emis_atlas_ir, rttov_emis_atlas_mw, &
      use_mspps_emis, use_mspps_ts
   use da_interpolation, only : da_to_zk_new, &
      da_interp_lin_2d, da_interp_lin_3d, da_interp_lin_3d_adj, da_interp_lin_2d_adj
   use da_tools_serial, only : da_get_unit, da_free_unit
#ifdef DM_PARALLEL
   use da_par_util, only :  true_mpi_real
   use da_wrf_interfaces, only : wrf_dm_bcast_integer
#endif
   use da_radiance1, only : num_tovs_after,tovs_copy_count, &
      tovs_send_pe, tovs_recv_pe, tovs_send_start, tovs_send_count, &
      tovs_recv_start,con_vars_type,aux_vars_type, &
      da_biascorr, da_detsurtyp,da_biasprep, da_mspps_emis, da_mspps_ts
   use da_reporting, only : da_message, message, da_warning, da_error
   use da_tools, only : da_convert_zk, da_get_time_slots
   use da_tracing, only : da_trace_entry, da_trace_exit, da_trace

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

!#include "rttov_setup.interface"
#include "rttov_direct.interface"
#include "rttov_tl.interface"
#include "rttov_ad.interface"
#include "rttov_k.interface"
!#include "rttov_dealloc_coefs.interface"
#include "rttov_alloc_rad.interface"
#include "rttov_alloc_transmission.interface"
#include "rttov_alloc_prof.interface"
#include "rttov_errorreport.interface"
#include "rttov_read_coefs.interface"
#include "rttov_init_coefs.interface"
#include "rttov_setup_emis_atlas.interface"
#include "rttov_get_emis.interface"
#include "rttov_deallocate_emis_atlas.interface"
   
contains

#include "da_get_innov_vector_rttov.inc"
#include "da_transform_xtoy_rttov.inc"
#include "da_transform_xtoy_rttov_adj.inc"

#include "da_rttov_init.inc"
#include "da_rttov_direct.inc"
#include "da_rttov_tl.inc"
#include "da_rttov_ad.inc"
#include "da_rttov_k.inc"

#endif

end module da_rttov

