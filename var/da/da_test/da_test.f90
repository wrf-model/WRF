module da_test

   !---------------------------------------------------------------------------
   ! Purpose: Collection of routines associated with minimisation.
   !---------------------------------------------------------------------------

   use module_configure, only : grid_config_rec_type
   use module_dm, only : wrf_dm_sum_real, wrf_dm_sum_integer

#ifdef DM_PARALLEL
   use module_dm, only : local_communicator, &
      ntasks_x, ntasks_y, data_order_xyz, mytask, &
      ntasks, data_order_xy
   use module_comm_dm, only : halo_psichi_uv_adj_sub, halo_xa_sub, &
      halo_sfc_xa_sub, halo_ssmi_xa_sub, halo_radar_xa_w_sub
!  use mpi, only : mpi_sum
#endif

   use da_control, only : num_procs, var4d_bin, var4d_lbc
   use module_domain, only : vp_type, xb_type, x_type, ep_type, &
      domain, domain_clock_get, domain_clock_set, domain_clockprint, domain_clockadvance
   use module_state_description, only : dyn_em,dyn_em_tl,dyn_em_ad,p_a_qv

   use da_control, only : trace_use,ierr, trace_use_dull, comm,global,stdout,rootproc, &
      sfc_assi_options,typical_qrn_rms,typical_qci_rms,typical_qsn_rms,typical_qgr_rms,jcdfi_use, jcdfi_diag, &
      typical_u_rms,typical_v_rms,typical_w_rms,typical_t_rms, typical_p_rms, typical_rain_rms, &
      typical_q_rms,typical_qcw_rms,print_detail_testing,typical_rh_rms, &
      fg_format, fg_format_wrf_arw_global, fg_format_wrf_arw_regional,fg_format_wrf_nmm_regional, &
      typical_rf_rms,typical_rv_rms, typical_thickness_rms, typical_tb19v_rms,typical_tb37h_rms, &
      typical_tb85h_rms,typical_tb37v_rms,typical_tb85v_rms,typical_tb22v_rms, &
      typical_tb19h_rms,typical_speed_rms,typical_tpw_rms,typical_ref_rms, &
      cv_options_hum,inv_typ_vp5_sumsq,inv_typ_vp1_sumsq, trajectory_io, &
      inv_typ_vp3_sumsq,inv_typ_vp2_sumsq,inv_typ_vpalpha_sumsq, &
      inv_typ_vp4_sumsq,typical_rho_rms,balance_geo,balance_cyc,balance_type, &
      balance_geocyc, var4d, num_fgat_time,cv_options_hum_specific_humidity, &
      cv_options_hum_relative_humidity, ids, ide, jds, jde, kds, kde, &
      sound, sonde_sfc, mtgirs, synop, profiler, gpsref, gpspw, polaramv, geoamv, ships, metar, &
      satem, radar, ssmi_rv, ssmi_tb, ssmt1, ssmt2, airsr, pilot, airep, tamdar, tamdar_sfc, rain, &
      bogus, buoy, qscat, pseudo, radiance, use_radarobs, use_ssmiretrievalobs,use_rainobs, &
      use_gpsrefobs, use_ssmt1obs, use_ssmitbobs, use_ssmt2obs, use_gpspwobs, &
      use_gpsztdobs, use_radar_rf, use_radar_rhv, use_rad, crtm_cloud, cloud_cv_options, &
      ids,ide,jds,jde,kds,kde, ims,ime,jms,jme,kms,kme, fgat_rain_flags, &
      its,ite,jts,jte,kts,kte, ips,ipe,jps,jpe,kps,kpe, cv_options, cv_size, &
      cloud_cv_options, cp, gas_constant, test_dm_exact, cv_size_domain, &
      its_int, ite_int, jts_int, jte_int, kts_int, kte_int, &
      ims_int, ime_int, jms_int, jme_int, kms_int, kme_int
   use da_control, only : use_cv_w
   use da_control, only : typical_eph_rms, gpseph, use_gpsephobs, missing_r

   use da_define_structures, only : da_zero_x,da_zero_vp_type,da_allocate_y, &
      da_deallocate_y,be_type, xbx_type, iv_type, y_type, j_type, da_initialize_cv
   use da_dynamics, only : da_uv_to_divergence,da_uv_to_vorticity, &
      da_psichi_to_uv, da_psichi_to_uv_adj, da_uv_to_divergence_adj, &
      da_divergence_constraint, da_divergence_constraint_adj
   use da_ffts, only : da_solve_poissoneqn_fct
   use da_minimisation, only : da_transform_vtoy_adj,da_transform_vtoy, da_swap_xtraj, &
       da_read_basicstates, da_calculate_j, da_calculate_gradj
   use da_obs, only : da_transform_xtoy,da_transform_xtoy_adj
   use da_par_util, only : da_patch_to_global, da_system, da_cv_to_global
#ifdef DM_PARALLEL
   use da_par_util1, only : true_mpi_real
#endif
   use da_physics, only : da_transform_xtopsfc,da_transform_xtopsfc_adj, &
      da_pt_to_rho_lin,da_transform_xtotpw,da_transform_xtogpsref_lin, &
      da_transform_xtowtq, da_transform_xtowtq_adj,da_pt_to_rho_adj, &
      da_transform_xtotpw_adj, da_transform_xtoztd_lin, da_transform_xtoztd_adj, &
      da_moist_phys_lin, da_moist_phys_adj, da_uvprho_to_w_lin, da_uvprho_to_w_adj
   use da_reporting, only : da_error, message, da_message
   use da_spectral, only : da_test_spectral
   use da_ssmi, only : da_transform_xtoseasfcwind_lin, &
      da_transform_xtoseasfcwind_adj
   use da_statistics, only : da_correlation_coeff1d,da_correlation_coeff2d
   use da_tools_serial, only : da_get_unit,da_free_unit
   use da_tracing, only : da_trace_entry,da_trace_exit
   use da_transfer_model, only : da_transfer_wrftltoxa,da_transfer_xatowrftl, &
      da_transfer_xatowrftl_adj,da_transfer_wrftltoxa_adj,da_transfer_wrftoxb
   ! Don't use, as we pass a 3D array into a 1D one
   ! use da_wrf_interfaces, only : wrf_dm_bcast_real
   use da_wrf_interfaces, only : wrf_debug, wrf_shutdown
   use da_wrfvar_io, only : da_med_initialdata_output,da_med_initialdata_input
   use da_vtox_transforms, only : da_transform_xtotb_lin, &
      da_transform_xtotb_adj, da_vertical_transform, da_transform_vptox, &
      da_transform_xtogpsref_adj,da_transform_vptox_adj,da_transform_vtox, &
      da_transform_vtox_adj,da_transform_vtovv,da_transform_vtovv_global, &
      da_transform_vtovv_global_adj, da_transform_vtovv_adj, da_transform_xtoxa, &
      da_transform_xtoxa_adj, da_apply_be, da_apply_be_adj, da_transform_bal, &
      da_transform_bal_adj
#ifdef VAR4D
   use da_transfer_model, only : da_transfer_xatowrftl_lbc, da_transfer_xatowrftl_adj_lbc, da_get_2nd_firstguess
   use da_4dvar, only : model_grid, da_tl_model, da_ad_model, input_nl_xtraj, upsidedown_ad_forcing, &
       u6_2, v6_2, w6_2, t6_2, ph6_2, p6, mu6_2, psfc6, moist6
   use da_rain, only : da_transform_xtoy_rain, da_transform_xtoy_rain_adj
#endif

   implicit none

   private :: da_dot_cv, da_dot

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

contains

#include "da_check_balance.inc"
#include "da_check_cvtovv_adjoint.inc"
#include "da_check_vtox_adjoint.inc"
#include "da_check_vptox_adjoint.inc"
#include "da_check_vp_errors.inc"
#include "da_check_vvtovp_adjoint.inc"
#include "da_check_xtovptox_errors.inc"
#include "da_check_xtoy_adjoint.inc"
#include "da_check_xtoy_adjoint_airep.inc"
#include "da_check_xtoy_adjoint_gpspw.inc"
#include "da_check_xtoy_adjoint_gpsref.inc"
#include "da_check_xtoy_adjoint_gpseph.inc"
#include "da_check_xtoy_adjoint_metar.inc"
#include "da_check_xtoy_adjoint_pilot.inc"
#include "da_check_xtoy_adjoint_ssmi_rv.inc"
#include "da_check_xtoy_adjoint_ssmi_tb.inc"
#include "da_check_xtoy_adjoint_satem.inc"
#include "da_check_xtoy_adjoint_geoamv.inc"
#include "da_check_xtoy_adjoint_polaramv.inc"
#include "da_check_xtoy_adjoint_ships.inc"
#include "da_check_xtoy_adjoint_radar.inc"
#include "da_check_xtoy_adjoint_rain.inc"
#include "da_check_xtoy_adjoint_bogus.inc"
#include "da_check_xtoy_adjoint_sound.inc"
#include "da_check_xtoy_adjoint_sonde_sfc.inc"
#include "da_check_xtoy_adjoint_mtgirs.inc"
#include "da_check_xtoy_adjoint_tamdar.inc"
#include "da_check_xtoy_adjoint_tamdar_sfc.inc"
#include "da_check_xtoy_adjoint_synop.inc"
#include "da_check_xtoy_adjoint_rad.inc"
#include "da_transform_xtovp.inc"
#include "da_check.inc"
#include "da_dot.inc"
#include "da_dot_cv.inc"
#include "da_check_xtoy_adjoint_pseudo.inc"
#include "da_check_xtoy_adjoint_qscat.inc"
#include "da_check_xtoy_adjoint_ssmt1.inc"
#include "da_check_xtoy_adjoint_ssmt2.inc"
#include "da_check_xtoy_adjoint_profiler.inc"
#include "da_check_xtoy_adjoint_buoy.inc"
#include "da_setup_testfield.inc"
#include "da_check_sfc_assi.inc"
#include "da_check_psfc.inc"
#include "da_set_tst_trnsf_fld.inc"
#include "da_check_vtoy_adjoint.inc"
#include "da_get_y_lhs_value.inc"
#include "da_check_gradient.inc"
#include "da_check_dynamics_adjoint.inc"

end module da_test
