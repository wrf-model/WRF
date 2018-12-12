module da_physics

   !---------------------------------------------------------------------------
   !  Purpose: Contains routines to calculate physical quantities.
   !---------------------------------------------------------------------------

   use module_domain, only : domain, x_type
#ifdef DM_PARALLEL
   use module_dm, only : local_communicator, mytask, ntasks, ntasks_x, &
      ntasks_y, data_order_xyz
   use module_comm_dm, only : halo_bal_eqn_adj_sub, halo_xa_cloud_sub
   use da_control, only : ips,ipe,jps,jpe,kps,kpe
#endif
   use da_define_structures, only : synop_type, residual_synop_type, infa_type, iv_type
   use da_control, only : gas_constant, gravity,kts,kte, svp3,svpt0, &
      a_ew,  wdk1, wdk2, zdk1, zdk2, zdk3, radian, &
      gas_constant_v, svp1, to, xls, svp2,its,ite,jts,jte,kts,kte, &
      ims,ime,jms,jme,kms,kme,xlv1,cp,ids,ide,jds,jde,kds,kde, test_transforms, &
      trace_use, missing_r, maximum_rh, minimum_rh,cv_options_hum,coeff,l_over_rv, &
      es_gammakelvin, es_gammabeta, rd_over_rv1,t_kelvin, es_alpha, es_gamma, &
      es_beta, rd_over_rv, trace_use_frequent,gamma, print_detail_xa, stdout, &
      cv_options_hum_specific_humidity, trace_use_dull, pi, consider_xap4ztd, &
      use_gpsephobs
   use da_par_util, only : da_transpose_z2y, da_transpose_y2x, da_patch_to_global, &
      da_transpose_x2z, da_transpose_z2x, da_transpose_x2y, da_transpose_y2z
   use da_tracing, only : da_trace_entry, da_trace_exit
   use da_interpolation, only : da_interp_lin_2d, da_interp_lin_2d_adj
   use da_dynamics, only : da_w_adjustment_adj, da_uv_to_divergence_adj, &
      da_w_adjustment_lin, da_uv_to_divergence
   use da_reporting, only : da_error, message
   use da_wrf_interfaces, only : wrf_debug
   use da_grid_definitions, only : da_ffdduv_model
   use da_gpseph, only : global_xa_ref

   implicit none

   REAL,   DIMENSION(0:1000 ),SAVE          :: PSIMTB,PSIHTB

   contains

#include "sfclayinit.inc"
#include "da_prho_to_t_adj.inc"
#include "da_prho_to_t_lin.inc"
#include "da_uvprho_to_w_lin.inc"
#include "da_uvprho_to_w_adj.inc"
#include "da_pt_to_rho_adj.inc"
#include "da_pt_to_rho_lin.inc"
#include "da_tpq_to_rh.inc"
#include "da_tpq_to_rh_lin.inc"
#include "da_tpq_to_rh_lin1.inc"
#include "da_tprh_to_q_adj.inc"
#include "da_tprh_to_q_adj1.inc"
#include "da_tprh_to_q_lin.inc"
#include "da_tprh_to_q_lin1.inc"
#include "da_tp_to_qs.inc"
#include "da_tp_to_qs1.inc"
#include "da_tp_to_qs_adj.inc"
#include "da_tp_to_qs_adj1.inc"
#include "da_tp_to_qs_lin.inc"
#include "da_tp_to_qs_lin1.inc"
#include "da_trh_to_td.inc"
#include "da_tpq_to_slp.inc"
#include "da_wrf_tpq_2_slp.inc"
#include "da_tpq_to_slp_lin.inc"
#include "da_tpq_to_slp_adj.inc"
#include "da_tv_profile.inc"
#include "da_find_layer_tl.inc"
#include "da_thickness_adj.inc"
#include "da_find_layer.inc"
#include "da_tv_profile_adj.inc"
#include "da_thickness.inc"
#include "da_find_layer_adj.inc"
#include "da_thickness_tl.inc"
#include "da_tv_profile_tl.inc"

#include "da_transform_xtoztd.inc"
#include "da_transform_xtoztd_lin.inc"
#include "da_transform_xtoztd_adj.inc"
#include "da_transform_xtotpw.inc"
#include "da_transform_xtotpw_adj.inc"
#include "da_transform_xtogpsref.inc"
#include "da_transform_xtogpsref_adj.inc"
#include "da_transform_xtogpsref_lin.inc"
#include "da_check_rh.inc"
#include "da_check_rh_simple.inc"
#include "da_get_q_error.inc"
#include "da_roughness_from_lanu.inc"
#include "da_julian_day.inc"
#include "da_sfc_wtq.inc"
#include "da_sfc_wtq_lin.inc"
#include "da_sfc_wtq_adj.inc"
#include "da_transform_xtopsfc.inc"
#include "da_transform_xtopsfc_adj.inc"
#include "da_transform_xtowtq.inc"
#include "da_transform_xtowtq_adj.inc"
#include "da_sfc_pre.inc"
#include "da_sfc_pre_lin.inc"
#include "da_sfc_pre_adj.inc"
#include "da_moist_phys_adj.inc"
#include "da_moist_phys_lin.inc"
#include "da_condens_adj.inc"
#include "da_condens_lin.inc"
#include "da_evapo_lin.inc"
#include "da_filter.inc"
#include "da_filter_adj.inc"
#include "da_wdt.inc"
#include "da_integrat_dz.inc"
#include "da_uv_to_sd_lin.inc"
#include "da_uv_to_sd_adj.inc"

end module da_physics

