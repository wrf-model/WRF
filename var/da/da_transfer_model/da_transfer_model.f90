module da_transfer_model

   !---------------------------------------------------------------------------
   ! Purpose: Transfer model states between different models
   !---------------------------------------------------------------------------

   use module_configure, only : grid_config_rec_type, model_config_rec
   use module_date_time, only : geth_julgmt, current_date, start_date
   use module_domain, only : domain, domain_clock_get, x_type, vp_type, ep_type
   use module_io_domain, only : open_r_dataset, close_dataset, input_auxinput17, &
      output_auxinput7, open_w_dataset
   use module_state_description, only : dyn_em_ad, dyn_em, dyn_em_tl, &
      p_qv, p_qr, p_qi, p_qs, p_qg, p_qc, param_first_scalar, num_moist, &
      p_g_qv, p_g_qr, p_g_qi, p_g_qs, p_g_qg, p_g_qc, &
      p_a_qv, p_a_qr, p_a_qi, p_a_qs, p_a_qg, p_a_qc, num_g_moist, num_a_moist, &
      f_qc, f_qr, f_qi, f_qs, f_qg, f_g_qc, f_g_qr, f_g_qi, f_g_qs, f_g_qg, &
      f_a_qc, f_a_qr, f_a_qi, f_a_qs, f_a_qg, warmrain_ad

#if (WRF_CHEM == 1)
   use module_state_description, only : &
      PARAM_FIRST_SCALAR, num_chem, p_bc2, p_oc2, p_co, p_no2, p_o3, p_so2, &
      p_p10, p_p25, p_sulf, &
      p_chem_ic_bc2, p_chem_ic_oc2, p_chem_ic_co, p_chem_ic_no2, p_chem_ic_o3, p_chem_ic_so2, &
      p_chem_ic_p10, p_chem_ic_p25, p_chem_ic_sulf , &
      p_bc_a01, p_bc_a02, p_bc_a03, p_oc_a01, p_oc_a02, p_oc_a03,  &
      p_so4_a01, p_so4_a02, p_so4_a03, p_no3_a01, p_no3_a02, p_no3_a03,  &
      p_nh4_a01, p_nh4_a02, p_nh4_a03, p_cl_a01, p_cl_a02, p_cl_a03,  &
      p_na_a01, p_na_a02, p_na_a03, p_oin_a01, p_oin_a02, p_oin_a03,  &
      p_chem_ic_bc_a01, p_chem_ic_bc_a02, p_chem_ic_bc_a03, p_chem_ic_oc_a01, p_chem_ic_oc_a02, p_chem_ic_oc_a03, &
      p_chem_ic_so4_a01, p_chem_ic_so4_a02, p_chem_ic_so4_a03, p_chem_ic_no3_a01, p_chem_ic_no3_a02, p_chem_ic_no3_a03, &
      p_chem_ic_nh4_a01, p_chem_ic_nh4_a02, p_chem_ic_nh4_a03, p_chem_ic_cl_a01, p_chem_ic_cl_a02, p_chem_ic_cl_a03, &
      p_chem_ic_na_a01, p_chem_ic_na_a02, p_chem_ic_na_a03, p_chem_ic_oin_a01, p_chem_ic_oin_a02, p_chem_ic_oin_a03, &
      p_so4aj,  p_so4ai,  p_nh4aj,  p_nh4ai,  &   ! aerosols in racm_soa_vbs_kpp
      p_no3aj,  p_no3ai,  p_naaj,   p_naai,   &
      p_asoa1j, p_asoa1i, p_asoa2j, p_asoa2i, &
      p_asoa3j, p_asoa3i, p_asoa4j, p_asoa4i, &
      p_bsoa1j, p_bsoa1i, p_bsoa2j, p_bsoa2i, &
      p_bsoa3j, p_bsoa3i, p_bsoa4j, p_bsoa4i, &
      p_orgpaj, p_orgpai, p_ecj,    p_eci,    &
      p_p25j,   p_p25i,   p_antha,  p_seas,   &
      p_claj,   p_clai,   p_soila,            &
      p_chem_ic_so4aj,  p_chem_ic_so4ai,  p_chem_ic_nh4aj,  p_chem_ic_nh4ai,  &   ! aerosols in racm_soa_vbs_da
      p_chem_ic_no3aj,  p_chem_ic_no3ai,  p_chem_ic_naaj,   p_chem_ic_naai,   &
      p_chem_ic_asoa1j, p_chem_ic_asoa1i, p_chem_ic_asoa2j, p_chem_ic_asoa2i, &
      p_chem_ic_asoa3j, p_chem_ic_asoa3i, p_chem_ic_asoa4j, p_chem_ic_asoa4i, &
      p_chem_ic_bsoa1j, p_chem_ic_bsoa1i, p_chem_ic_bsoa2j, p_chem_ic_bsoa2i, &
      p_chem_ic_bsoa3j, p_chem_ic_bsoa3i, p_chem_ic_bsoa4j, p_chem_ic_bsoa4i, &
      p_chem_ic_orgpaj, p_chem_ic_orgpai, p_chem_ic_ecj,    p_chem_ic_eci,    &
      p_chem_ic_p25j,   p_chem_ic_p25i,   p_chem_ic_antha,  p_chem_ic_seas,   &
      p_chem_ic_claj,   p_chem_ic_clai,   p_chem_ic_soila
#endif

   use module_dm, only : wrf_dm_sum_real, wrf_dm_sum_reals
#ifdef DM_PARALLEL
   use module_dm, only : local_communicator, &
      ntasks_x, ntasks_y, data_order_xyz, mytask, &
      ntasks, data_order_xy
   use module_comm_dm, only : halo_xa_sub, halo_init_sub, halo_psichi_uv_adj_sub, &
                              halo_xb_sub, halo_xb_uv_sub, halo_em_c_sub, halo_em_c_tl_sub, &
                              halo_xa_a_sub, halo_x6a_a_sub, halo_em_bdy_sub, halo_em_e_tl_sub, &
#if (WRF_CHEM == 1)
                              halo_chem_xb_sub, halo_chem_init_sub, halo_chem_xa_sub, &
#endif
                              halo_em_e_sub
#endif

   use da_control, only : cos_xls, sin_xls, cos_xle, sin_xle, trace_use, &
      coarse_jy, coarse_ix, cone_factor, delt_lon, delt_lat, gas_constant, &
      map_projection,earth_omega,mix,pi,phic,mkz,start_lon,start_lat, &
      start_x,xlonc,start_y,mjy, global, rad_to_deg, deg_to_rad, earth_radius, &
      var4d,var4d_lbc,analysis_date,coarse_ds,analysis_accu,dsm,pole, fg_format_kma_global, &
      fg_format, fg_format_wrf_arw_regional, fg_format_wrf_nmm_regional, &  
      print_detail_map,stdout,truelat1_3dv, base_pres, fg_format_wrf_arw_global, &
      truelat2_3dv, periodic_x,write_increments,max_ext_its, gravity, &
      kappa, print_detail_xa,rd_over_rv,t0, print_detail_xa, check_rh, adj_sens,&
      print_detail_xb,test_dm_exact,base_lapse,base_temp,vertical_ip,ptop, &
      use_gpsztdobs, use_ssmitbobs, use_radarobs, use_radar_rf, use_radar_rhv,&
      dt_cloud_model, cp, use_ssmiretrievalobs, var4d_detail_out, &
      vertical_ip_sqrt_delta_p, vertical_ip_delta_p,check_rh_simple, check_rh_tpw, &
      t_kelvin, num_fgat_time, num_pseudo, iso_temp, interval_seconds, trajectory_io, &
      cv_options, &
      ids,ide,jds,jde,kds,kde, ims,ime,jms,jme,kms,kme, num_fft_factors, &
      its,ite,jts,jte,kts,kte, ips,ipe,jps,jpe,kps,kpe, qlimit, &
      update_sfcdiags, use_wrf_sfcinfo, use_radar_rqv, cloudbase_calc_opt, use_gpsephobs, &
      cloud_cv_options
   use da_control, only: base_pres_strat, base_lapse_strat
   use da_control, only: c1f, c2f, c1h, c2h, c3f, c3h, c4f, c4h
   use da_define_structures, only : xbx_type, be_type
#if (WRF_CHEM == 1)
   use da_define_structures, only : iv_type, y_type
#endif
   use da_par_util, only : da_patch_to_global
   use da_physics, only : da_check_rh_simple,da_roughness_from_lanu, &
      da_sfc_wtq,da_tpq_to_rh,da_trh_to_td,da_wrf_tpq_2_slp,da_integrat_dz, &
      da_tp_to_qs, da_check_rh,da_transform_xtogpsref, da_transform_xtoztd, &
      sfclayinit
   use da_reporting, only : da_error,message, da_message, da_warning
   use da_setup_structures, only : da_setup_runconstants,da_write_increments, &
      da_write_kma_increments,da_cloud_model, da_write_increments_for_wrf_nmm_regional
   use da_ssmi, only : da_transform_xtotb
   use da_tools, only : map_info, proj_merc, proj_ps,proj_lc,proj_latlon, &
      da_llxy_default,da_llxy_wrf,da_xyll,da_diff_seconds,da_map_set, &
      da_set_boundary_xb
   use da_tracing, only : da_trace_entry, da_trace_exit, da_trace
   use da_vtox_transforms, only : da_get_vpoles
   use da_radar, only : zlcl_mean
   use da_gpseph, only : da_gpseph_init
   use da_wrf_interfaces, only : wrf_dm_bcast_real
#ifdef VAR4D
   use da_4dvar, only : model_grid, push_ad_forcing, push_tl_pert, pop_tl_pert, kj_swap, &
       kj_swap_reverse, model_config_flags, g_couple, g_stuff_bdy, a_couple, a_stuff_bdy, &
       g_stuff_bdytend, a_stuff_bdytend_old, a_stuff_bdytend_new, decouple, da_calc_2nd_fg, &
       ubdy3dtemp1 , vbdy3dtemp1 , tbdy3dtemp1 , pbdy3dtemp1 , qbdy3dtemp1, mbdy2dtemp1, &
       ubdy3dtemp2 , vbdy3dtemp2 , tbdy3dtemp2 , pbdy3dtemp2 , qbdy3dtemp2, mbdy2dtemp2, &
       wbdy3dtemp1,  wbdy3dtemp2, da_bdy_fields_halo
   use module_bc, only : set_physical_bc2d
   use module_big_step_utilities_em, only : calc_mu_uv
   use g_module_big_step_utilities_em, only : g_calc_mu_uv
   use a_module_big_step_utilities_em, only : a_calc_mu_uv
   USE module_io_wrf, only : auxinput8_alarm, auxhist8_alarm, auxhist7_alarm
#ifdef DM_PARALLEL
   use mediation_pertmod_io, only : da_halo_em_e_ad
#endif
#endif

   implicit none

   contains

#include "da_transfer_wrftoxb.inc"
#if (WRF_CHEM == 1)
#include "da_transfer_wrftoxb_chem.inc"
#endif
#include "da_transfer_wrf_nmm_regional_toxb.inc"
#include "da_transfer_kmatoxb.inc"
#include "da_transfer_xatowrf.inc"
#include "da_transfer_xatowrf_nmm_regional.inc"
#include "da_transfer_xatokma.inc"
#include "da_transfer_wrftltoxa.inc"
#include "da_transfer_wrftltoxa_adj.inc"
#include "da_transfer_xatowrftl.inc"
#include "da_transfer_xatowrftl_lbc.inc"
#include "da_transfer_wrftl_lbc_t0.inc"
#include "da_transfer_xatowrftl_adj.inc"
#include "da_transfer_xatowrftl_adj_lbc.inc"
#include "da_transfer_wrftl_lbc_t0_adj.inc"
#include "da_transfer_xatoanalysis.inc"
#include "da_setup_firstguess.inc"
#include "da_setup_firstguess_wrf.inc"
#include "da_setup_firstguess_wrf_nmm_regional.inc"
#include "da_setup_firstguess_kma.inc"
#include "da_get_2nd_firstguess.inc"

end module da_transfer_model
