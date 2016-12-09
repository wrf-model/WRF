module da_tools
   
   !---------------------------------------------------------------------------
   ! Purpose: Contains general tools.
   !---------------------------------------------------------------------------
   
   use module_bc, only : bdyzone
   use module_dm, only : wrf_dm_sum_real
   use module_domain, only : xb_type, domain

   use da_control, only : pi, gravity, gas_constant, ims, ime, jms,jme, &
      kms,kme,its,ite,jts,jte,kts,kte,ids,ide,stdout, &
      trace_use_dull, trace_use, fg_format_kma_global, coarse_ds, coarse_ix, &
      coarse_jy, fg_format, c2, cone_factor, earth_radius, dsm, &
      map_projection, psi1, pole, start_x, phic, start_y, xlonc, ycntr, &
      obs_qc_pointer, anal_type_verify, fg_format_wrf_arw_regional, &
      fg_format_wrf_nmm_regional, fg_format_wrf_arw_global, fg_format_kma_global, &
      set_omb_rand_fac, fails_error_max, fails_buddy_check, no_buddies, &
      missing_r, x_start_sub_domain, global, myproc, comm, &
      x_end_sub_domain, y_end_sub_domain, def_sub_domain, &
      y_start_sub_domain, start_lat, delt_lat, delt_lon, start_lon, cp, &
      missing_data, surface_correction,print_detail_map, use_rad, stderr, &
      t_kelvin, trace_use_frequent, jds, jde, pptop,ppbot,npres_print, &
      rad_to_deg, deg_to_rad, num_procs, print_detail_obs, psfc_from_slp

#ifdef DM_PARALLEL
!  use mpi, only : mpi_integer
#endif

   use da_define_structures, only : info_type, field_type, x_type,  &
      model_loc_type, synop_type, bad_info_type, da_gauss_noise, &
      iv_type, y_type, da_random_seed, infa_type
   use da_tools_serial, only : da_array_print
   use da_tracing, only : da_trace_entry, da_trace_exit
   use da_reporting, only : da_error, message, da_warning, da_message
   use da_lapack, only : dsyev
   
   implicit none
   
#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

   ! Code copied from SI, see header below
#include "da_map_utils_defines.inc"

contains

#include "da_llxy.inc"
#include "da_llxy_new.inc"
#include "da_llxy_default.inc"
#include "da_llxy_default_new.inc"
#include "da_llxy_kma_global.inc"
#include "da_llxy_kma_global_new.inc"
#include "da_llxy_global.inc"
#include "da_llxy_global_new.inc"
#include "da_llxy_rotated_latlon.inc"
#include "da_llxy_latlon.inc"
#include "da_llxy_latlon_new.inc"
#include "da_llxy_lc.inc"
#include "da_llxy_lc_new.inc"
#include "da_llxy_merc.inc"
#include "da_llxy_merc_new.inc"
#include "da_llxy_ps.inc"
#include "da_llxy_ps_new.inc"
#include "da_llxy_wrf.inc"
#include "da_llxy_wrf_new.inc"
#include "da_xyll.inc"
#include "da_xyll_default.inc"
#include "da_xyll_latlon.inc"
#include "da_xyll_lc.inc"
#include "da_xyll_merc.inc"
#include "da_xyll_ps.inc"
#include "da_set_lc.inc"
#include "da_set_ps.inc"
#include "da_map_init.inc"
#include "da_map_set.inc"
#include "da_set_merc.inc"
#include "da_lc_cone.inc"
#include "da_convert_zk.inc"

#include "da_1d_eigendecomposition.inc"
#include "da_obs_sfc_correction.inc"
#include "da_sfcprs.inc"
#include "da_intpsfc_prs.inc"
#include "da_intpsfc_tem.inc"
#include "da_mo_correction.inc"
#include "da_diff_seconds.inc"
#include "da_residual.inc"
#include "da_residual_new.inc"
#include "da_add_noise.inc"
#include "da_add_noise_new.inc"
#include "da_max_error_qc.inc"
#include "da_random_omb.inc"
#include "da_set_randomcv.inc"
#include "da_gaus_noise.inc"
#include "da_openfile.inc"
#include "da_smooth_anl.inc"
#include "da_togrid_new.inc"
#include "da_togrid.inc"
#include "da_unifva.inc"
#include "da_buddy_qc.inc"

#include "da_eof_decomposition_test.inc"
#include "da_eof_decomposition.inc"
#include "da_lubksb.inc"
#include "da_ludcmp.inc"
#include "da_set_boundary_xa.inc"
#include "da_set_boundary_xb.inc"
#include "da_set_boundary_3d.inc"

#include "da_get_2d_sum.inc"
#include "da_get_3d_sum.inc"
#include "da_get_print_lvl.inc"

#include "da_get_julian_time.inc"
#include "da_get_time_slots.inc"

#include "da_msl2geo1.inc"
#include "da_geo2msl1.inc"

#include "da_sfc_hori_interp_weights.inc"

end module da_tools

