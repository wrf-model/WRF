module da_chem_sfc

!---------------------------------------------------------------------------
! Purpose: Collection of routines associated with CHEM observations
! and CVs
!---------------------------------------------------------------------------

use module_dm, only : wrf_dm_sum_reals, wrf_dm_sum_real
use module_domain, only : head_grid
use da_reporting, only : da_message,da_warning,message
!   use da_wrf_interfaces, only : wrf_debug

use module_domain_type, only : domain  !!! add !!!
#if (WRF_CHEM == 1)
use da_control, only : rootproc,ierr,comm, &
      fails_error_max, trace_use, trace_use_dull, &
      chemic_surf, test_dm_exact, &
      sfc_assi_options, sfc_assi_options_1,sfc_assi_options_2, &
      obs_qc_pointer,missing_r, qcstat_conv_unit, &
      check_max_iv_print, check_max_iv, missing, rootproc, &
      max_error_chemic_surf, &
      chem_cv_options, &
      chemicda_opt, &
      use_chemic_surfobs, chemic_surf, &
      missing_r, stdout, &
      its, ite, jts, jte, kts, kte, &
      ims, ime, jms, jme, kms, kme, &
      ids, ide, jds, jde, kds, kde
use module_domain, only : xchem_type
use da_define_structures, only : da_allocate_observations_chem_sfc
use da_interpolation, only : da_interp_lin_2d,da_interp_lin_2d_adj
use da_statistics, only : da_stats_calculate
use da_par_util1, only : da_proc_sum_int, da_proc_sum_ints
use da_par_util, only : da_proc_stats_combine
use module_state_description, only : num_chemic_surf, num_chem, &
      PARAM_FIRST_SCALAR, &
      p_chemsi_pm25, p_chemsi_pm10, &
      p_chemsi_so2, p_chemsi_no2, p_chemsi_o3, p_chemsi_co, &
      p_chem_ic_p25, p_chem_ic_p10, p_chem_ic_sulf, p_chem_ic_bc1, p_chem_ic_bc2, p_chem_ic_oc1, p_chem_ic_oc2, &
      p_chem_ic_dust_1, p_chem_ic_dust_2, p_chem_ic_dust_3, p_chem_ic_dust_4, &
      p_chem_ic_seas_1, p_chem_ic_seas_2, p_chem_ic_seas_3, p_chem_ic_seas_4, &
      p_chem_ic_bc_a01, p_chem_ic_bc_a02, p_chem_ic_bc_a03, p_chem_ic_bc_a04, p_chem_ic_oc_a01, p_chem_ic_oc_a02, p_chem_ic_oc_a03, p_chem_ic_oc_a04, &
      p_chem_ic_so4_a01, p_chem_ic_so4_a02, p_chem_ic_so4_a03, p_chem_ic_so4_a04, p_chem_ic_no3_a01, p_chem_ic_no3_a02, p_chem_ic_no3_a03, p_chem_ic_no3_a04,&
      p_chem_ic_nh4_a01, p_chem_ic_nh4_a02, p_chem_ic_nh4_a03, p_chem_ic_nh4_a04, p_chem_ic_cl_a01, p_chem_ic_cl_a02, p_chem_ic_cl_a03, p_chem_ic_cl_a04, &
      p_chem_ic_na_a01, p_chem_ic_na_a02, p_chem_ic_na_a03, p_chem_ic_na_a04, p_chem_ic_oin_a01, p_chem_ic_oin_a02, p_chem_ic_oin_a03, p_chem_ic_oin_a04, &
      p_chem_ic_so2, p_chem_ic_no2, p_chem_ic_o3, p_chem_ic_co, &
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

use da_define_structures, only : iv_type, y_type, jo_type, maxmin_type, &
      bad_data_type, number_type

use da_tools_serial, only : da_get_unit,da_free_unit
use da_tracing, only: da_trace_entry, da_trace_exit
use da_tools, only : da_max_error_qc, da_residual

   type residual_chem_sfc_type
      real,    pointer     :: chem(:)
   end type residual_chem_sfc_type

   type maxmin_chem_sfc_stats_type
      type (maxmin_type),    pointer     :: chem(:)
   end type maxmin_chem_sfc_stats_type

   type stats_chem_sfc_type
      type (maxmin_chem_sfc_stats_type)  :: maximum, minimum
      type (residual_chem_sfc_type)     :: average, rms_err
   end type stats_chem_sfc_type

!   use da_wrf_interfaces, only: da_wrf_get_dm_communicator

#ifdef DM_PARALLEL
    include 'mpif.h'
#endif

contains

#if (WRF_CHEM == 1)
#include "da_ao_stats_chem_sfc.inc"
#include "da_jo_and_grady_chem_sfc.inc"
#include "da_jo_chem_sfc.inc"
#include "da_residual_chem_sfc.inc"
#include "da_transform_xtoy_chem_sfc.inc"
#include "da_transform_xtoy_chem_sfc_adj.inc"
#include "da_get_innov_vector_chem_sfc.inc"
#include "da_check_max_iv_chem_sfc.inc"
#include "da_calculate_grady_chem_sfc.inc"
#include "da_oi_stats_chem_sfc.inc"
#include "da_print_stats_chem_sfc.inc" 

#endif

end module da_chem_sfc
