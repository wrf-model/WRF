module da_chem

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
use da_control, only : rootproc,ierr,comm,num_surf_obs,num_acft_obs,num_ts, &
      fails_error_max, trace_use, &
      num_platform, chem_surf, chem_acft, &
      obs_qc_pointer,missing_r, qcstat_conv_unit, &
      check_max_iv_print, check_max_iv, missing, rootproc, &
      max_error_chem_surf, max_error_chem_acft, &
      sigma_r_surf, sigma_r_acft, sigma_c_surf, sigma_c_acft, &
      pinterp_option, &
      num_ant_steps, num_bb_steps, &
      use_chem_surfobs, use_chem_acftobs, chem_surf, chem_acft, &
      missing_r, stdout, adtl_run_hours, &
      crossval_chem_surfobs, crossval_chem_acftobs, &
      its, ite, jts, jte, kts, kte, &
      ims, ime, jms, jme, kms, kme, &
      ids, ide, jds, jde, kds, kde

use da_define_structures, only : da_allocate_observations_chem
use da_obs, only: da_fill_obs_structures_chem

use module_state_description, only : num_surf_hx, num_acft_hx, &
      num_chem_surf, num_chem_acft, PARAM_FIRST_SCALAR, &
      num_scaleant, num_scalebb
#endif

use da_define_structures, only : iv_type, y_type, jo_type, &
      bad_data_type, number_type

use da_tools_serial, only : da_get_unit,da_free_unit
use da_tracing, only: da_trace_entry, da_trace_exit
use da_tools, only : da_max_error_qc, da_residual

!   use da_wrf_interfaces, only: da_wrf_get_dm_communicator

#ifdef VAR4D
use module_wrf_top, only: domain
#endif

   implicit none

#ifdef DM_PARALLEL
    include 'mpif.h'
#endif

contains

#if (WRF_CHEM == 1)

#include "da_copy_chem_to_model.inc"
#include "da_retrieve_chem_hx.inc"
#include "da_write_obs_chem.inc"
#include "da_calculate_chem_forcing_ad.inc"
#include "da_get_innov_vector_chem_surf.inc"
#include "da_get_innov_vector_chem_acft.inc"
#include "da_check_max_iv_chem_surf.inc"
#include "da_check_max_iv_chem_acft.inc"
#include "da_jo_and_grady_chem_surf.inc"
#include "da_jo_and_grady_chem_acft.inc"
#include "da_residual_chem_surf.inc"
#include "da_residual_chem_acft.inc"
#include "da_calculate_grady_chem_surf.inc"
#include "da_calculate_grady_chem_acft.inc"

#endif

end module da_chem
