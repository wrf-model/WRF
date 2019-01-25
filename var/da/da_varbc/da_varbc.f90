module da_varbc

   !---------------------------------------------------------------------------
   ! Purpose: module for variational bias correction. 
   !---------------------------------------------------------------------------

#if defined(RTTOV) || defined(CRTM)
   use module_dm, only : wrf_dm_sum_real, wrf_dm_sum_reals, wrf_dm_sum_integer
   use module_radiance, only : q2ppmv, satinfo
   use da_control, only : trace_use,missing_r, qc_varbc_bad, rtm_option, &
      stdout,rtm_option_rttov,rtm_option_crtm, filename_len, cv_size_domain, &
      cv_size_domain_jp, use_varbc, freeze_varbc, varbc_factor, varbc_nobsmin, &
      rootproc, varbc_nbgerr, ierr, comm, max_ext_its, varbc_scan
   use da_define_structures, only : iv_type, y_type, be_type, &
      varbc_info_type,varbc_type
   use da_radiance1, only : stats_rad_type
#ifdef RTTOV
   use da_radiance1, only : da_predictor_rttov
#endif
#ifdef CRTM
   use da_radiance1, only : da_predictor_crtm
#endif
   use da_reporting, only : da_error, message, da_warning, da_message
   use da_tools, only : da_eof_decomposition
   use da_tools_serial, only : da_free_unit, da_get_unit
   use da_tracing, only : da_trace_entry, da_trace_exit, da_trace, &
      da_trace_int_sort
   
   implicit none
   
contains

#include "da_varbc_direct.inc"
#include "da_varbc_tl.inc"
#include "da_varbc_adj.inc"
#include "da_varbc_pred.inc"
#include "da_varbc_coldstart.inc"
#include "da_varbc_precond.inc"
#include "da_varbc_init.inc"
#include "da_varbc_update.inc"

#endif

end module da_varbc
