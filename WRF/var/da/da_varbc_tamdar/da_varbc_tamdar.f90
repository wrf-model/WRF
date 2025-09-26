module da_varbc_tamdar

   !---------------------------------------------------------------------------
   ! Purpose: module for variational bias correction of TAMDAR temperature. 
   !---------------------------------------------------------------------------

   use module_dm, only : wrf_dm_sum_real, wrf_dm_sum_integer
   use da_control, only : trace_use,missing_r, qc_varbc_bad, stdout, filename_len, &
      rootproc, varbc_nbgerr, ierr, comm, max_ext_its, &
      obs_qc_pointer, tamdar, tamdar_sfc, missing_r,print_detail_obs, &
      use_varbc_tamdar, varbc_tamdar_nbgerr, varbc_tamdar_nobsmin, &
      varbc_tamdar_unit, varbc_tamdar_bm, varbc_tamdar_pred0, fail_varbc_aircraft
   use da_define_structures, only : iv_type, y_type, be_type, varbc_tamdar_type
   use da_reporting, only : da_error, message, da_warning, da_message
   use da_tools, only : da_eof_decomposition, da_diff_seconds
   use da_tools_serial, only : da_free_unit, da_get_unit
   use da_tracing, only : da_trace_entry, da_trace_exit, da_trace, &
      da_trace_int_sort
   
   implicit none

contains

#include "da_varbc_tamdar_init.inc"
#include "da_varbc_tamdar_pred.inc"
#include "da_varbc_tamdar_direct.inc"
#include "da_varbc_tamdar_precond.inc"
#include "da_varbc_tamdar_tl.inc"
#include "da_varbc_tamdar_adj.inc"
#include "da_varbc_tamdar_update.inc"

end module da_varbc_tamdar
