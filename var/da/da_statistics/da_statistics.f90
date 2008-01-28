module da_statistics
   
   !---------------------------------------------------------------------------
   ! Purpose: Contains routines used to calculates statistical quantities.
   !---------------------------------------------------------------------------
   
   use module_domain, only : domain
   use da_control, only : obs_qc_pointer,stdout, missing_r, &
      rootproc, mjy, mix, mkz, jts,jte,its,ite,kts,kte, trace_use_dull, trace_use
   use da_define_structures, only : maxmin_type, x_type, maxmin_field_type
   use da_par_util1, only : da_proc_sum_real
   use da_par_util, only : da_proc_maxmin_combine
   use da_tracing, only : da_trace_entry, da_trace_exit
   
   implicit none
   
   contains
   
#include "da_analysis_stats.inc"
#include "da_correlation_coeff1d.inc"
#include "da_correlation_coeff2d.inc"
#include "da_data_distribution.inc"
#include "da_stats_calculate.inc"

end module da_statistics

