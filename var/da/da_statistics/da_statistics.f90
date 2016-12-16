module da_statistics
   
   !---------------------------------------------------------------------------
   ! Purpose: Contains routines used to calculates statistical quantities.
   !---------------------------------------------------------------------------
   
   use module_domain, only : domain
   use da_control, only : obs_qc_pointer,stdout, missing_r, &
      myproc,rootproc, mjy, mix, mkz, jts,jte,its,ite,kts,kte, trace_use_dull, trace_use,&
      crtm_cloud,use_radar_rf,pptop,ppbot,num_ob_indexes,num_ob_vars,npres_print,&
      obs_names, ob_vars, filename_len, cloud_cv_options
   use da_define_structures, only : iv_type, maxmin_type, x_type, maxmin_field_type
   use da_par_util1, only : da_proc_sum_real, da_proc_sum_int, da_proc_sum_ints
   use da_par_util, only : da_proc_maxmin_combine
   use da_tracing, only : da_trace_entry, da_trace_exit
   use da_tools_serial, only : da_free_unit, da_get_unit
   use da_reporting, only : da_error
   
   implicit none
   
   contains
   
#include "da_analysis_stats.inc"
#include "da_correlation_coeff1d.inc"
#include "da_correlation_coeff2d.inc"
#include "da_data_distribution.inc"
#include "da_stats_calculate.inc"
#include "da_print_qcstat.inc"

end module da_statistics

