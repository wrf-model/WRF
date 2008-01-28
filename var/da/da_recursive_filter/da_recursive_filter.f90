module da_recursive_filter

   !---------------------------------------------------------------------------
   ! Purpose: Jim Pursers recursive filter routines.
   !---------------------------------------------------------------------------

   use module_domain, only : domain
  
   use da_control, only : ims,ime,jms,jme,kms,kme,jds,jde, &
      rf_passes, its,ite,jts,jte,vert_corr, trace_use, vert_corr_1, trace_use_dull
   use da_par_util, only : da_transpose_z2y, da_transpose_x2y, &
      da_transpose_y2z, da_transpose_y2x, da_transpose_x2z, &
      da_transpose_z2x
   use da_tracing, only : da_trace_entry, da_trace_exit

   implicit none

   contains

#include "da_perform_2drf.inc"
#include "da_calculate_rf_factors.inc"
#include "da_recursive_filter_1d.inc"
#include "da_recursive_filter_1d_adj.inc"
#include "da_transform_through_rf.inc"
#include "da_transform_through_rf_adj.inc"

end module da_recursive_filter

