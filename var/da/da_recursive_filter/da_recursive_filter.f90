module da_recursive_filter

   !---------------------------------------------------------------------------
   ! Purpose: Jim Pursers recursive filter routines.
   !---------------------------------------------------------------------------

   use module_domain, only : domain,  vp_type
  
   use da_control, only : ims,ime,jms,jme,kms,kme,ids,ide,jds,jde,kds,kde, &
      rf_passes, its,ite,jts,jte,kts,kte,vert_corr, trace_use, vert_corr_1,&
      trace_use_dull, cv_size
   use da_define_structures, only : be_type 
   use da_par_util, only : da_transpose_z2y, da_transpose_x2y, &
      da_transpose_y2z, da_transpose_y2x, da_transpose_x2z, &
      da_transpose_z2x, da_vv_to_cv,da_cv_to_vv
   use da_tracing, only : da_trace_entry, da_trace_exit

   use da_rfz_cv3, only : da_rfz
   use da_rf_cv3, only : smoothx, smoothy

   implicit none

   contains

#include "da_perform_2drf.inc"
#include "da_calculate_rf_factors.inc"
#include "da_recursive_filter_1d.inc"
#include "da_recursive_filter_1d_adj.inc"
#include "da_transform_through_rf.inc"
#include "da_transform_through_rf_adj.inc"

#include "da_apply_rf_1v.inc"
#include "da_apply_rf_1v_adj.inc"
#include "da_apply_rf.inc"
#include "da_apply_rf_adj.inc"

end module da_recursive_filter

