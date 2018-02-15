module da_interpolation

   use da_control, only : stdout, trace_use, trace_use_frequent, missing_r, &
      anal_type_verify, v_interp_h, v_interp_p,ims,ime,jms,jme,kms,kme, &
      kts,kte, trace_use_dull, interp_option
   use da_define_structures, only : infa_type
   use da_tools, only : da_togrid
   use da_tracing, only : da_trace_entry, da_trace_exit
   use da_reporting, only: da_error, da_warning, message

   implicit none

contains

#include "da_to_zk.inc"
#include "da_to_zk_new.inc"

#include "da_interp_2d_partial.inc"
#include "da_interp_lin_2d_partial.inc"
#include "da_interp_lin_2d.inc"
#include "da_interp_lin_2d_adj_partial.inc"
#include "da_interp_lin_2d_adj.inc"
#include "da_interp_lin_3d.inc"
#include "da_interp_lin_3d_adj.inc"
#include "da_interp_quad_2d_partial.inc"
#include "da_interp_msk_avg_2d_partial.inc"
#include "da_splinx.inc"
#include "da_splinx_lin.inc"
#include "da_splinx_adj.inc"

end module da_interpolation

