module da_module_couple_uv

   ! update_bc
   ! use da_control, only : trace_use
   ! use da_tracing, only : da_trace_entry, da_trace_exit

contains

#include "da_couple_uv.inc"
#include "da_calc_mu_uv.inc"
#include "da_couple.inc"

end module da_module_couple_uv
