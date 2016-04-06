module da_ffts

   !---------------------------------------------------------------------------
   ! Purpose: Routines to perform Fourier transforms.
   !---------------------------------------------------------------------------

   use module_domain, only :domain
   use da_control, only : ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte, &
      Inverse_FFT,  Forward_FFT, ids,jds, trace_use, &
      ide,jde, stdout
   use da_define_structures, only : xbx_type
   use da_par_util, only : da_transpose_x2z, da_transpose_y2x, &
      da_transpose_y2x_v2, da_transpose_z2x, da_transpose_x2y, &
      da_transpose_x2y_v2
   use da_tracing, only : da_trace_entry, da_trace_exit
   use da_wrf_interfaces, only : wrf_debug
   use module_dm, only : wrf_dm_sum_reals
#ifdef DM_PARALLEL
   use module_dm, only : local_communicator, &
      ntasks_x, ntasks_y, data_order_xyz, mytask, ntasks
   use da_control, only : ids,ide,ips,ipe,jds,jde,jps,jpe,kds,kde,kps,kpe
   use module_comm_dm, only : halo_bal_eqn_adj_sub
#endif

   use module_ffts, only : fft661, fft551

   implicit none

   contains

#include "da_solve_poissoneqn_fct.inc"
#include "da_solve_poissoneqn_fct_adj.inc"
#include "da_solve_poissoneqn_fst.inc"
#include "da_solve_poissoneqn_fst_adj.inc"

end module da_ffts
