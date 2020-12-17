module da_etkf

!------------------------------------------------------------------------------
!  Purpose: Routines to run an ETKF within WRF-Var.
!
!  HISTORY: 11/21/2004 - Xuguang Wang's routines included in WRF-Var.Dale Barker
!------------------------------------------------------------------------------

   use da_control, only : stdout, trace_use
   use da_gen_be, only : da_trace_entry, da_trace_exit
   use da_lapack, only : dsyev

   implicit none

contains

#include "da_innerprod.inc"
#include "da_matmulti.inc"
#include "da_matmultiover.inc"
#include "da_solve_etkf.inc"

end module da_etkf

