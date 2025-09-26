module da_par_util1

   use da_control, only : rootproc, ierr, comm, root
#ifdef DM_PARALLEL

#if ( DWORDSIZE != RWORDSIZE )
!  use mpi, only : mpi_sum, mpi_integer, mpi_complex, mpi_real
#else
!  use mpi, only : mpi_sum, mpi_integer, mpi_double_complex, mpi_real8
#endif

#endif

   !---------------------------------------------------------------------------
   ! Purpose: Routines for local-to-global and global-to-local grid operations.
   !
   ! METHOD:  RSL/MPI
   !---------------------------------------------------------------------------

   implicit none

#ifdef DM_PARALLEL
   include 'mpif.h'
#if ( DWORDSIZE != RWORDSIZE )
   integer, parameter :: true_mpi_real    = mpi_real
   integer, parameter :: true_mpi_complex = mpi_complex
#else
   integer, parameter :: true_mpi_real    = mpi_real8
   integer, parameter :: true_mpi_complex = mpi_double_complex
#endif
#endif

   contains

#include "da_proc_sum_int.inc"
#include "da_proc_sum_ints.inc"
#include "da_proc_sum_real.inc"

end module da_par_util1
