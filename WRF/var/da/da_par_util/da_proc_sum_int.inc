subroutine da_proc_sum_int (value)

   !---------------------------------------------------------------------------
   !  Purpose: Do MPI sum operation across processors to get the global sum of
   !           an integer value. The sum is returned only on the root processor,
   !           i.e., processor 0. (In this way, we do not have to do all-to-all 
   !           communication, unlike wrf_dm_sum_int, which does)
   !
   ! The routine generates a MPI barrier
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(inout) :: value     ! Value on processor.

#ifdef DM_PARALLEL
   integer :: sum                   ! Sum across processors.

   ! Don't trace, as called within trace routines
   ! if (trace_use_frequent) call da_trace_entry("da_proc_sum_int")

   sum=0
   call mpi_reduce(value, sum, 1, mpi_integer, mpi_sum, root, &
      comm, ierr)

   if (rootproc) value = sum

   ! if (trace_use_frequent) call da_trace_exit("da_proc_sum_int")
#endif

end subroutine da_proc_sum_int


