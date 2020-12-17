subroutine da_proc_sum_ints (values)

   !---------------------------------------------------------------------------
   !  Purpose: Do MPI sum operation across processors to get the global sum of
   !           an integer array. The sum is returned only on the root processor,
   !           i.e., processor 0. (In this way, we do not have to do all-to-all 
   !           communication, unlike wrf_dm_sum_ints, which does)
   !
   ! The routine generates a MPI barrier
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(inout) :: values(:) ! Values

#ifdef DM_PARALLEL
   integer, allocatable :: sums(:) ! Sum across processors.


   ! Don't trace, as called within trace routines
   ! if (trace_use_frequent) call da_trace_entry("da_proc_sum_ints")

   allocate (sums(size(values)))
   sums(:)=0
   call mpi_reduce(values(:), sums(:), size(values), mpi_integer, mpi_sum, &
      root, comm, ierr)

   if (rootproc) values(:) = sums(:)
   deallocate(sums)

   ! if (trace_use_frequent) call da_trace_exit("da_proc_sum_ints")
#endif

end subroutine da_proc_sum_ints


