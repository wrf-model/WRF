subroutine da_proc_maxmin_combine(n, max, min)

   !---------------------------------------------------------------------------
   !  Purpose: Do MPI reduction operations across processors to get the minimum
   !           and maximum values for an observation field of length n. The
   !           i, j location of the minimum and maximum, for each n, is also
   !           communicated.
   !           The return values are stored only on the root processor, i.e., 
   !           processor 0.  (In this way, we do not have to do all-to-all 
   !           communication.)
   !---------------------------------------------------------------------------
   
   implicit none

   integer,                  intent(in)     :: n       ! Length of input fields.
   type (maxmin_field_type), intent(inout)  :: max(n)  ! Max values over proc.
   type (maxmin_field_type), intent(inout)  :: min(n)  ! Min values over proc.

   real    :: in(2*n)            ! mpi_reduce input value with processor rank.
   real    :: out(2*n)           ! mpi_reduce output min/max with associated processor.
   integer :: i                  ! Loop counter.
   integer :: proc_id(n)         ! Id of processor with max or min value.
   integer :: status(mpi_status_size) ! MPI status.

#ifdef DM_PARALLEL

   if (trace_use_frequent) call da_trace_entry("da_proc_maxmin_combine")

   ! Get minimum value and associated processor index.
   do i = 1, n
      in(2*i-1) = min(i)%value
      in(2*i)   = myproc
   end do

#if ( RWORDSIZE == 4 )
   call mpi_reduce(in, out, n, mpi_2real, mpi_minloc, root, comm, ierr)
#else
   call mpi_reduce(in, out, n, mpi_2double_precision, mpi_minloc, root, comm, ierr)
#endif

   if (rootproc) then
      do i = 1, n
         min(i)%value = out(2*i-1)
         proc_id(i)   = int(out(2*i))
      end do
   end if

   call wrf_dm_bcast_integer (proc_id, n)

   ! Get i and j where minimum occurs.
   do i = 1, n
      if (proc_id(i) .ne. 0) then
         if (rootproc) then
            call mpi_recv(min(i)%i, 1, mpi_integer, proc_id(i), 10, comm, STATUS, ierr)
            call mpi_recv(min(i)%j, 1, mpi_integer, proc_id(i), 11, comm, STATUS, ierr)
         else if (myproc == proc_id(i)) then
            call mpi_send(min(i)%i, 1, mpi_integer, root, 10, comm, ierr)
            call mpi_send(min(i)%j, 1, mpi_integer, root, 11, comm, ierr)
         end if
      end if
   end do

   ! Get maximum value and associated processor index.
   do i = 1, n
      in(2*i-1) = max(i)%value
      in(2*i)   = myproc
   end do
#if ( RWORDSIZE == 4 )
   call mpi_reduce(in, out, n, mpi_2real, mpi_maxloc, root, comm, ierr)
#else
   call mpi_reduce(in, out, n, mpi_2double_precision, mpi_maxloc, root, comm, ierr)
#endif

   if (rootproc) then
      do i = 1, n
         max(i)%value = out(2*i-1)
         proc_id(i)   = int(out(2*i))
      end do
   end if

   call wrf_dm_bcast_integer (proc_id, n)

   ! Get i and j where maximum occurs.
   do i = 1, n
      if (proc_id(i) .ne. root) then
         if (rootproc) then
            call mpi_recv(max(i)%i, 1, mpi_integer, proc_id(i), 10, comm, STATUS, ierr)
            call mpi_recv(max(i)%j, 1, mpi_integer, proc_id(i), 11, comm, STATUS, ierr)
         else if (myproc == proc_id(i)) then
            call mpi_send(max(i)%i, 1, mpi_integer, root, 10, comm, ierr)
            call mpi_send(max(i)%j, 1, mpi_integer, root, 11, comm, ierr)
         end if
      end if
   end do

   if (trace_use_frequent) call da_trace_exit("da_proc_maxmin_combine")
#endif

end subroutine da_proc_maxmin_combine


