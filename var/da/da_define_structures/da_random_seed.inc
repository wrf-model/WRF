subroutine da_random_seed

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

#ifdef DM_PARALLEL
   INCLUDE 'mpif.h'
#endif

   integer              :: seed_size
   integer, allocatable :: seed_array(:)

   integer :: myproc,ierr

   if (trace_use) call da_trace_entry("da_random_seed")

   !----------------------------------------------------------------------------
   !  Check that right seed_size is being used:
   !----------------------------------------------------------------------------

   myproc=0
#ifdef DM_PARALLEL
   call wrf_get_dm_communicator (comm)
   call mpi_comm_rank (comm, myproc, ierr)
#endif

   call random_seed(size=seed_size)              ! Get size of seed array.
   allocate(seed_array(1:seed_size))
   seed_array(1:seed_size) = 0

   if (put_rand_seed) then            ! Manually set random seed.
      if (seed_size /= 2) then
         write(unit=message(1),fmt='(a)') &
            ' Warning: only setting first two values of seed_array'
         call da_warning(__FILE__,__LINE__,message(1:1))
      end if
     
      seed_array(1) = seed_array1
      seed_array(2) = seed_array2 * seed_array1 + myproc*10000000
      write(unit=message(1),fmt='(a,i16)')' Setting seed_array(1) = ', seed_array(1)
      write(unit=message(2),fmt='(a,i16)')' Setting seed_array(2) = ', seed_array(2)
      call da_message(message(1:2))
      call random_seed(put=seed_array(1:seed_size)) ! Set random seed.
   else                                 ! Random seed set "randomly"
      call random_seed
      call random_seed(get=seed_array(1:seed_size))
      write(unit=message(1),fmt='(a,10i16)') 'Random number seed array = ', seed_array
      call da_message(message(1:1))
   end if
   
   deallocate(seed_array)

   if (trace_use) call da_trace_exit("da_random_seed")

end subroutine da_random_seed


