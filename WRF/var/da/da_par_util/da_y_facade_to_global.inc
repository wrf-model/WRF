subroutine da_y_facade_to_global( re_slice, template, re_glob_slice )

   !---------------------------------------------------------------------------
   ! Purpose:  Collect a local y_facade_type object into a global y_facade_type 
   !           object while preserving serial-code storage order.  This is 
   !           used to perform bitwise-exact floating-point summations in 
   !           serial-code-order during bitwise-exact testing of 
   !           distributed-memory parallel configurations.  
   !
   ! Method:   Indices stowed away during Read_Obs() are used to restore serial 
   !           storage order.  Memory for global objects is allocated here.  
   !           Global objects are minimally allocated to save memory.  
   !           Memory must be deallocated by the caller via a call to 
   !           da_y_facade_free().  
   !           Memory for local object re_slice is deallocated here.  Do not 
   !           use re_slice after this call.  
   !           The "template" argument is needed because some tasks may not 
   !           have any local obs.  
   !
   ! PERFORMANCE NOTE:   This implementation is NOT very efficient.  Speed it 
   !                     up if testing is too slow.  
   !---------------------------------------------------------------------------

   implicit none

   ! task-local objects  (really intent(in   ))
   type (y_facade_type),          intent(inout) :: re_slice      ! residual vector
   type (residual_template_type), intent(in)    :: template  ! allocation info
   ! task-global objects (really intent(  out))
   type (y_facade_type),          intent(inout) :: re_glob_slice ! residual vector

   ! Local declarations
#ifdef DM_PARALLEL
   integer                      :: n, k, serial_n, serial_numvals
   integer                      :: proc
   integer                      :: num_obs_send
   integer                      :: buf_idx
   integer                      :: num_obs_all
   integer                      :: num_recv_all
   integer                      :: obs_global_index(re_slice%num_obs)
   integer                      :: num_values(re_slice%num_obs)
   integer                      :: num_send_buf_lcl
   integer,allocatable          :: num_obs_send_all(:)
   integer,allocatable          :: obs_global_index_all(:)
   integer,allocatable          :: obs_global_index_inv(:)
   integer,allocatable          :: obs_start_all(:)  ! start index of each obs
   integer,pointer              :: num_values_all(:)
   integer,allocatable          :: num_send_buf_all(:)
   integer,allocatable          :: recv_counts(:)
   integer,allocatable          :: recv_displs(:)
   real,allocatable             :: re_vals_lcl(:)
   real,allocatable             :: re_vals_all(:)
   integer                      :: i

   if (trace_use) call da_trace_entry("da_y_facade_to_global")

   ! todo:  later, upgrade from "allgather" to "gather-broadcast"

   ! collect information about all observations
   num_obs_send = 0
   obs_global_index = -1
   do n=1, re_slice%num_obs
      if (re_slice%obs(n)%proc_domain) then
         num_obs_send = num_obs_send + 1
         obs_global_index(num_obs_send) = re_slice%obs(n)%obs_global_index
      end if
   end do
   do n=1, num_obs_send
      if (obs_global_index(n) < 0) then
         call da_error(__FILE__,__LINE__, &
            (/'ASSERTION ERROR:  bad local value of obs_global_index'/))
      end if
   end do

   ! exchange num_obs_send and obs_global_index
   allocate (num_obs_send_all(0:num_procs-1))
   allocate (num_send_buf_all(0:num_procs-1))
   allocate (recv_counts(0:num_procs-1))
   allocate (recv_displs(0:num_procs-1))

   ! gather num_obs_send
   call mpi_allgather( num_obs_send, 1, mpi_integer, num_obs_send_all, 1, &
      mpi_integer, comm, ierr )
   num_obs_all = sum( num_obs_send_all )
   if ( num_obs_all /= re_slice%num_obs_glo ) then
      call da_error (__FILE__,__LINE__, &
         (/'ASSERTION ERROR:  inconsistent count of sound obs'/))
   end if
   ! set up to gather obs_global_index
   recv_counts = num_obs_send_all
   recv_displs(0) = 0
   do proc=1, num_procs-1
      recv_displs(proc) = recv_displs(proc-1) + recv_counts(proc-1)
   end do
   allocate (num_values_all(num_obs_all))
   allocate (obs_global_index_all(num_obs_all))
   allocate (obs_global_index_inv(num_obs_all))
   allocate (obs_start_all(num_obs_all))

   ! gather obs_global_index
   call mpi_allgatherv( obs_global_index, num_obs_send, mpi_integer,    &
      obs_global_index_all, recv_counts, recv_displs, &
      mpi_integer, comm, ierr )

   ! invert obs_global_index_all
   ! initialize to "invalid" value
   obs_global_index_inv = -1
   do n=1, num_obs_all
      if ( ( obs_global_index_all(n) <  1 ) .OR. &
           ( obs_global_index_all(n) > size(obs_global_index_inv) ) ) then
         call da_error (__FILE__,__LINE__, &
            (/'ASSERTION ERROR:  obs_global_index_all(n) out of range'/))
      end if
      if ( obs_global_index_inv(obs_global_index_all(n)) /= -1 ) then
         call da_error (__FILE__,__LINE__, &
            (/'ASSERTION ERROR:  obs owned by more than one task'/))
      end if
      obs_global_index_inv(obs_global_index_all(n)) = n
   end do
   do n=1, num_obs_all
      if ( obs_global_index_inv(obs_global_index_all(n)) == -1 ) then
         call da_error (__FILE__,__LINE__, &
            (/'ASSERTION ERROR:  obs not owned by any task'/))
      end if
   end do

   ! Create re_glob_slice and populate with residual_generic_type objects 
   ! allocated to match template.  
   call da_y_facade_create( re_glob_slice, num_obs_all, num_obs_all, template=template )

   ! NOTE:  This i loop should be inside the n loops.  
   ! Ideally, residual_generic class should know how to pack/unpack 
   ! (serialize/deserialize) itself for arbitrary I/O or communications (MPI or 
   ! otherwise) that clients may choose to implement.  Below are a possible set 
   ! of new methods for residual_generic_type:  
   !  residual_generic_pack_counts( res_generic, (out)rcount, (out)icount )
   !  [client allocates ibuf(icount) and rbuf(rcount)]
   !  residual_generic_pack( res_generic, (inout)rstart, (inout)rbuf, &
   !                         (inout)istart, (inout)ibuf )
   !  [client MPI communications:   ibuf->ibufg  rbuf->rbufg]
   !  residual_generic_unpack_counts( ibufg, (out)rstarts, (out)rcounts )
   !  residual_generic_unpack( (out)res_generic, rstarts(n), rcounts(n), rbufg )
   ! TOdo:  
   !  1) Design serialization for residual_generic_type.  
   !  2) Implement new methods.  
   !  3) Refactor below...  
   !  4) Optimize performance.  
   ! At the moment this refactoring is relatively low-priority since 
   ! da_y_facade_to_global() is already well-encapsulated and peformance is not 
   ! (yet) a concern for testing.  
   ! Loop over vector and scalar elements
   
   do i=template%lbnd,template%ubnd
      num_obs_send = 0
      num_values = 0
      num_send_buf_lcl = 0
      ! collect information to allocate buffers
      do n=1, re_slice%num_obs
         if ( re_slice%obs(n)%proc_domain ) then
            num_obs_send = num_obs_send + 1
            ! track number of scalars/levels per obs element
            num_values(num_obs_send) = size(re_slice%obs(n)%values(i)%ptr)
            ! track total buffer size
            num_send_buf_lcl = num_send_buf_lcl + num_values(num_obs_send)
         end if
      end do
      ! gather num_send_buf_lcl
      call mpi_allgather( num_send_buf_lcl, 1, mpi_integer, &
                          num_send_buf_all, 1,              &
                          mpi_integer, comm, ierr )
      ! gather num_values
      recv_counts = num_obs_send_all
      recv_displs(0) = 0
      do proc=1, num_procs-1
         recv_displs(proc) = recv_displs(proc-1) + recv_counts(proc-1)
      end do
      ! num_values
      call mpi_allgatherv( num_values, num_obs_send, mpi_integer,    &
                           num_values_all, recv_counts, recv_displs, &
                           mpi_integer, comm, ierr )
      ! set up to gather local arrays
      ! compute start index of each obs in gathered buffers
      obs_start_all(1) = 1
      do n=2, num_obs_all
         obs_start_all(n) = obs_start_all(n-1) + num_values_all(n-1)
      end do
      ! finish setting up to gather local arrays
      recv_counts = num_send_buf_all
      recv_displs(0) = 0
      do proc=1, num_procs-1
         recv_displs(proc) = recv_displs(proc-1) + recv_counts(proc-1)
      end do
      num_recv_all = sum( recv_counts )
      ! allocate and initialize send buffer
      allocate( re_vals_lcl(num_send_buf_lcl) )
      buf_idx = 0
      do n=1, re_slice%num_obs
         if ( re_slice%obs(n)%proc_domain ) then
            do k=1, size(re_slice%obs(n)%values(i)%ptr)
               buf_idx = buf_idx + 1
               re_vals_lcl(buf_idx) = re_slice%obs(n)%values(i)%ptr(k)
            end do
         end if
      end do
      if ( buf_idx /= num_send_buf_lcl ) then
         call da_error (__FILE__,__LINE__, &
            (/'ASSERTION ERROR:  buf_idx /= num_send_buf_lcl'/))
      end if
      ! allocate receive buffer
      allocate( re_vals_all(num_recv_all) )
      ! finally, actually gather the values
      call mpi_allgatherv( re_vals_lcl, num_send_buf_lcl, true_mpi_real,   &
                           re_vals_all, recv_counts, recv_displs, &
                           true_mpi_real, comm, ierr )
      ! initialize re_glob_slice
      ! NOTE:  The i loop should be inside this n loop, see comment above...  
      ! TOdo:  Refactor...  
      do n=1, re_glob_slice%num_obs
         serial_n = obs_global_index_inv(n)
         serial_numvals = num_values_all(serial_n)
         buf_idx = obs_start_all(serial_n)
         ! note that we only collected obs someone owns
         re_glob_slice%obs(n)%proc_domain = .true.
         re_glob_slice%obs(n)%obs_global_index = obs_global_index_all(serial_n)
         call da_res_generic_alloc_and_set (re_glob_slice%obs(n), i, &
            re_vals_all(buf_idx:(buf_idx+serial_numvals-1)))
      end do
      ! deallocate communication buffers, etc.
      deallocate (re_vals_all)
      deallocate (re_vals_lcl) 
   end do  ! end of i loop

   call da_y_facade_free (re_slice)

   deallocate (num_values_all, obs_global_index_all, obs_global_index_inv, obs_start_all)
   deallocate (num_obs_send_all, num_send_buf_all, recv_counts, recv_displs)

   if (trace_use) call da_trace_exit("da_y_facade_to_global")

#else
   call da_error (__FILE__,__LINE__, &
      (/'may only be called for a distributed memory parallel run'/))
#endif

end subroutine da_y_facade_to_global


