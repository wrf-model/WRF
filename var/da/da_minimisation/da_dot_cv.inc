real function da_dot_cv(cv_size, cv_size_domain, x, y, grid, mzs)

   !-----------------------------------------------------------------------
   ! Purpose: Forms the dot product of two vectors that are organized in the 
   ! format of a "cv_type".  
   !
   ! Capable of producing bitwise-exact results for distributed-memory 
   ! parallel runs for testing.  This feature is very slow and consumes 
   ! lots of memory. 
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in) :: cv_size           ! Size of array (tile).
   integer,          intent(in) :: cv_size_domain    ! Size of array (domain).
   real,             intent(in) :: x(cv_size)       ! 1st vector.
   real,             intent(in) :: y(cv_size)       ! 1st vector.
   type(domain),     intent(in) :: grid             ! decomposed dimensions
   integer,          intent(in) :: mzs(:)     ! mz for each variable
                                                     ! (to identify 2D arrays)

   real, pointer :: xg(:), yg(:)      ! Serial data arrays.
   real          :: dtemp1(1), dtemp1x   ! Temporary.

   if (trace_use) call da_trace_entry("da_dot_cv")

   dtemp1(1) = 0.0

   ! Bitwise-exact reduction preserves operation order of serial code for
   ! testing, at the cost of much-increased run-time.  Turn it off when not
   ! testing.  This will always be .false. for a serial run or 
   ! one-processor DM_PARALLEL run.

   if (test_dm_exact) then

      ! Collect local cv arrays x and y to globally-sized serially-ordered 
      ! arrays xg and yg.  Note that xg and yg will only exist on the 
      ! monitor task.  

      if (rootproc) then
         allocate(xg(1:cv_size_domain))
         allocate(yg(1:cv_size_domain))
      end if

      call da_cv_to_global(cv_size, cv_size_domain, x, grid, mzs, xg)
      call da_cv_to_global(cv_size, cv_size_domain, y, grid, mzs, yg)

      if (rootproc) then
         dtemp1(1) = da_dot(cv_size_domain, xg, yg)
         deallocate(xg, yg)
      end if

      ! Broadcast result from monitor to other tasks.  
      call wrf_dm_bcast_real(dtemp1, 1)

   else

      dtemp1(1) = da_dot(cv_size, x, y)

      if (.not. global) then
         dtemp1x = dtemp1(1)
         ! summation across processors:
         dtemp1(1) = wrf_dm_sum_real(dtemp1x)
      end if

   end if

   da_dot_cv = dtemp1(1)

   if (trace_use) call da_trace_exit("da_dot_cv")

end function da_dot_cv


