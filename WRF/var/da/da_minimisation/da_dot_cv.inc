real function da_dot_cv(cv_size, x, y, grid, mzs, nmzs, jp_start, jp_end)

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
   real,             intent(in) :: x(cv_size)        ! 1st vector.
   real,             intent(in) :: y(cv_size)        ! 1st vector.
   type(domain),     intent(in) :: grid              ! decomposed dimensions
   integer,          intent(in) :: nmzs              ! number of mz
   integer,          intent(in) :: mzs(nmzs)         ! mz for each variable
                                                     ! (to identify 2D arrays)
   integer, optional,intent(in) :: jp_start, jp_end

   logical       :: lvarbc
   real, pointer :: xx(:), yy(:)                     ! Temporary vectors.
   real, pointer :: xg(:), yg(:)                     ! Serial data arrays.
   real          :: dtemp1(1), dtmp                  ! Temporary.

   if (trace_use) call da_trace_entry("da_dot_cv")

   allocate(xx(1:cv_size))
   allocate(yy(1:cv_size))
   xx = x
   yy = y

 ! VarBC parameters are global (no summation across processors)
 !-------------------------------------------------------------
   lvarbc = present(jp_start) .and. present(jp_end)
   if (lvarbc) lvarbc = lvarbc .and. (jp_end >= jp_start)   
   if (lvarbc .and. .not. rootproc) then
      xx(jp_start:jp_end) = 0.
      yy(jp_start:jp_end) = 0.
   end if
      
   ! Bitwise-exact reduction preserves operation order of serial code for
   ! testing, at the cost of much-increased run-time.  Turn it off when not
   ! testing.  This will always be .false. for a serial run or 
   ! one-processor DM_PARALLEL run.

   if (test_dm_exact) then

      ! Collect local cv arrays x and y to globally-sized serially-ordered 
      ! arrays xg and yg.  Note that xg and yg will only exist on the 
      ! monitor task.  

      if (rootproc) then
!         cv_size_domain = cv_size_domain_jb+cv_size_domain_je+cv_size_domain_jp+cv_size_domain_js   
         cv_size_domain = wrf_dm_sum_integer(cv_size)
         allocate(xg(1:cv_size_domain))
         allocate(yg(1:cv_size_domain))
      end if

      call da_cv_to_global(cv_size, cv_size_domain, xx, grid, mzs, nmzs, xg)
      call da_cv_to_global(cv_size, cv_size_domain, yy, grid, mzs, nmzs, yg)

      if (rootproc) then
         dtemp1(1) = da_dot(cv_size_domain, xg, yg)
         deallocate(xg, yg)
      end if

      ! Broadcast result from monitor to other tasks.  
      call wrf_dm_bcast_real(dtemp1, 1)

   else

      dtemp1(1) = da_dot(cv_size, xx, yy)
      
      !if (.not. global) then
         dtmp = dtemp1(1)
         ! summation across processors:
         dtemp1(1) = wrf_dm_sum_real(dtmp)
      !end if

   end if
  
   deallocate(xx,yy)

   da_dot_cv = dtemp1(1)

   if (trace_use) call da_trace_exit("da_dot_cv")

end function da_dot_cv
