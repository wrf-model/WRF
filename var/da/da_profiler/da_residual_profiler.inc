subroutine da_residual_profiler(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residuals for profilers
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv     ! Innovation vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(inout) :: re     ! Residual vector (O-A).

   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   type (bad_data_type) :: n_obs_bad
   integer              :: n, k

   if (trace_use_dull) call da_trace_entry("da_residual_profiler")

   n_obs_bad % u % num = number_type(0, 0, 0)
   n_obs_bad % v % num = number_type(0, 0, 0)

   do n=1, iv%info(profiler)%nlocal
      do k=1, iv%info(profiler)%levels(n)
         np_available = np_available + 2
         re%profiler(n)%u(k) = da_residual(n, k, y%profiler(n)%u(k), iv%profiler(n)%u(k), n_obs_bad % u)
         re%profiler(n)%v(k) = da_residual(n, k, y%profiler(n)%v(k), iv%profiler(n)%v(k), n_obs_bad % v)
      end do
   end do

   np_missing  = np_missing  + n_obs_bad % u % num % miss + n_obs_bad % v % num % miss
   np_bad_data = np_bad_data + n_obs_bad % u % num % bad  + n_obs_bad % v % num % bad 
   np_obs_used = np_obs_used + n_obs_bad % u % num % use  + n_obs_bad % v % num % use

   if (trace_use_dull) call da_trace_exit("da_residual_profiler")

end subroutine da_residual_profiler


