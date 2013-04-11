subroutine da_residual_airsr(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !----------------------------------------------------------------------
   ! Purpose: Calculates residual at AIRS retrieval locations                   
   !----------------------------------------------------------------------

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

   if (trace_use_dull) call da_trace_entry("da_residual_airsr")

   n_obs_bad % t % num = number_type(0, 0, 0)
   n_obs_bad % q % num = number_type(0, 0, 0)

   do n=1, iv%info(airsr)%nlocal
      do k=1, iv%info(airsr)%levels(n)
        np_available = np_available + 4
        re%airsr(n)%t(k) = da_residual(n, k, y%airsr(n)%t(k), iv%airsr(n)%t(k), n_obs_bad % t)
        re%airsr(n)%q(k) = da_residual(n, k, y%airsr(n)%q(k), iv%airsr(n)%q(k), n_obs_bad % q)
      end do
   end do

   np_missing  = np_missing  + n_obs_bad % t % num % miss + n_obs_bad % q % num % miss
   np_bad_data = np_bad_data + n_obs_bad % t % num % bad  + n_obs_bad % q % num % bad
   np_obs_used = np_obs_used + n_obs_bad % t % num % use  + n_obs_bad % q % num % use

   if (trace_use_dull) call da_trace_exit("da_residual_airsr")

end subroutine da_residual_airsr


