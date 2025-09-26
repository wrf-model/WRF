subroutine da_residual_airep(iv, y, re, np_missing, np_bad_data,np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv     ! Innovation vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(inout) :: re     ! Residual vector (O-A).

   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n, k

   if (trace_use_dull) call da_trace_entry("da_residual_airep")

   n_obs_bad % u % num = number_type(0, 0, 0)
   n_obs_bad % v % num = number_type(0, 0, 0)
   n_obs_bad % t % num = number_type(0, 0, 0)
   n_obs_bad % q % num = number_type(0, 0, 0)

   do n=1, iv%info(airep)%nlocal
      do k=1, iv%info(airep)%levels(n)
         np_available = np_available + 4
         re%airep(n)%u(k) = da_residual(n, k, y%airep(n)%u(k), iv%airep(n)%u(k), n_obs_bad % u)
         re%airep(n)%v(k) = da_residual(n, k, y%airep(n)%v(k), iv%airep(n)%v(k), n_obs_bad % v)
         re%airep(n)%t(k) = da_residual(n, k, y%airep(n)%t(k), iv%airep(n)%t(k), n_obs_bad % t)
         re%airep(n)%q(k) = da_residual(n, k, y%airep(n)%q(k), iv%airep(n)%q(k), n_obs_bad % q)
      end do
   end do

   np_missing = np_missing + n_obs_bad % u % num % miss + &
      n_obs_bad % v % num % miss + n_obs_bad % t % num % miss + n_obs_bad % q % num % miss
   np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
      n_obs_bad % v % num % bad + n_obs_bad % t % num % bad + n_obs_bad % q % num % bad
   np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
      n_obs_bad % v % num % use + n_obs_bad % t % num % use + n_obs_bad % q % num % use

   if (trace_use_dull) call da_trace_exit("da_residual_airep")

end subroutine da_residual_airep


