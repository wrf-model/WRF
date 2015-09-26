subroutine da_residual_tamdar_sfc(iv, y, re,np_missing, np_bad_data,np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residuals for tamdar surface obs
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv     ! Innovation vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(inout) :: re     ! Residual vector (O-A).

   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   type (bad_data_type)  :: n_obs_bad
   integer               :: n

   if (trace_use_dull) call da_trace_entry("da_residual_tamdar_sfc")

   n_obs_bad % u % num = number_type(0, 0, 0)
   n_obs_bad % v % num = number_type(0, 0, 0)
   n_obs_bad % t % num = number_type(0, 0, 0)
   n_obs_bad % p % num = number_type(0, 0, 0)
   n_obs_bad % q % num = number_type(0, 0, 0)

   do n=1, iv%info(tamdar_sfc)%nlocal
      np_available = np_available + 5

      re%tamdar_sfc(n)%u = da_residual(n, 0, y%tamdar_sfc(n)%u, iv%tamdar_sfc(n)%u, n_obs_bad % u)
      re%tamdar_sfc(n)%v = da_residual(n, 0, y%tamdar_sfc(n)%v, iv%tamdar_sfc(n)%v, n_obs_bad % v)
      re%tamdar_sfc(n)%t = da_residual(n, 0, y%tamdar_sfc(n)%t, iv%tamdar_sfc(n)%t, n_obs_bad % t)
      re%tamdar_sfc(n)%p = da_residual(n, 0, y%tamdar_sfc(n)%p, iv%tamdar_sfc(n)%p, n_obs_bad % p)
      re%tamdar_sfc(n)%q = da_residual(n, 0, y%tamdar_sfc(n)%q, iv%tamdar_sfc(n)%q, n_obs_bad % q)
   end do

   np_missing = np_missing + n_obs_bad % u % num % miss + &
      n_obs_bad % v % num % miss + n_obs_bad % t % num % miss + &
      n_obs_bad % p % num % miss + n_obs_bad % q % num % miss
   np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
      n_obs_bad % v % num % bad + n_obs_bad % t % num % bad + &
      n_obs_bad % p % num % bad + n_obs_bad % q % num % bad
   np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
      n_obs_bad % v % num % use + n_obs_bad % t % num % use + &
      n_obs_bad % p % num % use + n_obs_bad % q % num % use

   if (trace_use_dull) call da_trace_exit("da_residual_tamdar_sfc")

end subroutine da_residual_tamdar_sfc


