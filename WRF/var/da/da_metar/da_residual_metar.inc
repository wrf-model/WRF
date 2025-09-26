subroutine da_residual_metar(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !----------------------------------------------------------------------
   ! Purpose: Calculate residuals for metars
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
   integer                           :: n

   if (trace_use_dull) call da_trace_entry("da_residual_metar")

   n_obs_bad % u % num = number_type(0, 0, 0)
   n_obs_bad % v % num = number_type(0, 0, 0)
   n_obs_bad % t % num = number_type(0, 0, 0)
   n_obs_bad % p % num = number_type(0, 0, 0)
   n_obs_bad % q % num = number_type(0, 0, 0)

   do n=1, iv%info(metar)%nlocal
      np_available = np_available + 5
      re%metar(n)%u = da_residual(n, 0, y%metar(n)%u, iv%metar(n)%u, n_obs_bad % u)
      re%metar(n)%v = da_residual(n, 0, y%metar(n)%v, iv%metar(n)%v, n_obs_bad % v)
      re%metar(n)%t = da_residual(n, 0, y%metar(n)%t, iv%metar(n)%t, n_obs_bad % t)
      re%metar(n)%p = da_residual(n, 0, y%metar(n)%p, iv%metar(n)%p, n_obs_bad % p)
      re%metar(n)%q = da_residual(n, 0, y%metar(n)%q, iv%metar(n)%q, n_obs_bad % q)
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

   if (trace_use_dull) call da_trace_exit("da_residual_metar")

end subroutine da_residual_metar


