subroutine da_residual_ships(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residuals for ship obs
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
   integer              :: n

   if (trace_use_dull) call da_trace_entry("da_residual_ships")

   n_obs_bad % u % num = number_type(0, 0, 0)
   n_obs_bad % v % num = number_type(0, 0, 0)
   n_obs_bad % t % num = number_type(0, 0, 0)
   n_obs_bad % p % num = number_type(0, 0, 0)
   n_obs_bad % q % num = number_type(0, 0, 0)

   do n=1, iv%info(ships)%nlocal
      np_available = np_available + 5

      re%ships(n)%u = da_residual(n, 0, y%ships(n)%u, iv%ships(n)%u, n_obs_bad % u)
      re%ships(n)%v = da_residual(n, 0, y%ships(n)%v, iv%ships(n)%v, n_obs_bad % v)
      re%ships(n)%t = da_residual(n, 0, y%ships(n)%t, iv%ships(n)%t, n_obs_bad % t)
      re%ships(n)%p = da_residual(n, 0, y%ships(n)%p, iv%ships(n)%p, n_obs_bad % p)
      re%ships(n)%q = da_residual(n, 0, y%ships(n)%q, iv%ships(n)%q, n_obs_bad % q)
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

   if (trace_use_dull) call da_trace_exit("da_residual_ships")

end subroutine da_residual_ships


