subroutine da_residual_tamdar(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residuals for tamdar obs
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv     ! Innovation vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(inout) :: re     ! Residual structure.

   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n, k

   if (trace_use_dull) call da_trace_entry("da_residual_tamdar")

   n_obs_bad % u % num = number_type(0, 0, 0)
   n_obs_bad % v % num = number_type(0, 0, 0)
   n_obs_bad % t % num = number_type(0, 0, 0)
   n_obs_bad % q % num = number_type(0, 0, 0)

   do n=1, iv%info(tamdar)%nlocal
      do k=1, iv%info(tamdar)%levels(n)
         np_available = np_available + 4

         re%tamdar(n)%u(k) = da_residual(n, k, y%tamdar(n)%u(k), iv%tamdar(n)%u(k), n_obs_bad % u)
         re%tamdar(n)%v(k) = da_residual(n, k, y%tamdar(n)%v(k), iv%tamdar(n)%v(k), n_obs_bad % v)
         re%tamdar(n)%t(k) = da_residual(n, k, y%tamdar(n)%t(k), iv%tamdar(n)%t(k), n_obs_bad % t)
         re%tamdar(n)%q(k) = da_residual(n, k, y%tamdar(n)%q(k), iv%tamdar(n)%q(k), n_obs_bad % q)
      end do
   end do

   np_missing = np_missing + n_obs_bad % u % num % miss + &
      n_obs_bad % v % num % miss + n_obs_bad % t % num % miss + &
      n_obs_bad % q % num % miss
   np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
      n_obs_bad % v % num % bad + n_obs_bad % t % num % bad + &
      n_obs_bad % q % num % bad
   np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
      n_obs_bad % v % num % use + n_obs_bad % t % num % use + &
      n_obs_bad % q % num % use

   if (trace_use_dull) call da_trace_exit("da_residual_tamdar")

end subroutine da_residual_tamdar


