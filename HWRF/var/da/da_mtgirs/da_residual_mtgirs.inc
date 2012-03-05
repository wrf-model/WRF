subroutine da_residual_mtgirs(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residuals for mtgirs obs
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

   if (trace_use_dull) call da_trace_entry("da_residual_mtgirs")

   n_obs_bad % u % num = number_type(0, 0, 0)
   n_obs_bad % v % num = number_type(0, 0, 0)
   n_obs_bad % t % num = number_type(0, 0, 0)
   n_obs_bad % q % num = number_type(0, 0, 0)

   do n=1, iv%info(mtgirs)%nlocal
      do k=1, iv%info(mtgirs)%levels(n)
         np_available = np_available + 4

         re%mtgirs(n)%u(k) = da_residual(n, k, y%mtgirs(n)%u(k), iv%mtgirs(n)%u(k), n_obs_bad % u)
         re%mtgirs(n)%v(k) = da_residual(n, k, y%mtgirs(n)%v(k), iv%mtgirs(n)%v(k), n_obs_bad % v)
         re%mtgirs(n)%t(k) = da_residual(n, k, y%mtgirs(n)%t(k), iv%mtgirs(n)%t(k), n_obs_bad % t)
         re%mtgirs(n)%q(k) = da_residual(n, k, y%mtgirs(n)%q(k), iv%mtgirs(n)%q(k), n_obs_bad % q)
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

   if (trace_use_dull) call da_trace_exit("da_residual_mtgirs")

end subroutine da_residual_mtgirs


