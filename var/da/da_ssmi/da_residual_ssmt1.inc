subroutine da_residual_ssmt1(iv, y, re, np_missing, np_bad_data,np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv     ! Innovation vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(inout) :: re     ! Residual structure.

   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   type (bad_data_type) :: n_obs_bad
   integer              :: n, k

   if (trace_use_dull) call da_trace_entry("da_residual_ssmt1")

   n_obs_bad % t % num = number_type(0, 0, 0)

   do n=1, iv%info(ssmt1)%nlocal
      do k=1, iv%info(ssmt1)%levels(n)
         np_available = np_available + 1
         re%ssmt1(n)%t(k) = da_residual(n, k, y%ssmt1(n)%t(k), iv%ssmt1(n)%t(k), n_obs_bad % t)
      end do
   end do

   np_missing  = np_missing  + n_obs_bad % t % num % miss
   np_bad_data = np_bad_data + n_obs_bad % t % num % bad
   np_obs_used = np_obs_used + n_obs_bad % t % num % use

   if (trace_use_dull) call da_trace_exit("da_residual_ssmt1")

end subroutine da_residual_ssmt1


