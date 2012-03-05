subroutine da_residual_ssmt2(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

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

   if (trace_use_dull) call da_trace_entry("da_residual_ssmt2")

   n_obs_bad % rh % num = number_type(0, 0, 0)

   do n=1, iv%info(ssmt2)%nlocal
      do k=1, iv%info(ssmt2)%levels(n)
         np_available = np_available + 1
         re%ssmt2(n)%rh(k) = da_residual(n, k, y%ssmt2(n)%rh(k), iv%ssmt2(n)%rh(k), n_obs_bad % rh)
      end do
   end do

   np_missing  = np_missing  + n_obs_bad % rh % num % miss
   np_bad_data = np_bad_data + n_obs_bad % rh % num % bad
   np_obs_used = np_obs_used + n_obs_bad % rh % num % use

   if (trace_use_dull) call da_trace_exit("da_residual_ssmt2")

end subroutine da_residual_ssmt2


