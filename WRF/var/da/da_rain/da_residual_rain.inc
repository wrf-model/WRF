subroutine da_residual_rain(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

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

   type (bad_data_type)          :: n_obs_bad
   integer                       :: n

   if (trace_use) call da_trace_entry("da_residual_rain")

   n_obs_bad % rain % num = number_type(0, 0, 0)

   do n=1, iv%info(rain)%nlocal
      np_available = np_available + 1
      re%rain(n)%rain = da_residual(n, 0, y%rain(n)%rain, iv%rain(n)%rain, n_obs_bad % rain) 
   end do

   np_missing = np_missing + n_obs_bad % rain % num % miss 
   np_bad_data = np_bad_data + n_obs_bad % rain % num % bad  
   np_obs_used = np_obs_used + n_obs_bad % rain % num % use 

   if (trace_use) call da_trace_exit("da_residual_rain")

end subroutine da_residual_rain


