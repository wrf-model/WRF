subroutine da_residual_ssmi_tb(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv     ! Obs increment vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(inout) :: re     ! Residual structure.

   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   type (bad_data_type) :: n_obs_bad
   integer              :: n

   if (trace_use) call da_trace_entry("da_residual_ssmi_tb")

   n_obs_bad % t % num = number_type(0, 0, 0)

   do n=1, iv%info(ssmi_tb)%nlocal
      np_available = np_available + 7

      re%ssmi_tb(n)%tb19v = da_residual(n, 0, y%ssmi_tb(n)%tb19v, iv%ssmi_tb(n)%tb19v, n_obs_bad % t)
      re%ssmi_tb(n)%tb19h = da_residual(n, 0, y%ssmi_tb(n)%tb19h, iv%ssmi_tb(n)%tb19h, n_obs_bad % t)
      re%ssmi_tb(n)%tb22v = da_residual(n, 0, y%ssmi_tb(n)%tb22v, iv%ssmi_tb(n)%tb22v, n_obs_bad % t)
      re%ssmi_tb(n)%tb37v = da_residual(n, 0, y%ssmi_tb(n)%tb37v, iv%ssmi_tb(n)%tb37v, n_obs_bad % t)
      re%ssmi_tb(n)%tb37h = da_residual(n, 0, y%ssmi_tb(n)%tb37h, iv%ssmi_tb(n)%tb37h, n_obs_bad % t)
      re%ssmi_tb(n)%tb85v = da_residual(n, 0, y%ssmi_tb(n)%tb85v, iv%ssmi_tb(n)%tb85v, n_obs_bad % t)
      re%ssmi_tb(n)%tb85h = da_residual(n, 0, y%ssmi_tb(n)%tb85h, iv%ssmi_tb(n)%tb85h, n_obs_bad % t)
   end do

   np_missing  = np_missing  + n_obs_bad % t % num % miss  
   np_bad_data = np_bad_data + n_obs_bad % t % num % bad  
   np_obs_used = np_obs_used + n_obs_bad % t % num % use    

   if (trace_use) call da_trace_exit("da_residual_ssmi_tb") 

end subroutine da_residual_ssmi_tb


