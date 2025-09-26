subroutine da_residual_ssmi_rv(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

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

   if (trace_use) call da_trace_entry("da_residual_ssmi_rv")

   n_obs_bad % Speed % num = number_type(0, 0, 0)
   n_obs_bad % q % num     = number_type(0, 0, 0)

   do n=1, iv%info(ssmi_rv)%nlocal
      np_available = np_available + 2

      re%ssmi_rv(n)%Speed = da_residual(n, 0, y%ssmi_rv(n)%Speed, &
         iv%ssmi_rv(n)%Speed, n_obs_bad % Speed)
      re%ssmi_rv(n)%tpw   = da_residual(n, 0, y%ssmi_rv(n)%tpw,   &
         iv%ssmi_rv(n)%tpw, n_obs_bad % q      )
   end do

   np_missing  = np_missing + n_obs_bad % Speed % num % miss + n_obs_bad % q % num % miss
   np_bad_data = np_bad_data + n_obs_bad % Speed % num % bad + n_obs_bad % q % num % bad
   np_obs_used = np_obs_used + n_obs_bad % Speed % num % use + n_obs_bad % q % num % use

   if (trace_use) call da_trace_exit("da_residual_ssmi_rv")

end subroutine da_residual_ssmi_rv


