subroutine da_residual_rad(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !---------------------------------------------------------------------------
   ! Purpose: Calculate Obs Residual and counting obs number.
   !
   ! Method:  re = (d - H delta_x)
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv     ! Innovation vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(inout) :: re     ! Residual vector (O-A).

   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: i

   if (trace_use) call da_trace_entry("da_residual_rad")

   do i = 1, iv%num_inst
      if (iv%instid(i)%num_rad < 1) cycle

      n_obs_bad % rad % num = number_type(0, 0, 0)
      call da_residual_new(y%instid(i)%tb(:,:), iv%instid(i)%tb_qc(:,:), &
         iv%instid(i)%tb_inv(:,:), re%instid(i)%tb(:,:))
      np_available = iv%instid(i)%nchan*iv%instid(i)%num_rad
   end do

   np_missing  = np_missing  + n_obs_bad % rad % num % miss
   np_bad_data = np_bad_data + n_obs_bad % rad % num % bad
   np_obs_used = np_obs_used + n_obs_bad % rad % num % use 

   if (trace_use) call da_trace_exit("da_residual_rad")
   
end subroutine da_residual_rad


