subroutine da_residual_lightning(iv, y, re,np_missing, np_bad_data,np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residuals for lightning obs
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP)   
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
   integer              :: n, k

   if (trace_use_dull) call da_trace_entry("da_residual_lightning")

   n_obs_bad%w%num   = number_type(0, 0, 0)
   n_obs_bad%q%num   = number_type(0, 0, 0)
   n_obs_bad%div%num = number_type(0, 0, 0)   

   do n = 1, iv%info(lightning)%nlocal
       do k = 1, iv%info(lightning)%levels(n)

          if(use_lightning_w) then
             np_available = np_available + 1
             re%lightning(n)%w(k) = da_residual(n, k, y%lightning(n)%w(k), iv%lightning(n)%w(k), n_obs_bad % w) 
          end if

	  if(use_lightning_div) then
             np_available = np_available + 1
             re%lightning(n)%div(k) = da_residual(n, k, y%lightning(n)%div(k), iv%lightning(n)%div(k), n_obs_bad % div)		 
          end if
		  
	  if(use_lightning_qv) then
             np_available = np_available + 1
             re%lightning(n)%qv(k) = da_residual(n, k, y%lightning(n)%qv(k), iv%lightning(n)%qv(k), n_obs_bad % q)
          end if		  
       end do
   end do

   np_missing  = np_missing  + n_obs_bad%w%num%miss + n_obs_bad%div%num%miss + n_obs_bad%q%num%miss
   np_bad_data = np_bad_data + n_obs_bad%w%num%bad  + n_obs_bad%div%num%bad  + n_obs_bad%q%num%bad
   np_obs_used = np_obs_used + n_obs_bad%w%num%use  + n_obs_bad%div%num%use  + n_obs_bad%q%num%use
   
   if (trace_use_dull) call da_trace_exit("da_residual_lightning")

end subroutine da_residual_lightning
