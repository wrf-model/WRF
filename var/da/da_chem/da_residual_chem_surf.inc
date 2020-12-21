subroutine da_residual_chem_surf(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

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
   integer                       :: n, ichem

   if (trace_use) call da_trace_entry("da_residual_chem_surf")

   n_obs_bad % chem_surf % num = number_type(0, 0, 0)

   do n=1, iv%info(chem_surf)%nlocal
      do ichem = 1, num_chem_surf-1
         np_available = np_available + 1
         re%chem_surf(n)%chem(ichem) = da_residual(n, 0, y%chem_surf(n)%chem(ichem), iv%chem_surf(n)%chem(ichem), n_obs_bad % chem_surf) 
      end do
   end do

   np_missing = np_missing + n_obs_bad % chem_surf % num % miss 
   np_bad_data = np_bad_data + n_obs_bad % chem_surf % num % bad  
   np_obs_used = np_obs_used + n_obs_bad % chem_surf % num % use 

   if (trace_use) call da_trace_exit("da_residual_chem_surf")

end subroutine da_residual_chem_surf
