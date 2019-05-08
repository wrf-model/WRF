subroutine da_residual_chem_acft(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

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
   integer                       :: n, ichem, iplt

   if (trace_use) call da_trace_entry("da_residual_chem_acft")

   n_obs_bad % chem_acft % num = number_type(0, 0, 0)

   do n=1, iv%info(chem_acft)%nlocal
      do ichem = 1, num_chem_acft-1
         do iplt = 1, num_platform
            np_available = np_available + 1
            re%chem_acft(n)%chem(ichem,iplt) = da_residual(n, 0, y%chem_acft(n)%chem(ichem,iplt), iv%chem_acft(n)%chem(ichem,iplt), n_obs_bad % chem_acft) 
         end do
      end do
   end do

   np_missing = np_missing + n_obs_bad % chem_acft % num % miss 
   np_bad_data = np_bad_data + n_obs_bad % chem_acft % num % bad  
   np_obs_used = np_obs_used + n_obs_bad % chem_acft % num % use 

   if (trace_use) call da_trace_exit("da_residual_chem_acft")

end subroutine da_residual_chem_acft
