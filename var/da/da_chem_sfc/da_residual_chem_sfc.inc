subroutine da_residual_chem_sfc(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

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

   if (trace_use) call da_trace_entry("da_residual_chem_sfc")

   n_obs_bad % chemic_surf % num = number_type(0, 0, 0)


   allocate ( re%chemic_surf(iv%info(chemic_surf)%nlocal) )

   do n=1, iv%info(chemic_surf)%nlocal
      allocate ( re%chemic_surf(n)%chem(num_chemic_surf))
      do ichem = PARAM_FIRST_SCALAR, num_chemic_surf
         np_available = np_available + 1
         re%chemic_surf(n)%chem(ichem) = da_residual(n, 0, y%chemic_surf(n)%chem(ichem), &
                                         iv%chemic_surf(n)%chem(ichem), n_obs_bad % chemic_surf)
      end do
   end do

   np_missing = np_missing + n_obs_bad % chemic_surf % num % miss 
   np_bad_data = np_bad_data + n_obs_bad % chemic_surf % num % bad  
   np_obs_used = np_obs_used + n_obs_bad % chemic_surf % num % use 

   if (trace_use) call da_trace_exit("da_residual_chem_sfc")

end subroutine da_residual_chem_sfc
