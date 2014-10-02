subroutine da_store_obs_grid_info (info)

   !-----------------------------------------------------------------------
   ! Purpose: this is in parallel of da_store_obs_grid_info_rad but with
   !          an extra thinned check to decide proc_domain.
   !-----------------------------------------------------------------------

   implicit none

   type(infa_type), intent(inout) :: info

   integer :: n

   if (trace_use) call da_trace_entry("da_store_obs_grid_info")

   info%proc_domain(:,:) = .false.

   do n=1,info%nlocal
      if (info%i(1,n) >= its .and. info%i(1,n) <= ite .and. info%j(1,n) >= jts .and. info%j(1,n) <= jte) then
         if ( .not. info%thinned(1,n) ) then
            info%proc_domain(:,n) = .true.
         end if
      end if
   end do

   if (trace_use) call da_trace_exit("da_store_obs_grid_info")

end subroutine da_store_obs_grid_info


