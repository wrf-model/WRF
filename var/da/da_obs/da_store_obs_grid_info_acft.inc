subroutine da_store_obs_grid_info_acft (info)

   !-----------------------------------------------------------------------
   ! Purpose: this is in parallel of da_store_obs_grid_info_rad but with
   !          an extra thinned check to decide proc_domain.
   !-----------------------------------------------------------------------

   implicit none

   type(infa_type), intent(inout) :: info

   integer :: n, iplt

   if (trace_use) call da_trace_entry("da_store_obs_grid_info_acft")

   info%proc_domain(:,:) = .false.

   do n=1,info%nlocal
      do iplt=1,num_platform
         if (info%i(iplt,n) >= its .and. info%i(iplt,n) <= ite .and. info%j(iplt,n) >= jts .and. info%j(iplt,n) <= jte) then
            if ( .not. info%thinned(iplt,n) ) then
               info%proc_domain(iplt,n) = .true.
            end if
         end if
      end do
   end do

   if (trace_use) call da_trace_exit("da_store_obs_grid_info_acft")

end subroutine da_store_obs_grid_info_acft


