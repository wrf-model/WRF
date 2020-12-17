subroutine da_get_hr_rain(k, grid, hr_rainc, hr_rainnc, savegridrainc, savegridrainnc)

   !-----------------------------------------------------------------------
   ! Purpose: TBD    
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: k     
   type(domain),     intent(in)    :: grid    ! first guess state.
   real, dimension(ims:ime,jms:jme,1:num_fgat_time), intent(inout) :: hr_rainc, hr_rainnc
   real, dimension(ims:ime,jms:jme), intent(inout) :: savegridrainc, savegridrainnc
   
   if (trace_use) call da_trace_entry("da_get_hr_rain")

#ifdef DM_PARALLEL
#include "HALO_EM_RAIN.inc"
#endif

   if (k .lt. num_fgat_time) then
      !hr_rainc(:,:,k+1)  = hr_rainc (:,:,k+1) - grid%rainc(:,:) 
      !hr_rainnc(:,:,k+1) = hr_rainnc(:,:,k+1) - grid%rainnc(:,:)
      hr_rainc(:,:,k+1)  = savegridrainc(:,:)  - grid%rainc(:,:) 
      hr_rainnc(:,:,k+1) = savegridrainnc(:,:) - grid%rainnc(:,:)
      savegridrainc(:,:)  = grid%rainc(:,:)
      savegridrainnc(:,:) = grid%rainnc(:,:)
   else
      hr_rainc(:,:,k)  = grid%rainc(:,:) 
      hr_rainnc(:,:,k) = grid%rainnc(:,:)
      savegridrainc(:,:)  = grid%rainc(:,:)
      savegridrainnc(:,:) = grid%rainnc(:,:)
   endif

 if (trace_use) call da_trace_exit("da_get_hr_rain")

end subroutine da_get_hr_rain
