subroutine da_allocate_observations_chem_sfc (iv)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate components of observation structure.
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout) :: iv     ! Observation structure.

   integer :: i

   if (trace_use) call da_trace_entry("da_allocate_observations_chem_sfc")

   if (iv%info(chemic_surf)%nlocal > 0) allocate(iv%chemic_surf(1:iv%info(chemic_surf)%nlocal))

   call da_allocate_obs_info(iv,chemic_surf)

   if (trace_use) call da_trace_exit("da_allocate_observations_chem_sfc")

end subroutine da_allocate_observations_chem_sfc


