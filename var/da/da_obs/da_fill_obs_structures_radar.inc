subroutine da_fill_obs_structures_radar(iv, ob)

   !----------------------------------------------------------------------------   
   ! Purpose: Allocates observation structure and fills it from iv.
   !----------------------------------------------------------------------------   

   implicit none

   type (iv_type), intent(inout) :: iv   ! Obs and header structure.
   type (y_type), intent(out)    :: ob   ! (Smaller) observation structure.

   integer :: n, k     ! Loop counters.
   real    :: rh_error ! RH obs. error.
   real    :: q_error  ! q obs. error.
   real    :: geometric_h, geopotential_h
   integer :: i,j
   logical :: outside

   if (trace_use) call da_trace_entry("da_fill_obs_structures_radar")

   !---------------------------------------------------------------------------
   ! Initialise obs error factors (which will be overwritten in use_obs_errfac)
   !---------------------------------------------------------------------------

   iv % radar_ef_rv = 1.0
   iv % radar_ef_rf = 1.0

   !----------------------------------------------------------------------
   ! [1.0] Allocate innovation vector and observation structures:
   !----------------------------------------------------------------------
   call da_allocate_y_radar(iv, ob)

   !----------------------------------------------------------------------
   ! [2.0] Transfer observations:
   !----------------------------------------------------------------------

   ! [2.18] Transfer radar obs:

   if (iv%info(radar)%nlocal > 0) then
      do n = 1, iv%info(radar)%nlocal
         do k = 1, iv%info(radar)%levels(n)
            ! Copy observation variables:
            ob % radar(n) % rv(k) = iv % radar(n) % rv(k) % inv
            ob % radar(n) % rf(k) = iv % radar(n) % rf(k) % inv
         end do
      end do
   end if

   if (trace_use) call da_trace_exit("da_fill_obs_structures_radar")

end subroutine da_fill_obs_structures_radar


