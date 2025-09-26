subroutine da_fill_obs_structures_rain(iv, ob)

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

   if (trace_use) call da_trace_entry("da_fill_obs_structures_rain")

   !---------------------------------------------------------------------------
   ! Initialise obs error factors (which will be overwritten in use_obs_errfac)
   !---------------------------------------------------------------------------

   iv % rain_ef_r  = 1.0

   !----------------------------------------------------------------------
   ! [1.0] Allocate innovation vector and observation structures:
   !----------------------------------------------------------------------
   call da_allocate_y_rain(iv, ob)

   !----------------------------------------------------------------------
   ! [2.0] Transfer observations:
   !----------------------------------------------------------------------

   ! [2.20] Transfer rain obs:

   if (iv%info(rain)%nlocal > 0) then
      do n = 1, iv%info(rain)%nlocal
            ob % rain(n) % rain = iv % rain(n) % rain % inv
      end do
   end if

   if (trace_use) call da_trace_exit("da_fill_obs_structures_rain")

end subroutine da_fill_obs_structures_rain


