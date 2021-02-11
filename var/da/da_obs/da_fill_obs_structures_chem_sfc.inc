subroutine da_fill_obs_structures_chem_sfc(iv, ob)

   !----------------------------------------------------------------------------   
   ! Purpose: Allocates observation structure and fills it from iv.
   !----------------------------------------------------------------------------   

   implicit none

   type (iv_type), intent(inout) :: iv   ! Obs and header structure.
   type (y_type), intent(out)    :: ob   ! (Smaller) observation structure.

   integer :: n, k, iplt     ! Loop counters.
!   real    :: rh_error ! RH obs. error.
!   real    :: q_error  ! q obs. error.
!   real    :: geometric_h, geopotential_h
!   integer :: i,j
!   logical :: outside

   if (trace_use) call da_trace_entry("da_fill_obs_structures_chem_sfc")

   !---------------------------------------------------------------------------
   ! Initialise obs error factors (which will be overwritten in use_obs_errfac)
   !---------------------------------------------------------------------------

   iv % chemic_surf_ef  = 1.0

   !----------------------------------------------------------------------
   ! [1.0] Allocate innovation vector and observation structures:
   !----------------------------------------------------------------------
   call da_allocate_y_chem_sfc(iv, ob)

   !----------------------------------------------------------------------
   ! [2.0] Transfer observations:
   !----------------------------------------------------------------------

   ! [2.20] Transfer chemic obs:


   if (iv%info(chemic_surf)%nlocal > 0) then
      do n = 1, iv%info(chemic_surf)%nlocal
         do k = PARAM_FIRST_SCALAR, num_chemic_surf
            ob % chemic_surf(n) % chem(k) = iv % chemic_surf(n) % chem(k) % inv
         end do
      end do
   end if

   if (trace_use) call da_trace_exit("da_fill_obs_structures_chem_sfc")

end subroutine da_fill_obs_structures_chem_sfc


