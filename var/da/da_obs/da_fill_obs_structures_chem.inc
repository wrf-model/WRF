subroutine da_fill_obs_structures_chem(iv, ob)

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

   if (trace_use) call da_trace_entry("da_fill_obs_structures_chem")

   !---------------------------------------------------------------------------
   ! Initialise obs error factors (which will be overwritten in use_obs_errfac)
   !---------------------------------------------------------------------------

   iv % chem_surf_ef  = 1.0
   iv % chem_acft_ef  = 1.0

   !----------------------------------------------------------------------
   ! [1.0] Allocate innovation vector and observation structures:
   !----------------------------------------------------------------------
   call da_allocate_y_chem(iv, ob)

   !----------------------------------------------------------------------
   ! [2.0] Transfer observations:
   !----------------------------------------------------------------------

   ! [2.20] Transfer chem obs:

   if (iv%info(chem_surf)%nlocal > 0) then
      do n = 1, iv%info(chem_surf)%nlocal
         do k = 1, num_chem_surf-1
            ob % chem_surf(n) % chem(k) = iv % chem_surf(n) % chem(k) % inv
         end do
      end do
   end if

   if (iv%info(chem_acft)%nlocal > 0) then
      do n = 1, iv%info(chem_acft)%nlocal
         do k = 1, num_chem_acft-1
            do iplt = 1, num_platform
               ob % chem_acft(n) % chem(k,iplt) = iv % chem_acft(n) % chem(k,iplt) % inv
            end do
         end do
      end do
   end if

   if (trace_use) call da_trace_exit("da_fill_obs_structures_chem")

end subroutine da_fill_obs_structures_chem


