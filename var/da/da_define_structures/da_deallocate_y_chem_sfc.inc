subroutine da_deallocate_y_chem_sfc (iv, y)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate arrays used in y and residual obs structures.
   !---------------------------------------------------------------------------

   implicit none
   
   type (iv_type), intent(in)    :: iv      ! Ob type input.
   type (y_type),  intent(inout) :: y       ! Residual type structure.
   integer                       :: ilocal

   !---------------------------------------------------------------------------
   !  [1.0] Copy number of observations:
   !---------------------------------------------------------------------------

   if (trace_use) call da_trace_entry("da_deallocate_y_chem_sfc")

   y % nlocal(chemic_surf) = iv%info(chemic_surf)%nlocal
   y % ntotal(chemic_surf) = iv%info(chemic_surf)%ntotal

  !---------------------------------------------------------------------------
  ! [2.0] Allocate:
  !---------------------------------------------------------------------------

   if (y % nlocal(chemic_surf) > 0) then
      deallocate (y % chemic_surf)
   end if

   if (trace_use) call da_trace_exit("da_deallocate_y_chem_sfc")

end subroutine da_deallocate_y_chem_sfc


