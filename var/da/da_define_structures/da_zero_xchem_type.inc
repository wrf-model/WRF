subroutine da_zero_xchem_type( xchem )

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (xchem_type), intent(inout) :: xchem
   integer :: ic

   if (trace_use_dull) call da_trace_entry("da_zero_xchem_type")

!!!   do ic = 1, num_chem-1
   do ic = PARAM_FIRST_SCALAR, num_chem
      xchem%chem_ic(:,:,:,ic) = 0.D0
   end do

   if (trace_use_dull) call da_trace_exit("da_zero_xchem_type")

end subroutine da_zero_xchem_type


