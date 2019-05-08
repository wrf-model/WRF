subroutine da_zero_xch_type( xch )

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (xch_type), intent(inout) :: xch
   integer :: ic

   if (trace_use_dull) call da_trace_entry("da_zero_xch_type")

   do ic = 1, num_scaleant
      xch%alpha_ant(:,:,:,ic) = 0.D0
   end do

   do ic = 1, num_scalebb
      xch%alpha_bb(:,:,:,ic) = 0.D0
   end do

   if (trace_use_dull) call da_trace_exit("da_zero_xch_type")

end subroutine da_zero_xch_type


