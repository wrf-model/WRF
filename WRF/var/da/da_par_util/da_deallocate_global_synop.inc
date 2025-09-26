subroutine da_deallocate_global_synop(iv_glob, re_glob, jo_grad_y_glob)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout) :: iv_glob        ! Innovation vector
   type (y_type),  intent(inout) :: re_glob        ! residual vector
   type (y_type),  intent(inout) :: jo_grad_y_glob ! Grad_y(Jo)

   if (trace_use_dull) call da_trace_entry("da_deallocate_global_synop")

   deallocate(iv_glob%synop)
   deallocate(re_glob%synop)
   deallocate(jo_grad_y_glob%synop)

   if (trace_use_dull) call da_trace_exit("da_deallocate_global_synop")

end subroutine da_deallocate_global_synop


