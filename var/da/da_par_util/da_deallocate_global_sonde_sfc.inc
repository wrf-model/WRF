subroutine da_deallocate_global_sonde_sfc(iv_glob, re_glob, jo_grad_y_glob)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout) :: iv_glob        ! Innovation vector
   type (y_type),  intent(inout) :: re_glob        ! residual vector
   type (y_type),  intent(inout) :: jo_grad_y_glob ! Grad_y(Jo)

   if (trace_use_dull) call da_trace_entry("da_deallocate_global_sonde_sfc")

   deallocate(iv_glob%sonde_sfc)
   deallocate(re_glob%sonde_sfc)
   deallocate(jo_grad_y_glob%sonde_sfc)

   if (trace_use_dull) call da_trace_exit("da_deallocate_global_sonde_sfc")
   
end subroutine da_deallocate_global_sonde_sfc


