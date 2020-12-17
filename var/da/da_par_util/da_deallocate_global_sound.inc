subroutine da_deallocate_global_sound (iv_glob, re_glob, jo_grad_y_glob)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv_glob        ! Innovation vector
   type(y_type),  intent(inout) :: re_glob        ! residual vector
   type(y_type),  intent(inout) :: jo_grad_y_glob ! Grad_y(Jo)

   integer :: n

   if (trace_use_dull) call da_trace_entry("da_deallocate_global_sound")

   deallocate(iv_glob%sound)
   do n=1,size(re_glob%sound)
      deallocate (re_glob%sound(n)%u)
      deallocate (re_glob%sound(n)%v)
      deallocate (re_glob%sound(n)%t)
      deallocate (re_glob%sound(n)%q)
   end do
   deallocate(re_glob%sound)
   do n=1,size(jo_grad_y_glob%sound)
      deallocate (jo_grad_y_glob%sound(n)%u)
      deallocate (jo_grad_y_glob%sound(n)%v)
      deallocate (jo_grad_y_glob%sound(n)%t)
      deallocate (jo_grad_y_glob%sound(n)%q)
   end do
   deallocate(jo_grad_y_glob%sound)

   if (trace_use_dull) call da_trace_exit("da_deallocate_global_sound")

end subroutine da_deallocate_global_sound


