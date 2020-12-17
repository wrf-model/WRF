subroutine da_jo_and_grady_ssmt2(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(in)    :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo          ! Obs cost function.

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_ssmt2")

   jo % ssmt2_rh = 0.0
   
   do n=1, iv%info(ssmt2)%nlocal
      do k=1, iv%info(ssmt2)%levels(n)
         jo_grad_y%ssmt2(n)%rh(k) = -re%ssmt2(n)%rh(k) / &
            (iv%ssmt2(n)%rh(k)%error * iv%ssmt2(n)%rh(k)%error)

         jo % ssmt2_rh = jo % ssmt2_rh - re%ssmt2(n)%rh(k) * jo_grad_y%ssmt2(n)%rh(k)
      end do
   end do

   jo % ssmt2_rh = 0.5 * jo % ssmt2_rh

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_ssmt2")

end subroutine da_jo_and_grady_ssmt2


