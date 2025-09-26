subroutine da_jo_and_grady_ssmt1(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(in)    :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo          ! Obs cost function.

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_ssmt1")

   jo % ssmt1_t = 0.0

   do n=1, iv%info(ssmt1)%nlocal
      do k=1, iv%info(ssmt1)%levels(n)
         jo_grad_y%ssmt1(n)%t(k) = -re%ssmt1(n)%t(k) / (iv%ssmt1(n)%t(k)%error * iv%ssmt1(n)%t(k)%error)

         jo % ssmt1_t = jo % ssmt1_t - re%ssmt1(n)%t(k) * jo_grad_y%ssmt1(n)%t(k)
      end do
   end do

   jo % ssmt1_t = 0.5 * jo % ssmt1_t

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_ssmt1")

end subroutine da_jo_and_grady_ssmt1


