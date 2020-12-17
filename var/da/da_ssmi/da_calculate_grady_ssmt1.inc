subroutine da_calculate_grady_ssmt1(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(inout) :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_ssmt1")

   do n=1, iv%info(ssmt1)%nlocal
      do k=1, iv%info(ssmt1)%levels(n)
         if (iv%ssmt1(n)%t(k)%qc < obs_qc_pointer) re%ssmt1(n)%t(k) = 0.0
         jo_grad_y%ssmt1(n)%t(k) = -re%ssmt1(n)%t(k) / &
             (iv%ssmt1(n)%t(k)%error * iv%ssmt1(n)%t(k)%error)
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_ssmt1")

end subroutine da_calculate_grady_ssmt1


