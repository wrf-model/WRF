subroutine da_calculate_grady_ssmt2(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(inout) :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_ssmt2")

   do n=1, iv%info(ssmt2)%nlocal
      do k=1, iv%info(ssmt2)%levels(n)
         if (iv%ssmt2(n)%rh(k)%qc < obs_qc_pointer) re%ssmt2(n)%rh(k) = 0.0
         jo_grad_y%ssmt2(n)%rh(k) = -re%ssmt2(n)%rh(k) / &
            (iv%ssmt2(n)%rh(k)%error * iv%ssmt2(n)%rh(k)%error)
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_ssmt2")

end subroutine da_calculate_grady_ssmt2


