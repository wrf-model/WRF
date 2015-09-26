subroutine da_calculate_grady_airsr(iv, re, jo_grad_y)

   !----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on AIRS re-vector             
   !----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer :: n, k
   
   if (trace_use_dull) call da_trace_entry("da_calculate_grady_airsr")

   do n=1, iv%info(airsr)%nlocal
      do k=1, iv%info(airsr)%levels(n)
         if (iv%airsr(n)%t(k)%qc < obs_qc_pointer) re%airsr(n)%t(k) = 0.0
         if (iv%airsr(n)%q(k)%qc < obs_qc_pointer) re%airsr(n)%q(k) = 0.0

         jo_grad_y%airsr(n)%t(k) = -re%airsr(n)%t(k) / (iv%airsr(n)%t(k)%error * iv%airsr(n)%t(k)%error)
         jo_grad_y%airsr(n)%q(k) = -re%airsr(n)%q(k) / (iv%airsr(n)%q(k)%error * iv%airsr(n)%q(k)%error)
       end do
    end do
   
   if (trace_use_dull) call da_trace_exit("da_calculate_grady_airsr")

end subroutine da_calculate_grady_airsr


