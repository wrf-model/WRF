subroutine da_calculate_grady_tamdar(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n, k

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_tamdar")

   do n=1, iv%info(tamdar)%nlocal
      do k=1, iv%info(tamdar)%levels(n)
         if (iv%tamdar(n)%u(k)%qc < obs_qc_pointer) re%tamdar(n)%u(k) = 0.0
         if (iv%tamdar(n)%v(k)%qc < obs_qc_pointer) re%tamdar(n)%v(k) = 0.0
         if (iv%tamdar(n)%t(k)%qc < obs_qc_pointer) re%tamdar(n)%t(k) = 0.0
         if (iv%tamdar(n)%q(k)%qc < obs_qc_pointer) re%tamdar(n)%q(k) = 0.0

         jo_grad_y%tamdar(n)%u(k) = -re%tamdar(n)%u(k) / (iv%tamdar(n)%u(k)%error * iv%tamdar(n)%u(k)%error)
         jo_grad_y%tamdar(n)%v(k) = -re%tamdar(n)%v(k) / (iv%tamdar(n)%v(k)%error * iv%tamdar(n)%v(k)%error)
         jo_grad_y%tamdar(n)%t(k) = -re%tamdar(n)%t(k) / (iv%tamdar(n)%t(k)%error * iv%tamdar(n)%t(k)%error)
         jo_grad_y%tamdar(n)%q(k) = -re%tamdar(n)%q(k) / (iv%tamdar(n)%q(k)%error * iv%tamdar(n)%q(k)%error)
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_tamdar")

end subroutine da_calculate_grady_tamdar


