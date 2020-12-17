subroutine da_calculate_grady_mtgirs(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n, k

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_mtgirs")

   do n=1, iv%info(mtgirs)%nlocal
      do k=1, iv%info(mtgirs)%levels(n)
         if (iv%mtgirs(n)%u(k)%qc < obs_qc_pointer) re%mtgirs(n)%u(k) = 0.0
         if (iv%mtgirs(n)%v(k)%qc < obs_qc_pointer) re%mtgirs(n)%v(k) = 0.0
         if (iv%mtgirs(n)%t(k)%qc < obs_qc_pointer) re%mtgirs(n)%t(k) = 0.0
         if (iv%mtgirs(n)%q(k)%qc < obs_qc_pointer) re%mtgirs(n)%q(k) = 0.0

         jo_grad_y%mtgirs(n)%u(k) = -re%mtgirs(n)%u(k) / (iv%mtgirs(n)%u(k)%error * iv%mtgirs(n)%u(k)%error)
         jo_grad_y%mtgirs(n)%v(k) = -re%mtgirs(n)%v(k) / (iv%mtgirs(n)%v(k)%error * iv%mtgirs(n)%v(k)%error)
         jo_grad_y%mtgirs(n)%t(k) = -re%mtgirs(n)%t(k) / (iv%mtgirs(n)%t(k)%error * iv%mtgirs(n)%t(k)%error)
         jo_grad_y%mtgirs(n)%q(k) = -re%mtgirs(n)%q(k) / (iv%mtgirs(n)%q(k)%error * iv%mtgirs(n)%q(k)%error)
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_mtgirs")

end subroutine da_calculate_grady_mtgirs


