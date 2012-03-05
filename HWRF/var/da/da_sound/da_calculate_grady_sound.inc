subroutine da_calculate_grady_sound(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n, k

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_sound")

   do n=1, iv%info(sound)%nlocal
      do k=1, iv%info(sound)%levels(n)
         if (iv%sound(n)%u(k)%qc < obs_qc_pointer) re%sound(n)%u(k) = 0.0
         if (iv%sound(n)%v(k)%qc < obs_qc_pointer) re%sound(n)%v(k) = 0.0
         if (iv%sound(n)%t(k)%qc < obs_qc_pointer) re%sound(n)%t(k) = 0.0
         if (iv%sound(n)%q(k)%qc < obs_qc_pointer) re%sound(n)%q(k) = 0.0

         jo_grad_y%sound(n)%u(k) = -re%sound(n)%u(k) / (iv%sound(n)%u(k)%error * iv%sound(n)%u(k)%error)
         jo_grad_y%sound(n)%v(k) = -re%sound(n)%v(k) / (iv%sound(n)%v(k)%error * iv%sound(n)%v(k)%error)
         jo_grad_y%sound(n)%t(k) = -re%sound(n)%t(k) / (iv%sound(n)%t(k)%error * iv%sound(n)%t(k)%error)
         jo_grad_y%sound(n)%q(k) = -re%sound(n)%q(k) / (iv%sound(n)%q(k)%error * iv%sound(n)%q(k)%error)
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_sound")

end subroutine da_calculate_grady_sound


