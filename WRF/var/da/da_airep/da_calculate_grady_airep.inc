subroutine da_calculate_grady_airep(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none


   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_airep")

   do n=1, iv%info(airep)%nlocal
      do k=1, iv%info(airep)%levels(n)
         if (iv%airep(n)%u(k)%qc < obs_qc_pointer) re%airep(n)%u(k) = 0.0
         if (iv%airep(n)%v(k)%qc < obs_qc_pointer) re%airep(n)%v(k) = 0.0
         if (iv%airep(n)%t(k)%qc < obs_qc_pointer) re%airep(n)%t(k) = 0.0
         if (iv%airep(n)%q(k)%qc < obs_qc_pointer) re%airep(n)%q(k) = 0.0


         jo_grad_y%airep(n)%u(k) = -re%airep(n)%u(k) / (iv%airep(n)%u(k)%error*iv%airep(n)%u(k)%error)
         jo_grad_y%airep(n)%v(k) = -re%airep(n)%v(k) / (iv%airep(n)%v(k)%error*iv%airep(n)%v(k)%error)
         jo_grad_y%airep(n)%t(k) = -re%airep(n)%t(k) / (iv%airep(n)%t(k)%error*iv%airep(n)%t(k)%error)
         jo_grad_y%airep(n)%q(k) = -re%airep(n)%q(k) / (iv%airep(n)%q(k)%error*iv%airep(n)%q(k)%error)
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_calculate_grady_airep")

end subroutine da_calculate_grady_airep


