subroutine da_calculate_grady_pilot(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none


   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer  :: n, k
   
   if (trace_use_dull) call da_trace_entry("da_calculate_grady_pilot")

   do n=1, iv%info(pilot)%nlocal
      do k=1, iv%info(pilot)%levels(n)
         if (iv%pilot(n)%u(k)%qc < obs_qc_pointer) re%pilot(n)%u(k) = 0.0
         if (iv%pilot(n)%v(k)%qc < obs_qc_pointer) re%pilot(n)%v(k) = 0.0

         jo_grad_y%pilot(n)%u(k) = -re%pilot(n)%u(k) / (iv%pilot(n)%u(k)%error * iv%pilot(n)%u(k)%error)
         jo_grad_y%pilot(n)%v(k) = -re%pilot(n)%v(k) / (iv%pilot(n)%v(k)%error * iv%pilot(n)%v(k)%error)
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_calculate_grady_pilot")

end subroutine da_calculate_grady_pilot


