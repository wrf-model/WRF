subroutine da_calculate_grady_bogus(iv, re, jo_grad_y)

   !----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n, k
   
   if (trace_use_dull) call da_trace_entry("da_calculate_grady_bogus")

   do n=1, iv%info(bogus)%nlocal
      if (iv%bogus(n)%slp%qc < obs_qc_pointer) then
         re%bogus(n)%slp = 0.0
      end if

      jo_grad_y%bogus(n)%slp = -re%bogus(n)%slp / (iv%bogus(n)%slp%error * iv%bogus(n)%slp%error)

      do k=1, iv%info(bogus)%levels(n)
         if (iv%bogus(n)%u(k)%qc < obs_qc_pointer) re%bogus(n)%u(k) = 0.0
         if (iv%bogus(n)%v(k)%qc < obs_qc_pointer) re%bogus(n)%v(k) = 0.0
         if (iv%bogus(n)%t(k)%qc < obs_qc_pointer) re%bogus(n)%t(k) = 0.0
         if (iv%bogus(n)%q(k)%qc < obs_qc_pointer) re%bogus(n)%q(k) = 0.0

         jo_grad_y%bogus(n)%u(k) = -re%bogus(n)%u(k) / (iv%bogus(n)%u(k)%error * iv%bogus(n)%u(k)%error)
         jo_grad_y%bogus(n)%v(k) = -re%bogus(n)%v(k) / (iv%bogus(n)%v(k)%error * iv%bogus(n)%v(k)%error)
         jo_grad_y%bogus(n)%t(k) = -re%bogus(n)%t(k) / (iv%bogus(n)%t(k)%error * iv%bogus(n)%t(k)%error)
         jo_grad_y%bogus(n)%q(k) = -re%bogus(n)%q(k) / (iv%bogus(n)%q(k)%error * iv%bogus(n)%q(k)%error)
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_calculate_grady_bogus")

end subroutine da_calculate_grady_bogus


