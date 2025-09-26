subroutine da_jo_and_grady_airsr(iv, re, jo, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Calculates cost function and its gradient at all AIRS 
   ! retrieval locations   
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv         ! Innovation vector.
   type (y_type),  intent(in)    :: re         ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y  ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo         ! Obs cost function.

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_airsr")

   jo % airsr_t = 0.0
   jo % airsr_q = 0.0

   do n=1, iv%info(airsr)%nlocal
      do k=1, iv%info(airsr)%levels(n)
         jo_grad_y%airsr(n)%t(k) = -re%airsr(n)%t(k) / (iv%airsr(n)%t(k)%error * iv%airsr(n)%t(k)%error)
         jo_grad_y%airsr(n)%q(k) = -re%airsr(n)%q(k) / (iv%airsr(n)%q(k)%error * iv%airsr(n)%q(k)%error)
      end do

      if (iv%info(airsr)%proc_domain(1,n)) then
         do k=1, iv%info(airsr)%levels(n)
            jo % airsr_t = jo % airsr_t - re%airsr(n)%t(k) * jo_grad_y%airsr(n)%t(k)
            jo % airsr_q = jo % airsr_q - re%airsr(n)%q(k) * jo_grad_y%airsr(n)%q(k)
         end do
      end if
   end do

   jo % airsr_t = 0.5 * jo % airsr_t
   jo % airsr_q = 0.5 * jo % airsr_q

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_airsr")

 end subroutine da_jo_and_grady_airsr


