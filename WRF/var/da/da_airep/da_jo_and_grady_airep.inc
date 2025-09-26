subroutine da_jo_and_grady_airep(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(in)    :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(out)   :: jo          ! Obs cost function.

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_airep")

   jo % airep_u = 0.0
   jo % airep_v = 0.0
   jo % airep_t = 0.0
   jo % airep_q = 0.0

   do n=1, iv%info(airep)%nlocal
      do k=1, iv%info(airep)%levels(n)
         jo_grad_y%airep(n)%u(k) = -re%airep(n)%u(k) / &
            (iv%airep(n)%u(k)%error * iv%airep(n)%u(k)%error)
         jo_grad_y%airep(n)%v(k) = -re%airep(n)%v(k) / &
            (iv%airep(n)%v(k)%error * iv%airep(n)%v(k)%error)
         jo_grad_y%airep(n)%t(k) = -re%airep(n)%t(k) / &
            (iv%airep(n)%t(k)%error * iv%airep(n)%t(k)%error)
         jo_grad_y%airep(n)%q(k) = -re%airep(n)%q(k) / &
            (iv%airep(n)%q(k)%error * iv%airep(n)%q(k)%error)
      end do

      do k=1, iv%info(airep)%levels(n)
         if (iv%info(airep)%proc_domain(k,n)) then
           jo % airep_u = jo%airep_u - re%airep(n)%u(k) * jo_grad_y%airep(n)%u(k)
           jo % airep_v = jo%airep_v - re%airep(n)%v(k) * jo_grad_y%airep(n)%v(k)
           jo % airep_t = jo%airep_t - re%airep(n)%t(k) * jo_grad_y%airep(n)%t(k)
           jo % airep_q = jo%airep_q - re%airep(n)%q(k) * jo_grad_y%airep(n)%q(k)
         end if
      end do
   end do

   jo % airep_u = 0.5 * jo % airep_u
   jo % airep_v = 0.5 * jo % airep_v
   jo % airep_t = 0.5 * jo % airep_t
   jo % airep_q = 0.5 * jo % airep_q

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_airep")

end subroutine da_jo_and_grady_airep


