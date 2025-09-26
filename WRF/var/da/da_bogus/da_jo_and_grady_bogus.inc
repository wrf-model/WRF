subroutine da_jo_and_grady_bogus(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(in)    :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo          ! Obs cost function.

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_bogus") 

   jo%bogus_u   = 0.0
   jo%bogus_v   = 0.0
   jo%bogus_t   = 0.0
   jo%bogus_q   = 0.0
   jo%bogus_slp = 0.0

   if (iv%info(bogus)%nlocal > 0) then
      do n=1, iv%info(bogus)%nlocal
         jo_grad_y%bogus(n)%slp = -re%bogus(n)%slp &
            / (iv%bogus(n)%slp%error * iv%bogus(n)%slp%error)
         do k=1, iv%info(bogus)%levels(n)
            jo_grad_y%bogus(n)%u(k) = -re%bogus(n)%u(k) &
               / (iv%bogus(n)%u(k)%error *iv%bogus(n)%u(k)%error)
            jo_grad_y%bogus(n)%v(k) = -re%bogus(n)%v(k) &
               /(iv%bogus(n)%v(k)%error * iv%bogus(n)%v(k)%error)
            jo_grad_y%bogus(n)%t(k) = -re%bogus(n)%t(k) &
               / (iv%bogus(n)%t(k)%error * iv%bogus(n)%t(k)%error)
            jo_grad_y%bogus(n)%q(k) = -re%bogus(n)%q(k) &
               / (iv%bogus(n)%q(k)%error * iv%bogus(n)%q(k)%error)
         end do

         if (iv%info(bogus)%proc_domain(1,n)) then
            jo%bogus_slp = jo%bogus_slp - re%bogus(n)%slp * jo_grad_y%bogus(n)%slp

            do k=1, iv%info(bogus)%levels(n)
               jo%bogus_u = jo%bogus_u - re%bogus(n)%u(k) * jo_grad_y%bogus(n)%u(k)
               jo%bogus_v = jo%bogus_v - re%bogus(n)%v(k) * jo_grad_y%bogus(n)%v(k)
               jo%bogus_t = jo%bogus_t - re%bogus(n)%t(k) * jo_grad_y%bogus(n)%t(k)
               jo%bogus_q = jo%bogus_q - re%bogus(n)%q(k) * jo_grad_y%bogus(n)%q(k)
            end do
         end if 
      end do

      jo % bogus_slp = 0.5 * jo % bogus_slp
      jo % bogus_u = 0.5 * jo % bogus_u
      jo % bogus_v = 0.5 * jo % bogus_v
      jo % bogus_t = 0.5 * jo % bogus_t
      jo % bogus_q = 0.5 * jo % bogus_q
   end if

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_bogus") 

end subroutine da_jo_and_grady_bogus


