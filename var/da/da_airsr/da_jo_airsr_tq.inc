subroutine da_jo_airsr_tq(iv, re, jo_grad_y, jo)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv         ! Innovation vector.
   type (y_type),  intent(in)    :: re         ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y  ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo         ! Obs cost function.

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_airsr_tq")

   do n=1, iv%info(airsr)%nlocal
      if (iv%info(airsr)%proc_domain(1,n)) then
         do k=1, iv%info(airsr)%levels(n)
            jo % airsr_t = jo % airsr_t - re%airsr(n)%t(k) * jo_grad_y%airsr(n)%t(k)
            jo % airsr_q = jo % airsr_q - re%airsr(n)%q(k) * jo_grad_y%airsr(n)%q(k)
         end do
      end if
   end do

   if (trace_use_dull) call da_trace_exit("da_jo_airsr_tq")

end subroutine da_jo_airsr_tq


