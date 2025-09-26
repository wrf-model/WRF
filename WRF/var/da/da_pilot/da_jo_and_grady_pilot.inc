subroutine da_jo_and_grady_pilot(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type(jo_type), intent(inout) :: jo          ! Obs cost function.

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_pilot")

   jo % pilot_u = 0.0
   jo % pilot_v = 0.0

   do n=1, iv%info(pilot)%nlocal
      do k=1, iv%info(pilot)%levels(n)
         jo_grad_y%pilot(n)%u(k) = -re%pilot(n)%u(k) / (iv%pilot(n)%u(k)%error * iv%pilot(n)%u(k)%error)
         jo_grad_y%pilot(n)%v(k) = -re%pilot(n)%v(k) / (iv%pilot(n)%v(k)%error * iv%pilot(n)%v(k)%error)
      end do

     if (iv%info(pilot)%proc_domain(1,n)) then
        do k=1, iv%info(pilot)%levels(n)
           jo % pilot_u = jo % pilot_u - re%pilot(n)%u(k) * jo_grad_y%pilot(n)%u(k)
           jo % pilot_v = jo % pilot_v - re%pilot(n)%v(k) * jo_grad_y%pilot(n)%v(k)
        end do
      end if
   end do

   jo % pilot_u = 0.5 * jo % pilot_u
   jo % pilot_v = 0.5 * jo % pilot_v

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_pilot")

end subroutine da_jo_and_grady_pilot


