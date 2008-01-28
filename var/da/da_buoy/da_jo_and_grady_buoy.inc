subroutine da_jo_and_grady_buoy(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(in)    :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo          ! Obs cost function.

   integer :: n

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_buoy")

   jo % buoy_u = 0.0
   jo % buoy_v = 0.0
   jo % buoy_t = 0.0
   jo % buoy_p = 0.0
   jo % buoy_q = 0.0

   do n=1, iv%info(buoy)%nlocal
      jo_grad_y%buoy(n)%u = -re%buoy(n)%u / (iv%buoy(n)%u%error * iv%buoy(n)%u%error)
      jo_grad_y%buoy(n)%v = -re%buoy(n)%v / (iv%buoy(n)%v%error * iv%buoy(n)%v%error)
      jo_grad_y%buoy(n)%t = -re%buoy(n)%t / (iv%buoy(n)%t%error * iv%buoy(n)%t%error)
      jo_grad_y%buoy(n)%p = -re%buoy(n)%p / (iv%buoy(n)%p%error * iv%buoy(n)%p%error)
      jo_grad_y%buoy(n)%q = -re%buoy(n)%q / (iv%buoy(n)%q%error * iv%buoy(n)%q%error)

      if (iv%info(buoy)%proc_domain(1,n)) then
         jo % buoy_u = jo % buoy_u - re%buoy(n)%u * jo_grad_y%buoy(n)%u
         jo % buoy_v = jo % buoy_v - re%buoy(n)%v * jo_grad_y%buoy(n)%v
         jo % buoy_t = jo % buoy_t - re%buoy(n)%t * jo_grad_y%buoy(n)%t
         jo % buoy_p = jo % buoy_p - re%buoy(n)%p * jo_grad_y%buoy(n)%p
         jo % buoy_q = jo % buoy_q - re%buoy(n)%q * jo_grad_y%buoy(n)%q
      end if
   end do

   jo % buoy_u = 0.5 * jo % buoy_u
   jo % buoy_v = 0.5 * jo % buoy_v
   jo % buoy_t = 0.5 * jo % buoy_t
   jo % buoy_p = 0.5 * jo % buoy_p
   jo % buoy_q = 0.5 * jo % buoy_q

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_buoy")
     
end subroutine da_jo_and_grady_buoy


