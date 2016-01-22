subroutine da_jo_and_grady_ships(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(in)    :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo          ! Obs cost function.

   integer :: n

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_ships")

   jo % ships_u = 0.0
   jo % ships_v = 0.0
   jo % ships_t = 0.0
   jo % ships_p = 0.0
   jo % ships_q = 0.0

   do n=1, iv%info(ships)%nlocal
      jo_grad_y%ships(n)%u = -re%ships(n)%u / &
         (iv%ships(n)%u%error * iv%ships(n)%u%error)
      jo_grad_y%ships(n)%v = -re%ships(n)%v / &
         (iv%ships(n)%v%error * iv%ships(n)%v%error)
      jo_grad_y%ships(n)%t = -re%ships(n)%t / &
         (iv%ships(n)%t%error * iv%ships(n)%t%error)
      jo_grad_y%ships(n)%p = -re%ships(n)%p / &
         (iv%ships(n)%p%error * iv%ships(n)%p%error)
      jo_grad_y%ships(n)%q = -re%ships(n)%q / &
         (iv%ships(n)%q%error * iv%ships(n)%q%error)

      if (iv%info(ships)%proc_domain(1,n)) then
         jo % ships_u = jo % ships_u - re%ships(n)%u * jo_grad_y%ships(n)%u
         jo % ships_v = jo % ships_v - re%ships(n)%v * jo_grad_y%ships(n)%v
         jo % ships_t = jo % ships_t - re%ships(n)%t * jo_grad_y%ships(n)%t
         jo % ships_p = jo % ships_p - re%ships(n)%p * jo_grad_y%ships(n)%p
         jo % ships_q = jo % ships_q - re%ships(n)%q * jo_grad_y%ships(n)%q
      end if
   end do

   jo % ships_u = 0.5 * jo % ships_u
   jo % ships_v = 0.5 * jo % ships_v
   jo % ships_t = 0.5 * jo % ships_t
   jo % ships_p = 0.5 * jo % ships_p
   jo % ships_q = 0.5 * jo % ships_q

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_ships")
     
end subroutine da_jo_and_grady_ships


