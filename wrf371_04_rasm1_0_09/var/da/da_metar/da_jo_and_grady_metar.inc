subroutine da_jo_and_grady_metar(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type(jo_type), intent(inout) :: jo          ! Obs cost function.

   integer                      :: n

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_metar")


   jo % metar_u = 0.0
   jo % metar_v = 0.0
   jo % metar_t = 0.0
   jo % metar_p = 0.0
   jo % metar_q = 0.0

   if (iv%info(metar)%nlocal > 0) then
      do n=1, iv%info(metar)%nlocal
         jo_grad_y%metar(n)%u = -re%metar(n)%u / &
                              (iv%metar(n)%u%error * iv%metar(n)%u%error)
         jo_grad_y%metar(n)%v = -re%metar(n)%v / &
                              (iv%metar(n)%v%error * iv%metar(n)%v%error)
         jo_grad_y%metar(n)%t = -re%metar(n)%t / &
                              (iv%metar(n)%t%error * iv%metar(n)%t%error)
         jo_grad_y%metar(n)%p = -re%metar(n)%p / &
                              (iv%metar(n)%p%error * iv%metar(n)%p%error)
         jo_grad_y%metar(n)%q = -re%metar(n)%q / &
                              (iv%metar(n)%q%error * iv%metar(n)%q%error)

         if (iv%info(metar)%proc_domain(1,n)) then
            jo % metar_u = jo % metar_u - re%metar(n)%u * jo_grad_y%metar(n)%u
            jo % metar_v = jo % metar_v - re%metar(n)%v * jo_grad_y%metar(n)%v
            jo % metar_t = jo % metar_t - re%metar(n)%t * jo_grad_y%metar(n)%t
            jo % metar_p = jo % metar_p - re%metar(n)%p * jo_grad_y%metar(n)%p
            jo % metar_q = jo % metar_q - re%metar(n)%q * jo_grad_y%metar(n)%q
         end if
      end do

      jo % metar_u = 0.5 * jo % metar_u
      jo % metar_v = 0.5 * jo % metar_v
      jo % metar_t = 0.5 * jo % metar_t
      jo % metar_p = 0.5 * jo % metar_p
      jo % metar_q = 0.5 * jo % metar_q
   end if

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_metar")

end subroutine da_jo_and_grady_metar


