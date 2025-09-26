subroutine da_jo_and_grady_rain(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type),intent(inout) :: jo          ! Obs cost function.

   integer :: n 

   if (trace_use) call da_trace_entry("da_jo_and_grady_rain")

   jo % rain_r = 0.0
   
   do n=1, iv%info(rain)%nlocal
      jo_grad_y%rain(n)%rain = -re%rain(n)%rain / &
         (iv%rain(n)%rain%error * iv%rain(n)%rain%error)

      if (iv%info(rain)%proc_domain(1,n)) then
         jo % rain_r = jo % rain_r - re%rain(n)%rain * jo_grad_y%rain(n)%rain
      end if
   end do
      
   jo % rain_r = 0.5 * jo % rain_r

   if (trace_use) call da_trace_exit("da_jo_and_grady_rain")

end subroutine da_jo_and_grady_rain


