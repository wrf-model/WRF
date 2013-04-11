subroutine da_calculate_grady_rain(iv, re, jo_grad_y)

   !----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(inout) :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer :: n
   
   if (trace_use) call da_trace_entry("da_calculate_grady_rain")       

   do n=1, iv%info(rain)%nlocal
             if (iv%rain(n)%rain%qc < obs_qc_pointer) re%rain(n)%rain = 0.0
             jo_grad_y%rain(n)%rain = -re%rain(n)%rain / (iv%rain(n)%rain%error * iv%rain(n)%rain%error)
   end do
   
   if (trace_use) call da_trace_exit("da_calculate_grady_rain")  
     
end subroutine da_calculate_grady_rain


