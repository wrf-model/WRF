subroutine da_calculate_grady_gpspw(iv, re, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(inout) :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer :: n

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_gpspw")

   do n=1, iv%info(gpspw)%nlocal
      if (iv%gpspw(n)%tpw%qc < obs_qc_pointer) then
         re%gpspw(n)%tpw = 0.0
      end if

      jo_grad_y%gpspw(n)%tpw = -re%gpspw(n)%tpw / (iv%gpspw(n)%tpw%error * iv%gpspw(n)%tpw%error)
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_gpspw")


end subroutine da_calculate_grady_gpspw


