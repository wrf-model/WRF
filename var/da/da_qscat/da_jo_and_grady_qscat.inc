subroutine da_jo_and_grady_qscat(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(in)    :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo          ! Obs cost function.

   integer :: n

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_qscat")

   jo % qscat_u = 0.0
   jo % qscat_v = 0.0

   do n=1, iv%info(qscat)%nlocal
      jo_grad_y%qscat(n)%u = -re%qscat(n)%u / &
         (iv%qscat(n)%u%error * iv%qscat(n)%u%error)
      jo_grad_y%qscat(n)%v = -re%qscat(n)%v / &
         (iv%qscat(n)%v%error * iv%qscat(n)%v%error)

      if (iv%info(qscat)%proc_domain(1,n)) then
         jo % qscat_u = jo % qscat_u - re%qscat(n)%u * jo_grad_y%qscat(n)%u
         jo % qscat_v = jo % qscat_v - re%qscat(n)%v * jo_grad_y%qscat(n)%v
      end if
   end do

   jo % qscat_u = 0.5 * jo % qscat_u
   jo % qscat_v = 0.5 * jo % qscat_v

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_qscat")

     
end subroutine da_jo_and_grady_qscat


