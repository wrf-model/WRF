subroutine da_jo_and_grady_ssmi_rv(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Ob Inc. structure.
   type (y_type),  intent(in)    :: re          ! Residual structure.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo          ! Obs cost function.

   integer  :: n

   if (trace_use) call da_trace_entry("da_jo_and_grady_ssmi_rv")

   jo % ssmir_speed = 0.0
   jo % ssmir_tpw   = 0.0

   do n=1, iv%info(ssmi_rv)%nlocal
      jo_grad_y%ssmi_rv(n)%speed = - re%ssmi_rv(n)%speed / &
         (iv%ssmi_rv(n)%speed%error * iv%ssmi_rv(n)%speed%error)

      jo_grad_y%ssmi_rv(n)%tpw = -re%ssmi_rv(n)%tpw / &
          (iv%ssmi_rv(n)%tpw%error * iv%ssmi_rv(n)%tpw%error)

      if (iv%info(ssmi_rv)%proc_domain(1,n)) then

         jo%ssmir_speed = jo%ssmir_speed - re%ssmi_rv(n)%speed * jo_grad_y%ssmi_rv(n)%speed
         jo%ssmir_tpw   = jo%ssmir_tpw   - re%ssmi_rv(n)%tpw   * jo_grad_y%ssmi_rv(n)%tpw
      end if
   end do
   
   jo % ssmir_speed = 0.5 * jo % ssmir_speed
   jo % ssmir_tpw   = 0.5 * jo % ssmir_tpw

   if (trace_use) call da_trace_exit("da_jo_and_grady_ssmi_rv")

end subroutine da_jo_and_grady_ssmi_rv


