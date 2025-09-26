subroutine da_calculate_grady_ssmi_rv(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)   :: iv          ! Ob Inc. structure.
   type (y_type), intent(inout) :: re          ! Residual structure.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_ssmi_rv")

   do n=1, iv%info(ssmi_rv)%nlocal
      if (iv%ssmi_rv(n)%speed%qc < obs_qc_pointer) then
         re%ssmi_rv(n)%speed = 0.0
      end if
      jo_grad_y%ssmi_rv(n)%Speed = - re%ssmi_rv(n)%Speed / (iv%ssmi_rv(n)%Speed%error * iv%ssmi_rv(n)%Speed%error)

      if (iv%ssmi_rv(n)%tpw%qc < obs_qc_pointer) then
         re%ssmi_rv(n)%tpw = 0.0
      end if
      jo_grad_y%ssmi_rv(n)%tpw = -re%ssmi_rv(n)%tpw / (iv%ssmi_rv(n)%tpw%error * iv%ssmi_rv(n)%tpw%error)
   end do  

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_ssmi_rv")

end subroutine da_calculate_grady_ssmi_rv


