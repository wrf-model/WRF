subroutine da_calculate_grady_ssmi_tb(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)   :: iv          ! Ob Inc. structure.
   type (y_type), intent(inout) :: re          ! Residual structure.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_ssmi_tb")

   do n=1, iv%info(ssmi_tb)%nlocal
      if (iv%ssmi_tb(n)%tb19v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb19v = 0.0
      jo_grad_y%ssmi_tb(n)%tb19v = - re%ssmi_tb(n)%tb19v / (iv%ssmi_tb(n)%tb19v%error * iv%ssmi_tb(n)%tb19v%error)

      if (iv%ssmi_tb(n)%tb19h%qc < obs_qc_pointer) re%ssmi_tb(n)%tb19h = 0.0
      jo_grad_y%ssmi_tb(n)%tb19h = - re%ssmi_tb(n)%tb19h / (iv%ssmi_tb(n)%tb19h%error * iv%ssmi_tb(n)%tb19h%error)

      if (iv%ssmi_tb(n)%tb22v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb22v = 0.0
      jo_grad_y%ssmi_tb(n)%tb22v = - re%ssmi_tb(n)%tb22v / (iv%ssmi_tb(n)%tb22v%error * iv%ssmi_tb(n)%tb22v%error)

      if (iv%ssmi_tb(n)%tb37v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb37v = 0.0
      jo_grad_y%ssmi_tb(n)%tb37v = - re%ssmi_tb(n)%tb37v / (iv%ssmi_tb(n)%tb37v%error * iv%ssmi_tb(n)%tb37v%error)

      if (iv%ssmi_tb(n)%tb37h%qc < obs_qc_pointer) re%ssmi_tb(n)%tb37h = 0.0
      jo_grad_y%ssmi_tb(n)%tb37h = - re%ssmi_tb(n)%tb37h / (iv%ssmi_tb(n)%tb37h%error * iv%ssmi_tb(n)%tb37h%error)

      if (iv%ssmi_tb(n)%tb85v%qc < obs_qc_pointer) re%ssmi_tb(n)%tb85v = 0.0
      jo_grad_y%ssmi_tb(n)%tb85v = - re%ssmi_tb(n)%tb85v / (iv%ssmi_tb(n)%tb85v%error * iv%ssmi_tb(n)%tb85v%error)

      if (iv%ssmi_tb(n)%tb85h%qc < obs_qc_pointer) re%ssmi_tb(n)%tb85h = 0.0
      jo_grad_y%ssmi_tb(n)%tb85h = - re%ssmi_tb(n)%tb85h / (iv%ssmi_tb(n)%tb85h%error * iv%ssmi_tb(n)%tb85h%error)
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_ssmi_tb")

end subroutine da_calculate_grady_ssmi_tb


