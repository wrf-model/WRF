subroutine da_calculate_grady_synop(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_synop")

   do n=1, iv%info(synop)%nlocal
      if (iv%synop(n)%u%qc < obs_qc_pointer) re%synop(n)%u = 0.0
      if (iv%synop(n)%v%qc < obs_qc_pointer) re%synop(n)%v = 0.0
      if (iv%synop(n)%t%qc < obs_qc_pointer) re%synop(n)%t = 0.0
      if (iv%synop(n)%p%qc < obs_qc_pointer) re%synop(n)%p = 0.0
      if (iv%synop(n)%q%qc < obs_qc_pointer) re%synop(n)%q = 0.0

      if (iv%synop(n)%u%qc < obs_qc_pointer) re%synop(n)%u = 0.0
      if (iv%synop(n)%v%qc < obs_qc_pointer) re%synop(n)%v = 0.0
      if (iv%synop(n)%t%qc < obs_qc_pointer) re%synop(n)%t = 0.0
      if (iv%synop(n)%p%qc < obs_qc_pointer) re%synop(n)%p = 0.0
      if (iv%synop(n)%q%qc < obs_qc_pointer) re%synop(n)%q = 0.0

      jo_grad_y%synop(n)%u = -re%synop(n)%u / (iv%synop(n)%u%error * iv%synop(n)%u%error)
      jo_grad_y%synop(n)%v = -re%synop(n)%v / (iv%synop(n)%v%error * iv%synop(n)%v%error)
      jo_grad_y%synop(n)%t = -re%synop(n)%t / (iv%synop(n)%t%error * iv%synop(n)%t%error)
      jo_grad_y%synop(n)%p = -re%synop(n)%p / (iv%synop(n)%p%error * iv%synop(n)%p%error)
      jo_grad_y%synop(n)%q = -re%synop(n)%q / (iv%synop(n)%q%error * iv%synop(n)%q%error)
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_synop")
     
end subroutine da_calculate_grady_synop


