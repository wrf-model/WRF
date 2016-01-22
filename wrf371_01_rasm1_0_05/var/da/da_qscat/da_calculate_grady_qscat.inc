subroutine da_calculate_grady_qscat(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none


   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer :: n
   
   if (trace_use_dull) call da_trace_entry("da_calculate_grady_qscat")

   do n=1, iv%info(qscat)%nlocal
      if (iv%qscat(n)%u%qc < obs_qc_pointer) re%qscat(n)%u = 0.0
      if (iv%qscat(n)%v%qc < obs_qc_pointer) re%qscat(n)%v = 0.0
      jo_grad_y%qscat(n)%u = -re%qscat(n)%u / (iv%qscat(n)%u%error * iv%qscat(n)%u%error)
      jo_grad_y%qscat(n)%v = -re%qscat(n)%v / (iv%qscat(n)%v%error * iv%qscat(n)%v%error)
   end do
   
   if (trace_use_dull) call da_trace_exit("da_calculate_grady_qscat")
        
end subroutine da_calculate_grady_qscat


