subroutine da_calculate_grady_tamdar_sfc(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector              
   !-------------------------------------------------------------------------

   implicit none


   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_tamdar_sfc")

   do n=1, iv%info(tamdar_sfc)%nlocal
      if (iv%tamdar_sfc(n)%u%qc < obs_qc_pointer) re%tamdar_sfc(n)%u = 0.0
      if (iv%tamdar_sfc(n)%v%qc < obs_qc_pointer) re%tamdar_sfc(n)%v = 0.0
      if (iv%tamdar_sfc(n)%t%qc < obs_qc_pointer) re%tamdar_sfc(n)%t = 0.0
      if (iv%tamdar_sfc(n)%p%qc < obs_qc_pointer) re%tamdar_sfc(n)%p = 0.0
      if (iv%tamdar_sfc(n)%q%qc < obs_qc_pointer) re%tamdar_sfc(n)%q = 0.0

      if (iv%tamdar_sfc(n)%u%qc < obs_qc_pointer) re%tamdar_sfc(n)%u = 0.0
      if (iv%tamdar_sfc(n)%v%qc < obs_qc_pointer) re%tamdar_sfc(n)%v = 0.0
      if (iv%tamdar_sfc(n)%t%qc < obs_qc_pointer) re%tamdar_sfc(n)%t = 0.0
      if (iv%tamdar_sfc(n)%p%qc < obs_qc_pointer) re%tamdar_sfc(n)%p = 0.0
      if (iv%tamdar_sfc(n)%q%qc < obs_qc_pointer) re%tamdar_sfc(n)%q = 0.0

      jo_grad_y%tamdar_sfc(n)%u = -re%tamdar_sfc(n)%u / &
          (iv%tamdar_sfc(n)%u%error * iv%tamdar_sfc(n)%u%error)
      jo_grad_y%tamdar_sfc(n)%v = -re%tamdar_sfc(n)%v / &
          (iv%tamdar_sfc(n)%v%error * iv%tamdar_sfc(n)%v%error)
      jo_grad_y%tamdar_sfc(n)%t = -re%tamdar_sfc(n)%t / &
          (iv%tamdar_sfc(n)%t%error * iv%tamdar_sfc(n)%t%error)
      jo_grad_y%tamdar_sfc(n)%p = -re%tamdar_sfc(n)%p / &
          (iv%tamdar_sfc(n)%p%error * iv%tamdar_sfc(n)%p%error)
      jo_grad_y%tamdar_sfc(n)%q = -re%tamdar_sfc(n)%q / &
          (iv%tamdar_sfc(n)%q%error * iv%tamdar_sfc(n)%q%error)
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_tamdar_sfc")
     
end subroutine da_calculate_grady_tamdar_sfc


