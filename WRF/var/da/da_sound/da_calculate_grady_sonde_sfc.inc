subroutine da_calculate_grady_sonde_sfc(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector              
   !-------------------------------------------------------------------------

   implicit none


   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_sonde_sfc")

   do n=1, iv%info(sound)%nlocal
      if (iv%sonde_sfc(n)%u%qc < obs_qc_pointer) re%sonde_sfc(n)%u = 0.0
      if (iv%sonde_sfc(n)%v%qc < obs_qc_pointer) re%sonde_sfc(n)%v = 0.0
      if (iv%sonde_sfc(n)%t%qc < obs_qc_pointer) re%sonde_sfc(n)%t = 0.0
      if (iv%sonde_sfc(n)%p%qc < obs_qc_pointer) re%sonde_sfc(n)%p = 0.0
      if (iv%sonde_sfc(n)%q%qc < obs_qc_pointer) re%sonde_sfc(n)%q = 0.0

      if (iv%sonde_sfc(n)%u%qc < obs_qc_pointer) re%sonde_sfc(n)%u = 0.0
      if (iv%sonde_sfc(n)%v%qc < obs_qc_pointer) re%sonde_sfc(n)%v = 0.0
      if (iv%sonde_sfc(n)%t%qc < obs_qc_pointer) re%sonde_sfc(n)%t = 0.0
      if (iv%sonde_sfc(n)%p%qc < obs_qc_pointer) re%sonde_sfc(n)%p = 0.0
      if (iv%sonde_sfc(n)%q%qc < obs_qc_pointer) re%sonde_sfc(n)%q = 0.0

      jo_grad_y%sonde_sfc(n)%u = -re%sonde_sfc(n)%u / &
          (iv%sonde_sfc(n)%u%error * iv%sonde_sfc(n)%u%error)
      jo_grad_y%sonde_sfc(n)%v = -re%sonde_sfc(n)%v / &
          (iv%sonde_sfc(n)%v%error * iv%sonde_sfc(n)%v%error)
      jo_grad_y%sonde_sfc(n)%t = -re%sonde_sfc(n)%t / &
          (iv%sonde_sfc(n)%t%error * iv%sonde_sfc(n)%t%error)
      jo_grad_y%sonde_sfc(n)%p = -re%sonde_sfc(n)%p / &
          (iv%sonde_sfc(n)%p%error * iv%sonde_sfc(n)%p%error)
      jo_grad_y%sonde_sfc(n)%q = -re%sonde_sfc(n)%q / &
          (iv%sonde_sfc(n)%q%error * iv%sonde_sfc(n)%q%error)
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_sonde_sfc")
     
end subroutine da_calculate_grady_sonde_sfc


