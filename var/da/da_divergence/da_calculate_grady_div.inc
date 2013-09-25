subroutine da_calculate_grady_div(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(inout) :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer :: n
   
   if (trace_use_dull) call da_trace_entry("da_calculate_grady_div")

   do n=1, iv%info(idxdiv)%nlocal
  ! do n=1, 1
!       if (iv%pseudo(n)%u%qc < obs_qc_pointer) re%pseudo(n)%u = 0.0
!       if (iv%pseudo(n)%v%qc < obs_qc_pointer) re%pseudo(n)%v = 0.0
!       if (iv%pseudo(n)%t%qc < obs_qc_pointer) re%pseudo(n)%t = 0.0
!       if (iv%pseudo(n)%p%qc < obs_qc_pointer) re%pseudo(n)%p = 0.0
!       if (iv%pseudo(n)%q%qc < obs_qc_pointer) re%pseudo(n)%q = 0.0
      if (iv%div(n)%qc < obs_qc_pointer) re%div(n) = 0.0
!       jo_grad_y%pseudo(n)%u = -re%pseudo(n)%u / (iv%pseudo(n)%u%error * iv%pseudo(n)%u%error)
!       jo_grad_y%pseudo(n)%v = -re%pseudo(n)%v / (iv%pseudo(n)%v%error * iv%pseudo(n)%v%error)
!       jo_grad_y%pseudo(n)%t = -re%pseudo(n)%t / (iv%pseudo(n)%t%error * iv%pseudo(n)%t%error)
!       jo_grad_y%pseudo(n)%p = -re%pseudo(n)%p / (iv%pseudo(n)%p%error * iv%pseudo(n)%p%error)
!       jo_grad_y%pseudo(n)%q = -re%pseudo(n)%q / (iv%pseudo(n)%q%error * iv%pseudo(n)%q%error)
      jo_grad_y%div(n) = -re%div(n) / (iv%div(n)%error * iv%div(n)%error)
   end do
   
   if (trace_use_dull) call da_trace_exit("da_calculate_grady_div")
  
end subroutine da_calculate_grady_div


