subroutine da_calculate_grady_ships(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(inout) :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer :: n

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_ships")

   do n=1, iv%info(ships)%nlocal
      if (iv%ships(n)%u%qc < obs_qc_pointer) re%ships(n)%u = 0.0
      if (iv%ships(n)%v%qc < obs_qc_pointer) re%ships(n)%v = 0.0
      if (iv%ships(n)%t%qc < obs_qc_pointer) re%ships(n)%t = 0.0
      if (iv%ships(n)%p%qc < obs_qc_pointer) re%ships(n)%p = 0.0
      if (iv%ships(n)%q%qc < obs_qc_pointer) re%ships(n)%q = 0.0

      jo_grad_y%ships(n)%u = -re%ships(n)%u / (iv%ships(n)%u%error * iv%ships(n)%u%error)
      jo_grad_y%ships(n)%v = -re%ships(n)%v / (iv%ships(n)%v%error * iv%ships(n)%v%error)
      jo_grad_y%ships(n)%t = -re%ships(n)%t / (iv%ships(n)%t%error * iv%ships(n)%t%error)
      jo_grad_y%ships(n)%p = -re%ships(n)%p / (iv%ships(n)%p%error * iv%ships(n)%p%error)
      jo_grad_y%ships(n)%q = -re%ships(n)%q / (iv%ships(n)%q%error * iv%ships(n)%q%error)
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_ships")
     
end subroutine da_calculate_grady_ships


