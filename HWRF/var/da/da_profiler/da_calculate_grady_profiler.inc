subroutine da_calculate_grady_profiler(iv, re, jo_grad_y)

   !---------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer :: n, k
   
   if (trace_use_dull) call da_trace_entry("da_calculate_grady_profiler")

   do n=1, iv%info(profiler)%nlocal
      do k=1, iv%info(profiler)%levels(n)
         if (iv%profiler(n)%u(k)%qc < obs_qc_pointer) then
            re%profiler(n)%u(k) = 0.0
         end if
         if (iv%profiler(n)%v(k)%qc < obs_qc_pointer) then
            re%profiler(n)%v(k) = 0.0
         end if

         jo_grad_y%profiler(n)%u(k) = -re%profiler(n)%u(k) &
            / (iv%profiler(n)%u(k)%error * iv%profiler(n)%u(k)%error)
         jo_grad_y%profiler(n)%v(k) = -re%profiler(n)%v(k) &
            / (iv%profiler(n)%v(k)%error * iv%profiler(n)%v(k)%error)
      end do
   end do 
   
   if (trace_use_dull) call da_trace_exit("da_calculate_grady_profiler")

end subroutine da_calculate_grady_profiler


