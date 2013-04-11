subroutine da_calculate_grady_polaramv(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Calculates Gradient of Polar AMVs  Obs.          
   !-------------------------------------------------------------------------

   implicit none


   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer :: n , k
   
   if (trace_use_dull) call da_trace_entry("da_calculate_grady_polaramv")

   do n=1, iv%info(polaramv)%nlocal
      do k=1, iv%info(polaramv)%levels(n)
         if (iv%polaramv(n)%u(k)%qc < obs_qc_pointer) then
            re%polaramv(n)%u(k) = 0.0
         end if
         if (iv%polaramv(n)%v(k)%qc < obs_qc_pointer) then
            re%polaramv(n)%v(k) = 0.0
         end if

         jo_grad_y%polaramv(n)%u(k) = -re%polaramv(n)%u(k) / &
             (iv%polaramv(n)%u(k)%error * iv%polaramv(n)%u(k)%error)
         jo_grad_y%polaramv(n)%v(k) = -re%polaramv(n)%v(k) / &
            (iv%polaramv(n)%v(k)%error * iv%polaramv(n)%v(k)%error)
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_calculate_grady_polaramv")

end subroutine da_calculate_grady_polaramv


