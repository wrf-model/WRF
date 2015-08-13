subroutine da_calculate_grady_geoamv(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Calculates Gradient of Geo. CMVs Obs.
   !               
   !-------------------------------------------------------------------------

   implicit none


   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n , k
   
   if (trace_use_dull) call da_trace_entry("da_calculate_grady_geoamv")  

   do n=1, iv%info(geoamv)%nlocal
      do k=1, iv%info(geoamv)%levels(n)
         if (iv%geoamv(n)%u(k)%qc < obs_qc_pointer) re%geoamv(n)%u(k) = 0.0
         if (iv%geoamv(n)%v(k)%qc < obs_qc_pointer) re%geoamv(n)%v(k) = 0.0

         jo_grad_y%geoamv(n)%u(k) = -re%geoamv(n)%u(k) / (iv%geoamv(n)%u(k)%error * iv%geoamv(n)%u(k)%error)
         jo_grad_y%geoamv(n)%v(k) = -re%geoamv(n)%v(k) / (iv%geoamv(n)%v(k)%error * iv%geoamv(n)%v(k)%error)
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_calculate_grady_geoamv")  

end subroutine da_calculate_grady_geoamv


