subroutine da_calculate_grady_gpsref(iv, re, jo_grad_y)

   !----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector         
   !----------------------------------------------------------------------

   implicit none


   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_gpsref")
   
   do n=1, iv%info(gpsref)%nlocal
      do k=1, iv%info(gpsref)%levels(n)
         if (iv%gpsref(n)%ref(k)%qc < obs_qc_pointer) then
            re%gpsref(n)%ref(k) = 0.0
         end if
         jo_grad_y%gpsref(n)%ref(k) = -re%gpsref(n)%ref(k) / (iv%gpsref(n)%ref(k)%error * iv%gpsref(n)%ref(k)%error)
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_gpsref")

end subroutine da_calculate_grady_gpsref


