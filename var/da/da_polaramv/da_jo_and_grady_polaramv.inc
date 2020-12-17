subroutine da_jo_and_grady_polaramv( iv, re, jo, jo_grad_y)

   !----------------------------------------------------------------------
   ! Purpose:  Calculates Cost function and Gradient for Polar AMVs Obs
   !----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(in)    :: iv          ! Innovation vector.
   type(y_type),  intent(in)    :: re          ! Residual vector.
   type(y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type(jo_type), intent(inout) :: jo          ! Obs cost function.

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_polaramv")

   jo % polaramv_u = 0.0
   jo % polaramv_v = 0.0

   do n=1, iv%info(polaramv)%nlocal
      do k=1, iv%info(polaramv)%levels(n)
         jo_grad_y%polaramv(n)%u(k) = -re%polaramv(n)%u(k) / &
            ( iv%polaramv(n)%u(k)%error * iv%polaramv(n)%u(k)%error)
         jo_grad_y%polaramv(n)%v(k) = -re%polaramv(n)%v(k) / &
            ( iv%polaramv(n)%v(k)%error * iv%polaramv(n)%v(k)%error)
      end do

      do k=1, iv%info(polaramv)%levels(n)
         if (iv%info(polaramv)%proc_domain(k,n)) then
            jo % polaramv_u = jo % polaramv_u - re%polaramv(n)%u(k) * jo_grad_y%polaramv(n)%u(k)
            jo % polaramv_v = jo % polaramv_v - re%polaramv(n)%v(k) * jo_grad_y%polaramv(n)%v(k)
         end if
      end do
   end do

   jo % polaramv_u = 0.5 * jo % polaramv_u
   jo % polaramv_v = 0.5 * jo % polaramv_v

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_polaramv")
     
end subroutine da_jo_and_grady_polaramv


