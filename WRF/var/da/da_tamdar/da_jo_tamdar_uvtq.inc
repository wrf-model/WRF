subroutine da_jo_tamdar_uvtq (iv, re, jo_grad_y, jo)

   !-----------------------------------------------------------------------
   ! Purpose: Ensures that exactly the same code is used for both 
   ! serial and parallel computations when testing_dm_bitwise_exact is set.
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv         ! Innovation vector.
   type (y_type),  intent(in)    :: re         ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y  ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo         ! Obs cost function.
 
   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_tamdar_uvtq")

   do n=1, iv%info(tamdar)%nlocal
      do k=1, iv%info(tamdar)%levels(n)
         if (iv%info(tamdar)%proc_domain(1,n)) then
            jo % tamdar_u = jo % tamdar_u - re%tamdar(n)%u(k) * jo_grad_y%tamdar(n)%u(k)
            jo % tamdar_v = jo % tamdar_v - re%tamdar(n)%v(k) * jo_grad_y%tamdar(n)%v(k)
            jo % tamdar_t = jo % tamdar_t - re%tamdar(n)%t(k) * jo_grad_y%tamdar(n)%t(k)
            jo % tamdar_q = jo % tamdar_q - re%tamdar(n)%q(k) * jo_grad_y%tamdar(n)%q(k)
         end if
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_jo_tamdar_uvtq")

end subroutine da_jo_tamdar_uvtq


