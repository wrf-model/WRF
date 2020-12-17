subroutine da_jo_and_grady_profiler( iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(in)    :: iv          ! Innovation vector.
   type(y_type),  intent(in)    :: re          ! Residual vector.
   type(y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type(jo_type), intent(inout) :: jo          ! Obs cost function.

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_profiler")

   jo % profiler_u = 0.0
   jo % profiler_v = 0.0

   do n=1, iv%info(profiler)%nlocal
      do k=1, iv%info(profiler)%levels(n)
         jo_grad_y%profiler(n)%u(k) = -re%profiler(n)%u(k) / &
            (iv%profiler(n)%u(k)%error * iv%profiler(n)%u(k)%error)
         jo_grad_y%profiler(n)%v(k) = -re%profiler(n)%v(k) / &
            (iv%profiler(n)%v(k)%error * iv%profiler(n)%v(k)%error)
      end do

      if (iv%info(profiler)%proc_domain(1,n)) then
         do k=1, iv%info(profiler)%levels(n)
            jo % profiler_u = jo % profiler_u - &
               re%profiler(n)%u(k) * jo_grad_y%profiler(n)%u(k)
            jo % profiler_v = jo % profiler_v - &
               re%profiler(n)%v(k) * jo_grad_y%profiler(n)%v(k)
         end do
      end if
   end do

   jo % profiler_u = 0.5 * jo % profiler_u
   jo % profiler_v = 0.5 * jo % profiler_v

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_profiler")

end subroutine da_jo_and_grady_profiler


