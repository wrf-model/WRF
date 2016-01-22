subroutine da_jo_and_grady_gpsref( iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   !----------------------------------------------------------------------
   ! Called by da_minimisation/da_jo_and_grady.inc
   !----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(in)    :: iv          ! Innovation vector.
   type(y_type),  intent(in)    :: re          ! Residual vector.
   type(y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type(jo_type), intent(inout) :: jo          ! Obs cost function.

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_gpsref")

   jo % gpsref_ref = 0.0

   do n=1, iv%info(gpsref)%nlocal
      do k=1, iv%info(gpsref)%levels(n)
         jo_grad_y%gpsref(n)%ref(k) = -re%gpsref(n)%ref(k) / &
            ( iv%gpsref(n)%ref(k)%error * iv%gpsref(n)%ref(k)%error)
      end do

      if (iv%info(gpsref)%proc_domain(1,n)) then
         do k=1, iv%info(gpsref)%levels(n)
            jo % gpsref_ref = jo % gpsref_ref - &
                re%gpsref(n)%ref(k) * jo_grad_y%gpsref(n)%ref(k)
         end do
      end if

   end do

   jo % gpsref_ref = 0.5 * jo % gpsref_ref    

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_gpsref")

end subroutine da_jo_and_grady_gpsref


