subroutine da_jo_and_grady_gpseph( iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: calculate Jo and Grad_y(Jo) for gpseph obs
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

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_gpseph")

   jo % gpseph_eph = 0.0

   do n=1, iv%info(gpseph)%nlocal

      do k=1, iv%info(gpseph)%levels(n)
         if ( iv%gpseph(n)%eph(k)%qc >= obs_qc_pointer ) then
            jo_grad_y%gpseph(n)%eph(k) = -re%gpseph(n)%eph(k) / &
               ( iv%gpseph(n)%eph(k)%error * iv%gpseph(n)%eph(k)%error)
         endif
      end do

      if (iv%info(gpseph)%proc_domain(1,n)) then
         do k=1, iv%info(gpseph)%levels(n)
            if ( iv%gpseph(n)%eph(k)%qc >= obs_qc_pointer ) then
               jo % gpseph_eph = jo % gpseph_eph - &
                   re%gpseph(n)%eph(k) * jo_grad_y%gpseph(n)%eph(k)
            endif
         end do
      end if

   end do

   jo % gpseph_eph = 0.5 * jo % gpseph_eph

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_gpseph")

end subroutine da_jo_and_grady_gpseph
