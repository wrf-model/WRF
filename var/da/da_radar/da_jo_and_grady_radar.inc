subroutine da_jo_and_grady_radar(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type),intent(inout) :: jo          ! Obs cost function.

   integer :: n, k

   if (trace_use) call da_trace_entry("da_jo_and_grady_radar")

   jo % radar_rv = 0.0
   jo % radar_rf = 0.0
   
   do n=1, iv%info(radar)%nlocal
      do k=1, iv%info(radar)%levels(n)
         if (use_radar_rv) then
            jo_grad_y%radar(n)%rv(k) = -re%radar(n)%rv(k) / &
               (iv%radar(n)%rv(k)%error * iv%radar(n)%rv(k)%error)
         end if

         if (use_radar_rf) then
            jo_grad_y%radar(n)%rf(k) = -re%radar(n)%rf(k) / &
               (iv%radar(n)%rf(k)%error * iv%radar(n)%rf(k)%error)
         end if
      end do

      if (iv%info(radar)%proc_domain(1,n)) then
         do k=1, iv%info(radar)%levels(n)
            if (use_radar_rv) then
               jo % radar_rv = jo % radar_rv - re%radar(n)%rv(k) * jo_grad_y%radar(n)%rv(k)
            end if

            if (use_radar_rf) then
               jo % radar_rf = jo % radar_rf - re%radar(n)%rf(k) * jo_grad_y%radar(n)%rf(k)
            end if
         end do
      end if
   end do
      
   jo % radar_rv = 0.5 * jo % radar_rv
   jo % radar_rf = 0.5 * jo % radar_rf

   if (trace_use) call da_trace_exit("da_jo_and_grady_radar")

end subroutine da_jo_and_grady_radar


