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
   jo % radar_rrn = 0.0
   jo % radar_rsn = 0.0
   jo % radar_rgr = 0.0
   jo % radar_rqv = 0.0 

   do n=1, iv%info(radar)%nlocal
      do k=1, iv%info(radar)%levels(n)
         if (use_radar_rv) then
            jo_grad_y%radar(n)%rv(k) = -re%radar(n)%rv(k) / (iv%radar(n)%rv(k)%error * iv%radar(n)%rv(k)%error) 
         end if

         if (use_radar_rf) then
            jo_grad_y%radar(n)%rf(k) = -re%radar(n)%rf(k) / (iv%radar(n)%rf(k)%error * iv%radar(n)%rf(k)%error) 
         end if

         if (use_radar_rhv) then
            jo_grad_y%radar(n)%rrn(k) = -re%radar(n)%rrn(k) / (iv%radar(n)%rrn(k)%error * iv%radar(n)%rrn(k)%error) 
            jo_grad_y%radar(n)%rsn(k) = -re%radar(n)%rsn(k) / (iv%radar(n)%rsn(k)%error * iv%radar(n)%rsn(k)%error) 
            jo_grad_y%radar(n)%rgr(k) = -re%radar(n)%rgr(k) / (iv%radar(n)%rgr(k)%error * iv%radar(n)%rgr(k)%error) 
         end if

         if (use_radar_rqv) then
            jo_grad_y%radar(n)%rqv(k) = -re%radar(n)%rqv(k) / (iv%radar(n)%rqv(k)%error * iv%radar(n)%rqv(k)%error) 
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
       
            if (use_radar_rhv) then
               jo % radar_rrn = jo % radar_rrn - re%radar(n)%rrn(k) * jo_grad_y%radar(n)%rrn(k)
               jo % radar_rsn = jo % radar_rsn - re%radar(n)%rsn(k) * jo_grad_y%radar(n)%rsn(k)
               jo % radar_rgr = jo % radar_rgr - re%radar(n)%rgr(k) * jo_grad_y%radar(n)%rgr(k)
            end if

            if (use_radar_rqv) then
               jo % radar_rqv = jo % radar_rqv - re%radar(n)%rqv(k) * jo_grad_y%radar(n)%rqv(k)
            end if
         end do
      end if
   end do
      
   jo % radar_rv = 0.5 * jo % radar_rv
   jo % radar_rf = 0.5 * jo % radar_rf
   jo % radar_rrn= 0.5 * jo % radar_rrn
   jo % radar_rsn= 0.5 * jo % radar_rsn
   jo % radar_rgr= 0.5 * jo % radar_rgr
   jo % radar_rqv= 0.5 * jo % radar_rqv

   if (trace_use) call da_trace_exit("da_jo_and_grady_radar")

end subroutine da_jo_and_grady_radar


