subroutine da_calculate_grady_radar(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer  :: n, k

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_radar")

   do n=1, iv%info(radar)%nlocal
      do k=1, iv%info(radar)%levels(n)
         if (use_radar_rv) then
            if (iv%radar(n)%rv(k)%qc < obs_qc_pointer) then
               re%radar(n)%rv(k) = 0.0
            end if

            jo_grad_y%radar(n)%rv(k) = -re%radar(n)%rv(k) / &
               (iv%radar(n)%rv(k)%error * iv%radar(n)%rv(k)%error)
         end if

         if (use_radar_rf) then
            if (iv%radar(n)%rf(k)%qc < obs_qc_pointer) then
               re%radar(n)%rf(k) = 0.0
            end if
            jo_grad_y%radar(n)%rf(k) = -re%radar(n)%rf(k) / &
                (iv%radar(n)%rf(k)%error * iv%radar(n)%rf(k)%error)
         end if
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_radar")

end subroutine da_calculate_grady_radar


