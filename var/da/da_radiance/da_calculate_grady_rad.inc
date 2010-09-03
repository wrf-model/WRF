
subroutine da_calculate_grady_rad( iv, re, jo_grad_y ) 
!------------------------------------------------------------------------------
!  Purpose: Calculate Gradient_y for radiance data.
!
!  METHOD:  grad_y = -R^-1 (d - H delta_x ).
!
!  HISTORY: 08/2005 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------
   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type) , intent(inout) :: re          ! Residual vector.
   type (y_type) , intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer                       :: n, k, i

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_rad")
   do i =1, iv%num_inst
      if ( iv%instid(i)%num_rad < 1 .or. iv%instid(i)%rad_monitoring == monitor_on ) cycle
      do n=1, iv%instid(i)%num_rad
         do k=1, iv%instid(i)%nchan
            if ( iv%instid(i)%tb_qc(k,n) < obs_qc_pointer) re%instid(i)%tb(k,n) = 0.0

            jo_grad_y%instid(i)%tb(k,n) = -re%instid(i)%tb(k,n) / (iv%instid(i)%tb_error(k,n) * iv%instid(i)%tb_error(k,n))


         end do
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_rad")

end subroutine da_calculate_grady_rad

