subroutine da_jo_and_grady_rad(iv, re, jo, jo_grad_y) 

   !---------------------------------------------------------------------------
   ! Purpose: Calculate Gradient_y i and cost function Jo for radiance data.
   !
   ! Method:  grad_y = -R^-1 (d - H delta_x)
   !              Jo = -(d - H delta_x) grad_y
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type) , intent(in)    :: re          ! Residual vector.
   type (y_type) , intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo          ! Obs cost function.

   integer                       :: n, k, i

   if (trace_use) call da_trace_entry("da_jo_and_grady_rad")

   do i =1, iv%num_inst

      jo % rad(i)%jo_ichan(:) = 0.0
      jo % rad(i)%num_ichan(:) = 0

      if (iv%instid(i)%num_rad < 1 .or. iv%instid(i)%rad_monitoring == monitor_on ) cycle

      do n=1, iv%instid(i)%num_rad
         do k=1, iv%instid(i)%nchan
            jo_grad_y%instid(i)%tb(k,n) = -re%instid(i)%tb(k,n) / &
               (iv%instid(i)%tb_error(k,n) * iv%instid(i)%tb_error(k,n))
         end do
         if (iv%instid(i)%info%proc_domain(1,n)) then
            do k=1, iv%instid(i)%nchan
               if (iv%instid(i)%tb_qc(k,n) >= obs_qc_pointer) then
                  jo % rad(i) % jo_ichan(k) = jo % rad(i) % jo_ichan(k) - &
                     re%instid(i)%tb(k,n) * jo_grad_y%instid(i)%tb(k,n)
                  jo % rad(i) % num_ichan(k) = jo % rad(i) % num_ichan(k) + 1
               end if
            end do
         end if
      end do
      jo % rad(i)%jo_ichan(:) = 0.5 * jo % rad(i)%jo_ichan(:)
   end do

   if (trace_use) call da_trace_exit("da_jo_and_grady_rad")

end subroutine da_jo_and_grady_rad


