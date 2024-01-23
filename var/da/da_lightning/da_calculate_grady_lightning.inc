subroutine da_calculate_grady_lightning(iv, re, jo_grad_y)

   !----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP) 
   !------------------------------------------------------------------------------
   implicit none

   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer  :: n, k

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_lightning")

   do n = 1, iv%info(lightning)%nlocal
      do k = 2, iv%info(lightning)%levels(n)

         if(use_lightning_w) then
           if(iv%lightning(n)%w(k)%qc < obs_qc_pointer) then
             re%lightning(n)%w(k) = 0.0
           end if
           jo_grad_y%lightning(n)%w(k) = -re%lightning(n)%w(k) / (iv%lightning(n)%w(k)%error * iv%lightning(n)%w(k)%error)
         end if
 
         if(use_lightning_div) then
           if(iv%lightning(n)%div(k)%qc < obs_qc_pointer) then
             re%lightning(n)%div(k) = 0.0
           end if
           jo_grad_y%lightning(n)%div(k) = -re%lightning(n)%div(k) / (iv%lightning(n)%div(k)%error * iv%lightning(n)%div(k)%error)
         end if

         if(use_lightning_qv) then
           if(iv%lightning(n)%qv(k)%qc < obs_qc_pointer) then
             re%lightning(n)%qv(k) = 0.0
           end if
           jo_grad_y%lightning(n)%qv(k) = -re%lightning(n)%qv(k) / (iv%lightning(n)%qv(k)%error * iv%lightning(n)%qv(k)%error) 
         end if

      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_lightning")

end subroutine da_calculate_grady_lightning
