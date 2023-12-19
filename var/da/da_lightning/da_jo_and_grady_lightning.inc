subroutine da_jo_and_grady_lightning(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP) 
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(in)    :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo          ! Obs cost function.
   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_lightning")
   
   ! defined in da_define_structure.f90
   jo % lightning_w   = 0.0
   jo % lightning_div = 0.0
   jo % lightning_qv  = 0.0 
  
   do n = 1, iv%info(lightning)%nlocal
      do k = 2, iv%info(lightning)%levels(n)
	 if(use_lightning_w) then
           jo_grad_y%lightning(n)%w(k) = -re%lightning(n)%w(k)/(iv%lightning(n)%w(k)%error * iv%lightning(n)%w(k)%error)
         end if
		
         if(use_lightning_div) then
           jo_grad_y%lightning(n)%div(k) = -re%lightning(n)%div(k)/(iv%lightning(n)%div(k)%error * iv%lightning(n)%div(k)%error)
         end if
 
	 if(use_lightning_qv) then
           jo_grad_y%lightning(n)%qv(k) = -re%lightning(n)%qv(k)/(iv%lightning(n)%qv(k)%error * iv%lightning(n)%qv(k)%error)
         end if		 
      end do

      if(iv%info(lightning)%proc_domain(1,n)) then
        do k = 2, iv%info(lightning)%levels(n)

           if(use_lightning_w) then
             jo%lightning_w = jo%lightning_w-re%lightning(n)%w(k)*jo_grad_y%lightning(n)%w(k)
           end if                           

	   if(use_lightning_div) then
             jo%lightning_div = jo%lightning_div-re%lightning(n)%div(k) * jo_grad_y%lightning(n)%div(k)
           end if			    

	   if(use_lightning_qv) then
             jo%lightning_qv = jo%lightning_qv-re%lightning(n)%qv(k)*jo_grad_y%lightning(n)%qv(k)
           end if		

        end do
      end if
   
   end do

   jo%lightning_w   = 0.5 * jo % lightning_w
   jo%lightning_div = 0.5 * jo % lightning_div
   jo%lightning_qv  = 0.5 * jo % lightning_qv

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_lightning")

end subroutine da_jo_and_grady_lightning


