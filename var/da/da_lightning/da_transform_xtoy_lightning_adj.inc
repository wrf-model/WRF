subroutine da_transform_xtoy_lightning_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP)    
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n, k       ! Loop counter.
   integer :: num_levs ! obs vertical levels

   integer :: i, j         ! Index dimension.
   real    :: dx, dxm  !
   real    :: dy, dym  !
   
   real    :: div(kts:kte)      !Model divergence at ob loc   
   real    :: ave_div(kts:kte)  !Model averaged divergence at ob loc
   real    :: model_q(kts:kte) !Model RH at ob loc
   real    :: model_t(kts:kte)  !Model T at ob loc

   real    :: ADJ_div(kts:kte)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_lightning_adj")
    
   do n = iv%info(lightning)%n1, iv%info(lightning)%n2
      num_levs = iv%info(lightning)%levels(n)
	  
      ! [1.0] Get horizontal interpolation weights:

      i   = iv%info(lightning)%i(1,n)
      dy  = iv%info(lightning)%dy(1,n)
      dym = iv%info(lightning)%dym(1,n)
      j   = iv%info(lightning)%j(1,n)
      dx  = iv%info(lightning)%dx(1,n)
      dxm = iv%info(lightning)%dxm(1,n)
	  
      ADJ_div(:)      = 0.0
	  
      do k = 1, num_levs
         if(use_lightning_w) then
           if(iv % lightning(n)%w(k)%qc /= missing_data) then
             jo_grad_x%w(i,j,k) = jo_grad_x%w(i,j,k) + jo_grad_y%lightning(n)%w(k)
           end if
         end if

	 if(use_lightning_div) then
           if(iv % lightning(n)%div(k)%qc /= missing_data) then	
	     call da_div_profile_adj(grid, jo_grad_x, iv%info(lightning), n, k, jo_grad_y%lightning(n)%div(k))			   
           end if
         end if
		 
	 if(use_lightning_qv) then
           if(iv % lightning(n)%qv(k)%qc /= missing_data) then
             jo_grad_x%q(i,j,k) = jo_grad_x%q(i,j,k) + jo_grad_y%lightning(n)%qv(k)
             jo_grad_x%t(i,j,k) = jo_grad_x%t(i,j,k) + (17.67*243.5/(grid%xb%t(i,j,k)+243.5)**2.0)*grid%xb%q(i,j,k)*jo_grad_y%lightning(n)%qv(k)
           end if
         end if	 
		 
      end do
	  
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_lightning_adj")

end subroutine da_transform_xtoy_lightning_adj
