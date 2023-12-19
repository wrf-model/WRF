subroutine da_transform_xtoy_lightning (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP) 
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer :: n               ! Loop counter.
   integer :: i, j, k         ! Index dimension.
   real    :: dx, dxm  !
   real    :: dy, dym  !
   integer :: num_levs ! obs vertical levels
   
   real    :: div(kts:kte)      !Model divergence at ob loc   
   real    :: ave_div(kts:kte)  !Model averaged divergence at ob loc
   real    :: model_q(kts:kte)  !Model Q at ob loc
   real    :: model_t(kts:kte)  !Model T at ob loc

   real    :: TGL_div(kts:kte)
   real    :: TGL_model_q(kts:kte)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_lightning")
   
   do n = iv%info(lightning)%n1, iv%info(lightning)%n2

      num_levs = iv%info(lightning)%levels(n)

      ! [1.0] Get horizontal interpolation weights:

      i   = iv%info(lightning)%i(1,n)
      dy  = iv%info(lightning)%dy(1,n)
      dym = iv%info(lightning)%dym(1,n)
      j   = iv%info(lightning)%j(1,n)
      dx  = iv%info(lightning)%dx(1,n)
      dxm = iv%info(lightning)%dxm(1,n)
	  
      TGL_div(:) = 0.0
      do k= 1, num_levs	  

         if(use_lightning_w) then
           if(iv%lightning(n)%w(k)%qc ==  missing_data) then
             y%lightning(n)%w(k) = 0.0
           else
             y%lightning(n)%w(k) = grid%xa%w(i,j,k)
           end if
         end if

         if(use_lightning_div) then
           if(iv%lightning(n)%div(k)%qc ==  missing_data) then
             y%lightning(n)%div(k) = 0.0
           else		   
             call da_div_profile_tl(grid, iv%info(lightning), n, k, TGL_div(k))                 ! divergence profile
	     y%lightning(n)%div(k) = TGL_div(k)
           end if
	 end if

         if(use_lightning_qv) then
           if(iv%lightning(n)%qv(k)%qc == missing_data) then
             y%lightning(n)%qv(k) = 0.0
           else			  
             y%lightning(n)%qv(k) = grid%xa%q(i,j,k)
             y%lightning(n)%qv(k) = y%lightning(n)%qv(k) + (17.67*243.5/(grid%xb%t(i,j,k)+243.5)**2.0)*grid%xb%q(i,j,k)*grid%xa%t(i,j,k)
           end if
         end if

      end do
   end do	  
   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_lightning")

end subroutine da_transform_xtoy_lightning
