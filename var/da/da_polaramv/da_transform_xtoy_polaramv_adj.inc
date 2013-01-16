subroutine da_transform_xtoy_polaramv_adj (iv, jo_grad_y, jo_grad_x)

   !-------------------------------------------------------------------------
   ! Purpose: X to Y Transpose operator for Polar AMVs 
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer           :: n     ! Loop counter.
   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_polaramv_adj")

   allocate (u(iv%info(polaramv)%max_lev,iv%info(polaramv)%n1:iv%info(polaramv)%n2))
   allocate (v(iv%info(polaramv)%max_lev,iv%info(polaramv)%n1:iv%info(polaramv)%n2))

   do n=iv%info(polaramv)%n1,iv%info(polaramv)%n2
      u(1:size(jo_grad_y%polaramv(n)%u),n) = jo_grad_y%polaramv(n)%u(:)
      v(1:size(jo_grad_y%polaramv(n)%v),n) = jo_grad_y%polaramv(n)%v(:)
   end do

#ifdef A2C
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(polaramv), u,'u')
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(polaramv), v,'v')
#else
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(polaramv), u)
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(polaramv), v)
#endif

   deallocate (u)
   deallocate (v)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_polaramv_adj")

end subroutine da_transform_xtoy_polaramv_adj


