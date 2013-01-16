subroutine da_transform_xtoy_airep_adj(iv, jo_grad_y, jo_grad_x)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !--------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n

   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)
   real, allocatable :: t(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_airep_adj")

   allocate (u(iv%info(airep)%max_lev,iv%info(airep)%n1:iv%info(airep)%n2))
   allocate (v(iv%info(airep)%max_lev,iv%info(airep)%n1:iv%info(airep)%n2))
   allocate (t(iv%info(airep)%max_lev,iv%info(airep)%n1:iv%info(airep)%n2))

   do n=iv%info(airep)%n1,iv%info(airep)%n2
      u(1:size(jo_grad_y%airep(n)%u),n) = jo_grad_y%airep(n)%u(:)
      v(1:size(jo_grad_y%airep(n)%v),n) = jo_grad_y%airep(n)%v(:)
      t(1:size(jo_grad_y%airep(n)%t),n) = jo_grad_y%airep(n)%t(:)
   end do

#ifdef A2C
   call da_interp_lin_3d_adj(jo_grad_x%u, iv%info(airep), u,'u')
   call da_interp_lin_3d_adj(jo_grad_x%v, iv%info(airep), v,'v')
#else
   call da_interp_lin_3d_adj(jo_grad_x%u, iv%info(airep), u)
   call da_interp_lin_3d_adj(jo_grad_x%v, iv%info(airep), v)
#endif
   call da_interp_lin_3d_adj(jo_grad_x%t, iv%info(airep), t)

   deallocate (u)
   deallocate (v)
   deallocate (t)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_airep_adj")

end subroutine da_transform_xtoy_airep_adj


