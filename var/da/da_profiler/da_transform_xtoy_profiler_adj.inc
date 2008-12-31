subroutine da_transform_xtoy_profiler_adj(iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n  ! Loop counter.

   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_profiler_adj")

   allocate (model_u(iv%info(profiler)%max_lev,iv%info(profiler)%n1:iv%info(profiler)%n2))
   allocate (model_v(iv%info(profiler)%max_lev,iv%info(profiler)%n1:iv%info(profiler)%n2))

   do n=iv%info(profiler)%n1,iv%info(profiler)%n2
      model_u(1:iv%info(profiler)%levels(n),n) = jo_grad_y%profiler(n)%u(:)
      model_v(1:iv%info(profiler)%levels(n),n) = jo_grad_y%profiler(n)%v(:)
   end do

#ifdef A2C
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(profiler), model_u,'u')
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(profiler), model_v,'v')
#else
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(profiler), model_u)
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(profiler), model_v)
#endif

   deallocate(model_u)
   deallocate(model_v)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_profiler_adj")

end subroutine da_transform_xtoy_profiler_adj


