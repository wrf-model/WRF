subroutine da_transform_xtoy_pilot_adj(iv, jo_grad_y, jo_grad_x)

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

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_pilot_adj")

   allocate (model_u(iv%info(pilot)%max_lev,iv%info(pilot)%n1:iv%info(pilot)%n2))
   allocate (model_v(iv%info(pilot)%max_lev,iv%info(pilot)%n1:iv%info(pilot)%n2))

   do n=iv%info(pilot)%n1,iv%info(pilot)%n2
      model_u(1:iv%info(pilot)%levels(n),n) = jo_grad_y%pilot(n)%u(:)
      model_v(1:iv%info(pilot)%levels(n),n) = jo_grad_y%pilot(n)%v(:)
   end do

#ifdef A2C
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(pilot), model_u,'u')
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(pilot), model_v,'v')
#else
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(pilot), model_u)
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(pilot), model_v)
#endif

   deallocate(model_u)
   deallocate(model_v)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_pilot_adj")

end subroutine da_transform_xtoy_pilot_adj


