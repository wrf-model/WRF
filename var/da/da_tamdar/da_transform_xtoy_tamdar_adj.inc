subroutine da_transform_xtoy_tamdar_adj(iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n

   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)
   real, allocatable :: t(:,:)
   real, allocatable :: q(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_tamdar_adj")

   allocate (u(iv%info(tamdar)%max_lev,iv%info(tamdar)%n1:iv%info(tamdar)%n2))
   allocate (v(iv%info(tamdar)%max_lev,iv%info(tamdar)%n1:iv%info(tamdar)%n2))
   allocate (t(iv%info(tamdar)%max_lev,iv%info(tamdar)%n1:iv%info(tamdar)%n2))
   allocate (q(iv%info(tamdar)%max_lev,iv%info(tamdar)%n1:iv%info(tamdar)%n2))

   do n=iv%info(tamdar)%n1,iv%info(tamdar)%n2
      u(1:size(jo_grad_y%tamdar(n)%u),n) = jo_grad_y%tamdar(n)%u(:)
      v(1:size(jo_grad_y%tamdar(n)%v),n) = jo_grad_y%tamdar(n)%v(:)
      t(1:size(jo_grad_y%tamdar(n)%t),n) = jo_grad_y%tamdar(n)%t(:)
      q(1:size(jo_grad_y%tamdar(n)%q),n) = jo_grad_y%tamdar(n)%q(:)
   end do

#ifdef A2C
   call da_interp_lin_3d (jo_grad_x%u, iv%info(tamdar), u, 'u')
   call da_interp_lin_3d (jo_grad_x%v, iv%info(tamdar), v, 'v')
#else
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(tamdar), u)
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(tamdar), v)
#endif
   call da_interp_lin_3d_adj (jo_grad_x%t, iv%info(tamdar), t)
   call da_interp_lin_3d_adj (jo_grad_x%q, iv%info(tamdar), q)

   deallocate (u)
   deallocate (v)
   deallocate (t)
   deallocate (q)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_tamdar_adj")

end subroutine da_transform_xtoy_tamdar_adj


