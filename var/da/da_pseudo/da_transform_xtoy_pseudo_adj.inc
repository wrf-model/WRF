subroutine da_transform_xtoy_pseudo_adj(iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n        ! Loop counter.

   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)
   real, allocatable :: q(:,:)
   real, allocatable :: p(:,:)
   real, allocatable :: t(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_pseudo_adj")

   allocate (u(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (v(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (q(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (p(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (t(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))

   do n=iv%info(pseudo)%n1,iv%info(pseudo)%n2
      u(1,n) = jo_grad_y%pseudo(n)%u
      v(1,n) = jo_grad_y%pseudo(n)%v
      q(1,n) = jo_grad_y%pseudo(n)%q
      p(1,n) = jo_grad_y%pseudo(n)%p
      t(1,n) = jo_grad_y%pseudo(n)%t
   end do

#ifdef A2C
   call da_interp_lin_3d_adj(jo_grad_x%u, iv%info(pseudo), u,'u')
   call da_interp_lin_3d_adj(jo_grad_x%v, iv%info(pseudo), v,'v')
#else
   call da_interp_lin_3d_adj(jo_grad_x%u, iv%info(pseudo), u)
   call da_interp_lin_3d_adj(jo_grad_x%v, iv%info(pseudo), v)
#endif
   call da_interp_lin_3d_adj(jo_grad_x%q, iv%info(pseudo), q)
   call da_interp_lin_3d_adj(jo_grad_x%p, iv%info(pseudo), p)
   call da_interp_lin_3d_adj(jo_grad_x%t, iv%info(pseudo), t)

   deallocate (u)
   deallocate (v)
   deallocate (q)
   deallocate (p)
   deallocate (t)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_pseudo_adj")

end subroutine da_transform_xtoy_pseudo_adj


