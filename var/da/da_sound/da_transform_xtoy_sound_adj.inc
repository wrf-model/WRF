subroutine da_transform_xtoy_sound_adj(iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
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

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_sound_adj")

   allocate (u(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
   allocate (v(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
   allocate (t(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
   allocate (q(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))

   do n=iv%info(sound)%n1,iv%info(sound)%n2
      u(1:size(jo_grad_y%sound(n)%u),n) = jo_grad_y%sound(n)%u(:)
      v(1:size(jo_grad_y%sound(n)%v),n) = jo_grad_y%sound(n)%v(:)
      t(1:size(jo_grad_y%sound(n)%t),n) = jo_grad_y%sound(n)%t(:)
      q(1:size(jo_grad_y%sound(n)%q),n) = jo_grad_y%sound(n)%q(:)
   end do

#ifdef A2C
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(sound), u,'u')
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(sound), v,'v')
#else
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(sound), u)
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(sound), v)
#endif
   call da_interp_lin_3d_adj (jo_grad_x%t, iv%info(sound), t)
   call da_interp_lin_3d_adj (jo_grad_x%q, iv%info(sound), q)
   
   deallocate (u)
   deallocate (v)
   deallocate (t)
   deallocate (q)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_sound_adj")

end subroutine da_transform_xtoy_sound_adj


