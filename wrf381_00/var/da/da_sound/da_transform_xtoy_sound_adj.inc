subroutine da_transform_xtoy_sound_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none
   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n, k

   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)
   real, allocatable :: t(:,:)
   real, allocatable :: q(:,:)

   real, allocatable :: ub(:,:)
   real, allocatable :: vb(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_sound_adj")

   allocate (u(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
   allocate (v(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
   allocate (t(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
   allocate (q(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))

   allocate (ub(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
   allocate (vb(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))

   call da_interp_lin_3d (grid%xb%u, iv%info(sound), ub)
   call da_interp_lin_3d (grid%xb%v, iv%info(sound), vb)

   do n=iv%info(sound)%n1,iv%info(sound)%n2
       do k = 1, iv%info(sound)%levels(n)
         if(wind_sd_sound) then
            call da_uv_to_sd_adj(jo_grad_y%sound(n)%u(k), &
                                 jo_grad_y%sound(n)%v(k), u(k,n), v(k,n), ub(k,n), vb(k,n))
         else
            u(k,n) = jo_grad_y%sound(n)%u(k)
            v(k,n) = jo_grad_y%sound(n)%v(k)
         end if
      end do
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
   deallocate (ub)
   deallocate (vb)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_sound_adj")

end subroutine da_transform_xtoy_sound_adj


