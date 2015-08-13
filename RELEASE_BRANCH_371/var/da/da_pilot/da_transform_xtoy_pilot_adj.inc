subroutine da_transform_xtoy_pilot_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none
   type(domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n, k  ! Loop counter.

   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: ub(:,:)
   real, allocatable :: vb(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_pilot_adj")

   allocate (model_u(iv%info(pilot)%max_lev,iv%info(pilot)%n1:iv%info(pilot)%n2))
   allocate (model_v(iv%info(pilot)%max_lev,iv%info(pilot)%n1:iv%info(pilot)%n2))
   allocate (ub(iv%info(pilot)%max_lev,iv%info(pilot)%n1:iv%info(pilot)%n2))
   allocate (vb(iv%info(pilot)%max_lev,iv%info(pilot)%n1:iv%info(pilot)%n2))
   call da_interp_lin_3d (grid%xb%u, iv%info(pilot), ub)
   call da_interp_lin_3d (grid%xb%v, iv%info(pilot), vb)

   do n=iv%info(pilot)%n1,iv%info(pilot)%n2
       do k = 1, iv%info(pilot)%levels(n)

         if(wind_sd_pilot) then
            call da_uv_to_sd_adj(jo_grad_y%pilot(n)%u(k), &
                                 jo_grad_y%pilot(n)%v(k), model_u(k,n), model_v(k,n), ub(k,n), vb(k,n))
         else
             model_u(k,n) = jo_grad_y%pilot(n)%u(k)
             model_v(k,n) = jo_grad_y%pilot(n)%v(k)
         end if

      end do

   end do

#ifdef A2C
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(pilot), model_u,'u')
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(pilot), model_v,'v')
#else
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(pilot), model_u)
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(pilot), model_v)
#endif
   call da_interp_lin_3d (grid%xb%u, iv%info(pilot), ub)
   call da_interp_lin_3d (grid%xb%v, iv%info(pilot), vb)

   deallocate(model_u)
   deallocate(model_v)
   deallocate (ub)
   deallocate (vb)
   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_pilot_adj")

end subroutine da_transform_xtoy_pilot_adj


