subroutine da_transform_xtoy_qscat_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none
   type (domain),  intent(in)    :: grid          ! first guess state.
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n        ! Loop counter.

   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)
   real, allocatable :: ub(:,:)
   real, allocatable :: vb(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_qscat_adj")

   allocate (u(1,iv%info(qscat)%n1:iv%info(qscat)%n2))
   allocate (v(1,iv%info(qscat)%n1:iv%info(qscat)%n2))

   allocate (ub(1,iv%info(qscat)%n1:iv%info(qscat)%n2))
   allocate (vb(1,iv%info(qscat)%n1:iv%info(qscat)%n2))

   call da_interp_lin_3d (grid%xb%u, iv%info(qscat), ub)
   call da_interp_lin_3d (grid%xb%v, iv%info(qscat), vb)

   do n=iv%info(qscat)%n1,iv%info(qscat)%n2
      if (wind_sd_qscat) then
          call da_uv_to_sd_adj(jo_grad_y%qscat(n)%u, &
                               jo_grad_y%qscat(n)%v, u(1,n), v(1,n), ub(1,n), vb(1,n))
      else
          u(1,n) = jo_grad_y%qscat(n)%u
          v(1,n) = jo_grad_y%qscat(n)%v
      end if
   end do

#ifdef A2C
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(qscat), u,'u')
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(qscat), v,'v')
#else
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(qscat), u)
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(qscat), v)
#endif

   deallocate (u)
   deallocate (v)
   deallocate (ub)
   deallocate (vb)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_qscat_adj")

end subroutine da_transform_xtoy_qscat_adj


