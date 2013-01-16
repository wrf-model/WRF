subroutine da_transform_xtoy_qscat_adj(iv, jo_grad_y, jo_grad_x)

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

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_qscat_adj")

   allocate (u(1,iv%info(qscat)%n1:iv%info(qscat)%n2))
   allocate (v(1,iv%info(qscat)%n1:iv%info(qscat)%n2))

   do n=iv%info(qscat)%n1,iv%info(qscat)%n2
      u(1,n) = jo_grad_y%qscat(n)%u
      v(1,n) = jo_grad_y%qscat(n)%v
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

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_qscat_adj")

end subroutine da_transform_xtoy_qscat_adj


