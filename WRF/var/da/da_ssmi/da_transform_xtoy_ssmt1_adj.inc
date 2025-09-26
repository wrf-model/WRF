subroutine da_transform_xtoy_ssmt1_adj(iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n  ! Loop counter.

   real, allocatable :: t(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_ssmt1_adj") 

   allocate (t(iv%info(ssmt1)%max_lev,iv%info(ssmt1)%n1:iv%info(ssmt1)%n2))

   do n=iv%info(ssmt1)%n1,iv%info(ssmt1)%n2
      t(1:iv%info(ssmt1)%levels(n),n) = jo_grad_y%ssmt1(n)%t(:)
   end do

   call da_interp_lin_3d_adj (jo_grad_x%t, iv%info(ssmt1), t)

   deallocate (t)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_ssmt1_adj") 

end subroutine da_transform_xtoy_ssmt1_adj


