subroutine da_transform_xtoy_ssmt2_adj(iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer  :: n  ! Loop counter.

   real, allocatable :: rh(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_ssmt2_adj") 

   allocate (rh(1:iv%info(ssmt2)%max_lev,iv%info(ssmt2)%n1:iv%info(ssmt2)%n2))

   do n=iv%info(ssmt2)%n1,iv%info(ssmt2)%n2
      rh(1:iv%info(ssmt2)%levels(n),n) = jo_grad_y%ssmt2(n)%rh(:)
   end do

   call da_interp_lin_3d_adj (jo_grad_x%rh, iv%info(ssmt2), rh)

   deallocate (rh)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_ssmt2_adj") 

end subroutine da_transform_xtoy_ssmt2_adj


