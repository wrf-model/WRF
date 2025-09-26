subroutine da_transform_xtoy_gpsref_adj(iv, jo_grad_y, jo_grad_x)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n  ! Loop counter.

   real, allocatable :: model_ref(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_gpsref_adj")

   allocate (model_ref(iv%info(gpsref)%max_lev,iv%info(gpsref)%n1:iv%info(gpsref)%n2))

   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
      model_ref(1:iv%info(gpsref)%levels(n),n) = jo_grad_y%gpsref(n)%ref(:)
   end do

   call da_interp_lin_3d_adj (jo_grad_x%ref, iv%info(gpsref), model_ref)

   deallocate(model_ref)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_gpsref_adj")

end subroutine da_transform_xtoy_gpsref_adj


