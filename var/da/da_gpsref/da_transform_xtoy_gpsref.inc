subroutine da_transform_xtoy_gpsref (grid, iv, y)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   !-------------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer :: n  ! Loop counter.

   real, allocatable :: model_ref(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_gpsref")

   allocate (model_ref(iv%info(gpsref)%max_lev,iv%info(gpsref)%n1:iv%info(gpsref)%n2))

   call da_interp_lin_3d (grid%xa%ref, iv%info(gpsref), model_ref)

   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
      y%gpsref(n)%ref(:) = model_ref(1:iv%info(gpsref)%levels(n),n)
   end do

   deallocate (model_ref)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_gpsref")

end subroutine da_transform_xtoy_gpsref


