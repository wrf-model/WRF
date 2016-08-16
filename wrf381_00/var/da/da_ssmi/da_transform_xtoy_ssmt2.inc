subroutine da_transform_xtoy_ssmt2 (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer  :: n  ! Loop counter.

   real, allocatable :: rh(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_ssmt2") 

   allocate (rh(1:iv%info(ssmt2)%max_lev,iv%info(ssmt2)%n1:iv%info(ssmt2)%n2))

   call da_interp_lin_3d (grid%xa%rh, iv%info(ssmt2), rh)

   do n=iv%info(ssmt2)%n1,iv%info(ssmt2)%n2
      y%ssmt2(n)%rh(:) = rh(1:iv%info(ssmt2)%levels(n),n)
   end do

   deallocate (rh)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_ssmt2") 

end subroutine da_transform_xtoy_ssmt2


