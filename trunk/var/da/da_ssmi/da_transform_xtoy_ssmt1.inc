subroutine da_transform_xtoy_ssmt1 (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n  ! Loop counter.

   real, allocatable :: t (:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_ssmt1") 

   allocate (t(iv%info(ssmt1)%max_lev,iv%info(ssmt1)%n1:iv%info(ssmt1)%n2))

   call da_interp_lin_3d (grid%xa % t, iv%info(ssmt1), t)

   do n=iv%info(ssmt1)%n1,iv%info(ssmt1)%n2
      y%ssmt1(n)%t(:) = t(1:iv%info(ssmt1)%levels(n),n)
   end do

   deallocate (t)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_ssmt1")

end subroutine da_transform_xtoy_ssmt1


