subroutine da_transform_xtoy_polaramv (grid, iv, y)

   !----------------------------------------------------------------------
   ! Purpose: X to Y Transform operator for Polar AMV's               
   !----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer           :: n ! Loop counter.
   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_polaramv")

   allocate (u(iv%info(polaramv)%max_lev,iv%info(polaramv)%n1:iv%info(polaramv)%n2))
   allocate (v(iv%info(polaramv)%max_lev,iv%info(polaramv)%n1:iv%info(polaramv)%n2))

   call da_interp_lin_3d (grid%xa%u, iv%info(polaramv), u)
   call da_interp_lin_3d (grid%xa%v, iv%info(polaramv), v)

   do n=iv%info(polaramv)%n1,iv%info(polaramv)%n2
      y%polaramv(n)%u(:) = u(1:size(y%polaramv(n)%u),n)
      y%polaramv(n)%v(:) = v(1:size(y%polaramv(n)%v),n)
   end do

   deallocate (u)
   deallocate (v)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_polaramv")

end subroutine da_transform_xtoy_polaramv


