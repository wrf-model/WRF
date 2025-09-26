subroutine da_transform_vtod_wpec_adj(cv_size, be, ep, cv, vp, vv, xbx, grid)

   !-------------------------------------------------------------------------
   ! Purpose:  Does Adjoint of control variable (V) transform to Obs-space(Y)
   !-------------------------------------------------------------------------

   implicit none

   integer,                    intent(in)    :: cv_size ! Size of cv array.
   type(be_type),              intent(in)    :: be     ! background error structure.
   type(ep_type),              intent(in)    :: ep     ! ensemble perturbation structure.
   real,                       intent(out)   :: cv(1:cv_size) ! control variables.
   type(vp_type),              intent(inout) :: vp     ! Grdipt/level CV.
   type(vp_type),              intent(inout) :: vv     ! Grdipt/EOF CV.
   type(xbx_type),             intent(inout) :: xbx    ! For header & non-grid arrays.
   type(domain),               intent(inout) :: grid

   if (trace_use) call da_trace_entry("da_transform_vtod_wpec_adj")

      cv = 0.0

      call da_zero_x(grid%xa)
      call da_zero_vp_type(vp)

      call da_wpec_constraint_adj(grid, xbx)

      call da_transform_xtoxa_adj(grid, .true.)

      if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
         call da_transform_vpatox_adj(grid, be, ep, vp)
      end if

      call da_transform_vtox_adj(grid, cv_size, xbx, be, ep, vp, vv, cv)

   if (trace_use) call da_trace_exit("da_transform_vtod_wpec_adj")

end subroutine da_transform_vtod_wpec_adj


