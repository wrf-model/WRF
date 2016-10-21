subroutine da_transform_vtod_wpec(cv_size, be, ep, cv, vp, vv, xbx, grid)

   !----------------------------------------------------------------------
   ! Purpose:  Transform control variable (V) to observation-space (Y)
   !----------------------------------------------------------------------

   implicit none

   integer,                    intent(in)    :: cv_size ! Size of cv array.
   type(be_type),              intent(in)    :: be     ! background error structure.
   type(ep_type),              intent(in)    :: ep     ! Ensemble perturbation structure.
   real,                       intent(in)    :: cv(1:cv_size)     ! control variables.
   type(vp_type),              intent(inout) :: vp     ! Grdipt/level CV.
   type(vp_type),              intent(inout) :: vv     ! Grdipt/EOF CV.
   type(xbx_type),             intent(inout) :: xbx    ! For header & non-grid arrays.
   type(domain),               intent(inout) :: grid


   if (trace_use) call da_trace_entry("da_transform_vtod_wpec")

      call da_transform_vtox(grid, cv_size, xbx, be, ep, cv, vv, vp)

      if ( be%ne > 0 .and. alphacv_method == alphacv_method_xa ) then
         call da_transform_vpatox(grid, be, ep, vp)
         call da_add_xa(grid%xa, grid%xa_ens) !grid%xa = grid%xa + xa_ens
      end if

      call da_transform_xtoxa(grid, .true.)

      call da_wpec_constraint_lin(grid, xbx)


   if (trace_use) call da_trace_exit("da_transform_vtod_wpec")

end subroutine da_transform_vtod_wpec


