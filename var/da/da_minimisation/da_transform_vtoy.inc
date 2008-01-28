subroutine da_transform_vtoy(cv_size, be, ep, cv, iv, vp, vv, xbx, &
                              y, &
                              grid, config_flags)

   !----------------------------------------------------------------------
   ! Purpose:  Does transform of control variable (V) to Obs-space (Y)
   !----------------------------------------------------------------------

   implicit none

   integer,                    intent(in)    :: cv_size ! Size of cv array.
   type(be_type),              intent(in)    :: be     ! background error structure.
   type(ep_type),              intent(in)    :: ep     ! Ensemble perturbation structure.
   real,                       intent(in)    :: cv(1:cv_size)     ! control variables.
   type(iv_type),              intent(inout) :: iv     ! innovation vector (o-b).
   type(vp_type),              intent(inout) :: vp     ! Grdipt/level CV.
   type(vp_type),              intent(inout) :: vv     ! Grdipt/EOF CV.
   type(xbx_type),             intent(in)    :: xbx    ! For header & non-grid arrays.
   type(y_type),               intent(inout) :: y      ! y = H(x_inc).
   type(domain),               intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags
   type (xbx_type)                    :: xbx_tmp

   integer :: nobwin
#ifdef DM_PARALLEL
   integer :: wrf_done_unit
#endif

   character(len=4) :: filnam

   if (trace_use) call da_trace_entry("da_transform_vtoy")

   call da_transform_vtox(grid, cv_size, xbx, be, ep, cv, vv, vp)

   call da_transform_xtoy(grid, iv, y)

   if (trace_use) call da_trace_exit("da_transform_vtoy")

end subroutine da_transform_vtoy


