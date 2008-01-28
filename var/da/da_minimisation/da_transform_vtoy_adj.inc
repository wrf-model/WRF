subroutine da_transform_vtoy_adj(iter,cv_size, be, ep, cv, iv, vp, vv, xbx, y, &
   grid, config_flags, jcdf_flag)

   !-------------------------------------------------------------------------
   ! Purpose:  Does Adjoint of control variable (V) transform to Obs-space(Y)
   !-------------------------------------------------------------------------

   implicit none

   integer,                    intent(in)    :: iter
   integer,                    intent(in)    :: cv_size ! Size of cv array.
   type(be_type),              intent(in)    :: be     ! background error structure.
   type(ep_type),              intent(in)    :: ep     ! ensemble perturbation structure.
   real,                       intent(out)   :: cv(1:cv_size) ! control variables.
   type(iv_type),              intent(inout) :: iv     ! innovation vector (o-b).
   type(vp_type),              intent(inout) :: vp     ! Grdipt/level CV.
   type(vp_type),              intent(inout) :: vv     ! Grdipt/EOF CV.
   type(xbx_type),             intent(in)    :: xbx    ! For header & non-grid arrays.
   type(y_type),               intent(inout) :: y      ! y = H(x_inc).
   type(domain),               intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags
   logical,                    intent(in)    :: jcdf_flag       ! additional flag to switch off JcDF, used to switch off JcDF in calculation of J.

   type (xbx_type)                           :: xbx_tmp

   integer :: nobwin,ndynopt
#ifdef DM_PARALLEL
   integer :: wrf_done_unit
#endif

   character(len=4) :: filnam

   call da_trace_entry("da_transform_vtoy_adj")

   iv%time = 1
   iv%info(:)%n1 = iv%info(:)%plocal(iv%time-1) + 1
   iv%info(:)%n2 = iv%info(:)%plocal(iv%time)
   call da_zero_x(grid%xa)
   call da_transform_xtoy_adj(grid, iv, y,grid%xa)

   cv = 0.0
   call da_transform_vtox_adj(grid, cv_size, xbx, be, ep, vp, vv, cv)

   call da_trace_exit("da_transform_vtoy_adj")

end subroutine da_transform_vtoy_adj


