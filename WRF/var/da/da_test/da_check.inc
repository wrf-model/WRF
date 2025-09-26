subroutine da_check(grid, config_flags, cv_size, xbx, be, ep, iv, vv, vp, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain),               intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   integer, intent(in)              :: cv_size ! Size of cv array.
   type (xbx_type),   intent(inout) :: xbx   ! Header & non-gridded vars.
   type (be_type),    intent(in)    :: be    ! background error structure.
   type (ep_type),    intent(in)    :: ep    ! Ensemble perturbation structure.
   type (iv_type),    intent(inout) :: iv    ! ob. increment vector.
   type (vp_type),    intent(inout) :: vv    ! Grdipt/EOF CV.
   type (vp_type),    intent(inout) :: vp    ! Grdipt/level CV.
   type (y_type),     intent(inout) :: y             ! y = h (xa)

   integer :: sizec
   real    :: cvtest(1:cv_size)    ! background error structure.
   real    :: field(its:ite,jts:jte) ! Field for spectral transform test.

   call da_trace_entry("da_check")

   !----------------------------------------------------------------------------
   ! [1] Set up test data:
   !----------------------------------------------------------------------------

   ! Initialize cv values with random data:
   call random_number(cvtest(:))
   cvtest(:) = cvtest(:) - 0.5

   ! vv arrays initialized already.
   ! vp arrays initialized already.

   !----------------------------------------------------------------------------
   ! [2] Perform vtox adjoint tests:
   !----------------------------------------------------------------------------

   call da_message((/"Performing vtox adjoint tests"/))

   ! v_to_vv adjoint test:

   call da_check_cvtovv_adjoint(grid, cv_size, xbx, be, cvtest, vv)

   !-------------------------------------------------------------------------
   ! vv_to_vp adjoint test:
   !-------------------------------------------------------------------------

   call da_check_vvtovp_adjoint(grid, be % ne, grid%xb, be, vv, vp)

   !-------------------------------------------------------------------------
   ! vptox adjoint test:
   !-------------------------------------------------------------------------

   call da_check_vptox_adjoint(grid, be % ne, be, ep, vp, cv_size)

   !-------------------------------------------------------------------------
   ! vtox adjoint test: <x,x> = <v_adj,v>
   !-------------------------------------------------------------------------

   call da_check_vtox_adjoint(grid, cv_size, xbx, be, ep, cvtest, vv, vp)

   !----------------------------------------------------------------------------
   ! [2] Perform xtoy adjoint tests:
   !----------------------------------------------------------------------------

   call da_message((/"Performing xtoy adjoint tests"/))

   call da_allocate_y(iv, y)
   call da_zero_x(grid%xa)

   call da_setup_testfield(grid)

   ! WHY?
   ! Make cv_array random.

   ! call random_number(cvtest(1:cv_size))
   ! cvtest(1:cv_size) = cvtest(1:cv_size) - 0.5

   ! call da_transform_vtox(grid, cv_size, xbx, be, ep, cvtest, vv, vp)

   call da_check_xtoy_adjoint(cv_size, cvtest, xbx, be, grid, config_flags, iv, y)

   call da_deallocate_y(y)

   !----------------------------------------------------------------------------
   ! [3] Perform dynamical constraint test:
   !----------------------------------------------------------------------------
   call da_message((/"Performing dynamical constraint adjoint tests"/))
   call da_zero_x(grid%xa)
   call da_setup_testfield(grid)
   call da_check_dynamics_adjoint(cv_size, cvtest, xbx, be, grid, config_flags)

   !----------------------------------------------------------------------------
   ! [4] Perform spectral test:
   !----------------------------------------------------------------------------

   if (global) then

      call da_message((/"Performing spectral tests"/))

      call random_number(field(:,:))
      field(:,:) = field(:,:) - 0.5

      sizec = (be % max_wave+1) * (be % max_wave+2)/2
      call da_test_spectral(be % max_wave, sizec, xbx, field)

   end if

   call da_trace_exit("da_check")

end subroutine da_check


