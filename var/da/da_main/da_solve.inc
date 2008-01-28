subroutine da_solve ( grid , config_flags)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),               intent(inout) :: grid
   type (grid_config_rec_type), intent(inout) :: config_flags

   type (xbx_type)              :: xbx         ! For header & non-grid arrays.
   type (be_type)               :: be          ! Background error structure.
   real, allocatable            :: cvt(:)      ! Control variable structure.
   real, allocatable            :: xhat(:)     ! Control variable structure.
   type (y_type)                :: ob          ! Observation structure.
   type (iv_type)               :: iv          ! Obs. increment structure.
   type (y_type)                :: re          ! Residual (o-a) structure.
   type (y_type)                :: y           ! y = H(x_inc) structure.
   integer                      :: it          ! External loop counter.
   type (j_type)                :: j           ! Cost function.

   integer                      :: cv_size, i
   real                         :: j_grad_norm_target ! Target j norm.

#ifdef DM_PARALLEL
   integer                      :: wrf_done_unit
#endif

   if (trace_use) call da_trace_entry("da_solve")

#ifdef DM_PARALLEL
   call mpi_barrier(comm,ierr)
#endif

   !---------------------------------------------------------------------------
   ! If it is verification run set check_max_iv as .false.
   !---------------------------------------------------------------------------

   if (anal_type_verify) then
      check_max_iv = .false.
      ntmax=0
   end if

   if (cv_options_hum /= cv_options_hum_specific_humidity .and. &
       cv_options_hum /= cv_options_hum_relative_humidity) then
      write(unit=message(1),fmt='(A,I3)') &
         'Invalid cv_options_hum = ', cv_options_hum
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   if (vert_corr == vert_corr_2) then
      if (vertical_ip < vertical_ip_0 .or. vertical_ip > vertical_ip_delta_p) then
         write (unit=message(1),fmt='(A,I3)') &
           'Invalid vertical_ip = ', vertical_ip
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
   end if

   if (0.5 * real(rf_passes) /= real(rf_passes / 2)) then
      write(unit=stdout,fmt='(A,I4,A)') &
         'rf_passes = ', rf_passes, ' .Should be even.'
      rf_passes = int(real(rf_passes / 2))
      write(unit=stdout,fmt='(A,I4)') 'Resetting rf_passes = ', rf_passes
   end if

   if (anal_type_randomcv) then
      ntmax = 0
      write(unit=stdout,fmt='(a)') &
         ' Resetting ntmax = 0 for analysis_type = randomcv' 
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Initialise wrfvar parameters:
   !---------------------------------------------------------------------------

   call da_solve_init(grid &
#include "actual_new_args.inc"
)

   !---------------------------------------------------------------------------
   ! [3.0] Set up first guess field (grid%xb):
   !---------------------------------------------------------------------------

   call da_setup_firstguess(xbx, grid)

   !---------------------------------------------------------------------------
   ! [4.0] Set up observations (ob):
   !---------------------------------------------------------------------------

   call da_setup_obs_structures (grid, ob, iv)

   if (use_rad) then
      allocate (j % jo % rad(1:iv%num_inst))
      do i=1,iv%num_inst
         allocate (j % jo % rad(i) % jo_ichan(iv%instid(i)%nchan))
         allocate (j % jo % rad(i) % num_ichan(iv%instid(i)%nchan))
      end do
   end if

   !---------------------------------------------------------------------------
   ! [5.0] Set up background errors (be):
   !---------------------------------------------------------------------------

   call da_setup_background_errors (grid%xb, be)
   cv_size = be % cv % size

   !---------------------------------------------------------------------------
   ! [6.0] Set up ensemble perturbation input:
   !---------------------------------------------------------------------------

   grid%ep % ne = be % ne
   if (be % ne > 0) then
      call da_setup_flow_predictors (ide, jde, kde, be % ne, grid%ep )
   end if

   !---------------------------------------------------------------------------
   ! [7.0] Setup control variable (cv):
   !---------------------------------------------------------------------------

   allocate (cvt(1:cv_size))
   allocate (xhat(1:cv_size))
   call da_initialize_cv (cv_size, cvt)
   call da_initialize_cv (cv_size, xhat)
      
   call da_zero_vp_type (grid%vv)
   call da_zero_vp_type (grid%vp)

   !---------------------------------------------------------------------------
   ! [8] Outerloop
   !---------------------------------------------------------------------------

   j_grad_norm_target = 1.0
   do it = 1, max_ext_its

      ! [8.1] Calculate nonlinear model trajectory 

      ! [8.2] Calculate innovation vector (O-B):

      call da_get_innov_vector (it, ob, iv, grid , config_flags)

      if (test_transforms) then
         call da_check (grid, config_flags, cv_size, xbx, be, grid%ep, iv, &
            grid%vv, grid%vp, y)
         ! No point continuing, as data corrupted
         call wrf_shutdown
         stop
      end if

      ! [8.4] Minimize cost function:

      call da_allocate_y (iv, re)
      call da_allocate_y (iv, y)

      call da_minimise_cg( grid, config_flags, it, be % cv % size, & 
         xbx, be, iv, j_grad_norm_target, xhat, cvt, re, y, j)

      !------------------------------------------------------------------------

      ! reset cv to random noise
      if (anal_type_randomcv) then
         call da_set_randomcv (cv_size, xhat)
      end if

      ! [8.5] Update latest analysis solution:

      call da_transform_vtox (grid,cv_size,xbx,be,grid%ep,xhat,grid%vv,grid%vp)

      ! [8.6] Only when use_radarobs = .false. and calc_w_increment =.true.,
      !       the w_increment need to be diagnosed:

      if (calc_w_increment .and. .not. use_radarobs) then
         call da_uvprho_to_w_lin (grid)

#ifdef DM_PARALLEL
#include "HALO_RADAR_XA_W.inc"
#endif
      end if

      ! [8.7] Write out diagnostics

      call da_write_diagnostics (grid, ob, iv, re, y, j)

      ! Write "clean" QCed observations if requested:
      if (anal_type_qcobs) then
         if (it == 1) then
            call da_write_filtered_obs (grid, ob, iv, &
               coarse_ix, coarse_jy, start_x, start_y)
          end if     
      end if

      ! [8.3] Interpolate x_g to low resolution grid

      ! [8.8] Write Ascii radiance OMB and OMA file

      !------------------------------------------------------------------------
      ! [8.0] Output WRFVAR analysis and analysis increments:
      !------------------------------------------------------------------------

      call da_transfer_xatoanalysis (it, xbx, grid, config_flags)
   end do

   !---------------------------------------------------------------------------
   ! [9.0] Tidy up:
   !---------------------------------------------------------------------------

   deallocate (cvt)
   deallocate (xhat)

   call da_deallocate_observations (iv)
   call da_deallocate_y (re)
   call da_deallocate_y (y)
   call da_deallocate_y (ob)
   call da_deallocate_background_errors (be)

   if (xbx%pad_num > 0) then
      deallocate (xbx%pad_loc)
      deallocate (xbx%pad_pos)
   end if

   deallocate (xbx % fft_factors_x)
   deallocate (xbx % fft_factors_y)
   deallocate (xbx % fft_coeffs)
   deallocate (xbx % trig_functs_x)
   deallocate (xbx % trig_functs_y)

   if (global) then
      deallocate (xbx%coslat)
      deallocate (xbx%sinlat)
      deallocate (xbx%coslon)
      deallocate (xbx%sinlon)
      deallocate (xbx%int_wgts)
      deallocate (xbx%alp)
      deallocate (xbx%wsave)
      if (jts == jds) then
         deallocate (cos_xls)
         deallocate (sin_xls)
      end if
                                                                                
      if (jte == jde) then
         deallocate (cos_xle)
         deallocate (sin_xle)
      end if
   end if

   deallocate (xbx % latc_mean)

#ifdef DM_PARALLEL
   call mpi_barrier (comm,ierr)
#endif

   if (trace_use) call da_trace_exit ("da_solve")

contains

#include "da_solve_init.inc"

end subroutine da_solve

