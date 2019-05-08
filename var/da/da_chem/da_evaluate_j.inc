subroutine da_evaluate_j(grid, config_flags, cv_size, cv_size_jb, xbx, be, iv, xhat, cv, ob, &
                         cv_s_ant, cv_e_ant, cv_s_bb, cv_e_bb, eta, jout)

   !---------------------------------------------------------------------------
   ! Purpose: Used to evaluate cost function given:
   !          cv, previous accumulated cv increments
   !          xhat, current cv increment
   !          eta, scalar to multiply emission cv members
   !
   ! Called from da_dgn
   !---------------------------------------------------------------------------

   implicit none

   type (domain),       intent(inout) :: grid
   integer, intent(in)                :: cv_size    ! Total cv size.
   integer, intent(in)                :: cv_size_jb ! Jb cv size.
   type (xbx_type),intent(inout)      :: xbx    ! For header & non-grid arrays.
   type (be_type), intent(in)         :: be     ! background error structure.
   type (iv_type), intent(inout)      :: iv     ! innovation vector (o-b).
   real, intent(in)                   :: xhat(1:cv_size) ! control variables.
   real, intent(in)                   :: cv(1:cv_size)   ! control variables.
   type (y_type) , intent(inout)      :: ob          ! Observation structure.
   integer, intent(in)                :: cv_s_ant, cv_e_ant, cv_s_bb, cv_e_bb
   real, intent(in)                   :: eta
   type(grid_config_rec_type), intent(inout) :: config_flags
   real, intent(out)                  :: jout

   real             :: xhat_store(1:cv_size) ! initial control variables.
   real             :: jo_partial                   ! jo for this processor
   real             :: joc_partial                  ! jo for cross-validation for this processor
   real             :: cv_xhat_jb(cv_size_jb)
   real             :: xhat_temp(1:cv_size) ! initial control variables.
   type (j_type)    :: j
   type (y_type)    :: jo_grad_y 
   type (y_type)    :: re     ! residual vector (o-a).
   type (y_type)    :: y      ! y = H(x_inc) = 0.; just a placeholder here
   integer          :: i
#ifdef CLOUD_CV
   integer          :: mz(13)
#else
   integer          :: mz(9)
#endif

   if (trace_use) call da_trace_entry("da_evaluate_j")

   !-------------------------------------------------------------------------
   ! [0.0] initialization:
   !-------------------------------------------------------------------------

#ifdef CLOUD_CV
   mz = (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz,  be%v6%mz, be%v7%mz,  be%v8%mz, be%v9%mz, be%alpha%mz, be%ne, be%alpha_ant_chem(PARAM_FIRST_SCALAR)%mz, be%alpha_bb_chem(PARAM_FIRST_SCALAR)%mz /)
#else
   mz =  (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz, be%ne, be%alpha_ant_chem(PARAM_FIRST_SCALAR)%mz, be%alpha_bb_chem(PARAM_FIRST_SCALAR)%mz /)
#endif

   call da_allocate_y (iv, re)
   call da_allocate_y (iv, y)
   call da_allocate_y (iv, jo_grad_y)

   call da_allocate_y_chem (iv, re)
   call da_allocate_y_chem (iv, y)
   call da_allocate_y_chem (iv, jo_grad_y)

   xhat_temp = xhat

   if ( cv_e_ant - cv_s_ant .gt. 0 ) xhat_temp(cv_s_ant:cv_e_ant) = eta * xhat(cv_s_ant:cv_e_ant)
   if ( cv_e_bb - cv_s_bb .gt. 0 ) xhat_temp(cv_s_bb:cv_e_bb) = eta * xhat(cv_s_bb:cv_e_bb)

   !-------------------------------------------------------------------------
   ! [1.0] calculate jo:
   !-------------------------------------------------------------------------

   ! Transform xhat (dv) to dx increments
   call da_transform_vtox (grid,be%cv%size_jb,xbx,be,grid%ep,xhat_temp(1:be%cv%size_jb),grid%vv,grid%vp, grid%vch)

   ! Update analysis in WRFDA grid for scaling factors only
   call da_transfer_xatowrf_temp(grid)

!   if ( .not. use_nonchemobs ) then
      ! Reset MET variables in model_grid to their initial values
      call da_init_model_input
!   else
!      ! Set MET variables in model_grid to their posterior values
!      ! This functionality is not available yet, since grid%XX is not updated for MET.
!      call kj_swap (grid%u_2, model_grid%u_2, &
!                    grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
!      call kj_swap (grid%v_2, model_grid%v_2, &
!                    grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
!      call kj_swap (grid%w_2, model_grid%w_2, &
!                    grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
!      call kj_swap (grid%t_2, model_grid%t_2, &
!                    grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
!      call kj_swap (grid%ph_2, model_grid%ph_2, &
!                    grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
!      call kj_swap (grid%p, model_grid%p, &
!                    grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
!
!      model_grid%mu_2 = grid%mu_2
!      model_grid%t2 = grid%t2
!      model_grid%th2 = grid%th2
!      model_grid%q2 = grid%q2
!      model_grid%u10 = grid%u10
!      model_grid%v10 = grid%v10
!      model_grid%tsk = grid%tsk
!      model_grid%psfc = grid%psfc
!
!      do i = PARAM_FIRST_SCALAR, num_moist
!         call kj_swap (grid%moist(:,:,:,i), model_grid%moist(:,:,:,i), &
!                       grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
!      enddo
!   end if

   ! Transferring chem from grid to model_grid gives same result as da_init_model_input, unless
   ! chem is added as a control variable.
   do i = PARAM_FIRST_SCALAR, num_chem
!   do i = 1, num_chem-1
      call kj_swap (grid%chem(:,:,:,i), model_grid%chem(:,:,:,i), &
                    grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   enddo

   ! Transfer posterior scaling factors to model_grid
   do i = PARAM_FIRST_SCALAR, num_scaleant
      call kj_swap (grid%scaleant(:,:,:,i), model_grid%scaleant(:,:,:,i), &
                    grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, 1, num_ant_steps)
   enddo
   do i = PARAM_FIRST_SCALAR, num_scalebb
      call kj_swap (grid%scalebb(:,:,:,i), model_grid%scalebb(:,:,:,i), &
                    grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, 1, num_bb_steps)
   enddo

   ! Relinearize
   if ( checkpoint_interval .gt. 0 ) write_checkpoints = 2
!!! add !!!   call da_nl_model(-1)
!!! add !!!   if ( checkpoint_interval .gt. 0 ) write_checkpoints = 0

   ! Calculate new cost function
   call da_retrieve_chem_hx( model_grid, iv )

   call da_get_innov_vector (2, num_qcstat_conv, ob, iv, grid , config_flags)

   call da_calculate_residual(iv, y, re)

   call da_jo_and_grady(iv, re, jo_partial, j % jo, jo_grad_y, joc_partial )

   if (test_dm_exact) then
      ! jo_partial has been already summed at lower level
      j % jo % total = jo_partial
   else
      j % jo % total = wrf_dm_sum_real(jo_partial)
   end if


   !-------------------------------------------------------------------------
   ! [2.0] calculate jb:
   !-------------------------------------------------------------------------

   j % jb = 0.0
   if (cv_size_jb > 0) then
      cv_xhat_jb(1:cv_size_jb) = cv(1:cv_size_jb) + xhat_temp(1:cv_size_jb)
      j % jb = jb_factor * 0.5 * da_dot_cv(cv_size_jb,  cv_xhat_jb, cv_xhat_jb, grid, be%cv_mz, be%ncv_mz, be%cv_mz_chem )
   end if

   jout = j % jb + j % jo % total

   call da_deallocate_y (re)
   call da_deallocate_y (y)
   call da_deallocate_y (jo_grad_y)

   if (trace_use) call da_trace_exit("da_evaluate_j")

end subroutine da_evaluate_j

