subroutine da_transform_vtoy_adj(cv_size, be, ep, cv, iv, vp, vv, xbx, y, &
   grid, config_flags, jcdf_flag)

   !-------------------------------------------------------------------------
   ! Purpose:  Does Adjoint of control variable (V) transform to Obs-space(Y)
   !-------------------------------------------------------------------------

   implicit none

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

   cv = 0.0

   if (var4d) then
      ndynopt      = grid%dyn_opt
      grid%dyn_opt = DYN_EM_TL
      call nl_set_dyn_opt (1 , DYN_EM_TL)

      if (jcdfi_use .AND. jcdf_flag .AND. jcdfi_io ) then
         call da_med_initialdata_input(grid , config_flags, 'tldf')
         grid%g_u_2 = - grid%g_u_2
         grid%g_v_2 = - grid%g_v_2
         grid%g_w_2 = - grid%g_w_2
         grid%g_t_2 = - grid%g_t_2
         grid%g_ph_2 = - grid%g_ph_2
         grid%g_mu_2 = - grid%g_mu_2
         grid%g_moist = - grid%g_moist
      else
         grid%g_u_2 = 0.0
         grid%g_v_2 = 0.0
         grid%g_w_2 = 0.0
         grid%g_t_2 = 0.0
         grid%g_ph_2 = 0.0
         grid%g_mu_2 = 0.0
         grid%g_moist = 0.0
      end if

      if (jcdfi_io) call med_hist_out(grid , 3 , config_flags)

      grid%dyn_opt = ndynopt
      call nl_set_dyn_opt (1 , DYN_EM)

      do nobwin=num_fgat_time,1,-1
         write(unit=filnam, fmt='(a, i2.2)') 'fg', nobwin
         call da_med_initialdata_input( grid , config_flags, &
            filnam)
         call da_setup_firstguess( xbx_tmp, grid, config_flags)

         iv%time = nobwin
         iv%info(:)%n1 = iv%info(:)%plocal(iv%time-1) + 1
         iv%info(:)%n2 = iv%info(:)%plocal(iv%time)
         if ( use_rad ) then
            iv%instid(:)%info%n1 = iv%instid(:)%info%plocal(iv%time-1) + 1
            iv%instid(:)%info%n2 = iv%instid(:)%info%plocal(iv%time)
         end if
         call da_zero_x(grid%xa)
         call da_transform_xtoy_adj(cv_size, cv, grid, iv, y, grid%xa)
         call da_transform_xtoxa_adj(grid)
         write(unit=filnam,fmt='(a2,i2.2)') 'af',nobwin

         call da_transfer_wrftltoxa_adj(grid, config_flags, filnam)
      end do

      call da_trace("da_transform_vtoy_adj","Starting da_run_wrfplus_ad.ksh")

#ifdef DM_PARALLEL
      if (var4d_coupling == var4d_coupling_disk_simul) then

         if (rootproc) then
            call da_system("da_run_wrfplus_ad.ksh pre")
            call da_system("rm -rf wrf_done")
            call da_system("touch wrf_go_ahead")
            call da_get_unit(wrf_done_unit)
            do while (.true.)
               open(wrf_done_unit,file="wrf_done",status="old",err=303)
               close(wrf_done_unit)
               exit
303            continue
               call da_system("sleep 1")
            end do
            call da_free_unit(wrf_done_unit)
            call da_system("da_run_wrfplus_ad.ksh post")
         end if
         ! Wait until PE 0 agrees that AD finished
         call wrf_get_dm_communicator ( comm )
         call mpi_barrier(comm, ierr)
      end if
#else
      call da_system("da_run_wrfplus_ad.ksh")
#endif
      call da_trace("da_transform_vtoy_adj","Finished da_run_wrfplus_ad.ksh")

      call da_transfer_xatowrftl_adj(grid, config_flags, 'gr01')

      if (num_fgat_time > 1) then ! Recover the firstguess fields touched by reading gr01
         call da_med_initialdata_input( grid , config_flags, 'fg')
      end if
   else  ! not var4d
      iv%info(:)%n1 = 1
      iv%info(:)%n2 = iv%info(:)%nlocal
      if ( use_rad ) then
         iv%instid(:)%info%n1 = 1
         iv%instid(:)%info%n2 = iv%instid(:)%num_rad
      end if
      call da_zero_x(grid%xa)
      call da_transform_xtoy_adj(cv_size, cv, grid, iv, y,grid%xa)
      call da_transform_xtoxa_adj(grid)
   end if ! var4d
   call da_transform_vtox_adj(grid, cv_size, xbx, be, ep, vp, vv, cv)

   !--------------------------------------------------------------
   ! ADJ of Variational Bias Correction
   !--------------------------------------------------------------
#if defined(RTTOV) || defined(CRTM)
   if (use_varbc) call da_varbc_adj(cv_size, cv, iv, y)
#endif
   if (trace_use) call da_trace_exit("da_transform_vtoy_adj")

end subroutine da_transform_vtoy_adj


