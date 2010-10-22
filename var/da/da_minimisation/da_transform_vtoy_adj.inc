subroutine da_transform_vtoy_adj(cv_size, be, ep, cv, iv, vp, vv, vp6, vv6, xbx, y, &
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
   type(vp_type),              intent(inout) :: vp6    ! Grdipt/level CV for 6h.
   type(vp_type),              intent(inout) :: vv     ! Grdipt/EOF CV.
   type(vp_type),              intent(inout) :: vv6    ! Grdipt/EOF CV for 6h.
   type(xbx_type),             intent(inout) :: xbx    ! For header & non-grid arrays.
   type(y_type),               intent(inout) :: y      ! y = H(x_inc).
   type(domain),               intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags
   logical,                    intent(in)    :: jcdf_flag       ! additional flag to switch off JcDF, used to switch off JcDF in calculation of J.

   type(x_type) :: shuffle
   integer :: nobwin,ndynopt, jl_start, jl_end, time_step_seconds
   character*256 :: timestr, timestr1

#ifdef DM_PARALLEL
   integer :: wrf_done_unit
#endif

   character(len=4) :: filnam

   call da_trace_entry("da_transform_vtoy_adj")

   cv = 0.0

   if (var4d) then

#ifdef VAR4D
      if (jcdfi_use .and. .not. jcdf_flag ) then
         model_grid%jcdfi_u = 0.0
         model_grid%jcdfi_v = 0.0
         model_grid%jcdfi_t = 0.0
         model_grid%jcdfi_mu = 0.0
      endif

      if (jcdfi_use .and. jcdf_flag ) then
         ! Multipled by -1.0 because the calulated gradient of Jo will be multiplied by -1.0 
         model_grid%jcdfi_u = -1.0 * 1.0/grid%jcdfi_error_wind**2 * model_grid%jcdfi_u
         model_grid%jcdfi_v = -1.0 * 1.0/grid%jcdfi_error_wind**2 * model_grid%jcdfi_v
         model_grid%jcdfi_t = -1.0 * 1.0/grid%jcdfi_error_t**2 * model_grid%jcdfi_t
         model_grid%jcdfi_mu = -1.0 * 1.0/grid%jcdfi_error_mu**2 * model_grid%jcdfi_mu
      endif
   
      if (num_fgat_time > 1) then
         call domain_clock_get (grid, stop_timestr=timestr1)
         call domain_clock_set( grid, current_timestr=timestr1 )
         call domain_clock_set (grid, time_step_seconds=-1*var4d_bin)
         call domain_clockprint(150, grid, 'get CurrTime from clock,')
      endif

      do nobwin=num_fgat_time, 1, -1

         call domain_clock_get( grid, current_timestr=timestr )
         call da_read_basicstates (xbx, grid, config_flags, timestr)

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

         call da_transfer_wrftltoxa_adj(grid, config_flags, filnam, timestr)

         if (nobwin >  1) call domain_clockadvance (grid)
         call domain_clockprint(150, grid, 'DEBUG Adjoint Forcing:  get CurrTime from clock,')

      end do

      if (num_fgat_time > 1) then
         call nl_get_time_step ( grid%id, time_step_seconds)
         call domain_clock_set (grid, time_step_seconds=time_step_seconds)
         call domain_clockprint(150, grid, 'get CurrTime from clock,')
      endif

      ! for memory io, we need to up-side-down the adjoint forcing linked list generated in previous step.
      call upsidedown_ad_forcing

      call da_ad_model

      call da_transfer_xatowrftl_adj(grid, config_flags, 'gr01')
      call da_transform_vtox_adj(grid, be%cv%size_jb, xbx, be, ep, vp, vv, cv(1:be%cv%size_jb))

      call da_zero_x(grid%x6a)
      if (var4d_lbc) then
         call da_get_2rd_firstguess ( grid )
         call da_setup_firstguess( xbx, grid, config_flags)

         call da_transfer_xatowrftl_adj_lbc(grid, config_flags, 'gr01')
         shuffle = grid%xa
         jl_start    = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + 1
         jl_end      = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + be%cv%size_jl
         grid%xa = grid%x6a
         call da_transform_vtox_adj(grid, be%cv%size_jl, xbx, be, ep, vp6, vv6, cv(jl_start:jl_end))
         grid%xa = shuffle

         call da_read_basicstates ( xbx, grid, config_flags, timestr )
      end if

#endif
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
      call da_transform_vtox_adj(grid, cv_size, xbx, be, ep, vp, vv, cv)
   end if ! var4d

   !--------------------------------------------------------------
   ! ADJ of Variational Bias Correction
   !--------------------------------------------------------------
#if defined(RTTOV) || defined(CRTM)
   if (use_varbc) call da_varbc_adj(cv_size, cv, iv, y)
#endif
   if (trace_use) call da_trace_exit("da_transform_vtoy_adj")

end subroutine da_transform_vtoy_adj


