subroutine da_transform_vtoy(cv_size, be, ep, cv, iv, vp, vv, vp6, vv6, xbx, &
                              y, grid, config_flags)

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
   type(vp_type),              intent(inout) :: vp6     ! Grdipt/level CV for 6h.
   type(vp_type),              intent(inout) :: vv     ! Grdipt/EOF CV.
   type(vp_type),              intent(inout) :: vv6     ! Grdipt/EOF CV for 6h.
   type(xbx_type),             intent(inout) :: xbx    ! For header & non-grid arrays.
   type(y_type),               intent(inout) :: y      ! y = H(x_inc).
   type(domain),               intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   type(x_type) :: shuffle
   integer :: nobwin, jl_start, jl_end, time_step_seconds

   character(len=4) :: filnam
   character(len=256) :: timestr, timestr1

   if (trace_use) call da_trace_entry("da_transform_vtoy")

   if (var4d) then
#ifdef VAR4D
      call da_transform_vtox(grid, be%cv%size_jb, xbx, be, ep, cv(1:be%cv%size_jb), vv, vp)
      call domain_clock_get( grid, start_timestr=timestr )
      call da_transfer_xatowrftl(grid, config_flags, 'tl01', timestr)

      if ( var4d_lbc ) then 
         call da_get_2nd_firstguess ( grid )
         call da_setup_firstguess( xbx, grid, config_flags)

         shuffle = grid%xa
         jl_start    = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + 1
         jl_end      = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + be%cv%size_jl
         grid%xa  = grid%x6a
         call da_transform_vtox(grid, be%cv%size_jl, xbx, be, ep, cv(jl_start:jl_end), vv6, vp6)
         grid%xa  = shuffle

         call da_transfer_xatowrftl_lbc(grid, config_flags, 'tl01')

         call domain_clock_get( grid, start_timestr=timestr )
         call da_read_basicstates ( xbx, grid, config_flags, timestr )
      else
         call da_transfer_xatowrftl_lbc(grid, config_flags, 'tl01')
      endif

      call da_tl_model

      if ( num_fgat_time > 1 ) then
         call domain_clock_get (grid, stop_timestr=timestr1)
         call domain_clock_set( grid, current_timestr=timestr1 )
         call domain_clock_set (grid, time_step_seconds=-1*var4d_bin)
         call domain_clockprint(150, grid, 'get CurrTime from clock,')
      endif

      do nobwin= num_fgat_time, 1 , -1

         call domain_clock_get( grid, current_timestr=timestr )
         call da_read_basicstates ( xbx, grid, config_flags, timestr )

         iv%time = nobwin
         iv%info(:)%n1 = iv%info(:)%plocal(iv%time-1) + 1
         iv%info(:)%n2 = iv%info(:)%plocal(iv%time)
         if ( use_rad ) then   
            iv%instid(:)%info%n1 = iv%instid(:)%info%plocal(iv%time-1) + 1
            iv%instid(:)%info%n2 = iv%instid(:)%info%plocal(iv%time)
         end if
         write(filnam,'(a2,i2.2)') 'tl',nobwin
         call da_zero_x(grid%xa)
         call da_transfer_wrftltoxa(grid, config_flags, filnam, timestr)
         call da_transform_xtoxa(grid)
         call da_transform_xtoy(cv_size, cv, grid, iv, y)

         if ( nobwin > 1 ) call domain_clockadvance (grid) ! We don't need the advance at the last step
         call domain_clockprint(150, grid, 'DEBUG Adjoint Forcing:  get CurrTime from clock,')

      end do

      if ( num_fgat_time > 1 ) then
         call nl_get_time_step ( grid%id, time_step_seconds)
         call domain_clock_set (grid, time_step_seconds=time_step_seconds)
         call domain_clockprint(150, grid, 'get CurrTime from clock,')
      endif

#endif
   else  ! not var4d
      call da_transform_vtox(grid, cv_size, xbx, be, ep, cv, vv, vp)
      iv%info(:)%n1 = 1
      iv%info(:)%n2 = iv%info(:)%nlocal
      if ( use_rad ) then
         iv%instid(:)%info%n1 = 1
         iv%instid(:)%info%n2 = iv%instid(:)%num_rad
      end if
      call da_transform_xtoxa(grid)
      call da_transform_xtoy(cv_size, cv, grid, iv, y)
   end if ! var4d

   !--------------------------------------------------------------
   ! TL of Variational Bias Correction
   !--------------------------------------------------------------
#if defined(RTTOV) || defined(CRTM)
   if (use_varbc) call da_varbc_tl(cv_size, cv, iv, y)
#endif
   if (trace_use) call da_trace_exit("da_transform_vtoy")

end subroutine da_transform_vtoy


