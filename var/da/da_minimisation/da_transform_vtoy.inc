subroutine da_transform_vtoy(cv_size, be, ep, cv, iv, vp, vv, xbx, &
                              y, grid, config_flags, vp6, vv6)

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
   type(xbx_type),             intent(inout) :: xbx    ! For header & non-grid arrays.
   type(y_type),               intent(inout) :: y      ! y = H(x_inc).
   type(domain),               intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   ! vp6 and vv6 are defined only when the code is configured and compiled for 4dvar
   type(vp_type), optional,    intent(inout) :: vp6     ! Grdipt/level CV for 6h.
   type(vp_type), optional,    intent(inout) :: vv6     ! Grdipt/EOF CV for 6h.

   type(x_type) :: shuffle
   integer :: nobwin, jl_start, jl_end, time_step_seconds
   integer :: num_subtwindow

   character(len=4) :: filnam
   character(len=256) :: timestr, timestr1
   real, dimension(:,:,:), allocatable :: hr_rainc, hr_rainnc
   real, dimension(:,:,:), allocatable :: g_rainc, g_rainnc

   if (trace_use) call da_trace_entry("da_transform_vtoy")

   if (var4d) then
#ifdef VAR4D
      call da_transform_vtox(grid, be%cv%size_jb, xbx, be, ep, cv(1:be%cv%size_jb), vv, vp)
      if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
         call da_transform_vpatox(grid, be, ep, vp)
         call da_add_xa(grid%xa, grid%xa_ens) !grid%xa = grid%xa + xa_ens
      end if
      call domain_clock_get( grid, start_timestr=timestr )
      call da_transfer_xatowrftl(grid, config_flags, 'tl01', timestr)

      if ( var4d_lbc ) then 
         call domain_clock_get (grid, stop_timestr=timestr1)
         call domain_clock_set( grid, current_timestr=timestr1 )
         grid%u_2 = u6_2 ; grid%v_2 = v6_2; grid%t_2 = t6_2;
         grid%w_2 = w6_2 ; grid%mu_2 = mu6_2 ; grid%ph_2 =ph6_2
         grid%moist = moist6; grid%p = p6; grid%psfc = psfc6
         call da_transfer_wrftoxb(xbx, grid, config_flags)

         shuffle = grid%xa
         jl_start    = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + 1
         jl_end      = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + be%cv%size_jl
         grid%xa  = grid%x6a
         call da_transform_vtox(grid, be%cv%size_jl, xbx, be, ep, cv(jl_start:jl_end), vv6, vp6)
         grid%xa  = shuffle

         call da_transfer_xatowrftl_lbc(grid, config_flags, 'tl01')

         call domain_clock_get( grid, start_timestr=timestr1 )
         call domain_clock_set( grid, current_timestr=timestr1 )
         call da_read_basicstates ( xbx, grid, config_flags, timestr1 )
      else
         call da_transfer_xatowrftl_lbc(grid, config_flags, 'tl01')
      endif

      call da_tl_model

      if (num_fgat_time > 1 .and. use_rainobs) then
         ! Buffer to save var4d_bin_rain rainfall
         allocate (hr_rainc (ims:ime,jms:jme,1:num_fgat_time))
         allocate (hr_rainnc(ims:ime,jms:jme,1:num_fgat_time))
         hr_rainc =0.0
         hr_rainnc=0.0
         ! Buffer to save accumulated rainfall
         allocate (g_rainc (ims:ime,jms:jme,1:num_fgat_time))
         allocate (g_rainnc(ims:ime,jms:jme,1:num_fgat_time))
         g_rainc =0.0
         g_rainnc=0.0
      endif

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
         if ( use_rainobs ) then
            g_rainc (:,:,nobwin)=grid%g_rainc(:,:)
            g_rainnc(:,:,nobwin)=grid%g_rainnc(:,:)  
         endif
         call da_transform_xtoxa(grid)
         call da_transform_xtoy(cv_size, cv, grid, iv, y)

         if ( nobwin > 1 ) call domain_clockadvance (grid) ! We don't need the advance at the last step
         call domain_clockprint(150, grid, 'DEBUG :  get CurrTime from clock,')

      end do
      
      ! Caculate var4d_bin_rain rainfall
      if (num_fgat_time > 1 .and. use_rainobs) then
         do nobwin=num_fgat_time, 1, -1
            if ( .not. fgat_rain_flags(nobwin) ) cycle
            if (nobwin .lt. num_fgat_time) then
               hr_rainc (:,:,nobwin+INT(var4d_bin_rain/var4d_bin))= &
                         hr_rainc (:,:,nobwin+INT(var4d_bin_rain/var4d_bin))-g_rainc(:,:,nobwin)
               hr_rainnc(:,:,nobwin+INT(var4d_bin_rain/var4d_bin))= &
                         hr_rainnc(:,:,nobwin+INT(var4d_bin_rain/var4d_bin))-g_rainnc(:,:,nobwin)  
            else
               hr_rainc (:,:,nobwin)=g_rainc(:,:,nobwin)
               hr_rainnc(:,:,nobwin)=g_rainnc(:,:,nobwin)  
            endif
         end do
      endif

      if (iv%info(rain)%nlocal > 0 .and. num_fgat_time > 1) then
         do nobwin=num_fgat_time, 1, -1
            iv%time = nobwin
            iv%info(rain)%n1 = iv%info(rain)%plocal(iv%time-1) + 1
            iv%info(rain)%n2 = iv%info(rain)%plocal(iv%time)
            call da_transform_xtoy_rain( grid, iv, y, hr_rainc(:,:,nobwin), hr_rainnc(:,:,nobwin))
         end do
      endif
      
      if ( num_fgat_time > 1 ) then
         call nl_get_time_step ( grid%id, time_step_seconds)
         call domain_clock_set (grid, time_step_seconds=time_step_seconds)
         call domain_clockprint(150, grid, 'get CurrTime from clock,')
      endif

      if (num_fgat_time > 1 .and. use_rainobs) then
         deallocate (hr_rainc )
         deallocate (hr_rainnc)
         deallocate (g_rainc )
         deallocate (g_rainnc)
      endif

#endif
   else  ! not var4d

      ! as of V3.9, vp%alpha is not included in the vptox (called by vtox) calculation
      ! here grid%xa contains only the static increment
      call da_transform_vtox(grid, cv_size, xbx, be, ep, cv, vv, vp)

      if ( use_4denvar ) then
         num_subtwindow = num_fgat_time
      else
         num_subtwindow = 1
      end if

      if ( num_subtwindow > 1 ) then
         !grid%xa_static = grid%xa !save the static increment
         call da_copy_xa(grid%xa_static, grid%xa)
      end if

      do nobwin = num_subtwindow, 1 , -1
         if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
            call da_transform_vpatox(grid, be, ep, vp, nobwin)
            if ( num_subtwindow > 1 ) then
               !reset grid%xa = grid%xa_static
               call da_copy_xa(grid%xa, grid%xa_static)
            end if
            call da_add_xa(grid%xa, grid%xa_ens) !grid%xa = grid%xa + xa_ens
         end if
         call da_transform_xtoxa(grid)
         iv%time = nobwin
         if ( use_4denvar ) then
            iv%info(:)%n1 = iv%info(:)%plocal(iv%time-1) + 1
            iv%info(:)%n2 = iv%info(:)%plocal(iv%time)
            if ( use_rad ) then
               iv%instid(:)%info%n1 = iv%instid(:)%info%plocal(iv%time-1) + 1
               iv%instid(:)%info%n2 = iv%instid(:)%info%plocal(iv%time)
            end if
         else
            iv%info(:)%n1 = 1
            iv%info(:)%n2 = iv%info(:)%nlocal
            if ( use_rad ) then
               iv%instid(:)%info%n1 = 1
               iv%instid(:)%info%n2 = iv%instid(:)%num_rad
            end if
         end if !4denvar
         call da_transform_xtoy(cv_size, cv, grid, iv, y)
      end do

   end if ! var4d

   !--------------------------------------------------------------
   ! TL of Variational Bias Correction
   !--------------------------------------------------------------
#if defined(RTTOV) || defined(CRTM)
   if (use_varbc) call da_varbc_tl(cv_size, cv, iv, y)
#endif

   if (use_varbc_tamdar) call da_varbc_tamdar_tl(cv_size, cv, iv, y)

   if (trace_use) call da_trace_exit("da_transform_vtoy")

end subroutine da_transform_vtoy


