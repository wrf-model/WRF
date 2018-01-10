subroutine da_transform_vtoy_adj(cv_size, be, ep, cv, iv, vp, vv, xbx, y, &
   grid, config_flags, jcdf_flag, vp6, vv6)

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
   type(xbx_type),             intent(inout) :: xbx    ! For header & non-grid arrays.
   type(y_type),               intent(inout) :: y      ! y = H(x_inc).
   type(domain),               intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags
   logical,                    intent(in)    :: jcdf_flag       ! additional flag to switch off JcDF, used to switch off JcDF in calculation of J.

   ! vp6 and vv6 are defined only when the code is configured and compiled for 4dvar
   type(vp_type), optional,    intent(inout) :: vp6    ! Grdipt/level CV for 6h.
   type(vp_type), optional,    intent(inout) :: vv6    ! Grdipt/EOF CV for 6h.

   type(x_type)  :: shuffle
   real          :: cv_local(1:cv_size) ! control variables
   integer       :: nobwin, jl_start, jl_end, time_step_seconds
   integer       :: num_subtwindow
   character*256 :: timestr, timestr1
   integer       :: i, j, k
   real          :: subarea, whole_area
   real          :: f_flag
   real, dimension(:,:,:), allocatable :: a_hr_rainc, a_hr_rainnc
   real, dimension(:,:,:), allocatable :: a_rainc, a_rainnc

   character(len=4) :: filnam

   call da_trace_entry("da_transform_vtoy_adj")

   cv = 0.0

   if (var4d) then

#ifdef VAR4D
      if (jcdfi_use .and. .not. jcdf_flag ) then
         model_grid%jcdfi_u = 0.0
         model_grid%jcdfi_v = 0.0
         model_grid%jcdfi_t = 0.0
         model_grid%jcdfi_p = 0.0
      endif

      if (jcdfi_use .and. jcdf_flag ) then

         subarea = SUM ( grid%xb%grid_box_area(its:ite,jts:jte) )
         whole_area = wrf_dm_sum_real(subarea)

         ! Multipled by -1.0 because the dnw is negative, multiplied by -1.0 again to revert the direction.

         f_flag = 1.0 ! -1.0*-1.0

         if ( test_gradient ) f_flag = -1.0 ! compensate for negative dnw

         do j = jms, jme
            do k = kms, kme
               do i = ims, ime
                  model_grid%jcdfi_u(i,k,j) = f_flag*config_flags%jcdfi_penalty * model_grid%jcdfi_u(i,k,j) * &
                        grid%xb%grid_box_area(i,j)/whole_area*grid%xb%dnw(k)
                  model_grid%jcdfi_v(i,k,j) = f_flag*config_flags%jcdfi_penalty * model_grid%jcdfi_v(i,k,j) * &
                        grid%xb%grid_box_area(i,j)/whole_area*grid%xb%dnw(k)
                  model_grid%jcdfi_t(i,k,j) = f_flag*config_flags%jcdfi_penalty * model_grid%jcdfi_t(i,k,j) * &
                        (9.81/3.0)**2*grid%xb%grid_box_area(i,j)/whole_area*grid%xb%dnw(k)
                  model_grid%jcdfi_p(i,k,j) = f_flag*config_flags%jcdfi_penalty * model_grid%jcdfi_p(i,k,j) * &
                        (1.0/300.)**2*grid%xb%grid_box_area(i,j)/whole_area*grid%xb%dnw(k)
               enddo
            enddo
         enddo

      endif
   
      if (num_fgat_time > 1) then
         call domain_clock_get (grid, stop_timestr=timestr1)
         call domain_clock_set( grid, current_timestr=timestr1 )
         call domain_clock_set (grid, time_step_seconds=-1*var4d_bin)
         call domain_clockprint(150, grid, 'get CurrTime from clock,')
      endif

      if (num_fgat_time > 1 .and. use_rainobs) then
         ! Buffer to save adjoint var4d_bin_rain rainfall
         allocate (a_hr_rainc (ims:ime,jms:jme,1:num_fgat_time))
         allocate (a_hr_rainnc(ims:ime,jms:jme,1:num_fgat_time))
         a_hr_rainc =0.0
         a_hr_rainnc=0.0
         ! Buffer to save adjint accumulated rainfall
         allocate (a_rainc (ims:ime,jms:jme,1:num_fgat_time))
         allocate (a_rainnc(ims:ime,jms:jme,1:num_fgat_time))
         a_rainc =0.0
         a_rainnc=0.0
      endif
 
      if (iv%info(rain)%nlocal > 0 .and. num_fgat_time > 1) then
         do nobwin=1, num_fgat_time
            iv%time = nobwin
            iv%info(rain)%n1 = iv%info(rain)%plocal(iv%time-1) + 1
            iv%info(rain)%n2 = iv%info(rain)%plocal(iv%time)
            call da_transform_xtoy_rain_adj( grid, iv, y, a_hr_rainc(:,:,nobwin), a_hr_rainnc(:,:,nobwin))
         end do
      endif

      ! Adjoint of calculate hourly rainfall
      if (num_fgat_time > 1 .and. use_rainobs) then
         do nobwin=1, num_fgat_time
            if ( .not. fgat_rain_flags(nobwin) ) cycle
            if (nobwin .lt. num_fgat_time) then
               a_rainc(:,:,nobwin) = a_rainc(:,:,nobwin) - a_hr_rainc (:,:,nobwin+INT(var4d_bin_rain/var4d_bin))
               a_rainnc(:,:,nobwin) = a_rainnc(:,:,nobwin) - a_hr_rainnc(:,:,nobwin+INT(var4d_bin_rain/var4d_bin))
            else
               a_rainc(:,:,nobwin) = a_rainc(:,:,nobwin) + a_hr_rainc (:,:,nobwin)
               a_rainnc(:,:,nobwin) = a_rainnc(:,:,nobwin) + a_hr_rainnc(:,:,nobwin)
               a_hr_rainc (:,:,nobwin) = 0.0
               a_hr_rainnc(:,:,nobwin) = 0.0
            endif
         end do
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
         grid%g_rainnc = 0.0
         grid%g_rainc = 0.0
         call da_transform_xtoy_adj(cv_size, cv, grid, iv, y, grid%xa)
         call da_transform_xtoxa_adj(grid)
         write(unit=filnam,fmt='(a2,i2.2)') 'af',nobwin

         if ( use_rainobs ) then
            grid%g_rainc(:,:) = grid%g_rainc(:,:) + a_rainc (:,:,nobwin)
            grid%g_rainnc(:,:) = grid%g_rainnc(:,:) + a_rainnc(:,:,nobwin) 
            a_rainc (:,:,nobwin) = 0.0
            a_rainnc(:,:,nobwin) = 0.0
         endif

         call da_transfer_wrftltoxa_adj(grid, config_flags, filnam, timestr)

         if (nobwin >  1) call domain_clockadvance (grid)
         call domain_clockprint(150, grid, 'DEBUG Adjoint Forcing:  get CurrTime from clock,')

      end do

      if (num_fgat_time > 1) then
         call nl_get_time_step ( grid%id, time_step_seconds)
         call domain_clock_set (grid, time_step_seconds=time_step_seconds)
         call domain_clockprint(150, grid, 'get CurrTime from clock,')
      endif

      if ( trajectory_io ) then
         ! for memory io, we need to up-side-down the adjoint forcing linked list generated in previous step.
          call upsidedown_ad_forcing
      endif

      call da_ad_model

      call da_zero_x(grid%x6a)
      if (var4d_lbc) then
         call domain_clock_get (grid, stop_timestr=timestr)
         call domain_clock_set( grid, current_timestr=timestr )
         grid%u_2 = u6_2 ; grid%v_2 = v6_2; grid%t_2 = t6_2;
         grid%w_2 = w6_2 ; grid%mu_2 = mu6_2 ; grid%ph_2 =ph6_2
         grid%moist = moist6; grid%p = p6; grid%psfc = psfc6
         call da_transfer_wrftoxb(xbx, grid, config_flags)

         call da_transfer_xatowrftl_adj_lbc(grid, config_flags, 'gr01')
         shuffle = grid%xa
         jl_start    = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + 1
         jl_end      = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + be%cv%size_jl
         grid%xa = grid%x6a
         !as of V3.9, da_zero_vp_type(vp) is commented out in the da_transform_vtox_adj,
         call da_zero_vp_type (vp6) !as of V3.9, zero out vp here
         call da_transform_vtox_adj(grid, be%cv%size_jl, xbx, be, ep, vp6, vv6, cv(jl_start:jl_end))
         grid%xa = shuffle

         call domain_clock_get( grid, start_timestr=timestr1 )
         call domain_clock_set( grid, current_timestr=timestr1 )
         call da_read_basicstates ( xbx, grid, config_flags, timestr1 )
      end if

      call da_zero_x(grid%xa)
      call da_transfer_xatowrftl_adj(grid, config_flags, 'gr01')
      !as of V3.9, da_zero_vp_type(vp) is commented out in the da_transform_vtox_adj,
      call da_zero_vp_type (vp) !as of V3.9, zero out vp here
      call da_transform_vpatox_adj(grid, be, ep, vp)
      call da_transform_vtox_adj(grid, be%cv%size_jb, xbx, be, ep, vp, vv, cv(1:be%cv%size_jb))
  
      if (num_fgat_time > 1 .and. use_rainobs) then
         deallocate (a_hr_rainc )
         deallocate (a_hr_rainnc)
         deallocate (a_rainc )
         deallocate (a_rainnc)
      endif

#endif
   else  ! not var4d

      if ( use_4denvar ) then
         num_subtwindow = num_fgat_time
      else
         num_subtwindow = 1
      end if

      do nobwin= num_subtwindow, 1 , -1
         cv_local = 0.0
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
         call da_zero_x(grid%xa)
         call da_transform_xtoy_adj(cv_size, cv_local, grid, iv, y, grid%xa)
         call da_transform_xtoxa_adj(grid)
         call da_zero_vp_type (vp)
         call da_transform_vpatox_adj(grid, be, ep, vp, nobwin)
         !as of V3.9, the calculation of vp%alpha is taken out of vptox_adj.
         !da_zero_vp_type(vp) is commented out in the da_transform_vtox_adj,
         !in order to pass in vp%alpha
         call da_transform_vtox_adj(grid, cv_size, xbx, be, ep, vp, vv, cv_local)
         cv = cv + cv_local
      end do

   end if ! var4d

   !--------------------------------------------------------------
   ! ADJ of Variational Bias Correction
   !--------------------------------------------------------------
#if defined(RTTOV) || defined(CRTM)
   if (use_varbc) call da_varbc_adj(cv_size, cv, iv, y)
#endif
   if (trace_use) call da_trace_exit("da_transform_vtoy_adj")

end subroutine da_transform_vtoy_adj


