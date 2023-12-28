  subroutine da_solve ( grid , config_flags)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! 
   ! Edited 09/06/2012: Allow for variable ntmax for each outer loop (Mike Kavulich)
   !-----------------------------------------------------------------------

   implicit none



   type (domain),               intent(inout) :: grid
   type (grid_config_rec_type), intent(inout) :: config_flags

   type (xbx_type)              :: xbx         ! For header & non-grid arrays.
   type (be_type)               :: be          ! Background error structure.
   real, allocatable            :: cvt(:)      ! Control variable structure.
   real, allocatable            :: xhat(:)     ! Control variable structure.
   real, allocatable            :: qhat(:,:)   ! Control variable structure.
   real*8, allocatable          :: eignvec(:,:)
   real*8, allocatable          :: eignval(:)   
!   real, allocatable            :: full_eignvec(:)   
   type (y_type)                :: ob          ! Observation structure.
   type (iv_type)               :: iv          ! Obs. increment structure.
   type (y_type)                :: re          ! Residual (o-a) structure.
   type (y_type)                :: y           ! y = H(x_inc) structure.
   integer                      :: it          ! External loop counter.
   integer                      :: neign       
   type (j_type)                :: j           ! Cost function.
   type (y_type)                :: jo_grad_y   ! Grad_y(jo)

   integer                      :: cv_size, i, ichan, k, n
   real                         :: j_grad_norm_target ! Target j norm.

   character (len=3)            :: ci
   character (len=2)            :: outerloop
   character (len=256)          :: timestr
   integer   :: min_yyyy,min_mm,min_dd,min_hh,min_mn,min_ss
   integer   :: max_yyyy,max_mm,max_dd,max_hh,max_mn,max_ss
   character :: s
   real*8    :: time_min, time_max 
   integer   :: jl_start, jl_end
   character(len=256) :: timestr1
   type(x_type) :: shuffle

   real, allocatable :: grid_box_area(:,:), mapfac(:,:)
   real, allocatable :: v1(:,:,:),v2(:,:,:),v3(:,:,:),v4(:,:,:),v5(:,:,:)
   real, allocatable :: v6(:,:,:),v7(:,:,:),v8(:,:,:),v9(:,:,:),v10(:,:,:),v11(:,:,:)
   character (len=10)    :: variable_name

   integer :: iwin, num_subtwindow
   integer :: ni, nj, ii, jj
   real    :: dx, dy, dxm, dym, cxp0, cxp1, cyp0, cyp1
   real,    external :: nest_loc_of_cg  ! from share/interp_fcn.F
   integer, external :: compute_CGLL    ! from share/interp_fcn.F

   integer   :: vp_unit, iost
   character(len=13) :: vpfile ! vp_input.0001
   integer   :: i1,i2,i3,i4,i5,i6
   logical :: ex
   logical :: offline_varbc

   character(len=10) :: this_time
   character(len=8)  :: this_date

   if (trace_use) call da_trace_entry("da_solve")

#ifdef DM_PARALLEL
   call mpi_barrier(comm,ierr)
#endif

   if ( config_flags%use_baseparam_fr_nml ) then
      call nl_get_base_pres  ( 1 , base_pres )
      call nl_get_base_temp  ( 1 , base_temp )
      call nl_get_base_lapse ( 1 , base_lapse )
      call nl_get_iso_temp   ( 1 , iso_temp )
      if ( iso_temp .NE. grid%tiso ) THEN
         write(unit=message(1),fmt='(A)') &
           'Namelist iso_temp does not equal iso_temp from fg. Reset namelist value and rerun.'
         write(unit=message(2),fmt='(A,F10.5)')'Namelist iso_temp   = ',iso_temp
         write(unit=message(3),fmt='(A,F10.5)')'Background iso_temp = ',grid%tiso
         call da_error(__FILE__,__LINE__,message(1:3))
      end if
      call nl_get_base_pres_strat  ( 1, base_pres_strat )
      call nl_get_base_lapse_strat ( 1, base_lapse_strat )

      grid%p00   = base_pres
      grid%t00   = base_temp
      grid%tlp   = base_lapse
      grid%tiso  = iso_temp
      grid%p_strat   = base_pres_strat
      grid%tlp_strat = base_lapse_strat
   else
      base_pres  = grid%p00
      base_temp  = grid%t00
      base_lapse = grid%tlp
      iso_temp   = grid%tiso
      base_pres_strat  = grid%p_strat
      base_lapse_strat = grid%tlp_strat
      if ( base_temp < 100.0 .or. base_pres < 10000.0 ) then
         write(unit=message(1),fmt='(A)') &
         'did not find base state parameters in fg. Add use_baseparam_fr_nml = .t. in &dynamics and rerun '
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
   end if

   ! Calculate the num_fgat_time based on time_window_min, time_window_max
    if ( var4d ) then
       if (time_step == 0) then
          write(unit=message(1),fmt='(A)') &
          'For 4DVAR, in the &domains namelist, "time_step" must be set to a non-zero value'
          call da_error(__FILE__,__LINE__,message(1:1))
       endif
       read(unit=time_window_min,fmt='(i4,5(a1,i2))') min_yyyy,s,min_mm,s,min_dd,s,min_hh,s,min_mn,s,min_ss
       read(unit=time_window_max,fmt='(i4,5(a1,i2))') max_yyyy,s,max_mm,s,max_dd,s,max_hh,s,max_mn,s,max_ss
       call da_get_julian_time(min_yyyy,min_mm,min_dd,min_hh,min_mn,time_min)
       call da_get_julian_time(max_yyyy,max_mm,max_dd,max_hh,max_mn,time_max)
       if ( var4d_bin < time_step ) call nl_set_var4d_bin (1, time_step)
       time_max = (time_max - time_min) * 60   ! unit is : seconds
       num_fgat_time = NINT(time_max/var4d_bin)
       if ( NINT(time_max/var4d_bin)*var4d_bin .ne. NINT(time_max) ) then
          write(unit=message(1),fmt='(A)') &
          '4DVAR assimilation window must be evenly divisible by var4d_bin!'
          write(unit=message(2),fmt='(A,I7)') &
          'var4d_bin       = ',var4d_bin
          write(unit=message(3),fmt='(A,A)') &
          'time_window_max = ',time_window_max
          write(unit=message(4),fmt='(A,A)') &
          'time_window_min = ',time_window_min
          write(unit=message(5),fmt='(A,F10.4)') &
          'time_window_max - time_window_min = ',time_max
          write(unit=message(6),fmt='(A)')'Change var4d_bin, time_window_max, or time_window_min in namelist and rerun'
          call da_error(__FILE__,__LINE__,message(1:6))
       endif
       if ( var4d_bin/time_step*time_step .ne. var4d_bin ) then
          write(unit=message(1),fmt='(A)') &
          'var4d_bin must be evenly divisible by time_step!'
          write(unit=message(2),fmt='(A,I7)') &
          'var4d_bin = ',var4d_bin
          write(unit=message(3),fmt='(A,I7)') &
          'time_step = ',time_step
          write(unit=message(4),fmt='(A)')'Change var4d_bin or time_step in namelist and rerun'
          call da_error(__FILE__,__LINE__,message(1:4))
       endif

       num_fgat_time = num_fgat_time + 1
       write(unit=message(1),fmt='(a,i10)') 'num_fgat_time is: ', num_fgat_time
       call da_message(message(1:1))
       if ( use_rainobs ) then
          allocate (fgat_rain_flags(1:num_fgat_time))
          fgat_rain_flags = .false.
          if ( INT(var4d_bin_rain/var4d_bin)*var4d_bin .ne. INT(var4d_bin_rain) ) then
             write(unit=message(1),fmt='(A,A,2I7)') &
             'Please change var4d_bin_rain in namelist and rerun==>', 'var4d_bin_rain, var4d_bin:',var4d_bin_rain,var4d_bin
             call da_error(__FILE__,__LINE__,message(1:1))
          endif
          do n = 1, num_fgat_time, INT(var4d_bin_rain/var4d_bin)
             fgat_rain_flags(n) = .true.
          end do
       end if
    endif

   !---------------------------------------------------------------------------
   ! [1.0] Initial checks
   !---------------------------------------------------------------------------

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

   if( use_rf )then
      if (0.5 * real(rf_passes) /= real(rf_passes / 2)) then
         write(unit=stdout,fmt='(A,I4,A)') &
            'rf_passes = ', rf_passes, ' .Should be even.'
         rf_passes = int(real(rf_passes / 2))
         write(unit=stdout,fmt='(A,I4)') 'Resetting rf_passes = ', rf_passes
      end if
   else
      write(stdout,'("da_solve: using wavelet transform")')
   endif

   if ( anal_type_hybrid_dual_res .and. alphacv_method .ne. alphacv_method_xa ) then
      write (unit=message(1),fmt='(A)') &
        'Dual-res hybrid only with alphacv_method = 2'
      call da_error(__FILE__,__LINE__,message(1:1))
   endif

   if (anal_type_randomcv) then
      write_gts_omb_oma  = .false.
      write_rej_obs_conv = .false.
      write_unpert_obs   = .false.
   end if

   if ( cv_options == 3 ) then
      if ( cloud_cv_options > 0 ) then
         write(unit=message(1),fmt='(A)') &
            'Cloud control variables are not implemented for cv_options = 3'
         write(unit=message(2),fmt='(A)') &
            'Resetting cloud_cv_options = 0 for cv_options = 3'
         cloud_cv_options = 0
         call da_warning(__FILE__,__LINE__,message(1:2))
      end if
      if ( ensdim_alpha > 0 ) then
         write(unit=message(1),fmt='(A)') &
            'Alpha control variables are not implemented for cv_options = 3'
         write(unit=message(2),fmt='(A)') &
            'Please set cv_options=5 or 7, or ensdim_alpha=0'
         call da_error(__FILE__,__LINE__,message(1:2))
      end if
   end if

   if ( use_radarobs ) then
      if ( use_radar_rf .and. use_radar_rhv ) then
         write(unit=message(1),fmt='(A)') "Both 'use_radar_rf' and 'use_radar_rhv' are set to true"
         write(unit=message(2),fmt='(A)') "You must only choose one of these options"
         call da_error(__FILE__,__LINE__,message(1:2))
      end if
      if ( (use_radar_rf .and. radar_rf_opt==1) .and. cloud_cv_options /= 1 ) then
         write(unit=message(1),fmt='(A)') "Please set cloud_cv_options=1 for use_radar_rf and radar_rf_opt=1"
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
      if ( use_radar_rhv .and. cloud_cv_options == 1 ) then
         write(unit=message(1),fmt='(A)') "Please set cloud_cv_options=3 or 2 (2 requires cloudy be.dat) for use_radar_rhv"
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
   end if

   if ( ensdim_alpha > 0 .and. alpha_hydrometeors ) then
      if ( cloud_cv_options == 1 ) then
         write(unit=message(1),fmt='(A)') "alpha_hydrometeors is not implemented for cloud_cv_options=1"
         write(unit=message(2),fmt='(A)') "Please choose cloud_cv_options=3"
         call da_error(__FILE__,__LINE__,message(1:2))
      end if
   end if

   if ( use_gpsrefobs .and. use_gpsephobs ) then
      write(unit=message(1),fmt='(A,I3)') &
         'Both use_gpsrefobs and use_gpsephobs are true. Should choose either use_gpsrefobs or use_gpsephobs'
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Initialise wrfvar parameters:
   !---------------------------------------------------------------------------

   if ( anal_type_hybrid_dual_res ) then

      !---------------------------------
      ! Get full ensemble grid dimensions
      !---------------------------------
      call da_solve_init(ensemble_grid &
#include "actual_new_args.inc"
)
      ide_ens = ide ! these are unstaggered dimensions of the full ensemble domain
      jde_ens = jde
      kde_ens = kde

      !---------------------------------------
      ! Get "intermediate" grid sizes and tiles
      !---------------------------------------
      call da_solve_init(grid%intermediate_grid &
#include "actual_new_args.inc"
)

      ! these are unstaggered dimensions of the "intermediate" ensemble domain
      !  The intermediate grid is the coarse (ensemble) domain that is co-located with the
      !  hi-resolution (analysis) grid

      ids_int = ids ; jds_int = jds ; kds_int = kds
      ide_int = ide ; jde_int = jde ; kde_int = kde
      
      its_int = its ; ite_int = ite
      jts_int = jts ; jte_int = jte
      kts_int = kts ; kte_int = kte

      ims_int = ims ; ime_int = ime
      jms_int = jms ; jme_int = jme
      kms_int = kms ; kme_int = kme

      ips_int = ips ; ipe_int = ipe
      jps_int = jps ; jpe_int = jpe
      kps_int = kps ; kpe_int = kpe


      grid%imask_xstag = 1   ; grid%imask_ystag = 1
      grid%imask_nostag = 1  ; grid%imask_xystag = 1

      !---------------------------------------------------------------------------
      ! De-allocate parts of grid and replace with grid%intermediate_grid dimensions
      !---------------------------------------------------------------------------
      call reallocate_analysis_grid(grid)

      !----------------------------------------------------------
      ! Allocate and initialize some of grid%intermediate_grid
      !----------------------------------------------------------
      call allocate_intermediate_grid(grid%intermediate_grid)


      !---------------------------------------
      ! Get map projection information for the ensemble
      !---------------------------------------

       call da_setup_firstguess(xbx, ensemble_grid, config_flags, .true. )
       map_info_ens = map_info ! map_info is read in from da_tools.f90, call it something else

   endif

   call da_solve_init(grid &
#include "actual_new_args.inc"
)

   if ( .not. anal_type_hybrid_dual_res ) then
      ide_ens = ide ; jde_ens = jde ; kde_ens = kde

      ids_int = ids ; ide_int = ide 
      jds_int = jds ; jde_int = jde 
      kds_int = kds ; kde_int = kde

      its_int = its ; ite_int = ite 
      jts_int = jts ; jte_int = jte
      kts_int = kts ; kte_int = kte

      ims_int = ims ; ime_int = ime
      jms_int = jms ; jme_int = jme
      kms_int = kms ; kme_int = kme

      ips_int = ips ; ipe_int = ipe
      jps_int = jps ; jpe_int = jpe
      kps_int = kps ; kpe_int = kpe
   endif

   !---------------------------------------------------------------------------
   ! [3.0] Set up first guess field (grid%xb):
   !---------------------------------------------------------------------------

   ! for WRF hybrid coordinate
   allocate ( c1f(kms:kme) )
   allocate ( c2f(kms:kme) )
   allocate ( c3f(kms:kme) )
   allocate ( c4f(kms:kme) )
   allocate ( c1h(kms:kme) )
   allocate ( c2h(kms:kme) )
   allocate ( c3h(kms:kme) )
   allocate ( c4h(kms:kme) )
   c1f = grid%c1f
   c2f = grid%c2f
   c3f = grid%c3f
   c4f = grid%c4f
   c1h = grid%c1h
   c2h = grid%c2h
   c3h = grid%c3h
   c4h = grid%c4h
   ! For WRFDA, namelist hybrid_opt setting is ignored.
   ! grid%hybrid_opt is set in da_med_initialdata_input.inc based on
   ! HYBRID_OPT global attribute contained in the input file.
   ! For input file prior to V3.9, grid%hybrid_opt is set to 0.
   if ( grid%hybrid_opt <= 0 ) then
      c3f = grid%znw
      c3h = grid%znu
      c4f = 0.0
      c4h = 0.0
      c1f = 1.0
      c1h = 1.0
      c2f = 0.0
      c2h = 0.0
   end if

   call da_setup_firstguess(xbx, grid, config_flags, .false.)

   if ( anal_type_hybrid_dual_res ) then

      ! 
      ! Get ensemble grid mapfactor on entire coarse grid
      ! 
      variable_name = 'MAPFAC_M'

      allocate( grid_box_area(1:ide_ens,1:jde_ens), mapfac(1:ide_ens,1:jde_ens) )  
      call da_get_var_2d_real_cdf( input_file_ens, variable_name, mapfac, ide_ens, jde_ens, 1, .false. )
      grid_box_area(:,:) = ( (ensemble_grid%dx)/mapfac(:,:) )**2 
      grid%intermediate_grid%xb%grid_box_area(its_int:ite_int,jts_int:jte_int) = grid_box_area(its_int:ite_int,jts_int:jte_int)
      deallocate(mapfac,grid_box_area)

      !
      ! get fine grid locs relative to coarse grid for bilinear interplation
      ! between coarse (ensemble) and fine (analysis) grids.
      ! use functions compute_CGLL and nest_loc_of_cg taken from
      ! share/interp_fcn.F
      !
      allocate(aens_locs(its:ite, jts:jte)) ! From da_control
      do nj = jts, jte
         jj = compute_CGLL (nj, grid%j_parent_start, grid%parent_grid_ratio, 1)
         cyp0 = nest_loc_of_cg (jj  , grid%j_parent_start, grid%parent_grid_ratio, 0)
         cyp1 = nest_loc_of_cg (jj+1, grid%j_parent_start, grid%parent_grid_ratio, 0)
         dym = ( cyp1 - real(nj) ) / ( cyp1 - cyp0 )
         dy = 1.0 - dym
         do ni = its, ite
            ii = compute_CGLL (ni, grid%i_parent_start, grid%parent_grid_ratio, 1)
            cxp0 = nest_loc_of_cg (ii  , grid%i_parent_start, grid%parent_grid_ratio, 0)
            cxp1 = nest_loc_of_cg (ii+1, grid%i_parent_start, grid%parent_grid_ratio, 0)
            dxm = ( cxp1 - real(ni) ) / ( cxp1 - cxp0 )
            dx = 1.0 - dxm
            aens_locs(ni,nj)%i   = ii
            aens_locs(ni,nj)%j   = jj
            aens_locs(ni,nj)%dx  = dx
            aens_locs(ni,nj)%dy  = dy
            aens_locs(ni,nj)%dxm = dxm
            aens_locs(ni,nj)%dym = dym
         end do
      end do

   end if !anal_type_hybrid_dual_res

   !---------------------------------------------------------------------------
   ! [4.0] Set up observations (ob):
   !---------------------------------------------------------------------------
   call da_setup_obs_structures (grid, ob, iv, j)
   if (use_rad) then
      allocate (j % jo % rad(1:iv%num_inst))
      do i=1,iv%num_inst
         allocate (j % jo % rad(i) % jo_ichan(iv%instid(i)%nchan))
         allocate (j % jo % rad(i) % num_ichan(iv%instid(i)%nchan))
      end do
   end if

   !---------------------------------------------------------------------------
   ! [4.1] Observer (ANAL_TYPE="VERIFY")
   !---------------------------------------------------------------------------

   if (anal_type_verify) then
      check_max_iv = .false.
      ntmax=0
      it = 1
      num_qcstat_conv=0

#if defined(RTTOV) || defined(CRTM)
      if (use_rad .and. (use_varbc.or.freeze_varbc)) call da_varbc_init(iv, be)
#endif

      call da_get_innov_vector (it, num_qcstat_conv, ob, iv, grid , config_flags)
      call da_allocate_y (iv, re)
 
      ! write out O-B statistics
      call da_write_diagnostics(it, grid,num_qcstat_conv, ob, iv, re, y, j)

      ! write out Gradient of Jo for adjoint sensitivity
      if (adj_sens) then
         cv_size = 1
         allocate (xhat(cv_size))
         call da_allocate_y (iv, y)
         call da_allocate_y (iv, jo_grad_y)

         call da_calculate_residual(iv, y, re)
         call da_calculate_grady(iv, re, jo_grad_y)
         call da_zero_x(grid%xa)

         call da_transform_xtoy_adj(cv_size, xhat, grid, iv, jo_grad_y, grid%xa)
         call da_transform_xtoxa_adj(grid)
         call da_transfer_wrftltoxa_adj(grid, config_flags, 'fcst', timestr)

         call da_deallocate_y (y)
         call da_deallocate_y (jo_grad_y)
      end if

      call da_deallocate_y(re)
      call da_deallocate_observations (iv)
      if (trace_use) call da_trace_exit ("da_solve")
      return
   end if
   
   !---------------------------------------------------------------------------
   ! [5.0] Set up control variable:
   !---------------------------------------------------------------------------
   be % cv % size_jb = 0
   be % cv % size_je = 0
   be % cv % size_jp = 0
   be % cv % size_js = 0
   be % cv % size_jl = 0
   be % cv % size_jt = 0
   
   !---------------------------------------------------------------------------
   ! [5.1] Set up background errors (be):
   !---------------------------------------------------------------------------
   if (use_background_errors .and. multi_inc /= 1) then
      call da_setup_background_errors (grid, be)
   else
      be%ncv_mz = 5
      allocate(be%cv_mz(5))
      be%cv_mz(:) = 0
      be % ne = ensdim_alpha
      be % v1 % mz = 0
      be % v2 % mz = 0
      be % v3 % mz = 0
      be % v4 % mz = 0
      be % v5 % mz = 0
      be % v6  % mz = 0
      be % v7  % mz = 0
      be % v8  % mz = 0
      be % v9  % mz = 0
      be % v10 % mz = 0
      be % v11 % mz = 0
   end if

    ! overwrite variables defined in da_setup_cv.inc set in the call to da_setup_background_errors
    if ( anal_type_hybrid_dual_res ) then
       be % cv % size_alphac = (ite_int - its_int + 1) * (jte_int - jts_int + 1) * be % alpha % mz * be % ne
       be % cv % size_je = be % cv % size_alphac
       cv_size_domain_je = (ide_int - ids_int + 1) * (jde_int - jds_int + 1) * be % alpha % mz * be % ne
    endif
   
   !---------------------------------------------------------------------------
   ! [5.2] Set up observation bias correction (VarBC):
   !---------------------------------------------------------------------------
#if defined(RTTOV) || defined(CRTM)
   if (use_rad .and. (use_varbc.or.freeze_varbc)) call da_varbc_init(iv, be)
#endif

   if ( be%cv%size_jb == 0 .and. be%cv%size_je == 0 .and. be%cv%size_jp > 0 ) then
      offline_varbc = .true.
   else
      offline_varbc = .false.
   end if

   if (use_tamdarobs .and. use_varbc_tamdar) then
        call da_varbc_tamdar_init(iv)
   else
        use_varbc_tamdar = .false.
   end if
   if (use_varbc_tamdar) call da_varbc_tamdar_pred(iv, be, ob)

   !---------------------------------------------------------------------------
   ! [5.3] Set up satellite control variable:
   !---------------------------------------------------------------------------
#if defined(RTTOV) || defined(CRTM)
   if (ANY(use_satcv)) call da_setup_satcv(iv, be)
#endif
   
   !---------------------------------------------------------------------------
   ! [5.4] Total control variable:
   !---------------------------------------------------------------------------   
   be % cv % size = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + be%cv%size_js + be%cv%size_jl + be%cv%size_jt
   cv_size = be % cv % size

   !---------------------------------------------------------------------------
   ! [6.0] Set up ensemble perturbation input:
   !---------------------------------------------------------------------------

      grid % ep % ne = be % ne
      if (use_background_errors .and. be % ne > 0) then

         call date_and_time(date=this_date, time=this_time)
         write(unit=message(1),fmt='(a,a8,1x,a12)') &
            '   Begin reading ensemble perturbations at ', this_date, &
            this_time(1:2)//':'//this_time(3:4)//' '//this_time(5:10)
         call da_message(message(1:1))

         select case ( ep_format )
            case ( 1 ) ! original behavior
               if ( ep_para_read == 0 ) then
                  ! each ep file is for one variable and one member
                  call da_setup_flow_predictors ( &
                     ide_ens, jde_ens, kde_ens, be % ne, grid%ep, &
                     its_int, ite_int, jts_int, jte_int, kts_int, kte_int )
               else if ( ep_para_read == 1 ) then
                  ! para_read_opt1, breaks total number of ep into reading bins
                  call da_setup_flow_predictors_para_read_opt1 ( &
                     ide_ens, jde_ens, kde_ens, be % ne, grid%ep, &
                     its_int, ite_int, jts_int, jte_int, kts_int, kte_int )
               end if
            case ( 11 )
               ! similar to ep_format=1 except data are in single precision
               ! each ep file is for one variable and one member
               call da_setup_flow_predictors ( &
                  ide_ens, jde_ens, kde_ens, be % ne, grid%ep, &
                  its_int, ite_int, jts_int, jte_int, kts_int, kte_int )
            case ( 2 )
               ! full-domain
               ! each ep file in single precision is for one variable and all members
               call da_setup_flow_predictors_ep_format2 ( &
                  ide_ens, jde_ens, kde_ens, be % ne, grid%ep, &
                  its_int, ite_int, jts_int, jte_int, kts_int, kte_int )
            case ( 3 )
               ! decomposed sub-domain
               ! each ep file in single precision is for one variable and all members
               call da_setup_flow_predictors_ep_format3 ( &
                  ide_ens, jde_ens, kde_ens, be % ne, grid%ep, &
                  its_int, ite_int, jts_int, jte_int, kts_int, kte_int )
         end select

         call date_and_time(date=this_date, time=this_time)
         write(unit=message(1),fmt='(a,a8,1x,a12)') &
            '  Finish reading ensemble perturbations at ', this_date, &
            this_time(1:2)//':'//this_time(3:4)//' '//this_time(5:10)
         call da_message(message(1:1))

      end if

   !---------------------------------------------------------------------------
   ! [7.0] Setup control variable (cv):
   !---------------------------------------------------------------------------

!  Dynamically allocate the variables which don't rely on ntmax
      allocate (cvt(1:cv_size))
      allocate (xhat(1:cv_size))
!      if (use_lanczos) then
!        allocate (full_eignvec(cv_size))
!      end if

      !------------------------------------------------------
      ! set CV to random noise ("RANDOMCV")
      !------------------------------------------------------
      if (anal_type_randomcv) then
         it = 1
         ! Initialize random number generator and scalars
         call da_random_seed
         do i= 1, n_randomcv
            write(ci,'(i3.3)') i
            write(unit=message(1),fmt='(a,a)') &
               '  Setting randomcv for wrfvar_output_randomcv.e', trim(ci)
            call da_message(message(1:1))
            call da_set_randomcv    (cv_size, cvt)
            call da_transform_vtox  (grid,cv_size,xbx,be,grid%ep,cvt,grid%vv,grid%vp)
            call da_transform_xtoxa (grid)
            call da_transfer_xatoanalysis (it, xbx, grid, config_flags)
            call da_update_firstguess(grid,'wrfvar_output_randomcv.e'//trim(ci))
            if ( i < n_randomcv ) then
               ! restore the original grid info for the next realization of randomcv
               call da_med_initialdata_input( grid , config_flags, 'fg')
               call da_setup_firstguess(xbx, grid, config_flags, .false.)
            end if
         end do
         ! Done with randomcv.
         ! Set the following to skip some code to get to the deallocation part.
         max_ext_its = 0
      end if  !anal_type_randomcv

! mri-4dvar: if multi_inc /= 2: run normal 3D/4D-Var
!------------------------------------------------------------------------
    ! cvt is outer loop control variable, it is zero for the first outer loop,
    ! but non-zero from the second outer loop in normal 3d/4dvar.
    ! for MRI-4DVar, vp from the previous outer loop needs to be read in, 
    ! then perform the inverse transform to derive cvt
    !-----------------------------------------------------
      call da_initialize_cv (cv_size, cvt)
      call da_zero_vp_type (grid%vp)
      call da_zero_vp_type (grid%vv)

      if ( multi_inc == 2 ) then
         if ( max_ext_its > 1 ) then
           max_ext_its=1
           write(unit=message(1),fmt='(a)') "Re-set max_ext_its = 1 for multi_inc==2"
           call da_message(message(1:1))
         end if

      ! read vp files for different PEs
      !----------------------------------
         write(unit=vpfile,fmt='(a,i4.4)') 'vp_input.',myproc
         inquire(file=trim(vpfile), exist=ex)
         if ( ex ) then
           call da_get_unit(vp_unit)
           open(unit=vp_unit,file=trim(vpfile),iostat=iost,form='UNFORMATTED',status='OLD')
           if (iost /= 0) then
             write(unit=message(1),fmt='(A,I5,A)') &
                "Error ",iost," opening vp file "//trim(vpfile)
             call da_error(__FILE__,__LINE__,message(1:1))
           end if
           if ( use_interpolate_cvt ) then         ! works for CV3?, 3D RF
               write(unit=message(1),fmt='(a)') 'Reading vv from : '//trim(vpfile) 
           elseif ( use_inverse_squarerootb ) then ! works for CV5,6,7, vertical EOF
               write(unit=message(1),fmt='(a)') 'Reading vp from : '//trim(vpfile)
           end if 
           call da_message(message(1:1))
           read(vp_unit) i1, i2, i3, i4, i5, i6 ! read dimension of patch for current processor
           allocate( v1(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1) )
           allocate( v2(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1) )
           allocate( v3(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1) )
           allocate( v4(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1) )
           allocate( v5(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1) )
           if ( cloud_cv_options >= 2 ) then
             allocate( v6(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1) )
             allocate( v7(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1) )
             allocate( v8(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1) )
             allocate( v9(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1) )
             allocate( v10(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1) )
           end if
           if ( use_cv_w ) allocate( v11(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1) )

           read(vp_unit) v1, v2, v3, v4, v5
           if ( cloud_cv_options >= 2 ) read(vp_unit) v6, v7, v8, v9, v10
           if ( use_cv_w ) read(vp_unit) v11

           if ( use_interpolate_cvt ) then
             grid%vv%v1(ips:ipe,jps:jpe,kps:kpe) = v1(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
             grid%vv%v2(ips:ipe,jps:jpe,kps:kpe) = v2(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
             grid%vv%v3(ips:ipe,jps:jpe,kps:kpe) = v3(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
             grid%vv%v4(ips:ipe,jps:jpe,kps:kpe) = v4(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
             grid%vv%v5(ips:ipe,jps:jpe,kps:kpe) = v5(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
             if ( cloud_cv_options >= 2 ) then
               grid%vv%v6(ips:ipe,jps:jpe,kps:kpe) = v6(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
               grid%vv%v7(ips:ipe,jps:jpe,kps:kpe) = v7(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
               grid%vv%v8(ips:ipe,jps:jpe,kps:kpe) = v8(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
               grid%vv%v9(ips:ipe,jps:jpe,kps:kpe) = v9(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
               grid%vv%v10(ips:ipe,jps:jpe,kps:kpe) = v10(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
             end if
             if ( use_cv_w ) then
               grid%vv%v11(ips:ipe,jps:jpe,kps:kpe) = v11(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
             end if
             call da_vv_to_cv( grid%vv, grid%xp, be%cv_mz, be%ncv_mz, cv_size, cvt ) 
           elseif ( use_inverse_squarerootb ) then
             grid%vp%v1(ips:ipe,jps:jpe,kps:kpe) = v1(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
             grid%vp%v2(ips:ipe,jps:jpe,kps:kpe) = v2(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
             grid%vp%v3(ips:ipe,jps:jpe,kps:kpe) = v3(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
             grid%vp%v4(ips:ipe,jps:jpe,kps:kpe) = v4(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
             grid%vp%v5(ips:ipe,jps:jpe,kps:kpe) = v5(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
             if ( cloud_cv_options >= 2 ) then
               grid%vp%v6(ips:ipe,jps:jpe,kps:kpe) = v6(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
               grid%vp%v7(ips:ipe,jps:jpe,kps:kpe) = v7(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
               grid%vp%v8(ips:ipe,jps:jpe,kps:kpe) = v8(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
               grid%vp%v9(ips:ipe,jps:jpe,kps:kpe) = v9(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
               grid%vp%v10(ips:ipe,jps:jpe,kps:kpe) = v10(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
             end if
             if ( use_cv_w ) then ! vertical stagging +1?
               grid%vp%v11(ips:ipe,jps:jpe,kps:kpe) = v11(1:i2-i1+1, 1:i4-i3+1, 1:i6-i5+1)
             end if
             !call da_write_vp(grid,grid%vp,'vp_input.global ') ! to verify correctness
             print '(/10X,"===> Use inverse transform of square-root B for outer-loop=",i2)', it
             if ( cv_options == 3 ) then
                write(unit=message(1),fmt='(A,I5,A)') &
                   "Error: inverse U transform not for cv_options = 3"
                call da_error(__FILE__,__LINE__,message(1:1))
             end if
             call da_transform_vtox_inv (grid,be%cv%size_jb,xbx,be,grid%ep,cvt(1:be%cv%size_jb),grid%vv,grid%vp)
           end if

           deallocate( v1 )
           deallocate( v2 )
           deallocate( v3 )
           deallocate( v4 )
           deallocate( v5 )
           if ( cloud_cv_options >= 2 ) then
             deallocate( v6 )
             deallocate( v7 )
             deallocate( v8 )
             deallocate( v9 )
             deallocate( v10 )
           end if
           if ( use_cv_w ) deallocate( v11 )

           close(vp_unit)
           call da_free_unit(vp_unit)

         else
           write(unit=message(1),fmt='(a)') "vp files '"//trim(vpfile)//"' does not exists, initiallizing cvt."
           call da_message(message(1:1))
           call da_initialize_cv (cv_size, cvt) ! perhaps better use da_error
         end if

         call da_zero_vp_type (grid%vv)
         call da_zero_vp_type (grid%vp)

      end if
! mri-4dvar -------------------------------------------

      if ( var4d ) then
#ifdef VAR4D
         call da_zero_vp_type (grid%vv6)
         call da_zero_vp_type (grid%vp6)
#endif
      end if

   !---------------------------------------------------------------------------
   ! [8] Outerloop
   !---------------------------------------------------------------------------

   j_grad_norm_target = 1.0
   do it = 1, max_ext_its

!  Dynamically allocate the variables which depend on ntmax
      if (use_lanczos) then
         allocate (qhat(1:cv_size, 0:ntmax(it)))
         allocate (eignvec(ntmax(it), ntmax(it)))
         allocate (eignval(ntmax(it)))
      end if

! Re-scale the variances and the scale-length for outer-loop > 1:
      if (it > 1 .and. (cv_options == 5 .or. cv_options == 7)) then
         print '(/10X,"===> Re-set BE SCALINGS for outer-loop=",i2)', it
         call  da_scale_background_errors ( be, it )
      else if (it > 1 .and. cv_options == 3) then
         print '(/10X,"===> Re-set CV3 BE SCALINGS for outer-loop=",i2)', it
         call  da_scale_background_errors_cv3 ( grid, be, it )
      endif

      call da_initialize_cv (cv_size, xhat)

! mri-4dvar----------------------
! Apply inverse transform of squareroot(B) for full-resolution non-stop Var
! from 2nd outer loop, this is to check correctness of inverse U transform
! does not apply this setting for real world application
!-----------------------------
      if (multi_inc == 0 .and. it > 1 .and. use_inverse_squarerootb .and. cv_options /= 3) then
         print '(/10X,"===> Use inverse transform of square-root B for outer-loop=",i2)', it
         call da_transform_vtox_inv (grid,be%cv%size_jb,xbx,be,grid%ep,cvt(1:be%cv%size_jb),grid%vv,grid%vp)
      endif

! Reinitialize cvt=0 for full-resolution non-stop Var for each loop
!------another option not tested --------------
      if (multi_inc == 0 .and. it > 1 .and. use_interpolate_cvt) then
         print '(/10X,"===> Reinitialize cvt as zeros for outer loop ",i2)', it
         call da_initialize_cv (cv_size, cvt)
      endif
! mri-4dvar------------------------

      ! [8.1] Calculate nonlinear model trajectory 

!     if (var4d .and. multi_inc /= 2 ) then
      if (var4d) then
#ifdef VAR4D
         if (it > 1) then
            call kj_swap (grid%u_2, model_grid%u_2, &
                          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
            call kj_swap (grid%v_2, model_grid%v_2, &
                          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
            call kj_swap (grid%w_2, model_grid%w_2, &
                          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
            call kj_swap (grid%t_2, model_grid%t_2, &
                          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
            call kj_swap (grid%ph_2, model_grid%ph_2, &
                          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
            call kj_swap (grid%p, model_grid%p, &
                          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
            model_grid%mu_2 = grid%mu_2
            model_grid%t2 = grid%t2
            model_grid%th2 = grid%th2
            model_grid%q2 = grid%q2
            model_grid%u10 = grid%u10
            model_grid%v10 = grid%v10
            model_grid%tsk = grid%tsk
            model_grid%psfc = grid%psfc

            do i = PARAM_FIRST_SCALAR, num_moist
               call kj_swap (grid%moist(:,:,:,i), model_grid%moist(:,:,:,i), &
                             grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
            enddo
            
            ! Update boundary condition of model

            model_grid%u_bxs = grid%u_bxs
            model_grid%u_bxe = grid%u_bxe
            model_grid%u_bys = grid%u_bys
            model_grid%u_bye = grid%u_bye
            model_grid%v_bxs = grid%v_bxs
            model_grid%v_bxe = grid%v_bxe
            model_grid%v_bys = grid%v_bys
            model_grid%v_bye = grid%v_bye
            model_grid%w_bxs = grid%w_bxs
            model_grid%w_bxe = grid%w_bxe
            model_grid%w_bys = grid%w_bys
            model_grid%w_bye = grid%w_bye
            model_grid%t_bxs = grid%t_bxs
            model_grid%t_bxe = grid%t_bxe
            model_grid%t_bys = grid%t_bys
            model_grid%t_bye = grid%t_bye
            model_grid%mu_bxs = grid%mu_bxs
            model_grid%mu_bxe = grid%mu_bxe
            model_grid%mu_bys = grid%mu_bys
            model_grid%mu_bye = grid%mu_bye
            model_grid%ph_bxs = grid%ph_bxs
            model_grid%ph_bxe = grid%ph_bxe
            model_grid%ph_bys = grid%ph_bys
            model_grid%ph_bye = grid%ph_bye
            model_grid%moist_bxs = grid%moist_bxs
            model_grid%moist_bxe = grid%moist_bxe
            model_grid%moist_bys = grid%moist_bys
            model_grid%moist_bye = grid%moist_bye

            model_grid%u_btxs = grid%u_btxs
            model_grid%u_btxe = grid%u_btxe
            model_grid%u_btys = grid%u_btys
            model_grid%u_btye = grid%u_btye
            model_grid%v_btxs = grid%v_btxs
            model_grid%v_btxe = grid%v_btxe
            model_grid%v_btys = grid%v_btys
            model_grid%v_btye = grid%v_btye
            model_grid%w_btxs = grid%w_btxs
            model_grid%w_btxe = grid%w_btxe
            model_grid%w_btys = grid%w_btys
            model_grid%w_btye = grid%w_btye
            model_grid%t_btxs = grid%t_btxs
            model_grid%t_btxe = grid%t_btxe
            model_grid%t_btys = grid%t_btys
            model_grid%t_btye = grid%t_btye
            model_grid%mu_btxs = grid%mu_btxs
            model_grid%mu_btxe = grid%mu_btxe
            model_grid%mu_btys = grid%mu_btys
            model_grid%mu_btye = grid%mu_btye
            model_grid%ph_btxs = grid%ph_btxs
            model_grid%ph_btxe = grid%ph_btxe
            model_grid%ph_btys = grid%ph_btys
            model_grid%ph_btye = grid%ph_btye
            model_grid%moist_btxs = grid%moist_btxs
            model_grid%moist_btxe = grid%moist_btxe
            model_grid%moist_btys = grid%moist_btys
            model_grid%moist_btye = grid%moist_btye

            ! Turn off model boundary reading as we already provide a new one.
            call da_model_lbc_off
         endif

         call nl_set_var4d_run (head_grid%id, .true.)
         call da_nl_model(it)
!      elseif (var4d .and. multi_inc == 2 ) then
#else
         write(unit=message(1),fmt='(A)')'Please re-compile the code with 4dvar option'
         call da_error(__FILE__,__LINE__,message(1:1))
#endif
      end if

      ! [8.2] Calculate innovation vector (O-B):

      num_qcstat_conv=0
      call da_get_innov_vector (it, num_qcstat_conv, ob, iv, grid , config_flags)
      if ( multi_inc == 1 ) then 
         if (trace_use) call da_trace_exit ("da_solve")
         return
      end if

      if (test_transforms .or. test_gradient) then

         if (test_gradient) then
            call da_allocate_y (iv, re)
            call da_allocate_y (iv, y)
            call da_check_gradient (grid, config_flags, cv_size, xhat, cvt, 1.0e-10, 8, &
                 xbx, be, iv, y, re, j)
            call da_deallocate_y (re)
            call da_deallocate_y (y)
         endif

         if (test_transforms) then
            call da_check (grid, config_flags, cv_size, xbx, be, grid%ep, iv, &
                        grid%vv, grid%vp, y)
         endif

         if (trace_use) call da_trace_exit("da_solve")
         return

      end if

      ! [8.4] Minimize cost function:

      call da_allocate_y (iv, re)
      call da_allocate_y (iv, y)

      if (use_lanczos) then
         if (read_lanczos) then
            call da_lanczos_io('r',cv_size,ntmax(it),neign,eignvec,eignval,qhat)

            call da_kmat_mul(grid,config_flags,it,cv_size,xbx, &
                             be,iv,xhat,qhat,cvt,re,y,j,eignvec,eignval,neign)
          ! Output Cost Function
            call da_calculate_j(it, 1, cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
                             be%cv%size_jl, be%cv%size_jt, xbx, be, iv, xhat, cvt, re, y, j, grid, config_flags )

         else
            call da_minimise_lz(grid, config_flags, it, cv_size, xbx,& 
                             be, iv, j_grad_norm_target, xhat, qhat, cvt, re, y, j, eignvec, eignval, neign )


         end if

         if (write_lanczos) call da_lanczos_io('w',cv_size,ntmax(it),neign,eignvec,eignval,qhat)

         if (adj_sens) call da_sensitivity(grid,config_flags,it,cv_size,xbx, &
                                           be,iv,xhat,qhat,cvt,y,eignvec,eignval,neign )

       
      else

         call da_minimise_cg( grid, config_flags, it, be % cv % size, & 
              xbx, be, iv, j_grad_norm_target, xhat, cvt, re, y, j)
      end if
 
      ! Update outer-loop control variable
      cvt = cvt + xhat
      if ( multi_inc == 2 .and. use_interpolate_cvt ) then ! obsolete option
        call da_cv_to_vv( cv_size, cvt, be%cv_mz, be%ncv_mz, grid%vv )
        call da_write_vp(grid,grid%vv,'vp_output.global') ! wrtie vv to vp file
      end if
      !------------------------------------------------------------------------

      ! [8.5] Update latest analysis solution:

      if (.not. var4d) then
         ! as of V3.9, vp%alpha is not included in the vptox (called by vtox) calculation,
         ! here grid%xa contains only the static increment

#if (WRF_CHEM == 1)
   if (cv_options == 7 .and. chem_cv_options>=10) then
         call da_transform_vtox (grid,cv_size,xbx,be,grid%ep,xhat,grid%vv,grid%vp,vchem=grid%vchem)
   end if
#else
         call da_transform_vtox (grid,cv_size,xbx,be,grid%ep,xhat,grid%vv,grid%vp)
#endif

         ! adding the ensemble contribution
         if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
            if ( use_4denvar ) then
               ! add it to the analysis time ifgat_ana
               ! ifgat_ana is declared in da_control.f90 and defined in da_get_time_slots.inc
               write(unit=message(1),fmt='(a)')    ''
               write(unit=message(2),fmt='(a,i3)') 'Applying ensemble contribution to FGAT time: ', ifgat_ana
               call da_message(message(1:2))
               call da_transform_vpatox (grid,be%ne,grid%ep,grid%vp,ifgat_ana)
            else
               ! ep does not have time dimension for non-4denvar
               call da_transform_vpatox (grid,be%ne,grid%ep,grid%vp)
            end if
            call da_add_xa(grid%xa, grid%xa_ens) !grid%xa = grid%xa + xa_ens
         end if !ne>0
      else
         call da_transform_vtox (grid,be%cv%size_jb,xbx,be,grid%ep,xhat(1:be%cv%size_jb),grid%vv,grid%vp)
         call da_transform_vpatox (grid,be%ne,grid%ep,grid%vp)
      endif

! mri-4dvar--------------------------
      if (multi_inc == 2 .and. use_inverse_squarerootb) then
         call da_write_vp(grid,grid%vp,'vp_output.global') ! write vp to vp file
      end if
! mri-4dvar--------------------------

      call da_transform_xtoxa (grid)

      ! [8.6] Only when use_radarobs = .false. and calc_w_increment =.true.,
      !       the w_increment need to be diagnosed:

      if (calc_w_increment .and. .not. use_radarobs .and. .not. var4d) then
         call da_uvprho_to_w_lin (grid)

#ifdef DM_PARALLEL
#include "HALO_RADAR_XA_W.inc"
#endif
      end if

      ! [8.7] Write out diagnostics

      call da_write_diagnostics (it, grid, num_qcstat_conv, ob, iv, re, y, j)

      ! Write "clean" QCed observations if requested:
      if (anal_type_qcobs) then
         ! if (it == 1) then
          if (write_mod_filtered_obs) then
            call da_write_modified_filtered_obs (grid, ob, iv, &
               coarse_ix, coarse_jy, start_x, start_y)
          else
            call da_write_filtered_obs (it, grid, ob, iv, &
               coarse_ix, coarse_jy, start_x, start_y)
          end if     
         ! end if     
      end if

      ! [8.7.1] Write Ascii radar OMB and OMA file

      if ( use_radarobs .and. write_oa_radar_ascii ) then
         call da_write_oa_radar_ascii (ob,iv,re,it)
      end if

      ! [8.3] Interpolate x_g to low resolution grid

      ! [8.8] Write Ascii radiance OMB and OMA file

#if defined(CRTM) || defined(RTTOV)
      if (use_rad .and. write_oa_rad_ascii) then
         call da_write_oa_rad_ascii (it,ob,iv,re)
      end if
#endif

      ! [8.9] Update VarBC parameters and write output file
#if defined(CRTM) || defined(RTTOV)      
      if ( use_rad .and. (use_varbc.or.freeze_varbc) ) &
                call da_varbc_update(it, cv_size, xhat, iv)
#endif

      if (use_varbc_tamdar) &
         call da_varbc_tamdar_update(cv_size, xhat, iv)

      !------------------------------------------------------------------------
      ! [8.10] Output WRFVAR analysis and analysis increments:
      !------------------------------------------------------------------------

      if ( .not. offline_varbc ) &
      call da_transfer_xatoanalysis (it, xbx, grid, config_flags)

     if ( it < max_ext_its .and. print_detail_outerloop ) then
        write(outerloop,'(i2.2)') it
        call da_update_firstguess(grid,'wrfvar_output_'//outerloop)
#ifdef VAR4D
        !if (var4d) call da_med_initialdata_output_lbc (grid , config_flags, 'wrfvar_bdyout_'//outerloop)
#endif
     end if

     call da_deallocate_y (re)
     call da_deallocate_y (y)


   ! Deallocate arrays which depend on ntmax
     if (use_lanczos) then
        deallocate (qhat)
        deallocate (eignvec)
        deallocate (eignval)
     end if


   end do

   ! output wrfvar analysis

   if ((config_flags%real_data_init_type == 1) .or. &
       (config_flags%real_data_init_type == 3)) then
      if ( .not. offline_varbc ) &
      call da_update_firstguess(input_grid)
#ifdef VAR4D
      !if (var4d) call da_med_initialdata_output_lbc (head_grid , config_flags)
      if ( var4d_lbc ) then
         call domain_clock_get (grid, stop_timestr=timestr1)
         call domain_clock_set( grid, current_timestr=timestr1 )
         call da_med_initialdata_input (grid, config_flags, 'fg02')
         call da_setup_firstguess(xbx, grid, config_flags, .false. )
         shuffle = grid%xa
         jl_start    = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + 1
         jl_end      = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + be%cv%size_jl
         grid%xa  = grid%x6a
         call da_transform_vtox(grid, be%cv%size_jl, xbx, be, grid%ep, &
              xhat(jl_start:jl_end), grid%vv6, grid%vp6)
         grid%xa  = shuffle
         call da_transfer_xatoanalysis (it, xbx, grid, config_flags)
         call da_update_firstguess (grid, 'ana02')
         call domain_clock_get (grid, start_timestr=timestr1)
         call domain_clock_set( grid, current_timestr=timestr1 )
      endif
#endif
      call med_shutdown_io (input_grid, config_flags)
   end if

   !---------------------------------------------------------------------------
   ! [9.0] Tidy up:
   !---------------------------------------------------------------------------

   deallocate (cvt)
   deallocate (xhat)
!   if (use_lanczos) then
!      deallocate (full_eignvec)
!   end if

   ! clean up radiance related arrays
#if defined(RTTOV) || defined(CRTM)
   if (use_rad) then
      call da_deallocate_radiance (ob, iv, j)
      deallocate (time_slots)
#ifdef RTTOV
      if (rtm_option == rtm_option_rttov) then
         deallocate (coefs)
         deallocate (opts)
      end if
#endif
   end if
#endif

   if (var4d .and. use_rainobs) deallocate(fgat_rain_flags)
   call da_deallocate_observations (iv)
   call da_deallocate_y (ob)
   if (use_background_errors .and. jb_factor>0.0) call da_deallocate_background_errors (be)

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

   if ( anal_type_hybrid_dual_res ) deallocate(aens_locs)

   if (use_varbc_tamdar) then
      deallocate (iv%varbc_tamdar%nobs)
      deallocate (iv%varbc_tamdar%nobs_sum)
      deallocate (iv%varbc_tamdar%tail_id)
      deallocate (iv%varbc_tamdar%obs_sn)
      deallocate (iv%varbc_tamdar%ifuse)
      deallocate (iv%varbc_tamdar%index)
      deallocate (iv%varbc_tamdar%pred)
      deallocate (iv%varbc_tamdar%param)
      deallocate (iv%varbc_tamdar%bgerr)
      deallocate (iv%varbc_tamdar%vtox)
   end if

   deallocate ( c1f )
   deallocate ( c2f )
   deallocate ( c3f )
   deallocate ( c4f )
   deallocate ( c1h )
   deallocate ( c2h )
   deallocate ( c3h )
   deallocate ( c4h )

#ifdef DM_PARALLEL
   call mpi_barrier (comm,ierr)
#endif

   if (trace_use) call da_trace_exit ("da_solve")

end subroutine da_solve

