subroutine da_check_xtoy_adjoint(cv_size, cv, xbx, be, grid, config_flags, iv, y)
   
   !--------------------------------------------------------------------------
   ! Purpose: Test observation operator transform and adjoint for compatibility.
   !
   ! Method:  Standard adjoint test: < y, y > = < x, x_adj >.
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !---------------------------------------------------------------------------
   
   implicit none
   
   integer, intent(in)                       :: cv_size ! Size of cv array.
   type (be_type),             intent(in)    :: be    ! background error structure.
   real, intent(inout)                       :: cv(1:cv_size)   ! control variables.
   type (xbx_type),            intent(inout) :: xbx   ! Header & non-gridded vars.
   type (domain),              intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags
   type (iv_type),             intent(inout) :: iv    ! ob. increment vector.
   type (y_type),              intent(inout) :: y     ! y = h (grid%xa)

   real                           :: adj_ttl_lhs   ! < y, y >
   real                           :: adj_ttl_rhs   ! < x, x_adj >

   real                           :: partial_lhs   ! < y, y >
   real                           :: partial_rhs   ! < x, x_adj >

   real                           :: pertile_lhs   ! < y, y >
   real                           :: pertile_rhs   ! < x, x_adj >
 
   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_u, xa2_v, xa2_t, &
                                                 xa2_p, xa2_q, xa2_rh
   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_w
   real, dimension(ims:ime, jms:jme)          :: xa2_psfc
   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_qcw, xa2_qci, xa2_qrn, xa2_qsn, xa2_qgr
   real, dimension(ims:ime, jms:jme, kms:kme) :: x6a2_u, x6a2_v, x6a2_t, &
                                                 x6a2_p, x6a2_q, x6a2_rh
   real, dimension(ims:ime, jms:jme, kms:kme) :: x6a2_w
   real, dimension(ims:ime, jms:jme)          :: x6a2_psfc
   real, dimension(ims:ime, jms:jme, kms:kme) :: x6a2_qcw, x6a2_qci, x6a2_qrn, x6a2_qsn, x6a2_qgr
   real, dimension(:,:,:), allocatable :: a_hr_rainc, a_hr_rainnc
   integer :: nobwin, i, j, k, fgat_rain
   character(len=4) :: filnam
   character(len=256) :: timestr
   integer :: time_step_seconds
   type(x_type) :: shuffle
   real             :: subarea, whole_area

   if (trace_use) call da_trace_entry("da_check_xtoy_adjoint")

   write (unit=stdout, fmt='(/a/)') 'da_check_xtoy_adjoint: Test Results:'

   !----------------------------------------------------------------------
   ! [1.0] Initialise:
   !----------------------------------------------------------------------

   partial_lhs = 0.0
   pertile_lhs = 0.0

#ifdef A2C
  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. ide == ipe ) then
     ipe = ipe + 1
     ide = ide + 1
  end if

  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. jde == jpe ) then
     jpe = jpe + 1
     jde = jde + 1
  end if
#endif

#ifdef DM_PARALLEL
#include "HALO_XA.inc"
#endif

#ifdef A2C
  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. ide == ipe ) then
     ipe = ipe - 1
     ide = ide - 1
  end if

  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. jde == jpe ) then
     jpe = jpe - 1
     jde = jde - 1
  end if
#endif
   xa2_u(ims:ime, jms:jme, kms:kme) = grid%xa%u(ims:ime, jms:jme, kms:kme)
   xa2_v(ims:ime, jms:jme, kms:kme) = grid%xa%v(ims:ime, jms:jme, kms:kme)
   xa2_t(ims:ime, jms:jme, kms:kme) = grid%xa%t(ims:ime, jms:jme, kms:kme)
   xa2_p(ims:ime, jms:jme, kms:kme) = grid%xa%p(ims:ime, jms:jme, kms:kme)
   xa2_q(ims:ime, jms:jme, kms:kme) = grid%xa%q(ims:ime, jms:jme, kms:kme)
   xa2_w(ims:ime, jms:jme, kms:kme) = grid%xa%w(ims:ime, jms:jme, kms:kme)
   xa2_rh(ims:ime, jms:jme, kms:kme)= grid%xa%rh(ims:ime, jms:jme, kms:kme)
   xa2_psfc(ims:ime, jms:jme)       = grid%xa%psfc(ims:ime, jms:jme)

   xa2_qcw = 0.0
   xa2_qrn = 0.0
   xa2_qci = 0.0
   xa2_qsn = 0.0
   xa2_qgr = 0.0
   if ( cloud_cv_options >= 1 ) then
      xa2_qcw(ims:ime, jms:jme, kms:kme) = grid%xa%qcw(ims:ime, jms:jme, kms:kme)
      xa2_qrn(ims:ime, jms:jme, kms:kme) = grid%xa%qrn(ims:ime, jms:jme, kms:kme)
      if ( cloud_cv_options >= 2 ) then
         xa2_qci(ims:ime, jms:jme, kms:kme) = grid%xa%qci(ims:ime, jms:jme, kms:kme)
         xa2_qsn(ims:ime, jms:jme, kms:kme) = grid%xa%qsn(ims:ime, jms:jme, kms:kme)
         xa2_qgr(ims:ime, jms:jme, kms:kme) = grid%xa%qgr(ims:ime, jms:jme, kms:kme)
      end if
   end if

   x6a2_u = 0.0
   x6a2_v = 0.0
   x6a2_t = 0.0
   x6a2_p = 0.0
   x6a2_q = 0.0
   x6a2_w = 0.0
   x6a2_rh = 0.0
   x6a2_psfc = 0.0

   x6a2_qcw = 0.0
   x6a2_qci = 0.0
   x6a2_qrn = 0.0
   x6a2_qsn = 0.0
   x6a2_qgr = 0.0

#ifdef A2C
    if( ite == ide ) &    
print*,__FILE__,jte,' xa2_u.xa2_u for col= ',ite+1,sum(xa2_u(ite+1, jts:jte, kts:kte) * xa2_u(ite+1, jts:jte, kts:kte))
    if( jte == jde ) &    
print*,__FILE__,jte,' xa2_v.xa2_v for row= ',jte+1,sum(xa2_v(its:ite, jte+1, kts:kte) * xa2_v(its:ite, jte+1, kts:kte))
#endif
   if (var4d) then
#ifdef VAR4D
      call domain_clock_get( grid, current_timestr=timestr )
      call da_transfer_xatowrftl(grid, config_flags, 'tl01', timestr)

      if ( var4d_lbc ) then
         call domain_clock_get (grid, stop_timestr=timestr)
         call domain_clock_set( grid, current_timestr=timestr )
         grid%u_2 = u6_2 ; grid%v_2 = v6_2; grid%t_2 = t6_2;
         grid%w_2 = w6_2 ; grid%mu_2 = mu6_2 ; grid%ph_2 =ph6_2
         grid%moist = moist6; grid%p = p6; grid%psfc = psfc6
         call da_transfer_wrftoxb(xbx, grid, config_flags)

         call da_zero_x(grid%x6a)
         shuffle = grid%xa
         grid%xa  = grid%x6a
         call da_setup_testfield(grid)
         grid%xa  = shuffle

         x6a2_u(ims:ime, jms:jme, kms:kme) = grid%x6a%u(ims:ime, jms:jme, kms:kme)
         x6a2_v(ims:ime, jms:jme, kms:kme) = grid%x6a%v(ims:ime, jms:jme, kms:kme)
         x6a2_t(ims:ime, jms:jme, kms:kme) = grid%x6a%t(ims:ime, jms:jme, kms:kme)
         x6a2_p(ims:ime, jms:jme, kms:kme) = grid%x6a%p(ims:ime, jms:jme, kms:kme)
         x6a2_q(ims:ime, jms:jme, kms:kme) = grid%x6a%q(ims:ime, jms:jme, kms:kme)
         x6a2_w(ims:ime, jms:jme, kms:kme) = grid%x6a%w(ims:ime, jms:jme, kms:kme)
         x6a2_rh(ims:ime, jms:jme, kms:kme)= grid%x6a%rh(ims:ime, jms:jme, kms:kme)
         x6a2_psfc(ims:ime, jms:jme)       = grid%x6a%psfc(ims:ime, jms:jme)

         x6a2_qcw(ims:ime, jms:jme, kms:kme) = grid%x6a%qcw(ims:ime, jms:jme, kms:kme)
         x6a2_qci(ims:ime, jms:jme, kms:kme) = grid%x6a%qci(ims:ime, jms:jme, kms:kme)
         x6a2_qrn(ims:ime, jms:jme, kms:kme) = grid%x6a%qrn(ims:ime, jms:jme, kms:kme)
         x6a2_qsn(ims:ime, jms:jme, kms:kme) = grid%x6a%qsn(ims:ime, jms:jme, kms:kme)
         x6a2_qgr(ims:ime, jms:jme, kms:kme) = grid%x6a%qgr(ims:ime, jms:jme, kms:kme)

         call da_transfer_xatowrftl_lbc(grid, config_flags, 'tl01')

         call domain_clock_get( grid, start_timestr=timestr )
         call domain_clock_set( grid, current_timestr=timestr )
         call da_read_basicstates ( xbx, grid, config_flags, timestr )
      else
         call da_transfer_xatowrftl_lbc(grid, config_flags, 'tl01')
      endif

      call da_tl_model

      if ( use_rainobs .and. num_fgat_time > 1 ) then
         allocate (a_hr_rainc (ims:ime,jms:jme,1:num_fgat_time))
         allocate (a_hr_rainnc(ims:ime,jms:jme,1:num_fgat_time))
         a_hr_rainc =0.0
         a_hr_rainnc=0.0
      endif

      if (jcdfi_use) then

         subarea = SUM ( grid%xb%grid_box_area(its:ite,jts:jte) )
         whole_area = wrf_dm_sum_real(subarea)

         do j = jms, jme
            do k = kms, kme
               do i = ims, ime
                  pertile_lhs = pertile_lhs - config_flags%jcdfi_penalty * model_grid%jcdfi_u(i,k,j)**2 * &
                        grid%xb%grid_box_area(i,j)/whole_area*grid%xb%dnw(k)
                  pertile_lhs = pertile_lhs - config_flags%jcdfi_penalty * model_grid%jcdfi_v(i,k,j)**2 * &
                        grid%xb%grid_box_area(i,j)/whole_area*grid%xb%dnw(k)
                  pertile_lhs = pertile_lhs - config_flags%jcdfi_penalty * model_grid%jcdfi_t(i,k,j)**2 * &
                        (9.81/3.0)**2*grid%xb%grid_box_area(i,j)/whole_area*grid%xb%dnw(k)
                  pertile_lhs = pertile_lhs - config_flags%jcdfi_penalty * model_grid%jcdfi_p(i,k,j)**2 * &
                        (1.0/300.)**2*grid%xb%grid_box_area(i,j)/whole_area*grid%xb%dnw(k)
               enddo
            enddo
         enddo 

         ! We can not calculate the Y just with tile.
         partial_lhs = pertile_lhs
      endif

#else
      write(unit=message(1),fmt='(A)')'Please recompile the code with 4dvar option' 
      call da_error(__FILE__,__LINE__,message(1:1))
#endif
   end if

   if ( num_fgat_time > 1 ) then
      call domain_clock_get (grid, stop_timestr=timestr)
      call domain_clock_set( grid, current_timestr=timestr )
      call domain_clock_set (grid, time_step_seconds=-1*var4d_bin)
      call domain_clockprint(150, grid, 'get CurrTime from clock,')
   endif

   fgat_rain = num_fgat_time
   do nobwin= num_fgat_time, 1, -1

      iv%time = nobwin
      iv%info(:)%n1 = iv%info(:)%plocal(iv%time-1) + 1
      iv%info(:)%n2 = iv%info(:)%plocal(iv%time)

      if (var4d) then
#ifdef VAR4D
         call domain_clock_get( grid, current_timestr=timestr )
         call da_read_basicstates ( xbx, grid, config_flags, timestr )

         write(filnam,'(a2,i2.2)') 'tl',nobwin
         call da_transfer_wrftltoxa(grid, config_flags, filnam, timestr)

         if ( use_rainobs ) then
            if ( num_fgat_time > 1 .and. fgat_rain_flags(nobwin) ) then
!!!            We can not calculate the hourly rainfall in adjoint check, here, we just make sure the adjoint 
!!!            algorithm is correct mathmatically. so the amount of forcings doesn't matter
               a_hr_rainc (:,:,fgat_rain)=grid%g_rainc(:,:)
               a_hr_rainnc(:,:,fgat_rain)=grid%g_rainnc(:,:)
            endif
         endif
#endif
      end if

      call da_pt_to_rho_lin(grid)
#ifdef DM_PARALLEL
#include "HALO_XA.inc"
#endif

      if (sfc_assi_options == 2) then
         call da_transform_xtowtq (grid)
#ifdef DM_PARALLEL
#include "HALO_SFC_XA.inc"
#endif
      end if

      if (use_ssmt1obs .or. use_ssmt2obs .or. use_gpspwobs .or. &
          use_gpsztdobs .or. use_gpsrefobs .or. use_gpsephobs .or. &
          use_ssmitbobs .or. use_ssmiretrievalobs) then

         ! Now do something for PW
         call da_transform_xtotpw(grid)

         ! GPS Refractivity:
         if (use_gpsrefobs .or. use_gpsztdobs .or. use_gpsephobs) then
            call da_transform_xtogpsref_lin(grid)
            if (use_gpsztdobs) call da_transform_xtoztd_lin(grid)
         end if

         if (use_ssmt1obs .or. use_ssmt2obs .or. &
             use_ssmitbobs .or. use_ssmiretrievalobs) then
            if (global) then
              call da_error(__FILE__,__LINE__, &
                (/"grid%xb%speed is not available, see da_transfer_kmatoxb.inc"/))
            end if
            call da_transform_xtoseasfcwind_lin(grid)
         end if

         if (use_ssmitbobs) call da_transform_xtotb_lin (grid)

#ifdef DM_PARALLEL
#include "HALO_SSMI_XA.inc"
#endif
      end if

   ! Compute w increments using Richardson's eqn.

   if ( Use_RadarObs ) then
      if ( .not. var4d ) call da_uvprho_to_w_lin(grid)

      do k=kts,kte
         do j=jts,jte
            do i=its,ite
               grid%xa%wh(i,j,k)=0.5*(grid%xa%w(i,j,k)+grid%xa%w(i,j,k+1))
            end do
         end do
      end do

#ifdef DM_PARALLEL
#include "HALO_RADAR_XA_W.inc"
#endif
   end if

   if ( cloud_cv_options == 1 )then
      ! Partition of hydrometeor increments via warm rain process
      call da_moist_phys_lin(grid)
   end if

      !----------------------------------------------------------------------
      ! [2.0] Perform y = Hx transform:
      !----------------------------------------------------------------------
      call da_transform_xtoy (cv_size, cv, grid, iv, y)

#ifdef VAR4D
      if (iv%info(rain)%nlocal > 0 .and. var4d) &
         call da_transform_xtoy_rain (grid, iv, y, a_hr_rainc(:,:,nobwin), a_hr_rainnc(:,:,nobwin))
#endif      

      !----------------------------------------------------------------------
      ! [3.0] Calculate LHS of adjoint test equation and
      !       Rescale input to adjoint routine :
      !----------------------------------------------------------------------

      if (iv%info(sound)%nlocal > 0) call da_check_xtoy_adjoint_sound(iv, y, partial_lhs, pertile_lhs)
      if (iv%info(sonde_sfc)%nlocal > 0) call da_check_xtoy_adjoint_sonde_sfc (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(mtgirs)%nlocal   > 0) call da_check_xtoy_adjoint_mtgirs   (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(tamdar)%nlocal   > 0) call da_check_xtoy_adjoint_tamdar   (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(tamdar_sfc)%nlocal   > 0) call da_check_xtoy_adjoint_tamdar_sfc(iv, y, partial_lhs, pertile_lhs)
      if (iv%info(synop)%nlocal    > 0) call da_check_xtoy_adjoint_synop    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(geoamv)%nlocal   > 0) call da_check_xtoy_adjoint_geoamv   (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(polaramv)%nlocal > 0) call da_check_xtoy_adjoint_polaramv (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(airep)%nlocal    > 0) call da_check_xtoy_adjoint_airep    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(pilot)%nlocal    > 0) call da_check_xtoy_adjoint_pilot    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(radar)%nlocal    > 0) call da_check_xtoy_adjoint_radar    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(satem)%nlocal    > 0) call da_check_xtoy_adjoint_satem    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(metar)%nlocal    > 0) call da_check_xtoy_adjoint_metar    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(ships)%nlocal    > 0) call da_check_xtoy_adjoint_ships    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(gpspw)%nlocal    > 0) call da_check_xtoy_adjoint_gpspw    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(gpsref)%nlocal   > 0) call da_check_xtoy_adjoint_gpsref   (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(gpseph)%nlocal   > 0) call da_check_xtoy_adjoint_gpseph   (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(ssmi_tb)%nlocal  > 0) call da_check_xtoy_adjoint_ssmi_tb  (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(ssmi_rv)%nlocal  > 0) call da_check_xtoy_adjoint_ssmi_rv  (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(ssmt2)%nlocal    > 0) call da_check_xtoy_adjoint_ssmt1    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(ssmt2)%nlocal    > 0) call da_check_xtoy_adjoint_ssmt2    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(qscat)%nlocal    > 0) call da_check_xtoy_adjoint_qscat    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(profiler)%nlocal > 0) call da_check_xtoy_adjoint_profiler (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(buoy)%nlocal     > 0) call da_check_xtoy_adjoint_buoy     (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(bogus)%nlocal    > 0) call da_check_xtoy_adjoint_bogus    (iv, y, partial_lhs, pertile_lhs)
      if (iv%num_inst              > 0) call da_check_xtoy_adjoint_rad      (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(rain)%nlocal     > 0) call da_check_xtoy_adjoint_rain     (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(pseudo)%nlocal   > 0) call da_check_xtoy_adjoint_pseudo   (iv, y, partial_lhs, pertile_lhs)

      !----------------------------------------------------------------------
      ! [5.0] Perform adjoint operation:
      !----------------------------------------------------------------------
      call da_zero_x (grid%xa)

      if (use_rainobs .and. num_fgat_time > 1) then
         a_hr_rainc(:,:,nobwin) = 0.0
         a_hr_rainnc(:,:,nobwin) = 0.0
      endif

#ifdef VAR4D
      if (iv%info(rain)%nlocal > 0 .and. var4d) then
         call da_transform_xtoy_rain_adj (grid, iv, y, a_hr_rainc(:,:,nobwin), a_hr_rainnc(:,:,nobwin))
      endif
#endif

      call da_transform_xtoy_adj (cv_size, cv, grid, iv, y, grid%xa)

#ifdef A2C
    if( ite == ide ) &    
print*,__FILE__,jte,' grid%xa%u.grid%xa%u for col= ',ite+1,sum(grid%xa%u(ite+1, jts:jte, kts:kte) * grid%xa%u(ite+1, jts:jte, kts:kte))
    if( jte == jde ) &    
print*,__FILE__,jte,' grid%xa%v.grid%x%%v for row= ',jte+1,sum(grid%xa%v(its:ite, jte+1, kts:kte) * grid%xa%v(its:ite, jte+1, kts:kte))
#endif

   ! Compute w increments using Richardson's eqn.
   if ( Use_RadarObs)  then
      do k=kts,kte
         do j=jts,jte
            do i=its,ite
               grid%xa%w(i,j,k)=grid%xa%w(i,j,k)+0.5*grid%xa%wh(i,j,k)
               grid%xa%w(i,j,k+1)=grid%xa%w(i,j,k+1)+0.5*grid%xa%wh(i,j,k)
               grid%xa%wh(i,j,k)=0.0
            end do
         end do
      end do

      if ( .not. var4d ) call da_uvprho_to_w_adj(grid)
   end if

   if ( cloud_cv_options == 1) then
      ! Partition of hydrometeor increments via warm rain process
      call da_moist_phys_adj(grid)
   end if

      if (use_ssmt1obs .or. use_ssmt2obs .or. use_gpspwobs .or. &
          use_gpsztdobs .or. use_gpsrefobs .or. use_gpsephobs .or. &
          use_ssmitbobs .or. use_ssmiretrievalobs) then

         if (use_ssmitbobs) call da_transform_xtotb_adj (grid)

         ! for PW
         call da_transform_xtotpw_adj (grid)

         ! GPS Refractivity:
         if (use_gpsrefobs .or. use_gpsztdobs .or. use_gpsephobs) then
            if (use_gpsztdobs) call da_transform_xtoztd_adj(grid)
            call da_transform_xtogpsref_adj (grid)
         end if

         if (use_ssmt1obs .or. use_ssmt2obs .or. &
             use_ssmitbobs .or. use_ssmiretrievalobs) then
            if (global) then
               call da_error(__FILE__,__LINE__, &
                  (/"grid%xb%speed is not available, see da_transfer_kmatoxb.inc"/))
            end if
            call da_transform_xtoseasfcwind_adj (grid)
         end if
      end if

      ! Now do something for surface variables
      if (sfc_assi_options == 2) then
         call da_transform_xtowtq_adj (grid)

      end if

      call da_pt_to_rho_adj (grid)

      if (var4d) then
#ifdef VAR4D
         grid%g_u_2 = 0.0
         grid%g_v_2 = 0.0
         grid%g_w_2 = 0.0
         grid%g_t_2 = 0.0
         grid%g_ph_2 = 0.0
         grid%g_p = 0.0
         grid%g_mu_2 = 0.0
         grid%g_moist = 0.0
         grid%g_rainnc = 0.0
         grid%g_rainncv = 0.0
         grid%g_rainc = 0.0
         grid%g_raincv = 0.0

         write(unit=filnam,fmt='(a2,i2.2)') 'af',nobwin

         if ( use_rainobs ) then
            if ( num_fgat_time > 1 .and. fgat_rain_flags(nobwin) ) then
!!!            We can not calculate the hourly rainfall in adjoint check, just ensure the adjoint 
!!!            algorithm is right mathmatically. so the amount of forcings doesn't matter
               grid%g_rainc(:,:) = grid%g_rainc(:,:) + a_hr_rainc (:,:,fgat_rain)
               grid%g_rainnc(:,:) = grid%g_rainnc(:,:) + a_hr_rainnc(:,:,fgat_rain)
               a_hr_rainc (:,:,fgat_rain) = 0.0
               a_hr_rainnc(:,:,fgat_rain) = 0.0
               fgat_rain = fgat_rain - 1
            endif
         endif

         call domain_clock_get( grid, current_timestr=timestr )
         call da_transfer_wrftltoxa_adj(grid, config_flags, filnam, timestr)
#endif
      end if

      if ( nobwin > 1 ) call domain_clockadvance (grid)
      call domain_clockprint(150, grid, 'DEBUG Adjoint Check:  get CurrTime from clock,')

   end do

   if ( num_fgat_time > 1 ) then
      call nl_get_time_step ( grid%id, time_step_seconds)
      call domain_clock_set (grid, time_step_seconds=time_step_seconds)
      call domain_clockprint(150, grid, 'get CurrTime from clock,')
   endif

   if (var4d) then
#ifdef VAR4D
      if (jcdfi_use) then

         do j = jms, jme
            do k = kms, kme
               do i = ims, ime
                  model_grid%jcdfi_u(i,k,j) = -1.0 * config_flags%jcdfi_penalty * model_grid%jcdfi_u(i,k,j) * &
                        grid%xb%grid_box_area(i,j)/whole_area*grid%xb%dnw(k)
                  model_grid%jcdfi_v(i,k,j) = -1.0 * config_flags%jcdfi_penalty * model_grid%jcdfi_v(i,k,j) * &
                        grid%xb%grid_box_area(i,j)/whole_area*grid%xb%dnw(k)
                  model_grid%jcdfi_t(i,k,j) = -1.0 * config_flags%jcdfi_penalty * model_grid%jcdfi_t(i,k,j) * &
                        (9.81/3.0)**2*grid%xb%grid_box_area(i,j)/whole_area*grid%xb%dnw(k)
                  model_grid%jcdfi_p(i,k,j) = -1.0 * config_flags%jcdfi_penalty * model_grid%jcdfi_p(i,k,j) * &
                        (1.0/300.)**2*grid%xb%grid_box_area(i,j)/whole_area*grid%xb%dnw(k)
               enddo
            enddo
         enddo

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

         call domain_clock_get( grid, start_timestr=timestr )
         call domain_clock_set( grid, current_timestr=timestr )
         call da_read_basicstates ( xbx, grid, config_flags, timestr )
      end if

      call da_zero_x(grid%xa)
      call da_transfer_xatowrftl_adj(grid, config_flags, 'gr01')

      if ( use_rainobs .and. num_fgat_time > 1 ) then
         deallocate (a_hr_rainc)
         deallocate (a_hr_rainnc)
      endif
#endif
   end if


   pertile_rhs = sum (grid%xa%u(ims:ime, jms:jme, kms:kme) * xa2_u(ims:ime, jms:jme, kms:kme)) &
      + sum (grid%xa%v(ims:ime, jms:jme, kms:kme) * xa2_v(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%w(ims:ime, jms:jme, kms:kme) * xa2_w(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%t(ims:ime, jms:jme, kms:kme) * xa2_t(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%p(ims:ime, jms:jme, kms:kme) * xa2_p(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%q(ims:ime, jms:jme, kms:kme) * xa2_q(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%rh(ims:ime, jms:jme, kms:kme)* xa2_rh(ims:ime, jms:jme, kms:kme))         &
      + sum (grid%xa%psfc(ims:ime, jms:jme) * xa2_psfc(ims:ime, jms:jme))
#ifdef VAR4D
   pertile_rhs = pertile_rhs &
      + sum (grid%x6a%u(ims:ime, jms:jme, kms:kme) * x6a2_u(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%v(ims:ime, jms:jme, kms:kme) * x6a2_v(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%w(ims:ime, jms:jme, kms:kme) * x6a2_w(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%t(ims:ime, jms:jme, kms:kme) * x6a2_t(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%p(ims:ime, jms:jme, kms:kme) * x6a2_p(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%q(ims:ime, jms:jme, kms:kme) * x6a2_q(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%rh(ims:ime, jms:jme, kms:kme)* x6a2_rh(ims:ime, jms:jme, kms:kme))       &
      + sum (grid%x6a%psfc(ims:ime, jms:jme) * x6a2_psfc(ims:ime, jms:jme))
#endif
   if ( cloud_cv_options >= 1 ) then
      pertile_rhs = pertile_rhs &
         + sum (grid%xa%qcw(ims:ime, jms:jme, kms:kme) * xa2_qcw(ims:ime, jms:jme, kms:kme))   &
         + sum (grid%xa%qrn(ims:ime, jms:jme, kms:kme) * xa2_qrn(ims:ime, jms:jme, kms:kme))
      if ( cloud_cv_options >= 2 ) then
         pertile_rhs = pertile_rhs &
            + sum (grid%xa%qci(ims:ime, jms:jme, kms:kme) * xa2_qci(ims:ime, jms:jme, kms:kme))&
            + sum (grid%xa%qsn(ims:ime, jms:jme, kms:kme) * xa2_qsn(ims:ime, jms:jme, kms:kme))&
            + sum (grid%xa%qgr(ims:ime, jms:jme, kms:kme) * xa2_qgr(ims:ime, jms:jme, kms:kme))
      end if
   end if
#ifdef VAR4D
   pertile_rhs = pertile_rhs &
      + sum (grid%x6a%qcw(ims:ime, jms:jme, kms:kme) * x6a2_qcw(ims:ime, jms:jme, kms:kme))    &
      + sum (grid%x6a%qci(ims:ime, jms:jme, kms:kme) * x6a2_qci(ims:ime, jms:jme, kms:kme))    &
      + sum (grid%x6a%qrn(ims:ime, jms:jme, kms:kme) * x6a2_qrn(ims:ime, jms:jme, kms:kme))    &
      + sum (grid%x6a%qsn(ims:ime, jms:jme, kms:kme) * x6a2_qsn(ims:ime, jms:jme, kms:kme))    &
      + sum (grid%x6a%qgr(ims:ime, jms:jme, kms:kme) * x6a2_qgr(ims:ime, jms:jme, kms:kme))
#endif


   !----------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjoint test equation:
   !----------------------------------------------------------------------
   
   partial_rhs = sum (grid%xa%u(its:ite, jts:jte, kts:kte) * xa2_u(its:ite, jts:jte, kts:kte)) &
      + sum (grid%xa%v(its:ite, jts:jte, kts:kte) * xa2_v(its:ite, jts:jte, kts:kte))          &
      + sum (grid%xa%w(its:ite, jts:jte, kts:kte+1) * xa2_w(its:ite, jts:jte, kts:kte+1))      &
      + sum (grid%xa%t(its:ite, jts:jte, kts:kte) * xa2_t(its:ite, jts:jte, kts:kte))          &
      + sum (grid%xa%p(its:ite, jts:jte, kts:kte) * xa2_p(its:ite, jts:jte, kts:kte))          &
      + sum (grid%xa%q(its:ite, jts:jte, kts:kte) * xa2_q(its:ite, jts:jte, kts:kte))          &
      + sum (grid%xa%rh(its:ite, jts:jte, kts:kte)* xa2_rh(its:ite, jts:jte, kts:kte))         &
      + sum (grid%xa%psfc(its:ite, jts:jte) * xa2_psfc(its:ite, jts:jte))
#ifdef VAR4D
   partial_rhs = partial_rhs &
      + sum (grid%x6a%u(its:ite, jts:jte, kts:kte) * x6a2_u(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%v(its:ite, jts:jte, kts:kte) * x6a2_v(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%w(its:ite, jts:jte, kts:kte+1) * x6a2_w(its:ite, jts:jte, kts:kte+1))    &
      + sum (grid%x6a%t(its:ite, jts:jte, kts:kte) * x6a2_t(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%p(its:ite, jts:jte, kts:kte) * x6a2_p(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%q(its:ite, jts:jte, kts:kte) * x6a2_q(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%rh(its:ite, jts:jte, kts:kte)* x6a2_rh(its:ite, jts:jte, kts:kte))       &
      + sum (grid%x6a%psfc(its:ite, jts:jte) * x6a2_psfc(its:ite, jts:jte)) 
#endif

   if ( cloud_cv_options >= 1 ) then
      partial_rhs = partial_rhs &
         + sum (grid%xa%qcw(its:ite, jts:jte, kts:kte) * xa2_qcw(its:ite, jts:jte, kts:kte))   &
         + sum (grid%xa%qrn(its:ite, jts:jte, kts:kte) * xa2_qrn(its:ite, jts:jte, kts:kte))
      if ( cloud_cv_options >= 2 ) then
         partial_rhs = partial_rhs &
            + sum (grid%xa%qci(its:ite, jts:jte, kts:kte) * xa2_qci(its:ite, jts:jte, kts:kte)) &
            + sum (grid%xa%qsn(its:ite, jts:jte, kts:kte) * xa2_qsn(its:ite, jts:jte, kts:kte)) &
            + sum (grid%xa%qgr(its:ite, jts:jte, kts:kte) * xa2_qgr(its:ite, jts:jte, kts:kte))
      end if
   end if
#ifdef VAR4D
   partial_rhs = partial_rhs &
      + sum (grid%x6a%qcw(its:ite, jts:jte, kts:kte) * x6a2_qcw(its:ite, jts:jte, kts:kte)) &
      + sum (grid%x6a%qci(its:ite, jts:jte, kts:kte) * x6a2_qci(its:ite, jts:jte, kts:kte)) &
      + sum (grid%x6a%qrn(its:ite, jts:jte, kts:kte) * x6a2_qrn(its:ite, jts:jte, kts:kte)) &
      + sum (grid%x6a%qsn(its:ite, jts:jte, kts:kte) * x6a2_qsn(its:ite, jts:jte, kts:kte)) &
      + sum (grid%x6a%qgr(its:ite, jts:jte, kts:kte) * x6a2_qgr(its:ite, jts:jte, kts:kte))
#endif

#ifdef A2C
    if( ite == ide ) then
print*,__FILE__,' contribution from ',ite+1,' col for U : ',sum (grid%xa%u(ite+1, jts:jte, kts:kte) * xa2_u(ite+1, jts:jte, kts:kte))
   partial_rhs = partial_rhs &
      + sum (grid%xa%u(ite+1, jts:jte, kts:kte) * xa2_u(ite+1, jts:jte, kts:kte))
   end if
    if( jte == jde ) then
print*,__FILE__,' contribution from ',jte+1,' row for V : ',sum(grid%xa%v(its:ite, jte+1, kts:kte) * xa2_v(its:ite, jte+1, kts:kte))
   partial_rhs = partial_rhs &
      + sum (grid%xa%v(its:ite, jte+1, kts:kte) * xa2_v(its:ite, jte+1, kts:kte))
   end if
#endif

   !----------------------------------------------------------------------
   !  [7.0] Print output:
   !----------------------------------------------------------------------
   write (unit=stdout, fmt='(A,1pe22.14)') ' Single Domain < y, y     > = ', pertile_lhs
   write (unit=stdout, fmt='(A,1pe22.14)') ' Single Domain < x, x_adj > = ', pertile_rhs

   adj_ttl_lhs = wrf_dm_sum_real (partial_lhs)
   adj_ttl_rhs = wrf_dm_sum_real (partial_rhs)
   
   if (rootproc) then
      write(unit=stdout, fmt='(/)')
      write (unit=stdout, fmt='(A,1pe22.14)') ' Whole Domain < y, y     > = ', adj_ttl_lhs
      write (unit=stdout, fmt='(A,1pe22.14)') ' Whole Domain < x, x_adj > = ', adj_ttl_rhs
   end if

   write (unit=stdout, fmt='(/a/)') 'da_check_xtoy_adjoint: Test Finished:'
   if (trace_use) call da_trace_exit("da_check_xtoy_adjoint")
   
end subroutine da_check_xtoy_adjoint


