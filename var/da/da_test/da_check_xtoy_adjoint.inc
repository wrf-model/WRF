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
   integer :: nobwin, ndynopt, i, j, k, jl_start, jl_end
   character(len=4) :: filnam
   character(len=256) :: timestr
   integer :: wrf_done_unit, time_step_seconds
   type(x_type) :: shuffle

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

   xa2_qcw(ims:ime, jms:jme, kms:kme) = grid%xa%qcw(ims:ime, jms:jme, kms:kme)
   xa2_qci(ims:ime, jms:jme, kms:kme) = grid%xa%qci(ims:ime, jms:jme, kms:kme)
   xa2_qrn(ims:ime, jms:jme, kms:kme) = grid%xa%qrn(ims:ime, jms:jme, kms:kme)
   xa2_qsn(ims:ime, jms:jme, kms:kme) = grid%xa%qsn(ims:ime, jms:jme, kms:kme)
   xa2_qgr(ims:ime, jms:jme, kms:kme) = grid%xa%qgr(ims:ime, jms:jme, kms:kme)

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
         call da_get_2nd_firstguess ( grid )
         call da_setup_firstguess( xbx, grid, config_flags)

         shuffle = grid%xa
         jl_start    = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + 1
         jl_end      = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + be%cv%size_jl
         grid%xa  = grid%x6a
         call da_transform_vtox(grid, be%cv%size_jl, xbx, be, grid%ep, cv(jl_start:jl_end), grid%vv6, grid%vp6)
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
         call da_read_basicstates ( xbx, grid, config_flags, timestr )

      endif

      call da_tl_model

      if (jcdfi_use) then

         pertile_lhs = pertile_lhs &
                       + (1.0)/grid%jcdfi_error_wind**2 * sum( model_grid%jcdfi_u(ims:ime, kms:kme, jms:jme)*model_grid%jcdfi_u(ims:ime, kms:kme, jms:jme) ) &  
                       + (1.0)/grid%jcdfi_error_wind**2 * sum( model_grid%jcdfi_v(ims:ime, kms:kme, jms:jme)*model_grid%jcdfi_v(ims:ime, kms:kme, jms:jme) ) &  
                       + (1.0)/grid%jcdfi_error_t**2 * sum( model_grid%jcdfi_t(ims:ime, kms:kme, jms:jme)*model_grid%jcdfi_t(ims:ime, kms:kme, jms:jme) ) &  
                       + (1.0)/grid%jcdfi_error_mu**2 * sum( model_grid%jcdfi_mu(ims:ime, jms:jme)*model_grid%jcdfi_mu(ims:ime, jms:jme) ) 

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
          use_gpsztdobs .or. use_gpsrefobs .or.                 &
          use_ssmitbobs .or. use_ssmiretrievalobs) then

         ! Now do something for PW
         call da_transform_xtotpw(grid)

         ! GPS Refractivity:
         if (use_gpsrefobs .or. use_gpsztdobs) then
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
      call da_uvprho_to_w_lin(grid)

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

   if ( (use_radarobs .and. use_radar_rf) .or. (use_rad .and. crtm_cloud) ) then
    if ( cloud_cv_options == 1 )then
      print*,'Options: set CLOUD_CV and cloud_cv_options to 2 or 3 !'
      ! Partition of hydrometeor increments via warm rain process
      call da_moist_phys_lin(grid)
    end if
   end if

      !----------------------------------------------------------------------
      ! [2.0] Perform y = Hx transform:
      !----------------------------------------------------------------------
      call da_transform_xtoy (cv_size, cv, grid, iv, y)


            

      !----------------------------------------------------------------------
      ! [3.0] Calculate LHS of adjoint test equation and
      !       Rescale input to adjoint routine :
      !----------------------------------------------------------------------

      if (iv%info(sound)%nlocal > 0) then
         call da_check_xtoy_adjoint_sound     (iv, y, partial_lhs, pertile_lhs)
         call da_check_xtoy_adjoint_sonde_sfc (iv, y, partial_lhs, pertile_lhs)
      end if
      if (iv%info(mtgirs)%nlocal   > 0) call da_check_xtoy_adjoint_mtgirs   (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(tamdar)%nlocal   > 0) call da_check_xtoy_adjoint_tamdar   (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(tamdar)%nlocal   > 0) call da_check_xtoy_adjoint_tamdar_sfc(iv, y, partial_lhs, pertile_lhs)
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
      if (iv%info(ssmi_tb)%nlocal  > 0) call da_check_xtoy_adjoint_ssmi_tb  (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(ssmi_rv)%nlocal  > 0) call da_check_xtoy_adjoint_ssmi_rv  (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(ssmt2)%nlocal    > 0) call da_check_xtoy_adjoint_ssmt1    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(ssmt2)%nlocal    > 0) call da_check_xtoy_adjoint_ssmt2    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(qscat)%nlocal    > 0) call da_check_xtoy_adjoint_qscat    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(profiler)%nlocal > 0) call da_check_xtoy_adjoint_profiler (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(buoy)%nlocal     > 0) call da_check_xtoy_adjoint_buoy     (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(bogus)%nlocal    > 0) call da_check_xtoy_adjoint_bogus    (iv, y, partial_lhs, pertile_lhs)
      if (iv%num_inst     > 0) call da_check_xtoy_adjoint_rad      (iv, y, partial_lhs, pertile_lhs)

      !----------------------------------------------------------------------
      ! [5.0] Perform adjoint operation:
      !----------------------------------------------------------------------
      call da_zero_x (grid%xa)


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

      call da_uvprho_to_w_adj(grid)
   end if

   if ( (use_radarobs .and. use_radar_rf) .or. (use_rad .and. crtm_cloud) ) then
     if ( cloud_cv_options == 1) then
      ! Partition of hydrometeor increments via warm rain process
      call da_moist_phys_adj(grid)
     end if
   end if

      if (use_ssmt1obs .or. use_ssmt2obs .or. use_gpspwobs .or. &
          use_gpsztdobs .or. use_gpsrefobs .or.                 &
          use_ssmitbobs .or. use_ssmiretrievalobs) then

         if (use_ssmitbobs) call da_transform_xtotb_adj (grid)

         ! for PW
         call da_transform_xtotpw_adj (grid)

         ! GPS Refractivity:
         if (use_gpsrefobs .or. use_gpsztdobs) then
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
         grid%g_mu_2 = 0.0
         grid%g_moist = 0.0

         write(unit=filnam,fmt='(a2,i2.2)') 'af',nobwin

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
         model_grid%jcdfi_u = (1.0)/grid%jcdfi_error_wind**2 * model_grid%jcdfi_u
         model_grid%jcdfi_v = (1.0)/grid%jcdfi_error_wind**2 * model_grid%jcdfi_v
         model_grid%jcdfi_t = (1.0)/grid%jcdfi_error_t**2 * model_grid%jcdfi_t
         model_grid%jcdfi_mu = (1.0)/grid%jcdfi_error_mu**2 * model_grid%jcdfi_mu
      endif

      ! for memory io, we need to up-side-down the adjoint forcing linked list generated in previous step.
      call upsidedown_ad_forcing

      call da_ad_model

      call da_transfer_xatowrftl_adj(grid, config_flags, 'gr01')

      call da_zero_x(grid%x6a)
      if (var4d_lbc) then
         !call domain_clock_get( grid, stop_timestr=timestr )
         !call da_read_basicstates ( xbx, grid, config_flags, timestr )
         call da_get_2nd_firstguess ( grid )
         call da_setup_firstguess( xbx, grid, config_flags)

         call da_transfer_xatowrftl_adj_lbc(grid, config_flags, 'gr01')

         call domain_clock_get( grid, start_timestr=timestr )
         call da_read_basicstates ( xbx, grid, config_flags, timestr )
      end if
#endif
   end if


   pertile_rhs = sum (grid%xa%u(ims:ime, jms:jme, kms:kme) * xa2_u(ims:ime, jms:jme, kms:kme)) &
      + sum (grid%xa%v(ims:ime, jms:jme, kms:kme) * xa2_v(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%w(ims:ime, jms:jme, kms:kme) * xa2_w(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%t(ims:ime, jms:jme, kms:kme) * xa2_t(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%p(ims:ime, jms:jme, kms:kme) * xa2_p(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%q(ims:ime, jms:jme, kms:kme) * xa2_q(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%rh(ims:ime, jms:jme, kms:kme)* xa2_rh(ims:ime, jms:jme, kms:kme))         &
      + sum (grid%xa%psfc(ims:ime, jms:jme) * xa2_psfc(ims:ime, jms:jme))                      &
      + sum (grid%x6a%u(ims:ime, jms:jme, kms:kme) * x6a2_u(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%v(ims:ime, jms:jme, kms:kme) * x6a2_v(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%w(ims:ime, jms:jme, kms:kme) * x6a2_w(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%t(ims:ime, jms:jme, kms:kme) * x6a2_t(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%p(ims:ime, jms:jme, kms:kme) * x6a2_p(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%q(ims:ime, jms:jme, kms:kme) * x6a2_q(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%rh(ims:ime, jms:jme, kms:kme)* x6a2_rh(ims:ime, jms:jme, kms:kme))       &
      + sum (grid%x6a%psfc(ims:ime, jms:jme) * x6a2_psfc(ims:ime, jms:jme))
   pertile_rhs = pertile_rhs &
      + sum (grid%xa%qcw(ims:ime, jms:jme, kms:kme) * xa2_qcw(ims:ime, jms:jme, kms:kme))      &
      + sum (grid%xa%qci(ims:ime, jms:jme, kms:kme) * xa2_qci(ims:ime, jms:jme, kms:kme))      &
      + sum (grid%xa%qrn(ims:ime, jms:jme, kms:kme) * xa2_qrn(ims:ime, jms:jme, kms:kme))      &
      + sum (grid%xa%qsn(ims:ime, jms:jme, kms:kme) * xa2_qsn(ims:ime, jms:jme, kms:kme))      &
      + sum (grid%xa%qgr(ims:ime, jms:jme, kms:kme) * xa2_qgr(ims:ime, jms:jme, kms:kme))      &
      + sum (grid%x6a%qcw(ims:ime, jms:jme, kms:kme) * x6a2_qcw(ims:ime, jms:jme, kms:kme))    &
      + sum (grid%x6a%qci(ims:ime, jms:jme, kms:kme) * x6a2_qci(ims:ime, jms:jme, kms:kme))    &
      + sum (grid%x6a%qrn(ims:ime, jms:jme, kms:kme) * x6a2_qrn(ims:ime, jms:jme, kms:kme))    &
      + sum (grid%x6a%qsn(ims:ime, jms:jme, kms:kme) * x6a2_qsn(ims:ime, jms:jme, kms:kme))    &
      + sum (grid%x6a%qgr(ims:ime, jms:jme, kms:kme) * x6a2_qgr(ims:ime, jms:jme, kms:kme))


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
      + sum (grid%xa%psfc(its:ite, jts:jte) * xa2_psfc(its:ite, jts:jte))                      &
      + sum (grid%x6a%u(its:ite, jts:jte, kts:kte) * x6a2_u(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%v(its:ite, jts:jte, kts:kte) * x6a2_v(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%w(its:ite, jts:jte, kts:kte+1) * x6a2_w(its:ite, jts:jte, kts:kte+1))    &
      + sum (grid%x6a%t(its:ite, jts:jte, kts:kte) * x6a2_t(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%p(its:ite, jts:jte, kts:kte) * x6a2_p(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%q(its:ite, jts:jte, kts:kte) * x6a2_q(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%rh(its:ite, jts:jte, kts:kte)* x6a2_rh(its:ite, jts:jte, kts:kte))       &
      + sum (grid%x6a%psfc(its:ite, jts:jte) * x6a2_psfc(its:ite, jts:jte)) 

   partial_rhs = partial_rhs &
      + sum (grid%xa%qcw(its:ite, jts:jte, kts:kte) * xa2_qcw(its:ite, jts:jte, kts:kte))   &
      + sum (grid%xa%qci(its:ite, jts:jte, kts:kte) * xa2_qci(its:ite, jts:jte, kts:kte))   & 
      + sum (grid%xa%qrn(its:ite, jts:jte, kts:kte) * xa2_qrn(its:ite, jts:jte, kts:kte))   & 
      + sum (grid%xa%qsn(its:ite, jts:jte, kts:kte) * xa2_qsn(its:ite, jts:jte, kts:kte))   & 
      + sum (grid%xa%qgr(its:ite, jts:jte, kts:kte) * xa2_qgr(its:ite, jts:jte, kts:kte))   & 
      + sum (grid%x6a%qcw(its:ite, jts:jte, kts:kte) * x6a2_qcw(its:ite, jts:jte, kts:kte)) &
      + sum (grid%x6a%qci(its:ite, jts:jte, kts:kte) * x6a2_qci(its:ite, jts:jte, kts:kte)) &
      + sum (grid%x6a%qrn(its:ite, jts:jte, kts:kte) * x6a2_qrn(its:ite, jts:jte, kts:kte)) &
      + sum (grid%x6a%qsn(its:ite, jts:jte, kts:kte) * x6a2_qsn(its:ite, jts:jte, kts:kte)) &
      + sum (grid%x6a%qgr(its:ite, jts:jte, kts:kte) * x6a2_qgr(its:ite, jts:jte, kts:kte))

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


