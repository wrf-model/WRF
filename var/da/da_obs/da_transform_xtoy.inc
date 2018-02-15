subroutine da_transform_xtoy(cv_size, cv, grid, iv, y)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   !-------------------------------------------------------------------------

   implicit none
   
   integer, intent(in)           :: cv_size         ! Size of cv array.
   real, intent(in)              :: cv(1:cv_size)   ! control variables.
   type (domain),  intent(inout) :: grid
   type (iv_type), intent(inout) :: iv       ! obs. increment vector (o-b).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   if (trace_use) call da_trace_entry("da_transform_xtoy")
   
   !--------------------------------------------------------------------------
   ! [1.0] observation operator y = H(x):
   !--------------------------------------------------------------------------
  
   if (iv%info(sound)%nlocal          > 0) call da_transform_xtoy_sound    (grid, iv, y)
   if (iv%info(sonde_sfc)%nlocal      > 0) call da_transform_xtoy_sonde_sfc(grid, iv, y)
   if (iv%info(mtgirs)%nlocal         > 0) call da_transform_xtoy_mtgirs   (grid, iv, y)
   if (iv%info(tamdar)%nlocal         > 0) call da_transform_xtoy_tamdar   (grid, iv, y)
   if (iv%info(tamdar_sfc)%nlocal     > 0) call da_transform_xtoy_tamdar_sfc(grid, iv, y)
   if (iv%info(synop)%nlocal          > 0) call da_transform_xtoy_synop    (grid, iv, y)
   if (iv%info(geoamv)%nlocal         > 0) call da_transform_xtoy_geoamv   (grid, iv, y)
   if (iv%info(polaramv)%nlocal       > 0) call da_transform_xtoy_polaramv (grid, iv, y)
   if (iv%info(airep)%nlocal          > 0) call da_transform_xtoy_airep    (grid, iv, y)
   if (iv%info(metar)%nlocal          > 0) call da_transform_xtoy_metar    (grid, iv, y)
   if (iv%info(ships)%nlocal          > 0) call da_transform_xtoy_ships    (grid, iv, y)
   if (iv%info(gpspw)%nlocal          > 0) then
      if (use_gpspwobs) then
         call da_transform_xtoy_gpspw    (grid, iv, y)
      else if (use_gpsztdobs) then
         call da_transform_xtoy_gpsztd   (grid, iv, y)
      endif
   end if
   if (iv%info(ssmi_tb)%nlocal        > 0) call da_transform_xtoy_ssmi_tb  (grid, iv, y)
   if (iv%info(ssmi_rv)%nlocal        > 0) call da_transform_xtoy_ssmi_rv  (grid, iv, y)
   if (iv%info(pilot)%nlocal          > 0) call da_transform_xtoy_pilot    (grid, iv, y)
   if (iv%info(satem)%nlocal          > 0) call da_transform_xtoy_satem    (grid, iv, y)
   if (iv%info(ssmt1)%nlocal          > 0) call da_transform_xtoy_ssmt1    (grid, iv, y)
   if (iv%info(ssmt2)%nlocal          > 0) call da_transform_xtoy_ssmt2    (grid, iv, y)
   if (iv%info(qscat)%nlocal          > 0) call da_transform_xtoy_qscat    (grid, iv, y)
   if (iv%info(profiler)%nlocal       > 0) call da_transform_xtoy_profiler (grid, iv, y)
   if (iv%info(buoy)%nlocal           > 0) call da_transform_xtoy_buoy     (grid, iv, y)
   if (iv%info(gpsref)%nlocal         > 0) call da_transform_xtoy_gpsref   (grid, iv, y)
   if (iv%info(gpseph)%nlocal         > 0) call da_transform_xtoy_gpseph   (iv, y)
   if (iv%info(radar)%nlocal          > 0) call da_transform_xtoy_radar    (grid, iv, y)
   if (iv%info(bogus)%nlocal          > 0) call da_transform_xtoy_bogus    (grid, iv, y)
   if (iv%info(airsr)%nlocal          > 0) call da_transform_xtoy_airsr    (grid, iv, y)
   if (iv%info(pseudo)%nlocal         > 0) call da_transform_xtoy_pseudo   (grid, iv, y)

   if (use_rad) then
      if (rtm_option == rtm_option_rttov) then
#ifdef RTTOV
         call da_transform_xtoy_rttov (grid, iv, y)
#endif
      elseif (rtm_option == rtm_option_crtm) then
#ifdef CRTM
         !if (use_crtm_kmatrix) then
         !   call da_transform_xtoy_crtmk (grid, iv, y)
         !else if (use_crtm_kmatrix_fast) then
         !   call da_transform_xtoy_crtmk_f (grid, iv, y)
         !else
            call da_transform_xtoy_crtm (cv_size, cv, grid, iv, y)
         !end if
#endif
       else
          call da_warning(__FILE__,__LINE__,(/"Unknown radiative transfer model"/))
       end if
   end if

   if (trace_use) call da_trace_exit("da_transform_xtoy")

end subroutine da_transform_xtoy


