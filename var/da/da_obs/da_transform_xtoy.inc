subroutine da_transform_xtoy(grid, iv, y)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   !-------------------------------------------------------------------------

   implicit none
   
   type (domain),  intent(inout) :: grid
   type (iv_type), intent(inout) :: iv       ! obs. increment vector (o-b).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   if (trace_use) call da_trace_entry("da_transform_xtoy")
   
   !--------------------------------------------------------------------------
   ! [1.0] observation operator y = H(x):
   !--------------------------------------------------------------------------
  
   if (iv%info(sound)%nlocal > 0) then
      call da_transform_xtoy_sound (grid, iv, y) 
      call da_transform_xtoy_sonde_sfc (grid, iv, y) 
   end if

   if (iv%info(synop)%nlocal          > 0) call da_transform_xtoy_synop    (grid, iv, y)
   if (iv%info(geoamv)%nlocal         > 0) call da_transform_xtoy_geoamv   (grid, iv, y)
   if (iv%info(polaramv)%nlocal       > 0) call da_transform_xtoy_polaramv (grid, iv, y)
   if (iv%info(airep)%nlocal          > 0) call da_transform_xtoy_airep    (grid, iv, y)
   if (iv%info(metar)%nlocal          > 0) call da_transform_xtoy_metar    (grid, iv, y)
   if (iv%info(ships)%nlocal          > 0) call da_transform_xtoy_ships    (grid, iv, y)
   if (iv%info(gpspw)%nlocal          > 0) call da_transform_xtoy_gpspw    (grid, iv, y)
   if (iv%info(ssmi_tb)%nlocal        > 0) call da_transform_xtoy_ssmi_tb  (grid, iv, y)
   if (iv%info(ssmi_rv)%nlocal > 0) call da_transform_xtoy_ssmi_rv  (grid, iv, y)
   if (iv%info(pilot)%nlocal          > 0) call da_transform_xtoy_pilot    (grid, iv, y)
   if (iv%info(satem)%nlocal          > 0) call da_transform_xtoy_satem    (grid, iv, y)
   if (iv%info(ssmt1)%nlocal          > 0) call da_transform_xtoy_ssmt1    (grid, iv, y)
   if (iv%info(ssmt2)%nlocal          > 0) call da_transform_xtoy_ssmt2    (grid, iv, y)
   if (iv%info(qscat)%nlocal          > 0) call da_transform_xtoy_qscat    (grid, iv, y)
   if (iv%info(profiler)%nlocal       > 0) call da_transform_xtoy_profiler (grid, iv, y)
   if (iv%info(buoy)%nlocal           > 0) call da_transform_xtoy_buoy     (grid, iv, y)
   if (iv%info(gpsref)%nlocal         > 0) call da_transform_xtoy_gpsref   (grid, iv, y)
   if (iv%info(radar)%nlocal          > 0) call da_transform_xtoy_radar    (grid, iv, y)
   if (iv%info(bogus)%nlocal          > 0) call da_transform_xtoy_bogus    (grid, iv, y)
   if (iv%info(airsr)%nlocal          > 0) call da_transform_xtoy_airsr    (grid, iv, y)

   if (num_pseudo              > 0) call da_transform_xtoy_pseudo   (grid, iv, y)

   if (trace_use) call da_trace_exit("da_transform_xtoy")

end subroutine da_transform_xtoy


