subroutine da_calculate_residual(iv, y, re)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residuals
   !-----------------------------------------------------------------------

   implicit none
      
   type (iv_type), intent(inout)     :: iv     ! Innovation vector (O-B).
   type (y_type),  intent(in)        :: y      ! y = H (xa)
   type (y_type),  intent(inout)     :: re     ! Residual (O-A).

   integer    :: np_available, np_obs_used, np_missing, np_bad_data 

   if (trace_use) call da_trace_entry("da_calculate_residual")
      
   np_available = 0
   np_obs_used  = 0
   np_missing   = 0
   np_bad_data  = 0

   !-------------------------------------------------------------------------
   ! [1.0] (O-A) = (O-B) - H x~:
   !-------------------------------------------------------------------------

   if (iv%info(sound)%nlocal > 0) &
      call da_residual_sound(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(sonde_sfc)%nlocal > 0) &
      call da_residual_sonde_sfc(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(mtgirs)%nlocal > 0) & 
      call da_residual_mtgirs(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(tamdar)%nlocal > 0)  &   
      call da_residual_tamdar(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(tamdar_sfc)%nlocal > 0)  &   
      call da_residual_tamdar_sfc(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(synop)%nlocal > 0) &
      call da_residual_synop(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(geoamv)%nlocal > 0) &
      call da_residual_geoamv(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(polaramv)%nlocal > 0) &
      call da_residual_polaramv(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(airep)%nlocal > 0) &
      call da_residual_airep(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(metar)%nlocal > 0) &
      call da_residual_metar(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(ships)%nlocal > 0) &
      call da_residual_ships(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(gpspw)%nlocal > 0) &
      call da_residual_gpspw(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(gpsref)%nlocal > 0) &
      call da_residual_gpsref(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(gpseph)%nlocal > 0) &
      call da_residual_gpseph(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(ssmi_tb)%nlocal > 0) &
      call da_residual_ssmi_tb(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(ssmi_rv)%nlocal > 0) &
      call da_residual_ssmi_rv(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if ( iv%info(ssmt2)%nlocal > 0) &
      call da_residual_ssmt1(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(ssmt2)%nlocal > 0) &
      call da_residual_ssmt2(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(pilot)%nlocal > 0) &
      call da_residual_pilot(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(bogus)%nlocal > 0) &
      call da_residual_bogus(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(satem)%nlocal > 0) &
      call da_residual_satem(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if ( pseudo_uvtpq ) &
      call da_residual_pseudo(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(qscat)%nlocal > 0) &
      call da_residual_qscat(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(radar)%nlocal > 0) &
      call da_residual_radar(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(profiler)%nlocal > 0) &
      call da_residual_profiler(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (iv%info(buoy)%nlocal > 0) &
      call da_residual_buoy(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

    if (iv%info(rain)%nlocal > 0) &
      call da_residual_rain(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

#if defined(RTTOV) || defined(CRTM)
   if (iv%num_inst > 0) &
      call da_residual_rad(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)
#endif

   if (iv%info(airsr)%nlocal > 0) &
      call da_residual_airsr(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   if (trace_use) call da_trace_exit("da_calculate_residual")

end subroutine da_calculate_residual


