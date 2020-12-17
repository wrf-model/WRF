subroutine da_calculate_grady(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Does part of the obs gradient operation   
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: re          ! Residual vector (O-A).
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   if (trace_use) call da_trace_entry("da_calculate_grady")

   !-------------------------------------------------------------------------
   ! [1.0] Compute components of Grad_y(Jo):
   !-------------------------------------------------------------------------

   if (iv%info(sound)%nlocal     > 0) call da_calculate_grady_sound    (iv, re, jo_grad_y)
   if (iv%info(sonde_sfc)%nlocal > 0) call da_calculate_grady_sonde_sfc(iv, re, jo_grad_y)
   if (iv%info(synop)%nlocal     > 0) call da_calculate_grady_synop    (iv, re, jo_grad_y)
   if (iv%info(geoamv)%nlocal    > 0) call da_calculate_grady_geoamv   (iv, re, jo_grad_y)
   if (iv%info(polaramv)%nlocal  > 0) call da_calculate_grady_polaramv (iv, re, jo_grad_y)
   if (iv%info(airep)%nlocal     > 0) call da_calculate_grady_airep    (iv, re, jo_grad_y)
   if (iv%info(pilot)%nlocal     > 0) call da_calculate_grady_pilot    (iv, re, jo_grad_y)
   if (iv%info(profiler)%nlocal  > 0) call da_calculate_grady_profiler (iv, re, jo_grad_y)
   if (iv%info(satem)%nlocal     > 0) call da_calculate_grady_satem    (iv, re, jo_grad_y)
   if (iv%info(metar)%nlocal     > 0) call da_calculate_grady_metar    (iv, re, jo_grad_y)
   if (iv%info(ships)%nlocal     > 0) call da_calculate_grady_ships    (iv, re, jo_grad_y)
   if (iv%info(buoy)%nlocal      > 0) call da_calculate_grady_buoy     (iv, re, jo_grad_y)
   if (iv%info(gpspw)%nlocal     > 0) call da_calculate_grady_gpspw    (iv, re, jo_grad_y)
   if (iv%info(gpsref)%nlocal    > 0) call da_calculate_grady_gpsref   (iv, re, jo_grad_y)
   if (iv%info(gpseph)%nlocal    > 0) call da_calculate_grady_gpseph   (iv, re, jo_grad_y)
   if (iv%info(ssmi_tb)%nlocal   > 0) call da_calculate_grady_ssmi_tb  (iv, re, jo_grad_y) 
   if (iv%info(ssmi_rv)%nlocal   > 0) call da_calculate_grady_ssmi_rv  (iv, re, jo_grad_y) 
   if (iv%info(ssmt1)%nlocal     > 0) call da_calculate_grady_ssmt1    (iv, re, jo_grad_y)
   if (iv%info(ssmt2)%nlocal     > 0) call da_calculate_grady_ssmt2    (iv, re, jo_grad_y)
   if (iv%info(pseudo)%nlocal    > 0) call da_calculate_grady_pseudo   (iv, re, jo_grad_y)
   if (iv%info(bogus)%nlocal     > 0) call da_calculate_grady_bogus    (iv, re, jo_grad_y)  
   if (iv%info(qscat)%nlocal     > 0) call da_calculate_grady_qscat    (iv, re, jo_grad_y)
   if (iv%info(radar)%nlocal     > 0) call da_calculate_grady_radar    (iv, re, jo_grad_y)
   if (iv%info(mtgirs)%nlocal    > 0) call da_calculate_grady_mtgirs   (iv, re, jo_grad_y)
   if (iv%info(tamdar)%nlocal    > 0) call da_calculate_grady_tamdar   (iv, re, jo_grad_y)
   if (iv%info(tamdar_sfc)%nlocal> 0) call da_calculate_grady_tamdar_sfc(iv, re, jo_grad_y)
   if (iv%info(rain)%nlocal      > 0) call da_calculate_grady_rain     (iv, re, jo_grad_y)

#if defined(CRTM) || defined(RTTOV)
   if (iv%num_inst               > 0) call da_calculate_grady_rad      (iv, re, jo_grad_y)
#endif
   if (iv%info(airsr)%nlocal     > 0) call da_calculate_grady_airsr    (iv, re, jo_grad_y)

   if (trace_use) call da_trace_exit("da_calculate_grady")

end subroutine da_calculate_grady


