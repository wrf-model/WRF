subroutine da_transform_xtoy_adj(grid, iv, jo_grad_y, jo_grad_x)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------
   
   implicit none
   
   type (domain),  intent(inout) :: grid
   type (iv_type), intent(inout) :: iv          ! obs. inc vector (o-b).
   type (y_type),  intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type),  intent(inout) :: jo_grad_x   ! grad_x(jo)

   if (trace_use) call da_trace_entry("da_transform_xtoy_adj")
  
   !--------------------------------------------------------------------------
   ! [1.0] observation operator y = H(x):
   !--------------------------------------------------------------------------
  
   if (iv%info(sound)%nlocal > 0) then
      call da_transform_xtoy_sound_adj     (iv, jo_grad_y, jo_grad_x) 
      call da_transform_xtoy_sonde_sfc_adj (grid, iv, jo_grad_y, jo_grad_x) 
   end if

   if (iv%info(synop)%nlocal    > 0) call da_transform_xtoy_synop_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(geoamv)%nlocal   > 0) call da_transform_xtoy_geoamv_adj   (iv, jo_grad_y, jo_grad_x)
   if (iv%info(polaramv)%nlocal > 0) call da_transform_xtoy_polaramv_adj (iv, jo_grad_y, jo_grad_x)   
   if (iv%info(airep)%nlocal    > 0) call da_transform_xtoy_airep_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv%info(metar)%nlocal    > 0) call da_transform_xtoy_metar_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(ships)%nlocal    > 0) call da_transform_xtoy_ships_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(gpspw)%nlocal    > 0) call da_transform_xtoy_gpspw_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(ssmi_tb)%nlocal  > 0) call da_transform_xtoy_ssmi_tb_adj  (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(ssmi_rv)%nlocal  > 0) call da_transform_xtoy_ssmi_rv_adj  (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(pilot)%nlocal    > 0) call da_transform_xtoy_pilot_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv%info(satem)%nlocal    > 0) call da_transform_xtoy_satem_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(ssmt1)%nlocal    > 0) call da_transform_xtoy_ssmt1_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv%info(ssmt2)%nlocal    > 0) call da_transform_xtoy_ssmt2_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv%info(qscat)%nlocal    > 0) call da_transform_xtoy_qscat_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv%info(profiler)%nlocal > 0) call da_transform_xtoy_profiler_adj (iv, jo_grad_y, jo_grad_x)
   if (iv%info(buoy)%nlocal     > 0) call da_transform_xtoy_buoy_adj     (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(gpsref)%nlocal   > 0) call da_transform_xtoy_gpsref_adj   (iv, jo_grad_y, jo_grad_x)
   if (iv%info(radar)%nlocal    > 0) call da_transform_xtoy_radar_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(bogus)%nlocal    > 0) call da_transform_xtoy_bogus_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(airsr)%nlocal    > 0) call da_transform_xtoy_airsr_adj    (iv, jo_grad_y, jo_grad_x)

   if (num_pseudo            > 0) call da_transform_xtoy_pseudo_adj   (iv, jo_grad_y, jo_grad_x)

   if (trace_use) call da_trace_exit("da_transform_xtoy_adj")

end subroutine da_transform_xtoy_adj


