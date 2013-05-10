subroutine da_transform_xtoy_adj(cv_size, cv, grid, iv, jo_grad_y, jo_grad_x)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !--------------------------------------------------------------------------
   
   implicit none
   
   integer, intent(in)           :: cv_size         ! Size of cv array.
   real, intent(inout)           :: cv(1:cv_size)   ! control variables.
   type (domain),  intent(inout) :: grid
   type (iv_type), intent(inout) :: iv          ! obs. inc vector (o-b).
   type (y_type),  intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type),  intent(inout) :: jo_grad_x   ! grad_x(jo)

   if (trace_use) call da_trace_entry("da_transform_xtoy_adj")
  
   !--------------------------------------------------------------------------
   ! [1.0] observation operator y = H(x):
   !--------------------------------------------------------------------------
  
   if (iv%info(sound)%nlocal > 0) then
      call da_transform_xtoy_sound_adj     (grid, iv, jo_grad_y, jo_grad_x) 
      call da_transform_xtoy_sonde_sfc_adj (grid, iv, jo_grad_y, jo_grad_x) 
   end if
   if (iv%info(mtgirs)%nlocal   > 0) call da_transform_xtoy_mtgirs_adj   (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(tamdar)%nlocal   > 0) call da_transform_xtoy_tamdar_adj   (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(tamdar)%nlocal   > 0) call da_transform_xtoy_tamdar_sfc_adj(grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(synop)%nlocal    > 0) call da_transform_xtoy_synop_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(geoamv)%nlocal   > 0) call da_transform_xtoy_geoamv_adj   (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(polaramv)%nlocal > 0) call da_transform_xtoy_polaramv_adj (grid, iv, jo_grad_y, jo_grad_x)   
   if (iv%info(airep)%nlocal    > 0) call da_transform_xtoy_airep_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(metar)%nlocal    > 0) call da_transform_xtoy_metar_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(ships)%nlocal    > 0) call da_transform_xtoy_ships_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(gpspw)%nlocal    > 0) then
     if (use_gpspwObs) then
       call da_transform_xtoy_gpspw_adj    (grid, iv, jo_grad_y, jo_grad_x)
     else if (use_gpsztdObs) then
       call da_transform_xtoy_gpsztd_Adj   (grid, iv, jo_grad_y, jo_grad_x)
     endif
   end if
   if (iv%info(ssmi_tb)%nlocal  > 0) call da_transform_xtoy_ssmi_tb_adj  (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(ssmi_rv)%nlocal  > 0) call da_transform_xtoy_ssmi_rv_adj  (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(pilot)%nlocal    > 0) call da_transform_xtoy_pilot_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(satem)%nlocal    > 0) call da_transform_xtoy_satem_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(ssmt1)%nlocal    > 0) call da_transform_xtoy_ssmt1_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv%info(ssmt2)%nlocal    > 0) call da_transform_xtoy_ssmt2_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv%info(qscat)%nlocal    > 0) call da_transform_xtoy_qscat_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(profiler)%nlocal > 0) call da_transform_xtoy_profiler_adj (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(buoy)%nlocal     > 0) call da_transform_xtoy_buoy_adj     (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(gpsref)%nlocal   > 0) call da_transform_xtoy_gpsref_adj   (iv, jo_grad_y, jo_grad_x)
   if (iv%info(radar)%nlocal    > 0) call da_transform_xtoy_radar_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(bogus)%nlocal    > 0) call da_transform_xtoy_bogus_adj    (grid, iv, jo_grad_y, jo_grad_x)
   if (iv%info(airsr)%nlocal    > 0) call da_transform_xtoy_airsr_adj    (iv, jo_grad_y, jo_grad_x)
   if (iv%info(pseudo)%nlocal   > 0) call da_transform_xtoy_pseudo_adj   (iv, jo_grad_y, jo_grad_x)
#if defined(CRTM) || defined(RTTOV)
   if (use_rad) then
      if (rtm_option == rtm_option_rttov) then
#ifdef RTTOV
         call da_transform_xtoy_rttov_adj (iv, jo_grad_y, jo_grad_x)
#endif
      elseif (rtm_option == rtm_option_crtm) then
#ifdef CRTM
         call da_transform_xtoy_crtm_adj (cv_size, cv, iv, jo_grad_y, jo_grad_x)
#endif
      else
         call da_warning(__FILE__,__LINE__,(/"Unknown radiative transfer model"/))
      end if
   end if
#endif

   if (trace_use) call da_trace_exit("da_transform_xtoy_adj")

end subroutine da_transform_xtoy_adj


