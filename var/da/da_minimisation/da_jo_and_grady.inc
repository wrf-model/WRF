subroutine da_jo_and_grady(iv, re, jot, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)   :: iv          ! Innovation vector (O-B).
   type (y_type),  intent(in)   :: re          ! Residual vector (O-A).
   real,           intent(out)  :: jot         ! Obs cost function.
   type (jo_type), intent(out)  :: jo          ! Obs cost function.
   type (y_type),  intent(out)  :: jo_grad_y   ! Grad_y(Jo)


   real    :: jo_sound, jo_sonde_sfc,jo_synop, jo_geoamv, jo_polaramv, &
              jo_airep, jo_pilot, jo_satem, &
              jo_metar, jo_ships, jo_gpspw, &
              jo_ssmi_tb, jo_ssmi_rv, jo_ssmt1, jo_ssmt2, &
              jo_pseudo, jo_qscat, jo_buoy, &
              jo_profiler, jo_radar, jo_gpsref, jo_gpseph, jo_bogus, jo_rain, &
              jo_radiance, jo_airsr, jo_mtgirs, jo_tamdar, jo_tamdar_sfc
   integer :: i,k

   if (trace_use) call da_trace_entry("da_jo_and_grady")

   !-------------------------------------------------------------------------
   ! [1.0] Compute components of Grad_y(Jo):
   !-------------------------------------------------------------------------


   if (iv%info(sound)%nlocal > 0) then
      call da_jo_and_grady_sound(iv, re, jo, jo_grad_y)
      jo_sound = jo%sound_u + jo%sound_v + jo%sound_t + jo%sound_q

      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            ' jo_sound          ', jo_sound, &
            '   jo%sound_u      ', jo%sound_u, &
            '   jo%sound_v      ', jo%sound_v, &
            '   jo%sound_t      ', jo%sound_t, &
            '   jo%sound_q      ', jo%sound_q
      end if

   else
      jo % sound_u = 0.0
      jo % sound_v = 0.0
      jo % sound_t = 0.0
      jo % sound_q = 0.0
      jo_sound     = 0.0
   end if

   if (iv%info(sonde_sfc)%nlocal > 0) then
      call da_jo_and_grady_sonde_sfc(iv, re, jo, jo_grad_y)
      jo_sonde_sfc = jo%sonde_sfc_u + jo%sonde_sfc_v + jo%sonde_sfc_t + &
         jo%sonde_sfc_q + jo%sonde_sfc_p

      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            ' jo_sonde_sfc      ', jo_sonde_sfc,     &
            '   jo%sonde_sfc_u  ', jo%sonde_sfc_u, &
            '   jo%sonde_sfc_v  ', jo%sonde_sfc_v, &
            '   jo%sonde_sfc_t  ', jo%sonde_sfc_t, &
            '   jo%sonde_sfc_p  ', jo%sonde_sfc_p, &
            '   jo%sonde_sfc_q  ', jo%sonde_sfc_q
      end if
   else
      jo % sonde_sfc_u = 0.0
      jo % sonde_sfc_v = 0.0
      jo % sonde_sfc_t = 0.0
      jo % sonde_sfc_p = 0.0
      jo % sonde_sfc_q = 0.0
      jo_sonde_sfc = 0.0
   end if



   if (iv%info(mtgirs)%nlocal > 0) then
      call da_jo_and_grady_mtgirs(iv, re, jo, jo_grad_y)
      jo_mtgirs = jo%mtgirs_u + jo%mtgirs_v + jo%mtgirs_t + jo%mtgirs_q

      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            ' jo_mtgirs          ', jo_mtgirs, &
            '   jo%mtgirs_u      ', jo%mtgirs_u, &
            '   jo%mtgirs_v      ', jo%mtgirs_v, &
            '   jo%mtgirs_t      ', jo%mtgirs_t, &
            '   jo%mtgirs_q      ', jo%mtgirs_q
      end if

   else
      jo % mtgirs_u = 0.0
      jo % mtgirs_v = 0.0
      jo % mtgirs_t = 0.0
      jo % mtgirs_q = 0.0
      jo_mtgirs     = 0.0
   end if

   if (iv%info(tamdar)%nlocal > 0) then
      call da_jo_and_grady_tamdar(iv, re, jo, jo_grad_y)
      jo_tamdar = jo%tamdar_u + jo%tamdar_v + jo%tamdar_t + jo%tamdar_q

      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            ' jo_tamdar          ', jo_tamdar, &
            '   jo%tamdar_u      ', jo%tamdar_u, &
            '   jo%tamdar_v      ', jo%tamdar_v, &
            '   jo%tamdar_t      ', jo%tamdar_t, &
            '   jo%tamdar_q      ', jo%tamdar_q
      end if
   else
      jo % tamdar_u = 0.0
      jo % tamdar_v = 0.0
      jo % tamdar_t = 0.0
      jo % tamdar_q = 0.0
      jo_tamdar     = 0.0
   end if

   if (iv%info(tamdar_sfc)%nlocal > 0) then
      call da_jo_and_grady_tamdar_sfc(iv, re, jo, jo_grad_y)
      jo_tamdar_sfc = jo%tamdar_sfc_u + jo%tamdar_sfc_v + jo%tamdar_sfc_t + &
         jo%tamdar_sfc_q + jo%tamdar_sfc_p

      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            ' jo_tamdar_sfc      ', jo_tamdar_sfc,     &
            '   jo%tamdar_sfc_u  ', jo%tamdar_sfc_u, &
            '   jo%tamdar_sfc_v  ', jo%tamdar_sfc_v, &
            '   jo%tamdar_sfc_t  ', jo%tamdar_sfc_t, &
            '   jo%tamdar_sfc_p  ', jo%tamdar_sfc_p, &
            '   jo%tamdar_sfc_q  ', jo%tamdar_sfc_q
      end if

   else
      jo % tamdar_sfc_u = 0.0
      jo % tamdar_sfc_v = 0.0
      jo % tamdar_sfc_t = 0.0
      jo % tamdar_sfc_p = 0.0
      jo % tamdar_sfc_q = 0.0
      jo_tamdar_sfc     = 0.0

   end if

   if (iv%info(synop)%nlocal > 0) then
      call da_jo_and_grady_synop(iv, re, jo, jo_grad_y)
      jo_synop = jo%synop_u + jo%synop_v + jo%synop_t + jo%synop_p + jo%synop_q

      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            ' jo_synop          ', jo_synop, &
            '   jo%synop_u      ', jo%synop_u, &
            '   jo%synop_v      ', jo%synop_v, &
            '   jo%synop_t      ', jo%synop_t, &
            '   jo%synop_p      ', jo%synop_p, &
            '   jo%synop_q      ', jo%synop_q
      end if
   else
      jo % synop_u = 0.0
      jo % synop_v = 0.0
      jo % synop_t = 0.0
      jo % synop_p = 0.0
      jo % synop_q = 0.0
      jo_synop = 0.0
   end if

   if (iv%info(geoamv)%nlocal > 0) then
      call da_jo_and_grady_geoamv(iv, re, jo, jo_grad_y)
      jo_geoamv = jo%geoamv_u + jo%geoamv_v
      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            ' jo_geoamv         ', jo_geoamv, &
            '   jo%geoamv_u     ', jo%geoamv_u, &
            '   jo%geoamv_v     ', jo%geoamv_v
      end if
   else
      jo % geoamv_u = 0.0
      jo % geoamv_v = 0.0
      jo_geoamv = 0.0
   end if

   if (iv%info(polaramv)%nlocal > 0) then
      call da_jo_and_grady_polaramv(iv, re, jo, jo_grad_y)
      jo_polaramv = jo%polaramv_u + jo%polaramv_v
      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            ' jo_polaramv       ', jo_polaramv, &
            '   jo%polaramv_u   ', jo%polaramv_u, &
            '   jo%polaramv_v   ', jo%polaramv_v
      end if
   else
      jo % polaramv_u = 0.0
      jo % polaramv_v = 0.0
      jo_polaramv = 0.0
   end if

   if (iv%info(airep)%nlocal > 0) then
      call da_jo_and_grady_airep(iv, re, jo, jo_grad_y)
      jo_airep = jo%airep_u + jo%airep_v + jo%airep_t + jo%airep_q

      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_airep          ', jo_airep, &
               '   jo%airep_u      ', jo%airep_u, &
               '   jo%airep_v      ', jo%airep_v, &
               '   jo%airep_t      ', jo%airep_t, &
               '   jo%airep_q      ', jo%airep_q
      end if
   else
      jo % airep_u = 0.0
      jo % airep_v = 0.0
      jo % airep_t = 0.0
      jo % airep_q = 0.0
      jo_airep = 0.0
   end if

   if (iv%info(pilot)%nlocal > 0) then
      call da_jo_and_grady_pilot(iv, re, jo, jo_grad_y)
      jo_pilot = jo%pilot_u + jo%pilot_v
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_pilot          ', jo_pilot, &
               '   jo%pilot_u      ', jo%pilot_u, &
               '   jo%pilot_v      ', jo%pilot_v
      end if
   else
      jo % pilot_u = 0.0
      jo % pilot_v = 0.0
      jo_pilot = 0.0
   end if

   if (iv%info(satem)%nlocal > 0) then
      call da_jo_and_grady_satem(iv, re, jo, jo_grad_y)
      jo_satem = jo%satem_thickness
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_satem          ', jo_satem, &
               '   jo%satem_thckns ', jo%satem_thickness
      end if
   else
      jo % satem_thickness = 0.0
      jo_satem = 0.0
   end if

   if (iv%info(metar)%nlocal > 0) then
      call da_jo_and_grady_metar(iv, re, jo, jo_grad_y)
      jo_metar = jo%metar_u + jo%metar_v + jo%metar_t + jo%metar_p + jo%metar_q
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_metar          ', jo_metar, &
               '   jo%metar_u      ', jo%metar_u, &
               '   jo%metar_v      ', jo%metar_v, &
               '   jo%metar_t      ', jo%metar_t, &
               '   jo%metar_p      ', jo%metar_p, &
               '   jo%metar_q      ', jo%metar_q
      end if
   else
      jo % metar_u = 0.0
      jo % metar_v = 0.0
      jo % metar_t = 0.0
      jo % metar_p = 0.0
      jo % metar_q = 0.0
      jo_metar = 0.0
   end if

   if (iv%info(ships)%nlocal > 0) then
      call da_jo_and_grady_ships(iv, re, jo, jo_grad_y)
      jo_ships = jo%ships_u + jo%ships_v + jo%ships_t + jo%ships_p + jo%ships_q
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_ships          ', jo_ships, &
               '   jo%ships_u      ', jo%ships_u, &
               '   jo%ships_v      ', jo%ships_v, &
               '   jo%ships_t      ', jo%ships_t, &
               '   jo%ships_p      ', jo%ships_p, &
               '   jo%ships_q      ', jo%ships_q
      end if
   else
      jo % ships_u = 0.0
      jo % ships_v = 0.0
      jo % ships_t = 0.0
      jo % ships_p = 0.0
      jo % ships_q = 0.0
      jo_ships = 0.0
   end if

   if (iv%info(gpspw)%nlocal > 0) then
      call da_jo_and_grady_gpspw(iv, re, jo, jo_grad_y)
      jo_gpspw = jo%gpspw_tpw
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_gpspw          ', jo_gpspw, &
               '   jo%gpspw_tpw    ', jo%gpspw_tpw
      end if
   else
      jo % gpspw_tpw = 0.0
      jo_gpspw = 0.0
   end if

   if (iv%info(gpsref)%nlocal > 0) then
      call da_jo_and_grady_gpsref(iv, re, jo, jo_grad_y)
      jo_gpsref = jo%gpsref_ref
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_gpsref         ', jo_gpsref, &
               '   jo%gpsref_ref   ', jo%gpsref_ref
      end if
   else
      jo % gpsref_ref = 0.0
      jo_gpsref = 0.0
   end if

   if (iv%info(gpseph)%nlocal > 0) then
      call da_jo_and_grady_gpseph(iv, re, jo, jo_grad_y)
      jo_gpseph = jo%gpseph_eph
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_gpseph         ', jo_gpseph, &
               '   jo%gpseph_eph   ', jo%gpseph_eph
      end if
   else
      jo % gpseph_eph = 0.0
      jo_gpseph = 0.0
   end if

   if (iv%info(ssmi_tb)%nlocal > 0) then
      call da_jo_and_grady_ssmi_tb (iv, re, jo, jo_grad_y)
      jo_ssmi_tb = jo % ssmi_tb19v + jo % ssmi_tb19h + jo % ssmi_tb22v + &
         jo % ssmi_tb37v + jo % ssmi_tb37h + jo % ssmi_tb85v + &
         jo % ssmi_tb85h 
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_ssmi_tb        ', jo_ssmi_tb, &
               '   jo%ssmi_tb19v   ', jo%ssmi_tb19v, &
               '   jo%ssmi_tb19h   ', jo%ssmi_tb19h, &
               '   jo%ssmi_tb22v   ', jo%ssmi_tb22v, &
               '   jo%ssmi_tb37v   ', jo%ssmi_tb37v, &
               '   jo%ssmi_tb37h   ', jo%ssmi_tb37h, &
               '   jo%ssmi_tb85v   ', jo%ssmi_tb85v, &
               '   jo%ssmi_tb85h   ', jo%ssmi_tb85h
      end if
   else
      jo % ssmi_tb19v = 0.0
      jo % ssmi_tb19h = 0.0
      jo % ssmi_tb22v = 0.0
      jo % ssmi_tb37v = 0.0
      jo % ssmi_tb37h = 0.0
      jo % ssmi_tb85v = 0.0
      jo % ssmi_tb85h = 0.0
      jo_ssmi_tb = 0.0
   end if

   if (iv%info(ssmi_rv)%nlocal > 0) then
      call da_jo_and_grady_ssmi_rv(iv, re, jo, jo_grad_y)
      jo_ssmi_rv = jo % ssmir_speed + jo % ssmir_tpw
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_ssmi_rv        ', jo_ssmi_rv, &
               '   jo%ssmir_speed  ', jo%ssmir_speed, &
               '   jo%ssmir_tpw    ', jo%ssmir_tpw
      end if
   else
      jo % ssmir_speed = 0.0
      jo % ssmir_tpw   = 0.0
      jo_ssmi_rv = 0.0
   end if

   if (iv%info(ssmt1)%nlocal > 0) then
      call da_jo_and_grady_ssmt1(iv, re, jo, jo_grad_y)
      jo_ssmt1 = jo%ssmt1_t
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_ssmt1          ', jo_ssmt1, &
               '   jo%ssmt1_t      ', jo%ssmt1_t
      end if
   else
      jo % ssmt1_t = 0.0
      jo_ssmt1 = 0.0
   end if

   if (iv%info(ssmt2)%nlocal > 0) then
      call da_jo_and_grady_ssmt2(iv, re, jo, jo_grad_y)  
      jo_ssmt2 = jo%ssmt2_rh
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_ssmt2          ', jo_ssmt2, &
               '   jo%ssmt2_rh     ', jo%ssmt2_rh
      end if
   else
      jo % ssmt2_rh = 0.0
      jo_ssmt2 = 0.0
   end if

   if (iv%info(radar)%nlocal > 0) then
      call da_jo_and_grady_radar(iv, re, jo, jo_grad_y)
      jo_radar = jo%radar_rv + jo%radar_rf + jo%radar_rrn + jo%radar_rsn + jo%radar_rgr + jo%radar_rqv
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_radar          ', jo_radar, &
               '   jo%radar_rv     ', jo%radar_rv, &
               '   jo%radar_rf     ', jo%radar_rf, &
               '   jo%radar_rrn    ', jo%radar_rrn, &
               '   jo%radar_rsn    ', jo%radar_rsn, &
               '   jo%radar_rgr    ', jo%radar_rgr, &
               '   jo%radar_rqv    ', jo%radar_rqv
      end if
   else
      jo % radar_rv  = 0.0
      jo % radar_rf  = 0.0
      jo % radar_rrn = 0.0
      jo % radar_rsn = 0.0
      jo % radar_rgr = 0.0
      jo % radar_rqv = 0.0
      jo_radar = 0.0
   end if

   if (iv%info(rain)%nlocal > 0) then
       call da_jo_and_grady_rain(iv, re, jo, jo_grad_y)
       jo_rain = jo%rain_r
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_rain          ', jo_rain, &
               '   jo%rain_r      ', jo%rain_r
      end if
   else
      jo % rain_r = 0.0
      jo_rain = 0.0
   end if

   if (iv%info(pseudo)%nlocal > 0) then
      call da_jo_and_grady_pseudo(iv, re, jo, jo_grad_y)    
      jo_pseudo = jo%pseudo_u + jo%pseudo_v + jo%pseudo_t + jo%pseudo_p + jo%pseudo_q
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_pseudo          ', jo_pseudo, &
               '   jo%pseudo_u      ', jo%pseudo_u, &
               '   jo%pseudo_v      ', jo%pseudo_v, &
               '   jo%pseudo_t      ', jo%pseudo_t, &
               '   jo%pseudo_p      ', jo%pseudo_p, &
               '   jo%pseudo_q      ', jo%pseudo_q
      end if
   else
      jo % pseudo_u = 0.0
      jo % pseudo_v = 0.0
      jo % pseudo_t = 0.0
      jo % pseudo_p = 0.0
      jo % pseudo_q = 0.0
      jo_pseudo = 0.0
   end if
   
   if (iv%info(qscat)%nlocal > 0) then
      call da_jo_and_grady_qscat(iv, re, jo, jo_grad_y)
      jo_qscat = jo%qscat_u + jo%qscat_v
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_qscat           ', jo_qscat, &
               '   jo%qscat_u       ', jo%qscat_u, &
               '   jo%qscat_v       ', jo%qscat_v
      end if
   else
      jo % qscat_u = 0.0
      jo % qscat_v = 0.0
      jo_qscat = 0.0
   end if

   if (iv%info(profiler)%nlocal > 0) then
      call da_jo_and_grady_profiler (iv, re, jo, jo_grad_y)
      jo_profiler = jo%profiler_u + jo%profiler_v
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_profiler        ', jo_profiler, &
               '   jo%profiler_u    ', jo%profiler_u, &
               '   jo%profiler_v    ', jo%profiler_v
      end if
   else
      jo % profiler_u = 0.0
      jo % profiler_v = 0.0
      jo_profiler = 0.0
   end if

   if (iv%info(bogus)%nlocal > 0) then
      call da_jo_and_grady_bogus (iv, re, jo, jo_grad_y)
      jo_bogus = jo%bogus_u + jo%bogus_v + jo%bogus_slp + jo%bogus_t + jo%bogus_q
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_bogus           ', jo_bogus, &
               '   jo%bogus_u       ', jo%bogus_u, &
               '   jo%bogus_v       ', jo%bogus_v, &
               '   jo%bogus_t       ', jo%bogus_t, &
               '   jo%bogus_slp     ', jo%bogus_slp, &
               '   jo%bogus_q       ', jo%bogus_q
      end if
   else
      jo % bogus_u   = 0.0
      jo % bogus_v   = 0.0
      jo % bogus_t   = 0.0
      jo % bogus_q   = 0.0
      jo % bogus_slp = 0.0
      jo_bogus = 0.0
   end if

   if (iv%info(buoy)%nlocal > 0) then
      call da_jo_and_grady_buoy (iv, re, jo, jo_grad_y)
      jo_buoy = jo%buoy_u + jo%buoy_v + jo%buoy_t + jo%buoy_p + jo%buoy_q
      if (print_detail_grad) then
          write(unit=stdout, fmt='(a, e24.12)') &
               ' jo_buoy            ', jo_buoy, &
               '   jo%buoy_u        ', jo%buoy_u, &
               '   jo%buoy_v        ', jo%buoy_v, &
               '   jo%buoy_t        ', jo%buoy_t, &
               '   jo%buoy_p        ', jo%buoy_p, &
               '   jo%buoy_q        ', jo%buoy_q
      end if
   else
      jo % buoy_u = 0.0
      jo % buoy_v = 0.0
      jo % buoy_t = 0.0
      jo % buoy_p = 0.0
      jo % buoy_q = 0.0
      jo_buoy = 0.0
   end if

   jo_radiance = 0.0
#if defined(CRTM) || defined(RTTOV)
   if (iv%num_inst > 0) then
      call da_jo_and_grady_rad (iv, re, jo, jo_grad_y)

      if (use_rad) then
         do i=1,iv%num_inst
            do k=1,iv%instid(i)%nchan
               jo_radiance = jo_radiance + jo%rad(i)%jo_ichan(k)
            end do
         end do
      end if
      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            ' jo_radiance       ', jo_radiance
         do i = 1, iv%num_inst
            write(unit=stdout, fmt='(a, e24.12)') &
               trim('   jo_'//iv%instid(i)%rttovid_string), sum(jo%rad(i)%jo_ichan(:))
         end do
      end if
   end if
#endif

   if (iv%info(airsr)%nlocal > 0) then
      call da_jo_and_grady_airsr(iv, re, jo, jo_grad_y)
      jo_airsr = jo%airsr_t + jo%airsr_q

      if (print_detail_grad) then
         write(unit=stdout, fmt='(a, e24.12)') &
            ' jo_airsr          ', jo_airsr, &
            '   jo%airsr_t      ', jo%airsr_t, &
            '   jo%airsr_q      ', jo%airsr_q
      end if
   else
      jo%airsr_t = 0.0
      jo%airsr_q = 0.0
      jo_airsr = 0.0
   end if

   !-------------------------------------------------------------------------
   ! [2.0] Jo = 1/2 * (yo-y)**2/ob_err_variance:
   !-------------------------------------------------------------------------

   jo%total = jo_sound + jo_sonde_sfc+jo_geoamv + jo_polaramv + jo_synop + jo_satem + &
      jo_pilot + jo_airep + jo_metar + jo_ships + &
      jo_gpspw + jo_ssmi_tb + jo_ssmi_rv + jo_ssmt1 + jo_ssmt2 + &
      jo_pseudo + jo_qscat + jo_profiler + jo_buoy + &
      jo_radar + jo_gpsref + jo_gpseph + jo_bogus + jo_radiance + jo_airsr + jo_mtgirs + &
      jo_tamdar + jo_tamdar_sfc + jo_rain 

   jot = jo%total

   if (print_detail_grad) then
      write(unit=stdout, fmt='(a, e24.12)') &
         '   jo%total      ', jot

      write(unit=stdout, fmt='(a, e24.12)') &
         '   jo_sound        ', jo_sound, &
         '   jo_sonde_sfc    ', jo_sonde_sfc, &
         '   jo_geoamv       ', jo_geoamv, &
         '   jo_polaramv     ', jo_polaramv, &
         '   jo_synop        ', jo_synop, &
         '   jo_satem        ', jo_satem, &
         '   jo_pilot        ', jo_pilot, &
         '   jo_airep        ', jo_airep, &
         '   jo_metar        ', jo_metar, &
         '   jo_ships        ', jo_ships, &
         '   jo_gpspw        ', jo_gpspw, &
         '   jo_ssmi_tb      ', jo_ssmi_tb, &
         '   jo_ssmi_rv      ', jo_ssmi_rv, &
         '   jo_ssmt1        ', jo_ssmt1, &
         '   jo_ssmt2        ', jo_ssmt2, &
         '   jo_pseudo       ', jo_pseudo, &
         '   jo_qscat        ', jo_qscat, &
         '   jo_profiler     ', jo_profiler, &
         '   jo_buoy         ', jo_buoy, &
         '   jo_radar        ', jo_radar, &
         '   jo_gpsref       ', jo_gpsref, &
         '   jo_gpseph       ', jo_gpseph, &
         '   jo_bogus        ', jo_bogus,  &
         '   jo_radiance     ', jo_radiance, &
         '   jo_airsr        ', jo_airsr,&
         '   jo_mtgirs       ', jo_mtgirs, &
         '   jo_tamdar       ', jo_tamdar, &
         '   jo_tamdar_sfc   ', jo_tamdar_sfc, &
         '   jo_rain         ', jo_rain

   end if

   if (trace_use) call da_trace_exit("da_jo_and_grady")

end subroutine da_jo_and_grady


