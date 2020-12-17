subroutine da_use_obs_errfac(iv)

   !-------------------------------------------------------------------------
   ! Purpose: Allocates observation structure and fills it from iv.
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout) :: iv              ! Obs and header structure.

   integer                       :: n, k            ! Loop counters.
   real                          :: d1, d2, d3, d4  ! Dummy values.

   if (trace_use) call da_trace_entry("da_use_obs_errfac")

   !----------------------------------------------------------------------
   ! [2.0] Scale observation errors:
   !-------------------------------------------------------------------

   ! [2.1] Transfer surface obs:

   call da_read_errfac('synop', iv % synop_ef_u, &
                        iv % synop_ef_v, iv % synop_ef_t, &
                        iv % synop_ef_p, iv % synop_ef_q)                           

   if (iv%info(synop)%nlocal > 0) then
      do n = 1, iv%info(synop)%nlocal
         iv % synop(n) % u % error = iv % synop(n) % u % error * iv % synop_ef_u
         iv % synop(n) % v % error = iv % synop(n) % v % error * iv % synop_ef_v
         iv % synop(n) % t % error = iv % synop(n) % t % error * iv % synop_ef_t
         iv % synop(n) % p % error = iv % synop(n) % p % error * iv % synop_ef_p
         iv % synop(n) % q % error = iv % synop(n) % q % error * iv % synop_ef_q
      end do
   end if

   ! [2.2] Transfer metar obs:


   call da_read_errfac('metar', iv % metar_ef_u, &
                        iv % metar_ef_v, iv % metar_ef_t, &
                        iv % metar_ef_p, iv % metar_ef_q)
                           
   if (iv%info(metar)%nlocal > 0) then
      do n = 1, iv%info(metar)%nlocal
         iv % metar(n) % u % error = iv % metar(n) % u % error * iv % metar_ef_u
         iv % metar(n) % v % error = iv % metar(n) % v % error * iv % metar_ef_v
         iv % metar(n) % t % error = iv % metar(n) % t % error * iv % metar_ef_t
         iv % metar(n) % p % error = iv % metar(n) % p % error * iv % metar_ef_p
         iv % metar(n) % q % error = iv % metar(n) % q % error * iv % metar_ef_q
      end do
   end if

   ! [2.2] Transfer ships obs:

      
   call da_read_errfac('ships', iv % ships_ef_u, &
                        iv % ships_ef_v, iv % ships_ef_t, &
                        iv % ships_ef_p, iv % ships_ef_q)
                           
   if (iv%info(ships)%nlocal > 0) then
      do n = 1, iv%info(ships)%nlocal
         iv % ships(n) % u % error = iv % ships(n) % u % error * iv % ships_ef_u
         iv % ships(n) % v % error = iv % ships(n) % v % error * iv % ships_ef_v
         iv % ships(n) % t % error = iv % ships(n) % t % error * iv % ships_ef_t
         iv % ships(n) % p % error = iv % ships(n) % p % error * iv % ships_ef_p
         iv % ships(n) % q % error = iv % ships(n) % q % error * iv % ships_ef_q
      end do
   end if

   ! [2.4.1] Transfer Geo. AMVs Obs:
   
   
   call da_read_errfac('geoamv', iv % geoamv_ef_u, iv % geoamv_ef_v, d1, d2, d3)

   if (iv%info(geoamv)%nlocal > 0) then
      do n = 1, iv%info(geoamv)%nlocal
        do k = 1, iv%info(geoamv)%levels(n)
         iv % geoamv(n) % u(k) % error = iv % geoamv(n) % u(k) % error * iv % geoamv_ef_u
         iv % geoamv(n) % v(k) % error = iv % geoamv(n) % v(k) % error * iv % geoamv_ef_v
        end do
      end do 
   end if

   ! [2.4.2] Transfer Polar AMVs Obs:


   call da_read_errfac('polaramv', iv % polaramv_ef_u, iv % polaramv_ef_v, d1, d2, d3)

   if (iv%info(polaramv)%nlocal > 0) then 
      do n = 1, iv%info(polaramv)%nlocal
        do k = 1, iv%info(polaramv)%levels(n)
         iv % polaramv(n) % u(k) % error = iv % polaramv(n) % u(k) % error * iv % polaramv_ef_u
         iv % polaramv(n) % v(k) % error = iv % polaramv(n) % v(k) % error * iv % polaramv_ef_v
        end do
      end do
   end if


   ! [2.5] Transfer gpspw obs:


   call da_read_errfac('gpspw', iv % gpspw_ef_tpw, d1, d2, d3, d4)

   if (iv%info(gpspw)%nlocal > 0) then
      do n = 1, iv%info(gpspw)%nlocal
         iv % gpspw(n) % tpw % error = iv % gpspw(n) % tpw % error * &
                                       iv % gpspw_ef_tpw

      end do
   end if

! [2.5.1] Transfer gpsref obs:

   call da_read_errfac('gpsre', iv % gpsref_ef_ref, d1, d2, d3, d4)

   if (iv%info(gpsref)%nlocal > 0) then
      do n = 1, iv%info(gpsref)%nlocal
         do k = 1, iv%info(gpsref)%levels(n)
            iv % gpsref(n) % ref(k) % error = iv % gpsref(n) % ref(k) % error * &
                                              iv % gpsref_ef_ref
         enddo
      end do
   end if

! [2.5.2] Transfer gpseph obs:

   call da_read_errfac('gpsep', iv % gpseph_ef_eph, d1, d2, d3, d4)

   if (iv%info(gpseph)%nlocal > 0) then
      do n = 1, iv%info(gpseph)%nlocal
         do k = 1, iv%info(gpseph)%levels(n)
            if ( iv % gpseph(n) % eph(k) % error > 0.0 ) then
               !only apply the error factor when the error is not missing
               iv % gpseph(n) % eph(k) % error = iv % gpseph(n) % eph(k) % error * &
                                                 iv % gpseph_ef_eph
            end if
         enddo
      end do
   end if

   ! [2.6] Transfer sonde obs:


   call da_read_errfac('sound', iv % sound_ef_u, iv % sound_ef_v, &
                        iv % sound_ef_t, iv % sound_ef_q, d1)

   if (iv%info(sound)%nlocal > 0) then
      do n = 1, iv%info(sound)%nlocal
         do k = 1, iv%info(sound)%levels(n)
            iv % sound(n) % u(k) % error = iv % sound(n) % u(k) % error * &
                                           iv % sound_ef_u
            iv % sound(n) % v(k) % error = iv % sound(n) % v(k) % error * &
                                           iv % sound_ef_v
            iv % sound(n) % t(k) % error = iv % sound(n) % t(k) % error * &
                                           iv % sound_ef_t
            iv % sound(n) % q(k) % error = iv % sound(n) % q(k) % error * &
                                           iv % sound_ef_q
         end do
      end do
   end if

   if (iv%info(sonde_sfc)%nlocal > 0) then
      do n = 1, iv%info(sonde_sfc)%nlocal
         iv % sonde_sfc(n) % u % error = iv % sonde_sfc(n) % u % error * iv % synop_ef_u
         iv % sonde_sfc(n) % v % error = iv % sonde_sfc(n) % v % error * iv % synop_ef_v
         iv % sonde_sfc(n) % t % error = iv % sonde_sfc(n) % t % error * iv % synop_ef_t
         iv % sonde_sfc(n) % p % error = iv % sonde_sfc(n) % p % error * iv % synop_ef_p
         iv % sonde_sfc(n) % q % error = iv % sonde_sfc(n) % q % error * iv % synop_ef_q
      end do
   end if

   ! [2.7] Transfer airep obs:

   
   call da_read_errfac('airep', iv % airep_ef_u, iv % airep_ef_v, &
                        iv % airep_ef_t, iv % airep_ef_q, d1)

   if (iv%info(airep)%nlocal > 0) then
      do n = 1, iv%info(airep)%nlocal
         do k = 1, iv%info(airep)%levels(n)
            iv % airep(n) % u(k) % error = iv % airep(n) % u(k) % error * &
                                           iv % airep_ef_u
            iv % airep(n) % v(k) % error = iv % airep(n) % v(k) % error * &
                                           iv % airep_ef_v
            iv % airep(n) % t(k) % error = iv % airep(n) % t(k) % error * &
                                           iv % airep_ef_t
            iv % airep(n) % q(k) % error = iv % airep(n) % q(k) % error * &
                                           iv % airep_ef_q
         end do
      end do
   end if

   ! [2.8] Transfer pilot obs:


   call da_read_errfac('pilot', iv % pilot_ef_u, iv % pilot_ef_v, d1, d2, d3)

   if (iv%info(pilot)%nlocal > 0) then
      do n = 1, iv%info(pilot)%nlocal
         do k = 1, iv%info(pilot)%levels(n)
            iv % pilot(n) % u(k) % error = iv % pilot(n) % u(k) % error * &
                                           iv % pilot_ef_u
            iv % pilot(n) % v(k) % error = iv % pilot(n) % v(k) % error * &
                                           iv % pilot_ef_v

         end do
      end do
   end if

   ! [2.9] Transfer SSM/I obs:SSMI:


   call da_read_errfac('ssmir', iv % ssmir_ef_speed, iv % ssmir_ef_tpw, d1, d2, d3)

   if (iv%info(ssmi_rv)%nlocal > 0) then
      do n = 1, iv%info(ssmi_rv)%nlocal
         iv%ssmi_rv(n)%tpw%error = iv%ssmi_rv(n)%tpw%error * &
                                          iv % ssmir_ef_tpw
         iv%ssmi_rv(n)%speed%error = iv%ssmi_rv(n)%speed%error * &
                                            iv % ssmir_ef_speed
      end do
   end if


   ! iv % ssmit_ef_tb19h = 1.0 ! Tuning not yet coded.
   ! iv % ssmit_ef_tb19v = 1.0 ! Tuning not yet coded.
   ! iv % ssmit_ef_tb22v = 1.0 ! Tuning not yet coded.
   ! iv % ssmit_ef_tb37h = 1.0 ! Tuning not yet coded.
   ! iv % ssmit_ef_tb37v = 1.0 ! Tuning not yet coded.
   ! iv % ssmit_ef_tb85h = 1.0 ! Tuning not yet coded.
   ! iv % ssmit_ef_tb85v = 1.0 ! Tuning not yet coded.

   if (iv%info(ssmi_tb)%nlocal > 0) then
      ! do n = 1, iv%info(ssmi_tb)%nlocal
      !    iv%ssmi_tb(n)%tb19h%error = iv%ssmi_tb(n)%tb19h%error
      !    iv%ssmi_tb(n)%tb19v%error = iv%ssmi_tb(n)%tb19v%error
      !    iv%ssmi_tb(n)%tb22v%error = iv%ssmi_tb(n)%tb22v%error
      !    iv%ssmi_tb(n)%tb37h%error = iv%ssmi_tb(n)%tb37h%error * &
      !                                fac_ssmit_tb37h
      !    iv%ssmi_tb(n)%tb37v%error = iv%ssmi_tb(n)%tb37v%error * &
      !                                fac_ssmit_tb37v
      !    iv%ssmi_tb(n)%tb85h%error = iv%ssmi_tb(n)%tb85h%error * &
      !                                fac_ssmit_tb85h
      !    iv%ssmi_tb(n)%tb85v%error = iv%ssmi_tb(n)%tb85v%error * &
      !                                fac_ssmit_tb85v
      ! end do
   end if

   ! [2.10] Transfer satem obs:

   call da_read_errfac('satem', iv % satem_ef_thickness, d1, d2, d3, d4)

   if (iv%info(satem)%nlocal > 0) then
      do n = 1, iv%info(satem)%nlocal
         do k = 1, iv%info(satem)%levels(n)
            iv % satem(n) % thickness(k) % error = iv % satem(n) % thickness(k) % error*&
                                                   iv % satem_ef_thickness
         end do
      end do
   end if
   
   ! [2.11] Transfer ssmt1 obs:

   call da_read_errfac('ssmt1', iv % ssmt1_ef_t, d1, d2, d3, d4)
      
   if (iv%info(ssmt1)%nlocal > 0) then
      do n = 1, iv%info(ssmt1)%nlocal
         do k = 1, iv%info(ssmt1)%levels(n)   
            iv % ssmt1(n) % t(k) % error = iv % ssmt1(n) % t(k) % error * &
                                        iv % ssmt1_ef_t
         end do
      end do
   end if

   ! [2.12] Transfer ssmt2 obs:

   call da_read_errfac('ssmt2', iv % ssmt2_ef_rh, d1, d2, d3, d4)

   if (iv%info(ssmt2)%nlocal > 0) then
      do n = 1, iv%info(ssmt2)%nlocal
         do k = 1, iv%info(ssmt2)%levels(n)      
            iv % ssmt2(n) % rh(k) % error = iv % ssmt2(n) % rh(k) % error * &
                                         iv % ssmt2_ef_rh
         end do
      end do
   end if
   
   ! [2.13] Transfer scatterometer obs:

   call da_read_errfac('qscat', iv % qscat_ef_u, &
                        iv % qscat_ef_v, d1, d2, d3)
                           
   if (iv%info(qscat)%nlocal > 0) then
      do n = 1, iv%info(qscat)%nlocal
         iv % qscat(n) % u % error = iv % qscat(n) % u % error * iv % qscat_ef_u
         iv % qscat(n) % v % error = iv % qscat(n) % v % error * iv % qscat_ef_v
      end do
   end if

   ! [2.14] Transfer profiler obs:

   call da_read_errfac('profi', iv % profiler_ef_u, iv % profiler_ef_v, d1, d2, d3)

   if (iv%info(profiler)%nlocal > 0) then
      do n = 1, iv%info(profiler)%nlocal
         do k = 1, iv%info(profiler)%levels(n)
            iv % profiler(n) % u(k) % error = iv % profiler(n) % u(k) % error * &
                                           iv % profiler_ef_u
            iv % profiler(n) % v(k) % error = iv % profiler(n) % v(k) % error * &
                                           iv % profiler_ef_v

         end do
      end do
   end if

   ! [2.15] Transfer buoy obs:

   call da_read_errfac('buoy ', iv % buoy_ef_u, &
                        iv % buoy_ef_v, iv % buoy_ef_t, &
                        iv % buoy_ef_p, iv % buoy_ef_q)
                           
   if (iv%info(buoy)%nlocal > 0) then  
      do n = 1, iv%info(buoy)%nlocal
         iv % buoy(n) % u % error = iv % buoy(n) % u % error * iv % buoy_ef_u
         iv % buoy(n) % v % error = iv % buoy(n) % v % error * iv % buoy_ef_v
         iv % buoy(n) % t % error = iv % buoy(n) % t % error * iv % buoy_ef_t
         iv % buoy(n) % p % error = iv % buoy(n) % p % error * iv % buoy_ef_p
         iv % buoy(n) % q % error = iv % buoy(n) % q % error * iv % buoy_ef_q
      end do
   end if

   ! [2.16] Transfer TC bogus obs:

   call da_read_errfac('bogus', iv % bogus_ef_u, iv % bogus_ef_v, &
                        iv % bogus_ef_t, iv % bogus_ef_q, iv % bogus_ef_slp)

   if (iv%info(bogus)%nlocal > 0) then
      do n = 1, iv%info(bogus)%nlocal
         do k = 1, iv%info(bogus)%levels(n)
            iv % bogus(n) % u(k) % error = iv % bogus(n) % u(k) % error * &
                                           iv % bogus_ef_u
            iv % bogus(n) % v(k) % error = iv % bogus(n) % v(k) % error * &
                                           iv % bogus_ef_v
            iv % bogus(n) % t(k) % error = iv % bogus(n) % t(k) % error * &
                                           iv % bogus_ef_t
            iv % bogus(n) % q(k) % error = iv % bogus(n) % q(k) % error * &
                                           iv % bogus_ef_q

         end do

         iv % bogus(n) % slp % error = iv % bogus(n) % slp % error * iv % bogus_ef_slp
      end do
   end if

   ! Transfer AIRS retrievals:

   call da_read_errfac('airsr', iv % airsr_ef_t, iv % airsr_ef_q, d1, d3, d3)

   if (iv%info(airsr)%nlocal > 0) then
      do n = 1, iv%info(airsr)%nlocal
         do k = 1, iv%info(airsr)%levels(n)
            iv % airsr(n) % t(k) % error = iv % airsr(n) % t(k) % error * &
                                           iv % airsr_ef_t
            iv % airsr(n) % q(k) % error = iv % airsr(n) % q(k) % error * &
                                           iv % airsr_ef_q
         end do
      end do
   end if

   ! Transfer mtgirs obs:

   call da_read_errfac('mtgirs', iv % mtgirs_ef_u, iv % mtgirs_ef_v, &
                        iv % mtgirs_ef_t, iv % mtgirs_ef_q, d1)

   if (iv%info(mtgirs)%nlocal > 0) then
      do n = 1, iv%info(mtgirs)%nlocal
         do k = 1, iv%info(mtgirs)%levels(n)
            iv % mtgirs(n) % u(k) % error = iv % mtgirs(n) % u(k) % error * &
                                           iv % mtgirs_ef_u
            iv % mtgirs(n) % v(k) % error = iv % mtgirs(n) % v(k) % error * &
                                           iv % mtgirs_ef_v
            iv % mtgirs(n) % t(k) % error = iv % mtgirs(n) % t(k) % error * &
                                           iv % mtgirs_ef_t
            iv % mtgirs(n) % q(k) % error = iv % mtgirs(n) % q(k) % error * &
                                           iv % mtgirs_ef_q

         end do

      end do
   end if

   ! Transfer tamdar obs:

   if (iv%info(tamdar)%nlocal > 0) then
      call da_read_errfac('tamdar', iv % tamdar_ef_u, iv % tamdar_ef_v, &
                           iv % tamdar_ef_t, iv % tamdar_ef_q, d1)

      do n = 1, iv%info(tamdar)%nlocal
         do k = 1, iv%info(tamdar)%levels(n)
            iv % tamdar(n) % u(k) % error = iv % tamdar(n) % u(k) % error * &
                                           iv % tamdar_ef_u
            iv % tamdar(n) % v(k) % error = iv % tamdar(n) % v(k) % error * &
                                           iv % tamdar_ef_v
            iv % tamdar(n) % t(k) % error = iv % tamdar(n) % t(k) % error * &
                                           iv % tamdar_ef_t
            iv % tamdar(n) % q(k) % error = iv % tamdar(n) % q(k) % error * &
                                           iv % tamdar_ef_q
         end do
      end do
    end if

   ! Transfer tamdar_sfc obs:

   if (iv%info(tamdar_sfc)%nlocal > 0) then
      do n = 1, iv%info(tamdar_sfc)%nlocal
         iv % tamdar_sfc(n) % u % error = iv % tamdar_sfc(n) % u % error * iv % tamdar_sfc_ef_u
         iv % tamdar_sfc(n) % v % error = iv % tamdar_sfc(n) % v % error * iv % tamdar_sfc_ef_v
         iv % tamdar_sfc(n) % t % error = iv % tamdar_sfc(n) % t % error * iv % tamdar_sfc_ef_t
         iv % tamdar_sfc(n) % p % error = iv % tamdar_sfc(n) % p % error * iv % tamdar_sfc_ef_p
         iv % tamdar_sfc(n) % q % error = iv % tamdar_sfc(n) % q % error * iv % tamdar_sfc_ef_q
      end do
   end if


   if (trace_use) call da_trace_exit("da_use_obs_errfac")

end subroutine da_use_obs_errfac


