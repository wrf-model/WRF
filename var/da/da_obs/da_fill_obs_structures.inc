subroutine da_fill_obs_structures(iv, ob, uvq_direct)

   !----------------------------------------------------------------------------   
   ! Purpose: Allocates observation structure and fills it from iv.
   !----------------------------------------------------------------------------   

   implicit none

   type (iv_type), intent(inout) :: iv   ! Obs and header structure.
   type (y_type), intent(out)    :: ob   ! (Smaller) observation structure.
   logical, optional             :: uvq_direct  !flag for having direct u,v,q obs

   integer :: n, k     ! Loop counters.
   real    :: rh_error ! RH obs. error.
   real    :: q_error  ! q obs. error.
   real    :: geometric_h, geopotential_h
   integer :: i,j
   logical :: outside

   if (trace_use) call da_trace_entry("da_fill_obs_structures")

   !---------------------------------------------------------------------------
   ! Initialise obs error factors (which will be overwritten in use_obs_errfac)
   !---------------------------------------------------------------------------

   iv % synop_ef_u = 1.0
   iv % synop_ef_v = 1.0
   iv % synop_ef_t = 1.0
   iv % synop_ef_p = 1.0
   iv % synop_ef_q = 1.0

   iv % metar_ef_u = 1.0
   iv % metar_ef_v = 1.0
   iv % metar_ef_t = 1.0
   iv % metar_ef_p = 1.0
   iv % metar_ef_q = 1.0

   iv % ships_ef_u = 1.0
   iv % ships_ef_v = 1.0
   iv % ships_ef_t = 1.0
   iv % ships_ef_p = 1.0
   iv % ships_ef_q = 1.0

   iv % geoamv_ef_u = 1.0
   iv % geoamv_ef_v = 1.0

   iv % polaramv_ef_u = 1.0
   iv % polaramv_ef_v = 1.0

   iv % gpspw_ef_tpw = 1.0

   iv % gpsref_ef_ref = 1.0
   iv % gpsref_ef_p = 1.0
   iv % gpsref_ef_t = 1.0
   iv % gpsref_ef_q = 1.0

   iv % gpseph_ef_eph = 1.0

   iv % sound_ef_u = 1.0
   iv % sound_ef_v = 1.0
   iv % sound_ef_t = 1.0
   iv % sound_ef_q = 1.0

   iv % airep_ef_u = 1.0
   iv % airep_ef_v = 1.0
   iv % airep_ef_t = 1.0
   iv % airep_ef_q = 1.0

   iv % pilot_ef_u = 1.0
   iv % pilot_ef_v = 1.0

   iv % ssmir_ef_speed = 1.0
   iv % ssmir_ef_tpw = 1.0

   iv % satem_ef_thickness = 1.0

   iv % ssmt1_ef_t = 1.0

   iv % ssmt2_ef_rh = 1.0

   iv % qscat_ef_u = 1.0
   iv % qscat_ef_v = 1.0

   iv % profiler_ef_u = 1.0
   iv % profiler_ef_v = 1.0
   
   iv % buoy_ef_u = 1.0
   iv % buoy_ef_v = 1.0
   iv % buoy_ef_t = 1.0
   iv % buoy_ef_p = 1.0
   iv % buoy_ef_q = 1.0

   iv % radar_ef_rv = 1.0
   iv % radar_ef_rf = 1.0

   iv % rain_ef_r  = 1.0

   iv % bogus_ef_u = 1.0
   iv % bogus_ef_v = 1.0
   iv % bogus_ef_t = 1.0
   iv % bogus_ef_p = 1.0
   iv % bogus_ef_q = 1.0
   iv % bogus_ef_slp = 1.0

   iv % airsr_ef_t = 1.0
   iv % airsr_ef_q = 1.0

   iv % mtgirs_ef_u = 1.0
   iv % mtgirs_ef_v = 1.0
   iv % mtgirs_ef_t = 1.0
   iv % mtgirs_ef_q = 1.0

   iv % tamdar_ef_u = 1.0
   iv % tamdar_ef_v = 1.0
   iv % tamdar_ef_t = 1.0
   iv % tamdar_ef_q = 1.0

   iv % tamdar_sfc_ef_u = 1.0
   iv % tamdar_sfc_ef_v = 1.0
   iv % tamdar_sfc_ef_t = 1.0
   iv % tamdar_sfc_ef_p = 1.0
   iv % tamdar_sfc_ef_q = 1.0

   !----------------------------------------------------------------------
   ! [1.0] Allocate innovation vector and observation structures:
   !----------------------------------------------------------------------
   call da_allocate_y(iv, ob)

   !----------------------------------------------------------------------
   ! [2.0] Transfer observations:
   !----------------------------------------------------------------------

   ! [2.1] Transfer surface obs:

   if (iv%info(synop)%nlocal > 0) then
      do n = 1, iv%info(synop)%nlocal
         ob % synop(n) % u = iv % synop(n) % u % inv
         ob % synop(n) % v = iv % synop(n) % v % inv
         ob % synop(n) % t = iv % synop(n) % t % inv
         ob % synop(n) % q = iv % synop(n) % q % inv
         ob % synop(n) % p = iv % synop(n) % p % inv

        if ( q_error_options == 1 ) then
         ! Calculate q error from rh error:

         if (.not. present(uvq_direct) .or. (present(uvq_direct) .and. (.not. uvq_direct))) then
         rh_error = iv%synop(n)%q%error ! q error is rh at this stage!

         ! if((ob % synop(n) % p > iv%ptop) .AND. &
         !    (ob % synop(n) % t > 100.0) .AND. &
         !    (ob % synop(n) % q > 0.0) .AND. &
         !    (iv % synop(n) % p % qc >= obs_qc_pointer) .and. &
         !    (iv % synop(n) % t % qc >= obs_qc_pointer) .and. &
         !    (iv % synop(n) % q % qc >= obs_qc_pointer)) then
         call da_get_q_error(ob % synop(n) % p, &
                              ob % synop(n) % t, &
                              ob % synop(n) % q, &
                              iv % synop(n) % t % error, &
                              rh_error, iv % synop(n) % q % error)
         if (iv%synop(n)% q % error == missing_r) iv%synop(n)% q % qc = missing_data

         ! end if
         end if
        end if !q_error_options
      end do      
   end if

   ! [2.2] Transfer metar obs:

   if (iv%info(metar)%nlocal > 0) then
      do n = 1, iv%info(metar)%nlocal
         ob % metar(n) % u = iv % metar(n) % u % inv
         ob % metar(n) % v = iv % metar(n) % v % inv
         ob % metar(n) % t = iv % metar(n) % t % inv
         ob % metar(n) % q = iv % metar(n) % q % inv
         ob % metar(n) % p = iv % metar(n) % p % inv

         ! Calculate q error from rh error:

         if (.not. present(uvq_direct) .or. (present(uvq_direct) .and. (.not. uvq_direct))) then
         rh_error = iv%metar(n)%q%error ! q error is rh at this stage!
         call da_get_q_error(iv % metar(n) % p % inv, &
                              ob % metar(n) % t, &
                              ob % metar(n) % q, &
                              iv % metar(n) % t % error, &
                              rh_error, q_error)
         iv % metar(n) % q % error = q_error
         if (iv%metar(n)% q % error == missing_r) &
            iv%metar(n)% q % qc = missing_data
         end if
      end do
   end if

   ! [2.2] Transfer ships obs:

   if (iv%info(ships)%nlocal > 0) then   
      do n = 1, iv%info(ships)%nlocal
         ob % ships(n) % u = iv % ships(n) % u % inv
         ob % ships(n) % v = iv % ships(n) % v % inv
         ob % ships(n) % t = iv % ships(n) % t % inv
         ob % ships(n) % q = iv % ships(n) % q % inv
         ob % ships(n) % p = iv % ships(n) % p % inv

         ! Calculate q error from rh error:

         if (.not. present(uvq_direct) .or. (present(uvq_direct) .and. (.not. uvq_direct))) then
         rh_error = iv%ships(n)%q%error ! q error is rh at this stage!
         call da_get_q_error(iv % ships(n) % p % inv, &
                              ob % ships(n) % t, &
                              ob % ships(n) % q, &
                              iv % ships(n) % t % error, &
                              rh_error, q_error)
         iv % ships(n) % q % error = q_error

         if(iv%ships(n)% q % error == missing_r) iv%ships(n)% q % qc = missing_data
         end if
      end do
      
   end if

   ! [2.4.1] Transfer Geo. AMVs Obs:

   if (iv%info(geoamv)%nlocal > 0) then
      do n = 1, iv%info(geoamv)%nlocal
         do k = 1, iv%info(geoamv)%levels(n)
            ob % geoamv(n) % u(k) = iv % geoamv(n) % u(k) % inv
            ob % geoamv(n) % v(k) = iv % geoamv(n) % v(k) % inv
         end do
      end do
   end if

   ! [2.4.2] Transfer  Polar AMVs Obs:

   if (iv%info(polaramv)%nlocal > 0) then
      do n = 1, iv%info(polaramv)%nlocal
         do k = 1,iv%info(polaramv)%levels(n)
            ob % polaramv(n) % u(k) = iv % polaramv(n) % u(k) % inv
            ob % polaramv(n) % v(k) = iv % polaramv(n) % v(k) % inv
         end do
      end do
   end if

   ! [2.5] Transfer gpspw obs:

   if (iv%info(gpspw)%nlocal > 0) then
      do n = 1, iv%info(gpspw)%nlocal
         ob % gpspw(n) % tpw = iv % gpspw(n) % tpw % inv
      end do

   end if

   ! [2.6] Transfer GPS REF obs:

   if (iv%info(gpsref)%nlocal > 0) then
      do n = 1, iv%info(gpsref)%nlocal
         do k = 1, iv%info(gpsref)%levels(n)
! ............................................................................
! Convert the geometric height to the geopotential height for GPSREF data
! because GPSRO used geometric height for impact parameter, and the WRF model
! always uses the geopotential height as the vertical coordinate for P, T, q.
! YRG (10/05/2010):
            geometric_h = iv%gpsref(n)%h(k) / 1000.0
            call da_msl2geo1 (geometric_h, iv%info(gpsref)%lat(1,n),  &
                           iv%info(gpsref)%lon(1,n), geopotential_h)
            iv % gpsref(n) % h(k) = geopotential_h * 1000.0
!            write (999,'("n=",i6," k=",i3,2x,"gmh, gph, lat, lon:",4f15.5)') &
!                          n, k, geometric_h*1000.0, iv % gpsref(n) % h(k), &
!                          iv%info(gpsref)%lat(1,n), iv%info(gpsref)%lon(1,n)
! ............................................................................
            ob % gpsref(n) % ref(k) = iv % gpsref(n) % ref(k) % inv
            ob % gpsref(n) %   p(k) = iv % gpsref(n) %   p(k) % inv
            ob % gpsref(n) %   t(k) = iv % gpsref(n) %   t(k) % inv
            ob % gpsref(n) %   q(k) = iv % gpsref(n) %   q(k) % inv
         end do
      end do
   end if

   ! [2.6.1] Transfer GPS EPH obs:

   if (iv%info(gpseph)%nlocal > 0) then
      do n = 1, iv%info(gpseph)%nlocal
         do k = 1, iv%info(gpseph)%levels(n)
            ob % gpseph(n) % eph(k) = iv % gpseph(n) % eph(k) % inv
         end do
      end do
   end if

   ! [2.7] Transfer sonde obs:

   if (iv%info(sound)%nlocal > 0) then
      do n = 1, iv%info(sound)%nlocal
         do k = 1, iv%info(sound)%levels(n)
            ob % sound(n) % u(k) = iv % sound(n) % u(k) % inv
            ob % sound(n) % v(k) = iv % sound(n) % v(k) % inv
            ob % sound(n) % t(k) = iv % sound(n) % t(k) % inv
            ob % sound(n) % q(k) = iv % sound(n) % q(k) % inv

            ! Calculate q error from rh error:

         if (.not. present(uvq_direct) .or. (present(uvq_direct) .and. (.not. uvq_direct))) then
            rh_error = iv%sound(n)%q(k)%error ! q error is rh at this stage!
            call da_get_q_error(iv % sound(n) % p(k), &
                                 ob % sound(n) % t(k), &
                                 ob % sound(n) % q(k), &
                                 iv % sound(n) % t(k) % error, &
                                 rh_error, q_error)

            iv % sound(n) % q(k) % error = q_error
         if (iv%sound(n)% q(k) % error == missing_r) &
            iv%sound(n)% q(k) % qc = missing_data
         end if
         end do
      end do
   end if

   if (iv%info(sonde_sfc)%nlocal > 0) then
      do n = 1, iv%info(sonde_sfc)%nlocal
         ob % sonde_sfc(n) % u = iv % sonde_sfc(n) % u % inv
         ob % sonde_sfc(n) % v = iv % sonde_sfc(n) % v % inv
         ob % sonde_sfc(n) % t = iv % sonde_sfc(n) % t % inv
         ob % sonde_sfc(n) % q = iv % sonde_sfc(n) % q % inv
         ob % sonde_sfc(n) % p = iv % sonde_sfc(n) % p % inv

         ! Calculate q error from rh error:

         if (.not. present(uvq_direct) .or. (present(uvq_direct) .and. (.not. uvq_direct))) then
         rh_error = iv%sonde_sfc(n)%q%error ! q error is rh at this stage!
         call da_get_q_error(iv % sonde_sfc(n) % p % inv, &
                              ob % sonde_sfc(n) % t, &
                              ob % sonde_sfc(n) % q, &
                              iv % sonde_sfc(n) % t % error, &
                              rh_error, iv % sonde_sfc(n) % q % error)
         if (iv%sonde_sfc(n)% q % error == missing_r) &
            iv%sonde_sfc(n)% q % qc = missing_data
         end if
      end do
   end if

   ! [2.8] Transfer airep obs:

   if (iv%info(airep)%nlocal > 0) then
      do n = 1, iv%info(airep)%nlocal
         do k = 1, iv%info(airep)%levels(n)
            ob % airep(n) % u(k) = iv % airep(n) % u(k) % inv
            ob % airep(n) % v(k) = iv % airep(n) % v(k) % inv
            ob % airep(n) % t(k) = iv % airep(n) % t(k) % inv
            ob % airep(n) % q(k) = iv % airep(n) % q(k) % inv

            if (.not. present(uvq_direct) .or. (present(uvq_direct) .and. (.not. uvq_direct))) then
                rh_error = iv%airep(n)%q(k)%error ! q error is rh at this stage!
                call da_get_q_error(iv % airep(n) % p(k), &
                                    ob % airep(n) % t(k), &
                                    ob % airep(n) % q(k), &
                                    iv % airep(n) % t(k) % error, &
                                    rh_error, q_error)

                iv % airep(n) % q(k) % error = q_error
                if (iv%airep(n)% q(k) % error == missing_r) &
                    iv%airep(n)% q(k) % qc = missing_data
            end if
         end do
      end do
   end if

   ! [2.9] Transfer pilot obs:

   if (iv%info(pilot)%nlocal > 0) then
      do n = 1, iv%info(pilot)%nlocal
         do k = 1, iv%info(pilot)%levels(n)
            ob % pilot(n) % u(k) = iv % pilot(n) % u(k) % inv
            ob % pilot(n) % v(k) = iv % pilot(n) % v(k) % inv
         end do
      end do
   end if

   ! [2.10] Transfer SSM/I obs:SSMI:

   if (iv%info(ssmi_rv)%nlocal > 0) then
      do n = 1, iv%info(ssmi_rv)%nlocal
         ob % ssmi_rv(n) % speed = iv % ssmi_rv(n) % speed % inv
         ob % ssmi_rv(n) % tpw   = iv % ssmi_rv(n) % tpw % inv
      end do
   end if

   if (iv%info(ssmi_tb)%nlocal > 0) then
      do n = 1, iv%info(ssmi_tb)%nlocal
         ob % ssmi_tb(n) % tb19v = iv % ssmi_tb(n) % tb19v % inv
         ob % ssmi_tb(n) % tb19h = iv % ssmi_tb(n) % tb19h % inv
         ob % ssmi_tb(n) % tb22v = iv % ssmi_tb(n) % tb22v % inv
         ob % ssmi_tb(n) % tb37v = iv % ssmi_tb(n) % tb37v % inv
         ob % ssmi_tb(n) % tb37h = iv % ssmi_tb(n) % tb37h % inv
         ob % ssmi_tb(n) % tb85v = iv % ssmi_tb(n) % tb85v % inv
         ob % ssmi_tb(n) % tb85h = iv % ssmi_tb(n) % tb85h % inv
      end do
   end if

   ! [2.11] Transfer satem obs:

   if (iv%info(satem)%nlocal > 0) then
      do n = 1, iv%info(satem)%nlocal
         do k = 1, iv%info(satem)%levels(n)
            ob % satem(n) % thickness(k) = iv % satem(n) % thickness(k) % inv
         end do
      end do
   end if
   
   ! [2.12] Transfer ssmt1 obs:

   if (iv%info(ssmt1)%nlocal > 0) then
      do n = 1, iv%info(ssmt1)%nlocal
         do k = 1, iv%info(ssmt1)%levels(n)
            ob % ssmt1(n) % t(k) = iv % ssmt1(n) % t(k) % inv
         end do
      end do

   end if

   ! [2.13] Transfer ssmt2 obs:

   if (iv%info(ssmt2)%nlocal > 0) then
      do n = 1, iv%info(ssmt2)%nlocal
         do k = 1, iv%info(ssmt2)%levels(n)
            ob % ssmt2(n) % rh(k) = iv % ssmt2(n) % rh(k) % inv
         end do
      end do
   end if

   ! [2.15] Transfer scatterometer obs:

   if (iv%info(qscat)%nlocal > 0) then
      do n = 1, iv%info(qscat)%nlocal
         ob % qscat(n) % u = iv % qscat(n) % u % inv
         ob % qscat(n) % v = iv % qscat(n) % v % inv
      end do     
   end if

   ! [2.16] Transfer profiler obs:

   if (iv%info(profiler)%nlocal > 0) then
      do n = 1, iv%info(profiler)%nlocal
         do k = 1, iv%info(profiler)%levels(n)
            ob % profiler(n) % u(k) = iv % profiler(n) % u(k) % inv
            ob % profiler(n) % v(k) = iv % profiler(n) % v(k) % inv
         end do
      end do
   end if

   ! [2.17] Transfer buoy obs:

   if (iv%info(buoy)%nlocal > 0) then
      do n = 1, iv%info(buoy)%nlocal
         ob % buoy(n) % p = iv % buoy(n) % p % inv
      end do
      do n = 1, iv%info(buoy)%nlocal
         ob % buoy(n) % u = iv % buoy(n) % u % inv
         ob % buoy(n) % v = iv % buoy(n) % v % inv
         ob % buoy(n) % t = iv % buoy(n) % t % inv
         ob % buoy(n) % q = iv % buoy(n) % q % inv

         ! Calculate q error from rh error:

         if (.not. present(uvq_direct) .or. (present(uvq_direct) .and. (.not. uvq_direct))) then
         rh_error = iv%buoy(n)%q%error ! q error is rh at this stage!
         call da_get_q_error(iv % buoy(n) % p % inv, &
                              ob % buoy(n) % t, &
                              ob % buoy(n) % q, &
                              iv % buoy(n) % t % error, &
                              rh_error, q_error)
         iv % buoy(n) % q % error = q_error

         if(iv%buoy (n)% q % error == missing_r) iv%buoy (n)% q % qc = missing_data
         end if
      end do
   end if

   ! [2.18] Transfer radar obs: this section has been moved to da_fill_obs_structures_radar.inc

!   if (iv%info(radar)%nlocal > 0) then
!      do n = 1, iv%info(radar)%nlocal
!         do k = 1, iv%info(radar)%levels(n)
!            ! Copy observation variables:
!            ob % radar(n) % rv(k) = iv % radar(n) % rv(k) % inv
!           ob % radar(n) % rf(k) = iv % radar(n) % rf(k) % inv
!         end do
!      end do
!   end if

   ! [2.19] Transfer TC bogus:

   if (iv%info(bogus)%nlocal > 0) then
      do n = 1, iv%info(bogus)%nlocal
         do k = 1, iv%info(bogus)%levels(n)

            ! Copy observation variables:

            ob % bogus(n) % u(k) = iv % bogus(n) % u(k) % inv
            ob % bogus(n) % v(k) = iv % bogus(n) % v(k) % inv
            ob % bogus(n) % t(k) = iv % bogus(n) % t(k) % inv
            ob % bogus(n) % q(k) = iv % bogus(n) % q(k) % inv

            ! Calculate q error from rh error:
            ! the conversion is not needed for LSAC
            if (iv%info(bogus)%name(n)(1:4) /= "LSAC") then
               rh_error = iv%bogus(n)%q(k)%error ! q error is rh at this stage!
               call da_get_q_error(iv % bogus(n) % p(k), &
                                   ob % bogus(n) % t(k), &
                                   ob % bogus(n) % q(k), &
                                   iv % bogus(n) % t(k) % error, &
                                   rh_error, q_error)

               iv % bogus(n) % q(k) % error = q_error
               if (iv%bogus(n)% q(k) % error == missing_r) &
                  iv%bogus(n)% q(k) % qc = missing_data
            end if !LSAC
         end do
         ob % bogus(n) % slp = iv % bogus(n) % slp % inv
      end do
   end if

   ! [2.20] Transfer rain obs:

   if (iv%info(rain)%nlocal > 0) then
      do n = 1, iv%info(rain)%nlocal
            ob % rain(n) % rain = iv % rain(n) % rain % inv
      end do
   end if

   ! Transfer AIRS  retrievals:

   if (iv%info(airsr)%nlocal > 0) then
      do n = 1, iv%info(airsr)%nlocal
         do k = 1, iv%info(airsr)%levels(n)

            ! Copy observation variables:

            ob % airsr(n) % t(k) = iv % airsr(n) % t(k) % inv
            ob % airsr(n) % q(k) = iv % airsr(n) % q(k) % inv

            ! Calculate q error from rh error:

         if (.not. present(uvq_direct) .or. (present(uvq_direct) .and. (.not. uvq_direct))) then
            rh_error = iv%airsr(n)%q(k)%error ! q error is rh at this stage!
            call da_get_q_error(iv % airsr(n) % p(k), &
                                 ob % airsr(n) % t(k), &
                                 ob % airsr(n) % q(k), &
                                 iv % airsr(n) % t(k) % error, &
                                 rh_error, q_error)

            iv % airsr(n) % q(k) % error = q_error
            if (iv%airsr(n)% q(k) % error == missing_r) &
               iv%airsr(n)% q(k) % qc = missing_data
         end if
         end do
      end do
   end if
   if (iv%info(mtgirs)%nlocal > 0) then
      do n = 1, iv%info(mtgirs)%nlocal
         do k = 1, iv%info(mtgirs)%levels(n)
            ob % mtgirs(n) % u(k) = iv % mtgirs(n) % u(k) % inv
            ob % mtgirs(n) % v(k) = iv % mtgirs(n) % v(k) % inv
            ob % mtgirs(n) % t(k) = iv % mtgirs(n) % t(k) % inv
            ob % mtgirs(n) % q(k) = iv % mtgirs(n) % q(k) % inv
         if (iv%mtgirs(n)% q(k) % error == missing_r) &
            iv%mtgirs(n)% q(k) % qc = missing_data
         end do
      end do
   end if

   if (iv%info(tamdar)%nlocal > 0) then
      do n = 1, iv%info(tamdar)%nlocal
         do k = 1, iv%info(tamdar)%levels(n)
            ob % tamdar(n) % u(k) = iv % tamdar(n) % u(k) % inv
            ob % tamdar(n) % v(k) = iv % tamdar(n) % v(k) % inv
            ob % tamdar(n) % t(k) = iv % tamdar(n) % t(k) % inv
            ob % tamdar(n) % q(k) = iv % tamdar(n) % q(k) % inv

         if (iv%tamdar(n)% u(k) % error == missing_r) &
            iv%tamdar(n)% u(k) % qc = missing_data
         if (iv%tamdar(n)% v(k) % error == missing_r) &
            iv%tamdar(n)% v(k) % qc = missing_data
         if (iv%tamdar(n)% t(k) % error == missing_r) &
            iv%tamdar(n)% t(k) % qc = missing_data

            ! Calculate q error from rh error:

            rh_error = iv%tamdar(n)%q(k)%error ! q error is rh at this stage!
            call da_get_q_error(iv % tamdar(n) % p(k), &
                                ob % tamdar(n) % t(k), &
                                ob % tamdar(n) % q(k), &
                                iv % tamdar(n) % t(k) % error, &
                                rh_error, q_error)

            iv % tamdar(n) % q(k) % error = q_error

         if (iv%tamdar(n)% q(k) % error == missing_r) &
            iv%tamdar(n)% q(k) % qc = missing_data
         end do
      end do
   end if

   if (iv%info(tamdar_sfc)%nlocal > 0) then
      do n = 1, iv%info(tamdar_sfc)%nlocal

         ob % tamdar_sfc(n) % u = iv % tamdar_sfc(n) % u % inv
         ob % tamdar_sfc(n) % v = iv % tamdar_sfc(n) % v % inv
         ob % tamdar_sfc(n) % t = iv % tamdar_sfc(n) % t % inv
         ob % tamdar_sfc(n) % q = iv % tamdar_sfc(n) % q % inv
         ob % tamdar_sfc(n) % p = iv % tamdar_sfc(n) % p % inv

         if (iv%tamdar_sfc(n)% u % error == missing_r) &
            iv%tamdar_sfc(n)% u % qc = missing_data
         if (iv%tamdar_sfc(n)% v % error == missing_r) &
            iv%tamdar_sfc(n)% v % qc = missing_data
         if (iv%tamdar_sfc(n)% t % error == missing_r) &
            iv%tamdar_sfc(n)% t % qc = missing_data

         ! Calculate q error from rh error:

         rh_error = iv%tamdar_sfc(n)%q%error ! q error is rh at this stage!
         call da_get_q_error(iv % tamdar_sfc(n) % p % inv, &
                              ob % tamdar_sfc(n) % t, &
                              ob % tamdar_sfc(n) % q, &
                              iv % tamdar_sfc(n) % t % error, &
                              rh_error, iv % tamdar_sfc(n) % q % error)
         if (iv%tamdar_sfc(n)% q % error == missing_r) &
            iv%tamdar_sfc(n)% q % qc = missing_data
      end do
   end if

   if (trace_use) call da_trace_exit("da_fill_obs_structures")

end subroutine da_fill_obs_structures


