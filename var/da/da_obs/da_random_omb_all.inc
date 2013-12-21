subroutine da_random_omb_all(iv, ob)

   !-------------------------------------------------------------------------
   ! Purpose: Allocates observation structure and fills it fro iv.
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout) :: iv   ! Obs and header structure.
   type (y_type), intent(inout)  :: ob   ! (Smaller) observation structure.

   integer                       :: n, k ! Loop counters.

   if (trace_use) call da_trace_entry("da_random_omb_all")

   !----------------------------------------------------------------------
   ! [1.0] Initialise random number sequence:
   !----------------------------------------------------------------------

   call da_random_seed
   
   !----------------------------------------------------------------------
   !  [2.0] Randomize each ob in turn:
   !----------------------------------------------------------------------

   ! [2.1] Transfer surface obs:

   if (iv%info(synop)%nlocal > 0) then
      do n = 1, iv%info(synop)%nlocal
         call da_random_omb(iv % synop(n) % u % error, ob % synop(n) % u, &
                           iv % synop(n) % u % qc, iv % synop(n) % u % inv)
         call da_random_omb(iv % synop(n) % v % error, ob % synop(n) % v, &
                           iv % synop(n) % v % qc, iv % synop(n) % v % inv)
         call da_random_omb(iv % synop(n) % t % error, ob % synop(n) % t, &
                           iv % synop(n) % t % qc, iv % synop(n) % t % inv )
         call da_random_omb(iv % synop(n) % p % error, ob % synop(n) % p, &
                           iv % synop(n) % p % qc, iv % synop(n) % p % inv)
         call da_random_omb(iv % synop(n) % q % error, ob % synop(n) % q, &
                           iv % synop(n) % q % qc, iv % synop(n) % q % inv)
      end do
   end if

   ! [2.2] Transfer metar obs:

   if (iv%info(metar)%nlocal > 0) then
      do n = 1, iv%info(metar)%nlocal
         call da_random_omb(iv % metar(n) % u % error, ob % metar(n) % u, &
                           iv % metar(n) % u % qc, iv % metar(n) % u % inv)
         call da_random_omb(iv % metar(n) % v % error, ob % metar(n) % v, &
                           iv % metar(n) % v % qc, iv % metar(n) % v % inv)
         call da_random_omb(iv % metar(n) % t % error, ob % metar(n) % t, &
                           iv % metar(n) % t % qc, iv % metar(n) % t % inv)
         call da_random_omb(iv % metar(n) % p % error, ob % metar(n) % p, &
                           iv % metar(n) % p % qc, iv % metar(n) % p % inv)
         call da_random_omb(iv % metar(n) % q % error, ob % metar(n) % q, &
                           iv % metar(n) % q % qc, iv % metar(n) % q % inv)
      end do
   end if

   ! [2.3] Transfer ships obs:

   if (iv%info(ships)%nlocal > 0) then
      do n = 1, iv%info(ships)%nlocal
         call da_random_omb(iv % ships(n) % u % error, ob % ships(n) % u, &
                           iv % ships(n) % u % qc, iv % ships(n) % u % inv)
         call da_random_omb(iv % ships(n) % v % error, ob % ships(n) % v, &
                           iv % ships(n) % v % qc, iv % ships(n) % v % inv)
         call da_random_omb(iv % ships(n) % t % error, ob % ships(n) % t, &
                           iv % ships(n) % t % qc, iv % ships(n) % t % inv)
         call da_random_omb(iv % ships(n) % p % error, ob % ships(n) % p, &
                           iv % ships(n) % p % qc, iv % ships(n) % p % inv)
         call da_random_omb(iv % ships(n) % q % error, ob % ships(n) % q, &
                           iv % ships(n) % q % qc, iv % ships(n) % q % inv)
      end do
   end if

   ! [2.4.1] Transfer Geo. AMVs Obs:

   if (iv%info(geoamv)%nlocal > 0) then
      do n = 1, iv%info(geoamv)%nlocal 
        do k = 1, iv%info(geoamv)%levels(n)
         call da_random_omb(iv % geoamv(n) % u(k) % error, ob % geoamv(n) % u(k), &
                           iv % geoamv(n) % u(k) % qc, iv % geoamv(n) % u(k) % inv)
         call da_random_omb(iv % geoamv(n) % v(k) % error, ob % geoamv(n) % v(k), &
                           iv % geoamv(n) % v(k) % qc, iv % geoamv(n) % v(k) % inv)
        end do
      end do 
   end if 

   ! [2.4.2] Transfer Polar  AMVs Obs:

   if (iv%info(polaramv)%nlocal > 0) then
      do n = 1, iv%info(polaramv)%nlocal
        do k = 1, iv%info(polaramv)%levels(n)
         call da_random_omb(iv % polaramv(n) % u(k) % error, ob % polaramv(n) % u(k), &
                           iv % polaramv(n) % u(k) % qc, iv % polaramv(n) % u(k) % inv)
         call da_random_omb(iv % polaramv(n) % v(k) % error, ob % polaramv(n) % v(k), &
                           iv % polaramv(n) % v(k) % qc, iv % polaramv(n) % v(k) % inv)
        end do
      end do
   end if

   ! [2.5] Transfer gpspw obs:

   if (iv%info(gpspw)%nlocal > 0) then
      do n = 1, iv%info(gpspw)%nlocal
         call da_random_omb(iv % gpspw(n) % tpw % error, ob % gpspw(n) % tpw, &
                           iv % gpspw(n) % tpw % qc, iv % gpspw(n) % tpw % inv)
      end do
   end if

   ! [2.6] Transfer sonde obs:

   if (iv%info(sound)%nlocal > 0) then
      do n = 1, iv%info(sound)%nlocal
         do k = 1, iv%info(sound)%levels(n)
            call da_random_omb(iv % sound(n) % u(k) % error, ob % sound(n) % u(k), &
                              iv % sound(n) % u(k) % qc, iv % sound(n) % u(k) % inv)
            call da_random_omb(iv % sound(n) % v(k) % error, ob % sound(n) % v(k), &
                              iv % sound(n) % v(k) % qc, iv % sound(n) % v(k) % inv)
            call da_random_omb(iv % sound(n) % t(k) % error, ob % sound(n) % t(k), &
                              iv % sound(n) % t(k) % qc, iv % sound(n) % t(k) % inv)
            call da_random_omb(iv % sound(n) % q(k) % error, ob % sound(n) % q(k), &
                              iv % sound(n) % q(k) % qc, iv % sound(n) % q(k) % inv)
         end do
      end do
   end if

   if (iv%info(sonde_sfc)%nlocal > 0) then
      do n = 1, iv%info(sonde_sfc)%nlocal
         call da_random_omb(iv % sonde_sfc(n) % u % error, ob % sonde_sfc(n) % u, &
                             iv % sonde_sfc(n) % u % qc, iv % sonde_sfc(n) % u % inv)
         call da_random_omb(iv % sonde_sfc(n) % v % error, ob % sonde_sfc(n) % v, &
                             iv % sonde_sfc(n) % v % qc, iv % sonde_sfc(n) % v % inv)
         call da_random_omb(iv % sonde_sfc(n) % t % error, ob % sonde_sfc(n) % t, &
                             iv % sonde_sfc(n) % t % qc, iv % sonde_sfc(n) % t % inv )
         call da_random_omb(iv % sonde_sfc(n) % p % error, ob % sonde_sfc(n) % p, &
                             iv % sonde_sfc(n) % p % qc, iv % sonde_sfc(n) % p % inv)
         call da_random_omb(iv % sonde_sfc(n) % q % error, ob % sonde_sfc(n) % q, &
                             iv % sonde_sfc(n) % q % qc, iv % sonde_sfc(n) % q % inv)
      end do
   end if

   ! [2.7] Transfer airep obs:

   if (iv%info(airep)%nlocal > 0) then
      do n = 1, iv%info(airep)%nlocal
         do k = 1, iv%info(airep)%levels(n)
            call da_random_omb(iv % airep(n) % u(k) % error, ob % airep(n) % u(k), &
                              iv % airep(n) % u(k) % qc, iv % airep(n) % u(k) % inv)
            call da_random_omb(iv % airep(n) % v(k) % error, ob % airep(n) % v(k), &
                              iv % airep(n) % v(k) % qc, iv % airep(n) % v(k) % inv)
            call da_random_omb(iv % airep(n) % t(k) % error, ob % airep(n) % t(k), &
                              iv % airep(n) % t(k) % qc, iv % airep(n) % t(k) % inv)
            call da_random_omb(iv % airep(n) % q(k) % error, ob % airep(n) % q(k), &
                              iv % airep(n) % q(k) % qc, iv % airep(n) % q(k) % inv)
         end do
      end do
   end if

   ! [2.8] Transfer pilot obs:

   if (iv%info(pilot)%nlocal > 0) then
      do n = 1, iv%info(pilot)%nlocal
         do k = 1, iv%info(pilot)%levels(n)
            call da_random_omb(iv % pilot(n) % u(k) % error, ob % pilot(n) % u(k), &
                              iv % pilot(n) % u(k) % qc, iv % pilot(n) % u(k) % inv)
            call da_random_omb(iv % pilot(n) % v(k) % error, ob % pilot(n) % v(k), &
                              iv % pilot(n) % v(k) % qc, iv % pilot(n) % v(k) % inv)
         end do
      end do
   end if

   ! [2.9] Transfer SSM/I obs:SSMI:

   if (iv%info(ssmi_rv)%nlocal > 0) then
      do n = 1, iv%info(ssmi_rv)%nlocal
         call da_random_omb(iv % ssmi_rv(n) % speed % error, &
                           ob % ssmi_rv(n) % speed, &
                           iv % ssmi_rv(n) % speed % qc, &
                           iv % ssmi_rv(n) % speed % inv)
         call da_random_omb(iv % ssmi_rv(n) % tpw % error, &
                           ob % ssmi_rv(n) % tpw, &
                           iv % ssmi_rv(n) % tpw % qc, &
                           iv % ssmi_rv(n) % tpw % inv)
      end do
   end if

   if (iv%info(ssmi_tb)%nlocal > 0) then
      do n = 1, iv%info(ssmi_tb)%nlocal
         call da_random_omb(iv % ssmi_tb(n) % tb19h % error, &
                           ob % ssmi_tb(n) % tb19h, &
                           iv % ssmi_tb(n) % tb19h % qc, &
                           iv % ssmi_tb(n) % tb19h % inv)
         call da_random_omb(iv % ssmi_tb(n) % tb19v % error, &
                           ob % ssmi_tb(n) % tb19v, &
                           iv % ssmi_tb(n) % tb19v % qc, &
                           iv % ssmi_tb(n) % tb19v % inv)
         call da_random_omb(iv % ssmi_tb(n) % tb22v % error, &
                           ob % ssmi_tb(n) % tb22v, &
                           iv % ssmi_tb(n) % tb22v % qc, &
                           iv % ssmi_tb(n) % tb22v % inv)
         call da_random_omb(iv % ssmi_tb(n) % tb37h % error, &
                           ob % ssmi_tb(n) % tb37h, &
                           iv % ssmi_tb(n) % tb37h % qc, &
                           iv % ssmi_tb(n) % tb37h % inv)
         call da_random_omb(iv % ssmi_tb(n) % tb37v % error, &
                           ob % ssmi_tb(n) % tb37v, &
                           iv % ssmi_tb(n) % tb37v % qc, &
                           iv % ssmi_tb(n) % tb37v % inv)
         call da_random_omb(iv % ssmi_tb(n) % tb85h % error, &
                           ob % ssmi_tb(n) % tb85h, &
                           iv % ssmi_tb(n) % tb85h % qc, &
                           iv % ssmi_tb(n) % tb85h % inv)
         call da_random_omb(iv % ssmi_tb(n) % tb85v % error, &
                           ob % ssmi_tb(n) % tb85v, &
                           iv % ssmi_tb(n) % tb85v % qc, &
                           iv % ssmi_tb(n) % tb85v % inv)
      end do
   end if

   ! [2.10] Transfer satem obs:

   if (iv%info(satem)%nlocal > 0) then
      do n = 1, iv%info(satem)%nlocal
         do k = 1, iv%info(satem)%levels(n)
            call da_random_omb(iv % satem(n) % thickness(k) % error, &
                              ob % satem(n) % thickness(k), &
                              iv % satem(n) % thickness(k) % qc, &
                              iv % satem(n) % thickness(k) % inv)
         end do
      end do
   end if
   
   ! [2.11] Transfer ssmt1 obs:

   if (iv%info(ssmt1)%nlocal > 0) then
      do n = 1, iv%info(ssmt1)%nlocal
         do k = 1, iv%info(ssmt1)%levels(n)
            call da_random_omb(iv % ssmt1(n) % t(k) % error, &
                              ob % ssmt1(n) % t(k), &
                              iv % ssmt1(n) % t(k) % qc, &
                              iv % ssmt1(n) % t(k) % inv)
         end do
      end do
   end if

   ! [2.12] Transfer ssmt2 obs:

   if (iv%info(ssmt2)%nlocal > 0) then
      do n = 1, iv%info(ssmt2)%nlocal
         do k = 1, iv%info(ssmt2)%levels(n)
            call da_random_omb(iv % ssmt2(n) % rh(k) % error, &
                              ob % ssmt2(n) % rh(k), &
                              iv % ssmt2(n) % rh(k) % qc, &
                              iv % ssmt2(n) % rh(k) % inv)
         end do
      end do
   end if
   
   ! [2.13] Transfer scatterometer obs:

   if (iv%info(qscat)%nlocal > 0) then
      do n = 1, iv%info(qscat)%nlocal
         call da_random_omb(iv % qscat(n) % u % error, ob % qscat(n) % u, &
                           iv % qscat(n) % u % qc, iv % qscat(n) % u % inv)
         call da_random_omb(iv % qscat(n) % v % error, ob % qscat(n) % v, &
                           iv % qscat(n) % v % qc, iv % qscat(n) % v % inv)
      end do
   end if

   ! [2.14] Transfer buoy obs:

   if (iv%info(buoy)%nlocal > 0) then
      do n = 1, iv%info(buoy)%nlocal
         call da_random_omb(iv % buoy(n) % u % error, ob % buoy(n) % u, &
                           iv % buoy(n) % u % qc, iv % buoy(n) % u % inv)
         call da_random_omb(iv % buoy(n) % v % error, ob % buoy(n) % v, &
                           iv % buoy(n) % v % qc, iv % buoy(n) % v % inv)
         call da_random_omb(iv % buoy(n) % t % error, ob % buoy(n) % t, &
                           iv % buoy(n) % t % qc, iv % buoy(n) % t % inv)
         call da_random_omb(iv % buoy(n) % p % error, ob % buoy(n) % p, &
                           iv % buoy(n) % p % qc, iv % buoy(n) % p % inv)
         call da_random_omb(iv % buoy(n) % q % error, ob % buoy(n) % q, &
                           iv % buoy(n) % q % qc, iv % buoy(n) % q % inv)
      end do
   end if

   ! [2.15] Transfer profiler obs:

   if (iv%info(profiler)%nlocal > 0) then
      do n = 1, iv%info(profiler)%nlocal
         do k = 1, iv%info(profiler)%levels(n)
            call da_random_omb(iv % profiler(n) % u(k) % error, &
               ob % profiler(n) % u(k), &
               iv % profiler(n) % u(k) % qc, iv % profiler(n) % u(k) % inv)
            call da_random_omb(iv % profiler(n) % v(k) % error, &
               ob % profiler(n) % v(k), &
               iv % profiler(n) % v(k) % qc, iv % profiler(n) % v(k) % inv)
         end do
      end do
   end if

   ! [2.16] Transfer TC bogus obs:

   if (iv%info(bogus)%nlocal > 0) then
      do n = 1, iv%info(bogus)%nlocal
         do k = 1, iv%info(bogus)%levels(n)
            call da_random_omb(iv % bogus(n) % u(k) % error, &
               ob % bogus(n) % u(k), &
               iv % bogus(n) % u(k) % qc, iv % bogus(n) % u(k) % inv)
            call da_random_omb(iv % bogus(n) % v(k) % error, &
               ob % bogus(n) % v(k), &
               iv % bogus(n) % v(k) % qc, iv % bogus(n) % v(k) % inv)
            call da_random_omb(iv % bogus(n) % t(k) % error, &
               ob % bogus(n) % t(k), &
               iv % bogus(n) % t(k) % qc, iv % bogus(n) % t(k) % inv)
            call da_random_omb(iv % bogus(n) % q(k) % error, &
              ob % bogus(n) % q(k), &
              iv % bogus(n) % q(k) % qc, iv % bogus(n) % q(k) % inv)
         end do

         call da_random_omb(iv % bogus(n) % slp % error, ob % bogus(n) % slp, &
                            iv % bogus(n) % slp % qc, iv % bogus(n) % slp % inv)
      end do
   end if

   ! Transfer AIRS retrievals:

   if (iv%info(airsr)%nlocal > 0) then
      do n = 1, iv%info(airsr)%nlocal
         do k = 1, iv%info(airsr)%levels(n)
            call da_random_omb(iv % airsr(n) % t(k) % error, &
               ob % airsr(n) % t(k), &
               iv % airsr(n) % t(k) % qc, iv % airsr(n) % t(k) % inv)
            call da_random_omb(iv % airsr(n) % q(k) % error, &
               ob % airsr(n) % q(k), &
               iv % airsr(n) % q(k) % qc, iv % airsr(n) % q(k) % inv)
         end do
      end do
   end if

   if (iv%info(mtgirs)%nlocal > 0) then
      do n = 1, iv%info(mtgirs)%nlocal
         do k = 1, iv%info(mtgirs)%levels(n)
            call da_random_omb(iv % mtgirs(n) % u(k) % error, ob %  mtgirs(n) % u(k), &
                              iv %  mtgirs(n) % u(k) % qc, iv %  mtgirs(n) % u(k) % inv)
            call da_random_omb(iv %  mtgirs(n) % v(k) % error, ob %  mtgirs(n) % v(k), &
                              iv %  mtgirs(n) % v(k) % qc, iv %  mtgirs(n) % v(k) % inv)
            call da_random_omb(iv %  mtgirs(n) % t(k) % error, ob %  mtgirs(n) % t(k), &
                              iv %  mtgirs(n) % t(k) % qc, iv %  mtgirs(n) % t(k) % inv)
            call da_random_omb(iv %  mtgirs(n) % q(k) % error, ob %  mtgirs(n) % q(k), &
                              iv %  mtgirs(n) % q(k) % qc, iv %  mtgirs(n) % q(k) % inv)
         end do
      end do
    end if

   if (iv%info(tamdar)%nlocal > 0) then
      do n = 1, iv%info(tamdar)%nlocal
         do k = 1, iv%info(tamdar)%levels(n)
            call da_random_omb(iv % tamdar(n) % u(k) % error, ob %  tamdar(n) % u(k), &
                              iv %  tamdar(n) % u(k) % qc, iv %  tamdar(n) % u(k) % inv)
            call da_random_omb(iv %  tamdar(n) % v(k) % error, ob %  tamdar(n) % v(k), &
                              iv %  tamdar(n) % v(k) % qc, iv %  tamdar(n) % v(k) % inv)
            call da_random_omb(iv %  tamdar(n) % t(k) % error, ob %  tamdar(n) % t(k), &
                              iv %  tamdar(n) % t(k) % qc, iv %  tamdar(n) % t(k) % inv)
            call da_random_omb(iv %  tamdar(n) % q(k) % error, ob %  tamdar(n) % q(k), &
                              iv %  tamdar(n) % q(k) % qc, iv %  tamdar(n) % q(k) % inv)
         end do
      end do
   end if

   if (iv%info(tamdar_sfc)%nlocal > 0) then
      do n = 1, iv%info(tamdar_sfc)%nlocal
         call da_random_omb(iv % tamdar_sfc(n) % u % error, ob % tamdar_sfc(n) % u, &
                             iv % tamdar_sfc(n) % u % qc, iv % tamdar_sfc(n) % u % inv)
         call da_random_omb(iv % tamdar_sfc(n) % v % error, ob % tamdar_sfc(n) % v, &
                             iv % tamdar_sfc(n) % v % qc, iv % tamdar_sfc(n) % v % inv)
         call da_random_omb(iv % tamdar_sfc(n) % t % error, ob % tamdar_sfc(n) % t, &
                             iv % tamdar_sfc(n) % t % qc, iv % tamdar_sfc(n) % t % inv )
         call da_random_omb(iv % tamdar_sfc(n) % p % error, ob % tamdar_sfc(n) % p, &
                             iv % tamdar_sfc(n) % p % qc, iv % tamdar_sfc(n) % p % inv)
         call da_random_omb(iv % tamdar_sfc(n) % q % error, ob % tamdar_sfc(n) % q, &
                             iv % tamdar_sfc(n) % q % qc, iv % tamdar_sfc(n) % q % inv)

      end do
    end if

   if (trace_use) call da_trace_exit("da_random_omb_all")

end subroutine da_random_omb_all


