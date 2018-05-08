subroutine da_allocate_y (iv, y)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate arrays used in y and residual obs structures.
   !---------------------------------------------------------------------------

   implicit none
   
   type (iv_type), intent(in)    :: iv      ! Ob type input.
   type (y_type),  intent(inout) :: y       ! Residual type structure.

   integer :: n, i    ! Loop counter.
   integer :: nlevels ! Number of levels.

   !---------------------------------------------------------------------------
   !  [1.0] Copy number of observations:
   !---------------------------------------------------------------------------

   if (trace_use) call da_trace_entry("da_allocate_y")

   y % nlocal(:) = iv%info(:)%nlocal
   y % ntotal(:) = iv%info(:)%ntotal

   y % num_inst     = iv % num_inst

  !---------------------------------------------------------------------------
  ! [2.0] Allocate:
  !---------------------------------------------------------------------------

   if (y % nlocal(synop) > 0) then
      allocate (y % synop(1:y % nlocal(synop)))
      y % synop(1:y % nlocal(synop)) % u = 0.0
      y % synop(1:y % nlocal(synop)) % v = 0.0
      y % synop(1:y % nlocal(synop)) % t = 0.0
      y % synop(1:y % nlocal(synop)) % p = 0.0
      y % synop(1:y % nlocal(synop)) % q = 0.0
   end if

   if (y % nlocal(ships) > 0) then
      allocate (y % ships(1:y % nlocal(ships)))
      y % ships(1:y % nlocal(ships)) % u = 0.0
      y % ships(1:y % nlocal(ships)) % v = 0.0
      y % ships(1:y % nlocal(ships)) % t = 0.0
      y % ships(1:y % nlocal(ships)) % p = 0.0
      y % ships(1:y % nlocal(ships)) % q = 0.0
   end if

   if (y % nlocal(metar) > 0) then
      allocate (y % metar(1:y % nlocal(metar)))
      y % metar(1:y % nlocal(metar)) % u = 0.0
      y % metar(1:y % nlocal(metar)) % v = 0.0
      y % metar(1:y % nlocal(metar)) % t = 0.0
      y % metar(1:y % nlocal(metar)) % p = 0.0
      y % metar(1:y % nlocal(metar)) % q = 0.0
   end if

   if (y % nlocal(geoamv) > 0) then
      allocate (y % geoamv(1:y % nlocal(geoamv)))
      do n = 1, y % nlocal(geoamv)
         nlevels = iv%info(geoamv)%levels(n)
         allocate (y % geoamv(n)%u(1:nlevels))
         allocate (y % geoamv(n)%v(1:nlevels))
         y % geoamv(n) % u(1:nlevels) = 0.0
         y % geoamv(n) % v(1:nlevels) = 0.0
      end do
   end if

   if (y % nlocal(polaramv) > 0) then
      allocate (y % polaramv(1:y % nlocal(polaramv)))
      do n = 1, y % nlocal(polaramv)
         nlevels = iv%info(polaramv)%levels(n)
         allocate (y % polaramv(n)%u(1:nlevels))
         allocate (y % polaramv(n)%v(1:nlevels))
         y % polaramv(n) % u(1:nlevels) = 0.0
         y % polaramv(n) % v(1:nlevels) = 0.0
      end do
   end if
 
   if (y % nlocal(gpspw) > 0) then
      allocate (y % gpspw(1:y % nlocal(gpspw)))
      y % gpspw(1:y % nlocal(gpspw)) % tpw = 0.0
   end if

   if (y % nlocal(gpsref) > 0) then
      allocate (y % gpsref(1:y % nlocal(gpsref)))
      do n = 1, y % nlocal(gpsref)
         nlevels = iv%info(gpsref)%levels(n)
         allocate (y % gpsref(n)%ref(1:nlevels))
         allocate (y % gpsref(n)%  p(1:nlevels))
         allocate (y % gpsref(n)%  t(1:nlevels))
         allocate (y % gpsref(n)%  q(1:nlevels))

         y % gpsref(n) % ref(1:nlevels) = 0.0
         y % gpsref(n) %   p(1:nlevels) = 0.0
         y % gpsref(n) %   t(1:nlevels) = 0.0
         y % gpsref(n) %   q(1:nlevels) = 0.0
      end do
   end if

   if (y % nlocal(gpseph) > 0) then
      allocate (y % gpseph(1:y % nlocal(gpseph)))
      do n = 1, y % nlocal(gpseph)
         nlevels = iv%info(gpseph)%levels(n)
         if ( nlevels > 0 ) then
            allocate (y % gpseph(n)%eph(1:nlevels))
            y % gpseph(n) % eph(1:nlevels) = 0.0
         end if
      end do
   end if

   if (y % nlocal(sound) > 0) then
      allocate (y % sound(1:y % nlocal(sound)))
      do n = 1, y % nlocal(sound)
         nlevels = max(1,iv%info(sound)%levels(n))
         allocate (y % sound(n)%u(1:nlevels))
         allocate (y % sound(n)%v(1:nlevels))
         allocate (y % sound(n)%t(1:nlevels))
         allocate (y % sound(n)%q(1:nlevels))
         y % sound(n) % u(1:nlevels) = 0.0
         y % sound(n) % v(1:nlevels) = 0.0
         y % sound(n) % t(1:nlevels) = 0.0
         y % sound(n) % q(1:nlevels) = 0.0
      end do
   end if

   if (y % nlocal(sonde_sfc) > 0) then
      allocate (y % sonde_sfc(1:y % nlocal(sonde_sfc)))

      y % sonde_sfc(1:y % nlocal(sonde_sfc)) % u = 0.0
      y % sonde_sfc(1:y % nlocal(sonde_sfc)) % v = 0.0
      y % sonde_sfc(1:y % nlocal(sonde_sfc)) % t = 0.0
      y % sonde_sfc(1:y % nlocal(sonde_sfc)) % p = 0.0
      y % sonde_sfc(1:y % nlocal(sonde_sfc)) % q = 0.0
   end if
     
   if (y % nlocal(mtgirs) > 0) then
      allocate (y % mtgirs(1:y % nlocal(mtgirs)))
      do n = 1, y % nlocal(mtgirs)
         nlevels = max(1,iv%info(mtgirs)%levels(n))
         allocate (y % mtgirs(n)%u(1:nlevels))
         allocate (y % mtgirs(n)%v(1:nlevels))
         allocate (y % mtgirs(n)%t(1:nlevels))
         allocate (y % mtgirs(n)%q(1:nlevels))
         y % mtgirs(n) % u(1:nlevels) = 0.0
         y % mtgirs(n) % v(1:nlevels) = 0.0
         y % mtgirs(n) % t(1:nlevels) = 0.0
         y % mtgirs(n) % q(1:nlevels) = 0.0
      end do
   end if

   if (y % nlocal(tamdar) > 0) then
      allocate (y % tamdar(1:y % nlocal(tamdar)))
      do n = 1, y % nlocal(tamdar)
         nlevels = max(1,iv%info(tamdar)%levels(n))
         allocate (y % tamdar(n)%u(1:nlevels))
         allocate (y % tamdar(n)%v(1:nlevels))
         allocate (y % tamdar(n)%t(1:nlevels))
         allocate (y % tamdar(n)%q(1:nlevels))
         y % tamdar(n) % u(1:nlevels) = 0.0
         y % tamdar(n) % v(1:nlevels) = 0.0
         y % tamdar(n) % t(1:nlevels) = 0.0
         y % tamdar(n) % q(1:nlevels) = 0.0
      end do
   end if
   if (y % nlocal(tamdar_sfc) > 0) then
      allocate (y % tamdar_sfc(1:y % nlocal(tamdar_sfc)))

      y % tamdar_sfc(1:y % nlocal(tamdar_sfc)) % u = 0.0
      y % tamdar_sfc(1:y % nlocal(tamdar_sfc)) % v = 0.0
      y % tamdar_sfc(1:y % nlocal(tamdar_sfc)) % t = 0.0
      y % tamdar_sfc(1:y % nlocal(tamdar_sfc)) % p = 0.0
      y % tamdar_sfc(1:y % nlocal(tamdar_sfc)) % q = 0.
   end if

   if (y % nlocal(pilot) > 0) then
      allocate (y % pilot(1:y % nlocal(pilot)))
      do n = 1, y % nlocal(pilot)
         nlevels = iv%info(pilot)%levels(n)
         allocate (y % pilot(n)%u(1:nlevels))
         allocate (y % pilot(n)%v(1:nlevels))
         y % pilot(n) % u(1:nlevels) = 0.0
         y % pilot(n) % v(1:nlevels) = 0.0
      end do
   end if

   if (y % nlocal(radar) > 0) then
      allocate (y % radar(1:y % nlocal(radar)))
      do n = 1, y % nlocal(radar)
         nlevels = iv%info(radar)%levels(n)
         allocate (y % radar(n)%rv(1:nlevels))
         allocate (y % radar(n)%rf(1:nlevels))
         allocate (y % radar(n)%rrn(1:nlevels))
         allocate (y % radar(n)%rsn(1:nlevels))
         allocate (y % radar(n)%rgr(1:nlevels))
         allocate (y % radar(n)%rqv(1:nlevels))

         y % radar(n) % rv(1:nlevels)  = 0.0
         y % radar(n) % rf(1:nlevels)  = 0.0
         y % radar(n) % rrn(1:nlevels) = 0.0
         y % radar(n) % rsn(1:nlevels) = 0.0
         y % radar(n) % rgr(1:nlevels) = 0.0
         y % radar(n) % rqv(1:nlevels) = 0.0
      end do
   end if

   if (y % nlocal(airep) > 0) then
      allocate (y % airep(1:y % nlocal(airep)))
      do n = 1, y % nlocal(airep)
         nlevels = iv%info(airep)%levels(n)
         allocate (y % airep(n)%u(1:nlevels))
         allocate (y % airep(n)%v(1:nlevels))
         allocate (y % airep(n)%t(1:nlevels))
         allocate (y % airep(n)%q(1:nlevels))
         y % airep(n) % u(1:nlevels) = 0.0
         y % airep(n) % v(1:nlevels) = 0.0
         y % airep(n) % t(1:nlevels) = 0.0
         y % airep(n) % q(1:nlevels) = 0.0
      end do
   end if

   if (y % nlocal(bogus) > 0) then
      allocate (y % bogus(1:y % nlocal(bogus)))
      do n = 1, y % nlocal(bogus)
         nlevels = iv%info(bogus)%levels(n)
         allocate (y % bogus(n)%u(1:nlevels))
         allocate (y % bogus(n)%v(1:nlevels))
         allocate (y % bogus(n)%t(1:nlevels))
         allocate (y % bogus(n)%q(1:nlevels))
         y % bogus(n) % u(1:nlevels) = 0.0
         y % bogus(n) % v(1:nlevels) = 0.0
         y % bogus(n) % t(1:nlevels) = 0.0
         y % bogus(n) % q(1:nlevels) = 0.0
      end do

      y % bogus(1:y % nlocal(bogus)) % slp = 0.0
   end if

   if (y % nlocal(satem) > 0) then
      allocate (y % satem(1:y % nlocal(satem)))
      do n = 1, y % nlocal(satem)
         nlevels = iv%info(satem)%levels(n)
         allocate (y % satem(n) % thickness(1:nlevels))
         y % satem(n) % thickness(1:nlevels) = 0.0
      end do
   end if

   if (y % nlocal(ssmi_tb) > 0) then
      allocate (y % ssmi_tb(1:y % nlocal(ssmi_tb)))
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb19v = 0.0
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb19h = 0.0
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb22v = 0.0
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb37v = 0.0
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb37h = 0.0
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb85v = 0.0
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb85h = 0.0
   end if

   if (y % nlocal(ssmi_rv) > 0) then
        allocate (y % ssmi_rv(1:y % nlocal(ssmi_rv)))
        y % ssmi_rv(1:y % nlocal(ssmi_rv)) % tpw = 0.0
        y % ssmi_rv(1:y % nlocal(ssmi_rv)) % Speed = 0.0
   end if
   
   if (y % nlocal(ssmt1) > 0) then
      allocate (y % ssmt1(1:y % nlocal(ssmt1)))
      do n = 1, y % nlocal(ssmt1)
         nlevels = iv%info(ssmt1)%levels(n)
         allocate (y % ssmt1(n) % t(1:nlevels))
         y % ssmt1(n) % t(1:nlevels) = 0.0
      end do
   end if
   
   if (y % nlocal(ssmt2) > 0) then
      allocate (y % ssmt2(1:y % nlocal(ssmt2)))
      do n = 1, y % nlocal(ssmt2)
         nlevels=iv%info(ssmt2)%levels(n)
         allocate (y % ssmt2(n) % rh(1:nlevels))
         y % ssmt2(n) % rh(1:nlevels) = 0.0
      end do
   end if
   
   if (y % nlocal(pseudo) > 0) then
        allocate (y % pseudo(1:y % nlocal(pseudo)))
        y % pseudo(1:y % nlocal(pseudo)) % u = 0.0
        y % pseudo(1:y % nlocal(pseudo)) % v = 0.0
        y % pseudo(1:y % nlocal(pseudo)) % t = 0.0
        y % pseudo(1:y % nlocal(pseudo)) % p = 0.0
        y % pseudo(1:y % nlocal(pseudo)) % q = 0.0
   end if

   if (y % nlocal(qscat) > 0) then
      allocate (y % qscat(1:y % nlocal(qscat)))
      y % qscat(1:y % nlocal(qscat)) % u = 0.0
      y % qscat(1:y % nlocal(qscat)) % v = 0.0
   end if
      
   if (y % nlocal(profiler) > 0) then
      allocate (y % profiler(1:y % nlocal(profiler)))
      do n = 1, y % nlocal(profiler)
         nlevels = iv%info(profiler)%levels(n)
         allocate (y % profiler(n)%u(1:nlevels))
         allocate (y % profiler(n)%v(1:nlevels))
         y % profiler(n) % u(1:nlevels) = 0.0
         y % profiler(n) % v(1:nlevels) = 0.0
      end do
   end if

   if (y % nlocal(buoy) > 0) then
      allocate (y % buoy(1:y % nlocal(buoy)))
      y % buoy(1:y % nlocal(buoy)) % u = 0.0
      y % buoy(1:y % nlocal(buoy)) % v = 0.0
      y % buoy(1:y % nlocal(buoy)) % t = 0.0
      y % buoy(1:y % nlocal(buoy)) % p = 0.0
      y % buoy(1:y % nlocal(buoy)) % q = 0.0
   end if

   if (y % nlocal(rain) > 0) then
      allocate (y % rain(1:y % nlocal(rain)))
      y % rain(1:y % nlocal(rain)) % rain = 0.0
   end if

   if (y % num_inst > 0) then
      allocate (y % instid(1:y % num_inst))
      do i = 1,  y % num_inst
         y % instid(i) % num_rad = iv % instid(i) % num_rad
         y % instid(i) % nchan   = iv % instid(i) % nchan
         ! allocate (y % instid(i) % ichan(1:y % instid(i) % nchan))
         ! do n = 1, y % instid(i) % nchan
         !     y % instid(i) % ichan(n) = n
         ! end do
         if (y % instid(i) % num_rad < 1)  then
            nullify (y % instid(i) % tb)
            cycle
         end if
         allocate (y % instid(i) % tb(1:y % instid(i) % nchan, y % instid(i) % num_rad))
         y % instid(i) % tb(:,:) = 0.0
      end do
   end if

   if (y % nlocal(airsr) > 0) then
      allocate (y % airsr(1:y % nlocal(airsr)))
      do n = 1, y % nlocal(airsr)
         nlevels = iv%info(airsr)%levels(n)
         allocate (y % airsr(n)%t(1:nlevels))
         allocate (y % airsr(n)%q(1:nlevels))
         y % airsr(n) % t(1:nlevels) = 0.0
         y % airsr(n) % q(1:nlevels) = 0.0
      end do
   end if

   if (trace_use) call da_trace_exit("da_allocate_y")

end subroutine da_allocate_y


