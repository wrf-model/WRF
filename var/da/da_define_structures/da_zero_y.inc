subroutine da_zero_y( iv, y, value )

   !---------------------------------------------------------------------------
   ! Purpose: Initialises the Y-array
   !---------------------------------------------------------------------------

   implicit none
   
   type (iv_type), intent(in)            :: iv      ! Ob type input.
   type (y_type),  intent(inout)         :: y       ! Residual type structure.
   real, optional, intent(inout)         :: value

   integer                               :: n       ! Loop counter.
   integer                               :: nlevels ! Number of levels.

   if (trace_use_dull) call da_trace_entry("da_zero_y")
 
   if (.not.(present(value))) value = 0.0
   !---------------------------------------------------------------------------
   ! [1.0] Copy number of observations:
   !---------------------------------------------------------------------------

   y % nlocal(:) = iv % info(:) % nlocal

   !---------------------------------------------------------------------------
   ! [2.0] Allocate:
   !---------------------------------------------------------------------------

   ! Initialize synops:

   if ( y % nlocal(synop) > 0 ) then
      y % synop(1:y % nlocal(synop)) % u = value
      y % synop(1:y % nlocal(synop)) % v = value
      y % synop(1:y % nlocal(synop)) % t = value
      y % synop(1:y % nlocal(synop)) % p = value
      y % synop(1:y % nlocal(synop)) % q = value
   end if

   ! Initialize ships:

   if ( y % nlocal(ships) > 0 ) then
      y % ships(1:y % nlocal(ships)) % u = value
      y % ships(1:y % nlocal(ships)) % v = value
      y % ships(1:y % nlocal(ships)) % t = value
      y % ships(1:y % nlocal(ships)) % p = value
      y % ships(1:y % nlocal(ships)) % q = value
   end if

   ! Initialize metars:

   if ( y % nlocal(metar) > 0 ) then
      y % metar(1:y % nlocal(metar)) % u = value
      y % metar(1:y % nlocal(metar)) % v = value
      y % metar(1:y % nlocal(metar)) % t = value
      y % metar(1:y % nlocal(metar)) % p = value
      y % metar(1:y % nlocal(metar)) % q = value
   end if

   ! Initialize Geo. AMV's:

   if ( y % nlocal(geoamv) > 0 ) then
      do n = 1, y % nlocal(geoamv)
       nlevels = iv%info(geoamv)%levels(n)
       y % geoamv(n) % u(1:nlevels) = value
       y % geoamv(n) % v(1:nlevels) = value
      end do
   end if

   ! Initialize Polat AMVs:

   if ( y % nlocal(polaramv) > 0 ) then
      do n = 1, y % nlocal(polaramv)
       nlevels = iv%info(polaramv)%levels(n)
       y % polaramv(n) % u(1:nlevels) = value
       y % polaramv(n) % v(1:nlevels) = value
      end do
   end if

   ! Initialize GPS TPW:

   if ( y % nlocal(gpspw) > 0 ) then
      y % gpspw(1:y % nlocal(gpspw)) % tpw = value
   end if

   ! Initialize GPS REFRACTIVITY:

   if ( y % nlocal(gpsref) > 0 ) then
      do n = 1, y % nlocal(gpsref)
         nlevels = iv % info(gpsref) % levels(n)
         y % gpsref(n) % ref(1:nlevels) = value
         y % gpsref(n) %   p(1:nlevels) = value
         y % gpsref(n) %   t(1:nlevels) = value
         y % gpsref(n) %   q(1:nlevels) = value
      end do
   end if

   ! Initialize GPS EXCESS PHASE:

   if ( y % nlocal(gpseph) > 0 ) then
      do n = 1, y % nlocal(gpseph)
         nlevels = iv % info(gpseph) % levels(n)
         if ( nlevels > 0 ) then
            y % gpseph(n) % eph(1:nlevels) = value
         end if
      end do
   end if

   ! Initialize sondes:

   if ( y % nlocal(sound) > 0 ) then
      do n = 1, y % nlocal(sound)
         nlevels = iv%info(sound)%levels(n)

         y % sound(n) % u(1:nlevels) = value
         y % sound(n) % v(1:nlevels) = value
         y % sound(n) % t(1:nlevels) = value
         y % sound(n) % q(1:nlevels) = value
      end do
   end if

   ! Initialize sonde_sfc
   if ( y % nlocal(sonde_sfc) > 0 ) then
      do n = 1, y % nlocal(sonde_sfc)
         y % sonde_sfc(n) % u = value
         y % sonde_sfc(n) % v = value
         y % sonde_sfc(n) % t = value
         y % sonde_sfc(n) % p = value
         y % sonde_sfc(n) % q = value
      end do
   end if
      
   if ( y % nlocal(mtgirs) > 0 ) then
      do n = 1, y % nlocal(mtgirs)
         nlevels = iv%info(mtgirs)%levels(n)

         y % mtgirs(n) % u(1:nlevels) = value
         y % mtgirs(n) % v(1:nlevels) = value
         y % mtgirs(n) % t(1:nlevels) = value
         y % mtgirs(n) % q(1:nlevels) = value

      end do
   end if

   if ( y % nlocal(tamdar) > 0 ) then
      do n = 1, y % nlocal(tamdar)
         nlevels = iv%info(tamdar)%levels(n)

         y % tamdar(n) % u(1:nlevels) = 0.0
         y % tamdar(n) % v(1:nlevels) = 0.0
         y % tamdar(n) % t(1:nlevels) = 0.0
         y % tamdar(n) % q(1:nlevels) = 0.0

      end do
   end if

! Initialize tamdar_sfc
   if ( y % nlocal(tamdar_sfc) > 0 ) then

         y % tamdar_sfc(n) % u = 0.0
         y % tamdar_sfc(n) % v = 0.0
         y % tamdar_sfc(n) % t = 0.0
         y % tamdar_sfc(n) % p = 0.0
         y % tamdar_sfc(n) % q = 0.0
   end if

   if ( y % nlocal(bogus) > 0 ) then
      do n = 1, y % nlocal(bogus)
         nlevels = iv % info(bogus) % levels(n)

         y % bogus(n) % u(1:nlevels) = value
         y % bogus(n) % v(1:nlevels) = value
         y % bogus(n) % t(1:nlevels) = value
         y % bogus(n) % q(1:nlevels) = value
         y % bogus(n) % slp          = value
      end do
   end if

   ! Initialize pilots:

   if ( y % nlocal(pilot) > 0 ) then
      do n = 1, y % nlocal(pilot)
         nlevels = iv % info(pilot) % levels(n)

         y % pilot(n) % u(1:nlevels) = value
         y % pilot(n) % v(1:nlevels) = value
      end do
   end if

   ! Initialize AIREPs:

   if ( y % nlocal(airep) > 0 ) then
      do n = 1, y % nlocal(airep)
         nlevels = iv%info(airep)%levels(n)

         y % airep(n) % u(1:nlevels) = value
         y % airep(n) % v(1:nlevels) = value
         y % airep(n) % t(1:nlevels) = value
         y % airep(n) % q(1:nlevels) = value
      end do
   end if

   ! Initialize satem:

   if ( y % nlocal(satem) > 0 ) then
      do n = 1, y % nlocal(satem)
         nlevels = iv % info(satem) % levels(n)

         y % satem(n) % thickness(1:nlevels) = value
      end do
   end if

   if ( y % nlocal(ssmi_tb) > 0 ) then
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb19v = value
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb19h = value
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb22v = value
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb37v = value
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb37h = value
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb85v = value
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb85h = value
   end if

   if ( y % nlocal(ssmi_rv) > 0 ) then
        y % ssmi_rv(1:y % nlocal(ssmi_rv)) % tpw = value
        y % ssmi_rv(1:y % nlocal(ssmi_rv)) % Speed = value
   end if
   
   if ( y % nlocal(ssmt1) > 0 ) then
      do n = 1, y % nlocal(ssmt1)
         nlevels = iv % info(ssmt1) % levels(n)
         y % ssmt1(n) % t(1:nlevels) = value
      end do
   end if
   
   if ( y % nlocal(ssmt2) > 0 ) then
      do n = 1, y % nlocal(ssmt2)
         nlevels = iv % info(ssmt2) % levels(n)
         y % ssmt2(n) % rh(1:nlevels) = value
      end do
   end if
   
   if ( pseudo_uvtpq ) then
        y % pseudo(1:num_pseudo) % u = value
        y % pseudo(1:num_pseudo) % v = value
        y % pseudo(1:num_pseudo) % t = value
        y % pseudo(1:num_pseudo) % p = value
        y % pseudo(1:num_pseudo) % q = value
   end if

   !  Initialize Quikscat:

   if ( y % nlocal(qscat) > 0 ) then
      y % qscat(1:y % nlocal(qscat)) % u = value
      y % qscat(1:y % nlocal(qscat)) % v = value
   end if
      
   ! Initialize profilers:

   if ( y % nlocal(profiler) > 0 ) then
      do n = 1, y % nlocal(profiler)
         nlevels = iv % info(profiler) % levels(n)

         y % profiler(n) % u(1:nlevels) = value
         y % profiler(n) % v(1:nlevels) = value
      end do
   end if

   ! Initialize buoy:

   if ( y % nlocal(buoy) > 0 ) then
      y % buoy(1:y % nlocal(buoy)) % u = value
      y % buoy(1:y % nlocal(buoy)) % v = value
      y % buoy(1:y % nlocal(buoy)) % t = value
      y % buoy(1:y % nlocal(buoy)) % p = value
      y % buoy(1:y % nlocal(buoy)) % q = value
   end if

   ! Initialize radar:
   if ( y % nlocal(radar) > 0 ) then
      do n = 1, y % nlocal(radar)
         nlevels = iv % info(radar) % levels(n)

         y % radar(n) % rv(1:nlevels) = value
         y % radar(n) % rf(1:nlevels) = value
      end do
   end if

   ! Initialize rain:
   if ( y % nlocal(rain) > 0 ) then
          y % rain(1:y % nlocal(rain)) % rain = value
   end if

   ! Initialize AIRS retrievals:

   if ( y % nlocal(airsr) > 0 ) then
      do n = 1, y % nlocal(airsr)
         nlevels = iv % info(airsr) % levels(n)

         y % airsr(n) % t(1:nlevels) = value
         y % airsr(n) % q(1:nlevels) = value
      end do
   end if


   if (trace_use_dull) call da_trace_exit("da_zero_y")

end subroutine da_zero_y        


