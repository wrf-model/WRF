subroutine da_zero_y( iv, y )

   !---------------------------------------------------------------------------
   ! Purpose: Initialises the Y-array
   !---------------------------------------------------------------------------

   implicit none
   
   type (iv_type), intent(in)            :: iv      ! Ob type input.
   type (y_type), intent(inout)          :: y       ! Residual type structure.

   integer                               :: n       ! Loop counter.
   integer                               :: nlevels ! Number of levels.

   if (trace_use_dull) call da_trace_entry("da_zero_y")

   !---------------------------------------------------------------------------
   ! [1.0] Copy number of observations:
   !---------------------------------------------------------------------------

   y % nlocal(:) = iv % nlocal(:)

   !---------------------------------------------------------------------------
   ! [2.0] Allocate:
   !---------------------------------------------------------------------------

   ! Initialize synops:

   if ( y % nlocal(synop) > 0 ) then
      y % synop(1:y % nlocal(synop)) % u = 0.0
      y % synop(1:y % nlocal(synop)) % v = 0.0
      y % synop(1:y % nlocal(synop)) % t = 0.0
      y % synop(1:y % nlocal(synop)) % p = 0.0
      y % synop(1:y % nlocal(synop)) % q = 0.0
   end if

   ! Initialize ships:

   if ( y % nlocal(ships) > 0 ) then
      y % ships(1:y % nlocal(ships)) % u = 0.0
      y % ships(1:y % nlocal(ships)) % v = 0.0
      y % ships(1:y % nlocal(ships)) % t = 0.0
      y % ships(1:y % nlocal(ships)) % p = 0.0
      y % ships(1:y % nlocal(ships)) % q = 0.0
   end if

   ! Initialize metars:

   if ( y % nlocal(metar) > 0 ) then
      y % metar(1:y % nlocal(metar)) % u = 0.0
      y % metar(1:y % nlocal(metar)) % v = 0.0
      y % metar(1:y % nlocal(metar)) % t = 0.0
      y % metar(1:y % nlocal(metar)) % p = 0.0
      y % metar(1:y % nlocal(metar)) % q = 0.0
   end if

   ! Initialize Geo. AMV's:

   if ( y % nlocal(geoamv) > 0 ) then
      do n = 1, y % nlocal(geoamv)
       nlevels = iv%info(geoamv)%levels(n)
       y % geoamv(n) % u(1:nlevels) = 0.0
       y % geoamv(n) % v(1:nlevels) = 0.0
      end do
   end if

   ! Initialize Polat AMVs:

   if ( y % nlocal(polaramv) > 0 ) then
      do n = 1, y % nlocal(polaramv)
       nlevels = iv%info(polaramv)%levels(n)
       y % polaramv(n) % u(1:nlevels) = 0.0
       y % polaramv(n) % v(1:nlevels) = 0.0
      end do
   end if

   ! Initialize GPS TPW:

   if ( y % nlocal(gpspw) > 0 ) then
      y % gpspw(1:y % nlocal(gpspw)) % tpw = 0.0
   end if

   ! Initialize GPS REFRACTIVITY:

   if ( y % nlocal(gpsref) > 0 ) then
      do n = 1, y % nlocal(gpsref)
         nlevels = iv % gpsref(n) % info % levels
         y % gpsref(n) % ref(1:nlevels) = 0.0
         y % gpsref(n) %   p(1:nlevels) = 0.0
         y % gpsref(n) %   t(1:nlevels) = 0.0
         y % gpsref(n) %   q(1:nlevels) = 0.0
      end do
   end if

   ! Initialize sondes:

   if ( y % nlocal(sound) > 0 ) then
      do n = 1, y % nlocal(sound)
         nlevels = iv%info(sound)%levels(n)

         y % sound(n) % u(1:nlevels) = 0.0
         y % sound(n) % v(1:nlevels) = 0.0
         y % sound(n) % t(1:nlevels) = 0.0
         y % sound(n) % q(1:nlevels) = 0.0

         ! Initialize sonde_sfc

         y % sonde_sfc(n) % u = 0.0
         y % sonde_sfc(n) % v = 0.0
         y % sonde_sfc(n) % t = 0.0
         y % sonde_sfc(n) % p = 0.0
         y % sonde_sfc(n) % q = 0.0
      end do
   end if
      
   if ( y % nlocal(mtgirs) > 0 ) then
      do n = 1, y % nlocal(mtgirs)
         nlevels = iv%info(mtgirs)%levels(n)

         y % mtgirs(n) % u(1:nlevels) = 0.0
         y % mtgirs(n) % v(1:nlevels) = 0.0
         y % mtgirs(n) % t(1:nlevels) = 0.0
         y % mtgirs(n) % q(1:nlevels) = 0.0

      end do
   end if

   if ( y % nlocal(bogus) > 0 ) then
      do n = 1, y % nlocal(bogus)
         nlevels = iv % bogus(n) % info % levels

         y % bogus(n) % u(1:nlevels) = 0.0
         y % bogus(n) % v(1:nlevels) = 0.0
         y % bogus(n) % t(1:nlevels) = 0.0
         y % bogus(n) % q(1:nlevels) = 0.0
         y % bogus(n) % slp          = 0.0
      end do
   end if

   ! Initialize pilots:

   if ( y % nlocal(pilot) > 0 ) then
      do n = 1, y % nlocal(pilot)
         nlevels = iv % pilot(n) % info % levels

         y % pilot(n) % u(1:nlevels) = 0.0
         y % pilot(n) % v(1:nlevels) = 0.0
      end do
   end if

   ! Initialize AIREPs:

   if ( y % nlocal(airep) > 0 ) then
      do n = 1, y % nlocal(airep)
         nlevels = iv%info(airep)%levels(n)

         y % airep(n) % u(1:nlevels) = 0.0
         y % airep(n) % v(1:nlevels) = 0.0
         y % airep(n) % t(1:nlevels) = 0.0
      end do
   end if

   ! Initialize satem:

   if ( y % nlocal(satem) > 0 ) then
      do n = 1, y % nlocal(satem)
         nlevels = iv % satem(n) % info % levels

         y % satem(n) % thickness(1:nlevels) = 0.0
      end do
   end if

   if ( y % nlocal(ssmi_tb) > 0 ) then
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb19v = 0.0
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb19h = 0.0
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb22v = 0.0
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb37v = 0.0
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb37h = 0.0
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb85v = 0.0
      y % ssmi_tb(1:y % nlocal(ssmi_tb)) % tb85h = 0.0
   end if

   if ( y % nlocal(ssmi_rv) > 0 ) then
        y % ssmi_rv(1:y % nlocal(ssmi_rv)) % tpw = 0.0
        y % ssmi_rv(1:y % nlocal(ssmi_rv)) % Speed = 0.0
   end if
   
   if ( y % nlocal(ssmt1) > 0 ) then
      do n = 1, y % nlocal(ssmt1)
         nlevels = iv % ssmt1(n) % info % levels
         y % ssmt1(n) % t(1:nlevels) = 0.0
      end do
   end if
   
   if ( y % nlocal(ssmt2) > 0 ) then
      do n = 1, y % nlocal(ssmt2)
         nlevels = iv % ssmt2(n) % info % levels
         y % ssmt2(n) % rh(1:nlevels) = 0.0
      end do
   end if
   
   if ( y % num_pseudo > 0 ) then
        y % pseudo(1:y % num_pseudo) % u = 0.0
        y % pseudo(1:y % num_pseudo) % v = 0.0
        y % pseudo(1:y % num_pseudo) % t = 0.0
        y % pseudo(1:y % num_pseudo) % p = 0.0
        y % pseudo(1:y % num_pseudo) % q = 0.0
   end if

   !  Initialize Quikscat:

   if ( y % nlocal(qscat) > 0 ) then
      y % qscat(1:y % nlocal(qscat)) % u = 0.0
      y % qscat(1:y % nlocal(qscat)) % v = 0.0
   end if
      
   ! Initialize profilers:

   if ( y % nlocal(profiler) > 0 ) then
      do n = 1, y % nlocal(profiler)
         nlevels = iv % profiler(n) % info % levels

         y % profiler(n) % u(1:nlevels) = 0.0
         y % profiler(n) % v(1:nlevels) = 0.0
      end do
   end if

   ! Initialize buoy:

   if ( y % nlocal(buoy) > 0 ) then
      y % buoy(1:y % nlocal(buoy)) % u = 0.0
      y % buoy(1:y % nlocal(buoy)) % v = 0.0
      y % buoy(1:y % nlocal(buoy)) % t = 0.0
      y % buoy(1:y % nlocal(buoy)) % p = 0.0
      y % buoy(1:y % nlocal(buoy)) % q = 0.0
   end if

   ! Initialize buoy:
   if ( y % nlocal(radar) > 0 ) then
      do n = 1, y % nlocal(radar)
         nlevels = iv % radar(n) % info % levels

         y % radar(n) % rv(1:nlevels) = 0.0
         y % radar(n) % rf(1:nlevels) = 0.0
      end do
   end if
   ! Initialize AIRS retrievals:

   if ( y % nlocal(airsr) > 0 ) then
      do n = 1, y % nlocal(airsr)
         nlevels = iv % airsr(n) % info % levels

         y % airsr(n) % t(1:nlevels) = 0.0
         y % airsr(n) % q(1:nlevels) = 0.0
      end do
   end if


   if (trace_use_dull) call da_trace_exit("da_zero_y")

end subroutine da_zero_y        


