subroutine da_get_solar_angles( yr, mt, dy, hr, mn, sc, &
                                lat, lon, solzen, solazi )
   !--------------------------------------------------------------------------------+
   !  This subroutine calculates the local azimuth and zenith angles of the sun at  |
   !  a specific location and time using an approximation to equations used         |
   !  to generate tables in The Astronomical Almanac.                               |
   !  Refraction correction is added so sun position is apparent one.               |
   !                                                                                |
   !  Michalsky, Joseph J., The Astronomical Almanac's algorithm for approximate    |
   !     solar position (1950-2050), Solar Energy, Vol. 40, No. 3, pp227-235, 1988. |
   !                                                                                |
   !   AND                                                                          |
   !                                                                                |
   !   U.S. Gov't Printing Office, Washington,D.C. (1985).                          |
   !                                                                                |
   !   Provides solar zenith and azimuth angles with errors within ±0.01 deg.       |
   !    for the time period 1950-2050.                                              |
   !                                                                                |
   !   INPUT parameters                                                             |
   !      yr, mt, dy, hr, mn, sc = integer date/time quantities     |
   !      lat = latitude in degrees (north is positive)                             |
   !      lon = longitude in degrees (east is positive)                             |
   !                                                                                |
   !   OUTPUT parameters                                                            |
   !      solazi = sun azimuth angle (measured east from north, 0 to 360 degs)        |
   !      solzen = sun elevation angle (degs)                                       |
   !                                                                                |
   !   Converted from F77 to F90 by Juan Pablo Justiniano  <jpjustiniano@gmail.com> |
   !   (https://github.com/jpjustiniano/Subroutines)                                |
   !                                                                                |
   !   For more accurate algorithms (±0.0003 deg.) across longer periods of time,   |
   !   refer to the National Renewable Energy Laboratory (NREL) Solar Postion       |
   !   Algorithm (SPA), available in C, Matlab, and Python:                         |
   !     - https://rredc.nrel.gov/solar/codesandalgorithms/spa                      |
   !     - https://www.mathworks.com/matlabcentral/fileexchange/59903-nrel-s-solar-position-algorithm-spa |
   !     - https://sunpy.org                                                        |
   !--------------------------------------------------------------------------------+

   implicit none

   integer, intent(in) :: yr, mt, dy, hr, mn, sc
   real, intent(in) :: lat
   real, intent(in) :: lon
   real, intent(out) :: solazi
   real, intent(out) :: solzen
 
   real(r_double) :: latrad
   real(r_double) :: delta, ju, jmod, time, gmst, lmst
   real(r_double) :: mnlon, mnanom, eclon, oblqec
   real(r_double) :: num, den, ra, dec, ha
   real(r_double) :: elev, refrac !, elc

   ! Conversion to Julian day from MJD reference time: 1978 Jan 01 00:12:00 (see da_get_julian_time)
   real(r_double), parameter :: jd_jmod = 43510.0  ! = 2443510.0 - 2.4e6 (rel. adjust improves precision of ±)

!   ! Conversion to Julian day from MJD reference time: 1978 Jan 01 00:00:00 (see da_get_julian_time)
!   real(r_double), parameter :: jd_jmod = 43509.5  ! = 2443510.0 - 2.4e6 (rel. adjust improves precision of ±)

   solzen = missing_r
   solazi = missing_r
   if ( lat .gt.   90. .or. &
        lat .lt.  -90. .or. &
        lon .gt.  180. .or. &
        lon .lt. -180. ) then
      return
   end if

   call da_get_julian_time( yr, mt, dy, hr, mn, jmod )
   ju = jmod / 1440.0 + real(sc,r_double) / 86400.0 + jd_jmod

   ! Calculate ecliptic coordinates (depends on time [days] since noon 1 Jan, 2000)
   ! 51545.0 + 2.4e6 = noon 1 Jan, 2000
   time  = ju - 51545.0

   ! Force mean longitude between 0 and 360 degs
   mnlon = 280.460 + 0.9856474 * time
   mnlon = mod( mnlon, 360. )
   if ( mnlon.lt.0. ) mnlon = mnlon + 360.

   ! Mean anomaly in radians between 0 and 2*pi
   mnanom = 357.528 + 0.9856003 * time
   mnanom = mod( mnanom, 360. )
   if ( mnanom.lt.0. ) mnanom = mnanom + 360.
   mnanom = mnanom * deg2rad

   ! Compute the ecliptic longitude and obliquity of ecliptic in radians
   eclon = mnlon + 1.915*sin( mnanom ) + 0.020*sin( 2.*mnanom )
   eclon = mod( eclon, 360. )

   if ( eclon.lt.0. ) eclon = eclon + 360.

   oblqec = 23.439 - 0.0000004*time
   eclon = eclon * deg2rad
   oblqec = oblqec * deg2rad

   ! Calculate right ascension and force between 0 and 2*pi
   num = cos( oblqec ) * sin( eclon )
   den = cos( eclon )
   ra  = atan( num/den )
   if ( den.lt.0 ) then
       ra = ra + PI
   elseif ( num.lt.0 ) then
       ra = ra + 2.0*PI
   endif

   ! Calculate declination in radians
   ! (asin varies between -pi/2 to pi/2)
   dec = asin( sin( oblqec ) * sin( eclon ) )

   ! Calculate Greenwich mean sidereal time in hours
!   gmst = 6.697375 + 0.0657098242*time + real(hr,r_double) + real(mn,r_double) / 60. + real(sc,r_double) / 3600.
   gmst = 6.697375 + 0.0657098242*time + real(hr * 3600 + mn * 60 + sc, r_double) / 3600.

   ! Hour not changed to sidereal time since 'time' includes the fractional day 
   gmst = mod( gmst, 24. )
   if ( gmst.lt.0. ) gmst = gmst + 24.

   ! Calculate local mean sidereal time in radians 
   lmst = gmst + lon / 15.
   lmst = mod( lmst, 24. )
   if ( lmst.lt.0. ) lmst = lmst + 24.
   lmst = lmst * 15. * deg2rad


   ! Calculate hour angle in radians between -pi and pi
   ha = lmst - ra
   if ( ha .lt. -PI ) ha = ha + 2.0*PI
   if ( ha .gt.  PI ) ha = ha - 2.0*PI

   ! Change latitude to radians
   latrad = lat * deg2rad

   ! From this point on:
   ! mnlon in degs, gmst in hours, ju in days minus 2.4e6;
   ! mnanom, eclon, oblqec, ra, lmst, and ha in radians

   ! Calculate elevation (90 - zenith)
   ! (asin varies between -pi/2 to pi/2)
   elev = asin(  sin( dec ) * sin( latrad ) + cos( dec ) * cos( latrad ) * cos( ha ) )

   ! Night-time angles are inconsequential
   if ( elev < 0. ) return

   ! Calculate azimuth
   ! (asin varies between -pi/2 to pi/2)
   solazi  = asin( -cos( dec ) * sin( ha ) / cos( elev ) )

!JJG: From J.P. Justiniano (not in Michalsky, causes differences with NREL SPA)
!! This puts azimuth between 0 and 2*pi radians
!   if ( sin(dec) - sin(elev) * sin(latrad) .ge. 0. ) then
!      if ( sin(solazi) .lt. 0. ) solazi = solazi + 2.0*PI
!   else
!      solazi = PI - solazi
!   endif


!   ! When solazi=90 degs, elev == elcritical = asin( sin(dec) / sin(latrad) )
! JJG: elc is undefined when sin(dec) / sin(latrad) is outside [-1,1] or dec > latrad when both are positive...need better method to determine quadrant
   !ORIGINAL:  
   !elc = asin( sin( dec ) / sin( latrad ) )
   !if ( elev.ge.elc )                solazi = PI - solazi
   !if ( elev.le.elc .and. ha.gt.0. ) solazi = 2.0*PI + solazi

   !Updated according to Eq. 3.18 at https://www.powerfromthesun.net/Book/chapter03/chapter03.html
   ! "Power From The Sun" is the great new website by William Stine and Michael Geyer. It features 
   ! a revised and updated (and free!) version of "Solar Energy Systems Design" by W.B.Stine and 
   ! R.W.Harrigan (John Wiley and Sons, Inc. 1986) retitled "Power From The Sun", along with 
   ! resources we hope you will find useful in learning about solar energy. 
   if ( cos(ha) < ( tan(dec) / tan(latrad) ) ) then
      solazi = 2.0*PI + solazi
   else
      solazi = PI - solazi
   end if

   ! Convert az to degs, force between 0 and 2*pi
   solazi = solazi / deg2rad
   solazi = mod( solazi, 360. )

   ! Calculate refraction correction for US stan. atmosphere
   ! (need to have elev in degs before calculating correction)
   elev = elev / deg2rad

   !JJG: Added these bounds (should not need them)
   !Keep elevation between -90. to +90.
   if ( elev.lt.-90. ) &
      elev = - (180. + elev)
   if ( elev.gt.90. ) &
      elev = 180. - elev

!   ! Michalsky (1988)
!   if ( elev.gt. - 0.56 ) then
!      refrac = 3.51579 * ( 0.1594 + 0.0196*elev + 0.00002*elev**2 ) / &
!         ( 1. + 0.505*elev + 0.0845*elev**2 ) 
!   else
!      refrac = 0.56 
!   endif

   !J.P. Justiniano (not in Michalsky, more accurate than above?)
   if ( elev.ge.19.225 ) then 
      refrac = 0.00452 * 3.51823 / tan( elev*deg2rad )
   else if ( elev.gt.-0.766 .and. elev.lt.19.225 ) then
      refrac = 3.51579 * ( 0.1594 + 0.0196 * elev + 0.00002*elev**2 ) / &
         ( 1. + 0.505*elev + 0.0845*elev**2 )
   else
      refrac = 0.0
   end if

   ! note that 3.51579=1013.25 mb/288.2 C

   elev = elev + refrac

   ! Convert elevation to topocentric zenith
   solzen = 90.0_r_kind - elev

end subroutine da_get_solar_angles
