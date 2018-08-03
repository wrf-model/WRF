subroutine da_get_sat_angles ( lat, lon, sate_index, satzen, satazi )
!-------------------------------------------------
! Purpose: calculate geostationary satellite_zenith_angle
!
! Menthod: Yang et al., 2017: Impact of assimilating GOES imager 
!          clear-sky radiance with a rapid refresh assimilation 
!          system for convection-permitting forecast over Mexico. 
!          J. Geophys. Res. Atmos., 122, 5472–5490
!-------------------------------------------------

   implicit none

   real,           intent(in)   :: lat,lon
   integer,        intent(in)   :: sate_index
   real,           intent(out)  :: satzen
   real, optional, intent(out)  :: satazi

   real(r_double) :: alat, alon, alon_sat
   real(r_double) :: theta, r_tmp, theta_tmp, gam, beta

   satzen = missing_r
   if ( present( satazi ) ) satazi = missing_r

   if ( lat .ge.   90. .or. &
        lat .le.  -90. .or. &
        lon .gt.  180. .or. &
        lon .lt. -180. ) then
      return
   end if

   if (sate_index .eq. 11) then
      alon_sat = -135. * deg2rad
   else if (sate_index .eq. 12) then
      alon_sat = -60. * deg2rad
   else if (sate_index .eq. 13) then
      alon_sat = -75. * deg2rad
   else if (sate_index .eq. 14) then
      alon_sat = -105. * deg2rad
   else if (sate_index .eq. 15) then
      alon_sat = -135. * deg2rad
   else if (sate_index .eq. 16) then
!      alon_sat = -75.2 * deg2rad !True Value?
      alon_sat = -75. * deg2rad !Nominal Value
!   else if (sate_index .eq. 17) then
!      alon_sat = -137. * deg2rad
   else
      write(*,*)'this satellite is not included'
      stop
   end if
 
   alat = lat * deg2rad
   alon = lon * deg2rad
   theta = alon-alon_sat

   ! Yang et al., 2017

   ! zenith
!   r_tmp = (2*earth_radius*sin(abs(theta)/2.)-earth_radius*(1-cos(alat))*sin(abs(theta)/2.))**2 &
!           +(2*earth_radius*sin(alat/2.))**2-(earth_radius*(1-cos(alat))*sin(abs(theta)/2.))**2
!   r_tmp = sqrt(r_tmp)
!   satzen = 2*asin(r_tmp/earth_radius/2.)
!   theta_tmp = atan(earth_radius*sin(satzen)/(satellite_height+earth_radius*(1-sin(satzen))))
!   satzen = (satzen+theta_tmp) / deg2rad !to degrees


   ! Soler et al., Determination of Look Angles to Geostationary Communication Satellites, 
   !    Journal of Surveying Engineering, Vol. 120, No. 3, August, 1994.
   ! follows spherical earth approximation

   ! zenith (up to 1 deg difference with code from Yang et al., 2017)
   gam = acos( cos( alat ) * cos( abs( theta ) ) )
   r_tmp = ( satellite_height+earth_radius )**2 * &
              ( 1.d0 + ( earth_radius / ( satellite_height+earth_radius ) )**2 - &
                2.d0 * ( earth_radius ) / ( satellite_height+earth_radius ) * cos(gam) )

   if (r_tmp .lt. 0) return

   r_tmp = sqrt(r_tmp)
   satzen = asin((satellite_height+earth_radius) / r_tmp * sin(gam)) / deg2rad !to degrees


   ! azimuth
   if ( present(satazi) ) then
      beta = tan(alat) / tan(gam)
      if (beta.gt.1.D0 .and. beta.lt.1.00000001D0) beta = 1.0D0
      beta = acos( beta ) / deg2rad !to degrees

      if ( lat.lt.0. .and. theta.le.0. ) &
         satazi = beta
      if ( lat.ge.0. .and. theta.le.0. ) &
         satazi = 180.d0 - beta
      if ( lat.ge.0. .and. theta.gt.0. ) &
         satazi = 180.d0 + beta
      if ( lat.lt.0. .and. theta.gt.0. ) &
         satazi = 360.d0 - beta
   end if

   return

end subroutine da_get_sat_angles
