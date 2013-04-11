!/**----------------------------------------------------------------------    
! @sub       geo2msl1
!
! Convert a geopotential height to mean sea level geometric height.
! Use a more accurate conversion found at:
! http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
! @parameter  
! @ input:    h   -- geopotential height (km)
! @           lat -- latitude  of profile in degrees
! @           lon -- longitude of profile in degrees.
! @ output:   z   -- geometric height (km)
! -----------------------------------------------------------------------*/
subroutine da_geo2msl1 (h, lat, lon, z)
      implicit none
      real*8 h, lat, lon, z     ! Note lon is currently not used
      real*8 pi, latr, hm
      real*8 semi_major_axis, semi_minor_axis, grav_polar, grav_equator
      real*8 earth_omega, grav_constant, flattening, somigliana
      real*8 grav_ratio, sin2, termg, termr, grav, eccentricity

!     Parameters below from WGS-84 model software inside GPS receivers.
      parameter(semi_major_axis = 6378.1370d3)    ! (m)
      parameter(semi_minor_axis = 6356.7523142d3) ! (m)
      parameter(grav_polar = 9.8321849378)        ! (m/s2)
      parameter(grav_equator = 9.7803253359)      ! (m/s2)
      parameter(earth_omega = 7.292115d-5)        ! (rad/s)
      parameter(grav_constant = 3.986004418d14)   ! (m3/s2)
      parameter(grav = 9.80665d0)                 ! (m/s2) WMO std g at 45 deg lat
      parameter(eccentricity = 0.081819d0)        ! unitless

!     Derived geophysical constants
      parameter(flattening = (semi_major_axis-semi_minor_axis) /  &
                              semi_major_axis)

      parameter(somigliana = &
       (semi_minor_axis/semi_major_axis)*(grav_polar/grav_equator)-1.d0)

      parameter(grav_ratio = (earth_omega*earth_omega * &
       semi_major_axis*semi_major_axis * semi_minor_axis)/grav_constant)

      pi   = 3.14159265358979d0
      latr = lat * (pi/180.d0)        ! in radians
      hm   = h * 1000d0               ! in meters

      if (h.eq.-999.d0) then
         z = -999.d0
      else 
         sin2  = sin(latr) * sin(latr)
         termg = grav_equator *  &
          ( (1.d0+somigliana*sin2) /  &
            sqrt(1.d0-eccentricity*eccentricity*sin2) )
         termr = semi_major_axis   /  &
          (1.d0 + flattening + grav_ratio - 2.d0*flattening*sin2)

!     geometric height 

         z = ( (termr*hm) / ( (termg/grav) * termr - hm ) ) * 0.001 ! in km

      endif
  
end subroutine da_geo2msl1 
