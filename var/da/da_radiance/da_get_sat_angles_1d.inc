subroutine da_get_sat_angles_1d ( lat, lon, sate_index, satzen, satazi )
!-------------------------------------------------
! Purpose: calculate geostationary satellite_zenith_angle
!
! Method: Yang et al., 2017: Impact of assimilating GOES imager 
!          clear-sky radiance with a rapid refresh assimilation 
!          system for convection-permitting forecast over Mexico. 
!          J. Geophys. Res. Atmos., 122, 5472–5490
!-------------------------------------------------

   implicit none

   real,           intent(in)   :: lat(:),lon(:)
   integer,        intent(in)   :: sate_index
   real,           intent(out)  :: satzen(:)
   real, optional, intent(out)  :: satazi(:)

   integer                         :: n
   real(r_double)                  :: alon_sat
   real(r_double), allocatable     :: alat(:), alon(:)
   real(r_double), allocatable     :: theta(:), r_tmp(:), theta_tmp(:), gam(:)
   real(r_double), allocatable     :: beta(:)
   logical, allocatable  :: valid_loc(:)

   satzen = missing_r
   if (present(satazi)) satazi = missing_r

   n = size(lat)
   if (n.le.0) return

   allocate( alat(n) )
   allocate( alon(n) )
   allocate( theta(n) )
   allocate( r_tmp(n) )
   allocate( theta_tmp(n) )
   allocate( gam(n) )
   allocate( valid_loc(n) )

   !Define valid locations for vectorized operations
   valid_loc = ( lat .lt.   90. .and. &
                 lat .gt.  -90. .and. &
                 lon .le.  180. .and. &
                 lon .ge. -180. )

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
      alon_sat = -75.2 * deg2rad
   else if (sate_index .eq. 17) then
      alon_sat = -137.2 * deg2rad
   else
      write(*,*)'this satellite is not included'
      stop
   end if

   where ( valid_loc )
      alat = lat * deg2rad
      alon = lon * deg2rad
      theta = alon - alon_sat
   elsewhere
      alat = missing_r
      alon = missing_r
      theta = missing_r
      gam = missing_r
      r_tmp = missing_r
   end where

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
   where ( valid_loc )
      gam = acos( cos( alat ) * cos( abs( theta ) ) )
      r_tmp = ( satellite_height+earth_radius )**2 * &
                 ( 1.d0 + ( earth_radius / ( satellite_height+earth_radius ) )**2 - &
                   2.d0 * ( earth_radius ) / ( satellite_height+earth_radius ) * cos( gam ) )
   end where

   valid_loc = (valid_loc .and. r_tmp.ge.0)

   where ( valid_loc )
      r_tmp = sqrt(r_tmp)
      satzen = asin((satellite_height+earth_radius) / r_tmp * sin(gam)) / deg2rad !to degrees
   end where


   ! azimuth
   if ( present(satazi) ) then
      allocate( beta(n) )
      beta = missing_r
      where ( valid_loc ) &
         beta = tan(alat) / tan(gam)
      where ( beta.gt.1._r_double .and. &
              beta.lt.1.00000001_r_double .and. valid_loc ) &
         beta = 1.0_r_double
      where ( valid_loc ) &
         beta = acos( beta ) / deg2rad !to degrees
      where ( lat.lt.0. .and. theta.le.0. .and. valid_loc ) &
         satazi = beta
      where ( lat.ge.0. .and. theta.le.0. .and. valid_loc ) &
         satazi = 180.d0 - beta
      where ( lat.ge.0. .and. theta.gt.0. .and. valid_loc ) &
         satazi = 180.d0 + beta
      where ( lat.lt.0. .and. theta.gt.0. .and. valid_loc ) &
         satazi = 360.d0 - beta
      deallocate( beta )
   end if

   deallocate( alat, alon, theta, r_tmp, theta_tmp, gam, valid_loc )

   return

end subroutine da_get_sat_angles_1d
