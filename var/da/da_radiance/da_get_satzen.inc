subroutine da_get_satzen ( lat,lon,sate_index,theta_true )
!-------------------------------------------------
! Purpose: calculate geostationary satellite_zenith_angle
!
! Menthod: Yang et al., 2017: Impact of assimilating GOES imager 
!          clear-sky radiance with a rapid refresh assimilation 
!          system for convection-permitting forecast over Mexico. 
!          J. Geophys. Res. Atmos., 122, 5472–5490
!-------------------------------------------------

  implicit none

  real,    intent(in)   :: lat,lon
  integer, intent(in)   :: sate_index
  real,    intent(out)  :: theta_true

  real                  :: alat, alon, alon_sat
  real                  :: theta, r_tmp, theta_tmp


        alat = lat
        alon = lon

        if (sate_index .eq. 11) then
           alon_sat = -135.*pi/180.
        else if (sate_index .eq. 12) then
           alon_sat = -60.*pi/180.
        else if (sate_index .eq. 13) then
           alon_sat = -75.*pi/180.
        else if (sate_index .eq. 14) then
           alon_sat = -105.*pi/180.
        else if (sate_index .eq. 15) then
           alon_sat = -135.*pi/180.
        else
           write(*,*)'this satellite is not included'
           stop
        end if
 
        alat = alat*pi/180.
        alon = alon*pi/180.
        theta = abs(alon-alon_sat)
        r_tmp = (2*earth_radius*sin(theta/2.)-earth_radius*(1-cos(alat))*sin(theta/2.))**2 &
                +(2*earth_radius*sin(alat/2.))**2-(earth_radius*(1-cos(alat))*sin(theta/2.))**2
        r_tmp = sqrt(r_tmp)
        theta_true = 2*asin(r_tmp/earth_radius/2.)
        theta_tmp = atan(earth_radius*sin(theta_true)/(satellite_height+earth_radius*(1-sin(theta_true))))
        theta_true = (theta_true+theta_tmp)*180./pi

        return

end subroutine da_get_satzen
