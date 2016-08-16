subroutine da_earth_2_model_wind(eu,ev,mu,mv,lon)

   !---------------------------------------------------------------------------
   ! Purpose: Convert earth wind to model wind.
   !
   ! Need map projection parameters.
   !
   ! IPROJ: Projection type
   ! PHIC:  Central latitude 
   ! XLONC: Central longitude
   ! XN:    Cone projection
   ! CONV:  180/Pi
   !---------------------------------------------------------------------------

   implicit none

   real*8, intent(in)  :: eu, ev
   real, intent(out) :: mu, mv
   real, intent(in)  :: lon

   real :: xlonrt, ang

   if (trace_use) call da_trace_entry("da_earth_2_model_wind")

   ! for mercator projection, the winds are as in earth coordinates

   if (map_projection == 3) then
      mu = eu
      mv = ev
      if (trace_use) call da_trace_exit("da_earth_2_model_wind")
      return
   end if

   ! for conversion to grid coordinates,
   ! see program datamap, subr vect, and
   ! ANTHES METEO. 597 NOTES, EQUA. 2.23, 2.25, 2.28.

   xlonrt = xlonc-lon

   if (xlonrt > 180.0) xlonrt=xlonrt-360.0
   if (xlonrt <-180.0) xlonrt=xlonrt+360.0

   ang=xlonrt*cone_factor*pi/180.0

   if (phic < 0.0) ang=-ang

   mu = ev*sin(ang) + eu*cos(ang)
   mv = ev*cos(ang) - eu*sin(ang)

   if (trace_use) call da_trace_exit("da_earth_2_model_wind")

end subroutine da_earth_2_model_wind

