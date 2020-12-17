subroutine da_llxy_latlon(lat, lon, proj, x, y)

   !----------------------------------------------------------------------- 
   ! Purpose: Compute the x/y location of a lat/lon on a LATLON 
   !          (cylindrical equidistant) grid.
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)             :: lat
   real, intent(in)             :: lon
   type(proj_info), intent(in)  :: proj
   real, intent(out)            :: x
   real, intent(out)            :: y

   real                         :: deltalat
   real                         :: deltalon
   real                         :: lon360
   real                         :: latinc
   real                         :: loninc

   if (trace_use_frequent) call da_trace_entry("da_llxy_latlon")

   ! To account for issues around the dateline, convert the incoming
   ! longitudes to be 0->360.0
   if (lon < 0) then
      lon360 = lon + 360.0
   else
      lon360 = lon
   end if

   deltalat = lat - proj%lat1
   deltalon = lon360 - proj%lon1

   !For cylindrical equidistant, dx == dy
   loninc = proj%dx*360.0/(2.0*EARTH_RADIUS_M*PI)
   latinc = proj%dx*360.0/(2.0*EARTH_RADIUS_M*PI)

   ! Compute x/y
   x = deltalon/loninc
   y = deltalat/latinc

   x = x + proj%knowni
   y = y + proj%knownj


   if (trace_use_frequent) call da_trace_exit("da_llxy_latlon")

end subroutine da_llxy_latlon


