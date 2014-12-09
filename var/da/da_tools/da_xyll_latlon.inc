subroutine da_xyll_latlon(x, y, proj, lat, lon)

   !-----------------------------------------------------------------------
   ! Purpose: Compute the lat/lon location of an x/y on a LATLON grid.
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)             :: x
   real, intent(in)             :: y
   type(proj_info), intent(in)  :: proj
   real, intent(out)            :: lat
   real, intent(out)            :: lon

   real                         :: deltalat
   real                         :: deltalon
   real                         :: latinc
   real                         :: loninc
   real                         :: lon360

   if (trace_use_frequent) call da_trace_entry("da_xyll_latlon")

   loninc = proj%dx*360.0/(2.0*EARTH_RADIUS_M*PI)
   latinc = proj%dx*360.0/(2.0*EARTH_RADIUS_M*PI)

   deltalon = (x - proj%knowni) * loninc
   deltalat = (y - proj%knownj) * latinc

   lon360 = deltalon + proj%lon1
   lat    = deltalat + proj%lat1

   if ((ABS(lat) > 90.0).OR.(ABS(deltalon) > 360.0)) then
      ! Off the earth for this grid; THIS SHOULD NEVER HAPPEN
      lat = -999.0
      lon = -999.0
   else
      lon = MOD(lon360,360.0)
      if (lon > 180.0) lon = lon - 360.0
   end if

   if (trace_use_frequent) call da_trace_exit("da_xyll_latlon")

end subroutine da_xyll_latlon


