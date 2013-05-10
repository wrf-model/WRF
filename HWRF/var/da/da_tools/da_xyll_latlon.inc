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

   if (trace_use_frequent) call da_trace_entry("da_xyll_latlon")

   ! Extract the latitude and longitude increments for this grid
   ! (e.g., 2.5 deg for NCEP reanalysis) from the proj structure, where
   ! loninc is saved in the stdlon tag and latinc is saved in truelat1

   latinc = proj%truelat1
   loninc = proj%stdlon

   ! Compute deltalat and deltalon 

   deltalat = (x-1.0)*latinc
   deltalon = (y-1.0)*loninc
   lat = proj%lat1 + deltalat
   lon = proj%lon1 + deltalon

   if ((ABS(lat) > 90.0).OR.(ABS(deltalon) > 360.0)) then
      ! Off the earth for this grid
      lat = -999.0
      lon = -999.0
   else
      lon = lon + 360.0
      lon = MOD(lon,360.0)
      if (lon > 180.0) lon = lon -360.0
   end if

   if (trace_use_frequent) call da_trace_exit("da_xyll_latlon")

end subroutine da_xyll_latlon


