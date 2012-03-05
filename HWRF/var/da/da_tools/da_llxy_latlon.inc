subroutine da_llxy_latlon(lat, lon, proj, x, y)

   !----------------------------------------------------------------------- 
   ! Purpose: Compute the x/y location of a lat/lon on a LATLON grid.
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

   ! Extract the latitude and longitude increments for this grid
   ! (e.g., 2.5 deg for NCEP reanalysis) from the proj structure, where
   ! loninc is saved in the stdlon tag and latinc is saved in truelat1

   latinc = proj%truelat1
   loninc = proj%stdlon

   ! Compute deltalat and deltalon as the difference between the input 
   ! lat/lon and the origin lat/lon

   deltalat = lat - proj%lat1

   ! To account for issues around the dateline, convert the incoming
   ! longitudes to be 0->360.0
   if (lon < 0) then 
      lon360 = lon + 360.0 
   else 
      lon360 = lon
   end if    
   deltalon = lon360 - proj%lon1      
    
   ! Compute x/y
   x = deltalon/loninc + 1.0
   y = deltalat/latinc + 1.0

   if (trace_use_frequent) call da_trace_exit("da_llxy_latlon")

end subroutine da_llxy_latlon


