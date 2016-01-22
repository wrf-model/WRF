subroutine da_llxy_kma_global(lat,lon,x,y)

   !----------------------------------------------------------------------------
   ! Purpose:  calculates the(x,y) location(dot) in the global grids
   !           from latitudes and longitudes
   !----------------------------------------------------------------------------
   
   implicit none
   
   real, intent(in)  :: lat, lon
   real, intent(out) :: x, y

   real              :: xlat, xlon

   if (trace_use_frequent) call da_trace_entry("da_llxy_kma_global")
   
   xlat = lat - start_lat
   xlon = lon - start_lon

   if (xlat < 0.0) xlat = xlat + 180.0
   if (xlon < 0.0) xlon = xlon + 360.0

   x = start_x + xlon/delt_lon
   y = start_y + xlat/delt_lat

   if (trace_use_frequent) call da_trace_exit("da_llxy_kma_global")
   
end subroutine da_llxy_kma_global


