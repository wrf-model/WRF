subroutine da_llxy_global(lat,lon,x,y)

   !----------------------------------------------------------------------------
   ! Purpose:  calculates the(x,y) location(dot) in the global grids
   !           from latitudes and longitudes
   !----------------------------------------------------------------------------
   
   implicit none
   
   real, intent(in)  :: lat, lon
   real, intent(out) :: x, y

   real              :: xlat, xlon

   if (trace_use_frequent) call da_trace_entry("da_llxy_global")
   xlat = lat - start_lat
   xlon = lon - start_lon
   if (xlat < 0.0) xlat = xlat + 180.0
   if (xlon < 0.0) xlon = xlon + 360.0

   x = start_x + xlon/delt_lon
   y = start_y + xlat/delt_lat
   if((fg_format == fg_format_wrf_arw_global) .and. (lat.le.start_lat)) y = 1.0 

   if (trace_use_frequent) call da_trace_exit("da_llxy_global")
   
end subroutine da_llxy_global
