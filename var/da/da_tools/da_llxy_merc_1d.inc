subroutine da_llxy_merc_1d(lat, lon, proj, x, y)

   !-----------------------------------------------------------------------
   ! Purpose: Compute x,y coordinate from lat lon for mercator projection
   !-----------------------------------------------------------------------
  
   implicit none

   real, intent(in)              :: lat(:)
   real, intent(in)              :: lon(:)
   type(proj_info),intent(in)    :: proj
   real,intent(out)              :: x(:)
   real,intent(out)              :: y(:)
   real, allocatable             :: deltalon(:)
   integer                       :: n

   if (trace_use_frequent) call da_trace_entry("da_llxy_merc_1d")

   n = size(lat)
   allocate ( deltalon(n) )

   deltalon = lon - proj%lon1
   where (deltalon < -180.0) deltalon = deltalon + 360.0
   where (deltalon > 180.0) deltalon = deltalon - 360.0
   x = 1.0 + (deltalon/(proj%dlon*deg_per_rad))
   y = 1.0 + (ALOG(TAN(0.5*((lat + 90.0) * rad_per_deg)))) / &
           proj%dlon - proj%rsw

   deallocate ( deltalon )

   if (trace_use_frequent) call da_trace_exit("da_llxy_merc_1d")

end subroutine da_llxy_merc_1d


