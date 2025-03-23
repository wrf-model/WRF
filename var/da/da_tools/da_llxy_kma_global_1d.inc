subroutine da_llxy_kma_global_1d(lat,lon,x,y)

   !----------------------------------------------------------------------------
   ! Purpose:  calculates the(x,y) location(dot) in the global grids
   !           from latitudes and longitudes
   !----------------------------------------------------------------------------
   
   implicit none
   
   real, intent(in)  :: lat(:), lon(:)
   real, intent(out) :: x(:), y(:)

   real, allocatable :: xlat(:), xlon(:)
   integer           :: n

   if (trace_use_frequent) call da_trace_entry("da_llxy_kma_global_1d")

   n = size(lat)
   allocate ( xlat(n), xlon(n) ) 

   xlat = lat - start_lat
   xlon = lon - start_lon

   where (xlat < 0.0) xlat = xlat + 180.0
   where (xlon < 0.0) xlon = xlon + 360.0

   x = start_x + xlon/delt_lon
   y = start_y + xlat/delt_lat

   deallocate ( xlat, xlon )

   if (trace_use_frequent) call da_trace_exit("da_llxy_kma_global_1d")
   
end subroutine da_llxy_kma_global_1d


