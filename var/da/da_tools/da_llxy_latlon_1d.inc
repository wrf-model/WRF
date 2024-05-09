subroutine da_llxy_latlon_1d(lat, lon, proj, x, y)

   !----------------------------------------------------------------------- 
   ! Purpose: Compute the x/y location of a lat/lon on a LATLON 
   !          (cylindrical equidistant) grid.
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)             :: lat(:)
   real, intent(in)             :: lon(:)
   type(proj_info), intent(in)  :: proj
   real, intent(out)            :: x(:)
   real, intent(out)            :: y(:)

   real, allocatable            :: deltalat(:)
   real, allocatable            :: deltalon(:)
   real, allocatable            :: lon360(:)
   real                         :: latinc
   real                         :: loninc
   integer                      :: n

   if (trace_use_frequent) call da_trace_entry("da_llxy_latlon_1d")

   n = size(lat)
   allocate ( deltalat(n), deltalon(n), lon360(n) )

   ! To account for issues around the dateline, convert the incoming
   ! longitudes to be 0->360.0
   where (lon < 0)
      lon360 = lon + 360.0
   elsewhere
      lon360 = lon
   end where

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

   deallocate ( deltalat, deltalon, lon360 )

   if (trace_use_frequent) call da_trace_exit("da_llxy_latlon_1d")

end subroutine da_llxy_latlon_1d


