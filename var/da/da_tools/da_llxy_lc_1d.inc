subroutine da_llxy_lc_1d(lat, lon, proj, x, y)

   !-----------------------------------------------------------------------
   ! Purpose: compute the geographical latitude and longitude values
   ! to the cartesian x/y on a Lambert Conformal projection.
   !-----------------------------------------------------------------------
    
   implicit none

   real, intent(in)              :: lat(:)      ! Latitude (-90->90 deg N)
   real, intent(in)              :: lon(:)      ! Longitude (-180->180 E)
   type(proj_info),intent(in)    :: proj        ! Projection info structure

   real, intent(out)             :: x(:)        ! Cartesian X coordinate
   real, intent(out)             :: y(:)        ! Cartesian Y coordinate

   real, allocatable             :: arg(:)
   real, allocatable             :: deltalon(:)
   real                          :: tl1r
   real, allocatable             :: rm(:)
   real                          :: ctl1r
   integer                       :: n

   if (trace_use_dull) call da_trace_entry("da_llxy_lc_1d")

   n = size(lat)
   allocate ( arg(n), deltalon(n), rm(n) )

   ! Compute deltalon between known longitude and standard lon and ensure
   ! it is not in the cut zone
   deltalon = lon - proj%stdlon
   where (deltalon > +180.0) deltalon = deltalon - 360.0
   where (deltalon < -180.0) deltalon = deltalon + 360.0
    
   ! Convert truelat1 to radian and compute COS for later use
   tl1r = proj%truelat1 * rad_per_deg
   ctl1r = COS(tl1r)     
   
   ! Radius to desired point
   rm = proj%rebydx * ctl1r/proj%cone * &
       (TAN((90.0*proj%hemi-lat)*rad_per_deg/2.0) / &
        TAN((90.0*proj%hemi-proj%truelat1)*rad_per_deg/2.0))**proj%cone

   arg = proj%cone*(deltalon*rad_per_deg)
   x = proj%polei + proj%hemi * rm * Sin(arg)
   y = proj%polej - rm * COS(arg)

   ! Finally, if we are in the southern hemisphere, flip the i/j
   ! values to a coordinate system where (1,1) is the SW corner
   ! (what we assume) which is different than the original NCEP
   ! algorithms which used the NE corner as the origin in the 
   ! southern hemisphere (left-hand vs. right-hand coordinate?)
   if (proj%hemi == -1.0) then
      x = 2.0 - x  
      y = 2.0 - y
   end if

   deallocate ( arg, deltalon, rm )

   if (trace_use_dull) call da_trace_exit("da_llxy_lc_1d")

end subroutine da_llxy_lc_1d


