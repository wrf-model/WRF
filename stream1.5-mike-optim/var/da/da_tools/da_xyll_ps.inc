subroutine da_xyll_ps(x, y, proj, lat, lon)

   ! This is the inverse subroutine da_of llij_ps.  It returns the 
   ! latitude and longitude of an x/y point given the projection info 
   ! structure.  

   implicit none

   real, intent(in)                    :: x    ! Column
   real, intent(in)                    :: y    ! Row
   type (proj_info), intent(in)        :: proj
    
   real, intent(out)                   :: lat     ! -90 -> 90 North
   real, intent(out)                   :: lon     ! -180 -> 180 East

   real                                :: reflon
   real                                :: scale_top
   real                                :: xx,yy
   real                                :: gi2, r2
   real                                :: arccos

   if (trace_use_frequent) call da_trace_entry("da_xyll_ps")

   ! Compute the reference longitude by rotating 90 degrees to the east
   ! to find the longitude line parallel to the positive x-axis.
   reflon = proj%stdlon + 90.0
   
   ! Compute numerator term of map scale factor
   scale_top = 1.0 + proj%hemi * Sin(proj%truelat1 * rad_per_deg)

   ! Compute radius to point of interest
   xx = x - proj%polei
   yy = (y - proj%polej) * proj%hemi
   r2 = xx**2 + yy**2

   ! Now the magic code
   if (r2 == 0.0) then 
      lat = proj%hemi * 90.0
      lon = reflon
   else
      gi2 = (proj%rebydx * scale_top)**2.0
      lat = deg_per_rad * proj%hemi * ASin((gi2-r2)/(gi2+r2))
      arccos = ACOS(xx/sqrt(r2))
      if (yy > 0) then
         lon = reflon + deg_per_rad * arccos
      else
         lon = reflon - deg_per_rad * arccos
      end if
   end if
  
   ! Convert to a -180 -> 180 East convention
   if (lon > 180.0) lon = lon - 360.0
   if (lon < -180.0) lon = lon + 360.0

   if (trace_use_frequent) call da_trace_exit("da_xyll_ps")
     
end subroutine da_xyll_ps


