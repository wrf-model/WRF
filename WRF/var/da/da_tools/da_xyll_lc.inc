subroutine da_xyll_lc(x, y, proj, lat, lon)

   ! subroutine da_to convert from the (x,y) cartesian coordinate to the 
   ! geographical latitude and longitude for a Lambert Conformal projection.

   implicit none

   real, intent(in)              :: x        ! Cartesian X coordinate
   real, intent(in)              :: y        ! Cartesian Y coordinate
   type(proj_info),intent(in)    :: proj     ! Projection info structure

                
   real, intent(out)             :: lat      ! Latitude (-90->90 deg N)
   real, intent(out)             :: lon      ! Longitude (-180->180 E)

   real                          :: inew
   real                          :: jnew
   real                          :: r
   real                          :: chi,chi1,chi2
   real                          :: r2
   real                          :: xx
   real                          :: yy

   if (trace_use_dull) call da_trace_entry("da_xyll_lc")


   chi1 = (90.0 - proj%hemi*proj%truelat1)*rad_per_deg
   chi2 = (90.0 - proj%hemi*proj%truelat2)*rad_per_deg

   ! See if we are in the southern hemispere and flip the indices
   ! if we are. 
   if (proj%hemi == -1.0) then 
      inew = -x + 2.0
      jnew = -y + 2.0
   else
      inew = x
      jnew = y
   end if

   ! Compute radius**2 to i/j location
   xx = inew - proj%polei
   yy = proj%polej - jnew
   r2 = (xx*xx + yy*yy)
   r = sqrt(r2)/proj%rebydx
   
   ! Convert to lat/lon
   if (r2 == 0.0) then
      lat = proj%hemi * 90.0
      lon = proj%stdlon
   else
       
      ! Longitude
      lon = proj%stdlon + deg_per_rad * ATAN2(proj%hemi*xx,yy)/proj%cone
      lon = MOD(lon+360.0, 360.0)

      ! Latitude.  Latitude determined by solving an equation adapted 
      ! from:
      !  Maling, D.H., 1973: Coordinate Systems and Map Projections
      ! Equations #20 in Appendix I.  
        
      if (chi1 == chi2) then
         chi = 2.0*ATAN((r/TAN(chi1))**(1.0/proj%cone) * TAN(chi1*0.5))
      else
         chi = 2.0*ATAN((r*proj%cone/Sin(chi1))**(1.0/proj%cone) * TAN(chi1*0.5)) 
      end if
      lat = (90.0-chi*deg_per_rad)*proj%hemi
   end if

   if (lon > +180.0) lon = lon - 360.0
   if (lon < -180.0) lon = lon + 360.0

   if (trace_use_dull) call da_trace_exit("da_xyll_lc")

end subroutine da_xyll_lc


