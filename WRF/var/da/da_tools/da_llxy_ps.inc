subroutine da_llxy_ps(lat,lon,proj,x,y)

   !-----------------------------------------------------------------------
   ! Purpose: Given latitude (-90 to 90), longitude (-180 to 180), and the
   ! standard polar-stereographic projection information via the 
   ! public proj structure, this routine returns the x/y indices which
   ! if within the domain range from 1->nx and 1->ny, respectively.
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)               :: lat
   real, intent(in)               :: lon
   type(proj_info),intent(in)     :: proj

   real, intent(out)              :: x !(x-index)
   real, intent(out)              :: y !(y-index)
   
   real                           :: reflon
   real                           :: scale_top
   real                           :: ala
   real                           :: alo
   real                           :: rm

   if (trace_use_frequent) call da_trace_entry("da_llxy_ps")

   reflon = proj%stdlon + 90.0
   
   ! Compute numerator term of map scale factor

   scale_top = 1.0 + proj%hemi * Sin(proj%truelat1 * rad_per_deg)

   ! Find radius to desired point
   ala = lat * rad_per_deg
   rm = proj%rebydx * COS(ala) * scale_top/(1.0 + proj%hemi *Sin(ala))
   alo = (lon - reflon) * rad_per_deg
   x = proj%polei + rm * COS(alo)
   y = proj%polej + proj%hemi * rm * Sin(alo)

   if (trace_use_frequent) call da_trace_exit("da_llxy_ps")
 
end subroutine da_llxy_ps


