subroutine da_lc_cone(truelat1, truelat2, cone)

   !------------------------------------------------------------------------
   ! Purpose: compute the cone factor of a Lambert Conformal projection
   !------------------------------------------------------------------------

   implicit none
    
   real, intent(in)             :: truelat1  ! (-90 -> 90 degrees N)
   real, intent(in)             :: truelat2  !   "   "  "   "     "
   real, intent(out)            :: cone

   if (trace_use_dull) call da_trace_entry("da_lc_cone")

   ! First, see if this is a secant or tangent projection.  For tangent
   ! projections, truelat1 = truelat2 and the cone is tangent to the 
   ! Earth's surface at this latitude.  For secant projections, the cone
   ! intersects the Earth's surface at each of the distinctly different
   ! latitudes
   if (abs(truelat1-truelat2) > 0.1) then
      cone = alog10(cos(truelat1*rad_per_deg)) - &
             alog10(cos(truelat2*rad_per_deg))
      cone = cone /(alog10(tan((45.0 - abs(truelat1)/2.0) * rad_per_deg)) - &
             alog10(tan((45.0 - abs(truelat2)/2.0) * rad_per_deg)))        
   else
      cone = sin(abs(truelat1)*rad_per_deg)  
   end if

   if (trace_use_dull) call da_trace_exit("da_lc_cone")

end subroutine da_lc_cone


