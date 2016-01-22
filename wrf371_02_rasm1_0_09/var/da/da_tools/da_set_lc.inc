subroutine da_set_lc(proj)

   !-----------------------------------------------------------------------
   ! Purpose: Initialize the remaining items in the proj structure for a
   ! lambert conformal grid.
   !-----------------------------------------------------------------------

   implicit none
    
   type(proj_info), intent(inout) :: proj

   real :: arg
   real :: deltalon1
   real :: tl1r
   real :: ctl1r

   if (trace_use_dull) call da_trace_entry("da_set_lc")

   ! Compute cone factor
   call da_lc_cone(proj%truelat1, proj%truelat2, proj%cone)
   if (print_detail_map) then
      write(unit=stdout, fmt='(A,F8.6)') 'Computed cone factor: ', proj%cone
   end if
   ! Compute longitude differences and ensure we stay out of the
   ! forbidden "cut zone"
   deltalon1 = proj%lon1 - proj%stdlon
   if (deltalon1 .gt. +180.0) deltalon1 = deltalon1 - 360.0
   if (deltalon1 .lt. -180.0) deltalon1 = deltalon1 + 360.0

   ! Convert truelat1 to radian and compute COS for later use
   tl1r = proj%truelat1 * rad_per_deg
   ctl1r = COS(tl1r)

   ! Compute the radius to our known lower-left (SW) corner
   proj%rsw = proj%rebydx * ctl1r/proj%cone * &
           (TAN((90.0*proj%hemi-proj%lat1)*rad_per_deg/2.0) / &
            TAN((90.0*proj%hemi-proj%truelat1)*rad_per_deg/2.0))**proj%cone

   ! Find pole point
   arg = proj%cone*(deltalon1*rad_per_deg)
   proj%polei = 1.0 - proj%hemi * proj%rsw * Sin(arg)
   proj%polej = 1.0 + proj%rsw * COS(arg)  
   if (print_detail_map) then
      write(unit=stdout,fmt='(A,2F10.3)') 'Computed pole i/j = ', proj%polei, proj%polej
   end if

   if (trace_use_dull) call da_trace_exit("da_set_lc")

end subroutine da_set_lc                             


