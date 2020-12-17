subroutine da_set_ps(proj)

   ! Initializes a polar-stereographic map projection from the partially
   ! filled proj structure. This routine computes the radius to the
   ! southwest corner and computes the i/j location of the pole for use
   ! in llij_ps and ijll_ps.

   implicit none
 
   type(proj_info), intent(inout)    :: proj

   real :: ala1
   real :: alo1
   real :: reflon
   real :: scale_top

   if (trace_use) call da_trace_entry("da_set_ps")

   ! To define the cone factor for polar stereographic projection 
   proj%cone = 1.0

   reflon = proj%stdlon + 90.0

   ! Compute numerator term of map scale factor
   scale_top = 1.0 + proj%hemi * Sin(proj%truelat1 * rad_per_deg)

   ! Compute radius to lower-left (SW) corner
   ala1 = proj%lat1 * rad_per_deg
   proj%rsw = proj%rebydx*COS(ala1)*scale_top/(1.0+proj%hemi*Sin(ala1))

   ! Find the pole point
   alo1 = (proj%lon1 - reflon) * rad_per_deg
   proj%polei = proj%knowni - proj%rsw * COS(alo1)
   proj%polej = proj%knownj - proj%hemi * proj%rsw * Sin(alo1)
   if (print_detail_map) then
      write(unit=stdout,fmt='(A,2F10.1)') 'Computed (I,J) of pole point: ',proj%polei,proj%polej
   end if

   if (trace_use) call da_trace_exit("da_set_ps")

end subroutine da_set_ps


