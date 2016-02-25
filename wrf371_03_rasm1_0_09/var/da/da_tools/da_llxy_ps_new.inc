subroutine da_llxy_ps_new(proj,info)

   !-----------------------------------------------------------------------
   ! Purpose: Given latitude (-90 to 90), longitude (-180 to 180), and the
   ! standard polar-stereographic projection information via the 
   ! public proj structure, this routine returns the x/y indices which
   ! if within the domain range from 1->nx and 1->ny, respectively.
   !-----------------------------------------------------------------------

   implicit none

   type(proj_info), intent(in)    :: proj
   type(infa_type), intent(inout) :: info
   
   real    :: reflon
   integer :: n
   real    :: scale_top, ala, rm, alo

   if (trace_use) call da_trace_entry("da_llxy_ps_new")

   reflon = proj%stdlon + 90.0

   ! FAST

!   x(:,:) = proj%polei + proj%rebydx * COS(lat(:,:) * rad_per_deg) &
!      * (1.0 + proj%hemi * SIN(proj%truelat1 * rad_per_deg) &
!      / (1.0 + proj%hemi *SIN(lat(:,:) * rad_per_deg)) &
!      * COS((lon(:,:) - reflon) * rad_per_deg)

!   y(:,:) = proj%polej + proj%hemi * proj%rebydx * COS(lat(:,:) * rad_per_deg) &
!      * (1.0 + proj%hemi * SIN(proj%truelat1 * rad_per_deg) &
!      / (1.0 + proj%hemi *SIN(lat(:,:) * rad_per_deg)) &
!      * SIN((lon(:,:) - reflon) * rad_per_deg)

! SLOW

   do n=lbound(info%lat,2),ubound(info%lat,2)
      ! compute numerator term of map scale factor

      scale_top = 1.0 + proj%hemi * sin(proj%truelat1 * rad_per_deg)

      ! find radius to desired point
      ala = info%lat(1,n) * rad_per_deg
      rm = proj%rebydx * cos(ala) * scale_top/(1.0 + proj%hemi *sin(ala))
      alo = (info%lon(1,n) - reflon) * rad_per_deg
      info%x(:,n) = proj%polei + rm * cos(alo)
      info%y(:,n) = proj%polej + proj%hemi * rm * sin(alo)
   end do

   if (trace_use) call da_trace_exit("da_llxy_ps_new")
 
end subroutine da_llxy_ps_new


