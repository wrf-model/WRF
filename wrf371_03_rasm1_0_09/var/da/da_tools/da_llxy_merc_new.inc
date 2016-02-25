subroutine da_llxy_merc_new(proj, info)

   !-----------------------------------------------------------------------
   ! Purpose: Compute x/y coordinate from lat lon for mercator projection
   !-----------------------------------------------------------------------
  
   implicit none

   type(proj_info), intent(in)    :: proj
   type(infa_type), intent(inout) :: info

   real :: deltalon
   integer :: n

if (trace_use) call da_trace_entry("da_llxy_merc_new")

! FAST

!   where (lon(:,:) - proj%lon1 < -180.0)
!      x(:,:) = 1.0 + (lon(:,:) - proj%lon1 + 360.0)/(proj%dlon*deg_per_rad))
!   elsewhere (lon(:,:) - proj%lon1 > 180.0)
!      x(:,:) = 1.0 + (lon(:,:) - proj%lon1 - 360.0)/(proj%dlon*deg_per_rad))
!   else
!      x(:,:) = 1.0 + (lon(:,:) - proj%lon1)/(proj%dlon*deg_per_rad))
!   end where

!   y(:,:) = 1.0 + (ALOG(TAN(0.5*((lat(:,: + 90.0) * rad_per_deg)))) / proj%dlon - proj%rsw

! SLOW

   do n=lbound(info%lat,2),ubound(info%lat,2)
      deltalon = info%lon(1,n) - proj%lon1
      if (deltalon < -180.0) deltalon = deltalon + 360.0
      if (deltalon > 180.0) deltalon = deltalon - 360.0
      info%x(:,n) = 1.0 + (deltalon/(proj%dlon*deg_per_rad))
      info%y(:,n) = 1.0 + (ALOG(TAN(0.5*((info%lat(1,n) + 90.0) * rad_per_deg)))) / proj%dlon - proj%rsw
   end do

if (trace_use) call da_trace_exit("da_llxy_merc_new")


end subroutine da_llxy_merc_new


