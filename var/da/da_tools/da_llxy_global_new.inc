subroutine da_llxy_global_new(info)

   !----------------------------------------------------------------------------
   ! Purpose:  calculates the(x,y) location(dot) in the global grids
   !           from latitudes and longitudes
   !----------------------------------------------------------------------------
   
   implicit none

   type(infa_type), intent(inout) :: info

   real    :: xlat, xlon
   integer :: n

   if (trace_use) call da_trace_entry("da_llxy_global_new")

!FAST

!   where (lat(:,:) - start_lat < 0)
!      y(:,:) = start_y + (lat(:,:) - start_lat+180.0)/delt_lat
!   else
!      y(:,:) = start_y + (lat(:,:) - start_lat)/delt_lat
!   end where

!   where (lon(:,:) - start_lon < 0.)
!      x(:,:) = start_x + (lon(:,:) - start_lon+360.0)/delt_lon
!   else
!      x(:,:) = start_x + (lon(:,:) - start_lon)/delt_lon
!   end where

! SLOW

   do n=lbound(info%lat,2),ubound(info%lat,2)
      xlat = info%lat(1,n) - start_lat
      xlon = info%lon(1,n) - start_lon
      if (xlat < 0.0) xlat = xlat + 180.0
      if (xlon < 0.0) xlon = xlon + 360.0
      info%x(:,n) = start_x + xlon/delt_lon
      info%y(:,n) = start_y + xlat/delt_lat
      if((fg_format == fg_format_wrf_arw_global) .and. (info%lat(1,n).le.start_lat)) info%y(:,n) = 1.0                       
   end do

   if (trace_use) call da_trace_exit("da_llxy_global_new")
   
end subroutine da_llxy_global_new


