subroutine da_llxy_latlon_new(proj, info)

   !----------------------------------------------------------------------- 
   ! Purpose: Compute the x/y location of a lat/lon on a LATLON grid.
   !-----------------------------------------------------------------------

   implicit none

   type(proj_info), intent(in)    :: proj
   type(infa_type), intent(inout) :: info

   integer :: n

   if (trace_use) call da_trace_entry("da_llxy_latlon_new")

   ! Extract the latitude and longitude increments for this grid
   ! (e.g., 2.5 deg for NCEP reanalysis) from the proj structure, where
   ! loninc is saved in the stdlon tag and latinc is saved in truelat1

   ! Compute the difference between the input lat/lon and the origin lat/lon

   info%y(1,:) = (info%lat(1,:) - proj%lat1)/proj%truelat1 + 1.0

   ! To account for issues around the dateline, convert the incoming
   ! longitudes to be 0->360.0
   where (info%lon(1,:) < 0)
      info%x(1,:) = (info%lon(1,:) + 360.0  - proj%lon1)/proj%stdlon + 1.0
   elsewhere
      info%x(1,:) = (info%lon(1,:) - proj%lon1)/proj%stdlon + 1.0
   end where

   do n=1,ubound(info%x,2)
      info%x(:,n) = info%x(1,n)
      info%y(:,n) = info%y(1,n)
   end do

   if (trace_use) call da_trace_exit("da_llxy_latlon_new")

end subroutine da_llxy_latlon_new


