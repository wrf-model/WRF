subroutine da_llxy_wrf_1d(proj, lat, lon, x, y)

   !-----------------------------------------------------------------------
   ! Purpose: Converts input lat/lon values to the cartesian (x, y) value
   ! for the given projection. 
   !-----------------------------------------------------------------------

   implicit none

   type(proj_info), intent(in)  :: proj
   real,            intent(in)  :: lat(:)
   real,            intent(in)  :: lon(:)
   real,            intent(out) :: x(:)
   real,            intent(out) :: y(:)

   if (trace_use_frequent) call da_trace_entry("da_llxy_wrf_1d")

   if (.NOT.proj%init) then
      call da_error(__FILE__,__LINE__, &
        (/"You have not called map_set for this projection!"/))
   end if

   select case(proj%code)
 
      case(PROJ_LATLON)
         call da_llxy_latlon_1d(lat,lon,proj,x,y)

      case(PROJ_MERC)
         call da_llxy_merc_1d(lat,lon,proj,x,y)
         x = x + proj%knowni - 1.0
         y = y + proj%knownj - 1.0

      case(PROJ_PS)
         call da_llxy_ps_1d(lat,lon,proj,x,y)
      
      case(PROJ_LC)
         call da_llxy_lc_1d(lat,lon,proj,x,y)
         x = x + proj%knowni - 1.0
         y = y + proj%knownj - 1.0

      case default
         write(unit=message(1),fmt='(A,I2)') &
            'Unrecognized map projection code: ', proj%code
         call da_error(__FILE__,__LINE__,message(1:1))
   end select

   if (trace_use_frequent) call da_trace_exit("da_llxy_wrf_1d")

end subroutine da_llxy_wrf_1d


