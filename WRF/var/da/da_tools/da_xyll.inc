subroutine da_xyll(proj, xx, yy, lat, lon)

   !-----------------------------------------------------------------------
   ! Purpose: Computes geographical latitude and longitude for a given (i,j) 
   ! point in a grid with a projection of proj
   !-----------------------------------------------------------------------

   implicit none

   type(proj_info), intent(in)  :: proj
   real,            intent(in)  :: xx
   real,            intent(in)  :: yy
   real,            intent(out) :: lat
   real,            intent(out) :: lon

   real :: x, y

   if (trace_use) call da_trace_entry("da_xyll")

   if (.NOT.proj%init) then
      call da_error(__FILE__,__LINE__, &
         (/"You have not called map_set for this projection!"/))
   end if

   x = xx
   y = yy

   select case (proj%code)
      case (PROJ_LATLON)
         call da_xyll_latlon(x, y, proj, lat, lon)

      case (PROJ_MERC)
         x = xx - proj%knowni + 1.0
         y = yy - proj%knownj + 1.0
         call da_xyll_merc(x, y, proj, lat, lon)

      case (PROJ_PS)
         call da_xyll_ps(x, y, proj, lat, lon)

      case (PROJ_LC)

         x = xx - proj%knowni + 1.0
         y = yy - proj%knownj + 1.0
         call da_xyll_lc(x, y, proj, lat, lon)

      case default
         write(unit=message(1),fmt='(A,I2)') &
            "Unrecognized map projection code: ", proj%code
         call da_error(__FILE__,__LINE__,message(1:1))

   end select

   if (trace_use) call da_trace_exit("da_xyll")

end subroutine da_xyll


