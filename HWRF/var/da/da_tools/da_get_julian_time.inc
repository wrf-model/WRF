subroutine da_get_julian_time(year,month,day,hour,minute,gstime)

   !------------------------------------------------------------------------------
   ! Purpose: Calculate Julian time from year/month/day/hour/minute.
   !------------------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: year
   integer, intent(in)  :: month
   integer, intent(in)  :: day
   integer, intent(in)  :: hour
   integer, intent(in)  :: minute
   real*8,  intent(out) :: gstime

   integer    :: iw3jdn, ndays, nmind

   if (trace_use) call da_trace_entry("da_get_julian_time")

   iw3jdn  =    day - 32075 &
              + 1461 * (year + 4800 + (month - 14) / 12) / 4 &
              + 367 * (month - 2 - (month - 14) / 12 * 12) / 12 &
              - 3 * ((year + 4900 + (month - 14) / 12) / 100) / 4
   ndays = iw3jdn - 2443510

   nmind = ndays*1440 + hour * 60 + minute
   gstime = float(nmind)

   if (trace_use) call da_trace_exit("da_get_julian_time")

end subroutine da_get_julian_time
