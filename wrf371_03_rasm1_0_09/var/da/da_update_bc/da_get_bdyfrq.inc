subroutine da_get_bdyfrq(this_datestr, next_datestr, bdyfrq, debug)
 
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
         
   implicit none

#include "netcdf.inc"

   character(len=*), intent(in)  :: this_datestr, next_datestr
   real,              intent(out) :: bdyfrq
   logical,           intent(in)  :: debug

   real    :: this_bdy_time, next_bdy_time

   ! if (trace_use) call da_trace_entry("da_get_bdyfrq")

   call da_atotime(this_datestr, this_bdy_time)
   call da_atotime(next_datestr, next_bdy_time)

   bdyfrq = next_bdy_time - this_bdy_time

   if (debug) then
      write(unit=stdout, fmt='(a, f20.1)') &
           'next_bdy_time=', next_bdy_time, &
           'this_bdy_time=', this_bdy_time, &
           'bdyfrq       =', bdyfrq
   end if

   ! if (trace_use) call da_trace_exit("da_get_bdyfrq")

end subroutine da_get_bdyfrq


