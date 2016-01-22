subroutine da_get_att_cdf(file, var, debug)
 
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

#include "netcdf.inc"

   character (len=*),  intent(in) :: file
   character (len=*),  intent(in) :: var
   logical,            intent(in) :: debug

   integer             :: cdfid, status, varid, n, natts
   character (len=256) :: att_name

   ! if (trace_use) call da_trace_entry("da_get_att_cdf")

   status = NF_OPEN(file, NF_NOWRITE, cdfid)

   status = NF_inQ_VARID(cdfid, var, varid)

   if (status == 0) then
      if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
      write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
      stop
   end if

   status = NF_inQ_VARNATTS(cdfid, varid, natts)

   do n=1, natts
      status = NF_inQ_ATTNAME(cdfid, varid, n, att_name)

      write(unit=stdout, fmt='(a,i2,2a)') &
        'att_name(',n,')=', trim(att_name)
   end do

   status = NF_close(cdfid)

   ! if (trace_use) call da_trace_exit("da_get_att_cdf")

end subroutine da_get_att_cdf


