subroutine da_put_att_cdf(file, var, att_name, text, debug)
 
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

#include "netcdf.inc"

   character (len=*),  intent(in) :: file
   character (len=*),  intent(in) :: var, att_name, text
   logical,            intent(in) :: debug

   integer :: cdfid, status, varid, n, natts
   character (len=256) :: loc_att_name


   ! not for update_bc
   ! if (trace_use) call da_trace_entry("da_put_att_cdf")

   status = NF_OPEN(file, NF_WRITE, cdfid)

   if (status == 0) then
      if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
      write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
      stop
   end if

   status = NF_inQ_VARID(cdfid, var, varid)

   status = NF_inQ_VARNATTS(cdfid, varid, natts)

   do n=1, natts
      status = NF_inQ_ATTNAME(cdfid, varid, n, loc_att_name)

      write(unit=stdout, fmt='(a,i2,2a)') &
        'loc_att_name(',n,')=', trim(loc_att_name)

      if (trim(loc_att_name) == trim(att_name)) then
         write(unit=stdout, fmt='(2a)') &
           'att_name=', trim(att_name)

         status = NF_PUT_ATT_TEXT(cdfid, varid, trim(att_name), len(text), trim(text))

         if (status == 0) then
            if (debug) then
               write(unit=stdout, fmt='(4a)') &
                    'write ', trim(att_name), 'to netcdf file ', trim(file)
            end if
         else
            write(unit=stdout, fmt='(a, i8)') &
                   'Status= ', status

            write(unit=stdout, fmt='(4a)') &
                   'Failed to write ', trim(att_name), 'to netcdf file ', trim(file)

            ! if (status /= NF_NOERR) call handle_err(status)
            stop
         end if

         exit
      end if
   end do

   status = NF_close(cdfid)

   !if (trace_use) call da_trace_exit("da_put_att_cdf")

end subroutine da_put_att_cdf


