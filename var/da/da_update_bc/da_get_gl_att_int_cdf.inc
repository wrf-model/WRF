subroutine da_get_gl_att_int_cdf(file, att_name, value, debug, return_code)
 
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
        
   implicit none

#include "netcdf.inc"

   character (len=*),  intent(in)  :: file
   character (len=*),  intent(in)  :: att_name
   logical,            intent(in)  :: debug
   integer,            intent(out) :: value
   integer, optional,  intent(out) :: return_code

   integer :: cdfid, rcode

   ! if (trace_use_dull) call da_trace_entry("da_get_gl_att_int_cdf")

   cdfid = ncopn(file, NCNOWRIT, rcode)

   if (rcode == 0) then
     if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
     write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
     stop
   end if

   rcode = NF_GET_ATT_inT(cdfid, nf_global, att_name, value)

   if ( present(return_code) ) then
      return_code = rcode
   end if

   call ncclos(cdfid,rcode)

   if (debug) write(unit=stdout,fmt=*) ' global attribute ',att_name,' is ',value

   ! if (trace_use_dull) call da_trace_exit("da_get_gl_att_int_cdf")

end subroutine da_get_gl_att_int_cdf


