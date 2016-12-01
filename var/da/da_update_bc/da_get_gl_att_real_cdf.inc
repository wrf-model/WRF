subroutine da_get_gl_att_real_cdf(file, att_name, value, debug, return_code)
 
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
        
   implicit none

#include "netcdf.inc"

   character (len=*), intent(in)  :: file
   character (len=*),  intent(in)  :: att_name
   logical,            intent(in)  :: debug
   real,               intent(out) :: value
   integer, optional,  intent(out) :: return_code

   real(kind=8)         :: tmp
   real(kind=4)         :: tmp4
   integer              :: cdfid, rcode, ivtype

   ! if (trace_use_dull) call da_trace_entry("da_get_gl_att_real_cdf")

   cdfid = ncopn(file, NCNOWRIT, rcode)

   if (rcode == 0) then
     if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
     write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
     stop
   end if

   rcode = NF_inQ_ATTtype(cdfid, nf_global, att_name, ivtype)

   write(unit=stdout, fmt='(a, i6)') &
        'ivtype:', ivtype, &
        'NF_real=', NF_real, &
        'NF_DOUBLE=', NF_DOUBLE, &
        'kind(value)=', kind(value)

   if ((ivtype == NF_real) .and. (kind(value) == 4)) then
      rcode = NF_GET_ATT_real(cdfid, nf_global, att_name, value)
   else if ((ivtype == NF_DOUBLE) .and. (kind(value) == 4)) then
      rcode = NF_GET_ATT_real(cdfid, nf_global, att_name, tmp)
      value = tmp
   else if ((ivtype == NF_DOUBLE) .and. (kind(value) == 8)) then
      rcode = NF_GET_ATT_real(cdfid, nf_global, att_name, value)
   else if ((ivtype == NF_REAL) .and. (kind(value) == 8)) then
      rcode = NF_GET_ATT_real(cdfid, nf_global, att_name, tmp4)
      value = tmp4
   else
      write(unit=stdout, fmt='(a, i6)') &
         'Unrecognizable ivtype:', ivtype
      stop
   end if

   if ( present(return_code) ) then
      return_code = rcode
   end if

   call ncclos(cdfid,rcode)

   if (debug) write(unit=stdout,fmt=*) ' global attribute ',att_name,' is ',value

   ! if (trace_use_dull) call da_trace_exit("da_get_gl_att_real_cdf")

end subroutine da_get_gl_att_real_cdf


