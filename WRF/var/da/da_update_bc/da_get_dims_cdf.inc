subroutine da_get_dims_cdf(file, var, dims, ndims, debug)
 
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
        
   implicit none

#include "netcdf.inc"

   character (len=*),  intent(in)  :: file
   character (len=*),  intent(in)  :: var
   logical,            intent(in)  :: debug
   integer,            intent(out) :: dims(4)
   integer,            intent(out) :: ndims

   integer            :: cdfid, rcode, id_time
   character (len=80) :: varnam
   integer            :: natts, dimids(10)
   integer            :: i, ivtype

   ! if (trace_use) call da_trace_entry("da_get_dims_cdf")

   cdfid = ncopn(file, NCNOWRIT, rcode)

   if (rcode == 0) then
      if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
      write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
      stop
   end if

   id_time = ncvid(cdfid, var, rcode)

   rcode = nf_inq_var(cdfid, id_time, varnam, ivtype, ndims, dimids, natts)
   if (debug) then
      write(unit=stdout,fmt=*) ' number of dims for ',var,' ',ndims
   end if
   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), dims(i))
      if (debug) write(unit=stdout,fmt=*) ' dimension ',i,dims(i)
   end do

   call ncclos(cdfid,rcode)

   ! if (trace_use) call da_trace_exit("da_get_dims_cdf")

end subroutine da_get_dims_cdf


