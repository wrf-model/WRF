subroutine da_get_var_1d_real_cdf(file, var, data, i1, time, debug)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

#include "netcdf.inc"

   integer,            intent(in)  :: i1, time
   character (len=*),  intent(in)  :: file
   logical,            intent(in)  :: debug
   character (len=*),  intent(in)  :: var
   real,               intent(out) :: data(i1)

   real(kind=8)       :: tmp(i1)
   real(kind=4)       :: tmp4(i1)
   integer            :: cdfid, rcode, id_data
   character (len=80) :: varnam
   integer            :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
   integer            :: i, ivtype

   ! if (trace_use) call da_trace_entry("da_get_var_1d_real_cdf")

   cdfid = ncopn(file, NCNOWRIT, rcode)

   if (rcode /= 0) then
      write(unit=stdout, fmt='(2a)') ' error openiing netcdf file ', trim(file)
      stop
   end if

   id_data = ncvid(cdfid, var, rcode)

   rcode = nf_inq_var(cdfid, id_data, varnam, ivtype, ndims, dimids, natts)

   if (debug) then
      write(unit=stdout, fmt='(3a,i6)') ' get_var_1d_real_cdf: dims for ',var,' ',ndims
   end if

   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), idims(i))
      if (debug) then
         write(unit=stdout, fmt='(a,2i6)') ' dimension ',i,idims(i)
         write(unit=stdout, fmt='(a,i6)') ' ivtype=', ivtype
         write(unit=stdout, fmt='(a, a)') ' varnam=', trim(varnam)
      end if
   end do

   ! check the dimensions

   if ((i1 /= idims(1)) .or.  &
       (time > idims(2))    )  then

      write(unit=stdout,fmt=*) ' error in 1d_var_real read, dimension problem '
      write(unit=stdout,fmt=*) i1, idims(1)
      write(unit=stdout,fmt=*) time, idims(2)
      write(unit=stdout,fmt=*) ' error stop '
      stop
   end if

   ! get the data

   istart(1) = 1
   iend(1) = i1
   istart(2) = time
   iend(2) = 1

   if ((ivtype == NF_real) .and. (kind(data) == 4)) then
      call ncvgt(cdfid,id_data,istart,iend,data,rcode)
   else if ((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
      call ncvgt(cdfid,id_data,istart,iend,data,rcode)
   else if ((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
      call ncvgt(cdfid,id_data,istart,iend,tmp,rcode)
      data = tmp
   else if ((ivtype == NF_REAL) .and. (kind(data) == 8)) then
      call ncvgt(cdfid,id_data,istart,iend,tmp4,rcode)
      data = tmp4
   else
      write(unit=stdout, fmt='(a, i6)') &
         'Unrecognizable ivtype:', ivtype
      stop
   end if

   if (debug) then
      write(unit=stdout, fmt='(a,e24.12)') ' Sample data=', data(1)
   end if

   call ncclos(cdfid,rcode)

   ! if (trace_use) call da_trace_exit("da_get_var_1d_real_cdf")

end subroutine da_get_var_1d_real_cdf
