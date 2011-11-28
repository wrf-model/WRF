subroutine da_get_var_2d_int_cdf(file, var, data, i1, i2, time, debug)
 
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
        
   implicit none

#include "netcdf.inc"

   integer,            intent(in)  ::  i1, i2, time
   character (len=*),  intent(in)  :: file
   logical,            intent(in)  :: debug
   character (len=*),  intent(in)  :: var
   integer,            intent(out) :: data(i1,i2)

   integer            :: cdfid, rcode, id_data
   character (len=80) :: varnam
   integer            :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
   integer            :: i, ivtype

   ! if (trace_use) call da_trace_entry("da_get_var_2d_int_cdf")

   cdfid = ncopn(file, NCNOWRIT, rcode)

   if (rcode /= 0) then
      write(unit=stdout, fmt='(2a)') ' error opening netcdf file ', trim(file)
      stop
   end if

   id_data = ncvid(cdfid, var, rcode)

   rcode = nf_inq_var(cdfid, id_data, varnam, ivtype, ndims, dimids, natts)

   if (debug) then
      write(unit=stdout, fmt='(3a,i6)') ' get_var_2d_real_cdf: dims for ',var,' ',ndims
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
       (i2 /= idims(2)) .or.  &
       (time > idims(3))    )  then

      write(unit=stdout,fmt=*) ' error in 2d_var_real read, dimension problem '
      write(unit=stdout,fmt=*) i1, idims(1)
      write(unit=stdout,fmt=*) i2, idims(2)
      write(unit=stdout,fmt=*) time, idims(4)
      write(unit=stdout,fmt=*) ' error stop '
      stop

   end if

   ! get the data
  
   istart(1) = 1
   iend(1) = i1
   istart(2) = 1
   iend(2) = i2
   istart(3) = time
   iend(3) = 1

   call ncvgt(cdfid,id_data,istart,iend,data,rcode)

   if (debug) then
      write(unit=stdout, fmt='(a, i8)') ' Sample data=', data(1,1)
   end if

   call ncclos(cdfid,rcode)

   ! if (trace_use) call da_trace_exit("da_get_var_2d_int_cdf")

end subroutine da_get_var_2d_int_cdf


