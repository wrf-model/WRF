subroutine da_put_var_2d_real_cdf(file, var, data, i1, i2, time, debug)
 
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
        
   implicit none

#include "netcdf.inc"

   integer,            intent(in)  ::  i1, i2, time
   character (len=*),  intent(in) :: file
   logical,            intent(in) :: debug
   character (len=*),  intent(in) :: var
   real,               intent(in) :: data(i1,i2)

   real(kind=8) :: tmp(i1,i2)
   real(kind=4) :: tmp4(i1,i2)

   integer :: cdfid, rcode, id_data
   character (len=80) :: varnam
   integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
   integer :: i, ivtype

   ! not for update_bc
   !if (trace_use) call da_trace_entry("da_put_var_2d_real_cdf")

   cdfid = ncopn(file, NCWRITE, rcode)

   if (rcode == 0) then
      if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
      write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
      stop
   end if

   id_data = ncvid(cdfid, var, rcode)

   rcode = nf_inq_var(cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
   if (debug) then
      write(unit=stdout,fmt=*) ' number of dims for ',var,' ',ndims
   end if
   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), idims(i))
      if (debug) write(unit=stdout,fmt=*) ' dimension ',i,idims(i)
   end do

   ! check the dimensions
 
   if ((i1 /= idims(1)) .or.  &
      (i2 /= idims(2)) .or.  &
      (time > idims(3))    )  then

      write(unit=stdout,fmt=*) ' error in 3d_var_real read, dimension problem '
      write(unit=stdout,fmt=*) i1, idims(1)
      write(unit=stdout,fmt=*) i2, idims(2)
      write(unit=stdout,fmt=*) time, idims(3)
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

   if ((ivtype == NF_real) .and. (kind(data) == 4)) then
      call ncvpt(cdfid,id_data,istart,iend,data,rcode)
   else if ((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
      tmp = data
      call ncvpt(cdfid,id_data,istart,iend,tmp,rcode)
   else if ((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
      tmp = data
      call ncvpt(cdfid,id_data,istart,iend,tmp,rcode)
   else if ((ivtype == NF_REAL) .and. (kind(data) == 8)) then
      tmp4 = data
      call ncvpt(cdfid,id_data,istart,iend,tmp4,rcode)
   else
      write(unit=stdout, fmt='(a, 4i6)') &
         'Unrecognizable ivtype:', ivtype,nf_double,nf_real,kind(data)
      stop
   end if

   call ncclos(cdfid,rcode)

   !if (trace_use) call da_trace_exit("da_put_var_2d_real_cdf")

end subroutine da_put_var_2d_real_cdf


