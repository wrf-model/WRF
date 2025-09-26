subroutine da_get_times_cdf(file, times, n_times, max_times, debug)
 
   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
       
   implicit none

#include "netcdf.inc"

   integer,            intent(in)  ::  max_times  
   integer,            intent(out) ::  n_times
   character (len=*),  intent(in)  :: file
   character (len=80), intent(out) :: times(max_times)
   logical,            intent(in)  :: debug

   integer            :: cdfid, rcode, id_time
   character (len=80) :: varnam, time1
   integer            :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
   integer            :: i, ivtype

   ! if (trace_use) call da_trace_entry("da_get_times_cdf")

   cdfid = ncopn(file, NCNOWRIT, rcode)

   if (rcode == 0) then
      if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
      write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
      stop
   end if

   id_time = ncvid(cdfid, 'Times', rcode)

   rcode = nf_inq_var(cdfid, id_time, varnam, ivtype, ndims, dimids, natts)
   if (debug) then
      write(unit=stdout,fmt=*) ' number of dims for Time ',ndims
   end if
   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), idims(i))
      if (debug) write(unit=stdout,fmt=*) ' dimension ',i,idims(i)
   end do

   !  get the times

   n_times = idims(2)
   do i=1,idims(2)
      istart(1) = 1
      iend(1) = idims(1)
      istart(2) = i
      iend(2) = 1

      rcode = NF_GET_VARA_TEXT  (cdfid, id_time,  &
                                 istart, iend,    &
                                 times(i)         )
      time1 = times(i)

      if (debug) write(unit=stdout,fmt=*) trim(file), time1(1:19)
      times(i) = time1(1:19)
   end do

   write(unit=stdout,fmt=*) ' exiting get_times_cdf '

   call ncclos(cdfid,rcode)

   ! if (trace_use) call da_trace_exit("da_get_times_cdf")

end subroutine da_get_times_cdf


