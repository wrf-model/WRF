subroutine da_get_bdytimestr_cdf(file, time_flag, bdytimestr, max_times, debug)

   implicit none

#include "netcdf.inc"

   integer,           intent(in) :: max_times
   logical,           intent(in) :: debug
   character(len=*),  intent(in) :: file
   character(len=*),  intent(in) :: time_flag
   character(len=80), intent(out) :: bdytimestr(max_times)

   character(len=80) :: varnam, time1

   integer :: cdfid, rcode, id_time
   integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
   integer :: i, ivtype

   ! if (trace_use) call da_trace_entry("da_get_bdytimestr_cdf")

   cdfid = ncopn(file, NCNOWRIT, rcode)

   if (rcode == 0) then
      if (debug) write(unit=stdout,fmt=*) ' open netcdf file ', trim(file)
   else
      write(unit=stdout,fmt=*) ' error openiing netcdf file ', trim(file)
      stop
   end if

   varnam = 'md___' // trim(time_flag) // 'e_x_t_d_o_m_a_i_n_m_e_t_a_data_'

   id_time = ncvid(cdfid, varnam, rcode)

   rcode = nf_inq_var(cdfid, id_time, varnam, ivtype, ndims, dimids, natts)

   if (debug) then
     write(unit=stdout,fmt=*) ' number of dims for bdytime ',ndims
   end if

   do i=1,ndims
      rcode = nf_inq_dimlen(cdfid, dimids(i), idims(i))
      if (debug) write(unit=stdout,fmt=*) ' dimension ',i,idims(i)
   end do

   ! get the bdytime

   do i=1,idims(2)
      istart(1) = 1
      iend(1) = idims(1)
      istart(2) = i
      iend(2) = 1

      rcode = NF_GET_VARA_TEXT  (cdfid, id_time,  &
                                 istart, iend,    &
                                 bdytimestr(i))
      time1 = bdytimestr(i)

      if (debug) write(unit=stdout,fmt=*) trim(file), time1(1:19)
      bdytimestr(i) = time1(1:19)
   end do

   call ncclos(cdfid,rcode)

   ! if (trace_use) call da_trace_exit("da_get_bdytimestr_cdf")

end subroutine da_get_bdytimestr_cdf


