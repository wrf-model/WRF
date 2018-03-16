subroutine da_read_y_unit(filename,unit_out,num,obs_type_in, nc)

   !-------------------------------------------------------------------------  
   ! Purpose: read diagnostics written on yp_unit or y_unit by WRF-Var
   !-------------------------------------------------------------------------   

   implicit none

   integer      ,intent (in)    :: unit_out
   integer      ,intent (inout) :: num
   character*(*),intent (in)    :: obs_type_in, filename                 
   integer      ,intent (in)    :: nc      

   integer      :: num_obs , unit_in, ios
   character*20 :: iv_type               
   logical      :: if_write
   
   real         :: fld(7), fld_rad                          
   integer      :: n, k, n1,n2, levels

   if (trace_use_dull) call da_trace_entry("da_read_y_unit")

   iv_type="Unknown"

   call da_get_unit(unit_in)
   open(unit=unit_in,file=trim(filename),form='formatted',iostat=ios,status='old')
   if (ios /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open random observation error file"//filename/))
   end if

   reports: do
   read(unit_in,'(a20,i8)', end = 999, err = 1000) iv_type,num_obs
   
   if_write = .false.
   if (index(iv_type,OBS_type_in(1:nc)) > 0) if_write = .true.

   ! If radiance data treat differently
   if ( (index(iv_type,'noaa') > 0) .or. (index(iv_type,'eos') > 0) .or.   &
        (index(iv_type,'dmsp') > 0) .or. (index(iv_type,'metop') > 0) .or. &
        (index(iv_type,'tiros') > 0) .or. (index(iv_type,'msg') > 0) .or. &
        (index(iv_type,'jpss') > 0)  .or. (index(iv_type,'gcom-w') >0) .or. &
        (index(iv_type,'goes') > 0) ) then

      do n = 1, num_obs
         if (if_write) num = num + 1
         read(unit_in,'(2i8,e15.7)')n1, n2, fld_rad
         if (if_write)write(unit_out,'(2i8,e15.7)')num,n2, fld_rad
      end do
   else
      do n = 1, num_obs
         if (if_write) num = num + 1
         if (index(iv_type,'bogus') > 0) then
            read(unit_in,'(i8)', err=1000)levels
            if (if_write) write(unit_out,'(i8)')levels
            read(unit_in,'(2i8,7e15.7)', err= 1000) n1, n2, fld
            if (if_write) write(unit_out,'(2i8,7e15.7)') num, levels, fld  
         end if
         read(unit_in,'(i8)', err=1000)levels
         if (if_write) write(unit_out,'(i8)')levels
         do k = 1, levels
            read(unit_in,'(2i8,7e15.7)', err= 1000) n1, n2, fld  
            if (if_write) write(unit_out,'(2i8,7e15.7)') num, k, fld
         end do
      end do
   end if
   if (if_write) exit reports
   cycle reports
1000  continue 
   write(unit=message(1), fmt='(a,i3,a,a)') &
      'read error on unit: ',unit_in, ' for iv_type', trim(iv_type)
   ! call da_warning(__FILE__,__LINE__,message(1:1))
   end do reports
 999  continue 
   close (unit_in)
   call da_free_unit(unit_in)

   if (trace_use_dull) call da_trace_exit("da_read_y_unit")

end subroutine da_read_y_unit


