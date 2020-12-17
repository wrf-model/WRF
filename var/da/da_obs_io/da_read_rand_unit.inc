subroutine da_read_rand_unit(filename, unit_in,num,obs_type_in, nc)

   !----------------------------------------------------------------------------   
   ! Purpose: Program to read diagnostics written on rand_unit by WRF-Var
   !----------------------------------------------------------------------------   

   implicit none

   integer      ,intent (in)    :: unit_in
   integer      ,intent (inout) :: num
   character*(*),intent (in)    :: obs_type_in 
   character*(*),intent (inout) :: filename                    
   integer      ,intent (in)    :: nc      

   integer      :: num_obs 
   character*20 :: iv_type               
   logical   :: if_write
   
   real         :: fld(10), fld1_rad , fld2_rad                         
   integer      :: n, k, n1,n2, levels, ios

   if (trace_use) call da_trace_entry("da_read_rand_unit")

   open(unit=unit_in,   file=trim(filename), status='old',iostat=ios)
   if (ios /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open file"//filename/))
   end if
1  continue
  

   read(unit_in,'(a20,i8)', end = 999, err = 1000)iv_type,num_obs                    
   
   if_write = .false.
   if (index(iv_type,OBS_type_in(1:nc)) > 0) if_write = .true.

   ! If radiance data treat differently
   if ( (index(iv_type,'noaa') > 0)  .or. (index(iv_type,'eos') > 0) .or.  &
        (index(iv_type,'dmsp') > 0) .or. (index(iv_type,'metop') > 0) ) then
      do n = 1, num_obs
         if (if_write) num = num + 1
         read(unit_in,'(2i8,f10.3,e15.7)')n1, n2, fld1_rad,fld2_rad
         if (if_write)write(rand_unit,'(2i8,f10.3,e15.7)')num,n2, fld1_rad,fld2_rad
      end do
   else
      do n = 1, num_obs        
         if (if_write) num = num + 1
         if (index(iv_type,'bogus')     > 0)  then
            read(unit_in,'(i8)', err=1000)levels
            if (if_write) write(rand_unit,'(i8)')levels
            read(unit_in,'(2i8,10e15.7)', err= 1000) n1, n2, fld
            if (if_write) write(rand_unit,'(2i8,10e15.7)') num, levels, fld  
         end if
         read(unit_in,'(i8)', err=1000)levels
         if (if_write) write(rand_unit,'(i8)')levels
         do k = 1, levels
            read(unit_in,'(2i8,10e15.7)', err= 1000) n1, n2, fld  
            if (if_write) write(rand_unit,'(2i8,10e15.7)') num, k, fld
         end do
      end do
   end if

   if (if_write)  goto 999
   goto 1

999 continue
   close (unit_in)
   if (trace_use) call da_trace_exit("da_read_rand_unit")
   return

1000  continue 
   write(unit=message(1), fmt='(/a,i3/a/)') &
              'read error on unit: ',unit_in, &
              'in da_read_rand_unit'
   call da_warning(__FILE__,__LINE__,message(1:1))

   if (trace_use) call da_trace_exit("da_read_rand_unit")

end subroutine da_read_rand_unit


