subroutine da_read_errfac(ob_name, f1, f2, f3, f4, f5)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none
   
   character (len=5), intent(in)  :: ob_name
   real,              intent(out) :: f1
   real,              intent(out) :: f2
   real,              intent(out) :: f3
   real,              intent(out) :: f4
   real,              intent(out) :: f5

   character (len=5)  :: ob_name1
   character (len=21) :: string1
   character (len=91) :: string2
   integer            :: fac_unit
   real               :: d1, d2, d3, d4, d5

   f1 = 1.0
   f2 = 1.0
   f3 = 1.0
   f4 = 1.0
   f5 = 1.0

   if (trace_use_dull) call da_trace_entry("da_read_errfac")

   call da_get_unit(fac_unit)
   open(unit=fac_unit, status='old', file = 'errfac.dat', iostat=ierr)

   if (ierr == 0) then
      do 
         read(unit=fac_unit,fmt='(1x,a5,a21,a91)')ob_name1, string1, string2

         if (ob_name == ob_name1 .and. string1 == ' obs, Error Factor = ') then
            read(unit=string2(17:31),fmt=*)d1
            read(unit=string2(32:46),fmt=*)d2
            read(unit=string2(47:61),fmt=*)d3
            read(unit=string2(62:76),fmt=*)d4
            read(unit=string2(77:91),fmt=*)d5
            if (d1 > 0.0) f1 = d1
            if (d2 > 0.0) f2 = d2
            if (d3 > 0.0) f3 = d3
            if (d4 > 0.0) f4 = d4
            if (d5 > 0.0) f5 = d5

            exit
         else if (ob_name1 == 'Total') then
            write(unit=message(1),fmt='(a,a)') ' No Tuning Error factors for ', ob_name
            write(unit=message(2),fmt='(a)') ' So setting to 1.0 i.e. default errors.'  
            call da_warning(__FILE__,__LINE__,message(1:2))
            exit
         end if
      end do     
   else   
      call da_warning(__FILE__,__LINE__, (/"Problem reading errfac.dat - Not tuning ob errors"/))
   end if

   close(fac_unit)
   call da_free_unit(fac_unit)

   if (trace_use_dull) call da_trace_exit("da_read_errfac")

end subroutine da_read_errfac


