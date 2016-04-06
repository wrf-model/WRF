real function da_diff_seconds (date_char_1, date_char_2)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   character(len=24), intent(in) :: date_char_1, date_char_2
  
   integer :: ccyy_1,mo_1,dd_1,hh_1,mi_1,ss_1,jd_1
   integer :: ccyy_2,mo_2,dd_2,hh_2,mi_2,ss_2,jd_2
   integer :: i, year, diff_days
   integer :: start_year, end_year
   integer :: mmday(12)
 
   real    :: s_1, s_2

   if (trace_use_dull) call da_trace_entry("da_diff_seconds")
  
   mmday=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  
   read(date_char_1(1:19), fmt='(i4,1x,4(i2,1x),i2)') &
        ccyy_1, &
          mo_1, &
          dd_1, &
          hh_1, &
          mi_1, &
          ss_1
  
   read(date_char_2(1:19), fmt='(i4,1x,4(i2,1x),i2)') &
        ccyy_2, &
          mo_2, &
          dd_2, &
          hh_2, &
          mi_2, &
          ss_2

   if (ccyy_2 >= ccyy_1) then
      start_year = ccyy_1
      end_year   = ccyy_2
   else
      start_year = ccyy_2
      end_year   = ccyy_1
   end if

   diff_days = 0
  
   do year=start_year,end_year-1
      diff_days = diff_days + 365
      if (mod(year,4) == 0) then
         diff_days = diff_days + 1

         if ((mod(year,100) == 0) .and. (mod(year,400) /= 0)) then
            diff_days = diff_days - 1
         end if
      end if
   end do

   if (mod(ccyy_1,4) == 0) then
      mmday(2) = 29

      if((mod(ccyy_1,100) == 0) .and. (mod(ccyy_1,400) /= 0)) then
         mmday(2) = 28
      end if
   end if

   jd_1 = dd_1

   do i=1,mo_1-1
      jd_1=jd_1+mmday(i)
   end do

   s_1 = real(ss_1) &
       + 60.0*(real(mi_1) &
       + 60.0*(real(hh_1) &
       + 24.0* real(jd_1)))

   if (mod(ccyy_2,4) == 0) then
      mmday(2) = 29

      if((mod(ccyy_2,100) == 0) .and. (mod(ccyy_2,400) /= 0)) then
         mmday(2) = 28
      end if
   end if

   if (ccyy_2 >= ccyy_1) then
      jd_2 = dd_2 + diff_days
   else
      jd_2 = dd_2 - diff_days
   end if

   do i=1,mo_2-1
      jd_2=jd_2+mmday(i)
   end do

   s_2 = real(ss_2) &
       + 60.0*(real(mi_2) &
       + 60.0*(real(hh_2) &
       + 24.0* real(jd_2)))

   da_diff_seconds = abs(s_1 - s_2)

   if (trace_use_dull) call da_trace_exit("da_diff_seconds")

end function da_diff_seconds


