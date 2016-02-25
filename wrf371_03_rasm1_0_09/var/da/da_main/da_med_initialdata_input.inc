subroutine da_med_initialdata_input (grid, config_flags, filename, in_date)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)                :: grid
   type (grid_config_rec_type), intent(inout) :: config_flags

   character(*),  intent (in)                 :: filename
   character(*),  intent (in),  optional      :: in_date

   integer                 :: fid , status, n, nsave

   integer :: julyr, julday
   real    :: gmt

   if (trace_use) call da_trace_entry("da_med_initialdata_input")
   if (trace_use) call da_trace("da_med_initialdata_input", &
      Message="Reading "//trim(filename))

   ! Initialize the mother domain.

   grid%input_from_file = .true.
   ! Initialize the array grid%pb (YRG, 08/26/2010) for use of wrftoxb:
   grid%pb = 0.0
   
   call ext_ncd_open_for_read(trim(filename), 0, 0, "", fid, ierr)

   if (ierr /= 0) then
      write(unit=message(1), fmt='(2a)') &
         'Netcdf error opening file:', trim(filename)
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   call ext_ncd_get_next_time(fid, current_date, Status)

   if (present(in_date)) then
      ! Set start_date to current_date.
      read(in_date(1:19), fmt='(i4, 5(1x, i2))') &
         grid%start_year,   &
         grid%start_month,  &
         grid%start_day,    &
         grid%start_hour,   &
         grid%start_minute, &
         grid%start_second 

      nsave = -1
      do n=1, 1000
         if (current_date(1:19) == in_date(1:19)) then
            nsave = n - 1
            exit
         end if
         call ext_ncd_get_next_time(fid, current_date, Status)
      end do

      if (nsave < 0) then
         call da_error(__FILE__,__LINE__,(/"Cannot find the needed time"/))
      end if
   else
      ! Set start_date to current_date.
      read(current_date(1:19), fmt='(i4, 5(1x, i2))') &
           grid%start_year,  &
           grid%start_month, &
           grid%start_day,   &
           grid%start_hour,  &
           grid%start_minute,&
           grid%start_second
   end if

   call geth_julgmt(julyr, julday, gmt)
   call nl_set_gmt (grid%id, gmt)
   call nl_set_julyr (grid%id, julyr)
   call nl_set_julday (grid%id, julday)

   call nl_set_iswater (grid%id, grid%iswater)
   call nl_set_cen_lat (grid%id , grid%cen_lat)
   call nl_set_cen_lon (grid%id , grid%cen_lon)
   call nl_set_truelat1 (grid%id , grid%truelat1)
   call nl_set_truelat2 (grid%id , grid%truelat2)
   call nl_set_moad_cen_lat (grid%id , grid%moad_cen_lat)
   call nl_set_stand_lon (grid%id , grid%stand_lon)
   call nl_set_pole_lat (grid%id , grid%pole_lat)
   call nl_set_map_proj (grid%id , grid%map_proj)
   start_date=current_date

   call geth_julgmt(julyr, julday, gmt)
   config_flags%gmt = gmt
   config_flags%julyr = julyr
   config_flags%julday = julday

   call ext_ncd_ioclose(fid, ierr)

   call da_trace("da_med_initialdata_input", &
       message="open_r_dataset for "//trim(filename))
   call open_r_dataset (fid, trim(filename), grid , config_flags , &
      "DATASET=INPUT", ierr)

   if (ierr .NE. 0) then
      write(unit=message(1),fmt='(A,A,A,I5)') 'Error opening ', &
        trim(filename),' for reading ierr=',ierr
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   if (present(in_date)) then
      do n=1, nsave
         call da_message((/"current_date="//current_date// &
            ', in_date='//in_date/))
         call ext_ncd_get_next_time(fid, current_date, Status)
      end do
   end if

   call input_input (fid ,   grid , config_flags , ierr)

   call nl_get_mminlu (grid%id , grid%mminlu)

   call close_dataset (fid , config_flags , "DATASET=INPUT")

   if (trace_use) call da_trace_exit("da_med_initialdata_input")

end subroutine da_med_initialdata_input


