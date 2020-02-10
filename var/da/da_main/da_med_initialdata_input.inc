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
   integer :: icnt

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

   ! Check if this is a restart file; quit if it is.
   ! Restart files can be successfully read and used but will result in garbage data, so we should quit now.
   call wrf_get_dom_ti_integer (fid, 'FLAG_RESTART', status, 1, icnt, ierr)
   if (ierr == 0) then
      if (status == 1) then
         write(unit=message(1),fmt='(A,A)') trim(filename), ' appears to be a WRF restart file'
         write(unit=message(2),fmt='(A)')'WRFDA can only work with wrfinput or wrfout format files'
         call da_error(__FILE__,__LINE__,message(1:2))
      end if
   end if


   ! mp_physics is used in da_transfer_wrftoxb.inc
   ! get the setting from the input file
   call wrf_get_dom_ti_integer (fid, 'MP_PHYSICS', grid%mp_physics, 1, icnt, ierr)

   ! sf_surface_physics is used in da_transfer_xatowrf.inc
   ! get the setting from the input file
   call wrf_get_dom_ti_integer (fid, 'SF_SURFACE_PHYSICS', grid%sf_surface_physics, 1, icnt, ierr)

   !global attribute HYBRID_OPT exists in WRF V3.9 and later
   call wrf_get_dom_ti_integer (fid, 'HYBRID_OPT', grid%hybrid_opt, 1, icnt, ierr)
   if ( ierr /= 0 ) then
      grid%hybrid_opt = 0
      write(unit=message(1),fmt='(A,A)') trim(filename), ' appears to be from earlier than WRF V3.9'
      write(unit=message(2),fmt='(A)')   'Setting hybrid_opt = 0'
      call da_message(message(1:2))
   else
      ! when input file contains HYBRID_OPT, stop 4DVAR if not terrain-following.
      if ( var4d ) then
         if ( grid%hybrid_opt > 0 ) then
            write(unit=message(1),fmt='(A,I2,A,A)') 'HYBRID_OPT = ', grid%hybrid_opt, ' in ', trim(filename)
            write(unit=message(2),fmt='(A)') '4DVAR does not work with hybrid vertical coordinate'
            call da_error(__FILE__,__LINE__,message(1:2))
         end if
      end if
   end if

   write(unit=message(1),fmt='(A,I2)') 'hybrid_opt = ', grid%hybrid_opt
   call da_message(message(1:1))

   !global attribute USE_THETA_M exists in WRF V3.7? and later
   call wrf_get_dom_ti_integer (fid, 'USE_THETA_M', grid%use_theta_m, 1, icnt, ierr)
   if ( ierr /= 0 ) then
      grid%use_theta_m = 0
      write(unit=message(1),fmt='(A,A)') trim(filename), ' appears to be from earlier than WRF V3.7'
      write(unit=message(2),fmt='(A)')   'Setting use_theta_m = 0'
      call da_message(message(1:2))
   else
      ! when input file contains USE_THETA_M, stop 4DVAR (wrfplus not work with thetam yet).
      if ( var4d ) then
         if ( grid%use_theta_m > 0 ) then
            write(unit=message(1),fmt='(A,I2,A,A)') 'USE_THETA_M = ', grid%use_theta_m, ' in ', trim(filename)
            write(unit=message(2),fmt='(A)') '4DVAR does not work with moist potential T'
            call da_error(__FILE__,__LINE__,message(1:2))
         end if
      end if
   end if

   write(unit=message(1),fmt='(A,I2)') 'use_theta_m = ', grid%use_theta_m
   call da_message(message(1:1))


   call close_dataset (fid , config_flags , "DATASET=INPUT")

   if (trace_use) call da_trace_exit("da_med_initialdata_input")

end subroutine da_med_initialdata_input


