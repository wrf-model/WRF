subroutine da_med_initialdata_output (grid , config_flags, out_filename)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain), intent(in)                   :: grid
   type (grid_config_rec_type) , intent(inout) :: config_flags       
   character(*),  intent (in),  optional       :: out_filename

   integer                :: fid
   character (len=80)     :: file_name

   integer :: julyr, julday
   real    :: gmt

   if (trace_use) call da_trace_entry("da_med_initialdata_output")

   if (present(out_filename)) then
      file_name = trim(out_filename)
   else
      file_name = 'wrfvar_output'
   end if

   call da_trace ("da_med_initialdata_ouput",Message="Writing wrfvar output")
   call open_w_dataset (fid, trim(file_name), grid , config_flags , &
                         output_input , "DATASET=INPUT,REAL_OUTPUT_SIZE=4", ierr)

   if (ierr /= 0) then
      write(unit=message(1),fmt=*) 'Error opening ', &
         trim(file_name),' for writing ierr=',ierr
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   start_date=current_date

   call geth_julgmt(julyr, julday, gmt)
   config_flags%gmt = gmt
   config_flags%julyr = julyr
   config_flags%julday = julday

   call output_input (fid, grid , config_flags , ierr)

   call close_dataset (fid , config_flags, "DATASET=INPUT,REAL_OUTPUT_SIZE=4")

   if (trace_use) call da_trace_exit("da_med_initialdata_output")

end subroutine da_med_initialdata_output


