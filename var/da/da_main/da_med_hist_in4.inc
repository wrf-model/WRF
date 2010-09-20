subroutine da_med_hist_in4 (grid , config_flags, out_filename)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain), intent(in)                    :: grid
   type (grid_config_rec_type) , intent(inout) :: config_flags       
   character(*),  intent (in)                  :: out_filename

   integer                :: fid
   character (len=80)     :: file_name

   if (trace_use) call da_trace_entry("da_med_hist_in4")

   file_name = trim(out_filename)

   call open_r_dataset (fid, trim(file_name), grid , config_flags , &
                        "DATASET=AUXHIST4", ierr)

   if (ierr /= 0) then
      write(unit=message(1),fmt=*) 'Error opening ', &
         trim(file_name),' for writing ierr=',ierr
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   call input_auxhist4 (fid, grid , config_flags , ierr)

   call close_dataset (fid , config_flags, "DATASET=AUXHIST4")

   if (trace_use) call da_trace_exit("da_med_hist_in4")

end subroutine da_med_hist_in4



