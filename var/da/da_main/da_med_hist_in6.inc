subroutine da_med_hist_in6 (grid, config_flags, filename)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)                :: grid
   type (grid_config_rec_type), intent(inout) :: config_flags

   character(*),  intent (in)                 :: filename

   integer                 :: fid 

   if (trace_use) call da_trace_entry("da_med_hist_in6")

   call open_r_dataset (fid, trim(filename), grid , config_flags , &
         "DATASET=AUXHIST6", ierr)

   if (ierr .NE. 0) then
      write(unit=message(1),fmt='(A,A,A,I5)') 'Error opening ', &
         trim(filename),' for reading ierr=',ierr
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   call input_auxhist6 (fid , grid , config_flags , ierr)

   call close_dataset (fid , config_flags , "DATASET=AUXINPUT6")

   if (trace_use) call da_trace_exit("da_med_hist_in6")

end subroutine da_med_hist_in6


