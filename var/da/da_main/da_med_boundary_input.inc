subroutine da_med_boundary_input (grid, config_flags, filename)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)                :: grid
   type (grid_config_rec_type), intent(inout) :: config_flags

   character(*),  intent (in)                 :: filename

   integer                                    :: bid

   if (trace_use) call da_trace_entry("da_med_boundary_input")

   call open_r_dataset (bid, trim(filename), grid , config_flags , &
        "DATASET=BOUNDARY", ierr)

   if (ierr .NE. 0) then
      write(unit=message(1),fmt='(A,A,A,I5)') 'Error opening ', &
        trim(filename),' for reading ierr=',ierr
      call da_error("da_med_boundary_input.inc",24,message(1:1))
   end if

   call input_boundary (bid , grid , config_flags , ierr)

   call close_dataset (bid , config_flags , "DATASET=BOUNDARY")

   if (trace_use) call da_trace_exit("da_med_boundary_input")

end subroutine da_med_boundary_input


