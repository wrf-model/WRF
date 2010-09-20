subroutine da_med_initialdata_output_lbc (grid , config_flags, out_filename)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain), intent(in)                   :: grid
   type (grid_config_rec_type) , intent(inout) :: config_flags       
   character(*),  intent (in),  optional       :: out_filename

   integer                :: fid, bid
   character (len=80)     :: file_name

   integer :: julyr, julday
   real    :: gmt

   if (trace_use) call da_trace_entry("da_med_initialdata_output_lbc")

   if (present(out_filename)) then
      file_name = trim(out_filename)
   else
      file_name = 'wrfvar_output'
   end if

   call da_trace ("da_med_initialdata_ouput_lbc",Message="Writing wrfvar output")
   
   if (present(out_filename) .and. trim(out_filename) .eq. 'tl01') then
      file_name = 'wrfbdy_tl01'

      call open_w_dataset (bid, trim(file_name), grid , config_flags , &
                           output_boundary , "DATASET=BOUNDARY", ierr )

      if (ierr /= 0) then
         write(unit=message(1),fmt=*) 'Error opening ', &
            trim(file_name),' for writing ierr=',ierr
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
   endif

   if (present(out_filename) .and. trim(out_filename) .eq. 'af07') then
      file_name = 'wrfbdy_af07'

      call open_w_dataset (bid, trim(file_name), grid , config_flags , &
                           output_boundary , "DATASET=BOUNDARY", ierr )

      if (ierr /= 0) then
         write(unit=message(1),fmt=*) 'Error opening ', &
            trim(file_name),' for writing ierr=',ierr
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
   endif

   start_date=current_date

   call geth_julgmt(julyr, julday, gmt)
   config_flags%gmt = gmt
   config_flags%julyr = julyr
   config_flags%julday = julday
   CALL nl_set_bdyfrq ( grid%id , REAL(model_config_rec%interval_seconds) )
   call model_to_grid_config_rec (grid%id , model_config_rec , config_flags)

   if (present(out_filename) .and. (trim(out_filename) .eq. 'tl01') .or. &
       (trim(out_filename) .eq. 'af07') ) then
      call output_boundary (bid, grid , config_flags , ierr)

      call close_dataset (bid , config_flags, "DATASET=BOUNDARY")
   end if

   if (trace_use) call da_trace_exit("da_med_initialdata_output_lbc")

end subroutine da_med_initialdata_output_lbc


