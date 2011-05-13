subroutine da_med_initialdata_output_lbc (grid , config_flags, out_filename)

   !-----------------------------------------------------------------------
   ! Purpose: Write out LBC condition at the t=0 for 4DVAR with LBC control
   !          We only use the perturbation of LBC
   ! Xin Zhang: 10/8/2010
   !-----------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)                 :: grid
   type (grid_config_rec_type) , intent(inout) :: config_flags       
   character(*),  intent (in),  optional       :: out_filename

#ifdef VAR4D

   integer                :: fid, bid
   character (len=80)     :: file_name

   integer :: julyr, julday
   real    :: gmt

   if (trace_use) call da_trace_entry("da_med_initialdata_output_lbc")

   if (present(out_filename)) then
      file_name = trim(out_filename)
   else
      file_name = 'wrfvar_bdyout'
   end if

   call open_w_dataset (bid, trim(file_name), grid , config_flags , &
                        output_boundary , "DATASET=BOUNDARY", ierr )

   if (ierr /= 0) then
      write(unit=message(1),fmt=*) 'Error opening ', &
            trim(file_name),' for writing ierr=',ierr
      call da_error(__FILE__,__LINE__,message(1:1))
   endif

   start_date=current_date

   call geth_julgmt(julyr, julday, gmt)
   config_flags%gmt = gmt
   config_flags%julyr = julyr
   config_flags%julday = julday
   call nl_set_bdyfrq ( grid%id , REAL(model_config_rec%interval_seconds) )
   call model_to_grid_config_rec (grid%id , model_config_rec , config_flags)

   call output_boundary (bid, grid , config_flags , ierr)

   call close_dataset (bid , config_flags, "DATASET=BOUNDARY")

   if (trace_use) call da_trace_exit("da_med_initialdata_output_lbc")

#endif
end subroutine da_med_initialdata_output_lbc


