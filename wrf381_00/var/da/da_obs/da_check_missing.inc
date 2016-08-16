subroutine da_check_missing(qc_flag, y_in, y_out)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none
   
   integer, intent(in)   :: qc_flag
   real, intent(in)      :: y_in
   real, intent(out)     :: y_out

   if (qc_flag < obs_qc_pointer) then
      y_out = missing_r
   else
      y_out = y_in
   end if

end subroutine da_check_missing


