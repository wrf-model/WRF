subroutine da_max_error_qc (it, info, n, field, max_error,failed)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)     :: it
   type(infa_type), intent(in)      :: info
   integer,           intent(in)    :: n
   type(field_type),  intent(inout) :: field
   real,              intent(in)    :: max_error
   logical,           intent(out)   :: failed

   real                               :: err, err_max
   integer                            :: qc_flag

   if (trace_use_frequent) call da_trace_entry("da_max_error_qc")

   failed = .false.

   qc_flag = field % qc
   err_max = field % error * max_error
   err     = field % inv
   err     = ABS (err)

   if (err > err_max) then
      field % qc = fails_error_max 
      failed = .true.
      field % inv = 0.0
   end if

   if (trace_use_frequent) call da_trace_exit("da_max_error_qc")

end subroutine da_max_error_qc


