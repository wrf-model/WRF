subroutine da_pack_count_obs (num_obs, offset, value)

   !---------------------------------------------------------------------------
   ! Purpose: Pack the 4 integer num_obs values into value(offset) to 
   !          value(offset+3).
   !---------------------------------------------------------------------------

   implicit none

   type(count_obs_number_type), intent(in)     :: num_obs
   integer,                     intent(inout)  :: offset
   integer,                     intent(inout)  :: value(*)

   if (trace_use_dull) call da_trace_entry("da_pack_count_obs")

   value(offset)   = num_obs % num_used
   value(offset+1) = num_obs % num_outside_iyjx
   value(offset+2) = num_obs % num_max_err_chk
   value(offset+3) = num_obs % num_missing

   offset = offset + 4

   if (trace_use_dull) call da_trace_exit("da_pack_count_obs")

end subroutine da_pack_count_obs


