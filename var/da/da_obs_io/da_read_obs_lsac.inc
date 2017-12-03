subroutine da_read_obs_lsac (iv)

   !-----------------------------------------------------------------------
   ! Purpose: Read the lsac "observation" file
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type),    intent(inout) :: iv


   if (trace_use) call da_trace_entry("da_read_obs_lsac")

   call da_read_lsac_wrfinput(iv, .false.)

   if (trace_use) call da_trace_exit("da_read_obs_lsac")

end subroutine da_read_obs_lsac
 
