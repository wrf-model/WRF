subroutine da_scan_obs_lsac (iv)

   !---------------------------------------------------------------------------
   ! Purpose: Scan the radar observation file
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),    intent(inout) :: iv

   call da_read_lsac_wrfinput(iv, .true.)

   if (trace_use) call da_trace_exit("da_scan_obs_lsac")

end subroutine da_scan_obs_lsac
