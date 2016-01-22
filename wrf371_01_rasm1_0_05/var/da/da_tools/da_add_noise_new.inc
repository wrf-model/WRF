subroutine da_add_noise_new (qc, error, inv, yo, z)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)              :: qc
   real, intent(in)                 :: error
   real, intent(inout)              :: inv
   real, intent(inout)              :: yo      ! Observation.
   real, intent(out)                :: z       ! Random number.

   real                             :: noise

   if (trace_use) call da_trace_entry("da_add_noise_new")

   z = missing_r

   if (qc >= obs_qc_pointer) then
      ! [1] Calculate scaled Gaussian noise:

      call da_gauss_noise (z)      
      noise = error * z
      
      ! [3] Recalculate corresponding O and O-B:
      yo = yo + noise
      inv = inv + noise
   end if

   if (trace_use) call da_trace_exit("da_add_noise_new")

end subroutine da_add_noise_new


