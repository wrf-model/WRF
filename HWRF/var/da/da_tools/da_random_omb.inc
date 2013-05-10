subroutine da_random_omb(std_dev, yo, qc_flag, omb)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none
   
   real, intent(in)     :: std_dev      ! Standard deviation scaling.
   real, intent(inout)  :: yo           ! Observatopn.
   integer, intent(in)  :: qc_flag      ! Ob QC flag.
   real, intent(inout)  :: omb          ! O-B.

   real :: xb           ! Background value.
   real :: z            ! Gaussian noise.

   if (trace_use) call da_trace_entry("da_random_omb")

   if (qc_flag >= obs_qc_pointer) then
      ! [1] Calculate background value from old O, O-B:
      xb = yo - omb
   
      ! [2] Calculate new O-B as scaled Gaussian noise:

      call da_gauss_noise(z)   
      omb = set_omb_rand_fac * std_dev * z

      ! [3] Recalculate corresponding observation:
      yo = xb + omb
   end if

   if (trace_use) call da_trace_exit("da_random_omb")

end subroutine da_random_omb


