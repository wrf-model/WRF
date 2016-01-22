subroutine da_ao_stats_airsr (stats_unit, iv, re)

   !-------------------------------------------------------------------------
   ! Purpose: Compute analysis increment at AIRS retrieval locations
   !-------------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: stats_unit    ! Output unit for stats.
   type (iv_type), intent(inout) :: iv            ! iv
   type (y_type),  intent(in)    :: re            ! A - O

   type (stats_airsr_type) :: stats
   integer                 :: nt, nq
   integer                 :: n, k

   if (trace_use_dull) call da_trace_entry("da_ao_stats_airsr")

   nt = 0
   nq = 0

   stats%maximum%t = maxmin_type (missing_r, 0, 0)
   stats%maximum%q = maxmin_type (missing_r, 0, 0)
   stats%minimum%t = maxmin_type(-missing_r, 0, 0)
   stats%minimum%q = maxmin_type(-missing_r, 0, 0)

   stats%average = residual_airsr1_type(0.0, 0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(airsr)%nlocal
      if (iv%info(airsr)%proc_domain(1,n)) then
         do k=1, iv%info(airsr)%levels(n)
            call da_stats_calculate (n, k, iv%airsr(n)%t(k)%qc, & 
               re%airsr(n)%t(k), nt, &
               stats%minimum%t, stats%maximum%t, &
               stats%average%t, stats%rms_err%t)
            call da_stats_calculate (n, k, iv%airsr(n)%q(k)%qc, & 
               re%airsr(n)%q(k), nq, &
               stats%minimum%q, stats%maximum%q, &
               stats%average%q, stats%rms_err%q)
         end do
      end if
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nt)
   call da_proc_sum_int (nq)
   iv%nstats(airsr) = nt + nq

   call da_proc_stats_combine(stats%average%t, stats%rms_err%t, &
      stats%minimum%t%value, stats%maximum%t%value, &
      stats%minimum%t%n, stats%maximum%t%n, &
      stats%minimum%t%l, stats%maximum%t%l)
   call da_proc_stats_combine(stats%average%q, stats%rms_err%q, &
      stats%minimum%q%value, stats%maximum%q%value, &
      stats%minimum%q%n, stats%maximum%q%n, &
      stats%minimum%q%l, stats%maximum%q%l)

   if (rootproc) then
      if (nt /= 0 .or. nq /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for airs retrievals'
         call da_print_stats_airsr(stats_unit, nt, nq, stats)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_ao_stats_airsr")

end subroutine da_ao_stats_airsr


