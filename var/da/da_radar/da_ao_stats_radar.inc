subroutine da_ao_stats_radar (stats_unit, iv, re)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)    :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (inout) :: iv            ! iv
   type (y_type),  intent (in)    :: re            ! A - O

   type (stats_radar_type) :: stats
   integer                 :: nrv, nrf
   integer                 :: n, k

   if (trace_use) call da_trace_entry("da_ao_stats_radar")

   nrv = 0
   nrf = 0

   stats%maximum%rv = maxmin_type (missing_r, 0, 0)
   stats%maximum%rf = maxmin_type (missing_r, 0, 0)
   stats%minimum%rv = maxmin_type(-missing_r, 0, 0)
   stats%minimum%rf = maxmin_type(-missing_r, 0, 0)

   stats%average = residual_radar1_type(0, 0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(radar)%nlocal
      if (iv%info(radar)%proc_domain(1,n)) then
         do k=1, iv%info(radar)%levels(n)
            if (use_radar_rv) then
               call da_stats_calculate (n, k, iv%radar(n)%rv(k)%qc, & 
                  re%radar(n)%rv(k), nrv, & 
                  stats%minimum%rv, stats%maximum%rv, &
                  stats%average%rv, stats%rms_err%rv)
            end if

            if (use_radar_rf) then
               call da_stats_calculate (n, k, iv%radar(n)%rf(k)%qc, & 
                  re%radar(n)%rf(k), nrf, & 
                  stats%minimum%rf, stats%maximum%rf, &
                  stats%average%rf, stats%rms_err%rf)
            end if
         end do
      end if     ! end if (iv%pilot(n)%loc%proc_domain)
   end do

   ! Do inter-processor communication to gather statistics.
   if (use_radar_rv) then
      call da_proc_sum_int (nrv)
      call da_proc_stats_combine(stats%average%rv, stats%rms_err%rv, &
         stats%minimum%rv%value, stats%maximum%rv%value, &
         stats%minimum%rv%n, stats%maximum%rv%n, &
         stats%minimum%rv%l, stats%maximum%rv%l)
   end if

   if (use_radar_rf) then
      call da_proc_sum_int (nrf)
      call da_proc_stats_combine(stats%average%rf, stats%rms_err%rf, &
          stats%minimum%rf%value, stats%maximum%rf%value, &
          stats%minimum%rf%n, stats%maximum%rf%n, &
          stats%minimum%rf%l, stats%maximum%rf%l)
   end if
   iv%nstats(radar) = nrv + nrf

   if (rootproc) then
      if (nrv /= 0 .or. nrf /= 0) then 
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for radar'
         call da_print_stats_radar(stats_unit, nrv, nrf, stats)
      end if
   end if

   if (trace_use) call da_trace_exit("da_ao_stats_radar")

end subroutine da_ao_stats_radar


