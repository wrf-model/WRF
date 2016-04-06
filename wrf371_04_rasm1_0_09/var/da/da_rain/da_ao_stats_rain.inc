subroutine da_ao_stats_rain (stats_unit, iv, re)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: stats_unit    ! Output unit for stats.
   type (iv_type), intent(inout) :: iv            ! iv
   type (y_type),  intent(in)    :: re            ! A - O

   type (stats_rain_type) :: stats
   integer                :: nrain
   integer                :: n
   
   if (trace_use) call da_trace_entry("da_ao_stats_rain")    

   nrain = 0

   stats%maximum%rain = maxmin_type (missing_r, 0, 0)
   stats%minimum%rain = maxmin_type(-missing_r, 0, 0)

   stats%average = residual_rain1_type(0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(rain)%nlocal
      if (iv%info(rain)%proc_domain(1,n)) then
         call da_stats_calculate (n, 0, iv%rain(n)%rain%qc, & 
            re%rain(n)%rain, nrain, & 
            stats%minimum%rain, stats%maximum%rain, &
            stats%average%rain, stats%rms_err%rain)
      end if
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nrain)
   iv%nstats(rain) = nrain

   call da_proc_stats_combine(stats%average%rain, stats%rms_err%rain, &
      stats%minimum%rain%value, stats%maximum%rain%value, &
      stats%minimum%rain%n, stats%maximum%rain%n, &
      stats%minimum%rain%l, stats%maximum%rain%l)
   if (rootproc) then
      if (nrain /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for rainfall'
         call da_print_stats_rain(stats_unit, nrain, stats)
      end if
   end if
   
   if (trace_use) call da_trace_exit("da_ao_stats_rain")    

 end subroutine da_ao_stats_rain


