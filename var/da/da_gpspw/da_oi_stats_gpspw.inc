
subroutine da_oi_stats_gpspw (stats_unit, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in) :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (in) :: iv            ! OI

   type (stats_gpspw_type) :: stats
   integer                 :: ntpw
   integer                 :: n

   if (trace_use_dull) call da_trace_entry("da_oi_stats_gpspw")

   ntpw = 0

   stats%maximum%tpw = maxmin_type(missing_r, 0, 0)
   stats%minimum%tpw = maxmin_type(-missing_r, 0, 0)

   stats%average = residual_gpspw1_type(0.0)
   stats%rms_err = stats%average
   do n=1, iv%info(gpspw)%nlocal
      if (iv%info(gpspw)%proc_domain(1,n)) then
         call da_stats_calculate(iv%info(gpspw)%obs_global_index(n), &
            0, iv%gpspw(n)%tpw%qc, &
            iv%gpspw(n)%tpw%inv, ntpw, &
            stats%minimum%tpw  , stats%maximum%tpw, &
            stats%average%tpw  , stats%rms_err%tpw)

      end if    ! end if (iv%info(gpspw)%proc_domain(1,n))
   end do

   ! do inter-processor communication to gather statistics.

   call da_proc_sum_int(ntpw)

   call da_proc_stats_combine(stats%average%tpw, stats%rms_err%tpw, &
      stats%minimum%tpw%value, stats%maximum%tpw%value, &
      stats%minimum%tpw%n, stats%maximum%tpw%n, &
      stats%minimum%tpw%l, stats%maximum%tpw%l)

   if (rootproc) then
      if (ntpw /= 0) then
        if (use_gpspwObs) then
          write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for gpspw'
        else if (use_gpsztdObs) then
          write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for gpsztd'
        endif

         call da_print_stats_gpspw(stats_unit, ntpw, stats)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_oi_stats_gpspw")

end subroutine da_oi_stats_gpspw


