subroutine da_oi_stats_ssmi_rv(stats_unit, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in) :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (in) :: iv            ! iv

   integer                   :: ntpw, nspeed, n
   type (stats_ssmi_rv_type) :: stats

   if (trace_use) call da_trace_entry("da_oi_stats_ssmi_rv")

   ntpw = 0
   nspeed = 0

   stats%maximum%tpw   = maxmin_type(missing_r, 0, 0)
   stats%maximum%speed = maxmin_type(missing_r, 0, 0)
   stats%minimum%tpw   = maxmin_type(-missing_r, 0, 0)
   stats%minimum%speed = maxmin_type(-missing_r, 0, 0)
   stats%average = residual_ssmi_rv_type(0.0, 0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(ssmi_rv)%nlocal
      if (iv%info(ssmi_rv)%proc_domain(1,n)) then
         call da_stats_calculate(iv%info(ssmi_rv)%obs_global_index(n), &
           0, iv%ssmi_rv(n)%tpw%qc, &
           iv%ssmi_rv(n)%tpw%inv, ntpw, &
           stats%minimum%tpw, &
           stats%maximum%tpw,  &
           stats%average%tpw, &
           stats%rms_err%tpw)
        call da_stats_calculate(iv%info(ssmi_rv)%obs_global_index(n), &
           0, iv%ssmi_rv(n)%speed%qc, &
           iv%ssmi_rv(n)%speed%inv, nspeed, &
           stats%minimum%speed, &
           stats%maximum%speed, &
           stats%average%speed, &
           stats%rms_err%speed)
      end if
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(ntpw)
   call da_proc_sum_int(nspeed)

   call da_proc_stats_combine(stats%average%tpw, stats%rms_err%tpw, &
       stats%minimum%tpw%value, stats%maximum%tpw%value, &
       stats%minimum%tpw%n, stats%maximum%tpw%n, &
       stats%minimum%tpw%l, stats%maximum%tpw%l)

   call da_proc_stats_combine(stats%average%speed, stats%rms_err%speed, &
      stats%minimum%speed%value, stats%maximum%speed%value, &
      stats%minimum%speed%n, stats%maximum%speed%n, &
      stats%minimum%speed%l, stats%maximum%speed%l)

   if (rootproc) then
      if (ntpw > 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for ssmi_retrieval'
         write(unit=stats_unit, fmt='(a/)') '   var           tpw(cm)     n'
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntpw
         write(unit=stats_unit, fmt='(a, f12.4,i9)') &
            ' Minimum(n): ', stats%minimum%tpw%value, stats%minimum%tpw%n    , &
            ' Maximum(n): ', stats%maximum%tpw%value, stats%maximum%tpw%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
            ' Average   : ', stats%average%tpw/real(ntpw), &
            '    RMSE   : ', sqrt(stats%rms_err%tpw/real(ntpw))
      end if

      if (nspeed > 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for ssmi_retrieval'
         write(unit=stats_unit, fmt='(a/)') '   var           speed(m/s)     n'
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', nspeed
         write(unit=stats_unit, fmt='(a, f12.4,i9)') &
           ' Minimum(n): ', stats%minimum%speed%value, stats%minimum%speed%n    , &
           ' Maximum(n): ', stats%maximum%speed%value, stats%maximum%speed%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
             ' Average   : ', stats%average%speed/real(nspeed), &
             '    RMSE   : ', sqrt(stats%rms_err%speed/real(nspeed))
      end if
   end if

   if (trace_use) call da_trace_exit("da_iv_stats_ssmi_rv")

end subroutine da_oi_stats_ssmi_rv


