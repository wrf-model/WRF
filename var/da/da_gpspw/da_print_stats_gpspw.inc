subroutine da_print_stats_gpspw(stats_unit, ntpw, Gpspw)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: ntpw
   type (stats_gpspw_type), intent(in)    :: gpspw

   if (trace_use_dull) call da_trace_entry("da_residual_gpspw")

   if (ntpw > 0) then
      if (use_gpspwObs) then
        write(unit=stats_unit, fmt='(a/)') '   var           tpw(cm)     n'
      else if (use_gpsztdObs) then
        write(unit=stats_unit, fmt='(a/)') '   var           ztd(cm)     n'
      endif
 
      write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntpw

      write(unit=stats_unit, fmt='(a, f12.4,i5)') &
        ' Minimum(n): ', gpspw%minimum%tpw%value, &
                         gpspw%minimum%tpw%n    , &
        ' Maximum(n): ', gpspw%maximum%tpw%value, &
                         gpspw%maximum%tpw%n
      write(unit=stats_unit, fmt='(a, f12.4,5x)') &
        ' Average   : ', gpspw%average%tpw/real(ntpw), &
        '    RMSE   : ', sqrt(gpspw%rms_err%tpw/real(ntpw))
   end if

   if (trace_use_dull) call da_trace_exit("da_residual_gpspw")

end subroutine da_print_stats_gpspw


