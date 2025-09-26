subroutine da_print_stats_rain(stats_unit, nrain, rain)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                intent(in)    :: stats_unit
   integer,                intent(inout) :: nrain 
   type (stats_rain_type), intent(in)  :: rain

   if (trace_use) call da_trace_entry("da_print_stats_rain")

   if (nrain > 0) then

      write(unit=stats_unit, fmt='(a/)') '   var        rainfall(mm)     n'

      write(unit=stats_unit, fmt='(a,i14)') '  Number: ', nrain

      write(unit=stats_unit, fmt='(a, f12.4,i5)') &
         ' Minimum(n): ', rain%minimum%rain%value, &
                          rain%minimum%rain%n    , &
         ' Maximum(n): ', rain%maximum%rain%value, &
                          rain%maximum%rain%n
      write(unit=stats_unit, fmt='(a, f12.4,5x)') &
         ' Average   : ', rain%average%rain/real(nrain), &
         '    RMSE   : ', sqrt(rain%rms_err%rain/real(nrain))
   end if

   if (trace_use) call da_trace_exit("da_print_stats_rain")

end subroutine da_print_stats_rain


