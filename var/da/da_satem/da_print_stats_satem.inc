subroutine da_print_stats_satem(stats_unit, nthickness, satem)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nthickness
   type (stats_satem_type), intent(in)    :: satem

   if (trace_use_dull) call da_trace_entry("da_print_stats_satem")

   write(unit=stats_unit, fmt='(a/)') &
      '   var           thickness(m)  n    k'  

   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nthickness

   if (nthickness < 1) nthickness = 1

   write(unit=stats_unit, fmt='((a,f12.4,2i5))') &
      ' Minimum(n,k): ', satem%minimum%thickness,    &
      ' Maximum(n,k): ', satem%maximum%thickness
   write(unit=stats_unit, fmt='((a,f12.4,10x))') &
      ' Average     : ', satem%average%thickness/real(nthickness),    &
      '    RMSE     : ', sqrt(satem%rms_err%thickness/real(nthickness))

   if (trace_use_dull) call da_trace_exit("da_print_stats_satem")

end subroutine da_print_stats_satem


