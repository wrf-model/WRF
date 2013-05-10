subroutine da_print_stats_radar(stats_unit, nrv, nrf, radar)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nrv, nrf
   type (stats_radar_type), intent(in)    :: radar

   if (trace_use) call da_trace_entry("da_print_stats_radar")
   
   write(unit=stats_unit, fmt='(a/)') &
      '   var             rv (m/s)        n    k         rf (dBZ)        n    k  '

   write(unit=stats_unit, fmt='(a,i16,i31)') &
      '  Number: ', nrv, nrf

   if (nrv < 1) nrv = 1
   if (nrf < 1) nrf = 1
   
   write(unit=stats_unit, fmt='((a,f12.4,i9,i5, f17.4,i9,i5))')  &
      ' Minimum(n,k): ', radar%minimum%rv, radar%minimum%rf, &
      ' Maximum(n,k): ', radar%maximum%rv, radar%maximum%rf
   write(unit=stats_unit, fmt='((a,2(f12.4,19x)))')              &
      ' Average     : ', radar%average%rv/real(nrv), radar%average%rf/real(nrf), &
      '    RMSE     : ', sqrt(radar%rms_err%rv/real(nrv)), &
      sqrt(radar%rms_err%rf/real(nrf))

   if (trace_use) call da_trace_exit("da_print_stats_radar")

end subroutine da_print_stats_radar


