subroutine da_print_stats_gpseph(stats_unit, ngpseph, gpseph)

   !-----------------------------------------------------------------------
   ! Purpose: write out gpseph O-B statistics
   !-----------------------------------------------------------------------

   implicit none

   integer,                  intent(in)    :: stats_unit
   integer,                  intent(inout) :: ngpseph
   type (stats_gpseph_type), intent(in)    :: gpseph

   if (trace_use_dull) call da_trace_entry("da_print_stats_gpseph")

   write (unit=stats_unit, fmt='(a/)') '   var           eph(m)  n    k'

   write (unit=stats_unit, fmt='(a,i16)') '  Number: ', ngpseph

   if (ngpseph < 1) ngpseph = 1

   write(unit=stats_unit, fmt='((a,f12.4,2i5))') &
      ' Minimum(n,k): ', gpseph%minimum%eph,    &
      ' Maximum(n,k): ', gpseph%maximum%eph
   write(unit=stats_unit, fmt='((a,f12.4,10x))') &
      ' Average     : ', gpseph%average%eph/real(ngpseph),    &
      '    RMSE     : ', sqrt(gpseph%rms_err%eph/real(ngpseph))

   if (trace_use_dull) call da_trace_exit("da_print_stats_gpseph")

end subroutine da_print_stats_gpseph
