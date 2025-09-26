subroutine da_print_stats_gpsref(stats_unit, ngpsref, GPSRef)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                  intent(in)    :: stats_unit
   integer,                  intent(inout) :: ngpsref
   type (stats_gpsref_type), intent(in)    :: gpsref

   if (trace_use_dull) call da_trace_entry("da_print_stats_gpsref")
   
   write (unit=stats_unit, fmt='(a/)') '   var           ref(m)  n    k'  

   write (unit=stats_unit, fmt='(a,i16)') '  Number: ', ngpsref

   if (ngpsref < 1) ngpsref = 1
   
   write(unit=stats_unit, fmt='((a,f12.4,2i5))') &
      ' Minimum(n,k): ', GPSRef%minimum%ref,    &
      ' Maximum(n,k): ', GPSRef%maximum%ref
   write(unit=stats_unit, fmt='((a,f12.4,10x))') &
      ' Average     : ', GPSRef%average%ref/real(ngpsref),    &
      '    RMSE     : ', sqrt(GPSRef%rms_err%ref/real(ngpsref))

   if (trace_use_dull) call da_trace_exit("da_print_stats_gpsref")

end subroutine da_print_stats_gpsref


