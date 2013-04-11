subroutine da_print_stats_profiler(stats_unit, nu, nv, Profiler)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                    intent(in)    :: stats_unit
   integer,                    intent(inout) :: nu, nv
   type (stats_profiler_type), intent(in)    :: profiler

   if (trace_use_dull) call da_trace_entry("da_print_stats_profiler")
   
   write(unit=stats_unit, fmt='(a/)') &
      '   var             u (m/s)     n    k    v (m/s)     n    k  '

   write(unit=stats_unit, fmt='(a,i16,i22)') &
      '  Number: ', nu, nv

   if (nu < 1) nu = 1
   if (nv < 1) nv = 1
   
   write(unit=stats_unit, fmt='((a,2(f12.4,2i5)))') &
      ' Minimum(n,k): ', Profiler%minimum%u, Profiler%minimum%v, &
      ' Maximum(n,k): ', Profiler%maximum%u, Profiler%maximum%v
   write(unit=stats_unit, fmt='((a,2(f12.4,10x)))')  &
      ' Average     : ', Profiler%average%u/real(nu), &
                    Profiler%average%v/real(nv),      &
      '    RMSE     : ', sqrt(Profiler%rms_err%u/real(nu)), &
                    sqrt(Profiler%rms_err%v/real(nv))

   if (trace_use_dull) call da_trace_exit("da_print_stats_profiler")

end subroutine da_print_stats_profiler


