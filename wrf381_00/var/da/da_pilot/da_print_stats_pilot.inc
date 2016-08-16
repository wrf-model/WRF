subroutine da_print_stats_pilot(stats_unit, nu, nv, Pilot)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nu, nv
   type (stats_pilot_type), intent(in)    :: pilot

   if (trace_use_dull) call da_trace_entry("da_print_stats_pilot")

   write(unit=stats_unit, fmt='(a/)') &
      '   var             u (m/s)     n    k    v (m/s)     n    k  '

   write(unit=stats_unit, fmt='(a,i16,i22)') &
      '  Number: ', nu, nv

   if (nu < 1) nu = 1
   if (nv < 1) nv = 1

   write(unit=stats_unit, fmt='((a,2(f12.4,2i5)))') &
       ' Minimum(n,k): ', pilot%minimum%u, pilot%minimum%v, &
       ' Maximum(n,k): ', pilot%maximum%u, pilot%maximum%v
   write(unit=stats_unit, fmt='((a,2(f12.4,10x)))')  &
      ' Average     : ', pilot%average%u/real(nu), &
                         pilot%average%v/real(nv),      &
      '    RMSE     : ', sqrt(pilot%rms_err%u/real(nu)), &
                         sqrt(pilot%rms_err%v/real(nv))

   if (trace_use_dull) call da_trace_exit("da_print_stats_pilot")

end subroutine da_print_stats_pilot


