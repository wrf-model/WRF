subroutine da_print_stats_ships(stats_unit, nu, nv, nt, np, nq, ships)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nu, nv, nt, np, nq
   type (stats_ships_type), intent(in)    :: ships

   if (trace_use_dull) call da_trace_entry("da_print_stats_ships")

   write(unit=stats_unit, fmt='(6a/)') &
      '   var             ', &
      'u (m/s)     n    k    ', &
      'v (m/s)     n    k    ', &
      't (K)       n    k    ', &
      'p (Pa)      n    k    ', &
      'q (kg/kg)   n    k'

   write(unit=stats_unit, fmt='(a,i16,4i22)') &
      '  Number: ', nu, nv, nt, np, nq

   if (nu < 1) nu = 1
   if (nv < 1) nv = 1
   if (nt < 1) nt = 1
   if (np < 1) np = 1
   if (nq < 1) nq = 1

   write(unit=stats_unit, fmt='((a,4(f12.4,2i5),e12.4,2i5))') &
      ' Minimum(n,k): ', ships%minimum%u, ships%minimum%v, ships%minimum%t, &
                         ships%minimum%p, ships%minimum%q, &
      ' Maximum(n,k): ', ships%maximum%u, ships%maximum%v, ships%maximum%t, &
                         ships%maximum%p, ships%maximum%q
   write(unit=stats_unit, fmt='((a,4(f12.4,10x),e12.4,10x))') &
      ' Average     : ', ships%average%u/real(nu), ships%average%v/real(nv), &
                         ships%average%t/real(nt), ships%average%p/real(np), &
                         ships%average%q/real(nq), &
      '    RMSE     : ', sqrt(ships%rms_err%u/real(nu)), &
                         sqrt(ships%rms_err%v/real(nv)), &
                         sqrt(ships%rms_err%t/real(nt)), &
                         sqrt(ships%rms_err%p/real(np)), &
                         sqrt(ships%rms_err%q/real(nq))

   if (trace_use_dull) call da_trace_exit("da_print_stats_ships")

end subroutine da_print_stats_ships


