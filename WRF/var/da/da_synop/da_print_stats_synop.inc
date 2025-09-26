subroutine da_print_stats_synop(stats_unit, nu, nv, nt, np, nq, stats)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nu, nv, nt, np, nq
   type (stats_synop_type), intent(in)    :: stats

   if (trace_use_dull) call da_trace_entry("da_print_stats_synop")

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
     ' Minimum(n,k): ', stats%minimum%u, stats%minimum%v, stats%minimum%t, &
                        stats%minimum%p, stats%minimum%q, &
     ' Maximum(n,k): ', stats%maximum%u, stats%maximum%v, stats%maximum%t, &
                        stats%maximum%p, stats%maximum%q
   write(unit=stats_unit, fmt='((a,4(f12.4,10x),e12.4,10x))') &
      ' Average     : ', stats%average%u/real(nu), stats%average%v/real(nv), &
                         stats%average%t/real(nt), stats%average%p/real(np), &
                         stats%average%q/real(nq), &
      '    RMSE     : ', sqrt(stats%rms_err%u/real(nu)), &
                         sqrt(stats%rms_err%v/real(nv)), &
                         sqrt(stats%rms_err%t/real(nt)), &
                         sqrt(stats%rms_err%p/real(np)), &
                         sqrt(stats%rms_err%q/real(nq))

   if (trace_use_dull) call da_trace_exit("da_print_stats_synop")

end subroutine da_print_stats_synop


