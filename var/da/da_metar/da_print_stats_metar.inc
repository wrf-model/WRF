subroutine da_print_stats_metar(stats_unit, nu, nv, nt, np, nq, metar)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,           intent(in)    :: stats_unit
   integer,           intent(inout) :: nu, nv, nt, np, nq
   type (stats_metar_type), intent(in)    :: metar

   if (trace_use_dull) call da_trace_entry("da_print_stats_metar")

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
      ' Minimum(n,k): ', metar%minimum%u, metar%minimum%v, metar%minimum%t, &
                       metar%minimum%p, metar%minimum%q, &
      ' Maximum(n,k): ', metar%maximum%u, metar%maximum%v, metar%maximum%t, &
                       metar%maximum%p, metar%maximum%q
   write(unit=stats_unit, fmt='((a,4(f12.4,10x),e12.4,10x))') &
      ' Average     : ', metar%average%u/real(nu), metar%average%v/real(nv), &
                       metar%average%t/real(nt), metar%average%p/real(np), &
                       metar%average%q/real(nq), &
      '    RMSE     : ', sqrt(metar%rms_err%u/real(nu)), &
                       sqrt(metar%rms_err%v/real(nv)), &
                       sqrt(metar%rms_err%t/real(nt)), &
                       sqrt(metar%rms_err%p/real(np)), &
                       sqrt(metar%rms_err%q/real(nq))

   if (trace_use_dull) call da_trace_exit("da_print_stats_metar")

end subroutine da_print_stats_metar


