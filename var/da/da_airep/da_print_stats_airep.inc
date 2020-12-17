subroutine da_print_stats_airep(stats_unit, nu, nv, nt, nq, airep)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nu, nv, nt, nq
   type (stats_airep_type), intent(in)    :: airep

   if (trace_use_dull) call da_trace_entry("da_print_stats_airep")

   write(unit=stats_unit, fmt='(5a/)') &
      '   var             ', &
      'u (m/s)     n    k    ', &
      'v (m/s)     n    k    ', &
      't (K)       n    k    ', &
      'q (kg/kg)   n    k'

   write(unit=stats_unit, fmt='(a,i16,4i22)') &
      '  Number: ', nu, nv, nt, nq

   if (nu < 1) nu = 1
   if (nv < 1) nv = 1
   if (nt < 1) nt = 1
   if (nq < 1) nq = 1

   write(unit=stats_unit, fmt='((a,3(f12.4,2i5),e12.4,2i5))') &
      ' Minimum(n,k): ', airep%minimum%u, airep%minimum%v, airep%minimum%t, airep%minimum%q, &
      ' Maximum(n,k): ', airep%maximum%u, airep%maximum%v, airep%maximum%t, airep%maximum%q
   write(unit=stats_unit, fmt='((a,3(f12.4,10x),e12.4,10x))') &
      ' Average     : ', airep%average%u/real(nu), airep%average%v/real(nv), airep%average%t/real(nt), airep%average%q/real(nq), &
      '    RMSE     : ', sqrt(airep%rms_err%u/real(nu)), sqrt(airep%rms_err%v/real(nv)), &
      sqrt(airep%rms_err%t/real(nt)), sqrt(airep%rms_err%q/real(nq))

   if (trace_use_dull) call da_trace_exit("da_print_stats_airep")

end subroutine da_print_stats_airep


