subroutine da_print_stats_buoy(stats_unit, nu, nv, nt, np, nq, buoy)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                intent(in)    :: stats_unit
   integer,                intent(inout) :: nu, nv, nt, np, nq
   type (stats_buoy_type), intent(in)    :: buoy

   if (trace_use_dull) call da_trace_entry("da_print_stats_buoy")

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
      ' Minimum(n,k): ', buoy%minimum%u, buoy%minimum%v, buoy%minimum%t, &
                         buoy%minimum%p, buoy%minimum%q, &
      ' Maximum(n,k): ', buoy%maximum%u, buoy%maximum%v, buoy%maximum%t, &
                         buoy%maximum%p, buoy%maximum%q
   write(unit=stats_unit, fmt='((a,4(f12.4,10x),e12.4,10x))') &
      ' Average     : ', buoy%average%u/real(nu), buoy%average%v/real(nv), &
                         buoy%average%t/real(nt), buoy%average%p/real(np), &
                         buoy%average%q/real(nq), &
      '    RMSE     : ', sqrt(buoy%rms_err%u/real(nu)), &
                         sqrt(buoy%rms_err%v/real(nv)), &
                         sqrt(buoy%rms_err%t/real(nt)), &
                         sqrt(buoy%rms_err%p/real(np)), &
                         sqrt(buoy%rms_err%q/real(nq))

   if (trace_use_dull) call da_trace_exit("da_print_stats_buoy")

end subroutine da_print_stats_buoy


