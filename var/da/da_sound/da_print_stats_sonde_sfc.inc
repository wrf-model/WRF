subroutine da_print_stats_sonde_sfc(stats_unit, nu, nv, nt, np, nq, sonde_sfc)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                     intent(in)    :: stats_unit
   integer,                     intent(inout) :: nu, nv, nt, np, nq
   type (stats_sonde_sfc_type), intent(in)    :: sonde_sfc

   if (trace_use_dull) call da_trace_entry("da_print_stats_sonde_sfc")

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
      ' Minimum(n,k): ', sonde_sfc%minimum%u, sonde_sfc%minimum%v, &
      sonde_sfc%minimum%t, sonde_sfc%minimum%p, sonde_sfc%minimum%q, &
      ' Maximum(n,k): ', sonde_sfc%maximum%u, sonde_sfc%maximum%v, &
      sonde_sfc%maximum%t, &
                        sonde_sfc%maximum%p, sonde_sfc%maximum%q
   write(unit=stats_unit, fmt='((a,4(f12.4,10x),e12.4,10x))') &
      ' Average     : ', sonde_sfc%average%u/real(nu), &
      sonde_sfc%average%v/real(nv), &
      sonde_sfc%average%t/real(nt), sonde_sfc%average%p/real(np), &
      sonde_sfc%average%q/real(nq), &
      '    RMSE     : ', sqrt(sonde_sfc%rms_err%u/real(nu)), &
      sqrt(sonde_sfc%rms_err%v/real(nv)), &
      sqrt(sonde_sfc%rms_err%t/real(nt)), &
      sqrt(sonde_sfc%rms_err%p/real(np)), &
      sqrt(sonde_sfc%rms_err%q/real(nq))

   if (trace_use_dull) call da_trace_exit("da_print_stats_sonde_sfc")

end subroutine da_print_stats_sonde_sfc


