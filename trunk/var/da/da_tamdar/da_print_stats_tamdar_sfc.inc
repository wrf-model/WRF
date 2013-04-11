subroutine da_print_stats_tamdar_sfc(stats_unit, nu, nv, nt, np, nq, tamdar_sfc)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                     intent(in)    :: stats_unit
   integer,                     intent(inout) :: nu, nv, nt, np, nq
   type (stats_tamdar_sfc_type), intent(in)    :: tamdar_sfc

   if (trace_use_dull) call da_trace_entry("da_print_stats_tamdar_sfc")

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
      ' Minimum(n,k): ', tamdar_sfc%minimum%u, tamdar_sfc%minimum%v, &
      tamdar_sfc%minimum%t, tamdar_sfc%minimum%p, tamdar_sfc%minimum%q, &
      ' Maximum(n,k): ', tamdar_sfc%maximum%u, tamdar_sfc%maximum%v, &
      tamdar_sfc%maximum%t, &
                        tamdar_sfc%maximum%p, tamdar_sfc%maximum%q
   write(unit=stats_unit, fmt='((a,4(f12.4,10x),e12.4,10x))') &
      ' Average     : ', tamdar_sfc%average%u/real(nu), &
      tamdar_sfc%average%v/real(nv), &
      tamdar_sfc%average%t/real(nt), tamdar_sfc%average%p/real(np), &
      tamdar_sfc%average%q/real(nq), &
      '    RMSE     : ', sqrt(tamdar_sfc%rms_err%u/real(nu)), &
      sqrt(tamdar_sfc%rms_err%v/real(nv)), &
      sqrt(tamdar_sfc%rms_err%t/real(nt)), &
      sqrt(tamdar_sfc%rms_err%p/real(np)), &
      sqrt(tamdar_sfc%rms_err%q/real(nq))

   if (trace_use_dull) call da_trace_exit("da_print_stats_tamdar_sfc")

end subroutine da_print_stats_tamdar_sfc


