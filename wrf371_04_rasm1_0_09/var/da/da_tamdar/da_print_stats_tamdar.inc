subroutine da_print_stats_tamdar(stats_unit, nu, nv, nt, nq, tamdar)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nu, nv, nt, nq
   type (stats_tamdar_type), intent(in)    :: tamdar

   if (trace_use_dull) call da_trace_entry("da_print_stats_tamdar")

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
      ' Minimum(n,k): ', tamdar%minimum%u, tamdar%minimum%v, &
                         tamdar%minimum%t, tamdar%minimum%q, &
      ' Maximum(n,k): ', tamdar%maximum%u, tamdar%maximum%v, &
                         tamdar%maximum%t, tamdar%maximum%q
   write(unit=stats_unit, fmt='((a,3(f12.4,10x),e12.4,10x))') &
      ' Average     : ', tamdar%average%u/real(nu), &
                         tamdar%average%v/real(nv), &
                         tamdar%average%t/real(nt), &
                         tamdar%average%q/real(nq), &
      '    RMSE     : ', sqrt(tamdar%rms_err%u/real(nu)), &
                         sqrt(tamdar%rms_err%v/real(nv)), &
                         sqrt(tamdar%rms_err%t/real(nt)), &
                         sqrt(tamdar%rms_err%q/real(nq))

   if (trace_use_dull) call da_trace_exit("da_print_stats_tamdar")

end subroutine da_print_stats_tamdar


