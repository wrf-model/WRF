subroutine da_print_stats_mtgirs(stats_unit, nu, nv, nt, nq, mtgirs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nu, nv, nt, nq
   type (stats_mtgirs_type), intent(in)    :: mtgirs

   if (trace_use_dull) call da_trace_entry("da_print_stats_mtgirs")

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
      ' Minimum(n,k): ', mtgirs%minimum%u, mtgirs%minimum%v, &
                         mtgirs%minimum%t, mtgirs%minimum%q, &
      ' Maximum(n,k): ', mtgirs%maximum%u, mtgirs%maximum%v, &
                         mtgirs%maximum%t, mtgirs%maximum%q
   write(unit=stats_unit, fmt='((a,3(f12.4,10x),e12.4,10x))') &
      ' Average     : ', mtgirs%average%u/real(nu), &
                         mtgirs%average%v/real(nv), &
                         mtgirs%average%t/real(nt), &
                         mtgirs%average%q/real(nq), &
      '    RMSE     : ', sqrt(mtgirs%rms_err%u/real(nu)), &
                         sqrt(mtgirs%rms_err%v/real(nv)), &
                         sqrt(mtgirs%rms_err%t/real(nt)), &
                         sqrt(mtgirs%rms_err%q/real(nq))

   if (trace_use_dull) call da_trace_exit("da_print_stats_mtgirs")

end subroutine da_print_stats_mtgirs


