subroutine da_print_stats_bogus(stats_unit, nu, nv, nt, nq, nslp, Bogus)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nu, nv, nt, nq, nslp
   type (stats_bogus_type), intent(in)    :: bogus

   if (trace_use_dull) call da_trace_entry("da_print_stats_bogus") 

   write(unit=stats_unit, fmt='(6a/)') &
      '   var             ', &
      'u (m/s)     n    k    ', &
      'v (m/s)     n    k    ', &
      't (K)       n    k    ', &
      'q (kg/kg)   n    k    ', &
      'slp (pa)    n    k'

   write(unit=stats_unit, fmt='(a,i16,4i22)') &
      '  Number: ', nu, nv, nt, nq, nslp

   if (nu < 1) nu = 1
   if (nv < 1) nv = 1
   if (nt < 1) nt = 1
   if (nq < 1) nq = 1
   if (nslp < 1) nslp = 1

   write(unit=stats_unit, fmt='((a,3(f12.4,2i5),e12.4,2i5,f12.4,2i5))') &
      ' Minimum(n,k): ', Bogus%minimum%u, Bogus%minimum%v, Bogus%minimum%t, &
                         Bogus%minimum%q, Bogus%minimum%slp,                &
      ' Maximum(n,k): ', Bogus%maximum%u, Bogus%maximum%v, Bogus%maximum%t, &
                         Bogus%maximum%q, Bogus%maximum%slp
   write(unit=stats_unit, fmt='((a,3(f12.4,10x),e12.4,10x,f12.4,10x))') &
      ' Average     : ', Bogus%average%u/real(nu), &
                         Bogus%average%v/real(nv), &
                         Bogus%average%t/real(nt), &
                         Bogus%average%q/real(nq), &
                         Bogus%average%slp/real(nslp),  &
      '    RMSE     : ', sqrt(Bogus%rms_err%u/real(nu)), &
                         sqrt(Bogus%rms_err%v/real(nv)), &
                         sqrt(Bogus%rms_err%t/real(nt)), &
                         sqrt(Bogus%rms_err%q/real(nq)), &
                         sqrt(Bogus%rms_err%slp/real(nslp))

   if (trace_use_dull) call da_trace_exit("da_print_stats_bogus") 

end subroutine da_print_stats_bogus


