subroutine da_print_stats_airep(stats_unit, nu, nv, nt, airep)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nu, nv, nt
   type (stats_airep_type), intent(in)    :: airep

   if (trace_use_dull) call da_trace_entry("da_print_stats_airep")

   write(unit=stats_unit, fmt='(4a/)') &
      '   var             ', &
      'u (m/s)     n    k    ', &
      'v (m/s)     n    k    ', &
      't (K)       n    k    '

   write(unit=stats_unit, fmt='(a,i16,3i22)') &
      '  Number: ', nu, nv, nt

   if (nu < 1) nu = 1
   if (nv < 1) nv = 1
   if (nt < 1) nt = 1

   write(unit=stats_unit, fmt='((a,3(f12.4,2i5)))') &
      ' Minimum(n,k): ', airep%minimum%u, airep%minimum%v, airep%minimum%t, &
      ' Maximum(n,k): ', airep%maximum%u, airep%maximum%v, airep%maximum%t
   write(unit=stats_unit, fmt='((a,3(f12.4,10x)))') &
      ' Average     : ', airep%average%u/real(nu), airep%average%v/real(nv), airep%average%t/real(nt), &
      '    RMSE     : ', sqrt(airep%rms_err%u/real(nu)), sqrt(airep%rms_err%v/real(nv)), &
      sqrt(airep%rms_err%t/real(nt))

   if (trace_use_dull) call da_trace_exit("da_print_stats_airep")

end subroutine da_print_stats_airep


