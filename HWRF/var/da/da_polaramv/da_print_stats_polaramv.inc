subroutine da_print_stats_polaramv(stats_unit, nu, nv, polaramv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                    intent(in)    :: stats_unit
   integer,                    intent(inout) :: nu, nv
   type (stats_polaramv_type), intent(in)    :: polaramv

   if (trace_use_dull) call da_trace_entry("da_print_stats_polaramv")

   write(unit=stats_unit, fmt='(a/)') &
      '   var             u (m/s)     n    k    v (m/s)     n    k'

   write(unit=stats_unit, fmt='(a,i16,4i22)') &
      '  Number: ', nu, nv

   if (nu < 1) nu = 1
   if (nv < 1) nv = 1

   write(unit=stats_unit, fmt='((a,2(f12.4,2i5)))') &
      ' Minimum(n,k): ', polaramv%minimum%u, polaramv%minimum%v, &
      ' Maximum(n,k): ', polaramv%maximum%u, polaramv%maximum%v

   write(unit=stats_unit, fmt='((a,2(f12.4,10x)))') &
      ' Average     : ', polaramv%average%u/real(nu), &
      polaramv%average%v/real(nv), &
      '    RMSE     : ', sqrt(polaramv%rms_err%u/real(nu)), &
                      sqrt(polaramv%rms_err%v/real(nv))

   if (trace_use_dull) call da_trace_exit("da_print_stats_polaramv")

end subroutine da_print_stats_polaramv


