subroutine da_print_stats_geoamv(stats_unit, nu, nv, geoamv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                  intent(in)    :: stats_unit
   integer,                  intent(inout) :: nu, nv
   type (stats_geoamv_type), intent(in)    :: geoamv

   if (trace_use_dull) call da_trace_entry("da_print_stats_geoamv")

   write(unit=stats_unit, fmt='(a/)') &
      '   var             u (m/s)     n    k    v (m/s)     n    k'

   write(unit=stats_unit, fmt='(a,i16,4i22)') &
      '  Number: ', nu, nv

   if (nu < 1) nu = 1
   if (nv < 1) nv = 1

   write(unit=stats_unit, fmt='((a,2(f12.4,2i5)))') &
      ' Minimum(n,k): ', geoamv%minimum%u, geoamv%minimum%v, &
      ' Maximum(n,k): ', geoamv%maximum%u, geoamv%maximum%v

   write(unit=stats_unit, fmt='((a,2(f12.4,10x)))') &
      ' Average     : ', geoamv%average%u/real(nu), geoamv%average%v/real(nv), &
      '    RMSE     : ', sqrt(geoamv%rms_err%u/real(nu)), &
                         sqrt(geoamv%rms_err%v/real(nv))

   if (trace_use_dull) call da_trace_exit("da_print_stats_geoamv")

end subroutine da_print_stats_geoamv


