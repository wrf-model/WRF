subroutine da_print_stats_sound(stats_unit, nu, nv, nt, nq, Sound)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nu, nv, nt, nq
   type (stats_sound_type), intent(in)    :: sound

   if (trace_use_dull) call da_trace_entry("da_print_stats_sound")

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
      ' Minimum(n,k): ', sound%minimum%u, sound%minimum%v, &
                         sound%minimum%t, sound%minimum%q, &
      ' Maximum(n,k): ', sound%maximum%u, sound%maximum%v, &
                         sound%maximum%t, sound%maximum%q
   write(unit=stats_unit, fmt='((a,3(f12.4,10x),e12.4,10x))') &
      ' Average     : ', sound%average%u/real(nu), &
                         sound%average%v/real(nv), &
                         sound%average%t/real(nt), &
                         sound%average%q/real(nq), &
      '    RMSE     : ', sqrt(sound%rms_err%u/real(nu)), &
                         sqrt(sound%rms_err%v/real(nv)), &
                         sqrt(sound%rms_err%t/real(nt)), &
                         sqrt(sound%rms_err%q/real(nq))

   if (trace_use_dull) call da_trace_exit("da_print_stats_sound")

end subroutine da_print_stats_sound


