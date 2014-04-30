subroutine da_print_stats_qscat(stats_unit, nu, nv, qscat)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit  
   integer,                 intent(inout) :: nu, nv      
   type (stats_qscat_type), intent(in)    :: qscat

   if (trace_use_dull) call da_trace_entry("da_print_stats_qscat")

      write(unit=stats_unit, fmt='(3a/)') &
        '   var             ', &
        'u (m/s)     n    k    ', &
        'v (m/s)     n    k    '

   write(unit=stats_unit, fmt='(a,i16,4i22)') &
        '  Number: ', nu, nv

   if (nu < 1) nu = 1
   if (nv < 1) nv = 1

   write(unit=stats_unit, fmt='((a,2(f12.4,2i5)))') &
        ' Minimum(n,k): ', qscat%minimum%u, qscat%minimum%v, &
        ' Maximum(n,k): ', qscat%maximum%u, qscat%maximum%v
   
   write(unit=stats_unit, fmt='((a,2(f12.4,10x)))') &
        ' Average     : ', qscat%average%u/real(nu), qscat%average%v/real(nv), &
        '    RMSE     : ', sqrt(qscat%rms_err%u/real(nu)), &
                           sqrt(qscat%rms_err%v/real(nv))

   if (trace_use_dull) call da_trace_exit("da_print_stats_qscat")
   
end subroutine da_print_stats_qscat


