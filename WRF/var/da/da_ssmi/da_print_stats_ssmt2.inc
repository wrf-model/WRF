subroutine da_print_stats_ssmt2(stats_unit, nrh, minimum, maximum, &
                                 average, rms_err)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,            intent(in)    :: stats_unit
   integer,            intent(inout) :: nrh
   type (maxmin_type), intent(in)    :: minimum
   type (maxmin_type), intent(in)    :: maximum
   real,               intent(in)    :: average
   real,               intent(in)    :: rms_err

   if (trace_use_dull) call da_trace_entry("da_print_stats_ssmt2")
   
   write(unit=stats_unit, fmt='(a/)') '   var             rh(K)     n    k'

   write(unit=stats_unit, fmt='(a,i16,4i22)') &
        '  Number: ', nrh

   if (nrh < 1) nrh = 1
   
   write(unit=stats_unit, fmt='((a,f12.4,2i5))') &
        ' Minimum(n,k): ', minimum,    &
        ' Maximum(n,k): ', maximum
   write(unit=stats_unit, fmt='((a,f12.4,10x))') &
        ' Average     : ', average/real(nrh),    &
        '    RMSE     : ', sqrt(rms_err/real(nrh))

end subroutine da_print_stats_ssmt2


