subroutine da_print_aostats_chem_sfc(stats_unit, nchem, stats)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

#if (WRF_CHEM == 1)
use module_state_description, only : num_chem, PARAM_FIRST_SCALAR                   &
                     ,p_chemsi_pm25                              
#endif

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nchem
   type (stats_chem_sfc_type), intent(in)    :: stats

   if (trace_use_dull) call da_trace_entry("da_print_aostats_chem_sfc")

   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', &
      'pm25 (ug/kg)     n         k    '

   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem

   if (nchem < 1) nchem = 1

   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_pm25), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_pm25)

   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_pm25)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_pm25)/real(nchem))

   if (trace_use_dull) call da_trace_exit("da_print_aostats_chem_sfc")

end subroutine da_print_aostats_chem_sfc


