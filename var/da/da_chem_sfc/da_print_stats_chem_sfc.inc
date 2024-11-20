subroutine da_print_stats_chem_sfc(stats_unit, nchem, stats, ichem)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

#if (WRF_CHEM == 1)
use module_state_description, only : num_chem, PARAM_FIRST_SCALAR                   &
                     ,p_chemsi_pm25, p_chemsi_pm10 
#endif

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nchem
   type (stats_chem_sfc_type), intent(in)    :: stats
   integer,  intent(in), optional    :: ichem

   ! local variables (Ha)
   integer :: ipc
   character*64   :: str_pm2,str_pm1,str_so2,str_no2,str_o3,str_co
   str_pm2 = 'pm25 (ug/m3)     n         k    '
   str_pm1 = 'pm10 (ug/m3)     n         k    '
   str_so2 = 'so2 (ug/m3)     n         k    '
   str_no2 = 'no2 (ug/m3)     n         k    '
   str_o3  = 'o3 (ug/m3)     n         k    '
   str_co  = 'co (ug/m3)     n         k    '

   if (chem_cv_options >= 108) then
   str_so2 = 'so2 (ppmv)      n         k    '
   str_no2 = 'no2 (ppmv)      n         k    '
   str_o3  = 'o3 (ppmv)      n         k    '
   str_co  = 'co (ppmv)      n         k    '
   endif

   if (trace_use_dull) call da_trace_entry("da_print_stats_chem_sfc")

   if (chemicda_opt == 1 ) then
   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', str_pm2
   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem
   if (nchem < 1) nchem = 1
   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_pm25), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_pm25)
   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_pm25)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_pm25)/real(nchem))

   else if (chemicda_opt == 2 ) then
   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', str_pm1
   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem
   if (nchem < 1) nchem = 1
   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_pm10), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_pm10)
   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_pm10)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_pm10)/real(nchem))

   else if (chemicda_opt == 3) then
   if (present(ichem).and.ichem == PARAM_FIRST_SCALAR) then
   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', str_pm2
   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem
   if (nchem < 1) nchem = 1
   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_pm25), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_pm25)
   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_pm25)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_pm25)/real(nchem))

   else if (present(ichem).and.ichem == PARAM_FIRST_SCALAR+1) then
   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', str_pm1
   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem
   if (nchem < 1) nchem = 1
   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_pm10), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_pm10)
   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_pm10)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_pm10)/real(nchem))

   end if

   else if (chemicda_opt == 4) then
   if (present(ichem).and.ichem == PARAM_FIRST_SCALAR) then
   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', str_so2
   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem
   if (nchem < 1) nchem = 1
   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_so2), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_so2)
   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_so2)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_so2)/real(nchem))

   else if (present(ichem).and.ichem == PARAM_FIRST_SCALAR+1) then
   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', str_no2
   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem
   if (nchem < 1) nchem = 1
   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_no2), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_no2)
   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_no2)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_no2)/real(nchem))

   else if (present(ichem).and.ichem == PARAM_FIRST_SCALAR+2) then
   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', str_o3
   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem
   if (nchem < 1) nchem = 1
   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_o3), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_o3)
   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_o3)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_o3)/real(nchem))

   else if (present(ichem).and.ichem == PARAM_FIRST_SCALAR+3) then
   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', str_co
   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem
   if (nchem < 1) nchem = 1
   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_co), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_co)
   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_co)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_co)/real(nchem))

   end if

   else if (chemicda_opt == 5) then
   if (present(ichem).and.ichem == PARAM_FIRST_SCALAR) then
   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', str_pm2
   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem
   if (nchem < 1) nchem = 1
   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_pm25), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_pm25)
   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_pm25)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_pm25)/real(nchem))

   else if (present(ichem).and.ichem == PARAM_FIRST_SCALAR+1) then
   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', str_pm1
   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem
   if (nchem < 1) nchem = 1
   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_pm10), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_pm10)
   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_pm10)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_pm10)/real(nchem))                                                                                  

   else if (present(ichem).and.ichem == PARAM_FIRST_SCALAR+2) then
   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', str_so2
   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem
   if (nchem < 1) nchem = 1
   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_so2), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_so2)
   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_so2)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_so2)/real(nchem))

   else if (present(ichem).and.ichem == PARAM_FIRST_SCALAR+3) then
   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', str_no2
   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem
   if (nchem < 1) nchem = 1
   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_no2), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_no2)
   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_no2)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_no2)/real(nchem))

   else if (present(ichem).and.ichem == PARAM_FIRST_SCALAR+4) then
   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', str_o3
   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem
   if (nchem < 1) nchem = 1
   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_o3), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_o3)
   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_o3)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_o3)/real(nchem))

   else if (present(ichem).and.ichem == PARAM_FIRST_SCALAR+5) then
   write(unit=stats_unit, fmt='(2a/)') &
      '   var             ', str_co
   write(unit=stats_unit, fmt='(a,i16)') &
      '  Number: ', nchem
   if (nchem < 1) nchem = 1
   write(unit=stats_unit, fmt='((a,f12.4,2i10))') &
     ' Minimum(n,k): ', stats%minimum%chem(p_chemsi_co), & 
     ' Maximum(n,k): ', stats%maximum%chem(p_chemsi_co)
   write(unit=stats_unit, fmt='((a,(f12.4,10x)))') &
      ' Average     : ', stats%average%chem(p_chemsi_co)/real(nchem), &
      '    RMSE     : ', sqrt(stats%rms_err%chem(p_chemsi_co)/real(nchem))

   end if
   end if

   if (trace_use_dull) call da_trace_exit("da_print_stats_chem_sfc")

end subroutine da_print_stats_chem_sfc


