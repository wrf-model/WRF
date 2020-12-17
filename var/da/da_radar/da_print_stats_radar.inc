subroutine da_print_stats_radar(stats_unit, nrv, nrf, nrrn, nrsn, nrgr, nrqv, radar)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit
   integer,                 intent(inout) :: nrv, nrf, nrrn, nrsn, nrgr,nrqv
   type (stats_radar_type), intent(in)    :: radar

   if (trace_use) call da_trace_entry("da_print_stats_radar")
   
   write(unit=stats_unit, fmt='(a/)') &
      '   var             rv (m/s)        n    k         rf (dBZ)        n    k        rrn(kg/kg)       n    k        rsn(kg/kg)       n    k        rgr(kg/kg)       n    k        rqv(kg/kg)       n    k'

   write(unit=stats_unit, fmt='(a,i16,5(i31))') &
      '  Number: ', nrv, nrf, nrrn, nrsn, nrgr, nrqv

   if (nrv < 1) nrv = 1
   if (nrf < 1) nrf = 1
   if (nrrn < 1) nrrn = 1
   if (nrsn < 1) nrsn = 1
   if (nrgr < 1) nrgr = 1
   if (nrqv < 1) nrqv = 1
   
   write(unit=stats_unit, fmt='((a,f12.4,i9,i5, 5(f17.4,i9,i5)))')  &
      ' Minimum(n,k): ', radar%minimum%rv, radar%minimum%rf, radar%minimum%rrn, radar%minimum%rsn,radar%minimum%rgr, radar%minimum%rqv
   write(unit=stats_unit, fmt='((a,f12.4,i9,i5, 5(f17.4,i9,i5)))') &
      ' Maximum(n,k): ', radar%maximum%rv, radar%maximum%rf, radar%maximum%rrn, radar%maximum%rsn,radar%maximum%rgr, radar%maximum%rqv
   write(unit=stats_unit, fmt='((a,6(f12.4,19x)))')              &
      ' Average     : ', radar%average%rv/real(nrv), radar%average%rf/real(nrf),radar%average%rrn/real(nrrn), radar%average%rsn/real(nrsn),radar%average%rgr/real(nrgr), radar%average%rqv/real(nrqv) 
   write(unit=stats_unit, fmt='((a,6(f12.4,19x)))')              &
      '    RMSE     : ', sqrt(radar%rms_err%rv/real(nrv)), sqrt(radar%rms_err%rf/real(nrf)), &
                         sqrt(radar%rms_err%rrn/real(nrrn)),sqrt(radar%rms_err%rsn/real(nrsn)),sqrt(radar%rms_err%rgr/real(nrgr)),sqrt(radar%rms_err%rqv/real(nrqv))

   if (trace_use) call da_trace_exit("da_print_stats_radar")

end subroutine da_print_stats_radar


