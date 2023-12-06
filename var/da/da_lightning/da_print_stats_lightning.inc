subroutine da_print_stats_lightning(stats_unit, nw, ndiv, nqv, lightning)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP)   
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in)    :: stats_unit  
   integer,                 intent(inout) :: nw, ndiv, nqv      
   type (stats_lightning_type), intent(in):: lightning

   if (trace_use_dull) call da_trace_entry("da_print_stats_lightning")

   write(unit=stats_unit, fmt='(a/)') &
      '   var              w (m/s)        n    k        div (1/s)        n    k        qv (kg/kg)       n    k'

   write(unit=stats_unit, fmt='(a,(i16,2i31))') &
      '  Number: ', nw, ndiv, nqv

   if (nw < 1)   nw = 1
   if (ndiv < 1) ndiv = 1
   if (nqv < 1)  nqv = 1

   write(unit=stats_unit, fmt='((a,f12.4,i9,i5, 2(f17.4,i9,i5)))')  &
        ' Minimum(n,k): ', lightning%minimum%w, lightning%minimum%div, lightning%minimum%qv
   write(unit=stats_unit, fmt='((a,f12.4,i9,i5, 2(f17.4,i9,i5)))')  &
        ' Maximum(n,k): ', lightning%maximum%w, lightning%maximum%div, lightning%maximum%qv
   write(unit=stats_unit, fmt='((a,3(f12.4,19x)))') &
        ' Average     : ', lightning%average%w/real(nw), &
                           lightning%average%div/real(ndiv), &
                           lightning%average%qv/real(nqv)
   write(unit=stats_unit, fmt='((a,3(f12.4,19x)))') &
        '    RMSE     : ', sqrt(lightning%rms_err%w/real(nw)), &
                           sqrt(lightning%rms_err%div/real(ndiv)), &
                           sqrt(lightning%rms_err%qv/real(nqv))

   if (trace_use_dull) call da_trace_exit("da_print_stats_lightning")
   
end subroutine da_print_stats_lightning
