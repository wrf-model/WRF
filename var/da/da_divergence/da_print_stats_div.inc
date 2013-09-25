subroutine da_print_stats_div(stats_unit, ndiv, pseudo)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                  intent(in)    :: stats_unit
   integer,                  intent(inout) :: ndiv
   type (stats_pseudo_type), intent(in)    :: pseudo

   if (trace_use_dull) call da_trace_entry("da_print_stats_div")


   write(unit=stats_unit, fmt='(2a/)') &
        '   var             ', &
        'div (1/s)      n    k'
! #endif
   write(unit=stats_unit, fmt='(a,i22)') &
        '  Number: ', ndiv

!    if (nu < 1) nu = 1
!    if (nv < 1) nv = 1
!    if (nt < 1) nt = 1
!    if (np < 1) np = 1
!    if (nq < 1) nq = 1
   if (ndiv < 1) ndiv = 1
!    write(unit=stats_unit, fmt='((a,4(f12.4,2i5),e12.4,2i5,e12.4,2i5))') &
!         ' Minimum(n,k): ', pseudo%minimum%u, pseudo%minimum%v, pseudo%minimum%t, &
!                       pseudo%minimum%p, pseudo%minimum%q, pseudo%minimum%div, &
!         ' Maximum(n,k): ', pseudo%maximum%u, pseudo%maximum%v, pseudo%maximum%t, &
!                       pseudo%maximum%p, pseudo%maximum%q, pseudo%maximum%div
!    write(unit=stats_unit, fmt='((a,4(f12.4,10x),e12.4,10x,e12.4,10x))') &
!         ' Average     : ', pseudo%average%u/real(nu), pseudo%average%v/real(nv), &
!                       pseudo%average%t/real(nt), pseudo%average%p/real(np), &
!                       pseudo%average%q/real(nq), pseudo%average%div/real(ndiv),&
!         '    RMSE     : ', sqrt(pseudo%rms_err%u/real(nu)), &
!                       sqrt(pseudo%rms_err%v/real(nv)), &
!                       sqrt(pseudo%rms_err%t/real(nt)), &
!                       sqrt(pseudo%rms_err%p/real(np)), &
!                       sqrt(pseudo%rms_err%q/real(nq)), sqrt(pseudo%rms_err%div/real(ndiv))

   write(unit=stats_unit, fmt='((a,e12.4,2i5))') &
        ' Minimum(n,k): ',  pseudo%minimum%div, &
        ' Maximum(n,k): ',  pseudo%maximum%div
   write(unit=stats_unit, fmt='((a,e12.4,10x))') &
        ' Average     : ',  pseudo%average%div/real(ndiv),&
        '    RMSE     : ',  sqrt(pseudo%rms_err%div/real(ndiv))

   if (trace_use_dull) call da_trace_exit("da_print_stats_div")

end subroutine da_print_stats_div


