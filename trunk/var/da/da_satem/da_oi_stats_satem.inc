subroutine da_oi_stats_satem (stats_unit, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in) :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (in) :: iv            ! OI

   type (stats_satem_type) :: stats
   integer                 :: nthickness
   integer                 :: n, k

   if (trace_use_dull) call da_trace_entry("da_oi_stats_satem")

   nthickness = 0

   stats%maximum%thickness = maxmin_type(missing_r, 0, 0)
   stats%minimum%thickness = maxmin_type(-missing_r, 0, 0)

   stats%average = residual_satem1_type(0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(satem)%nlocal
      if (iv%info(satem)%proc_domain(1,n)) then
         do k=1, iv%info(satem)%levels(n)
            call da_stats_calculate(iv%info(satem)%obs_global_index(n), &
               k, iv%satem(n)%thickness(k)%qc, &
               iv%satem(n)%thickness(k)%inv, nthickness, &
               stats%minimum%thickness, stats%maximum%thickness, &
               stats%average%thickness, stats%rms_err%thickness)
         end do
      end if
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(nthickness)

   call da_proc_stats_combine(stats%average%thickness, stats%rms_err%thickness, &
      stats%minimum%thickness%value, stats%maximum%thickness%value, &
      stats%minimum%thickness%n, stats%maximum%thickness%n, &
      stats%minimum%thickness%l, stats%maximum%thickness%l)

   if (rootproc) then
      if (nthickness /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for satem'
         call da_print_stats_satem(stats_unit, nthickness, stats)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_oi_stats_satem")

end subroutine da_oi_stats_satem


