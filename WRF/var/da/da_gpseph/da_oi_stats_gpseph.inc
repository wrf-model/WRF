subroutine da_oi_stats_gpseph (stats_unit, iv)

   ! -------------------------------------------------------------------
   ! Purpose: calculate and write out gpseph O-B statistics
   ! -------------------------------------------------------------------

   implicit none

   integer,        intent (in) :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (in) :: iv            ! OI

   type (stats_gpseph_type) :: stats
   integer                  :: ngpseph
   integer                  :: n, k

   if (trace_use_dull) call da_trace_entry("da_oi_stats_gpseph")

   ngpseph = 0

   stats%maximum%eph = maxmin_type(missing_r, 0, 0)
   stats%minimum%eph = maxmin_type(-missing_r, 0, 0)

   stats%average = residual_gpseph1_type(0.0)
   stats%rms_err = stats%average
   do n=1, iv%info(gpseph)%nlocal
      if (iv%info(gpseph)%proc_domain(1,n)) then
        do k=1, iv%info(gpseph)%levels(n)
            call da_stats_calculate(iv%info(gpseph)%obs_global_index(n), &
               k, iv%gpseph(n)%eph(k)%qc, &
               iv%gpseph(n)%eph(k)%inv, ngpseph, &
               stats%minimum%eph, stats%maximum%eph, &
               stats%average%eph, stats%rms_err%eph)

         end do
      end if
   end do

   ! do inter-processor communication to gather statistics.

   call da_proc_sum_int(ngpseph)

   call da_proc_stats_combine(stats%average%eph, stats%rms_err%eph, &
       stats%minimum%eph%value, stats%maximum%eph%value, &
       stats%minimum%eph%n, stats%maximum%eph%n, &
       stats%minimum%eph%l, stats%maximum%eph%l)

   if (rootproc .and. (ngpseph > 0)) then
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for gpseph'
         call da_print_stats_gpseph(stats_unit, ngpseph, stats)
   end if

   if (trace_use_dull) call da_trace_exit("da_oi_stats_gpseph")

end subroutine da_oi_stats_gpseph
