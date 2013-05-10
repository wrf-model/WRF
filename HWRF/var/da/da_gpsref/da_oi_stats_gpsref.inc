subroutine da_oi_stats_gpsref (stats_unit, iv)

   ! -------------------------------------------------------------------
   ! Purpose: TBD
   ! -------------------------------------------------------------------

   implicit none

   integer,        intent (in) :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (in) :: iv            ! OI

   type (stats_gpsref_type) :: stats
   integer                  :: ngpsref
   integer                  :: n, k

   if (trace_use_dull) call da_trace_entry("da_oi_stats_gpsref")

   ngpsref = 0
   
   stats%maximum%ref = maxmin_type(missing_r, 0, 0)
   stats%minimum%ref = maxmin_type(-missing_r, 0, 0)

   stats%average = residual_gpsref1_type(0.0,0.0,0.0,0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(gpsref)%nlocal
      if (iv%info(gpsref)%proc_domain(1,n)) then
         do k=1, iv%info(gpsref)%levels(n)
            call da_stats_calculate(iv%info(gpsref)%obs_global_index(n), &
               k, iv%gpsref(n)%ref(k)%qc, &
               iv%gpsref(n)%ref(k)%inv, ngpsref, &
               stats%minimum%ref, stats%maximum%ref, &
               stats%average%ref, stats%rms_err%ref)
         end do
      end if
   end do

   ! do inter-processor communication to gather statistics.

   call da_proc_sum_int(ngpsref)
   
   call da_proc_stats_combine(stats%average%ref, stats%rms_err%ref, &
       stats%minimum%ref%value, stats%maximum%ref%value, &
       stats%minimum%ref%n, stats%maximum%ref%n, &
       stats%minimum%ref%l, stats%maximum%ref%l)
   
   if (rootproc .and. (ngpsref > 0)) then
      write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for gpsref'
         call da_print_stats_gpsref(stats_unit, ngpsref, stats)
   end if

   if (trace_use_dull) call da_trace_exit("da_oi_stats_gpsref")

end subroutine da_oi_stats_gpsref


