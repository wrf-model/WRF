subroutine da_ao_stats_geoamv (stats_unit, iv, re)

   !-------------------------------------------------------------------------
   ! Purpose: Calculates (Obs - Analysis) statistics for Geo.  AMV's
   !
   !-------------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: stats_unit    ! Output unit for stats.
   type (iv_type), intent(inout) :: iv            ! iv
   type (y_type),  intent(in)    :: re            ! A - O

   type (stats_geoamv_type) :: stats
   integer                  :: nu, nv
   integer                  :: n, k

   if (trace_use_dull) call da_trace_entry("da_ao_stats_geoamv")

   nu = 0
   nv = 0

   stats%maximum%u = maxmin_type (missing_r, 0, 0)
   stats%maximum%v = maxmin_type (missing_r, 0, 0)
   stats%minimum%u = maxmin_type(-missing_r, 0, 0)
   stats%minimum%v = maxmin_type(-missing_r, 0, 0)

   stats%average = residual_geoamv1_type (0.0, 0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(geoamv)%nlocal
      if (iv%info(geoamv)%proc_domain(1,n)) then
         do k=1, iv%info(geoamv)%levels(n)
            call da_stats_calculate (n, 0, iv%geoamv(n)%u(k)%qc, & 
               re%geoamv(n)%u(k), nu, & 
               stats%minimum%u, stats%maximum%u, &
               stats%average%u, stats%rms_err%u)
            call da_stats_calculate (n, 0, iv%geoamv(n)%v(k)%qc, & 
               re%geoamv(n)%v(k), nv, & 
               stats%minimum%v, stats%maximum%v, &
               stats%average%v, stats%rms_err%v)
         end do
      end if
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nu)
   call da_proc_sum_int (nv)
   iv%nstats(geoamv) = nu + nv

   call da_proc_stats_combine(stats%average%u, stats%rms_err%u, &
      stats%minimum%u%value, stats%maximum%u%value, &
      stats%minimum%u%n, stats%maximum%u%n, &
      stats%minimum%u%l, stats%maximum%u%l)
   call da_proc_stats_combine(stats%average%v, stats%rms_err%v, &
      stats%minimum%v%value, stats%maximum%v%value, &
      stats%minimum%v%n, stats%maximum%v%n, &
      stats%minimum%v%l, stats%maximum%v%l)

   if (rootproc) then
      if (nu /= 0 .or. nv /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for geoamv'
         call da_print_stats_geoamv (stats_unit, nu, nv, stats)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_ao_stats_geoamv")

end subroutine da_ao_stats_geoamv  


