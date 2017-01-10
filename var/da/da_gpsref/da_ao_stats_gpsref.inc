subroutine da_ao_stats_gpsref (stats_unit, iv, re)

   !--------------------------------------------------------------------
   ! Purpose: Called by da_minimisation/da_write_diagnostics.inc.
   !--------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: stats_unit    ! Output unit for stats.
   type (iv_type), intent(inout) :: iv            ! iv
   type  (y_type), intent(in)    :: re            ! A - O

   type (stats_gpsref_type)         :: stats
   integer                          :: ngpsref
   integer                          :: n, k
   real                             :: o_minus_b, o_minus_a, sigma_o, sigma_b
   real                             :: o_minus_b_0, o_minus_a_0, sigma_o_0, sigma_b_0

   if (trace_use_dull) call da_trace_entry("da_ao_stats_gpsref")

   ngpsref = 0
   o_minus_b = 0.0; o_minus_a = 0.0; sigma_o = 0.0; sigma_b = 0.0

   stats%maximum%ref = maxmin_type (missing_r, 0, 0)
   stats%minimum%ref = maxmin_type(-missing_r, 0, 0)

   stats%average = residual_gpsref1_type(0.0,0.0,0.0,0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(gpsref)%nlocal
      if (iv%info(gpsref)%proc_domain(1,n)) then
         do k=1, iv%info(gpsref)%levels(n)
            call da_stats_calculate (n, k, iv%gpsref(n)%ref(k)%qc, & 
               re%gpsref(n)%ref(k), ngpsref, &
               stats%minimum%ref, stats%maximum%ref, &
               stats%average%ref, stats%rms_err%ref)

            if ( pseudo_ref ) then
               o_minus_b = iv%GPSRef(n)%ref(k)%inv
               o_minus_a = re%gpsref(n)%ref(k)
               sigma_o   = iv%gpsref(n)%ref(k)%error
               ! Calculate equivalent sigma_b using
               ! O-A=(O-B)*sigma_o/(sigma_o+sigma_b)
               sigma_b = sqrt ((o_minus_b - o_minus_a) &
                         / o_minus_a) * sigma_o
            end if
         end do

      end if    ! end if (iv%info(gpsref)%proc_domain(1,n))
   end do

   ! Do inter-processor communication to gather statistics.

   call da_proc_sum_int (ngpsref)
   iv%nstats(gpsref) = ngpsref
    
   call da_proc_stats_combine(stats%average%ref, stats%rms_err%ref, &
      stats%minimum%ref%value, stats%maximum%ref%value, &
      stats%minimum%ref%n, stats%maximum%ref%n, &
      stats%minimum%ref%l, stats%maximum%ref%l)

   if ( pseudo_ref ) then
      o_minus_b_0 = wrf_dm_sum_real (o_minus_b)
      o_minus_a_0 = wrf_dm_sum_real (o_minus_a)
      sigma_o_0 = wrf_dm_sum_real (sigma_o)
      sigma_b_0 = wrf_dm_sum_real (sigma_b)
      if (rootproc) then
         write(stats_unit,'(/A,A3,A,f12.3)')  &
            ' Pseudo ', pseudo_var, '     O-B: ', o_minus_b_0
         write(stats_unit,' (A,A3,A,f12.3)')  &
            ' Pseudo ', pseudo_var, '     O-A: ', o_minus_a_0 
         write(stats_unit,' (A,A3,A,f12.3)')  &
            ' Pseudo ', pseudo_var, ' sigma_o: ', sigma_o_0
         write(stats_unit,'(A,A3,A,f12.3)')  &
            ' Pseudo ', pseudo_var, ' sigma_b: ', sigma_b_0
      end if
   end if

   if (rootproc) then
      if (ngpsref > 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for gpsref'
            call da_print_stats_gpsref(stats_unit, ngpsref, stats)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_ao_stats_gpsref")

end subroutine da_ao_stats_gpsref


