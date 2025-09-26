subroutine da_ao_stats_gpseph (stats_unit, iv, re)

   !--------------------------------------------------------------------
   ! Purpose: calculate and write out gpseph O-A statistics
   !--------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: stats_unit    ! Output unit for stats.
   type (iv_type), intent(inout) :: iv            ! iv
   type  (y_type), intent(in)    :: re            ! A - O

   type (stats_gpseph_type)         :: stats
   integer                          :: ngpseph
   integer                          :: n, k
   real                             :: o_minus_b, o_minus_a, sigma_o, sigma_b
   real                             :: o_minus_b_0, o_minus_a_0, sigma_o_0, sigma_b_0

   if (trace_use_dull) call da_trace_entry("da_ao_stats_gpseph")

   ngpseph = 0
   o_minus_b_0 = 0.0; o_minus_a_0 = 0.0; sigma_o_0 = 0.0; sigma_b_0 = 0.0

   stats%maximum%eph = maxmin_type (missing_r, 0, 0)
   stats%minimum%eph = maxmin_type(-missing_r, 0, 0)

   stats%average = residual_gpseph1_type(0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(gpseph)%nlocal
      if (iv%info(gpseph)%proc_domain(1,n)) then
         do k=1, iv%info(gpseph)%levels(n)
            call da_stats_calculate (n, k, iv%gpseph(n)%eph(k)%qc, &
               re%gpseph(n)%eph(k), ngpseph, &
               stats%minimum%eph, stats%maximum%eph, &
               stats%average%eph, stats%rms_err%eph)

            if (pseudo_var(1:3) == 'eph' .and. num_pseudo > 0) then
               o_minus_b = iv%GPSEph(n)%eph(k)%inv
               o_minus_a = re%gpseph(n)%eph(k)
               sigma_o   = iv%gpseph(n)%eph(k)%error
            end if
         end do

         if (pseudo_var(1:3) == 'eph' .and. num_pseudo > 0) then
            ! Calculate equivalent sigma_b using
            ! O-A=(O-B)*sigma_o/(sigma_o+sigma_b)

            sigma_b = sqrt ((o_minus_b - o_minus_a) &
               / o_minus_a) * sigma_o
            o_minus_b_0 = wrf_dm_sum_real (o_minus_b)
            o_minus_a_0 = wrf_dm_sum_real (o_minus_a)
            sigma_o_0 = wrf_dm_sum_real (sigma_o)
            sigma_b_0 = wrf_dm_sum_real (sigma_b)
            write (unit=stdout,fmt='(A,F10.2)') &
               'TEST_COVERAGE_da_ao_stats_gpseph:  o_minus_b_0 = ', o_minus_b_0
            write (unit=stdout,fmt='(A,F10.2)') &
               'TEST_COVERAGE_da_ao_stats_gpseph:  o_minus_a_0 = ', o_minus_a_0
            write (unit=stdout,fmt='(A,F10.2)') &
               'TEST_COVERAGE_da_ao_stats_gpseph:  sigma_o_0 = ', sigma_o_0
            write (unit=stdout,fmt='(A,F10.2)') &
               'TEST_COVERAGE_da_ao_stats_gpseph:  sigma_b_0 = ', sigma_b_0
            if (rootproc) then
               write(stats_unit,'(/A,A3,A,f12.3)')  &
                  ' Pseudo ', pseudo_var, ' O-B: ', o_minus_b_0
               write(stats_unit,' (A,A3,A,f12.3)')  &
                  ' Pseudo ', pseudo_var, ' O-A: ', o_minus_a_0
               write(stats_unit,' (A,A3,A,f12.3)')  &
                  ' Pseudo ', pseudo_var, ' sigma_o: ', sigma_o_0
               write(stats_unit,'(A,A3,A,f12.3)')   &
                  ' Pseudo ', pseudo_var, ' sigma_b: ', sigma_b_0
            end if
         end if
      end if    ! end if (iv%info(gpseph)%proc_domain(1,n))
   end do

   ! Do inter-processor communication to gather statistics.

   call da_proc_sum_int (ngpseph)
   iv%nstats(gpseph) = ngpseph

   call da_proc_stats_combine(stats%average%eph, stats%rms_err%eph, &
      stats%minimum%eph%value, stats%maximum%eph%value, &
      stats%minimum%eph%n, stats%maximum%eph%n, &
      stats%minimum%eph%l, stats%maximum%eph%l)

   if (rootproc) then
      if (ngpseph > 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for gpseph'
            call da_print_stats_gpseph(stats_unit, ngpseph, stats)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_ao_stats_gpseph")

end subroutine da_ao_stats_gpseph
