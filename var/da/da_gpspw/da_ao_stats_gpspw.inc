subroutine da_ao_stats_gpspw (stats_unit, iv, re)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: stats_unit    ! Output unit for stats.
   type (iv_type), intent(inout) :: iv            ! iv
   type (y_type),  intent(in)    :: re            ! A - O

   type (stats_gpspw_type) :: stats
   integer                 :: ntpw
   integer                 :: n
   real                    :: o_minus_b, o_minus_a, sigma_o, sigma_b
   real                    :: o_minus_b_0, o_minus_a_0, sigma_o_0, sigma_b_0 

   if (trace_use_dull) call da_trace_entry("da_ao_stats_gpspw")

   ntpw = 0
   o_minus_b = 0.0; o_minus_a = 0.0; sigma_o = 0.0; sigma_b = 0.0

   stats%maximum%tpw = maxmin_type (missing_r, 0, 0)
   stats%minimum%tpw = maxmin_type(-missing_r, 0, 0)

   stats%average = residual_gpspw1_type(0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(gpspw)%nlocal
      if (iv%info(gpspw)%proc_domain(1,n)) then
         call da_stats_calculate (n, 0, iv%gpspw(n)%tpw%qc, &
            re%gpspw(n)%tpw, ntpw,   &
            stats%minimum%tpw,  stats%maximum%tpw, &
            stats%average%tpw,  stats%rms_err%tpw)

         if ( pseudo_tpw .or. pseudo_ztd ) then
            o_minus_b = iv%GPSpw(n)%tpw%inv
            o_minus_a = re%gpspw(n)%tpw
            sigma_o   = iv%gpspw(n)%tpw%error

            ! Calculate equivalent sigma_b using
            ! O-A=(O-B)*sigma_o/(sigma_o+sigma_b)
            sigma_b = sqrt ((o_minus_b - o_minus_a) / o_minus_a) * sigma_o
         end if
      end if    ! end if (iv%info(gpspw)%proc_domain(1,n))
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (ntpw)
   iv%nstats(gpspw) = ntpw

   call da_proc_stats_combine(stats%average%tpw, stats%rms_err%tpw, &
      stats%minimum%tpw%value, stats%maximum%tpw%value, &
      stats%minimum%tpw%n, stats%maximum%tpw%n, &
      stats%minimum%tpw%l, stats%maximum%tpw%l)

   if ( pseudo_tpw .or. pseudo_ztd ) then
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
      if (ntpw /= 0) then
         if (use_gpspwObs) then
           write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for gpspw'
         else if (use_gpsztdObs) then
           write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for gpsztd'
         endif

         call da_print_stats_gpspw(stats_unit, ntpw, stats)
       end if
   end if

   if (trace_use_dull) call da_trace_exit("da_ao_stats_gpspw")

end subroutine da_ao_stats_gpspw


