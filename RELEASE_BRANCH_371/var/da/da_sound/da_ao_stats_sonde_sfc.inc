subroutine da_ao_stats_sonde_sfc (stats_unit, iv, re, ob)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)    :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (inout) :: iv            ! iv
   type  (y_type), intent (in)    :: re            ! A - O
   type(y_type),   intent (in)    :: ob            ! Observation structure.

   type (stats_sonde_sfc_type)      :: stats
   integer                          :: nu, nv, nt, np, nq
   integer                          :: n
   real                             :: u_inc, v_inc, u_obs, v_obs

   if (trace_use_dull) call da_trace_entry("da_ao_stats_sonde_sfc")

   nu = 0
   nv = 0
   nt = 0
   np = 0
   nq = 0

   stats%maximum%u = maxmin_type (missing_r, 0, 0)
   stats%maximum%v = maxmin_type (missing_r, 0, 0)
   stats%maximum%t = maxmin_type (missing_r, 0, 0)
   stats%maximum%p = maxmin_type (missing_r, 0, 0)
   stats%maximum%q = maxmin_type (missing_r, 0, 0)
   stats%minimum%u = maxmin_type(-missing_r, 0, 0)
   stats%minimum%v = maxmin_type(-missing_r, 0, 0)
   stats%minimum%t = maxmin_type(-missing_r, 0, 0)
   stats%minimum%p = maxmin_type(-missing_r, 0, 0)
   stats%minimum%q = maxmin_type(-missing_r, 0, 0)
   stats%average = residual_sonde_sfc1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(sound)%nlocal
      if (iv%info(sonde_sfc)%proc_domain(1,n)) then

         u_inc = re%sonde_sfc(n)%u
         v_inc = re%sonde_sfc(n)%v
         u_obs = ob%sonde_sfc(n)%u
         v_obs = ob%sonde_sfc(n)%v

         if (.not. wind_sd_sound .and. wind_stats_sd) &
            call da_ffdduv_diagnose(u_obs, u_obs, u_inc, v_obs, v_obs, v_inc, &
                                    iv%sonde_sfc(n)%u%qc, iv%sonde_sfc(n)%v%qc, convert_uv2fd)
         if (wind_sd_sound .and. .not. wind_stats_sd) &
            call da_ffdduv_diagnose(u_obs, u_obs, u_inc, v_obs, v_obs, v_inc, &
                                    iv%sonde_sfc(n)%u%qc, iv%sonde_sfc(n)%v%qc, convert_fd2uv)

         call da_stats_calculate (n, 0, iv%sonde_sfc(n)%u%qc,  & 
            u_inc, nu, & 
            stats%minimum%u, stats%maximum%u, &
            stats%average%u, stats%rms_err%u)
         call da_stats_calculate (n, 0, iv%sonde_sfc(n)%v%qc,  & 
            v_inc, nv, & 
            stats%minimum%v, stats%maximum%v, &
            stats%average%v, stats%rms_err%v)
         call da_stats_calculate (n, 0, iv%sonde_sfc(n)%t%qc,  & 
            re%sonde_sfc(n)%t, nt, & 
            stats%minimum%t, stats%maximum%t, &
            stats%average%t, stats%rms_err%t)  
         call da_stats_calculate (n, 0, iv%sonde_sfc(n)%p%qc,  & 
            re%sonde_sfc(n)%p, np, & 
            stats%minimum%p, stats%maximum%p, &
            stats%average%p, stats%rms_err%p)
         call da_stats_calculate (n, 0, iv%sonde_sfc(n)%q%qc,  & 
            re%sonde_sfc(n)%q, nq, & 
            stats%minimum%q, stats%maximum%q, &
            stats%average%q, stats%rms_err%q)
      end if
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nu)
   call da_proc_sum_int (nv)
   call da_proc_sum_int (nt)
   call da_proc_sum_int (np)
   call da_proc_sum_int (nq)
   iv%nstats(sonde_sfc) = nu + nv + nt + np + nq

   call da_proc_stats_combine(stats%average%u, stats%rms_err%u, &
      stats%minimum%u%value, stats%maximum%u%value, &
      stats%minimum%u%n, stats%maximum%u%n, &
      stats%minimum%u%l, stats%maximum%u%l)
   call da_proc_stats_combine(stats%average%v, stats%rms_err%v, &
      stats%minimum%v%value, stats%maximum%v%value, &
      stats%minimum%v%n, stats%maximum%v%n, &
      stats%minimum%v%l, stats%maximum%v%l)
   call da_proc_stats_combine(stats%average%t, stats%rms_err%t, &
      stats%minimum%t%value, stats%maximum%t%value, &
      stats%minimum%t%n, stats%maximum%t%n, &
      stats%minimum%t%l, stats%maximum%t%l)
   call da_proc_stats_combine(stats%average%p, stats%rms_err%p, &
      stats%minimum%p%value, stats%maximum%p%value, &
      stats%minimum%p%n, stats%maximum%p%n, &
      stats%minimum%p%l, stats%maximum%p%l)
   call da_proc_stats_combine(stats%average%q, stats%rms_err%q, &
      stats%minimum%q%value, stats%maximum%q%value, &
      stats%minimum%q%n, stats%maximum%q%n, &
      stats%minimum%q%l, stats%maximum%q%l)

   if (rootproc) then
      if (nu /= 0 .or. nv /= 0 .or. nt /= 0 .or. np /= 0 .or. nq /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for sonde_sfc'
         call da_print_stats_sonde_sfc(stats_unit, nu, nv, nt, np, nq, stats)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_ao_stats_sonde_sfc")

end subroutine da_ao_stats_sonde_sfc


