subroutine da_oi_stats_synop (stats_unit, iv, ob)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in) :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (in) :: iv            ! OI
   type(y_type),   intent (in) :: ob            ! Observation structure.

   type (stats_synop_type) :: stats
   integer                 :: nu, nv, nt, np, nq
   integer                 :: n
   real                    :: u_inv, v_inv, u_obs, v_obs

   if (trace_use_dull) call da_trace_entry("da_oi_stats_synop")

   nu = 0
   nv = 0
   nt = 0
   np = 0
   nq = 0

   stats%maximum%u = maxmin_type(missing_r, 0, 0)
   stats%maximum%v = maxmin_type(missing_r, 0, 0)
   stats%maximum%t = maxmin_type(missing_r, 0, 0)
   stats%maximum%p = maxmin_type(missing_r, 0, 0)
   stats%maximum%q = maxmin_type(missing_r, 0, 0)
   stats%minimum%u = maxmin_type(-missing_r, 0, 0)
   stats%minimum%v = maxmin_type(-missing_r, 0, 0)
   stats%minimum%t = maxmin_type(-missing_r, 0, 0)
   stats%minimum%p = maxmin_type(-missing_r, 0, 0)
   stats%minimum%q = maxmin_type(-missing_r, 0, 0)

   stats%average = residual_synop1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(synop)%nlocal
      if (iv%info(synop)%proc_domain(1,n)) then

        u_inv = iv%synop(n)%u%inv
        v_inv = iv%synop(n)%v%inv
        u_obs = ob%synop(n)%u
        v_obs = ob%synop(n)%v

        if (.not. wind_sd_synop .and. wind_stats_sd) &
           call da_ffdduv_diagnose(u_obs, u_inv, u_obs, v_obs, v_inv, v_obs, &
                                   iv%synop(n)%u%qc, iv%synop(n)%v%qc, convert_uv2fd)
        if (wind_sd_synop .and. .not. wind_stats_sd) &
           call da_ffdduv_diagnose(u_obs, u_inv, u_obs, v_obs, v_inv, v_obs, &
                                   iv%synop(n)%u%qc, iv%synop(n)%v%qc, convert_fd2uv)

        call da_stats_calculate(iv%info(synop)%obs_global_index(n), &
           0, iv%synop(n)%u%qc, &
           u_inv, nu, &
           stats%minimum%u, stats%maximum%u, &
           stats%average%u, stats%rms_err%u)
        call da_stats_calculate(iv%info(synop)%obs_global_index(n), &
           0, iv%synop(n)%v%qc, &
           v_inv, nv, &
           stats%minimum%v, stats%maximum%v, &
           stats%average%v, stats%rms_err%v)
        call da_stats_calculate(iv%info(synop)%obs_global_index(n), &
           0, iv%synop(n)%t%qc, &
           iv%synop(n)%t%inv, nt, &
           stats%minimum%t, stats%maximum%t, &
           stats%average%t, stats%rms_err%t)
        call da_stats_calculate(iv%info(synop)%obs_global_index(n), &
           0, iv%synop(n)%p%qc, &
           iv%synop(n)%p%inv, np, &
           stats%minimum%p, stats%maximum%p, &
           stats%average%p, stats%rms_err%p)
        call da_stats_calculate(iv%info(synop)%obs_global_index(n), &
            0, iv%synop(n)%q%qc, &
            iv%synop(n)%q%inv, nq, &
            stats%minimum%q, stats%maximum%q, &
            stats%average%q, stats%rms_err%q)
      end if
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(nu)
   call da_proc_sum_int(nv)
   call da_proc_sum_int(nt)
   call da_proc_sum_int(np)
   call da_proc_sum_int(nq)

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
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for synop'
         call da_print_stats_synop(stats_unit, nu, nv, nt, np, nq, stats)

         ! min_val = -50.0
         ! max_val = 50.0
         ! bin_width = 1.0
         ! call da_data_distribution('synop u ', iv%total(synop), min_val, max_val, &
         !                            bin_width, iv%synop(:)%u%inv)
         ! call da_data_distribution('synop v ', iv%total(synop), min_val, max_val, &
         !                            bin_width, iv%synop(:)%v%inv)
         ! call da_data_distribution('synop t ', iv%total(synop), min_val, max_val, &
         !                            bin_width, iv%synop(:)%t%inv)
         ! call da_data_distribution('synop p ', iv%total(synop), -1000.0, 1000.0, &
         !                            50.0, iv%synop(:)%p%inv)
         ! call da_data_distribution('synop q ', iv%total(synop), -0.03, 0.03, &
         !                               0.001, iv%synop(:)%q%inv)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_oi_stats_synop")

end subroutine da_oi_stats_synop


