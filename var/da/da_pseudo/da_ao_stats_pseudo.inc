subroutine da_ao_stats_pseudo  (stats_unit, iv, re)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: stats_unit    ! Output unit for stats.
   type (iv_type), intent(inout) :: iv            ! O-B
   type  (y_type), intent(in)    :: re            ! O-A

   type (stats_pseudo_type) :: stats
   integer                  :: nu, nv, nt, np, nq
   integer                  :: n
   real                     :: o_minus_b, o_minus_a, sigma_o, sigma_b
   
   if (trace_use_dull) call da_trace_entry("da_ao_stats_pseudo")

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

   stats%average = residual_pseudo1_type(0.0, 0.0, 0.0, 0.0, 0.0)
   stats%rms_err = stats%average

   do n=1, iv%info(pseudo)%nlocal
      if (iv%info(pseudo)%proc_domain(1,n)) then
         call da_stats_calculate (n, 0, iv%pseudo(n)%u%qc, & 
            re%pseudo(n)%u, nu, & 
            stats%minimum%u, stats%maximum%u, &
            stats%average%u, stats%rms_err%u)
         call da_stats_calculate (n, 0, iv%pseudo(n)%v%qc, & 
            re%pseudo(n)%v, nv, & 
            stats%minimum%v, stats%maximum%v, &
            stats%average%v, stats%rms_err%v)
         call da_stats_calculate (n, 0, iv%pseudo(n)%t%qc, & 
            re%pseudo(n)%t, nt, & 
            stats%minimum%t, stats%maximum%t, &
            stats%average%t, stats%rms_err%t)
         call da_stats_calculate (n, 0, iv%pseudo(n)%p%qc, & 
            re%pseudo(n)%p, np, & 
            stats%minimum%p, stats%maximum%p, &
            stats%average%p, stats%rms_err%p)
         call da_stats_calculate (n, 0, iv%pseudo(n)%q%qc, & 
            re%pseudo(n)%q, nq, & 
            stats%minimum%q, stats%maximum%q, &
            stats%average%q, stats%rms_err%q)

         if (nu > 0) then
            o_minus_b = iv%pseudo(n)%u%inv
            o_minus_a = re%pseudo(n)%u
            sigma_o   = iv%pseudo(n)%u%error
         else if (nv > 0) then
            o_minus_b = iv%pseudo(n)%v%inv
            o_minus_a = re%pseudo(n)%v
            sigma_o   = iv%pseudo(n)%v%error
         else if (nt > 0) then
            o_minus_b = iv%pseudo(n)%t%inv
            o_minus_a = re%pseudo(n)%t
            sigma_o   = iv%pseudo(n)%t%error
         else if (np > 0) then
            o_minus_b = iv%pseudo(n)%p%inv
            o_minus_a = re%pseudo(n)%p
            sigma_o   = iv%pseudo(n)%p%error
         else if (nq > 0) then
            o_minus_b = iv%pseudo(n)%q%inv
            o_minus_a = re%pseudo(n)%q
            sigma_o   = iv%pseudo(n)%q%error
         end if

         write(stats_unit,'(/a,a1,a,e15.5)')' pseudo ', pseudo_var, ' o-b: ', o_minus_b
         write(stats_unit,' (a,a1,a,e15.5)')' pseudo ', pseudo_var, ' o-a: ', o_minus_a
         write(stats_unit,' (a,a1,a,e15.5)')' pseudo ', pseudo_var, ' sigma_o: ', sigma_o

         ! calculate equivalent sigma_b using o-a=(o-b)*sigma_o/(sigma_o+sigma_b)
         sigma_b = sqrt ((o_minus_b - o_minus_a) / o_minus_a) * sigma_o
         write(stats_unit,'(a,a1,a,e15.5)')' pseudo ', pseudo_var, ' sigma_b: ', sigma_b
      end if
   end do    

   ! do inter-processor communication to gather statistics.
   call da_proc_sum_int (nu)
   call da_proc_sum_int (nv)
   call da_proc_sum_int (nt)
   call da_proc_sum_int (np)
   call da_proc_sum_int (nq)
   iv%nstats(pseudo) = nu + nv + nt + np + nq
   
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
      if (nu /= 0 .or. nv /= 0 .OR. nt /= 0 .or. np /= 0 .OR. nq /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' O-A Diagnostics for pseudo'
         call da_print_stats_pseudo (stats_unit, nu, nv, nt, np, nq, stats)
      end if
   end if
   
   if (trace_use_dull) call da_trace_exit("da_ao_stats_pseudo")

end subroutine da_ao_stats_pseudo


