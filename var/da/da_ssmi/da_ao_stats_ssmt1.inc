subroutine da_ao_stats_ssmt1 (stats_unit, iv, re)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)    :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (inout) :: iv            ! OI
   type  (y_type), intent (in)    :: re            ! O-A

   type (maxmin_type) :: minimum
   type (maxmin_type) :: maximum
   integer            :: nt
   integer            :: n, k
   real               :: average, rms_err

   if (trace_use_dull) call da_trace_entry("da_ao_stats_ssmt1")

   nt = 0

   maximum = maxmin_type(-1.0E+20, 0, 0)
   minimum = maxmin_type (1.0E+20, 0, 0)
   average = 0.0
   rms_err = 0.0

   if (iv%info(ssmt1)%nlocal > 0) then
      do n=1, iv%info(ssmt1)%nlocal
         if (iv%info(ssmt1)%proc_domain(1,n)) then
            do k=1, iv%info(ssmt1)%levels(n)
               call da_stats_calculate (n, k, iv%ssmt1(n)%t(k)%qc, re%ssmt1(n)%t(k), nt, &
                  minimum, maximum, average, rms_err)
            end do
         end if    ! end if (iv%info(ssmt1)%proc_domain(1,n))
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nt)
   iv%nstats(ssmt1) = nt

   call da_proc_stats_combine(average, rms_err, minimum%value, maximum%value, &
      minimum%n, maximum%n, minimum%l, maximum%l)
   
   if (rootproc) then
      if (nt /= 0) then
          write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of O-A for SSMT1'
          call da_print_stats_ssmt1 (stats_unit, nt, minimum, maximum, average, rms_err)
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_ao_stats_ssmt1")

end subroutine da_ao_stats_ssmt1


