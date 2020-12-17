subroutine da_oi_stats_ssmt1 (stats_unit, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in) :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (in) :: iv            ! O-B

   type (maxmin_type) :: minimum
   type (maxmin_type) :: maximum
   integer            :: nt
   integer            :: n, k
   real               :: average, rms_err

   if (trace_use_dull) call da_trace_entry("da_oi_stats_ssmt1")

   nt = 0
   
   maximum = maxmin_type(-1.0E+20, 0, 0)
   minimum = maxmin_type(1.0E+20, 0, 0)
   average = 0.0
   rms_err = 0.0

   do n=1, iv%info(ssmt1)%nlocal
      if (iv%info(ssmt1)%proc_domain(1,n)) then
         do k=1, iv%info(ssmt1)%levels(n)   
            call da_stats_calculate(n, k, iv%ssmt1(n)%t(k)%qc, iv%ssmt1(n)%t(k)%inv, nt, &
               minimum, maximum, average, rms_err)
         end do
      end if    ! end if (iv%info(ssmt1)%proc_domain(1,n))
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(nt)
   
   call da_proc_stats_combine(average, rms_err, minimum%value, maximum%value, &
      minimum%n, maximum%n, minimum%l, maximum%l)
   
   if (rootproc) then
      if (nt /= 0) then  
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of O-B for SSMT1'
            call da_print_stats_ssmt1(stats_unit, nt, minimum, maximum, average, rms_err)   
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_oi_stats_ssmt1")

end subroutine da_oi_stats_ssmt1


