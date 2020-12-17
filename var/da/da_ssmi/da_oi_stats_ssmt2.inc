subroutine da_oi_stats_ssmt2 (stats_unit, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)      :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (in)      :: iv            ! O-B

   type (maxmin_type) :: minimum
   type (maxmin_type) :: maximum
   integer            :: nrh
   integer            :: n, k
   real               :: average, rms_err

   if (trace_use_dull) call da_trace_entry("da_oi_stats_ssmt2")

   nrh = 0
   
   maximum = maxmin_type(-1.0E+20, 0, 0)
   minimum = maxmin_type(1.0E+20, 0, 0)
   average = 0.0
   rms_err = 0.0

   do n=1, iv%info(ssmt2)%nlocal
      if (iv%info(ssmt2)%proc_domain(1,n)) then
         do k=1, iv%info(ssmt2)%levels(n)   
            call da_stats_calculate(n, k, iv%ssmt2(n)%rh(k)%qc, iv%ssmt2(n)%rh(k)%inv, nrh, &
               minimum, maximum, average, rms_err)
         end do
      end if    ! end if (iv%info(ssmt2)%proc_domain(1,n))
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int(nrh)
   
   call da_proc_stats_combine(average, rms_err, minimum%value, maximum%value, &
      minimum%n, maximum%n, minimum%l, maximum%l)
   
   if (rootproc) then
      if (nrh /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of O-B for SSMT2'
         call da_print_stats_ssmt2(stats_unit, nrh, minimum, maximum, average, rms_err)
      end if 
   end if

   if (trace_use_dull) call da_trace_exit("da_oi_stats_ssmt2")

end subroutine da_oi_stats_ssmt2


