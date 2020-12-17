subroutine da_stats_calculate(n, k, qc_flag, x, nn, minimum, maximum, &
   average, rms_err)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,            intent(in)    :: n             ! Sequence number of ob.
   integer,            intent(in)    :: k             ! Level of ob.
   integer,            intent(in)    :: qc_flag       ! QC flag.
   real,               intent(in)    :: x             ! Value.
   integer,            intent(inout) :: nn            ! Number of ok obs.
   type (maxmin_type), intent(inout) :: minimum, maximum
   real,               intent(inout) :: average, rms_err

   if (trace_use_dull) call da_trace_entry("da_stats_calculate")

   if (qc_flag >= obs_qc_pointer) then
      nn = nn + 1

      if (x < minimum%value) then
         minimum%value = x
         minimum%n     = n
         minimum%l     = k
      end if

      if (x > maximum%value) then
         maximum%value = x
         maximum%n     = n
         maximum%l     = k
      end if

      average = average + x
      rms_err = rms_err + x * x
   end if

   if (trace_use_dull) call da_trace_exit("da_stats_calculate")

end subroutine da_stats_calculate


