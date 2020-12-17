function da_residual(n, k, yy, ox, n_bad) result (rr)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,              intent(in)    :: n, k
   type (field_type),    intent(in)    :: ox
   real,                 intent(in)    :: yy
   type (bad_info_type), intent(inout) :: n_bad

   real :: rr

   if (trace_use_frequent) call da_trace_entry("da_residual")

   ! WHY?
   if (k ==0 .OR. n==0) then
   end if

   rr = 0.0

   if (ox % qc >= obs_qc_pointer) then
      ! WHY?
      ! n_bad % num % use = n_bad % num % use + 1
      rr = ox % inv - yy
      ! else
      !    if (ox % qc /= -88) then
      !       n_bad % num % bad = n_bad % num % bad + 1
      !       n_bad % nn(n_bad % num % bad) = n
      !       n_bad % kk(n_bad % num % bad) = k
      !     else
      !       n_bad % num % miss = n_bad % num % miss + 1
      !    end if
   end if

   if (trace_use_frequent) call da_trace_exit("da_residual")

end function da_residual


