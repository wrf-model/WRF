subroutine da_residual_new(yy, qc, inv, rr)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: qc(:,:)
   real,    intent(in)  :: inv(:,:)
   real,    intent(in)  :: yy(:,:)
   real,    intent(out) :: rr(:,:)

   if (trace_use) call da_trace_entry("da_residual_new")
  
   where (qc(:,:) >= obs_qc_pointer)
      rr(:,:) = inv(:,:) - yy(:,:)
   elsewhere
      rr(:,:) = 0.0
   endwhere

   if (trace_use) call da_trace_exit("da_residual_new")

end subroutine da_residual_new


