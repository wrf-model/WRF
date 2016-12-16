subroutine da_add_xa (xa, xa_ens)

   !-----------------------------------------------------------------------
   ! Purpose: Add flow-dependent increments in model space (grid%xa).
   !-----------------------------------------------------------------------

   implicit none

   type (x_type),    intent(inout)      :: xa     ! analysis increment
   type (x_subtype), intent(in   )      :: xa_ens ! flow-dependent increment

   if (trace_use) call da_trace_entry("da_add_xa")

   xa % u(:,:,:)   = xa % u(:,:,:)   + xa_ens % u(:,:,:)
   xa % v(:,:,:)   = xa % v(:,:,:)   + xa_ens % v(:,:,:)
   xa % t(:,:,:)   = xa % t(:,:,:)   + xa_ens % t(:,:,:)
   xa % q(:,:,:)   = xa % q(:,:,:)   + xa_ens % q(:,:,:)

   if ( alpha_hydrometeors ) then
      xa % qcw(:,:,:) = xa % qcw(:,:,:) + xa_ens % qcw(:,:,:)
      xa % qrn(:,:,:) = xa % qrn(:,:,:) + xa_ens % qrn(:,:,:)
      xa % qci(:,:,:) = xa % qci(:,:,:) + xa_ens % qci(:,:,:)
      xa % qsn(:,:,:) = xa % qsn(:,:,:) + xa_ens % qsn(:,:,:)
      xa % qgr(:,:,:) = xa % qgr(:,:,:) + xa_ens % qgr(:,:,:)
   end if

   xa % psfc(:,:)  = xa % psfc(:,:)  + xa_ens % psfc(:,:)

   if (trace_use) call da_trace_exit("da_add_xa")

end subroutine da_add_xa


