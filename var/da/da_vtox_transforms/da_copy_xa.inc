subroutine da_copy_xa (xa_dst, xa_src)

   !-----------------------------------------------------------------------
   ! Purpose: make a copy of grid%xa
   !-----------------------------------------------------------------------

   implicit none

   type (x_type), intent(inout)         :: xa_dst ! destination
   type (x_type), intent(in   )         :: xa_src ! source

   if (trace_use) call da_trace_entry("da_copy_xa")

   xa_dst % psfc(:,:)  = xa_src % psfc(:,:)
   xa_dst % mu(:,:)    = xa_src % mu(:,:)
   xa_dst % u(:,:,:)   = xa_src % u(:,:,:)
   xa_dst % v(:,:,:)   = xa_src % v(:,:,:)
   xa_dst % t(:,:,:)   = xa_src % t(:,:,:)
   xa_dst % q(:,:,:)   = xa_src % q(:,:,:)

   xa_dst % qcw(:,:,:) = xa_src % qcw(:,:,:)
   xa_dst % qrn(:,:,:) = xa_src % qrn(:,:,:)
   xa_dst % qci(:,:,:) = xa_src % qci(:,:,:)
   xa_dst % qsn(:,:,:) = xa_src % qsn(:,:,:)
   xa_dst % qgr(:,:,:) = xa_src % qgr(:,:,:)

   xa_dst % w(:,:,:)   = xa_src % w(:,:,:)
   xa_dst % p(:,:,:)   = xa_src % p(:,:,:)
   xa_dst % geoh(:,:,:)= xa_src % geoh(:,:,:)
   xa_dst % rh(:,:,:)  = xa_src % rh(:,:,:)
   xa_dst % wh(:,:,:)  = xa_src % wh(:,:,:)
   xa_dst % rho(:,:,:) = xa_src % rho(:,:,:)
   xa_dst % ref(:,:,:) = xa_src % ref(:,:,:)

   xa_dst % qt (:,:,:) = xa_src % qt (:,:,:)

   xa_dst % tgrn(:,:)  = xa_src % tgrn(:,:)
   xa_dst % u10(:,:)   = xa_src % u10(:,:)
   xa_dst % v10(:,:)   = xa_src % v10(:,:)
   xa_dst % t2(:,:)    = xa_src % t2(:,:)
   xa_dst % q2(:,:)    = xa_src % q2(:,:)

   xa_dst % ztd(:,:)   = xa_src % ztd(:,:)
   xa_dst % tpw(:,:)   = xa_src % tpw(:,:)
   xa_dst % speed(:,:) = xa_src % speed(:,:)
   xa_dst % tb19v(:,:) = xa_src % tb19v(:,:)
   xa_dst % tb19h(:,:) = xa_src % tb19h(:,:)
   xa_dst % tb22v(:,:) = xa_src % tb22v(:,:)
   xa_dst % tb37v(:,:) = xa_src % tb37v(:,:)
   xa_dst % tb37h(:,:) = xa_src % tb37h(:,:)
   xa_dst % tb85v(:,:) = xa_src % tb85v(:,:)
   xa_dst % tb85h(:,:) = xa_src % tb85h(:,:)

   if (trace_use) call da_trace_exit("da_copy_xa")

end subroutine da_copy_xa


