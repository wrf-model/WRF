subroutine da_add_xa (xa, xa_ens)

   !-----------------------------------------------------------------------
   ! Purpose: Add flow-dependent increments in model space (grid%xa).
   !-----------------------------------------------------------------------

   implicit none

   type (x_type),    intent(inout)      :: xa     ! analysis increment
   type (x_subtype), intent(in   )      :: xa_ens ! flow-dependent increment
!   type (x_type), intent(in   )      :: xa_ens ! flow-dependent increment

   if (trace_use) call da_trace_entry("da_add_xa")

   xa % u(:,:,:)   = xa % u(:,:,:)   + xa_ens % u(:,:,:)
   xa % v(:,:,:)   = xa % v(:,:,:)   + xa_ens % v(:,:,:)
   xa % t(:,:,:)   = xa % t(:,:,:)   + xa_ens % t(:,:,:)
   xa % q(:,:,:)   = xa % q(:,:,:)   + xa_ens % q(:,:,:)
!   xa % p(:,:,:)   = xa % p(:,:,:)   + xa_ens % p(:,:,:)

#ifdef CLOUD_CV
   xa % qcw(:,:,:) = xa % qcw(:,:,:) + xa_ens % qcw(:,:,:)
   xa % qrn(:,:,:) = xa % qrn(:,:,:) + xa_ens % qrn(:,:,:)
   xa % qci(:,:,:) = xa % qci(:,:,:) + xa_ens % qci(:,:,:)
   xa % qsn(:,:,:) = xa % qsn(:,:,:) + xa_ens % qsn(:,:,:)
   xa % qgr(:,:,:) = xa % qgr(:,:,:) + xa_ens % qgr(:,:,:)
#endif

   xa % psfc(:,:)  = xa % psfc(:,:)  + xa_ens % psfc(:,:)
!   xa % w(:,:,:)   = xa % w(:,:,:)   + xa_ens % w(:,:,:)
!   xa % rh(:,:,:)  = xa % rh(:,:,:)  + xa_ens % rh(:,:,:)
!   xa % wh(:,:,:)  = xa % wh(:,:,:)  + xa_ens % wh(:,:,:)
!   xa % rho(:,:,:) = xa % rho(:,:,:) + xa_ens % rho(:,:,:)
!   xa % ref(:,:,:) = xa % ref(:,:,:) + xa_ens % ref(:,:,:)

!   xa % qt (:,:,:) = xa % qt (:,:,:) + xa_ens % qt (:,:,:)

!   xa % tgrn(:,:)  = xa % tgrn(:,:)  + xa_ens % tgrn(:,:)
!   xa % u10(:,:)   = xa % u10(:,:)   + xa_ens % u10(:,:)
!   xa % v10(:,:)   = xa % v10(:,:)   + xa_ens % v10(:,:)
!   xa % t2(:,:)    = xa % t2(:,:)    + xa_ens % t2(:,:)
!   xa % q2(:,:)    = xa % q2(:,:)    + xa_ens % q2(:,:)

!   xa % ztd(:,:)   = xa % ztd(:,:)   + xa_ens % ztd(:,:)
!   xa % tpw(:,:)   = xa % tpw(:,:)   + xa_ens % tpw(:,:)
!   xa % speed(:,:) = xa % speed(:,:) + xa_ens % speed(:,:)
!   xa % tb19v(:,:) = xa % tb19v(:,:) + xa_ens % tb19v(:,:)
!   xa % tb19h(:,:) = xa % tb19h(:,:) + xa_ens % tb19h(:,:)
!   xa % tb22v(:,:) = xa % tb22v(:,:) + xa_ens % tb22v(:,:)
!   xa % tb37v(:,:) = xa % tb37v(:,:) + xa_ens % tb37v(:,:)
!   xa % tb37h(:,:) = xa % tb37h(:,:) + xa_ens % tb37h(:,:)
!   xa % tb85v(:,:) = xa % tb85v(:,:) + xa_ens % tb85v(:,:)
!   xa % tb85h(:,:) = xa % tb85h(:,:) + xa_ens % tb85h(:,:)

   if (trace_use) call da_trace_exit("da_add_xa")

end subroutine da_add_xa


