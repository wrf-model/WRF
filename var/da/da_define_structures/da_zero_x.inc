subroutine da_zero_x ( x )

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (x_type), intent(inout)        :: x      ! Analysis incrs structure.

   if (trace_use_dull) call da_trace_entry("da_zero_x")

   x % u(:,:,:) = 0.0
   x % v(:,:,:) = 0.0
   x % w(:,:,:) = 0.0
   x % t(:,:,:) = 0.0
   x % q(:,:,:) = 0.0
   x % p(:,:,:) = 0.0
   x % geoh(:,:,:) = 0.0
   x % rh(:,:,:) = 0.0
   x % wh(:,:,:) = 0.0
   x % rho(:,:,:) = 0.0
   x % ref(:,:,:) = 0.0

   x % qcw(:,:,:) = 0.0
   x % qrn(:,:,:) = 0.0
   x % qt (:,:,:) = 0.0
   x % qci(:,:,:) = 0.0
   x % qsn(:,:,:) = 0.0
   x % qgr(:,:,:) = 0.0

   x % tgrn(:,:) = 0.0
   x % psfc(:,:) = 0.0
   x % mu(:,:) = 0.0
   x % u10(:,:) = 0.0
   x % v10(:,:) = 0.0
   x % t2(:,:) = 0.0
   x % q2(:,:) = 0.0

   x % ztd(:,:) = 0.0
   x % tpw(:,:) = 0.0
   x % speed(:,:) = 0.0
   x % tb19v(:,:) = 0.0
   x % tb19h(:,:) = 0.0
   x % tb22v(:,:) = 0.0
   x % tb37v(:,:) = 0.0
   x % tb37h(:,:) = 0.0
   x % tb85v(:,:) = 0.0
   x % tb85h(:,:) = 0.0

   if (trace_use_dull) call da_trace_exit("da_zero_x")

end subroutine da_zero_x


