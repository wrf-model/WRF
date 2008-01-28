subroutine da_add_flow_dependence_xa_adj (ne, ep, xa, vp)

   !-----------------------------------------------------------------------
   ! Purpose: Add flow-dependent increments in model space (xa).
   !-----------------------------------------------------------------------
                                      
   implicit none
   
   integer, intent(in)                  :: ne  ! Ensemble size.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbations.
   type (x_type), intent(in)            :: xa  ! Analysis increments.
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   
   integer                              :: n, k! Loop counters.

   vp % alpha(:,:,:) = 0.0

   do n = ne, 1, -1
      ! ps:
      vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                      ep % v5(its:ite,jts:jte,1,n) * & ! v5 = ps
                                      xa % psfc(its:ite,jts:jte)

      do k = kte, kts, -1

         ! q:
         vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                         ep % v4(its:ite,jts:jte,k,n) * & ! v4 = q
                                         xa % q(its:ite,jts:jte,k)

         ! t:
         vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                         ep % v3(its:ite,jts:jte,k,n) * & ! v3 = t
                                         xa % t(its:ite,jts:jte,k)

         ! v:
         vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                         ep % v2(its:ite,jts:jte,k,n) * & ! v2 = v
                                         xa % v(its:ite,jts:jte,k)

         ! u:
         vp % alpha(its:ite,jts:jte,n) = vp % alpha(its:ite,jts:jte,n) + &
                                         ep % v1(its:ite,jts:jte,k,n) * & ! v1 = u
                                         xa % u(its:ite,jts:jte,k)
      end do
   end do

   if (trace_use) call da_trace_exit("da_add_flow_dependence_xa_adj")

end subroutine da_add_flow_dependence_xa_adj


