subroutine da_add_flow_dependence_vp_adj (ne, ep, vp)

   !-----------------------------------------------------------------------
   ! Purpose: Add flow-dependent increments in control variable space (vp).
   !-----------------------------------------------------------------------
                                      
   implicit none
   
   integer, intent(in)                  :: ne  ! Ensemble size.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbations.
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   
   integer                              :: n, k! Loop counters.

   vp % alpha(:,:,:,:) = 0.0

   do n = ne, 1, -1
      do k = kte, kts, -1

         vp % alpha(its:ite,jts:jte,k,n) = vp % alpha(its:ite,jts:jte,k,n) + &
                                         ep % v5(its:ite,jts:jte,k,n) * &
                                         vp % v5(its:ite,jts:jte,k)

         ! rh:
         vp % alpha(its:ite,jts:jte,k,n) = vp % alpha(its:ite,jts:jte,k,n) + &
                                         ep % v4(its:ite,jts:jte,k,n) * &
                                         vp % v4(its:ite,jts:jte,k)

         ! t_u:
         vp % alpha(its:ite,jts:jte,k,n) = vp % alpha(its:ite,jts:jte,k,n) + &
                                         ep % v3(its:ite,jts:jte,k,n) * &
                                         vp % v3(its:ite,jts:jte,k)

         ! chi_u:
         vp % alpha(its:ite,jts:jte,k,n) = vp % alpha(its:ite,jts:jte,k,n) + &
                                         ep % v2(its:ite,jts:jte,k,n) * &
                                         vp % v2(its:ite,jts:jte,k)

         ! psi:
         vp % alpha(its:ite,jts:jte,k,n) = vp % alpha(its:ite,jts:jte,k,n) + &
                                         ep % v1(its:ite,jts:jte,k,n) * &
                                         vp % v1(its:ite,jts:jte,k)

      end do
   end do

   if (trace_use) call da_trace_exit("da_add_flow_dependence_vp_adj")

end subroutine da_add_flow_dependence_vp_adj


