subroutine da_add_flow_dependence_vp (ne, ep, vp, &
                                      its,ite, jts,jte, kts,kte)

   !-----------------------------------------------------------------------
   ! Purpose: Add flow-dependent increments in control variable space (vp).
   !-----------------------------------------------------------------------
                                      
   implicit none
   
   integer, intent(in)                  :: ne  ! Ensemble size.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbations.
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   integer, intent(in)                  :: its, ite, jts, jte, kts, kte
   
   integer                              :: n, k! Loop counters.

   do n = 1, ne
      do k = kts, kte

         ! psi:
         vp % v1(its:ite,jts:jte,k) = vp % v1(its:ite,jts:jte,k) + &
                                      vp % alpha(its:ite,jts:jte,k,n) * &
                                      ep % v1(its:ite,jts:jte,k,n)
         ! chi_u:
         vp % v2(its:ite,jts:jte,k) = vp % v2(its:ite,jts:jte,k) + &
                                      vp % alpha(its:ite,jts:jte,k,n) * &
                                      ep % v2(its:ite,jts:jte,k,n)
         ! t_u:
         vp % v3(its:ite,jts:jte,k) = vp % v3(its:ite,jts:jte,k) + &
                                      vp % alpha(its:ite,jts:jte,k,n) * &
                                      ep % v3(its:ite,jts:jte,k,n)
         ! rh:
         vp % v4(its:ite,jts:jte,k) = vp % v4(its:ite,jts:jte,k) + &
                                      vp % alpha(its:ite,jts:jte,k,n) * &
                                      ep % v4(its:ite,jts:jte,k,n)
        ! ps_u
        vp % v5(its:ite,jts:jte,k) = vp % v5(its:ite,jts:jte,k) + &
                                     vp % alpha(its:ite,jts:jte,k,n) * &
                                     ep % v5(its:ite,jts:jte,k,n)
      end do
   end do

   if (trace_use) call da_trace_exit("da_add_flow_dependence_vp")

end subroutine da_add_flow_dependence_vp


