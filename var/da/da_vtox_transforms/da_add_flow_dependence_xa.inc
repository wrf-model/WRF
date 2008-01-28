subroutine da_add_flow_dependence_xa (grid, ne, ep, vp)

   !-----------------------------------------------------------------------
   ! Purpose: Add flow-dependent increments in model space (grid%xa).
   !-----------------------------------------------------------------------
                                      
   implicit none
   
   type (domain), intent(inout)         :: grid
   integer, intent(in)                  :: ne  ! Ensemble size.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbations.
   type (vp_type), intent(in)           :: vp  ! CV on grid structure.
   
   integer                              :: n, k! Loop counters.

   do n = 1, ne
      do k = kts, kte

         ! u:
         grid%xa % u(its:ite,jts:jte,k) = grid%xa % u(its:ite,jts:jte,k) + &
                                     vp % alpha(its:ite,jts:jte,n) * &
                                     ep % v1(its:ite,jts:jte,k,n) ! v1 = u
         ! v:
         grid%xa % v(its:ite,jts:jte,k) = grid%xa % v(its:ite,jts:jte,k) + &
                                     vp % alpha(its:ite,jts:jte,n) * &
                                     ep % v2(its:ite,jts:jte,k,n) ! v2 = v
         ! t:
         grid%xa % t(its:ite,jts:jte,k) = grid%xa % t(its:ite,jts:jte,k) + &
                                     vp % alpha(its:ite,jts:jte,n) * &
                                     ep % v3(its:ite,jts:jte,k,n) ! v3 = t
         ! q:
         grid%xa % q(its:ite,jts:jte,k) = grid%xa % q(its:ite,jts:jte,k) + &
                                     vp % alpha(its:ite,jts:jte,n) * &
                                     ep % v4(its:ite,jts:jte,k,n) ! v4 = q
      end do

      ! ps:
      grid%xa % psfc(its:ite,jts:jte) = grid%xa % psfc(its:ite,jts:jte) + &
                                  vp % alpha(its:ite,jts:jte,n) * &
                                  ep % v5(its:ite,jts:jte,1,n) ! v5 = ps
   end do

   if (trace_use) call da_trace_exit("da_add_flow_dependence_xa")

end subroutine da_add_flow_dependence_xa


