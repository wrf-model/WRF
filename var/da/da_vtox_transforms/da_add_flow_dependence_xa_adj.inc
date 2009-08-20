subroutine da_add_flow_dependence_xa_adj (ne, ep, xa, vp)

   !-----------------------------------------------------------------------
   ! Purpose: Add flow-dependent increments in model space (xa).
   !-----------------------------------------------------------------------
                                      
   implicit none
   
   integer, intent(in)                  :: ne  ! Ensemble size.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbations.
   type (x_type), intent(in)            :: xa  ! Analysis increments.
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   
   integer                              :: i, j, k, n ! Loop counters.
   real                                 :: alpha       ! Local alpha copy.

   vp % alpha = 0.0

   do n = ne, 1, -1
      ! ps:
      vp % alpha(its:ite,jts:jte,1,n) = vp % alpha(its:ite,jts:jte,1,n) + &
                                        ep % v5(its:ite,jts:jte,1,n) * & ! v5 = ps
                                        xa % psfc(its:ite,jts:jte)
   end do

   do k = kte, kts, -1
      do j = jte, jts, -1
         do i = ite, its, -1

            do n = ne, 1, -1
               alpha = 0.0
               alpha = alpha + ep % v4(i,j,k,n) * xa % q(i,j,k)
               alpha = alpha + ep % v3(i,j,k,n) * xa % t(i,j,k)
               alpha = alpha + ep % v2(i,j,k,n) * xa % v(i,j,k)
               alpha = alpha + ep % v1(i,j,k,n) * xa % u(i,j,k)
               vp % alpha(i,j,k,n) = vp % alpha(i,j,k,n) + alpha
            end do
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_add_flow_dependence_xa_adj")

end subroutine da_add_flow_dependence_xa_adj

