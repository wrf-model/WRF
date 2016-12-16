subroutine da_calc_flow_dependence_xa_adj (ne, ep, xa, vp, nobwin)

   !-----------------------------------------------------------------------
   ! Purpose: adjoint of calculating flow-dependent increments in model space (xa)
   !          for a certain sub-window
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                  :: ne  ! Ensemble size.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbations.
   type (x_type), intent(inout)         :: xa  ! Analysis increments.
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.

   integer                              :: i, j, k, n ! Loop counters.
   real                                 :: alpha      ! Local alpha copy.
   integer, intent(in), optional        :: nobwin
   integer                              :: iobwin, ii

   if (trace_use) call da_trace_entry("da_calc_flow_dependence_xa_adj")

   iobwin = 1
   if ( present(nobwin) ) then
      iobwin = nobwin
   end if

   !done outside this subroutine
   !vp % alpha = 0.0

   do n = ne, 1, -1
      ii = (iobwin-1)*ensdim_alpha + n

      if ( alpha_hydrometeors ) then
         do k = kte, kts, -1
            do j = jte, jts, -1
               do i = ite, its, -1
                  alpha = 0.0
                  alpha = alpha + ep % gr(i,j,k,ii) * xa % qgr(i,j,k)
                  alpha = alpha + ep % sn(i,j,k,ii) * xa % qsn(i,j,k)
                  alpha = alpha + ep % ci(i,j,k,ii) * xa % qci(i,j,k)
                  alpha = alpha + ep % rn(i,j,k,ii) * xa % qrn(i,j,k)
                  alpha = alpha + ep % cw(i,j,k,ii) * xa % qcw(i,j,k)
                  vp % alpha(i,j,k,n) = vp % alpha(i,j,k,n) + alpha
               end do
            end do
         end do
      end if ! alpha_hydrometeors

      ! ps:
      vp % alpha(its:ite,jts:jte,1,n) = vp % alpha(its:ite,jts:jte,1,n) + &
                                        ep % v5(its:ite,jts:jte,1,ii) *   & ! v5 = ps
                                        xa % psfc(its:ite,jts:jte)
      do k = kte, kts, -1
         do j = jte, jts, -1
            do i = ite, its, -1
               alpha = 0.0
               alpha = alpha + ep % v4(i,j,k,ii) * xa % q(i,j,k)
               alpha = alpha + ep % v3(i,j,k,ii) * xa % t(i,j,k)
               alpha = alpha + ep % v2(i,j,k,ii) * xa % v(i,j,k)
               alpha = alpha + ep % v1(i,j,k,ii) * xa % u(i,j,k)
               vp % alpha(i,j,k,n) = vp % alpha(i,j,k,n) + alpha
            end do
         end do
      end do

   end do !n loop

   if (trace_use) call da_trace_exit("da_calc_flow_dependence_xa_adj")

end subroutine da_calc_flow_dependence_xa_adj

