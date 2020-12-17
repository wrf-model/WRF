subroutine da_calc_flow_dependence_xa (xa_ens, ne,  ep, vp, nobwin)

   !-----------------------------------------------------------------------
   ! Purpose: calculate flow-dependent increments in model space (xa)
   !          for a certain sub-window
   !-----------------------------------------------------------------------

   implicit none

   type (x_subtype), intent(inout)      :: xa_ens
   integer, intent(in)                  :: ne  ! Ensemble size.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbations.
   type (vp_type), intent(in)           :: vp  ! CV on grid structure.
   integer, intent(in), optional        :: nobwin

   integer                              :: iobwin, ii
   integer                              :: i, j, k, n  ! Loop counters.
   real                                 :: alpha       ! Local alpha copy.

   if (trace_use) call da_trace_entry("da_calc_flow_dependence_xa")

   iobwin = 1
   if ( present(nobwin) ) then
      iobwin = nobwin
   end if

   do n = 1, ne
      ii = (iobwin-1)*ensdim_alpha + n

      do k = kts, kte
         do j = jts, jte
            do i = its, ite

               alpha = vp % alpha(i,j,k,n)

               ! u:
               xa_ens % u(i,j,k) = xa_ens % u(i,j,k) + alpha * ep % v1(i,j,k,ii) ! v1 = u

               ! v:
               xa_ens % v(i,j,k) = xa_ens % v(i,j,k) + alpha * ep % v2(i,j,k,ii) ! v2 = v

               ! t:
               xa_ens % t(i,j,k) = xa_ens % t(i,j,k) + alpha * ep % v3(i,j,k,ii) ! v3 = t

               ! q:
               xa_ens % q(i,j,k) = xa_ens % q(i,j,k) + alpha * ep % v4(i,j,k,ii) ! v4 = q

            end do !i loop
         end do !j loop
      end do !k loop

      xa_ens % psfc(its:ite,jts:jte) = xa_ens % psfc(its:ite,jts:jte) +  &
                                       vp % alpha(its:ite,jts:jte,1,n) * &
                                       ep % v5(its:ite,jts:jte,1,ii)       ! v5 = ps

      if ( alpha_hydrometeors ) then
         do k = kts, kte
            do j = jts, jte
               do i = its, ite
                  alpha = vp % alpha(i,j,k,n)
                  xa_ens%qcw(i,j,k) = xa_ens % qcw(i,j,k) + alpha * ep%cw(i,j,k,ii)
                  xa_ens%qrn(i,j,k) = xa_ens % qrn(i,j,k) + alpha * ep%rn(i,j,k,ii)
                  xa_ens%qci(i,j,k) = xa_ens % qci(i,j,k) + alpha * ep%ci(i,j,k,ii)
                  xa_ens%qsn(i,j,k) = xa_ens % qsn(i,j,k) + alpha * ep%sn(i,j,k,ii)
                  xa_ens%qgr(i,j,k) = xa_ens % qgr(i,j,k) + alpha * ep%gr(i,j,k,ii)
               end do !i loop
            end do !j loop
         end do !k loop
      end if ! alpha_hydrometeors

   end do !n loop

   if (trace_use) call da_trace_exit("da_calc_flow_dependence_xa")

end subroutine da_calc_flow_dependence_xa


