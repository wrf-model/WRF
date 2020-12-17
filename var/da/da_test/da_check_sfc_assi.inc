subroutine da_check_sfc_assi(grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   
   implicit none

   type (domain), intent(inout)     :: grid
  
   type (iv_type),    intent(inout) :: iv    ! ob. increment vector.
   type (y_type),     intent(inout) :: y     ! y = h (grid%xa)

   real                           :: adj_ttl_lhs   ! < y, y >
   real                           :: adj_ttl_rhs   ! < x, x_adj >

   real                           :: partial_lhs   ! < y, y >
   real                           :: partial_rhs   ! < x, x_adj >

   real                           :: pertile_lhs   ! < y, y >
   real                           :: pertile_rhs   ! < x, x_adj >

   integer                        :: n

   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_u, xa2_v, xa2_t, &
                                                 xa2_p, xa2_q
 
   real, dimension(ims:ime, jms:jme)          :: xa2_u10, xa2_v10, xa2_t2, &
                                                 xa2_q2, xa2_tgrn, xa2_psfc


   if (trace_use) call da_trace_entry("da_check_sfc_assi")
  
   call da_message((/"check_sfc_assi: Adjoint Test Results:"/))
    
   xa2_u(ims:ime, jms:jme, kms:kme) = grid%xa%u(ims:ime, jms:jme, kms:kme)
   xa2_v(ims:ime, jms:jme, kms:kme) = grid%xa%v(ims:ime, jms:jme, kms:kme)
   xa2_t(ims:ime, jms:jme, kms:kme) = grid%xa%t(ims:ime, jms:jme, kms:kme)
   xa2_p(ims:ime, jms:jme, kms:kme) = grid%xa%p(ims:ime, jms:jme, kms:kme)
   xa2_q(ims:ime, jms:jme, kms:kme) = grid%xa%q(ims:ime, jms:jme, kms:kme)

   xa2_psfc(ims:ime, jms:jme) = grid%xa%psfc(ims:ime, jms:jme)
   xa2_tgrn(ims:ime, jms:jme) = grid%xa%tgrn(ims:ime, jms:jme)
   xa2_u10 (ims:ime, jms:jme) = grid%xa%u10 (ims:ime, jms:jme)
   xa2_v10 (ims:ime, jms:jme) = grid%xa%v10 (ims:ime, jms:jme)
   xa2_t2  (ims:ime, jms:jme) = grid%xa%t2  (ims:ime, jms:jme)
   xa2_q2  (ims:ime, jms:jme) = grid%xa%q2  (ims:ime, jms:jme)

   ! WHY?
   ! call check_psfc(grid, iv, y)

   call da_transform_xtowtq (grid)

#ifdef DM_PARALLEL
#include "HALO_SFC_XA.inc"
#endif


   partial_lhs = 0.0
   pertile_lhs = 0.0

   do n=1, iv%info(synop)%nlocal
      call da_transform_xtopsfc(grid, iv, synop, iv%synop(:), y%synop(:))


      pertile_lhs = pertile_lhs &
                  + y%synop(n)%u * y%synop(n)%u &
                  + y%synop(n)%v * y%synop(n)%v &
                  + y%synop(n)%t * y%synop(n)%t &
                  + y%synop(n)%p * y%synop(n)%p &
                  + y%synop(n)%q * y%synop(n)%q

      if (iv%info(synop)%proc_domain(1,n)) then
         partial_lhs = partial_lhs &
                     + y%synop(n)%u * y%synop(n)%u &
                     + y%synop(n)%v * y%synop(n)%v &
                     + y%synop(n)%t * y%synop(n)%t &
                     + y%synop(n)%p * y%synop(n)%p &
                     + y%synop(n)%q * y%synop(n)%q
      end if
   end do

   !----------------------------------------------------------------------
   ! [5.0] Perform adjoint operation:
   !----------------------------------------------------------------------

   call da_zero_x(grid%xa)

   do n=1, iv%info(synop)%nlocal
      call da_transform_xtopsfc_adj(grid,iv, synop,iv%synop(:),y%synop(:),grid%xa)
   end do

   call da_transform_xtowtq_adj (grid)
   
   pertile_rhs = sum(grid%xa%u(ims:ime, jms:jme, kms:kme) * &
      xa2_u(ims:ime, jms:jme, kms:kme)) + &
                 sum(grid%xa%v(ims:ime, jms:jme, kms:kme) * &
      xa2_v(ims:ime, jms:jme, kms:kme)) + &
                 sum(grid%xa%t(ims:ime, jms:jme, kms:kme) * &
      xa2_t(ims:ime, jms:jme, kms:kme)) + &
                 sum(grid%xa%p(ims:ime, jms:jme, kms:kme) * &
      xa2_p(ims:ime, jms:jme, kms:kme)) + &
                 sum(grid%xa%q(ims:ime, jms:jme, kms:kme) * &
      xa2_q(ims:ime, jms:jme, kms:kme)) + &
                 sum(grid%xa%psfc(ims:ime, jms:jme) * xa2_psfc(ims:ime, jms:jme))

   !-------------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjivnt test equation:
   !-------------------------------------------------------------------------
   
   partial_rhs = &
      sum(grid%xa%u(its:ite, jts:jte, kts:kte) * xa2_u(its:ite,jts:jte,kts:kte)) + &
      sum(grid%xa%v(its:ite, jts:jte, kts:kte) * xa2_v(its:ite,jts:jte,kts:kte)) + &
      sum(grid%xa%t(its:ite, jts:jte, kts:kte) * xa2_t(its:ite,jts:jte,kts:kte)) + &
      sum(grid%xa%p(its:ite, jts:jte, kts:kte) * xa2_p(its:ite,jts:jte,kts:kte)) + &
      sum(grid%xa%q(its:ite, jts:jte, kts:kte) * xa2_q(its:ite,jts:jte,kts:kte)) + &
      sum(grid%xa%psfc(its:ite, jts:jte) * xa2_psfc(its:ite, jts:jte))
   
   !-------------------------------------------------------------------------
   ! [7.0] Print output:
   !-------------------------------------------------------------------------
   
   write(unit=stdout, fmt='(A,1pe22.14)') &
        ' Tile < y, y     > = ', pertile_lhs, &
        ' Tile < x, x_adj > = ', pertile_rhs

   adj_ttl_lhs = wrf_dm_sum_real(partial_lhs)
   adj_ttl_rhs = wrf_dm_sum_real(partial_rhs)
   write (unit=stdout,fmt='(A,2F10.2)') &
      'TEST_COVERAGE_check_sfc_assi_A:  adj_ttl_lhs,adj_ttl_rhs = ', &
      adj_ttl_lhs,adj_ttl_rhs
   if (rootproc) then
      write(unit=stdout, fmt='(A,1pe22.14)') &
         ' Whole Domain < y, y     > = ', adj_ttl_lhs
      write(unit=stdout, fmt='(A,1pe22.14)') &
         ' Whole Domain < x, x_adj > = ', adj_ttl_rhs
   end if

   ! recover grid%xa
   grid%xa%u(ims:ime, jms:jme, kms:kme) = xa2_u(ims:ime, jms:jme, kms:kme)
   grid%xa%v(ims:ime, jms:jme, kms:kme) = xa2_v(ims:ime, jms:jme, kms:kme)
   grid%xa%t(ims:ime, jms:jme, kms:kme) = xa2_t(ims:ime, jms:jme, kms:kme)
   grid%xa%p(ims:ime, jms:jme, kms:kme) = xa2_p(ims:ime, jms:jme, kms:kme)
   grid%xa%q(ims:ime, jms:jme, kms:kme) = xa2_q(ims:ime, jms:jme, kms:kme)

   grid%xa%psfc(ims:ime, jms:jme) = xa2_psfc(ims:ime, jms:jme)
   grid%xa%tgrn(ims:ime, jms:jme) = xa2_tgrn(ims:ime, jms:jme)
   grid%xa%u10 (ims:ime, jms:jme) = xa2_u10 (ims:ime, jms:jme)
   grid%xa%v10 (ims:ime, jms:jme) = xa2_v10 (ims:ime, jms:jme)
   grid%xa%t2  (ims:ime, jms:jme) = xa2_t2  (ims:ime, jms:jme)
   grid%xa%q2  (ims:ime, jms:jme) = xa2_q2  (ims:ime, jms:jme)

   call wrf_shutdown

   if (trace_use) call da_trace_exit("da_check_sfc_assi")
   
end subroutine da_check_sfc_assi



