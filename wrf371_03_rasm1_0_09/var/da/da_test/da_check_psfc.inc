subroutine da_check_psfc(grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
     
   implicit none
   
   type (domain),  intent(inout)     :: grid
   type (iv_type), intent(inout)     :: iv  ! ob. increment vector.
   type (y_type),  intent(inout)     :: y   ! residual

   real                           :: adj_ttl_lhs   ! < y, y >
   real                           :: adj_ttl_rhs   ! < x, x_adj >

   real                           :: partial_lhs   ! < y, y >
   real                           :: partial_rhs   ! < x, x_adj >

   real                           :: pertile_lhs   ! < y, y >
   real                           :: pertile_rhs   ! < x, x_adj >

   integer                        :: n

   real, dimension(ims:ime, jms:jme) :: xa2_u10, xa2_v10, xa2_t2, &
                                        xa2_q2, xa2_psfc

   if (trace_use) call da_trace_entry("da_check_psfc")
   
   write(unit=stdout, fmt='(/3a,i6/a)') &
        'File: ', __FILE__, ', line:', __LINE__, &
        'Adjoint Test Results:'

   ! save input

   xa2_psfc(ims:ime, jms:jme) = grid%xa%p   (ims:ime, jms:jme, kts)
   xa2_u10 (ims:ime, jms:jme) = grid%xa%u10 (ims:ime, jms:jme)
   xa2_v10 (ims:ime, jms:jme) = grid%xa%v10 (ims:ime, jms:jme)
   xa2_t2  (ims:ime, jms:jme) = grid%xa%t2  (ims:ime, jms:jme)
   xa2_q2  (ims:ime, jms:jme) = grid%xa%q2  (ims:ime, jms:jme)

   !----------------------------------------------------------------------

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

   !-------------------------------------------------------------------------
   ! [5.0] Perform adjoint operation:
   !-------------------------------------------------------------------------

   grid%xa%psfc(ims:ime, jms:jme) = 0.0
   grid%xa%tgrn(ims:ime, jms:jme) = 0.0
   grid%xa%u10 (ims:ime, jms:jme) = 0.0
   grid%xa%v10 (ims:ime, jms:jme) = 0.0
   grid%xa%t2  (ims:ime, jms:jme) = 0.0
   grid%xa%q2  (ims:ime, jms:jme) = 0.0
   
   do n=1, iv%info(synop)%nlocal
      call da_transform_xtopsfc_adj(grid,iv,synop,iv%synop(:),y%synop(:),grid%xa)
   end do

   pertile_rhs = sum(grid%xa%u10 (ims:ime, jms:jme) * xa2_u10 (ims:ime, jms:jme)) &
               + sum(grid%xa%v10 (ims:ime, jms:jme) * xa2_v10 (ims:ime, jms:jme)) &
               + sum(grid%xa%t2  (ims:ime, jms:jme) * xa2_t2  (ims:ime, jms:jme)) &
               + sum(grid%xa%q2  (ims:ime, jms:jme) * xa2_q2  (ims:ime, jms:jme)) &
               + sum(grid%xa%psfc(ims:ime, jms:jme) * xa2_psfc(ims:ime, jms:jme))

   partial_rhs = sum(grid%xa%u10 (its:ite, jts:jte) * xa2_u10 (its:ite, jts:jte)) &
               + sum(grid%xa%v10 (its:ite, jts:jte) * xa2_v10 (its:ite, jts:jte)) &
               + sum(grid%xa%t2  (its:ite, jts:jte) * xa2_t2  (its:ite, jts:jte)) &
               + sum(grid%xa%q2  (its:ite, jts:jte) * xa2_q2  (its:ite, jts:jte)) &
               + sum(grid%xa%psfc(its:ite, jts:jte) * xa2_psfc(its:ite, jts:jte))
   
   !----------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjoint test equation:
   !----------------------------------------------------------------------

   !----------------------------------------------------------------------
   ! [7.0] Print output:
   !----------------------------------------------------------------------
   
   write(unit=stdout, fmt='(A,1pe22.14)') &
      ' Tile < y, y     > = ', pertile_lhs, &
      ' Tile < x, x_adj > = ', pertile_rhs

   adj_ttl_lhs = wrf_dm_sum_real(partial_lhs)
   adj_ttl_rhs = wrf_dm_sum_real(partial_rhs)
   write (unit=stdout,fmt='(A,2F10.2)') &
      'TEST_COVERAGE_check_sfc_assi_B:  adj_ttl_lhs,adj_ttl_rhs = ', &
      adj_ttl_lhs,adj_ttl_rhs
   if (rootproc) then
      write(unit=stdout, fmt='(A,1pe22.14)') ' Whole Domain < y, y     > = ', &
         adj_ttl_lhs
      write(unit=stdout, fmt='(A,1pe22.14)') ' Whole Domain < x, x_adj > = ', &
         adj_ttl_rhs
   end if

   ! recover
   grid%xa%psfc(ims:ime, jms:jme) = xa2_psfc(ims:ime, jms:jme)
   grid%xa%u10 (ims:ime, jms:jme) = xa2_u10 (ims:ime, jms:jme)
   grid%xa%v10 (ims:ime, jms:jme) = xa2_v10 (ims:ime, jms:jme)
   grid%xa%t2  (ims:ime, jms:jme) = xa2_t2  (ims:ime, jms:jme)
   grid%xa%q2  (ims:ime, jms:jme) = xa2_q2  (ims:ime, jms:jme)

   if (trace_use) call da_trace_exit("da_check_psfc")

end subroutine da_check_psfc


