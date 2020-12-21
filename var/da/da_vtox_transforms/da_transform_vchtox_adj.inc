subroutine da_transform_vchtox_adj(grid, vch, be)

   !-----------------------------------------------------------------------
   ! Purpose: Transform chemical CV to model space
   !-----------------------------------------------------------------------

   implicit none

   type(domain),  intent(in) :: grid
   type(xch_type), intent(out) :: vch   ! Grid point/EOF equivalent.
   type(be_type), intent(in)    :: be   ! Background error structure.

   integer :: ij, ic, i, j, m, n  ! Loop counters
   integer :: mz
   real :: temp

   if (trace_use) call da_trace_entry("da_transform_vchtox_adj")

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, ic, m, n, j, i, temp )
   do ij = 1 , grid%num_tiles
      do ic = PARAM_FIRST_SCALAR, num_scaleant
         vch%alpha_ant(:,grid%j_start(ij):grid%j_end(ij),:,ic) = 0.D0
         do n = 1, be % alpha_ant_chem(ic) % mz  !EIGENMODES
            do m = 1, num_ant_steps !LEVELS
               do j = grid%j_start(ij), grid%j_end(ij)
                  temp = be % alpha_ant_chem(ic) % evec (j,m,n) * be % alpha_ant_chem(ic) % val (j,n)
                  do i = its, ite
                      vch%alpha_ant (i,j,n,ic) = vch%alpha_ant (i,j,n,ic) + &
                                                       temp * grid%xach%alpha_ant (i,j,m,ic) 
                  end do
               end do
            end do
         end do
      end do
   end do
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, ic, m, n, j, i, temp )
   do ij = 1 , grid%num_tiles
      do ic = PARAM_FIRST_SCALAR, num_scalebb
         vch%alpha_bb(:,grid%j_start(ij):grid%j_end(ij),:,ic) = 0.D0
         do n = 1, be % alpha_bb_chem(ic) % mz  !EIGENMODES
            do m = 1, num_bb_steps !LEVELS
               do j = grid%j_start(ij), grid%j_end(ij)
                  temp = be % alpha_bb_chem(ic) % evec (j,m,n) * be % alpha_bb_chem(ic) % val (j,n)
                  do i = its, ite
                      vch%alpha_bb (i,j,n,ic) = vch%alpha_bb (i,j,n,ic) + &
                                                       temp * grid%xach%alpha_bb (i,j,m,ic) 
                  end do
               end do
            end do
         end do
      end do
   end do
   !$OMP END PARALLEL DO

   if (trace_use) call da_trace_exit("da_transform_vchtox_adj")

end subroutine da_transform_vchtox_adj


