subroutine da_transform_vchemtox_adj(grid, vchem, be)

   !-----------------------------------------------------------------------
   ! Purpose: Transform chemical CV to model space
   !-----------------------------------------------------------------------

   implicit none

   type(domain),  intent(in) :: grid
   type(xchem_type), intent(out) :: vchem   ! Grid point/EOF equivalent.
   type(be_type), intent(in)    :: be   ! Background error structure.

   integer :: ij, ic, i, j, m, n  ! Loop counters
   integer :: mz
   real :: temp

   if (trace_use) call da_trace_entry("da_transform_vchemtox_adj")

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, ic, m, n, j, i, temp )

         vchem%chem_ic = 0.D0

   do ij = 1 , grid%num_tiles
      do ic = PARAM_FIRST_SCALAR, num_chem
!!!         vchem%chem_ic(:,grid%j_start(ij):grid%j_end(ij),:,ic) = 0.D0
        do m = kts,kte !LEVELS
         do n = 1, be % v12(ic-1) % mz  !EIGENMODES
               do j = grid%j_start(ij), grid%j_end(ij) !               do j = jts, jte
                  temp = be % v12(ic-1) % evec (j,m,n) * be % v12(ic-1) % val (j,n)
                  do i = its, ite
                      vchem%chem_ic (i,j,n,ic) = vchem%chem_ic (i,j,n,ic) + &
                                                       temp * grid%xachem%chem_ic (i,j,m,ic) 
                  end do
               end do
            end do
         end do
      end do
   end do
   !$OMP END PARALLEL DO

   if (trace_use) call da_trace_exit("da_transform_vchemtox_adj")

end subroutine da_transform_vchemtox_adj


