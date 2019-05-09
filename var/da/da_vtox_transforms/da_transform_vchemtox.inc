subroutine da_transform_vchemtox(grid, vchem, be)

   !-----------------------------------------------------------------------
   ! Purpose: Transform chemical CV to model space
   !-----------------------------------------------------------------------

   implicit none

   type(domain),  intent(inout) :: grid
   type(xchem_type), intent(in) :: vchem   ! Grid point/EOF equivalent.
   type(be_type), intent(in)    :: be   ! Background error structure.

   integer :: ij, ic, i, j, m, n  ! Loop counters
   integer :: mz
   real :: temp

   if (trace_use) call da_trace_entry("da_transform_vchemtox")

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, ic, m, n, j, i, temp )

         grid%xachem%chem_ic = 0.D0

   do ij = 1 , grid%num_tiles
      do ic = PARAM_FIRST_SCALAR, num_chem
         do m = kts, kte    !LEVELS
            do n = 1, be % v12(ic-1) % mz !EIGENMODES
               do j = grid%j_start(ij), grid%j_end(ij)
!               do j = jts, jte
                  temp = be % v12(ic-1) % evec (j,m,n) * be % v12(ic-1) % val (j,n)
                  do i = its, ite
                      grid%xachem%chem_ic (i,j,m,ic) = grid%xachem%chem_ic (i,j,m,ic) + &
                                                       temp * vchem%chem_ic (i,j,n,ic)
                  end do
               end do
            end do
         end do
      end do
   end do
   !$OMP END PARALLEL DO

#ifdef DM_PARALLEL
#include "HALO_CHEM_XA.inc"     
#endif

   if (trace_use) call da_trace_exit("da_transform_vchemtox")

end subroutine da_transform_vchemtox


