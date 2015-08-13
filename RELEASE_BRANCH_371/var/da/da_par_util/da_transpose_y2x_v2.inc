subroutine da_transpose_y2x_v2 (grid)

   implicit none

   type(domain), intent(inout)               :: grid
   integer                                   :: ij, i, j, k

   if (trace_use) call da_trace_entry("da_transpose_y2x_v2")
   
#ifdef DM_PARALLEL
#include "XPOSE_V2_y2x.inc"
#else
   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, i, j, k )
   do ij = 1 , grid%num_tiles
      do k = grid%xp%kds, grid%xp%kde
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = grid%xp%ids, grid%xp%ide
               grid%xp % v2x(i,j,k) = grid%xp % v2y(i,j,k)
            end do
         end do
      end do
   end do
   !$OMP END PARALLEL DO
#endif

   if (trace_use) call da_trace_exit("da_transpose_y2x_v2")

end subroutine da_transpose_y2x_v2


