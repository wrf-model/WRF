subroutine da_transpose_y2z (grid)

   implicit none

   type(domain), intent(inout)               :: grid
   integer                                   :: ij, i, j, k

   if (trace_use_dull) call da_trace_entry("da_transpose_y2z")
   
#ifdef DM_PARALLEL
#include "XPOSE_V1_y2z.inc"
#else
   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, i, j, k )
   do ij = 1 , grid%num_tiles
      do k = grid%xp%kds, grid%xp%kde
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = grid%xp%ids, grid%xp%ide
               grid%xp % v1z(i,j,k) = grid%xp % v1y(i,j,k)
            end do
         end do
      end do
   end do
   !$OMP END PARALLEL DO
#endif

   if (trace_use_dull) call da_trace_exit("da_transpose_y2z")

end subroutine da_transpose_y2z


