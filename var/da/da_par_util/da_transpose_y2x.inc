subroutine da_transpose_y2x (grid)

   implicit none

   type(domain), intent(inout)               :: grid

   if (trace_use_dull) call da_trace_entry("da_transpose_y2x")

#ifdef DM_PARALLEL
#include "XPOSE_V1_y2x.inc"
#else
   grid%xp % v1x(grid%xp%ids:grid%xp%ide,grid%xp%jds:grid%xp%jde,grid%xp%kds:grid%xp%kde) = &
   grid%xp % v1y(grid%xp%ids:grid%xp%ide,grid%xp%jds:grid%xp%jde,grid%xp%kds:grid%xp%kde)
#endif

   if (trace_use_dull) call da_trace_exit("da_transpose_y2x")

end subroutine da_transpose_y2x


