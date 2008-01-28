subroutine da_transpose_y2z (grid)

   implicit none

   type(domain), intent(inout)               :: grid

   if (trace_use_dull) call da_trace_entry("da_transpose_y2z")
   
#ifdef DM_PARALLEL
#include "XPOSE_V1_y2z.inc"
#else
   grid%xp % v1z(grid%xp%ids:grid%xp%ide,grid%xp%jds:grid%xp%jde,grid%xp%kds:grid%xp%kde) = &
   grid%xp % v1y(grid%xp%ids:grid%xp%ide,grid%xp%jds:grid%xp%jde,grid%xp%kds:grid%xp%kde)
#endif

   if (trace_use_dull) call da_trace_exit("da_transpose_y2z")

end subroutine da_transpose_y2z


