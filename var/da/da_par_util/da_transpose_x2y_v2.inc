subroutine da_transpose_x2y_v2 (grid)

   implicit none

   type(domain), intent(inout)               :: grid

   if (trace_use) call da_trace_entry("da_transpose_x2y_v2")
   
#ifdef DM_PARALLEL
#include "XPOSE_V2_x2y.inc"
#else
   grid%xp % v2y(grid%xp%ids:grid%xp%ide,grid%xp%jds:grid%xp%jde,grid%xp%kds:grid%xp%kde) = &
   grid%xp % v2x(grid%xp%ids:grid%xp%ide,grid%xp%jds:grid%xp%jde,grid%xp%kds:grid%xp%kde)
#endif

   if (trace_use) call da_trace_exit("da_transpose_x2y_v2")

end subroutine da_transpose_x2y_v2


