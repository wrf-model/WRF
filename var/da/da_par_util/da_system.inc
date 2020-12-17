subroutine da_system(cmd)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   character (len=*) , intent(in) :: cmd

#ifdef crayx1
   call pxfsystem(cmd, len(cmd), ierr)
#else
   call system(cmd)
#endif

end subroutine da_system


