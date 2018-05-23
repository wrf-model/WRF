subroutine da_togrid_1d (x, ib, ie, i, dx, dxm)

   !-----------------------------------------------------------------------
   ! Purpose: Transfer obs. x to grid i and calculate its
   ! distance to grid i and i+1
   !-----------------------------------------------------------------------

   implicit none

   real,    intent(in)  :: x(:)
   integer, intent(in)  :: ib, ie
   real,    intent(out) :: dx(:), dxm(:)
   integer, intent(out) :: i(:)

   if (trace_use) call da_trace_entry("da_togrid_1d")

!   where (x(:) > 0.0) 
!      i   = int (x)
!
!      where(i(:) < ib)  i = ib
!      where(i(:) >= ie) i = ie-1
!
!      dx  = x - real(i)
!      dxm = 1.0 - dx
!   elsewhere 
!      i   = 0
!      dx  = 0.0
!      dxm = 0.0
!   end where

   i   = int (x)
   where (i(:) < ib)
      i = ib
   elsewhere (i(:) >= ie)
      i = ie - 1
   end where
   dx  = x - real(i)
   dxm = 1.0 - dx

   if (trace_use) call da_trace_exit("da_togrid_1d")

end subroutine da_togrid_1d


