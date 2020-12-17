subroutine da_togrid (x, ib, ie, i, dx, dxm)

   !-----------------------------------------------------------------------
   ! Purpose: Transfer obs. x to grid i and calculate its
   ! distance to grid i and i+1
   !-----------------------------------------------------------------------

   implicit none

   real,                     intent(in)  :: x
   integer,                  intent(in)  :: ib, ie
   real,                     intent(out) :: dx, dxm
   integer,                  intent(out) :: i

   if (trace_use_dull) call da_trace_entry("da_togrid")
   
   i = int (x)

   if (i < ib) then
      i = ib
   else if (i >= ie) then
      i = ie - 1
   end if

   dx = x - real(i)
   dxm= 1.0 - dx

   if (trace_use_dull) call da_trace_exit("da_togrid")

end subroutine da_togrid


