subroutine da_matmultiover(mata,matb,ni,nj)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: ni, nj
   real,    intent(in)    :: matb(nj, nj)
   real,    intent(inout) :: mata(ni,nj)

   integer :: i, j, k          ! Loop counters
   real    :: tmp(1:nj)

   if (trace_use) call da_trace_entry("da_matmultiover")

   do i=1,ni
      tmp = 0.0
      do j=1,nj 
         do k=1,nj
            tmp(j) = tmp(j) + mata(i,k)*matb(k,j)
         end do
      end do
      do j=1,nj
         mata(i,j) = tmp(j) 
      end do
   end do

   if (trace_use) call da_trace_exit("da_matmultiover")

end subroutine da_matmultiover


