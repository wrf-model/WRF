subroutine da_matmulti(mata,matb,matc,ni,nj,nab)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: ni, nj, nab
   real,    intent(in)  :: mata(ni,nab), matb(nab, nj)
   real,    intent(out) :: matc(ni,nj)

   integer :: i, j, k            ! Loop counters

   if (trace_use) call da_trace_entry("da_matmultiover")

   matc = 0.0

   do i=1,ni
      do j=1,nj 
         do k=1,nab
            matc(i,j) = matc(i,j) + mata(i,k)*matb(k,j)
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_matmultiover")

end subroutine da_matmulti


