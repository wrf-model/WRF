subroutine da_innerprod(mata,matc,ni,nj)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: ni, nj
   real,    intent(in)  :: mata(ni,nj)
   real*8,  intent(out) :: matc(nj,nj)

   integer             :: i1, i2, k       ! Loop counters.

   if (trace_use) call da_trace_entry("da_innerprod")

   matc = 0.0

   do i1=1,nj
      do i2=1,nj
         do k=1,ni
            matc(i1,i2) = matc(i1,i2) + mata(k,i1)*mata(k,i2)
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_innerprod")

end subroutine da_innerprod


