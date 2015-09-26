subroutine da_couple (mut, field, msf, ids, ide, jds, jde, kds, kde)

   implicit none

   integer, intent(in)    :: ids, ide, jds, jde, kds, kde
   real,    intent(in)    :: mut(ids:ide, jds:jde)
   real,    intent(in)    :: msf(ids:ide, jds:jde)
   real,    intent(inout) :: field(ids:ide, jds:jde, kds:kde)
  
   integer :: i, j, k

   ! update_bc
   ! if (trace_use) call da_trace_entry("da_couple")
  
   do j=jds,jde
      do k=kds,kde
         do i=ids,ide
            field(i,j,k)=field(i,j,k)*mut(i,j)/msf(i,j)
         end do
      end do
   end do

   ! if (trace_use) call da_trace_exit("da_couple")

end subroutine da_couple
