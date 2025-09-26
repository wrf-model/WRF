subroutine da_couple (mut, field, msf, c1, c2, ids, ide, jds, jde, kds, kde)

   implicit none

   integer, intent(in)    :: ids, ide, jds, jde, kds, kde
   real,    intent(in)    :: mut(ids:ide, jds:jde)
   real,    intent(in)    :: msf(ids:ide, jds:jde)
   real,    intent(in)    :: c1(kds:kde)
   real,    intent(in)    :: c2(kds:kde)
   real,    intent(inout) :: field(ids:ide, jds:jde, kds:kde)

   integer :: i, j, k

   ! update_bc
   ! if (trace_use) call da_trace_entry("da_couple")

   do k=kds,kde
      do j=jds,jde
         do i=ids,ide
            field(i,j,k)=field(i,j,k)*(c1(k)*mut(i,j)+c2(k))/msf(i,j)
         end do
      end do
   end do

   ! if (trace_use) call da_trace_exit("da_couple")

end subroutine da_couple
