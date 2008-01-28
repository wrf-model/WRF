subroutine da_set_boundary_3d(var)
   !------------------------------------------------------------------------
   !  Purpose: 
   !
   !  Merge East-West boundary values for input 3d-array (var)
   !------------------------------------------------------------------------

   implicit none

   real, intent(inout) :: var(ims:ime, jms:jme, kms:kme)

   integer :: n, j, k

   if ((its /= ids) .or. (ite /= ide)) return

   if (trace_use) call da_trace_entry("da_set_boundary_3d")

   do k=kts, kte
      do j=jts, jte
         do n=1,bdyzone
            var(ids-n,j,k) = var(ide+1-n,j,k)
            var(ide+n,j,k) = var(ids-1+n,j,k)
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_set_boundary_3d")

end subroutine da_set_boundary_3d


