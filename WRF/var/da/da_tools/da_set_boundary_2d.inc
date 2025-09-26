subroutine da_set_boundary_2d(var)

   !------------------------------------------------------------------------
   ! Purpose: Merge East-West boundary values for input 3d-array (var)   
   !------------------------------------------------------------------------

   implicit none

   real :: var(ims:ime, jms:jme)

   integer :: n, j

   if ((its /= ids) .or. (ite /= ide)) return

   if (trace_use) call da_trace_entry("da_set_boundary_2d")

   do j=jts, jte
      do n=1,bdyzone
         var(ids-n,j) = var(ide+1-n,j)
         var(ide+n,j) = var(ids-1+n,j)
      end do
   end do

   if (trace_use) call da_trace_exit("da_set_boundary_2d")

end subroutine da_set_boundary_2d


