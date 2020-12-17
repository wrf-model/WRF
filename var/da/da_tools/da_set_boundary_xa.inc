subroutine da_set_boundary_xa(grid)

   !------------------------------------------------------------------------
   !  Purpose: 
   !
   !  Merge East-West boundary values for the desired grid%xa-type variables
   !------------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout) :: grid

   integer :: n, j, k

   if ((its /= ids) .or. (ite /= ide)) return

   if (trace_use) call da_trace_entry("da_set_boundary_xa")

   ! 2d

   k = kte + 1
   do j=jms, jme
      do n=1,bdyzone
         grid%xa%psfc(ids-n,j) = grid%xa%psfc(ide+1-n,j)
         grid%xa%w(ids-n,j,k) = grid%xa%w(ide+1-n,j,k)

         grid%xa%psfc(ide+n,j) = grid%xa%psfc(ids-1+n,j)
         grid%xa%w(ide+n,j,k) = grid%xa%w(ids-1+n,j,k)
      end do
   end do

   ! 3d

   do k=kts, kte
      do j=jms, jme
         do n=1,bdyzone
            grid%xa%u(ids-n,j,k) = grid%xa%u(ide+1-n,j,k)
            grid%xa%v(ids-n,j,k) = grid%xa%v(ide+1-n,j,k)
            grid%xa%t(ids-n,j,k) = grid%xa%t(ide+1-n,j,k)
            grid%xa%p(ids-n,j,k) = grid%xa%p(ide+1-n,j,k)
            grid%xa%q(ids-n,j,k) = grid%xa%q(ide+1-n,j,k)
            grid%xa%w(ids-n,j,k) = grid%xa%w(ide+1-n,j,k)

            grid%xa%u(ide+n,j,k) = grid%xa%u(ids-1+n,j,k)
            grid%xa%v(ide+n,j,k) = grid%xa%v(ids-1+n,j,k)
            grid%xa%t(ide+n,j,k) = grid%xa%t(ids-1+n,j,k)
            grid%xa%p(ide+n,j,k) = grid%xa%p(ids-1+n,j,k)
            grid%xa%q(ide+n,j,k) = grid%xa%q(ids-1+n,j,k)
            grid%xa%w(ide+n,j,k) = grid%xa%w(ids-1+n,j,k)
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_set_boundary_xa")

end subroutine da_set_boundary_xa


