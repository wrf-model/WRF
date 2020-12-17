subroutine da_filter(grid, var)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain), intent(inout) :: grid
   real,         intent(inout) :: var(ims:ime,jms:jme,kms:kme)

   integer :: i, j, k
   real    :: w(3)

   data  w/0.25,0.5,0.25/

   if (trace_use) call da_trace_entry("da_filter")

   ! Copy var for transpose.

   grid%xp%v1z(its:ite,jts:jte,kts:kte) = &
      var(its:ite,jts:jte,kts:kte)

   ! Apply (i',j',k -> i,j',k') transpose (v1z -> v1x).

   call da_transpose_z2x (grid)

   ! Perform x-direction filter:

   do k = grid%xp%ktsx, grid%xp%ktex
      do j=grid%xp%jtsx, grid%xp%jtex
         ! Forward 
         do i=ids+1, ide-1
            grid%xp%v1x(i,j,k) = w(1)*grid%xp%v1x(i-1,j,k) + w(2)*grid%xp%v1x(i,j,k) + &
               w(3)*grid%xp%v1x(i+1,j,k)
         end do

         ! Backward
         do i=ide-1,ids+1,-1
            grid%xp%v1x(i,j,k) = w(1)*grid%xp%v1x(i-1,j,k) + w(2)*grid%xp%v1x(i,j,k) + &
               w(3)*grid%xp%v1x(i+1,j,k)
         end do
      end do
   end do


   ! Apply (i,j',k' -> i',j,k') transpose (v1x -> v1y).

   call da_transpose_x2y (grid)

   ! Perform y-direction filter:

   do k=grid%xp%ktsy, grid%xp%ktey
      do i=grid%xp%itsy, grid%xp%itey
         ! Forward
         do j=jds+1, jde-1
            grid%xp%v1y(i,j,k) = w(1)*grid%xp%v1y(i,j-1,k) + w(2)*grid%xp%v1y(i,j,k) + &
               w(3)*grid%xp%v1y(i,j+1,k)
         end do

         ! Backward
         do j=jde-1,jds+1,-1
            grid%xp%v1y(i,j,k) = w(1)*grid%xp%v1y(i,j-1,k) + w(2)*grid%xp%v1y(i,j,k) + &
               w(3)*grid%xp%v1y(i,j+1,k)
         end do
      end do
   end do

   ! Apply (i',j,k' -> i',j',k) transpose (v1y -> v1z).

   call da_transpose_y2z (grid)

   var(its:ite,jts:jte,kts:kte) = grid%xp%v1z(its:ite,jts:jte,kts:kte)

   if (trace_use) call da_trace_exit("da_filter")

end subroutine da_filter


