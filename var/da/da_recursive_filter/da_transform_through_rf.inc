subroutine da_transform_through_rf(grid,mz, rf_alpha, val,field)

   !---------------------------------------------------------------------------
   ! Purpose: Control variable transform through the recursive filter.  
   !
   ! Method: 1) Transform to nondimensional v_hat space.
   !         2) Perform recursive filter in non-dimensional space (ds = 1).
   !         3) Scale recursive filter output.
   !         4) Transform filtered field to dimensional space.
   !         5) Optionally scale by background error.
   !---------------------------------------------------------------------------

   implicit none

   type(domain),     intent(inout) :: grid
   integer,          intent(in)    :: mz                ! Vertical truncation.
   real,             intent(in)    :: rf_alpha(mz)      ! RF scale parameter. 
   real,             intent(in)    :: val(jds:jde,mz)   ! Error standard deviation.
   real,             intent(inout) :: field(ims:ime,jms:jme,kms:kme) ! Field to be transformed. 
      
   integer :: rf_passes_over_two ! rf_passes / 2
   integer :: i, j, m, n, pass   ! Loop counters.
   real    :: p_x(ims:ime,jms:jme)! sqrt(Grid box area).
   real    :: val_j(grid%xp%jtsy:grid%xp%jtey)

   !----------------------------------------------------------------------
   ! [1.0]: Initialise:
   !----------------------------------------------------------------------  

   if (trace_use_dull) call da_trace_entry("da_transform_through_rf")

   rf_passes_over_two = rf_passes / 2
   
   ! [1.1] Define inner product (square root of grid box area):
   p_x(its:ite,jts:jte) = sqrt(grid%xb%grid_box_area(its:ite,jts:jte))

   grid%xp%v1z(:,:,:) = 0.0

   ! [1.2] Transform to nondimensional v_hat space:

   do m = 1, mz
      grid%xp%v1z(its:ite,jts:jte,m) = field(its:ite,jts:jte,m) / p_x(its:ite,jts:jte)
   end do

   !-------------------------------------------------------------------------
   ! [2.0]: Perform 1D recursive filter in x-direction:
   !-------------------------------------------------------------------------

   ! [2.1] Apply (i',j',k -> i,j',k') (grid%xp%v1z -> grid%xp%v1x)
   ! convert from vertical column to x-stripe

   call da_transpose_z2x (grid)

   ! [2.2] Apply 1D filter in x direction:

   n = grid%xp%itex - grid%xp%itsx + 1
   do m = grid%xp%ktsx, min(grid%xp%ktex,mz)
      do j = grid%xp%jtsx, grid%xp%jtex
         do pass = 1, rf_passes_over_two
            call da_recursive_filter_1d(pass, rf_alpha(m), grid%xp%v1x(grid%xp%itsx:grid%xp%itex,j,m), n)
         end do
      end do
   end do

   !-------------------------------------------------------------------------
   ! [3.0]: Perform 1D recursive filter in y-direction:
   !-------------------------------------------------------------------------

   ! [3.1] Apply (i, j' ,k' -> i', j ,k') (grid%xp%v1x -> grid%xp%v1y)
   ! convert from vertical column to y-stripe

   call da_transpose_x2y (grid)

   ! [3.2] Apply 1D filter in y direction:

   n = grid%xp%jtey - grid%xp%jtsy + 1
   do m = grid%xp%ktsy, min(grid%xp%ktey,mz)
      do i = grid%xp%itsy, grid%xp%itey
         val_j(grid%xp%jtsy:grid%xp%jtey) = grid%xp%v1y(i,grid%xp%jtsy:grid%xp%jtey,m)
         do pass = 1, rf_passes_over_two
            call da_recursive_filter_1d(pass, rf_alpha(m), val_j, n)
         end do
         grid%xp%v1y(i,grid%xp%jtsy:grid%xp%jtey,m) = val_j(grid%xp%jtsy:grid%xp%jtey)
      end do
   end do
   
   !-------------------------------------------------------------------------
   ! [4.0]: Perform 1D recursive filter in y-direction:
   !-------------------------------------------------------------------------

   ! [4.1] Apply (i',j,k' -> i',j',k) (grid%xp%v1y -> grid%xp%v1z)
   ! convert from y-stripe to vertical column.
   
   call da_transpose_y2z (grid)

   ! [4.2] Transform filtered field to dimensional space:

   do m = 1, mz
      field(its:ite,jts:jte,m) = grid%xp%v1z(its:ite,jts:jte,m) * p_x(its:ite,jts:jte)
   end do

   ! [4.3] Optionally scale by background error:

   ! be_s % val = Gridpoint standard deviation - only required for
   ! vert_corr = vert_corr_1 as scaling is performed in vertical transform
   ! for vert_corr = vert_corr_2:

   if (vert_corr == vert_corr_1) then
      do m = 1, mz
         do i = its, ite
            field(i,jts:jte,m) = field(i,jts:jte,m) * val(jts:jte,m)
         end do
      end do
   end if

   if (trace_use_dull) call da_trace_exit("da_transform_through_rf")

end subroutine da_transform_through_rf


