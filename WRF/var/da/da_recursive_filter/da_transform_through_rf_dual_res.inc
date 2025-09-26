subroutine da_transform_through_rf_dual_res(grid,mz, rf_alpha, val,field, scaling)

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
   real*8,           intent(in)    :: rf_alpha(mz)      ! RF scale parameter. 
   real*8,           intent(in)    :: val(jds_int:jde_int,mz)   ! Error standard deviation.
!  real*8,           intent(in)    :: val(1:jde_int-jds_int+1,mz)   ! Error standard deviation.
   real,             intent(inout) :: field(ims_int:ime_int,jms_int:jme_int,kms_int:kme_int) ! Field to be transformed.
      
   integer :: rf_passes_over_two ! rf_passes / 2
   integer :: i, j, m, n, pass, ij   ! Loop counters.
   real    :: p_x(ims_int:ime_int,jms_int:jme_int)! sqrt(Grid box area).
   real*8  :: val_j(grid%xp%jtsy:grid%xp%jtey)
   real*8  :: val_i(grid%xp%itsx:grid%xp%itex)
   
   logical, optional, intent(in) :: scaling

   !----------------------------------------------------------------------
   ! [1.0]: Initialise:
   !----------------------------------------------------------------------  

   if (trace_use_dull) call da_trace_entry("da_transform_through_rf")

   rf_passes_over_two = rf_passes / 2
   
   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, i, j )
   do ij = 1 , grid%num_tiles
      ! [1.1] Define inner product (square root of grid box area):
      do j = grid%j_start(ij), grid%j_end(ij)
         do i = its_int, ite_int
            p_x(i,j) = sqrt(grid%xb%grid_box_area(i,j))
         end do
      end do
   end do
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, m, i, j )
   do ij = 1 , grid%num_tiles
      do m = 1, mz
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = its_int, ite_int
               grid%xp%v1z(i,j,m) = 0.0
            end do
         end do
      end do
   end do
   !$OMP END PARALLEL DO

      ! [1.2] Transform to nondimensional v_hat space:

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, m, i, j )
   do ij = 1 , grid%num_tiles
      do m = 1, mz
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = its_int, ite_int
               grid%xp%v1z(i,j,m) = field(i,j,m) / p_x(i,j)
            end do
         end do
      end do
   end do
   !$OMP END PARALLEL DO

   !-------------------------------------------------------------------------
   ! [2.0]: Perform 1D recursive filter in x-direction:
   !-------------------------------------------------------------------------

   ! [2.1] Apply (i',j',k -> i,j',k') (grid%xp%v1z -> grid%xp%v1x)
   ! convert from vertical column to x-stripe

   call da_transpose_z2x (grid)

   ! [2.2] Apply 1D filter in x direction:

   n = grid%xp%itex - grid%xp%itsx + 1
   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( m, j, pass, val_i, i )
   do m = grid%xp%ktsx, min(grid%xp%ktex,mz)
      do j = grid%xp%jtsx, grid%xp%jtex
         do i = grid%xp%itsx, grid%xp%itex
            val_i(i) = grid%xp%v1x(i,j,m)
         end do
         do pass = 1, rf_passes_over_two
            call da_recursive_filter_1d(pass, rf_alpha(m), val_i, n)
         end do
         do i = grid%xp%itsx, grid%xp%itex
            grid%xp%v1x(i,j,m) = val_i(i)
         end do
      end do
   end do
   !$OMP END PARALLEL DO

   !-------------------------------------------------------------------------
   ! [3.0]: Perform 1D recursive filter in y-direction:
   !-------------------------------------------------------------------------

   ! [3.1] Apply (i, j' ,k' -> i', j ,k') (grid%xp%v1x -> grid%xp%v1y)
   ! convert from vertical column to y-stripe

   call da_transpose_x2y (grid)

   ! [3.2] Apply 1D filter in y direction:

   n = grid%xp%jtey - grid%xp%jtsy + 1
   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( m, i, pass, val_j, j )
   do m = grid%xp%ktsy, min(grid%xp%ktey,mz)
      do i = grid%xp%itsy, grid%xp%itey
         do j = grid%xp%jtsy, grid%xp%jtey
            val_j(j) = grid%xp%v1y(i,j,m)
         end do
         do pass = 1, rf_passes_over_two
            call da_recursive_filter_1d(pass, rf_alpha(m), val_j, n)
         end do
         do j = grid%xp%jtsy, grid%xp%jtey
            grid%xp%v1y(i,j,m) = val_j(j)
         end do
      end do
   end do
   !$OMP END PARALLEL DO
   
   !-------------------------------------------------------------------------
   ! [4.0]: Perform 1D recursive filter in y-direction:
   !-------------------------------------------------------------------------

   ! [4.1] Apply (i',j,k' -> i',j',k) (grid%xp%v1y -> grid%xp%v1z)
   ! convert from y-stripe to vertical column.
   
   call da_transpose_y2z (grid)

   ! [4.2] Transform filtered field to dimensional space:

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, m, i, j )
   do ij = 1 , grid%num_tiles
      do m = 1, mz
         do j = grid%j_start(ij), grid%j_end(ij)
   !     do j = jts, jte
            do i = its_int, ite_int
               field(i,j,m) = grid%xp%v1z(i,j,m) * p_x(i,j)
            end do
         end do
      end do
   end do
   !$OMP END PARALLEL DO

   ! [4.3] Optionally scale by background error:

   ! be_s % val = Gridpoint standard deviation - only required for
   ! vert_corr = vert_corr_1 as scaling is performed in vertical transform
   ! for vert_corr = vert_corr_2:

   if (vert_corr == vert_corr_1 .or. present(scaling)) then
      if (scaling .or. vert_corr == vert_corr_1) then
         do m = 1, mz
            do i = its_int, ite_int
               field(i,jts_int:jte_int,m) = field(i,jts_int:jte_int,m) * val(jts_int:jte_int,m)
            end do
         end do
      end if
   end if
   if (trace_use_dull) call da_trace_exit("da_transform_through_rf")

end subroutine da_transform_through_rf_dual_res


