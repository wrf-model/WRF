SUBROUTINE da_apply_be( be, cv, vp, grid )

   IMPLICIT NONE

   TYPE (be_type), INTENT(IN)           :: be   ! Background error structure.
   REAL, INTENT(IN)                     :: cv(:)! Control variable.
   TYPE (vp_type), INTENT(INOUT)        :: vp   ! Grid point/EOF equivalent.
   type (domain) , intent(inout) :: grid   ! Dimensions and xpose buffers.

   INTEGER                              :: i,j,k,ij

!-------------------------------------------------------------------------
!  [1.0] Make local-grid copy of vp from 1-dimensional global-grid cv.
!-------------------------------------------------------------------------

   call da_cv_to_vv( cv_size, cv, be%cv_mz, be%ncv_mz, vp )

!  [2.0] Transform control variable:

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, k, j, i )
   do ij = 1, grid%num_tiles
      do k=grid%xp%kts,grid%xp%kte
      do j=grid%j_start(ij), grid%j_end(ij)
      do i=grid%xp%its,grid%xp%ite
         vp % v1(i,j,k)=vp % v1(i,j,k)*be % corz(i,j,k,1)
         vp % v2(i,j,k)=vp % v2(i,j,k)*be % corz(i,j,k,2)
         vp % v3(i,j,k)=vp % v3(i,j,k)*be % corz(i,j,k,3)
         vp % v4(i,j,k)=vp % v4(i,j,k)*be % corz(i,j,k,4)
      enddo
      enddo
      enddo
   enddo
   !$OMP END PARALLEL DO

!-----Transform 5th control variable
      k=1
   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, i, j )
   do ij = 1, grid%num_tiles
      do j=grid%j_start(ij),grid%j_end(ij)
      do i=grid%xp%its,grid%xp%ite
         vp % v5(i,j,k)=vp % v5(i,j,k)*be % corp(i,j)
      enddo
      enddo
   enddo
   !$OMP END PARALLEL DO

   CALL da_apply_rf( be, vp , grid )

END SUBROUTINE da_apply_be          

