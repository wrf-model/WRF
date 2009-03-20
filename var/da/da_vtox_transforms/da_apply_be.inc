SUBROUTINE da_apply_be( be, cv, vp, grid )

   IMPLICIT NONE

   TYPE (be_type), INTENT(IN)           :: be   ! Background error structure.
   REAL, INTENT(IN)                     :: cv(:)! Control variable.
   TYPE (vp_type), INTENT(INOUT)        :: vp   ! Grid point/EOF equivalent.
   type (domain) , intent(inout) :: grid   ! Dimensions and xpose buffers.

   INTEGER                              :: i,j,k

!-------------------------------------------------------------------------
!  [1.0] Make local-grid copy of vp from 1-dimensional global-grid cv.
!-------------------------------------------------------------------------

   call da_cv_to_vv( cv_size, cv,&
           (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%ne /), vp )

!  [2.0] Transform control variable:

      do k=grid%xp%kts,grid%xp%kte
      do j=grid%xp%jts,grid%xp%jte
      do i=grid%xp%its,grid%xp%ite
         vp % v1(i,j,k)=vp % v1(i,j,k)*be % corz(i,j,k,1)
         vp % v2(i,j,k)=vp % v2(i,j,k)*be % corz(i,j,k,2)
         vp % v3(i,j,k)=vp % v3(i,j,k)*be % corz(i,j,k,3)
         vp % v4(i,j,k)=vp % v4(i,j,k)*be % corz(i,j,k,4)
      enddo
      enddo
      enddo

!-----Transform 5th control variable
      k=1
      do j=grid%xp%jts,grid%xp%jte
      do i=grid%xp%its,grid%xp%ite
         vp % v5(i,j,k)=vp % v5(i,j,k)*be % corp(i,j)
      enddo
      enddo

   CALL da_apply_rf( be, vp , grid )

END SUBROUTINE da_apply_be          

