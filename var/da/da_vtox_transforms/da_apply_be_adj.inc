SUBROUTINE da_apply_be_adj( be, cv, vp, grid )

   IMPLICIT NONE

   TYPE (be_type), INTENT(IN)           :: be     ! Background error structure.
   REAL, INTENT(INOUT)                  :: cv(:)  ! Control variable.
   TYPE (vp_type), INTENT(INOUT)        :: vp     ! Grid point/EOF equivalent.
   type (domain), intent(inout)         :: grid   ! Dimensions and xpose buffers.

   INTEGER                              :: i,j,k,ijk,ij,iunit

!-------------------------------------------------------------------------
!  [2.0] Transform from cv to vp:
!-------------------------------------------------------------------------

   CALL da_apply_rf_adj( be, vp, grid )

!  [2.1] Transform control variable:
!!!!!!!!!!!!!!!!!!!!!!!

   do k=kts,kte
   do j=jts,jte
   do i=its,ite
      vp % v1(i,j,k)=vp % v1(i,j,k)*be % corz(i,j,k,1)
      vp % v2(i,j,k)=vp % v2(i,j,k)*be % corz(i,j,k,2)
      vp % v3(i,j,k)=vp % v3(i,j,k)*be % corz(i,j,k,3)
      vp % v4(i,j,k)=vp % v4(i,j,k)*be % corz(i,j,k,4)
   enddo
   enddo         
   enddo         

!-----Transform 5th control variable
      k=1
   do j=jts,jte
      do i=its,ite
         vp % v5(i,j,k)=vp % v5(i,j,k)*be % corp(i,j)
      enddo
   enddo         

!-------------------------------------------------------------------------
!  [1.0] Make global-grid copy of cc from 3-dimensional local-grid vv.
!-------------------------------------------------------------------------

   call da_vv_to_cv( vp, grid%xp, be%cv_mz, be%ncv_mz, cv_size, cv )

END SUBROUTINE da_apply_be_adj

