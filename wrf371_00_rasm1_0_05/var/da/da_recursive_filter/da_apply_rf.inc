SUBROUTINE da_apply_rf( be, vp, grid )

   IMPLICIT NONE

   TYPE (be_type), INTENT(IN)       :: be   ! Background error structure.
   TYPE (vp_type), INTENT(INOUT)    :: vp   ! working array
   type (domain) , intent(inout)    :: grid   ! Dimensions and xpose buffers.

   integer :: in, jn

   integer :: i, j, k

!-------------------------------------------------------------------------

#ifndef DEREF_KLUDGE
   call da_apply_rf_1v( be, vp%v1, grid, 1)
#else
   call da_apply_rf_1v( be, vp%v1(ims,jms,kms), grid, 1)
#endif

!-------------------------------------------------------------------------

#ifndef DEREF_KLUDGE
   call da_apply_rf_1v( be, vp%v2, grid, 2)
#else
   call da_apply_rf_1v( be, vp%v2(ims,jms,kms), grid, 2)
#endif

!-------------------------------------------------------------------------

#ifndef DEREF_KLUDGE
   call da_apply_rf_1v( be, vp%v3, grid, 3)
#else
   call da_apply_rf_1v( be, vp%v3(ims,jms,kms), grid, 3)
#endif

!-------------------------------------------------------------------------

#ifndef DEREF_KLUDGE
   call da_apply_rf_1v( be, vp%v4, grid, 4)
#else
   call da_apply_rf_1v( be, vp%v4(ims,jms,kms), grid, 4)
#endif

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  [2.0]: Perform 1D recursive filter in y-x direction:
!-------------------------------------------------------------------------

   grid%xp%v1z(its:ite,jts:jte,1) = vp%v5(its:ite,jts:jte,1)

   call da_transpose_z2x ( grid )

   in=grid%xp%ipex-grid%xp%ipsx
   jn=grid%xp%jpex-grid%xp%jpsx

   if ( LBOUND(grid%xp%v1x,3) == 1 ) then
      call smoothx(in,jn,&
           grid%xp%v1x(grid%xp%ipsx:grid%xp%ipex,grid%xp%jpsx:grid%xp%jpex,1),&
           be%slipx(grid%xp%ipsx:grid%xp%ipex,grid%xp%jpsx:grid%xp%jpex),&
           be%ndeg,be%be,be%nta,be%swidth,be%table)
   endif

   call da_transpose_x2y ( grid )

   in=grid%xp%ipey-grid%xp%ipsy
   jn=grid%xp%jpey-grid%xp%jpsy

   if ( LBOUND(grid%xp%v1y,3) == 1 ) then
      call smoothy(in,jn, &
           grid%xp%v1y(grid%xp%ipsy:grid%xp%ipey,grid%xp%jpsy:grid%xp%jpey,1),&
           be%sljpy(grid%xp%ipsy:grid%xp%ipey,grid%xp%jpsy:grid%xp%jpey),&
           be%ndeg,be%be,be%nta,be%swidth,be%table)
   endif

   call da_transpose_y2z ( grid )

   vp%v5(its:ite,jts:jte,1)= grid%xp % v1z(its:ite,jts:jte,1)

!-------------------------------------------------------------------------

END SUBROUTINE da_apply_rf          

