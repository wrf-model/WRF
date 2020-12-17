SUBROUTINE da_apply_rf_adj( be, vp , grid )

   IMPLICIT NONE

   TYPE (be_type), INTENT(IN)       :: be     ! Background error structure.
   TYPE (vp_type), INTENT(INOUT)    :: vp     ! working array
   type (domain) , intent(inout)    :: grid   ! Dimensions and xpose buffers.

   integer             :: i, j, k
   integer             :: in, jn, kn

!-------------------------------------------------------------------------

#ifndef DEREF_KLUDGE
   call da_apply_rf_1v_adj( be, vp%v1, grid, 1 )
#else
   call da_apply_rf_1v_adj( be, vp%v1(ims,jms,kms), grid, 1 )
#endif
!-------------------------------------------------------------------------

#ifndef DEREF_KLUDGE
   call da_apply_rf_1v_adj( be, vp%v2, grid, 2 )
#else
   call da_apply_rf_1v_adj( be, vp%v2(ims,jms,kms), grid, 2 )
#endif

!-------------------------------------------------------------------------

#ifndef DEREF_KLUDGE
   call da_apply_rf_1v_adj( be, vp%v3, grid, 3 )
#else
   call da_apply_rf_1v_adj( be, vp%v3(ims,jms,kms), grid, 3 )
#endif

!-------------------------------------------------------------------------

#ifndef DEREF_KLUDGE
   call da_apply_rf_1v_adj( be, vp%v4, grid, 4 )
#else
   call da_apply_rf_1v_adj( be, vp%v4(ims,jms,kms), grid, 4 )
#endif

!-------------------------------------------------------------------------
!  [1.0] Perform 1D recursive filter in x-y direction:
!-------------------------------------------------------------------------

   grid%xp % v1z(its:ite,jts:jte,1) = vp%v5(its:ite,jts:jte,1)

   call da_transpose_z2y ( grid )

   in=grid%xp%ipey-grid%xp%ipsy
   jn=grid%xp%jpey-grid%xp%jpsy
 
   if ( LBOUND(grid%xp%v1y,3) == 1 ) then
      call smoothy(in,jn, &
           grid%xp%v1y(grid%xp%ipsy:grid%xp%ipey,grid%xp%jpsy:grid%xp%jpey,1),&
           be%sljpy(grid%xp%ipsy:grid%xp%ipey,grid%xp%jpsy:grid%xp%jpey),&
           be%ndeg,be%be,be%nta,be%swidth,be%table)
   endif

   call da_transpose_y2x ( grid )

   in=grid%xp%ipex-grid%xp%ipsx
   jn=grid%xp%jpex-grid%xp%jpsx

   if ( LBOUND(grid%xp%v1x,3) == 1 ) then
      call smoothx(in,jn,&
           grid%xp%v1x(grid%xp%ipsx:grid%xp%ipex,grid%xp%jpsx:grid%xp%jpex,1),&
           be%slipx(grid%xp%ipsx:grid%xp%ipex,grid%xp%jpsx:grid%xp%jpex),&
           be%ndeg,be%be,be%nta,be%swidth,be%table)
   endif

   call da_transpose_x2z ( grid )

   vp%v5(its:ite,jts:jte,1)= grid%xp % v1z(its:ite,jts:jte,1)

!-------------------------------------------------------------------------

END SUBROUTINE da_apply_rf_adj      

