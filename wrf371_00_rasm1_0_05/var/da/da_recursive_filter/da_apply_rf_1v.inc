SUBROUTINE da_apply_rf_1v( be, vp, grid, nv )

   IMPLICIT NONE

   TYPE (be_type), INTENT(IN)       :: be     ! Background error structure.
   type (domain) , intent(inout)    :: grid   ! Dimensions and xpose buffers.

   integer, intent(in) :: nv                        ! # of var.

   real, dimension(ims:ime, jms:jme, kms:kme), INTENT(INOUT) :: vp   ! working array

   integer                 :: in, jn, kn, k
!-------------------------------------------------------------------------
!  [1.0] Initialise:
!-------------------------------------------------------------------------

   in=ite-its+1
   jn=jte-jts+1
   kn=kte-kts+1

   call da_rfz(vp(ims:ime,jms:jme,kms:kme),in,jn,kn,be%ndeg,&
     be%vz(kts:kte,its:ite,jts:jte,nv),be%be,be%table,be%nta,be%swidth, &
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )

!-------------------------------------------------------------------------
!  [2.0]: Perform 1D recursive filter in y-x direction:
!-------------------------------------------------------------------------

   do k = kts,kte
      grid%xp % v1z(its:ite,jts:jte,k) = vp(its:ite,jts:jte,k)
   end do

   call da_transpose_z2x ( grid )

   in=grid%xp%ipex-grid%xp%ipsx
   jn=grid%xp%jpex-grid%xp%jpsx

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( k )
   do k=grid%xp%kpsx,grid%xp%kpex
     call smoothx(in,jn, &
          grid%xp% v1x(grid%xp%ipsx:grid%xp%ipex,grid%xp%jpsx:grid%xp%jpex,k),&
          be%slix(grid%xp%ipsx:grid%xp%ipex,grid%xp%jpsx:grid%xp%jpex,k,nv),  &
          be%ndeg,be%be,be%nta,be%swidth,be%table)
   enddo
   !$OMP END PARALLEL DO


   call da_transpose_x2y ( grid )

   in=grid%xp%ipey-grid%xp%ipsy
   jn=grid%xp%jpey-grid%xp%jpsy

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( k )
   do k=grid%xp%kpsy,grid%xp%kpey
     call smoothy(in,jn, &
          grid%xp%v1y(grid%xp%ipsy:grid%xp%ipey,grid%xp%jpsy:grid%xp%jpey,k),&
          be%sljy(grid%xp%ipsy:grid%xp%ipey,grid%xp%jpsy:grid%xp%jpey,k,nv), &
          be%ndeg,be%be,be%nta,be%swidth,be%table)
   enddo
   !$OMP END PARALLEL DO


   call da_transpose_y2z ( grid )

   do k = kts,kte
      vp(its:ite,jts:jte,k)= grid%xp % v1z(its:ite,jts:jte,k)
   end do

END SUBROUTINE da_apply_rf_1v

