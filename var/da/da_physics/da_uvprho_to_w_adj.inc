subroutine da_uvprho_to_w_adj(grid)

   !---------------------------------------------------------------------------
   ! Purpose: Calculates vertical velocity increments from Richardson's Eq.
   !
   ! Method: Richardson's Eq., which
   !         combines continuity Eq., thermodynamic Eq. and hrdrostatic Eq.
   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)     :: grid

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.

   integer                       :: I,J,K

   real, dimension(ims:ime,jms:jme,kms:kme) :: URHO, VRHO
   real, dimension(ims:ime,jms:jme,kms:kme) :: DIV, WZ
   real                                     :: TERM3

   if (trace_use) call da_trace_entry("da_uvprho_to_w_adj")

   ! initialize to zero for some variables because of the adjoint requirements.

   WZ(:,:,:)   = 0.0
   URHO(:,:,:) = 0.0
   VRHO(:,:,:) = 0.0
   DIV(:,:,:)  = 0.0
   TERM3       = 0.0

   ! integration to calculate the vertical velocity

   do J=jts,jte
      do I=its,ite
         do K=kts,kte
            grid%xa%w(I,J,K+1)=grid%xa%w(I,J,K+1)+grid%xa%w(I,J,K)
            WZ(I,J,K)=grid%xa%w(I,J,K)*(grid%xb%hf(I,J,K)-grid%xb%hf(I,J,K+1))
            grid%xa%w(I,J,K)=0.0
         end do
         grid%xa%w(I,J,kte+1)=0.0
      end do
   end do

   call da_w_adjustment_adj(grid%xb,WZ)

   ! Divide by constant

   WZ(its:ite,jts:jte,kts:kte)=WZ(its:ite,jts:jte,kts:kte)/(GAMMA*grid%xb%p(its:ite,jts:jte,kts:kte))

   ! Term 4: Derivative of basic vertical velocity with respect to z.

   do J=jts,jte
      do I=its,ite
         do K=kte,kts,-1
            grid%xa%p(I,J,K)=grid%xa%p(I,J,K)-WZ(I,J,K)*GAMMA*  &
                     (grid%xb%w(I,J,K+1)-grid%xb%w(I,J,K))/  &
                     (grid%xb%hf(I,J,K+1)-grid%xb%hf(I,J,K))
         end do
      end do
   end do

   ! Term 3.2: Vertical integration of the basic mass divergence

   do J=jts,jte
      do I=its,ite
         do K=kts,kte-1
            TERM3=TERM3+WZ(I,J,K)
            DIV(I,J,K+1)=DIV(I,J,K+1)+  &
                      TERM3*GRAVITY*0.5*(grid%xb%h(I,J,K+1)-grid%xb%h(I,J,K))
            DIV(I,J,K)  =DIV(I,J,K)+  &
                      TERM3*GRAVITY*0.5*(grid%xb%h(I,J,K+1)-grid%xb%h(I,J,K))
         end do
        TERM3=0.0
      end do
   end do

   call da_uv_to_divergence_adj(grid, URHO,VRHO, DIV)

   ! Computation to check for edge of domain:
   if (test_transforms) then
      is = its-1; ie = ite+1; js = jts-1; je = jte+1
      if (its == ids) is = ids; if (ite == ide) ie = ide
      if (jts == jds) js = jds; if (jte == jde) je = jde
   else
      is = its
      ie = ite
      js = jts
      je = jte
   end if

   grid%xa%rho(is:ie,js:je,kts:kte)=grid%xa%rho(is:ie,js:je,kts:kte)+VRHO(is:ie,js:je,kts:kte)*grid%xb%v(is:ie,js:je,kts:kte)
   grid%xa%rho(is:ie,js:je,kts:kte)=grid%xa%rho(is:ie,js:je,kts:kte)+URHO(is:ie,js:je,kts:kte)*grid%xb%u(is:ie,js:je,kts:kte)

   URHO(:,:,:)=0.0
   VRHO(:,:,:)=0.0

   ! Term 3.1: Vertical integration of the perturbed mass divergence

   do J=jts,jte
      do I=its,ite
         do K=kts,kte-1
            TERM3=TERM3+WZ(I,J,K)
            DIV(I,J,K+1)=DIV(I,J,K+1)+  &
                      TERM3*GRAVITY*0.5*(grid%xb%h(I,J,K+1)-grid%xb%h(I,J,K))
            DIV(I,J,K)  =DIV(I,J,K)+  &
                      TERM3*GRAVITY*0.5*(grid%xb%h(I,J,K+1)-grid%xb%h(I,J,K))
         end do
         TERM3=0.0
      end do
   end do

   call da_uv_to_divergence_adj(grid, URHO,VRHO, DIV)

   grid%xa%v(is:ie,js:je,kts:kte)=grid%xa%v(is:ie,js:je,kts:kte)+VRHO(is:ie,js:je,kts:kte)*grid%xb%rho(is:ie,js:je,kts:kte)
   grid%xa%u(is:ie,js:je,kts:kte)=grid%xa%u(is:ie,js:je,kts:kte)+URHO(is:ie,js:je,kts:kte)*grid%xb%rho(is:ie,js:je,kts:kte)

   URHO(:,:,:)=0.0
   VRHO(:,:,:)=0.0

   ! Term 2.2: Divergence term from basic wind

   call da_uv_to_divergence(grid%xb, grid%xb%u,grid%xb%v, DIV)

   grid%xa%p(its:ite,jts:jte,kts:kte)=grid%xa%p(its:ite,jts:jte,kts:kte)-WZ(its:ite,jts:jte,kts:kte)*GAMMA*DIV(its:ite,jts:jte,kts:kte)
   
   ! Term 2.1: Divergence term from perturbed wind

   DIV(its:ite,jts:jte,kts:kte)=-WZ(its:ite,jts:jte,kts:kte)*GAMMA*grid%xb%p(its:ite,jts:jte,kts:kte)  ! DIV redefined

   call da_uv_to_divergence_adj(grid, grid%xa%u,grid%xa%v, DIV)

   ! Computation to check for edge of domain:
   is = its
   ie = ite
   js = jts
   je = jte
   if (its == ids) is = ids+1
   if (ite == ide) ie = ide-1
   if (jts == jds) js = jds+1
   if (jte == jde) je = jde-1

   ! Term 1.2: Basic pressure advection along the perturbed wind

   if (jte == jde) then
      j = jte
      do K=kts,kte
         do I=its, ite
            WZ(I,J-1,K)=WZ(I,J-1,K)+WZ(I,J,K)
         end do
      end do
   end if

   if (jts == jds) then
      j = jts
      do K=kts,kte
         do I=its, ite
            WZ(I,J+1,K)=WZ(I,J+1,K)+WZ(I,J,K)
         end do
      end do
   end if

   if (ite == ide) then
      i = ite
      do K=kts,kte
         do J=js,je
            WZ(I-1,J,K)=WZ(I-1,J,K)+WZ(I,J,K)
         end do
      end do
   end if

   if (its == ids) then
      i = its
      do K=kts,kte
         do J=js,je
            WZ(I+1,J,K)=WZ(I+1,J,K)+WZ(I,J,K)
         end do
      end do
   end if

   do K=kts,kte
      do J=js,je
         do I=is,ie
            grid%xa%v(I,J,K)=grid%xa%v(I,J,K)-WZ(I,J,K)*  &
               (grid%xb%p(I,J+1,K)-grid%xb%p(I,J-1,K))*grid%xb%coefy(I,J)
            grid%xa%u(I,J,K)=grid%xa%u(I,J,K)-WZ(I,J,K)*  &
               (grid%xb%p(I+1,J,K)-grid%xb%p(I-1,J,K))*grid%xb%coefx(I,J)
         end do
      end do
   end do

   !-------------------------------------------------------------------------
   ! Computation to check for edge of domain:
   ! This is only for adjoint, as we have to cross the processor boundary
   ! to get the contribution.

   is = its - 1
   ie = ite + 1
   js = jts - 1
   je = jte + 1

   grid%xp%v1z(its:ite, jts:jte, kts:kte) = wz(its:ite, jts:jte, kts:kte)

#ifdef DM_PARALLEL
#include "HALO_BAL_EQN_ADJ.inc"
#endif

   if (its == ids) then
      is = ids+1
   else
      wz(is, js:je, kts:kte) = grid%xp%v1z(is, js:je, kts:kte)
   end if

   if (ite == ide) then
      ie = ide-1
   else
      wz(ie, js:je, kts:kte) = grid%xp%v1z(ie, js:je, kts:kte)
   end if

   if (jts == jds) then
      js = jds+1
   else
      wz(is:ie, js, kts:kte) = grid%xp%v1z(is:ie, js, kts:kte)
   end if

   if (jte == jde) then
      je = jde-1
   else
      wz(is:ie, je, kts:kte) = grid%xp%v1z(is:ie, je, kts:kte)
   end if

   ! Term 1.1: Perturbed pressure advection along the basic wind

   do K=kts,kte
      do J=js,je
         do I=is,ie
            grid%xa%p(I,J+1,K)=grid%xa%p(I,J+1,K)-WZ(I,J,K)*grid%xb%v(I,J,K)*grid%xb%coefy(I,J)
            grid%xa%p(I,J-1,K)=grid%xa%p(I,J-1,K)+WZ(I,J,K)*grid%xb%v(I,J,K)*grid%xb%coefy(I,J)
            grid%xa%p(I+1,J,K)=grid%xa%p(I+1,J,K)-WZ(I,J,K)*grid%xb%u(I,J,K)*grid%xb%coefx(I,J)
            grid%xa%p(I-1,J,K)=grid%xa%p(I-1,J,K)+WZ(I,J,K)*grid%xb%u(I,J,K)*grid%xb%coefx(I,J)
         end do
      end do
   end do

   WZ(:,:,:) = 0.0

   if (trace_use) call da_trace_exit("da_uvprho_to_w_adj")

end subroutine da_uvprho_to_w_adj


