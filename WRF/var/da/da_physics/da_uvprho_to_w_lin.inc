subroutine da_uvprho_to_w_lin(grid)

   !------------------------------------------------------------------------------
   !  Purpose: Calculates vertical velocity increments from Richardson's Eq.
   !
   !  Method: Richardson's Eq., which
   !          combines continuity Eq., thermodynamic Eq. and hrdrostatic Eq.
   !------------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout) :: grid

   integer :: is, ie       ! 1st dim. end points.
   integer :: js, je       ! 2nd dim. end points.

   integer :: I,J,K

   real    :: urho(ims:ime,jms:jme,kms:kme)
   real    :: vrho(ims:ime,jms:jme,kms:kme)
   real    :: div(ims:ime,jms:jme,kms:kme)
   real    :: wz(ims:ime,jms:jme,kms:kme)
   real    :: term3

   if (trace_use) call da_trace_entry("da_uvprho_to_w_lin")
   
   ! Computation to check for edge of domain:
   is = its
   ie = ite
   js = jts
   je = jte
   if (its == ids) is = ids+1
   if (ite == ide) ie = ide-1
   if (jts == jds) js = jds+1
   if (jte == jde) je = jde-1

   WZ(:,:,:) = 0.0
   ! Term 1.1: perturbed pressure advection along the basic wind
   do K=kts,kte
      do J=js,je
         do I=is,ie
            WZ(I,J,K)=WZ(I,J,K)-grid%xb%u(I,J,K)*(grid%xa%p(I+1,J,K)-grid%xa%p(I-1,J,K))* &
               grid%xb%coefx(I,J)
            WZ(I,J,K)=WZ(I,J,K)-grid%xb%v(I,J,K)*(grid%xa%p(I,J+1,K)-grid%xa%p(I,J-1,K))* &
               grid%xb%coefy(I,J)
         end do
      end do
   end do

   ! Term 1.2: Basic pressure advection along the perturbed wind

   do K=kts,kte
      do J=js,je
         do I=is,ie
            WZ(I,J,K)=WZ(I,J,K)-grid%xa%u(I,J,K)*(grid%xb%p(I+1,J,K)-grid%xb%p(I-1,J,K))* &
               grid%xb%coefx(I,J)
            WZ(I,J,K)=WZ(I,J,K)-grid%xa%v(I,J,K)*(grid%xb%p(I,J+1,K)-grid%xb%p(I,J-1,K))* &
               grid%xb%coefy(I,J)
         end do
      end do
   end do

   ! Dealing the laterial boundary because of the advection.
   ! boundary too simple? (It is the same as fill in interpf, fill can be used)

   if (its == ids) then
      do K=kts,kte
         do J=js,je
            WZ(its,J,K)=WZ(its+1,J,K)
         end do
      end do
   end if

   if (ite == ide) then
      do K=kts,kte
         do J=js,je
            WZ(ite,J,K)=WZ(ite-1,J,K)
         end do
      end do
   end if

   if (jts == jds) then
      do K=kts,kte
         do I=its, ite
            WZ(I,jts,K)=WZ(I,jts+1,K)       
         end do
      end do
   end if

   if (jte == jde) then
      do K=kts,kte
         do I=its, ite
            WZ(I,jte,K)=WZ(I,jte-1,K)
         end do
      end do
   end if

   ! Term 2.1: Divergence term from perturbed wind

   call da_uv_to_divergence(grid%xb, grid%xa%u, grid%xa%v, DIV)

   WZ(its:ite,jts:jte,kts:kte)=WZ(its:ite,jts:jte,kts:kte)-GAMMA*grid%xb%p(its:ite,jts:jte,kts:kte)*DIV(its:ite,jts:jte,kts:kte)

   ! Term 2.2: Divergence term from basic wind

   call da_uv_to_divergence(grid%xb, grid%xb%u, grid%xb%v, DIV)

   WZ(its:ite,jts:jte,kts:kte)=WZ(its:ite,jts:jte,kts:kte)-GAMMA*grid%xa%p(its:ite,jts:jte,kts:kte)*DIV(its:ite,jts:jte,kts:kte)

   ! Computation to check for edge of domain:
   is = its-1; ie = ite+1; js = jts-1; je = jte+1
   if (its == ids) is = ids; if (ite == ide) ie = ide
   if (jts == jds) js = jds; if (jte == jde) je = jde

   ! Term 3.1: Vertical integration of the perturbed mass divergence

   URHO(is:ie,js:je,kts:kte)=grid%xb%rho(is:ie,js:je,kts:kte)*grid%xa%u(is:ie,js:je,kts:kte)
   VRHO(is:ie,js:je,kts:kte)=grid%xb%rho(is:ie,js:je,kts:kte)*grid%xa%v(is:ie,js:je,kts:kte)

   call da_uv_to_divergence(grid%xb, URHO, VRHO, DIV)

   do J=jts,jte
      do I=its,ite
         TERM3=0.0

         do K=kte-1,kts,-1
            TERM3=TERM3+GRAVITY*(DIV(I,J,K+1)+DIV(I,J,K))*0.5 *(grid%xb%h(I,J,K+1)-grid%xb%h(I,J,K))
            WZ(I,J,K)=WZ(I,J,K)+TERM3
         end do
      end do
   end do

   ! Term 3.2: Vertical integration of the basic mass divergence

   URHO(is:ie,js:je,kts:kte)=grid%xa%rho(is:ie,js:je,kts:kte)*grid%xb%u(is:ie,js:je,kts:kte)
   VRHO(is:ie,js:je,kts:kte)=grid%xa%rho(is:ie,js:je,kts:kte)*grid%xb%v(is:ie,js:je,kts:kte)

   call da_uv_to_divergence(grid%xb, URHO, VRHO, DIV)

   do J=jts,jte
      do I=its,ite
         TERM3=0.0

         do K=kte-1,kts,-1
            TERM3=TERM3+GRAVITY*(DIV(I,J,K+1)+DIV(I,J,K))*0.5*(grid%xb%h(I,J,K+1)-grid%xb%h(I,J,K))
            WZ(I,J,K)=WZ(I,J,K)+TERM3
         end do
      end do
   end do

   ! Term 4: Derivative of basic vertical velocity with respect to z.

   do J=jts,jte
      do I=its,ite
         do K=kts,kte
            WZ(I,J,K)=WZ(I,J,K)-GAMMA*grid%xa%p(I,J,K)*(grid%xb%w(I,J,K+1)-grid%xb%w(I,J,K))/  &
               (grid%xb%hf(I,J,K+1)-grid%xb%hf(I,J,K))
         end do
      end do
   end do

   ! Divide by constant

   WZ(its:ite,jts:jte,kts:kte)=WZ(its:ite,jts:jte,kts:kte)/(GAMMA*grid%xb%p(its:ite,jts:jte,kts:kte))

   ! integration to calculate the vertical velocity 

   call da_w_adjustment_lin(grid%xb,grid%xa%w,WZ)

   do J=jts,jte
      do I=its,ite
         grid%xa%w(I,J,kte+1)=0.0
         do K=kte,kts,-1
            grid%xa%w(I,J,K)=grid%xa%w(I,J,K+1) + WZ(I,J,K)*(grid%xb%hf(I,J,K)-grid%xb%hf(I,J,K+1))
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_uvprho_to_w_lin")

end subroutine da_uvprho_to_w_lin


