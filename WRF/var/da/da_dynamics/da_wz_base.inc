subroutine da_wz_base(xb,WZ_b)

   !--------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)    :: xb                ! First guess structure.

   real, dimension(ims:ime,jms:jme,kms:kme), intent(inout) :: WZ_b

   integer                       :: is, ie       ! 1st dim. end points.
   integer                       :: js, je       ! 2nd dim. end points.

   integer                       :: I,J,K

   real  ::  TERM3

   real, dimension(ims:ime,jms:jme,kms:kme) :: URHO, VRHO
   real, dimension(ims:ime,jms:jme,kms:kme) :: DIV

   if (trace_use) call da_trace_entry("da_wz_base")


   ! Computation to check for edge of domain:

   is = its
   ie = ite
   js = jts
   je = jte
   if (its == ids) is = ids+1
   if (ite == ide) ie = ide-1
   if (jts == jds) js = jds+1
   if (jte == jde) je = jde-1


   do K=kts,kte
      do J=js,je
         do I=is,ie
            WZ_b(I,J,K)=-xb%u(I,J,K)*(xb%p(I+1,J,K)-xb%p(I-1,J,K))*xb%coefx(I,J)
         end do
      end do

      do J=js,je
         do I=is,ie
            WZ_b(I,J,K)=WZ_b(I,J,K)-xb%v(I,J,K)*(xb%p(I,J+1,K)-xb%p(I,J-1,K))*xb%coefy(I,J)
         end do
      end do
   end do

   if (its == ids) then
      i = its
      do K=kts,kte
         do J=js,je
            WZ_b(I,J,K)=WZ_b(I+1,J,K)
         end do
      end do
   end if

   if (ite == ide) then
      i = ite
      do K=kts,kte
         do J=js,je
            WZ_b(I,J,K)=WZ_b(I-1,J,K)
         end do
      end do
   end if

   if (jts == jds) then
      j = jts
      do K=kts,kte
         do I=its, ite
            WZ_b(I,J,K)=WZ_b(I,J+1,K)
         end do
      end do
   end if

   if (jte == jde) then
      j = jte
      do K=kts,kte
         do I=its, ite
            WZ_b(I,J,K)=WZ_b(I,J-1,K)
         end do
      end do
   end if


   call da_uv_to_divergence(xb, xb%u,xb%v, DIV)

   do K=kts,kte
      do J=jts,jte
         do I=its,ite
            WZ_b(I,J,K)=WZ_b(I,J,K)-GAMMA*xb%p(I,J,K)*DIV(I,J,K)
         end do
      end do
   end do


   ! Computation to check for edge of domain:
   is = its-1; ie = ite+1; js = jts-1; je = jte+1
   if (its == ids) is = ids; if (ite == ide) ie = ide
   if (jts == jds) js = jds; if (jte == jde) je = jde

   do K=kts,kte
      do J=js,je
         do I=is,ie
            URHO(I,J,K)=xb%rho(I,J,K)*xb%u(I,J,K)
            VRHO(I,J,K)=xb%rho(I,J,K)*xb%v(I,J,K)
         end do
      end do
   end do

   call da_uv_to_divergence(xb, URHO, VRHO, DIV)

   do J=jts,jte
      do I=its,ite
         TERM3=0.0

         do K=kte-1,kts,-1
            TERM3=TERM3+GRAVITY*(DIV(I,J,K+1)+DIV(I,J,K))*0.5  &
                       *(xb%h(I,J,K+1)-xb%h(I,J,K))
            WZ_b(I,J,K)=WZ_b(I,J,K)+TERM3
         end do
      end do
   end do


   do K=kts,kte
      do J=jts,jte
         do I=its,ite
            WZ_b(I,J,K)=WZ_b(I,J,K)/(GAMMA*xb%p(I,J,K))
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_wz_base")

end subroutine da_wz_base


