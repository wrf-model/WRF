subroutine da_uv_to_divergence_adj(grid, u, v, div)

   !---------------------------------------------------------------------------
   !  Purpose: Adjoint of the subroutine da_uv_to_divergence
   !                        d   U      d   V
   !           Div = m^2 *[---(---) + ---(---) ] 
   !                        dx  m      dy  M
   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout) :: grid

   real, intent(out)  :: u(ims:ime,jms:jme,kms:kme)   ! u wind comp.
   real, intent(out)  :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp.
   real, intent(inout):: div(ims:ime,jms:jme,kms:kme) ! Divergence.

   integer            :: i, j, k                      ! Loop counters.
   integer            :: is, ie                       ! 1st dim. end points.
   integer            :: js, je                       ! 2nd dim. end points.
   real               :: one_third                    ! 1/3.

   real               :: coeff, inv_2ds
   real               :: um(ims:ime,jms:jme)          ! Temp. storage of u/m.
   real               :: vm(ims:ime,jms:jme)          ! Temp. storage of v/m

   if (trace_use) call da_trace_entry("da_uv_to_divergence_adj")

   !---------------------------------------------------------------------------
   ! [1.0] Initialise:
   !---------------------------------------------------------------------------

   one_third = 1.0 / 3.0

   is = its - 1; ie = ite + 1; js = jts - 1; je = jte + 1

   if (.not. global .and. its == ids) is = ids+1
   if (.not. global .and. ite == ide) ie = ide-1
   if (jts == jds) js = jds+1; if (jte == jde) je = jde-1

   !---------------------------------------------------------------------------
   ! [2.0] Calculate divergence:
   !---------------------------------------------------------------------------

   if (.not. global) then
      inv_2ds = 0.5 / grid%xb%ds
      do k =kds, kde
         um(ims:ime,jms:jme) = 0.0
         vm(ims:ime,jms:jme) = 0.0
         ! [2.2] Impose zero divergence gradient condition at boundaries:

         ! [2.2.4] Right boundaries:

         if (jte == jde) then
            j = jte
            do i = its, ite    ! This is different to original
               div(i,j-1,k)=div(i,j-1,k)+div(i,j,k)*one_third*4.0
               div(i,j-2,k)=div(i,j-2,k)-div(i,j,k)*one_third
               div(i,j,k)=0.0
            end do
         end if

         ! [2.2.3] Left boundaries:

         if (jts == jds) then
            j = jts
            do i = its, ite    ! This is different to original
               div(i,j+1,k)=div(i,j+1,k)+div(i,j,k)*one_third*4.0
               div(i,j+2,k)=div(i,j+2,k)-div(i,j,k)*one_third
               div(i,j,k)=0.0
            end do
         end if

         ! [2.2.2] Top boundaries:

         if (ite == ide) then
            i = ite
            do j = jts, jte
               div(i-1,j,k)=div(i-1,j,k)+div(i,j,k)*one_third*4.0
               div(i-2,j,k)=div(i-2,j,k)-div(i,j,k)*one_third
               div(i,j,k)=0.0
            end do
         end if

         ! [2.2.1] Bottom boundaries:

         if (its == ids) then
            i = its 
            do j = jts, jte
               div(i+1,j,k)=div(i+1,j,k)+div(i,j,k)*one_third*4.0
               div(i+2,j,k)=div(i+2,j,k)-div(i,j,k)*one_third
               div(i,j,k)=0.0
            end do
         end if

         ! [2.1] Compute fd divergence at interior points:
         ! Computation to check for edge of domain:
         ! This is only for adjoint, as we have to cross the processor boundary
         ! to get the contribution.

         grid%xp%vxy(its:ite, jts:jte) = div(its:ite, jts:jte, k)
#ifdef DM_PARALLEL
#include "HALO_2D_WORK.inc"
#endif

         div(is, js:je, k) = grid%xp%vxy(is, js:je)
         div(ie, js:je, k) = grid%xp%vxy(ie, js:je)
         div(is:ie, js, k) = grid%xp%vxy(is:ie, js)
         div(is:ie, je, k) = grid%xp%vxy(is:ie, je)

         do j = js, je
            do i = is, ie
               coeff = grid%xb%map_factor(i,j) * grid%xb%map_factor(i,j) * inv_2ds
               um(i+1,j)=um(i+1,j)+div(i,j,k)*coeff
               um(i-1,j)=um(i-1,j)-div(i,j,k)*coeff
               vm(i,j+1)=vm(i,j+1)+div(i,j,k)*coeff
               vm(i,j-1)=vm(i,j-1)-div(i,j,k)*coeff
            end do
         end do

         u(is-1:ie+1,js-1:je+1,k) = u(is-1:ie+1,js-1:je+1,k) +um(is-1:ie+1,js-1:je+1) / grid%xb%map_factor(is-1:ie+1,js-1:je+1)
         v(is-1:ie+1,js-1:je+1,k) = v(is-1:ie+1,js-1:je+1,k) +vm(is-1:ie+1,js-1:je+1) / grid%xb%map_factor(is-1:ie+1,js-1:je+1)
      end do

   else ! global
      call da_set_boundary_3d(div)

      do k =kds, kde
         !-------------------------------------------------------------------------
         ! [2.1] Compute fd divergence at interior points:
         !-------------------------------------------------------------------------

         do j = je, js, -1
            do i = ie, is, -1  
               u(i+1,j,k) = u(i+1,j,k) + grid%xb%coefx(i,j) * div(i,j,k) 
               u(i-1,j,k) = u(i-1,j,k) - grid%xb%coefx(i,j) * div(i,j,k) 
               v(i,j+1,k) = v(i,j+1,k) + grid%xb%coefy(i,j) * div(i,j,k) 
               v(i,j-1,k) = v(i,j-1,k) - grid%xb%coefy(i,j) * div(i,j,k) 
            end do
         end do
      end do
   end if

   div = 0.0

   if (trace_use) call da_trace_exit("da_uv_to_divergence_adj")

end subroutine da_uv_to_divergence_adj


