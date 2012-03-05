subroutine da_uv_to_divergence(xb, u, v, div)

   !---------------------------------------------------------------------------
   !  Purpose: Calculate divergence on a co-ordinate surface, given an input
   !           wind field.
   !
   !                        d   U      d   V
   !           Div = m^2 *[---(---) + ---(---) ] 
   !                        dx  m      dy  M
   !---------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)           :: xb         ! First guess structure.
   real, intent(in)   :: u(ims:ime,jms:jme,kms:kme)   ! u wind comp.
   real, intent(in)   :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp.
   real, intent(inout):: div(ims:ime,jms:jme,kms:kme) ! Divergence.

   integer            :: i, j, k                      ! Loop counters.
   integer            :: is, ie                       ! 1st dim. end points.
   integer            :: js, je                       ! 2nd dim. end points.
   real               :: one_third                    ! 1/3.

   real               :: coeff, inv_2ds
   real               :: um(ims:ime,jms:jme)          ! Temp. storage of u/m.
   real               :: vm(ims:ime,jms:jme)          ! Temp. storage of v/m.

   if (trace_use) call da_trace_entry("da_uv_to_divergence")

   !---------------------------------------------------------------------------
   ! [1.0] Initialise:
   !---------------------------------------------------------------------------

   one_third = 1.0 / 3.0
   div = 0.0

   !---------------------------------------------------------------------------
   ! Computation to check for edge of domain:
   !---------------------------------------------------------------------------

   is = its; ie = ite; js = jts; je = jte
   if (.not. global .and. its == ids) is = ids+1  
   if (.not. global .and. ite == ide) ie = ide-1
   if (jts == jds) js = jds+1; if (jte == jde) je = jde-1
  
   if (.not.global) inv_2ds = 0.5 / xb%ds

   !---------------------------------------------------------------------------
   ! [2.0] Calculate divergence:
   !---------------------------------------------------------------------------

   if (global) then
      do k = kts, kte
         ! [2.1] Compute fd divergence at interior points:

         do j = js, je
            do i = is, ie
               div(i,j,k) = xb%coefx(i,j) * (u(i+1,j,k) - u(i-1,j,k)) + &
                            xb%coefy(i,j) * (v(i,j+1,k) - v(i,j-1,k))
            end do
         end do
      end do
      call da_set_boundary_3d(div)
   else
      do k = kts, kte

         um(is-1:ie+1,js-1:je+1) = u(is-1:ie+1,js-1:je+1,k) / xb%map_factor(is-1:ie+1,js-1:je+1)
         vm(is-1:ie+1,js-1:je+1) = v(is-1:ie+1,js-1:je+1,k) / xb%map_factor(is-1:ie+1,js-1:je+1)

         ! [2.1] Compute fd divergence at interior points:

         do j = js, je
            do i = is, ie
               coeff = xb%map_factor(i,j) * xb%map_factor(i,j) * inv_2ds
               div(i,j,k) = (um(i+1,j) - um(i-1,j) + vm(i,j+1) - vm(i,j-1)) * coeff
            end do
         end do

         ! [2.2] Impose zero divergence gradient condition at boundaries:

         ! [2.2.1] Bottom boundaries:

         if (its == ids) then
            i = its 
            do j = jts, jte
               div(i,j,k) = one_third * (4.0 * div(i+1,j,k) - div(i+2,j,k))
            end do
         end if

         ! [2.2.2] Top boundaries:

         if (ite == ide) then
            i = ite
            do j = jts, jte
               div(i,j,k) = one_third * (4.0 * div(i-1,j,k) - div(i-2,j,k))
            end do
         end if

         ! [2.2.3] Left boundaries:

         if (jts == jds) then
            j = jts
            do i = its, ite
               div(i,j,k) = one_third * (4.0 * div(i,j+1,k) - div(i,j+2,k))
            end do
         end if

         ! [2.2.4] right boundaries:

         if (jte == jde) then
            j = jte
            do i = its, ite
               div(i,j,k) = one_third * (4.0 * div(i,j-1,k) - div(i,j-2,k))
            end do
         end if
      end do
   end if

   if (trace_use) call da_trace_exit("da_uv_to_divergence")

end subroutine da_uv_to_divergence


