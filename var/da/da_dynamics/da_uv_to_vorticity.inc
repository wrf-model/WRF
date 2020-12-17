subroutine da_uv_to_vorticity(xb , u, v, vor)

   !---------------------------------------------------------------------------
   !  Purpose: Calculate vorticity on a co-ordinate surface, given an input 
   !           wind field.
   !
   !  Method:  vor = m**2 (d(v/m)/dx - d(u/m)/dy)
   !---------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)    :: xb         ! First guess structure.
   real,           intent(in)    :: u(ims:ime,jms:jme,kms:kme)   ! u wind comp.
   real,           intent(in)    :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp.
   real,           intent(inout) :: vor(ims:ime,jms:jme,kms:kme) ! Vorticity.

   integer :: i, j, k                      ! Loop counters.
   integer :: is, ie                       ! 1st dim. end points.
   integer :: js, je                       ! 2nd dim. end points.
   real    :: one_third                    ! 1/3.
   real    :: inv_2ds                      ! 1/2ds.
   real    :: coeff(ims:ime,jms:jme)       ! Mult. coefficient.
   real    :: um(ims:ime,jms:jme)          ! Temp. storage of u/m.
   real    :: vm(ims:ime,jms:jme)          ! Temp. storage of v/m.

   if (trace_use) call da_trace_entry("da_uv_to_vorticity")

   !---------------------------------------------------------------------------
   ! [1.0] Initialise:
   !---------------------------------------------------------------------------

   one_third = 1.0 / 3.0

   vor = 0.0

   !---------------------------------------------------------------------------
   ! Computation to check for edge of domain:
   !---------------------------------------------------------------------------

   is = its
   ie = ite
   js = jts
   je = jte
   if (.not. global .and. its == ids) is = ids+1 
   if (.not. global .and. ite == ide) ie = ide-1
   if (jts == jds) js = jds+1
   if (jte == jde) je = jde-1

   if (.not.global) then
      inv_2ds = 0.5 / xb%ds
      coeff(its:ite,jts:jte) = xb%map_factor(its:ite,jts:jte) * &
         xb%map_factor(its:ite,jts:jte) * inv_2ds
   end if
     
   !---------------------------------------------------------------------------
   ! [2.0] Calculate vorticity:
   !---------------------------------------------------------------------------

   do k = kts, kte
      ! [2.1] Compute finite difference vorticity at interior points:

      if (global) then
         do j = js, je
            do i = is, ie
               vor(i,j,k) = xb%coefy(i,j) * (-u(i,j+1,k) + u(i,j-1,k))  + & 
                            xb%coefx(i,j) * ( v(i+1,j,k) - v(i-1,j,k))
            end do
         end do

         ! if (global) cycle
      else
         um(is-1:ie+1,js-1:je+1) = u(is-1:ie+1,js-1:je+1,k) / xb%map_factor(is-1:ie+1,js-1:je+1)
         vm(is-1:ie+1,js-1:je+1) = v(is-1:ie+1,js-1:je+1,k) / xb%map_factor(is-1:ie+1,js-1:je+1)

         ! [2.1] Compute finite difference vorticity at interior points:

         do j = js, je
            do i = is, ie
               vor(i,j,k) = (-um(i,j+1) + um(i,j-1) + vm(i+1,j) - vm(i-1,j)) * coeff(i,j)
            end do
         end do

         ! [2.2] Impose zero vorticity gradient condition at boundaries:

         ! [2.2.1] Bottom boundaries:

         if (its == ids) then
            i = its 
            do j = jts, jte
               vor(i,j,k) = one_third * (4.0 * vor(i+1,j,k) - vor(i+2,j,k))
            end do
         end if

         ! [2.2.2] Top boundaries:

         if (ite == ide) then
            i = ite
            do j = jts, jte
               vor(i,j,k) = one_third * (4.0 * vor(i-1,j,k) - vor(i-2,j,k))
            end do
         end if

         ! [2.2.3] Left boundaries:

         if (jts == jds) then
            j = jts
            do i = its, ite
               vor(i,j,k) = one_third * (4.0 * vor(i,j+1,k) - vor(i,j+2,k))
            end do
         end if

         ! [2.2.4] right boundaries:

         if (jte == jde) then
            j = jte
            do i = its, ite
               vor(i,j,k) = one_third * (4.0 * vor(i,j-1,k) - vor(i,j-2,k))
            end do
         end if
      end if
   end do

   if (global) then
      call da_set_boundary_3d(vor)
   end if

   if (trace_use) call da_trace_exit("da_uv_to_vorticity")

end subroutine da_uv_to_vorticity


