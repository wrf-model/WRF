subroutine da_psichi_to_uv(psi, chi, coefx,coefy, u, v)

   !---------------------------------------------------------------------------
   !  Purpose: Calculate wind components u and v from psi and chi.
   !
   !  Method:  u = coefx * (-dpsi/dy + dchi/dx)
   !           v = coefy * ( dpsi/dx + dchi/dy)
   !
   !  Assumptions: Unstaggered grid.
   !               Lateral boundary conditions - dpsi/dn, dchi/dn = 0 (FCT)
   !               grid size may or may not be equal
   !----------------------------------------------------------------------------

   implicit none
   
   real, intent(inout) :: psi(ims:ime,jms:jme,kms:kme) ! Stream function
   real, intent(inout) :: chi(ims:ime,jms:jme,kms:kme) ! Velocity potential
   real, intent(in)    :: coefx(ims:ime,jms:jme)       ! Multiplicative coeff.
   real, intent(in)    :: coefy(ims:ime,jms:jme)       ! Multiplicative coeff.
   real, intent(out)   :: u(ims:ime,jms:jme,kms:kme)   ! u wind comp.
   real, intent(out)   :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp.


   integer            :: i, j, k                      ! Loop counters.
   integer            :: is, ie                       ! 1st dim. end points.
   integer            :: js, je                       ! 2nd dim. end points.

   if (trace_use) call da_trace_entry("da_psichi_to_uv")

   !---------------------------------------------------------------------------
   ! [1.0] For Global application, set Wast-Eest Periodic boundary
   !---------------------------------------------------------------------------

   ! Computation to check for edge of domain:
   is = its
   ie = ite
   js = jts
   je = jte
   if (jts == jds) js = jds+1
   if (jte == jde) je = jde-1

   if (global) then  
      call da_set_boundary_3d(psi)
      call da_set_boundary_3d(chi)
   else
      if (its == ids) is = ids+1
      if (ite == ide) ie = ide-1
   end if

   do k = kts, kte
      !------------------------------------------------------------------------
      !  [2.0] Compute u, v at interior points (2nd order central finite diffs):
      !------------------------------------------------------------------------

      do j = js, je
         do i = is, ie
            u(i,j,k) = -coefy(i,j)*(psi(i  ,j+1,k) - psi(i  ,j-1,k)) + &
                        coefx(i,j)*(chi(i+1,j  ,k) - chi(i-1,j  ,k)) 

            v(i,j,k) = coefx(i,j)*(psi(i+1,j  ,k) - psi(i-1,j  ,k))  + &
                       coefy(i,j)*(chi(i  ,j+1,k) - chi(i  ,j-1,k)) 
         end do
      end do

      if (global) cycle
      !------------------------------------------------------------------------
      ! [3.0] Compute u, v at domain boundaries:
      !------------------------------------------------------------------------

      ! [3.1] Western boundaries:

      if (its == ids) then
         i = its
         do j = js, je
            u(i,j,k) = -coefy(i,j)*(psi(i  ,j+1,k) - psi(i,j-1,k)) + &
                        coefx(i,j)*(chi(i+2,j  ,k) - chi(i,j  ,k))  

            v(i,j,k) = coefx(i,j)*(psi(i+2,j  ,k) - psi(i,j  ,k))  + &
                       coefy(i,j)*(chi(i  ,j+1,k) - chi(i,j-1,k)) 
         end do
      end if

      ! [3.2] Eastern boundaries:

      if (ite == ide) then
         i = ite
         do j = js, je
            u(i,j,k) = -coefy(i,j)*(psi(i,j+1,k) - psi(i  ,j-1,k)) + &
                        coefx(i,j)*(chi(i,j  ,k) - chi(i-2,j  ,k)) 

            v(i,j,k) = coefx(i,j)*(psi(i,j  ,k) - psi(i-2,j  ,k))+ &
                       coefy(i,j)*(chi(i,j+1,k) - chi(i  ,j-1,k)) 
         end do
      end if

      ! [3.3] Southern boundaries:

      if (jts == jds) then
         j = jts
         do i = is, ie
            u(i,j,k) = -coefy(i,j)*(psi(i  ,j+2,k) - psi(i  ,j,k)) + &
                        coefx(i,j)*(chi(i+1,j  ,k) - chi(i-1,j,k))  

            v(i,j,k) = coefx(i,j)*(psi(i+1,j  ,k) - psi(i-1,j,k)) + &
                       coefy(i,j)*(chi(i  ,j+2,k) - chi(i  ,j,k)) 
         end do
      end if

      ! [3.4] Northern boundaries:

      if (jte == jde) then
         j = jte
         do i = is, ie
            u(i,j,k) = -coefy(i,j)*(psi(i  ,j,k) - psi(i  ,j-2,k)) + &
                        coefx(i,j)*(chi(i+1,j,k) - chi(i-1,j  ,k))  

            v(i,j,k) = coefx(i,j)*(psi(i+1,j,k) - psi(i-1,j  ,k))+ &
                       coefy(i,j)*(chi(i  ,j,k) - chi(i  ,j-2,k)) 
         end do
      end if

      !------------------------------------------------------------------------
      ! [4.0] Corner points (assume average of surrounding points - poor?):
      !------------------------------------------------------------------------

      ! [4.1] Bottom-left point:

      if (its == ids .AND. jts == jds) then
         u(its,jts,k) = 0.5 * (u(its+1,jts,k) + u(its,jts+1,k))
         v(its,jts,k) = 0.5 * (v(its+1,jts,k) + v(its,jts+1,k))
      end if

      ! [4.2] Top-left point:

      if (ite == ide .AND. jts == jds) then
         u(ite,jts,k) = 0.5 * (u(ite-1,jts,k) + u(ite,jts+1,k))
         v(ite,jts,k) = 0.5 * (v(ite-1,jts,k) + v(ite,jts+1,k))
      end if

      ! [4.3] Bottom-right point:

      if (its == ids .AND. jte == jde) then
         u(its,jte,k) = 0.5 * (u(its+1,jte,k) + u(its,jte-1,k))
         v(its,jte,k) = 0.5 * (v(its+1,jte,k) + v(its,jte-1,k))
      end if

      ! [4.4] Top-right point:

      if (ite == ide .AND. jte == jde) then
         u(ite,jte,k) = 0.5 * (u(ite-1,jte,k) + u(ite,jte-1,k))
         v(ite,jte,k) = 0.5 * (v(ite-1,jte,k) + v(ite,jte-1,k))
      end if
   end do

   !---------------------------------------------------------------------------
   ! [5.0] For Global application, set Wast-Eest Periodic boundary
   !---------------------------------------------------------------------------

   if (global) then
      call da_set_boundary_3d(u)
      call da_set_boundary_3d(v)
   end if

   if (trace_use) call da_trace_exit("da_psichi_to_uv")

end subroutine da_psichi_to_uv


