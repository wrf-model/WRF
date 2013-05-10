subroutine da_hydrostaticp_to_rho_adj (xb, rho, p) 

   !---------------------------------------------------------------------------
   !  Purpose: Adjoint of da_hydrostaticp_to_rho.
   !
   !  Method:  Standard adjoint coding.
   !
   !  Assumptions: 1) Hydrostatic pressure.
   !               2) Model level stored top down.
   !---------------------------------------------------------------------------

   implicit none
   
   type(xb_type), intent(in)    :: xb                           ! First guess structure.
   real,          intent(in)    :: rho(ims:ime,jms:jme,kms:kme) ! Density inc. (cross pts).
   real,          intent(inout) :: p(ims:ime,jms:jme,kms:kme)   ! Pressure inc. (cross pts)

   integer               :: i, j, k      ! Loop counters.
   real                  :: delta1       ! Height difference.
   real                  :: delta2       ! Height difference.
   real                  :: dPdz         ! Vertical pressure gradient.
   real                  :: temp(its:ite,jts:jte) ! Temporary array.

   if (trace_use) call da_trace_entry("da_hydrostaticp_to_rho_adj")
   
   !---------------------------------------------------------------------------
   ! [4.0] Calculate density increment on top level:
   !---------------------------------------------------------------------------

   do j = jts, jte
      do i = its, ite
      
         ! Put together to get density increment at bottom level:
         dPdz = -rho(i,j,kte) / gravity      
      
         ! dP~/dz by forwards one-sided 2nd order finite differences:
         
         delta1 = xb % h(i,j,kte) - xb % h(i,j,kte-1)
         delta2 = xb % h(i,j,kte) - xb % h(i,j,kte-2)
         
         p(i,j,k) = p(i,j,kte) + ( delta1 + delta2 ) * dPdz / (delta1 * delta2)
         p(i,j,k-1) = p(i,j,kte-1) - (delta2 / delta1) * dPdz / (delta2 - delta1)
         p(i,j,k-2) = p(i,j,kte-2) + (delta1 / delta2) * dPdz / (delta2 - delta1)
      end do
   end do

   !---------------------------------------------------------------------------
   ! [3.0] Calculate density increment on top level:
   !---------------------------------------------------------------------------

   do j = jts, jte
      do i = its, ite

         ! Put together to get density increment at top level:
         dPdz = -rho(i,j,kts) / gravity

         ! dP~/dz by backwards one-sided 2nd order finite differences:

         delta1 = xb % h(i,j,kts+1) - xb % h(i,j,kts)
         delta2 = xb % h(i,j,kts+2) - xb % h(i,j,kts)

         p(i,j,kts)   = p(i,j,k)     - (delta1 + delta2) * dPdz / (delta1 * delta2)
         p(i,j,kts+1) = p(i,j,kts+1) + (delta2 / delta1) * dPdz / (delta2 - delta1)
         p(i,j,kts+2) = p(i,j,kts+2) - (delta1 / delta2) * dPdz / (delta2 - delta1)
      end do
   end do  
   
   !---------------------------------------------------------------------------
   ! [2.0] Calculate density increments at all levels except top/bottom:
   !---------------------------------------------------------------------------

   do k = kte-1, kts+1, -1 
      temp(its:ite,jts:jte) = -rho(its:ite,jts:jte,k) / &
         ((xb%h(its:ite,jts:jte,k+1) - xb%h(its:ite,jts:jte,k-1)) * gravity)

      p(its:ite,jts:jte,k-1) = p(its:ite,jts:jte,k-1) - temp(its:ite,jts:jte)
      p(its:ite,jts:jte,k+1) = p(its:ite,jts:jte,k+1) + temp(its:ite,jts:jte)
   end do                                  

end subroutine da_hydrostaticp_to_rho_adj


