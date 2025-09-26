subroutine da_hydrostaticp_to_rho_lin(grid, p, rho ) 

   !---------------------------------------------------------------------------
   !  Purpose: Calculates density increment from pressure increment fiteld.
   !
   !  Method:  Hydrostatic eqn => rho~ = -dP~/dz / g.
   !
   !  Assumptions: 1) Hydrostatic pressure.
   !               2) Model level stored top down.
   !
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(in)  :: grid
   real,         intent(in)  :: p(ims:ime,jms:jme,kms:kme)   ! Pressure inc. (cross pts)
   real,         intent(out) :: rho(ims:ime,jms:jme,kms:kme) ! Density inc. (cross pts)

   integer :: i, j, k      ! Loop counters.
   real    :: delta1       ! Height difference.
   real    :: delta2       ! Height difference.
   real    :: dPdz         ! Vertical pressure gradient.

   if (trace_use) call da_trace_entry("da_hydrostaticp_to_rho_lin")

   !---------------------------------------------------------------------------
   ! [2.0] Calculate density increments at all levels except top/bottom:
   !---------------------------------------------------------------------------

   do k = kts+1, kte-1 
      rho(its:ite,jts:jte,k) =  -( p(its:ite,jts:jte,k+1) - p(its:ite,jts:jte,k-1) ) / &
                             ( ( grid%xb % h(its:ite,jts:jte,k+1) - &
                             grid%xb % h(its:ite,jts:jte,k-1) ) * gravity )
   end do                                  

   !---------------------------------------------------------------------------
   ! [3.0] Calculate density increment on bottom level:
   !---------------------------------------------------------------------------

   k = kts
   do j = jts, jte
      do i = its, ite
         ! dP~/dz by backwards one-sided 2nd order finite differences:
         
         delta1 = grid%xb % h(i,j,k+1) - grid%xb % h(i,j,k)
         delta2 = grid%xb % h(i,j,k+2) - grid%xb % h(i,j,k)
         dPdz = -( delta1 + delta2 ) * p(i,j,k)  / ( delta1 * delta2 ) + &
                 ( delta2 / delta1 ) * p(i,j,k+1)/ ( delta2 - delta1 ) - &
                 ( delta1 / delta2 ) * p(i,j,k+2)/ ( delta2 - delta1 )
                      
         ! Put together to get density increment at top level:
         rho(i,j,k) = -dPdz / gravity
      end do
   end do
                       
   !---------------------------------------------------------------------------
   ! [4.0] Calculate density increment on top level:
   !---------------------------------------------------------------------------

   k = kte
   do j = jts, jte
      do i = its, ite
         ! dP~/dz by forwards one-sided 2nd order finite differences:
         
         delta1 = grid%xb % h(i,j,k) - grid%xb % h(i,j,k-1)
         delta2 = grid%xb % h(i,j,k) - grid%xb % h(i,j,k-2)
         
         dPdz = ( delta1 + delta2 ) * p(i,j,k)   / ( delta1 * delta2 ) - &
                ( delta2 / delta1 ) * p(i,j,k-1) / ( delta2 - delta1 ) + &
                ( delta1 / delta2 ) * p(i,j,k-2) / ( delta2 - delta1 )

         ! Put together to get density increment at bottom level:
         rho(i,j,k) = -dPdz / gravity
      end do
   end do

   if (trace_use) call da_trace_exit("da_hydrostaticp_to_rho_lin")
   
end subroutine da_hydrostaticp_to_rho_lin


