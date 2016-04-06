subroutine da_balance_geoterm_adj( cori, rho, term_x, term_y, u, v)
 
   !---------------------------------------------------------------------------
   ! Purpose: Adjoint of da_balance_geoterm.
   !---------------------------------------------------------------------------

   implicit none
   
   real, intent(in)    :: cori(ims:ime,jms:jme)   ! Coriolis factor.
   real, intent(in)    :: rho(ims:ime,jms:jme)    ! Density
   real, intent(in)    :: term_x(ims:ime,jms:jme) ! x component of term.
   real, intent(in)    :: term_y(ims:ime,jms:jme) ! y component of term.
   real, intent(inout) :: u(ims:ime,jms:jme)      ! u wind increment
   real, intent(inout) :: v(ims:ime,jms:jme)      ! v wind increment

   if (trace_use) call da_trace_entry("da_balance_geoterm_adj")

   !---------------------------------------------------------------------------
   ! [2.0] Calculate term_y = f rho u~:
   !---------------------------------------------------------------------------

   u(its:ite,jts:jte) = u(its:ite,jts:jte) + rho(its:ite,jts:jte) * cori(its:ite,jts:jte) &
      * term_y(its:ite,jts:jte)

   !---------------------------------------------------------------------------
   ! [1.0] Calculate term_x = -f rho v~:
   !---------------------------------------------------------------------------

   v(its:ite,jts:jte) = v(its:ite,jts:jte) - rho(its:ite,jts:jte) * cori(its:ite,jts:jte) &
      * term_x(its:ite,jts:jte)

   if (trace_use) call da_trace_exit("da_balance_geoterm_adj")

end subroutine da_balance_geoterm_adj


