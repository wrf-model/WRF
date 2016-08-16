subroutine da_pt_to_rho_lin(grid)

   !---------------------------------------------------------------------------
   ! Purpose: Calculates density increments from pressure/temperature increments
   !
   ! Method:  Linearised ideal gas law: rho~/rho = p'/p - T'/T
   !
   ! Assumptions: 1) Model level stored top down.
   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout) :: grid

   if (trace_use) call da_trace_entry("da_pt_to_rho_lin")
   
   grid%xa % rho(its:ite,jts:jte,kts:kte) = grid%xb % rho(its:ite,jts:jte,kts:kte) * ( &
      grid%xa % p(its:ite,jts:jte,kts:kte) / grid%xb % p(its:ite,jts:jte,kts:kte) - &
      grid%xa % t(its:ite,jts:jte,kts:kte) / grid%xb % t(its:ite,jts:jte,kts:kte))                       


   if (trace_use) call da_trace_exit("da_pt_to_rho_lin")

end subroutine da_pt_to_rho_lin


