subroutine da_prho_to_t_lin(grid) 

   !---------------------------------------------------------------------------
   ! Purpose: Calculates temperature increments from pressure/density increments
   !
   ! Method:  Linearitsed ideal gas law: T~ = T (p"/p - rho"/rho)
   !
   ! Assumptions: 1) Model level stored top down.
   !---------------------------------------------------------------------------

   implicit none
   
   type (domain), intent(inout)    :: grid

   if (trace_use) call da_trace_entry("da_prho_to_t_lin")

   grid%xa % t(its:ite,jts:jte,kts:kte) = grid%xb % t(its:ite,jts:jte,kts:kte) * &
                            (grid%xa % p(its:ite,jts:jte,kts:kte) / &
                              grid%xb % p(its:ite,jts:jte,kts:kte) - &
                              grid%xa % rho(its:ite,jts:jte,kts:kte) / &
                              grid%xb % rho(its:ite,jts:jte,kts:kte)) 

   if (trace_use) call da_trace_exit("da_prho_to_t_lin")    

end subroutine da_prho_to_t_lin


