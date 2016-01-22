subroutine da_tprh_to_q_lin(grid)

   !---------------------------------------------------------------------------
   ! Purpose: Convert T/pressure/rh to specific humidity increments.
   !
   ! Method: q~ = q (rh~/rh + qs~/qs)
   !---------------------------------------------------------------------------

   implicit none
   
   type (domain), intent(inout) :: grid
   
   real :: qs_prime_over_qs(its:ite,jts:jte,kts:kte) ! qs~/qs.

   if (trace_use) call da_trace_entry("da_tprh_to_q_lin")

   !---------------------------------------------------------------------------
   ! [1.0] Calculate saturation specific humidity ratio qs~/qs:
   !---------------------------------------------------------------------------

   call da_tp_to_qs_lin( grid, qs_prime_over_qs )
   
   !---------------------------------------------------------------------------
   ! [2.0] Calculate specific humidity increment:
   !---------------------------------------------------------------------------

   grid%xa % q(its:ite,jts:jte,kts:kte) = grid%xb % q(its:ite,jts:jte,kts:kte) * &
                               ( grid%xa % rh(its:ite,jts:jte,kts:kte) / &
                                 grid%xb % rh(its:ite,jts:jte,kts:kte) + &
                                 qs_prime_over_qs(its:ite,jts:jte,kts:kte) )

   if (trace_use) call da_trace_exit("da_tprh_to_q_lin")

end subroutine da_tprh_to_q_lin


