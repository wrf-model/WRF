subroutine da_tpq_to_rh_lin(grid)

   !---------------------------------------------------------------------------
   !  Purpose: Convert T/pressure/q to relative humidity increments.
   !
   !  Method: r~ = r (q~/q - qs~/qs).
   !
   ! When q approaching to zero, the above formula its undefined. The
   ! general formula below must be used:
   ! 
   !  Method: r~ = 100 * (q~/qs - q*(qs~/qs)/qs))
   !             = 100 * q~/qs - (100*q/qs)*(qs~/qs)
   !             = 100 * q~/qs - rh * (qs~/qs) 
   !
   !---------------------------------------------------------------------------

   implicit none
   
   type (domain),  intent(inout) ::grid
   
   real :: qs(its:ite,jts:jte,kts:kte)
   real :: es(its:ite,jts:jte,kts:kte)
   real :: qs_prime_over_qs(its:ite,jts:jte,kts:kte)

   if (trace_use_dull) call da_trace_entry("da_tpq_to_rh_lin")

   !---------------------------------------------------------------------------
   ! [1.0] Calculate saturation specific humidity ratio qs~/qs:
   !---------------------------------------------------------------------------
 
   call da_tp_to_qs_lin(grid, qs_prime_over_qs )

   !--------------------------------------------------------------------------
   ! [2.0] Culcalete background saturation specific humidity qs:
   !--------------------------------------------------------------------------

   call da_tp_to_qs1(grid, es, qs) 
   
   !---------------------------------------------------------------------------
   ! [3.0] Calculate relative humidity increment:
   !---------------------------------------------------------------------------

   grid%xa % rh(its:ite,jts:jte,kts:kte) = 100.0 * &
                                ( grid%xa % q(its:ite,jts:jte,kts:kte) / &
                                      qs(its:ite,jts:jte,kts:kte) ) - &
                                  grid%xb % rh(its:ite,jts:jte,kts:kte) * &
                                  qs_prime_over_qs(its:ite,jts:jte,kts:kte)

   if (trace_use_dull) call da_trace_exit("da_tpq_to_rh_lin")

end subroutine da_tpq_to_rh_lin


