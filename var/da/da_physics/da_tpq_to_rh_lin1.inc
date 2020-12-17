subroutine da_tpq_to_rh_lin1( t, p, es, rh, t_prime, p_prime, q_prime, rh_prime )

   !---------------------------------------------------------------------------
   !  Purpose: Convert T/pressure/q to relative humidity increments.
   !
   !  Method: r~ = r (q~/q - qs~/qs).
   !
   !  When q approaching to zero, the above formula is undefined. The
   !  general formula below must be used:
   ! 
   !  Method: r~ = 100 * (q~/qs) - rh*(qs~/qs)
   !---------------------------------------------------------------------------

   implicit none

   real, intent(in)  :: t        ! Temperature.
   real, intent(in)  :: p        ! Pressure.
   real, intent(in)  :: es       ! Saturation vapour pressure.
   real, intent(in)  :: rh       ! Relative Humidity.
   real, intent(in)  :: t_prime  ! Temperature increment.
   real, intent(in)  :: p_prime  ! Pressure increment.
   real, intent(in)  :: q_prime  ! Pressure increment.
   real, intent(out) :: rh_prime ! Pressure increment.
   
   real :: es1, qs  ! Saturation specific humidity.
   real :: qs_prime_over_qs ! qs~/qs.

   if (trace_use_dull) call da_trace_entry("da_tpq_to_rh_lin1")

   !---------------------------------------------------------------------------
   ! [1.0] Calculate saturation specific humidity ratio qs~/qs:
   !---------------------------------------------------------------------------

   call da_tp_to_qs_lin1( t, p, es, t_prime, p_prime, qs_prime_over_qs )
   
   !--------------------------------------------------------------------------
   ! [2.0] Culcalete background saturation specific humidity qs:
   !--------------------------------------------------------------------------

   call da_tp_to_qs( t, p, es1, qs) 
   
   !---------------------------------------------------------------------------
   ! [3.0] Calculate relative humidity increment:
   !---------------------------------------------------------------------------

   rh_prime = 100.0 * (q_prime / qs) - rh * qs_prime_over_qs

   if (trace_use_dull) call da_trace_exit("da_tpq_to_rh_lin1")

end subroutine da_tpq_to_rh_lin1


