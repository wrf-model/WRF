subroutine da_tprh_to_q_lin1( t, p, es, q, rh, t_prime, p_prime, rh_prime, q_prime )

   !---------------------------------------------------------------------------
   !  Purpose: Convert T/pressure/rh to specific humidity increments.
   !
   !  Method: q~ = q (rh~/rh + qs~/qs)
   !---------------------------------------------------------------------------

   implicit none

   real, intent(in)  :: t        ! Temperature.
   real, intent(in)  :: p        ! Pressure.
   real, intent(in)  :: es       ! Saturation vapour pressure.
   real, intent(in)  :: q        ! Specific humidity.
   real, intent(in)  :: rh       ! Relative Humidity.
   real, intent(in)  :: t_prime  ! Temperature increment.
   real, intent(in)  :: p_prime  ! Pressure increment.
   real, intent(in)  :: rh_prime ! Pressure increment.
   real, intent(out) :: q_prime  ! Pressure increment.
   
   real :: qs_prime_over_qs ! qs~/qs.

   if (trace_use_dull) call da_trace_entry("da_tprh_to_q_lin1")

   !---------------------------------------------------------------------------
   ! [1.0] Calculate saturation specific humidity ratio qs~/qs:
   !---------------------------------------------------------------------------

   call da_tp_to_qs_lin1( t, p, es, t_prime, p_prime, qs_prime_over_qs )
   
   !---------------------------------------------------------------------------
   ! [2.0] Calculate specific humidity increment:
   !---------------------------------------------------------------------------

   q_prime = q * ( rh_prime / rh + qs_prime_over_qs )

   if (trace_use_dull) call da_trace_exit("da_tprh_to_q_lin1")

end subroutine da_tprh_to_q_lin1


