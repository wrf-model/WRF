subroutine da_tprh_to_q_adj1( t, p, es, q, rh, t_prime, p_prime, rh_prime, q_prime )

   !---------------------------------------------------------------------------
   !  Purpose: Adjoint of da_tprh_to_q_adj.
   !---------------------------------------------------------------------------

   implicit none

   real, intent(in)    :: t        ! Temperature.
   real, intent(in)    :: es       ! Saturation vapour pressure.
   real, intent(in)    :: p        ! Pressure.
   real, intent(in)    :: q        ! Specific humidity.
   real, intent(in)    :: rh       ! Relative Humidity.
   real, intent(inout) :: t_prime  ! Temperature increment.
   real, intent(inout) :: p_prime  ! Pressure increment.
   real, intent(inout) :: rh_prime ! Pressure increment.
   real, intent(in)    :: q_prime  ! Pressure increment.
   
   real :: temp     ! Temporary storage.
   real :: qs_prime_over_qs ! qs~/qs.

   if (trace_use) call da_trace_entry("da_tprh_to_q_adj1")

   temp = q * q_prime

   !---------------------------------------------------------------------------
   ! [2.0] Calculate relative humidity increment:
   !---------------------------------------------------------------------------

   rh_prime = rh_prime + temp / rh
   qs_prime_over_qs = temp

   !---------------------------------------------------------------------------
   ! [1.0] Calculate saturation specific humidity ratio qs~/qs:
   !---------------------------------------------------------------------------

   call da_tp_to_qs_adj1 (t, p, es, t_prime, p_prime, qs_prime_over_qs)

   if (trace_use) call da_trace_exit("da_tprh_to_q_adj1")

end subroutine da_tprh_to_q_adj1


