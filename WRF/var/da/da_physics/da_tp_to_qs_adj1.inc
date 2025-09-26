subroutine da_tp_to_qs_adj1( t, p, es, t_prime, p_prime, qs_prime_over_qs )

   !---------------------------------------------------------------------------
   !  Purpose: Adjoint of da_tp_to_qs_lin.
   !---------------------------------------------------------------------------

   implicit none
   
   real, intent(in)    :: t                ! Temperature.
   real, intent(in)    :: p                ! Pressure.
   real, intent(in)    :: es               ! Sat. vapour pressure.
   real, intent(inout) :: t_prime          ! Temperature increment.
   real, intent(inout) :: p_prime          ! Pressure increment.
   real, intent(in)    :: qs_prime_over_qs ! qs~/qs.
   
   real :: temp             ! Temporary storage.
   real :: es_prime_over_es ! es~/es

   if (trace_use) call da_trace_entry("da_tp_to_qs_adj1")
      
   !---------------------------------------------------------------------------
   ! [3.0] Calculate saturation specific humidity increment:
   !---------------------------------------------------------------------------

   temp = qs_prime_over_qs / ( p - rd_over_rv1 * es )
   
   es_prime_over_es = temp * p

   p_prime = p_prime - temp
   
   !---------------------------------------------------------------------------
   ! [2.0] Calculate saturation vapour pressure increment:
   !---------------------------------------------------------------------------

   temp = t + es_gammakelvin

   t_prime = t_prime + es_gammabeta * es_prime_over_es / ( temp * temp )

   if (trace_use) call da_trace_exit("da_tp_to_qs_adj1")

end subroutine da_tp_to_qs_adj1


