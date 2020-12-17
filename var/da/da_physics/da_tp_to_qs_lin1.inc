subroutine da_tp_to_qs_lin1( t, p, es, t_prime, p_prime, qs_prime_over_qs )

   !---------------------------------------------------------------------------
   ! Purpose: Convert es/p/es_prime to saturation specific humidity increment.
   !
   !  Method: qs~ = qs * ( p es'/es - p' ) / ( p - (1-rd_over_rv) es ).
   !          use Rogers & Yau (1989) formula: es = a exp( bTc / (T_c + c) ).
   !---------------------------------------------------------------------------

   implicit none
   
   real, intent(in)  :: t                ! Temperature.
   real, intent(in)  :: p                ! Pressure.
   real, intent(in)  :: es               ! Sat. vapour pressure.
   real, intent(in)  :: t_prime          ! Temperature increment.
   real, intent(in)  :: p_prime          ! Pressure increment.
   real, intent(out) :: qs_prime_over_qs ! qs~/qs.
   
   real :: temp           ! Temporary value.
   real :: es_prime_over_es ! es~/es

   if (trace_use_dull) call da_trace_entry("da_tp_to_qs_lin1")

   !---------------------------------------------------------------------------
   ! [1.0] initialise:
   !---------------------------------------------------------------------------

   temp = t + es_gammakelvin
   
   !---------------------------------------------------------------------------
   ! [2.0] Calculate saturation vapour pressure increment:
   !---------------------------------------------------------------------------

   es_prime_over_es = es_gammabeta * t_prime / ( temp * temp )

   !---------------------------------------------------------------------------
   ! [3.0] Calculate saturation specific humidity increment:
   !---------------------------------------------------------------------------

   qs_prime_over_qs = (p * es_prime_over_es - p_prime) / (p - rd_over_rv1 * es)

   if (trace_use_dull) call da_trace_exit("da_tp_to_qs_lin1")


end subroutine da_tp_to_qs_lin1


