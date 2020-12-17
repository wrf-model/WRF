subroutine da_tp_to_qs( t, p, es, qs)

   !---------------------------------------------------------------------------
   ! Purpose: Convert T/p to saturation specific humidity.
   !
   !  Method: qs = es_alpha * es / ( p - ( 1 - rd_over_rv ) * es ).
   !          use Rogers & Yau (1989) formula: es = a exp( bTc / (T_c + c) )
   !--------------------------------------------------------------------------

   implicit none

   real, intent(in)  :: t, p
   real, intent(out) :: es, qs
   
   real              :: t_c              ! T in degreesC.

   if (trace_use_dull) call da_trace_entry("da_tp_to_qs")

   !---------------------------------------------------------------------------
   ! [1.0] initialise:
   !---------------------------------------------------------------------------
   
   t_c = t - t_kelvin
   
   !---------------------------------------------------------------------------
   ! [2.0] Calculate saturation vapour pressure:
   !---------------------------------------------------------------------------

   es = es_alpha * exp( es_beta * t_c / ( t_c + es_gamma ) )
    
   !---------------------------------------------------------------------------
   ! [3.0] Calculate saturation specific humidity:
   !---------------------------------------------------------------------------

   qs = rd_over_rv * es / ( p - rd_over_rv1 * es )

   if (trace_use_dull) call da_trace_exit("da_tp_to_qs")

end subroutine da_tp_to_qs


