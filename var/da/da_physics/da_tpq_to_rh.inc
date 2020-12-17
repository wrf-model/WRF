subroutine da_tpq_to_rh( t, p, q, es, qs, rh )

   !---------------------------------------------------------------------------
   ! Purpose: Convert T/p/q to relative humidity rh.
   !---------------------------------------------------------------------------

   implicit none

   real, intent(in)  :: t, p, q
   real, intent(out) :: es, qs, rh

   if (trace_use_dull) call da_trace_entry("da_tpq_to_rh")

   !---------------------------------------------------------------------------
   ! [1.0] Calculate saturation specific humidity:
   !---------------------------------------------------------------------------

   call da_tp_to_qs( t, p, es, qs )
   
   !---------------------------------------------------------------------------
   ! [2.0] Calculate relative humidity:
   !---------------------------------------------------------------------------

   rh = 100.0 * q / qs

   if (trace_use_dull) call da_trace_exit("da_tpq_to_rh")

end subroutine da_tpq_to_rh


