subroutine da_qfrmrh ( p, t, rh, q )

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: p, t, rh
   real, intent(out) :: q

   real   :: a, b, e, qs

   if (trace_use) call da_trace_entry("da_qfrmrh")

   a=17.26
   b=35.86
   if (t <= 263.0) a=21.87
   if (t <= 263.0) b= 7.66
   e  = 6.11*exp(a*(t-t_triple)/(t-b))
   qs = 0.622*e/(p-0.378*e)
   q  = qs*rh/100.0

   if (trace_use) call da_trace_exit("da_qfrmrh")

end subroutine da_qfrmrh


