subroutine da_evapo_lin(dt,scr3,scr5,qv_b,pre,scr6, scr39,scr59,qv_a,pre9,scr69, kts,kte,kms,kme)

   !-----------------------------------------------------------------------
   ! Purpose: Rainwater evaporation
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: kts, kte, kms, kme
   real,    intent(in)  :: dt(kms:kme)
   real,    intent(in)  :: scr3(kms:kme)
   real,    intent(in)  :: scr5(kms:kme)
   real,    intent(in)  :: scr6(kms:kme)
   real,    intent(in)  :: qv_b(kms:kme)
   real,    intent(out) :: pre(kms:kme)
   real,    intent(out) :: pre9(kms:kme)
   real,    intent(in)  :: scr39(kms:kme)
   real,    intent(in)  :: scr59(kms:kme)
   real,    intent(in)  :: scr69(kms:kme)
   real,    intent(in)  :: qv_a(kms:kme)

   integer :: k
   real    :: beta, qrth
   real    :: tmp, tmp2

   if (trace_use) call da_trace_entry("da_evapo_lin")

   qrth = 1.0e-6
   beta = 0.0486   ! original

   do k = kts, kte
      if (dt(k) <= 0.0) cycle

      if ( scr3(k) > qrth .and. qv_b(k) < scr5(k) ) then
         tmp  = beta * ( qv_b(k)-scr5(k) )* 0.65 * ( scr6(k)*scr3(k) )**(-0.35)
         tmp2 = beta * ( scr6(k)*scr3(k) )**0.65
         pre9(k) = tmp * ( scr69(k)*scr3(k)+scr6(k)*scr39(k) ) + &
                   tmp2 * ( qv_a(k)-scr59(k) )
         pre(k)  = beta * ( qv_b(k)-scr5(k) ) * ( scr6(k)*scr3(k) )**0.65
      else if ( scr3(k) <= qrth .and. scr3(k) > 0.0 .and. qv_b(k) < scr5(k) ) then
         tmp  = beta * ( qv_b(k)-scr5(k) ) * 0.65 * ( scr6(k)*qrth )**(-0.35)
         tmp2 = beta * ( scr6(k)*qrth )**0.65
         pre9(k) = tmp * ( scr69(k)*qrth ) + tmp2 * ( qv_a(k)-scr59(k) )
         pre(k)  = beta * ( qv_b(k)-scr5(k) ) * ( scr6(k)*qrth )**0.65
      else
         pre9(k) = 0.0
         pre(k) = 0.0
      end if

      if ( pre(k) < -scr3(k)/dt(k) ) then
         pre9(k) = -scr39(k) / dt(k)
         pre(k)  = -scr3(k) / dt(k)
      end if
   end do

   if (trace_use) call da_trace_exit("da_evapo_lin")

end subroutine da_evapo_lin

