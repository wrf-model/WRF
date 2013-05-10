subroutine da_intpsfc_prs (val, ho, po, hm, tm, qm, to, qo)

   !----------------------------------------------------------------------------
   ! Purpose: Correct pressure between two levels. 
   !
   ! Reference: make use of the hydrosatic equation:
   !
   !  P2 = P1 * exp [-G/R * (z2-z1) / (tv1 + tv2)/2)
   !
   ! Where:
   !  z1  = height at level 1
   !  z1  = height at level 2
   !  tv1 = temperature at level 1
   !  tv2 = temperature at level 2
   !  P1  = Pressure at level 1
   !  P2  = Pressure at level 2
   !----------------------------------------------------------------------------

   implicit none

   real, intent (out)          :: val
   real, intent (in)           :: ho, po
   real, intent (in)           :: hm, tm, qm    
   real, intent (in), optional :: to, qo

   real :: tvo, tvm, tv, dz, arg

   if (trace_use) call da_trace_entry("da_intpsfc_prs")

   ! 1.  model and observation virtual temperature
   ! ---------------------------------------------

   tvm = tm  * (1.0 + 0.608 * qm)
   if (present(to) .and. present(qo)) then
      tvo = to  * (1.0 + 0.608 * qo)
   else if (present(to) .and. .not.present(qo)) then
      tvo = to
   else
      tvo = tvm
   end if

   tv  = 0.5 * (tvm + tvo)

   ! 2. height difference bewteen model surface and observations
   ! ------------------------------------------------------------

   dz = hm - ho
 
   ! 3.  extrapolate pressure obs to model surface
   ! ---------------------------------------------

   arg = dz * gravity / gas_constant
   arg = arg    / tv 

   val = po * exp (-arg)

   if (trace_use) call da_trace_exit("da_intpsfc_prs")

end subroutine da_intpsfc_prs


