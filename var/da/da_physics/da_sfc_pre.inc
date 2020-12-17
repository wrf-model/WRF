subroutine da_sfc_pre (psfcm, psm, tsm, qsm, hsm, ho, to, qvo)

   !-----------------------------------------------------------------------
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
   !---------------------------------------------------------------------------

   implicit none

   real, intent (out)   :: psfcm   ! model pressure at ho
   real, intent (in)    :: psm, tsm, qsm

   real, intent (in)           :: hsm, ho
   real, intent (in), optional :: to, qvo

   real                 :: tvo, tvsm, tv, dz, arg0, arg

   real, parameter      :: GASR =  gas_constant
   real, parameter      :: G = gravity

   if (trace_use) call da_trace_entry("da_sfc_pre")

   ! 1.  MODEL AND OBSERVATION VIRTUAL TEMPERATURE
   ! ---------------------------------------------

   tvsm = tsm  * (1.0 + 0.608 * qsm)
   if (present(to) .and. present(qvo)) then
      tvo = to  * (1.0 + 0.608 * qvo)
   else if (present(to) .and. .not.present(qvo)) then
      tvo = to
   else
      tvo = tvsm
   end if

   tv  = 0.5 * (tvsm + tvo)

   ! 2. HEIGHT DifFERENCE BEWTEEN MODEL SURFACE AND OBSERVATIONS
   ! ------------------------------------------------------------

   dz = hsm - ho
   arg0 = dz * g / gasr

   ! 3.  EXTRAPOLATE PRESSURE OBS TO MODEL SURFACE
   ! ---------------------------------------------

   arg = arg0    / tv 

   psfcm = psm * exp (arg)

   if (trace_use) call da_trace_exit("da_sfc_pre")

end subroutine da_sfc_pre


