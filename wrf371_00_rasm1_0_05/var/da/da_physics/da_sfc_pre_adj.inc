subroutine da_sfc_pre_adj (psfcm_prime, psm_prime, tsm_prime, qsm_prime, &
   psm, tsm, qsm, hsm, ho, to, qvo)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   !---------------------------------------------------------------------------
   !
   ! Correct pressure between two levels. 
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

   ! Perturbation:
   real, intent (in)     :: psfcm_prime          ! model pressure at ho
   real, intent (inout)  :: psm_prime, tsm_prime, qsm_prime            ! model surface p, t, q 
   ! Basic state:
   real, intent (in)     :: psm, tsm, qsm        ! model pressure at ho and
                                                 ! model surface p, t, q 
   ! Constant variables:
   real, intent (in)           :: hsm, ho
   real, intent (in), optional :: to, qvo
   ! working array:
   real                 :: tvo, tvsm, tv, dz, arg0
   real                 :: tvsm_prime, tvo_prime, tv_prime, arg, arg_prime

   real, parameter      :: GASR =  gas_constant
   real, parameter      :: G = gravity

   if (trace_use) call da_trace_entry("da_sfc_pre_adj")

   !---------------------------------------------------------------------------
   ! 1.0 Basic state
   ! --------------------------------------------------------------------------

   ! 1.1  MODEL AND OBSERVATION VIRTUAL TEMPERATURE
   ! ---------------------------------------------

   tvsm = tsm  * (1.0 + 0.608 * qsm)

   if (present(to) .and. present(qvo)) then
      tvo = to  * (1.0 + 0.608 * qvo)
   else if (present(to) .and. .not.present(qvo)) then
      tvo = to
   else
      tvo = tvsm
   end if

   ! 1.2  Mean virtual temperature
   ! ----------------------------

   tv  = 0.5 * (tvsm + tvo)

   ! 1.3  Compute (g/RTv) * dZ
   ! --------------------------

   dz = hsm - ho
   arg0 = dz * g / gasr     
   arg =  arg0    / tv

   ! ---------------------------------------------------------------------------|
   ! 2.0 Adjoint
   ! ---------------------------------------------------------------------------|

   ! 2.1  psfcm_prime ==> psm_prime, arg_prime
   ! -----------------------------------------

   arg_prime = exp(arg) * psm * psfcm_prime
   psm_prime = exp(arg) * psfcm_prime + psm_prime

   ! 2.2 arg_prim ==> tv_prime
   ! -------------------------

   tv_prime = - arg0 * arg_prime / (tv * tv)

   ! 2.3 tv_prime ==> tvsm_prime, tvo_prime
   ! --------------------------------------

   tvsm_prime = 0.5 * tv_prime
   tvo_prime  = 0.5 * tv_prime

   ! 2.4 tvo_prime ==> tsm_prime
   ! ---------------------------

   if (present(to) .and. present(qvo)) then
      tvo_prime = 0.0
   else if (present(to) .and. .not.present(qvo)) then
      tvo_prime = 0.0
   else
      tvsm_prime = tvo_prime + tvsm_prime
   end if

   ! 2.5 tvsm_prime ==>  tsm_prime, qsm_prime
   ! ----------------------------------------

   tsm_prime = tvsm_prime * (1.0 + 0.608 * qsm) + tsm_prime
   qsm_prime = tvsm_prime * tsm * 0.608 + qsm_prime

   if (trace_use) call da_trace_exit("da_sfc_pre_adj")

end subroutine da_sfc_pre_adj


