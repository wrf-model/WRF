function da_mo_correction (ho, po, to, qo, &
                        hm, pm, tm, qm, um ,vm, &
                        roughness)   RESULT (correc)

   !----------------------------------------------------------------------------
   ! Purpose: Compute the correction factor to convert surface wind into 40m 
   ! wind using similarity laws.
   !
   ! Reference:
   ! ---------
   !  A description of the fifth generation Penn State/NCAR Mesoscale Model
   !  Grell et al. 1994, page 29-30 and 80-83.
   !----------------------------------------------------------------------------

   implicit none

   real, intent (in)                :: ho, po, to, qo
   real, intent (in)                :: hm, pm, tm, qm, um, vm
   real, intent (in)                :: roughness

   real                             :: correc, winfac

   real :: thm , tho, tvm, tvo, thvm, thvo, rcp
   real :: za, Vc2, Va2, V2 
   ! FIX? real :: hdif, rib, rll, hll, zint
   real :: hdif, rib, hll, zint

   ! Height difference (in m) above wich correction is applied

   real, parameter :: hmax = 10.0, hs_max = 40.0  

   ! Default Roughness length im m (for land, set to 0 if on sea)

   real, parameter :: zint0 = 0.2

   rcp = gas_constant/cp

   ! 0.0  initialize correction factor to 1
   ! =====================================

   correc = 1.0
   winfac = 1.0

   ! 1.  height difference and roughness length
   ! ==========================================

   ! 1.1 Height difference
   !     -----------------

   hdif = hm - ho

   ! 1.2 No correction if height difference is less than hmax. 
   !     -----------------------------------------------------

   if (hdif <= hmax) then
      return
   end if

   ! too many
   ! if (trace_use) call da_trace_entry("da_mo_correction")

   ! 1.3 Compute the roughness length based upon season and land use 
   !     -----------------------------------------------------------

   zint = roughness

   if (zint < 0.0001) zint = 0.0001

   ! 2.  potential temperature at model surface and observation location
   ! ===================================================================

   ! 2.1 potential temperature on model surface
   !     --------------------------------------

   thm  = tm * (1000.0 / (pm/100.0)) ** rcp

   ! 2.2 potential temperature at observation location
   !     ---------------------------------------------

   tho  = to * (1000.0 / (po/100.0)) ** rcp

   ! 3.  virtual temperature at model surface and observation location
   ! ===================================================================

   ! 3.1 Virtual temperature on model surface
   !     -------------------------------------

   tvm  = tm * (1.0 + 0.608 * qm)

   ! 3.2 Virtual temperature at observation location
   !     -------------------------------------------

      tvo  = to * (1.0 + 0.608 * qo)

   ! 4.  potential virtual temperature at model surface and observation location 
   ! ===========================================================================

   ! 4.1 potential virtual temperature on model surface
   !     ----------------------------------------------

   thvm = tvm * (1000.0 / (pm/100.0)) ** rcp

   ! 4.2 potential virtual temperature at observation location
   !     -----------------------------------------------------

   thvo = tvo * (1000.0 / (po/100.0)) ** rcp


   ! 5.  bulk richardson number and moni-obukov length
   ! =================================================

   ! 5.1 Pre-calculations
   !     ----------------

   za  = hm - ho

   ! Because the following formula is derived based on
   ! the surface layer height is hs_max=40m. Under
   ! free convection, we assume that atmospheric state
   ! above the surface layer is fully mixed, and the
   ! wind at the lowest sigma half level is same as the
   ! wind at top of the surface layer. 

   if (za > hs_max) za = hs_max
      
   Va2 =   um*um + vm*vm
   Vc2 = 4.0 * MAX ((tho - thm), 0.0)

   V2  = Va2 + Vc2

   ! 5.2 Bulk richardson number
   !     ----------------------

   rib = (gravity * za / thm) * (thvm - thvo) / V2

   ! 5.3 Monin-obukov length
   !     -------------------

      hll = rib * LOG (za/zint)

   ! FIX? is this right?
   ! rll = za / hll

   ! 5.4 Ratio PBL height/Monin Obukov length: za/rll
   !     ------------------------------------

   hll =  ABS (hll)


   ! 6.  CORRECTION FACTOR BASED UPON REGIME
   ! =======================================

   ! 6.1 Stable conditions (REGIME 1)
   !     ---------------------------
 
   ! correc = 1.0      !  rib > 0.2

   ! 6.2 Mechanically driven turbulence (REGIME 2)
   !     ------------------------------------------

   ! correc = 1.0      !  0.0 =< rib <= 0.2

   correc = 1.0

   if (rib < 0.0) then

      ! 6.3 Unstable Forced convection (REGIME 3)
      !     -------------------------------------

      ! correc = 1.0  !   hll <= 1.5


      ! 6.4 Free convection (REGIME 4)
      !     --------------------------

      if (hll > 1.5) then
         if (zint < 0.2) then
            correc = 1.000 + 0.320 * zint ** 0.200
         else if (zint >= 0.2) then
            correc = 1.169 + 0.315 * zint
         end if

         ! 6.4.1 The factor depended on Za (MM5, param.F)
      
         winfac = 0.5*(log(za/0.05)/log(40.0/0.05) &
                       + log(za)/log(40.0))

         correc =  correc * winfac
      end if
   end if

   ! if (trace_use) call da_trace_exit("da_mo_correction")

end function da_mo_correction


