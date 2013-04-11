
subroutine da_sfc_wtq_adj (psfc, tg, ps, ts, qs, us, vs, regime,& 
   psfc_prime, tg_prime, ps_prime, ts_prime, qs_prime, &
   us_prime, vs_prime,  hs, roughness, xland, dx,      & 
   u10_prime, v10_prime, t2_prime, q2_prime) 

   !---------------------------------------------------------------------------
   ! Purpose: Calculate the  10m wind, 2m temperature and moisture based on the
   ! similarity theory
   !
   ! Reference:
   ! ---------
   !
   !  input Variables(basic state):
   ! 
   !   psfc, tg               : surface pressure and ground temperature
   !   ps, ts, qs, us, vs, hs : model variable at lowlest half sigma level
   !   regime                 : PBL regime
   !
   !  input Variables(pertubation):
   ! 
   !   psfc_prime, tg_prime   : Surface pressure and ground temperature
   !   ps_prime, ts_prime,    : Model variables at the lowest half sigma
   !   qs_prime, us_prime,    : level 
   !   vs_prime               : 
   !
   !  Constants:
   !
   !   hs                     : height at the lowest half sigma level
   !   roughness              : roughness
   !   xland                  : land-water-mask (=2 water, =1 land)
   !
   !  output Variables(pertubation):
   !
   !   u10_prime, v10_prime   : 10-m high observed wind components
   !   t2_prime , q2_prime    : 2-m high observed temperature and mixing ratio
   !
   !---------------------------------------------------------------------------
   !  
   !                      psim  : mechanical psi at lowlest sigma level
   !                      psim2 : mechanical psi at 2m 
   !                      psimz : mechanical psi at 10m 
   !
   !---------------------------------------------------------------------------

   implicit none

   real, intent (in)          :: regime
   real, intent (in)          :: ps , ts , qs , us, vs, psfc, tg
   real, intent (in)          :: hs, roughness, xland
   real, intent (in)          :: u10_prime, v10_prime, t2_prime, q2_prime
   real, intent (inout)       :: ps_prime, ts_prime, qs_prime, us_prime, vs_prime, psfc_prime, tg_prime

   ! Maximum number of iterations in computing psim, psih

   integer, parameter :: k_iteration = 10 
   !      integer, parameter :: k_iteration = 1

   ! h10 is the height of 10m where the wind observed
   ! h2  is the height of 2m where the temperature and 
   !        moisture observed.

   real, parameter :: h10 = 10.0, h2 = 2.0
   !
   ! Default roughness over the land

   real, parameter :: zint0 = 0.01 
   !
   ! Von Karman constant

   real, parameter :: k_kar = 0.4
   !
   ! Working variables

   real :: Vc2, Va2, V2 
   real :: rib, rcp, xx, yy, cc, Pi
   real :: psiw, psiz, mol, ust, hol, holz, hol2
   real :: psim, psimz, psim2, psih, psihz, psih2
   real :: psit,  psit2, psiq, psiq2
   real :: gzsoz0, gz10oz0, gz2oz0
   real :: eg, qg, tvg, tvs
   real :: ths, thg, thvs, thvg
   real :: vsgd, vsgd2, dx                 ! Add by Eric Chiang (AUG 2010)
   real :: zq0, z0
   real :: ust_s, hol_s, psim_s, psim2_s, psimz_s, psih_s, psih2_s, psihz_s

   real :: Vc2_prime, Va2_prime, V2_prime
   real :: rib_prime, xx_prime, yy_prime
   real :: psiw_prime, psiz_prime, mol_prime, ust_prime, &
           hol_prime, holz_prime, hol2_prime
   real :: psim_prime, psimz_prime, psim2_prime, &
           psih_prime, psihz_prime, psih2_prime
   real :: psit_prime, psit2_prime, &
           psiq_prime, psiq2_prime
   real :: qg_prime, tvg_prime, tvs_prime
   real :: ths_prime, thg_prime, thvs_prime, thvg_prime

   real, parameter :: ka = 2.4E-5

   integer :: iregime

   if (trace_use) call da_trace_entry("da_sfc_wtq_adj")

   !-----------------------------------------------------------------------------!
   !  initialize perturbation value
   !------- ----------------------------------------------------------------------!
   !        tg_prime = 0.0
   !        us_prime = 0.0
   !        vs_prime = 0.0
   !        ts_prime = 0.0
   !        ps_prime = 0.0
   !        qs_prime = 0.0
   !      psfc_prime = 0.0

   psim_prime = 0.0
   psimz_prime = 0.0
   psim2_prime = 0.0
   psih2_prime = 0.0
   psihz_prime = 0.0
   psih_prime = 0.0

   psiw_prime = 0.0
   psiz_prime = 0.0
   psit_prime = 0.0
   psit2_prime = 0.0
   psiq_prime = 0.0
   psiq2_prime = 0.0

   qg_prime = 0.0
   ths_prime = 0.0
   thg_prime = 0.0

   thvs_prime = 0.0
   thvg_prime = 0.0
   V2_prime = 0.0
   rib_prime = 0.0
   ust_prime = 0.0
   tvg_prime = 0.0
   tvs_prime = 0.0
   va2_prime = 0.0
   vc2_prime = 0.0

   !  +++++++++++++++++++++++++++++++++ 
   ! A.0  Calculate Basic state
   !  +++++++++++++++++++++++++++++++++ 
   rcp = gas_constant/cp

   ! 1 Compute the roughness length based upon season and land use 
   ! =====================================

   ! 1.1 Define the rouhness length
   !     -----------------

   z0 = roughness

   if (z0 < 0.0001) z0 = 0.0001

   ! 1.2 Define the rouhgness length for moisture
   !     -----------------

   if (xland .ge. 1.5) then
      zq0 = z0
   else
      zq0 =  zint0
   end if

   ! 1.3 Define the some constant variable for psi
   !     -----------------

   gzsoz0 = log(hs/z0)

   gz10oz0 = log(h10/z0)

   gz2oz0 = log(h2/z0)


   ! 2.0 Calculate the virtual temperature
   ! =====================================

   ! 2.1 Compute Virtual temperature on the lowest half sigma level
   !     ---------------------------------------------------------

   tvs  = ts * (1.0 + 0.608 * qs)

   ! 2.2 Convert ground virtual temperature assuming it's saturated
   !     ----------------------------------------------------------
   call da_tp_to_qs(tg, psfc, eg, qg)
   tvg  = tg * (1.0 + 0.608 * qg)

   ! 3.0  Compute the potential temperature and virtual potential temperature
   ! =======================================================================

   ! 3.1 Potential temperature on the lowest half sigma level
   !     ----------------------------------------------------

   Pi = (100000.0 / ps) ** rcp
   ths  = ts * Pi

   ! 3.2 Virtual potential temperature on the lowest half sigma level
   !     ------------------------------------------------------------

   thvs = tvs * Pi

   ! 3.3 Potential temperature at the ground
   !     -----------------------------------

   Pi =  (100000.0 / psfc) ** rcp
   thg  = tg * Pi

   ! 3.4 Virtual potential temperature at ground
   !     ---------------------------------------

   thvg = tvg * Pi


   ! 4.0  BULK RICHARDSON NUMBER AND MONI-OBUKOV LENGTH
   ! =================================================

   ! 4.1 Velocity
   !     --------
   
   !     Wind speed:

   Va2 =   us*us + vs*vs
     
   !     Convective velocity:

   if (thvg >= thvs) then
      Vc2 = 4.0 * (thvg - thvs)
   else
      Vc2 = 0.0
   end if

   ! Calculate Mahrt and Sun low-res correction         ! Add by Eric Chiang ( AUG 2010 )
   vsgd = 0.32 * (max(dx/5000.-1.,0.))**0.33            ! Add by Eric Chiang ( AUG 2010 )
   vsgd2 = vsgd * vsgd                                  ! Add by Eric Chiang ( AUG 2010 )
   
   V2  = Va2 + Vc2 + vsgd2                              ! Modified by Eric Chiang ( AUG 2010 ) 

   ! 4.2 Bulk richardson number
   !     ----------------------

   rib = (gravity * hs / ths) * (thvs - thvg) / V2
   ! 5.0  CALCULATE PSI BASED UPON REGIME
   ! =======================================

   iregime = int(regime)
   select case (iregime) 

   ! 5.1 Stable conditions (REGIME 1)
   !     ---------------------------

   case (1);

      psim = -10.0*gzsoz0
      psimz = -10.0*gz10oz0
      psim2 = -10.0*gz2oz0

      psim_s  = psim
      psimz_s = psimz
      psim2_s = psim2

      psim  = max(psim,-10.0)
      psimz = max(psimz,-10.0)
      psim2 = max(psim2,-10.0)

      psih = psim
      psihz = psimz
      psih2 = psim2

   ! 5.2 Mechanically driven turbulence (REGIME 2)
   !     ------------------------------------------

   case (2);
      Pi =  (-5.0 * rib) / (1.1 - 5.0*rib)
      psim  = gzsoz0  * Pi
      psimz = gz10oz0 * Pi
      psim2 = gz2oz0  * Pi
      psim_s  = psim
      psimz_s = psimz
      psim2_s = psim2
      if (psim >= -10.0) then
          psim = psim
      else
         psim = -10.0
      end if
      if (psimz >= -10.0) then
         psimz = psimz
      else
         psimz = -10.0
      end if
      if (psim2 >= -10.0) then
         psim2 = psim2
      else
         psim2 = -10.0
      end if

      psih = psim
      psihz = psimz
      psih2 = psim2

   ! 5.3 Unstable Forced convection (REGIME 3)
   !     -------------------------------------

   case (3);

      psim = 0.0
      psimz = 0.0
      psim2 = 0.0
      psih = psim
      psihz = psimz
      psih2 = psim2


   ! 5.4 Free convection (REGIME 4)
   !     --------------------------

   case (4);

      ! Calculate psi m and pshi h using iteration method

      psim = 0.0
      psih = 0.0
      cc = 2.0 * atan(1.0)
      !
      !        do k = 1 , k_iteration

      ! 5.4.1  Calculate   ust, m/L (mol), h/L (hol)
      !        --------------------------

      ! Friction speed

      ust = k_kar * sqrt(v2) /(gzsoz0 - psim)

      ! save ust for adjoint:
      ust_s = ust

      ! Heat flux factor

      mol = k_kar * (ths - thg)/(gzsoz0 - psih)

      ! Ratio of PBL height to Monin-Obukhov length

      if (ust .LT. 0.01) then
         hol = rib * gzsoz0
      else
         hol = k_kar * gravity * hs * mol / (ths * ust * ust)
      end if

      ! 5.4.2  Calculate n, nz, R, Rz
      !        --------------------------

      hol_s = hol

      if (hol >= 0.0) then
         hol = 0.0
      else
         hol = hol
      end if
      if (hol >= -10.0) then
         hol = hol
      else
         hol = -10.0
      end if

      holz = (h10 / hs) * hol
      if (holz >= 0.0) then
         holz = 0.0
      else
         holz = holz
      end if
      if (holz >= -10.0) then
         holz = holz
      else
         holz = -10.0
      end if

      hol2 = (h2 / hs) * hol
      if (hol2 >= 0.0) then
         hol2 = 0.0
      else
         hol2 = hol2
      end if
      if (hol2 >= -10.0) then
         hol2 = hol2
      else
         hol2 = -10.0
      end if

      ! 5.4.3 Calculate Psim & psih
      !        --------------------------

      ! Using the continuous function:
      xx = (1.0 - 16.0 * hol) ** 0.25
      yy = log((1.0+xx*xx)/2.0)
      psim = 2.0 * log((1.0+xx)/2.0) + yy - 2.0 * atan(xx) + cc
      psih = 2.0 * yy

      ! Using the continuous function:
      xx = (1.0 - 16.0 * holz) ** 0.25
      yy = log((1.0+xx*xx)/2.0)
      psimz = 2.0 * log((1.0+xx)/2.0) + yy - 2.0 * atan(xx) + cc
      psihz = 2.0 * yy

      ! Using the continuous function:
      xx = (1.0 - 16.0 * hol2) ** 0.25
      yy = log((1.0+xx*xx)/2.0)
      psim2 = 2.0 * log((1.0+xx)/2.0) + yy - 2.0 * atan(xx) + cc
      psih2 = 2.0 * yy

      ! end do 

      ! 5.4.4 Define the limit value for psim & psih
      !        --------------------------
 
      !  Save the values for adjoint:

      psim_s  = psim
      psimz_s = psimz
      psim2_s = psim2
      psih_s  = psih
      psihz_s = psihz
      psih2_s = psih2

      if (psim <= 0.9*gzsoz0) then
         psim = psim
      else
         psim = 0.9*gzsoz0
      end if
      if (psimz <= 0.9*gz10oz0) then
         psimz = psimz
      else
         psimz = 0.9*gz10oz0
      end if
      if (psim2 <= 0.9*gz2oz0) then
         psim2 = psim2
      else
         psim2 = 0.9*gz2oz0
      end if
      if (psih <= 0.9*gzsoz0) then
         psih = psih
      else
         psih = 0.9*gzsoz0
      end if
      if (psihz <= 0.9*gz10oz0) then
         psihz = psihz
      else
         psihz = 0.9*gz10oz0
      end if
      if (psih2 <= 0.9*gz2oz0) then
         psih2 = psih2
      else
         psih2 = 0.9*gz2oz0
      end if

   case default;
      write(unit=message(1),fmt='(A,I2,A)') &
         "Regime=",iregime," is invalid."
      call da_error(__FILE__,__LINE__,message(1:1))

   end select
   
   ! 6.0  CALCULATE PSI FOR WinD, TEMPERATURE AND MOISTURE
   ! =======================================

   psiw = gzsoz0 - psim
   psiz = gz10oz0 - psimz
   psit = gzsoz0 - psih
   psit2 = gz2oz0 - psih2
   ust = k_kar * sqrt(v2) /(gzsoz0 - psim)
   psiq  = log(k_kar*ust*hs/ka + hs / zq0) - psih
   psiq2 = log(k_kar*ust*h2/ka + h2 / zq0) - psih2
   !  +++++++++++++++++++++++++++++++++ 
   !  B.0  Calculate adjoint solution
   !  +++++++++++++++++++++++++++++++++ 

   ! 7.0  CALCULATE 10M WinD, 2M TEMPERATURE AND MOISTURE
   ! =======================================

   qg_prime    = (1.0 - psiq2/psiq) * q2_prime
   qs_prime    = qs_prime + psiq2/psiq * q2_prime
   psiq2_prime =  (qs -qg)/psiq * q2_prime
   psiq_prime  = -(qs -qg)*psiq2/(psiq*psiq) * q2_prime
   ! q2_prime = 0.0
   
   Pi = (psfc/100000.0)**rcp
   thg_prime   = (1.0 - psit2/psit) * Pi * t2_prime
   ths_prime   = (psit2/psit) * Pi * t2_prime
   psit2_prime =   (ths - thg)/psit * Pi * t2_prime
   psit_prime  = - (ths - thg)*psit2/(psit*psit) * Pi * t2_prime
   psfc_prime  = psfc_prime + Pi * rcp*(thg + (ths - thg)*psit2/psit)/psfc * t2_prime 
   ! t2_prime = 0.0
   
   Pi = psiz / psiw
   vs_prime   = vs_prime + Pi * v10_prime
   psiz_prime =   vs / psiz * Pi * v10_prime
   psiw_prime = - vs / psiw * Pi * v10_prime
   ! v10_prime = 0.0 
   
   us_prime   = us_prime + Pi * u10_prime
   psiz_prime =  psiz_prime + us / psiz * Pi * u10_prime
   psiw_prime =  psiw_prime - us / psiw * Pi * u10_prime
   ! u10_prime = 0.0
   
   ! 6.0  CALCULATE PSI FOR WinD, TEMPERATURE AND MOISTURE
   ! =======================================
   
   ! moisture part:
   psih2_prime = - psiq2_prime
   psih_prime  = - psiq_prime
   psiq2_prime = psiq2_prime + psiq_prime
   ust_prime   = k_kar*hs/(ka*(k_kar*ust*hs/ka + hs / zq0)) * psiq2_prime 
   
   v2_prime   = 0.5/V2 * ust * ust_prime
   psim_prime = ust / (gzsoz0 - psim) * ust_prime
   ust_prime = 0.0

   ! temperature part:
   psih2_prime = psih2_prime - psit2_prime
   psih_prime  = psih_prime  - psit_prime

   ! wind part:
   psimz_prime = psimz_prime - psiz_prime
   psim_prime  = psim_prime  - psiw_prime

   ! 5.0  CALCULATE PSI BASED UPON REGIME
   ! =======================================
   select case (iregime) 

   ! 5.1 Stable conditions (REGIME 1)
   !     ---------------------------

   case (1);

      psim2_prime = psim2_prime + psih2_prime
      psimz_prime = psimz_prime + psihz_prime
      psim_prime  = psim_prime  + psih_prime
      psim_prime  = 0.0
      psimz_prime = 0.0
      psim2_prime = 0.0

   ! 5.2 Mechanically driven turbulence (REGIME 2)
   !      ------------------------------------------

   case (2);

     psim2_prime = psim2_prime + psih2_prime
     psimz_prime = psimz_prime + psihz_prime
     psim_prime  = psim_prime  + psih_prime

     psim  = psim_s
     psimz = psimz_s
     psim2 = psim2_S

     if (psim2 >= -10.0) then
        psim2_prime = psim2_prime
     else
        psim2_prime = 0.0
     end if
     if (psimz >= -10.0) then
        psimz_prime = psimz_prime
     else
        psimz_prime = 0.0
     end if
     if (psim >= -10.0) then
        psim_prime = psim_prime
     else
        psim_prime = 0.0
     end if

     Pi = -1.0 / ((1.1 - 5.*rib)*(1.1 - 5.0*rib))
     rib_prime =             5.5 * gz2oz0  * psim2_prime * Pi
     rib_prime = rib_prime + 5.5 * gz10oz0 * psimz_prime * Pi
     rib_prime = rib_prime + 5.5 * gzsoz0  * psim_prime  * Pi

     ! 5.3 Unstable Forced convection (REGIME 3)
     !     -------------------------------------

   case (3);

      psim2_prime = psih2_prime
      psimz_prime = psihz_prime
      psim_prime  = psih_prime

      psim2_prime = 0.0
      psimz_prime = 0.0
      psim_prime  = 0.0

   ! 5.4 Free convection (REGIME 4)
   !     --------------------------

   case (4);

      ! 5.4.4 Define the limit value for psim & psih
      !        -------------------------------------

      ! Recover the values:

      psim = psim_s
      psim2 = psim2_s
      psimz = psimz_s
      psihz = psihz_s
      psih  = psih_s
      psih2 = psih2_s

      if (psih2 <= 0.9*gz2oz0) then
         psih2_prime = psih2_prime
      else
         psih2_prime = 0.0
      end if
      if (psihz <= 0.9*gz10oz0) then
         psihz_prime = psihz_prime
      else
         psihz_prime = 0.0
      end if
      if (psih <= 0.9*gzsoz0) then
         psih_prime = psih_prime
      else
         psih_prime = 0.0
      end if
      if (psim2 <= 0.9*gz2oz0) then
         psim2_prime = psim2_prime
      else
         psim2_prime = 0.0
      end if
      if (psimz <= 0.9*gz10oz0) then
         psimz_prime = psimz_prime
      else
         psimz_prime = 0.0
      end if
      if (psim <= 0.9*gzsoz0) then
         psim_prime = psim_prime
      else
         psim_prime = 0.0
      end if

      ! 5.4.3 Calculate Psim & psih
      !        --------------------------

      ! Recover ust:
      ust = ust_s
      psim = 0.0
      psih = 0.0

      xx = (1.0 - 16.0 * hol2) ** 0.25
      yy = log((1.0+xx*xx)/2.0)
      yy_prime = 2.0 * psih2_prime
      psih2_prime = 0.0
   
      xx_prime = 2.0*(1.0/(1.0+xx)-1.0/(1+xx*xx)) * psim2_prime
      yy_prime = yy_prime + psim2_prime
      psim2_prime = 0.0
  
      xx_prime = xx_prime + 2.0* xx/(1.0+xx*xx) * yy_prime
      yy_prime = 0.0
      hol2_prime = -4.0/((1.0 - 16.0 * hol2) ** 0.75) * xx_prime
      xx_prime = 0.0

      xx = (1.0 - 16.0 * holz) ** 0.25
      yy = log((1.0+xx*xx)/2.0)
      yy_prime = 2.0 * psihz_prime
      psihz_prime = 0.0
      
      xx_prime = 2.0*(1.0/(1.0+xx)-1.0/(1+xx*xx)) * psimz_prime
      yy_prime = yy_prime + psimz_prime
      psimz_prime = 0.0
      
      xx_prime = xx_prime + 2.0* xx/(1.0+xx*xx) * yy_prime
      yy_prime = 0.0
      holz_prime = -4.0/((1.0 - 16.0 * holz) ** 0.75) * xx_prime
      xx_prime = 0.0

      xx = (1.0 - 16.0 * hol) ** 0.25
      yy = log((1.0+xx*xx)/2.0)
      yy_prime = 2.0 * psih_prime
      psih_prime = 0.0
      
      xx_prime = 2.0*(1.0/(1.0+xx)-1.0/(1+xx*xx)) * psim_prime
      yy_prime = yy_prime + psim_prime
      psim_prime = 0.0
      
      xx_prime = xx_prime + 2.0* xx/(1.0+xx*xx)*yy_prime
      yy_prime = 0.0
      hol_prime = -4.0/((1.0 - 16.0 * hol) ** 0.75)*xx_prime
      xx_prime = 0.0

      ! 5.4.2  Calculate n, nz, R, Rz
      !        --------------------------

      !    Recover the values:

      hol = hol_s

      hol2 = (h2 / hs) * hol
      if (hol2 >= -10.0) then
         hol2_prime = hol2_prime
      else
         hol2_prime = 0.0
      end if
      if (hol2 >= 0.0) then       
         hol2_prime = 0.0
      else
         hol2_prime = hol2_prime
      end if
      
      hol_prime = hol_prime + (h2 / hs) * hol2_prime
      hol2_prime = 0.0
      
      holz = (h10 / hs) * hol
      if (holz >= -10.0) then
         holz_prime = holz_prime
      else
         holz_prime = 0.0
      end if
      if (holz >= 0.0) then
         holz_prime = 0.0
      else
         holz_prime = holz_prime
      end if
      
      hol_prime = hol_prime + (h10 / hs) * holz_prime
      holz_prime = 0.0
      
      if (hol >= -10.0) then
         hol_prime = hol_prime
      else
         hol_prime = 0.0
      end if
      if (hol >= 0.0) then
         hol_prime = 0.0
      else
         hol_prime = hol_prime
      end if

      ! 5.4.1  Calculate   ust, m/L (mol), h/L (hol)
      !        --------------------------

      !       Ratio of PBL height to Monin-Obukhov length
      if (ust .LT. 0.01) then
         rib_prime = hol_prime * gzsoz0
         hol_prime = 0.0
      else
         mol_prime =        hol / mol * hol_prime
         ths_prime = ths_prime - hol / ths * hol_prime
         ust_prime = - 2.0 * hol / ust * hol_prime
         hol_prime = 0.0
      end if

      ! Heat flux factor
      ths_prime  = ths_prime + mol/(ths - thg) * mol_prime
      thg_prime  = thg_prime - mol/(ths - thg) * mol_prime
      psih_prime = psih_prime + mol/(gzsoz0 - psih) * mol_prime
      mol_prime = 0.0

      ! Friction speed

      v2_prime   = V2_prime + 0.5 * ust/V2 * ust_prime 
      psim_prime = psim_prime + ust/(gzsoz0 - psim) * ust_prime 
      ust_prime = 0.0

      ! Calculate psi m and pshi h using iteration method

      psim_prime = 0.0
      psih_prime = 0.0

   case default;
      write(unit=message(1),fmt='(A,I2,A)') &
         "Regime=",iregime," is invalid."
      call da_error(__FILE__,__LINE__,message(1:1))

   end select

   ! 4.0  BULK RICHARDSON NUMBER AND MONI-OBUKOV LENGTH
   ! =================================================

   ! 4.2 Bulk richardson number
   !     ----------------------

   Pi = gravity * hs / (ths*V2)
   ths_prime = ths_prime - Pi * (thvs-thvg)/ths * rib_prime
   V2_prime  = V2_prime  - Pi * (thvs-thvg)/V2 * rib_prime
   thvs_prime = thvs_prime + Pi * rib_prime
   thvg_prime = thvg_prime - Pi * rib_prime
   rib_prime = 0.0
   
   ! 4.1 Velocity
   !     --------
   
   ! Convective velocity:

   Va2_prime = V2_prime
   Vc2_prime = V2_prime

   if (thvg >= thvs) then
     thvg_prime = thvg_prime + 4.0 * Vc2_prime
     thvs_prime = thvs_prime - 4.0 * Vc2_prime
     Vc2_prime = 0.0
    else
     Vc2_prime = 0.0
   end if

   ! Wind speed:

   us_prime = us_prime + 2.0 *us * Va2_prime
   vs_prime = vs_prime + 2.0 *vs * Va2_prime
   Va2_prime = 0.0

   ! 3.0 Virtual potential temperature
   ! ==================================

   ! 3.4 Virtual potential temperature at ground
   !     ---------------------------------------

   Pi = (100000.0 / psfc) ** rcp
   tvg_prime = tvg_prime +  Pi * thvg_prime
   psfc_prime = psfc_prime - thvg_prime * rcp * tvg/psfc * Pi
   thvg_prime = 0.0

   ! 3.3 Potential temperature at the ground
   !     -----------------------------------

   tg_prime = tg_prime + Pi * thg_prime 
   psfc_prime = psfc_prime - thg_prime *rcp * tg/psfc * Pi
   thg_prime = 0.0

   ! 3.2 Virtual potential temperature on the lowest half sigma level
   !     ------------------------------------------------------------

   Pi = (100000.0 / ps) ** rcp
   tvs_prime = tvs_prime + PI * thvs_prime
   ps_prime = ps_prime - thvs_prime *rcp * tvs/ps * Pi
   thvs_prime = 0.0

   ! 3.1 Potential temperature on the lowest half sigma level
   !     ----------------------------------------------------
   ts_prime = ts_prime + Pi * ths_prime
   ps_prime = ps_prime - ths_prime *  rcp * ts/ps * Pi
   ths_prime = 0.0

   ! 2.0 Calculate the virtual temperature
   ! =====================================

   ! 2.2 Compute the ground saturated mixing ratio and the ground virtual 
   !     temperature
   !     ----------------------------------------------------------------

   tg_prime = tg_prime + tvg_prime * (1.0 + 0.608 * qg)
   qg_prime = qg_prime + tvg_prime * 0.608 * tg
   tvg_prime = 0.0

   qg_prime = qg_prime * qg
   call da_tp_to_qs_adj1(tg, psfc, eg, tg_prime, psfc_prime, qg_prime)

   ! 2.1 Compute Virtual temperature on the lowest half sigma level
   !     ---------------------------------------------------------

   ts_prime = ts_prime + tvs_prime  * (1.0 + 0.608 * qs)
   qs_prime = qs_prime + tvs_prime *  0.608 * ts
   tvs_prime = 0.0

   if (trace_use) call da_trace_exit("da_sfc_wtq_adj")

end subroutine da_sfc_wtq_adj


