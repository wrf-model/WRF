subroutine da_sfc_wtq (psfc, tg, ps, ts, qs, us, vs, &
   hs, roughness, xland, dx, u10, v10, t2, q2, regime, &
   has_lsm, regime_wrf, qsfc_wrf, znt_wrf, ust_wrf, mol_wrf, hfx, qfx, pblh)

   !---------------------------------------------------------------------------
   ! Purpose: Calculate the  10m wind, 2m temperature and moisture based on the
   ! similarity theory/
   !
   !  The unit for pressure   : psfc, ps          is Pa.
   !  The unit for temperature: tg, ts, t2        is K.
   !  The unit for moisture   : qs, q2            is kg/kg.
   !  The unit for wind       : us, vs, u10, v10  is m/s.
   !  The unit for height     : hs, roughness     is m.
   !  xland and regime are dimensionless.
   !
   ! History: Nov 2010 - improve calculation consistency with WRF model (Eric Chiang)
   !          Jul 2015 - further improvement on consistency
   !
   ! Reference:
   ! ---------
   !
   !  input Variables:
   ! 
   !   psfc, tg               : surface pressure and ground temperature
   !   ps, ts, qs, us, vs, hs : model variable at lowlest half sigma level
   !   dx  (m)                : horizontal resolution
   !
   !
   !  Constants:
   !
   !   hs                     : height at the lowest half sigma level
   !   roughness              : roughness
   !   xland                  : land-water-mask (=2 water, =1 land)
   !
   !  output Variables:
   !
   !   regime                 : PBL regime
   !   u10, v10               : 10-m high observed wind components
   !   t2 , q2                : 2-m high observed temperature and mixing ratio
   !
   !---------------------------------------------------------------------------
   !  
   !                      psim  : mechanical psi at lowlest sigma level
   !                      psim2 : mechanical psi at 2m 
   !                      psimz : mechanical psi at 10m 
   !
   !---------------------------------------------------------------------------

   implicit none

   real, intent (in)  :: ps , ts , qs , us, vs
   real, intent (in)  :: psfc, tg
   real, intent (in)  :: hs, roughness, xland
   real, intent (out) :: regime
   real, intent (out) :: u10, v10, t2, q2
   logical, intent(in), optional :: has_lsm
   real,    intent(in), optional :: regime_wrf, qsfc_wrf, znt_wrf, ust_wrf, mol_wrf
   real,    intent(in), optional :: hfx, qfx, pblh

   logical :: use_table = .true.
   logical :: use_ust_wrf = .false.
   logical :: vconv_wrf
   integer :: nn, nz, n2
   real    :: rr, rz, r2
   real    :: cqs2, chs2, rho, rhox, fluxc, visc, restar, z0t, z0q

   ! h10 is the height of 10m where the wind observed
   ! h2  is the height of 2m where the temperature and 
   !        moisture observed.

   real, parameter :: h10 = 10.0, h2 = 2.0
   
   ! Default roughness over the land

   real, parameter :: zint0 = 0.01 
   
   ! Von Karman constant

   real, parameter :: k_kar = 0.4
   
   ! Working variables

   real :: Vc2, Va2, V2, vc, wspd
   real :: rib, rcp, xx, yy, cc
   real :: psiw, psiz, mol, ust, hol, holz, hol2
   real :: psim, psimz, psim2, psih, psihz, psih2
   real :: psit, psit2, psiq, psiq2
   real :: gzsoz0, gz10oz0, gz2oz0
   real :: eg, qg, tvg, tvs, tvs2
   real :: ths, thg, thvs, thvg, thvs2, vsgd, vsgd2, dx
   real :: zq0, z0

   real, parameter :: ka = 2.4E-5

   if (trace_use_dull) call da_trace_entry("da_sfc_wtq")

   rcp = gas_constant/cp

   ! 1 Compute the roughness length based upon season and land use 

   ! 1.1 Define the roughness length

   z0 = roughness

   if (z0 < 0.0001) z0 = 0.0001

   if ( present(znt_wrf) ) then
      if ( znt_wrf > 0.0 ) then
         z0 = znt_wrf
      end if
   end if

   ! 1.2 Define the rouhgness length for moisture

   if (xland .ge. 1.5) then
      zq0 = z0
   else
      zq0 =  zint0
   end if

   ! 1.3 Define the some constant variable for psi

   gzsoz0 = log(hs/z0)

   gz10oz0 = log(h10/z0)

   gz2oz0 = log(h2/z0)


   ! 2. Calculate the virtual temperature

   ! 2.1 Compute Virtual temperature on the lowest half sigma level

   tvs  = ts * (1.0 + 0.608 * qs)

   ! 2.2 Convert ground virtual temperature assuming it's saturated

   call da_tp_to_qs(tg, psfc, eg, qg) !output qg is specific humidity
   qg = qg*(1.0-qg) !hcl convert to mixing ratio
   if ( present(qsfc_wrf) ) then
      if ( qsfc_wrf > 0.0 ) then
         qg = qsfc_wrf
      end if
   endif

   tvg  = tg * (1.0 + 0.608 * qg)

   ! 3.  Compute the potential temperature

   ! 3.1 Potential temperature on the lowest half sigma level

   ths  = ts * (1000.0 / (ps/100.0)) ** rcp

   ! 3.2 Potential temperature at the ground

   thg  = tg * (1000.0 / (psfc/100.0)) ** rcp

   ! 4. Virtual potential temperature

   ! 4.1 Virtual potential temperature on the lowest half sigma level

   thvs = tvs * (1000.0 / (ps/100.0)) ** rcp

   ! 4.2 Virtual potential temperature at ground

   thvg = tvg * (1000.0 / (psfc/100.0)) ** rcp


   ! 5.  BULK RICHARDSON NUMBER AND MONI-OBUKOV LENGTH

   ! 5.1 Velocity
   
   !     Wind speed:

   Va2 =   us*us + vs*vs
   !  
   !     Convective velocity:

   vconv_wrf = .false.
   if ( present(hfx) .and. present(qfx) .and. present(pblh) ) then
      ! calculating vconv over land following wrf method
      if ( pblh > 0.0 ) then
         vconv_wrf = .true.
      end if
   end if

   if (thvg >= thvs) then
      ! prior to V3.7, Vc2 = 4.0 * (thvg - thvs)
      Vc2 = thvg - thvs
   else
      Vc2 = 0.0
   end if
   if ( xland < 1.5 ) then !land
      if ( vconv_wrf ) then
         ! following the calculation as in module_sf_sfclay.F
         rhox = psfc/(gas_constant*tvg)
         fluxc = max(hfx/rhox/cp+0.608*tvg*qfx/rhox, 0.0)
         vc = (gravity/tg*pblh*fluxc)**0.33
         vc2 = vc*vc
      end if
   end if

   ! Calculate Mahrt and Sun low-res correction         ! Add by Eric Chiang ( July 2010 )
   vsgd = 0.32 * (max(dx/5000.-1.,0.))**0.33            ! Add by Eric Chiang ( July 2010 )
   vsgd2 = vsgd * vsgd                                  ! Add by Eric Chiang ( July 2010 )
   
   V2  = Va2 + Vc2 + vsgd2                              ! Add by Eric Chiang ( July 2010 )  
   wspd = sqrt(v2)
   wspd = max(wspd,0.1)
   v2 = wspd*wspd

   ! 5.2 Bulk richardson number

   rib = (gravity * hs / ths) * (thvs - thvg) / V2

   ! if previously unstable, do not let into regime 1 and 2
   if ( present(mol_wrf) ) then
      if ( mol_wrf < 0.0 ) rib = min(rib, 0.0)
   end if

   !  Calculate   ust, m/L (mol), h/L (hol)

   psim = 0.0
   psih = 0.0

   ! Friction speed

   if ( present(ust_wrf) ) then
      if ( ust_wrf > 0.0 ) then
         use_ust_wrf = .true.
         ust = ust_wrf
      end if
   end if
   if ( .not. use_ust_wrf ) then
      !ust = 0.0001  !init value as in phys/module_physics_init.F
      ust = k_kar * sqrt(v2) /(gzsoz0 - psim)
   end if

   ! Heat flux factor

   if ( present(mol_wrf) ) then
      mol = mol_wrf
   else
      mol = k_kar * (ths - thg)/(gzsoz0 - psih)
      !mol = 0.0
   end if

   ! set regimes based on rib
   if (rib .GE. 0.2) then
      ! Stable conditions (REGIME 1)
      regime = 1.1
   else if ((rib .LT. 0.2) .AND. (rib .GT. 0.0)) then
      ! Mechanically driven turbulence (REGIME 2)
      regime = 2.1
   else if (rib .EQ. 0.0) then
      ! Unstable Forced convection (REGIME 3)
      regime = 3.1
   else 
      ! Free convection (REGIME 4)
      regime = 4.1
   end if

   if ( present(regime_wrf) ) then
      if ( regime_wrf > 0.0 ) then
         regime = regime_wrf
      end if
   end if

   ! 6.  CALCULATE PSI BASED UPON REGIME

   !if (rib .GE. 0.2) then
   if ( nint(regime) == 1 ) then
      ! 6.1 Stable conditions (REGIME 1)
      !     ---------------------------
      regime = 1.1
      psim = -10.0*gzsoz0
      psim = max(psim,-10.0)
      psimz = h10/hs * psim
      psimz = max(psimz,-10.0)
      psim2 = h2 /hs * psim
      psim2 = max(psim2,-10.0)
      psih = psim
      psihz = psimz
      psih2 = psim2

   !else if ((rib .LT. 0.2) .AND. (rib .GT. 0.0)) then
   else if ( nint(regime) == 2 ) then

      ! 6.2 Mechanically driven turbulence (REGIME 2)

      regime = 2.1
      psim = (-5.0 * rib) * gzsoz0 / (1.1 - 5.0*rib)
      psim = max(psim,-10.0)
      psimz = h10/hs * psim
      psimz = max(psimz,-10.0)
      psim2 = h2 /hs * psim
      psim2 = max(psim2,-10.0)
      psih = psim
      psihz = psimz
      psih2 = psim2

   !else if (rib .EQ. 0.0) then
   else if ( nint(regime) == 3 ) then
      ! 6.3 Unstable Forced convection (REGIME 3)

      regime = 3.1
      psim = 0.0
      psimz = 0.0
      psim2 = 0.0
      psih = psim
      psihz = psimz
      psih2 = psim2

   else 
      ! 6.4 Free convection (REGIME 4)
      regime = 4.1

      cc = 2.0 * atan(1.0)

      ! Ratio of PBL height to Monin-Obukhov length

      if (ust .LT. 0.01) then
         hol = rib * gzsoz0
      else
         hol = k_kar * gravity * hs * mol / (ths * ust * ust)
      end if

      ! 6.4.2  Calculate n, nz, R, Rz

      holz = (h10 / hs) * hol
      hol2 = (h2 / hs) * hol

      hol = min(hol,0.0)
      hol = max(hol,-9.9999)

      holz = min(holz,0.0)
      holz = max(holz,-9.9999)

      hol2 = min(hol2,0.0)
      hol2 = max(hol2,-9.9999)

      ! 6.4.3 Calculate Psim & psih

      if ( use_table ) then
         ! Using the look-up table:
         nn = int(-100.0 * hol)
         rr = (-100.0 * hol) - nn
         psim = psimtb(nn) + rr * (psimtb(nn+1) - psimtb(nn))
         psih = psihtb(nn) + rr * (psihtb(nn+1) - psihtb(nn))
      else
         ! Using the continuous function:
         xx = (1.0 - 16.0 * hol) ** 0.25
         yy = log((1.0+xx*xx)/2.0)
         psim = 2.0 * log((1.0+xx)/2.0) + yy - 2.0 * atan(xx) + cc
         psih = 2.0 * yy
      end if

      if ( use_table ) then
         ! Using the look-up table:
         nz = int(-100.0 * holz)
         rz = (-100.0 * holz) - nz
         psimz = psimtb(nz) + rz * (psimtb(nz+1) - psimtb(nz))
         psihz = psihtb(nz) + rz * (psihtb(nz+1) - psihtb(nz))
      else
         ! Using the continuous function:
         xx = (1.0 - 16.0 * holz) ** 0.25
         yy = log((1.0+xx*xx)/2.0)
         psimz = 2.0 * log((1.0+xx)/2.0) + yy - 2.0 * atan(xx) + cc
         psihz = 2.0 * yy
      end if

      if ( use_table ) then
         ! Using the look-up table:
         n2 = int(-100.0 * hol2)
         r2 = (-100.0 * hol2) - n2
         psim2 = psimtb(n2) + r2 * (psimtb(n2+1) - psimtb(n2))
         psih2 = psihtb(n2) + r2 * (psihtb(n2+1) - psihtb(n2))
      else
         ! Using the continuous function:
         xx = (1.0 - 16.0 * hol2) ** 0.25
         yy = log((1.0+xx*xx)/2.0)
         psim2 = 2.0 * log((1.0+xx)/2.0) + yy - 2.0 * atan(xx) + cc
         psih2 = 2.0 * yy
      end if

      ! 6.4.4 Define the limit value for psim & psih

      psim = min(psim,0.9*gzsoz0)
      psimz = min(psimz,0.9*gz10oz0)
      psim2 = min(psim2,0.9*gz2oz0)
      psih = min(psih,0.9*gzsoz0)
      psihz = min(psihz,0.9*gz10oz0)
      psih2 = min(psih2,0.9*gz2oz0)
   end if  ! Regime

   ! 7.  Calculate psi for wind, temperature and moisture

   psiw = gzsoz0 - psim
   psiz = gz10oz0 - psimz
   psit = max(gzsoz0-psih, 2.0)
   psit2 = gz2oz0 - psih2

   if ( .not. use_ust_wrf ) then
      ! re-calculate ust since psim is now available
      ust = k_kar * sqrt(v2) /(gzsoz0 - psim)
   end if

   psiq  = log(k_kar*ust*hs/ka + hs / zq0) - psih
   psiq2 = log(k_kar*ust*h2/ka + h2 / zq0) - psih2

   !V3.7, as in module_sf_sfclay.F
   if ( xland >= 1.5 ) then !water
      visc = (1.32+0.009*(ts-273.15))*1.e-5
      restar = ust*z0/visc
      z0t = (5.5e-5)*(restar**(-0.60))
      z0t = min(z0t,1.0e-4)
      z0t = max(z0t,2.0e-9)
      z0q = z0t
      psiq  = max(log((hs+z0q)/z0q)-psih,  2.)
      psit  = max(log((hs+z0t)/z0t)-psih,  2.)
      psiq2 = max(log((2.+z0q)/z0q)-psih2, 2.)
      psit2 = max(log((2.+z0t)/z0t)-psih2, 2.)
   end if

   ! 8.  Calculate 10m wind, 2m temperature and moisture

   u10 = us * psiz / psiw
   v10 = vs * psiz / psiw
   t2 = (thg + (ths - thg)*psit2/psit)*((psfc/100.0)/1000.0)**rcp
   q2 = qg + (qs - qg)*psiq2/psiq 

   if ( present(has_lsm) ) then
      if ( has_lsm ) then
         !cqs2: 2m surface exchange coefficient for moisture
         !chs2: 2m surface exchange coefficient for heat
         cqs2 = ust*k_kar/psiq2
         if (xland .ge. 1.5) then
            !water
            chs2 = ust*k_kar/psit2
         else
            !land
            chs2 = cqs2 !as in subroutine lsm in phys/module_sf_noahdrv.F
         end if

         !re-calculate T2/Q2 as in module_sf_sfcdiags.F
         rho  = psfc/(gas_constant*tg)
         if ( cqs2 < 1.e-5 ) then
            q2 = qg
         else
            if ( present(qfx) ) then
               q2 = qg - qfx/(rho*cqs2)
            end if
         end if
         if ( chs2 < 1.e-5 ) then
            t2 = tg
         else
            if ( present(hfx) ) then
               t2 = tg - hfx/(rho*cp*chs2)
            end if
         end if
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_sfc_wtq")

end subroutine da_sfc_wtq


