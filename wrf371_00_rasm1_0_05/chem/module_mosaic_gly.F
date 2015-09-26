      !----------------------------------------------------------------
      ! SOA formation from glyoxal with complex formulation including
      ! * reversible formation (Kampf et al., ES&T, submitted)
      ! * dark/ammonium-catalyzed formation (Noziere, J. Phys. Chem., 2009)
      ! * OH chemistry (Ervens and Volkamer, ACP, 2010)
      ! * surface uptake (Ervens and Volkamer, ACP, 2010)
      ! Christoph Knote, ACD, NCAR, 20130326
      !----------------------------------------------------------------

      MODULE module_mosaic_gly

      IMPLICIT NONE

      INTEGER, PARAMETER :: nspecs   = 13,                             &
                            igly_g   =  1,                             &
                            igly_r1  =  2,                             &
                            igly_r2  =  3,                             &
                            igly_nh4 =  4,                             &
                            igly_sfc =  5,                             &
                            igly_oh  =  6,                             &
                            ic_as    =  7,                             &
                            ic_an    =  8,                             &
                            ia_nh4   =  9,                             &
                            ioh_g    = 10,                             &
                            iph      = 11,                             &
                            iwater   = 12,                             &
                            iarea    = 13

      ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      LOGICAL, PARAMETER :: lfast_tau1 = .FALSE.
      ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      CONTAINS

      ! adapted from Numerical Recipes, Second Edition, p. 706-707
      ! changes: added "h" (timestep) argument to DERIVS calls
      SUBROUTINE rk4(y, dydx, n, x, h, yout, derivs)

      INTEGER n
      REAL(kind=8) :: h, x, dydx(n), y(n), yout(n)
      EXTERNAL derivs
      INTEGER i
      REAL(kind=8) :: h6, hh, xh, dym(nspecs), dyt(nspecs), yt(nspecs)

      hh=h*0.5
      h6=h/6.
      xh=x+hh

      DO i=1, n
        yt(i) = y(i) + hh * dydx(i)
      ENDDO

      CALL derivs(xh, yt, h, dyt)

      DO i=1, n
        yt(i) = y(i) + hh * dyt(i)
      ENDDO

      CALL derivs(xh, yt, h, dym)

      DO i=1, n
        yt(i) = y(i) + h * dym(i)
        dym(i) = dyt(i) + dym(i)
      ENDDO

      CALL derivs(x+h, yt, h, dyt)

      DO i=1, n
        yout(i) = y(i) + h6 * ( dydx(i) + dyt(i) + 2. * dym(i))
      ENDDO

      RETURN

      END SUBROUTINE rk4

      ! Simple SOA formation from glyoxal as presented in
      ! Washenfelder et al, JGR, 2011
      SUBROUTINE glysoa_simple(dtchem)

      USE module_data_mosaic_therm, ONLY: t_k, area_wet_a, gas, aer,   &
                                          jtotal, igly, iglysoa_sfc_a, nbin_a

      IMPLICIT NONE

      REAL(kind=8), INTENT(IN)  :: dtchem
      REAL(kind=8)              :: omega, gamma_gly, A, delta_gly, frac_A
      INTEGER                   :: ibin

      ! mean molecular velocity of glyoxal (cm/s)
      omega = 1.455e4 * sqrt(t_k / 58.0_8)

      ! aerosol uptake coefficient for glyoxal (-)
      ! Washenfelder et al., 2011:
      ! 0 - 8   x 10^-4
      !     2   x 10^-4 (+/- 1 x 10^-4)
      ! Volkamer et al., 2007
      !     3.7 x 10^-3
      ! B. Ervens, pers. comm., 2010
      gamma_gly = 3.3E-3

      ! get total aerosol surface area (cm^2 / cm^3)
      A = 0.0
      DO ibin = 1, nbin_a
        A = A + area_wet_a(ibin)
      ENDDO

      ! no aerosol surface area - no uptake
      IF (A > 0.0) THEN
        ! first order uptake, Fuchs and Sutugin, 1971
        ! dCg = 1/4 * gamma * A * |v_mol| * Cg * dt
        delta_gly = 0.25 * gamma_gly * A * omega * gas(igly) * dtchem

        ! avoid negative concentrations
        delta_gly = MIN(gas(igly), delta_gly)

        ! update partitioning
        gas(igly) = gas(igly) - delta_gly

        ! distribute onto bins according to fraction of surface area
        DO ibin = 1, nbin_a
          frac_A = area_wet_a(ibin) / A
          ! we take the "photochemical" glysoa aerosol as surrogate
          aer(iglysoa_sfc_a, jtotal, ibin) = aer(iglysoa_sfc_a, jtotal, ibin) &
                                       + frac_A * delta_gly
        ENDDO
      ENDIF

      END SUBROUTINE glysoa_simple

      SUBROUTINE glysoa_complex_derivs(x, y, dt, dydx)

      USE module_data_mosaic_therm, ONLY: conv1a,  & ! converts q/mol(air) to nq/m^3 (q = mol or g)
                                          p_atm,   & ! pressure (atm)
                                          t_k        ! temperature (K)

      REAL(kind=8), INTENT(IN)  :: x, y(nspecs), dt
      REAL(kind=8), INTENT(OUT) :: dydx(nspecs)

      REAL(kind=8), PARAMETER   :: eps      = 1.e-16 , & ! minimum allowed concentration in reservoirs
                                   Kh_water = 4.19e5 , & ! effective Henry's law constant of glyoxal in pure water (M atm-1) (Ip et al., GRL, 2011)
                                   Kh_oh    = 25.0,    & ! Henry's law constant of OH in pure water (M atm-1) (Klaening et al., 1985)
                                   k_oh     = 1.1e9      ! OH reaction rate (mol L-1 s-1) (Ervens and Volkamer, ACP, 2010)

      REAL(kind=8)              :: gly_g_atm,  & ! gas-phase concentration in atm
                                   f_A1,       & ! fraction of glyoxal in reservoir 1
                                   tau1,       & ! characteristical timescale reservoir 1
                                   tau2,       & ! characteristical timescale reservoir 2
                                   oh_g_atm,   & ! gas-phase OH concentration in atm
                                   oh_a,       & ! liquid-phase OH concentration
                                   c_tot,      & ! total concentration of dissolved salts (M)
                                   Kh_eq,      & ! Henry's law constant at equilibrium (M atm-1)
                                   gly_ptot_eq,& ! total glyoxal concentration (reservoirs 1 and 2) at equilibrium
                                   gly_r1_eq,  & ! glyoxal concentration (reservoir 1) at equilibrium
                                   gly_r2_eq,  & ! glyoxal concentration (reservoir 2) at equilibrium
                                   anh4,       & ! ammonium-ion activity (constrained)
                                   kII, kI,    & ! second and first order dark rate constants
                                   omega         ! mean molecular velocity of glyoxal (cm/s)

      ! tendencies
      REAL(kind=8)              :: dg_r1,      & ! from gas-phase to reservoir 1
                                   dr1_r2,     & ! reservoir 1 to reservoir 2 (or vice versa)
                                   dr1_nh4,    & ! reservoir 1 to nh4
                                   dr1_oh,     & ! reservoir 1 to oh
                                   dg_sfc        ! gas-phase to surface uptake

      REAL(kind=8)              :: accloss,    & ! acc. loss
                                   scaling       ! tendency scaling

      dg_r1   = 0.0
      dr1_r2  = 0.0
      dr1_nh4 = 0.0
      dr1_oh  = 0.0
      dg_sfc  = 0.0

      ! convert gas-phase glyoxal concentration
      ! -------------------------------------------------------------

      gly_g_atm = y(igly_g) / conv1a ! mole / mole
      gly_g_atm = gly_g_atm * p_atm  ! atm

      ! with A2/A1 = Kolig, and Kolig is 1
      f_A1 = 0.5
      tau1 = 2.5e2 ! s
      tau2 = 5.5e3 ! s
      IF ( y(ic_as) + y(ic_an) .GT. 12.0 ) THEN
        ! with A2/A1 = Kolig, and Kolig is 0.5
        f_A1 = 0.6667
        tau1 = 4.4e4 ! s
        tau2 = 4.7e4 ! s
      ENDIF

      ! Kampff et al., ES&T, 2013, submitted: kinetic limitation of
      ! salting-in for SO4 concentrations > 12 M.
      c_tot = MIN( 12.0, y(ic_as) + y(ic_an) )

      ! effective Henry's law constant including salting-in effect (Kampff et al.)
      ! at equilibrium
      ! derived from eqn. 3 in Kampff et al., -0.24 is "salting-in" constant
      Kh_eq = Kh_water / 10**(-0.24D0 * c_tot) ! mol L-1 atm-1 == mol kg-1 atm-1

      ! gly_g_atm in atm, Kh_eq in mol kg-1 atm-1, water in kg m-3 air
      ! total glyoxal concentration (reservoirs 1 and 2) at equilibrium
      gly_ptot_eq = gly_g_atm * Kh_eq * y(iwater) * 1e9 ! nmol / m3 air

      gly_r1_eq   = gly_ptot_eq *        f_A1  ! in reservoir 1 at equilibrium
      gly_r2_eq   = gly_ptot_eq * (1.0 - f_A1) ! in reservoir 2 at equilibrium

      ! process tendencies in nmol m-3 s-1
      ! from gas-phase to reservoir 1
      ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      IF (.NOT. lfast_tau1) THEN
      ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        dg_r1       = (1.0/tau1) * (gly_r1_eq - y(igly_r1))
      ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ENDIF
      ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ! reservoir 1 to reservoir 2
      dr1_r2      = (1.0/tau2) * (gly_r2_eq - y(igly_r2))

      ! OH reaction (Ervens and Volkamer, Atm. Chem. Phys., 2010)
      ! -------------------------------------------------------------

      oh_g_atm = y(ioh_g) / conv1a                  ! now its in mole / mole
      oh_g_atm = oh_g_atm * p_atm                   ! partial pressure (atm)
      oh_a     = oh_g_atm * Kh_oh * y(iwater) * 1e9 ! nmol / m3 air

      dr1_oh   = k_oh * y(igly_r1) * oh_a           ! nmole m-3 s-1

      ! dark pathway (Noziere et al., J. Phys. Chem., 2009)
      ! -------------------------------------------------------------

      ! second-order rate constant (mol-1 kg s-1):
      anh4 = MAX( 0.0, MIN( 4.0, y(ia_nh4)) ) ! restrict to measured range
      kII   = 2.e-10 * exp(1.5 * anh4) * exp(2.5  * y(iph))

      ! dark process uses reservoir 1
      kI = kII * y(igly_r1) / y(iwater) * 1e-9 ! s-1

      dr1_nh4 = kI * y(igly_r1) ! nmol m-3 s-1

      ! surface uptake (Ervens and Volkamer, ACP, 2010)
      ! -------------------------------------------------------------

      ! mean molecular velocity of glyoxal (cm/s)
      omega = 1.455e4 * sqrt(t_k / 58.0D0)

      ! first order uptake, Fuchs and Sutugin, 1971
      ! dCg = 1/4 * gamma * A * |v_mol| * Cg * dt,
      ! gamma downscaled to 1.0e-3 according to Waxman et al., 2013
      dg_sfc = 0.25D0 * 1.e-3 * y(iarea) * omega * y(igly_g)

      ! Numerical integration
      ! -------------------------------------------------------------

      ! check for undershoots, avoid negative concentrations
      ! while ensuring we don't loose mass
      IF ( y(igly_g) < eps ) THEN
        dg_r1  = 0.0
        dg_sfc = 0.0
      ELSE
        accloss = (dg_r1 + dg_sfc) * dt
        IF ( ( y(igly_g) - accloss ) < eps ) THEN
          scaling = y(igly_g) / (accloss + eps)
          dg_r1  = dg_r1  * scaling
          dg_sfc = dg_sfc  * scaling
        ENDIF
      ENDIF

      IF ( y(igly_r1) < eps ) THEN
        dr1_r2  = 0.0
        dr1_nh4 = 0.0
        dr1_oh  = 0.0
      ELSE
        accloss = (-dg_r1 + dr1_r2 + dr1_nh4 + dr1_oh) * dt
        IF ( ( y(igly_r1) - accloss ) < eps) THEN
          scaling = y(igly_r1) / (accloss + eps)
          dr1_r2  = dr1_r2  * scaling
          dr1_nh4 = dr1_nh4 * scaling
          dr1_oh  = dr1_oh  * scaling
        ENDIF
      ENDIF

      IF ( y(igly_r2) < eps ) THEN
        dr1_r2  = MAX( 0.0, dr1_r2 )
      ELSE
        accloss = -dr1_r2 * dt
        IF ( ( y(igly_r2) - accloss ) < eps ) THEN
          scaling = y(igly_r2) / (accloss + eps)
          dr1_r2  = dr1_r2  * scaling
        ENDIF
      ENDIF

      ! sum tendencies
      dydx(igly_g)   = -dg_r1                          -dg_sfc
      dydx(igly_r1)  =  dg_r1 -dr1_r2 -dr1_nh4 -dr1_oh
      dydx(igly_r2)  =         dr1_r2
      dydx(igly_nh4) =                +dr1_nh4
      dydx(igly_oh)  =                          dr1_oh
      dydx(igly_sfc) =                                  dg_sfc

      END SUBROUTINE glysoa_complex_derivs

      SUBROUTINE glysoa_complex(dtchem)

      USE module_data_mosaic_therm, ONLY : jaerosolstate, all_liquid, mixed, &
                                           jtotal, jliquid, nbin_a, &
                                           area_wet_a, gas, water_a, aer, mc, &
                                           ph, a_nh4, c_as, c_an, &
                                           igly, iho, &
                                           iglysoa_r1_a, iglysoa_r2_a, &
                                           iglysoa_oh_a, &
                                           iglysoa_nh4_a, iglysoa_sfc_a, &
                                           iso4_a, ino3_a, inh4_a, jc_h

      ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      USE module_data_mosaic_therm, ONLY: conv1a,  & ! converts q/mol(air) to nq/m^3 (q = mol or g)
                                          p_atm      ! pressure (atm)
      ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      REAL(kind=8), INTENT(IN)  :: dtchem

      REAL(kind=8)              :: A, conv, y(nspecs), yout(nspecs), &
                                   dydx(nspecs), gly_g

      INTEGER                   :: i, ii, nbin_proc, bin_proc(nbin_a)

      ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      REAL(kind=8), PARAMETER   :: Kh_water = 4.19e5     ! effective Henry's law constant of glyoxal in pure water (M atm-1) (Ip et al., GRL, 2011)

      REAL(kind=8)              :: gly_g_atm,  & ! gas-phase concentration in atm
                                   f_A1,       & ! fraction of glyoxal in reservoir 1
                                   c_tot,      & ! total concentration of dissolved salts (M)
                                   Kh_eq,      & ! Henry's law constant at equilibrium (M atm-1)
                                   gly_ptot_eq,& ! total glyoxal concentration (reservoirs 1 and 2) at equilibrium
                                   gly_r1_eq,  & ! glyoxal concentration (reservoir 1) at equilibrium
                                   deltagly      ! delta to bring r1 in equilibrium
      ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


      ! liquid / mixed phase bins available?
      ! ---------------------------------------------------------------
      nbin_proc   = 0
      bin_proc(:) = -1 ! see which bins are either mixed,
                       ! or all_liquid (only these are to be processed)
      DO i = 1, nbin_a
        IF (jaerosolstate(i) == all_liquid .OR. &
            jaerosolstate(i) == mixed) THEN
          nbin_proc = nbin_proc + 1
          bin_proc(nbin_proc) = i
        ENDIF
      ENDDO
      IF (nbin_proc == 0) RETURN

      ! aerosol surface area available?
      ! ---------------------------------------------------------------

      A     =     0.0 ! total aerosol surface area (cm^2 / cm^3)
      DO i = 1, nbin_proc
        ii = bin_proc(i)
        A = A + area_wet_a(ii)
      ENDDO
      IF (A <= 0) RETURN

      ! clean diagnostic arrays
      ! ---------------------------------------------------------------

      ph(:) = -9999.0 ! aerosol pH
      a_nh4(:) = 0.0  ! ammonium ion activity (M, mol/m^3)
      c_as(:) = 0.0   ! ammonium sulfate concentration (M, mol/kg)
      c_an(:) = 0.0   ! ammonium nitrate concentration (M, mol/kg)

      ! get gas-phase
      ! ---------------------------------------------------------------

      ! gly_g will be re-used for all bins
      gly_g     = gas(igly)         ! nmol / m3

      DO i = 1, nbin_proc

        ii = bin_proc(i)

        ! load concentrations array
        ! -------------------------------------------------------------

        conv        = 1.e-9 / water_a(ii) ! nmol/m^3 (air) -> mol/kg (water)

        y(:)        = 0.0

        ! nmol/m^3
        y(igly_g)   = gly_g
        y(igly_r1)  = aer(iglysoa_r1_a,jtotal,ii)
        y(igly_r2)  = aer(iglysoa_r2_a,jtotal,ii)
        y(igly_nh4) = aer(iglysoa_nh4_a,jtotal,ii)
        y(igly_sfc) = aer(iglysoa_sfc_a,jtotal,ii)
        y(igly_oh)  = aer(iglysoa_oh_a,jtotal,ii)

        y(ic_as)    = aer(iso4_a,jliquid,ii) * conv ! assume we can only form (NH4)2SO4
        y(ic_an)    = aer(ino3_a,jliquid,ii) * conv ! assume we can only form NH4NO3
        y(ia_nh4)   = aer(inh4_a,jliquid,ii) * conv ! set activity == concentration

        y(iph)      = MIN(14.0D0, MAX(0.0D0, -log10(mc(jc_h,ii)) ))

        y(iwater)   = water_a(ii) ! kg m-3

        y(ioh_g)    = gas(iho) ! nmol m-3

        y(iarea)    = area_wet_a(ii) ! cm^2 / cm^3

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        IF (lfast_tau1) THEN
          ! instantaneous Henry's law equilibrium
          ! for fast partitioning (in case characteristic timescale
          ! of Kampf et al. is only artefact...)
          gly_g_atm = y(igly_g) / conv1a ! mole / mole
          gly_g_atm = gly_g_atm * p_atm  ! atm

          ! with A2/A1 = Kolig, and Kolig is 1
          f_A1 = 0.5
          IF ( y(ic_as) + y(ic_an) .GT. 12.0 ) THEN
            f_A1 = 0.6667
          ENDIF

          ! Kampff et al., ES&T, 2013, submitted: kinetic limitation of
          ! salting-in for SO4 concentrations > 12 M.
          c_tot = MIN( 12.0, y(ic_as) + y(ic_an) )

          ! effective Henry's law constant including salting-in effect (Kampff et al.)
          ! at equilibrium
          ! derived from eqn. 3 in Kampff et al., -0.24 is "salting-in" constant
          Kh_eq = Kh_water / 10**(-0.24D0 * c_tot) ! mol L-1 atm-1 == mol kg-1 atm-1

          ! gly_g_atm in atm, Kh_eq in mol kg-1 atm-1, water in kg m-3 air
          ! total glyoxal concentration (reservoirs 1 and 2) at equilibrium
          gly_ptot_eq = gly_g_atm * Kh_eq * y(iwater) * 1e9 ! nmol / m3 air

          gly_r1_eq   = gly_ptot_eq *        f_A1  ! in reservoir 1 at equilibrium

          deltagly    = gly_r1_eq - y(igly_r1)

          y(igly_g)   = y(igly_g) - deltagly
          y(igly_r1)  = y(igly_r1) + deltagly

          IF (y(igly_g) < 0.0 .OR. y(igly_r1) < 0.0) THEN
            WRITE(*,*) "THIS IS NOT RIGHT: ",y(igly_g), y(igly_r1),deltagly
          ENDIF
        ENDIF
        ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

        ! integrate system
        ! -------------------------------------------------------------
        CALL glysoa_complex_derivs(1._8, y, dtchem, dydx)
        CALL rk4(y, dydx, nspecs, 1._8, dtchem, yout, glysoa_complex_derivs)

        ! update transported fields
        aer(iglysoa_r1_a,jtotal,ii)  = yout(igly_r1)
        aer(iglysoa_r2_a,jtotal,ii)  = yout(igly_r2)
        aer(iglysoa_nh4_a,jtotal,ii) = yout(igly_nh4)
        aer(iglysoa_sfc_a,jtotal,ii) = yout(igly_sfc)
        aer(iglysoa_oh_a,jtotal,ii)  = yout(igly_oh)

        ! do not put gas-phase glyoxal back yet, as we will use it
        ! for the next bin as well...
        gly_g                        = yout(igly_g)

        ! save diagnostics
        c_as(ii)  = yout(ic_as)
        c_an(ii)  = yout(ic_an)
        ph(ii)    = yout(iph)
        a_nh4(ii) = yout(ia_nh4)

      ENDDO

      ! update gas-phase reservoir, after all bins have been treated.
      gas(igly) = gly_g

      END SUBROUTINE glysoa_complex


      END MODULE module_mosaic_gly

