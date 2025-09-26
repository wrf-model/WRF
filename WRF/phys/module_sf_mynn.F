!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_sf_mynn

!-------------------------------------------------------------------
!Modifications implemented by Joseph Olson NOAA/GSD/AMB - CU/CIRES
!The following overviews the current state of this scheme::
!
!   BOTH LAND AND WATER:
!1) Calculation of stability parameter (z/L) taken from Li et al. (2010 BLM)
!   for first iteration of first time step; afterwards, exact calculation
!   using basically the same iterative technique in the module_sf_sfclayrev.F,
!   which leverages Pedro Jimenez's code, and is adapted for MYNN.
!2) Fixed isflux=0 option to turn off scalar fluxes, but keep momentum
!   fluxes for idealized studies (credit: Anna Fitch).
!3) Kinematic viscosity varies with temperature according to Andreas (1989).
!4) Uses the blended Monin-Obukhov flux-profile relationships COARE (Fairall 
!   et al 2003) for the unstable regime (a blended mix of Dyer-Hicks 1974 and
!   Grachev et al (2000). Uses Cheng and Brutsaert (2005) for stable conditions.
!5) The following overviews the namelist variables that control the 
!   aerodynamic roughness lengths (over water) and the thermal and moisture
!   roughness lengths (defaults are recommended):
!
!   LAND only:
!   "iz0tlnd" namelist option is used to select the following options:
!   (default) =0: Zilitinkevich (1995); Czil now set to 0.085
!             =1: Czil_new (modified according to Chen & Zhang 2008)
!             =2: Modified Yang et al (2002, 2008) - generalized for all landuse
!             =3: constant zt = z0/7.4 (original form; Garratt 1992)
!
!   WATER only:
!   "isftcflx" namelist option is used to select the following options:
!   (default) =0: z0, zt, and zq from the COARE algorithm. Set COARE_OPT (below) to
!                 3.0 (Fairall et al. 2003, default)
!                 3.5 (Edson et al 2013) 
!             =1: z0 from Davis et al (2008), zt & zq from COARE 3.0/3.5
!             =2: z0 from Davis et al (2008), zt & zq from Garratt (1992)
!             =3: z0 from Taylor and Yelland (2004), zt and zq from COARE 3.0/3.5
!
!   SNOW/ICE only:
!   Andreas (2002) snow/ice parameterization for thermal and
!   moisture roughness is used over all gridpoints with snow deeper than
!   0.1 m. This algorithm calculates a z0 for snow (Andreas et al. 2005, BLM), 
!   which is only used as part of the thermal and moisture roughness
!   length calculation, not to directly impact the surface winds.
!
! Misc:
!1) Added a more elaborate diagnostic for u10 & V10 for high vertical resolution
!   model configurations but for most model configurations with depth of
!   the lowest half-model level near 10 m, a neutral-log diagnostic is used.
!
!2) Option to activate stochastic parameter perturbations (SPP), which
!   perturb z0, zt, and zq, along with many other parameters in the MYNN-
!   EDMF scheme. 
!
!NOTE: This code was primarily tested in combination with the RUC LSM.
!      Performance with the Noah (or other) LSM is relatively unknown.
!-------------------------------------------------------------------
!For WRF
  USE module_model_constants, only: &
       &p1000mb, ep_2

!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!For non-WRF
!   REAL    , PARAMETER :: g            = 9.81
!   REAL    , PARAMETER :: r_d          = 287.
!   REAL    , PARAMETER :: cp           = 7.*r_d/2.
!   REAL    , PARAMETER :: r_v          = 461.6
!   REAL    , PARAMETER :: cpv          = 4.*r_v
!   REAL    , PARAMETER :: rcp          = r_d/cp
!   REAL    , PARAMETER :: XLV          = 2.5E6
!   REAL    , PARAMETER :: XLF          = 3.50E5
!   REAL    , PARAMETER :: p1000mb      = 100000.
!   REAL    , PARAMETER :: EP_2         = r_d/r_v

  REAL, PARAMETER :: ep_3=1.-ep_2 
  REAL, PARAMETER :: wmin=0.1    ! Minimum wind speed
  REAL, PARAMETER :: VCONVC=1.25
  REAL, PARAMETER :: SNOWZ0=0.011
  REAL, PARAMETER :: COARE_OPT=3.0  ! 3.0 or 3.5
  !For debugging purposes:
  LOGICAL, PARAMETER :: debug_code = .false.

  REAL,   DIMENSION(0:1000 ),SAVE :: psim_stab,psim_unstab, &
                                     psih_stab,psih_unstab

CONTAINS

!-------------------------------------------------------------------
  SUBROUTINE mynn_sf_init_driver(allowed_to_read)

    LOGICAL, INTENT(in) :: allowed_to_read

    !Fill the PSIM and PSIH tables. This code was leveraged from
    !module_sf_sfclayrev.F, leveraging the work from Pedro Jimenez.
    !This subroutine returns a blended form from Dyer and Hicks (1974)
    !and Grachev et al (2000) for unstable conditions and the form 
    !from Cheng and Brutsaert (2005) for stable conditions.

     CALL psi_init

  END SUBROUTINE mynn_sf_init_driver

!-------------------------------------------------------------------
   SUBROUTINE SFCLAY_mynn(                                  &
              U3D,V3D,T3D,QV3D,P3D,dz8w,                    &
              CP,G,ROVCP,R,XLV,PSFCPA,CHS,CHS2,CQS2,CPM,    &
              ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
              XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
              U10,V10,TH2,T2,Q2,SNOWH,                      &
              GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
              SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
              KARMAN,itimestep,ch,th3d,pi3d,qc3d,rho3d,qcg, &
              spp_pbl,pattern_spp_pbl,                      &
              ids,ide, jds,jde, kds,kde,                    &
              ims,ime, jms,jme, kms,kme,                    &
              its,ite, jts,jte, kts,kte,                    &
              ustm,ck,cka,cd,cda,isftcflx,iz0tlnd           )
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
!-- U3D         3D u-velocity interpolated to theta points (m/s)
!-- V3D         3D v-velocity interpolated to theta points (m/s)
!-- T3D         3D temperature (K)
!-- QV3D        3D water vapor mixing ratio (Kg/Kg)
!-- P3D         3D pressure (Pa)
!-- RHO3D       3D density (kg/m3) 
!-- dz8w        3D dz between full levels (m)
!-- CP          heat capacity at constant pressure for dry air (J/kg/K)
!-- G           acceleration due to gravity (m/s^2)
!-- ROVCP       R/CP
!-- R           gas constant for dry air (J/kg/K)
!-- XLV         latent heat of vaporization for water (J/kg)
!-- PSFCPA      surface pressure (Pa)
!-- ZNT         roughness length (m)
!-- UST         u* in similarity theory (m/s)
!-- USTM        u* in similarity theory (m/s) w* added to WSPD. This is
!               used to couple with TKE scheme but not in MYNN.
!               (as of now, USTM = UST in this version)
!-- PBLH        PBL height from previous time (m)
!-- MAVAIL      surface moisture availability (between 0 and 1)
!-- ZOL         z/L height over Monin-Obukhov length
!-- MOL         T* (similarity theory) (K)
!-- RMOL        Reciprocal of M-O length (/m)
!-- REGIME      flag indicating PBL regime (stable, unstable, etc.)
!-- PSIM        similarity stability function for momentum
!-- PSIH        similarity stability function for heat
!-- XLAND       land mask (1 for land, 2 for water)
!-- HFX         upward heat flux at the surface (W/m^2)
!-- QFX         upward moisture flux at the surface (kg/m^2/s)
!-- LH          net upward latent heat flux at surface (W/m^2)
!-- TSK         surface temperature (K)
!-- FLHC        exchange coefficient for heat (W/m^2/K)
!-- FLQC        exchange coefficient for moisture (kg/m^2/s)
!-- CHS         heat/moisture exchange coefficient for LSM (m/s)
!-- QGH         lowest-level saturated mixing ratio
!-- QSFC        qv (specific humidity) at the surface
!-- QSFCMR      qv (mixing ratio) at the surface
!-- U10         diagnostic 10m u wind
!-- V10         diagnostic 10m v wind
!-- TH2         diagnostic 2m theta (K)
!-- T2          diagnostic 2m temperature (K)
!-- Q2          diagnostic 2m mixing ratio (kg/kg)
!-- SNOWH       Snow height (m)
!-- GZ1OZ0      log((z1+ZNT)/ZNT) where ZNT is roughness length 
!-- WSPD        wind speed at lowest model level (m/s)
!-- BR          bulk Richardson number in surface layer
!-- ISFFLX      isfflx=1 for surface heat and moisture fluxes
!-- DX          horizontal grid size (m)
!-- SVP1        constant for saturation vapor pressure (=0.6112 kPa)
!-- SVP2        constant for saturation vapor pressure (=17.67 dimensionless)
!-- SVP3        constant for saturation vapor pressure (=29.65 K)
!-- SVPT0       constant for saturation vapor pressure (=273.15 K)
!-- EP1         constant for virtual temperature (Rv/Rd - 1) (dimensionless)
!-- EP2         constant for spec. hum. calc (Rd/Rv = 0.622) (dimensionless)
!-- EP3         constant for spec. hum. calc (1 - Rd/Rv = 0.378 ) (dimensionless)
!-- KARMAN      Von Karman constant
!-- ck          enthalpy exchange coeff at 10 meters
!-- cd          momentum exchange coeff at 10 meters
!-- cka         enthalpy exchange coeff at the lowest model level
!-- cda         momentum exchange coeff at the lowest model level
!-- isftcflx    =0: z0, zt, and zq from COARE3.0/3.5 (Fairall et al 2003/Edson et al 2013)
!   (water      =1: z0 from Davis et al (2008), zt & zq from COARE3.0/3.5
!    only)      =2: z0 from Davis et al (2008), zt & zq from Garratt (1992)
!               =3: z0 from Taylor and Yelland (2004), zt and zq from COARE 3.0/3.5
!-- iz0tlnd     =0: Zilitinkevich (1995) with Czil=0.085, 
!   (land       =1: Czil_new (modified according to Chen & Zhang 2008)
!    only)      =2: Modified Yang et al (2002, 2008) - generalized for all landuse
!               =3: constant zt = z0/7.4 (Garratt 1992)
!
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!=================================================================
! SCALARS
!===================================
      INTEGER,  INTENT(IN)   ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte
      INTEGER,  INTENT(IN)   ::        itimestep
      REAL,     INTENT(IN)   ::        SVP1,SVP2,SVP3,SVPT0
      REAL,     INTENT(IN)   ::        EP1,EP2,KARMAN
      REAL,     INTENT(IN)   ::        CP,G,ROVCP,R,XLV,DX
!NAMELIST OPTIONS:
      INTEGER,  INTENT(IN)   ::        ISFFLX
      INTEGER,  OPTIONAL,  INTENT(IN)   ::     ISFTCFLX, IZ0TLND
      INTEGER,  OPTIONAL,  INTENT(IN)   ::     spp_pbl

!===================================
! 3D VARIABLES
!===================================
      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                           dz8w, &
                                                             QV3D, &
                                                              P3D, &
                                                              T3D, &
                                                             QC3D, &
                                                          U3D,V3D, &
                                                  RHO3D,th3d,pi3d

      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL,      &
                INTENT(IN) ::                      pattern_spp_pbl
!===================================
! 2D VARIABLES
!===================================
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )               ::             MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK, &
                                                              QCG, &
                                                           PSFCPA ,&
                                                            SNOWH


      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(OUT  )               ::            U10,V10, &
                                                        TH2,T2,Q2

      REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme )              , &
                INTENT(OUT)     ::              ck,cka,cd,cda,ustm
!
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)               ::             REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                               LH, &
                                                         MOL,RMOL, &
                                                        QSFC, QGH, &
                                                              ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                             CHS2, &
                                                             CQS2, &
                                                              CHS, &
                                                               CH, &
                                                        FLHC,FLQC, &
                                                   GZ1OZ0,WSPD,BR, &
                                                        PSIM,PSIH

!ADDITIONAL OUTPUT
      REAL,     DIMENSION( ims:ime, jms:jme )    ::   wstar,qstar
!===================================
! 1D LOCAL ARRAYS
!===================================
      REAL,     DIMENSION( its:ite ) ::                       U1D, &
                                                              V1D, &
                                                        U1D2,V1D2, & !level2 winds
                                                             QV1D, &
                                                              P1D, &
                                                         T1D,QC1D, &
                                                            RHO1D, &
                                                           dz8w1d, & !level 1 height
                                                           dz2w1d    !level 2 height


      REAL,     DIMENSION( its:ite ) ::                  rstoch1D

      INTEGER ::  I,J,K,itf,jtf,ktf
!-----------------------------------------------------------

      itf=MIN0(ite,ide-1)
      jtf=MIN0(jte,jde-1)
      ktf=MIN0(kte,kde-1)

      DO J=jts,jte
        DO i=its,ite
           dz8w1d(I) = dz8w(i,kts,j)
           dz2w1d(I) = dz8w(i,kts+1,j)
           U1D(i) =U3D(i,kts,j)
           V1D(i) =V3D(i,kts,j)
           !2nd model level winds - for diags with high-res grids
           U1D2(i) =U3D(i,kts+1,j)
           V1D2(i) =V3D(i,kts+1,j)
           QV1D(i)=QV3D(i,kts,j)
           QC1D(i)=QC3D(i,kts,j)
           P1D(i) =P3D(i,kts,j)
           T1D(i) =T3D(i,kts,j)
           RHO1D(i)=RHO3D(i,kts,j)
           if (spp_pbl==1) then
               rstoch1D(i)=pattern_spp_pbl(i,kts,j)
           else
               rstoch1D(i)=0.0
           endif
        ENDDO

        IF (itimestep==1) THEN
           DO i=its,ite
              UST(i,j)=MAX(0.04*SQRT(U1D(i)*U1D(i) + V1D(i)*V1D(i)),0.001)
              MOL(i,j)=0.     ! Tstar
              QSFC(i,j)=QV3D(i,kts,j)/(1.+QV3D(i,kts,j))
              qstar(i,j)=0.0
           ENDDO
        ENDIF

        CALL SFCLAY1D_mynn(                                        &
                J,U1D,V1D,T1D,QV1D,P1D,dz8w1d,rho1d,               &
                U1D2,V1D2,dz2w1d,                                  &
                CP,G,ROVCP,R,XLV,PSFCPA(ims,j),CHS(ims,j),CHS2(ims,j),&
                CQS2(ims,j),CPM(ims,j),PBLH(ims,j), RMOL(ims,j),   &
                ZNT(ims,j),UST(ims,j),MAVAIL(ims,j),ZOL(ims,j),    &
                MOL(ims,j),REGIME(ims,j),PSIM(ims,j),PSIH(ims,j),  &
                XLAND(ims,j),HFX(ims,j),QFX(ims,j),TSK(ims,j),     &
                U10(ims,j),V10(ims,j),TH2(ims,j),T2(ims,j),        &
                Q2(ims,j),FLHC(ims,j),FLQC(ims,j),SNOWH(ims,j),    &
                QGH(ims,j),QSFC(ims,j),LH(ims,j),                  &
                GZ1OZ0(ims,j),WSPD(ims,j),BR(ims,j),ISFFLX,DX,     &
                SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,               &
                ch(ims,j),qc1d,qcg(ims,j),                         &
                itimestep,                                         &
!JOE-begin additional output
                wstar(ims,j),qstar(ims,j),                         &
!JOE-end
                spp_pbl,rstoch1D,                                  &
                ids,ide, jds,jde, kds,kde,                         &
                ims,ime, jms,jme, kms,kme,                         &
                its,ite, jts,jte, kts,kte                          &
                ,isftcflx,iz0tlnd,                                 &
                USTM(ims,j),CK(ims,j),CKA(ims,j),                  &
                CD(ims,j),CDA(ims,j)                               &
                                                                   )

      ENDDO

    END SUBROUTINE SFCLAY_MYNN

!-------------------------------------------------------------------
   SUBROUTINE SFCLAY1D_mynn(                                       &
                     J,U1D,V1D,T1D,QV1D,P1D,dz8w1d,rho1d,          &
                     U1D2,V1D2,dz2w1d,                             &
                     CP,G,ROVCP,R,XLV,PSFCPA,CHS,CHS2,CQS2,CPM,    &
                     PBLH,RMOL,ZNT,UST,MAVAIL,ZOL,MOL,REGIME,      &
                     PSIM,PSIH,XLAND,HFX,QFX,TSK,                  &
                     U10,V10,TH2,T2,Q2,FLHC,FLQC,SNOWH,QGH,        &
                     QSFC,LH,GZ1OZ0,WSPD,BR,ISFFLX,DX,             &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                     KARMAN,ch,qc1d,qcg,                           &
                     itimestep,                                    &
!JOE-additional output
                     wstar,qstar,                                  &
!JOE-end
                     spp_pbl,rstoch1D,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     &
                     ,isftcflx, iz0tlnd,                           &
                     ustm,ck,cka,cd,cda                            &
                     )

!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
! SCALARS
!-----------------------------
      INTEGER,  INTENT(IN) ::        ids,ide, jds,jde, kds,kde, &
                                     ims,ime, jms,jme, kms,kme, &
                                     its,ite, jts,jte, kts,kte, &
                                     J, itimestep

      REAL,     PARAMETER  :: XKA=2.4E-5   !molecular diffusivity
      REAL,     PARAMETER  :: PRT=1.       !prandlt number
      REAL,     INTENT(IN) :: SVP1,SVP2,SVP3,SVPT0,EP1,EP2
      REAL,     INTENT(IN) :: KARMAN,CP,G,ROVCP,R,XLV,DX

!-----------------------------
! NAMELIST OPTIONS
!-----------------------------
      INTEGER,  INTENT(IN) :: ISFFLX
      INTEGER,  OPTIONAL,  INTENT(IN )   ::     ISFTCFLX, IZ0TLND
      INTEGER,    INTENT(IN)             ::     spp_pbl

!-----------------------------
! 1D ARRAYS
!-----------------------------
      REAL,     DIMENSION( ims:ime ), INTENT(IN)    ::     MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK, &
                                                           PSFCPA, &
                                                              QCG, &
                                                            SNOWH

      REAL,     DIMENSION( its:ite ), INTENT(IN)   ::     U1D,V1D, &
                                                        U1D2,V1D2, &
                                                         QV1D,P1D, &
                                                         T1D,QC1d, &
                                                    dz8w1d,dz2w1d, &
                                                            RHO1D

      REAL,     DIMENSION( ims:ime ), INTENT(INOUT) ::     REGIME, &
                                                       HFX,QFX,LH, &
                                                         MOL,RMOL, &
                                                         QGH,QSFC, &
                                                              ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                        CHS2,CQS2, &
                                                           CHS,CH, &
                                                        FLHC,FLQC, &
                                                           GZ1OZ0, &
                                                             WSPD, &
                                                               BR, &
                                                        PSIM,PSIH
      REAL,     DIMENSION( its:ite ), INTENT(IN)   ::     rstoch1D

      ! DIAGNOSTIC OUTPUT
      REAL,     DIMENSION( ims:ime ), INTENT(OUT)   ::    U10,V10, &
                                                        TH2,T2,Q2

      REAL, OPTIONAL, DIMENSION( ims:ime )                       , &
                INTENT(OUT)     ::              ck,cka,cd,cda,ustm
!--------------------------------------------
!JOE-additinal output
      REAL,     DIMENSION( ims:ime ) ::                wstar,qstar
!JOE-end
!----------------------------------------------------------------
! LOCAL VARS
!----------------------------------------------------------------
      REAL, DIMENSION(its:ite) :: &
                 ZA, &    !Height of lowest 1/2 sigma level(m)
                ZA2, &    !Height of 2nd lowest 1/2 sigma level(m)
              THV1D, &    !Theta-v at lowest 1/2 sigma (K)
               TH1D, &    !Theta at lowest 1/2 sigma (K)
               TC1D, &    !T at lowest 1/2 sigma (Celsius)
               TV1D, &    !Tv at lowest 1/2 sigma (K)
               QVSH, &    !qv at lowest 1/2 sigma (spec humidity)
              PSIH2, &    !M-O stability functions at z=2 m
             PSIM10, &    !M-O stability functions at z=10 m
             PSIH10, &    !M-O stability functions at z=10 m
              WSPDI, & 
            z_t,z_q, &    !thermal & moisture roughness lengths
           ZNTstoch, &
             GOVRTH, &    !g/theta
               THGB, &    !theta at ground
              THVGB, &    !theta-v at ground
               PSFC, &    !press at surface (Pa/1000)
             QSFCMR, &    !qv at surface (mixing ratio, kg/kg)
             GZ2OZ0, &    !LOG((2.0+ZNT(I))/ZNT(I))
            GZ10OZ0, &    !LOG((10.+ZNT(I))/ZNT(I))
             GZ2OZt, &    !LOG((2.0+z_t(i))/z_t(i))
            GZ10OZt, &    !LOG((10.+z_t(i))/z_t(i))
             GZ1OZt, &    !LOG((ZA(I)+z_t(i))/z_t(i))
             zratio       !z0/zt

      INTEGER ::  N,I,K,L,yesno

      REAL    ::  PL,THCON,TVCON,E1
      REAL    ::  DTHVDZ,DTHVM,VCONV,ZOL2,ZOL10,ZOLZA,ZOLZ0
      REAL    ::  DTG,PSIX,DTTHX,DTHDZ,PSIX10,PSIT,PSIT2, &
                  PSIQ,PSIQ2,PSIQ10
      REAL    ::  FLUXC,VSGD
      REAL    ::  restar,VISC,DQG,OLDUST,OLDTST

!-------------------------------------------------------------------

      DO I=its,ite
         ! CONVERT GROUND & LOWEST LAYER TEMPERATURE TO POTENTIAL TEMPERATURE:
         ! PSFC cmb
         PSFC(I)=PSFCPA(I)/1000.
         THGB(I)=TSK(I)*(100./PSFC(I))**ROVCP   !(K)              
         ! PL cmb
         PL=P1D(I)/1000.                                                   
         THCON=(100./PL)**ROVCP                                                 
         TH1D(I)=T1D(I)*THCON                   !(Theta, K)
         TC1D(I)=T1D(I)-273.15                  !(T, Celsius)    

         ! CONVERT TO VIRTUAL TEMPERATURE
         QVSH(I)=QV1D(I)/(1.+QV1D(I))        !CONVERT TO SPEC HUM (kg/kg)
         TVCON=(1.+EP1*QVSH(I))
         THV1D(I)=TH1D(I)*TVCON                 !(K)
         TV1D(I)=T1D(I)*TVCON   !(K)

         !RHO1D(I)=PSFCPA(I)/(R*TV1D(I)) !now using value calculated in sfc driver
         ZA(I)=0.5*dz8w1d(I)             !height of first half-sigma level 
         ZA2(I)=dz8w1d(I) + 0.5*dz2w1d(I)    !height of 2nd half-sigma level
         GOVRTH(I)=G/TH1D(I)
      ENDDO

      DO I=its,ite
         IF (TSK(I) .LT. 273.15) THEN
            !SATURATION VAPOR PRESSURE WRT ICE (SVP1=.6112; 10*mb)
            E1=SVP1*EXP(4648*(1./273.15 - 1./TSK(I)) - &
            & 11.64*LOG(273.15/TSK(I)) + 0.02265*(273.15 - TSK(I)))
         ELSE
            !SATURATION VAPOR PRESSURE WRT WATER (Bolton 1980)
            E1=SVP1*EXP(SVP2*(TSK(I)-SVPT0)/(TSK(I)-SVP3))
         ENDIF
         !FOR LAND POINTS, QSFC can come from LSM, ONLY RECOMPUTE OVER WATER
         IF (xland(i).gt.1.5 .or. QSFC(i).le.0.0) THEN   !WATER
            QSFC(I)=EP2*E1/(PSFC(I)-ep_3*E1)             !specific humidity    
            QSFCMR(I)=EP2*E1/(PSFC(I)-E1)                !mixing ratio 
         ELSE                                            !LAND 
            QSFCMR(I)=QSFC(I)/(1.-QSFC(I))
         ENDIF

         ! QGH CHANGED TO USE LOWEST-LEVEL AIR TEMP CONSISTENT WITH MYJSFC CHANGE
         ! Q2SAT = QGH IN LSM
         IF (TSK(I) .LT. 273.15) THEN
            !SATURATION VAPOR PRESSURE WRT ICE
            E1=SVP1*EXP(4648*(1./273.15 - 1./T1D(I)) - &
            &  11.64*LOG(273.15/T1D(I)) + 0.02265*(273.15 - T1D(I)))
         ELSE
            !SATURATION VAPOR PRESSURE WRT WATER (Bolton 1980)
            E1=SVP1*EXP(SVP2*(T1D(I)-SVPT0)/(T1D(I)-SVP3))
         ENDIF
         PL=P1D(I)/1000.
         !QGH(I)=EP2*E1/(PL-ep_3*E1)    !specific humidity
         QGH(I)=EP2*E1/(PL-E1)          !mixing ratio
         CPM(I)=CP*(1.+0.84*QV1D(I))
      ENDDO

      DO I=its,ite
         WSPD(I)=SQRT(U1D(I)*U1D(I)+V1D(I)*V1D(I))     

         !TGS:THVGB(I)=THGB(I)*(1.+EP1*QSFC(I)*MAVAIL(I)) 
         THVGB(I)=THGB(I)*(1.+EP1*QSFC(I))

         DTHDZ=(TH1D(I)-THGB(I))
         DTHVDZ=(THV1D(I)-THVGB(I))

         !--------------------------------------------------------
         !  Calculate the convective velocity scale (WSTAR) and 
         !  subgrid-scale velocity (VSGD) following Beljaars (1995, QJRMS) 
         !  and Mahrt and Sun (1995, MWR), respectively
         !-------------------------------------------------------
         !  Use Beljaars over land and water
         fluxc = max(hfx(i)/RHO1D(i)/cp                    &
         &    + ep1*THVGB(I)*qfx(i)/RHO1D(i),0.)
         !WSTAR(I) = vconvc*(g/TSK(i)*pblh(i)*fluxc)**.33
         IF (xland(i).gt.1.5 .or. QSFC(i).le.0.0) THEN   !WATER
            WSTAR(I) = vconvc*(g/TSK(i)*pblh(i)*fluxc)**.33
         ELSE                                            !LAND
            !increase height scale, assuming that the non-local transoport
            !from the mass-flux (plume) mixing exceedsd the PBLH.
            WSTAR(I) = vconvc*(g/TSK(i)*MIN(1.5*pblh(i),4000.)*fluxc)**.33
         ENDIF
         !--------------------------------------------------------
         ! Mahrt and Sun low-res correction
         ! (for 13 km ~ 0.37 m/s; for 3 km == 0 m/s)
         !--------------------------------------------------------
         VSGD = 0.32 * (max(dx/5000.-1.,0.))**.33
         WSPD(I)=SQRT(WSPD(I)*WSPD(I)+WSTAR(I)*WSTAR(I)+vsgd*vsgd)
         WSPD(I)=MAX(WSPD(I),wmin)

         !--------------------------------------------------------
         ! CALCULATE THE BULK RICHARDSON NUMBER OF SURFACE LAYER, 
         ! ACCORDING TO AKB(1976), EQ(12). 
         !--------------------------------------------------------
         BR(I)=GOVRTH(I)*ZA(I)*DTHVDZ/(WSPD(I)*WSPD(I))
         IF (ITIMESTEP == 1) THEN
            !SET LIMITS ACCORDING TO Li et al. (2010) Boundary-Layer Meteorol (p.158)
            BR(I)=MAX(BR(I),-2.0)
            BR(I)=MIN(BR(I),2.0)
         ELSE
           BR(I)=MAX(BR(I),-50.0)
           BR(I)=MIN(BR(I), 50.0)
         ENDIF

         ! IF PREVIOUSLY UNSTABLE, DO NOT LET INTO REGIMES 1 AND 2 (STABLE)
         !if (itimestep .GT. 1) THEN
         !    IF(MOL(I).LT.0.)BR(I)=MIN(BR(I),0.0)
         !ENDIF
     
      ENDDO

 1006   format(A,F7.3,A,f9.4,A,f9.5,A,f9.4)
 1007   format(A,F2.0,A,f6.2,A,f7.3,A,f7.2)

!--------------------------------------------------------------------      
!--------------------------------------------------------------------      
!--- BEGIN I-LOOP
!--------------------------------------------------------------------
!--------------------------------------------------------------------

 DO I=its,ite

    !COMPUTE KINEMATIC VISCOSITY (m2/s) Andreas (1989) CRREL Rep. 89-11
    !valid between -173 and 277 degrees C.
    VISC=1.326e-5*(1. + 6.542e-3*TC1D(I) + 8.301e-6*TC1D(I)*TC1D(I) &
                      - 4.84e-9*TC1D(I)*TC1D(I)*TC1D(I))

    IF ((XLAND(I)-1.5).GE.0) THEN
       !--------------------------------------
       ! WATER
       !--------------------------------------
       ! CALCULATE z0 (znt)
       !--------------------------------------
       IF ( PRESENT(ISFTCFLX) ) THEN
          IF ( ISFTCFLX .EQ. 0 ) THEN
             IF (COARE_OPT .EQ. 3.0) THEN
                !COARE 3.0 (MISLEADING SUBROUTINE NAME)
                CALL charnock_1955(ZNT(i),UST(i),WSPD(i),visc,ZA(I))
             ELSE
                !COARE 3.5
                CALL edson_etal_2013(ZNT(i),UST(i),WSPD(i),visc,ZA(I))
             ENDIF
          ELSEIF ( ISFTCFLX .EQ. 1 .OR. ISFTCFLX .EQ. 2 ) THEN
             CALL davis_etal_2008(ZNT(i),UST(i))
          ELSEIF ( ISFTCFLX .EQ. 3 ) THEN
             CALL Taylor_Yelland_2001(ZNT(i),UST(i),WSPD(i))
          ELSEIF ( ISFTCFLX .EQ. 4 ) THEN
             IF (COARE_OPT .EQ. 3.0) THEN
                !COARE 3.0 (MISLEADING SUBROUTINE NAME)
                CALL charnock_1955(ZNT(i),UST(i),WSPD(i),visc,ZA(I))
             ELSE
                !COARE 3.5
                CALL edson_etal_2013(ZNT(i),UST(i),WSPD(i),visc,ZA(I))
             ENDIF
          ENDIF
       ELSE
          !DEFAULT TO COARE 3.0/3.5
          IF (COARE_OPT .EQ. 3.0) THEN
             !COARE 3.0
             CALL charnock_1955(ZNT(i),UST(i),WSPD(i),visc,ZA(I))
          ELSE
             !COARE 3.5
             CALL edson_etal_2013(ZNT(i),UST(i),WSPD(i),visc,ZA(I))
          ENDIF
       ENDIF

       ! add stochastic perturbaction of ZNT
       if (spp_pbl==1) then
          ZNTstoch(I)  = MAX(ZNT(I) + ZNT(I)*1.0*rstoch1D(i), 1e-6)
       else
          ZNTstoch(I)  = ZNT(I)
       endif

       !COMPUTE ROUGHNESS REYNOLDS NUMBER (restar) USING NEW ZNT
       ! AHW: Garrattt formula: Calculate roughness Reynolds number
       !      Kinematic viscosity of air (linear approx to
       !      temp dependence at sea level)
       restar=MAX(ust(i)*ZNTstoch(i)/visc, 0.1)

       !--------------------------------------
       !CALCULATE z_t and z_q
       !--------------------------------------
       IF ( PRESENT(ISFTCFLX) ) THEN
          IF ( ISFTCFLX .EQ. 0 ) THEN
             IF (COARE_OPT .EQ. 3.0) THEN
                CALL fairall_etal_2003(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
             ELSE
                !presumably, this will be published soon, but hasn't yet
                CALL fairall_etal_2014(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
             ENDIF
          ELSEIF ( ISFTCFLX .EQ. 1 ) THEN
             IF (COARE_OPT .EQ. 3.0) THEN
                CALL fairall_etal_2003(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
             ELSE
                CALL fairall_etal_2014(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
             ENDIF
          ELSEIF ( ISFTCFLX .EQ. 2 ) THEN
             CALL garratt_1992(z_t(i),z_q(i),ZNTstoch(i),restar,XLAND(I))
          ELSEIF ( ISFTCFLX .EQ. 3 ) THEN
             IF (COARE_OPT .EQ. 3.0) THEN
                CALL fairall_etal_2003(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
             ELSE
                CALL fairall_etal_2014(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
             ENDIF
          ENDIF
       ELSE
          !DEFAULT TO COARE 3.0/3.5
          IF (COARE_OPT .EQ. 3.0) THEN
             CALL fairall_etal_2003(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
          ELSE
             CALL fairall_etal_2014(z_t(i),z_q(i),restar,UST(i),visc,rstoch1D(i),spp_pbl)
          ENDIF
       ENDIF
 
    ELSE

       ! add stochastic perturbaction of ZNT
       if (spp_pbl==1) then
          ZNTstoch(I)  = MAX(ZNT(I) + ZNT(I)*1.0*rstoch1D(i), 1e-6)
       else
          ZNTstoch(I)  = ZNT(I)
       endif

       !--------------------------------------
       ! LAND
       !--------------------------------------
       !COMPUTE ROUGHNESS REYNOLDS NUMBER (restar) USING DEFAULT ZNT
       restar=MAX(ust(i)*ZNTstoch(i)/visc, 0.1)

       !--------------------------------------
       !GET z_t and z_q
       !--------------------------------------
       !CHECK FOR SNOW/ICE POINTS OVER LAND
       IF ( SNOWH(i) .GE. 0.1) THEN
          CALL Andreas_2002(ZNTSTOCH(i),visc,ust(i),z_t(i),z_q(i))
       ELSE
          IF ( PRESENT(IZ0TLND) ) THEN
             IF ( IZ0TLND .LE. 1 ) THEN
                CALL zilitinkevich_1995(ZNTSTOCH(i),z_t(i),z_q(i),restar,&
                           UST(I),KARMAN,XLAND(I),IZ0TLND,spp_pbl,rstoch1D(i))
             ELSEIF ( IZ0TLND .EQ. 2 ) THEN
                CALL Yang_2008(ZNTSTOCH(i),z_t(i),z_q(i),UST(i),MOL(I),&
                               qstar(I),restar,visc,XLAND(I))
             ELSEIF ( IZ0TLND .EQ. 3 ) THEN
                !Original MYNN in WRF-ARW used this form:
                CALL garratt_1992(z_t(i),z_q(i),ZNTSTOCH(i),restar,XLAND(I))
             ENDIF
          ELSE
             !DEFAULT TO ZILITINKEVICH
             CALL zilitinkevich_1995(ZNTSTOCH(i),z_t(i),z_q(i),restar,&
                           UST(I),KARMAN,XLAND(I),0,spp_pbl,rstoch1D(i))
          ENDIF
       ENDIF

    ENDIF
    zratio(i)=ZNTstoch(I)/z_t(I)   !needed for Li et al.

    GZ1OZ0(I)= LOG((ZA(I)+ZNTstoch(I))/ZNTstoch(I))
    GZ1OZt(I)= LOG((ZA(I)+z_t(i))/z_t(i))           
    GZ2OZ0(I)= LOG((2.0+ZNTstoch(I))/ZNTstoch(I))                                        
    GZ2OZt(I)= LOG((2.0+z_t(i))/z_t(i))                                        
    GZ10OZ0(I)=LOG((10.+ZNTstoch(I))/ZNTstoch(I)) 
    GZ10OZt(I)=LOG((10.+z_t(i))/z_t(i)) 

    !--------------------------------------------------------------------      
    !--- DIAGNOSE BASIC PARAMETERS FOR THE APPROPRIATE STABILITY CLASS:
    !                                                                                
    !    THE STABILITY CLASSES ARE DETERMINED BY BR (BULK RICHARDSON NO.).
    !                                                                                
    !    CRITERIA FOR THE CLASSES ARE AS FOLLOWS:                                   
    !                                                                                
    !        1. BR .GE. 0.2;                                                         
    !               REPRESENTS NIGHTTIME STABLE CONDITIONS (REGIME=1),               
    !                                                                                
    !        2. BR .LT. 0.2 .AND. BR .GT. 0.0;                                       
    !               REPRESENTS DAMPED MECHANICAL TURBULENT CONDITIONS                
    !               (REGIME=2),                                                      
    !                                                                                
    !        3. BR .EQ. 0.0                                                          
    !               REPRESENTS FORCED CONVECTION CONDITIONS (REGIME=3),              
    !                                                                                
    !        4. BR .LT. 0.0                                                          
    !               REPRESENTS FREE CONVECTION CONDITIONS (REGIME=4).                
    !                                                                                
    !--------------------------------------------------------------------
    IF (BR(I) .GT. 0.0) THEN
       IF (BR(I) .GT. 0.2) THEN        
          !---CLASS 1; STABLE (NIGHTTIME) CONDITIONS:                                    
          REGIME(I)=1.
       ELSE
          !---CLASS 2; DAMPED MECHANICAL TURBULENCE:
          REGIME(I)=2.
       ENDIF

       !COMPUTE z/L first guess:
       IF (itimestep .LE. 1) THEN
          CALL Li_etal_2010(ZOL(I),BR(I),ZA(I)/ZNTstoch(I),zratio(I))
       ELSE
          ZOL(I)=ZA(I)*KARMAN*G*MOL(I)/(TH1D(I)*MAX(UST(I)*UST(I),0.0001))
          ZOL(I)=MAX(ZOL(I),0.0)
          ZOL(I)=MIN(ZOL(I),50.)
       ENDIF

       !Use Pedros iterative function to find z/L
       zol(I)=zolri(br(I),ZA(I),ZNTstoch(I),z_t(I),ZOL(I))
       ZOL(I)=MAX(ZOL(I),0.0)
       ZOL(I)=MIN(ZOL(I),50.)

       zolz0 = zol(I)*ZNTstoch(I)/ZA(I)          ! z0/L
       zolza = zol(I)*(za(I)+ZNTstoch(I))/za(I)  ! (z+z0/L
       zol10 = zol(I)*(10.+ZNTstoch(I))/za(I)    ! (10+z0)/L
       zol2  = zol(I)*(2.+ZNTstoch(I))/za(I)     ! (2+z0)/L 

       !COMPUTE PSIM and PSIH
       IF ((XLAND(I)-1.5).GE.0) THEN                                            
          ! WATER
          !CALL PSI_Suselj_Sood_2010(PSIM(I),PSIH(I),ZOL(I))
          !CALL PSI_Beljaars_Holtslag_1991(PSIM(I),PSIH(I),ZOL(I))
          !CALL PSI_Businger_1971(PSIM(I),PSIH(I),ZOL(I))
          !CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I),z_t(I),ZNTstoch(I),ZA(I))
          !CALL PSI_CB2005(PSIM(I),PSIH(I),zolza,zolz0)
          ! or use tables
          psim(I)=psim_stable(zolza)-psim_stable(zolz0)
          psih(I)=psih_stable(zolza)-psih_stable(zolz0)
          psim10(I)=psim_stable(zol10)-psim_stable(zolz0)
          psih10(I)=psih_stable(zol10)-psih_stable(zolz0)
          psih2(I)=psih_stable(zol2)-psih_stable(zolz0)
       ELSE
          ! LAND  
          !CALL PSI_Beljaars_Holtslag_1991(PSIM(I),PSIH(I),ZOL(I))
          !CALL PSI_Businger_1971(PSIM(I),PSIH(I),ZOL(I))
          !CALL PSI_Zilitinkevich_Esau_2007(PSIM(I),PSIH(I),ZOL(I))
          !CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I),z_t(I),ZNTstoch(I),ZA(I))
          !CALL PSI_CB2005(PSIM(I),PSIH(I),zolza,zolz0)
          ! or use tables
          psim(I)=psim_stable(zolza)-psim_stable(zolz0)
          psih(I)=psih_stable(zolza)-psih_stable(zolz0)
          psim10(I)=psim_stable(zol10)-psim_stable(zolz0)
          psih10(I)=psih_stable(zol10)-psih_stable(zolz0)
          psih2(I)=psih_stable(zol2)-psih_stable(zolz0)
       ENDIF

       !PSIM10(I)=10./ZA(I)*PSIM(I)
       !PSIH10(I)=10./ZA(I)*PSIH(I)
       !PSIM2(I)=2./ZA(I)*PSIM(I)
       !PSIH2(I)=2./ZA(I)*PSIH(I)

       ! 1.0 over Monin-Obukhov length
       RMOL(I)= ZOL(I)/ZA(I)

    ELSEIF(BR(I) .EQ. 0.) THEN                  
       !=========================================================  
       !-----CLASS 3; FORCED CONVECTION/NEUTRAL:                                                
       !=========================================================
       REGIME(I)=3.

       PSIM(I)=0.0
       PSIH(I)=PSIM(I)
       PSIM10(I)=0.
       PSIH10(I)=0.
       PSIH2(I)=0.

       !ZOL(I)=0.
       IF (UST(I) .LT. 0.01) THEN
          ZOL(I)=BR(I)*GZ1OZ0(I)
       ELSE
          ZOL(I)=KARMAN*GOVRTH(I)*ZA(I)*MOL(I)/(MAX(UST(I)*UST(I),0.001))
       ENDIF
       RMOL(I) = ZOL(I)/ZA(I)

    ELSEIF(BR(I) .LT. 0.)THEN
       !==========================================================
       !-----CLASS 4; FREE CONVECTION:                                                  
       !==========================================================
       REGIME(I)=4.

       !COMPUTE z/L first guess:
       IF (itimestep .LE. 1) THEN
          CALL Li_etal_2010(ZOL(I),BR(I),ZA(I)/ZNTstoch(I),zratio(I))
       ELSE
          ZOL(I)=ZA(I)*KARMAN*G*MOL(I)/(TH1D(I)*MAX(UST(I)*UST(I),0.001))
          ZOL(I)=MAX(ZOL(I),-50.0)
          ZOL(I)=MIN(ZOL(I),0.0)
       ENDIF

       !Use Pedros iterative function to find z/L
       zol(I)=zolri(br(I),ZA(I),ZNTstoch(I),z_t(I),ZOL(I))
       ZOL(I)=MAX(ZOL(I),-50.0)
       ZOL(I)=MIN(ZOL(I),0.0)

       zolz0 = zol(I)*ZNTstoch(I)/ZA(I)           ! z0/L
       zolza = zol(I)*(za(I)+ZNTstoch(I))/za(I)   ! (z+z0/L
       zol10 = zol(I)*(10.+ZNTstoch(I))/za(I)     ! (10+z0)/L
       zol2  = zol(I)*(2.+ZNTstoch(I))/za(I)      ! (2+z0)/L

       !COMPUTE PSIM and PSIH
       IF ((XLAND(I)-1.5).GE.0) THEN                                            
          ! WATER
          !CALL PSI_Suselj_Sood_2010(PSIM(I),PSIH(I),ZOL(I))
          !CALL PSI_Hogstrom_1996(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNTstoch(I), ZA(I))
          !CALL PSI_Businger_1971(PSIM(I),PSIH(I),ZOL(I))
          !CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I),z_t(I),ZNTstoch(I),ZA(I))
          ! use tables
          psim(I)=psim_unstable(zolza)-psim_unstable(zolz0)
          psih(I)=psih_unstable(zolza)-psih_unstable(zolz0)
          psim10(I)=psim_unstable(zol10)-psim_unstable(zolz0)
          psih10(I)=psih_unstable(zol10)-psih_unstable(zolz0)
          psih2(I)=psih_unstable(zol2)-psih_unstable(zolz0)
       ELSE           
          ! LAND  
          !CALL PSI_Hogstrom_1996(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNTstoch(I), ZA(I))
          !CALL PSI_Businger_1971(PSIM(I),PSIH(I),ZOL(I))
          !CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I),z_t(I),ZNTstoch(I),ZA(I))
          ! use tables
          psim(I)=psim_unstable(zolza)-psim_unstable(zolz0)
          psih(I)=psih_unstable(zolza)-psih_unstable(zolz0)
          psim10(I)=psim_unstable(zol10)-psim_unstable(zolz0)
          psih10(I)=psih_unstable(zol10)-psih_unstable(zolz0)
          psih2(I)=psih_unstable(zol2)-psih_unstable(zolz0)
       ENDIF              

       !PSIM10(I)=10./ZA(I)*PSIM(I)
       !PSIH2(I)=2./ZA(I)*PSIH(I)

       !---LIMIT PSIH AND PSIM IN THE CASE OF THIN LAYERS AND
       !---HIGH ROUGHNESS.  THIS PREVENTS DENOMINATOR IN FLUXES
       !---FROM GETTING TOO SMALL
       PSIH(I)=MIN(PSIH(I),0.9*GZ1OZt(I))
       PSIM(I)=MIN(PSIM(I),0.9*GZ1OZ0(I))
       PSIH2(I)=MIN(PSIH2(I),0.9*GZ2OZt(I))
       PSIM10(I)=MIN(PSIM10(I),0.9*GZ10OZ0(I))
       PSIH10(I)=MIN(PSIH10(I),0.9*GZ10OZt(I))

       RMOL(I) = ZOL(I)/ZA(I)  

    ENDIF

    !------------------------------------------------------------
    !-----COMPUTE THE FRICTIONAL VELOCITY:                                           
    !------------------------------------------------------------
    !     ZA(1982) EQS(2.60),(2.61).
    PSIX=GZ1OZ0(I)-PSIM(I)
    PSIX10=GZ10OZ0(I)-PSIM10(I)
    ! TO PREVENT OSCILLATIONS AVERAGE WITH OLD VALUE 
    OLDUST = UST(I)
    UST(I)=0.5*UST(I)+0.5*KARMAN*WSPD(I)/PSIX 
    !NON-AVERAGED: UST(I)=KARMAN*WSPD(I)/PSIX
     
    ! Compute u* without vconv for use in HFX calc when isftcflx > 0           
    WSPDI(I)=MAX(SQRT(U1D(I)*U1D(I)+V1D(I)*V1D(I)), wmin)
    IF ( PRESENT(USTM) ) THEN
       USTM(I)=0.5*USTM(I)+0.5*KARMAN*WSPDI(I)/PSIX
    ENDIF

    IF ((XLAND(I)-1.5).LT.0.) THEN        !LAND
       UST(I)=MAX(UST(I),0.005)  !Further relaxing this limit - no need to go lower
       !Keep ustm = ust over land.
       IF ( PRESENT(USTM) ) USTM(I)=UST(I)
    ENDIF

    !------------------------------------------------------------
    !-----COMPUTE THE THERMAL AND MOISTURE RESISTANCE (PSIQ AND PSIT):
    !------------------------------------------------------------
    ! LOWER LIMIT ADDED TO PREVENT LARGE FLHC IN SOIL MODEL
    ! ACTIVATES IN UNSTABLE CONDITIONS WITH THIN LAYERS OR HIGH Z0
    GZ1OZt(I)= LOG((ZA(I)+z_t(i))/z_t(i))
    GZ2OZt(I)= LOG((2.0+z_t(i))/z_t(i))                                        

    PSIT =MAX(GZ1OZt(I)-PSIH(I) ,1.)
    PSIT2=MAX(GZ2OZt(I)-PSIH2(I),1.)

    PSIQ=MAX(LOG((ZA(I)+z_q(i))/z_q(I))-PSIH(I) ,1.0)
    PSIQ2=MAX(LOG((2.0+z_q(i))/z_q(I))-PSIH2(I) ,1.0)
    PSIQ10=MAX(LOG((10.0+z_q(i))/z_q(I))-PSIH10(I) ,1.0)

    !----------------------------------------------------
    !COMPUTE THE TEMPERATURE SCALE (or FRICTION TEMPERATURE, T*)
    !----------------------------------------------------
    DTG=THV1D(I)-THVGB(I)
    OLDTST=MOL(I)
    MOL(I)=KARMAN*DTG/PSIT/PRT
    !t_star(I) = -HFX(I)/(UST(I)*CPM(I)*RHO1D(I))
    !t_star(I) = MOL(I)
    !----------------------------------------------------
    !COMPUTE THE MOISTURE SCALE (or q*)
    DQG=(QVSH(i)-qsfc(i))*1000.   !(kg/kg -> g/kg)
    qstar(I)=KARMAN*DQG/PSIQ/PRT

    !IF () THEN
        !  write(*,1001)"REGIME:",REGIME(I)," z/L:",ZOL(I)," U*:",UST(I)," Tstar:",MOL(I)
        !  write(*,1002)"PSIM:",PSIM(I)," PSIH:",PSIH(I)," W*:",WSTAR(I)," DTHV:",THV1D(I)-THVGB(I)
        !  write(*,1003)"CPM:",CPM(I)," RHO1D:",RHO1D(I)," L:",ZOL(I)/ZA(I)," DTH:",TH1D(I)-THGB(I)
        !  write(*,1004)"Z0/Zt:",zratio(I)," Z0:",ZNTstoch(I)," Zt:",z_t(I)," za:",za(I)
        !  write(*,1005)"Re:",restar," MAVAIL:",MAVAIL(I)," QSFC(I):",QSFC(I)," QVSH(I):",QVSH(I)
        !  print*,"VISC=",VISC," Z0:",ZNTstoch(I)," T1D(i):",T1D(i)
        !  write(*,*)"============================================="
    !ENDIF

 ENDDO     ! end i-loop

 1000   format(A,F6.1, A,f6.1, A,f5.1, A,f7.1)
 1001   format(A,F2.0, A,f10.4,A,f5.3, A,f11.5)
 1002   format(A,f7.2, A,f7.2, A,f7.2, A,f10.3)
 1003   format(A,f7.2, A,f7.2, A,f10.3,A,f10.3)
 1004   format(A,f11.3,A,f9.7, A,f9.7, A,f6.2, A,f10.3)
 1005   format(A,f9.2,A,f6.4,A,f7.4,A,f7.4)

      !----------------------------------------------------------
      !  COMPUTE SURFACE HEAT AND MOISTURE FLUXES
      !----------------------------------------------------------
 DO I=its,ite

    !For computing the diagnostics and fluxes (below), whether the fluxes
    !are turned off or on, we need the following:
    PSIX=GZ1OZ0(I)-PSIM(I)
    PSIX10=GZ10OZ0(I)-PSIM10(I)

    PSIT =MAX(GZ1OZt(I)-PSIH(I), 1.0)
    PSIT2=MAX(GZ2OZt(I)-PSIH2(I),1.0)
  
    PSIQ=MAX(LOG((ZA(I)+z_q(i))/z_q(I))-PSIH(I) ,1.0)
    PSIQ2=MAX(LOG((2.0+z_q(i))/z_q(I))-PSIH2(I) ,1.0)
    PSIQ10=MAX(LOG((10.0+z_q(i))/z_q(I))-PSIH10(I) ,1.0)

    IF (ISFFLX .LT. 1) THEN                            

       QFX(i)  = 0.                                                              
       HFX(i)  = 0.    
       FLHC(I) = 0.                                                             
       FLQC(I) = 0.                                                             
       LH(I)   = 0.                                                             
       CHS(I)  = 0.                                                             
       CH(I)   = 0.                                                             
       CHS2(i) = 0.                                                              
       CQS2(i) = 0.                                                              
       IF(PRESENT(ck)  .and. PRESENT(cd) .and. &
         &PRESENT(cka) .and. PRESENT(cda)) THEN
           Ck(I) = 0.
           Cd(I) = 0.
           Cka(I)= 0.
           Cda(I)= 0.
       ENDIF
    ELSE

      !------------------------------------------
      ! CALCULATE THE EXCHANGE COEFFICIENTS FOR HEAT (FLHC)
      ! AND MOISTURE (FLQC)
      !------------------------------------------
      FLQC(I)=RHO1D(I)*MAVAIL(I)*UST(I)*KARMAN/PSIQ
      FLHC(I)=RHO1D(I)*CPM(I)*UST(I)*KARMAN/PSIT

      !----------------------------------
      ! COMPUTE SURFACE MOISTURE FLUX:
      !----------------------------------
      QFX(I)=FLQC(I)*(QSFCMR(I)-QV1D(I))
      !JOE: QFX(I)=MAX(QFX(I),0.)   !originally did not allow neg QFX           
      QFX(I)=MAX(QFX(I),-0.02)      !allows small neg QFX, like MYJ
      LH(I)=XLV*QFX(I)

      !----------------------------------
      ! COMPUTE SURFACE HEAT FLUX:
      !----------------------------------
      IF(XLAND(I)-1.5.GT.0.)THEN      !WATER                                           
         HFX(I)=FLHC(I)*(THGB(I)-TH1D(I))                                
         IF ( PRESENT(ISFTCFLX) ) THEN
            IF ( ISFTCFLX.NE.0 ) THEN
               ! AHW: add dissipative heating term
               HFX(I)=HFX(I)+RHO1D(I)*USTM(I)*USTM(I)*WSPDI(I)
            ENDIF
         ENDIF
      ELSEIF(XLAND(I)-1.5.LT.0.)THEN  !LAND                               
         HFX(I)=FLHC(I)*(THGB(I)-TH1D(I))                                
         HFX(I)=MAX(HFX(I),-250.)                                       
      ENDIF

      !CHS(I)=UST(I)*KARMAN/(ALOG(KARMAN*UST(I)*ZA(I) &
      !       /XKA+ZA(I)/ZL)-PSIH(I))

      CHS(I)=UST(I)*KARMAN/PSIT

      ! The exchange coefficient for cloud water is assumed to be the 
      ! same as that for heat. CH is multiplied by WSPD.

      !ch(i)=chs(i)
      ch(i)=flhc(i)/( cpm(i)*RHO1D(i) )

      !THESE ARE USED FOR 2-M DIAGNOSTICS ONLY
      CQS2(I)=UST(I)*KARMAN/PSIQ2
      CHS2(I)=UST(I)*KARMAN/PSIT2

      IF(PRESENT(ck)  .and. PRESENT(cd) .and. &
        &PRESENT(cka) .and. PRESENT(cda)) THEN
         Ck(I)=(karman/psix10)*(karman/psiq10)
         Cd(I)=(karman/psix10)*(karman/psix10)
         Cka(I)=(karman/psix)*(karman/psiq)
         Cda(I)=(karman/psix)*(karman/psix)
      ENDIF

   ENDIF !end ISFFLX option

   !-----------------------------------------------------
   !COMPUTE DIAGNOSTICS
   !-----------------------------------------------------
   !COMPUTE 10 M WNDS
   !-----------------------------------------------------
   ! If the lowest model level is close to 10-m, use it
   ! instead of the flux-based diagnostic formula.
   if (ZA(i) .le. 7.0) then
      ! high vertical resolution
      if(ZA2(i) .gt. 7.0 .and. ZA2(i) .lt. 13.0) then
         !use 2nd model level
         U10(I)=U1D2(I)
         V10(I)=V1D2(I)
      else
         U10(I)=U1D(I)*log(10./ZNTstoch(I))/log(ZA(I)/ZNTstoch(I))
         V10(I)=V1D(I)*log(10./ZNTstoch(I))/log(ZA(I)/ZNTstoch(I))
      endif
   elseif(ZA(i) .gt. 7.0 .and. ZA(i) .lt. 13.0) then
      !moderate vertical resolution
      !U10(I)=U1D(I)*PSIX10/PSIX
      !V10(I)=V1D(I)*PSIX10/PSIX
      !use neutral-log:
      U10(I)=U1D(I)*log(10./ZNTstoch(I))/log(ZA(I)/ZNTstoch(I))
      V10(I)=V1D(I)*log(10./ZNTstoch(I))/log(ZA(I)/ZNTstoch(I))
   else
      ! very coarse vertical resolution
      U10(I)=U1D(I)*PSIX10/PSIX
      V10(I)=V1D(I)*PSIX10/PSIX
   endif

   !-----------------------------------------------------
   !COMPUTE 2m T, TH, AND Q
   !THESE WILL BE OVERWRITTEN FOR LAND POINTS IN THE LSM
   !-----------------------------------------------------
   DTG=TH1D(I)-THGB(I) 
   TH2(I)=THGB(I)+DTG*PSIT2/PSIT
   !***  BE CERTAIN THAT THE 2-M THETA IS BRACKETED BY
   !***  THE VALUES AT THE SURFACE AND LOWEST MODEL LEVEL.
   IF ((TH1D(I)>THGB(I) .AND. (TH2(I)<THGB(I) .OR. TH2(I)>TH1D(I))) .OR. &
       (TH1D(I)<THGB(I) .AND. (TH2(I)>THGB(I) .OR. TH2(I)<TH1D(I)))) THEN
       TH2(I)=THGB(I) + 2.*(TH1D(I)-THGB(I))/ZA(I)
   ENDIF
   T2(I)=TH2(I)*(PSFC(I)/100.)**ROVCP

   Q2(I)=QSFCMR(I)+(QV1D(I)-QSFCMR(I))*PSIQ2/PSIQ
   Q2(I)= MAX(Q2(I), MIN(QSFCMR(I), QV1D(I)))
   Q2(I)= MIN(Q2(I), 1.05*QV1D(I))

   IF ( debug_code ) THEN
      yesno = 0
      IF (HFX(I) > 1200. .OR. HFX(I) < -700.)THEN
            print*,"SUSPICIOUS VALUES IN MYNN SFCLAYER",&
            I,J, "HFX: ",HFX(I)
            yesno = 1
      ENDIF
      IF (LH(I)  > 1200. .OR. LH(I)  < -700.)THEN
            print*,"SUSPICIOUS VALUES IN MYNN SFCLAYER",&
            I,J, "LH: ",LH(I)
            yesno = 1
      ENDIF
      IF (UST(I) < 0.0 .OR. UST(I) > 4.0 )THEN
            print*,"SUSPICIOUS VALUES IN MYNN SFCLAYER",&
            I,J, "UST: ",UST(I)
            yesno = 1
      ENDIF
      IF (WSTAR(I)<0.0 .OR. WSTAR(I) > 6.0)THEN
            print*,"SUSPICIOUS VALUES IN MYNN SFCLAYER",&
            I,J, "WSTAR: ",WSTAR(I)
            yesno = 1
      ENDIF
      IF (RHO1D(I)<0.0 .OR. RHO1D(I) > 1.6 )THEN
            print*,"SUSPICIOUS VALUES IN MYNN SFCLAYER",&
            I,J, "rho: ",RHO1D(I)
            yesno = 1
      ENDIF
      IF (QSFC(I)*1000. <0.0 .OR. QSFC(I)*1000. >40.)THEN
            print*,"SUSPICIOUS VALUES IN MYNN SFCLAYER",&
            I,J, "QSFC: ",QSFC(I)
            yesno = 1
      ENDIF
      IF (PBLH(I)<0. .OR. PBLH(I)>6000.)THEN
            print*,"SUSPICIOUS VALUES IN MYNN SFCLAYER",&
            I,J, "PBLH: ",PBLH(I)
            yesno = 1
      ENDIF

      IF (yesno == 1) THEN
        print*," OTHER INFO:"
        write(*,1001)"REGIME:",REGIME(I)," z/L:",ZOL(I)," U*:",UST(I),&
              " Tstar:",MOL(I)
        write(*,1002)"PSIM:",PSIM(I)," PSIH:",PSIH(I)," W*:",WSTAR(I),&
              " DTHV:",THV1D(I)-THVGB(I)
        write(*,1003)"CPM:",CPM(I)," RHO1D:",RHO1D(I)," L:",&
              ZOL(I)/ZA(I)," DTH:",TH1D(I)-THGB(I)
        write(*,*)" Z0:",ZNTstoch(I)," Zt:",z_t(I)," za:",za(I)
        write(*,1005)"Re:",restar," MAVAIL:",MAVAIL(I)," QSFC(I):",&
              QSFC(I)," QVSH(I):",QVSH(I)
        print*,"PSIX=",PSIX," Z0:",ZNTstoch(I)," T1D(i):",T1D(i)
        write(*,*)"============================================="
      ENDIF
   ENDIF

 ENDDO !end i-loop

END SUBROUTINE SFCLAY1D_mynn
!-------------------------------------------------------------------          
   SUBROUTINE zilitinkevich_1995(Z_0,Zt,Zq,restar,ustar,KARMAN,&
       & landsea,IZ0TLND2,spp_pbl,rstoch)

       ! This subroutine returns the thermal and moisture roughness lengths
       ! from Zilitinkevich (1995) and Zilitinkevich et al. (2001) over
       ! land and water, respectively. 
       !
       ! MODS:
       ! 20120705 : added IZ0TLND option. Note: This option was designed
       !            to work with the Noah LSM and may be specific for that
       !            LSM only. Tests with RUC LSM showed no improvements. 

       IMPLICIT NONE
       REAL, INTENT(IN) :: Z_0,restar,ustar,KARMAN,landsea
       INTEGER, OPTIONAL, INTENT(IN)::  IZ0TLND2
       REAL, INTENT(OUT) :: Zt,Zq
       REAL :: CZIL  !=0.100 in Chen et al. (1997)
                     !=0.075 in Zilitinkevich (1995)
                     !=0.500 in Lemone et al. (2008)
       INTEGER,  INTENT(IN)  ::    spp_pbl
       REAL,     INTENT(IN)  ::    rstoch


       IF (landsea-1.5 .GT. 0) THEN    !WATER

          !THIS IS BASED ON Zilitinkevich, Grachev, and Fairall (2001;
          !Their equations 15 and 16).
          IF (restar .LT. 0.1) THEN
             Zt = Z_0*EXP(KARMAN*2.0)
             Zt = MIN( Zt, 6.0e-5)
             Zt = MAX( Zt, 2.0e-9)
             Zq = Z_0*EXP(KARMAN*3.0)
             Zq = MIN( Zq, 6.0e-5)
             Zq = MAX( Zq, 2.0e-9)
          ELSE
             Zt = Z_0*EXP(-KARMAN*(4.0*SQRT(restar)-3.2))
             Zt = MIN( Zt, 6.0e-5)
             Zt = MAX( Zt, 2.0e-9)
             Zq = Z_0*EXP(-KARMAN*(4.0*SQRT(restar)-4.2))
             Zq = MIN( Zt, 6.0e-5)
             Zq = MAX( Zt, 2.0e-9)
          ENDIF

       ELSE                             !LAND

          !Option to modify CZIL according to Chen & Zhang, 2009
          IF ( IZ0TLND2 .EQ. 1 ) THEN
             CZIL = 10.0 ** ( -0.40 * ( Z_0 / 0.07 ) )
          ELSE
             CZIL = 0.085 !0.075 !0.10
          END IF

          Zt = Z_0*EXP(-KARMAN*CZIL*SQRT(restar))
          Zt = MIN( Zt, 0.75*Z_0)

          Zq = Z_0*EXP(-KARMAN*CZIL*SQRT(restar))
          Zq = MIN( Zq, 0.75*Z_0)

! stochastically perturb thermal and moisture roughness length.
! currently set to half the amplitude: 
          if (spp_pbl==1) then
             Zt = Zt + Zt * 0.5 * rstoch
             Zt = MAX(Zt, 0.0001)
             Zq = Zt
          endif

       ENDIF
                   
       return

   END SUBROUTINE zilitinkevich_1995
!--------------------------------------------------------------------
   SUBROUTINE davis_etal_2008(Z_0,ustar)

    !a.k.a. : Donelan et al. (2004)
    !This formulation for roughness length was designed to match 
    !the labratory experiments of Donelan et al. (2004).
    !This is an update version from Davis et al. 2008, which
    !corrects a small-bias in Z_0 (AHW real-time 2012).

       IMPLICIT NONE
       REAL, INTENT(IN)  :: ustar
       REAL, INTENT(OUT)  :: Z_0
       REAL :: ZW, ZN1, ZN2
       REAL, PARAMETER :: G=9.81, OZO=1.59E-5

       !OLD FORM: Z_0 = 10.*EXP(-10./(ustar**(1./3.)))
       !NEW FORM:

       ZW  = MIN((ustar/1.06)**(0.3),1.0)
       ZN1 = 0.011*ustar*ustar/G + OZO
       ZN2 = 10.*exp(-9.5*ustar**(-.3333)) + &
             0.11*1.5E-5/AMAX1(ustar,0.01)
       Z_0 = (1.0-ZW) * ZN1 + ZW * ZN2

       Z_0 = MAX( Z_0, 1.27e-7)  !These max/mins were suggested by
       Z_0 = MIN( Z_0, 2.85e-3)  !Davis et al. (2008)
                   
       return

   END SUBROUTINE davis_etal_2008
!--------------------------------------------------------------------
   SUBROUTINE Taylor_Yelland_2001(Z_0,ustar,wsp10)

    !This formulation for roughness length was designed account for 
    !wave steepness.

       IMPLICIT NONE
       REAL, INTENT(IN)  :: ustar,wsp10
       REAL, INTENT(OUT) :: Z_0
       REAL, parameter  :: g=9.81, pi=3.14159265
       REAL :: hs, Tp, Lp

       !hs is the significant wave height
        hs = 0.0248*(wsp10**2.)
       !Tp dominant wave period
        Tp = 0.729*MAX(wsp10,0.1)
       !Lp is the wavelength of the dominant wave
        Lp = g*Tp**2/(2*pi)

       Z_0 = 1200.*hs*(hs/Lp)**4.5
       Z_0 = MAX( Z_0, 1.27e-7)  !These max/mins were suggested by
       Z_0 = MIN( Z_0, 2.85e-3)  !Davis et al. (2008)
                   
       return

   END SUBROUTINE Taylor_Yelland_2001
!--------------------------------------------------------------------
   SUBROUTINE charnock_1955(Z_0,ustar,wsp10,visc,zu)
 
    !This version of Charnock's relation employs a varying
    !Charnock parameter, similar to COARE3.0 [Fairall et al. (2003)].
    !The Charnock parameter CZC is varied from .011 to .018 
    !between 10-m wsp = 10 and 18. 

       IMPLICIT NONE
       REAL, INTENT(IN)  :: ustar, visc, wsp10, zu
       REAL, INTENT(OUT) :: Z_0
       REAL, PARAMETER   :: G=9.81, CZO2=0.011
       REAL              :: CZC    !variable charnock "constant"   
       REAL              :: wsp10m ! logarithmically calculated 10 m

       wsp10m = wsp10*log(10./1e-4)/log(zu/1e-4)
       CZC = CZO2 + 0.007*MIN(MAX((wsp10m-10.)/8., 0.), 1.0)

       Z_0 = CZC*ustar*ustar/G + (0.11*visc/MAX(ustar,0.05))
       Z_0 = MAX( Z_0, 1.27e-7)  !These max/mins were suggested by
       Z_0 = MIN( Z_0, 2.85e-3)  !Davis et al. (2008)

       return

   END SUBROUTINE charnock_1955
!--------------------------------------------------------------------
   SUBROUTINE edson_etal_2013(Z_0,ustar,wsp10,visc,zu)

     !This version of Charnock's relation employs a varying
     !Charnock parameter, taken from COARE 3.5 [Edson et al. (2001, JPO)].
     !The Charnock parameter CZC is varied from about .005 to .028
     !between 10-m wind speeds of 6 and 19 m/s.

       IMPLICIT NONE
       REAL, INTENT(IN)  :: ustar, visc, wsp10, zu
       REAL, INTENT(OUT) :: Z_0
       REAL, PARAMETER   :: G=9.81
       REAL, PARAMETER   :: m=0.017, b=-0.005
       REAL              :: CZC    ! variable charnock "constant"
       REAL              :: wsp10m ! logarithmically calculated 10 m

       wsp10m = wsp10*log(10/1e-4)/log(zu/1e-4)
       wsp10m = MIN(19., wsp10m)
       CZC    = m*wsp10m + b
       CZC    = MAX(CZC, 0.0)

       Z_0 = CZC*ustar*ustar/G + (0.11*visc/MAX(ustar,0.07))
       Z_0 = MAX( Z_0, 1.27e-7)  !These max/mins were suggested by
       Z_0 = MIN( Z_0, 2.85e-3)  !Davis et al. (2008)

       return

   END SUBROUTINE edson_etal_2013
!--------------------------------------------------------------------
   SUBROUTINE garratt_1992(Zt,Zq,Z_0,Ren,landsea)

    !This formulation for the thermal and moisture roughness lengths
    !(Zt and Zq) relates them to Z0 via the roughness Reynolds number (Ren).
    !This formula comes from Fairall et al. (2003). It is modified from
    !the original Garratt-Brutsaert model to better fit the COARE/HEXMAX
    !data. The formula for land uses a constant ratio (Z_0/7.4) taken
    !from Garratt (1992).

       IMPLICIT NONE
       REAL, INTENT(IN)  :: Ren, Z_0,landsea
       REAL, INTENT(OUT) :: Zt,Zq
       REAL :: Rq
       REAL, PARAMETER  :: e=2.71828183

       IF (landsea-1.5 .GT. 0) THEN    !WATER

          Zt = Z_0*EXP(2.0 - (2.48*(Ren**0.25)))
          Zq = Z_0*EXP(2.0 - (2.28*(Ren**0.25)))

          Zq = MIN( Zq, 5.5e-5)
          Zq = MAX( Zq, 2.0e-9)
          Zt = MIN( Zt, 5.5e-5)
          Zt = MAX( Zt, 2.0e-9) !same lower limit as ECMWF
       ELSE                            !LAND
          Zq = Z_0/(e**2.)      !taken from Garratt (1980,1992)
          Zt = Zq
       ENDIF
                   
       return

    END SUBROUTINE garratt_1992
!--------------------------------------------------------------------
    SUBROUTINE fairall_etal_2003(Zt,Zq,Ren,ustar,visc,rstoch,spp_pbl)

    !This formulation for thermal and moisture roughness length (Zt and Zq)
    !as a function of the roughness Reynolds number (Ren) comes from the
    !COARE3.0 formulation, empirically derived from COARE and HEXMAX data
    ![Fairall et al. (2003)]. Edson et al. (2004; JGR) suspected that this
    !relationship overestimated the scalar roughness lengths for low Reynolds
    !number flows, so an optional smooth flow relationship, taken from Garratt
    !(1992, p. 102), is available for flows with Ren < 2.
    !
    !This is for use over water only.

       IMPLICIT NONE
       REAL, INTENT(IN)   :: Ren,ustar,visc,rstoch
       INTEGER, INTENT(IN):: spp_pbl
       REAL, INTENT(OUT)  :: Zt,Zq

       IF (Ren .le. 2.) then

          Zt = (5.5e-5)*(Ren**(-0.60))
          Zq = Zt
          !FOR SMOOTH SEAS, CAN USE GARRATT
          !Zq = 0.2*visc/MAX(ustar,0.1)
          !Zq = 0.3*visc/MAX(ustar,0.1)

       ELSE

          !FOR ROUGH SEAS, USE COARE
          Zt = (5.5e-5)*(Ren**(-0.60))
          Zq = Zt

       ENDIF

       if (spp_pbl==1) then
          Zt = Zt + Zt * 0.5 * rstoch
          Zq = Zt
       endif

       Zt = MIN(Zt,1.0e-4)
       Zt = MAX(Zt,2.0e-9)

       Zq = MIN(Zt,1.0e-4)
       Zq = MAX(Zt,2.0e-9)

       return

    END SUBROUTINE fairall_etal_2003
!--------------------------------------------------------------------
    SUBROUTINE fairall_etal_2014(Zt,Zq,Ren,ustar,visc,rstoch,spp_pbl)

    !This formulation for thermal and moisture roughness length (Zt and Zq)
    !as a function of the roughness Reynolds number (Ren) comes from the
    !COARE 3.5/4.0 formulation, empirically derived from COARE and HEXMAX data
    ![Fairall et al. (2014? coming soon, not yet published as of July 2014)].
    !This is for use over water only.

       IMPLICIT NONE
       REAL, INTENT(IN)  :: Ren,ustar,visc,rstoch
       INTEGER, INTENT(IN):: spp_pbl
       REAL, INTENT(OUT) :: Zt,Zq

       !Zt = (5.5e-5)*(Ren**(-0.60))
       Zt = MIN(1.6E-4, 5.8E-5/(Ren**0.72))
       Zq = Zt

       IF (spp_pbl ==1) THEN
          Zt = MAX(Zt + Zt*0.5*rstoch,2.0e-9)
          Zq = MAX(Zt + Zt*0.5*rstoch,2.0e-9)
       ELSE
          Zt = MAX(Zt,2.0e-9)
          Zq = MAX(Zt,2.0e-9)
       ENDIF

       return

    END SUBROUTINE fairall_etal_2014
!--------------------------------------------------------------------
    SUBROUTINE Yang_2008(Z_0,Zt,Zq,ustar,tstar,qst,Ren,visc,landsea)

    !This is a modified version of Yang et al (2002 QJRMS, 2008 JAMC) 
    !and Chen et al (2010, J of Hydromet). Although it was originally 
    !designed for arid regions with bare soil, it is modified 
    !here to perform over a broader spectrum of vegetation.
    !
    !The original formulation relates the thermal roughness length (Zt) 
    !to u* and T*:
    !  
    ! Zt = ht * EXP(-beta*(ustar**0.5)*(ABS(tstar)**0.25))
    !
    !where ht = Renc*visc/ustar and the critical Reynolds number 
    !(Renc) = 70. Beta was originally = 10 (2002 paper) but was revised 
    !to 7.2 (in 2008 paper). Their form typically varies the
    !ratio Z0/Zt by a few orders of magnitude (1-1E4).
    !
    !This modified form uses beta = 1.5 and a variable Renc (function of Z_0),
    !so zt generally varies similarly to the Zilitinkevich form (with Czil ~ 0.1)
    !for very small or negative surface heat fluxes but can become close to the
    !Zilitinkevich with Czil = 0.2 for very large HFX (large negative T*).
    !Also, the exponent (0.25) on tstar was changed to 1.0, since we found
    !Zt was reduced too much for low-moderate positive heat fluxes.
    !
    !This should only be used over land!

       IMPLICIT NONE
       REAL, INTENT(IN)  :: Z_0, Ren, ustar, tstar, qst, visc, landsea
       REAL              :: ht,     &! roughness height at critical Reynolds number
                            tstar2, &! bounded T*, forced to be non-positive
                            qstar2, &! bounded q*, forced to be non-positive
                            Z_02,   &! bounded Z_0 for variable Renc2 calc
                            Renc2    ! variable Renc, function of Z_0
       REAL, INTENT(OUT) :: Zt,Zq
       REAL, PARAMETER  :: Renc=300., & !old constant Renc
                           beta=1.5,  & !important for diurnal variation
                           m=170.,    & !slope for Renc2 function
                           b=691.       !y-intercept for Renc2 function

       Z_02 = MIN(Z_0,0.5)
       Z_02 = MAX(Z_02,0.04)
       Renc2= b + m*log(Z_02)
       ht     = Renc2*visc/MAX(ustar,0.01)
       tstar2 = MIN(tstar, 0.0)
       qstar2 = MIN(qst,0.0)

       Zt     = ht * EXP(-beta*(ustar**0.5)*(ABS(tstar2)**1.0))
       Zq     = ht * EXP(-beta*(ustar**0.5)*(ABS(qstar2)**1.0))
       !Zq     = Zt

       Zt = MIN(Zt, Z_0/2.0)
       Zq = MIN(Zq, Z_0/2.0)

       return

    END SUBROUTINE Yang_2008
!--------------------------------------------------------------------
    SUBROUTINE Andreas_2002(Z_0,bvisc,ustar,Zt,Zq)

    ! This is taken from Andreas (2002; J. of Hydromet) and 
    ! Andreas et al. (2005; BLM).
    !
    ! This should only be used over snow/ice!

       IMPLICIT NONE
       REAL, INTENT(IN)  :: Z_0, bvisc, ustar
       REAL, INTENT(OUT) :: Zt, Zq
       REAL :: Ren2, zntsno

       REAL, PARAMETER  :: bt0_s=1.25,  bt0_t=0.149,  bt0_r=0.317,  &
                           bt1_s=0.0,   bt1_t=-0.55,  bt1_r=-0.565, &
                           bt2_s=0.0,   bt2_t=0.0,    bt2_r=-0.183

       REAL, PARAMETER  :: bq0_s=1.61,  bq0_t=0.351,  bq0_r=0.396,  &
                           bq1_s=0.0,   bq1_t=-0.628, bq1_r=-0.512, &
                           bq2_s=0.0,   bq2_t=0.0,    bq2_r=-0.180

      !Calculate zo for snow (Andreas et al. 2005, BLM)                                                                     
       zntsno = 0.135*bvisc/ustar + &
               (0.035*(ustar*ustar)/9.8) * &
               (5.*exp(-1.*(((ustar - 0.18)/0.1)*((ustar - 0.18)/0.1))) + 1.)                                                
       Ren2 = ustar*zntsno/bvisc

       ! Make sure that Re is not outside of the range of validity
       ! for using their equations
       IF (Ren2 .gt. 1000.) Ren2 = 1000. 

       IF (Ren2 .le. 0.135) then

          Zt = zntsno*EXP(bt0_s + bt1_s*LOG(Ren2) + bt2_s*LOG(Ren2)**2)
          Zq = zntsno*EXP(bq0_s + bq1_s*LOG(Ren2) + bq2_s*LOG(Ren2)**2)

       ELSE IF (Ren2 .gt. 0.135 .AND. Ren2 .lt. 2.5) then

          Zt = zntsno*EXP(bt0_t + bt1_t*LOG(Ren2) + bt2_t*LOG(Ren2)**2)
          Zq = zntsno*EXP(bq0_t + bq1_t*LOG(Ren2) + bq2_t*LOG(Ren2)**2)

       ELSE

          Zt = zntsno*EXP(bt0_r + bt1_r*LOG(Ren2) + bt2_r*LOG(Ren2)**2)
          Zq = zntsno*EXP(bq0_r + bq1_r*LOG(Ren2) + bq2_r*LOG(Ren2)**2)

       ENDIF

       return

    END SUBROUTINE Andreas_2002
!--------------------------------------------------------------------
    SUBROUTINE PSI_Hogstrom_1996(psi_m, psi_h, zL, Zt, Z_0, Za)

    ! This subroutine returns the stability functions based off
    ! of Hogstrom (1996).

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL, Zt, Z_0, Za
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL  :: x, x0, y, y0, zmL, zhL

       zmL = Z_0*zL/Za  
       zhL = Zt*zL/Za

       IF (zL .gt. 0.) THEN  !STABLE (not well tested - seem large)

          psi_m = -5.3*(zL - zmL)
          psi_h = -8.0*(zL - zhL)
 
       ELSE                 !UNSTABLE

          x = (1.-19.0*zL)**0.25
          x0= (1.-19.0*zmL)**0.25
          y = (1.-11.6*zL)**0.5
          y0= (1.-11.6*zhL)**0.5

          psi_m = 2.*LOG((1.+x)/(1.+x0)) + &
                    &LOG((1.+x**2.)/(1.+x0**2.)) - &
                    &2.0*ATAN(x) + 2.0*ATAN(x0)
          psi_h = 2.*LOG((1.+y)/(1.+y0))

       ENDIF
                   
       return

    END SUBROUTINE PSI_Hogstrom_1996
!--------------------------------------------------------------------
    SUBROUTINE PSI_DyerHicks(psi_m, psi_h, zL, Zt, Z_0, Za)

    ! This subroutine returns the stability functions based off
    ! of Hogstrom (1996), but with different constants compatible
    ! with Dyer and Hicks (1970/74?). This formulation is used for
    ! testing/development by Nakanishi (personal communication).

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL, Zt, Z_0, Za
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL  :: x, x0, y, y0, zmL, zhL

       zmL = Z_0*zL/Za  !Zo/L
       zhL = Zt*zL/Za   !Zt/L

       IF (zL .gt. 0.) THEN  !STABLE

          psi_m = -5.0*(zL - zmL)
          psi_h = -5.0*(zL - zhL)
 
       ELSE                 !UNSTABLE

          x = (1.-16.*zL)**0.25
          x0= (1.-16.*zmL)**0.25

          y = (1.-16.*zL)**0.5
          y0= (1.-16.*zhL)**0.5

          psi_m = 2.*LOG((1.+x)/(1.+x0)) + &
                    &LOG((1.+x**2.)/(1.+x0**2.)) - & 
                    &2.0*ATAN(x) + 2.0*ATAN(x0)
          psi_h = 2.*LOG((1.+y)/(1.+y0))

       ENDIF
                   
       return

    END SUBROUTINE PSI_DyerHicks
!--------------------------------------------------------------------
    SUBROUTINE PSI_Beljaars_Holtslag_1991(psi_m, psi_h, zL)

    ! This subroutine returns the stability functions based off
    ! of Beljaar and Holtslag 1991, which is an extension of Holtslag
    ! and Debruin 1989.

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL, PARAMETER  :: a=1., b=0.666, c=5., d=0.35

       IF (zL .lt. 0.) THEN  !UNSTABLE

          WRITE(*,*)"WARNING: Universal stability functions from"
          WRITE(*,*)"        Beljaars and Holtslag (1991) should only"
          WRITE(*,*)"        be used in the stable regime!"
          psi_m = 0.
          psi_h = 0.
 
       ELSE                 !STABLE

          psi_m = -(a*zL + b*(zL -(c/d))*exp(-d*zL) + (b*c/d))
          psi_h = -((1.+.666*a*zL)**1.5 + &
                  b*(zL - (c/d))*exp(-d*zL) + (b*c/d) -1.)

       ENDIF
                   
       return

    END SUBROUTINE PSI_Beljaars_Holtslag_1991
!--------------------------------------------------------------------
    SUBROUTINE PSI_Zilitinkevich_Esau_2007(psi_m, psi_h, zL)

    ! This subroutine returns the stability functions come from
    ! Zilitinkevich and Esau (2007, BM), which are formulatioed from the
    ! "generalized similarity theory" and tuned to the LES DATABASE64
    ! to determine their dependence on z/L.

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL, PARAMETER  :: Cm=3.0, Ct=2.5

       IF (zL .lt. 0.) THEN  !UNSTABLE

          WRITE(*,*)"WARNING: Universal stability function from"
          WRITE(*,*)"        Zilitinkevich and Esau (2007) should only"
          WRITE(*,*)"        be used in the stable regime!"
          psi_m = 0.
          psi_h = 0.
 
       ELSE                 !STABLE

          psi_m = -Cm*(zL**(5./6.))
          psi_h = -Ct*(zL**(4./5.))

       ENDIF
                   
       return

    END SUBROUTINE PSI_Zilitinkevich_Esau_2007
!--------------------------------------------------------------------
    SUBROUTINE PSI_Businger_1971(psi_m, psi_h, zL)

    ! This subroutine returns the flux-profile relationships
    ! of Businger el al. 1971.

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL  :: x, y
       REAL, PARAMETER  ::  Pi180 = 3.14159265/180.

       IF (zL .lt. 0.) THEN  !UNSTABLE

          x = (1. - 15.0*zL)**0.25
          y = (1. - 9.0*zL)**0.5

          psi_m = LOG(((1.+x)/2.)**2.) + &
                 &LOG((1.+x**2.)/2.) - &
                 &2.0*ATAN(x) + Pi180*90.
          psi_h = 2.*LOG((1.+y)/2.)

       ELSE                 !STABLE

          psi_m = -4.7*zL
          psi_h = -(4.7/0.74)*zL

       ENDIF
                   
       return

    END SUBROUTINE PSI_Businger_1971
!--------------------------------------------------------------------
    SUBROUTINE PSI_Suselj_Sood_2010(psi_m, psi_h, zL)

    !This subroutine returns flux-profile relatioships based off
    !of Lobocki (1993), which is derived from the MY-level 2 model.
    !Suselj and Sood (2010) applied the surface layer length scales
    !from Nakanishi (2001) to get this new relationship. These functions
    !are more agressive (larger magnitude) than most formulations. They
    !showed improvement over water, but untested over land.

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL, PARAMETER  :: Rfc=0.19, Ric=0.183, PHIT=0.8

       IF (zL .gt. 0.) THEN  !STABLE

          psi_m = -(zL/Rfc + 1.1223*EXP(1.-1.6666/zL))
          !psi_h = -zL*Ric/((Rfc**2.)*PHIT) + 8.209*(zL**1.1091)
          !THEIR EQ FOR PSI_H CRASHES THE MODEL AND DOES NOT MATCH
          !THEIR FIG 1. THIS EQ (BELOW) MATCHES THEIR FIG 1 BETTER:
          psi_h = -(zL*Ric/((Rfc**2.)*5.) + 7.09*(zL**1.1091))
 
       ELSE                 !UNSTABLE

          psi_m = 0.9904*LOG(1. - 14.264*zL)
          psi_h = 1.0103*LOG(1. - 16.3066*zL)

       ENDIF
                   
       return

    END SUBROUTINE PSI_Suselj_Sood_2010
!--------------------------------------------------------------------
    SUBROUTINE PSI_CB2005(psim1,psih1,zL,z0L)

    ! This subroutine returns the stability functions based off
    ! of Cheng and Brutseart (2005, BLM), for use in stable conditions only.
    ! The returned values are the combination of psi((za+zo)/L) - psi(z0/L)

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL,z0L
       REAL, INTENT(OUT) :: psim1,psih1

       psim1 = -6.1*LOG(zL  + (1.+ zL **2.5)**0.4) &
               -6.1*LOG(z0L + (1.+ z0L**2.5)**0.4)
       psih1 = -5.5*LOG(zL  + (1.+ zL **1.1)**0.90909090909) &
               -5.5*LOG(z0L + (1.+ z0L**1.1)**0.90909090909)

       return

    END SUBROUTINE PSI_CB2005
!--------------------------------------------------------------------
    SUBROUTINE Li_etal_2010(zL, Rib, zaz0, z0zt)

    !This subroutine returns a more robust z/L that best matches
    !the z/L from Hogstrom (1996) for unstable conditions and Beljaars
    !and Holtslag (1991) for stable conditions.

       IMPLICIT NONE
       REAL, INTENT(OUT)  :: zL
       REAL, INTENT(IN) :: Rib, zaz0, z0zt
       REAL :: alfa, beta, zaz02, z0zt2
       REAL, PARAMETER  :: au11=0.045, bu11=0.003, bu12=0.0059, &
                          &bu21=-0.0828, bu22=0.8845, bu31=0.1739, &
                          &bu32=-0.9213, bu33=-0.1057
       REAL, PARAMETER  :: aw11=0.5738, aw12=-0.4399, aw21=-4.901,&
                          &aw22=52.50, bw11=-0.0539, bw12=1.540, &
                          &bw21=-0.669, bw22=-3.282
       REAL, PARAMETER  :: as11=0.7529, as21=14.94, bs11=0.1569,&
                          &bs21=-0.3091, bs22=-1.303
          
       !set limits according to Li et al (2010), p 157.
       zaz02=zaz0
       IF (zaz0 .lt. 100.0) zaz02=100.
       IF (zaz0 .gt. 100000.0) zaz02=100000.

       !set more limits according to Li et al (2010)
       z0zt2=z0zt
       IF (z0zt .lt. 0.5) z0zt2=0.5
       IF (z0zt .gt. 100.0) z0zt2=100.

       alfa = LOG(zaz02)
       beta = LOG(z0zt2)

       IF (Rib .le. 0.0) THEN
          zL = au11*alfa*Rib**2 + (                   &
               &  (bu11*beta + bu12)*alfa**2 +        &
               &  (bu21*beta + bu22)*alfa    +        &
               &  (bu31*beta**2 + bu32*beta + bu33))*Rib
          !if(zL .LT. -15 .OR. zl .GT. 0.)print*,"VIOLATION Rib<0:",zL
          zL = MAX(zL,-15.) !LIMITS SET ACCORDING TO Li et al (2010)
          zL = MIN(zL,0.)   !Figure 1.
       ELSEIF (Rib .gt. 0.0 .AND. Rib .le. 0.2) THEN
          zL = ((aw11*beta + aw12)*alfa +             &
             &  (aw21*beta + aw22))*Rib**2 +          &
             & ((bw11*beta + bw12)*alfa +             &
             &  (bw21*beta + bw22))*Rib
          !if(zL .LT. 0 .OR. zl .GT. 4)print*,"VIOLATION 0<Rib<0.2:",zL
          zL = MIN(zL,4.) !LIMITS APPROX SET ACCORDING TO Li et al (2010)
          zL = MAX(zL,0.) !THEIR FIGURE 1B.
       ELSE
          zL = (as11*alfa + as21)*Rib + bs11*alfa +   &
             &  bs21*beta + bs22
          !if(zL .LE. 1 .OR. zl .GT. 23)print*,"VIOLATION Rib>0.2:",zL
          zL = MIN(zL,20.) !LIMITS ACCORDING TO Li et al (2010), THIER
                           !FIGUE 1C.
          zL = MAX(zL,1.)
       ENDIF

       return

    END SUBROUTINE Li_etal_2010
!-------------------------------------------------------------------
      REAL function zolri(ri,za,z0,zt,zol1)

      ! This iterative algorithm was taken from the revised surface layer 
      ! scheme in WRF-ARW, written by Pedro Jimenez and Jimy Dudhia and 
      ! summarized in Jimenez et al. (2012, MWR). This function was adapted
      ! to input the thermal roughness length, zt, (as well as z0) because
      ! zt is necessary input for the Dyer-Hicks functions used in MYNN.

      IMPLICIT NONE
      REAL, INTENT(IN) :: ri,za,z0,zt,zol1
      REAL :: x1,x2,fx1,fx2
      INTEGER :: n

      if (ri.lt.0.)then
         x1=zol1 - 0.02  !-5.
         x2=0.
      else
         x1=0.
         x2=zol1 + 0.02 !5.
      endif

      n=0
      fx1=zolri2(x1,ri,za,z0,zt)
      fx2=zolri2(x2,ri,za,z0,zt)
      Do While (abs(x1 - x2) > 0.01 .and. n < 5)
        if(abs(fx2).lt.abs(fx1))then
          x1=x1-fx1/(fx2-fx1)*(x2-x1)
          fx1=zolri2(x1,ri,za,z0,zt)
          zolri=x1
        else
          x2=x2-fx2/(fx2-fx1)*(x2-x1)
          fx2=zolri2(x2,ri,za,z0,zt)
          zolri=x2
        endif
        n=n+1
        !print*," n=",n," x1=",x1," x2=",x2
      enddo

      if (n==5 .and. abs(x1 - x2) >= 0.01) then
         !print*,"iter FAIL, n=",n," Ri=",ri," z/L=",zolri
         !Tests results: fails convergence ~ 0.07 % of the time
         !set approximate values:
         if (ri.lt.0.)then
            zolri=ri*5.
         else
            zolri=ri*8.
         endif
      !else
      !   print*,"iter OK, n=",n," Ri=",ri," z/L=",zolri
      endif

      return
      end function
!-------------------------------------------------------------------
      REAL function zolri2(zol2,ri2,za,z0,zt)

      ! INPUT: =================================
      ! zol2 - estimated z/L
      ! ri2  - calculated bulk Richardson number
      ! za   - 1/2 depth of first model layer
      ! z0   - aerodynamic roughness length
      ! zt   - thermal roughness length
      ! OUTPUT: ================================
      ! zolri2 - updated estimate of z/L

      IMPLICIT NONE
      REAL, INTENT(IN) :: ri2,za,z0,zt
      REAL, INTENT(INOUT) :: zol2
      REAL :: zol20,zol3,psim1,psih1,psix2,psit2

      if(zol2*ri2 .lt. 0.)zol2=0.  ! limit zol2 - must be same sign as ri2

      zol20=zol2*z0/za ! z0/L
      zol3=zol2+zol20  ! (z+z0)/L

      if (ri2.lt.0) then
         !CALL PSI_DyerHicks(psim1,psih1,zol3,zt,z0,za) 
         psix2=log((za+z0)/z0)-(psim_unstable(zol3)-psim_unstable(zol20))
         psit2=log((za+zt)/zt)-(psih_unstable(zol3)-psih_unstable(zol20))
         !psix2=log((za+z0)/z0)-psim1
         !psit2=log((za+zt)/zt)-psih1
      else
         !CALL PSI_DyerHicks(psim1,psih1,zol2,zt,z0,za)
         !CALL PSI_CB2005(psim1,psih1,zol3,zol20)
         psix2=log((za+z0)/z0)-(psim_stable(zol3)-psim_stable(zol20))
         psit2=log((za+zt)/zt)-(psih_stable(zol3)-psih_stable(zol20))
         !psix2=log((za+z0)/z0)-psim1
         !psit2=log((za+zt)/zt)-psih1
      endif

      zolri2=zol2*psit2/psix2**2 - ri2

      return
      end function
!====================================================================
   SUBROUTINE psi_init

    INTEGER                   ::      N
    REAL                      ::      zolf

    DO N=0,1000
       ! stable function tables
       zolf = float(n)*0.01
       psim_stab(n)=psim_stable_full(zolf)
       psih_stab(n)=psih_stable_full(zolf)

       ! unstable function tables
       zolf = -float(n)*0.01
       psim_unstab(n)=psim_unstable_full(zolf)
       psih_unstab(n)=psih_unstable_full(zolf)
    ENDDO

   END SUBROUTINE psi_init
! ==================================================================
! ... integrated similarity functions ...
!                                                                                                                                                 
   REAL function psim_stable_full(zolf)
        REAL :: zolf   

        psim_stable_full=-6.1*log(zolf+(1+zolf**2.5)**(1./2.5))

        return
   end function

   REAL function psih_stable_full(zolf)
        REAL :: zolf

        psih_stable_full=-5.3*log(zolf+(1+zolf**1.1)**(1./1.1))

        return
   end function

   REAL function psim_unstable_full(zolf)
        REAL :: zolf,x,ym,psimc,psimk

        x=(1.-16.*zolf)**.25
        psimk=2*ALOG(0.5*(1+X))+ALOG(0.5*(1+X*X))-2.*ATAN(X)+2.*ATAN(1.)

        ym=(1.-10.*zolf)**0.33
        psimc=(3./2.)*log((ym**2.+ym+1.)/3.)-sqrt(3.)*ATAN((2.*ym+1)/sqrt(3.))+4.*ATAN(1.)/sqrt(3.)

        psim_unstable_full=(psimk+zolf**2*(psimc))/(1+zolf**2.)

        return
   end function

   REAL function psih_unstable_full(zolf)
        REAL :: zolf,y,yh,psihc,psihk

        y=(1.-16.*zolf)**.5
        psihk=2.*log((1+y)/2.)

        yh=(1.-34.*zolf)**0.33
        psihc=(3./2.)*log((yh**2.+yh+1.)/3.)-sqrt(3.)*ATAN((2.*yh+1)/sqrt(3.))+4.*ATAN(1.)/sqrt(3.)

        psih_unstable_full=(psihk+zolf**2*(psihc))/(1+zolf**2.)

        return
   end function
!=================================================================
! look-up table functions
!=================================================================
   REAL function psim_stable(zolf)
        integer :: nzol
        real    :: rzol,zolf

        nzol = int(zolf*100.)
        rzol = zolf*100. - nzol
        if(nzol+1 .le. 1000)then
           psim_stable = psim_stab(nzol) + rzol*(psim_stab(nzol+1)-psim_stab(nzol))
        else
           psim_stable = psim_stable_full(zolf)
        endif

      return
   end function

   REAL function psih_stable(zolf)
        integer :: nzol
        real    :: rzol,zolf

        nzol = int(zolf*100.)
        rzol = zolf*100. - nzol
        if(nzol+1 .le. 1000)then
           psih_stable = psih_stab(nzol) + rzol*(psih_stab(nzol+1)-psih_stab(nzol))
        else
           psih_stable = psih_stable_full(zolf)
        endif

      return
   end function

   REAL function psim_unstable(zolf)
        integer :: nzol
        real    :: rzol,zolf

        nzol = int(-zolf*100.)
        rzol = -zolf*100. - nzol
        if(nzol+1 .le. 1000)then
           psim_unstable = psim_unstab(nzol) + rzol*(psim_unstab(nzol+1)-psim_unstab(nzol))
        else
           psim_unstable = psim_unstable_full(zolf)
        endif

      return
   end function

   REAL function psih_unstable(zolf)
        integer :: nzol
        real    :: rzol,zolf

        nzol = int(-zolf*100.)
        rzol = -zolf*100. - nzol
        if(nzol+1 .le. 1000)then
           psih_unstable = psih_unstab(nzol) + rzol*(psih_unstab(nzol+1)-psih_unstab(nzol))
        else
           psih_unstable = psih_unstable_full(zolf)
        endif

      return
   end function
!========================================================================

END MODULE module_sf_mynn

