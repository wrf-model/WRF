! The National Taiwan University (NTU) scheme is a multi-moment bulk scheme (two moments for the liquid-phase and 3 moments 
! for the ice-phase hydrometeors) with consideration for ice crystal shape and density variations.  This scheme is developed 
! by Dr. Tzu-Chin Tsai and Prof. Jen-Ping Chen at the National Taiwan University. The NTU multi-moment bulk microphysics scheme 
! default version implemented into the WRF model by Tzu-Chin Tsai at NTU (d97229002@ntu.edu.tw; tzuchin12@gmail.com)
! Realsed on 2021/1/6
!-------------------------------------------------------------------------------------------------------------------------------
! References :
! 1. Chen, J.-P. and S.-T. Liu, 2004: Physically based two-moment bulkwater parametrization for warm-cloud microphysics.
!       Quart. J. Roy. Meteor. Soc., 130, 51-78.
! 2. Cheng, C.-T., W.-C. Wang, and J.-P. Chen, 2007: A modeling study of aerosol impacts on cloud microphysics and radiative 
!       properties. Quart. J. Roy. Meteor. Soc., 133, 283-297.
! 3. Cheng, C.-T., W.-C. Wang, and J.-P. Chen, 2010: Simulation of the effects of increasing cloud condensation nuclei on 
!       mixed-phase clouds and precipitation of a front system. Atmos. Res., 96, 461-476.
! 4. Chen, J.-P. and T.-C. Tsai, 2016: Triple-moment modal parameterization for the adaptive growth habit of pristine ice 
!       crystals. J. Atmos. Sci., 73, 2105-2122.
! 5. Tsai, T.-C., and J.-P. Chen, 2020: Multi-moment ice bulk microphysics scheme with consideration for particle shape 
!       and apparent density. Part I: Methodology and idealized simulation. J. Atmos. Sci., 77, 1821-1850.
!
!--------------------------------------------------------------------------------------------------------------------------------
! The notation for terms involving two interacting categories are denoted by B(x,y)MPxy, where B is the prognostic variable
! MP represents the microphysical processes, and the subscripts x and y indicate the quantity B being transferred from the
! hydrometeor category x to y. If terms involve three interacting categories, then a three-letter subscript of xyz would
! follow the microphysical process with the destination of hydrometeor category z. Another notation denoted by QHomp is
! only used for the change rate of the hailstone mixing ratio, and o indicates whether the dry (denote d) or wet (denote w)
! growth mode occurs. Please referred to supplemental information related to the Tsai and Chen (2020) is available at the
! Journals Online website: https://doi.org/10.1175/JAS-D-19-0125.s1.
!
!=================================================================================================================================

      MODULE module_mp_ntu
      USE    module_wrf_error

      IMPLICIT NONE
      PUBLIC  :: MP_NTU
      PRIVATE :: GAMMA,GAMLN,GAMIN,GAMMP,GSER,CFG,GUESS_RC,YEQU,DYEQU, &
                 PDF,DPDF,DLNX,POLYSVP

      INTEGER, PRIVATE, PARAMETER :: ID_NH42SO4 = 1,                   &! 1. (NH4)2SO4
                                     ID_DUST = 0,                      &! 2. DUST
                                     ID_IN = 2                          ! 3. IN
      INTEGER, PRIVATE, PARAMETER :: INSPEC = 2                         ! 1. SOOT; 2. SD; 3. AD; 4. BT
      INTEGER, PRIVATE, PARAMETER :: ICE_SHAPE = 1                      ! 0. SPHERICAL; 1. ASPHERICAL; PRISTINE ICE (HABIT)
      INTEGER, PRIVATE, PARAMETER :: AGG_SHAPE = 1                      ! 0. SPHERICAL; 1. CL94; ASPHERICAL AGGREGATES
      INTEGER, PRIVATE, PARAMETER :: ICE_RHOI = 1                       ! 0. FIXED; 1. PREDICT; 2. FIXED; PRISTINE ICE DENSITY
      INTEGER, PRIVATE, PARAMETER :: ICE_RHOS = 1                       ! 0. FIXED; 1. PREDICT; 2. MAPPING; AGGREGATES DENSITY
      INTEGER, PRIVATE, PARAMETER :: ICE_RHOG = 1                       ! 0. FIXED; 1. CL1993; 2. RH1985; PREDICT GRAUPEL DENSITY
      INTEGER, PRIVATE, PARAMETER :: ICE_VENT = 2                       ! 0. OFF; 1. HP76; 2. CL94; 3. JW99; VENTILATION EFFECT
      INTEGER, PRIVATE, PARAMETER :: HAIL_VENT = 1                      ! 0. DEFAULT; 1. CHENG ET AL. (2014); VENTILATION EFFECT
      INTEGER, PRIVATE, PARAMETER :: HWET_MODE = 1                      ! 0. OFF; 1. ON; HAILSTONE WET GROWTH
      INTEGER, PRIVATE, PARAMETER :: LIQ_VTC = 1                        ! 0. RAMS/FIXED; 1.,2. CL04; CLOUD DROPS/FLUX FALL SPEED
      INTEGER, PRIVATE, PARAMETER :: LIQ_VTR = 1                        ! 0. RAMS/FIXED; 1.,2. CL04; RAIN DROPS/FLUX FALL SPEED
      INTEGER, PRIVATE, PARAMETER :: ICE_VTI = 1                        ! 0. MORR/FIXED; 1. MH05; PRISTINE ICE FALL SPEED
      INTEGER, PRIVATE, PARAMETER :: ICE_VTS = 1                        ! 0. RAMS/FIXED; 1. MH05/EM16 SYNOPTIC; 
!                                                                         2. SH09 CRYSTAL-FACE; AGGREGATES FALL SPEED
      INTEGER, PRIVATE, PARAMETER :: ICE_VTG = 1                        ! 0. RAMS/FIXED; 1. MH05; GRAUPEL FALL SPEED
      INTEGER, PRIVATE, PARAMETER :: ICE_VTH = 1                        ! 0. RAMS/FIXED; 1. MH05; HAIL FALL SPEED
      INTEGER, PRIVATE, PARAMETER :: AFAC_3M = 1                        ! 0. FIXED; 1.,2. VARIABLE AFAC PARAMETER
      INTEGER, PRIVATE, PARAMETER :: AFAR_3M = 1                        ! 0. FIXED; 1.,2. VARIABLE AFAR PARAMETER
      INTEGER, PRIVATE, PARAMETER :: AFAI_3M = 1                        ! 0. FIXED; 1.,2. VARIABLE AFAI PARAMETER
      INTEGER, PRIVATE, PARAMETER :: AFAS_3M = 1                        ! 0. FIXED; 1.,2. VARIABLE AFAS PARAMETER
      INTEGER, PRIVATE, PARAMETER :: AFAG_3M = 1                        ! 0. FIXED; 1.,2. VARIABLE AFAG PARAMETER
      INTEGER, PRIVATE, PARAMETER :: AFAH_3M = 1                        ! 0. FIXED; 1.,2. VARIABLE AFAH PARAMETER
      INTEGER, PRIVATE, PARAMETER :: SAT_ADJ = 0                        ! 0. OFF (SUPER-SAT); 1. WITH SATURATION ADJUSTMENT
      INTEGER, PRIVATE, PARAMETER :: NCCN = 3                           ! for aerosol modes
      INTEGER, PRIVATE, PARAMETER :: NAER = 2                           ! for aerosol categories of CCN and IN
      INTEGER, PRIVATE, PARAMETER :: MAER = 4, NAER1 = 4, NAER2 = 1
      INTEGER, PRIVATE, PARAMETER :: NAERT = NAER1+NAER2                ! 5
      INTEGER, PRIVATE, PARAMETER :: NTBXA = 25                         ! size of tables for the first guess of Rc
      INTEGER, PRIVATE, DIMENSION(NAER) :: IBAER
      INTEGER, PRIVATE, DIMENSION(NAER) :: NAERN(1:NAER)=(/NAER1,NAER2/)
      DOUBLE PRECISION, PRIVATE, SAVE, DIMENSION(NTBXA) :: TBLRC        ! table of curresponding R-cut-off
      DOUBLE PRECISION, PRIVATE, SAVE, DIMENSION(NTBXA,NAER) :: TBLXF
!----------------- FOR MICROPHYSICAL PROCESSES -------------------------
      REAL, PRIVATE, PARAMETER :: DTMIN = 0.01                          ! [sec] mininum dt for cond./evap. calculation
      REAL, PRIVATE, PARAMETER :: DT20S = 20.                           ! [sec] dt for coalscence and accretion calculation
      REAL, PRIVATE, PARAMETER :: PI = 3.1415926535897932384626434
      REAL, PRIVATE, PARAMETER :: SQRTPI = 9.189385332046727417803297E-1
      REAL, PRIVATE, PARAMETER :: SQRT2 = 1.4142135623730950488016887
      REAL, PRIVATE, PARAMETER :: THRD = 1./3.,     C4PI3 = 4.*PI/3.
      REAL, PRIVATE, PARAMETER :: CP = 1.00546E3,   TK0C = 2.7315E2
      REAL, PRIVATE, PARAMETER :: R = 2.87058E2,    RV = 4.61495E2
      REAL, PRIVATE, PARAMETER :: CPI = 2.093E3,    CPW = 4.218E3       ! SPECIFIC HEAT 
      REAL, PRIVATE, PARAMETER :: CMW = 1.8015E-2
      REAL, PRIVATE, PARAMETER :: RHOSU = 8.5E4/(2.8715E2*2.7315E2)
      REAL, PRIVATE, PARAMETER :: RHOW = 9.97E2,    RHOG1 = 4.E2        ! BULK DENSITY
      REAL, PRIVATE, PARAMETER :: RHOI0 = 9.1E2,    iRHOI0 = 1./RHOI0   ! SOLID ICE DENSITY
      REAL, PRIVATE, PARAMETER :: RHOS0 = 1.E2,     RHOI1 = 5.E2
      REAL, PRIVATE, PARAMETER :: RHOG0 = (0.078+0.184*6.-0.015*36.)*1000
      REAL, PRIVATE, PARAMETER :: RHOH = 9.E2,      iRHOH = 1./RHOH
      REAL, PRIVATE, PARAMETER :: RHOIMIN = 5.E1,   RHOIMAX = RHOI0
      REAL, PRIVATE, PARAMETER :: C4PI3W = 4.*PI*RHOW/3.
      REAL, PRIVATE, PARAMETER :: iAMI0 = 6./(PI*RHOI0)
      REAL, PRIVATE, PARAMETER :: G = 9.806,        iAPW = 4./(PI*RHOW)
      REAL, PRIVATE, PARAMETER :: AAW = PI/4.,      V2M3 = 6./PI
      REAL, PRIVATE, PARAMETER :: BMW = 3.,         AMW = PI*RHOW/6.    ! MASS-DIAMETER PARAMETERS
      REAL, PRIVATE, PARAMETER :: BMI0 = 3.,        AMI0 = PI*RHOI0/6.  !
      REAL, PRIVATE, PARAMETER :: BMS = 3.,         AMS0 = PI*RHOS0/6.  !
      REAL, PRIVATE, PARAMETER :: BMG = 3.,         AMG0 = PI*RHOG1/6.  !
      REAL, PRIVATE, PARAMETER :: BMH = 3.,         AMH = PI*RHOH/6.    !
      REAL, PRIVATE, PARAMETER :: iAMW = 1./AMW,    iAMH = 1./AMH       !
      REAL, PRIVATE, PARAMETER :: AIMM = 6.6E-1,    BIMM = 1.E2         ! BIGG 1953 PARAMETERS FOR IMMERSION FREEZING
      REAL, PRIVATE, PARAMETER :: AVSG = 8.6E-1,    BVSG = 2.8E-1       ! VENTILATION COEFFICIENTS
      REAL, PRIVATE, PARAMETER :: AVRH = 7.8E-1,    BVRH = 3.08E-1      ! 
      REAL, PRIVATE, PARAMETER :: AVIS = 1.,        BVIS = 1.4E-1       !
      REAL, PRIVATE, PARAMETER :: VENC1 = 3.09E-2,  VENC2 = 1.447E-1    ! VENTILATION COEFFICIENTS
      REAL, PRIVATE, PARAMETER :: VENP1 = 1.05E-2,  VENP2 = 2.28E-2     ! FOR PRISITNE ICE CRYSTAL 
      REAL, PRIVATE, PARAMETER :: VENH1 = 0.22385,  VENH2 = 0.00101     ! FOR HAILSTONE (Cheng et al., 2014)
      REAL, PRIVATE, PARAMETER :: NSMALL = 1.E-2,   NSMAL1 = 1.E2       ! MOMENTS LIMITS
      REAL, PRIVATE, PARAMETER :: RLIMIT = 1.E-32,  SLIMIT = 1.E-2      !
      REAL, PRIVATE, PARAMETER :: QSMAL1 = 1.E-9,   QSMALL = 1.E-14     !
      REAL, PRIVATE, PARAMETER :: QLIMIT = 1.E-6,   RSMALL = 1.E-20     !
      REAL, PRIVATE, PARAMETER :: ASMALL = 1.E-12,  ISMALL = 1.E-17     !
      REAL, PRIVATE, PARAMETER :: BOLTZ = 1.38E-23, MLIMIT = 1.E-2      !
      REAL, PRIVATE, PARAMETER :: VTZ0 = 5.83,      VTC0 = 0.6          ! SURFACE ROUGHNESS PARAMETERS
      REAL, PRIVATE, PARAMETER :: VTC1 = 0.151931,  VTC2 = VTZ0**2./4.  ! VTC1=4./(VTZ0**2.*VTC0**5.E-1)
      REAL, PRIVATE, PARAMETER :: VTA0 = 1.7E-3,    VTB0 = 0.           ! VTB0 = 0.8
      REAL, PRIVATE, PARAMETER :: AVC0 = 3.E7,      BVC0 = 2.           ! Morrison scheme
      REAL, PRIVATE, PARAMETER :: AVR0 = 841.997,   BVR0 = 0.8          ! Morrison scheme
      REAL, PRIVATE, PARAMETER :: AVI0 = 700.,      BVI0 = 1.           ! Morrison scheme
      REAL, PRIVATE, PARAMETER :: AVS0 = 11.72,     BVS0 = 0.41         ! Locatelli and Hobbs (1974)
      REAL, PRIVATE, PARAMETER :: AVG0 = 19.3,      BVG0 = 0.37         ! Ferrier (1994)
      REAL, PRIVATE, PARAMETER :: AVH0 = 206.89,    BVH0 = 0.6384       ! Ferrier (1994)
      REAL, PRIVATE, PARAMETER :: VTCMAX = 1.,      VTSMAX = 10.        ! UPPER LIMIT FALL SPEED CHECK
      REAL, PRIVATE, PARAMETER :: VTIMAX = 10.,     VTRMAX = 15.        ! 
      REAL, PRIVATE, PARAMETER :: VTGMAX = 20.,     VTHMAX = 25.        !
      REAL, PRIVATE, PARAMETER :: AFAC0 = 0.,       AFAR0 = 0.          ! INITIAL SPECTRAL INDEX
      REAL, PRIVATE, PARAMETER :: AFAI0 = 3.,       AFAS0 = 0.          !
      REAL, PRIVATE, PARAMETER :: AFAG0 = 0.,       AFAH0 = 0.          !
      REAL, PRIVATE, PARAMETER :: SASMAX = 1.,      SASMIN = 1.E-3      ! ASPECT RATIO CHECK FOR AGGREGATES SHAPE
      REAL, PRIVATE, PARAMETER :: KCCMIN = 0.223,   KCCMAX = 0.999      ! MOMENT RATIO CHECK
      REAL, PRIVATE, PARAMETER :: KCRMIN = 0.223,   KCRMAX = 0.999      !
      REAL, PRIVATE, PARAMETER :: KCIMIN = 0.556,   KCIMAX = 0.999      ! KC = 0.999, AFAMAX = 2996
      REAL, PRIVATE, PARAMETER :: KCSMIN = 0.223,   KCSMAX = 0.999      ! KC = 0.995, AFAMAX = 596
      REAL, PRIVATE, PARAMETER :: KCGMIN = 0.223,   KCGMAX = 0.999      ! KC = 0.99,  AFAMAX = 296
      REAL, PRIVATE, PARAMETER :: KCHMIN = 0.223,   KCHMAX = 0.999      ! KC = 0.95,  AFAMAX = 56
      REAL, PRIVATE, PARAMETER :: AFAMAX = 3.E4,    AFAMIN = 0.         ! SPECTRAL INDEX CHECK
      REAL, PRIVATE, PARAMETER :: AFU = 3.125E-1,   BFU = 1.0552E-3     ! CL04 FALL SPEED ADJUSTMENT
      REAL, PRIVATE, PARAMETER :: CFU = -2.4023
      REAL, PRIVATE, PARAMETER :: DCMIN = 1.E-7,    DCMAX = 1.E-4       ! DIAMETER CHECK
      REAL, PRIVATE, PARAMETER :: DIMIN = 1.E-6,    DIMAX = 5.E-3       !
      REAL, PRIVATE, PARAMETER :: DRMIN = 3.E-5,    DRMAX = 6.E-3       !
      REAL, PRIVATE, PARAMETER :: DSMIN = 2.E-5,    DSMAX = 1.E-2       !
      REAL, PRIVATE, PARAMETER :: DGMIN = 5.E-5,    DGMAX = 2.E-2       !
      REAL, PRIVATE, PARAMETER :: DHMIN = 1.E-3,    DHMAX = 4.E-2       !
      REAL, PRIVATE, PARAMETER :: RCMIN = DCMIN/2., RCMAX = DCMAX/2.    ! RADIUS CHECK
      REAL, PRIVATE, PARAMETER :: RRMIN = DRMIN/2., RRMAX = DRMAX/2.    !
      REAL, PRIVATE, PARAMETER :: DI0 = 6.E-6,      DCR = 100.E-6       ! ISOMETRIC LIMIT FOR PI; THRESHOLD FOR RAINDROPS
      REAL, PRIVATE, PARAMETER :: MI0 = AMI0*DI0**BMI0                  ! INITIAL PI MASS
      REAL, PRIVATE, PARAMETER :: SIG1 = -6.2685,   SIG2 = -2.7312E-1   ! CL04 CLOUD DROPLETS SPECTRUM
      REAL, PRIVATE, PARAMETER :: SIG3 = 2.2606E-1, MNR1 = -2.2920      !
      REAL, PRIVATE, PARAMETER :: MNR2 = -3.5158E-1, MNR3 = 3.4708E-1   !
      REAL, PRIVATE, PARAMETER :: EFC1 = -1.2560,    EFC2 = -1.7904E-02 ! CL04 CLOUD DROPLETS EFFECTIVE RADIUS
      REAL, PRIVATE, PARAMETER :: EFC3 = 8.5536E-01, EFR1 = -9.9216E-02 ! CL04 RAINDROPS EFFECTIVE RADIUS
      REAL, PRIVATE, PARAMETER :: EFR2 = 2.9490E-02, EFR3 = 9.9238E-01  !
      REAL, DIMENSION(0:120) :: ITBLE                                   ! DEPOSITION GROWTH COEFFICIENTS
      DATA ITBLE /1.000000,0.979490,0.959401,0.939723,0.920450,        &
                  0.899498,0.879023,0.857038,0.833681,0.810961,        &
                  0.783430,0.755092,0.703072,0.537032,0.467735,        &
                  0.524807,0.630957,0.812831,1.096478,1.479108,        &
                  1.905461,2.089296,2.290868,2.398833,2.454709,        &
                  2.426610,2.371374,2.290868,2.137962,1.995262,        &
                  1.862087,1.737801,1.621810,1.513561,1.396368,        &
                  1.288250,1.188502,1.096478,1.000000,0.922571,        &
                  0.851138,0.785236,0.724436,0.668344,0.616595,        &
                  0.575440,0.537032,0.501187,0.467735,0.436516,        &
                  0.407380,0.380189,0.354813,0.331131,0.316228,        &
                  0.301995,0.291743,0.285102,0.281838,0.278612,        &
                  0.275423,0.278612,0.281838,0.285102,0.291743,        &
                  0.298538,0.309030,0.319890,0.331131,0.346737,        &
                  0.367282,0.393550,0.426580,0.457088,0.489779,        &
                  0.524807,0.562341,0.609537,0.660693,0.716143,        &
                  0.785236,0.860994,0.954993,1.047129,1.148154,        &
                  1.258925,1.380384,1.496236,1.603245,1.698244,        &
                  1.778279,1.840772,1.883649,1.905461,1.905461,        &
                  1.883649,1.862087,1.840772,1.798871,1.737801,        &
                  1.698244,1.640590,1.584893,1.548817,1.513561,        &
                  1.475707,1.452112,1.428894,1.412538,1.393157,        &
                  1.377209,1.361445,1.348963,1.336596,1.327394,        &
                  1.318257,1.309182,1.303167,1.294196,1.288250,1.279381/
      REAL, DIMENSION(0:167) :: IECC                                    ! JW99 RIMING COLLECTION EFFICIENCY COLUMNS
      DATA IECC /0.00,0.03,0.29,0.25,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.10,0.42,0.50,0.47,0.21,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.15,0.49,0.56,0.55,0.46,0.10,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.20,0.52,0.61,0.62,0.59,0.49,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.25,0.62,0.72,0.75,0.74,    &
                 0.71,0.68,0.57,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.30,0.70,0.80,0.84,    &
                 0.85,0.85,0.84,0.83,0.81,0.77,0.69,0.10,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.35,0.75,0.85,    &
                 0.89,0.91,0.92,0.92,0.91,0.90,0.89,0.87,0.85,0.82,    &
                 0.71,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.40,0.80,    &
                 0.89,0.93,0.94,0.95,0.95,0.95,0.95,0.95,0.95,0.94,    &
                 0.93,0.92,0.91,0.88,0.80,0.15,0.00,0.00/
      REAL, DIMENSION(0:167) :: IEPC                                    ! JW99 RIMING COLLECTION EFFICIENCY PLATES
      DATA IEPC /0.00,0.00,0.13,0.41,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.25,0.54,0.56,0.39,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.08,0.57,0.75,0.83,0.85,0.86,0.84,    &
                 0.78,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.10,0.65,0.80,0.87,0.90,0.91,    &
                 0.91,0.91,0.88,0.78,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.12,0.67,0.81,0.88,0.91,    &
                 0.92,0.93,0.93,0.92,0.91,0.89,0.81,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.15,0.68,0.82,0.89,    &
                 0.91,0.93,0.94,0.94,0.95,0.95,0.95,0.94,0.91,0.83,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.18,0.69,0.82,    &
                 0.89,0.92,0.94,0.95,0.95,0.96,0.96,0.96,0.96,0.95,    &
                 0.94,0.91,0.80,0.00,0.00,0.00,0.00,0.00,0.22,0.70,    &
                 0.83,0.90,0.93,0.95,0.96,0.97,0.98,0.98,0.98,0.98,    &
                 0.97,0.96,0.94,0.92,0.84,0.00,0.00,0.00/
      REAL, DIMENSION(0:44) :: AMS1                                     ! EM16, MASS-DIAMETER PARAMETERS SYNOPTIC CLOUDS
      DATA AMS1 /2.728E+01,1.258E+00,6.452E-02,6.259E-03,8.658E-04,    &
                 4.657E+01,1.193E+00,3.790E-02,2.688E-03,3.101E-04,    &
                 2.260E+01,1.000E+00,5.059E-02,4.889E-03,6.826E-04,    &
                 6.176E+00,6.002E-01,5.889E-02,8.892E-03,1.647E-03,    &
                 2.982E+00,1.795E-01,1.261E-02,1.647E-03,3.128E-04,    &
                 2.783E+00,2.179E-01,1.901E-02,2.825E-03,5.699E-04,    &
                 1.630E+00,3.267E-01,6.223E-02,1.493E-02,3.902E-03,    &
                 1.145E+01,3.346E-02,1.178E-04,7.701E-05,2.463E-05,    &
                 6.606E+01,1.799E-03,1.178E-04,7.701E-05,2.463E-05/
      REAL, DIMENSION(0:44) :: BMS1                                     ! SYNOPTIC CLOUDS
      DATA BMS1 /2.792,2.455,2.085,1.748,1.411,2.846,2.449,2.015,1.618,&
                 1.221,2.773,2.429,2.053,1.710,1.367,2.642,2.371,2.073,&
                 1.802,1.530,2.556,2.254,1.923,1.621,1.320,2.549,2.276,&
                 1.977,1.704,1.431,2.495,2.322,2.133,1.960,1.787,2.686,&
                 2.064,1.382,1.382,1.382,2.863,1.732,1.382,1.382,1.382/
      REAL, DIMENSION(0:44) :: AAS1                                     ! EM16, AREA-DIAMETER PARAMETERS SYNOPTIC CLOUDS
      DATA AAS1 /1.782E+00,2.778E-01,4.794E-02,1.241E-02,4.069E-03,    &
                 2.571E+00,2.635E-01,3.180E-02,6.517E-03,1.879E-03,    &
                 1.910E+00,2.522E-01,3.759E-02,8.905E-03,2.787E-03,    &
                 6.238E-01,1.697E-01,4.686E-02,1.662E-02,6.761E-03,    &
                 5.604E-01,9.194E-02,1.726E-02,5.030E-03,1.927E-03,    &
                 2.802E-01,8.345E-02,2.602E-02,1.042E-02,4.854E-03,    &
                 1.269E-01,1.106E-01,8.453E-02,5.913E-02,3.763E-02,    &
                 3.569E-01,2.314E-02,1.664E-03,1.397E-03,8.521E-04,    &
                 6.339E-01,5.656E-03,1.664E-03,1.397E-03,8.521E-04/
      REAL, DIMENSION(0:44) :: BAS1                                     ! SYNOPTIC CLOUDS
      DATA BAS1 /2.133,1.938,1.725,1.531,1.337,2.170,1.932,1.671,1.432,&
                 1.194,2.140,1.927,1.693,1.480,1.267,2.027,1.882,1.722,&
                 1.576,1.431,2.011,1.821,1.612,1.422,1.232,1.941,1.810,&
                 1.666,1.534,1.403,1.861,1.842,1.821,1.801,1.782,1.960,&
                 1.669,1.350,1.350,1.350,2.018,1.509,1.350,1.350,1.350/
!----------------- FOR AEROSOL PARAMETER -------------------------------
      REAL, SAVE :: DNC0,DNC1,DNC2,DNC3
      REAL, SAVE, DIMENSION(NAER) :: SENS,ASH,CMAS,DNAS,AVAN,BETA1,RXMIN
      REAL, SAVE, DIMENSION(NCCN,NAER) :: ZCCN,CNMOD,CNSTD,WMAS,RFACT

      CONTAINS
!======================================================================
      SUBROUTINE NTU_INIT(PHB,PH,P,PB,ALT,QV,QDCN,QTCN,QCCN,QRCN,QNIN, &
                 XLAND,CCNTY,RESTART,IDS,IDE,JDS,JDE,KDS,KDE,IMS,IME,  &
                 JMS,JME,KMS,KME,ITS,ITE,JTS,JTE,KTS,KTE)
!======================================================================
      IMPLICIT NONE
      LOGICAL, INTENT(IN) :: RESTART
      INTEGER, INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,IMS,IME,JMS,JME,  &
                             KMS,KME,ITS,ITE,JTS,JTE,KTS,KTE,CCNTY
      REAL, INTENT(IN), DIMENSION(IMS:IME,JMS:JME) :: XLAND             ! 1:land ; 2:ocean
      REAL, INTENT(IN), DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: PHB,PH,P,&
                        PB,ALT,QV
      REAL, INTENT(INOUT), DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: QDCN, &
                           QTCN,QCCN,QRCN,QNIN
      REAL, DIMENSION(ITS:ITE,KTS:KTE,JTS:JTE) :: DZ,RHO,DZ8W,P_PHY
      REAL, DIMENSION(ITS:ITE,KTS:KTE,JTS:JTE,NAERT) :: QAERO
      INTEGER :: I,J,K,NK,ITF,JTF

      ITF = MIN(ITE,IDE-1)
      JTF = MIN(JTE,JDE-1)
      CALL AERO_CONST(CCNTY)
      DO J = JTS,JTF
         DO K = KTS,KTE
            DO I = ITS,ITF
               DZ(I,K,J) = (PHB(I,K,J)+PH(I,K,J))/G
            ENDDO
         ENDDO
      ENDDO
      DO J = JTS,JTF
         DO K = KTS,KTE-1
            DO I = ITS,ITF
               NK = KTE-K
               DZ8W(I,K,J) = DZ(I,NK+1,J)-DZ(I,NK,J)
               RHO(I,K,J) = 1./ALT(I,NK,J)*(1.+QV(I,NK,J))
               P_PHY(I,K,J) = P(I,NK,J)+PB(I,NK,J)
               QAERO(I,K,J,1) = QDCN(I,NK,J)
               QAERO(I,K,J,2) = QTCN(I,NK,J)
               QAERO(I,K,J,3) = QCCN(I,NK,J)
               QAERO(I,K,J,4) = QRCN(I,NK,J)
               QAERO(I,K,J,5) = QNIN(I,NK,J)
            ENDDO
         ENDDO
      ENDDO
      IF (.NOT.RESTART) THEN
         CALL INIT_AEROSOL(P_PHY,RHO,DZ8W,XLAND,QAERO,IDS,IDE,JDS,JDE, &
              KDS,KDE,IMS,IME,JMS,JME,KMS,KME,ITS,ITE,JTS,JTE,KTS,KTE)
      ENDIF
      DO J = JTS,JTF
         DO K = KTS,KTE-1
            DO I = ITS,ITF
               NK = KTE-K
               QDCN(I,NK,J) = QAERO(I,K,J,1)
               QTCN(I,NK,J) = QAERO(I,K,J,2)
               QCCN(I,NK,J) = QAERO(I,K,J,3)
               QRCN(I,NK,J) = QAERO(I,K,J,4)
               QNIN(I,NK,J) = QAERO(I,K,J,5)
            ENDDO
         ENDDO
      ENDDO

      END SUBROUTINE NTU_INIT
!======================================================================
!
!======================================================================
      SUBROUTINE FIND_RC0(XAFRC,CMODE,CSTDV,WMAS,RC,TBLXA,TBLRC)    
!======================================================================
      IMPLICIT NONE
      INTEGER, PARAMETER :: ITERMAX = 50
      INTEGER :: IM,I
!----------------- AEROSOL TRI-MODAL SIZE DISTRIBUTION ----------------
      REAL :: RC                                                        ! cut-off radius [m]
      REAL, DIMENSION(NCCN) :: WMAS,                                   &! mass weighting for each mode
                               CMODE,                                  &! modal value (ln R) of each mode [m]
                               CSTDV                                    ! standard distribution (in logarithm) of the mode
      REAL, PARAMETER :: RC_MAX = 99., RC_MIN = 1.E-9, XTOR = 1.E-5
      DOUBLE PRECISION, DIMENSION(NCCN) :: DWMAS,DMODE,DSTDV
!----------------- GET THE FIRST GUESS OF RC ---------------------------
      DOUBLE PRECISION :: DRC,UL,X1MAFRC,Y0,Y,DY,DX,DERF,DLOG,DEXP,    &
                          DSQRT,XAFRC                                   ! ratio of dry/total aerosol mass
      DOUBLE PRECISION, DIMENSION(NTBXA) :: TBLRC,TBLXA                 ! table of dry aerosol mass fraction

      DO IM = 1,NCCN
         DWMAS(IM) = DBLE(WMAS(IM))
         DSTDV(IM) = DBLE(CSTDV(IM))
         DMODE(IM) = DLOG(DBLE(CMODE(IM)))+DBLE(3.*CSTDV(IM)*CSTDV(IM)) ! for mass, the 3rd momentum DBLE(CMODE (IM))+
      ENDDO
      UL = (1.-DERF(5.D+0/DSQRT(2.D+0)))*0.5                            ! after 5 sigma, x1mfrc will < 2.86E-7
      X1MAFRC = 1.D+0-XAFRC
      IF (XAFRC.LT.1.D-10) THEN
         RC = RC_MIN
         RETURN
      ENDIF
      IF (X1MAFRC.LT.UL) THEN
         RC = RC_MAX
         RETURN
      ENDIF
      DRC = GUESS_RC(XAFRC,TBLXA,TBLRC)                                 ! cut-off radius [m]
      Y0  = YEQU(DRC,XAFRC,DWMAS,DMODE,DSTDV)
      DO I = 1,ITERMAX
         Y  = Y0
         DY = DYEQU(DRC,DWMAS,DMODE,DSTDV)
         IF (DY.LE.1.D-50) THEN
            PRINT *,'IN FIND_RC0.F DY IS',DY
            PRINT *,I,DX,Y,DY,DEXP(DRC),DRC,XAFRC,X1MAFRC,DMODE,DSTDV, &
                    CMODE,CSTDV
            STOP
         ENDIF
         DX = -Y/DY
         IF (DABS(DX).GT.3.D+0) DX = DX/(DABS(DX/0.4))
         IF (DABS(DX).LT.XTOR) THEN
            RC = REAL(DEXP(DRC+DX))
            RC = MIN(RC_MAX,MAX(RC,RC_MIN))
            RETURN
         ENDIF
         Y0 = YEQU(DRC+DX,XAFRC,DWMAS,DMODE,DSTDV)
         IF (Y*Y0.LT.0.) THEN
            DX = DX*5.D-1
            Y0 = YEQU(DRC+DX,XAFRC,DWMAS,DMODE,DSTDV)
         ENDIF
         DRC = DRC+DX
      ENDDO
      RC = REAL(DEXP(DRC))

      END SUBROUTINE FIND_RC0
!======================================================================
!
!======================================================================
      FUNCTION GUESS_RC(XAFRC,TBLXA,TBLRC)
!======================================================================
      DOUBLE PRECISION :: GUESS_RC,XAFRC,TBLXA(NTBXA),TBLRC(NTBXA)
      INTEGER :: IBNG,IEND,I

      IF (XAFRC.GE.TBLXA(NTBXA)) THEN
         GUESS_RC = TBLRC(NTBXA)
         RETURN
      ELSEIF (XAFRC.LT.TBLXA(1)) THEN
         GUESS_RC = TBLRC(1)
         RETURN
      ENDIF
      IF (XAFRC.LT.TBLXA(NTBXA/4)) THEN
         IBNG = 1
         IEND = NTBXA/4-1
      ELSEIF (XAFRC.LT.TBLXA(NTBXA*2/4)) THEN
         IBNG = NTBXA/4
         IEND = NTBXA*2/4-1
      ELSEIF (XAFRC.LT.TBLXA(NTBXA*3/4)) THEN
         IBNG = NTBXA*2/4
         IEND = NTBXA*3/4-1
      ELSE
         IBNG = NTBXA*3/4
         IEND = NTBXA-1
      ENDIF
      DO I = IEND,IBNG,-1
         IF (XAFRC.GE.TBLXA(I)) GOTO 111
      ENDDO
111   GUESS_RC = TBLRC(I)

      END FUNCTION GUESS_RC
!======================================================================
!
!======================================================================
      FUNCTION  YEQU(X,XAFRC,WEGHT,XLBAR,SIGMA)
!======================================================================
      INTEGER :: I
      DOUBLE PRECISION :: YEQU,DX,XAFRC,X,DSQRT2,DSQRT
      DOUBLE PRECISION, DIMENSION(NCCN) :: WEGHT,XLBAR,SIGMA

      DSQRT2 = DSQRT(2.D+0)
      YEQU   = 0.D+0
      DO I = 1,NCCN
         DX = (X-XLBAR(I))/SIGMA(I)/DSQRT2
         YEQU = YEQU+WEGHT(I)*PDF(DX)
      ENDDO
      YEQU = YEQU-XAFRC

      END FUNCTION YEQU
!----------------------------------------------------------------------
      FUNCTION DYEQU(X,WEGHT,XLBAR,SIGMA)
!======================================================================
      INTEGER :: I
      DOUBLE PRECISION :: DYEQU,X,DX,DSQRT2,DSQRT
      DOUBLE PRECISION, DIMENSION(NCCN) :: WEGHT,XLBAR,SIGMA 

      DSQRT2 = DSQRT(2.D+0)
      DYEQU  = 0.D+0
      DO I = 1,NCCN
         DX = (X-XLBAR(I))/SIGMA(I)/DSQRT2
         DYEQU = DYEQU+WEGHT(I)*DPDF(DX)/SIGMA(I)/DSQRT2
      ENDDO

      END FUNCTION DYEQU
!======================================================================
      FUNCTION  PDF(X)
      DOUBLE PRECISION :: PDF,X,DERF

      PDF = (1.D+0+DERF(X))*5.D-1

      END FUNCTION PDF
!----------------------------------------------------------------------
      FUNCTION DPDF(X)
      DOUBLE PRECISION :: DPDF,X,DPI,DACOS,DEXP,DSQRT

      DPI  = DACOS(-1.D+0)
      DPDF = DEXP(-X*X)/DSQRT(DPI)

      END FUNCTION DPDF
!======================================================================
!
!======================================================================
      REAL FUNCTION GAMMA(X)                                            ! IMPLEMETED FROM MORRISON SCHEME
!======================================================================
      IMPLICIT NONE
      INTEGER :: I,N
      LOGICAL :: PARITY
      REAL :: CONV,EPS,FACT,HALF,ONE,RES,SUM,TWELVE,TWO,X,XBIG,       &
              XDEN,XINF,XMININ,XNUM,Y,Y1,YSQ,Z,ZERO
      REAL, DIMENSION(7) :: C
      REAL, DIMENSION(8) :: P
      REAL, DIMENSION(8) :: Q
!----------------------------------------------------------------------
!  MATHEMATICAL CONSTANTS
!----------------------------------------------------------------------
      DATA ONE,HALF,TWELVE,TWO,ZERO/1.0E0,0.5E0,12.0E0,2.0E0,0.0E0/
!----------------------------------------------------------------------
!  MACHINE DEPENDENT PARAMETERS
!----------------------------------------------------------------------
      DATA XBIG,XMININ,EPS/35.040E0,1.18E-38,1.19E-7/,XINF/3.4E38/
!----------------------------------------------------------------------
!  NUMERATOR AND DENOMINATOR COEFFICIENTS FOR RATIONAL MINIMAX
!     APPROXIMATION OVER (1,2).
!----------------------------------------------------------------------
      DATA P /-1.71618513886549492533811E0,2.47656508055759199108314E1,&
              -3.79804256470945635097577E2,6.29331155312818442661052E2,&
              8.66966202790413211295064E2,-3.14512729688483675254357E4,&
              -3.61444134186911729807069E4,6.64561438202405440627855E4/
      DATA Q /-3.08402300119738975254353E1,3.15350626979604161529144E2,&
             -1.01515636749021914166146E3,-3.10777167157231109440444E3,&
              2.25381184209801510330112E4,4.75584627752788110767815E3, &
             -1.34659959864969306392456E5,-1.15132259675553483497211E5/
!----------------------------------------------------------------------
!  COEFFICIENTS FOR MINIMAX APPROXIMATION OVER (12, INF).
!----------------------------------------------------------------------
      DATA C /-1.910444077728E-3,8.4171387781295E-4,                   &
             -5.952379913043012E-4,7.93650793500350248E-4,             &
             -2.777777777777681622553E-3,8.333333333333333331554247E-2,&
             5.7083835261E-3/
!----------------------------------------------------------------------
!  STATEMENT FUNCTIONS FOR CONVERSION BETWEEN INTEGER AND FLOAT
!----------------------------------------------------------------------
      CONV(I) = REAL(I)
      PARITY = .FALSE.
      FACT = ONE
      N = 0
      Y = X
      IF (Y.LE.ZERO) THEN
!----------------------------------------------------------------------
!  ARGUMENT IS NEGATIVE
!----------------------------------------------------------------------
         Y   = -X
         Y1  = AINT(Y)
         RES = Y-Y1
         IF (RES.NE.ZERO) THEN
            IF (Y1.NE.AINT(Y1*HALF)*TWO) PARITY = .TRUE.
            FACT = -PI/SIN(PI*RES)
            Y = Y+ONE
         ELSE
            RES = XINF
            GOTO 900
         ENDIF
      ENDIF
!----------------------------------------------------------------------
!  ARGUMENT IS POSITIVE
!----------------------------------------------------------------------
      IF (Y.LT.EPS) THEN
!----------------------------------------------------------------------
!  ARGUMENT .LT. EPS
!----------------------------------------------------------------------
         IF (Y.GE.XMININ) THEN
            RES = ONE/Y
         ELSE
            RES = XINF
            GOTO 900
         ENDIF
      ELSEIF (Y.LT.TWELVE) THEN
         Y1 = Y
         IF (Y.LT.ONE) THEN
!----------------------------------------------------------------------
!  0.0 .LT. ARGUMENT .LT. 1.0
!----------------------------------------------------------------------
            Z = Y
            Y = Y+ONE
         ELSE
!----------------------------------------------------------------------
!  1.0 .LT. ARGUMENT .LT. 12.0, REDUCE ARGUMENT IF NECESSARY
!----------------------------------------------------------------------
            N = INT(Y)-1
            Y = Y-CONV(N)
            Z = Y-ONE
         ENDIF
!----------------------------------------------------------------------
!  EVALUATE APPROXIMATION FOR 1.0 .LT. ARGUMENT .LT. 2.0
!----------------------------------------------------------------------
         XNUM = ZERO
         XDEN = ONE
         DO I = 1,8
            XNUM = (XNUM+P(I))*Z
            XDEN = XDEN*Z+Q(I)
         END DO
         RES = XNUM/XDEN+ONE
         IF (Y1.LT.Y) THEN
!----------------------------------------------------------------------
!  ADJUST RESULT FOR CASE  0.0 .LT. ARGUMENT .LT. 1.0
!----------------------------------------------------------------------
            RES = RES/Y1
         ELSEIF (Y1.GT.Y) THEN
!----------------------------------------------------------------------
!  ADJUST RESULT FOR CASE  2.0 .LT. ARGUMENT .LT. 12.0
!----------------------------------------------------------------------
            DO I = 1,N
               RES = RES*Y
               Y = Y+ONE
            END DO
         ENDIF
      ELSE
!----------------------------------------------------------------------
!  EVALUATE FOR ARGUMENT .GE. 12.0,
!----------------------------------------------------------------------
         IF (Y.LE.XBIG) THEN
            YSQ = Y*Y
            SUM = C(7)
            DO I = 1,6
               SUM = SUM/YSQ+C(I)
            END DO
            SUM = SUM/Y-Y+SQRTPI
            SUM = SUM+(Y-HALF)*LOG(Y)
            RES = EXP(SUM)
         ELSE
            RES = XINF
            GOTO 900
         ENDIF
      ENDIF
!----------------------------------------------------------------------
!  FINAL ADJUSTMENTS AND RETURN
!----------------------------------------------------------------------
      IF (PARITY) RES = -RES
      IF (FACT.NE.ONE) RES = FACT/RES
  900 GAMMA = RES
      RETURN

      END FUNCTION GAMMA
!======================================================================

!======================================================================
      REAL FUNCTION POLYSVP(T,TYPE)                                    ! IMPLEMETED FROM MORRISON SCHEME
!======================================================================
!  COMPUTE SATURATION VAPOR PRESSURE POLYSVP RETURNED IN UNITS OF PA. T IS INPUT IN UNITS OF K.
!  TYPE REFERS TO SATURATION WITH RESPECT TO LIQUID (0) OR ICE (1)
!  REPLACE GOFF-GRATCH WITH FASTER FORMULATION FROM FLATAU ET AL. 1992, TABLE 4 (RIGHT-HAND COLUMN)
      IMPLICIT NONE
      REAL :: DUM,T,DT
      INTEGER :: TYPE
! ice
      REAL :: a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i 
      DATA a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i /6.11147274,0.503160820,&
           0.188439774E-1,0.420895665E-3,0.615021634E-5,0.602588177E-7,&
           0.385852041E-9,0.146898966E-11,0.252751365E-14/
! liquid
      REAL :: a0,a1,a2,a3,a4,a5,a6,a7,a8 
      DATA a0,a1,a2,a3,a4,a5,a6,a7,a8 /6.11239921,0.443987641,         &
           0.142986287E-1,0.264847430E-3,0.302950461E-5,0.206739458E-7,&
           0.640689451E-10,-0.952447341E-13,-0.976195544E-15/
      IF (TYPE.EQ.1) THEN                                               ! ICE
         DT = MAX(-80.,T-273.16)
         POLYSVP = a0i+DT*(a1i+DT*(a2i+DT*(a3i+DT*(a4i+DT*             &
                          (a5i+DT*(a6i+DT*(a7i+a8i*DT))))))) 
         POLYSVP = POLYSVP*100.
      ENDIF
      IF (TYPE.EQ.0) THEN                                               ! LIQUID
         DT = MAX(-80.,T-273.16)
         POLYSVP = a0+DT*(a1+DT*(a2+DT*(a3+DT*(a4+DT*(a5+DT*           &
                         (a6+DT*(a7+a8*DT)))))))
         POLYSVP = POLYSVP*100.
      ENDIF

      END FUNCTION POLYSVP
!======================================================================
!
!======================================================================
      REAL FUNCTION GAMLN(XX)                                           ! Referred to MY2 scheme
!======================================================================
!  Returns value of ln(GAMMA(XX)) for XX>0 (modified from "Numerical Recipes")
      IMPLICIT NONE
      REAL, INTENT(IN) :: XX
      INTEGER :: J
      DOUBLE PRECISION :: ser,stp,TMP,X,y,cof(6)
      SAVE cof,stp
      DATA cof,stp /76.18009172947146d0,-86.50532032941677d0,          &
                    24.01409824083091d0,-1.231739572450155d0,          &
                    .1208650973866179d-2,-.5395239384953d-5,           &
                    2.5066282746310005d0/

      X = DBLE(XX)
      y = X
      TMP = X+5.5D0
      TMP = (X+0.5D0)*LOG(TMP)-TMP
      ser = 1.000000000190015d0
      DO J = 1,6   !original
         y = y+1.D0
         ser = ser+cof(J)/y
      ENDDO
#if (DWORDSIZE == 8 && RWORDSIZE == 8)
      GAMLN = TMP+LOG(stp*ser/X)
#elif (DWORDSIZE == 8 && RWORDSIZE == 4)
      GAMLN = SNGL(TMP+LOG(stp*ser/X))
#else
!      This is a temporary hack assuming double precision is 8 bytes.
#endif

      END FUNCTION GAMLN
!======================================================================
!
!======================================================================
      REAL FUNCTION GAMIN(P,XMAX)
!======================================================================
! The incomplete gamma function below the limit of P = AFA+1 and XMAX = DI0*LAMDA
      REAL :: P,XMAX

      GAMIN = GAMMP(P,XMAX)*EXP(GAMLN(P))

      END FUNCTION GAMIN
!=======================================================================
      REAL FUNCTION GAMMP(A,X)
!=======================================================================
! The fraction of distribution below the limit of A = AFA+1 and X = Db*LAMDA
      IMPLICIT NONE
      REAL :: A,X,GAMMCF,GAMSER,GLN

      IF (X.LT.0..OR.A.LE.0.) &
         CALL wrf_error_fatal('warning : bad arguments in gammq')
      IF (X.LT.A+1.) THEN
         CALL GSER(GAMSER,A,X,GLN)
         GAMMP = GAMSER
      ELSE
         CALL CFG(GAMMCF,A,X,GLN)
         GAMMP = 1.-GAMMCF
      ENDIF
      RETURN

      END FUNCTION GAMMP
!======================================================================
      SUBROUTINE GSER(GAMSER,A,X,GLN)                                  ! Referred to MY2 scheme
!======================================================================
! USES GAMLN, Returns the incomplete gamma function P(A,X) evaluated by its series 
! representation as GAMSER. Also returns GAMMA(A) as GLN.
      IMPLICIT NONE
      INTEGER :: N
      REAL :: A,GAMSER,GLN,X,AP,de1,summ
      INTEGER, PARAMETER :: ITMAX = 500
      REAL, PARAMETER :: EPS = 3.E-7

      GLN = GAMLN(A)
      IF (X.LE.0.) THEN
         IF (X.LT.0.) CALL wrf_error_fatal ( 'WARNING: X <0 in GSER' )
         GAMSER = 0.
         RETURN
      ENDIF
      AP = A
      summ = 1./A
      de1 = summ
      DO N = 1,ITMAX
         AP = AP+1.
         de1 = de1*X/AP
         summ = summ+de1
         IF (ABS(de1).LT.ABS(summ)*EPS) GOTO 777
      ENDDO
      CALL wrf_error_fatal ('Warning : ITMAX too small in GSER')
 777  GAMSER = summ*EXP(-X+A*LOG(X)-GLN)

      RETURN

      END SUBROUTINE GSER
!=======================================================================
      SUBROUTINE CFG(GAMMCF,A,X,GLN)                                    ! Referred to MY2 scheme
!=======================================================================
! USES GAMLN, Returns the incomplete gamma function (Q(A,X) evaluated by tis continued fraction
! representation as GAMMCF.  Also returns ln(GAMMA(A)) as GLN.  ITMAX is the maximum allowed number of iterations;
! EPS is the relative accuracy; FPMIN is a number near the smallest representable floating-point number.
      IMPLICIT NONE
      INTEGER :: I
      REAL :: A,GAMMCF,GLN,X,AN,b,c,d,de1,h
      INTEGER, PARAMETER :: ITMAX = 500
      REAL, PARAMETER :: EPS = 3.E-7
      REAL, PARAMETER :: fpmin = 1.E-30

      GLN = GAMLN(A)
      b = X+1.-A
      c = 1./fpmin
      d = 1./b
      h = d
      DO I = 1,ITMAX
         AN = -I*(I-A)
         b = b+2.
         d = AN*d+b
         IF (ABS(d).LT.fpmin) d = fpmin
         c = b+AN/c
         IF (ABS(c).LT.fpmin) c = fpmin
         d = 1./d
         de1 = d*c
         h = h*de1
         IF (ABS(de1-1.).LT.EPS) GOTO 888
      ENDDO
      CALL wrf_error_fatal ('Warning : ITMAX too small in gcf')
 888  GAMMCF = EXP(-X+A*LOG(X)-GLN)*h
      RETURN

      END SUBROUTINE CFG
!=======================================================================
!
!======================================================================
      SUBROUTINE SOLVE_AFAC(TK1D,QC1D,NC1D,LAMC,MVDC,AFAC)
!======================================================================
      IMPLICIT NONE
      REAL :: TK1D,QC1D,NC1D,LAMC,GC1,MVDC,AFAC,LAMCMIN,LAMCMAX,C3M1D, &
              SIGC,MNRC,MVRC,EFRC,KDX,LTK,LQC

      IF (NC1D.LT.NSMALL) THEN
         LTK  = LOG(TK1D)
         LQC  = -1.*LOG(QC1D)
         NC1D = EXP(DNC0+DNC1*LTK+DNC2*LTK**2.+DNC3*LTK**3.-0.25*LQC)
      ENDIF
      IF (QC1D.GE.QSMAL1.AND.NC1D.LT.NSMAL1) THEN
         LTK  = LOG(TK1D)
         LQC  = -1.*LOG(QC1D)
         NC1D = EXP(DNC0+DNC1*LTK+DNC2*LTK**2.+DNC3*LTK**3.-0.25*LQC)
      ENDIF
      C3M1D = QC1D*V2M3/RHOW
      IF (AFAC_3M.EQ.0) THEN
         AFAC = AFAC0
      ELSEIF (AFAC_3M.EQ.1) THEN
         MVRC = (QC1D/NC1D/C4PI3W)**THRD
         MVRC = MIN(MAX(MVRC,RCMIN),RCMAX)
         EFRC = EXP(EFC1+EFC2*LOG(NC1D)+EFC3*LOG(MVRC))
!         KDX  = MIN(MAX(KCCMIN,C3M1D/(8.*EFRC**3.*NC1D)),KCCMAX)
         KDX  = MAX(KCCMIN,MIN(KCCMAX,(MVRC/EFRC)**3.))
         AFAC = (6.*KDX-3.+SQRT(8.*KDX+1.))/(2.-2.*KDX)
         AFAC = MIN(MAX(AFAC,AFAMIN),AFAMAX)
      ELSEIF (AFAC_3M.EQ.2) THEN
         SIGC = EXP(SIG1+SIG2*LOG(NC1D)+SIG3*LOG(QC1D))
         MNRC = EXP(MNR1+MNR2*LOG(NC1D)+MNR3*LOG(QC1D))
         AFAC = MIN(MAX(SIGC/MNRC,AFAMIN),AFAMAX)
      ENDIF
      GC1     = GAMLN(AFAC+1.)
      LAMC    = (EXP(GAMLN(AFAC+4.)-GC1)*NC1D/C3M1D)**THRD
      LAMCMIN = (EXP(GAMLN(AFAC+4.)-GC1))**THRD/DCMAX
      LAMCMAX = (EXP(GAMLN(AFAC+4.)-GC1))**THRD/DCMIN
      IF (LAMC.LT.LAMCMIN) THEN
         LAMC = LAMCMIN
         NC1D = C3M1D*EXP(GAMLN(AFAC+1.)-GAMLN(AFAC+4.)+3.*LOG(LAMC))
      ELSEIF (LAMC.GT.LAMCMAX) THEN
         LAMC = LAMCMAX
         NC1D = C3M1D*EXP(GAMLN(AFAC+1.)-GAMLN(AFAC+4.)+3.*LOG(LAMC))
      ENDIF
      MVDC = (EXP(GAMLN(AFAC+4.)-GAMLN(AFAC+1.)))**THRD/LAMC

      END SUBROUTINE SOLVE_AFAC
!======================================================================
!
!======================================================================
      SUBROUTINE SOLVE_AFAR(TK1D,QR1D,NR1D,LAMR,MVDR,AFAR)
!======================================================================
      IMPLICIT NONE
      REAL :: TK1D,QR1D,NR1D,LAMR,GR1,MVDR,LAMRMIN,LAMRMAX,R3M1D,AFAR, &
              BDR,MVRR,EFRR,KDX,LTK,LQR

      IF (NR1D.LT.NSMALL) THEN
         LTK  = LOG(TK1D)
         LQR  = -1.*LOG(QR1D)
         NR1D = EXP(-5793.7852+3191.1171*LTK-582.73279*LTK**2.+        &
                35.346854*LTK**3.-0.25*LQR)
      ENDIF
      R3M1D = QR1D*V2M3/RHOW
      IF (AFAR_3M.EQ.0) THEN
         AFAR = AFAR0
      ELSEIF (AFAR_3M.EQ.1) THEN
         MVRR = MIN(RRMAX,MAX(RRMIN,(QR1D/NR1D/C4PI3W)**THRD))
         EFRR = EXP(EFR1+EFR2*LOG(NR1D)+EFR3*LOG(MVRR))
!         KDX  = MIN(MAX(KCRMIN,R3M1D/(8.*EFRR**3.*NR1D)),KCRMAX)
         KDX  = MAX(KCRMIN,MIN(KCRMAX,(MVRR/EFRR)**3.))
         AFAR = (6.*KDX-3.+SQRT(8.*KDX+1.))/(2.-2.*KDX)
         AFAR = MIN(MAX(AFAR,AFAMIN),AFAMAX)
      ELSEIF (AFAR_3M.EQ.2) THEN
         BDR  = MIN(MAX((R3M1D/NR1D)**THRD,DRMIN),DRMAX)
         AFAR = MAX(AFAMIN,19.*TANH(0.6*(1.E3*BDR-1.8))+17.)
      ENDIF
      GR1     = GAMLN(AFAR+1.)
      LAMR    = (EXP(GAMLN(AFAR+4.)-GR1)*NR1D/R3M1D)**THRD
      LAMRMIN = (EXP(GAMLN(AFAR+4.)-GR1))**THRD/DRMAX
      LAMRMAX = (EXP(GAMLN(AFAR+4.)-GR1))**THRD/DRMIN
      IF (LAMR.LT.LAMRMIN) THEN
         LAMR = LAMRMIN
         NR1D = R3M1D*EXP(GAMLN(AFAR+1.)-GAMLN(AFAR+4.)+3.*LOG(LAMR))
      ELSEIF (LAMR.GT.LAMRMAX) THEN
         LAMR = LAMRMAX
         NR1D = R3M1D*EXP(GAMLN(AFAR+1.)-GAMLN(AFAR+4.)+3.*LOG(LAMR))
      ENDIF
      MVDR = (EXP(GAMLN(AFAR+4.)-GAMLN(AFAR+1.)))**THRD/LAMR

      END SUBROUTINE SOLVE_AFAR
!======================================================================

!======================================================================
      SUBROUTINE SOLVE_AFAI(TK1D,P1D,RHO,QV1D,QI1D,NI1D,VI1D,FI1D,     &
                 I2M1D,I3M1D,ADAGR,ZETA,LAMI,AFAI,MVDI,RHOI,AMI,BMI,   &
                 AVI,BVI,BEST)
!======================================================================
      IMPLICIT NONE
      INTEGER :: HID
      REAL :: TK1D,P1D,RHO,QV1D,QI1D,NI1D,VI1D,FI1D,I2M1D,I3M1D,ADAGR, &
              ZETA,RHOI,LAMI,AFAI,AMI,BMI,AVI,BVI,MVDI,GI1,TC1D,ESW,   &
              ESI,QVSI,INHGR,I3M0,BEST0,LAMIMIN,LAMIMAX,IPF,IPG,KDX,   &
              ZETA2,ZETA4,KINV,AAI,BAI,IBA1,C1X2,VTA1,VTB1,IPH2,IPG2,  &
              AVIA0,BEST,BDI,FDI,LTK,LQI,MDI

      IF (ICE_RHOI.EQ.0) THEN
         RHOI = RHOI0
         VI1D = 0.
      ELSEIF (ICE_RHOI.EQ.1) THEN
         IF (VI1D.LT.ISMALL) THEN
            TC1D  = TK1D-TK0C
            HID   = MAX(MIN(NINT(ABS(TC1D)/0.25),120),0)
            ESW   = MIN(0.99*P1D,POLYSVP(TK1D,0))
            ESI   = MIN(0.99*P1D,POLYSVP(TK1D,1))
            IF (ESI.GT.ESW) ESI = ESW
            QVSI  = 0.622*ESI/(P1D-ESI)
            INHGR = ITBLE(HID)
            RHOI  = RHOI0*EXP(-3.*MAX((QV1D-QVSI)-5.E-5,0.)/INHGR)
            VI1D  = QI1D/RHOI
         ENDIF
         RHOI = QI1D/VI1D
         IF (RHOI.LT.RHOIMIN) THEN
            RHOI = RHOIMIN
         ELSEIF (RHOI.GT.RHOIMAX) THEN
            RHOI = RHOIMAX
         ENDIF
         VI1D = QI1D/RHOI
      ELSEIF (ICE_RHOI.EQ.2) THEN
         RHOI = RHOI1
         VI1D = 0.
      ENDIF
      IF (NI1D.LT.NSMALL) THEN
         LTK  = LOG(TK1D)
         LQI  = -1.*LOG(QI1D)
         MDI  = EXP(-3.2653646+2.0539073*LTK-0.25*LQI)/1.E3
         NI1D = 1.E9*QI1D*V2M3/RHOI/MDI**3.
      ENDIF
      IF (QI1D.GE.QSMAL1.AND.NI1D.LT.NSMAL1) THEN
         LTK  = LOG(TK1D)
         LQI  = -1.*LOG(QI1D)
         MDI  = EXP(-3.2653646+2.0539073*LTK-0.25*LQI)/1.E3
         NI1D = 1.E9*QI1D*V2M3/RHOI/MDI**3.
      ENDIF
      IF (I2M1D.GE.ASMALL.AND.I3M1D.GE.ISMALL) THEN
         KDX = MAX(0.,I2M1D**3./(I3M1D**2.*NI1D))
         IF (KDX.GT.KCIMAX) THEN
            KDX   = KCIMIN
            I2M1D = (KDX*NI1D*I3M1D**2.)**THRD
         ELSEIF (KDX.LT.KCIMIN) THEN
            KDX   = KCIMIN
            I2M1D = (KDX*NI1D*I3M1D**2.)**THRD
         ENDIF
         AFAI = (6.*KDX-3.+SQRT(8.*KDX+1.))/(2.-2.*KDX)
         AFAI = MIN(MAX(AFAI,AFAMIN),AFAMAX)
!         LAMI = SQRT(NI1D*(AFAI+2.)*(AFAI+1.)/I2M1D)
!         LAMI = (NI1D*(AFAI+1.)*(AFAI+2.)*(AFAI+3.)/I3M1D)**THRD
         LAMI = (AFAI+3.)*I2M1D/I3M1D
         LAMIMIN = (EXP(GAMLN(AFAI+4.)-GAMLN(AFAI+1.)))**THRD/DIMAX
         LAMIMAX = (EXP(GAMLN(AFAI+4.)-GAMLN(AFAI+1.)))**THRD/DIMIN
         IF (LAMI.LT.LAMIMIN) THEN
            LAMI = LAMIMIN
            NI1D = I3M1D*EXP(GAMLN(AFAI+1.)-GAMLN(AFAI+4.)+3.*LOG(LAMI))
         ELSEIF (LAMI.GT.LAMIMAX) THEN
            LAMI = LAMIMAX
            NI1D = I3M1D*EXP(GAMLN(AFAI+1.)-GAMLN(AFAI+4.)+3.*LOG(LAMI))
         ENDIF
      ELSEIF (I2M1D.LT.ASMALL.AND.I3M1D.GE.ISMALL) THEN
         IF (AFAI_3M.EQ.0) THEN
            AFAI  = AFAI0
            I2M1D = 0.
         ELSEIF (AFAI_3M.EQ.1) THEN
            AFAI  = AFAI0
            I2M1D = (KCIMIN*NI1D*I3M1D**2.)**THRD
         ELSEIF (AFAI_3M.EQ.2) THEN
            BDI  = MIN(MAX((I3M1D/NI1D)**THRD*1.E3,DIMIN*1.E3),DIMAX*1.E3)
            FDI  = 0.074015986+0.79866676*BDI-0.0094468892*LOG(NI1D)+  &
                   0.38235092*BDI**2.+0.00029811542*LOG(NI1D)**2.+     &
                   0.019052614*BDI*LOG(NI1D)
            KDX  = MAX(KCIMIN,MIN(KCIMAX,(BDI/FDI)**3.))
            AFAI = (6.*KDX-3.+SQRT(8.*KDX+1.))/(2.-2.*KDX)
            AFAI = MIN(MAX(AFAI,AFAMIN),AFAMAX)
!            AFAI  = MAX(AFAMIN,12.*TANH(0.7*(BDI-1.7))+11.)
            I2M1D = 0.
         ENDIF
         LAMI = (EXP(GAMLN(AFAI+4.)-GAMLN(AFAI+1.))*NI1D/I3M1D)**THRD
         LAMIMIN = (EXP(GAMLN(AFAI+4.)-GAMLN(AFAI+1.)))**THRD/DIMAX
         LAMIMAX = (EXP(GAMLN(AFAI+4.)-GAMLN(AFAI+1.)))**THRD/DIMIN
         IF (LAMI.LT.LAMIMIN) THEN
            LAMI = LAMIMIN
            NI1D = I3M1D*EXP(GAMLN(AFAI+1.)-GAMLN(AFAI+4.)+3.*LOG(LAMI))
         ELSEIF (LAMI.GT.LAMIMAX) THEN
            LAMI = LAMIMAX
            NI1D = I3M1D*EXP(GAMLN(AFAI+1.)-GAMLN(AFAI+4.)+3.*LOG(LAMI))
         ENDIF
      ELSE
         IF (AFAI_3M.EQ.0) THEN
            AFAI  = AFAI0
            I2M1D = 0.
         ELSE
            BDI  = (QI1D*V2M3/NI1D/RHOI)**THRD*1.E3
            BDI  = MAX(MIN(DIMAX*1.E3,BDI),DIMIN*1.E3)
            FDI  = 7.4015986E-2+7.9866676E-1*BDI-9.4468892E-3*LOG(     &
                   NI1D)+3.8235092E-1*BDI**2.+2.9811542E-4*LOG(NI1D)** &
                   2.+1.9052614E-2*BDI*LOG(NI1D)
            KDX  = MAX(KCIMIN,MIN(KCIMAX,(BDI/FDI)**3.))
            AFAI = (6.*KDX-3.+SQRT(8.*KDX+1.))/(2.-2.*KDX)
            AFAI = MIN(MAX(AFAI,AFAMIN),AFAMAX)
!            AFAI = MAX(AFAMIN,12.*TANH(0.7*(BDI-1.7))+11.)
            IF (AFAI_3M.EQ.1) THEN
               KDX   = (AFAI**2.+3.*AFAI+2.)/(AFAI**2.+6.*AFAI+9.)
               I2M1D = (KDX*NI1D*(QI1D*V2M3/RHOI)**2.)**THRD
            ELSEIF (AFAI_3M.EQ.2) THEN
               I2M1D = 0.
            ENDIF
         ENDIF
         GI1  = GAMLN(AFAI+1.)
         LAMI = (EXP(GAMLN(AFAI+4.)-GI1)*PI*RHOI*NI1D/QI1D/6.)**THRD
         LAMIMIN = (EXP(GAMLN(AFAI+4.)-GAMLN(AFAI+1.)))**THRD/DIMAX
         LAMIMAX = (EXP(GAMLN(AFAI+4.)-GAMLN(AFAI+1.)))**THRD/DIMIN
         IF (LAMI.LT.LAMIMIN) THEN
            LAMI = LAMIMIN
            NI1D = QI1D*V2M3/RHOI*EXP(GAMLN(AFAI+1.)-GAMLN(AFAI+4.)+   &
                   3.*LOG(LAMI))
         ELSEIF (LAMI.GT.LAMIMAX) THEN
            LAMI = LAMIMAX
            NI1D = QI1D*V2M3/RHOI*EXP(GAMLN(AFAI+1.)-GAMLN(AFAI+4.)+   &
                   3.*LOG(LAMI))
         ENDIF
      ENDIF
      MVDI = (EXP(GAMLN(AFAI+4.)-GAMLN(AFAI+1.)))**THRD/LAMI
      IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
         I3M0 = NI1D*DI0**3.
         IF (MVDI.GT.(DI0+1.E-7)) THEN
            IF (ICE_SHAPE.EQ.0) THEN
               ZETA = 0.; ADAGR = 1.
               FI1D = I3M1D
            ELSEIF (ICE_SHAPE.EQ.1) THEN
               ZETA = LOG(FI1D/I3M1D)/LOG(I3M1D/I3M0)
               IF (ZETA.GT.0.4) THEN
                  ZETA = 0.4
                  FI1D = (I3M1D/I3M0)**ZETA*I3M1D
               ELSEIF (ZETA.LT.(-0.4)) THEN
                  ZETA = -0.4
                  FI1D = (I3M1D/I3M0)**ZETA*I3M1D
               ENDIF
               ADAGR = (1.+2.*ZETA)/(1.-ZETA)
            ENDIF
         ELSE
            FI1D = I3M1D
            ZETA = 0.; ADAGR = 1.
         ENDIF
      ELSE
         I3M1D = QI1D*V2M3/RHOI
         IF (ICE_SHAPE.EQ.0) THEN
            FI1D = I3M1D
            ZETA = 0.; ADAGR = 1.
         ELSEIF (ICE_SHAPE.EQ.1) THEN
            TC1D  = TK1D-TK0C
            HID   = MAX(MIN(NINT(ABS(TC1D)/0.25),120),0)
            ADAGR = MAX(MIN(ITBLE(HID),2.),0.5)**THRD
            ZETA  = (ADAGR-1.)/(ADAGR+2.)
            I3M0  = NI1D*DI0**3.
            FI1D  = (I3M1D/I3M0)**ZETA*I3M1D
         ENDIF
      ENDIF
      IF ((ADAGR-1.).GE.SLIMIT) THEN
         AMI = PI*RHOI*DI0**(2.-2./ADAGR)/6.
         BMI = 2./ADAGR+1.
      ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
         AMI = PI*RHOI*DI0**(1.-ADAGR)/6.
         BMI = ADAGR+2.
      ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
         AMI = PI*RHOI/6.
         BMI = BMI0
      ENDIF
      IF (ICE_VTI.EQ.0) THEN
         AVI = AVI0
         BVI = BVI0
      ELSEIF (ICE_VTI.EQ.1) THEN
         KINV  = (1.72E-5*(393./(TK1D+120.))*(TK1D/TK0C)**1.5)/RHO
         BEST0 = 2.*G*NI1D/(KINV**2.)
         IF ((ADAGR-1.).GE.SLIMIT) THEN
            AAI   = PI/4./DI0**ZETA
            BAI   = 3.*(ADAGR+1.)/(ADAGR+2.)
            IPH2  = 2.*3.*ADAGR/(ADAGR+2.)
            ZETA4 = 4.*(ADAGR-1.)/(ADAGR+2.)
            IBA1  = BMI+IPH2-BAI
            BEST  = BEST0*AMI*EXP(GAMLN(IBA1+AFAI+1.)-GAMLN(AFAI+1.)-  &
                    IBA1*LOG(LAMI))/(AAI*DI0**ZETA4)
            C1X2  = VTC1*BEST**5.E-1
            VTB1  = C1X2/(1.+C1X2)**5.E-1/((1.+C1X2)**5.E-1-1.)/2.-    &
                    VTA0*VTB0*BEST**VTB0/VTC2/(SQRT(1.+C1X2)-1.)**2.
            VTA1  = MAX((VTC2*((1.+C1X2)**5.E-1-1.)**2.-VTA0*BEST**    &
                    VTB0)/BEST**VTB1,0.)
            AVIA0 = VTA1*KINV**(1.-2.*VTB1)
            AVI   = AVIA0*(2.*AMI*G/(AAI*DI0**ZETA4))**VTB1
            BVI   = VTB1*(BMI+IPH2-BAI)-1.
         ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
            ZETA2 = 2.*(ADAGR-1.)/(ADAGR+2.)
            AAI   = PI*DI0**ZETA2/4.
            BAI   = 2.*3./(ADAGR+2.)
            IPG2  = 2.*3./(ADAGR+2.)
            IBA1  = BMI+IPG2-BAI
            BEST  = BEST0*AMI*DI0**ZETA2*EXP(GAMLN(IBA1+AFAI+1.)-      &
                    GAMLN(AFAI+1.)-IBA1*LOG(LAMI))/AAI
            C1X2  = VTC1*BEST**5.E-1
            VTB1  = C1X2/(1.+C1X2)**5.E-1/((1.+C1X2)**5.E-1-1.)/2.-    &
                    VTA0*VTB0*BEST**VTB0/VTC2/(SQRT(1.+C1X2)-1.)**2.
            VTA1  = MAX((VTC2*((1.+C1X2)**5.E-1-1.)**2.-VTA0*BEST**    &
                    VTB0)/BEST**VTB1,0.)
            AVIA0 = VTA1*KINV**(1.-2.*VTB1)
            AVI   = AVIA0*(2.*AMI*G*DI0**ZETA2/AAI)**VTB1
            BVI   = VTB1*(BMI+IPG2-BAI)-1.
         ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
            AAI   = PI/4.
            BAI   = 2.
            IBA1  = BMI
            BEST  = BEST0*AMI*EXP(GAMLN(IBA1+AFAI+1.)-GAMLN(AFAI+1.)-  &
                    IBA1*LOG(LAMI))/AAI
            C1X2  = VTC1*BEST**5.E-1
            VTB1  = C1X2/(1.+C1X2)**5.E-1/((1.+C1X2)**5.E-1-1.)/2.-    &
                    VTA0*VTB0*BEST**VTB0/VTC2/(SQRT(1.+C1X2)-1.)**2.
            VTA1  = MAX((VTC2*((1.+C1X2)**5.E-1-1.)**2.-VTA0*BEST**    &
                    VTB0)/BEST**VTB1,0.)
            AVIA0 = VTA1*KINV**(1.-2.*VTB1)
            AVI   = AVIA0*(2.*AMI*G/AAI)**VTB1
            BVI   = VTB1*BMI-1.
         ENDIF
      ENDIF

      END SUBROUTINE SOLVE_AFAI
!======================================================================
!
!======================================================================
      SUBROUTINE SOLVE_AFAS(TK1D,RHO,QS1D,QC1D,NS1D,VS1D,FS1D,S2M1D,   &
                 AFAS,LAMS,MVDS,RHOS,SASPR,AMS,AVS,BVS)
!======================================================================
      IMPLICIT NONE
      INTEGER :: TBIN,DBIN
      REAL :: TK1D,RHO,QS1D,QC1D,NS1D,VS1D,FS1D,S2M1D,AFAS,RHOS,SASPR, &
              LAMS,AMS,AVS,BVS,MVDS,LAMSMIN,LAMSMAX,KINV,C1X2,VTA1,    &
              VTB1,BEST,S3M1D,KDX,TC1D,SBA1,AMS2,AAS2,AMS3,BMS3,AAS3,  &
              BAS3,LAMD,BDS,FDS,LTK,LQS,MDS

      IF (ICE_RHOS.EQ.0) THEN
         RHOS = RHOS0
         VS1D = 0.
      ELSEIF (ICE_RHOS.EQ.1) THEN
         RHOS = QS1D/(VS1D+ISMALL)
         IF (RHOS.GT.RHOIMAX) THEN
            RHOS = RHOIMAX
         ELSEIF (RHOS.LT.RHOIMIN) THEN
            RHOS = RHOIMIN
         ENDIF
         VS1D = QS1D/RHOS
      ELSEIF (ICE_RHOS.EQ.2) THEN
         LTK = LOG(TK1D)
         LQS = -1.*LOG(QS1D)
         IF (TK1D.LT.TK0C) THEN
            RHOS = 15740.702-6098.0087*LTK+503.33089*LQS+594.29913*    &
                   LTK**2.+1.9033961*LQS**2.-94.950429*LTK*LQS
         ELSE
            RHOS = EXP(-64808.666+23113.508*LTK-36.46632*LQS-2060.6024*&
                   LTK**2.-0.005729458*LQS**2.+6.5057411*LTK*LQS)
         ENDIF
         RHOS = MIN(MAX(RHOS,RHOIMIN),RHOIMAX)
         VS1D = 0.
      ENDIF
      IF (NS1D.LT.NSMALL) THEN
         LTK  = LOG(TK1D)
         LQS  = -1.*LOG(QS1D)
         MDS  = EXP(-123.23898+40.74706*LTK-3.0333477*LTK**2.-         &
                0.31219981*LQS+0.0012798222*LQS**2.)/1.E3
         NS1D = 1.E9*QS1D*V2M3/RHOS/MDS**3.
      ENDIF
      AMS   = PI*RHOS/6.
      S3M1D = QS1D/AMS
      IF (AGG_SHAPE.EQ.0) THEN
         SASPR = 1.
         FS1D  = 0.
      ELSEIF (AGG_SHAPE.EQ.1) THEN
         SASPR = FS1D/S3M1D
         IF (SASPR.LT.SASMIN) THEN
            SASPR = SASMIN
            FS1D  = SASPR*S3M1D
         ELSEIF (SASPR.GT.SASMAX) THEN
            SASPR = SASMAX
            FS1D  = SASPR*S3M1D
         ENDIF
      ENDIF
      IF (S2M1D.GE.ASMALL.AND.S3M1D.GE.ISMALL) THEN
         KDX = MAX(0.,S2M1D**3./(S3M1D**2.*NS1D))
         IF (KDX.GT.KCSMAX) THEN
            KDX   = KCSMIN
            S2M1D = (KDX*NS1D*S3M1D**2.)**THRD
         ELSEIF (KDX.LT.KCSMIN) THEN
            KDX   = KCSMIN
            S2M1D = (KDX*NS1D*S3M1D**2.)**THRD
         ENDIF
         AFAS = (6.*KDX-3.+SQRT(8.*KDX+1.))/(2.-2.*KDX)
         AFAS = MIN(MAX(AFAS,AFAMIN),AFAMAX)
!         LAMS = (NS1D*(AFAS+1.)*(AFAS+2.)*(AFAS+3.)/S3M1D)**THRD
         LAMS = (AFAS+3.)*S2M1D/S3M1D
      ELSE
         IF (AFAS_3M.EQ.0) THEN
            AFAS  = AFAS0
            S2M1D = 0.
         ELSEIF (AFAS_3M.EQ.1) THEN
            AFAS  = AFAS0
            S2M1D = (KCSMIN*NS1D*S3M1D**2.)**THRD
         ELSEIF (AFAS_3M.EQ.2) THEN
            BDS   = MIN(MAX((S3M1D/NS1D)**THRD*1.E3,DSMIN*1.E3),DSMAX*1.E3)
            IF (TK1D.GE.TK0C) THEN
               FDS = -0.21911541+1.2739845*BDS+0.10141003*LOG(NS1D)+   &
                     0.30063818*BDS**2.-4.3857765E-3*LOG(NS1D)**2.-    &
                     7.8801732E-2*BDS*LOG(NS1D)
            ELSE
               IF (QC1D.GE.1.E-8) THEN
                  FDS = -1.1527014+2.9067645*BDS+0.25316062*LOG(NS1D)- &
                        0.17768557*BDS**2.-0.013117292*LOG(NS1D)**2.-  &
                        0.17020429*BDS*LOG(NS1D)
               ELSE
                  FDS = -0.2813929+1.7275463*BDS+0.045550156*LOG(NS1D)-&
                        0.16526226*BDS**2.-1.7699916E-3*LOG(NS1D)**2.- &
                        4.6441257E-2*BDS*LOG(NS1D)
               ENDIF
            ENDIF
            KDX   = MAX(KCSMIN,MIN(KCSMAX,(BDS/FDS)**3.))
            AFAS  = (6.*KDX-3.+SQRT(8.*KDX+1.))/(2.-2.*KDX)
            AFAS  = MIN(MAX(AFAS,AFAMIN),AFAMAX)
!            AFAS  = MAX(AFAMIN,4.5*TANH(0.5*(BDS-5.))+5.5)
            S2M1D = 0.
         ENDIF
         LAMS = (EXP(GAMLN(AFAS+4.)-GAMLN(AFAS+1.))*NS1D/S3M1D)**THRD
      ENDIF
      LAMSMIN = (EXP(GAMLN(AFAS+4.)-GAMLN(AFAS+1.)))**THRD/DSMAX
      LAMSMAX = (EXP(GAMLN(AFAS+4.)-GAMLN(AFAS+1.)))**THRD/DSMIN
      IF (LAMS.LT.LAMSMIN) THEN
         LAMS = LAMSMIN
         NS1D = S3M1D*EXP(GAMLN(AFAS+1.)-GAMLN(AFAS+4.)+3.*LOG(LAMS))
      ELSEIF (LAMS.GT.LAMSMAX) THEN
         LAMS = LAMSMAX
         NS1D = S3M1D*EXP(GAMLN(AFAS+1.)-GAMLN(AFAS+4.)+3.*LOG(LAMS))
      ENDIF
      MVDS = (EXP(GAMLN(AFAS+4.)-GAMLN(AFAS+1.)))**THRD/LAMS
      IF (ICE_VTS.EQ.0) THEN
         AVS = AVS0
         BVS = BVS0
      ELSEIF (ICE_VTS.EQ.1.OR.ICE_VTS.EQ.2) THEN
         KINV = (1.72E-5*(393./(TK1D+120.))*(TK1D/TK0C)**1.5)/RHO
         IF (AGG_SHAPE.EQ.0) THEN
            AMS3  = AMS
            BMS3  = BMS
            AAS3  = PI/4.
            BAS3  = 2.
            LAMD  = LAMS
         ELSEIF (AGG_SHAPE.EQ.1) THEN
            TC1D  = TK1D-TK0C
            IF (ICE_VTS.EQ.1) THEN
               TBIN = MIN(MAX(INT(ABS(TC1D)/5.)-4,0),8)
               IF (MVDS.LT.1.5E-4) DBIN = 0
               IF (MVDS.GE.1.5E-4.AND.MVDS.LT.3.E-4) DBIN = 1
               IF (MVDS.GE.3.E-4.AND.MVDS.LT.1.E-3) DBIN = 2
               IF (MVDS.GE.1.E-3.AND.MVDS.LT.3.E-3) DBIN = 3
               IF (MVDS.GE.3.E-3) DBIN = 4
               DBIN = MIN(MAX(DBIN,0),4)
               AMS3 = AMS1(TBIN*5+DBIN)
               BMS3 = BMS1(TBIN*5+DBIN)
               AAS3 = AAS1(TBIN*5+DBIN)
               BAS3 = BAS1(TBIN*5+DBIN)
               LAMD = MIN((AMS3*LAMS**3./AMS*EXP(GAMLN(BMS3+AFAS+1.)-  &
                      GAMLN(AFAS+4.)))**(1./BMS3),LAMS)
            ELSEIF (ICE_VTS.EQ.2) THEN
               BMS3 = 2.4+0.0085*MAX(TC1D,-65.)
               BAS3 = 2-0.19+0.0056*MAX(TC1D,-65.)
               AMS2 = 0.0102+0.00013*MAX(TC1D,-65.)
               AAS2 = 0.29+0.0035*MAX(TC1D,-65.)
               AMS3 = ((AMS2*(100.*MVDS)**BMS3)/1.E3)/(MVDS**BMS3)
               AAS3 = ((AAS2*(100.*MVDS)**BAS3)/1.E4)/(MVDS**BAS3)
               LAMD = LAMS
            ENDIF
         ENDIF
         SBA1 = BMS3+2.-BAS3
         BEST = 2.*G*NS1D*AMS3*EXP(GAMLN(SBA1+AFAS+1.)-GAMLN(AFAS+1.)- &
                SBA1*LOG(LAMD))/(KINV**2.*AAS3)
         C1X2 = VTC1*BEST**5.E-1
         VTB1 = C1X2/(1.+C1X2)**5.E-1/((1.+C1X2)**5.E-1-1.)/2.
         VTA1 = VTC2*((1+C1X2)**5.E-1-1.)**2./BEST**VTB1
         AVS  = VTA1*KINV**(1.-2.*VTB1)*(2.*AMS3*G/AAS3)**VTB1
         BVS  = VTB1*(BMS3+2.-BAS3)-1.
      ENDIF

      END SUBROUTINE SOLVE_AFAS
!======================================================================

!======================================================================
      SUBROUTINE SOLVE_AFAG(TK1D,RHO,QG1D,QC1D,NG1D,VG1D,G2M1D,LAMG,   &
                 AFAG,MVDG,RHOG,AMG,AVG,BVG)
!======================================================================
      IMPLICIT NONE
      REAL :: TK1D,RHO,QG1D,QC1D,NG1D,VG1D,G2M1D,G3M1D,RHOG,LAMG,AFAG, &
              AMG,AVG,BVG,BEST0,LAMGMIN,LAMGMAX,KDX,KINV,GG1,C1X2,VTA1,&
              VTB1,BEST,MVDG,GMLR,BDG,FDG,LTK,LQG,MDG

      IF (ICE_RHOG.EQ.0) THEN
         RHOG = RHOG1
         VG1D = 0.
      ELSEIF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
         RHOG = QG1D/(VG1D+ISMALL)
         IF (TK1D.GE.TK0C) THEN
            IF (RHOG.GT.RHOH) THEN
               RHOG = RHOH
            ENDIF
         ELSE
            IF (RHOG.GT.RHOG0) THEN
               RHOG = RHOG0
            ENDIF
         ENDIF
         IF (RHOG.LT.RHOIMIN) THEN
            RHOG = RHOIMIN
         ENDIF
         VG1D = QG1D/RHOG
      ENDIF
      IF (NG1D.LT.NSMALL) THEN
         LTK  = LOG(TK1D)
         LQG  = -1.*LOG(QG1D)
         MDG  = EXP(-2205.8027+1225.8046*LTK-226.27995*LTK**2.+        &
                13.929644*LTK**3.-0.25*LQG)/1.E3
         NG1D = 1.E9*QG1D*V2M3/RHOG/MDG**3.
      ENDIF
      AMG   = PI*RHOG/6.
      G3M1D = QG1D*V2M3/RHOG
      IF (G2M1D.GE.ASMALL.AND.G3M1D.GE.ISMALL) THEN
         KDX = MAX(0.,G2M1D**3./(G3M1D**2.*NG1D))
         IF (KDX.GT.KCGMAX) THEN
            KDX   = KCGMIN
            G2M1D = (KDX*NG1D*G3M1D**2.)**THRD
         ELSEIF (KDX.LT.KCGMIN) THEN
            KDX   = KCGMIN
            G2M1D = (KDX*NG1D*G3M1D**2.)**THRD
         ENDIF
         AFAG = (6.*KDX-3.+SQRT(8.*KDX+1.))/(2.-2.*KDX)
         AFAG = MIN(MAX(AFAG,AFAMIN),AFAMAX)
!         LAMG = (NG1D*(AFAG+1.)*(AFAG+2.)*(AFAG+3.)/G3M1D)**THRD
         LAMG = (AFAG+3.)*G2M1D/G3M1D
      ELSE
         IF (AFAG_3M.EQ.0) THEN
            AFAG  = AFAG0
            G2M1D = 0.
         ELSEIF (AFAG_3M.EQ.1) THEN
            AFAG  = AFAG0
            G2M1D = (KCGMIN*NG1D*G3M1D**2.)**THRD
         ELSEIF (AFAG_3M.EQ.2) THEN
           BDG = MIN(MAX((G3M1D/NG1D)**THRD*1.E3,DGMIN*1.E3),DGMAX*1.E3)
            IF (TK1D.GE.TK0C) THEN
               FDG = 0.58006354+0.79661229*BDG-0.18394382*LOG(NG1D)+   &
                     0.067371044*BDG**2.+9.832945E-3*LOG(NG1D)**2.+    &
                     0.12433055*BDG*LOG(NG1D)
            ELSE
               IF (QC1D.GE.1.E-8) THEN
                  FDG = 0.17363469+1.5044291*BDG-0.050639722*LOG(NG1D)+&
                        0.015101052*BDG**2.+2.5974719E-3*LOG(NG1D)**2.+&
                        0.01961464*BDG*LOG(NG1D)
               ELSE
                  FDG = 0.59259317-0.89933515*BDG+2.0893032*BDG**2.-   &
                        0.50305755*BDG**3.-2.2446793E-2*LOG(NG1D)+     &
                        2.7589047E-3*LOG(NG1D)**2.
               ENDIF
            ENDIF
            KDX   = MAX(KCGMIN,MIN(KCGMAX,(BDG/FDG)**3.))
            AFAG  = (6.*KDX-3.+SQRT(8.*KDX+1.))/(2.-2.*KDX)
            AFAG  = MIN(MAX(AFAG,AFAMIN),AFAMAX)
!            AFAG  = MAX(AFAMIN,5.5*TANH(0.7*(BDG-4.5))+8.5)
            G2M1D = 0.
         ENDIF
         LAMG = (EXP(GAMLN(AFAG+4.)-GAMLN(AFAG+1.))*NG1D/G3M1D)**THRD
      ENDIF
      LAMGMIN = (EXP(GAMLN(AFAG+4.)-GAMLN(AFAG+1.)))**THRD/DGMAX
      LAMGMAX = (EXP(GAMLN(AFAG+4.)-GAMLN(AFAG+1.)))**THRD/DGMIN
      IF (LAMG.LT.LAMGMIN) THEN
         LAMG = LAMGMIN
         NG1D = G3M1D*EXP(GAMLN(AFAG+1.)-GAMLN(AFAG+4.)+3.*LOG(LAMG))
      ELSEIF (LAMG.GT.LAMGMAX) THEN
         LAMG = LAMGMAX
         NG1D = G3M1D*EXP(GAMLN(AFAG+1.)-GAMLN(AFAG+4.)+3.*LOG(LAMG))
      ENDIF
      MVDG = (EXP(GAMLN(AFAG+4.)-GAMLN(AFAG+1.)))**THRD/LAMG
      IF (ICE_VTG.EQ.0) THEN
         AVG  = AVG0
         BVG  = BVG0
      ELSEIF (ICE_VTG.EQ.1) THEN
         KINV  = (1.72E-5*(393./(TK1D+120.))*(TK1D/TK0C)**1.5)/RHO
         BEST0 = 2.*G*NG1D/(KINV**2.)
         GG1   = GAMLN(AFAG+1.)
         BEST  = BEST0*AMG*EXP(GAMLN(BMG+AFAG+1.)-GG1-BMG*LOG(LAMG))/AAW
         C1X2  = VTC1*BEST**5.E-1
         VTB1  = C1X2/(1.+C1X2)**5.E-1/((1.+C1X2)**5.E-1-1.)/2.
         VTA1  = VTC2*((1+C1X2)**5.E-1-1.)**2./BEST**VTB1
         AVG   = VTA1*KINV**(1.-2.*VTB1)*(2.*AMG*G/AAW)**VTB1
         BVG   = VTB1*BMG-1.
      ENDIF

      END SUBROUTINE SOLVE_AFAG
!======================================================================
!
!======================================================================
      SUBROUTINE SOLVE_AFAH(TK1D,RHO,QH1D,NH1D,H2M1D,LAMH,AFAH,MVDH,   &
                 AVH,BVH)
!======================================================================
      IMPLICIT NONE
      REAL :: TK1D,RHO,QH1D,NH1D,H2M1D,H3M1D,LAMH,AFAH,AVH,BVH,BEST0,  &
              GH1,KDX,MVDH,LAMHMIN,LAMHMAX,KINV,C1X2,VTA1,VTB1,BEST,   &
              BDH,FDH,LTK,LQH

      IF (NH1D.LT.NSMALL) THEN
         LTK  = LOG(TK1D)
         LQH  = -1.*LOG(QH1D)
         NH1D = EXP(22.929406-4.2328364*LTK+0.30647567*LTK**2.-        &
                0.009233271*LTK**3.-0.25*LQH)
      ENDIF
      H3M1D = QH1D*V2M3/RHOH
      IF (H2M1D.GE.ASMALL.AND.H3M1D.GE.ISMALL) THEN
         KDX = MAX(0.,H2M1D**3./(H3M1D**2.*NH1D))
         IF (KDX.GT.KCHMAX) THEN
            KDX   = KCHMIN
            H2M1D = (KDX*NH1D*H3M1D**2.)**THRD
         ELSEIF (KDX.LT.KCHMIN) THEN
            KDX   = KCHMIN
            H2M1D = (KDX*NH1D*H3M1D**2.)**THRD
         ENDIF
         AFAH = (6.*KDX-3.+SQRT(8.*KDX+1.))/(2.-2.*KDX)
         AFAH = MIN(MAX(AFAH,AFAMIN),AFAMAX)
!         LAMH = (NH1D*(AFAH+1.)*(AFAH+2.)*(AFAH+3.)/H3M1D)**THRD
         LAMH = (AFAH+3.)*H2M1D/H3M1D
      ELSE
         IF (AFAH_3M.EQ.0) THEN
            AFAH  = AFAH0
            H2M1D = 0.
         ELSEIF (AFAH_3M.EQ.1) THEN
            AFAH  = AFAH0
            H2M1D = (KCHMIN*NH1D*H3M1D**2.)**THRD
         ELSEIF (AFAH_3M.EQ.2) THEN
           BDH = MIN(MAX((H3M1D/NH1D)**THRD*1.E3,DHMIN*1.E3),DHMAX*1.E3)
            IF (TK1D.GE.TK0C) THEN
               FDH = 1.157754+0.37852874*BDH-0.11129737*LOG(NH1D)+     &
                     0.13929599*BDH**2.+8.1105237E-3*LOG(NH1D)**2.+    &
                     5.7432113E-2*BDH*LOG(NH1D)
            ELSE
               FDH = -0.48246793+2.0407077*BDH+2.2262969E-2*LOG(NH1D)- &
                     0.158389*BDH**2.-5.5545804E-3*LOG(NH1D)**2.+      &
                     2.9443577E-2*BDH*LOG(NH1D)
            ENDIF
            KDX   = MAX(KCHMIN,MIN(KCHMAX,(BDH/FDH)**3.))
            AFAH  = (6.*KDX-3.+SQRT(8.*KDX+1.))/(2.-2.*KDX)
            AFAH  = MIN(MAX(AFAH,AFAMIN),AFAMAX)
!            AFAH = MAX(AFAMIN,3.7*TANH(0.3*(BDH-9.))+6.5)
            H2M1D = 0.
         ENDIF
         LAMH = (EXP(GAMLN(AFAH+4.)-GAMLN(AFAH+1.))*NH1D/H3M1D)**THRD
      ENDIF
      LAMHMIN = (EXP(GAMLN(AFAH+4.)-GAMLN(AFAH+1.)))**THRD/DHMAX
      LAMHMAX = (EXP(GAMLN(AFAH+4.)-GAMLN(AFAH+1.)))**THRD/DHMIN
      IF (LAMH.LT.LAMHMIN) THEN
         LAMH = LAMHMIN
         NH1D = H3M1D*EXP(GAMLN(AFAH+1.)-GAMLN(AFAH+4.)+3.*LOG(LAMH))
      ELSEIF (LAMH.GT.LAMHMAX) THEN
         LAMH = LAMHMAX
         NH1D = H3M1D*EXP(GAMLN(AFAH+1.)-GAMLN(AFAH+4.)+3.*LOG(LAMH))
      ENDIF
      MVDH = (EXP(GAMLN(AFAH+4.)-GAMLN(AFAH+1.)))**THRD/LAMH
      IF (ICE_VTH.EQ.0) THEN
         AVH  = AVH0
         BVH  = BVH0
      ELSEIF (ICE_VTH.EQ.1) THEN
         KINV  = (1.72E-5*(393./(TK1D+120.))*(TK1D/TK0C)**1.5)/RHO
         BEST0 = 2.*G*NH1D/(KINV**2.)
         GH1   = GAMLN(AFAH+1.)
         BEST  = BEST0*AMH*EXP(GAMLN(BMH+AFAH+1.)-GH1-BMH*LOG(LAMH))/AAW
         C1X2  = VTC1*BEST**5.E-1
         VTB1  = C1X2/(1.+C1X2)**5.E-1/((1.+C1X2)**5.E-1-1.)/2.
         VTA1  = VTC2*((1+C1X2)**5.E-1-1.)**2./BEST**VTB1
         AVH   = VTA1*KINV**(1.-2.*VTB1)*(2.*AMH*G/AAW)**VTB1
         BVH   = VTB1*BMH-1.
      ENDIF

      END SUBROUTINE SOLVE_AFAH
!======================================================================
!
!======================================================================
      SUBROUTINE AERO_CONST(CCNTY)
!======================================================================
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: CCNTY                                      ! 1.MARINE(WHITBY,1978);2.CLEAN CONTINENTAL(WHITBY,1978);
                                                                        ! 3.AVERAGE CONTINENTAL(WHITBY,1978);
                                                                        ! 4.URBAN(WHITBY,1978); 5.ICE NUECEI
      INTEGER, PARAMETER :: ID_SENS = 1                                 ! sensetivity test ID
      INTEGER, PARAMETER :: N1 = NCCN, N2 = 5                           ! AEROSOL MODE AND COMPONENT
      INTEGER :: IA,I,K,J,IM,IV,IB
      INTEGER, DIMENSION(NAER) :: IDAER
      REAL, DIMENSION(NAER) :: XMASS
      REAL, DIMENSION(N2) :: ASHX,CMASX,DNASX,AVANX                     ! AEROSOL SCALE HEIGHT. 3.57km-continental, 0.8km-marine;
                                                                        ! MOLAR WEIGHT OF AEROSOL (kg/mol); 
                                                                        ! DENSITY OF SOLID AEROSOL (kg/m^3);
                                                                        ! VAN'T HOFF FACTOR
      REAL, DIMENSION(N1,N2) :: ZCCNX,CNMODX,CNSTDX                     ! AEROSOL TRI-MODAL SPECTRUM PARAMETERS
      REAL, DIMENSION(N2,3) :: SENX                                     ! SENSITIVE TEST RATIO
      REAL :: XLOG3,XMTOT,XMAS,DUST_IN0,DUST_IN,DAIR,DZ0,ZH             ! Height of grid point [m]
      DOUBLE PRECISION :: DMODE,D2STDV,DLNXX,DERF,DSQRT,DLOG
      DATA ZCCNX/3.4E8,6.E7,3.E6, 1.E9,8.E8,7.2E5, 6.4E9,2.3E9,3.2E6,  &! Cheng et al. 2007, Table 1, number concentration
                 1.06E11,3.2E10,5.4E6, 4.02E-11,4.5E9,5.4E6/
      DATA CNMODX/5.E-9,3.55E-8,3.1E-7,8.E-9,3.3E-8,4.6E-7,7.5E-9,     &! Cheng et al. 2007, Table 1, mean size
                  3.8E-8,5.1E-7,7.E-9,2.7E-8,4.3E-7,4.5E-9,2.35E-8,    &
                  5.5E-7/
      DATA CNSTDX/0.47,0.69,0.99,0.47,0.74,0.79,0.53,0.69,0.77,0.59,   &! Cheng et al. 2007, Table 1, standard deviation
                  0.77,0.79,0.53,0.75,0.74/
      DATA ASHX/8.E2,3.57E3,3.57E3,2.E3,1.E4/                           ! SCALING HEIGHT
      DATA CMASX/0.13214,0.13214,0.13214,0.13214,0.13214/               ! MOLAR WEIGHT OF AEROSOL (kg/mol)
      DATA DNASX/1769.,1769.,1769.,1769.,1769./                         ! DENSITY OF SOLID AEROSOL (kg/m^3)
      DATA AVANX/3.,3.,3.,3.,3./                                        ! VAN'T HOFF FACTOR
      DATA SENX/1.,1.,1.,1.,1.,10.,10.,10.,10.,10.,0.1,0.1,0.1,0.1,0.1/ ! 1st, 2nd, and 3rd set for sensetivity test

      IF (CCNTY.EQ.1) THEN
         DNC0 = 46635.361;  DNC1 = -25567.933
         DNC2 = 4674.3534;  DNC3 = -284.84377
      ELSEIF (CCNTY.EQ.2) THEN
         DNC0 = -48575.888; DNC1 = 26634.802
         DNC2 = -4862.961;  DNC3 = 295.7886
      ELSEIF (CCNTY.EQ.3) THEN
         DNC0 = -63646.587; DNC1 = 34778.66
         DNC2 = -6326.6327; DNC3 = 383.28849
      ELSEIF (CCNTY.EQ.4) THEN
         DNC0 = -6069.3697; DNC1 = 3667.889
         DNC2 = -725.3412;  DNC3 = 47.260735
      ENDIF
      IBAER(1) = 1
      DO I = 2,NAER
         IBAER(I) = IBAER(I-1)+NAERN(I-1)
      ENDDO
      TBLRC(1)     = DLOG(1.D-9)                                        ! 0.001 um
      TBLRC(NTBXA) = DLOG(1.D-3)                                        ! 1000. um
      DO I = 2,NTBXA-1
         TBLRC(I) = TBLRC(1)+DBLE(I-1)/DBLE(NTBXA-1)*(TBLRC(NTBXA)-    &
                    TBLRC(1))                                           ! ln(rc)
      ENDDO
      IDAER = (/CCNTY,5/)
      PRINT *, 'AEROSOL ID =',IDAER
      DO J = 1,NAER
         ASH(J)   = ASHX(IDAER(J))
         DNAS(J)  = DNASX(IDAER(J))
         CMAS(J)  = CMASX(IDAER(J))
         AVAN(J)  = AVANX(IDAER(J))
         BETA1(J) = CMW*DNAS(J)*AVAN(J)/(CMAS(J)*RHOW)
         XMTOT = 0.
         DO IM = 1,N1
            ZCCN(IM,J)  = ZCCNX(IM,IDAER(J))*1.                         ! change the factor to change the initial concentration
            CNMOD(IM,J) = CNMODX(IM,IDAER(J))
            CNSTD(IM,J) = CNSTDX(IM,IDAER(J))
            XLOG3       = CNMOD(IM,J)*CNMOD(IM,J)*CNMOD(IM,J)
            WMAS(IM,J)  = ZCCN(IM,J)*XLOG3*EXP(4.5*CNSTD(IM,J)**2.)     ! factor for mass, the 3rd momentum
            RFACT(IM,J) = 1./(C4PI3*DNAS(J)*XLOG3*EXP(4.5*             &
                          CNSTD(IM,J)**2.))
            XMTOT = XMTOT+WMAS(IM,J)
         ENDDO
         DO IM = 1,N1
            WMAS(IM,J) = WMAS(IM,J)/XMTOT
         ENDDO

         DO I = 1,NTBXA
            TBLXF(I,J) = DBLE(0.)
         ENDDO
         DO IM = 1,N1
            DMODE = DLOG(DBLE(CNMOD(IM,J)))+DBLE(3.)*DBLE(CNSTD(IM,J))*DBLE(CNSTD(IM,J))
            D2STDV = DSQRT(DBLE(2.))*DBLE(CNSTD(IM,J))
            DO I = 1,NTBXA
               DLNXX = (TBLRC(I)-DMODE)/D2STDV
               TBLXF(I,J) = TBLXF(I,J)+DBLE(WMAS(IM,J))*DBLE(0.5)*     &
                            (1.D+0+DERF(DLNXX))
            ENDDO
         ENDDO                                                          ! IM=1,N1
         DO I = 1,NTBXA
            TBLXF(I,J) = DMAX1(1.D-20,TBLXF(I,J))
         ENDDO

         CALL FIND_RC0(1.D-6,CNMOD(1,J),CNSTD(1,J),WMAS(1,J),RXMIN(J), &
                       TBLXF(1,J),TBLRC)                                ! lower limit of dry aerosol fraction is set as 1.E-6
         WRITE(*,*)'FOR DRY AEROSOL FRACTION BE > 1.E-6'
         WRITE(*,'(A30,I1,A8,F6.4,A3)')'MINIMUN R-CUTOFF FOR AEROSOL'  &
               ,J,' IS SET ',RXMIN(J)*1.E6,' um'
         WRITE(*,*)'SENSITIVITY TEST ID:',ID_SENS
!----------------- FACTOR FOR SENSETIVITY ------------------------------
         SENS(J) = SENX(IDAER(J),ID_SENS)
      ENDDO

      END SUBROUTINE AERO_CONST
!======================================================================

!======================================================================
      SUBROUTINE INIT_AEROSOL(P,RHO,DZ,XLAND,QAERO,IDS,IDE,JDS,JDE,KDS,&
                 KDE,IMS,IME,JMS,JME,KMS,KME,ITS,ITE,JTS,JTE,KTS,KTE)
!======================================================================
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,IMS,IME,JMS,JME,  &
                             KMS,KME,ITS,ITE,JTS,JTE,KTS,KTE
      INTEGER :: ITF,JTF,IA,I,K,J,IM,K_PBL,IV,IB
      INTEGER, DIMENSION(NAER) :: IDAER
      REAL, INTENT(IN), DIMENSION(IMS:IME,JMS:JME) :: XLAND             ! 1:land ; 2:ocean
      REAL, INTENT(IN), DIMENSION(ITS:ITE,KTS:KTE,JTS:JTE) :: P,RHO,DZ
      REAL, DIMENSION(ITS:ITE,KTS:KTE,JTS:JTE,NAERT) :: QAERO
      REAL, DIMENSION(ITS:ITE,JTS:JTE,NAER) :: RASH
      REAL, DIMENSION(NAER) :: XMASS
      REAL, PARAMETER :: P_PBL = 85000.                                 ! set PBL pressure [Pa]
      REAL, PARAMETER :: BACKIN = 4.E5                                  ! initial ice nuclei number concentration (#/kg)
      REAL :: XLOG3,XMTOT,XMAS,DUST_IN0,DUST_IN,DAIR,DZ0,ZH             ! Height of grid point [m]
      DOUBLE PRECISION :: DMODE,D2STDV,DLNXX,DERF,DSQRT,DLOG

      ITF = MIN(ITE,IDE-1)
      JTF = MIN(JTE,JDE-1)
      DO IA = 1,NAERT
         DO K = KTS,KTE
         DO J = JTS,JTF
         DO I = ITS,ITF
            QAERO(I,K,J,IA) = 0.
         ENDDO
         ENDDO
         ENDDO
      ENDDO
      DO J = 1,NAER
         XMASS(J) = 0.
         DO IM = 1,NCCN
            XMAS = ZCCN(IM,J)*CNMOD(IM,J)**3.*EXP(4.5*CNSTD(IM,J)**2.)
            XMASS(J) = XMASS(J)+XMAS
         ENDDO
         XMASS(J) = XMASS(J)*C4PI3*DNAS(J)*SENS(J)
      ENDDO
      IF (ID_DUST*ID_IN.NE.0) THEN                                      ! IF DUST EXISTS
         DUST_IN0 = 0.
!         DO IM = 1,NCCN
!            DUST_IN0 = DUST_IN0+ZCCN(IM,ID_DUST)
!         ENDDO
      ELSE                                                              ! NO DUST
         DUST_IN0 = BACKIN
      ENDIF
      DO J = JTS,JTF
         DO I = ITS,ITF
            K_PBL = 1
            DO K = KTS,KTE
               IV = 1
               DO IA = 1,NAER
                  QAERO(I,K,J,IV) = XMASS(IA)/RHO(I,K,J)                ! dry aerosol [kg/kg]
                  RASH (I,J,IA)   = 1./ASH(IA)                          ! increase scale height if necessary
                  DUST_IN         = DUST_IN0                            ! dust # over land
                  IF (IA.EQ.ID_IN) THEN                                 ! IN
                     IF (ID_DUST.NE.0) THEN                             ! DUST IS CONSIDERED AS IN
!                        IF (XLAND(I,J).EQ.2.) THEN                      ! over ocean
!                           DUST_IN       = DUST_IN*0.01                 ! dust #
!                           RASH (I,J,IA) = RASH (I,J,IA)*0.5            ! double scale height
!                        ENDIF
                     ELSE
                        DUST_IN       = BACKIN                          ! set to background value
                        RASH (I,J,IA) = RASH (I,J,IA)*0.5               ! double scale height
                     ENDIF
                     DAIR    = RHO(I,K,J)
                     DUST_IN = MAX(BACKIN,DUST_IN)
                     QAERO(I,K,J,IV) = DUST_IN/DAIR                     ! 0.5 #/L [#/kg]*[kPa]      ! tsai
                  ELSE                                                  ! dust, NH42SO4, chemicals....
!                     IF (XLAND(I,J).EQ.2.) THEN                         ! over ocean
!                        IF (IA.EQ.ID_NH42SO4) THEN                      !
!                           QAERO(I,K,J,IV) = QAERO(I,K,J,IV)*0.05       ! reduce it
!                           RASH(I,J,IA)    = RASH(I,J,IA)*0.5           ! double scale height
!                        ELSE                                            ! dust or IN, not sulfate (the same)
!                           QAERO(I,K,J,IV) = QAERO(I,K,J,IV)*0.01       ! reduce it
!                           RASH(I,J,IA)    = RASH (I,J,IA)*0.5          ! double scale height
!                           DUST_IN         = DUST_IN0*0.01              ! dust #
!                        ENDIF
!                     ENDIF
                  ENDIF
                  IV = IV+NAERN(IA)
               ENDDO
               IF (P(I,K,J).LT.P_PBL) K_PBL = K
            ENDDO
            K_PBL = MIN(K_PBL,KTE-3+1)                                  ! at least 3 layers are within PBL
            ZH  = 0.
            DZ0 = 0.
            DO K = K_PBL-1,1,-1
               DAIR = RHO(I,K,J)
               ZH   = ZH+(DZ0+DZ(I,K,J))*0.5
               DZ0  = DZ(I,K,J)
               IV = 1
               DO IA = 1,NAER
                  QAERO(I,K,J,IV)=QAERO(I,K,J,IV)*EXP(-RASH(I,J,IA)*ZH) ! dry aerosol [kg/kg]*[kPa]
                  IV = IV+NAERN(IA)
               ENDDO
            ENDDO
            IV = 0
            DO IA = 1,NAER
               DO IB = 2,MIN(2,NAERN(IA))                               !  if NV>2
                  DO K = KTS,KTE
                     QAERO(I,K,J,IV+IB) = QAERO(I,K,J,IV+1)             ! set NV2: total = dry aerosol
                  ENDDO
               ENDDO
               IV = IV+NAERN(IA)
            ENDDO
         ENDDO                                                          ! FOR I LOOPS
      ENDDO                                                             ! FOR J LOOPS

      PRINT *,'aerosols_init,I,J,NAER',(ITS+ITE)/2,(JTS+JTE)/2,NAER
      DO K = KTS,KTE-1
         WRITE(*,'(I2,X,20(E12.6,X))') K,(QAERO((ITS+ITE)/2,K,         &
         (JTS+JTE)/2,IV),IV=1,NAERT)
      ENDDO

      RETURN

      END SUBROUTINE INIT_AEROSOL
!======================================================================
!
!=======================================================================
      SUBROUTINE MP_NTU(ITIMESTEP,TH,P,DZ,W,PII,DT_MP,SR,QV,QC,QR,QI,  &
                 QS,QG,QH,NC,NR,NI,NS,NG,NH,QDCN,QTCN,QCCN,QRCN,QNIN,  &
                 FI,FS,VI,VS,VG,AI,AS,AG,AH,I3M,RAINNC,RAINNCV,SNOWNC, &
                 SNOWNCV,GRAPNC,GRAPNCV,HAILNC,HAILNCV,IDS,IDE,JDS,JDE,&
                 KDS,KDE,IMS,IME,JMS,JME,KMS,KME,ITS,ITE,JTS,JTE,KTS,  &
                 KTE)
!======================================================================
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,IMS,IME,JMS,JME,  &
            KMS,KME,ITS,ITE,JTS,JTE,KTS,KTE,ITIMESTEP
      REAL, INTENT(IN) :: DT_MP
      REAL, DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: RAINNC,RAINNCV,&
            SNOWNC,SNOWNCV,GRAPNC,GRAPNCV,HAILNC,HAILNCV,SR
      REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(IN) :: PII,DZ,W,P
      REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(INOUT) ::       &
            TH,QV,QC,QR,QI,QS,QG,QH,NC,NR,NI,NS,NG,NH,VI,VS,VG,FI,FS,  &
            AI,AS,AG,AH,I3M,QDCN,QTCN,QCCN,QRCN,QNIN
      INTEGER :: I,K,J,NK,ITF,JTF
      REAL :: DT,DTMN
      REAL, DIMENSION(KTS:KTE) :: TK1D,P1D,W1D,S1D,DZ1D,QV1D,QC1D,QR1D,&
            QI1D,QS1D,QG1D,QH1D,NC1D,NR1D,NI1D,NS1D,NG1D,NH1D,VI1D,    &
            VS1D,VG1D,FI1D,FS1D,AI1D,AS1D,AG1D,AH1D,I3M1D,QDCN1D,      &
            QTCN1D,QCCN1D,QRCN1D,QNIN1D
      REAL, DIMENSION(ITS:ITE,JTS:JTE) :: CLODNCV,ICENCV,VINCV,VSNCV,  &
            VGNCV,FINCV,FSNCV,AINCV,ASNCV,AGNCV,AHNCV,I3MNCV
      REAL, DIMENSION(ITS:ITE,KTS:KTE,JTS:JTE) :: TK

      DT = DT_MP
      DTMN = DT_MP/60.
      ITF = MIN(ITE,IDE-1)
      JTF = MIN(JTE,JDE-1)
      DO I = ITS,ITF
      DO J = JTS,JTF
         CLODNCV(I,J) = 0.; RAINNCV(I,J) = 0.; ICENCV(I,J) = 0.
         SNOWNCV(I,J) = 0.; GRAPNCV(I,J) = 0.; HAILNCV(I,J) = 0.
         VINCV(I,J) = 0.;  VSNCV(I,J) = 0.;  VGNCV(I,J) = 0.
         FINCV(I,J) = 0.;  FSNCV(I,J) = 0.;  AINCV(I,J) = 0.
         ASNCV(I,J) = 0.;  AGNCV(I,J) = 0.;  AHNCV(I,J) = 0.
         I3MNCV(I,J) = 0.; SR(I,J) = 0.
!----------------- write data from 3D to 1D and assign upside down -----
         DO K = KTS,KTE
            NK = KTE-K+1
            S1D(K) = 0.
            TK1D(K) = TH(I,NK,J)*PII(I,NK,J)
            W1D(K)  = 0.5*(W(I,NK,J)+W(I,NK+1,J))                      ! W at half level
            QV1D(K) = QV(I,NK,J)
            DZ1D(K) = DZ(I,NK,J)
            P1D(K)  = P(I,NK,J)
            QC1D(K) = QC(I,NK,J)
            QR1D(K) = QR(I,NK,J)
            QI1D(K) = QI(I,NK,J)
            QS1D(K) = QS(I,NK,J)
            QG1D(K) = QG(I,NK,J)
            QH1D(K) = QH(I,NK,J)
            NC1D(K) = NC(I,NK,J)
            NR1D(K) = NR(I,NK,J)
            NI1D(K) = NI(I,NK,J)
            NS1D(K) = NS(I,NK,J)
            NG1D(K) = NG(I,NK,J)
            NH1D(K) = NH(I,NK,J)
            VI1D(K) = VI(I,NK,J)
            VS1D(K) = VS(I,NK,J)
            VG1D(K) = VG(I,NK,J)
            FI1D(K) = FI(I,NK,J)
            FS1D(K) = FS(I,NK,J)
            AI1D(K) = AI(I,NK,J)
            AS1D(K) = AS(I,NK,J)
            AG1D(K) = AG(I,NK,J)
            AH1D(K) = AH(I,NK,J)
            I3M1D(K) = I3M(I,NK,J)
            QDCN1D(K) = MAX(RLIMIT,QDCN(I,NK,J))
            QTCN1D(K) = MAX(RLIMIT,QTCN(I,NK,J))
            QCCN1D(K) = MAX(RLIMIT,QCCN(I,NK,J))
            QRCN1D(K) = MAX(RLIMIT,QRCN(I,NK,J))
            QNIN1D(K) = MAX(RLIMIT,QNIN(I,NK,J))
         ENDDO

         CALL NTU_MICRO(TK1D,QV1D,DZ1D,P1D,S1D,W1D,SR(I,J),ICENCV(I,J),&
              VINCV(I,J),VSNCV(I,J),VGNCV(I,J),FINCV(I,J),FSNCV(I,J),  &
              AINCV(I,J),ASNCV(I,J),AGNCV(I,J),AHNCV(I,J),I3MNCV(I,J), &
              CLODNCV(I,J),RAINNC(I,J),RAINNCV(I,J),SNOWNC(I,J),       &
              SNOWNCV(I,J),GRAPNC(I,J),GRAPNCV(I,J),HAILNC(I,J),       &
              HAILNCV(I,J),DT,DTMN,QC1D,QR1D,QI1D,QS1D,QG1D,QH1D,NC1D, &
              NR1D,NI1D,NS1D,NG1D,NH1D,VI1D,VS1D,VG1D,FI1D,FS1D,AI1D,  &
              AS1D,AG1D,AH1D,I3M1D,QDCN1D,QTCN1D,QCCN1D,QRCN1D,QNIN1D, &
              KTS,KTE)

         DO K = KTS,KTE
            NK = KTE-K+1
            TH(I,NK,J) = TK1D(K)/PII(I,NK,J)
            QV(I,NK,J) = MAX(0.,QV1D(K))
            QC(I,NK,J) = MAX(0.,QC1D(K))
            QR(I,NK,J) = MAX(0.,QR1D(K))
            QI(I,NK,J) = MAX(0.,QI1D(K))
            QS(I,NK,J) = MAX(0.,QS1D(K))
            QG(I,NK,J) = MAX(0.,QG1D(K))
            QH(I,NK,J) = MAX(0.,QH1D(K))
            NC(I,NK,J) = MAX(0.,NC1D(K))
            NR(I,NK,J) = MAX(0.,NR1D(K))
            NI(I,NK,J) = MAX(0.,NI1D(K))
            NS(I,NK,J) = MAX(0.,NS1D(K))
            NG(I,NK,J) = MAX(0.,NG1D(K))
            NH(I,NK,J) = MAX(0.,NH1D(K))
            VI(I,NK,J) = MAX(0.,VI1D(K))
            VS(I,NK,J) = MAX(0.,VS1D(K))
            VG(I,NK,J) = MAX(0.,VG1D(K))
            FI(I,NK,J) = MAX(0.,FI1D(K))
            FS(I,NK,J) = MAX(0.,FS1D(K))
            AI(I,NK,J) = MAX(0.,AI1D(K))
            AS(I,NK,J) = MAX(0.,AS1D(K))
            AG(I,NK,J) = MAX(0.,AG1D(K))
            AH(I,NK,J) = MAX(0.,AH1D(K))
            I3M(I,NK,J) = MAX(0.,I3M1D(K))
            QDCN(I,NK,J) = MAX(0.,QDCN1D(K))
            QTCN(I,NK,J) = MAX(0.,QTCN1D(K))
            QCCN(I,NK,J) = MAX(0.,QCCN1D(K))
            QRCN(I,NK,J) = MAX(0.,QRCN1D(K))
            QNIN(I,NK,J) = MAX(0.,QNIN1D(K))
         ENDDO
      ENDDO
      ENDDO

      END SUBROUTINE MP_NTU
!======================================================================
!
!======================================================================
      SUBROUTINE NTU_MICRO(TK3D,QV3D,DZ3D,P3D,S3D,W3D,SR,ICENCV,VINCV, &
                 VSNCV,VGNCV,FINCV,FSNCV,AINCV,ASNCV,AGNCV,AHNCV,      &
                 I3MNCV,CLODNCV,RAINNC,RAINNCV,SNOWNC,SNOWNCV,GRAPNC,  &
                 GRAPNCV,HAILNC,HAILNCV,DT,DTMN,QC3D,QR3D,QI3D,QS3D,   &
                 QG3D,QH3D,NC3D,NR3D,NI3D,NS3D,NG3D,NH3D,VI3D,VS3D,    &
                 VG3D,FI3D,FS3D,AI3D,AS3D,AG3D,AH3D,I3M3D,QDCN3D,      &
                 QTCN3D,QCCN3D,QRCN3D,QNIN3D,KTS,KTE)
!======================================================================
      IMPLICIT NONE 
      INTEGER, INTENT(IN) :: KTS,KTE
      INTEGER :: K,A,IV,IV0,IV1,IV2,IV3,IV4,IM,IT,J
      INTEGER, DIMENSION(KTS:KTE) :: HID
      REAL, DIMENSION(KTS:KTE), INTENT(IN) :: W3D,P3D
      REAL, DIMENSION(KTS:KTE) :: TK3D,TC3D,QV3D,DZ3D,S3D,QC3D,QR3D,   &
            QI3D,QS3D,QG3D,QH3D,QDCN3D,QTCN3D,QCCN3D,QRCN3D,QNIN3D,    &
            NC3D,NR3D,NI3D,NS3D,NG3D,NH3D,VI3D,VS3D,VG3D,FI3D,FS3D,    &
            AI3D,AS3D,AG3D,AH3D,I3M3D,TAIR,PRES,QVAP,ADAGR,AMI,BMI,AVI,&
            BVI,AMS,AVS,BVS,AMG,AVG,BVG,AVH,BVH,INHGR,DCLDMT,DCLDMC,   &
            DCLDMR,DTAIR,DPDT,DQVAP,QACac,QACar,VT_QC,VT_QR,VT_QI,     &
            VT_QS,VT_QG,VT_QH,VT_NC,VT_NR,VT_NI,VT_NS,VT_NG,VT_NH,     &
            VT_VI,VT_VS,VT_VG,VT_FI,VT_FS,VT_AI,VT_AS,VT_AG,VT_AH,     &
            VTI3M,TK0,P40,RHO,CPM,GQCTR,ESW,ESI,QVSW,QVSI,SSRW,SSRI,   &
            XXLV,XXLS,XXLF,XDNC,XDNR,IPF,IPG,I3M0,ZETA,BEST,MVRC,MVRR, &
            SIGC,MNRC,KDX,BDR,BDI,BDS,BDG,BDH,R3M3D,S3M3D,G3M3D,H3M3D, &
            FDI,FDS,FDG,FDH,RHOI,RHOS,RHOG,IASPR,SASPR,MVDC,MVDR,MVDI, &
            MVDS,MVDG,MVDH,EFRC,EFRR,AFAC,AFAR,AFAI,AFAS,AFAG,AFAH,    &
            LAMC,LAMR,LAMI,LAMS,LAMG,LAMH,LTK,LQC,LQR,LQI,LQS,LQG,LQH, &
            MDI,MDS,MDG
      REAL :: SR,ICENCV,CLODNCV,RAINNC,RAINNCV,SNOWNC,SNOWNCV,GRAPNC,  &
              GRAPNCV,HAILNC,HAILNCV,DT,DTMN,SDTL,SDTS,DTL,DTS,CNDMAX, &
              TMP,XMASS,TEMPC,TEMPR,TEMPT,FINCV,FSNCV,VINCV,VSNCV,     &
              VGNCV,AINCV,ASNCV,AGNCV,AHNCV,I3MNCV
      REAL, DIMENSION(KTS:KTE,3) :: VTMEAN
      LOGICAL, DIMENSION(KTS:KTE,NAER) :: DO_FR                         ! re-construct r-cutoff from XA & XC for once only
      REAL, DIMENSION(KTS:KTE,NAER) :: RX0                              ! CUTOFF RADIUS (m)
      REAL, DIMENSION(KTS:KTE,NAERT) :: AERO                            ! AEROSOL MIXING RATIO
      REAL, DIMENSION(MAER) :: ABCD
      REAL, DIMENSION(NCCN,KTS:KTE,NAER) :: ZCCNS                       ! AEROSOL # OF EACH MODE (#/kg)

!----- AT SUBSATURATION, REMOVE SMALL AMOUNTS OF HYDROMETEORS ----------
      DO K = KTS,KTE
         AERO(K,1) = MAX(0.,QDCN3D(K))
         AERO(K,2) = MAX(0.,QTCN3D(K))
         AERO(K,3) = MAX(0.,QCCN3D(K))
         AERO(K,4) = MAX(0.,QRCN3D(K))
         AERO(K,5) = MAX(0.,QNIN3D(K))
         TC3D(K)   = TK3D(K)-TK0C
         ESW(K)    = MIN(0.99*P3D(K),POLYSVP(TK3D(K),0))
         ESI(K)    = MIN(0.99*P3D(K),POLYSVP(TK3D(K),1))
         IF (ESI(K).GT.ESW(K)) ESI(K) = ESW(K)
         SSRW(K)   = QV3D(K)/(0.622*ESW(K)/(P3D(K)-ESW(K)))-1.
         SSRI(K)   = QV3D(K)/(0.622*ESI(K)/(P3D(K)-ESI(K)))-1.
         XXLV(K)   = 3.1484E6-2370.*TK3D(K)
         XXLS(K)   = 3.15E6-2370.*TK3D(K)+0.3337E6
         XXLF(K)   = 2836310.8-(3.1484E6-2370.*TK3D(K))
         CPM(K)    = CP*(1.+0.887*QV3D(K))
         HID(K)    = MAX(MIN(NINT(ABS(TC3D(K))/0.25),120),0)
!-------------------------------------------------------------------------
         IF (QC3D(K).GE.QSMALL.AND.NC3D(K).LT.NSMALL) THEN
            LTK(K)  = LOG(TK3D(K))
            LQC(K)  = -1.*LOG(QC3D(K))
            NC3D(K) = EXP(DNC0+DNC1*LTK(K)+DNC2*LTK(K)**2.+DNC3*       &
                      LTK(K)**3.-0.25*LQC(K))
            IF (AFAC_3M.EQ.0) THEN
               AFAC(K) = AFAC0
            ELSEIF (AFAC_3M.EQ.1) THEN
               MVRC(K) = (QC3D(K)/NC3D(K)/C4PI3W)**THRD
               MVRC(K) = MIN(MAX(MVRC(K),RCMIN),RCMAX)
               EFRC(K) = EXP(EFC1+EFC2*LOG(NC3D(K))+EFC3*LOG(MVRC(K)))
               KDX(K)  = MAX(KCCMIN,MIN(KCCMAX,(MVRC(K)/EFRC(K))**3.))
!               KDX(K)  = QC3D(K)*V2M3/(8.*EFRC(K)**3.*NC3D(K)*RHOW)
               KDX(K)  = MIN(KCCMAX,MAX(KCCMIN,KDX(K)))
               AFAC(K) = (6.*KDX(K)-3.+SQRT(8.*KDX(K)+1.))/(2.-2.*     &
                         KDX(K))
               AFAC(K) = MIN(MAX(AFAC(K),AFAMIN),AFAMAX)
            ELSEIF (AFAC_3M.EQ.2) THEN
               SIGC(K) = EXP(SIG1+SIG2*LOG(NC3D(K))+SIG3*LOG(QC3D(K)))
               MNRC(K) = EXP(MNR1+MNR2*LOG(NC3D(K))+MNR3*LOG(QC3D(K)))
               AFAC(K) = MIN(MAX(SIGC(K)/MNRC(K),AFAMIN),AFAMAX)
            ENDIF
         ENDIF
         IF (QR3D(K).GE.QSMALL.AND.NR3D(K).LT.NSMALL) THEN
            LTK(K)  = LOG(TK3D(K))
            LQR(K)  = -1.*LOG(QR3D(K))
            NR3D(K) = EXP(-5793.7852+3191.1171*LTK(K)-582.73279*       &
                      LTK(K)**2.+35.346854*LTK(K)**3.-0.25*LQR(K))
            IF (AFAR_3M.EQ.0) THEN
               AFAR(K) = AFAR0
            ELSEIF (AFAR_3M.EQ.1) THEN
               MVRR(K) = (QR3D(K)/NR3D(K)/C4PI3W)**THRD
               MVRR(K) = MIN(RRMAX,MAX(RRMIN,MVRR(K)))
               EFRR(K) = EXP(EFR1+EFR2*LOG(NR3D(K))+EFR3*LOG(MVRR(K)))
               KDX(K)  = MAX(KCRMIN,MIN(KCRMAX,(MVRR(K)/EFRR(K))**3.))
!               KDX(K)  = QR3D(K)*V2M3/(8.*EFRR(K)**3.*NR3D(K)*RHOW)
               KDX(K)  = MIN(KCRMAX,MAX(KCRMIN,KDX(K)))
               AFAR(K) = (6.*KDX(K)-3.+SQRT(8.*KDX(K)+1.))/(2.-2.*     &
                         KDX(K))
               AFAR(K) = MIN(MAX(AFAR(K),AFAMIN),AFAMAX)
            ELSEIF (AFAR_3M.EQ.2) THEN
               BDR(K)  = (QR3D(K)*V2M3/RHOW/NR3D(K))**THRD
               BDR(K)  = MIN(MAX(BDR(K),DRMIN),DRMAX)
               AFAR(K) = MAX(AFAMIN,19.*TANH(0.6*(1.E3*BDR(K)-1.8))+17.)
            ENDIF
         ENDIF
         IF (QI3D(K).GE.QSMALL.AND.NI3D(K).LT.NSMALL) THEN
            IF (ICE_RHOI.EQ.0) THEN
               RHOI(K)  = RHOI0
               VI3D(K)  = 0.
            ELSEIF (ICE_RHOI.EQ.1) THEN
               QVSI(K)  = 0.622*ESI(K)/(P3D(K)-ESI(K))
               INHGR(K) = ITBLE(HID(K))
               RHOI(K)  = RHOI0*EXP(-3.*MAX((QV3D(K)-QVSI(K))-5.E-5,   &
                          0.)/INHGR(K))
               VI3D(K)  = QI3D(K)/RHOI(K)
            ELSEIF (ICE_RHOI.EQ.2) THEN
               RHOI(K)  = RHOI1
               VI3D(K)  = 0.
            ENDIF
            LTK(K) = LOG(TK3D(K))
            LQI(K) = -1.*LOG(QI3D(K))
            MDI(K) = EXP(-3.2653646+2.0539073*LTK(K)-0.25*LQI(K))/1.E3
            NI3D(K) = 1.E9*QI3D(K)*V2M3/RHOI(K)/MDI(K)**3.
            I3M3D(K) = QI3D(K)*V2M3/RHOI(K)
            IF (ICE_SHAPE.EQ.0) THEN
               FI3D(K)  = I3M3D(K)
            ELSEIF (ICE_SHAPE.EQ.1) THEN
               ADAGR(K) = (MAX(MIN(ITBLE(HID(K)),2.),0.5))**THRD
               ZETA(K)  = (ADAGR(K)-1.)/(ADAGR(K)+2.)
               I3M0(K)  = NI3D(K)*DI0**3.
               FI3D(K)  = (I3M3D(K)/I3M0(K))**ZETA(K)*I3M3D(K)
            ENDIF
            IF (AFAI_3M.EQ.0) THEN
               AFAI(K) = AFAI0
               AI3D(K) = 0.
            ELSE
               BDI(K)  = (I3M3D(K)/NI3D(K))**THRD*1.E3
               BDI(K)  = MIN(MAX(BDI(K),DIMIN*1.E3),DIMAX*1.E3)
               FDI(K)  = 7.4015986E-2+0.79866676*BDI(K)-9.4468892E-3*  &
                         LOG(NI3D(K))+0.38235092*BDI(K)**2.+           &
                         2.9811542E-4*LOG(NI3D(K))**2.+1.9052614E-2*   &
                         BDI(K)*LOG(NI3D(K))
               KDX(K)  = MAX(KCIMIN,MIN(KCIMAX,(BDI(K)/FDI(K))**3.))
               AFAI(K) = (6.*KDX(K)-3.+SQRT(8.*KDX(K)+1.))/(2.-2.*     &
                         KDX(K))
               AFAI(K) = MIN(MAX(AFAI(K),AFAMIN),AFAMAX)
!               AFAI(K) = MAX(AFAMIN,12.*TANH(0.7*(BDI(K)-1.7))+11.)
               IF (AFAI_3M.EQ.1) THEN
                  KDX(K)  = (AFAI(K)**2.+3.*AFAI(K)+2.)/(AFAI(K)**2.+  &
                            6.*AFAI(K)+9.)
                  AI3D(K) = (KDX(K)*NI3D(K)*I3M3D(K)**2.)**THRD
               ELSEIF (AFAI_3M.EQ.2) THEN
                  AI3D(K) = 0.
               ENDIF
            ENDIF
         ENDIF
         IF (QS3D(K).GE.QSMALL.AND.NS3D(K).LT.NSMALL) THEN
            IF (ICE_RHOS.EQ.0) THEN
               RHOS(K) = RHOS0
               VS3D(K) = 0.
            ELSE
               LTK(K) = LOG(TK3D(K))
               LQS(K) = -1.*LOG(QS3D(K))
               IF (TK3D(K).LT.TK0C) THEN
                  RHOS(K) = 15740.702-6098.0087*LTK(K)+503.33089*      &
                            LQS(K)+594.29913*LTK(K)**2.+1.9033961*     &
                            LQS(K)**2.-94.950429*LTK(K)*LQS(K)
               ELSE
                  RHOS(K) = EXP(-64808.666+23113.508*LTK(K)-36.46632*  &
                            LQS(K)-2060.6024*LTK(K)**2.-0.005729458*   &
                            LQS(K)**2.+6.5057411*LTK(K)*LQS(K))
               ENDIF
               RHOS(K) = MIN(RHOIMAX,MAX(RHOIMIN,RHOS(K)))
               IF (ICE_RHOS.EQ.1) THEN
                  VS3D(K) = QS3D(K)/RHOS(K)
               ELSEIF (ICE_RHOS.EQ.2) THEN
                  VS3D(K) = 0.
               ENDIF
            ENDIF
            LTK(K) = LOG(TK3D(K))
            LQS(K) = -1.*LOG(QS3D(K))
            MDS(K) = EXP(-123.23898+40.74706*LTK(K)-3.0333477*         &
                     LTK(K)**2.-0.31219981*LQS(K)+0.0012798222*        &
                     LQS(K)**2.)/1.E3
            NS3D(K) = 1.E9*QS3D(K)*V2M3/RHOS(K)/MDS(K)**3.
            S3M3D(K) = QS3D(K)*V2M3/RHOS(K)
            IF (AGG_SHAPE.EQ.0) THEN
               SASPR(K) = 1.
               FS3D(K)  = 0.
            ELSEIF (AGG_SHAPE.EQ.1) THEN
               SASPR(K) = 0.7
               FS3D(K) = SASPR(K)*S3M3D(K)
            ENDIF
            IF (AFAS_3M.EQ.0) THEN
               AFAS(K) = AFAS0
               AS3D(K) = 0.
            ELSE
               BDS(K) = (S3M3D(K)/NS3D(K))**THRD*1.E3
               BDS(K) = MIN(MAX(BDS(K),DSMIN*1.E3),DSMAX*1.E3)
               IF (TK3D(K).GE.TK0C) THEN
                  FDS(K) = -0.21911541+1.2739845*BDS(K)+0.10141003*    &
                           LOG(NS3D(K))+0.30063818*BDS(K)**2.-         &
                           4.3857765E-3*LOG(NS3D(K))**2.-0.078801732*  &
                           BDS(K)*LOG(NS3D(K))
               ELSE
                  IF (QC3D(K).GE.1.E-8) THEN
                     FDS(K) = -1.1527014+2.9067645*BDS(K)+0.25316062*  &
                              LOG(NS3D(K))-0.17768557*BDS(K)**2.-      &
                              0.013117292*LOG(NS3D(K))**2.-0.17020429* &
                              BDS(K)*LOG(NS3D(K))
                  ELSE
                     FDS(K) = -0.2813929+1.7275463*BDS(K)+0.045550156* &
                              LOG(NS3D(K))-0.16526226*BDS(K)**2.-      &
                              1.7699916E-3*LOG(NS3D(K))**2.-           &
                              4.6441257E-2*BDS(K)*LOG(NS3D(K))
                  ENDIF
               ENDIF
               KDX(K)  = MAX(KCSMIN,MIN(KCSMAX,(BDS(K)/FDS(K))**3.))
               AFAS(K) = (6.*KDX(K)-3.+SQRT(8.*KDX(K)+1.))/(2.-2.*     &
                         KDX(K))
               AFAS(K) = MIN(MAX(AFAS(K),AFAMIN),AFAMAX)
!               AFAS(K) = MAX(AFAMIN,4.5*TANH(0.5*(BDS(K)-5.))+5.5)
               IF (AFAS_3M.EQ.1) THEN
                  KDX(K)  = (AFAS(K)**2.+3.*AFAS(K)+2.)/(AFAS(K)**2.+  &
                            6.*AFAS(K)+9.)
                  AS3D(K) = (KDX(K)*NS3D(K)*S3M3D(K)**2.)**THRD
               ELSEIF (AFAS_3M.EQ.2) THEN
                  AS3D = 0.
               ENDIF
            ENDIF
         ENDIF
         IF (QG3D(K).GE.QSMALL.AND.NG3D(K).LT.NSMALL) THEN
            IF (ICE_RHOG.EQ.0) THEN
               RHOG(K) = RHOG1
               VG3D(K) = 0.
            ELSEIF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
               LTK(K) = LOG(TK3D(K))
               LQG(K) = -1.*LOG(QG3D(K))
               IF (TK3D(K).LT.TK0C) THEN
                  RHOG(K) = EXP(-509.23219+187.98419*LTK(K)+1.5493489* &
                            LQG(K)-17.147561*LTK(K)**2.-0.005829723*   &
                            LQG(K)**2.-0.26664109*LTK(K)*LQG(K))
               ELSE
                  RHOG(K) = EXP(-2894.6795+1050.5173*LTK(K)-11.536497* &
                            LQG(K)-95.054373*LTK(K)**2.-0.001986964*   &
                            LQG(K)**2.+2.0502147*LTK(K)*LQG(K))
               ENDIF
               VG3D(K) = QG3D(K)/RHOG(K)
            ENDIF
            LTK(K)  = LOG(TK3D(K))
            LQG(K)  = -1.*LOG(QG3D(K))
            MDG(K)  = EXP(-2205.8027+1225.8046*LTK(K)-226.27995*       &
                      LTK(K)**2.+13.929644*LTK(K)**3.-0.25*LQG(K))/1.E3
            NG3D(K) = 1.E9*QG3D(K)*V2M3/RHOG(K)/MDG(K)**3.
            G3M3D(K) = QG3D(K)*V2M3/RHOG(K)
            IF (AFAG_3M.EQ.0) THEN
               AFAG(K) = AFAG0
               AG3D(K) = 0.
            ELSE
               BDG(K) = (G3M3D(K)/NG3D(K))**THRD*1.E3
               BDG(K) = MIN(MAX(BDG(K),DGMIN*1.E3),DGMAX*1.E3)
               IF (TK3D(K).GE.TK0C) THEN
                  FDG(K) = 0.58006354+0.79661229*BDG(K)-0.18394382*    &
                           LOG(NG3D(K))+6.7371044E-2*BDG(K)**2.+       &
                           9.832945E-3*LOG(NG3D(K))**2.+0.12433055*    &
                           BDG(K)*LOG(NG3D(K))
               ELSE
                  IF (QC3D(K).GE.1.E-8) THEN
                     FDG(K) = 0.17363469+1.5044291*BDG(K)-0.050639722* &
                              LOG(NG3D(K))+1.5101052E-2*BDG(K)**2.+    &
                              2.5974719E-3*LOG(NG3D(K))**2.+0.01961464*&
                              BDG(K)*LOG(NG3D(K))
                  ELSE
                     FDG(K) = 0.59259317-0.89933515*BDG(K)+2.0893032*  &
                              BDG(K)**2.-0.50305755*BDG(K)**3.-        &
                              2.2446793E-2*LOG(NG3D(K))+2.7589047E-3*  &
                              LOG(NG3D(K))**2.
                  ENDIF
               ENDIF
               KDX(K)  = MAX(KCGMIN,MIN(KCGMAX,(BDG(K)/FDG(K))**3.))
               AFAG(K) = (6.*KDX(K)-3.+SQRT(8.*KDX(K)+1.))/(2.-2.*     &
                         KDX(K))
               AFAG(K) = MIN(MAX(AFAG(K),AFAMIN),AFAMAX)
!               AFAG(K) = MAX(AFAMIN,5.5*TANH(0.7*(BDG(K)-4.5))+8.5)
               IF (AFAG_3M.EQ.1) THEN
                  KDX(K)  = (AFAG(K)**2.+3.*AFAG(K)+2.)/(AFAG(K)**2.+  &
                            6.*AFAG(K)+9.)
                  AG3D(K) = (KDX(K)*NG3D(K)*G3M3D(K)**2.)**THRD
               ELSEIF (AFAG_3M.EQ.2) THEN
                  AG3D = 0.
               ENDIF
            ENDIF
         ENDIF
         IF (QH3D(K).GE.QSMALL.AND.NH3D(K).LT.NSMALL) THEN
            LTK(K) = LOG(TK3D(K))
            LQH(K) = -1.*LOG(QH3D(K))
            NH3D(K) = EXP(22.929406-4.2328364*LTK(K)+0.30647567*       &
                      LTK(K)**2.-0.009233271*LTK(K)**3.-0.25*LQH(K))
            H3M3D(K) = QH3D(K)*V2M3/RHOH
            IF (AFAH_3M.EQ.0) THEN
               AFAH(K) = AFAH0
               AH3D(K) = 0.
            ELSE
               BDH(K) = (H3M3D(K)/NH3D(K))**THRD*1.E3
               BDH(K) = MIN(MAX(BDH(K),DHMIN*1.E3),DHMAX*1.E3)
               IF (TK3D(K).GE.TK0C) THEN
                  FDH(K) = 1.157754+0.37852874*BDH(K)-0.11129737*      &
                           LOG(NH3D(K))+0.13929599*BDH(K)**2.+         &
                           8.1105237E-3*LOG(NH3D(K))**2.+5.7432113E-2* &
                           BDH(K)*LOG(NH3D(K))
               ELSE
                  FDH(K) = -0.48246793+2.0407077*BDH(K)+0.022262969*   &
                           LOG(NH3D(K))-0.158389*BDH(K)**2.-           &
                           5.5545804E-3*LOG(NH3D(K))**2.+2.9443577E-2* &
                           BDH(K)*LOG(NH3D(K))
               ENDIF
               KDX(K)  = MAX(KCHMIN,MIN(KCHMAX,(BDH(K)/FDH(K))**3.))
               AFAH(K) = (6.*KDX(K)-3.+SQRT(8.*KDX(K)+1.))/(2.-2.*     &
                         KDX(K))
               AFAH(K) = MIN(MAX(AFAH(K),AFAMIN),AFAMAX)
!               AFAH(K) = MAX(AFAMIN,3.7*TANH(0.3*(BDH(K)-9.))+6.5)
               IF (AFAH_3M.EQ.1) THEN
                  KDX(K)  = (AFAH(K)**2.+3.*AFAH(K)+2.)/(AFAH(K)**2.+  &
                            6.*AFAH(K)+9.)
                  AH3D(K) = (KDX(K)*NH3D(K)*H3M3D(K)**2.)**THRD
               ELSEIF (AFAH_3M.EQ.2) THEN
                  AH3D(K) = 0.
               ENDIF
            ENDIF
         ENDIF
!-----------------------------------------------------------------------------
         IF (SSRW(K).LT.-1.E-1) THEN
            IF (QR3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QR3D(K)
               TK3D(K) = TK3D(K)-QR3D(K)*XXLV(K)/CPM(K)
               QR3D(K) = 0.; NR3D(K) = 0.
            ENDIF
            IF (QC3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QC3D(K)
               TK3D(K) = TK3D(K)-QC3D(K)*XXLV(K)/CPM(K)
               QC3D(K) = 0.; NC3D(K) = 0.
            ENDIF
         ENDIF
         IF (SSRI(K).LT.-1.E-1) THEN
            IF (QI3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QI3D(K)
               TK3D(K) = TK3D(K)-QI3D(K)*XXLS(K)/CPM(K)
               QI3D(K) = 0.; NI3D(K) = 0.; I3M3D(K) = 0.; FI3D(K) = 0.
               VI3D(K) = 0.; AI3D(K) = 0.
            ENDIF
            IF (QS3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QS3D(K)
               TK3D(K) = TK3D(K)-QS3D(K)*XXLS(K)/CPM(K)
               QS3D(K) = 0.; NS3D(K) = 0.; VS3D(K) = 0.; AS3D(K) = 0.
               FS3D(K) = 0.
            ENDIF
            IF (QG3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QG3D(K)
               TK3D(K) = TK3D(K)-QG3D(K)*XXLS(K)/CPM(K)
               QG3D(K) = 0.; NG3D(K) = 0.; VG3D(K) = 0.; AG3D(K) = 0.
            ENDIF
            IF (QH3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QH3D(K)
               TK3D(K) = TK3D(K)-QH3D(K)*XXLS(K)/CPM(K)
               QH3D(K) = 0.; NH3D(K) = 0.; AH3D(K) = 0.
            ENDIF
         ENDIF
!----- MELT VERY SMALL SNOW/GRAUPEL/HAIL MIXING RATIOS, ADD TO RAIN ----
         IF (TK3D(K).GE.TK0C.AND.QS3D(K).LT.QLIMIT) THEN
            QR3D(K) = QR3D(K)+QS3D(K)
            NR3D(K) = NR3D(K)+NS3D(K)
            TK3D(K) = TK3D(K)-QS3D(K)*XXLF(K)/CPM(K)
            QS3D(K) = 0.; NS3D(K) = 0.; VS3D(K) = 0.; AS3D(K) = 0.
            FS3D(K) = 0.
         ENDIF
         IF (TK3D(K).GE.TK0C.AND.QG3D(K).LT.QLIMIT) THEN
            QR3D(K) = QR3D(K)+QG3D(K)
            NR3D(K) = NR3D(K)+NG3D(K)
            TK3D(K) = TK3D(K)-QG3D(K)*XXLF(K)/CPM(K)
            QG3D(K) = 0.; NG3D(K) = 0.; VG3D(K) = 0.; AG3D(K) = 0.
         ENDIF
         IF (TK3D(K).GE.TK0C.AND.QH3D(K).LT.QLIMIT) THEN
            QR3D(K) = QR3D(K)+QH3D(K)
            NR3D(K) = NR3D(K)+NH3D(K)
            TK3D(K) = TK3D(K)-QH3D(K)*XXLF(K)/CPM(K)
            QH3D(K) = 0.; NH3D(K) = 0.; AH3D(K) = 0.
         ENDIF
      ENDDO
!------- INITIALIZE VARIABLE,ONLY T&QV ARE TREATED DIFFERENTLY ---------
      DO K = KTS,KTE
         TAIR(K)  = TK3D(K)
         QVAP(K)  = QV3D(K)
         DTAIR(K) = -W3D(K)*G/CP                                        ! Lagrangian parcel dry adiabatic lapse rate
         DQVAP(K) = 0.
         PRES(K)  = P3D(K)
         TK0(K)   = TAIR(K)-DTAIR(K)*DT                                 ! diabatically decend parcel
         P40(K)   = PRES(K)*(TK0(K)/TAIR(K))**(CP/R)
         TAIR(K)  = TK0(K)
         IV1 = 1
         DO IV = 1,NAER
            IV2 = IV1+1
            DO_FR(K,IV) = .TRUE.                                        ! FIND CUT OFF RADIUS
            RX0(K,IV) = 99.                                             ! initial R cutoff
            IF (NAERN(IV).GE.2) THEN                                    ! have info of dry and total aerosol 
               AERO(K,IV1) = MIN(AERO(K,IV1),AERO(K,IV2))               ! dry aerosol < total aerosol
               DO IM = 1,NCCN                                           ! calculate CCN # [#/kg] from CCN mass
                  ZCCNS(IM,K,IV) = AERO(K,IV2)*WMAS(IM,IV)*RFACT(IM,IV)
               ENDDO
            ELSE                                                        ! only info available is total aerosol 
               DO IM = 1,NCCN                                           ! calculate CCN # [#/kg] from CCN mass
                  ZCCNS(IM,K,IV) = AERO(K,IV1)*WMAS(IM,IV)*RFACT(IM,IV)
               ENDDO
            ENDIF
            IV1 = IV1+NAERN(IV)
         ENDDO
      ENDDO                                                             ! INITILIZATION LOOPS

      SDTL = 0.                                                         ! initial integrated largedt timestep
      DO IT = 1,INT((DT-DT20S*0.01)/DT20S)+1
         DTL = MIN(DT20S,DT-SDTL)                                       ! initial largedt timestep
         DO K = KTS,KTE
            DTS  = DTL                                                  ! initial smalldt timestep (could be changed later)
            SDTS = 0.                                                   ! initial integrated smalldt timestep
            TMP  = W3D(K)*G/CP/TK0(K)                                   ! 1-TAIR/TK0
            XDNC(K) = 0.; XDNR(K) = 0.; GQCTR(K) = 0.; ADAGR(K) = 1.
            RHOI(K) = RHOI0; RHOS(K) = RHOS0; RHOG(K) = RHOG0
            AFAC(K) = AFAC0; AFAR(K) = AFAR0; AFAI(K) = AFAI0
            AFAS(K) = AFAS0; AFAG(K) = AFAG0; AFAH(K) = AFAH0
            AVI(K) = AVI0; BVI(K) = BVI0; AVS(K) = AVS0; BVS(K) = BVS0
            AVG(K) = AVG0; BVG(K) = BVG0; AVH(K) = AVH0; BVH(K) = BVH0
            AMI(K) = AMI0; BMI(K) = BMI0; AMS(K) = AMS0; AMG(K) = AMG0
            IASPR(K) = 1.; SASPR(K) = 1.; QACac(K) = 0.; QACar(K) = 0.
 111        CONTINUE 

            PRES(K) = P40(K)*(1.-TMP*(SDTL+SDTS))**(CP/R) 
            DPDT(K) = -G*W3D(K)*PRES(K)/(R*TK0(K)*(1.-TMP*(SDTL+SDTS)))
            ESW(K)  = MIN(0.99*PRES(K),POLYSVP(TAIR(K),0))
            ESI(K)  = MIN(0.99*PRES(K),POLYSVP(TAIR(K),1))
            IF (ESI(K).GT.ESW(K)) ESI(K) = ESW(K)
            QVSW(K) = 0.622*ESW(K)/(PRES(K)-ESW(K))
            QVSI(K) = 0.622*ESI(K)/(PRES(K)-ESI(K))
            SSRW(K) = QVAP(K)/QVSW(K)-1.
            SSRI(K) = QVAP(K)/QVSI(K)-1.
            XXLV(K) = 3.1484E6-2370.*TAIR(K)
            CPM(K)  = CP*(1.+0.887*QVAP(K))
            RHO(K)  = PRES(K)/(TAIR(K)*(1.+0.61*QVAP(K)))/R             ! [KG/M**3]
            CNDMAX  = (QVAP(K)-QVSW(K))/(1.+XXLV(K)**2.*QVAP(K)/       &
                      (CPM(K)*RV*TAIR(K)**2.))
!--------------------------- CCN ACTIVATION. ---------------------------
            IF (SSRW(K).GT.RSMALL.AND.SSRW(K).GT.S3D(K)) THEN 
               DCLDMT(K) = 0.; DCLDMC(K) = 0.; DCLDMR(K) = 0.
               IV1 = 1
               DO IV = 1,NAER
                  DO J = 1,NAERN(IV)
                     ABCD(J) = AERO(K,IV1-1+J)
                  ENDDO
                  IF (AERO(K,IV1).GT.1.E-20.AND.NAERN(IV).GE.2.AND.    &
                     W3D(K).GT.1.E-5) THEN
                     CALL ACTIVA(TAIR(K),W3D(K),NC3D(K),NR3D(K),ABCD,  &
                          QACac(K),QACar(K),RX0(K,IV),ZCCNS(1,K,IV),   &
                          SSRW(K),IV,DO_FR(K,IV))
                     DCLDMC(K) = DCLDMC(K)+QACac(K)
                     DCLDMR(K) = DCLDMR(K)+QACar(K)
                     DO J = 1,NAERN(IV)
                        AERO(K,IV1-1+J) = ABCD(J)
                     ENDDO
                  ENDIF
                  IV1 = IV1+NAERN(IV)
               ENDDO
               DCLDMT(K) = DCLDMC(K)+DCLDMR(K)
               IF (DCLDMT(K).GT.RLIMIT) THEN
                  IF (DCLDMT(K).GT.CNDMAX) THEN
                     DCLDMC(K) = DCLDMC(K)*(CNDMAX/DCLDMT(K))
                     DCLDMR(K) = DCLDMR(K)*(CNDMAX/DCLDMT(K))
                     DCLDMT(K) = DCLDMC(K)+DCLDMR(K)
                  ENDIF
                  QVAP(K) = MAX(0.,QVAP(K)-DCLDMT(K))
                  QC3D(K) = MAX(0.,QC3D(K)+DCLDMC(K))
                  QR3D(K) = MAX(0.,QR3D(K)+DCLDMR(K))
                  CPM(K)  = CP*(1.+0.887*QVAP(K))
                  XXLV(K) = 3.1484E6-2370.*TAIR(K)
                  TAIR(K) = TAIR(K)+XXLV(K)*DCLDMT(K)/CPM(K)
               ENDIF
               ESW(K)  = MIN(0.99*PRES(K),POLYSVP(TAIR(K),0))
               ESI(K)  = MIN(0.99*PRES(K),POLYSVP(TAIR(K),1))
               IF (ESI(K).GT.ESW(K)) ESI(K) = ESW(K)
               SSRW(K) = QVAP(K)/(0.622*ESW(K)/(PRES(K)-ESW(K)))-1.
               SSRI(K) = QVAP(K)/(0.622*ESI(K)/(PRES(K)-ESI(K)))-1.
               RHO(K)  = PRES(K)/(TAIR(K)*(1.+0.61*QVAP(K)))/R
            ENDIF 
            S3D(K) = MAX(S3D(K),SSRW(K))
!------------------------- ICE NUCLEATION ------------------------------
            IF (TAIR(K).LT.(TK0C-2.).AND.SSRI(K).GT.1.E-4) THEN
               CALL ICENU(TAIR(K),PRES(K),DTS,RHO(K),QVAP(K),QI3D(K),  &
                    NI3D(K),VI3D(K),FI3D(K),AI3D(K),I3M3D(K),AERO(K,5))
               ESW(K)  = MIN(0.99*PRES(K),POLYSVP(TAIR(K),0))
               ESI(K)  = MIN(0.99*PRES(K),POLYSVP(TAIR(K),1))
               IF (ESI(K).GT.ESW(K)) ESI(K) = ESW(K)
               SSRW(K) = QVAP(K)/(0.622*ESW(K)/(PRES(K)-ESW(K)))-1.
               SSRI(K) = QVAP(K)/(0.622*ESI(K)/(PRES(K)-ESI(K)))-1.
               RHO(K)  = PRES(K)/(TAIR(K)*(1.+0.61*QVAP(K)))/R
            ENDIF
!-----------MICROPHYSICAL TENDENCY CALCULATION IN SMALL TIMESTEP -------
            IF (QC3D(K).GE.QSMALL.OR.QR3D(K).GE.QSMALL.OR.QI3D(K).GE.  &
               QSMALL.OR.QS3D(K).GE.QSMALL.OR.QG3D(K).GE.QSMALL.OR.    &
               QH3D(K).GE.QSMALL) THEN
               CALL SMALL_DT(DTL,DTS,SDTS,DTAIR(K),DQVAP(K),DPDT(K),   &
                    RHO(K),TAIR(K),PRES(K),QVAP(K),QC3D(K),QR3D(K),    &
                    QI3D(K),QS3D(K),QG3D(K),QH3D(K),NC3D(K),NR3D(K),   &
                    NI3D(K),NS3D(K),NG3D(K),NH3D(K),VI3D(K),VS3D(K),   &
                    VG3D(K),FI3D(K),FS3D(K),AI3D(K),AS3D(K),AG3D(K),   &
                    AH3D(K),I3M3D(K),SASPR(K),XDNC(K),XDNR(K),GQCTR(K))
            ENDIF
!----------------- UPDATE TIME & PROGNOSTIC VARIABLES ------------------
            TAIR(K) = TAIR(K)+DTAIR(K)*DTS
            QVAP(K) = QVAP(K)+DQVAP(K)*DTS
            SDTS    = SDTS+DTS

            IF (SDTS.GE.DTL) GOTO 333
               GOTO 111
 333        CONTINUE
!----------------- RAIN DROP DEACTIVATION. -----------------------------
            IF (XDNR(K).GT.1.) THEN
               IV0 = 0
               DO IV = 1,NAER
                  IF (NAERN(IV).EQ.4) THEN
                     IV3 = IV0+3
                     IV4 = IV0+4
                     IF (NR3D(K).GT.1.) THEN
                        XMASS = AERO(K,IV4)*MIN(1.,XDNR(K)/NR3D(K))
                     ELSE
                        XMASS = AERO(K,IV4)
                     ENDIF 
                     AERO(K,IV3) = AERO(K,IV3)+XMASS
                     AERO(K,IV4) = AERO(K,IV4)-XMASS
                  ENDIF
                  IV0 = IV0+NAERN(IV)
               ENDDO
            ENDIF
!----------------- CLOUD DROP DEACTIVATION. ----------------------------
            IF (XDNC(K).GT.1.) THEN 
               IV1 = 1
               DO IV = 1,NAER
                  IV2 = IV1+1
                  IV3 = IV2+1
                  IF (NAERN(IV).GE.3) THEN
                     IF (NC3D(K).GT.1.) THEN
                        XMASS = AERO(K,IV3)*MIN(1.,XDNC(K)/NC3D(K))
                     ELSE
                        XMASS = AERO(K,IV3)
                     ENDIF 
                     AERO(K,IV1) = AERO(K,IV1)+XMASS
                     AERO(K,IV3) = AERO(K,IV3)-XMASS
                     AERO(K,IV1) = MIN(AERO(K,IV1),AERO(K,IV2))
                  ELSEIF (NAERN(IV).EQ.2) THEN
                     CALL DEACTIVA(XDNC(K),QC3D(K),NC3D(K),AERO(K,IV1),&
                          AERO(K,IV2),AERO(K,IV3),RX0(K,IV),           &
                          ZCCNS(1,K,IV),IV,DO_FR(K,IV))
                  ELSE
                     XMASS = AERO(K,IV1)*MIN(1.,XDNC(K)/               &
                            (ZCCNS(2,K,IV)+ZCCNS(3,K,IV)))              ! COARSE AND ACCUMULATION MODE
                     AERO(K,IV1) = AERO(K,IV1)+XMASS
                  ENDIF
                  IV1 = IV1+NAERN(IV)
               ENDDO 
            ENDIF 
            PRES(K) = P40(K)*(1.-TMP*(SDTL+SDTS))**(CP/R) 
            RHO(K)  = PRES(K)/(TAIR(K)*(1.+0.61*QVAP(K)))/R
         ENDDO
!-----------MICROPHYSICAL TENDENCY CALCULATION IN LARGE TIMESTEP -------
         DO K = KTS,KTE
            IF (QC3D(K).GE.QSMALL.OR.QR3D(K).GE.QSMALL.OR.QI3D(K).GE.  &
               QSMALL.OR.QS3D(K).GE.QSMALL.OR.QG3D(K).GE.QSMALL.OR.    &
               QH3D(K).GE.QSMALL) THEN
               CALL LARGE_DT(DTL,TAIR(K),QVAP(K),PRES(K),RHO(K),       &
                    QC3D(K),QR3D(K),QI3D(K),QS3D(K),QG3D(K),QH3D(K),   &
                    NC3D(K),NR3D(K),NI3D(K),NS3D(K),NG3D(K),NH3D(K),   &
                    VI3D(K),VS3D(K),VG3D(K),FI3D(K),FS3D(K),AI3D(K),   &
                    AS3D(K),AG3D(K),AH3D(K),I3M3D(K),SASPR(K),GQCTR(K))
            ENDIF
            IV0 = 0
            DO IV = 1,NAER                                              ! for all aerosol compoment
               IF (NAERN(IV).GE.4) THEN                                 ! with aerosol in cloud/precip. recored
                  IV3 = IV0+3                                           ! aerosols in clouds ll
                  IV4 = IV0+4                                           ! aerosols in rains
                  IF (GQCTR(K).GT.0.) THEN                              ! + from cloud to precip.
                      XMASS = GQCTR(K)*AERO(K,IV3)                      ! + from cloud to precip.
                  ELSE                                                  ! - from precip. to cloud
                      XMASS = GQCTR(K)*AERO(K,IV4)                      ! - from precip. to cloud
                  ENDIF
                  AERO(K,IV3) = AERO(K,IV3)-XMASS                       ! update aerosol in cloud
                  AERO(K,IV4) = AERO(K,IV4)+XMASS                       ! update aerosol in precipi.
               ENDIF
               IV0 = IV0+NAERN(IV)
            ENDDO
         ENDDO 
         SDTL = SDTL+DTL
      ENDDO                                                             ! FOR IT LOOPS
!----- AT SUBSATURATION, REMOVE SMALL AMOUNTS OF HYDROMETEORS ----------
      DO K = KTS,KTE
         ESW(K)  = MIN(0.99*PRES(K),POLYSVP(TAIR(K),0))
         ESI(K)  = MIN(0.99*PRES(K),POLYSVP(TAIR(K),1))
         IF (ESI(K).GT.ESW(K)) ESI(K) = ESW(K)
         SSRW(K) = QVAP(K)/(0.622*ESW(K)/(PRES(K)-ESW(K)))-1.
         SSRI(K) = QVAP(K)/(0.622*ESI(K)/(PRES(K)-ESI(K)))-1.
         XXLV(K) = 3.1484E6-2370.*TAIR(K)
         XXLS(K) = 3.15E6-2.37E3*TAIR(K)+0.3337E6
         CPM(K)  = CP*(1.+0.887*QVAP(K))
         IF (SSRW(K).LT.-1.E-1) THEN
            IF (QR3D(K).LT.QSMAL1) THEN
               QVAP(K) = QVAP(K)+QR3D(K)
               TAIR(K) = TAIR(K)-QR3D(K)*XXLV(K)/CPM(K)
               QR3D(K) = 0.; NR3D(K) = 0.
            ENDIF
            IF (QC3D(K).LT.QSMAL1) THEN
               QVAP(K) = QVAP(K)+QC3D(K)
               TAIR(K) = TAIR(K)-QC3D(K)*XXLV(K)/CPM(K)
               QC3D(K) = 0.; NC3D(K) = 0.
            ENDIF
         ENDIF
         IF (SSRI(K).LT.-1.E-1) THEN
            IF (QI3D(K).LT.QSMAL1) THEN
               QVAP(K) = QVAP(K)+QI3D(K)
               TAIR(K) = TAIR(K)-QI3D(K)*XXLS(K)/CPM(K)
               QI3D(K) = 0.; NI3D(K) = 0.; I3M3D(K) = 0.; FI3D(K) = 0.
               VI3D(K) = 0.; AI3D(K) = 0.
            ENDIF
            IF (QS3D(K).LT.QSMAL1) THEN
               QVAP(K) = QVAP(K)+QS3D(K)
               TAIR(K) = TAIR(K)-QS3D(K)*XXLS(K)/CPM(K)
               QS3D(K) = 0.; NS3D(K) = 0.; VS3D(K) = 0.; AS3D(K) = 0.
               FS3D(K) = 0.
            ENDIF
            IF (QG3D(K).LT.QSMAL1) THEN
               QVAP(K) = QVAP(K)+QG3D(K)
               TAIR(K) = TAIR(K)-QG3D(K)*XXLS(K)/CPM(K)
               QG3D(K) = 0.; NG3D(K) = 0.; VG3D(K) = 0.; AG3D(K) = 0.
            ENDIF
            IF (QH3D(K).LT.QSMAL1) THEN
               QVAP(K) = QVAP(K)+QH3D(K)
               TAIR(K) = TAIR(K)-QH3D(K)*XXLS(K)/CPM(K)
               QH3D(K) = 0.; NH3D(K) = 0.; AH3D(K) = 0.
            ENDIF
         ENDIF
         IF (TAIR(K).GT.TK0C.AND.QI3D(K).GT.0.) THEN
            QR3D(K) = QR3D(K)+QI3D(K)
            TAIR(K) = TAIR(K)-QI3D(K)*XXLF(K)/CPM(K)
            NR3D(K) = NR3D(K)+NI3D(K)
            QI3D(K) = 0.; NI3D(K) = 0.; I3M3D(K) = 0.; FI3D(K) = 0.
            VI3D(K) = 0.; AI3D(K) = 0.
         ENDIF
         IF (QC3D(K).LT.QSMALL.OR.NC3D(K).LT.NSMALL) THEN
            QVAP(K) = QVAP(K)+QC3D(K)
            TAIR(K) = TAIR(K)-QC3D(K)*XXLV(K)/CPM(K)
            QC3D(K) = 0.; NC3D(K) = 0.
         ENDIF
         IF (QR3D(K).LT.QSMALL.OR.NR3D(K).LT.NSMALL) THEN
            QVAP(K) = QVAP(K)+QR3D(K)
            TAIR(K) = TAIR(K)-QR3D(K)*XXLV(K)/CPM(K)
            QR3D(K) = 0.; NR3D(K) = 0.
         ENDIF
         IF (QI3D(K).LT.QSMALL.OR.NI3D(K).LT.NSMALL) THEN
            QVAP(K) = QVAP(K)+QI3D(K)
            TAIR(K) = TAIR(K)-QI3D(K)*XXLS(K)/CPM(K)
            QI3D(K) = 0.; NI3D(K) = 0.; I3M3D(K) = 0.; FI3D(K) = 0.
            VI3D(K) = 0.; AI3D(K) = 0.
         ENDIF
         IF (QS3D(K).LT.QSMALL.OR.NS3D(K).LT.NSMALL) THEN
            QVAP(K) = QVAP(K)+QS3D(K)
            TAIR(K) = TAIR(K)-QS3D(K)*XXLS(K)/CPM(K)
            QS3D(K) = 0.; NS3D(K) = 0.; VS3D(K) = 0.; AS3D(K) = 0.
            FS3D(K) = 0.
         ENDIF
         IF (QG3D(K).LT.QSMALL.OR.NG3D(K).LT.NSMALL) THEN
            QVAP(K) = QVAP(K)+QG3D(K)
            TAIR(K) = TAIR(K)-QG3D(K)*XXLS(K)/CPM(K)
            QG3D(K) = 0.; NG3D(K) = 0.; VG3D(K) = 0.; AG3D(K) = 0.
         ENDIF
         IF (QH3D(K).LT.QSMALL.OR.NH3D(K).LT.NSMALL) THEN
            QVAP(K) = QVAP(K)+QH3D(K)
            TAIR(K) = TAIR(K)-QH3D(K)*XXLS(K)/CPM(K)
            QH3D(K) = 0.; NH3D(K) = 0.; AH3D(K) = 0.
         ENDIF
         RHO(K) = PRES(K)/(TAIR(K)*(1.+0.61*QVAP(K)))/R
         VT_QC(K) = 0.; VT_NC(K) = 0.; VT_QR(K) = 0.; VT_NR(K) = 0.
         VT_QI(K) = 0.; VT_NI(K) = 0.; VT_QS(K) = 0.; VT_NS(K) = 0.
         VT_QG(K) = 0.; VT_NG(K) = 0.; VT_QH(K) = 0.; VT_NH(K) = 0.
         VT_VI(K) = 0.; VT_VS(K) = 0.; VT_VG(K) = 0.; VT_FI(K) = 0.
         VT_FS(K) = 0.; VT_AI(K) = 0.; VT_AS(K) = 0.; VT_AG(K) = 0.
         VT_AH(K) = 0.; VTI3M(K) = 0.
         IF (QC3D(K).GE.QSMALL.OR.QR3D(K).GE.QSMALL.OR.QI3D(K).GE.     &
            QSMALL.OR.QS3D(K).GE.QSMALL.OR.QG3D(K).GE.QSMALL.OR.       &
            QH3D(K).GE.QSMALL) THEN
            CALL SEDI_FALL(TAIR(K),PRES(K),QVAP(K),QC3D(K),QR3D(K),    &
                 QI3D(K),QS3D(K),QG3D(K),QH3D(K),NC3D(K),NR3D(K),      &
                 NI3D(K),NS3D(K),NG3D(K),NH3D(K),VI3D(K),VS3D(K),      &
                 VG3D(K),FI3D(K),FS3D(K),AI3D(K),AS3D(K),AG3D(K),      &
                 AH3D(K),I3M3D(K),VT_QC(K),VT_QR(K),VT_QI(K),VT_QS(K), &
                 VT_QG(K),VT_QH(K),VT_NC(K),VT_NR(K),VT_NI(K),VT_NS(K),&
                 VT_NG(K),VT_NH(K),VT_VI(K),VT_VS(K),VT_VG(K),VT_FI(K),&
                 VT_FS(K),VT_AI(K),VT_AS(K),VT_AG(K),VT_AH(K),VTI3M(K),&
                 SASPR(K),RHO(K))
         ENDIF
         IF (QC3D(K).GE.QSMALL.AND.NC3D(K).GE.NSMALL) THEN
            MVDC(K) = (QC3D(K)*iAMW/NC3D(K))**THRD
            IF (MVDC(K).GT.DCR) THEN
               QR3D(K) = QR3D(K)+QC3D(K)
               NR3D(K) = NR3D(K)+NC3D(K)
               QC3D(K) = 0.; NC3D(K) = 0.
            ENDIF
         ENDIF
         IF (QR3D(K).GE.QSMALL.AND.NR3D(K).GE.NSMALL) THEN
            MVDR(K) = (QR3D(K)*iAMW/NR3D(K))**THRD
            IF (MVDR(K).LT.DCR) THEN
               QC3D(K) = QC3D(K)+QR3D(K)
               NC3D(K) = NC3D(K)+NR3D(K)
               QR3D(K) = 0.; NR3D(K) = 0.
            ENDIF
         ENDIF
         IF (QH3D(K).GE.QSMALL.AND.NH3D(K).GE.NSMALL) THEN
            MVDH(K) = (QH3D(K)*iAMH/NH3D(K))**THRD
            IF (MVDH(K).LT.DHMIN) THEN
               QG3D(K) = QG3D(K)+QH3D(K)
               NG3D(K) = NG3D(K)+NH3D(K)
               VG3D(K) = VG3D(K)+QH3D(K)/RHOG0
               QH3D(K) = 0.; NH3D(K) = 0.
               IF (AH3D(K).GE.ASMALL.AND.AFAG_3M.EQ.1) THEN
                  AG3D(K) = AG3D(K)+AH3D(K)
                  AH3D(K) = 0.
               ENDIF
            ENDIF
         ENDIF
!----------------- FOR TENDENCIES DUE TO PRECIPITATION/SEDIMENTATION ---
         TEMPC = QC3D(K)+QI3D(K)                                        ! QC+QI
         IF (TEMPC.GT.0.) THEN
            VTMEAN(K,1) = (VT_QC(K)*QC3D(K)+VT_QI(K)*QI3D(K))/TEMPC     ! SEDIMENTATION SPEED OF WET AEROSOL IN QC+QI
         ELSE
            VTMEAN(K,1) = 0.
         ENDIF
         TEMPR = QR3D(K)+QS3D(K)+QG3D(K)+QH3D(K)                        ! QR+QS+QG+QH
         IF (TEMPR.GT.0.) THEN
            VTMEAN(K,2) = (VT_QR(K)*QR3D(K)+VT_QS(K)*QS3D(K)+VT_QG(K)* &
                          QG3D(K)+VT_QH(K)*QH3D(K))/TEMPR               ! SEDIMENTATION SPEED OF WET AEROSOL IN QR+QS+QG+QH
         ELSE
            VTMEAN(K,2) = 0.
         ENDIF
         TEMPT = TEMPC+TEMPR                                            ! QC+QR+QI+QS+QG
         IF (TEMPT.GT.0.) THEN
            VTMEAN(K,3) = (VTMEAN(K,1)*TEMPC+VTMEAN(K,2)*TEMPR)/TEMPT   ! MEAN SEDIMENTATION SPEED OF WET AEROSOL 
                                                                        ! IN QC+QR+QI+QS+QG+QH
          ELSE
            VTMEAN(K,3) = 0.
         ENDIF
      ENDDO
!----------------- CALCULATED FALLING TERMS ----------------------------
      CALL PTFLUX(QC3D(1),VT_QC(1),RHO(1),DZ3D(1),KTE,DT,DTMN,CLODNCV)
      CALL PTFLUX(QR3D(1),VT_QR(1),RHO(1),DZ3D(1),KTE,DT,DTMN,RAINNCV)
      CALL PTFLUX(QI3D(1),VT_QI(1),RHO(1),DZ3D(1),KTE,DT,DTMN,ICENCV)
      CALL PTFLUX(QS3D(1),VT_QS(1),RHO(1),DZ3D(1),KTE,DT,DTMN,SNOWNCV)
      CALL PTFLUX(QG3D(1),VT_QG(1),RHO(1),DZ3D(1),KTE,DT,DTMN,GRAPNCV)
      CALL PTFLUX(QH3D(1),VT_QH(1),RHO(1),DZ3D(1),KTE,DT,DTMN,HAILNCV)
      RAINNC = RAINNC+CLODNCV+RAINNCV+ICENCV+SNOWNCV+GRAPNCV+HAILNCV
      SNOWNC = SNOWNC+ICENCV+SNOWNCV
      GRAPNC = GRAPNC+GRAPNCV
      HAILNC = HAILNC+HAILNCV
      SR     = (ICENCV+SNOWNCV+GRAPNCV+HAILNCV)/                       &
               (CLODNCV+RAINNCV+ICENCV+SNOWNCV+GRAPNCV+HAILNCV+1.E-12)
      CALL FLFLUX(NC3D(1),VT_NC(1),RHO(1),DZ3D(1),KTE,DT)
      CALL FLFLUX(NR3D(1),VT_NR(1),RHO(1),DZ3D(1),KTE,DT)
      CALL FLFLUX(NI3D(1),VT_NI(1),RHO(1),DZ3D(1),KTE,DT)
      CALL FLFLUX(NS3D(1),VT_NS(1),RHO(1),DZ3D(1),KTE,DT)
      CALL FLFLUX(NG3D(1),VT_NG(1),RHO(1),DZ3D(1),KTE,DT)
      CALL FLFLUX(NH3D(1),VT_NH(1),RHO(1),DZ3D(1),KTE,DT)
      CALL PTFLUX(VI3D(1),VT_VI(1),RHO(1),DZ3D(1),KTE,DT,DTMN,VINCV)
      CALL PTFLUX(VS3D(1),VT_VS(1),RHO(1),DZ3D(1),KTE,DT,DTMN,VSNCV)
      CALL PTFLUX(VG3D(1),VT_VG(1),RHO(1),DZ3D(1),KTE,DT,DTMN,VGNCV)
      CALL PTFLUX(FI3D(1),VT_FI(1),RHO(1),DZ3D(1),KTE,DT,DTMN,FINCV)
      CALL PTFLUX(FS3D(1),VT_FS(1),RHO(1),DZ3D(1),KTE,DT,DTMN,FSNCV)
      CALL PTFLUX(AI3D(1),VT_AI(1),RHO(1),DZ3D(1),KTE,DT,DTMN,AINCV)
      CALL PTFLUX(AS3D(1),VT_AS(1),RHO(1),DZ3D(1),KTE,DT,DTMN,ASNCV)
      CALL PTFLUX(AG3D(1),VT_AG(1),RHO(1),DZ3D(1),KTE,DT,DTMN,AGNCV)
      CALL PTFLUX(AH3D(1),VT_AH(1),RHO(1),DZ3D(1),KTE,DT,DTMN,AHNCV)
      CALL PTFLUX(I3M3D(1),VTI3M(1),RHO(1),DZ3D(1),KTE,DT,DTMN,I3MNCV)

      IV0 = 0
      DO IV = 1,NAER
         IF (NAERN(IV).GT.3) THEN
            CALL FLFLUX(AERO(1,IV0+3),VTMEAN(1,1),RHO(1),DZ3D(1),KTE,DT)! SEDI. OF AEROSOLS IN CLOUDS
            CALL FLFLUX(AERO(1,IV0+4),VTMEAN(1,2),RHO(1),DZ3D(1),KTE,DT)! SEDI. OF AEROSOLS IN PRECIP.
         ELSEIF (NAERN(IV).EQ.3) THEN
            CALL FLFLUX(AERO(1,IV0+3),VTMEAN(1,3),RHO(1),DZ3D(1),KTE,DT)! SEDI. OF AEROSOLS IN HYDROM.
         ENDIF
         IV0 = IV0+NAERN(IV)
      ENDDO
!----------------------------------------------------------------------
      DO K = KTS,KTE
         TK3D(K) = TAIR(K)
         QV3D(K) = QVAP(K)
         IF (QC3D(K).GE.QSMALL.AND.NC3D(K).GE.NSMALL) THEN
            CALL SOLVE_AFAC(TK3D(K),QC3D(K),NC3D(K),LAMC(K),MVDC(K),   &
                 AFAC(K))
         ELSE
            QC3D(K) = 0.; NC3D(K) = 0.; MVDC(K) = 0.; AFAC(K) = 0.
            LAMC(K) = 0.
         ENDIF
         IF (QR3D(K).GE.QSMALL.AND.NR3D(K).GE.NSMALL) THEN
            CALL SOLVE_AFAR(TK3D(K),QR3D(K),NR3D(K),LAMR(K),MVDR(K),   &
                 AFAR(K))
         ELSE
            QR3D(K) = 0.; NR3D(K) = 0.; MVDR(K) = 0.; LAMR(K) = 0.
            AFAR(K) = 0.
         ENDIF
         IF (QI3D(K).GE.QSMALL.AND.NI3D(K).GE.NSMALL) THEN
            CALL SOLVE_AFAI(TK3D(K),PRES(K),RHO(K),QV3D(K),QI3D(K),    &
                 NI3D(K),VI3D(K),FI3D(K),AI3D(K),I3M3D(K),ADAGR(K),    &
                 ZETA(K),LAMI(K),AFAI(K),MVDI(K),RHOI(K),AMI(K),BMI(K),&
                 AVI(K),BVI(K),BEST(K))
            IASPR(K) = FI3D(K)/I3M3D(K)
         ELSE
            QI3D(K) = 0.;  NI3D(K) = 0.;  MVDI(K) = 0.;  I3M3D(K) = 0.
            FI3D(K) = 0.;  VI3D(K) = 0.;  AFAI(K) = 0.;  IASPR(K) = 1.
            ADAGR(K) = 1.; RHOI(K) = 0.;  AI3D(K) = 0.;  LAMI(K) = 0.
         ENDIF
         IF (AS3D(K).LT.ASMALL) THEN
            AS3D(K) = 0.
         ENDIF
         IF (QS3D(K).GE.QSMALL.AND.NS3D(K).GE.NSMALL) THEN
            CALL SOLVE_AFAS(TK3D(K),RHO(K),QS3D(K),QC3D(K),NS3D(K),    &
                 VS3D(K),FS3D(K),AS3D(K),AFAS(K),LAMS(K),MVDS(K),      &
                 RHOS(K),SASPR(K),AMS(K),AVS(K),BVS(K))
         ELSE
            QS3D(K) = 0.; NS3D(K) = 0.; VS3D(K) = 0.; AS3D(K) = 0.
            MVDS(K) = 0.; RHOS(K) = 0.; AFAS(K) = 0.; LAMS(K) = 0.
            FS3D(K) = 0.; SASPR(K) = 1.
         ENDIF
         IF (AG3D(K).LT.ASMALL) THEN
            AG3D(K) = 0.
         ENDIF
         IF (QG3D(K).GE.QSMALL.AND.NG3D(K).GE.NSMALL) THEN
            CALL SOLVE_AFAG(TK3D(K),RHO(K),QG3D(K),QC3D(K),NG3D(K),    &
                 VG3D(K),AG3D(K),LAMG(K),AFAG(K),MVDG(K),RHOG(K),      &
                 AMG(K),AVG(K),BVG(K))
         ELSE
            QG3D(K) = 0.; NG3D(K) = 0.; MVDG(K) = 0.; VG3D(K) = 0.
            AG3D(K) = 0.; AFAG(K) = 0.; RHOG(K) = 0.; LAMG(K) = 0.
         ENDIF
         IF (AH3D(K).LT.ASMALL) THEN
            AH3D(K) = 0.
         ENDIF
         IF (QH3D(K).GE.QSMALL.AND.NH3D(K).GE.NSMALL) THEN
            CALL SOLVE_AFAH(TK3D(K),RHO(K),QH3D(K),NH3D(K),AH3D(K),    &
                 LAMH(K),AFAH(K),MVDH(K),AVH(K),BVH(K))
         ELSE
            QH3D(K) = 0.; NH3D(K) = 0.; MVDH(K) = 0.; LAMH(K) = 0.
            AH3D(K) = 0.; AFAH(K) = 0.
         ENDIF
         IF ((QC3D(K)+QR3D(K)).LT.QSMALL) THEN
            AERO(K,1) = MAX(RLIMIT,AERO(K,1)+AERO(K,3))
            AERO(K,1) = MIN(AERO(K,1),AERO(K,2))
            AERO(K,3) = 0.
         ENDIF
         IF ((QR3D(K)+QS3D(K)+QG3D(K)+QH3D(K)).LT.QSMALL) THEN
            AERO(K,1) = MAX(RLIMIT,AERO(K,1)+AERO(K,4))
            AERO(K,1) = MIN(AERO(K,1),AERO(K,2))
            AERO(K,4) = 0.
         ENDIF
         QDCN3D(K) = AERO(K,1)
         QTCN3D(K) = AERO(K,2)
         QCCN3D(K) = AERO(K,3)
         QRCN3D(K) = AERO(K,4)
         QNIN3D(K) = AERO(K,5)
!------------------------------------------------------------------------
      ENDDO 

      END SUBROUTINE NTU_MICRO
!======================================================================

!======================================================================
      SUBROUTINE ACTIVA(TK1D,W1D,NC1D,NR1D,ABCD,QACac,QACar,RX0,ZCCNS, &
                        SSW,IAE,DO_FR)
!======================================================================
      IMPLICIT NONE
      INTEGER :: IAE                                                    ! (I) aerosol component
      REAL :: TK1D,NC1D,NR1D,QACac,QACar,NACac,NACar,RX0,SSW
      REAL, INTENT(IN) :: W1D
      DOUBLE PRECISION :: X1,X3,DMASS,DERF
      REAL, DIMENSION(NCCN) :: ZCCNS
      REAL, DIMENSION(MAER) :: ABCD
      LOGICAL :: DO_FR
      INTEGER :: IM
      REAL :: GAMAA,ALPHA,SSRW,ES0,RX9,RX1,XMASS,RACT,ZCN9,ZCN99,      &
              ZCCN10,ZCCN15,ZCCN20,ZCCN25,RACT10,RACT15,RACT20,RACT25, &
              WR10,WR15,WR20,WR25,WR10TO15,WR15TO20,WR20TO25,WR25PLUS
      REAL, PARAMETER :: RS10 = 10.E-6, RS15 = 15.E-6, RS20 = 20.E-6,  &
                         RS25 = 25.E-6

      QACac = 0.; QACar = 0.; NACac = 0.; NACar = 0.
!------ decide the number of each mode and radius for CCN cut-off ------
      IF (DO_FR) THEN
         CALL FIND_RC0(DBLE(ABCD(1)/ABCD(2)),CNMOD(1,IAE),CNSTD(1,IAE),&
              WMAS(1,IAE),RX0,TBLXF(1,IAE),TBLRC)
         DO_FR = .FALSE.
      ENDIF
      IF (RX0.EQ.1.E-9) RETURN                                          ! RC=RC_MIN, no dry aerosol available
      RX1 = RX0
!----------------- Kohler Curve parameters -----------------------------
      SSRW  = MIN(SSW,0.03)
      ES0   = 0.0761-1.55E-4*(TK1D-TK0C)                                ! [J/M**2]
      ALPHA = 2.*ES0/(RV*TK1D*RHOW)                                     ! [J/m^2]/[J/kg/K]/[K]/[kg/m^3] = [m]
      GAMAA = 4.*ALPHA**3./(27.*BETA1(IAE))                             ! [m^3]
      RX9   = (GAMAA/(SSRW**2.))**THRD                                  ! critical dry radius [m]
      RX9   = MAX(RX9,RXMIN(IAE))                                       ! set lower limit of RX
      IF (RX1.LE.RX9) RETURN
!----------------- RAINDROP ACTIVATION ---------------------------------
      IF (RX1.GT.RS10) THEN                                             ! force all aerosol > RS10 into rain
         DMASS = 0.D+0
         DO IM = 1,NCCN
            X3 = DLNX(RX1,CNMOD(IM,IAE),CNSTD(IM,IAE),3)
            X1 = DLNX(RS10,CNMOD(IM,IAE),CNSTD(IM,IAE),3)
            DMASS = DMASS+DBLE(ZCCNS(IM))/DBLE(RFACT(IM,IAE))*(DERF(X3)-DERF(X1))/2.D+0  ! aerosol into rain
         ENDDO
         XMASS = MAX(0.,REAL(DMASS))
         XMASS = MIN(ABCD(1),XMASS)
         IF (NAERN(IAE).GE.4) THEN
            ABCD(4) = ABCD(4)+XMASS                                     ! update aerosol in rain
         ELSEIF (NAERN(IAE).EQ.3) THEN
            ABCD(3) = ABCD(3)+XMASS                                     ! update aerosol in (cloud+rain)
         ENDIF
         ABCD(1) = ABCD(1)-XMASS                                        ! update dry aerosol
         CALL RSWHITBY(W1D,RS10,ZCCN10,RACT10,BETA1(IAE),ALPHA,ZCCNS,  &
                       CNMOD(1,IAE),CNSTD(1,IAE))
         CALL RSWHITBY(W1D,RS15,ZCCN15,RACT15,BETA1(IAE),ALPHA,ZCCNS,  &
                       CNMOD(1,IAE),CNSTD(1,IAE))
         CALL RSWHITBY(W1D,RS20,ZCCN20,RACT20,BETA1(IAE),ALPHA,ZCCNS,  &
                       CNMOD(1,IAE),CNSTD(1,IAE))
         CALL RSWHITBY(W1D,RS25,ZCCN25,RACT25,BETA1(IAE),ALPHA,ZCCNS,  &
                       CNMOD(1,IAE),CNSTD(1,IAE))
         WR10 = C4PI3W*RACT10**3.                                       ! [kg]
         WR15 = C4PI3W*RACT15**3.                                       ! [kg]
         WR20 = C4PI3W*RACT20**3.                                       ! [kg]
         WR25 = C4PI3W*RACT25**3.                                       ! [kg]
         WR10TO15 = 0.5*(WR10+WR15)*(ZCCN10-ZCCN15)                     ! [kg/kg]
         WR15TO20 = 0.5*(WR15+WR20)*(ZCCN15-ZCCN20)                     ! [kg/kg]
         WR20TO25 = 0.5*(WR20+WR25)*(ZCCN20-ZCCN25)                     ! [kg/kg]
         WR25PLUS = WR25*ZCCN25                                         ! [kg/kg]
         NACar    = ZCCN10                                              ! [1/kg]
         QACar    = WR10TO15+WR15TO20+WR20TO25+WR25PLUS                 ! [kg/kg]
         RX1      = RS10                                                ! [m]
!----------------- end sectionalize rain embryo ------------------------
!----------------- single category rain embryo -------------------------
         NR1D = NR1D+NACar                                              ! [ #/kg]
      ENDIF
!----------------- CLOUD DROP ACTIVATION ------------------------------
      IF (RX1.GT.RX9) THEN
         DMASS = 0.D+0
         DO IM = 1,NCCN
            X3 = DLNX(RX1,CNMOD(IM,IAE),CNSTD(IM,IAE),3)
            X1 = DLNX(RX9,CNMOD(IM,IAE),CNSTD(IM,IAE),3)
            DMASS = DMASS+DBLE(ZCCNS(IM))/DBLE(RFACT(IM,IAE))*(DERF(X3)-DERF(X1))/2.D+0  ! aerosol into rain
         ENDDO
         XMASS = MAX(0.,REAL(DMASS))
         XMASS = MIN(XMASS,ABCD(1))
         IF (NAERN(IAE).GE.3) THEN
            ABCD(3) = ABCD(3)+XMASS                                     ! update wet aerosol in (cloud+rain)
         ENDIF
         ABCD(1) = ABCD(1)-XMASS                                        ! update dry aerosol
         CALL CCNWHITBY(W1D,RS10,RX9,ZCN99,RACT,BETA1(IAE),ALPHA,ZCCNS,&
                        CNMOD(1,IAE),CNSTD(1,IAE))
         CALL CCNWHITBY(W1D,RX1,RX9,ZCN9,RACT,BETA1(IAE),ALPHA,ZCCNS,  &
                        CNMOD(1,IAE),CNSTD(1,IAE))
         NACac = MIN(ZCN9,MAX(ZCN99-NC1D,0.))                           ! Total # should less than ZCN99
         QACac = NACac*C4PI3W*RACT**3.                                  ! [kg/kg]
         NC1D  = NC1D+NACac                                             ! [#/kg]
         RX1   = RX9
      ENDIF
      RX0 = RX1

      END SUBROUTINE ACTIVA
!======================================================================
!
!----------------- FUNCTION FOR CALCULATE lnX --------------------------
      FUNCTION DLNX(RX,XMODE,SIGMA,N)
!======================================================================
      INTEGER :: N
      REAL :: RX,XMODE,SIGMA
      DOUBLE PRECISION :: DRX,DXMODE,DSTDV,DLNX,DLOG,DSQRT

      DRX    = DBLE(RX)
      DXMODE = DBLE(XMODE)
      DSTDV  = DBLE(SIGMA)
      DLNX   = (DLOG(DRX/DXMODE)-DSTDV**2.*DBLE(N))/(DSQRT(2.D+0)*DSTDV)

      END FUNCTION DLNX
!----------------- FUNCTION FOR CALCULATE lnX --------------------------
      FUNCTION DLNX2(DRX,XMODE,SIGMA,N)
      INTEGER :: N
      REAL :: XMODE,SIGMA
      DOUBLE PRECISION :: DRX,DXMODE,DSTDV,DLNX2,DLOG,DSQRT

      DXMODE = DBLE(XMODE)
      DSTDV  = DBLE(SIGMA)
      DLNX2  = (DLOG(DRX/DXMODE)-DSTDV**2.*DBLE(N))/(DSQRT(2.D+0)*DSTDV)

      END FUNCTION DLNX2
!======================================================================
!
!======================================================================
      SUBROUTINE RSWHITBY(W1D,RSX,Z,RACT,BETA1,ALPHA,ZCCN,CNMOD,CNSTD)
!======================================================================
      IMPLICIT NONE
!----------------- calculate number of CCN with dry radii > R0 ---------
      INTEGER :: IM
      REAL :: RSX,                                                     &! smallest dry radius of rain embryo (input)
              RWS0,                                                    &! wet radius at S = 1
              RACT,                                                    &! activation radius (output)
              Z,                                                       &! number of CCN that fits the criterion (output)
              BETA1,ALPHA,TEMP,TEMP1,TEMP2,TMPX,TEMP4,ERF
      REAL, INTENT(IN) :: W1D
      REAL, DIMENSION(NCCN) :: ZCCN,CNMOD,CNSTD
!----- integrate the number concentration of aerosols with size > RACT--
      Z = 0.
      DO IM = 1,NCCN
         TEMP1 = ZCCN(IM)/2.      
         TEMP2 = SQRT2*CNSTD(IM) 
         TEMP  = (LOG(RSX/CNMOD(IM)))/TEMP2
         Z     = Z+TEMP1*(1.-ERF(TEMP))
      END DO
      RWS0  = SQRT(BETA1*RSX**3./ALPHA)                                 ! wet radius at S = 1
      TMPX  = LOG10(RSX*1.E6)
      TEMP4 = -0.61425115+(-0.66624878-0.17367658*TMPX)*TMPX
      RACT  = 10.**TEMP4*RWS0*W1D**(-0.11782)

      END SUBROUTINE RSWHITBY
!----------------------------------------------------------------------
      SUBROUTINE CCNWHITBY(W1D,RSX,RS9,ZCN9,RACT,BETA1,ALPHA,ZCCN,     &
                 CNMOD,CNSTD)
!======================================================================
      IMPLICIT NONE
!---- derive number of cloud drop activation according to Whitby's distributions
      INTEGER :: IM
      REAL :: RSX,                                                     &! previously activated smallest dry radius
              RS9,                                                     &! current activated smallest dry radius
              ZCN9,                                                    &! newly activated number concentration (#/mol)
              RACT,                                                    &! wet radius of the activated drop
              RWS0,                                                    &! wet radius at S = 1
              BETA1,ALPHA,TMPX,TMP1,TMP2,TMP3,TMP4,TMP5,TMP6,ERF
      REAL, INTENT(IN) :: W1D
      REAL, DIMENSION(NCCN) :: ZCCN,CNMOD,CNSTD

      RWS0 = SQRT(BETA1*RS9**3./ALPHA)                                  ! wet radius at S = 1
      TMPX = LOG10(RSX*1.E6)
      TMP1 = -0.61425115+(-0.66624878-0.17367658*TMPX)*TMPX
      RACT = 10.**TMP1*RWS0*W1D**(-0.11782)                             ! activation size
      ZCN9 = 0.
      DO IM = 1,NCCN
         TMP2 = ZCCN(IM)/2.
         TMP3 = SQRT2*CNSTD(IM) 
         TMP4 = (LOG(RSX/CNMOD(IM)))/TMP3
         TMP5 = (LOG(RS9/CNMOD(IM)))/TMP3
         TMP6 = TMP2*(ERF(TMP4)-ERF(TMP5))
         ZCN9 = ZCN9+TMP6
      END DO

      END SUBROUTINE CCNWHITBY
!======================================================================
!
!======================================================================
      SUBROUTINE DEACTIVA(NACcv,QC1D,NC1D,DCN,TCN,WCN,RC,ZCCNS,IAE,    &
                          DO_FR)  
!======================================================================
      IMPLICIT NONE
      INTEGER :: IAE                                                    ! (I) aerosol component
      LOGICAL :: DO_FR
      REAL :: NC1D,NACcv,QC1D,DCN,TCN,WCN,                             &! (I/O) wet aerosol mixing ratio [kg/kg]
              RC                                                        ! (I/O) R-CUTOFF [um]
      REAL, DIMENSION(NCCN) :: ZCCNS                                    ! (I) # of each mode in [#/kg]
      INTEGER :: I,IM
      DOUBLE PRECISION :: DNACcv,                                      &! # of deactivated cloud droplet
                          DMASS,                                       &! mass of deactivated wet aerosol
                          DSQRT2,X0,X1,X2,X3,S1,S2,DERF,DEXP,DSQRT
!----------------- deactivation of cloud drops -------------------------
      DSQRT2 = DSQRT(2.D+0)
      DNACcv = DBLE(MIN(NACcv,NC1D))
      IF (QC1D.LT.RSMALL) THEN
         DNACcv = DBLE(NC1D)                                            ! deactivate all cld drop for negligiable QC
         NC1D = RLIMIT
      ENDIF
      DMASS = 0.D+0
      IF (DNACcv.GT.1.D+1) THEN
!------ decide the number of each mode and radius for CCN cut-off ------
         IF (DO_FR) THEN
            CALL FIND_RC0(DBLE(DCN/TCN),CNMOD(1,IAE),CNSTD(1,IAE),     &
                 WMAS(1,IAE),RC,TBLXF(1,IAE),TBLRC)
            DO_FR = .FALSE.
         ENDIF
         S1 = 0.D+0
         DO IM = 1,NCCN
            X0 = DLNX(RC,CNMOD(IM,IAE),CNSTD(IM,IAE),0)
            S1 = S1+DBLE(ZCCNS(IM))*(1.D+0-DERF(X0))*5.D-1              ! # for all with r > RC
         ENDDO
         IF (DNACcv.GT.S1) THEN
            DCN = DCN+WCN
            WCN = 0.
            CALL FIND_RC0(DBLE(DCN/TCN),CNMOD(1,IAE),CNSTD(1,IAE),     &
                 WMAS(1,IAE),RC,TBLXF(1,IAE),TBLRC)
            RETURN
         ENDIF
         S1 = 0.
         DO IM = 1,NCCN
            X0 = DLNX(RC,CNMOD(IM,IAE),CNSTD(IM,IAE),0)
            S1 = S1+DBLE(ZCCNS(IM)/CNSTD(IM,IAE))/DSQRT2*DPDF(X0)       ! # between 0 & RC
         ENDDO
         X1 = DBLE(RC)
         X2 = DBLE(RC)*DEXP(DNACcv/S1)
         DO I = 1,4                                                     ! iteratively fixed dN 3 times
            S1 = 0.D+0
            S2 = 0.D+0
            DO IM = 1,NCCN
               X0 = DLNX2(X1,CNMOD(IM,IAE),CNSTD(IM,IAE),0)
               X3 = DLNX2(X2,CNMOD(IM,IAE),CNSTD(IM,IAE),0)
               S1 = S1+DBLE(ZCCNS(IM)/CNSTD(IM,IAE))/DSQRT2*DPDF(X3)
               S2 = S2+DBLE(ZCCNS(IM))*(DERF(X3)-DERF(X0))*5.D-1        ! # between X1 & X2
            ENDDO
            DNACcv = DNACcv-S2
            X1 = X2
            X2 = X2*DEXP(DNACcv/S1)
         ENDDO
         X1 = DBLE(RC)
         X2 = MAX(X1,X2)
         DO IM = 1,NCCN
            X0 = DLNX2(X1,CNMOD(IM,IAE),CNSTD(IM,IAE),3)
            X3 = DLNX2(X2,CNMOD(IM,IAE),CNSTD(IM,IAE),3)
            DMASS = DMASS+DBLE(ZCCNS(IM))/DBLE(RFACT(IM,IAE))*(DERF(X3)-DERF(X0))*5.D-1 ! MASS between X0 & X3
         ENDDO
         DCN = DCN+MIN(WCN,REAL(DMASS))
         WCN = WCN-MIN(WCN,REAL(DMASS))
         RC  = REAL(X2)
      ENDIF

      END SUBROUTINE DEACTIVA
!======================================================================
!
!======================================================================
      SUBROUTINE ICENU(TK1D,P1D,DT,RHO,QV1D,QI1D,NI1D,VI1D,FI1D,AI1D,  &
                 I3M1D,QNIN)
!======================================================================
      IMPLICIT NONE
      INTEGER :: IDEPNU
      REAL :: TK1D,P1D,DT,RHO,QV1D,QI1D,NI1D,VI1D,FI1D,AI1D,I3M1D,QNIN,&
              QVSI,XXLS,ESI,QNDvi,NNDvi,VNDvi,FNDvi,ANDvi,INDvi,NVI0,  &
              EPA,RGDEP,SFCTNV,ICED,GGDEP,SRI,ARDEP0,ARDEP,COSM1,GEOF1,&
              IJDEP0,IJDEP,DC1,DC2,DC3,TC1D,INR0,DANGLE,DACTE,QVDMAX,  &
              NNUMAX,SSRI,CPM

      QNDvi = 0.;NNDvi = 0.;VNDvi = 0.;FNDvi = 0.;ANDvi = 0.;INDvi = 0.

      IDEPNU = 8                                                        ! Ice deposition-nucleation equation option
      TC1D   = TK1D-TK0C
      XXLS   = 3.15E6-2.37E3*TK1D+0.3337E6
      ESI    = MIN(0.99*P1D,POLYSVP(TK1D,1))
      QVSI   = 0.622*ESI/(P1D-ESI)
      SRI    = QV1D/QVSI
      SSRI   = QV1D/QVSI-1.
      QVDMAX = (QV1D-QVSI)/(1.+XXLS**2.*QV1D/(CP*RV*TK1D**2.))          ! SUPERSATURATED WATER FIXED DUE TO TEMPERATURE CHANGE
      NNUMAX = 0.98*QVDMAX/MI0                                          ! INITIATION OF CLOUD ICE CONSTRAINED by 0.98*SUPICE
      ICED   = 916.7-0.175*TC1D-5.E-4*TC1D**2.                          ! ICE DENSITY
      SRI    = QV1D/QVSI                                                ! SATURATION RATIO OVER ICE
      SFCTNV = ((76.1-0.155*TC1D)+(28.5+0.25*TC1D))*1.E-3               ! SURFACE TENSION OVER ICE/VAPOR

      IF (IDEPNU.EQ.0) THEN                                             ! NO DEPOSITION NUCLEATION
         NNDvi = 0.
      ELSEIF (IDEPNU.EQ.1) THEN                                         ! Fletcher (1962)
         NVI0  = 1.E-2*EXP(0.6*(TK0C-MAX(TK1D,2.46E2)))
         NNDvi = MIN(QNIN,NNUMAX,MAX(0.,NVI0/RHO-NI1D))
      ELSEIF (IDEPNU.EQ.2) THEN                                         ! Cooper
         NVI0  = 5.*EXP(0.304*(TK0C-MAX(TK1D,2.46E2)))
         NNDvi = MIN(QNIN,NNUMAX,MAX(0.,NVI0/RHO-NI1D))
      ELSEIF (IDEPNU.EQ.3) THEN                                         ! Huffman (1973)
         NVI0  = 1007.08*SSRI**4.5*1.E3
         NNDvi = MIN(QNIN,NNUMAX,MAX(0.,NVI0/RHO-NI1D))
      ELSEIF (IDEPNU.EQ.4) THEN                                         ! Meyers et al (1992)
         NVI0  = 1.E3*EXP(1.296E1*SSRI-6.39E-1)
         NNDvi = MIN(QNIN,NNUMAX,MAX(0.,NVI0/RHO-NI1D))
      ELSEIF (IDEPNU.EQ.5.AND.INSPEC.EQ.4) THEN                         ! Bacteria(P. syringae):Yankofsky 1981
         NVI0  = QNIN*1.1776*EXP(-89318./((LOG(SRI))**2.*TK1D**3.))
         NNDvi = MIN(QNIN,NNUMAX,MAX(0.,NVI0*DT))
      ELSEIF (IDEPNU.EQ.6) THEN                                         ! DUST (Field 2006)(Asia)
         NVI0  = QNIN*4.337E-1*EXP(-2.E6/((LOG(SRI))**2.*TK1D**3.))
         NNDvi = MIN(QNIN,NNUMAX,MAX(0.,NVI0*DT))
      ELSEIF (IDEPNU.EQ.7) THEN
         IF (INSPEC.EQ.1) THEN                                          ! SOOT
            DC1 = -0.5411955; DC2 = 1.879918; DC3 = 1.607947            ! CHEN Table3
            INR0 = 0.4E-7; DANGLE = 28.; DACTE = -20.E-20               ! HOOSE Table3
         ELSEIF (INSPEC.EQ.2) THEN                                      ! SAHARAN DUST
            DC1 = -0.3353619; DC2 = 1.990979; DC3 = 2.175539            ! CHEN Table3
            INR0 = 1.75E-7; DANGLE = 5.06; DACTE = 3.35E-20             ! CHEN Table6
         ELSEIF (INSPEC.EQ.3) THEN                                      ! ASIAN DUST
            DC1 = -0.3598818; DC2 = 1.982032; DC3 = 2.025390            ! CHEN Table3
            INR0 = 2.E-7; DANGLE = 8.1; DACTE = 1.82E-20                ! CHEN Table6
         ENDIF
         EPA    = ESI*SRI
         RGDEP  = 2.*2.99E-26*SFCTNV/(ICED*BOLTZ*TK1D*LOG(SRI))         ! GERM RADIUS FOR DEPOSITION NUCLEATION
         GGDEP  = C4PI3*SFCTNV*RGDEP**2.                                ! HOMOGENEOUS ENERGY
         ARDEP0 = SFCTNV**5.E-1*(BOLTZ*TK1D)**(-1.5)/1.E13
         ARDEP  = EPA**2.*ARDEP0/ICED
         COSM1  = LOG(1.-COS(DANGLE*PI/1.8E2))
         GEOF1  = MIN(1.,EXP(DC1+DC2*COSM1+DC3*RGDEP/INR0))
         IJDEP0 = EXP((-1.*DACTE-GEOF1*GGDEP)/(BOLTZ*TK1D))
         IJDEP  = ARDEP*INR0**2.*GEOF1**5.E-1*IJDEP0
         NVI0   = QNIN*(1.-EXP(-IJDEP*DT))
         NNDvi  = MIN(QNIN,NNUMAX,MAX(0.,NVI0))
      ELSEIF (IDEPNU.EQ.8) THEN
         NVI0  = MAX(0.,5.94E-5*((TK0C-TK1D)**3.33)*((1.E6/1.E6)**     &! set naer05=1e6/kg (derived to cm-3 at STP below)
                 (0.0264*(TK0C-TK1D)+3.3E-3))*1.E3)                     ! units of #/kg
         NNDvi = MIN(QNIN,NNUMAX,MAX(0.,NVI0/RHO-NI1D))
      ENDIF
      QNDvi = NNDvi*MI0
      VNDvi = QNDvi*iRHOI0
      FNDvi = QNDvi*1.*iAMI0                                            ! ISOMETRIC
      ANDvi = (KCIMIN*NNDvi*INDvi**2.)**THRD
      INDvi = QNDvi*iAMI0
      QNIN  = MAX(0.,QNIN-NNDvi)
      QV1D  = MAX(0.,QV1D-QNDvi)
      QI1D  = MAX(0.,QI1D+QNDvi)
      NI1D  = MAX(0.,NI1D+NNDvi)
      IF (ICE_RHOI.EQ.0.OR.ICE_RHOI.EQ.2) THEN
         VI1D = 0.
      ELSEIF (ICE_RHOI.EQ.1) THEN
         VI1D = MAX(0.,VI1D+VNDvi)
      ENDIF
      FI1D  = MAX(0.,FI1D+FNDvi)
      I3M1D = MAX(0.,I3M1D+INDvi)
      IF (AFAI_3M.EQ.0.OR.AFAI_3M.EQ.2) THEN
         AI1D = 0.
      ELSEIF (AFAI_3M.EQ.1) THEN
         AI1D = MAX(0.,AI1D+ANDvi)
      ENDIF
      CPM  = CP*(1.+0.887*QV1D)
      XXLS = 3.15E6-2370.*TK1D+0.3337E6
      TK1D = TK1D+XXLS*QNDvi/CPM

      END SUBROUTINE ICENU
!======================================================================
!
!======================================================================
      SUBROUTINE SEDI_FALL(TK1D,P1D,QV1D,QC1D,QR1D,QI1D,QS1D,QG1D,QH1D,&
                 NC1D,NR1D,NI1D,NS1D,NG1D,NH1D,VI1D,VS1D,VG1D,FI1D,    &
                 FS1D,AI1D,AS1D,AG1D,AH1D,I3M1D,VTQC,VTQR,VTQI,VTQS,   &
                 VTQG,VTQH,VTNC,VTNR,VTNI,VTNS,VTNG,VTNH,VTVI,VTVS,    &
                 VTVG,VTFI,VTFS,VTAI,VTAS,VTAG,VTAH,VTI3M,SASPR,RHO)
!======================================================================
      IMPLICIT NONE
      REAL :: TK1D,P1D,QV1D,QC1D,QR1D,QI1D,QS1D,QG1D,QH1D,NC1D,NR1D,   &
              NI1D,NS1D,NG1D,NH1D,VI1D,VS1D,VG1D,FI1D,FS1D,AI1D,AS1D,  &
              AG1D,AH1D,I3M1D,VTQC,VTQR,VTQI,VTQS,VTQG,VTQH,VTNC,VTNR, &
              VTNI,VTNS,VTNG,VTNH,VTVI,VTVS,VTVG,VTFI,VTFS,VTAI,VTAS,  &
              VTAG,VTAH,VTI3M,QRHO,RHOAJ,RHO,MVRC,MVRR,GUC,GUR,LAMC,   &
              LAMR,LAMI,LAMS,LAMG,LAMH,RHOI,RHOS,RHOG,AFAC,AFAR,AFAI,  &
              AFAS,AFAG,AFAH,ADAGR,ZETA3,AMI,BMI,AMS,AMG,AVI,BVI,AVS,  &
              BVS,AVG,BVG,AVH,BVH,ZETA,FSQC,FSQR,FSQI,FSQS,FSQG,FSQH,  &
              FSNC,FSNR,FSNI,FSNS,FSNG,FSNH,FSVI,FSVS,FSVG,FSFI,FSAI,  &
              FSAS,FSAG,FSAH,MVDC,MVDR,MVDI,MVDS,MVDG,MVDH,BEST,SASPR
      REAL, PARAMETER :: AVTC = 8.8462E+02, BVTC = 9.7593E+07
      REAL, PARAMETER :: CVTC = -3.4249E+11, AVTR = 2.1454E+00
      REAL, PARAMETER :: BVTR = -2.2812E-04, CVTR = 2.9676E-09
      REAL, PARAMETER :: CQC1 = 2.0901E+01, CQC2 = 9.9111E-01
      REAL, PARAMETER :: CQC3 = 4.4182E+00, CNC1 = 1.8276E+01
      REAL, PARAMETER :: CNC2 = 1.0015E+00, CNC3 = 1.9838E+00
      REAL, PARAMETER :: CQR1 = 1.5943E+01, CQR2 = 1.1898E+00
      REAL, PARAMETER :: CQR3 = 4.0073E+00, CNR1 = 9.4791E+00
      REAL, PARAMETER :: CNR2 = 9.7607E-01, CNR3 = 1.0858E+00

      QRHO  = SQRT(RHO)
      RHOAJ = (RHOSU/RHO)**0.54
      IF (QC1D.GE.QSMALL) THEN
         CALL SOLVE_AFAC(TK1D,QC1D,NC1D,LAMC,MVDC,AFAC)
         IF (LIQ_VTC.EQ.0) THEN
            FSQC = EXP(GAMLN(BVC0+BMW+AFAC+1.)-GAMLN(BMW+AFAC+1.)-     &
                   BVC0*LOG(LAMC))
            FSNC = EXP(GAMLN(BVC0+AFAC+1.)-GAMLN(AFAC+1.)-BVC0*        &
                   LOG(LAMC))
            VTQC = RHOAJ*FSQC*AVC0
            VTNC = RHOAJ*FSNC*AVC0
         ELSEIF (LIQ_VTC.EQ.1) THEN
            MVRC = MIN(MAX((QC1D/NC1D/C4PI3W)**THRD,RCMIN),RCMAX)
            GUC  = EXP(EXP(AFU+BFU*(LOG(MVRC))**3.+CFU*QRHO**3.))
            VTQC = EXP(CQC1+CQC2*LOG(NC1D)+CQC3*LOG(MVRC))*GUC/QC1D
            VTNC = EXP(CNC1+CNC2*LOG(NC1D)+CNC3*LOG(MVRC))*GUC/NC1D
         ELSEIF (LIQ_VTC.EQ.2) THEN
            MVRC = MIN(MAX((QC1D/NC1D/C4PI3W)**THRD,RCMIN),RCMAX)
            GUC  = EXP(EXP(AFU+BFU*(LOG(MVRC))**3.+CFU*QRHO**3.))
            VTQC = MVRC*(AVTC+BVTC*MVRC+CVTC*MVRC**2.)*GUC
            VTNC = MVRC*(AVTC+BVTC*MVRC+CVTC*MVRC**2.)*GUC
         ENDIF
         VTQC = MIN(VTQC,VTCMAX)
         VTNC = MIN(VTNC,VTCMAX)
      ENDIF
      IF (QR1D.GE.QSMALL) THEN
         CALL SOLVE_AFAR(TK1D,QR1D,NR1D,LAMR,MVDR,AFAR)
         IF (LIQ_VTR.EQ.0) THEN
            FSQR = EXP(GAMLN(BVR0+BMW+AFAR+1.)-GAMLN(BMW+AFAR+1.)-     &
                   BVR0*LOG(LAMR))
            FSNR = EXP(GAMLN(BVR0+AFAR+1.)-GAMLN(AFAR+1.)-BVR0*        &
                   LOG(LAMR))
            VTQR = RHOAJ*FSQR*AVR0
            VTNR = RHOAJ*FSNR*AVR0
         ELSEIF (LIQ_VTR.EQ.1) THEN
            MVRR = MIN(MAX((QR1D/NR1D/C4PI3W)**THRD,RRMIN),RRMAX)
            GUR  = EXP(EXP(AFU+BFU*(LOG(MVRR))**3.+CFU*QRHO**3.))
            VTQR = EXP(CQR1+CQR2*LOG(NR1D)+CQR3*LOG(MVRR))*GUR/QR1D
            VTNR = EXP(CNR1+CNR2*LOG(NR1D)+CNR3*LOG(MVRR))*GUR/NR1D
         ELSEIF (LIQ_VTR.EQ.2) THEN
            MVRR = MIN(MAX((QR1D/NR1D/C4PI3W)**THRD,RRMIN),RRMAX)
            GUR  = EXP(EXP(AFU+BFU*(LOG(MVRR))**3.+CFU*QRHO**3.))
            VTQR = EXP(AVTR+BVTR/MVRR+CVTR/(MVRR**2.))*GUR
            VTNR = EXP(AVTR+BVTR/MVRR+CVTR/(MVRR**2.))*GUR
         ENDIF
         VTQR = MIN(VTQR,VTRMAX)
         VTNR = MIN(VTNR,VTRMAX)
      ENDIF
      IF (QI1D.GE.QSMALL) THEN
         CALL SOLVE_AFAI(TK1D,P1D,RHO,QV1D,QI1D,NI1D,VI1D,FI1D,AI1D,   &
              I3M1D,ADAGR,ZETA,LAMI,AFAI,MVDI,RHOI,AMI,BMI,AVI,BVI,    &
              BEST)
         FSQI = EXP(GAMLN(BVI+BMI+AFAI+1.)-GAMLN(BMI+AFAI+1.)-BVI*     &
                LOG(LAMI))
         FSNI = EXP(GAMLN(BVI+AFAI+1.)-GAMLN(AFAI+1.)-BVI*LOG(LAMI))
         FSVI = EXP(GAMLN(BVI+AFAI+4.)-GAMLN(AFAI+4.)-BVI*LOG(LAMI))
         VTQI = MIN(RHOAJ*FSQI*AVI,VTIMAX)
         VTNI = MIN(RHOAJ*FSNI*AVI,VTIMAX)
         VTVI = MIN(RHOAJ*FSVI*AVI,VTIMAX)
        IF (AI1D.GE.ASMALL) THEN
            FSAI = EXP(GAMLN(BVI+AFAI+3.)-GAMLN(AFAI+3.)-BVI*LOG(LAMI))
            VTAI = MIN(RHOAJ*FSAI*AVI,VTIMAX)
         ENDIF
         IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
            ZETA3 = 3.*(ADAGR-1.)/(ADAGR+2.)
            FSFI  = EXP(GAMLN(BVI+ZETA3+AFAI+4.)-GAMLN(ZETA3+AFAI+4.)- &
                    BVI*LOG(LAMI))
            VTFI  = MIN(RHOAJ*FSFI*AVI,VTIMAX)
            VTI3M = MIN(RHOAJ*FSVI*AVI,VTIMAX)
         ENDIF
      ENDIF
      IF (QS1D.GE.QSMALL) THEN
         CALL SOLVE_AFAS(TK1D,RHO,QS1D,QC1D,NS1D,VS1D,FS1D,AS1D,AFAS,  &
              LAMS,MVDS,RHOS,SASPR,AMS,AVS,BVS)
         FSQS = EXP(GAMLN(BVS+BMS+AFAS+1.)-GAMLN(BMS+AFAS+1.)-BVS*     &
                LOG(LAMS))
         FSNS = EXP(GAMLN(BVS+AFAS+1.)-GAMLN(AFAS+1.)-BVS*LOG(LAMS))
         FSVS = EXP(GAMLN(BVS+AFAS+4.)-GAMLN(AFAS+4.)-BVS*LOG(LAMS))
         VTQS = MIN(RHOAJ*FSQS*AVS,VTSMAX)
         VTNS = MIN(RHOAJ*FSNS*AVS,VTSMAX)
         VTVS = MIN(RHOAJ*FSVS*AVS,VTSMAX)
         VTFS = VTVS
         IF (AS1D.GE.ASMALL) THEN
            FSAS = EXP(GAMLN(BVS+AFAS+3.)-GAMLN(AFAS+3.)-BVS*LOG(LAMS))
            VTAS = MIN(RHOAJ*FSAS*AVS,VTSMAX)
         ENDIF
      ENDIF
      IF (QG1D.GE.QSMALL) THEN
         CALL SOLVE_AFAG(TK1D,RHO,QG1D,QC1D,NG1D,VG1D,AG1D,LAMG,AFAG,  &
              MVDG,RHOG,AMG,AVG,BVG)
         FSQG = EXP(GAMLN(BVG+BMG+AFAG+1.)-GAMLN(BMG+AFAG+1.)-BVG*     &
                LOG(LAMG))
         FSNG = EXP(GAMLN(BVG+AFAG+1.)-GAMLN(AFAG+1.)-BVG*LOG(LAMG))
         FSVG = EXP(GAMLN(BVG+AFAG+4.)-GAMLN(AFAG+4.)-BVG*LOG(LAMG))
         VTQG = MIN(RHOAJ*FSQG*AVG,VTGMAX)
         VTNG = MIN(RHOAJ*FSNG*AVG,VTGMAX)
         VTVG = MIN(RHOAJ*FSVG*AVG,VTGMAX)
         IF (AG1D.GE.ASMALL) THEN
            FSAG = EXP(GAMLN(BVG+AFAG+3.)-GAMLN(AFAG+3.)-BVG*LOG(LAMG))
            VTAG = MIN(RHOAJ*FSAG*AVG,VTGMAX)
         ENDIF
      ENDIF
      IF (QH1D.GE.QSMALL) THEN
         CALL SOLVE_AFAH(TK1D,RHO,QH1D,NH1D,AH1D,LAMH,AFAH,MVDH,AVH,BVH)
         FSQH = EXP(GAMLN(BVH+BMH+AFAH+1.)-GAMLN(BMH+AFAH+1.)-BVH*     &
                LOG(LAMH))
         FSNH = EXP(GAMLN(BVH+AFAH+1.)-GAMLN(AFAH+1.)-BVH*LOG(LAMH))
         VTQH = MIN(RHOAJ*FSQH*AVH,VTHMAX)
         VTNH = MIN(RHOAJ*FSNH*AVH,VTHMAX)
         IF (AH1D.GE.ASMALL) THEN
            FSAH = EXP(GAMLN(BVH+AFAH+3.)-GAMLN(AFAH+3.)-BVH*LOG(LAMH))
            VTAH = MIN(RHOAJ*FSAH*AVH,VTHMAX)
         ENDIF
      ENDIF

      END SUBROUTINE SEDI_FALL
!======================================================================
!
!======================================================================
      SUBROUTINE PTFLUX(Q1D,VT1D,RHO,DZ,NK,DT,DTMN,PRT1D) 
!======================================================================
      IMPLICIT NONE
      INTEGER :: K,NK,NS,NSTEP
      INTEGER, PARAMETER :: MAXSTP = 1000
      REAL :: PRT1D,                                                   &! input/output ; accumulative prcipitation (cm)
              DT,                                                      &! time step for integration(=2*DTMN*60 for leapfrog scheme)
              DTMN                                                      ! time step (min)
      REAL, DIMENSION(NK) :: Q1D,VT1D,RHO,DZ,DQDT
!------------ FOR TENDENCIES DUE TO PRECIPITATION/SEDIMENTATION -------
      NSTEP = 1
      DO K = 1,NK
         NSTEP = MAX(NSTEP,INT(VT1D(K)*DT/DZ(K)+1.))
         IF (NSTEP.GT.80) THEN
             PRINT *,'IN PREFLUX',K,NSTEP,DT,VT1D(K),DZ(K),Q1D(K),RHO(K)
         ENDIF
      ENDDO
      IF (NSTEP.GT.MAXSTP) THEN
         PRINT *,'NSTEP FOR PRECIP. IS: ',NSTEP,VT1D,DT,DZ
         STOP 
      ENDIF 
      DO NS = 1,NSTEP
         DQDT(1) = -VT1D(1)*Q1D(1)/DZ(1)
         DO K = 2,NK,1
            DQDT(K) = (VT1D(K-1)*RHO(K-1)*Q1D(K-1)-                    &
                       VT1D(K)*RHO(K)*Q1D(K))/(DZ(K)*RHO(K))
         ENDDO
         PRT1D = PRT1D+VT1D(NK)*RHO(NK)*Q1D(NK)*DTMN*60./REAL(NSTEP)    ! accumulate precipitation [kg/m^2]; 1 kg/m^2 = 1 mm 
         DO K = 1,NK
            Q1D(K) = Q1D(K)+DQDT(K)*DT/REAL(NSTEP) 
         ENDDO 
      ENDDO 

      END SUBROUTINE PTFLUX
!======================================================================
!
!======================================================================
      SUBROUTINE FLFLUX(Q1D,VT1D,RHO,DZ,NK,DT) 
!======================================================================
      IMPLICIT NONE
      INTEGER :: NSTEP,K,NK,NS
      INTEGER, PARAMETER :: MAXSTP = 1000
      REAL :: DT
      REAL, DIMENSION(NK) :: Q1D,VT1D,RHO,DZ,DQDT
!-- INPUT DATA FOR STEPSIZE RK4 CALCULATION FOR TENDENCIES DUE TO PRECIPITATION/SEDIMENTATION
      NSTEP = 1
      DO K = 1,NK
         NSTEP = MAX(NSTEP,INT(VT1D(K)*DT/DZ(K)+1.))
      ENDDO
      DO NS = 1,NSTEP
         DQDT(1) = -VT1D(1)*Q1D(1)/DZ(1) 
         DO K = 2,NK,1
            DQDT(K) = (VT1D(K-1)*RHO(K-1)*Q1D(K-1)-                    &
                       VT1D(K)*RHO(K)*Q1D(K))/(DZ(K)*RHO(K))
         ENDDO
         DO K = 1,NK
            Q1D(K) = Q1D(K)+DQDT(K)*DT/REAL(NSTEP) 
         ENDDO 
      ENDDO 

      END SUBROUTINE FLFLUX
!======================================================================
!
!======================================================================
      SUBROUTINE SMALL_DT(DT,DTS,SDTS,DTKDT,DQVDT,DPDT,RHO,TK1D,P1D,   &
                 QV1D,QC1D,QR1D,QI1D,QS1D,QG1D,QH1D,NC1D,NR1D,NI1D,    &
                 NS1D,NG1D,NH1D,VI1D,VS1D,VG1D,FI1D,FS1D,AI1D,AS1D,    &
                 AG1D,AH1D,I3M1D,SASPR,XDNC,XDNR,GQCTR)
!======================================================================
      IMPLICIT NONE
      INTEGER :: I,HID
      REAL :: DT,DTS,SDTS,                                             &! TIME STEP(s)& SUB-TIME STEP(s)& DTS MUST<DT-SDTS
              DTKDT,DQVDT,DPDT                                          ! TEMPERATURE,WATER VAPOR,PRESSURE FORCING
      REAL :: RHO,P1D,CPM,ESW,ESI,QVSW,QVSI,SSRW,SSRI,XXLV,XXLS,XXLF,  &
              QV1D,TK1D,QC1D,QR1D,QI1D,QS1D,QG1D,QH1D,GQCTR,NC1D,NR1D, &
              NI1D,NS1D,NG1D,NH1D,VI1D,VS1D,VG1D,FI1D,FS1D,AI1D,AS1D,  &
              AG1D,AH1D,I3M1D,AFAC,AFAR,AFAI,AFAS,AFAG,AFAH,ADAGR,ZETA,&
              AMI,BMI,AMS,AMG,AVI,BVI,AVS,BVS,AVG,BVG,AVH,BVH,DV,MUA,  &
              KAP,SCN,TC1D,iDT,SASPR,ELCLD,ELDLD,ELCLC,CAPS
      REAL :: QVTEND,QCTEND,QRTEND,QITEND,QSTEND,QGTEND,QHTEND,RATIO,  &
              QVSOUR,QCSOUR,QRSOUR,QISOUR,QSSOUR,QGSOUR,QHSOUR,QVSINK, &
              QCSINK,QRSINK,QISINK,QSSINK,QGSINK,QHSINK,NCSOUR,NISOUR, &
              NCSINK,NISINK,NSSOUR,NSSINK,FISOUR,FISINK,FSSOUR,FSSINK, &
              VISOUR,VISINK,VSSOUR,VSSINK,VGSOUR,VGSINK,AISOUR,AISINK, &
              ASSOUR,ASSINK,AGSOUR,AGSINK,AHSOUR,AHSINK,IISOUR,IISINK, &
              QVWTEND,QVITEND,DRHIDT,DRHWDT
      REAL :: XDNC                                                      ! deactivated cloud drops (#/kg) = NACcv*DTS
      REAL :: XDNR                                                      ! raindrop evap into cloud drop
      REAL :: DTSI                                                      ! TIME TO REACH RHI=100% (NEGATIVE MEANS THAT TIME IS)
      REAL :: DTSW                                                      ! TIME TO REACH RHW=100% (NEVER HAPPEN OR ALREADY PAST)
      REAL :: QACcv,QACrc,QVDvc,QVDvr,QVDvi,QVDvs,QVDvg,QVDvh,QSBiv,   &
              QSBsv,QSBgv,QSBhv,QEVcv,QEVrv,QEVsv,QEVgv,QEVhv,NACcv,   &
              NACrc,NSBiv,NSBsv,NSBgv,NSBhv,VVDvi,VSBiv,VVDvg,VSBgv,   &
              VEVgv,FVDvi,VVDvs,VSBsv,VEVsv,FSBiv,FVDvs,FSBsv,FEVsv,   &
              AVDvi,ASBiv,AVDvs,ASBsv,AEVsv,AVDvg,ASBgv,AEVgv,AVDvh,   &
              ASBhv,AEVhv,IVDvi,ISBiv
      REAL :: MVDC,MVDR,MVDI,MVDS,MVDG,MVDH,MVRC,MVRR,BTMP,BSTMP,BGTMP,&
              BHTMP,ABW,ABI,RHOAJ,LMVRC,LMVRR,RHOI,RHOS,RHOG,VENQS,    &
              VENQG,VENQH,SUMDEP,VDMAX,EVMAX,SBMAX,SUMCND,SUMEVP,      &
              SUMSUB,H2Z,H4Z,ZETA2,ZETA3,ZETA4,ZETA5,GVHAB,IPH,IPG,GI1,&
              INHGR,BEST,DNIVD,DNSVD,DNGVD,QTMP0,QTMP1,QTMP2,QTMP3,    &
              QTMP4,QTMP5,QTMP6,QTMP7,QTMP8,QTMP9,FTMP0,FTMP1,FTMP2,   &
              FTMP3,FTMP4,FTMP5,FTMP6,FTMP7,FTMP8,FTMP9,ATMP0,ATMP1,   &
              ATMP2,ATMP3,ATMP4,ATMP5,ATMP6,ATMP7,ATMP8,ATMP9,LAMC,    &
              LAMR,LAMI,LAMS,LAMG,LAMH,VENQI,VENFI,VENAI,VENAS,VENAG,  &
              VENAH,VENIC,VENIA,VENQI0,VENAI0,RAT1,RAT2,LLMI,LLMS,LLMG,&
              LLMH
      REAL, PARAMETER :: TORR = 1.E-2                                   ! for 0.01 changes
      REAL, PARAMETER :: AQ1 = 6.6793E+0, BQ1 = 1.0090E+0               ! See CL04 Tables
      REAL, PARAMETER :: CQ1 = 1.4095E+0, AQ2 = 9.9912E+0               !
      REAL, PARAMETER :: BQ2 = -4.7678E-1, CQ2 = -3.1388E-2             !
      REAL, PARAMETER :: AN9 = -1.0593E+0, BN9 = 8.9774E-1              !
      REAL, PARAMETER :: CN9 = -2.8403E-1, DN9 = 1.6328E+0              !
      REAL, PARAMETER :: AN10 = 8.2841E+0, BN10 = 9.7219E-1             !
      REAL, PARAMETER :: CN10 = -5.0808E-1                              !
      REAL :: ZC1,ZC2,ZC3,ZC4,ZP1,ZP2,ZP3,ZP4                           ! See CT16
      DATA ZC1,ZC2,ZC3,ZC4/0.69509913,-0.46685819,0.30490087,1.62148100/
      DATA ZP1,ZP2,ZP3,ZP4/0.36793126,1.82782890,0.63206874,-1.00164090/

      QVTEND = 0.; QVWTEND = 0.; QVITEND = 0.; QCTEND = 0.; QRTEND = 0.
      QITEND = 0.; QSTEND  = 0.; QGTEND  = 0.; QHTEND = 0.
      MVRC  = 0.;  MVRR  = 0.;   MVDC  = 0.;   MVDR  = 0.;  MVDI  = 0.
      MVDS  = 0.;  MVDG  = 0.;   MVDH  = 0.;   QACcv = 0.;  QACrc = 0.
      QVDvc = 0.;  QVDvr = 0.;   QVDvi = 0.;   QVDvs = 0.;  QVDvg = 0.
      QVDvh = 0.;  QSBiv = 0.;   QSBsv = 0.;   QSBgv = 0.;  QSBhv = 0.
      QEVcv = 0.;  QEVrv = 0.;   QEVsv = 0.;   QEVgv = 0.;  QEVhv = 0.
      NACcv = 0.;  NACrc = 0.;   NSBiv = 0.;   NSBsv = 0.;  NSBgv = 0.
      NSBhv = 0.;  VVDvi = 0.;   VSBiv = 0.;   VVDvs = 0.;  VSBsv = 0.
      VEVsv = 0.;  VVDvg = 0.;   VSBgv = 0.;   VEVgv = 0.;  FVDvi = 0.
      FSBiv = 0.;  FVDvs = 0.;   FSBsv = 0.;   FEVsv = 0.;  IVDvi = 0.
      ISBiv = 0.;  AVDvi = 0.;   ASBiv = 0.;   AVDvs = 0.;  ASBsv = 0.
      AEVsv = 0.;  AVDvg = 0.;   ASBgv = 0.;   AEVgv = 0.;  AVDvh = 0.
      ASBhv = 0.;  AEVhv = 0.

      ESW    = MIN(0.99*P1D,POLYSVP(TK1D,0))
      ESI    = MIN(0.99*P1D,POLYSVP(TK1D,1))
      IF (ESI.GT.ESW) ESI = ESW
      QVSW   = 0.622*ESW/(P1D-ESW)
      QVSI   = 0.622*ESI/(P1D-ESI)
      SSRW   = QV1D/QVSW-1.
      SSRI   = QV1D/QVSI-1.
      XXLV   = 3.1484E6-2370.*TK1D
      XXLS   = 3.15E6-2370.*TK1D+0.3337E6
      XXLF   = 2836310.8-(3.1484E6-2370.*TK1D)
      ELDLD  = (1.+XXLS*XXLS*QV1D/(CP*RV*TK1D**2.))
      ELCLD  = (1.+XXLV*XXLS*QV1D/(CP*RV*TK1D**2.))
      ELCLC  = (1.+XXLV*XXLV*QV1D/(CP*RV*TK1D**2.))
      DRHWDT = (1.+SSRW)*(DPDT/P1D+DQVDT/QV1D-XXLV*DTKDT/(RV*TK1D**2.)) ! caused by large scale QV & P T changes
      DRHIDT = (1.+SSRI)*(DPDT/P1D+DQVDT/QV1D-XXLS*DTKDT/(RV*TK1D**2.)) ! caused by large scale QV & P T changes

      IF (ABS(DRHWDT).LT.RLIMIT) THEN
         DTSW = 1.E7                                                    ! very long time to reach saturation
      ELSE
         DTSW = -SSRW/DRHWDT*1.01                                       ! time to reach saturation
      ENDIF                                                             ! negative means the right time has past
      IF (ABS(DRHIDT).LT.RLIMIT) THEN
         DTSI = 1.E7                                                    ! very long time to reach saturation
      ELSE
         DTSI = -SSRI/DRHIDT*1.01                                       ! time to reach saturation
      ENDIF                                                             ! negative means the right time has past
      IF (TK1D.GT.TK0C) THEN
         IF (DTSW.GT.0.) DTS = MIN(DT,DTSW,DT-SDTS)
      ELSE 
         IF (DTSI.GT.0.) DTS = MIN(DT,DTSI,DT-SDTS)
         IF (DTSW.GT.0.) DTS = MIN(DT,DTS,DTSW,DT-SDTS)
      ENDIF 
      DTS = MAX(DTMIN,MIN(DT,DT-SDTS))

      iDT    = 1./DTS
      TC1D   = TK1D-TK0C
      DV     = 2.11E-5*(TK1D/TK0C)**1.94*(101325./P1D)                  ! DIFFUSIVITY OF WATER VAPOR IN AIR(PRUPPACHER&KLETT:13-3)
      MUA    = 1.72E-5*(393./(TK1D+120.))*(TK1D/TK0C)**1.5              ! DYNAMIC VISCOSITY OF AIR (ROGER AND YAU,P.102)
      KAP    = 2.3971E-2+0.0078E-2*TC1D                                 ! THERMAL CONDUCTIVITY OF AIR
      SCN    = (MUA/(RHO*DV))**THRD                                     ! SCHMIDT NUMBER
      CPM    = CP*(1.+0.887*QV1D)
      ABW    = TK1D*RV/ESW/DV+XXLV*(XXLV/TK1D/RV-1.)/TK1D/KAP
      ABI    = TK1D*RV/ESI/DV+XXLS*(XXLS/TK1D/RV-1.)/TK1D/KAP
      RHOAJ  = (RHOSU/RHO)**0.54
      HID    = MAX(MIN(NINT(ABS(TC1D)/0.25),120),0)
      INHGR  = ITBLE(HID)
      GVHAB  = (INHGR-1.)/(INHGR+2.)+1.

      IF (QC1D.GE.QSMALL) THEN
         CALL SOLVE_AFAC(TK1D,QC1D,NC1D,LAMC,MVDC,AFAC)
         MVRC  = MIN(MAX((QC1D/NC1D/C4PI3W)**THRD,RCMIN),RCMAX)
         LMVRC = LOG(MVRC)
      ENDIF
      IF (QR1D.GE.QSMALL) THEN
         CALL SOLVE_AFAR(TK1D,QR1D,NR1D,LAMR,MVDR,AFAR)
         MVRR  = MIN(MAX((QR1D/NR1D/C4PI3W)**THRD,RRMIN),RRMAX)
         LMVRR = LOG(MVRR)
      ENDIF
      IF (QI1D.GE.QSMALL) THEN
         CALL SOLVE_AFAI(TK1D,P1D,RHO,QV1D,QI1D,NI1D,VI1D,FI1D,AI1D,   &
              I3M1D,ADAGR,ZETA,LAMI,AFAI,MVDI,RHOI,AMI,BMI,AVI,BVI,    &
              BEST)
         GI1  = GAMLN(AFAI+1.)
         LLMI = LOG(LAMI)
      IF (ICE_VENT.EQ.3) THEN
         IF ((ADAGR-1.).GE.SLIMIT) THEN
            BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
            IPH   = 3.*ADAGR/(ADAGR+2.)
            ZETA2 = 2.*(ADAGR-1.)/(ADAGR+2.)
            ZETA3 = 3.*(ADAGR-1.)/(ADAGR+2.)
            ZETA4 = 4.*(ADAGR-1.)/(ADAGR+2.)
            ZETA5 = 5.*(ADAGR-1.)/(ADAGR+2.)
            H2Z   = ZC2*ZETA
            H4Z   = ZC4*ZETA
            QTMP0 = EXP(GAMLN(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
            QTMP1 = EXP(GAMLN(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
            QTMP2 = LLMI*(H2Z+BVI/2.+IPH/2.+1.)
            QTMP3 = LLMI*(H4Z+BVI/2.+IPH/2.+1.)
            QTMP4 = EXP(GAMLN(H2Z+BVI/2.+IPH/2.+AFAI+2.)-GI1-QTMP2)
            QTMP5 = EXP(GAMLN(H4Z+BVI/2.+IPH/2.+AFAI+2.)-GI1-QTMP3)
            QTMP6 = LLMI*(H2Z+BVI+IPH+1.)
            QTMP7 = LLMI*(H4Z+BVI+IPH+1.)
            QTMP8 = EXP(GAMLN(H2Z+BVI+IPH+AFAI+2.)-GI1-QTMP6)
            QTMP9 = EXP(GAMLN(H4Z+BVI+IPH+AFAI+2.)-GI1-QTMP7)
            FTMP0 = EXP(GAMLN(H2Z+ZETA3+AFAI+2.)-GI1-LLMI*(H2Z+ZETA3+  &
                    1.))
            FTMP1 = EXP(GAMLN(H4Z+ZETA3+AFAI+2.)-GI1-LLMI*(H4Z+ZETA3+  &
                    1.))
            FTMP2 = LLMI*(H2Z+BVI/2.+IPH/2.+ZETA3+1.)
            FTMP3 = LLMI*(H4Z+BVI/2.+IPH/2.+ZETA3+1.)
            FTMP4 = EXP(GAMLN(H2Z+BVI/2.+IPH/2.+ZETA3+AFAI+2.)-GI1-    &
                    FTMP2)
            FTMP5 = EXP(GAMLN(H4Z+BVI/2.+IPH/2.+ZETA3+AFAI+2.)-GI1-    &
                    FTMP3)
            FTMP6 = LLMI*(H2Z+BVI+IPH+ZETA3+1.)
            FTMP7 = LLMI*(H4Z+BVI+IPH+ZETA3+1.)
            FTMP8 = EXP(GAMLN(H2Z+ZETA3+BVI+IPH+AFAI+2.)-GI1-FTMP6)
            FTMP9 = EXP(GAMLN(H4Z+ZETA3+BVI+IPH+AFAI+2.)-GI1-FTMP7)
            ATMP0 = EXP(GAMLN(H2Z+AFAI+1.)-GI1-LLMI*H2Z)
            ATMP1 = EXP(GAMLN(H4Z+AFAI+1.)-GI1-LLMI*H4Z)
            ATMP2 = LLMI*(H2Z+BVI/2.+IPH/2.+ZETA3)
            ATMP3 = LLMI*(H4Z+BVI/2.+IPH/2.+ZETA3)
            ATMP4 = EXP(GAMLN(H2Z+BVI/2.+IPH/2.+ZETA3+AFAI+1.)-GI1-    &
                    ATMP2)
            ATMP5 = EXP(GAMLN(H4Z+BVI/2.+IPH/2.+ZETA3+AFAI+1.)-GI1-    &
                    ATMP3)
            ATMP6 = LLMI*(H2Z+BVI+IPH)
            ATMP7 = LLMI*(H4Z+BVI+IPH)
            ATMP8 = EXP(GAMLN(H2Z+BVI+IPH+AFAI+1.)-GI1-ATMP6)
            ATMP9 = EXP(GAMLN(H4Z+BVI+IPH+AFAI+1.)-GI1-ATMP7)
            VENQI = ZC1*QTMP0/DI0**H2Z+ZC3*QTMP1/DI0**H4Z+VENC1*ZC1*   &
                    BTMP*QTMP4/DI0**(H2Z+ZETA)+VENC1*ZC3*BTMP*QTMP5/   &
                    DI0**(H4Z+ZETA)+VENC2*ZC1*BTMP**2.*QTMP8/DI0**(H2Z+&
                    ZETA2)+VENC2*ZC3*BTMP**2.*QTMP9/DI0**(H4Z+ZETA2)
            VENFI = ZC1*FTMP0/DI0**(ZETA3+H2Z)+ZC3*FTMP1/DI0**(ZETA3+  &
                    H4Z)+VENC1*ZC1*BTMP*FTMP4/DI0**(H2Z+ZETA4)+VENC1*  &
                    ZC3*BTMP*FTMP5/DI0**(H4Z+ZETA4)+VENC2*ZC1*BTMP**2.*&
                    FTMP8/DI0**(H2Z+ZETA5)+VENC2*ZC3*BTMP**2.*FTMP9/   &
                    DI0**(H4Z+ZETA5)
            VENAI = ZC1*ATMP0/DI0**H2Z+ZC3*ATMP1/DI0**H4Z+VENC1*ZC1*   &
                    BTMP*ATMP4/DI0**(H2Z+ZETA)+VENC1*ZC3*BTMP*ATMP5/   &
                    DI0**(H4Z+ZETA)+VENC2*ZC1*BTMP**2.*ATMP8/DI0**(H2Z+&
                    ZETA2)+VENC2*ZC3*BTMP**2.*ATMP9/DI0**(H4Z+ZETA2)
         ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
            BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
            IPG   = 3./(ADAGR+2.)
            ZETA2 = 2.*(ADAGR-1.)/(ADAGR+2.)
            ZETA3 = 3.*(ADAGR-1.)/(ADAGR+2.)
            ZETA4 = 2.5*(ADAGR-1.)/(ADAGR+2.)
            H2Z   = ZP2*ZETA
            H4Z   = ZP4*ZETA
            QTMP0 = EXP(GAMLN(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
            QTMP1 = EXP(GAMLN(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
            QTMP2 = LLMI*(H2Z+BVI/2.+IPG/2.+1.)
            QTMP3 = LLMI*(H4Z+BVI/2.+IPG/2.+1.)
            QTMP4 = EXP(GAMLN(H2Z+BVI/2.+IPG/2.+AFAI+2.)-GI1-QTMP2)
            QTMP5 = EXP(GAMLN(H4Z+BVI/2.+IPG/2.+AFAI+2.)-GI1-QTMP3)
            QTMP6 = LLMI*(H2Z+BVI+IPG+1.)
            QTMP7 = LLMI*(H4Z+BVI+IPG+1.)
            QTMP8 = EXP(GAMLN(H2Z+BVI+IPG+AFAI+2.)-GI1-QTMP6)
            QTMP9 = EXP(GAMLN(H4Z+BVI+IPG+AFAI+2.)-GI1-QTMP7)
            FTMP0 = EXP(GAMLN(H2Z+ZETA3+AFAI+2.)-GI1-LLMI*(H2Z+ZETA3+  &
                    1.))
            FTMP1 = EXP(GAMLN(H4Z+ZETA3+AFAI+2.)-GI1-LLMI*(H4Z+ZETA3+  &
                    1.))
            FTMP2 = LLMI*(H2Z+BVI/2.+IPG/2.+ZETA3+1.)
            FTMP3 = LLMI*(H4Z+BVI/2.+IPG/2.+ZETA3+1.)
            FTMP4 = EXP(GAMLN(H2Z+BVI/2.+IPG/2.+ZETA3+AFAI+2.)-GI1-    &
                    FTMP2)
            FTMP5 = EXP(GAMLN(H4Z+BVI/2.+IPG/2.+ZETA3+AFAI+2.)-GI1-    &
                    FTMP3)
            FTMP6 = LLMI*(H2Z+BVI+IPG+ZETA3+1.)
            FTMP7 = LLMI*(H4Z+BVI+IPG+ZETA3+1.)
            FTMP8 = EXP(GAMLN(H2Z+BVI+IPG+ZETA3+AFAI+2.)-GI1-FTMP6)
            FTMP9 = EXP(GAMLN(H4Z+BVI+IPG+ZETA3+AFAI+2.)-GI1-FTMP7)
            ATMP0 = EXP(GAMLN(H2Z+AFAI+1.)-GI1-LLMI*H2Z)
            ATMP1 = EXP(GAMLN(H4Z+AFAI+1.)-GI1-LLMI*H4Z)
            ATMP2 = LLMI*(H2Z+BVI/2.+IPG/2.)
            ATMP3 = LLMI*(H4Z+BVI/2.+IPG/2.)
            ATMP4 = EXP(GAMLN(H2Z+BVI/2.+IPG/2.+AFAI+1.)-GI1-ATMP2)
            ATMP5 = EXP(GAMLN(H4Z+BVI/2.+IPG/2.+AFAI+1.)-GI1-ATMP3)
            ATMP6 = LLMI*(H2Z+BVI+IPG)
            ATMP7 = LLMI*(H4Z+BVI+IPG)
            ATMP8 = EXP(GAMLN(H2Z+BVI+IPG+AFAI+1.)-GI1-ATMP6)
            ATMP9 = EXP(GAMLN(H4Z+BVI+IPG+AFAI+1.)-GI1-ATMP7)
            VENQI = ZP1*QTMP0/DI0**H2Z+ZP3*QTMP1/DI0**H4Z+VENP1*ZP1*   &
                    BTMP*QTMP4/DI0**(H2Z-ZETA/2.)+VENP1*ZP3*BTMP*QTMP5/&
                    DI0**(H4Z-ZETA/2.)+VENP2*ZP1*BTMP**2.*QTMP8/DI0**  &
                    (H2Z-ZETA)+VENP2*ZP3*BTMP**2.*QTMP9/DI0**(H4Z-ZETA)
            VENFI = ZP1*FTMP0/DI0**(H2Z+ZETA3)+ZP3*FTMP1/DI0**(H4Z+    &
                    ZETA3)+VENP1*ZP1*BTMP*FTMP4/DI0**(H2Z+ZETA4)+VENP1*&
                    ZP3*BTMP*FTMP5/DI0**(H4Z+ZETA4)+VENP2*ZP1*BTMP**2.*&
                    FTMP8/DI0**(H2Z+ZETA2)+VENP2*ZP3*BTMP**2.*FTMP9/   &
                    DI0**(H4Z+ZETA2)
            VENAI = ZP1*ATMP0/DI0**H2Z+ZP3*ATMP1/DI0**H4Z+VENP1*ZP1*   &
                    BTMP*ATMP4/DI0**(H2Z-ZETA/2.)+VENP1*ZP3*BTMP*ATMP5/&
                    DI0**(H4Z-ZETA/2.)+VENP2*ZP1*BTMP**2.*ATMP8/DI0**( &
                    H2Z-ZETA)+VENP2*ZP3*BTMP**2.*ATMP9/DI0**(H4Z-ZETA)
         ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
            BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
            QTMP0 = EXP(GAMLN(AFAI+2.)-GI1-LOG(LAMI))
            QTMP1 = LLMI*(1.5+BVI/2.)
            QTMP2 = EXP(GAMLN(BVI/2.+AFAI+2.5)-GI1-QTMP1)
            ATMP1 = LLMI*(0.5+BVI/2.)
            ATMP2 = EXP(GAMLN(BVI/2.+AFAI+1.5)-GI1-ATMP1)
            VENQI = AVSG*QTMP0+BVSG*BTMP*QTMP2
            VENFI = VENQI
            VENAI = AVSG+BVSG*BTMP*ATMP2
         ENDIF
      ELSEIF (ICE_VENT.EQ.1.OR.ICE_VENT.EQ.2) THEN
         IF ((ADAGR-1.).GE.SLIMIT) THEN
            IPH   = 3.*ADAGR/(ADAGR+2.)
            IPG   = 3./(ADAGR+2.)
            ZETA2 = 2.*(ADAGR-1.)/(ADAGR+2.)
            ZETA3 = 3.*(ADAGR-1.)/(ADAGR+2.)
            ZETA4 = 4.*(ADAGR-1.)/(ADAGR+2.)
            ZETA5 = 5.*(ADAGR-1.)/(ADAGR+2.)
            H2Z   = ZC2*ZETA
            H4Z   = ZC4*ZETA
            QTMP0 = EXP(GAMLN(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
            QTMP1 = EXP(GAMLN(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
            FTMP0 = EXP(GAMLN(H2Z+ZETA3+AFAI+2.)-GI1-LLMI*(H2Z+ZETA3+  &
                    1.))
            FTMP1 = EXP(GAMLN(H4Z+ZETA3+AFAI+2.)-GI1-LLMI*(H4Z+ZETA3+  &
                    1.))
            ATMP0 = EXP(GAMLN(H2Z+AFAI+1.)-GI1-LLMI*H2Z)
            ATMP1 = EXP(GAMLN(H4Z+AFAI+1.)-GI1-LLMI*H4Z)
            IF (BEST.LE.1.) THEN
               BTMP  = SCN**2.*(AVI*RHOAJ/MUA)
               QTMP2 = LLMI*(H2Z+BVI+IPH+1.)
               QTMP3 = LLMI*(H4Z+BVI+IPH+1.)
               QTMP4 = EXP(GAMLN(H2Z+BVI+IPH+AFAI+2.)-GI1-QTMP2)
               QTMP5 = EXP(GAMLN(H4Z+BVI+IPH+AFAI+2.)-GI1-QTMP3)
               FTMP2 = LLMI*(H2Z+BVI+IPH+ZETA3+1.)
               FTMP3 = LLMI*(H4Z+BVI+IPH+ZETA3+1.)
               FTMP4 = EXP(GAMLN(H2Z+BVI+IPH+ZETA3+AFAI+2.)-GI1-FTMP2)
               FTMP5 = EXP(GAMLN(H4Z+BVI+IPH+ZETA3+AFAI+2.)-GI1-FTMP3)
               ATMP2 = LLMI*(H2Z+BVI+IPH)
               ATMP3 = LLMI*(H4Z+BVI+IPH)
               ATMP4 = EXP(GAMLN(H2Z+BVI+IPH+AFAI+1.)-GI1-ATMP2)
               ATMP5 = EXP(GAMLN(H4Z+BVI+IPH+AFAI+1.)-GI1-ATMP3)
               VENQI = AVIS*ZC1*QTMP0/DI0**H2Z+AVIS*ZC3*QTMP1/DI0**H4Z+&
                       BVIS*ZC1*BTMP*QTMP4/DI0**(H2Z+ZETA2)+BVIS*ZC3*  &
                       BTMP*QTMP5/DI0**(H4Z+ZETA2)
               VENFI = AVIS*ZC1*FTMP0/DI0**(ZETA3+H2Z)+AVIS*ZC3*FTMP1/ &
                       DI0**(H4Z+ZETA3)+BVIS*ZC1*BTMP*FTMP4/DI0**(H2Z+ &
                       ZETA5)+BVIS*ZC3*BTMP*FTMP5/DI0**(H4Z+ZETA5)
               VENAI = AVIS*ZC1*ATMP0/DI0**H2Z+AVIS*ZC3*ATMP1/DI0**H4Z+&
                       BVIS*ZC1*BTMP*ATMP4/DI0**(H2Z+ZETA2)+BVIS*ZC3*  &
                       BTMP*ATMP5/DI0**(H4Z+ZETA2)
               IF (ICE_VENT.EQ.2) THEN
                  QTMP6 = LLMI*(BVI+IPH)
                  QTMP7 = LLMI*(BVI+IPG)
                  QTMP8 = EXP(GAMLN(BVI+IPH+AFAI+1.)-GI1-QTMP6)
                  QTMP9 = EXP(GAMLN(BVI+IPG+AFAI+1.)-GI1-QTMP7)
                  VENIC = AVIS+BVIS*BTMP*QTMP8/DI0**ZETA2
                  VENIA = AVIS+BVIS*BTMP*QTMP9*DI0**ZETA
                  INHGR = INHGR*VENIC/VENIA
                  GVHAB = (INHGR-1.)/(INHGR+2.)+1.
               ENDIF
            ELSEIF (BEST.GT.1.) THEN
               BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
               QTMP2 = LLMI*(H2Z+BVI/2.+IPH/2.+1.)
               QTMP3 = LLMI*(H4Z+BVI/2.+IPH/2.+1.)
               QTMP4 = EXP(GAMLN(H2Z+BVI/2.+IPH/2.+AFAI+2.)-GI1-QTMP2)
               QTMP5 = EXP(GAMLN(H4Z+BVI/2.+IPH/2.+AFAI+2.)-GI1-QTMP3)
               FTMP2 = LLMI*(H2Z+BVI/2.+IPH/2.+ZETA3+1.)
               FTMP3 = LLMI*(H4Z+BVI/2.+IPH/2.+ZETA3+1.)
               FTMP4 = EXP(GAMLN(H2Z+BVI/2.+IPH/2.+ZETA3+AFAI+2.)-GI1- &
                       FTMP2)
               FTMP5 = EXP(GAMLN(H4Z+BVI/2.+IPH/2.+ZETA3+AFAI+2.)-GI1- &
                       FTMP3)
               ATMP2 = LLMI*(H2Z+BVI/2.+IPH/2.+ZETA3)
               ATMP3 = LLMI*(H4Z+BVI/2.+IPH/2.+ZETA3)
               ATMP4 = EXP(GAMLN(H2Z+BVI/2.+IPH/2.+ZETA3+AFAI+1.)-GI1- &
                       ATMP2)
               ATMP5 = EXP(GAMLN(H4Z+BVI/2.+IPH/2.+ZETA3+AFAI+1.)-GI1- &
                       ATMP3)
               VENQI = AVSG*ZC1*QTMP0/DI0**H2Z+AVSG*ZC3*QTMP1/DI0**H4Z+&
                       BVSG*ZC1*BTMP*QTMP4/DI0**(H2Z+ZETA)+BVSG*ZC3*   &
                       BTMP*QTMP5/DI0**(H4Z+ZETA)
               VENFI = AVSG*ZC1*FTMP0/DI0**(ZETA3+H2Z)+AVSG*ZC3*FTMP1/ &
                       DI0**(H4Z+ZETA3)+BVSG*ZC1*BTMP*FTMP4/DI0**(H2Z+ &
                       ZETA4)+BVSG*ZC3*BTMP*FTMP5/DI0**(H4Z+ZETA4)
               VENAI = AVSG*ZC1*ATMP0/DI0**H2Z+AVSG*ZC3*ATMP1/DI0**H4Z+&
                       BVSG*ZC1*BTMP*ATMP4/DI0**(H2Z+ZETA)+BVSG*ZC3*   &
                       BTMP*ATMP5/DI0**(H4Z+ZETA)
               IF (ICE_VENT.EQ.2) THEN
                  QTMP6 = LLMI*(BVI/2.+IPH/2.)
                  QTMP7 = LLMI*(BVI/2.+IPG/2.)
                  QTMP8 = EXP(GAMLN(BVI/2.+IPH/2.+AFAI+1.)-GI1-QTMP6)
                  QTMP9 = EXP(GAMLN(BVI/2.+IPG/2.+AFAI+1.)-GI1-QTMP7)
                  VENIC = AVSG+BVSG*BTMP*QTMP8/DI0**ZETA
                  VENIA = AVSG+BVSG*BTMP*QTMP9*DI0**(ZETA/2.)
                  INHGR = INHGR*VENIC/VENIA
                  GVHAB = (INHGR-1.)/(INHGR+2.)+1.
               ENDIF
            ENDIF
         ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
            IPG   = 3./(ADAGR+2.)
            IPH   = 3.*ADAGR/(ADAGR+2.)
            ZETA2 = 2.*(ADAGR-1.)/(ADAGR+2.)
            ZETA3 = 3.*(ADAGR-1.)/(ADAGR+2.)
            ZETA4 = 2.5*(ADAGR-1.)/(ADAGR+2.)
            H2Z   = ZP2*ZETA
            H4Z   = ZP4*ZETA
            QTMP0 = EXP(GAMLN(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
            QTMP1 = EXP(GAMLN(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
            FTMP0 = EXP(GAMLN(H2Z+ZETA3+AFAI+2.)-GI1-LLMI*(H2Z+ZETA3+  &
                    1.))
            FTMP1 = EXP(GAMLN(H4Z+ZETA3+AFAI+2.)-GI1-LLMI*(H4Z+ZETA3+  &
                    1.))
            ATMP0 = EXP(GAMLN(H2Z+AFAI+1.)-GI1-LLMI*H2Z)
            ATMP1 = EXP(GAMLN(H4Z+AFAI+1.)-GI1-LLMI*H4Z)
            IF (BEST.LE.1.) THEN
               BTMP  = SCN**2.*(AVI*RHOAJ/MUA)
               QTMP2 = LLMI*(H2Z+BVI+IPG+1.)
               QTMP3 = LLMI*(H4Z+BVI+IPG+1.)
               QTMP4 = EXP(GAMLN(H2Z+BVI+IPG+AFAI+2.)-GI1-QTMP2)
               QTMP5 = EXP(GAMLN(H4Z+BVI+IPG+AFAI+2.)-GI1-QTMP3)
               FTMP2 = LLMI*(H2Z+BVI+IPG+ZETA3+1.)
               FTMP3 = LLMI*(H4Z+BVI+IPG+ZETA3+1.)
               FTMP4 = EXP(GAMLN(H2Z+BVI+IPG+ZETA3+AFAI+2.)-GI1-FTMP2)
               FTMP5 = EXP(GAMLN(H4Z+BVI+IPG+ZETA3+AFAI+2.)-GI1-FTMP3)
               ATMP2 = LLMI*(H2Z+BVI+IPG)
               ATMP3 = LLMI*(H4Z+BVI+IPG)
               ATMP4 = EXP(GAMLN(H2Z+BVI+IPG+AFAI+1.)-GI1-ATMP2)
               ATMP5 = EXP(GAMLN(H4Z+BVI+IPG+AFAI+1.)-GI1-ATMP3)
               VENQI = AVIS*ZP1*QTMP0/DI0**H2Z+AVIS*ZP3*QTMP1/DI0**H4Z+&
                       BVIS*ZP1*BTMP*QTMP4/DI0**(H2Z-ZETA)+BVIS*ZP3*   &
                       BTMP*QTMP5/DI0**(H4Z-ZETA)
               VENFI = AVIS*ZP1*FTMP0/DI0**(ZETA3+H2Z)+AVIS*ZP3*FTMP1/ &
                       DI0**(H4Z+ZETA3)+BVIS*ZP1*BTMP*FTMP4/DI0**(H2Z+ &
                       ZETA2)+BVIS*ZP3*BTMP*FTMP5/DI0**(H4Z+ZETA2)
               VENAI = AVIS*ZP1*ATMP0/DI0**H2Z+AVIS*ZP3*ATMP1/DI0**H4Z+&
                       BVIS*ZP1*BTMP*ATMP4/DI0**(H2Z-ZETA)+BVIS*ZP3*   &
                       BTMP*ATMP5/DI0**(H4Z-ZETA)
               IF (ICE_VENT.EQ.2) THEN
                  QTMP6 = LLMI*(BVI+IPH)
                  QTMP7 = LLMI*(BVI+IPG)
                  QTMP8 = EXP(GAMLN(BVI+IPH+AFAI+1.)-GI1-QTMP6)
                  QTMP9 = EXP(GAMLN(BVI+IPG+AFAI+1.)-GI1-QTMP7)
                  VENIC = AVIS+BVIS*BTMP*QTMP8/DI0**ZETA2
                  VENIA = AVIS+BVIS*BTMP*QTMP9*DI0**ZETA
                  INHGR = INHGR*VENIC/VENIA
                  GVHAB = (INHGR-1.)/(INHGR+2.)+1.
               ENDIF
            ELSEIF (BEST.GT.1.) THEN
               BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
               QTMP2 = LLMI*(H2Z+BVI/2.+IPG/2.+1.)
               QTMP3 = LLMI*(H4Z+BVI/2.+IPG/2.+1.)
               QTMP4 = EXP(GAMLN(H2Z+BVI/2.+IPG/2.+AFAI+2.)-GI1-QTMP2)
               QTMP5 = EXP(GAMLN(H4Z+BVI/2.+IPG/2.+AFAI+2.)-GI1-QTMP3)
               FTMP2 = LLMI*(H2Z+BVI/2.+IPG/2.+ZETA3+1.)
               FTMP3 = LLMI*(H4Z+BVI/2.+IPG/2.+ZETA3+1.)
               FTMP4 = EXP(GAMLN(H2Z+BVI/2.+IPG/2.+ZETA3+AFAI+2.)-GI1- &
                       FTMP2)
               FTMP5 = EXP(GAMLN(H4Z+BVI/2.+IPG/2.+ZETA3+AFAI+2.)-GI1- &
                       FTMP3)
               ATMP2 = LLMI*(H2Z+BVI/2.+IPG/2.)
               ATMP3 = LLMI*(H4Z+BVI/2.+IPG/2.)
               ATMP4 = EXP(GAMLN(H2Z+BVI/2.+IPG/2.+AFAI+1.)-GI1-ATMP2)
               ATMP5 = EXP(GAMLN(H4Z+BVI/2.+IPG/2.+AFAI+1.)-GI1-ATMP3)
               VENQI = AVSG*ZP1*QTMP0/DI0**H2Z+AVSG*ZP3*QTMP1/DI0**H4Z+&
                       BVSG*ZP1*BTMP*QTMP4/DI0**(H2Z-ZETA/2.)+BVSG*ZP3*&
                       BTMP*QTMP5/DI0**(H4Z-ZETA/2.)
               VENFI = AVSG*ZP1*FTMP0/DI0**(ZETA3+H2Z)+AVSG*ZP3*FTMP1/ &
                       DI0**(H4Z+ZETA3)+BVSG*ZP1*BTMP*FTMP4/DI0**(H2Z+ &
                       ZETA4)+BVSG*ZP3*BTMP*FTMP5/DI0**(H4Z+ZETA4)
               VENAI = AVSG*ZP1*ATMP0/DI0**H2Z+AVSG*ZP3*ATMP1/DI0**H4Z+&
                       BVSG*ZP1*BTMP*ATMP4/DI0**(H2Z-ZETA/2.)+BVSG*ZP3*&
                       BTMP*ATMP5/DI0**(H4Z-ZETA/2.)
               IF (ICE_VENT.EQ.2) THEN
                  QTMP6 = LLMI*(BVI/2.+IPH/2.)
                  QTMP7 = LLMI*(BVI/2.+IPG/2.)
                  QTMP8 = EXP(GAMLN(BVI/2.+IPH/2.+AFAI+1.)-GI1-QTMP6)
                  QTMP9 = EXP(GAMLN(BVI/2.+IPG/2.+AFAI+1.)-GI1-QTMP7)
                  VENIC = AVSG+BVSG*BTMP*QTMP8/DI0**ZETA
                  VENIA = AVSG+BVSG*BTMP*QTMP9*DI0**(ZETA/2.)
                  INHGR = INHGR*VENIC/VENIA
                  GVHAB = (INHGR-1.)/(INHGR+2.)+1.
               ENDIF
            ENDIF
         ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
            QTMP0 = EXP(GAMLN(AFAI+2.)-GI1-LOG(LAMI))
            IF (BEST.LE.1.) THEN
               BTMP  = SCN**2.*(AVI*RHOAJ/MUA)
               QTMP2 = LLMI*(2.+BVI)
               QTMP3 = EXP(GAMLN(BVI+AFAI+3.)-GI1-QTMP2)
               ATMP2 = LLMI*(1.+BVI)
               ATMP3 = EXP(GAMLN(BVI+AFAI+2.)-GI1-ATMP2)
               VENQI = AVIS*QTMP0+BVIS*BTMP*QTMP3
               VENFI = VENQI
               VENAI = AVIS+BVIS*BTMP*ATMP3
            ELSEIF (BEST.GT.1.) THEN
               BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
               QTMP2 = LLMI*(1.5+BVI/2.)
               QTMP3 = EXP(GAMLN(BVI/2.+AFAI+2.5)-GI1-QTMP2)
               ATMP2 = LLMI*(0.5+BVI/2.)
               ATMP3 = EXP(GAMLN(BVI/2.+AFAI+1.5)-GI1-ATMP2)
               VENQI = AVSG*QTMP0+BVSG*BTMP*QTMP3
               VENFI = VENQI
               VENAI = AVSG+BVSG*BTMP*ATMP3
            ENDIF
         ENDIF
      ELSEIF (ICE_VENT.EQ.0) THEN
         IF ((ADAGR-1.).GE.SLIMIT) THEN
            H2Z   = ZC2*ZETA
            H4Z   = ZC4*ZETA
            ZETA3 = 3.*(ADAGR-1.)/(ADAGR+2.)
            QTMP0 = EXP(GAMLN(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
            QTMP1 = EXP(GAMLN(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
            FTMP0 = EXP(GAMLN(H2Z+ZETA3+AFAI+2.)-GI1-LLMI*(H2Z+ZETA3+  &
                    1.))
            FTMP1 = EXP(GAMLN(H4Z+ZETA3+AFAI+2.)-GI1-LLMI*(H4Z+ZETA3+  &
                    1.))
            ATMP0 = EXP(GAMLN(H2Z+AFAI+1.)-GI1-LLMI*H2Z)
            ATMP1 = EXP(GAMLN(H4Z+AFAI+1.)-GI1-LLMI*H4Z)
            VENQI = ZC1*QTMP0/DI0**H2Z+ZC3*QTMP1/DI0**H4Z
            VENFI = ZC1*FTMP0/DI0**(H2Z+ZETA3)+ZC3*FTMP1/DI0**(H4Z+    &
                    ZETA3)
            VENAI = ZC1*ATMP0/DI0**H2Z+ZC3*ATMP1/DI0**H4Z
         ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
            H2Z   = ZP2*ZETA
            H4Z   = ZP4*ZETA
            ZETA3 = 3.*(ADAGR-1.)/(ADAGR+2.)
            QTMP0 = EXP(GAMLN(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
            QTMP1 = EXP(GAMLN(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
            FTMP0 = EXP(GAMLN(H2Z+ZETA3+AFAI+2.)-GI1-LLMI*(H2Z+ZETA3+  &
                    1.))
            FTMP1 = EXP(GAMLN(H4Z+ZETA3+AFAI+2.)-GI1-LLMI*(H4Z+ZETA3+  &
                    1.))
            ATMP0 = EXP(GAMLN(H2Z+AFAI+1.)-GI1-LLMI*H2Z)
            ATMP1 = EXP(GAMLN(H4Z+AFAI+1.)-GI1-LLMI*H4Z)
            VENQI = ZP1*QTMP0/DI0**H2Z+ZP3*QTMP1/DI0**H4Z
            VENFI = ZP1*FTMP0/DI0**(H2Z+ZETA3)+ZP3*FTMP1/DI0**(H4Z+    &
                    ZETA3)
            VENAI = ZP1*ATMP0/DI0**H2Z+ZP3*ATMP1/DI0**H4Z
         ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
            VENQI = EXP(GAMLN(AFAI+2.)-GI1-LOG(LAMI))
            VENFI = VENQI
            VENAI = 1.
         ENDIF
      ENDIF                                                             ! ICE_VENT
      IF (ICE_SHAPE.EQ.1.AND.AFAI.LE.20.) THEN
         QTMP0 = EXP(GAMLN(AFAI+2.)-GI1-LOG(LAMI))
         ATMP0 = EXP(GAMLN(AFAI+1.)-GI1-LOG(LAMI))
         IF (ICE_VENT.EQ.3) THEN
            BTMP   = SCN*SQRT(AVI*RHOAJ/MUA)
            QTMP1  = LLMI*(1.5+BVI/2.)
            QTMP2  = EXP(GAMLN(BVI/2.+AFAI+2.5)-GI1-QTMP1)
            ATMP1  = LLMI*(0.5+BVI/2.)
            ATMP2  = EXP(GAMLN(BVI/2.+AFAI+1.5)-GI1-ATMP1)
            VENQI0 = AVSG*QTMP0+BVSG*BTMP*QTMP2
            VENAI0 = AVSG*ATMP0+BVSG*BTMP*ATMP2
         ELSEIF (ICE_VENT.EQ.1.OR.ICE_VENT.EQ.2) THEN
            IF (BEST.LE.1.) THEN
               BTMP   = SCN**2.*(AVI*RHOAJ/MUA)
               QTMP2  = LLMI*(2.+BVI)
               QTMP3  = EXP(GAMLN(BVI+AFAI+3.)-GI1-QTMP2)
               ATMP2  = LLMI*(1.+BVI)
               ATMP3  = EXP(GAMLN(BVI+AFAI+2.)-GI1-ATMP2)
               VENQI0 = AVIS*QTMP0+BVIS*BTMP*QTMP3
               VENAI0 = AVIS*ATMP0+BVIS*BTMP*ATMP3
            ELSEIF (BEST.GT.1.) THEN
               BTMP   = SCN*SQRT(AVI*RHOAJ/MUA)
               QTMP2  = LLMI*(1.5+BVI/2.)
               QTMP3  = EXP(GAMLN(BVI/2.+AFAI+2.5)-GI1-QTMP2)
               ATMP2  = LLMI*(0.5+BVI/2.)
               ATMP3  = EXP(GAMLN(BVI/2.+AFAI+1.5)-GI1-ATMP2)
               VENQI0 = AVSG*QTMP0+BVSG*BTMP*QTMP3
               VENAI0 = AVSG*ATMP0+BVSG*BTMP*ATMP3
            ENDIF
         ELSEIF (ICE_VENT.EQ.0) THEN
            VENQI0 = EXP(GAMLN(AFAI+2.)-GI1-LOG(LAMI))
            VENAI0 = EXP(GAMLN(AFAI+1.)-GI1-LOG(LAMI))
         ENDIF
      ENDIF                                                             ! DI0_CORRECTION
      ELSE
         ADAGR = 1.
         ZETA = 0.
      ENDIF                                                             ! QI SMALL
      IF (QS1D.GE.QSMALL) THEN
         CALL SOLVE_AFAS(TK1D,RHO,QS1D,QC1D,NS1D,VS1D,FS1D,AS1D,AFAS,  &
              LAMS,MVDS,RHOS,SASPR,AMS,AVS,BVS)
         LLMS  = LOG(LAMS)
         BSTMP = SCN*SQRT(AVS*RHOAJ/MUA)
         QTMP1 = LLMS*(1.5+BVS/2.)
         QTMP2 = EXP(GAMLN(AFAS+2.)-GAMLN(AFAS+1.)-LOG(LAMS))
         QTMP3 = EXP(GAMLN(BVS/2.+AFAS+2.5)-GAMLN(AFAS+1.)-QTMP1)
         ATMP1 = LLMS*(0.5+BVS/2.)
         ATMP2 = EXP(GAMLN(BVS/2.+AFAS+1.5)-GAMLN(AFAS+1.)-ATMP1)
         CAPS  = ZP1*SASPR**(ZP2/3.)+ZP3*SASPR**(ZP4/3.)
         VENQS = AVSG*QTMP2*CAPS+BVSG*BSTMP*QTMP3*CAPS
         VENAS = AVSG*CAPS+BVSG*BSTMP*ATMP2*CAPS
      ENDIF
      IF (QG1D.GE.QSMALL) THEN
         CALL SOLVE_AFAG(TK1D,RHO,QG1D,QC1D,NG1D,VG1D,AG1D,LAMG,AFAG,  &
              MVDG,RHOG,AMG,AVG,BVG)
         LLMG  = LOG(LAMG)
         BGTMP = SCN*SQRT(AVG*RHOAJ/MUA)
         QTMP1 = LLMG*(1.5+BVG/2.)
         QTMP2 = EXP(GAMLN(AFAG+2.)-GAMLN(AFAG+1.)-LOG(LAMG))
         QTMP3 = EXP(GAMLN(BVG/2.+AFAG+2.5)-GAMLN(AFAG+1.)-QTMP1)
         ATMP1 = LLMG*(0.5+BVG/2.)
         ATMP2 = EXP(GAMLN(BVG/2.+AFAG+1.5)-GAMLN(AFAG+1.)-ATMP1)
         VENQG = AVSG*QTMP2+BVSG*BGTMP*QTMP3
         VENAG = AVSG+BVSG*BGTMP*ATMP2
      ENDIF
      IF (QH1D.GE.QSMALL) THEN
         CALL SOLVE_AFAH(TK1D,RHO,QH1D,NH1D,AH1D,LAMH,AFAH,MVDH,AVH,BVH)
         LLMH = LOG(LAMH)
         BHTMP = SCN*SQRT(AVH*RHOAJ/MUA)
         IF (HAIL_VENT.EQ.0) THEN
            QTMP1 = LLMH*(1.5+BVH/2.)
            QTMP2 = EXP(GAMLN(AFAH+2.)-GAMLN(AFAH+1.)-LOG(LAMH))
            QTMP3 = EXP(GAMLN(BVH/2.+AFAH+2.5)-GAMLN(AFAH+1.)-QTMP1)
            ATMP1 = LLMH*(0.5+BVH/2.)
            ATMP2 = EXP(GAMLN(BVH/2.+AFAH+1.5)-GAMLN(AFAH+1.)-ATMP1)
            VENQH = AVRH*QTMP2+BVRH*BHTMP*QTMP3
            VENAH = AVRH+BVRH*BHTMP*ATMP2
         ELSEIF (HAIL_VENT.EQ.1) THEN
            QTMP1 = LLMH*(1.5+BVH/2.)
            QTMP2 = EXP(GAMLN(AFAH+2.)-GAMLN(AFAH+1.)-LOG(LAMH))
            QTMP3 = EXP(GAMLN(BVH/2.+AFAH+2.5)-GAMLN(AFAH+1.)-QTMP1)
            QTMP4 = LLMH*(2.+BVH)
            QTMP5 = EXP(GAMLN(BVH+AFAH+3.)-GAMLN(AFAH+1.)-QTMP4)
            ATMP1 = LLMH*(0.5+BVH/2.)
            ATMP2 = EXP(GAMLN(BVH/2.+AFAH+1.5)-GAMLN(AFAH+1.)-ATMP1)
            ATMP3 = LLMH*(1.+BVH)
            ATMP4 = EXP(GAMLN(BVH+AFAH+2.)-GAMLN(AFAH+1.)-ATMP3)
            VENQH = QTMP2+VENH1*BHTMP*QTMP3+VENH2*BHTMP**2.*QTMP5
            VENAH = 1.+VENH1*BHTMP*ATMP2+VENH2*BHTMP**2.*ATMP4
         ENDIF
      ENDIF
!----------------- WARM RAIN PROCESSES ---------------------------------
      IF (QC1D.GE.QSMALL) THEN
         IF (SSRW.GT.RSMALL) THEN
            QVDvc = SSRW*EXP(AQ1+BQ1*LOG(NC1D)+CQ1*LMVRC)/ABW           ! diffusion growth of cloud drops
         ELSEIF (SSRW.LT.(-1.*RSMALL)) THEN
            QEVcv = MIN(SSRW*EXP(AQ1+BQ1*LOG(NC1D)+CQ1*LMVRC)/ABW,0.)   ! diffusion growth of cloud drops
            NACcv = -1.*EXP(AN9+BN9*LOG(NC1D)+CN9*LMVRC+DN9*LOG(-SSRW)) ! deactivation of cloud drop into CN
            QACcv = NACcv*4.1888E-15                                    ! deactivation of cloud drop into CN
            QACcv = MAX(QACcv,-1.*QC1D*iDT)
            NACcv = MAX(NACcv,-1.*NC1D*iDT)
         ENDIF
      ENDIF
      IF (QR1D.GE.QSMALL) THEN
         IF (SSRW.GT.RSMALL) THEN
            QVDvr = SSRW*NR1D*EXP(AQ2+(BQ2+CQ2*LMVRR)*LMVRR**2.)/ABW    ! diffusion growth of raindrops
         ELSEIF (SSRW.LT.(-1.*RSMALL)) THEN
            QEVrv = SSRW*NR1D*EXP(AQ2+(BQ2+CQ2*LMVRR)*LMVRR**2.)/ABW    ! diffusion growth of raindrops
            NACrc = SSRW*EXP(AN10+BN10*LOG(NR1D)+CN10*LMVRR)/ABW        ! rain drop evaporate into cloud drop
            QACrc = NACrc*5.236E-10                                     ! rain drop evaporate into cloud drop
            QEVrv = MIN(QEVrv,0.)
            QACrc = MAX(QACrc,-1.*QR1D*iDT)
            NACrc = MAX(NACrc,-1.*NR1D*iDT)
         ENDIF
      ENDIF
      IF (SAT_ADJ.EQ.1) THEN
         IF ((QVDvc+QVDvr).GE.QSMALL) THEN
            VDMAX = (QV1D-QVSW)/(1.+XXLV**2.*QV1D/(CPM*RV*TK1D**2.))*iDT
            SUMCND = QVDvc+QVDvr
            IF (SUMCND.GT.VDMAX.AND.VDMAX.GE.QSMALL) THEN
               RATIO = MIN(1.,VDMAX/(SUMCND+QSMALL))
               QVDvc = QVDvc*RATIO
               QVDvr = QVDvr*RATIO
            ENDIF
         ENDIF
      ENDIF
!-------------- DEPOSITION/SUBLIMATION TERMS FOR QI,QS,QG,QH -----------
      IF (TK1D.LT.TK0C) THEN
         IF (QI1D.GE.QSMALL) THEN
            DNIVD = RHOI0*EXP(-3.*MAX((QV1D-QVSI)-5.E-5,0.)/INHGR)
            DNIVD = MIN(MAX(DNIVD,RHOIMIN),RHOIMAX)
         IF (ICE_SHAPE.EQ.1.AND.AFAI.LE.20.) THEN
            RAT1  = MIN(1.,MAX(0.,ABS(GAMMP(AFAI+1.,DI0*LAMI))))
            RAT2  = 1.-RAT1
            QVDvi = 2.*PI*NI1D*(RAT2*VENQI+RAT1*VENQI0)*SSRI/ABI
            IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
               FVDvi = 12.*NI1D*(RAT2*VENFI*GVHAB+RAT1*VENQI0)*SSRI/   &
                       ABI/DNIVD
               IVDvi = 12.*NI1D*(RAT2*VENQI+RAT1*VENQI0)*SSRI/ABI/DNIVD
            ENDIF
            IF (AI1D.GE.ASMALL) THEN
               AVDvi = 8.*NI1D*(RAT2*VENAI+RAT1*VENAI0)*SSRI/ABI/DNIVD
            ENDIF
         ELSE
            QVDvi = 2.*PI*NI1D*VENQI*SSRI/ABI
            IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
               FVDvi = 12.*NI1D*GVHAB*VENFI*SSRI/ABI/DNIVD
               IVDvi = 12.*NI1D*VENQI*SSRI/ABI/DNIVD
            ENDIF
            IF (AI1D.GE.ASMALL) THEN
               AVDvi = 8.*NI1D*VENAI*SSRI/ABI/DNIVD
            ENDIF
         ENDIF
            VVDvi = QVDvi/DNIVD
            IF (QVDvi.LT.0.) THEN
               QSBiv = MIN(QVDvi,0.)
               VSBiv = MIN(QVDvi/RHOI,0.)
               QVDvi = 0.
               VVDvi = 0.
               IF (AI1D.GE.ASMALL) THEN
                  ASBiv = MIN(AVDvi*DNIVD/RHOI,0.)
                  AVDvi = 0.
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FSBiv = MIN(FVDvi*DNIVD/RHOI,0.)
                  ISBiv = MIN(IVDvi*DNIVD/RHOI,0.)
                  FVDvi = 0.
                  IVDvi = 0.
               ENDIF
            ENDIF
         ENDIF                                                          ! QISMALL
         IF (QS1D.GE.QSMALL) THEN
            DNSVD = RHOS*EXP(-3.*MAX((QV1D-QVSI)-5.E-5,0.)/1.)
            DNSVD = MIN(MAX(DNSVD,RHOIMIN),RHOIMAX)
            QVDvs = 2.*PI*NS1D*VENQS*SSRI/ABI
            IF (ICE_RHOS.EQ.1) THEN
               VVDvs = QVDvs/DNSVD
            ENDIF
            IF (AGG_SHAPE.EQ.1) THEN
               FVDvs = QVDvs/DNSVD*SASPR*V2M3
            ENDIF
            IF (AS1D.GE.ASMALL) THEN
               AVDvs = 8.*NS1D*VENAS*SSRI/ABI/DNSVD
            ENDIF
            IF (QVDvs.LT.0.) THEN
               QSBsv = MIN(QVDvs,0.)
               VSBsv = MIN(QVDvs/RHOS,0.)
               FSBsv = MIN(FVDvs*DNSVD/RHOS,0.)
               QVDvs = 0.
               VVDvs = 0.
               FVDvs = 0.
               IF (AS1D.GE.ASMALL) THEN
                  ASBsv = MIN(AVDvs*DNSVD/RHOS,0.)
                  AVDvs = 0.
               ENDIF
            ENDIF
         ENDIF
         IF (QG1D.GE.QSMALL) THEN
            DNGVD = RHOG*EXP(-3.*MAX((QV1D-QVSI)-5.E-5,0.))
            DNGVD = MIN(MAX(DNGVD,RHOIMIN),RHOG0)
            QVDvg = 2.*PI*NG1D*VENQG*SSRI/ABI
            IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
               VVDvg = QVDvg/DNGVD
            ENDIF
            IF (AG1D.GE.ASMALL) THEN
               AVDvg = 8.*NG1D*VENAG*SSRI/ABI/DNGVD
            ENDIF
            IF (QVDvg.LT.0.) THEN
               QSBgv = MIN(QVDvg,0.)
               QVDvg = 0.
               VSBgv = MIN(QVDvg/RHOG,0.)
               VVDvg = 0.
               IF (AG1D.GE.ASMALL) THEN
                  ASBgv = MIN(AVDvg*DNGVD/RHOG,0.)
                  AVDvg = 0.
               ENDIF
            ENDIF
         ENDIF
         IF (QH1D.GE.QSMALL) THEN
            QVDvh = 2.*PI*NH1D*VENQH*SSRI/ABI
            IF (AH1D.GE.ASMALL) THEN
               AVDvh = 8.*NH1D*VENAH*SSRI/ABI/RHOH
            ENDIF
            IF (QVDvh.LT.0.) THEN
               QSBhv = MIN(QVDvh,0.)
               QVDvh = 0.
               IF (AH1D.GE.ASMALL) THEN
                  ASBhv = MIN(AVDvh,0.)
                  AVDvh = 0.
               ENDIF
            ENDIF
         ENDIF
         IF (SAT_ADJ.EQ.1) THEN
            IF ((QVDvi+QVDvs+QVDvg+QVDvh).GE.QSMALL) THEN
               VDMAX  = (QV1D-QVSI)/(1.+XXLS**2.*QV1D/(CPM*RV*TK1D**   &
                        2.))*iDT
               SUMDEP = QVDvi+QVDvs+QVDvg+QVDvh
               IF (SUMDEP.GT.VDMAX.AND.VDMAX.GE.QSMALL) THEN
                  RATIO = MIN(1.,VDMAX/(SUMDEP+QSMALL))
                  QVDvi = QVDvi*RATIO
                  QVDvs = QVDvs*RATIO
                  QVDvg = QVDvg*RATIO
                  QVDvh = QVDvh*RATIO
                  VVDvi = VVDvi*RATIO
                  VVDvs = VVDvs*RATIO
                  VVDvg = VVDvg*RATIO
                  FVDvi = FVDvi*RATIO
                  FVDvs = FVDvs*RATIO
                  IVDvi = IVDvi*RATIO
                  AVDvi = AVDvi*RATIO
                  AVDvs = AVDvs*RATIO
                  AVDvg = AVDvg*RATIO
                  AVDvh = AVDvh*RATIO
               ENDIF
            ENDIF
         ENDIF
            IF ((QSBiv+QSBsv+QSBgv+QSBhv).LT.0.) THEN
               SBMAX  = (QV1D-QVSI)/(1.+XXLS**2.*QV1D/(CPM*RV*TK1D**   &
                        2.))*iDT
               SUMSUB = QSBiv+QSBsv+QSBgv+QSBhv
               IF (SBMAX.LT.0..AND.SUMSUB.LT.SBMAX*0.9999) THEN
                  QSBiv = QSBiv*MIN(1.,0.9999*SBMAX/SUMSUB)
                  QSBsv = QSBsv*MIN(1.,0.9999*SBMAX/SUMSUB)
                  QSBgv = QSBgv*MIN(1.,0.9999*SBMAX/SUMSUB)
                  QSBhv = QSBhv*MIN(1.,0.9999*SBMAX/SUMSUB)
                  VSBiv = VSBiv*MIN(1.,0.9999*SBMAX/SUMSUB)
                  VSBsv = VSBsv*MIN(1.,0.9999*SBMAX/SUMSUB)
                  VSBgv = VSBgv*MIN(1.,0.9999*SBMAX/SUMSUB)
                  FSBiv = FSBiv*MIN(1.,0.9999*SBMAX/SUMSUB)
                  FSBsv = FSBsv*MIN(1.,0.9999*SBMAX/SUMSUB)
                  ISBiv = ISBiv*MIN(1.,0.9999*SBMAX/SUMSUB)
                  ASBiv = ASBiv*MIN(1.,0.9999*SBMAX/SUMSUB)
                  ASBsv = ASBsv*MIN(1.,0.9999*SBMAX/SUMSUB)
                  ASBgv = ASBgv*MIN(1.,0.9999*SBMAX/SUMSUB)
                  ASBhv = ASBhv*MIN(1.,0.9999*SBMAX/SUMSUB)
               ENDIF
            ENDIF
!         ENDIF
      ELSE                                                              ! TK1D.GE.TK0C
         IF (QS1D.GE.QSMALL.AND.SSRW.LT.-1.*RSMALL) THEN
            QEVsv = 2.*PI*NS1D*VENQS*SSRW/ABW
            QEVsv = MAX(MIN(QEVsv,0.),-1.*QS1D*iDT)
            IF (ICE_RHOS.EQ.1) THEN
               VEVsv = MAX(MIN(QEVsv/RHOS,0.),-1.*VS1D*iDT)
            ENDIF
            IF (AGG_SHAPE.EQ.1) THEN
               FEVsv = MAX(QEVsv/RHOS*SASPR*V2M3,-1.*FS1D*iDT)
            ENDIF
            IF (AS1D.GE.ASMALL) THEN
               AEVsv = 8.*NS1D*VENAS*SSRW/ABW/RHOS
               AEVsv = MAX(MIN(AEVsv,0.),-1.*AS1D*iDT)
            ENDIF
         ENDIF
         IF (QG1D.GE.QSMALL.AND.SSRW.LT.-1.*RSMALL) THEN
            QEVgv = 2.*PI*NG1D*VENQG*SSRW/ABW
            QEVgv = MAX(MIN(QEVgv,0.),-1.*QG1D*iDT)
            IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
               VEVgv = MAX(MIN(QEVgv/RHOG,0.),-1.*VG1D*iDT)
            ENDIF
            IF (AG1D.GE.ASMALL) THEN
               AEVgv = 8.*NG1D*VENAG*SSRW/ABW/RHOG
               AEVgv = MAX(MIN(AEVgv,0.),-1.*AG1D*iDT)
            ENDIF
         ENDIF
         IF (QH1D.GE.QSMALL.AND.SSRW.LT.-1.*RSMALL) THEN
            QEVhv = 2.*PI*NH1D*VENQH*SSRW/ABW
            QEVhv = MAX(MIN(QEVhv,0.),-1.*QH1D*iDT)
            IF (AH1D.GE.ASMALL) THEN
               AEVhv = 8.*NH1D*VENAH*SSRW/ABW/RHOH
               AEVhv = MAX(MIN(AEVhv,0.),-1.*AH1D*iDT)
            ENDIF
         ENDIF
         IF ((QEVcv+QEVrv+QEVsv+QEVgv+QEVhv).LT.0.) THEN
            EVMAX  = (QV1D-QVSW)/(1.+XXLV**2.*QV1D/(CPM*RV*TK1D**2.))* &
                     iDT
            SUMEVP = QEVcv+QEVrv+QEVsv+QEVgv+QEVhv
            IF (EVMAX.LT.0..AND.SUMEVP.LT.EVMAX*0.9999) THEN
               QEVcv = QEVcv*MIN(1.,0.9999*EVMAX/SUMEVP)
               QEVrv = QEVrv*MIN(1.,0.9999*EVMAX/SUMEVP)
               QEVsv = QEVsv*MIN(1.,0.9999*EVMAX/SUMEVP)
               QEVgv = QEVgv*MIN(1.,0.9999*EVMAX/SUMEVP)
               QEVhv = QEVhv*MIN(1.,0.9999*EVMAX/SUMEVP)
               VEVsv = VEVsv*MIN(1.,0.9999*EVMAX/SUMEVP)
               VEVgv = VEVgv*MIN(1.,0.9999*EVMAX/SUMEVP)
               FEVsv = FEVsv*MIN(1.,0.9999*EVMAX/SUMEVP)
               AEVsv = AEVsv*MIN(1.,0.9999*EVMAX/SUMEVP)
               AEVgv = AEVgv*MIN(1.,0.9999*EVMAX/SUMEVP)
               AEVhv = AEVhv*MIN(1.,0.9999*EVMAX/SUMEVP)
            ENDIF
         ENDIF
      ENDIF                                                             ! TEMPERATURE LOOP

      QVWTEND = -QACcv-QVDvc-QVDvr-QEVcv-QEVrv-QEVsv-QEVgv-QEVhv
      QVITEND = -QVDvi-QVDvs-QVDvg-QVDvh-QSBiv-QSBsv-QSBgv-QSBhv
      QVTEND  = QVWTEND+QVITEND
      QCTEND  = QACcv-QACrc+QVDvc+QEVcv
      QRTEND  = QACrc+QVDvr+QEVrv
      QITEND  = QVDvi+QSBiv
      QSTEND  = QVDvs+QEVsv+QSBsv
      QGTEND  = QVDvg+QEVgv+QSBgv
      QHTEND  = QVDvh+QEVhv+QSBhv

      DTS = MIN(TORR*MAX(QV1D,1.E-7)/MAX(ABS(QVTEND),QSMALL),          &! limited by d(QV)
                TORR*MAX(QC1D,QLIMIT)/MAX(ABS(QCTEND),QSMALL),         &! limited by d(QC)
                TORR*MAX(QR1D,QLIMIT)/MAX(ABS(QRTEND),QSMALL),         &! limited by d(QR)
                TORR*MAX(QI1D,QLIMIT)/MAX(ABS(QITEND),QSMALL),         &! limited by d(QI)
                TORR*MAX(QS1D,QLIMIT)/MAX(ABS(QSTEND),QSMALL),         &! limited by d(QS)
                TORR*MAX(QG1D,QLIMIT)/MAX(ABS(QGTEND),QSMALL),         &! limited by d(QG)
                TORR*MAX(QH1D,QLIMIT)/MAX(ABS(QHTEND),QSMALL),         &! limited by d(QH)
                DT,DTS,DT-SDTS)
      DTS = MIN(DT-SDTS,MAX(DTMIN,DTS))

!--- CALCULATE TIME TO REACH RHW=100%, RHI=100% 
!--- NEGATIVE VALUE MEANS TIME HAS PAST AND WOULD NEVER REACH 100% IN THE FUTURE

      DTSI = -(QVWTEND*ELCLD+QVITEND*ELDLD)                             ! rate of saturation vapor over ice change
      DTSW = -(QVWTEND*ELCLC+QVITEND*ELCLD)                             ! rate of saturation vapor over liquid change 
      IF (ABS(DTSI).LT.RLIMIT) THEN
         DTSI = 1.E5                                                    ! AN ARBITARY VALUE
      ELSE
         DTSI = SSRI*QV1D/((1.+SSRI)*DTSI)                              ! time to reach saturation over ice 
      ENDIF
      IF (ABS(DTSW).LT.RLIMIT) THEN
         DTSW = 1.E5                                                    ! AN ARBITARY VALUE
      ELSE
         DTSW = SSRW*QV1D/((1.+SSRW)*DTSW)                              ! time to reach saturation over liquid 
      ENDIF

      IF (TK1D.GT.TK0C) THEN 
         IF(DTSW.GT.0.) DTS = MAX(MIN(DTS,DTSW),DTMIN)
      ELSE 
         IF(DTSI.GT.0.) DTS = MAX(MIN(DTS,DTSI),DTMIN)
         IF(DTSW.GT.0.) DTS = MAX(MIN(DTS,DTSW),DTMIN)
      ENDIF

      QVSOUR = QV1D+(-QACcv-QEVcv-QEVrv-QEVsv-QEVgv-QEVhv-QSBiv-QSBsv- &
               QSBgv-QSBhv)*DTS
      QVSINK = (QVDvc+QVDvr+QVDvi+QVDvs+QVDvg+QVDvh)*DTS
      IF (QVSINK.GT.QVSOUR.AND.QVSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QVSOUR/(QVSINK+QSMALL))
         QVDvc = QVDvc*RATIO; QVDvr = QVDvr*RATIO
         QVDvi = QVDvi*RATIO; QVDvs = QVDvs*RATIO
         QVDvg = QVDvg*RATIO; QVDvh = QVDvh*RATIO
      ENDIF
      QCSOUR = QC1D+(QVDvc-QACrc)*DTS
      QCSINK = (-QACcv-QEVcv)*DTS
      IF (QCSINK.GT.QCSOUR.AND.QCSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QCSOUR/(QCSINK+QSMALL))
         QACcv = QACcv*RATIO; QEVcv = QEVcv*RATIO
      ENDIF
      QRSOUR = QR1D+QVDvr*DTS
      QRSINK = (-QACrc-QEVrv)*DTS
      IF (QRSINK.GT.QRSOUR.AND.QRSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QRSOUR/(QRSINK+QSMALL))
         QACrc = QACrc*RATIO; QEVrv = QEVrv*RATIO
      ENDIF
      QISOUR = QI1D+QVDvi*DTS
      QISINK = (-QSBiv)*DTS
      IF (QISINK.GT.QISOUR.AND.QISOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QISOUR/(QISINK+QSMALL))
         QSBiv = QSBiv*RATIO
      ENDIF
      QSSOUR = QS1D+QVDvs*DTS
      QSSINK = (-QEVsv-QSBsv)*DTS
      IF (QSSINK.GT.QSSOUR.AND.QSSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QSSOUR/(QSSINK+QSMALL))
         QSBsv = QSBsv*RATIO; QEVsv = QEVsv*RATIO
      ENDIF
      QGSOUR = QG1D+QVDvg*DTS
      QGSINK = (-QSBgv-QEVgv)*DTS
      IF (QGSINK.GT.QGSOUR.AND.QGSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QGSOUR/(QGSINK+QSMALL))
         QSBgv = QSBgv*RATIO; QEVgv = QEVgv*RATIO
      ENDIF
      QHSOUR = QH1D+QVDvh*DTS
      QHSINK = (-QSBhv-QEVhv)*DTS
      IF (QHSINK.GT.QHSOUR.AND.QHSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QHSOUR/(QHSINK+QSMALL))
         QSBhv = QSBhv*RATIO; QEVhv = QEVhv*RATIO
      ENDIF
      IF (QI1D.GE.QSMALL) THEN
         NSBiv = MIN(QSBiv*NI1D/QI1D,0.)
      ENDIF
      IF (QS1D.GE.QSMALL) THEN
         NSBsv = MIN(QSBsv*NS1D/QS1D,0.)
      ENDIF
      IF (QG1D.GE.QSMALL) THEN
         NSBgv = MIN(QSBgv*NG1D/QG1D,0.)
      ENDIF
      IF (QH1D.GE.QSMALL) THEN
         NSBhv = MIN(QSBhv*NH1D/QH1D,0.)
      ENDIF
      NCSOUR = NC1D+(-NACrc)*DTS
      NCSINK = (-NACcv)*DTS
      IF (NCSINK.GT.NCSOUR.AND.NCSOUR.GE.NSMALL) THEN
         RATIO = MIN(1.,NCSOUR/(NCSINK+NSMALL))
         NACcv = NACcv*RATIO
      ENDIF
      NISOUR = NI1D
      NISINK = (-NSBiv)*DTS
      IF (NISINK.GT.NISOUR.AND.NISOUR.GE.NSMALL) THEN
         RATIO = MIN(1.,NISOUR/(NISINK+NSMALL))
         NSBiv = NSBiv*RATIO
      ENDIF
      NSSOUR = NS1D
      NSSINK = (-NSBsv)*DTS
      IF (NSSINK.GT.NSSOUR.AND.NSSOUR.GE.NSMALL) THEN
         RATIO = MIN(1.,NSSOUR/(NSSINK+NSMALL))
         NSBsv = NSBsv*RATIO
      ENDIF
      VISOUR = VI1D+VVDvi*DTS
      VISINK = (-VSBiv)*DTS
      IF (VISINK.GT.VISOUR.AND.VISOUR.GE.ISMALL) THEN
         RATIO = MIN(1.,VISOUR/(VISINK+ISMALL))
         VSBiv = VSBiv*RATIO
      ENDIF
      VSSOUR = VS1D+VVDvs*DTS
      VSSINK = (-VEVsv-VSBsv)*DTS
      IF (VSSINK.GT.VSSOUR.AND.VSSOUR.GE.ISMALL) THEN
         RATIO = MIN(1.,VSSOUR/(VSSINK+ISMALL))
         VSBsv = VSBsv*RATIO; VEVsv = VEVsv*RATIO
      ENDIF
      VGSOUR = VG1D+VVDvg*DTS
      VGSINK = (-VSBgv-VEVgv)*DTS
      IF (VGSINK.GT.VGSOUR.AND.VGSOUR.GE.ISMALL) THEN
         RATIO = MIN(1.,VGSOUR/(VGSINK+ISMALL))
         VSBgv = VSBgv*RATIO; VEVgv = VEVgv*RATIO
      ENDIF
      FISOUR = FI1D+FVDvi*DTS
      FISINK = (-FSBiv)*DTS
      IF (FISINK.GT.FISOUR.AND.FISOUR.GE.ISMALL) THEN
         RATIO = MIN(1.,FISOUR/(FISINK+ISMALL))
         FSBiv = FSBiv*RATIO
      ENDIF
      FSSOUR = FS1D+FVDvs*DTS
      FSSINK = (-FEVsv-FSBsv)*DTS
      IF (FSSINK.GT.FSSOUR.AND.FSSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,FSSOUR/(FSSINK+QSMALL))
         FSBsv = FSBsv*RATIO; FEVsv = FEVsv*RATIO
      ENDIF
      AISOUR = AI1D+AVDvi*DTS
      AISINK = (-ASBiv)*DTS
      IF (AISINK.GT.AISOUR.AND.AISOUR.GE.ASMALL) THEN
         RATIO = MIN(1.,AISOUR/(AISINK+ASMALL))
         ASBiv = ASBiv*RATIO
      ENDIF
      ASSOUR = AS1D+AVDvs*DTS
      ASSINK = (-ASBsv-AEVsv)*DTS
      IF (ASSINK.GT.ASSOUR.AND.ASSOUR.GE.ASMALL) THEN
         RATIO = MIN(1.,ASSOUR/(ASSINK+ASMALL))
         ASBsv = ASBsv*RATIO; AEVsv = AEVsv*RATIO
      ENDIF
      AGSOUR = AG1D+AVDvg*DTS
      AGSINK = (-ASBgv-AEVgv)*DTS
      IF (AGSINK.GT.AGSOUR.AND.AGSOUR.GE.ASMALL) THEN
         RATIO = MIN(1.,AGSOUR/(AGSINK+ASMALL))
         ASBgv = ASBgv*RATIO; AEVgv = AEVgv*RATIO
      ENDIF
      AHSOUR = AH1D+AVDvh*DTS
      AHSINK = (-ASBhv-AEVhv)*DTS
      IF (AHSINK.GT.AHSOUR.AND.AHSOUR.GE.ASMALL) THEN
         RATIO = MIN(1.,AHSOUR/(AHSINK+ASMALL))
         ASBhv = ASBhv*RATIO; AEVhv = AEVhv*RATIO
      ENDIF
      IISOUR = I3M1D+IVDvi*DTS
      IISINK = (-ISBiv)*DTS
      IF (IISINK.GT.IISOUR.AND.IISOUR.GE.ISMALL) THEN
         RATIO = MIN(1.,IISOUR/(IISINK+ISMALL))
         ISBiv = ISBiv*RATIO
      ENDIF
!----------------- update values of variables -------------------------
      QV1D = MAX(0.,QV1D+(-QACcv-QVDvc-QVDvr-QEVcv-QEVrv-QEVsv-QEVgv-  &
             QEVhv-QVDvi-QVDvs-QVDvg-QVDvh-QSBiv-QSBsv-QSBgv-QSBhv)*DTS)
      QC1D = MAX(0.,QC1D+(QACcv-QACrc+QVDvc+QEVcv)*DTS)
      QR1D = MAX(0.,QR1D+(QACrc+QVDvr+QEVrv)*DTS)
      QI1D = MAX(0.,QI1D+(QVDvi+QSBiv)*DTS)
      QS1D = MAX(0.,QS1D+(QVDvs+QSBsv+QEVsv)*DTS)
      QG1D = MAX(0.,QG1D+(QVDvg+QSBgv+QEVgv)*DTS)
      QH1D = MAX(0.,QH1D+(QVDvh+QSBhv+QEVhv)*DTS)
      NC1D = MAX(0.,NC1D+(NACcv-NACrc)*DTS)
      NR1D = MAX(0.,NR1D+(NACrc)*DTS)
      NI1D = MAX(0.,NI1D+(NSBiv)*DTS)
      NS1D = MAX(0.,NS1D+(NSBsv)*DTS)
      NG1D = MAX(0.,NG1D+(NSBgv)*DTS)
      NH1D = MAX(0.,NH1D+(NSBhv)*DTS)
      IF (ICE_RHOI.EQ.0.OR.ICE_RHOI.EQ.2) THEN
         VI1D = 0.
      ELSEIF (ICE_RHOI.EQ.1) THEN
         VI1D = MAX(0.,VI1D+(VVDvi+VSBiv)*DTS)
      ENDIF
      IF (ICE_RHOS.EQ.0.OR.ICE_RHOS.EQ.2) THEN
         VS1D = 0.
      ELSEIF (ICE_RHOS.EQ.1) THEN
         VS1D = MAX(0.,VS1D+(VVDvs+VSBsv+VEVsv)*DTS)
      ENDIF
      IF (ICE_RHOG.EQ.0) THEN
         VG1D = 0.
      ELSEIF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
         VG1D = MAX(0.,VG1D+(VVDvg+VSBgv+VEVgv)*DTS)
      ENDIF
      FI1D = MAX(0.,FI1D+(FVDvi+FSBiv)*DTS)
      IF (AGG_SHAPE.EQ.0) THEN
         FS1D = 0.
      ELSEIF (AGG_SHAPE.EQ.1) THEN
         FS1D = MAX(0.,FS1D+(FVDvs+FSBsv+FEVsv)*DTS)
      ENDIF
      I3M1D = MAX(0.,I3M1D+(IVDvi+ISBiv)*DTS)
      IF (AFAI_3M.EQ.0.OR.AFAI_3M.EQ.2) THEN
         AI1D = 0.
      ELSEIF (AFAI_3M.EQ.1) THEN
         AI1D = MAX(0.,AI1D+(AVDvi+ASBiv)*DTS)
      ENDIF
      IF (AFAS_3M.EQ.0.OR.AFAS_3M.EQ.2) THEN
         AS1D = 0.
      ELSEIF (AFAS_3M.EQ.1) THEN
         AS1D = MAX(0.,AS1D+(AVDvs+ASBsv+AEVsv)*DTS)
      ENDIF
      IF (AFAG_3M.EQ.0.OR.AFAG_3M.EQ.2) THEN
         AG1D = 0.
      ELSEIF (AFAG_3M.EQ.1) THEN
         AG1D = MAX(0.,AG1D+(AVDvg+ASBgv+AEVgv)*DTS)
      ENDIF
      IF (AFAH_3M.EQ.0.OR.AFAH_3M.EQ.2) THEN
         AH1D = 0.
      ELSEIF (AFAH_3M.EQ.1) THEN
         AH1D = MAX(0.,AH1D+(AVDvh+ASBhv+AEVhv)*DTS)
      ENDIF
      XDNC  = XDNC+NACcv*DTS
      XDNR  = XDNR+NACrc*DTS
      GQCTR = QACrc*DTS
      TK1D  = TK1D+((QACcv+QACrc+QVDvc+QVDvr+QEVcv+QEVrv+QEVsv+QEVgv+  &
              QEVhv)*XXLV+(QVDvi+QVDvs+QVDvg+QVDvh+QSBiv+QSBsv+QSBgv+  &
              QSBhv)*XXLS)/CPM*DTS

      IF (QC1D.GE.QSMALL.AND.NC1D.GE.NSMALL) THEN
         MVDC = (QC1D*iAMW/NC1D)**THRD
         IF (MVDC.GT.DCR) THEN
            QR1D = QR1D+QC1D
            NR1D = NR1D+NC1D
            QC1D = 0.; NC1D = 0.
         ENDIF
      ENDIF
      IF (QR1D.GE.QSMALL.AND.NR1D.GE.NSMALL) THEN
         MVDR = (QR1D*iAMW/NR1D)**THRD
         IF (MVDR.LT.DCR) THEN
            QC1D = QC1D+QR1D
            NC1D = NC1D+NR1D
            QR1D = 0.; NR1D = 0.
         ENDIF
      ENDIF
      IF (QH1D.GE.QSMALL.AND.NH1D.GE.NSMALL) THEN
         MVDH = (QH1D*iAMH/NH1D)**THRD
         IF (MVDH.LT.DHMIN) THEN
            QG1D = QG1D+QH1D
            NG1D = NG1D+NH1D
            VG1D = VG1D+QH1D/RHOG0
            QH1D = 0.; NH1D = 0.
            IF (AH1D.GE.ASMALL.AND.AFAG_3M.EQ.1) THEN
               AG1D = AG1D+AH1D
               AH1D = 0.
            ENDIF
         ENDIF
      ENDIF

      END SUBROUTINE SMALL_DT
!======================================================================
!
!======================================================================
      SUBROUTINE LARGE_DT(DT,TK1D,QV1D,P1D,RHO,QC1D,QR1D,QI1D,QS1D,    &
                 QG1D,QH1D,NC1D,NR1D,NI1D,NS1D,NG1D,NH1D,VI1D,VS1D,    &
                 VG1D,FI1D,FS1D,AI1D,AS1D,AG1D,AH1D,I3M1D,SASPR,GQCTR)
!=======================================================================
      IMPLICIT NONE
      INTEGER :: I,WBIN,CBIN,PBIN,TBIN,DBIN
      REAL :: TK1D,QV1D,P1D,TC1D,DT,GQCTR,QI1D,AI1D,AS1D,AG1D,AH1D,    &
              I3M1D,QNIN,VI1D,VS1D,VG1D,FI1D,FS1D,AFAC,AFAR,AFAI,AFAS, &
              AFAG,AFAH,ADAGR,QC1D,QR1D,QS1D,QG1D,QH1D,NC1D,NR1D,NI1D, &
              NS1D,NG1D,NH1D,RHOI,RHOS,RHOG,RHOIS,iRHOI,iRHOS,iRHOG,   &
              ZETA,ZETA2,ZETA3,iAMI,iAMG,DI0Z1,DI0Z2,DI0Z3,DI0Z4,IPG,  &
              IPH,IPF,SASPR,DNRI,DNIRM,DNIAG,DNSAG,DNSAC,DNGAC,DNSRM,  &
              DNGRM,AMI,BMI,AMG,AMS,RHO,iRHO,AVI,BVI,AVS,BVS,AVG,BVG,  &
              AVH,BVH,CAPS,SASR1,SASR2,SASR3,SASR4,SASP1,SASP2,SASP3
      REAL :: QHOci,QHOrg,QNMci,QNMrg,QNCci,QFZci,QFZrg,QMLir,QMLic,   &
              QMLsr,QMLgr,QMLhr,QIMii,QIMcsi,QIMrsi,QIMcgi,QIMrgi,     &
              QBKrc,QINig,QINsg,QCNcr,QCNis,QCNgh,QRMci,QRMcs,QRMcg,   &
              QRMch,QRMri,QRMrs,QRMrg,QRMrh,QCLcr,QCLir,QCLis,QCLig,   &
              QCLih,QCLsr,QCLsg,QCLsh,QCLgr,QCLirg,QCLsrg,QCLgrg,QHwsh,&
              QHwml,QHdrm,QCLss
      REAL :: NHOci,NHOrg,NNMci,NNMrg,NNCci,NFZci,NFZrg,NMLir,NMLic,   &
              NMLsr,NMLgr,NMLhr,NIMii,NIMcsi,NIMrsi,NIMcgi,NIMrgi,     &
              NBKrc,NBKrr,NINig,NINsg,NCNcr,NiCNis,NgCNgh,NhCNgh,NRMci,&
              NRMcs,NRMcg,NRMch,NRMri,NRMrs,NRMrg,NRMrh,NCLcc,NCLcr,   &
              NCLrr,NCLir,NCLis,NCLig,NCLih,NCLsr,NCLss,NCLsg,NCLsh,   &
              NCLgr,NCLirg,NCLsrg,NCLgrg,NHwsh,NsCNis
      REAL :: VIMii,VCLis,ViCLis,VsCLis,VCLig,ViCLig,VgCLig,VCLih,     &
              ViCNis,VsCNis,ViINig,VgINig,VFZci,VFZrg,VIMcsi,VMLir,    &
              VMLic,VRMci,VCLir,VHOci,VHOrg,VNMci,VNMrg,VNCci,VIMcgi,  &
              VIMrsi,VIMrgi,VCLss,VCLsg,VsCLsg,VgCLsg,VCLsh,VsINsg,    &
              VgINsg,VMLsr,VRMcs,VCLsr,VMLgr,VRMcg,VCNgh,VCLgr,VCLirg, &
              VCLsrg,VCLgrg,FHOci,FNMci,FNCci,FRMci,FMLir,FMLic,FCLir, &
              FiCLis,FCLig,FIMcsi,FCLih,FiCNis,FINig,FFZci,FIMii,      &
              FIMcgi,FIMrsi,FIMrgi,FsCNis,FsCLis,FCLsg,FCLsh,FINsg,    &
              FMLsr,FRMcs,FCLsr,FCLss
      REAL :: IHOci,INMci,INCci,IRMci,IMLir,IMLic,ICLir,ICLis,ICLig,   &
              IIMcsi,ICLih,ICNis,IINig,IFZci,IIMii,IIMcgi,IIMrsi,      &
              IIMrgi,AHOci,ANMci,ANCci,ARMci,AMLir,AMLic,ACLir,AiCLis, &
              AiCLig,AIMcsi,AiCLih,AiCNis,AsCNis,AiINig,AgINig,AFZci,  &
              AIMii,AIMcgi,AIMrsi,AIMrgi,AMLsr,AsCLis,AsCLsg,AsCLsh,   &
              ARMcs,ACLsr,AgCLig,AsINsg,AgINsg,AMLgr,ARMcg,AgCNgh,     &
              AhCNgh,AgCLsg,ACLgr,ACLirg,ACLsrg,ACLgrg,AHOrg,ANMrg,    &
              AFZrg,AMLhr,ACLss,AhCLih,AhCLsh,ARMch,ARMrg,ARMrh,AHdrm, &
              AHwml,ACLss1
      REAL :: QCSOUR,QCSINK,QRSOUR,QRSINK,QISOUR,QISINK,QSSOUR,QSSINK, &
              QGSOUR,QGSINK,QHSOUR,QHSINK,NCSOUR,NCSINK,NRSOUR,NRSINK, &
              NISOUR,NISINK,NSSOUR,NSSINK,NGSOUR,NGSINK,NHSOUR,NHSINK, &
              FISOUR,FISINK,FSSOUR,FSSINK,VISOUR,VISINK,VSSOUR,VSSINK, &
              VGSOUR,VGSINK,AISOUR,AISINK,ASSOUR,ASSINK,AGSOUR,AGSINK, &
              AHSOUR,AHSINK,IISOUR,IISINK
      REAL :: RHOAJ,CPM,SSRI,XXLS,XXLF,XXLV,QVSI,ESI,ABI,iDT,SSRW,KAP, &
              DV,MUA,SCN,ACTW,ESW,SFCTNW,SFCTNV,EPA,QVSW,TQCI,TQRSG,FF,&
              RFZ,ICED,VOLMC,IJHOF,AREAC,RGIMF,GGIMF,ARIMF0,ARIMF,     &
              COSM2,GEOF2,IJIMF0,IJIMF,GGCNT,NGCNT0,NGCNT1,NGCNT,TCC,  &
              CNTGG0,CNTGG,PSIA,KDIFF,KAPA,CNTKN,CNTFT0,CNTFT1,CNTF1,  &
              CNTF2,CNTFT,IJCNT1,IJCNT2,IJCNT3,DC1,DC2,DC3,INR0,FANGLE,&
              FACTE,DACTE,RATIO,WNRE,CNRE,PNRE,MRATO,MVRC,MVRR,MVDC,   &
              MVDR,MVDI,MVDS,MVDG,MVDH,DSMM,LMVRC,LMVRR,TMP1,GUC,GUR,  &
              RMcsq,RMrsq,RMcgq,RMrgq,RMchq,RMrhq,RMcsa,RMrsa,RMcga,   &
              RMrga,RMcha,RMrha,VTQ0,VTQC,VTQR,VTQI,VTQS,VTQG,VTQH,    &
              VTN0,VTNC,VTNR,VTNI,VTNS,VTNG,VTNH,VTAX,VTAC,VTAR,VTAI,  &
              VTAS,VTAG,VTAH,VTV0,VTVI,VTVS,VTVG,VTF0,VTFI,VTI3M,VTQIC,&
              VTQSC,VTQGC,VTQHC,VTQRI,VTQRS,VTQRG,VTQRH,VTQIS,VTQIG,   &
              VTQIH,VTQSG,VTQSH,VTNIC,VTNSC,VTNGC,VTNHC,VTNRI,VTNRS,   &
              VTNRG,VTNRH,VTNIS,VTNIG,VTNIH,VTNSG,VTNSH,VTARI,VTARS,   &
              VTARG,VTARH,VTAIC,VTASC,VTAGC,VTAHC,VTAIS,VTAIG,VTAIH,   &
              VTASG,VTASH,VTVRI,VTVIC,VTVSC,VTVIS,VTVIG,VTVIH,VTVSG,   &
              VTFRI,VTFIC,VTFIS,VTFIG,VTFIH,EIS,EIG,EIH,ESG,ESH,EII,   &
              ESS,ECI,ECS,ECG,ECH,ERI,ERS,ERG,ERH,EII1,EII2,EIS1,EIS2, &
              ESS1,ESS2,SMLTQ,GMLTQ,HMLTQ,SMLTA,GMLTA,HMLTA,VENQS,     &
              VENQG,VENQH,VENAS,VENAG,VENAH,STOKE,DSLL,NGTAL,MVDX,ABW, &
              FSQC,FSNC,FSQR,FSNR,FSQI,FSNI,FSQS,FSNS,FSQG,FSNG,FSQH,  &
              FSNH,FSAC,FSAR,FSAI,FSAS,FSAG,FSAH,FSVI,FSVS,FSVG,FSFI
      REAL :: DMWDT,DMIDT,HIdqv,HSdqv,HGdqv,HHdqv,HCwqv,HRwqv,HSwqv,   &
              HGwqv,HHwqv,HHdcd,HHdrm,HHdcl,HHdtt,HSwcd,HGwcd,HHwcd,   &
              HSwrm,HGwrm,HHwrm,VDMAX,SBMAX,EVMAX,SUMDEP,SUMSUB,SUMEVP,&
              SUMCND,MLMAX,ESW0,ESI0,QVSW0,QVSI0,XXLF0,MLWM,MWT,MLWC,  &
              DH9,MCORE,ICOR1,DIT,BEST,VENQI,QTMP0,QTMP1,QTMP2,QTMP3,  &
              QTMP4,QTMP5,QTMP6,QTMP7,QTMP8,QTMP9,ATMP1,ATMP2,ATMP3,   &
              ATMP4,NCLS2,BTMP,BSTMP,BGTMP,BHTMP,H2Z,H4Z,QRMC1,NRMC1,  &
              FRMC1,IRMC1,ARMC1,QRMR1,NRMR1,ARMR1,QFZC1,NFZC1,QFZR1,   &
              NFZR1,AFZR1,QCLI1,NCLI1,QCNI1,NCNI1,ACNI1,FCNI1,QCLG1,   &
              NCLG1,ACLG1,ACLI1,FCLI1,QCLS1,NCLS1,ACLS1,LIM1,LIM2,LIMA,&
              LIMB,LIMC,LIMD,LIME,LIMF,DICC,DICA,DIAC,DIAA,DIC0,DIA0,  &
              DIF0,DSS0,DSL0,DIC2,DIA2,DILSV,DSLSV,DGLSV,DILSF,DSLSF,  &
              SMLR,GMLR,SMLF,GMLF,HMLF,AVWSG,BVWSG,CAPWS,WSAPR,RHOWS,  &
              RHOWG,VENWS,VENWG,SSRW0,SSRI0,LLMI,LLMS,LLMG,LLMH,KDX,   &
              RHOIW,RHOSW,RHOGW,RHOIG,RHOIH,RHOSG,RHOSH,RHOHW,KINV,    &
              BEST0,C1X2,VTB1,VTA1,LIM3,LIM4,LIM5,LIM6,LIM7,LIM8
      REAL :: LAMC,LAMR,LAMS,LAMG,LAMH,LAMI,GC2,GC3,GC4,GC5,GC6,GC7,   &
              GR2,GR3,GR4,GR5,GR6,GR7,GI1,GI2,GI2G1,GI2G2,GI2H1,GI3,   &
              GI3H1,GIF1,GIF2,GIG1,GIG2,GIH1,GIH2,GIM1,GIM2,GI4,GI5,   &
              GIM2H1,GI2H3,GI2HG1,GIH2G1,GIF3,GI2G3,GI3G1,GIZM2H1,GIG3,&
              GIM2G1,GIZM2G1,GIM3,GIMF1,GIZMF1,GIMG1,GIZMG1,GIMH1,     &
              GIZMH1,GIZM1,GIZ1,GIZ2G1,GIZF1,GIZG1,GIZH1,GIH3,GS2,GS3, &
              GS4,GS5,GSM1,GSM2,GSM3,GG2,GG3,GG4,GG5,GGM1,GGM2,GGM3,   &
              GH2,GH3,GH4,GH5,Z32G,Z32H,Z3BMI
!-----------------------See CL04 Tables--------------------------------------
      REAL, PARAMETER :: AVTC = 8.8462E+02,  BVTC = 9.7593E+07
      REAL, PARAMETER :: CVTC = -3.4249E+11, AVTR = 2.1454E+00
      REAL, PARAMETER :: BVTR = -2.2812E-04, CVTR = 2.9676E-09
      REAL, PARAMETER :: CQC1 = 2.0901E+01,  CQC2 = 9.9111E-01
      REAL, PARAMETER :: CQC3 = 4.4182E+00,  CNC1 = 1.8276E+01
      REAL, PARAMETER :: CNC2 = 1.0015E+00,  CNC3 = 1.9838E+00
      REAL, PARAMETER :: CQR1 = 1.5943E+01,  CQR2 = 1.1898E+00
      REAL, PARAMETER :: CQR3 = 4.0073E+00,  CNR1 = 9.4791E+00
      REAL, PARAMETER :: CNR2 = 9.7607E-01,  CNR3 = 1.0858E+00
      REAL, PARAMETER :: AQ1 = 6.6793E+0,    BQ1 = 1.0090E+0
      REAL, PARAMETER :: CQ1 = 1.4095E+0,    AQ2 = 9.9912E+0
      REAL, PARAMETER :: BQ2 = -4.7678E-1,   CQ2 = -3.1388E-2
      REAL, PARAMETER :: AN3 = -4.3561E0,    BN3 = 1.9934E0
      REAL, PARAMETER :: CN3 = 1.6465E-2,    AN4 = -4.0731E1
      REAL, PARAMETER :: BN4 = 5.3720E5,     CN4 = -2.0139E-5
      REAL, PARAMETER :: AQ4 = -2.1370E1,    BQ4 = 1.9899E9
      REAL, PARAMETER :: AN5 = 1.5519E1,     BN5 = 3.1491E-0
      REAL, PARAMETER :: CN5 = 4.3989E-1,    AQ5 = 2.0090E1
      REAL, PARAMETER :: BQ5 = 2.9626E0,     CQ5 = 3.2358E0
      REAL, PARAMETER :: AN6 = -1.8239E1,    BN6 = 2.2956E0
      REAL, PARAMETER :: CN6 = -2.3261E-4,   AN7 = -1.7431E2
      REAL, PARAMETER :: BN7 = 2.6031E5,     CN7 = -9.3613E7
      REAL, PARAMETER :: AN8 = -1.6185E2,    BN8 = 2.2786E5
      REAL, PARAMETER :: CN8 = -7.6988E7,    AQ8 = -2.3531E1
      REAL, PARAMETER :: BQ8 = 9.8271E-1,    CQ8 = -1.3202E-1
!---------------------------------------------------------------------------
      REAL, PARAMETER :: DSHED = 1.E-3,     XISP = 0.25                 ! SHEDDING DIAMETER
      REAL, PARAMETER :: ISEPL = 0.6,       ISEPS = 0.6                 ! SEPARATION RATIO, See TC20
      REAL :: ZC1,ZC2,ZC3,ZC4,ZP1,ZP2,ZP3,ZP4                           ! See CT16
      DATA ZC1,ZC2,ZC3,ZC4/0.69509913,-0.46685819,0.30490087,1.62148100/
      DATA ZP1,ZP2,ZP3,ZP4/0.36793126,1.82782890,0.63206874,-1.00164090/

      IF (INSPEC.EQ.1) THEN                                             ! SOOT
         DC1 = -0.5411955; DC2 = 1.879918; DC3 = 1.607947               ! Chen et al. 2008 Table3
         INR0 = 0.4E-7; FANGLE = 33.2; FACTE = 13.8E-20
         DACTE = -20.E-20
      ELSEIF (INSPEC.EQ.2) THEN                                         ! SAHARAN DUST
         DC1 = -0.3353619; DC2 = 1.990979; DC3 = 2.175539               ! Chen et al. 2008 Table3
         INR0 = 1.75E-7; FANGLE = 30.98; FACTE = 15.7E-20
         DACTE = 3.35E-20
      ELSEIF (INSPEC.EQ.3) THEN                                         ! ASIAN DUST
         DC1 = -0.3598818; DC2 = 1.982032; DC3 = 2.025390               ! Chen et al. 2008 Table3
         INR0 = 2.E-7; FANGLE = 30.98; FACTE = 15.7E-20
         DACTE = 1.82E-20
      ELSEIF (INSPEC.EQ.4) THEN                                         ! BACTERIA
         DC1 = -0.3541568; DC2 = 1.983928; DC3 = 2.019456               ! Chen et al. 2008 Table3
         INR0 = 5.E-7; FANGLE = 14.82; FACTE = 17.6E-20
         DACTE = 1.82E-20                                               ! DACTE NOT SURE
      ENDIF

      MVDC   = 0.; MVDR   = 0.; MVDI   = 0.; MVDS   = 0.; MVDG   = 0.
      MVDH   = 0.; MVRC   = 0.; MVRR   = 0.; SMLF   = 0.; GMLF   = 0.
      HMLF   = 0.; FF     = 0.; DMWDT  = 0.; DMIDT  = 0.; ACLss1 = 0.
      HIdqv  = 0.; HSdqv  = 0.; HGdqv  = 0.; HHdqv  = 0.; HCwqv  = 0.
      HRwqv  = 0.; HSwqv  = 0.; HGwqv  = 0.; HHwqv  = 0.; HHdtt  = 0.
      QHOci  = 0.; QHOrg  = 0.; QNMci  = 0.; QNMrg  = 0.; QNCci  = 0.
      QFZci  = 0.; QFZrg  = 0.; QMLir  = 0.; QMLic  = 0.; QMLsr  = 0.
      QMLgr  = 0.; QMLhr  = 0.; QIMii  = 0.; QIMcsi = 0.; QIMrsi = 0.
      QIMcgi = 0.; QIMrgi = 0.; QBKrc  = 0.; QINig  = 0.; QINsg  = 0.
      QCNcr  = 0.; QCNis  = 0.; QCNgh  = 0.; QRMci  = 0.; QRMcs  = 0.
      QRMcg  = 0.; QRMch  = 0.; QRMri  = 0.; QRMrs  = 0.; QRMrg  = 0.
      QRMrh  = 0.; QCLcr  = 0.; QCLir  = 0.; QCLis  = 0.; QCLig  = 0.
      QCLih  = 0.; QCLsr  = 0.; QCLsg  = 0.; QCLsh  = 0.; QCLgr  = 0.
      QCLirg = 0.; QCLsrg = 0.; QCLgrg = 0.; QHwsh  = 0.; QHdrm  = 0.
      QHwml  = 0.; NHOci  = 0.; NHOrg  = 0.; NNMci  = 0.; NNMrg  = 0.
      NNCci  = 0.; NFZci  = 0.; NFZrg  = 0.; NMLir  = 0.; NMLic  = 0.
      NMLsr  = 0.; NMLgr  = 0.; NMLhr  = 0.; NIMii  = 0.; NIMcsi = 0.
      NIMrsi = 0.; NIMcgi = 0.; NIMrgi = 0.; NBKrc  = 0.; NBKrr  = 0.
      NINig  = 0.; NINsg  = 0.; NCNcr  = 0.; NiCNis = 0.; NgCNgh = 0.
      NRMci  = 0.; NRMcs  = 0.; NRMcg  = 0.; NRMch  = 0.; NRMri  = 0.
      NRMrs  = 0.; NRMrg  = 0.; NRMrh  = 0.; NCLcc  = 0.; NCLcr  = 0.
      NCLrr  = 0.; NCLir  = 0.; NCLis  = 0.; NCLig  = 0.; NCLih  = 0.
      NCLsr  = 0.; NCLss  = 0.; NhCNgh = 0.; NCLsg  = 0.; NCLsh  = 0.
      NCLgr  = 0.; NCLirg = 0.; NCLsrg = 0.; NCLgrg = 0.; NHwsh  = 0.
      NsCNis = 0.; VCLss  = 0.; ViCNis = 0.; VIMii  = 0.; ViCLis = 0.
      VsCLis = 0.; ViCLig = 0.; VgCLig = 0.; VCLih  = 0.; VCLsg  = 0.
      VsCNis = 0.; ViINig = 0.; VgINig = 0.; VFZci  = 0.; VFZrg  = 0.
      VMLir  = 0.; VMLic  = 0.; VRMci  = 0.; VCLir  = 0.; VHOci  = 0.
      VHOrg  = 0.; VNMci  = 0.; VNMrg  = 0.; VNCci  = 0.; VIMcsi = 0.
      VIMcgi = 0.; VIMrsi = 0.; VIMrgi = 0.; VCLsr  = 0.; VsCLsg = 0.
      VgCLsg = 0.; VCLsh  = 0.; VsINsg = 0.; VgINsg = 0.; VMLsr  = 0.
      VRMcs  = 0.; VMLgr  = 0.; VRMcg  = 0.; VCNgh  = 0.; VCLgr  = 0.
      VCLirg = 0.; VCLsrg = 0.; VCLgrg = 0.; VCLis  = 0.; VCLig  = 0.
      FIMcsi = 0.; FIMcgi = 0.; FIMrsi = 0.; FIMrgi = 0.; FCLss  = 0.
      FIMii  = 0.; FHOci  = 0.; FNMci  = 0.; FNCci  = 0.; FFZci  = 0.
      FMLir  = 0.; FCLir  = 0.; FiCLis = 0.; FCLig  = 0.; FCLih  = 0.
      FRMci  = 0.; FiCNis = 0.; FINig  = 0.; FMLic  = 0.; FsCNis = 0.
      FsCLis = 0.; FCLsg  = 0.; FCLsh  = 0.; FINsg  = 0.; FMLsr  = 0.
      FRMcs  = 0.; FCLsr  = 0.; IINig  = 0.; IIMcsi = 0.; IIMcgi = 0.
      IIMrsi = 0.; IIMrgi = 0.; IIMii  = 0.; IHOci  = 0.; INMci  = 0.
      INCci  = 0.; IFZci  = 0.; IMLir  = 0.; IMLic  = 0.; ICLir  = 0.
      ICLis  = 0.; ICLig  = 0.; ICLih  = 0.; IRMci  = 0.; ICNis  = 0.
      ACLss  = 0.; AIMcsi = 0.; AIMcgi = 0.; AIMrsi = 0.; AIMrgi = 0.
      AIMii  = 0.; AHOci  = 0.; ANMci  = 0.; ANCci  = 0.; AFZci  = 0.
      AMLic  = 0.; AMLir  = 0.; ACLir  = 0.; AiCLis = 0.; AiCLig = 0.
      AiCLih = 0.; ARMci  = 0.; AiCNis = 0.; AiINig = 0.; AgINig = 0.
      AsINsg = 0.; AgINsg = 0.; AsCLis = 0.; AsCLsg = 0.; AsCLsh = 0.
      ARMcs  = 0.; ACLsr  = 0.; AMLsr  = 0.; AgCLig = 0.; AgCLsg = 0.
      AMLgr  = 0.; ARMcg  = 0.; AgCNgh = 0.; ACLgr  = 0.; ACLirg = 0.
      ACLsrg = 0.; ACLgrg = 0.; AHOrg  = 0.; ANMrg  = 0.; AFZrg  = 0.
      AMLhr  = 0.; AhCLih = 0.; AhCLsh = 0.; ARMch  = 0.; ARMrg  = 0.
      ARMrh  = 0.; AHdrm  = 0.; AHwml  = 0.; AsCNis = 0.; AhCNgh = 0.

      iDT    = 1./DT
      iRHO   = 1./RHO
      TC1D   = TK1D-TK0C
      IF (TK1D.GE.265..AND.TK1D.LT.268.) FF = (TC1D+8.)*THRD
      IF (TK1D.GE.268..AND.TK1D.LE.270.) FF = (-3.-TC1D)*0.5
      RHOAJ  = (RHOSU/RHO)**0.54
      CPM    = CP*(1.+0.887*QV1D)
      ESW    = POLYSVP(TK1D,0)
      ESI    = POLYSVP(TK1D,1)
      QVSW   = 0.622*ESW/(P1D-ESW)
      QVSI   = 0.622*ESI/(P1D-ESI)
      SSRW   = QV1D/QVSW-1.
      SSRI   = QV1D/QVSI-1.
      ESW0   = POLYSVP(TK0C,0)
      QVSW0  = 0.622*ESW0/(P1D-ESW0)
      ESI0   = POLYSVP(TK0C,1)
      QVSI0  = 0.622*ESI0/(P1D-ESI0)
      XXLV   = 3.1484E6-2370.*TK1D
      XXLS   = 3.15E6-2370.*TK1D+0.3337E6
      XXLF   = 2836310.8-(3.1484E6-2370.*TK1D)
      XXLF0  = 2836310.8-(3.1484E6-2370.*TK0C)
      SSRW0  = QV1D/QVSW0-1.
      SSRI0  = QV1D/QVSI0-1.
      DV     = 2.11E-5*(TK1D/TK0C)**1.94*(101325./P1D)
      KAP    = 2.3971E-2+0.0078E-2*TC1D
      MUA    = 1.72E-5*(393./(TK1D+120.))*(TK1D/TK0C)**1.5
      SCN    = (MUA/(RHO*DV))**THRD
      ICED   = 916.7-0.175*TC1D-5.E-4*TC1D**2.                          ! ICE DENSITY
      ABW    = TK1D*RV/ESW/DV+XXLV*(XXLV/TK1D/RV-1.)/TK1D/KAP
      ABI    = TK1D*RV/ESI/DV+XXLS*(XXLS/TK1D/RV-1.)/TK1D/KAP
      SFCTNV = (0.095*TC1D+104.6)*1.E-3                                 ! SURFACE TENSION OVER ICE/VAPOR
      SFCTNW = (28.+0.25*TC1D)*1.E-3
      COSM2  = LOG(1.-COS(FANGLE*PI/180.))
      ACTW   = LOG(1.*ESW/ESI)                                          ! WATER ACTIVITY NEED TO BE MODIFIED
      EIH    = MIN(1.,0.01*EXP(0.1*TC1D))                               ! FERRIER ET AL., 1995
      ESH    = MIN(1.,0.01*EXP(0.1*TC1D))                               ! FERRIER ET AL., 1995

      IF (QC1D.GE.QSMALL) THEN
         CALL SOLVE_AFAC(TK1D,QC1D,NC1D,LAMC,MVDC,AFAC)
         GC2   = EXP(GAMLN(AFAC+2.)-GAMLN(AFAC+1.)-LOG(LAMC))
         GC3   = EXP(GAMLN(AFAC+3.)-GAMLN(AFAC+1.)-2.*LOG(LAMC))
         GC4   = EXP(GAMLN(AFAC+4.)-GAMLN(AFAC+1.)-3.*LOG(LAMC))
         GC5   = EXP(GAMLN(AFAC+5.)-GAMLN(AFAC+1.)-4.*LOG(LAMC))
         GC6   = EXP(GAMLN(AFAC+6.)-GAMLN(AFAC+1.)-5.*LOG(LAMC))
         GC7   = EXP(GAMLN(AFAC+7.)-GAMLN(AFAC+1.)-6.*LOG(LAMC))
         MVRC  = MIN(MAX((QC1D/NC1D/C4PI3W)**THRD,RCMIN),RCMAX)
         LMVRC = LOG(MVRC)
         GUC   = EXP(EXP(AFU+BFU*LMVRC**3.+CFU*SQRT(RHO)**3.))
         IF (LIQ_VTC.EQ.0) THEN
            FSQC = EXP(GAMLN(BVC0+BMW+AFAC+1.)-GAMLN(BMW+AFAC+1.)-BVC0*&
                   LOG(LAMC))
            FSNC = EXP(GAMLN(BVC0+AFAC+1.)-GAMLN(AFAC+1.)-BVC0*        &
                   LOG(LAMC))
            FSAC = EXP(GAMLN(BVC0+AFAC+3.)-GAMLN(AFAC+3.)-BVC0*        &
                   LOG(LAMC))
            VTQC = RHOAJ*FSQC*AVC0
            VTNC = RHOAJ*FSNC*AVC0
            VTAC = RHOAJ*FSAC*AVC0
         ELSEIF (LIQ_VTC.EQ.1) THEN
            VTQC = EXP(CQC1+CQC2*LOG(NC1D)+CQC3*LOG(MVRC))*GUC/QC1D
            VTNC = EXP(CNC1+CNC2*LOG(NC1D)+CNC3*LOG(MVRC))*GUC/NC1D
            VTAC = VTQC**(3./4.)*VTNC**(1./4.)
         ELSEIF (LIQ_VTC.EQ.2) THEN
            VTQC = MVRC*(AVTC+BVTC*MVRC+CVTC*MVRC**2.)*GUC
            VTNC = MVRC*(AVTC+BVTC*MVRC+CVTC*MVRC**2.)*GUC
            VTAC = VTQC**(3./4.)*VTNC**(1./4.)
         ENDIF
         VTQC  = MIN(VTQC,VTCMAX)
         VTNC  = MIN(VTNC,VTCMAX)
         VTAC  = MIN(VTAC,VTCMAX)
         QRMC1 = PI*PI*RHOW*NC1D/24.
         NRMC1 = PI*NC1D/4.
         FRMC1 = PI*RHOW*NC1D
         IRMC1 = PI*RHOW*NC1D/4.
         ARMC1 = PI*NC1D/4.
         QFZC1 = PI*PI*RHOW*NC1D*GC7/36.
         NFZC1 = PI*NC1D*GC4/6.
         HCwqv = SSRW0*EXP(AQ1+BQ1*LOG(NC1D)+CQ1*LMVRC)/ABW
      ENDIF
      IF (QR1D.GE.QSMALL) THEN
         CALL SOLVE_AFAR(TK1D,QR1D,NR1D,LAMR,MVDR,AFAR)
         GR2   = EXP(GAMLN(AFAR+2.)-GAMLN(AFAR+1.)-LOG(LAMR))
         GR3   = EXP(GAMLN(AFAR+3.)-GAMLN(AFAR+1.)-2.*LOG(LAMR))
         GR4   = EXP(GAMLN(AFAR+4.)-GAMLN(AFAR+1.)-3.*LOG(LAMR))
         GR5   = EXP(GAMLN(AFAR+5.)-GAMLN(AFAR+1.)-4.*LOG(LAMR))
         GR6   = EXP(GAMLN(AFAR+6.)-GAMLN(AFAR+1.)-5.*LOG(LAMR))
         GR7   = EXP(GAMLN(AFAR+6.)-GAMLN(AFAR+1.)-6.*LOG(LAMR))
         MVRR  = MIN(MAX((QR1D/NR1D/C4PI3W)**THRD,RRMIN),RRMAX)
         LMVRR = LOG(MVRR)
         GUR   = EXP(EXP(AFU+BFU*LMVRR**3.+CFU*SQRT(RHO)**3.))
         IF (LIQ_VTR.EQ.0) THEN
            FSQR = EXP(GAMLN(BVR0+BMW+AFAR+1.)-GAMLN(BMW+AFAR+1.)-BVR0*&
                   LOG(LAMR))
            FSNR = EXP(GAMLN(BVR0+AFAR+1.)-GAMLN(AFAR+1.)-BVR0*        &
                   LOG(LAMR))
            FSAR = EXP(GAMLN(BVR0+AFAR+3.)-GAMLN(AFAR+3.)-BVR0*        &
                   LOG(LAMR))
            VTQR = RHOAJ*FSQR*AVR0
            VTNR = RHOAJ*FSNR*AVR0
            VTAR = RHOAJ*FSAR*AVR0
         ELSEIF (LIQ_VTR.EQ.1) THEN
            VTQR = EXP(CQR1+CQR2*LOG(NR1D)+CQR3*LOG(MVRR))*GUR/QR1D
            VTNR = EXP(CNR1+CNR2*LOG(NR1D)+CNR3*LOG(MVRR))*GUR/NR1D
            VTAR = VTQR**(3./4.)*VTNR*(1./4.)
         ELSEIF (LIQ_VTR.EQ.2) THEN
            VTQR = EXP(AVTR+BVTR/MVRR+CVTR/(MVRR**2.))*GUR
            VTNR = EXP(AVTR+BVTR/MVRR+CVTR/(MVRR**2.))*GUR
            VTAR = VTQR**(3./4.)*VTNR*(1./4.)
         ENDIF
         VTQR  = MIN(VTQR,VTRMAX)
         VTNR  = MIN(VTNR,VTRMAX)
         VTAR  = MIN(VTAR,VTRMAX)
         QRMR1 = PI*PI*RHOW*NR1D/24.
         NRMR1 = PI*NR1D/4.
         ARMR1 = PI*NR1D/4.
         QFZR1 = PI*PI*RHOW*NR1D*GR7/36.
         NFZR1 = PI*NR1D*GR4/6.
         AFZR1 = PI*NR1D*GR6/6.
         HRwqv = SSRW0*NR1D*EXP(AQ2+(BQ2+CQ2*LMVRR)*LMVRR**2.)/ABW
      ENDIF
      IF (QI1D.GE.QSMALL) THEN
         CALL SOLVE_AFAI(TK1D,P1D,RHO,QV1D,QI1D,NI1D,VI1D,FI1D,AI1D,   &
              I3M1D,ADAGR,ZETA,LAMI,AFAI,MVDI,RHOI,AMI,BMI,AVI,BVI,    &
              BEST)
         IPG    = 3./(ADAGR+2.)
         IPH    = 3.*ADAGR/(ADAGR+2.)
         IPF    = 3.*(ADAGR+1.)/(ADAGR+2.)
         ZETA2  = 2.*(ADAGR-1.)/(ADAGR+2.)
         ZETA3  = 3.*(ADAGR-1.)/(ADAGR+2.)
         DI0Z1  = DI0**ZETA
         DI0Z2  = DI0**ZETA2
         DI0Z3  = DI0**ZETA3
         DI0Z4  = DI0**(4.*ZETA)
         iRHOI  = 1./RHOI
         iAMI   = 1./AMI
         QCLI1  = PI*AMI*NI1D/4.
         QCNI1  = PI*XISP*AMI*NI1D/6.
         NCLI1  = PI*NI1D/4.
         NCNI1  = PI*XISP*NI1D/6.
         ACLI1  = PI*NI1D/4.
         ACNI1  = PI*XISP*NI1D/6.
         FCLI1  = PI*NI1D/4./DI0Z3
         FCNI1  = AMI*XISP*iRHOI*NI1D
         Z32G   = ZETA3+2.*IPG
         Z32H   = ZETA3+2.*IPH
         Z3BMI  = ZETA3+BMI
         LLMI   = LOG(LAMI)
         GI1    = GAMLN(AFAI+1.)
         GI2    = EXP(GAMLN(AFAI+2.)-GI1-LLMI)
         GI3    = EXP(GAMLN(AFAI+3.)-GI1-2.*LLMI)
         GI4    = EXP(GAMLN(AFAI+4.)-GI1-3.*LLMI)
         GI5    = EXP(GAMLN(AFAI+5.)-GI1-4.*LLMI)
         GIM1   = EXP(GAMLN(AFAI+BMI+1.)-GI1-BMI*LLMI)
         GIM2   = EXP(GAMLN(AFAI+BMI+2.)-GI1-(BMI+1.)*LLMI)
         GIM3   = EXP(GAMLN(AFAI+BMI+3.)-GI1-(BMI+2.)*LLMI)
         GIF1   = EXP(GAMLN(AFAI+IPF+1.)-GI1-IPF*LLMI)
         GIF2   = EXP(GAMLN(AFAI+IPF+2.)-GI1-(IPF+1.)*LLMI)
         GIF3   = EXP(GAMLN(AFAI+IPF+3.)-GI1-(IPF+2.)*LLMI)
         GIG1   = EXP(GAMLN(AFAI+IPG+1.)-GI1-IPG*LLMI)
         GIG2   = EXP(GAMLN(AFAI+IPG+2.)-GI1-(IPG+1.)*LLMI)
         GIG3   = EXP(GAMLN(AFAI+IPG+3.)-GI1-(IPG+2.)*LLMI)
         GIH1   = EXP(GAMLN(AFAI+IPH+1.)-GI1-IPH*LLMI)
         GIH2   = EXP(GAMLN(AFAI+IPH+2.)-GI1-(IPH+1.)*LLMI)
         GIH3   = EXP(GAMLN(AFAI+IPH+3.)-GI1-(IPH+2.)*LLMI)
         GIZ1   = EXP(GAMLN(AFAI+ZETA3+1.)-GI1-ZETA3*LLMI)
         GI2G1  = EXP(GAMLN(AFAI+2.*IPG+1.)-GI1-2.*IPG*LLMI)
         GI2G2  = EXP(GAMLN(AFAI+2.*IPG+2.)-GI1-(2.*IPG+1.)*LLMI)
         GI2G3  = EXP(GAMLN(AFAI+2.*IPG+3.)-GI1-(2.*IPG+2.)*LLMI)
         GI3G1  = EXP(GAMLN(AFAI+3.*IPG+1.)-GI1-3.*IPG*LLMI)
         GI2H1  = EXP(GAMLN(AFAI+2.*IPH+1.)-GI1-2.*IPH*LLMI)
         GI2H3  = EXP(GAMLN(AFAI+2.*IPH+3.)-GI1-(2.*IPH+2.)*LLMI)
         GI3H1  = EXP(GAMLN(AFAI+3.*IPH+1.)-GI1-3.*IPH*LLMI)
         GIMF1  = EXP(GAMLN(AFAI+BMI+IPF+1.)-GI1-(BMI+IPF)*LLMI)
         GIMG1  = EXP(GAMLN(AFAI+BMI+IPG+1.)-GI1-(BMI+IPG)*LLMI)
         GIMH1  = EXP(GAMLN(AFAI+BMI+IPH+1.)-GI1-(BMI+IPH)*LLMI)
         GIZM1  = EXP(GAMLN(AFAI+ZETA3+BMI+1.)-GI1-(ZETA3+BMI)*LLMI)
         GIZF1  = EXP(GAMLN(AFAI+ZETA3+IPF+1.)-GI1-(ZETA3+IPF)*LLMI)
         GIZG1  = EXP(GAMLN(AFAI+ZETA3+IPG+1.)-GI1-(ZETA3+IPG)*LLMI)
         GIZH1  = EXP(GAMLN(AFAI+ZETA3+IPH+1.)-GI1-(ZETA3+IPH)*LLMI)
         GI2HG1 = EXP(GAMLN(AFAI+2.*IPH+IPG+1.)-GI1-(2.*IPH+IPG)*LLMI)
         GIH2G1 = EXP(GAMLN(AFAI+IPH+2.*IPG+1.)-GI1-(IPH+2.*IPG)*LLMI)
         GIM2G1 = EXP(GAMLN(AFAI+BMI+2.*IPG+1.)-GI1-(BMI+2.*IPG)*LLMI)
         GIM2H1 = EXP(GAMLN(AFAI+BMI+2.*IPH+1.)-GI1-(BMI+2.*IPH)*LLMI)
         GIZ2G1 = EXP(GAMLN(AFAI+Z32G+1.)-GI1-Z32G*LLMI)
         GIZMF1 = EXP(GAMLN(AFAI+Z3BMI+IPF+1.)-GI1-(Z3BMI+IPF)*LLMI)
         GIZMG1 = EXP(GAMLN(AFAI+Z3BMI+IPG+1.)-GI1-(Z3BMI+IPG)*LLMI)
         GIZMH1 = EXP(GAMLN(AFAI+Z3BMI+IPH+1.)-GI1-(Z3BMI+IPH)*LLMI)
         GIZM2G1 = EXP(GAMLN(AFAI+Z32G+BMI+1.)-GI1-(Z32G+BMI)*LLMI)
         GIZM2H1 = EXP(GAMLN(AFAI+Z32H+BMI+1.)-GI1-(Z32H+BMI)*LLMI)
         FSQI  = EXP(GAMLN(BVI+BMI+AFAI+1.)-GAMLN(BMI+AFAI+1.)-BVI*LLMI)
         FSNI  = EXP(GAMLN(BVI+AFAI+1.)-GI1-BVI*LLMI)
         FSVI  = EXP(GAMLN(BVI+AFAI+4.)-GAMLN(AFAI+4.)-BVI*LLMI)
         VTQI  = MIN(RHOAJ*FSQI*AVI,VTIMAX)
         VTNI  = MIN(RHOAJ*FSNI*AVI,VTIMAX)
         VTVI  = MIN(RHOAJ*FSVI*AVI,VTIMAX)
         IF (AI1D.GE.ASMALL) THEN
            FSAI = EXP(GAMLN(BVI+AFAI+3.)-GAMLN(AFAI+3.)-BVI*LLMI)
            VTAI = MIN(RHOAJ*FSAI*AVI,VTIMAX)
         ENDIF
         IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
            FSFI  = EXP(GAMLN(BVI+ZETA3+AFAI+4.)-GAMLN(ZETA3+AFAI+4.)- &
                    BVI*LLMI)
            FSVI  = EXP(GAMLN(BVI+AFAI+4.)-GAMLN(AFAI+4.)-BVI*LLMI)
            VTFI  = MIN(RHOAJ*FSFI*AVI,VTIMAX)
            VTI3M = MIN(RHOAJ*FSVI*AVI,VTIMAX)
         ENDIF
      IF (ICE_VENT.EQ.3) THEN
         IF ((ADAGR-1.).GE.SLIMIT) THEN
            BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
            H2Z   = ZC2*ZETA
            H4Z   = ZC4*ZETA
            QTMP0 = EXP(GAMLN(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
            QTMP1 = EXP(GAMLN(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
            QTMP2 = LLMI*(H2Z+BVI/2.+IPH/2.+1.)
            QTMP3 = LLMI*(H4Z+BVI/2.+IPH/2.+1.)
            QTMP4 = EXP(GAMLN(H2Z+BVI/2.+IPH/2.+AFAI+2.)-GI1-QTMP2)
            QTMP5 = EXP(GAMLN(H4Z+BVI/2.+IPH/2.+AFAI+2.)-GI1-QTMP3)
            QTMP6 = LLMI*(H2Z+BVI+IPH+1.)
            QTMP7 = LLMI*(H4Z+BVI+IPH+1.)
            QTMP8 = EXP(GAMLN(H2Z+BVI+IPH+AFAI+2.)-GI1-QTMP6)
            QTMP9 = EXP(GAMLN(H4Z+BVI+IPH+AFAI+2.)-GI1-QTMP7)
            VENQI = ZC1*QTMP0/DI0**H2Z+ZC3*QTMP1/DI0**H4Z+VENC1*ZC1*   &
                    BTMP*QTMP4/DI0**(H2Z+ZETA)+VENC1*ZC3*BTMP*QTMP5/   &
                    DI0**(H4Z+ZETA)+VENC2*ZC1*BTMP**2.*QTMP8/DI0**(H2Z+&
                    ZETA2)+VENC2*ZC3*BTMP**2.*QTMP9/DI0**(H4Z+ZETA2)
         ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
            BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
            H2Z   = ZP2*ZETA
            H4Z   = ZP4*ZETA
            QTMP0 = EXP(GAMLN(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
            QTMP1 = EXP(GAMLN(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
            QTMP2 = LLMI*(H2Z+BVI/2.+IPG/2.+1.)
            QTMP3 = LLMI*(H4Z+BVI/2.+IPG/2.+1.)
            QTMP4 = EXP(GAMLN(H2Z+BVI/2.+IPG/2.+AFAI+2.)-GI1-QTMP2)
            QTMP5 = EXP(GAMLN(H4Z+BVI/2.+IPG/2.+AFAI+2.)-GI1-QTMP3)
            QTMP6 = LLMI*(H2Z+BVI+IPG+1.)
            QTMP7 = LLMI*(H4Z+BVI+IPG+1.)
            QTMP8 = EXP(GAMLN(H2Z+BVI+IPG+AFAI+2.)-GI1-QTMP6)
            QTMP9 = EXP(GAMLN(H4Z+BVI+IPG+AFAI+2.)-GI1-QTMP7)
            VENQI = ZP1*QTMP0/DI0**H2Z+ZP3*QTMP1/DI0**H4Z+VENP1*ZP1*   &
                    BTMP*QTMP4/DI0**(H2Z-ZETA/2.)+VENP1*ZP3*BTMP*QTMP5/&
                    DI0**(H4Z-ZETA/2.)+VENP2*ZP1*BTMP**2.*QTMP8/DI0**  &
                    (H2Z-ZETA)+VENP2*ZP3*BTMP**2.*QTMP9/DI0**(H4Z-ZETA)
         ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
            BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
            QTMP0 = EXP(GAMLN(AFAI+2.)-GI1-LOG(LAMI))
            QTMP1 = LLMI*(1.5+BVI/2.)
            QTMP2 = EXP(GAMLN(BVI/2.+AFAI+2.5)-GI1-QTMP1)
            VENQI = AVSG*QTMP0+BVSG*BTMP*QTMP2
         ENDIF
      ELSEIF (ICE_VENT.EQ.1.OR.ICE_VENT.EQ.2) THEN
         IF ((ADAGR-1.).GE.SLIMIT) THEN
            H2Z   = ZC2*ZETA
            H4Z   = ZC4*ZETA
            QTMP0 = EXP(GAMLN(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
            QTMP1 = EXP(GAMLN(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
            IF (BEST.LE.1.) THEN
               BTMP  = SCN**2.*(AVI*RHOAJ/MUA)
               QTMP2 = LLMI*(H2Z+BVI+IPH+1.)
               QTMP3 = LLMI*(H4Z+BVI+IPH+1.)
               QTMP4 = EXP(GAMLN(H2Z+BVI+IPH+AFAI+2.)-GI1-QTMP2)
               QTMP5 = EXP(GAMLN(H4Z+BVI+IPH+AFAI+2.)-GI1-QTMP3)
               VENQI = AVIS*ZC1*QTMP0/DI0**H2Z+AVIS*ZC3*QTMP1/DI0**H4Z+&
                       BVIS*ZC1*BTMP*QTMP4/DI0**(H2Z+ZETA2)+BVIS*ZC3*  &
                       BTMP*QTMP5/DI0**(H4Z+ZETA2)
            ELSEIF (BEST.GT.1.) THEN
               BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
               QTMP2 = LLMI*(H2Z+BVI/2.+IPH/2.+1.)
               QTMP3 = LLMI*(H4Z+BVI/2.+IPH/2.+1.)
               QTMP4 = EXP(GAMLN(H2Z+BVI/2.+IPH/2.+AFAI+2.)-GI1-QTMP2)
               QTMP5 = EXP(GAMLN(H4Z+BVI/2.+IPH/2.+AFAI+2.)-GI1-QTMP3)
               VENQI = AVSG*ZC1*QTMP0/DI0**H2Z+AVSG*ZC3*QTMP1/DI0**H4Z+&
                       BVSG*ZC1*BTMP*QTMP4/DI0**(H2Z+ZETA)+BVSG*ZC3*   &
                       BTMP*QTMP5/DI0**(H4Z+ZETA)
            ENDIF
         ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
            H2Z   = ZP2*ZETA
            H4Z   = ZP4*ZETA
            QTMP0 = EXP(GAMLN(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
            QTMP1 = EXP(GAMLN(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
            IF (BEST.LE.1.) THEN
               BTMP  = SCN**2.*(AVI*RHOAJ/MUA)
               QTMP2 = LLMI*(H2Z+BVI+IPG+1.)
               QTMP3 = LLMI*(H4Z+BVI+IPG+1.)
               QTMP4 = EXP(GAMLN(H2Z+BVI+IPG+AFAI+2.)-GI1-QTMP2)
               QTMP5 = EXP(GAMLN(H4Z+BVI+IPG+AFAI+2.)-GI1-QTMP3)
               VENQI = AVIS*ZP1*QTMP0/DI0**H2Z+AVIS*ZP3*QTMP1/DI0**H4Z+&
                       BVIS*ZP1*BTMP*QTMP4/DI0**(H2Z-ZETA)+BVIS*ZP3*   &
                       BTMP*QTMP5/DI0**(H4Z-ZETA)
            ELSEIF (BEST.GT.1.) THEN
               BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
               QTMP2 = LLMI*(H2Z+BVI/2.+IPG/2.+1.)
               QTMP3 = LLMI*(H4Z+BVI/2.+IPG/2.+1.)
               QTMP4 = EXP(GAMLN(H2Z+BVI/2.+IPG/2.+AFAI+2.)-GI1-QTMP2)
               QTMP5 = EXP(GAMLN(H4Z+BVI/2.+IPG/2.+AFAI+2.)-GI1-QTMP3)
               VENQI = AVSG*ZP1*QTMP0/DI0**H2Z+AVSG*ZP3*QTMP1/DI0**H4Z+&
                       BVSG*ZP1*BTMP*QTMP4/DI0**(H2Z-ZETA/2.)+BVSG*ZP3*&
                       BTMP*QTMP5/DI0**(H4Z-ZETA/2.)
            ENDIF
         ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
            QTMP0 = EXP(GAMLN(AFAI+2.)-GI1-LOG(LAMI))
            IF (BEST.LE.1.) THEN
               BTMP  = SCN**2.*(AVI*RHOAJ/MUA)
               QTMP1 = LLMI*(BVI+2.)
               QTMP2 = EXP(GAMLN(BVI+AFAI+3.)-GI1-QTMP1)
               VENQI = AVIS*QTMP0+BVIS*BTMP*QTMP2
            ELSEIF (BEST.GT.1.) THEN
               BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
               QTMP1 = LLMI*(1.5+BVI/2.)
               QTMP2 = EXP(GAMLN(BVI/2.+AFAI+2.5)-GI1-QTMP1)
               VENQI = AVSG*QTMP0+BVSG*BTMP*QTMP2
            ENDIF
         ENDIF
      ELSEIF (ICE_VENT.EQ.0) THEN
         IF ((ADAGR-1.).GE.SLIMIT) THEN
            H2Z   = ZC2*ZETA
            H4Z   = ZC4*ZETA
            QTMP0 = EXP(GAMLN(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
            QTMP1 = EXP(GAMLN(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
            VENQI = ZC1*QTMP0/DI0**H2Z+ZC3*QTMP1/DI0**H4Z
         ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
            H2Z   = ZP2*ZETA
            H4Z   = ZP4*ZETA
            QTMP0 = EXP(GAMLN(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
            QTMP1 = EXP(GAMLN(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
            VENQI = ZP1*QTMP0/DI0**H2Z+ZP3*QTMP1/DI0**H4Z
         ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
            VENQI = EXP(GAMLN(AFAI+2.)-GI1-LOG(LAMI))
         ENDIF
      ENDIF                                                             ! ICE_VENT
      HIdqv = 2.*PI*NI1D*VENQI*XXLS*SSRI0/ABI
      ENDIF                                                             ! QI QSMALL
      IF (QS1D.GE.QSMALL) THEN
         CALL SOLVE_AFAS(TK1D,RHO,QS1D,QC1D,NS1D,VS1D,FS1D,AS1D,AFAS,  &
              LAMS,MVDS,RHOS,SASPR,AMS,AVS,BVS)
         LLMS  = LOG(LAMS)
         GS2   = EXP(GAMLN(AFAS+2.)-GAMLN(AFAS+1.)-LLMS)
         GS3   = EXP(GAMLN(AFAS+3.)-GAMLN(AFAS+1.)-2.*LLMS)
         GS4   = EXP(GAMLN(AFAS+4.)-GAMLN(AFAS+1.)-3.*LLMS)
         GS5   = EXP(GAMLN(AFAS+5.)-GAMLN(AFAS+1.)-4.*LLMS)
         GSM1  = EXP(GAMLN(AFAS+BMS+1.)-GAMLN(AFAS+1.)-BMS*LLMS)
         GSM2  = EXP(GAMLN(AFAS+BMS+2.)-GAMLN(AFAS+1.)-(BMS+1.)*LLMS)
         GSM3  = EXP(GAMLN(AFAS+BMS+3.)-GAMLN(AFAS+1.)-(BMS+2.)*LLMS)
         iRHOS = 1./RHOS
         FSQS  = EXP(GAMLN(BVS+BMS+AFAS+1.)-GAMLN(BMS+AFAS+1.)-BVS*LLMS)
         FSNS  = EXP(GAMLN(BVS+AFAS+1.)-GAMLN(AFAS+1.)-BVS*LLMS)
         FSVS  = EXP(GAMLN(BVS+AFAS+4.)-GAMLN(AFAS+4.)-BVS*LOG(LAMS))
         VTQS  = MIN(RHOAJ*FSQS*AVS,VTSMAX)
         VTNS  = MIN(RHOAJ*FSNS*AVS,VTSMAX)
         VTVS  = MIN(RHOAJ*FSVS*AVS,VTSMAX)
         IF (AS1D.GE.ASMALL) THEN
            FSAS = EXP(GAMLN(BVS+AFAS+3.)-GAMLN(AFAS+3.)-BVS*LLMS)
            VTAS = MIN(RHOAJ*FSAS*AVS,VTSMAX)
         ENDIF
         QCLS1 = PI*AMS*NS1D/4.
         NCLS1 = PI*NS1D/4.
         ACLS1 = PI*NS1D/4.
         BSTMP = SCN*SQRT(AVS*RHOAJ/MUA)
         QTMP1 = LLMS*(1.5+BVS/2.)
         QTMP2 = EXP(GAMLN(AFAS+2.)-GAMLN(AFAS+1.)-LLMS)
         QTMP3 = EXP(GAMLN(BVS/2.+AFAS+2.5)-GAMLN(AFAS+1.)-QTMP1)
         CAPS  = ZP1*SASPR**(ZP2/3.)+ZP3*SASPR**(ZP4/3.)
         VENQS = AVSG*QTMP2*CAPS+BVSG*BSTMP*QTMP3*CAPS
         SASR1 = SASPR**(-1./3.)
         SASR2 = SASPR**(-2./3.)
         SASR3 = SASPR**(1./3.)
         SASR4 = SASPR**(2./3.)
         HSdqv = 2.*PI*NS1D*VENQS*XXLS*SSRI0/ABI
         HSwqv = 2.*PI*NS1D*VENQS*XXLV*SSRW0/ABW
      ENDIF
      IF (QG1D.GE.QSMALL) THEN
         CALL SOLVE_AFAG(TK1D,RHO,QG1D,QC1D,NG1D,VG1D,AG1D,LAMG,AFAG,  &
              MVDG,RHOG,AMG,AVG,BVG)
         LLMG  = LOG(LAMG)
         GG2   = EXP(GAMLN(AFAG+2.)-GAMLN(AFAG+1.)-LLMG)
         GG3   = EXP(GAMLN(AFAG+3.)-GAMLN(AFAG+1.)-2.*LLMG)
         GG4   = EXP(GAMLN(AFAG+4.)-GAMLN(AFAG+1.)-3.*LLMG)
         GG5   = EXP(GAMLN(AFAG+5.)-GAMLN(AFAG+1.)-4.*LLMG)
         GGM1  = EXP(GAMLN(AFAG+BMG+1.)-GAMLN(AFAG+1.)-BMG*LLMG)
         GGM2  = EXP(GAMLN(AFAG+BMG+2.)-GAMLN(AFAG+1.)-(BMG+1.)*LLMG)
         GGM3  = EXP(GAMLN(AFAG+BMG+3.)-GAMLN(AFAG+1.)-(BMG+2.)*LLMG)
         iAMG  = 1./AMG
         iRHOG = 1./RHOG
         FSQG  = EXP(GAMLN(BVG+BMG+AFAG+1.)-GAMLN(BMG+AFAG+1.)-BVG*LLMG)
         FSNG  = EXP(GAMLN(BVG+AFAG+1.)-GAMLN(AFAG+1.)-BVG*LLMG)
         FSVG  = EXP(GAMLN(BVG+AFAG+4.)-GAMLN(AFAG+4.)-BVG*LOG(LAMG))
         VTQG  = MIN(RHOAJ*FSQG*AVG,VTGMAX)
         VTNG  = MIN(RHOAJ*FSNG*AVG,VTGMAX)
         VTVG  = MIN(RHOAJ*FSVG*AVG,VTGMAX)
         IF (AG1D.GE.ASMALL) THEN
            FSAG = EXP(GAMLN(BVG+AFAG+3.)-GAMLN(AFAG+3.)-BVG*LLMG)
            VTAG = MIN(RHOAJ*FSAG*AVG,VTGMAX)
         ENDIF
         QCLG1 = PI*AMG*NG1D/4.
         NCLG1 = PI*NG1D/4.
         ACLG1 = PI*NG1D/4.
         BGTMP = SCN*SQRT(AVG*RHOAJ/MUA)
         QTMP1 = LLMG*(1.5+BVG/2.)
         QTMP2 = EXP(GAMLN(AFAG+2.)-GAMLN(AFAG+1.)-LLMG)
         QTMP3 = EXP(GAMLN(BVG/2.+AFAG+2.5)-GAMLN(AFAG+1.)-QTMP1)
         VENQG = AVSG*QTMP2+BVSG*BGTMP*QTMP3
         HGdqv = 2.*PI*NG1D*VENQG*XXLS*SSRI0/ABI
         HGwqv = 2.*PI*NG1D*VENQG*XXLV*SSRW0/ABW
      ENDIF
      IF (QH1D.GE.QSMALL) THEN
         CALL SOLVE_AFAH(TK1D,RHO,QH1D,NH1D,AH1D,LAMH,AFAH,MVDH,AVH,BVH)
         LLMH  = LOG(LAMH)
         GH2   = EXP(GAMLN(AFAH+2.)-GAMLN(AFAH+1.)-LLMH)
         GH3   = EXP(GAMLN(AFAH+3.)-GAMLN(AFAH+1.)-2.*LLMH)
         GH4   = EXP(GAMLN(AFAH+4.)-GAMLN(AFAH+1.)-3.*LLMH)
         GH5   = EXP(GAMLN(AFAH+5.)-GAMLN(AFAH+1.)-4.*LLMH)
         FSQH  = EXP(GAMLN(BVH+BMH+AFAH+1.)-GAMLN(BMH+AFAH+1.)-BVH*LLMH)
         FSNH  = EXP(GAMLN(BVH+AFAH+1.)-GAMLN(AFAH+1.)-BVH*LLMH)
         VTQH  = MIN(RHOAJ*FSQH*AVH,VTHMAX)
         VTNH  = MIN(RHOAJ*FSNH*AVH,VTHMAX)
         IF (AH1D.GE.ASMALL) THEN
            FSAH = EXP(GAMLN(BVH+AFAH+3.)-GAMLN(AFAH+3.)-BVH*LLMH)
            VTAH = MIN(RHOAJ*FSAH*AVH,VTHMAX)
         ENDIF
         BHTMP = SCN*SQRT(AVH*RHOAJ/MUA)
         IF (HAIL_VENT.EQ.0) THEN
            QTMP1 = LLMH*(1.5+BVH/2.)
            QTMP2 = EXP(GAMLN(AFAH+2.)-GAMLN(AFAH+1.)-LOG(LAMH))
            QTMP3 = EXP(GAMLN(BVH/2.+AFAH+2.5)-GAMLN(AFAH+1.)-QTMP1)
            ATMP1 = LLMH*(0.5+BVH/2.)
            ATMP2 = EXP(GAMLN(BVH/2.+AFAH+1.5)-GAMLN(AFAH+1.)-ATMP1)
            VENQH = AVRH*QTMP2+BVRH*BHTMP*QTMP3
            VENAH = AVRH+BVRH*BHTMP*ATMP2
         ELSEIF (HAIL_VENT.EQ.1) THEN
            QTMP1 = LLMH*(1.5+BVH/2.)
            QTMP2 = EXP(GAMLN(AFAH+2.)-GAMLN(AFAH+1.)-LOG(LAMH))
            QTMP3 = EXP(GAMLN(BVH/2.+AFAH+2.5)-GAMLN(AFAH+1.)-QTMP1)
            QTMP4 = LLMH*(2.+BVH)
            QTMP5 = EXP(GAMLN(BVH+AFAH+3.)-GAMLN(AFAH+1.)-QTMP4)
            ATMP1 = LLMH*(0.5+BVH/2.)
            ATMP2 = EXP(GAMLN(BVH/2.+AFAH+1.5)-GAMLN(AFAH+1.)-ATMP1)
            ATMP3 = LLMH*(1.+BVH)
            ATMP4 = EXP(GAMLN(BVH+AFAH+2.)-GAMLN(AFAH+1.)-ATMP3)
            VENQH = QTMP2+VENH1*BHTMP*QTMP3+VENH2*BHTMP**2.*QTMP5
            VENAH = 1.+VENH1*BHTMP*ATMP2+VENH2*BHTMP**2.*ATMP4
         ENDIF
         HHdqv = 2.*PI*NH1D*VENQH*XXLS*SSRI0/ABI
         HHwqv = 2.*PI*NH1D*VENQH*XXLV*SSRW0/ABW
      ENDIF
!----------------- CHEN&LIU WARM CLOUD PROCESSES ----------------------
      IF (QC1D.GE.QSMALL) THEN
         NCLcc = EXP(AN3+BN3*LOG(NC1D)+CN3*LMVRC**3.)*GUC
         NCNcr = NC1D*NC1D*EXP(AN4+BN4*MVRC+CN4/MVRC)*GUC
         QCNcr = NCNcr*EXP(AQ4+BQ4*QC1D/NC1D)
         NCNcr = MIN(NCNcr,NC1D*iDT)
         QCNcr = MIN(QCNcr,QC1D*iDT)
      ENDIF
      IF (QR1D.GE.QSMALL) THEN
         NCLrr = MIN(EXP(AN6+BN6*LOG(NR1D)+CN6/MVRR)*GUR,NR1D*iDT)
         NBKrr = NCLrr*EXP(AN7+(BN7+CN7*MVRR)*MVRR)
         NBKrc = NCLrr*EXP(AN8+(BN8+CN8*MVRR)*MVRR)
         QBKrc = MIN(NBKrc**BQ8*EXP(AQ8+CQ8*LMVRR),QR1D*iDT)
         NBKrc = MIN(NBKrc,NR1D*iDT)
      ENDIF
      IF (QC1D.GE.QSMALL.AND.QR1D.GE.QSMALL) THEN
         NCLcr = NC1D*NR1D*GUR*EXP(AN5+BN5*LMVRR+CN5*LMVRC)
         QCLcr = NC1D*NR1D*GUR*EXP(AQ5+BQ5*LMVRR+CQ5*LMVRC)
         NCLcr = MIN(NCLcr,NC1D*iDT)
         QCLcr = MIN(QCLcr,QC1D*iDT)
      ENDIF
      IF (TK1D.LT.TK0C) THEN
         IF ((HIdqv+HSdqv+HGdqv+HHdqv).GE.QSMALL) THEN
            VDMAX  = XXLS*(QV1D-QVSI0)/(1.+XXLS**2.*QV1D/(CPM*RV*      &
                     TK1D**2.))*iDT
            SUMDEP = HIdqv+HSdqv+HGdqv+HHdqv
            IF (SUMDEP.GT.VDMAX.AND.VDMAX.GE.QSMALL) THEN
               RATIO = MIN(1.,VDMAX/(SUMDEP+QSMALL))
               HIdqv = HIdqv*RATIO
               HSdqv = HSdqv*RATIO
               HGdqv = HGdqv*RATIO
               HHdqv = HHdqv*RATIO
            ENDIF
         ENDIF
         IF ((HIdqv+HSdqv+HGdqv+HHdqv).LT.0.) THEN
            SBMAX  = XXLS*(QV1D-QVSI0)/(1.+XXLS**2.*QV1D/(CPM*RV*      &
                     TK1D**2.))*iDT
            SUMSUB = HIdqv+HSdqv+HGdqv+HHdqv
            IF (SBMAX.LT.0..AND.SUMSUB.LT.SBMAX*0.9999) THEN
               HIdqv = HIdqv*MIN(1.,0.9999*SBMAX/SUMSUB)
               HSdqv = HSdqv*MIN(1.,0.9999*SBMAX/SUMSUB)
               HGdqv = HGdqv*MIN(1.,0.9999*SBMAX/SUMSUB)
               HHdqv = HHdqv*MIN(1.,0.9999*SBMAX/SUMSUB)
            ENDIF
         ENDIF
!---------- HOMO/HETER FREEZING OF DROPLETS AND RAIN DROPS  ------------
         IF (QC1D.GE.QSMALL) THEN                                       ! DeMott et al. (1994)
            TMP1  = TC1D*TC1D
            VOLMC = C4PI3*(MVRC*1.E6)**3.
            IJHOF = (10.**MAX(-20.,(-606.3952-52.6611*TC1D-1.7439*TMP1-&
                    0.0265*TMP1*TC1D-1.536e-4*TMP1**2.)))
            RFZ   = 1.-EXP(-IJHOF*VOLMC*DT)
            IF (TK1D.GT.243.15) RFZ = 0.
            IF (TK1D.LT.223.15) RFZ = 1.
            NHOci = RFZ*NC1D*iDT
            QHOci = RFZ*QC1D*iDT
            VHOci = QHOci*iRHOI0
            FHOci = QHOci*1.*iAMI0
            IHOci = QHOci*iAMI0
            AHOci = (KCIMIN*NHOci*IHOci**2.)**THRD
         ENDIF
         IF (TK1D.LT.269.15.AND.QC1D.GE.QSMALL) THEN
            QNMci = QFZC1*BIMM*EXP(AIMM*(TK0C-TK1D)-1.)
            NNMci = NFZC1*BIMM*EXP(AIMM*(TK0C-TK1D)-1.)
            QNMci = MIN(QNMci,QC1D*iDT)
            NNMci = MIN(NNMci,NC1D*iDT)
            VNMci = QNMci*iRHOI0
            FNMci = QNMci*1.*iAMI0                                      ! ISOMETRIC
            INMci = QNMci*iAMI0
            ANMci = (KCIMIN*NNMci*INMci**2.)**THRD
         ENDIF
         IF (QC1D.GE.QSMALL.AND.TK1D.LT.271.15) THEN                    ! Referred to MY2 scheme
            RGIMF  = (2.*2.99E-26*SFCTNW)/(ICED*BOLTZ*TK1D*ACTW)
            GGCNT  = C4PI3*SFCTNV*RGIMF**2.
            EPA    = ESI*QV1D/QVSI
            GEOF2  = EXP(DC1+DC2*COSM2+DC3*RGIMF/INR0)
            NGCNT0 = EXP((-DACTE-GEOF2*GGCNT)/(BOLTZ*TK1D))
            NGCNT1 = EPA/(1.61E-11*TK1D**(0.5))                         ! 1.61E-11 = vs*(2*PI*mw*k)**0.5
!            NGCNT  = MAX(4.*PI*INR0**2.*NGCNT0*NGCNT1,1.)*QNIN
            CNTGG0 = (XXLV**2.-XXLV*RV*TK1D)/(KAP*RV*TK1D**2.)
            CNTGG  = (RV*TK1D/(ESW*DV)+CNTGG0)**(-1.)/RHOW
            KDIFF  = 9.1018E-11*TK1D**2.+8.8197E-8*TK1D-1.0654E-5       ! AIR THERMAL DIFFUSIVITY (COTTON ET AL., 1986)
            TCC    = TC1D+CNTGG*SSRW*XXLV/KDIFF                         ! DROPLETS TEMPERATURE
            NGCNT  = EXP(4.11-0.262*TCC)
            CNTKN  = 6.6E-8*TK1D*101325./(293.15*P1D*INR0)              ! KNUDSEN NUMBER
            PSIA   = -1.*BOLTZ*TCC*(1.+CNTKN)/(6.*PI*INR0*MUA)          ! AEROSOL DIFFUSIVITY
            KAPA   = 5.39E5                                             ! AEROSOL THERMAL CONDUCTIVITY
            CNTFT0 = 0.4*(1.+1.45*CNTKN+0.4*CNTKN*EXP(-1./CNTKN))
            CNTFT1 = (1.+3.*CNTKN)*(2.*KAP+5.*KAPA*CNTKN+KAPA)
            CNTFT  = CNTFT0*(KAP+2.5*CNTKN*KAPA)/CNTFT1
            CNTF1  = 4.*PI*NGCNT*NC1D*MVRC
            CNTF2  = KAP*(TC1D-TCC)/P1D
            IJCNT1 = -1.*CNTF1*CNTF2*RV*TK1D/(XXLV*RHO)                 ! DIFFUSIOPHORESIS
            IJCNT2 = CNTF1*CNTF2*CNTFT*iRHO                             ! THERMEOPHORESIS
            IJCNT3 = CNTF1*PSIA                                         ! BROWNIAN DIFFUSION
!            NNCci  = MIN(MAX(0.,IJCNT1+IJCNT2+IJCNT3),NC1D*iDT)
!            QNCci  = NNCci*QC1D/NC1D
!            VNCci  = QNCci*iRHOI0
!            FNCci  = QNCci*1.*iAMI0                                     ! ISOMETRIC
!            INCci  = QNCci*iAMI0
!            ANCci  = (KCIMIN*NNCci*INCci**2.)**THRD
         ENDIF
         IF (TK1D.LE.233.15.AND.QR1D.GE.QSMALL) THEN
            QHOrg = QR1D*iDT
            NHOrg = NR1D*iDT
            IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
               VHOrg = QHOrg/RHOG0
            ENDIF
            AHOrg = (KCGMAX*NHOrg*(QHOrg*V2M3/RHOG0)**2.)**THRD
         ENDIF
         IF (TK1D.LT.269.15.AND.QR1D.GE.QSMALL) THEN
            QNMrg = QFZR1*BIMM*EXP(AIMM*(TK0C-TK1D)-1.)
            NNMrg = NFZR1*BIMM*EXP(AIMM*(TK0C-TK1D)-1.)
            QNMrg = MIN(QNMrg,QR1D*iDT)
            NNMrg = MIN(NNMrg,NR1D*iDT)
            IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
               VNMrg = QNMrg/RHOG0
            ENDIF
            ANMrg = AFZR1*BIMM*EXP(AIMM*(TK0C-TK1D)-1.)
         ENDIF
!------------------ AUTO-CONVERSION PROCESSES -------------------------
         IF (QI1D.GE.QSMALL.AND.MVDI.GE.1.E-5) THEN
!            EII1 = 0.1*EXP(0.1*TC1D)                                    ! STRAKA AND MANSELL (2005)
            EII1 = 10.**(3.5E-2*TC1D-0.7)                               ! BASEN ON COTTON ET AL. (1986)
            IF (ICE_RHOI.EQ.1) THEN
               EII2 = 1.-RHOI/RHOI0
            ELSE
               EII2 = 0.
            ENDIF
            EII = MIN(MAX(EII1,EII2,0.),1.)
            IF ((ADAGR-1.).GE.SLIMIT) THEN
               QCNis = QCNI1*VTQI*EII*NI1D*(GIM2H1/DI0Z4+2.*GIMF1/     &
                       DI0Z1+DI0Z2*GIM2G1)
               NiCNis = NCNI1*VTNI*EII*NI1D*(GI2H1/DI0Z4+2.*GIF1/DI0Z1+&
                        DI0Z2*GI2G1)
               IF (AI1D.GE.ASMALL) THEN
                  AiCNis = ACNI1*VTAI*EII*NI1D*(GI2H3/DI0Z4+2.*GIF3/   &
                           DI0Z1+DI0Z2*GI2G3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FiCNis = FCNI1*VTFI*EII*NI1D/DI0Z3*(GIZM2H1/DI0Z4+2.*&
                           GIZMF1/DI0Z1+DI0Z2*GIZM2G1)
                  ICNis = FCNI1*VTI3M*EII*NI1D*(GIM2H1/DI0Z4+2.*GIMF1/ &
                          DI0Z1+DI0Z2*GIM2G1)
               ENDIF
            ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
               QCNis = QCNI1*VTQI*EII*NI1D*4.*DI0Z2*GIM2G1
               NiCNis = NCNI1*VTNI*EII*NI1D*4.*DI0Z2*GI2G1
               IF (AI1D.GE.ASMALL) THEN
                  AiCNis = ACNI1*VTAI*EII*NI1D*4.*DI0Z2*GI2G3
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FiCNis = FCNI1*VTFI*EII*NI1D*4.*DI0Z2*GIZM2G1/DI0Z3
                  ICNis = FCNI1*VTI3M*EII*NI1D*4.*DI0Z2*GIM2G1
               ENDIF
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
               QCNis = QCNI1*VTQI*EII*NI1D*4.*GIM3
               NiCNis = NCNI1*VTNI*EII*NI1D*4.*GI3
               IF (AI1D.GE.ASMALL) THEN
                 AiCNis = ACNI1*VTAI*EII*NI1D*4.*GI5
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FiCNis = FCNI1*VTFI*EII*NI1D*4.*GIM3
                  ICNis = FCNI1*VTI3M*EII*NI1D*4.*GIM3
               ENDIF
            ENDIF
            QCNis  = MIN(2.*QCNis,QI1D*iDT)
            NsCNis = MIN(NiCNis,NI1D*iDT)
            NiCNis = MIN(2.*NiCNis,NI1D*iDT)
            AiCNis = MIN(2.*AiCNis,AI1D*iDT)
            FiCNis = MIN(2.*FiCNis,FI1D*iDT)
            ICNis  = MIN(2.*ICNis,I3M1D*iDT)
            ViCNis = MIN(QCNis*iRHOI,VI1D*iDT)
            IF (ICE_RHOS.EQ.1.OR.AGG_SHAPE.EQ.1) THEN
               LIM1 = 0.7*(1.-RHOI/RHOI0)                               ! CONTACT ANGLE = 45
               LIM2 = SQRT(1.-LIM1*LIM1)
               LIM3 = (1.+2.*ISEPS+ISEPS**2.+ISEPL+2.*ISEPS*ISEPL+     &
                      ISEPS**2.*ISEPL)/8.
               LIM4 = 2.*LIM3*LIM2
               LIM5 = LIM3*LIM2**2.
               LIM6 = LIM3*LIM2
               LIM7 = 2.*LIM3*LIM2**2.
               LIM8 = LIM3*LIM2**3.
               LIMA = (1.+2.*ISEPL+ISEPL**2.+ISEPS+2.*ISEPL*ISEPS+     &
                      ISEPL**2.*ISEPS)/8.
               LIMB = LIMA*LIM2
               LIMC = 2.*LIMA*LIM2
               LIMD = 2.*LIMA*LIM2**2.
               LIME = LIMA*LIM2**2.
               LIMF = LIMA*LIM2**3.
               DICC = DI0**(-6.*ZETA)*GI3H1
               DICA = GI2HG1/DI0Z3
               DIAC = GIH2G1
               DIAA = DI0Z3*GI3G1
               DIA0 = DI0Z1*GIG1
               DIC0 = GIH1/DI0Z2
               IF ((ADAGR-1.).GE.SLIMIT) THEN
                  DILSV = (LIMA+LIMB+LIMC+LIMD+LIME+LIMF)*DICA
                  DILSF = (LIM3+LIM4+LIM5+LIM6+LIM7+LIM8)*DIAC
                  SASP1 = MIN((1.+LIM2+ISEPS+ISEPS*LIM2)*DIA0,(1.+     &
                          LIM2+ISEPL+ISEPL*LIM2)*DIC0)/MAX((1.+LIM2+   &
                          ISEPS+ISEPS*LIM2)*DIA0,(1.+LIM2+ISEPL+ISEPL* &
                          LIM2)*DIC0)
               ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
                  DILSV = (LIMA+LIMB+LIMC+LIMD+LIME+LIMF)*DIAC
                  DILSF = (LIM3+LIM4+LIM5+LIM6+LIM7+LIM8)*DICA
                  SASP1 = MIN((1.+LIM2+ISEPS+ISEPS*LIM2)*DIC0,(1.+     &
                          LIM2+ISEPL+ISEPL*LIM2)*DIA0)/MAX((1.+LIM2+   &
                          ISEPS+ISEPS*LIM2)*DIC0,(1.+LIM2+ISEPL+ISEPL* &
                          LIM2)*DIA0)
               ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
                  DILSV = (LIMA+LIMB+LIMC+LIMD+LIME+LIMF)*GI4
                  DILSF = (LIM3+LIM4+LIM5+LIM6+LIM7+LIM8)*GI4
                  SASP1 = MIN(1.+LIM2+ISEPS+ISEPS*LIM2,1.+LIM2+ISEPL+  &
                          ISEPL*LIM2)/MAX(1.+LIM2+ISEPS+ISEPS*LIM2,1.+ &
                          LIM2+ISEPL+ISEPL*LIM2)
               ENDIF
               DNIAG  = 2.*AMI*GIM1*V2M3/DILSV
               VsCNis = NiCNis*DILSV/V2M3
               FsCNis = NiCNis*DILSF
               RATIO  = (RHOI/DNIAG)**THRD
            ELSE
               RATIO  = 1.
            ENDIF
            IF ((ADAGR-1.).GE.SLIMIT) THEN
               AsCNis = ACNI1*VTAI*EII*NI1D*(GI2H3/DI0Z4+2.*GIF3/DI0Z1+&
                        DI0Z2*GI2G3)*1.5874*RATIO**2.
            ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
               AsCNis = ACNI1*VTAI*EII*NI1D*4.*DI0Z2*GI2G3*1.5874*     &
                        RATIO**2.
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
               AsCNis = ACNI1*VTAI*EII*NI1D*4.*GI5*1.5874*RATIO**2.
            ENDIF
         ENDIF
         IF (QG1D.GE.QSMALL) THEN
            DSLL = 2.*1.E-2*(EXP(MIN(20.,-TC1D/(1.1E4*(QC1D+QR1D)-     &
                   1.3E3*QG1D+1.)))-1.)
            DSLL = MIN(1.,MAX(1.E-4,DSLL))
            IF (AFAG.LE.20.) THEN
               RATIO = MIN(1.,MAX(0.,ABS(GAMMP(AFAG+1.,DSLL*LAMG))))
               NGTAL = NG1D*(1.-RATIO)
               IF (NGTAL.GE.NSMALL) THEN
                  QCNgh  = MIN((1.-RATIO)*QG1D*iDT,QG1D*iDT)
                  NgCNgh = MIN((1.-RATIO)*NG1D*iDT,NG1D*iDT)
                  IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
                     VCNgh = MIN((1.-RATIO)*VG1D*iDT,VG1D*iDT)
                  ENDIF
                  IF (AG1D.GE.ASMALL) THEN
                     AgCNgh = MIN((1.-RATIO)*AG1D*iDT,AG1D*iDT)
                  ENDIF
               ENDIF
            ELSE
               IF (MVDG.GE.DSLL) THEN
                  QCNgh  = QG1D*iDT
                  NgCNgh = NG1D*iDT
                  IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
                     VCNgh = VG1D*iDT
                  ENDIF
                  IF (AG1D.GE.ASMALL) THEN
                     AgCNgh = AG1D*iDT
                  ENDIF
               ENDIF
            ENDIF
            NhCNgh = QCNgh/(AMH*DHMIN**BMH)
            AhCNgh = (KCHMIN*NhCNgh*(QCNgh*iAMH)**2.)**THRD
         ENDIF
!----------------- RIMING OF CLOUD DROPLETS ----------------------------
         IF (QI1D.GE.QSMALL.AND.QC1D.GE.QSMALL) THEN
            VTQ0  = VTQI-VTQC
            VTN0  = VTNI-VTNC
            VTAX  = VTAI-VTAC
            VTV0  = VTVI-VTQC
            VTF0  = VTFI-VTQC
            VTQIC = SQRT(VTQ0*VTQ0+0.04*VTQI*VTQC)
            VTNIC = SQRT(VTN0*VTN0+0.04*VTNI*VTNC)
            VTAIC = SQRT(VTAX*VTAX+0.04*VTAI*VTAC)
            VTVIC = SQRT(VTV0*VTV0+0.04*VTVI*VTQC)
            VTFIC = SQRT(VTF0*VTF0+0.04*VTFI*VTQC)
            IF (ICE_RHOG.EQ.0) THEN
               DNIRM = RHOG1
            ELSEIF (ICE_RHOG.EQ.1) THEN
               DNRI  = MIN(MAX(-5.E5*MVDC*VTQIC/TC1D,0.),6.)
               DNIRM = 1.E3*(0.078+0.184*DNRI-0.015*DNRI**2.)
            ELSEIF (ICE_RHOG.EQ.2) THEN
               DNIRM = 3.E2*(-5.E5*MVDC*0.6*VTQIC/TC1D)**0.44
            ENDIF
            DNIRM = MIN(MAX(DNIRM,RHOIMIN),RHOG0)
            RHOIW = (QI1D+QC1D)/(QI1D/RHOI+QC1D/RHOW+ISMALL)
            RATIO = (RHOIW/RHOI)**THRD
            MVDX  = MAX((MVDC**3.+MVDI**3.)**THRD*RATIO,MVDI)
            IF ((ADAGR-1.).GE.SLIMIT.AND.MVDI.GE.1.E-4) THEN
               WBIN = MIN(MAX(NINT(MVDC*1.E6/10.),0),20)
               CNRE = VTQI*DI0**(-2.*ZETA)*MVDI**IPH*RHO/MUA
               IF (CNRE.LT.0.4) CBIN = 0
               IF (CNRE.GE.0.4.AND.CNRE.LT.0.6) CBIN = 1
               IF (CNRE.GE.0.6.AND.CNRE.LT.0.9) CBIN = 2
               IF (CNRE.GE.0.9.AND.CNRE.LT.1.5) CBIN = 3
               IF (CNRE.GE.1.5.AND.CNRE.LT.3.5) CBIN = 4
               IF (CNRE.GE.3.5.AND.CNRE.LT.7.5) CBIN = 5
               IF (CNRE.GE.7.5.AND.CNRE.LT.15.) CBIN = 6
               IF (CNRE.GE.15.) CBIN = 7
               CBIN  = MIN(MAX(CBIN,0),7)
               ECI   = MAX(0.,IECC(CBIN*21+WBIN))
               QRMci = QRMC1*ECI*VTQIC*NI1D*(GIF1*GC4/DI0Z1+GIG1*GC5*  &
                       DI0Z1+GIH1*GC5/DI0Z2+GC6)
               NRMci = NRMC1*ECI*VTNIC*NI1D*(GIF1/DI0Z1+GIG1*GC2*DI0Z1+&
                       GIH1*GC2/DI0Z2+GC3)
               IF (AI1D.GE.ASMALL) THEN
                  ARMci = (MVDX**2.-MVDI**2.)*NRMC1*ECI*VTAIC*NI1D*(   &
                          GIF1/DI0Z1+GIG1*GC2*DI0Z1+GIH1*GC2/DI0Z2+GC3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FRMci = FRMC1*ECI*VTFIC*NI1D/DNIRM/8./DI0Z3*(GIZF1*  &
                          GC4/DI0Z1+GIZG1*GC5*DI0Z1+GIZH1*GC5/DI0Z2+   &
                          GIZ1*GC6)
                  IRMci = IRMC1*ECI*VTVIC*NI1D/DNIRM*(GIF1*GC4/DI0Z1+  &
                          GIG1*GC5*DI0Z1+GIH1*GC5/DI0Z2+GC6)
               ENDIF
            ELSEIF ((1.-ADAGR).GE.SLIMIT.AND.MVDI.GE.1.5E-4) THEN
               WBIN  = MIN(MAX(NINT(MVDC*1.E6/10.),0),20)
               PNRE  = VTQI*DI0**ZETA*MVDI**IPG*RHO/MUA
               IF (PNRE.LT.1.5) PBIN = 0
               IF (PNRE.GE.1.5.AND.PNRE.LT.6.0) PBIN = 1
               IF (PNRE.GE.6.0.AND.PNRE.LT.15.) PBIN = 2
               IF (PNRE.GE.15..AND.PNRE.LT.28.) PBIN = 3
               IF (PNRE.GE.28..AND.PNRE.LT.43.) PBIN = 4
               IF (PNRE.GE.43..AND.PNRE.LT.70.) PBIN = 5
               IF (PNRE.GE.70..AND.PNRE.LT.105.) PBIN = 6
               IF (PNRE.GE.105.) PBIN = 7
               PBIN  = MIN(MAX(PBIN,0),7)
               ECI   = MAX(0.,IEPC(PBIN*21+WBIN))
               QRMci = QRMC1*ECI*VTQIC*NI1D*(GI2G1*GC4*DI0Z2+2.*GIG1*  &
                       GC5*DI0Z1+GC6)
               NRMci = NRMC1*ECI*VTNIC*NI1D*(GI2G1*DI0Z2+2.*GIG1*GC2*  &
                       DI0Z1+GC3)
               IF (AI1D.GE.ASMALL) THEN
                  ARMci = (MVDX**2.-MVDI**2.)*NRMC1*ECI*VTAIC*NI1D*(   &
                          GI2G1*DI0Z2+2.*GIG1*GC2*DI0Z1+GC3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FRMci = FRMC1*ECI*VTFIC*NI1D/DNIRM/2./DI0Z3*(GIZ2G1* &
                          GC4*DI0Z2+2.*GIZG1*GC5*DI0Z1+GIZ1*GC6)
                  IRMci = IRMC1*ECI*VTVIC*NI1D/DNIRM*(GI2G1*GC4*DI0Z2+ &
                          2.*GIG1*GC5*DI0Z1+GC6)
               ENDIF
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT.AND.MVDI.GE.2.E-4) THEN
               ECI   = 0.5
               QRMci = QRMC1*ECI*VTQIC*NI1D*(GI3*GC4+2.*GI2*GC5+GC6)
               NRMci = NRMC1*ECI*VTNIC*NI1D*(GI3+2.*GI2*GC2+GC3)
               IF (AI1D.GE.ASMALL) THEN
                  ARMci = (MVDX**2.-MVDI**2.)*NRMC1*ECI*VTAIC*NI1D*(   &
                          GI3+2.*GI2*GC2+GC3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FRMci = FRMC1*ECI*VTFIC*NI1D/DNIRM/4.*(GI3*GC4+2.*   &
                          GI2*GC5+GC6)
                  IRMci = IRMC1*ECI*VTVIC*NI1D/DNIRM*(GI3*GC4+2.*GI2*  &
                          GC5+GC6)
               ENDIF
            ENDIF
            QRMci = MIN(QRMci,QC1D*iDT)
            NRMci = MIN(NRMci,NC1D*iDT)
            VRMci = QRMci/DNIRM
            FRMci = MIN(FRMci,QC1D*iDT/AMW)
            IRMci = MIN(IRMci,QC1D*iDT/AMW)
            ARMci = MAX(0.,MIN(ARMci,QC1D*iDT*iAPW/MVDC))
            IF (QRMci.GT.0.) THEN
               QINig  = MIN(2.*QRMci,QI1D*iDT)
               NINig  = MIN(QINig*NI1D/QI1D,NI1D*iDT)
               ViINig = MIN(VRMci+QRMci*VI1D/QI1D,VI1D*iDT)
               IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
                  VgINig = 2.*VRMci
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FINig = MIN(FRMci+QRMci*FI1D/QI1D,FI1D*iDT)
                  IINig = MIN(IRMci+QRMci*I3M1D/QI1D,I3M1D*iDT)
               ENDIF
               IF (AI1D.GE.ASMALL) THEN
                  AiINig = MIN(ARMci+QRMci*AI1D/QI1D,AI1D*iDT)
                  AgINig = (KCGMAX*NINig*(QINig*V2M3/DNIRM)**2.)**THRD
               ENDIF
            ENDIF
         ENDIF
         IF (QS1D.GE.QSMALL.AND.QC1D.GE.QSMALL.AND.MVDS.GE.1.5E-4) THEN
            ECS   = MIN(MVDC,3.E-5)*3.333E4*SQRT(MIN(MVDS,1.E-3)*1.E3)  ! Pruppacher&Klett(1997) Fig.14-11
            VTQ0  = VTQS-VTQC
            VTN0  = VTNS-VTNC
            VTAX  = VTAS-VTAC
            VTV0  = VTVS-VTQC
            VTQSC = SQRT(VTQ0*VTQ0+0.04*VTQS*VTQC)
            VTNSC = SQRT(VTN0*VTN0+0.04*VTNS*VTNC)
            VTASC = SQRT(VTAX*VTAX+0.04*VTAS*VTAC)
            VTVSC = SQRT(VTV0*VTV0+0.04*VTVS*VTQC)
            IF (ICE_RHOG.EQ.0) THEN
               DNSRM = RHOG1
            ELSEIF (ICE_RHOG.EQ.1) THEN
               DNRI  = MIN(MAX(-5.E5*MVDC*VTQSC/TC1D,0.),6.)
               DNSRM = 1.E3*(0.078+0.184*DNRI-0.015*DNRI**2.)
            ELSEIF (ICE_RHOG.EQ.2) THEN
               DNSRM = 3.E2*(-5.E5*MVDC*0.6*VTQSC/TC1D)**0.44
            ENDIF
            DNSRM = MIN(MAX(DNSRM,RHOIMIN),RHOG0)
            RHOSW = (QS1D+QC1D)/(QS1D/RHOS+QC1D/RHOW+ISMALL)
            RATIO = (RHOSW/RHOS)**THRD
            MVDX  = MAX((MVDC**3.+MVDS**3.)**THRD*RATIO,MVDS)
            QRMcs = QRMC1*ECS*VTQSC*NS1D*(SASR2*GS3*GC4+SASR1*2.*GS2*  &
                    GC5+GC6)
            NRMcs = NRMC1*ECS*VTNSC*NS1D*(SASR2*GS3+SASR1*2.*GS2*GC2+  &
                    GC3)
            QRMcs = MIN(QRMcs,QC1D*iDT)
            NRMcs = MIN(NRMcs,NC1D*iDT)
            IF (ICE_RHOS.EQ.1) THEN
               VRMcs = QRMcs/DNSRM
            ENDIF
            IF (AGG_SHAPE.EQ.1) THEN
               FRMcs = FRMC1*ECS*VTVSC*NS1D*SASPR/DNSRM/2.*(SASR2*GS3* &
                       GC4+SASR1*2.*GS2*GC5+GC6)
               FRMcs = MIN(FRMcs,QC1D*iDT/AMW)
            ENDIF
            IF (AS1D.GE.ASMALL) THEN
               ARMcs = (MVDX**2.-MVDS**2.)*NRMC1*ECS*VTASC*NS1D*(SASR2*&
                       GS3+SASR1*2.*GS2*GC2+GC3)
               ARMcs = MAX(0.,MIN(ARMcs,QC1D*iDT*iAPW/MVDC))
            ENDIF
            IF (QRMcs.GT.0.) THEN
               NIMcsi = 3.5E8*QRMcs*FF
               QIMcsi = MIN(NIMcsi*MI0,QRMcs)
               VIMcsi = QIMcsi*iRHOI0
               FIMcsi = QIMcsi*1.*iAMI0
               IIMcsi = QIMcsi*iAMI0
               AIMcsi = (KCIMIN*NIMcsi*IIMcsi**2.)**THRD
               QINsg  = MIN(2.*QRMcs,QS1D*iDT)
               NINsg  = MIN(QINsg*NS1D/QS1D,NS1D*iDT)
               IF (ICE_RHOS.EQ.1) THEN
                  VsINsg = MIN(VRMcs+QRMcs*VS1D/QS1D,VS1D*iDT)
               ENDIF
               IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
                  VgINsg = 2.*VRMcs
               ENDIF
               IF (AGG_SHAPE.EQ.1) THEN
                  FINsg = MIN(FRMcs+QRMcs*FS1D/QS1D,FS1D*iDT)
               ENDIF
               IF (AS1D.GE.ASMALL) THEN
                  AsINsg = MIN(ARMcs+QRMcs*AS1D/QS1D,AS1D*iDT)
                  AgINsg = (KCGMAX*NINsg*(QINsg*V2M3/DNSRM)**2.)**THRD
               ENDIF
            ENDIF
         ENDIF
         IF (QG1D.GE.QSMALL.AND.QC1D.GE.QSMALL.AND.MVDG.GE.2.E-4) THEN
            STOKE = (RHOW*VTQG*MVDC**2.)/(9.*MUA*MVDG)
            STOKE = MAX(1.5,MIN(10.,STOKE))
            ECG   = 5.5E-1*LOG10(2.51*STOKE)                            ! parameterization based on Cober and List,1993 [JAS]
            VTQ0  = VTQG-VTQC
            VTN0  = VTNG-VTNC
            VTAX  = VTAG-VTAC
            VTQGC = SQRT(VTQ0*VTQ0+0.04*VTQG*VTQC)
            VTNGC = SQRT(VTN0*VTN0+0.04*VTNG*VTNC)
            VTAGC = SQRT(VTAX*VTAX+0.04*VTAG*VTAC)
            IF (ICE_RHOG.EQ.0) THEN
               DNGRM = RHOG1
            ELSEIF (ICE_RHOG.EQ.1) THEN
               DNRI  = MIN(MAX(-5.E5*MVDC*VTQGC/TC1D,0.),6.)
               DNGRM = 1.E3*(0.078+0.184*DNRI-0.015*DNRI**2.)
            ELSEIF (ICE_RHOG.EQ.2) THEN
               DNGRM = 3.E2*(-5.E5*MVDC*0.6*VTQGC/TC1D)**0.44
            ENDIF
            DNGRM = MIN(MAX(DNGRM,RHOIMIN),RHOG0)
            RHOGW = (QG1D+QC1D)/(QG1D/RHOG+QC1D/RHOW+ISMALL)
            RATIO = (RHOGW/RHOG)**THRD
            MVDX  = MAX((MVDC**3.+MVDG**3.)**THRD*RATIO,MVDG)
            QRMcg = QRMC1*ECG*VTQGC*NG1D*(GG3*GC4+2.*GG2*GC5+GC6)
            NRMcg = NRMC1*ECG*VTNGC*NG1D*(GG3+2.*GG2*GC2+GC3)
            QRMcg = MIN(QRMcg,QC1D*iDT)
            NRMcg = MIN(NRMcg,NC1D*iDT)
            IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
               VRMcg = QRMcg/DNGRM
            ENDIF
            IF (AG1D.GE.ASMALL) THEN
               ARMcg = (MVDX**2.-MVDG**2.)*NRMC1*ECG*VTAGC*NG1D*(GG3+  &
                       2.*GG2*GC2+GC3)
               ARMcg = MAX(0.,MIN(ARMcg,QC1D*iDT*iAPW/MVDC))
            ENDIF
            IF (QRMcg.GT.0.) THEN
               NIMcgi = 3.5E8*QRMcg*FF
               QIMcgi = MIN(NIMcgi*MI0,QRMcg)
               VIMcgi = QIMcgi*iRHOI0
               FIMcgi = QIMcgi*1.*iAMI0
               IIMcgi = QIMcgi*iAMI0
               AIMcgi = (KCIMIN*NIMcgi*IIMcgi**2.)**THRD
            ENDIF
         ENDIF
         IF (QH1D.GE.QSMALL.AND.QC1D.GE.QSMALL) THEN
            ECH   = EXP(-8.68E-7*MVDC**(-1.6)*MVDH)                     ! Ziegler (1985) A24
            VTQ0  = VTQH-VTQC
            VTN0  = VTNH-VTNC
            VTAX  = VTAH-VTAC
            VTQHC = SQRT(VTQ0*VTQ0+0.04*VTQH*VTQC)
            VTNHC = SQRT(VTN0*VTN0+0.04*VTNH*VTNC)
            VTAHC = SQRT(VTAX*VTAX+0.04*VTAH*VTAC)
            RHOHW = (QH1D+QC1D)/(QH1D/RHOH+QC1D/RHOW+ISMALL)
            RATIO = (RHOHW/RHOH)**THRD
            MVDX  = MAX((MVDC**3.+MVDH**3.)**THRD*RATIO,MVDH)
            QRMch = QRMC1*ECH*VTQHC*NH1D*(GH3*GC4+2.*GH2*GC5+GC6)
            NRMch = NRMC1*ECH*VTNHC*NH1D*(GH3+2.*GH2*GC2+GC3)
            QRMch = MIN(QRMch,QC1D*iDT)
            NRMch = MIN(NRMch,NC1D*iDT)
            IF (AH1D.GE.ASMALL) THEN
               ARMch = (MVDX**2.-MVDH**2.)*NRMC1*ECH*VTAHC*NH1D*(GH3+  &
                       2.*GH2*GC2+GC3)
               ARMch = MAX(0.,MIN(ARMch,QC1D*iDT*iAPW/MVDC))
            ENDIF
         ENDIF
!----------------- RIMING/COLLECTION OF RAIN DROPS ---------------------
         IF (QR1D.GE.QSMALL.AND.QI1D.GE.QSMALL) THEN
            ERI   = 1.
            VTQ0  = VTQR-VTQI
            VTN0  = VTNR-VTNI
            VTAX  = VTAR-VTAI
            VTV0  = VTQR-VTVI
            VTF0  = VTQR-VTFI
            VTQRI = SQRT(VTQ0*VTQ0+0.04*VTQR*VTQI)
            VTNRI = SQRT(VTN0*VTN0+0.04*VTNR*VTNI)
            VTARI = SQRT(VTAX*VTAX+0.04*VTAR*VTAI)
            VTVRI = SQRT(VTV0*VTV0+0.04*VTQR*VTVI)
            VTFRI = SQRT(VTF0*VTF0+0.04*VTQR*VTFI)
            IF ((ADAGR-1.).GE.SLIMIT.AND.MVDR.GE.MVDI) THEN
               QCLir = QCLI1*ERI*VTQRI*NR1D*(GIMF1/DI0Z1+GIMG1*GR2*    &
                       DI0Z1+GIMH1*GR2/DI0Z2+GIM1*GR3)
               NCLir = NCLI1*ERI*VTNRI*NR1D*(GIF1/DI0Z1+GIG1*GR2*DI0Z1+&
                       GIH1*GR2/DI0Z2+GR3)
               IF (AI1D.GE.ASMALL) THEN
                  ACLir = ACLI1*ERI*VTARI*NR1D*(GIF3/DI0Z1+GIG3*GR2*   &
                          DI0Z1+GIH3*GR2/DI0Z2+GI3*GR3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FCLir = FCLI1*ERI*VTFRI*NR1D*(GIZMF1/DI0Z1+GIZMG1*   &
                          GR2*DI0Z1+GIZMH1*GR2/DI0Z2+GIZM1*GR3)
                  ICLir = NCLI1*ERI*VTVRI*NR1D*(GIMF1/DI0Z1+GIMG1*GR2* &
                          DI0Z1+GIMH1*GR2/DI0Z2+GIM1*GR3)
               ENDIF
            ELSEIF ((1.-ADAGR).GE.SLIMIT.AND.MVDR.GE.MVDI) THEN
               QCLir = QCLI1*ERI*VTQRI*NR1D*(GIM2G1*DI0Z2+2.*GIMG1*GR2*&
                       DI0Z1+GIM1*GR3)
               NCLir = NCLI1*ERI*VTNRI*NR1D*(GI2G1*DI0Z2+2.*GIG1*GR2*  &
                       DI0Z1+GR3)
               IF (AI1D.GE.ASMALL) THEN
                  ACLir = ACLI1*ERI*VTARI*NR1D*(GI2G3*DI0Z2+2.*GIG3*  &
                          GR2*DI0Z1+GI3*GR3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FCLir = FCLI1*ERI*VTFRI*NR1D*(GIZM2G1*DI0Z2+2.*      &
                          GIZMG1*GR2*DI0Z1+GIZM1*GR3)
                  ICLir = NCLI1*ERI*VTVRI*NR1D*(GIM2G1*DI0Z2+2.*GIMG1* &
                          GR2*DI0Z1+GIM1*GR3)
               ENDIF
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT.AND.MVDR.GE.MVDI) THEN
               QCLir = QCLI1*ERI*VTQRI*NR1D*(GIM3+2.*GIM2*GR2+GIM1*GR3)
               NCLir = NCLI1*ERI*VTNRI*NR1D*(GI3+2.*GI2*GR2+GR3)
               IF (AI1D.GE.ASMALL) THEN
                  ACLir = ACLI1*ERI*VTARI*NR1D*(GI5+2.*GI4*GR2+GI3*GR3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FCLir = NCLI1*ERI*VTFRI*NR1D*(GIM3+2.*GIM2*GR2+GIM1* &
                          GR3)
                  ICLir = NCLI1*ERI*VTVRI*NR1D*(GIM3+2.*GIM2*GR2+GIM1* &
                          GR3)
               ENDIF
            ENDIF
            QCLir = MIN(QCLir,QI1D*iDT)
            NCLir = MIN(NCLir,NI1D*iDT)
            VCLir = MIN(QCLir*iRHOI,VI1D*iDT)
            ACLir = MIN(ACLir,AI1D*iDT)
            FCLir = MIN(FCLir,FI1D*iDT)
            ICLir = MIN(ICLir,I3M1D*iDT)
            IF (ICE_RHOG.EQ.0) THEN
               DNIRM = RHOG1
            ELSEIF (ICE_RHOG.EQ.1) THEN
               DNRI  = MIN(MAX(-5.E5*MVDR*VTQRI/TC1D,0.),6.)
               DNIRM = 1.E3*(0.078+0.184*DNRI-0.015*DNRI**2.)
            ELSEIF (ICE_RHOG.EQ.2) THEN
               DNIRM = 3.E2*(-5.E5*MVDR*0.6*VTQRI/TC1D)**0.44
            ENDIF
            DNIRM = MIN(MAX(DNIRM,RHOIMIN),RHOG0)
            RHOIW = (QI1D+QR1D)/(QI1D/RHOI+QR1D/RHOW+ISMALL)
            RATIO = (RHOIW/DNIRM)**THRD
            IF (MVDI.GE.MVDR) THEN
            IF ((ADAGR-1.).GE.SLIMIT.AND.MVDI.GE.1.E-4) THEN
               QRMri = QRMR1*ERI*VTQRI*NI1D*(GIF1*GR4/DI0Z1+GIG1*GR5*  &
                       DI0Z1+GIH1*GR5/DI0Z2+GR6)
               NRMri = NRMR1*ERI*VTNRI*NI1D*(GIF1/DI0Z1+GIG1*GR2*DI0Z1+&
                       GIH1*GR2/DI0Z2+GR3)
            ELSEIF ((1.-ADAGR).GE.SLIMIT.AND.MVDI.GE.1.5E-4) THEN
               QRMri = QRMR1*ERI*VTQRI*NI1D*(GI2G1*GR4*DI0Z2+2.*GIG1*  &
                       GR5*DI0Z1+GR6)
               NRMri = NRMR1*ERI*VTNRI*NI1D*(GI2G1*DI0Z2+2.*GIG1*GR2*  &
                       DI0Z1+GR3)
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT.AND.MVDI.GE.2.E-4) THEN
               QRMri = QRMR1*ERI*VTQRI*NI1D*(GI3*GR4+2.*GI2*GR5+GR6)
               NRMri = NRMR1*ERI*VTNRI*NI1D*(GI3+2.*GI2*GR2+GR3)
            ENDIF
            QRMri = MIN(QRMri,QR1D*iDT)
            NRMri = MIN(NRMri,NR1D*iDT)
            ENDIF
            MVDX = MAX((MVDR**3.+MVDI**3.)**THRD*RATIO,MVDI,MVDR)
            QCLirg = QRMri+QCLir
            NCLirg = QCLirg*V2M3/(DNIRM*(RATIO*MVDX)**3.)
            IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
               VCLirg = QCLirg/DNIRM
            ENDIF
            ACLirg = (KCIMIN*NCLirg*(QCLirg*V2M3/DNIRM)**2.)**THRD
         ENDIF
         IF (QR1D.GE.QSMALL.AND.QS1D.GE.QSMALL) THEN
            ERS   = 1.
            VTQ0  = VTQR-VTQS
            VTN0  = VTNR-VTNS
            VTAX  = VTAR-VTAS
            VTQRS = SQRT(VTQ0*VTQ0+0.04*VTQR*VTQS)
            VTNRS = SQRT(VTN0*VTN0+0.04*VTNR*VTNS)
            VTARS = SQRT(VTAX*VTAX+0.04*VTAR*VTAS)
            IF (ICE_RHOG.EQ.0) THEN
               DNSRM = RHOG1
            ELSEIF (ICE_RHOG.EQ.1) THEN
               DNRI  = MIN(MAX(-5.E5*MVDR*VTQRS/TC1D,0.),6.)
               DNSRM = 1.E3*(0.078+0.184*DNRI-0.015*DNRI**2.)
            ELSEIF (ICE_RHOG.EQ.2) THEN
               DNSRM = 3.E2*(-5.E5*MVDR*0.6*VTQRS/TC1D)**0.44
            ENDIF
            DNSRM = MIN(MAX(DNSRM,RHOIMIN),RHOG0)
            RHOSW = (QS1D+QR1D)/(QS1D/RHOS+QR1D/RHOW+ISMALL)
            RATIO = (RHOSW/DNSRM)**THRD
            IF (MVDS.GE.2.E-4.AND.MVDS.GE.MVDR) THEN
               QRMrs = QRMR1*ERS*VTQRS*NS1D*(SASR2*GS3*GR4+SASR1*2.*   &
                       GS2*GR5+GR6)
               NRMrs = NRMR1*ERS*VTNRS*NS1D*(SASR2*GS3+SASR1*2.*GS2*   &
                       GR2+GR3)
               QRMrs = MIN(QRMrs,QR1D*iDT)
               NRMrs = MIN(NRMrs,NR1D*iDT)
            ENDIF
            IF (MVDR.GE.MVDS) THEN
               QCLsr = QCLS1*ERS*VTQRS*NR1D*(SASR2*GSM3+SASR1*2.*GR2*  &
                       GSM2+GR3*GSM1)
               NCLsr = NCLS1*ERS*VTNRS*NR1D*(SASR2*GS3+SASR1*2.*GR2*   &
                       GS2+GR3)
               QCLsr = MIN(QCLsr,QS1D*iDT)
               NCLsr = MIN(NCLsr,NS1D*iDT)
               IF (ICE_RHOS.EQ.1) THEN
                  VCLsr = MIN(QCLsr*iRHOS,VS1D*iDT)
               ENDIF
               IF (AGG_SHAPE.EQ.1) THEN
                  FCLsr = MIN(QCLsr*iRHOS*SASPR*V2M3,FS1D*iDT)
               ENDIF
               IF (AS1D.GE.ASMALL) THEN
                  ACLsr = ACLS1*ERS*VTARS*NR1D*(SASR2*GS5+SASR1*2.*GR2*&
                          GS4+GR3*GS3)
                  ACLsr = MIN(ACLsr,AS1D*iDT)
               ENDIF
            ENDIF
            IF (QRMrs.GT.0.) THEN
               NIMrsi = 3.5E8*QRMrs*FF
               QIMrsi = MIN(NIMrsi*MI0,QRMrs)
               VIMrsi = QIMrsi*iRHOI0
               FIMrsi = QIMrsi*1.*iAMI0
               IIMrsi = QIMrsi*iAMI0
               AIMrsi = (KCIMIN*NIMrsi*IIMrsi**2.)**THRD
            ENDIF
            MVDX = MAX((MVDR**3.+MVDS**3.)**THRD*RATIO,MVDR,MVDS)
            QCLsrg = QRMrs+QCLsr
            NCLsrg = QCLsrg*V2M3/(DNSRM*(RATIO*MVDX)**3.)
            IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
               VCLsrg = QCLsrg/DNSRM
            ENDIF
            ACLsrg = (KCIMIN*NCLsrg*(QCLsrg*V2M3/DNSRM)**2.)**THRD
         ENDIF
         IF (QR1D.GE.QSMALL.AND.QG1D.GE.QSMALL) THEN
            VTQ0  = VTQR-VTQG
            VTN0  = VTNR-VTNG
            VTAX  = VTAR-VTAG
            VTQRG = SQRT(VTQ0*VTQ0+0.04*VTQR*VTQG)
            VTNRG = SQRT(VTN0*VTN0+0.04*VTNR*VTNG)
            VTARG = SQRT(VTAX*VTAX+0.04*VTAR*VTAG)
            RHOGW = (QG1D+QR1D)/(QG1D/RHOG+QR1D/RHOW+ISMALL)
            RATIO = (RHOGW/DNGRM)**THRD
            STOKE = (RHOW*ABS(VTQ0)*MVDR**2.)/(9.*MUA*MVDG)
            STOKE = MAX(1.5,MIN(10.,STOKE))
            ERG   = 5.5E-1*LOG10(2.51*STOKE)                            ! parameterization based on Cober and List, 1993 [JAS]
            IF (ICE_RHOG.EQ.0) THEN
               DNGRM = RHOG1
            ELSEIF (ICE_RHOG.EQ.1) THEN
               DNRI  = MIN(MAX(-5.E5*MVDR*VTQRG/TC1D,0.),6.)
               DNGRM = 1.E3*(0.078+0.184*DNRI-0.015*DNRI**2.)
            ELSEIF (ICE_RHOG.EQ.2) THEN
               DNGRM = 3.E2*(-5.E5*MVDR*0.6*VTQRG/TC1D)**0.44
            ENDIF
            DNGRM = MIN(MAX(DNGRM,RHOIMIN),RHOG0)
            IF (MVDG.GE.2.E-4.AND.MVDG.GE.MVDR) THEN
               QRMrg = QRMR1*ERG*VTQRG*NG1D*(GG3*GR4+2.*GG2*GR5+GR6)
               NRMrg = NRMR1*ERG*VTNRG*NG1D*(GG3+2.*GG2*GR2+GR3)
               QRMrg = MIN(QRMrg,QR1D*iDT)
               NRMrg = MIN(NRMrg,NR1D*iDT)
               IF (AG1D.GE.ASMALL) THEN
                  MVDX  = MAX((MVDR**3.+MVDG**3.)**THRD*RATIO,MVDG)
                  ARMrg = (MVDX**2.-MVDG**2.)*NRMR1*ERG*VTARG*NG1D*(   &
                          GG3+2.*GG2*GR2+GR3)
                  ARMrg = MIN(ARMrg,QR1D*iDT*iAPW/MVDR)
               ENDIF
            ENDIF
            IF (MVDR.GE.MVDG) THEN
               QCLgr = QCLG1*ERG*VTQRG*NR1D*(GGM3+2.*GR2*GGM2+GR3*GGM1)
               NCLgr = NCLG1*ERG*VTNRG*NR1D*(GG3+2.*GR2*GG2+GR3)
               QCLgr = MIN(QCLgr,QG1D*iDT)
               NCLgr = MIN(NCLgr,NG1D*iDT)
               IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
                  VCLgr = MIN(QCLgr*iRHOG,VG1D*iDT)
               ENDIF
               IF (AG1D.GE.ASMALL) THEN
                  ACLgr = ACLG1*ERG*VTARG*NR1D*(GG5+2.*GR2*GG4+GR3*GG3)
                  ACLgr = MIN(ACLgr,AG1D*iDT)
               ENDIF
            ENDIF
            IF (QRMrg.GT.0.) THEN
               NIMrgi = 3.5E8*QRMrg*FF
               QIMrgi = MIN(NIMrgi*MI0,QRMrg)
               VIMrgi = QIMrgi*iRHOI0
               FIMrgi = QIMrgi*1.*iAMI0
               IIMrgi = QIMrgi*iAMI0
               AIMrgi = (KCIMIN*NIMrgi*IIMrgi**2.)**THRD
            ENDIF
            MVDX = MAX((MVDR**3.+MVDG**3.)**THRD*RATIO,MVDR,MVDG)
            QCLgrg = QRMrg+QCLgr
            NCLgrg = QCLgrg*V2M3/(DNGRM*(RATIO*MVDX)**3.)
            IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
               VCLgrg = QCLgrg/DNGRM
            ENDIF
!            ACLgrg = (KCIMIN*NCLgrg*(QCLgrg*V2M3/DNGRM)**2.)**THRD
            ACLgrg = ARMrg+ACLgr
         ENDIF
         IF (QH1D.GE.QSMALL.AND.QR1D.GE.QSMALL.AND.MVDH.GE.MVDR) THEN
            ERH   = 1.
            VTQ0  = VTQR-VTQH
            VTN0  = VTNR-VTNH
            VTAX  = VTAR-VTAH
            VTQRH = SQRT(VTQ0*VTQ0+0.04*VTQR*VTQH)
            VTNRH = SQRT(VTN0*VTN0+0.04*VTNR*VTNH)
            VTARH = SQRT(VTAX*VTAX+0.04*VTAR*VTAH)
            RHOHW = (QH1D+QR1D)/(QH1D/RHOH+QR1D/RHOW+ISMALL)
            RATIO = (RHOHW/RHOH)**THRD
            MVDX  = MAX((MVDR**3.+MVDH**3.)**THRD*RATIO,MVDH)
            QRMrh = QRMR1*ERH*VTQRH*NH1D*(GH3*GR4+2.*GH2*GR5+GR6)
            NRMrh = NRMR1*ERH*VTNRH*NH1D*(GH3+2.*GH2*GR2+GR3)
            QRMrh = MIN(QRMrh,QR1D*iDT)
            NRMrh = MIN(NRMrh,NR1D*iDT)
            IF (AH1D.GE.ASMALL) THEN
               ARMrh = (MVDX**2.-MVDH**2.)*NRMR1*ERH*VTARH*NH1D*(GH3+  &
                       2.*GH2*GR2+GR3)
               ARMrh = MIN(ARMrh,QR1D*iDT*iAPW/MVDR)
            ENDIF
         ENDIF
!----------------- ICE-PHASE COLLECTION&CONVERSION*INITILIZATION -------
         IF (QS1D.GE.QSMALL.AND.QI1D.GE.QSMALL.AND.MVDS.GE.MVDI) THEN
!            EIS1  = MIN(1.,0.05*EXP(0.1*TC1D))                          ! FERRIER ET AL., 1995
            EIS1  = 10.**(3.5E-2*TC1D-0.7)
            RHOIS = (QI1D+QS1D)/(QI1D/RHOI+QS1D/RHOS+ISMALL)
            IF (ICE_RHOI.NE.2.AND.ICE_RHOS.NE.0) THEN
               EIS2 = 1.-RHOIS/RHOI0
            ELSE
               EIS2 = 0.
            ENDIF
            EIS   = MIN(MAX(EIS1,EIS2,0.),1.)
            RATIO = (RHOIS/RHOS)**THRD
            MVDX  = MAX((MVDI**3.+MVDS**3.)**THRD*RATIO,MVDS)
            VTQ0  = VTQS-VTQI
            VTN0  = VTNS-VTNI
            VTV0  = VTVS-VTVI
            VTF0  = VTVS-VTFI
            VTQIS = SQRT(VTQ0*VTQ0+0.04*VTQI*VTQS)
            VTNIS = SQRT(VTN0*VTN0+0.04*VTNI*VTNS)
            VTVIS = SQRT(VTV0*VTV0+0.04*VTVI*VTVS)
            VTFIS = SQRT(VTF0*VTF0+0.04*VTFI*VTVS)
            IF (AS1D.GE.ASMALL.OR.AI1D.GE.ASMALL) THEN
               VTAX  = VTAS-VTAI
               VTAIS = SQRT(VTAX*VTAX+0.04*VTAI*VTAS)
            ENDIF
            IF ((ADAGR-1.).GE.SLIMIT) THEN
               QCLis = QCLI1*EIS*VTQIS*NS1D*(GIMF1/DI0Z1+SASR1*GIMG1*  &
                       GS2*DI0Z1+SASR1*GIMH1*GS2/DI0Z2+SASR2*GIM1*GS3)
               NCLis = NCLI1*EIS*VTNIS*NS1D*(GIF1/DI0Z1+SASR1*GIG1*GS2*&
                       DI0Z1+SASR1*GIH1*GS2/DI0Z2+SASR2*GS3)
               IF (AI1D.GE.ASMALL) THEN
                  AiCLis = ACLI1*EIS*VTAIS*NS1D*(GIF3/DI0Z1+SASR1*GIG3*&
                           GS2*DI0Z1+SASR1*GIH3*GS2/DI0Z2+SASR2*GI3*GS3)
               ENDIF
               IF (AS1D.GE.ASMALL) THEN
                  AsCLis = (MVDX**2.-MVDS**2.)*NCLI1*EIS*VTAIS*NS1D*(  &
                           GIF1/DI0Z1+SASR1*GIG1*GS2*DI0Z1+SASR1*GIH1* &
                           GS2/DI0Z2+SASR2*GS3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FiCLis = FCLI1*EIS*VTFIS*NS1D*(GIZMF1/DI0Z1+SASR1*   &
                           GIZMG1*GS2*DI0Z1+SASR1*GIZMH1*GS2/DI0Z2+    &
                           SASR2*GIZM1*GS3)
                  ICLis = NCLI1*EIS*VTVIS*NS1D*(GIMF1/DI0Z1+SASR1*     &
                          GIMG1*GS2*DI0Z1+SASR1*GIMH1*GS2/DI0Z2+SASR2* &
                          GIM1*GS3)
               ENDIF
            ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
               QCLis = QCLI1*EIS*VTQIS*NS1D*(GIM2G1*DI0Z2+SASR1*2.*    &
                       GIMG1*GS2*DI0Z1+SASR2*GIM1*GS3)
               NCLis = NCLI1*EIS*VTNIS*NS1D*(GI2G1*DI0Z2+SASR1*2.*GIG1*&
                       GS2*DI0Z1+SASR2*GS3)
               IF (AI1D.GE.ASMALL) THEN
                  AiCLis = ACLI1*EIS*VTAIS*NS1D*(GI2G3*DI0Z2+SASR1*2.* &
                           GIG3*GS2*DI0Z1+SASR2*GI3*GS3)
               ENDIF
               IF (AS1D.GE.ASMALL) THEN
                  AsCLis = (MVDX**2.-MVDS**2.)*NCLI1*EIS*VTAIS*NS1D*(  &
                           GI2G1*DI0Z2+SASR1*2.*GIG1*GS2*DI0Z1+SASR2*  &
                           GS3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FiCLis = FCLI1*EIS*VTFIS*NS1D*(GIZM2G1*DI0Z2+SASR1*  &
                           2.*GIZMG1*GS2*DI0Z1+SASR2*GIZM1*GS3)
                  ICLis = NCLI1*EIS*VTVIS*NS1D*(GIM2G1*DI0Z2+SASR1*2.* &
                          GIMG1*GS2*DI0Z1+SASR2*GIM1*GS3)
               ENDIF
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
               QCLis = QCLI1*EIS*VTQIS*NS1D*(GIM3+SASR1*2.*GIM2*GS2+   &
                       SASR2*GIM1*GS3)
               NCLis = NCLI1*EIS*VTNIS*NS1D*(GI3+SASR1*2.*GI2*GS2+     &
                       SASR2*GS3)
               IF (AI1D.GE.ASMALL) THEN
                  AiCLis = ACLI1*EIS*VTAIS*NS1D*(GI5+SASR1*2.*GI4*GS2+ &
                           SASR2*GI3*GS3)
               ENDIF
               IF (AS1D.GE.ASMALL) THEN
                  AsCLis = (MVDX**2.-MVDS**2.)*NCLI1*EIS*VTAIS*NS1D*(  &
                           GI3+SASR1*2.*GI2*GS2+SASR2*GS3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FiCLis = NCLI1*EIS*VTFIS*NS1D*(GIM3+SASR1*2.*GIM2*   &
                           GS2+SASR2*GIM1*GS3)
                  ICLis = NCLI1*EIS*VTVIS*NS1D*(GIM3+SASR1*2.*GIM2*GS2+&
                          SASR2*GIM1*GS3)
               ENDIF
            ENDIF
            QCLis  = MIN(QCLis,QI1D*iDT)
            NCLis  = MIN(NCLis,NI1D*iDT)
            AiCLis = MIN(AiCLis,AI1D*iDT)
            ViCLis = MIN(QCLis*iRHOI,VI1D*iDT)
            FiCLis = MIN(FiCLis,FI1D*iDT)
            ICLis  = MIN(ICLis,I3M1D*iDT)
            IF (ICE_RHOS.EQ.1.OR.AGG_SHAPE.EQ.1) THEN
               LIM1 = 0.7*(1.-RHOI/RHOI0)
               LIM2 = SQRT(1.-LIM1*LIM1)
               LIM3 = (1.+2.*ISEPS+ISEPS**2.+ISEPL+2.*ISEPS*ISEPL+     &
                      ISEPS**2.*ISEPL)/8.
               LIM4 = 2.*LIM3*LIM2
               LIM5 = LIM3*LIM2**2.
               LIM6 = LIM3*LIM2
               LIM7 = 2.*LIM3*LIM2**2.
               LIM8 = LIM3*LIM2**3.
               LIMA = (1.+2.*ISEPL+ISEPL**2.+ISEPS+2.*ISEPL*ISEPS+     &
                      ISEPL**2.*ISEPS)/8.
               LIMB = LIMA*LIM2
               LIMC = 2.*LIMA*LIM2
               LIMD = 2.*LIMA*LIM2**2.
               LIME = LIMA*LIM2**2.
               LIMF = LIMA*LIM2**3.
               DSS0 = SASR4*GS2
               DSL0 = SASR1*GS2
               DICC = DI0**(-6.*ZETA)*GI3H1
               DICA = GI2HG1/DI0Z3
               DIAC = GIH2G1
               DIAA = DI0Z3*GI3G1
               DIA0 = DI0Z1*GIG1
               DIC0 = GIH1/DI0Z2
               DIF0 = GIF1/DI0Z1
               DIC2 = GI2H1/DI0Z4
               DIA2 = DI0Z2*GI2G1
               IF ((ADAGR-1.).GE.SLIMIT) THEN
                  DSLSV = LIMA*GS4+LIMB*DIA0*GS3*SASR2+LIMC*DIC0*GS3*  &
                          SASR3+LIMD*DIF0*GS2*SASR1+LIME*DIC2*GS2*     &
                          SASR4+LIMF*DICA
                  DSLSF = LIM3*GS4*SASPR+LIM4*DIA0*GS3*SASR3+LIM5*DIA2*&
                          GS2*SASR1+LIM6*DIC0*GS3*SASR4*SASR4+LIM7*    &
                          DIF0*GS2*SASR4+LIM8*DIAC
                  VCLis = NCLI1*EIS*VTVIS*NS1D*(GIF1/DI0Z1*GSM1+SASR1* &
                          GIG1*GSM2*DI0Z1+SASR1*GIH1*GSM2/DI0Z2+SASR2* &
                          GSM3)/V2M3
                  SASP2  = MIN(1.,((1.+ISEPS)*(DSS0+LIM2*DIA0))/((1.+  &
                           ISEPL)*(DSL0+LIM2*DIC0)))
                  DSLSF  = MAX(DSLSF,MAX(DSLSV,GS4)*SASP2)
               ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
                  DSLSV = LIMA*GS4+LIMB*GS3*SASR2*DIC0+LIMC*DIA0*GS3*  &
                          SASR3+LIMD*DIF0*GS2*SASR1+LIME*DIA2*GS2*     &
                          SASR4+LIMF*DIAC
                  DSLSF = LIM3*GS4*SASPR+LIM4*DIC0*GS3*SASR3+LIM5*DIC2*&
                          GS2*SASR1+LIM6*DIA0*GS3*SASR4*SASR4+LIM7*    &
                          DIF0*GS2*SASR4+LIM8*DICA
                  VCLis = NCLI1*EIS*VTVIS*NS1D*(GI2G1*DI0Z2*GSM1+SASR1*&
                          2.*GIG1*GSM2*DI0Z1+SASR2*GSM3)/V2M3
                  SASP2  = MIN(1.,((1.+ISEPS)*(DSS0+LIM2*DIC0))/((1.+  &
                           ISEPL)*(DSL0+LIM2*DIA0)))
                  DSLSF  = MAX(DSLSF,MAX(DSLSV,GS4)*SASP2)
               ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
                  DSLSV = LIMA*GS4+LIMB*GS3*SASR2*GI2+LIMC*GI2*GS3*    &
                          SASR3+LIMD*GI3*GS2*SASR1+LIME*GI3*GS2*SASR4+ &
                          LIMF*GI4
                  DSLSF = LIM3*GS4*SASPR+LIM4*GI2*GS3*SASR3+LIM5*GI3*  &
                          GS2*SASR1+LIM6*GI2*GS3*SASR4*SASR4+LIM7*GI3* &
                          GS2*SASR4+LIM8*GI4
                  VCLis = NCLI1*EIS*VTVIS*NS1D*(GI3*GSM1+SASR1*2.*GI2* &
                          GSM2+SASR2*GSM3)/V2M3
                  SASP2  = MIN(1.,((1.+ISEPS)*(DSS0+LIM2*GI2))/((1.+   &
                           ISEPL)*(DSL0+LIM2*GI2)))
                  DSLSF  = MAX(DSLSF,MAX(DSLSV,GS4)*SASP2)
               ENDIF
               DNSAC  = (AMI*GIM1*V2M3+RHOS*GSM1)/MAX(DSLSV,GS4)
               DNSAC  = MIN(DNSAC,RHOIS)
               VsCLis = NCLis*MAX(DSLSV-GS4,0.)/V2M3
               FsCLis = NCLis*(DSLSF-GS4*SASPR)
            ENDIF
         ENDIF
         IF (QG1D.GE.QSMALL.AND.QI1D.GE.QSMALL) THEN
!            EIG   = MIN(1.,0.01*EXP(0.1*TC1D))                          ! FERRIER ET AL., 1995
            EIG   = 0.
            RHOIG = (QI1D+QG1D)/(QI1D/RHOI+QG1D/RHOG+ISMALL)
            RATIO = (RHOIG/RHOG)**THRD
            MVDX  = MAX((MVDI**3.+MVDG**3.)**THRD*RATIO,MVDG)
            VTQ0  = VTQG-VTQI
            VTN0  = VTNG-VTNI
            VTV0  = VTVG-VTVI
            VTF0  = VTVG-VTFI
            VTQIG = SQRT(VTQ0*VTQ0+0.04*VTQI*VTQG)
            VTNIG = SQRT(VTN0*VTN0+0.04*VTNI*VTNG)
            VTVIG = SQRT(VTV0*VTV0+0.04*VTVI*VTVG)
            VTFIG = SQRT(VTF0*VTF0+0.04*VTFI*VTVG)
            IF (AG1D.GE.ASMALL.OR.AI1D.GE.ASMALL) THEN
               VTAX  = VTAG-VTAI
               VTAIG = SQRT(VTAX*VTAX+0.04*VTAI*VTAG)
            ENDIF
            IF ((ADAGR-1.).GE.SLIMIT) THEN
               QCLig = QCLI1*EIG*VTQIG*NG1D*(GIMF1/DI0Z1+GIMG1*GG2*    &
                       DI0Z1+GIMH1*GG2/DI0Z2+GIM1*GG3)
               NCLig = NCLI1*EIG*VTNIG*NG1D*(GIF1/DI0Z1+GIG1*GG2*DI0Z1+&
                       GIH1*GG2/DI0Z2+GG3)
               IF (AI1D.GE.ASMALL) THEN
                  AiCLig = ACLI1*EIG*VTAIG*NG1D*(GIF3/DI0Z1+GIG3*GG2*  &
                           DI0Z1+GIH3*GG2/DI0Z2+GI3*GG3)
               ENDIF
               IF (AG1D.GE.ASMALL) THEN
                  AgCLig = (MVDX**2.-MVDG**2.)*NCLI1*EIG*VTAIG*NG1D*(  &
                           GIF1/DI0Z1+GIG1*GG2*DI0Z1+GIH1*GG2/DI0Z2+GG3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FCLig = FCLI1*EIG*VTFIG*NG1D*(GIZMF1/DI0Z1+GIZMG1*   &
                          GG2*DI0Z1+GIZMH1*GG2/DI0Z2+GIZM1*GG3)
                  ICLig = NCLI1*EIG*VTVIG*NG1D*(GIMF1/DI0Z1+GIMG1*GG2* &
                          DI0Z1+GIMH1*GG2/DI0Z2+GIM1*GG3)
               ENDIF
            ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
               QCLig = QCLI1*EIG*VTQIG*NG1D*(GIM2G1*DI0Z2+2.*GIMG1*GG2*&
                       DI0Z1+GIM1*GG3)
               NCLig = NCLI1*EIG*VTNIG*NG1D*(GI2G1*DI0Z2+2.*GIG1*GG2*  &
                       DI0Z1+GG3)
               IF (AI1D.GE.ASMALL) THEN
                  AiCLig = ACLI1*EIG*VTAIG*NG1D*(GI2G3*DI0Z2+2.*GIG3*  &
                           GG2*DI0Z1+GI3*GG3)
               ENDIF
               IF (AG1D.GE.ASMALL) THEN
                  AgCLig = (MVDX**2.-MVDG**2.)*NCLI1*EIG*VTAIG*NG1D*(  &
                           GI2G1*DI0Z2+2.*GIG1*GG2*DI0Z1+GG3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FCLig = FCLI1*EIG*VTFIG*NG1D*(GIZM2G1*DI0Z2+2.*      &
                          GIZMG1*GG2*DI0Z1+GIZM1*GG3)
                  ICLig = NCLI1*EIG*VTVIG*NG1D*(GIM2G1*DI0Z2+2.*GIMG1* &
                          GG2*DI0Z1+GIM1*GG3)
               ENDIF
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
               QCLig = QCLI1*EIG*VTQIG*NG1D*(GIM3+2.*GIM2*GG2+GIM1*GG3)
               NCLig = NCLI1*EIG*VTNIG*NG1D*(GI3+2.*GI2*GG2+GG3)
               IF (AI1D.GE.ASMALL) THEN
                  AiCLig = ACLI1*EIG*VTAIG*NG1D*(GI5+2.*GI4*GG2+GI3*GG3)
               ENDIF
               IF (AG1D.GE.ASMALL) THEN
                  AgCLig = (MVDX**2.-MVDG**2.)*NCLI1*EIG*VTAIG*NG1D*(  &
                           GI3+2.*GI2*GG2+GG3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FCLig = NCLI1*EIG*VTFIG*NG1D*(GIM3+2.*GIM2*GG2+GIM1* &
                          GG3)
                  ICLig = NCLI1*EIG*VTVIG*NG1D*(GIM3+2.*GIM2*GG2+GIM1* &
                          GG3)
               ENDIF
            ENDIF
            QCLig  = MIN(QCLig,QI1D*iDT)
            NCLig  = MIN(NCLig,NI1D*iDT)
            AiCLig = MIN(AiCLig,AI1D*iDT)
            AgCLig = MIN(AgCLig,AI1D*iDT)
            ViCLig = MIN(QCLig*iRHOI,VI1D*iDT)
            FCLig  = MIN(FCLig,FI1D*iDT)
            ICLig  = MIN(ICLig,I3M1D*iDT)
            IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
               LIM1 = 0.7*(1.-RHOI/RHOI0)
               LIM2 = SQRT(1.-LIM1*LIM1)
               LIMA = (1.+2.*ISEPL+ISEPL**2.+ISEPS+2.*ISEPL*ISEPS+     &
                      ISEPL**2.*ISEPS)/8.
               LIMB = LIMA*LIM2
               LIMC = 2.*LIMA*LIM2
               LIMD = 2.*LIMA*LIM2**2.
               LIME = LIMA*LIM2**2.
               LIMF = LIMA*LIM2**3.
               DICC = DI0**(-6.*ZETA)*GI3H1
               DICA = GI2HG1/DI0Z3
               DIAC = GIH2G1
               DIAA = DI0Z3*GI3G1
               DIA0 = DI0Z1*GIG1
               DIC0 = GIH1/DI0Z2
               DIF0 = GIF1/DI0Z1
               DIC2 = GI2H1/DI0Z4
               DIA2 = DI0Z2*GI2G1
               IF ((ADAGR-1.).GE.SLIMIT) THEN
                  DGLSV = LIMA*GG4+LIMB*DIA0*GG3+LIMC*DIC0*GG3+LIMD*   &
                          DIF0*GG2+LIME*DIC2*GG2+LIMF*DICA
                  VCLig = NCLI1*EIG*VTVIG*NG1D*(GIF1/DI0Z1*GGM1+GIG1*  &
                          GGM2*DI0Z1+GIH1*GGM2/DI0Z2+GGM3)/V2M3
               ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
                  DGLSV = LIMA*GG4+LIMB*DIC0*GG3+LIMC*DIA0*GG3+LIMD*   &
                          DIF0*GG2+LIME*DIA2*GG2+LIMF*DIAC
                  VCLig = NCLI1*EIG*VTVIG*NG1D*(GI2G1*DI0Z2*GGM1+2.*   &
                          GIG1*GGM2*DI0Z1+GGM3)/V2M3
               ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
                  DGLSV = LIMA*GG4+LIMB*GI2*GG3+LIMC*GI2*GG3+LIMD*GI3* &
                          GG2+LIME*GI3*GG2+LIMF*GI4
                  VCLig = NCLI1*EIG*VTVIG*NG1D*(GI3*GGM1+2.*GI2*GGM2+  &
                          GGM3)/V2M3
               ENDIF
               DNGAC  = (AMI*GIM1*V2M3+RHOG*GG4)/MAX(DGLSV,GG4)
               DNGAC  = MAX(MIN(DNGAC,RHOIG),RHOIMIN)
               VgCLig = MAX(0.,QCLig/DNGAC+(RHOG/DNGAC-1.)*VCLig)
            ENDIF
         ENDIF
         IF (QH1D.GE.QSMALL.AND.QI1D.GE.QSMALL) THEN
            RHOIH = (QI1D+QH1D)/(QI1D/RHOI+QH1D/RHOH+ISMALL)
            RATIO = (RHOIH/RHOH)**THRD
            MVDX  = MAX((MVDI**3.+MVDH**3.)**THRD*RATIO,MVDH)
            VTQ0  = VTQH-VTQI
            VTN0  = VTNH-VTNI
            VTV0  = VTQH-VTVI
            VTF0  = VTQH-VTFI
            VTQIH = SQRT(VTQ0*VTQ0+0.04*VTQI*VTQH)
            VTNIH = SQRT(VTN0*VTN0+0.04*VTNI*VTNH)
            VTVIH = SQRT(VTV0*VTV0+0.04*VTVI*VTQH)
            VTFIH = SQRT(VTF0*VTF0+0.04*VTFI*VTQH)
            IF (AH1D.GE.ASMALL.OR.AI1D.GE.ASMALL) THEN
               VTAX  = VTAH-VTAI
               VTAIH = SQRT(VTAX*VTAX+0.04*VTAI*VTAH)
            ENDIF
            IF ((ADAGR-1.).GE.SLIMIT) THEN
               QCLih = QCLI1*EIH*VTQIH*NH1D*(GIMF1/DI0Z1+GIMG1*GH2*    &
                       DI0Z1+GIMH1*GH2/DI0Z2+GIM1*GH3)
               NCLih = NCLI1*EIH*VTNIH*NH1D*(GIF1/DI0Z1+GIG1*GH2*DI0Z1+&
                       GIH1*GH2/DI0Z2+GH3)
               IF (AI1D.GE.ASMALL) THEN
                  AiCLih = ACLI1*EIH*VTAIH*NH1D*(GIF3/DI0Z1+GIG3*GH2*  &
                           DI0Z1+GIH3*GH2/DI0Z2+GI3*GH3)
               ENDIF
               IF (AH1D.GE.ASMALL) THEN
                  AhCLih = (MVDX**2.-MVDH**2.)*NCLI1*EIH*VTAIH*NH1D*(  &
                           GIF1/DI0Z1+GIG1*GH2*DI0Z1+GIH1*GH2/DI0Z2+GH3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FCLih = FCLI1*EIH*VTFIH*NH1D*(GIZMF1/DI0Z1+GIZMG1*   &
                          GH2*DI0Z1+GIZMH1*GH2/DI0Z2+GIZM1*GH3)
                  ICLih = NCLI1*EIH*VTVIH*NH1D*(GIMF1/DI0Z1+GIMG1*GH2* &
                          DI0Z1+GIMH1*GH2/DI0Z2+GIM1*GH3)
               ENDIF
            ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
               QCLih = QCLI1*EIH*VTQIH*NH1D*(GIM2G1*DI0Z2+2.*GIMG1*GH2*&
                       DI0Z1+GIM1*GH3)
               NCLih = NCLI1*EIH*VTNIH*NH1D*(GI2G1*DI0Z2+2.*GIG1*GH2*  &
                       DI0Z1+GH3)
               IF (AI1D.GE.ASMALL) THEN
                  AiCLih = ACLI1*EIH*VTAIH*NH1D*(GI2G3*DI0Z2+2.*GIG3*  &
                           GH2*DI0Z1+GI3*GH3)
               ENDIF
               IF (AH1D.GE.ASMALL) THEN
                  AhCLih = (MVDX**2.-MVDH**2.)*NCLI1*EIH*VTAIH*NH1D*(  &
                           GI2G1*DI0Z2+2.*GIG1*GH2*DI0Z1+GH3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FCLih = FCLI1*EIH*VTFIH*NH1D*(GIZM2G1*DI0Z2+2.*      &
                          GIZMG1*GH2*DI0Z1+GIZM1*GH3)
                  ICLih = NCLI1*EIH*VTVIH*NH1D*(GIM2G1*DI0Z2+2.*GIMG1* &
                          GH2*DI0Z1+GIM1*GH3)
               ENDIF
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
               QCLih = QCLI1*EIH*VTQIH*NH1D*(GIM3+2.*GIM2*GH2+GIM1*GH3)
               NCLih = NCLI1*EIH*VTNIH*NH1D*(GI3+2.*GI2*GH2+GH3)
               IF (AI1D.GE.ASMALL) THEN
                  AiCLih = ACLI1*EIH*VTAIH*NH1D*(GI5+2.*GI4*GH2+GI3*GH3)
               ENDIF
               IF (AH1D.GE.ASMALL) THEN
                  AhCLih = (MVDX**2.-MVDH**2.)*NCLI1*EIH*VTAIH*NH1D*(  &
                           GI3+2.*GI2*GH2+GH3)
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FCLih = NCLI1*EIH*VTFIH*NH1D*(GIM3+2.*GIM2*GH2+GIM1* &
                          GH3)
                  ICLih = NCLI1*EIH*VTVIH*NH1D*(GIM3+2.*GIM2*GH2+GIM1* &
                          GH3)
               ENDIF
            ENDIF
            QCLih  = MIN(QCLih,QI1D*iDT)
            NCLih  = MIN(NCLih,NI1D*iDT)
            VCLih  = MIN(QCLih*iRHOI,VI1D*iDT)
            AiCLih = MIN(AiCLih,AI1D*iDT)
            AhCLih = MIN(AhCLih,AI1D*iDT)
            FCLih  = MIN(FCLih,FI1D*iDT)
            ICLih  = MIN(ICLih,I3M1D*iDT)
         ENDIF
         IF (QS1D.GE.QSMALL.AND.QG1D.GE.QSMALL) THEN
!            ESG   = MIN(1.,0.01*EXP(0.1*TC1D))                         ! FERRIER ET AL., 1995
            ESG   = 0.
            RHOSG = (QS1D+QG1D)/(QS1D/RHOS+QG1D/RHOG+ISMALL)
            RATIO = (RHOSG/RHOG)**THRD
            MVDX  = MAX((MVDS**3.+MVDG**3.)**THRD*RATIO,MVDG)
            VTQ0  = VTQG-VTQS
            VTN0  = VTNG-VTNS
            VTV0  = VTVG-VTVS
            VTQSG = SQRT(VTQ0*VTQ0+0.04*VTQS*VTQG)
            VTNSG = SQRT(VTN0*VTN0+0.04*VTNS*VTNG)
            VTVSG = SQRT(VTV0*VTV0+0.04*VTVS*VTVG)
            IF (AS1D.GE.ASMALL.OR.AG1D.GE.ASMALL) THEN
               VTAX  = VTAG-VTAS
               VTASG = SQRT(VTAX*VTAX+0.04*VTAS*VTAG)
            ENDIF
            QCLsg = QCLS1*ESG*VTQSG*NG1D*(SASR2*GSM3+SASR1*2.*GG2*GSM2+&
                    GSM1*GG3)
            NCLsg = NCLS1*ESG*VTNSG*NG1D*(SASR2*GS3+SASR1*2.*GS2*GG2+  &
                    GG3)
            QCLsg = MIN(QCLsg,QS1D*iDT)
            NCLsg = MIN(NCLsg,NS1D*iDT)
            IF (ICE_RHOS.EQ.1) THEN
               VsCLsg = MIN(QCLsg*iRHOS,VS1D*iDT)
            ENDIF
            IF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
               LIM1   = 0.7*(1.-RHOS/RHOI0)
               LIM2   = SQRT(1.-LIM1*LIM1)
               LIMA   = (1.+2.*ISEPL+ISEPL**2.+ISEPS+2.*ISEPL*ISEPS+   &
                        ISEPL**2.*ISEPS)/8.
               LIMB   = LIMA*LIM2
               LIMC   = 2.*LIMA*LIM2
               LIMD   = 2.*LIMA*LIM2**2.
               LIME   = LIMA*LIM2**2.
               LIMF   = LIMA*LIM2**3.
               DGLSV  = LIMA*GG4+(LIMB+LIMC)*GG3*GS2+(LIMD+LIME)*GG2*  &
                        GS3+LIMF*GS4
               DNGAC  = (AMS*GSM1+AMG*GGM1)*V2M3/MAX(DGLSV,GG4)
               DNGAC  = MAX(MIN(DNGAC,RHOSG),RHOIMIN)
               VCLsg  = NCLS1*ESG*VTVSG*NG1D*(SASR2*GS3*GGM1+SASR1*2.* &
                        GS2*GGM2+GGM3)
               VgCLsg = MAX(0.,QCLsg/DNGAC+(RHOG/DNGAC-1.)*VCLsg)
            ENDIF
            IF (AGG_SHAPE.EQ.1) THEN
               FCLsg = MIN(QCLsg*iRHOS*SASPR*V2M3,FS1D*iDT)
            ENDIF
            IF (AS1D.GE.ASMALL) THEN
               AsCLsg = ACLS1*ESG*VTASG*NG1D*(SASR2*GS5+SASR1*2.*GG2*  &
                        GS4+GG3*GS3)
               AsCLsg = MIN(AsCLsg,AS1D*iDT)
            ENDIF
            IF (AG1D.GE.ASMALL) THEN
               AgCLsg = (MVDX**2.-MVDG**2.)*NCLS1*ESG*VTASG*NG1D*(     &
                        SASR2*GS3+SASR1*2.*GS2*GG2+GG3)
            ENDIF
         ENDIF
         IF (QH1D.GE.QSMALL.AND.QS1D.GE.QSMALL) THEN
            RHOSH = (QS1D+QH1D)/(QS1D/RHOS+QH1D/RHOH+ISMALL)
            RATIO = (RHOSH/RHOH)**THRD
            MVDX  = MAX((MVDS**3.+MVDH**3.)**THRD*RATIO,MVDH)
            VTQ0  = VTQH-VTQS
            VTN0  = VTNH-VTNS
            VTQSH = SQRT(VTQ0*VTQ0+0.04*VTQS*VTQH)
            VTNSH = SQRT(VTN0*VTN0+0.04*VTNS*VTNH)
            IF (AH1D.GE.ASMALL.OR.AS1D.GE.ASMALL) THEN
               VTAX  = VTAH-VTAS
               VTASH = SQRT(VTAX*VTAX+0.04*VTAS*VTAH)
            ENDIF
            QCLsh = QCLS1*ESH*VTQSH*NH1D*(SASR2*GSM3+SASR1*2.*GH2*GSM2+&
                    GH3*GSM1)
            NCLsh = NCLS1*ESH*VTNSH*NH1D*(SASR2*GS3 +SASR1*2.*GH2*GS2+ &
                    GH3)
            QCLsh = MIN(QCLsh,QS1D*iDT)
            NCLsh = MIN(NCLsh,NS1D*iDT)
            IF (ICE_RHOS.EQ.1) THEN
               VCLsh = MIN(QCLsh*iRHOS,VS1D*iDT)
            ENDIF
            IF (AGG_SHAPE.EQ.1) THEN
               FCLsh = MIN(QCLsh*iRHOS*SASPR*V2M3,FS1D*iDT)
            ENDIF
            IF (AS1D.GE.ASMALL) THEN
               AsCLsh = ACLS1*ESH*VTASH*NH1D*(SASR2*GS5+SASR1*2.*GH2*  &
                        GS4+GH3*GS3)
               AsCLsh = MIN(AsCLsh,AS1D*iDT)
            ENDIF
            IF (AH1D.GE.ASMALL) THEN
               AhCLsh = (MVDX**2.-MVDH**2.)*NCLS1*ESH*VTASH*NH1D*(     &
                        SASR2*GS3 +SASR1*2.*GH2*GS2+GH3)
            ENDIF
         ENDIF
         IF (QS1D.GE.QSMALL.AND.MVDS.GE.2.E-4) THEN
            ESS1 = 0.1*MIN(1.,0.05*EXP(0.1*TC1D))                      ! FERRIER ET AL., 1995
            IF (ICE_RHOS.NE.0) THEN
               ESS2 = 1.-RHOS/RHOI0
            ELSE
               ESS2 = 0.
            ENDIF
!            ESS    = MIN(MAX(ESS1,ESS2,0.),1.)
            ESS   = ESS1
            QCLss = 2.*PI*XISP*VTQS*ESS*SASR2/3.*NS1D**2.*GSM3*AMS
            NCLss = -2.*PI*XISP*VTNS*ESS*SASR2/3.*NS1D**2.*GS3
            IF (ICE_RHOS.EQ.1.OR.AGG_SHAPE.EQ.1) THEN
               LIM1  = 0.7*(1.-RHOS/RHOI0)
               LIM2  = SQRT(1.-LIM1*LIM1)
               LIM3  = (1.+2.*ISEPS+ISEPS**2.+ISEPL+2.*ISEPS*ISEPL+    &
                       ISEPS**2.*ISEPL)/8.
               LIM4  = 2.*LIM3*LIM2
               LIM5  = LIM3*LIM2**2.
               LIM6  = LIM3*LIM2
               LIM7  = 2.*LIM3*LIM2**2.
               LIM8  = LIM3*LIM2**3.
               LIMA  = (1.+2.*ISEPL+ISEPL**2.+ISEPS+2.*ISEPL*ISEPS+    &
                       ISEPL**2.*ISEPS)/8.
               LIMB  = LIMA*LIM2
               LIMC  = 2.*LIMA*LIM2
               LIMD  = 2.*LIMA*LIM2**2.
               LIME  = LIMA*LIM2**2.
               LIMF  = LIMA*LIM2**3.
               DSLSV = (LIMA+LIMB+LIMC+LIMD+LIME+LIMF)*GS4
               DSLSF = (LIM3+LIM4+LIM5+LIM6+LIM7+LIM8)*GS4*SASPR
               SASP3 = MIN((1.+LIM2+ISEPS+ISEPS*LIM2)*SASR4,(1.+LIM2+  &
                       ISEPL+ISEPL*LIM2)*SASR1)/MAX((1.+LIM2+ISEPS+    &
                       ISEPS*LIM2)*SASR4,(1.+LIM2+ISEPL+ISEPL*LIM2)*   &
                       SASR1)
               DNSAG = MAX(RHOIMIN,2.*RHOS*GSM1/DSLSV)
               VCLss = 2.*QCLss*MAX(1./DNSAG-iRHOS,0.)
               FCLss = 2.*QCLss*V2M3*MAX(SASP3/DNSAG-SASPR*iRHOS,0.)
               RATIO = (RHOS/DNSAG)**THRD
            ELSE
               RATIO = 1.
            ENDIF
            IF (AS1D.GE.ASMALL) THEN
               ACLss = 2.*PI*XISP*VTAS*ESS*SASR2/3.*NS1D**2.*GS5*(     &
                       1.5874*RATIO**2.-2.)
               IF (ACLss.LT.0.) THEN
                  ACLss1 = MIN(ACLss,0.)
                  ACLss1 = MAX(ACLss1,-1.*AS1D*iDT)
                  ACLss  = 0.
               ENDIF
            ENDIF
         ENDIF
      ELSE                                                              ! WARM CLOUD START
         IF ((HCwqv+HRwqv+HSwqv+HGwqv+HHwqv).GE.QSMALL) THEN
            VDMAX = XXLV*(QV1D-QVSW0)/(1.+XXLV**2.*QV1D/(CPM*RV*TK1D** &
                    2.))*iDT
            SUMCND = HCwqv+HRwqv+HSwqv+HGwqv+HHwqv
            IF (SUMCND.GT.VDMAX.AND.VDMAX.GE.QSMALL) THEN
               RATIO = MIN(1.,VDMAX/(SUMCND+QSMALL))
               HCwqv = HCwqv*RATIO
               HRwqv = HRwqv*RATIO
               HSwqv = HSwqv*RATIO
               HGwqv = HGwqv*RATIO
               HHwqv = HHwqv*RATIO
            ENDIF
         ENDIF
         IF ((HCwqv+HRwqv+HSwqv+HGwqv+HHwqv).LT.0.) THEN
            EVMAX = XXLV*(QV1D-QVSW0)/(1.+XXLV**2.*QV1D/(CPM*RV*TK1D** &
                    2.))*iDT
            SUMEVP = HCwqv+HRwqv+HSwqv+HGwqv+HHwqv
            IF (EVMAX.LT.0..AND.SUMEVP.LT.EVMAX*0.9999) THEN
               HCwqv = HCwqv*MIN(1.,0.9999*EVMAX/SUMEVP)
               HRwqv = HRwqv*MIN(1.,0.9999*EVMAX/SUMEVP)
               HSwqv = HSwqv*MIN(1.,0.9999*EVMAX/SUMEVP)
               HGwqv = HGwqv*MIN(1.,0.9999*EVMAX/SUMEVP)
               HHwqv = HHwqv*MIN(1.,0.9999*EVMAX/SUMEVP)
            ENDIF
         ENDIF
!----------------- MELTING PROXESSES -----------------------------------
         IF (QI1D.GE.QSMALL) THEN
            IF (MVDI.GT.DCR) THEN
               QMLir = -1.*QI1D*iDT
               VMLir = -1.*VI1D*iDT
               IF (AI1D.GE.ASMALL) THEN
                  AMLir = -1.*AI1D*iDT
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FMLir = -1.*FI1D*iDT
                  IMLir = -1.*I3M1D*iDT
               ENDIF
            ELSE
               QMLic = -1.*QI1D*iDT
               VMLic = -1.*VI1D*iDT
               IF (AI1D.GE.ASMALL) THEN
                  AMLic = -1.*AI1D*iDT
               ENDIF
               IF (I3M1D.GE.ISMALL.AND.FI1D.GE.ISMALL) THEN
                  FMLic = -1.*FI1D*iDT
                  IMLic = -1.*I3M1D*iDT
               ENDIF
            ENDIF
            NMLir = MIN(QMLir*NI1D/QI1D,0.)
            NMLic = MIN(QMLic*NI1D/QI1D,0.)
         ENDIF
         IF (QS1D.GE.QSMALL) THEN
            RMcsq = 0.
            RMrsq = 0.
            RMcsa = 0.
            RMrsa = 0.
            IF (QC1D.GE.QSMALL) THEN
               ECS   = 1.
               VTQ0  = VTQS-VTQC
               VTQSC = SQRT(VTQ0*VTQ0+4.E-2*VTQS*VTQC)
               RMcsq = QRMC1*ECS*VTQSC*NS1D*(GS3*GC4+2.*GS2*GC5+GC6)
               RMcsq = MIN(RMcsq,QC1D*iDT)
               IF (AS1D.GE.ASMALL) THEN
                  MVDX  = MAX((MVDC**3.+MVDS**3.)**THRD,MVDS)
                  RMcsa = (MVDX**2.-MVDS**2.)*NRMC1*VTQSC*NS1D*(GS3+   &
                          2.*GS2*GC2+GC3)
                  RMcsa = MAX(0.,MIN(RMcsa,QC1D*iDT*iAPW/MVDC))
               ENDIF
            ENDIF
            IF (QR1D.GE.QSMALL) THEN
               ERS   = 1.
               VTQ0  = VTQR-VTQS
               VTQRS = SQRT(VTQ0*VTQ0+4.E-2*VTQR*VTQS)
               RMrsq = QRMR1*ERS*VTQRS*NS1D*(GS3*GR4+2.*GS2*GR5+GR6)
               RMrsq = MIN(RMrsq,QR1D*iDT)
               IF (AS1D.GE.ASMALL) THEN
                  MVDX  = MAX((MVDR**3.+MVDS**3.)**THRD,MVDS)
                  RMrsa = (MVDX**2.-MVDS**2.)*NRMR1*VTQRS*NS1D*(GS3+   &
                          2.*GS2*GR2+GR3)
                  RMrsa = MAX(0.,MIN(RMrsa,QR1D*iDT*iAPW/MVDR))
               ENDIF
            ENDIF
            MLMAX = 1.-MIN(1.,RHOS/RHOIMAX)
            HSwcd = 2.*PI*NS1D*KAP*VENQS*(TK0C-TK1D)
            HSwrm = CPW*(TK0C-TK1D)*(RMrsq+RMcsq)
            SMLF  = MAX(0.,-1.*(HSwcd+HSwqv+HSwrm)*DT/XXLF0)/QS1D
            IF (SMLF.LT.MLIMIT) THEN
               SMLF = 0.
               SMLR = 0.
            ELSE
               SMLF = MIN(MLMAX,MAX(0.,SMLF))
               SMLR = 0.01195*EXP(4.411*SMLF)
            ENDIF
            AVWSG = SMLR*AVRH+(1.-SMLR)*AVSG
            BVWSG = SMLR*BVRH+(1.-SMLR)*BVSG
            RHOWS = SMLF*RHOW+(1.-SMLF)*RHOS
            IF (ICE_VTS.EQ.1.OR.ICE_VTS.EQ.2) THEN
               IF (SMLF.GE.MLIMIT) THEN
                  KINV  = (1.72E-5*(393./(TK1D+120.))*(TK1D/TK0C)**    &
                          1.5)/RHO
                  BEST0 = 2.*G*NS1D/(KINV**2.)
                  BEST  = 2.*BEST0*RHOWS*EXP(GAMLN(BMS+AFAS+1.)-GAMLN( &
                          AFAS+1.)-BMS*LOG(LAMS))/3.
                  C1X2  = VTC1*BEST**5.E-1
                  VTB1  = C1X2/(1.+C1X2)**5.E-1/((1.+C1X2)**5.E-1-1.)/2.
                  VTA1  = VTC2*((1+C1X2)**5.E-1-1.)**2./BEST**VTB1
                  AVS   = VTA1*KINV**(1.-2.*VTB1)*(4.*RHOWS*G/3.)**VTB1
                  BVS   = VTB1*BMS-1.
               ELSE
                  AVS = AVS
                  BVS = BVS
               ENDIF
            ENDIF
            BSTMP = SCN*SQRT(AVS*RHOAJ/MUA)
            QTMP1 = LLMS*(1.5+BVS/2.)
            QTMP2 = EXP(GAMLN(AFAS+2.)-GAMLN(AFAS+1.)-LLMS)
            QTMP3 = EXP(GAMLN(BVS/2.+AFAS+2.5)-GAMLN(AFAS+1.)-QTMP1)
            IF (AGG_SHAPE.EQ.0) THEN
               WSAPR = 1.
            ELSEIF (AGG_SHAPE.EQ.1) THEN
               DSMM  = MVDS*1.E3
               WSAPR = SMLF*(0.9951+2.51E-2*DSMM-3.644E-2*DSMM**2.+5.303E-3*&
                       DSMM**3.-2.492E-4*DSMM**4.)+(1.-SMLF)*SASPR
            ENDIF
            CAPWS = ZP1*WSAPR**(ZP2/3.)+ZP3*WSAPR**(ZP4/3.)
            VENWS = AVWSG*QTMP2*CAPWS+BVWSG*BSTMP*QTMP3*CAPWS
            SMLTQ = CPW/XXLF*(TK0C-TK1D)*(RMcsq+RMrsq)
            QMLsr = 2.*PI*NS1D*KAP*(TK0C-TK1D)/XXLF*VENWS+SMLTQ     ! NEGATIVE
            QMLsr = MAX(MIN(QMLsr,0.),-1.*QS1D*iDT)
            NMLsr = MIN(QMLsr*NS1D/QS1D,0.)
            IF (ICE_RHOS.EQ.1) THEN
               VMLsr = ((QS1D+QMLsr*DT)/RHOWS-VS1D)*iDT
               VMLsr = MAX(VMLsr,-1.*VS1D*iDT)
            ENDIF
            IF (AGG_SHAPE.EQ.1) THEN
               FMLsr = MAX((-1.*FS1D+WSAPR*(QS1D/RHOWS*V2M3+QMLsr/     &
                       RHOWS*V2M3*DT))*iDT,-1.*FS1D*iDT)
            ENDIF
            IF (AS1D.GE.ASMALL) THEN
               ATMP1 = LOG(LAMS)*(0.5+BVS/2.)
               ATMP2 = EXP(GAMLN(BVS/2.+AFAS+1.5)-GAMLN(AFAS+1.)-ATMP1)
               VENAS = AVWSG*CAPWS+BVWSG*BSTMP*ATMP2*CAPWS
               SMLTA = CPW/XXLF*(TK0C-TK1D)*(RMcsa+RMrsa)
               AMLsr = 8.*NS1D*KAP*(TK0C-TK1D)/XXLF/RHOWS*VENAS+SMLTA
               AMLsr = MAX(MIN(AMLsr,0.),-1.*AS1D*iDT)
            ENDIF
         ENDIF
         IF (QG1D.GE.QSMALL) THEN
            RMcgq = 0.
            RMrgq = 0.
            RMcga = 0.
            RMrga = 0.
            IF (QC1D.GE.QSMALL) THEN
               ECG   = 1.
               VTQ0  = VTQG-VTQC
               VTQGC = SQRT(VTQ0*VTQ0+0.04*VTQG*VTQC)
               RMcgq = QRMC1*ECG*VTQGC*NG1D*(GG3*GC4+2.*GG2*GC5+GC6)
               RMcgq = MIN(RMcgq,QC1D*iDT)
               IF (AG1D.GE.ASMALL) THEN
                  MVDX  = MAX((MVDC**3.+MVDG**3.)**THRD,MVDG)
                  RMcga = (MVDX**2.-MVDG**2.)*NRMC1*VTQGC*NG1D*(GG3+   &
                          2.*GG2*GR2+GR3)
                  RMcga = MAX(0.,MIN(RMcga,QC1D*iDT*iAPW/MVDC))
               ENDIF
            ENDIF
            IF (QR1D.GE.QSMALL) THEN
               ERG  = 1.
               VTQ0  = VTQR-VTQG
               VTQRG = SQRT(VTQ0*VTQ0+0.04*VTQR*VTQG)
               RMrgq = QRMR1*ERG*VTQRG*NG1D*(GG3*GR4+2.*GG2*GR5+GR6)
               RMrgq = MIN(RMrgq,QR1D*iDT)
               IF (AG1D.GE.ASMALL) THEN
                  MVDX  = MAX((MVDR**3.+MVDG**3.)**THRD,MVDG)
                  RMrga = (MVDX**2.-MVDG**2.)*NRMR1*VTQRG*NG1D*(GG3+   &
                          2.*GG2*GR2+GR3)
                  RMrga = MAX(0.,MIN(RMrga,QR1D*iDT*iAPW/MVDR))
               ENDIF
            ENDIF
            MLMAX = 1.-MIN(1.,RHOG/RHOIMAX)
            HGwcd = 2.*PI*NG1D*KAP*VENQG*(TK0C-TK1D)
            HGwrm = CPW*(TK0C-TK1D)*(RMrgq+RMcgq)
            GMLF  = MAX(0.,-1.*(HGwcd+HGwqv+HGwrm)*DT/XXLF0)/QG1D
            IF (GMLF.LT.MLIMIT) THEN
               GMLF = 0.
               GMLR = 0.
            ELSE
               GMLF = MIN(MLMAX,MAX(0.,GMLF))
               GMLR = 0.01195*EXP(4.411*GMLF)
            ENDIF
            AVWSG = GMLR*AVRH+(1.-GMLR)*AVSG
            BVWSG = GMLR*BVRH+(1.-GMLR)*BVSG
            RHOWG = GMLF*RHOW+(1.-GMLF)*RHOG
            IF (ICE_VTG.EQ.1) THEN
               KINV  = (1.72E-5*(393./(TK1D+120.))*(TK1D/TK0C)**1.5)/RHO
               BEST0 = 2.*G*NG1D/(KINV**2.)
               BEST  = 2.*BEST0*RHOWG*EXP(GAMLN(BMG+AFAG+1.)-GAMLN(    &
                       AFAG+1.)-BMG*LOG(LAMG))/3.
               C1X2  = VTC1*BEST**5.E-1
               VTB1  = C1X2/(1.+C1X2)**5.E-1/((1.+C1X2)**5.E-1-1.)/2.
               VTA1  = VTC2*((1+C1X2)**5.E-1-1.)**2./BEST**VTB1
               AVG   = VTA1*KINV**(1.-2.*VTB1)*(4.*RHOWG*G/3.)**VTB1
               BVG   = VTB1*BMG-1.
            ENDIF
            BGTMP = SCN*SQRT(AVG*RHOAJ/MUA)
            QTMP1 = LLMG*(1.5+BVG/2.)
            QTMP2 = EXP(GAMLN(AFAG+2.)-GAMLN(AFAG+1.)-LLMG)
            QTMP3 = EXP(GAMLN(BVG/2.+AFAG+2.5)-GAMLN(AFAG+1.)-QTMP1)
            VENWG = AVWSG*QTMP2+BVWSG*BGTMP*QTMP3
            GMLTQ = CPW/XXLF*(TK0C-TK1D)*(RMcgq+RMrgq)
            QMLgr = 2.*PI*NG1D*KAP*(TK0C-TK1D)/XXLF*VENWG+GMLTQ     ! NEGATIVE
            QMLgr = MAX(MIN(QMLgr,0.),-1.*QG1D*iDT)
            NMLgr = MIN(QMLgr*NG1D/QG1D,0.)
            VMLgr = MAX(((QG1D+QMLgr*DT)/RHOWG-VG1D)*iDT,-1.*VG1D*iDT)
            IF (AG1D.GE.ASMALL) THEN
               ATMP1 = LOG(LAMG)*(0.5+BVG/2.)
               ATMP2 = EXP(GAMLN(BVG/2.+AFAG+1.5)-GAMLN(AFAG+1.)-ATMP1)
               VENAG = AVWSG+BVWSG*BGTMP*ATMP2
               GMLTA = CPW/XXLF*(TK0C-TK1D)*(RMcga+RMrga)
               AMLgr = 8.*NG1D*KAP*(TK0C-TK1D)/XXLF/RHOWG*VENAG+GMLTA ! NEGATIVE
               AMLgr = MAX(MIN(AMLgr,0.),-1.*AG1D*iDT)
            ENDIF
         ENDIF
         IF (QH1D.GE.QSMALL) THEN
            RMchq = 0.
            RMrhq = 0.
            RMcha = 0.
            RMrha = 0.
            IF (QC1D.GE.QSMALL) THEN
               ECH   = 1.
               VTQ0  = VTQH-VTQC
               VTQHC = SQRT(VTQ0*VTQ0+0.04*VTQH*VTQC)
               RMchq = QRMC1*ECH*VTQHC*NH1D*(GH3*GC4+2.*GH2*GC5+GC6)
               RMchq = MIN(RMchq,QC1D*iDT)
               IF (AH1D.GE.ASMALL) THEN
                  MVDX  = MAX((MVDC**3.+MVDH**3.)**THRD,MVDH)
                  RMcha = (MVDX**2.-MVDH**2.)*NRMC1*VTQHC*NH1D*(GH3+   &
                          2.*GH2*GR2+GR3)
                  RMcha = MAX(0.,MIN(RMcha,QC1D*iDT*iAPW/MVDC))
               ENDIF
            ENDIF
            IF (QR1D.GE.QSMALL) THEN
               ERH   = 1.
               VTQ0  = VTQH-VTQR
               VTQRH = SQRT(VTQ0*VTQ0+0.04*VTQH*VTQR)
               RMrhq = QRMR1*ERH*VTQRH*NH1D*(GH3*GR4+2.*GH2*GR5+GR6)
               RMrhq = MIN(RMrhq,QR1D*iDT)
               IF (AH1D.GE.ASMALL) THEN
                  MVDX  = MAX((MVDR**3.+MVDH**3.)**THRD,MVDH)
                  RMrha = (MVDX**2.-MVDH**2.)*NRMR1*VTQRH*NH1D*(GH3+   &
                          2.*GH2*GR2+GR3)
                  RMrha = MAX(0.,MIN(RMrha,QR1D*iDT*iAPW/MVDR))
               ENDIF
            ENDIF
            HMLTQ = CPW/XXLF*(TK0C-TK1D)*(RMrhq+RMchq)
            QMLhr = 2.*PI*NH1D*KAP*(TK0C-TK1D)/XXLF*VENQH+HMLTQ    ! NEGATIVE
            QMLhr = MAX(MIN(QMLhr,0.),-1.*QH1D*iDT)
            NMLhr = MIN(QMLhr*NH1D/QH1D,0.)
            IF (AH1D.GE.ASMALL) THEN
               HMLTA = CPW/XXLF*(TK0C-TK1D)*(RMrha+RMcha)
               AMLhr = 8.*NH1D*KAP*(TK0C-TK1D)/XXLF/RHOH*VENAH+HMLTA ! NEGATIVE
               AMLhr = MAX(MIN(AMLhr,0.),-1.*AH1D*iDT)
            ENDIF
         ENDIF
      ENDIF                                                             ! TEMPERATURE LOOPS
!----------------- HAILSTONE GROWTH HEATBALANCE ------------------------
      IF (HWET_MODE.EQ.1) THEN
         DMWDT = QRMch+QRMrh
         DMIDT = QCLih+QCLsh
         IF (QH1D.GE.QLIMIT.AND.DMWDT.GE.QSMALL) THEN
            HHdcd = 2.*PI*NH1D*KAP*VENQH*(TK1D-TK0C)
            HHdrm = DMWDT*(XXLF+CPW*(TK1D-TK0C))
            HHdcl = DMIDT*CPI*(TK1D-TK0C)
            HHdtt = HHdcd+HHdqv+HHdrm+HHdcl
            IF (HHdtt.GT.0.) THEN                                       ! WET-GROWTH MODE
               QCLih = QCLih/EIH; NCLih = NCLih/EIH; VCLih = VCLih/EIH
               FCLih = FCLih/EIH; ICLih = ICLih/EIH
               QCLsh = QCLsh/ESH; NCLsh = NCLsh/ESH; VCLsh = VCLsh/ESH
               AiCLih = AiCLih/EIH; AhCLih = AhCLih/EIH
               AsCLsh = AsCLsh/ESH; AhCLsh = AhCLsh/ESH
               MCORE = (QH1D*iDT+QCLih+QCLsh)
               HHwcd = 2.*PI*NH1D*KAP*VENQH*(TK0C-TK1D)
               HHwrm = CPW*(TK0C-TK1D)*DMWDT
               MLWM  = MAX(0.,-1.*(HHwcd+HHwqv+HHwrm)*DT/XXLF0)
               HMLF  = MIN(0.9,MAX(0.,MLWM/QH1D))
               IF (MLWM.GT.DMWDT) THEN
                  QHwml = MAX(-1.*MCORE,DMWDT-MLWM)
                  AHwml = QHwml*4.*iRHOH/PI/MVDH
               ENDIF
               MLWC = 0.268+0.1389*MCORE*1.E3
               IF ((MLWM*1.E3).GT.MLWC) THEN
                  DH9   = MVDH*1.E2-9.E-1
                  MWT   = (QH1D*iDT+QCLih+QCLsh+DMWDT)*1.E3
                  ICOR1 = MWT/(1.+10.67*DH9-10.81*DH9**2.+10.26*DH9**3.)
                  QHwsh = MAX(0.,MLWM-MAX(0.,ICOR1*1.E-3))
                  NHwsh = QHwsh/(AMW*DSHED**BMW)
               ENDIF
            ELSE                                                        ! DRY-GROWTH MODE
               QHdrm = DMWDT
               AHdrm = ARMch+ARMrh
            ENDIF
         ENDIF
      ELSEIF (HWET_MODE.EQ.0) THEN
         QHdrm = QRMch+QRMrh
         AHdrm = ARMch+ARMrh
      ENDIF                                                             ! HAILSTONE WET GROWTH MODE OPTION
!----------------- CONSERVATION OF HYDROMETEORS ------------------------
      QCSOUR = QC1D+(QBKrc-QMLic)*DT
      QCSINK = (QHOci+QNMci+QNCci+QRMci+QRMcs+QRMcg+QRMch+QCLcr+QCNcr+ &
               QIMcsi+QIMcgi)*DT
      IF (QCSINK.GT.QCSOUR.AND.QCSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QCSOUR/(QCSINK+QSMALL))
         QHOci = QHOci*RATIO; QNMci = QNMci*RATIO; QNCci = QNCci*RATIO
         QRMci = QRMci*RATIO; QRMcs = QRMcs*RATIO; QRMcg = QRMcg*RATIO
         QRMch = QRMch*RATIO; QCLcr = QCLcr*RATIO; QCNcr = QCNcr*RATIO
         QIMcsi = QIMcsi*RATIO; QIMcgi = QIMcgi*RATIO
      ENDIF
      NCSOUR = NC1D+(NBKrc-NMLic)*DT
      NCSINK = (NHOci+NNMci+NNCci+NRMci+NRMcs+NRMcg+NRMch+NCLcr+NCLcc+ &
               NCNcr+NIMcsi+NIMcgi)*DT
      IF (NCSINK.GT.NCSOUR.AND.NCSOUR.GE.NSMALL) THEN
         RATIO = MIN(1.,NCSOUR/(NCSINK+NSMALL))
         NHOci = NHOci*RATIO; NNMci = NNMci*RATIO; NNCci = NNCci*RATIO
         NRMci = NRMci*RATIO; NRMcs = NRMcs*RATIO; NRMcg = NRMcg*RATIO
         NRMch = NRMch*RATIO; NCLcr = NCLcr*RATIO; NCLcc = NCLcc*RATIO
         NCNcr = NCNcr*RATIO; NIMcsi = NIMcsi*RATIO
         NIMcgi = NIMcgi*RATIO
      ENDIF
      QRSOUR = QR1D+(QCLcr+QCNcr-QMLir-QMLsr-QMLgr-QMLhr+QHwsh-QHwml)*DT
      QRSINK = (QHOrg+QNMrg+QRMri+QRMrs+QRMrg+QRMrh+QBKrc+QIMrsi+      &
               QIMrgi)*DT
      IF (QRSINK.GT.QRSOUR.AND.QRSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QRSOUR/(QRSINK+QSMALL))
         QHOrg = QHOrg*RATIO; QNMrg = QNMrg*RATIO; QRMri = QRMri*RATIO
         QRMrs = QRMrs*RATIO; QRMrg = QRMrg*RATIO; QRMrh = QRMrh*RATIO
         QBKrc = QBKrc*RATIO; QIMrsi = QIMrsi*RATIO
         QIMrgi = QIMrgi*RATIO
      ENDIF
      NRSOUR = NR1D+(NCNcr+NBKrr-NMLir-NMLsr-NMLgr-NMLhr+NCLcr+NHwsh)*DT
      NRSINK = (NHOrg+NNMrg+NRMri+NRMrs+NRMrg+NRMrh+NCLrr+NBKrc+NIMrsi+&
                NIMrgi)*DT
      IF (NRSINK.GT.NRSOUR.AND.NRSOUR.GE.NSMALL) THEN
         RATIO = MIN(1.,NRSOUR/(NRSINK+NSMALL))
         NHOrg = NHOrg*RATIO; NNMrg = NNMrg*RATIO; NRMri = NRMri*RATIO
         NRMrs = NRMrs*RATIO; NRMrg = NRMrg*RATIO; NRMrh = NRMrh*RATIO
         NCLrr = NCLrr*RATIO; NBKrc = NBKrc*RATIO; NIMrsi = NIMrsi*RATIO
         NIMrgi = NIMrgi*RATIO
      ENDIF
      QISOUR = QI1D+(QIMcsi+QIMcgi+QIMrsi+QIMrgi+QHOci+QNMci+QNCci+    &
                     QRMci)*DT
      QISINK = (-QMLir-QMLic+QCLir+QCLis+QCLig+QCLih+QINig+QCNis)*DT
      IF (QISINK.GT.QISOUR.AND.QISOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QISOUR/(QISINK+QSMALL))
         QMLir = QMLir*RATIO; QMLic = QMLic*RATIO; QCLir = QCLir*RATIO
         QCLis = QCLis*RATIO; QCLig = QCLig*RATIO; QCLih = QCLih*RATIO
         QINig = QINig*RATIO; QCNis = QCNis*RATIO
      ENDIF
      NISOUR = NI1D+(NIMcsi+NIMcgi+NIMrsi+NIMrgi+NHOci+NNMci+NNCci)*DT
      NISINK = (-NMLir-NMLic+NCLir+NCLis+NCLig+NCLih+NINig+NiCNis)*DT
      IF (NISINK.GT.NISOUR.AND.NISOUR.GE.NSMALL) THEN
         RATIO = MIN(1.,NISOUR/(NISINK+NSMALL))
         NCLis = NCLis*RATIO; NCLig = NCLig*RATIO; NCLih = NCLih*RATIO
         NINig = NINig*RATIO; NMLir = NMLir*RATIO; NMLic = NMLic*RATIO
         NCLir = NCLir*RATIO; NiCNis = NiCNis*RATIO
      ENDIF
      VISOUR = VI1D+(VIMcsi+VIMcgi+VIMrsi+VIMrgi+VHOci+VNMci+VNCci+    &
                     VRMci)*DT
      VISINK = (-VMLir-VMLic+VCLir+ViCLis+ViCLig+VCLih+ViINig+ViCNis)*DT
      IF (VISINK.GT.VISOUR.AND.VISOUR.GE.ISMALL) THEN
         RATIO = MIN(1.,VISOUR/(VISINK+ISMALL))
         VMLir = VMLir*RATIO; VMLic = VMLic*RATIO; VCLir = VCLir*RATIO
         ViCLis = ViCLis*RATIO; ViCLig = ViCLig*RATIO
         VCLih = VCLih*RATIO; ViINig = ViINig*RATIO
         ViCNis = ViCNis*RATIO
      ENDIF
      FISOUR = FI1D+(FIMcsi+FIMcgi+FIMrsi+FIMrgi+FHOci+FNMci+FNCci+    &
                     FRMci)*DT
      FISINK = (-FMLir-FMLic+FCLir+FiCLis+FCLig+FCLih+FINig+FiCNis)*DT
      IF (FISINK.GT.FISOUR.AND.FISOUR.GE.ISMALL) THEN
         RATIO = MIN(1.,FISOUR/(FISINK+ISMALL))
         FMLir = FMLir*RATIO; FMLic = FMLic*RATIO; FCLir = FCLir*RATIO
         FiCLis = FiCLis*RATIO; FCLig = FCLig*RATIO; FCLih = FCLih*RATIO
         FINig = FINig*RATIO; FiCNis = FiCNis*RATIO
      ENDIF
      AISOUR = AI1D+(AIMcsi+AIMcgi+AIMrsi+AIMrgi+AHOci+ANMci+ANCci+    &
                     ARMci)*DT
      AISINK = (ACLir-AMLir-AMLic+AiCLis+AiCLig+AiCLih+AiINig+AiCNis)*DT
      IF (AISINK.GT.AISOUR.AND.AISOUR.GE.ASMALL) THEN
         RATIO = MIN(1.,AISOUR/(AISINK+ASMALL))
         AMLir = AMLir*RATIO; AMLic = AMLic*RATIO
         ACLir = ACLir*RATIO; AiCLis = AiCLis*RATIO
         AiCLig = AiCLig*RATIO; AiCLih = AiCLih*RATIO
         AiINig = AiINig*RATIO; AiCNis = AiCNis*RATIO
      ENDIF
      IISOUR = I3M1D+(IIMcsi+IIMcgi+IIMrsi+IIMrgi+IHOci+INMci+INCci+   &
                     IRMci)*DT
      IISINK = (-IMLir-IMLic+ICLir+ICLis+ICLig+ICLih+IINig+ICNis)*DT
      IF (IISINK.GT.IISOUR.AND.IISOUR.GE.ISMALL) THEN
         RATIO = MIN(1.,IISOUR/(IISINK+ISMALL))
         IMLir = IMLir*RATIO; IMLic = IMLic*RATIO; ICLir = ICLir*RATIO
         ICLis = ICLis*RATIO; ICLig = ICLig*RATIO; ICLih = ICLih*RATIO
         IINig = IINig*RATIO; ICNis = ICNis*RATIO
      ENDIF
      QSSOUR = QS1D+(QCNis+QCLis+QRMcs)*DT
      QSSINK = (-QMLsr+QCLsr+QCLsg+QCLsh+QINsg)*DT
      IF (QSSINK.GT.QSSOUR.AND.QSSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QSSOUR/(QSSINK+QSMALL))
         QMLsr = QMLsr*RATIO; QCLsg = QCLsg*RATIO; QCLsh = QCLsh*RATIO
         QINsg = QINsg*RATIO; QCLsr = QCLsr*RATIO
      ENDIF
      NSSOUR = NS1D+NsCNis*DT
      NSSINK = (-NMLsr+NCLsr+NCLsg+NCLsh-NCLss+NINsg)*DT
      IF (NSSINK.GT.NSSOUR.AND.NSSOUR.GE.NSMALL) THEN
         RATIO = MIN(1.,NSSOUR/(NSSINK+NSMALL))
         NMLsr = NMLsr*RATIO; NCLsg = NCLsg*RATIO; NCLsh = NCLsh*RATIO
         NCLss = NCLss*RATIO; NINsg = NINsg*RATIO; NCLsr = NCLsr*RATIO
      ENDIF
      VSSOUR = VS1D+(VsCNis+VsCLis+VRMcs+VCLss)*DT
      VSSINK = (-VMLsr+VCLsr+VsCLsg+VCLsh+VsINsg)*DT
      IF (VSSINK.GT.VSSOUR.AND.VSSOUR.GE.ISMALL) THEN
         RATIO = MIN(1.,VSSOUR/(VSSINK+ISMALL))
         VMLsr = VMLsr*RATIO; VsCLsg = VsCLsg*RATIO; VCLsh = VCLsh*RATIO
         VsINsg = VsINsg*RATIO; VCLsr = VCLsr*RATIO
      ENDIF
      FSSOUR = FS1D+(FsCNis+FsCLis+FRMcs+FCLss)*DT
      FSSINK = (-FMLsr+FCLsr+FCLsg+FCLsh+FINsg)*DT
      IF (FSSINK.GT.FSSOUR.AND.FSSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,FSSOUR/(FSSINK+QSMALL))
         FMLsr = FMLsr*RATIO; FCLsg = FCLsg*RATIO; FCLsh = FCLsh*RATIO
         FINsg = FINsg*RATIO; FCLsr = FCLsr*RATIO
      ENDIF
      ASSOUR = AS1D+(AsCNis+AsCLis+ARMcs+ACLss)*DT
      ASSINK = (-AMLsr+ACLsr+AsCLsg+AsCLsh+AsINsg-ACLss1)*DT
      IF (ASSINK.GT.ASSOUR.AND.ASSOUR.GE.ASMALL) THEN
         RATIO = MIN(1.,ASSOUR/(ASSINK+ASMALL))
         AMLsr = AMLsr*RATIO; AsINsg = AsINsg*RATIO; ACLsr = ACLsr*RATIO
         AsCLsg = AsCLsg*RATIO; AsCLsh = AsCLsh*RATIO
         ACLss1 = ACLss1*RATIO
      ENDIF
      QGSOUR = QG1D+(QHOrg+QNMrg+QCLig+QCLsg+QINig+QINsg+QRMcg+QCLirg+ &
               QCLsrg+QCLgrg)*DT
      QGSINK = (-QMLgr+QCNgh+QCLgr)*DT
      IF (QGSINK.GT.QGSOUR.AND.QGSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QGSOUR/(QGSINK+QSMALL))
         QMLgr = QMLgr*RATIO; QCNgh = QCNgh*RATIO; QCLgr = QCLgr*RATIO
      ENDIF
      NGSOUR = NG1D+(NHOrg+NNMrg+NINig+NINsg+NCLirg+NCLsrg+NCLgrg)*DT
      NGSINK = (-NMLgr+NgCNgh+NCLgr)*DT
      IF (NGSINK.GT.NGSOUR.AND.NGSOUR.GE.NSMALL) THEN
         RATIO = MIN(1.,NGSOUR/(NGSINK+NSMALL))
         NMLgr = NMLgr*RATIO; NgCNgh = NgCNgh*RATIO; NCLgr = NCLgr*RATIO
      ENDIF
      VGSOUR = VG1D+(VHOrg+VNMrg+VgCLig+VgCLsg+VgINig+VgINsg+VRMcg+    &
               VCLirg+VCLsrg+VCLgrg)*DT
      VGSINK = (-VMLgr+VCNgh+VCLgr)*DT
      IF (VGSINK.GT.VGSOUR.AND.VGSOUR.GE.ISMALL) THEN
         RATIO = MIN(1.,VGSOUR/(VGSINK+ISMALL))
         VMLgr = VMLgr*RATIO; VCNgh = VCNgh*RATIO; VCLgr = VCLgr*RATIO
      ENDIF
      AGSOUR = AG1D+(AHOrg+ANMrg+AgCLig+AgCLsg+AgINig+AgINsg+ARMcg+    &
               ACLirg+ACLsrg+ACLgrg)*DT
      AGSINK = (-AMLgr+AgCNgh+ACLgr)*DT
      IF (AGSINK.GT.AGSOUR.AND.AGSOUR.GE.ASMALL) THEN
         RATIO = MIN(1.,AGSOUR/(AGSINK+ASMALL))
         AMLgr = AMLgr*RATIO; AgCNgh = AgCNgh*RATIO; ACLgr = ACLgr*RATIO
      ENDIF
      QHSOUR = QH1D+(QCNgh+QCLih+QCLsh+QHdrm)*DT
      QHSINK = (-QMLhr-QHwml)*DT
      IF (QHSINK.GT.QHSOUR.AND.QHSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QHSOUR/(QHSINK+QSMALL))
         QMLhr = QMLhr*RATIO; QHwml = QHwml*RATIO
      ENDIF
      NHSOUR = NH1D+(NhCNgh)*DT
      NHSINK = (-NMLhr)*DT
      IF (NHSINK.GT.NHSOUR.AND.NHSOUR.GE.NSMALL) THEN
         RATIO = MIN(1.,NHSOUR/(NHSINK+NSMALL))
         NMLhr = NMLhr*RATIO
      ENDIF
      AHSOUR = AH1D+(AhCNgh+AhCLih+AhCLsh+AHdrm)*DT
      AHSINK = (-AMLhr-AHwml)*DT
      IF (AHSINK.GT.AHSOUR.AND.AHSOUR.GE.ASMALL) THEN
         RATIO = MIN(1.,AHSOUR/(AHSINK+ASMALL))
         AMLhr = AMLhr*RATIO; AHwml = AHwml*RATIO
      ENDIF

      QFZci = QHOci+QNMci+QNCci
      NFZci = NHOci+NNMci+NNCci
      VFZci = VHOci+VNMci+VNCci
      FFZci = FHOci+FNMci+FNCci
      AFZci = AHOci+ANMci+ANCci
      IFZci = IHOci+INMci+INCci
      QFZrg = QHOrg+QNMrg
      NFZrg = NHOrg+NNMrg
      VFZrg = VHOrg+VNMrg
      AFZrg = AHOrg+ANMrg
      QIMii = QIMcsi+QIMcgi+QIMrsi+QIMrgi
      NIMii = NIMcsi+NIMcgi+NIMrsi+NIMrgi
      VIMii = VIMcsi+VIMcgi+VIMrsi+VIMrgi
      AIMii = AIMcsi+AIMcgi+AIMrsi+AIMrgi
      FIMii = FIMcsi+FIMcgi+FIMrsi+FIMrgi
      IIMii = IIMcsi+IIMcgi+IIMrsi+IIMrgi
      GQCTR = (QRMcs+QRMcg+QRMch+QCLir+QCLcr+QCLis+QCLig+QCLih+QCNcr+  &
              QCNis+QINig-QBKrc-QIMcsi-QIMcgi-QMLir)*DT
!----------------- UPDATE VARIABLES ------------------------------------
      QC1D = MAX(0.,QC1D+(QBKrc-QCLcr-QCNcr-QFZci-QMLic-QRMci-QRMcs-   &
             QRMcg-QRMch-QIMcsi-QIMcgi)*DT)
      QR1D = MAX(0.,QR1D+(QCLcr+QCNcr-QBKrc-QFZrg-QIMrsi-QIMrgi-QMLir- &
             QMLsr-QMLgr-QMLhr-QRMri-QRMrs-QRMrg-QRMrh+QHwsh-QHwml)*DT)
      QI1D = MAX(0.,QI1D+(QIMii-QCNis-QCLis-QCLig-QCLih-QINig+QFZci+   &
             QMLir+QMLic+QRMci-QCLir)*DT)
      QS1D = MAX(0.,QS1D+(QCNis+QCLis-QCLsg-QCLsh-QINsg+QMLsr+QRMcs-   &
             QCLsr)*DT)
      QG1D = MAX(0.,QG1D+(QFZrg+QCLig+QCLsg+QINig+QINsg+QMLgr+QRMcg-   &
             QCNgh-QCLgr+QCLirg+QCLsrg+QCLgrg)*DT)
      QH1D = MAX(0.,QH1D+(QMLhr+QCNgh+QHdrm+QCLih+QCLsh+QHwml)*DT)
      NC1D = MAX(0.,NC1D+(NBKrc-NCLcr-NCLcc-NCNcr-NFZci-NRMci-NRMcs-   &
             NRMcg-NRMch-NMLic-NIMcsi-NIMcgi)*DT)
      NR1D = MAX(0.,NR1D+(NCNcr+NBKrr-NBKrc-NCLrr-NIMrsi-NIMrgi-NFZrg- &
             NMLir-NMLsr-NMLgr-NMLhr-NRMri-NRMrs-NRMrg-NRMrh+NHwsh)*DT)
      NI1D = MAX(0.,NI1D+(NIMii-NiCNis-NCLis-NCLig-NCLih-NINig+NFZci+  &
             NMLir+NMLic-NCLir)*DT)
      NS1D = MAX(0.,NS1D+(NsCNis-NCLsg-NCLsh+NCLss-NINsg+NMLsr-NCLsr)* &
             DT)
      NG1D = MAX(0.,NG1D+(NFZrg+NINig+NINsg+NMLgr-NgCNgh-NCLgr+NCLirg+ &
             NCLsrg+NCLgrg)*DT)
      NH1D = MAX(0.,NH1D+(NMLhr+NhCNgh)*DT)
      IF (ICE_RHOI.EQ.0.OR.ICE_RHOI.EQ.2) THEN
         VI1D = 0.
      ELSEIF (ICE_RHOI.EQ.1) THEN
         VI1D = MAX(0.,VI1D+(VIMii-ViCNis-ViCLis-ViCLig-VCLih-ViINig+  &
                VFZci+VMLir+VMLic+VRMci-VCLir)*DT)
      ENDIF
      IF (ICE_RHOS.EQ.0.OR.ICE_RHOS.EQ.2) THEN
         VS1D = 0.
      ELSEIF (ICE_RHOS.EQ.1) THEN
         VS1D = MAX(0.,VS1D+(VsCNis+VsCLis-VsCLsg-VCLsh-VsINsg+VMLsr+  &
                VRMcs-VCLsr+VCLss)*DT)
      ENDIF
      IF (ICE_RHOG.EQ.0) THEN
         VG1D = 0.
      ELSEIF (ICE_RHOG.EQ.1.OR.ICE_RHOG.EQ.2) THEN
         VG1D = MAX(0.,VG1D+(VFZrg+VgCLig+VgCLsg+VgINig+VgINsg+VMLgr+  &
                VRMcg-VCNgh-VCLgr+VCLirg+VCLsrg+VCLgrg)*DT)
      ENDIF
      FI1D = MAX(0.,FI1D+(FIMii-FiCNis-FiCLis-FCLig-FCLih-FINig+FFZci+ &
             FMLir+FMLic+FRMci-FCLir)*DT)
      IF (AGG_SHAPE.EQ.0) THEN
         FS1D = 0.
      ELSEIF (AGG_SHAPE.EQ.1) THEN
         FS1D = MAX(0.,FS1D+(FsCNis+FsCLis-FCLsg-FCLsh-FINsg+FMLsr+    &
                FRMcs-FCLsr+FCLss)*DT)
      ENDIF
      I3M1D = MAX(0.,I3M1D+(IIMii-ICNis-ICLis-ICLig-ICLih-IINig+IFZci+ &
              IMLir+IMLic+IRMci-ICLir)*DT)
      IF (AFAI_3M.EQ.0.OR.AFAI_3M.EQ.2) THEN
         AI1D = 0.
      ELSEIF (AFAI_3M.EQ.1) THEN
         AI1D = MAX(0.,AI1D+(AIMii-AiCNis-AiCLis-AiCLig-AiCLih-AiINig+ &
                AFZci+AMLir+AMLic+ARMci-ACLir)*DT)
      ENDIF
      IF (AFAS_3M.EQ.0.OR.AFAS_3M.EQ.2) THEN
         AS1D = 0.
      ELSEIF (AFAS_3M.EQ.1) THEN
         AS1D = MAX(0.,AS1D+(AsCNis+AsCLis-AsCLsg-AsCLsh-AsINsg+AMLsr+ &
                ARMcs-ACLsr+ACLss+ACLss1)*DT)
      ENDIF
      IF (AFAG_3M.EQ.0.OR.AFAG_3M.EQ.2) THEN
         AG1D = 0.
      ELSEIF (AFAG_3M.EQ.1) THEN
         AG1D = MAX(0.,AG1D+(AFZrg+AgCLig+AgCLsg+AgINig+AgINsg+AMLgr+  &
                ARMcg-AgCNgh-ACLgr+ACLirg+ACLsrg+ACLgrg)*DT)
      ENDIF
      IF (AFAH_3M.EQ.0.OR.AFAH_3M.EQ.2) THEN
         AH1D = 0.
      ELSEIF (AFAH_3M.EQ.1) THEN
         AH1D = MAX(0.,AH1D+(AMLhr+AhCNgh+AHdrm+AhCLih+AhCLsh+AHwml)*DT)
      ENDIF
!      QNIN  = MAX(0.,QNIN-(NNCci+NNMci)*DT)
      TK1D = TK1D+((QFZci+QFZrg+QMLir+QMLic+QMLsr+QMLgr+QMLhr+QRMci+   &
             QRMcs+QRMcg+QRMri+QRMrs+QRMrg+QIMii+QHdrm+QHwml)*XXLF)/   &
             CPM*DT

      IF (QC1D.GE.QSMALL.AND.NC1D.GE.NSMALL) THEN
         MVDC = (QC1D*iAMW/NC1D)**THRD
         IF (MVDC.GT.DCR) THEN
            QR1D = QR1D+QC1D
            NR1D = NR1D+NC1D
            QC1D = 0.; NC1D = 0.
         ENDIF
      ENDIF
      IF (QR1D.GE.QSMALL.AND.NR1D.GE.NSMALL) THEN
         MVDR = (QR1D*iAMW/NR1D)**THRD
         IF (MVDR.LT.DCR) THEN
            QC1D = QC1D+QR1D
            NC1D = NC1D+NR1D
            QR1D = 0.; NR1D = 0.
         ENDIF
      ENDIF
      IF (QH1D.GE.QSMALL.AND.NH1D.GE.NSMALL) THEN
         MVDH = (QH1D*iAMH/NH1D)**THRD
         IF (MVDH.LT.DHMIN) THEN
            QG1D = QG1D+QH1D
            NG1D = NG1D+NH1D
            VG1D = VG1D+QH1D/RHOG0
            QH1D = 0.; NH1D = 0.
            IF (AH1D.GE.ASMALL.AND.AFAG_3M.EQ.1) THEN
               AG1D = AG1D+AH1D
               AH1D = 0.
            ENDIF
         ENDIF
      ENDIF
      IF (GQCTR.GT.0.) THEN
         TQCI = QC1D+QI1D 
         IF (TQCI.GT.RLIMIT*2.) THEN
            GQCTR = MIN(1.,GQCTR/TQCI)
         ELSE
            GQCTR = 0.
         ENDIF
      ELSE
         TQRSG = QR1D+QS1D+QG1D+QH1D
         IF (TQRSG.GT.RLIMIT*3.) THEN 
            GQCTR = MAX(-1.,GQCTR/TQRSG) 
         ELSE 
            GQCTR = 0.
         ENDIF 
      ENDIF

      END SUBROUTINE LARGE_DT
!======================================================================
      END MODULE MODULE_MP_NTU
!======================================================================
