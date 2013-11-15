MODULE module_sf_noahmpdrv

!-------------------------------
  USE module_sf_noahmplsm
  USE module_sf_urban
  USE module_sf_noahdrv, ONLY : SOIL_VEG_GEN_PARM
  USE module_sf_noah_seaice
  USE module_sf_noahmp_glacier
  USE MODULE_RA_GFDLETA, ONLY: CAL_MON_DAY
#ifdef WRF_CHEM
  USE module_data_gocart_dust
#endif
!-------------------------------

!
CONTAINS
!
  SUBROUTINE noahmplsm(ITIMESTEP,        YR,   JULIAN,   COSZIN,   XLATIN,  & ! IN : Time/Space-related
                  DZ8W,       DT,       DZS,    NSOIL,       DX,            & ! IN : Model configuration 
	        IVGTYP,   ISLTYP,    VEGFRA,   VEGMAX,      TMN,            & ! IN : Vegetation/Soil characteristics
		 XLAND,     XICE,XICE_THRES,    ISICE,  ISURBAN,                      & ! IN : Vegetation/Soil characteristics
                 IDVEG, IOPT_CRS,  IOPT_BTR, IOPT_RUN, IOPT_SFC, IOPT_FRZ,  & ! IN : User options
              IOPT_INF, IOPT_RAD,  IOPT_ALB, IOPT_SNF,IOPT_TBOT, IOPT_STC,  & ! IN : User options
               IZ0TLND,                                                     & ! IN : User options
                   T3D,     QV3D,     U_PHY,    V_PHY,   SWDOWN,      GLW,  & ! IN : Forcing
                 P8W3D,   RAINBL,                                           & ! IN : Forcing
                   TSK,      HFX,      QFX,        LH,   GRDFLX,    SMSTAV, & ! IN/OUT LSM eqv
                SMSTOT,SFCRUNOFF, UDRUNOFF,    ALBEDO,    SNOWC,     SMOIS, & ! IN/OUT LSM eqv
		  SH2O,     TSLB,     SNOW,     SNOWH,   CANWAT,    ACSNOM, & ! IN/OUT LSM eqv
		ACSNOW,    EMISS,     QSFC,                                 & ! IN/OUT LSM eqv
               ISNOWXY,     TVXY,     TGXY,  CANICEXY, CANLIQXY,     EAHXY, & ! IN/OUT Noah MP only
	         TAHXY,     CMXY,     CHXY,    FWETXY, SNEQVOXY,  ALBOLDXY, & ! IN/OUT Noah MP only
               QSNOWXY, WSLAKEXY,    ZWTXY,      WAXY,     WTXY,    TSNOXY, & ! IN/OUT Noah MP only
	       ZSNSOXY,  SNICEXY,  SNLIQXY,  LFMASSXY, RTMASSXY,  STMASSXY, & ! IN/OUT Noah MP only
	        WOODXY, STBLCPXY, FASTCPXY,    XLAIXY,   XSAIXY,   TAUSSXY, & ! IN/OUT Noah MP only
	        T2MVXY,   T2MBXY,    Q2MVXY,   Q2MBXY,                      & ! OUT Noah MP only
	        TRADXY,    NEEXY,    GPPXY,     NPPXY,   FVEGXY,   RUNSFXY, & ! OUT Noah MP only
	       RUNSBXY,   ECANXY,   EDIRXY,   ETRANXY,    FSAXY,    FIRAXY, & ! OUT Noah MP only
	        APARXY,    PSNXY,    SAVXY,     SAGXY,  RSSUNXY,   RSSHAXY, & ! OUT Noah MP only
		BGAPXY,   WGAPXY,    TGVXY,     TGBXY,    CHVXY,     CHBXY, & ! OUT Noah MP only
		 SHGXY,    SHCXY,    SHBXY,     EVGXY,    EVBXY,     GHVXY, & ! OUT Noah MP only
		 GHBXY,    IRGXY,    IRCXY,     IRBXY,     TRXY,     EVCXY, & ! OUT Noah MP only
              CHLEAFXY,   CHUCXY,   CHV2XY,    CHB2XY,                      & ! OUT Noah MP only
#ifdef WRF_HYDRO
               sfcheadrt,INFXSRT,soldrain,                                  &
#endif
               ids,ide,  jds,jde,  kds,kde,                    &
               ims,ime,  jms,jme,  kms,kme,                    &
               its,ite,  jts,jte,  kts,kte                     )
!----------------------------------------------------------------
    IMPLICIT NONE
!----------------------------------------------------------------

! IN only

    INTEGER,                                         INTENT(IN   ) ::  ITIMESTEP ! timestep number
    INTEGER,                                         INTENT(IN   ) ::  YR        ! 4-digit year
    REAL,                                            INTENT(IN   ) ::  JULIAN    ! Julian day
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  COSZIN    ! cosine zenith angle
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XLATIN    ! latitude [rad]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  DZ8W      ! thickness of atmo layers [m]
    REAL,                                            INTENT(IN   ) ::  DT        ! timestep [s]
    REAL,    DIMENSION(1:nsoil),                     INTENT(IN   ) ::  DZS       ! thickness of soil layers [m]
    INTEGER,                                         INTENT(IN   ) ::  NSOIL     ! number of soil layers
    REAL,                                            INTENT(IN   ) ::  DX        ! horizontal grid spacing [m]
    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  IVGTYP    ! vegetation type
    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  ISLTYP    ! soil type
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  VEGFRA    ! vegetation fraction []
    REAL,    DIMENSION( ims:ime ,         jms:jme ), INTENT(IN   ) ::  VEGMAX    ! annual max vegetation fraction []
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  TMN       ! deep soil temperature [K]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XLAND     ! =2 ocean; =1 land/seaice
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XICE      ! fraction of grid that is seaice
    REAL,                                            INTENT(IN   ) ::  XICE_THRES! fraction of grid determining seaice
    INTEGER,                                         INTENT(IN   ) ::  ISICE     ! land cover category for ice
    INTEGER,                                         INTENT(IN   ) ::  ISURBAN   ! land cover category for urban
    INTEGER,                                         INTENT(IN   ) ::  IDVEG     ! dynamic vegetation (1 -> off ; 2 -> on) with opt_crs = 1      
    INTEGER,                                         INTENT(IN   ) ::  IOPT_CRS  ! canopy stomatal resistance (1-> Ball-Berry; 2->Jarvis)
    INTEGER,                                         INTENT(IN   ) ::  IOPT_BTR  ! soil moisture factor for stomatal resistance (1-> Noah; 2-> CLM; 3-> SSiB)
    INTEGER,                                         INTENT(IN   ) ::  IOPT_RUN  ! runoff and groundwater (1->SIMGM; 2->SIMTOP; 3->Schaake96; 4->BATS)
    INTEGER,                                         INTENT(IN   ) ::  IOPT_SFC  ! surface layer drag coeff (CH & CM) (1->M-O; 2->Chen97)
    INTEGER,                                         INTENT(IN   ) ::  IOPT_FRZ  ! supercooled liquid water (1-> NY06; 2->Koren99)
    INTEGER,                                         INTENT(IN   ) ::  IOPT_INF  ! frozen soil permeability (1-> NY06; 2->Koren99)
    INTEGER,                                         INTENT(IN   ) ::  IOPT_RAD  ! radiation transfer (1->gap=F(3D,cosz); 2->gap=0; 3->gap=1-Fveg)
    INTEGER,                                         INTENT(IN   ) ::  IOPT_ALB  ! snow surface albedo (1->BATS; 2->CLASS)
    INTEGER,                                         INTENT(IN   ) ::  IOPT_SNF  ! rainfall & snowfall (1-Jordan91; 2->BATS; 3->Noah)
    INTEGER,                                         INTENT(IN   ) ::  IOPT_TBOT ! lower boundary of soil temperature (1->zero-flux; 2->Noah)
    INTEGER,                                         INTENT(IN   ) ::  IOPT_STC  ! snow/soil temperature time scheme
    INTEGER,                                         INTENT(IN   ) ::  IZ0TLND   ! option of Chen adjustment of Czil (not used)
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  T3D       ! 3D atmospheric temperature valid at mid-levels [K]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  QV3D      ! 3D water vapor mixing ratio [kg/kg_dry]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  U_PHY     ! 3D U wind component [m/s]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  V_PHY     ! 3D V wind component [m/s]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SWDOWN    ! solar down at surface [W m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  GLW       ! longwave down at surface [W m-2]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  P8W3D     ! 3D pressure, valid at interface [Pa]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  RAINBL    ! precipitation entering land model [mm]
#ifdef WRF_HYDRO
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  sfcheadrt,INFXSRT,soldrain   ! for WRF-Hydro
#endif

! INOUT (with generic LSM equivalent)

    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TSK       ! surface radiative temperature [K]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  HFX       ! sensible heat flux [W m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QFX       ! latent heat flux [kg s-1 m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  LH        ! latent heat flux [W m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  GRDFLX    ! ground/snow heat flux [W m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SMSTAV    ! soil moisture avail. [not used]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SMSTOT    ! total soil water [mm][not used]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SFCRUNOFF ! accumulated surface runoff [m]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  UDRUNOFF  ! accumulated sub-surface runoff [m]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ALBEDO    ! total grid albedo []
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SNOWC     ! snow cover fraction []
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  SMOIS     ! volumetric soil moisture [m3/m3]
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  SH2O      ! volumetric liquid soil moisture [m3/m3]
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  TSLB      ! soil temperature [K]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SNOW      ! snow water equivalent [mm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SNOWH     ! physical snow depth [m]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CANWAT    ! total canopy water + ice [mm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ACSNOM    ! accumulated snow melt leaving pack
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ACSNOW    ! accumulated snow on grid
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  EMISS     ! surface bulk emissivity
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QSFC      ! bulk surface specific humidity

! INOUT (with no Noah LSM equivalent)

    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ISNOWXY   ! actual no. of snow layers
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TVXY      ! vegetation leaf temperature
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TGXY      ! bulk ground surface temperature
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CANICEXY  ! canopy-intercepted ice (mm)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CANLIQXY  ! canopy-intercepted liquid water (mm)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  EAHXY     ! canopy air vapor pressure (pa)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TAHXY     ! canopy air temperature (k)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CMXY      ! bulk momentum drag coefficient
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CHXY      ! bulk sensible heat exchange coefficient
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  FWETXY    ! wetted or snowed fraction of the canopy (-)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SNEQVOXY  ! snow mass at last time step(mm h2o)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ALBOLDXY  ! snow albedo at last time step (-)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QSNOWXY   ! snowfall on the ground [mm/s]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  WSLAKEXY  ! lake water storage [mm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ZWTXY     ! water table depth [m]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  WAXY      ! water in the "aquifer" [mm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  WTXY      ! groundwater storage [mm]
    REAL,    DIMENSION( ims:ime,-2:0,     jms:jme ), INTENT(INOUT) ::  TSNOXY    ! snow temperature [K]
    REAL,    DIMENSION( ims:ime,-2:NSOIL, jms:jme ), INTENT(INOUT) ::  ZSNSOXY   ! snow layer depth [m]
    REAL,    DIMENSION( ims:ime,-2:0,     jms:jme ), INTENT(INOUT) ::  SNICEXY   ! snow layer ice [mm]
    REAL,    DIMENSION( ims:ime,-2:0,     jms:jme ), INTENT(INOUT) ::  SNLIQXY   ! snow layer liquid water [mm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  LFMASSXY  ! leaf mass [g/m2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  RTMASSXY  ! mass of fine roots [g/m2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  STMASSXY  ! stem mass [g/m2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  WOODXY    ! mass of wood (incl. woody roots) [g/m2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  STBLCPXY  ! stable carbon in deep soil [g/m2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  FASTCPXY  ! short-lived carbon, shallow soil [g/m2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  XLAIXY    ! leaf area index
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  XSAIXY    ! stem area index
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TAUSSXY   ! snow age factor

! OUT (with no Noah LSM equivalent)

    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  T2MVXY    ! 2m temperature of vegetation part
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  T2MBXY    ! 2m temperature of bare ground part
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  Q2MVXY    ! 2m mixing ratio of vegetation part
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  Q2MBXY    ! 2m mixing ratio of bare ground part
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  TRADXY    ! surface radiative temperature (k)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  NEEXY     ! net ecosys exchange (g/m2/s CO2)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  GPPXY     ! gross primary assimilation [g/m2/s C]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  NPPXY     ! net primary productivity [g/m2/s C]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  FVEGXY    ! Noah-MP vegetation fraction [-]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  RUNSFXY   ! surface runoff [mm/s]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  RUNSBXY   ! subsurface runoff [mm/s]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  ECANXY    ! evaporation of intercepted water (mm/s)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  EDIRXY    ! soil surface evaporation rate (mm/s]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  ETRANXY   ! transpiration rate (mm/s)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  FSAXY     ! total absorbed solar radiation (w/m2)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  FIRAXY    ! total net longwave rad (w/m2) [+ to atm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  APARXY    ! photosyn active energy by canopy (w/m2)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  PSNXY     ! total photosynthesis (umol co2/m2/s) [+]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SAVXY     ! solar rad absorbed by veg. (w/m2)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SAGXY     ! solar rad absorbed by ground (w/m2)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  RSSUNXY   ! sunlit leaf stomatal resistance (s/m)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  RSSHAXY   ! shaded leaf stomatal resistance (s/m)
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  BGAPXY    ! between gap fraction
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  WGAPXY    ! within gap fraction
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  TGVXY     ! under canopy ground temperature[K]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  TGBXY     ! bare ground temperature [K]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHVXY     ! sensible heat exchange coefficient vegetated
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHBXY     ! sensible heat exchange coefficient bare-ground
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SHGXY     ! veg ground sen. heat [w/m2]   [+ to atm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SHCXY     ! canopy sen. heat [w/m2]   [+ to atm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SHBXY     ! bare sensible heat [w/m2]     [+ to atm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  EVGXY     ! veg ground evap. heat [w/m2]  [+ to atm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  EVBXY     ! bare soil evaporation [w/m2]  [+ to atm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  GHVXY     ! veg ground heat flux [w/m2]  [+ to soil]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  GHBXY     ! bare ground heat flux [w/m2] [+ to soil]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  IRGXY     ! veg ground net LW rad. [w/m2] [+ to atm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  IRCXY     ! canopy net LW rad. [w/m2] [+ to atm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  IRBXY     ! bare net longwave rad. [w/m2] [+ to atm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  TRXY      ! transpiration [w/m2]  [+ to atm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  EVCXY     ! canopy evaporation heat [w/m2]  [+ to atm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHLEAFXY  ! leaf exchange coefficient 
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHUCXY    ! under canopy exchange coefficient 
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHV2XY    ! veg 2m exchange coefficient 
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHB2XY    ! bare 2m exchange coefficient 
    INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &  ! d -> domain
         &                           ims,ime, jms,jme, kms,kme,  &  ! m -> memory
         &                           its,ite, jts,jte, kts,kte      ! t -> tile


! 1D equivalent of 2D/3D fields

! IN only

    REAL                                :: COSZ         ! cosine zenith angle
    REAL                                :: LAT          ! latitude [rad]
    REAL                                :: Z_ML         ! model height [m]
    INTEGER                             :: VEGTYP       ! vegetation type
    INTEGER                             :: SOILTYP      ! soil type
    REAL                                :: FVEG         ! vegetation fraction [-]
    REAL                                :: FVGMAX       ! annual max vegetation fraction []
    REAL                                :: TBOT         ! deep soil temperature [K]
    REAL                                :: T_ML         ! temperature valid at mid-levels [K]
    REAL                                :: Q_ML         ! water vapor mixing ratio [kg/kg_dry]
    REAL                                :: U_ML         ! U wind component [m/s]
    REAL                                :: V_ML         ! V wind component [m/s]
    REAL                                :: SWDN         ! solar down at surface [W m-2]
    REAL                                :: LWDN         ! longwave down at surface [W m-2]
    REAL                                :: P_ML         ! pressure, valid at interface [Pa]
    REAL                                :: PSFC         ! surface pressure [Pa]
    REAL                                :: PRCP         ! precipitation entering land model [mm]

! INOUT (with generic LSM equivalent)

    REAL                                :: FSH          ! total sensible heat (w/m2) [+ to atm]
    REAL                                :: SSOIL        ! soil heat heat (w/m2) 
    REAL                                :: SALB         ! surface albedo (-)
    REAL                                :: FSNO         ! snow cover fraction (-)
    REAL,   DIMENSION( 1:NSOIL)         :: SMC          ! vol. soil moisture (m3/m3)
    REAL,   DIMENSION( 1:NSOIL)         :: SMH2O        ! vol. soil liquid water (m3/m3)
    REAL,   DIMENSION(-2:NSOIL)         :: STC          ! snow/soil tmperatures
    REAL                                :: SWE          ! snow water equivalent (mm)
    REAL                                :: SNDPTH       ! snow depth (m)
    REAL                                :: EMISSI       ! net surface emissivity
    REAL                                :: QSFC1D       ! bulk surface specific humidity

! INOUT (with no Noah LSM equivalent)

    INTEGER                             :: ISNOW        ! actual no. of snow layers
    REAL                                :: TV           ! vegetation canopy temperature
    REAL                                :: TG           ! ground surface temperature
    REAL                                :: CANICE       ! canopy-intercepted ice (mm)
    REAL                                :: CANLIQ       ! canopy-intercepted liquid water (mm)
    REAL                                :: EAH          ! canopy air vapor pressure (pa)
    REAL                                :: TAH          ! canopy air temperature (k)
    REAL                                :: CM           ! momentum drag coefficient
    REAL                                :: CH           ! sensible heat exchange coefficient
    REAL                                :: FWET         ! wetted or snowed fraction of the canopy (-)
    REAL                                :: SNEQVO       ! snow mass at last time step(mm h2o)
    REAL                                :: ALBOLD       ! snow albedo at last time step (-)
    REAL                                :: QSNOW        ! snowfall on the ground [mm/s]
    REAL                                :: WSLAKE       ! lake water storage [mm]
    REAL                                :: ZWT          ! water table depth [m]
    REAL                                :: WA           ! water in the "aquifer" [mm]
    REAL                                :: WT           ! groundwater storage [mm]
    REAL, DIMENSION(-2:NSOIL)           :: ZSNSO        ! snow layer depth [m]
    REAL, DIMENSION(-2:              0) :: SNICE        ! snow layer ice [mm]
    REAL, DIMENSION(-2:              0) :: SNLIQ        ! snow layer liquid water [mm]
    REAL                                :: LFMASS       ! leaf mass [g/m2]
    REAL                                :: RTMASS       ! mass of fine roots [g/m2]
    REAL                                :: STMASS       ! stem mass [g/m2]
    REAL                                :: WOOD         ! mass of wood (incl. woody roots) [g/m2]
    REAL                                :: STBLCP       ! stable carbon in deep soil [g/m2]
    REAL                                :: FASTCP       ! short-lived carbon, shallow soil [g/m2]
    REAL                                :: PLAI         ! leaf area index
    REAL                                :: PSAI         ! stem area index
    REAL                                :: TAUSS        ! non-dimensional snow age

! OUT (with no Noah LSM equivalent)

    REAL                                :: T2MV         ! 2m temperature of vegetation part
    REAL                                :: T2MB         ! 2m temperature of bare ground part
    REAL                                :: Q2MV         ! 2m mixing ratio of vegetation part
    REAL                                :: Q2MB         ! 2m mixing ratio of bare ground part
    REAL                                :: TRAD         ! surface radiative temperature (k)
    REAL                                :: NEE          ! net ecosys exchange (g/m2/s CO2)
    REAL                                :: GPP          ! gross primary assimilation [g/m2/s C]
    REAL                                :: NPP          ! net primary productivity [g/m2/s C]
    REAL                                :: FVEGMP       ! greenness vegetation fraction [-]
    REAL                                :: RUNSF        ! surface runoff [mm/s]
    REAL                                :: RUNSB        ! subsurface runoff [mm/s]
    REAL                                :: ECAN         ! evaporation of intercepted water (mm/s)
    REAL                                :: ETRAN        ! transpiration rate (mm/s)
    REAL                                :: ESOIL        ! soil surface evaporation rate (mm/s]
    REAL                                :: FSA          ! total absorbed solar radiation (w/m2)
    REAL                                :: FIRA         ! total net longwave rad (w/m2) [+ to atm]
    REAL                                :: APAR         ! photosyn active energy by canopy (w/m2)
    REAL                                :: PSN          ! total photosynthesis (umol co2/m2/s) [+]
    REAL                                :: SAV          ! solar rad absorbed by veg. (w/m2)
    REAL                                :: SAG          ! solar rad absorbed by ground (w/m2)
    REAL                                :: RSSUN        ! sunlit leaf stomatal resistance (s/m)
    REAL                                :: RSSHA        ! shaded leaf stomatal resistance (s/m)
    REAL                                :: BGAP         ! between gap fraction
    REAL                                :: WGAP         ! within gap fraction
    REAL                                :: TGV          ! under canopy ground temperature[K]
    REAL                                :: TGB          ! bare ground temperature [K]
    REAL                                :: CHV          ! sensible heat exchange coefficient vegetated
    REAL                                :: CHB          ! sensible heat exchange coefficient bare-ground
    REAL                                :: IRC          ! canopy net LW rad. [w/m2] [+ to atm]
    REAL                                :: IRG          ! veg ground net LW rad. [w/m2] [+ to atm]
    REAL                                :: SHC          ! canopy sen. heat [w/m2]   [+ to atm]
    REAL                                :: SHG          ! veg ground sen. heat [w/m2]   [+ to atm]
    REAL                                :: EVG          ! veg ground evap. heat [w/m2]  [+ to atm]
    REAL                                :: GHV          ! veg ground heat flux [w/m2]  [+ to soil]
    REAL                                :: IRB          ! bare net longwave rad. [w/m2] [+ to atm]
    REAL                                :: SHB          ! bare sensible heat [w/m2]     [+ to atm]
    REAL                                :: EVB          ! bare evaporation heat [w/m2]  [+ to atm]
    REAL                                :: GHB          ! bare ground heat flux [w/m2] [+ to soil]
    REAL                                :: TR           ! transpiration [w/m2]  [+ to atm]
    REAL                                :: EVC          ! canopy evaporation heat [w/m2]  [+ to atm]
    REAL                                :: CHLEAF       ! leaf exchange coefficient 
    REAL                                :: CHUC         ! under canopy exchange coefficient 
    REAL                                :: CHV2         ! veg 2m exchange coefficient 
    REAL                                :: CHB2         ! bare 2m exchange coefficient 

! Intermediate terms

    REAL                                :: FPICE        ! snow fraction of precip
    REAL                                :: FCEV         ! canopy evaporation heat (w/m2) [+ to atm]
    REAL                                :: FGEV         ! ground evaporation heat (w/m2) [+ to atm]
    REAL                                :: FCTR         ! transpiration heat flux (w/m2) [+ to atm]
    REAL                                :: QSNBOT       ! snowmelt out bottom of pack [mm/s]
    REAL                                :: PONDING      ! snowmelt with no pack [mm]
    REAL                                :: PONDING1     ! snowmelt with no pack [mm]
    REAL                                :: PONDING2     ! snowmelt with no pack [mm]

! Local terms

    REAL                                :: FSR          ! total reflected solar radiation (w/m2)
    REAL, DIMENSION(-2:0)               :: FICEOLD      ! snow layer ice fraction []
    REAL                                :: CO2PP        ! CO2 partial pressure [Pa]
    REAL                                :: O2PP         ! O2 partial pressure [Pa]
    REAL, DIMENSION(1:NSOIL)            :: ZSOIL        ! depth to soil interfaces [m]
    REAL                                :: FOLN         ! nitrogen saturation [%]

    REAL                                :: QC           ! cloud specific humidity for MYJ [not used]
    REAL                                :: PBLH         ! PBL height for MYJ [not used]
    REAL                                :: DZ8W1D       ! model level heights for MYJ [not used]

    INTEGER                             :: I
    INTEGER                             :: J
    INTEGER                             :: K
    INTEGER                             :: ICE
    INTEGER                             :: SLOPETYP
    LOGICAL                             :: IPRINT

    INTEGER                             :: ISC          ! soil color index
    INTEGER                             :: IST          ! surface type 1-soil; 2-lake
    INTEGER                             :: YEARLEN

    INTEGER, PARAMETER                  :: NSNOW = 3    ! number of snow layers fixed to 3
    REAL, PARAMETER                     :: CO2 = 395.e-06
    REAL, PARAMETER                     :: O2 = 0.209
    REAL, PARAMETER                     :: undefined_value = -1.E36


! ----------------------------------------------------------------------
! ----------------------------------------------------------------------

    CALL NOAHMP_OPTIONS(IDVEG  ,IOPT_CRS  ,IOPT_BTR  ,IOPT_RUN  ,IOPT_SFC  ,IOPT_FRZ , &
                     IOPT_INF  ,IOPT_RAD  ,IOPT_ALB  ,IOPT_SNF  ,IOPT_TBOT, IOPT_STC )

    IPRINT    =  .false.                     ! debug printout

    YEARLEN = 365                            ! find length of year for phenology (also S Hemisphere)
    if (mod(YR,4) == 0) then
       YEARLEN = 366
       if (mod(YR,100) == 0) then
          YEARLEN = 365
          if (mod(YR,400) == 0) then
             YEARLEN = 366
          endif
       endif
    endif

    ZSOIL(1) = -DZS(1)                    ! depth to soil interfaces (<0) [m]
    DO K = 2, NSOIL
       ZSOIL(K) = -DZS(K) + ZSOIL(K-1)
    END DO

    JLOOP : DO J=jts,jte

       IF(ITIMESTEP == 1)THEN
          DO I=its,ite
             IF((XLAND(I,J)-1.5) >= 0.) THEN    ! Open water case
                IF(XICE(I,J) == 1. .AND. IPRINT) PRINT *,' sea-ice at water point, I=',I,'J=',J
                SMSTAV(I,J) = 1.0
                SMSTOT(I,J) = 1.0
                DO K = 1, NSOIL
                   SMOIS(I,K,J) = 1.0
                    TSLB(I,K,J) = 273.16
                ENDDO
             ELSE
                IF(XICE(I,J) == 1.) THEN        ! Sea-ice case
                   SMSTAV(I,J) = 1.0
                   SMSTOT(I,J) = 1.0
                   DO K = 1, NSOIL
                      SMOIS(I,K,J) = 1.0
                   ENDDO
                ENDIF
             ENDIF
          ENDDO
       ENDIF                                                               ! end of initialization over ocean


!-----------------------------------------------------------------------
   ILOOP : DO I = its, ite

    IF (XICE(I,J) >= XICE_THRES) THEN
       ICE = 1                            ! Sea-ice point
    ELSE IF ( IVGTYP(I,J) == ISICE ) THEN
       ICE = -1                           ! Land-ice point
    ELSE
       ICE=0                              ! Neither sea ice or land ice.
    ENDIF
    
    IF((XLAND(I,J)-1.5) >= 0.) CYCLE ILOOP   ! Open water case

    IF ( ICE == 1) THEN

       SH2O  (i,1:NSOIL,j) = 1.0
       XLAIXY(i,j)         = 0.01

       CYCLE ILOOP ! Skip any processing at sea-ice points

    ELSE

!     2D to 1D       

! IN only

       COSZ   = COSZIN  (I,J)                         ! cos zenith angle []
       LAT    = XLATIN  (I,J)                         ! latitude [rad]
       Z_ML   = 0.5*DZ8W(I,1,J)                       ! DZ8W: thickness of full levels; ZLVL forcing height [m]
       VEGTYP = IVGTYP(I,J)                           ! vegetation type
       SOILTYP= ISLTYP(I,J)                           ! soil type
       FVEG   = VEGFRA(I,J)/100.                      ! vegetation fraction [0-1]
       FVGMAX = VEGMAX (I,J)/100.                     ! Vegetation fraction annual max [0-1]
       TBOT = TMN(I,J)                                ! Fixed deep soil temperature for land
       T_ML   = T3D(I,1,J)                            ! temperature defined at intermediate level [K]
       Q_ML   = QV3D(I,1,J)/(1.0+QV3D(I,1,J))         ! convert from mixing ratio to specific humidity [kg/kg]
       U_ML   = U_PHY(I,1,J)                          ! u-wind at interface [m/s]
       V_ML   = V_PHY(I,1,J)                          ! v-wind at interface [m/s]
       SWDN   = SWDOWN(I,J)                           ! shortwave down from SW scheme [W/m2]
       LWDN   = GLW(I,J)                              ! total longwave down from LW scheme [W/m2]
       P_ML   =(P8W3D(I,KTS+1,J)+P8W3D(I,KTS,J))*0.5  ! surface pressure defined at intermediate level [Pa]
	                                              !    consistent with temperature, mixing ratio
       PSFC   = P8W3D(I,1,J)                          ! surface pressure defined a full levels [Pa]
       PRCP   = RAINBL(I,J)/DT                        ! timestep precipitation [mm/s]

! IN/OUT fields

       ISNOW                 = ISNOWXY (I,J)                ! snow layers []
       SMC  (      1:NSOIL)  = SMOIS   (I,      1:NSOIL,J)  ! soil total moisture [m3/m3]
       SMH2O(      1:NSOIL)  = SH2O    (I,      1:NSOIL,J)  ! soil liquid moisture [m3/m3]
       STC  (-NSNOW+1:    0) = TSNOXY  (I,-NSNOW+1:    0,J) ! snow temperatures [K]
       STC  (      1:NSOIL)  = TSLB    (I,      1:NSOIL,J)  ! soil temperatures [K]
       SWE                   = SNOW    (I,J)                ! snow water equivalent [mm]
       SNDPTH                = SNOWH   (I,J)                ! snow depth [m]
       QSFC1D                = QSFC    (I,J)

! INOUT (with no Noah LSM equivalent)

       TV                    = TVXY    (I,J)                ! leaf temperature [K]
       TG                    = TGXY    (I,J)                ! ground temperature [K]
       CANLIQ                = CANLIQXY(I,J)                ! canopy liquid water [mm]
       CANICE                = CANICEXY(I,J)                ! canopy frozen water [mm]
       EAH                   = EAHXY   (I,J)                ! canopy vapor pressure [Pa]
       TAH                   = TAHXY   (I,J)                ! canopy temperature [K]
       CM                    = CMXY    (I,J)                ! avg. momentum exchange (MP only) [m/s]
       CH                    = CHXY    (I,J)                ! avg. heat exchange (MP only) [m/s]
       FWET                  = FWETXY  (I,J)                ! canopy fraction wet or snow
       SNEQVO                = SNEQVOXY(I,J)                ! SWE previous timestep
       ALBOLD                = ALBOLDXY(I,J)                ! albedo previous timestep, for snow aging
       QSNOW                 = QSNOWXY (I,J)                ! snow falling on ground
       WSLAKE                = WSLAKEXY(I,J)                ! lake water storage (can be neg.) (mm)
       ZWT                   = ZWTXY   (I,J)                ! depth to water table [m]
       WA                    = WAXY    (I,J)                ! water storage in aquifer [mm]
       WT                    = WTXY    (I,J)                ! water in aquifer&saturated soil [mm]
       ZSNSO(-NSNOW+1:NSOIL) = ZSNSOXY (I,-NSNOW+1:NSOIL,J) ! depth to layer interface
       SNICE(-NSNOW+1:    0) = SNICEXY (I,-NSNOW+1:    0,J) ! snow layer ice content
       SNLIQ(-NSNOW+1:    0) = SNLIQXY (I,-NSNOW+1:    0,J) ! snow layer water content
       LFMASS                = LFMASSXY(I,J)                ! leaf mass
       RTMASS                = RTMASSXY(I,J)                ! root mass
       STMASS                = STMASSXY(I,J)                ! stem mass
       WOOD                  = WOODXY  (I,J)                ! mass of wood (incl. woody roots) [g/m2]
       STBLCP                = STBLCPXY(I,J)                ! stable carbon pool
       FASTCP                = FASTCPXY(I,J)                ! fast carbon pool
       PLAI                  = XLAIXY  (I,J)                ! leaf area index [-] (no snow effects)
       PSAI                  = XSAIXY  (I,J)                ! stem area index [-] (no snow effects)
       TAUSS                 = TAUSSXY (I,J)                ! non-dimensional snow age

! Initialized local

       FICEOLD = 0.0
       FICEOLD(ISNOW+1:0) = SNICEXY(I,ISNOW+1:0,J) &  ! snow ice fraction  
           /(SNICEXY(I,ISNOW+1:0,J)+SNLIQXY(I,ISNOW+1:0,J))
       CO2PP  = CO2 * P_ML                            ! partial pressure co2 [Pa]
       O2PP   = O2  * P_ML                            ! partial pressure  o2 [Pa]
       FOLN   = 1.0                                   ! for now, set to nitrogen saturation
       QC     = undefined_value                       ! test dummy value
       PBLH   = undefined_value                       ! test dummy value ! PBL height
       DZ8W1D = DZ8W (I,1,J)                          ! thickness of atmospheric layers
       SLOPETYP  =  1                                 ! set underground runoff slope term
       IST    = 1                                     ! MP surface type: 1 = land; 2 = lake
       ISC    = 4                                     ! soil color: assuming a middle color category ?????????


       IF(SOILTYP == 14 .AND. XICE(I,J) == 0.) THEN
          IF(IPRINT) PRINT *, ' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'
          IF(IPRINT) PRINT *, i,j,'RESET SOIL in surfce.F'
          SOILTYP = 7
       ENDIF

       IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == 31 .or. &
            IVGTYP(I,J) == 32 .or. IVGTYP(I,J) == 33) THEN
          VEGTYP = ISURBAN
       ENDIF
       IF(VEGTYP == 25) FVEG = 0.0                  ! Set playa, lava, sand to bare
       IF(VEGTYP == 25) PLAI = 0.0 
       IF(VEGTYP == 26) FVEG = 0.0                  ! hard coded for USGS
       IF(VEGTYP == 26) PLAI = 0.0
       IF(VEGTYP == 27) FVEG = 0.0
       IF(VEGTYP == 27) PLAI = 0.0

       CALL REDPRM (VEGTYP,SOILTYP,SLOPETYP,ZSOIL,NSOIL,ISURBAN)

    IF ( ICE == -1 ) THEN


      CALL NOAHMP_OPTIONS_GLACIER(IDVEG  ,IOPT_CRS  ,IOPT_BTR  ,IOPT_RUN  ,IOPT_SFC  ,IOPT_FRZ , &
                      IOPT_INF  ,IOPT_RAD  ,IOPT_ALB  ,IOPT_SNF  ,IOPT_TBOT, IOPT_STC )
      
      TBOT = MIN(TBOT,263.15)                      ! set deep temp to at most -10C
      CALL NOAHMP_GLACIER(       I,       J,    COSZ,   NSNOW,   NSOIL,      DT, & ! IN : Time/Space/Model-related
                               T_ML,    P_ML,    U_ML,    V_ML,    Q_ML,    SWDN, & ! IN : Forcing
                               PRCP,    LWDN,    TBOT,    Z_ML, FICEOLD,   ZSOIL, & ! IN : Forcing
                              QSNOW,  SNEQVO,  ALBOLD,      CM,      CH,   ISNOW, & ! IN/OUT :
                                SWE,     SMC,   ZSNSO,  SNDPTH,   SNICE,   SNLIQ, & ! IN/OUT :
                                TGB,     STC,   SMH2O,   TAUSS,  QSFC1D,          & ! IN/OUT :
                                FSA,     FSR,    FIRA,     FSH,    FGEV,   SSOIL, & ! OUT : 
                               TRAD,   ESOIL,   RUNSF,   RUNSB,     SAG,    SALB, & ! OUT :
                              QSNBOT,PONDING,PONDING1,PONDING2,    T2MB,    Q2MB, & ! OUT :
			      EMISSI,  FPICE,    CHB2 )                             ! OUT :

       FSNO   = 1.0       
       TV     = undefined_value     ! Output from standard Noah-MP undefined for glacier points
       TG     = TGB 
       CANICE = undefined_value 
       CANLIQ = undefined_value 
       EAH    = undefined_value 
       TAH    = undefined_value
       FWET   = undefined_value 
       WSLAKE = undefined_value 
       ZWT    = undefined_value 
       WA     = undefined_value 
       WT     = undefined_value 
       LFMASS = undefined_value 
       RTMASS = undefined_value 
       STMASS = undefined_value 
       WOOD   = undefined_value 
       STBLCP = undefined_value 
       FASTCP = undefined_value 
       PLAI   = undefined_value 
       PSAI   = undefined_value 
       T2MV   = undefined_value 
       Q2MV   = undefined_value 
       NEE    = undefined_value 
       GPP    = undefined_value 
       NPP    = undefined_value 
       FVEGMP = 0.0 
       ECAN   = undefined_value 
       ETRAN  = undefined_value 
       APAR   = undefined_value 
       PSN    = undefined_value 
       SAV    = undefined_value 
       RSSUN  = undefined_value 
       RSSHA  = undefined_value 
       BGAP   = undefined_value 
       WGAP   = undefined_value 
       TGV    = undefined_value
       CHV    = undefined_value 
       CHB    = CH 
       IRC    = undefined_value 
       IRG    = undefined_value 
       SHC    = undefined_value 
       SHG    = undefined_value 
       EVG    = undefined_value 
       GHV    = undefined_value 
       IRB    = FIRA
       SHB    = FSH
       EVB    = FGEV
       GHB    = SSOIL
       TR     = undefined_value 
       EVC    = undefined_value 
       CHLEAF = undefined_value 
       CHUC   = undefined_value 
       CHV2   = undefined_value 
       FCEV   = undefined_value 
       FCTR   = undefined_value        
       
       QFX(I,J) = ESOIL


    ELSE
    goto 1000
if(i==1.and.j==8) then
    print*,I       , J       , LAT     , YEARLEN , JULIAN  , COSZ
    print*,'DT'
    print*,DT      , DX      , DZ8W1D  , NSOIL   , ZSOIL   , 3
    print*,'FVEG'
    print*,FVEG    , FVGMAX  , VEGTYP  , ISURBAN , ICE     , IST
    print*,ISC     
    print*,IZ0TLND 
    print*,'T_ML'
    print*,T_ML    , P_ML    , PSFC    , U_ML    , V_ML    , Q_ML
    print*,'QC'
    print*,QC      , SWDN    , LWDN    , PRCP    , TBOT    , CO2PP
    print*,'O2PP'
    print*,O2PP    , FOLN    , FICEOLD , PBLH    , Z_ML
    print*,'ALBOLD'
    print*,ALBOLD  , SNEQVO
    print*,'STC'
    print*,STC     , SMH2O   , SMC     , TAH     , EAH     , FWET
    print*,'CANLIQ'
    print*,CANLIQ  , CANICE  , TV      , TG      , QSFC1D  , QSNOW
    print*,'ISNOW'
    print*,ISNOW   , ZSNSO   , SNDPTH  , SWE     , SNICE   , SNLIQ
    print*,'ZWT'
    print*,ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS
    print*,'STMASS'
    print*,STMASS  , WOOD    , STBLCP  , FASTCP  , PLAI    , PSAI
    print*,'CM'
    print*,CM      , CH      , TAUSS
    print*,'FSA'
    print*,FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV
    print*,'FGEV'
    print*,FGEV    , FCTR    , ECAN    , ETRAN   , ESOIL   , TRAD
    print*,'TGB'
    print*, TGB     , TGV     , T2MV    , T2MB    
    print*,'Q2MV'
    print*, Q2MV    , Q2MB    , RUNSF   , RUNSB   , APAR
    print*,'PSN'
    print*,PSN     , SAV     , SAG     , FSNO    , NEE     , GPP
    print*,'NPP'
    print*,NPP     , FVEGMP  , SALB    , QSNBOT   , PONDING , PONDING1
    print*,'PONDING2'
    print*,PONDING2, RSSUN   , RSSHA   , BGAP    , WGAP    
    print*,'CHV'
    print*, CHV     , CHB     , EMISSI
end if
!	    stop999
1000 continue

       CALL NOAHMP_SFLX (&
            I       , J       , LAT     , YEARLEN , JULIAN  , COSZ    , & ! IN : Time/Space-related
            DT      , DX      , DZ8W1D  , NSOIL   , ZSOIL   , NSNOW   , & ! IN : Model configuration 
            FVEG    , FVGMAX  , VEGTYP  , ISURBAN , ICE     , IST     , & ! IN : Vegetation/Soil characteristics
            ISC     ,                                                   & ! IN : Vegetation/Soil characteristics
            IZ0TLND ,                                                   & ! IN : User options
            T_ML    , P_ML    , PSFC    , U_ML    , V_ML    , Q_ML    , & ! IN : Forcing
            QC      , SWDN    , LWDN    , PRCP    , TBOT    , CO2PP   , & ! IN : Forcing
            O2PP    , FOLN    , FICEOLD , PBLH    , Z_ML    ,           & ! IN : Forcing
            ALBOLD  , SNEQVO  ,                                         & ! IN/OUT : 
            STC     , SMH2O   , SMC     , TAH     , EAH     , FWET    , & ! IN/OUT : 
            CANLIQ  , CANICE  , TV      , TG      , QSFC1D  , QSNOW   , & ! IN/OUT : 
            ISNOW   , ZSNSO   , SNDPTH  , SWE     , SNICE   , SNLIQ   , & ! IN/OUT : 
            ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & ! IN/OUT : 
            STMASS  , WOOD    , STBLCP  , FASTCP  , PLAI    , PSAI    , & ! IN/OUT : 
            CM      , CH      , TAUSS   ,                               & ! IN/OUT : 
            FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV    , & ! OUT : 
            FGEV    , FCTR    , ECAN    , ETRAN   , ESOIL   , TRAD    , & ! OUT : 
            TGB     , TGV     , T2MV    , T2MB    , Q2MV    , Q2MB    , & ! OUT : 
            RUNSF   , RUNSB   , APAR    , PSN     , SAV     , SAG     , & ! OUT : 
            FSNO    , NEE     , GPP     , NPP     , FVEGMP  , SALB    , & ! OUT : 
            QSNBOT  , PONDING , PONDING1, PONDING2, RSSUN   , RSSHA   , & ! OUT : 
            BGAP    , WGAP    , CHV     , CHB     , EMISSI  ,           & ! OUT : 
            SHG     , SHC     , SHB     , EVG     , EVB     , GHV     , & ! OUT :
	    GHB     , IRG     , IRC     , IRB     , TR      , EVC     , & ! OUT :
	    CHLEAF  , CHUC    , CHV2    , CHB2    , FPICE               &
#ifdef WRF_HYDRO
            , sfcheadrt(i,j)                               &
#endif
            )            ! OUT :
                  
            QFX(I,J) = ECAN + ESOIL + ETRAN

#ifdef WRF_HYDRO
            soldrain(i,j) = RUNSB*dt        !mm , underground runoff
            INFXSRT(i,j) = RUNSF*dt        !mm , surface runoff
#endif

   ENDIF ! glacial split ends 

! INPUT/OUTPUT

             TSK      (I,J)                = TRAD
             HFX      (I,J)                = FSH
             LH       (I,J)                = FCEV + FGEV + FCTR
             GRDFLX   (I,J)                = SSOIL
	     SMSTAV   (I,J)                = 0.0  ! [maintained as Noah consistency]
             SMSTOT   (I,J)                = 0.0  ! [maintained as Noah consistency]
             SFCRUNOFF(I,J)                = SFCRUNOFF(I,J) + RUNSF * DT
             UDRUNOFF (I,J)                = UDRUNOFF(I,J)  + RUNSB * DT
             IF ( SALB > -999 ) THEN
                ALBEDO(I,J)                = SALB
             ENDIF
             SNOWC    (I,J)                = FSNO
             SMOIS    (I,      1:NSOIL,J)  = SMC   (      1:NSOIL)
             SH2O     (I,      1:NSOIL,J)  = SMH2O (      1:NSOIL)
             TSLB     (I,      1:NSOIL,J)  = STC   (      1:NSOIL)
             SNOW     (I,J)                = SWE
             SNOWH    (I,J)                = SNDPTH
             CANWAT   (I,J)                = CANLIQ + CANICE
             ACSNOW   (I,J)                = ACSNOW(I,J) + PRCP * FPICE
             ACSNOM   (I,J)                = ACSNOM(I,J) + QSNBOT*DT + PONDING + PONDING1 + PONDING2
             EMISS    (I,J)                = EMISSI
             QSFC     (I,J)                = QSFC1D

             ISNOWXY  (I,J)                = ISNOW
             TVXY     (I,J)                = TV
             TGXY     (I,J)                = TG
             CANLIQXY (I,J)                = CANLIQ
             CANICEXY (I,J)                = CANICE
             EAHXY    (I,J)                = EAH
             TAHXY    (I,J)                = TAH
             CMXY     (I,J)                = CM
             CHXY     (I,J)                = CH
             FWETXY   (I,J)                = FWET
             SNEQVOXY (I,J)                = SNEQVO
             ALBOLDXY (I,J)                = ALBOLD
             QSNOWXY  (I,J)                = QSNOW
             WSLAKEXY (I,J)                = WSLAKE
             ZWTXY    (I,J)                = ZWT
             WAXY     (I,J)                = WA
             WTXY     (I,J)                = WT
             TSNOXY   (I,-NSNOW+1:    0,J) = STC   (-NSNOW+1:    0)
             ZSNSOXY  (I,-NSNOW+1:NSOIL,J) = ZSNSO (-NSNOW+1:NSOIL)
             SNICEXY  (I,-NSNOW+1:    0,J) = SNICE (-NSNOW+1:    0)
             SNLIQXY  (I,-NSNOW+1:    0,J) = SNLIQ (-NSNOW+1:    0)
             LFMASSXY (I,J)                = LFMASS
             RTMASSXY (I,J)                = RTMASS
             STMASSXY (I,J)                = STMASS
             WOODXY   (I,J)                = WOOD
             STBLCPXY (I,J)                = STBLCP
             FASTCPXY (I,J)                = FASTCP
             XLAIXY   (I,J)                = PLAI
             XSAIXY   (I,J)                = PSAI
             TAUSSXY  (I,J)                = TAUSS

! OUTPUT

             T2MVXY   (I,J)                = T2MV
             T2MBXY   (I,J)                = T2MB
             Q2MVXY   (I,J)                = Q2MV/(1.0 - Q2MV)  ! specific humidity to mixing ratio
             Q2MBXY   (I,J)                = Q2MB/(1.0 - Q2MB)  ! consistent with registry def of Q2
             TRADXY   (I,J)                = TRAD
             NEEXY    (I,J)                = NEE
             GPPXY    (I,J)                = GPP
             NPPXY    (I,J)                = NPP
             FVEGXY   (I,J)                = FVEGMP
             RUNSFXY  (I,J)                = RUNSF
             RUNSBXY  (I,J)                = RUNSB
             ECANXY   (I,J)                = ECAN
             EDIRXY   (I,J)                = ESOIL
             ETRANXY  (I,J)                = ETRAN
             FSAXY    (I,J)                = FSA
             FIRAXY   (I,J)                = FIRA
             APARXY   (I,J)                = APAR
             PSNXY    (I,J)                = PSN
             SAVXY    (I,J)                = SAV
             SAGXY    (I,J)                = SAG
             RSSUNXY  (I,J)                = RSSUN
             RSSHAXY  (I,J)                = RSSHA
             BGAPXY   (I,J)                = BGAP
             WGAPXY   (I,J)                = WGAP
             TGVXY    (I,J)                = TGV
             TGBXY    (I,J)                = TGB
             CHVXY    (I,J)                = CHV
             CHBXY    (I,J)                = CHB
             IRCXY    (I,J)                = IRC
             IRGXY    (I,J)                = IRG
             SHCXY    (I,J)                = SHC
             SHGXY    (I,J)                = SHG
             EVGXY    (I,J)                = EVG
             GHVXY    (I,J)                = GHV
             IRBXY    (I,J)                = IRB
             SHBXY    (I,J)                = SHB
             EVBXY    (I,J)                = EVB
             GHBXY    (I,J)                = GHB
             TRXY     (I,J)                = TR
             EVCXY    (I,J)                = EVC
             CHLEAFXY (I,J)                = CHLEAF
             CHUCXY   (I,J)                = CHUC
             CHV2XY   (I,J)                = CHV2
             CHB2XY   (I,J)                = CHB2

          ENDIF                                                         ! endif of land-sea test

      ENDDO ILOOP                                                       ! of I loop
   ENDDO JLOOP                                                          ! of J loop

!------------------------------------------------------
  END SUBROUTINE noahmplsm
!------------------------------------------------------

  SUBROUTINE NOAHMP_INIT ( MMINLU, SNOW , SNOWH , CANWAT , ISLTYP ,   IVGTYP, &
       TSLB , SMOIS , SH2O , DZS , FNDSOILW , FNDSNOWH ,   ISICE,             &
       TSK, isnowxy , tvxy     ,tgxy     ,canicexy ,         TMN,     XICE,   &
       canliqxy ,eahxy    ,tahxy    ,cmxy     ,chxy     ,                     &
       fwetxy   ,sneqvoxy ,alboldxy ,qsnowxy  ,wslakexy ,zwtxy    ,waxy     , &
       wtxy     ,tsnoxy   ,zsnsoxy  ,snicexy  ,snliqxy  ,lfmassxy ,rtmassxy , &
       stmassxy ,woodxy   ,stblcpxy ,fastcpxy ,xsaixy   , &
!jref:start
       t2mvxy   ,t2mbxy   ,chstarxy,            &
!jref:end       
       NSOIL, restart,                 &
       allowed_to_read ,                         &
       ids,ide, jds,jde, kds,kde,                &
       ims,ime, jms,jme, kms,kme,                &
       its,ite, jts,jte, kts,kte                 )

! Initializing Canopy air temperature to 287 K seems dangerous to me [KWM].

    INTEGER, INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde,  &
         &                           ims,ime, jms,jme, kms,kme,  &
         &                           its,ite, jts,jte, kts,kte

    INTEGER, INTENT(IN)       ::     NSOIL, ISICE

    LOGICAL, INTENT(IN)       ::     restart,                    &
         &                           allowed_to_read

    REAL,    DIMENSION( NSOIL), INTENT(IN)    ::     DZS  ! Thickness of the soil layers [m]

    REAL,    DIMENSION( ims:ime, NSOIL, jms:jme ) ,    &
         &   INTENT(INOUT)    ::     SMOIS,                      &
         &                           SH2O,                       &
         &                           TSLB

    REAL,    DIMENSION( ims:ime, jms:jme ) ,                     &
         &   INTENT(INOUT)    ::     SNOW,                       &
         &                           SNOWH,                      &
         &                           CANWAT

    INTEGER, DIMENSION( ims:ime, jms:jme ),                      &
         &   INTENT(IN)       ::     ISLTYP,  &
                                     IVGTYP

    LOGICAL, INTENT(IN)       ::     FNDSOILW,                   &
         &                           FNDSNOWH

    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: TSK         !skin temperature (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: TMN         !deep soil temperature (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: XICE         !sea ice fraction
    INTEGER, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: isnowxy     !actual no. of snow layers
    REAL, DIMENSION(ims:ime,-2:NSOIL,jms:jme), INTENT(INOUT) :: zsnsoxy  !snow layer depth [m]
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: tsnoxy   !snow temperature [K]
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: snicexy  !snow layer ice [mm]
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: snliqxy  !snow layer liquid water [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tvxy        !vegetation canopy temperature
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tgxy        !ground surface temperature
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: canicexy    !canopy-intercepted ice (mm)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: canliqxy    !canopy-intercepted liquid water (mm)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: eahxy       !canopy air vapor pressure (pa)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tahxy       !canopy air temperature (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: cmxy        !momentum drag coefficient
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chxy        !sensible heat exchange coefficient
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fwetxy      !wetted or snowed fraction of the canopy (-)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: sneqvoxy    !snow mass at last time step(mm h2o)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: alboldxy    !snow albedo at last time step (-)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: qsnowxy     !snowfall on the ground [mm/s]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wslakexy    !lake water storage [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: zwtxy       !water table depth [m]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: waxy        !water in the "aquifer" [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wtxy        !groundwater storage [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: lfmassxy    !leaf mass [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: rtmassxy    !mass of fine roots [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stmassxy    !stem mass [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: woodxy      !mass of wood (incl. woody roots) [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stblcpxy    !stable carbon in deep soil [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fastcpxy    !short-lived carbon, shallow soil [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: xsaixy      !stem area index

!jref:start
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: t2mvxy        !2m temperature vegetation part (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: t2mbxy        !2m temperature bare ground part (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chstarxy        !dummy
!jref:end


    REAL, DIMENSION(1:NSOIL)  :: ZSOIL      ! Depth of the soil layer bottom (m) from 
    !                                                   the surface (negative)

    REAL                      :: BX, SMCMAX, PSISAT

    REAL, PARAMETER           :: BLIM  = 5.5
    REAL, PARAMETER           :: HLICE = 3.335E5
    REAL, PARAMETER           :: GRAV = 9.81
    REAL, PARAMETER           :: T0 = 273.15

    INTEGER                   :: errflag

    character(len=80) :: err_message
    character(len=4)  :: MMINSL
    character(len=*), intent(in) :: MMINLU
    MMINSL='STAS'

    call read_mp_veg_parameters(trim(MMINLU))

    !
    ! initialize three Noah LSM related tables
    !
    IF ( allowed_to_read ) THEN
       CALL wrf_message( 'INITIALIZE THREE Noah LSM RELATED TABLES' )
       CALL  SOIL_VEG_GEN_PARM( MMINLU, MMINSL )
    ENDIF

    IF( .NOT. restart ) THEN

       itf=min0(ite,ide-1)
       jtf=min0(jte,jde-1)

       errflag = 0
       DO j = jts,jtf
          DO i = its,itf
             IF ( ISLTYP( i,j ) .LT. 1 ) THEN
                errflag = 1
                WRITE(err_message,*)"module_sf_noahlsm.F: lsminit: out of range ISLTYP ",i,j,ISLTYP( i,j )
                CALL wrf_message(err_message)
             ENDIF
          ENDDO
       ENDDO
       IF ( errflag .EQ. 1 ) THEN
          CALL wrf_error_fatal( "module_sf_noahlsm.F: lsminit: out of range value "// &
               "of ISLTYP. Is this field in the input?" )
       ENDIF
#ifdef WRF_CHEM
       !
       ! need this parameter for dust parameterization in wrf/chem
       !
       do I=1,NSLTYPE
          porosity(i)=maxsmc(i)
       enddo
#endif

! initialize soil liquid water content SH2O

       DO J = jts , jtf
          DO I = its , itf
	    IF(IVGTYP(I,J)==ISICE .AND. XICE(I,J) <= 0.0) THEN
              DO NS=1, NSOIL
	        SMOIS(I,NS,J) = 1.0                     ! glacier starts all frozen
	        SH2O(I,NS,J) = 0.0
	        TSLB(I,NS,J) = MIN(TSLB(I,NS,J),263.15) ! set glacier temp to at most -10C
              END DO
	        !TMN(I,J) = MIN(TMN(I,J),263.15)         ! set deep temp to at most -10C
		SNOW(I,J) = MAX(SNOW(I,J), 10.0)        ! set SWE to at least 10mm
	    ELSE
	      
             BX = BB(ISLTYP(I,J))
             SMCMAX = MAXSMC(ISLTYP(I,J))
             PSISAT = SATPSI(ISLTYP(I,J))
             IF ( ( BX > 0.0 ) .AND. ( SMCMAX > 0.0 ) .AND. ( PSISAT > 0.0 ) ) THEN
                DO NS=1, NSOIL
                   IF ( TSLB(I,NS,J) < 273.149 ) THEN    ! Use explicit as initial soil ice
                      FK=(( (HLICE/(GRAV*(-PSISAT))) *                              &
                           ((TSLB(I,NS,J)-T0)/TSLB(I,NS,J)) )**(-1/BX) )*SMCMAX
                      FK = MAX(FK, 0.02)
                      SH2O(I,NS,J) = MIN( FK, SMOIS(I,NS,J) )
                   ELSE
                      SH2O(I,NS,J)=SMOIS(I,NS,J)
                   ENDIF
                END DO
             ELSE
                DO NS=1, NSOIL
                   SH2O(I,NS,J)=SMOIS(I,NS,J)
                END DO
             ENDIF
            ENDIF
          ENDDO
       ENDDO
!  ENDIF


       !
       ! initialize physical snow height SNOWH
       !
       IF(.NOT.FNDSNOWH)THEN
          ! If no SNOWH do the following
          CALL wrf_message( 'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN LSMINIT' )
          DO J = jts,jtf
             DO I = its,itf
                SNOWH(I,J)=SNOW(I,J)*0.005               ! SNOW in mm and SNOWH in m
             ENDDO
          ENDDO
       ENDIF

       DO J = jts,jtf
          DO I = its,itf
             tvxy       (I,J) = TSK(I,J)
             tgxy       (I,J) = TSK(I,J)
             CANWAT     (I,J) = 0.0
             canliqxy   (I,J) = CANWAT(I,J)
             canicexy   (I,J) = 0.
             eahxy      (I,J) = 2000. 
             tahxy      (I,J) = TSK(I,J)
!             tahxy      (I,J) = 287.
!jref:start
             t2mvxy     (I,J) = TSK(I,J)
             t2mbxy     (I,J) = TSK(I,J)
             chstarxy     (I,J) = 0.1
!jref:end

             cmxy       (I,J) = 0.0
             chxy       (I,J) = 0.0
             fwetxy     (I,J) = 0.0
             sneqvoxy   (I,J) = 0.0
             alboldxy   (I,J) = 0.65
             qsnowxy    (I,J) = 0.0
             wslakexy   (I,J) = 0.0

             waxy       (I,J) = 4900.                                       !???
             wtxy       (I,J) = waxy(i,j)                                   !???
             zwtxy      (I,J) = (25. + 2.0) - waxy(i,j)/1000/0.2            !???

             lfmassxy   (I,J) = 50.         !
             stmassxy   (I,J) = 50.0        !
             rtmassxy   (I,J) = 500.0       !
             woodxy     (I,J) = 500.0       !
             stblcpxy   (I,J) = 1000.0      !
             fastcpxy   (I,J) = 1000.0      !
             xsaixy     (I,J) = 0.1         !

          enddo
       enddo

       ! Given the soil layer thicknesses (in DZS), initialize the soil layer
       ! depths from the surface.
       ZSOIL(1)         = -DZS(1)          ! negative
       DO NS=2, NSOIL
          ZSOIL(NS)       = ZSOIL(NS-1) - DZS(NS)
       END DO

       ! Initialize snow/soil layer arrays ZSNSOXY, TSNOXY, SNICEXY, SNLIQXY, 
       ! and ISNOWXY
       CALL snow_init ( ims , ime , jms , jme , its , itf , jts , jtf , 3 , &
            &           NSOIL , zsoil , snow , tgxy , snowh ,     &
            &           zsnsoxy , tsnoxy , snicexy , snliqxy , isnowxy )

    ENDIF
  END SUBROUTINE NOAHMP_INIT

!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------

  SUBROUTINE SNOW_INIT ( ims , ime , jms , jme , its , itf , jts , jtf ,                  &
       &                 NSNOW , NSOIL , ZSOIL , SWE , TGXY , SNODEP ,                    &
       &                 ZSNSOXY , TSNOXY , SNICEXY ,SNLIQXY , ISNOWXY )
!------------------------------------------------------------------------------------------
!   Initialize snow arrays for Noah-MP LSM, based in input SNOWDEP, NSNOW
!   ISNOWXY is an index array, indicating the index of the top snow layer.  Valid indices
!           for snow layers range from 0 (no snow) and -1 (shallow snow) to (-NSNOW)+1 (deep snow).
!   TSNOXY holds the temperature of the snow layer.  Snow layers are initialized with 
!          temperature = ground temperature [?].  Snow-free levels in the array have value 0.0
!   SNICEXY is the frozen content of a snow layer.  Initial estimate based on SNODEP and SWE
!   SNLIQXY is the liquid content of a snow layer.  Initialized to 0.0
!   ZNSNOXY is the layer depth from the surface.  
!------------------------------------------------------------------------------------------
    IMPLICIT NONE
!------------------------------------------------------------------------------------------
    INTEGER, INTENT(IN)                              :: ims, ime, jms, jme
    INTEGER, INTENT(IN)                              :: its, itf, jts, jtf
    INTEGER, INTENT(IN)                              :: NSNOW
    INTEGER, INTENT(IN)                              :: NSOIL
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SWE 
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SNODEP
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: TGXY
    REAL,    INTENT(IN), DIMENSION(1:NSOIL)          :: ZSOIL

    INTEGER, INTENT(OUT), DIMENSION(ims:ime, jms:jme)                :: ISNOWXY ! Top snow layer index
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:NSOIL,jms:jme) :: ZSNSOXY ! Snow/soil layer depth from surface [m]
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: TSNOXY  ! Snow layer temperature [K]
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: SNICEXY ! Snow layer ice content [mm]
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: SNLIQXY ! snow layer liquid content [mm]

! Local variables:
!   DZSNO   holds the thicknesses of the various snow layers.
!   DZSNOSO holds the thicknesses of the various soil/snow layers.
    INTEGER                           :: I,J,IZ
    REAL,   DIMENSION(-NSNOW+1:    0) :: DZSNO
    REAL,   DIMENSION(-NSNOW+1:NSOIL) :: DZSNSO

!------------------------------------------------------------------------------------------

    DO J = jts , jtf
       DO I = its , itf
          IF ( SNODEP(I,J) < 0.025 ) THEN
             ISNOWXY(I,J) = 0
             DZSNO(-NSNOW+1:0) = 0.
          ELSE
             IF ( ( SNODEP(I,J) >= 0.025 ) .AND. ( SNODEP(I,J) <= 0.05 ) ) THEN
                ISNOWXY(I,J)    = -1
                DZSNO(0)  = SNODEP(I,J)
             ELSE IF ( ( SNODEP(I,J) > 0.05 ) .AND. ( SNODEP(I,J) <= 0.10 ) ) THEN
                ISNOWXY(I,J)    = -2
                DZSNO(-1) = SNODEP(I,J)/2.
                DZSNO( 0) = SNODEP(I,J)/2.
             ELSE IF ( (SNODEP(I,J) > 0.10 ) .AND. ( SNODEP(I,J) <= 0.25 ) ) THEN
                ISNOWXY(I,J)    = -2
                DZSNO(-1) = 0.05
                DZSNO( 0) = SNODEP(I,J) - DZSNO(-1)
             ELSE IF ( ( SNODEP(I,J) > 0.25 ) .AND. ( SNODEP(I,J) <= 0.45 ) ) THEN
                ISNOWXY(I,J)    = -3
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.5*(SNODEP(I,J)-DZSNO(-2))
                DZSNO( 0) = 0.5*(SNODEP(I,J)-DZSNO(-2))
             ELSE IF ( SNODEP(I,J) > 0.45 ) THEN
                ISNOWXY(I,J)     = -3
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.20
                DZSNO( 0) = SNODEP(I,J) - DZSNO(-1) - DZSNO(-2)
             ELSE
                CALL wrf_error_fatal("Problem with the logic assigning snow layers.")
             END IF
          END IF

          TSNOXY (I,-NSNOW+1:0,J) = 0.
          SNICEXY(I,-NSNOW+1:0,J) = 0.
          SNLIQXY(I,-NSNOW+1:0,J) = 0.
          DO IZ = ISNOWXY(I,J)+1 , 0
             TSNOXY(I,IZ,J)  = TGXY(I,J)  ! [k]
             SNLIQXY(I,IZ,J) = 0.00
             SNICEXY(I,IZ,J) = 1.00 * DZSNO(IZ) * (SWE(I,J)/SNODEP(I,J))  ! [kg/m3]
          END DO

          ! Assign local variable DZSNSO, the soil/snow layer thicknesses, for snow layers
          DO IZ = ISNOWXY(I,J)+1 , 0
             DZSNSO(IZ) = -DZSNO(IZ)
          END DO

          ! Assign local variable DZSNSO, the soil/snow layer thicknesses, for soil layers
          DZSNSO(1) = ZSOIL(1)
          DO IZ = 2 , NSOIL
             DZSNSO(IZ) = (ZSOIL(IZ) - ZSOIL(IZ-1))
          END DO

          ! Assign ZSNSOXY, the layer depths, for soil and snow layers
          ZSNSOXY(I,ISNOWXY(I,J)+1,J) = DZSNSO(ISNOWXY(I,J)+1)
          DO IZ = ISNOWXY(I,J)+2 , NSOIL
             ZSNSOXY(I,IZ,J) = ZSNSOXY(I,IZ-1,J) + DZSNSO(IZ)
          ENDDO

       END DO
    END DO

  END SUBROUTINE SNOW_INIT
!
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------
!
END MODULE module_sf_noahmpdrv
