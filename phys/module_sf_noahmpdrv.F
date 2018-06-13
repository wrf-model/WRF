MODULE module_sf_noahmpdrv

!-------------------------------
#if ( WRF_CHEM == 1 )
  USE module_data_gocart_dust
#endif
!-------------------------------

!
CONTAINS
!
  SUBROUTINE noahmplsm(ITIMESTEP,        YR,   JULIAN,   COSZIN,XLAT,XLONG, & ! IN : Time/Space-related
                  DZ8W,       DT,       DZS,    NSOIL,       DX,            & ! IN : Model configuration 
	        IVGTYP,   ISLTYP,    VEGFRA,   VEGMAX,      TMN,            & ! IN : Vegetation/Soil characteristics
		 XLAND,     XICE,XICE_THRES,  CROPCAT,                      & ! IN : Vegetation/Soil characteristics
	       PLANTING,  HARVEST,SEASON_GDD,                               &
                 IDVEG, IOPT_CRS,  IOPT_BTR, IOPT_RUN, IOPT_SFC, IOPT_FRZ,  & ! IN : User options
              IOPT_INF, IOPT_RAD,  IOPT_ALB, IOPT_SNF,IOPT_TBOT, IOPT_STC,  & ! IN : User options
              IOPT_GLA, IOPT_RSF, IOPT_SOIL,IOPT_PEDO,IOPT_CROP,            & ! IN : User options
              IZ0TLND, SF_URBAN_PHYSICS,                                    & ! IN : User options
	      SOILCOMP,  SOILCL1,  SOILCL2,   SOILCL3,  SOILCL4,            & ! IN : User options
                   T3D,     QV3D,     U_PHY,    V_PHY,   SWDOWN,      GLW,  & ! IN : Forcing
		 P8W3D,PRECIP_IN,        SR,                                & ! IN : Forcing
                   TSK,      HFX,      QFX,        LH,   GRDFLX,    SMSTAV, & ! IN/OUT LSM eqv
                SMSTOT,SFCRUNOFF, UDRUNOFF,    ALBEDO,    SNOWC,     SMOIS, & ! IN/OUT LSM eqv
		  SH2O,     TSLB,     SNOW,     SNOWH,   CANWAT,    ACSNOM, & ! IN/OUT LSM eqv
		ACSNOW,    EMISS,     QSFC,                                 & ! IN/OUT LSM eqv
 		    Z0,      ZNT,                                           & ! IN/OUT LSM eqv
               ISNOWXY,     TVXY,     TGXY,  CANICEXY, CANLIQXY,     EAHXY, & ! IN/OUT Noah MP only
	         TAHXY,     CMXY,     CHXY,    FWETXY, SNEQVOXY,  ALBOLDXY, & ! IN/OUT Noah MP only
               QSNOWXY, WSLAKEXY,    ZWTXY,      WAXY,     WTXY,    TSNOXY, & ! IN/OUT Noah MP only
	       ZSNSOXY,  SNICEXY,  SNLIQXY,  LFMASSXY, RTMASSXY,  STMASSXY, & ! IN/OUT Noah MP only
	        WOODXY, STBLCPXY, FASTCPXY,    XLAIXY,   XSAIXY,   TAUSSXY, & ! IN/OUT Noah MP only
	       SMOISEQ, SMCWTDXY,DEEPRECHXY,   RECHXY,  GRAINXY,    GDDXY,PGSXY,  & ! IN/OUT Noah MP only
               GECROS_STATE,                                                & ! IN/OUT gecros model
	        T2MVXY,   T2MBXY,    Q2MVXY,   Q2MBXY,                      & ! OUT Noah MP only
	        TRADXY,    NEEXY,    GPPXY,     NPPXY,   FVEGXY,   RUNSFXY, & ! OUT Noah MP only
	       RUNSBXY,   ECANXY,   EDIRXY,   ETRANXY,    FSAXY,    FIRAXY, & ! OUT Noah MP only
	        APARXY,    PSNXY,    SAVXY,     SAGXY,  RSSUNXY,   RSSHAXY, & ! OUT Noah MP only
		BGAPXY,   WGAPXY,    TGVXY,     TGBXY,    CHVXY,     CHBXY, & ! OUT Noah MP only
		 SHGXY,    SHCXY,    SHBXY,     EVGXY,    EVBXY,     GHVXY, & ! OUT Noah MP only
		 GHBXY,    IRGXY,    IRCXY,     IRBXY,     TRXY,     EVCXY, & ! OUT Noah MP only
              CHLEAFXY,   CHUCXY,   CHV2XY,    CHB2XY, RS,                  & ! OUT Noah MP only
!                 BEXP_3D,SMCDRY_3D,SMCWLT_3D,SMCREF_3D,SMCMAX_3D,          & ! placeholders to activate 3D soil
!		 DKSAT_3D,DWSAT_3D,PSISAT_3D,QUARTZ_3D,                     &
!		 REFDK_2D,REFKDT_2D,                                        &
#ifdef WRF_HYDRO
               sfcheadrt,INFXSRT,soldrain,                                  &
#endif
               ids,ide,  jds,jde,  kds,kde,                    &
               ims,ime,  jms,jme,  kms,kme,                    &
               its,ite,  jts,jte,  kts,kte,                    &
               MP_RAINC, MP_RAINNC, MP_SHCV, MP_SNOW, MP_GRAUP, MP_HAIL     )
!----------------------------------------------------------------
    USE MODULE_SF_NOAHMPLSM
!    USE MODULE_SF_NOAHMPLSM, only: noahmp_options, NOAHMP_SFLX, noahmp_parameters
    USE module_sf_noahmp_glacier
    USE NOAHMP_TABLES, ONLY: ISICE_TABLE, CO2_TABLE, O2_TABLE, DEFAULT_CROP_TABLE, ISCROP_TABLE, ISURBAN_TABLE, NATURAL_TABLE, &
                             LOW_DENSITY_RESIDENTIAL_TABLE, HIGH_DENSITY_RESIDENTIAL_TABLE, HIGH_INTENSITY_INDUSTRIAL_TABLE
    USE module_sf_urban,    only: IRI_SCHEME
    USE module_ra_gfdleta,  only: cal_mon_day
!----------------------------------------------------------------
    IMPLICIT NONE
!----------------------------------------------------------------

! IN only

    INTEGER,                                         INTENT(IN   ) ::  ITIMESTEP ! timestep number
    INTEGER,                                         INTENT(IN   ) ::  YR        ! 4-digit year
    REAL,                                            INTENT(IN   ) ::  JULIAN    ! Julian day
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  COSZIN    ! cosine zenith angle
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XLAT      ! latitude [rad]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XLONG     ! latitude [rad]
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
    INTEGER,                                         INTENT(IN   ) ::  IOPT_GLA  ! glacier option (1->phase change; 2->simple)
    INTEGER,                                         INTENT(IN   ) ::  IOPT_RSF  ! surface resistance (1->Sakaguchi/Zeng; 2->Seller; 3->mod Sellers; 4->1+snow)
    INTEGER,                                         INTENT(IN   ) ::  IOPT_SOIL ! soil configuration option
    INTEGER,                                         INTENT(IN   ) ::  IOPT_PEDO ! soil pedotransfer function option
    INTEGER,                                         INTENT(IN   ) ::  IOPT_CROP ! crop model option (0->none; 1->Liu et al.; 2->Gecros)
    INTEGER,                                         INTENT(IN   ) ::  IZ0TLND   ! option of Chen adjustment of Czil (not used)
    INTEGER,                                         INTENT(IN   ) ::  sf_urban_physics   ! urban physics option
    REAL,    DIMENSION( ims:ime,       8, jms:jme ), INTENT(IN   ) ::  SOILCOMP  ! soil sand and clay percentage
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SOILCL1   ! soil texture in layer 1
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SOILCL2   ! soil texture in layer 2
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SOILCL3   ! soil texture in layer 3
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SOILCL4   ! soil texture in layer 4
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  T3D       ! 3D atmospheric temperature valid at mid-levels [K]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  QV3D      ! 3D water vapor mixing ratio [kg/kg_dry]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  U_PHY     ! 3D U wind component [m/s]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  V_PHY     ! 3D V wind component [m/s]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SWDOWN    ! solar down at surface [W m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  GLW       ! longwave down at surface [W m-2]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  P8W3D     ! 3D pressure, valid at interface [Pa]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  PRECIP_IN ! total input precipitation [mm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SR        ! frozen precipitation ratio [-]

!Optional Detailed Precipitation Partitioning Inputs
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ), OPTIONAL ::  MP_RAINC  ! convective precipitation entering land model [mm] ! MB/AN : v3.7
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ), OPTIONAL ::  MP_RAINNC ! large-scale precipitation entering land model [mm]! MB/AN : v3.7
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ), OPTIONAL ::  MP_SHCV   ! shallow conv precip entering land model [mm]      ! MB/AN : v3.7
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ), OPTIONAL ::  MP_SNOW   ! snow precipitation entering land model [mm]       ! MB/AN : v3.7
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ), OPTIONAL ::  MP_GRAUP  ! graupel precipitation entering land model [mm]    ! MB/AN : v3.7
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ), OPTIONAL ::  MP_HAIL   ! hail precipitation entering land model [mm]       ! MB/AN : v3.7

! Crop Model
    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  CROPCAT   ! crop catagory
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  PLANTING  ! planting date
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  HARVEST   ! harvest date
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SEASON_GDD! growing season GDD
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  GRAINXY   ! mass of grain XING [g/m2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  GDDXY     ! growing degree days XING (based on 10C) 
 INTEGER,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  PGSXY

! gecros model
    REAL,    DIMENSION( ims:ime,       60,jms:jme ), INTENT(INOUT) :: gecros_state !  gecros crop

#ifdef WRF_HYDRO
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  sfcheadrt,INFXSRT,soldrain   ! for WRF-Hydro
#endif
! placeholders for 3D soil
!    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(IN) ::  BEXP_3D   ! C-H B exponent
!    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(IN) ::  SMCDRY_3D ! Soil Moisture Limit: Dry
!    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(IN) ::  SMCWLT_3D ! Soil Moisture Limit: Wilt
!    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(IN) ::  SMCREF_3D ! Soil Moisture Limit: Reference
!    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(IN) ::  SMCMAX_3D ! Soil Moisture Limit: Max
!    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(IN) ::  DKSAT_3D  ! Saturated Soil Conductivity
!    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(IN) ::  DWSAT_3D  ! Saturated Soil Diffusivity
!    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(IN) ::  PSISAT_3D ! Saturated Matric Potential
!    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(IN) ::  QUARTZ_3D ! Soil quartz content
!    REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN)          ::  REFDK_2D  ! Reference Soil Conductivity
!    REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN)          ::  REFKDT_2D ! Soil Infiltration Parameter

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
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  Z0        ! combined z0 sent to coupled model
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ZNT       ! combined z0 sent to coupled model
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  RS        ! Total stomatal resistance (s/m)

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
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  SMOISEQ   ! eq volumetric soil moisture [m3/m3]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SMCWTDXY  ! soil moisture content in the layer to the water table when deep
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  DEEPRECHXY ! recharge to the water table when deep
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  RECHXY    ! recharge to the water table (diagnostic) 

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
    INTEGER,    DIMENSION(NSOIL)        :: SOILTYP      ! soil type
    INTEGER                             :: CROPTYPE     ! crop type
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
    REAL                                :: PRCP         ! total precipitation entering  [mm]         ! MB/AN : v3.7
    REAL                                :: PRCPCONV     ! convective precipitation entering  [mm]    ! MB/AN : v3.7
    REAL                                :: PRCPNONC     ! non-convective precipitation entering [mm] ! MB/AN : v3.7
    REAL                                :: PRCPSHCV     ! shallow convective precip entering  [mm]   ! MB/AN : v3.7
    REAL                                :: PRCPSNOW     ! snow entering land model [mm]              ! MB/AN : v3.7
    REAL                                :: PRCPGRPL     ! graupel entering land model [mm]           ! MB/AN : v3.7
    REAL                                :: PRCPHAIL     ! hail entering land model [mm]              ! MB/AN : v3.7
    REAL                                :: PRCPOTHR     ! other precip, e.g. fog [mm]                ! MB/AN : v3.7

! INOUT (with generic LSM equivalent)

    REAL                                :: FSH          ! total sensible heat (w/m2) [+ to atm]
    REAL                                :: SSOIL        ! soil heat heat (w/m2) 
    REAL                                :: SALB         ! surface albedo (-)
    REAL                                :: FSNO         ! snow cover fraction (-)
    REAL,   DIMENSION( 1:NSOIL)         :: SMCEQ        ! eq vol. soil moisture (m3/m3)
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
    REAL                                :: SMCWTD       ! soil moisture content in the layer to the water table when deep
    REAL                                :: DEEPRECH     ! recharge to the water table when deep
    REAL                                :: RECH         ! recharge to the water table (diagnostic)  
    REAL, DIMENSION(-2:NSOIL)           :: ZSNSO        ! snow layer depth [m]
    REAL, DIMENSION(-2:              0) :: SNICE        ! snow layer ice [mm]
    REAL, DIMENSION(-2:              0) :: SNLIQ        ! snow layer liquid water [mm]
    REAL                                :: LFMASS       ! leaf mass [g/m2]
    REAL                                :: RTMASS       ! mass of fine roots [g/m2]
    REAL                                :: STMASS       ! stem mass [g/m2]
    REAL                                :: WOOD         ! mass of wood (incl. woody roots) [g/m2]
    REAL                                :: GRAIN        ! mass of grain XING [g/m2]
    REAL                                :: GDD          ! mass of grain XING[g/m2]
    INTEGER                             :: PGS          !stem respiration [g/m2/s]
    REAL                                :: STBLCP       ! stable carbon in deep soil [g/m2]
    REAL                                :: FASTCP       ! short-lived carbon, shallow soil [g/m2]
    REAL                                :: PLAI         ! leaf area index
    REAL                                :: PSAI         ! stem area index
    REAL                                :: TAUSS        ! non-dimensional snow age

! OUT (with no Noah LSM equivalent)

    REAL                                :: Z0WRF        ! combined z0 sent to coupled model
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
    REAL                                :: RB           ! leaf boundary layer resistance (s/m)
    REAL                                :: LAISUN       ! sunlit leaf area index (m2/m2)
    REAL                                :: LAISHA       ! shaded leaf area index (m2/m2)
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
  REAL   :: PAHV    !precipitation advected heat - vegetation net (W/m2)
  REAL   :: PAHG    !precipitation advected heat - under canopy net (W/m2)
  REAL   :: PAHB    !precipitation advected heat - bare ground net (W/m2)
  REAL   :: PAH     !precipitation advected heat - total (W/m2)

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

    REAL, DIMENSION(1:60)               :: gecros1d     !  gecros crop
    REAL                                :: gecros_dd ,gecros_tbem,gecros_emb ,gecros_ema, &
                                           gecros_ds1,gecros_ds2 ,gecros_ds1x,gecros_ds2x 

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

    INTEGER                             :: SOILCOLOR          ! soil color index
    INTEGER                             :: IST          ! surface type 1-soil; 2-lake
    INTEGER                             :: YEARLEN
    REAL                                :: SOLAR_TIME
    INTEGER                             :: JMONTH, JDAY

    INTEGER, PARAMETER                  :: NSNOW = 3    ! number of snow layers fixed to 3
    REAL, PARAMETER                     :: undefined_value = -1.E36
    
    REAL, DIMENSION( 1:nsoil ) :: SAND
    REAL, DIMENSION( 1:nsoil ) :: CLAY
    REAL, DIMENSION( 1:nsoil ) :: ORGM
    
    type(noahmp_parameters) :: parameters


! ----------------------------------------------------------------------

    CALL NOAHMP_OPTIONS(IDVEG  ,IOPT_CRS  ,IOPT_BTR  ,IOPT_RUN  ,IOPT_SFC  ,IOPT_FRZ , &
                     IOPT_INF  ,IOPT_RAD  ,IOPT_ALB  ,IOPT_SNF  ,IOPT_TBOT, IOPT_STC , &
		     IOPT_RSF  ,IOPT_SOIL ,IOPT_PEDO ,IOPT_CROP )

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

       SH2O  (i,1:NSOIL,j) = 1.0
       XLAIXY(i,j)         = 0.01

       CYCLE ILOOP ! Skip any processing at sea-ice points

    ELSE

       IF((XLAND(I,J)-1.5) >= 0.) CYCLE ILOOP   ! Open water case

!     2D to 1D       

! IN only

       COSZ   = COSZIN  (I,J)                         ! cos zenith angle []
       LAT    = XLAT  (I,J)                           ! latitude [rad]
       Z_ML   = 0.5*DZ8W(I,1,J)                       ! DZ8W: thickness of full levels; ZLVL forcing height [m]
       VEGTYP = IVGTYP(I,J)                           ! vegetation type
       if(iopt_soil == 1) then
         SOILTYP= ISLTYP(I,J)                         ! soil type same in all layers
       elseif(iopt_soil == 2) then
         SOILTYP(1) = nint(SOILCL1(I,J))              ! soil type in layer1
         SOILTYP(2) = nint(SOILCL2(I,J))              ! soil type in layer2
         SOILTYP(3) = nint(SOILCL3(I,J))              ! soil type in layer3
         SOILTYP(4) = nint(SOILCL4(I,J))              ! soil type in layer4
       elseif(iopt_soil == 3) then
         SOILTYP= ISLTYP(I,J)                         ! to initialize with default
       end if 
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
       PRCP   = PRECIP_IN (I,J) / DT                  ! timestep total precip rate (glacier) [mm/s]! MB: v3.7

       CROPTYPE = 0
       IF (IOPT_CROP > 0 .AND. VEGTYP == ISCROP_TABLE) CROPTYPE = DEFAULT_CROP_TABLE ! default croptype is generic dynamic vegetation crop
       IF (IOPT_CROP > 0 .AND. CROPCAT(I,J) > 0) THEN
         CROPTYPE = CROPCAT(I,J)                      ! crop type
	 VEGTYP = ISCROP_TABLE
         FVGMAX = 0.95
	 FVEG   = 0.95
       END IF

       IF (PRESENT(MP_RAINC) .AND. PRESENT(MP_RAINNC) .AND. PRESENT(MP_SHCV) .AND. &
           PRESENT(MP_SNOW)  .AND. PRESENT(MP_GRAUP)  .AND. PRESENT(MP_HAIL)   ) THEN

         PRCPCONV  = MP_RAINC (I,J)/DT                ! timestep convective precip rate [mm/s]     ! MB: v3.7
         PRCPNONC  = MP_RAINNC(I,J)/DT                ! timestep non-convective precip rate [mm/s] ! MB: v3.7
         PRCPSHCV  = MP_SHCV(I,J)  /DT                ! timestep shallow conv precip rate [mm/s]   ! MB: v3.7
         PRCPSNOW  = MP_SNOW(I,J)  /DT                ! timestep snow precip rate [mm/s]           ! MB: v3.7
         PRCPGRPL  = MP_GRAUP(I,J) /DT                ! timestep graupel precip rate [mm/s]        ! MB: v3.7
         PRCPHAIL  = MP_HAIL(I,J)  /DT                ! timestep hail precip rate [mm/s]           ! MB: v3.7

         PRCPOTHR  = PRCP - PRCPCONV - PRCPNONC - PRCPSHCV ! take care of other (fog) contained in rainbl
	 PRCPOTHR  = MAX(0.0,PRCPOTHR)
	 PRCPNONC  = PRCPNONC + PRCPOTHR
         PRCPSNOW  = PRCPSNOW + SR(I,J)  * PRCPOTHR 
       ELSE
         PRCPCONV  = 0.
         PRCPNONC  = PRCP
         PRCPSHCV  = 0.
         PRCPSNOW  = SR(I,J) * PRCP
         PRCPGRPL  = 0.
         PRCPHAIL  = 0.
       ENDIF

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
       SMCEQ(       1:NSOIL) = SMOISEQ (I,       1:NSOIL,J)
       SMCWTD                = SMCWTDXY(I,J)
       RECH                  = 0.
       DEEPRECH              = 0.  

       if(iopt_crop == 2) then   ! gecros crop model

         gecros1d(1:60)      = gecros_state(I,1:60,J)       ! Gecros variables 2D -> local
         
         if(croptype == 1) then
           gecros_dd   =  2.5
           gecros_tbem =  2.0
           gecros_emb  = 10.2 
           gecros_ema  = 40.0
           gecros_ds1  =  2.1 !BBCH 92
           gecros_ds2  =  2.0 !BBCH 90
           gecros_ds1x =  0.0
           gecros_ds2x = 10.0
         end if
           
         if(croptype == 2) then
           gecros_dd   =  5.0
           gecros_tbem =  8.0
           gecros_emb  = 15.0 
           gecros_ema  =  6.0
           gecros_ds1  =  1.78  !BBCH 85
           gecros_ds2  =  1.63  !BBCH 80
           gecros_ds1x =  0.0
           gecros_ds2x = 14.0
         end if

       end if 

       SLOPETYP     = 1                               ! set underground runoff slope term
       IST          = 1                               ! MP surface type: 1 = land; 2 = lake
       SOILCOLOR    = 4                               ! soil color: assuming a middle color category ?????????

       IF(any(SOILTYP == 14) .AND. XICE(I,J) == 0.) THEN
          IF(IPRINT) PRINT *, ' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'
          IF(IPRINT) PRINT *, i,j,'RESET SOIL in surfce.F'
          SOILTYP = 7
       ENDIF

       IF( IVGTYP(I,J) == ISURBAN_TABLE                  .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL_TABLE .or. &
           IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL_TABLE .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL_TABLE ) THEN

         IF(SF_URBAN_PHYSICS == 0 ) THEN
           VEGTYP = ISURBAN_TABLE
         ELSE
           VEGTYP = NATURAL_TABLE  ! set urban vegetation type based on table natural
           FVGMAX = 0.96 
         ENDIF

       ENDIF

! placeholders for 3D soil
!       parameters%bexp   = BEXP_3D  (I,1:NSOIL,J) ! C-H B exponent
!       parameters%smcdry = SMCDRY_3D(I,1:NSOIL,J) ! Soil Moisture Limit: Dry
!       parameters%smcwlt = SMCWLT_3D(I,1:NSOIL,J) ! Soil Moisture Limit: Wilt
!       parameters%smcref = SMCREF_3D(I,1:NSOIL,J) ! Soil Moisture Limit: Reference
!       parameters%smcmax = SMCMAX_3D(I,1:NSOIL,J) ! Soil Moisture Limit: Max
!       parameters%dksat  = DKSAT_3D (I,1:NSOIL,J) ! Saturated Soil Conductivity
!       parameters%dwsat  = DWSAT_3D (I,1:NSOIL,J) ! Saturated Soil Diffusivity
!       parameters%psisat = PSISAT_3D(I,1:NSOIL,J) ! Saturated Matric Potential
!       parameters%quartz = QUARTZ_3D(I,1:NSOIL,J) ! Soil quartz content
!       parameters%refdk  = REFDK_2D (I,J)         ! Reference Soil Conductivity
!       parameters%refkdt = REFKDT_2D(I,J)         ! Soil Infiltration Parameter

       CALL TRANSFER_MP_PARAMETERS(VEGTYP,SOILTYP,SLOPETYP,SOILCOLOR,CROPTYPE,parameters)
       
       if(iopt_soil == 3 .and. .not. parameters%urban_flag) then
         
	sand = 0.01 * soilcomp(i,1:4,j)
	clay = 0.01 * soilcomp(i,5:8,j)
        orgm = 0.0

        if(opt_pedo == 1) call pedotransfer_sr2006(nsoil,sand,clay,orgm,parameters)

       end if

       GRAIN = GRAINXY (I,J)                ! mass of grain XING [g/m2]
       GDD   = GDDXY (I,J)                  ! growing degree days XING
       PGS   = PGSXY (I,J)                  ! growing degree days XING

       if(iopt_crop == 1 .and. croptype > 0) then
         parameters%PLTDAY = PLANTING(I,J)
	 parameters%HSDAY  = HARVEST (I,J)
	 parameters%GDDS1  = SEASON_GDD(I,J) / 1770.0 * parameters%GDDS1
	 parameters%GDDS2  = SEASON_GDD(I,J) / 1770.0 * parameters%GDDS2
	 parameters%GDDS3  = SEASON_GDD(I,J) / 1770.0 * parameters%GDDS3
	 parameters%GDDS4  = SEASON_GDD(I,J) / 1770.0 * parameters%GDDS4
	 parameters%GDDS5  = SEASON_GDD(I,J) / 1770.0 * parameters%GDDS5
       end if

!=== hydrological processes for vegetation in urban model ===
!=== irrigate vegetaion only in urban area, MAY-SEP, 9-11pm

       IF( IVGTYP(I,J) == ISURBAN_TABLE                  .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL_TABLE .or. &
           IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL_TABLE .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL_TABLE ) THEN

         IF(SF_URBAN_PHYSICS > 0 .AND. IRI_SCHEME == 1 ) THEN
	     SOLAR_TIME = (JULIAN - INT(JULIAN))*24 + XLONG(I,J)/15.0
	     IF(SOLAR_TIME < 0.) SOLAR_TIME = SOLAR_TIME + 24.
             CALL CAL_MON_DAY(INT(JULIAN),YR,JMONTH,JDAY)
             IF (SOLAR_TIME >= 21. .AND. SOLAR_TIME <= 23. .AND. JMONTH >= 5 .AND. JMONTH <= 9) THEN
                SMC(1) = max(SMC(1),parameters%SMCREF(1))
                SMC(2) = max(SMC(2),parameters%SMCREF(2))
             ENDIF
         ENDIF

       ENDIF

! Initialized local

       FICEOLD = 0.0
       FICEOLD(ISNOW+1:0) = SNICEXY(I,ISNOW+1:0,J) &  ! snow ice fraction  
           /(SNICEXY(I,ISNOW+1:0,J)+SNLIQXY(I,ISNOW+1:0,J))
       CO2PP  = CO2_TABLE * P_ML                      ! partial pressure co2 [Pa]
       O2PP   = O2_TABLE  * P_ML                      ! partial pressure  o2 [Pa]
       FOLN   = 1.0                                   ! for now, set to nitrogen saturation
       QC     = undefined_value                       ! test dummy value
       PBLH   = undefined_value                       ! test dummy value ! PBL height
       DZ8W1D = DZ8W (I,1,J)                          ! thickness of atmospheric layers

       IF(VEGTYP == 25) FVEG = 0.0                  ! Set playa, lava, sand to bare
       IF(VEGTYP == 25) PLAI = 0.0 
       IF(VEGTYP == 26) FVEG = 0.0                  ! hard coded for USGS
       IF(VEGTYP == 26) PLAI = 0.0
       IF(VEGTYP == 27) FVEG = 0.0
       IF(VEGTYP == 27) PLAI = 0.0

       IF ( VEGTYP == ISICE_TABLE ) THEN
         ICE = -1                           ! Land-ice point
         CALL NOAHMP_OPTIONS_GLACIER(IOPT_ALB  ,IOPT_SNF  ,IOPT_TBOT, IOPT_STC, IOPT_GLA )
      
         TBOT = MIN(TBOT,263.15)                      ! set deep temp to at most -10C
         CALL NOAHMP_GLACIER(     I,       J,    COSZ,   NSNOW,   NSOIL,      DT, & ! IN : Time/Space/Model-related
                               T_ML,    P_ML,    U_ML,    V_ML,    Q_ML,    SWDN, & ! IN : Forcing
                               PRCP,    LWDN,    TBOT,    Z_ML, FICEOLD,   ZSOIL, & ! IN : Forcing
                              QSNOW,  SNEQVO,  ALBOLD,      CM,      CH,   ISNOW, & ! IN/OUT :
                                SWE,     SMC,   ZSNSO,  SNDPTH,   SNICE,   SNLIQ, & ! IN/OUT :
                                 TG,     STC,   SMH2O,   TAUSS,  QSFC1D,          & ! IN/OUT :
                                FSA,     FSR,    FIRA,     FSH,    FGEV,   SSOIL, & ! OUT : 
                               TRAD,   ESOIL,   RUNSF,   RUNSB,     SAG,    SALB, & ! OUT :
                              QSNBOT,PONDING,PONDING1,PONDING2,    T2MB,    Q2MB, & ! OUT :
			      EMISSI,  FPICE,    CHB2 &                             ! OUT :
#ifdef WRF_HYDRO
                              , sfcheadrt(i,j)                                      &
#endif
                              )

         FSNO   = 1.0       
         TV     = undefined_value     ! Output from standard Noah-MP undefined for glacier points
         TGB    = TG 
         CANICE = undefined_value 
         CANLIQ = undefined_value 
         EAH    = undefined_value 
         TAH    = undefined_value
         FWET   = undefined_value 
         WSLAKE = undefined_value 
!         ZWT    = undefined_value 
         WA     = undefined_value 
         WT     = undefined_value 
         LFMASS = undefined_value 
         RTMASS = undefined_value 
         STMASS = undefined_value 
         WOOD   = undefined_value 
         GRAIN  = undefined_value
         GDD    = undefined_value
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
         RB     = undefined_value
         LAISUN = undefined_value
         LAISHA = undefined_value
         RS(I,J)= undefined_value
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
         Z0WRF  = 0.002
         QFX(I,J) = ESOIL
         LH (I,J) = FGEV


    ELSE
         ICE=0                              ! Neither sea ice or land ice.
         CALL NOAHMP_SFLX (parameters, &
            I       , J       , LAT     , YEARLEN , JULIAN  , COSZ    , & ! IN : Time/Space-related
            DT      , DX      , DZ8W1D  , NSOIL   , ZSOIL   , NSNOW   , & ! IN : Model configuration 
            FVEG    , FVGMAX  , VEGTYP  , ICE     , IST     , CROPTYPE, & ! IN : Vegetation/Soil characteristics
            SMCEQ   ,                                                   & ! IN : Vegetation/Soil characteristics
            T_ML    , P_ML    , PSFC    , U_ML    , V_ML    , Q_ML    , & ! IN : Forcing
            QC      , SWDN    , LWDN    ,                               & ! IN : Forcing
	    PRCPCONV, PRCPNONC, PRCPSHCV, PRCPSNOW, PRCPGRPL, PRCPHAIL, & ! IN : Forcing
            TBOT    , CO2PP   , O2PP    , FOLN    , FICEOLD , Z_ML    , & ! IN : Forcing
            ALBOLD  , SNEQVO  ,                                         & ! IN/OUT : 
            STC     , SMH2O   , SMC     , TAH     , EAH     , FWET    , & ! IN/OUT : 
            CANLIQ  , CANICE  , TV      , TG      , QSFC1D  , QSNOW   , & ! IN/OUT : 
            ISNOW   , ZSNSO   , SNDPTH  , SWE     , SNICE   , SNLIQ   , & ! IN/OUT : 
            ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & ! IN/OUT : 
            STMASS  , WOOD    , STBLCP  , FASTCP  , PLAI    , PSAI    , & ! IN/OUT : 
            CM      , CH      , TAUSS   ,                               & ! IN/OUT : 
            GRAIN   , GDD     , PGS     ,                               & ! IN/OUT 
            SMCWTD  ,DEEPRECH , RECH    ,                               & ! IN/OUT :
            GECROS1D,                                                   & ! IN/OUT :
            Z0WRF   ,                                                   &
            FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV    , & ! OUT : 
            FGEV    , FCTR    , ECAN    , ETRAN   , ESOIL   , TRAD    , & ! OUT : 
            TGB     , TGV     , T2MV    , T2MB    , Q2MV    , Q2MB    , & ! OUT : 
            RUNSF   , RUNSB   , APAR    , PSN     , SAV     , SAG     , & ! OUT : 
            FSNO    , NEE     , GPP     , NPP     , FVEGMP  , SALB    , & ! OUT : 
            QSNBOT  , PONDING , PONDING1, PONDING2, RSSUN   , RSSHA   , & ! OUT : 
            BGAP    , WGAP    , CHV     , CHB     , EMISSI  ,           & ! OUT : 
            SHG     , SHC     , SHB     , EVG     , EVB     , GHV     , & ! OUT :
	    GHB     , IRG     , IRC     , IRB     , TR      , EVC     , & ! OUT :
	    CHLEAF  , CHUC    , CHV2    , CHB2    , FPICE   , PAHV    , & 
            PAHG    , PAHB    , PAH     , LAISUN  , LAISHA  , RB        &
#ifdef WRF_HYDRO
            , sfcheadrt(i,j)                               &
#endif
            )            ! OUT :
                  
            QFX(I,J) = ECAN + ESOIL + ETRAN
            LH       (I,J)                = FCEV + FGEV + FCTR

   ENDIF ! glacial split ends 

#ifdef WRF_HYDRO
!AD_CHANGE: Glacier cells can produce small negative subsurface runoff for mass balance.
!           This will crash channel routing, so only pass along positive runoff.
            soldrain(i,j) = max(RUNSB*dt, 0.)        !mm , underground runoff
            INFXSRT(i,j) = RUNSF*dt        !mm , surface runoff
#endif


! INPUT/OUTPUT

             TSK      (I,J)                = TRAD
             HFX      (I,J)                = FSH
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
             ACSNOW   (I,J)                = ACSNOW(I,J) +  PRECIP_IN(I,J) * FPICE
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

             Z0       (I,J)                = Z0WRF
             ZNT      (I,J)                = Z0WRF
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
             LAISUN                        = MAX(LAISUN, 0.0)
             LAISHA                        = MAX(LAISHA, 0.0)
             RB                            = MAX(RB, 0.0)
! New Calculation of total Canopy/Stomatal Conductance Based on Bonan et al. (2011) 
! -- Inverse of Canopy Resistance (below)
             IF(RSSUN .le. 0.0 .and. RSSHA .le. 0.0) THEN
                RS    (I,J)                = 0.0
             ELSE
                RS    (I,J)                = ((1.0/(RSSUN+RB)*LAISUN) + ((1.0/(RSSHA+RB))*LAISHA))
                RS    (I,J)                = 1.0/RS(I,J) !Resistance
             ENDIF
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
             RECHXY   (I,J)                = RECHXY(I,J) + RECH*1.E3 !RECHARGE TO THE WATER TABLE
             DEEPRECHXY(I,J)               = DEEPRECHXY(I,J) + DEEPRECH
             SMCWTDXY(I,J)                 = SMCWTD

             GRAINXY  (I,J) = GRAIN !GRAIN XING
             GDDXY    (I,J) = GDD   !XING 
	     PGSXY    (I,J) = PGS

             if(iopt_crop == 2) then   ! gecros crop model

               !*** Check for harvest
               if ((gecros1d(1) >= gecros_ds1).and.(gecros1d(42) < 0)) then   
                 if (checkIfHarvest(gecros_state, DT, gecros_ds1, gecros_ds2, gecros_ds1x, &
                     gecros_ds2x) == 1) then
           
                   call gecros_reinit(gecros1d)
                 endif
               endif                  

               gecros_state (i,1:60,j)     = gecros1d(1:60)
             end if 
 
          ENDIF                                                         ! endif of land-sea test

      ENDDO ILOOP                                                       ! of I loop
   ENDDO JLOOP                                                          ! of J loop

!------------------------------------------------------
  END SUBROUTINE noahmplsm
!------------------------------------------------------

SUBROUTINE TRANSFER_MP_PARAMETERS(VEGTYPE,SOILTYPE,SLOPETYPE,SOILCOLOR,CROPTYPE,parameters)

  USE NOAHMP_TABLES
  USE MODULE_SF_NOAHMPLSM

  implicit none

  INTEGER, INTENT(IN)    :: VEGTYPE
  INTEGER, INTENT(IN)    :: SOILTYPE(4)
  INTEGER, INTENT(IN)    :: SLOPETYPE
  INTEGER, INTENT(IN)    :: SOILCOLOR
  INTEGER, INTENT(IN)    :: CROPTYPE
    
  type (noahmp_parameters), intent(inout) :: parameters
    
  REAL    :: REFDK
  REAL    :: REFKDT
  REAL    :: FRZK
  REAL    :: FRZFACT
  INTEGER :: ISOIL

  parameters%ISWATER   =   ISWATER_TABLE
  parameters%ISBARREN  =  ISBARREN_TABLE
  parameters%ISICE     =     ISICE_TABLE
  parameters%ISCROP    =    ISCROP_TABLE
  parameters%EBLFOREST = EBLFOREST_TABLE

  parameters%URBAN_FLAG = .FALSE.
  IF( VEGTYPE == ISURBAN_TABLE                  .or. VEGTYPE == LOW_DENSITY_RESIDENTIAL_TABLE  .or. &
      VEGTYPE == HIGH_DENSITY_RESIDENTIAL_TABLE .or. VEGTYPE == HIGH_INTENSITY_INDUSTRIAL_TABLE ) THEN
     parameters%URBAN_FLAG = .TRUE.
  ENDIF

!------------------------------------------------------------------------------------------!
! Transfer veg parameters
!------------------------------------------------------------------------------------------!

  parameters%CH2OP  =  CH2OP_TABLE(VEGTYPE)       !maximum intercepted h2o per unit lai+sai (mm)
  parameters%DLEAF  =  DLEAF_TABLE(VEGTYPE)       !characteristic leaf dimension (m)
  parameters%Z0MVT  =  Z0MVT_TABLE(VEGTYPE)       !momentum roughness length (m)
  parameters%HVT    =    HVT_TABLE(VEGTYPE)       !top of canopy (m)
  parameters%HVB    =    HVB_TABLE(VEGTYPE)       !bottom of canopy (m)
  parameters%DEN    =    DEN_TABLE(VEGTYPE)       !tree density (no. of trunks per m2)
  parameters%RC     =     RC_TABLE(VEGTYPE)       !tree crown radius (m)
  parameters%MFSNO  =  MFSNO_TABLE(VEGTYPE)       !snowmelt m parameter ()
  parameters%SAIM   =   SAIM_TABLE(VEGTYPE,:)     !monthly stem area index, one-sided
  parameters%LAIM   =   LAIM_TABLE(VEGTYPE,:)     !monthly leaf area index, one-sided
  parameters%SLA    =    SLA_TABLE(VEGTYPE)       !single-side leaf area per Kg [m2/kg]
  parameters%DILEFC = DILEFC_TABLE(VEGTYPE)       !coeficient for leaf stress death [1/s]
  parameters%DILEFW = DILEFW_TABLE(VEGTYPE)       !coeficient for leaf stress death [1/s]
  parameters%FRAGR  =  FRAGR_TABLE(VEGTYPE)       !fraction of growth respiration  !original was 0.3 
  parameters%LTOVRC = LTOVRC_TABLE(VEGTYPE)       !leaf turnover [1/s]

  parameters%C3PSN  =  C3PSN_TABLE(VEGTYPE)       !photosynthetic pathway: 0. = c4, 1. = c3
  parameters%KC25   =   KC25_TABLE(VEGTYPE)       !co2 michaelis-menten constant at 25c (pa)
  parameters%AKC    =    AKC_TABLE(VEGTYPE)       !q10 for kc25
  parameters%KO25   =   KO25_TABLE(VEGTYPE)       !o2 michaelis-menten constant at 25c (pa)
  parameters%AKO    =    AKO_TABLE(VEGTYPE)       !q10 for ko25
  parameters%VCMX25 = VCMX25_TABLE(VEGTYPE)       !maximum rate of carboxylation at 25c (umol co2/m**2/s)
  parameters%AVCMX  =  AVCMX_TABLE(VEGTYPE)       !q10 for vcmx25
  parameters%BP     =     BP_TABLE(VEGTYPE)       !minimum leaf conductance (umol/m**2/s)
  parameters%MP     =     MP_TABLE(VEGTYPE)       !slope of conductance-to-photosynthesis relationship
  parameters%QE25   =   QE25_TABLE(VEGTYPE)       !quantum efficiency at 25c (umol co2 / umol photon)
  parameters%AQE    =    AQE_TABLE(VEGTYPE)       !q10 for qe25
  parameters%RMF25  =  RMF25_TABLE(VEGTYPE)       !leaf maintenance respiration at 25c (umol co2/m**2/s)
  parameters%RMS25  =  RMS25_TABLE(VEGTYPE)       !stem maintenance respiration at 25c (umol co2/kg bio/s)
  parameters%RMR25  =  RMR25_TABLE(VEGTYPE)       !root maintenance respiration at 25c (umol co2/kg bio/s)
  parameters%ARM    =    ARM_TABLE(VEGTYPE)       !q10 for maintenance respiration
  parameters%FOLNMX = FOLNMX_TABLE(VEGTYPE)       !foliage nitrogen concentration when f(n)=1 (%)
  parameters%TMIN   =   TMIN_TABLE(VEGTYPE)       !minimum temperature for photosynthesis (k)

  parameters%XL     =     XL_TABLE(VEGTYPE)       !leaf/stem orientation index
  parameters%RHOL   =   RHOL_TABLE(VEGTYPE,:)     !leaf reflectance: 1=vis, 2=nir
  parameters%RHOS   =   RHOS_TABLE(VEGTYPE,:)     !stem reflectance: 1=vis, 2=nir
  parameters%TAUL   =   TAUL_TABLE(VEGTYPE,:)     !leaf transmittance: 1=vis, 2=nir
  parameters%TAUS   =   TAUS_TABLE(VEGTYPE,:)     !stem transmittance: 1=vis, 2=nir

  parameters%MRP    =    MRP_TABLE(VEGTYPE)       !microbial respiration parameter (umol co2 /kg c/ s)
  parameters%CWPVT  =  CWPVT_TABLE(VEGTYPE)       !empirical canopy wind parameter

  parameters%WRRAT  =  WRRAT_TABLE(VEGTYPE)       !wood to non-wood ratio
  parameters%WDPOOL = WDPOOL_TABLE(VEGTYPE)       !wood pool (switch 1 or 0) depending on woody or not [-]
  parameters%TDLEF  =  TDLEF_TABLE(VEGTYPE)       !characteristic T for leaf freezing [K]

  parameters%NROOT  =  NROOT_TABLE(VEGTYPE)       !number of soil layers with root present
  parameters%RGL    =    RGL_TABLE(VEGTYPE)       !Parameter used in radiation stress function
  parameters%RSMIN  =     RS_TABLE(VEGTYPE)       !Minimum stomatal resistance [s m-1]
  parameters%HS     =     HS_TABLE(VEGTYPE)       !Parameter used in vapor pressure deficit function
  parameters%TOPT   =   TOPT_TABLE(VEGTYPE)       !Optimum transpiration air temperature [K]
  parameters%RSMAX  =  RSMAX_TABLE(VEGTYPE)       !Maximal stomatal resistance [s m-1]

!------------------------------------------------------------------------------------------!
! Transfer rad parameters
!------------------------------------------------------------------------------------------!

   parameters%ALBSAT    = ALBSAT_TABLE(SOILCOLOR,:)
   parameters%ALBDRY    = ALBDRY_TABLE(SOILCOLOR,:)
   parameters%ALBICE    = ALBICE_TABLE
   parameters%ALBLAK    = ALBLAK_TABLE               
   parameters%OMEGAS    = OMEGAS_TABLE
   parameters%BETADS    = BETADS_TABLE
   parameters%BETAIS    = BETAIS_TABLE
   parameters%EG        = EG_TABLE

!------------------------------------------------------------------------------------------!
! Transfer crop parameters
!------------------------------------------------------------------------------------------!

  IF(CROPTYPE > 0) THEN
   parameters%PLTDAY    =    PLTDAY_TABLE(CROPTYPE)    ! Planting date
   parameters%HSDAY     =     HSDAY_TABLE(CROPTYPE)    ! Harvest date
   parameters%PLANTPOP  =  PLANTPOP_TABLE(CROPTYPE)    ! Plant density [per ha] - used?
   parameters%IRRI      =      IRRI_TABLE(CROPTYPE)    ! Irrigation strategy 0= non-irrigation 1=irrigation (no water-stress)
   parameters%GDDTBASE  =  GDDTBASE_TABLE(CROPTYPE)    ! Base temperature for GDD accumulation [C]
   parameters%GDDTCUT   =   GDDTCUT_TABLE(CROPTYPE)    ! Upper temperature for GDD accumulation [C]
   parameters%GDDS1     =     GDDS1_TABLE(CROPTYPE)    ! GDD from seeding to emergence
   parameters%GDDS2     =     GDDS2_TABLE(CROPTYPE)    ! GDD from seeding to initial vegetative 
   parameters%GDDS3     =     GDDS3_TABLE(CROPTYPE)    ! GDD from seeding to post vegetative 
   parameters%GDDS4     =     GDDS4_TABLE(CROPTYPE)    ! GDD from seeding to intial reproductive
   parameters%GDDS5     =     GDDS5_TABLE(CROPTYPE)    ! GDD from seeding to pysical maturity 
   parameters%C3C4      =      C3C4_TABLE(CROPTYPE)    ! photosynthetic pathway:  1. = c3 2. = c4
   parameters%AREF      =      AREF_TABLE(CROPTYPE)    ! reference maximum CO2 assimulation rate 
   parameters%PSNRF     =     PSNRF_TABLE(CROPTYPE)    ! CO2 assimulation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
   parameters%I2PAR     =     I2PAR_TABLE(CROPTYPE)    ! Fraction of incoming solar radiation to photosynthetically active radiation
   parameters%TASSIM0   =   TASSIM0_TABLE(CROPTYPE)    ! Minimum temperature for CO2 assimulation [C]
   parameters%TASSIM1   =   TASSIM1_TABLE(CROPTYPE)    ! CO2 assimulation linearly increasing until temperature reaches T1 [C]
   parameters%TASSIM2   =   TASSIM2_TABLE(CROPTYPE)    ! CO2 assmilation rate remain at Aref until temperature reaches T2 [C]
   parameters%K         =         K_TABLE(CROPTYPE)    ! light extinction coefficient
   parameters%EPSI      =      EPSI_TABLE(CROPTYPE)    ! initial light use efficiency
   parameters%Q10MR     =     Q10MR_TABLE(CROPTYPE)    ! q10 for maintainance respiration
   parameters%FOLN_MX   =   FOLN_MX_TABLE(CROPTYPE)    ! foliage nitrogen concentration when f(n)=1 (%)
   parameters%LEFREEZ   =   LEFREEZ_TABLE(CROPTYPE)    ! characteristic T for leaf freezing [K]
   parameters%DILE_FC   =   DILE_FC_TABLE(CROPTYPE,:)  ! coeficient for temperature leaf stress death [1/s]
   parameters%DILE_FW   =   DILE_FW_TABLE(CROPTYPE,:)  ! coeficient for water leaf stress death [1/s]
   parameters%FRA_GR    =    FRA_GR_TABLE(CROPTYPE)    ! fraction of growth respiration
   parameters%LF_OVRC   =   LF_OVRC_TABLE(CROPTYPE,:)  ! fraction of leaf turnover  [1/s]
   parameters%ST_OVRC   =   ST_OVRC_TABLE(CROPTYPE,:)  ! fraction of stem turnover  [1/s]
   parameters%RT_OVRC   =   RT_OVRC_TABLE(CROPTYPE,:)  ! fraction of root tunrover  [1/s]
   parameters%LFMR25    =    LFMR25_TABLE(CROPTYPE)    ! leaf maintenance respiration at 25C [umol CO2/m**2  /s]
   parameters%STMR25    =    STMR25_TABLE(CROPTYPE)    ! stem maintenance respiration at 25C [umol CO2/kg bio/s]
   parameters%RTMR25    =    RTMR25_TABLE(CROPTYPE)    ! root maintenance respiration at 25C [umol CO2/kg bio/s]
   parameters%GRAINMR25 = GRAINMR25_TABLE(CROPTYPE)    ! grain maintenance respiration at 25C [umol CO2/kg bio/s]
   parameters%LFPT      =      LFPT_TABLE(CROPTYPE,:)  ! fraction of carbohydrate flux to leaf
   parameters%STPT      =      STPT_TABLE(CROPTYPE,:)  ! fraction of carbohydrate flux to stem
   parameters%RTPT      =      RTPT_TABLE(CROPTYPE,:)  ! fraction of carbohydrate flux to root
   parameters%GRAINPT   =   GRAINPT_TABLE(CROPTYPE,:)  ! fraction of carbohydrate flux to grain
   parameters%BIO2LAI   =   BIO2LAI_TABLE(CROPTYPE)    ! leaf are per living leaf biomass [m^2/kg]
  END IF

!------------------------------------------------------------------------------------------!
! Transfer global parameters
!------------------------------------------------------------------------------------------!

   parameters%CO2        =         CO2_TABLE
   parameters%O2         =          O2_TABLE
   parameters%TIMEAN     =      TIMEAN_TABLE
   parameters%FSATMX     =      FSATMX_TABLE
   parameters%Z0SNO      =       Z0SNO_TABLE
   parameters%SSI        =         SSI_TABLE
   parameters%SWEMX      =       SWEMX_TABLE
   parameters%RSURF_SNOW =  RSURF_SNOW_TABLE

! ----------------------------------------------------------------------
!  Transfer soil parameters
! ----------------------------------------------------------------------

    do isoil = 1, size(soiltype)
      parameters%BEXP(isoil)   = BEXP_TABLE   (SOILTYPE(isoil))
      parameters%DKSAT(isoil)  = DKSAT_TABLE  (SOILTYPE(isoil))
      parameters%DWSAT(isoil)  = DWSAT_TABLE  (SOILTYPE(isoil))
      parameters%PSISAT(isoil) = PSISAT_TABLE (SOILTYPE(isoil))
      parameters%QUARTZ(isoil) = QUARTZ_TABLE (SOILTYPE(isoil))
      parameters%SMCDRY(isoil) = SMCDRY_TABLE (SOILTYPE(isoil))
      parameters%SMCMAX(isoil) = SMCMAX_TABLE (SOILTYPE(isoil))
      parameters%SMCREF(isoil) = SMCREF_TABLE (SOILTYPE(isoil))
      parameters%SMCWLT(isoil) = SMCWLT_TABLE (SOILTYPE(isoil))
    end do
    
    parameters%F1     = F1_TABLE(SOILTYPE(1))
    parameters%REFDK  = REFDK_TABLE
    parameters%REFKDT = REFKDT_TABLE

! ----------------------------------------------------------------------
! Transfer GENPARM parameters
! ----------------------------------------------------------------------
    parameters%CSOIL  = CSOIL_TABLE
    parameters%ZBOT   = ZBOT_TABLE
    parameters%CZIL   = CZIL_TABLE

    FRZK   = FRZK_TABLE
    parameters%KDT    = parameters%REFKDT * parameters%DKSAT(1) / parameters%REFDK
    parameters%SLOPE  = SLOPE_TABLE(SLOPETYPE)

    IF(parameters%URBAN_FLAG)THEN  ! Hardcoding some urban parameters for soil
       parameters%SMCMAX = 0.45 
       parameters%SMCREF = 0.42 
       parameters%SMCWLT = 0.40 
       parameters%SMCDRY = 0.40 
       parameters%CSOIL  = 3.E6
    ENDIF

! adjust FRZK parameter to actual soil type: FRZK * FRZFACT

    IF(SOILTYPE(1) /= 14) then
      FRZFACT = (parameters%SMCMAX(1) / parameters%SMCREF(1)) * (0.412 / 0.468)
      parameters%FRZX = FRZK * FRZFACT
    END IF

 END SUBROUTINE TRANSFER_MP_PARAMETERS

SUBROUTINE PEDOTRANSFER_SR2006(nsoil,sand,clay,orgm,parameters)

  use module_sf_noahmplsm
  use noahmp_tables
        
  integer,                    intent(in   ) :: nsoil     ! number of soil layers
  real, dimension( 1:nsoil ), intent(inout) :: sand
  real, dimension( 1:nsoil ), intent(inout) :: clay
  real, dimension( 1:nsoil ), intent(inout) :: orgm
    
  real, dimension( 1:nsoil ) :: theta_1500t
  real, dimension( 1:nsoil ) :: theta_1500
  real, dimension( 1:nsoil ) :: theta_33t
  real, dimension( 1:nsoil ) :: theta_33
  real, dimension( 1:nsoil ) :: theta_s33t
  real, dimension( 1:nsoil ) :: theta_s33
  real, dimension( 1:nsoil ) :: psi_et
  real, dimension( 1:nsoil ) :: psi_e
    
  type(noahmp_parameters), intent(inout) :: parameters

  do k = 1,4
    if(sand(k) <= 0 .or. clay(k) <= 0) then
      sand(k) = 0.41
      clay(k) = 0.18
    end if
    if(orgm(k) <= 0 ) orgm(k) = 0.0
  end do
        
  theta_1500t =   sr2006_theta_1500t_a*sand       &
                + sr2006_theta_1500t_b*clay       &
                + sr2006_theta_1500t_c*orgm       &
                + sr2006_theta_1500t_d*sand*orgm  &
                + sr2006_theta_1500t_e*clay*orgm  &
                + sr2006_theta_1500t_f*sand*clay  &
                + sr2006_theta_1500t_g

  theta_1500  =   theta_1500t                      &
                + sr2006_theta_1500_a*theta_1500t  &
                + sr2006_theta_1500_b

  theta_33t   =   sr2006_theta_33t_a*sand       &
                + sr2006_theta_33t_b*clay       &
                + sr2006_theta_33t_c*orgm       &
                + sr2006_theta_33t_d*sand*orgm  &
                + sr2006_theta_33t_e*clay*orgm  &
                + sr2006_theta_33t_f*sand*clay  &
                + sr2006_theta_33t_g

  theta_33    =   theta_33t                              &
                + sr2006_theta_33_a*theta_33t*theta_33t  &
                + sr2006_theta_33_b*theta_33t            &
                + sr2006_theta_33_c

  theta_s33t  =   sr2006_theta_s33t_a*sand      &
                + sr2006_theta_s33t_b*clay      &
                + sr2006_theta_s33t_c*orgm      &
                + sr2006_theta_s33t_d*sand*orgm &
                + sr2006_theta_s33t_e*clay*orgm &
                + sr2006_theta_s33t_f*sand*clay &
                + sr2006_theta_s33t_g

  theta_s33   = theta_s33t                       &
                + sr2006_theta_s33_a*theta_s33t  &
                + sr2006_theta_s33_b

  psi_et      =   sr2006_psi_et_a*sand           &
                + sr2006_psi_et_b*clay           &
                + sr2006_psi_et_c*theta_s33      &
                + sr2006_psi_et_d*sand*theta_s33 &
                + sr2006_psi_et_e*clay*theta_s33 &
                + sr2006_psi_et_f*sand*clay      &
                + sr2006_psi_et_g
 
  psi_e       =   psi_et                        &
                + sr2006_psi_e_a*psi_et*psi_et  &
                + sr2006_psi_e_b*psi_et         &
                + sr2006_psi_e_c
    
  parameters%smcwlt = theta_1500
  parameters%smcref = theta_33
  parameters%smcmax =   theta_33    &
                      + theta_s33            &
                      + sr2006_smcmax_a*sand &
                      + sr2006_smcmax_b

  parameters%bexp   = 3.816712826 / (log(theta_33) - log(theta_1500) )
  parameters%psisat = psi_e
  parameters%dksat  = 1930.0 * (parameters%smcmax - theta_33) ** (3.0 - 1.0/parameters%bexp)
  parameters%quartz = sand
    
! Units conversion
    
  parameters%psisat = max(0.1,parameters%psisat)     ! arbitrarily impose a limit of 0.1kpa
  parameters%psisat = 0.101997 * parameters%psisat   ! convert kpa to m
  parameters%dksat  = parameters%dksat / 3600000.0   ! convert mm/h to m/s
  parameters%dwsat  = parameters%dksat * parameters%psisat *parameters%bexp / parameters%smcmax  ! units should be m*m/s
  parameters%smcdry = parameters%smcwlt
  
! Introducing somewhat arbitrary limits (based on SOILPARM) to prevent bad things
  
  parameters%smcmax = max(0.32 ,min(parameters%smcmax,             0.50 ))
  parameters%smcref = max(0.17 ,min(parameters%smcref,parameters%smcmax ))
  parameters%smcwlt = max(0.01 ,min(parameters%smcwlt,parameters%smcref ))
  parameters%smcdry = max(0.01 ,min(parameters%smcdry,parameters%smcref ))
  parameters%bexp   = max(2.50 ,min(parameters%bexp,               12.0 ))
  parameters%psisat = max(0.03 ,min(parameters%psisat,             1.00 ))
  parameters%dksat  = max(5.e-7,min(parameters%dksat,              1.e-5))
  parameters%dwsat  = max(1.e-6,min(parameters%dwsat,              3.e-5))
  parameters%quartz = max(0.05 ,min(parameters%quartz,             0.95 ))
    
 END SUBROUTINE PEDOTRANSFER_SR2006

  SUBROUTINE NOAHMP_INIT ( MMINLU, SNOW , SNOWH , CANWAT , ISLTYP ,   IVGTYP, XLAT, &
       TSLB , SMOIS , SH2O , DZS , FNDSOILW , FNDSNOWH ,             &
       TSK, isnowxy , tvxy     ,tgxy     ,canicexy ,         TMN,     XICE,   &
       canliqxy ,eahxy    ,tahxy    ,cmxy     ,chxy     ,                     &
       fwetxy   ,sneqvoxy ,alboldxy ,qsnowxy  ,wslakexy ,zwtxy    ,waxy     , &
       wtxy     ,tsnoxy   ,zsnsoxy  ,snicexy  ,snliqxy  ,lfmassxy ,rtmassxy , &
       stmassxy ,woodxy   ,stblcpxy ,fastcpxy ,xsaixy   ,lai      ,           &
       grainxy  ,gddxy    ,                                                   &
       croptype ,cropcat  ,                      &
!jref:start
       t2mvxy   ,t2mbxy   ,chstarxy,            &
!jref:end       
       NSOIL, restart,                 &
       allowed_to_read , iopt_run,  iopt_crop,                        &
       sf_urban_physics,                         &  ! urban scheme
       ids,ide, jds,jde, kds,kde,                &
       ims,ime, jms,jme, kms,kme,                &
       its,ite, jts,jte, kts,kte,                &
       smoiseq  ,smcwtdxy ,rechxy   ,deeprechxy, areaxy, dx, dy, msftx, msfty,&     ! Optional groundwater
       wtddt    ,stepwtd  ,dt       ,qrfsxy     ,qspringsxy  , qslatxy    ,  &      ! Optional groundwater
       fdepthxy ,ht     ,riverbedxy ,eqzwt     ,rivercondxy ,pexpxy       ,  &      ! Optional groundwater
       rechclim,                                                             &      ! Optional groundwater
       gecros_state)                                                                ! Optional gecros crop

  USE NOAHMP_TABLES
  use module_sf_gecros, only: seednc,sla0,slnmin,ffat,flig,foac,fmin,npl,seedw,eg,fcrsh,seednc,lnci,cfv


  IMPLICIT NONE

! Initializing Canopy air temperature to 287 K seems dangerous to me [KWM].

    INTEGER, INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde,  &
         &                           ims,ime, jms,jme, kms,kme,  &
         &                           its,ite, jts,jte, kts,kte

    INTEGER, INTENT(IN)       ::     NSOIL, iopt_run, iopt_crop

    LOGICAL, INTENT(IN)       ::     restart,                    &
         &                           allowed_to_read
    INTEGER, INTENT(IN)       ::     sf_urban_physics                              ! urban, by yizhou

    REAL,    DIMENSION( NSOIL), INTENT(IN)    ::     DZS  ! Thickness of the soil layers [m]
    REAL,    INTENT(IN) , OPTIONAL ::     DX, DY
    REAL,    DIMENSION( ims:ime, jms:jme ) ,  INTENT(IN) , OPTIONAL :: MSFTX,MSFTY

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

    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: XLAT         !latitude
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
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: grainxy     !mass of grain [g/m2] !XING
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: gddxy       !growing degree days !XING
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stblcpxy    !stable carbon in deep soil [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fastcpxy    !short-lived carbon, shallow soil [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: xsaixy      !stem area index
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: lai         !leaf area index

    INTEGER, DIMENSION(ims:ime,  jms:jme), INTENT(OUT) :: cropcat
    REAL   , DIMENSION(ims:ime,5,jms:jme), INTENT(IN ) :: croptype

! IOPT_RUN = 5 option

    REAL, DIMENSION(ims:ime,1:nsoil,jms:jme), INTENT(INOUT) , OPTIONAL :: smoiseq !equilibrium soil moisture content [m3m-3]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: smcwtdxy    !deep soil moisture content [m3m-3]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: deeprechxy  !deep recharge [m]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: rechxy      !accumulated recharge [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: qrfsxy      !accumulated flux from groundwater to rivers [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: qspringsxy  !accumulated seeping water [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: qslatxy     !accumulated lateral flow [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: areaxy      !grid cell area [m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: FDEPTHXY    !efolding depth for transmissivity (m)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: HT          !terrain height (m)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: RIVERBEDXY  !riverbed depth (m)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: EQZWT       !equilibrium water table depth (m)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT), OPTIONAL :: RIVERCONDXY !river conductance
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT), OPTIONAL :: PEXPXY      !factor for river conductance
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: rechclim

    REAL, DIMENSION(ims:ime,60,jms:jme), INTENT(INOUT),   OPTIONAL :: gecros_state                                     ! Optional gecros crop

    INTEGER,  INTENT(OUT) , OPTIONAL :: STEPWTD
    REAL, INTENT(IN) , OPTIONAL :: DT, WTDDT

!jref:start
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: t2mvxy        !2m temperature vegetation part (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: t2mbxy        !2m temperature bare ground part (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chstarxy        !dummy
!jref:end


    REAL, DIMENSION(1:NSOIL)  :: ZSOIL      ! Depth of the soil layer bottom (m) from 
    !                                                   the surface (negative)

    REAL                      :: BEXP, SMCMAX, PSISAT
    REAL                      :: FK, masslai,masssai
    
! gecros local variables
    REAL ::  hti,rdi,fpro,lncmin,fcar,cfo,clvi,crti,ygo,nlvi,laii,nrti,slnbi


    REAL, PARAMETER           :: BLIM  = 5.5
    REAL, PARAMETER           :: HLICE = 3.335E5
    REAL, PARAMETER           :: GRAV = 9.81
    REAL, PARAMETER           :: T0 = 273.15

    INTEGER                   :: errflag, i,j,itf,jtf,ns

    character(len=240) :: err_message
    character(len=4)  :: MMINSL
    character(len=*), intent(in) :: MMINLU
    MMINSL='STAS'

    call read_mp_veg_parameters(trim(MMINLU))
    call read_mp_soil_parameters()
    call read_mp_rad_parameters()
    call read_mp_global_parameters()
    call read_mp_crop_parameters()
    call read_mp_optional_parameters()

    IF( .NOT. restart ) THEN

       itf=min0(ite,ide-1)
       jtf=min0(jte,jde-1)

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


       ! Check if snow/snowh are consistent and cap SWE at 2000mm;
       !  the Noah-MP code does it internally but if we don't do it here, problems ensue
       DO J = jts,jtf
          DO I = its,itf
             IF ( SNOW(i,j) > 0. .AND. SNOWH(i,j) == 0. .OR. SNOWH(i,j) > 0. .AND. SNOW(i,j) == 0.) THEN
               WRITE(err_message,*)"problem with initial snow fields: snow/snowh>0 while snowh/snow=0 at i,j" &
                                     ,i,j,snow(i,j),snowh(i,j)
               CALL wrf_message(err_message)
             ENDIF
             IF ( SNOW( i,j ) > 2000. ) THEN
               SNOWH(I,J) = SNOWH(I,J) * 2000. / SNOW(I,J)      ! SNOW in mm and SNOWH in m
               SNOW (I,J) = 2000.                               ! cap SNOW at 2000, maintain density
             ENDIF
          ENDDO
       ENDDO

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
! GAC-->LATERALFLOW
! 20130219 - No longer need this - see module_data_gocart_dust
!#if ( WRF_CHEM == 1 )
!       !
!       ! need this parameter for dust parameterization in wrf/chem
!       !
!       do I=1,NSLTYPE
!          porosity(i)=maxsmc(i)
!       enddo
!#endif
! <--GAC

! initialize soil liquid water content SH2O

       DO J = jts , jtf
          DO I = its , itf
	    IF(IVGTYP(I,J)==ISICE_TABLE .AND. XICE(I,J) <= 0.0) THEN
              DO NS=1, NSOIL
	        SMOIS(I,NS,J) = 1.0                     ! glacier starts all frozen
	        SH2O(I,NS,J) = 0.0
	        TSLB(I,NS,J) = MIN(TSLB(I,NS,J),263.15) ! set glacier temp to at most -10C
              END DO
	        !TMN(I,J) = MIN(TMN(I,J),263.15)         ! set deep temp to at most -10C
		SNOW(I,J) = MAX(SNOW(I,J), 10.0)        ! set SWE to at least 10mm
                SNOWH(I,J)=SNOW(I,J)*0.01               ! SNOW in mm and SNOWH in m
	    ELSE
	      
              BEXP   =   BEXP_TABLE(ISLTYP(I,J))
              SMCMAX = SMCMAX_TABLE(ISLTYP(I,J))
              PSISAT = -1.0*PSISAT_TABLE(ISLTYP(I,J))

              DO NS=1, NSOIL
	        IF ( SMOIS(I,NS,J) > SMCMAX )  SMOIS(I,NS,J) = SMCMAX
              END DO
              IF ( ( BEXP > 0.0 ) .AND. ( SMCMAX > 0.0 ) .AND. ( PSISAT > 0.0 ) ) THEN
                DO NS=1, NSOIL
                   IF ( TSLB(I,NS,J) < 273.149 ) THEN    ! Use explicit as initial soil ice
                      FK=(( (HLICE/(GRAV*(-PSISAT))) *                              &
                           ((TSLB(I,NS,J)-T0)/TSLB(I,NS,J)) )**(-1/BEXP) )*SMCMAX
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


       DO J = jts,jtf
          DO I = its,itf
             tvxy       (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tvxy(I,J) = 273.15
             tgxy       (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tgxy(I,J) = 273.15
             CANWAT     (I,J) = 0.0
             canliqxy   (I,J) = CANWAT(I,J)
             canicexy   (I,J) = 0.
             eahxy      (I,J) = 2000. 
             tahxy      (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tahxy(I,J) = 273.15
!             tahxy      (I,J) = 287.
!jref:start
             t2mvxy     (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) t2mvxy(I,J) = 273.15
             t2mbxy     (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) t2mbxy(I,J) = 273.15
             chstarxy     (I,J) = 0.1
!jref:end

             cmxy       (I,J) = 0.0
             chxy       (I,J) = 0.0
             fwetxy     (I,J) = 0.0
             sneqvoxy   (I,J) = 0.0
             alboldxy   (I,J) = 0.65
             qsnowxy    (I,J) = 0.0
             wslakexy   (I,J) = 0.0

             if(iopt_run.ne.5) then 
                   waxy       (I,J) = 4900.                                       !???
                   wtxy       (I,J) = waxy(i,j)                                   !???
                   zwtxy      (I,J) = (25. + 2.0) - waxy(i,j)/1000/0.2            !???
             else
                   waxy       (I,J) = 0.
                   wtxy       (I,J) = 0.
                   areaxy     (I,J) = (DX * DY) / ( MSFTX(I,J) * MSFTY(I,J) )
             endif

           IF(IVGTYP(I,J) == ISBARREN_TABLE .OR. IVGTYP(I,J) == ISICE_TABLE .OR. &
	      ( SF_URBAN_PHYSICS == 0 .AND. IVGTYP(I,J) == ISURBAN_TABLE )  .OR. &
	      IVGTYP(I,J) == ISWATER_TABLE ) THEN
	     
	     lai        (I,J) = 0.0
             xsaixy     (I,J) = 0.0
             lfmassxy   (I,J) = 0.0
             stmassxy   (I,J) = 0.0
             rtmassxy   (I,J) = 0.0
             woodxy     (I,J) = 0.0
             stblcpxy   (I,J) = 0.0
             fastcpxy   (I,J) = 0.0
             grainxy    (I,J) = 1E-10
             gddxy      (I,J) = 0
	     cropcat    (I,J) = 0

	   ELSE
	     
	     lai        (I,J) = max(lai(i,j),0.05)             ! at least start with 0.05 for arbitrary initialization (v3.7)
             xsaixy     (I,J) = max(0.1*lai(I,J),0.05)         ! MB: arbitrarily initialize SAI using input LAI (v3.7)
             masslai = 1000. / max(SLA_TABLE(IVGTYP(I,J)),1.0) ! conversion from lai to mass  (v3.7)
             lfmassxy   (I,J) = lai(i,j)*masslai               ! use LAI to initialize (v3.7)
             masssai = 1000. / 3.0                             ! conversion from lai to mass (v3.7)
             stmassxy   (I,J) = xsaixy(i,j)*masssai            ! use SAI to initialize (v3.7)
             rtmassxy   (I,J) = 500.0                          ! these are all arbitrary and probably should be
             woodxy     (I,J) = 500.0                          ! in the table or read from initialization
             stblcpxy   (I,J) = 1000.0                         !
             fastcpxy   (I,J) = 1000.0                         !
             grainxy    (I,J) = 1E-10
             gddxy      (I,J) = 0    

! Initialize crop for Liu crop model

	     if(iopt_crop == 1 ) then
	       cropcat    (i,j) = default_crop_table
               if(croptype(i,5,j) >= 0.5) then
                 rtmassxy(i,j) = 0.0
                 woodxy  (i,j) = 0.0                    

	         if(    croptype(i,1,j) > croptype(i,2,j) .and. &
		        croptype(i,1,j) > croptype(i,3,j) .and. &
		        croptype(i,1,j) > croptype(i,4,j) ) then   ! choose corn

		   cropcat (i,j) = 1
                   lfmassxy(i,j) =    lai(i,j)/0.035
                   stmassxy(i,j) = xsaixy(i,j)/0.003

	         elseif(croptype(i,2,j) > croptype(i,1,j) .and. &
		        croptype(i,2,j) > croptype(i,3,j) .and. &
		        croptype(i,2,j) > croptype(i,4,j) ) then   ! choose soybean

		   cropcat (i,j) = 2
                   lfmassxy(i,j) =    lai(i,j)/0.015
                   stmassxy(i,j) = xsaixy(i,j)/0.003

	         else

		   cropcat (i,j) = default_crop_table
                   lfmassxy(i,j) =    lai(i,j)/0.035
                   stmassxy(i,j) = xsaixy(i,j)/0.003

	         end if

	       end if
	     end if

! Initialize cropcat for gecros crop model

	     if(iopt_crop == 2) then
	       cropcat    (i,j) = 0
               if(croptype(i,5,j) >= 0.5) then
                  if(croptype(i,3,j) > 0.0)             cropcat(i,j) = 1 ! if any wheat, set to wheat
                  if(croptype(i,1,j) > croptype(i,3,j)) cropcat(i,j) = 2 ! change to maize
	       end if

               hti    = 0.01
               rdi    = 10.
               fpro   = 6.25*seednc
               lncmin = sla0*slnmin
               fcar   = 1.-fpro-ffat-flig-foac-fmin
               cfo    = 0.444*fcar+0.531*fpro+0.774*ffat+0.667*flig+0.368*foac
               clvi   = npl * seedw * cfo * eg * fcrsh
               crti   = npl * seedw * cfo * eg * (1.-fcrsh)
               ygo    = cfo/(1.275*fcar+1.887*fpro+3.189*ffat+2.231*flig+0.954* &
                        foac)*30./12.
               nlvi   = min(0.75 * npl * seedw * eg * seednc, lnci * clvi/cfv)
               laii   = clvi/cfv*sla0
               nrti   = npl * seedw * eg * seednc - nlvi
               slnbi  = nlvi/laii
    
               call gecros_init(xlat(i,j),hti,rdi,clvi,crti,nlvi,laii,nrti,slnbi,gecros_state(i,:,j))

             end if

	   END IF

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

       !initialize arrays for groundwater dynamics iopt_run=5

       if(iopt_run.eq.5) then
          IF ( PRESENT(smoiseq)     .AND. &
            PRESENT(smcwtdxy)    .AND. &
            PRESENT(rechxy)      .AND. &
            PRESENT(deeprechxy)  .AND. &
            PRESENT(areaxy)      .AND. &
            PRESENT(dx)          .AND. &
            PRESENT(dy)          .AND. &
            PRESENT(msftx)       .AND. &
            PRESENT(msfty)       .AND. &
            PRESENT(wtddt)       .AND. &
            PRESENT(stepwtd)     .AND. &
            PRESENT(dt)          .AND. &
            PRESENT(qrfsxy)      .AND. &
            PRESENT(qspringsxy)  .AND. &
            PRESENT(qslatxy)     .AND. &
            PRESENT(fdepthxy)    .AND. &
            PRESENT(ht)          .AND. &
            PRESENT(riverbedxy)  .AND. &
            PRESENT(eqzwt)       .AND. &
            PRESENT(rivercondxy) .AND. &
            PRESENT(pexpxy)      .AND. &
            PRESENT(rechclim)    ) THEN

             STEPWTD = nint(WTDDT*60./DT)
             STEPWTD = max(STEPWTD,1)

          ELSE
             CALL wrf_error_fatal ('Not enough fields to use groundwater option in Noah-MP')
          END IF
       endif

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
! ==================================================================================================
! ----------------------------------------------------------------------
    SUBROUTINE GROUNDWATER_INIT (   &
            &            GRID, NSOIL , DZS, ISLTYP, IVGTYP, WTDDT , &
            &            FDEPTH, TOPO, RIVERBED, EQWTD, RIVERCOND, PEXP , AREA ,WTD ,  &
            &            SMOIS,SH2O, SMOISEQ, SMCWTDXY, DEEPRECHXY, RECHXY ,  &
            &            QSLATXY, QRFSXY, QSPRINGSXY,                  &
            &            rechclim  ,                                   &
            &            ids,ide, jds,jde, kds,kde,                    &
            &            ims,ime, jms,jme, kms,kme,                    &
            &            ips,ipe, jps,jpe, kps,kpe,                    &
            &            its,ite, jts,jte, kts,kte                     )


  USE NOAHMP_TABLES, ONLY : BEXP_TABLE,SMCMAX_TABLE,PSISAT_TABLE,SMCWLT_TABLE,DWSAT_TABLE,DKSAT_TABLE, &
                                ISURBAN_TABLE, ISICE_TABLE ,ISWATER_TABLE
  USE module_sf_noahmp_groundwater, ONLY : LATERALFLOW
  USE module_domain, only: domain
#if (EM_CORE == 1)
#ifdef DM_PARALLEL
    USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks
    USE module_comm_dm , ONLY : halo_em_hydro_noahmp_sub
#endif
#endif

! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------

    INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
         &                           ims,ime, jms,jme, kms,kme,  &
         &                           ips,ipe, jps,jpe, kps,kpe,  &
         &                           its,ite, jts,jte, kts,kte
    TYPE(domain) , TARGET :: grid                             ! state
    INTEGER, INTENT(IN)                              :: NSOIL
    REAL,   INTENT(IN)                               ::     WTDDT
    REAL,    INTENT(IN), DIMENSION(1:NSOIL)          :: DZS
    INTEGER, INTENT(IN), DIMENSION(ims:ime, jms:jme) :: ISLTYP, IVGTYP
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: FDEPTH, TOPO , AREA
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: rechclim 
    REAL,    INTENT(OUT), DIMENSION(ims:ime, jms:jme) :: RIVERCOND
    REAL,    INTENT(INOUT), DIMENSION(ims:ime, jms:jme) :: WTD, RIVERBED, EQWTD, PEXP
    REAL,     DIMENSION( ims:ime , 1:nsoil, jms:jme ), &
         &    INTENT(INOUT)   ::                          SMOIS, &
         &                                                 SH2O, &
         &                                                 SMOISEQ
    REAL,    INTENT(INOUT), DIMENSION(ims:ime, jms:jme) ::  &
                                                           SMCWTDXY, &
                                                           DEEPRECHXY, &
                                                           RECHXY, &
                                                           QSLATXY, &
                                                           QRFSXY, &
                                                           QSPRINGSXY  
! local
    INTEGER  :: I,J,K,ITER,itf,jtf, NITER, NCOUNT,NS
    REAL :: BEXP,SMCMAX,PSISAT,SMCWLT,DWSAT,DKSAT
    REAL :: FRLIQ,SMCEQDEEP
    REAL :: DELTAT,RCOND,TOTWATER
    REAL :: AA,BBB,CC,DD,DX,FUNC,DFUNC,DDZ,EXPON,SMC,FLUX
    REAL, DIMENSION(1:NSOIL) :: SMCEQ,ZSOIL
    REAL,      DIMENSION( ims:ime, jms:jme )    :: QLAT, QRF
    INTEGER,   DIMENSION( ims:ime, jms:jme )    :: LANDMASK !-1 for water (ice or no ice) and glacial areas, 1 for land where the LSM does its soil moisture calculations

       ! Given the soil layer thicknesses (in DZS), calculate the soil layer
       ! depths from the surface.
       ZSOIL(1)         = -DZS(1)          ! negative
       DO NS=2, NSOIL
          ZSOIL(NS)       = ZSOIL(NS-1) - DZS(NS)
       END DO


       itf=min0(ite,ide-1)
       jtf=min0(jte,jde-1)


    WHERE(IVGTYP.NE.ISWATER_TABLE.AND.IVGTYP.NE.ISICE_TABLE)
         LANDMASK=1
    ELSEWHERE
         LANDMASK=-1
    ENDWHERE
    
    PEXP = 1.0

    DELTAT=365.*24*3600. !1 year

!readjust the raw aggregated water table from hires, so that it is better compatible with topography

!use WTD here, to use the lateral communication routine
    WTD=EQWTD

    NCOUNT=0

 DO NITER=1,500

#if (EM_CORE == 1)
#ifdef DM_PARALLEL
#     include "HALO_EM_HYDRO_NOAHMP.inc"
#endif
#endif

!Calculate lateral flow

IF(NCOUNT.GT.0.OR.NITER.eq.1)THEN
    QLAT = 0.
    CALL LATERALFLOW(ISLTYP,WTD,QLAT,FDEPTH,TOPO,LANDMASK,DELTAT,AREA       &
                        ,ids,ide,jds,jde,kds,kde                      & 
                        ,ims,ime,jms,jme,kms,kme                      &
                        ,its,ite,jts,jte,kts,kte                      )

    NCOUNT=0
    DO J=jts,jtf
       DO I=its,itf
          IF(LANDMASK(I,J).GT.0)THEN
            IF(QLAT(i,j).GT.1.e-2)THEN
                 NCOUNT=NCOUNT+1
                 WTD(i,j)=min(WTD(i,j)+0.25,0.)
            ENDIF
          ENDIF
        ENDDO
     ENDDO
ENDIF

 ENDDO

#if (EM_CORE == 1)
#ifdef DM_PARALLEL
#     include "HALO_EM_HYDRO_NOAHMP.inc"
#endif
#endif

EQWTD=WTD

!after adjusting, where qlat > 1cm/year now wtd is at the surface.
!it may still happen that qlat + rech > 0 and eqwtd-rbed <0. There the wtd can
!rise to the surface (poor drainage) but the et will then increase.


!now, calculate rcond:

    DO J=jts,jtf
       DO I=its,itf

        DDZ = EQWTD(I,J)- ( RIVERBED(I,J)-TOPO(I,J) )
!dont allow riverbed above water table
        IF(DDZ.LT.0.)then
               RIVERBED(I,J)=TOPO(I,J)+EQWTD(I,J)
               DDZ=0.
        ENDIF


        TOTWATER = AREA(I,J)*(QLAT(I,J)+RECHCLIM(I,J)*0.001)/DELTAT

        IF (TOTWATER.GT.0) THEN
              RIVERCOND(I,J) = TOTWATER / MAX(DDZ,0.05)
        ELSE
              RIVERCOND(I,J)=0.01
!and make riverbed  equal to eqwtd, otherwise qrf might be too big...
              RIVERBED(I,J)=TOPO(I,J)+EQWTD(I,J)
        ENDIF


       ENDDO
    ENDDO

!make riverbed to be height down from the surface instead of above sea level

    RIVERBED = min( RIVERBED-TOPO, 0.)

!now recompute lateral flow and flow to rivers to initialize deep soil moisture

    DELTAT = WTDDT * 60. !timestep in seconds for this calculation


!recalculate lateral flow

    QLAT = 0.
    CALL LATERALFLOW(ISLTYP,WTD,QLAT,FDEPTH,TOPO,LANDMASK,DELTAT,AREA       &
                        ,ids,ide,jds,jde,kds,kde                      & 
                        ,ims,ime,jms,jme,kms,kme                      &
                        ,its,ite,jts,jte,kts,kte                      )
                        
!compute flux from grounwater to rivers in the cell

    DO J=jts,jtf
       DO I=its,itf
          IF(LANDMASK(I,J).GT.0)THEN
             IF(WTD(I,J) .GT. RIVERBED(I,J) .AND.  EQWTD(I,J) .GT. RIVERBED(I,J)) THEN
               RCOND = RIVERCOND(I,J) * EXP(PEXP(I,J)*(WTD(I,J)-EQWTD(I,J)))
             ELSE    
               RCOND = RIVERCOND(I,J)
             ENDIF
             QRF(I,J) = RCOND * (WTD(I,J)-RIVERBED(I,J)) * DELTAT/AREA(I,J)
!for now, dont allow it to go from river to groundwater
             QRF(I,J) = MAX(QRF(I,J),0.) 
          ELSE
             QRF(I,J) = 0.
          ENDIF
       ENDDO
    ENDDO

!now compute eq. soil moisture, change soil moisture to be compatible with the water table and compute deep soil moisture

       DO J = jts,jtf
          DO I = its,itf
             BEXP   =   BEXP_TABLE(ISLTYP(I,J))
             SMCMAX = SMCMAX_TABLE(ISLTYP(I,J))
             SMCWLT = SMCWLT_TABLE(ISLTYP(I,J))
             IF(IVGTYP(I,J)==ISURBAN_TABLE)THEN
                 SMCMAX = 0.45         
                 SMCWLT = 0.40         
             ENDIF 
             DWSAT  =   DWSAT_TABLE(ISLTYP(I,J))
             DKSAT  =   DKSAT_TABLE(ISLTYP(I,J))
             PSISAT = -PSISAT_TABLE(ISLTYP(I,J))
           IF ( ( BEXP > 0.0 ) .AND. ( smcmax > 0.0 ) .AND. ( -psisat > 0.0 ) ) THEN
             !initialize equilibrium soil moisture for water table diagnostic
                    CALL EQSMOISTURE(NSOIL ,  ZSOIL , SMCMAX , SMCWLT ,DWSAT, DKSAT  ,BEXP  , & !in
                                     SMCEQ                          )  !out

             SMOISEQ (I,1:NSOIL,J) = SMCEQ (1:NSOIL)


              !make sure that below the water table the layers are saturated and initialize the deep soil moisture
             IF(WTD(I,J) < ZSOIL(NSOIL)-DZS(NSOIL)) THEN

!initialize deep soil moisture so that the flux compensates qlat+qrf
!use Newton-Raphson method to find soil moisture

                         EXPON = 2. * BEXP + 3.
                         DDZ = ZSOIL(NSOIL) - WTD(I,J)
                         CC = PSISAT/DDZ
                         FLUX = (QLAT(I,J)-QRF(I,J))/DELTAT

                         SMC = 0.5 * SMCMAX

                         DO ITER = 1, 100
                           DD = (SMC+SMCMAX)/(2.*SMCMAX)
                           AA = -DKSAT * DD  ** EXPON
                           BBB = CC * ( (SMCMAX/SMC)**BEXP - 1. ) + 1. 
                           FUNC =  AA * BBB - FLUX
                           DFUNC = -DKSAT * (EXPON/(2.*SMCMAX)) * DD ** (EXPON - 1.) * BBB &
                                   + AA * CC * (-BEXP) * SMCMAX ** BEXP * SMC ** (-BEXP-1.)

                           DX = FUNC/DFUNC
                           SMC = SMC - DX
                           IF ( ABS (DX) < 1.E-6)EXIT
                         ENDDO

                  SMCWTDXY(I,J) = MAX(SMC,1.E-4)

             ELSEIF(WTD(I,J) < ZSOIL(NSOIL))THEN
                  SMCEQDEEP = SMCMAX * ( PSISAT / ( PSISAT - DZS(NSOIL) ) ) ** (1./BEXP)
!                  SMCEQDEEP = MAX(SMCEQDEEP,SMCWLT)
                  SMCEQDEEP = MAX(SMCEQDEEP,1.E-4)
                  SMCWTDXY(I,J) = SMCMAX * ( WTD(I,J) -  (ZSOIL(NSOIL)-DZS(NSOIL))) + &
                                  SMCEQDEEP * (ZSOIL(NSOIL) - WTD(I,J))

             ELSE !water table within the resolved layers
                  SMCWTDXY(I,J) = SMCMAX
                  DO K=NSOIL,2,-1
                     IF(WTD(I,J) .GE. ZSOIL(K-1))THEN
                          FRLIQ = SH2O(I,K,J) / SMOIS(I,K,J)
                          SMOIS(I,K,J) = SMCMAX
                          SH2O(I,K,J) = SMCMAX * FRLIQ
                     ELSE
                          IF(SMOIS(I,K,J).LT.SMCEQ(K))THEN
                              WTD(I,J) = ZSOIL(K)
                          ELSE
                              WTD(I,J) = ( SMOIS(I,K,J)*DZS(K) - SMCEQ(K)*ZSOIL(K-1) + SMCMAX*ZSOIL(K) ) / &
                                         (SMCMAX - SMCEQ(K))   
                          ENDIF
                          EXIT
                     ENDIF
                  ENDDO
             ENDIF
            ELSE
              SMOISEQ (I,1:NSOIL,J) = SMCMAX
              SMCWTDXY(I,J) = SMCMAX
              WTD(I,J) = 0.
            ENDIF

!zero out some arrays

             DEEPRECHXY(I,J) = 0.
             RECHXY(I,J) = 0.
             QSLATXY(I,J) = 0.
             QRFSXY(I,J) = 0.
             QSPRINGSXY(I,J) = 0.

          ENDDO
       ENDDO




    END  SUBROUTINE GROUNDWATER_INIT
! ==================================================================================================
! ----------------------------------------------------------------------
  SUBROUTINE EQSMOISTURE(NSOIL  ,  ZSOIL , SMCMAX , SMCWLT, DWSAT , DKSAT ,BEXP , & !in
                         SMCEQ                          )  !out
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  INTEGER,                         INTENT(IN) :: NSOIL !no. of soil layers
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL !depth of soil layer-bottom [m]
  REAL,                            INTENT(IN) :: SMCMAX , SMCWLT, BEXP , DWSAT, DKSAT
!output
  REAL,  DIMENSION(      1:NSOIL), INTENT(OUT) :: SMCEQ  !equilibrium soil water  content [m3/m3]
!local
  INTEGER                                     :: K , ITER
  REAL                                        :: DDZ , SMC, FUNC, DFUNC , AA, BB , EXPON, DX

!gmmcompute equilibrium soil moisture content for the layer when wtd=zsoil(k)


   DO K=1,NSOIL

            IF ( K == 1 )THEN
                DDZ = -ZSOIL(K+1) * 0.5
            ELSEIF ( K < NSOIL ) THEN
                DDZ = ( ZSOIL(K-1) - ZSOIL(K+1) ) * 0.5
            ELSE
                DDZ = ZSOIL(K-1) - ZSOIL(K)
            ENDIF

!use Newton-Raphson method to find eq soil moisture

            EXPON = BEXP +1.
            AA = DWSAT/DDZ
            BB = DKSAT / SMCMAX ** EXPON

            SMC = 0.5 * SMCMAX

         DO ITER = 1, 100
            FUNC = (SMC - SMCMAX) * AA +  BB * SMC ** EXPON
            DFUNC = AA + BB * EXPON * SMC ** BEXP 

            DX = FUNC/DFUNC
            SMC = SMC - DX
            IF ( ABS (DX) < 1.E-6)EXIT
         ENDDO

!             SMCEQ(K) = MIN(MAX(SMC,SMCWLT),SMCMAX*0.99)
             SMCEQ(K) = MIN(MAX(SMC,1.E-4),SMCMAX*0.99)
   ENDDO

END  SUBROUTINE EQSMOISTURE

! gecros initialization routines

SUBROUTINE gecros_init(xlat,hti,rdi,clvi,crti,nlvi,laii,nrti,slnbi,state_gecros)   
implicit none
REAL, INTENT(IN)     :: HTI
REAL, INTENT(IN)     :: RDI
REAL, INTENT(IN)     :: CLVI
REAL, INTENT(IN)     :: CRTI
REAL, INTENT(IN)     :: NLVI
REAL, INTENT(IN)     :: LAII
REAL, INTENT(IN)     :: NRTI
REAL, INTENT(IN)     :: SLNBI
REAL, INTENT(IN)     :: XLAT
REAL, DIMENSION(1:60), INTENT(INOUT) :: STATE_GECROS

  !Inititalization of Gecros variables  
  STATE_GECROS(1) = 0.      !DS
  STATE_GECROS(2) = 0.      !CTDURDI, HTI, CLVI, CRTI, NLVI, LAII, NRTI, SLNBI,
  STATE_GECROS(3) = 0.      !CVDU
  STATE_GECROS(4) = CLVI    !CLV 
  STATE_GECROS(5) = 0.      !CLVD  
  STATE_GECROS(6) = 0.      !CSST  
  STATE_GECROS(7) = 0.      !CSO
  STATE_GECROS(8) = CRTI    !CSRT 
  STATE_GECROS(9) =  0.     !CRTD 
  STATE_GECROS(10) = 0.     !CLVDS 
  STATE_GECROS(11) = NRTI   !NRT
  STATE_GECROS(12) = 0.     !NST
  STATE_GECROS(13) = NLVI   !NLV 
  STATE_GECROS(14) = 0.     !NSO
  STATE_GECROS(15) = NLVI   !TNLV
  STATE_GECROS(16) = 0.     !NLVD
  STATE_GECROS(17) = 0.     !NRTD
  STATE_GECROS(18) = 0.     !CRVS
  STATE_GECROS(19) = 0.     !CRVR
  STATE_GECROS(20) = 0.     !NREOE
  STATE_GECROS(21) = 0.     !NREOF
  STATE_GECROS(22) = 0.     !DCDSR
  STATE_GECROS(23) = 0.     !DCDTR
  STATE_GECROS(24) = SLNBI  !SLNB
  STATE_GECROS(25) = LAII   !LAIC
  STATE_GECROS(26) = 0.     !RMUL
  STATE_GECROS(27) = 0.     !NDEMP
  STATE_GECROS(28) = 0.     !NSUPP
  STATE_GECROS(29) = 0.     !NFIXT
  STATE_GECROS(30) = 0.     !NFIXR
  STATE_GECROS(31) = 0.     !DCDTP
  STATE_GECROS(32) = 0.01   !HTI
  STATE_GECROS(33) = RDI    !RDI
  STATE_GECROS(34) = 0.     !TPCAN
  STATE_GECROS(35) = 0.     !TRESP
  STATE_GECROS(36) = 0.     !TNUPT
  STATE_GECROS(37) = 0.     !LITNT
  STATE_GECROS(38) = 0.     !daysSinceDS1
  STATE_GECROS(39) = 0.     !daysSinceDS2
  STATE_GECROS(40) = -1.    !drilled -1:false, 1:true
  STATE_GECROS(41) = -1.    !emerged -1:false, 1:true
  STATE_GECROS(42) = -1.    !harvested -1:false, 1:true
  STATE_GECROS(43) = 0.     !TTEM
  STATE_GECROS(44) = XLAT   !GLAT
  STATE_GECROS(45) = 0.     !WSO
  STATE_GECROS(46) = 0.     !WSTRAW
  STATE_GECROS(47) = 0.     !GrainNC
  STATE_GECROS(48) = 0.     !StrawNC
  STATE_GECROS(49) = 0.01   !GLAI
  STATE_GECROS(50) = 0.01   !TLAI
  STATE_GECROS(51) = HTI    !Fields 51-58 set for reinitialization  
  STATE_GECROS(52) = RDI    
  STATE_GECROS(53) = CLVI   
  STATE_GECROS(54) = CRTI   
  STATE_GECROS(55) = NRTI   
  STATE_GECROS(56) = NLVI   
  STATE_GECROS(57) = SLNBI  
  STATE_GECROS(58) = LAII   
    
END SUBROUTINE gecros_init

SUBROUTINE gecros_reinit(STATE_GECROS)   
implicit none
REAL, DIMENSION(1:60), INTENT(INOUT) :: STATE_GECROS

  !Re-inititalization of Gecros variables after harvest
  STATE_GECROS(1) = 0.               !DS
  STATE_GECROS(2) = 0.               !CTDU
  STATE_GECROS(3) = 0.               !CVDU
  STATE_GECROS(4) = STATE_GECROS(53) !CLV 
  STATE_GECROS(5) = 0.               !CLVD  
  STATE_GECROS(6) = 0.               !CSST  
  STATE_GECROS(7) = 0.               !CSO
  STATE_GECROS(8) = STATE_GECROS(54) !CRT 
  STATE_GECROS(9) = 0.               !CRTD 
  STATE_GECROS(10) = 0.              !CLVDS 
  STATE_GECROS(11) = STATE_GECROS(55)!NRT
  STATE_GECROS(12) = 0.              !NST
  STATE_GECROS(13) = STATE_GECROS(56)!NLV 
  STATE_GECROS(14) = 0.              !NSO
  STATE_GECROS(15) = STATE_GECROS(56)!TNLV
  STATE_GECROS(16) = 0.              !NLVD
  STATE_GECROS(17) = 0.              !NRTD
  STATE_GECROS(18) = 0.              !CRVS
  STATE_GECROS(19) = 0.              !CRVR
  STATE_GECROS(20) = 0.              !NREOE
  STATE_GECROS(21) = 0.              !NREOF
  STATE_GECROS(22) = 0.              !DCDSR
  STATE_GECROS(23) = 0.              !DCDTR
  STATE_GECROS(24) = STATE_GECROS(57)!SLNB
  STATE_GECROS(25) = STATE_GECROS(58)!LAIC
  STATE_GECROS(26) = 0.              !RMUL
  STATE_GECROS(27) = 0.              !NDEMP
  STATE_GECROS(28) = 0.              !NSUPP
  STATE_GECROS(29) = 0.              !NFIXT
  STATE_GECROS(30) = 0.              !NFIXR
  STATE_GECROS(31) = 0.              !DCDTP
  STATE_GECROS(32) = STATE_GECROS(51)!HT
  STATE_GECROS(33) = STATE_GECROS(52)!ROOTD
  STATE_GECROS(34) = 0.              !TPCAN
  STATE_GECROS(35) = 0.              !TRESP
  STATE_GECROS(36) = 0.              !TNUPT
  STATE_GECROS(37) = 0.              !LITNT
  STATE_GECROS(38) = 0.              !daysSinceDS1
  STATE_GECROS(39) = 0.              !daysSinceDS2
  STATE_GECROS(40) = -1.             !drilled -1:false, 1:true
  STATE_GECROS(41) = -1.             !emerged -1:false, 1:true
  STATE_GECROS(42) = 1.              !harvested -1:false, 1:true
  STATE_GECROS(43) = 0.              !TTEM
  STATE_GECROS(45) = 0.              !WSO
  STATE_GECROS(46) = 0.              !WSTRAW
  STATE_GECROS(47) = 0.              !GrainNC
  STATE_GECROS(48) = 0.              !StrawNC
  STATE_GECROS(49) = 0.01            !GLAI
  STATE_GECROS(50) = 0.01            !TLAI
  
END SUBROUTINE gecros_reinit

!***Function for HARVEST DATES:

!Determine if crop is to be harvested today
!function to be called once a day
!return codes: 0 - no, 1- yes 
!requires two counters 'daysSinceDS2', 'daysSinceDS1' , zero-initialized to be maintained within caller
!STATE_GECROS(1) = current DS
!STATE_GECROS(38)=daysSinceDS1
!STATE_GECROS(39)=daysSinceDS2

function checkIfHarvest(STATE_GECROS, DT, harvestDS1, harvestDS2, harvestDS1ExtraDays, harvestDS2ExtraDays) 
implicit none
real :: DT, harvestDS1, harvestDS2
real :: daysSinceDS1, daysSinceDS2
real :: harvestDS1ExtraDays, harvestDS2ExtraDays
integer :: checkIfHarvest
REAL, DIMENSION(1:60), INTENT(INOUT) :: STATE_GECROS


 !***check whether maturity (DS1) has been reached
 if (STATE_GECROS(1) >= harvestDS1) then
 
    if (STATE_GECROS(38) >= harvestDS1ExtraDays) then
        checkIfHarvest=1
 !if we are > DS1, but not over the limit, increase the counter of days
    else
        STATE_GECROS(38) = STATE_GECROS(38) + DT/86400.
    endif
 else 
 
 !if maturity has not been reached, but we are close (> DS2)
 !check the number of days for which we have been > DS2
 !and harvest in case we are over the limit given for that stage 
 !(in case that maturity will not be reached at all)
 
 checkIfHarvest=0
 if (STATE_GECROS(1) >= harvestDS2 ) then

       if (STATE_GECROS(39) >= harvestDS2ExtraDays) then
           checkIfHarvest=1
       else !if we are > DS2, but not over the limit, increase the counter of days
           STATE_GECROS(39) = STATE_GECROS(39) + DT/86400.
           checkIfHarvest=0
      endif
 endif
 endif
 return
end function checkIfHarvest

!------------------------------------------------------------------------------------------

  SUBROUTINE noahmp_urban(sf_urban_physics,   NSOIL,         IVGTYP,  ITIMESTEP,            & ! IN : Model configuration 
                                 DT,     COSZ_URB2D,     XLAT_URB2D,                        & ! IN : Time/Space-related
                                T3D,           QV3D,          U_PHY,      V_PHY,   SWDOWN,  & ! IN : Forcing
		                GLW,          P8W3D,         RAINBL,       DZ8W,      ZNT,  & ! IN : Forcing
                                TSK,            HFX,            QFX,         LH,   GRDFLX,  & ! IN/OUT : LSM 
		             ALBEDO,          EMISS,           QSFC,                        & ! IN/OUT : LSM 
                            ids,ide,        jds,jde,        kds,kde,                        &
                            ims,ime,        jms,jme,        kms,kme,                        &
                            its,ite,        jts,jte,        kts,kte,                        &
                         cmr_sfcdif,     chr_sfcdif,     cmc_sfcdif,                        &
	                 chc_sfcdif,    cmgr_sfcdif,    chgr_sfcdif,                        &
                           tr_urb2d,       tb_urb2d,       tg_urb2d,                        & !H urban
	                   tc_urb2d,       qc_urb2d,       uc_urb2d,                        & !H urban
                         xxxr_urb2d,     xxxb_urb2d,     xxxg_urb2d, xxxc_urb2d,            & !H urban
                          trl_urb3d,      tbl_urb3d,      tgl_urb3d,                        & !H urban
                           sh_urb2d,       lh_urb2d,        g_urb2d,   rn_urb2d,  ts_urb2d, & !H urban
                         psim_urb2d,     psih_urb2d,      u10_urb2d,  v10_urb2d,            & !O urban
                       GZ1OZ0_urb2d,     AKMS_URB2D,                                        & !O urban
                          th2_urb2d,       q2_urb2d,      ust_urb2d,                        & !O urban
                         declin_urb,      omg_urb2d,                                        & !I urban
                    num_roof_layers,num_wall_layers,num_road_layers,                        & !I urban
                                dzr,            dzb,            dzg,                        & !I urban
                         cmcr_urb2d,      tgr_urb2d,     tgrl_urb3d,  smr_urb3d,            & !H urban
                        drelr_urb2d,    drelb_urb2d,    drelg_urb2d,                        & !H urban
                      flxhumr_urb2d,  flxhumb_urb2d,  flxhumg_urb2d,                        & !H urban
                             julian,          julyr,                                        & !H urban
                          frc_urb2d,    utype_urb2d,                                        & !I urban
                                chs,           chs2,           cqs2,                        & !H
                   num_urban_layers,                                                        & !I multi-layer urban
                       num_urban_hi,                                                        & !I multi-layer urban
                          trb_urb4d,      tw1_urb4d,      tw2_urb4d,  tgb_urb4d,            & !H multi-layer urban
                         tlev_urb3d,     qlev_urb3d,                                        & !H multi-layer urban
                       tw1lev_urb3d,   tw2lev_urb3d,                                        & !H multi-layer urban
                        tglev_urb3d,    tflev_urb3d,                                        & !H multi-layer urban
                        sf_ac_urb3d,    lf_ac_urb3d,    cm_ac_urb3d,                        & !H multi-layer urban
                       sfvent_urb3d,   lfvent_urb3d,                                        & !H multi-layer urban
                       sfwin1_urb3d,   sfwin2_urb3d,                                        & !H multi-layer urban
                         sfw1_urb3d,     sfw2_urb3d,      sfr_urb3d,  sfg_urb3d,            & !H multi-layer urban
                           lp_urb2d,       hi_urb2d,       lb_urb2d,  hgt_urb2d,            & !H multi-layer urban
                           mh_urb2d,     stdh_urb2d,       lf_urb2d,                        & !SLUCM
                             th_phy,            rho,          p_phy,        ust,            & !I multi-layer urban
                                gmt,         julday,          xlong,       xlat,            & !I multi-layer urban
                            a_u_bep,        a_v_bep,        a_t_bep,    a_q_bep,            & !O multi-layer urban
                            a_e_bep,        b_u_bep,        b_v_bep,                        & !O multi-layer urban
                            b_t_bep,        b_q_bep,        b_e_bep,    dlg_bep,            & !O multi-layer urban
                           dl_u_bep,         sf_bep,         vl_bep                         & !O multi-layer urban
                 )

  USE module_sf_urban,    only: urban
  USE module_sf_bep,      only: bep
  USE module_sf_bep_bem,  only: bep_bem
  USE module_ra_gfdleta,  only: cal_mon_day
  USE NOAHMP_TABLES, ONLY: ISURBAN_TABLE
  USE module_model_constants, only: KARMAN, CP, XLV
!----------------------------------------------------------------
    IMPLICIT NONE
!----------------------------------------------------------------

    INTEGER,                                         INTENT(IN   ) ::  sf_urban_physics   ! urban physics option
    INTEGER,                                         INTENT(IN   ) ::  NSOIL     ! number of soil layers
    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  IVGTYP    ! vegetation type
    INTEGER,                                         INTENT(IN   ) ::  ITIMESTEP ! timestep number
    REAL,                                            INTENT(IN   ) ::  DT        ! timestep [s]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  COSZ_URB2D
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XLAT_URB2D
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  T3D       ! 3D atmospheric temperature valid at mid-levels [K]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  QV3D      ! 3D water vapor mixing ratio [kg/kg_dry]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  U_PHY     ! 3D U wind component [m/s]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  V_PHY     ! 3D V wind component [m/s]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SWDOWN    ! solar down at surface [W m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  GLW       ! longwave down at surface [W m-2]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  P8W3D     ! 3D pressure, valid at interface [Pa]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  RAINBL    ! total input precipitation [mm]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  DZ8W      ! thickness of atmo layers [m]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ZNT       ! combined z0 sent to coupled model
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TSK       ! surface radiative temperature [K]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  HFX       ! sensible heat flux [W m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QFX       ! latent heat flux [kg s-1 m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  LH        ! latent heat flux [W m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  GRDFLX    ! ground/snow heat flux [W m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ALBEDO    ! total grid albedo []
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  EMISS     ! surface bulk emissivity
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QSFC      ! bulk surface mixing ratio

    INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &  ! d -> domain
         &                           ims,ime, jms,jme, kms,kme,  &  ! m -> memory
         &                           its,ite, jts,jte, kts,kte      ! t -> tile

! input variables surface_driver --> lsm

     INTEGER,                                                INTENT(IN   ) :: num_roof_layers
     INTEGER,                                                INTENT(IN   ) :: num_wall_layers
     INTEGER,                                                INTENT(IN   ) :: num_road_layers

     INTEGER,        DIMENSION( ims:ime, jms:jme ),          INTENT(IN   ) :: UTYPE_URB2D
     REAL,           DIMENSION( ims:ime, jms:jme ),          INTENT(IN   ) :: FRC_URB2D

     REAL, OPTIONAL, DIMENSION(1:num_roof_layers),           INTENT(IN   ) :: DZR
     REAL, OPTIONAL, DIMENSION(1:num_wall_layers),           INTENT(IN   ) :: DZB
     REAL, OPTIONAL, DIMENSION(1:num_road_layers),           INTENT(IN   ) :: DZG
     REAL, OPTIONAL,                                         INTENT(IN   ) :: DECLIN_URB
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),          INTENT(IN   ) :: OMG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: TH_PHY
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: P_PHY
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: RHO

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),          INTENT(INOUT) :: UST
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),          INTENT(INOUT) :: CHS, CHS2, CQS2

     INTEGER,  INTENT(IN   )   ::  julian, julyr                  !urban

! local variables lsm --> urban

     INTEGER :: UTYPE_URB ! urban type [urban=1, suburban=2, rural=3]
     REAL    :: TA_URB       ! potential temp at 1st atmospheric level [K]
     REAL    :: QA_URB       ! mixing ratio at 1st atmospheric level  [kg/kg]
     REAL    :: UA_URB       ! wind speed at 1st atmospheric level    [m/s]
     REAL    :: U1_URB       ! u at 1st atmospheric level             [m/s]
     REAL    :: V1_URB       ! v at 1st atmospheric level             [m/s]
     REAL    :: SSG_URB      ! downward total short wave radiation    [W/m/m]
     REAL    :: LLG_URB      ! downward long wave radiation           [W/m/m]
     REAL    :: RAIN_URB     ! precipitation                          [mm/h]
     REAL    :: RHOO_URB     ! air density                            [kg/m^3]
     REAL    :: ZA_URB       ! first atmospheric level                [m]
     REAL    :: DELT_URB     ! time step                              [s]
     REAL    :: SSGD_URB     ! downward direct short wave radiation   [W/m/m]
     REAL    :: SSGQ_URB     ! downward diffuse short wave radiation  [W/m/m]
     REAL    :: XLAT_URB     ! latitude                               [deg]
     REAL    :: COSZ_URB     ! cosz
     REAL    :: OMG_URB      ! hour angle
     REAL    :: ZNT_URB      ! roughness length                       [m]
     REAL    :: TR_URB
     REAL    :: TB_URB
     REAL    :: TG_URB
     REAL    :: TC_URB
     REAL    :: QC_URB
     REAL    :: UC_URB
     REAL    :: XXXR_URB
     REAL    :: XXXB_URB
     REAL    :: XXXG_URB
     REAL    :: XXXC_URB
     REAL, DIMENSION(1:num_roof_layers) :: TRL_URB  ! roof layer temp [K]
     REAL, DIMENSION(1:num_wall_layers) :: TBL_URB  ! wall layer temp [K]
     REAL, DIMENSION(1:num_road_layers) :: TGL_URB  ! road layer temp [K]
     LOGICAL  :: LSOLAR_URB

!===hydrological variable for single layer UCM===

     INTEGER :: jmonth, jday
     REAL    :: DRELR_URB
     REAL    :: DRELB_URB
     REAL    :: DRELG_URB
     REAL    :: FLXHUMR_URB
     REAL    :: FLXHUMB_URB
     REAL    :: FLXHUMG_URB
     REAL    :: CMCR_URB
     REAL    :: TGR_URB

     REAL, DIMENSION(1:num_roof_layers) :: SMR_URB  ! green roof layer moisture
     REAL, DIMENSION(1:num_roof_layers) :: TGRL_URB ! green roof layer temp [K]

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: DRELR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: DRELB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: DRELG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: FLXHUMR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: FLXHUMB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: FLXHUMG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: CMCR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: TGR_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TGRL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: SMR_URB3D


! state variable surface_driver <--> lsm <--> urban

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: G_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RN_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TRL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_wall_layers, jms:jme ), INTENT(INOUT) :: TBL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_road_layers, jms:jme ), INTENT(INOUT) :: TGL_URB3D

! output variable lsm --> surface_driver

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIM_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: GZ1OZ0_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: U10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: V10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: TH2_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: Q2_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: AKMS_URB2D
     REAL,           DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: UST_URB2D


! output variables urban --> lsm

     REAL :: TS_URB           ! surface radiative temperature    [K]
     REAL :: QS_URB           ! surface humidity                 [-]
     REAL :: SH_URB           ! sensible heat flux               [W/m/m]
     REAL :: LH_URB           ! latent heat flux                 [W/m/m]
     REAL :: LH_KINEMATIC_URB ! latent heat flux, kinetic  [kg/m/m/s]
     REAL :: SW_URB           ! upward short wave radiation flux [W/m/m]
     REAL :: ALB_URB          ! time-varying albedo            [fraction]
     REAL :: LW_URB           ! upward long wave radiation flux  [W/m/m]
     REAL :: G_URB            ! heat flux into the ground        [W/m/m]
     REAL :: RN_URB           ! net radiation                    [W/m/m]
     REAL :: PSIM_URB         ! shear f for momentum             [-]
     REAL :: PSIH_URB         ! shear f for heat                 [-]
     REAL :: GZ1OZ0_URB       ! shear f for heat                 [-]
     REAL :: U10_URB          ! wind u component at 10 m         [m/s]
     REAL :: V10_URB          ! wind v component at 10 m         [m/s]
     REAL :: TH2_URB          ! potential temperature at 2 m     [K]
     REAL :: Q2_URB           ! humidity at 2 m                  [-]
     REAL :: CHS_URB
     REAL :: CHS2_URB
     REAL :: UST_URB

! NUDAPT Parameters urban --> lam

     REAL :: mh_urb
     REAL :: stdh_urb
     REAL :: lp_urb
     REAL :: hgt_urb
     REAL, DIMENSION(4) :: lf_urb

! Local variables

     INTEGER :: I,J,K
     REAL :: Q1

! Noah UA changes

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: CMR_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: CHR_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: CMGR_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: CHGR_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: CMC_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: CHC_SFCDIF

! Variables for multi-layer UCM

     REAL, OPTIONAL,                                                    INTENT(IN   ) :: GMT
     INTEGER, OPTIONAL,                                                 INTENT(IN   ) :: JULDAY
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: XLAT, XLONG
     INTEGER,                                                           INTENT(IN   ) :: NUM_URBAN_LAYERS
     INTEGER,                                                           INTENT(IN   ) :: NUM_URBAN_HI
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_hi, jms:jme ),     INTENT(IN   ) :: hi_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: lp_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: lb_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: hgt_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: mh_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: stdh_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, 4, jms:jme ),                  INTENT(IN   ) :: lf_urb2d

     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: trb_urb4d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw1_urb4d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw2_urb4d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tgb_urb4d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tlev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: qlev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw1lev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw2lev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tglev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tflev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: lf_ac_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: sf_ac_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: cm_ac_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: sfvent_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: lfvent_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfwin1_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfwin2_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfw1_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfw2_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfr_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfg_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_u_bep   !Implicit momemtum component X-direction
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_v_bep   !Implicit momemtum component Y-direction
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_t_bep   !Implicit component pot. temperature
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_q_bep   !Implicit momemtum component X-direction
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_e_bep   !Implicit component TKE
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_u_bep   !Explicit momentum component X-direction
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_v_bep   !Explicit momentum component Y-direction
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_t_bep   !Explicit component pot. temperature
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_q_bep   !Implicit momemtum component Y-direction
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_e_bep   !Explicit component TKE
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: vl_bep    !Fraction air volume in grid cell
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: dlg_bep   !Height above ground
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: sf_bep    !Fraction air at the face of grid cell
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: dl_u_bep  !Length scale

! Local variables for multi-layer UCM

     REAL,    DIMENSION( its:ite, jts:jte) :: HFX_RURAL,GRDFLX_RURAL          ! ,LH_RURAL,RN_RURAL
     REAL,    DIMENSION( its:ite, jts:jte) :: QFX_RURAL                       ! ,QSFC_RURAL,UMOM_RURAL,VMOM_RURAL
     REAL,    DIMENSION( its:ite, jts:jte) :: ALB_RURAL,EMISS_RURAL,TSK_RURAL ! ,UST_RURAL
     REAL,    DIMENSION( its:ite, jts:jte) :: HFX_URB,UMOM_URB,VMOM_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: QFX_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: EMISS_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: RL_UP_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: RS_ABS_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: GRDFLX_URB

     REAL :: SIGMA_SB,RL_UP_RURAL,RL_UP_TOT,RS_ABS_TOT,UMOM,VMOM
     REAL :: r1,r2,r3
     REAL :: CMR_URB, CHR_URB, CMC_URB, CHC_URB, CMGR_URB, CHGR_URB
     REAL :: frc_urb,lb_urb
     REAL :: check

    character(len=80) :: message

    DO J=JTS,JTE
    DO I=ITS,ITE
      HFX_RURAL(I,J)                = HFX(I,J)
      QFX_RURAL(I,J)                = QFX(I,J)
      GRDFLX_RURAL(I,J)             = GRDFLX(I,J)
      EMISS_RURAL(I,J)              = EMISS(I,J)
      TSK_RURAL(I,J)                = TSK(I,J)
      ALB_RURAL(I,J)                = ALBEDO(I,J)
    END DO
    END DO

IF (SF_URBAN_PHYSICS == 1 ) THEN         ! Beginning of UCM CALL if block

!--------------------------------------
! URBAN CANOPY MODEL START
!--------------------------------------

JLOOP : DO J = jts, jte

ILOOP : DO I = its, ite


  IF( IVGTYP(I,J) == ISURBAN_TABLE .or. IVGTYP(I,J) == 31 .or. &
      IVGTYP(I,J) == 32 .or. IVGTYP(I,J) == 33 ) THEN

    UTYPE_URB = UTYPE_URB2D(I,J) !urban type (low, high or industrial)

    TA_URB    = T3D(I,1,J)                                ! [K]            
    QA_URB    = QV3D(I,1,J)/(1.0+QV3D(I,1,J))             ! [kg/kg]       
    UA_URB    = SQRT(U_PHY(I,1,J)**2.+V_PHY(I,1,J)**2.)
    U1_URB    = U_PHY(I,1,J)
    V1_URB    = V_PHY(I,1,J)
    IF(UA_URB < 1.) UA_URB=1.                             ! [m/s]
    SSG_URB   = SWDOWN(I,J)                               ! [W/m/m]      
    SSGD_URB  = 0.8*SWDOWN(I,J)                           ! [W/m/m]     
    SSGQ_URB  = SSG_URB-SSGD_URB                          ! [W/m/m]
    LLG_URB   = GLW(I,J)                                  ! [W/m/m]
    RAIN_URB  = RAINBL(I,J)                               ! [mm]       
    RHOO_URB  = (P8W3D(I,KTS+1,J)+P8W3D(I,KTS,J))*0.5 / (287.04 * TA_URB * (1.0+ 0.61 * QA_URB)) ![kg/m/m/m] 
    ZA_URB    = 0.5*DZ8W(I,1,J)                           ! [m]         
    DELT_URB  = DT                                        ! [sec]
    XLAT_URB  = XLAT_URB2D(I,J)                           ! [deg]
    COSZ_URB  = COSZ_URB2D(I,J) 
    OMG_URB   = OMG_URB2D(I,J)
    ZNT_URB   = ZNT(I,J)

    LSOLAR_URB = .FALSE.

    TR_URB = TR_URB2D(I,J)
    TB_URB = TB_URB2D(I,J)
    TG_URB = TG_URB2D(I,J)
    TC_URB = TC_URB2D(I,J)
    QC_URB = QC_URB2D(I,J)
    UC_URB = UC_URB2D(I,J)

    TGR_URB     = TGR_URB2D(I,J)
    CMCR_URB    = CMCR_URB2D(I,J)
    FLXHUMR_URB = FLXHUMR_URB2D(I,J)
    FLXHUMB_URB = FLXHUMB_URB2D(I,J)
    FLXHUMG_URB = FLXHUMG_URB2D(I,J)
    DRELR_URB   = DRELR_URB2D(I,J)
    DRELB_URB   = DRELB_URB2D(I,J)
    DRELG_URB   = DRELG_URB2D(I,J)

    DO K = 1,num_roof_layers
      TRL_URB(K) = TRL_URB3D(I,K,J)
      SMR_URB(K) = SMR_URB3D(I,K,J)
      TGRL_URB(K)= TGRL_URB3D(I,K,J)
    END DO

    DO K = 1,num_wall_layers
      TBL_URB(K) = TBL_URB3D(I,K,J)
    END DO

    DO K = 1,num_road_layers
      TGL_URB(K) = TGL_URB3D(I,K,J)
    END DO

    XXXR_URB = XXXR_URB2D(I,J)
    XXXB_URB = XXXB_URB2D(I,J)
    XXXG_URB = XXXG_URB2D(I,J)
    XXXC_URB = XXXC_URB2D(I,J)

! Limits to avoid dividing by small number
    IF (CHS(I,J) < 1.0E-02) THEN
      CHS(I,J)  = 1.0E-02
    ENDIF
    IF (CHS2(I,J) < 1.0E-02) THEN
      CHS2(I,J)  = 1.0E-02
    ENDIF
    IF (CQS2(I,J) < 1.0E-02) THEN
      CQS2(I,J)  = 1.0E-02
    ENDIF

    CHS_URB  = CHS(I,J)
    CHS2(I,J)= CQS2(I,J)      
    CHS2_URB = CHS2(I,J)
    IF (PRESENT(CMR_SFCDIF)) THEN
      CMR_URB = CMR_SFCDIF(I,J)
      CHR_URB = CHR_SFCDIF(I,J)
      CMGR_URB = CMGR_SFCDIF(I,J)
      CHGR_URB = CHGR_SFCDIF(I,J)
      CMC_URB = CMC_SFCDIF(I,J)
      CHC_URB = CHC_SFCDIF(I,J)
    ENDIF

! NUDAPT for SLUCM

    MH_URB   = MH_URB2D(I,J)
    STDH_URB = STDH_URB2D(I,J)
    LP_URB   = LP_URB2D(I,J)
    HGT_URB  = HGT_URB2D(I,J)
    LF_URB   = 0.0
    DO K = 1,4
      LF_URB(K) = LF_URB2D(I,K,J)
    ENDDO
    FRC_URB  = FRC_URB2D(I,J)
    LB_URB   = LB_URB2D(I,J)
    CHECK    = 0
    IF (I.EQ.73.AND.J.EQ.125)THEN
      CHECK = 1
    END IF

! Call urban

    CALL cal_mon_day(julian,julyr,jmonth,jday)
    CALL urban(LSOLAR_URB,                                                             & ! I
          num_roof_layers, num_wall_layers, num_road_layers,                           & ! C
                DZR,        DZB,        DZG, & ! C
          UTYPE_URB,     TA_URB,     QA_URB,     UA_URB,   U1_URB,  V1_URB, SSG_URB,   & ! I
           SSGD_URB,   SSGQ_URB,    LLG_URB,   RAIN_URB, RHOO_URB,                     & ! I
             ZA_URB, DECLIN_URB,   COSZ_URB,    OMG_URB,                               & ! I
           XLAT_URB,   DELT_URB,    ZNT_URB,                                           & ! I
            CHS_URB,   CHS2_URB,                                                       & ! I
             TR_URB,     TB_URB,     TG_URB,     TC_URB,   QC_URB,   UC_URB,           & ! H
            TRL_URB,    TBL_URB,    TGL_URB,                                           & ! H
           XXXR_URB,   XXXB_URB,   XXXG_URB,   XXXC_URB,                               & ! H
             TS_URB,     QS_URB,     SH_URB,     LH_URB, LH_KINEMATIC_URB,             & ! O
             SW_URB,    ALB_URB,     LW_URB,      G_URB,   RN_URB, PSIM_URB, PSIH_URB, & ! O
         GZ1OZ0_URB,                                                                   & !O
            CMR_URB,    CHR_URB,    CMC_URB,    CHC_URB,                               &
            U10_URB,    V10_URB,    TH2_URB,     Q2_URB,                               & ! O
            UST_URB,     mh_urb,   stdh_urb,     lf_urb,   lp_urb,                     & ! 0
            hgt_urb,    frc_urb,     lb_urb,      check, CMCR_URB,TGR_URB,             & ! H
           TGRL_URB,    SMR_URB,   CMGR_URB,   CHGR_URB,   jmonth,                     & ! H
          DRELR_URB,  DRELB_URB,                                                       & ! H
          DRELG_URB,FLXHUMR_URB,FLXHUMB_URB,FLXHUMG_URB )

    TS_URB2D(I,J) = TS_URB

    ALBEDO(I,J)   = FRC_URB2D(I,J) * ALB_URB + (1-FRC_URB2D(I,J)) * ALBEDO(I,J)        ![-]      
    HFX(I,J)      = FRC_URB2D(I,J) * SH_URB  + (1-FRC_URB2D(I,J)) * HFX(I,J)           ![W/m/m] 
    QFX(I,J)      = FRC_URB2D(I,J) * LH_KINEMATIC_URB &
                       + (1-FRC_URB2D(I,J))* QFX(I,J)                                  ![kg/m/m/s] 
    LH(I,J)       = FRC_URB2D(I,J) * LH_URB  + (1-FRC_URB2D(I,J)) * LH(I,J)            ![W/m/m]   
    GRDFLX(I,J)   = FRC_URB2D(I,J) * (G_URB) + (1-FRC_URB2D(I,J)) * GRDFLX(I,J)        ![W/m/m]  
    TSK(I,J)      = FRC_URB2D(I,J) * TS_URB  + (1-FRC_URB2D(I,J)) * TSK(I,J)           ![K]    
!    Q1            = QSFC(I,J)/(1.0+QSFC(I,J))                                         
!    Q1            = FRC_URB2D(I,J) * QS_URB  + (1-FRC_URB2D(I,J)) * Q1                 ![-]

! Convert QSFC back to mixing ratio

!    QSFC(I,J)     = Q1/(1.0-Q1)
                   QSFC(I,J)= FRC_URB2D(I,J)*QS_URB+(1-FRC_URB2D(I,J))*QSFC(I,J)               !!   QSFC(I,J)=QSFC1D
    UST(I,J)      = FRC_URB2D(I,J) * UST_URB + (1-FRC_URB2D(I,J)) * UST(I,J)     ![m/s]

! Renew Urban State Variables

    TR_URB2D(I,J) = TR_URB
    TB_URB2D(I,J) = TB_URB
    TG_URB2D(I,J) = TG_URB
    TC_URB2D(I,J) = TC_URB
    QC_URB2D(I,J) = QC_URB
    UC_URB2D(I,J) = UC_URB

    TGR_URB2D(I,J)     = TGR_URB
    CMCR_URB2D(I,J)    = CMCR_URB
    FLXHUMR_URB2D(I,J) = FLXHUMR_URB
    FLXHUMB_URB2D(I,J) = FLXHUMB_URB
    FLXHUMG_URB2D(I,J) = FLXHUMG_URB
    DRELR_URB2D(I,J)   = DRELR_URB
    DRELB_URB2D(I,J)   = DRELB_URB
    DRELG_URB2D(I,J)   = DRELG_URB

    DO K = 1,num_roof_layers
      TRL_URB3D(I,K,J) = TRL_URB(K)
      SMR_URB3D(I,K,J) = SMR_URB(K)
      TGRL_URB3D(I,K,J)= TGRL_URB(K)
    END DO
    DO K = 1,num_wall_layers
      TBL_URB3D(I,K,J) = TBL_URB(K)
    END DO
    DO K = 1,num_road_layers
      TGL_URB3D(I,K,J) = TGL_URB(K)
    END DO

    XXXR_URB2D(I,J)    = XXXR_URB
    XXXB_URB2D(I,J)    = XXXB_URB
    XXXG_URB2D(I,J)    = XXXG_URB
    XXXC_URB2D(I,J)    = XXXC_URB

    SH_URB2D(I,J)      = SH_URB
    LH_URB2D(I,J)      = LH_URB
    G_URB2D(I,J)       = G_URB         
    RN_URB2D(I,J)      = RN_URB
    PSIM_URB2D(I,J)    = PSIM_URB
    PSIH_URB2D(I,J)    = PSIH_URB
    GZ1OZ0_URB2D(I,J)  = GZ1OZ0_URB
    U10_URB2D(I,J)     = U10_URB
    V10_URB2D(I,J)     = V10_URB
    TH2_URB2D(I,J)     = TH2_URB
    Q2_URB2D(I,J)      = Q2_URB
    UST_URB2D(I,J)     = UST_URB
    AKMS_URB2D(I,J)    = KARMAN * UST_URB2D(I,J)/(GZ1OZ0_URB2D(I,J)-PSIM_URB2D(I,J))
    IF (PRESENT(CMR_SFCDIF)) THEN
      CMR_SFCDIF(I,J)  = CMR_URB
      CHR_SFCDIF(I,J)  = CHR_URB
      CMGR_SFCDIF(I,J) = CMGR_URB
      CHGR_SFCDIF(I,J) = CHGR_URB
      CMC_SFCDIF(I,J)  = CMC_URB
      CHC_SFCDIF(I,J)  = CHC_URB
    ENDIF

  ENDIF                                 ! urban land used type block

ENDDO ILOOP                             ! of I loop
ENDDO JLOOP                             ! of J loop

ENDIF                                   ! sf_urban_physics = 1 block

!--------------------------------------
! URBAN CANOPY MODEL END
!--------------------------------------

!--------------------------------------
! URBAN BEP and BEM MODEL BEGIN
!--------------------------------------

IF (SF_URBAN_PHYSICS == 2) THEN

DO J=JTS,JTE
DO I=ITS,ITE

  EMISS_URB(I,J)       = 0.
  RL_UP_URB(I,J)       = 0.
  RS_ABS_URB(I,J)      = 0.
  GRDFLX_URB(I,J)      = 0.
  B_Q_BEP(I,KTS:KTE,J) = 0.

END DO
END DO

  CALL BEP(frc_urb2d,  utype_urb2d, itimestep,       dz8w,         &
                  dt,        u_phy,     v_phy,                     &
              th_phy,          rho,     p_phy,     swdown,    glw, &
                 gmt,       julday,     xlong,       xlat,         &
          declin_urb,   cosz_urb2d, omg_urb2d,                     &
    num_urban_layers, num_urban_hi,                                &
           trb_urb4d,    tw1_urb4d, tw2_urb4d,  tgb_urb4d,         &
          sfw1_urb3d,   sfw2_urb3d, sfr_urb3d,  sfg_urb3d,         &
            lp_urb2d,     hi_urb2d,  lb_urb2d,  hgt_urb2d,         &
             a_u_bep,      a_v_bep,   a_t_bep,                     &
             a_e_bep,      b_u_bep,   b_v_bep,                     &
             b_t_bep,      b_e_bep,   b_q_bep,    dlg_bep,         &
            dl_u_bep,       sf_bep,    vl_bep,                     &
           rl_up_urb,   rs_abs_urb, emiss_urb, grdflx_urb,         &
         ids,ide, jds,jde, kds,kde,                                &
         ims,ime, jms,jme, kms,kme,                                &
         its,ite, jts,jte, kts,kte )

ENDIF ! SF_URBAN_PHYSICS == 2

IF (SF_URBAN_PHYSICS == 3) THEN

DO J=JTS,JTE
DO I=ITS,ITE

  EMISS_URB(I,J)       = 0.
  RL_UP_URB(I,J)       = 0.
  RS_ABS_URB(I,J)      = 0.
  GRDFLX_URB(I,J)      = 0.
  B_Q_BEP(I,KTS:KTE,J) = 0.

END DO
END DO

  CALL BEP_BEM( frc_urb2d,  utype_urb2d,    itimestep,         dz8w,       &
                       dt,        u_phy,        v_phy,                     &
                   th_phy,          rho,        p_phy,       swdown,  glw, &
                      gmt,       julday,        xlong,         xlat,       &
               declin_urb,   cosz_urb2d,    omg_urb2d,                     &
         num_urban_layers, num_urban_hi,                                   &
                trb_urb4d,    tw1_urb4d,    tw2_urb4d,    tgb_urb4d,       &
               tlev_urb3d,   qlev_urb3d, tw1lev_urb3d, tw2lev_urb3d,       &
              tglev_urb3d,  tflev_urb3d,  sf_ac_urb3d,  lf_ac_urb3d,       &
              cm_ac_urb3d, sfvent_urb3d, lfvent_urb3d,                     &
             sfwin1_urb3d, sfwin2_urb3d,                                   &
               sfw1_urb3d,   sfw2_urb3d,    sfr_urb3d,    sfg_urb3d,       &
                 lp_urb2d,     hi_urb2d,     lb_urb2d,    hgt_urb2d,       &
                  a_u_bep,      a_v_bep,      a_t_bep,                     &
                  a_e_bep,      b_u_bep,      b_v_bep,                     &
                  b_t_bep,      b_e_bep,      b_q_bep,      dlg_bep,       &
                 dl_u_bep,       sf_bep,       vl_bep,                     &
                rl_up_urb,   rs_abs_urb,    emiss_urb,   grdflx_urb, qv3d, &
             ids,ide, jds,jde, kds,kde,                                    &
             ims,ime, jms,jme, kms,kme,                                    &
             its,ite, jts,jte, kts,kte )

ENDIF ! SF_URBAN_PHYSICS == 3

IF((SF_URBAN_PHYSICS == 2).OR.(SF_URBAN_PHYSICS == 3))THEN 

  sigma_sb=5.67e-08
  do j = jts, jte
  do i = its, ite
    UMOM_URB(I,J)     = 0.
    VMOM_URB(I,J)     = 0.
    HFX_URB(I,J)      = 0.
    QFX_URB(I,J)      = 0.

    do k=kts,kte
      a_u_bep(i,k,j) = a_u_bep(i,k,j)*frc_urb2d(i,j)
      a_v_bep(i,k,j) = a_v_bep(i,k,j)*frc_urb2d(i,j)
      a_t_bep(i,k,j) = a_t_bep(i,k,j)*frc_urb2d(i,j)
      a_q_bep(i,k,j) = 0.
      a_e_bep(i,k,j) = 0.
      b_u_bep(i,k,j) = b_u_bep(i,k,j)*frc_urb2d(i,j)
      b_v_bep(i,k,j) = b_v_bep(i,k,j)*frc_urb2d(i,j)
      b_t_bep(i,k,j) = b_t_bep(i,k,j)*frc_urb2d(i,j)
      b_q_bep(i,k,j) = b_q_bep(i,k,j)*frc_urb2d(i,j)
      b_e_bep(i,k,j) = b_e_bep(i,k,j)*frc_urb2d(i,j)
      HFX_URB(I,J)   = HFX_URB(I,J) + B_T_BEP(I,K,J)*RHO(I,K,J)*CP*DZ8W(I,K,J)*VL_BEP(I,K,J)
      QFX_URB(I,J)   = QFX_URB(I,J) + B_Q_BEP(I,K,J)*DZ8W(I,K,J)*VL_BEP(I,K,J)
      UMOM_URB(I,J)  = UMOM_URB(I,J)+ (A_U_BEP(I,K,J)*U_PHY(I,K,J)+B_U_BEP(I,K,J))*DZ8W(I,K,J)*VL_BEP(I,K,J)
      VMOM_URB(I,J)  = VMOM_URB(I,J)+ (A_V_BEP(I,K,J)*V_PHY(I,K,J)+B_V_BEP(I,K,J))*DZ8W(I,K,J)*VL_BEP(I,K,J)
      vl_bep(i,k,j)  = (1.-frc_urb2d(i,j)) + vl_bep(i,k,j)*frc_urb2d(i,j)
      sf_bep(i,k,j)  = (1.-frc_urb2d(i,j)) + sf_bep(i,k,j)*frc_urb2d(i,j)
    end do

    a_u_bep(i,1,j)   = (1.-frc_urb2d(i,j))*(-ust(I,J)*ust(I,J))/dz8w(i,1,j)/   &
                          ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+a_u_bep(i,1,j)

    a_v_bep(i,1,j)   = (1.-frc_urb2d(i,j))*(-ust(I,J)*ust(I,J))/dz8w(i,1,j)/   &
                          ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+a_v_bep(i,1,j)

    b_t_bep(i,1,j)   = (1.-frc_urb2d(i,j))*hfx_rural(i,j)/dz8w(i,1,j)/rho(i,1,j)/CP+ & 
                           b_t_bep(i,1,j)

    b_q_bep(i,1,j)   = (1.-frc_urb2d(i,j))*qfx_rural(i,j)/dz8w(i,1,j)/rho(i,1,j)+b_q_bep(i,1,j)

    umom             = (1.-frc_urb2d(i,j))*ust(i,j)*ust(i,j)*u_phy(i,1,j)/               &
                         ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+umom_urb(i,j)

    vmom             = (1.-frc_urb2d(i,j))*ust(i,j)*ust(i,j)*v_phy(i,1,j)/               &
                         ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+vmom_urb(i,j)
    sf_bep(i,1,j)    = 1.

! using the emissivity and the total longwave upward radiation estimate the averaged skin temperature

  IF (FRC_URB2D(I,J).GT.0.) THEN
    rl_up_rural   = -emiss_rural(i,j)*sigma_sb*(tsk_rural(i,j)**4.)-(1.-emiss_rural(i,j))*glw(i,j)
    rl_up_tot     = (1.-frc_urb2d(i,j))*rl_up_rural     + frc_urb2d(i,j)*rl_up_urb(i,j)
    emiss(i,j)    = (1.-frc_urb2d(i,j))*emiss_rural(i,j)+ frc_urb2d(i,j)*emiss_urb(i,j)
    ts_urb2d(i,j) = (max(0.,(-rl_up_urb(i,j)-(1.-emiss_urb(i,j))*glw(i,j))/emiss_urb(i,j)/sigma_sb))**0.25
    tsk(i,j)      = (max(0., (-1.*rl_up_tot-(1.-emiss(i,j))*glw(i,j) )/emiss(i,j)/sigma_sb))**.25
    rs_abs_tot    = (1.-frc_urb2d(i,j))*swdown(i,j)*(1.-albedo(i,j))+frc_urb2d(i,j)*rs_abs_urb(i,j)

    if(swdown(i,j) > 0.)then
      albedo(i,j) = 1.-rs_abs_tot/swdown(i,j)
    else
      albedo(i,j) = alb_rural(i,j)
    endif

! rename *_urb to sh_urb2d,lh_urb2d,g_urb2d,rn_urb2d

    grdflx(i,j)   = (1.-frc_urb2d(i,j))*grdflx_rural(i,j)+ frc_urb2d(i,j)*grdflx_urb(i,j)
    qfx(i,j)      = (1.-frc_urb2d(i,j))*qfx_rural(i,j)   + qfx_urb(i,j)
    lh(i,j)       = qfx(i,j)*xlv
    hfx(i,j)      = hfx_urb(i,j)                         + (1-frc_urb2d(i,j))*hfx_rural(i,j)      ![W/m/m]
    sh_urb2d(i,j) = hfx_urb(i,j)/frc_urb2d(i,j)
    lh_urb2d(i,j) = qfx_urb(i,j)*xlv
    g_urb2d(i,j)  = grdflx_urb(i,j)
    rn_urb2d(i,j) = rs_abs_urb(i,j)+emiss_urb(i,j)*glw(i,j)-rl_up_urb(i,j)
    ust(i,j)      = (umom**2.+vmom**2.)**.25

  ELSE

    sh_urb2d(i,j)    = 0.
    lh_urb2d(i,j)    = 0.
    g_urb2d(i,j)     = 0.
    rn_urb2d(i,j)    = 0.

  ENDIF

  enddo ! jloop
  enddo ! iloop

ENDIF ! SF_URBAN_PHYSICS == 2 or 3

!--------------------------------------
! URBAN BEP and BEM MODEL END
!--------------------------------------


END SUBROUTINE noahmp_urban

!------------------------------------------------------------------------------------------
!
END MODULE module_sf_noahmpdrv
