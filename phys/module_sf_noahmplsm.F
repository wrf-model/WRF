!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

MODULE MODULE_SF_NOAHMPLSM

  use module_sf_gecros, only : gecros

  IMPLICIT NONE

  public  :: noahmp_options
  public  :: NOAHMP_SFLX

  private :: ATM
  private :: PHENOLOGY
  private :: PRECIP_HEAT
  private :: ENERGY
  private ::       THERMOPROP
  private ::               CSNOW
  private ::               TDFCND
  private ::       RADIATION
  private ::               ALBEDO
  private ::                         SNOW_AGE
  private ::                         SNOWALB_BATS  
  private ::                         SNOWALB_CLASS
  private ::                         GROUNDALB
  private ::                         TWOSTREAM
  private ::               SURRAD
  private ::       VEGE_FLUX
  private ::               SFCDIF1                  
  private ::               SFCDIF2                
  private ::               STOMATA                  
  private ::               CANRES                  
  private ::               ESAT
  private ::               RAGRB
  private ::       BARE_FLUX
  private ::       TSNOSOI
  private ::               HRT
  private ::               HSTEP   
  private ::                         ROSR12
  private ::       PHASECHANGE
  private ::               FRH2O           

  private :: WATER
  private ::       CANWATER
  private ::       SNOWWATER
  private ::               SNOWFALL
  private ::               COMBINE
  private ::               DIVIDE
  private ::                         COMBO
  private ::               COMPACT
  private ::               SNOWH2O
  private ::       SOILWATER
  private ::               ZWTEQ
  private ::               INFIL
  private ::               SRT
  private ::                         WDFCND1        
  private ::                         WDFCND2       
  private ::               SSTEP
  private ::       GROUNDWATER
  private ::       SHALLOWWATERTABLE

  private :: CARBON
  private ::       CO2FLUX
!  private ::       BVOCFLUX
!  private ::       CH4FLUX

  private :: ERROR

! =====================================options for different schemes================================
! **recommended

  INTEGER :: DVEG     ! options for dynamic vegetation: 
                      !   1 -> off (use table LAI; use FVEG = SHDFAC from input)
                      !   2 -> on  (together with OPT_CRS = 1)
                      !   3 -> off (use table LAI; calculate FVEG)
                      ! **4 -> off (use table LAI; use maximum vegetation fraction)
                      ! **5 -> on  (use maximum vegetation fraction)
                      !   6 -> on  (use FVEG = SHDFAC from input)
                      !   7 -> off (use input LAI; use FVEG = SHDFAC from input)
                      !   8 -> off (use input LAI; calculate FVEG)
                      !   9 -> off (use input LAI; use maximum vegetation fraction)
                      !  10 -> crop model on (use maximum vegetation fraction)

  INTEGER :: OPT_CRS  ! options for canopy stomatal resistance
                      ! **1 -> Ball-Berry
		      !   2 -> Jarvis

  INTEGER :: OPT_BTR  ! options for soil moisture factor for stomatal resistance
                      ! **1 -> Noah (soil moisture) 
                      !   2 -> CLM  (matric potential)
                      !   3 -> SSiB (matric potential)

  INTEGER :: OPT_RUN  ! options for runoff and groundwater
                      ! **1 -> TOPMODEL with groundwater (Niu et al. 2007 JGR) ;
                      !   2 -> TOPMODEL with an equilibrium water table (Niu et al. 2005 JGR) ;
                      !   3 -> original surface and subsurface runoff (free drainage)
                      !   4 -> BATS surface and subsurface runoff (free drainage)
                      !   5 -> Miguez-Macho&Fan groundwater scheme (Miguez-Macho et al. 2007 JGR; Fan et al. 2007 JGR)
		      !          (needs further testing for public use)

  INTEGER :: OPT_SFC  ! options for surface layer drag coeff (CH & CM)
                      ! **1 -> M-O
		      ! **2 -> original Noah (Chen97)
		      ! **3 -> MYJ consistent; 4->YSU consistent. MB: removed in v3.7 for further testing

  INTEGER :: OPT_FRZ  ! options for supercooled liquid water (or ice fraction)
                      ! **1 -> no iteration (Niu and Yang, 2006 JHM)
		      !   2 -> Koren's iteration 

  INTEGER :: OPT_INF  ! options for frozen soil permeability
                      ! **1 -> linear effects, more permeable (Niu and Yang, 2006, JHM)
                      !   2 -> nonlinear effects, less permeable (old)

  INTEGER :: OPT_RAD  ! options for radiation transfer
                      !   1 -> modified two-stream (gap = F(solar angle, 3D structure ...)<1-FVEG)
                      !   2 -> two-stream applied to grid-cell (gap = 0)
                      ! **3 -> two-stream applied to vegetated fraction (gap=1-FVEG)

  INTEGER :: OPT_ALB  ! options for ground snow surface albedo
                      !   1 -> BATS
		      ! **2 -> CLASS

  INTEGER :: OPT_SNF  ! options for partitioning  precipitation into rainfall & snowfall
                      ! **1 -> Jordan (1991)
		      !   2 -> BATS: when SFCTMP<TFRZ+2.2 
		      !   3 -> SFCTMP < TFRZ
		      !   4 -> Use WRF microphysics output
		      !   5 -> Use wetbulb temperature (Wang et al., 2019 GRL) C.He, 12/18/2020
 
  INTEGER :: OPT_TBOT ! options for lower boundary condition of soil temperature
                      !   1 -> zero heat flux from bottom (ZBOT and TBOT not used)
                      ! **2 -> TBOT at ZBOT (8m) read from a file (original Noah)

  INTEGER :: OPT_STC  ! options for snow/soil temperature time scheme (only layer 1)
                      ! **1 -> semi-implicit; flux top boundary condition
		      !   2 -> full implicit (original Noah); temperature top boundary condition
                      !   3 -> same as 1, but FSNO for TS calculation (generally improves snow; v3.7)

  INTEGER :: OPT_RSF  ! options for surface resistent to evaporation/sublimation
                      ! **1 -> Sakaguchi and Zeng, 2009
		      !   2 -> Sellers (1992)
                      !   3 -> adjusted Sellers to decrease RSURF for wet soil
		      !   4 -> option 1 for non-snow; rsurf = rsurf_snow for snow (set in MPTABLE); AD v3.8

  INTEGER :: OPT_SOIL ! options for defining soil properties
                      ! **1 -> use input dominant soil texture
		      !   2 -> use input soil texture that varies with depth
                      !   3 -> use soil composition (sand, clay, orgm) and pedotransfer functions (OPT_PEDO)
		      !   4 -> use input soil properties (BEXP_3D, SMCMAX_3D, etc.)

  INTEGER :: OPT_PEDO ! options for pedotransfer functions (used when OPT_SOIL = 3)
                      ! **1 -> Saxton and Rawls (2006)

  INTEGER :: OPT_CROP ! options for crop model
                      ! **0 -> No crop model, will run default dynamic vegetation
                      !   1 -> Liu, et al. 2016
		      !   2 -> Gecros (Genotype-by-Environment interaction on CROp growth Simulator) Yin and van Laar, 2005

    REAL :: SCFFAC             !snow cover factor (m) (originally hard-coded 2.5*z0 in SCF formulation)
    REAL :: SAIM(12)           !monthly stem area index, one-sided
    REAL :: LAIM(12)           !monthly leaf area index, one-sided
    REAL :: SLA                !single-side leaf area per Kg [m2/kg]
    REAL :: DILEFC             !coeficient for leaf stress death [1/s]
    REAL :: DILEFW             !coeficient for leaf stress death [1/s]
    REAL :: FRAGR              !fraction of growth respiration  !original was 0.3 
    REAL :: LTOVRC             !leaf turnover [1/s]

    REAL :: C3PSN              !photosynthetic pathway: 0. = c4, 1. = c3
    REAL :: KC25               !co2 michaelis-menten constant at 25c (pa)
    REAL :: AKC                !q10 for kc25
    REAL :: KO25               !o2 michaelis-menten constant at 25c (pa)
    REAL :: AKO                !q10 for ko25
    REAL :: VCMX25             !maximum rate of carboxylation at 25c (umol co2/m**2/s)
    REAL :: AVCMX              !q10 for vcmx25
    REAL :: BP                 !minimum leaf conductance (umol/m**2/s)
    REAL :: MP                 !slope of conductance-to-photosynthesis relationship
    REAL :: QE25               !quantum efficiency at 25c (umol co2 / umol photon)
    REAL :: AQE                !q10 for qe25
    REAL :: RMF25              !leaf maintenance respiration at 25c (umol co2/m**2/s)
    REAL :: RMS25              !stem maintenance respiration at 25c (umol co2/kg bio/s)
    REAL :: RMR25              !root maintenance respiration at 25c (umol co2/kg bio/s)
    REAL :: ARM                !q10 for maintenance respiration
    REAL :: FOLNMX             !foliage nitrogen concentration when f(n)=1 (%)
    REAL :: TMIN               !minimum temperature for photosynthesis (k)
       
    REAL :: XL                 !leaf/stem orientation index
    REAL :: RHOL(MBAND)        !leaf reflectance: 1=vis, 2=nir
    REAL :: RHOS(MBAND)        !stem reflectance: 1=vis, 2=nir
    REAL :: TAUL(MBAND)        !leaf transmittance: 1=vis, 2=nir
    REAL :: TAUS(MBAND)        !stem transmittance: 1=vis, 2=nir

    REAL :: MRP                !microbial respiration parameter (umol co2 /kg c/ s)
    REAL :: CWPVT              !empirical canopy wind parameter

    REAL :: WRRAT              !wood to non-wood ratio
    REAL :: WDPOOL             !wood pool (switch 1 or 0) depending on woody or not [-]
    REAL :: TDLEF              !characteristic T for leaf freezing [K]

  INTEGER :: NROOT              !number of soil layers with root present
     REAL :: RGL                !Parameter used in radiation stress function
     REAL :: RSMIN              !Minimum stomatal resistance [s m-1]
     REAL :: HS                 !Parameter used in vapor pressure deficit function
     REAL :: TOPT               !Optimum transpiration air temperature [K]
     REAL :: RSMAX              !Maximal stomatal resistance [s m-1]

     REAL :: SLAREA
     REAL :: EPS(5)

!------------------------------------------------------------------------------------------!
! From the rad section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!

     REAL :: ALBSAT(MBAND)       !saturated soil albedos: 1=vis, 2=nir
     REAL :: ALBDRY(MBAND)       !dry soil albedos: 1=vis, 2=nir
     REAL :: ALBICE(MBAND)       !albedo land ice: 1=vis, 2=nir
     REAL :: ALBLAK(MBAND)       !albedo frozen lakes: 1=vis, 2=nir
     REAL :: OMEGAS(MBAND)       !two-stream parameter omega for snow
     REAL :: BETADS              !two-stream parameter betad for snow
     REAL :: BETAIS              !two-stream parameter betad for snow
     REAL :: EG(2)               !emissivity

!------------------------------------------------------------------------------------------!
! From the globals section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!
 
     REAL :: CO2          !co2 partial pressure
     REAL :: O2           !o2 partial pressure
     REAL :: TIMEAN       !gridcell mean topgraphic index (global mean)
     REAL :: FSATMX       !maximum surface saturated fraction (global mean)
     REAL :: Z0SNO        !snow surface roughness length (m) (0.002)
     REAL :: SSI          !liquid water holding capacity for snowpack (m3/m3)
! From the irrigation section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!
     REAL :: IRR_FRAC         ! irrigation Fraction
  INTEGER :: IRR_HAR          ! number of days before harvest date to stop irrigation 
     REAL :: IRR_LAI          ! Minimum lai to trigger irrigation
     REAL :: IRR_MAD          ! management allowable deficit (0-1)
     REAL :: FILOSS           ! fraction of flood irrigation loss (0-1) 
     REAL :: SPRIR_RATE       ! mm/h, sprinkler irrigation rate
     REAL :: MICIR_RATE       ! mm/h, micro irrigation rate
     REAL :: FIRTFAC          ! flood application rate factor
     REAL :: IR_RAIN          ! maximum precipitation to stop irrigation trigger

!------------------------------------------------------------------------------------------!
! From the crop section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!
 
  INTEGER :: PLTDAY           ! Planting date
  INTEGER :: HSDAY            ! Harvest date
     REAL :: PLANTPOP         ! Plant density [per ha] - used?
     REAL :: IRRI             ! Irrigation strategy 0= non-irrigation 1=irrigation (no water-stress)
     REAL :: GDDTBASE         ! Base temperature for GDD accumulation [C]
     REAL :: GDDTCUT          ! Upper temperature for GDD accumulation [C]
     REAL :: GDDS1            ! GDD from seeding to emergence
     REAL :: GDDS2            ! GDD from seeding to initial vegetative 
     REAL :: GDDS3            ! GDD from seeding to post vegetative 
     REAL :: GDDS4            ! GDD from seeding to intial reproductive
     REAL :: GDDS5            ! GDD from seeding to pysical maturity 
  INTEGER :: C3C4             ! photosynthetic pathway:  1 = c3 2 = c4
     REAL :: AREF             ! reference maximum CO2 assimulation rate 
     REAL :: PSNRF            ! CO2 assimulation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
     REAL :: I2PAR            ! Fraction of incoming solar radiation to photosynthetically active radiation
     REAL :: TASSIM0          ! Minimum temperature for CO2 assimulation [C]
     REAL :: TASSIM1          ! CO2 assimulation linearly increasing until temperature reaches T1 [C]
     REAL :: TASSIM2          ! CO2 assmilation rate remain at Aref until temperature reaches T2 [C]
     REAL :: K                ! light extinction coefficient
     REAL :: EPSI             ! initial light use efficiency
     REAL :: Q10MR            ! q10 for maintainance respiration
     REAL :: FOLN_MX          ! foliage nitrogen concentration when f(n)=1 (%)
     REAL :: LEFREEZ          ! characteristic T for leaf freezing [K]
     REAL :: DILE_FC(NSTAGE)  ! coeficient for temperature leaf stress death [1/s]
     REAL :: DILE_FW(NSTAGE)  ! coeficient for water leaf stress death [1/s]
     REAL :: FRA_GR           ! fraction of growth respiration 
     REAL :: LF_OVRC(NSTAGE)  ! fraction of leaf turnover  [1/s]
     REAL :: ST_OVRC(NSTAGE)  ! fraction of stem turnover  [1/s]
     REAL :: RT_OVRC(NSTAGE)  ! fraction of root tunrover  [1/s]
     REAL :: LFMR25           ! leaf maintenance respiration at 25C [umol CO2/m**2  /s]
     REAL :: STMR25           ! stem maintenance respiration at 25C [umol CO2/kg bio/s]
     REAL :: RTMR25           ! root maintenance respiration at 25C [umol CO2/kg bio/s]
     REAL :: GRAINMR25        ! grain maintenance respiration at 25C [umol CO2/kg bio/s]
     REAL :: LFPT(NSTAGE)     ! fraction of carbohydrate flux to leaf
     REAL :: STPT(NSTAGE)     ! fraction of carbohydrate flux to stem
     REAL :: RTPT(NSTAGE)     ! fraction of carbohydrate flux to root
                   IRRFRA  , SIFRA   , MIFRA   , FIFRA   , LLANDUSE,           & ! IN : Irrigation: fractions
                   ALBOLD  , SNEQVO  ,                                         & ! IN/OUT : 
                   STC     , SH2O    , SMC     , TAH     , EAH     , FWET    , & ! IN/OUT : 
                   CANLIQ  , CANICE  , TV      , TG      , QSFC    , QSNOW   , & ! IN/OUT :
                   QRAIN   ,                                                   & ! IN/OUT : 
                   ISNOW   , ZSNSO   , SNOWH   , SNEQV   , SNICE   , SNLIQ   , & ! IN/OUT : 
                   ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & ! IN/OUT : 
                   STMASS  , WOOD    , STBLCP  , FASTCP  , LAI     , SAI     , & ! IN/OUT : 
                   CM      , CH      , TAUSS   ,                               & ! IN/OUT : 
                   GRAIN   , GDD     , PGS     ,                               & ! IN/OUT 
                   SMCWTD  ,DEEPRECH , RECH    ,                               & ! IN/OUT :
                   GECROS1D,                                                   & ! IN/OUT :
		   Z0WRF   ,                                                   &
                   IRCNTSI , IRCNTMI , IRCNTFI , IRAMTSI , IRAMTMI , IRAMTFI , & ! IN/OUT : Irrigation: vars
                   IRSIRATE, IRMIRATE, IRFIRATE, FIRR    , EIRR    ,           & ! IN/OUT : Irrigation: vars
                   FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV    , & ! OUT : 
                   FGEV    , FCTR    , ECAN    , ETRAN   , EDIR    , TRAD    , & ! OUT :
                   TGB     , TGV     , T2MV    , T2MB    , Q2V     , Q2B     , & ! OUT :
                   RUNSRF  , RUNSUB  , APAR    , PSN     , SAV     , SAG     , & ! OUT :
                   FSNO    , NEE     , GPP     , NPP     , FVEG    , ALBEDO  , & ! OUT :
                   QSNBOT  , PONDING , PONDING1, PONDING2, RSSUN   , RSSHA   , & ! OUT :
		   ALBSND  , ALBSNI  ,                                         & ! OUT :
                   BGAP    , WGAP    , CHV     , CHB     , EMISSI  ,           & ! OUT :
		   SHG     , SHC     , SHB     , EVG     , EVB     , GHV     , & ! OUT :
		   GHB     , IRG     , IRC     , IRB     , TR      , EVC     , & ! OUT :
		   CHLEAF  , CHUC    , CHV2    , CHB2    , FPICE   , PAHV    , &
                   PAHG    , PAHB    , PAH     , LAISUN  , LAISHA  , RB        & ! OUT
#ifdef WRF_HYDRO
                   ,SFCHEADRT                                                  & ! IN/OUT :
#endif
                   )

! --------------------------------------------------------------------------------------------------
! Initial code: Guo-Yue Niu, Oct. 2007
! --------------------------------------------------------------------------------------------------

  implicit none
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters), INTENT(IN) :: parameters

  INTEGER                        , INTENT(IN)    :: ICE    !ice (ice = 1)
  INTEGER                        , INTENT(IN)    :: IST    !surface type 1->soil; 2->lake
  INTEGER                        , INTENT(IN)    :: VEGTYP !vegetation type 
  INTEGER                        , INTENT(IN)    :: CROPTYPE !crop type 
  INTEGER                        , INTENT(IN)    :: NSNOW  !maximum no. of snow layers        
  INTEGER                        , INTENT(IN)    :: NSOIL  !no. of soil layers        
  INTEGER                        , INTENT(IN)    :: ILOC   !grid index
  INTEGER                        , INTENT(IN)    :: JLOC   !grid index
  REAL                           , INTENT(IN)    :: DT     !time step [sec]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !layer-bottom depth from soil surf (m)
  REAL                           , INTENT(IN)    :: Q2     !mixing ratio (kg/kg) lowest model layer
  REAL                           , INTENT(IN)    :: SFCTMP !surface air temperature [K]
  REAL                           , INTENT(IN)    :: UU     !wind speed in eastward dir (m/s)
  REAL                           , INTENT(IN)    :: VV     !wind speed in northward dir (m/s)
  REAL                           , INTENT(IN)    :: SOLDN  !downward shortwave radiation (w/m2)
  REAL                           , INTENT(IN)    :: LWDN   !downward longwave radiation (w/m2)
  REAL                           , INTENT(IN)    :: SFCPRS !pressure (pa)
  REAL                           , INTENT(INOUT) :: ZLVL   !reference height (m)
  REAL                           , INTENT(IN)    :: COSZ   !cosine solar zenith angle [0-1]
  REAL                           , INTENT(IN)    :: TBOT   !bottom condition for soil temp. [K]
  REAL                           , INTENT(IN)    :: FOLN   !foliage nitrogen (%) [1-saturated]
  REAL                           , INTENT(IN)    :: SHDFAC !green vegetation fraction [0.0-1.0]
  INTEGER                        , INTENT(IN)    :: YEARLEN!Number of days in the particular year.
  REAL                           , INTENT(IN)    :: JULIAN !Julian day of year (floating point)
  REAL                           , INTENT(IN)    :: LAT    !latitude (radians)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD!ice fraction at last timestep
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: SMCEQ  !equilibrium soil water  content [m3/m3]
  REAL                           , INTENT(IN)    :: PRCPCONV ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPNONC ! non-convective precipitation entering [mm/s] ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPSHCV ! shallow convective precip entering  [mm/s]   ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPSNOW ! snow entering land model [mm/s]              ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPGRPL ! graupel entering land model [mm/s]           ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPHAIL ! hail entering land model [mm/s]              ! MB/AN : v3.7

!jref:start; in 
  REAL                           , INTENT(IN)    :: QC     !cloud water mixing ratio
  REAL                           , INTENT(INOUT)    :: QSFC   !mixing ratio at lowest model layer
  REAL                           , INTENT(IN)    :: PSFC   !pressure at lowest model layer
  REAL                           , INTENT(IN)    :: DZ8W   !thickness of lowest layer
  REAL                           , INTENT(IN)    :: DX
  REAL                           , INTENT(IN)    :: SHDMAX  !yearly max vegetation fraction
!jref:end

#ifdef WRF_HYDRO
  REAL                           , INTENT(INOUT)    :: sfcheadrt
#endif

! input/output : need arbitary intial values
  REAL                           , INTENT(INOUT) :: QSNOW  !snowfall [mm/s]
! irrigation variables 
  REAL                        , INTENT(IN)       :: IRRFRA         ! irrigation fraction
  REAL                        , INTENT(IN)       :: SIFRA          ! sprinkler irrigation fraction
  REAL                        , INTENT(IN)       :: MIFRA          ! micro irrigation fraction
  REAL                        , INTENT(IN)       :: FIFRA          ! flood irrigation fraction   
  INTEGER                     , INTENT(INOUT)    :: IRCNTSI        ! irrigation event number, Sprinkler
  INTEGER                     , INTENT(INOUT)    :: IRCNTMI        ! irrigation event number, Micro
  INTEGER                     , INTENT(INOUT)    :: IRCNTFI        ! irrigation event number, Flood 
  REAL                        , INTENT(INOUT)    :: IRAMTSI        ! irrigation water amount [m] to be applied, Sprinkler
  REAL                        , INTENT(INOUT)    :: IRAMTMI        ! irrigation water amount [m] to be applied, Micro
  REAL                        , INTENT(INOUT)    :: IRAMTFI        ! irrigation water amount [m] to be applied, Flood
  REAL                        , INTENT(INOUT)    :: IRSIRATE       ! rate of irrigation by sprinkler [m/timestep]
  REAL                        , INTENT(INOUT)    :: IRMIRATE       ! rate of irrigation by micro [m/timestep]
  REAL                        , INTENT(INOUT)    :: IRFIRATE       ! rate of irrigation by micro [m/timestep]
  REAL                        , INTENT(INOUT)    :: FIRR           ! irrigation:latent heating due to sprinkler evaporation [w/m2]
  REAL                        , INTENT(INOUT)    :: EIRR           ! evaporation of irrigation water to evaporation,sprinkler [mm/s]
  CHARACTER(LEN=256)          , INTENT(IN)       :: LLANDUSE       ! landuse data name (USGS or MODIS_IGBP)
  REAL                                           :: IREVPLOS       ! loss of irrigation water to evaporation,sprinkler [m/timestep]
  REAL                                           :: SIFAC          ! sprinkler irrigation fraction (local)
  REAL                                           :: MIFAC          ! micro irrigation fraction (local)
  REAL                                           :: FIFAC          ! flood irrigation fraction (local)

! outputs
  REAL                          , INTENT(OUT)    :: NEE    !net ecosys exchange (g/m2/s CO2)
  REAL                          , INTENT(OUT)    :: GPP    !net instantaneous assimilation [g/m2/s C]
  REAL                          , INTENT(OUT)    :: NPP    !net primary productivity [g/m2/s C]
  REAL                                           :: AUTORS !net ecosystem respiration (g/m2/s C)
  REAL                                           :: HETERS !organic respiration (g/m2/s C)
  REAL                                           :: TROOT  !root-zone averaged temperature (k)
  REAL                                           :: BDFALL   !bulk density of new snow (kg/m3)    ! MB/AN: v3.7
  REAL                                           :: RAIN     !rain rate                   (mm/s)  ! MB/AN: v3.7
  REAL                                           :: SNOW     !liquid equivalent snow rate (mm/s)  ! MB/AN: v3.7
  REAL                                           :: FP                                            ! MB/AN: v3.7
  REAL                                           :: PRCP                                          ! MB/AN: v3.7
!more local variables for precip heat MB
  REAL                                           :: QINTR   !interception rate for rain (mm/s)
  REAL                                           :: QDRIPR  !drip rate for rain (mm/s)
  REAL                                           :: QTHROR  !throughfall for rain (mm/s)
  REAL                                           :: QINTS   !interception (loading) rate for snowfall (mm/s)
  REAL                                           :: QDRIPS  !drip (unloading) rate for intercepted snow (mm/s)
  REAL                                           :: QTHROS  !throughfall of snowfall (mm/s)
  REAL                                           :: SNOWHIN !snow depth increasing rate (m/s)
  REAL                                 :: LATHEAV !latent heat vap./sublimation (j/kg)
  REAL                                 :: LATHEAG !latent heat vap./sublimation (j/kg)
  LOGICAL                             :: FROZEN_GROUND ! used to define latent heat pathway
  LOGICAL                             :: FROZEN_CANOPY ! used to define latent heat pathway
  LOGICAL                             :: dveg_active ! flag to run dynamic vegetation
  LOGICAL                             :: crop_active ! flag to run crop model
  LOGICAL                             :: CROPLU      ! flag to identify croplands
  REAL                                :: SIFCUK      ! Sprinkler fraction for unknown irrigation methods
  REAL :: FB
  
  ! INTENT (OUT) variables need to be assigned a value.  These normally get assigned values
  ! only if DVEG == 2.
  nee = 0.0
  npp = 0.0
  gpp = 0.0
      PAHV  = 0.
      PAHG  = 0.
      PAHB  = 0.
      PAH  = 0.
  CROPLU = .FALSE.
  
! --------------------------------------------------------------------------------------------------
! re-process atmospheric forcing

   CALL ATM (parameters,SFCPRS  ,SFCTMP   ,Q2      ,                            &
             PRCPCONV, PRCPNONC,PRCPSHCV,PRCPSNOW,PRCPGRPL,PRCPHAIL, &
             SOLDN   ,COSZ     ,THAIR   ,QAIR    ,                   & 
             EAIR    ,RHOAIR   ,QPRECC  ,QPRECL  ,SOLAD   ,SOLAI   , &
             SWDOWN  ,BDFALL   ,RAIN    ,SNOW    ,FP      ,FPICE   , PRCP )     

! snow/soil layer thickness (m)

     DO IZ = ISNOW+1, NSOIL
         IF(IZ == ISNOW+1) THEN
           DZSNSO(IZ) = - ZSNSO(IZ)
         ELSE
           DZSNSO(IZ) = ZSNSO(IZ-1) - ZSNSO(IZ)
         END IF
     END DO

! root-zone temperature

     TROOT  = 0.
     DO IZ=1,parameters%NROOT
        TROOT = TROOT + STC(IZ)*DZSNSO(IZ)/(-ZSOIL(parameters%NROOT))
     ENDDO

! total water storage for water balance check
    
     IF(IST == 1) THEN
     BEG_WB = CANLIQ + CANICE + SNEQV + WA
     DO IZ = 1,NSOIL
        BEG_WB = BEG_WB + SMC(IZ) * DZSNSO(IZ) * 1000.
     END DO
     END IF

! vegetation phenology

     CALL PHENOLOGY (parameters,VEGTYP ,croptype, SNOWH  , TV     , LAT   , YEARLEN , JULIAN , & !in
                     LAI    , SAI    , TROOT  , ELAI    , ESAI   ,IGS, PGS)

!input GVF should be consistent with LAI
     IF(DVEG == 1 .or. DVEG == 6 .or. DVEG == 7) THEN
        FVEG = SHDFAC
        IF(FVEG <= 0.05) FVEG = 0.05
     ELSE IF (DVEG == 2 .or. DVEG == 3 .or. DVEG == 8) THEN
        FVEG = 1.-EXP(-0.52*(LAI+SAI))
        IF(FVEG <= 0.05) FVEG = 0.05
     ELSE IF (DVEG == 4 .or. DVEG == 5 .or. DVEG == 9) THEN
        FVEG = SHDMAX
        IF(FVEG <= 0.05) FVEG = 0.05
     ELSE
        WRITE(*,*) "-------- FATAL CALLED IN SFLX -----------"
        CALL wrf_error_fatal("Namelist parameter DVEG unknown") 
     ENDIF
     IF(OPT_CROP > 0 .and. CROPTYPE > 0) THEN
        FVEG = SHDMAX
        IF(FVEG <= 0.05) FVEG = 0.05
     ENDIF
     IF(parameters%urban_flag .OR. VEGTYP == parameters%ISBARREN) FVEG = 0.0
     IF(ELAI+ESAI == 0.0) FVEG = 0.0

                 IRRFRA ,MIFAC  ,FIFAC  ,CROPLU ,                 & !in
                 FICEOLD,PONDING,TG     ,IST    ,FVEG   ,iloc,jloc , SMCEQ , & !in
                 BDFALL ,FP     ,RAIN   ,SNOW   ,                 & !in  MB/AN: v3.7
		 QSNOW  ,QRAIN  ,SNOWHIN,LATHEAV,LATHEAG,frozen_canopy,frozen_ground,  & !in  MB
                 ISNOW  ,CANLIQ ,CANICE ,TV     ,SNOWH  ,SNEQV  , & !inout
                 SNICE  ,SNLIQ  ,STC    ,ZSNSO  ,SH2O   ,SMC    , & !inout
                 SICE   ,ZWT    ,WA     ,WT     ,DZSNSO ,WSLAKE , & !inout
                 SMCWTD ,DEEPRECH,RECH                          , & !inout
! before waterbalance check add irrigation water to precipitation
IF((CROPLU .EQV. .TRUE.) .AND. (IRRFRA .GE. parameters%IRR_FRAC))THEN
   PRCP = PRCP + ((IRSIRATE+IRMIRATE+IRFIRATE)*1000./DT)  ! irrigation
   FSH  = FSH - FIRR                                      ! (W/m2)
END IF

! water and energy balance check

     CALL ERROR (parameters,SWDOWN ,FSA    ,FSR    ,FIRA   ,FSH    ,FCEV   , & !in
                 FGEV   ,FCTR   ,SSOIL  ,BEG_WB ,CANLIQ ,CANICE , & !in
                 SNEQV  ,WA     ,SMC    ,DZSNSO ,PRCP   ,ECAN   , & !in
                 ETRAN  ,EDIR   ,RUNSRF ,RUNSUB ,DT     ,NSOIL  , & !in
                 NSNOW  ,IST    ,ERRWAT ,ILOC   , JLOC  ,FVEG   , &
                 SAV    ,SAG    ,FSRV   ,FSRG   ,ZWT    ,PAH    , &
                 PAHV   ,PAHG   ,PAHB   ,FIRR)   !in ( Except ERRWAT, which is out )

! urban - jref
    QFX = ETRAN + ECAN + EDIR
    IF ( parameters%urban_flag ) THEN
       QSFC = QFX/(RHOAIR*CH) + QAIR
       Q2B = QSFC
    END IF

    IF(SNOWH <= 1.E-6 .OR. SNEQV <= 1.E-3) THEN
     SNOWH = 0.0
     SNEQV = 0.0
    END IF

    IF(SWDOWN.NE.0.) THEN
      ALBEDO = FSR / SWDOWN
    ELSE
      ALBEDO = -999.9
    END IF
    

  END SUBROUTINE NOAHMP_SFLX

!== begin atm ======================================================================================

  SUBROUTINE ATM (parameters,SFCPRS  ,SFCTMP   ,Q2      ,                             &
                  PRCPCONV,PRCPNONC ,PRCPSHCV,PRCPSNOW,PRCPGRPL,PRCPHAIL , &
                  SOLDN   ,COSZ     ,THAIR   ,QAIR    ,                    & 
                  EAIR    ,RHOAIR   ,QPRECC  ,QPRECL  ,SOLAD   , SOLAI   , &
		  SWDOWN  ,BDFALL   ,RAIN    ,SNOW    ,FP      , FPICE   ,PRCP )     
! --------------------------------------------------------------------------------------------------
! re-process atmospheric forcing
! ----------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs

  type (noahmp_parameters), intent(in) :: parameters
  REAL                          , INTENT(IN)  :: SFCPRS !pressure (pa)
  REAL                          , INTENT(IN)  :: SFCTMP !surface air temperature [k]
  REAL                          , INTENT(IN)  :: Q2     !mixing ratio (kg/kg)
  REAL                          , INTENT(IN)  :: PRCPCONV ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPNONC ! non-convective precipitation entering [mm/s] ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPSHCV ! shallow convective precip entering  [mm/s]   ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPSNOW ! snow entering land model [mm/s]              ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPGRPL ! graupel entering land model [mm/s]           ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPHAIL ! hail entering land model [mm/s]              ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: SOLDN  !downward shortwave radiation (w/m2)
  REAL                          , INTENT(IN)  :: COSZ   !cosine solar zenith angle [0-1]

! outputs

  REAL                          , INTENT(OUT) :: THAIR  !potential temperature (k)
  REAL                          , INTENT(OUT) :: QAIR   !specific humidity (kg/kg) (q2/(1+q2))
  REAL                          , INTENT(OUT) :: EAIR   !vapor pressure air (pa)
  REAL                          , INTENT(OUT) :: RHOAIR !density air (kg/m3)
  REAL                          , INTENT(OUT) :: QPRECC !convective precipitation (mm/s)
  REAL                          , INTENT(OUT) :: QPRECL !large-scale precipitation (mm/s)
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAD  !incoming direct solar radiation (w/m2)
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAI  !incoming diffuse solar radiation (w/m2)
  REAL                          , INTENT(OUT) :: SWDOWN !downward solar filtered by sun angle [w/m2]
  REAL                          , INTENT(OUT) :: BDFALL  !!bulk density of snowfall (kg/m3) AJN
  REAL                          , INTENT(OUT) :: RAIN    !rainfall (mm/s) AJN
  REAL                          , INTENT(OUT) :: SNOW    !liquid equivalent snowfall (mm/s) AJN
  REAL                          , INTENT(OUT) :: FP      !fraction of area receiving precipitation  AJN
  REAL                          , INTENT(OUT) :: FPICE   !fraction of ice                AJN
  REAL                          , INTENT(OUT) :: PRCP    !total precipitation [mm/s]     ! MB/AN : v3.7

!locals

  REAL                                        :: PAIR   !atm bottom level pressure (pa)
  REAL                                        :: PRCP_FROZEN   !total frozen precipitation [mm/s] ! MB/AN : v3.7
  REAL, PARAMETER                             :: RHO_GRPL = 500.0  ! graupel bulk density [kg/m3] ! MB/AN : v3.7
  REAL, PARAMETER                             :: RHO_HAIL = 917.0  ! hail bulk density [kg/m3]    ! MB/AN : v3.7
! wet-bulb scheme (Wang et al., 2019 GRL), C.He, 12/18/2020
     IF(OPT_SNF == 5) THEN
       TDC   = MIN( 50., MAX(-50.,(SFCTMP-TFRZ)) ) !Kelvin to degree Celsius with limit -50 to +50
       IF (SFCTMP > TFRZ) THEN
          LATHEA = HVAP
       ELSE
          LATHEA = HSUB
       END IF
       GAMMA_b = CPAIR*SFCPRS/(0.622*LATHEA)
       TWET    = TDC - 5.  ! first guess wetbulb temperature
       DO ITER = 1, NITER
          ESATAIR = 610.8 * EXP((17.27*TWET)/(237.3+TWET))
          TWET    = TWET - (ESATAIR-EAIR)/ GAMMA_b  ! Wang et al., 2019 GRL Eq.2
       END DO
       FPICE = 1.0/(1.0+6.99E-5*exp(2.0*(TWET+3.97))) ! Wang et al., 2019 GRL Eq. 1
     ENDIF

     RAIN   = PRCP * (1.-FPICE)
     SNOW   = PRCP * FPICE


  END SUBROUTINE ATM

!== begin phenology ================================================================================

  SUBROUTINE PHENOLOGY (parameters,VEGTYP ,croptype, SNOWH  , TV     , LAT   , YEARLEN , JULIAN , & !in
                        LAI    , SAI    , TROOT  , ELAI    , ESAI   , IGS, PGS)

! --------------------------------------------------------------------------------------------------
! vegetation phenology considering vegeation canopy being buries by snow and evolution in time
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER                , INTENT(IN   ) :: VEGTYP !vegetation type 
  INTEGER                , INTENT(IN   ) :: CROPTYPE !vegetation type 
  REAL                   , INTENT(IN   ) :: SNOWH  !snow height [m]
  REAL                   , INTENT(IN   ) :: TV     !vegetation temperature (k)
  REAL                   , INTENT(IN   ) :: LAT    !latitude (radians)
  INTEGER                , INTENT(IN   ) :: YEARLEN!Number of days in the particular year
  REAL                   , INTENT(IN   ) :: JULIAN !Julian day of year (fractional) ( 0 <= JULIAN < YEARLEN )
  real                   , INTENT(IN   ) :: TROOT  !root-zone averaged temperature (k)
  REAL                   , INTENT(INOUT) :: LAI    !LAI, unadjusted for burying by snow
  REAL                   , INTENT(INOUT) :: SAI    !SAI, unadjusted for burying by snow

! outputs
  REAL                   , INTENT(OUT  ) :: ELAI   !leaf area index, after burying by snow
  REAL                   , INTENT(OUT  ) :: ESAI   !stem area index, after burying by snow
  REAL                   , INTENT(OUT  ) :: IGS    !growing season index (0=off, 1=on)
  INTEGER                , INTENT(IN   ) :: PGS    !plant growing stage

! locals

  REAL                                   :: DB     !thickness of canopy buried by snow (m)
  REAL                                   :: FB     !fraction of canopy buried by snow
  REAL                                   :: SNOWHC !critical snow depth at which short vege
                                                   !is fully covered by snow

  INTEGER                                :: K       !index
  INTEGER                                :: IT1,IT2 !interpolation months
  REAL                                   :: DAY     !current day of year ( 0 <= DAY < YEARLEN )
  REAL                                   :: WT1,WT2 !interpolation weights
  REAL                                   :: T       !current month (1.00, ..., 12.00)
! --------------------------------------------------------------------------------------------------

IF (CROPTYPE == 0) THEN

  IF ( DVEG == 1 .or. DVEG == 3 .or. DVEG == 4 ) THEN

     IF (LAT >= 0.) THEN
        ! Northern Hemisphere
        DAY = JULIAN
     ELSE
        ! Southern Hemisphere.  DAY is shifted by 1/2 year.
        DAY = MOD ( JULIAN + ( 0.5 * YEARLEN ) , REAL(YEARLEN) )
     ENDIF

     T = 12. * DAY / REAL(YEARLEN)
     IT1 = T + 0.5
     IT2 = IT1 + 1
     WT1 = (IT1+0.5) - T
     WT2 = 1.-WT1
     IF (IT1 .LT.  1) IT1 = 12
     IF (IT2 .GT. 12) IT2 = 1

     LAI = WT1*parameters%LAIM(IT1) + WT2*parameters%LAIM(IT2)
     SAI = WT1*parameters%SAIM(IT1) + WT2*parameters%SAIM(IT2)
  ENDIF

  IF(DVEG == 7 .or. DVEG == 8 .or. DVEG == 9) THEN
    SAI = MAX(0.05,0.1 * LAI)  ! when reading LAI, set SAI to 10% LAI, but not below 0.05 MB: v3.8
    IF (LAI < 0.05) SAI = 0.0  ! if LAI below minimum, make sure SAI = 0
  ENDIF

  IF (SAI < 0.05) SAI = 0.0                    ! MB: SAI CHECK, change to 0.05 v3.6
  IF (LAI < 0.05 .OR. SAI == 0.0) LAI = 0.0  ! MB: LAI CHECK

  IF ( ( VEGTYP == parameters%iswater ) .OR. ( VEGTYP == parameters%ISBARREN ) .OR. &
       ( VEGTYP == parameters%ISICE   ) .or. ( parameters%urban_flag ) ) THEN
     LAI  = 0.
     SAI  = 0.
  ENDIF

ENDIF   ! CROPTYPE == 0

!buried by snow

     DB = MIN( MAX(SNOWH - parameters%HVB,0.), parameters%HVT-parameters%HVB )
     FB = DB / MAX(1.E-06,parameters%HVT-parameters%HVB)

     IF(parameters%HVT> 0. .AND. parameters%HVT <= 1.0) THEN          !MB: change to 1.0 and 0.2 to reflect
       SNOWHC = parameters%HVT*EXP(-SNOWH/0.2)             !      changes to HVT in MPTABLE
       FB     = MIN(SNOWH,SNOWHC)/SNOWHC
     ENDIF

     ELAI =  LAI*(1.-FB)
     ESAI =  SAI*(1.-FB)
     IF (ESAI < 0.05 .and. CROPTYPE == 0) ESAI = 0.0                   ! MB: ESAI CHECK, change to 0.05 v3.6
     IF ((ELAI < 0.05 .OR. ESAI == 0.0) .and. CROPTYPE == 0) ELAI = 0.0  ! MB: LAI CHECK

! set growing season flag

     IF ((TV .GT. parameters%TMIN .and. CROPTYPE == 0).or.(PGS > 2 .and. PGS < 7 .and. CROPTYPE > 0)) THEN
         IGS = 1.
     ELSE
         IGS = 0.
     ENDIF

  END SUBROUTINE PHENOLOGY

!== begin precip_heat ==============================================================================

  SUBROUTINE PRECIP_HEAT (parameters,ILOC   ,JLOC   ,VEGTYP ,DT     ,UU     ,VV     , & !in
                          ELAI   ,ESAI   ,FVEG   ,IST    ,                 & !in
                          BDFALL ,RAIN   ,SNOW   ,FP     ,                 & !in
                          CANLIQ ,CANICE ,TV     ,SFCTMP ,TG     ,         & !in
                          QINTR  ,QDRIPR ,QTHROR ,QINTS  ,QDRIPS ,QTHROS , & !out
			  PAHV   ,PAHG   ,PAHB   ,QRAIN  ,QSNOW  ,SNOWHIN, & !out
			  FWET   ,CMC                                    )   !out

! ------------------------ code history ------------------------------
! Michael Barlage: Oct 2013 - split CANWATER to calculate precip movement for 
!                             tracking of advected heat
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------ input/output variables --------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,INTENT(IN)  :: ILOC    !grid index
  INTEGER,INTENT(IN)  :: JLOC    !grid index
  INTEGER,INTENT(IN)  :: VEGTYP  !vegetation type
  INTEGER,INTENT(IN)  :: IST     !surface type 1-soil; 2-lake
  REAL,   INTENT(IN)  :: DT      !main time step (s)
  REAL,   INTENT(IN)  :: UU      !u-direction wind speed [m/s]
  REAL,   INTENT(IN)  :: VV      !v-direction wind speed [m/s]
  REAL,   INTENT(IN)  :: ELAI    !leaf area index, after burying by snow
  REAL,   INTENT(IN)  :: ESAI    !stem area index, after burying by snow
  REAL,   INTENT(IN)  :: FVEG    !greeness vegetation fraction (-)
  REAL,   INTENT(IN)  :: BDFALL  !bulk density of snowfall (kg/m3)
  REAL,   INTENT(IN)  :: RAIN    !rainfall (mm/s)
  REAL,   INTENT(IN)  :: SNOW    !snowfall (mm/s)
  REAL,   INTENT(IN)  :: FP      !fraction of the gridcell that receives precipitation
  REAL,   INTENT(IN)  :: TV      !vegetation temperature (k)
  REAL,   INTENT(IN)  :: SFCTMP  !model-level temperature (k)
  REAL,   INTENT(IN)  :: TG      !ground temperature (k)

! input & output
  REAL, INTENT(INOUT) :: CANLIQ  !intercepted liquid water (mm)
  REAL, INTENT(INOUT) :: CANICE  !intercepted ice mass (mm)

! output
  REAL, INTENT(OUT)   :: QINTR   !interception rate for rain (mm/s)
  REAL, INTENT(OUT)   :: QDRIPR  !drip rate for rain (mm/s)
  REAL, INTENT(OUT)   :: QTHROR  !throughfall for rain (mm/s)
  REAL, INTENT(OUT)   :: QINTS   !interception (loading) rate for snowfall (mm/s)
  REAL, INTENT(OUT)   :: QDRIPS  !drip (unloading) rate for intercepted snow (mm/s)
  REAL, INTENT(OUT)   :: QTHROS  !throughfall of snowfall (mm/s)
  REAL, INTENT(OUT)   :: PAHV    !precipitation advected heat - vegetation net (W/m2)
  REAL, INTENT(OUT)   :: PAHG    !precipitation advected heat - under canopy net (W/m2)
  REAL, INTENT(OUT)   :: PAHB    !precipitation advected heat - bare ground net (W/m2)
  REAL, INTENT(OUT)   :: QRAIN   !rain at ground srf (mm/s) [+]
  REAL, INTENT(OUT)   :: QSNOW   !snow at ground srf (mm/s) [+]
  REAL, INTENT(OUT)   :: SNOWHIN !snow depth increasing rate (m/s)
  REAL, INTENT(OUT)   :: FWET    !wetted or snowed fraction of the canopy (-)
  REAL, INTENT(OUT)   :: CMC     !intercepted water (mm)
! --------------------------------------------------------------------

! ------------------------ local variables ---------------------------
  REAL                :: MAXSNO  !canopy capacity for snow interception (mm)
  REAL                :: MAXLIQ  !canopy capacity for rain interception (mm)
  REAL                :: FT      !temperature factor for unloading rate
  REAL                :: FV      !wind factor for unloading rate
  REAL                :: PAH_AC  !precipitation advected heat - air to canopy (W/m2)
  REAL                :: PAH_CG  !precipitation advected heat - canopy to ground (W/m2)
  REAL                :: PAH_AG  !precipitation advected heat - air to ground (W/m2)
  REAL                :: ICEDRIP !canice unloading
! --------------------------------------------------------------------
! initialization

      QINTR   = 0.
      QDRIPR  = 0.
      QTHROR  = 0.
      QINTR   = 0.
      QINTS   = 0.
      QDRIPS  = 0.
      QTHROS  = 0.
      PAH_AC  = 0.
      PAH_CG  = 0.
      PAH_AG  = 0.
      PAHV    = 0.
      PAHG    = 0.
      PAHB    = 0.
      QRAIN   = 0.0
      QSNOW   = 0.0
      SNOWHIN = 0.0
      ICEDRIP = 0.0
!      print*, "precip_heat begin canopy balance:",canliq+canice+(rain+snow)*dt
!      print*,  "precip_heat snow*3600.0:",snow*3600.0
!      print*,  "precip_heat rain*3600.0:",rain*3600.0
!      print*,  "precip_heat canice:",canice
!      print*,  "precip_heat canliq:",canliq

! --------------------------- liquid water ------------------------------
! maximum canopy water

      MAXLIQ =  parameters%CH2OP * (ELAI+ ESAI)

! average interception and throughfall

      IF((ELAI+ ESAI).GT.0.) THEN
         QINTR  = FVEG * RAIN * FP  ! interception capability
         QINTR  = MIN(QINTR, (MAXLIQ - CANLIQ)/DT * (1.-EXP(-RAIN*DT/MAXLIQ)) )
         QINTR  = MAX(QINTR, 0.)
         QDRIPR = FVEG * RAIN - QINTR
         QTHROR = (1.-FVEG) * RAIN
         CANLIQ=MAX(0.,CANLIQ+QINTR*DT)
      ELSE
         QINTR  = 0.
         QDRIPR = 0.
         QTHROR = RAIN
	 IF(CANLIQ > 0.) THEN             ! FOR CASE OF CANOPY GETTING BURIED
	   QDRIPR = QDRIPR + CANLIQ/DT
	   CANLIQ = 0.0
	 END IF
      END IF
      
! heat transported by liquid water

      PAH_AC = FVEG * RAIN * (CWAT/1000.0) * (SFCTMP - TV)
      PAH_CG = QDRIPR * (CWAT/1000.0) * (TV - TG)
      PAH_AG = QTHROR * (CWAT/1000.0) * (SFCTMP - TG)
!      print*, "precip_heat PAH_AC:",PAH_AC
!      print*, "precip_heat PAH_CG:",PAH_CG
!      print*, "precip_heat PAH_AG:",PAH_AG

! --------------------------- canopy ice ------------------------------
! for canopy ice

      MAXSNO = 6.6*(0.27+46./BDFALL) * (ELAI+ ESAI)

      IF((ELAI+ ESAI).GT.0.) THEN
         QINTS = FVEG * SNOW * FP
         QINTS = MIN(QINTS, (MAXSNO - CANICE)/DT * (1.-EXP(-SNOW*DT/MAXSNO)) )
         QINTS = MAX(QINTS, 0.)
         FT = MAX(0.0,(TV - 270.15) / 1.87E5)
         FV = SQRT(UU*UU + VV*VV) / 1.56E5
	 ! MB: changed below to reflect the rain assumption that all precip gets intercepted 
	 ICEDRIP = MAX(0.,CANICE) * (FV+FT)    !MB: removed /DT
         QDRIPS = (FVEG * SNOW - QINTS) + ICEDRIP
         QTHROS = (1.0-FVEG) * SNOW
         CANICE= MAX(0.,CANICE + (QINTS - ICEDRIP)*DT)
      ELSE
         QINTS  = 0.
         QDRIPS = 0.
         QTHROS = SNOW
	 IF(CANICE > 0.) THEN             ! FOR CASE OF CANOPY GETTING BURIED
	   QDRIPS = QDRIPS + CANICE/DT
	   CANICE = 0.0
	 END IF
      ENDIF
!      print*, "precip_heat canopy through:",3600.0*(FVEG * SNOW - QINTS)
!      print*, "precip_heat canopy drip:",3600.0*MAX(0.,CANICE) * (FV+FT)

! wetted fraction of canopy

      IF(CANICE.GT.0.) THEN
           FWET = MAX(0.,CANICE) / MAX(MAXSNO,1.E-06)
      ELSE
           FWET = MAX(0.,CANLIQ) / MAX(MAXLIQ,1.E-06)
      ENDIF
      FWET = MIN(FWET, 1.) ** 0.667

! total canopy water

      CMC = CANLIQ + CANICE

! heat transported by snow/ice

      PAH_AC = PAH_AC +  FVEG * SNOW * (CICE/1000.0) * (SFCTMP - TV)
      PAH_CG = PAH_CG + QDRIPS * (CICE/1000.0) * (TV - TG)
      PAH_AG = PAH_AG + QTHROS * (CICE/1000.0) * (SFCTMP - TG)
      
      PAHV = PAH_AC - PAH_CG
      PAHG = PAH_CG
      PAHB = PAH_AG
      
      IF (FVEG > 0.0 .AND. FVEG < 1.0) THEN
        PAHG = PAHG / FVEG         ! these will be multiplied by fraction later
	PAHB = PAHB / (1.0-FVEG)
      ELSEIF (FVEG <= 0.0) THEN
        PAHB = PAHG + PAHB         ! for case of canopy getting buried
        PAHG = 0.0
	PAHV = 0.0
      ELSEIF (FVEG >= 1.0) THEN
	PAHB = 0.0
      END IF
      
      PAHV = MAX(PAHV,-20.0)       ! Put some artificial limits here for stability
      PAHV = MIN(PAHV,20.0)
      PAHG = MAX(PAHG,-20.0)
      PAHG = MIN(PAHG,20.0)
      PAHB = MAX(PAHB,-20.0)
      PAHB = MIN(PAHB,20.0)
      
!      print*, 'precip_heat sfctmp,tv,tg:',sfctmp,tv,tg
!      print*, 'precip_heat 3600.0*qints+qdrips+qthros:',3600.0*(qints+qdrips+qthros)
!      print*, "precip_heat maxsno:",maxsno
!      print*, "precip_heat PAH_AC:",PAH_AC
!      print*, "precip_heat PAH_CG:",PAH_CG
!      print*, "precip_heat PAH_AG:",PAH_AG
      
!      print*, "precip_heat PAHV:",PAHV
!      print*, "precip_heat PAHG:",PAHG
!      print*, "precip_heat PAHB:",PAHB
!      print*, "precip_heat fveg:",fveg
!      print*,  "precip_heat qints*3600.0:",qints*3600.0
!      print*,  "precip_heat qdrips*3600.0:",qdrips*3600.0
!      print*,  "precip_heat qthros*3600.0:",qthros*3600.0
      
! rain or snow on the ground

      QRAIN   = QDRIPR + QTHROR
      QSNOW   = QDRIPS + QTHROS
      SNOWHIN = QSNOW/BDFALL

      IF (IST == 2 .AND. TG > TFRZ) THEN
         QSNOW   = 0.
         SNOWHIN = 0.
      END IF
!      print*,  "precip_heat qsnow*3600.0:",qsnow*3600.0
!      print*,  "precip_heat qrain*3600.0:",qrain*3600.0
!      print*,  "precip_heat SNOWHIN:",SNOWHIN
!      print*,  "precip_heat canice:",canice
!      print*,  "precip_heat canliq:",canliq
!      print*, "precip_heat end canopy balance:",canliq+canice+(qrain+qsnow)*dt
      

  END SUBROUTINE PRECIP_HEAT

!== begin error ====================================================================================

  SUBROUTINE ERROR (parameters,SWDOWN ,FSA    ,FSR    ,FIRA   ,FSH    ,FCEV   , &
                    FGEV   ,FCTR   ,SSOIL  ,BEG_WB ,CANLIQ ,CANICE , &
                    SNEQV  ,WA     ,SMC    ,DZSNSO ,PRCP   ,ECAN   , &
                    ETRAN  ,EDIR   ,RUNSRF ,RUNSUB ,DT     ,NSOIL  , &
                    NSNOW  ,IST    ,ERRWAT, ILOC   ,JLOC   ,FVEG   , &
                    SAV    ,SAG    ,FSRV   ,FSRG   ,ZWT    ,PAH    , &
                    PAHV   ,PAHG   ,PAHB   ,FIRR)
! --------------------------------------------------------------------------------------------------
! check surface energy balance and water balance
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER                        , INTENT(IN) :: NSNOW  !maximum no. of snow layers        
  INTEGER                        , INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER                        , INTENT(IN) :: IST    !surface type 1->soil; 2->lake
  INTEGER                        , INTENT(IN) :: ILOC   !grid index
  INTEGER                        , INTENT(IN) :: JLOC   !grid index
  REAL                           , INTENT(IN) :: SWDOWN !downward solar filtered by sun angle [w/m2]
  REAL                           , INTENT(IN) :: FSA    !total absorbed solar radiation (w/m2)
  REAL                           , INTENT(IN) :: FSR    !total reflected solar radiation (w/m2)
  REAL                           , INTENT(IN) :: FIRA   !total net longwave rad (w/m2)  [+ to atm]
  REAL                           , INTENT(IN) :: FSH    !total sensible heat (w/m2)     [+ to atm]
  REAL                           , INTENT(IN) :: FCEV   !canopy evaporation heat (w/m2) [+ to atm]
  REAL                           , INTENT(IN) :: FGEV   !ground evaporation heat (w/m2) [+ to atm]
  REAL                           , INTENT(IN) :: FCTR   !transpiration heat flux (w/m2) [+ to atm]
  REAL                           , INTENT(IN) :: SSOIL  !ground heat flux (w/m2)        [+ to soil]
  REAL                           , INTENT(IN) :: FVEG
  REAL                           , INTENT(IN) :: SAV
  REAL                           , INTENT(IN) :: SAG
  REAL                           , INTENT(IN) :: FSRV
  REAL                           , INTENT(IN) :: FSRG
  REAL                           , INTENT(IN) :: ZWT

  REAL                           , INTENT(IN) :: PRCP   !precipitation rate (kg m-2 s-1)
  REAL                           , INTENT(IN) :: ECAN   !evaporation of intercepted water (mm/s)
  REAL                           , INTENT(IN) :: ETRAN  !transpiration rate (mm/s)
  REAL                           , INTENT(IN) :: EDIR   !soil surface evaporation rate[mm/s]
  REAL                           , INTENT(IN) :: RUNSRF !surface runoff [mm/s] 
  REAL                           , INTENT(IN) :: RUNSUB !baseflow (saturation excess) [mm/s]
  REAL                           , INTENT(IN) :: CANLIQ !intercepted liquid water (mm)
  REAL                           , INTENT(IN) :: CANICE !intercepted ice mass (mm)
  REAL                           , INTENT(IN) :: SNEQV  !snow water eqv. [mm]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer thickness [m]
  REAL                           , INTENT(IN) :: WA     !water storage in aquifer [mm]
  REAL                           , INTENT(IN) :: DT     !time step [sec]
  REAL                           , INTENT(IN) :: BEG_WB !water storage at begin of a timesetp [mm]
  REAL                           , INTENT(OUT) :: ERRWAT !error in water balance [mm/timestep]
  REAL, INTENT(IN)   :: PAH     !precipitation advected heat - total (W/m2)
  REAL, INTENT(IN)   :: PAHV    !precipitation advected heat - total (W/m2)
  REAL, INTENT(IN)   :: PAHG    !precipitation advected heat - total (W/m2)
  REAL, INTENT(IN)   :: PAHB    !precipitation advected heat - total (W/m2)
   ERRENG = SAV+SAG-(FIRA+FSH+FCEV+FGEV+FCTR+SSOIL+FIRR) +PAH
!   ERRENG = FVEG*SAV+SAG-(FIRA+FSH+FCEV+FGEV+FCTR+SSOIL)
!   WRITE(*,*) "ERRENG =",ERRENG
   IF(ABS(ERRENG) > 0.01) THEN
      write(message,*) 'ERRENG =',ERRENG,' at i,j: ',ILOC,JLOC
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Net solar:       ",FSA
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Net longwave:    ",FIRA
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Total sensible:  ",FSH
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Canopy evap:     ",FCEV
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Ground evap:     ",FGEV
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Transpiration:   ",FCTR
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Total ground:    ",SSOIL
      call wrf_message(trim(message))
         !FSNO     = TANH( SNOWH /(2.5* Z0 * FMELT))
         FSNO     = TANH( SNOWH /(parameters%SCFFAC * FMELT)) ! C.He: bring hard-coded 2.5*z0 to MPTABLE tunable parameter SCFFAC
     ENDIF

! ground roughness length

     IF(IST == 2) THEN
       IF(TG .LE. TFRZ) THEN
         Z0MG = 0.01 * (1.0-FSNO) + FSNO * parameters%Z0SNO
       ELSE
         Z0MG = 0.01  
       END IF
     ELSE
       Z0MG = Z0 * (1.0-FSNO) + FSNO * parameters%Z0SNO
     END IF

! roughness length and displacement height

     ZPDG  = SNOWH
     IF(VEG) THEN
        Z0M  = parameters%Z0MVT
        ZPD  = 0.65 * parameters%HVT
        IF(SNOWH.GT.ZPD) ZPD  = SNOWH
     ELSE
        Z0M  = Z0MG
        ZPD  = ZPDG
     END IF

! special case for urban

     IF (parameters%urban_flag) THEN
       Z0MG = parameters%Z0MVT
       ZPDG  = 0.65 * parameters%HVT
       Z0M  = Z0MG
       ZPD  = ZPDG
     END IF

     ZLVL = MAX(ZPD,parameters%HVT) + ZREF
     IF(ZPDG >= ZLVL) ZLVL = ZPDG + ZREF
!     UR   = UR*LOG(ZLVL/Z0M)/LOG(10./Z0M)       !input UR is at 10m

! canopy wind absorption coeffcient

     CWP = parameters%CWPVT

! Thermal properties of soil, snow, lake, and frozen soil

  CALL THERMOPROP (parameters,NSOIL   ,NSNOW   ,ISNOW   ,IST     ,DZSNSO  , & !in
                   DT      ,SNOWH   ,SNICE   ,SNLIQ   , & !in
                   SMC     ,SH2O    ,TG      ,STC     ,UR      , & !in
                   LAT     ,Z0M     ,ZLVL    ,VEGTYP  , & !in
                   DF      ,HCPCT   ,SNICEV  ,SNLIQV  ,EPORE   , & !out
                   FACT    )                              !out

! Solar radiation: absorbed & reflected by the ground and canopy

  CALL  RADIATION (parameters,VEGTYP  ,IST     ,ICE     ,NSOIL   , & !in 
                   SNEQVO  ,SNEQV   ,DT      ,COSZ    ,SNOWH   , & !in
                   TG      ,TV      ,FSNO    ,QSNOW   ,FWET    , & !in
                   ELAI    ,ESAI    ,SMC     ,SOLAD   ,SOLAI   , & !in
                   FVEG    ,ILOC    ,JLOC    ,                   & !in
                   ALBOLD  ,TAUSS   ,                            & !inout
                   FSUN    ,LAISUN  ,LAISHA  ,PARSUN  ,PARSHA  , & !out
                   SAV     ,SAG     ,FSR     ,FSA     ,FSRV    , & 
                   FSRG    ,ALBSND  ,ALBSNI  ,BGAP    ,WGAP     )  !out

! vegetation and ground emissivity

     EMV = 1. - EXP(-(ELAI+ESAI)/1.0)
     IF (ICE == 1) THEN
       EMG = 0.98*(1.-FSNO) + parameters%SNOW_EMIS*FSNO  ! move hard-coded snow emissivity as a global parameter to MPTABLE
     ELSE
       EMG = parameters%EG(IST)*(1.-FSNO) + parameters%SNOW_EMIS*FSNO
     END IF

! soil moisture factor controlling stomatal resistance
   
     BTRAN = 0.

     IF(IST ==1 ) THEN
       DO IZ = 1, parameters%NROOT
          IF(OPT_BTR == 1) then                  ! Noah
            GX    = (SH2O(IZ)-parameters%SMCWLT(IZ)) / (parameters%SMCREF(IZ)-parameters%SMCWLT(IZ))
          END IF
          IF(OPT_BTR == 2) then                  ! CLM
            PSI   = MAX(PSIWLT,-parameters%PSISAT(IZ)*(MAX(0.01,SH2O(IZ))/parameters%SMCMAX(IZ))**(-parameters%BEXP(IZ)) )
            GX    = (1.-PSI/PSIWLT)/(1.+parameters%PSISAT(IZ)/PSIWLT)
          END IF
          IF(OPT_BTR == 3) then                  ! SSiB
            PSI   = MAX(PSIWLT,-parameters%PSISAT(IZ)*(MAX(0.01,SH2O(IZ))/parameters%SMCMAX(IZ))**(-parameters%BEXP(IZ)) )
            GX    = 1.-EXP(-5.8*(LOG(PSIWLT/PSI))) 
          END IF
       
          GX = MIN(1.,MAX(0.,GX))
          BTRANI(IZ) = MAX(MPE,DZSNSO(IZ) / (-ZSOIL(parameters%NROOT)) * GX)
          BTRAN      = BTRAN + BTRANI(IZ)
       END DO
       BTRAN = MAX(MPE,BTRAN)

       BTRANI(1:parameters%NROOT) = BTRANI(1:parameters%NROOT)/BTRAN
     END IF

! soil surface resistance for ground evap.

     BEVAP = MAX(0.0,SH2O(1)/parameters%SMCMAX(1))
     IF(IST == 2) THEN
       RSURF = 1.          ! avoid being divided by 0
       RHSUR = 1.0
     ELSE

       IF(OPT_RSF == 1 .OR. OPT_RSF == 4) THEN
         ! RSURF based on Sakaguchi and Zeng, 2009
         ! taking the "residual water content" to be the wilting point, 
         ! and correcting the exponent on the D term (typo in SZ09 ?)
         L_RSURF = (-ZSOIL(1)) * ( exp ( (1.0 - MIN(1.0,SH2O(1)/parameters%SMCMAX(1))) ** parameters%RSURF_EXP ) - 1.0 ) / ( 2.71828 - 1.0 ) 
         D_RSURF = 2.2E-5 * parameters%SMCMAX(1) * parameters%SMCMAX(1) * ( 1.0 - parameters%SMCWLT(1) / parameters%SMCMAX(1) ) ** (2.0+3.0/parameters%BEXP(1))
         RSURF = L_RSURF / D_RSURF
       ELSEIF(OPT_RSF == 2) THEN
         RSURF = FSNO * 1. + (1.-FSNO)* EXP(8.25-4.225*BEVAP) !Sellers (1992) ! Older RSURF computations
       ELSEIF(OPT_RSF == 3) THEN
         RSURF = FSNO * 1. + (1.-FSNO)* EXP(8.25-6.0  *BEVAP) !adjusted to decrease RSURF for wet soil
       ENDIF

       IF(OPT_RSF == 4) THEN  ! AD: FSNO weighted; snow RSURF set in MPTABLE v3.8
         RSURF = 1. / (FSNO * (1./parameters%RSURF_SNOW) + (1.-FSNO) * (1./max(RSURF, 0.001)))
       ENDIF

       IF(SH2O(1) < 0.01 .and. SNOWH == 0.) RSURF = 1.E6
       PSI   = -parameters%PSISAT(1)*(MAX(0.01,SH2O(1))/parameters%SMCMAX(1))**(-parameters%BEXP(1))   
       RHSUR = FSNO + (1.-FSNO) * EXP(PSI*GRAV/(RW*TG)) 
     END IF

! urban - jref 
     IF (parameters%urban_flag .and. SNOWH == 0. ) THEN
        RSURF = 1.E6
     ENDIF

! set psychrometric constant

     IF (TV .GT. TFRZ) THEN           ! Barlage: add distinction between ground and 
        LATHEAV = HVAP                ! vegetation in v3.6
	frozen_canopy = .false.
     ELSE
        LATHEAV = HSUB
	frozen_canopy = .true.
     END IF
     GAMMAV = CPAIR*SFCPRS/(0.622*LATHEAV)

     IF (TG .GT. TFRZ) THEN
        LATHEAG = HVAP
	frozen_ground = .false.
     ELSE
        LATHEAG = HSUB
	frozen_ground = .true.
     END IF
     GAMMAG = CPAIR*SFCPRS/(0.622*LATHEAG)

!     IF (SFCTMP .GT. TFRZ) THEN
!        LATHEA = HVAP
!     ELSE
!        LATHEA = HSUB
!     END IF
!     GAMMA = CPAIR*SFCPRS/(0.622*LATHEA)

! Surface temperatures of the ground and canopy and energy fluxes

    IF (VEG .AND. FVEG > 0) THEN 
    TGV = TG
    CMV = CM
    CHV = CH
    CALL VEGE_FLUX (parameters,NSNOW   ,NSOIL   ,ISNOW   ,VEGTYP  ,VEG     , & !in
                    DT      ,SAV     ,SAG     ,LWDN    ,UR      , & !in
                    UU      ,VV      ,SFCTMP  ,THAIR   ,QAIR    , & !in
                    EAIR    ,RHOAIR  ,SNOWH   ,VAI     ,GAMMAV   ,GAMMAG   , & !in
                    FWET    ,LAISUN  ,LAISHA  ,CWP     ,DZSNSO  , & !in
                    ZLVL    ,ZPD     ,Z0M     ,FVEG    , & !in
                    Z0MG    ,EMV     ,EMG     ,CANLIQ  ,FSNO, & !in
                    CANICE  ,STC     ,DF      ,RSSUN   ,RSSHA   , & !in
                    RSURF   ,LATHEAV ,LATHEAG ,PARSUN  ,PARSHA  ,IGS     , & !in
                    FOLN    ,CO2AIR  ,O2AIR   ,BTRAN   ,SFCPRS  , & !in
                    RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHV  ,PAHG  , & !in
                    EAH     ,TAH     ,TV      ,TGV     ,CMV     , & !inout
                    CHV     ,DX      ,DZ8W    ,                   & !inout
                    TAUXV   ,TAUYV   ,IRG     ,IRC     ,SHG     , & !out
                    SHC     ,EVG     ,EVC     ,TR      ,GHV     , & !out
                    T2MV    ,PSNSUN  ,PSNSHA  ,                   & !out
!jref:start
                    QC      ,QSFC    ,PSFC    , & !in
                    Q2V     ,CHV2, CHLEAF, CHUC, &
                    SH2O,JULIAN, SWDOWN, PRCP, FB, FSR, GECROS1D)      ! Gecros 
!jref:end                            
    END IF

    TGB = TG
    CMB = CM
    CHB = CH
    CALL BARE_FLUX (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,SAG     , & !in
                    LWDN    ,UR      ,UU      ,VV      ,SFCTMP  , & !in
                    THAIR   ,QAIR    ,EAIR    ,RHOAIR  ,SNOWH   , & !in
                    DZSNSO  ,ZLVL    ,ZPDG    ,Z0MG    ,FSNO,          & !in
                    EMG     ,STC     ,DF      ,RSURF   ,LATHEAG  , & !in
                    GAMMAG   ,RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHB  , & !in
                    TGB     ,CMB     ,CHB     ,                   & !inout
                    TAUXB   ,TAUYB   ,IRB     ,SHB     ,EVB     , & !out
                    GHB     ,T2MB    ,DX      ,DZ8W    ,VEGTYP  , & !out
!jref:start
                    QC      ,QSFC    ,PSFC    , & !in
                    SFCPRS  ,Q2B,   CHB2)                          !in 
!jref:end                            

!energy balance at vege canopy: SAV          =(IRC+SHC+EVC+TR)     *FVEG  at   FVEG 
!energy balance at vege ground: SAG*    FVEG =(IRG+SHG+EVG+GHV)    *FVEG  at   FVEG
!energy balance at bare ground: SAG*(1.-FVEG)=(IRB+SHB+EVB+GHB)*(1.-FVEG) at 1-FVEG

    IF (VEG .AND. FVEG > 0) THEN 
        TAUX  = FVEG * TAUXV     + (1.0 - FVEG) * TAUXB
        TAUY  = FVEG * TAUYV     + (1.0 - FVEG) * TAUYB
        FIRA  = FVEG * IRG       + (1.0 - FVEG) * IRB       + IRC
        FSH   = FVEG * SHG       + (1.0 - FVEG) * SHB       + SHC
        FGEV  = FVEG * EVG       + (1.0 - FVEG) * EVB
        SSOIL = FVEG * GHV       + (1.0 - FVEG) * GHB
        FCEV  = EVC
        FCTR  = TR
	PAH   = FVEG * PAHG      + (1.0 - FVEG) * PAHB   + PAHV
        TG    = FVEG * TGV       + (1.0 - FVEG) * TGB
        T2M   = FVEG * T2MV      + (1.0 - FVEG) * T2MB
        TS    = FVEG * TV        + (1.0 - FVEG) * TGB
        CM    = FVEG * CMV       + (1.0 - FVEG) * CMB      ! better way to average?
        CH    = FVEG * CHV       + (1.0 - FVEG) * CHB
        Q1    = FVEG * (EAH*0.622/(SFCPRS - 0.378*EAH)) + (1.0 - FVEG)*QSFC
        Q2E   = FVEG * Q2V       + (1.0 - FVEG) * Q2B
	Z0WRF = Z0M
    ELSE
        TAUX  = TAUXB
        TAUY  = TAUYB
        FIRA  = IRB
        FSH   = SHB
        FGEV  = EVB
        SSOIL = GHB
        TG    = TGB
        T2M   = T2MB
        FCEV  = 0.
        FCTR  = 0.
	PAH   = PAHB
        TS    = TG
        CM    = CMB
        CH    = CHB
        Q1    = QSFC
        Q2E   = Q2B
        RSSUN = 0.0
        RSSHA = 0.0
        TGV   = TGB
        CHV   = CHB
	Z0WRF = Z0MG
    END IF

    FIRE = LWDN + FIRA

    IF(FIRE <=0.) THEN
       WRITE(6,*) 'emitted longwave <0; skin T may be wrong due to inconsistent'
       WRITE(6,*) 'input of SHDFAC with LAI'
       WRITE(6,*) ILOC, JLOC, 'SHDFAC=',FVEG,'VAI=',VAI,'TV=',TV,'TG=',TG
       WRITE(6,*) 'LWDN=',LWDN,'FIRA=',FIRA,'SNOWH=',SNOWH
       call wrf_error_fatal("STOP in Noah-MP")
    END IF

    ! Compute a net emissivity
    EMISSI = FVEG * ( EMG*(1-EMV) + EMV + EMV*(1-EMV)*(1-EMG) ) + &
         (1-FVEG) * EMG

    ! When we're computing a TRAD, subtract from the emitted IR the
    ! reflected portion of the incoming LWDN, so we're just
    ! considering the IR originating in the canopy/ground system.
    
    TRAD = ( ( FIRE - (1-EMISSI)*LWDN ) / (EMISSI*SB) ) ** 0.25

    ! Old TRAD calculation not taking into account Emissivity:
    ! TRAD = (FIRE/SB)**0.25

    APAR = PARSUN*LAISUN + PARSHA*LAISHA
    PSN  = PSNSUN*LAISUN + PSNSHA*LAISHA

! 3L snow & 4L soil temperatures

    CALL TSNOSOI (parameters,ICE     ,NSOIL   ,NSNOW   ,ISNOW   ,IST     , & !in
                  TBOT    ,ZSNSO   ,SSOIL   ,DF      ,HCPCT   , & !in
                  SAG     ,DT      ,SNOWH   ,DZSNSO  , & !in
                  TG      ,ILOC    ,JLOC    ,                   & !in
                  STC     )                                       !inout

! adjusting snow surface temperature
     IF(OPT_STC == 2) THEN
      IF (SNOWH > 0.05 .AND. TG > TFRZ) THEN
        TGV = TFRZ
        TGB = TFRZ
          IF (VEG .AND. FVEG > 0) THEN
             TG    = FVEG * TGV       + (1.0 - FVEG) * TGB
             TS    = FVEG * TV        + (1.0 - FVEG) * TGB
          ELSE
             TG    = TGB
             TS    = TGB
          END IF
      END IF
     END IF

! Energy released or consumed by snow & frozen soil

 CALL PHASECHANGE (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & !in
                   DZSNSO  ,HCPCT   ,IST     ,ILOC    ,JLOC    , & !in
                   STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & !inout
                   SMC     ,SH2O    ,                            & !inout
                   QMELT   ,IMELT   ,PONDING )                     !out


  END SUBROUTINE ENERGY

!== begin thermoprop ===============================================================================

  SUBROUTINE THERMOPROP (parameters,NSOIL   ,NSNOW   ,ISNOW   ,IST     ,DZSNSO  , & !in
                         DT      ,SNOWH   ,SNICE   ,SNLIQ   , & !in
                         SMC     ,SH2O    ,TG      ,STC     ,UR      , & !in
                         LAT     ,Z0M     ,ZLVL    ,VEGTYP  , & !in
                         DF      ,HCPCT   ,SNICEV  ,SNLIQV  ,EPORE   , & !out
                         FACT    )                                       !out
! ------------------------------------------------------------------------------------------------- 
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER                        , INTENT(IN)  :: NSOIL   !number of soil layers
  INTEGER                        , INTENT(IN)  :: NSNOW   !maximum no. of snow layers        
  INTEGER                        , INTENT(IN)  :: ISNOW   !actual no. of snow layers
  INTEGER                        , INTENT(IN)  :: IST     !surface type
  REAL                           , INTENT(IN)  :: DT      !time step [s]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)  :: SNICE   !snow ice mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)  :: SNLIQ   !snow liq mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DZSNSO  !thickness of snow/soil layers [m]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)  :: SMC     !soil moisture (ice + liq.) [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)  :: SH2O    !liquid soil moisture [m3/m3]
  REAL                           , INTENT(IN)  :: SNOWH   !snow height [m]
  REAL,                            INTENT(IN)  :: TG      !surface temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: STC     !snow/soil/lake temp. (k)
  REAL,                            INTENT(IN)  :: UR      !wind speed at ZLVL (m/s)
  REAL,                            INTENT(IN)  :: LAT     !latitude (radians)
  REAL,                            INTENT(IN)  :: Z0M     !roughness length (m)
  REAL,                            INTENT(IN)  :: ZLVL    !reference height (m)
  INTEGER                        , INTENT(IN)  :: VEGTYP  !vegtyp type

! outputs
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: DF      !thermal conductivity [w/m/k]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: HCPCT   !heat capacity [j/m3/k]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNICEV  !partial volume of ice [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNLIQV  !partial volume of liquid water [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: EPORE   !effective porosity [m3/m3]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: FACT    !computing energy for phase change
! --------------------------------------------------------------------------------------------------
! locals

  INTEGER :: IZ
  REAL, DIMENSION(-NSNOW+1:    0)              :: CVSNO   !volumetric specific heat (j/m3/k)
  REAL, DIMENSION(-NSNOW+1:    0)              :: TKSNO   !snow thermal conductivity (j/m3/k)
  REAL, DIMENSION(       1:NSOIL)              :: SICE    !soil ice content
! --------------------------------------------------------------------------------------------------

! compute snow thermal conductivity and heat capacity

    CALL CSNOW (parameters,ISNOW   ,NSNOW   ,NSOIL   ,SNICE   ,SNLIQ   ,DZSNSO  , & !in
                TKSNO   ,CVSNO   ,SNICEV  ,SNLIQV  ,EPORE   )   !out

    DO IZ = ISNOW+1, 0
      DF   (IZ) = TKSNO(IZ)
      HCPCT(IZ) = CVSNO(IZ)
    END DO

! compute soil thermal properties

    DO  IZ = 1, NSOIL
       SICE(IZ)  = SMC(IZ) - SH2O(IZ)
       HCPCT(IZ) = SH2O(IZ)*CWAT + (1.0-parameters%SMCMAX(IZ))*parameters%CSOIL &
                + (parameters%SMCMAX(IZ)-SMC(IZ))*CPAIR + SICE(IZ)*CICE
       CALL TDFCND (parameters,IZ,DF(IZ), SMC(IZ), SH2O(IZ))
    END DO
       
    IF ( parameters%urban_flag ) THEN
       DO IZ = 1,NSOIL
         DF(IZ) = 3.24
       END DO
    ENDIF

! heat flux reduction effect from the overlying green canopy, adapted from 
! section 2.1.2 of Peters-Lidard et al. (1997, JGR, VOL 102(D4)).
! not in use because of the separation of the canopy layer from the ground.
! but this may represent the effects of leaf litter (Niu comments)
!       DF1 = DF1 * EXP (SBETA * SHDFAC)

! compute lake thermal properties 
! (no consideration of turbulent mixing for this version)

    IF(IST == 2) THEN
       DO IZ = 1, NSOIL 
         IF(STC(IZ) > TFRZ) THEN
            HCPCT(IZ) = CWAT
            DF(IZ)    = TKWAT  !+ KEDDY * CWAT 
         ELSE
            HCPCT(IZ) = CICE
            DF(IZ)    = TKICE 
         END IF
       END DO
    END IF

! combine a temporary variable used for melting/freezing of snow and frozen soil

    DO IZ = ISNOW+1,NSOIL
     FACT(IZ) = DT/(HCPCT(IZ)*DZSNSO(IZ))
    END DO

! snow/soil interface

    IF(ISNOW == 0) THEN
       DF(1) = (DF(1)*DZSNSO(1)+0.35*SNOWH)      / (SNOWH    +DZSNSO(1)) 
    ELSE
       DF(1) = (DF(1)*DZSNSO(1)+DF(0)*DZSNSO(0)) / (DZSNSO(0)+DZSNSO(1))
    END IF


  END SUBROUTINE THERMOPROP

!== begin csnow ====================================================================================

  SUBROUTINE CSNOW (parameters,ISNOW   ,NSNOW   ,NSOIL   ,SNICE   ,SNLIQ   ,DZSNSO  , & !in
                    TKSNO   ,CVSNO   ,SNICEV  ,SNLIQV  ,EPORE   )   !out
! --------------------------------------------------------------------------------------------------
! Snow bulk density,volumetric capacity, and thermal conductivity
!---------------------------------------------------------------------------------------------------
  IMPLICIT NONE
!---------------------------------------------------------------------------------------------------
! inputs

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                          INTENT(IN) :: ISNOW  !number of snow layers (-)            
  INTEGER                        ,  INTENT(IN) :: NSNOW  !maximum no. of snow layers        
  INTEGER                        ,  INTENT(IN) :: NSOIL  !number of soil layers
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN) :: SNICE  !snow ice mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN) :: SNLIQ  !snow liq mass (kg/m2) 
  REAL, DIMENSION(-NSNOW+1:NSOIL),  INTENT(IN) :: DZSNSO !snow/soil layer thickness [m]

! outputs

  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: CVSNO  !volumetric specific heat (j/m3/k)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: TKSNO  !thermal conductivity (w/m/k)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNICEV !partial volume of ice [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNLIQV !partial volume of liquid water [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: EPORE  !effective porosity [m3/m3]

! locals

  INTEGER :: IZ
  REAL, DIMENSION(-NSNOW+1:    0) :: BDSNOI  !bulk density of snow(kg/m3)

!---------------------------------------------------------------------------------------------------
! thermal capacity of snow

  DO IZ = ISNOW+1, 0
      SNICEV(IZ)   = MIN(1., SNICE(IZ)/(DZSNSO(IZ)*DENICE) )
      EPORE(IZ)    = 1. - SNICEV(IZ)
      SNLIQV(IZ)   = MIN(EPORE(IZ),SNLIQ(IZ)/(DZSNSO(IZ)*DENH2O))
  ENDDO

  DO IZ = ISNOW+1, 0
      BDSNOI(IZ) = (SNICE(IZ)+SNLIQ(IZ))/DZSNSO(IZ)
      CVSNO(IZ) = CICE*SNICEV(IZ)+CWAT*SNLIQV(IZ)
!      CVSNO(IZ) = 0.525E06                          ! constant
  enddo

! thermal conductivity of snow

  DO IZ = ISNOW+1, 0
     TKSNO(IZ) = 3.2217E-6*BDSNOI(IZ)**2.           ! Stieglitz(yen,1965)
!    TKSNO(IZ) = 2E-2+2.5E-6*BDSNOI(IZ)*BDSNOI(IZ)   ! Anderson, 1976
!    TKSNO(IZ) = 0.35                                ! constant
!    TKSNO(IZ) = 2.576E-6*BDSNOI(IZ)**2. + 0.074    ! Verseghy (1991)
!    TKSNO(IZ) = 2.22*(BDSNOI(IZ)/1000.)**1.88      ! Douvill(Yen, 1981)
  ENDDO

  END SUBROUTINE CSNOW

!== begin tdfcnd ===================================================================================

  SUBROUTINE TDFCND (parameters, ISOIL, DF, SMC, SH2O)
! --------------------------------------------------------------------------------------------------
! Calculate thermal diffusivity and conductivity of the soil.
! Peters-Lidard approach (Peters-Lidard et al., 1998)
! --------------------------------------------------------------------------------------------------
! Code history:
! June 2001 changes: frozen soil condition.
! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE
  type (noahmp_parameters), intent(in) :: parameters
    INTEGER, INTENT(IN)    :: ISOIL  ! soil layer
    REAL, INTENT(IN)       :: SMC    ! total soil water
    REAL, INTENT(IN)       :: SH2O   ! liq. soil water
    REAL, INTENT(OUT)      :: DF     ! thermal diffusivity

! local variables
    REAL  :: AKE
    REAL  :: GAMMD
    REAL  :: THKDRY
    REAL  :: THKO     ! thermal conductivity for other soil components         
    REAL  :: THKQTZ   ! thermal conductivity for quartz
    REAL  :: THKSAT   ! 
    REAL  :: THKS     ! thermal conductivity for the solids
    REAL  :: THKW     ! water thermal conductivity
    REAL  :: SATRATIO
    REAL  :: XU
    REAL  :: XUNFROZ
! --------------------------------------------------------------------------------------------------
! We now get quartz as an input argument (set in routine redprm):
!      DATA QUARTZ /0.82, 0.10, 0.25, 0.60, 0.52,
!     &             0.35, 0.60, 0.40, 0.82/
! --------------------------------------------------------------------------------------------------
! If the soil has any moisture content compute a partial sum/product
! otherwise use a constant value which works well with most soils
! --------------------------------------------------------------------------------------------------
!  QUARTZ ....QUARTZ CONTENT (SOIL TYPE DEPENDENT)
! --------------------------------------------------------------------------------------------------
! USE AS IN PETERS-LIDARD, 1998 (MODIF. FROM JOHANSEN, 1975).

!                                  PABLO GRUNMANN, 08/17/98
! Refs.:
!      Farouki, O.T.,1986: Thermal properties of soils. Series on Rock
!              and Soil Mechanics, Vol. 11, Trans Tech, 136 pp.
!      Johansen, O., 1975: Thermal conductivity of soils. PH.D. Thesis,
!              University of Trondheim,
!      Peters-Lidard, C. D., et al., 1998: The effect of soil thermal
!              conductivity parameterization on surface energy fluxes
!              and temperatures. Journal of The Atmospheric Sciences,
!              Vol. 55, pp. 1209-1224.
! --------------------------------------------------------------------------------------------------
! NEEDS PARAMETERS
! POROSITY(SOIL TYPE):
!      POROS = SMCMAX
! SATURATION RATIO:
! PARAMETERS  W/(M.K)
    SATRATIO = SMC / parameters%SMCMAX(ISOIL)
    THKW = 0.57
!      IF (QUARTZ .LE. 0.2) THKO = 3.0
    THKO = 2.0
! SOLIDS' CONDUCTIVITY
! QUARTZ' CONDUCTIVITY
    THKQTZ = 7.7

! UNFROZEN FRACTION (FROM 1., i.e., 100%LIQUID, TO 0. (100% FROZEN))
    THKS = (THKQTZ ** parameters%QUARTZ(ISOIL))* (THKO ** (1. - parameters%QUARTZ(ISOIL)))

! UNFROZEN VOLUME FOR SATURATION (POROSITY*XUNFROZ)
    XUNFROZ = 1.0                       ! Prevent divide by zero (suggested by D. Mocko)
    IF(SMC > 0.) XUNFROZ = SH2O / SMC
! SATURATED THERMAL CONDUCTIVITY
    XU = XUNFROZ * parameters%SMCMAX(ISOIL)

! DRY DENSITY IN KG/M3
    THKSAT = THKS ** (1. - parameters%SMCMAX(ISOIL))* TKICE ** (parameters%SMCMAX(ISOIL) - XU)* THKW **   &
         (XU)

! DRY THERMAL CONDUCTIVITY IN W.M-1.K-1
    GAMMD = (1. - parameters%SMCMAX(ISOIL))*2700.

    THKDRY = (0.135* GAMMD+ 64.7)/ (2700. - 0.947* GAMMD)
! FROZEN
    IF ( (SH2O + 0.0005) <  SMC ) THEN
       AKE = SATRATIO
! UNFROZEN
! RANGE OF VALIDITY FOR THE KERSTEN NUMBER (AKE)
    ELSE

! KERSTEN NUMBER (USING "FINE" FORMULA, VALID FOR SOILS CONTAINING AT
! LEAST 5% OF PARTICLES WITH DIAMETER LESS THAN 2.E-6 METERS.)
! (FOR "COARSE" FORMULA, SEE PETERS-LIDARD ET AL., 1998).

       IF ( SATRATIO >  0.1 ) THEN

          AKE = LOG10 (SATRATIO) + 1.0

! USE K = KDRY
       ELSE

          AKE = 0.0
       END IF
!  THERMAL CONDUCTIVITY

    END IF

    DF = AKE * (THKSAT - THKDRY) + THKDRY


  end subroutine TDFCND

!== begin radiation ================================================================================

  SUBROUTINE RADIATION (parameters,VEGTYP  ,IST     ,ICE     ,NSOIL   , & !in
                        SNEQVO  ,SNEQV   ,DT      ,COSZ    ,SNOWH   , & !in
                        TG      ,TV      ,FSNO    ,QSNOW   ,FWET    , & !in
                        ELAI    ,ESAI    ,SMC     ,SOLAD   ,SOLAI   , & !in
                        FVEG    ,ILOC    ,JLOC    ,                   & !in
                        ALBOLD  ,TAUSS   ,                            & !inout
                        FSUN    ,LAISUN  ,LAISHA  ,PARSUN  ,PARSHA  , & !out
                        SAV     ,SAG     ,FSR     ,FSA     ,FSRV    , &
                        FSRG    ,ALBSND  ,ALBSNI  ,BGAP    ,WGAP    )   !out
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER, INTENT(IN)                  :: ILOC
  INTEGER, INTENT(IN)                  :: JLOC
  INTEGER, INTENT(IN)                  :: VEGTYP !vegetation type
  INTEGER, INTENT(IN)                  :: IST    !surface type
  INTEGER, INTENT(IN)                  :: ICE    !ice (ice = 1)
  INTEGER, INTENT(IN)                  :: NSOIL  !number of soil layers

  REAL, INTENT(IN)                     :: DT     !time step [s]
  REAL, INTENT(IN)                     :: QSNOW  !snowfall (mm/s)
  REAL, INTENT(IN)                     :: SNEQVO !snow mass at last time step(mm)
  REAL, INTENT(IN)                     :: SNEQV  !snow mass (mm)
  REAL, INTENT(IN)                     :: SNOWH  !snow height (mm)
  REAL, INTENT(IN)                     :: COSZ   !cosine solar zenith angle (0-1)
  REAL, INTENT(IN)                     :: TG     !ground temperature (k)
  REAL, INTENT(IN)                     :: TV     !vegetation temperature (k)
  REAL, INTENT(IN)                     :: ELAI   !LAI, one-sided, adjusted for burying by snow
  REAL, INTENT(IN)                     :: ESAI   !SAI, one-sided, adjusted for burying by snow
  REAL, INTENT(IN)                     :: FWET   !fraction of canopy that is wet
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SMC    !volumetric soil water [m3/m3]
  REAL, DIMENSION(1:2)    , INTENT(IN) :: SOLAD  !incoming direct solar radiation (w/m2)
  REAL, DIMENSION(1:2)    , INTENT(IN) :: SOLAI  !incoming diffuse solar radiation (w/m2)
  REAL, INTENT(IN)                     :: FSNO   !snow cover fraction (-)
  REAL, INTENT(IN)                     :: FVEG   !green vegetation fraction [0.0-1.0]

! inout
  REAL,                  INTENT(INOUT) :: ALBOLD !snow albedo at last time step (CLASS type)
  REAL,                  INTENT(INOUT) :: TAUSS  !non-dimensional snow age.

! output
  REAL, INTENT(OUT)                    :: FSUN   !sunlit fraction of canopy (-)
  REAL, INTENT(OUT)                    :: LAISUN !sunlit leaf area (-)
  REAL, INTENT(OUT)                    :: LAISHA !shaded leaf area (-)
  REAL, INTENT(OUT)                    :: PARSUN !average absorbed par for sunlit leaves (w/m2)
  REAL, INTENT(OUT)                    :: PARSHA !average absorbed par for shaded leaves (w/m2)
  REAL, INTENT(OUT)                    :: SAV    !solar radiation absorbed by vegetation (w/m2)
  REAL, INTENT(OUT)                    :: SAG    !solar radiation absorbed by ground (w/m2)
  REAL, INTENT(OUT)                    :: FSA    !total absorbed solar radiation (w/m2)
  REAL, INTENT(OUT)                    :: FSR    !total reflected solar radiation (w/m2)

!jref:start  
  REAL, INTENT(OUT)                    :: FSRV    !veg. reflected solar radiation (w/m2)
  REAL, INTENT(OUT)                    :: FSRG    !ground reflected solar radiation (w/m2)
  REAL, INTENT(OUT)                    :: BGAP
  REAL, INTENT(OUT)                    :: WGAP
  REAL, DIMENSION(1:2), INTENT(OUT)    :: ALBSND   !snow albedo (direct)
  REAL, DIMENSION(1:2), INTENT(OUT)    :: ALBSNI   !snow albedo (diffuse)
!jref:end  

! local
  REAL                                 :: FAGE   !snow age function (0 - new snow)
  REAL, DIMENSION(1:2)                 :: ALBGRD !ground albedo (direct)
  REAL, DIMENSION(1:2)                 :: ALBGRI !ground albedo (diffuse)
  REAL, DIMENSION(1:2)                 :: ALBD   !surface albedo (direct)
  REAL, DIMENSION(1:2)                 :: ALBI   !surface albedo (diffuse)
  REAL, DIMENSION(1:2)                 :: FABD   !flux abs by veg (per unit direct flux)
  REAL, DIMENSION(1:2)                 :: FABI   !flux abs by veg (per unit diffuse flux)
  REAL, DIMENSION(1:2)                 :: FTDD   !down direct flux below veg (per unit dir flux)
  REAL, DIMENSION(1:2)                 :: FTID   !down diffuse flux below veg (per unit dir flux)
  REAL, DIMENSION(1:2)                 :: FTII   !down diffuse flux below veg (per unit dif flux)
!jref:start  
  REAL, DIMENSION(1:2)                 :: FREVI
  REAL, DIMENSION(1:2)                 :: FREVD
  REAL, DIMENSION(1:2)                 :: FREGI
  REAL, DIMENSION(1:2)                 :: FREGD
!jref:end

  REAL                                 :: FSHA   !shaded fraction of canopy
  REAL                                 :: VAI    !total LAI + stem area index, one sided

  REAL,PARAMETER :: MPE = 1.E-6
  LOGICAL VEG  !true: vegetated for surface temperature calculation

! --------------------------------------------------------------------------------------------------

! surface abeldo

   CALL ALBEDO (parameters,VEGTYP ,IST    ,ICE    ,NSOIL  , & !in
                DT     ,COSZ   ,FAGE   ,ELAI   ,ESAI   , & !in
                TG     ,TV     ,SNOWH  ,FSNO   ,FWET   , & !in
                SMC    ,SNEQVO ,SNEQV  ,QSNOW  ,FVEG   , & !in
                ILOC   ,JLOC   ,                         & !in
                ALBOLD ,TAUSS                          , & !inout
                ALBGRD ,ALBGRI ,ALBD   ,ALBI   ,FABD   , & !out
                FABI   ,FTDD   ,FTID   ,FTII   ,FSUN   , & !)   !out
                FREVI  ,FREVD   ,FREGD ,FREGI  ,BGAP   , & !inout
                WGAP   ,ALBSND ,ALBSNI )

! surface radiation

     FSHA = 1.-FSUN
     LAISUN = ELAI*FSUN
     LAISHA = ELAI*FSHA
     VAI = ELAI+ ESAI
     IF (VAI .GT. 0.) THEN
        VEG = .TRUE.
     ELSE
        VEG = .FALSE.
     END IF

   CALL SURRAD (parameters,MPE    ,FSUN   ,FSHA   ,ELAI   ,VAI    , & !in
                LAISUN ,LAISHA ,SOLAD  ,SOLAI  ,FABD   , & !in
                FABI   ,FTDD   ,FTID   ,FTII   ,ALBGRD , & !in
                ALBGRI ,ALBD   ,ALBI   ,ILOC   ,JLOC   , & !in
                PARSUN ,PARSHA ,SAV    ,SAG    ,FSA    , & !out
                FSR    ,                                 & !out
                FREVI  ,FREVD  ,FREGD  ,FREGI  ,FSRV   , & !inout
                FSRG)

  END SUBROUTINE RADIATION

!== begin albedo ===================================================================================

  SUBROUTINE ALBEDO (parameters,VEGTYP ,IST    ,ICE    ,NSOIL  , & !in
                     DT     ,COSZ   ,FAGE   ,ELAI   ,ESAI   , & !in
                     TG     ,TV     ,SNOWH  ,FSNO   ,FWET   , & !in
                     SMC    ,SNEQVO ,SNEQV  ,QSNOW  ,FVEG   , & !in
                     ILOC   ,JLOC   ,                         & !in
                     ALBOLD ,TAUSS                          , & !inout
                     ALBGRD ,ALBGRI ,ALBD   ,ALBI   ,FABD   , & !out
                     FABI   ,FTDD   ,FTID   ,FTII   ,FSUN   , & !out
                     FREVI  ,FREVD  ,FREGD  ,FREGI  ,BGAP   , & !out
                     WGAP   ,ALBSND ,ALBSNI )

! --------------------------------------------------------------------------------------------------
! surface albedos. also fluxes (per unit incoming direct and diffuse
! radiation) reflected, transmitted, and absorbed by vegetation.
! also sunlit fraction of the canopy.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                  INTENT(IN)  :: ILOC
  INTEGER,                  INTENT(IN)  :: JLOC
  INTEGER,                  INTENT(IN)  :: NSOIL  !number of soil layers
  INTEGER,                  INTENT(IN)  :: VEGTYP !vegetation type
  INTEGER,                  INTENT(IN)  :: IST    !surface type
  INTEGER,                  INTENT(IN)  :: ICE    !ice (ice = 1)

  REAL,                     INTENT(IN)  :: DT     !time step [sec]
  REAL,                     INTENT(IN)  :: QSNOW  !snowfall
  REAL,                     INTENT(IN)  :: COSZ   !cosine solar zenith angle for next time step
  REAL,                     INTENT(IN)  :: SNOWH  !snow height (mm)
  REAL,                     INTENT(IN)  :: TG     !ground temperature (k)
  REAL,                     INTENT(IN)  :: TV     !vegetation temperature (k)
  REAL,                     INTENT(IN)  :: ELAI   !LAI, one-sided, adjusted for burying by snow
  REAL,                     INTENT(IN)  :: ESAI   !SAI, one-sided, adjusted for burying by snow
  REAL,                     INTENT(IN)  :: FSNO   !fraction of grid covered by snow
  REAL,                     INTENT(IN)  :: FWET   !fraction of canopy that is wet
  REAL,                     INTENT(IN)  :: SNEQVO !snow mass at last time step(mm)
  REAL,                     INTENT(IN)  :: SNEQV  !snow mass (mm)
  REAL,                     INTENT(IN)  :: FVEG   !green vegetation fraction [0.0-1.0]
  REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC    !volumetric soil water (m3/m3)

! inout
  REAL,                  INTENT(INOUT)  :: ALBOLD !snow albedo at last time step (CLASS type)
  REAL,                  INTENT(INOUT)  :: TAUSS  !non-dimensional snow age

! output
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRD !ground albedo (direct)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRI !ground albedo (diffuse)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBD   !surface albedo (direct)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBI   !surface albedo (diffuse)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FABD   !flux abs by veg (per unit direct flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FABI   !flux abs by veg (per unit diffuse flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTDD   !down direct flux below veg (per unit dir flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTID   !down diffuse flux below veg (per unit dir flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTII   !down diffuse flux below veg (per unit dif flux)
  REAL,                     INTENT(OUT) :: FSUN   !sunlit fraction of canopy (-)
!jref:start
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREVD
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREVI
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREGD
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREGI
  REAL, INTENT(OUT) :: BGAP
  REAL, INTENT(OUT) :: WGAP
!jref:end

! ------------------------------------------------------------------------
! ------------------------ local variables -------------------------------
! local
  REAL                 :: FAGE     !snow age function
  REAL                 :: ALB
  INTEGER              :: IB       !indices
  INTEGER              :: NBAND    !number of solar radiation wave bands
  INTEGER              :: IC       !direct beam: ic=0; diffuse: ic=1

  REAL                 :: WL       !fraction of LAI+SAI that is LAI
  REAL                 :: WS       !fraction of LAI+SAI that is SAI
  REAL                 :: MPE      !prevents overflow for division by zero

  REAL, DIMENSION(1:2) :: RHO      !leaf/stem reflectance weighted by fraction LAI and SAI
  REAL, DIMENSION(1:2) :: TAU      !leaf/stem transmittance weighted by fraction LAI and SAI
  REAL, DIMENSION(1:2) :: FTDI     !down direct flux below veg per unit dif flux = 0
  REAL, DIMENSION(1:2), INTENT(OUT) :: ALBSND   !snow albedo (direct)
  REAL, DIMENSION(1:2), INTENT(OUT) :: ALBSNI   !snow albedo (diffuse)

  REAL                 :: VAI      !ELAI+ESAI
  REAL                 :: GDIR     !average projected leaf/stem area in solar direction
  REAL                 :: EXT      !optical depth direct beam per unit leaf + stem area

! --------------------------------------------------------------------------------------------------

  NBAND = 2
  MPE = 1.E-06
  BGAP = 0.
  WGAP = 0.

! initialize output because solar radiation only done if COSZ > 0

  DO IB = 1, NBAND
    ALBD(IB) = 0.
    ALBI(IB) = 0.
    ALBGRD(IB) = 0.
    ALBGRI(IB) = 0.
    ALBSND(IB) = 0.
    ALBSNI(IB) = 0.
    FABD(IB) = 0.
    FABI(IB) = 0.
    FTDD(IB) = 0.
    FTID(IB) = 0.
    FTII(IB) = 0.
    IF (IB.EQ.1) FSUN = 0.
  END DO

  IF(COSZ <= 0) GOTO 100

! weight reflectance/transmittance by LAI and SAI

  DO IB = 1, NBAND
    VAI = ELAI + ESAI
    WL  = ELAI / MAX(VAI,MPE)
    WS  = ESAI / MAX(VAI,MPE)
    RHO(IB) = MAX(parameters%RHOL(IB)*WL+parameters%RHOS(IB)*WS, MPE)
    TAU(IB) = MAX(parameters%TAUL(IB)*WL+parameters%TAUS(IB)*WS, MPE)
  END DO

! snow age

   CALL SNOW_AGE (parameters,DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)

! snow albedos: only if COSZ > 0 and FSNO > 0

  IF(OPT_ALB == 1) &
     CALL SNOWALB_BATS (parameters,NBAND, FSNO,COSZ,FAGE,ALBSND,ALBSNI)
  IF(OPT_ALB == 2) THEN
     CALL SNOWALB_CLASS (parameters,NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI,ILOC,JLOC)
     ALBOLD = ALB
  END IF

! ground surface albedo

  CALL GROUNDALB (parameters,NSOIL   ,NBAND   ,ICE     ,IST     , & !in
                  FSNO    ,SMC     ,ALBSND  ,ALBSNI  ,COSZ    , & !in
                  TG      ,ILOC    ,JLOC    ,                   & !in
                  ALBGRD  ,ALBGRI  )                              !out

! loop over NBAND wavebands to calculate surface albedos and solar
! fluxes for unit incoming direct (IC=0) and diffuse flux (IC=1)

  DO IB = 1, NBAND
      IC = 0      ! direct
      CALL TWOSTREAM (parameters,IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !in
                      FWET   ,TV      ,ALBGRD  ,ALBGRI  ,RHO    , & !in
                      TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & !in
                      FABD   ,ALBD    ,FTDD    ,FTID    ,GDIR   , &!)   !out
                      FREVD  ,FREGD   ,BGAP    ,WGAP)

      IC = 1      ! diffuse
      CALL TWOSTREAM (parameters,IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !in
                      FWET   ,TV      ,ALBGRD  ,ALBGRI  ,RHO    , & !in
                      TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & !in
                      FABI   ,ALBI    ,FTDI    ,FTII    ,GDIR   , & !)   !out
                      FREVI  ,FREGI   ,BGAP    ,WGAP)

  END DO

! sunlit fraction of canopy. set FSUN = 0 if FSUN < 0.01.

  EXT = GDIR/COSZ * SQRT(1.-RHO(1)-TAU(1))
  FSUN = (1.-EXP(-EXT*VAI)) / MAX(EXT*VAI,MPE)
  EXT = FSUN

  IF (EXT .LT. 0.01) THEN
     WL = 0.
  ELSE
     WL = EXT 
  END IF
  FSUN = WL

100 CONTINUE

  END SUBROUTINE ALBEDO

!== begin surrad ===================================================================================

  SUBROUTINE SURRAD (parameters,MPE     ,FSUN    ,FSHA    ,ELAI    ,VAI     , & !in
                     LAISUN  ,LAISHA  ,SOLAD   ,SOLAI   ,FABD    , & !in
                     FABI    ,FTDD    ,FTID    ,FTII    ,ALBGRD  , & !in
                     ALBGRI  ,ALBD    ,ALBI    ,ILOC    ,JLOC    , & !in
                     PARSUN  ,PARSHA  ,SAV     ,SAG     ,FSA     , & !out
                     FSR     , & !)                                       !out
                     FREVI   ,FREVD   ,FREGD   ,FREGI   ,FSRV    , &
                     FSRG) !inout

! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER, INTENT(IN)              :: ILOC
  INTEGER, INTENT(IN)              :: JLOC
  REAL, INTENT(IN)                 :: MPE     !prevents underflow errors if division by zero

  REAL, INTENT(IN)                 :: FSUN    !sunlit fraction of canopy
  REAL, INTENT(IN)                 :: FSHA    !shaded fraction of canopy
  REAL, INTENT(IN)                 :: ELAI    !leaf area, one-sided
  REAL, INTENT(IN)                 :: VAI     !leaf + stem area, one-sided
  REAL, INTENT(IN)                 :: LAISUN  !sunlit leaf area index, one-sided
  REAL, INTENT(IN)                 :: LAISHA  !shaded leaf area index, one-sided

  REAL, DIMENSION(1:2), INTENT(IN) :: SOLAD   !incoming direct solar radiation (w/m2)
  REAL, DIMENSION(1:2), INTENT(IN) :: SOLAI   !incoming diffuse solar radiation (w/m2)
  REAL, DIMENSION(1:2), INTENT(IN) :: FABD    !flux abs by veg (per unit incoming direct flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FABI    !flux abs by veg (per unit incoming diffuse flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FTDD    !down dir flux below veg (per incoming dir flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FTID    !down dif flux below veg (per incoming dir flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FTII    !down dif flux below veg (per incoming dif flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBGRD  !ground albedo (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBGRI  !ground albedo (diffuse)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBD    !overall surface albedo (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBI    !overall surface albedo (diffuse)

  REAL, DIMENSION(1:2), INTENT(IN) :: FREVD    !overall surface albedo veg (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: FREVI    !overall surface albedo veg (diffuse)
  REAL, DIMENSION(1:2), INTENT(IN) :: FREGD    !overall surface albedo grd (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: FREGI    !overall surface albedo grd (diffuse)

! output

  REAL, INTENT(OUT)                :: PARSUN  !average absorbed par for sunlit leaves (w/m2)
  REAL, INTENT(OUT)                :: PARSHA  !average absorbed par for shaded leaves (w/m2)
  REAL, INTENT(OUT)                :: SAV     !solar radiation absorbed by vegetation (w/m2)
  REAL, INTENT(OUT)                :: SAG     !solar radiation absorbed by ground (w/m2)
  REAL, INTENT(OUT)                :: FSA     !total absorbed solar radiation (w/m2)
  REAL, INTENT(OUT)                :: FSR     !total reflected solar radiation (w/m2)
  REAL, INTENT(OUT)                :: FSRV    !reflected solar radiation by vegetation
  REAL, INTENT(OUT)                :: FSRG    !reflected solar radiation by ground

! ------------------------ local variables ----------------------------------------------------
  INTEGER                          :: IB      !waveband number (1=vis, 2=nir)
  INTEGER                          :: NBAND   !number of solar radiation waveband classes

  REAL                             :: ABS     !absorbed solar radiation (w/m2)
  REAL                             :: RNIR    !reflected solar radiation [nir] (w/m2)
  REAL                             :: RVIS    !reflected solar radiation [vis] (w/m2)
  REAL                             :: LAIFRA  !leaf area fraction of canopy
  REAL                             :: TRD     !transmitted solar radiation: direct (w/m2)
  REAL                             :: TRI     !transmitted solar radiation: diffuse (w/m2)
  REAL, DIMENSION(1:2)             :: CAD     !direct beam absorbed by canopy (w/m2)
  REAL, DIMENSION(1:2)             :: CAI     !diffuse radiation absorbed by canopy (w/m2)
! ---------------------------------------------------------------------------------------------
   NBAND = 2

! zero summed solar fluxes

    SAG = 0.
    SAV = 0.
    FSA = 0.

! loop over nband wavebands

  DO IB = 1, NBAND

! absorbed by canopy

    CAD(IB) = SOLAD(IB)*FABD(IB)    
    CAI(IB) = SOLAI(IB)*FABI(IB)
    SAV     = SAV + CAD(IB) + CAI(IB)
    FSA     = FSA + CAD(IB) + CAI(IB)
 
! transmitted solar fluxes incident on ground

    TRD = SOLAD(IB)*FTDD(IB)
    TRI = SOLAD(IB)*FTID(IB) + SOLAI(IB)*FTII(IB)

! solar radiation absorbed by ground surface

    ABS = TRD*(1.-ALBGRD(IB)) + TRI*(1.-ALBGRI(IB))
    SAG = SAG + ABS
    FSA = FSA + ABS
  END DO

! partition visible canopy absorption to sunlit and shaded fractions
! to get average absorbed par for sunlit and shaded leaves

     LAIFRA = ELAI / MAX(VAI,MPE)
     IF (FSUN .GT. 0.) THEN
        PARSUN = (CAD(1)+FSUN*CAI(1)) * LAIFRA / MAX(LAISUN,MPE)
        PARSHA = (FSHA*CAI(1))*LAIFRA / MAX(LAISHA,MPE)
     ELSE
        PARSUN = 0.
        PARSHA = (CAD(1)+CAI(1))*LAIFRA /MAX(LAISHA,MPE)
     ENDIF

! reflected solar radiation

     RVIS = ALBD(1)*SOLAD(1) + ALBI(1)*SOLAI(1)
     RNIR = ALBD(2)*SOLAD(2) + ALBI(2)*SOLAI(2)
     FSR  = RVIS + RNIR

! reflected solar radiation of veg. and ground (combined ground)
     FSRV = FREVD(1)*SOLAD(1)+FREVI(1)*SOLAI(1)+FREVD(2)*SOLAD(2)+FREVI(2)*SOLAI(2)
     FSRG = FREGD(1)*SOLAD(1)+FREGI(1)*SOLAI(1)+FREGD(2)*SOLAD(2)+FREGI(2)*SOLAI(2)


  END SUBROUTINE SURRAD

!== begin snow_age =================================================================================

  SUBROUTINE SNOW_AGE (parameters,DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------ code history ------------------------------------------------------------
! from BATS
! ------------------------ input/output variables --------------------------------------------------
!input
  type (noahmp_parameters), intent(in) :: parameters
   REAL, INTENT(IN) :: DT        !main time step (s)
   REAL, INTENT(IN) :: TG        !ground temperature (k)
   REAL, INTENT(IN) :: SNEQVO    !snow mass at last time step(mm)
   REAL, INTENT(IN) :: SNEQV     !snow water per unit ground area (mm)

!output
   REAL, INTENT(OUT) :: FAGE     !snow age

!input/output
   REAL, INTENT(INOUT) :: TAUSS      !non-dimensional snow age
!local
   REAL            :: TAGE       !total aging effects
   REAL            :: AGE1       !effects of grain growth due to vapor diffusion
   REAL            :: AGE2       !effects of grain growth at freezing of melt water
   REAL            :: AGE3       !effects of soot
   REAL            :: DELA       !temporary variable
   REAL            :: SGE        !temporary variable
   REAL            :: DELS       !temporary variable
   REAL            :: DELA0      !temporary variable
   REAL            :: ARG        !temporary variable
! See Yang et al. (1997) J.of Climate for detail.
!---------------------------------------------------------------------------------------------------

   IF(SNEQV.LE.0.0) THEN
          TAUSS = 0.
   ELSE
          DELA0 = DT/parameters%TAU0
          ARG   = parameters%GRAIN_GROWTH*(1./TFRZ-1./TG)
          AGE1  = EXP(ARG)
          AGE2  = EXP(AMIN1(0.,parameters%EXTRA_GROWTH*ARG))
          AGE3  = parameters%DIRT_SOOT
          TAGE  = AGE1+AGE2+AGE3
          DELA  = DELA0*TAGE
          DELS  = AMAX1(0.0,SNEQV-SNEQVO) / parameters%SWEMX
          SGE   = (TAUSS+DELA)*(1.0-DELS)
          TAUSS = AMAX1(0.,SGE)
   ENDIF

   FAGE= TAUSS/(TAUSS+1.)

  END SUBROUTINE SNOW_AGE

!== begin snowalb_bats =============================================================================

  SUBROUTINE SNOWALB_BATS (parameters,NBAND,FSNO,COSZ,FAGE,ALBSND,ALBSNI)
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,INTENT(IN) :: NBAND  !number of waveband classes

  REAL,INTENT(IN) :: COSZ    !cosine solar zenith angle
  REAL,INTENT(IN) :: FSNO    !snow cover fraction (-)
  REAL,INTENT(IN) :: FAGE    !snow age correction

! output

  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSND !snow albedo for direct(1=vis, 2=nir)
  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSNI !snow albedo for diffuse
! ---------------------------------------------------------------------------------------------

! ------------------------ local variables ----------------------------------------------------
  INTEGER :: IB          !waveband class

  REAL :: FZEN                 !zenith angle correction
  REAL :: CF1                  !temperary variable
  REAL :: SL2                  !2.*SL
  REAL :: SL1                  !1/SL
  REAL :: SL                   !adjustable parameter
!  REAL, PARAMETER :: C1 = 0.2  !default in BATS 
!  REAL, PARAMETER :: C2 = 0.5  !default in BATS
!  REAL, PARAMETER :: C1 = 0.2 * 2. ! double the default to match Sleepers River's
!  REAL, PARAMETER :: C2 = 0.5 * 2. ! snow surface albedo (double aging effects)
! ---------------------------------------------------------------------------------------------
! zero albedos for all points

        ALBSND(1: NBAND) = 0.
        ALBSNI(1: NBAND) = 0.

! when cosz > 0

        SL=parameters%BATS_COSZ
        SL1=1./SL
        SL2=2.*SL
        CF1=((1.+SL1)/(1.+SL2*COSZ)-SL1)
        FZEN=AMAX1(CF1,0.)

        ALBSNI(1)=parameters%BATS_VIS_NEW*(1.-parameters%BATS_VIS_AGE*FAGE)         
        ALBSNI(2)=parameters%BATS_NIR_NEW*(1.-parameters%BATS_NIR_AGE*FAGE)        

        ALBSND(1)=ALBSNI(1)+parameters%BATS_VIS_DIR*FZEN*(1.-ALBSNI(1))    !  vis direct
        ALBSND(2)=ALBSNI(2)+parameters%BATS_VIS_DIR*FZEN*(1.-ALBSNI(2))    !  nir direct

  END SUBROUTINE SNOWALB_BATS

!== begin snowalb_class ============================================================================

  SUBROUTINE SNOWALB_CLASS (parameters,NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI,ILOC,JLOC)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,INTENT(IN) :: ILOC !grid index
  INTEGER,INTENT(IN) :: JLOC !grid index
  INTEGER,INTENT(IN) :: NBAND  !number of waveband classes

  REAL,INTENT(IN) :: QSNOW     !snowfall (mm/s)
  REAL,INTENT(IN) :: DT        !time step (sec)
  REAL,INTENT(IN) :: ALBOLD    !snow albedo at last time step

! in & out

  REAL,                INTENT(INOUT) :: ALB        ! 
! output

  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSND !snow albedo for direct(1=vis, 2=nir)
  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSNI !snow albedo for diffuse
! ---------------------------------------------------------------------------------------------

! ------------------------ local variables ----------------------------------------------------
  INTEGER :: IB          !waveband class

! ---------------------------------------------------------------------------------------------
! zero albedos for all points

        ALBSND(1: NBAND) = 0.
        ALBSNI(1: NBAND) = 0.

! when cosz > 0

         ALB = 0.55 + (ALBOLD-0.55) * EXP(-0.01*DT/3600.)

! 1 mm fresh snow(SWE) -- 10mm snow depth, assumed the fresh snow density 100kg/m3
! here assume 1cm snow depth will fully cover the old snow

         IF (QSNOW > 0.) then
           ALB = ALB + MIN(QSNOW,parameters%SWEMX/DT) * (0.84-ALB)/(parameters%SWEMX/DT)
         ENDIF

         ALBSNI(1)= ALB         ! vis diffuse
         ALBSNI(2)= ALB         ! nir diffuse
         ALBSND(1)= ALB         ! vis direct
         ALBSND(2)= ALB         ! nir direct

  END SUBROUTINE SNOWALB_CLASS

!== begin groundalb ================================================================================

  SUBROUTINE GROUNDALB (parameters,NSOIL   ,NBAND   ,ICE     ,IST     , & !in
                        FSNO    ,SMC     ,ALBSND  ,ALBSNI  ,COSZ    , & !in
                        TG      ,ILOC    ,JLOC    ,                   & !in
                        ALBGRD  ,ALBGRI  )                              !out
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
!input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                  INTENT(IN)  :: ILOC   !grid index
  INTEGER,                  INTENT(IN)  :: JLOC   !grid index
  INTEGER,                  INTENT(IN)  :: NSOIL  !number of soil layers
  INTEGER,                  INTENT(IN)  :: NBAND  !number of solar radiation waveband classes
  INTEGER,                  INTENT(IN)  :: ICE    !value of ist for land ice
  INTEGER,                  INTENT(IN)  :: IST    !surface type
  REAL,                     INTENT(IN)  :: FSNO   !fraction of surface covered with snow (-)
  REAL,                     INTENT(IN)  :: TG     !ground temperature (k)
  REAL,                     INTENT(IN)  :: COSZ   !cosine solar zenith angle (0-1)
  REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC    !volumetric soil water content (m3/m3)
  REAL, DIMENSION(1:    2), INTENT(IN)  :: ALBSND !direct beam snow albedo (vis, nir)
  REAL, DIMENSION(1:    2), INTENT(IN)  :: ALBSNI !diffuse snow albedo (vis, nir)

!output

  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRD !ground albedo (direct beam: vis, nir)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRI !ground albedo (diffuse: vis, nir)

!local 

  INTEGER                               :: IB     !waveband number (1=vis, 2=nir)
  REAL                                  :: INC    !soil water correction factor for soil albedo
  REAL                                  :: ALBSOD !soil albedo (direct)
  REAL                                  :: ALBSOI !soil albedo (diffuse)
! --------------------------------------------------------------------------------------------------

  DO IB = 1, NBAND
        INC = MAX(0.11-0.40*SMC(1), 0.)
        IF (IST .EQ. 1)  THEN                     !soil
           ALBSOD = MIN(parameters%ALBSAT(IB)+INC,parameters%ALBDRY(IB))
           ALBSOI = ALBSOD
        ELSE IF (TG .GT. TFRZ) THEN               !unfrozen lake, wetland
           ALBSOD = 0.06/(MAX(0.01,COSZ)**1.7 + 0.15)
           ALBSOI = 0.06
        ELSE                                      !frozen lake, wetland
           ALBSOD = parameters%ALBLAK(IB)
           ALBSOI = ALBSOD
        END IF

! increase desert and semi-desert albedos

!        IF (IST .EQ. 1 .AND. ISC .EQ. 9) THEN
!           ALBSOD = ALBSOD + 0.10
!           ALBSOI = ALBSOI + 0.10
!        end if

        ALBGRD(IB) = ALBSOD*(1.-FSNO) + ALBSND(IB)*FSNO
        ALBGRI(IB) = ALBSOI*(1.-FSNO) + ALBSNI(IB)*FSNO
  END DO

  END SUBROUTINE GROUNDALB

!== begin twostream ================================================================================

  SUBROUTINE TWOSTREAM (parameters,IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !in
                        FWET   ,T       ,ALBGRD  ,ALBGRI  ,RHO    , & !in
                        TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & !in
                        FAB    ,FRE     ,FTD     ,FTI     ,GDIR   , & !)   !out
                        FREV   ,FREG    ,BGAP    ,WGAP)

! --------------------------------------------------------------------------------------------------
! use two-stream approximation of Dickinson (1983) Adv Geophysics
! 25:305-353 and Sellers (1985) Int J Remote Sensing 6:1335-1372
! to calculate fluxes absorbed by vegetation, reflected by vegetation,
! and transmitted through vegetation for unit incoming direct or diffuse
! flux given an underlying surface with known albedo.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
   INTEGER,              INTENT(IN)  :: ILOC    !grid index
   INTEGER,              INTENT(IN)  :: JLOC    !grid index
   INTEGER,              INTENT(IN)  :: IST     !surface type
   INTEGER,              INTENT(IN)  :: IB      !waveband number
   INTEGER,              INTENT(IN)  :: IC      !0=unit incoming direct; 1=unit incoming diffuse
   INTEGER,              INTENT(IN)  :: VEGTYP  !vegetation type

   REAL,                 INTENT(IN)  :: COSZ    !cosine of direct zenith angle (0-1)
   REAL,                 INTENT(IN)  :: VAI     !one-sided leaf+stem area index (m2/m2)
   REAL,                 INTENT(IN)  :: FWET    !fraction of lai, sai that is wetted (-)
   REAL,                 INTENT(IN)  :: T       !surface temperature (k)

   REAL, DIMENSION(1:2), INTENT(IN)  :: ALBGRD  !direct  albedo of underlying surface (-)
   REAL, DIMENSION(1:2), INTENT(IN)  :: ALBGRI  !diffuse albedo of underlying surface (-)
   REAL, DIMENSION(1:2), INTENT(IN)  :: RHO     !leaf+stem reflectance
   REAL, DIMENSION(1:2), INTENT(IN)  :: TAU     !leaf+stem transmittance
   REAL,                 INTENT(IN)  :: FVEG    !green vegetation fraction [0.0-1.0]

! output

   REAL, DIMENSION(1:2), INTENT(OUT) :: FAB     !flux abs by veg layer (per unit incoming flux)
   REAL, DIMENSION(1:2), INTENT(OUT) :: FRE     !flux refl above veg layer (per unit incoming flux)
   REAL, DIMENSION(1:2), INTENT(OUT) :: FTD     !down dir flux below veg layer (per unit in flux)
   REAL, DIMENSION(1:2), INTENT(OUT) :: FTI     !down dif flux below veg layer (per unit in flux)
   REAL,                 INTENT(OUT) :: GDIR    !projected leaf+stem area in solar direction
   REAL, DIMENSION(1:2), INTENT(OUT) :: FREV    !flux reflected by veg layer   (per unit incoming flux) 
   REAL, DIMENSION(1:2), INTENT(OUT) :: FREG    !flux reflected by ground (per unit incoming flux)

! local
   REAL                              :: OMEGA   !fraction of intercepted radiation that is scattered
   REAL                              :: OMEGAL  !omega for leaves
   REAL                              :: BETAI   !upscatter parameter for diffuse radiation
   REAL                              :: BETAIL  !betai for leaves
   REAL                              :: BETAD   !upscatter parameter for direct beam radiation
   REAL                              :: BETADL  !betad for leaves
   REAL                              :: EXT     !optical depth of direct beam per unit leaf area
   REAL                              :: AVMU    !average diffuse optical depth

   REAL                              :: COSZI   !0.001 <= cosz <= 1.000
   REAL                              :: ASU     !single scattering albedo
   REAL                              :: CHIL    ! -0.4 <= xl <= 0.6

   REAL                              :: TMP0,TMP1,TMP2,TMP3,TMP4,TMP5,TMP6,TMP7,TMP8,TMP9
   REAL                              :: P1,P2,P3,P4,S1,S2,U1,U2,U3
   REAL                              :: B,C,D,D1,D2,F,H,H1,H2,H3,H4,H5,H6,H7,H8,H9,H10
   REAL                              :: PHI1,PHI2,SIGMA
   REAL                              :: FTDS,FTIS,FRES
   REAL                              :: DENFVEG
   REAL                              :: VAI_SPREAD
!jref:start
   REAL                              :: FREVEG,FREBAR,FTDVEG,FTIVEG,FTDBAR,FTIBAR
   REAL                              :: THETAZ
!jref:end   

!  variables for the modified two-stream scheme
!  Niu and Yang (2004), JGR

   REAL, PARAMETER :: PAI = 3.14159265 
   REAL :: HD       !crown depth (m)
   REAL :: BB       !vertical crown radius (m)
   REAL :: THETAP   !angle conversion from SZA 
   REAL :: FA       !foliage volume density (m-1)
   REAL :: NEWVAI   !effective LSAI (-)

   REAL,INTENT(INOUT) :: BGAP     !between canopy gap fraction for beam (-)
   REAL,INTENT(INOUT) :: WGAP     !within canopy gap fraction for beam (-)

   REAL :: KOPEN    !gap fraction for diffue light (-)
   REAL :: GAP      !total gap fraction for beam ( <=1-shafac )

! -----------------------------------------------------------------
! compute within and between gaps
     VAI_SPREAD = VAI
     if(VAI == 0.0) THEN
         GAP     = 1.0
         KOPEN   = 1.0
     ELSE
         IF(OPT_RAD == 1) THEN
	   DENFVEG = -LOG(MAX(1.0-FVEG,0.01))/(PAI*parameters%RC**2)
           HD      = parameters%HVT - parameters%HVB
           BB      = 0.5 * HD           
           THETAP  = ATAN(BB/parameters%RC * TAN(ACOS(MAX(0.01,COSZ))) )
           ! BGAP    = EXP(-parameters%DEN * PAI * parameters%RC**2/COS(THETAP) )
           BGAP    = EXP(-DENFVEG * PAI * parameters%RC**2/COS(THETAP) )
           FA      = VAI/(1.33 * PAI * parameters%RC**3.0 *(BB/parameters%RC)*DENFVEG)
           NEWVAI  = HD*FA
           WGAP    = (1.0-BGAP) * EXP(-0.5*NEWVAI/COSZ)
           GAP     = MIN(1.0-FVEG, BGAP+WGAP)

           KOPEN   = 0.05
         END IF

         IF(OPT_RAD == 2) THEN
           GAP     = 0.0
           KOPEN   = 0.0
         END IF

         IF(OPT_RAD == 3) THEN
           GAP     = 1.0-FVEG
           KOPEN   = 1.0-FVEG
         END IF
     end if

! calculate two-stream parameters OMEGA, BETAD, BETAI, AVMU, GDIR, EXT.
! OMEGA, BETAD, BETAI are adjusted for snow. values for OMEGA*BETAD
! and OMEGA*BETAI are calculated and then divided by the new OMEGA
! because the product OMEGA*BETAI, OMEGA*BETAD is used in solution.
! also, the transmittances and reflectances (TAU, RHO) are linear
! weights of leaf and stem values.

     COSZI  = MAX(0.001, COSZ)
     CHIL   = MIN( MAX(parameters%XL, -0.4), 0.6)
     IF (ABS(CHIL) .LE. 0.01) CHIL = 0.01
     PHI1   = 0.5 - 0.633*CHIL - 0.330*CHIL*CHIL
     PHI2   = 0.877 * (1.-2.*PHI1)
     GDIR   = PHI1 + PHI2*COSZI
     EXT    = GDIR/COSZI
     AVMU   = ( 1. - PHI1/PHI2 * LOG((PHI1+PHI2)/PHI1) ) / PHI2
     OMEGAL = RHO(IB) + TAU(IB)
     TMP0   = GDIR + PHI2*COSZI
     TMP1   = PHI1*COSZI
     ASU    = 0.5*OMEGAL*GDIR/TMP0 * ( 1.-TMP1/TMP0*LOG((TMP1+TMP0)/TMP1) )
     BETADL = (1.+AVMU*EXT)/(OMEGAL*AVMU*EXT)*ASU
     BETAIL = 0.5 * ( RHO(IB)+TAU(IB) + (RHO(IB)-TAU(IB))   &
            * ((1.+CHIL)/2.)**2 ) / OMEGAL

! adjust omega, betad, and betai for intercepted snow

     IF (T .GT. TFRZ) THEN                                !no snow
        TMP0 = OMEGAL
        TMP1 = BETADL
        TMP2 = BETAIL
     ELSE
        TMP0 =   (1.-FWET)*OMEGAL        + FWET*parameters%OMEGAS(IB)
        TMP1 = ( (1.-FWET)*OMEGAL*BETADL + FWET*parameters%OMEGAS(IB)*parameters%BETADS ) / TMP0
        TMP2 = ( (1.-FWET)*OMEGAL*BETAIL + FWET*parameters%OMEGAS(IB)*parameters%BETAIS ) / TMP0
     END IF

     OMEGA = TMP0
     BETAD = TMP1
     BETAI = TMP2

! absorbed, reflected, transmitted fluxes per unit incoming radiation

     B = 1. - OMEGA + OMEGA*BETAI
     C = OMEGA*BETAI
     TMP0 = AVMU*EXT
     D = TMP0 * OMEGA*BETAD
     F = TMP0 * OMEGA*(1.-BETAD)
     TMP1 = B*B - C*C
     H = SQRT(TMP1) / AVMU
     SIGMA = TMP0*TMP0 - TMP1
     if ( ABS (SIGMA) < 1.e-6 ) SIGMA = SIGN(1.e-6,SIGMA)
     P1 = B + AVMU*H
     P2 = B - AVMU*H
     P3 = B + TMP0
     P4 = B - TMP0
     S1 = EXP(-H*VAI)
     S2 = EXP(-EXT*VAI)
     IF (IC .EQ. 0) THEN
        U1 = B - C/ALBGRD(IB)
        U2 = B - C*ALBGRD(IB)
        U3 = F + C*ALBGRD(IB)
     ELSE
        U1 = B - C/ALBGRI(IB)
        U2 = B - C*ALBGRI(IB)
        U3 = F + C*ALBGRI(IB)
     END IF
     TMP2 = U1 - AVMU*H
     TMP3 = U1 + AVMU*H
     D1 = P1*TMP2/S1 - P2*TMP3*S1
     TMP4 = U2 + AVMU*H
     TMP5 = U2 - AVMU*H
     D2 = TMP4/S1 - TMP5*S1
     H1 = -D*P4 - C*F
     TMP6 = D - H1*P3/SIGMA
     TMP7 = ( D - C - H1/SIGMA*(U1+TMP0) ) * S2
     H2 = ( TMP6*TMP2/S1 - P2*TMP7 ) / D1
     H3 = - ( TMP6*TMP3*S1 - P1*TMP7 ) / D1
     H4 = -F*P3 - C*D
     TMP8 = H4/SIGMA
     TMP9 = ( U3 - TMP8*(U2-TMP0) ) * S2
     H5 = - ( TMP8*TMP4/S1 + TMP9 ) / D2
     H6 = ( TMP8*TMP5*S1 + TMP9 ) / D2
     H7 = (C*TMP2) / (D1*S1)
     H8 = (-C*TMP3*S1) / D1
     H9 = TMP4 / (D2*S1)
     H10 = (-TMP5*S1) / D2

! downward direct and diffuse fluxes below vegetation
! Niu and Yang (2004), JGR.

     IF (IC .EQ. 0) THEN
        FTDS = S2                           *(1.0-GAP) + GAP
        FTIS = (H4*S2/SIGMA + H5*S1 + H6/S1)*(1.0-GAP)
     ELSE
        FTDS = 0.
        FTIS = (H9*S1 + H10/S1)*(1.0-KOPEN) + KOPEN
     END IF
     FTD(IB) = FTDS
     FTI(IB) = FTIS

! flux reflected by the surface (veg. and ground)

     IF (IC .EQ. 0) THEN
        FRES   = (H1/SIGMA + H2 + H3)*(1.0-GAP  ) + ALBGRD(IB)*GAP        
        FREVEG = (H1/SIGMA + H2 + H3)*(1.0-GAP  ) 
        FREBAR = ALBGRD(IB)*GAP                   !jref - separate veg. and ground reflection
     ELSE
        FRES   = (H7 + H8) *(1.0-KOPEN) + ALBGRI(IB)*KOPEN        
        FREVEG = (H7 + H8) *(1.0-KOPEN) + ALBGRI(IB)*KOPEN
        FREBAR = 0                                !jref - separate veg. and ground reflection
     END IF
     FRE(IB) = FRES

     FREV(IB) = FREVEG 
     FREG(IB) = FREBAR 

! flux absorbed by vegetation

     FAB(IB) = 1. - FRE(IB) - (1.-ALBGRD(IB))*FTD(IB) &
                            - (1.-ALBGRI(IB))*FTI(IB)

!if(iloc == 1.and.jloc ==  2) then
!  write(*,'(a7,2i2,5(a6,f8.4),2(a9,f8.4))') "ib,ic: ",ib,ic," GAP: ",GAP," FTD: ",FTD(IB)," FTI: ",FTI(IB)," FRE: ", &
!         FRE(IB)," FAB: ",FAB(IB)," ALBGRD: ",ALBGRD(IB)," ALBGRI: ",ALBGRI(IB)
!end if

  END SUBROUTINE TWOSTREAM

!== begin vege_flux ================================================================================

  SUBROUTINE VEGE_FLUX(parameters,NSNOW   ,NSOIL   ,ISNOW   ,VEGTYP  ,VEG     , & !in
                       DT      ,SAV     ,SAG     ,LWDN    ,UR      , & !in
                       UU      ,VV      ,SFCTMP  ,THAIR   ,QAIR    , & !in
                       EAIR    ,RHOAIR  ,SNOWH   ,VAI     ,GAMMAV   ,GAMMAG,  & !in
                       FWET    ,LAISUN  ,LAISHA  ,CWP     ,DZSNSO  , & !in
                       ZLVL    ,ZPD     ,Z0M     ,FVEG    , & !in
                       Z0MG    ,EMV     ,EMG     ,CANLIQ  ,FSNO,          & !in
                       CANICE  ,STC     ,DF      ,RSSUN   ,RSSHA   , & !in
                       RSURF   ,LATHEAV ,LATHEAG  ,PARSUN  ,PARSHA  ,IGS     , & !in
                       FOLN    ,CO2AIR  ,O2AIR   ,BTRAN   ,SFCPRS  , & !in
                       RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHV    ,PAHG     , & !in
                       EAH     ,TAH     ,TV      ,TG      ,CM      , & !inout
                       CH      ,DX      ,DZ8W    ,                   & !
                       TAUXV   ,TAUYV   ,IRG     ,IRC     ,SHG     , & !out
                       SHC     ,EVG     ,EVC     ,TR      ,GH      , & !out
                       T2MV    ,PSNSUN  ,PSNSHA  ,                   & !out
                       QC      ,QSFC    ,PSFC    ,                   & !in
                       Q2V     ,CAH2    ,CHLEAF  ,CHUC,              & !inout 
                       SH2O,JULIAN, SWDOWN, PRCP, FB, FSR, GECROS1D)      ! Gecros 

! --------------------------------------------------------------------------------------------------
! use newton-raphson iteration to solve for vegetation (tv) and
! ground (tg) temperatures that balance the surface energy budgets

! vegetated:
! -SAV + IRC[TV] + SHC[TV] + EVC[TV] + TR[TV] = 0
! -SAG + IRG[TG] + SHG[TG] + EVG[TG] + GH[TG] = 0
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                         INTENT(IN) :: ILOC   !grid index
  INTEGER,                         INTENT(IN) :: JLOC   !grid index
  LOGICAL,                         INTENT(IN) :: VEG    !true if vegetated surface
  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers        
  INTEGER,                         INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER,                         INTENT(IN) :: ISNOW  !actual no. of snow layers
  INTEGER,                         INTENT(IN) :: VEGTYP !vegetation physiology type
  REAL,                            INTENT(IN) :: FVEG   !greeness vegetation fraction (-)
  REAL,                            INTENT(INOUT) :: SAV    !solar rad absorbed by veg (w/m2)
  REAL,                            INTENT(INOUT) :: SAG    !solar rad absorbed by ground (w/m2)
  REAL,                            INTENT(IN) :: LWDN   !atmospheric longwave radiation (w/m2)
  REAL,                            INTENT(IN) :: UR     !wind speed at height zlvl (m/s)
  REAL,                            INTENT(IN) :: UU     !wind speed in eastward dir (m/s)
  REAL,                            INTENT(IN) :: VV     !wind speed in northward dir (m/s)
  REAL,                            INTENT(IN) :: SFCTMP !air temperature at reference height (k)
  REAL,                            INTENT(IN) :: THAIR  !potential temp at reference height (k)
  REAL,                            INTENT(IN) :: EAIR   !vapor pressure air at zlvl (pa)
  REAL,                            INTENT(IN) :: QAIR   !specific humidity at zlvl (kg/kg)
  REAL,                            INTENT(IN) :: RHOAIR !density air (kg/m**3)
  REAL,                            INTENT(IN) :: DT     !time step (s)
  REAL,                            INTENT(IN) :: FSNO     !snow fraction

  REAL,                            INTENT(IN) :: SNOWH  !actual snow depth [m]
  REAL,                            INTENT(IN) :: FWET   !wetted fraction of canopy
  REAL,                            INTENT(IN) :: CWP    !canopy wind parameter

  REAL,                            INTENT(IN) :: VAI    !total leaf area index + stem area index
  REAL,                            INTENT(IN) :: LAISUN !sunlit leaf area index, one-sided (m2/m2)
  REAL,                            INTENT(IN) :: LAISHA !shaded leaf area index, one-sided (m2/m2)
  REAL,                            INTENT(IN) :: ZLVL   !reference height (m)
  REAL,                            INTENT(IN) :: ZPD    !zero plane displacement (m)
  REAL,                            INTENT(IN) :: Z0M    !roughness length, momentum (m)
  REAL,                            INTENT(IN) :: Z0MG   !roughness length, momentum, ground (m)
  REAL,                            INTENT(IN) :: EMV    !vegetation emissivity
  REAL,                            INTENT(IN) :: EMG    !ground emissivity

  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !soil/snow temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DF     !thermal conductivity of snow/soil (w/m/k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !thinkness of snow/soil layers (m)
  REAL,                            INTENT(IN) :: CANLIQ !intercepted liquid water (mm)
  REAL,                            INTENT(IN) :: CANICE !intercepted ice mass (mm)
  REAL,                            INTENT(IN) :: RSURF  !ground surface resistance (s/m)
!  REAL,                            INTENT(IN) :: GAMMA  !psychrometric constant (pa/K)
!  REAL,                            INTENT(IN) :: LATHEA !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: GAMMAV  !psychrometric constant (pa/K)
  REAL,                            INTENT(IN) :: LATHEAV !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: GAMMAG  !psychrometric constant (pa/K)
  REAL,                            INTENT(IN) :: LATHEAG !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: PARSUN !par absorbed per unit sunlit lai (w/m2)
  REAL,                            INTENT(IN) :: PARSHA !par absorbed per unit shaded lai (w/m2)
  REAL,                            INTENT(IN) :: FOLN   !foliage nitrogen (%)
  REAL,                            INTENT(IN) :: CO2AIR !atmospheric co2 concentration (pa)
  REAL,                            INTENT(IN) :: O2AIR  !atmospheric o2 concentration (pa)
  REAL,                            INTENT(IN) :: IGS    !growing season index (0=off, 1=on)
  REAL,                            INTENT(IN) :: SFCPRS !pressure (pa)
  REAL,                            INTENT(IN) :: BTRAN  !soil water transpiration factor (0 to 1)
  REAL,                            INTENT(IN) :: RHSUR  !raltive humidity in surface soil/snow air space (-)

  REAL                           , INTENT(IN) :: QC     !cloud water mixing ratio
  REAL                           , INTENT(IN) :: PSFC   !pressure at lowest model layer
  REAL                           , INTENT(IN) :: DX     !grid spacing
  REAL                           , INTENT(IN) :: Q2     !mixing ratio (kg/kg)
  REAL                           , INTENT(IN) :: DZ8W   !thickness of lowest layer
  REAL                           , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer
  REAL, INTENT(IN)   :: PAHV  !precipitation advected heat - canopy net IN (W/m2)
  REAL, INTENT(IN)   :: PAHG  !precipitation advected heat - ground net IN (W/m2)

! input/output
  REAL,                         INTENT(INOUT) :: EAH    !canopy air vapor pressure (pa)
  REAL,                         INTENT(INOUT) :: TAH    !canopy air temperature (k)
  REAL,                         INTENT(INOUT) :: TV     !vegetation temperature (k)
  REAL,                         INTENT(INOUT) :: TG     !ground temperature (k)
  REAL,                         INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL,                         INTENT(INOUT) :: CH     !sensible heat exchange coefficient

! output
! -FSA + FIRA + FSH + (FCEV + FCTR + FGEV) + FCST + SSOIL = 0
  REAL,                           INTENT(OUT) :: TAUXV  !wind stress: e-w (n/m2)
  REAL,                           INTENT(OUT) :: TAUYV  !wind stress: n-s (n/m2)
  REAL,                           INTENT(OUT) :: IRC    !net longwave radiation (w/m2) [+= to atm]
  REAL,                           INTENT(OUT) :: SHC    !sensible heat flux (w/m2)     [+= to atm]
  REAL,                           INTENT(OUT) :: EVC    !evaporation heat flux (w/m2)  [+= to atm]
  REAL,                           INTENT(OUT) :: IRG    !net longwave radiation (w/m2) [+= to atm]
  REAL,                           INTENT(OUT) :: SHG    !sensible heat flux (w/m2)     [+= to atm]
  REAL,                           INTENT(OUT) :: EVG    !evaporation heat flux (w/m2)  [+= to atm]
  REAL,                           INTENT(OUT) :: TR     !transpiration heat flux (w/m2)[+= to atm]
  REAL,                           INTENT(OUT) :: GH     !ground heat (w/m2) [+ = to soil]
  REAL,                           INTENT(OUT) :: T2MV   !2 m height air temperature (k)
  REAL,                           INTENT(OUT) :: PSNSUN !sunlit leaf photosynthesis (umolco2/m2/s)
  REAL,                           INTENT(OUT) :: PSNSHA !shaded leaf photosynthesis (umolco2/m2/s)
  REAL,                           INTENT(OUT) :: CHLEAF !leaf exchange coefficient
  REAL,                           INTENT(OUT) :: CHUC   !under canopy exchange coefficient

  REAL,                           INTENT(OUT) :: Q2V
  REAL :: CAH    !sensible heat conductance, canopy air to ZLVL air (m/s)
  REAL :: U10V    !10 m wind speed in eastward dir (m/s) 
  REAL :: V10V    !10 m wind speed in eastward dir (m/s) 
  REAL :: WSPD

! ------------------------ local variables ----------------------------------------------------
  REAL :: CW           !water vapor exchange coefficient
  REAL :: FV           !friction velocity (m/s)
  REAL :: WSTAR        !friction velocity n vertical direction (m/s) (only for SFCDIF2)
  REAL :: Z0H          !roughness length, sensible heat (m)
  REAL :: Z0HG         !roughness length, sensible heat (m)
  REAL :: RB           !bulk leaf boundary layer resistance (s/m)
  REAL :: RAMC         !aerodynamic resistance for momentum (s/m)
  REAL :: RAHC         !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWC         !aerodynamic resistance for water vapor (s/m)
  REAL :: RAMG         !aerodynamic resistance for momentum (s/m)
  REAL :: RAHG         !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWG         !aerodynamic resistance for water vapor (s/m)

  REAL, INTENT(OUT) :: RSSUN        !sunlit leaf stomatal resistance (s/m)
  REAL, INTENT(OUT) :: RSSHA        !shaded leaf stomatal resistance (s/m)

  REAL :: MOL          !Monin-Obukhov length (m)
  REAL :: DTV          !change in tv, last iteration (k)
  REAL :: DTG          !change in tg, last iteration (k)

  REAL :: AIR,CIR      !coefficients for ir as function of ts**4
  REAL :: CSH          !coefficients for sh as function of ts
  REAL :: CEV          !coefficients for ev as function of esat[ts]
  REAL :: CGH          !coefficients for st as function of ts
  REAL :: ATR,CTR      !coefficients for tr as function of esat[ts]
  REAL :: ATA,BTA      !coefficients for tah as function of ts
  REAL :: AEA,BEA      !coefficients for eah as function of esat[ts]

  REAL :: ESTV         !saturation vapor pressure at tv (pa)
  REAL :: ESTG         !saturation vapor pressure at tg (pa)
  REAL :: DESTV        !d(es)/dt at ts (pa/k)
  REAL :: DESTG        !d(es)/dt at tg (pa/k)
  REAL :: ESATW        !es for water
  REAL :: ESATI        !es for ice
  REAL :: DSATW        !d(es)/dt at tg (pa/k) for water
  REAL :: DSATI        !d(es)/dt at tg (pa/k) for ice

  REAL :: FM           !momentum stability correction, weighted by prior iters
  REAL :: FH           !sen heat stability correction, weighted by prior iters
  REAL :: FHG          !sen heat stability correction, ground
  REAL :: HCAN         !canopy height (m) [note: hcan >= z0mg]

  REAL :: A            !temporary calculation
  REAL :: B            !temporary calculation
  REAL :: CVH          !sensible heat conductance, leaf surface to canopy air (m/s)
  REAL :: CAW          !latent heat conductance, canopy air ZLVL air (m/s)
  REAL :: CTW          !transpiration conductance, leaf to canopy air (m/s)
  REAL :: CEW          !evaporation conductance, leaf to canopy air (m/s)
  REAL :: CGW          !latent heat conductance, ground to canopy air (m/s)
  REAL :: COND         !sum of conductances (s/m)
  REAL :: UC           !wind speed at top of canopy (m/s)
  REAL :: KH           !turbulent transfer coefficient, sensible heat, (m2/s)
  REAL :: H            !temporary sensible heat flux (w/m2)
  REAL :: HG           !temporary sensible heat flux (w/m2)
  REAL :: MOZ          !Monin-Obukhov stability parameter
  REAL :: MOZG         !Monin-Obukhov stability parameter
  REAL :: MOZOLD       !Monin-Obukhov stability parameter from prior iteration
  REAL :: FM2          !Monin-Obukhov momentum adjustment at 2m
  REAL :: FH2          !Monin-Obukhov heat adjustment at 2m
  REAL :: CH2          !Surface exchange at 2m
  REAL :: THSTAR          !Surface exchange at 2m

  REAL :: THVAIR
  REAL :: THAH 
  REAL :: RAHC2        !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWC2        !aerodynamic resistance for water vapor (s/m)
  REAL, INTENT(OUT):: CAH2         !sensible heat conductance for diagnostics
  REAL :: CH2V         !exchange coefficient for 2m over vegetation. 
  REAL :: CQ2V         !exchange coefficient for 2m over vegetation. 
  REAL :: EAH2         !2m vapor pressure over canopy
  REAL :: QFX        !moisture flux
  REAL :: E1           


  REAL :: VAIE         !total leaf area index + stem area index,effective
  REAL :: LAISUNE      !sunlit leaf area index, one-sided (m2/m2),effective
  REAL :: LAISHAE      !shaded leaf area index, one-sided (m2/m2),effective

  INTEGER :: K         !index
  INTEGER :: ITER      !iteration index

!jref - NITERC test from 5 to 20  
  INTEGER, PARAMETER :: NITERC = 20   !number of iterations for surface temperature
!jref - NITERG test from 3-5
  INTEGER, PARAMETER :: NITERG = 5   !number of iterations for ground temperature
  INTEGER :: MOZSGN    !number of times MOZ changes sign
  REAL    :: MPE       !prevents overflow error if division by zero

  INTEGER :: LITER     !Last iteration

  REAL, INTENT(IN)                    :: JULIAN, SWDOWN, PRCP, FB
  REAL, INTENT(INOUT)                 :: FSR
  REAL,DIMENSION(1:60), INTENT(INOUT) :: GECROS1D 
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SH2O    !soil liquid water
  
  REAL :: ROOTD, WUL, WLL, Thickness, TLAIE, GLAIE, TLAI, GLAI, FRSU
  INTEGER :: NROOT, J


  REAL :: T, TDC       !Kelvin to degree Celsius with limit -50 to +50

  character(len=80) ::  message

  TDC(T)   = MIN( 50., MAX(-50.,(T-TFRZ)) )
! ---------------------------------------------------------------------------------------------

        MPE = 1E-6
        LITER = 0
        FV = 0.1

! ---------------------------------------------------------------------------------------------
! initialization variables that do not depend on stability iteration
! ---------------------------------------------------------------------------------------------
        DTV = 0.
        DTG = 0.
        MOZ    = 0.
        MOZSGN = 0
        MOZOLD = 0.
        FH2    = 0.
        HG     = 0.
        H      = 0.
        QFX    = 0.

! limit LAI

        VAIE    = MIN(6.,VAI   )
        LAISUNE = MIN(6.,LAISUN)
        LAISHAE = MIN(6.,LAISHA)

! saturation vapor pressure at ground temperature

        T = TDC(TG)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
           ESTG = ESATW
        ELSE
           ESTG = ESATI
        END IF

!jref - consistent surface specific humidity for sfcdif3 and sfcdif4

        QSFC = 0.622*EAIR/(PSFC-0.378*EAIR)  

! canopy height

        HCAN = parameters%HVT
        UC = UR*LOG(HCAN/Z0M)/LOG(ZLVL/Z0M)
        UC = UR*LOG((HCAN-ZPD+Z0M)/Z0M)/LOG(ZLVL/Z0M)   ! MB: add ZPD v3.7
        IF((HCAN-ZPD) <= 0.) THEN
          WRITE(message,*) "CRITICAL PROBLEM: HCAN <= ZPD"
          call wrf_message ( message )
          WRITE(message,*) 'i,j point=',ILOC, JLOC
          call wrf_message ( message )
          WRITE(message,*) 'HCAN  =',HCAN
          call wrf_message ( message )
          WRITE(message,*) 'ZPD   =',ZPD
          call wrf_message ( message )
          write (message, *) 'SNOWH =',SNOWH
          call wrf_message ( message )
          call wrf_error_fatal ( "CRITICAL PROBLEM IN MODULE_SF_NOAHMPLSM:VEGEFLUX" )
        END IF

! prepare for longwave rad.

        AIR = -EMV*(1.+(1.-EMV)*(1.-EMG))*LWDN - EMV*EMG*SB*TG**4  
        CIR = (2.-EMV*(1.-EMG))*EMV*SB
! ---------------------------------------------------------------------------------------------
      loop1: DO ITER = 1, NITERC    !  begin stability iteration

       IF(ITER == 1) THEN
            Z0H  = Z0M  
            Z0HG = Z0MG
       ELSE
            Z0H  = Z0M    !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0M))
            Z0HG = Z0MG   !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0MG))
       END IF

! aerodyn resistances between heights zlvl and d+z0v

       IF(OPT_SFC == 1) THEN
          CALL SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
                       ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
                       MPE    ,ILOC   ,JLOC   ,                 & !in
                       MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
                       CM     ,CH     ,FV     ,CH2     )          !out
       ENDIF
     
       IF(OPT_SFC == 2) THEN
          CALL SFCDIF2(parameters,ITER   ,Z0M    ,TAH    ,THAIR  ,UR     , & !in
                       ZLVL   ,ILOC   ,JLOC   ,         & !in
                       CM     ,CH     ,MOZ    ,WSTAR  ,         & !in
                       FV     )                                   !out
          ! Undo the multiplication by windspeed that SFCDIF2 
          ! applies to exchange coefficients CH and CM:
          CH = CH / UR
          CM = CM / UR
       ENDIF

       RAMC = MAX(1.,1./(CM*UR))
       RAHC = MAX(1.,1./(CH*UR))
       RAWC = RAHC

! aerodyn resistance between heights z0g and d+z0v, RAG, and leaf
! boundary layer resistance, RB
       
       CALL RAGRB(parameters,ITER   ,VAIE   ,RHOAIR ,HG     ,TAH    , & !in
                  ZPD    ,Z0MG   ,Z0HG   ,HCAN   ,UC     , & !in
                  Z0H    ,FV     ,CWP    ,VEGTYP ,MPE    , & !in
                  TV     ,MOZG   ,FHG    ,ILOC   ,JLOC   , & !inout
                  RAMG   ,RAHG   ,RAWG   ,RB     )           !out

! es and d(es)/dt evaluated at tv

       T = TDC(TV)
       CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
       IF (T .GT. 0.) THEN
          ESTV  = ESATW
          DESTV = DSATW
       ELSE
          ESTV  = ESATI
          DESTV = DSATI
       END IF

! stomatal resistance
        
     IF(ITER == 1) THEN
        IF (OPT_CRS == 1) then  ! Ball-Berry
         CALL STOMATA (parameters,VEGTYP,MPE   ,PARSUN ,FOLN  ,ILOC  , JLOC , & !in       
                       TV    ,ESTV  ,EAH    ,SFCTMP,SFCPRS, & !in
                       O2AIR ,CO2AIR,IGS    ,BTRAN ,RB    , & !in
                       RSSUN ,PSNSUN)                         !out

         CALL STOMATA (parameters,VEGTYP,MPE   ,PARSHA ,FOLN  ,ILOC  , JLOC , & !in
                       TV    ,ESTV  ,EAH    ,SFCTMP,SFCPRS, & !in
                       O2AIR ,CO2AIR,IGS    ,BTRAN ,RB    , & !in
                       RSSHA ,PSNSHA)                         !out
        END IF

        IF (OPT_CRS == 2) then  ! Jarvis
         CALL  CANRES (parameters,PARSUN,TV    ,BTRAN ,EAH    ,SFCPRS, & !in
                       RSSUN ,PSNSUN,ILOC  ,JLOC   )          !out

         CALL  CANRES (parameters,PARSHA,TV    ,BTRAN ,EAH    ,SFCPRS, & !in
                       RSSHA ,PSNSHA,ILOC  ,JLOC   )          !out
        END IF

     ! Call Gecros
       IF (opt_crop == 2) then
         IF ((GECROS1D(41).GT.0).and.(GECROS1D(42).LT.0.)) then   !Gecros
           Thickness = 0.
           NROOT = 0
           ROOTD = GECROS1D(33)
           WUL = 0.
           WLL = 0.

           DO J = 1,NSOIL
            Thickness = Thickness + DZSNSO (J)
            if (Thickness.lt.ROOTD/100.) then
              NROOT = NROOT + 1
            endif
           ENDDO
          
           NROOT = NROOT + 1
           NROOT = MAX(1,NROOT)
        
           Thickness = 0.
       
           DO J = 1,NROOT
             Thickness = Thickness + DZSNSO (J) 
             if (Thickness.gt.ROOTD/100.) then
                WUL = WUL + ((ROOTD/100.-Thickness+DZSNSO(J))*1000.*(SH2O(J)-parameters%SMCWLT(J)))
             else
                WUL = WUL + (DZSNSO(J)*1000.*(SH2O(J)-parameters%SMCWLT(J)))
             endif
           ENDDO
       
           DO J = 1,NSOIL
             WLL = WLL + (DZSNSO(J)*1000.*(SH2O(J)-parameters%SMCWLT(J)))
           ENDDO
           WLL = WLL - WUL
         
           CALL gecros (JULIAN, DT, 1, RB, RAHC, RAHG+RSURF, FB, SNOWH        , & !I
               UR, SFCTMP, EAIR, SWDOWN, LWDN, PRCP, WUL, WLL                      , & !I
               parameters%SMCWLT(1), parameters%DLEAF                              , & !I
               GECROS1D                                                        , & !H                                    
               SAV, SAG, FSR, FRSU, RSSUN, RSSHA)                    !O
           
               GLAI = GECROS1D(49)
               TLAI = GECROS1D(50)

               ! effective LAIs
               TLAIE    = MIN(6.,TLAI    / FVEG)
               GLAIE    = MIN(6.,GLAI    / FVEG) 
         ENDIF     
       ENDIF     
       
     END IF

! prepare for sensible heat flux above veg.

        CAH  = 1./RAHC
        CVH  = 2.*VAIE/RB
        CGH  = 1./RAHG
        COND = CAH + CVH + CGH
        ATA  = (SFCTMP*CAH + TG*CGH) / COND
        BTA  = CVH/COND
        CSH  = (1.-BTA)*RHOAIR*CPAIR*CVH

! prepare for latent heat flux above veg.

        CAW  = 1./RAWC
        CEW  = FWET*VAIE/RB

        IF (OPT_CROP /= 2) THEN
          CTW  = (1.-FWET)*(LAISUNE/(RB+RSSUN) + LAISHAE/(RB+RSSHA))
        ELSE
           !RSSUN and RSSHA are in resistance per unit LAI in the Jarvis and Ball-Berry!. RSSUN and RSSHA of Gecros are in s/m
          CTW  = (1.-FWET)*(1./(RB/(FRSU*GLAIE)+RSSUN) + 1./(RB/((1.-FRSU)*GLAIE)+RSSHA)) !transpiration conductance leaf to canopy air
        ENDIF
        CGW  = 1./(RAWG+RSURF)
        COND = CAW + CEW + CTW + CGW
        AEA  = (EAIR*CAW + ESTG*CGW) / COND
        BEA  = (CEW+CTW)/COND
        CEV  = (1.-BEA)*CEW*RHOAIR*CPAIR/GAMMAV   ! Barlage: change to vegetation v3.6
        CTR  = (1.-BEA)*CTW*RHOAIR*CPAIR/GAMMAV

! evaluate surface fluxes with current temperature and solve for dts

        TAH = ATA + BTA*TV               ! canopy air T.
        EAH = AEA + BEA*ESTV             ! canopy air e

        IRC = FVEG*(AIR + CIR*TV**4)
        SHC = FVEG*RHOAIR*CPAIR*CVH * (  TV-TAH)
        EVC = FVEG*RHOAIR*CPAIR*CEW * (ESTV-EAH) / GAMMAV ! Barlage: change to v in v3.6
        TR  = FVEG*RHOAIR*CPAIR*CTW * (ESTV-EAH) / GAMMAV
	IF (TV > TFRZ) THEN
          EVC = MIN(CANLIQ*LATHEAV/DT,EVC)    ! Barlage: add if block for canice in v3.6
	ELSE
          EVC = MIN(CANICE*LATHEAV/DT,EVC)
	END IF

        B   = SAV-IRC-SHC-EVC-TR+PAHV                          !additional w/m2
        A   = FVEG*(4.*CIR*TV**3 + CSH + (CEV+CTR)*DESTV) !volumetric heat capacity
        DTV = B/A

        IRC = IRC + FVEG*4.*CIR*TV**3*DTV
        SHC = SHC + FVEG*CSH*DTV
        EVC = EVC + FVEG*CEV*DESTV*DTV
        TR  = TR  + FVEG*CTR*DESTV*DTV                               

! update vegetation surface temperature
        TV  = TV + DTV
!        TAH = ATA + BTA*TV               ! canopy air T; update here for consistency

! for computing M-O length in the next iteration
        H  = RHOAIR*CPAIR*(TAH - SFCTMP) /RAHC        
        HG = RHOAIR*CPAIR*(TG  - TAH)   /RAHG

! consistent specific humidity from canopy air vapor pressure
        QSFC = (0.622*EAH)/(SFCPRS-0.378*EAH)

        IF (LITER == 1) THEN
           exit loop1 
        ENDIF
        IF (ITER >= 5 .AND. ABS(DTV) <= 0.01 .AND. LITER == 0) THEN
           LITER = 1
        ENDIF

     END DO loop1 ! end stability iteration

! under-canopy fluxes and tg

        AIR = - EMG*(1.-EMV)*LWDN - EMG*EMV*SB*TV**4
        CIR = EMG*SB
        CSH = RHOAIR*CPAIR/RAHG
        CEV = RHOAIR*CPAIR / (GAMMAG*(RAWG+RSURF))  ! Barlage: change to ground v3.6
        CGH = 2.*DF(ISNOW+1)/DZSNSO(ISNOW+1)

     loop2: DO ITER = 1, NITERG

        T = TDC(TG)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
            ESTG  = ESATW
            DESTG = DSATW
        ELSE
            ESTG  = ESATI
            DESTG = DSATI
        END IF

        IRG = CIR*TG**4 + AIR
        SHG = CSH * (TG         - TAH         )
        EVG = CEV * (ESTG*RHSUR - EAH         )
        GH  = CGH * (TG         - STC(ISNOW+1))

        B = SAG-IRG-SHG-EVG-GH+PAHG
        A = 4.*CIR*TG**3+CSH+CEV*DESTG+CGH
        DTG = B/A

        IRG = IRG + 4.*CIR*TG**3*DTG
        SHG = SHG + CSH*DTG
        EVG = EVG + CEV*DESTG*DTG
        GH  = GH  + CGH*DTG
        TG  = TG  + DTG

     END DO loop2
     
!     TAH = (CAH*SFCTMP + CVH*TV + CGH*TG)/(CAH + CVH + CGH)

! if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.

     IF(OPT_STC == 1 .OR. OPT_STC == 3) THEN
     IF (SNOWH > 0.05 .AND. TG > TFRZ) THEN
        IF(OPT_STC == 1) TG  = TFRZ
        IF(OPT_STC == 3) TG  = (1.-FSNO)*TG + FSNO*TFRZ   ! MB: allow TG>0C during melt v3.7
        IRG = CIR*TG**4 - EMG*(1.-EMV)*LWDN - EMG*EMV*SB*TV**4
        SHG = CSH * (TG         - TAH)
        EVG = CEV * (ESTG*RHSUR - EAH)
        GH  = SAG+PAHG - (IRG+SHG+EVG)
     END IF
     END IF

! wind stresses

     TAUXV = -RHOAIR*CM*UR*UU
     TAUYV = -RHOAIR*CM*UR*VV

! consistent vegetation air temperature and vapor pressure since TG is not consistent with the TAH/EAH
! calculation.
!     TAH = SFCTMP + (SHG+SHC)/(RHOAIR*CPAIR*CAH) 
!     TAH = SFCTMP + (SHG*FVEG+SHC)/(RHOAIR*CPAIR*CAH) ! ground flux need fveg
!     EAH = EAIR + (EVC+FVEG*(TR+EVG))/(RHOAIR*CAW*CPAIR/GAMMAG )
!     QFX = (QSFC-QAIR)*RHOAIR*CAW !*CPAIR/GAMMAG

! 2m temperature over vegetation ( corrected for low CQ2V values )
   IF (OPT_SFC == 1 .OR. OPT_SFC == 2) THEN
!      CAH2 = FV*1./VKC*LOG((2.+Z0H)/Z0H)
      CAH2 = FV*VKC/LOG((2.+Z0H)/Z0H)
      CAH2 = FV*VKC/(LOG((2.+Z0H)/Z0H)-FH2)
      CQ2V = CAH2
      IF (CAH2 .LT. 1.E-5 ) THEN
         T2MV = TAH
!         Q2V  = (EAH*0.622/(SFCPRS - 0.378*EAH))
         Q2V  = QSFC
      ELSE
         T2MV = TAH - (SHG+SHC/FVEG)/(RHOAIR*CPAIR) * 1./CAH2
!         Q2V = (EAH*0.622/(SFCPRS - 0.378*EAH))- QFX/(RHOAIR*FV)* 1./VKC * LOG((2.+Z0H)/Z0H)
         Q2V = QSFC - ((EVC+TR)/FVEG+EVG)/(LATHEAV*RHOAIR) * 1./CQ2V
      ENDIF
   ENDIF

! update CH for output
     CH = CAH
     CHLEAF = CVH
     CHUC = 1./RAHG

  END SUBROUTINE VEGE_FLUX

!== begin bare_flux ================================================================================

  SUBROUTINE BARE_FLUX (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,SAG     , & !in
                        LWDN    ,UR      ,UU      ,VV      ,SFCTMP  , & !in
                        THAIR   ,QAIR    ,EAIR    ,RHOAIR  ,SNOWH   , & !in
                        DZSNSO  ,ZLVL    ,ZPD     ,Z0M     ,FSNO    , & !in
                        EMG     ,STC     ,DF      ,RSURF   ,LATHEA  , & !in
                        GAMMA   ,RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHB  , & !in
                        TGB     ,CM      ,CH      ,          & !inout
                        TAUXB   ,TAUYB   ,IRB     ,SHB     ,EVB     , & !out
                        GHB     ,T2MB    ,DX      ,DZ8W    ,IVGTYP  , & !out
                        QC      ,QSFC    ,PSFC    ,                   & !in
                        SFCPRS  ,Q2B     ,EHB2    )                     !in 

! --------------------------------------------------------------------------------------------------
! use newton-raphson iteration to solve ground (tg) temperature
! that balances the surface energy budgets for bare soil fraction.

! bare soil:
! -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  integer                        , INTENT(IN) :: ILOC   !grid index
  integer                        , INTENT(IN) :: JLOC   !grid index
  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers
  INTEGER,                         INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER,                         INTENT(IN) :: ISNOW  !actual no. of snow layers
  REAL,                            INTENT(IN) :: DT     !time step (s)
  REAL,                            INTENT(IN) :: SAG    !solar radiation absorbed by ground (w/m2)
  REAL,                            INTENT(IN) :: LWDN   !atmospheric longwave radiation (w/m2)
  REAL,                            INTENT(IN) :: UR     !wind speed at height zlvl (m/s)
  REAL,                            INTENT(IN) :: UU     !wind speed in eastward dir (m/s)
  REAL,                            INTENT(IN) :: VV     !wind speed in northward dir (m/s)
  REAL,                            INTENT(IN) :: SFCTMP !air temperature at reference height (k)
  REAL,                            INTENT(IN) :: THAIR  !potential temperature at height zlvl (k)
  REAL,                            INTENT(IN) :: QAIR   !specific humidity at height zlvl (kg/kg)
  REAL,                            INTENT(IN) :: EAIR   !vapor pressure air at height (pa)
  REAL,                            INTENT(IN) :: RHOAIR !density air (kg/m3)
  REAL,                            INTENT(IN) :: SNOWH  !actual snow depth [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !thickness of snow/soil layers (m)
  REAL,                            INTENT(IN) :: ZLVL   !reference height (m)
  REAL,                            INTENT(IN) :: ZPD    !zero plane displacement (m)
  REAL,                            INTENT(IN) :: Z0M    !roughness length, momentum, ground (m)
  REAL,                            INTENT(IN) :: EMG    !ground emissivity
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !soil/snow temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DF     !thermal conductivity of snow/soil (w/m/k)
  REAL,                            INTENT(IN) :: RSURF  !ground surface resistance (s/m)
  REAL,                            INTENT(IN) :: LATHEA !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: GAMMA  !psychrometric constant (pa/k)
  REAL,                            INTENT(IN) :: RHSUR  !raltive humidity in surface soil/snow air space (-)
  REAL,                            INTENT(IN) :: FSNO     !snow fraction

!jref:start; in 
  INTEGER                        , INTENT(IN) :: IVGTYP
  REAL                           , INTENT(IN) :: QC     !cloud water mixing ratio
  REAL                           , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer
  REAL                           , INTENT(IN) :: PSFC   !pressure at lowest model layer
  REAL                           , INTENT(IN) :: SFCPRS !pressure at lowest model layer
  REAL                           , INTENT(IN) :: DX     !horisontal grid spacing
  REAL                           , INTENT(IN) :: Q2     !mixing ratio (kg/kg)
  REAL                           , INTENT(IN) :: DZ8W   !thickness of lowest layer
!jref:end
  REAL, INTENT(IN)   :: PAHB  !precipitation advected heat - ground net IN (W/m2)

! input/output
  REAL,                         INTENT(INOUT) :: TGB    !ground temperature (k)
  REAL,                         INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL,                         INTENT(INOUT) :: CH     !sensible heat exchange coefficient

! output
! -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0

  REAL,                           INTENT(OUT) :: TAUXB  !wind stress: e-w (n/m2)
  REAL,                           INTENT(OUT) :: TAUYB  !wind stress: n-s (n/m2)
  REAL,                           INTENT(OUT) :: IRB    !net longwave rad (w/m2)   [+ to atm]
  REAL,                           INTENT(OUT) :: SHB    !sensible heat flux (w/m2) [+ to atm]
  REAL,                           INTENT(OUT) :: EVB    !latent heat flux (w/m2)   [+ to atm]
  REAL,                           INTENT(OUT) :: GHB    !ground heat flux (w/m2)  [+ to soil]
  REAL,                           INTENT(OUT) :: T2MB   !2 m height air temperature (k)
!jref:start
  REAL,                           INTENT(OUT) :: Q2B    !bare ground heat conductance
  REAL :: EHB    !bare ground heat conductance
  REAL :: U10B    !10 m wind speed in eastward dir (m/s)
  REAL :: V10B    !10 m wind speed in eastward dir (m/s)
  REAL :: WSPD
!jref:end

! local variables 

  REAL :: TAUX       !wind stress: e-w (n/m2)
  REAL :: TAUY       !wind stress: n-s (n/m2)
  REAL :: FIRA       !total net longwave rad (w/m2)      [+ to atm]
  REAL :: FSH        !total sensible heat flux (w/m2)    [+ to atm]
  REAL :: FGEV       !ground evaporation heat flux (w/m2)[+ to atm]
  REAL :: SSOIL      !soil heat flux (w/m2)             [+ to soil]
  REAL :: FIRE       !emitted ir (w/m2)
  REAL :: TRAD       !radiative temperature (k)
  REAL :: TAH        !"surface" temperature at height z0h+zpd (k)

  REAL :: CW         !water vapor exchange coefficient
  REAL :: FV         !friction velocity (m/s)
  REAL :: WSTAR      !friction velocity n vertical direction (m/s) (only for SFCDIF2)
  REAL :: Z0H        !roughness length, sensible heat, ground (m)
  REAL :: RB         !bulk leaf boundary layer resistance (s/m)
  REAL :: RAMB       !aerodynamic resistance for momentum (s/m)
  REAL :: RAHB       !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWB       !aerodynamic resistance for water vapor (s/m)
  REAL :: MOL        !Monin-Obukhov length (m)
  REAL :: DTG        !change in tg, last iteration (k)

  REAL :: CIR        !coefficients for ir as function of ts**4
  REAL :: CSH        !coefficients for sh as function of ts
  REAL :: CEV        !coefficients for ev as function of esat[ts]
  REAL :: CGH        !coefficients for st as function of ts

!jref:start
  REAL :: RAHB2      !aerodynamic resistance for sensible heat 2m (s/m)
  REAL :: RAWB2      !aerodynamic resistance for water vapor 2m (s/m)
  REAL,INTENT(OUT) :: EHB2       !sensible heat conductance for diagnostics
  REAL :: CH2B       !exchange coefficient for 2m temp.
  REAL :: CQ2B       !exchange coefficient for 2m temp.
  REAL :: THVAIR     !virtual potential air temp
  REAL :: THGH       !potential ground temp
  REAL :: EMB        !momentum conductance
  REAL :: QFX        !moisture flux
  REAL :: ESTG2      !saturation vapor pressure at 2m (pa)
  INTEGER :: VEGTYP     !vegetation type set to isbarren
  REAL :: E1
!jref:end

  REAL :: ESTG       !saturation vapor pressure at tg (pa)
  REAL :: DESTG      !d(es)/dt at tg (pa/K)
  REAL :: ESATW      !es for water
  REAL :: ESATI      !es for ice
  REAL :: DSATW      !d(es)/dt at tg (pa/K) for water
  REAL :: DSATI      !d(es)/dt at tg (pa/K) for ice

  REAL :: A          !temporary calculation
  REAL :: B          !temporary calculation
  REAL :: H          !temporary sensible heat flux (w/m2)
  REAL :: MOZ        !Monin-Obukhov stability parameter
  REAL :: MOZOLD     !Monin-Obukhov stability parameter from prior iteration
  REAL :: FM         !momentum stability correction, weighted by prior iters
  REAL :: FH         !sen heat stability correction, weighted by prior iters
  INTEGER :: MOZSGN  !number of times MOZ changes sign
  REAL :: FM2          !Monin-Obukhov momentum adjustment at 2m
  REAL :: FH2          !Monin-Obukhov heat adjustment at 2m
  REAL :: CH2          !Surface exchange at 2m

  INTEGER :: ITER    !iteration index
  INTEGER :: NITERB  !number of iterations for surface temperature
  REAL    :: MPE     !prevents overflow error if division by zero
!jref:start
!  DATA NITERB /3/
  DATA NITERB /5/
  SAVE NITERB
  REAL :: T, TDC     !Kelvin to degree Celsius with limit -50 to +50
  TDC(T)   = MIN( 50., MAX(-50.,(T-TFRZ)) )

! -----------------------------------------------------------------
! initialization variables that do not depend on stability iteration
! -----------------------------------------------------------------
        MPE = 1E-6
        DTG = 0.
        MOZ    = 0.
        MOZSGN = 0
        MOZOLD = 0.
        FH2    = 0.
        H      = 0.
        QFX    = 0.
        FV     = 0.1

        CIR = EMG*SB
        CGH = 2.*DF(ISNOW+1)/DZSNSO(ISNOW+1)

! -----------------------------------------------------------------
      loop3: DO ITER = 1, NITERB  ! begin stability iteration

        IF(ITER == 1) THEN
            Z0H = Z0M 
        ELSE
            Z0H = Z0M !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0M))
        END IF

        IF(OPT_SFC == 1) THEN
          CALL SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
                       ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
                       MPE    ,ILOC   ,JLOC   ,                 & !in
                       MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
                       CM     ,CH     ,FV     ,CH2     )          !out
        ENDIF

        IF(OPT_SFC == 2) THEN
          CALL SFCDIF2(parameters,ITER   ,Z0M    ,TGB    ,THAIR  ,UR     , & !in
                       ZLVL   ,ILOC   ,JLOC   ,         & !in
                       CM     ,CH     ,MOZ    ,WSTAR  ,         & !in
                       FV     )                                   !out
          ! Undo the multiplication by windspeed that SFCDIF2 
          ! applies to exchange coefficients CH and CM:
          CH = CH / UR
          CM = CM / UR
          IF(SNOWH > 0.) THEN
             CM = MIN(0.01,CM)   ! CM & CH are too large, causing
             CH = MIN(0.01,CH)   ! computational instability
          END IF

        ENDIF

        RAMB = MAX(1.,1./(CM*UR))
        RAHB = MAX(1.,1./(CH*UR))
        RAWB = RAHB

!jref - variables for diagnostics         
        EMB = 1./RAMB
        EHB = 1./RAHB

! es and d(es)/dt evaluated at tg

        T = TDC(TGB)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
            ESTG  = ESATW
            DESTG = DSATW
        ELSE
            ESTG  = ESATI
            DESTG = DSATI
        END IF

        CSH = RHOAIR*CPAIR/RAHB
        CEV = RHOAIR*CPAIR/GAMMA/(RSURF+RAWB)

! surface fluxes and dtg

        IRB   = CIR * TGB**4 - EMG*LWDN
        SHB   = CSH * (TGB        - SFCTMP      )
        EVB   = CEV * (ESTG*RHSUR - EAIR        )
        GHB   = CGH * (TGB        - STC(ISNOW+1))

        B     = SAG-IRB-SHB-EVB-GHB+PAHB
        A     = 4.*CIR*TGB**3 + CSH + CEV*DESTG + CGH
        DTG   = B/A

        IRB = IRB + 4.*CIR*TGB**3*DTG
        SHB = SHB + CSH*DTG
        EVB = EVB + CEV*DESTG*DTG
        GHB = GHB + CGH*DTG

! update ground surface temperature
        TGB = TGB + DTG

! for M-O length
        H = CSH * (TGB - SFCTMP)

        T = TDC(TGB)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
            ESTG  = ESATW
        ELSE
            ESTG  = ESATI
        END IF
        QSFC = 0.622*(ESTG*RHSUR)/(PSFC-0.378*(ESTG*RHSUR))

        QFX = (QSFC-QAIR)*CEV*GAMMA/CPAIR

     END DO loop3 ! end stability iteration
! -----------------------------------------------------------------

! if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.

     IF(OPT_STC == 1 .OR. OPT_STC == 3) THEN
     IF (SNOWH > 0.05 .AND. TGB > TFRZ) THEN
          IF(OPT_STC == 1) TGB = TFRZ
          IF(OPT_STC == 3) TGB  = (1.-FSNO)*TGB + FSNO*TFRZ  ! MB: allow TG>0C during melt v3.7
          IRB = CIR * TGB**4 - EMG*LWDN
          SHB = CSH * (TGB        - SFCTMP)
          EVB = CEV * (ESTG*RHSUR - EAIR )          !ESTG reevaluate ?
          GHB = SAG+PAHB - (IRB+SHB+EVB)
     END IF
     END IF

! wind stresses
         
     TAUXB = -RHOAIR*CM*UR*UU
     TAUYB = -RHOAIR*CM*UR*VV

!jref:start; errors in original equation corrected.
! 2m air temperature
     IF(OPT_SFC == 1 .OR. OPT_SFC ==2) THEN
       EHB2  = FV*VKC/LOG((2.+Z0H)/Z0H)
       EHB2  = FV*VKC/(LOG((2.+Z0H)/Z0H)-FH2)
       CQ2B  = EHB2
       IF (EHB2.lt.1.E-5 ) THEN
         T2MB  = TGB
         Q2B   = QSFC
       ELSE
         T2MB  = TGB - SHB/(RHOAIR*CPAIR) * 1./EHB2
         Q2B   = QSFC - EVB/(LATHEA*RHOAIR)*(1./CQ2B + RSURF)
       ENDIF
       IF (parameters%urban_flag) Q2B = QSFC
     END IF

! update CH 
     CH = EHB

  END SUBROUTINE BARE_FLUX

!== begin ragrb ====================================================================================

  SUBROUTINE RAGRB(parameters,ITER   ,VAI    ,RHOAIR ,HG     ,TAH    , & !in
                   ZPD    ,Z0MG   ,Z0HG   ,HCAN   ,UC     , & !in
                   Z0H    ,FV     ,CWP    ,VEGTYP ,MPE    , & !in
                   TV     ,MOZG   ,FHG    ,ILOC   ,JLOC   , & !inout
                   RAMG   ,RAHG   ,RAWG   ,RB     )           !out
! --------------------------------------------------------------------------------------------------
! compute under-canopy aerodynamic resistance RAG and leaf boundary layer
! resistance RB
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,              INTENT(IN) :: ILOC   !grid index
  INTEGER,              INTENT(IN) :: JLOC   !grid index
  INTEGER,              INTENT(IN) :: ITER   !iteration index
  INTEGER,              INTENT(IN) :: VEGTYP !vegetation physiology type
  REAL,                 INTENT(IN) :: VAI    !total LAI + stem area index, one sided
  REAL,                 INTENT(IN) :: RHOAIR !density air (kg/m3)
  REAL,                 INTENT(IN) :: HG     !ground sensible heat flux (w/m2)
  REAL,                 INTENT(IN) :: TV     !vegetation temperature (k)
  REAL,                 INTENT(IN) :: TAH    !air temperature at height z0h+zpd (k)
  REAL,                 INTENT(IN) :: ZPD    !zero plane displacement (m)
  REAL,                 INTENT(IN) :: Z0MG   !roughness length, momentum, ground (m)
  REAL,                 INTENT(IN) :: HCAN   !canopy height (m) [note: hcan >= z0mg]
  REAL,                 INTENT(IN) :: UC     !wind speed at top of canopy (m/s)
  REAL,                 INTENT(IN) :: Z0H    !roughness length, sensible heat (m)
  REAL,                 INTENT(IN) :: Z0HG   !roughness length, sensible heat, ground (m)
  REAL,                 INTENT(IN) :: FV     !friction velocity (m/s)
  REAL,                 INTENT(IN) :: CWP    !canopy wind parameter
  REAL,                 INTENT(IN) :: MPE    !prevents overflow error if division by zero

! in & out

  REAL,              INTENT(INOUT) :: MOZG   !Monin-Obukhov stability parameter
  REAL,              INTENT(INOUT) :: FHG    !stability correction

! outputs
  REAL                             :: RAMG   !aerodynamic resistance for momentum (s/m)
  REAL                             :: RAHG   !aerodynamic resistance for sensible heat (s/m)
  REAL                             :: RAWG   !aerodynamic resistance for water vapor (s/m)
  REAL                             :: RB     !bulk leaf boundary layer resistance (s/m)


  REAL :: KH           !turbulent transfer coefficient, sensible heat, (m2/s)
  REAL :: TMP1         !temporary calculation
  REAL :: TMP2         !temporary calculation
  REAL :: TMPRAH2      !temporary calculation for aerodynamic resistances
  REAL :: TMPRB        !temporary calculation for rb
  real :: MOLG,FHGNEW,CWPC
! --------------------------------------------------------------------------------------------------
! stability correction to below canopy resistance

       MOZG = 0.
       MOLG = 0.

       IF(ITER > 1) THEN
        TMP1 = VKC * (GRAV/TAH) * HG/(RHOAIR*CPAIR)
        IF (ABS(TMP1) .LE. MPE) TMP1 = MPE
        MOLG = -1. * FV**3 / TMP1
        MOZG = MIN( (ZPD-Z0MG)/MOLG, 1.)
       END IF

       IF (MOZG < 0.) THEN
          FHGNEW  = (1. - 15.*MOZG)**(-0.25)
       ELSE
          FHGNEW  = 1.+ 4.7*MOZG
       ENDIF

       IF (ITER == 1) THEN
          FHG = FHGNEW
       ELSE
          FHG = 0.5 * (FHG+FHGNEW)
       ENDIF

       CWPC = (CWP * VAI * HCAN * FHG)**0.5
!       CWPC = (CWP*FHG)**0.5

       TMP1 = EXP( -CWPC*Z0HG/HCAN )
       TMP2 = EXP( -CWPC*(Z0H+ZPD)/HCAN )
       TMPRAH2 = HCAN*EXP(CWPC) / CWPC * (TMP1-TMP2)

! aerodynamic resistances raw and rah between heights zpd+z0h and z0hg.

       KH  = MAX ( VKC*FV*(HCAN-ZPD), MPE )
       RAMG = 0.
       RAHG = TMPRAH2 / KH
       RAWG = RAHG

! leaf boundary layer resistance

       TMPRB  = CWPC*50. / (1. - EXP(-CWPC/2.))
       RB     = TMPRB * SQRT(parameters%DLEAF/UC)
       RB     = MIN(MAX(RB, 5.0),50.0) ! limit RB to 5-50, typically RB<50

  END SUBROUTINE RAGRB

!== begin sfcdif1 ==================================================================================

  SUBROUTINE SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
       &             ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
       &             MPE    ,ILOC   ,JLOC   ,                 & !in
       &             MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
       &             CM     ,CH     ,FV     ,CH2     )          !out
! -------------------------------------------------------------------------------------------------
! computing surface drag coefficient CM for momentum and CH for heat
! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE
! -------------------------------------------------------------------------------------------------
! inputs
    
  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,              INTENT(IN) :: ILOC   !grid index
    INTEGER,              INTENT(IN) :: JLOC   !grid index
    INTEGER,              INTENT(IN) :: ITER   !iteration index
    REAL,                 INTENT(IN) :: SFCTMP !temperature at reference height (k)
    REAL,                 INTENT(IN) :: RHOAIR !density air (kg/m**3)
    REAL,                 INTENT(IN) :: H      !sensible heat flux (w/m2) [+ to atm]
    REAL,                 INTENT(IN) :: QAIR   !specific humidity at reference height (kg/kg)
    REAL,                 INTENT(IN) :: ZLVL   !reference height  (m)
    REAL,                 INTENT(IN) :: ZPD    !zero plane displacement (m)
    REAL,                 INTENT(IN) :: Z0H    !roughness length, sensible heat, ground (m)
    REAL,                 INTENT(IN) :: Z0M    !roughness length, momentum, ground (m)
    REAL,                 INTENT(IN) :: UR     !wind speed (m/s)
    REAL,                 INTENT(IN) :: MPE    !prevents overflow error if division by zero
! in & out

    INTEGER,           INTENT(INOUT) :: MOZSGN !number of times moz changes sign
    REAL,              INTENT(INOUT) :: MOZ    !Monin-Obukhov stability (z/L)
    REAL,              INTENT(INOUT) :: FM     !momentum stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FH     !sen heat stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FM2    !sen heat stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FH2    !sen heat stability correction, weighted by prior iters

! outputs

    REAL,                INTENT(OUT) :: CM     !drag coefficient for momentum
    REAL,                INTENT(OUT) :: CH     !drag coefficient for heat
    REAL,                INTENT(OUT) :: FV     !friction velocity (m/s)
    REAL,                INTENT(OUT) :: CH2    !drag coefficient for heat

! locals
    REAL    :: MOL                      !Monin-Obukhov length (m)
    REAL    :: TMPCM                    !temporary calculation for CM
    REAL    :: TMPCH                    !temporary calculation for CH
    REAL    :: FMNEW                    !stability correction factor, momentum, for current moz
    REAL    :: FHNEW                    !stability correction factor, sen heat, for current moz
    REAL    :: MOZOLD                   !Monin-Obukhov stability parameter from prior iteration
    REAL    :: TMP1,TMP2,TMP3,TMP4,TMP5 !temporary calculation
    REAL    :: TVIR                     !temporary virtual temperature (k)
    REAL    :: MOZ2                     !2/L
    REAL    :: TMPCM2                   !temporary calculation for CM2
    REAL    :: TMPCH2                   !temporary calculation for CH2
    REAL    :: FM2NEW                   !stability correction factor, momentum, for current moz
    REAL    :: FH2NEW                   !stability correction factor, sen heat, for current moz
    REAL    :: TMP12,TMP22,TMP32        !temporary calculation

    REAL    :: CMFM, CHFH, CM2FM2, CH2FH2
! -------------------------------------------------------------------------------------------------
! Monin-Obukhov stability parameter moz for next iteration

    MOZOLD = MOZ
  
    IF(ZLVL <= ZPD) THEN
       write(*,*) 'WARNING: critical problem: ZLVL <= ZPD; model stops'
       call wrf_error_fatal("STOP in Noah-MP")
    ENDIF

    TMPCM = LOG((ZLVL-ZPD) / Z0M)
    TMPCH = LOG((ZLVL-ZPD) / Z0H)
    TMPCM2 = LOG((2.0 + Z0M) / Z0M)
    TMPCH2 = LOG((2.0 + Z0H) / Z0H)

    IF(ITER == 1) THEN
       FV   = 0.0
       MOZ  = 0.0
       MOL  = 0.0
       MOZ2 = 0.0
    ELSE
       TVIR = (1. + 0.61*QAIR) * SFCTMP
       TMP1 = VKC * (GRAV/TVIR) * H/(RHOAIR*CPAIR)
       IF (ABS(TMP1) .LE. MPE) TMP1 = MPE
       MOL  = -1. * FV**3 / TMP1
       MOZ  = MIN( (ZLVL-ZPD)/MOL, 1.)
       MOZ2  = MIN( (2.0 + Z0H)/MOL, 1.)
    ENDIF

! accumulate number of times moz changes sign.

    IF (MOZOLD*MOZ .LT. 0.) MOZSGN = MOZSGN+1
    IF (MOZSGN .GE. 2) THEN
       MOZ = 0.
       FM = 0.
       FH = 0.
       MOZ2 = 0.
       FM2 = 0.
       FH2 = 0.
    ENDIF

! evaluate stability-dependent variables using moz from prior iteration
    IF (MOZ .LT. 0.) THEN
       TMP1 = (1. - 16.*MOZ)**0.25
       TMP2 = LOG((1.+TMP1*TMP1)/2.)
       TMP3 = LOG((1.+TMP1)/2.)
       FMNEW = 2.*TMP3 + TMP2 - 2.*ATAN(TMP1) + 1.5707963
       FHNEW = 2*TMP2

! 2-meter
       TMP12 = (1. - 16.*MOZ2)**0.25
       TMP22 = LOG((1.+TMP12*TMP12)/2.)
       TMP32 = LOG((1.+TMP12)/2.)
       FM2NEW = 2.*TMP32 + TMP22 - 2.*ATAN(TMP12) + 1.5707963
       FH2NEW = 2*TMP22
    ELSE
       FMNEW = -5.*MOZ
       FHNEW = FMNEW
       FM2NEW = -5.*MOZ2
       FH2NEW = FM2NEW
    ENDIF

! except for first iteration, weight stability factors for previous
! iteration to help avoid flip-flops from one iteration to the next

    IF (ITER == 1) THEN
       FM = FMNEW
       FH = FHNEW
       FM2 = FM2NEW
       FH2 = FH2NEW
    ELSE
       FM = 0.5 * (FM+FMNEW)
       FH = 0.5 * (FH+FHNEW)
       FM2 = 0.5 * (FM2+FM2NEW)
       FH2 = 0.5 * (FH2+FH2NEW)
    ENDIF

! exchange coefficients

    FH = MIN(FH,0.9*TMPCH)
    FM = MIN(FM,0.9*TMPCM)
    FH2 = MIN(FH2,0.9*TMPCH2)
    FM2 = MIN(FM2,0.9*TMPCM2)

    CMFM = TMPCM-FM
    CHFH = TMPCH-FH
    CM2FM2 = TMPCM2-FM2
    CH2FH2 = TMPCH2-FH2
    IF(ABS(CMFM) <= MPE) CMFM = MPE
    IF(ABS(CHFH) <= MPE) CHFH = MPE
    IF(ABS(CM2FM2) <= MPE) CM2FM2 = MPE
    IF(ABS(CH2FH2) <= MPE) CH2FH2 = MPE
    CM  = VKC*VKC/(CMFM*CMFM)
    CH  = VKC*VKC/(CMFM*CHFH)
    CH2  = VKC*VKC/(CM2FM2*CH2FH2)
        
! friction velocity

    FV = UR * SQRT(CM)
    CH2  = VKC*FV/CH2FH2

  END SUBROUTINE SFCDIF1

!== begin sfcdif2 ==================================================================================

  SUBROUTINE SFCDIF2(parameters,ITER   ,Z0     ,THZ0   ,THLM   ,SFCSPD , & !in
                     ZLM    ,ILOC   ,JLOC   ,         & !in
                     AKMS   ,AKHS   ,RLMO   ,WSTAR2 ,         & !in
                     USTAR  )                                   !out

! -------------------------------------------------------------------------------------------------
! SUBROUTINE SFCDIF (renamed SFCDIF_off to avoid clash with Eta PBL)
! -------------------------------------------------------------------------------------------------
! CALCULATE SURFACE LAYER EXCHANGE COEFFICIENTS VIA ITERATIVE PROCESS.
! SEE CHEN ET AL (1997, BLM)
! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE
  type (noahmp_parameters), intent(in) :: parameters
    INTEGER, INTENT(IN) :: ILOC
    INTEGER, INTENT(IN) :: JLOC
    INTEGER, INTENT(IN) :: ITER
    REAL,    INTENT(IN) :: ZLM, Z0, THZ0, THLM, SFCSPD
    REAL, intent(INOUT) :: AKMS
    REAL, intent(INOUT) :: AKHS
    REAL, intent(INOUT) :: RLMO
    REAL, intent(INOUT) :: WSTAR2
    REAL,   intent(OUT) :: USTAR

    REAL     ZZ, PSLMU, PSLMS, PSLHU, PSLHS
    REAL     XX, PSPMU, YY, PSPMS, PSPHU, PSPHS
    REAL     ZILFC, ZU, ZT, RDZ, CXCH
    REAL     DTHV, DU2, BTGH, ZSLU, ZSLT, RLOGU, RLOGT
    REAL     ZETALT, ZETALU, ZETAU, ZETAT, XLU4, XLT4, XU4, XT4

    REAL     XLU, XLT, XU, XT, PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN,  &
         &         RLMA

    INTEGER  ILECH, ITR

    INTEGER, PARAMETER :: ITRMX  = 5
    REAL,    PARAMETER :: WWST   = 1.2
    REAL,    PARAMETER :: WWST2  = WWST * WWST
    REAL,    PARAMETER :: VKRM   = 0.40
    REAL,    PARAMETER :: EXCM   = 0.001
    REAL,    PARAMETER :: BETA   = 1.0 / 270.0
    REAL,    PARAMETER :: BTG    = BETA * GRAV
    REAL,    PARAMETER :: ELFC   = VKRM * BTG
    REAL,    PARAMETER :: WOLD   = 0.15
    REAL,    PARAMETER :: WNEW   = 1.0 - WOLD
    REAL,    PARAMETER :: PIHF   = 3.14159265 / 2.
    REAL,    PARAMETER :: EPSU2  = 1.E-4
    REAL,    PARAMETER :: EPSUST = 0.07
    REAL,    PARAMETER :: EPSIT  = 1.E-4
    REAL,    PARAMETER :: EPSA   = 1.E-8
    REAL,    PARAMETER :: ZTMIN  = -5.0
    REAL,    PARAMETER :: ZTMAX  = 1.0
    REAL,    PARAMETER :: HPBL   = 1000.0
    REAL,    PARAMETER :: SQVISC = 258.2
    REAL,    PARAMETER :: RIC    = 0.183
    REAL,    PARAMETER :: RRIC   = 1.0 / RIC
    REAL,    PARAMETER :: FHNEU  = 0.8
    REAL,    PARAMETER :: RFC    = 0.191
    REAL,    PARAMETER :: RFAC   = RIC / ( FHNEU * RFC * RFC )

! ----------------------------------------------------------------------
! NOTE: THE TWO CODE BLOCKS BELOW DEFINE FUNCTIONS
! ----------------------------------------------------------------------
! LECH'S SURFACE FUNCTIONS
    PSLMU (ZZ)= -0.96* log (1.0-4.5* ZZ)
    PSLMS (ZZ)= ZZ * RRIC -2.076* (1. -1./ (ZZ +1.))
    PSLHU (ZZ)= -0.96* log (1.0-4.5* ZZ)
    PSLHS (ZZ)= ZZ * RFAC -2.076* (1. -1./ (ZZ +1.))
! PAULSON'S SURFACE FUNCTIONS
    PSPMU (XX)= -2.* log ( (XX +1.)*0.5) - log ( (XX * XX +1.)*0.5)   &
         &        +2.* ATAN (XX)                                            &
         &- PIHF
    PSPMS (YY)= 5.* YY
    PSPHU (XX)= -2.* log ( (XX * XX +1.)*0.5)
    PSPHS (YY)= 5.* YY

! THIS ROUTINE SFCDIF CAN HANDLE BOTH OVER OPEN WATER (SEA, OCEAN) AND
! OVER SOLID SURFACE (LAND, SEA-ICE).
! ----------------------------------------------------------------------
!     ZTFC: RATIO OF ZOH/ZOM  LESS OR EQUAL THAN 1
!     C......ZTFC=0.1
!     CZIL: CONSTANT C IN Zilitinkevich, S. S.1995,:NOTE ABOUT ZT
! ----------------------------------------------------------------------
    ILECH = 0

! ----------------------------------------------------------------------
    ZILFC = - parameters%CZIL * VKRM * SQVISC
    ZU = Z0
    RDZ = 1./ ZLM
    CXCH = EXCM * RDZ
    DTHV = THLM - THZ0

! BELJARS CORRECTION OF USTAR
    DU2 = MAX (SFCSPD * SFCSPD,EPSU2)
    BTGH = BTG * HPBL

    IF(ITER == 1) THEN
        IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
           WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
        ELSE
           WSTAR2 = 0.0
        END IF
        USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)
        RLMO = ELFC * AKHS * DTHV / USTAR **3
    END IF
 
! ZILITINKEVITCH APPROACH FOR ZT
    ZT = MAX(1.E-6,EXP (ZILFC * SQRT (USTAR * Z0))* Z0)
    ZSLU = ZLM + ZU
    ZSLT = ZLM + ZT
    RLOGU = log (ZSLU / ZU)
    RLOGT = log (ZSLT / ZT)

! ----------------------------------------------------------------------
! 1./MONIN-OBUKKHOV LENGTH-SCALE
! ----------------------------------------------------------------------
    ZETALT = MAX (ZSLT * RLMO,ZTMIN)
    RLMO = ZETALT / ZSLT
    ZETALU = ZSLU * RLMO
    ZETAU = ZU * RLMO
    ZETAT = ZT * RLMO

    IF (ILECH .eq. 0) THEN
       IF (RLMO .lt. 0.)THEN
          XLU4 = 1. -16.* ZETALU
          XLT4 = 1. -16.* ZETALT
          XU4  = 1. -16.* ZETAU
          XT4  = 1. -16.* ZETAT
          XLU  = SQRT (SQRT (XLU4))
          XLT  = SQRT (SQRT (XLT4))
          XU   = SQRT (SQRT (XU4))

          XT = SQRT (SQRT (XT4))
          PSMZ = PSPMU (XU)
          SIMM = PSPMU (XLU) - PSMZ + RLOGU
          PSHZ = PSPHU (XT)
          SIMH = PSPHU (XLT) - PSHZ + RLOGT
       ELSE
          ZETALU = MIN (ZETALU,ZTMAX)
          ZETALT = MIN (ZETALT,ZTMAX)
          ZETAU  = MIN (ZETAU,ZTMAX/(ZSLU/ZU))   ! Barlage: add limit on ZETAU/ZETAT
          ZETAT  = MIN (ZETAT,ZTMAX/(ZSLT/ZT))   ! Barlage: prevent SIMM/SIMH < 0
          PSMZ = PSPMS (ZETAU)
          SIMM = PSPMS (ZETALU) - PSMZ + RLOGU
          PSHZ = PSPHS (ZETAT)
          SIMH = PSPHS (ZETALT) - PSHZ + RLOGT
       END IF
! ----------------------------------------------------------------------
! LECH'S FUNCTIONS
! ----------------------------------------------------------------------
    ELSE
       IF (RLMO .lt. 0.)THEN
          PSMZ = PSLMU (ZETAU)
          SIMM = PSLMU (ZETALU) - PSMZ + RLOGU
          PSHZ = PSLHU (ZETAT)
          SIMH = PSLHU (ZETALT) - PSHZ + RLOGT
       ELSE
          ZETALU = MIN (ZETALU,ZTMAX)
          ZETALT = MIN (ZETALT,ZTMAX)
          PSMZ = PSLMS (ZETAU)
          SIMM = PSLMS (ZETALU) - PSMZ + RLOGU
          PSHZ = PSLHS (ZETAT)
          SIMH = PSLHS (ZETALT) - PSHZ + RLOGT
       END IF
! ----------------------------------------------------------------------
       END IF

! ----------------------------------------------------------------------
! BELJAARS CORRECTION FOR USTAR
! ----------------------------------------------------------------------
       USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)

! ZILITINKEVITCH FIX FOR ZT
       ZT = MAX(1.E-6,EXP (ZILFC * SQRT (USTAR * Z0))* Z0)
       ZSLT = ZLM + ZT
!-----------------------------------------------------------------------
       RLOGT = log (ZSLT / ZT)
       USTARK = USTAR * VKRM
       IF(SIMM < 1.e-6) SIMM = 1.e-6        ! Limit stability function
       AKMS = MAX (USTARK / SIMM,CXCH)
!-----------------------------------------------------------------------
! IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO
!-----------------------------------------------------------------------
       IF(SIMH < 1.e-6) SIMH = 1.e-6        ! Limit stability function
       AKHS = MAX (USTARK / SIMH,CXCH)

       IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
          WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
       ELSE
          WSTAR2 = 0.0
       END IF
!-----------------------------------------------------------------------
       RLMN = ELFC * AKHS * DTHV / USTAR **3
!-----------------------------------------------------------------------
!     IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)    GO TO 110
!-----------------------------------------------------------------------
       RLMA = RLMO * WOLD+ RLMN * WNEW
!-----------------------------------------------------------------------
       RLMO = RLMA

!       write(*,'(a20,10f15.6)')'SFCDIF: RLMO=',RLMO,RLMN,ELFC , AKHS , DTHV , USTAR
!    END DO
! ----------------------------------------------------------------------
  END SUBROUTINE SFCDIF2

!== begin esat =====================================================================================

  SUBROUTINE ESAT(T, ESW, ESI, DESW, DESI)
!---------------------------------------------------------------------------------------------------
! use polynomials to calculate saturation vapor pressure and derivative with
! respect to temperature: over water when t > 0 c and over ice when t <= 0 c
  IMPLICIT NONE
!---------------------------------------------------------------------------------------------------
! in

  REAL, intent(in)  :: T              !temperature

!out

  REAL, intent(out) :: ESW            !saturation vapor pressure over water (pa)
  REAL, intent(out) :: ESI            !saturation vapor pressure over ice (pa)
  REAL, intent(out) :: DESW           !d(esat)/dt over water (pa/K)
  REAL, intent(out) :: DESI           !d(esat)/dt over ice (pa/K)

! local

  REAL :: A0,A1,A2,A3,A4,A5,A6  !coefficients for esat over water
  REAL :: B0,B1,B2,B3,B4,B5,B6  !coefficients for esat over ice
  REAL :: C0,C1,C2,C3,C4,C5,C6  !coefficients for dsat over water
  REAL :: D0,D1,D2,D3,D4,D5,D6  !coefficients for dsat over ice

  PARAMETER (A0=6.107799961    , A1=4.436518521E-01,  &
             A2=1.428945805E-02, A3=2.650648471E-04,  &
             A4=3.031240396E-06, A5=2.034080948E-08,  &
             A6=6.136820929E-11)

  PARAMETER (B0=6.109177956    , B1=5.034698970E-01,  &
             B2=1.886013408E-02, B3=4.176223716E-04,  &
             B4=5.824720280E-06, B5=4.838803174E-08,  &
             B6=1.838826904E-10)

  PARAMETER (C0= 4.438099984E-01, C1=2.857002636E-02,  &
             C2= 7.938054040E-04, C3=1.215215065E-05,  &
             C4= 1.036561403E-07, C5=3.532421810e-10,  &
             C6=-7.090244804E-13)

  PARAMETER (D0=5.030305237E-01, D1=3.773255020E-02,  &
             D2=1.267995369E-03, D3=2.477563108E-05,  &
             D4=3.005693132E-07, D5=2.158542548E-09,  &
             D6=7.131097725E-12)

  ESW  = 100.*(A0+T*(A1+T*(A2+T*(A3+T*(A4+T*(A5+T*A6))))))
  ESI  = 100.*(B0+T*(B1+T*(B2+T*(B3+T*(B4+T*(B5+T*B6))))))
  DESW = 100.*(C0+T*(C1+T*(C2+T*(C3+T*(C4+T*(C5+T*C6))))))
  DESI = 100.*(D0+T*(D1+T*(D2+T*(D3+T*(D4+T*(D5+T*D6))))))

  END SUBROUTINE ESAT

!== begin stomata ==================================================================================

  SUBROUTINE STOMATA (parameters,VEGTYP  ,MPE     ,APAR    ,FOLN    ,ILOC    , JLOC, & !in
                      TV      ,EI      ,EA      ,SFCTMP  ,SFCPRS  , & !in
                      O2      ,CO2     ,IGS     ,BTRAN   ,RB      , & !in
                      RS      ,PSN     )                              !out
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
      INTEGER,INTENT(IN)  :: ILOC   !grid index
      INTEGER,INTENT(IN)  :: JLOC   !grid index
      INTEGER,INTENT(IN)  :: VEGTYP !vegetation physiology type

      REAL, INTENT(IN)    :: IGS    !growing season index (0=off, 1=on)
      REAL, INTENT(IN)    :: MPE    !prevents division by zero errors

      REAL, INTENT(IN)    :: TV     !foliage temperature (k)
      REAL, INTENT(IN)    :: EI     !vapor pressure inside leaf (sat vapor press at tv) (pa)
      REAL, INTENT(IN)    :: EA     !vapor pressure of canopy air (pa)
      REAL, INTENT(IN)    :: APAR   !par absorbed per unit lai (w/m2)
      REAL, INTENT(IN)    :: O2     !atmospheric o2 concentration (pa)
      REAL, INTENT(IN)    :: CO2    !atmospheric co2 concentration (pa)
      REAL, INTENT(IN)    :: SFCPRS !air pressure at reference height (pa)
      REAL, INTENT(IN)    :: SFCTMP !air temperature at reference height (k)
      REAL, INTENT(IN)    :: BTRAN  !soil water transpiration factor (0 to 1)
      REAL, INTENT(IN)    :: FOLN   !foliage nitrogen concentration (%)
      REAL, INTENT(IN)    :: RB     !boundary layer resistance (s/m)

! output
      REAL, INTENT(OUT)   :: RS     !leaf stomatal resistance (s/m)
      REAL, INTENT(OUT)   :: PSN    !foliage photosynthesis (umol co2 /m2/ s) [always +]

! in&out
      REAL                :: RLB    !boundary layer resistance (s m2 / umol)
! ---------------------------------------------------------------------------------------------

! ------------------------ local variables ----------------------------------------------------
      INTEGER :: ITER     !iteration index
      INTEGER :: NITER    !number of iterations

      DATA NITER /3/
      SAVE NITER

      REAL :: AB          !used in statement functions
      REAL :: BC          !used in statement functions
      REAL :: F1          !generic temperature response (statement function)
      REAL :: F2          !generic temperature inhibition (statement function)
      REAL :: TC          !foliage temperature (degree Celsius)
      REAL :: CS          !co2 concentration at leaf surface (pa)
      REAL :: KC          !co2 Michaelis-Menten constant (pa)
      REAL :: KO          !o2 Michaelis-Menten constant (pa)
      REAL :: A,B,C,Q     !intermediate calculations for RS
      REAL :: R1,R2       !roots for RS
      REAL :: FNF         !foliage nitrogen adjustment factor (0 to 1)
      REAL :: PPF         !absorb photosynthetic photon flux (umol photons/m2/s)
      REAL :: WC          !Rubisco limited photosynthesis (umol co2/m2/s)
      REAL :: WJ          !light limited photosynthesis (umol co2/m2/s)
      REAL :: WE          !export limited photosynthesis (umol co2/m2/s)
      REAL :: CP          !co2 compensation point (pa)
      REAL :: CI          !internal co2 (pa)
      REAL :: AWC         !intermediate calculation for wc
      REAL :: VCMX        !maximum rate of carbonylation (umol co2/m2/s)
      REAL :: J           !electron transport (umol co2/m2/s)
      REAL :: CEA         !constrain ea or else model blows up
      REAL :: CF          !s m2/umol -> s/m

      F1(AB,BC) = AB**((BC-25.)/10.)
      F2(AB) = 1. + EXP((-2.2E05+710.*(AB+273.16))/(8.314*(AB+273.16)))
      REAL :: T
! ---------------------------------------------------------------------------------------------

! initialize RS=RSMAX and PSN=0 because will only do calculations
! for APAR > 0, in which case RS <= RSMAX and PSN >= 0

         CF = SFCPRS/(8.314*SFCTMP)*1.e06
         RS = 1./parameters%BP * CF
         PSN = 0.

         IF (APAR .LE. 0.) RETURN

         FNF = MIN( FOLN/MAX(MPE,parameters%FOLNMX), 1.0 )
         TC  = TV-TFRZ
         PPF = 4.6*APAR
         J   = PPF*parameters%QE25
         KC  = parameters%KC25 * F1(parameters%AKC,TC)
         KO  = parameters%KO25 * F1(parameters%AKO,TC)
         AWC = KC * (1.+O2/KO)
         CP  = 0.5*KC/KO*O2*0.21
         VCMX = parameters%VCMX25 / F2(TC) * FNF * BTRAN * F1(parameters%AVCMX,TC)

! first guess ci

         CI = 0.7*CO2*parameters%C3PSN + 0.4*CO2*(1.-parameters%C3PSN)

! rb: s/m -> s m**2 / umol

         RLB = RB/CF

! constrain ea

         CEA = MAX(0.25*EI*parameters%C3PSN+0.40*EI*(1.-parameters%C3PSN), MIN(EA,EI) )

! ci iteration
!jref: C3PSN is equal to 1 for all veg types.
       DO ITER = 1, NITER
            WJ = MAX(CI-CP,0.)*J/(CI+2.*CP)*parameters%C3PSN  + J*(1.-parameters%C3PSN)
            WC = MAX(CI-CP,0.)*VCMX/(CI+AWC)*parameters%C3PSN + VCMX*(1.-parameters%C3PSN)
            WE = 0.5*VCMX*parameters%C3PSN + 4000.*VCMX*CI/SFCPRS*(1.-parameters%C3PSN)
            PSN = MIN(WJ,WC,WE) * IGS

            CS = MAX( CO2-1.37*RLB*SFCPRS*PSN, MPE )
            A = parameters%MP*PSN*SFCPRS*CEA / (CS*EI) + parameters%BP
            B = ( parameters%MP*PSN*SFCPRS/CS + parameters%BP ) * RLB - 1.
            C = -RLB
            IF (B .GE. 0.) THEN
               Q = -0.5*( B + SQRT(B*B-4.*A*C) )
            ELSE
               Q = -0.5*( B - SQRT(B*B-4.*A*C) )
            END IF
            R1 = Q/A
            R2 = C/Q
            RS = MAX(R1,R2)
            CI = MAX( CS-PSN*SFCPRS*1.65*RS, 0. )
       END DO 

! rs, rb:  s m**2 / umol -> s/m

         RS = RS*CF

  END SUBROUTINE STOMATA

!== begin canres ===================================================================================

  SUBROUTINE CANRES (parameters,PAR   ,SFCTMP,RCSOIL ,EAH   ,SFCPRS , & !in
                     RC    ,PSN   ,ILOC   ,JLOC  )           !out

! --------------------------------------------------------------------------------------------------
! calculate canopy resistance which depends on incoming solar radiation,
! air temperature, atmospheric water vapor pressure deficit at the
! lowest model level, and soil moisture (preferably unfrozen soil
! moisture rather than total)
! --------------------------------------------------------------------------------------------------
! source:  Jarvis (1976), Noilhan and Planton (1989, MWR), Jacquemin and
! Noilhan (1990, BLM). Chen et al (1996, JGR, Vol 101(D3), 7251-7268), 
! eqns 12-14 and table 2 of sec. 3.1.2
! --------------------------------------------------------------------------------------------------
!niu    USE module_Noahlsm_utility
! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                  INTENT(IN)  :: ILOC   !grid index
    INTEGER,                  INTENT(IN)  :: JLOC   !grid index
    REAL,                     INTENT(IN)  :: PAR    !par absorbed per unit sunlit lai (w/m2)
    REAL,                     INTENT(IN)  :: SFCTMP !canopy air temperature
    REAL,                     INTENT(IN)  :: SFCPRS !surface pressure (pa)
    REAL,                     INTENT(IN)  :: EAH    !water vapor pressure (pa)
    REAL,                     INTENT(IN)  :: RCSOIL !soil moisture stress factor

!outputs

    REAL,                     INTENT(OUT) :: RC     !canopy resistance per unit LAI
    REAL,                     INTENT(OUT) :: PSN    !foliage photosynthesis (umolco2/m2/s)

!local

    REAL                                  :: RCQ
    REAL                                  :: RCS
    REAL                                  :: RCT
    REAL                                  :: FF
    REAL                                  :: Q2     !water vapor mixing ratio (kg/kg)
    REAL                                  :: Q2SAT  !saturation Q2
    REAL                                  :: DQSDT2 !d(Q2SAT)/d(T)

! RSMIN, RSMAX, TOPT, RGL, HS are canopy stress parameters set in REDPRM
! ----------------------------------------------------------------------
! initialize canopy resistance multiplier terms.
! ----------------------------------------------------------------------
    RC     = 0.0
    RCS    = 0.0
    RCT    = 0.0
    RCQ    = 0.0

!  compute Q2 and Q2SAT

    Q2 = 0.622 *  EAH  / (SFCPRS - 0.378 * EAH) !specific humidity [kg/kg]
    Q2 = Q2 / (1.0 + Q2)                        !mixing ratio [kg/kg]

    CALL CALHUM(parameters,SFCTMP, SFCPRS, Q2SAT, DQSDT2)

! contribution due to incoming solar radiation

    FF  = 2.0 * PAR / parameters%RGL                
    RCS = (FF + parameters%RSMIN / parameters%RSMAX) / (1.0+ FF)
    RCS = MAX (RCS,0.0001)

! contribution due to air temperature

    RCT = 1.0- 0.0016* ( (parameters%TOPT - SFCTMP)**2.0)
    RCT = MAX (RCT,0.0001)

! contribution due to vapor pressure deficit

    RCQ = 1.0/ (1.0+ parameters%HS * MAX(0.,Q2SAT-Q2))
    RCQ = MAX (RCQ,0.01)

! determine canopy resistance due to all factors

    RC  = parameters%RSMIN / (RCS * RCT * RCQ * RCSOIL)
    PSN = -999.99       ! PSN not applied for dynamic carbon

  END SUBROUTINE CANRES

!== begin calhum ===================================================================================

        SUBROUTINE CALHUM(parameters,SFCTMP, SFCPRS, Q2SAT, DQSDT2)

        IMPLICIT NONE

  type (noahmp_parameters), intent(in) :: parameters
        REAL, INTENT(IN)       :: SFCTMP, SFCPRS
        REAL, INTENT(OUT)      :: Q2SAT, DQSDT2
        REAL, PARAMETER        :: A2=17.67,A3=273.15,A4=29.65, ELWV=2.501E6,         &
                                  A23M4=A2*(A3-A4), E0=0.611, RV=461.0,             &
                                  EPSILON=0.622
        REAL                   :: ES, SFCPRSX

! Q2SAT: saturated mixing ratio
        ES = E0 * EXP ( ELWV/RV*(1./A3 - 1./SFCTMP) )
! convert SFCPRS from Pa to KPa
        SFCPRSX = SFCPRS*1.E-3
        Q2SAT = EPSILON * ES / (SFCPRSX-ES)
! convert from  g/g to g/kg
        Q2SAT = Q2SAT * 1.E3
! Q2SAT is currently a 'mixing ratio'

! DQSDT2 is calculated assuming Q2SAT is a specific humidity
        DQSDT2=(Q2SAT/(1+Q2SAT))*A23M4/(SFCTMP-A4)**2

! DG Q2SAT needs to be in g/g when returned for SFLX
        Q2SAT = Q2SAT / 1.E3

        END SUBROUTINE CALHUM

!== begin tsnosoi ==================================================================================

  SUBROUTINE TSNOSOI (parameters,ICE     ,NSOIL   ,NSNOW   ,ISNOW   ,IST     , & !in
                      TBOT    ,ZSNSO   ,SSOIL   ,DF      ,HCPCT   , & !in
                      SAG     ,DT      ,SNOWH   ,DZSNSO  , & !in
                      TG      ,ILOC    ,JLOC    ,                   & !in
                      STC     )                                       !inout
! --------------------------------------------------------------------------------------------------
! Compute snow (up to 3L) and soil (4L) temperature. Note that snow temperatures
! during melting season may exceed melting point (TFRZ) but later in PHASECHANGE
! subroutine the snow temperatures are reset to TFRZ for melting snow.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
!input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                         INTENT(IN)  :: ILOC
    INTEGER,                         INTENT(IN)  :: JLOC
    INTEGER,                         INTENT(IN)  :: ICE    !
    INTEGER,                         INTENT(IN)  :: NSOIL  !no of soil layers (4)
    INTEGER,                         INTENT(IN)  :: NSNOW  !maximum no of snow layers (3)
    INTEGER,                         INTENT(IN)  :: ISNOW  !actual no of snow layers
    INTEGER,                         INTENT(IN)  :: IST    !surface type

    REAL,                            INTENT(IN)  :: DT     !time step (s)
    REAL,                            INTENT(IN)  :: TBOT   !
    REAL,                            INTENT(IN)  :: SSOIL  !ground heat flux (w/m2)
    REAL,                            INTENT(IN)  :: SAG    !solar rad. absorbed by ground (w/m2)
    REAL,                            INTENT(IN)  :: SNOWH  !snow depth (m)
    REAL,                            INTENT(IN)  :: TG     !ground temperature (k)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: ZSNSO  !layer-bot. depth from snow surf.(m)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DZSNSO !snow/soil layer thickness (m)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DF     !thermal conductivity
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: HCPCT  !heat capacity (J/m3/k)

!input and output

    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC

!local

    INTEGER                                      :: IZ
    REAL                                         :: ZBOTSNO   !ZBOT from snow surface
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: AI, BI, CI, RHSTS
    REAL                                         :: EFLXB !energy influx from soil bottom (w/m2)
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: PHI   !light through water (w/m2)

    REAL, DIMENSION(-NSNOW+1:NSOIL) :: TBEG
    REAL                            :: ERR_EST !heat storage error  (w/m2)
    REAL                            :: SSOIL2  !ground heat flux (w/m2) (for energy check)
    REAL                            :: EFLXB2  !heat flux from the bottom (w/m2) (for energy check)
    character(len=256)              :: message
! ----------------------------------------------------------------------
! compute solar penetration through water, needs more work

    PHI(ISNOW+1:NSOIL) = 0.

! adjust ZBOT from soil surface to ZBOTSNO from snow surface

    ZBOTSNO = parameters%ZBOT - SNOWH    !from snow surface

! snow/soil heat storage for energy balance check

    DO IZ = ISNOW+1, NSOIL
       TBEG(IZ) = STC(IZ)
    ENDDO

! compute soil temperatures

      CALL HRT   (parameters,NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , &
                  STC       ,TBOT      ,ZBOTSNO   ,DT        , &
                  DF        ,HCPCT     ,SSOIL     ,PHI       , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  EFLXB     )

      CALL HSTEP (parameters,NSNOW     ,NSOIL     ,ISNOW     ,DT        , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  STC       ) 

! update ground heat flux just for energy check, but not for final output
! otherwise, it would break the surface energy balance

    IF(OPT_TBOT == 1) THEN
       EFLXB2  = 0.
    ELSE IF(OPT_TBOT == 2) THEN
       EFLXB2  = DF(NSOIL)*(TBOT-STC(NSOIL)) / &
            (0.5*(ZSNSO(NSOIL-1)+ZSNSO(NSOIL)) - ZBOTSNO)
    END IF

    ! Skip the energy balance check for now, until we can make it work
    ! right for small time steps.
    return

! energy balance check

    ERR_EST = 0.0
    DO IZ = ISNOW+1, NSOIL
       ERR_EST = ERR_EST + (STC(IZ)-TBEG(IZ)) * DZSNSO(IZ) * HCPCT(IZ) / DT
    ENDDO

    if (OPT_STC == 1 .OR. OPT_STC == 3) THEN   ! semi-implicit
       ERR_EST = ERR_EST - (SSOIL +EFLXB)
    ELSE                     ! full-implicit
       SSOIL2 = DF(ISNOW+1)*(TG-STC(ISNOW+1))/(0.5*DZSNSO(ISNOW+1))   !M. Barlage
       ERR_EST = ERR_EST - (SSOIL2+EFLXB2)
    ENDIF

    IF (ABS(ERR_EST) > 1.) THEN    ! W/m2
       WRITE(message,*) 'TSNOSOI is losing(-)/gaining(+) false energy',ERR_EST,' W/m2'
       call wrf_message(trim(message))
       WRITE(message,'(i6,1x,i6,1x,i3,F18.13,5F20.12)') &
            ILOC, JLOC, IST,ERR_EST,SSOIL,SNOWH,TG,STC(ISNOW+1),EFLXB
       call wrf_message(trim(message))
       !niu      STOP
    END IF

  END SUBROUTINE TSNOSOI

!== begin hrt ======================================================================================

  SUBROUTINE HRT (parameters,NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , &
                  STC       ,TBOT      ,ZBOT      ,DT        , &
                  DF        ,HCPCT     ,SSOIL     ,PHI       , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  BOTFLX    )
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! calculate the right hand side of the time tendency term of the soil
! thermal diffusion equation.  also to compute ( prepare ) the matrix
! coefficients for the tri-diagonal matrix of the implicit time scheme.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                         INTENT(IN)  :: NSOIL  !no of soil layers (4)
    INTEGER,                         INTENT(IN)  :: NSNOW  !maximum no of snow layers (3)
    INTEGER,                         INTENT(IN)  :: ISNOW  !actual no of snow layers
    REAL,                            INTENT(IN)  :: TBOT   !bottom soil temp. at ZBOT (k)
    REAL,                            INTENT(IN)  :: ZBOT   !depth of lower boundary condition (m)
                                                           !from soil surface not snow surface
    REAL,                            INTENT(IN)  :: DT     !time step (s)
    REAL,                            INTENT(IN)  :: SSOIL  !ground heat flux (w/m2)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: ZSNSO  !depth of layer-bottom of snow/soil (m)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: STC    !snow/soil temperature (k)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DF     !thermal conductivity [w/m/k]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: HCPCT  !heat capacity [j/m3/k]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: PHI    !light through water (w/m2)

! output

    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: RHSTS  !right-hand side of the matrix
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: AI     !left-hand side coefficient
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: BI     !left-hand side coefficient
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: CI     !left-hand side coefficient
    REAL,                            INTENT(OUT) :: BOTFLX !energy influx from soil bottom (w/m2)

! local

    INTEGER                                      :: K
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DDZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DENOM
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DTSDZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: EFLUX
    REAL                                         :: TEMP1
! ----------------------------------------------------------------------

    DO K = ISNOW+1, NSOIL
        IF (K == ISNOW+1) THEN
           DENOM(K)  = - ZSNSO(K) * HCPCT(K)
           TEMP1     = - ZSNSO(K+1)
           DDZ(K)    = 2.0 / TEMP1
           DTSDZ(K)  = 2.0 * (STC(K) - STC(K+1)) / TEMP1
           EFLUX(K)  = DF(K) * DTSDZ(K) - SSOIL - PHI(K)
        ELSE IF (K < NSOIL) THEN
           DENOM(K)  = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
           TEMP1     = ZSNSO(K-1) - ZSNSO(K+1)
           DDZ(K)    = 2.0 / TEMP1
           DTSDZ(K)  = 2.0 * (STC(K) - STC(K+1)) / TEMP1
           EFLUX(K)  = (DF(K)*DTSDZ(K) - DF(K-1)*DTSDZ(K-1)) - PHI(K)
        ELSE IF (K == NSOIL) THEN
           DENOM(K)  = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
           TEMP1     =  ZSNSO(K-1) - ZSNSO(K)
           IF(OPT_TBOT == 1) THEN
               BOTFLX     = 0. 
           END IF
           IF(OPT_TBOT == 2) THEN
               DTSDZ(K)  = (STC(K) - TBOT) / ( 0.5*(ZSNSO(K-1)+ZSNSO(K)) - ZBOT)
               BOTFLX    = -DF(K) * DTSDZ(K)
           END IF
           EFLUX(K)  = (-BOTFLX - DF(K-1)*DTSDZ(K-1) ) - PHI(K)
        END IF
    END DO

    DO K = ISNOW+1, NSOIL
        IF (K == ISNOW+1) THEN
           AI(K)    =   0.0
           CI(K)    = - DF(K)   * DDZ(K) / DENOM(K)
           IF (OPT_STC == 1 .OR. OPT_STC == 3 ) THEN
              BI(K) = - CI(K)
           END IF                                        
           IF (OPT_STC == 2) THEN
              BI(K) = - CI(K) + DF(K)/(0.5*ZSNSO(K)*ZSNSO(K)*HCPCT(K))
           END IF
        ELSE IF (K < NSOIL) THEN
           AI(K)    = - DF(K-1) * DDZ(K-1) / DENOM(K) 
           CI(K)    = - DF(K  ) * DDZ(K  ) / DENOM(K) 
           BI(K)    = - (AI(K) + CI (K))
        ELSE IF (K == NSOIL) THEN
           AI(K)    = - DF(K-1) * DDZ(K-1) / DENOM(K) 
           CI(K)    = 0.0
           BI(K)    = - (AI(K) + CI(K))
        END IF
           RHSTS(K)  = EFLUX(K)/ (-DENOM(K))
    END DO

  END SUBROUTINE HRT

!== begin hstep ====================================================================================

  SUBROUTINE HSTEP (parameters,NSNOW     ,NSOIL     ,ISNOW     ,DT        ,  &
                    AI        ,BI        ,CI        ,RHSTS     ,  &
                    STC       )  
! ----------------------------------------------------------------------
! CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD.
! ----------------------------------------------------------------------
    implicit none
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                         INTENT(IN)    :: NSOIL
    INTEGER,                         INTENT(IN)    :: NSNOW
    INTEGER,                         INTENT(IN)    :: ISNOW
    REAL,                            INTENT(IN)    :: DT

! output & input
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: RHSTS
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: AI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: BI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: CI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC

! local
    INTEGER                                        :: K
    REAL, DIMENSION(-NSNOW+1:NSOIL)                :: RHSTSIN
    REAL, DIMENSION(-NSNOW+1:NSOIL)                :: CIIN
! ----------------------------------------------------------------------

    DO K = ISNOW+1,NSOIL
       RHSTS(K) =   RHSTS(K) * DT
       AI(K)    =      AI(K) * DT
       BI(K)    = 1. + BI(K) * DT
       CI(K)    =      CI(K) * DT
    END DO

! copy values for input variables before call to rosr12

    DO K = ISNOW+1,NSOIL
       RHSTSIN(K) = RHSTS(K)
       CIIN(K)    = CI(K)
    END DO

! solve the tri-diagonal matrix equation

    CALL ROSR12 (CI,AI,BI,CIIN,RHSTSIN,RHSTS,ISNOW+1,NSOIL,NSNOW)

! update snow & soil temperature

    DO K = ISNOW+1,NSOIL
       STC (K) = STC (K) + CI (K)
    END DO

  END SUBROUTINE HSTEP

!== begin rosr12 ===================================================================================

  SUBROUTINE ROSR12 (P,A,B,C,D,DELTA,NTOP,NSOIL,NSNOW)
! ----------------------------------------------------------------------
! SUBROUTINE ROSR12
! ----------------------------------------------------------------------
! INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN BELOW:
! ###                                            ### ###  ###   ###  ###
! #B(1), C(1),  0  ,  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! #A(2), B(2), C(2),  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! # 0  , A(3), B(3), C(3),  0  ,   . . .  ,    0   # #      #   # D(3) #
! # 0  ,  0  , A(4), B(4), C(4),   . . .  ,    0   # # P(4) #   # D(4) #
! # 0  ,  0  ,  0  , A(5), B(5),   . . .  ,    0   # # P(5) #   # D(5) #
! # .                                          .   # #  .   # = #   .  #
! # .                                          .   # #  .   #   #   .  #
! # .                                          .   # #  .   #   #   .  #
! # 0  , . . . , 0 , A(M-2), B(M-2), C(M-2),   0   # #P(M-2)#   #D(M-2)#
! # 0  , . . . , 0 ,   0   , A(M-1), B(M-1), C(M-1)# #P(M-1)#   #D(M-1)#
! # 0  , . . . , 0 ,   0   ,   0   ,  A(M) ,  B(M) # # P(M) #   # D(M) #
! ###                                            ### ###  ###   ###  ###
! ----------------------------------------------------------------------
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: NTOP           
    INTEGER, INTENT(IN)   :: NSOIL,NSNOW
    INTEGER               :: K, KK

    REAL, DIMENSION(-NSNOW+1:NSOIL),INTENT(IN):: A, B, D
    REAL, DIMENSION(-NSNOW+1:NSOIL),INTENT(INOUT):: C,P,DELTA

! ----------------------------------------------------------------------
! INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER
! ----------------------------------------------------------------------
    C (NSOIL) = 0.0
    P (NTOP) = - C (NTOP) / B (NTOP)
! ----------------------------------------------------------------------
! SOLVE THE COEFS FOR THE 1ST SOIL LAYER
! ----------------------------------------------------------------------
    DELTA (NTOP) = D (NTOP) / B (NTOP)
! ----------------------------------------------------------------------
! SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL
! ----------------------------------------------------------------------
    DO K = NTOP+1,NSOIL
       P (K) = - C (K) * ( 1.0 / (B (K) + A (K) * P (K -1)) )
       DELTA (K) = (D (K) - A (K)* DELTA (K -1))* (1.0/ (B (K) + A (K)&
            * P (K -1)))
    END DO
! ----------------------------------------------------------------------
! SET P TO DELTA FOR LOWEST SOIL LAYER
! ----------------------------------------------------------------------
    P (NSOIL) = DELTA (NSOIL)
! ----------------------------------------------------------------------
! ADJUST P FOR SOIL LAYERS 2 THRU NSOIL
! ----------------------------------------------------------------------
    DO K = NTOP+1,NSOIL
       KK = NSOIL - K + (NTOP-1) + 1
       P (KK) = P (KK) * P (KK +1) + DELTA (KK)
    END DO
! ----------------------------------------------------------------------
  END SUBROUTINE ROSR12

!== begin phasechange ==============================================================================

  SUBROUTINE PHASECHANGE (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & !in
                          DZSNSO  ,HCPCT   ,IST     ,ILOC    ,JLOC    , & !in
                          STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & !inout
                          SMC     ,SH2O    ,                            & !inout
                          QMELT   ,IMELT   ,PONDING )                     !out
! ----------------------------------------------------------------------
! melting/freezing of snow water and soil water
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! inputs

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER, INTENT(IN)                             :: ILOC   !grid index
  INTEGER, INTENT(IN)                             :: JLOC   !grid index
  INTEGER, INTENT(IN)                             :: NSNOW  !maximum no. of snow layers [=3]
  INTEGER, INTENT(IN)                             :: NSOIL  !No. of soil layers [=4]
  INTEGER, INTENT(IN)                             :: ISNOW  !actual no. of snow layers [<=3]
  INTEGER, INTENT(IN)                             :: IST    !surface type: 1->soil; 2->lake
  REAL, INTENT(IN)                                :: DT     !land model time step (sec)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: FACT   !temporary
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: DZSNSO !snow/soil layer thickness [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: HCPCT  !heat capacity (J/m3/k)

! outputs
  INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: IMELT  !phase change index
  REAL,                               INTENT(OUT) :: QMELT  !snowmelt rate [mm/s]
  REAL,                               INTENT(OUT) :: PONDING!snowmelt when snow has no layer [mm]

! inputs and outputs

  REAL, INTENT(INOUT) :: SNEQV
  REAL, INTENT(INOUT) :: SNOWH
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT)  :: STC    !snow/soil layer temperature [k]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SH2O   !soil liquid water [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SMC    !total soil water [m3/m3]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT)  :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT)  :: SNLIQ  !snow layer liquid water [mm]

! local

  INTEGER                         :: J         !do loop index
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: HM        !energy residual [w/m2]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: XM        !melting or freezing water [kg/m2]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WMASS0
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WICE0 
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WLIQ0 
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: MICE      !soil/snow ice mass [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: MLIQ      !soil/snow liquid water mass [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: SUPERCOOL !supercooled water in soil (kg/m2)
  REAL                            :: HEATR     !energy residual or loss after melting/freezing
  REAL                            :: TEMP1     !temporary variables [kg/m2]
  REAL                            :: PROPOR
  REAL                            :: SMP       !frozen water potential (mm)
  REAL                            :: XMF       !total latent heat of phase change

! ----------------------------------------------------------------------
! Initialization

    QMELT   = 0.
    PONDING = 0.
    XMF     = 0.

    DO J = -NSNOW+1, NSOIL
         SUPERCOOL(J) = 0.0
    END DO

    DO J = ISNOW+1,0       ! all layers
         MICE(J) = SNICE(J)
         MLIQ(J) = SNLIQ(J)
    END DO

    DO J = 1, NSOIL               ! soil
         MLIQ(J) =  SH2O(J)            * DZSNSO(J) * 1000.
         MICE(J) = (SMC(J) - SH2O(J))  * DZSNSO(J) * 1000.
    END DO

    DO J = ISNOW+1,NSOIL       ! all layers
         IMELT(J)    = 0
         HM(J)       = 0.
         XM(J)       = 0.
         WICE0(J)    = MICE(J)
         WLIQ0(J)    = MLIQ(J)
         WMASS0(J)   = MICE(J) + MLIQ(J)
    ENDDO

    if(ist == 1) then
      DO J = 1,NSOIL
         IF (OPT_FRZ == 1) THEN
            IF(STC(J) < TFRZ) THEN
               SMP = HFUS*(TFRZ-STC(J))/(GRAV*STC(J))             !(m)
               SUPERCOOL(J) = parameters%SMCMAX(J)*(SMP/parameters%PSISAT(J))**(-1./parameters%BEXP(J))
               SUPERCOOL(J) = SUPERCOOL(J)*DZSNSO(J)*1000.        !(mm)
            END IF
         END IF
         IF (OPT_FRZ == 2) THEN
               CALL FRH2O (parameters,J,SUPERCOOL(J),STC(J),SMC(J),SH2O(J))
               SUPERCOOL(J) = SUPERCOOL(J)*DZSNSO(J)*1000.        !(mm)
         END IF
      ENDDO
    end if

    DO J = ISNOW+1,NSOIL
         IF (MICE(J) > 0. .AND. STC(J) >= TFRZ) THEN  !melting 
             IMELT(J) = 1
         ENDIF
         IF (MLIQ(J) > SUPERCOOL(J) .AND. STC(J) < TFRZ) THEN
             IMELT(J) = 2
         ENDIF

         ! If snow exists, but its thickness is not enough to create a layer
         IF (ISNOW == 0 .AND. SNEQV > 0. .AND. J == 1) THEN
             IF (STC(J) >= TFRZ) THEN
                IMELT(J) = 1
             ENDIF
         ENDIF
    ENDDO

! Calculate the energy surplus and loss for melting and freezing

    DO J = ISNOW+1,NSOIL
         IF (IMELT(J) > 0) THEN
             HM(J) = (STC(J)-TFRZ)/FACT(J)
             STC(J) = TFRZ
         ENDIF

         IF (IMELT(J) == 1 .AND. HM(J) < 0.) THEN
            HM(J) = 0.
            IMELT(J) = 0
         ENDIF
         IF (IMELT(J) == 2 .AND. HM(J) > 0.) THEN
            HM(J) = 0.
            IMELT(J) = 0
         ENDIF
         XM(J) = HM(J)*DT/HFUS                           
    ENDDO

! The rate of melting and freezing for snow without a layer, needs more work.

    IF (ISNOW == 0 .AND. SNEQV > 0. .AND. XM(1) > 0.) THEN  
        TEMP1  = SNEQV
        SNEQV  = MAX(0.,TEMP1-XM(1))  
        PROPOR = SNEQV/TEMP1
        SNOWH  = MAX(0.,PROPOR * SNOWH)
                    IRRFRA ,MIFAC  ,FIFAC  ,CROPLU ,                 & !in
                    FICEOLD,PONDING,TG     ,IST    ,FVEG   ,ILOC   ,JLOC ,SMCEQ , & !in
                    BDFALL ,FP     ,RAIN   ,SNOW,                    & !in  MB/AN: v3.7
		    QSNOW  ,QRAIN  ,SNOWHIN,LATHEAV,LATHEAG,frozen_canopy,frozen_ground,    & !in  MB
                    ISNOW  ,CANLIQ ,CANICE ,TV     ,SNOWH  ,SNEQV  , & !inout
                    SNICE  ,SNLIQ  ,STC    ,ZSNSO  ,SH2O   ,SMC    , & !inout
                    SICE   ,ZWT    ,WA     ,WT     ,DZSNSO ,WSLAKE , & !inout
                    SMCWTD ,DEEPRECH,RECH                          , & !inout
! irrigation 
  REAL,                              INTENT(IN)   :: IRRFRA   ! irrigation fraction
  REAL,                              INTENT(IN)   :: MIFAC    ! micro irrigation fraction
  REAL,                              INTENT(IN)   :: FIFAC    ! flood irrigation fraction
  REAL,                              INTENT(INOUT):: IRAMTFI  ! irrigation water amount [m] to be applied, Sprinkler
  REAL,                              INTENT(INOUT):: IRAMTMI  ! irrigation water amount [m] to be applied, Micro
  REAL,                              INTENT(INOUT):: IRFIRATE ! rate of irrigation by micro [m/timestep]
  REAL,                              INTENT(INOUT):: IRMIRATE ! rate of irrigation by micro [m/timestep]
  LOGICAL,                           INTENT(IN)   :: CROPLU   ! flag to identify croplands

! local
  INTEGER                                        :: IZ
  REAL                                           :: QINSUR  !water input on soil surface [m/s]
  REAL                                           :: QSEVA   !soil surface evap rate [mm/s]
  REAL                                           :: QSDEW   !soil surface dew rate [mm/s]
  REAL                                           :: QSNFRO  !snow surface frost rate[mm/s]
  REAL                                           :: QSNSUB  !snow surface sublimation rate [mm/s]
  REAL, DIMENSION(       1:NSOIL)                :: ETRANI  !transpiration rate (mm/s) [+]
  REAL, DIMENSION(       1:NSOIL)                :: WCND   !hydraulic conductivity (m/s)
  REAL                                           :: QDRAIN  !soil-bottom free drainage [mm/s] 
  REAL                                           :: SNOFLOW !glacier flow [mm/s]
  REAL                                           :: FCRMAX !maximum of FCR (-)

  REAL, PARAMETER ::  WSLMAX = 5000.      !maximum lake water storage (mm)

#ifdef WRF_HYDRO
  REAL                           , INTENT(INOUT)    :: sfcheadrt
#endif

! ----------------------------------------------------------------------
! initialize

   ETRANI(1:NSOIL) = 0.
   SNOFLOW         = 0.
   RUNSUB          = 0.
   QINSUR          = 0.

! canopy-intercepted snowfall/rainfall, drips, and throughfall

   CALL CANWATER (parameters,VEGTYP ,DT     , & !in
                  FCEV   ,FCTR   ,ELAI   , & !in
                  ESAI   ,TG     ,FVEG   ,ILOC   , JLOC, & !in
                  BDFALL ,FROZEN_CANOPY  , & !in     
                  CANLIQ ,CANICE ,TV     ,                 & !inout
                  CMC    ,ECAN   ,ETRAN  , & !out
                  FWET      )                           !out

! sublimation, frost, evaporation, and dew

     QSNSUB = 0.
     IF (SNEQV > 0.) THEN
       QSNSUB = MIN(QVAP, SNEQV/DT)
     ENDIF
     QSEVA = QVAP-QSNSUB

     QSNFRO = 0.
     IF (SNEQV > 0.) THEN
        QSNFRO = QDEW
     ENDIF
     QSDEW = QDEW - QSNFRO

     CALL SNOWWATER (parameters,NSNOW  ,NSOIL  ,IMELT  ,DT     ,ZSOIL  , & !in
          &          SFCTMP ,SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB , & !in
          &          QRAIN  ,FICEOLD,ILOC   ,JLOC   ,         & !in
          &          ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & !inout
          &          SH2O   ,SICE   ,STC    ,ZSNSO  ,DZSNSO , & !inout
          &          QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  !out

   IF(FROZEN_GROUND) THEN
      SICE(1) =  SICE(1) + (QSDEW-QSEVA)*DT/(DZSNSO(1)*1000.)
      QSDEW = 0.0
      QSEVA = 0.0
      IF(SICE(1) < 0.) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.
      END IF
   END IF

! convert units (mm/s -> m/s)

    !PONDING: melting water from snow when there is no layer
    QINSUR = (PONDING+PONDING1+PONDING2)/DT * 0.001
!    QINSUR = PONDING/DT * 0.001

    IF(ISNOW == 0) THEN
       QINSUR = QINSUR+(QSNBOT + QSDEW + QRAIN) * 0.001
    ELSE
       QINSUR = QINSUR+(QSNBOT + QSDEW) * 0.001
    ENDIF

    QSEVA  = QSEVA * 0.001 

    DO IZ = 1, parameters%NROOT
       ETRANI(IZ) = ETRAN * BTRANI(IZ) * 0.001
    ENDDO

#ifdef WRF_HYDRO
       QINSUR = QINSUR+sfcheadrt/DT*0.001  !sfcheadrt units (m)
#endif

                DZSNSO(J+1) = DZSNSO(J+1) + DZSNSO(J)
             ELSE
               IF (ISNOW_OLD < -1) THEN    ! MB/KM: change to ISNOW
                SNLIQ(J-1) = SNLIQ(J-1) + SNLIQ(J)
                SNICE(J-1) = SNICE(J-1) + SNICE(J)
           DZSNSO(J) = max(DZSNSO(J),SNICE(J)/DENICE + SNLIQ(J)/DENH2O)
        END IF

        ! Pressure of overlying snow

        BURDEN = BURDEN + WX

    END DO

  END SUBROUTINE COMPACT

!== begin snowh2o ==================================================================================

  SUBROUTINE SNOWH2O (parameters,NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & !in 
                      QRAIN  ,ILOC   ,JLOC   ,                 & !in
                      ISNOW  ,DZSNSO ,SNOWH  ,SNEQV  ,SNICE  , & !inout
                      SNLIQ  ,SH2O   ,SICE   ,STC    ,         & !inout
                      QSNBOT ,PONDING1       ,PONDING2)          !out
! ----------------------------------------------------------------------
! Renew the mass of ice lens (SNICE) and liquid (SNLIQ) of the
! surface snow layer resulting from sublimation (frost) / evaporation (dew)
! ----------------------------------------------------------------------
   IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
   INTEGER,                         INTENT(IN)    :: ILOC   !grid index
   INTEGER,                         INTENT(IN)    :: JLOC   !grid index
   INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers[=3]
   INTEGER,                         INTENT(IN)    :: NSOIL  !No. of soil layers[=4]
   REAL,                            INTENT(IN)    :: DT     !time step
   REAL,                            INTENT(IN)    :: QSNFRO !snow surface frost rate[mm/s]
   REAL,                            INTENT(IN)    :: QSNSUB !snow surface sublimation rate[mm/s]
   REAL,                            INTENT(IN)    :: QRAIN  !snow surface rain rate[mm/s]

! output

   REAL,                            INTENT(OUT)   :: QSNBOT !melting water out of snow bottom [mm/s]

! input and output

   INTEGER,                         INTENT(INOUT) :: ISNOW  !actual no. of snow layers
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO ! snow layer depth [m]
   REAL,                            INTENT(INOUT) :: SNOWH  !snow height [m]
   REAL,                            INTENT(INOUT) :: SNEQV  !snow water eqv. [mm]
   REAL, DIMENSION(-NSNOW+1:0),     INTENT(INOUT) :: SNICE  !snow layer ice [mm]
   REAL, DIMENSION(-NSNOW+1:0),     INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]
   REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid moisture (m3/m3)
   REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE   !soil ice moisture (m3/m3)
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow layer temperature [k]

! local variables:

   INTEGER                     :: J         !do loop/array indices
   REAL                        :: QIN       !water flow into the element (mm/s)
   REAL                        :: QOUT      !water flow out of the element (mm/s)
   REAL                        :: WGDIF     !ice mass after minus sublimation
   REAL, DIMENSION(-NSNOW+1:0) :: VOL_LIQ   !partial volume of liquid water in layer
   REAL, DIMENSION(-NSNOW+1:0) :: VOL_ICE   !partial volume of ice lens in layer
   REAL, DIMENSION(-NSNOW+1:0) :: EPORE     !effective porosity = porosity - VOL_ICE
   REAL :: PROPOR, TEMP
   REAL :: PONDING1, PONDING2
      SNOWH  = MIN(MAX(SNOWH,SNEQV/500.0),SNEQV/50.0)  ! limit adjustment to a reasonable density

      IF(SNEQV < 0.) THEN
         SICE(1) = SICE(1) + SNEQV/(DZSNSO(1)*1000.)
         SNEQV   = 0.
         SNOWH   = 0.
      END IF
      IF(SICE(1) < 0.) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.
      END IF
   END IF

   IF(SNOWH <= 1.E-8 .OR. SNEQV <= 1.E-6) THEN
     SNOWH = 0.0
     SNEQV = 0.0
   END IF

! for deep snow

   IF ( ISNOW < 0 ) THEN !KWM added this IF statement to prevent out-of-bounds array references

      WGDIF = SNICE(ISNOW+1) - QSNSUB*DT + QSNFRO*DT
      SNICE(ISNOW+1) = WGDIF
      IF (WGDIF < 1.e-6 .and. ISNOW <0) THEN
         CALL  COMBINE (parameters,NSNOW  ,NSOIL  ,ILOC, JLOC   , & !in
              ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !inout
              DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & !inout
              PONDING1, PONDING2 )                       !out
      ENDIF
      !KWM:  Subroutine COMBINE can change ISNOW to make it 0 again?
      IF ( ISNOW < 0 ) THEN !KWM added this IF statement to prevent out-of-bounds array references
         SNLIQ(ISNOW+1) = SNLIQ(ISNOW+1) + QRAIN * DT
         SNLIQ(ISNOW+1) = MAX(0., SNLIQ(ISNOW+1))
      ENDIF
      
   ENDIF !KWM  -- Can the ENDIF be moved toward the end of the subroutine (Just set QSNBOT=0)?

! Porosity and partial volume

   DO J = ISNOW+1, 0
     VOL_ICE(J)      = MIN(1., SNICE(J)/(DZSNSO(J)*DENICE))
     EPORE(J)        = 1. - VOL_ICE(J)
   END DO

   QIN = 0.
   QOUT = 0.

   DO J = ISNOW+1, 0
     SNLIQ(J) = SNLIQ(J) + QIN
     VOL_LIQ(J) = SNLIQ(J)/(DZSNSO(J)*DENH2O)
     QOUT = MAX(0.,(VOL_LIQ(J)-parameters%SSI*EPORE(J))*DZSNSO(J))
     IF(J == 0) THEN
       QOUT = MAX((VOL_LIQ(J)- EPORE(J))*DZSNSO(J) , parameters%SNOW_RET_FAC*DT*QOUT)
     END IF
     QOUT = QOUT*DENH2O
     SNLIQ(J) = SNLIQ(J) - QOUT
     IF((SNLIQ(J)/(SNICE(J)+SNLIQ(J))) > max_liq_mass_fraction) THEN
       QOUT = QOUT + (SNLIQ(J) - max_liq_mass_fraction/(1.0 - max_liq_mass_fraction)*SNICE(J))
       SNLIQ(J) = max_liq_mass_fraction/(1.0 - max_liq_mass_fraction)*SNICE(J)
     ENDIF
     QIN = QOUT
   END DO

   DO J = ISNOW+1, 0
     DZSNSO(J) = MAX(DZSNSO(J),SNLIQ(J)/DENH2O + SNICE(J)/DENICE)
   END DO

! Liquid water from snow bottom to soil

   QSNBOT = QOUT / DT           ! mm/s

  END SUBROUTINE SNOWH2O

!== begin soilwater ================================================================================

  SUBROUTINE SOILWATER (parameters,NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & !in
                        QINSUR ,QSEVA  ,ETRANI ,SICE   ,ILOC   , JLOC, & !in
                        SH2O   ,SMC    ,ZWT    ,VEGTYP ,& !inout
                        SMCWTD, DEEPRECH                       ,& !inout
                        RUNSRF ,QDRAIN ,RUNSUB ,WCND   ,FCRMAX )   !out

! ----------------------------------------------------------------------
! calculate surface runoff and soil moisture.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                     INTENT(IN) :: ILOC   !grid index
  INTEGER,                     INTENT(IN) :: JLOC   !grid index
  INTEGER,                     INTENT(IN) :: NSOIL  !no. of soil layers
  INTEGER,                     INTENT(IN) :: NSNOW  !maximum no. of snow layers
  REAL,                        INTENT(IN) :: DT     !time step (sec)
  REAL, INTENT(IN)                        :: QINSUR !water input on soil surface [mm/s]
  REAL, INTENT(IN)                        :: QSEVA  !evap from soil surface [mm/s]
  REAL, DIMENSION(1:NSOIL),    INTENT(IN) :: ZSOIL  !depth of soil layer-bottom [m]
  REAL, DIMENSION(1:NSOIL),    INTENT(IN) :: ETRANI !evapotranspiration from soil layers [mm/s]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer depth [m]
  REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: SICE   !soil ice content [m3/m3]

  INTEGER,                     INTENT(IN) :: VEGTYP

! input & output
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid water content [m3/m3]
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC    !total soil water content [m3/m3]
  REAL, INTENT(INOUT)                     :: ZWT    !water table depth [m]
  REAL,                     INTENT(INOUT) :: SMCWTD !soil moisture between bottom of the soil and the water table [m3/m3]
  REAL                    , INTENT(INOUT) :: DEEPRECH

! output
  REAL, INTENT(OUT)                       :: QDRAIN !soil-bottom free drainage [mm/s] 
  REAL, INTENT(OUT)                       :: RUNSRF !surface runoff [mm/s] 
  REAL, INTENT(OUT)                       :: RUNSUB !subsurface runoff [mm/s] 
  REAL, INTENT(OUT)                       :: FCRMAX !maximum of FCR (-)
  REAL, DIMENSION(1:NSOIL), INTENT(OUT)   :: WCND   !hydraulic conductivity (m/s)

! local
  INTEGER                                 :: K,IZ   !do-loop index
  INTEGER                                 :: ITER   !iteration index
  REAl                                    :: DTFINE !fine time step (s)
  REAL, DIMENSION(1:NSOIL)                :: RHSTT  !right-hand side term of the matrix
  REAL, DIMENSION(1:NSOIL)                :: AI     !left-hand side term
  REAL, DIMENSION(1:NSOIL)                :: BI     !left-hand side term
  REAL, DIMENSION(1:NSOIL)                :: CI     !left-hand side term

  REAL                                    :: FFF    !runoff decay factor (m-1)
  REAL                                    :: RSBMX  !baseflow coefficient [mm/s]
  REAL                                    :: PDDUM  !infiltration rate at surface (m/s)
  REAL                                    :: FICE   !ice fraction in frozen soil
  REAL                                    :: WPLUS  !saturation excess of the total soil [m]
  REAL                                    :: RSAT   !accumulation of WPLUS (saturation excess) [m]
  REAL                                    :: SICEMAX!maximum soil ice content (m3/m3)
  REAL                                    :: SH2OMIN!minimum soil liquid water content (m3/m3)
  REAL                                    :: WTSUB  !sum of WCND(K)*DZSNSO(K)
  REAL                                    :: MH2O   !water mass removal (mm)
  REAL                                    :: FSAT   !fractional saturated area (-)
  REAL, DIMENSION(1:NSOIL)                :: MLIQ   !
  REAL                                    :: XS     !
  REAL                                    :: WATMIN !
  REAL                                    :: QDRAIN_SAVE !
  REAL                                    :: RUNSRF_SAVE !
  REAL                                    :: EPORE  !effective porosity [m3/m3]
  REAL, DIMENSION(1:NSOIL)                :: FCR    !impermeable fraction due to frozen soil
  INTEGER                                 :: NITER  !iteration times soil moisture (-)
  REAL                                    :: SMCTOT !2-m averaged soil moisture (m3/m3)
  REAL                                    :: DZTOT  !2-m soil depth (m)
  REAL, PARAMETER :: A = 4.0
! ----------------------------------------------------------------------
    RUNSRF = 0.0
    PDDUM  = 0.0
    RSAT   = 0.0

! for the case when snowmelt water is too large

    DO K = 1,NSOIL
       EPORE   = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - SICE(K) ) )
       RSAT    = RSAT + MAX(0.,SH2O(K)-EPORE)*DZSNSO(K)  
       SH2O(K) = MIN(EPORE,SH2O(K))             
    END DO

!impermeable fraction due to frozen soil

    DO K = 1,NSOIL
       FICE    = MIN(1.0,SICE(K)/parameters%SMCMAX(K))
       FCR(K)  = MAX(0.0,EXP(-A*(1.-FICE))- EXP(-A)) /  &
                        (1.0              - EXP(-A))
    END DO

! maximum soil ice content and minimum liquid water of all layers

    SICEMAX = 0.0
    FCRMAX  = 0.0
    SH2OMIN = parameters%SMCMAX(1)
    DO K = 1,NSOIL
       IF (SICE(K) > SICEMAX) SICEMAX = SICE(K)
       IF (FCR(K)  > FCRMAX)  FCRMAX  = FCR(K)
       IF (SH2O(K) < SH2OMIN) SH2OMIN = SH2O(K)
    END DO

!subsurface runoff for runoff scheme option 2

    IF(OPT_RUN == 2) THEN 
        FFF   = 2.0
        RSBMX = 4.0
        CALL ZWTEQ (parameters,NSOIL  ,NSNOW  ,ZSOIL  ,DZSNSO ,SH2O   ,ZWT)
        RUNSUB = (1.0-FCRMAX) * RSBMX * EXP(-parameters%TIMEAN) * EXP(-FFF*ZWT)   ! mm/s
    END IF

!surface runoff and infiltration rate using different schemes

!jref impermable surface at urban
    IF ( parameters%urban_flag ) FCR(1)= 0.95

    IF(OPT_RUN == 1) THEN
       FFF = 6.0
       FSAT   = parameters%FSATMX*EXP(-0.5*FFF*(ZWT-2.0))
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s 
       END IF
    END IF

    IF(OPT_RUN == 5) THEN
       FFF = 6.0
       FSAT   = parameters%FSATMX*EXP(-0.5*FFF*MAX(-2.0-ZWT,0.))
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s
       END IF
    END IF

    IF(OPT_RUN == 2) THEN
       FFF   = 2.0
       FSAT   = parameters%FSATMX*EXP(-0.5*FFF*ZWT)
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s 
       END IF
    END IF

    IF(OPT_RUN == 3) THEN
       CALL INFIL (parameters,NSOIL  ,DT     ,ZSOIL  ,SH2O   ,SICE   , & !in
                   SICEMAX,QINSUR ,                         & !in
                   PDDUM  ,RUNSRF )                           !out
    END IF

    IF(OPT_RUN == 4) THEN
       SMCTOT = 0.
       DZTOT  = 0.
       DO K = 1,NSOIL
          DZTOT   = DZTOT  + DZSNSO(K)  
          SMCTOT  = SMCTOT + SMC(K)/parameters%SMCMAX(K)*DZSNSO(K)
          IF(DZTOT >= 2.0) EXIT
       END DO
       SMCTOT = SMCTOT/DZTOT
       FSAT   = MAX(0.01,SMCTOT) ** 4.        !BATS

       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ((1.0-FCR(1))*FSAT+FCR(1))  
         PDDUM  = QINSUR - RUNSRF                       ! m/s
       END IF
    END IF

! determine iteration times and finer time step

    NITER = 1

!    IF(OPT_INF == 1) THEN    !OPT_INF =2 may cause water imbalance
       NITER = 3
       IF (PDDUM*DT>DZSNSO(1)*parameters%SMCMAX(1) ) THEN
          NITER = NITER*2
       END IF
!    END IF                 

    DTFINE  = DT / NITER

! solve soil moisture

    QDRAIN_SAVE = 0.0
    RUNSRF_SAVE = 0.0
    DO ITER = 1, NITER
       IF(QINSUR > 0. .and. OPT_RUN == 3) THEN
          CALL INFIL (parameters,NSOIL  ,DTFINE     ,ZSOIL  ,SH2O   ,SICE   , & !in
                      SICEMAX,QINSUR ,                         & !in
                      PDDUM  ,RUNSRF )                           !out
       END IF

       CALL SRT   (parameters,NSOIL  ,ZSOIL  ,DTFINE ,PDDUM  ,ETRANI , & !in
                   QSEVA  ,SH2O   ,SMC    ,ZWT    ,FCR    , & !in
                   SICEMAX,FCRMAX ,ILOC   ,JLOC   ,SMCWTD ,         & !in
                   RHSTT  ,AI     ,BI     ,CI     ,QDRAIN , & !out
                   WCND   )                                   !out
  
       CALL SSTEP (parameters,NSOIL  ,NSNOW  ,DTFINE ,ZSOIL  ,DZSNSO , & !in
                   SICE   ,ILOC   ,JLOC   ,ZWT            ,                 & !in
                   SH2O   ,SMC    ,AI     ,BI     ,CI     , & !inout
                   RHSTT  ,SMCWTD ,QDRAIN ,DEEPRECH,                                 & !inout
                   WPLUS)                                     !out
       RSAT =  RSAT + WPLUS
       QDRAIN_SAVE = QDRAIN_SAVE + QDRAIN
       RUNSRF_SAVE = RUNSRF_SAVE + RUNSRF
    END DO

    QDRAIN = QDRAIN_SAVE/NITER
    RUNSRF = RUNSRF_SAVE/NITER

    RUNSRF = RUNSRF * 1000. + RSAT * 1000./DT  ! m/s -> mm/s
    QDRAIN = QDRAIN * 1000.

!WRF_HYDRO_DJG...
!yw    INFXSRT = RUNSRF * DT   !mm/s -> mm

! removal of soil water due to groundwater flow (option 2)

    IF(OPT_RUN == 2) THEN
         WTSUB = 0.
         DO K = 1, NSOIL
           WTSUB = WTSUB + WCND(K)*DZSNSO(K)
         END DO

         DO K = 1, NSOIL
           MH2O    = RUNSUB*DT*(WCND(K)*DZSNSO(K))/WTSUB       ! mm
           SH2O(K) = SH2O(K) - MH2O/(DZSNSO(K)*1000.)
         END DO
    END IF

! Limit MLIQ to be greater than or equal to watmin.
! Get water needed to bring MLIQ equal WATMIN from lower layer.

   IF(OPT_RUN /= 1) THEN
      DO IZ = 1, NSOIL
         MLIQ(IZ) = SH2O(IZ)*DZSNSO(IZ)*1000.
      END DO

      WATMIN = 0.01           ! mm
      DO IZ = 1, NSOIL-1
          IF (MLIQ(IZ) .LT. 0.) THEN
             XS = WATMIN-MLIQ(IZ)
          ELSE
             XS = 0.
          END IF
          MLIQ(IZ  ) = MLIQ(IZ  ) + XS
          MLIQ(IZ+1) = MLIQ(IZ+1) - XS
      END DO

        IZ = NSOIL
        IF (MLIQ(IZ) .LT. WATMIN) THEN
           XS = WATMIN-MLIQ(IZ)
        ELSE
           XS = 0.
        END IF
        MLIQ(IZ) = MLIQ(IZ) + XS
        RUNSUB   = RUNSUB - XS/DT
        IF(OPT_RUN == 5)DEEPRECH = DEEPRECH - XS*1.E-3

      DO IZ = 1, NSOIL
        SH2O(IZ)     = MLIQ(IZ) / (DZSNSO(IZ)*1000.)
      END DO
   END IF

  END SUBROUTINE SOILWATER

!== begin zwteq ====================================================================================

  SUBROUTINE ZWTEQ (parameters,NSOIL  ,NSNOW  ,ZSOIL  ,DZSNSO ,SH2O   ,ZWT)
! ----------------------------------------------------------------------
! calculate equilibrium water table depth (Niu et al., 2005)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                         INTENT(IN) :: NSOIL  !no. of soil layers
  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers
  REAL, DIMENSION(1:NSOIL),        INTENT(IN) :: ZSOIL  !depth of soil layer-bottom [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer depth [m]
  REAL, DIMENSION(1:NSOIL),        INTENT(IN) :: SH2O   !soil liquid water content [m3/m3]

! output

  REAL,                           INTENT(OUT) :: ZWT    !water table depth [m]

! locals

  INTEGER :: K                      !do-loop index
  INTEGER, PARAMETER :: NFINE = 100 !no. of fine soil layers of 6m soil
  REAL    :: WD1                    !water deficit from coarse (4-L) soil moisture profile
  REAL    :: WD2                    !water deficit from fine (100-L) soil moisture profile
  REAL    :: DZFINE                 !layer thickness of the 100-L soil layers to 6.0 m
  REAL    :: TEMP                   !temporary variable
  REAL, DIMENSION(1:NFINE) :: ZFINE !layer-bottom depth of the 100-L soil layers to 6.0 m
! ----------------------------------------------------------------------

   WD1 = 0.
   DO K = 1,NSOIL
     WD1 = WD1 + (parameters%SMCMAX(1)-SH2O(K)) * DZSNSO(K) ! [m]
   ENDDO

   DZFINE = 3.0 * (-ZSOIL(NSOIL)) / NFINE  
   do K =1,NFINE
      ZFINE(K) = FLOAT(K) * DZFINE
   ENDDO

   ZWT = -3.*ZSOIL(NSOIL) - 0.001   ! initial value [m]

   WD2 = 0.
   DO K = 1,NFINE
     TEMP  = 1. + (ZWT-ZFINE(K))/parameters%PSISAT(1)
     WD2   = WD2 + parameters%SMCMAX(1)*(1.-TEMP**(-1./parameters%BEXP(1)))*DZFINE
     IF(ABS(WD2-WD1).LE.0.01) THEN
        ZWT = ZFINE(K)
        EXIT
     ENDIF
   ENDDO

  END SUBROUTINE ZWTEQ

!== begin infil ====================================================================================

  SUBROUTINE INFIL (parameters,NSOIL  ,DT     ,ZSOIL  ,SH2O   ,SICE   , & !in
                    SICEMAX,QINSUR ,                         & !in
                    PDDUM  ,RUNSRF )                           !out
! --------------------------------------------------------------------------------
! compute inflitration rate at soil surface and surface runoff
! --------------------------------------------------------------------------------
    IMPLICIT NONE
! --------------------------------------------------------------------------------
! inputs
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                  INTENT(IN) :: NSOIL  !no. of soil layers
  REAL,                     INTENT(IN) :: DT     !time step (sec)
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: ZSOIL  !depth of soil layer-bottom [m]
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SH2O   !soil liquid water content [m3/m3]
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SICE   !soil ice content [m3/m3]
  REAL,                     INTENT(IN) :: QINSUR !water input on soil surface [mm/s]
  REAL,                     INTENT(IN) :: SICEMAX!maximum soil ice content (m3/m3)

! outputs
  REAL,                    INTENT(OUT) :: RUNSRF !surface runoff [mm/s] 
  REAL,                    INTENT(OUT) :: PDDUM  !infiltration rate at surface

! locals
  INTEGER :: IALP1, J, JJ,  K
  REAL                     :: VAL
  REAL                     :: DDT
  REAL                     :: PX
  REAL                     :: DT1, DD, DICE
  REAL                     :: FCR
  REAL                     :: SUM
  REAL                     :: ACRT
  REAL                     :: WDF
  REAL                     :: WCND
  REAL                     :: SMCAV
  REAL                     :: INFMAX
  REAL, DIMENSION(1:NSOIL) :: DMAX
  INTEGER, PARAMETER       :: CVFRZ = 3
! --------------------------------------------------------------------------------

    IF (QINSUR >  0.0) THEN
       DT1 = DT /86400.
       SMCAV = parameters%SMCMAX(1) - parameters%SMCWLT(1)

! maximum infiltration rate

       DMAX(1)= -ZSOIL(1) * SMCAV
       DICE   = -ZSOIL(1) * SICE(1)
       DMAX(1)= DMAX(1)* (1.0-(SH2O(1) + SICE(1) - parameters%SMCWLT(1))/SMCAV)

       DD = DMAX(1)

       DO K = 2,NSOIL
          DICE    = DICE + (ZSOIL(K-1) - ZSOIL(K) ) * SICE(K)
          DMAX(K) = (ZSOIL(K-1) - ZSOIL(K)) * SMCAV
          DMAX(K) = DMAX(K) * (1.0-(SH2O(K) + SICE(K) - parameters%SMCWLT(K))/SMCAV)
          DD      = DD + DMAX(K)
       END DO

       VAL = (1. - EXP ( - parameters%KDT * DT1))
       DDT = DD * VAL
       PX  = MAX(0.,QINSUR * DT)
       INFMAX = (PX * (DDT / (PX + DDT)))/ DT

! impermeable fraction due to frozen soil

       FCR = 1.
       IF (DICE >  1.E-2) THEN
          ACRT = CVFRZ * parameters%FRZX / DICE
          SUM = 1.
          IALP1 = CVFRZ - 1
          DO J = 1,IALP1
             K = 1
             DO JJ = J +1,IALP1
                K = K * JJ
             END DO
             SUM = SUM + (ACRT ** (CVFRZ - J)) / FLOAT(K)
          END DO
          FCR = 1. - EXP (-ACRT) * SUM
       END IF

! correction of infiltration limitation

       INFMAX = INFMAX * FCR

! jref for urban areas
!       IF ( parameters%urban_flag ) INFMAX == INFMAX * 0.05

       CALL WDFCND2 (parameters,WDF,WCND,SH2O(1),SICEMAX,1)
       INFMAX = MAX (INFMAX,WCND)
       INFMAX = MIN (INFMAX,PX)

       RUNSRF= MAX(0., QINSUR - INFMAX)
       PDDUM = QINSUR - RUNSRF

    END IF

  END SUBROUTINE INFIL

!== begin srt ======================================================================================

  SUBROUTINE SRT (parameters,NSOIL  ,ZSOIL  ,DT     ,PDDUM  ,ETRANI , & !in
                  QSEVA  ,SH2O   ,SMC    ,ZWT    ,FCR    , & !in
                  SICEMAX,FCRMAX ,ILOC   ,JLOC   ,SMCWTD ,         & !in
                  RHSTT  ,AI     ,BI     ,CI     ,QDRAIN , & !out
                  WCND   )                                   !out
! ----------------------------------------------------------------------
! calculate the right hand side of the time tendency term of the soil
! water diffusion equation.  also to compute ( prepare ) the matrix
! coefficients for the tri-diagonal matrix of the implicit time scheme.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
!input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                  INTENT(IN)  :: ILOC   !grid index
    INTEGER,                  INTENT(IN)  :: JLOC   !grid index
    INTEGER,                  INTENT(IN)  :: NSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: ZSOIL
    REAL,                     INTENT(IN)  :: DT
    REAL,                     INTENT(IN)  :: PDDUM
    REAL,                     INTENT(IN)  :: QSEVA
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: ETRANI
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC
    REAL,                     INTENT(IN)  :: ZWT    ! water table depth [m]
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: FCR
    REAL, INTENT(IN)                      :: FCRMAX !maximum of FCR (-)
    REAL,                     INTENT(IN)  :: SICEMAX!maximum soil ice content (m3/m3)
    REAL,                     INTENT(IN)  :: SMCWTD !soil moisture between bottom of the soil and the water table

! output

    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: RHSTT
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: AI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: BI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: CI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: WCND    !hydraulic conductivity (m/s)
    REAL,                     INTENT(OUT) :: QDRAIN  !bottom drainage (m/s)

! local
    INTEGER                               :: K
    REAL, DIMENSION(1:NSOIL)              :: DDZ
    REAL, DIMENSION(1:NSOIL)              :: DENOM
    REAL, DIMENSION(1:NSOIL)              :: DSMDZ
    REAL, DIMENSION(1:NSOIL)              :: WFLUX
    REAL, DIMENSION(1:NSOIL)              :: WDF
    REAL, DIMENSION(1:NSOIL)              :: SMX
    REAL                                  :: TEMP1
    REAL                                  :: SMXWTD !soil moisture between bottom of the soil and water table
    REAL                                  :: SMXBOT  !soil moisture below bottom to calculate flux

! Niu and Yang (2006), J. of Hydrometeorology
! ----------------------------------------------------------------------

    IF(OPT_INF == 1) THEN
      DO K = 1, NSOIL
        CALL WDFCND1 (parameters,WDF(K),WCND(K),SMC(K),FCR(K),K)
        SMX(K) = SMC(K)
      END DO
        IF(OPT_RUN == 5)SMXWTD=SMCWTD
    END IF

    IF(OPT_INF == 2) THEN
      DO K = 1, NSOIL
        CALL WDFCND2 (parameters,WDF(K),WCND(K),SH2O(K),SICEMAX,K)
        SMX(K) = SH2O(K)
      END DO
          IF(OPT_RUN == 5)SMXWTD=SMCWTD*SH2O(NSOIL)/SMC(NSOIL)  !same liquid fraction as in the bottom layer
    END IF

    DO K = 1, NSOIL
       IF(K == 1) THEN
          DENOM(K) = - ZSOIL (K)
          TEMP1    = - ZSOIL (K+1)
          DDZ(K)   = 2.0 / TEMP1
          DSMDZ(K) = 2.0 * (SMX(K) - SMX(K+1)) / TEMP1
          WFLUX(K) = WDF(K) * DSMDZ(K) + WCND(K) - PDDUM + ETRANI(K) + QSEVA
       ELSE IF (K < NSOIL) THEN
          DENOM(k) = (ZSOIL(K-1) - ZSOIL(K))
          TEMP1    = (ZSOIL(K-1) - ZSOIL(K+1))
          DDZ(K)   = 2.0 / TEMP1
          DSMDZ(K) = 2.0 * (SMX(K) - SMX(K+1)) / TEMP1
          WFLUX(K) = WDF(K  ) * DSMDZ(K  ) + WCND(K  )         &
                   - WDF(K-1) * DSMDZ(K-1) - WCND(K-1) + ETRANI(K)
       ELSE
          DENOM(K) = (ZSOIL(K-1) - ZSOIL(K))
          IF(OPT_RUN == 1 .or. OPT_RUN == 2) THEN
             QDRAIN   = 0.
          END IF
          IF(OPT_RUN == 3) THEN
             QDRAIN   = parameters%SLOPE*WCND(K)
          END IF
          IF(OPT_RUN == 4) THEN
             QDRAIN   = (1.0-FCRMAX)*WCND(K)
          END IF
          IF(OPT_RUN == 5) THEN   !gmm new m-m&f water table dynamics formulation
             TEMP1    = 2.0 * DENOM(K)
             IF(ZWT < ZSOIL(NSOIL)-DENOM(NSOIL))THEN
!gmm interpolate from below, midway to the water table, to the middle of the auxiliary layer below the soil bottom
                SMXBOT = SMX(K) - (SMX(K)-SMXWTD) *  DENOM(K) * 2./ (DENOM(K) + ZSOIL(K) - ZWT)
             ELSE
                SMXBOT = SMXWTD
             ENDIF
             DSMDZ(K) = 2.0 * (SMX(K) - SMXBOT) / TEMP1
             QDRAIN   = WDF(K  ) * DSMDZ(K  ) + WCND(K  )
          END IF   
          WFLUX(K) = -(WDF(K-1)*DSMDZ(K-1))-WCND(K-1)+ETRANI(K) + QDRAIN
       END IF
    END DO

    DO K = 1, NSOIL
       IF(K == 1) THEN
          AI(K)    =   0.0
          BI(K)    =   WDF(K  ) * DDZ(K  ) / DENOM(K)
          CI(K)    = - BI (K)
       ELSE IF (K < NSOIL) THEN
          AI(K)    = - WDF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K)    = - WDF(K  ) * DDZ(K  ) / DENOM(K)
          BI(K)    = - ( AI (K) + CI (K) )
       ELSE
          AI(K)    = - WDF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K)    = 0.0
          BI(K)    = - ( AI (K) + CI (K) )
       END IF
          RHSTT(K) = WFLUX(K) / (-DENOM(K))
    END DO

! ----------------------------------------------------------------------
  END SUBROUTINE SRT

!== begin sstep ====================================================================================

  SUBROUTINE SSTEP (parameters,NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & !in
                    SICE   ,ILOC   ,JLOC   ,ZWT            ,                 & !in
                    SH2O   ,SMC    ,AI     ,BI     ,CI     , & !inout
                    RHSTT  ,SMCWTD ,QDRAIN ,DEEPRECH,                                 & !inout
                    WPLUS  )                                   !out

! ----------------------------------------------------------------------
! calculate/update soil moisture content values 
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
!input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                         INTENT(IN) :: ILOC   !grid index
    INTEGER,                         INTENT(IN) :: JLOC   !grid index
    INTEGER,                         INTENT(IN) :: NSOIL  !
    INTEGER,                         INTENT(IN) :: NSNOW  !
    REAL, INTENT(IN)                            :: DT
    REAL, INTENT(IN)                            :: ZWT
    REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL
    REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICE
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO ! snow/soil layer thickness [m]

!input and output
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: AI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: BI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: CI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: RHSTT
    REAL                    , INTENT(INOUT) :: SMCWTD
    REAL                    , INTENT(INOUT) :: QDRAIN
    REAL                    , INTENT(INOUT) :: DEEPRECH

!output
    REAL, INTENT(OUT)                       :: WPLUS     !saturation excess water (m)

!local
    INTEGER                                 :: K
    REAL, DIMENSION(1:NSOIL)                :: RHSTTIN
    REAL, DIMENSION(1:NSOIL)                :: CIIN
    REAL                                    :: STOT
    REAL                                    :: EPORE
    REAL                                    :: WMINUS
! ----------------------------------------------------------------------
    WPLUS = 0.0

    DO K = 1,NSOIL
       RHSTT (K) =   RHSTT(K) * DT
       AI (K)    =      AI(K) * DT
       BI (K)    = 1. + BI(K) * DT
       CI (K)    =      CI(K) * DT
    END DO

! copy values for input variables before calling rosr12

    DO K = 1,NSOIL
       RHSTTIN(k) = RHSTT(K)
       CIIN(k)    = CI(K)
    END DO

! call ROSR12 to solve the tri-diagonal matrix

    CALL ROSR12 (CI,AI,BI,CIIN,RHSTTIN,RHSTT,1,NSOIL,0)

    DO K = 1,NSOIL
        SH2O(K) = SH2O(K) + CI(K)
    ENDDO

!  excessive water above saturation in a layer is moved to
!  its unsaturated layer like in a bucket

!gmmwith opt_run=5 there is soil moisture below nsoil, to the water table
  IF(OPT_RUN == 5) THEN

!update smcwtd

     IF(ZWT < ZSOIL(NSOIL)-DZSNSO(NSOIL))THEN
!accumulate qdrain to update deep water table and soil moisture later
        DEEPRECH =  DEEPRECH + DT * QDRAIN
     ELSE
        SMCWTD = SMCWTD + DT * QDRAIN  / DZSNSO(NSOIL)
        WPLUS        = MAX((SMCWTD-parameters%SMCMAX(NSOIL)), 0.0) * DZSNSO(NSOIL)
        WMINUS       = MAX((1.E-4-SMCWTD), 0.0) * DZSNSO(NSOIL)

        SMCWTD = MAX( MIN(SMCWTD,parameters%SMCMAX(NSOIL)) , 1.E-4)
        SH2O(NSOIL)    = SH2O(NSOIL) + WPLUS/DZSNSO(NSOIL)

!reduce fluxes at the bottom boundaries accordingly
        QDRAIN = QDRAIN - WPLUS/DT
        DEEPRECH = DEEPRECH - WMINUS
     ENDIF

  ENDIF

    DO K = NSOIL,2,-1
      EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - SICE(K) ) )
      WPLUS        = MAX((SH2O(K)-EPORE), 0.0) * DZSNSO(K)
      SH2O(K)      = MIN(EPORE,SH2O(K))
      SH2O(K-1)    = SH2O(K-1) + WPLUS/DZSNSO(K-1)
    END DO

    EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(1) - SICE(1) ) )
    WPLUS        = MAX((SH2O(1)-EPORE), 0.0) * DZSNSO(1) 
    SH2O(1)      = MIN(EPORE,SH2O(1))

   IF(WPLUS > 0.0) THEN
    SH2O(2)      = SH2O(2) + WPLUS/DZSNSO(2)
    DO K = 2,NSOIL-1
      EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - SICE(K) ) )
      WPLUS        = MAX((SH2O(K)-EPORE), 0.0) * DZSNSO(K)
      SH2O(K)      = MIN(EPORE,SH2O(K))
      SH2O(K+1)    = SH2O(K+1) + WPLUS/DZSNSO(K+1)
    END DO

    EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(NSOIL) - SICE(NSOIL) ) )
    WPLUS        = MAX((SH2O(NSOIL)-EPORE), 0.0) * DZSNSO(NSOIL) 
    SH2O(NSOIL)  = MIN(EPORE,SH2O(NSOIL))
   END IF
   
    SMC = SH2O + SICE

  END SUBROUTINE SSTEP

!== begin wdfcnd1 ==================================================================================

  SUBROUTINE WDFCND1 (parameters,WDF,WCND,SMC,FCR,ISOIL)
! ----------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input 
  type (noahmp_parameters), intent(in) :: parameters
    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: FCR
    INTEGER,INTENT(IN)  :: ISOIL

! output
    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF

! local
    REAL :: EXPON
    REAL :: FACTR
    REAL :: VKWGT
! ----------------------------------------------------------------------

! soil water diffusivity

    FACTR = MAX(0.01, SMC/parameters%SMCMAX(ISOIL))
    EXPON = parameters%BEXP(ISOIL) + 2.0
    WDF   = parameters%DWSAT(ISOIL) * FACTR ** EXPON
    WDF   = WDF * (1.0 - FCR)

! hydraulic conductivity

    EXPON = 2.0*parameters%BEXP(ISOIL) + 3.0
    WCND  = parameters%DKSAT(ISOIL) * FACTR ** EXPON
    WCND  = WCND * (1.0 - FCR)

  END SUBROUTINE WDFCND1

!== begin wdfcnd2 ==================================================================================

  SUBROUTINE WDFCND2 (parameters,WDF,WCND,SMC,SICE,ISOIL)
! ----------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: SICE
    INTEGER,INTENT(IN)  :: ISOIL

! output
    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF

! local
    REAL :: EXPON
    REAL :: FACTR1,FACTR2
    REAL :: VKWGT
! ----------------------------------------------------------------------

! soil water diffusivity

    FACTR1 = 0.05/parameters%SMCMAX(ISOIL)
    FACTR2 = MAX(0.01, SMC/parameters%SMCMAX(ISOIL))
    FACTR1 = MIN(FACTR1,FACTR2)
    EXPON = parameters%BEXP(ISOIL) + 2.0
    WDF   = parameters%DWSAT(ISOIL) * FACTR2 ** EXPON

    IF (SICE > 0.0) THEN
    VKWGT = 1./ (1. + (500.* SICE)**3.)
    WDF   = VKWGT * WDF + (1.-VKWGT)*parameters%DWSAT(ISOIL)*(FACTR1)**EXPON
    END IF

! hydraulic conductivity

    EXPON = 2.0*parameters%BEXP(ISOIL) + 3.0
    WCND  = parameters%DKSAT(ISOIL) * FACTR2 ** EXPON

  END SUBROUTINE WDFCND2

  REAL                   :: LFCONVERT   !leaf to grain conversion ! Zhe Zhang 2020-07-13
  REAL                   :: STCONVERT   !stem to grain conversion [g/m2/s]
  REAL                   :: RTCONVERT   !root to grain conversion [g/m2/s]
! -------------------------- constants -------------------------------
  REAL                   :: BF       !parameter for present wood allocation [-]
  REAL                   :: RSWOODC  !wood respiration coeficient [1/s]
  REAL                   :: STOVRC   !stem turnover coefficient [1/s]
  REAL                   :: RSDRYC   !degree of drying that reduces soilrespiration [-]
  REAL                   :: RTOVRC   !root turnover coefficient [1/s]
  REAL                   :: WSTRC    !water stress coeficient [-]
  REAL                   :: LAIMIN   !minimum leaf area index [m2/m2]
  REAL                   :: XSAMIN   !minimum leaf area index [m2/m2]
  REAL                   :: SC
  REAL                   :: SD
  REAL                   :: VEGFRAC
  REAL                   :: TEMP

! Respiration as a function of temperature

  real :: r,x
          r(x) = exp(0.08*(x-298.16))
! ---------------------------------------------------------------------------------

! constants
    RSDRYC  = 40.0          !original was 40.0
    RSWOODC = 3.0E-10       !
    BF      = 0.90          !original was 0.90   ! carbon to roots
    WSTRC   = 100.0
    LAIMIN  = 0.05
    XSAMIN  = 0.05

    SAPM    = 3.*0.001      ! m2/kg -->m2/g
    LFMSMN  = laimin/0.035
    STMSMN  = xsamin/sapm
! ---------------------------------------------------------------------------------

! carbon assimilation
! 1 mole -> 12 g carbon or 44 g CO2 or 30 g CH20

     CARBFX     = PSN*12.e-6!*IPA   !umol co2 /m2/ s -> g/m2/s C
     CBHYDRAFX  = PSN*30.e-6!*IPA

! mainteinance respiration
     FNF     = MIN( FOLN/MAX(1.E-06,parameters%FOLN_MX), 1.0 )
     TF      = parameters%Q10MR**( (TV-298.16)/10. )
     RESP    = parameters%LFMR25 * TF * FNF * XLAI  * (1.-WSTRES)  ! umol/m2/s
     RSLEAF  = MIN((LFMASS-LFMSMN)/DT,RESP*30.e-6)                       ! g/m2/s
     RSROOT  = parameters%RTMR25*(RTMASS*1E-3)*TF * 30.e-6         ! g/m2/s
     RSSTEM  = parameters%STMR25*(STMASS*1E-3)*TF * 30.e-6         ! g/m2/s
     RSGRAIN = parameters%GRAINMR25*(GRAIN*1E-3)*TF * 30.e-6       ! g/m2/s

! calculate growth respiration for leaf, rtmass and grain

     GRLEAF  = MAX(0.0,parameters%FRA_GR*(parameters%LFPT(PGS)*CBHYDRAFX  - RSLEAF))
     GRSTEM  = MAX(0.0,parameters%FRA_GR*(parameters%STPT(PGS)*CBHYDRAFX  - RSSTEM))
     GRROOT  = MAX(0.0,parameters%FRA_GR*(parameters%RTPT(PGS)*CBHYDRAFX  - RSROOT))
     GRGRAIN = MAX(0.0,parameters%FRA_GR*(parameters%GRAINPT(PGS)*CBHYDRAFX  - RSGRAIN))

! leaf turnover, stem turnover, root turnover and leaf death caused by soil
! water and soil temperature stress

     LFTOVR  = parameters%LF_OVRC(PGS)*1.E-6*LFMASS
     RTTOVR  = parameters%RT_OVRC(PGS)*1.E-6*RTMASS
     STTOVR  = parameters%ST_OVRC(PGS)*1.E-6*STMASS
     SC  = EXP(-0.3*MAX(0.,TV-parameters%LEFREEZ)) * (LFMASS/120.)
     SD  = EXP((WSTRES-1.)*WSTRC)
     DIELF = LFMASS*1.E-6*(parameters%DILE_FW(PGS) * SD + parameters%DILE_FC(PGS)*SC)

! Allocation of CBHYDRAFX to leaf, stem, root and grain at each growth stage


     ADDNPPLF    = MAX(0.,parameters%LFPT(PGS)*CBHYDRAFX - GRLEAF-RSLEAF)
     ADDNPPLF    = parameters%LFPT(PGS)*CBHYDRAFX - GRLEAF-RSLEAF
     ADDNPPST    = MAX(0.,parameters%STPT(PGS)*CBHYDRAFX - GRSTEM-RSSTEM)
     ADDNPPST    = parameters%STPT(PGS)*CBHYDRAFX - GRSTEM-RSSTEM
    

! avoid reducing leaf mass below its minimum value but conserve mass

     LFDEL = (LFMASS - LFMSMN)/DT
     STDEL = (STMASS - STMSMN)/DT
     LFTOVR  = MIN(LFTOVR,LFDEL+ADDNPPLF)
     STTOVR  = MIN(STTOVR,STDEL+ADDNPPST)
     DIELF = MIN(DIELF,LFDEL+ADDNPPLF-LFTOVR)

! net primary productivities

     NPPL   = MAX(ADDNPPLF,-LFDEL)
     NPPL   = ADDNPPLF
     NPPS   = MAX(ADDNPPST,-STDEL)
     NPPS   = ADDNPPST
     NPPR   = parameters%RTPT(PGS)*CBHYDRAFX - RSROOT - GRROOT
     NPPG  =  parameters%GRAINPT(PGS)*CBHYDRAFX - RSGRAIN - GRGRAIN

! masses of plant components
  
     LFMASS = LFMASS + (NPPL-LFTOVR-DIELF)*DT
     STMASS = STMASS + (NPPS-STTOVR)*DT   ! g/m2
     RTMASS = RTMASS + (NPPR-RTTOVR)*DT
     GRAIN =  GRAIN + NPPG*DT 

     GPP = CBHYDRAFX* 0.4 !!g/m2/s C  0.4=12/30, CH20 to C

     LFCONVERT = 0.0 ! Zhe Zhang 2020-07-13
     STCONVERT = 0.0
     RTCONVERT = 0.0
     LFCONVERT = LFMASS*(parameters%LFCT(PGS)*DT/3600.0)
     STCONVERT = STMASS*(parameters%STCT(PGS)*DT/3600.0)
     RTCONVERT = RTMASS*(parameters%RTCT(PGS)*DT/3600.0)
     LFMASS = LFMASS - LFCONVERT
     STMASS = STMASS - STCONVERT
     RTMASS = RTMASS - RTCONVERT
     GRAIN  = GRAIN + STCONVERT + RTCONVERT + LFCONVERT
     !IF(PGS==6) THEN
     !  STCONVERT = STMASS*(0.00005*DT/3600.0)
     !  STMASS = STMASS - STCONVERT
     !  RTCONVERT = RTMASS*(0.0005*DT/3600.0)
     !  RTMASS = RTMASS - RTCONVERT
     !  GRAIN  = GRAIN + STCONVERT + RTCONVERT
     !END IF
    
     IF(RTMASS.LT.0.0) THEN
       RTTOVR = NPPR
       RTMASS = 0.0
     ENDIF

     IF(GRAIN.LT.0.0) THEN
       GRAIN = 0.0
     ENDIF

 ! soil carbon budgets

!     IF(PGS == 1 .OR. PGS == 2 .OR. PGS == 8) THEN
!       FASTCP=1000
!     ELSE
       FASTCP = FASTCP + (RTTOVR+LFTOVR+STTOVR+DIELF)*DT 
!     END IF
     FST = 2.0**( (STC-283.16)/10. )
     FSW = WROOT / (0.20+WROOT) * 0.23 / (0.23+WROOT)
     RSSOIL = FSW * FST * parameters%MRP* MAX(0.,FASTCP*1.E-3)*12.E-6

     STABLC = 0.1*RSSOIL
     FASTCP = FASTCP - (RSSOIL + STABLC)*DT
     STBLCP = STBLCP + STABLC*DT

!  total carbon flux

     CFLUX  = - CARBFX + RSLEAF + RSROOT  + RSSTEM &
              + RSSOIL + GRLEAF + GRROOT                  ! g/m2/s 0.4=12/30, CH20 to C

! for outputs
                                                                 !g/m2/s C

     NPP   = (NPPL + NPPS+ NPPR +NPPG)*0.4      !!g/m2/s C  0.4=12/30, CH20 to C
 
  
     AUTORS = RSROOT + RSGRAIN  + RSLEAF +  &                     !g/m2/s C
              GRLEAF + GRROOT + GRGRAIN                           !g/m2/s C

     HETERS = RSSOIL                                             !g/m2/s C
     NEE    = (AUTORS + HETERS - GPP)*44./30.                    !g/m2/s CO2
     TOTSC  = FASTCP + STBLCP                                    !g/m2   C

     TOTLB  = LFMASS + RTMASS + GRAIN         

! leaf area index and stem area index
  
     XLAI    = MAX(LFMASS*parameters%BIO2LAI,LAIMIN)
     XSAI    = MAX(STMASS*SAPM,XSAMIN)

   
!After harversting
!     IF(PGS == 8 ) THEN
!       LFMASS = 0.62
!       STMASS = 0
!       GRAIN  = 0
!     END IF

!    IF(PGS == 1 .OR. PGS == 2 .OR. PGS == 8) THEN
    IF(PGS == 8 .and. (GRAIN > 0. .or. LFMASS > 0 .or. STMASS > 0 .or. RTMASS > 0)) THEN
     XLAI   = 0.05
     XSAI   = 0.05
     LFMASS = LFMSMN
     STMASS = STMSMN
     RTMASS = 0
     GRAIN  = 0
    END IF 
    
END SUBROUTINE CO2FLUX_CROP

!== begin growing_gdd ==============================================================================

  SUBROUTINE GROWING_GDD (parameters,                         & !in
                          T2M ,   DT, JULIAN,                 & !in
                          GDD ,                               & !inout 
                          IPA,   IHA,     PGS)                  !out  
!===================================================================================================

! input

  type (noahmp_parameters), intent(in) :: parameters
   REAL                     , INTENT(IN)        :: T2M     !Air temperature
   REAL                     , INTENT(IN)        :: DT      !time step (s)
   REAL                     , INTENT(IN)        :: JULIAN  !Julian day of year (fractional) ( 0 <= JULIAN < YEARLEN )

! input and output

   REAL                     , INTENT(INOUT)     :: GDD     !growing degress days

! output

   INTEGER                  , INTENT(OUT)       :: IPA     !Planting index index(0=off, 1=on)
   INTEGER                  , INTENT(OUT)       :: IHA     !Havestindex(0=on,1=off) 
   INTEGER                  , INTENT(OUT)       :: PGS     !Plant growth stage(1=S1,2=S2,3=S3)

!local 

   REAL                                         :: GDDDAY    !gap bewtween GDD and GDD8
   REAL                                         :: DAYOFS2   !DAYS in stage2
   REAL                                         :: TDIFF     !temperature difference for growing degree days calculation
   REAL                                         :: TC

   TC = T2M - 273.15

!Havestindex(0=on,1=off) 

   IPA = 1
   IHA = 1

!turn on/off the planting 
 
   IF(JULIAN < parameters%PLTDAY)  IPA = 0

!turn on/off the harvesting
    IF(JULIAN >= parameters%HSDAY) IHA = 0
   
!Calculate the growing degree days
   
    IF(TC <  parameters%GDDTBASE) THEN
      TDIFF = 0.0
    ELSEIF(TC >= parameters%GDDTCUT) THEN
      TDIFF = parameters%GDDTCUT - parameters%GDDTBASE
    ELSE
      TDIFF = TC - parameters%GDDTBASE
    END IF

    GDD     = (GDD + TDIFF * DT / 86400.0) * IPA * IHA

    GDDDAY  = GDD

   ! Decide corn growth stage, based on Hybrid-Maize 
   !   PGS = 1 : Before planting
   !   PGS = 2 : from tassel initiation to silking
   !   PGS = 3 : from silking to effective grain filling
   !   PGS = 4 : from effective grain filling to pysiological maturity 
   !   PGS = 5 : GDDM=1389
   !   PGS = 6 :
   !   PGS = 7 :
   !   PGS = 8 :
   !  GDDM = 1389
   !  GDDM = 1555
   ! GDDSK = 0.41*GDDM +145.4+150 !from hybrid-maize 
   ! GDDS1 = ((GDDSK-96)/38.9-4)*21
   ! GDDS1 = 0.77*GDDSK
   ! GDDS3 = GDDSK+170
   ! GDDS3 = 170

   PGS = 1                         ! MB: set PGS = 1 (for initialization during growing season when no GDD)

   IF(GDDDAY > 0.0) PGS = 2

   IF(GDDDAY >= parameters%GDDS1)  PGS = 3

   IF(GDDDAY >= parameters%GDDS2)  PGS = 4 

   IF(GDDDAY >= parameters%GDDS3)  PGS = 5

   IF(GDDDAY >= parameters%GDDS4)  PGS = 6

   IF(GDDDAY >= parameters%GDDS5)  PGS = 7

   IF(JULIAN >= parameters%HSDAY)  PGS = 8
 
   IF(JULIAN <  parameters%PLTDAY) PGS = 1   

END SUBROUTINE GROWING_GDD

!== begin psn_crop =================================================================================

SUBROUTINE PSN_CROP ( parameters,       & !in
                      SOLDN, XLAI,T2M,  & !in
                      PSNCROP        )    !out
!===================================================================================================

! input

  type (noahmp_parameters), intent(in) :: parameters
  REAL     , INTENT(IN)    :: SOLDN    ! downward solar radiation
  REAL     , INTENT(IN)    :: XLAI     ! LAI
  REAL     , INTENT(IN)    :: T2M      ! air temp
  REAL     , INTENT(OUT)   :: PSNCROP  !

!local

  REAL                     :: PAR      ! photosynthetically active radiation (w/m2) 1 W m-2 = 0.0864 MJ m-2 day-1
  REAL                     :: Amax     ! Maximum CO2 assimulation rate g/co2/s  
  REAL                     :: L1       ! Three Gaussian method
  REAL                     :: L2       ! Three Gaussian method
  REAL                     :: L3       ! Three Gaussian method
  REAL                     :: I1       ! Three Gaussian method
  REAL                     :: I2       ! Three Gaussian method
  REAL                     :: I3       ! Three Gaussian method
  REAL                     :: A1       ! Three Gaussian method
  REAL                     :: A2       ! Three Gaussian method
  REAL                     :: A3       ! Three Gaussian method
  REAL                     :: A        ! CO2 Assimulation 
  REAL                     :: TC

  TC = T2M - 273.15

  PAR = parameters%I2PAR * SOLDN * 0.0036  !w to MJ m-2

  IF(TC < parameters%TASSIM0) THEN
    Amax = 1E-10
  ELSEIF(TC >= parameters%TASSIM0 .and. TC < parameters%TASSIM1) THEN
    Amax = (TC - parameters%TASSIM0) * parameters%Aref / (parameters%TASSIM1 - parameters%TASSIM0)
  ELSEIF(TC >= parameters%TASSIM1 .and. TC < parameters%TASSIM2) THEN
    Amax = parameters%Aref
  ELSE
    Amax= parameters%Aref - 0.2 * (T2M - parameters%TASSIM2)
  ENDIF 
  
  Amax = max(amax,0.01)

  IF(XLAI <= 0.05) THEN
    L1 = 0.1127 * 0.05   !use initial LAI(0.05), avoid error
    L2 = 0.5    * 0.05
    L3 = 0.8873 * 0.05
  ELSE
    L1 = 0.1127 * XLAI
    L2 = 0.5    * XLAI
    L3 = 0.8873 * XLAI
  END IF

  I1 = parameters%k * PAR * exp(-parameters%k * L1)
  I2 = parameters%k * PAR * exp(-parameters%k * L2)
  I3 = parameters%k * PAR * exp(-parameters%k * L3)

  I1 = max(I1,1E-10)
  I2 = max(I2,1E-10)
  I3 = max(I3,1E-10)

  A1 = Amax * (1 - exp(-parameters%epsi * I1 / Amax))
  A2 = Amax * (1 - exp(-parameters%epsi * I2 / Amax)) * 1.6
  A3 = Amax * (1 - exp(-parameters%epsi * I3 / Amax))

  IF (XLAI <= 0.05) THEN
    A  = (A1+A2+A3) / 3.6 * 0.05
  ELSEIF (XLAI > 0.05 .and. XLAI <= 4.0) THEN
    A  = (A1+A2+A3) / 3.6 * XLAI
  ELSE
    A = (A1+A2+A3) / 3.6 * 4
  END IF

  A = A * parameters%PSNRF ! Attainable 

  PSNCROP = 6.313 * A   ! (1/44) * 1000000)/3600 = 6.313

END SUBROUTINE PSN_CROP

!== begin bvocflux =================================================================================

!  SUBROUTINE BVOCFLUX(parameters,VOCFLX,  VEGTYP,  VEGFRAC,  APAR,   TV )
!
! ------------------------------------------------------------------------------------------
!      implicit none
! ------------------------------------------------------------------------------------------
!
! ------------------------ code history ---------------------------
! source file:       BVOC
! purpose:           BVOC emissions
! DESCRIPTION:
! Volatile organic compound emission 
! This code simulates volatile organic compound emissions
! following the algorithm presented in Guenther, A., 1999: Modeling
! Biogenic Volatile Organic Compound Emissions to the Atmosphere. In
! Reactive Hydrocarbons in the Atmosphere, Ch. 3
! This model relies on the assumption that 90% of isoprene and monoterpene
! emissions originate from canopy foliage:
!    E = epsilon * gamma * density * delta
! The factor delta (longterm activity factor) applies to isoprene emission
! from deciduous plants only. We neglect this factor at the present time.
! This factor is discussed in Guenther (1997).
! Subroutine written to operate at the patch level.
! IN FINAL IMPLEMENTATION, REMEMBER:
! 1. may wish to call this routine only as freq. as rad. calculations
! 2. may wish to place epsilon values directly in pft-physiology file
! ------------------------ input/output variables -----------------
! input
!  integer                     ,INTENT(IN) :: vegtyp  !vegetation type 
!  real                        ,INTENT(IN) :: vegfrac !green vegetation fraction [0.0-1.0]
!  real                        ,INTENT(IN) :: apar    !photosynthesis active energy by canopy (w/m2)
!  real                        ,INTENT(IN) :: tv      !vegetation canopy temperature (k)
!
! output
!  real                        ,INTENT(OUT) :: vocflx(5) ! voc fluxes [ug C m-2 h-1]
!
! Local Variables
!
!  real, parameter :: R      = 8.314    ! univ. gas constant [J K-1 mol-1]
!  real, parameter :: alpha  = 0.0027   ! empirical coefficient
!  real, parameter :: cl1    = 1.066    ! empirical coefficient
!  real, parameter :: ct1    = 95000.0  ! empirical coefficient [J mol-1]
!  real, parameter :: ct2    = 230000.0 ! empirical coefficient [J mol-1]
!  real, parameter :: ct3    = 0.961    ! empirical coefficient
!  real, parameter :: tm     = 314.0    ! empirical coefficient [K]
!  real, parameter :: tstd   = 303.0    ! std temperature [K]
!  real, parameter :: bet    = 0.09     ! beta empirical coefficient [K-1]
!
!  integer ivoc        ! do-loop index
!  integer ityp        ! do-loop index
!  real epsilon(5)
!  real gamma(5)
!  real density
!  real elai
!  real par,cl,reciprod,ct
!
! epsilon :
!
!    do ivoc = 1, 5
!    epsilon(ivoc) = parameters%eps(VEGTYP,ivoc)
!    end do
!
! gamma : Activity factor. Units [dimensionless]
!
!      reciprod = 1. / (R * tv * tstd)
!      ct = exp(ct1 * (tv - tstd) * reciprod) / &
!           (ct3 + exp(ct2 * (tv - tm) * reciprod))
!
!      par = apar * 4.6 ! (multiply w/m2 by 4.6 to get umol/m2/s)
!      cl  = alpha * cl1 * par * (1. + alpha * alpha * par * par)**(-0.5)
!
!   gamma(1) = cl * ct ! for isoprenes
!
!   do ivoc = 2, 5
!   gamma(ivoc) = exp(bet * (tv - tstd))
!   end do
!
! Foliage density
!
! transform vegfrac to lai      
!
!   elai    = max(0.0,-6.5/2.5*alog((1.-vegfrac)))
!   density = elai / (parameters%slarea(VEGTYP) * 0.5)
!
! calculate the voc flux
!
!   do ivoc = 1, 5
!   vocflx(ivoc) = epsilon(ivoc) * gamma(ivoc) * density
!   end do
!
!   end subroutine bvocflux
! ==================================================================================================

!***************** SUBROUTINES FOR GECROS CROP SIMULATION ***************
!*----------------------------------------------------------------------*
!*  SUBROUTINE EMERG                                                    *
!*  Purpose: This subroutine calculates germination and emergence of    *
!*  the crop                                                            *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*                                                                      *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  nowdate C12 Actual date                                dd.mm.yy  I  *
!*  DT      R4  Time step of integration                     s       I  *
!*  DD      R4  Drilling depth                               cm      I  *
!*  TSOIL   R4  Soil temperature in first layer              K       I  *
!*  TBEM    R4  Temperature threshold                        oC      I  *
!*  EMA     R4  Intercept of function for emergence          oC      I  *
!*  EMB     R4  Slope of function for emergence              oC      I  *
!*  TTEM    R4  Cumulative temperature sum for emergence     oC     I/O *
!*  EMERGENCE LOG Flag for emergence                          -     I/O *  
!*  STATE_GECROS(41) = emerged yes/no                                   *
!*  STATE_GECROS(43) = TTEM                                             *
!*----------------------------------------------------------------------*

SUBROUTINE EMERG(DT, TSOIL, DD, TBEM, EMA, EMB, STATE_GECROS)
      IMPLICIT NONE
      REAL, INTENT(IN) :: DT, TSOIL, DD, TBEM, EMA, EMB
      REAL, DIMENSION(1:60), INTENT(INOUT) :: STATE_GECROS
      REAL :: EMTH, TINT
      SAVE
      
      IF ((TSOIL-273.15).LT.TBEM) THEN
      ELSE
           STATE_GECROS(43) = STATE_GECROS(43) + (TSOIL-273.15-TBEM)/(86400./DT)
      ENDIF
      
      EMTH = EMA + EMB*DD
           
      IF (STATE_GECROS(43).GT.EMTH) THEN
         STATE_GECROS(41)=1.
!         write(*,*) 'Crop emerged on ', nowdate
!		 read(*,*)
      ELSE
         STATE_GECROS(41)=-1.
      ENDIF
      
      RETURN
END SUBROUTINE EMERG

! ********************************* end of carbon subroutines *****************************
! ==================================================================================================

!== begin noahmp_options ===========================================================================

  subroutine noahmp_options(idveg     ,iopt_crs  ,iopt_btr  ,iopt_run  ,iopt_sfc  ,iopt_frz , & 
                             iopt_inf  ,iopt_rad  ,iopt_alb  ,iopt_snf  ,iopt_tbot, iopt_stc, &
			     iopt_rsf , iopt_soil, iopt_pedo, iopt_crop, iopt_irr, iopt_irrm)

  implicit none

  INTEGER,  INTENT(IN) :: idveg     !dynamic vegetation (1 -> off ; 2 -> on) with opt_crs = 1
  INTEGER,  INTENT(IN) :: iopt_crs  !canopy stomatal resistance (1-> Ball-Berry; 2->Jarvis)
  INTEGER,  INTENT(IN) :: iopt_btr  !soil moisture factor for stomatal resistance (1-> Noah; 2-> CLM; 3-> SSiB)
  INTEGER,  INTENT(IN) :: iopt_run  !runoff and groundwater (1->SIMGM; 2->SIMTOP; 3->Schaake96; 4->BATS)
  INTEGER,  INTENT(IN) :: iopt_sfc  !surface layer drag coeff (CH & CM) (1->M-O; 2->Chen97)
  INTEGER,  INTENT(IN) :: iopt_frz  !supercooled liquid water (1-> NY06; 2->Koren99)
  INTEGER,  INTENT(IN) :: iopt_inf  !frozen soil permeability (1-> NY06; 2->Koren99)
  INTEGER,  INTENT(IN) :: iopt_rad  !radiation transfer (1->gap=F(3D,cosz); 2->gap=0; 3->gap=1-Fveg)
  INTEGER,  INTENT(IN) :: iopt_alb  !snow surface albedo (1->BATS; 2->CLASS)
  INTEGER,  INTENT(IN) :: iopt_snf  !rainfall & snowfall (1-Jordan91; 2->BATS; 3->Noah)
  INTEGER,  INTENT(IN) :: iopt_tbot !lower boundary of soil temperature (1->zero-flux; 2->Noah)

  INTEGER,  INTENT(IN) :: iopt_stc  !snow/soil temperature time scheme (only layer 1)
                                    ! 1 -> semi-implicit; 2 -> full implicit (original Noah)
  INTEGER,  INTENT(IN) :: iopt_rsf  !surface resistance (1->Sakaguchi/Zeng; 2->Seller; 3->mod Sellers; 4->1+snow)
  INTEGER,  INTENT(IN) :: iopt_soil !soil parameters set-up option
  INTEGER,  INTENT(IN) :: iopt_pedo !pedo-transfer function (1->Saxton and Rawls)
  INTEGER,  INTENT(IN) :: iopt_crop !crop model option (0->none; 1->Liu et al.; 2->Gecros)
  INTEGER,  INTENT(IN) :: iopt_irr  ! 0 -> No irrigation; 
                                    ! 1 -> Irrigation ON;
                                    ! 2 -> irrigation trigger based on crop season Planting and harvesting dates; 
                                    ! 3 -> irrigation trigger based on LAI threshold
  INTEGER,  INTENT(IN) :: iopt_irrm ! 0 -> all methods ON based on geo_em inputs
                                    ! 1 -> sprinkler ON
                                    ! 2 -> micro/drip ON
                                    ! 3 -> flood irrigation ON
! -------------------------------------------------------------------------------------------------

  dveg = idveg
  
  opt_crs  = iopt_crs  
  opt_btr  = iopt_btr  
  opt_run  = iopt_run  
  opt_sfc  = iopt_sfc  
  opt_frz  = iopt_frz  
  opt_inf  = iopt_inf  
  opt_rad  = iopt_rad  
  opt_alb  = iopt_alb  
  opt_snf  = iopt_snf  
  opt_tbot = iopt_tbot 
  opt_stc  = iopt_stc
  opt_rsf  = iopt_rsf
  opt_soil = iopt_soil
  opt_pedo = iopt_pedo
  opt_crop = iopt_crop
    INTEGER :: LCZ_1_TABLE
    INTEGER :: LCZ_2_TABLE
    INTEGER :: LCZ_3_TABLE
    INTEGER :: LCZ_4_TABLE
    INTEGER :: LCZ_5_TABLE
    INTEGER :: LCZ_6_TABLE
    INTEGER :: LCZ_7_TABLE
    INTEGER :: LCZ_8_TABLE
    INTEGER :: LCZ_9_TABLE
    INTEGER :: LCZ_10_TABLE
    INTEGER :: LCZ_11_TABLE

    REAL :: CH2OP_TABLE(MVT)       !maximum intercepted h2o per unit lai+sai (mm)
    REAL :: DLEAF_TABLE(MVT)       !characteristic leaf dimension (m)
    REAL :: Z0MVT_TABLE(MVT)       !momentum roughness length (m)
    REAL :: HVT_TABLE(MVT)         !top of canopy (m)
    REAL :: HVB_TABLE(MVT)         !bottom of canopy (m)
    REAL :: DEN_TABLE(MVT)         !tree density (no. of trunks per m2)
    REAL :: RC_TABLE(MVT)          !tree crown radius (m)
    REAL :: MFSNO_TABLE(MVT)       !snowmelt curve parameter ()
    REAL :: SNOW_RET_FAC_TABLE  !snowpack water release timescale factor (1/s)
    REAL :: SNOW_EMIS_TABLE!snow emissivity
    REAL :: SWEMX_TABLE    !new snow mass to fully cover old snow (mm)
    REAL :: TAU0_TABLE          !tau0 from Yang97 eqn. 10a
    REAL :: GRAIN_GROWTH_TABLE  !growth from vapor diffusion Yang97 eqn. 10b
    REAL :: EXTRA_GROWTH_TABLE  !extra growth near freezing Yang97 eqn. 10c
    REAL :: DIRT_SOOT_TABLE     !dirt and soot term Yang97 eqn. 10d
    REAL :: BATS_COSZ_TABLE     !zenith angle snow albedo adjustment; b in Yang97 eqn. 15
    REAL :: BATS_VIS_NEW_TABLE  !new snow visible albedo
    REAL :: BATS_NIR_NEW_TABLE  !new snow NIR albedo
    REAL :: BATS_VIS_AGE_TABLE  !age factor for diffuse visible snow albedo Yang97 eqn. 17
    REAL :: BATS_NIR_AGE_TABLE  !age factor for diffuse NIR snow albedo Yang97 eqn. 18
    REAL :: BATS_VIS_DIR_TABLE  !cosz factor for direct visible snow albedo Yang97 eqn. 15
    REAL :: BATS_NIR_DIR_TABLE  !cosz factor for direct NIR snow albedo Yang97 eqn. 16
    REAL :: RSURF_SNOW_TABLE    !surface resistance for snow(s/m)
    REAL :: RSURF_EXP_TABLE    !exponent in the shape parameter for soil resistance option 1

    REAL :: C3PSNI_TABLE(NCROP)       !photosynthetic pathway: 0. = c4, 1. = c3 ! Zhe Zhang 2020-07-03
    REAL :: KC25I_TABLE(NCROP)        !co2 michaelis-menten constant at 25c (pa)
    REAL :: AKCI_TABLE(NCROP)         !q10 for kc25
    REAL :: KO25I_TABLE(NCROP)        !o2 michaelis-menten constant at 25c (pa)
    REAL :: AKOI_TABLE(NCROP)         !q10 for ko25
    REAL :: VCMX25I_TABLE(NCROP)      !maximum rate of carboxylation at 25c (umol co2/m**2/s)
    REAL :: AVCMXI_TABLE(NCROP)       !q10 for vcmx25
    REAL :: BPI_TABLE(NCROP)          !minimum leaf conductance (umol/m**2/s)
    REAL :: MPI_TABLE(NCROP)          !slope of conductance-to-photosynthesis relationship
    REAL :: QE25I_TABLE(NCROP)        !quantum efficiency at 25c (umol co2 / umol photon)
    REAL :: FOLNMXI_TABLE(NCROP)      !foliage nitrogen concentration when

 INTEGER :: C3C4_TABLE(NCROP)           ! photosynthetic pathway:  1. = c3 2. = c4
    REAL :: AREF_TABLE(NCROP)           ! reference maximum CO2 assimulation rate 
    REAL :: PSNRF_TABLE(NCROP)          ! CO2 assimulation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
    REAL :: I2PAR_TABLE(NCROP)          ! Fraction of incoming solar radiation to photosynthetically active radiation
    REAL :: TASSIM0_TABLE(NCROP)        ! Minimum temperature for CO2 assimulation [C]
    REAL :: TASSIM1_TABLE(NCROP)        ! CO2 assimulation linearly increasing until temperature reaches T1 [C]
    REAL :: TASSIM2_TABLE(NCROP)        ! CO2 assmilation rate remain at Aref until temperature reaches T2 [C]
    REAL :: K_TABLE(NCROP)              ! light extinction coefficient
    REAL :: EPSI_TABLE(NCROP)           ! initial light use efficiency

    REAL :: Q10MR_TABLE(NCROP)          ! q10 for maintainance respiration
    REAL :: FOLN_MX_TABLE(NCROP)        ! foliage nitrogen concentration when f(n)=1 (%)
    REAL :: LEFREEZ_TABLE(NCROP)        ! characteristic T for leaf freezing [K]

    REAL :: DILE_FC_TABLE(NCROP,NSTAGE) ! coeficient for temperature leaf stress death [1/s]
    REAL :: DILE_FW_TABLE(NCROP,NSTAGE) ! coeficient for water leaf stress death [1/s]
    REAL :: FRA_GR_TABLE(NCROP)         ! fraction of growth respiration

    REAL :: LF_OVRC_TABLE(NCROP,NSTAGE) ! fraction of leaf turnover  [1/s]
    REAL :: ST_OVRC_TABLE(NCROP,NSTAGE) ! fraction of stem turnover  [1/s]
    REAL :: RT_OVRC_TABLE(NCROP,NSTAGE) ! fraction of root tunrover  [1/s]
    REAL :: LFMR25_TABLE(NCROP)         !  leaf maintenance respiration at 25C [umol CO2/m**2  /s]
    REAL :: STMR25_TABLE(NCROP)         !  stem maintenance respiration at 25C [umol CO2/kg bio/s]
    REAL :: RTMR25_TABLE(NCROP)         !  root maintenance respiration at 25C [umol CO2/kg bio/s]
    REAL :: GRAINMR25_TABLE(NCROP)      ! grain maintenance respiration at 25C [umol CO2/kg bio/s]

    REAL :: LFPT_TABLE(NCROP,NSTAGE)    ! fraction of carbohydrate flux to leaf
    REAL :: STPT_TABLE(NCROP,NSTAGE)    ! fraction of carbohydrate flux to stem
    REAL :: RTPT_TABLE(NCROP,NSTAGE)    ! fraction of carbohydrate flux to root
    REAL :: GRAINPT_TABLE(NCROP,NSTAGE) ! fraction of carbohydrate flux to grain
    INTEGER :: LCZ_1
    INTEGER :: LCZ_2
    INTEGER :: LCZ_3
    INTEGER :: LCZ_4
    INTEGER :: LCZ_5
    INTEGER :: LCZ_6
    INTEGER :: LCZ_7
    INTEGER :: LCZ_8
    INTEGER :: LCZ_9
    INTEGER :: LCZ_10
    INTEGER :: LCZ_11

    REAL, DIMENSION(MVT) :: SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN, &
                                     SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC
    REAL, DIMENSION(MVT) :: LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN, &
                                     LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC
    REAL, DIMENSION(MVT) :: RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, &
                                     TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR
    REAL, DIMENSION(MVT) :: CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, &
                     AVCMX, AQE, LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  &
                     BP, MP, QE25, RMS25, RMR25, ARM, FOLNMX, WDPOOL, WRRAT, MRP, NROOT, RGL, RS, HS, TOPT, RSMAX, &
		     SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5
			     
    NAMELIST / noahmp_usgs_veg_categories / VEG_DATASET_DESCRIPTION, NVEG
    NAMELIST / noahmp_usgs_parameters / ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL, &
         LCZ_1,LCZ_2,LCZ_3,LCZ_4,LCZ_5,LCZ_6,LCZ_7,LCZ_8,LCZ_9,LCZ_10,LCZ_11,&
         CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
         LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
         FOLNMX, WDPOOL, WRRAT, MRP, NROOT, RGL, RS, HS, TOPT, RSMAX, &
         SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY, SAI_JUN,SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC, &
         LAI_JAN, LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN,LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC, &
         RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR, SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5
	 
    NAMELIST / noahmp_modis_veg_categories / VEG_DATASET_DESCRIPTION, NVEG
    NAMELIST / noahmp_modis_parameters / ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL, &
         LCZ_1,LCZ_2,LCZ_3,LCZ_4,LCZ_5,LCZ_6,LCZ_7,LCZ_8,LCZ_9,LCZ_10,LCZ_11, &
         CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
         LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
         FOLNMX, WDPOOL, WRRAT, MRP, NROOT, RGL, RS, HS, TOPT, RSMAX, &
         SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY, SAI_JUN,SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC, &
         LAI_JAN, LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN,LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC, &
         RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR, SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5

    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
    CH2OP_TABLE  = -1.E36
    DLEAF_TABLE  = -1.E36
    Z0MVT_TABLE  = -1.E36
    HVT_TABLE    = -1.E36
    HVB_TABLE    = -1.E36
    DEN_TABLE    = -1.E36
    RC_TABLE     = -1.E36
    MFSNO_TABLE  = -1.E36
    LCZ_1_TABLE   = -99999
    LCZ_2_TABLE   = -99999
    LCZ_3_TABLE   = -99999
    LCZ_4_TABLE   = -99999
    LCZ_5_TABLE   = -99999
    LCZ_6_TABLE   = -99999
    LCZ_7_TABLE   = -99999
    LCZ_8_TABLE   = -99999
    LCZ_9_TABLE   = -99999
    LCZ_10_TABLE   = -99999
    LCZ_11_TABLE   = -99999

    inquire( file='MPTABLE.TBL', exist=file_named ) 
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file MPTABLE.TBL")')
       call wrf_error_fatal("STOP in Noah-MP read_mp_veg_parameters")
    endif

    if ( trim(DATASET_IDENTIFIER) == "USGS" ) then
       read(15,noahmp_usgs_veg_categories)
       read(15,noahmp_usgs_parameters)
    else if ( trim(DATASET_IDENTIFIER) == "MODIFIED_IGBP_MODIS_NOAH" ) then
       read(15,noahmp_modis_veg_categories)
       read(15,noahmp_modis_parameters)
    else
       write(*,'("WARNING: Unrecognized DATASET_IDENTIFIER in subroutine READ_MP_VEG_PARAMETERS")')
       write(*,'("WARNING: DATASET_IDENTIFIER = ''", A, "''")') trim(DATASET_IDENTIFIER)
       call wrf_error_fatal("STOP in Noah-MP read_mp_veg_parameters")
    endif
    close(15)

                      ISURBAN_TABLE   = ISURBAN
                      ISWATER_TABLE   = ISWATER
                     ISBARREN_TABLE   = ISBARREN
                        ISICE_TABLE   = ISICE
                       ISCROP_TABLE   = ISCROP
                    EBLFOREST_TABLE   = EBLFOREST
                      NATURAL_TABLE   = NATURAL
                        LCZ_1_TABLE   = LCZ_1
                        LCZ_2_TABLE   = LCZ_2
                        LCZ_3_TABLE   = LCZ_3
                        LCZ_4_TABLE   = LCZ_4
                        LCZ_5_TABLE   = LCZ_5
                        LCZ_6_TABLE   = LCZ_6
                        LCZ_7_TABLE   = LCZ_7
                        LCZ_8_TABLE   = LCZ_8
                        LCZ_9_TABLE   = LCZ_9
                        LCZ_10_TABLE  = LCZ_10
                        LCZ_11_TABLE  = LCZ_11

     CH2OP_TABLE(1:NVEG)  = CH2OP(1:NVEG)
     DLEAF_TABLE(1:NVEG)  = DLEAF(1:NVEG)
     Z0MVT_TABLE(1:NVEG)  = Z0MVT(1:NVEG)
       HVT_TABLE(1:NVEG)  = HVT(1:NVEG)
       HVB_TABLE(1:NVEG)  = HVB(1:NVEG)
       DEN_TABLE(1:NVEG)  = DEN(1:NVEG)
        RC_TABLE(1:NVEG)  = RC(1:NVEG)
     MFSNO_TABLE(1:NVEG)  = MFSNO(1:NVEG)
    REAL :: CO2,O2,TIMEAN,FSATMX,Z0SNO,SSI,SNOW_RET_FAC,SNOW_EMIS,&
            SWEMX,TAU0,GRAIN_GROWTH,EXTRA_GROWTH,DIRT_SOOT,&
	    BATS_COSZ,BATS_VIS_NEW,BATS_NIR_NEW,BATS_VIS_AGE,BATS_NIR_AGE,BATS_VIS_DIR,BATS_NIR_DIR,&
	    RSURF_SNOW,RSURF_EXP

    NAMELIST / noahmp_global_parameters / CO2,O2,TIMEAN,FSATMX,Z0SNO,SSI,SNOW_RET_FAC,SNOW_EMIS,&
            SWEMX,TAU0,GRAIN_GROWTH,EXTRA_GROWTH,DIRT_SOOT,&
	    BATS_COSZ,BATS_VIS_NEW,BATS_NIR_NEW,BATS_VIS_AGE,BATS_NIR_AGE,BATS_VIS_DIR,BATS_NIR_DIR,&
	    RSURF_SNOW,RSURF_EXP


    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
       CO2_TABLE     = -1.E36
        O2_TABLE     = -1.E36
    TIMEAN_TABLE     = -1.E36
    FSATMX_TABLE     = -1.E36
     Z0SNO_TABLE     = -1.E36
       SSI_TABLE     = -1.E36
SNOW_RET_FAC_TABLE   = -1.E36
   SNOW_EMIS_TABLE   = -1.E36
       SWEMX_TABLE   = -1.E36
        TAU0_TABLE   = -1.E36
GRAIN_GROWTH_TABLE   = -1.E36
EXTRA_GROWTH_TABLE   = -1.E36
   DIRT_SOOT_TABLE   = -1.E36
   BATS_COSZ_TABLE   = -1.E36
BATS_VIS_NEW_TABLE   = -1.E36
BATS_NIR_NEW_TABLE   = -1.E36
BATS_VIS_AGE_TABLE   = -1.E36
BATS_NIR_AGE_TABLE   = -1.E36
BATS_VIS_DIR_TABLE   = -1.E36
BATS_NIR_DIR_TABLE   = -1.E36
RSURF_SNOW_TABLE     = -1.E36
 RSURF_EXP_TABLE     = -1.E36

    inquire( file='MPTABLE.TBL', exist=file_named ) 
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file MPTABLE.TBL")')
       call wrf_error_fatal("STOP in Noah-MP read_mp_global_parameters")
    endif

    read(15,noahmp_global_parameters)
    close(15)

       CO2_TABLE     = CO2
        O2_TABLE     = O2
    TIMEAN_TABLE     = TIMEAN
    FSATMX_TABLE     = FSATMX
     Z0SNO_TABLE     = Z0SNO
       SSI_TABLE     = SSI
    REAL, DIMENSION(NCROP) :: C3PSN   ! this session copied from stomata parameters Zhe Zhang 2020-07-13
    REAL, DIMENSION(NCROP) :: KC25
    REAL, DIMENSION(NCROP) :: AKC
    REAL, DIMENSION(NCROP) :: KO25
    REAL, DIMENSION(NCROP) :: AKO
    REAL, DIMENSION(NCROP) :: AVCMX
    REAL, DIMENSION(NCROP) :: VCMX25
    REAL, DIMENSION(NCROP) :: BP
    REAL, DIMENSION(NCROP) :: MP
    REAL, DIMENSION(NCROP) :: FOLNMX
    REAL, DIMENSION(NCROP) :: QE25    ! until here
 INTEGER, DIMENSION(NCROP) :: C3C4
    REAL, DIMENSION(NCROP) :: AREF
    REAL, DIMENSION(NCROP) :: PSNRF
    REAL, DIMENSION(NCROP) :: I2PAR
    REAL, DIMENSION(NCROP) :: TASSIM0
    REAL, DIMENSION(NCROP) :: TASSIM1
    REAL, DIMENSION(NCROP) :: TASSIM2
    REAL, DIMENSION(NCROP) :: K
    REAL, DIMENSION(NCROP) :: EPSI
    REAL, DIMENSION(NCROP) :: Q10MR
    REAL, DIMENSION(NCROP) :: FOLN_MX
    REAL, DIMENSION(NCROP) :: LEFREEZ
    REAL, DIMENSION(NCROP) :: DILE_FC_S1,DILE_FC_S2,DILE_FC_S3,DILE_FC_S4,DILE_FC_S5,DILE_FC_S6,DILE_FC_S7,DILE_FC_S8
    REAL, DIMENSION(NCROP) :: DILE_FW_S1,DILE_FW_S2,DILE_FW_S3,DILE_FW_S4,DILE_FW_S5,DILE_FW_S6,DILE_FW_S7,DILE_FW_S8
    REAL, DIMENSION(NCROP) :: FRA_GR
    REAL, DIMENSION(NCROP) :: LF_OVRC_S1,LF_OVRC_S2,LF_OVRC_S3,LF_OVRC_S4,LF_OVRC_S5,LF_OVRC_S6,LF_OVRC_S7,LF_OVRC_S8
    REAL, DIMENSION(NCROP) :: ST_OVRC_S1,ST_OVRC_S2,ST_OVRC_S3,ST_OVRC_S4,ST_OVRC_S5,ST_OVRC_S6,ST_OVRC_S7,ST_OVRC_S8
    REAL, DIMENSION(NCROP) :: RT_OVRC_S1,RT_OVRC_S2,RT_OVRC_S3,RT_OVRC_S4,RT_OVRC_S5,RT_OVRC_S6,RT_OVRC_S7,RT_OVRC_S8
    REAL, DIMENSION(NCROP) :: LFMR25
    REAL, DIMENSION(NCROP) :: STMR25
    REAL, DIMENSION(NCROP) :: RTMR25
    REAL, DIMENSION(NCROP) :: GRAINMR25
    REAL, DIMENSION(NCROP) :: LFPT_S1,LFPT_S2,LFPT_S3,LFPT_S4,LFPT_S5,LFPT_S6,LFPT_S7,LFPT_S8
    REAL, DIMENSION(NCROP) :: STPT_S1,STPT_S2,STPT_S3,STPT_S4,STPT_S5,STPT_S6,STPT_S7,STPT_S8
    REAL, DIMENSION(NCROP) :: RTPT_S1,RTPT_S2,RTPT_S3,RTPT_S4,RTPT_S5,RTPT_S6,RTPT_S7,RTPT_S8
    REAL, DIMENSION(NCROP) :: GRAINPT_S1,GRAINPT_S2,GRAINPT_S3,GRAINPT_S4,GRAINPT_S5,GRAINPT_S6,GRAINPT_S7,GRAINPT_S8
    REAL, DIMENSION(NCROP) :: LFCT_S1,LFCT_S2,LFCT_S3,LFCT_S4,LFCT_S5,LFCT_S6,LFCT_S7,LFCT_S8
    REAL, DIMENSION(NCROP) :: STCT_S1,STCT_S2,STCT_S3,STCT_S4,STCT_S5,STCT_S6,STCT_S7,STCT_S8
    REAL, DIMENSION(NCROP) :: RTCT_S1,RTCT_S2,RTCT_S3,RTCT_S4,RTCT_S5,RTCT_S6,RTCT_S7,RTCT_S8
    REAL, DIMENSION(NCROP) :: BIO2LAI


!    NAMELIST / noahmp_crop_parameters /DEFAULT_CROP,   PLTDAY,     HSDAY,  PLANTPOP,      IRRI,  GDDTBASE,   GDDTCUT,     GDDS1,     GDDS2, &
!                                             GDDS3,     GDDS4,     GDDS5,      C3C4,      AREF,     PSNRF,     I2PAR,   TASSIM0, &
!                                           TASSIM1,   TASSIM2,         K,      EPSI,     Q10MR,   FOLN_MX,   LEFREEZ,            &
! Zhe Zhang 2020-07-13
    NAMELIST / noahmp_crop_parameters /DEFAULT_CROP,   PLTDAY,     HSDAY,  PLANTPOP,      IRRI,  GDDTBASE,   GDDTCUT,     GDDS1,  GDDS2,  GDDS3,     GDDS4,     GDDS5, & !
                                              C3PSN,     KC25,       AKC,      KO25,       AKO,     AVCMX,    VCMX25,        BP,     MP, FOLNMX,      QE25, &  ! parameters added from stomata
                                               C3C4,     AREF,     PSNRF,     I2PAR,   TASSIM0,                                               &
                                        TASSIM1,   TASSIM2,         K,      EPSI,     Q10MR,   FOLN_MX,   LEFREEZ,               & 
                                        DILE_FC_S1,DILE_FC_S2,DILE_FC_S3,DILE_FC_S4,DILE_FC_S5,DILE_FC_S6,DILE_FC_S7,DILE_FC_S8, &
                                        DILE_FW_S1,DILE_FW_S2,DILE_FW_S3,DILE_FW_S4,DILE_FW_S5,DILE_FW_S6,DILE_FW_S7,DILE_FW_S8, &
                                            FRA_GR,                                                                              &
                                        LF_OVRC_S1,LF_OVRC_S2,LF_OVRC_S3,LF_OVRC_S4,LF_OVRC_S5,LF_OVRC_S6,LF_OVRC_S7,LF_OVRC_S8, &
                                        ST_OVRC_S1,ST_OVRC_S2,ST_OVRC_S3,ST_OVRC_S4,ST_OVRC_S5,ST_OVRC_S6,ST_OVRC_S7,ST_OVRC_S8, &
                                        RT_OVRC_S1,RT_OVRC_S2,RT_OVRC_S3,RT_OVRC_S4,RT_OVRC_S5,RT_OVRC_S6,RT_OVRC_S7,RT_OVRC_S8, &
                                            LFMR25,    STMR25,    RTMR25, GRAINMR25,                                             &
                                           LFPT_S1,   LFPT_S2,   LFPT_S3,   LFPT_S4,   LFPT_S5,   LFPT_S6,   LFPT_S7,   LFPT_S8, &
                                           STPT_S1,   STPT_S2,   STPT_S3,   STPT_S4,   STPT_S5,   STPT_S6,   STPT_S7,   STPT_S8, &
                                           RTPT_S1,   RTPT_S2,   RTPT_S3,   RTPT_S4,   RTPT_S5,   RTPT_S6,   RTPT_S7,   RTPT_S8, &
                                        GRAINPT_S1,GRAINPT_S2,GRAINPT_S3,GRAINPT_S4,GRAINPT_S5,GRAINPT_S6,GRAINPT_S7,GRAINPT_S8, &
       C3PSNI_TABLE     = -1.E36 ! parameter from PSN copied from stomata ! Zhe Zhang 2020-07-13
        KC25I_TABLE     = -1.E36
         AKCI_TABLE     = -1.E36
        KO25I_TABLE     = -1.E36
         AKOI_TABLE     = -1.E36
       AVCMXI_TABLE     = -1.E36
      VCMX25I_TABLE     = -1.E36
          BPI_TABLE     = -1.E36
          MPI_TABLE     = -1.E36
      FOLNMXI_TABLE     = -1.E36
        QE25I_TABLE     = -1.E36 ! ends here
         C3C4_TABLE     = -99999
         AREF_TABLE     = -1.E36
        PSNRF_TABLE     = -1.E36
        I2PAR_TABLE     = -1.E36
      TASSIM0_TABLE     = -1.E36
      TASSIM1_TABLE     = -1.E36
      TASSIM2_TABLE     = -1.E36
            K_TABLE     = -1.E36
         EPSI_TABLE     = -1.E36
        Q10MR_TABLE     = -1.E36
      FOLN_MX_TABLE     = -1.E36
      LEFREEZ_TABLE     = -1.E36
      DILE_FC_TABLE     = -1.E36
      DILE_FW_TABLE     = -1.E36
       FRA_GR_TABLE     = -1.E36
      LF_OVRC_TABLE     = -1.E36
      ST_OVRC_TABLE     = -1.E36
      RT_OVRC_TABLE     = -1.E36
       LFMR25_TABLE     = -1.E36
       STMR25_TABLE     = -1.E36
       RTMR25_TABLE     = -1.E36
    GRAINMR25_TABLE     = -1.E36
         LFPT_TABLE     = -1.E36
         STPT_TABLE     = -1.E36
         RTPT_TABLE     = -1.E36
      GRAINPT_TABLE     = -1.E36
       C3PSNI_TABLE(1:5) = C3PSN(1:5)  ! parameters from stomata ! Zhe Zhang 2020-07-13
        KC25I_TABLE(1:5) = KC25(1:5)
         AKCI_TABLE(1:5) = AKC(1:5)
        KO25I_TABLE(1:5) = KO25(1:5)
         AKOI_TABLE(1:5) = AKO(1:5)
       AVCMXI_TABLE(1:5) = AVCMX(1:5)
      VCMX25I_TABLE(1:5) = VCMX25(1:5)
          BPI_TABLE(1:5) = BP(1:5)
          MPI_TABLE(1:5) = MP(1:5)
      FOLNMXI_TABLE(1:5) = FOLNMX(1:5)
        QE25I_TABLE(1:5) = QE25(1:5)   ! ends here
         C3C4_TABLE      = C3C4
         AREF_TABLE      = AREF
        PSNRF_TABLE      = PSNRF
        I2PAR_TABLE      = I2PAR
      TASSIM0_TABLE      = TASSIM0
      TASSIM1_TABLE      = TASSIM1
      TASSIM2_TABLE      = TASSIM2
            K_TABLE      = K
         EPSI_TABLE      = EPSI
        Q10MR_TABLE      = Q10MR
      FOLN_MX_TABLE      = FOLN_MX
      LEFREEZ_TABLE      = LEFREEZ
      DILE_FC_TABLE(:,1) = DILE_FC_S1
      DILE_FC_TABLE(:,2) = DILE_FC_S2
      DILE_FC_TABLE(:,3) = DILE_FC_S3
      DILE_FC_TABLE(:,4) = DILE_FC_S4
      DILE_FC_TABLE(:,5) = DILE_FC_S5
      DILE_FC_TABLE(:,6) = DILE_FC_S6
      DILE_FC_TABLE(:,7) = DILE_FC_S7
      DILE_FC_TABLE(:,8) = DILE_FC_S8
      DILE_FW_TABLE(:,1) = DILE_FW_S1
      DILE_FW_TABLE(:,2) = DILE_FW_S2
      DILE_FW_TABLE(:,3) = DILE_FW_S3
      DILE_FW_TABLE(:,4) = DILE_FW_S4
      DILE_FW_TABLE(:,5) = DILE_FW_S5
      DILE_FW_TABLE(:,6) = DILE_FW_S6
      DILE_FW_TABLE(:,7) = DILE_FW_S7
      DILE_FW_TABLE(:,8) = DILE_FW_S8
       FRA_GR_TABLE      = FRA_GR
      LF_OVRC_TABLE(:,1) = LF_OVRC_S1
      LF_OVRC_TABLE(:,2) = LF_OVRC_S2
      LF_OVRC_TABLE(:,3) = LF_OVRC_S3
      LF_OVRC_TABLE(:,4) = LF_OVRC_S4
      LF_OVRC_TABLE(:,5) = LF_OVRC_S5
      LF_OVRC_TABLE(:,6) = LF_OVRC_S6
      LF_OVRC_TABLE(:,7) = LF_OVRC_S7
      LF_OVRC_TABLE(:,8) = LF_OVRC_S8
      ST_OVRC_TABLE(:,1) = ST_OVRC_S1
      ST_OVRC_TABLE(:,2) = ST_OVRC_S2
      ST_OVRC_TABLE(:,3) = ST_OVRC_S3
      ST_OVRC_TABLE(:,4) = ST_OVRC_S4
      ST_OVRC_TABLE(:,5) = ST_OVRC_S5
      ST_OVRC_TABLE(:,6) = ST_OVRC_S6
      ST_OVRC_TABLE(:,7) = ST_OVRC_S7
      ST_OVRC_TABLE(:,8) = ST_OVRC_S8
      RT_OVRC_TABLE(:,1) = RT_OVRC_S1
      RT_OVRC_TABLE(:,2) = RT_OVRC_S2
      RT_OVRC_TABLE(:,3) = RT_OVRC_S3
      RT_OVRC_TABLE(:,4) = RT_OVRC_S4
      RT_OVRC_TABLE(:,5) = RT_OVRC_S5
      RT_OVRC_TABLE(:,6) = RT_OVRC_S6
      RT_OVRC_TABLE(:,7) = RT_OVRC_S7
      RT_OVRC_TABLE(:,8) = RT_OVRC_S8
       LFMR25_TABLE      = LFMR25
       STMR25_TABLE      = STMR25
       RTMR25_TABLE      = RTMR25
    GRAINMR25_TABLE      = GRAINMR25
         LFPT_TABLE(:,1) = LFPT_S1
         LFPT_TABLE(:,2) = LFPT_S2
         LFPT_TABLE(:,3) = LFPT_S3
         LFPT_TABLE(:,4) = LFPT_S4
         LFPT_TABLE(:,5) = LFPT_S5
         LFPT_TABLE(:,6) = LFPT_S6
         LFPT_TABLE(:,7) = LFPT_S7
         LFPT_TABLE(:,8) = LFPT_S8
         STPT_TABLE(:,1) = STPT_S1
         STPT_TABLE(:,2) = STPT_S2
         STPT_TABLE(:,3) = STPT_S3
         STPT_TABLE(:,4) = STPT_S4
         STPT_TABLE(:,5) = STPT_S5
         STPT_TABLE(:,6) = STPT_S6
         STPT_TABLE(:,7) = STPT_S7
         STPT_TABLE(:,8) = STPT_S8
         RTPT_TABLE(:,1) = RTPT_S1
         RTPT_TABLE(:,2) = RTPT_S2
         RTPT_TABLE(:,3) = RTPT_S3
         RTPT_TABLE(:,4) = RTPT_S4
         RTPT_TABLE(:,5) = RTPT_S5
         RTPT_TABLE(:,6) = RTPT_S6
         RTPT_TABLE(:,7) = RTPT_S7
         RTPT_TABLE(:,8) = RTPT_S8
      GRAINPT_TABLE(:,1) = GRAINPT_S1
      GRAINPT_TABLE(:,2) = GRAINPT_S2
      GRAINPT_TABLE(:,3) = GRAINPT_S3
      GRAINPT_TABLE(:,4) = GRAINPT_S4
      GRAINPT_TABLE(:,5) = GRAINPT_S5
      GRAINPT_TABLE(:,6) = GRAINPT_S6
      GRAINPT_TABLE(:,7) = GRAINPT_S7
      GRAINPT_TABLE(:,8) = GRAINPT_S8
  subroutine read_mp_irrigation_parameters()
    implicit none
    integer :: ierr
    logical :: file_named 

    REAL    :: IRR_FRAC              ! irrigation Fraction
    INTEGER :: IRR_HAR               ! number of days before harvest date to stop irrigation 
    REAL    :: IRR_LAI               ! Minimum lai to trigger irrigation
    REAL    :: IRR_MAD               ! management allowable deficit (0-1)
    REAL    :: FILOSS                ! fraction of flood irrigation loss (0-1) 
    REAL    :: SPRIR_RATE            ! mm/h, sprinkler irrigation rate
    REAL    :: MICIR_RATE            ! mm/h, micro irrigation rate
    REAL    :: FIRTFAC               ! flood application rate factor
    REAL    :: IR_RAIN               ! maximum precipitation to stop irrigation trigger

    NAMELIST / noahmp_irrigation_parameters / IRR_FRAC, IRR_HAR, IRR_LAI, IRR_MAD, FILOSS, &
                                              SPRIR_RATE, MICIR_RATE, FIRTFAC, IR_RAIN

    IRR_FRAC_TABLE   = -1.E36    ! irrigation Fraction
    IRR_HAR_TABLE    =  0        ! number of days before harvest date to stop irrigation 
    IRR_LAI_TABLE    = -1.E36    ! Minimum lai to trigger irrigation
    IRR_MAD_TABLE    = -1.E36    ! management allowable deficit (0-1)
    FILOSS_TABLE     = -1.E36    ! fraction of flood irrigation loss (0-1) 
    SPRIR_RATE_TABLE = -1.E36    ! mm/h, sprinkler irrigation rate
    MICIR_RATE_TABLE = -1.E36    ! mm/h, micro irrigation rate
    FIRTFAC_TABLE    = -1.E36    ! flood application rate factor
    IR_RAIN_TABLE    = -1.E36    ! maximum precipitation to stop irrigation trigger

    inquire( file='MPTABLE.TBL', exist=file_named ) 
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file MPTABLE.TBL")')
       call wrf_error_fatal("STOP in Noah-MP read_mp_crop_parameters")
    endif

    read(15,noahmp_irrigation_parameters)
    close(15)

    IRR_FRAC_TABLE   = IRR_FRAC    ! irrigation Fraction
    IRR_HAR_TABLE    = IRR_HAR     ! number of days before harvest date to stop irrigation 
    IRR_LAI_TABLE    = IRR_LAI     ! Minimum lai to trigger irrigation
    IRR_MAD_TABLE    = IRR_MAD     ! management allowable deficit (0-1)
    FILOSS_TABLE     = FILOSS      ! fraction of flood irrigation loss (0-1) 
    SPRIR_RATE_TABLE = SPRIR_RATE  ! mm/h, sprinkler irrigation rate
    MICIR_RATE_TABLE = MICIR_RATE  ! mm/h, micro irrigation rate
    FIRTFAC_TABLE    = FIRTFAC     ! flood application rate factor
    IR_RAIN_TABLE    = IR_RAIN     ! maximum precipitation to stop irrigation trigger

  end subroutine read_mp_irrigation_parameters

  subroutine read_mp_optional_parameters()
    implicit none
    integer :: ierr
    logical :: file_named 

    NAMELIST / noahmp_optional_parameters /                                &
         sr2006_theta_1500t_a, sr2006_theta_1500t_b, sr2006_theta_1500t_c, &
         sr2006_theta_1500t_d, sr2006_theta_1500t_e, sr2006_theta_1500t_f, &
         sr2006_theta_1500t_g                                            , &
         sr2006_theta_1500_a , sr2006_theta_1500_b                       , &
         sr2006_theta_33t_a  , sr2006_theta_33t_b  , sr2006_theta_33t_c  , &
         sr2006_theta_33t_d  , sr2006_theta_33t_e  , sr2006_theta_33t_f  , &
         sr2006_theta_33t_g                                              , &
         sr2006_theta_33_a   , sr2006_theta_33_b   , sr2006_theta_33_c   , &
         sr2006_theta_s33t_a , sr2006_theta_s33t_b , sr2006_theta_s33t_c , &
         sr2006_theta_s33t_d , sr2006_theta_s33t_e , sr2006_theta_s33t_f , &
         sr2006_theta_s33t_g                                             , &
         sr2006_theta_s33_a  , sr2006_theta_s33_b                        , &
         sr2006_psi_et_a     , sr2006_psi_et_b     , sr2006_psi_et_c     , &
         sr2006_psi_et_d     , sr2006_psi_et_e     , sr2006_psi_et_f     , &
         sr2006_psi_et_g                                                 , &
         sr2006_psi_e_a      , sr2006_psi_e_b      , sr2006_psi_e_c      , &
         sr2006_smcmax_a     , sr2006_smcmax_b

    inquire( file='MPTABLE.TBL', exist=file_named ) 
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file MPTABLE.TBL")')
       call wrf_error_fatal("STOP in Noah-MP read_mp_optional_parameters")
    endif

    read(15,noahmp_optional_parameters)
    close(15)


  end subroutine read_mp_optional_parameters

END MODULE NOAHMP_TABLES

