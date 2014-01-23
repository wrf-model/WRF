module noahmp_globals

  ! Maybe most of these can be moved to a REDPRM use statement?
  use module_sf_noahlsm, only: &
       &                       SLCATS,     &
       &                       LUCATS,     &
       &                       CSOIL_DATA, & 
       &                       BB,         &
       &                       SATDK,      &
       &                       SATDW,      &
       &                       F11,        &
       &                       SATPSI,     &
       &                       QTZ,        &
       &                       DRYSMC,     &
       &                       MAXSMC,     &
       &                       REFSMC,     &
       &                       WLTSMC,     &
       &                       RSTBL,      &
       &                       RGLTBL,     &
       &                       HSTBL,      &
       &                       NROTBL,     &
       &                       TOPT_DATA,  &
       &                       RSMAX_DATA, &
       &                       ZBOT_DATA,  &
       &                       CZIL_DATA,  &
       &                       FRZK_DATA,  &
       &                       SLOPE_DATA, &
       &                       REFDK_DATA, &
       &                       REFKDT_DATA
       
  implicit none

! ==================================================================================================
!------------------------------------------------------------------------------------------!
! Physical Constants:                                                                      !
!------------------------------------------------------------------------------------------!

  REAL, PARAMETER :: GRAV   = 9.80616   !acceleration due to gravity (m/s2)
  REAL, PARAMETER :: SB     = 5.67E-08  !Stefan-Boltzmann constant (w/m2/k4)
  REAL, PARAMETER :: VKC    = 0.40      !von Karman constant
  REAL, PARAMETER :: TFRZ   = 273.16    !freezing/melting point (k)
  REAL, PARAMETER :: HSUB   = 2.8440E06 !latent heat of sublimation (j/kg)
  REAL, PARAMETER :: HVAP   = 2.5104E06 !latent heat of vaporization (j/kg)
  REAL, PARAMETER :: HFUS   = 0.3336E06 !latent heat of fusion (j/kg)
  REAL, PARAMETER :: CWAT   = 4.188E06  !specific heat capacity of water (j/m3/k)
  REAL, PARAMETER :: CICE   = 2.094E06  !specific heat capacity of ice (j/m3/k)
  REAL, PARAMETER :: CPAIR  = 1004.64   !heat capacity dry air at const pres (j/kg/k)
  REAL, PARAMETER :: TKWAT  = 0.6       !thermal conductivity of water (w/m/k)
  REAL, PARAMETER :: TKICE  = 2.2       !thermal conductivity of ice (w/m/k)
  REAL, PARAMETER :: TKAIR  = 0.023     !thermal conductivity of air (w/m/k)
  REAL, PARAMETER :: RAIR   = 287.04    !gas constant for dry air (j/kg/k)
  REAL, PARAMETER :: RW     = 461.269   !gas constant for  water vapor (j/kg/k)
  REAL, PARAMETER :: DENH2O = 1000.     !density of water (kg/m3)
  REAL, PARAMETER :: DENICE = 917.      !density of ice (kg/m3)

!------------------------------------------------------------------------------------------!
! From the VEGPARM.TBL tables, as functions of vegetation category.
!------------------------------------------------------------------------------------------!
  INTEGER :: NROOT        !rooting depth [as the number of layers] ( Assigned in REDPRM )
  REAL    :: RGL          !parameter used in radiation stress function ( Assigned in REDPRM )
  REAL    :: RSMIN        !minimum Canopy Resistance [s/m] ( Assigned in REDPRM )
  REAL    :: HS           !parameter used in vapor pressure deficit function ( Assigned in REDPRM )
  REAL    :: RSMAX        !maximum stomatal resistance ( Assigned in REDPRM )
  REAL    :: TOPT         !optimum transpiration air temperature.

!KWM  CHARACTER(LEN=256) ::  LUTYPE
!KWM  INTEGER                    :: LUCATS, BARE
!KWM  INTEGER, PARAMETER         :: NLUS=50
!KWM  INTEGER, DIMENSION(1:NLUS) :: NROTBL
!KWM  REAL,    DIMENSION(1:NLUS) :: RSTBL, RGLTBL, HSTBL
!KWM  REAL                       :: TOPT_DATA,RSMAX_DATA

! not further used in this version (niu):

!KWM  REAL,    DIMENSION(1:NLUS) ::  SNUPTBL, LAITBL,        &
!KWM                                 ALBTBL, SHDTBL, MAXALB
!KWM  REAL                       ::  CMCMAX_DATA,CFACTR_DATA,SBETA_DATA,&
!KWM                                 SALP_DATA  ,SMLOW_DATA ,SMHIGH_DATA

!KWM  REAL,    DIMENSION(NLUS)   ::  LAIMINTBL    !KWM
!KWM  REAL,    DIMENSION(NLUS)   ::  LAIMAXTBL    !KWM
!KWM  REAL,    DIMENSION(NLUS)   ::  EMISSMINTBL  !KWM
!KWM  REAL,    DIMENSION(NLUS)   ::  EMISSMAXTBL  !KWM
!KWM  REAL,    DIMENSION(NLUS)   ::  ALBEDOMINTBL !KWM
!KWM  REAL,    DIMENSION(NLUS)   ::  ALBEDOMAXTBL !KWM
!KWM  REAL,    DIMENSION(NLUS)   ::  Z0MINTBL     !KWM
!KWM  REAL,    DIMENSION(NLUS)   ::  Z0MAXTBL     !KWM


!------------------------------------------------------------------------------------------!
! From the SOILPARM.TBL tables, as functions of soil category.
!------------------------------------------------------------------------------------------!
  REAL    :: BEXP         !B parameter ( Assigned in REDPRM )
  REAL    :: SMCDRY       !dry soil moisture threshold where direct evap from top
                          !layer ends (volumetric) ( Assigned in REDPRM )
  REAL    :: F1           !soil thermal diffusivity/conductivity coef ( Assigned in REDPRM )
  REAL    :: SMCMAX       !porosity, saturated value of soil moisture (volumetric)
  REAL    :: SMCREF       !reference soil moisture (field capacity) (volumetric) ( Assigned in REDPRM )
  REAL    :: PSISAT       !saturated soil matric potential ( Assigned in REDPRM )
  REAL    :: DKSAT        !saturated soil hydraulic conductivity ( Assigned in REDPRM )
  REAL    :: DWSAT        !saturated soil hydraulic diffusivity ( Assigned in REDPRM )
  REAL    :: SMCWLT       !wilting point soil moisture (volumetric) ( Assigned in REDPRM )
  REAL    :: QUARTZ       !soil quartz content ( Assigned in REDPRM )

!KWM  CHARACTER*4 SLTYPE
!KWM  INTEGER                     :: SLCATS
!KWM  INTEGER, PARAMETER          :: NSLTYPE=30
!KWM  REAL, DIMENSION (1:NSLTYPE) :: BB,DRYSMC,F11,                           &
!KWM        MAXSMC, REFSMC,SATPSI,SATDK,SATDW, WLTSMC,QTZ
!------------------------------------------------------------------------------------------!
! From the GENPARM.TBL file
!------------------------------------------------------------------------------------------!
  REAL    :: SLOPE       !slope index (0 - 1) ( Assigned in REDPRM )
  REAL    :: CSOIL       !vol. soil heat capacity [j/m3/K] ( Assigned in REDPRM )
  REAL    :: ZBOT        !Depth (m) of lower boundary soil temperature ( Assigned in REDPRM )
  REAL    :: CZIL        !Calculate roughness length of heat ( Assigned in REDPRM )

  REAL    :: KDT         !used in compute maximum infiltration rate (in INFIL) ( Assigned in REDPRM )
  REAL    :: FRZX        !used in compute maximum infiltration rate (in INFIL) ( Assigned in REDPRM )

! LSM GENERAL PARAMETERS

!KWM  INTEGER :: SLPCATS
!KWM  INTEGER, PARAMETER :: NSLOPE=30
!KWM  REAL, DIMENSION (1:NSLOPE) :: SLOPE_DATA
!KWM  REAL ::  FXEXP_DATA,CSOIL_DATA,REFDK_DATA ,         &
!KWM           REFKDT_DATA,FRZK_DATA ,ZBOT_DATA ,CZIL_DATA

! =====================================options for different schemes================================
! options for dynamic vegetation: 
! 1 -> off (use table LAI; use FVEG = SHDFAC from input)
! 2 -> on (together with OPT_CRS = 1)
! 3 -> off (use table LAI; calculate FVEG)
! 4 -> off (use table LAI; use maximum vegetation fraction)

  INTEGER :: DVEG    != 4   !

! options for canopy stomatal resistance
! 1-> Ball-Berry; 2->Jarvis

  INTEGER :: OPT_CRS != 1    !(must 1 when DVEG = 2)

! options for soil moisture factor for stomatal resistance
! 1-> Noah (soil moisture) 
! 2-> CLM  (matric potential)
! 3-> SSiB (matric potential)

  INTEGER :: OPT_BTR != 1    !(suggested 1)

! options for runoff and groundwater
! 1 -> TOPMODEL with groundwater (Niu et al. 2007 JGR) ;
! 2 -> TOPMODEL with an equilibrium water table (Niu et al. 2005 JGR) ;
! 3 -> original surface and subsurface runoff (free drainage)
! 4 -> BATS surface and subsurface runoff (free drainage)

  INTEGER :: OPT_RUN != 1    !(suggested 1)

! options for surface layer drag coeff (CH & CM)
! 1->M-O ; 2->original Noah (Chen97); 3->MYJ consistent; 4->YSU consistent. 

  INTEGER :: OPT_SFC != 1    !(1 or 2 or 3 or 4)

! options for supercooled liquid water (or ice fraction)
! 1-> no iteration (Niu and Yang, 2006 JHM); 2: Koren's iteration 

  INTEGER :: OPT_FRZ != 1    !(1 or 2)

! options for frozen soil permeability
! 1 -> linear effects, more permeable (Niu and Yang, 2006, JHM)
! 2 -> nonlinear effects, less permeable (old)

  INTEGER :: OPT_INF != 1    !(suggested 1)

! options for radiation transfer
! 1 -> modified two-stream (gap = F(solar angle, 3D structure ...)<1-FVEG)
! 2 -> two-stream applied to grid-cell (gap = 0)
! 3 -> two-stream applied to vegetated fraction (gap=1-FVEG)

  INTEGER :: OPT_RAD != 1    !(suggested 1)

! options for ground snow surface albedo
! 1-> BATS; 2 -> CLASS

  INTEGER :: OPT_ALB != 2    !(suggested 2)

! options for partitioning  precipitation into rainfall & snowfall
! 1 -> Jordan (1991); 2 -> BATS: when SFCTMP<TFRZ+2.2 ; 3-> SFCTMP<TFRZ

  INTEGER :: OPT_SNF != 1    !(suggested 1)

! options for lower boundary condition of soil temperature
! 1 -> zero heat flux from bottom (ZBOT and TBOT not used)
! 2 -> TBOT at ZBOT (8m) read from a file (original Noah)

  INTEGER :: OPT_TBOT != 2   !(suggested 2)

! options for snow/soil temperature time scheme (only layer 1)
! 1 -> semi-implicit; 2 -> full implicit (original Noah)

  INTEGER :: OPT_STC != 1    !(suggested 1)
! ==================================================================================================
! runoff parameters used for SIMTOP and SIMGM:
  REAL, PARAMETER :: TIMEAN = 10.5   !gridcell mean topgraphic index (global mean)
  REAL, PARAMETER :: FSATMX = 0.38   !maximum surface saturated fraction (global mean)

! adjustable parameters for snow processes

  REAL, PARAMETER :: M      = 1.0 ! 2.50   !melting factor (-) 
  REAL, PARAMETER :: Z0SNO  = 0.002  !snow surface roughness length (m) (0.002)
  REAL, PARAMETER :: SSI    = 0.03   !liquid water holding capacity for snowpack (m3/m3) (0.03)
  REAL, PARAMETER :: SWEMX  = 1.00   !new snow mass to fully cover old snow (mm)
                                     !equivalent to 10mm depth (density = 100 kg/m3)

! NOTES: things to add or improve
! 1. lake model: explicit representation of lake water storage, sunlight through lake
!    with different purity, turbulent mixing of surface laker water, snow on frozen lake, etc.
! 2. shallow snow wihtout a layer: melting energy
! 3. urban model to be added.
! 4. irrigation
!------------------------------------------------------------------------------------------!
END MODULE NOAHMP_GLOBALS
!------------------------------------------------------------------------------------------!
!------------------------------------------------------------------------------------------!
MODULE NOAHMP_VEG_PARAMETERS

    IMPLICIT NONE

    INTEGER, PARAMETER :: MAX_VEG_PARAMS = 33
    INTEGER, PARAMETER :: MVT   = 27
    INTEGER, PARAMETER :: MBAND = 2

    INTEGER, PRIVATE :: ISURBAN
    INTEGER :: ISWATER
    INTEGER :: ISBARREN
    INTEGER :: ISSNOW
    INTEGER :: EBLFOREST

    REAL :: CH2OP(MVT)       !maximum intercepted h2o per unit lai+sai (mm)
    REAL :: DLEAF(MVT)       !characteristic leaf dimension (m)
    REAL :: Z0MVT(MVT)       !momentum roughness length (m)
    REAL :: HVT(MVT)         !top of canopy (m)
    REAL :: HVB(MVT)         !bottom of canopy (m)
    REAL :: DEN(MVT)         !tree density (no. of trunks per m2)
    REAL :: RC(MVT)          !tree crown radius (m)
    REAL :: SAIM(MVT,12)     !monthly stem area index, one-sided
    REAL :: LAIM(MVT,12)     !monthly leaf area index, one-sided
    REAL :: SLA(MVT)         !single-side leaf area per Kg [m2/kg]
    REAL :: DILEFC(MVT)      !coeficient for leaf stress death [1/s]
    REAL :: DILEFW(MVT)      !coeficient for leaf stress death [1/s]
    REAL :: FRAGR(MVT)       !fraction of growth respiration  !original was 0.3 
    REAL :: LTOVRC(MVT)      !leaf turnover [1/s]

    REAL :: C3PSN(MVT)       !photosynthetic pathway: 0. = c4, 1. = c3
    REAL :: KC25(MVT)        !co2 michaelis-menten constant at 25c (pa)
    REAL :: AKC(MVT)         !q10 for kc25
    REAL :: KO25(MVT)        !o2 michaelis-menten constant at 25c (pa)
    REAL :: AKO(MVT)         !q10 for ko25
    REAL :: VCMX25(MVT)      !maximum rate of carboxylation at 25c (umol co2/m**2/s)
    REAL :: AVCMX(MVT)       !q10 for vcmx25
    REAL :: BP(MVT)          !minimum leaf conductance (umol/m**2/s)
    REAL :: MP(MVT)          !slope of conductance-to-photosynthesis relationship
    REAL :: QE25(MVT)        !quantum efficiency at 25c (umol co2 / umol photon)
    REAL :: AQE(MVT)         !q10 for qe25
    REAL :: RMF25(MVT)       !leaf maintenance respiration at 25c (umol co2/m**2/s)
    REAL :: RMS25(MVT)       !stem maintenance respiration at 25c (umol co2/kg bio/s)
    REAL :: RMR25(MVT)       !root maintenance respiration at 25c (umol co2/kg bio/s)
    REAL :: ARM(MVT)         !q10 for maintenance respiration
    REAL :: FOLNMX(MVT)      !foliage nitrogen concentration when f(n)=1 (%)
    REAL :: TMIN(MVT)        !minimum temperature for photosynthesis (k)

    REAL :: XL(MVT)          !leaf/stem orientation index
    REAL :: RHOL(MVT,MBAND)  !leaf reflectance: 1=vis, 2=nir
    REAL :: RHOS(MVT,MBAND)  !stem reflectance: 1=vis, 2=nir
    REAL :: TAUL(MVT,MBAND)  !leaf transmittance: 1=vis, 2=nir
    REAL :: TAUS(MVT,MBAND)  !stem transmittance: 1=vis, 2=nir

    REAL :: MRP(MVT)         !microbial respiration parameter (umol co2 /kg c/ s)
    REAL :: CWPVT(MVT)       !empirical canopy wind parameter

    REAL :: WRRAT(MVT)       !wood to non-wood ratio
    REAL :: WDPOOL(MVT)      !wood pool (switch 1 or 0) depending on woody or not [-]
    REAL :: TDLEF(MVT)       !characteristic T for leaf freezing [K]

    INTEGER :: IK,IM
    REAL :: TMP10(MVT*MBAND)
    REAL :: TMP11(MVT*MBAND)
    REAL :: TMP12(MVT*MBAND)
    REAL :: TMP13(MVT*MBAND)
    REAL :: TMP14(MVT*12)
    REAL :: TMP15(MVT*12)
    REAL :: TMP16(MVT*5)

    real slarea(MVT)
    real eps(MVT,5)

CONTAINS
  subroutine read_mp_veg_parameters(DATASET_IDENTIFIER)
    implicit none
    character(len=*), intent(in) :: DATASET_IDENTIFIER
    integer :: ierr

    ! Temporary arrays used in reshaping namelist arrays
    REAL :: TMP10(MVT*MBAND)
    REAL :: TMP11(MVT*MBAND)
    REAL :: TMP12(MVT*MBAND)
    REAL :: TMP13(MVT*MBAND)
    REAL :: TMP14(MVT*12)
    REAL :: TMP15(MVT*12)
    REAL :: TMP16(MVT*5)

    integer :: NVEG
    character(len=256) :: VEG_DATASET_DESCRIPTION

    NAMELIST / noah_mp_usgs_veg_categories / VEG_DATASET_DESCRIPTION, NVEG
    NAMELIST / noah_mp_usgs_parameters / ISURBAN, ISWATER, ISBARREN, ISSNOW, EBLFOREST, &
         CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, RHOL,  RHOS, TAUL, TAUS, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
         LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
         FOLNMX, WDPOOL, WRRAT, MRP, SAIM,  LAIM,  SLAREA, EPS

    NAMELIST / noah_mp_modis_veg_categories / VEG_DATASET_DESCRIPTION, NVEG
    NAMELIST / noah_mp_modis_parameters / ISURBAN, ISWATER, ISBARREN, ISSNOW, EBLFOREST, &
         CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, RHOL,  RHOS, TAUL, TAUS, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
         LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
         FOLNMX, WDPOOL, WRRAT, MRP, SAIM,  LAIM,  SLAREA, EPS

    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
    CH2OP  = -1.E36
    DLEAF  = -1.E36
    Z0MVT  = -1.E36
    HVT    = -1.E36
    HVB    = -1.E36
    DEN    = -1.E36
    RC     = -1.E36
    RHOL   = -1.E36
    RHOS   = -1.E36
    TAUL   = -1.E36
    TAUS   = -1.E36
    XL     = -1.E36
    CWPVT  = -1.E36
    C3PSN  = -1.E36
    KC25   = -1.E36
    AKC    = -1.E36
    KO25   = -1.E36
    AKO    = -1.E36
    AVCMX  = -1.E36
    AQE    = -1.E36
    LTOVRC = -1.E36
    DILEFC = -1.E36
    DILEFW = -1.E36
    RMF25  = -1.E36
    SLA    = -1.E36
    FRAGR  = -1.E36
    TMIN   = -1.E36
    VCMX25 = -1.E36
    TDLEF  = -1.E36
    BP     = -1.E36
    MP     = -1.E36
    QE25   = -1.E36
    RMS25  = -1.E36
    RMR25  = -1.E36
    ARM    = -1.E36
    FOLNMX = -1.E36
    WDPOOL = -1.E36
    WRRAT  = -1.E36
    MRP    = -1.E36
    SAIM   = -1.E36
    LAIM   = -1.E36
    SLAREA = -1.E36
    EPS    = -1.E36

    open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    if (ierr /= 0) then
       write(*,'("****** Error ******************************************************")')
       write(*,'("Cannot find file MPTABLE.TBL")')
       write(*,'("STOP")')
       write(*,'("*******************************************************************")')
       call wrf_error_fatal("STOP in Noah-MP read_mp_veg_parameters")
    endif

    if ( trim(DATASET_IDENTIFIER) == "USGS" ) then
       read(15,noah_mp_usgs_veg_categories)
       read(15,noah_mp_usgs_parameters)
    else if ( trim(DATASET_IDENTIFIER) == "MODIFIED_IGBP_MODIS_NOAH" ) then
       read(15,noah_mp_modis_veg_categories)
       read(15,noah_mp_modis_parameters)
    else
       write(*,'("Unrecognized DATASET_IDENTIFIER in subroutine READ_MP_VEG_PARAMETERS")')
       write(*,'("DATASET_IDENTIFIER = ''", A, "''")') trim(DATASET_IDENTIFIER)
       call wrf_error_fatal("STOP in Noah-MP read_mp_veg_parameters")
    endif
    close(15)

    ! Problem.  Namelist reading of 2-d arrays doesn't work well when the arrays are declared with larger dimension than the
    ! variables in the provided namelist.  So we need to reshape the 2-d arrays after we've read them.

    if ( MVT > NVEG ) then

       ! 
       ! Reshape the 2-d arrays:
       ! 

       TMP10 = reshape( RHOL, (/ MVT*size(RHOL,2) /))
       TMP11 = reshape( RHOS, (/ MVT*size(RHOS,2) /))
       TMP12 = reshape( TAUL, (/ MVT*size(TAUL,2) /))
       TMP13 = reshape( TAUS, (/ MVT*size(TAUS,2) /))
       TMP14 = reshape( SAIM, (/ MVT*size(SAIM,2) /))
       TMP15 = reshape( LAIM, (/ MVT*size(LAIM,2) /))
       TMP16 = reshape( EPS,  (/ MVT*size(EPS ,2) /))

       RHOL(1:NVEG,:) = reshape( TMP10, (/ NVEG, size(RHOL,2) /))
       RHOS(1:NVEG,:) = reshape( TMP11, (/ NVEG, size(RHOS,2) /))
       TAUL(1:NVEG,:) = reshape( TMP12, (/ NVEG, size(TAUL,2) /))
       TAUS(1:NVEG,:) = reshape( TMP13, (/ NVEG, size(TAUS,2) /))
       SAIM(1:NVEG,:) = reshape( TMP14, (/ NVEG, size(SAIM,2) /))
       LAIM(1:NVEG,:) = reshape( TMP15, (/ NVEG, size(LAIM,2) /))
       EPS(1:NVEG,:)  = reshape( TMP16, (/ NVEG, size(EPS,2)  /))

       RHOL(NVEG+1:MVT,:) = -1.E36
       RHOS(NVEG+1:MVT,:) = -1.E36
       TAUL(NVEG+1:MVT,:) = -1.E36
       TAUS(NVEG+1:MVT,:) = -1.E36
       SAIM(NVEG+1:MVT,:) = -1.E36
       LAIM(NVEG+1:MVT,:) = -1.E36
       EPS( NVEG+1:MVT,:) = -1.E36
    endif

  end subroutine read_mp_veg_parameters

END MODULE NOAHMP_VEG_PARAMETERS
! ==================================================================================================
! ==================================================================================================
MODULE NOAHMP_RAD_PARAMETERS

    IMPLICIT NONE
 
    INTEGER I                ! loop index
    INTEGER, PARAMETER :: MSC   = 9
    INTEGER, PARAMETER :: MBAND = 2

    REAL :: ALBSAT(MSC,MBAND)   !saturated soil albedos: 1=vis, 2=nir
    REAL :: ALBDRY(MSC,MBAND)   !dry soil albedos: 1=vis, 2=nir
    REAL :: ALBICE(MBAND)       !albedo land ice: 1=vis, 2=nir
    REAL :: ALBLAK(MBAND)       !albedo frozen lakes: 1=vis, 2=nir
    REAL :: OMEGAS(MBAND)       !two-stream parameter omega for snow
    REAL :: BETADS              !two-stream parameter betad for snow
    REAL :: BETAIS              !two-stream parameter betad for snow
    REAL :: EG(2)               !emissivity

! saturated soil albedos: 1=vis, 2=nir
    DATA(ALBSAT(I,1),I=1,8)/0.15,0.11,0.10,0.09,0.08,0.07,0.06,0.05/
    DATA(ALBSAT(I,2),I=1,8)/0.30,0.22,0.20,0.18,0.16,0.14,0.12,0.10/

! dry soil albedos: 1=vis, 2=nir
    DATA(ALBDRY(I,1),I=1,8)/0.27,0.22,0.20,0.18,0.16,0.14,0.12,0.10/
    DATA(ALBDRY(I,2),I=1,8)/0.54,0.44,0.40,0.36,0.32,0.28,0.24,0.20/

! albedo land ice: 1=vis, 2=nir
    DATA (ALBICE(I),I=1,MBAND) /0.80, 0.55/

! albedo frozen lakes: 1=vis, 2=nir
    DATA (ALBLAK(I),I=1,MBAND) /0.60, 0.40/

! omega,betad,betai for snow
    DATA (OMEGAS(I),I=1,MBAND) /0.8, 0.4/
    DATA BETADS, BETAIS /0.5, 0.5/

! emissivity ground surface    
      DATA EG /0.97, 0.98/ ! 1-soil;2-lake

END MODULE NOAHMP_RAD_PARAMETERS
! ==================================================================================================

MODULE NOAHMP_ROUTINES
  USE NOAHMP_GLOBALS
  IMPLICIT NONE

  public  :: noahmp_options
  public  :: NOAHMP_SFLX
  public  :: REDPRM
  public  :: FRH2O           

  private :: ATM
  private :: PHENOLOGY
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
!  private ::                         INFIL       
  private ::               SSTEP
  private ::       GROUNDWATER

  private :: CARBON
  private ::       CO2FLUX
!  private ::       BVOCFLUX
!  private ::       CH4FLUX

  private :: ERROR

contains
!
! ==================================================================================================

  SUBROUTINE NOAHMP_SFLX (&
                   ILOC    , JLOC    , LAT     , YEARLEN , JULIAN  , COSZ    , & ! IN : Time/Space-related
                   DT      , DX      , DZ8W    , NSOIL   , ZSOIL   , NSNOW   , & ! IN : Model configuration 
                   SHDFAC  , SHDMAX  , VEGTYP  , ISURBAN , ICE     , IST     , & ! IN : Vegetation/Soil characteristics
                   ISC     ,                                                   & ! IN : Vegetation/Soil characteristics
                   IZ0TLND ,                                                   & ! IN : User options
                   SFCTMP  , SFCPRS  , PSFC    , UU      , VV      , Q2      , & ! IN : Forcing
                   QC      , SOLDN   , LWDN    , PRCP    , TBOT    , CO2AIR  , & ! IN : Forcing
                   O2AIR   , FOLN    , FICEOLD , PBLH    , ZLVL    ,           & ! IN : Forcing
                   ALBOLD  , SNEQVO  ,                                         & ! IN/OUT : 
                   STC     , SH2O    , SMC     , TAH     , EAH     , FWET    , & ! IN/OUT : 
                   CANLIQ  , CANICE  , TV      , TG      , QSFC    , QSNOW   , & ! IN/OUT : 
                   ISNOW   , ZSNSO   , SNOWH   , SNEQV   , SNICE   , SNLIQ   , & ! IN/OUT : 
                   ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & ! IN/OUT : 
                   STMASS  , WOOD    , STBLCP  , FASTCP  , LAI     , SAI     , & ! IN/OUT : 
                   CM      , CH      , TAUSS   ,                               & ! IN/OUT : 
                   FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV    , & ! OUT : 
                   FGEV    , FCTR    , ECAN    , ETRAN   , EDIR    , TRAD    , & ! OUT :
                   TGB     , TGV     , T2MV    , T2MB    , Q2V     , Q2B     , & ! OUT :
                   RUNSRF  , RUNSUB  , APAR    , PSN     , SAV     , SAG     , & ! OUT :
                   FSNO    , NEE     , GPP     , NPP     , FVEG    , ALBEDO  , & ! OUT :
                   QSNBOT  , PONDING , PONDING1, PONDING2, RSSUN   , RSSHA   , & ! OUT :
                   BGAP    , WGAP    , CHV     , CHB     , EMISSI  ,           & ! OUT :
		   SHG     , SHC     , SHB     , EVG     , EVB     , GHV     , & ! OUT :
		   GHB     , IRG     , IRC     , IRB     , TR      , EVC     , & ! OUT :
		   CHLEAF  , CHUC    , CHV2    , CHB2    , FPICE               &
#ifdef WRF_HYDRO
                   ,sfcheadrt                                          &
#endif
                   )            ! OUT :

! --------------------------------------------------------------------------------------------------
! Initial code: Guo-Yue Niu, Oct. 2007
! --------------------------------------------------------------------------------------------------
  USE NOAHMP_VEG_PARAMETERS
  USE NOAHMP_RAD_PARAMETERS
! --------------------------------------------------------------------------------------------------
  implicit none
! --------------------------------------------------------------------------------------------------
! input
  INTEGER                        , INTENT(IN)    :: ICE    !ice (ice = 1)
  INTEGER                        , INTENT(IN)    :: IST    !surface type 1->soil; 2->lake
  INTEGER                        , INTENT(IN)    :: VEGTYP !vegetation type 
  INTEGER                        , INTENT(IN)    :: ISC    !soil color type (1-lighest; 8-darkest)
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
  REAL                           , INTENT(IN)    :: PRCP   !precipitation rate (kg m-2 s-1)
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

!jref:start; in 
  INTEGER                        , INTENT(IN)    :: ISURBAN
  INTEGER                        , INTENT(IN)    :: IZ0TLND
  REAL                           , INTENT(IN)    :: QC     !cloud water mixing ratio
  REAL                           , INTENT(IN)    :: PBLH   !planetary boundary layer height
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
  REAL                           , INTENT(INOUT) :: FWET   !wetted or snowed fraction of canopy (-)
  REAL                           , INTENT(INOUT) :: SNEQVO !snow mass at last time step (mm)
  REAL                           , INTENT(INOUT) :: EAH    !canopy air vapor pressure (pa)
  REAL                           , INTENT(INOUT) :: TAH    !canopy air tmeperature (k)
  REAL                           , INTENT(INOUT) :: ALBOLD !snow albedo at last time step (CLASS type)
  REAL                           , INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL                           , INTENT(INOUT) :: CH     !sensible heat exchange coefficient
  REAL                           , INTENT(INOUT) :: TAUSS  !non-dimensional snow age

! prognostic variables
  INTEGER                        , INTENT(INOUT) :: ISNOW  !actual no. of snow layers [-]
  REAL                           , INTENT(INOUT) :: CANLIQ !intercepted liquid water (mm)
  REAL                           , INTENT(INOUT) :: CANICE !intercepted ice mass (mm)
  REAL                           , INTENT(INOUT) :: SNEQV  !snow water eqv. [mm]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  !layer-bottom depth from snow surf [m]
  REAL                           , INTENT(INOUT) :: SNOWH  !snow height [m]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]
  REAL                           , INTENT(INOUT) :: TV     !vegetation temperature (k)
  REAL                           , INTENT(INOUT) :: TG     !ground temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow/soil temperature [k]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !liquid soil moisture [m3/m3]
  REAL                           , INTENT(INOUT) :: ZWT    !depth to water table [m]
  REAL                           , INTENT(INOUT) :: WA     !water storage in aquifer [mm]
  REAL                           , INTENT(INOUT) :: WT     !water in aquifer&saturated soil [mm]
  REAL                           , INTENT(INOUT) :: WSLAKE !lake water storage (can be neg.) (mm)

! output
  REAL                           , INTENT(OUT)   :: FSA    !total absorbed solar radiation (w/m2)
  REAL                           , INTENT(OUT)   :: FSR    !total reflected solar radiation (w/m2)
  REAL                           , INTENT(OUT)   :: FIRA   !total net LW rad (w/m2)  [+ to atm]
  REAL                           , INTENT(OUT)   :: FSH    !total sensible heat (w/m2) [+ to atm]
  REAL                           , INTENT(OUT)   :: FCEV   !canopy evap heat (w/m2) [+ to atm]
  REAL                           , INTENT(OUT)   :: FGEV   !ground evap heat (w/m2) [+ to atm]
  REAL                           , INTENT(OUT)   :: FCTR   !transpiration heat (w/m2) [+ to atm]
  REAL                           , INTENT(OUT)   :: SSOIL  !ground heat flux (w/m2)   [+ to soil]
  REAL                           , INTENT(OUT)   :: TRAD   !surface radiative temperature (k)
  REAL                                           :: TS     !surface temperature (k)
  REAL                           , INTENT(OUT)   :: ECAN   !evaporation of intercepted water (mm/s)
  REAL                           , INTENT(OUT)   :: ETRAN  !transpiration rate (mm/s)
  REAL                           , INTENT(OUT)   :: EDIR   !soil surface evaporation rate (mm/s]
  REAL                           , INTENT(OUT)   :: RUNSRF !surface runoff [mm/s] 
  REAL                           , INTENT(OUT)   :: RUNSUB !baseflow (saturation excess) [mm/s]
  REAL                           , INTENT(OUT)   :: PSN    !total photosynthesis (umol co2/m2/s) [+]
  REAL                           , INTENT(OUT)   :: APAR   !photosyn active energy by canopy (w/m2)
  REAL                           , INTENT(OUT)   :: SAV    !solar rad absorbed by veg. (w/m2)
  REAL                           , INTENT(OUT)   :: SAG    !solar rad absorbed by ground (w/m2)
  REAL                           , INTENT(OUT)   :: FSNO   !snow cover fraction on the ground (-)
  REAL                           , INTENT(OUT)   :: FVEG   !green vegetation fraction [0.0-1.0]
  REAL                           , INTENT(OUT)   :: ALBEDO !surface albedo [-]
  REAL                                           :: ERRWAT !water error [kg m{-2}]
  REAL                           , INTENT(OUT)   :: QSNBOT !snowmelt out bottom of pack [mm/s]
  REAL                           , INTENT(OUT)   :: PONDING!surface ponding [mm]
  REAL                           , INTENT(OUT)   :: PONDING1!surface ponding [mm]
  REAL                           , INTENT(OUT)   :: PONDING2!surface ponding [mm]

!jref:start; output
  REAL                           , INTENT(OUT)     :: T2MV   !2-m air temperature over vegetated part [k]
  REAL                           , INTENT(OUT)     :: T2MB   !2-m air temperature over bare ground part [k]
  REAL, INTENT(OUT) :: RSSUN        !sunlit leaf stomatal resistance (s/m)
  REAL, INTENT(OUT) :: RSSHA        !shaded leaf stomatal resistance (s/m)
  REAL, INTENT(OUT) :: BGAP
  REAL, INTENT(OUT) :: WGAP
  REAL, INTENT(OUT) :: TGV
  REAL, INTENT(OUT) :: TGB
  REAL              :: Q1
  REAL, INTENT(OUT) :: EMISSI
!jref:end

! local
  INTEGER                                        :: IZ     !do-loop index
  INTEGER, DIMENSION(-NSNOW+1:NSOIL)             :: IMELT  !phase change index [1-melt; 2-freeze]
  REAL                                           :: CMC    !intercepted water (CANICE+CANLIQ) (mm)
  REAL                                           :: TAUX   !wind stress: e-w (n/m2)
  REAL                                           :: TAUY   !wind stress: n-s (n/m2)
  REAL                                           :: RHOAIR !density air (kg/m3)
!  REAL, DIMENSION(       1:    5)                :: VOCFLX !voc fluxes [ug C m-2 h-1]
  REAL, DIMENSION(-NSNOW+1:NSOIL)                :: DZSNSO !snow/soil layer thickness [m]
  REAL                                           :: THAIR  !potential temperature (k)
  REAL                                           :: QAIR   !specific humidity (kg/kg) (q2/(1+q2))
  REAL                                           :: EAIR   !vapor pressure air (pa)
  REAL, DIMENSION(       1:    2)                :: SOLAD  !incoming direct solar rad (w/m2)
  REAL, DIMENSION(       1:    2)                :: SOLAI  !incoming diffuse solar rad (w/m2)
  REAL                                           :: QPRECC !convective precipitation (mm/s)
  REAL                                           :: QPRECL !large-scale precipitation (mm/s)
  REAL                                           :: IGS    !growing season index (0=off, 1=on)
  REAL                                           :: ELAI   !leaf area index, after burying by snow
  REAL                                           :: ESAI   !stem area index, after burying by snow
  REAL                                           :: BEVAP  !soil water evaporation factor (0 - 1)
  REAL, DIMENSION(       1:NSOIL)                :: BTRANI !Soil water transpiration factor (0 - 1)
  REAL                                           :: BTRAN  !soil water transpiration factor (0 - 1)
  REAL                                           :: HTOP   !top of canopy layer (m)
  REAL                                           :: QIN    !groundwater recharge [mm/s]
  REAL                                           :: QDIS   !groundwater discharge [mm/s]
  REAL, DIMENSION(       1:NSOIL)                :: SICE   !soil ice content (m3/m3)
  REAL, DIMENSION(-NSNOW+1:    0)                :: SNICEV !partial volume ice of snow [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0)                :: SNLIQV !partial volume liq of snow [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0)                :: EPORE  !effective porosity [m3/m3]
  REAL                                           :: TOTSC  !total soil carbon (g/m2)
  REAL                                           :: TOTLB  !total living carbon (g/m2)
  REAL                                           :: T2M    !2-meter air temperature (k)
  REAL                                           :: QDEW   !ground surface dew rate [mm/s]
  REAL                                           :: QVAP   !ground surface evap. rate [mm/s]
  REAL                                           :: LATHEA !latent heat [j/kg]
  REAL                                           :: SWDOWN !downward solar [w/m2]
  REAL                                           :: QMELT  !snowmelt [mm/s]
  REAL                                           :: BEG_WB !water storage at begin of a step [mm]
  REAL,INTENT(OUT)                                              :: IRC    !canopy net LW rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: IRG    !ground net LW rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: SHC    !canopy sen. heat [w/m2]   [+ to atm]
  REAL,INTENT(OUT)                                              :: SHG    !ground sen. heat [w/m2]   [+ to atm]
  REAL,INTENT(OUT)                                              :: EVG    !ground evap. heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: GHV    !ground heat flux [w/m2]  [+ to soil]
  REAL,INTENT(OUT)                                              :: IRB    !net longwave rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: SHB    !sensible heat [w/m2]     [+ to atm]
  REAL,INTENT(OUT)                                              :: EVB    !evaporation heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: GHB    !ground heat flux [w/m2] [+ to soil]
  REAL,INTENT(OUT)                                              :: EVC    !canopy evap. heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: TR     !transpiration heat [w/m2] [+ to atm]
  REAL, INTENT(OUT)   :: FPICE   !snow fraction in precipitation

!jref:start 
  REAL                                           :: FSRV
  REAL                                           :: FSRG
  REAL,INTENT(OUT)                               :: Q2V
  REAL,INTENT(OUT)                               :: Q2B
  REAL :: Q2E
  REAL :: QFX
  REAL,INTENT(OUT)                               :: CHV    !sensible heat exchange coefficient over vegetated fraction
  REAL,INTENT(OUT)                               :: CHB    !sensible heat exchange coefficient over bare-ground
  REAL,INTENT(OUT)                               :: CHLEAF !leaf exchange coefficient
  REAL,INTENT(OUT)                               :: CHUC   !under canopy exchange coefficient
  REAL,INTENT(OUT)                               :: CHV2    !sensible heat exchange coefficient over vegetated fraction
  REAL,INTENT(OUT)                               :: CHB2    !sensible heat exchange coefficient over bare-ground
!jref:end  

! carbon
! inputs
  REAL                           , INTENT(IN)    :: CO2AIR !atmospheric co2 concentration (pa)
  REAL                           , INTENT(IN)    :: O2AIR  !atmospheric o2 concentration (pa)

! inputs and outputs : prognostic variables
  REAL                        , INTENT(INOUT)    :: LFMASS !leaf mass [g/m2]
  REAL                        , INTENT(INOUT)    :: RTMASS !mass of fine roots [g/m2]
  REAL                        , INTENT(INOUT)    :: STMASS !stem mass [g/m2]
  REAL                        , INTENT(INOUT)    :: WOOD   !mass of wood (incl. woody roots) [g/m2]
  REAL                        , INTENT(INOUT)    :: STBLCP !stable carbon in deep soil [g/m2]
  REAL                        , INTENT(INOUT)    :: FASTCP !short-lived carbon, shallow soil [g/m2]
  REAL                        , INTENT(INOUT)    :: LAI    !leaf area index [-]
  REAL                        , INTENT(INOUT)    :: SAI    !stem area index [-]

! outputs
  REAL                          , INTENT(OUT)    :: NEE    !net ecosys exchange (g/m2/s CO2)
  REAL                          , INTENT(OUT)    :: GPP    !net instantaneous assimilation [g/m2/s C]
  REAL                          , INTENT(OUT)    :: NPP    !net primary productivity [g/m2/s C]
  REAL                                           :: AUTORS !net ecosystem respiration (g/m2/s C)
  REAL                                           :: HETERS !organic respiration (g/m2/s C)
  REAL                                           :: TROOT  !root-zone averaged temperature (k)

  ! INTENT (OUT) variables need to be assigned a value.  These normally get assigned values
  ! only if DVEG == 2.
  nee = 0.0
  npp = 0.0
  gpp = 0.0

! --------------------------------------------------------------------------------------------------
! re-process atmospheric forcing

   CALL ATM (SFCPRS ,SFCTMP ,Q2     ,PRCP   ,SOLDN  ,COSZ   ,THAIR  , & 
             QAIR   ,EAIR   ,RHOAIR ,QPRECC ,QPRECL ,SOLAD  ,SOLAI  , &
             SWDOWN )

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
     DO IZ=1,NROOT
        TROOT = TROOT + STC(IZ)*DZSNSO(IZ)/(-ZSOIL(NROOT))
     ENDDO

! total water storage for water balance check
    
     IF(IST == 1) THEN
     BEG_WB = CANLIQ + CANICE + SNEQV + WA
     DO IZ = 1,NSOIL
        BEG_WB = BEG_WB + SMC(IZ) * DZSNSO(IZ) * 1000.
     END DO
     END IF

! vegetation phenology

     CALL PHENOLOGY (VEGTYP , ISURBAN, SNOWH  , TV     , LAT   , YEARLEN , JULIAN , & !in
                     LAI    , SAI    , TROOT  , HTOP  , ELAI    , ESAI   ,IGS)

!input GVF should be consistent with LAI
!     IF(DVEG == 1) THEN
!        FVEG = SHDFAC
!        IF(FVEG <= 0.05) FVEG = 0.05
!     ELSE IF (DVEG == 2 .or. DVEG == 3) THEN
!        FVEG = 1.-EXP(-0.52*(LAI+SAI))
!        IF(FVEG <= 0.05) FVEG = 0.05
!     ELSE IF (DVEG == 4) THEN
!        FVEG = SHDMAX
!        IF(FVEG <= 0.05) FVEG = 0.05
!     ELSE
!        WRITE(*,*) "-------- FATAL CALLED IN SFLX -----------"
!        CALL wrf_error_fatal("Namelist parameter DVEG unknown") 
!     ENDIF
     IF(DVEG == 1) THEN
        FVEG = SHDFAC
        IF(FVEG <= 0.01) FVEG = 0.01
     ELSE IF (DVEG == 2 .or. DVEG == 3) THEN
        FVEG = 1.-EXP(-0.52*(LAI+SAI))
        IF(FVEG <= 0.01) FVEG = 0.01
     ELSE IF (DVEG == 4 .or. DVEG == 5) THEN
        FVEG = SHDMAX
        IF(FVEG <= 0.01) FVEG = 0.01
     ELSE
        WRITE(*,*) "-------- FATAL CALLED IN SFLX -----------"
        CALL wrf_error_fatal("Namelist parameter DVEG unknown") 
     ENDIF
     IF(VEGTYP == ISURBAN .OR. VEGTYP == ISBARREN) FVEG = 0.0
     IF(ELAI+ESAI == 0.0) FVEG = 0.0

!     CALL PHENOLOGY (VEGTYP,IMONTH ,IDAY   ,SNOWH  ,TV     ,LAT   , & !in
!                     LAI   ,SAI    ,TROOT  ,                & !in
!                     HTOP  ,ELAI   ,ESAI   ,IGS    )   !out

! compute energy budget (momentum & energy fluxes and phase changes) 

    CALL ENERGY (ICE    ,VEGTYP ,IST    ,ISC    ,NSNOW  ,NSOIL  , & !in
                 ISNOW  ,NROOT  ,DT     ,RHOAIR ,SFCPRS ,QAIR   , & !in
                 SFCTMP ,THAIR  ,LWDN   ,UU     ,VV     ,ZLVL   , & !in
                 CO2AIR ,O2AIR  ,SOLAD  ,SOLAI  ,COSZ   ,IGS    , & !in
                 EAIR   ,HTOP   ,TBOT   ,ZBOT   ,ZSNSO  ,ZSOIL  , & !in
                 ELAI   ,ESAI   ,CSOIL  ,FWET   ,FOLN   ,         & !in
                 FVEG   ,                                         & !in
                 QSNOW  ,DZSNSO ,LAT    ,CANLIQ ,CANICE ,iloc, jloc , & !in
                 IMELT  ,SNICEV ,SNLIQV ,EPORE  ,T2M    ,FSNO   , & !out
                 SAV    ,SAG    ,QMELT  ,FSA    ,FSR    ,TAUX   , & !out
                 TAUY   ,FIRA   ,FSH    ,FCEV   ,FGEV   ,FCTR   , & !out
                 TRAD   ,PSN    ,APAR   ,SSOIL  ,BTRANI ,BTRAN  , & !out
                 PONDING,TS     ,LATHEA ,                         & !out
                 TV     ,TG     ,STC    ,SNOWH  ,EAH    ,TAH    , & !inout
                 SNEQVO ,SNEQV  ,SH2O   ,SMC    ,SNICE  ,SNLIQ  , & !inout
                 ALBOLD ,CM     ,CH     ,DX     ,DZ8W   ,Q2     , & !inout
                 TAUSS  ,                                         & !inout
!jref:start
                 QC     ,PBLH   ,QSFC   ,PSFC   ,ISURBAN,IZ0TLND, & !in 
                 T2MV   ,T2MB  ,FSRV   , &
                 FSRG   ,RSSUN   ,RSSHA ,BGAP   ,WGAP, TGV,TGB,&
                 Q1     ,Q2V    ,Q2B    ,Q2E    ,CHV   ,CHB     , & !out
                 EMISSI,&
		     SHG,SHC,SHB,EVG,EVB,GHV,GHB,IRG,IRC,IRB,TR,EVC,CHLEAF,CHUC,CHV2,CHB2 )                                            !out
!jref:end                            

    SICE(:) = MAX(0.0, SMC(:) - SH2O(:))   
    SNEQVO  = SNEQV

    QVAP = MAX( FGEV/LATHEA, 0.)       ! positive part of fgev
    QDEW = ABS( MIN(FGEV/LATHEA, 0.))  ! negative part of fgev
    EDIR = QVAP - QDEW

! compute water budgets (water storages, ET components, and runoff)

     CALL WATER (VEGTYP ,NSNOW  ,NSOIL  ,IMELT  ,DT     ,UU     , & !in
                 VV     ,FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & !in
                 ESAI   ,SFCTMP ,QVAP   ,QDEW   ,ZSOIL  ,BTRANI , & !in
                 FICEOLD,PONDING,TG     ,IST    ,FVEG   ,iloc,jloc , & !in
                 ISNOW  ,CANLIQ ,CANICE ,TV     ,SNOWH  ,SNEQV  , & !inout
                 SNICE  ,SNLIQ  ,STC    ,ZSNSO  ,SH2O   ,SMC    , & !inout
                 SICE   ,ZWT    ,WA     ,WT     ,DZSNSO ,WSLAKE , & !inout
                 CMC    ,ECAN   ,ETRAN  ,FWET   ,RUNSRF ,RUNSUB , & !out
                 QIN    ,QDIS   ,QSNOW  ,PONDING1       ,PONDING2,&
                 ISURBAN,QSNBOT,FPICE                             &
#ifdef WRF_HYDRO
                        ,sfcheadrt                     &
#endif
                 )  !out

!     write(*,'(a20,10F15.5)') 'SFLX:RUNOFF=',RUNSRF*DT,RUNSUB*DT,EDIR*DT

! compute carbon budgets (carbon storages and co2 & bvoc fluxes)

   IF (DVEG == 2 .OR. DVEG == 5) THEN
    CALL CARBON (NSNOW  ,NSOIL  ,VEGTYP ,NROOT  ,DT     ,ZSOIL  , & !in
                 DZSNSO ,STC    ,SMC    ,TV     ,TG     ,PSN    , & !in
                 FOLN   ,SMCMAX ,BTRAN  ,APAR   ,FVEG   ,IGS    , & !in
                 TROOT  ,IST    ,LAT    ,iloc   ,jloc   ,ISURBAN, & !in
                 LFMASS ,RTMASS ,STMASS ,WOOD   ,STBLCP ,FASTCP , & !inout
                 GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  , & !out
                 TOTLB  ,LAI    ,SAI    )                   !out
   END IF

! water and energy balance check

     CALL ERROR (SWDOWN ,FSA    ,FSR    ,FIRA   ,FSH    ,FCEV   , & !in
                 FGEV   ,FCTR   ,SSOIL  ,BEG_WB ,CANLIQ ,CANICE , & !in
                 SNEQV  ,WA     ,SMC    ,DZSNSO ,PRCP   ,ECAN   , & !in
                 ETRAN  ,EDIR   ,RUNSRF ,RUNSUB ,DT     ,NSOIL  , & !in
                 NSNOW  ,IST    ,ERRWAT ,ILOC   , JLOC  ,FVEG   , &
                 SAV    ,SAG    ,FSRV   ,FSRG)   !in ( Except ERRWAT, which is out )

! urban - jref
    QFX = ETRAN + ECAN + EDIR
    IF ( VEGTYP == ISURBAN ) THEN
       QSFC = (QFX/RHOAIR*CH) + QAIR
       Q2B = QSFC
    END IF

    IF(SWDOWN.NE.0.) THEN
      ALBEDO = FSR / SWDOWN
    ELSE
      ALBEDO = -999.9
    END IF
    

  END SUBROUTINE NOAHMP_SFLX
! ==================================================================================================
  SUBROUTINE ATM (SFCPRS ,SFCTMP ,Q2     ,PRCP   ,SOLDN  ,COSZ   ,THAIR  , &
                  QAIR   ,EAIR   ,RHOAIR ,QPRECC ,QPRECL ,SOLAD  ,SOLAI  , &
                  SWDOWN )     
! --------------------------------------------------------------------------------------------------
! re-process atmospheric forcing
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs

  REAL                          , INTENT(IN)  :: SFCPRS !pressure (pa)
  REAL                          , INTENT(IN)  :: SFCTMP !surface air temperature [k]
  REAL                          , INTENT(IN)  :: Q2     !mixing ratio (kg/kg)
  REAL                          , INTENT(IN)  :: SOLDN  !downward shortwave radiation (w/m2)
  REAL                          , INTENT(IN)  :: PRCP   !precipitation rate (kg m-2 s-1)
  REAL                          , INTENT(IN)  :: COSZ   !cosine solar zenith angle [0-1]

! outputs

  REAL                          , INTENT(OUT) :: THAIR  !potential temperature (k)
  REAL                          , INTENT(OUT) :: QAIR   !specific humidity (kg/kg) (q2/(1+q2))
  REAL                          , INTENT(OUT) :: EAIR   !vapor pressure air (pa)
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAD  !incoming direct solar radiation (w/m2)
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAI  !incoming diffuse solar radiation (w/m2)
  REAL                          , INTENT(OUT) :: QPRECC !convective precipitation (mm/s)
  REAL                          , INTENT(OUT) :: QPRECL !large-scale precipitation (mm/s)
  REAL                          , INTENT(OUT) :: RHOAIR !density air (kg/m3)
  REAL                          , INTENT(OUT) :: SWDOWN !downward solar filtered by sun angle [w/m2]

!locals

  REAL                                        :: PAIR   !atm bottom level pressure (pa)
! --------------------------------------------------------------------------------------------------

!jref: seems like PAIR should be P1000mb??
       PAIR   = SFCPRS                   ! atm bottom level pressure (pa)
       THAIR  = SFCTMP * (SFCPRS/PAIR)**(RAIR/CPAIR) 

!       QAIR   = Q2 / (1.0+Q2)           ! mixing ratio to specific humidity [kg/kg]
       QAIR   = Q2                       ! In WRF, driver converts to specific humidity

       EAIR   = QAIR*SFCPRS / (0.622+0.378*QAIR)
       RHOAIR = (SFCPRS-0.378*EAIR) / (RAIR*SFCTMP)

       QPRECC = 0.10 * PRCP          ! should be from the atmospheric model
       QPRECL = 0.90 * PRCP          ! should be from the atmospheric model

       IF(COSZ <= 0.) THEN 
          SWDOWN = 0.
       ELSE
          SWDOWN = SOLDN
       END IF 

       SOLAD(1) = SWDOWN*0.7*0.5     ! direct  vis
       SOLAD(2) = SWDOWN*0.7*0.5     ! direct  nir
       SOLAI(1) = SWDOWN*0.3*0.5     ! diffuse vis
       SOLAI(2) = SWDOWN*0.3*0.5     ! diffuse nir

  END SUBROUTINE ATM
! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE PHENOLOGY (VEGTYP , ISURBAN, SNOWH  , TV     , LAT   , YEARLEN , JULIAN , & !in
                        LAI    , SAI    , TROOT  , HTOP  , ELAI    , ESAI   , IGS)

! --------------------------------------------------------------------------------------------------
! vegetation phenology considering vegeation canopy being buries by snow and evolution in time
! --------------------------------------------------------------------------------------------------
  USE NOAHMP_VEG_PARAMETERS
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
  INTEGER                , INTENT(IN   ) :: VEGTYP !vegetation type 
  INTEGER                , INTENT(IN   ) :: ISURBAN!urban category
  REAL                   , INTENT(IN   ) :: SNOWH  !snow height [m]
  REAL                   , INTENT(IN   ) :: TV     !vegetation temperature (k)
  REAL                   , INTENT(IN   ) :: LAT    !latitude (radians)
  INTEGER                , INTENT(IN   ) :: YEARLEN!Number of days in the particular year
  REAL                   , INTENT(IN   ) :: JULIAN !Julian day of year (fractional) ( 0 <= JULIAN < YEARLEN )
  real                   , INTENT(IN   ) :: TROOT  !root-zone averaged temperature (k)
  REAL                   , INTENT(INOUT) :: LAI    !LAI, unadjusted for burying by snow
  REAL                   , INTENT(INOUT) :: SAI    !SAI, unadjusted for burying by snow

! outputs
  REAL                   , INTENT(OUT  ) :: HTOP   !top of canopy layer (m)
  REAL                   , INTENT(OUT  ) :: ELAI   !leaf area index, after burying by snow
  REAL                   , INTENT(OUT  ) :: ESAI   !stem area index, after burying by snow
  REAL                   , INTENT(OUT  ) :: IGS    !growing season index (0=off, 1=on)

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

     LAI = WT1*LAIM(VEGTYP,IT1) + WT2*LAIM(VEGTYP,IT2)
     SAI = WT1*SAIM(VEGTYP,IT1) + WT2*SAIM(VEGTYP,IT2)
  ENDIF
  IF (SAI < 0.1) SAI = 0.0  ! MB: SAI CHECK
  IF (LAI < 0.1 .OR. SAI == 0.0) LAI = 0.0  ! MB: LAI CHECK

  IF ( ( VEGTYP == ISWATER ) .OR. ( VEGTYP == ISBARREN ) .OR. ( VEGTYP == ISSNOW ) .or. ( VEGTYP == ISURBAN) ) THEN
     LAI  = 0.
     SAI  = 0.
  ENDIF

!buried by snow

     DB = MIN( MAX(SNOWH - HVB(VEGTYP),0.), HVT(VEGTYP)-HVB(VEGTYP) )
     FB = DB / MAX(1.E-06,HVT(VEGTYP)-HVB(VEGTYP))

     IF(HVT(VEGTYP)> 0. .AND. HVT(VEGTYP) <= 0.5) THEN
       SNOWHC = HVT(VEGTYP)*EXP(-SNOWH/0.1)
       FB     = MIN(SNOWH,SNOWHC)/SNOWHC
     ENDIF

     ELAI =  LAI*(1.-FB)
     ESAI =  SAI*(1.-FB)
     IF (ESAI < 0.1) ESAI = 0.0  ! MB: ESAI CHECK
     IF (ELAI < 0.1 .OR. ESAI == 0.0) ELAI = 0.0  ! MB: LAI CHECK

     IF (TV .GT. TMIN(VEGTYP)) THEN
         IGS = 1.
     ELSE
         IGS = 0.
     ENDIF

     HTOP = HVT(VEGTYP)

  END SUBROUTINE PHENOLOGY
! ==================================================================================================
  SUBROUTINE ERROR (SWDOWN ,FSA    ,FSR    ,FIRA   ,FSH    ,FCEV   , &
                    FGEV   ,FCTR   ,SSOIL  ,BEG_WB ,CANLIQ ,CANICE , &
                    SNEQV  ,WA     ,SMC    ,DZSNSO ,PRCP   ,ECAN   , &
                    ETRAN  ,EDIR   ,RUNSRF ,RUNSUB ,DT     ,NSOIL  , &
                    NSNOW  ,IST    ,ERRWAT, ILOC   ,JLOC   ,FVEG   , &
                    SAV    ,SAG    ,FSRV   ,FSRG)
! --------------------------------------------------------------------------------------------------
! check surface energy balance and water balance
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
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

  INTEGER                                     :: IZ     !do-loop index
  REAL                                        :: END_WB !water storage at end of a timestep [mm]
  !KWM REAL                                        :: ERRWAT !error in water balance [mm/timestep]
  REAL                                        :: ERRENG !error in surface energy balance [w/m2]
  REAL                                        :: ERRSW  !error in shortwave radiation balance [w/m2]
  REAL                                        :: FSRVG
  CHARACTER(len=256)                          :: message
! --------------------------------------------------------------------------------------------------
!jref:start
   ERRSW   = SWDOWN - (FSA + FSR)
!   ERRSW   = SWDOWN - (SAV+SAG + FSRV+FSRG)
!   WRITE(*,*) "ERRSW =",ERRSW
   IF (ABS(ERRSW) > 0.01) THEN            ! w/m2
   WRITE(*,*) "VEGETATION!"
   WRITE(*,*) "SWDOWN*FVEG =",SWDOWN*FVEG
   WRITE(*,*) "FVEG*(SAV+SAG) =",FVEG*SAV + SAG
   WRITE(*,*) "FVEG*(FSRV +FSRG)=",FVEG*FSRV + FSRG
   WRITE(*,*) "GROUND!"
   WRITE(*,*) "(1-.FVEG)*SWDOWN =",(1.-FVEG)*SWDOWN
   WRITE(*,*) "(1.-FVEG)*SAG =",(1.-FVEG)*SAG
   WRITE(*,*) "(1.-FVEG)*FSRG=",(1.-FVEG)*FSRG
   WRITE(*,*) "FSRV   =",FSRV
   WRITE(*,*) "FSRG   =",FSRG
   WRITE(*,*) "FSR    =",FSR
   WRITE(*,*) "SAV    =",SAV
   WRITE(*,*) "SAG    =",SAG
   WRITE(*,*) "FSA    =",FSA
!jref:end   
      WRITE(message,*) 'ERRSW =',ERRSW
      call wrf_message(trim(message))
      call wrf_error_fatal("Stop in Noah-MP")
   END IF

   ERRENG = SAV+SAG-(FIRA+FSH+FCEV+FGEV+FCTR+SSOIL)
!   ERRENG = FVEG*SAV+SAG-(FIRA+FSH+FCEV+FGEV+FCTR+SSOIL)
!   WRITE(*,*) "ERRENG =",ERRENG
   IF(ABS(ERRENG) > 0.01) THEN
      write(message,*) 'ERRENG =',ERRENG
      call wrf_message(trim(message))
      WRITE(message,'(i6,1x,i6,1x,7F10.4)')ILOC,JLOC,FSA,FIRA,FSH,FCEV,FGEV,FCTR,SSOIL
      call wrf_message(trim(message))
      call wrf_error_fatal("Energy budget problem in NOAHMP LSM")
   END IF

   IF (IST == 1) THEN                                       !soil
        END_WB = CANLIQ + CANICE + SNEQV + WA
        DO IZ = 1,NSOIL
          END_WB = END_WB + SMC(IZ) * DZSNSO(IZ) * 1000.
        END DO
        ERRWAT = END_WB-BEG_WB-(PRCP-ECAN-ETRAN-EDIR-RUNSRF-RUNSUB)*DT

#ifndef WRF_HYDRO
        IF(ABS(ERRWAT) > 0.1) THEN
           if (ERRWAT > 0) then
              call wrf_message ('The model is gaining water (ERRWAT is positive)')
           else
              call wrf_message('The model is losing water (ERRWAT is negative)')
           endif
           write(message, *) 'ERRWAT =',ERRWAT, "kg m{-2} timestep{-1}"
           call wrf_message(trim(message))
           WRITE(message, &
           '("    I      J     END_WB     BEG_WB       PRCP       ECAN       EDIR      ETRAN      RUNSRF     RUNSUB")')
           call wrf_message(trim(message))
           WRITE(message,'(i6,1x,i6,1x,2f15.3,8f11.5)')ILOC,JLOC,END_WB,BEG_WB,PRCP*DT,ECAN*DT,&
                EDIR*DT,ETRAN*DT,RUNSRF*DT,RUNSUB*DT
           call wrf_message(trim(message))
           call wrf_error_fatal("Water budget problem in NOAHMP LSM")
        END IF
#endif
   ELSE                 !KWM
      ERRWAT = 0.0      !KWM
   ENDIF

 END SUBROUTINE ERROR
! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE ENERGY (ICE    ,VEGTYP ,IST    ,ISC    ,NSNOW  ,NSOIL  , & !in
                     ISNOW  ,NROOT  ,DT     ,RHOAIR ,SFCPRS ,QAIR   , & !in
                     SFCTMP ,THAIR  ,LWDN   ,UU     ,VV     ,ZREF   , & !in
                     CO2AIR ,O2AIR  ,SOLAD  ,SOLAI  ,COSZ   ,IGS    , & !in
                     EAIR   ,HTOP   ,TBOT   ,ZBOT   ,ZSNSO  ,ZSOIL  , & !in
                     ELAI   ,ESAI   ,CSOIL  ,FWET   ,FOLN   ,         & !in
                     FVEG   ,                                         & !in
                     QSNOW  ,DZSNSO ,LAT    ,CANLIQ ,CANICE ,ILOC   , JLOC, & !in
                     IMELT  ,SNICEV ,SNLIQV ,EPORE  ,T2M    ,FSNO   , & !out
                     SAV    ,SAG    ,QMELT  ,FSA    ,FSR    ,TAUX   , & !out
                     TAUY   ,FIRA   ,FSH    ,FCEV   ,FGEV   ,FCTR   , & !out
                     TRAD   ,PSN    ,APAR   ,SSOIL  ,BTRANI ,BTRAN  , & !out
                     PONDING,TS     ,LATHEA ,                         & !out
                     TV     ,TG     ,STC    ,SNOWH  ,EAH    ,TAH    , & !inout
                     SNEQVO ,SNEQV  ,SH2O   ,SMC    ,SNICE  ,SNLIQ  , & !inout
                     ALBOLD ,CM     ,CH     ,DX     ,DZ8W   ,Q2     , &   !inout
                     TAUSS  ,                                         & !inout
!jref:start
                     QC     ,PBLH   ,QSFC   ,PSFC   ,ISURBAN,IZ0TLND, & !in 
                     T2MV   ,T2MB   ,FSRV   , &
                     FSRG   ,RSSUN  ,RSSHA  ,BGAP   ,WGAP,TGV,TGB,&
                     Q1     ,Q2V    ,Q2B    ,Q2E    ,CHV  ,CHB, EMISSI,&
		     SHG,SHC,SHB,EVG,EVB,GHV,GHB,IRG,IRC,IRB,TR,EVC,CHLEAF,CHUC,CHV2,CHB2 )   !out 
!jref:end                            

! --------------------------------------------------------------------------------------------------
! --------------------------------------------------------------------------------------------------
  USE NOAHMP_VEG_PARAMETERS
  USE NOAHMP_RAD_PARAMETERS
! --------------------------------------------------------------------------------------------------
! we use different approaches to deal with subgrid features of radiation transfer and turbulent
! transfer. We use 'tile' approach to compute turbulent fluxes, while we use modified two-
! stream to compute radiation transfer. Tile approach, assemblying vegetation canopies together,
! may expose too much ground surfaces (either covered by snow or grass) to solar radiation. The
! modified two-stream assumes vegetation covers fully the gridcell but with gaps between tree
! crowns.
! --------------------------------------------------------------------------------------------------
! turbulence transfer : 'tile' approach to compute energy fluxes in vegetated fraction and
!                         bare fraction separately and then sum them up weighted by fraction
!                     --------------------------------------
!                    / O  O  O  O  O  O  O  O  /          / 
!                   /  |  |  |  |  |  |  |  | /          /
!                  / O  O  O  O  O  O  O  O  /          /
!                 /  |  |  |tile1|  |  |  | /  tile2   /
!                / O  O  O  O  O  O  O  O  /  bare    /
!               /  |  |  | vegetated |  | /          /
!              / O  O  O  O  O  O  O  O  /          /
!             /  |  |  |  |  |  |  |  | /          /
!            --------------------------------------
! --------------------------------------------------------------------------------------------------
! radiation transfer : modified two-stream (Yang and Friedl, 2003, JGR; Niu ang Yang, 2004, JGR)
!                     --------------------------------------  two-stream treats leaves as
!                    /   O   O   O   O   O   O   O   O    /  cloud over the entire grid-cell,
!                   /    |   |   |   |   |   |   |   |   / while the modified two-stream 
!                  /   O   O   O   O   O   O   O   O    / aggregates cloudy leaves into  
!                 /    |   |   |   |   |   |   |   |   / tree crowns with gaps (as shown in
!                /   O   O   O   O   O   O   O   O    / the left figure). We assume these
!               /    |   |   |   |   |   |   |   |   / tree crowns are evenly distributed
!              /   O   O   O   O   O   O   O   O    / within the gridcell with 100% veg
!             /    |   |   |   |   |   |   |   |   / fraction, but with gaps. The 'tile'
!            -------------------------------------- approach overlaps too much shadows.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
  integer                           , INTENT(IN)    :: ILOC
  integer                           , INTENT(IN)    :: JLOC
  INTEGER                           , INTENT(IN)    :: ICE    !ice (ice = 1)
  INTEGER                           , INTENT(IN)    :: VEGTYP !vegetation physiology type
  INTEGER                           , INTENT(IN)    :: IST    !surface type: 1->soil; 2->lake
  INTEGER                           , INTENT(IN)    :: ISC    !soil color type (1-lighest; 8-darkest)
  INTEGER                           , INTENT(IN)    :: NSNOW  !maximum no. of snow layers        
  INTEGER                           , INTENT(IN)    :: NSOIL  !number of soil layers
  INTEGER                           , INTENT(IN)    :: NROOT  !number of root layers
  INTEGER                           , INTENT(IN)    :: ISNOW  !actual no. of snow layers
  REAL                              , INTENT(IN)    :: DT     !time step [sec]
  REAL                              , INTENT(IN)    :: QSNOW  !snowfall on the ground (mm/s)
  REAL                              , INTENT(IN)    :: RHOAIR !density air (kg/m3)
  REAL                              , INTENT(IN)    :: EAIR   !vapor pressure air (pa)
  REAL                              , INTENT(IN)    :: SFCPRS !pressure (pa)
  REAL                              , INTENT(IN)    :: QAIR   !specific humidity (kg/kg)
  REAL                              , INTENT(IN)    :: SFCTMP !air temperature (k)
  REAL                              , INTENT(IN)    :: THAIR  !potential temperature (k)
  REAL                              , INTENT(IN)    :: LWDN   !downward longwave radiation (w/m2)
  REAL                              , INTENT(IN)    :: UU     !wind speed in e-w dir (m/s)
  REAL                              , INTENT(IN)    :: VV     !wind speed in n-s dir (m/s)
  REAL   , DIMENSION(       1:    2), INTENT(IN)    :: SOLAD  !incoming direct solar rad. (w/m2)
  REAL   , DIMENSION(       1:    2), INTENT(IN)    :: SOLAI  !incoming diffuse solar rad. (w/m2)
  REAL                              , INTENT(IN)    :: COSZ   !cosine solar zenith angle (0-1)
  REAL                              , INTENT(IN)    :: ELAI   !LAI adjusted for burying by snow
  REAL                              , INTENT(IN)    :: ESAI   !LAI adjusted for burying by snow
  REAL                              , INTENT(IN)    :: CSOIL  !vol. soil heat capacity [j/m3/k]
  REAL                              , INTENT(IN)    :: FWET   !fraction of canopy that is wet [-]
  REAL                              , INTENT(IN)    :: HTOP   !top of canopy layer (m)
  REAL                              , INTENT(IN)    :: FVEG   !greeness vegetation fraction (-)
  REAL                              , INTENT(IN)    :: LAT    !latitude (radians)
  REAL                              , INTENT(IN)    :: CANLIQ !canopy-intercepted liquid water (mm)
  REAL                              , INTENT(IN)    :: CANICE !canopy-intercepted ice mass (mm)
  REAL                              , INTENT(IN)    :: FOLN   !foliage nitrogen (%)
  REAL                              , INTENT(IN)    :: CO2AIR !atmospheric co2 concentration (pa)
  REAL                              , INTENT(IN)    :: O2AIR  !atmospheric o2 concentration (pa)
  REAL                              , INTENT(IN)    :: IGS    !growing season index (0=off, 1=on)

  REAL                              , INTENT(IN)    :: ZREF   !reference height (m)
  REAL                              , INTENT(IN)    :: TBOT   !bottom condition for soil temp. (k) 
  REAL                              , INTENT(IN)    :: ZBOT   !depth for TBOT [m]
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: ZSNSO  !layer-bottom depth from snow surf [m]
  REAL   , DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !layer-bottom depth from soil surf [m]
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: DZSNSO !depth of snow & soil layer-bottom [m]

!jref:start; in 
  INTEGER                           , INTENT(IN)    :: ISURBAN
  INTEGER                           , INTENT(IN)    :: IZ0TLND
  REAL                              , INTENT(IN)    :: QC     !cloud water mixing ratio
  REAL                              , INTENT(IN)    :: PBLH   !planetary boundary layer height
  REAL                              , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer
  REAL                              , INTENT(IN)    :: PSFC   !pressure at lowest model layer
  REAL                              , INTENT(IN)    :: DX     !horisontal resolution
  REAL                              , INTENT(IN)    :: DZ8W   !thickness of lowest layer
  REAL                              , INTENT(IN)    :: Q2     !mixing ratio (kg/kg)
!jref:end

! outputs
  INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT)   :: IMELT  !phase change index [1-melt; 2-freeze]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: SNICEV !partial volume ice [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: SNLIQV !partial volume liq. water [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: EPORE  !effective porosity [m3/m3]
  REAL                              , INTENT(OUT)   :: FSNO   !snow cover fraction (-)
  REAL                              , INTENT(OUT)   :: QMELT  !snowmelt [mm/s]
  REAL                              , INTENT(OUT)   :: PONDING!pounding at ground [mm]
  REAL                              , INTENT(OUT)   :: SAV    !solar rad. absorbed by veg. (w/m2)
  REAL                              , INTENT(OUT)   :: SAG    !solar rad. absorbed by ground (w/m2)
  REAL                              , INTENT(OUT)   :: FSA    !tot. absorbed solar radiation (w/m2)
  REAL                              , INTENT(OUT)   :: FSR    !tot. reflected solar radiation (w/m2)
  REAL                              , INTENT(OUT)   :: TAUX   !wind stress: e-w (n/m2)
  REAL                              , INTENT(OUT)   :: TAUY   !wind stress: n-s (n/m2)
  REAL                              , INTENT(OUT)   :: FIRA   !total net LW. rad (w/m2)   [+ to atm]
  REAL                              , INTENT(OUT)   :: FSH    !total sensible heat (w/m2) [+ to atm]
  REAL                              , INTENT(OUT)   :: FCEV   !canopy evaporation (w/m2)  [+ to atm]
  REAL                              , INTENT(OUT)   :: FGEV   !ground evaporation (w/m2)  [+ to atm]
  REAL                              , INTENT(OUT)   :: FCTR   !transpiration (w/m2)       [+ to atm]
  REAL                              , INTENT(OUT)   :: TRAD   !radiative temperature (k)
  REAL                              , INTENT(OUT)   :: T2M    !2 m height air temperature (k)
  REAL                              , INTENT(OUT)   :: PSN    !total photosyn. (umolco2/m2/s) [+]
  REAL                              , INTENT(OUT)   :: APAR   !total photosyn. active energy (w/m2)
  REAL                              , INTENT(OUT)   :: SSOIL  !ground heat flux (w/m2)   [+ to soil]
  REAL   , DIMENSION(       1:NSOIL), INTENT(OUT)   :: BTRANI !soil water transpiration factor (0-1)
  REAL                              , INTENT(OUT)   :: BTRAN  !soil water transpiration factor (0-1)
  REAL                              , INTENT(OUT)   :: LATHEA !latent heat vap./sublimation (j/kg)

!jref:start  
  REAL                              , INTENT(OUT)   :: FSRV    !veg. reflected solar radiation (w/m2)
  REAL                              , INTENT(OUT)   :: FSRG    !ground reflected solar radiation (w/m2)
  REAL, INTENT(OUT) :: RSSUN        !sunlit leaf stomatal resistance (s/m)
  REAL, INTENT(OUT) :: RSSHA        !shaded leaf stomatal resistance (s/m)
!jref:end - out for debug  

!jref:start; output
  REAL                              , INTENT(OUT)   :: T2MV   !2-m air temperature over vegetated part [k]
  REAL                              , INTENT(OUT)   :: T2MB   !2-m air temperature over bare ground part [k]
  REAL                              , INTENT(OUT)   :: BGAP
  REAL                              , INTENT(OUT)   :: WGAP
!jref:end

! input & output
  REAL                              , INTENT(INOUT) :: TS     !surface temperature (k)
  REAL                              , INTENT(INOUT) :: TV     !vegetation temperature (k)
  REAL                              , INTENT(INOUT) :: TG     !ground temperature (k)
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow/soil temperature [k]
  REAL                              , INTENT(INOUT) :: SNOWH  !snow height [m]
  REAL                              , INTENT(INOUT) :: SNEQV  !snow mass (mm)
  REAL                              , INTENT(INOUT) :: SNEQVO !snow mass at last time step (mm)
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !liquid soil moisture [m3/m3]
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow ice mass (kg/m2)
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow liq mass (kg/m2)
  REAL                              , INTENT(INOUT) :: EAH    !canopy air vapor pressure (pa)
  REAL                              , INTENT(INOUT) :: TAH    !canopy air temperature (k)
  REAL                              , INTENT(INOUT) :: ALBOLD !snow albedo at last time step(CLASS type)
  REAL                              , INTENT(INOUT) :: TAUSS  !non-dimensional snow age
  REAL                              , INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL                              , INTENT(INOUT) :: CH     !sensible heat exchange coefficient
  REAL                              , INTENT(INOUT) :: Q1
!  REAL                                              :: Q2E
  REAL,                               INTENT(OUT)   :: EMISSI

! local
  INTEGER                                           :: IZ     !do-loop index
  LOGICAL                                           :: VEG    !true if vegetated surface
  REAL                                              :: UR     !wind speed at height ZLVL (m/s)
  REAL                                              :: ZLVL   !reference height (m)
  REAL                                              :: FSUN   !sunlit fraction of canopy [-]
  REAL                                              :: RB     !leaf boundary layer resistance (s/m)
  REAL                                              :: RSURF  !ground surface resistance (s/m)
  REAL                                              :: L_RSURF!Dry-layer thickness for computing RSURF (Sakaguchi and Zeng, 2009)
  REAL                                              :: D_RSURF!Reduced vapor diffusivity in soil for computing RSURF (SZ09)
  REAL                                              :: BEVAP  !soil water evaporation factor (0- 1)
  REAL                                              :: MOL    !Monin-Obukhov length (m)
  REAL                                              :: VAI    !sum of LAI  + stem area index [m2/m2]
  REAL                                              :: CWP    !canopy wind extinction parameter
  REAL                                              :: ZPD    !zero plane displacement (m)
  REAL                                              :: Z0M    !z0 momentum (m)
  REAL                                              :: ZPDG   !zero plane displacement (m)
  REAL                                              :: Z0MG   !z0 momentum, ground (m)
  REAL                                              :: EMV    !vegetation emissivity
  REAL                                              :: EMG    !ground emissivity
  REAL                                              :: FIRE   !emitted IR (w/m2)

  REAL                                              :: LAISUN !sunlit leaf area index (m2/m2)
  REAL                                              :: LAISHA !shaded leaf area index (m2/m2)
  REAL                                              :: PSNSUN !sunlit photosynthesis (umolco2/m2/s)
  REAL                                              :: PSNSHA !shaded photosynthesis (umolco2/m2/s)
!jref:start - for debug  
!  REAL                                              :: RSSUN  !sunlit stomatal resistance (s/m)
!  REAL                                              :: RSSHA  !shaded stomatal resistance (s/m)
!jref:end - for debug
  REAL                                              :: PARSUN !par absorbed per sunlit LAI (w/m2)
  REAL                                              :: PARSHA !par absorbed per shaded LAI (w/m2)

  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: FACT   !temporary used in phase change
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: DF     !thermal conductivity [w/m/k]
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: HCPCT  !heat capacity [j/m3/k]
  REAL                                              :: BDSNO  !bulk density of snow (kg/m3)
  REAL                                              :: FMELT  !melting factor for snow cover frac
  REAL                                              :: GX     !temporary variable
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: PHI    !light through water (w/m2)
  REAL                                              :: GAMMA  !psychrometric constant (pa/k)
  REAL                                              :: PSI    !surface layer soil matrix potential (m)
  REAL                                              :: RHSUR  !raltive humidity in surface soil/snow air space (-)

! temperature and fluxes over vegetated fraction

  REAL                                              :: TAUXV  !wind stress: e-w dir [n/m2]
  REAL                                              :: TAUYV  !wind stress: n-s dir [n/m2]
  REAL,INTENT(OUT)                                              :: IRC    !canopy net LW rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: IRG    !ground net LW rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: SHC    !canopy sen. heat [w/m2]   [+ to atm]
  REAL,INTENT(OUT)                                              :: SHG    !ground sen. heat [w/m2]   [+ to atm]
!jref:start  
  REAL,INTENT(OUT)                                  :: Q2V
  REAL,INTENT(OUT)                                  :: Q2B
  REAL,INTENT(OUT)                                  :: Q2E
!jref:end  
  REAL,INTENT(OUT)                                              :: EVC    !canopy evap. heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: EVG    !ground evap. heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: TR     !transpiration heat [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: GHV    !ground heat flux [w/m2]  [+ to soil]
  REAL,INTENT(OUT)                                  :: TGV    !ground surface temp. [k]
  REAL                                              :: CMV    !momentum drag coefficient
  REAL,INTENT(OUT)                                  :: CHV    !sensible heat exchange coefficient

! temperature and fluxes over bare soil fraction

  REAL                                              :: TAUXB  !wind stress: e-w dir [n/m2]
  REAL                                              :: TAUYB  !wind stress: n-s dir [n/m2]
  REAL,INTENT(OUT)                                              :: IRB    !net longwave rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: SHB    !sensible heat [w/m2]     [+ to atm]
  REAL,INTENT(OUT)                                              :: EVB    !evaporation heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: GHB    !ground heat flux [w/m2] [+ to soil]
  REAL,INTENT(OUT)                                  :: TGB    !ground surface temp. [k]
  REAL                                              :: CMB    !momentum drag coefficient
  REAL,INTENT(OUT)                                  :: CHB    !sensible heat exchange coefficient
  REAL,INTENT(OUT)                                  :: CHLEAF !leaf exchange coefficient
  REAL,INTENT(OUT)                                  :: CHUC   !under canopy exchange coefficient
!jref:start  
  REAL,INTENT(OUT)                                  :: CHV2    !sensible heat conductance, canopy air to ZLVL air (m/s)
  REAL,INTENT(OUT)                                  :: CHB2    !sensible heat conductance, canopy air to ZLVL air (m/s)
  REAL                                  :: noahmpres

!jref:end  

  REAL, PARAMETER                   :: MPE    = 1.E-6
  REAL, PARAMETER                   :: PSIWLT = -150.  !metric potential for wilting point (m)
  REAL, PARAMETER                   :: Z0     = 0.01   ! Bare-soil roughness length (m) (i.e., under the canopy)

! ---------------------------------------------------------------------------------------------------
! initialize fluxes from veg. fraction

    TAUXV     = 0.    
    TAUYV     = 0.
    IRC       = 0.
    SHC       = 0.
    IRG       = 0.
    SHG       = 0.
    EVG       = 0.       
    EVC       = 0.
    TR        = 0.
    GHV       = 0.       
    PSNSUN    = 0.
    PSNSHA    = 0.
    T2MV      = 0.
    Q2V       = 0.
    CHV       = 0.
    CHLEAF    = 0.
    CHUC      = 0.
    CHV2      = 0.

! wind speed at reference height: ur >= 1

    UR = MAX( SQRT(UU**2.+VV**2.), 1. )

! vegetated or non-vegetated

    VAI = ELAI + ESAI
    VEG = .FALSE.
    IF(VAI > 0.) VEG = .TRUE.

! ground snow cover fraction [Niu and Yang, 2007, JGR]

     FSNO = 0.
     IF(SNOWH.GT.0.)  THEN
         BDSNO    = SNEQV / SNOWH
         FMELT    = (BDSNO/100.)**M
         FSNO     = TANH( SNOWH /(2.5* Z0 * FMELT))
     ENDIF

! ground roughness length

     IF(IST == 2) THEN
       IF(TG .LE. TFRZ) THEN
         Z0MG = 0.01 * (1.0-FSNO) + FSNO * Z0SNO
       ELSE
         Z0MG = 0.01  
       END IF
     ELSE
       Z0MG = Z0 * (1.0-FSNO) + FSNO * Z0SNO
     END IF

! roughness length and displacement height

     ZPDG  = SNOWH
     IF(VEG) THEN
        Z0M  = Z0MVT(VEGTYP)
        ZPD  = 0.65 * HTOP
        IF(SNOWH.GT.ZPD) ZPD  = SNOWH
     ELSE
        Z0M  = Z0MG
        ZPD  = ZPDG
     END IF

     ZLVL = MAX(ZPD,HTOP) + ZREF
     IF(ZPDG >= ZLVL) ZLVL = ZPDG + ZREF
!     UR   = UR*LOG(ZLVL/Z0M)/LOG(10./Z0M)       !input UR is at 10m

! canopy wind absorption coeffcient

     CWP = CWPVT(VEGTYP)

! Thermal properties of soil, snow, lake, and frozen soil

  CALL THERMOPROP (NSOIL   ,NSNOW   ,ISNOW   ,IST     ,DZSNSO  , & !in
                   DT      ,SNOWH   ,SNICE   ,SNLIQ   ,CSOIL   , & !in
                   SMC     ,SH2O    ,TG      ,STC     ,UR      , & !in
                   LAT     ,Z0M     ,ZLVL    ,VEGTYP  ,ISURBAN , & !in
                   DF      ,HCPCT   ,SNICEV  ,SNLIQV  ,EPORE   , & !out
                   FACT    )                              !out

! Solar radiation: absorbed & reflected by the ground and canopy

  CALL  RADIATION (VEGTYP  ,IST     ,ISC     ,ICE     ,NSOIL   , & !in 
                   SNEQVO  ,SNEQV   ,DT      ,COSZ    ,SNOWH   , & !in
                   TG      ,TV      ,FSNO    ,QSNOW   ,FWET    , & !in
                   ELAI    ,ESAI    ,SMC     ,SOLAD   ,SOLAI   , & !in
                   FVEG    ,ILOC    ,JLOC    ,                   & !in
                   ALBOLD  ,TAUSS   ,                            & !inout
                   FSUN    ,LAISUN  ,LAISHA  ,PARSUN  ,PARSHA  , & !out
                   SAV     ,SAG     ,FSR     ,FSA     ,FSRV    , & 
                   FSRG    ,BGAP    ,WGAP    )            !out

! vegetation and ground emissivity

     EMV = 1. - EXP(-(ELAI+ESAI)/1.0)
     IF (ICE == 1) THEN
       EMG = 0.98*(1.-FSNO) + 1.0*FSNO
     ELSE
       EMG = EG(IST)*(1.-FSNO) + 1.0*FSNO
     END IF

! soil moisture factor controlling stomatal resistance
   
     BTRAN = 0.

     IF(IST ==1 ) THEN
       DO IZ = 1, NROOT
          IF(OPT_BTR == 1) then                  ! Noah
            GX    = (SH2O(IZ)-SMCWLT) / (SMCREF-SMCWLT)
          END IF
          IF(OPT_BTR == 2) then                  ! CLM
            PSI   = MAX(PSIWLT,-PSISAT*(MAX(0.01,SH2O(IZ))/SMCMAX)**(-BEXP) )
            GX    = (1.-PSI/PSIWLT)/(1.+PSISAT/PSIWLT)
          END IF
          IF(OPT_BTR == 3) then                  ! SSiB
            PSI   = MAX(PSIWLT,-PSISAT*(MAX(0.01,SH2O(IZ))/SMCMAX)**(-BEXP) )
            GX    = 1.-EXP(-5.8*(LOG(PSIWLT/PSI))) 
          END IF
       
          GX = MIN(1.,MAX(0.,GX))
          BTRANI(IZ) = MAX(MPE,DZSNSO(IZ) / (-ZSOIL(NROOT)) * GX)
          BTRAN      = BTRAN + BTRANI(IZ)
       END DO
       BTRAN = MAX(MPE,BTRAN)

       BTRANI(1:NROOT) = BTRANI(1:NROOT)/BTRAN
     END IF

! soil surface resistance for ground evap.

     BEVAP = MAX(0.0,SH2O(1)/SMCMAX)
     IF(IST == 2) THEN
       RSURF = 1.          ! avoid being divided by 0
       RHSUR = 1.0
     ELSE

        ! RSURF based on Sakaguchi and Zeng, 2009
        ! taking the "residual water content" to be the wilting point, 
        ! and correcting the exponent on the D term (typo in SZ09 ?)
        L_RSURF = (-ZSOIL(1)) * ( exp ( (1.0 - MIN(1.0,SH2O(1)/SMCMAX)) ** 5 ) - 1.0 ) / ( 2.71828 - 1.0 ) 
        D_RSURF = 2.2E-5 * SMCMAX * SMCMAX * ( 1.0 - SMCWLT / SMCMAX ) ** (2.0+3.0/BEXP)
        RSURF = L_RSURF / D_RSURF

        ! Older RSURF computations:
        !    RSURF = FSNO * 1. + (1.-FSNO)* EXP(8.25-4.225*BEVAP) !Sellers (1992)
        !    RSURF = FSNO * 1. + (1.-FSNO)* EXP(8.25-6.0  *BEVAP) !adjusted to decrease RSURF for wet soil

       IF(SH2O(1) < 0.01 .and. SNOWH == 0.) RSURF = 1.E6
       PSI   = -PSISAT*(MAX(0.01,SH2O(1))/SMCMAX)**(-BEXP)   
       RHSUR = FSNO + (1.-FSNO) * EXP(PSI*GRAV/(RW*TG)) 
     END IF

! urban - jref 
     IF (VEGTYP == ISURBAN .and. SNOWH == 0. ) THEN
        RSURF = 1.E6
     ENDIF

! set psychrometric constant

     IF (SFCTMP .GT. TFRZ) THEN
        LATHEA = HVAP
     ELSE
        LATHEA = HSUB
     END IF
     GAMMA = CPAIR*SFCPRS/(0.622*LATHEA)

! Surface temperatures of the ground and canopy and energy fluxes

    IF (VEG .AND. FVEG > 0) THEN 
    TGV = TG
    CMV = CM
    CHV = CH
    CALL VEGE_FLUX (NSNOW   ,NSOIL   ,ISNOW   ,VEGTYP  ,VEG     , & !in
                    DT      ,SAV     ,SAG     ,LWDN    ,UR      , & !in
                    UU      ,VV      ,SFCTMP  ,THAIR   ,QAIR    , & !in
                    EAIR    ,RHOAIR  ,SNOWH   ,VAI     ,GAMMA   , & !in
                    FWET    ,LAISUN  ,LAISHA  ,CWP     ,DZSNSO  , & !in
                    HTOP    ,ZLVL    ,ZPD     ,Z0M     ,FVEG    , & !in
                    Z0MG    ,EMV     ,EMG     ,CANLIQ           , & !in
                    CANICE  ,STC     ,DF      ,RSSUN   ,RSSHA   , & !in
                    RSURF   ,LATHEA  ,PARSUN  ,PARSHA  ,IGS     , & !in
                    FOLN    ,CO2AIR  ,O2AIR   ,BTRAN   ,SFCPRS  , & !in
                    RHSUR   ,ILOC    ,JLOC    ,Q2      , & !in
                    EAH     ,TAH     ,TV      ,TGV     ,CMV     , & !inout
                    CHV     ,DX      ,DZ8W    ,                   & !inout
                    TAUXV   ,TAUYV   ,IRG     ,IRC     ,SHG     , & !out
                    SHC     ,EVG     ,EVC     ,TR      ,GHV     , & !out
                    T2MV    ,PSNSUN  ,PSNSHA  ,                   & !out
!jref:start
                    QC      ,PBLH    ,QSFC    ,PSFC    ,ISURBAN , & !in
                    IZ0TLND ,Q2V     ,CHV2, CHLEAF, CHUC)               !inout 
!jref:end                            
    END IF

    TGB = TG
    CMB = CM
    CHB = CH
    CALL BARE_FLUX (NSNOW   ,NSOIL   ,ISNOW   ,DT      ,SAG     , & !in
                    LWDN    ,UR      ,UU      ,VV      ,SFCTMP  , & !in
                    THAIR   ,QAIR    ,EAIR    ,RHOAIR  ,SNOWH   , & !in
                    DZSNSO  ,ZLVL    ,ZPDG    ,Z0MG    ,          & !in
                    EMG     ,STC     ,DF      ,RSURF   ,LATHEA  , & !in
                    GAMMA   ,RHSUR   ,ILOC    ,JLOC    ,Q2      , & !in
                    TGB     ,CMB     ,CHB     ,                   & !inout
                    TAUXB   ,TAUYB   ,IRB     ,SHB     ,EVB     , & !out
                    GHB     ,T2MB    ,DX      ,DZ8W    ,VEGTYP  , & !out
!jref:start
                    QC      ,PBLH    ,QSFC    ,PSFC    ,ISURBAN , & !in
                    IZ0TLND ,SFCPRS  ,Q2B,   CHB2)                          !in 
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
        TG    = FVEG * TGV       + (1.0 - FVEG) * TGB
        T2M   = FVEG * T2MV      + (1.0 - FVEG) * T2MB
        TS    = FVEG * TV        + (1.0 - FVEG) * TGB
        CM    = FVEG * CMV       + (1.0 - FVEG) * CMB      ! better way to average?
        CH    = FVEG * CHV       + (1.0 - FVEG) * CHB
        Q1    = FVEG * (EAH*0.622/(SFCPRS - 0.378*EAH)) + (1.0 - FVEG)*QSFC
        Q2E   = FVEG * Q2V       + (1.0 - FVEG) * Q2B
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
        TS    = TG
        CM    = CMB
        CH    = CHB
        Q1    = QSFC
        Q2E   = Q2B
        RSSUN = 0.0
        RSSHA = 0.0
        TGV   = TGB
        CHV   = CHB
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

    CALL TSNOSOI (ICE     ,NSOIL   ,NSNOW   ,ISNOW   ,IST     , & !in
                  TBOT    ,ZSNSO   ,SSOIL   ,DF      ,HCPCT   , & !in
                  ZBOT    ,SAG     ,DT      ,SNOWH   ,DZSNSO  , & !in
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

 CALL PHASECHANGE (NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & !in
                   DZSNSO  ,HCPCT   ,IST     ,ILOC    ,JLOC    , & !in
                   STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & !inout
                   SMC     ,SH2O    ,                            & !inout
                   QMELT   ,IMELT   ,PONDING )                     !out


  END SUBROUTINE ENERGY
! ==================================================================================================
  SUBROUTINE THERMOPROP (NSOIL   ,NSNOW   ,ISNOW   ,IST     ,DZSNSO  , & !in
                         DT      ,SNOWH   ,SNICE   ,SNLIQ   ,CSOIL   , & !in
                         SMC     ,SH2O    ,TG      ,STC     ,UR      , & !in
                         LAT     ,Z0M     ,ZLVL    ,VEGTYP  ,ISURBAN , & !in
                         DF      ,HCPCT   ,SNICEV  ,SNLIQV  ,EPORE   , & !out
                         FACT    )                                       !out
! ------------------------------------------------------------------------------------------------- 
! -------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
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
  REAL                           , INTENT(IN)  :: CSOIL   !vol. soil heat capacity [j/m3/k]
  REAL,                            INTENT(IN)  :: TG      !surface temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: STC     !snow/soil/lake temp. (k)
  REAL,                            INTENT(IN)  :: UR      !wind speed at ZLVL (m/s)
  REAL,                            INTENT(IN)  :: LAT     !latitude (radians)
  REAL,                            INTENT(IN)  :: Z0M     !roughness length (m)
  REAL,                            INTENT(IN)  :: ZLVL    !reference height (m)
  INTEGER                        , INTENT(IN)  :: VEGTYP  !vegtyp type
  INTEGER                        , INTENT(IN)  :: ISURBAN !urban type

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

    CALL CSNOW (ISNOW   ,NSNOW   ,NSOIL   ,SNICE   ,SNLIQ   ,DZSNSO  , & !in
                TKSNO   ,CVSNO   ,SNICEV  ,SNLIQV  ,EPORE   )   !out

    DO IZ = ISNOW+1, 0
      DF   (IZ) = TKSNO(IZ)
      HCPCT(IZ) = CVSNO(IZ)
    END DO

! compute soil thermal properties

    DO  IZ = 1, NSOIL
       SICE(IZ)  = SMC(IZ) - SH2O(IZ)
       HCPCT(IZ) = SH2O(IZ)*CWAT + (1.0-SMCMAX)*CSOIL &
                + (SMCMAX-SMC(IZ))*CPAIR + SICE(IZ)*CICE
       CALL TDFCND (DF(IZ), SMC(IZ), SH2O(IZ))
    END DO
       
    IF ( VEGTYP == ISURBAN ) THEN
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
! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE CSNOW (ISNOW   ,NSNOW   ,NSOIL   ,SNICE   ,SNLIQ   ,DZSNSO  , & !in
                    TKSNO   ,CVSNO   ,SNICEV  ,SNLIQV  ,EPORE   )   !out
! --------------------------------------------------------------------------------------------------
! Snow bulk density,volumetric capacity, and thermal conductivity
!---------------------------------------------------------------------------------------------------
  IMPLICIT NONE
!---------------------------------------------------------------------------------------------------
! inputs

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
!===================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE TDFCND ( DF, SMC, SH2O)
! --------------------------------------------------------------------------------------------------
! Calculate thermal diffusivity and conductivity of the soil.
! Peters-Lidard approach (Peters-Lidard et al., 1998)
! --------------------------------------------------------------------------------------------------
! Code history:
! June 2001 changes: frozen soil condition.
! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE
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
    SATRATIO = SMC / SMCMAX
    THKW = 0.57
!      IF (QUARTZ .LE. 0.2) THKO = 3.0
    THKO = 2.0
! SOLIDS' CONDUCTIVITY
! QUARTZ' CONDUCTIVITY
    THKQTZ = 7.7

! UNFROZEN FRACTION (FROM 1., i.e., 100%LIQUID, TO 0. (100% FROZEN))
    THKS = (THKQTZ ** QUARTZ)* (THKO ** (1. - QUARTZ))

! UNFROZEN VOLUME FOR SATURATION (POROSITY*XUNFROZ)
    XUNFROZ = SH2O / SMC
! SATURATED THERMAL CONDUCTIVITY
    XU = XUNFROZ * SMCMAX

! DRY DENSITY IN KG/M3
    THKSAT = THKS ** (1. - SMCMAX)* TKICE ** (SMCMAX - XU)* THKW **   &
         (XU)

! DRY THERMAL CONDUCTIVITY IN W.M-1.K-1
    GAMMD = (1. - SMCMAX)*2700.

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
! ==================================================================================================
  SUBROUTINE RADIATION (VEGTYP  ,IST     ,ISC     ,ICE     ,NSOIL   , & !in
                        SNEQVO  ,SNEQV   ,DT      ,COSZ    ,SNOWH   , & !in
                        TG      ,TV      ,FSNO    ,QSNOW   ,FWET    , & !in
                        ELAI    ,ESAI    ,SMC     ,SOLAD   ,SOLAI   , & !in
                        FVEG    ,ILOC    ,JLOC    ,                   & !in
                        ALBOLD  ,TAUSS   ,                            & !inout
                        FSUN    ,LAISUN  ,LAISHA  ,PARSUN  ,PARSHA  , & !out
                        SAV     ,SAG     ,FSR     ,FSA     ,FSRV    , &
                        FSRG    ,BGAP    ,WGAP)            !out
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  INTEGER, INTENT(IN)                  :: ILOC
  INTEGER, INTENT(IN)                  :: JLOC
  INTEGER, INTENT(IN)                  :: VEGTYP !vegetation type
  INTEGER, INTENT(IN)                  :: IST    !surface type
  INTEGER, INTENT(IN)                  :: ISC    !soil color type (1-lighest; 8-darkest)
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

   CALL ALBEDO (VEGTYP ,IST    ,ISC    ,ICE    ,NSOIL  , & !in
                DT     ,COSZ   ,FAGE   ,ELAI   ,ESAI   , & !in
                TG     ,TV     ,SNOWH  ,FSNO   ,FWET   , & !in
                SMC    ,SNEQVO ,SNEQV  ,QSNOW  ,FVEG   , & !in
                ILOC   ,JLOC   ,                         & !in
                ALBOLD ,TAUSS                          , & !inout
                ALBGRD ,ALBGRI ,ALBD   ,ALBI   ,FABD   , & !out
                FABI   ,FTDD   ,FTID   ,FTII   ,FSUN   , & !)   !out
                FREVI  ,FREVD   ,FREGD ,FREGI  ,BGAP   , & !inout
                WGAP)

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

   CALL SURRAD (MPE    ,FSUN   ,FSHA   ,ELAI   ,VAI    , & !in
                LAISUN ,LAISHA ,SOLAD  ,SOLAI  ,FABD   , & !in
                FABI   ,FTDD   ,FTID   ,FTII   ,ALBGRD , & !in
                ALBGRI ,ALBD   ,ALBI   ,ILOC   ,JLOC   , & !in
                PARSUN ,PARSHA ,SAV    ,SAG    ,FSA    , & !out
                FSR    ,                                 & !out
                FREVI  ,FREVD  ,FREGD  ,FREGI  ,FSRV   , & !inout
                FSRG)

  END SUBROUTINE RADIATION
! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE ALBEDO (VEGTYP ,IST    ,ISC    ,ICE    ,NSOIL  , & !in
                     DT     ,COSZ   ,FAGE   ,ELAI   ,ESAI   , & !in
                     TG     ,TV     ,SNOWH  ,FSNO   ,FWET   , & !in
                     SMC    ,SNEQVO ,SNEQV  ,QSNOW  ,FVEG   , & !in
                     ILOC   ,JLOC   ,                         & !in
                     ALBOLD ,TAUSS                          , & !inout
                     ALBGRD ,ALBGRI ,ALBD   ,ALBI   ,FABD   , & !out
                     FABI   ,FTDD   ,FTID   ,FTII   ,FSUN   , & !out
                     FREVI  ,FREVD  ,FREGD  ,FREGI  ,BGAP   , & !out
                     WGAP)

! --------------------------------------------------------------------------------------------------
! surface albedos. also fluxes (per unit incoming direct and diffuse
! radiation) reflected, transmitted, and absorbed by vegetation.
! also sunlit fraction of the canopy.
! --------------------------------------------------------------------------------------------------
  USE NOAHMP_VEG_PARAMETERS
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  INTEGER,                  INTENT(IN)  :: ILOC
  INTEGER,                  INTENT(IN)  :: JLOC
  INTEGER,                  INTENT(IN)  :: NSOIL  !number of soil layers
  INTEGER,                  INTENT(IN)  :: VEGTYP !vegetation type
  INTEGER,                  INTENT(IN)  :: IST    !surface type
  INTEGER,                  INTENT(IN)  :: ISC    !soil color type (1-lighest; 8-darkest)
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
  REAL, DIMENSION(1:2) :: ALBSND   !snow albedo (direct)
  REAL, DIMENSION(1:2) :: ALBSNI   !snow albedo (diffuse)

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
    RHO(IB) = MAX(RHOL(VEGTYP,IB)*WL+RHOS(VEGTYP,IB)*WS, MPE)
    TAU(IB) = MAX(TAUL(VEGTYP,IB)*WL+TAUS(VEGTYP,IB)*WS, MPE)
  END DO

! snow age

   CALL SNOW_AGE (DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)

! snow albedos: only if COSZ > 0 and FSNO > 0

  IF(OPT_ALB == 1) &
     CALL SNOWALB_BATS (NBAND, FSNO,COSZ,FAGE,ALBSND,ALBSNI)
  IF(OPT_ALB == 2) THEN
     CALL SNOWALB_CLASS (NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI,ILOC,JLOC)
     ALBOLD = ALB
  END IF

! ground surface albedo

  CALL GROUNDALB (NSOIL   ,NBAND   ,ICE     ,IST     ,ISC     , & !in
                  FSNO    ,SMC     ,ALBSND  ,ALBSNI  ,COSZ    , & !in
                  TG      ,ILOC    ,JLOC    ,                   & !in
                  ALBGRD  ,ALBGRI  )                              !out

! loop over NBAND wavebands to calculate surface albedos and solar
! fluxes for unit incoming direct (IC=0) and diffuse flux (IC=1)

  DO IB = 1, NBAND
      IC = 0      ! direct
      CALL TWOSTREAM (IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !in
                      FWET   ,TV      ,ALBGRD  ,ALBGRI  ,RHO    , & !in
                      TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & !in
                      FABD   ,ALBD    ,FTDD    ,FTID    ,GDIR   , &!)   !out
                      FREVD  ,FREGD   ,BGAP    ,WGAP)

      IC = 1      ! diffuse
      CALL TWOSTREAM (IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !in
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
! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE SURRAD (MPE     ,FSUN    ,FSHA    ,ELAI    ,VAI     , & !in
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
! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE SNOW_AGE (DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------ code history ------------------------------------------------------------
! from BATS
! ------------------------ input/output variables --------------------------------------------------
!input
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
   ELSE IF (SNEQV.GT.800.) THEN
          TAUSS = 0.
   ELSE
          DELA0 = 1.E-6*DT
          ARG   = 5.E3*(1./TFRZ-1./TG)
          AGE1  = EXP(ARG)
          AGE2  = EXP(AMIN1(0.,10.*ARG))
          AGE3  = 0.3
          TAGE  = AGE1+AGE2+AGE3
          DELA  = DELA0*TAGE
          DELS  = AMAX1(0.0,SNEQV-SNEQVO) / SWEMX
          SGE   = (TAUSS+DELA)*(1.0-DELS)
          TAUSS = AMAX1(0.,SGE)
   ENDIF

   FAGE= TAUSS/(TAUSS+1.)

  END SUBROUTINE SNOW_AGE
! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE SNOWALB_BATS (NBAND,FSNO,COSZ,FAGE,ALBSND,ALBSNI)
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

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
  REAL, PARAMETER :: C1 = 0.2  !default in BATS 
  REAL, PARAMETER :: C2 = 0.5  !default in BATS
!  REAL, PARAMETER :: C1 = 0.2 * 2. ! double the default to match Sleepers River's
!  REAL, PARAMETER :: C2 = 0.5 * 2. ! snow surface albedo (double aging effects)
! ---------------------------------------------------------------------------------------------
! zero albedos for all points

        ALBSND(1: NBAND) = 0.
        ALBSNI(1: NBAND) = 0.

! when cosz > 0

        SL=2.0
        SL1=1./SL
        SL2=2.*SL
        CF1=((1.+SL1)/(1.+SL2*COSZ)-SL1)
        FZEN=AMAX1(CF1,0.)

        ALBSNI(1)=0.95*(1.-C1*FAGE)         
        ALBSNI(2)=0.65*(1.-C2*FAGE)        

        ALBSND(1)=ALBSNI(1)+0.4*FZEN*(1.-ALBSNI(1))    !  vis direct
        ALBSND(2)=ALBSNI(2)+0.4*FZEN*(1.-ALBSNI(2))    !  nir direct

  END SUBROUTINE SNOWALB_BATS
! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE SNOWALB_CLASS (NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI,ILOC,JLOC)
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

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
           ALB = ALB + MIN(QSNOW*DT,SWEMX) * (0.84-ALB)/(SWEMX)
         ENDIF

         ALBSNI(1)= ALB         ! vis diffuse
         ALBSNI(2)= ALB         ! nir diffuse
         ALBSND(1)= ALB         ! vis direct
         ALBSND(2)= ALB         ! nir direct

  END SUBROUTINE SNOWALB_CLASS
! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE GROUNDALB (NSOIL   ,NBAND   ,ICE     ,IST     ,ISC     , & !in
                        FSNO    ,SMC     ,ALBSND  ,ALBSNI  ,COSZ    , & !in
                        TG      ,ILOC    ,JLOC    ,                   & !in
                        ALBGRD  ,ALBGRI  )                              !out
! --------------------------------------------------------------------------------------------------
  USE NOAHMP_RAD_PARAMETERS
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
!input

  INTEGER,                  INTENT(IN)  :: ILOC   !grid index
  INTEGER,                  INTENT(IN)  :: JLOC   !grid index
  INTEGER,                  INTENT(IN)  :: NSOIL  !number of soil layers
  INTEGER,                  INTENT(IN)  :: NBAND  !number of solar radiation waveband classes
  INTEGER,                  INTENT(IN)  :: ICE    !value of ist for land ice
  INTEGER,                  INTENT(IN)  :: IST    !surface type
  INTEGER,                  INTENT(IN)  :: ISC    !soil color class (1-lighest; 8-darkest)
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
           ALBSOD = MIN(ALBSAT(ISC,IB)+INC,ALBDRY(ISC,IB))
           ALBSOI = ALBSOD
        ELSE IF (TG .GT. TFRZ) THEN               !unfrozen lake, wetland
           ALBSOD = 0.06/(MAX(0.01,COSZ)**1.7 + 0.15)
           ALBSOI = 0.06
        ELSE                                      !frozen lake, wetland
           ALBSOD = ALBLAK(IB)
           ALBSOI = ALBSOD
        END IF

! increase desert and semi-desert albedos

        IF (IST .EQ. 1 .AND. ISC .EQ. 9) THEN
           ALBSOD = ALBSOD + 0.10
           ALBSOI = ALBSOI + 0.10
        end if

        ALBGRD(IB) = ALBSOD*(1.-FSNO) + ALBSND(IB)*FSNO
        ALBGRI(IB) = ALBSOI*(1.-FSNO) + ALBSNI(IB)*FSNO
  END DO

  END SUBROUTINE GROUNDALB
! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE TWOSTREAM (IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !in
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
  USE NOAHMP_VEG_PARAMETERS
  USE NOAHMP_RAD_PARAMETERS
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

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
	   DENFVEG = -LOG(MAX(1.0-FVEG,0.01))/(PAI*RC(VEGTYP)**2)
           HD      = HVT(VEGTYP) - HVB(VEGTYP)
           BB      = 0.5 * HD           
           THETAP  = ATAN(BB/RC(VEGTYP) * TAN(ACOS(MAX(0.01,COSZ))) )
           ! BGAP    = EXP(-DEN(VEGTYP) * PAI * RC(VEGTYP)**2/COS(THETAP) )
           BGAP    = EXP(-DENFVEG * PAI * RC(VEGTYP)**2/COS(THETAP) )
           FA      = VAI/(1.33 * PAI * RC(VEGTYP)**3.0 *(BB/RC(VEGTYP))*DENFVEG)
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
     CHIL   = MIN( MAX(XL(VEGTYP), -0.4), 0.6)
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
        TMP0 =   (1.-FWET)*OMEGAL        + FWET*OMEGAS(IB)
        TMP1 = ( (1.-FWET)*OMEGAL*BETADL + FWET*OMEGAS(IB)*BETADS ) / TMP0
        TMP2 = ( (1.-FWET)*OMEGAL*BETAIL + FWET*OMEGAS(IB)*BETAIS ) / TMP0
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
! ==================================================================================================
  SUBROUTINE VEGE_FLUX(NSNOW   ,NSOIL   ,ISNOW   ,VEGTYP  ,VEG     , & !in
                       DT      ,SAV     ,SAG     ,LWDN    ,UR      , & !in
                       UU      ,VV      ,SFCTMP  ,THAIR   ,QAIR    , & !in
                       EAIR    ,RHOAIR  ,SNOWH   ,VAI     ,GAMMA   , & !in
                       FWET    ,LAISUN  ,LAISHA  ,CWP     ,DZSNSO  , & !in
                       HTOP    ,ZLVL    ,ZPD     ,Z0M     ,FVEG    , & !in
                       Z0MG    ,EMV     ,EMG     ,CANLIQ  ,          & !in
                       CANICE  ,STC     ,DF      ,RSSUN   ,RSSHA   , & !in
                       RSURF   ,LATHEA  ,PARSUN  ,PARSHA  ,IGS     , & !in
                       FOLN    ,CO2AIR  ,O2AIR   ,BTRAN   ,SFCPRS  , & !in
                       RHSUR   ,ILOC    ,JLOC    ,Q2      , & !in
                       EAH     ,TAH     ,TV      ,TG      ,CM      , & !inout
                       CH      ,DX      ,DZ8W    ,                   & !
                       TAUXV   ,TAUYV   ,IRG     ,IRC     ,SHG     , & !out
                       SHC     ,EVG     ,EVC     ,TR      ,GH      , & !out
                       T2MV    ,PSNSUN  ,PSNSHA  ,                   & !out
                       QC      ,PBLH    ,QSFC    ,PSFC    ,ISURBAN , & !in
                       IZ0TLND ,Q2V     ,CAH2,CHLEAF,CHUC)              !inout 

! --------------------------------------------------------------------------------------------------
! use newton-raphson iteration to solve for vegetation (tv) and
! ground (tg) temperatures that balance the surface energy budgets

! vegetated:
! -SAV + IRC[TV] + SHC[TV] + EVC[TV] + TR[TV] = 0
! -SAG + IRG[TG] + SHG[TG] + EVG[TG] + GH[TG] = 0
! --------------------------------------------------------------------------------------------------
  USE NOAHMP_VEG_PARAMETERS
  USE MODULE_MODEL_CONSTANTS
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  INTEGER,                         INTENT(IN) :: ILOC   !grid index
  INTEGER,                         INTENT(IN) :: JLOC   !grid index
  LOGICAL,                         INTENT(IN) :: VEG    !true if vegetated surface
  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers        
  INTEGER,                         INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER,                         INTENT(IN) :: ISNOW  !actual no. of snow layers
  INTEGER,                         INTENT(IN) :: VEGTYP !vegetation physiology type
  REAL,                            INTENT(IN) :: FVEG   !greeness vegetation fraction (-)
  REAL,                            INTENT(IN) :: SAV    !solar rad absorbed by veg (w/m2)
  REAL,                            INTENT(IN) :: SAG    !solar rad absorbed by ground (w/m2)
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

  REAL,                            INTENT(IN) :: SNOWH  !actual snow depth [m]
  REAL,                            INTENT(IN) :: FWET   !wetted fraction of canopy
  REAL,                            INTENT(IN) :: HTOP   !top of canopy layer (m)
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
  REAL,                            INTENT(IN) :: GAMMA  !psychrometric constant (pa/K)
  REAL,                            INTENT(IN) :: LATHEA !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: PARSUN !par absorbed per unit sunlit lai (w/m2)
  REAL,                            INTENT(IN) :: PARSHA !par absorbed per unit shaded lai (w/m2)
  REAL,                            INTENT(IN) :: FOLN   !foliage nitrogen (%)
  REAL,                            INTENT(IN) :: CO2AIR !atmospheric co2 concentration (pa)
  REAL,                            INTENT(IN) :: O2AIR  !atmospheric o2 concentration (pa)
  REAL,                            INTENT(IN) :: IGS    !growing season index (0=off, 1=on)
  REAL,                            INTENT(IN) :: SFCPRS !pressure (pa)
  REAL,                            INTENT(IN) :: BTRAN  !soil water transpiration factor (0 to 1)
  REAL,                            INTENT(IN) :: RHSUR  !raltive humidity in surface soil/snow air space (-)

  INTEGER                        , INTENT(IN) :: ISURBAN
  INTEGER                        , INTENT(IN) :: IZ0TLND
  REAL                           , INTENT(IN) :: QC     !cloud water mixing ratio
  REAL                           , INTENT(IN) :: PBLH   !planetary boundary layer height
  REAL                           , INTENT(IN) :: PSFC   !pressure at lowest model layer
  REAL                           , INTENT(IN) :: DX     !grid spacing
  REAL                           , INTENT(IN) :: Q2     !mixing ratio (kg/kg)
  REAL                           , INTENT(IN) :: DZ8W   !thickness of lowest layer
  REAL                           , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer

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
        MOZSGN = 0
        MOZOLD = 0.
        HG     = 0.
        H      = 0.
        QFX    = 0.

! convert grid-cell LAI to the fractional vegetated area (FVEG)

        VAIE    = MIN(6.,VAI    / FVEG)
        LAISUNE = MIN(6.,LAISUN / FVEG)
        LAISHAE = MIN(6.,LAISHA / FVEG)

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

        HCAN = HTOP
        UC = UR*LOG(HCAN/Z0M)/LOG(ZLVL/Z0M)
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
          CALL SFCDIF1(ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
                       ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
                       MPE    ,ILOC   ,JLOC   ,                 & !in
                       MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
                       CM     ,CH     ,FV     ,CH2     )          !out
       ENDIF
     
       IF(OPT_SFC == 2) THEN
          CALL SFCDIF2(ITER   ,Z0M    ,TAH    ,THAIR  ,UR     , & !in
                       CZIL   ,ZLVL   ,ILOC   ,JLOC   ,         & !in
                       CM     ,CH     ,MOZ    ,WSTAR  ,         & !in
                       FV     )                                   !out
          ! Undo the multiplication by windspeed that SFCDIF2 
          ! applies to exchange coefficients CH and CM:
          CH = CH / UR
          CM = CM / UR
       ENDIF

       IF(OPT_SFC == 3) THEN
          CALL SFCDIF3(ILOC   ,JLOC   ,TAH    ,QSFC   ,PSFC   ,&  !in 
                       PBLH   ,Z0M    ,Z0MG   ,VEGTYP ,ISURBAN,&  !in
                       IZ0TLND,UC     ,ITER   ,NITERC ,SFCTMP ,&  !in
                       THAIR  ,QAIR   ,QC     ,ZLVL   , &  !in
                       SFCPRS ,FV     ,CM     ,CH     ,CH2V   ,&  !inout
                       CQ2V   ,MOZ)                               !out
          ! Undo the multiplication by windspeed that SFCDIF3 
          ! applies to exchange coefficients CH and CM:
          CH   = CH / UR
          CM   = CM / UR
          CH2V = CH2V / UR
       ENDIF

       IF(OPT_SFC == 4) THEN
          CALL SFCDIF4(ILOC   ,JLOC   ,UU     ,VV     ,SFCTMP ,&  !in
                       SFCPRS ,PSFC   ,PBLH   ,DX     ,Z0M    ,&
                       TAH    ,QAIR   ,ZLVL   ,IZ0TLND,QSFC   ,&
                       H      ,QFX    ,CM     ,CH     ,CH2V   ,& 
                       CQ2V   ,MOZ    ,FV     ,U10V   ,V10V)
          ! Undo the multiplication by windspeed that SFCDIF4 
          ! applies to exchange coefficients CH and CM:
          CH   = CH / UR
          CM   = CM / UR
          CH2V = CH2V / UR
       ENDIF

       RAMC = MAX(1.,1./(CM*UR))
       RAHC = MAX(1.,1./(CH*UR))
       RAWC = RAHC

       IF (OPT_SFC == 3 .OR. OPT_SFC == 4 ) THEN
         RAHC2 = MAX(1.,1./(CH2V*UR))
         RAWC2 = RAHC2
         CAH2 = 1./RAHC2
         CQ2V = CAH2
       ENDIF

! aerodyn resistance between heights z0g and d+z0v, RAG, and leaf
! boundary layer resistance, RB
       
       CALL RAGRB(ITER   ,VAIE   ,RHOAIR ,HG     ,TAH    , & !in
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
         CALL STOMATA (VEGTYP,MPE   ,PARSUN ,FOLN  ,ILOC  , JLOC , & !in       
                       TV    ,ESTV  ,EAH    ,SFCTMP,SFCPRS, & !in
                       O2AIR ,CO2AIR,IGS    ,BTRAN ,RB    , & !in
                       RSSUN ,PSNSUN)                         !out

         CALL STOMATA (VEGTYP,MPE   ,PARSHA ,FOLN  ,ILOC  , JLOC , & !in
                       TV    ,ESTV  ,EAH    ,SFCTMP,SFCPRS, & !in
                       O2AIR ,CO2AIR,IGS    ,BTRAN ,RB    , & !in
                       RSSHA ,PSNSHA)                         !out
        END IF

        IF (OPT_CRS == 2) then  ! Jarvis
         CALL  CANRES (PARSUN,TV    ,BTRAN ,EAH    ,SFCPRS, & !in
                       RSSUN ,PSNSUN,ILOC  ,JLOC   )          !out

         CALL  CANRES (PARSHA,TV    ,BTRAN ,EAH    ,SFCPRS, & !in
                       RSSHA ,PSNSHA,ILOC  ,JLOC   )          !out
        END IF
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
        CTW  = (1.-FWET)*(LAISUNE/(RB+RSSUN) + LAISHAE/(RB+RSSHA))
        CGW  = 1./(RAWG+RSURF)
        COND = CAW + CEW + CTW + CGW
        AEA  = (EAIR*CAW + ESTG*CGW) / COND
        BEA  = (CEW+CTW)/COND
        CEV  = (1.-BEA)*CEW*RHOAIR*CPAIR/GAMMA
        CTR  = (1.-BEA)*CTW*RHOAIR*CPAIR/GAMMA

! evaluate surface fluxes with current temperature and solve for dts

        TAH = ATA + BTA*TV               ! canopy air T.
        EAH = AEA + BEA*ESTV             ! canopy air e

        IRC = FVEG*(AIR + CIR*TV**4)
        SHC = FVEG*RHOAIR*CPAIR*CVH * (  TV-TAH)
        EVC = FVEG*RHOAIR*CPAIR*CEW * (ESTV-EAH) / GAMMA
        TR  = FVEG*RHOAIR*CPAIR*CTW * (ESTV-EAH) / GAMMA
        EVC = MIN(CANLIQ*LATHEA/DT,EVC)

        B   = SAV-IRC-SHC-EVC-TR                          !additional w/m2
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

! added moisture flux for sfcdif4
        IF ( OPT_SFC == 4 ) THEN
           QFX = (QSFC-QAIR)*RHOAIR*CAW !*CPAIR/GAMMA
        ENDIF

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
        CEV = RHOAIR*CPAIR / (GAMMA*(RAWG+RSURF))
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

        B = SAG-IRG-SHG-EVG-GH
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

     IF(OPT_STC == 1) THEN
     IF (SNOWH > 0.05 .AND. TG > TFRZ) THEN
        TG  = TFRZ
        IRG = CIR*TG**4 - EMG*(1.-EMV)*LWDN - EMG*EMV*SB*TV**4
        SHG = CSH * (TG         - TAH)
        EVG = CEV * (ESTG*RHSUR - EAH)
        GH  = SAG - (IRG+SHG+EVG)
     END IF
     END IF

! wind stresses

     TAUXV = -RHOAIR*CM*UR*UU
     TAUYV = -RHOAIR*CM*UR*VV

! consistent vegetation air temperature and vapor pressure since TG is not consistent with the TAH/EAH
! calculation.
!     TAH = SFCTMP + (SHG+SHC)/(RHOAIR*CPAIR*CAH) 
!     TAH = SFCTMP + (SHG*FVEG+SHC)/(RHOAIR*CPAIR*CAH) ! ground flux need fveg
!     EAH = EAIR + (EVC+FVEG*(TR+EVG))/(RHOAIR*CAW*CPAIR/GAMMA )
!     QFX = (QSFC-QAIR)*RHOAIR*CAW !*CPAIR/GAMMA

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
         Q2V = QSFC - ((EVC+TR)/FVEG+EVG)/(LATHEA*RHOAIR) * 1./CQ2V
      ENDIF
   ENDIF

! myj/ysu consistent 2m temperature over vegetation (if CQ2V .lt. 1e-5? )
   IF (OPT_SFC == 3 .OR. OPT_SFC == 4 ) THEN
         IF (CAH2 .LT. 1.E-5 ) THEN
            T2MV = TAH
            Q2V  = (EAH*0.622/(SFCPRS - 0.378*EAH))
         ELSE
            T2MV = TAH - (SHG+SHC)/(RHOAIR*CPAIR*CAH2)
            Q2V  = (EAH*0.622/(SFCPRS - 0.378*EAH)) - QFX/(RHOAIR*CQ2V)
         ENDIF
   ENDIF  

! update CH for output
     CH = CAH
     CHLEAF = CVH
     CHUC = 1./RAHG

  END SUBROUTINE VEGE_FLUX
! ==================================================================================================
  SUBROUTINE BARE_FLUX (NSNOW   ,NSOIL   ,ISNOW   ,DT      ,SAG     , & !in
                        LWDN    ,UR      ,UU      ,VV      ,SFCTMP  , & !in
                        THAIR   ,QAIR    ,EAIR    ,RHOAIR  ,SNOWH   , & !in
                        DZSNSO  ,ZLVL    ,ZPD     ,Z0M     ,          & !in
                        EMG     ,STC     ,DF      ,RSURF   ,LATHEA  , & !in
                        GAMMA   ,RHSUR   ,ILOC    ,JLOC    ,Q2      , & !in
                        TGB     ,CM      ,CH      ,          & !inout
                        TAUXB   ,TAUYB   ,IRB     ,SHB     ,EVB     , & !out
                        GHB     ,T2MB    ,DX      ,DZ8W    ,IVGTYP  , & !out
                        QC      ,PBLH    ,QSFC    ,PSFC    ,ISURBAN , & !in
                        IZ0TLND ,SFCPRS  ,Q2B     ,EHB2)      !in 

! --------------------------------------------------------------------------------------------------
! use newton-raphson iteration to solve ground (tg) temperature
! that balances the surface energy budgets for bare soil fraction.

! bare soil:
! -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0
! ----------------------------------------------------------------------
  USE NOAHMP_VEG_PARAMETERS
  USE MODULE_MODEL_CONSTANTS
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
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

!jref:start; in 
  INTEGER                        , INTENT(IN) :: ISURBAN
  INTEGER                        , INTENT(IN) :: IVGTYP
  INTEGER                        , INTENT(IN) :: IZ0TLND
  REAL                           , INTENT(IN) :: QC     !cloud water mixing ratio
  REAL                           , INTENT(IN) :: PBLH   !planetary boundary layer height
  REAL                           , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer
  REAL                           , INTENT(IN) :: PSFC   !pressure at lowest model layer
  REAL                           , INTENT(IN) :: SFCPRS !pressure at lowest model layer
  REAL                           , INTENT(IN) :: DX     !horisontal grid spacing
  REAL                           , INTENT(IN) :: Q2     !mixing ratio (kg/kg)
  REAL                           , INTENT(IN) :: DZ8W   !thickness of lowest layer
!jref:end

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
        MOZSGN = 0
        MOZOLD = 0.
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
          CALL SFCDIF1(ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
                       ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
                       MPE    ,ILOC   ,JLOC   ,                 & !in
                       MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
                       CM     ,CH     ,FV     ,CH2     )          !out
        ENDIF

        IF(OPT_SFC == 2) THEN
          CALL SFCDIF2(ITER   ,Z0M    ,TGB    ,THAIR  ,UR     , & !in
                       CZIL   ,ZLVL   ,ILOC   ,JLOC   ,         & !in
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

        IF(OPT_SFC == 3) THEN
          VEGTYP = ISBARREN
          CALL SFCDIF3(ILOC   ,JLOC   ,TGB    ,QSFC   ,PSFC   ,&  !in    
                       PBLH   ,Z0M    ,Z0M    ,VEGTYP ,ISURBAN,&  !in    
                       IZ0TLND,UR     ,ITER   ,NITERB ,SFCTMP ,&  !in    
                       THAIR  ,QAIR   ,QC     ,ZLVL   ,        &  !in            
                       SFCPRS ,FV     ,CM     ,CH     ,CH2B   ,&  !inout 
                       CQ2B   ,MOZ)                               !out   
          ! Undo the multiplication by windspeed that SFCDIF3 
          ! applies to exchange coefficients CH and CM:
          CH   = CH / UR
          CM   = CM / UR
          CH2B = CH2B / UR

          IF(SNOWH > 0.) THEN    ! jref: does this still count??
             CM = MIN(0.01,CM)   ! CM & CH are too large, causing
             CH = MIN(0.01,CH)   ! computational instability
             CH2B = MIN(0.01,CH2B)
             CQ2B = MIN(0.01,CQ2B)
          END IF
        ENDIF

       IF(OPT_SFC == 4) THEN
          CALL SFCDIF4(ILOC   ,JLOC   ,UU     ,VV     ,SFCTMP ,&  !in
                       SFCPRS ,PSFC   ,PBLH   ,DX     ,Z0M    ,&
                       TGB    ,QAIR   ,ZLVL   ,IZ0TLND,QSFC   ,&
                       H      ,QFX    ,CM     ,CH     ,CH2B   ,& 
                       CQ2B   ,MOZ    ,FV     ,U10B   ,V10B)
          ! Undo the multiplication by windspeed that SFCDIF4 
          ! applies to exchange coefficients CH and CM:
          CH   = CH / UR
          CM   = CM / UR
          CH2B = CH2B / UR

          IF(SNOWH > 0.) THEN    ! jref: does this still count??
             CM = MIN(0.01,CM)   ! CM & CH are too large, causing
             CH = MIN(0.01,CH)   ! computational instability
             CH2B = MIN(0.01,CH2B)
             CQ2B = MIN(0.01,CQ2B)
          END IF

       ENDIF

        RAMB = MAX(1.,1./(CM*UR))
        RAHB = MAX(1.,1./(CH*UR))
        RAWB = RAHB

!jref - variables for diagnostics         
        EMB = 1./RAMB
        EHB = 1./RAHB
        IF (OPT_SFC == 3 .OR. OPT_SFC == 4) THEN
          RAHB2 = MAX(1.,1./(CH2B*UR))
          EHB2 = 1./RAHB2
          CQ2B = EHB2
        END IF

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

        B     = SAG-IRB-SHB-EVB-GHB
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

     IF(OPT_STC == 1) THEN
     IF (SNOWH > 0.05 .AND. TGB > TFRZ) THEN
          TGB = TFRZ
          IRB = CIR * TGB**4 - EMG*LWDN
          SHB = CSH * (TGB        - SFCTMP)
          EVB = CEV * (ESTG*RHSUR - EAIR )          !ESTG reevaluate ?
          GHB = SAG - (IRB+SHB+EVB)
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
       IF (IVGTYP == ISURBAN) Q2B = QSFC
     END IF

! myj consistent 2m temperature over bare soil
     IF(OPT_SFC ==3 .OR. OPT_SFC == 4) THEN
       IF (EHB2.lt.1.E-5 ) THEN
          T2MB = TGB
          Q2B  = QSFC
       ELSE
          T2MB = TGB - SHB/(RHOAIR*CPAIR*EHB2)
          Q2B  = QSFC - QFX/(RHOAIR*CQ2B)
       END IF
!       IF (IVGTYP == ISURBAN) THEN
!          Q2B = QSFC
!       END IF
     END IF

! update CH 
     CH = EHB

  END SUBROUTINE BARE_FLUX
! ==================================================================================================
  SUBROUTINE RAGRB(ITER   ,VAI    ,RHOAIR ,HG     ,TAH    , & !in
                   ZPD    ,Z0MG   ,Z0HG   ,HCAN   ,UC     , & !in
                   Z0H    ,FV     ,CWP    ,VEGTYP ,MPE    , & !in
                   TV     ,MOZG   ,FHG    ,ILOC   ,JLOC   , & !inout
                   RAMG   ,RAHG   ,RAWG   ,RB     )           !out
! --------------------------------------------------------------------------------------------------
! compute under-canopy aerodynamic resistance RAG and leaf boundary layer
! resistance RB
! --------------------------------------------------------------------------------------------------
  USE NOAHMP_VEG_PARAMETERS
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs

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
       RB     = TMPRB * SQRT(DLEAF(VEGTYP)/UC)
!       RB = 200

  END SUBROUTINE RAGRB

! ==================================================================================================

  SUBROUTINE SFCDIF1(ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
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
       write(*,*) 'critical problem: ZLVL <= ZPD; model stops'
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

! ==================================================================================================

  SUBROUTINE SFCDIF2(ITER   ,Z0     ,THZ0   ,THLM   ,SFCSPD , & !in
                     CZIL   ,ZLM    ,ILOC   ,JLOC   ,         & !in
                     AKMS   ,AKHS   ,RLMO   ,WSTAR2 ,         & !in
                     USTAR  )                                   !out

! -------------------------------------------------------------------------------------------------
! SUBROUTINE SFCDIF (renamed SFCDIF_off to avoid clash with Eta PBL)
! -------------------------------------------------------------------------------------------------
! CALCULATE SURFACE LAYER EXCHANGE COEFFICIENTS VIA ITERATIVE PROCESS.
! SEE CHEN ET AL (1997, BLM)
! -------------------------------------------------------------------------------------------------

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ILOC
    INTEGER, INTENT(IN) :: JLOC
    INTEGER, INTENT(IN) :: ITER
    REAL,    INTENT(IN) :: ZLM, Z0, THZ0, THLM, SFCSPD, CZIL
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
    ZILFC = - CZIL * VKRM * SQVISC
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
       AKMS = MAX (USTARK / SIMM,CXCH)
!-----------------------------------------------------------------------
! IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO
!-----------------------------------------------------------------------
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
!jref:start 
! ==================================================================================================
  SUBROUTINE SFCDIF3(ILOC   ,JLOC   ,TSK    ,QS     ,PSFC   ,&  !in  
                     PBLH   ,Z0     ,Z0BASE ,VEGTYP ,ISURBAN,&  !in  
                     IZ0TLND,SFCSPD ,ITER   ,ITRMX  ,TLOW   ,&  !in  
                     THLOW  ,QLOW   ,CWMLOW ,ZSL    ,        &  !in
                     PLOW   ,USTAR  ,AKMS   ,AKHS   ,CHS2   ,&  !inout
                     CQS2   ,RLMO   )                           !out 

  USE MODULE_SF_MYJSFC, ONLY :   &
     &                         EPSU2  , &  
     &                         EPSUST , &
     &                         EPSZT  , &  
     &                         BETA   , &  
     &                         EXCML  , &  
     &                         RIC    , &  
     &                         SQVISC , &
     &                         ZTFC   , &  
     &                         BTG    , &  
     &                         CZIV   , &  
     &                         PI     , &  
     &                         PIHF   , &  
     &                         KZTM   , &  
     &                         KZTM2  , &  
     &                         DZETA1 , &
     &                         DZETA2 , &
     &                         FH01   , &  
     &                         FH02   , &  
     &                         WWST2  , &  
     &                         WWST   , &  
     &                         ZTMAX1 , &
     &                         ZTMAX2 , &
     &                         ZTMIN1 , &
     &                         ZTMIN2 , &
     &                         PSIH1  , &  
     &                         PSIH2  , &  
     &                         PSIM1  , &  
     &                         PSIM2   

  USE MODULE_MODEL_CONSTANTS

!----------------------------------------------------------------------   
!  computing surface drag coefficient CM for momentum and CH for heat
!  Joakim Refslund, 2011, MYJ SFCLAY
!----------------------------------------------------------------------   
   IMPLICIT NONE     
!----------------------------------------------------------------------   
! input
    INTEGER,INTENT(IN) :: ILOC
    INTEGER,INTENT(IN) :: JLOC
    REAL   ,INTENT(IN) :: TSK
    REAL   ,INTENT(IN) :: PSFC
    REAL   ,INTENT(IN) :: PBLH
    INTEGER,INTENT(IN) :: VEGTYP  !in routine
    INTEGER,INTENT(IN) :: ISURBAN !in veg_parm                    
    INTEGER,INTENT(IN) :: IZ0TLND
    REAL   ,INTENT(IN) :: QLOW
    REAL   ,INTENT(IN) :: THLOW
    REAL   ,INTENT(IN) :: TLOW
    REAL   ,INTENT(IN) :: CWMLOW
    REAL   ,INTENT(IN) :: SFCSPD
    REAL   ,INTENT(IN) :: PLOW
    REAL   ,INTENT(IN) :: ZSL
    REAL   ,INTENT(IN) :: Z0BASE
    INTEGER,INTENT(IN) :: ITER
    INTEGER,INTENT(IN) :: ITRMX

! output                                                                        
    REAL   ,INTENT(OUT) :: CHS2
    REAL   ,INTENT(OUT) :: CQS2
    REAL   ,INTENT(OUT) :: RLMO

! input/output
    REAL   ,INTENT(INOUT) :: AKHS
    REAL   ,INTENT(INOUT) :: AKMS
    REAL   :: QZ0
    REAL   ,INTENT(INOUT) :: USTAR
    REAL   ,INTENT(IN) :: Z0
    REAL   ,INTENT(INOUT):: QS
    REAL                :: RIB

! local
    INTEGER :: ITR,K
    REAL :: THZ0
    REAL :: THVLOW
    REAL :: CT
    REAL :: BTGH
    REAL :: BTGX
    REAL :: CXCHL
    REAL :: DTHV
    REAL :: DU2
    REAL :: ELFC
    REAL :: PSH02
    REAL :: PSH10
    REAL :: PSHZ
    REAL :: PSHZL
    REAL :: PSM10
    REAL :: PSMZ
    REAL :: PSMZL
    REAL :: RDZ
    REAL :: RDZT
    REAL :: RLMA !???
    REAL :: RLMN !???
    REAL :: RLOGT
    REAL :: RLOGU
    REAL :: RZ
    REAL :: SIMH
    REAL :: SIMM
    REAL :: USTARK
    REAL :: WSTAR2
    REAL :: WSTAR
    REAL :: CHS
    REAL :: RZSU
    REAL :: RZST
    REAL :: X,XLT,XLT4,XLU,XLU4,XT,XT4,XU,XU4,ZETALT,ZETALU         , &
            ZETAT,ZETAU,ZQ,ZSLT,ZSLU,ZT,ZU,TOPOTERM,ZZIL
    REAL :: AKHS02,AKHS10,AKMS02,AKMS10
    REAL :: ZU10
    REAL :: ZT02
    REAL :: ZT10
    REAL :: RLNU10
    REAL :: RLNT02
    REAL :: RLNT10
    REAL :: ZTAU10
    REAL :: ZTAT02
    REAL :: ZTAT10
    REAL :: SIMM10
    REAL :: SIMH02
    REAL :: SIMH10
    REAL :: ZUUZ
    REAL :: EKMS10
    REAL :: test
    REAL :: E1

   REAL,    PARAMETER :: VKRM    = 0.40
   REAL,    PARAMETER :: CZETMAX = 10.

! diagnostic terms                                                         

   REAL :: CZIL
   REAL :: ZILFC

! KTMZ,KTMZ2,DZETA1,DZETA2,FH01,FH02,ZTMAX1,ZTMAX2,ZTMIN1,ZTMIN2,
! PSIH1,PSIH2,PSIM1,PSIM2 ARE DEFINED IN MODULE_SF_MYJSFC

!----------------------------------------------------------------------   
!    IF (ILOC.eq.39 .and. JLOC.eq.63 .and. ITER == 1 ) then
!      write(*,*) "THZ0=",THZ0
!      write(*,*) "QS =",QS
!      write(*,*) "PSFC=",PSFC
!      write(*,*) "PBLH=",PBLH
!      write(*,*) "Z0=",Z0
!      write(*,*) "Z0BASE=",Z0BASE
!      write(*,*) "VEGTYP=",VEGTYP
!      write(*,*) "ISURBAN=",ISURBAN
!      write(*,*) "IZ0TLND=",IZ0TLND
!      write(*,*) "SFCSPD=",SFCSPD
!      write(*,*) "TLOW=",TLOW
!      write(*,*) "THLOW=",THLOW
!      write(*,*) "THVLOW=",THVLOW
!      write(*,*) "QLOW=",QLOW
!      write(*,*) "CWMLOW=",CWMLOW
!      write(*,*) "ZSL=",ZSL
!      write(*,*) "PLOW=",PLOW
!      write(*,*) "USTAR=",USTAR
!      write(*,*) "AKMS=",AKMS
!      write(*,*) "AKHS=",AKHS
!      write(*,*) "CHS2=",CHS2
!      write(*,*) "CQS2=",CQS2
!      write(*,*) "RLMO=",RLMO
!      write(*,*) "ITER=",ITER
!      call wrf_error_fatal("STOP in SFCDIF3")
!    ENDIF

! calculate potential and virtual potential temperatures
    THVLOW = THLOW*(1.+EP_1*QLOW)
    THZ0   = TSK*(P1000mb/PSFC)**RCP

! calculate initial values
    ZU = Z0
    ZT = ZU*ZTFC      !ZTFC = ZOH/ZOM =<1 set to 1 at beginning                                                 
    ZQ = ZT
    QZ0 = QS

    RDZ = 1./ZSL
    CXCHL = EXCML*RDZ
    DTHV = THVLOW-THZ0*(0.608*QZ0+1.)    !delta pot. virtual temperature 

    BTGX=GRAV/THLOW
    ELFC=VKRM*BTGX

! Minimum PBLH is >= 1000.
    IF(PBLH > 1000.)THEN  
       BTGH = BTGX*PBLH
    ELSE
       BTGH = BTGX*1000.
    ENDIF

    DU2 = MAX(SFCSPD*SFCSPD,EPSU2)  !Wind speed - EPSU2 parm = 1*10^-6                    
    RIB = BTGX*DTHV*ZSL/DU2         !Bulk richardson stability                               

    ZSLU = ZSL+ZU
    RZSU = ZSLU/ZU
    RLOGU = LOG(RZSU)       !log(z/z0) 

    ZSLT = ZSL + ZU

    IF ( (IZ0TLND==0) .or. (VEGTYP == ISURBAN) ) THEN    ! ARE IZ0TLND DEFINED HERE?           
        ! Just use the original CZIL value.                          
        CZIL = 0.1
    ELSE
        ! Modify CZIL according to Chen & Zhang, 2009                
        ! CZIL = 10 ** -0.40 H, ( where H = 10*Zo )                  
        CZIL = 10.0 ** ( -0.40 * ( Z0 / 0.07 ) )
    ENDIF
    ZILFC=-CZIL*VKRM*SQVISC     !SQVISC parm                                 

! stable                                                                  
    IF(DTHV>0.)THEN
       IF (RIB<RIC) THEN
          ZZIL=ZILFC*(1.0+(RIB/RIC)*(RIB/RIC)*CZETMAX)
       ELSE
          ZZIL=ZILFC*(1.0+CZETMAX)
       ENDIF
! unstable                                                                
    ELSE
       ZZIL=ZILFC
    ENDIF

!---  ZILITINKEVITCH FIX FOR ZT                                           
! oldform   ZT=MAX(EXP(ZZIL*SQRT(USTAR*ZU))*ZU,EPSZT)                     
    ZT=MAX(EXP(ZZIL*SQRT(USTAR*Z0BASE))*Z0BASE,EPSZT)  !Z0 is backgrund roughness?           
    RZST=ZSLT/ZT
    RLOGT=LOG(RZST)

!----------------------------------------------------------------------   
!  1./MONIN-OBUKHOV LENGTH-SCALE                                       
!----------------------------------------------------------------------   
    RLMO=ELFC*AKHS*DTHV/USTAR**3

    ZETALU=ZSLU*RLMO
    ZETALT=ZSLT*RLMO
    ZETAU=ZU*RLMO
    ZETAT=ZT*RLMO

    ZETALU=MIN(MAX(ZETALU,ZTMIN2),ZTMAX2)
    ZETALT=MIN(MAX(ZETALT,ZTMIN2),ZTMAX2)
    ZETAU=MIN(MAX(ZETAU,ZTMIN2/RZSU),ZTMAX2/RZSU)
    ZETAT=MIN(MAX(ZETAT,ZTMIN2/RZST),ZTMAX2/RZST)

!----------------------------------------------------------------------   
!***  LAND FUNCTIONS                                                      
!----------------------------------------------------------------------   

    RZ=(ZETAU-ZTMIN2)/DZETA2
    K=INT(RZ)
    RDZT=RZ-REAL(K)
    K=MIN(K,KZTM2)
    K=MAX(K,0)
    PSMZ=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)

    RZ=(ZETALU-ZTMIN2)/DZETA2
    K=INT(RZ)
    RDZT=RZ-REAL(K)
    K=MIN(K,KZTM2)
    K=MAX(K,0)
    PSMZL=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)

    SIMM=PSMZL-PSMZ+RLOGU

    RZ=(ZETAT-ZTMIN2)/DZETA2
    K=INT(RZ)
    RDZT=RZ-REAL(K)
    K=MIN(K,KZTM2)
    K=MAX(K,0)
    PSHZ=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)

    RZ=(ZETALT-ZTMIN2)/DZETA2
    K=INT(RZ)
    RDZT=RZ-REAL(K)
    K=MIN(K,KZTM2)
    K=MAX(K,0)
    PSHZL=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)

    SIMH=(PSHZL-PSHZ+RLOGT)*FH02
!----------------------------------------------------------------------   
    USTARK=USTAR*VKRM
    AKMS=MAX(USTARK/SIMM,CXCHL)
    AKHS=MAX(USTARK/SIMH,CXCHL)

!----------------------------------------------------------------------   
!  BELJAARS CORRECTION FOR USTAR                                       
!----------------------------------------------------------------------   

    IF(DTHV<=0.)THEN                                           !zj
      WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)                !zj
    ELSE                                                       !zj
      WSTAR2=0.                                                !zj
    ENDIF                                                      !zj

    USTAR=MAX(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)

    CT=0.
!----------------------------------------------------------------------   
!***  THE FOLLOWING DIAGNOSTIC BLOCK PRODUCES 2-m and 10-m VALUES         
!***  FOR TEMPERATURE, MOISTURE, AND WINDS.  IT IS DONE HERE SINCE        
!***  THE VARIOUS QUANTITIES NEEDED FOR THE COMPUTATION ARE LOST          
!***  UPON EXIT FROM THE ROTUINE.                                         
!----------------------------------------------------------------------   
    WSTAR=SQRT(WSTAR2)/WWST

!jref: calculate in last iteration
!  IF (ITER == ITRMX) THEN

    ZU10=ZU+10.
    ZT02=ZT+02.
    ZT10=ZT+10.

    RLNU10=LOG(ZU10/ZU)
    RLNT02=LOG(ZT02/ZT)
    RLNT10=LOG(ZT10/ZT)

    ZTAU10=ZU10*RLMO
    ZTAT02=ZT02*RLMO
    ZTAT10=ZT10*RLMO

    ZTAU10=MIN(MAX(ZTAU10,ZTMIN2),ZTMAX2)
    ZTAT02=MIN(MAX(ZTAT02,ZTMIN2),ZTMAX2)
    ZTAT10=MIN(MAX(ZTAT10,ZTMIN2),ZTMAX2)

!jref: land diagnostic functions
    RZ=(ZTAU10-ZTMIN2)/DZETA2
    K=INT(RZ)
    RDZT=RZ-REAL(K)
    K=MIN(K,KZTM2)
    K=MAX(K,0)
    PSM10=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)

    SIMM10=PSM10-PSMZ+RLNU10

    RZ=(ZTAT02-ZTMIN2)/DZETA2
    K=INT(RZ)
    RDZT=RZ-REAL(K)
    K=MIN(K,KZTM2)
    K=MAX(K,0)
    PSH02=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)

    SIMH02=(PSH02-PSHZ+RLNT02)*FH02

    RZ=(ZTAT10-ZTMIN2)/DZETA2
    K=INT(RZ)
    RDZT=RZ-REAL(K)
    K=MIN(K,KZTM2)
    K=MAX(K,0)
    PSH10=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)

    SIMH10=(PSH10-PSHZ+RLNT10)*FH02

!jref: diagnostic exchange coefficients                                                                    
    AKMS10=MAX(USTARK/SIMM10,CXCHL)
    AKHS02=MAX(USTARK/SIMH02,CXCHL)
    AKHS10=MAX(USTARK/SIMH10,CXCHL)

!jref: calculation of diagnostics for wind, humidity

!    WSTAR=SQRT(WSTAR2)/WWST
!
!    UMFLX=AKMS*(ULOW -UZ0 )
!    VMFLX=AKMS*(VLOW -VZ0 )
!    HSFLX=AKHS*(THLOW-THZ0)
!    HLFLX=AKHS*(QLOW -QZ0 )

!uncommented for now...
!    U10 =UMFLX/AKMS10+UZ0                                               
!    V10 =VMFLX/AKMS10+VZ0                                               
!    TH02=HSFLX/AKHS02+THZ0                                              
!                                                                         
!***  BE CERTAIN THAT THE 2-M THETA AND 10-M THETA ARE BRACKETED BY       
!***  THE VALUES OF THZ0 AND THLOW.                                       
!                                                                         
!      IF(THLOW>THZ0.AND.(TH02<THZ0.OR.TH02>THLOW).OR.                  &  
!         THLOW<THZ0.AND.(TH02>THZ0.OR.TH02<THLOW))THEN                    
!           TH02=THZ0+2.*RDZ*(THLOW-THZ0)                                  
!      ENDIF                                                               
!                                                                         
!uncommented for now
!      TH10=HSFLX/AKHS10+THZ0                                              
!                                                                         
!      IF(THLOW>THZ0.AND.(TH10<THZ0.OR.TH10>THLOW).OR.                  &  
!         THLOW<THZ0.AND.(TH10>THZ0.OR.TH10<THLOW))THEN                    
!           TH10=THZ0+10.*RDZ*(THLOW-THZ0)                                 
!      ENDIF                                                               
!                                                                         
!      Q02 =HLFLX/AKHS02+QZ0                                               
!      Q10 =HLFLX/AKHS10+QZ0                                               
!jref commented out     
!      TERM1=-0.068283/TLOW                                                
!      PSHLTR=PSFC*EXP(TERM1)                                              
!                                
!----------------------------------------------------------------------   
!***  COMPUTE "EQUIVALENT" Z0 TO APPROXIMATE LOCAL SHELTER READINGS.      
!----------------------------------------------------------------------   
!                                                                         
!      U10E=U10                                                            
!      V10E=V10                                                            
!                                                                         
!      IF(SEAMASK<0.5)THEN                                                 
!LAND :                                                                          
!1st        ZUUZ=MIN(0.5*ZU,0.1)                                          
!1st        ZU=MAX(0.1*ZU,ZUUZ)                                           
!tst        ZUUZ=amin1(ZU*0.50,0.3)                                       
!tst        ZU=amax1(ZU*0.3,ZUUZ)                                         

        ZUUZ=AMIN1(ZU*0.50,0.18)
        ZU=AMAX1(ZU*0.35,ZUUZ)
!                                                                         
        ZU10=ZU+10.
        RZSU=ZU10/ZU
        RLNU10=LOG(RZSU)

        ZETAU=ZU*RLMO
        ZTAU10=ZU10*RLMO

        ZTAU10=MIN(MAX(ZTAU10,ZTMIN2),ZTMAX2)
        ZETAU=MIN(MAX(ZETAU,ZTMIN2/RZSU),ZTMAX2/RZSU)

        RZ=(ZTAU10-ZTMIN2)/DZETA2
        K=INT(RZ)
        RDZT=RZ-REAL(K)
        K=MIN(K,KZTM2)
        K=MAX(K,0)
        PSM10=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)
        SIMM10=PSM10-PSMZ+RLNU10
        EKMS10=MAX(USTARK/SIMM10,CXCHL)

!        U10E=UMFLX/EKMS10+UZ0                                             
!        V10E=VMFLX/EKMS10+VZ0                                             

!      ENDIF                                                               
!                                                                         
!      U10=U10E                                                            
!      V10=V10E                                                            
!                                                                         
!----------------------------------------------------------------------   
!***  SET OTHER WRF DRIVER ARRAYS                                         
!----------------------------------------------------------------------   
!                                                                         
!jref commented out
!      RLOW=PLOW/(R_D*TLOW)                                                
      CHS=AKHS
      CHS2=AKHS02
      CQS2=AKHS02

!  END IF
  END SUBROUTINE SFCDIF3
!jref:end
! ==================================================================================================
!jref:start
!-------------------------------------------------------------------                                                      
   SUBROUTINE SFCDIF4(ILOC  ,JLOC  ,UX    ,VX     ,T1D  , &
                      P1D   ,PSFCPA,PBLH  ,DX     ,ZNT  , &
                      TSK   ,QX    ,ZLVL  ,IZ0TLND,QSFC , &
                      HFX   ,QFX   ,CM    ,CHS    ,CHS2 , &
                      CQS2  ,RMOL  ,UST   ,U10    ,V10)
                                                                                                                          
   USE MODULE_SF_SFCLAY                                                                                                   
   USE MODULE_MODEL_CONSTANTS                                                                                             
!-------------------------------------------------------------------                                                      
!  Compute surface drag coefficients CM for momentum and CH for heat                                                      
!  Joakim Refslund, 2011. Modified from YSU SFCLAY.                                                                                     
!-------------------------------------------------------------------                                                      
   IMPLICIT NONE                                                                                                          
!-------------------------------------------------------------------                                                      
! parameters                                                                                                              
   REAL,   PARAMETER     :: XKA=2.4E-5                                                                                    
   REAL,   PARAMETER     :: PRT=1.     !prandtl number                                                                    
                                                                                                                          
! input                                                                                                                   
   INTEGER,INTENT(IN )   :: ILOC                                                                                          
   INTEGER,INTENT(IN )   :: JLOC                                                                                          
                                                                                                                          
   REAL,   INTENT(IN )   :: PBLH      ! planetary boundary layer height                                                   
   REAL,   INTENT(IN )   :: TSK       ! skin temperature                                                                  
   REAL,   INTENT(IN )   :: PSFCPA    ! pressure in pascal                                                                
   REAL,   INTENT(IN )   :: P1D       !lowest model layer pressure (Pa)                                                      
   REAL,   INTENT(IN )   :: T1D       !lowest model layer temperature
!   REAL,   INTENT(IN )   :: QX        !water vapor mixing ratio (kg/kg)
   REAL,   INTENT(IN )   :: QX        !water vapor specific humidity (kg/kg)
   REAL,   INTENT(IN )   :: ZLVL      ! thickness of lowest full level layer
   REAL,   INTENT(IN )   :: HFX       ! sensible heat flux
   REAL,   INTENT(IN )   :: QFX       ! moisture flux
   REAL,   INTENT(IN )   :: DX        ! horisontal grid spacing
   REAL,   INTENT(IN )   :: UX
   REAL,   INTENT(IN )   :: VX
   REAL,   INTENT(IN )   :: ZNT                                                                                           
   REAL,   INTENT(INOUT ) :: QSFC

   REAL,   INTENT(INOUT) :: RMOL                                                                                          
   REAL,   INTENT(INOUT) :: UST                                                                                           
   REAL,   INTENT(INOUT) :: CHS2                                                                                          
   REAL,   INTENT(INOUT) :: CQS2                                                                                          
   REAL,   INTENT(INOUT) :: CHS                                                                                           
   REAL,   INTENT(INOUT) :: CM                
                                                                                                                          
! diagnostics out                                                                                                         
   REAL,   INTENT(OUT)   :: U10                                                                                           
   REAL,   INTENT(OUT)   :: V10                                                                                           
!   REAL,   INTENT(OUT)   :: TH2                                                                                           
!   REAL,   INTENT(OUT)   :: T2                                                                                            
!   REAL,   INTENT(OUT)   :: Q2                                                                                            
!   REAL,   INTENT(OUT)   :: QSFC                                                                                          
                                                                                                                          
! optional vars                                                                                                           
   INTEGER,OPTIONAL,INTENT(IN ) :: IZ0TLND                                                                                
                                                                                                                          
! local                                                                                                                   
   INTEGER :: REGIME  ! Stability regime
   REAL    :: ZA      ! Height of full-sigma level                                                                        
   REAL    :: THVX    ! Virtual potential temperature                                                                     
   REAL    :: ZQKL    ! Height of upper half level                                                                        
   REAL    :: ZQKLP1  ! Height of lower half level (surface)                                                              
   REAL    :: THX     ! Potential temperature                                                                             
   REAL    :: PSIH    ! similarity function for heat                                                                      
   REAL    :: PSIH2   ! Similarity function for heat 2m                                                                   
   REAL    :: PSIH10  ! Similarity function for heat 10m                                                                  
   REAL    :: PSIM    ! similarity function for momentum                                                                  
   REAL    :: PSIM2   ! Similarity function for momentum 2m                                                               
   REAL    :: PSIM10  ! Similarity function for momentum 10m                                                              
   REAL    :: DENOMQ  ! Denominator used for flux calc.                                                                   
   REAL    :: DENOMQ2 ! Denominator used for flux calc.                                                                   
   REAL    :: DENOMT2 ! Denominator used for flux calc.                                                                   
   REAL    :: WSPDI   ! Initial wind speed                                                                                
   REAL    :: GZ1OZ0  ! log(za/z0)                                                                                        
   REAL    :: GZ2OZ0  ! log(z2/z0)                                                                                        
   REAL    :: GZ10OZ0 ! log(z10/z0)                                                                                       
   REAL    :: RHOX    ! density                                                                                           
   REAL    :: GOVRTH  ! g/theta for stability L                                                                           
   REAL    :: TGDSA   ! tsk                                                                                               
!   REAL    :: SCR3    ! temporal variable -> input variable T1D                                                                                
   REAL    :: TVIR    ! temporal variable SRC4 -> TVIR                                                                                
   REAL    :: THGB    ! Potential temperature ground                                                                      
   REAL    :: PSFC    ! Surface pressure                                                                                  
   REAL    :: BR      ! bulk richardson number                                                                            
   REAL    :: CPM                                                                                           
   REAL    :: MOL                                                                                           
   REAL    :: ZOL  
   REAL    :: QGH    
   REAL    :: WSPD                                                                                          
                                                                                                                          
   INTEGER :: N,I,K,KK,L,NZOL,NK,NZOL2,NZOL10                                                                             
                                                                                                                          
   REAL    ::  PL,THCON,TVCON,E1                                                                                          
   REAL    ::  ZL,TSKV,DTHVDZ,DTHVM,VCONV,RZOL,RZOL2,RZOL10,ZOL2,ZOL10                                                    
   REAL    ::  DTG,PSIX,DTTHX,PSIX10,PSIT,PSIT2,PSIQ,PSIQ2,PSIQ10                                                         
   REAL    ::  FLUXC,VSGD,Z0Q,VISC,RESTAR,CZIL,RESTAR2                                                                    
!-------------------------------------------------------------------                                                      
   
   MOL = 1./RMOL
   ZL=0.01                                                                                                                
   PSFC=PSFCPA/1000.                                                                                                      
                                                                                                                          
! convert (tah or tgb = tsk) temperature to potential temperature.                                                                    
   TGDSA = TSK                                                                                                            
   THGB  = TSK*(P1000mb/PSFCPA)**RCP                                                                                      
                                                                                                                          
! store virtual, virtual potential and potential temperature
   PL    = P1D/1000.                                                                                                      
   THX   = T1D*(P1000mb*0.001/PL)**RCP                                                                                        
   THVX  = THX*(1.+EP_1*QX)                                                                                                   
   TVIR  = T1D*(1.+EP_1*QX)

! for land points QSFC can come from previous time step                                                                   
   !QSFC=EP_2*E1/(PSFC-E1)                                                                                
   IF (QSFC.LE.0.0) THEN
      !testing this
      E1=SVP1*EXP(SVP2*(TGDSA-SVPT0)/(TGDSA-SVP3))                                                                           
      QSFC=EP_2*E1/(PSFC-E1)                                                                                
      write(*,*) "JREF: IN SFCDIF4, QSFC WAS NEG. NOW = ",QSFC
   ENDIF

! qgh changed to use lowest-level air temp consistent with myjsfc change                                                  
! q2sat = qgh in lsm                                                                                                      
!jref: canres and esat is calculated in the loop so should that be changed??
!   QGH=EP_2*E1/(PL-E1)                                                                                                    
   CPM=CP*(1.+0.8*QX)                                                                                                     
                                                                                                                          
! compute the height of half-sigma levels above ground level                                                         
   !ZA=0.5*DZ8W                                                                                                   
   ZA = ZLVL
                                                                                                                          
! compute density and part of monin-obukhov length L                                                                      
   RHOX=PSFC*1000./(R_D*TVIR)                                                                                             
   GOVRTH=G/THX                                                                                                           
                                                                                                                          
! calculate bulk richardson no. of surface layer,                                                                         
! according to akb(1976), eq(12).                                                                                         
   GZ1OZ0=ALOG(ZA/ZNT)                                                                                                    
   GZ2OZ0=ALOG(2./ZNT)                                                                                                    
   GZ10OZ0=ALOG(10./ZNT)                                                                                                  
   WSPD=SQRT(UX*UX+VX*VX)                                                                                                 
                                                                                                                          
! virtual pot. temperature difference between input layer and lowest model layer                                              
   TSKV=THGB*(1.+EP_1*QSFC)                                                                                               
   DTHVDZ=(THVX-TSKV)                                                                                                     
                                                                                                                          
! convective velocity scale Vc and subgrid-scale velocity Vsg                                                             
! following Beljaars (1995, QJRMS) and Mahrt and Sun (1995, MWR)                                                          
!                                ... HONG Aug. 2001                                                                       
!                                                                                                                         
! VCONV = 0.25*sqrt(g/tskv*pblh(i)*dthvm)                                                                                 
! use Beljaars over land, old MM5 (Wyngaard) formula over water                                                           
                                                                                                                          
!jref:start commented out to see if stability is affected.                                                                                                                          
  FLUXC = MAX(HFX/RHOX/CP + EP_1*TSKV*QFX/RHOX,0.)                                                                                          
  VCONV = VCONVC*(G/TGDSA*PBLH*FLUXC)**.33                                                                                
!  VCONV = 0
!jref:end
                                                                                                                          
! Mahrt and Sun low-res correction                                                                                        
   VSGD = 0.32 * (max(dx/5000.-1.,0.))**.33                                                                               
   WSPD=SQRT(WSPD*WSPD+VCONV*VCONV+VSGD*VSGD)                                                                             
   WSPD=AMAX1(WSPD,0.1)                                                                                                   
   BR=GOVRTH*ZA*DTHVDZ/(WSPD*WSPD)                                                                                        
!  if previously unstable, do not let into regimes 1 and 2                                                                
   IF(MOL.LT.0.) BR=AMIN1(BR,0.0)                                                                                         
   RMOL=-GOVRTH*DTHVDZ*ZA*KARMAN                                                                                          
                                                                                                                          
!-----------------------------------------------------------------------                                                  
!     diagnose basic parameters for the appropriated stability class:                                                     
!                                                                                                                         
!     the stability classes are determined by br (bulk richardson no.)                                                    
!     and hol (height of pbl/monin-obukhov length).                                                                       
!                                                                                                                         
!     criteria for the classes are as follows:                                                                            
!                                                                                                                         
!        1. br .ge. 0.2;                                                                                                  
!               represents nighttime stable conditions (regime=1),                                                        
!                                                                                                                         
!        2. br .lt. 0.2 .and. br .gt. 0.0;                                                                                
!               represents damped mechanical turbulent conditions                                                         
!               (regime=2),                                                                                               
!                                                                                                                         
!        3. br .eq. 0.0                                                                                                   
!               represents forced convection conditions (regime=3),                                                       
!                                                                                                                         
!        4. br .lt. 0.0                                                                                                   
!               represents free convection conditions (regime=4).                                                         
!                                                                                                                         
!-----------------------------------------------------------------------                                                  
                                                                                                                          
   IF (BR.GE.0.2) REGIME=1                                                                                                
   IF (BR.LT.0.2 .AND. BR.GT.0.0) REGIME=2                                                                                
   IF (BR.EQ.0.0) REGIME=3                                                                                                
   IF (BR.LT.0.0) REGIME=4                                                                                                
                                                                                                                          
   SELECT CASE(REGIME)                                                                                                    
     CASE(1) ! class 1; stable (nighttime) conditions:                                                                    
       PSIM=-10.*GZ1OZ0                                                                                                   
! lower limit on psi in stable conditions                                                                                 
       PSIM=AMAX1(PSIM,-10.)                                                                                              
       PSIH=PSIM                                                                                                          
       PSIM10=10./ZA*PSIM                                                                                                 
       PSIM10=AMAX1(PSIM10,-10.)                                                                                          
       PSIH10=PSIM10                                                                                                      
       PSIM2=2./ZA*PSIM                                                                                                   
       PSIM2=AMAX1(PSIM2,-10.)                                                                                            
       PSIH2=PSIM2                                                                                                        
                                                                                                                          
! 1.0 over Monin-Obukhov length                                                                                           
       IF(UST.LT.0.01)THEN                                                                                                
          RMOL=BR*GZ1OZ0 !ZA/L                                                                                            
       ELSE                                                                                                               
          RMOL=KARMAN*GOVRTH*ZA*MOL/(UST*UST) !ZA/L                                                                       
       ENDIF                                                                                                              
       RMOL=AMIN1(RMOL,9.999) ! ZA/L                                                                                      
       RMOL = RMOL/ZA !1.0/L                                                                                              
                                                                                                                          
     CASE(2) ! class 2; damped mechanical turbulence:                                                                     
       PSIM=-5.0*BR*GZ1OZ0/(1.1-5.0*BR)                                                                                   
! lower limit on psi in stable conditions                                                                                 
       PSIM=AMAX1(PSIM,-10.)                                                                                              
! AKB(1976), EQ(16).                                                                                                      
       PSIH=PSIM                                                                                                          
       PSIM10=10./ZA*PSIM                                                                                                 
       PSIM10=AMAX1(PSIM10,-10.)                                                                                          
       PSIH10=PSIM10                                                                                                      
       PSIM2=2./ZA*PSIM                                                                                                   
       PSIM2=AMAX1(PSIM2,-10.)                                                                                            
       PSIH2=PSIM2                                                                                                        
                                                                                                                          
     ! Linear form: PSIM = -0.5*ZA/L; e.g, see eqn 16 of                                                                  
     ! Blackadar, Modeling the nocturnal boundary layer, Preprints,                                                       
     ! Third Symposium on Atmospheric Turbulence Diffusion and Air Quality,                                               
     ! Raleigh, NC, 1976                                                                                                  
       ZOL = BR*GZ1OZ0/(1.00001-5.0*BR)                                                                                   
                                                                                                                          
       IF ( ZOL .GT. 0.5 ) THEN ! linear form ok                                                                          
        ! Holtslag and de Bruin, J. App. Meteor 27, 689-704, 1988;                                                        
        ! see also, Launiainen, Boundary-Layer Meteor 76,165-179, 1995                                                    
        ! Eqn (8) of Launiainen, 1995                                                                                     
          ZOL = ( 1.89*GZ1OZ0 + 44.2 ) * BR*BR    &                                                                       
               + ( 1.18*GZ1OZ0 - 1.37 ) * BR                                                                              
          ZOL=AMIN1(ZOL,9.999)                                                                                            
       END IF                                                                                                             
                                                                                                                          
! 1.0 over Monin-Obukhov length                                                                                           
       RMOL= ZOL/ZA                                                                                                       
                                                                                                                          
     CASE(3)  ! class 3; forced convection:                                                                               
       PSIM=0.0                                                                                                           
       PSIH=PSIM                                                                                                          
       PSIM10=0.                                                                                                          
       PSIH10=PSIM10                                                                                                      
       PSIM2=0.                                                                                                           
       PSIH2=PSIM2                                                                                                        
       IF(UST.LT.0.01)THEN                                                                                                
         ZOL=BR*GZ1OZ0                                                                                                    
       ELSE                                                                                                               
         ZOL=KARMAN*GOVRTH*ZA*MOL/(UST*UST)                                                                               
       ENDIF                                                                                                              
                                                                                                                          
       RMOL = ZOL/ZA                                                                                                      
                                                                                                                          
     CASE(4) ! class 4; free convection:                                                                                  
       IF(UST.LT.0.01)THEN                                                                                                
         ZOL=BR*GZ1OZ0                                                                                                    
       ELSE                                                                                                               
         ZOL=KARMAN*GOVRTH*ZA*MOL/(UST*UST)                                                                               
       ENDIF                                                                                                              
       ZOL10=10./ZA*ZOL                                                                                                   
       ZOL2=2./ZA*ZOL                                                                                                     
       ZOL=AMIN1(ZOL,0.)                                                                                                  
       ZOL=AMAX1(ZOL,-9.9999)                                                                                             
       ZOL10=AMIN1(ZOL10,0.)                                                                                              
       ZOL10=AMAX1(ZOL10,-9.9999)                                                                                         
       ZOL2=AMIN1(ZOL2,0.)                                                                                                
       ZOL2=AMAX1(ZOL2,-9.9999)                                                                                           
       NZOL=INT(-ZOL*100.)                                                                                                
       RZOL=-ZOL*100.-NZOL                                                                                                
       NZOL10=INT(-ZOL10*100.)                                                                                            
       RZOL10=-ZOL10*100.-NZOL10                                                                                          
       NZOL2=INT(-ZOL2*100.)                                                                                              
       RZOL2=-ZOL2*100.-NZOL2                                                                                             
       PSIM=PSIMTB(NZOL)+RZOL*(PSIMTB(NZOL+1)-PSIMTB(NZOL))                                                               
       PSIH=PSIHTB(NZOL)+RZOL*(PSIHTB(NZOL+1)-PSIHTB(NZOL))                                                               
       PSIM10=PSIMTB(NZOL10)+RZOL10*(PSIMTB(NZOL10+1)-PSIMTB(NZOL10))                                                     
       PSIH10=PSIHTB(NZOL10)+RZOL10*(PSIHTB(NZOL10+1)-PSIHTB(NZOL10))                                                     
       PSIM2=PSIMTB(NZOL2)+RZOL2*(PSIMTB(NZOL2+1)-PSIMTB(NZOL2))                                                          
       PSIH2=PSIHTB(NZOL2)+RZOL2*(PSIHTB(NZOL2+1)-PSIHTB(NZOL2))                                                          
                                                                                                                          
! limit psih and psim in the case of thin layers and high roughness                                                       
! this prevents denominator in fluxes from getting too small                                                              
!       PSIH=AMIN1(PSIH,0.9*GZ1OZ0)                                                                                       
!       PSIM=AMIN1(PSIM,0.9*GZ1OZ0)                                                                                       
       PSIH=AMIN1(PSIH,0.9*GZ1OZ0)                                                                                        
       PSIM=AMIN1(PSIM,0.9*GZ1OZ0)                                                                                        
       PSIH2=AMIN1(PSIH2,0.9*GZ2OZ0)                                                                                      
       PSIM10=AMIN1(PSIM10,0.9*GZ10OZ0)                                                                                   
! AHW: mods to compute ck, cd                                                                                             
       PSIH10=AMIN1(PSIH10,0.9*GZ10OZ0)                                                                                   
                                                                                                                          
       RMOL = ZOL/ZA                                                                                                      
                                                                                                                          
   END SELECT ! stability regime done                                                                                     
                                                                                                                          
! compute the frictional velocity: ZA(1982) EQS(2.60),(2.61).                                                             
   DTG=THX-THGB                                                                                                           
   PSIX=GZ1OZ0-PSIM                                                                                                       
   PSIX10=GZ10OZ0-PSIM10                                                                                                  
                                                                                                                          
! lower limit added to prevent large flhc in soil model                                                                   
! activates in unstable conditions with thin layers or high z0                                                            
   PSIT=AMAX1(GZ1OZ0-PSIH,2.) !does this still apply???? jref                                                             
   PSIQ=ALOG(KARMAN*UST*ZA/XKA+ZA/ZL)-PSIH                                                                                
   PSIT2=GZ2OZ0-PSIH2                                                                                                     
   PSIQ2=ALOG(KARMAN*UST*2./XKA+2./ZL)-PSIH2                                                                              
! AHW: mods to compute ck, cd                                                                                             
   PSIQ10=ALOG(KARMAN*UST*10./XKA+10./ZL)-PSIH10                                                                          

!jref:start - commented out since these values can be produced by sfclay routine   
!   IF(PRESENT(ck) .and. PRESENT(cd) .and. PRESENT(cka) .and. PRESENT(cda)) THEN                                           
!      Ck=(karman/psix10)*(karman/psiq10)                                                                                  
!      Cd=(karman/psix10)*(karman/psix10)                                                                                  
!      Cka=(karman/psix)*(karman/psiq)                                                                                     
!      Cda=(karman/psix)*(karman/psix)                                                                                     
!   ENDIF                                                                                                                  

!   WRITE(*,*) "KARMAN=",KARMAN
!   WRITE(*,*) "UST=",UST
!   WRITE(*,*) "XKA=",XKA
!   WRITE(*,*) "ZA =",ZA
!   WRITE(*,*) "ZL =",ZL
!   WRITE(*,*) "PSIH=",PSIH
!   WRITE(*,*) "PSIQ=",PSIQ,"PSIT=",PSIT

   IF ( PRESENT(IZ0TLND) ) THEN                                                                                           
      IF ( IZ0TLND.EQ.1 ) THEN                                                                                            
         ZL=ZNT                                                                                                           
!       czil related changes for land                                                                                     
         VISC=(1.32+0.009*(T1D-273.15))*1.E-5                                                                            
         RESTAR=UST*ZL/VISC                                                                                               
!       modify CZIL according to Chen & Zhang, 2009                                                                       
                                                                                                                          
         CZIL = 10.0 ** ( -0.40 * ( ZL / 0.07 ) )                                                                         
                                                                                                                          
         PSIT=GZ1OZ0-PSIH+CZIL*KARMAN*SQRT(RESTAR)                                                                        
         PSIQ=GZ1OZ0-PSIH+CZIL*KARMAN*SQRT(RESTAR)                                                                        
         PSIT2=GZ2OZ0-PSIH2+CZIL*KARMAN*SQRT(RESTAR)                                                                      
         PSIQ2=GZ2OZ0-PSIH2+CZIL*KARMAN*SQRT(RESTAR)                                                                      
      ENDIF                                                                                                               
   ENDIF                                                                                                                  
                                                                                                                          

! to prevent oscillations average with old value                                                                          
   UST=0.5*UST+0.5*KARMAN*WSPD/PSIX                                                                                       
   UST=AMAX1(UST,0.1)                                                                                                     
!jref: should this be converted to RMOL???
   MOL=KARMAN*DTG/PSIT/PRT                                                                                                
   DENOMQ=PSIQ                                                                                                            
   DENOMQ2=PSIQ2                                                                                                          
   DENOMT2=PSIT2                                                                                                          
!   WRITE(*,*) "ILOC,JLOC=",ILOC,JLOC,"DENOMQ=",DENOMQ
!   WRITE(*,*) "UST=",UST,"PSIT=",PSIT
!   call wrf_error_fatal("stop in sfcdif4")
                                                                                                                          
! calculate exchange coefficients                                                                                         
!jref: start exchange coefficient for momentum
   CM =KARMAN*KARMAN/(PSIX*PSIX)
!jref:end
   CHS=UST*KARMAN/DENOMQ                                                                                                  
!        GZ2OZ0=ALOG(2./ZNT)                                                                                              
!        PSIM2=-10.*GZ2OZ0                                                                                                
!        PSIM2=AMAX1(PSIM2,-10.)                                                                                          
!        PSIH2=PSIM2                                                                                                      
   CQS2=UST*KARMAN/DENOMQ2                                                                                                
   CHS2=UST*KARMAN/DENOMT2                                                                                                
! jref: in last iteration calculate diagnostics                                                                           
                                                                                                                         
   U10=UX*PSIX10/PSIX                                                                                                     
   V10=VX*PSIX10/PSIX                                                                                                     
                                                                                                                          
! jref: check the following for correct calculation                                                                       
!   TH2=THGB+DTG*PSIT2/PSIT                                                                                               
!   Q2=QSFC+(QX-QSFC)*PSIQ2/PSIQ                                                                                          
!   T2 = TH2*(PSFCPA/P1000mb)**RCP                                                                                        
                                                                                                                          
   END SUBROUTINE SFCDIF4                                                                                                 
!jref:end
!  ==================================================================================================
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
! ==================================================================================================
! ----------------------------------------------------------------------
  SUBROUTINE STOMATA (VEGTYP  ,MPE     ,APAR    ,FOLN    ,ILOC    , JLOC, & !in
                      TV      ,EI      ,EA      ,SFCTMP  ,SFCPRS  , & !in
                      O2      ,CO2     ,IGS     ,BTRAN   ,RB      , & !in
                      RS      ,PSN     )                              !out
! --------------------------------------------------------------------------------------------------
  USE NOAHMP_VEG_PARAMETERS
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
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
         RS = 1./BP(VEGTYP) * CF
         PSN = 0.

         IF (APAR .LE. 0.) RETURN

         FNF = MIN( FOLN/MAX(MPE,FOLNMX(VEGTYP)), 1.0 )
         TC  = TV-TFRZ
         PPF = 4.6*APAR
         J   = PPF*QE25(VEGTYP)
         KC  = KC25(VEGTYP) * F1(AKC(VEGTYP),TC)
         KO  = KO25(VEGTYP) * F1(AKO(VEGTYP),TC)
         AWC = KC * (1.+O2/KO)
         CP  = 0.5*KC/KO*O2*0.21
         VCMX = VCMX25(VEGTYP) / F2(TC) * FNF * BTRAN * F1(AVCMX(VEGTYP),TC)

! first guess ci

         CI = 0.7*CO2*C3PSN(VEGTYP) + 0.4*CO2*(1.-C3PSN(VEGTYP))

! rb: s/m -> s m**2 / umol

         RLB = RB/CF

! constrain ea

         CEA = MAX(0.25*EI*C3PSN(VEGTYP)+0.40*EI*(1.-C3PSN(VEGTYP)), MIN(EA,EI) )

! ci iteration
!jref: C3PSN is equal to 1 for all veg types.
       DO ITER = 1, NITER
            WJ = MAX(CI-CP,0.)*J/(CI+2.*CP)*C3PSN(VEGTYP)  + J*(1.-C3PSN(VEGTYP))
            WC = MAX(CI-CP,0.)*VCMX/(CI+AWC)*C3PSN(VEGTYP) + VCMX*(1.-C3PSN(VEGTYP))
            WE = 0.5*VCMX*C3PSN(VEGTYP) + 4000.*VCMX*CI/SFCPRS*(1.-C3PSN(VEGTYP))
            PSN = MIN(WJ,WC,WE) * IGS

            CS = MAX( CO2-1.37*RLB*SFCPRS*PSN, MPE )
            A = MP(VEGTYP)*PSN*SFCPRS*CEA / (CS*EI) + BP(VEGTYP)
            B = ( MP(VEGTYP)*PSN*SFCPRS/CS + BP(VEGTYP) ) * RLB - 1.
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
! ==================================================================================================
  SUBROUTINE CANRES (PAR   ,SFCTMP,RCSOIL ,EAH   ,SFCPRS , & !in
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

    CALL CALHUM(SFCTMP, SFCPRS, Q2SAT, DQSDT2)

! contribution due to incoming solar radiation

    FF  = 2.0 * PAR / RGL                
    RCS = (FF + RSMIN / RSMAX) / (1.0+ FF)
    RCS = MAX (RCS,0.0001)

! contribution due to air temperature

    RCT = 1.0- 0.0016* ( (TOPT - SFCTMP)**2.0)
    RCT = MAX (RCT,0.0001)

! contribution due to vapor pressure deficit

    RCQ = 1.0/ (1.0+ HS * MAX(0.,Q2SAT-Q2))
    RCQ = MAX (RCQ,0.01)

! determine canopy resistance due to all factors

    RC  = RSMIN / (RCS * RCT * RCQ * RCSOIL)
    PSN = -999.99       ! PSN not applied for dynamic carbon

  END SUBROUTINE CANRES
! ==================================================================================================
        SUBROUTINE CALHUM(SFCTMP, SFCPRS, Q2SAT, DQSDT2)

        IMPLICIT NONE

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
! ==================================================================================================
  SUBROUTINE TSNOSOI (ICE     ,NSOIL   ,NSNOW   ,ISNOW   ,IST     , & !in
                      TBOT    ,ZSNSO   ,SSOIL   ,DF      ,HCPCT   , & !in
                      ZBOT    ,SAG     ,DT      ,SNOWH   ,DZSNSO  , & !in
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
    REAL,                            INTENT(IN)  :: ZBOT   !from soil surface (m)
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

    ZBOTSNO = ZBOT - SNOWH    !from snow surface

! snow/soil heat storage for energy balance check

    DO IZ = ISNOW+1, NSOIL
       TBEG(IZ) = STC(IZ)
    ENDDO

! compute soil temperatures

      CALL HRT   (NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , &
                  STC       ,TBOT      ,ZBOTSNO   ,DT        , &
                  DF        ,HCPCT     ,SSOIL     ,PHI       , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  EFLXB     )

      CALL HSTEP (NSNOW     ,NSOIL     ,ISNOW     ,DT        , &
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

    if (OPT_STC == 1) THEN   ! semi-implicit
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
! ==================================================================================================
! ----------------------------------------------------------------------
  SUBROUTINE HRT (NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , &
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
           IF (OPT_STC == 1) THEN
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
! ==================================================================================================
! ----------------------------------------------------------------------
  SUBROUTINE HSTEP (NSNOW     ,NSOIL     ,ISNOW     ,DT        ,  &
                    AI        ,BI        ,CI        ,RHSTS     ,  &
                    STC       )  
! ----------------------------------------------------------------------
! CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD.
! ----------------------------------------------------------------------
    implicit none
! ----------------------------------------------------------------------
! input

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
! ==================================================================================================
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
! ----------------------------------------------------------------------
! ==================================================================================================
  SUBROUTINE PHASECHANGE (NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & !in
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
               SUPERCOOL(J) = SMCMAX*(SMP/PSISAT)**(-1./BEXP)
               SUPERCOOL(J) = SUPERCOOL(J)*DZSNSO(J)*1000.        !(mm)
            END IF
         END IF
         IF (OPT_FRZ == 2) THEN
               CALL FRH2O (SUPERCOOL(J),STC(J),SMC(J),SH2O(J))
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
        HEATR  = HM(1) - HFUS*(TEMP1-SNEQV)/DT  
        IF (HEATR > 0.) THEN
              XM(1) = HEATR*DT/HFUS             
              HM(1) = HEATR                    
        ELSE
              XM(1) = 0.
              HM(1) = 0.
        ENDIF
        QMELT   = MAX(0.,(TEMP1-SNEQV))/DT
        XMF     = HFUS*QMELT
        PONDING = TEMP1-SNEQV
    ENDIF

! The rate of melting and freezing for snow and soil

    DO J = ISNOW+1,NSOIL
      IF (IMELT(J) > 0 .AND. ABS(HM(J)) > 0.) THEN

         HEATR = 0.
         IF (XM(J) > 0.) THEN                            
            MICE(J) = MAX(0., WICE0(J)-XM(J))
            HEATR = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ELSE IF (XM(J) < 0.) THEN                      
            IF (J <= 0) THEN                             ! snow
               MICE(J) = MIN(WMASS0(J), WICE0(J)-XM(J))  
            ELSE                                         ! soil
               IF (WMASS0(J) < SUPERCOOL(J)) THEN
                  MICE(J) = 0.
               ELSE
                  MICE(J) = MIN(WMASS0(J) - SUPERCOOL(J),WICE0(J)-XM(J))
                  MICE(J) = MAX(MICE(J),0.0)
               ENDIF
            ENDIF
            HEATR = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ENDIF

         MLIQ(J) = MAX(0.,WMASS0(J)-MICE(J))

         IF (ABS(HEATR) > 0.) THEN
            STC(J) = STC(J) + FACT(J)*HEATR
            IF (J <= 0) THEN                             ! snow
               IF (MLIQ(J)*MICE(J)>0.) STC(J) = TFRZ
            END IF
         ENDIF

         XMF = XMF + HFUS * (WICE0(J)-MICE(J))/DT

         IF (J < 1) THEN
            QMELT = QMELT + MAX(0.,(WICE0(J)-MICE(J)))/DT
         ENDIF
      ENDIF
    ENDDO

    DO J = ISNOW+1,0             ! snow
       SNLIQ(J) = MLIQ(J)
       SNICE(J) = MICE(J)
    END DO

    DO J = 1, NSOIL              ! soil
       SH2O(J) =  MLIQ(J)            / (1000. * DZSNSO(J))
       SMC(J)  = (MLIQ(J) + MICE(J)) / (1000. * DZSNSO(J))
    END DO
   
  END SUBROUTINE PHASECHANGE
! ==================================================================================================
  SUBROUTINE FRH2O (FREE,TKELV,SMC,SH2O)

! ----------------------------------------------------------------------
! SUBROUTINE FRH2O
! ----------------------------------------------------------------------
! CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT IF
! TEMPERATURE IS BELOW 273.15K (TFRZ).  REQUIRES NEWTON-TYPE ITERATION
! TO SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF KOREN ET AL
! (1999, JGR, VOL 104(D16), 19569-19585).
! ----------------------------------------------------------------------
! NEW VERSION (JUNE 2001): MUCH FASTER AND MORE ACCURATE NEWTON
! ITERATION ACHIEVED BY FIRST TAKING LOG OF EQN CITED ABOVE -- LESS THAN
! 4 (TYPICALLY 1 OR 2) ITERATIONS ACHIEVES CONVERGENCE.  ALSO, EXPLICIT
! 1-STEP SOLUTION OPTION FOR SPECIAL CASE OF PARAMETER CK=0, WHICH
! REDUCES THE ORIGINAL IMPLICIT EQUATION TO A SIMPLER EXPLICIT FORM,
! KNOWN AS THE "FLERCHINGER EQN". IMPROVED HANDLING OF SOLUTION IN THE
! LIMIT OF FREEZING POINT TEMPERATURE TFRZ.
! ----------------------------------------------------------------------
! INPUT:

!   TKELV.........TEMPERATURE (Kelvin)
!   SMC...........TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC)
!   SH2O..........LIQUID SOIL MOISTURE CONTENT (VOLUMETRIC)
!   B.............SOIL TYPE "B" PARAMETER (FROM REDPRM)
!   PSISAT........SATURATED SOIL MATRIC POTENTIAL (FROM REDPRM)

! OUTPUT:
!   FREE..........SUPERCOOLED LIQUID WATER CONTENT [m3/m3]
! ----------------------------------------------------------------------
    IMPLICIT NONE
    REAL, INTENT(IN)     :: SH2O,SMC,TKELV
    REAL, INTENT(OUT)    :: FREE
    REAL                 :: BX,DENOM,DF,DSWL,FK,SWL,SWLK
    INTEGER              :: NLOG,KCOUNT
!      PARAMETER(CK = 0.0)
    REAL, PARAMETER      :: CK = 8.0, BLIM = 5.5, ERROR = 0.005,       &
         DICE = 920.0
    CHARACTER(LEN=80)    :: message

! ----------------------------------------------------------------------
! LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)
! SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT IS
! NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES.
! ----------------------------------------------------------------------
    BX = BEXP
! ----------------------------------------------------------------------
! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
! ----------------------------------------------------------------------

    IF (BEXP >  BLIM) BX = BLIM
    NLOG = 0

! ----------------------------------------------------------------------
!  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (TFRZ), SH2O = SMC
! ----------------------------------------------------------------------
    KCOUNT = 0
    IF (TKELV > (TFRZ- 1.E-3)) THEN
       FREE = SMC
    ELSE

! ----------------------------------------------------------------------
! OPTION 1: ITERATED SOLUTION IN KOREN ET AL, JGR, 1999, EQN 17
! ----------------------------------------------------------------------
! INITIAL GUESS FOR SWL (frozen content)
! ----------------------------------------------------------------------
       IF (CK /= 0.0) THEN
          SWL = SMC - SH2O
! ----------------------------------------------------------------------
! KEEP WITHIN BOUNDS.
! ----------------------------------------------------------------------
          IF (SWL > (SMC -0.02)) SWL = SMC -0.02
! ----------------------------------------------------------------------
!  START OF ITERATIONS
! ----------------------------------------------------------------------
          IF (SWL < 0.) SWL = 0.
1001      Continue
          IF (.NOT.( (NLOG < 10) .AND. (KCOUNT == 0)))   goto 1002
          NLOG = NLOG +1
          DF = ALOG ( ( PSISAT * GRAV / hfus ) * ( ( 1. + CK * SWL )**2.) * &
               ( SMCMAX / (SMC - SWL) )** BX) - ALOG ( - (               &
               TKELV - TFRZ)/ TKELV)
          DENOM = 2. * CK / ( 1. + CK * SWL ) + BX / ( SMC - SWL )
          SWLK = SWL - DF / DENOM
! ----------------------------------------------------------------------
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
! ----------------------------------------------------------------------
          IF (SWLK > (SMC -0.02)) SWLK = SMC - 0.02
          IF (SWLK < 0.) SWLK = 0.

! ----------------------------------------------------------------------
! MATHEMATICAL SOLUTION BOUNDS APPLIED.
! ----------------------------------------------------------------------
          DSWL = ABS (SWLK - SWL)
! IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)
! WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.
! ----------------------------------------------------------------------
          SWL = SWLK
          IF ( DSWL <= ERROR ) THEN
             KCOUNT = KCOUNT +1
          END IF
! ----------------------------------------------------------------------
!  END OF ITERATIONS
! ----------------------------------------------------------------------
! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
! ----------------------------------------------------------------------
          goto 1001
1002      continue
          FREE = SMC - SWL
       END IF
! ----------------------------------------------------------------------
! END OPTION 1
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0
! IN KOREN ET AL., JGR, 1999, EQN 17
! APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
! ----------------------------------------------------------------------
       IF (KCOUNT == 0) THEN
          write(message, '("Flerchinger used in NEW version. Iterations=", I6)') NLOG
          call wrf_message(trim(message))
          FK = ( ( (hfus / (GRAV * ( - PSISAT)))*                    &
               ( (TKELV - TFRZ)/ TKELV))** ( -1/ BX))* SMCMAX
          IF (FK < 0.02) FK = 0.02
          FREE = MIN (FK, SMC)
! ----------------------------------------------------------------------
! END OPTION 2
! ----------------------------------------------------------------------
       END IF
    END IF
! ----------------------------------------------------------------------
  END SUBROUTINE FRH2O
! ----------------------------------------------------------------------
! ==================================================================================================
! **********************End of energy subroutines***********************
! ==================================================================================================
  SUBROUTINE WATER (VEGTYP ,NSNOW  ,NSOIL  ,IMELT  ,DT     ,UU     , & !in
                    VV     ,FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & !in
                    ESAI   ,SFCTMP ,QVAP   ,QDEW   ,ZSOIL  ,BTRANI , & !in
                    FICEOLD,PONDING,TG     ,IST    ,FVEG   ,ILOC   ,JLOC , & !in
                    ISNOW  ,CANLIQ ,CANICE ,TV     ,SNOWH  ,SNEQV  , & !inout
                    SNICE  ,SNLIQ  ,STC    ,ZSNSO  ,SH2O   ,SMC    , & !inout
                    SICE   ,ZWT    ,WA     ,WT     ,DZSNSO ,WSLAKE , & !inout
                    CMC    ,ECAN   ,ETRAN  ,FWET   ,RUNSRF ,RUNSUB , & !out
                    QIN    ,QDIS   ,QSNOW  ,PONDING1       ,PONDING2,&
                    ISURBAN,QSNBOT,FPICE                             &
#ifdef WRF_HYDRO
                        ,sfcheadrt                     &
#endif
                    )  !out
! ----------------------------------------------------------------------  
! Code history:
! Initial code: Guo-Yue Niu, Oct. 2007
! ----------------------------------------------------------------------
  implicit none
! ----------------------------------------------------------------------
! input
  INTEGER,                         INTENT(IN)    :: ILOC    !grid index
  INTEGER,                         INTENT(IN)    :: JLOC    !grid index
  INTEGER,                         INTENT(IN)    :: VEGTYP  !vegetation type
  INTEGER,                         INTENT(IN)    :: NSNOW   !maximum no. of snow layers
  INTEGER                        , INTENT(IN)    :: IST     !surface type 1-soil; 2-lake
  INTEGER,                         INTENT(IN)    :: NSOIL   !no. of soil layers
  INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT   !melting state index [1-melt; 2-freeze]
  REAL,                            INTENT(IN)    :: DT      !main time step (s)
  REAL,                            INTENT(IN)    :: UU      !u-direction wind speed [m/s]
  REAL,                            INTENT(IN)    :: VV      !v-direction wind speed [m/s]
  REAL,                            INTENT(IN)    :: FCEV    !canopy evaporation (w/m2) [+ to atm ]
  REAL,                            INTENT(IN)    :: FCTR    !transpiration (w/m2) [+ to atm]
  REAL,                            INTENT(IN)    :: QPRECC  !convective precipitation (mm/s)
  REAL,                            INTENT(IN)    :: QPRECL  !large-scale precipitation (mm/s)
  REAL,                            INTENT(IN)    :: ELAI    !leaf area index, after burying by snow
  REAL,                            INTENT(IN)    :: ESAI    !stem area index, after burying by snow
  REAL,                            INTENT(IN)    :: SFCTMP  !surface air temperature [k]
  REAL,                            INTENT(IN)    :: QVAP    !soil surface evaporation rate[mm/s]
  REAL,                            INTENT(IN)    :: QDEW    !soil surface dew rate[mm/s]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL   !depth of layer-bottom from soil surface
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: BTRANI  !soil water stress factor (0 to 1)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD !ice fraction at last timestep
!  REAL                           , INTENT(IN)    :: PONDING ![mm]
  REAL                           , INTENT(IN)    :: TG      !ground temperature (k)
  REAL                           , INTENT(IN)    :: FVEG    !greeness vegetation fraction (-)

! input/output
  INTEGER,                         INTENT(INOUT) :: ISNOW   !actual no. of snow layers
  REAL,                            INTENT(INOUT) :: CANLIQ  !intercepted liquid water (mm)
  REAL,                            INTENT(INOUT) :: CANICE  !intercepted ice mass (mm)
  REAL,                            INTENT(INOUT) :: TV      !vegetation temperature (k)
  REAL,                            INTENT(INOUT) :: SNOWH   !snow height [m]
  REAL,                            INTENT(INOUT) :: SNEQV   !snow water eqv. [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE   !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ   !snow layer liquid water [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC     !snow/soil layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO   !depth of snow/soil layer-bottom
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO  !snow/soil layer thickness [m]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O    !soil liquid water content [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE    !soil ice content [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC     !total soil water content [m3/m3]
  REAL,                            INTENT(INOUT) :: ZWT     !the depth to water table [m]
  REAL,                            INTENT(INOUT) :: WA      !water storage in aquifer [mm]
  REAL,                            INTENT(INOUT) :: WT      !water storage in aquifer 
                                                            !+ stuarated soil [mm]
  REAL,                            INTENT(INOUT) :: WSLAKE  !water storage in lake (can be -) (mm)
  REAL                           , INTENT(INOUT) :: PONDING ![mm]

! output
  REAL,                            INTENT(OUT)   :: CMC     !intercepted water per ground area (mm)
  REAL,                            INTENT(OUT)   :: ECAN    !evap of intercepted water (mm/s) [+]
  REAL,                            INTENT(OUT)   :: ETRAN   !transpiration rate (mm/s) [+]
  REAL,                            INTENT(OUT)   :: FWET    !wetted/snowed fraction of canopy (-)
  REAL,                            INTENT(OUT)   :: RUNSRF  !surface runoff [mm/s] 
  REAL,                            INTENT(OUT)   :: RUNSUB  !baseflow (sturation excess) [mm/s]
  REAL,                            INTENT(OUT)   :: QIN     !groundwater recharge [mm/s]
  REAL,                            INTENT(OUT)   :: QDIS    !groundwater discharge [mm/s]
  REAL,                            INTENT(OUT)   :: QSNOW   !snow at ground srf (mm/s) [+]
  REAL,                            INTENT(OUT)   :: PONDING1
  REAL,                            INTENT(OUT)   :: PONDING2
  REAL,                            INTENT(OUT)   :: QSNBOT  !melting water out of snow bottom [mm/s]
  REAL,                            INTENT(OUT)   :: FPICE   !snow fraction in precipitation

  INTEGER,                         INTENT(IN)    :: ISURBAN

! local
  INTEGER                                        :: IZ
  REAL                                           :: QINSUR  !water input on soil surface [m/s]
  REAL                                           :: QRAIN   !rain at ground srf (mm) [+]
  REAL                                           :: QSEVA   !soil surface evap rate [mm/s]
  REAL                                           :: QSDEW   !soil surface dew rate [mm/s]
  REAL                                           :: QSNFRO  !snow surface frost rate[mm/s]
  REAL                                           :: QSNSUB  !snow surface sublimation rate [mm/s]
  REAL                                           :: SNOWHIN !snow depth increasing rate (m/s)
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

   CALL CANWATER (VEGTYP ,DT     ,SFCTMP ,UU     ,VV     , & !in
                  FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & !in
                  ESAI   ,IST    ,TG     ,FVEG   ,ILOC   , JLOC, & !in
                  CANLIQ ,CANICE ,TV     ,                 & !inout
                  CMC    ,ECAN   ,ETRAN  ,QRAIN  ,QSNOW  , & !out
                  SNOWHIN,FWET   ,FPICE   )                           !out

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

     CALL SNOWWATER (NSNOW  ,NSOIL  ,IMELT  ,DT     ,ZSOIL  , & !in
          &          SFCTMP ,SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB , & !in
          &          QRAIN  ,FICEOLD,ILOC   ,JLOC   ,         & !in
          &          ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & !inout
          &          SH2O   ,SICE   ,STC    ,ZSNSO  ,DZSNSO , & !inout
          &          QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  !out

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

    DO IZ = 1, NROOT
       ETRANI(IZ) = ETRAN * BTRANI(IZ) * 0.001
    ENDDO

#ifdef WRF_HYDRO
       QINSUR = QINSUR+sfcheadrt/DT*0.001  !sfcheadrt units (m)
#endif

! lake/soil water balances

    IF (IST == 2) THEN                                        ! lake
       RUNSRF = 0.
       IF(WSLAKE >= WSLMAX) RUNSRF = QINSUR*1000.             !mm/s
       WSLAKE = WSLAKE + (QINSUR-QSEVA)*1000.*DT -RUNSRF*DT   !mm
    ELSE                                                      ! soil
       CALL      SOILWATER (NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & !in
                            QINSUR ,QSEVA  ,ETRANI ,SICE   ,ILOC   , JLOC , & !in
                            SH2O   ,SMC    ,ZWT    ,VEGTYP ,ISURBAN, & !inout
                            RUNSRF ,QDRAIN ,RUNSUB ,WCND   ,FCRMAX )   !out
 
       IF(OPT_RUN == 1) THEN 
          CALL GROUNDWATER (NSNOW  ,NSOIL  ,DT     ,SICE   ,ZSOIL  , & !in
                            STC    ,WCND   ,FCRMAX ,ILOC   ,JLOC   , & !in
                            SH2O   ,ZWT    ,WA     ,WT     ,         & !inout
                            QIN    ,QDIS   )                           !out
          RUNSUB       = QDIS          !mm/s
       END IF

       IF(OPT_RUN == 3 .or. OPT_RUN == 4) THEN 
          RUNSUB       = RUNSUB + QDRAIN        !mm/s
       END IF

       DO IZ = 1,NSOIL
           SMC(IZ) = SH2O(IZ) + SICE(IZ)
       ENDDO
    ENDIF

    RUNSUB       = RUNSUB + SNOFLOW         !mm/s

  END SUBROUTINE WATER
! ==================================================================================================
  SUBROUTINE CANWATER (VEGTYP ,DT     ,SFCTMP ,UU     ,VV     , & !in
                       FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & !in
                       ESAI   ,IST    ,TG     ,FVEG   ,ILOC   , JLOC , & !in
                       CANLIQ ,CANICE ,TV     ,                 & !inout
                       CMC    ,ECAN   ,ETRAN  ,QRAIN  ,QSNOW  , & !out
                       SNOWHIN,FWET   ,FPICE   )                           !out

! ------------------------ code history ------------------------------
! canopy hydrology
! --------------------------------------------------------------------
  USE NOAHMP_VEG_PARAMETERS
! --------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------ input/output variables --------------------
! input
  INTEGER,INTENT(IN)  :: ILOC    !grid index
  INTEGER,INTENT(IN)  :: JLOC    !grid index
  INTEGER,INTENT(IN)  :: VEGTYP  !vegetation type
  REAL,   INTENT(IN)  :: DT      !main time step (s)
  REAL,   INTENT(IN)  :: SFCTMP  !air temperature (k)
  REAL,   INTENT(IN)  :: UU      !u-direction wind speed [m/s]
  REAL,   INTENT(IN)  :: VV      !v-direction wind speed [m/s]
  REAL,   INTENT(IN)  :: FCEV    !canopy evaporation (w/m2) [+ = to atm]
  REAL,   INTENT(IN)  :: FCTR    !transpiration (w/m2) [+ = to atm]
  REAL,   INTENT(IN)  :: QPRECC  !convective precipitation (mm/s)
  REAL,   INTENT(IN)  :: QPRECL  !large-scale precipitation (mm/s)
  REAL,   INTENT(IN)  :: ELAI    !leaf area index, after burying by snow
  REAL,   INTENT(IN)  :: ESAI    !stem area index, after burying by snow
  INTEGER,INTENT(IN)  :: IST     !surface type 1-soil; 2-lake
  REAL,   INTENT(IN)  :: TG      !ground temperature (k)
  REAL,   INTENT(IN)  :: FVEG    !greeness vegetation fraction (-)

! input & output
  REAL, INTENT(INOUT) :: CANLIQ  !intercepted liquid water (mm)
  REAL, INTENT(INOUT) :: CANICE  !intercepted ice mass (mm)
  REAL, INTENT(INOUT) :: TV      !vegetation temperature (k)

! output
  REAL, INTENT(OUT)   :: CMC     !intercepted water (mm)
  REAL, INTENT(OUT)   :: ECAN    !evaporation of intercepted water (mm/s) [+]
  REAL, INTENT(OUT)   :: ETRAN   !transpiration rate (mm/s) [+]
  REAL, INTENT(OUT)   :: QRAIN   !rain at ground srf (mm/s) [+]
  REAL, INTENT(OUT)   :: QSNOW   !snow at ground srf (mm/s) [+]
  REAL, INTENT(OUT)   :: SNOWHIN !snow depth increasing rate (m/s)
  REAL, INTENT(OUT)   :: FWET    !wetted or snowed fraction of the canopy (-)
  REAL, INTENT(OUT)   :: FPICE   !snow fraction in precipitation
! --------------------------------------------------------------------

! ------------------------ local variables ---------------------------
  REAL                :: MAXSNO  !canopy capacity for snow interception (mm)
  REAL                :: MAXLIQ  !canopy capacity for rain interception (mm)
  REAL                :: FP      !fraction of the gridcell that receives precipitation
  REAL                :: BDFALL  !bulk density of snowfall (kg/m3)
  REAL                :: QINTR   !interception rate for rain (mm/s)
  REAL                :: QDRIPR  !drip rate for rain (mm/s)
  REAL                :: QTHROR  !throughfall for rain (mm/s)
  REAL                :: QINTS   !interception (loading) rate for snowfall (mm/s)
  REAL                :: QDRIPS  !drip (unloading) rate for intercepted snow (mm/s)
  REAL                :: QTHROS  !throughfall of snowfall (mm/s)
  REAL                :: QEVAC   !evaporation rate (mm/s)
  REAL                :: QDEWC   !dew rate (mm/s)
  REAL                :: QFROC   !frost rate (mm/s)
  REAL                :: QSUBC   !sublimation rate (mm/s)
  REAL                :: FT      !temperature factor for unloading rate
  REAL                :: FV      !wind factor for unloading rate
  REAL                :: QMELTC  !melting rate of canopy snow (mm/s)
  REAL                :: QFRZC   !refreezing rate of canopy liquid water (mm/s)
  REAL                :: RAIN    !rainfall (mm/s)
  REAL                :: SNOW    !snowfall (mm/s)
  REAL                :: CANMAS  !total canopy mass (kg/m2)
! --------------------------------------------------------------------
! initialization

      FP      = 0.0
      RAIN    = 0.0
      SNOW    = 0.0 
      QINTR   = 0.
      QDRIPR  = 0.
      QTHROR  = 0.
      QINTR   = 0.
      QINTS   = 0.
      QDRIPS  = 0.0
      QTHROS  = 0.
      QRAIN   = 0.0
      QSNOW   = 0.0
      SNOWHIN = 0.0
      ECAN    = 0.0
! --------------------------------------------------------------------
! partition precipitation into rain and snow.

! Jordan (1991)

     IF(OPT_SNF == 1) THEN
       IF(SFCTMP > TFRZ+2.5)THEN
           FPICE = 0.
       ELSE
         IF(SFCTMP <= TFRZ+0.5)THEN
           FPICE = 1.0
         ELSE IF(SFCTMP <= TFRZ+2.)THEN
           FPICE = 1.-(-54.632 + 0.2*SFCTMP)
         ELSE
           FPICE = 0.6
         ENDIF
       ENDIF
     ENDIF

     IF(OPT_SNF == 2) THEN
       IF(SFCTMP >= TFRZ+2.2) THEN
           FPICE = 0.
       ELSE
           FPICE = 1.0
       ENDIF
     ENDIF

     IF(OPT_SNF == 3) THEN
       IF(SFCTMP >= TFRZ) THEN
           FPICE = 0.
       ELSE
           FPICE = 1.0
       ENDIF
     ENDIF

! Hedstrom NR and JW Pomeroy (1998), Hydrol. Processes, 12, 1611-1625
! fresh snow density

     BDFALL = MAX(120.,67.92+51.25*EXP((SFCTMP-TFRZ)/2.59))

     RAIN   = (QPRECC + QPRECL) * (1.-FPICE)
     SNOW   = (QPRECC + QPRECL) * FPICE

! fractional area that receives precipitation (see, Niu et al. 2005)
   
    IF(QPRECC + QPRECL > 0.) & 
       FP = (QPRECC + QPRECL) / (10.*QPRECC + QPRECL)

! --------------------------- liquid water ------------------------------
! maximum canopy water

      MAXLIQ =  CH2OP(VEGTYP) * (ELAI+ ESAI)

! average interception and throughfall

      IF((ELAI+ ESAI).GT.0.) THEN
         QINTR  = FVEG * RAIN * FP  ! interception capability
         QINTR  = MIN(QINTR, (MAXLIQ - CANLIQ)/DT * (1.-EXP(-RAIN*DT/MAXLIQ)) )
         QINTR  = MAX(QINTR, 0.)
         QDRIPR = FVEG * RAIN - QINTR
         QTHROR = (1.-FVEG) * RAIN
      ELSE
         QINTR  = 0.
         QDRIPR = 0.
         QTHROR = RAIN
      END IF

! evaporation, transpiration, and dew

      IF (TV .GT. TFRZ) THEN
        ETRAN = MAX( FCTR/HVAP, 0. )
        QEVAC = MAX( FCEV/HVAP, 0. )
        QDEWC = ABS( MIN( FCEV/HVAP, 0. ) )
        QSUBC = 0.
        QFROC = 0.
      ELSE
        ETRAN = MAX( FCTR/HSUB, 0. )
        QEVAC = 0.
        QDEWC = 0.
        QSUBC = MAX( FCEV/HSUB, 0. )
        QFROC = ABS( MIN( FCEV/HSUB, 0. ) )
      ENDIF

! canopy water balance. for convenience allow dew to bring CANLIQ above
! maxh2o or else would have to re-adjust drip

       QEVAC = MIN(CANLIQ/DT,QEVAC)
       CANLIQ=MAX(0.,CANLIQ+(QINTR+QDEWC-QEVAC)*DT)
       IF(CANLIQ <= 1.E-06) CANLIQ = 0.0

! --------------------------- canopy ice ------------------------------
! for canopy ice

      MAXSNO = 6.6*(0.27+46./BDFALL) * (ELAI+ ESAI)

      IF((ELAI+ ESAI).GT.0.) THEN
         QINTS = FVEG * SNOW * FP
         QINTS = MIN(QINTS, (MAXSNO - CANICE)/DT * (1.-EXP(-SNOW*DT/MAXSNO)) )
         QINTS = MAX(QINTS, 0.)
         FT = MAX(0.0,(TV - 270.15) / 1.87E5)
         FV = SQRT(UU*UU + VV*VV) / 1.56E5
         QDRIPS = MAX(0.,CANICE) * (FV+FT)
         QTHROS = (1.0-FVEG) * SNOW + (FVEG * SNOW - QINTS)
      ELSE
         QINTS  = 0.
         QDRIPS = 0.
         QTHROS = SNOW
      ENDIF

      QSUBC = MIN(CANICE/DT,QSUBC) 
      CANICE= MAX(0.,CANICE+(QINTS-QDRIPS)*DT + (QFROC-QSUBC)*DT)
      IF(CANICE.LE.1.E-6) CANICE = 0.
     
! wetted fraction of canopy

      IF(CANICE.GT.0.) THEN
           FWET = MAX(0.,CANICE) / MAX(MAXSNO,1.E-06)
      ELSE
           FWET = MAX(0.,CANLIQ) / MAX(MAXLIQ,1.E-06)
      ENDIF
      FWET = MIN(FWET, 1.) ** 0.667

! phase change

      QMELTC = 0.
      QFRZC = 0.

      IF(CANICE.GT.1.E-6.AND.TV.GT.TFRZ) THEN
         QMELTC = MIN(CANICE/DT,(TV-TFRZ)*CICE*CANICE/DENICE/(DT*HFUS))
         CANICE = MAX(0.,CANICE - QMELTC*DT)
         CANLIQ = MAX(0.,CANLIQ + QMELTC*DT)
         TV     = FWET*TFRZ + (1.-FWET)*TV
      ENDIF

      IF(CANLIQ.GT.1.E-6.AND.TV.LT.TFRZ) THEN
         QFRZC  = MIN(CANLIQ/DT,(TFRZ-TV)*CWAT*CANLIQ/DENH2O/(DT*HFUS))
         CANLIQ = MAX(0.,CANLIQ - QFRZC*DT)
         CANICE = MAX(0.,CANICE + QFRZC*DT)
         TV     = FWET*TFRZ + (1.-FWET)*TV
      ENDIF

! total canopy water

      CMC = CANLIQ + CANICE

! total canopy evaporation

      ECAN = QEVAC + QSUBC - QDEWC - QFROC

! rain or snow on the ground

      QRAIN   = QDRIPR + QTHROR
      QSNOW   = QDRIPS + QTHROS
      SNOWHIN = QSNOW/BDFALL


      IF (IST == 2 .AND. TG > TFRZ) THEN
         QSNOW   = 0.
         SNOWHIN = 0.
      END IF

  END SUBROUTINE CANWATER
! ==================================================================================================
! ----------------------------------------------------------------------
  SUBROUTINE SNOWWATER (NSNOW  ,NSOIL  ,IMELT  ,DT     ,ZSOIL  , & !in
                        SFCTMP ,SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB , & !in
                        QRAIN  ,FICEOLD,ILOC   ,JLOC   ,         & !in
                        ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & !inout
                        SH2O   ,SICE   ,STC    ,ZSNSO  ,DZSNSO , & !inout
                        QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  !out
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  INTEGER,                         INTENT(IN)    :: ILOC   !grid index
  INTEGER,                         INTENT(IN)    :: JLOC   !grid index
  INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers
  INTEGER,                         INTENT(IN)    :: NSOIL  !no. of soil layers
  INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT  !melting state index [0-no melt;1-melt]
  REAL,                            INTENT(IN)    :: DT     !time step (s)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !depth of layer-bottom from soil surface
  REAL,                            INTENT(IN)    :: SFCTMP !surface air temperature [k]
  REAL,                            INTENT(IN)    :: SNOWHIN!snow depth increasing rate (m/s)
  REAL,                            INTENT(IN)    :: QSNOW  !snow at ground srf (mm/s) [+]
  REAL,                            INTENT(IN)    :: QSNFRO !snow surface frost rate[mm/s]
  REAL,                            INTENT(IN)    :: QSNSUB !snow surface sublimation rate[mm/s]
  REAL,                            INTENT(IN)    :: QRAIN  !snow surface rain rate[mm/s]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(IN)    :: FICEOLD!ice fraction at last timestep

! input & output
  INTEGER,                         INTENT(INOUT) :: ISNOW  !actual no. of snow layers
  REAL,                            INTENT(INOUT) :: SNOWH  !snow height [m]
  REAL,                            INTENT(INOUT) :: SNEQV  !snow water eqv. [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid moisture (m3/m3)
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE   !soil ice moisture (m3/m3)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  !depth of snow/soil layer-bottom
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO !snow/soil layer thickness [m]

! output
  REAL,                              INTENT(OUT) :: QSNBOT !melting water out of snow bottom [mm/s]
  REAL,                              INTENT(OUT) :: SNOFLOW!glacier flow [mm]
  REAL,                              INTENT(OUT) :: PONDING1
  REAL,                              INTENT(OUT) :: PONDING2

! local
  INTEGER :: IZ,i
  REAL    :: BDSNOW  !bulk density of snow (kg/m3)
! ----------------------------------------------------------------------
   SNOFLOW = 0.0
   PONDING1 = 0.0
   PONDING2 = 0.0

   CALL SNOWFALL (NSOIL  ,NSNOW  ,DT     ,QSNOW  ,SNOWHIN, & !in
                  SFCTMP ,ILOC   ,JLOC   ,                 & !in
                  ISNOW  ,SNOWH  ,DZSNSO ,STC    ,SNICE  , & !inout
                  SNLIQ  ,SNEQV  )                           !inout

! MB: do each if block separately

   IF(ISNOW < 0) &        ! when multi-layer
   CALL  COMPACT (NSNOW  ,NSOIL  ,DT     ,STC    ,SNICE  , & !in
                  SNLIQ  ,ZSOIL  ,IMELT  ,FICEOLD,ILOC   , JLOC ,& !in
                  ISNOW  ,DZSNSO ,ZSNSO  )                   !inout

   IF(ISNOW < 0) &        !when multi-layer
   CALL  COMBINE (NSNOW  ,NSOIL  ,ILOC   ,JLOC   ,         & !in
                  ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !inout
                  DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & !inout
                  PONDING1       ,PONDING2)                  !out

   IF(ISNOW < 0) &        !when multi-layer
   CALL   DIVIDE (NSNOW  ,NSOIL  ,                         & !in
                  ISNOW  ,STC    ,SNICE  ,SNLIQ  ,DZSNSO )   !inout

   CALL  SNOWH2O (NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & !in 
                  QRAIN  ,ILOC   ,JLOC   ,                 & !in
                  ISNOW  ,DZSNSO ,SNOWH  ,SNEQV  ,SNICE  , & !inout
                  SNLIQ  ,SH2O   ,SICE   ,STC    ,         & !inout
                  QSNBOT ,PONDING1       ,PONDING2)           !out

!set empty snow layers to zero

   do iz = -nsnow+1, isnow
        snice(iz) = 0.
        snliq(iz) = 0.
        stc(iz)   = 0.
        dzsnso(iz)= 0.
        zsnso(iz) = 0.
   enddo

!to obtain equilibrium state of snow in glacier region
       
   IF(SNEQV > 2000.) THEN   ! 2000 mm -> maximum water depth
      BDSNOW      = SNICE(0) / DZSNSO(0)
      SNOFLOW     = (SNEQV - 2000.)
      SNICE(0)    = SNICE(0)  - SNOFLOW 
      DZSNSO(0)   = DZSNSO(0) - SNOFLOW/BDSNOW
      SNOFLOW     = SNOFLOW / DT
   END IF

! sum up snow mass for layered snow

   IF(ISNOW < 0) THEN  ! MB: only do for multi-layer
       SNEQV = 0.
       DO IZ = ISNOW+1,0
             SNEQV = SNEQV + SNICE(IZ) + SNLIQ(IZ)
       ENDDO
   END IF

! Reset ZSNSO and layer thinkness DZSNSO

   DO IZ = ISNOW+1, 0
        DZSNSO(IZ) = -DZSNSO(IZ)
   END DO

   DZSNSO(1) = ZSOIL(1)
   DO IZ = 2,NSOIL
        DZSNSO(IZ) = (ZSOIL(IZ) - ZSOIL(IZ-1))
   END DO

   ZSNSO(ISNOW+1) = DZSNSO(ISNOW+1)
   DO IZ = ISNOW+2 ,NSOIL
       ZSNSO(IZ) = ZSNSO(IZ-1) + DZSNSO(IZ)
   ENDDO

   DO IZ = ISNOW+1 ,NSOIL
       DZSNSO(IZ) = -DZSNSO(IZ)
   END DO

  END SUBROUTINE SNOWWATER
! ==================================================================================================
  SUBROUTINE SNOWFALL (NSOIL  ,NSNOW  ,DT     ,QSNOW  ,SNOWHIN , & !in
                       SFCTMP ,ILOC   ,JLOC   ,                  & !in
                       ISNOW  ,SNOWH  ,DZSNSO ,STC    ,SNICE   , & !inout
                       SNLIQ  ,SNEQV  )                            !inout
! ----------------------------------------------------------------------
! snow depth and density to account for the new snowfall.
! new values of snow depth & density returned.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  INTEGER,                            INTENT(IN) :: ILOC   !grid index
  INTEGER,                            INTENT(IN) :: JLOC   !grid index
  INTEGER,                            INTENT(IN) :: NSOIL  !no. of soil layers
  INTEGER,                            INTENT(IN) :: NSNOW  !maximum no. of snow layers
  REAL,                               INTENT(IN) :: DT     !main time step (s)
  REAL,                               INTENT(IN) :: QSNOW  !snow at ground srf (mm/s) [+]
  REAL,                               INTENT(IN) :: SNOWHIN!snow depth increasing rate (m/s)
  REAL,                               INTENT(IN) :: SFCTMP !surface air temperature [k]

! input and output

  INTEGER,                         INTENT(INOUT) :: ISNOW  !actual no. of snow layers
  REAL,                            INTENT(INOUT) :: SNOWH  !snow depth [m]
  REAL,                            INTENT(INOUT) :: SNEQV  !swow water equivalent [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO !thickness of snow/soil layers (m)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]

! local

  INTEGER :: NEWNODE            ! 0-no new layers, 1-creating new layers
! ----------------------------------------------------------------------
    NEWNODE  = 0

! shallow snow / no layer

    IF(ISNOW == 0 .and. QSNOW > 0.)  THEN
      SNOWH = SNOWH + SNOWHIN * DT
      SNEQV = SNEQV + QSNOW * DT
    END IF

! creating a new layer
 
    IF(ISNOW == 0  .AND. QSNOW>0. .AND. SNOWH >= 0.025) THEN !MB: change limit
!    IF(ISNOW == 0  .AND. QSNOW>0. .AND. SNOWH >= 0.05) THEN
      ISNOW    = -1
      NEWNODE  =  1
      DZSNSO(0)= SNOWH
      SNOWH    = 0.
      STC(0)   = MIN(273.16, SFCTMP)   ! temporary setup
      SNICE(0) = SNEQV
      SNLIQ(0) = 0.
    END IF

! snow with layers

    IF(ISNOW <  0 .AND. NEWNODE == 0 .AND. QSNOW > 0.) then
         SNICE(ISNOW+1)  = SNICE(ISNOW+1)   + QSNOW   * DT
         DZSNSO(ISNOW+1) = DZSNSO(ISNOW+1)  + SNOWHIN * DT
    ENDIF

! ----------------------------------------------------------------------
  END SUBROUTINE SNOWFALL
! ==================================================================================================
  SUBROUTINE COMBINE (NSNOW  ,NSOIL  ,ILOC   ,JLOC   ,         & !in
                      ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !inout
                      DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & !inout
                      PONDING1       ,PONDING2)                  !out
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input

    INTEGER, INTENT(IN)     :: ILOC
    INTEGER, INTENT(IN)     :: JLOC
    INTEGER, INTENT(IN)     :: NSNOW                        !maximum no. of snow layers
    INTEGER, INTENT(IN)     :: NSOIL                        !no. of soil layers

! input and output

    INTEGER,                         INTENT(INOUT) :: ISNOW !actual no. of snow layers
    REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O  !soil liquid moisture (m3/m3)
    REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE  !soil ice moisture (m3/m3)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC   !snow layer temperature [k]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE !snow layer ice [mm]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ !snow layer liquid water [mm]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO!snow layer depth [m]
    REAL,                            INTENT(INOUT) :: sneqv !snow water equivalent [m]
    REAL,                            INTENT(INOUT) :: snowh !snow depth [m]
    REAL,                            INTENT(OUT) :: PONDING1
    REAL,                            INTENT(OUT) :: PONDING2

! local variables:

    INTEGER :: I,J,K,L               ! node indices
    INTEGER :: ISNOW_OLD             ! number of top snow layer
    INTEGER :: MSSI                  ! node index
    INTEGER :: NEIBOR                ! adjacent node selected for combination
    REAL    :: ZWICE                 ! total ice mass in snow
    REAL    :: ZWLIQ                 ! total liquid water in snow

    REAL    :: DZMIN(3)              ! minimum of top snow layer
!    DATA DZMIN /0.045, 0.05, 0.2/
    DATA DZMIN /0.025, 0.025, 0.1/  ! MB: change limit
!-----------------------------------------------------------------------

       ISNOW_OLD = ISNOW

       DO J = ISNOW_OLD+1,0
          IF (SNICE(J) <= .1) THEN
             IF(J /= 0) THEN
                SNLIQ(J+1) = SNLIQ(J+1) + SNLIQ(J)
                SNICE(J+1) = SNICE(J+1) + SNICE(J)
             ELSE
               IF (ISNOW_OLD < -1) THEN    ! MB/KM: change to ISNOW
                SNLIQ(J-1) = SNLIQ(J-1) + SNLIQ(J)
                SNICE(J-1) = SNICE(J-1) + SNICE(J)
               ELSE
                PONDING1 = SNLIQ(J)       ! ISNOW WILL GET SET TO ZERO BELOW
                SNEQV = SNICE(J)          ! PONDING1 WILL GET ADDED TO PONDING FROM
                SNOWH = DZSNSO(J)         ! PHASECHANGE WHICH SHOULD BE ZERO HERE
                SNLIQ(J) = 0.0            ! BECAUSE THERE IT WAS ONLY CALCULATED
                SNICE(J) = 0.0            ! FOR THIN SNOW
                DZSNSO(J) = 0.0
               ENDIF
!                SH2O(1) = SH2O(1)+SNLIQ(J)/(DZSNSO(1)*1000.)
!                SICE(1) = SICE(1)+SNICE(J)/(DZSNSO(1)*1000.)
             ENDIF

             ! shift all elements above this down by one.
             IF (J > ISNOW+1 .AND. ISNOW < -1) THEN
                DO I = J, ISNOW+2, -1
                   STC(I)   = STC(I-1)
                   SNLIQ(I) = SNLIQ(I-1)
                   SNICE(I) = SNICE(I-1)
                   DZSNSO(I)= DZSNSO(I-1)
                END DO
             END IF
             ISNOW = ISNOW + 1
          END IF
       END DO

! to conserve water in case of too large surface sublimation

       IF(SICE(1) < 0.) THEN
          SH2O(1) = SH2O(1) + SICE(1)
          SICE(1) = 0.
       END IF

       IF(ISNOW ==0) RETURN   ! MB: get out if no longer multi-layer

       SNEQV  = 0.
       SNOWH  = 0.
       ZWICE  = 0.
       ZWLIQ  = 0.

       DO J = ISNOW+1,0
             SNEQV = SNEQV + SNICE(J) + SNLIQ(J)
             SNOWH = SNOWH + DZSNSO(J)
             ZWICE = ZWICE + SNICE(J)
             ZWLIQ = ZWLIQ + SNLIQ(J)
       END DO

! check the snow depth - all snow gone
! the liquid water assumes ponding on soil surface.

       IF (SNOWH < 0.025 .AND. ISNOW < 0 ) THEN ! MB: change limit
!       IF (SNOWH < 0.05 .AND. ISNOW < 0 ) THEN
          ISNOW  = 0
          SNEQV = ZWICE
          PONDING2 = ZWLIQ           ! LIMIT OF ISNOW < 0 MEANS INPUT PONDING
          IF(SNEQV <= 0.) SNOWH = 0. ! SHOULD BE ZERO; SEE ABOVE
       END IF

!       IF (SNOWH < 0.05 ) THEN
!          ISNOW  = 0
!          SNEQV = ZWICE
!          SH2O(1) = SH2O(1) + ZWLIQ / (DZSNSO(1) * 1000.)
!          IF(SNEQV <= 0.) SNOWH = 0.
!       END IF

! check the snow depth - snow layers combined

       IF (ISNOW < -1) THEN

          ISNOW_OLD = ISNOW
          MSSI     = 1

          DO I = ISNOW_OLD+1,0
             IF (DZSNSO(I) < DZMIN(MSSI)) THEN

                IF (I == ISNOW+1) THEN
                   NEIBOR = I + 1
                ELSE IF (I == 0) THEN
                   NEIBOR = I - 1
                ELSE
                   NEIBOR = I + 1
                   IF ((DZSNSO(I-1)+DZSNSO(I)) < (DZSNSO(I+1)+DZSNSO(I))) NEIBOR = I-1
                END IF

                ! Node l and j are combined and stored as node j.
                IF (NEIBOR > I) THEN
                   J = NEIBOR
                   L = I
                ELSE
                   J = I
                   L = NEIBOR
                END IF

                CALL COMBO (DZSNSO(J), SNLIQ(J), SNICE(J), &
                   STC(J), DZSNSO(L), SNLIQ(L), SNICE(L), STC(L) )

                ! Now shift all elements above this down one.
                IF (J-1 > ISNOW+1) THEN
                   DO K = J-1, ISNOW+2, -1
                      STC(K)   = STC(K-1)
                      SNICE(K) = SNICE(K-1)
                      SNLIQ(K) = SNLIQ(K-1)
                      DZSNSO(K) = DZSNSO(K-1)
                   END DO
                END IF

                ! Decrease the number of snow layers
                ISNOW = ISNOW + 1
                IF (ISNOW >= -1) EXIT
             ELSE

                ! The layer thickness is greater than the prescribed minimum value
                MSSI = MSSI + 1

             END IF
          END DO

       END IF

  END SUBROUTINE COMBINE
! ==================================================================================================
  SUBROUTINE DIVIDE (NSNOW  ,NSOIL  ,                         & !in
                     ISNOW  ,STC    ,SNICE  ,SNLIQ  ,DZSNSO  )  !inout
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input

    INTEGER, INTENT(IN)                            :: NSNOW !maximum no. of snow layers [ =3]
    INTEGER, INTENT(IN)                            :: NSOIL !no. of soil layers [ =4]

! input and output

    INTEGER                        , INTENT(INOUT) :: ISNOW !actual no. of snow layers 
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC   !snow layer temperature [k]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE !snow layer ice [mm]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ !snow layer liquid water [mm]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO!snow layer depth [m]

! local variables:

    INTEGER                                        :: J     !indices
    INTEGER                                        :: MSNO  !number of layer (top) to MSNO (bot)
    REAL                                           :: DRR   !thickness of the combined [m]
    REAL, DIMENSION(       1:NSNOW)                :: DZ    !snow layer thickness [m]
    REAL, DIMENSION(       1:NSNOW)                :: SWICE !partial volume of ice [m3/m3]
    REAL, DIMENSION(       1:NSNOW)                :: SWLIQ !partial volume of liquid water [m3/m3]
    REAL, DIMENSION(       1:NSNOW)                :: TSNO  !node temperature [k]
    REAL                                           :: ZWICE !temporary
    REAL                                           :: ZWLIQ !temporary
    REAL                                           :: PROPOR!temporary
    REAL                                           :: DTDZ  !temporary
! ----------------------------------------------------------------------

    DO J = 1,NSNOW
          IF (J <= ABS(ISNOW)) THEN
             DZ(J)    = DZSNSO(J+ISNOW)
             SWICE(J) = SNICE(J+ISNOW)
             SWLIQ(J) = SNLIQ(J+ISNOW)
             TSNO(J)  = STC(J+ISNOW)
          END IF
    END DO

       MSNO = ABS(ISNOW)

       IF (MSNO == 1) THEN
          ! Specify a new snow layer
          IF (DZ(1) > 0.05) THEN
             MSNO = 2
             DZ(1)    = DZ(1)/2.
             SWICE(1) = SWICE(1)/2.
             SWLIQ(1) = SWLIQ(1)/2.
             DZ(2)    = DZ(1)
             SWICE(2) = SWICE(1)
             SWLIQ(2) = SWLIQ(1)
             TSNO(2)  = TSNO(1)
          END IF
       END IF

       IF (MSNO > 1) THEN
          IF (DZ(1) > 0.05) THEN
             DRR      = DZ(1) - 0.05
             PROPOR   = DRR/DZ(1)
             ZWICE    = PROPOR*SWICE(1)
             ZWLIQ    = PROPOR*SWLIQ(1)
             PROPOR   = 0.05/DZ(1)
             SWICE(1) = PROPOR*SWICE(1)
             SWLIQ(1) = PROPOR*SWLIQ(1)
             DZ(1)    = 0.05

             CALL COMBO (DZ(2), SWLIQ(2), SWICE(2), TSNO(2), DRR, &
                  ZWLIQ, ZWICE, TSNO(1))

             ! subdivide a new layer
             IF (MSNO <= 2 .AND. DZ(2) > 0.20) THEN  ! MB: change limit
!             IF (MSNO <= 2 .AND. DZ(2) > 0.10) THEN
                MSNO = 3
                DTDZ = (TSNO(1) - TSNO(2))/((DZ(1)+DZ(2))/2.)
                DZ(2)    = DZ(2)/2.
                SWICE(2) = SWICE(2)/2.
                SWLIQ(2) = SWLIQ(2)/2.
                DZ(3)    = DZ(2)
                SWICE(3) = SWICE(2)
                SWLIQ(3) = SWLIQ(2)
                TSNO(3) = TSNO(2) - DTDZ*DZ(2)/2.
                IF (TSNO(3) >= TFRZ) THEN
                   TSNO(3)  = TSNO(2)
                ELSE
                   TSNO(2) = TSNO(2) + DTDZ*DZ(2)/2.
                ENDIF

             END IF
          END IF
       END IF

       IF (MSNO > 2) THEN
          IF (DZ(2) > 0.2) THEN
             DRR = DZ(2) - 0.2
             PROPOR   = DRR/DZ(2)
             ZWICE    = PROPOR*SWICE(2)
             ZWLIQ    = PROPOR*SWLIQ(2)
             PROPOR   = 0.2/DZ(2)
             SWICE(2) = PROPOR*SWICE(2)
             SWLIQ(2) = PROPOR*SWLIQ(2)
             DZ(2)    = 0.2
             CALL COMBO (DZ(3), SWLIQ(3), SWICE(3), TSNO(3), DRR, &
                  ZWLIQ, ZWICE, TSNO(2))
          END IF
       END IF

       ISNOW = -MSNO

    DO J = ISNOW+1,0
             DZSNSO(J) = DZ(J-ISNOW)
             SNICE(J) = SWICE(J-ISNOW)
             SNLIQ(J) = SWLIQ(J-ISNOW)
             STC(J)   = TSNO(J-ISNOW)
    END DO


!    DO J = ISNOW+1,NSOIL
!    WRITE(*,'(I5,7F10.3)') J, DZSNSO(J), SNICE(J), SNLIQ(J),STC(J)
!    END DO

  END SUBROUTINE DIVIDE
! ==================================================================================================

! ----------------------------------------------------------------------
  SUBROUTINE COMBO(DZ,  WLIQ,  WICE, T, DZ2, WLIQ2, WICE2, T2)
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------s
! input

    REAL, INTENT(IN)    :: DZ2   !nodal thickness of 2 elements being combined [m]
    REAL, INTENT(IN)    :: WLIQ2 !liquid water of element 2 [kg/m2]
    REAL, INTENT(IN)    :: WICE2 !ice of element 2 [kg/m2]
    REAL, INTENT(IN)    :: T2    !nodal temperature of element 2 [k]
    REAL, INTENT(INOUT) :: DZ    !nodal thickness of 1 elements being combined [m]
    REAL, INTENT(INOUT) :: WLIQ  !liquid water of element 1
    REAL, INTENT(INOUT) :: WICE  !ice of element 1 [kg/m2]
    REAL, INTENT(INOUT) :: T     !node temperature of element 1 [k]

! local 

    REAL                :: DZC   !total thickness of nodes 1 and 2 (DZC=DZ+DZ2).
    REAL                :: WLIQC !combined liquid water [kg/m2]
    REAL                :: WICEC !combined ice [kg/m2]
    REAL                :: TC    !combined node temperature [k]
    REAL                :: H     !enthalpy of element 1 [J/m2]
    REAL                :: H2    !enthalpy of element 2 [J/m2]
    REAL                :: HC    !temporary

!-----------------------------------------------------------------------

    DZC = DZ+DZ2
    WICEC = (WICE+WICE2)
    WLIQC = (WLIQ+WLIQ2)
    H = (CICE*WICE+CWAT*WLIQ) * (T-TFRZ)+HFUS*WLIQ
    H2= (CICE*WICE2+CWAT*WLIQ2) * (T2-TFRZ)+HFUS*WLIQ2

    HC = H + H2
    IF(HC < 0.)THEN
       TC = TFRZ + HC/(CICE*WICEC + CWAT*WLIQC)
    ELSE IF (HC.LE.HFUS*WLIQC) THEN
       TC = TFRZ
    ELSE
       TC = TFRZ + (HC - HFUS*WLIQC) / (CICE*WICEC + CWAT*WLIQC)
    END IF

    DZ = DZC
    WICE = WICEC
    WLIQ = WLIQC
    T = TC

  END SUBROUTINE COMBO
! ==================================================================================================
! ----------------------------------------------------------------------
  SUBROUTINE COMPACT (NSNOW  ,NSOIL  ,DT     ,STC    ,SNICE  , & !in
                      SNLIQ  ,ZSOIL  ,IMELT  ,FICEOLD,ILOC   , JLOC , & !in
                      ISNOW  ,DZSNSO ,ZSNSO )                    !inout
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
   INTEGER,                         INTENT(IN)    :: ILOC   !grid index
   INTEGER,                         INTENT(IN)    :: JLOC   !grid index
   INTEGER,                         INTENT(IN)    :: NSOIL  !no. of soil layers [ =4]
   INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers [ =3]
   INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT  !melting state index [0-no melt;1-melt]
   REAL,                            INTENT(IN)    :: DT     !time step (sec)
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: STC    !snow layer temperature [k]
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNICE  !snow layer ice [mm]
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNLIQ  !snow layer liquid water [mm]
   REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !depth of layer-bottom from soil srf
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD!ice fraction at last timestep

! input and output
   INTEGER,                         INTENT(INOUT) :: ISNOW  ! actual no. of snow layers
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO ! snow layer thickness [m]
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  ! depth of snow/soil layer-bottom

! local
   REAL, PARAMETER     :: C2 = 21.e-3   ![m3/kg] ! default 21.e-3
   REAL, PARAMETER     :: C3 = 2.5e-6   ![1/s]  
   REAL, PARAMETER     :: C4 = 0.04     ![1/k]
   REAL, PARAMETER     :: C5 = 2.0      !
   REAL, PARAMETER     :: DM = 100.0    !upper Limit on destructive metamorphism compaction [kg/m3]
   REAL, PARAMETER     :: ETA0 = 0.8e+6 !viscosity coefficient [kg-s/m2] 
                                        !according to Anderson, it is between 0.52e6~1.38e6
   REAL :: BURDEN !pressure of overlying snow [kg/m2]
   REAL :: DDZ1   !rate of settling of snow pack due to destructive metamorphism.
   REAL :: DDZ2   !rate of compaction of snow pack due to overburden.
   REAL :: DDZ3   !rate of compaction of snow pack due to melt [1/s]
   REAL :: DEXPF  !EXPF=exp(-c4*(273.15-STC)).
   REAL :: TD     !STC - TFRZ [K]
   REAL :: PDZDTC !nodal rate of change in fractional-thickness due to compaction [fraction/s]
   REAL :: VOID   !void (1 - SNICE - SNLIQ)
   REAL :: WX     !water mass (ice + liquid) [kg/m2]
   REAL :: BI     !partial density of ice [kg/m3]
   REAL, DIMENSION(-NSNOW+1:0) :: FICE   !fraction of ice at current time step

   INTEGER  :: J

! ----------------------------------------------------------------------
    BURDEN = 0.0

    DO J = ISNOW+1, 0

        WX      = SNICE(J) + SNLIQ(J)
        FICE(J) = SNICE(J) / WX
        VOID    = 1. - (SNICE(J)/DENICE + SNLIQ(J)/DENH2O) / DZSNSO(J)

        ! Allow compaction only for non-saturated node and higher ice lens node.
        IF (VOID > 0.001 .AND. SNICE(J) > 0.1) THEN
           BI = SNICE(J) / DZSNSO(J)
           TD = MAX(0.,TFRZ-STC(J))
           DEXPF = EXP(-C4*TD)

           ! Settling as a result of destructive metamorphism

           DDZ1 = -C3*DEXPF

           IF (BI > DM) DDZ1 = DDZ1*EXP(-46.0E-3*(BI-DM))

           ! Liquid water term

           IF (SNLIQ(J) > 0.01*DZSNSO(J)) DDZ1=DDZ1*C5

           ! Compaction due to overburden

           DDZ2 = -(BURDEN+0.5*WX)*EXP(-0.08*TD-C2*BI)/ETA0 ! 0.5*WX -> self-burden

           ! Compaction occurring during melt

           IF (IMELT(J) == 1) THEN
              DDZ3 = MAX(0.,(FICEOLD(J) - FICE(J))/MAX(1.E-6,FICEOLD(J)))
              DDZ3 = - DDZ3/DT           ! sometimes too large
           ELSE
              DDZ3 = 0.
           END IF

           ! Time rate of fractional change in DZ (units of s-1)

           PDZDTC = (DDZ1 + DDZ2 + DDZ3)*DT
           PDZDTC = MAX(-0.5,PDZDTC)

           ! The change in DZ due to compaction

           DZSNSO(J) = DZSNSO(J)*(1.+PDZDTC)
        END IF

        ! Pressure of overlying snow

        BURDEN = BURDEN + WX

    END DO

  END SUBROUTINE COMPACT
! ==================================================================================================
  SUBROUTINE SNOWH2O (NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & !in 
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
! ----------------------------------------------------------------------

!for the case when SNEQV becomes '0' after 'COMBINE'

   IF(SNEQV == 0.) THEN
      SH2O(1) =  SH2O(1) + (QSNFRO-QSNSUB)*DT/(DZSNSO(1)*1000.)
   END IF

! for shallow snow without a layer
! snow surface sublimation may be larger than existing snow mass. To conserve water,
! excessive sublimation is used to reduce soil water. Smaller time steps would tend 
! to aviod this problem.

   IF(ISNOW == 0 .and. SNEQV > 0.) THEN
      TEMP   = SNEQV
      SNEQV  = SNEQV - QSNSUB*DT + QSNFRO*DT
      PROPOR = SNEQV/TEMP
      SNOWH  = MAX(0.,PROPOR * SNOWH)

      IF(SNEQV < 0.) THEN
         SICE(1) = SICE(1) + SNEQV/(DZSNSO(1)*1000.)
         SNEQV   = 0.
      END IF
      IF(SICE(1) < 0.) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.
      END IF
   END IF

   IF(SNOWH <= 1.E-8) SNOWH = 0.0
   IF(SNEQV <= 1.E-6) SNEQV = 0.0

! for deep snow

   IF ( ISNOW < 0 ) THEN !KWM added this IF statement to prevent out-of-bounds array references

      WGDIF = SNICE(ISNOW+1) - QSNSUB*DT + QSNFRO*DT
      SNICE(ISNOW+1) = WGDIF
      IF (WGDIF < 1.e-6 .and. ISNOW <0) THEN
         CALL  COMBINE (NSNOW  ,NSOIL  ,ILOC, JLOC   , & !in
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

   !KWM Looks to me like loop index / IF test can be simplified.

   DO J = -NSNOW+1, 0
      IF (J >= ISNOW+1) THEN
         VOL_ICE(J)      = MIN(1., SNICE(J)/(DZSNSO(J)*DENICE))
         EPORE(J)        = 1. - VOL_ICE(J)
         VOL_LIQ(J)      = MIN(EPORE(J),SNLIQ(J)/(DZSNSO(J)*DENH2O))
      END IF
   END DO

   QIN = 0.
   QOUT = 0.

   !KWM Looks to me like loop index / IF test can be simplified.

   DO J = -NSNOW+1, 0
      IF (J >= ISNOW+1) THEN
         SNLIQ(J) = SNLIQ(J) + QIN
         IF (J <= -1) THEN
            IF (EPORE(J) < 0.05 .OR. EPORE(J+1) < 0.05) THEN
               QOUT = 0.
            ELSE
               QOUT = MAX(0.,(VOL_LIQ(J)-SSI*EPORE(J))*DZSNSO(J))
               QOUT = MIN(QOUT,(1.-VOL_ICE(J+1)-VOL_LIQ(J+1))*DZSNSO(J+1))
            END IF
         ELSE
            QOUT = MAX(0.,(VOL_LIQ(J) - SSI*EPORE(J))*DZSNSO(J))
         END IF
         QOUT = QOUT*1000.
         SNLIQ(J) = SNLIQ(J) - QOUT
         QIN = QOUT
      END IF
   END DO

! Liquid water from snow bottom to soil

   QSNBOT = QOUT / DT           ! mm/s

  END SUBROUTINE SNOWH2O
! ==================================================================================================
  SUBROUTINE SOILWATER (NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & !in
                        QINSUR ,QSEVA  ,ETRANI ,SICE   ,ILOC   , JLOC, & !in
                        SH2O   ,SMC    ,ZWT    ,ISURBAN,VEGTYP ,& !inout
                        RUNSRF ,QDRAIN ,RUNSUB ,WCND   ,FCRMAX )   !out

! ----------------------------------------------------------------------
! calculate surface runoff and soil moisture.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
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
  INTEGER,                     INTENT(IN) :: ISURBAN

! input & output
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid water content [m3/m3]
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC    !total soil water content [m3/m3]
  REAL, INTENT(INOUT)                     :: ZWT    !water table depth [m]

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
       EPORE   = MAX ( 1.E-4 , ( SMCMAX - SICE(K) ) )
       RSAT    = RSAT + MAX(0.,SH2O(K)-EPORE)*DZSNSO(K)  
       SH2O(K) = MIN(EPORE,SH2O(K))             
    END DO

!impermeable fraction due to frozen soil

    DO K = 1,NSOIL
       FICE    = MIN(1.0,SICE(K)/SMCMAX)
       FCR(K)  = MAX(0.0,EXP(-A*(1.-FICE))- EXP(-A)) /  &
                        (1.0              - EXP(-A))
    END DO

! maximum soil ice content and minimum liquid water of all layers

    SICEMAX = 0.0
    FCRMAX  = 0.0
    SH2OMIN = SMCMAX
    DO K = 1,NSOIL
       IF (SICE(K) > SICEMAX) SICEMAX = SICE(K)
       IF (FCR(K)  > FCRMAX)  FCRMAX  = FCR(K)
       IF (SH2O(K) < SH2OMIN) SH2OMIN = SH2O(K)
    END DO

!subsurface runoff for runoff scheme option 2

    IF(OPT_RUN == 2) THEN 
        FFF   = 2.0
        RSBMX = 4.0
        CALL ZWTEQ (NSOIL  ,NSNOW  ,ZSOIL  ,DZSNSO ,SH2O   ,ZWT)
        RUNSUB = (1.0-FCRMAX) * RSBMX * EXP(-TIMEAN) * EXP(-FFF*ZWT)   ! mm/s
    END IF

!surface runoff and infiltration rate using different schemes

!jref impermable surface at urban
    IF ( VEGTYP == ISURBAN ) FCR(1)= 0.95

    IF(OPT_RUN == 1) THEN
       FFF = 6.0
       FSAT   = FSATMX*EXP(-0.5*FFF*(ZWT-2.0))
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s 
       END IF
    END IF

    IF(OPT_RUN == 2) THEN
       FFF   = 2.0
       FSAT   = FSATMX*EXP(-0.5*FFF*ZWT)
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s 
       END IF
    END IF

    IF(OPT_RUN == 3) THEN
       CALL INFIL (NSOIL  ,DT     ,ZSOIL  ,SH2O   ,SICE   , & !in
                   SICEMAX,QINSUR ,                         & !in
                   PDDUM  ,RUNSRF )                           !out
    END IF

    IF(OPT_RUN == 4) THEN
       SMCTOT = 0.
       DZTOT  = 0.
       DO K = 1,NSOIL
          DZTOT   = DZTOT  + DZSNSO(K)  
          SMCTOT  = SMCTOT + SMC(K)*DZSNSO(K)
          IF(DZTOT >= 2.0) EXIT
       END DO
       SMCTOT = SMCTOT/DZTOT
       FSAT   = MAX(0.01,SMCTOT/SMCMAX) ** 4.        !BATS

       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ((1.0-FCR(1))*FSAT+FCR(1))  
         PDDUM  = QINSUR - RUNSRF                       ! m/s
       END IF
    END IF

! determine iteration times and finer time step

    NITER = 1

    IF(OPT_INF == 1) THEN    !OPT_INF =2 may cause water imbalance
       NITER = 3
       IF (PDDUM*DT>DZSNSO(1)*SMCMAX ) THEN
          NITER = NITER*2
       END IF
    END IF                 

    DTFINE  = DT / NITER

! solve soil moisture

    DO ITER = 1, NITER
       CALL SRT   (NSOIL  ,ZSOIL  ,DTFINE ,PDDUM  ,ETRANI , & !in
                   QSEVA  ,SH2O   ,SMC    ,ZWT    ,FCR    , & !in
                   SICEMAX,FCRMAX ,ILOC   ,JLOC   ,         & !in
                   RHSTT  ,AI     ,BI     ,CI     ,QDRAIN , & !out
                   WCND   )                                   !out
  
       CALL SSTEP (NSOIL  ,NSNOW  ,DTFINE ,ZSOIL  ,DZSNSO , & !in
                   SICE   ,ILOC   ,JLOC   ,                 & !in
                   SH2O   ,SMC    ,AI     ,BI     ,CI     , & !inout
                   RHSTT  ,                                 & !inout
                   WPLUS)                                     !out
       RSAT =  RSAT + WPLUS
    END DO

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

      DO IZ = 1, NSOIL
        SH2O(IZ)     = MLIQ(IZ) / (DZSNSO(IZ)*1000.)
      END DO
   END IF

  END SUBROUTINE SOILWATER
! ==================================================================================================
  SUBROUTINE ZWTEQ (NSOIL  ,NSNOW  ,ZSOIL  ,DZSNSO ,SH2O   ,ZWT)
! ----------------------------------------------------------------------
! calculate equilibrium water table depth (Niu et al., 2005)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input

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
     WD1 = WD1 + (SMCMAX-SH2O(K)) * DZSNSO(K) ! [m]
   ENDDO

   DZFINE = 3.0 * (-ZSOIL(NSOIL)) / NFINE  
   do K =1,NFINE
      ZFINE(K) = FLOAT(K) * DZFINE
   ENDDO

   ZWT = -3.*ZSOIL(NSOIL) - 0.001   ! initial value [m]

   WD2 = 0.
   DO K = 1,NFINE
     TEMP  = 1. + (ZWT-ZFINE(K))/PSISAT
     WD2   = WD2 + SMCMAX*(1.-TEMP**(-1./BEXP))*DZFINE
     IF(ABS(WD2-WD1).LE.0.01) THEN
        ZWT = ZFINE(K)
        EXIT
     ENDIF
   ENDDO

  END SUBROUTINE ZWTEQ
! ----------------------------------------------------------------------
! ==================================================================================================
  SUBROUTINE INFIL (NSOIL  ,DT     ,ZSOIL  ,SH2O   ,SICE   , & !in
                    SICEMAX,QINSUR ,                         & !in
                    PDDUM  ,RUNSRF )                           !out
! --------------------------------------------------------------------------------
! compute inflitration rate at soil surface and surface runoff
! --------------------------------------------------------------------------------
    IMPLICIT NONE
! --------------------------------------------------------------------------------
! inputs
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
       SMCAV = SMCMAX - SMCWLT

! maximum infiltration rate

       DMAX(1)= -ZSOIL(1) * SMCAV
       DICE   = -ZSOIL(1) * SICE(1)
       DMAX(1)= DMAX(1)* (1.0-(SH2O(1) + SICE(1) - SMCWLT)/SMCAV)

       DD = DMAX(1)

       DO K = 2,NSOIL
          DICE    = DICE + (ZSOIL(K-1) - ZSOIL(K) ) * SICE(K)
          DMAX(K) = (ZSOIL(K-1) - ZSOIL(K)) * SMCAV
          DMAX(K) = DMAX(K) * (1.0-(SH2O(K) + SICE(K) - SMCWLT)/SMCAV)
          DD      = DD + DMAX(K)
       END DO

       VAL = (1. - EXP ( - KDT * DT1))
       DDT = DD * VAL
       PX  = MAX(0.,QINSUR * DT)
       INFMAX = (PX * (DDT / (PX + DDT)))/ DT

! impermeable fraction due to frozen soil

       FCR = 1.
       IF (DICE >  1.E-2) THEN
          ACRT = CVFRZ * FRZX / DICE
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
!       IF (VEGTYP == ISURBAN ) INFMAX == INFMAX * 0.05

       CALL WDFCND2 (WDF,WCND,SH2O(1),SICEMAX)
       INFMAX = MAX (INFMAX,WCND)
       INFMAX = MIN (INFMAX,PX)

       RUNSRF= MAX(0., QINSUR - INFMAX)
       PDDUM = QINSUR - RUNSRF

    END IF

  END SUBROUTINE INFIL
! ==================================================================================================
  SUBROUTINE SRT (NSOIL  ,ZSOIL  ,DT     ,PDDUM  ,ETRANI , & !in
                  QSEVA  ,SH2O   ,SMC    ,ZWT    ,FCR    , & !in
                  SICEMAX,FCRMAX ,ILOC   ,JLOC   ,         & !in
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

! Niu and Yang (2006), J. of Hydrometeorology
! ----------------------------------------------------------------------

    IF(OPT_INF == 1) THEN
      DO K = 1, NSOIL
        CALL WDFCND1 (WDF(K),WCND(K),SMC(K),FCR(K))
        SMX(K) = SMC(K)
      END DO
    END IF

    IF(OPT_INF == 2) THEN
      DO K = 1, NSOIL
        CALL WDFCND2 (WDF(K),WCND(K),SH2O(K),SICEMAX)
        SMX(K) = SH2O(K)
      END DO
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
             QDRAIN   = SLOPE*WCND(K)
          END IF
          IF(OPT_RUN == 4) THEN
             QDRAIN   = (1.0-FCRMAX)*WCND(K)
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
! ----------------------------------------------------------------------
! ==================================================================================================
  SUBROUTINE SSTEP (NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & !in
                    SICE   ,ILOC   ,JLOC   ,                 & !in
                    SH2O   ,SMC    ,AI     ,BI     ,CI     , & !inout
                    RHSTT  ,                                 & !inout
                    WPLUS  )                                   !out

! ----------------------------------------------------------------------
! calculate/update soil moisture content values 
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
!input

    INTEGER,                         INTENT(IN) :: ILOC   !grid index
    INTEGER,                         INTENT(IN) :: JLOC   !grid index
    INTEGER,                         INTENT(IN) :: NSOIL  !
    INTEGER,                         INTENT(IN) :: NSNOW  !
    REAL, INTENT(IN)                            :: DT
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

!output
    REAL, INTENT(OUT)                       :: WPLUS     !saturation excess water (m)

!local
    INTEGER                                 :: K
    REAL, DIMENSION(1:NSOIL)                :: RHSTTIN
    REAL, DIMENSION(1:NSOIL)                :: CIIN
    REAL                                    :: STOT
    REAL                                    :: EPORE
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

    DO K = NSOIL,2,-1
      EPORE        = MAX ( 1.E-4 , ( SMCMAX - SICE(K) ) )
      WPLUS        = MAX((SH2O(K)-EPORE), 0.0) * DZSNSO(K)
      SH2O(K)      = MIN(EPORE,SH2O(K))
      SH2O(K-1)    = SH2O(K-1) + WPLUS/DZSNSO(K-1)
    END DO

    EPORE        = MAX ( 1.E-4 , ( SMCMAX - SICE(1) ) )
    WPLUS        = MAX((SH2O(1)-EPORE), 0.0) * DZSNSO(1) 
    SH2O(1)      = MIN(EPORE,SH2O(1))

  END SUBROUTINE SSTEP
! ==================================================================================================
  SUBROUTINE WDFCND1 (WDF,WCND,SMC,FCR)
! ----------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input 
    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: FCR

! output
    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF

! local
    REAL :: EXPON
    REAL :: FACTR
    REAL :: VKWGT
! ----------------------------------------------------------------------

! soil water diffusivity

    FACTR = MAX(0.01, SMC/SMCMAX)
    EXPON = BEXP + 2.0
    WDF   = DWSAT * FACTR ** EXPON
    WDF   = WDF * (1.0 - FCR)

! hydraulic conductivity

    EXPON = 2.0*BEXP + 3.0
    WCND  = DKSAT * FACTR ** EXPON
    WCND  = WCND * (1.0 - FCR)

  END SUBROUTINE WDFCND1
! ==================================================================================================
  SUBROUTINE WDFCND2 (WDF,WCND,SMC,SICE)
! ----------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input
    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: SICE

! output
    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF

! local
    REAL :: EXPON
    REAL :: FACTR
    REAL :: VKWGT
! ----------------------------------------------------------------------

! soil water diffusivity

    FACTR = MAX(0.01, SMC/SMCMAX)
    EXPON = BEXP + 2.0
    WDF   = DWSAT * FACTR ** EXPON

    IF (SICE > 0.0) THEN
    VKWGT = 1./ (1. + (500.* SICE)**3.)
    WDF   = VKWGT * WDF + (1.-VKWGT)*DWSAT*(0.2/SMCMAX)**EXPON
    END IF

! hydraulic conductivity

    EXPON = 2.0*BEXP + 3.0
    WCND  = DKSAT * FACTR ** EXPON

  END SUBROUTINE WDFCND2
! ==================================================================================================
! ----------------------------------------------------------------------
  SUBROUTINE GROUNDWATER(NSNOW  ,NSOIL  ,DT     ,SICE   ,ZSOIL  , & !in
                         STC    ,WCND   ,FCRMAX ,ILOC   ,JLOC   , & !in
                         SH2O   ,ZWT    ,WA     ,WT     ,         & !inout
                         QIN    ,QDIS   )                           !out
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  INTEGER,                         INTENT(IN) :: ILOC  !grid index
  INTEGER,                         INTENT(IN) :: JLOC  !grid index
  INTEGER,                         INTENT(IN) :: NSNOW !maximum no. of snow layers
  INTEGER,                         INTENT(IN) :: NSOIL !no. of soil layers
  REAL,                            INTENT(IN) :: DT    !timestep [sec]
  REAL,                            INTENT(IN) :: FCRMAX!maximum FCR (-)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICE  !soil ice content [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL !depth of soil layer-bottom [m]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: WCND  !hydraulic conductivity (m/s)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC   !snow/soil temperature (k)

! input and output
  REAL, DIMENSION(    1:NSOIL), INTENT(INOUT) :: SH2O  !liquid soil water [m3/m3]
  REAL,                         INTENT(INOUT) :: ZWT   !the depth to water table [m]
  REAL,                         INTENT(INOUT) :: WA    !water storage in aquifer [mm]
  REAL,                         INTENT(INOUT) :: WT    !water storage in aquifer 
                                                           !+ saturated soil [mm]
! output
  REAL,                           INTENT(OUT) :: QIN   !groundwater recharge [mm/s]
  REAL,                           INTENT(OUT) :: QDIS  !groundwater discharge [mm/s]

! local
  REAL                                        :: FFF   !runoff decay factor (m-1)
  REAL                                        :: RSBMX !baseflow coefficient [mm/s]
  INTEGER                                     :: IZ    !do-loop index
  INTEGER                                     :: IWT   !layer index above water table layer
  REAL,  DIMENSION(    1:NSOIL)               :: DZMM  !layer thickness [mm]
  REAL,  DIMENSION(    1:NSOIL)               :: ZNODE !node depth [m]
  REAL,  DIMENSION(    1:NSOIL)               :: MLIQ  !liquid water mass [kg/m2 or mm]
  REAL,  DIMENSION(    1:NSOIL)               :: EPORE !effective porosity [-]
  REAL,  DIMENSION(    1:NSOIL)               :: HK    !hydraulic conductivity [mm/s]
  REAL,  DIMENSION(    1:NSOIL)               :: SMC   !total soil water  content [m3/m3]
  REAL(KIND=8)                                :: S_NODE!degree of saturation of IWT layer
  REAL                                        :: DZSUM !cumulative depth above water table [m]
  REAL                                        :: SMPFZ !matric potential (frozen effects) [mm]
  REAL                                        :: KA    !aquifer hydraulic conductivity [mm/s]
  REAL                                        :: WH_ZWT!water head at water table [mm]
  REAL                                        :: WH    !water head at layer above ZWT [mm]
  REAL                                        :: WS    !water used to fill air pore [mm]
  REAL                                        :: WTSUB !sum of HK*DZMM
  REAL                                        :: WATMIN!minimum soil vol soil moisture [m3/m3]
  REAL                                        :: XS    !excessive water above saturation [mm]
  REAL, PARAMETER                             :: ROUS = 0.2    !specific yield [-]
  REAL, PARAMETER                             :: CMIC = 0.20   !microprore content (0.0-1.0)
                                                               !0.0-close to free drainage
! -------------------------------------------------------------
      QDIS      = 0.0
      QIN       = 0.0

! Derive layer-bottom depth in [mm]
!KWM:  Derive layer thickness in mm

      DZMM(1) = -ZSOIL(1)*1.E3
      DO IZ = 2, NSOIL
         DZMM(IZ)  = 1.E3 * (ZSOIL(IZ - 1) - ZSOIL(IZ))
      ENDDO

! Derive node (middle) depth in [m]
!KWM:  Positive number, depth below ground surface in m
      ZNODE(1) = -ZSOIL(1) / 2.
      DO IZ = 2, NSOIL
         ZNODE(IZ)  = -ZSOIL(IZ-1) + 0.5 * (ZSOIL(IZ-1) - ZSOIL(IZ))
      ENDDO

! Convert volumetric soil moisture "sh2o" to mass

      DO IZ = 1, NSOIL
         SMC(IZ)      = SH2O(IZ) + SICE(IZ)
         MLIQ(IZ)     = SH2O(IZ) * DZMM(IZ)
         EPORE(IZ)    = MAX(0.01,SMCMAX - SICE(IZ))
         HK(IZ)       = 1.E3*WCND(IZ)
      ENDDO

! The layer index of the first unsaturated layer,
! i.e., the layer right above the water table

      IWT = NSOIL
      DO IZ = 2,NSOIL
         IF(ZWT   .LE. -ZSOIL(IZ) ) THEN
            IWT = IZ-1
            EXIT
         END IF
      ENDDO

! Groundwater discharge [mm/s]

      FFF   = 6.0
      RSBMX = 5.0

      QDIS = (1.0-FCRMAX)*RSBMX*EXP(-TIMEAN)*EXP(-FFF*(ZWT-2.0))

! Matric potential at the layer above the water table

      S_NODE = MIN(1.0,SMC(IWT)/SMCMAX )
      S_NODE = MAX(S_NODE,REAL(0.01,KIND=8))
      SMPFZ  = -PSISAT*1000.*S_NODE**(-BEXP)   ! m --> mm
      SMPFZ  = MAX(-120000.0,CMIC*SMPFZ)   

! Recharge rate qin to groundwater

      KA  = HK(IWT)

      WH_ZWT  = - ZWT * 1.E3                          !(mm)
      WH      = SMPFZ  - ZNODE(IWT)*1.E3              !(mm)
      QIN     = - KA * (WH_ZWT-WH)  /((ZWT-ZNODE(IWT))*1.E3)
      QIN     = MAX(-10.0/DT,MIN(10./DT,QIN))
     
! Water storage in the aquifer + saturated soil

      WT  = WT + (QIN - QDIS) * DT     !(mm)

      IF(IWT.EQ.NSOIL) THEN
         WA          = WA + (QIN - QDIS) * DT     !(mm)
         WT          = WA
         ZWT         = (-ZSOIL(NSOIL) + 25.) - WA/1000./ROUS      !(m)
         MLIQ(NSOIL) = MLIQ(NSOIL) - QIN * DT        ! [mm]

         MLIQ(NSOIL) = MLIQ(NSOIL) + MAX(0.,(WA - 5000.))
         WA          = MIN(WA, 5000.)
      ELSE
         
         IF (IWT.EQ.NSOIL-1) THEN
            ZWT = -ZSOIL(NSOIL)                   &
                 - (WT-ROUS*1000*25.) / (EPORE(NSOIL))/1000.
         ELSE
            WS = 0.   ! water used to fill soil air pores
            DO IZ = IWT+2,NSOIL
               WS = WS + EPORE(IZ) * DZMM(IZ)
            ENDDO
            ZWT = -ZSOIL(IWT+1)                  &
                  - (WT-ROUS*1000.*25.-WS) /(EPORE(IWT+1))/1000.
         ENDIF

         WTSUB = 0.
         DO IZ = 1, NSOIL
           WTSUB = WTSUB + HK(IZ)*DZMM(IZ)
         END DO

         DO IZ = 1, NSOIL           ! Removing subsurface runoff
         MLIQ(IZ) = MLIQ(IZ) - QDIS*DT*HK(IZ)*DZMM(IZ)/WTSUB
         END DO
      END IF

      ZWT = MAX(1.5,ZWT)

!
! Limit MLIQ to be greater than or equal to watmin.
! Get water needed to bring MLIQ equal WATMIN from lower layer.
!
      WATMIN = 0.01
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
        WA       = WA - XS
        WT       = WT - XS

      DO IZ = 1, NSOIL
        SH2O(IZ)     = MLIQ(IZ) / DZMM(IZ)
      END DO

  END SUBROUTINE GROUNDWATER
! ==================================================================================================
! ********************* end of water subroutines ******************************************
! ==================================================================================================
  SUBROUTINE CARBON (NSNOW  ,NSOIL  ,VEGTYP ,NROOT  ,DT     ,ZSOIL  , & !in
                     DZSNSO ,STC    ,SMC    ,TV     ,TG     ,PSN    , & !in
                     FOLN   ,SMCMAX ,BTRAN  ,APAR   ,FVEG   ,IGS    , & !in
                     TROOT  ,IST    ,LAT    ,ILOC   ,JLOC   ,ISURBAN, & !in
                     LFMASS ,RTMASS ,STMASS ,WOOD   ,STBLCP ,FASTCP , & !inout
                     GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  , & !out
                     TOTLB  ,XLAI   ,XSAI   )                   !out
! ------------------------------------------------------------------------------------------
  USE NOAHMP_VEG_PARAMETERS
! ------------------------------------------------------------------------------------------
      IMPLICIT NONE
! ------------------------------------------------------------------------------------------
! inputs (carbon)

  INTEGER                        , INTENT(IN) :: ILOC   !grid index
  INTEGER                        , INTENT(IN) :: JLOC   !grid index
  INTEGER                        , INTENT(IN) :: VEGTYP !vegetation type 
  INTEGER                        , INTENT(IN) :: ISURBAN!Urban category
  INTEGER                        , INTENT(IN) :: NSNOW  !number of snow layers
  INTEGER                        , INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER                        , INTENT(IN) :: NROOT  !no. of root layers
  REAL                           , INTENT(IN) :: LAT    !latitude (radians)
  REAL                           , INTENT(IN) :: DT     !time step (s)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL  !depth of layer-bottom from soil surface
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer thickness [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !snow/soil temperature [k]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL                           , INTENT(IN) :: TV     !vegetation temperature (k)
  REAL                           , INTENT(IN) :: TG     !ground temperature (k)
  REAL                           , INTENT(IN) :: FOLN   !foliage nitrogen (%)
  REAL                           , INTENT(IN) :: SMCMAX !soil porosity (m3/m3)
  REAL                           , INTENT(IN) :: BTRAN  !soil water transpiration factor (0 to 1)
  REAL                           , INTENT(IN) :: PSN    !total leaf photosyn (umolco2/m2/s) [+]
  REAL                           , INTENT(IN) :: APAR   !PAR by canopy (w/m2)
  REAL                           , INTENT(IN) :: IGS    !growing season index (0=off, 1=on)
  REAL                           , INTENT(IN) :: FVEG   !vegetation greenness fraction
  REAL                           , INTENT(IN) :: TROOT  !root-zone averaged temperature (k)
  INTEGER                        , INTENT(IN) :: IST    !surface type 1->soil; 2->lake

! input & output (carbon)

  REAL                        , INTENT(INOUT) :: LFMASS !leaf mass [g/m2]
  REAL                        , INTENT(INOUT) :: RTMASS !mass of fine roots [g/m2]
  REAL                        , INTENT(INOUT) :: STMASS !stem mass [g/m2]
  REAL                        , INTENT(INOUT) :: WOOD   !mass of wood (incl. woody roots) [g/m2]
  REAL                        , INTENT(INOUT) :: STBLCP !stable carbon in deep soil [g/m2]
  REAL                        , INTENT(INOUT) :: FASTCP !short-lived carbon in shallow soil [g/m2]

! outputs: (carbon)

  REAL                          , INTENT(OUT) :: GPP    !net instantaneous assimilation [g/m2/s C]
  REAL                          , INTENT(OUT) :: NPP    !net primary productivity [g/m2/s C]
  REAL                          , INTENT(OUT) :: NEE    !net ecosystem exchange [g/m2/s CO2]
  REAL                          , INTENT(OUT) :: AUTORS !net ecosystem respiration [g/m2/s C]
  REAL                          , INTENT(OUT) :: HETERS !organic respiration [g/m2/s C]
  REAL                          , INTENT(OUT) :: TOTSC  !total soil carbon [g/m2 C]
  REAL                          , INTENT(OUT) :: TOTLB  !total living carbon ([g/m2 C]
  REAL                          , INTENT(OUT) :: XLAI   !leaf area index [-]
  REAL                          , INTENT(OUT) :: XSAI   !stem area index [-]
!  REAL                          , INTENT(OUT) :: VOCFLX(5) ! voc fluxes [ug C m-2 h-1]

! local variables

  INTEGER :: J         !do-loop index
  REAL    :: WROOT     !root zone soil water [-]
  REAL    :: WSTRES    !water stress coeficient [-]  (1. for wilting )
  REAL    :: LAPM      !leaf area per unit mass [m2/g]
! ------------------------------------------------------------------------------------------

   IF ( ( VEGTYP == ISWATER ) .OR. ( VEGTYP == ISBARREN ) .OR. ( VEGTYP == ISSNOW ) .or. (VEGTYP == ISURBAN) ) THEN
      XLAI   = 0.
      XSAI   = 0.
      GPP    = 0.
      NPP    = 0.
      NEE    = 0.
      AUTORS = 0.
      HETERS = 0.
      TOTSC  = 0.
      TOTLB  = 0.
      LFMASS = 0.
      RTMASS = 0.
      STMASS = 0.
      WOOD   = 0.
      STBLCP = 0.
      FASTCP = 0.

      RETURN
   END IF

      LAPM       = SLA(VEGTYP) / 1000.   ! m2/kg -> m2/g

! water stress

      WSTRES  = 1.- BTRAN

      WROOT  = 0.
      DO J=1,NROOT
        WROOT = WROOT + SMC(J)/SMCMAX *  DZSNSO(J) / (-ZSOIL(NROOT))
      ENDDO

  CALL CO2FLUX (NSNOW  ,NSOIL  ,VEGTYP ,IGS    ,DT     , & !in
                DZSNSO ,STC    ,PSN    ,TROOT  ,TV     , & !in
                WROOT  ,WSTRES ,FOLN   ,LAPM   ,         & !in
                LAT    ,ILOC   ,JLOC   ,FVEG   ,         & !in
                XLAI   ,XSAI   ,LFMASS ,RTMASS ,STMASS , & !inout
                FASTCP ,STBLCP ,WOOD   ,                 & !inout
                GPP    ,NPP    ,NEE    ,AUTORS ,HETERS , & !out
                TOTSC  ,TOTLB  )                           !out

!   CALL BVOC (VOCFLX,  VEGTYP,  VEGFAC,   APAR,   TV)
!   CALL CH4

  END SUBROUTINE CARBON
! ==================================================================================================
  SUBROUTINE CO2FLUX (NSNOW  ,NSOIL  ,VEGTYP ,IGS    ,DT     , & !in
                      DZSNSO ,STC    ,PSN    ,TROOT  ,TV     , & !in
                      WROOT  ,WSTRES ,FOLN   ,LAPM   ,         & !in
                      LAT    ,ILOC   ,JLOC   ,FVEG   ,         & !in
                      XLAI   ,XSAI   ,LFMASS ,RTMASS ,STMASS , & !inout
                      FASTCP ,STBLCP ,WOOD   ,                 & !inout
                      GPP    ,NPP    ,NEE    ,AUTORS ,HETERS , & !out
                      TOTSC  ,TOTLB  )                           !out
! -----------------------------------------------------------------------------------------
! The original code is from RE Dickinson et al.(1998), modifed by Guo-Yue Niu, 2004
! -----------------------------------------------------------------------------------------
  USE NOAHMP_VEG_PARAMETERS
! -----------------------------------------------------------------------------------------
  IMPLICIT NONE
! -----------------------------------------------------------------------------------------

! input

  INTEGER                        , INTENT(IN) :: ILOC   !grid index
  INTEGER                        , INTENT(IN) :: JLOC   !grid index
  INTEGER                        , INTENT(IN) :: VEGTYP !vegetation physiology type
  INTEGER                        , INTENT(IN) :: NSNOW  !number of snow layers
  INTEGER                        , INTENT(IN) :: NSOIL  !number of soil layers
  REAL                           , INTENT(IN) :: DT     !time step (s)
  REAL                           , INTENT(IN) :: LAT    !latitude (radians)
  REAL                           , INTENT(IN) :: IGS    !growing season index (0=off, 1=on)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer thickness [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !snow/soil temperature [k]
  REAL                           , INTENT(IN) :: PSN    !total leaf photosynthesis (umolco2/m2/s)
  REAL                           , INTENT(IN) :: TROOT  !root-zone averaged temperature (k)
  REAL                           , INTENT(IN) :: TV     !leaf temperature (k)
  REAL                           , INTENT(IN) :: WROOT  !root zone soil water
  REAL                           , INTENT(IN) :: WSTRES !soil water stress
  REAL                           , INTENT(IN) :: FOLN   !foliage nitrogen (%)
  REAL                           , INTENT(IN) :: LAPM   !leaf area per unit mass [m2/g]
  REAL                           , INTENT(IN) :: FVEG   !vegetation greenness fraction

! input and output

  REAL                        , INTENT(INOUT) :: XLAI   !leaf  area index from leaf carbon [-]
  REAL                        , INTENT(INOUT) :: XSAI   !stem area index from leaf carbon [-]
  REAL                        , INTENT(INOUT) :: LFMASS !leaf mass [g/m2]
  REAL                        , INTENT(INOUT) :: RTMASS !mass of fine roots [g/m2]
  REAL                        , INTENT(INOUT) :: STMASS !stem mass [g/m2]
  REAL                        , INTENT(INOUT) :: FASTCP !short lived carbon [g/m2]
  REAL                        , INTENT(INOUT) :: STBLCP !stable carbon pool [g/m2]
  REAL                        , INTENT(INOUT) :: WOOD   !mass of wood (incl. woody roots) [g/m2]

! output

  REAL                          , INTENT(OUT) :: GPP    !net instantaneous assimilation [g/m2/s]
  REAL                          , INTENT(OUT) :: NPP    !net primary productivity [g/m2]
  REAL                          , INTENT(OUT) :: NEE    !net ecosystem exchange (autors+heters-gpp)
  REAL                          , INTENT(OUT) :: AUTORS !net ecosystem resp. (maintance and growth)
  REAL                          , INTENT(OUT) :: HETERS !organic respiration
  REAL                          , INTENT(OUT) :: TOTSC  !total soil carbon (g/m2)
  REAL                          , INTENT(OUT) :: TOTLB  !total living carbon (g/m2)

! local

  REAL                   :: CFLUX    !carbon flux to atmosphere [g/m2/s]
  REAL                   :: LFMSMN   !minimum leaf mass [g/m2]
  REAL                   :: RSWOOD   !wood respiration [g/m2]
  REAL                   :: RSLEAF   !leaf maintenance respiration per timestep [g/m2]
  REAL                   :: RSROOT   !fine root respiration per time step [g/m2]
  REAL                   :: NPPL     !leaf net primary productivity [g/m2/s]
  REAL                   :: NPPR     !root net primary productivity [g/m2/s]
  REAL                   :: NPPW     !wood net primary productivity [g/m2/s]
  REAL                   :: NPPS     !wood net primary productivity [g/m2/s]
  REAL                   :: DIELF    !death of leaf mass per time step [g/m2]

  REAL                   :: ADDNPPLF !leaf assimil after resp. losses removed [g/m2]
  REAL                   :: ADDNPPST !stem assimil after resp. losses removed [g/m2]
  REAL                   :: CARBFX   !carbon assimilated per model step [g/m2]
  REAL                   :: GRLEAF   !growth respiration rate for leaf [g/m2/s]
  REAL                   :: GRROOT   !growth respiration rate for root [g/m2/s]
  REAL                   :: GRWOOD   !growth respiration rate for wood [g/m2/s]
  REAL                   :: GRSTEM   !growth respiration rate for stem [g/m2/s]
  REAL                   :: LEAFPT   !fraction of carbon allocated to leaves [-]
  REAL                   :: LFDEL    !maximum  leaf mass  available to change [g/m2/s]
  REAL                   :: LFTOVR   !stem turnover per time step [g/m2]
  REAL                   :: STTOVR   !stem turnover per time step [g/m2]
  REAL                   :: WDTOVR   !wood turnover per time step [g/m2]
  REAL                   :: RSSOIL   !soil respiration per time step [g/m2]
  REAL                   :: RTTOVR   !root carbon loss per time step by turnover [g/m2]
  REAL                   :: STABLC   !decay rate of fast carbon to slow carbon [g/m2/s]
  REAL                   :: WOODF    !calculated wood to root ratio [-]
  REAL                   :: NONLEF   !fraction of carbon to root and wood [-]
  REAL                   :: ROOTPT   !fraction of carbon flux to roots [-]
  REAL                   :: WOODPT   !fraction of carbon flux to wood [-]
  REAL                   :: STEMPT   !fraction of carbon flux to stem [-]
  REAL                   :: RESP     !leaf respiration [umol/m2/s]
  REAL                   :: RSSTEM   !stem respiration [g/m2/s]

  REAL                   :: FSW      !soil water factor for microbial respiration
  REAL                   :: FST      !soil temperature factor for microbial respiration
  REAL                   :: FNF      !foliage nitrogen adjustemt to respiration (<= 1)
  REAL                   :: TF       !temperature factor
  REAL                   :: RF       !respiration reduction factor (<= 1)
  REAL                   :: STDEL
  REAL                   :: STMSMN
  REAL                   :: SAPM     !stem area per unit mass (m2/g)
  REAL                   :: DIEST
! -------------------------- constants -------------------------------
  REAL                   :: BF       !parameter for present wood allocation [-]
  REAL                   :: RSWOODC  !wood respiration coeficient [1/s]
  REAL                   :: STOVRC   !stem turnover coefficient [1/s]
  REAL                   :: RSDRYC   !degree of drying that reduces soil respiration [-]
  REAL                   :: RTOVRC   !root turnover coefficient [1/s]
  REAL                   :: WSTRC    !water stress coeficient [-]
  REAL                   :: LAIMIN   !minimum leaf area index [m2/m2]
  REAL                   :: XSAMIN   !minimum leaf area index [m2/m2]
  REAL                   :: SC
  REAL                   :: SD
  REAL                   :: VEGFRAC

! Respiration as a function of temperature

  real :: r,x
          r(x) = exp(0.08*(x-298.16))
! ---------------------------------------------------------------------------------

! constants
    RTOVRC  = 2.0E-8        !original was 2.0e-8
    RSDRYC  = 40.0          !original was 40.0
    RSWOODC = 3.0E-10       !
    BF      = 0.90          !original was 0.90   ! carbon to roots
    WSTRC   = 100.0
    LAIMIN  = 0.05   
    XSAMIN  = 0.01

    SAPM    = 3.*0.001      ! m2/kg -->m2/g
    LFMSMN  = laimin/lapm
    STMSMN  = xsamin/sapm
! ---------------------------------------------------------------------------------

! respiration

     IF(IGS .EQ. 0.) THEN
       RF = 0.5
     ELSE
       RF = 1.0
     ENDIF
            
     FNF     = MIN( FOLN/MAX(1.E-06,FOLNMX(VEGTYP)), 1.0 )
     TF      = ARM(VEGTYP)**( (TV-298.16)/10. )
     RESP    = RMF25(VEGTYP) * TF * FNF * XLAI * RF * (1.-WSTRES) ! umol/m2/s
     RSLEAF  = MIN(LFMASS/DT,RESP*12.e-6)                         ! g/m2/s
     
     RSROOT  = RMR25(VEGTYP)*(RTMASS*1E-3)*TF *RF* 12.e-6         ! g/m2/s
     RSSTEM  = RMS25(VEGTYP)*(STMASS*1E-3)*TF *RF* 12.e-6         ! g/m2/s
     RSWOOD  = RSWOODC * R(TV) * WOOD*WDPOOL(VEGTYP)

! carbon assimilation
! 1 mole -> 12 g carbon or 44 g CO2; 1 umol -> 12.e-6 g carbon;

     CARBFX  = PSN * 12.e-6              ! umol co2 /m2/ s -> g/m2/s carbon

! fraction of carbon into leaf versus nonleaf

     LEAFPT = EXP(0.01*(1.-EXP(0.75*XLAI))*XLAI)
     IF(VEGTYP ==EBLFOREST) LEAFPT = EXP(0.01*(1.-EXP(0.50*XLAI))*XLAI)

     NONLEF = 1.0 - LEAFPT
     STEMPT = XLAI/10.0
     LEAFPT = LEAFPT - STEMPT

!  fraction of carbon into wood versus root

     IF(WOOD.GT.0) THEN
        WOODF = (1.-EXP(-BF*(WRRAT(VEGTYP)*RTMASS/WOOD))/BF)*WDPOOL(VEGTYP)
     ELSE
        WOODF = 0.
     ENDIF

     ROOTPT = NONLEF*(1.-WOODF)
     WOODPT = NONLEF*WOODF

! leaf and root turnover per time step

     LFTOVR = LTOVRC(VEGTYP)*1.E-6*LFMASS
     STTOVR = LTOVRC(VEGTYP)*1.E-6*STMASS
     RTTOVR = RTOVRC*RTMASS
     WDTOVR = 9.5E-10*WOOD

! seasonal leaf die rate dependent on temp and water stress
! water stress is set to 1 at permanent wilting point

     SC  = EXP(-0.3*MAX(0.,TV-TDLEF(VEGTYP))) * (LFMASS/120.) 
     SD  = EXP((WSTRES-1.)*WSTRC)
     DIELF = LFMASS*1.E-6*(DILEFW(VEGTYP) * SD + DILEFC(VEGTYP)*SC)
     DIEST = STMASS*1.E-6*(DILEFW(VEGTYP) * SD + DILEFC(VEGTYP)*SC)

! calculate growth respiration for leaf, rtmass and wood

     GRLEAF = MAX(0.0,FRAGR(VEGTYP)*(LEAFPT*CARBFX - RSLEAF))
     GRSTEM = MAX(0.0,FRAGR(VEGTYP)*(STEMPT*CARBFX - RSSTEM))
     GRROOT = MAX(0.0,FRAGR(VEGTYP)*(ROOTPT*CARBFX - RSROOT))
     GRWOOD = MAX(0.0,FRAGR(VEGTYP)*(WOODPT*CARBFX - RSWOOD))

! Impose lower T limit for photosynthesis

     ADDNPPLF = MAX(0.,LEAFPT*CARBFX - GRLEAF-RSLEAF)
     ADDNPPST = MAX(0.,STEMPT*CARBFX - GRSTEM-RSSTEM)
     IF(TV.LT.TMIN(VEGTYP)) ADDNPPLF =0.
     IF(TV.LT.TMIN(VEGTYP)) ADDNPPST =0.

! update leaf, root, and wood carbon
! avoid reducing leaf mass below its minimum value but conserve mass

     LFDEL = (LFMASS - LFMSMN)/DT
     STDEL = (STMASS - STMSMN)/DT
     DIELF = MIN(DIELF,LFDEL+ADDNPPLF-LFTOVR)
     DIEST = MIN(DIEST,STDEL+ADDNPPST-STTOVR)

! net primary productivities

     NPPL   = MAX(ADDNPPLF,-LFDEL)
     NPPS   = MAX(ADDNPPST,-STDEL)
     NPPR   = ROOTPT*CARBFX - RSROOT - GRROOT
     NPPW   = WOODPT*CARBFX - RSWOOD - GRWOOD

! masses of plant components

     LFMASS = LFMASS + (NPPL-LFTOVR-DIELF)*DT
     STMASS = STMASS + (NPPS-STTOVR-DIEST)*DT   ! g/m2
     RTMASS = RTMASS + (NPPR-RTTOVR)      *DT

     IF(RTMASS.LT.0.0) THEN
           RTTOVR = NPPR
           RTMASS = 0.0
     ENDIF
     WOOD = (WOOD+(NPPW-WDTOVR)*DT)*WDPOOL(VEGTYP)

! soil carbon budgets

     FASTCP = FASTCP + (RTTOVR+LFTOVR+STTOVR+WDTOVR+DIELF)*DT

     FST = 2.0**( (STC(1)-283.16)/10. )
     FSW = WROOT / (0.20+WROOT) * 0.23 / (0.23+WROOT)
     RSSOIL = FSW * FST * MRP(VEGTYP)* MAX(0.,FASTCP*1.E-3)*12.E-6

     STABLC = 0.1*RSSOIL
     FASTCP = FASTCP - (RSSOIL + STABLC)*DT
     STBLCP = STBLCP + STABLC*DT

!  total carbon flux

     CFLUX  = - CARBFX + RSLEAF + RSROOT + RSWOOD + RSSTEM &
              + RSSOIL + GRLEAF + GRROOT + GRWOOD                ! g/m2/s

! for outputs

     GPP    = CARBFX                                             !g/m2/s C
     NPP    = NPPL + NPPW + NPPR                                 !g/m2/s C
     AUTORS = RSROOT + RSWOOD  + RSLEAF +  &                     !g/m2/s C
              GRLEAF + GRROOT + GRWOOD                           !g/m2/s C
     HETERS = RSSOIL                                             !g/m2/s C
     NEE    = (AUTORS + HETERS - GPP)*44./12.                    !g/m2/s CO2
     TOTSC  = FASTCP + STBLCP                                    !g/m2   C
     TOTLB  = LFMASS + RTMASS + WOOD                             !g/m2   C

! leaf area index and stem area index

     XLAI    = MAX(LFMASS*LAPM,LAIMIN)
     XSAI    = MAX(STMASS*SAPM,XSAMIN)
    
  END SUBROUTINE CO2FLUX
! ==================================================================================================
! ------------------------------------------------------------------------------------------
  SUBROUTINE BVOCFLUX(VOCFLX,  VEGTYP,  VEGFRAC,  APAR,   TV )
    use NOAHMP_VEG_PARAMETERS , ONLY : SLAREA, EPS
! ------------------------------------------------------------------------------------------

! ------------------------------------------------------------------------------------------
      implicit none
! ------------------------------------------------------------------------------------------

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
  integer                     ,INTENT(IN) :: vegtyp  !vegetation type 
  real                        ,INTENT(IN) :: vegfrac !green vegetation fraction [0.0-1.0]
  real                        ,INTENT(IN) :: apar    !photosynthesis active energy by canopy (w/m2)
  real                        ,INTENT(IN) :: tv      !vegetation canopy temperature (k)

! output
  real                        ,INTENT(OUT) :: vocflx(5) ! voc fluxes [ug C m-2 h-1]

! Local Variables

  real, parameter :: R      = 8.314    ! univ. gas constant [J K-1 mol-1]
  real, parameter :: alpha  = 0.0027   ! empirical coefficient
  real, parameter :: cl1    = 1.066    ! empirical coefficient
  real, parameter :: ct1    = 95000.0  ! empirical coefficient [J mol-1]
  real, parameter :: ct2    = 230000.0 ! empirical coefficient [J mol-1]
  real, parameter :: ct3    = 0.961    ! empirical coefficient
  real, parameter :: tm     = 314.0    ! empirical coefficient [K]
  real, parameter :: tstd   = 303.0    ! std temperature [K]
  real, parameter :: bet    = 0.09     ! beta empirical coefficient [K-1]

  integer ivoc        ! do-loop index
  integer ityp        ! do-loop index
  real epsilon(5)
  real gamma(5)
  real density
  real elai
  real par,cl,reciprod,ct

! epsilon :

    do ivoc = 1, 5
    epsilon(ivoc) = eps(VEGTYP,ivoc)
    end do

! gamma : Activity factor. Units [dimensionless]

      reciprod = 1. / (R * tv * tstd)
      ct = exp(ct1 * (tv - tstd) * reciprod) / &
           (ct3 + exp(ct2 * (tv - tm) * reciprod))

      par = apar * 4.6 ! (multiply w/m2 by 4.6 to get umol/m2/s)
      cl  = alpha * cl1 * par * (1. + alpha * alpha * par * par)**(-0.5)

   gamma(1) = cl * ct ! for isoprenes

   do ivoc = 2, 5
   gamma(ivoc) = exp(bet * (tv - tstd))
   end do

! Foliage density

! transform vegfrac to lai      

   elai    = max(0.0,-6.5/2.5*alog((1.-vegfrac)))
   density = elai / (slarea(VEGTYP) * 0.5)

! calculate the voc flux

   do ivoc = 1, 5
   vocflx(ivoc) = epsilon(ivoc) * gamma(ivoc) * density
   end do

   end subroutine bvocflux
! ==================================================================================================
! ********************************* end of carbon subroutines *****************************
! ==================================================================================================
  SUBROUTINE REDPRM (VEGTYP,SOILTYP,SLOPETYP,ZSOIL,NSOIL,ISURBAN)
   
!niu  use module_sf_noahlsm_param_init

    IMPLICIT NONE
! ----------------------------------------------------------------------
! Internally set (default valuess)
! all soil and vegetation parameters required for the execusion oF
! the Noah lsm are defined in VEGPARM.TBL, SOILPARM.TB, and GENPARM.TBL.
! ----------------------------------------------------------------------
!     Vegetation parameters:
!             CMXTBL: MAX CNPY Capacity
!              NROOT: Rooting depth
!
! ----------------------------------------------------------------------
!      Soil parameters:
!       SSATPSI: SAT (saturation) soil potential
!        SSATDW: SAT soil diffusivity
!           F1: Soil thermal diffusivity/conductivity coef.
!        QUARTZ: Soil quartz content
!  Modified by F. Chen (12/22/97)  to use the STATSGO soil map
!  Modified By F. Chen (01/22/00)  to include PLaya, Lava, and White San
!  Modified By F. Chen (08/05/02)  to include additional parameters for the Noah
! NOTE: SATDW = BB*SATDK*(SATPSI/MAXSMC)
!         F11 = ALOG10(SATPSI) + BB*ALOG10(MAXSMC) + 2.0
!       REFSMC1=MAXSMC*(5.79E-9/SATDK)**(1/(2*BB+3)) 5.79E-9 m/s= 0.5 mm
!       REFSMC=REFSMC1+1./3.(MAXSMC-REFSMC1)
!       WLTSMC1=MAXSMC*(200./SATPSI)**(-1./BB)    (Wetzel and Chang, 198
!       WLTSMC=WLTSMC1-0.5*WLTSMC1
! Note: the values for playa is set for it to have a thermal conductivit
! as sand and to have a hydrulic conductivity as clay
!
! ----------------------------------------------------------------------
! BLANK        OCEAN/SEA
!       CSOIL_DATA: soil heat capacity [J M-3 K-1]
!       ZBOT_DATA: depth[M] of lower boundary soil temperature
!       CZIL_DATA: calculate roughness length of heat
!       SMLOW_DATA and MHIGH_DATA: two soil moisture wilt, soil moisture referen
!                 parameters
! Set maximum number of soil- and veg- in data statement.
! ----------------------------------------------------------------------
    INTEGER, PARAMETER     :: MAX_SOILTYP=30,MAX_VEGTYP=30

! Veg parameters
    INTEGER, INTENT(IN)    :: VEGTYP
    INTEGER, INTENT(IN)    :: ISURBAN
! Soil parameters
    INTEGER, INTENT(IN)    :: SOILTYP
! General parameters
    INTEGER, INTENT(IN)    :: SLOPETYP
! General parameters
    INTEGER, INTENT(IN)    :: NSOIL
! Layer parameters
    REAL,DIMENSION(NSOIL),INTENT(IN) :: ZSOIL

! Locals
    REAL    :: REFDK
    REAL    :: REFKDT
    REAL    :: FRZK
    REAL                   :: FRZFACT
    INTEGER                :: I
    CHARACTER(len=256)     :: message
! ----------------------------------------------------------------------
!
    IF (SOILTYP .gt. SLCATS) THEN
       call wrf_message('SOILTYP must be less than SLCATS:')
       write(message, '("SOILTYP = ", I6, ";    SLCATS = ", I6)') SOILTYP, SLCATS
       call wrf_message(trim(message))
       call wrf_error_fatal ('REDPRM: Error: too many input soil types')
    END IF
    IF (VEGTYP .gt. LUCATS) THEN
       call wrf_message('VEGTYP must be less than LUCATS:')
       write(message, '("VEGTYP = ", I6, ";    LUCATS = ", I6)') VEGTYP, LUCATS
       call wrf_message(trim(message))
       call wrf_error_fatal ('Error: too many input landuse types')
    END IF

! ----------------------------------------------------------------------
!  SET-UP SOIL PARAMETERS
! ----------------------------------------------------------------------
    CSOIL  = CSOIL_DATA
    BEXP   = BB (SOILTYP)
    DKSAT  = SATDK (SOILTYP)
    DWSAT  = SATDW (SOILTYP)
    F1     = F11 (SOILTYP)
    PSISAT = SATPSI (SOILTYP)
    QUARTZ = QTZ (SOILTYP)
    SMCDRY = DRYSMC (SOILTYP)
    SMCMAX = MAXSMC (SOILTYP)
    SMCREF = REFSMC (SOILTYP)
    SMCWLT = WLTSMC (SOILTYP)

    IF(VEGTYP==ISURBAN)THEN
       SMCMAX = 0.45 
       SMCREF = 0.42 
       SMCWLT = 0.40 
       SMCDRY = 0.40 
       CSOIL  = 3.E6
    ENDIF

! ----------------------------------------------------------------------
! Set-up universal parameters (not dependent on SOILTYP, VEGTYP)
! ----------------------------------------------------------------------
    ZBOT   = ZBOT_DATA
    CZIL   = CZIL_DATA

    FRZK   = FRZK_DATA
    REFDK  = REFDK_DATA
    REFKDT = REFKDT_DATA
    KDT    = REFKDT * DKSAT / REFDK
    SLOPE  = SLOPE_DATA (SLOPETYP)

! adjust FRZK parameter to actual soil type: FRZK * FRZFACT

    if(SOILTYP /= 14) then
      FRZFACT = (SMCMAX / SMCREF) * (0.412 / 0.468)
      FRZX = FRZK * FRZFACT
    end if

!    write(*,*) FRZK, FRZX, KDT, SLOPE, SLOPETYP
! ----------------------------------------------------------------------
! SET-UP VEGETATION PARAMETERS
! ----------------------------------------------------------------------
    ! Six redprm_canres variables:
    TOPT = TOPT_DATA
    RGL = RGLTBL (VEGTYP)
    RSMAX = RSMAX_DATA
    RSMIN = RSTBL (VEGTYP)
    HS = HSTBL (VEGTYP)
    NROOT = NROTBL (VEGTYP)

    IF(VEGTYP==ISURBAN)THEN
       RSMIN=400.0
    ENDIF

!    SHDFAC = SHDTBL(VEGTYP)
!    IF (VEGTYP .eq. BARE) SHDFAC = 0.0

    IF (NROOT .gt. NSOIL) THEN
       WRITE (*,*) 'Warning: too many root layers'
       write (*,*) 'NROOT = ', nroot
       write (*,*) 'NSOIL = ', nsoil
       call wrf_error_fatal("STOP in Noah-MP")
    END IF

! ----------------------------------------------------------------------

  END  SUBROUTINE REDPRM
! ==================================================================================================

  subroutine noahmp_options(idveg     ,iopt_crs  ,iopt_btr  ,iopt_run  ,iopt_sfc  ,iopt_frz , & 
                             iopt_inf  ,iopt_rad  ,iopt_alb  ,iopt_snf  ,iopt_tbot, iopt_stc )

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
  
  end subroutine noahmp_options
 
END MODULE NOAHMP_ROUTINES
! ==================================================================================================

MODULE MODULE_SF_NOAHMPLSM

  USE NOAHMP_ROUTINES
  USE NOAHMP_GLOBALS
  USE NOAHMP_VEG_PARAMETERS

END MODULE MODULE_SF_NOAHMPLSM
