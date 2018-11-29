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

program Noah_hrldas_driver

  USE module_hrldas_netcdf_io
  USE module_Noahlsm_utility
  USE module_sf_noahlsm
  USE module_sf_urban
  USE module_sfcdif_wrf
  USE module_date_utilities

  IMPLICIT NONE

#ifdef MPP_LAND
  include "mpif.h"
#endif

  character(len=9), parameter :: version = "v20110427"
  integer :: LDASIN_VERSION

  ! Dummy parameterized dimension for maximum number of soil levels allowed
  INTEGER, PARAMETER :: NSOLDX = 100
  integer :: yw_rst_out

  ! ZSOIL, set through the namelist, is the BOTTOM of each soil layer (m)
  ! Values are negative, implying depth below the surface.
  REAL, DIMENSION(NSOLDX) :: ZSOIL

  REAL :: T1V,TH2V
  LOGICAL :: FNDSNOWH

!-----------------------------------------------------------------
!  Dimensions from the input file:
!-----------------------------------------------------------------

!     IX: x-direction points, usually the west-east dimension
!     JX: y-direction points, usually the south-north dimension
  INTEGER :: IX
  INTEGER :: JX

!
! Attributes from LDASIN input file (or HRLDAS_CONSTANTS_FILE, as the case may be)
  REAL              :: DX
  REAL              :: DY
  REAL              :: TRUELAT1
  REAL              :: TRUELAT2
  REAL              :: CEN_LON
  INTEGER           :: MAPPROJ
  REAL              :: LAT1
  REAL              :: LON1
  INTEGER           :: ISWATER

!-----------------------------------------------------------------
!  DECLARE VARIABLES FOR GRIDDED SIMULATION
!-----------------------------------------------------------------

! the following parameters are read from a namelist file

! setup model configuration
  INTEGER :: KHOUR
  INTEGER :: KDAY

! Unaccounted-for variables when compiling w/ implicit none F90

  REAL   :: SNOFAC
  REAL   :: RHO,CHKFF

  INTEGER   :: i,j,k,ns,ierr
  INTEGER   :: NTIME
  REAL      :: FRACTION

! Gridded fields

  REAL,    allocatable, DIMENSION(:,:)   :: infxsrt,sfcheadrt, soldrain
  integer :: forc_typ, snow_assim, HRLDAS_ini_typ

  real, allocatable, dimension(:,:) ::  qsgw
  integer :: gwsoilcpl 
  
  INTEGER, allocatable, DIMENSION(:,:)   :: VEGTYP,SOLTYP
  REAL,    allocatable, DIMENSION(:,:)   :: TERRAIN, LATITUDE, LONGITUDE
  REAL,    allocatable, DIMENSION(:,:)   :: refdk2d, refkdt2d
  REAL,    allocatable, DIMENSION(:,:)   :: T2,XLONG,U,V,PRES,SHORT,PRCP1, prcp_old
  REAL,    allocatable, DIMENSION(:,:)   :: FPAR, ZNT, LAI, TBOT_2D, FPAR_SAVE, LAI_SAVE
  REAL,    allocatable, DIMENSION(:,:)   :: CMC,SNODEP,WEASD,T1
  REAL,    allocatable, DIMENSION(:,:)   :: ETPX,ETAX,GRDFLX,CHX,RUNOFF1X,RUNOFF2X
  REAL,    allocatable, DIMENSION(:,:)   :: ETAKIN, QFX
  REAL,    allocatable, DIMENSION(:,:)   :: RUNOFF3X,EDIRX,ECX,ETTX,SNMAXX,RCX,HX
  REAL,    allocatable, DIMENSION(:,:)   :: Q2X,SFCSPDX,ALBEDX,SMCMAX1,SMCWLT1
  REAL,    allocatable, DIMENSION(:,:)   :: SMCREF1,FX,RESX
  REAL,    allocatable, DIMENSION(:,:)   :: SNOFLXX,SNOEVPX,ACSNOM,ESNOW2D,ACRAIN
  REAL,    allocatable, DIMENSION(:,:)   :: DRIP2D, DEWFALL, SOILMX, EMISS, XLAI2D
  REAL,    allocatable, DIMENSION(:,:)   :: SNOTIME
  REAL,    allocatable, DIMENSION(:,:)   :: EMBRD2D
  REAL,    allocatable, DIMENSION(:,:)   :: SNOALB2D
  REAL,    allocatable, DIMENSION(:,:)   :: NOAHRES
  REAL,    allocatable, DIMENSION(:,:)   :: CMX
  REAL,    allocatable, DIMENSION(:,:)   :: Q12D
!KWM  REAL,    allocatable, DIMENSION(:,:)   :: ETPNDX, SFCHEAD, INFXS1, PDDUM2, PCPDRP, SFCWATR2

  REAL,    allocatable, DIMENSION(:,:,:) :: SMC,STC,SH2OX
  REAL,    allocatable, DIMENSION(:,:,:) :: ZSOILX 
  REAL,    allocatable, DIMENSION(:,:,:) :: SMAV2D

  ! Min/Max values from the 12-monthly Green Vegetation Fraction:
  REAL,    allocatable, DIMENSION(:,:)   :: GVFMIN, GVFMAX

  REAL,    allocatable, DIMENSION(:,:)   :: etpnd, greenfrac
  real :: etpnd1



  real :: albbrd
!---------------------------------------------------------------------
! NEW VARIABLES ADDED DURING NOAH F90 UPGRADE CALL to SFLX
!---------------------------------------------------------------------
  REAL :: EMISSI
  REAL :: EMBRD


! INTENT(IN) to SFLX:
  REAL    ::            FFROZP      ! Fraction of precipitation that is frozen
  REAL    ::            SOLNET      ! Net solar radiaton (W m-2)
  INTEGER, PARAMETER :: ICE = 0     ! Sea-ice flag  (=1: sea-ice, =0: land)
  REAL    :: DT       ! Timestep (s) (DT should not exceed 3600 S, recommend 1800 s or less)
  REAL    :: ZLVL     ! Height (m) above ground of atmospheric forcing variables
  REAL    :: ZLVL_WIND! Height (m) above ground of atmospheric forcing variables for wind.
  INTEGER :: NSOIL    ! Number of soil layers
  REAL, ALLOCATABLE, DIMENSION(:) :: SLDPTH ! The THICKNESS (m) of each soil layer 
  CHARACTER(LEN=256) :: LLANDUSE ! (=USGS, using USGS landuse classification)
  CHARACTER(LEN=256) :: LSOIL    ! (=STAS, using FAO/STATSGO soil texture classification)

  REAL :: LWDN      ! LW downward radiation (W m-2; positive, not net longwave)
  REAL :: SOLDN     ! Solar downward radiation (W m-2; positive, not net solar)
  REAL :: SFCPRS    ! Pressure (Pa) at height ZLVL m above ground
  REAL :: PRCP      ! Precip rate (kg m-2 s-1) (NOTE: this is a rate)
  REAL :: SFCTMP    ! Air temperature (K) at height ZLVL m above ground
  REAL :: Q2        ! Specific Humidity (kg kg-1) at height ZLVL m above ground 
  REAL :: SFCSPD    ! Wind speed (m s-1) at height ZLVL m above ground
  REAL :: PRCPRAIN  ! Liquid-precipitation rate (KG M-2 S-1) (not used)
  REAL :: TH2       ! Air potential temperature (K) at height ZLVL m above ground
  REAL :: Q2SAT     ! Saturation specific humidity (kg kg-1) at height ZLVL m above ground
  REAL :: DQSDT2    ! Slope of saturation specific humidity curve  (kg kg-1 K-1) at T=SFCTMP
  INTEGER :: VEGTYPX! Vegetation type (integer index)
  INTEGER :: SOILTYP! Soil type (integer index)
  INTEGER ::SLOPETYP! Class of sfc slope (integer index)
  REAL :: SHDMIN    ! Minimum areal fractional coverage of green vegetation
  !         (dimensionless fraction 0.0-1.0) <= SHDFAC
  REAL :: SHDMAX    !UNDOCUMENTED IN SFLX
  REAL :: TBOT      ! Bottom soil temperature (local yearly-mean of surfacc air temperature)
  LOGICAL :: RDLAI2D = .FALSE.
  LOGICAL :: USEMONALB = .FALSE.
  REAL    :: RIBB = 0.0


! INTENT(INOUT) to/from SFLX:
  REAL :: SNCOVR  ! Fractional snow cover (dimensionless fraction, 0.0-1.0)
  REAL :: COSZ    ! Solar zenith angle
  REAL :: DECLIN  ! Solar declension (rad)
  REAL :: SOLARDIRECT ! Direct component of downward solar radiation (W m-2) (not used)
  REAL :: Z0      ! Time varying roughness length (m) as function of snow depth
  REAL :: CMCX    ! Canopy moisture content (m)
  REAL :: T1X     ! Ground/Canopy/Snowpack effective skin temperature (K)
  REAL, allocatable, DIMENSION(:)  :: STC1    ! Soil temp (K)                            
  REAL, allocatable, DIMENSION(:)  :: SMC1    ! Total soil moisture content (volumetric fraction)
  REAL, allocatable, DIMENSION(:)  :: SH2O    ! Unfrozen soil moisture content (volumetric fraction)
  !          NOTE: Frozen soil moisture = SMC - SH2O
  REAL :: SNOWH   ! Actual snow depth (m)                                      
  REAL :: SNEQV   ! Liquid water-equivalent snow depth (m)                     
  !       NOTE: snow density = SNEQV/SNOWH                         
  REAL :: ALBEDO  ! Surface albedo including snow effect (unitless fraction)   
  !      =snow-free albedo (ALB) when SNEQV=0, or                 
  !      =FCT(MSNOALB,ALB,VEGTYP,SHDFAC,SHDMIN) when SNEQV>0      
  REAL :: CH      ! Surface exchange coefficient for heat and moisture         
  !   (m s-1); NOTE: CH is technically a conductance since     
  !   it has been multiplied by wind speed.                    
  REAL :: CM      ! Surface exchange coefficient for momentum (m s-1); NOTE:   
  !   CM is technically a conductance since it has been        
  !   multiplied by wind speed.                                
  REAL :: SNOTIME1

! INTENT(OUT) from SFLX:
  REAL :: ETT     ! Total plant transpiration (W m-2)
  REAL :: ETA     ! Actual latent heat flux (W m-2: negative, if up from surface)     
!KWM ???  Is the note about the sign of ETA right ???? !KWM
  REAL :: SHEAT   ! Sensible heat flux (W m-2: negative, if upward from surface)
  REAL :: ETA_KINEMATIC ! Actual latent heat flux (kg m/s)
  REAL :: FDOWN   ! Radiation forcing at the surface (W m-2) = SOLDN*(1-alb)+LWDN
  REAL :: EC      ! Canopy water evaporation (W m-2)
  REAL :: EDIR    ! Direct soil evaporation (W m-2)
  REAL, allocatable, DIMENSION(:) :: ET      ! Plant transpiration from a particular root (soil) layer (W m-2)
  REAL :: ESNOW   ! Sublimation from (or deposition to if <0) snowpack (W m-2)
  REAL :: DRIP    ! Through-fall of precip and/or dew in excess of canopy
  ! water-holding capacity (m)
  REAL :: DEW     ! Dewfall (or Frostfall for T<273.15) (m)

  REAL :: BETA    ! Ratio of actual to potential evap (dimensionless)
  REAL :: ETP     ! Potential Evaporation (w m-2)
  REAL :: SSOIL   ! Soil heat flux (W m-2: negative if downward from surface)
  REAL :: FLX1    ! Precip-snow sfc (W m-2)
  REAL :: FLX2    ! Freezing rain latent heat flux (W m-2)      
  REAL :: FLX3    ! Phase-change heat flux from snowmelt (W m-2)
  REAL :: SNOMLT  ! Snow melt (m) (water equivalent)
  REAL :: RUNOFF1 ! Surface runoff (m s-1), not infiltrating the surface
  REAL :: RUNOFF2 ! Subsurface runoff (m s-1), drainage out bottom of last
  !       soil layer (baseflow).  Note: RUNOFF2 is actually 
  !       the sum of RUNOFF2 and RUNOFF3
  REAL :: RUNOFF3 ! Numerical trunctation in excess of porosity (SMCMAX)
  ! for a given soil layer at the end of a time step (m s-1).
  REAL :: RC      ! Canopy resistance (s m-1)
  REAL :: PC      ! Plant coefficient (dimensionless fraction, 0.0-1.0) 
  ! where PC*ETP = actual transpiration
  REAL :: RCS     ! Incoming solar RC factor (dimensionless)
  REAL :: RCT     ! Air temperature RC factor (dimensionless)
  REAL :: RCQ     ! Atmos. vapor pressure deficit RC factor (dimensionless)
  REAL :: RCSOIL  ! Soil moisture RC factor (dimensionless)
  REAL :: SOILW   ! Available soil moisture in root zone (dimensionless fraction
  !      between SMCWLT and SMCMAX)
  REAL :: SOILM   ! Total soil column moisture content (frozen+unfrozen) (m)
  REAL :: Q1      ! Effective specific humidity at surface (kg kg-1), used for
  !      diagnosing the specific humidity at 2 meter for 
  !      coupled model
  REAL, allocatable, dimension(:) :: SMAV    ! Documentation?

  real ::  snoalb, frzx
  logical :: local = .TRUE.

  character(len=4) :: MMINSL

! Timing:
  integer :: clock_count_1 = 0
  integer :: clock_count_2 = 0
  integer :: clock_rate    = 0
  real    :: timing_sum    = 0.0

#ifdef _HRLDAS_URBAN_
! ----------------------------------------------------------------------
! DECLARATIONS START - urban
! ----------------------------------------------------------------------

  INTEGER :: KROOF
  INTEGER :: KWALL
  INTEGER :: KROAD
! input variables surface_driver --> lsm
  INTEGER, parameter  :: num_roof_layers = 4
  INTEGER, parameter  :: num_wall_layers = 4
  INTEGER, parameter  :: num_road_layers = 4

  REAL, DIMENSION(1:num_roof_layers)  :: DZR
  REAL, DIMENSION(1:num_wall_layers)  :: DZB
  REAL, DIMENSION(1:num_road_layers)  :: DZG

! input variables lsm --> urban
  INTEGER :: UTYPE_URB ! urban type [urban=1, suburban=2, rural=3]
  REAL :: TA_URB       ! potential temp at 1st atmospheric level [K]
  REAL :: QA_URB       ! mixing ratio at 1st atmospheric level  [kg/kg]
  REAL :: UA_URB       ! wind speed at 1st atmospheric level    [m/s]
  REAL :: U1_URB       ! u at 1st atmospheric level             [m/s]
  REAL :: V1_URB       ! v at 1st atmospheric level             [m/s]
  REAL :: SSG_URB      ! downward total short wave radiation    [W/m/m]
  REAL :: LLG_URB      ! downward long wave radiation           [W/m/m]
  REAL :: RAIN_URB     ! precipitation                          [mm/h]
  REAL :: RHOO_URB     ! air density                            [kg/m^3]
  REAL :: ZA_URB       ! first atmospheric level                [m]
  REAL :: DELT_URB     ! time step                              [s]
  REAL :: SSGD_URB     ! downward direct short wave radiation   [W/m/m]
  REAL :: SSGQ_URB     ! downward diffuse short wave radiation  [W/m/m]
  REAL :: XLAT_URB     ! latitude                               [deg]
  REAL :: OMG_URB      ! hour angle
  REAL :: ZNT_URB      ! roughness length                       [m]
  REAL :: TR_URB
  REAL :: TB_URB
  REAL :: TG_URB
  REAL :: TC_URB
  REAL :: QC_URB
  REAL :: UC_URB
  REAL :: XXXR_URB
  REAL :: XXXB_URB
  REAL :: XXXG_URB
  REAL :: XXXC_URB
  REAL, DIMENSION(1:num_roof_layers) :: TRL_URB  ! roof layer temp [K]
  REAL, DIMENSION(1:num_wall_layers) :: TBL_URB  ! wall layer temp [K]
  REAL, DIMENSION(1:num_road_layers) :: TGL_URB  ! road layer temp [K]
  LOGICAL :: LSOLAR_URB
! state variable surface_driver <--> lsm <--> urban
  REAL, ALLOCATABLE, DIMENSION(:,:) :: TR_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: TB_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: TG_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: TC_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: QC_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: UC_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: XXXR_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: XXXB_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: XXXG_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: XXXC_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: SH_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: LH_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: G_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: RN_URB2D
!
  REAL, ALLOCATABLE, DIMENSION(:,:) :: TS_URB2D

  REAL, ALLOCATABLE, DIMENSION( :, :, :) :: TRL_URB3D
  REAL, ALLOCATABLE, DIMENSION( :, :, :) :: TBL_URB3D
  REAL, ALLOCATABLE, DIMENSION( :, :, :) :: TGL_URB3D

! output variable lsm --> surface_driver
  REAL, ALLOCATABLE, DIMENSION(:,:) :: PSIM_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: PSIH_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: GZ1OZ0_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: U10_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: V10_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: TH2_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: Q2_URB2D
!
!m     REAL, ALLOCATABLE, DIMENSION(:,:) :: AKHS_URB2D 
  REAL, ALLOCATABLE, DIMENSION(:,:) :: AKMS_URB2D
!
  REAL, ALLOCATABLE, DIMENSION(:,:) :: UST_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: FRC_URB2D
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: UTYPE_URB2D

  REAL, ALLOCATABLE, DIMENSION(:,:) :: CMR_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: CHR_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: CMC_URB2D
  REAL, ALLOCATABLE, DIMENSION(:,:) :: CHC_URB2D

! output variables urban --> lsm
  REAL :: TS_URB     ! surface radiative temperature    [K]
  REAL :: QS_URB     ! surface humidity                 [-]
  REAL :: SH_URB     ! sensible heat flux               [W/m/m]
  REAL :: LH_URB     ! latent heat flux                 [W/m/m]
  REAL :: LH_KINEMATIC_URB ! latent heat flux, kinetic  [kg/m/m/s]
  REAL :: SW_URB     ! upward short wave radiation flux [W/m/m]
  REAL :: ALB_URB    ! time-varying albedo            [fraction]
  REAL :: LW_URB     ! upward long wave radiation flux  [W/m/m]
  REAL :: G_URB      ! heat flux into the ground        [W/m/m]
  REAL :: RN_URB     ! net radiation                    [W/m/m]
  REAL :: PSIM_URB   ! shear f for momentum             [-]
  REAL :: PSIH_URB   ! shear f for heat                 [-]
  REAL :: GZ1OZ0_URB   ! shear f for heat                 [-]
  REAL :: U10_URB    ! wind u component at 10 m         [m/s]
  REAL :: V10_URB    ! wind v component at 10 m         [m/s]
  REAL :: TH2_URB    ! potential temperature at 2 m     [K]
  REAL :: Q2_URB     ! humidity at 2 m                  [-]
  REAL :: CHS_URB
  REAL :: CHS2_URB
  REAL :: UST_URB
  REAL :: CMR_URB
  REAL :: CHR_URB
  REAL :: CMC_URB
  REAL :: CHC_URB

  integer :: NUM_URBAN_LAYERS = 10

  REAL, allocatable, dimension(:,:,:) :: TRB_URB4D
  REAL, allocatable, dimension(:,:,:) :: TW1_URB4D
  REAL, allocatable, dimension(:,:,:) :: TW2_URB4D
  REAL, allocatable, dimension(:,:,:) :: TGB_URB4D
  REAL, allocatable, dimension(:,:,:) :: SFW1_URB3D
  REAL, allocatable, dimension(:,:,:) :: SFW2_URB3D
  REAL, allocatable, dimension(:,:,:) :: SFR_URB3D
  REAL, allocatable, dimension(:,:,:) :: SFG_URB3D

  REAL, allocatable, dimension(:,:,:) :: A_U_BEP
  REAL, allocatable, dimension(:,:,:) :: A_V_BEP
  REAL, allocatable, dimension(:,:,:) :: A_T_BEP
  REAL, allocatable, dimension(:,:,:) :: A_Q_BEP
  REAL, allocatable, dimension(:,:,:) :: A_E_BEP
  REAL, allocatable, dimension(:,:,:) :: B_U_BEP
  REAL, allocatable, dimension(:,:,:) :: B_V_BEP
  REAL, allocatable, dimension(:,:,:) :: B_T_BEP
  REAL, allocatable, dimension(:,:,:) :: B_Q_BEP
  REAL, allocatable, dimension(:,:,:) :: B_E_BEP
  REAL, allocatable, dimension(:,:,:) :: VL_BEP
  REAL, allocatable, dimension(:,:,:) :: DLG_BEP
  REAL, allocatable, dimension(:,:,:) :: SF_BEP
  REAL, allocatable, dimension(:,:,:) :: DL_U_BEP

  REAL, allocatable, dimension(:,:,:) :: TLEV_URB3D
  REAL, allocatable, dimension(:,:,:) :: QLEV_URB3D
  REAL, allocatable, dimension(:,:,:) :: TW1LEV_URB3D
  REAL, allocatable, dimension(:,:,:) :: TW2LEV_URB3D
  REAL, allocatable, dimension(:,:,:) :: TGLEV_URB3D
  REAL, allocatable, dimension(:,:,:) :: TFLEV_URB3D
  REAL, allocatable, dimension(:,:,:) :: SFWIN1_URB3D
  REAL, allocatable, dimension(:,:,:) :: SFWIN2_URB3D
  REAL, allocatable, dimension(:,:  ) :: LF_AC_URB3D
  REAL, allocatable, dimension(:,:  ) :: SF_AC_URB3D
  REAL, allocatable, dimension(:,:  ) :: CM_AC_URB3D
  REAL, allocatable, dimension(:,:  ) :: SFVENT_URB3D
  REAL, allocatable, dimension(:,:  ) :: LFVENT_URB3D


! ----------------------------------------------------------------------
! DECLARATIONS END - urban
! ----------------------------------------------------------------------

#endif

  real :: CFACTR, CMCMAX, RSMAX, TOPT, REFKDT, KDT, SBETA, SHDFAC, RSMIN
  real :: RGL, HS, ZBOT, PSISAT, SLOPE, SNUP, SALP, BEXP, DKSAT, DWSAT
  real :: SMCMAX, SMCWLT, SMCREF, SMCDRY, F1, QUARTZ, FXEXP, Z0BRD, CZIL
  real :: XLAI, CSOIL, ALB, PTU, ALBEDOMIN, ALBEDOMAX
  real :: EMISSMIN, EMISSMAX, LAIMIN, LAIMAX, Z0MIN, Z0MAXnoah
  real, dimension(NSOLD) :: RTDIS
  integer :: NROOT
  real :: LVCOEF

!---------------------------------------------------------------------
!  DECLARE/Initialize constants
!---------------------------------------------------------------------

  REAL, PARAMETER :: R=287.04
  REAL, PARAMETER :: CPHEAT=1004.5
  INTEGER, PARAMETER :: LAND=1

  character(len=19) :: olddate, newdate, startdate,forcDate
  CHARACTER(len=256) :: inflnm, outflnm, inflnm_template
  character :: hgrid
  integer   :: igrid

  logical            :: restart_flag

! NAMELIST:

  CHARACTER(len=256) :: indir
  character(len=256) :: outdir = "."
  character(len=256) :: hrldas_constants_file = " "
  character(len=256) :: external_fpar_filename_template = " "
  character(len=256) :: external_lai_filename_template = " "
  character(len=256) :: restart_filename_requested = " "
  integer            :: split_output_count = 1
  integer            :: restart_frequency_hours
  integer            :: output_timestep
  integer            :: subwindow_xstart = 1
  integer            :: subwindow_ystart = 1
  integer            :: subwindow_xend = 0
  integer            :: subwindow_yend = 0
  integer            :: sfcdif_option = 0
  integer            :: iz0tlnd = 0
  logical            :: update_snow_from_forcing = .TRUE.

  integer :: START_YEAR, START_MONTH, START_DAY, START_HOUR, START_MIN
  integer :: noah_timestep = -999
  integer :: forcing_timestep = -999
  character(len = 256):: GEO_STATIC_FLNM
  
  integer :: gwCycle, GWBASESWCRT

  namelist / NOAHLSM_OFFLINE/ INDIR, NSOIL, ZSOIL, FORCING_TIMESTEP, NOAH_TIMESTEP, &
       START_YEAR, START_MONTH, START_DAY, START_HOUR, START_MIN, &
       RESTART_FREQUENCY_HOURS, OUTPUT_TIMESTEP, &
       SPLIT_OUTPUT_COUNT, SFCDIF_OPTION, IZ0TLND, UPDATE_SNOW_FROM_FORCING, & 
       KHOUR, KDAY, ZLVL, ZLVL_WIND, HRLDAS_CONSTANTS_FILE, OUTDIR, RESTART_FILENAME_REQUESTED, &
       external_fpar_filename_template, external_lai_filename_template, &
       subwindow_xstart, subwindow_xend, subwindow_ystart, subwindow_yend, &
       forc_typ, snow_assim , GEO_STATIC_FLNM, HRLDAS_ini_typ
  INTEGER  ::  sf_urban_physics = 0                       !urban
#ifdef _HRLDAS_URBAN_
  REAL     ::  zlvl_urban
  namelist / URBAN_OFFLINE / SF_URBAN_PHYSICS, ZLVL_URBAN
#endif
!KWM       , VEGMIN, VEGMAX

  integer, parameter :: iunit = 10
  integer, parameter :: ounit = 11

  integer :: parallel_xstart
  integer :: parallel_xend
  integer :: ixfull
  integer :: jxfull
  integer :: ixpar
  integer :: jxpar
  integer :: xstartpar
  integer :: ystartpar
  integer :: nlandpts
  integer :: rank
  integer :: imode
  character(len=256) :: restart_flnm

#ifdef _PARALLEL_  
  integer :: numtasks
  real, pointer, dimension(:,:) :: vegtyp_ptr
  integer :: send_tag
  integer :: recv_tag
  integer :: istatus
  integer :: ip
#endif

#ifdef MPP_LAND
  call MPI_INIT(ierr)
  if (ierr /= MPI_SUCCESS) stop "MPI_INIT"
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
#else
  rank = 0
#endif
  if(rank == 0) then

  LSOIL = 'STAS'

  ! Initialize namelist variables to dummy values, so we can tell
  ! if they have not been set properly.

  nsoil = 4
  zsoil = -1.E36
  zsoil(1:4) = (/ -0.05, -0.25, -0.7, -1.5 /)

  dt = -999.

  start_year = -999
  start_month = -999
  start_day = -999
  start_hour = -999
  start_min = -999
  khour = -999
  kday = -999

  zlvl = 2.0
  zlvl_wind = 10.0
!KWM  vegmin = -999.
!KWM  vegmax = -999.
  
  output_timestep = 0
  restart_frequency_hours = 0

  open(30, file="namelist.hrldas", form="FORMATTED")
  read(30, NOAHLSM_OFFLINE, iostat=ierr)
  if (ierr /= 0) then
     write(*,'(/," ***** ERROR: Problem reading namelist NOAHLSM_OFFLINE",/)')
     rewind(30)
     read(30, NOAHLSM_OFFLINE)
     stop " ***** ERROR: Problem reading namelist NOAHLSM_OFFLINE"
  endif
#ifdef _HRLDAS_URBAN_
  zlvl_urban = 15.0 ! Default value
  read(30, URBAN_OFFLINE, iostat=ierr)
  if (ierr /= 0) then
     write(*,'(/," ***** ERROR: Problem reading namelist URBAN_OFFLINE",/)')
     rewind(30)
     read(30, URBAN_OFFLINE)
     stop " ***** ERROR: Problem reading namelist URBAN_OFFLINE"
  endif
#endif

  close(30)

  if ((khour < 0) .and. (kday < 0)) then
     write(*, '(" ***** Namelist error: ************************************")')
     write(*, '(" ***** ")')
     write(*, '(" *****      Either KHOUR or KDAY must be defined.")')
     write(*, '(" ***** ")')
     stop
  else if (( khour < 0 ) .and. (kday > 0)) then
     khour = kday * 24
  else if ((khour > 0) .and. (kday > 0)) then
     write(*, '("Namelist warning:  KHOUR and KDAY both defined.")')
  else
     ! all is well.  KHOUR defined
  endif

  if (forcing_timestep < 0) then
     write(*, *)
     write(*, '(" ***** Namelist error: *****************************************")')
     write(*, '(" ***** ")')
     write(*, '(" *****       FORCING_TIMESTEP needs to be set greater than zero.")')
     write(*, '(" ***** ")')
     write(*, *)
     stop
  endif

  if (noah_timestep < 0) then
     write(*, *)
     write(*, '(" ***** Namelist error: *****************************************")')
     write(*, '(" ***** ")')
     write(*, '(" *****       NOAH_TIMESTEP needs to be set greater than zero.")')
     write(*, '(" *****                     900 seconds is recommended.       ")')
     write(*, '(" ***** ")')
     write(*, *)
     stop
  endif

  !
  ! Check that OUTPUT_TIMESTEP fits into NOAH_TIMESTEP:
  !
  if (output_timestep /= 0) then
     if (mod(output_timestep, noah_timestep) > 0) then
        write(*, *)
        write(*, '(" ***** Namelist error: *********************************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****       OUTPUT_TIMESTEP should be an integer multiple of NOAH_TIMESTEP.")')
        write(*, '(" *****            OUTPUT_TIMESTEP = ", I12, " seconds")') output_timestep
        write(*, '(" *****            NOAH_TIMESTEP   = ", I12, " seconds")') noah_timestep
        write(*, '(" ***** ")')
        write(*, *)
        stop
     endif
  endif

  !
  ! Check that RESTART_FREQUENCY_HOURS fits into NOAH_TIMESTEP:
  !
  if (restart_frequency_hours /= 0) then
     if (mod(restart_frequency_hours*3600, noah_timestep) > 0) then
        write(*, *)
        write(*, '(" ***** Namelist error: ******************************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****       RESTART_FREQUENCY_HOURS (converted to seconds) should be an ")')
        write(*, '(" *****       integer multiple of NOAH_TIMESTEP.")')
        write(*, '(" *****            RESTART_FREQUENCY_HOURS = ", I12, " hours:  ", I12, " seconds")') &
             restart_frequency_hours, restart_frequency_hours*3600
        write(*, '(" *****            NOAH_TIMESTEP           = ", I12, " seconds")') noah_timestep
        write(*, '(" ***** ")')
        write(*, *)
        stop
     endif
  endif

  dt = real(noah_timestep)

#ifdef _PARALLEL_  

  call MPI_INIT(ierr)
  if (ierr /= MPI_SUCCESS) stop "MPI_INIT"

  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  if (ierr /= MPI_SUCCESS) stop "MPI_COMM_RANK"

  call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
  if (ierr /= MPI_SUCCESS) stop "MPI_COMM_WORLD"

#else
  rank = 0
#endif

  call read_hrldas_hdrinfo(hrldas_constants_file, ix, jx, &
       subwindow_xstart, subwindow_xend, subwindow_ystart, subwindow_yend, &
       iswater, isurban, llanduse, dx, dy, truelat1, truelat2, cen_lon, lat1, lon1, &
#ifdef _PARALLEL_
  igrid, mapproj, vegtyp_ptr)

#else
  igrid, mapproj)
#endif

  write(hgrid,'(I1)') igrid

  if (nsoil < 0) then
     stop " ***** ERROR: NSOIL must be set in the namelist."
  endif

  write(olddate,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
       start_year, start_month, start_day, start_hour, start_min, 0

  startdate = olddate
  forcdate = olddate

!......................... end of model configuration (later in a namelist) .....

!KWM#ifdef _PARALLEL_  
!KWM
!KWM  nlandpts = count(vegtyp_ptr/=iswater)
!KWM
!KWM  call MPI_INIT(ierr)
!KWM  if (ierr /= MPI_SUCCESS) stop "MPI_INIT"
!KWM
!KWM  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
!KWM  if (ierr /= MPI_SUCCESS) stop "MPI_COMM_RANK"
!KWM
!KWM  call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
!KWM  if (ierr /= MPI_SUCCESS) stop "MPI_COMM_WORLD"
!KWM
!KWM#else
!KWM  rank = 0
!KWM#endif

  call check_outdir(rank, trim(outdir))

  MMINSL='STAS'
  CALL SOIL_VEG_GEN_PARM( LLANDUSE, MMINSL )

!----------------------------------------------------------------------
! Allocate arrays for our gridded domain, now that we know the size
!----------------------------------------------------------------------
  ixfull = subwindow_xend-subwindow_xstart+1
  jxfull = subwindow_yend-subwindow_ystart+1

#ifdef _PARALLEL_

  ! Set up our parallal_xstart and parallel_xend values, the x-coordinate points over which 
  ! each particular process will span, such that the land points of the full domain are 
  ! approximately evenly distributed among the processors.  This will make the loop which calls
  ! SFLX for the I/J points a little more balanced, but could make the
  ! I/O a little less balanced.

  if (rank == 0) then
     nlandpts = count(vegtyp_ptr/=iswater)
     j = 0
     k= 0
     do i = subwindow_xstart, subwindow_xend
        k = k + count( vegtyp_ptr(i,:) /= iswater )
        if ( ( k >= real(nlandpts*(j+1))/real(numtasks) ) .or. ( i == subwindow_xend ) ) then
           write(*, '("Found range for rank ", I4, ":  points ", I6, I6, I6)') j, ip, i, (i-ip)+1

           if ( j > 0 ) then
              ! Send the starting and ending points we've computed to the appropriate process (rank).
              call mpi_send( i, 1, MPI_INTEGER, j, 0, MPI_COMM_WORLD, ierr )
              if (ierr /= MPI_SUCCESS) stop "Problem with MPI_SEND"
              call mpi_send( ip, 1, MPI_INTEGER, j, 1, MPI_COMM_WORLD, ierr )
              if (ierr /= MPI_SUCCESS) stop "Problem with MPI_SEND"
           else
              parallel_xstart = subwindow_xstart
              parallel_xend = i
           endif

           j = j + 1
           ip = i + 1
        endif
     enddo
  else
     call mpi_recv( parallel_xend, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, istatus, ierr )
     if (ierr /= MPI_SUCCESS) stop "Problem with MPI_RECV"
     call mpi_recv( parallel_xstart, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, istatus, ierr )
     if (ierr /= MPI_SUCCESS) stop "Problem with MPI_RECV"
     ! print*, 'Rank ', rank, ' received ', parallel_xstart, parallel_xend
  endif

  ixpar  = parallel_xend-parallel_xstart+1
  jxpar  = jxfull
  xstartpar = (parallel_xstart-subwindow_xstart)+1
  ystartpar = 1

#else
  ! If we don't run parallel, the single task allocates the full requested subwindow.
  parallel_xstart = subwindow_xstart
  parallel_xend   = subwindow_xend
  ixpar = ixfull
  jxpar = jxfull
  xstartpar = 1
  ystartpar = 1
#endif

  ! print*, 'parallel_xstart, parallel_xend = ', parallel_xstart, parallel_xend

  allocate( VEGTYP   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( SOLTYP   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( TERRAIN  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( LATITUDE (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( LONGITUDE(PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( refdk2d  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( refkdt2d (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( T2       (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( XLONG    (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( U        (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( V        (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( PRES     (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( SHORT    (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( PRCP1    (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( PRCP_old    (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( FPAR     (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( ZNT      (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( LAI      (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( CMC      (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( SNODEP   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( WEASD    (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( T1       (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( ETPX     (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( ETAX     (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( ETAKIN   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( GRDFLX   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( CHX      (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( CMX      (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( Q12D     (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( RUNOFF1X (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( RUNOFF2X (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( RUNOFF3X (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( EDIRX    (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( ECX      (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( ETTX     (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( SNMAXX   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( RCX      (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( HX       (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( QFX      (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( Q2X      (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( SFCSPDX  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( ALBEDX   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( SMCMAX1  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( SMCWLT1  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( SMCREF1  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( ACSNOM   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( ACRAIN   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( ESNOW2D  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
!KWM  allocate( SNOFLXX  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( SNOEVPX  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( FX       (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( RESX     (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( DRIP2D   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( DEWFALL  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( SOILMX   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( EMISS    (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( XLAI2D   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( GVFMIN   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( GVFMAX   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( TBOT_2D  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( SNOTIME  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( EMBRD2D  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( SNOALB2D (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
  allocate( NOAHRES  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
!KWM  allocate( ETPNDX   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
!KWM  allocate( SFCHEAD  (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
!KWM  allocate( INFXS1   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
!KWM  allocate( PDDUM2   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
!KWM  allocate( PCPDRP   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
!KWM  allocate( SFCWATR2 (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )

  allocate( SMC   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND,NSOIL) ) 
  allocate( STC   (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND,NSOIL) )
  allocate( SH2OX (PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND,NSOIL) )
  allocate( ZSOILX(PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND,NSOIL) )
  allocate( SMAV2D(PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND,NSOIL) )

  allocate( SH2O   (NSOIL) )
  allocate( SLDPTH (NSOIL) )
  allocate( SMC1   (NSOIL) )
  allocate( STC1   (NSOIL) )
  allocate( ET     (NSOIL) )
  allocate( SMAV   (NSOIL) )

#ifdef _HRLDAS_URBAN_
  call urban_param_init(DZR,DZB,DZG,num_roof_layers, sf_urban_physics)
  ALLOCATE(TR_URB2D  ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(TB_URB2D  ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(TG_URB2D  ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(TC_URB2D  ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(QC_URB2D  ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(UC_URB2D  ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(XXXR_URB2D( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(XXXB_URB2D( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(XXXG_URB2D( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(XXXC_URB2D( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(SH_URB2D  ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(LH_URB2D  ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(G_URB2D   ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(RN_URB2D  ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(TS_URB2D  ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(TRL_URB3D ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND, 1:NUM_ROOF_LAYERS ) )
  ALLOCATE(TBL_URB3D ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND, 1:NUM_WALL_LAYERS ) )
  ALLOCATE(TGL_URB3D ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND, 1:NUM_ROAD_LAYERS ) )
  ALLOCATE(PSIM_URB2D   ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(PSIH_URB2D   ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(GZ1OZ0_URB2D ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(U10_URB2D    ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(V10_URB2D    ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(TH2_URB2D    ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(Q2_URB2D     ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(AKMS_URB2D   ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(UST_URB2D    ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(FRC_URB2D    ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(UTYPE_URB2D  ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ! BEP:
  ALLOCATE(TRB_URB4D    ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(TW1_URB4D    ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(TW2_URB4D    ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(TGB_URB4D    ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(SFW1_URB3D   ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(SFW2_URB3D   ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(SFR_URB3D    ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(SFG_URB3D    ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )

  ALLOCATE(A_U_BEP      ( PARALLEL_XSTART:PARALLEL_XEND, 1, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(A_V_BEP      ( PARALLEL_XSTART:PARALLEL_XEND, 1, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(A_T_BEP      ( PARALLEL_XSTART:PARALLEL_XEND, 1, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(A_Q_BEP      ( PARALLEL_XSTART:PARALLEL_XEND, 1, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(A_E_BEP      ( PARALLEL_XSTART:PARALLEL_XEND, 1, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(B_U_BEP      ( PARALLEL_XSTART:PARALLEL_XEND, 1, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(B_V_BEP      ( PARALLEL_XSTART:PARALLEL_XEND, 1, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(B_T_BEP      ( PARALLEL_XSTART:PARALLEL_XEND, 1, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(B_Q_BEP      ( PARALLEL_XSTART:PARALLEL_XEND, 1, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(B_E_BEP      ( PARALLEL_XSTART:PARALLEL_XEND, 1, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(VL_BEP       ( PARALLEL_XSTART:PARALLEL_XEND, 1, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(DLG_BEP      ( PARALLEL_XSTART:PARALLEL_XEND, 1, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(SF_BEP       ( PARALLEL_XSTART:PARALLEL_XEND, 1, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(DL_U_BEP     ( PARALLEL_XSTART:PARALLEL_XEND, 1, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(TLEV_URB3D   ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(QLEV_URB3D   ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(TW1LEV_URB3D ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(TW2LEV_URB3D ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(TGLEV_URB3D  ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(TFLEV_URB3D  ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(SFWIN1_URB3D ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(SFWIN2_URB3D ( PARALLEL_XSTART:PARALLEL_XEND, 1:NUM_URBAN_LAYERS, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(LF_AC_URB3D  ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(SF_AC_URB3D  ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(CM_AC_URB3D  ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(SFVENT_URB3D ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) )
  ALLOCATE(LFVENT_URB3D ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) ) 
  ALLOCATE(CMR_URB2D ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) ) 
  ALLOCATE(CHR_URB2D ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) ) 
  ALLOCATE(CMC_URB2D ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) ) 
  ALLOCATE(CHC_URB2D ( PARALLEL_XSTART:PARALLEL_XEND, SUBWINDOW_YSTART:SUBWINDOW_YEND ) ) 
  CMR_URB2D = 0.1
  CHR_URB2D = 0.1
  CMC_URB2D = 0.1
  CHC_URB2D = 0.1
#endif

!----------------------------------------------------------------------
! Initialize gridded domain
!----------------------------------------------------------------------

  ! SLDPTH is the thickness of each layer
  SLDPTH(1) = -ZSOIL(1)
  do i = 2, nsoil
     sldpth(i) = zsoil(i-1)-zsoil(i)
  enddo

  ETAX=0.0 ! -999.9
  ETAKIN=0.0 ! -999.9
  ETPX=-999.9
  CHX=-999.9
  CMX = -999.0
  FX=-999.9
  HX=-999.9
  RESX=-999.9
  GRDFLX=-999.9
  CMC=0.0
  T2=-999.9
  T1=-999.9
  PRCP1=0
  PRCP_old=0

  RUNOFF1X=0.0 
  RUNOFF2X=0.0 
  RUNOFF3X=0.0 
  EDIRX=0.0
  ETTX=0.0
!FC  ETPNDX=-999.9
  SNOEVPX=-999.9
  SNODEP=-999.9
  SNODEP = 0 
  WEASD = 0 


  STC=-999.9
  SH2OX=-999.9
  SMC=-999.9

  ECX=0.0
  ACSNOM  = 0.0
  ACRAIN  = 0.0
  ESNOW2D = 0.0
  DRIP2D = 0.0
  SNOTIME = 0.0
  DEWFALL = 0.0
  SOILMX = -999.
  XLAI2D = 0.
  GVFMIN = -999.
  GVFMAX = -999.
 
  LAI = 0.0

!---------------------------------------------------------------------
! Initialize static surface data
!---------------------------------------------------------------------


!   OK     VEGTYP     VEGETATION TYPE (INTEGER INDEX)
!   OK     SOILTYP    SOIL TYPE (INTEGER INDEX)
!   OK     SLOPETYP   CLASS OF SFC SLOPE (INTEGER INDEX)
!          SHDMIN     MINIMUM AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
!                (FRACTION= 0.0-1.0) <= SHDFAC
!   N/U    PTU        PHOTO THERMAL UNIT (PLANT PHENOLOGY FOR ANNUALS/CROPS)
!                (NOT YET USED, BUT PASSED TO REDPRM FOR FUTURE USE IN
!                VEG PARMS)
!   OK     ALB        BACKROUND SNOW-FREE SURFACE ALBEDO (FRACTION), FOR JULIAN
!                DAY OF YEAR (USUALLY FROM TEMPORAL INTERPOLATION OF
!                MONTHLY MEAN VALUES' CALLING PROG MAY OR MAY NOT
!                INCLUDE DIURNAL SUN ANGLE EFFECT)
!   OK     SNOALB     UPPER BOUND ON MAXIMUM ALBEDO OVER DEEP SNOW (E.G. FROM
!                ROBINSON AND KUKLA, 1985, J. CLIM. & APPL. METEOR.)
!   OK     TBOT       BOTTOM SOIL TEMPERATURE (LOCAL YEARLY-MEAN SFC AIR
!                TEMPERATURE)
!   ??     Z0BRD      Background fixed roughness length (M)
!   ??     Z0         Time varying roughness length (M) as function of snow depth




!----------------------------------------------------------------------
! Read Landuse Type and Soil Texture and Other Information
!----------------------------------------------------------------------

  CALL READLAND_HRLDAS(hrldas_constants_file,                           &
       parallel_xstart, parallel_xend,                                  &
       subwindow_ystart, subwindow_yend,                                &
       ISWATER, VEGTYP, SOLTYP, TERRAIN, TBOT_2D, LATITUDE, LONGITUDE,  &
		      refdk2d, refkdt2d)

!----------------------------------------------------------------------
! Initialize Model State
!----------------------------------------------------------------------

  SLOPETYP = 2

  call myjsfcinit()

  if (restart_filename_requested /= " ") then

     restart_flag = .TRUE.

     call find_restart_file(rank, trim(restart_filename_requested), startdate, khour, olddate, restart_flnm)

     call read_restart(trim(restart_flnm), sf_urban_physics, iz0tlnd, sfcdif_option,                        &
          parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, nsoil, olddate)

     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "SOIL_T"  , stc      )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "SOIL_M"  , smc      )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "SOIL_W"  , sh2ox    )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "QFX"     , qfx      )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "SNODEP"  , snodep   )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "WEASD"   , weasd    )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "FDOWN"   , fx       )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "ETP"     , etpx     )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "SKINTEMP", t1       )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "ETA"     , etax     )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "ETAKIN"  , etakin   )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "CANWAT"  , cmc      )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "GRDFLX"  , grdflx   )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "CH"      , chx      )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "RUNOFF1" , runoff1x )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "RUNOFF2" , runoff2x )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "RUNOFF3" , runoff3x )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "EDIR"    , edirx    )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "EC"      , ecx      )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "ETT"     , ettx     )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "RC"      , rcx      )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "HFX"     , hx       )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "SMCMAX"  , smcmax1  )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "SMCREF"  , smcref1  )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "SMCWLT"  , smcwlt1  )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "ALBED"   , albedx   )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "ACRAIN"  , acrain   )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "ACSNOM"  , acsnom   )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "ESNOW"   , esnow2d  )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "DRIP"    , drip2d   )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "DEWFALL" , dewfall  )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "SOIL_MX" , soilmx   )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "EMISS"   , emiss    )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "FPAR"    , fpar     )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "ZNT"     , znt      )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "GVFMIN"  , gvfmin   )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "GVFMAX"  , gvfmax   )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "SNOTIME" , snotime  )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "Q12D"    , q12d     )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "CHX"     , chx      )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "CMX"     , cmx      )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "LAI"   , lai   )

     allocate(fpar_save(PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "FPAR_SAVE", fpar_save, ierr)
     if (ierr /= 0) deallocate(fpar_save)

     allocate(lai_save(PARALLEL_XSTART:PARALLEL_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
     call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "LAI_SAVE",  lai_save,  ierr)
     if (ierr /= 0) deallocate(lai_save)

#ifdef _HRLDAS_URBAN_
     if (sf_urban_physics == 1) then
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "TS_URB2D"   , ts_urb2d   )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "TR_URB2D"   , tr_urb2d   )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "TB_URB2D"   , tb_urb2d   )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "TG_URB2D"   , tg_urb2d   )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "TC_URB2D"   , tc_urb2d   )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "QC_URB2D"   , qc_urb2d   )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "UC_URB2D"   , uc_urb2d   )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "TRL_URB3D"  , trl_urb3d  )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "TBL_URB3D"  , tbl_urb3d  )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "TGL_URB3D"  , tgl_urb3d  )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "XXXR_URB2D" , xxxr_urb2d )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "XXXB_URB2D" , xxxb_urb2d )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "XXXG_URB2D" , xxxg_urb2d )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "XXXC_URB2D" , xxxc_urb2d )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "FRC_URB2D"  , frc_urb2d  )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "UTYPE_URB2D", utype_urb2d)
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "CMR_URB2D"  , cmr_urb2d  )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "CHR_URB2D"  , chr_urb2d  )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "CMC_URB2D"  , cmc_urb2d  )
        call get_from_restart(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, "CHC_URB2D"  , chc_urb2d  )
     endif
#endif

  else

     restart_flag = .FALSE.
     inflnm = trim(indir)//"/"//&
          startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
          ".LDASIN_DOMAIN"//hgrid

! only when forc_typ .eq. 1 or 2, HRLDAS_ini_typ can be set as 1
  if(forc_typ .gt. 2)  HRLDAS_ini_typ = 0

  if(HRLDAS_ini_typ .eq. 1) then
     ! read initial parameters and conditions from the HRLDAS forcing data
     if(forc_typ .eq. 2) then
          inflnm = trim(indir)//"/"//&
          startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
          startdate(15:16)//".LDASIN_DOMAIN"//hgrid
     else 
          inflnm = trim(indir)//"/"//&
          startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
          ".LDASIN_DOMAIN"//hgrid
     endif
     CALL READINIT_HRLDAS(inflnm, &
          parallel_xstart, parallel_xend, subwindow_ystart, subwindow_yend,  &
          NSOIL, SLDPTH, OLDDATE, LDASIN_VERSION, SMC, STC, SH2OX, CMC, T1, &
          WEASD, SNODEP)
     ! *** Read lai, fveg etc.  


     CALL READVEG_HRLDAS(inflnm, &
          parallel_xstart, parallel_xend, subwindow_ystart, subwindow_yend,  &
          OLDDATE, VEGTYP, FPAR, LAI, GVFMIN, GVFMAX)

  else
    call HYDRO_HRLDAS_ini(trim(hrldas_constants_file),&
       parallel_xend-parallel_xstart+1,subwindow_yend-subwindow_ystart+1, &
       nsoil,smc,stc,sh2ox, cmc, t1, weasd, snodep,lai, fpar,vegtyp,GVFMIN,GVFMAX,FNDSNOWH)
  endif

     SOILMX = -1.0 * SMC (:,:,1)* ZSOIL (1)
     DO K = 2,NSOIL
        where (SMC(:,:,K) > -1.E25)
           SOILMX = SOILMX + SMC(:,:,K) * (ZSOIL (K-1) - ZSOIL (K))
        elsewhere
           SOILMX = -1.E36
        endwhere

     ENDDO
     where (SOILMX > -1.E25) SOILMX = SOILMX * 1.E3 ! Convert from m to mm

     HX = 0.0
     QFX = 0.0
     ZNT = -1.0

  endif

!yw added this for restart from different restart date
  olddate = startdate
    call getHydroNameList("GWSPINCYCLES", gwCycle)
    call getHydroNameList("GWBASESWCRT", GWBASESWCRT)
    call getHydroNameList("GWSOILCPL", gwsoilcpl)

#ifdef _HRLDAS_URBAN_
  if (sf_urban_physics == 1) then
     ! Initialize urban stuff.
     if (.not. restart_flag) then
        write(*, '("Initialize urban var.")')
        call urban_var_init(ISURBAN, T1, STC, STC(:,:,4),VEGTYP,                              &
             PARALLEL_XSTART, PARALLEL_XEND, SUBWINDOW_YSTART, SUBWINDOW_YEND, 1, 1, NSOIL, &
             restart_flag,sf_urban_physics,                                                   &
             XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,                               & ! inout ! OUT ???
             TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,                              & ! inout ! OUT ???
             TRL_URB3D,TBL_URB3D,TGL_URB3D,                                             & ! inout ! OUT ???
             SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D,                                        & ! inout ! OUT ???
             TS_URB2D,                                                     &
             num_urban_layers,                             & ! in
             TRB_URB4D,TW1_URB4D,TW2_URB4D,TGB_URB4D,      & ! inout
             TLEV_URB3D,QLEV_URB3D,                        & ! inout
             TW1LEV_URB3D,TW2LEV_URB3D,                    & ! inout
             TGLEV_URB3D,TFLEV_URB3D,                      & ! inout
             SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,          & ! inout
             SFVENT_URB3D,LFVENT_URB3D,                    & ! inout
             SFWIN1_URB3D,SFWIN2_URB3D,                    & ! inout
             SFW1_URB3D,SFW2_URB3D,SFR_URB3D,SFG_URB3D,    & ! inout
             A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,              & ! inout multi-layer urban
             A_E_BEP,B_U_BEP,B_V_BEP,                      & ! inout multi-layer urban
             B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,              & ! inout multi-layer urban
             DL_U_BEP,SF_BEP,VL_BEP,                       & ! inout multi-layer urban
             FRC_URB2D, UTYPE_URB2D)                                            ! inout ! OUT ???
     else
        write(*, '("Restart, so we do not initialize urban var")')
     endif
  endif
#endif

!------------------------------------------------------------------------
! Begin Time Loop
!------------------------------------------------------------------------

  NTIME=(KHOUR+1)*3600./nint(dt)
  if ( rank == 0 ) then
     ! Start a timer
     call system_clock(count=clock_count_1)
  endif


 endif  ! end of rank if block

#ifdef MPP_LAND
   call mpi_bcast(ix,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
   call mpi_bcast(jx,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
   call mpi_bcast(nsoil,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
   call mpi_bcast(ntime,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
#endif

  print*, "ix =, jx =", ix,jx
  allocate( infxsrt   (ix,jx) )
  allocate( sfcheadrt   (ix,jx) )
  allocate( soldrain    (ix,jx) )
  allocate( etpnd   (ix,jx) )
  allocate( greenfrac   (ix,jx) )

  allocate ( qsgw (ix,jx))

  if(rank .ne. 0) then
    allocate(smc (ix,jx,nsoil))
    allocate(stc (ix,jx,nsoil))
    allocate(sh2ox (ix,jx,nsoil))
    smc = 0.0
    stc = 0.0 
    sh2ox = 0.0
  else
    greenfrac = 0.0
!yw    call get_greenfrac(trim(GEO_STATIC_FLNM),greenfrac, ix, jx, olddate)
  endif 
    sfcheadrt =0.0
    infxsrt = 0.0
    etpnd = 0.0
    soldrain = 0.0
    qsgw = 0.0

  call hrldas_drv_HYDRO_ini(STC(:,:,1:NSOIL),SMC(:,:,1:NSOIL),SH2OX(:,:,1:NSOIL),infxsrt,sfcheadrt,soldrain,ix,jx,NSOIL, 1, dt, olddate,zsoil(1:NSOIL))

1002   continue

  KLOOP : DO K=1,NTIME
    if(rank == 0) print*, "k, NTIME, dt = ",k,NTIME , dt

!--------------------------------------------------------------------------------
! Output for restart BEFORE the processing for this particular time has begun, 
! so that upon restart, we're ready to go on this time step.
!--------------------------------------------------------------------------------

  if(forc_typ .eq. 8 ) then
       if(rank == 0) then
          call read_forc_ldasout_seq(olddate,hgrid, indir, FORCING_TIMESTEP, ix,jx,infxsrt,soldrain)
       endif

          call hrldas_drv_HYDRO(STC(:,:,1:NSOIL),SMC(:,:,1:NSOIL),SH2OX(:,:,1:NSOIL), &
                        infxsrt,sfcheadrt,soldrain,ix,jx,NSOIL,               &
			qsgw)
          goto 1003 
  endif

  if(rank .eq. 0) then

!---------------------------------------------------------------------------------
! Read the forcing data.
!---------------------------------------------------------------------------------

! For HRLDAS, we're assuming (for now) that each time period is in a 
! separate file.  So we can open a new one right now.

     inflnm = trim(indir)//"/"//&
          olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
          ".LDASIN_DOMAIN"//hgrid

     ! Build a filename template
     inflnm_template = trim(indir)//"/<date>.LDASIN_DOMAIN"//hgrid

!yw     CALL READFORC_HRLDAS(inflnm_template, forcing_timestep, olddate,  &
!yw          parallel_xstart, parallel_xend, subwindow_ystart, subwindow_yend,    &
!yw          T2,Q2X,U,V,PRES,XLONG,SHORT,PRCP1,LAI,FPAR)

     if(olddate == forcDate) then
        CALL HYDRO_forcing_drv(trim(indir), forc_typ,snow_assim,olddate,                      &
          parallel_xstart, parallel_xend, subwindow_ystart, subwindow_yend,    &
          T2,Q2X,U,V,PRES,XLONG,SHORT,PRCP1,lai,fpar,snodep,k,FORCING_TIMESTEP,prcp_old)

        call geth_newdate(newdate, forcDate, FORCING_TIMESTEP)
        forcDate = newdate
     endif


!----------------------------------------------------------------------------------
! Read possible external maps.  There's got to be some good way to make this
! more general.
!----------------------------------------------------------------------------------

     if (external_fpar_filename_template /= " ") then
        call read_additional(external_fpar_filename_template, olddate, "fPAR", &
             subwindow_xstart, subwindow_xend, subwindow_ystart, subwindow_yend, FPAR, ierr)
        if (ierr == 0) then
           if ( .not. allocated(FPAR_SAVE)) then
              allocate( FPAR_SAVE (SUBWINDOW_XSTART:SUBWINDOW_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
           endif
           FPAR_SAVE = FPAR
        else
           if (allocated(FPAR_SAVE)) then
              FPAR = FPAR_SAVE
           endif
        endif
     endif

     if (external_lai_filename_template /= " ") then
        RDLAI2D = .TRUE.
        call read_additional(external_lai_filename_template,  olddate, "LAI",  &
             subwindow_xstart, subwindow_xend, subwindow_ystart, subwindow_yend, LAI, ierr)
        if (ierr == 0) then
           if ( .not. allocated(LAI_SAVE)) then
              allocate( LAI_SAVE (SUBWINDOW_XSTART:SUBWINDOW_XEND,SUBWINDOW_YSTART:SUBWINDOW_YEND) )
           endif
           LAI_SAVE = LAI
        else
           if (allocated(LAI_SAVE)) then
              LAI = LAI_SAVE
           endif
        endif
     endif

     if ( ( K == 1 ) .and. ( .not. restart_flag ) ) then

        ! Initial values of Q1, the Z0-level specific humidity, are taken from the ZLVL-level 
        ! values.  Subsequent Q1 values are remembered from the previous time step Q1 values
        ! as recomputed by SFLX.
        Q12D = Q2X/(1.0+Q2X) ! Convert mixing ratio to specific humidity
        if ( SFCDIF_OPTION == 1 ) THEN
           CHX = 0.0001
           CMX = 0.0001
        endif
     endif


!------------------------------------------------------------------------
! Spatial Loop to Convert Gridded data to single point for 1-D SFLX call
!------------------------------------------------------------------------

     JLOOP : DO J=subwindow_ystart,subwindow_yend
        ILOOP : DO I=parallel_xstart, parallel_xend
!yw           if (gvfmin(i,j) < -1.E25) cycle ILOOP
           if(abs(u(i,j)) .lt. 200 .and. abs(v(i,j)) .lt. 200) then   !! do not run for missing data

           IF((VEGTYP(I,J).GT.0).AND.(VEGTYP(I,J).NE.ISWATER)) THEN
              VEGTYPX=VEGTYP(I,J)
              SOILTYP=SOLTYP(I,J)
              SFCTMP=T2(I,J)
              SFCSPD=SQRT(U(I,J)*U(I,J)+V(I,J)*V(I,J))
              SFCPRS=PRES(I,J)
              Q2=Q2X(I,J)/(1.0+Q2X(I,J)) ! Convert mixing ratio to specific humidity
              PRCP=PRCP1(I,J)
              if (prcp < 0.0) prcp = 0.0 ! Just in case
              SOLDN=SHORT(I,J)
              Q1 = Q12D(I,J)

              !
              ! The following module variables are set in subroutine REDPRM.
              ! If you want other than the default values based on land/soil
              ! categories, reset them after the call to REDPRM.
              !
              !    -- SOIL PARAMETERS:
              !
              !           CSOIL
              !           BEXP
              !           DKSAT
              !           DWSAT
              !           F1
              !           PSISAT
              !           QUARTZ
              !           SMCDRY
              !           SMCMAX
              !           SMCREF
              !           SMCWLT
              !
              !    -- "UNIVERSAL" PARAMETERS:
              !
              !           ZBOT
              !           SALP
              !           SBETA
              !           REFDK
              !           FRZK
              !           FXEXP
              !           REFKDT
              !           PTU
              !           KDT
              !           CZIL
              !           SLOPE
              !           FRZFACT
              ! 
              !    -- VEGETATION PARAMETERS:
              !
              !           TOPT
              !           RGL
              !           RSMAX
              !           RSMIN
              !           HS
              !           XLAI
              !           CMCMAX
              !           CFACTR
              !           NROOT
              !           SNUP
              !           ALB
              !           Z0BRD
              !           SHDFAC
              !           SNOALB_NOAH
              !           RTDIS
              !

#ifdef _HRLDAS_URBAN_
              IF ( SF_URBAN_PHYSICS == 1 ) THEN
                 IF ( ( VEGTYP(I,J) == ISURBAN  ) .or. ( VEGTYP(I,J) == 31 ) .or. &
                      ( VEGTYP(I,J) == 32 ) .or. ( VEGTYP(I,J) == 33 ) ) THEN
                    ! VEGTYPX = 5   ! HARD WIRED CATEGORY IS A PROBLEM.  NEEDS CORRECTION
                    VEGTYPX = 10   ! HARD WIRED CATEGORY IS A PROBLEM.  NEEDS CORRECTION
                 ENDIF
              ELSE
                 IF ( ( VEGTYP(I,J) == ISURBAN  ) .or. ( VEGTYP(I,J) == 31 ) .or. &
                      ( VEGTYP(I,J) == 32 ) .or. ( VEGTYP(I,J) == 33 ) ) THEN
                    VEGTYPX = ISURBAN
           	 ENDIF
              ENDIF
#endif

              CALL REDPRM (VEGTYPX,SOILTYP,SLOPETYP,CFACTR,CMCMAX,RSMAX,TOPT,   &
                   REFKDT,KDT,SBETA, SHDFAC,RSMIN,RGL,HS,ZBOT,FRZX,            &
                   PSISAT,SLOPE,SNUP,SALP,BEXP,DKSAT,DWSAT,                    &
                   SMCMAX,SMCWLT,SMCREF,SMCDRY,F1,QUARTZ,FXEXP,                &
                   RTDIS,SLDPTH,ZSOIL,NROOT,NSOIL,CZIL,                        &
                   LAIMIN,LAIMAX,EMISSMIN,EMISSMAX,ALBEDOMIN,ALBEDOMAX,        &
                   Z0MIN,Z0MAXnoah,CSOIL,PTU,LLANDUSE,LSOIL,.True.,LVCOEF)

              offline_cfactr = cfactr
              offline_cmcmax = cmcmax
              offline_rsmax  = rsmax
              offline_topt   = topt
              offline_refkdt = refkdt
              offline_kdt    = kdt
              offline_sbeta  = sbeta
              offline_rsmin  = rsmin
              offline_rgl    = rgl
              offline_hs     = hs
              offline_zbot   = zbot
              offline_frzx   = frzx
              offline_psisat = psisat
              offline_slope  = slope
              offline_snup   = snup
              offline_salp   = salp
              offline_bexp   = bexp
              offline_dksat  = dksat
              offline_dwsat  = dwsat
              offline_smcmax = smcmax
              offline_smcwlt = smcwlt
              offline_smcref = smcref
              offline_smcdry = smcdry
              offline_f1     = f1
              offline_quartz = quartz
              offline_fxexp  = fxexp
              offline_rtdis  = rtdis
              offline_nroot  = nroot
              offline_czil   = czil
              offline_csoil  = csoil
              offline_ptu    = ptu
              offline_lvcoef = lvcoef

              SHDFAC = FPAR(I,J)
              if (VEGTYPX==BARE) SHDFAC=0.0; !KWM Added 20060920 to be consistent with REDPRM
              IF(VEGTYPX==ISURBAN)THEN
                 !urban change
                 SHDFAC=0.05
                 RSMIN=400.0
                 SMCMAX = 0.45
                 SMCREF = 0.42
                 SMCWLT = 0.40
              ENDIF


              shdmin = gvfmin(i,j)
              shdmax = gvfmax(i,j)

              if (gvfmax(i,j) == gvfmin(i,j)) then
                 fraction = 0.0
              else
                    fraction = real((dble(SHDFAC)-dble(GVFMIN(I,J)))/(dble(GVFMAX(I,J))-dble(GVFMIN(I,J))))
!                 fraction = greenfrac(i,j)
              endif

              fraction = max(fraction,0.)
              fraction = min(fraction,1.)
              if ( ( k==1 ) .and. ( .not. restart_flag ) ) then
                 !
                 !  First time step, initialize EMISS from reasonable background values.  Subsequent
                 !  time steps will use the EMISS value calculcated in the previous time step.  This 
                 !  means that EMISS has to go into the RESTART file.
                 !
                 emiss(i,j)  = real ( dble(emissmintbl(vegtypx))+dble(fraction)*(dble(emissmaxtbl(vegtypx))-dble(emissmintbl(vegtypx))) )
              endif
              z0brd  =  real( dble(z0mintbl    (vegtypx))+dble(fraction)*( dble(z0maxtbl     (vegtypx)) - dble(z0mintbl    (vegtypx))) )
              albbrd =  real( dble(albedomaxtbl(vegtypx))+dble(fraction)*( dble(albedomintbl (vegtypx)) - dble(albedomaxtbl(vegtypx))) )

              if (allocated(LAI_SAVE)) then
                 xlai = LAI(I,J) !EXTERNAL
                 if (xlai==0) xlai=0.05
              else
                 xlai   = real(dble(laimintbl(vegtypx))+dble(fraction)*(dble(laimaxtbl(vegtypx))-dble(laimintbl(vegtypx))))
              endif

              !
              ! ALBEDX is our 2D albedo map, remembered from the previous timestep through SFLX.
              ! For the first time step, just use ALBBRD.
              !

              if ( ( k==1 ) .and. ( .not. restart_flag ) ) then
                 albedx(i,j) = albbrd
              endif

              LWDN = XLONG(I,J) * EMISS(I,J)

              !   snow albedo
              alb = albbrd !?
              SNOFAC=MIN(SNODEP(I,J)*5.0, 1.0)

              !   Net downward radiation
              SOLNET=SOLDN*(1.-ALBEDX(I,J))

              IF ( SFCDIF_OPTION == 0 ) THEN
                 CH = 0.1
                 CM = 0.1
              ELSE IF ( SFCDIF_OPTION == 1 ) THEN
                 CH = CHX(I,J)
                 CM = CMX(I,J)
              ENDIF

              if (ZNT(I,J) < 0) then
                 Z0 = Z0BRD
              else
                 Z0 = ZNT(I,J)
              endif

!     Calculate a saturation specific humidity and slope of saturation specific humidity curve WRT Temperature

              CALL CALTMP(T1(I,J), SFCTMP, SFCPRS, ZLVL, Q2, TH2, T1V, TH2V, RHO)
              CALL CALHUM(SFCTMP, SFCPRS, Q2SAT, DQSDT2)

!     Calculate the surface exchange coefficients CM, CH
              IF ( SFCDIF_OPTION == 0 ) then
                 ! Intent (IN) :: ZLVL, Z0, T1V, TH2V, SFCSPD, CZIL, VEGTYPX, ISURBAN, IZ0TLND
                 ! Intent (INOUT) :: CM
                 CALL SFCDIF_OFF (ZLVL, ZLVL_WIND, Z0, T1V, TH2V, SFCSPD, CZIL, CM, CH, &
                      VEGTYPX, ISURBAN, IZ0TLND )
              ELSE IF ( SFCDIF_OPTION == 1 ) then
                 ! Intent (IN) :: ZLVL, Z0, Z0BRD, SFCPRS, T1, SFCTMP, Q1, Q2, SFCSPD, VEGTYP, ISURBAN, IZ0TLND
                 ! intent (OUT) :: RIBB
                 ! Intent (INOUT) :: CM, CH
                 CALL SFCDIF_MYJ ( ZLVL, ZLVL_WIND, Z0, Z0BRD, SFCPRS, T1(I,J), SFCTMP, Q1, Q2, &
                      SFCSPD, CZIL, RIBB, CM, CH, VEGTYPX, ISURBAN, IZ0TLND )
              ENDIF
              IF (Q2 .LT. 0.0) THEN
                 print*,'Q2<0','I=',I, 'J=',J
                 Q2=.1E-5
              ENDIF
              IF (Q2 .GT. Q2SAT) THEN
                 ! print*, 'Q2 .GT. Q2SAT', 'I=',I, 'J=',J
                 ! print*,'Q2=',Q2,'Q2SAT=',Q2SAT
                 Q2=Q2SAT*0.99
              ENDIF

              CHKFF = CH * CPHEAT * RHO           

              STC1(1:NSOIL)=STC(I,J,1:NSOIL)

              SMC1(1:NSOIL)=SMC(I,J,1:NSOIL)
              SH2O(1:NSOIL)=SH2OX(I,J,1:NSOIL)
              ZSOILX(I,J,1:NSOIL)=ZSOIL(1:NSOIL)
! *** diagnostics 
              SFCSPDX(I,J)=SFCSPD


!--- Other conversions 'History (State) Variables

              TBOT=TBOT_2D(I,J)
              CMCX=CMC(I,J)
              T1X=T1(I,J)
              SNOWH=SNODEP(I,J)

              SNEQV = WEASD(I,J)

              esnow = 0.0
              if (T1X <= 273.15) then
                 ffrozp = 1.0
              else
                 ffrozp = 0.0
              endif

#ifdef _HRLDAS_URBAN_
              IF ( SF_URBAN_PHYSICS == 1 ) THEN
                 IF ( ( VEGTYP(I,J) == ISURBAN  ) .or. ( VEGTYP(I,J) == 31 ) .or. &
                      ( VEGTYP(I,J) == 32 ) .or. ( VEGTYP(I,J) == 33 ) ) THEN
                    ! VEGTYPX = 5   ! HARD WIRED CATEGORY IS A PROBLEM.  NEEDS CORRECTION
                    VEGTYPX = 10   ! HARD WIRED CATEGORY IS A PROBLEM.  NEEDS CORRECTION
                    SHDFAC = 0.8 
                    ALBBRD  =0.2
                    T1X= ( T1(I,J) -FRC_URB2D(I,J) * TS_URB2D (I,J) )/ (1-FRC_URB2D(I,J))
                 ENDIF
              ELSE
                 IF ( ( VEGTYP(I,J) == ISURBAN  ) .or. ( VEGTYP(I,J) == 31 ) .or. &
                      ( VEGTYP(I,J) == 32 ) .or. ( VEGTYP(I,J) == 33 ) ) THEN
                    VEGTYPX = ISURBAN
           	 ENDIF
              ENDIF
#endif



              ! The following variables are available here, to subroutine 
              ! SFLX, and to its deeper subroutines via use-association with 
              ! module noahlsm_globals.
              ! 
              !    ALB   : Surface albedo (Fraction 0.0 to 1.0)
              !    Z0BRD : Roughness Length (m)
              !    SHDFAC: Green Vegetation Fraction
              !          : Areal fractional coverage of green vegetation (fraction: 0.0-1.0)
              !    NROOT : Rooting depth (as the number of layers)
              !    RSMIN : Minimum Canopy Resistance (s m-1)
              !    RGL   : Parameter used in radiation stress function
              !    HS    : Parameter used in vapor pressure deficit function
              !    SNUP  : Threshold snow depth (in water equivalent m) that
              !            implies 100 percent snow cover
              !    XLAI  : Leaf Area Index
              !    SNOALB_NOAH: Upper bound on maximum albedo over deep snow (e.g., from
              !            Robinson and Kukla, 1985, J. Clim. & Appl. Meteor.)
              !    TOPT  : Optimum transpiration air temperature.
              !    CMCMAX: Maximum canopy water capacity
              !    CFACTR: Parameter used in the canopy inteception calculation
              !    RSMAX : Maximum stomatal resistance
              !    PTU   : Photo thermal unit (plant phenology for annuals/crops)
              !
              !    BEXP  : B parameter
              !    SMCDRY: Dry soil moisture threshold where direct evap from top 
              !            layer ends (volumetric)
              !    F1    : Soil thermal diffusivity/conductivity coef.
              !    SMCMAX: Porosity, i.e. saturated value of soil moisture (volumetric)
              !             -- Max soil moisture content (porosity)
              !             -- Saturation soil moisture content (from REDPRM)
              !    SMCREF: Soil moisture threshold where transpiration begins to 
              !            stress (volumetric)
              !             -- Reference soil moisture  (field capacity)
              !             -- Reference soil moisture (where soil water 
              !                deficit stress sets in)
              !    PSISAT: Saturated soil matric potential
              !    DKSAT : Saturated soil conductivity
              !    DWSAT : 
              !    SMCWLT: Wilting point soil moisture (volumetric)
              !    QUARTZ: Soil quartz content
              !
              !    SLOPE
              !    SBETA
              !    FXEXP
              !    CSOIL
              !    SALP  : Tuning parameter
              !    REFDK : Parameter in the surface runoff parameterization
              !    REFKDT: Parameter in the surface runoff parameterization
              !    FRZFACT: Frozen ground parameter
              !    ZBOT  : Depth (m) of lower boundary soil temperature
              !    CZIL  : Calculate roughness length of heat
              !    KDT   
              !
              ! Arguments to SFLX are:
              ! ----------------------------------------------------------------------
              ! 1. CONFIGURATION INFORMATION (C):
              ! ----------------------------------------------------------------------
              !    ICE   : Sea-ice flag (1=ice, 0=land)
              !    DT    : Timestep (sec)
              !    ZLVL  : Height (m) above ground of atmospheric forcing variables
              !    NSOIL : Number of soil layers.
              !    SLDPTH: Thickness of each soil layer (m)
              ! ----------------------------------------------------------------------
              ! 3. FORCING DATA (F):
              ! ----------------------------------------------------------------------
              !    LWDN  : LW downward radiation, not net (w m-2)
              !    SOLDN : Solar downward radiation, not net (w m-2)
              !    SFCPRS: Pressure  (Pa) at height ZLVL above ground
              !    PRCP  : Precipitation rate (kg m-2 s-1; or mm/s)
              !    SFCTMP: Air temperature (K) at height ZLVL above ground
              !    Q2    : Specific humidity (kg kg-1) at height ZLVL above ground
              !    SFCSPD: Wind speed (m s-1) at height ZLVL above ground
! UNUSED     !    COSZ  : Solar zenith angle
! UNUSED     !    PRCPRAIN: Liquid-precipitation rate (kg m-2 s-1) (Not used)
! UNUSED     !    SOLARDIRECT: Direct component of downward solar radiation (W m-2) (Not used)
              !    TH2   : Air potential temperature (K) at height ZLVL above ground
              ! ----------------------------------------------------------------------
              ! 4. OTHER FORCING (INPUT) DATA (I):
              ! ----------------------------------------------------------------------
              !    Q2SAT : Saturation specific humidity (kg kg-1) at height ZLVL above ground
              !    DQSDT2: Slope (kg kg-1 K-1) of sat specific humidity curve at T=SFCTMP
              ! ----------------------------------------------------------------------
              ! 5. CANOPY/SOIL CHARACTERISTICS (S):
              ! ----------------------------------------------------------------------
              !   VEGTYPX: Vegetation type (integer index)
              !   SHDMIN : Minimum areal fractional coverage of green vegetation (Fraction= 0.0-1.0)
              !   SHDMAX : Maximum areal fractional coverage of annual green vegetation (Not used)
              !   TBOT   : Bottom soil temperature (K) (local yearly-mean sfc air Temp.
              !   Z0     : Roughness length (m) modifed by snow depth as appropriate
              ! ----------------------------------------------------------------------
              ! 6. HISTORY (STATE) VARIABLES (H):
              ! ----------------------------------------------------------------------
              !   CMCX   : Canopy moisture content (m)
              !   T1X    : Ground/canopy/snowpack effective skin temperature (K)
              !   STC1   : Soil Temperature (K) :: dimension(NSOIL)
              !   SMC1   : Total soil moisture content (volumetric fraction) :: dimension(NSOIL)
              !   SH2O   : Unfrozen soil moisture content (volumetric fraction) :: dimension(NSOIL)
              !            NOTE: Frozen soil moisture = SMC - SH2O
              !   SNOWH  : Actual snow depth (m)
              !   SNEQV  : Liquid water-equivalent snow depth (m).  NOTE: Snow density = SNEQV/SNOWH
              !   ALBEDO : Surface albedo including snow effect (Fraction)
              !   CH     : Surface exchange coefficient for heat and moisture (m s-1)
              !            NOTE: CH is technically a conductance since it has been 
              !            multiplied by wind speed.
! UNUSED     !   CM     : Surface exchange coefficient for momentum (m s-1)
              !            NOTE: CM is technically a conductance since it has been
              !            multiplied by wind speed. (Not used)
              ! ----------------------------------------------------------------------
              ! 7. OUTPUT (O):
              ! ----------------------------------------------------------------------
              !   ETA    : Actual latent heat flux (W m-2)
              !   SHEAT  : Sensible heat flux (W m-2)
              !   ETA_KINEMATIC : Actual latent heat flux (kg m s-1)
              !   FDOWN  : Radiation forcing at the surface (W m-2)
              !   EC     : Canopy water evaporation (W m-2)
              !   EDIR   : Direct soil evaporation (W m-2)
              !   ET     : Plant transpiration from a particular root/soil layer (W m-2) :: dimension(NSOIL)
              !   ETT    : Total plant transpiration (W m-2)
              !   ESNOW  : Sublimation from (or deposition to if ESNOW<0) snowpack (W m-2)
              !   DRIP   : Through-fall of precip and/or dew in excess of canopy water-holding capacity (m)
              !   DEW    : Dewfall (or frostfall for T<273.15) (m)
              !   BETA   : Ratio of actual/potential evaporation (dimensionless)
              !   ETP    : Potential evaporation (W m-2)
              !   SSOIL  : Soil heat flux (W m-2)
              !   FLX1   : Precip-snow sfc (W m-2)
              !   FLX2   : Freezing rain latent heat flux (W m-2)
              !   FLX3   : Phase-change heat flux from snowmelt (W m-2)
              !   SNOMLT : Snow melt (m) (water equivalent)
              !   SNCOVR : Fractional snow cover (Fraction, 0.0 to 1.0)
              !   RUNOFF1: Surface runoff (m s-1), not infiltrating the surface
              !   RUNOFF2: Subsurface runoff (m s-1), drainage out bottom of deepest soil layer (baseflow)
              !   RUNOFF3: Numerical trunctation in excess of porosity
              !            for a given soil layer at the end of a time step (m s-1).
              !            NOTE: the above RUNOFF2 is actually the sum of RUNOFF2 and RUNOFF3
              !   RC     : Canopy resistance (s m-1)
              !   PC     : Plant coefficient (Fraction, 0.0 to 1.0) where PC*ETP = Actual transpiration
              !   RCS    : Incoming solar RC factor (dimensionless)
              !   RCT    : Air temperature RC factor (dimensionless)
              !   RCQ    : Atmos vapor pressure deficit RC factor (dimensionless)
              !   RCSOIL : Soil moisture RC factor (dimensionless)
              ! ----------------------------------------------------------------------
              ! 8. DIAGNOSTIC OUTPUT (D):
              ! ----------------------------------------------------------------------
              !   SOILW  : Available soil moisture in root zone (Fraction, between SMCWLT and SMCMAX)
              !   SOILM  : Total soil column moisture content (frozen+unfrozen) (m)
              !   Q1     : Effective specific humidity at surface (kg kg-1), used for
              !            diagnosing the specific humidity at 2 meter for coupled model

#ifdef _HRLDAS_URBAN_
              if ( sf_urban_physics == 1) then
                 call calc_declin( olddate(1:13), latitude(i,j), longitude(i,j), cosz, omg_urb, declin )
              endif
#endif

              snoalb = MAXALB(VEGTYPX) * 0.01 !?????  albed !?????

              EMBRD = -1.E36

              SNOTIME1 = SNOTIME(I,J)

#define _DEBUG_PRINT_ 0
#if _DEBUG_PRINT_
              print*, "Before SFLX."
              print*, 'FFROZP = ', FFROZP
              print*, 'ICE = ', ICE
              print*, 'ISURBAN = ', ISURBAN
              print*, 'DT = ', DT
              print*, 'ZLVL = ', ZLVL
              print*, 'NSOIL = ', NSOIL
              print*, 'SLDPTH = ', SLDPTH
              print*, 'LLANDUSE = ', trim(LLANDUSE)
              print*, 'LSOIL = ', trim(LSOIL)
              print*, 'LWDN = ', LWDN
              print*, 'SOLDN = ', SOLDN
              print*, 'SOLNET = ', SOLNET
              print*, 'SFCPRS = ', SFCPRS
              print*, 'PRCP = ', PRCP
              print*, 'SFCTMP = ', SFCTMP
              print*, 'Q2 = ', Q2
              print*, 'SFCSPD = ', SFCSPD
              print*, 'COSZ = ', COSZ
              print*, 'PRCPRAIN = ', PRCPRAIN
              print*, 'SOLARDIRECT = ', SOLARDIRECT
              print*, 'TH2 = ', TH2
              print*, 'Q2SAT = ', Q2SAT
              print*, 'DQSDT2 = ', DQSDT2
              print*, 'VEGTYPX = ', VEGTYPX
              print*, 'SOILTYP = ', SOILTYP
              print*, 'SlOPETYP = ', SlOPETYP
              print*, 'SHDFAC = ', SHDFAC
!              print*, 'SHDMIN = ', SHDMIN
!              print*, 'SHDMAX = ', SHDMAX
              print*, 'ALB = ', ALB
              print*, 'SNOALB = ', SNOALB
              print*, 'TBOT = ', TBOT
              print*, 'Z0BRD = ', Z0BRD
              print*, 'Z0 = ', Z0
              print*, 'EMISSI = ', EMISSI
!              print*, 'EMBRD = ', EMBRD
              print*, 'CMCX = ', CMCX
              print*, 'T1X = ', T1X
              print*, 'STC1 = ', STC1
              print*, 'SMC1 = ', SMC1
              print*, 'SH2O = ', SH2O
              print*, 'SNOWH = ', SNOWH
              print*, 'SNEQV = ', SNEQV
              print*, 'ALBEDO = ', ALBEDO
              print*, 'CH = ', CH
              print*, 'CM = ', CM
              print*, 'ETA = ', ETA
              print*, 'SHEAT = ', SHEAT
              print*, 'ETA_KINEMATIC = ', ETA_KINEMATIC
              print*, 'FDOWN = ', FDOWN
              print*, 'EC = ', EC
              print*, 'EDIR = ', EDIR
              print*, 'ET = ', ET
              print*, 'ETT = ', ETT
              print*, 'ESNOW = ', ESNOW
              print*, 'DRIP = ', DRIP
              print*, 'DEW = ', DEW
              print*, 'BETA = ', BETA
              print*, 'ETP = ', ETP
              print*, 'SSOIL = ', SSOIL
              print*, 'FLX1 = ', FLX1
              print*, 'FLX2 = ', FLX2
              print*, 'FLX3 = ', FLX3
              print*, 'SNOMLT = ', SNOMLT
              print*, 'SNCOVR = ', SNCOVR
              print*, 'RUNOFF1 = ', RUNOFF1
              print*, 'RUNOFF2 = ', RUNOFF2
              print*, 'RUNOFF3 = ', RUNOFF3
              print*, 'RC = ', RC
              print*, 'PC = ', PC
              print*, 'RSMIN = ', RSMIN
              write(*, '("XLAI = ", F7.5)') XLAI
              print*, 'RCS = ', RCS
              print*, 'RCT = ', RCT
              print*, 'RCQ = ', RCQ
              print*, 'RCSOIL = ', RCSOIL
              print*, 'SOILW = ', SOILW
              print*, 'SOILM = ', SOILM
              print*, 'Q1 = ', Q1
#if 0
              print*, 'RDLAI2D = ', RDLAI2D
              print*, 'USEMONALB = ', USEMONALB
              print*, 'SNOTIME1 = ', SNOTIME1
              print*, 'RIBB = ', RIBB
#endif
              print*, 'SMCWLT = ', SMCWLT
              print*, 'SMCDRY = ', SMCDRY
              print*, 'SMCREF = ', SMCREF
              print*, 'SMCMAX = ', SMCMAX
              print*, 'NROOT = ', NROOT
#endif

!              print*, 'SNOWH = ', SNOWH, 'i=',i, 'j=',j
!              print*, 'SNEQV = ', SNEQV,'i=',i, 'j=',j

              if(GWBASESWCRT .eq. 3) then              
                  REFDK_DATA  = refdk2d(i,j)
                  REFKDT_DATA = refkdt2d(i,j)
              endif

              CALL SFLX (FFROZP,ICE,ISURBAN,DT,ZLVL,NSOIL,SLDPTH,   &    !C
                   .FALSE., LLANDUSE, LSOIL,                        &    !C
                   LWDN,SOLDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2,SFCSPD,  &    !F
                   COSZ,PRCPRAIN, SOLARDIRECT,                      &    !F
                   TH2,Q2SAT,DQSDT2,                                &    !I
                   VEGTYPX,SOILTYP,SlOPETYP,SHDFAC,SHDMIN,SHDMAX,   &    !S  
                   ALB,SNOALB,TBOT,Z0BRD,Z0,EMISSI, EMBRD,          &    !S   
                   CMCX,T1X,STC1,SMC1,SH2O,SNOWH,SNEQV,ALBEDO,CH,CM,&    !H  
! ----------------------------------------------------------------------         
! OUTPUTS, DIAGNOSTICS, PARAMETERS BELOW GENERALLY NOT NECESSARY WHEN            
! COUPLED WITH E.G. A NWP MODEL (SUCH AS THE NOAA/NWS/NCEP MESOSCALE ETA         
! MODEL).  OTHER APPLICATIONS MAY REQUIRE DIFFERENT OUTPUT VARIABLES.            
! ----------------------------------------------------------------------         
                   ETA,SHEAT, ETA_KINEMATIC,FDOWN,                  &    !O  
                   EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                   &    !O  
                   BETA,ETP,SSOIL,                                  &    !O  
                   FLX1,FLX2,FLX3,                                  &    !O  
                   SNOMLT,SNCOVR,                                   &    !O  
                   RUNOFF1,RUNOFF2,RUNOFF3,                         &    !O  
                   RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,             &    !O  
                   SOILW,SOILM,Q1,SMAV,                             &    !D
                   RDLAI2D, USEMONALB, SNOTIME1, RIBB,              &
                   SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT,               &
                   sfcheadrt(i,j),INFXSRT(i,j),ETPND1,              &
		   gwsoilcpl, qsgw(i,j))           !P  
         

#if _DEBUG_PRINT_
              ETPND(i,j) = ETPND(i,j) + etpnd1
              print*, "After SFLX."
              print*, 'FFROZP = ', FFROZP
              print*, 'ICE = ', ICE
              print*, 'ISURBAN = ', ISURBAN
              print*, 'DT = ', DT
              print*, 'ZLVL = ', ZLVL
              print*, 'NSOIL = ', NSOIL
              print*, 'SLDPTH = ', SLDPTH
              print*, 'LLANDUSE = ', trim(LLANDUSE)
              print*, 'LSOIL = ', trim(LSOIL)
              print*, 'LWDN = ', LWDN
              print*, 'SOLDN = ', SOLDN
              print*, 'SOLNET = ', SOLNET
              print*, 'SFCPRS = ', SFCPRS
              print*, 'PRCP = ', PRCP
              print*, 'SFCTMP = ', SFCTMP
              print*, 'Q2 = ', Q2
              print*, 'SFCSPD = ', SFCSPD
              print*, 'COSZ = ', COSZ
              print*, 'PRCPRAIN = ', PRCPRAIN
              print*, 'SOLARDIRECT = ', SOLARDIRECT
              print*, 'TH2 = ', TH2
              print*, 'Q2SAT = ', Q2SAT
              print*, 'DQSDT2 = ', DQSDT2
              print*, 'VEGTYPX = ', VEGTYPX
              print*, 'SOILTYP = ', SOILTYP
              print*, 'SlOPETYP = ', SlOPETYP
              print*, 'SHDFAC = ', SHDFAC
!              print*, 'SHDMIN = ', SHDMIN
!              print*, 'SHDMAX = ', SHDMAX
              print*, 'ALB = ', ALB
              print*, 'SNOALB = ', SNOALB
              print*, 'TBOT = ', TBOT
              print*, 'Z0BRD = ', Z0BRD
              print*, 'Z0 = ', Z0
              print*, 'EMISSI = ', EMISSI
!              print*, 'EMBRD = ', EMBRD
              print*, 'CMCX = ', CMCX
              print*, 'T1X = ', T1X
              print*, 'STC1 = ', STC1
              print*, 'SMC1 = ', SMC1
              print*, 'SH2O = ', SH2O
              print*, 'SNOWH = ', SNOWH
              print*, 'SNEQV = ', SNEQV
              print*, 'ALBEDO = ', ALBEDO
              print*, 'CH = ', CH
              print*, 'CM = ', CM
              print*, 'ETA = ', ETA
              print*, 'SHEAT = ', SHEAT
              print*, 'ETA_KINEMATIC = ', ETA_KINEMATIC
              print*, 'FDOWN = ', FDOWN
              print*, 'EC = ', EC
              print*, 'EDIR = ', EDIR
              print*, 'ET = ', ET
              print*, 'ETT = ', ETT
              print*, 'ESNOW = ', ESNOW
              print*, 'DRIP = ', DRIP
              print*, 'DEW = ', DEW
              print*, 'BETA = ', BETA
              print*, 'ETP = ', ETP
              print*, 'SSOIL = ', SSOIL
              print*, 'FLX1 = ', FLX1
              print*, 'FLX2 = ', FLX2
              print*, 'FLX3 = ', FLX3
              print*, 'SNOMLT = ', SNOMLT
              print*, 'SNCOVR = ', SNCOVR
              print*, 'RUNOFF1 = ', RUNOFF1
              print*, 'RUNOFF2 = ', RUNOFF2
              print*, 'RUNOFF3 = ', RUNOFF3
              print*, 'RC = ', RC
              print*, 'PC = ', PC
              print*, 'RSMIN = ', RSMIN
              write(*, '("XLAI = ", F7.5)') XLAI
              print*, 'RCS = ', RCS
              print*, 'RCT = ', RCT
              print*, 'RCQ = ', RCQ
              print*, 'RCSOIL = ', RCSOIL
              print*, 'SOILW = ', SOILW
              print*, 'SOILM = ', SOILM
              print*, 'Q1 = ', Q1
#if 0
              print*, 'RDLAI2D = ', RDLAI2D
              print*, 'USEMONALB = ', USEMONALB
              print*, 'SNOTIME1 = ', SNOTIME1
              print*, 'RIBB = ', RIBB
#endif
              print*, 'SMCWLT = ', SMCWLT
              print*, 'SMCDRY = ', SMCDRY
              print*, 'SMCREF = ', SMCREF
              print*, 'SMCMAX = ', SMCMAX
              print*, 'NROOT = ', NROOT
#endif
!---------------------------------------------------------------------
! Begin Converting Data back to grid from 1-d and make units conversions
!---------------------------------------------------------------------

!--------------------------------------------------------------------
!     Calculate residual of all surface energy balance eqn terms.
!--------------------------------------------------------------------
              NOAHRES(I,J) = ( solnet + lwdn ) - sheat + ssoil - eta - ( emissi * STBOLT * (t1x**4) ) - flx1 - flx2 - flx3

!  Convert ETA and ETP from W M-2 to KG M-2 S-1
!KWM              ETA = ETA/2.501E+6
              ETP = ETP/2.501E+6  

              QFX(I,J) = (EDIR+EC+ETT) + ESNOW ! in W m{-2}

!KWM Overwrite ETA for now
              ETA = ((EDIR+EC+ETT)/2.501E+6) + (ESNOW/2.83E6)

! Fill output variable arrays and doing output

              STC(I,J,1:NSOIL)=STC1(1:NSOIL)    ! Updated Soil Temperature
              SMC(I,J,1:NSOIL)=SMC1(1:NSOIL)    ! Updated Soil Moisture
              SH2OX(I,J,1:NSOIL)=SH2O(1:NSOIL)  ! Updated Soil Liquid Water
              SMAV2D(I,J,1:NSOIL) = SMAV(1:NSOIL)
              SNODEP(I,J)=SNOWH                 ! Updated snow depth !KWM
              WEASD(I,J)=SNEQV      ! ( m of water )
              FX(I,J)=FDOWN
              ETPX(I,J)=ETP
              T1(I,J)=T1X
              ETAX(I,J)=ETAX(I,J)+ETA*dt ! kg m{-2} s{-1} to mm liquid
              ETAKIN(I,J)=ETA_KINEMATIC*dt
              CMC(I,J)=CMCX
              GRDFLX(I,J)=SSOIL
              CHX(I,J)=CH
              CMX(I,J)=CM
              Q12D(I,J) = Q1
              ZNT(I,J)=Z0
              RUNOFF1X(I,J)=RUNOFF1X(I,J)+RUNOFF1*dt*1.E3
              RUNOFF2X(I,J)=RUNOFF2X(I,J)+RUNOFF2*dt*1.E3
              SOLDRAIN(i,j) = RUNOFF2*dt*1.E3
              RUNOFF3X(I,J)=RUNOFF3X(I,J)+RUNOFF3*dt*1.E3

              EDIRX(I,J)=EDIRX(I,J)+(EDIR/2.501E6)*dt ! (W m{-2} to kg m{-2} s{-2} to mm)
              ECX(I,J)=ECX(I,J)+(EC/2.501E6)*dt       ! (W m{-2} to kg m{-2} s{-2} to mm)
              ETTX(I,J)=ETTX(I,J)+(ETT/2.501E6)*dt    ! (W m{-2} to kg m{-2} s{-2} to mm)
!KWM              ETPNDX(I,J)=ETPND
!KWM              SNMAXX(I,J)=SNMAX
!KWM              SNOFLXX(I,J)=SNOFLX
!FC             SNOEVPX(I,J)=SNOEVP
              RCX(I,J)=RC
              HX(I,J)=SHEAT
              SMCMAX1(I,J)=SMCMAX
              SMCREF1(I,J)=SMCREF
              SMCWLT1(I,J)=SMCWLT
              ALBEDX(I,J)=ALBEDO
              ACRAIN(I,J)=ACRAIN(I,J)+PRCP*dt      ! (mm/s to mm)
              
              ACSNOM(I,J)=ACSNOM(I,J)+snomlt*1.E3  ! Accumulated snow melt (convert m to mm).
              ! ESNOW2D:  Accumulated snow sublimation (converted from W m{-2} to kg m{-2} s{-2} to mm)
              ESNOW2D(I,J)=ESNOW2D(I,J)+(ESNOW/2.83E6)*dt
              DRIP2D(I,J)=DRIP2D(I,J)+DRIP*1.E3   ! convert m to mm
              DEWFALL(I,J)=DEWFALL(I,J)+DEW*1.E3  ! convert m to mm
              SOILMX(I,J)= SOILM*1.E3             ! convert m to mm
              EMISS(I,J) = EMISSI
              XLAI2D(I,J) = XLAI
              ! If there's no snow, set SNOTIME to zero
              IF (snowh > 1.E-5) THEN
                 SNOTIME(I,J) = SNOTIME1
              ELSE
                 SNOTIME(I,J) = 0.0
              ENDIF
              EMBRD2D(I,J) = EMBRD
              SNOALB2D(I,J) = SNOALB

!DG Convert from point to grid (units of SFHEAD & INFXS (mm)
!DG Temp assignment for parking lot runoff
!KWM              SFCHEAD(I,J)=SFHEAD
!DG       INFXS1(I,J)=PRCP1(I,J)
!KWM              INFXS1(I,J)=INFXS
! End temp assignments
!KWM              PDDUM2(I,J)=PDDUM
!KWM              PCPDRP(I,J)=DRIP
!KWM              SFCWATR2(I,J)=SFCWATR

#ifdef _HRLDAS_URBAN_
              IF (SF_URBAN_PHYSICS == 1 ) THEN                                              ! Beginning of UCM CALL if block
!--------------------------------------
! URBAN CANOPY MODEL START - urban
!--------------------------------------
! Input variables lsm --> urban


                 IF ( ( VEGTYP(I,J) == ISURBAN  ) .or. ( VEGTYP(I,J) == 31 ) .or. &
                      ( VEGTYP(I,J) == 32 ) .or. ( VEGTYP(I,J) == 33 ) ) THEN
! Call urban

!
                    UTYPE_URB = UTYPE_URB2D(I,J) !urban type (low, high or industrial) 
                    TA_URB    = SFCTMP           ! [K]
                    QA_URB    = Q2X(I,J)         ! [kg/kg]
                    UA_URB    = SQRT(U(I,J)**2.+V(I,J)**2.)
                    U1_URB    = U(I,J)
                    V1_URB    = V(I,J)
                    IF(UA_URB < 1.) UA_URB=1.    ! [m/s]
                    SSG_URB   = SOLDN            ! [W/m/m]
                    SSGD_URB  = 0.8*SOLDN        ! [W/m/m]
                    SSGQ_URB  = SSG_URB-SSGD_URB ! [W/m/m]
                    LLG_URB   = LWDN             ! [W/m/m]
!KWM RAIN_URB  = RAINBL(I,J)      ! [mm]
                    RAIN_URB  = PRCP*dt          ! [mm]  !KWM
                    RHOO_URB  = SFCPRS / (287.04 * SFCTMP * (1.0+ 0.61 * Q2X(I,J))) ![kg/m/m/m]
                    ZA_URB    = ZLVL_URBAN       ! [m]
                    DELT_URB  = DT               ! [sec]
                    XLAT_URB  = LATITUDE(I,J)  ! [deg]
                    ZNT_URB   = ZNT(I,J)

                    LSOLAR_URB = .FALSE.

                    TR_URB = TR_URB2D(I,J)
                    TB_URB = TB_URB2D(I,J)
                    TG_URB = TG_URB2D(I,J)
                    TC_URB = TC_URB2D(I,J)
                    QC_URB = QC_URB2D(I,J)
                    UC_URB = UC_URB2D(I,J)

                    DO KROOF = 1,num_roof_layers
                       TRL_URB(KROOF) = TRL_URB3D(I,J,KROOF)
                    END DO
                    DO KWALL = 1,num_wall_layers
                       TBL_URB(KWALL) = TBL_URB3D(I,J,KWALL)
                    END DO
                    DO KROAD = 1,num_road_layers
                       TGL_URB(KROAD) = TGL_URB3D(I,J,KROAD)
                    END DO

                    XXXR_URB = XXXR_URB2D(I,J)
                    XXXB_URB = XXXB_URB2D(I,J)
                    XXXG_URB = XXXG_URB2D(I,J)
                    XXXC_URB = XXXC_URB2D(I,J)
!
!KWM            CHS_URB  = CHS(I,J)
!KWM            CHS2_URB = CHS2(I,J)
                    CHS_URB  = HS !KWM ?????
                    CHS2_URB = HS !KWM ?????

                    CMR_URB = CMR_URB2D(I,J)
                    CHR_URB = CHR_URB2D(I,J)
                    CMC_URB = CMC_URB2D(I,J)
                    CHC_URB = CHC_URB2D(I,J)
                    
!

                    IF ( .FALSE. ) THEN

                       print*, 'BEFORE CALL URBAN'
                       print*,'num_roof_layers',num_roof_layers, 'num_wall_layers',  &
                            num_wall_layers,                                             &
                            'DZR',DZR,'DZB',DZB,'DZG',DZG,'UTYPE_URB',UTYPE_URB,'TA_URB', &
                            TA_URB,                                                      &
                            'QA_URB',QA_URB,'UA_URB',UA_URB,'U1_URB',U1_URB,'V1_URB',    &
                            V1_URB,                                                     &
                            'SSG_URB',SSG_URB,'SSGD_URB',SSGD_URB,'SSGQ_URB',SSGQ_URB,  &
                            'LLG_URB',LLG_URB,'RAIN_URB',RAIN_URB,'RHOO_URB',RHOO_URB,   &
                            'ZA_URB',ZA_URB, 'DECLIN',DECLIN,'COSZ',COSZ,                &
                            'OMG_URB',OMG_URB,'XLAT_URB',XLAT_URB,'DELT_URB',DELT_URB,   &
                            'ZNT_URB',ZNT_URB,'TR_URB',TR_URB, 'TB_URB',TB_URB,'TG_URB',&
                            TG_URB,'TC_URB',TC_URB,'QC_URB',QC_URB,'TRL_URB',TRL_URB,   &
                            'TBL_URB',TBL_URB,'TGL_URB',TGL_URB,'XXXR_URB',XXXR_URB,   &
                            'XXXB_URB',XXXB_URB,'XXXG_URB',XXXG_URB,'XXXC_URB',XXXC_URB,&
                            'QS_URB',QS_URB,'SH_URB',SH_URB,'LH_URB',   &
                            LH_URB, 'LH_KINEMATIC_URB',LH_KINEMATIC_URB,'SW_URB',SW_URB,&
                            'ALB_URB',ALB_URB,'LW_URB',LW_URB,'G_URB',G_URB,'RN_URB',   &
                            RN_URB, 'PSIM_URB',PSIM_URB,'PSIH_URB',PSIH_URB,          &
                            'U10_URB',U10_URB,'V10_URB',V10_URB,'TH2_URB',TH2_URB,      &
                            'Q2_URB',Q2_URB,'CHS_URB',CHS_URB,'CHS2_URB',CHS2_URB
                    endif



! Call urban

                    CALL urban(LSOLAR_URB,                                & ! I
                         num_roof_layers,num_wall_layers,num_road_layers, & ! C
                         DZR,DZB,DZG,                                     & ! C
                         UTYPE_URB,TA_URB,QA_URB,UA_URB,U1_URB,V1_URB,SSG_URB, & ! I
                         SSGD_URB,SSGQ_URB,LLG_URB,RAIN_URB,RHOO_URB,     & ! I
                         ZA_URB,DECLIN,COSZ,OMG_URB,                      & ! I
                         XLAT_URB,DELT_URB,ZNT_URB,                       & ! I
                         CHS_URB, CHS2_URB,                               & ! I
                         TR_URB, TB_URB, TG_URB, TC_URB, QC_URB,UC_URB,   & ! H
                         TRL_URB,TBL_URB,TGL_URB,                         & ! H
                         XXXR_URB, XXXB_URB, XXXG_URB, XXXC_URB,          & ! H
                         TS_URB,QS_URB,SH_URB,LH_URB,LH_KINEMATIC_URB,    & ! O
                         SW_URB,ALB_URB,LW_URB,G_URB,RN_URB,PSIM_URB,PSIH_URB, & ! O
                         GZ1OZ0_URB,                                      & !O
                         CMR_URB, CHR_URB, CMC_URB, CHC_URB,              & ! INTENT (INOUT)
                         U10_URB, V10_URB, TH2_URB, Q2_URB,               & ! O
                         UST_URB)                                           !O


                    IF ( .FALSE. ) THEN

                       print*, 'AFTER CALL URBAN'
                       print*,'num_roof_layers',num_roof_layers, 'num_wall_layers',  &
                            num_wall_layers,                                             &
                            'DZR',DZR,'DZB',DZB,'DZG',DZG,'UTYPE_URB',UTYPE_URB,'TA_URB', &
                            TA_URB,                                                      &
                            'QA_URB',QA_URB,'UA_URB',UA_URB,'U1_URB',U1_URB,'V1_URB',    &
                            V1_URB,                                                     &
                            'SSG_URB',SSG_URB,'SSGD_URB',SSGD_URB,'SSGQ_URB',SSGQ_URB,  &
                            'LLG_URB',LLG_URB,'RAIN_URB',RAIN_URB,'RHOO_URB',RHOO_URB,   &
                            'ZA_URB',ZA_URB, 'DECLIN',DECLIN,'COSZ',COSZ,                &
                            'OMG_URB',OMG_URB,'XLAT_URB',XLAT_URB,'DELT_URB',DELT_URB,   &
                            'ZNT_URB',ZNT_URB,'TR_URB',TR_URB, 'TB_URB',TB_URB,'TG_URB',&
                            TG_URB,'TC_URB',TC_URB,'QC_URB',QC_URB,'TRL_URB',TRL_URB,   &
                            'TBL_URB',TBL_URB,'TGL_URB',TGL_URB,'XXXR_URB',XXXR_URB,   &
                            'XXXB_URB',XXXB_URB,'XXXG_URB',XXXG_URB,'XXXC_URB',XXXC_URB,&
                            'TS_URB',TS_URB,'QS_URB',QS_URB,'SH_URB',SH_URB,'LH_URB',   &
                            LH_URB, 'LH_KINEMATIC_URB',LH_KINEMATIC_URB,'SW_URB',SW_URB,&
                            'ALB_URB',ALB_URB,'LW_URB',LW_URB,'G_URB',G_URB,'RN_URB',   &
                            RN_URB, 'PSIM_URB',PSIM_URB,'PSIH_URB',PSIH_URB,          &
                            'U10_URB',U10_URB,'V10_URB',V10_URB,'TH2_URB',TH2_URB,      &
                            'Q2_URB',Q2_URB,'CHS_URB',CHS_URB,'CHS2_URB',CHS2_URB
                    endif

! Overwrite fluxes and surface temperature
!m

!m            ALBEDO(I,J) = FRC_URB*ALB_URB+FRC_NAT*ALBEDOK   ![-]
!m            HFX(I,J) = FRC_URB*SH_URB+FRC_NAT*SHEAT         ![W/m/m]
!m            QFX(I,J) = FRC_URB*LH_KINEMATIC_URB &
!m                     + FRC_NAT*ETA_KINEMATIC                ![kg/m/m/s]
!m            LH(I,J) = FRC_URB*LH_URB+FRC_NAT*ETA            ![W/m/m]
!m            GRDFLX(I,J) = FRC_URB*G_URB+FRC_NAT*SSOIL       ![W/m/m]
!m            TSK(I,J) = FRC_URB*TS_URB+FRC_NAT*T1            ![K]
!m            QSFC(I,J)= FRC_URB*QS_URB+FRC_NAT*Q1            ![-]

!
                    TS_URB2D(I,J) = TS_URB
!

                    ALBEDX(I,J) = FRC_URB2D(I,J)*ALB_URB+(1-FRC_URB2D(I,J))*ALBEDO    ![-]
                    HX(I,J) = FRC_URB2D(I,J)*SH_URB+(1-FRC_URB2D(I,J))*SHEAT         ![W/m/m]
                    ETAKIN(I,J) = FRC_URB2D(I,J)*LH_KINEMATIC_URB &
                         + (1-FRC_URB2D(I,J))*ETA_KINEMATIC                ![kg/m/m/s]
                    QFX(I,J) = FRC_URB2D(I,J)*LH_URB+(1-FRC_URB2D(I,J))*ETA            ![W/m/m]
                    GRDFLX(I,J) = FRC_URB2D(I,J)*G_URB+(1-FRC_URB2D(I,J))*SSOIL       ![W/m/m]
                    T1(I,J) = FRC_URB2D(I,J)*TS_URB+(1-FRC_URB2D(I,J))*T1X            ![K]
                    Q2X(I,J)= FRC_URB2D(I,J)*QS_URB+(1-FRC_URB2D(I,J))*Q1            ![-]

                    IF(.FALSE.)THEN

                       print*, ' FRC_URB2D', FRC_URB2D,                        &
                            'ALB_URB',ALB_URB, 'ALBEDO',ALBEDO, &
                            'ALBEDX(I,J)',  ALBEDX(I,J),                  &
                            'SH_URB',SH_URB,'SHEAT',SHEAT, 'HX(I,J)',HX(I,J),  &
                            'LH_KINEMATIC_URB',LH_KINEMATIC_URB,'ETA_KINEMATIC',  &
                            ETA_KINEMATIC, 'QFX(I,J)',QFX(I,J),                  &
                            'LH_URB',LH_URB, 'ETA',ETA, 'ETAKIN(I,J)',ETAKIN(I,J),        &
                            'G_URB',G_URB,'SSOIL',SSOIL,'GRDFLX(I,J)', GRDFLX(I,J),&
                            'TS_URB',TS_URB,'T1X',T1X,'T1(I,J)',T1(I,J),          &
                            'QS_URB',QS_URB,'Q1',Q1,'Q2X(I,J)',Q2X(I,J) 
                    endif




! Renew Urban State Varialbes

                    TR_URB2D(I,J) = TR_URB
                    TB_URB2D(I,J) = TB_URB
                    TG_URB2D(I,J) = TG_URB
                    TC_URB2D(I,J) = TC_URB
                    QC_URB2D(I,J) = QC_URB
                    UC_URB2D(I,J) = UC_URB

                    DO KROOF = 1,num_roof_layers
                       TRL_URB3D(I,J,KROOF) = TRL_URB(KROOF)
                    END DO
                    DO KWALL = 1,num_wall_layers
                       TBL_URB3D(I,J,KWALL) = TBL_URB(KWALL)
                    END DO
                    DO KROAD = 1,num_road_layers
                       TGL_URB3D(I,J,KROAD) = TGL_URB(KROAD)
                    END DO
                    XXXR_URB2D(I,J) = XXXR_URB
                    XXXB_URB2D(I,J) = XXXB_URB
                    XXXG_URB2D(I,J) = XXXG_URB
                    XXXC_URB2D(I,J) = XXXC_URB

                    SH_URB2D(I,J)    = SH_URB
                    LH_URB2D(I,J)    = LH_URB
                    G_URB2D(I,J)     = G_URB
                    RN_URB2D(I,J)    = RN_URB
                    PSIM_URB2D(I,J)  = PSIM_URB
                    PSIH_URB2D(I,J)  = PSIH_URB
                    GZ1OZ0_URB2D(I,J)= GZ1OZ0_URB
                    U10_URB2D(I,J)   = U10_URB
                    V10_URB2D(I,J)   = V10_URB
                    TH2_URB2D(I,J)   = TH2_URB
                    Q2_URB2D(I,J)    = Q2_URB
                    UST_URB2D(I,J)   = UST_URB
!
                    CMR_URB2D(I,J) = CMR_URB 
                    CHR_URB2D(I,J) = CHR_URB 
                    CMC_URB2D(I,J) = CMC_URB 
                    CHC_URB2D(I,J) = CHC_URB 

                    AKMS_URB2D(I,J)  = KARMAN * UST_URB2D(I,J)/(GZ1OZ0_URB2D(I,J)-PSIM_URB2D(I,J)) 

                 else                                          !KWM   ?????
                    do kroof = 1, num_roof_Layers             !KWM   ?????
                       TRL_URB3D(i,j,kroof) = STC(i,j,kroof)  !KWM   ?????
                       TBL_URB3D(i,j,kroof) = STC(i,j,kroof)  !KWM   ?????
                       TGL_URB3D(i,j,kroof) = STC(i,j,kroof)  !KWM   ?????
                    enddo                                     !KWM   ?????
                    TR_URB2D(i,j) = STC(i,j,1)                !KWM   ?????
                    TB_URB2D(i,j) = STC(i,j,1)                !KWM   ?????
                    TG_URB2D(i,j) = STC(i,j,1)                !KWM   ?????
                    TC_URB2D(i,j) = STC(i,j,1)                !KWM   ?????

                 END IF

              ENDIF                                   ! end of UCM CALL if block
!--------------------------------------
! Urban Part End - urban
!--------------------------------------
#endif

! ***  endif of the land-point
           ENDIF

       endif  !!!   if(abs(u) .lt. 200) then   !! do not run for missing data

        ENDDO ILOOP
     ENDDO JLOOP
!-------------------------------------------------------------------
! END of 1-D NOAH processing
!-------------------------------------------------------------------

! Output for history
     if (output_timestep > 0) then
        if (mod((k-1)*noah_timestep, output_timestep) == 0) then

           call prepare_output_file(trim(outdir), version, iz0tlnd, sfcdif_option, sf_urban_physics, &
                igrid, output_timestep, llanduse, split_output_count, hgrid,                &
                ixfull, jxfull, ixpar, jxpar, xstartpar, ystartpar,                         &
                iswater, mapproj, lat1, lon1, dx, dy, truelat1, truelat2, cen_lon,          &
                nsoil, sldpth, startdate, olddate, vegtyp, soltyp)

           DEFINE_MODE_LOOP : do imode = 1, 2

              call set_output_define_mode(imode)

              call add_to_output(vegtyp    , "IVGTYP"  , "Dominant vegetation category"        , "category"           )
              call add_to_output(soltyp    , "ISLTYP"  , "Dominant soil category"              , "category"           )
              call add_to_output(t1        , "SKINTEMP", "Skin temperature"                    , "K"                  )
              call add_to_output(cmc*1.E3  , "CANWAT"  , "Canopy water content"                , "mm"                 )
              call add_to_output(stc       , "SOIL_T"  , "soil temperature"                    , "K"                  )
              call add_to_output(smc       , "SOIL_M"  , "volumetric soil moisture"            , "m{3} m{-3}"         )
              call add_to_output(sh2ox     , "SOIL_W"  , "liquid volumetric soil moisture"     , "m{3} m{-3}"         )
              call add_to_output(soilmx    , "SOIL_MX" , "total column soil moisture"          , "mm"                 )
              call add_to_output(runoff1x  , "SFCRNOFF", "Accumulatetd surface runoff"         , "mm"                 )
              call add_to_output(runoff2x  , "UGDRNOFF", "Accumulated underground runoff"      , "mm"                 )
              call add_to_output(runoff3x  , "INTRFLOW", "Accumulated interflow runoff"        , "mm"                 )
              call add_to_output(etax      , "SFCEVP"  , "Accumulated evaporation from surface", "mm"                 )
              call add_to_output(etpnd      , "ETPND"  , "Accumulated evaporation from PONDED Water", "mm"                 )
              call add_to_output(etakin    , "ETAKIN"  , "Evapotranspiration"                  , "mm"                 )
              call add_to_output(ecx       , "CANEVP"  , "Accumulated canopy evaporation"      , "mm"                 )
              call add_to_output(edirx     , "EDIRX"   , "Accumulated direct soil evaporation" , "mm"                 )
              call add_to_output(ettx      , "ETTX"    , "Accumulated plant transpiration"     , "mm"                 )
              call add_to_output(albedx    , "ALBEDX"  , "Albedo -- What kind? (I.e., including what effects?)", "fraction")
              call add_to_output(weasd     , "WEASD"   , "Water equivalent snow depth"         , "m"                  )
              call add_to_output(acrain    , "ACRAIN"  , "Accumulated precipitation"           , "mm"                 )      
              call add_to_output(acsnom    , "ACSNOM"  , "Accumulated snow melt"               , "mm"                 )          
              call add_to_output(esnow2d   , "ESNOW"   , "Accumulated evaporation of snow"     , "mm"                 )
              call add_to_output(drip2d    , "DRIP"    , "Accumulated canopy drip"             , "mm"                 )
              call add_to_output(dewfall   , "DEWFALL" , "Accumulated dewfall"                 , "mm"                 )
              call add_to_output(snodep    , "SNODEP"  , "Snow depth"                          , "m"                  )
              call add_to_output(fpar      , "VEGFRA"  , "Green vegetation fraction"           , "fraction"           )
              call add_to_output(znt       , "Z0"      , "Roughness length"                    , "m"                  )
              call add_to_output(hx        , "HFX"     , "Upward surface sensible heat flux"   , "W m{-2}"            )
              call add_to_output(qfx       , "QFX"     , "Upward surface latent heat flux"     , "W m{-2}"            )
              call add_to_output(grdflx    , "GRDFLX"  , "Ground heat flux at surface"         , "W m{-2}"            )
              call add_to_output(short     , "SW"      , "Downward shortwave radiation flux"   , "W m{-2}"            )
              call add_to_output(xlong     , "LW"      , "Downward longwave radiation flux"    , "W m{-2}"            )
              call add_to_output(fx        , "FDOWN"   , "Radiation forcing at the surface"    , "W m{-2}"            )
              call add_to_output(xlai2d    , "XLAI"    , "Leaf area index"                     , "dimensionless"      )
              call add_to_output(snotime   , "SNOTIME" , "Snow age"                            , "s"                  )
              call add_to_output(embrd2d   , "EMBRD"   , "Background Emissivity"               , "dimensionless"      )
              call add_to_output(snoalb2d  , "SNOALB"  , "Maximum albedo over deep snow"       , "fraction"           )
              call add_to_output(noahres   , "NOAHRES" , "Residual of surface energy balance"  , "W m{-2}"            )
              call add_to_output(chx       , "CH"      , "Heat Exchange Coefficient"           , "-"                  )
#ifdef _HRLDAS_URBAN_
              if ( sf_urban_physics == 1 ) then
                 call add_to_output(trl_urb3d , "TRL"  , "Roof temperature"                    , "K"                  )
                 call add_to_output(tbl_urb3d , "TBL"  , "Wall temperature"                    , "K"                  )
                 call add_to_output(tgl_urb3d , "TGL"  , "Road temperature"                    , "K"                  )
                 call add_to_output(tr_urb2d  , "TR"   , "Roof layer temperature"              , "K"                  )  
                 call add_to_output(tb_urb2d  , "TB"   , "Wall layer temperature"              , "K"                  )  
                 call add_to_output(tg_urb2d  , "TG"   , "Road layer temperature"              , "K"                  )  
                 call add_to_output(tc_urb2d  , "TC"   , "Urban canopy temperature"            , "K"                  )
              endif
#endif

           enddo DEFINE_MODE_LOOP
           call finalize_output_file(split_output_count)
        endif
     endif

!yw     if ( rank == 0 ) then
!yw        if (vegtyp(parallel_xstart,subwindow_ystart)==ISWATER) then
!yw           write(*,'(" *** DATE = ", A19)', advance="NO") olddate
!yw        else
!yw           write(*,'(" *** DATE = ", A19, 6F10.5)', advance="NO") olddate, &
!yw                stc(parallel_xstart,subwindow_ystart,1), xlai2d(parallel_xstart,subwindow_ystart)
!yw        endif
!yw     endif

!------------------------------------------------------------------------
! Update the time 
!------------------------------------------------------------------------
 
     call geth_newdate(newdate, olddate, nint(dt))
     olddate = newdate

!------------------------------------------------------------------------
! End of Time Loop  (do K) 
!------------------------------------------------------------------------

#ifdef _PARALLEL_
     ! Just for sanity's sake, make sure all processors have caught up 
     ! before continuing to the next time step.
     call mpi_barrier(MPI_COMM_WORLD, ierr)
     if (ierr /= MPI_SUCCESS) stop "Problem with MPI_BARRIER"
#endif

     if (rank==0) then
        ! update the timer
        call system_clock(count=clock_count_2, count_rate=clock_rate)
        timing_sum = timing_sum + float(clock_count_2-clock_count_1)/float(clock_rate)
        write(*,'("    Timing: ",f6.2," Cumulative:  ", f10.2 )') &
             float(clock_count_2-clock_count_1)/float(clock_rate), &
             timing_sum
        clock_count_1 = clock_count_2

     endif
  endif   ! (rank if block)
  call hrldas_drv_HYDRO(STC(:,:,1:NSOIL),SMC(:,:,1:NSOIL),SH2OX(:,:,1:NSOIL), &
                        infxsrt,sfcheadrt,soldrain,ix,jx,NSOIL,               &
			qsgw)
     if (rank==0) then
     yw_rst_out = -9999

     print *, "restart_frequency_hours = ",restart_frequency_hours

!!! restart every month
     if( (olddate(9:10) == "01") .and. (olddate(12:13) == "00") .and. &
                     (olddate(15:16) == "00").and. (olddate(18:19) == "00")    &
             .and. (restart_frequency_hours <= 0) )  yw_rst_out = 99

        if ((k >= 0) .and. ((restart_frequency_hours .gt. 0) .and. (mod(k, int(restart_frequency_hours*3600./nint(dt))) == 0) &
               .or. yw_rst_out .eq. 99) ) then 
           yw_rst_out = -999

           if (rank == 0) print*, 'Write restart at '//olddate(1:13)
           call prepare_restart_file(trim(outdir), version, iz0tlnd, sfcdif_option,   &
                sf_urban_physics, igrid, llanduse, olddate, startdate,                         & 
                ixfull, jxfull, ixpar, jxpar, xstartpar, ystartpar,                   &
                nsoil, dx, dy, truelat1, truelat2, mapproj, lat1, lon1, cen_lon,      &
                iswater, vegtyp)

           ! Since the restart files are not really for user consumption, the units and description are 
           ! a little superfluous.  I've made them optional arguments.  If not present, they both default to "-".

           call add_to_restart ( stc,      "SOIL_T"   )
           call add_to_restart ( smc,      "SOIL_M"   )
           call add_to_restart ( sh2ox,    "SOIL_W"   )
           call add_to_restart ( qfx,      "QFX"      )
           call add_to_restart ( snodep,   "SNODEP"   )
           call add_to_restart ( weasd,    "WEASD"    )
           call add_to_restart ( fx,       "FDOWN"    )
           call add_to_restart ( etpx,     "ETP"      )
           call add_to_restart ( t1,       "SKINTEMP" )
           call add_to_restart ( etax,     "ETA"      )
           call add_to_restart ( etakin,   "ETAKIN"   )
           call add_to_restart ( cmc,      "CANWAT"   )
           call add_to_restart ( grdflx,   "GRDFLX"   )
           call add_to_restart ( chx,      "CH"       )
           call add_to_restart ( runoff1x, "RUNOFF1"  )
           call add_to_restart ( runoff2x, "RUNOFF2"  )
           call add_to_restart ( runoff3x, "RUNOFF3"  )
           call add_to_restart ( edirx,    "EDIR"     )
           call add_to_restart ( ettx,     "ETT"      )
           call add_to_restart ( ecx,      "EC"       )
           call add_to_restart ( emiss,    "EMISS"    )
           call add_to_restart ( gvfmin,   "GVFMIN"   )
           call add_to_restart ( gvfmax,   "GVFMAX"   )
           call add_to_restart ( snotime,  "SNOTIME"  )
           call add_to_restart ( q12d,     "Q12D"     )
           call add_to_restart ( chx,      "CHX"      )
           call add_to_restart ( cmx,      "CMX"      )
           call add_to_restart ( fpar,     "FPAR"     )
           call add_to_restart ( znt,      "ZNT"      )
           call add_to_restart ( soilmx,   "SOIL_MX"  )
           call add_to_restart ( dewfall,  "DEWFALL"  )
           call add_to_restart ( drip2d,   "DRIP"     )
           call add_to_restart ( esnow2d,  "ESNOW"    )
           call add_to_restart ( acsnom,   "ACSNOM"   )
           call add_to_restart ( acrain,   "ACRAIN"   )
           call add_to_restart ( albedx,   "ALBED"    )
           call add_to_restart ( smcwlt1,  "SMCWLT"   )
           call add_to_restart ( smcref1,  "SMCREF"   )
           call add_to_restart ( smcmax1,  "SMCMAX"   )
           call add_to_restart ( hx,       "HFX"      )
           call add_to_restart ( rcx,      "RC"       )
           call add_to_restart ( lai,      "LAI"       )
#ifdef _HRLDAS_URBAN_
           if (sf_urban_physics == 1) then
              call add_to_restart ( utype_urb2d, "UTYPE_URB2D" )
              call add_to_restart ( trl_urb3d,   "TRL_URB3D"   )
              call add_to_restart ( tbl_urb3d,   "TBL_URB3D"   )
              call add_to_restart ( tgl_urb3d,   "TGL_URB3D"   )
              call add_to_restart ( ts_urb2d,    "TS_URB2D"    )
              call add_to_restart ( tr_urb2d,    "TR_URB2D"    )
              call add_to_restart ( tb_urb2d,    "TB_URB2D"    )
              call add_to_restart ( tg_urb2d,    "TG_URB2D"    )
              call add_to_restart ( tc_urb2d,    "TC_URB2D"    )
              call add_to_restart ( qc_urb2d,    "QC_URB2D"    )
              call add_to_restart ( uc_urb2d,    "UC_URB2D"    )
              call add_to_restart ( xxxr_urb2d,  "XXXR_URB2D"  )
              call add_to_restart ( xxxb_urb2d,  "XXXB_URB2D"  )
              call add_to_restart ( xxxg_urb2d,  "XXXG_URB2D"  )
              call add_to_restart ( xxxc_urb2d,  "XXXC_URB2D"  )
              call add_to_restart ( frc_urb2d,   "FRC_URB2D"   )
              call add_to_restart ( cmr_urb2d,   "CMR_URB2D"     )
              call add_to_restart ( chr_urb2d,   "CHR_URB2D"     )
              call add_to_restart ( cmc_urb2d,   "CMC_URB2D"     )
              call add_to_restart ( chc_urb2d,   "CHC_URB2D"     )
           endif
#endif
           if (allocated(FPAR_SAVE)) then
              call add_to_restart( fpar_save, "FPAR_SAVE")
           endif

           if (allocated(LAI_SAVE)) then
              call add_to_restart( lai_save, "LAI_SAVE")
           endif

           call finalize_restart_file()
        endif
  endif !!! rank if block

1003   continue

  ENDDO KLOOP

   if(GWBASESWCRT .eq. 3) then
      if(gwCycle > 0) then
          gwCycle = gwCycle - 1
          olddate = startdate
          goto 1002
      end if
   endif

#ifdef _PARALLEL_
  call mpi_finalize(ierr)
  if (ierr /= MPI_SUCCESS) stop "Problem with MPI_FINALIZE"
#endif

  call hydro_finish()
END program Noah_hrldas_driver

!-------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------

subroutine wrf_message(msg)
  implicit none
  character(len=*), intent(in) :: msg
  write(*, '(A)') msg
end subroutine wrf_message

!-------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------

logical function wrf_dm_on_monitor() result(l)
  implicit none
  l = .TRUE.
end function wrf_dm_on_monitor

!-------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------
SUBROUTINE calc_declin ( nowdate, latitude, longitude, cosz, hrang, declin )
  use module_date_utilities
!---------------------------------------------------------------------
   IMPLICIT NONE
!---------------------------------------------------------------------

   REAL, PARAMETER :: DEGRAD = 3.14159265/180.
   REAL, PARAMETER :: DPD    = 360./365.
! !ARGUMENTS:
   character(len=13), intent(in)  :: nowdate
   real,              intent(in)  :: latitude
   real,              intent(in)  :: longitude
   REAL,              intent(out) :: cosz
   REAL,              intent(out) :: hrang
   real,              intent(out) :: DECLIN
   REAl                           :: JULIAN
   REAL                           :: OBECL
   REAL                           :: SINOB
   REAL                           :: SXLONG
   REAL                           :: ARG
   real                           :: tloctim
   integer                        :: iday
   integer                        :: ihour

   call geth_idts(nowdate(1:10), nowdate(1:4)//"-01-01", iday)
   read(nowdate(12:13), *) ihour
   julian = real(iday) + real(ihour)/24.
!
! for short wave radiation

   DECLIN=0.

!-----OBECL : OBLIQUITY = 23.5 DEGREE.
        
   OBECL=23.5*DEGRAD
   SINOB=SIN(OBECL)
        
!-----CALCULATE LONGITUDE OF THE SUN FROM VERNAL EQUINOX:
        
   IF(JULIAN.GE.80.)SXLONG=DPD*(JULIAN-80.)*DEGRAD
   IF(JULIAN.LT.80.)SXLONG=DPD*(JULIAN+285.)*DEGRAD
   ARG=SINOB*SIN(SXLONG)
   DECLIN=ASIN(ARG)

   TLOCTIM = IHOUR + LONGITUDE/15. ! Local time in hours
   tloctim = AMOD(tloctim+24.0, 24.0)
   HRANG=15.*(TLOCTIM-12.)*DEGRAD
   COSZ=SIN(LATITUDE*DEGRAD)*SIN(DECLIN)+COS(LATITUDE*DEGRAD)*COS(DECLIN)*COS(HRANG)

!KWM   write(wrf_err_message,10)DECDEG/DEGRAD
!KWM10 FORMAT(1X,'*** SOLAR DECLINATION ANGLE = ',F6.2,' DEGREES.',' ***')
!KWM   CALL wrf_debug (50, wrf_err_message)

 END SUBROUTINE calc_declin

!-----------------------------------------------------------------
 SUBROUTINE SOIL_VEG_GEN_PARM( MMINLU, MMINSL)
!-----------------------------------------------------------------

   USE module_sf_noahlsm
#ifdef _PARALLEL_
   use mpi
#endif
   IMPLICIT NONE

   CHARACTER(LEN=*), INTENT(IN) :: MMINLU, MMINSL
   integer :: LUMATCH, IINDEX, LC, NUM_SLOPE
   integer :: ierr
   INTEGER , PARAMETER :: OPEN_OK = 0

   character*128 :: mess , message
   logical, external :: wrf_dm_on_monitor


!-----SPECIFY VEGETATION RELATED CHARACTERISTICS :
!             ALBBCK: SFC albedo (in percentage)
!                 Z0: Roughness length (m)
!             SHDFAC: Green vegetation fraction (in percentage)
!  Note: The ALBEDO, Z0, and SHDFAC values read from the following table
!          ALBEDO, amd Z0 are specified in LAND-USE TABLE; and SHDFAC is
!          the monthly green vegetation data
!             CMXTBL: MAX CNPY Capacity (m)
!             NROTBL: Rooting depth (layer)
!              RSMIN: Mimimum stomatal resistance (s m-1)
!              RSMAX: Max. stomatal resistance (s m-1)
!                RGL: Parameters used in radiation stress function
!                 HS: Parameter used in vapor pressure deficit functio
!               TOPT: Optimum transpiration air temperature. (K)
!             CMCMAX: Maximum canopy water capacity
!             CFACTR: Parameter used in the canopy inteception calculati
!               SNUP: Threshold snow depth (in water equivalent m) that
!                     implies 100% snow cover
!                LAI: Leaf area index (dimensionless)
!             MAXALB: Upper bound on maximum albedo over deep snow
!
!-----READ IN VEGETAION PROPERTIES FROM VEGPARM.TBL
!

   integer :: rank

#ifdef _PARALLEL_
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  if (ierr /= MPI_SUCCESS) stop "MPI_COMM_RANK"
#else
  rank = 0
#endif


   IF ( wrf_dm_on_monitor() ) THEN

      OPEN(19, FILE='VEGPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
      IF(ierr .NE. OPEN_OK ) THEN
         WRITE(message,FMT='(A)') &
              'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening VEGPARM.TBL'
         CALL wrf_error_fatal ( message )
      END IF


      LUMATCH=0

      FIND_LUTYPE : DO WHILE (LUMATCH == 0)
         READ (19,*,END=2002)
         READ (19,*,END=2002)LUTYPE
         READ (19,*)LUCATS,IINDEX

         IF(LUTYPE.EQ.MMINLU)THEN
            WRITE( mess , * ) 'LANDUSE TYPE = ' // TRIM ( LUTYPE ) // ' FOUND', LUCATS,' CATEGORIES'
            if (rank == 0) CALL wrf_message( mess )
            LUMATCH=1
         ELSE
            call wrf_message ( "Skipping over LUTYPE = " // TRIM ( LUTYPE ) )
            DO LC = 1, LUCATS+12
               read(19,*)
            ENDDO
         ENDIF
      ENDDO FIND_LUTYPE
! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
      IF ( SIZE(SHDTBL)       < LUCATS .OR. &
           SIZE(NROTBL)       < LUCATS .OR. &
           SIZE(RSTBL)        < LUCATS .OR. &
           SIZE(RGLTBL)       < LUCATS .OR. &
           SIZE(HSTBL)        < LUCATS .OR. &
           SIZE(SNUPTBL)      < LUCATS .OR. &
           SIZE(MAXALB)       < LUCATS .OR. &
           SIZE(LAIMINTBL)    < LUCATS .OR. &
           SIZE(LAIMAXTBL)    < LUCATS .OR. &
           SIZE(Z0MINTBL)     < LUCATS .OR. &
           SIZE(Z0MAXTBL)     < LUCATS .OR. &
           SIZE(ALBEDOMINTBL) < LUCATS .OR. &
           SIZE(ALBEDOMAXTBL) < LUCATS .OR. &
           SIZE(EMISSMINTBL ) < LUCATS .OR. &
           SIZE(EMISSMAXTBL ) < LUCATS ) THEN
         CALL wrf_error_fatal('Table sizes too small for value of LUCATS in module_sf_noahdrv.F')
      ENDIF

      IF(LUTYPE.EQ.MMINLU)THEN
         DO LC=1,LUCATS
            READ (19,*)IINDEX,SHDTBL(LC),                        &
                 NROTBL(LC),RSTBL(LC),RGLTBL(LC),HSTBL(LC), &
                 SNUPTBL(LC),MAXALB(LC), LAIMINTBL(LC),     &
                 LAIMAXTBL(LC),EMISSMINTBL(LC),             &
                 EMISSMAXTBL(LC), ALBEDOMINTBL(LC),         &
                 ALBEDOMAXTBL(LC), Z0MINTBL(LC), Z0MAXTBL(LC)
         ENDDO
!
         READ (19,*)
         READ (19,*)TOPT_DATA
         READ (19,*)
         READ (19,*)CMCMAX_DATA
         READ (19,*)
         READ (19,*)CFACTR_DATA
         READ (19,*)
         READ (19,*)RSMAX_DATA
         READ (19,*)
         READ (19,*)BARE
         READ (19,*)
         READ (19,*)NATURAL
      ENDIF
!
2002  CONTINUE

      CLOSE (19)
      IF (LUMATCH == 0) then
         CALL wrf_error_fatal ("Land Use Dataset '"//MMINLU//"' not found in VEGPARM.TBL.")
      ENDIF
   ENDIF

   CALL wrf_dm_bcast_string  ( LUTYPE  , 4 )
   CALL wrf_dm_bcast_integer ( LUCATS  , 1 )
   CALL wrf_dm_bcast_integer ( IINDEX  , 1 )
   CALL wrf_dm_bcast_integer ( LUMATCH , 1 )
   CALL wrf_dm_bcast_real    ( SHDTBL  , NLUS )
   CALL wrf_dm_bcast_real    ( NROTBL  , NLUS )
   CALL wrf_dm_bcast_real    ( RSTBL   , NLUS )
   CALL wrf_dm_bcast_real    ( RGLTBL  , NLUS )
   CALL wrf_dm_bcast_real    ( HSTBL   , NLUS )
   CALL wrf_dm_bcast_real    ( SNUPTBL , NLUS )
   CALL wrf_dm_bcast_real    ( LAIMINTBL    , NLUS )
   CALL wrf_dm_bcast_real    ( LAIMAXTBL    , NLUS )
   CALL wrf_dm_bcast_real    ( Z0MINTBL     , NLUS )
   CALL wrf_dm_bcast_real    ( Z0MAXTBL     , NLUS )
   CALL wrf_dm_bcast_real    ( EMISSMINTBL  , NLUS )
   CALL wrf_dm_bcast_real    ( EMISSMAXTBL  , NLUS )
   CALL wrf_dm_bcast_real    ( ALBEDOMINTBL , NLUS )
   CALL wrf_dm_bcast_real    ( ALBEDOMAXTBL , NLUS )
   CALL wrf_dm_bcast_real    ( MAXALB  , NLUS )
   CALL wrf_dm_bcast_real    ( TOPT_DATA    , 1 )
   CALL wrf_dm_bcast_real    ( CMCMAX_DATA  , 1 )
   CALL wrf_dm_bcast_real    ( CFACTR_DATA  , 1 )
   CALL wrf_dm_bcast_real    ( RSMAX_DATA  , 1 )
   CALL wrf_dm_bcast_integer ( BARE    , 1 )
   CALL wrf_dm_bcast_integer ( NATURAL , 1 )

!
!-----READ IN SOIL PROPERTIES FROM SOILPARM.TBL
!
   IF ( wrf_dm_on_monitor() ) THEN
      OPEN(19, FILE='SOILPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
      IF(ierr .NE. OPEN_OK ) THEN
         WRITE(message,FMT='(A)') &
              'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening SOILPARM.TBL'
         CALL wrf_error_fatal ( message )
      END IF

      WRITE(mess,*) 'INPUT SOIL TEXTURE CLASSIFICATION = ', TRIM ( MMINSL )
      if (rank == 0) CALL wrf_message( mess )

      LUMATCH=0

      READ (19,*)
      READ (19,2000,END=2003)SLTYPE
2000  FORMAT (A4)
      READ (19,*)SLCATS,IINDEX
      IF(SLTYPE.EQ.MMINSL)THEN
         WRITE( mess , * ) 'SOIL TEXTURE CLASSIFICATION = ', TRIM ( SLTYPE ) , ' FOUND', &
              SLCATS,' CATEGORIES'
         if (rank == 0) CALL wrf_message ( mess )
         LUMATCH=1
      ENDIF
! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
      IF ( SIZE(BB    ) < SLCATS .OR. &
           SIZE(DRYSMC) < SLCATS .OR. &
           SIZE(F11   ) < SLCATS .OR. &
           SIZE(MAXSMC) < SLCATS .OR. &
           SIZE(REFSMC) < SLCATS .OR. &
           SIZE(SATPSI) < SLCATS .OR. &
           SIZE(SATDK ) < SLCATS .OR. &
           SIZE(SATDW ) < SLCATS .OR. &
           SIZE(WLTSMC) < SLCATS .OR. &
           SIZE(QTZ   ) < SLCATS  ) THEN
         CALL wrf_error_fatal('Table sizes too small for value of SLCATS in module_sf_noahdrv.F')
      ENDIF
      IF(SLTYPE.EQ.MMINSL)THEN
         DO LC=1,SLCATS
            READ (19,*) IINDEX,BB(LC),DRYSMC(LC),F11(LC),MAXSMC(LC),&
                 REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
                 WLTSMC(LC), QTZ(LC)
         ENDDO
      ENDIF

2003  CONTINUE

      CLOSE (19)
   ENDIF

   CALL wrf_dm_bcast_integer ( LUMATCH , 1 )
   CALL wrf_dm_bcast_string  ( SLTYPE  , 4 )
   CALL wrf_dm_bcast_string  ( MMINSL  , 4 )  ! since this is reset above, see oct2 ^
   CALL wrf_dm_bcast_integer ( SLCATS  , 1 )
   CALL wrf_dm_bcast_integer ( IINDEX  , 1 )
   CALL wrf_dm_bcast_real    ( BB      , NSLTYPE )
   CALL wrf_dm_bcast_real    ( DRYSMC  , NSLTYPE )
   CALL wrf_dm_bcast_real    ( F11     , NSLTYPE )
   CALL wrf_dm_bcast_real    ( MAXSMC  , NSLTYPE )
   CALL wrf_dm_bcast_real    ( REFSMC  , NSLTYPE )
   CALL wrf_dm_bcast_real    ( SATPSI  , NSLTYPE )
   CALL wrf_dm_bcast_real    ( SATDK   , NSLTYPE )
   CALL wrf_dm_bcast_real    ( SATDW   , NSLTYPE )
   CALL wrf_dm_bcast_real    ( WLTSMC  , NSLTYPE )
   CALL wrf_dm_bcast_real    ( QTZ     , NSLTYPE )

   IF(LUMATCH.EQ.0)THEN
      CALL wrf_message( 'SOIl TEXTURE IN INPUT FILE DOES NOT ' )
      CALL wrf_message( 'MATCH SOILPARM TABLE'                 )
      CALL wrf_error_fatal ( 'INCONSISTENT OR MISSING SOILPARM FILE' )
   ENDIF

!
!-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL
!
   IF ( wrf_dm_on_monitor() ) THEN
      OPEN(19, FILE='GENPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
      IF(ierr .NE. OPEN_OK ) THEN
         WRITE(message,FMT='(A)') &
              'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening GENPARM.TBL'
         CALL wrf_error_fatal ( message )
      END IF

      READ (19,*)
      READ (19,*)
      READ (19,*) NUM_SLOPE

      SLPCATS=NUM_SLOPE
! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
      IF ( SIZE(slope_data) < NUM_SLOPE ) THEN
         CALL wrf_error_fatal('NUM_SLOPE too large for slope_data array in module_sf_noahdrv')
      ENDIF

      DO LC=1,SLPCATS
         READ (19,*)SLOPE_DATA(LC)
      ENDDO

      READ (19,*)
      READ (19,*)SBETA_DATA
      READ (19,*)
      READ (19,*)FXEXP_DATA
      READ (19,*)
      READ (19,*)CSOIL_DATA
      READ (19,*)
      READ (19,*)SALP_DATA
      READ (19,*)
      READ (19,*)REFDK_DATA
      READ (19,*)
      READ (19,*)REFKDT_DATA
      READ (19,*)
      READ (19,*)FRZK_DATA
      READ (19,*)
      READ (19,*)ZBOT_DATA
      READ (19,*)
      READ (19,*)CZIL_DATA
      READ (19,*)
      READ (19,*)SMLOW_DATA
      READ (19,*)
      READ (19,*)SMHIGH_DATA
      READ (19,*)
      READ (19,*)LVCOEF_DATA
      CLOSE (19)
   ENDIF

   CALL wrf_dm_bcast_integer ( NUM_SLOPE    ,  1 )
   CALL wrf_dm_bcast_integer ( SLPCATS      ,  1 )
   CALL wrf_dm_bcast_real    ( SLOPE_DATA   ,  NSLOPE )
   CALL wrf_dm_bcast_real    ( SBETA_DATA   ,  1 )
   CALL wrf_dm_bcast_real    ( FXEXP_DATA   ,  1 )
   CALL wrf_dm_bcast_real    ( CSOIL_DATA   ,  1 )
   CALL wrf_dm_bcast_real    ( SALP_DATA    ,  1 )
   CALL wrf_dm_bcast_real    ( REFDK_DATA   ,  1 )
   CALL wrf_dm_bcast_real    ( REFKDT_DATA  ,  1 )
   CALL wrf_dm_bcast_real    ( FRZK_DATA    ,  1 )
   CALL wrf_dm_bcast_real    ( ZBOT_DATA    ,  1 )
   CALL wrf_dm_bcast_real    ( CZIL_DATA    ,  1 )
   CALL wrf_dm_bcast_real    ( SMLOW_DATA   ,  1 )
   CALL wrf_dm_bcast_real    ( SMHIGH_DATA  ,  1 )
   CALL wrf_dm_bcast_real    ( LVCOEF_DATA  ,  1 )

!-----------------------------------------------------------------
 END SUBROUTINE SOIL_VEG_GEN_PARM
!-----------------------------------------------------------------
