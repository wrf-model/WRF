MODULE module_sf_urban

!===============================================================================
!     Single-Layer Urban Canopy Model for WRF Noah-LSM
!     Original Version: 2002/11/06 by Hiroyuki Kusaka
!     Last Update:      2006/08/24 by Fei Chen and Mukul Tewari (NCAR/RAL)  
!===============================================================================

   CHARACTER(LEN=4)                :: LU_DATA_TYPE

   INTEGER                         :: ICATE

   REAL, ALLOCATABLE, DIMENSION(:) :: ZR_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: Z0C_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: Z0HC_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: ZDC_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: SVF_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: R_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: RW_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: HGT_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: AH_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: BETR_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: BETB_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: BETG_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: FRC_URB_TBL

   REAL, ALLOCATABLE, DIMENSION(:) :: COP_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: PWIN_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: BETA_TBL
   INTEGER, ALLOCATABLE, DIMENSION(:) :: SW_COND_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: TIME_ON_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: TIME_OFF_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: TARGTEMP_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: GAPTEMP_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: TARGHUM_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: GAPHUM_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: PERFLO_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: HSESF_TBL

   REAL, ALLOCATABLE, DIMENSION(:) :: CAPR_TBL, CAPB_TBL, CAPG_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: AKSR_TBL, AKSB_TBL, AKSG_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: ALBR_TBL, ALBB_TBL, ALBG_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: EPSR_TBL, EPSB_TBL, EPSG_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: Z0R_TBL,  Z0B_TBL,  Z0G_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: SIGMA_ZED_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: Z0HB_TBL, Z0HG_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: TRLEND_TBL, TBLEND_TBL, TGLEND_TBL
   REAL, ALLOCATABLE, DIMENSION(:) :: AKANDA_URBAN_TBL
!for BEP

   ! MAXDIRS :: The maximum number of street directions we're allowed to define
   INTEGER, PARAMETER :: MAXDIRS = 3
   ! MAXHGTS :: The maximum number of building height bins we're allowed to define
   INTEGER, PARAMETER :: MAXHGTS = 50

   INTEGER, ALLOCATABLE, DIMENSION(:)   :: NUMDIR_TBL
   REAL,    ALLOCATABLE, DIMENSION(:,:) :: STREET_DIRECTION_TBL
   REAL,    ALLOCATABLE, DIMENSION(:,:) :: STREET_WIDTH_TBL
   REAL,    ALLOCATABLE, DIMENSION(:,:) :: BUILDING_WIDTH_TBL
   INTEGER, ALLOCATABLE, DIMENSION(:)   :: NUMHGT_TBL
   REAL,    ALLOCATABLE, DIMENSION(:,:) :: HEIGHT_BIN_TBL
   REAL,    ALLOCATABLE, DIMENSION(:,:) :: HPERCENT_BIN_TBL
!end BEP
   INTEGER                         :: BOUNDR_DATA,BOUNDB_DATA,BOUNDG_DATA
   INTEGER                         :: CH_SCHEME_DATA, TS_SCHEME_DATA
   INTEGER                         :: ahoption        ! Miao, 2007/01/17, cal. ah
   REAL, DIMENSION(1:24)           :: ahdiuprf        ! ah diurnal profile, tloc: 1-24
   REAL, DIMENSION(1:24)           :: hsequip_tbl

   INTEGER                         :: allocate_status 

!   INTEGER                         :: num_roof_layers
!   INTEGER                         :: num_wall_layers
!   INTEGER                         :: num_road_layers

   CHARACTER (LEN=256) , PRIVATE   :: mesg

   CONTAINS

!===============================================================================
!
! Author:
!      Hiroyuki KUSAKA, PhD
!      University of Tsukuba, JAPAN
!      (CRIEPI, NCAR/MMM visiting scientist, 2002-2004)
!      kusaka@ccs.tsukuba.ac.jp
!
! Co-Researchers:
!     Fei CHEN, PhD
!      NCAR/RAP feichen@ucar.edu
!     Mukul TEWARI, PhD
!      NCAR/RAP mukul@ucar.edu
!
! Purpose:
!     Calculate surface temeprature, fluxes, canopy air temperature, and canopy wind
!
! Subroutines:
!     module_sf_urban
!       |- urban
!            |- read_param
!            |- mos or jurges
!            |- multi_layer or force_restore
!       |- urban_param_init <-- URBPARM.TBL
!       |- urban_var_init
!
! Input Data from WRF [MKS unit]:
!
!     UTYPE  [-]     : Urban type. 1=Commercial/Industrial; 2=High-intensity residential; 
!                    : 3=low-intensity residential 
!     TA     [K]     : Potential temperature at 1st wrf level (absolute temp)
!     QA     [kg/kg] : Mixing ratio at 1st atmospheric level
!     UA     [m/s]   : Wind speed at 1st atmospheric level
!     SSG    [W/m/m] : Short wave downward radiation at a flat surface
!                      Note this is the total of direct and diffusive solar
!                      downward radiation. If without two components, the
!                      single solar downward can be used instead.
!                      SSG = SSGD + SSGQ
!     LSOLAR [-]     : Indicating the input type of solar downward radiation
!                      True: both direct and diffusive solar radiation 
!                      are available
!                      False: only total downward ridiation is available.
!     SSGD   [W/m/m] : Direct solar radiation at a flat surface
!                      if SSGD is not available, one can assume a ratio SRATIO
!                      (e.g., 0.7), so that SSGD = SRATIO*SSG 
!     SSGQ   [W/m/m] : Diffuse solar radiation at a flat surface
!                      If SSGQ is not available, SSGQ = SSG - SSGD
!     LLG    [W/m/m] : Long wave downward radiation at a flat surface 
!     RAIN   [mm/h]  : Precipitation
!     RHOO   [kg/m/m/m] : Air density
!     ZA     [m]     : First atmospheric level
!                      as a lowest boundary condition
!     DECLIN [rad]   : solar declination
!     COSZ           : = sin(fai)*sin(del)+cos(fai)*cos(del)*cos(omg)
!     OMG    [rad]   : solar hour angle
!     XLAT   [deg]   : latitude
!     DELT   [sec]   : Time step
!     ZNT    [m]     : Roughnes length
!
! Output Data to WRF [MKS unit]:
!
!     TS  [K]            : Surface potential temperature (absolute temp)
!     QS  [-]            : Surface humidity
!
!     SH  [W/m/m/]       : Sensible heat flux, = FLXTH*RHOO*CPP
!     LH  [W/m/m]        : Latent heat flux, = FLXHUM*RHOO*ELL
!     LH_INEMATIC [kg/m/m/sec]: Moisture Kinematic flux, = FLXHUM*RHOO
!     SW  [W/m/m]        : Upward shortwave radiation flux, 
!                          = SSG-SNET*697.7*60. (697.7*60.=100.*100.*4.186)
!     ALB [-]            : Time-varying albedo
!     LW  [W/m/m]        : Upward longwave radiation flux, 
!                          = LNET*697.7*60.-LLG
!     G   [W/m/m]        : Heat Flux into the Ground
!     RN  [W/m/m]        : Net radiation
!
!     PSIM  [-]          : Diagnostic similarity stability function for momentum
!     PSIH  [-]          : Diagnostic similarity stability function for heat
!
!     TC  [K]            : Diagnostic canopy air temperature
!     QC  [-]            : Diagnostic canopy humidity
!
!     TH2 [K]            : Diagnostic potential temperature at 2 m
!     Q2  [-]            : Diagnostic humidity at 2 m
!     U10 [m/s]          : Diagnostic u wind component at 10 m
!     V10 [m/s]          : Diagnostic v wind component at 10 m
!
!     CHS, CHS2 [m/s]    : CH*U at ZA, CH*U at 2 m (not used)
!
! Important parameters:
!
! Morphology of the urban canyon:
! These parameters assigned in the URBPARM.TBL
!
!    ZR  [m]             : roof level (building height)
!    SIGMA_ZED [m]       : Standard Deviation of roof height
!    ROOF_WIDTH [m]      : roof (i.e., building) width
!    ROAD_WIDTH [m]      : road width
!
! Parameters derived from the morphological terms above.
! These parameters are computed in the code.
!
!    HGT [-]             : normalized building height
!    SVF [-]             : sky view factor
!    R   [-]             : Normalized roof width (a.k.a. "building coverage ratio")
!    RW  [-]             : = 1 - R
!    Z0C [m]             : Roughness length above canyon for momentum (1/10 of ZR)
!    Z0HC [m]            : Roughness length above canyon for heat (1/10 of Z0C)
!    ZDC [m]             : Zero plane displacement height (1/5 of ZR)
!
! Following parameter are assigned in run/URBPARM.TBL
!
!  AH  [ W m{-2} ]        : anthropogenic heat  ( W m{-2} in the table, converted internally to cal cm{-2} )
!  CAPR[ J m{-3} K{-1} ]  : heat capacity of roof ( units converted in code to [ cal cm{-3} deg{-1} ] )
!  CAPB[ J m{-3} K{-1} ]  : heat capacity of building wall ( units converted in code to [ cal cm{-3} deg{-1} ] )
!  CAPG[ J m{-3} K{-1} ]  : heat capacity of road ( units converted in code to [ cal cm{-3} deg{-1} ] )
!  AKSR [ J m{-1} s{-1} K{-1} ] : thermal conductivity of roof ( units converted in code to [ cal cm{-1} s{-1} deg{-1} ] )
!  AKSB [ J m{-1} s{-1} K{-1} ] : thermal conductivity of building wall ( units converted in code to [ cal cm{-1} s{-1} deg{-1} ] )
!  AKSG [ J m{-1} s{-1} K{-1} ] : thermal conductivity of road ( units converted in code to [ cal cm{-1} s{-1} deg{-1} ] )
!  ALBR [-]               : surface albedo of roof
!  ALBB [-]               : surface albedo of building wall
!  ALBG [-]               : surface albedo of road
!  EPSR [-]               : surface emissivity of roof
!  EPSB [-]               : surface emissivity of building wall
!  EPSG [-]               : surface emissivity of road
!  Z0B [m]                : roughness length for momentum of building wall (only for CH_SCHEME = 1)
!  Z0G [m]                : roughness length for momentum of road (only for CH_SCHEME = 1)
!  Z0HB [m]               : roughness length for heat of building wall (only for CH_SCHEME = 1)
!  Z0HG [m]               : roughness length for heat of road
!  num_roof_layers        : number of layers within roof
!  num_wall_layers        : number of layers within building walls
!  num_road_layers        : number of layers within below road surface
!   NOTE: for now, these layers are defined as same as the number of soil layers in namelist.input
!  DZR [cm]               : thickness of each roof layer
!  DZB [cm]               : thickness of each building wall layer
!  DZG [cm]               : thickness of each ground layer
!  BOUNDR [integer 1 or 2] : Boundary Condition for Roof Layer Temp [1: Zero-Flux, 2: T = Constant]
!  BOUNDB [integer 1 or 2] : Boundary Condition for Building Wall Layer Temp [1: Zero-Flux, 2: T = Constant]
!  BOUNDG [integer 1 or 2] : Boundary Condition for Road Layer Temp [1: Zero-Flux, 2: T = Constant]
!  TRLEND [K]             : lower boundary condition of roof temperature
!  TBLEND [K]             : lower boundary condition of building temperature
!  TGLEND [K]             : lower boundary condition of ground temperature
!  CH_SCHEME [integer 1 or 2] : Sfc exchange scheme used for building wall and road
!                         [1: M-O Similarity Theory,  2: Empirical Form (recommend)]
!  TS_SCHEME [integer 1 or 2] : Scheme for computing surface temperature (for roof, wall, and road)
!                         [1: 4-layer model,  2: Force-Restore method]
!
!for BEP
!  numdir [ - ]             : Number of street directions defined for a particular urban category
!  street_direction [ deg ] : Direction of streets for a particular urban category and a particular street direction
!  street_width [ m ]       : Width of street for a particular urban category and a particular street direction
!  building_width [ m ]     : Width of buildings for a particular urban category and a particular street direction
!  numhgt [ - ]             : Number of building height levels defined for a particular urban category
!  height_bin [ m ]         : Building height bins defined for a particular urban category.
!  hpercent_bin [ % ]       : Percentage of a particular urban category populated by buildings of particular height bins
!end BEP
! Moved from URBPARM.TBL
!
!  BETR [-]            : minimum moisture availability of roof
!  BETB [-]            : minimum moisture availability of building wall
!  BETG [-]            : minimum moisture availability of road
!  Z0R [m]                : roughness length for momentum of roof
!  Z0HB [m]               : roughness length for heat of building wall (only for CH_SCHEME = 1)
!  Z0HG [m]               : roughness length for heat of road
!  num_roof_layers        : number of layers within roof
!  num_wall_layers        : number of layers within building walls
!  num_road_layers        : number of layers within below road surface
!   NOTE: for now, these layers are defined as same as the number of soil layers in namelist.input
!
! References:
!     Kusaka and Kimura (2004) J.Appl.Meteor., vol.43, p1899-1910
!     Kusaka and Kimura (2004) J.Meteor.Soc.Japan, vol.82, p45-65
!     Kusaka et al. (2001) Bound.-Layer Meteor., vol.101, p329-358
!
! History:
!     2006/06          modified by H. Kusaka (Univ. Tsukuba), M. Tewari 
!     2005/10/26,      modified by Fei Chen, Mukul Tewari
!     2003/07/21 WRF , modified  by H. Kusaka of CRIEPI (NCAR/MMM)
!     2001/08/26 PhD , modified  by H. Kusaka of CRIEPI (Univ.Tsukuba)
!     1999/08/25 LCM , developed by H. Kusaka of CRIEPI (Univ.Tsukuba)
!
!===============================================================================
!
!  subroutine urban:
!
!===============================================================================

   SUBROUTINE urban(LSOLAR,                                           & ! L
                    num_roof_layers,num_wall_layers,num_road_layers,  & ! I
                    DZR,DZB,DZG,                                      & ! I
                    UTYPE,TA,QA,UA,U1,V1,SSG,SSGD,SSGQ,LLG,RAIN,RHOO, & ! I
                    ZA,DECLIN,COSZ,OMG,XLAT,DELT,ZNT,                 & ! I
                    CHS, CHS2,                                        & ! I
                    TR, TB, TG, TC, QC, UC,                           & ! H
                    TRL,TBL,TGL,                                      & ! H
                    XXXR, XXXB, XXXG, XXXC,                           & ! H
                    TS,QS,SH,LH,LH_KINEMATIC,                         & ! O
                    SW,ALB,LW,G,RN,PSIM,PSIH,                         & ! O
                    GZ1OZ0,                                           & ! O
                    CMR_URB,CHR_URB,CMC_URB,CHC_URB,                  & ! I/O
                    U10,V10,TH2,Q2,UST,mh_urb,stdh_urb,lf_urb,        & ! O 
                    lp_urb,hgt_urb,frc_urb,lb_urb,zo_check)

   IMPLICIT NONE

   REAL, PARAMETER    :: CP=0.24          ! heat capacity of dry air  [cgs unit]
   REAL, PARAMETER    :: EL=583.          ! latent heat of vaporation [cgs unit]
   REAL, PARAMETER    :: SIG=8.17E-11     ! stefun bolzman constant   [cgs unit]
   REAL, PARAMETER    :: SIG_SI=5.67E-8   !                           [MKS unit]
   REAL, PARAMETER    :: AK=0.4           ! kalman const.                [-]
   REAL, PARAMETER    :: PI=3.14159       ! pi                           [-]
   REAL, PARAMETER    :: TETENA=7.5       ! const. of Tetens Equation    [-]
   REAL, PARAMETER    :: TETENB=237.3     ! const. of Tetens Equation    [-]
   REAL, PARAMETER    :: SRATIO=0.75      ! ratio between direct/total solar [-]

   REAL, PARAMETER    :: CPP=1004.5       ! heat capacity of dry air    [J/K/kg]
   REAL, PARAMETER    :: ELL=2.442E+06    ! latent heat of vaporization [J/kg]
   REAL, PARAMETER    :: XKA=2.4E-5

!-------------------------------------------------------------------------------
! C: configuration variables
!-------------------------------------------------------------------------------

   LOGICAL, INTENT(IN) :: LSOLAR  ! logical    [true=both, false=SSG only]

!  The following variables are also model configuration variables, but are 
!  defined in the URBAN.TBL and in the contains statement in the top of 
!  the module_urban_init, so we should not declare them here.

  INTEGER, INTENT(IN) :: num_roof_layers
  INTEGER, INTENT(IN) :: num_wall_layers
  INTEGER, INTENT(IN) :: num_road_layers


  REAL, INTENT(IN), DIMENSION(1:num_roof_layers) :: DZR ! grid interval of roof layers [cm]
  REAL, INTENT(IN), DIMENSION(1:num_wall_layers) :: DZB ! grid interval of wall layers [cm]
  REAL, INTENT(IN), DIMENSION(1:num_road_layers) :: DZG ! grid interval of road layers [cm]

!-------------------------------------------------------------------------------
! I: input variables from LSM to Urban
!-------------------------------------------------------------------------------

   INTEGER, INTENT(IN) :: UTYPE ! urban type [1=Commercial/Industrial, 2=High-intensity residential, 
                                ! 3=low-intensity residential]

   REAL, INTENT(IN)    :: TA   ! potential temp at 1st atmospheric level [K]
   REAL, INTENT(IN)    :: QA   ! mixing ratio at 1st atmospheric level  [kg/kg]
   REAL, INTENT(IN)    :: UA   ! wind speed at 1st atmospheric level    [m/s]
   REAL, INTENT(IN)    :: U1   ! u at 1st atmospheric level             [m/s]
   REAL, INTENT(IN)    :: V1   ! v at 1st atmospheric level             [m/s]
   REAL, INTENT(IN)    :: SSG  ! downward total short wave radiation    [W/m/m]
   REAL, INTENT(IN)    :: LLG  ! downward long wave radiation           [W/m/m]
   REAL, INTENT(IN)    :: RAIN ! precipitation                          [mm/h]
   REAL, INTENT(IN)    :: RHOO ! air density                            [kg/m^3]
   REAL, INTENT(IN)    :: ZA   ! first atmospheric level                [m]
   REAL, INTENT(IN)    :: DECLIN ! solar declination                    [rad]
   REAL, INTENT(IN)    :: COSZ ! sin(fai)*sin(del)+cos(fai)*cos(del)*cos(omg)
   REAL, INTENT(IN)    :: OMG  ! solar hour angle                       [rad]

   REAL, INTENT(IN)    :: XLAT ! latitude                               [deg]
   REAL, INTENT(IN)    :: DELT ! time step                              [s]
   REAL, INTENT(IN)    :: CHS,CHS2 ! CH*U at za and 2 m             [m/s]

   REAL, INTENT(INOUT) :: SSGD ! downward direct short wave radiation   [W/m/m]
   REAL, INTENT(INOUT) :: SSGQ ! downward diffuse short wave radiation  [W/m/m]
   REAL, INTENT(INOUT) :: CMR_URB
   REAL, INTENT(INOUT) :: CHR_URB
   REAL, INTENT(INOUT) :: CMC_URB
   REAL, INTENT(INOUT) :: CHC_URB
   REAL, INTENT(INOUT) :: ZNT  ! roughness length                       [m]    ! modified by danli
!-------------------------------------------------------------------------------
! I: NUDAPT Input Parameters
!-------------------------------------------------------------------------------
   REAL, INTENT(INOUT) :: mh_urb   ! mean building height [m]
   REAL, INTENT(INOUT) :: stdh_urb ! standard deviation of building height [m]
   REAL, INTENT(INOUT) :: hgt_urb  ! area weighted mean building height [m] 
   REAL, INTENT(INOUT) :: lp_urb   ! plan area fraction [-]
   REAL, INTENT(INOUT) :: frc_urb  ! urban fraction [-]
   REAL, INTENT(INOUT) :: lb_urb   ! building surface to plan area ratio [-]
   REAL, INTENT(INOUT), DIMENSION(4) :: lf_urb   ! frontal area index [-]
   REAL, INTENT(INOUT) :: zo_check  ! check for printing ZOC

!-------------------------------------------------------------------------------
! O: output variables from Urban to LSM 
!-------------------------------------------------------------------------------

   REAL, INTENT(OUT) :: TS     ! surface potential temperature    [K]
   REAL, INTENT(OUT) :: QS     ! surface humidity                 [K]
   REAL, INTENT(OUT) :: SH     ! sensible heat flux               [W/m/m]
   REAL, INTENT(OUT) :: LH     ! latent heat flux                 [W/m/m]
   REAL, INTENT(OUT) :: LH_KINEMATIC ! latent heat, kinetic     [kg/m/m/s]
   REAL, INTENT(OUT) :: SW     ! upward short wave radiation flux [W/m/m]
   REAL, INTENT(OUT) :: ALB    ! time-varying albedo            [fraction]
   REAL, INTENT(OUT) :: LW     ! upward long wave radiation flux  [W/m/m]
   REAL, INTENT(OUT) :: G      ! heat flux into the ground        [W/m/m]
   REAL, INTENT(OUT) :: RN     ! net radition                     [W/m/m]
   REAL, INTENT(OUT) :: PSIM   ! similality stability shear function for momentum
   REAL, INTENT(OUT) :: PSIH   ! similality stability shear function for heat
   REAL, INTENT(OUT) :: GZ1OZ0   
   REAL, INTENT(OUT) :: U10    ! u at 10m                         [m/s]
   REAL, INTENT(OUT) :: V10    ! u at 10m                         [m/s]
   REAL, INTENT(OUT) :: TH2    ! potential temperature at 2 m     [K]
   REAL, INTENT(OUT) :: Q2     ! humidity at 2 m                  [-]
!m   REAL, INTENT(OUT) :: CHS,CHS2 ! CH*U at za and 2 m             [m/s]
   REAL, INTENT(OUT) :: UST    ! friction velocity                [m/s]


!-------------------------------------------------------------------------------
! H: Historical (state) variables of Urban : LSM <--> Urban
!-------------------------------------------------------------------------------

! TR: roof temperature              [K];      TRP: at previous time step [K]
! TB: building wall temperature     [K];      TBP: at previous time step [K]
! TG: road temperature              [K];      TGP: at previous time step [K]
! TC: urban-canopy air temperature  [K];      TCP: at previous time step [K]
!                                                  (absolute temperature)
! QC: urban-canopy air mixing ratio [kg/kg];  QCP: at previous time step [kg/kg]
!
! XXXR: Monin-Obkhov length for roof          [dimensionless]
! XXXB: Monin-Obkhov length for building wall [dimensionless]
! XXXG: Monin-Obkhov length for road          [dimensionless]
! XXXC: Monin-Obkhov length for urban-canopy  [dimensionless]
!
! TRL, TBL, TGL: layer temperature [K] (absolute temperature)

   REAL, INTENT(INOUT):: TR, TB, TG, TC, QC, UC
   REAL, INTENT(INOUT):: XXXR, XXXB, XXXG, XXXC

   REAL, DIMENSION(1:num_roof_layers), INTENT(INOUT) :: TRL
   REAL, DIMENSION(1:num_wall_layers), INTENT(INOUT) :: TBL
   REAL, DIMENSION(1:num_road_layers), INTENT(INOUT) :: TGL

!-------------------------------------------------------------------------------
! L:  Local variables from read_param
!-------------------------------------------------------------------------------

   REAL :: ZR, Z0C, Z0HC, ZDC, SVF, R, RW, HGT, AH
   REAL :: SIGMA_ZED
   REAL :: CAPR, CAPB, CAPG, AKSR, AKSB, AKSG, ALBR, ALBB, ALBG 
   REAL :: EPSR, EPSB, EPSG, Z0R,  Z0B,  Z0G,  Z0HB, Z0HG
   REAL :: TRLEND,TBLEND,TGLEND
   REAL :: T1VR, T1VC,TH2V
   REAL :: RLMO_URB
   REAL :: AKANDA_URBAN
   
   REAL :: TH2X                                                !m
   
   INTEGER :: BOUNDR, BOUNDB, BOUNDG
   INTEGER :: CH_SCHEME, TS_SCHEME

   LOGICAL :: SHADOW  ! [true=consider svf and shadow effects, false=consider svf effect only]

!for BEP
   INTEGER                        :: NUMDIR
   REAL,    DIMENSION ( MAXDIRS ) :: STREET_DIRECTION
   REAL,    DIMENSION ( MAXDIRS ) :: STREET_WIDTH
   REAL,    DIMENSION ( MAXDIRS ) :: BUILDING_WIDTH
   INTEGER                        :: NUMHGT
   REAL,    DIMENSION ( MAXHGTS ) :: HEIGHT_BIN
   REAL,    DIMENSION ( MAXHGTS ) :: HPERCENT_BIN
!end BEP
!-------------------------------------------------------------------------------
! L: Local variables
!-------------------------------------------------------------------------------

   REAL :: BETR, BETB, BETG
   REAL :: SX, SD, SQ, RX
   REAL :: UR, ZC, XLB, BB
   REAL :: Z, RIBB, RIBG, RIBC, BHR, BHB, BHG, BHC
   REAL :: TSC, LNET, SNET, FLXUV, THG, FLXTH, FLXHUM, FLXG
   REAL :: W, VFGS, VFGW, VFWG, VFWS, VFWW
   REAL :: HOUI1, HOUI2, HOUI3, HOUI4, HOUI5, HOUI6, HOUI7, HOUI8
   REAL :: SLX, SLX1, SLX2, SLX3, SLX4, SLX5, SLX6, SLX7, SLX8
   REAL :: FLXTHR, FLXTHB, FLXTHG, FLXHUMR, FLXHUMB, FLXHUMG
   REAL :: SR, SB, SG, RR, RB, RG
   REAL :: SR1, SR2, SB1, SB2, SG1, SG2, RR1, RR2, RB1, RB2, RG1, RG2
   REAL :: HR, HB, HG, ELER, ELEB, ELEG, G0R, G0B, G0G
   REAL :: ALPHAC, ALPHAR, ALPHAB, ALPHAG
   REAL :: CHC, CHR, CHB, CHG, CDC, CDR, CDB, CDG
   REAL :: C1R, C1B, C1G, TE, TC1, TC2, QC1, QC2, QS0R, QS0B, QS0G,RHO,ES

   REAL :: DESDT
   REAL :: F
   REAL :: DQS0RDTR
   REAL :: DRRDTR, DHRDTR, DELERDTR, DG0RDTR
   REAL :: DTR, DFDT
   REAL :: FX, FY, GF, GX, GY
   REAL :: DTCDTB, DTCDTG
   REAL :: DQCDTB, DQCDTG
   REAL :: DRBDTB1,  DRBDTG1,  DRBDTB2,  DRBDTG2
   REAL :: DRGDTB1,  DRGDTG1,  DRGDTB2,  DRGDTG2
   REAL :: DRBDTB,   DRBDTG,   DRGDTB,   DRGDTG
   REAL :: DHBDTB,   DHBDTG,   DHGDTB,   DHGDTG
   REAL :: DELEBDTB, DELEBDTG, DELEGDTG, DELEGDTB
   REAL :: DG0BDTB,  DG0BDTG,  DG0GDTG,  DG0GDTB
   REAL :: DQS0BDTB, DQS0GDTG
   REAL :: DTB, DTG, DTC

   REAL :: THEATAZ    ! Solar Zenith Angle [rad]
   REAL :: THEATAS    ! = PI/2. - THETAZ
   REAL :: FAI        ! Latitude [rad]
   REAL :: CNT,SNT
   REAL :: PS         ! Surface Pressure [hPa]
   REAL :: TAV        ! Vertial Temperature [K] 

   REAL :: XXX, X, Z0, Z0H, CD, CH
   REAL :: XXX2, PSIM2, PSIH2, XXX10, PSIM10, PSIH10
   REAL :: PSIX, PSIT, PSIX2, PSIT2, PSIX10, PSIT10

   REAL :: TRP, TBP, TGP, TCP, QCP, TST, QST

   REAL :: WDR,HGT2,BW,DHGT
   REAL, parameter :: VonK = 0.4
   REAL :: lambda_f,alpha_macd,beta_macd,lambda_fr

   INTEGER :: iteration, K, NUDAPT 
   INTEGER :: tloc

!-------------------------------------------------------------------------------
! Set parameters
!-------------------------------------------------------------------------------

! Miao, 2007/01/17, cal. ah
   if(ahoption==1) then
     tloc=mod(int(OMG/PI*180./15.+12.+0.5 ),24)
     if(tloc==0) tloc=24
   endif

   CALL read_param(UTYPE,ZR,SIGMA_ZED,Z0C,Z0HC,ZDC,SVF,R,RW,HGT,  &
                   AH,CAPR,CAPB,CAPG,AKSR,AKSB,AKSG,ALBR,ALBB,    &
                   ALBG,EPSR,EPSB,EPSG,Z0R,Z0B,Z0G,Z0HB,Z0HG,     &
                   BETR,BETB,BETG,TRLEND,TBLEND,TGLEND,           &
!for BEP
                   NUMDIR, STREET_DIRECTION, STREET_WIDTH,        & 
                   BUILDING_WIDTH, NUMHGT, HEIGHT_BIN,            & 
                   HPERCENT_BIN,                                  & 
!end BEP
                   BOUNDR,BOUNDB,BOUNDG,CH_SCHEME,TS_SCHEME,      &
                   AKANDA_URBAN)

! Glotfelty, 2012/07/05, NUDAPT Modification

 if(mh_urb.gt.0.0)THEN 
  !write(mesg,*) 'Mean Height NUDAPT',mh_urb
  !call wrf_message(mesg)
  !write(mesg,*) 'Mean Height Table',ZR
  !call wrf_message(mesg)
  if(zo_check.eq.1)THEN
   write(mesg,*) 'Mean Height NUDAPT',mh_urb
   call wrf_message(mesg)
   write(mesg,*) 'Mean Height Table',ZR
   call wrf_message(mesg)
   write(mesg,*) 'Roughness Length Table',Z0C
   call wrf_message(mesg)
   write(mesg,*) 'Roof Roughness Length Table',Z0R
   call wrf_message(mesg)
   write(mesg,*) 'Sky View Factor Table',SVF
   call wrf_message(mesg)
   write(mesg,*) 'Normalized Height Table',HGT
   call wrf_message(mesg)
   write(mesg,*) 'Plan Area Fraction', lp_urb
   call wrf_message(mesg)
   write(mesg,*) 'Plan Area Fraction table', R
   call wrf_message(mesg)
  end if
  !write(mesg,*) 'Area Weighted Mean Height',hgt_urb
  !call wrf_message(mesg)
  !write(mesg,*) 'Plan Area Fraction', lp_urb
  !call wrf_message(mesg)
  !write(mesg,*) 'STD Height', stdh_urb
  !call wrf_message(mesg)
  !write(mesg,*) 'Frontal Area Index',lf_urb
  !call wrf_message(mesg)
  !write(mesg,*) 'Urban Fraction',frc_urb
  !call wrf_message(mesg)
  !write(mesg,*) 'Building Surf Ratio',lb_urb
  !call wrf_message(mesg)
 
 !Calculate Building Width and Street Width Based on BEP formulation
 if(lb_urb.gt.lp_urb)THEN
  BW=2.*hgt_urb*lp_urb/(lb_urb-lp_urb)
  SW=2.*hgt_urb*lp_urb*((frc_urb/lp_urb)-1.)/(lb_urb-lp_urb)
  !write(mesg,*) 'Building Width',BW
  !call wrf_message(mesg)
  !write(mesg,*) 'Street Width',SW
  !call wrf_message(mesg)
 elseif (SW.lt.0.0.or.BW.lt.0.0)then
  BW=BUILDING_WIDTH(1)
  SW=STREET_WIDTH(1)
 else
    BW=BUILDING_WIDTH(1)
  SW=STREET_WIDTH(1)
 end if
 
 !Assign NUDAPT Parameters
   ZR = mh_urb
    R = lp_urb
    RW = 1.0-R
   HGT = mh_urb/(BW+SW)
   SIGMA_ZED = stdh_urb

 !Calculate Wind Direction and Assign Appropriae lf_urb
   !WDR = (180.0/PI)*ATAN2(U10,V10)
   
   IF(WDR.ge.0.0.and.WDR.lt.22.5)THEN
     lambda_f = lf_urb(1)
   ELSEIF(WDR.ge.-22.5.and.WDR.lt.0.0)THEN
     lambda_f = lf_urb(1)
   ELSEIF(WDR.gt.157.5.and.WDR.le.180.0)THEN
     lambda_f = lf_urb(1)
   ELSEIF(WDR.lt.-157.5)THEN
     lambda_f = lf_urb(1)
   ELSEIF(WDR.gt.22.5.and.WDR.le.67.5)THEN
     lambda_f = lf_urb(2)
   ELSEIF(WDR.ge.-67.5.and.WDR.lt.-22.5)THEN
     lambda_f = lf_urb(2)
   ELSEIF(WDR.gt.67.5.and.WDR.le.112.5)THEN
     lambda_f = lf_urb(3)
   ELSEIF(WDR.ge.-112.5.and.WDR.lt.-67.5)THEN
     lambda_f = lf_urb(3)
   ELSEIF(WDR.gt.112.5.and.WDR.le.157.5)THEN
     lambda_f = lf_urb(4)
   ELSEIF(WDR.ge.-157.5.and.WDR.lt.-112.5)THEN
     lambda_f = lf_urb(4)
   ELSE
     lambda_f = lf_urb(1)
   ENDIF

  !Calculate the following urban canyon geometry parameters following Macdonald's (1998) formulations
      Cd         = 1.2
      alpha_macd = 4.43
      beta_macd  = 1.0


      ZDC = ZR * ( 1.0 + ( alpha_macd ** ( -R ) )  * ( R - 1.0 ) )

      Z0C = ZR * ( 1.0 - ZDC/ZR ) * &
           exp (-(0.5 * beta_macd * Cd / (VonK**2) * ( 1.0-ZDC/ZR) * lambda_f )**(-0.5))
     
      if(zo_check.eq.1)THEN
        write(mesg,*) 'Roughness Length NUDAPT',Z0C
        call wrf_message(mesg)
      end if

      lambda_fr  = stdh_urb/(SW + BW)

       Z0R = ZR * ( 1.0 - ZDC/ZR) &
             * exp ( -(0.5 * beta_macd * Cd / (VonK**2) &
              * ( 1.0-ZDC/ZR) * lambda_fr )**(-0.5))

    

       Z0HC = 0.1 * Z0C

  ! Calculate Sky View Factor

     DHGT=HGT/100.
     HGT2=0.
     VFWS=0.
     HGT2=HGT-DHGT/2.
      do NUDAPT=1,99
         HGT2=HGT2-DHGT
         VFWS=VFWS+0.25*(1.-HGT2/SQRT(HGT2**2.+RW**2.))
      end do

     VFWS=VFWS/99.
     VFWS=VFWS*2.

     VFGS=1.-2.*VFWS*HGT/RW
     SVF=VFGS

    if(zo_check.eq.1)THEN
      write(mesg,*) 'Roof Roughness Length NUDAPT',Z0R
      call wrf_message(mesg)
      write(mesg,*) 'Sky View Factor NUDAPT',SVF
      call wrf_message(mesg)
      write(mesg,*) 'normalized Height NUDAPT', HGT
      call wrf_message(mesg)
    end if


 endif

 !End NUDAPT Modification


! Miao, 2007/01/17, cal. ah
   if(ahoption==1) AH=AH*ahdiuprf(tloc)

   IF( ZDC+Z0C+2. >= ZA) THEN
    CALL wrf_error_fatal ("ZDC + Z0C + 2m is larger than the 1st WRF level "// &
                           "Stop in subroutine urban - change ZDC and Z0C" ) 
   END IF

   IF(.NOT.LSOLAR) THEN
     SSGD = SRATIO*SSG
     SSGQ = SSG - SSGD
   ENDIF
   SSGD = SRATIO*SSG   ! No radiation scheme has SSGD and SSGQ.
   SSGQ = SSG - SSGD

   W=2.*1.*HGT
   VFGS=SVF
   VFGW=1.-SVF
   VFWG=(1.-SVF)*(1.-R)/W
   VFWS=VFWG
   VFWW=1.-2.*VFWG

!-------------------------------------------------------------------------------
! Convert unit from MKS to cgs
! Renew surface and layer temperatures
!-------------------------------------------------------------------------------

   SX=(SSGD+SSGQ)/697.7/60.  ! downward short wave radition [ly/min]
   SD=SSGD/697.7/60.         ! downward direct short wave radiation
   SQ=SSGQ/697.7/60.         ! downward diffuse short wave radiation
   RX=LLG/697.7/60.          ! downward long wave radiation
   RHO=RHOO*0.001            ! air density at first atmospheric level

   TRP=TR
   TBP=TB
   TGP=TG
   TCP=TC
   QCP=QC

   TAV=TA*(1.+0.61*QA)
   PS=RHOO*287.*TAV/100. ![hPa]

!-------------------------------------------------------------------------------
! Canopy wind
!-------------------------------------------------------------------------------

   IF ( ZR + 2. < ZA ) THEN
     UR=UA*LOG((ZR-ZDC)/Z0C)/LOG((ZA-ZDC)/Z0C)
     ZC=0.7*ZR
     XLB=0.4*(ZR-ZDC)
     ! BB formulation from Inoue (1963)
     BB = 0.4 * ZR / ( XLB * alog( ( ZR - ZDC ) / Z0C ) )
     UC=UR*EXP(-BB*(1.-ZC/ZR))
   ELSE
     ! PRINT *, 'Warning ZR + 2m  is larger than the 1st WRF level' 
     ZC=ZA/2.
     UC=UA/2.
   END IF

!-------------------------------------------------------------------------------
! Net Short Wave Radiation at roof, wall, and road
!-------------------------------------------------------------------------------

   SHADOW = .false.
!   SHADOW = .true.

   IF (SSG > 0.0) THEN

     IF(.NOT.SHADOW) THEN              ! no shadow effects model

       SR1=SX*(1.-ALBR)
       SG1=SX*VFGS*(1.-ALBG)
       SB1=SX*VFWS*(1.-ALBB)
       SG2=SB1*ALBB/(1.-ALBB)*VFGW*(1.-ALBG)
       SB2=SG1*ALBG/(1.-ALBG)*VFWG*(1.-ALBB)

     ELSE                              ! shadow effects model

       FAI=XLAT*PI/180.

       THEATAS=ABS(ASIN(COSZ))
       THEATAZ=ABS(ACOS(COSZ))

       SNT=COS(DECLIN)*SIN(OMG)/COS(THEATAS)
       CNT=(COSZ*SIN(FAI)-SIN(DECLIN))/COS(THEATAS)/COS(FAI)

       HOUI1=(SNT*COS(PI/8.)    -CNT*SIN(PI/8.))
       HOUI2=(SNT*COS(2.*PI/8.) -CNT*SIN(2.*PI/8.))
       HOUI3=(SNT*COS(3.*PI/8.) -CNT*SIN(3.*PI/8.))
       HOUI4=(SNT*COS(4.*PI/8.) -CNT*SIN(4.*PI/8.))
       HOUI5=(SNT*COS(5.*PI/8.) -CNT*SIN(5.*PI/8.))
       HOUI6=(SNT*COS(6.*PI/8.) -CNT*SIN(6.*PI/8.))
       HOUI7=(SNT*COS(7.*PI/8.) -CNT*SIN(7.*PI/8.))
       HOUI8=(SNT*COS(8.*PI/8.) -CNT*SIN(8.*PI/8.))

       SLX1=HGT*ABS(TAN(THEATAZ))*ABS(HOUI1)
       SLX2=HGT*ABS(TAN(THEATAZ))*ABS(HOUI2)
       SLX3=HGT*ABS(TAN(THEATAZ))*ABS(HOUI3)
       SLX4=HGT*ABS(TAN(THEATAZ))*ABS(HOUI4)
       SLX5=HGT*ABS(TAN(THEATAZ))*ABS(HOUI5)
       SLX6=HGT*ABS(TAN(THEATAZ))*ABS(HOUI6)
       SLX7=HGT*ABS(TAN(THEATAZ))*ABS(HOUI7)
       SLX8=HGT*ABS(TAN(THEATAZ))*ABS(HOUI8)

       IF(SLX1 > RW) SLX1=RW
       IF(SLX2 > RW) SLX2=RW
       IF(SLX3 > RW) SLX3=RW
       IF(SLX4 > RW) SLX4=RW
       IF(SLX5 > RW) SLX5=RW
       IF(SLX6 > RW) SLX6=RW
       IF(SLX7 > RW) SLX7=RW
       IF(SLX8 > RW) SLX8=RW

       SLX=(SLX1+SLX2+SLX3+SLX4+SLX5+SLX6+SLX7+SLX8)/8.

       SR1=SD*(1.-ALBR)+SQ*(1.-ALBR)
       SG1=SD*(RW-SLX)/RW*(1.-ALBG)+SQ*VFGS*(1.-ALBG)
       SB1=SD*SLX/W*(1.-ALBB)+SQ*VFWS*(1.-ALBB)
       SG2=SB1*ALBB/(1.-ALBB)*VFGW*(1.-ALBG)
       SB2=SG1*ALBG/(1.-ALBG)*VFWG*(1.-ALBB)

     END IF

     SR=SR1
     SG=SG1+SG2
     SB=SB1+SB2

     SNET=R*SR+W*SB+RW*SG

   ELSE

     SR=0.
     SG=0.
     SB=0.
     SNET=0.

   END IF

!-------------------------------------------------------------------------------
! Roof
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! CHR, CDR, BETR
!-------------------------------------------------------------------------------

   ! Z=ZA-ZDC
   ! BHR=LOG(Z0R/Z0HR)/0.4
   ! RIBR=(9.8*2./(TA+TRP))*(TA-TRP)*(Z+Z0R)/(UA*UA)
   ! CALL mos(XXXR,ALPHAR,CDR,BHR,RIBR,Z,Z0R,UA,TA,TRP,RHO)

   ! Alternative option for MOST using SFCDIF routine from Noah
   ! Virtual temperatures needed by SFCDIF
   T1VR = TRP* (1.0+ 0.61 * QA) 
   TH2V = (TA + ( 0.0098 * ZA)) * (1.0+ 0.61 * QA)
  
   ! note that CHR_URB contains UA (=CHR_MOS*UA)
   RLMO_URB=0.0
   CALL SFCDIF_URB (ZA,Z0R,T1VR,TH2V,UA,AKANDA_URBAN,CMR_URB,CHR_URB,RLMO_URB,CDR)
   ALPHAR =  RHO*CP*CHR_URB
   CHR=ALPHAR/RHO/CP/UA

   IF(RAIN > 1.) BETR=0.7  

   IF (TS_SCHEME == 1) THEN 

!-------------------------------------------------------------------------------
! TR  Solving Non-Linear Equation by Newton-Rapson
! TRL Solving Heat Equation by Tri Diagonal Matrix Algorithm  
!-------------------------------------------------------------------------------
!      TSC=TRP-273.15                          
!      ES=EXP(19.482-4303.4/(TSC+243.5))        ! WMO
!      ES=6.11*10.**(TETENA*TSC/(TETENB+TSC))   ! Tetens
!      DESDT=( 6.1078*(2500.-2.4*TSC)/ &        ! Tetens
!            (0.46151*(TSC+273.15)**2.) )*10.**(7.5*TSC/(237.3+TSC)) 
!      ES=6.11*EXP((2.5*10.**6./461.51)*(TRP-273.15)/(273.15*TRP) ) ! Clausius-Clapeyron 
!      DESDT=(2.5*10.**6./461.51)*ES/(TRP**2.)                      ! Clausius-Clapeyron
!      QS0R=0.622*ES/(PS-0.378*ES) 
!      DQS0RDTR = DESDT*0.622*PS/((PS-0.378*ES)**2.) 
!      DQS0RDTR = 17.269*(273.15-35.86)/((TRP-35.86)**2.)*QS0R

!    TRP=350.

     DO ITERATION=1,20

       ES=6.11*EXP( (2.5*10.**6./461.51)*(TRP-273.15)/(273.15*TRP) )
       DESDT=(2.5*10.**6./461.51)*ES/(TRP**2.)
       QS0R=0.622*ES/(PS-0.378*ES) 
       DQS0RDTR = DESDT*0.622*PS/((PS-0.378*ES)**2.) 

       RR=EPSR*(RX-SIG*(TRP**4.)/60.)
       HR=RHO*CP*CHR*UA*(TRP-TA)*100.
       ELER=RHO*EL*CHR*UA*BETR*(QS0R-QA)*100.
       G0R=AKSR*(TRP-TRL(1))/(DZR(1)/2.)

       F = SR + RR - HR - ELER - G0R

       DRRDTR = (-4.*EPSR*SIG*TRP**3.)/60.
       DHRDTR = RHO*CP*CHR*UA*100.
       DELERDTR = RHO*EL*CHR*UA*BETR*DQS0RDTR*100.
       DG0RDTR =  2.*AKSR/DZR(1)

       DFDT = DRRDTR - DHRDTR - DELERDTR - DG0RDTR 
       DTR = F/DFDT

       TR = TRP - DTR
       TRP = TR

       IF( ABS(F) < 0.000001 .AND. ABS(DTR) < 0.000001 ) EXIT

     END DO

! multi-layer heat equation model

     CALL multi_layer(num_roof_layers,BOUNDR,G0R,CAPR,AKSR,TRL,DZR,DELT,TRLEND)

   ELSE

     ES=6.11*EXP( (2.5*10.**6./461.51)*(TRP-273.15)/(273.15*TRP) )
     QS0R=0.622*ES/(PS-0.378*ES)        

     RR=EPSR*(RX-SIG*(TRP**4.)/60.)
     HR=RHO*CP*CHR*UA*(TRP-TA)*100.
     ELER=RHO*EL*CHR*UA*BETR*(QS0R-QA)*100.
     G0R=SR+RR-HR-ELER

     CALL force_restore(CAPR,AKSR,DELT,SR,RR,HR,ELER,TRLEND,TRP,TR)

     TRP=TR

   END IF

   FLXTHR=HR/RHO/CP/100.
   FLXHUMR=ELER/RHO/EL/100.

!-------------------------------------------------------------------------------
!  Wall and Road 
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!  CHC, CHB, CDB, BETB, CHG, CDG, BETG
!-------------------------------------------------------------------------------

   ! Z=ZA-ZDC
   ! BHC=LOG(Z0C/Z0HC)/0.4
   ! RIBC=(9.8*2./(TA+TCP))*(TA-TCP)*(Z+Z0C)/(UA*UA)
   !
   ! CALL mos(XXXC,ALPHAC,CDC,BHC,RIBC,Z,Z0C,UA,TA,TCP,RHO)
   ! Virtual temperatures needed by SFCDIF routine from Noah

   T1VC = TCP* (1.0+ 0.61 * QA) 
   RLMO_URB=0.0
   CALL SFCDIF_URB(ZA,Z0C,T1VC,TH2V,UA,AKANDA_URBAN,CMC_URB,CHC_URB,RLMO_URB,CDC)
   ALPHAC =  RHO*CP*CHC_URB

   IF (CH_SCHEME == 1) THEN 

     Z=ZDC
     BHB=LOG(Z0B/Z0HB)/0.4
     BHG=LOG(Z0G/Z0HG)/0.4
     RIBB=(9.8*2./(TCP+TBP))*(TCP-TBP)*(Z+Z0B)/(UC*UC)
     RIBG=(9.8*2./(TCP+TGP))*(TCP-TGP)*(Z+Z0G)/(UC*UC)

     CALL mos(XXXB,ALPHAB,CDB,BHB,RIBB,Z,Z0B,UC,TCP,TBP,RHO)
     CALL mos(XXXG,ALPHAG,CDG,BHG,RIBG,Z,Z0G,UC,TCP,TGP,RHO)

   ELSE

     ALPHAB=RHO*CP*(6.15+4.18*UC)/1200.
     IF(UC > 5.) ALPHAB=RHO*CP*(7.51*UC**0.78)/1200.
     ALPHAG=RHO*CP*(6.15+4.18*UC)/1200.
     IF(UC > 5.) ALPHAG=RHO*CP*(7.51*UC**0.78)/1200.

   END IF

   CHC=ALPHAC/RHO/CP/UA
   CHB=ALPHAB/RHO/CP/UC
   CHG=ALPHAG/RHO/CP/UC

   BETB=0.0
   IF(RAIN > 1.) BETG=0.7

   IF (TS_SCHEME == 1) THEN

!-------------------------------------------------------------------------------
!  TB, TG  Solving Non-Linear Simultaneous Equation by Newton-Rapson
!  TBL,TGL Solving Heat Equation by Tri Diagonal Matrix Algorithm  
!-------------------------------------------------------------------------------

!    TBP=350.
!    TGP=350.

     DO ITERATION=1,20

       ES=6.11*EXP( (2.5*10.**6./461.51)*(TBP-273.15)/(273.15*TBP) ) 
       DESDT=(2.5*10.**6./461.51)*ES/(TBP**2.)
       QS0B=0.622*ES/(PS-0.378*ES) 
       DQS0BDTB=DESDT*0.622*PS/((PS-0.378*ES)**2.)

       ES=6.11*EXP( (2.5*10.**6./461.51)*(TGP-273.15)/(273.15*TGP) ) 
       DESDT=(2.5*10.**6./461.51)*ES/(TGP**2.)
       QS0G=0.622*ES/(PS-0.378*ES)        
       DQS0GDTG=DESDT*0.22*PS/((PS-0.378*ES)**2.) 

       RG1=EPSG*( RX*VFGS          &
       +EPSB*VFGW*SIG*TBP**4./60.  &
       -SIG*TGP**4./60. )

       RB1=EPSB*( RX*VFWS         &
       +EPSG*VFWG*SIG*TGP**4./60. &
       +EPSB*VFWW*SIG*TBP**4./60. &
       -SIG*TBP**4./60. )

       RG2=EPSG*( (1.-EPSB)*(1.-SVF)*VFWS*RX                  &
       +(1.-EPSB)*(1.-SVF)*VFWG*EPSG*SIG*TGP**4./60.          &
       +EPSB*(1.-EPSB)*(1.-SVF)*(1.-2.*VFWS)*SIG*TBP**4./60. )

       RB2=EPSB*( (1.-EPSG)*VFWG*VFGS*RX                          &
       +(1.-EPSG)*EPSB*VFGW*VFWG*SIG*(TBP**4.)/60.                &
       +(1.-EPSB)*VFWS*(1.-2.*VFWS)*RX                            &
       +(1.-EPSB)*VFWG*(1.-2.*VFWS)*EPSG*SIG*EPSG*TGP**4./60.     &
       +EPSB*(1.-EPSB)*(1.-2.*VFWS)*(1.-2.*VFWS)*SIG*TBP**4./60. )

       RG=RG1+RG2
       RB=RB1+RB2

       DRBDTB1=EPSB*(4.*EPSB*SIG*TB**3.*VFWW-4.*SIG*TB**3.)/60.
       DRBDTG1=EPSB*(4.*EPSG*SIG*TG**3.*VFWG)/60.
       DRBDTB2=EPSB*(4.*(1.-EPSG)*EPSB*SIG*TB**3.*VFGW*VFWG &
               +4.*EPSB*(1.-EPSB)*SIG*TB**3.*VFWW*VFWW)/60.
       DRBDTG2=EPSB*(4.*(1.-EPSB)*EPSG*SIG*TG**3.*VFWG*VFWW)/60.

       DRGDTB1=EPSG*(4.*EPSB*SIG*TB**3.*VFGW)/60.
       DRGDTG1=EPSG*(-4.*SIG*TG**3.)/60.
       DRGDTB2=EPSG*(4.*EPSB*(1.-EPSB)*SIG*TB**3.*VFWW*VFGW)/60.
       DRGDTG2=EPSG*(4.*(1.-EPSB)*EPSG*SIG*TG**3.*VFWG*VFGW)/60.

       DRBDTB=DRBDTB1+DRBDTB2
       DRBDTG=DRBDTG1+DRBDTG2
       DRGDTB=DRGDTB1+DRGDTB2
       DRGDTG=DRGDTG1+DRGDTG2

       HB=RHO*CP*CHB*UC*(TBP-TCP)*100.
       HG=RHO*CP*CHG*UC*(TGP-TCP)*100.

       DTCDTB=W*ALPHAB/(RW*ALPHAC+RW*ALPHAG+W*ALPHAB)
       DTCDTG=RW*ALPHAG/(RW*ALPHAC+RW*ALPHAG+W*ALPHAB)

       DHBDTB=RHO*CP*CHB*UC*(1.-DTCDTB)*100.
       DHBDTG=RHO*CP*CHB*UC*(0.-DTCDTG)*100.
       DHGDTG=RHO*CP*CHG*UC*(1.-DTCDTG)*100.
       DHGDTB=RHO*CP*CHG*UC*(0.-DTCDTB)*100.

       ELEB=RHO*EL*CHB*UC*BETB*(QS0B-QCP)*100.
       ELEG=RHO*EL*CHG*UC*BETG*(QS0G-QCP)*100.

       DQCDTB=W*ALPHAB*BETB*DQS0BDTB/(RW*ALPHAC+RW*ALPHAG*BETG+W*ALPHAB*BETB)
       DQCDTG=RW*ALPHAG*BETG*DQS0GDTG/(RW*ALPHAC+RW*ALPHAG*BETG+W*ALPHAB*BETB)

       DELEBDTB=RHO*EL*CHB*UC*BETB*(DQS0BDTB-DQCDTB)*100.
       DELEBDTG=RHO*EL*CHB*UC*BETB*(0.-DQCDTG)*100.
       DELEGDTG=RHO*EL*CHG*UC*BETG*(DQS0GDTG-DQCDTG)*100.
       DELEGDTB=RHO*EL*CHG*UC*BETG*(0.-DQCDTB)*100.

       G0B=AKSB*(TBP-TBL(1))/(DZB(1)/2.)
       G0G=AKSG*(TGP-TGL(1))/(DZG(1)/2.)

       DG0BDTB=2.*AKSB/DZB(1)
       DG0BDTG=0.
       DG0GDTG=2.*AKSG/DZG(1)
       DG0GDTB=0.

       F = SB + RB - HB - ELEB - G0B
       FX = DRBDTB - DHBDTB - DELEBDTB - DG0BDTB 
       FY = DRBDTG - DHBDTG - DELEBDTG - DG0BDTG

       GF = SG + RG - HG - ELEG - G0G
       GX = DRGDTB - DHGDTB - DELEGDTB - DG0GDTB
       GY = DRGDTG - DHGDTG - DELEGDTG - DG0GDTG 

       DTB =  (GF*FY-F*GY)/(FX*GY-GX*FY)
       DTG = -(GF+GX*DTB)/GY

       TB = TBP + DTB
       TG = TGP + DTG

       TBP = TB
       TGP = TG

       TC1=RW*ALPHAC+RW*ALPHAG+W*ALPHAB
       TC2=RW*ALPHAC*TA+RW*ALPHAG*TGP+W*ALPHAB*TBP
       TC=TC2/TC1

       QC1=RW*ALPHAC+RW*ALPHAG*BETG+W*ALPHAB*BETB
       QC2=RW*ALPHAC*QA+RW*ALPHAG*BETG*QS0G+W*ALPHAB*BETB*QS0B
       QC=QC2/QC1

       DTC=TCP - TC
       TCP=TC
       QCP=QC

       IF( ABS(F) < 0.000001 .AND. ABS(DTB) < 0.000001 &
        .AND. ABS(GF) < 0.000001 .AND. ABS(DTG) < 0.000001 &
        .AND. ABS(DTC) < 0.000001) EXIT

     END DO

     CALL multi_layer(num_wall_layers,BOUNDB,G0B,CAPB,AKSB,TBL,DZB,DELT,TBLEND)

     CALL multi_layer(num_road_layers,BOUNDG,G0G,CAPG,AKSG,TGL,DZG,DELT,TGLEND)

   ELSE

!-------------------------------------------------------------------------------
!  TB, TG  by Force-Restore Method 
!-------------------------------------------------------------------------------

       ES=6.11*EXP((2.5*10.**6./461.51)*(TBP-273.15)/(273.15*TBP) )
       QS0B=0.622*ES/(PS-0.378*ES)       

       ES=6.11*EXP((2.5*10.**6./461.51)*(TGP-273.15)/(273.15*TGP) )
       QS0G=0.622*ES/(PS-0.378*ES)        

       RG1=EPSG*( RX*VFGS             &
       +EPSB*VFGW*SIG*TBP**4./60.     &
       -SIG*TGP**4./60. )

       RB1=EPSB*( RX*VFWS &
       +EPSG*VFWG*SIG*TGP**4./60. &
       +EPSB*VFWW*SIG*TBP**4./60. &
       -SIG*TBP**4./60. )

       RG2=EPSG*( (1.-EPSB)*(1.-SVF)*VFWS*RX                   &
       +(1.-EPSB)*(1.-SVF)*VFWG*EPSG*SIG*TGP**4./60.           &
       +EPSB*(1.-EPSB)*(1.-SVF)*(1.-2.*VFWS)*SIG*TBP**4./60. )

       RB2=EPSB*( (1.-EPSG)*VFWG*VFGS*RX                          &
       +(1.-EPSG)*EPSB*VFGW*VFWG*SIG*(TBP**4.)/60.                &
       +(1.-EPSB)*VFWS*(1.-2.*VFWS)*RX                            &
       +(1.-EPSB)*VFWG*(1.-2.*VFWS)*EPSG*SIG*EPSG*TGP**4./60.     &
       +EPSB*(1.-EPSB)*(1.-2.*VFWS)*(1.-2.*VFWS)*SIG*TBP**4./60. )

       RG=RG1+RG2
       RB=RB1+RB2

       HB=RHO*CP*CHB*UC*(TBP-TCP)*100.
       ELEB=RHO*EL*CHB*UC*BETB*(QS0B-QCP)*100.
       G0B=SB+RB-HB-ELEB

       HG=RHO*CP*CHG*UC*(TGP-TCP)*100.
       ELEG=RHO*EL*CHG*UC*BETG*(QS0G-QCP)*100.
       G0G=SG+RG-HG-ELEG

       CALL force_restore(CAPB,AKSB,DELT,SB,RB,HB,ELEB,TBLEND,TBP,TB)
       CALL force_restore(CAPG,AKSG,DELT,SG,RG,HG,ELEG,TGLEND,TGP,TG)

       TBP=TB
       TGP=TG

       TC1=RW*ALPHAC+RW*ALPHAG+W*ALPHAB
       TC2=RW*ALPHAC*TA+RW*ALPHAG*TGP+W*ALPHAB*TBP
       TC=TC2/TC1

       QC1=RW*ALPHAC+RW*ALPHAG*BETG+W*ALPHAB*BETB
       QC2=RW*ALPHAC*QA+RW*ALPHAG*BETG*QS0G+W*ALPHAB*BETB*QS0B
       QC=QC2/QC1

       TCP=TC
       QCP=QC

   END IF


   FLXTHB=HB/RHO/CP/100.
   FLXHUMB=ELEB/RHO/EL/100.
   FLXTHG=HG/RHO/CP/100.
   FLXHUMG=ELEG/RHO/EL/100.

!-------------------------------------------------------------------------------
! Total Fluxes from Urban Canopy
!-------------------------------------------------------------------------------

   FLXUV  = ( R*CDR + RW*CDC )*UA*UA
! Miao, 2007/01/17, cal. ah
   if(ahoption==1) then
     FLXTH  = ( R*FLXTHR  + W*FLXTHB  + RW*FLXTHG ) + AH/RHOO/CPP
   else
     FLXTH  = ( R*FLXTHR  + W*FLXTHB  + RW*FLXTHG )
   endif
   FLXHUM = ( R*FLXHUMR + W*FLXHUMB + RW*FLXHUMG )
   FLXG =   ( R*G0R + W*G0B + RW*G0G )
   LNET =     R*RR + W*RB + RW*RG

!----------------------------------------------------------------------------
! Convert Unit: FLUXES and u* T* q*  --> WRF
!----------------------------------------------------------------------------

   SH    = FLXTH  * RHOO * CPP    ! Sensible heat flux          [W/m/m]
   LH    = FLXHUM * RHOO * ELL    ! Latent heat flux            [W/m/m]
   LH_KINEMATIC = FLXHUM * RHOO   ! Latent heat, Kinematic      [kg/m/m/s]
   LW    = LLG - (LNET*697.7*60.) ! Upward longwave radiation   [W/m/m]
   SW    = SSG - (SNET*697.7*60.) ! Upward shortwave radiation  [W/m/m]
   ALB   = 0.
   IF( ABS(SSG) > 0.0001) ALB = SW/SSG ! Effective albedo [-] 
   G = -FLXG*697.7*60.            ! [W/m/m]
   RN = (SNET+LNET)*697.7*60.     ! Net radiation [W/m/m]

   UST = SQRT(FLXUV)              ! u* [m/s]
   TST = -FLXTH/UST               ! T* [K]
   QST = -FLXHUM/UST              ! q* [-]

!------------------------------------------------------
!  diagnostic GRID AVERAGED  PSIM  PSIH  TS QS --> WRF
!------------------------------------------------------

   Z0 = Z0C 
   Z0H = Z0HC
   Z = ZA - ZDC
   ZNT = Z0   ! add by Dan Li

   XXX = 0.4*9.81*Z*TST/TA/UST/UST

   IF ( XXX >= 1. ) XXX = 1.
   IF ( XXX <= -5. ) XXX = -5.

   IF ( XXX > 0 ) THEN
     PSIM = -5. * XXX
     PSIH = -5. * XXX
   ELSE
     X = (1.-16.*XXX)**0.25
     PSIM = 2.*ALOG((1.+X)/2.) + ALOG((1.+X*X)/2.) - 2.*ATAN(X) + PI/2.
     PSIH = 2.*ALOG((1.+X*X)/2.)
   END IF

   GZ1OZ0 = ALOG(Z/Z0)
   CD = 0.4**2./(ALOG(Z/Z0)-PSIM)**2.
!
!m   CH = 0.4**2./(ALOG(Z/Z0)-PSIM)/(ALOG(Z/Z0H)-PSIH)
!m   CHS = 0.4*UST/(ALOG(Z/Z0H)-PSIH)
!m   TS = TA + FLXTH/CH/UA    ! surface potential temp (flux temp)
!m   QS = QA + FLXHUM/CH/UA   ! surface humidity
!
   TS = TA + FLXTH/CHS    ! surface potential temp (flux temp)
   QS = QA + FLXHUM/CHS   ! surface humidity

!-------------------------------------------------------
!  diagnostic  GRID AVERAGED  U10  V10  TH2  Q2 --> WRF
!-------------------------------------------------------

   XXX2 = (2./Z)*XXX
   IF ( XXX2 >= 1. ) XXX2 = 1.
   IF ( XXX2 <= -5. ) XXX2 = -5.

   IF ( XXX2 > 0 ) THEN
      PSIM2 = -5. * XXX2
      PSIH2 = -5. * XXX2
   ELSE
      X = (1.-16.*XXX2)**0.25
      PSIM2 = 2.*ALOG((1.+X)/2.) + ALOG((1.+X*X)/2.) - 2.*ATAN(X) + 2.*ATAN(1.)
      PSIH2 = 2.*ALOG((1.+X*X)/2.)
   END IF
!
!m   CHS2 = 0.4*UST/(ALOG(2./Z0H)-PSIH2)
!

   XXX10 = (10./Z)*XXX
   IF ( XXX10 >= 1. ) XXX10 = 1.
   IF ( XXX10 <= -5. ) XXX10 = -5.

   IF ( XXX10 > 0 ) THEN
      PSIM10 = -5. * XXX10
      PSIH10 = -5. * XXX10
   ELSE
      X = (1.-16.*XXX10)**0.25
      PSIM10 = 2.*ALOG((1.+X)/2.) + ALOG((1.+X*X)/2.) - 2.*ATAN(X) + 2.*ATAN(1.)
      PSIH10 = 2.*ALOG((1.+X*X)/2.)
   END IF

   PSIX = ALOG(Z/Z0) - PSIM
   PSIT = ALOG(Z/Z0H) - PSIH

   PSIX2 = ALOG(2./Z0) - PSIM2
   PSIT2 = ALOG(2./Z0H) - PSIH2

   PSIX10 = ALOG(10./Z0) - PSIM10
   PSIT10 = ALOG(10./Z0H) - PSIH10

   U10 = U1 * (PSIX10/PSIX)       ! u at 10 m [m/s]
   V10 = V1 * (PSIX10/PSIX)       ! v at 10 m [m/s]

!   TH2 = TS + (TA-TS)*(PSIT2/PSIT)   ! potential temp at 2 m [K]
!  TH2 = TS + (TA-TS)*(PSIT2/PSIT)   ! Fei: this seems to be temp (not potential) at 2 m [K]
!Fei: consistant with M-O theory
   TH2 = TS + (TA-TS) *(CHS/CHS2) 

   Q2 = QS + (QA-QS)*(PSIT2/PSIT)    ! humidity at 2 m       [-]

!   TS = (LW/SIG_SI/0.88)**0.25       ! Radiative temperature [K]

   END SUBROUTINE urban
!===============================================================================
!
!  mos
!
!===============================================================================
   SUBROUTINE mos(XXX,ALPHA,CD,B1,RIB,Z,Z0,UA,TA,TSF,RHO)

!  XXX:   z/L (requires iteration by Newton-Rapson method)
!  B1:    Stanton number
!  PSIM:  = PSIX of LSM
!  PSIH:  = PSIT of LSM

   IMPLICIT NONE

   REAL, PARAMETER     :: CP=0.24
   REAL, INTENT(IN)    :: B1, Z, Z0, UA, TA, TSF, RHO
   REAL, INTENT(OUT)   :: ALPHA, CD
   REAL, INTENT(INOUT) :: XXX, RIB
   REAL                :: XXX0, X, X0, FAIH, DPSIM, DPSIH
   REAL                :: F, DF, XXXP, US, TS, AL, XKB, DD, PSIM, PSIH
   INTEGER             :: NEWT
   INTEGER, PARAMETER  :: NEWT_END=10

   IF(RIB <= -15.) RIB=-15. 

   IF(RIB < 0.) THEN

      DO NEWT=1,NEWT_END

        IF(XXX >= 0.) XXX=-1.E-3

        XXX0=XXX*Z0/(Z+Z0)

        X=(1.-16.*XXX)**0.25
        X0=(1.-16.*XXX0)**0.25

        PSIM=ALOG((Z+Z0)/Z0) &
            -ALOG((X+1.)**2.*(X**2.+1.)) &
            +2.*ATAN(X) &
            +ALOG((X+1.)**2.*(X0**2.+1.)) &
            -2.*ATAN(X0)
        FAIH=1./SQRT(1.-16.*XXX)
        PSIH=ALOG((Z+Z0)/Z0)+0.4*B1 &
            -2.*ALOG(SQRT(1.-16.*XXX)+1.) &
            +2.*ALOG(SQRT(1.-16.*XXX0)+1.)

        DPSIM=(1.-16.*XXX)**(-0.25)/XXX &
             -(1.-16.*XXX0)**(-0.25)/XXX
        DPSIH=1./SQRT(1.-16.*XXX)/XXX &
             -1./SQRT(1.-16.*XXX0)/XXX

        F=RIB*PSIM**2./PSIH-XXX

        DF=RIB*(2.*DPSIM*PSIM*PSIH-DPSIH*PSIM**2.) &
          /PSIH**2.-1.

        XXXP=XXX
        XXX=XXXP-F/DF
        IF(XXX <= -10.) XXX=-10.

      END DO

   ELSE IF(RIB >= 0.142857) THEN

      XXX=0.714
      PSIM=ALOG((Z+Z0)/Z0)+7.*XXX
      PSIH=PSIM+0.4*B1

   ELSE

      AL=ALOG((Z+Z0)/Z0)
      XKB=0.4*B1
      DD=-4.*RIB*7.*XKB*AL+(AL+XKB)**2.
      IF(DD <= 0.) DD=0.
      XXX=(AL+XKB-2.*RIB*7.*AL-SQRT(DD))/(2.*(RIB*7.**2-7.))
      PSIM=ALOG((Z+Z0)/Z0)+7.*MIN(XXX,0.714)
      PSIH=PSIM+0.4*B1

   END IF

   US=0.4*UA/PSIM             ! u*
   IF(US <= 0.01) US=0.01
   TS=0.4*(TA-TSF)/PSIH       ! T*

   CD=US*US/UA**2.            ! CD
   ALPHA=RHO*CP*0.4*US/PSIH   ! RHO*CP*CH*U

   RETURN 
   END SUBROUTINE mos
!===============================================================================
!
!  louis79
!
!===============================================================================
   SUBROUTINE louis79(ALPHA,CD,RIB,Z,Z0,UA,RHO)

   IMPLICIT NONE

   REAL, PARAMETER     :: CP=0.24
   REAL, INTENT(IN)    :: Z, Z0, UA, RHO
   REAL, INTENT(OUT)   :: ALPHA, CD
   REAL, INTENT(INOUT) :: RIB
   REAL                :: A2, XX, CH, CMB, CHB

   A2=(0.4/ALOG(Z/Z0))**2.

   IF(RIB <= -15.) RIB=-15.

   IF(RIB >= 0.0) THEN
      IF(RIB >= 0.142857) THEN 
         XX=0.714
      ELSE 
         XX=RIB*LOG(Z/Z0)/(1.-7.*RIB)
      END IF 
      CH=0.16/0.74/(LOG(Z/Z0)+7.*MIN(XX,0.714))**2.
      CD=0.16/(LOG(Z/Z0)+7.*MIN(XX,0.714))**2.
   ELSE 
      CMB=7.4*A2*9.4*SQRT(Z/Z0)
      CHB=5.3*A2*9.4*SQRT(Z/Z0)
      CH=A2/0.74*(1.-9.4*RIB/(1.+CHB*SQRT(-RIB)))
      CD=A2*(1.-9.4*RIB/(1.+CHB*SQRT(-RIB)))
   END IF

   ALPHA=RHO*CP*CH*UA

   RETURN 
   END SUBROUTINE louis79
!===============================================================================
!
!  louis82
!
!===============================================================================
   SUBROUTINE louis82(ALPHA,CD,RIB,Z,Z0,UA,RHO)

   IMPLICIT NONE

   REAL, PARAMETER     :: CP=0.24
   REAL, INTENT(IN)    :: Z, Z0, UA, RHO
   REAL, INTENT(OUT)   :: ALPHA, CD
   REAL, INTENT(INOUT) :: RIB 
   REAL                :: A2, FM, FH, CH, CHH 

   A2=(0.4/ALOG(Z/Z0))**2.

   IF(RIB <= -15.) RIB=-15.

   IF(RIB >= 0.0) THEN 
      FM=1./((1.+(2.*5.*RIB)/SQRT(1.+5.*RIB)))
      FH=1./(1.+(3.*5.*RIB)*SQRT(1.+5.*RIB))
      CH=A2*FH
      CD=A2*FM
   ELSE 
      CHH=5.*3.*5.*A2*SQRT(Z/Z0)
      FM=1.-(2.*5.*RIB)/(1.+3.*5.*5.*A2*SQRT(Z/Z0+1.)*(-RIB))
      FH=1.-(3.*5.*RIB)/(1.+CHH*SQRT(-RIB))
      CH=A2*FH
      CD=A2*FM
   END IF

   ALPHA=RHO*CP*CH*UA

   RETURN
   END SUBROUTINE louis82
!===============================================================================
!
!  multi_layer
!
!===============================================================================
   SUBROUTINE multi_layer(KM,BOUND,G0,CAP,AKS,TSL,DZ,DELT,TSLEND)

   IMPLICIT NONE

   REAL, INTENT(IN)                   :: G0

   REAL, INTENT(IN)                   :: CAP

   REAL, INTENT(IN)                   :: AKS

   REAL, INTENT(IN)                   :: DELT      ! Time step [ s ]

   REAL, INTENT(IN)                   :: TSLEND

   INTEGER, INTENT(IN)                :: KM

   INTEGER, INTENT(IN)                :: BOUND

   REAL, DIMENSION(KM), INTENT(IN)    :: DZ

   REAL, DIMENSION(KM), INTENT(INOUT) :: TSL

   REAL, DIMENSION(KM)                :: A, B, C, D, X, P, Q

   REAL                               :: DZEND

   INTEGER                            :: K

   DZEND=DZ(KM)

   A(1) = 0.0

   B(1) = CAP*DZ(1)/DELT &
          +2.*AKS/(DZ(1)+DZ(2))
   C(1) = -2.*AKS/(DZ(1)+DZ(2))
   D(1) = CAP*DZ(1)/DELT*TSL(1) + G0

   DO K=2,KM-1
      A(K) = -2.*AKS/(DZ(K-1)+DZ(K))
      B(K) = CAP*DZ(K)/DELT + 2.*AKS/(DZ(K-1)+DZ(K)) + 2.*AKS/(DZ(K)+DZ(K+1)) 
      C(K) = -2.*AKS/(DZ(K)+DZ(K+1))
      D(K) = CAP*DZ(K)/DELT*TSL(K)
   END DO 

   IF(BOUND == 1) THEN                  ! Flux=0
      A(KM) = -2.*AKS/(DZ(KM-1)+DZ(KM))
      B(KM) = CAP*DZ(KM)/DELT + 2.*AKS/(DZ(KM-1)+DZ(KM))  
      C(KM) = 0.0
      D(KM) = CAP*DZ(KM)/DELT*TSL(KM)
   ELSE                                 ! T=constant
      A(KM) = -2.*AKS/(DZ(KM-1)+DZ(KM))
      B(KM) = CAP*DZ(KM)/DELT + 2.*AKS/(DZ(KM-1)+DZ(KM)) + 2.*AKS/(DZ(KM)+DZEND) 
      C(KM) = 0.0
      D(KM) = CAP*DZ(KM)/DELT*TSL(KM) + 2.*AKS*TSLEND/(DZ(KM)+DZEND)
   END IF 

   P(1) = -C(1)/B(1)
   Q(1) =  D(1)/B(1)

   DO K=2,KM
      P(K) = -C(K)/(A(K)*P(K-1)+B(K))
      Q(K) = (-A(K)*Q(K-1)+D(K))/(A(K)*P(K-1)+B(K))
   END DO 

   X(KM) = Q(KM)

   DO K=KM-1,1,-1
      X(K) = P(K)*X(K+1)+Q(K)
   END DO 

   DO K=1,KM
      TSL(K) = X(K)
   END DO 

   RETURN 
   END SUBROUTINE multi_layer
!===============================================================================
!
!  subroutine read_param
!
!===============================================================================
   SUBROUTINE read_param(UTYPE,                                        & ! in
                         ZR,SIGMA_ZED,Z0C,Z0HC,ZDC,SVF,R,RW,HGT,AH,    & ! out
                         CAPR,CAPB,CAPG,AKSR,AKSB,AKSG,ALBR,ALBB,ALBG, & ! out
                         EPSR,EPSB,EPSG,Z0R,Z0B,Z0G,Z0HB,Z0HG,         & ! out
                         BETR,BETB,BETG,TRLEND,TBLEND,TGLEND,          & ! out
!for BEP
                         NUMDIR, STREET_DIRECTION, STREET_WIDTH,       & ! out
                         BUILDING_WIDTH, NUMHGT, HEIGHT_BIN,           & ! out
                         HPERCENT_BIN,                                 & ! out
!end BEP
                         BOUNDR,BOUNDB,BOUNDG,CH_SCHEME,TS_SCHEME,     & ! out
                         AKANDA_URBAN)                                   ! out

   INTEGER, INTENT(IN)  :: UTYPE 

   REAL, INTENT(OUT)    :: ZR,Z0C,Z0HC,ZDC,SVF,R,RW,HGT,AH,              &
                           CAPR,CAPB,CAPG,AKSR,AKSB,AKSG,ALBR,ALBB,ALBG, &
                           SIGMA_ZED,                                    &
                           EPSR,EPSB,EPSG,Z0R,Z0B,Z0G,Z0HB,Z0HG,         &
                           BETR,BETB,BETG,TRLEND,TBLEND,TGLEND
   REAL, INTENT(OUT)    :: AKANDA_URBAN
!for BEP
   INTEGER,                     INTENT(OUT) :: NUMDIR
   REAL,    DIMENSION(MAXDIRS), INTENT(OUT) :: STREET_DIRECTION
   REAL,    DIMENSION(MAXDIRS), INTENT(OUT) :: STREET_WIDTH
   REAL,    DIMENSION(MAXDIRS), INTENT(OUT) :: BUILDING_WIDTH
   INTEGER,                     INTENT(OUT) :: NUMHGT
   REAL,    DIMENSION(MAXHGTS), INTENT(OUT) :: HEIGHT_BIN
   REAL,    DIMENSION(MAXHGTS), INTENT(OUT) :: HPERCENT_BIN
   
!end BEP

   INTEGER, INTENT(OUT) :: BOUNDR,BOUNDB,BOUNDG,CH_SCHEME,TS_SCHEME

   ZR =     ZR_TBL(UTYPE)
   SIGMA_ZED = SIGMA_ZED_TBL(UTYPE)
   Z0C=     Z0C_TBL(UTYPE)
   Z0HC=    Z0HC_TBL(UTYPE)
   ZDC=     ZDC_TBL(UTYPE)
   SVF=     SVF_TBL(UTYPE)
   R=       R_TBL(UTYPE)
   RW=      RW_TBL(UTYPE)
   HGT=     HGT_TBL(UTYPE)
   AH=      AH_TBL(UTYPE)
   BETR=    BETR_TBL(UTYPE)
   BETB=    BETB_TBL(UTYPE)
   BETG=    BETG_TBL(UTYPE)

!m   FRC_URB= FRC_URB_TBL(UTYPE)

   CAPR=    CAPR_TBL(UTYPE)
   CAPB=    CAPB_TBL(UTYPE)
   CAPG=    CAPG_TBL(UTYPE)
   AKSR=    AKSR_TBL(UTYPE)
   AKSB=    AKSB_TBL(UTYPE)
   AKSG=    AKSG_TBL(UTYPE)
   ALBR=    ALBR_TBL(UTYPE)
   ALBB=    ALBB_TBL(UTYPE)
   ALBG=    ALBG_TBL(UTYPE)
   EPSR=    EPSR_TBL(UTYPE)
   EPSB=    EPSB_TBL(UTYPE)
   EPSG=    EPSG_TBL(UTYPE)
   Z0R=     Z0R_TBL(UTYPE)
   Z0B=     Z0B_TBL(UTYPE)
   Z0G=     Z0G_TBL(UTYPE)
   Z0HB=    Z0HB_TBL(UTYPE)
   Z0HG=    Z0HG_TBL(UTYPE)
   TRLEND=  TRLEND_TBL(UTYPE)
   TBLEND=  TBLEND_TBL(UTYPE)
   TGLEND=  TGLEND_TBL(UTYPE)
   BOUNDR=  BOUNDR_DATA
   BOUNDB=  BOUNDB_DATA
   BOUNDG=  BOUNDG_DATA
   CH_SCHEME = CH_SCHEME_DATA
   TS_SCHEME = TS_SCHEME_DATA
   AKANDA_URBAN = AKANDA_URBAN_TBL(UTYPE)

!for BEP

   STREET_DIRECTION = -1.E36
   STREET_WIDTH     = -1.E36
   BUILDING_WIDTH   = -1.E36
   HEIGHT_BIN       = -1.E36
   HPERCENT_BIN     = -1.E36

   NUMDIR                     = NUMDIR_TBL ( UTYPE )
   STREET_DIRECTION(1:NUMDIR) = STREET_DIRECTION_TBL( 1:NUMDIR, UTYPE )
   STREET_WIDTH    (1:NUMDIR) = STREET_WIDTH_TBL    ( 1:NUMDIR, UTYPE )
   BUILDING_WIDTH  (1:NUMDIR) = BUILDING_WIDTH_TBL  ( 1:NUMDIR, UTYPE )
   NUMHGT                     = NUMHGT_TBL ( UTYPE )
   HEIGHT_BIN      (1:NUMHGT) = HEIGHT_BIN_TBL   ( 1:NUMHGT , UTYPE )
   HPERCENT_BIN    (1:NUMHGT) = HPERCENT_BIN_TBL ( 1:NUMHGT , UTYPE )

!end BEP
   END SUBROUTINE read_param
!===============================================================================
!
! subroutine urban_param_init: Read parameters from URBPARM.TBL
!
!===============================================================================
   SUBROUTINE urban_param_init(DZR,DZB,DZG,num_soil_layers, &
                               sf_urban_physics)
!                              num_roof_layers,num_wall_layers,num_road_layers)

   IMPLICIT NONE

   INTEGER, INTENT(IN) :: num_soil_layers

!   REAL, DIMENSION(1:num_roof_layers), INTENT(INOUT) :: DZR
!   REAL, DIMENSION(1:num_wall_layers), INTENT(INOUT) :: DZB
!   REAL, DIMENSION(1:num_road_layers), INTENT(INOUT) :: DZG
   REAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZR
   REAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZB
   REAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZG
   INTEGER,                            INTENT(IN)    :: SF_URBAN_PHYSICS

   INTEGER :: LC, K
   INTEGER :: IOSTATUS, ALLOCATE_STATUS
   INTEGER :: num_roof_layers
   INTEGER :: num_wall_layers
   INTEGER :: num_road_layers
   INTEGER :: dummy 
   REAL    :: DHGT, HGT, VFWS, VFGS

   REAL, allocatable, dimension(:) :: ROOF_WIDTH
   REAL, allocatable, dimension(:) :: ROAD_WIDTH

   character(len=512) :: string
   character(len=128) :: name
   integer :: indx

   real, parameter :: VonK = 0.4
   real :: lambda_p
   real :: lambda_f
   real :: Cd
   real :: alpha_macd
   real :: beta_macd
   real :: lambda_fr


!for BEP
   real :: dummy_hgt
   real :: dummy_pct
   real :: pctsum
!end BEP
   num_roof_layers = num_soil_layers
   num_wall_layers = num_soil_layers
   num_road_layers = num_soil_layers


   ICATE=0

   OPEN (UNIT=11,                &
         FILE='URBPARM.TBL', &
         ACCESS='SEQUENTIAL',    &
         STATUS='OLD',           &
         ACTION='READ',          &
         POSITION='REWIND',      &
         IOSTAT=IOSTATUS)

   IF (IOSTATUS > 0) THEN
   CALL wrf_error_fatal('ERROR OPEN URBPARM.TBL')
   ENDIF

   READLOOP : do 
      read(11,'(A512)', iostat=iostatus) string
      if (iostatus /= 0) exit READLOOP
      if (string(1:1) == "#") cycle READLOOP
      if (trim(string) == "") cycle READLOOP
      indx = index(string,":")
      if (indx <= 0) cycle READLOOP
      name = trim(adjustl(string(1:indx-1)))
      
      ! Here are the variables we expect to be defined in the URBPARM.TBL:
      if (name == "Number of urban categories") then
         read(string(indx+1:),*) icate
         IF (.not. ALLOCATED(ZR_TBL)) then
            ALLOCATE( ZR_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating ZR_TBL in urban_param_init')
            ALLOCATE( SIGMA_ZED_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0)CALL wrf_error_fatal('Error allocating SIGMA_ZED_TBL in urban_param_init')
            ALLOCATE( Z0C_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating Z0C_TBL in urban_param_init')
            ALLOCATE( Z0HC_TBL(ICATE), stat=allocate_status ) 
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating Z0HC_TBL in urban_param_init')
            ALLOCATE( ZDC_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating ZDC_TBL in urban_param_init')
            ALLOCATE( SVF_TBL(ICATE), stat=allocate_status ) 
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating SVF_TBL in urban_param_init')
            ALLOCATE( R_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating R_TBL in urban_param_init')
            ALLOCATE( RW_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating RW_TBL in urban_param_init')
            ALLOCATE( HGT_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating HGT_TBL in urban_param_init')
            ALLOCATE( AH_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating AH_TBL in urban_param_init')
            ALLOCATE( BETR_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating BETR_TBL in urban_param_init')
            ALLOCATE( BETB_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating BETB_TBL in urban_param_init')
            ALLOCATE( BETG_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating BETG_TBL in urban_param_init')
            ALLOCATE( CAPR_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating CAPR_TBL in urban_param_init')
            ALLOCATE( CAPB_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating CAPB_TBL in urban_param_init')
            ALLOCATE( CAPG_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating CAPG_TBL in urban_param_init')
            ALLOCATE( AKSR_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating AKSR_TBL in urban_param_init')
            ALLOCATE( AKSB_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating AKSB_TBL in urban_param_init')
            ALLOCATE( AKSG_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating AKSG_TBL in urban_param_init')
            ALLOCATE( ALBR_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating ALBR_TBL in urban_param_init')
            ALLOCATE( ALBB_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating ALBB_TBL in urban_param_init')
            ALLOCATE( ALBG_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating ALBG_TBL in urban_param_init')
            ALLOCATE( EPSR_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating EPSR_TBL in urban_param_init')
            ALLOCATE( EPSB_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating EPSB_TBL in urban_param_init')
            ALLOCATE( EPSG_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating EPSG_TBL in urban_param_init')
            ALLOCATE( Z0R_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating Z0R_TBL in urban_param_init')
            ALLOCATE( Z0B_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating Z0B_TBL in urban_param_init')
            ALLOCATE( Z0G_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating Z0G_TBL in urban_param_init')
            ALLOCATE( AKANDA_URBAN_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating AKANDA_URBAN_TBL in urban_param_init')
            ALLOCATE( Z0HB_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating Z0HB_TBL in urban_param_init')
            ALLOCATE( Z0HG_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating Z0HG_TBL in urban_param_init')
            ALLOCATE( TRLEND_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating TRLEND_TBL in urban_param_init')
            ALLOCATE( TBLEND_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating TBLEND_TBL in urban_param_init')
            ALLOCATE( TGLEND_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating TGLEND_TBL in urban_param_init')
            ALLOCATE( FRC_URB_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating FRC_URB_TBL in urban_param_init')
            ! ALLOCATE( ROOF_WIDTH(ICATE), stat=allocate_status )
            ! if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating ROOF_WIDTH in urban_param_init')
            ! ALLOCATE( ROAD_WIDTH(ICATE), stat=allocate_status )
            ! if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating ROAD_WIDTH in urban_param_init')
            !for BEP
            ALLOCATE( NUMDIR_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating NUMDIR_TBL in urban_param_init')
            ALLOCATE( STREET_DIRECTION_TBL(MAXDIRS , ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating STREET_DIRECTION_TBL in urban_param_init')
            ALLOCATE( STREET_WIDTH_TBL(MAXDIRS , ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating STREET_WIDTH_TBL in urban_param_init')
            ALLOCATE( BUILDING_WIDTH_TBL(MAXDIRS , ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating BUILDING_WIDTH_TBL in urban_param_init')
            ALLOCATE( NUMHGT_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating NUMHGT_TBL in urban_param_init')
            ALLOCATE( HEIGHT_BIN_TBL(MAXHGTS , ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating HEIGHT_BIN_TBL in urban_param_init')
            ALLOCATE( HPERCENT_BIN_TBL(MAXHGTS , ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating HPERCENT_BIN_TBL in urban_param_init')
            ALLOCATE( COP_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating COP_TBL in urban_param_init')
            ALLOCATE( PWIN_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating PWIN_TBL in urban_param_init')
            ALLOCATE( BETA_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating BETA_TBL in urban_param_init')
            ALLOCATE( SW_COND_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating SW_COND_TBL in urban_param_init')
            ALLOCATE( TIME_ON_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating TIME_ON_TBL in urban_param_init')
            ALLOCATE( TIME_OFF_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating TIME_OFF_TBL in urban_param_init')
            ALLOCATE( TARGTEMP_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating TARGTEMP_TBL in urban_param_init')
            ALLOCATE( GAPTEMP_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating GAPTEMP_TBL in urban_param_init')
            ALLOCATE( TARGHUM_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating TARGHUM_TBL in urban_param_init')
            ALLOCATE( GAPHUM_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating GAPHUM_TBL in urban_param_init')
            ALLOCATE( PERFLO_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating PERFLO_TBL in urban_param_init')
            ALLOCATE( HSESF_TBL(ICATE), stat=allocate_status )
            if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating HSESF_TBL in urban_param_init')
         endif
         numdir_tbl = 0
         street_direction_tbl = -1.E36
         street_width_tbl = 0
         building_width_tbl = 0
         numhgt_tbl = 0
         height_bin_tbl = -1.E36
         hpercent_bin_tbl = -1.E36
!end BEP

      else if (name == "ZR") then
         read(string(indx+1:),*) zr_tbl(1:icate)
      else if (name == "SIGMA_ZED") then
         read(string(indx+1:),*) sigma_zed_tbl(1:icate)
      else if (name == "ROOF_WIDTH") then
         ALLOCATE( ROOF_WIDTH(ICATE), stat=allocate_status )
         if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating ROOF_WIDTH in urban_param_init')

         read(string(indx+1:),*) roof_width(1:icate)
      else if (name == "ROAD_WIDTH") then
         ALLOCATE( ROAD_WIDTH(ICATE), stat=allocate_status )
         if(allocate_status /= 0) CALL wrf_error_fatal('Error allocating ROAD_WIDTH in urban_param_init')
         read(string(indx+1:),*) road_width(1:icate)
      else if (name == "AH") then
         read(string(indx+1:),*) ah_tbl(1:icate)
      else if (name == "FRC_URB") then
         read(string(indx+1:),*) frc_urb_tbl(1:icate)
      else if (name == "CAPR") then
         read(string(indx+1:),*) capr_tbl(1:icate)
         ! Convert CAPR_TBL from J m{-3} K{-1} to cal cm{-3} deg{-1}
         capr_tbl = capr_tbl * ( 1.0 / 4.1868 ) * 1.E-6
      else if (name == "CAPB") then
         read(string(indx+1:),*) capb_tbl(1:icate)
         ! Convert CABR_TBL from J m{-3} K{-1} to cal cm{-3} deg{-1}
         capb_tbl = capb_tbl * ( 1.0 / 4.1868 ) * 1.E-6
      else if (name == "CAPG") then
         read(string(indx+1:),*) capg_tbl(1:icate)
         ! Convert CABG_TBL from J m{-3} K{-1} to cal cm{-3} deg{-1}
         capg_tbl = capg_tbl * ( 1.0 / 4.1868 ) * 1.E-6
      else if (name == "AKSR") then
         read(string(indx+1:),*) aksr_tbl(1:icate)
         ! Convert AKSR_TBL from J m{-1} s{-1} K{-1} to cal cm{-1} s{-1} deg{-1}
         AKSR_TBL = AKSR_TBL * ( 1.0 / 4.1868 ) * 1.E-2
      else if (name == "AKSB") then
         read(string(indx+1:),*) aksb_tbl(1:icate)
         ! Convert AKSB_TBL from J m{-1} s{-1} K{-1} to cal cm{-1} s{-1} deg{-1}
         AKSB_TBL = AKSB_TBL * ( 1.0 / 4.1868 ) * 1.E-2
      else if (name == "AKSG") then
         read(string(indx+1:),*) aksg_tbl(1:icate)
         ! Convert AKSG_TBL from J m{-1} s{-1} K{-1} to cal cm{-1} s{-1} deg{-1}
         AKSG_TBL = AKSG_TBL * ( 1.0 / 4.1868 ) * 1.E-2
      else if (name == "ALBR") then
         read(string(indx+1:),*) albr_tbl(1:icate)
      else if (name == "ALBB") then
         read(string(indx+1:),*) albb_tbl(1:icate)
      else if (name == "ALBG") then
         read(string(indx+1:),*) albg_tbl(1:icate)
      else if (name == "EPSR") then
         read(string(indx+1:),*) epsr_tbl(1:icate)
      else if (name == "EPSB") then
         read(string(indx+1:),*) epsb_tbl(1:icate)
      else if (name == "EPSG") then
         read(string(indx+1:),*) epsg_tbl(1:icate)
      else if (name == "AKANDA_URBAN") then
         read(string(indx+1:),*) akanda_urban_tbl(1:icate)
      else if (name == "Z0B") then
         read(string(indx+1:),*) z0b_tbl(1:icate)
      else if (name == "Z0G") then
         read(string(indx+1:),*) z0g_tbl(1:icate)
      else if (name == "DDZR") then
         read(string(indx+1:),*) dzr(1:num_roof_layers)
         ! Convert thicknesses from m to cm
         dzr = dzr * 100.0
      else if (name == "DDZB") then
         read(string(indx+1:),*) dzb(1:num_wall_layers)
         ! Convert thicknesses from m to cm
         dzb = dzb * 100.0
      else if (name == "DDZG") then
         read(string(indx+1:),*) dzg(1:num_road_layers)
         ! Convert thicknesses from m to cm
         dzg = dzg * 100.0
      else if (name == "BOUNDR") then
         read(string(indx+1:),*) boundr_data
      else if (name == "BOUNDB") then
         read(string(indx+1:),*) boundb_data
      else if (name == "BOUNDG") then
         read(string(indx+1:),*) boundg_data
      else if (name == "TRLEND") then
         read(string(indx+1:),*) trlend_tbl(1:icate)
      else if (name == "TBLEND") then
         read(string(indx+1:),*) tblend_tbl(1:icate)
      else if (name == "TGLEND") then
         read(string(indx+1:),*) tglend_tbl(1:icate)
      else if (name == "CH_SCHEME") then
         read(string(indx+1:),*) ch_scheme_data
      else if (name == "TS_SCHEME") then
         read(string(indx+1:),*) ts_scheme_data
      else if (name == "AHOPTION") then
         read(string(indx+1:),*) ahoption
      else if (name == "AHDIUPRF") then
         read(string(indx+1:),*) ahdiuprf(1:24)
!for BEP
      else if (name == "STREET PARAMETERS") then

         STREETLOOP : do
            read(11,'(A512)', iostat=iostatus) string
            if (string(1:1) == "#") cycle STREETLOOP
            if (trim(string) == "") cycle STREETLOOP
            if (string == "END STREET PARAMETERS") exit STREETLOOP
            read(string, *) k ! , dirst, ws, bs
            numdir_tbl(k) = numdir_tbl(k) + 1
            read(string, *) k, street_direction_tbl(numdir_tbl(k),k), &
                               street_width_tbl(numdir_tbl(k),k), &
                               building_width_tbl(numdir_tbl(k),k)
         enddo STREETLOOP

      else if (name == "BUILDING HEIGHTS") then

         read(string(indx+1:),*) k
         HEIGHTLOOP : do
            read(11,'(A512)', iostat=iostatus) string
            if (string(1:1) == "#") cycle HEIGHTLOOP
            if (trim(string) == "") cycle HEIGHTLOOP
            if (string == "END BUILDING HEIGHTS") exit HEIGHTLOOP
            read(string,*) dummy_hgt, dummy_pct
            numhgt_tbl(k) = numhgt_tbl(k) + 1
            height_bin_tbl(numhgt_tbl(k), k) = dummy_hgt
            hpercent_bin_tbl(numhgt_tbl(k),k) = dummy_pct
            
         enddo HEIGHTLOOP
         pctsum = sum ( hpercent_bin_tbl(:,k) , mask=(hpercent_bin_tbl(:,k)>-1.E25 ) )
         if ( pctsum /= 100.) then
            write (*,'(//,"Building height percentages for category ", I2, " must sum to 100.0")') k
            write (*,'("Currently, they sum to ", F6.2,/)') pctsum
            CALL wrf_error_fatal('pctsum is not equal to 100.') 
         endif
      else if ( name == "Z0R") then
         read(string(indx+1:),*) Z0R_tbl(1:icate)
      else if ( name == "COP") then
         read(string(indx+1:),*) cop_tbl(1:icate)
      else if ( name == "PWIN") then
         read(string(indx+1:),*) pwin_tbl(1:icate)
      else if ( name == "BETA") then
         read(string(indx+1:),*) beta_tbl(1:icate)
      else if ( name == "SW_COND") then
         read(string(indx+1:),*) sw_cond_tbl(1:icate)
      else if ( name == "TIME_ON") then
         read(string(indx+1:),*) time_on_tbl(1:icate)
      else if ( name == "TIME_OFF") then
         read(string(indx+1:),*) time_off_tbl(1:icate)
      else if ( name == "TARGTEMP") then
         read(string(indx+1:),*) targtemp_tbl(1:icate)
      else if ( name == "GAPTEMP") then
         read(string(indx+1:),*) gaptemp_tbl(1:icate)
      else if ( name == "TARGHUM") then
         read(string(indx+1:),*) targhum_tbl(1:icate)
      else if ( name == "GAPHUM") then
         read(string(indx+1:),*) gaphum_tbl(1:icate)
      else if ( name == "PERFLO") then
         read(string(indx+1:),*) perflo_tbl(1:icate)
      else if (name == "HSEQUIP") then
         read(string(indx+1:),*) hsequip_tbl(1:24)
      else if (name == "HSEQUIP_SCALE_FACTOR") then
         read(string(indx+1:),*) hsesf_tbl(1:icate)
!end BEP         
      else
         CALL wrf_error_fatal('URBPARM.TBL: Unrecognized NAME = "'//trim(name)//'" in Subr URBAN_PARAM_INIT')
      endif
   enddo READLOOP

   CLOSE(11)

   ! Assign a few table values that do not need to come from URBPARM.TBL

   Z0HB_TBL = 0.1 * Z0B_TBL
   Z0HG_TBL = 0.1 * Z0G_TBL

   DO LC = 1, ICATE

      ! HGT:  Normalized height
      HGT_TBL(LC) = ZR_TBL(LC) / ( ROAD_WIDTH(LC) + ROOF_WIDTH(LC) )

      ! R:  Normalized Roof Width (a.k.a. "building coverage ratio")
      R_TBL(LC)  = ROOF_WIDTH(LC) / ( ROAD_WIDTH(LC) + ROOF_WIDTH(LC) )

      RW_TBL(LC) = 1.0 - R_TBL(LC)
      BETR_TBL(LC) = 0.0
      BETB_TBL(LC) = 0.0
      BETG_TBL(LC) = 0.0

      ! The following urban canyon geometry parameters are following Macdonald's (1998) formulations
      
      ! Lambda_P :: Plan areal fraction, which corresponds to R for a 2-d canyon.
      ! Lambda_F :: Frontal area index, which corresponds to HGT for a 2-d canyon
      ! Cd       :: Drag coefficient ( 1.2 from Grimmond and Oke, 1998 )
      ! Alpha_macd :: Emperical coefficient ( 4.43 from Macdonald et al., 1998 )
      ! Beta_macd  :: Correction factor for the drag coefficient ( 1.0 from Macdonald et al., 1998 )

      Lambda_P = R_TBL(LC)
      Lambda_F = HGT_TBL(LC)
      Cd         = 1.2
      alpha_macd = 4.43 
      beta_macd  = 1.0


      ZDC_TBL(LC) = ZR_TBL(LC) * ( 1.0 + ( alpha_macd ** ( -Lambda_P ) )  * ( Lambda_P - 1.0 ) )

      Z0C_TBL(LC) = ZR_TBL(LC) * ( 1.0 - ZDC_TBL(LC)/ZR_TBL(LC) ) * &
           exp (-(0.5 * beta_macd * Cd / (VonK**2) * ( 1.0-ZDC_TBL(LC)/ZR_TBL(LC) ) * Lambda_F )**(-0.5))

      IF (SF_URBAN_PHYSICS == 1) THEN
         ! Include roof height variability in Macdonald
         ! to parameterize Z0R as a function of ZR_SD (Standard Deviation)
         Lambda_FR  = SIGMA_ZED_TBL(LC) / ( ROAD_WIDTH(LC) + ROOF_WIDTH(LC) )
         Z0R_TBL(LC) = ZR_TBL(LC) * ( 1.0 - ZDC_TBL(LC)/ZR_TBL(LC) ) &
              * exp ( -(0.5 * beta_macd * Cd / (VonK**2) &
              * ( 1.0-ZDC_TBL(LC)/ZR_TBL(LC) ) * Lambda_FR )**(-0.5))
      ENDIF

      !
      ! Z0HC still one-tenth of Z0C, as before ?
      !

      Z0HC_TBL(LC) = 0.1 * Z0C_TBL(LC)

      !
      ! Calculate Sky View Factor:
      !
      DHGT=HGT_TBL(LC)/100.
      HGT=0.
      VFWS=0.
      HGT=HGT_TBL(LC)-DHGT/2.
      do k=1,99
         HGT=HGT-DHGT
         VFWS=VFWS+0.25*(1.-HGT/SQRT(HGT**2.+RW_TBL(LC)**2.))
      end do

     VFWS=VFWS/99.
     VFWS=VFWS*2.

     VFGS=1.-2.*VFWS*HGT_TBL(LC)/RW_TBL(LC)
     SVF_TBL(LC)=VFGS
   END DO

   deallocate(roof_width)
   deallocate(road_width)

   END SUBROUTINE urban_param_init
!===========================================================================
!
! subroutine urban_var_init: initialization of urban state variables
!
!===========================================================================
   SUBROUTINE urban_var_init(ISURBAN, TSURFACE0_URB,TLAYER0_URB,TDEEP0_URB,IVGTYP,  & ! in
                             ims,ime,jms,jme,kms,kme,num_soil_layers,         & ! in
!                             num_roof_layers,num_wall_layers,num_road_layers, & ! in
                             restart,sf_urban_physics,                     & !in
                             XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,  & ! inout
                             TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D, & ! inout
                             TRL_URB3D,TBL_URB3D,TGL_URB3D,                & ! inout
                             SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D,           & ! inout
                             TS_URB2D,                                     & ! inout
                             num_urban_layers,                             & ! in
                             num_urban_hi,                                 & ! in
                             TRB_URB4D,TW1_URB4D,TW2_URB4D,TGB_URB4D,      & ! inout
                             TLEV_URB3D,QLEV_URB3D,                        & ! inout
                             TW1LEV_URB3D,TW2LEV_URB3D,                    & ! inout
                             TGLEV_URB3D,TFLEV_URB3D,                      & ! inout
                             SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,          & ! inout
                             SFVENT_URB3D,LFVENT_URB3D,                    & ! inout
                             SFWIN1_URB3D,SFWIN2_URB3D,                    & ! inout
                             SFW1_URB3D,SFW2_URB3D,SFR_URB3D,SFG_URB3D,    & ! inout
                             LP_URB2D,HI_URB2D,LB_URB2D,                   & !inout
                             HGT_URB2D,MH_URB2D,STDH_URB2D,                & !inout
                             LF_URB2D,                                     & !inout
                             A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,              & ! inout multi-layer urban
                             A_E_BEP,B_U_BEP,B_V_BEP,                      & ! inout multi-layer urban
                             B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,              & ! inout multi-layer urban
                             DL_U_BEP,SF_BEP,VL_BEP,                       & ! inout multi-layer urban
                             FRC_URB2D, UTYPE_URB2D)                         ! inout
   IMPLICIT NONE

   INTEGER, INTENT(IN) :: ISURBAN, sf_urban_physics
   INTEGER, INTENT(IN) :: ims,ime,jms,jme,kms,kme,num_soil_layers
   INTEGER, INTENT(IN) :: num_urban_layers                            !multi-layer urban
   INTEGER, INTENT(IN) :: num_urban_hi                                !multi-layer urban
!   INTEGER, INTENT(IN) :: num_roof_layers, num_wall_layers, num_road_layers

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN)                    :: TSURFACE0_URB
   REAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), INTENT(IN) :: TLAYER0_URB
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN)                    :: TDEEP0_URB
   INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(IN)                 :: IVGTYP
   LOGICAL , INTENT(IN) :: restart
 
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TR_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TB_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TG_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TC_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QC_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXR_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXB_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXG_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXC_URB2D

!   REAL, DIMENSION(ims:ime, 1:num_roof_layers, jms:jme), INTENT(INOUT) :: TRL_URB3D
!   REAL, DIMENSION(ims:ime, 1:num_wall_layers, jms:jme), INTENT(INOUT) :: TBL_URB3D
!   REAL, DIMENSION(ims:ime, 1:num_road_layers, jms:jme), INTENT(INOUT) :: TGL_URB3D
   REAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TRL_URB3D
   REAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TBL_URB3D
   REAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TGL_URB3D

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SH_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: G_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RN_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D

! multi-layer UCM variables
   REAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TRB_URB4D
   REAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TW1_URB4D
   REAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TW2_URB4D
   REAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TGB_URB4D
   REAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TLEV_URB3D
   REAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: QLEV_URB3D
   REAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW1LEV_URB3D
   REAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW2LEV_URB3D
   REAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TGLEV_URB3D
   REAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TFLEV_URB3D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LF_AC_URB3D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SF_AC_URB3D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CM_AC_URB3D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SFVENT_URB3D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LFVENT_URB3D
   REAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFWIN1_URB3D
   REAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFWIN2_URB3D
   REAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFW1_URB3D
   REAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFW2_URB3D
   REAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFR_URB3D
   REAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFG_URB3D
   REAL, DIMENSION( ims:ime,1:num_urban_hi, jms:jme), INTENT(INOUT) :: HI_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LP_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LB_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: HGT_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: MH_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: STDH_URB2D
   REAL, DIMENSION( ims:ime, 4,jms:jme ), INTENT(INOUT) :: LF_URB2D
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_U_BEP
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_V_BEP
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_T_BEP
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_Q_BEP
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_E_BEP
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_U_BEP
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_V_BEP
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_T_BEP
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_Q_BEP
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_E_BEP
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: VL_BEP
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: DLG_BEP
   REAL, DIMENSION(ims:ime, kms:kme,jms:jme),INTENT(INOUT) :: SF_BEP
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: DL_U_BEP
!
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FRC_URB2D
   INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UTYPE_URB2D
   INTEGER                                            :: UTYPE_URB
!FS
   INTEGER :: SWITCH_URB

   INTEGER :: I,J,K,CHECK

   CHECK = 0

   DO I=ims,ime
    DO J=jms,jme

!      XXXR_URB2D(I,J)=0.
!      XXXB_URB2D(I,J)=0.
!      XXXG_URB2D(I,J)=0.
!      XXXC_URB2D(I,J)=0.

      SH_URB2D(I,J)=0.
      LH_URB2D(I,J)=0.
      G_URB2D(I,J)=0.
      RN_URB2D(I,J)=0.
      
!m
!FS   FRC_URB2D(I,J)=0.
      UTYPE_URB2D(I,J)=0
      SWITCH_URB=0

            IF( IVGTYP(I,J) == ISURBAN)  THEN
               UTYPE_URB2D(I,J) = 2  ! for default. high-intensity
               UTYPE_URB = UTYPE_URB2D(I,J)  ! for default. high-intensity
               IF (HGT_URB2D(I,J)>0.) THEN
                  CONTINUE
               ELSE
                  WRITE(mesg,*) 'USING DEFAULT URBAN MORPHOLOGY'
                  call wrf_message(mesg)
                  LP_URB2D(I,J)=0.
                  LB_URB2D(I,J)=0.
                  HGT_URB2D(I,J)=0.
                  IF ( sf_urban_physics == 1 ) THEN
                     MH_URB2D(I,J)=0.
                     STDH_URB2D(I,J)=0.
                     DO K=1,4
                       LF_URB2D(I,K,J)=0.
                     ENDDO
                  ELSE IF ( ( sf_urban_physics == 2 ) .or. ( sf_urban_physics == 3 ) ) THEN
                     DO K=1,num_urban_hi
                        HI_URB2D(I,K,J)=0.
                     ENDDO
                  ENDIF
               ENDIF
                IF (FRC_URB2D(I,J)>0.and.FRC_URB2D(I,J)<=1.) THEN
                  CONTINUE
                ELSE
                  WRITE(mesg,*) 'WARNING, FRC_URB2D = 0 BUT IVGTYP IS URBAN'
                  call wrf_message(mesg)
                  WRITE(mesg,*) 'WARNING, THE URBAN FRACTION WILL BE READ FROM URBPARM.TBL'
                  call wrf_message(mesg)
                  FRC_URB2D(I,J) = FRC_URB_TBL(UTYPE_URB)
                ENDIF           
               SWITCH_URB=1
            ENDIF 

            IF( IVGTYP(I,J) == 31) THEN
               UTYPE_URB2D(I,J) = 3  ! low-intensity residential
               UTYPE_URB = UTYPE_URB2D(I,J)  ! low-intensity residential
               IF (HGT_URB2D(I,J)>0.) THEN
                  CONTINUE
               ELSE
                  WRITE(mesg,*) 'USING DEFAULT URBAN MORPHOLOGY'
                  call wrf_message(mesg)
                  LP_URB2D(I,J)=0.
                  LB_URB2D(I,J)=0.
                  HGT_URB2D(I,J)=0.
                  IF ( sf_urban_physics == 1 ) THEN
                     MH_URB2D(I,J)=0.
                     STDH_URB2D(I,J)=0.
                     DO K=1,4
                       LF_URB2D(I,K,J)=0.
                     ENDDO
                  ELSE IF ( ( sf_urban_physics == 2 ) .or. ( sf_urban_physics == 3 ) ) THEN
                     DO K=1,num_urban_hi
                        HI_URB2D(I,K,J)=0.
                     ENDDO
                  ENDIF
               ENDIF
                IF (FRC_URB2D(I,J)>0.and.FRC_URB2D(I,J)<=1.) THEN
                  CONTINUE
                ELSE
                  WRITE(mesg,*) 'WARNING, FRC_URB2D = 0 BUT IVGTYP IS URBAN'
                  call wrf_message(mesg)
                  WRITE(mesg,*) 'WARNING, THE URBAN FRACTION WILL BE READ FROM URBPARM.TBL'
                  call wrf_message(mesg)
                  FRC_URB2D(I,J) = FRC_URB_TBL(UTYPE_URB)
                ENDIF         
              SWITCH_URB=1
            ENDIF

           IF( IVGTYP(I,J) == 32) THEN
               UTYPE_URB2D(I,J) = 2  ! high-intensity
               UTYPE_URB = UTYPE_URB2D(I,J)  ! high-intensity
              IF (HGT_URB2D(I,J)>0.) THEN
                  CONTINUE
               ELSE
                  WRITE(mesg,*) 'USING DEFAULT URBAN MORPHOLOGY'
                  call wrf_message(mesg)
                  LP_URB2D(I,J)=0.
                  LB_URB2D(I,J)=0.
                  HGT_URB2D(I,J)=0.
                  IF ( sf_urban_physics == 1 ) THEN
                     MH_URB2D(I,J)=0.
                     STDH_URB2D(I,J)=0.
                     DO K=1,4
                       LF_URB2D(I,K,J)=0.
                     ENDDO
                  ELSE IF ( ( sf_urban_physics == 2 ) .or. ( sf_urban_physics == 3 ) ) THEN
                     DO K=1,num_urban_hi
                        HI_URB2D(I,K,J)=0.
                     ENDDO
                  ENDIF
               ENDIF
                IF (FRC_URB2D(I,J)>0.and.FRC_URB2D(I,J)<=1.) THEN
                  CONTINUE
                ELSE
                  WRITE(mesg,*) 'WARNING, FRC_URB2D = 0 BUT IVGTYP IS URBAN'
                  call wrf_message(mesg)
                  WRITE(mesg,*) 'WARNING, THE URBAN FRACTION WILL BE READ FROM URBPARM.TBL'
                  call wrf_message(mesg)
                  FRC_URB2D(I,J) = FRC_URB_TBL(UTYPE_URB)
                ENDIF
              SWITCH_URB=1
            ENDIF

            IF( IVGTYP(I,J) == 33) THEN
               UTYPE_URB2D(I,J) = 1  !  Commercial/Industrial/Transportation
               UTYPE_URB = UTYPE_URB2D(I,J)  !  Commercial/Industrial/Transportation
              IF (HGT_URB2D(I,J)>0.) THEN
                  CONTINUE
               ELSE
                  WRITE(mesg,*) 'USING DEFAULT URBAN MORPHOLOGY'
                  call wrf_message(mesg)
                  LP_URB2D(I,J)=0.
                  LB_URB2D(I,J)=0.
                  HGT_URB2D(I,J)=0.
                  IF ( sf_urban_physics == 1 ) THEN
                     MH_URB2D(I,J)=0.
                     STDH_URB2D(I,J)=0.
                     DO K=1,4
                       LF_URB2D(I,K,J)=0.
                     ENDDO
                  ELSE IF ( ( sf_urban_physics == 2 ) .or. ( sf_urban_physics == 3 ) ) THEN
                     DO K=1,num_urban_hi
                        HI_URB2D(I,K,J)=0.
                     ENDDO
                  ENDIF
               ENDIF
                IF (FRC_URB2D(I,J)>0.and.FRC_URB2D(I,J)<=1.) THEN
                  CONTINUE
                ELSE
                  WRITE(mesg,*) 'WARNING, FRC_URB2D = 0 BUT IVGTYP IS URBAN'
                  call wrf_message(mesg)
                  WRITE(mesg,*) 'WARNING, THE URBAN FRACTION WILL BE READ FROM URBPARM.TBL'
                  call wrf_message(mesg)
                  FRC_URB2D(I,J) = FRC_URB_TBL(UTYPE_URB)
                ENDIF
               SWITCH_URB=1
            ENDIF

            IF (SWITCH_URB==1) THEN
                CONTINUE
            ELSE
                FRC_URB2D(I,J)=0.
                LP_URB2D(I,J)=0.
                LB_URB2D(I,J)=0.
                HGT_URB2D(I,J)=0.
                IF ( sf_urban_physics == 1 ) THEN
                   MH_URB2D(I,J)=0.
                   STDH_URB2D(I,J)=0.
                   DO K=1,4
                      LF_URB2D(I,K,J)=0.
                   ENDDO
                ELSE IF ( ( sf_urban_physics == 2 ) .or. ( sf_urban_physics == 3 ) ) THEN
                   DO K=1,num_urban_hi
                      HI_URB2D(I,K,J)=0.
                   ENDDO
                ENDIF
            ENDIF


      QC_URB2D(I,J)=0.01

       IF (.not.restart) THEN

      XXXR_URB2D(I,J)=0.
      XXXB_URB2D(I,J)=0.
      XXXG_URB2D(I,J)=0.
      XXXC_URB2D(I,J)=0.


      TC_URB2D(I,J)=TSURFACE0_URB(I,J)+0.
      TR_URB2D(I,J)=TSURFACE0_URB(I,J)+0.
      TB_URB2D(I,J)=TSURFACE0_URB(I,J)+0.
      TG_URB2D(I,J)=TSURFACE0_URB(I,J)+0.
!
      TS_URB2D(I,J)=TSURFACE0_URB(I,J)+0.

!      DO K=1,num_roof_layers
!     DO K=1,num_soil_layers
!         TRL_URB3D(I,1,J)=TLAYER0_URB(I,1,J)+0.
!         TRL_URB3D(I,2,J)=TLAYER0_URB(I,2,J)+0.
!         TRL_URB3D(I,3,J)=TLAYER0_URB(I,3,J)+0.
!         TRL_URB3D(I,4,J)=TLAYER0_URB(I,4,J)+0.

          TRL_URB3D(I,1,J)=TLAYER0_URB(I,1,J)+0.
          TRL_URB3D(I,2,J)=0.5*(TLAYER0_URB(I,1,J)+TLAYER0_URB(I,2,J))
          TRL_URB3D(I,3,J)=TLAYER0_URB(I,2,J)+0.
          TRL_URB3D(I,4,J)=TLAYER0_URB(I,2,J)+(TLAYER0_URB(I,3,J)-TLAYER0_URB(I,2,J))*0.29
!     END DO

!      DO K=1,num_wall_layers
!      DO K=1,num_soil_layers
!m        TBL_URB3D(I,1,J)=TLAYER0_URB(I,1,J)+0.
!m        TBL_URB3D(I,2,J)=TLAYER0_URB(I,2,J)+0.
!m        TBL_URB3D(I,3,J)=TLAYER0_URB(I,3,J)+0.
!m        TBL_URB3D(I,4,J)=TLAYER0_URB(I,4,J)+0.

        TBL_URB3D(I,1,J)=TLAYER0_URB(I,1,J)+0.
        TBL_URB3D(I,2,J)=0.5*(TLAYER0_URB(I,1,J)+TLAYER0_URB(I,2,J))
        TBL_URB3D(I,3,J)=TLAYER0_URB(I,2,J)+0.
        TBL_URB3D(I,4,J)=TLAYER0_URB(I,2,J)+(TLAYER0_URB(I,3,J)-TLAYER0_URB(I,2,J))*0.29

!      END DO

!      DO K=1,num_road_layers
      DO K=1,num_soil_layers
        TGL_URB3D(I,K,J)=TLAYER0_URB(I,K,J)+0.
      END DO
      
! multi-layer urban
!      IF( sf_urban_physics .EQ. 2)THEN
       IF((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.eq.3)) THEN
      DO k=1,num_urban_layers
!        TRB_URB4D(I,k,J)=TSURFACE0_URB(I,J)
!        TW1_URB4D(I,k,J)=TSURFACE0_URB(I,J)
!        TW2_URB4D(I,k,J)=TSURFACE0_URB(I,J)
!        TGB_URB4D(I,k,J)=TSURFACE0_URB(I,J)
!MT        TRB_URB4D(I,K,J)=tlayer0_urb(I,1,J)
!MT        TW1_URB4D(I,K,J)=tlayer0_urb(I,1,J)
!MT        TW2_URB4D(I,K,J)=tlayer0_urb(I,1,J)
        IF (UTYPE_URB2D(I,J) > 0) THEN
           TRB_URB4D(I,K,J)=TBLEND_TBL(UTYPE_URB2D(I,J))
           TW1_URB4D(I,K,J)=TBLEND_TBL(UTYPE_URB2D(I,J))
           TW2_URB4D(I,K,J)=TBLEND_TBL(UTYPE_URB2D(I,J))
        ELSE
           TRB_URB4D(I,K,J)=tlayer0_urb(I,1,J)
           TW1_URB4D(I,K,J)=tlayer0_urb(I,1,J)
           TW2_URB4D(I,K,J)=tlayer0_urb(I,1,J)
        ENDIF
        TGB_URB4D(I,K,J)=tlayer0_urb(I,1,J)
        SFW1_URB3D(I,K,J)=0.
        SFW2_URB3D(I,K,J)=0.
        SFR_URB3D(I,K,J)=0.
        SFG_URB3D(I,K,J)=0.
      ENDDO
       
       ENDIF 
              
      if (SF_URBAN_PHYSICS.EQ.3) then
         LF_AC_URB3D(I,J)=0.
         SF_AC_URB3D(I,J)=0.
         CM_AC_URB3D(I,J)=0.
         SFVENT_URB3D(I,J)=0.
         LFVENT_URB3D(I,J)=0.

         DO K=1,num_urban_layers
            TLEV_URB3D(I,K,J)=tlayer0_urb(I,1,J)
            TW1LEV_URB3D(I,K,J)=tlayer0_urb(I,1,J)
            TW2LEV_URB3D(I,K,J)=tlayer0_urb(I,1,J)
            TGLEV_URB3D(I,K,J)=tlayer0_urb(I,1,J)
            TFLEV_URB3D(I,K,J)=tlayer0_urb(I,1,J)
            QLEV_URB3D(I,K,J)=0.01
            SFWIN1_URB3D(I,K,J)=0.
            SFWIN2_URB3D(I,K,J)=0.
!rm         LF_AC_URB3D(I,J)=0.
!rm         SF_AC_URB3D(I,J)=0.
!rm         CM_AC_URB3D(I,J)=0.
!rm         SFVENT_URB3D(I,J)=0.
!rm         LFVENT_URB3D(I,J)=0.
         ENDDO

      endif

!      IF( sf_urban_physics .EQ. 2 )THEN
      IF((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.eq.3)) THEN
      DO K= KMS,KME
          SF_BEP(I,K,J)=1.
          VL_BEP(I,K,J)=1.
          A_U_BEP(I,K,J)=0.
          A_V_BEP(I,K,J)=0.
          A_T_BEP(I,K,J)=0.
          A_E_BEP(I,K,J)=0.
          A_Q_BEP(I,K,J)=0.
          B_U_BEP(I,K,J)=0.
          B_V_BEP(I,K,J)=0.
          B_T_BEP(I,K,J)=0.
          B_E_BEP(I,K,J)=0.
          B_Q_BEP(I,K,J)=0.
          DLG_BEP(I,K,J)=0.
          DL_U_BEP(I,K,J)=0.
      END DO
      ENDIF       !sf_urban_physics=2
     ENDIF        !restart


      IF (CHECK.EQ.0)THEN
      IF(IVGTYP(I,J).EQ.1)THEN
        write(mesg,*) 'TSURFACE0_URB',TSURFACE0_URB(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'TDEEP0_URB', TDEEP0_URB(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'IVGTYP',IVGTYP(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'TR_URB2D',TR_URB2D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'TB_URB2D',TB_URB2D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'TG_URB2D',TG_URB2D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'TC_URB2D',TC_URB2D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'QC_URB2D',QC_URB2D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'XXXR_URB2D',XXXR_URB2D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'SH_URB2D',SH_URB2D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'LH_URB2D',LH_URB2D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'G_URB2D',G_URB2D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'RN_URB2D',RN_URB2D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'TS_URB2D',TS_URB2D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'LF_AC_URB3D', LF_AC_URB3D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'SF_AC_URB3D', SF_AC_URB3D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'CM_AC_URB3D', CM_AC_URB3D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'SFVENT_URB3D', SFVENT_URB3D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'LFVENT_URB3D', LFVENT_URB3D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'FRC_URB2D', FRC_URB2D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'UTYPE_URB2D', UTYPE_URB2D(I,J)
        call wrf_message(mesg)
        write(mesg,*) 'I',I,'J',J
        call wrf_message(mesg)
        write(mesg,*) 'num_urban_hi', num_urban_hi
        call wrf_message(mesg)
        CHECK = 1
       END IF
       END IF




    END DO
   END DO
   RETURN
   END SUBROUTINE urban_var_init
!===========================================================================
!
!  force_restore
!
!===========================================================================
   SUBROUTINE force_restore(CAP,AKS,DELT,S,R,H,LE,TSLEND,TSP,TS)

     REAL, INTENT(IN)  :: CAP,AKS,DELT,S,R,H,LE,TSLEND,TSP
     REAL, INTENT(OUT) :: TS
     REAL              :: C1,C2

     C2=24.*3600./2./3.14159
     C1=SQRT(0.5*C2*CAP*AKS)

     TS = TSP + DELT*( (S+R-H-LE)/C1 -(TSP-TSLEND)/C2 )

   END SUBROUTINE force_restore
!===========================================================================
!
!  bisection (not used)
!
!============================================================================== 
   SUBROUTINE bisection(TSP,PS,S,EPS,RX,SIG,RHO,CP,CH,UA,QA,TA,EL,BET,AKS,TSL,DZ,TS) 

     REAL, INTENT(IN) :: TSP,PS,S,EPS,RX,SIG,RHO,CP,CH,UA,QA,TA,EL,BET,AKS,TSL,DZ
     REAL, INTENT(OUT) :: TS
     REAL :: ES,QS0,R,H,ELE,G0,F1,F

     TS1 = TSP - 5.
     TS2 = TSP + 5.

     DO ITERATION = 1,22

       ES=6.11*EXP( (2.5*10.**6./461.51)*(TS1-273.15)/(273.15*TS1) )
       QS0=0.622*ES/(PS-0.378*ES)
       R=EPS*(RX-SIG*(TS1**4.)/60.)
       H=RHO*CP*CH*UA*(TS1-TA)*100.
       ELE=RHO*EL*CH*UA*BET*(QS0-QA)*100.
       G0=AKS*(TS1-TSL)/(DZ/2.)
       F1= S + R - H - ELE - G0

       TS=0.5*(TS1+TS2)

       ES=6.11*EXP( (2.5*10.**6./461.51)*(TS-273.15)/(273.15*TS) )
       QS0=0.622*ES/(PS-0.378*ES) 
       R=EPS*(RX-SIG*(TS**4.)/60.)
       H=RHO*CP*CH*UA*(TS-TA)*100.
       ELE=RHO*EL*CH*UA*BET*(QS0-QA)*100.
       G0=AKS*(TS-TSL)/(DZ/2.)
       F = S + R - H - ELE - G0

       IF (F1*F > 0.0) THEN
         TS1=TS
        ELSE
         TS2=TS
       END IF

     END DO

     RETURN
END SUBROUTINE bisection
!===========================================================================

SUBROUTINE SFCDIF_URB (ZLM,Z0,THZ0,THLM,SFCSPD,AKANDA,AKMS,AKHS,RLMO,CD)

! ----------------------------------------------------------------------
! SUBROUTINE SFCDIF_URB (Urban version of SFCDIF_off)
! ----------------------------------------------------------------------
! CALCULATE SURFACE LAYER EXCHANGE COEFFICIENTS VIA ITERATIVE PROCESS.
! SEE CHEN ET AL (1997, BLM)
! ----------------------------------------------------------------------

      IMPLICIT NONE
      REAL     WWST, WWST2, G, VKRM, EXCM, BETA, BTG, ELFC, WOLD, WNEW
      REAL     PIHF, EPSU2, EPSUST, EPSIT, EPSA, ZTMIN, ZTMAX, HPBL,     &
     & SQVISC
      REAL     RIC, RRIC, FHNEU, RFC,RLMO_THR, RFAC, ZZ, PSLMU, PSLMS, PSLHU,     &
     & PSLHS
      REAL     XX, PSPMU, YY, PSPMS, PSPHU, PSPHS, ZLM, Z0, THZ0, THLM
      REAL     SFCSPD, AKANDA, AKMS, AKHS, ZU, ZT, RDZ, CXCH
      REAL     DTHV, DU2, BTGH, WSTAR2, USTAR, ZSLU, ZSLT, RLOGU, RLOGT
      REAL     RLMO, ZETALT, ZETALU, ZETAU, ZETAT, XLU4, XLT4, XU4, XT4
!CC   ......REAL ZTFC

      REAL     XLU, XLT, XU, XT, PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN,  &
     &         RLMA

      INTEGER  ITRMX, ILECH, ITR
      REAL,    INTENT(OUT) :: CD
      PARAMETER                                                         &
     &        (WWST = 1.2,WWST2 = WWST * WWST,G = 9.8,VKRM = 0.40,      &
     &         EXCM = 0.001                                             &
     &        ,BETA = 1./270.,BTG = BETA * G,ELFC = VKRM * BTG          &
     &                  ,WOLD =.15,WNEW = 1. - WOLD,ITRMX = 05,         &
     &                   PIHF = 3.14159265/2.)
      PARAMETER                                                         &
     &         (EPSU2 = 1.E-4,EPSUST = 0.07,EPSIT = 1.E-4,EPSA = 1.E-8  &
     &         ,ZTMIN = -5.,ZTMAX = 1.,HPBL = 1000.0                    &
     &          ,SQVISC = 258.2)
      PARAMETER                                                         &
     &       (RIC = 0.183,RRIC = 1.0/ RIC,FHNEU = 0.8,RFC = 0.191       &
     &        ,RLMO_THR = 0.001,RFAC = RIC / (FHNEU * RFC * RFC))

! ----------------------------------------------------------------------
! NOTE: THE TWO CODE BLOCKS BELOW DEFINE FUNCTIONS
! ----------------------------------------------------------------------
! LECH'S SURFACE FUNCTIONS
! ----------------------------------------------------------------------
      PSLMU (ZZ)= -0.96* log (1.0-4.5* ZZ)
      PSLMS (ZZ)= ZZ * RRIC -2.076* (1. -1./ (ZZ +1.))
      PSLHU (ZZ)= -0.96* log (1.0-4.5* ZZ)

! ----------------------------------------------------------------------
! PAULSON'S SURFACE FUNCTIONS
! ----------------------------------------------------------------------
      PSLHS (ZZ)= ZZ * RFAC -2.076* (1. -1./ (ZZ +1.))
      PSPMU (XX)= -2.* log ( (XX +1.)*0.5) - log ( (XX * XX +1.)*0.5)   &
     &        +2.* ATAN (XX)                                            &
     &- PIHF
      PSPMS (YY)= 5.* YY
      PSPHU (XX)= -2.* log ( (XX * XX +1.)*0.5)

! ----------------------------------------------------------------------
! THIS ROUTINE SFCDIF CAN HANDLE BOTH OVER OPEN WATER (SEA, OCEAN) AND
! OVER SOLID SURFACE (LAND, SEA-ICE).
! ----------------------------------------------------------------------
      PSPHS (YY)= 5.* YY

! ----------------------------------------------------------------------
!     ZTFC: RATIO OF ZOH/ZOM  LESS OR EQUAL THAN 1
!     C......ZTFC=0.1
!     CZIL: CONSTANT C IN Zilitinkevich, S. S.1995,:NOTE ABOUT ZT
! ----------------------------------------------------------------------
      ILECH = 0

! ----------------------------------------------------------------------
!      ZILFC = - CZIL * VKRM * SQVISC
!     C.......ZT=Z0*ZTFC
      ZU = Z0
      RDZ = 1./ ZLM
      CXCH = EXCM * RDZ
      DTHV = THLM - THZ0

! ----------------------------------------------------------------------
! BELJARS CORRECTION OF USTAR
! ----------------------------------------------------------------------
      DU2 = MAX (SFCSPD * SFCSPD,EPSU2)
!cc   If statements to avoid TANGENT LINEAR problems near zero
      BTGH = BTG * HPBL
      IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
         WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
      ELSE
         WSTAR2 = 0.0
      END IF

! ----------------------------------------------------------------------
! ZILITINKEVITCH APPROACH FOR ZT
! ----------------------------------------------------------------------
      USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)

! ----------------------------------------------------------------------
! KCL/TL Try Kanda approach instead (Kanda et al. 2007, JAMC)
!      ZT = EXP (ZILFC * SQRT (USTAR * Z0))* Z0
      ZT = EXP (2.0-AKANDA*(SQVISC**2 * USTAR * Z0)**0.25)* Z0
      
      ZSLU = ZLM + ZU

      ZSLT = ZLM + ZT
      RLOGU = log (ZSLU / ZU)

      RLOGT = log (ZSLT / ZT)

      RLMO = ELFC * AKHS * DTHV / USTAR **3
! ----------------------------------------------------------------------
! 1./MONIN-OBUKKHOV LENGTH-SCALE
! ----------------------------------------------------------------------
      DO ITR = 1,ITRMX
         ZETALT = MAX (ZSLT * RLMO,ZTMIN)
         RLMO = ZETALT / ZSLT
         ZETALU = ZSLU * RLMO
         ZETAU = ZU * RLMO

         ZETAT = ZT * RLMO
         IF (ILECH .eq. 0) THEN
            IF (RLMO .lt. 0.0)THEN 
               XLU4 = 1. -16.* ZETALU
               XLT4 = 1. -16.* ZETALT
               XU4 = 1. -16.* ZETAU

               XT4 = 1. -16.* ZETAT
               XLU = SQRT (SQRT (XLU4))
               XLT = SQRT (SQRT (XLT4))
               XU = SQRT (SQRT (XU4))

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
! BELJAARS CORRECTION FOR USTAR
! ----------------------------------------------------------------------
         END IF
            USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)
            !KCL/TL
            !ZT = EXP (ZILFC * SQRT (USTAR * Z0))* Z0
            ZT = EXP (2.0-AKANDA*(SQVISC**2 * USTAR * Z0)**0.25)* Z0
            ZSLT = ZLM + ZT
            RLOGT = log (ZSLT / ZT)
            USTARK = USTAR * VKRM
            AKMS = MAX (USTARK / SIMM,CXCH)
            AKHS = MAX (USTARK / SIMH,CXCH)
!
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

      END DO

      CD = USTAR*USTAR/SFCSPD**2
! ----------------------------------------------------------------------
  END SUBROUTINE SFCDIF_URB
! ----------------------------------------------------------------------
!===========================================================================
END MODULE module_sf_urban
