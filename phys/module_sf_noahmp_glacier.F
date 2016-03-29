MODULE NOAHMP_GLACIER_GLOBALS

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

! =====================================options for different schemes================================

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

! options for glacier treatment
! 1 -> include phase change of ice; 2 -> ice treatment more like original Noah

  INTEGER :: OPT_GLA != 1    !(suggested 1)

! adjustable parameters for snow processes

  REAL, PARAMETER :: Z0SNO  = 0.002  !snow surface roughness length (m) (0.002)
  REAL, PARAMETER :: SSI    = 0.03   !liquid water holding capacity for snowpack (m3/m3) (0.03)
  REAL, PARAMETER :: SWEMX  = 1.00   !new snow mass to fully cover old snow (mm)
                                     !equivalent to 10mm depth (density = 100 kg/m3)

!------------------------------------------------------------------------------------------!
END MODULE NOAHMP_GLACIER_GLOBALS
!------------------------------------------------------------------------------------------!

MODULE NOAHMP_GLACIER_ROUTINES
  USE NOAHMP_GLACIER_GLOBALS
  IMPLICIT NONE

  public  :: NOAHMP_OPTIONS_GLACIER
  public  :: NOAHMP_GLACIER

  private :: ATM_GLACIER
  private :: ENERGY_GLACIER
  private ::       THERMOPROP_GLACIER
  private ::               CSNOW_GLACIER
  private ::       RADIATION_GLACIER
  private ::               SNOW_AGE_GLACIER
  private ::               SNOWALB_BATS_GLACIER  
  private ::               SNOWALB_CLASS_GLACIER
  private ::       GLACIER_FLUX
  private ::               SFCDIF1_GLACIER                  
  private ::       TSNOSOI_GLACIER
  private ::               HRT_GLACIER
  private ::               HSTEP_GLACIER   
  private ::                         ROSR12_GLACIER
  private ::       PHASECHANGE_GLACIER

  private :: WATER_GLACIER
  private ::       SNOWWATER_GLACIER
  private ::               SNOWFALL_GLACIER
  private ::               COMBINE_GLACIER
  private ::               DIVIDE_GLACIER
  private ::                         COMBO_GLACIER
  private ::               COMPACT_GLACIER
  private ::               SNOWH2O_GLACIER

  private :: ERROR_GLACIER

contains
!
! ==================================================================================================

  SUBROUTINE NOAHMP_GLACIER (&
                   ILOC    ,JLOC    ,COSZ    ,NSNOW   ,NSOIL   ,DT      , & ! IN : Time/Space/Model-related
                   SFCTMP  ,SFCPRS  ,UU      ,VV      ,Q2      ,SOLDN   , & ! IN : Forcing
                   PRCP    ,LWDN    ,TBOT    ,ZLVL    ,FICEOLD ,ZSOIL   , & ! IN : Forcing
                   QSNOW   ,SNEQVO  ,ALBOLD  ,CM      ,CH      ,ISNOW   , & ! IN/OUT : 
                   SNEQV   ,SMC     ,ZSNSO   ,SNOWH   ,SNICE   ,SNLIQ   , & ! IN/OUT :
                   TG      ,STC     ,SH2O    ,TAUSS   ,QSFC    ,          & ! IN/OUT : 
                   FSA     ,FSR     ,FIRA    ,FSH     ,FGEV    ,SSOIL   , & ! OUT : 
                   TRAD    ,EDIR    ,RUNSRF  ,RUNSUB  ,SAG     ,ALBEDO  , & ! OUT :
                   QSNBOT  ,PONDING ,PONDING1,PONDING2,T2M     ,Q2E     , & ! OUT :
                   EMISSI,  FPICE,    CH2B                                & ! OUT :
#ifdef WRF_HYDRO
                   , sfcheadrt                                            &
#endif
                   )

! --------------------------------------------------------------------------------------------------
! Initial code: Guo-Yue Niu, Oct. 2007
! Modified to glacier: Michael Barlage, June 2012
! --------------------------------------------------------------------------------------------------
  implicit none
! --------------------------------------------------------------------------------------------------
! input
  INTEGER                        , INTENT(IN)    :: ILOC   !grid index
  INTEGER                        , INTENT(IN)    :: JLOC   !grid index
  REAL                           , INTENT(IN)    :: COSZ   !cosine solar zenith angle [0-1]
  INTEGER                        , INTENT(IN)    :: NSNOW  !maximum no. of snow layers        
  INTEGER                        , INTENT(IN)    :: NSOIL  !no. of soil layers        
  REAL                           , INTENT(IN)    :: DT     !time step [sec]
  REAL                           , INTENT(IN)    :: SFCTMP !surface air temperature [K]
  REAL                           , INTENT(IN)    :: SFCPRS !pressure (pa)
  REAL                           , INTENT(IN)    :: UU     !wind speed in eastward dir (m/s)
  REAL                           , INTENT(IN)    :: VV     !wind speed in northward dir (m/s)
  REAL                           , INTENT(IN)    :: Q2     !mixing ratio (kg/kg) lowest model layer
  REAL                           , INTENT(IN)    :: SOLDN  !downward shortwave radiation (w/m2)
  REAL                           , INTENT(IN)    :: PRCP   !precipitation rate (kg m-2 s-1)
  REAL                           , INTENT(IN)    :: LWDN   !downward longwave radiation (w/m2)
  REAL                           , INTENT(IN)    :: TBOT   !bottom condition for soil temp. [K]
  REAL                           , INTENT(IN)    :: ZLVL   !reference height (m)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD!ice fraction at last timestep
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !layer-bottom depth from soil surf (m)

#ifdef WRF_HYDRO
  REAL                           , INTENT(INOUT)    :: sfcheadrt
#endif

! input/output : need arbitary intial values
  REAL                           , INTENT(INOUT) :: QSNOW  !snowfall [mm/s]
  REAL                           , INTENT(INOUT) :: SNEQVO !snow mass at last time step (mm)
  REAL                           , INTENT(INOUT) :: ALBOLD !snow albedo at last time step (CLASS type)
  REAL                           , INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL                           , INTENT(INOUT) :: CH     !sensible heat exchange coefficient

! prognostic variables
  INTEGER                        , INTENT(INOUT) :: ISNOW  !actual no. of snow layers [-]
  REAL                           , INTENT(INOUT) :: SNEQV  !snow water eqv. [mm]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  !layer-bottom depth from snow surf [m]
  REAL                           , INTENT(INOUT) :: SNOWH  !snow height [m]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]
  REAL                           , INTENT(INOUT) :: TG     !ground temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow/soil temperature [k]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !liquid soil moisture [m3/m3]
  REAL                           , INTENT(INOUT) :: TAUSS  !non-dimensional snow age
  REAL                           , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer

! output
  REAL                           , INTENT(OUT)   :: FSA    !total absorbed solar radiation (w/m2)
  REAL                           , INTENT(OUT)   :: FSR    !total reflected solar radiation (w/m2)
  REAL                           , INTENT(OUT)   :: FIRA   !total net LW rad (w/m2)  [+ to atm]
  REAL                           , INTENT(OUT)   :: FSH    !total sensible heat (w/m2) [+ to atm]
  REAL                           , INTENT(OUT)   :: FGEV   !ground evap heat (w/m2) [+ to atm]
  REAL                           , INTENT(OUT)   :: SSOIL  !ground heat flux (w/m2)   [+ to soil]
  REAL                           , INTENT(OUT)   :: TRAD   !surface radiative temperature (k)
  REAL                           , INTENT(OUT)   :: EDIR   !soil surface evaporation rate (mm/s]
  REAL                           , INTENT(OUT)   :: RUNSRF !surface runoff [mm/s] 
  REAL                           , INTENT(OUT)   :: RUNSUB !baseflow (saturation excess) [mm/s]
  REAL                           , INTENT(OUT)   :: SAG    !solar rad absorbed by ground (w/m2)
  REAL                           , INTENT(OUT)   :: ALBEDO !surface albedo [-]
  REAL                           , INTENT(OUT)   :: QSNBOT !snowmelt [mm/s]
  REAL                           , INTENT(OUT)   :: PONDING!surface ponding [mm]
  REAL                           , INTENT(OUT)   :: PONDING1!surface ponding [mm]
  REAL                           , INTENT(OUT)   :: PONDING2!surface ponding [mm]
  REAL                           , INTENT(OUT)   :: T2M     !2-m air temperature over bare ground part [k]
  REAL                           , INTENT(OUT)   :: Q2E
  REAL                           , INTENT(OUT)   :: EMISSI
  REAL                           , INTENT(OUT)   :: FPICE
  REAL                           , INTENT(OUT)   :: CH2B

! local
  INTEGER                                        :: IZ     !do-loop index
  INTEGER, DIMENSION(-NSNOW+1:NSOIL)             :: IMELT  !phase change index [1-melt; 2-freeze]
  REAL                                           :: RHOAIR !density air (kg/m3)
  REAL, DIMENSION(-NSNOW+1:NSOIL)                :: DZSNSO !snow/soil layer thickness [m]
  REAL                                           :: THAIR  !potential temperature (k)
  REAL                                           :: QAIR   !specific humidity (kg/kg) (q2/(1+q2))
  REAL                                           :: EAIR   !vapor pressure air (pa)
  REAL, DIMENSION(       1:    2)                :: SOLAD  !incoming direct solar rad (w/m2)
  REAL, DIMENSION(       1:    2)                :: SOLAI  !incoming diffuse solar rad (w/m2)
  REAL, DIMENSION(       1:NSOIL)                :: SICE   !soil ice content (m3/m3)
  REAL, DIMENSION(-NSNOW+1:    0)                :: SNICEV !partial volume ice of snow [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0)                :: SNLIQV !partial volume liq of snow [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0)                :: EPORE  !effective porosity [m3/m3]
  REAL                                           :: QDEW   !ground surface dew rate [mm/s]
  REAL                                           :: QVAP   !ground surface evap. rate [mm/s]
  REAL                                           :: LATHEA !latent heat [j/kg]
  REAL                                           :: QMELT  !internal pack melt
  REAL                                           :: SWDOWN !downward solar [w/m2]
  REAL                                           :: BEG_WB !beginning water for error check
  REAL                                           :: ZBOT = -8.0 

  CHARACTER*256 message

! --------------------------------------------------------------------------------------------------
! re-process atmospheric forcing

   CALL ATM_GLACIER (SFCPRS ,SFCTMP ,Q2     ,SOLDN  ,COSZ   ,THAIR  , & 
                     QAIR   ,EAIR   ,RHOAIR ,SOLAD  ,SOLAI  ,SWDOWN )

   BEG_WB = SNEQV

! snow/soil layer thickness (m); interface depth: ZSNSO < 0; layer thickness DZSNSO > 0

     DO IZ = ISNOW+1, NSOIL
         IF(IZ == ISNOW+1) THEN
           DZSNSO(IZ) = - ZSNSO(IZ)
         ELSE
           DZSNSO(IZ) = ZSNSO(IZ-1) - ZSNSO(IZ)
         END IF
     END DO

! compute energy budget (momentum & energy fluxes and phase changes) 

    CALL ENERGY_GLACIER (NSNOW  ,NSOIL  ,ISNOW  ,DT     ,QSNOW  ,RHOAIR , & !in
                         EAIR   ,SFCPRS ,QAIR   ,SFCTMP ,LWDN   ,UU     , & !in
                         VV     ,SOLAD  ,SOLAI  ,COSZ   ,ZLVL   ,         & !in
                         TBOT   ,ZBOT   ,ZSNSO  ,DZSNSO ,                 & !in
                         TG     ,STC    ,SNOWH  ,SNEQV  ,SNEQVO ,SH2O   , & !inout
                         SMC    ,SNICE  ,SNLIQ  ,ALBOLD ,CM     ,CH     , & !inout
                         TAUSS  ,QSFC   ,                                 & !inout
                         IMELT  ,SNICEV ,SNLIQV ,EPORE  ,QMELT  ,PONDING, & !out
		         SAG    ,FSA    ,FSR    ,FIRA   ,FSH    ,FGEV   , & !out
		         TRAD   ,T2M    ,SSOIL  ,LATHEA ,Q2E    ,EMISSI, CH2B )   !out

    SICE = MAX(0.0, SMC - SH2O)   
    SNEQVO  = SNEQV

    QVAP = MAX( FGEV/LATHEA, 0.)       ! positive part of fgev [mm/s] > 0
    QDEW = ABS( MIN(FGEV/LATHEA, 0.))  ! negative part of fgev [mm/s] > 0
    EDIR = QVAP - QDEW

! compute water budgets (water storages, ET components, and runoff)

     CALL WATER_GLACIER (NSNOW  ,NSOIL  ,IMELT  ,DT     ,PRCP   ,SFCTMP , & !in
                         QVAP   ,QDEW   ,FICEOLD,ZSOIL  ,                 & !in
                         ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  ,STC    , & !inout
                         DZSNSO ,SH2O   ,SICE   ,PONDING,ZSNSO  ,FSH    , & !inout
                         RUNSRF ,RUNSUB ,QSNOW  ,PONDING1       ,PONDING2,QSNBOT,FPICE &  !out
#ifdef WRF_HYDRO
                        , sfcheadrt                     &
#endif
                        )

     IF(OPT_GLA == 2) THEN
       EDIR = QVAP - QDEW
       FGEV = EDIR * LATHEA
     END IF

     IF(MAXVAL(SICE) < 0.0001) THEN
       WRITE(message,*) "GLACIER HAS MELTED AT:",ILOC,JLOC," ARE YOU SURE THIS SHOULD BE A GLACIER POINT?"
       CALL wrf_debug(10,TRIM(message))
     END IF
     
! water and energy balance check

     CALL ERROR_GLACIER (ILOC   ,JLOC   ,SWDOWN ,FSA    ,FSR    ,FIRA   , &
                         FSH    ,FGEV   ,SSOIL  ,SAG    ,PRCP   ,EDIR   , &
		         RUNSRF ,RUNSUB ,SNEQV  ,DT     ,BEG_WB )

    IF(SNOWH <= 1.E-6 .OR. SNEQV <= 1.E-3) THEN
     SNOWH = 0.0
     SNEQV = 0.0
    END IF

    IF(SWDOWN.NE.0.) THEN
      ALBEDO = FSR / SWDOWN
    ELSE
      ALBEDO = -999.9
    END IF
    

  END SUBROUTINE NOAHMP_GLACIER
! ==================================================================================================
  SUBROUTINE ATM_GLACIER (SFCPRS ,SFCTMP ,Q2     ,SOLDN  ,COSZ   ,THAIR  , &
                          QAIR   ,EAIR   ,RHOAIR ,SOLAD  ,SOLAI  , &
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
  REAL                          , INTENT(IN)  :: COSZ   !cosine solar zenith angle [0-1]

! outputs

  REAL                          , INTENT(OUT) :: THAIR  !potential temperature (k)
  REAL                          , INTENT(OUT) :: QAIR   !specific humidity (kg/kg) (q2/(1+q2))
  REAL                          , INTENT(OUT) :: EAIR   !vapor pressure air (pa)
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAD  !incoming direct solar radiation (w/m2)
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAI  !incoming diffuse solar radiation (w/m2)
  REAL                          , INTENT(OUT) :: RHOAIR !density air (kg/m3)
  REAL                          , INTENT(OUT) :: SWDOWN !downward solar filtered by sun angle [w/m2]

!locals

  REAL                                        :: PAIR   !atm bottom level pressure (pa)
! --------------------------------------------------------------------------------------------------

       PAIR   = SFCPRS                   ! atm bottom level pressure (pa)
       THAIR  = SFCTMP * (SFCPRS/PAIR)**(RAIR/CPAIR) 
!       QAIR   = Q2 / (1.0+Q2)           ! mixing ratio to specific humidity [kg/kg]
       QAIR   = Q2                       ! In WRF, driver converts to specific humidity

       EAIR   = QAIR*SFCPRS / (0.622+0.378*QAIR)
       RHOAIR = (SFCPRS-0.378*EAIR) / (RAIR*SFCTMP)

       IF(COSZ <= 0.) THEN 
          SWDOWN = 0.
       ELSE
          SWDOWN = SOLDN
       END IF 

       SOLAD(1) = SWDOWN*0.7*0.5     ! direct  vis
       SOLAD(2) = SWDOWN*0.7*0.5     ! direct  nir
       SOLAI(1) = SWDOWN*0.3*0.5     ! diffuse vis
       SOLAI(2) = SWDOWN*0.3*0.5     ! diffuse nir

  END SUBROUTINE ATM_GLACIER
! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE ENERGY_GLACIER (NSNOW  ,NSOIL  ,ISNOW  ,DT     ,QSNOW  ,RHOAIR , & !in
                             EAIR   ,SFCPRS ,QAIR   ,SFCTMP ,LWDN   ,UU     , & !in
                             VV     ,SOLAD  ,SOLAI  ,COSZ   ,ZREF   ,         & !in
                             TBOT   ,ZBOT   ,ZSNSO  ,DZSNSO ,                 & !in
                             TG     ,STC    ,SNOWH  ,SNEQV  ,SNEQVO ,SH2O   , & !inout
                             SMC    ,SNICE  ,SNLIQ  ,ALBOLD ,CM     ,CH     , & !inout
                             TAUSS  ,QSFC   ,                                 & !inout
                             IMELT  ,SNICEV ,SNLIQV ,EPORE  ,QMELT  ,PONDING, & !out
                             SAG    ,FSA    ,FSR    ,FIRA   ,FSH    ,FGEV   , & !out
                             TRAD   ,T2M    ,SSOIL  ,LATHEA ,Q2E    ,EMISSI, CH2B )   !out

! --------------------------------------------------------------------------------------------------
! --------------------------------------------------------------------------------------------------
!  USE NOAHMP_VEG_PARAMETERS
!  USE NOAHMP_RAD_PARAMETERS
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
  INTEGER                           , INTENT(IN)    :: NSNOW  !maximum no. of snow layers        
  INTEGER                           , INTENT(IN)    :: NSOIL  !number of soil layers
  INTEGER                           , INTENT(IN)    :: ISNOW  !actual no. of snow layers
  REAL                              , INTENT(IN)    :: DT     !time step [sec]
  REAL                              , INTENT(IN)    :: QSNOW  !snowfall on the ground (mm/s)
  REAL                              , INTENT(IN)    :: RHOAIR !density air (kg/m3)
  REAL                              , INTENT(IN)    :: EAIR   !vapor pressure air (pa)
  REAL                              , INTENT(IN)    :: SFCPRS !pressure (pa)
  REAL                              , INTENT(IN)    :: QAIR   !specific humidity (kg/kg)
  REAL                              , INTENT(IN)    :: SFCTMP !air temperature (k)
  REAL                              , INTENT(IN)    :: LWDN   !downward longwave radiation (w/m2)
  REAL                              , INTENT(IN)    :: UU     !wind speed in e-w dir (m/s)
  REAL                              , INTENT(IN)    :: VV     !wind speed in n-s dir (m/s)
  REAL   , DIMENSION(       1:    2), INTENT(IN)    :: SOLAD  !incoming direct solar rad. (w/m2)
  REAL   , DIMENSION(       1:    2), INTENT(IN)    :: SOLAI  !incoming diffuse solar rad. (w/m2)
  REAL                              , INTENT(IN)    :: COSZ   !cosine solar zenith angle (0-1)
  REAL                              , INTENT(IN)    :: ZREF   !reference height (m)
  REAL                              , INTENT(IN)    :: TBOT   !bottom condition for soil temp. (k) 
  REAL                              , INTENT(IN)    :: ZBOT   !depth for TBOT [m]
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: ZSNSO  !layer-bottom depth from snow surf [m]
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: DZSNSO !depth of snow & soil layer-bottom [m]

! input & output
  REAL                              , INTENT(INOUT) :: TG     !ground temperature (k)
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow/soil temperature [k]
  REAL                              , INTENT(INOUT) :: SNOWH  !snow height [m]
  REAL                              , INTENT(INOUT) :: SNEQV  !snow mass (mm)
  REAL                              , INTENT(INOUT) :: SNEQVO !snow mass at last time step (mm)
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !liquid soil moisture [m3/m3]
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow ice mass (kg/m2)
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow liq mass (kg/m2)
  REAL                              , INTENT(INOUT) :: ALBOLD !snow albedo at last time step(CLASS type)
  REAL                              , INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL                              , INTENT(INOUT) :: CH     !sensible heat exchange coefficient
  REAL                              , INTENT(INOUT) :: TAUSS  !snow aging factor
  REAL                              , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer

! outputs
  INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT)   :: IMELT  !phase change index [1-melt; 2-freeze]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: SNICEV !partial volume ice [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: SNLIQV !partial volume liq. water [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: EPORE  !effective porosity [m3/m3]
  REAL                              , INTENT(OUT)   :: QMELT  !snowmelt [mm/s]
  REAL                              , INTENT(OUT)   :: PONDING!pounding at ground [mm]
  REAL                              , INTENT(OUT)   :: SAG    !solar rad. absorbed by ground (w/m2)
  REAL                              , INTENT(OUT)   :: FSA    !tot. absorbed solar radiation (w/m2)
  REAL                              , INTENT(OUT)   :: FSR    !tot. reflected solar radiation (w/m2)
  REAL                              , INTENT(OUT)   :: FIRA   !total net LW. rad (w/m2)   [+ to atm]
  REAL                              , INTENT(OUT)   :: FSH    !total sensible heat (w/m2) [+ to atm]
  REAL                              , INTENT(OUT)   :: FGEV   !ground evaporation (w/m2)  [+ to atm]
  REAL                              , INTENT(OUT)   :: TRAD   !radiative temperature (k)
  REAL                              , INTENT(OUT)   :: T2M    !2 m height air temperature (k)
  REAL                              , INTENT(OUT)   :: SSOIL  !ground heat flux (w/m2)   [+ to soil]
  REAL                              , INTENT(OUT)   :: LATHEA !latent heat vap./sublimation (j/kg)
  REAL                              , INTENT(OUT)   :: Q2E
  REAL                              , INTENT(OUT)   :: EMISSI
  REAL                              , INTENT(OUT)   :: CH2B   !sensible heat conductance, canopy air to ZLVL air (m/s)


! local
  REAL                                              :: UR     !wind speed at height ZLVL (m/s)
  REAL                                              :: ZLVL   !reference height (m)
  REAL                                              :: RSURF  !ground surface resistance (s/m)
  REAL                                              :: ZPD    !zero plane displacement (m)
  REAL                                              :: Z0MG   !z0 momentum, ground (m)
  REAL                                              :: EMG    !ground emissivity
  REAL                                              :: FIRE   !emitted IR (w/m2)
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: FACT   !temporary used in phase change
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: DF     !thermal conductivity [w/m/k]
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: HCPCT  !heat capacity [j/m3/k]
  REAL                                              :: GAMMA  !psychrometric constant (pa/k)
  REAL                                              :: RHSUR  !raltive humidity in surface soil/snow air space (-)

! ---------------------------------------------------------------------------------------------------

! wind speed at reference height: ur >= 1

    UR = MAX( SQRT(UU**2.+VV**2.), 1. )

! roughness length and displacement height

     Z0MG = Z0SNO
     ZPD  = SNOWH

     ZLVL = ZPD + ZREF

! Thermal properties of soil, snow, lake, and frozen soil

  CALL THERMOPROP_GLACIER (NSOIL   ,NSNOW   ,ISNOW   ,DZSNSO  ,          & !in
                           DT      ,SNOWH   ,SNICE   ,SNLIQ   ,          & !in
                           DF      ,HCPCT   ,SNICEV  ,SNLIQV  ,EPORE   , & !out
                           FACT    )                                       !out

! Solar radiation: absorbed & reflected by the ground

  CALL  RADIATION_GLACIER (DT      ,TG      ,SNEQVO  ,SNEQV   ,COSZ    , & !in
                           QSNOW   ,SOLAD   ,SOLAI   ,                   & !in
                           ALBOLD  ,TAUSS   ,                            & !inout
                           SAG     ,FSR     ,FSA)                          !out

! vegetation and ground emissivity

     EMG = 0.98

! soil surface resistance for ground evap.

     RHSUR = 1.0
     RSURF = 1.0

! set psychrometric constant

     LATHEA = HSUB
     GAMMA = CPAIR*SFCPRS/(0.622*LATHEA)

! Surface temperatures of the ground and energy fluxes

    CALL GLACIER_FLUX (NSOIL   ,NSNOW   ,EMG     ,ISNOW   ,DF      ,DZSNSO  ,Z0MG    , & !in
                       ZLVL    ,ZPD     ,QAIR    ,SFCTMP  ,RHOAIR  ,SFCPRS  , & !in
		       UR      ,GAMMA   ,RSURF   ,LWDN    ,RHSUR   ,SMC     , & !in
		       EAIR    ,STC     ,SAG     ,SNOWH   ,LATHEA  ,SH2O    , & !in
		       CM      ,CH      ,TG      ,QSFC    ,          & !inout
		       FIRA    ,FSH     ,FGEV    ,SSOIL   ,          & !out
		       T2M     ,Q2E     ,CH2B)                         !out 

!energy balance at surface: SAG=(IRB+SHB+EVB+GHB)

    FIRE = LWDN + FIRA

    IF(FIRE <=0.) call wrf_error_fatal("STOP in Noah-MP: emitted longwave <0")

    ! Compute a net emissivity
    EMISSI = EMG

    ! When we're computing a TRAD, subtract from the emitted IR the
    ! reflected portion of the incoming LWDN, so we're just
    ! considering the IR originating in the canopy/ground system.
    
    TRAD = ( ( FIRE - (1-EMISSI)*LWDN ) / (EMISSI*SB) ) ** 0.25

! 3L snow & 4L soil temperatures

    CALL TSNOSOI_GLACIER (NSOIL   ,NSNOW   ,ISNOW   ,DT      ,TBOT    , & !in
                          SSOIL   ,SNOWH   ,ZBOT    ,ZSNSO   ,DF      , & !in
		          HCPCT   ,                                     & !in
                          STC     )                                       !inout

! adjusting snow surface temperature
     IF(OPT_STC == 2) THEN
      IF (SNOWH > 0.05 .AND. TG > TFRZ) TG = TFRZ
     END IF

! Energy released or consumed by snow & ice

 CALL PHASECHANGE_GLACIER (NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & !in
                           DZSNSO  ,                                     & !in
                           STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & !inout
                           SMC     ,SH2O    ,                            & !inout
                           QMELT   ,IMELT   ,PONDING )                     !out


  END SUBROUTINE ENERGY_GLACIER
! ==================================================================================================
  SUBROUTINE THERMOPROP_GLACIER (NSOIL   ,NSNOW   ,ISNOW   ,DZSNSO  , & !in
                                 DT      ,SNOWH   ,SNICE   ,SNLIQ   , & !in
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
  REAL                           , INTENT(IN)  :: DT      !time step [s]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)  :: SNICE   !snow ice mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)  :: SNLIQ   !snow liq mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DZSNSO  !thickness of snow/soil layers [m]
  REAL                           , INTENT(IN)  :: SNOWH   !snow height [m]

! outputs
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: DF      !thermal conductivity [w/m/k]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: HCPCT   !heat capacity [j/m3/k]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNICEV  !partial volume of ice [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNLIQV  !partial volume of liquid water [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: EPORE   !effective porosity [m3/m3]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: FACT    !computing energy for phase change
! --------------------------------------------------------------------------------------------------
! locals

  INTEGER :: IZ, IZ2
  REAL, DIMENSION(-NSNOW+1:    0)              :: CVSNO   !volumetric specific heat (j/m3/k)
  REAL, DIMENSION(-NSNOW+1:    0)              :: TKSNO   !snow thermal conductivity (j/m3/k)
  REAL                                         :: ZMID    !mid-point soil depth
! --------------------------------------------------------------------------------------------------

! compute snow thermal conductivity and heat capacity

    CALL CSNOW_GLACIER (ISNOW   ,NSNOW   ,NSOIL   ,SNICE   ,SNLIQ   ,DZSNSO  , & !in
                        TKSNO   ,CVSNO   ,SNICEV  ,SNLIQV  ,EPORE   )   !out

    DO IZ = ISNOW+1, 0
      DF   (IZ) = TKSNO(IZ)
      HCPCT(IZ) = CVSNO(IZ)
    END DO

! compute soil thermal properties (using Noah glacial ice approximations)

    DO  IZ = 1, NSOIL
       ZMID      = 0.5 * (DZSNSO(IZ))
       DO IZ2 = 1, IZ-1
         ZMID = ZMID + DZSNSO(IZ2)
       END DO
       HCPCT(IZ) = 1.E6 * ( 0.8194 + 0.1309*ZMID )
       DF(IZ)    = 0.32333 + ( 0.10073 * ZMID )
    END DO
       
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


  END SUBROUTINE THERMOPROP_GLACIER
! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE CSNOW_GLACIER (ISNOW   ,NSNOW   ,NSOIL   ,SNICE   ,SNLIQ   ,DZSNSO  , & !in
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

  END SUBROUTINE CSNOW_GLACIER
!===================================================================================================
  SUBROUTINE RADIATION_GLACIER (DT      ,TG      ,SNEQVO  ,SNEQV   ,COSZ    , & !in
                                QSNOW   ,SOLAD   ,SOLAI   ,                   & !in
                                ALBOLD  ,TAUSS   ,                            & !inout
                                SAG     ,FSR     ,FSA)                          !out
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  REAL, INTENT(IN)                     :: DT     !time step [s]
  REAL, INTENT(IN)                     :: TG     !ground temperature (k)
  REAL, INTENT(IN)                     :: SNEQVO !snow mass at last time step(mm)
  REAL, INTENT(IN)                     :: SNEQV  !snow mass (mm)
  REAL, INTENT(IN)                     :: COSZ   !cosine solar zenith angle (0-1)
  REAL, INTENT(IN)                     :: QSNOW  !snowfall (mm/s)
  REAL, DIMENSION(1:2)    , INTENT(IN) :: SOLAD  !incoming direct solar radiation (w/m2)
  REAL, DIMENSION(1:2)    , INTENT(IN) :: SOLAI  !incoming diffuse solar radiation (w/m2)

! inout
  REAL,                  INTENT(INOUT) :: ALBOLD !snow albedo at last time step (CLASS type)
  REAL,                  INTENT(INOUT) :: TAUSS  !non-dimensional snow age

! output
  REAL, INTENT(OUT)                    :: SAG    !solar radiation absorbed by ground (w/m2)
  REAL, INTENT(OUT)                    :: FSR    !total reflected solar radiation (w/m2)
  REAL, INTENT(OUT)                    :: FSA    !total absorbed solar radiation (w/m2)

! local
  INTEGER                              :: IB     !number of radiation bands
  INTEGER                              :: NBAND  !number of radiation bands
  REAL                                 :: FAGE   !snow age function (0 - new snow)
  REAL, DIMENSION(1:2)                 :: ALBSND !snow albedo (direct)
  REAL, DIMENSION(1:2)                 :: ALBSNI !snow albedo (diffuse)
  REAL                                 :: ALB    !current CLASS albedo
  REAL                                 :: ABS    !temporary absorbed rad
  REAL                                 :: REF    !temporary reflected rad
  REAL                                 :: FSNO   !snow-cover fraction, = 1 if any snow
  REAL, DIMENSION(1:2)                 :: ALBICE !albedo land ice: 1=vis, 2=nir

  REAL,PARAMETER :: MPE = 1.E-6

! --------------------------------------------------------------------------------------------------

  NBAND = 2
  ALBSND = 0.0
  ALBSNI = 0.0
  ALBICE(1) = 0.80    !albedo land ice: 1=vis, 2=nir
  ALBICE(2) = 0.55

! snow age

  CALL SNOW_AGE_GLACIER (DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)

! snow albedos: age even when sun is not present

  IF(OPT_ALB == 1) &
     CALL SNOWALB_BATS_GLACIER (NBAND,COSZ,FAGE,ALBSND,ALBSNI)
  IF(OPT_ALB == 2) THEN
     CALL SNOWALB_CLASS_GLACIER(NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI)
     ALBOLD = ALB
  END IF

! zero summed solar fluxes

   SAG = 0.
   FSA = 0.
   FSR = 0.
   
   FSNO = 0.0
   IF(SNEQV > 0.0) FSNO = 1.0

! loop over nband wavebands

  DO IB = 1, NBAND

    ALBSND(IB) = ALBICE(IB)*(1.-FSNO) + ALBSND(IB)*FSNO
    ALBSNI(IB) = ALBICE(IB)*(1.-FSNO) + ALBSNI(IB)*FSNO

! solar radiation absorbed by ground surface

    ABS = SOLAD(IB)*(1.-ALBSND(IB)) + SOLAI(IB)*(1.-ALBSNI(IB))
    SAG = SAG + ABS
    FSA = FSA + ABS
    
    REF = SOLAD(IB)*ALBSND(IB) + SOLAI(IB)*ALBSNI(IB)
    FSR = FSR + REF
    
  END DO

  END SUBROUTINE RADIATION_GLACIER
! ==================================================================================================
  SUBROUTINE SNOW_AGE_GLACIER (DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)
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

! inout
  REAL,  INTENT(INOUT) :: TAUSS  !non-dimensional snow age

!output
   REAL, INTENT(OUT) :: FAGE     !snow age

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
!          TAUSS = 0.
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

  END SUBROUTINE SNOW_AGE_GLACIER
! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE SNOWALB_BATS_GLACIER (NBAND,COSZ,FAGE,ALBSND,ALBSNI)
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

  INTEGER,INTENT(IN) :: NBAND  !number of waveband classes

  REAL,INTENT(IN) :: COSZ    !cosine solar zenith angle
  REAL,INTENT(IN) :: FAGE    !snow age correction

! output

  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSND !snow albedo for direct(1=vis, 2=nir)
  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSNI !snow albedo for diffuse
! ---------------------------------------------------------------------------------------------

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

  END SUBROUTINE SNOWALB_BATS_GLACIER
! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE SNOWALB_CLASS_GLACIER (NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI)
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

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

  END SUBROUTINE SNOWALB_CLASS_GLACIER
! ==================================================================================================
  SUBROUTINE GLACIER_FLUX (NSOIL   ,NSNOW   ,EMG     ,ISNOW   ,DF      ,DZSNSO  ,Z0M     , & !in
                           ZLVL    ,ZPD     ,QAIR    ,SFCTMP  ,RHOAIR  ,SFCPRS  , & !in
			   UR      ,GAMMA   ,RSURF   ,LWDN    ,RHSUR   ,SMC     , & !in
			   EAIR    ,STC     ,SAG     ,SNOWH   ,LATHEA  ,SH2O    , & !in
                           CM      ,CH      ,TGB     ,QSFC    ,          & !inout
                           IRB     ,SHB     ,EVB     ,GHB     ,          & !out
                           T2MB    ,Q2B     ,EHB2)                         !out 

! --------------------------------------------------------------------------------------------------
! use newton-raphson iteration to solve ground (tg) temperature
! that balances the surface energy budgets for glacier.

! bare soil:
! -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0
! ----------------------------------------------------------------------
!  USE MODULE_MODEL_CONSTANTS
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  INTEGER, INTENT(IN)                         :: NSNOW  !maximum no. of snow layers        
  INTEGER, INTENT(IN)                         :: NSOIL  !number of soil layers
  REAL,                            INTENT(IN) :: EMG    !ground emissivity
  INTEGER,                         INTENT(IN) :: ISNOW  !actual no. of snow layers
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DF     !thermal conductivity of snow/soil (w/m/k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !thickness of snow/soil layers (m)
  REAL,                            INTENT(IN) :: Z0M    !roughness length, momentum, ground (m)
  REAL,                            INTENT(IN) :: ZLVL   !reference height (m)
  REAL,                            INTENT(IN) :: ZPD    !zero plane displacement (m)
  REAL,                            INTENT(IN) :: QAIR   !specific humidity at height zlvl (kg/kg)
  REAL,                            INTENT(IN) :: SFCTMP !air temperature at reference height (k)
  REAL,                            INTENT(IN) :: RHOAIR !density air (kg/m3)
  REAL,                            INTENT(IN) :: SFCPRS !density air (kg/m3)
  REAL,                            INTENT(IN) :: UR     !wind speed at height zlvl (m/s)
  REAL,                            INTENT(IN) :: GAMMA  !psychrometric constant (pa/k)
  REAL,                            INTENT(IN) :: RSURF  !ground surface resistance (s/m)
  REAL,                            INTENT(IN) :: LWDN   !atmospheric longwave radiation (w/m2)
  REAL,                            INTENT(IN) :: RHSUR  !raltive humidity in surface soil/snow air space (-)
  REAL,                            INTENT(IN) :: EAIR   !vapor pressure air at height (pa)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !soil/snow temperature (k)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SMC    !soil moisture
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SH2O   !soil liquid water
  REAL,                            INTENT(IN) :: SAG    !solar radiation absorbed by ground (w/m2)
  REAL,                            INTENT(IN) :: SNOWH  !actual snow depth [m]
  REAL,                            INTENT(IN) :: LATHEA !latent heat of vaporization/subli (j/kg)

! input/output
  REAL,                         INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL,                         INTENT(INOUT) :: CH     !sensible heat exchange coefficient
  REAL,                         INTENT(INOUT) :: TGB    !ground temperature (k)
  REAL,                         INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer

! output
! -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0
  REAL,                           INTENT(OUT) :: IRB    !net longwave rad (w/m2)   [+ to atm]
  REAL,                           INTENT(OUT) :: SHB    !sensible heat flux (w/m2) [+ to atm]
  REAL,                           INTENT(OUT) :: EVB    !latent heat flux (w/m2)   [+ to atm]
  REAL,                           INTENT(OUT) :: GHB    !ground heat flux (w/m2)  [+ to soil]
  REAL,                           INTENT(OUT) :: T2MB   !2 m height air temperature (k)
  REAL,                           INTENT(OUT) :: Q2B    !bare ground heat conductance
  REAL,                           INTENT(OUT) :: EHB2   !sensible heat conductance for diagnostics


! local variables 
  INTEGER :: NITERB  !number of iterations for surface temperature
  REAL    :: MPE     !prevents overflow error if division by zero
  REAL    :: DTG        !change in tg, last iteration (k)
  INTEGER :: MOZSGN  !number of times MOZ changes sign
  REAL    :: MOZOLD     !Monin-Obukhov stability parameter from prior iteration
  REAL    :: FM2          !Monin-Obukhov momentum adjustment at 2m
  REAL    :: FH2          !Monin-Obukhov heat adjustment at 2m
  REAL    :: CH2          !Surface exchange at 2m
  REAL    :: H          !temporary sensible heat flux (w/m2)
  REAL    :: FV         !friction velocity (m/s)
  REAL    :: CIR        !coefficients for ir as function of ts**4
  REAL    :: CGH        !coefficients for st as function of ts
  REAL    :: CSH        !coefficients for sh as function of ts
  REAL    :: CEV        !coefficients for ev as function of esat[ts]
  REAL    :: CQ2B       !
  INTEGER :: ITER    !iteration index
  REAL    :: Z0H        !roughness length, sensible heat, ground (m)
  REAL    :: MOZ        !Monin-Obukhov stability parameter
  REAL    :: FM         !momentum stability correction, weighted by prior iters
  REAL    :: FH         !sen heat stability correction, weighted by prior iters
  REAL    :: RAMB       !aerodynamic resistance for momentum (s/m)
  REAL    :: RAHB       !aerodynamic resistance for sensible heat (s/m)
  REAL    :: RAWB       !aerodynamic resistance for water vapor (s/m)
  REAL    :: ESTG       !saturation vapor pressure at tg (pa)
  REAL    :: DESTG      !d(es)/dt at tg (pa/K)
  REAL    :: ESATW      !es for water
  REAL    :: ESATI      !es for ice
  REAL    :: DSATW      !d(es)/dt at tg (pa/K) for water
  REAL    :: DSATI      !d(es)/dt at tg (pa/K) for ice
  REAL    :: A          !temporary calculation
  REAL    :: B          !temporary calculation
  REAL    :: T, TDC     !Kelvin to degree Celsius with limit -50 to +50
  REAL, DIMENSION(       1:NSOIL) :: SICE   !soil ice

  TDC(T)   = MIN( 50., MAX(-50.,(T-TFRZ)) )

! -----------------------------------------------------------------
! initialization variables that do not depend on stability iteration
! -----------------------------------------------------------------
        NITERB = 5
        MPE    = 1E-6
        DTG    = 0.
        MOZSGN = 0
        MOZOLD = 0.
        H      = 0.
        FV     = 0.1

        CIR = EMG*SB
        CGH = 2.*DF(ISNOW+1)/DZSNSO(ISNOW+1)

! -----------------------------------------------------------------
      loop3: DO ITER = 1, NITERB  ! begin stability iteration

        Z0H = Z0M 

!       For now, only allow SFCDIF1 until others can be fixed

        CALL SFCDIF1_GLACIER(ITER   ,ZLVL   ,ZPD    ,Z0H    ,Z0M    , & !in
                     QAIR   ,SFCTMP ,H      ,RHOAIR ,MPE    ,UR     , & !in
       &             MOZ    ,MOZSGN ,FM     ,FH     ,FM2    ,FH2    , & !inout
       &             FV     ,CM     ,CH     ,CH2)                       !out

        RAMB = MAX(1.,1./(CM*UR))
        RAHB = MAX(1.,1./(CH*UR))
        RAWB = RAHB

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
	IF(SNOWH > 0.0 .OR. OPT_GLA == 1) THEN
          CEV = RHOAIR*CPAIR/GAMMA/(RSURF+RAWB)
	ELSE
	  CEV = 0.0   ! don't allow any sublimation of glacier in opt_gla=2
	END IF

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
        QSFC = 0.622*(ESTG*RHSUR)/(SFCPRS-0.378*(ESTG*RHSUR))

     END DO loop3 ! end stability iteration
! -----------------------------------------------------------------

! if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.

     SICE = SMC - SH2O
     IF(OPT_STC == 1 .OR. OPT_STC ==3) THEN
     IF ((MAXVAL(SICE) > 0.0 .OR. SNOWH > 0.0) .AND. TGB > TFRZ .AND. OPT_GLA == 1) THEN
          TGB = TFRZ
          T = TDC(TGB)                              ! MB: recalculate ESTG
          CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
          ESTG  = ESATI
          QSFC = 0.622*(ESTG*RHSUR)/(SFCPRS-0.378*(ESTG*RHSUR))
          IRB = CIR * TGB**4 - EMG*LWDN
          SHB = CSH * (TGB        - SFCTMP)
          EVB = CEV * (ESTG*RHSUR - EAIR )          !ESTG reevaluate ?
          GHB = SAG - (IRB+SHB+EVB)
     END IF
     END IF

! 2m air temperature
     EHB2  = FV*VKC/(LOG((2.+Z0H)/Z0H)-FH2)
     CQ2B  = EHB2
     IF (EHB2.lt.1.E-5 ) THEN
       T2MB  = TGB
       Q2B   = QSFC
     ELSE
       T2MB  = TGB - SHB/(RHOAIR*CPAIR) * 1./EHB2
       Q2B   = QSFC - EVB/(LATHEA*RHOAIR)*(1./CQ2B + RSURF)
     ENDIF

! update CH 
     CH = 1./RAHB

  END SUBROUTINE GLACIER_FLUX
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

  SUBROUTINE SFCDIF1_GLACIER(ITER   ,ZLVL   ,ZPD    ,Z0H    ,Z0M    , & !in
                     QAIR   ,SFCTMP ,H      ,RHOAIR ,MPE    ,UR     , & !in
       &             MOZ    ,MOZSGN ,FM     ,FH     ,FM2    ,FH2    , & !inout
       &             FV     ,CM     ,CH     ,CH2     )                  !out
! -------------------------------------------------------------------------------------------------
! computing surface drag coefficient CM for momentum and CH for heat
! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE
! -------------------------------------------------------------------------------------------------
! inputs
    INTEGER,              INTENT(IN) :: ITER   !iteration index
    REAL,                 INTENT(IN) :: ZLVL   !reference height  (m)
    REAL,                 INTENT(IN) :: ZPD    !zero plane displacement (m)
    REAL,                 INTENT(IN) :: Z0H    !roughness length, sensible heat, ground (m)
    REAL,                 INTENT(IN) :: Z0M    !roughness length, momentum, ground (m)
    REAL,                 INTENT(IN) :: QAIR   !specific humidity at reference height (kg/kg)
    REAL,                 INTENT(IN) :: SFCTMP !temperature at reference height (k)
    REAL,                 INTENT(IN) :: H      !sensible heat flux (w/m2) [+ to atm]
    REAL,                 INTENT(IN) :: RHOAIR !density air (kg/m**3)
    REAL,                 INTENT(IN) :: MPE    !prevents overflow error if division by zero
    REAL,                 INTENT(IN) :: UR     !wind speed (m/s)

! in & out
    REAL,              INTENT(INOUT) :: MOZ    !Monin-Obukhov stability (z/L)
    INTEGER,           INTENT(INOUT) :: MOZSGN !number of times moz changes sign
    REAL,              INTENT(INOUT) :: FM     !momentum stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FH     !sen heat stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FM2    !sen heat stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FH2    !sen heat stability correction, weighted by prior iters

! outputs
    REAL,                INTENT(OUT) :: FV     !friction velocity (m/s)
    REAL,                INTENT(OUT) :: CM     !drag coefficient for momentum
    REAL,                INTENT(OUT) :: CH     !drag coefficient for heat
    REAL,                INTENT(OUT) :: CH2    !drag coefficient for heat

! locals
    REAL    :: MOZOLD                   !Monin-Obukhov stability parameter from prior iteration
    REAL    :: TMPCM                    !temporary calculation for CM
    REAL    :: TMPCH                    !temporary calculation for CH
    REAL    :: MOL                      !Monin-Obukhov length (m)
    REAL    :: TVIR                     !temporary virtual temperature (k)
    REAL    :: TMP1,TMP2,TMP3           !temporary calculation
    REAL    :: FMNEW                    !stability correction factor, momentum, for current moz
    REAL    :: FHNEW                    !stability correction factor, sen heat, for current moz
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
       write(*,*) 'critical glacier problem: ZLVL <= ZPD; model stops', zlvl, zpd
       call wrf_error_fatal("STOP in Noah-MP glacier")
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

  END SUBROUTINE SFCDIF1_GLACIER
! ==================================================================================================
  SUBROUTINE TSNOSOI_GLACIER (NSOIL   ,NSNOW   ,ISNOW   ,DT      ,TBOT    , & !in
                              SSOIL   ,SNOWH   ,ZBOT    ,ZSNSO   ,DF      , & !in
			      HCPCT   ,                                     & !in
                              STC     )                                       !inout
! --------------------------------------------------------------------------------------------------
! Compute snow (up to 3L) and soil (4L) temperature. Note that snow temperatures
! during melting season may exceed melting point (TFRZ) but later in PHASECHANGE
! subroutine the snow temperatures are reset to TFRZ for melting snow.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
!input

    INTEGER,                         INTENT(IN)  :: NSOIL  !no of soil layers (4)
    INTEGER,                         INTENT(IN)  :: NSNOW  !maximum no of snow layers (3)
    INTEGER,                         INTENT(IN)  :: ISNOW  !actual no of snow layers

    REAL,                            INTENT(IN)  :: DT     !time step (s)
    REAL,                            INTENT(IN)  :: TBOT   !
    REAL,                            INTENT(IN)  :: SSOIL  !ground heat flux (w/m2)
    REAL,                            INTENT(IN)  :: SNOWH  !snow depth (m)
    REAL,                            INTENT(IN)  :: ZBOT   !from soil surface (m)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: ZSNSO  !layer-bot. depth from snow surf.(m)
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

! ----------------------------------------------------------------------

! prescribe solar penetration into ice/snow

    PHI(ISNOW+1:NSOIL) = 0.

! adjust ZBOT from soil surface to ZBOTSNO from snow surface

    ZBOTSNO = ZBOT - SNOWH    !from snow surface

! compute ice temperatures

      CALL HRT_GLACIER   (NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , &
                          STC       ,TBOT      ,ZBOTSNO   ,DF        , &
                          HCPCT     ,SSOIL     ,PHI       ,            &
                          AI        ,BI        ,CI        ,RHSTS     , &
                          EFLXB     )

      CALL HSTEP_GLACIER (NSNOW     ,NSOIL     ,ISNOW     ,DT        , &
                          AI        ,BI        ,CI        ,RHSTS     , &
                          STC       ) 

  END SUBROUTINE TSNOSOI_GLACIER
! ==================================================================================================
! ----------------------------------------------------------------------
  SUBROUTINE HRT_GLACIER (NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , & !in
                          STC       ,TBOT      ,ZBOT      ,DF        , & !in
                          HCPCT     ,SSOIL     ,PHI       ,            & !in
                          AI        ,BI        ,CI        ,RHSTS     , & !out
                          BOTFLX    )                                    !out
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
           IF (OPT_STC == 1 .OR. OPT_STC == 3) THEN
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

  END SUBROUTINE HRT_GLACIER
! ==================================================================================================
! ----------------------------------------------------------------------
  SUBROUTINE HSTEP_GLACIER (NSNOW     ,NSOIL     ,ISNOW     ,DT        ,  & !in
                            AI        ,BI        ,CI        ,RHSTS     ,  & !inout
                            STC       )                                     !inout
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
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: AI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: BI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: CI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: RHSTS

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

    CALL ROSR12_GLACIER (CI,AI,BI,CIIN,RHSTSIN,RHSTS,ISNOW+1,NSOIL,NSNOW)

! update snow & soil temperature

    DO K = ISNOW+1,NSOIL
       STC (K) = STC (K) + CI (K)
    END DO

  END SUBROUTINE HSTEP_GLACIER
! ==================================================================================================
  SUBROUTINE ROSR12_GLACIER (P,A,B,C,D,DELTA,NTOP,NSOIL,NSNOW)
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
  END SUBROUTINE ROSR12_GLACIER
! ----------------------------------------------------------------------
! ==================================================================================================
  SUBROUTINE PHASECHANGE_GLACIER (NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & !in
                                  DZSNSO  ,                                     & !in
                                  STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & !inout
                                  SMC     ,SH2O    ,                            & !inout
                                  QMELT   ,IMELT   ,PONDING )                     !out
! ----------------------------------------------------------------------
! melting/freezing of snow water and soil water
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! inputs

  INTEGER, INTENT(IN)                             :: NSNOW  !maximum no. of snow layers [=3]
  INTEGER, INTENT(IN)                             :: NSOIL  !No. of soil layers [=4]
  INTEGER, INTENT(IN)                             :: ISNOW  !actual no. of snow layers [<=3]
  REAL, INTENT(IN)                                :: DT     !land model time step (sec)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: FACT   !temporary
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: DZSNSO !snow/soil layer thickness [m]

! inputs/outputs

  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT)  :: STC    !snow/soil layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT)  :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT)  :: SNLIQ  !snow layer liquid water [mm]
  REAL, INTENT(INOUT)                             :: SNEQV
  REAL, INTENT(INOUT)                             :: SNOWH
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SH2O   !soil liquid water [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SMC    !total soil water [m3/m3]

! outputs
  REAL,                               INTENT(OUT) :: QMELT  !snowmelt rate [mm/s]
  INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: IMELT  !phase change index
  REAL,                               INTENT(OUT) :: PONDING!snowmelt when snow has no layer [mm]

! local

  INTEGER                         :: J,K         !do loop index
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: HM        !energy residual [w/m2]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: XM        !melting or freezing water [kg/m2]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WMASS0
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WICE0 
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WLIQ0 
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: MICE      !soil/snow ice mass [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: MLIQ      !soil/snow liquid water mass [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: HEATR     !energy residual or loss after melting/freezing
  REAL                            :: TEMP1     !temporary variables [kg/m2]
  REAL                            :: PROPOR
  REAL                            :: XMF       !total latent heat of phase change

! ----------------------------------------------------------------------
! Initialization

    QMELT   = 0.
    PONDING = 0.
    XMF     = 0.

    DO J = ISNOW+1,0           ! all snow layers
         MICE(J) = SNICE(J)
         MLIQ(J) = SNLIQ(J)
    END DO

    DO J = ISNOW+1,0           ! all snow layers; do ice later
         IMELT(J)    = 0
         HM(J)       = 0.
         XM(J)       = 0.
         WICE0(J)    = MICE(J)
         WLIQ0(J)    = MLIQ(J)
         WMASS0(J)   = MICE(J) + MLIQ(J)
    ENDDO
    
    DO J = ISNOW+1,0
         IF (MICE(J) > 0. .AND. STC(J) >= TFRZ) THEN  ! melting 
             IMELT(J) = 1
         ENDIF
         IF (MLIQ(J) > 0. .AND. STC(J)  < TFRZ) THEN  ! freezing 
             IMELT(J) = 2
         ENDIF

    ENDDO

! Calculate the energy surplus and loss for melting and freezing

    DO J = ISNOW+1,0
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

! The rate of melting and freezing for snow without a layer, opt_gla==1 treated below

IF (OPT_GLA == 2) THEN 

    IF (ISNOW == 0 .AND. SNEQV > 0. .AND. STC(1) >= TFRZ) THEN  
        HM(1)    = (STC(1)-TFRZ)/FACT(1)             ! available heat
        STC(1)   = TFRZ                              ! set T to freezing
        XM(1)    = HM(1)*DT/HFUS                     ! total snow melt possible       

        TEMP1  = SNEQV
        SNEQV  = MAX(0.,TEMP1-XM(1))                 ! snow remaining
        PROPOR = SNEQV/TEMP1                         ! fraction melted
        SNOWH  = MAX(0.,PROPOR * SNOWH)              ! new snow height
        HEATR(1)  = HM(1) - HFUS*(TEMP1-SNEQV)/DT    ! excess heat
        IF (HEATR(1) > 0.) THEN
              XM(1)  = HEATR(1)*DT/HFUS             
              STC(1) = STC(1) + FACT(1)*HEATR(1)     ! re-heat ice
        ELSE
              XM(1) = 0.                             ! heat used up
              HM(1) = 0.
        ENDIF
        QMELT   = MAX(0.,(TEMP1-SNEQV))/DT           ! melted snow rate
        XMF     = HFUS*QMELT                         ! melted snow energy
        PONDING = TEMP1-SNEQV                        ! melt water
    ENDIF

END IF  ! OPT_GLA == 2

! The rate of melting and freezing for snow

    DO J = ISNOW+1,0
      IF (IMELT(J) > 0 .AND. ABS(HM(J)) > 0.) THEN

         HEATR(J) = 0.
         IF (XM(J) > 0.) THEN                            
            MICE(J) = MAX(0., WICE0(J)-XM(J))
            HEATR(J) = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ELSE IF (XM(J) < 0.) THEN                      
            MICE(J) = MIN(WMASS0(J), WICE0(J)-XM(J))  
            HEATR(J) = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ENDIF

         MLIQ(J) = MAX(0.,WMASS0(J)-MICE(J))

         IF (ABS(HEATR(J)) > 0.) THEN
            STC(J) = STC(J) + FACT(J)*HEATR(J)
            IF (MLIQ(J)*MICE(J)>0.) STC(J) = TFRZ
         ENDIF

         QMELT = QMELT + MAX(0.,(WICE0(J)-MICE(J)))/DT

      ENDIF
    ENDDO

IF (OPT_GLA == 1) THEN     ! operate on the ice layers

    DO J = 1, NSOIL            ! all soil layers
         MLIQ(J) =  SH2O(J)            * DZSNSO(J) * 1000.
         MICE(J) = (SMC(J) - SH2O(J))  * DZSNSO(J) * 1000.
    END DO

    DO J = 1,NSOIL       ! all layers
         IMELT(J)    = 0
         HM(J)       = 0.
         XM(J)       = 0.
         WICE0(J)    = MICE(J)
         WLIQ0(J)    = MLIQ(J)
         WMASS0(J)   = MICE(J) + MLIQ(J)
    ENDDO
    
    DO J = 1,NSOIL
         IF (MICE(J) > 0. .AND. STC(J) >= TFRZ) THEN  ! melting 
             IMELT(J) = 1
         ENDIF
         IF (MLIQ(J) > 0. .AND. STC(J)  < TFRZ) THEN  ! freezing 
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

    DO J = 1,NSOIL
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
        HEATR(1)  = HM(1) - HFUS*(TEMP1-SNEQV)/DT  
        IF (HEATR(1) > 0.) THEN
              XM(1) = HEATR(1)*DT/HFUS             
              HM(1) = HEATR(1) 
	      IMELT(1) = 1                   
        ELSE
              XM(1) = 0.
              HM(1) = 0.
	      IMELT(1) = 0                   
        ENDIF
        QMELT   = MAX(0.,(TEMP1-SNEQV))/DT
        XMF     = HFUS*QMELT
        PONDING = TEMP1-SNEQV
    ENDIF

! The rate of melting and freezing for soil

    DO J = 1,NSOIL
      IF (IMELT(J) > 0 .AND. ABS(HM(J)) > 0.) THEN

         HEATR(J) = 0.
         IF (XM(J) > 0.) THEN                            
            MICE(J) = MAX(0., WICE0(J)-XM(J))
            HEATR(J) = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ELSE IF (XM(J) < 0.) THEN                      
            MICE(J) = MIN(WMASS0(J), WICE0(J)-XM(J))  
            HEATR(J) = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ENDIF

         MLIQ(J) = MAX(0.,WMASS0(J)-MICE(J))

         IF (ABS(HEATR(J)) > 0.) THEN
            STC(J) = STC(J) + FACT(J)*HEATR(J)
            IF (J <= 0) THEN                             ! snow
               IF (MLIQ(J)*MICE(J)>0.) STC(J) = TFRZ
            END IF
         ENDIF

         IF (J > 0) XMF = XMF + HFUS * (WICE0(J)-MICE(J))/DT

         IF (J < 1) THEN
            QMELT = QMELT + MAX(0.,(WICE0(J)-MICE(J)))/DT
         ENDIF
      ENDIF
    ENDDO
    HEATR = 0.0
    XM = 0.0

! Deal with residuals in ice/soil

! FIRST REMOVE EXCESS HEAT BY REDUCING TEMPERATURE OF LAYERS

    IF (ANY(STC(1:4) > TFRZ) .AND. ANY(STC(1:4) < TFRZ)) THEN
      DO J = 1,NSOIL
        IF ( STC(J) > TFRZ ) THEN                                       
	  HEATR(J) = (STC(J)-TFRZ)/FACT(J)
          DO K = 1,NSOIL
	    IF (J .NE. K .AND. STC(K) < TFRZ .AND. HEATR(J) > 0.1) THEN
	      HEATR(K) = (STC(K)-TFRZ)/FACT(K)
	      IF (ABS(HEATR(K)) > HEATR(J)) THEN  ! LAYER ABSORBS ALL
	        HEATR(K) = HEATR(K) + HEATR(J)
		STC(K) = TFRZ + HEATR(K)*FACT(K)
		HEATR(J) = 0.0
              ELSE
	        HEATR(J) = HEATR(J) + HEATR(K)
		HEATR(K) = 0.0
		STC(K) = TFRZ
              END IF
	    END IF
	  END DO
          STC(J) = TFRZ + HEATR(J)*FACT(J)
        END IF
      END DO
    END IF

! NOW REMOVE EXCESS COLD BY INCREASING TEMPERATURE OF LAYERS (MAY NOT BE NECESSARY WITH ABOVE LOOP)

    IF (ANY(STC(1:4) > TFRZ) .AND. ANY(STC(1:4) < TFRZ)) THEN
      DO J = 1,NSOIL
        IF ( STC(J) < TFRZ ) THEN                                       
	  HEATR(J) = (STC(J)-TFRZ)/FACT(J)
          DO K = 1,NSOIL
	    IF (J .NE. K .AND. STC(K) > TFRZ .AND. HEATR(J) < -0.1) THEN
	      HEATR(K) = (STC(K)-TFRZ)/FACT(K)
	      IF (HEATR(K) > ABS(HEATR(J))) THEN  ! LAYER ABSORBS ALL
	        HEATR(K) = HEATR(K) + HEATR(J)
		STC(K) = TFRZ + HEATR(K)*FACT(K)
		HEATR(J) = 0.0
              ELSE
	        HEATR(J) = HEATR(J) + HEATR(K)
		HEATR(K) = 0.0
		STC(K) = TFRZ
              END IF
	    END IF
	  END DO
          STC(J) = TFRZ + HEATR(J)*FACT(J)
        END IF
      END DO
    END IF

! NOW REMOVE EXCESS HEAT BY MELTING ICE

    IF (ANY(STC(1:4) > TFRZ) .AND. ANY(MICE(1:4) > 0.)) THEN
      DO J = 1,NSOIL
        IF ( STC(J) > TFRZ ) THEN                                       
	  HEATR(J) = (STC(J)-TFRZ)/FACT(J)
          XM(J) = HEATR(J)*DT/HFUS                           
          DO K = 1,NSOIL
	    IF (J .NE. K .AND. MICE(K) > 0. .AND. XM(J) > 0.1) THEN
	      IF (MICE(K) > XM(J)) THEN  ! LAYER ABSORBS ALL
	        MICE(K) = MICE(K) - XM(J)
		XMF = XMF + HFUS * XM(J)/DT
		STC(K) = TFRZ
		XM(J) = 0.0
              ELSE
	        XM(J) = XM(J) - MICE(K)
		XMF = XMF + HFUS * MICE(K)/DT
		MICE(K) = 0.0
		STC(K) = TFRZ
              END IF
              MLIQ(K) = MAX(0.,WMASS0(K)-MICE(K))
	    END IF
	  END DO
	  HEATR(J) = XM(J)*HFUS/DT
          STC(J) = TFRZ + HEATR(J)*FACT(J)
        END IF
      END DO
    END IF

! NOW REMOVE EXCESS COLD BY FREEZING LIQUID OF LAYERS (MAY NOT BE NECESSARY WITH ABOVE LOOP)

    IF (ANY(STC(1:4) < TFRZ) .AND. ANY(MLIQ(1:4) > 0.)) THEN
      DO J = 1,NSOIL
        IF ( STC(J) < TFRZ ) THEN                                       
	  HEATR(J) = (STC(J)-TFRZ)/FACT(J)
          XM(J) = HEATR(J)*DT/HFUS                           
          DO K = 1,NSOIL
	    IF (J .NE. K .AND. MLIQ(K) > 0. .AND. XM(J) < -0.1) THEN
	      IF (MLIQ(K) > ABS(XM(J))) THEN  ! LAYER ABSORBS ALL
	        MICE(K) = MICE(K) - XM(J)
		XMF = XMF + HFUS * XM(J)/DT
		STC(K) = TFRZ
		XM(J) = 0.0
              ELSE
	        XM(J) = XM(J) + MLIQ(K)
		XMF = XMF - HFUS * MLIQ(K)/DT
		MICE(K) = WMASS0(K)
		STC(K) = TFRZ
              END IF
              MLIQ(K) = MAX(0.,WMASS0(K)-MICE(K))
	    END IF
	  END DO
	  HEATR(J) = XM(J)*HFUS/DT
          STC(J) = TFRZ + HEATR(J)*FACT(J)
        END IF
      END DO
    END IF
    
END IF   ! OPT_GLA == 1

    DO J = ISNOW+1,0             ! snow
       SNLIQ(J) = MLIQ(J)
       SNICE(J) = MICE(J)
    END DO

    DO J = 1, NSOIL              ! soil
      IF(OPT_GLA == 1) THEN 
       SH2O(J) =  MLIQ(J)            / (1000. * DZSNSO(J))
       SH2O(J) =  MAX(0.0,MIN(1.0,SH2O(J)))
!       SMC(J)  = (MLIQ(J) + MICE(J)) / (1000. * DZSNSO(J))
      ELSEIF(OPT_GLA == 2) THEN 
       SH2O(J) = 0.0             ! ice, assume all frozen...forever
      END IF
      SMC(J)  = 1.0 
    END DO
   
  END SUBROUTINE PHASECHANGE_GLACIER
! ==================================================================================================
  SUBROUTINE WATER_GLACIER (NSNOW  ,NSOIL  ,IMELT  ,DT     ,PRCP   ,SFCTMP , & !in
                            QVAP   ,QDEW   ,FICEOLD,ZSOIL  ,                 & !in
                            ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  ,STC    , & !inout
                            DZSNSO ,SH2O   ,SICE   ,PONDING,ZSNSO  ,FSH    , & !inout
                            RUNSRF ,RUNSUB ,QSNOW  ,PONDING1 ,PONDING2,QSNBOT,FPICE     &   !out
#ifdef WRF_HYDRO
                            , sfcheadrt                                      &
#endif
                            )  !out
! ----------------------------------------------------------------------  
! Code history:
! Initial code: Guo-Yue Niu, Oct. 2007
! ----------------------------------------------------------------------
  implicit none
! ----------------------------------------------------------------------
! input
  INTEGER,                         INTENT(IN)    :: NSNOW   !maximum no. of snow layers
  INTEGER,                         INTENT(IN)    :: NSOIL   !no. of soil layers
  INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT   !melting state index [1-melt; 2-freeze]
  REAL,                            INTENT(IN)    :: DT      !main time step (s)
  REAL,                            INTENT(IN)    :: PRCP    !precipitation (mm/s)
  REAL,                            INTENT(IN)    :: SFCTMP  !surface air temperature [k]
  REAL,                            INTENT(INOUT)    :: QVAP    !soil surface evaporation rate[mm/s]
  REAL,                            INTENT(INOUT)    :: QDEW    !soil surface dew rate[mm/s]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD !ice fraction at last timestep
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !layer-bottom depth from soil surf (m)

! input/output
  INTEGER,                         INTENT(INOUT) :: ISNOW   !actual no. of snow layers
  REAL,                            INTENT(INOUT) :: SNOWH   !snow height [m]
  REAL,                            INTENT(INOUT) :: SNEQV   !snow water eqv. [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE   !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ   !snow layer liquid water [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC     !snow/soil layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO  !snow/soil layer thickness [m]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O    !soil liquid water content [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE    !soil ice content [m3/m3]
  REAL                           , INTENT(INOUT) :: PONDING ![mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO   !layer-bottom depth from snow surf [m]
  REAL                           , INTENT(INOUT) :: FSH     !total sensible heat (w/m2) [+ to atm]

! output
  REAL,                            INTENT(OUT)   :: RUNSRF  !surface runoff [mm/s] 
  REAL,                            INTENT(OUT)   :: RUNSUB  !baseflow (sturation excess) [mm/s]
  REAL,                            INTENT(OUT)   :: QSNOW   !snow at ground srf (mm/s) [+]
  REAL,                            INTENT(OUT)   :: PONDING1
  REAL,                            INTENT(OUT)   :: PONDING2
  REAL,                            INTENT(OUT)   :: QSNBOT  !melting water out of snow bottom [mm/s]
  REAL,                            INTENT(OUT)   :: FPICE   !precipitation frozen fraction

! local
  REAL                                           :: QRAIN   !rain at ground srf (mm) [+]
  REAL                                           :: QSEVA   !soil surface evap rate [mm/s]
  REAL                                           :: QSDEW   !soil surface dew rate [mm/s]
  REAL                                           :: QSNFRO  !snow surface frost rate[mm/s]
  REAL                                           :: QSNSUB  !snow surface sublimation rate [mm/s]
  REAL                                           :: SNOWHIN !snow depth increasing rate (m/s)
  REAL                                           :: SNOFLOW !glacier flow [mm/s]
  REAL                                           :: BDFALL  !density of new snow (mm water/m snow)
  REAL                                           :: REPLACE !replacement water due to sublimation of glacier
  REAL, DIMENSION(       1:NSOIL)                :: SICE_SAVE  !soil ice content [m3/m3]
  REAL, DIMENSION(       1:NSOIL)                :: SH2O_SAVE  !soil liquid water content [m3/m3]
  INTEGER :: ILEV

#ifdef WRF_HYDRO
  REAL                           , INTENT(INOUT)    :: sfcheadrt
#endif

! ----------------------------------------------------------------------
! initialize

   SNOFLOW         = 0.
   RUNSUB          = 0.
   RUNSRF          = 0.
   SICE_SAVE       = SICE
   SH2O_SAVE       = SH2O

! --------------------------------------------------------------------
! partition precipitation into rain and snow (from CANWATER)

! Jordan (1991)

     IF(OPT_SNF == 1 .OR. OPT_SNF == 4) THEN
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
!     print*, 'fpice: ',fpice

! Hedstrom NR and JW Pomeroy (1998), Hydrol. Processes, 12, 1611-1625
! fresh snow density

     BDFALL = MIN(120.,67.92+51.25*EXP((SFCTMP-TFRZ)/2.59)) !MB: change to MIN v3.7

     QRAIN   = PRCP * (1.-FPICE)
     QSNOW   = PRCP * FPICE
     SNOWHIN = QSNOW/BDFALL
!     print *, 'qrain, qsnow',qrain,qsnow,qrain*dt,qsnow*dt

! sublimation, frost, evaporation, and dew

     QSNSUB = QVAP  ! send total sublimation/frost to SNOWWATER and deal with it there
     QSNFRO = QDEW

     CALL SNOWWATER_GLACIER (NSNOW  ,NSOIL  ,IMELT  ,DT     ,SFCTMP , & !in
                             SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB ,QRAIN  , & !in
                             FICEOLD,ZSOIL  ,                         & !in
                             ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & !inout
                             SH2O   ,SICE   ,STC    ,DZSNSO ,ZSNSO  , & !inout
                             FSH    ,                                 & !inout
                             QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  !out

    !PONDING: melting water from snow when there is no layer
    
    RUNSRF = (PONDING+PONDING1+PONDING2)/DT

    IF(ISNOW == 0) THEN
      RUNSRF = RUNSRF + QSNBOT + QRAIN
    ELSE
      RUNSRF = RUNSRF + QSNBOT
    ENDIF

#ifdef WRF_HYDRO
      RUNSRF = RUNSRF + sfcheadrt/DT  !sfcheadrt units (mm)
#endif
    
    IF(OPT_GLA == 1) THEN
      REPLACE = 0.0
      DO ILEV = 1,NSOIL
       REPLACE = REPLACE + DZSNSO(ILEV)*(SICE(ILEV) - SICE_SAVE(ILEV) + SH2O(ILEV) - SH2O_SAVE(ILEV))
      END DO
      REPLACE = REPLACE * 1000.0 / DT     ! convert to [mm/s]
    
      SICE = MIN(1.0,SICE_SAVE)
    ELSEIF(OPT_GLA == 2) THEN
      SICE = 1.0
    END IF
    SH2O = 1.0 - SICE
    
    ! use RUNSUB as a water balancer, SNOFLOW is snow that disappears, REPLACE is
    !   water from below that replaces glacier loss

    IF(OPT_GLA == 1) THEN
      RUNSUB       = SNOFLOW + REPLACE
    ELSEIF(OPT_GLA == 2) THEN
      RUNSUB       = SNOFLOW
      QVAP = QSNSUB
      QDEW = QSNFRO
    END IF

  END SUBROUTINE WATER_GLACIER
! ==================================================================================================
! ----------------------------------------------------------------------
  SUBROUTINE SNOWWATER_GLACIER (NSNOW  ,NSOIL  ,IMELT  ,DT     ,SFCTMP , & !in
                                SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB ,QRAIN  , & !in
                                FICEOLD,ZSOIL  ,                         & !in
                                ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & !inout
                                SH2O   ,SICE   ,STC    ,DZSNSO ,ZSNSO  , & !inout
				FSH    ,                                 & !inout
                                QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  !out
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers
  INTEGER,                         INTENT(IN)    :: NSOIL  !no. of soil layers
  INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT  !melting state index [0-no melt;1-melt]
  REAL,                            INTENT(IN)    :: DT     !time step (s)
  REAL,                            INTENT(IN)    :: SFCTMP !surface air temperature [k]
  REAL,                            INTENT(IN)    :: SNOWHIN!snow depth increasing rate (m/s)
  REAL,                            INTENT(IN)    :: QSNOW  !snow at ground srf (mm/s) [+]
  REAL,                            INTENT(INOUT)    :: QSNFRO !snow surface frost rate[mm/s]
  REAL,                            INTENT(INOUT)    :: QSNSUB !snow surface sublimation rate[mm/s]
  REAL,                            INTENT(IN)    :: QRAIN  !snow surface rain rate[mm/s]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(IN)    :: FICEOLD!ice fraction at last timestep
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !layer-bottom depth from soil surf (m)

! input & output
  INTEGER,                         INTENT(INOUT) :: ISNOW  !actual no. of snow layers
  REAL,                            INTENT(INOUT) :: SNOWH  !snow height [m]
  REAL,                            INTENT(INOUT) :: SNEQV  !snow water eqv. [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid moisture (m3/m3)
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE   !soil ice moisture (m3/m3)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO !snow/soil layer thickness [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  !layer-bottom depth from snow surf [m]
  REAL                           , INTENT(INOUT) :: FSH     !total sensible heat (w/m2) [+ to atm]

! output
  REAL,                              INTENT(OUT) :: QSNBOT !melting water out of snow bottom [mm/s]
  REAL,                              INTENT(OUT) :: SNOFLOW!glacier flow [mm]
  REAL,                              INTENT(OUT) :: PONDING1
  REAL,                              INTENT(OUT) :: PONDING2

! local
  INTEGER :: IZ
  REAL    :: BDSNOW  !bulk density of snow (kg/m3)
! ----------------------------------------------------------------------
   SNOFLOW = 0.0
   PONDING1 = 0.0
   PONDING2 = 0.0

   CALL SNOWFALL_GLACIER (NSOIL  ,NSNOW  ,DT     ,QSNOW  ,SNOWHIN, & !in
                          SFCTMP ,                                 & !in
                          ISNOW  ,SNOWH  ,DZSNSO ,STC    ,SNICE  , & !inout
                          SNLIQ  ,SNEQV  )                           !inout

   IF(ISNOW < 0) THEN        !WHEN MORE THAN ONE LAYER
     CALL  COMPACT_GLACIER (NSNOW  ,NSOIL  ,DT     ,STC    ,SNICE  , & !in
                            SNLIQ  ,IMELT  ,FICEOLD,                 & !in
                            ISNOW  ,DZSNSO )                           !inout

     CALL  COMBINE_GLACIER (NSNOW  ,NSOIL  ,                         & !in
                            ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !inout
                            DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & !inout
                            PONDING1       ,PONDING2)                  !out

     CALL   DIVIDE_GLACIER (NSNOW  ,NSOIL  ,                         & !in
                            ISNOW  ,STC    ,SNICE  ,SNLIQ  ,DZSNSO )   !inout
   END IF

!SET EMPTY SNOW LAYERS TO ZERO

   DO IZ = -NSNOW+1, ISNOW
        SNICE(IZ) = 0.
        SNLIQ(IZ) = 0.
        STC(IZ)   = 0.
        DZSNSO(IZ)= 0.
        ZSNSO(IZ) = 0.
   ENDDO

   CALL  SNOWH2O_GLACIER (NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & !in 
                          QRAIN  ,                                 & !in
                          ISNOW  ,DZSNSO ,SNOWH  ,SNEQV  ,SNICE  , & !inout
                          SNLIQ  ,SH2O   ,SICE   ,STC    ,         & !inout
			  PONDING1       ,PONDING2       ,FSH    , & !inout
                          QSNBOT )                                   !out

!to obtain equilibrium state of snow in glacier region
       
   IF(SNEQV > 2000.) THEN   ! 2000 mm -> maximum water depth
      BDSNOW      = SNICE(0) / DZSNSO(0)
      SNOFLOW     = (SNEQV - 2000.)
      SNICE(0)    = SNICE(0)  - SNOFLOW 
      DZSNSO(0)   = DZSNSO(0) - SNOFLOW/BDSNOW
      SNOFLOW     = SNOFLOW / DT
   END IF

! sum up snow mass for layered snow

   IF(ISNOW /= 0) THEN
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

  END SUBROUTINE SNOWWATER_GLACIER
! ==================================================================================================
  SUBROUTINE SNOWFALL_GLACIER (NSOIL  ,NSNOW  ,DT     ,QSNOW  ,SNOWHIN , & !in
                               SFCTMP ,                                  & !in
                               ISNOW  ,SNOWH  ,DZSNSO ,STC    ,SNICE   , & !inout
                               SNLIQ  ,SNEQV  )                            !inout
! ----------------------------------------------------------------------
! snow depth and density to account for the new snowfall.
! new values of snow depth & density returned.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input

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
 
    IF(ISNOW == 0  .AND. QSNOW>0. .AND. SNOWH >= 0.05) THEN
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
  END SUBROUTINE SNOWFALL_GLACIER
! ==================================================================================================
! ----------------------------------------------------------------------
  SUBROUTINE COMPACT_GLACIER (NSNOW  ,NSOIL  ,DT     ,STC    ,SNICE , & !in
                              SNLIQ  ,IMELT  ,FICEOLD,                & !in
                              ISNOW  ,DZSNSO )                          !inout
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
   INTEGER,                         INTENT(IN)    :: NSOIL  !no. of soil layers [ =4]
   INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers [ =3]
   INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT  !melting state index [0-no melt;1-melt]
   REAL,                            INTENT(IN)    :: DT     !time step (sec)
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: STC    !snow layer temperature [k]
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNICE  !snow layer ice [mm]
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNLIQ  !snow layer liquid water [mm]
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD!ice fraction at last timestep

! input and output
   INTEGER,                         INTENT(INOUT) :: ISNOW  ! actual no. of snow layers
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO ! snow layer thickness [m]

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

  END SUBROUTINE COMPACT_GLACIER
! ==================================================================================================
  SUBROUTINE COMBINE_GLACIER (NSNOW  ,NSOIL  ,                         & !in
                              ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !inout
                              DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & !inout
                              PONDING1       ,PONDING2)                  !inout
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input

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
    REAL,                            INTENT(INOUT) :: SNEQV !snow water equivalent [m]
    REAL,                            INTENT(INOUT) :: SNOWH !snow depth [m]
    REAL,                            INTENT(INOUT) :: PONDING1
    REAL,                            INTENT(INOUT) :: PONDING2

! local variables:

    INTEGER :: I,J,K,L               ! node indices
    INTEGER :: ISNOW_OLD             ! number of top snow layer
    INTEGER :: MSSI                  ! node index
    INTEGER :: NEIBOR                ! adjacent node selected for combination
    REAL    :: ZWICE                 ! total ice mass in snow
    REAL    :: ZWLIQ                 ! total liquid water in snow
    REAL    :: DZMIN(3)              ! minimum of top snow layer
    DATA DZMIN /0.045, 0.05, 0.2/
!    DATA DZMIN /0.025, 0.025, 0.1/  ! MB: change limit
!-----------------------------------------------------------------------

       ISNOW_OLD = ISNOW

       DO J = ISNOW_OLD+1,0
          IF (SNICE(J) <= .1) THEN
             IF(J /= 0) THEN
                SNLIQ(J+1) = SNLIQ(J+1) + SNLIQ(J)
                SNICE(J+1) = SNICE(J+1) + SNICE(J)
             ELSE
               IF (ISNOW_OLD < -1) THEN
                SNLIQ(J-1) = SNLIQ(J-1) + SNLIQ(J)
                SNICE(J-1) = SNICE(J-1) + SNICE(J)
               ELSE
                PONDING1 = PONDING1 + SNLIQ(J)       ! ISNOW WILL GET SET TO ZERO BELOW
                SNEQV = SNICE(J)                     ! PONDING WILL GET ADDED TO PONDING FROM
                SNOWH = DZSNSO(J)                    ! PHASECHANGE WHICH SHOULD BE ZERO HERE
                SNLIQ(J) = 0.0                       ! BECAUSE THERE IT WAS ONLY CALCULATED
                SNICE(J) = 0.0                       ! FOR THIN SNOW
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

!       IF (SNOWH < 0.025 .AND. ISNOW < 0 ) THEN ! MB: change limit
       IF (SNOWH < 0.05 .AND. ISNOW < 0 ) THEN
          ISNOW  = 0
          SNEQV = ZWICE
          PONDING2 = PONDING2 + ZWLIQ           ! LIMIT OF ISNOW < 0 MEANS INPUT PONDING
          IF(SNEQV <= 0.) SNOWH = 0.            ! SHOULD BE ZERO; SEE ABOVE
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

                CALL COMBO_GLACIER (DZSNSO(J), SNLIQ(J), SNICE(J), &
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

  END SUBROUTINE COMBINE_GLACIER
! ==================================================================================================

! ----------------------------------------------------------------------
  SUBROUTINE COMBO_GLACIER(DZ,  WLIQ,  WICE, T, DZ2, WLIQ2, WICE2, T2)
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

  END SUBROUTINE COMBO_GLACIER
! ==================================================================================================
  SUBROUTINE DIVIDE_GLACIER (NSNOW  ,NSOIL  ,                         & !in
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

             CALL COMBO_GLACIER (DZ(2), SWLIQ(2), SWICE(2), TSNO(2), DRR, &
                  ZWLIQ, ZWICE, TSNO(1))

             ! subdivide a new layer
!             IF (MSNO <= 2 .AND. DZ(2) > 0.20) THEN  ! MB: change limit
             IF (MSNO <= 2 .AND. DZ(2) > 0.10) THEN
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
             CALL COMBO_GLACIER (DZ(3), SWLIQ(3), SWICE(3), TSNO(3), DRR, &
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

  END SUBROUTINE DIVIDE_GLACIER
! ==================================================================================================
  SUBROUTINE SNOWH2O_GLACIER (NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & !in 
                              QRAIN  ,                                 & !in
                              ISNOW  ,DZSNSO ,SNOWH  ,SNEQV  ,SNICE  , & !inout
                              SNLIQ  ,SH2O   ,SICE   ,STC    ,         & !inout
                              PONDING1       ,PONDING2       ,FSH    , & !inout
                              QSNBOT )                                   !out
! ----------------------------------------------------------------------
! Renew the mass of ice lens (SNICE) and liquid (SNLIQ) of the
! surface snow layer resulting from sublimation (frost) / evaporation (dew)
! ----------------------------------------------------------------------
   IMPLICIT NONE
! ----------------------------------------------------------------------
! input

   INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers[=3]
   INTEGER,                         INTENT(IN)    :: NSOIL  !No. of soil layers[=4]
   REAL,                            INTENT(IN)    :: DT     !time step
   REAL,                            INTENT(INOUT)    :: QSNFRO !snow surface frost rate[mm/s]
   REAL,                            INTENT(INOUT)    :: QSNSUB !snow surface sublimation rate[mm/s]
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
   REAL,                            INTENT(INOUT) :: PONDING1
   REAL,                            INTENT(INOUT) :: PONDING2
   REAL,                            INTENT(INOUT) :: FSH     !total sensible heat (w/m2) [+ to atm]

! local variables:

   INTEGER                     :: J         !do loop/array indices
   REAL                        :: QIN       !water flow into the element (mm/s)
   REAL                        :: QOUT      !water flow out of the element (mm/s)
   REAL                        :: WGDIF     !ice mass after minus sublimation
   REAL, DIMENSION(-NSNOW+1:0) :: VOL_LIQ   !partial volume of liquid water in layer
   REAL, DIMENSION(-NSNOW+1:0) :: VOL_ICE   !partial volume of ice lens in layer
   REAL, DIMENSION(-NSNOW+1:0) :: EPORE     !effective porosity = porosity - VOL_ICE
   REAL :: PROPOR, TEMP
! ----------------------------------------------------------------------

!for the case when SNEQV becomes '0' after 'COMBINE'

   IF(SNEQV == 0.) THEN
     IF(OPT_GLA == 1) THEN
       SICE(1) =  SICE(1) + (QSNFRO-QSNSUB)*DT/(DZSNSO(1)*1000.)
     ELSEIF(OPT_GLA == 2) THEN
       FSH = FSH - (QSNFRO-QSNSUB)*HSUB
       QSNFRO = 0.0
       QSNSUB = 0.0
     END IF
   END IF

! for shallow snow without a layer
! snow surface sublimation may be larger than existing snow mass. To conserve water,
! excessive sublimation is used to reduce soil water. Smaller time steps would tend 
! to aviod this problem.

   IF(ISNOW == 0 .and. SNEQV > 0.) THEN
      IF(OPT_GLA == 1) THEN
        TEMP   = SNEQV
        SNEQV  = SNEQV - QSNSUB*DT + QSNFRO*DT
        PROPOR = SNEQV/TEMP
        SNOWH  = MAX(0.,PROPOR * SNOWH)
      ELSEIF(OPT_GLA == 2) THEN
        FSH = FSH - (QSNFRO-QSNSUB)*HSUB
        QSNFRO = 0.0
        QSNSUB = 0.0
      END IF

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
         CALL  COMBINE_GLACIER (NSNOW  ,NSOIL  ,                         & !in
                                ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !inout
                                DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & !inout
                               PONDING1, PONDING2 )                        !inout
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

  END SUBROUTINE SNOWH2O_GLACIER
! ********************* end of water subroutines ******************************************
! ==================================================================================================
  SUBROUTINE ERROR_GLACIER (ILOC   ,JLOC   ,SWDOWN ,FSA    ,FSR    ,FIRA   , &
                            FSH    ,FGEV   ,SSOIL  ,SAG    ,PRCP   ,EDIR   , &
		            RUNSRF ,RUNSUB ,SNEQV  ,DT     ,BEG_WB )
! --------------------------------------------------------------------------------------------------
! check surface energy balance and water balance
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
  INTEGER                        , INTENT(IN) :: ILOC   !grid index
  INTEGER                        , INTENT(IN) :: JLOC   !grid index
  REAL                           , INTENT(IN) :: SWDOWN !downward solar filtered by sun angle [w/m2]
  REAL                           , INTENT(IN) :: FSA    !total absorbed solar radiation (w/m2)
  REAL                           , INTENT(IN) :: FSR    !total reflected solar radiation (w/m2)
  REAL                           , INTENT(IN) :: FIRA   !total net longwave rad (w/m2)  [+ to atm]
  REAL                           , INTENT(IN) :: FSH    !total sensible heat (w/m2)     [+ to atm]
  REAL                           , INTENT(IN) :: FGEV   !ground evaporation heat (w/m2) [+ to atm]
  REAL                           , INTENT(IN) :: SSOIL  !ground heat flux (w/m2)        [+ to soil]
  REAL                           , INTENT(IN) :: SAG

  REAL                           , INTENT(IN) :: PRCP   !precipitation rate (kg m-2 s-1)
  REAL                           , INTENT(IN) :: EDIR   !soil surface evaporation rate[mm/s]
  REAL                           , INTENT(IN) :: RUNSRF !surface runoff [mm/s] 
  REAL                           , INTENT(IN) :: RUNSUB !baseflow (saturation excess) [mm/s]
  REAL                           , INTENT(IN) :: SNEQV  !snow water eqv. [mm]
  REAL                           , INTENT(IN) :: DT     !time step [sec]
  REAL                           , INTENT(IN) :: BEG_WB !water storage at begin of a timesetp [mm]

  REAL                                        :: END_WB !water storage at end of a timestep [mm]
  REAL                                        :: ERRWAT !error in water balance [mm/timestep]
  REAL                                        :: ERRENG !error in surface energy balance [w/m2]
  REAL                                        :: ERRSW  !error in shortwave radiation balance [w/m2]
  CHARACTER(len=256)                          :: message
! --------------------------------------------------------------------------------------------------
   ERRSW   = SWDOWN - (FSA + FSR)
   IF (ERRSW > 0.01) THEN            ! w/m2
     WRITE(*,*) "SAG    =",SAG
     WRITE(*,*) "FSA    =",FSA
     WRITE(*,*) "FSR    =",FSR
     WRITE(message,*) 'ERRSW =',ERRSW
     call wrf_message(trim(message))
     call wrf_error_fatal("Radiation budget problem in NOAHMP GLACIER")
   END IF

   ERRENG = SAG-(FIRA+FSH+FGEV+SSOIL)
   IF(ERRENG > 0.01) THEN
      write(message,*) 'ERRENG =',ERRENG
      call wrf_message(trim(message))
      WRITE(message,'(i6,1x,i6,1x,5F10.4)')ILOC,JLOC,SAG,FIRA,FSH,FGEV,SSOIL
      call wrf_message(trim(message))
      call wrf_error_fatal("Energy budget problem in NOAHMP GLACIER")
   END IF

   END_WB = SNEQV
   ERRWAT = END_WB-BEG_WB-(PRCP-EDIR-RUNSRF-RUNSUB)*DT

#ifndef WRF_HYDRO
   IF(ABS(ERRWAT) > 0.1) THEN
      if (ERRWAT > 0) then
         call wrf_message ('The model is gaining water (ERRWAT is positive)')
      else
         call wrf_message('The model is losing water (ERRWAT is negative)')
      endif
      write(message, *) 'ERRWAT =',ERRWAT, "kg m{-2} timestep{-1}"
      call wrf_message(trim(message))
      WRITE(message,'("    I      J     END_WB     BEG_WB       PRCP       EDIR      RUNSRF     RUNSUB")')
           call wrf_message(trim(message))
           WRITE(message,'(i6,1x,i6,1x,2f15.3,4f11.5)')ILOC,JLOC,END_WB,BEG_WB,PRCP*DT,&
                EDIR*DT,RUNSRF*DT,RUNSUB*DT
           call wrf_message(trim(message))
           call wrf_error_fatal("Water budget problem in NOAHMP GLACIER")
        END IF
#endif

 END SUBROUTINE ERROR_GLACIER
! ==================================================================================================

  SUBROUTINE NOAHMP_OPTIONS_GLACIER(iopt_alb  ,iopt_snf  ,iopt_tbot, iopt_stc, iopt_gla )

  IMPLICIT NONE

  INTEGER,  INTENT(IN) :: iopt_alb  !snow surface albedo (1->BATS; 2->CLASS)
  INTEGER,  INTENT(IN) :: iopt_snf  !rainfall & snowfall (1-Jordan91; 2->BATS; 3->Noah)
  INTEGER,  INTENT(IN) :: iopt_tbot !lower boundary of soil temperature (1->zero-flux; 2->Noah)
  INTEGER,  INTENT(IN) :: iopt_stc  !snow/soil temperature time scheme (only layer 1)
                                    ! 1 -> semi-implicit; 2 -> full implicit (original Noah)
  INTEGER,  INTENT(IN) :: IOPT_GLA  ! glacier option (1->phase change; 2->simple)

! -------------------------------------------------------------------------------------------------

  opt_alb  = iopt_alb  
  opt_snf  = iopt_snf  
  opt_tbot = iopt_tbot 
  opt_stc  = iopt_stc
  opt_gla  = iopt_gla
  
  end subroutine noahmp_options_glacier
 
END MODULE NOAHMP_GLACIER_ROUTINES
! ==================================================================================================

MODULE MODULE_SF_NOAHMP_GLACIER

  USE NOAHMP_GLACIER_ROUTINES
  USE NOAHMP_GLACIER_GLOBALS

END MODULE MODULE_SF_NOAHMP_GLACIER
