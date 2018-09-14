!
! ODPS_Predictor
!
! Module containing routines to compute the optical depth predictors for
! the Optical Depth in Pressure Space (ODPS) algorithm.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, 29-Aug-2006
!                       yong.han@noaa.gov
!
!       Modified by:    Tong Zhu, 18-Nov-2008
!                       tong.zhu@noaa.gov
!

MODULE ODPS_Predictor

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds              , ONLY: fp
  USE CRTM_Parameters         , ONLY: MINIMUM_ABSORBER_AMOUNT, &
                                      RECIPROCAL_GRAVITY
  USE CRTM_Atmosphere_Define  , ONLY: CRTM_Atmosphere_type
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type, &
                                      CRTM_GeometryInfo_GetValue
  USE ODPS_Predictor_Define   , ONLY: ODPS_Predictor_type, &
                                      PAFV_type          , &
                                      PAFV_Associated    , &
                                      MAX_OPTRAN_ORDER
  USE ODPS_CoordinateMapping  , ONLY: Map_Input           , &
                                      Map_Input_TL        , &
                                      Map_Input_AD        , &
                                      Compute_Interp_Index
  USE ODPS_Define             , ONLY: ODPS_type
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: iVar_type
  ! Procedures
  PUBLIC :: ODPS_Assemble_Predictors
  PUBLIC :: ODPS_Assemble_Predictors_TL
  PUBLIC :: ODPS_Assemble_Predictors_AD
  PUBLIC :: ODPS_Compute_Predictor
  PUBLIC :: ODPS_Compute_Predictor_TL
  PUBLIC :: ODPS_Compute_Predictor_AD
  PUBLIC :: ODPS_Compute_Predictor_ODAS
  PUBLIC :: ODPS_Compute_Predictor_ODAS_TL
  PUBLIC :: ODPS_Compute_Predictor_ODAS_AD
  PUBLIC :: ODPS_Get_max_n_Predictors
  PUBLIC :: ODPS_Get_n_Components
  PUBLIC :: ODPS_Get_n_Absorbers
  PUBLIC :: ODPS_Get_Component_ID
  PUBLIC :: ODPS_Get_Absorber_ID
  PUBLIC :: ODPS_Get_Ozone_Component_ID
  PUBLIC :: ODPS_Get_SaveFWVFlag
  ! Parameters
  PUBLIC :: TOT_ComID
  PUBLIC :: WLO_ComID
  PUBLIC :: WET_ComID
  PUBLIC :: CO2_ComID
  PUBLIC :: GROUP_1
  PUBLIC :: GROUP_2
  PUBLIC :: GROUP_3
  PUBLIC :: ALLOW_OPTRAN


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
  '$Id: $'

  ! Dimensions of each predictor group.
  INTEGER, PARAMETER  :: N_G = 3
  INTEGER, PARAMETER  :: N_COMPONENTS_G(N_G)     = (/8,   5, 2/)
  INTEGER, PARAMETER  :: N_ABSORBERS_G(N_G)      = (/6,   3, 1/)
  INTEGER, PARAMETER  :: MAX_N_PREDICTORS_G(N_G) = (/18, 15, 14/)
  ! Group index (note, group indexes 4 - 6 are reserved for Zeeman sub-algorithms
  INTEGER, PARAMETER :: GROUP_1 = 1
  INTEGER, PARAMETER :: GROUP_2 = 2
  INTEGER, PARAMETER :: GROUP_3 = 3

  ! Number of predictors for each component
  INTEGER, PARAMETER :: N_PREDICTORS_G1(8) = (/ &
                     7, &  ! dry gas
                    18, &  ! water vapor line only, no continua
                     7, &  ! water vapor continua only, no line absorption
!                    13, &  ! ozone
                    11, &  ! ozone
                    11, &  ! CO2
                    14, &  ! N2O
                    10, &  ! CO
                    11 /)  ! CH4

  INTEGER, PARAMETER :: N_PREDICTORS_G2(5) = (/ &
                     7, &  ! dry gas
                    15, &  ! water vapor line only, no continua
                     7, &  ! water vapor continua only, no line absorption
!                    13, &  ! ozone
                    11, &  ! ozone
                    10 /)  ! CO2

  INTEGER, PARAMETER :: N_PREDICTORS_G3(2) = (/ &
                     7, &  ! dry gas
                    14 /)  ! water vapor line and continua


  ! Component IDs
  INTEGER,  PARAMETER :: TOT_ComID = 10    ! total tau
  INTEGER,  PARAMETER :: DRY_ComID_G1 =  7   ! dry gas for Group-1 sensors
  INTEGER,  PARAMETER :: DRY_ComID_G2 = 20   ! dry gas, for Gorup-2 sensors
  INTEGER,  PARAMETER :: WLO_ComID = 101  ! water vapor line only, no continua
  INTEGER,  PARAMETER :: WCO_ComID = 15   ! water vapor continua only, no line absorption
  INTEGER,  PARAMETER :: OZO_ComID = 114  ! ozone
  INTEGER,  PARAMETER :: CO2_ComID = 121  ! CO2
  INTEGER,  PARAMETER :: N2O_ComID = 120  ! N2O
  INTEGER,  PARAMETER :: CO_ComID  = 119  ! CO
  INTEGER,  PARAMETER :: CH4_ComID = 118  ! CH4

  ! Microwave sensors
  INTEGER,  PARAMETER :: EDRY_ComID = 113  ! Effective dry
  INTEGER,  PARAMETER :: WET_ComID = 12  ! water vapor line & no continua

  ! IR sensor Component indexes (sequence index in an array)
  INTEGER, PARAMETER :: COMP_DRY_IR = 1
  INTEGER, PARAMETER :: COMP_WLO_IR = 2
  INTEGER, PARAMETER :: COMP_WCO_IR = 3
  INTEGER, PARAMETER :: COMP_OZO_IR = 4
  INTEGER, PARAMETER :: COMP_CO2_IR = 5
  INTEGER, PARAMETER :: COMP_N2O_IR = 6
  INTEGER, PARAMETER :: COMP_CO_IR  = 7
  INTEGER, PARAMETER :: COMP_CH4_IR = 8

  ! MW sensor Component indexes
  INTEGER, PARAMETER :: COMP_DRY_MW = 1
  INTEGER, PARAMETER :: COMP_WET_MW = 2

  ! Component index to component ID mapping
  INTEGER, PARAMETER :: COMPONENT_ID_MAP_G1(8) = (/ &
                                DRY_ComID_G1, &
                                   WLO_ComID, &
                                   WCO_ComID, &
                                   OZO_ComID, &
                                   CO2_ComID, &
                                   N2O_ComID, &
                                   CO_ComID , &
                                   CH4_ComID /)

  INTEGER, PARAMETER :: COMPONENT_ID_MAP_G2(5) = (/ &
                                DRY_ComID_G2, &
                                   WLO_ComID, &
                                   WCO_ComID, &
                                   OZO_ComID, &
                                   CO2_ComID /)

  INTEGER, PARAMETER :: COMPONENT_ID_MAP_G3(2) = (/ &
                                  EDRY_ComID, &
                                   WET_ComID /)

  ! Absorber IDs (HITRAN)
  INTEGER, PARAMETER ::   H2O_ID =  1
  INTEGER, PARAMETER ::   CO2_ID =  2
  INTEGER, PARAMETER ::    O3_ID =  3
  INTEGER, PARAMETER ::   N2O_ID =  4
  INTEGER, PARAMETER ::    CO_ID =  5
  INTEGER, PARAMETER ::   CH4_ID =  6

  ! Absorber (Molecule) indexes for accessing absorber profile array
  INTEGER,  PARAMETER :: ABS_H2O_IR = 1
  INTEGER,  PARAMETER :: ABS_O3_IR  = 2
  INTEGER,  PARAMETER :: ABS_CO2_IR = 3
  INTEGER,  PARAMETER :: ABS_N2O_IR = 4
  INTEGER,  PARAMETER :: ABS_CO_IR  = 5
  INTEGER,  PARAMETER :: ABS_CH4_IR = 6

  INTEGER,  PARAMETER :: ABS_H2O_MW = 1

  ! Absorber index to absorber ID mapping
  INTEGER,  PARAMETER :: ABSORBER_ID_MAP_G1(6) = (/ &
                                    H2O_ID, &
                                    O3_ID,  &
                                    CO2_ID, &
                                    N2O_ID, &
                                    CO_ID,  &
                                    CH4_ID /)

  INTEGER,  PARAMETER :: ABSORBER_ID_MAP_G2(3) = (/ &
                                    H2O_ID, &
                                    O3_ID,  &
                                    CO2_ID /)

  INTEGER,  PARAMETER :: ABSORBER_ID_MAP_G3(1) = (/ &
                                           H2O_ID /)
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO      = 0.0_fp
  REAL(fp), PARAMETER :: ONE       = 1.0_fp
  REAL(fp), PARAMETER :: TWO       = 2.0_fp
  REAL(fp), PARAMETER :: THREE     = 3.0_fp
  REAL(fp), PARAMETER :: FOUR      = 4.0_fp
  REAL(fp), PARAMETER :: TEN       = 10.0_fp
  REAL(fp), PARAMETER :: POINT_25  = 0.25_fp
  REAL(fp), PARAMETER :: POINT_5   = 0.5_fp
  REAL(fp), PARAMETER :: POINT_75  = 0.75_fp
  REAL(fp), PARAMETER :: ONE_POINT_5   = 1.5_fp
  REAL(fp), PARAMETER :: ONE_POINT_25  = 1.25_fp
  REAL(fp), PARAMETER :: ONE_POINT_75  = 1.75_fp


  LOGICAL, PARAMETER  :: ALLOW_OPTRAN = .TRUE.


  ! ------------------------------------------
  ! Structure definition to hold forward model
  ! variables across FWD, TL, and AD calls
  ! ------------------------------------------
  TYPE :: iVar_type
    PRIVATE
    INTEGER :: dummy
  END TYPE iVar_type

CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Assemble_Predictors
!
! PURPOSE:
!       Subroutine to assemble all the gas absorption model predictors
!       for the ODPS algorithm.
!
! CALLING SEQUENCE:
!       CALL ODPS_Assemble_Predictors( &
!              TC       , &  ! Input
!              Atm      , &  ! Input
!              GeoInfo  , &  ! Input
!              Predictor  )  ! Output
!
! INPUT ARGUMENTS:
!       TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atm       :     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       GeoInfo     :   CRTM_GeometryInfo structure containing the
!                       view geometry information.
!                       UNITS:      N/A
!                       TYPE:       CRTM_GeometryInfo_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE ODPS_Assemble_Predictors( &
    TC       , &
    Atm      , &
    GeoInfo  , &
    Predictor  )
    ! Arguments
    TYPE(ODPS_type)             , INTENT(IN)     :: TC
    TYPE(CRTM_Atmosphere_type)  , INTENT(IN)     :: Atm
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeoInfo
    TYPE(ODPS_Predictor_type)   , INTENT(IN OUT) :: Predictor
    ! Local variables
    REAL(fp) :: Temperature(Predictor%n_Layers)
    REAL(fp) :: Absorber(Predictor%n_Layers, TC%n_Absorbers)
    INTEGER  :: H2O_idx
    REAL(fp) :: Secant_Sensor_Zenith


    ! Map data from user to internal fixed pressure layers/levels
    CALL Map_Input( &
      Atm                            , &
      TC                             , &
      GeoInfo                        , &
      Temperature                    , &
      Absorber                       , &
      Predictor%User_Level_LnPressure, &
      Predictor%Ref_Level_LnPressure , &
      Predictor%Secant_Zenith        , &
      H2O_idx                        , &
      Predictor%PAFV)


    ! ...Store the surface secant zenith angle
    CALL CRTM_GeometryInfo_GetValue( GeoInfo, Secant_Trans_Zenith = Secant_Sensor_Zenith )
    Predictor%Secant_Zenith_Surface = Secant_Sensor_Zenith


    ! Compute predictor
    CALL ODPS_Compute_Predictor( &
      TC%Group_index         , &
      Temperature            , &
      Absorber               , &
      TC%Ref_Level_Pressure  , &
      TC%Ref_Temperature     , &
      TC%Ref_Absorber        , &
      Predictor%Secant_Zenith, &
      Predictor                )
    ! ...Optional ODAS for water vapour lines
    IF( ALLOW_OPTRAN .AND. TC%n_OCoeffs > 0 )THEN
      CALL ODPS_Compute_Predictor_ODAS( &
        Temperature            , &
        Absorber(:,H2O_idx)    , &
        TC%Ref_Level_Pressure  , &
        TC%Ref_Pressure        , &
        Predictor%Secant_Zenith, &
        TC%Alpha               , &
        TC%Alpha_C1            , &
        TC%Alpha_C2            , &
        Predictor                )
    END IF
    ! ...Save the interpolation indices
    IF ( PAFV_Associated(Predictor%PAFV) ) THEN
      CALL Compute_Interp_Index( &
        Predictor%Ref_Level_LnPressure ,  &
        Predictor%User_Level_LnPressure,  &
        Predictor%PAFV%ODPS2User_Idx)
    END IF

  END SUBROUTINE ODPS_Assemble_Predictors

!--------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Assemble_Predictors_TL
!
! PURPOSE:
!       Subroutine to assemble all the tangent-linear gas absorption model
!       predictors.
!       It first interpolates the user temperature and absorber profiles on the
!       internal pressure grids and then calls the predictor computation routine
!       to compute the predictors
!
! CALLING SEQUENCE:
!       CALL ODPS_Assemble_Predictors_TL( &
!         TC          , &
!         Predictor   , &
!         Atm_TL      , &
!         Predictor_TL  )
!
! INPUT ARGUMENTS:
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atm_TL    :     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:   Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE ODPS_Assemble_Predictors_TL( &
    TC          , &  ! Input
    Predictor   , &  ! Input
    Atm_TL      , &  ! Input
    Predictor_TL  )  ! Output
    ! Arguments
    TYPE(ODPS_type)           , INTENT(IN)     :: TC
    TYPE(ODPS_Predictor_type) , INTENT(IN)     :: Predictor
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_TL
    TYPE(ODPS_Predictor_type) , INTENT(IN OUT) :: Predictor_TL
    ! Local variables
    REAL(fp) :: Absorber_TL(Predictor%n_Layers, TC%n_Absorbers)
    REAL(fp) :: Temperature_TL(Predictor%n_Layers)


    ! Map data from user to internal fixed pressure layers/levels
    CALL Map_Input_TL( &
      TC            , &
      Atm_TL        , &
      Temperature_TL, &
      Absorber_TL   , &
      Predictor%PAFV  )


    ! Compute predictor
    CALL ODPS_Compute_Predictor_TL( &
      TC%Group_index            , &
      Predictor%PAFV%Temperature, &
      Predictor%PAFV%Absorber   , &
      TC%Ref_Temperature        , &
      TC%Ref_Absorber           , &
      Predictor%Secant_Zenith   , &
      Predictor                 , &
      Temperature_TL            , &
      Absorber_TL               , &
      Predictor_TL                )
    ! ...Optional ODAS for water vapour lines
    IF ( ALLOW_OPTRAN .AND. TC%n_OCoeffs > 0 ) THEN
      CALL ODPS_Compute_Predictor_ODAS_TL( &
        Predictor%PAFV%Temperature                       , &
        Predictor%PAFV%Absorber(:,Predictor%PAFV%H2O_idx), &
        TC%Ref_Pressure                                  , &
        Predictor%Secant_Zenith                          , &
        TC%Alpha                                         , &
        TC%Alpha_C2                                      , &
        Predictor                                        , &
        Temperature_TL                                   , &
        Absorber_TL(:,Predictor%PAFV%H2O_idx)            , &
        Predictor_TL                                       )
    END IF

  END SUBROUTINE ODPS_Assemble_Predictors_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Assemble_Predictors_AD
!
! PURPOSE:
!       Subroutine to assemble the adjoint of the gas absorption model
!       predictors.
!       It first calss the adjoint of the predictor computation routine and
!       then performs the adjoint interpolation of the user temperature and
!       absorber profiles on the internal pressure grid.
!
! CALLING SEQUENCE:
!       CALL ODPS_Assemble_Predictors_AD( &
!         TC          , &
!         Predictor   , &
!         Predictor_AD, &
!         Atm_AD        )
!
! INPUT ARGUMENTS:
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:   Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!
!       Atm_AD    :     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE ODPS_Assemble_Predictors_AD( &
    TC          , &
    Predictor   , &
    Predictor_AD, &
    Atm_AD        )
    ! Arguments
    TYPE(ODPS_type)           , INTENT(IN)     :: TC
    TYPE(ODPS_Predictor_type) , INTENT(IN)     :: Predictor
    TYPE(ODPS_Predictor_type) , INTENT(IN OUT) :: predictor_AD
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_AD
    ! Local variables
    REAL(fp) :: Absorber_AD(Predictor%n_Layers, TC%n_Absorbers)
    REAL(fp) :: Temperature_AD(Predictor%n_Layers)

    ! Initialise local adjoint variables
    Temperature_AD = ZERO
    Absorber_AD    = ZERO


    ! Compute predictor
    ! ...Optional ODAS for water vapour lines
    IF ( ALLOW_OPTRAN .AND. TC%n_OCoeffs > 0 ) THEN
      CALL ODPS_Compute_Predictor_ODAS_AD( &
        Predictor%PAFV%Temperature                       , &
        Predictor%PAFV%Absorber(:,Predictor%PAFV%H2O_idx), &
        TC%Ref_Pressure                                  , &
        Predictor%Secant_Zenith                          , &
        TC%Alpha                                         , &
        TC%Alpha_C2                                      , &
        Predictor                                        , &
        Predictor_AD                                     , &
        Temperature_AD                                   , &
        Absorber_AD(:,Predictor%PAFV%H2O_idx)              )
    END IF
    ! ...The main ODPS predictor
    CALL ODPS_Compute_Predictor_AD( &
      TC%Group_index            , &
      Predictor%PAFV%Temperature, &
      Predictor%PAFV%Absorber   , &
      TC%Ref_Temperature        , &
      TC%Ref_Absorber           , &
      Predictor%Secant_Zenith   , &
      Predictor                 , &
      Predictor_AD              , &
      Temperature_AD            , &
      Absorber_AD                 )


    ! Map data from user to internal fixed pressure layers/levels
    CALL Map_Input_AD( &
      TC            , &
      Temperature_AD, &
      Absorber_AD   , &
      Atm_AD        , &
      Predictor%PAFV  )

  END SUBROUTINE ODPS_Assemble_Predictors_AD


!------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Compute_Predictor
!
! PURPOSE:
!       Subroutine to predictors
!
! CALLING SEQUENCE:
!       CALL ODPS_Compute_Predictor( &
!         Group_ID,           &  ! Input
!         Temperature,        &  ! Input
!         Absorber,           &  ! Input
!         Ref_Level_Pressure, &  ! Input
!         Ref_Temperature,    &  ! Input
!         Ref_Absorber,       &  ! Input
!         secang,             &  ! Input
!         Predictor )            ! Output
!
! INPUT ARGUMENTS:
!       Group_ID   :     The ID of predictor group
!                        UNITS:      N?A
!                        TYPE:       INTEGER
!                        DIMENSION:  scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       Absorber   :     Absorber profiles
!                        UNITS:      vary
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-2(n_Layers x n_Absorbers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       Ref_Level_Pressure : Reference level pressure profile
!                        UNITS:      hPa
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(0:n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       Ref_Temperature : Reference layer temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       Ref_Absorber :   Reference absorber profiles
!                        UNITS:      vary
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-2(n_Layers x n_Absorbers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       secang       :   Secont sensor zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_Predictor( &
    Group_ID,           &
    Temperature,        &
    Absorber,           &
    Ref_Level_Pressure, &
    Ref_Temperature,    &
    Ref_Absorber,       &
    secang,             &
    Predictor )

    INTEGER,                   INTENT(IN)     :: Group_ID
    REAL(fp),                  INTENT(IN)     :: Temperature(:)
    REAL(fp),                  INTENT(IN)     :: Absorber(:, :)
    REAL(fp),                  INTENT(IN)     :: Ref_Level_Pressure(0:)
    REAL(fp),                  INTENT(IN)     :: Ref_Temperature(:)
    REAL(fp),                  INTENT(IN)     :: Ref_Absorber(:, :)
    REAL(fp),                  INTENT(IN)     :: Secang(:)
    TYPE(ODPS_Predictor_type), INTENT(IN OUT) :: Predictor

    ! ---------------
    ! Local variables
    ! ---------------
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ODPS_Compute_Predictor'
    INTEGER    ::    n_layers
    INTEGER    ::    k ! n_Layers, n_Levels
    INTEGER    ::    j ! n_absorbers
    REAL(fp) ::    PDP
    REAL(fp) ::    Tzp_ref
    REAL(fp) ::    Tzp_sum
    REAL(fp) ::    Tzp(SIZE(Absorber, DIM=1))
    REAL(fp) ::    Tz_ref
    REAL(fp) ::    Tz_sum
    REAL(fp) ::    Tz(SIZE(Absorber, DIM=1))
    REAL(fp) ::    GAz_ref(SIZE(Absorber, DIM=2))
    REAL(fp) ::    GAz_sum(SIZE(Absorber, DIM=2))
    REAL(fp) ::    GAz(SIZE(Absorber, DIM=1), SIZE(Absorber, DIM=2))
    REAL(fp) ::    GAzp_ref(SIZE(Absorber, DIM=2))
    REAL(fp) ::    GAzp_sum(SIZE(Absorber, DIM=2))
    REAL(fp) ::    GAzp(SIZE(Absorber, DIM=1), SIZE(Absorber, DIM=2))
    REAL(fp) ::    GATzp_ref(SIZE(Absorber, DIM=2))
    REAL(fp) ::    GATzp_sum (SIZE(Absorber, DIM=2))
    REAL(fp) ::    GATzp(SIZE(Absorber, DIM=1), SIZE(Absorber, DIM=2))

    n_Layers = Predictor%n_Layers

    Predictor%Secant_Zenith = Secang

    !------------------------------------
    ! Compute integrated variables
    !------------------------------------

    Tzp_ref   = ZERO
    Tzp_sum   = ZERO
    Tz_ref    = ZERO
    Tz_sum    = ZERO
    GAz_ref   = ZERO
    GAz_sum   = ZERO
    GAzp_ref  = ZERO
    GAzp_sum  = ZERO
    GATzp_ref = ZERO
    GATzp_sum = ZERO

    Layer_Loop : DO k = 1, n_Layers

      ! weight for integrated variables
      if(k == 1)then
            PDP = Ref_Level_Pressure(0) * &
                 ( Ref_Level_Pressure(1) - Ref_Level_Pressure(0) )

      else
            PDP = Ref_Level_Pressure(k) * &
                 ( Ref_Level_Pressure(k) - Ref_Level_Pressure(k-1) )

      endif

      ! Temperature
      Tz_ref  = Tz_ref + Ref_Temperature(k)
      Tz_sum  = Tz_sum + Temperature(k)
      Tz(k)   = Tz_sum / Tz_ref
      Tzp_ref = Tzp_ref + PDP * Ref_Temperature(k)
      Tzp_sum = Tzp_sum + PDP*Temperature(k)
      Tzp(k)  = Tzp_sum/Tzp_ref

      ! absorbers
      DO j = 1, N_ABSORBERS_G(Group_ID)
        GAz_ref(j)   = GAz_ref(j) + Ref_absorber(k, j)
        GAz_sum(j)   = GAz_sum(j) + Absorber(k, j)
        GAz(k, j)    = GAz_sum(j) / GAz_ref(j)
        GAzp_ref(j)  = GAzp_ref(j) + PDP*Ref_absorber(k, j)
        GAzp_sum(j)  = GAzp_sum(j) + PDP*Absorber(k, j)
        GAzp(k, j)   = GAzp_sum(j) / GAzp_ref(j)
        GATzp_ref(j) = GATzp_ref(j) + PDP*Ref_absorber(k, j)*Ref_Temperature(k)
        GATzp_sum(j) = GATzp_sum(j) + PDP*Absorber(k, j)*Temperature(k)
        GATzp(k, j)  = GATzp_sum(j) / GATzp_ref(j)
      END DO

      ! save FW variables for TL and AD routines
      IF ( PAFV_Associated(Predictor%PAFV) ) THEN
        Predictor%PAFV%PDP(k)          = PDP
        Predictor%PAFV%Tz_ref(k)       = Tz_ref
        Predictor%PAFV%Tz(k)           = Tz(k)
        Predictor%PAFV%Tzp_ref(k)      = Tzp_ref
        Predictor%PAFV%Tzp(k)          = Tzp(k)
        Predictor%PAFV%GAz_ref(k, :)   = GAz_ref
        Predictor%PAFV%GAz_sum(k, :)   = GAz_sum
        Predictor%PAFV%GAz(k, :)       = GAz(k, :)
        Predictor%PAFV%GAzp_ref(k, :)  = GAzp_ref
        Predictor%PAFV%GAzp_sum(k, :)  = GAzp_sum
        Predictor%PAFV%GAzp(k, :)      = GAzp(k, :)
        Predictor%PAFV%GATzp_ref(k, :) = GATzp_ref
        Predictor%PAFV%GATzp_sum(k, :) = GATzp_sum
        Predictor%PAFV%GATzp(k, :)     = GATzp(k, :)
      END IF

    END DO Layer_Loop

    !----------------------------------------------------------------
    ! Call the group specific routine for remaining computation; all
    ! variables defined above are passed to the called routine
    !----------------------------------------------------------------

    SELECT CASE( Group_ID )
      CASE( GROUP_1, GROUP_2 )
         CALL ODPS_Compute_Predictor_IR()
      CASE( GROUP_3 )
         CALL ODPS_Compute_Predictor_MW()
    END SELECT

CONTAINS

    SUBROUTINE ODPS_Compute_Predictor_IR()

      ! ---------------
      ! Local variables
      ! ---------------
      INTEGER    ::    k ! n_Layers, n_Levels
      REAL(fp) ::    DT
      REAL(fp) ::    T
      REAL(fp) ::    T2
      REAL(fp) ::    DT2
      REAL(fp) ::    H2O
      REAL(fp) ::    H2O_A
      REAL(fp) ::    H2O_R
      REAL(fp) ::    H2O_S
      REAL(fp) ::    H2O_R4
      REAL(fp) ::    H2OdH2OTzp
      REAL(fp) ::    CO2
      REAL(fp) ::    O3
      REAL(fp) ::    O3_A
      REAL(fp) ::    O3_R
      REAL(fp) ::    CO
      REAL(fp) ::    CO_A
      REAL(fp) ::    CO_R
      REAL(fp) ::    CO_S
      REAL(fp) ::    CO_ACOdCOzp
      REAL(fp) ::    N2O
      REAL(fp) ::    N2O_A
      REAL(fp) ::    N2O_R
      REAL(fp) ::    N2O_S
      REAL(fp) ::    CH4
      REAL(fp) ::    CH4_A
      REAL(fp) ::    CH4_R
      REAL(fp) ::    CH4_ACH4zp

      Layer_Loop : DO k = 1, n_Layers

        !------------------------------------------
        !  Relative Temperature
        !------------------------------------------
        dT = Temperature(k) - Ref_Temperature(k)
        T = Temperature(k) / Ref_Temperature(k)

        !-------------------------------------------
        !  Abosrber amount scalled by the reference
        !-------------------------------------------
        H2O = Absorber(k,ABS_H2O_IR)/Ref_Absorber(k, ABS_H2O_IR)
        O3  = Absorber(k,ABS_O3_IR)/Ref_absorber(k,ABS_O3_IR)
        CO2 = Absorber(k,ABS_CO2_IR)/Ref_absorber(k,ABS_CO2_IR)

        ! Combinations of variables common to all predictor groups
        T2   = T*T
        DT2  = DT*ABS( DT )

        H2O_A = SECANG(k)*H2O
        H2O_R  = SQRT( H2O_A )
        H2O_S  = H2O_A*H2O_A
        H2O_R4 = SQRT( H2O_R )
        H2OdH2OTzp = H2O/GATzp(k, ABS_H2O_IR)

        O3_A = SECANG(k)*O3
        O3_R = SQRT( O3_A )

        IF( Group_ID == GROUP_1 )THEN
          CO  = Absorber(k,ABS_CO_IR)/Ref_absorber(k, ABS_CO_IR)
          N2O = Absorber(k,ABS_N2O_IR)/Ref_absorber(k,ABS_N2O_IR)
          CH4 = Absorber(k,ABS_CH4_IR)/Ref_absorber(k,ABS_CH4_IR)

          N2O_A = SECANG(k)*N2O
          N2O_R = SQRT( N2O_A )
          N2O_S = N2O_A*N2O_A

          CO_A = SECANG(k)*CO
          CO_R = SQRT( CO_A )
          CO_S = CO_A*CO_A
          CO_ACOdCOzp = CO_A*CO/GAzp(k, ABS_CO_IR)

          CH4_A = SECANG(k)*CH4
          CH4_R = SQRT(CH4_A)
          CH4_ACH4zp = SECANG(k)*GAzp(k, ABS_CH4_IR)

          ! set number of predictors
          Predictor%n_CP = N_PREDICTORS_G1
        ELSE
          Predictor%n_CP = N_PREDICTORS_G2
        END IF

       !#-------------------------------------------------------------------#
       !#        -- Predictors --                                           #
       !#-------------------------------------------------------------------#

        !  ----------------------
        !   Fixed (Dry) predictors
        !  ----------------------
        Predictor%X(k, 1, COMP_DRY_IR)  = SECANG(k)
        Predictor%X(k, 2, COMP_DRY_IR)  = SECANG(k) * T
        Predictor%X(k, 3, COMP_DRY_IR)  = SECANG(k) * T2
        Predictor%X(k, 4, COMP_DRY_IR)  = T
        Predictor%X(k, 5, COMP_DRY_IR)  = SECANG(k) * SECANG(k)
        Predictor%X(k, 6, COMP_DRY_IR)  = T2
        Predictor%X(k, 7, COMP_DRY_IR)  = Tz(k)

       !  --------------------------
       !  Water vapor continuum predictors
       !  --------------------------
        Predictor%X(k, 1, COMP_WCO_IR) = H2O_A/T
        Predictor%X(k, 2, COMP_WCO_IR) = H2O_A/T * H2O
        Predictor%X(k, 3, COMP_WCO_IR) = H2O_A/T2 * H2O/T2
        Predictor%X(k, 4, COMP_WCO_IR) = H2O_A/T2
        Predictor%X(k, 5, COMP_WCO_IR) = H2O_A/T2 * H2O
        Predictor%X(k, 6, COMP_WCO_IR) = H2O_A/T2**2
        Predictor%X(k, 7, COMP_WCO_IR) = H2O_A

        !  -----------------------
        !  Ozone predictors
        !  -----------------------
        Predictor%X(k, 1, COMP_OZO_IR)  = O3_A
        Predictor%X(k, 2, COMP_OZO_IR)  = O3_A*DT
        Predictor%X(k, 3, COMP_OZO_IR)  = O3_A*O3*GAzp(k,ABS_O3_IR)
        Predictor%X(k, 4, COMP_OZO_IR)  = O3_A*O3_A
        Predictor%X(k, 5, COMP_OZO_IR)  = O3_A*GAzp(k,ABS_O3_IR)
        Predictor%X(k, 6, COMP_OZO_IR)  = O3_A*SQRT(SECANG(k)*GAzp(k,ABS_O3_IR))
        Predictor%X(k, 7, COMP_OZO_IR)  = O3_R*DT   !T*T*T
        Predictor%X(k, 8, COMP_OZO_IR)  = O3_R
        Predictor%X(k, 9, COMP_OZO_IR)  = O3_R*O3/GAzp(k,ABS_O3_IR)
        Predictor%X(k,10, COMP_OZO_IR)  = SECANG(k)*GAzp(k,ABS_O3_IR)
        Predictor%X(k,11, COMP_OZO_IR)  = (SECANG(k)*GAzp(k,ABS_O3_IR))**2

!        Predictor%X(k, 12, COMP_OZO_IR)  = H2O_A
!        Predictor%X(k, 13, COMP_OZO_IR)  = SECANG(k)*GAzp(k,ABS_H2O_IR)

        !  -----------------------
        !  Carbon dioxide predictors
        !  -----------------------
        Predictor%X(k, 1, COMP_CO2_IR)  = SECANG(k) * T
        Predictor%X(k, 2, COMP_CO2_IR)  = SECANG(k) * T2
        Predictor%X(k, 3, COMP_CO2_IR)  = T
        Predictor%X(k, 4, COMP_CO2_IR)  = T2
        Predictor%X(k, 5, COMP_CO2_IR)  = SECANG(k)
        Predictor%X(k, 6, COMP_CO2_IR)  = SECANG(k)*CO2
        Predictor%X(k, 7, COMP_CO2_IR)  = SECANG(k) * Tzp(k)
        Predictor%X(k, 8, COMP_CO2_IR)  = (SECANG(k) * GAzp(k, ABS_CO2_IR))**2
        Predictor%X(k, 9, COMP_CO2_IR)  = Tzp(k)**3
        Predictor%X(k, 10, COMP_CO2_IR) = SECANG(k) * Tzp(k) * SQRT(T)

        !  --------------------------
        !  Water-line predictors
        !  --------------------------
        Predictor%X(k, 1, COMP_WLO_IR) = H2O_A
        Predictor%X(k, 2, COMP_WLO_IR) = H2O_A*DT
        Predictor%X(k, 3, COMP_WLO_IR) = H2O_S
        Predictor%X(k, 4, COMP_WLO_IR) = H2O_A*DT2
        Predictor%X(k, 5, COMP_WLO_IR) = H2O_R4
        Predictor%X(k, 6, COMP_WLO_IR) = H2O_S*H2O_A
        Predictor%X(k, 7, COMP_WLO_IR) = H2O_R
        Predictor%X(k, 8, COMP_WLO_IR) = H2O_R*DT
        Predictor%X(k, 9, COMP_WLO_IR) = H2O_S*H2O_S
        Predictor%X(k,10, COMP_WLO_IR) = H2OdH2OTzp
        Predictor%X(k,11, COMP_WLO_IR) = H2O_R*H2OdH2OTzp
        Predictor%X(k,12, COMP_WLO_IR) = (SECANG(k)*GAzp(k,ABS_H2O_IR))**2
        Predictor%X(k,13, COMP_WLO_IR) = SECANG(k)*GAzp(k,ABS_H2O_IR)
        Predictor%X(k,14, COMP_WLO_IR) = SECANG(k)
        Predictor%X(k,15, COMP_WLO_IR) = SECANG(k) * CO2

        ! Addtional predictors for group 1
        IF_Group1: IF( Group_ID == GROUP_1 )THEN

          Predictor%X(k, 11, COMP_CO2_IR)  = CO_A

          Predictor%X(k, 16, COMP_WLO_IR) = CH4_A
          Predictor%X(k, 17, COMP_WLO_IR) = CH4_A*CH4_A*DT
          Predictor%X(k, 18, COMP_WLO_IR) = CO_A

          !  -----------------------
          !  Carbon monoxide
          !  -----------------------
          Predictor%X(k, 1,  COMP_CO_IR)   = CO_A
          Predictor%X(k, 2,  COMP_CO_IR)   = CO_A*DT
          Predictor%X(k, 3,  COMP_CO_IR)   = SQRT( CO_R )
          Predictor%X(k, 4,  COMP_CO_IR)   = CO_R*DT
          Predictor%X(k, 5,  COMP_CO_IR)   = CO_S
          Predictor%X(k, 6,  COMP_CO_IR)   = CO_R
          Predictor%X(k, 7,  COMP_CO_IR)   = CO_A*DT2
          Predictor%X(k, 8,  COMP_CO_IR)   = CO_ACOdCOzp
          Predictor%X(k, 9,  COMP_CO_IR)   = CO_ACOdCOzp/CO_R
          Predictor%X(k, 10, COMP_CO_IR)   = CO_ACOdCOzp * SQRT( GAzp(k, ABS_CO_IR) )

          !  -----------------------
          !  Methane predictors
          !  -----------------------
          Predictor%X(k, 1,  COMP_CH4_IR)  = CH4_A*DT
          Predictor%X(k, 2,  COMP_CH4_IR)  = CH4_R
          Predictor%X(k, 3,  COMP_CH4_IR)  = CH4_A*CH4_A
          Predictor%X(k, 4,  COMP_CH4_IR)  = CH4_A
          Predictor%X(k, 5,  COMP_CH4_IR)  = CH4*DT
          Predictor%X(k, 6,  COMP_CH4_IR)  = CH4_ACH4zp
          Predictor%X(k, 7,  COMP_CH4_IR)  = CH4_ACH4zp**2
          Predictor%X(k, 8,  COMP_CH4_IR)  = SQRT(CH4_R)
          Predictor%X(k, 9,  COMP_CH4_IR)  = GATzp(k, ABS_CH4_IR)
          Predictor%X(k, 10, COMP_CH4_IR)  = SECANG(k)*GATzp(k, ABS_CH4_IR)
          Predictor%X(k, 11, COMP_CH4_IR)  = CH4_R * CH4/GAzp(k, ABS_CH4_IR)

          !  -----------------------
          !    N2O predictors
          !  -----------------------
          Predictor%X(k, 1, COMP_N2O_IR)   = N2O_A*DT
          Predictor%X(k, 2, COMP_N2O_IR)   = N2O_R
          Predictor%X(k, 3, COMP_N2O_IR)   = N2O*DT
          Predictor%X(k, 4, COMP_N2O_IR)   = N2O_A**POINT_25
          Predictor%X(k, 5, COMP_N2O_IR)   = N2O_A
          Predictor%X(k, 6, COMP_N2O_IR)   = SECANG(k) * GAzp(k, ABS_N2O_IR)
          Predictor%X(k, 7, COMP_N2O_IR)   = SECANG(k) * GATzp(k, ABS_N2O_IR)
          Predictor%X(k, 8, COMP_N2O_IR)   = N2O_S
          Predictor%X(k, 9, COMP_N2O_IR)   = GATzp(k, ABS_N2O_IR)
          Predictor%X(k,10, COMP_N2O_IR)   = N2O_R*N2O / GAzp(k, ABS_N2O_IR)

          Predictor%X(k,11, COMP_N2O_IR)   = CH4_A
          Predictor%X(k,12, COMP_N2O_IR)   = CH4_A*GAzp(k, ABS_CH4_IR)
          Predictor%X(k,13, COMP_N2O_IR)   = CO_A
          Predictor%X(k,14, COMP_N2O_IR)   = CO_A*SECANG(k)*GAzp(k, ABS_CO_IR)

        END IF IF_Group1

      END DO Layer_Loop

    END SUBROUTINE ODPS_Compute_Predictor_IR

    SUBROUTINE ODPS_Compute_Predictor_MW()

      ! ---------------
      ! Local variables
      ! ---------------
      INTEGER    ::    k ! n_Layers, n_Levels
      REAL(fp) ::    DT
      REAL(fp) ::    T
      REAL(fp) ::    T2
      REAL(fp) ::    DT2
      REAL(fp) ::    H2O
      REAL(fp) ::    H2O_A
      REAL(fp) ::    H2O_R
      REAL(fp) ::    H2O_S
      REAL(fp) ::    H2O_R4
      REAL(fp) ::    H2OdH2OTzp

      Layer_Loop : DO k = 1, n_Layers

        !------------------------------------------
        !  Relative Temperature
        !------------------------------------------
        dT = Temperature(k) - Ref_Temperature(k)
        T = Temperature(k) / Ref_Temperature(k)

        !-------------------------------------------
        !  Abosrber amount scalled by the reference
        !-------------------------------------------
        H2O = Absorber(k,ABS_H2O_MW)/Ref_Absorber(k, ABS_H2O_MW)

        ! Combinations of variables common to all predictor groups
        T2  = T*T
        DT2 = DT*ABS( DT )

        H2O_A = SECANG(k)*H2O
        H2O_R  = SQRT( H2O_A )
        H2O_S  = H2O_A*H2O_A
        H2O_R4 = SQRT( H2O_R )
        H2OdH2OTzp = H2O/GATzp(k, ABS_H2O_MW)

       !#-------------------------------------------------------------------#
       !#        -- Predictors --                                           #
       !#-------------------------------------------------------------------#

       ! set number of predictors
        Predictor%n_CP = N_PREDICTORS_G3

        !  ----------------------
        !   Fixed (Dry) predictors
        !  ----------------------
        Predictor%X(k, 1, COMP_DRY_MW)  = SECANG(k)
        Predictor%X(k, 2, COMP_DRY_MW)  = SECANG(k) * T
        Predictor%X(k, 3, COMP_DRY_MW)  = SECANG(k) * T2
        Predictor%X(k, 4, COMP_DRY_MW)  = T
        Predictor%X(k, 5, COMP_DRY_MW)  = SECANG(k) * SECANG(k)
        Predictor%X(k, 6, COMP_DRY_MW)  = T2
        Predictor%X(k, 7, COMP_DRY_MW)  = Tz(k)

       !  --------------------------------
       !  Water vapor (line and continuum)
       !  --------------------------------
        Predictor%X(k, 1, COMP_WET_MW) = H2O_A/T
        Predictor%X(k, 2, COMP_WET_MW) = H2O_A/T * H2O
        Predictor%X(k, 3, COMP_WET_MW) = H2O_A/T2 * H2O/T2
        Predictor%X(k, 4, COMP_WET_MW) = H2O_A/T2
        Predictor%X(k, 5, COMP_WET_MW) = H2O_A/T2 * H2O
        Predictor%X(k, 6, COMP_WET_MW) = H2O_A/T2**2
        Predictor%X(k, 7, COMP_WET_MW) = H2O_A
        Predictor%X(k, 8, COMP_WET_MW) = H2O_A*DT
        Predictor%X(k, 9, COMP_WET_MW) = (SECANG(k)*GAzp(k,ABS_H2O_MW))**2
        Predictor%X(k, 10,COMP_WET_MW) = SECANG(k)*GAzp(k,ABS_H2O_MW)
        Predictor%X(k, 11,COMP_WET_MW) = SECANG(k)
        Predictor%X(k, 12,COMP_WET_MW) = H2O_S*H2O_A
        Predictor%X(k, 13,COMP_WET_MW) = H2O_S*H2O_S
        Predictor%X(k, 14,COMP_WET_MW) = H2OdH2OTzp

      END DO Layer_Loop

    END SUBROUTINE ODPS_Compute_Predictor_MW

  END SUBROUTINE  ODPS_Compute_Predictor

!============================== TL
!------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Compute_Predictor_TL
!
! PURPOSE:
!       Subroutine to predictors
!
! CALLING SEQUENCE:
!       CALL ODPS_Compute_Predictor_TL( &
!         Group_ID,           &  ! Input
!         Temperature,        &  ! Input
!         Absorber,           &  ! Input
!         Ref_Temperature,    &  ! Input
!         Ref_Absorber,       &  ! Input
!         secang,             &  ! Input
!         Predictor           &  ! Input
!         Temperature_TL,     &  ! Input
!         Absorber_TL,        &  ! Input
!         Predictor_TL )         ! Output
!
! INPUT ARGUMENTS:
!       Group_ID   :     The ID of predictor group
!                        UNITS:      N?A
!                        TYPE:       INTEGER
!                        DIMENSION:  scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       Absorber   :     Absorber profiles
!                        UNITS:      vary
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-2(n_Layers x n_Absorbers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       Ref_Temperature : Reference layer temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       Ref_Absorber :   Reference absorber profiles
!                        UNITS:      vary
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-2(n_Layers x n_Absorbers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       secang       :   Secont sensor zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:  Temperature_TL profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       Absorber_TL:     Absorber_TL profiles
!                        UNITS:      vary
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-2(n_Layers x n_Absorbers) array
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!
!       Predictor_TL:   Predictor_TL structure containing the integrated absorber_TL
!                       and predictor_TL profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_Predictor_TL( &
    Group_ID,           &
    Temperature,        &
    Absorber,           &
    Ref_Temperature,    &
    Ref_Absorber,       &
    secang,             &
    Predictor,          &
    Temperature_TL,     &
    Absorber_TL,        &
    Predictor_TL )

    INTEGER,                           INTENT(IN)     :: Group_ID
    REAL(fp),                          INTENT(IN)     :: Temperature(:)
    REAL(fp),                          INTENT(IN)     :: Absorber(:, :)
    REAL(fp),                          INTENT(IN)     :: Ref_Temperature(:)
    REAL(fp),                          INTENT(IN)     :: Ref_Absorber(:, :)
    REAL(fp),                          INTENT(IN)     :: Secang(:)
    REAL(fp),                          INTENT(IN)     :: Temperature_TL(:)
    REAL(fp),                          INTENT(IN)     :: Absorber_TL(:, :)
    TYPE(ODPS_Predictor_type), TARGET, INTENT(IN)     :: Predictor
    TYPE(ODPS_Predictor_type),         INTENT(IN OUT) :: Predictor_TL

    ! ---------------
    ! Local variables
    ! ---------------
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ODPS_Compute_Predictor_TL'
    INTEGER    ::    n_layers
    INTEGER    ::    k ! n_Layers, n_Levels
    INTEGER    ::    j ! n_absorbers
    REAL(fp) ::    Tzp_sum_TL
    REAL(fp) ::    Tzp_TL(SIZE(Absorber, DIM=1))
    REAL(fp) ::    Tz_sum_TL
    REAL(fp) ::    Tz_TL(SIZE(Absorber, DIM=1))
    REAL(fp) ::    GAz_sum_TL(SIZE(Absorber, DIM=2))
    REAL(fp) ::    GAz_TL(SIZE(Absorber, DIM=1), SIZE(Absorber, DIM=2))
    REAL(fp) ::    GAzp_sum_TL(SIZE(Absorber, DIM=2))
    REAL(fp) ::    GAzp_TL(SIZE(Absorber, DIM=1), SIZE(Absorber, DIM=2))
    REAL(fp) ::    GATzp_sum_TL(SIZE(Absorber, DIM=2))
    REAL(fp) ::    GATzp_TL(SIZE(Absorber, DIM=1), SIZE(Absorber, DIM=2))
    TYPE(PAFV_type), POINTER  :: PAFV => NULL()

    ! use short name
    PAFV => Predictor%PAFV

    n_Layers = Predictor%n_Layers

    Predictor_TL%Secant_Zenith = Secang

    !------------------------------------
    ! Compute integrated variables
    !------------------------------------

    Tzp_sum_TL   = ZERO
    Tz_sum_TL    = ZERO
    GAz_sum_TL   = ZERO
    GAzp_sum_TL  = ZERO
    GATzp_sum_TL = ZERO

    Layer_Loop : DO k = 1, n_Layers

      ! Temperature
      Tz_sum_TL  = Tz_sum_TL + Temperature_TL(k)
      Tz_TL(k)   = Tz_sum_TL / PAFV%Tz_ref(k)
      Tzp_sum_TL = Tzp_sum_TL + PAFV%PDP(k)*Temperature_TL(k)
      Tzp_TL(k)  = Tzp_sum_TL/PAFV%Tzp_ref(k)

      ! absorbers
      DO j = 1, N_ABSORBERS_G(Group_ID)
        GAz_sum_TL(j)   = GAz_sum_TL(j) + Absorber_TL(k, j)
        GAz_TL(k, j)    = GAz_sum_TL(j) / PAFV%GAz_ref(k,j)
        GAzp_sum_TL(j)  = GAzp_sum_TL(j) + PAFV%PDP(k)*Absorber_TL(k, j)
        GAzp_TL(k, j)   = GAzp_sum_TL(j) / PAFV%GAzp_ref(k,j)
        GATzp_sum_TL(j) = GATzp_sum_TL(j) + PAFV%PDP(k)*Absorber_TL(k, j)*Temperature(k) + &
                          PAFV%PDP(k)*Absorber(k, j)*Temperature_TL(k)
        GATzp_TL(k, j)  = GATzp_sum_TL(j) / PAFV%GATzp_ref(k,j)
      END DO

    END DO Layer_Loop

    !----------------------------------------------------------------
    ! Call the group specific routine for remaining computation; all
    ! variables defined above are passed to the called routine
    !----------------------------------------------------------------

    SELECT CASE( Group_ID )
      CASE( GROUP_1, GROUP_2 )
         CALL ODPS_Compute_Predictor_IR_TL()
      CASE( GROUP_3 )
         CALL ODPS_Compute_Predictor_MW_TL()
    END SELECT

    NULLIFY(PAFV)

CONTAINS

    SUBROUTINE ODPS_Compute_Predictor_IR_TL()

      ! ---------------
      ! Local variables
      ! ---------------
      INTEGER    ::    k ! n_Layers, n_Levels
      REAL(fp) ::    DT,           DT_TL
      REAL(fp) ::    T,            T_TL
      REAL(fp) ::    T2,           T2_TL
      REAL(fp) ::    DT2,          DT2_TL
      REAL(fp) ::    H2O,          H2O_TL
      REAL(fp) ::    H2O_A,        H2O_A_TL
      REAL(fp) ::    H2O_R,        H2O_R_TL
      REAL(fp) ::    H2O_S,        H2O_S_TL
      REAL(fp) ::    H2O_R4,       H2O_R4_TL
      REAL(fp) ::    H2OdH2OTzp,   H2OdH2OTzp_TL
      REAL(fp) ::    CO2,          CO2_TL
      REAL(fp) ::    O3,           O3_TL
      REAL(fp) ::    O3_A,         O3_A_TL
      REAL(fp) ::    O3_R,         O3_R_TL
      REAL(fp) ::    CO,           CO_TL
      REAL(fp) ::    CO_A,         CO_A_TL
      REAL(fp) ::    CO_R,         CO_R_TL
      REAL(fp) ::    CO_S,         CO_S_TL
      REAL(fp) ::    CO_ACOdCOzp,  CO_ACOdCOzp_TL
      REAL(fp) ::    N2O,          N2O_TL
      REAL(fp) ::    N2O_A,        N2O_A_TL
      REAL(fp) ::    N2O_R,        N2O_R_TL
      REAL(fp) ::    N2O_S,        N2O_S_TL
      REAL(fp) ::    CH4,          CH4_TL
      REAL(fp) ::    CH4_A,        CH4_A_TL
      REAL(fp) ::    CH4_R,        CH4_R_TL
      REAL(fp) ::    CH4_ACH4zp,   CH4_ACH4zp_TL

      Layer_Loop : DO k = 1, n_Layers

        !------------------------------------------
        !  Relative Temperature
        !------------------------------------------
        dT = Temperature(k) - Ref_Temperature(k)
        T = Temperature(k) / Ref_Temperature(k)
        dT_TL = Temperature_TL(k)
        T_TL = Temperature_TL(k) / Ref_Temperature(k)

        !-------------------------------------------
        !  Abosrber amount scalled by the reference
        !-------------------------------------------
        H2O = Absorber(k,ABS_H2O_IR)/Ref_Absorber(k, ABS_H2O_IR)
        O3  = Absorber(k,ABS_O3_IR)/Ref_absorber(k,ABS_O3_IR)
        CO2 = Absorber(k,ABS_CO2_IR)/Ref_absorber(k,ABS_CO2_IR)

        H2O_TL = Absorber_TL(k,ABS_H2O_IR)/Ref_Absorber(k, ABS_H2O_IR)
        O3_TL  = Absorber_TL(k,ABS_O3_IR)/Ref_absorber(k,ABS_O3_IR)
        CO2_TL = Absorber_TL(k,ABS_CO2_IR)/Ref_absorber(k,ABS_CO2_IR)

        ! Combinations of variables common to all predictor groups
        T2   = T*T
        DT2  = DT*ABS( DT )

        T2_TL  = TWO*T*T_TL
        IF( DT > ZERO) THEN
          DT2_TL = TWO*DT*DT_TL
        ELSE
          DT2_TL = - TWO*DT*DT_TL
        ENDIF

        H2O_A  = SECANG(k)*H2O
        H2O_R  = SQRT( H2O_A )
        H2O_S  = H2O_A*H2O_A
        H2O_R4 = SQRT( H2O_R )
        H2OdH2OTzp = H2O/PAFV%GATzp(k, ABS_H2O_IR)

        H2O_A_TL  = SECANG(k)*H2O_TL
        H2O_R_TL  = (POINT_5 / SQRT(H2O_A)) * H2O_A_TL
        H2O_S_TL  = TWO * H2O_A * H2O_A_TL
        H2O_R4_TL = (POINT_5 / SQRT(H2O_R)) * H2O_R_TL
        H2OdH2OTzp_TL = H2O_TL/PAFV%GATzp(k, ABS_H2O_IR) - &
                        H2O * GATzp_TL(k, ABS_H2O_IR)/PAFV%GATzp(k, ABS_H2O_IR)**2

        O3_A = SECANG(k)*O3
        O3_R = SQRT( O3_A )

        O3_A_TL = SECANG(k)*O3_TL
        O3_R_TL = (POINT_5 / SQRT(O3_A)) * O3_A_TL

        IF( Group_ID == GROUP_1 )THEN
          CO  = Absorber(k,ABS_CO_IR)/Ref_absorber(k, ABS_CO_IR)
          N2O = Absorber(k,ABS_N2O_IR)/Ref_absorber(k,ABS_N2O_IR)
          CH4 = Absorber(k,ABS_CH4_IR)/Ref_absorber(k,ABS_CH4_IR)

          CO_TL  = Absorber_TL(k,ABS_CO_IR)/Ref_absorber(k, ABS_CO_IR)
          N2O_TL = Absorber_TL(k,ABS_N2O_IR)/Ref_absorber(k,ABS_N2O_IR)
          CH4_TL = Absorber_TL(k,ABS_CH4_IR)/Ref_absorber(k,ABS_CH4_IR)

          N2O_A = SECANG(k)*N2O
          N2O_R = SQRT( N2O_A )
          N2O_S = N2O_A*N2O_A

          N2O_A_TL = SECANG(k) * N2O_TL
          N2O_R_TL = (POINT_5 / SQRT(N2O_A)) * N2O_A_TL
          N2O_S_TL = TWO * N2O_A * N2O_A_TL

          CO_A = SECANG(k)*CO
          CO_R = SQRT( CO_A )
          CO_S = CO_A*CO_A
          CO_ACOdCOzp = CO_A*CO/PAFV%GAzp(k, ABS_CO_IR)

          CO_A_TL = SECANG(k)*CO_TL
          CO_R_TL = (POINT_5 / SQRT(CO_A)) * CO_A_TL
          CO_S_TL = TWO * CO_A * CO_A_TL
          CO_ACOdCOzp_TL = CO_A_TL*CO/PAFV%GAzp(k, ABS_CO_IR) + CO_A*CO_TL/PAFV%GAzp(k, ABS_CO_IR) &
                           - CO_A*CO*GAzp_TL(k, ABS_CO_IR)/PAFV%GAzp(k, ABS_CO_IR)**2

          CH4_A = SECANG(k)*CH4
          CH4_R = SQRT(CH4_A)
          CH4_ACH4zp = SECANG(k)*PAFV%GAzp(k, ABS_CH4_IR)

          CH4_A_TL = SECANG(k)*CH4_TL
          CH4_R_TL = (POINT_5 / SQRT(CH4_A)) * CH4_A_TL
          CH4_ACH4zp_TL = SECANG(k)*GAzp_TL(k, ABS_CH4_IR)

          ! set number of predictors
          Predictor_TL%n_CP = N_PREDICTORS_G1
        ELSE
          Predictor_TL%n_CP = N_PREDICTORS_G2
        END IF

       !#-------------------------------------------------------------------#
       !#        -- Predictors --                                           #
       !#-------------------------------------------------------------------#

        !  ----------------------
        !   Fixed (Dry) predictors
        !  ----------------------
        Predictor_TL%X(k, 1, COMP_DRY_IR)  = ZERO
        Predictor_TL%X(k, 2, COMP_DRY_IR)  = SECANG(k) * T_TL
        Predictor_TL%X(k, 3, COMP_DRY_IR)  = SECANG(k) * T2_TL
        Predictor_TL%X(k, 4, COMP_DRY_IR)  = T_TL
        Predictor_TL%X(k, 5, COMP_DRY_IR)  = ZERO
        Predictor_TL%X(k, 6, COMP_DRY_IR)  = T2_TL
        Predictor_TL%X(k, 7, COMP_DRY_IR)  = Tz_TL(k)

       !  --------------------------
       !  Water vapor continuum predictors
       !  --------------------------
        Predictor_TL%X(k, 1, COMP_WCO_IR) = H2O_A_TL/T - H2O_A * T_TL/T**2
        Predictor_TL%X(k, 2, COMP_WCO_IR) = H2O_A_TL*H2O/T + H2O_A*H2O_TL/T - H2O_A*H2O*T_TL/T**2
        Predictor_TL%X(k, 3, COMP_WCO_IR) = H2O_A_TL*H2O/T2**2 + H2O_A*H2O_TL/T2**2 - &
                                            Two*H2O_A*H2O*T2_TL/T2**3
        Predictor_TL%X(k, 4, COMP_WCO_IR) = H2O_A_TL/T2 - H2O_A * T2_TL/T2**2
        Predictor_TL%X(k, 5, COMP_WCO_IR) = H2O_A_TL*H2O/T2 + H2O_A*H2O_TL/T2 - H2O_A*H2O*T2_TL/T2**2
        Predictor_TL%X(k, 6, COMP_WCO_IR) = H2O_A_TL/T2**2 - TWO*H2O_A*T2_TL/T2**3
        Predictor_TL%X(k, 7, COMP_WCO_IR) = H2O_A_TL

        !  -----------------------
        !  Ozone predictors
        !  -----------------------
        Predictor_TL%X(k, 1, COMP_OZO_IR)  = O3_A_TL
        Predictor_TL%X(k, 2, COMP_OZO_IR)  = O3_A_TL*DT + O3_A*DT_TL
        Predictor_TL%X(k, 3, COMP_OZO_IR)  = O3_A_TL*O3*PAFV%GAzp(k,ABS_O3_IR) + O3_A*O3_TL*PAFV%GAzp(k,ABS_O3_IR) &
                                             + O3_A*O3*GAzp_TL(k,ABS_O3_IR)
        Predictor_TL%X(k, 4, COMP_OZO_IR)  = TWO*O3_A*O3_A_TL
        Predictor_TL%X(k, 5, COMP_OZO_IR)  = O3_A_TL*PAFV%GAzp(k,ABS_O3_IR) + O3_A*GAzp_TL(k,ABS_O3_IR)
        Predictor_TL%X(k, 6, COMP_OZO_IR)  = O3_A_TL*SQRT(SECANG(k)*PAFV%GAzp(k,ABS_O3_IR)) + &
                                             POINT_5*O3_A*SQRT(SECANG(k)/PAFV%GAzp(k,ABS_O3_IR))* &
                                             GAzp_TL(k,ABS_O3_IR)
        Predictor_TL%X(k, 7, COMP_OZO_IR)  = O3_R_TL*DT + O3_R*DT_TL  !T*T*T
        Predictor_TL%X(k, 8, COMP_OZO_IR)  = O3_R_TL
        Predictor_TL%X(k, 9, COMP_OZO_IR)  = O3_R_TL*O3/PAFV%GAzp(k,ABS_O3_IR) + O3_R*O3_TL/PAFV%GAzp(k,ABS_O3_IR) &
                                             - O3_R*O3*GAzp_TL(k,ABS_O3_IR)/PAFV%GAzp(k,ABS_O3_IR)**2
        Predictor_TL%X(k,10, COMP_OZO_IR)  = SECANG(k)*GAzp_TL(k,ABS_O3_IR)
        Predictor_TL%X(k,11, COMP_OZO_IR)  = TWO*SECANG(k)**2 * PAFV%GAzp(k,ABS_O3_IR)*GAzp_TL(k,ABS_O3_IR)
!        Predictor_TL%X(k, 12, COMP_OZO_IR)  = H2O_A_TL
!        Predictor_TL%X(k, 13, COMP_OZO_IR)  = SECANG(k)*GAzp_TL(k,ABS_H2O_IR)

        !  -----------------------
        !  Carbon dioxide predictors
        !  -----------------------
        Predictor_TL%X(k, 1, COMP_CO2_IR)  = SECANG(k) * T_TL
        Predictor_TL%X(k, 2, COMP_CO2_IR)  = SECANG(k) * T2_TL
        Predictor_TL%X(k, 3, COMP_CO2_IR)  = T_TL
        Predictor_TL%X(k, 4, COMP_CO2_IR)  = T2_TL
        Predictor_TL%X(k, 5, COMP_CO2_IR)  = ZERO
        Predictor_TL%X(k, 6, COMP_CO2_IR)  = SECANG(k)*CO2_TL
        Predictor_TL%X(k, 7, COMP_CO2_IR)  = SECANG(k)*Tzp_TL(k)
        Predictor_TL%X(k, 8, COMP_CO2_IR)  = TWO*SECANG(k)**2 * PAFV%GAzp(k, ABS_CO2_IR)* GAzp_TL(k, ABS_CO2_IR)
        Predictor_TL%X(k, 9, COMP_CO2_IR)  = THREE*PAFV%Tzp(k)**2*Tzp_TL(k)
        Predictor_TL%X(k, 10, COMP_CO2_IR) = SECANG(k)*( SQRT(T)*Tzp_TL(k) + (POINT_5*PAFV%Tzp(k)/SQRT(T))*T_TL )

        !  --------------------------
        !  Water-line predictors
        !  --------------------------
        Predictor_TL%X(k, 1, COMP_WLO_IR) = H2O_A_TL
        Predictor_TL%X(k, 2, COMP_WLO_IR) = H2O_A_TL*DT + H2O_A*DT_TL
        Predictor_TL%X(k, 3, COMP_WLO_IR) = H2O_S_TL
        Predictor_TL%X(k, 4, COMP_WLO_IR) = H2O_A_TL*DT2 + H2O_A*DT2_TL
        Predictor_TL%X(k, 5, COMP_WLO_IR) = H2O_R4_TL
        Predictor_TL%X(k, 6, COMP_WLO_IR) = H2O_S_TL*H2O_A + H2O_S*H2O_A_TL
        Predictor_TL%X(k, 7, COMP_WLO_IR) = H2O_R_TL
        Predictor_TL%X(k, 8, COMP_WLO_IR) = H2O_R_TL*DT + H2O_R*DT_TL
        Predictor_TL%X(k, 9, COMP_WLO_IR) = TWO*H2O_S*H2O_S_TL
        Predictor_TL%X(k,10, COMP_WLO_IR) = H2OdH2OTzp_TL
        Predictor_TL%X(k,11, COMP_WLO_IR) = H2O_R_TL*H2OdH2OTzp + H2O_R*H2OdH2OTzp_TL
        Predictor_TL%X(k,12, COMP_WLO_IR) = TWO*SECANG(k)**2 * PAFV%GAzp(k,ABS_H2O_IR)*GAzp_TL(k,ABS_H2O_IR)
        Predictor_TL%X(k,13, COMP_WLO_IR) = SECANG(k)*GAzp_TL(k,ABS_H2O_IR)
        Predictor_TL%X(k,14, COMP_WLO_IR) = ZERO
        Predictor_TL%X(k,15, COMP_WLO_IR) = SECANG(k)*CO2_TL

        ! Addtional predictors for group 1
        IF_Group1: IF( Group_ID == GROUP_1 )THEN

          Predictor_TL%X(k, 11, COMP_CO2_IR) = CO_A_TL

          Predictor_TL%X(k, 16, COMP_WLO_IR) = CH4_A_TL
          Predictor_TL%X(k, 17, COMP_WLO_IR) = TWO*CH4_A*CH4_A_TL*DT + CH4_A*CH4_A*DT_TL
          Predictor_TL%X(k, 18, COMP_WLO_IR) = CO_A_TL

          !  -----------------------
          !  Carbon monoxide
          !  -----------------------
          Predictor_TL%X(k, 1,  COMP_CO_IR)   = CO_A_TL
          Predictor_TL%X(k, 2,  COMP_CO_IR)   = CO_A_TL*DT + CO_A*DT_TL
          Predictor_TL%X(k, 3,  COMP_CO_IR)   = (POINT_5/SQRT(CO_R))*CO_R_TL
          Predictor_TL%X(k, 4,  COMP_CO_IR)   = CO_R_TL*DT + CO_R*DT_TL
          Predictor_TL%X(k, 5,  COMP_CO_IR)   = CO_S_TL
          Predictor_TL%X(k, 6,  COMP_CO_IR)   = CO_R_TL
          Predictor_TL%X(k, 7,  COMP_CO_IR)   = CO_A_TL*DT2 + CO_A*DT2_TL
          Predictor_TL%X(k, 8,  COMP_CO_IR)   = CO_ACOdCOzp_TL
          Predictor_TL%X(k, 9,  COMP_CO_IR)   = CO_ACOdCOzp_TL/CO_R - CO_ACOdCOzp*CO_R_TL/CO_R**2
          Predictor_TL%X(k, 10, COMP_CO_IR)   = CO_ACOdCOzp_TL * SQRT( PAFV%GAzp(k, ABS_CO_IR) ) + &
                                                (POINT_5*CO_ACOdCOzp/SQRT(PAFV%GAzp(k, ABS_CO_IR))) * &
                                                GAzp_TL(k, ABS_CO_IR)

          !  -----------------------
          !  Methane predictors
          !  -----------------------
          Predictor_TL%X(k, 1,  COMP_CH4_IR)  = CH4_A_TL*DT + CH4_A*DT_TL
          Predictor_TL%X(k, 2,  COMP_CH4_IR)  = CH4_R_TL
          Predictor_TL%X(k, 3,  COMP_CH4_IR)  = TWO*CH4_A*CH4_A_TL
          Predictor_TL%X(k, 4,  COMP_CH4_IR)  = CH4_A_TL
          Predictor_TL%X(k, 5,  COMP_CH4_IR)  = CH4_TL*DT + CH4*DT_TL
          Predictor_TL%X(k, 6,  COMP_CH4_IR)  = CH4_ACH4zp_TL
          Predictor_TL%X(k, 7,  COMP_CH4_IR)  = TWO*CH4_ACH4zp*CH4_ACH4zp_TL
          Predictor_TL%X(k, 8,  COMP_CH4_IR)  = (POINT_5/SQRT(CH4_R))*CH4_R_TL
          Predictor_TL%X(k, 9,  COMP_CH4_IR)  = GATzp_TL(k, ABS_CH4_IR)
          Predictor_TL%X(k, 10, COMP_CH4_IR)  = SECANG(k)*GATzp_TL(k, ABS_CH4_IR)
          Predictor_TL%X(k, 11, COMP_CH4_IR)  = CH4_R_TL*CH4/PAFV%GAzp(k, ABS_CH4_IR) + &
                                                CH4_R*CH4_TL/PAFV%GAzp(k, ABS_CH4_IR) - &
                                                CH4_R*CH4*GAzp_TL(k, ABS_CH4_IR)/PAFV%GAzp(k, ABS_CH4_IR)**2
          !  -----------------------
          !    N2O predictors
          !  -----------------------
          Predictor_TL%X(k, 1, COMP_N2O_IR)   = N2O_A_TL*DT + N2O_A*DT_TL
          Predictor_TL%X(k, 2, COMP_N2O_IR)   = N2O_R_TL
          Predictor_TL%X(k, 3, COMP_N2O_IR)   = N2O_TL*DT + N2O*DT_TL
          Predictor_TL%X(k, 4, COMP_N2O_IR)   = POINT_25*N2O_A**(-POINT_75) * N2O_A_TL
          Predictor_TL%X(k, 5, COMP_N2O_IR)   = N2O_A_TL
          Predictor_TL%X(k, 6, COMP_N2O_IR)   = SECANG(k) * GAzp_TL(k, ABS_N2O_IR)
          Predictor_TL%X(k, 7, COMP_N2O_IR)   = SECANG(k) * GATzp_TL(k, ABS_N2O_IR)
          Predictor_TL%X(k, 8, COMP_N2O_IR)   = N2O_S_TL
          Predictor_TL%X(k, 9, COMP_N2O_IR)   = GATzp_TL(k, ABS_N2O_IR)
          Predictor_TL%X(k,10, COMP_N2O_IR)   = N2O_R_TL*N2O / PAFV%GAzp(k, ABS_N2O_IR) + &
                                                N2O_R*N2O_TL / PAFV%GAzp(k, ABS_N2O_IR) - &
                                                N2O_R*N2O*GAzp_TL(k, ABS_N2O_IR)/PAFV%GAzp(k, ABS_N2O_IR)**2
          Predictor_TL%X(k,11, COMP_N2O_IR)   = CH4_A_TL
          Predictor_TL%X(k,12, COMP_N2O_IR)   = CH4_A_TL*PAFV%GAzp(k, ABS_CH4_IR) + CH4_A*GAzp_TL(k, ABS_CH4_IR)
          Predictor_TL%X(k,13, COMP_N2O_IR)   = CO_A_TL
          Predictor_TL%X(k,14, COMP_N2O_IR)   = CO_A_TL*SECANG(k)*PAFV%GAzp(k, ABS_CO_IR) + &
                                                CO_A*SECANG(k)*GAzp_TL(k, ABS_CO_IR)

        END IF IF_Group1

      END DO Layer_Loop

    END SUBROUTINE ODPS_Compute_Predictor_IR_TL

    SUBROUTINE ODPS_Compute_Predictor_MW_TL()

      ! ---------------
      ! Local variables
      ! ---------------
      INTEGER    ::    k ! n_Layers, n_Levels
      REAL(fp) ::    DT,           DT_TL
      REAL(fp) ::    T,            T_TL
      REAL(fp) ::    T2,           T2_TL
      REAL(fp) ::    DT2,          DT2_TL
      REAL(fp) ::    H2O,          H2O_TL
      REAL(fp) ::    H2O_A,        H2O_A_TL
      REAL(fp) ::    H2O_R,        H2O_R_TL
      REAL(fp) ::    H2O_S,        H2O_S_TL
      REAL(fp) ::    H2O_R4,       H2O_R4_TL
      REAL(fp) ::    H2OdH2OTzp,   H2OdH2OTzp_TL

      Layer_Loop : DO k = 1, n_Layers

        !------------------------------------------
        !  Relative Temperature
        !------------------------------------------
        dT = Temperature(k) - Ref_Temperature(k)
        T = Temperature(k) / Ref_Temperature(k)

        dT_TL = Temperature_TL(k)
        T_TL  = Temperature_TL(k) / Ref_Temperature(k)

        !-------------------------------------------
        !  Abosrber amount scalled by the reference
        !-------------------------------------------
        H2O = Absorber(k,ABS_H2O_MW)/Ref_Absorber(k, ABS_H2O_MW)
        H2O_TL = Absorber_TL(k,ABS_H2O_MW)/Ref_Absorber(k, ABS_H2O_MW)

        ! Combinations of variables common to all predictor groups
        T2  = T*T
        DT2 = DT*ABS( DT )

        T2_TL  = TWO*T*T_TL
        IF( DT > ZERO) THEN
          DT2_TL = TWO*DT*DT_TL
        ELSE
          DT2_TL = - TWO*DT*DT_TL
        ENDIF

        H2O_A = SECANG(k)*H2O
        H2O_R  = SQRT( H2O_A )
        H2O_S  = H2O_A*H2O_A
        H2O_R4 = SQRT( H2O_R )
        H2OdH2OTzp = H2O/PAFV%GATzp(k, ABS_H2O_MW)

        H2O_A_TL  = SECANG(k)*H2O_TL
        H2O_R_TL  = (POINT_5 / SQRT(H2O_A)) * H2O_A_TL
        H2O_S_TL  = TWO * H2O_A * H2O_A_TL
        H2O_R4_TL = (POINT_5 / SQRT(H2O_R)) * H2O_R_TL
        H2OdH2OTzp_TL = H2O_TL/PAFV%GATzp(k, ABS_H2O_MW) - &
                        H2O * GATzp_TL(k, ABS_H2O_IR)/PAFV%GATzp(k, ABS_H2O_MW)**2

       !#-------------------------------------------------------------------#
       !#        -- Predictors --                                           #
       !#-------------------------------------------------------------------#

       ! set number of predictors
        Predictor_TL%n_CP = N_PREDICTORS_G3

        !  ----------------------
        !   Fixed (Dry) predictors
        !  ----------------------
        Predictor_TL%X(k, 1, COMP_DRY_MW)  = ZERO
        Predictor_TL%X(k, 2, COMP_DRY_MW)  = SECANG(k) * T_TL
        Predictor_TL%X(k, 3, COMP_DRY_MW)  = SECANG(k) * T2_TL
        Predictor_TL%X(k, 4, COMP_DRY_MW)  = T_TL
        Predictor_TL%X(k, 5, COMP_DRY_MW)  = ZERO
        Predictor_TL%X(k, 6, COMP_DRY_MW)  = T2_TL
        Predictor_TL%X(k, 7, COMP_DRY_MW)  = Tz_TL(k)
        Predictor_TL%X(:, 8:, COMP_DRY_MW) = ZERO
       !  --------------------------------
       !  Water vapor (line and continuum)
       !  --------------------------------
        Predictor_TL%X(k, 1, COMP_WET_MW) = H2O_A_TL/T - H2O_A*T_TL/T**2
        Predictor_TL%X(k, 2, COMP_WET_MW) = H2O_A_TL*H2O/T + H2O_A*H2O_TL/T - H2O_A*H2O*T_TL/T**2
        Predictor_TL%X(k, 3, COMP_WET_MW) = H2O_A_TL*H2O/T2**2 + H2O_A*H2O_TL/T2**2 - &
                                            Two*H2O_A*H2O*T2_TL/T2**3
        Predictor_TL%X(k, 4, COMP_WET_MW) = H2O_A_TL/T2 - H2O_A * T2_TL/T2**2
        Predictor_TL%X(k, 5, COMP_WET_MW) = H2O_A_TL*H2O/T2 + H2O_A*H2O_TL/T2 - H2O_A*H2O*T2_TL/T2**2
        Predictor_TL%X(k, 6, COMP_WET_MW) = H2O_A_TL/T2**2 - TWO*H2O_A*T2_TL/T2**3
        Predictor_TL%X(k, 7, COMP_WET_MW) = H2O_A_TL
        Predictor_TL%X(k, 8, COMP_WET_MW) = H2O_A_TL*DT + H2O_A*DT_TL
        Predictor_TL%X(k, 9, COMP_WET_MW) = TWO*SECANG(k)**2 * PAFV%GAzp(k,ABS_H2O_MW)*GAzp_TL(k,ABS_H2O_MW)
        Predictor_TL%X(k, 10,COMP_WET_MW) = SECANG(k)*GAzp_TL(k,ABS_H2O_MW)
        Predictor_TL%X(k, 11,COMP_WET_MW) = ZERO
        Predictor_TL%X(k, 12,COMP_WET_MW) = H2O_S_TL*H2O_A + H2O_S*H2O_A_TL
        Predictor_TL%X(k, 13,COMP_WET_MW) = TWO*H2O_S*H2O_S_TL
        Predictor_TL%X(k, 14,COMP_WET_MW) = H2OdH2OTzp_TL

      END DO Layer_Loop

    END SUBROUTINE ODPS_Compute_Predictor_MW_TL

  END SUBROUTINE  ODPS_Compute_Predictor_TL

!============================== END OF TL

!============================== AD

!------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Compute_Predictor_AD
!
! PURPOSE:
!       Subroutine to predictors
!
! CALLING SEQUENCE:
!       CALL ODPS_Compute_Predictor_AD( &
!         Group_ID,           &  ! Input
!         Temperature,        &  ! Input
!         Absorber,           &  ! Input
!         Ref_Temperature,    &  ! Input
!         Ref_Absorber,       &  ! Input
!         secang,             &  ! Input
!         Predictor,          &  ! Input
!         Predictor_AD,       &  ! Input
!         Temperature_AD      &  ! Output
!         Absorber_AD         )  ! Output
!
! INPUT ARGUMENTS:
!       Group_ID   :     The ID of predictor group
!                        UNITS:      N?A
!                        TYPE:       INTEGER
!                        DIMENSION:  scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       Absorber   :     Absorber profiles
!                        UNITS:      vary
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-2(n_Layers x n_Absorbers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       Ref_Temperature : Reference layer temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       Ref_Absorber :   Reference absorber profiles
!                        UNITS:      vary
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-2(n_Layers x n_Absorbers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       secang       :   Secont sensor zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:   Predictor_AD structure containing the integrated absorber_AD
!                       and predictor_AD profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!
!       Temperature_AD:  Temperature_AD profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       Absorber_AD:     Absorber_AD profiles
!                        UNITS:      vary
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-2(n_Layers x n_Absorbers) array
!                        ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_Predictor_AD( &
    Group_ID,           &
    Temperature,        &
    Absorber,           &
    Ref_Temperature,    &
    Ref_Absorber,       &
    secang,             &
    Predictor,          &
    Predictor_AD,       &
    Temperature_AD,     &
    Absorber_AD )

    INTEGER,                           INTENT(IN)     :: Group_ID
    REAL(fp),                          INTENT(IN)     :: Temperature(:)
    REAL(fp),                          INTENT(IN)     :: Absorber(:, :)
    REAL(fp),                          INTENT(IN)     :: Ref_Temperature(:)
    REAL(fp),                          INTENT(IN)     :: Ref_Absorber(:, :)
    REAL(fp),                          INTENT(IN)     :: Secang(:)
    TYPE(ODPS_Predictor_type), TARGET, INTENT(IN)     :: Predictor
    TYPE(ODPS_Predictor_type),         INTENT(IN OUT) :: Predictor_AD
    REAL(fp),                          INTENT(IN OUT) :: Temperature_AD(:)
    REAL(fp),                          INTENT(IN OUT) :: Absorber_AD(:, :)

    ! ---------------
    ! Local variables
    ! ---------------
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ODPS_Compute_Predictor_AD'
    INTEGER    ::    n_layers
    INTEGER    ::    k ! n_Layers, n_Levels
    INTEGER    ::    j ! n_absorbers
    REAL(fp) ::    Tzp_sum_AD
    REAL(fp) ::    Tzp_AD(SIZE(Absorber, DIM=1))
    REAL(fp) ::    Tz_sum_AD
    REAL(fp) ::    Tz_AD(SIZE(Absorber, DIM=1))
    REAL(fp) ::    GAz_sum_AD(SIZE(Absorber, DIM=2))
    REAL(fp) ::    GAz_AD(SIZE(Absorber, DIM=1), SIZE(Absorber, DIM=2))
    REAL(fp) ::    GAzp_sum_AD(SIZE(Absorber, DIM=2))
    REAL(fp) ::    GAzp_AD(SIZE(Absorber, DIM=1), SIZE(Absorber, DIM=2))
    REAL(fp) ::    GATzp_sum_AD(SIZE(Absorber, DIM=2))
    REAL(fp) ::    GATzp_AD(SIZE(Absorber, DIM=1), SIZE(Absorber, DIM=2))
    TYPE(PAFV_type), POINTER  :: PAFV => NULL()

    ! use short name
    PAFV => Predictor%PAFV

    n_Layers = Predictor%n_Layers


    !------------------------------------
    ! Compute integrated variables
    !------------------------------------

    Tzp_sum_AD   = ZERO
    Tz_sum_AD    = ZERO
    GAz_sum_AD   = ZERO
    GAzp_sum_AD  = ZERO
    GATzp_sum_AD = ZERO

    GATzp_AD = ZERO
    GAzp_AD  = ZERO
    GAz_AD   = ZERO
    Tzp_AD   = ZERO
    Tz_AD    = ZERO

    !----------------------------------------------------------------
    ! Call the group specific routine for remaining computation; all
    ! variables defined above are passed to the called routine
    !----------------------------------------------------------------

    SELECT CASE( Group_ID )
      CASE( GROUP_1, GROUP_2 )
         CALL ODPS_Compute_Predictor_IR_AD()
      CASE( GROUP_3 )
         CALL ODPS_Compute_Predictor_MW_AD()
    END SELECT

    Adjoint_Layer_Loop : DO k = n_Layers, 1, -1

      ! absorbers
      DO j = N_ABSORBERS_G(Group_ID), 1, -1

        GATzp_sum_AD(j) = GATzp_sum_AD(j) + GATzp_AD(k, j)/PAFV%GATzp_ref(k,j)
        GAzp_sum_AD(j) = GAzp_sum_AD(j) + GAzp_AD(k, j)/PAFV%GAzp_ref(k,j)
        GAz_sum_AD(j) = GAz_sum_AD(j) + GAz_AD(k, j)/PAFV%GAz_ref(k,j)
        Temperature_AD(k) = Temperature_AD(k) + GATzp_sum_AD(j)*PAFV%PDP(k)*Absorber(k, j)
        Absorber_AD(k, j) = Absorber_AD(k, j) + GAz_sum_AD(j) + GAzp_sum_AD(j)*PAFV%PDP(k) &
                                              + GATzp_sum_AD(j)*PAFV%PDP(k)*Temperature(k)
        GATzp_AD(k, j) = ZERO
        GAzp_AD(k, j)  = ZERO
        GAz_AD(k, j)   = ZERO

      END DO

      ! Temperature
      Tzp_sum_AD = Tzp_sum_AD + Tzp_AD(k)/PAFV%Tzp_ref(k)
      Tz_sum_AD = Tz_sum_AD + Tz_AD(k)/PAFV%Tz_ref(k)
      Temperature_AD(k) = Temperature_AD(k) + Tz_sum_AD + PAFV%PDP(k)*Tzp_sum_AD
      Tzp_AD(k) = ZERO
      Tz_AD(k)  = ZERO

    END DO Adjoint_Layer_Loop

    NULLIFY(PAFV)

CONTAINS

    SUBROUTINE ODPS_Compute_Predictor_IR_AD()

      ! ---------------
      ! Local variables
      ! ---------------
      INTEGER  :: k ! n_Layers, n_Levels
      REAL(fp) :: DT,            DT_AD
      REAL(fp) :: T,             T_AD
      REAL(fp) :: T2,            T2_AD
      REAL(fp) :: DT2,           DT2_AD
      REAL(fp) :: H2O,           H2O_AD
      REAL(fp) :: H2O_A,         H2O_A_AD
      REAL(fp) :: H2O_R,         H2O_R_AD
      REAL(fp) :: H2O_S,         H2O_S_AD
      REAL(fp) :: H2O_R4,        H2O_R4_AD
      REAL(fp) :: H2OdH2OTzp,    H2OdH2OTzp_AD
      REAL(fp) :: CO2,           CO2_AD
      REAL(fp) :: O3,            O3_AD
      REAL(fp) :: O3_A,          O3_A_AD
      REAL(fp) :: O3_R,          O3_R_AD
      REAL(fp) :: CO,            CO_AD
      REAL(fp) :: CO_A,          CO_A_AD
      REAL(fp) :: CO_R,          CO_R_AD
      REAL(fp) :: CO_S,          CO_S_AD
      REAL(fp) :: CO_ACOdCOzp,   CO_ACOdCOzp_AD
      REAL(fp) :: N2O,           N2O_AD
      REAL(fp) :: N2O_A,         N2O_A_AD
      REAL(fp) :: N2O_R,         N2O_R_AD
      REAL(fp) :: N2O_S,         N2O_S_AD
      REAL(fp) :: CH4,           CH4_AD
      REAL(fp) :: CH4_A,         CH4_A_AD
      REAL(fp) :: CH4_R,         CH4_R_AD
      REAL(fp) :: CH4_ACH4zp,    CH4_ACH4zp_AD

      DT_AD          = ZERO
      T_AD           = ZERO
      T2_AD          = ZERO
      DT2_AD         = ZERO
      H2O_AD         = ZERO
      H2O_A_AD       = ZERO
      H2O_R_AD       = ZERO
      H2O_S_AD       = ZERO
      H2O_R4_AD      = ZERO
      H2OdH2OTzp_AD  = ZERO
      CO2_AD         = ZERO
      O3_AD          = ZERO
      O3_A_AD        = ZERO
      O3_R_AD        = ZERO
      CO_AD          = ZERO
      CO_A_AD        = ZERO
      CO_R_AD        = ZERO
      CO_S_AD        = ZERO
      CO_ACOdCOzp_AD = ZERO
      N2O_AD         = ZERO
      N2O_A_AD       = ZERO
      N2O_R_AD       = ZERO
      N2O_S_AD       = ZERO
      CH4_AD         = ZERO
      CH4_A_AD       = ZERO
      CH4_R_AD       = ZERO
      CH4_ACH4zp_AD  = ZERO

      Layer_Loop : DO k = n_Layers, 1, -1

        !------------------------------------------
        !  Relative Temperature
        !------------------------------------------
        dT = Temperature(k) - Ref_Temperature(k)
        T = Temperature(k) / Ref_Temperature(k)

        !-------------------------------------------
        !  Abosrber amount scalled by the reference
        !-------------------------------------------
        H2O = Absorber(k,ABS_H2O_IR)/Ref_Absorber(k, ABS_H2O_IR)
        O3  = Absorber(k,ABS_O3_IR)/Ref_absorber(k,ABS_O3_IR)
        CO2 = Absorber(k,ABS_CO2_IR)/Ref_absorber(k,ABS_CO2_IR)

        ! Combinations of variables common to all predictor groups
        T2   = T*T
        DT2  = DT*ABS( DT )

        H2O_A = SECANG(k)*H2O
        H2O_R  = SQRT( H2O_A )
        H2O_S  = H2O_A*H2O_A
        H2O_R4 = SQRT( H2O_R )
        H2OdH2OTzp = H2O/PAFV%GATzp(k, ABS_H2O_IR)

        O3_A = SECANG(k)*O3
        O3_R = SQRT( O3_A )

        IF( Group_ID == GROUP_1 )THEN
          CO  = Absorber(k,ABS_CO_IR)/Ref_absorber(k, ABS_CO_IR)
          N2O = Absorber(k,ABS_N2O_IR)/Ref_absorber(k,ABS_N2O_IR)
          CH4 = Absorber(k,ABS_CH4_IR)/Ref_absorber(k,ABS_CH4_IR)

          N2O_A = SECANG(k)*N2O
          N2O_R = SQRT( N2O_A )
          N2O_S = N2O_A*N2O_A

          CO_A = SECANG(k)*CO
          CO_R = SQRT( CO_A )
          CO_S = CO_A*CO_A
          CO_ACOdCOzp = CO_A*CO/PAFV%GAzp(k, ABS_CO_IR)

          CH4_A = SECANG(k)*CH4
          CH4_R = SQRT(CH4_A)
          CH4_ACH4zp = SECANG(k)*PAFV%GAzp(k, ABS_CH4_IR)

        END IF

       !#-------------------------------------------------------------------#
       !#        -- Predictors --                                           #
       !#-------------------------------------------------------------------#

        !  ----------------------
        !   Fixed (Dry) predictors
        !  ----------------------
        T_AD     = T_AD                                            &
                   + Predictor_AD%X(k, 2, COMP_DRY_IR) * SECANG(k) &
                   + Predictor_AD%X(k, 4, COMP_DRY_IR)
        T2_AD    = T2_AD                                           &
                   + Predictor_AD%X(k, 3, COMP_DRY_IR) * SECANG(k) &
                   + Predictor_AD%X(k, 6, COMP_DRY_IR)
        Tz_AD(k) = Tz_AD(k) + Predictor_AD%X(k, 7, COMP_DRY_IR)

        Predictor_AD%X(k, 1, COMP_DRY_IR)  = ZERO
        Predictor_AD%X(k, 2, COMP_DRY_IR)  = ZERO
        Predictor_AD%X(k, 3, COMP_DRY_IR)  = ZERO
        Predictor_AD%X(k, 4, COMP_DRY_IR)  = ZERO
        Predictor_AD%X(k, 5, COMP_DRY_IR)  = ZERO
        Predictor_AD%X(k, 6, COMP_DRY_IR)  = ZERO
        Predictor_AD%X(k, 7, COMP_DRY_IR)  = ZERO

       !  --------------------------
       !  Water vapor continuum predictors
       !  --------------------------
        H2O_A_AD = H2O_A_AD                                         &
                   + Predictor_AD%X(k, 1, COMP_WCO_IR)/T            &
                   + Predictor_AD%X(k, 2, COMP_WCO_IR)*H2O/T        &
                   + Predictor_AD%X(k, 3, COMP_WCO_IR)*H2O/T2**2    &
                   + Predictor_AD%X(k, 4, COMP_WCO_IR)/T2           &
                   + Predictor_AD%X(k, 5, COMP_WCO_IR)*H2O/T2       &
                   + Predictor_AD%X(k, 6, COMP_WCO_IR)/T2**2        &
                   + Predictor_AD%X(k, 7, COMP_WCO_IR)
        T_AD     = T_AD                                                &
                   - Predictor_AD%X(k, 1, COMP_WCO_IR)*H2O_A/T**2      &
                   - Predictor_AD%X(k, 2, COMP_WCO_IR)*H2O_A*H2O/T**2
        H2O_AD   = H2O_AD                                          &
                   + Predictor_AD%X(k, 2, COMP_WCO_IR)*H2O_A/T     &
                   + Predictor_AD%X(k, 3, COMP_WCO_IR)*H2O_A/T2**2 &
                   + Predictor_AD%X(k, 5, COMP_WCO_IR)*H2O_A/T2
        T2_AD    = T2_AD                                                   &
                   - Predictor_AD%X(k, 3, COMP_WCO_IR)*TWO*H2O_A*H2O/T2**3 &
                   - Predictor_AD%X(k, 4, COMP_WCO_IR)*H2O_A/T2**2         &
                   - Predictor_AD%X(k, 5, COMP_WCO_IR)*H2O_A*H2O/T2**2     &
                   - Predictor_AD%X(k, 6, COMP_WCO_IR)*TWO*H2O_A/T2**3

        Predictor_AD%X(k, 1, COMP_WCO_IR) = ZERO
        Predictor_AD%X(k, 2, COMP_WCO_IR) = ZERO
        Predictor_AD%X(k, 3, COMP_WCO_IR) = ZERO
        Predictor_AD%X(k, 4, COMP_WCO_IR) = ZERO
        Predictor_AD%X(k, 5, COMP_WCO_IR) = ZERO
        Predictor_AD%X(k, 6, COMP_WCO_IR) = ZERO
        Predictor_AD%X(k, 7, COMP_WCO_IR) = ZERO

        !  -----------------------
        !  Ozone predictors
        !  -----------------------

        O3_A_AD = O3_A_AD                                                               &
                  + Predictor_AD%X(k, 1, COMP_OZO_IR)                                   &
                  + Predictor_AD%X(k, 2, COMP_OZO_IR)*DT                                &
                  + Predictor_AD%X(k, 3, COMP_OZO_IR)*O3*PAFV%GAzp(k,ABS_O3_IR)         &
                  + Predictor_AD%X(k, 4, COMP_OZO_IR)*TWO*O3_A                          &
                  + Predictor_AD%X(k, 5, COMP_OZO_IR)*PAFV%GAzp(k,ABS_O3_IR)            &
                  + Predictor_AD%X(k, 6, COMP_OZO_IR)*SQRT(SECANG(k)*PAFV%GAzp(k,ABS_O3_IR))

        DT_AD   = DT_AD                                                                 &
                  + Predictor_AD%X(k, 2, COMP_OZO_IR)*O3_A                              &
                  + Predictor_AD%X(k, 7, COMP_OZO_IR)*O3_R

        O3_AD   = O3_AD                                                                 &
                  + Predictor_AD%X(k, 3, COMP_OZO_IR)*O3_A*PAFV%GAzp(k,ABS_O3_IR)       &
                  + Predictor_AD%X(k, 9, COMP_OZO_IR)*O3_R/PAFV%GAzp(k,ABS_O3_IR)

        GAzp_AD(k,ABS_O3_IR) = GAzp_AD(k,ABS_O3_IR)                                     &
                  + Predictor_AD%X(k, 3, COMP_OZO_IR)*O3_A*O3                           &
                  + Predictor_AD%X(k, 5, COMP_OZO_IR)*O3_A                              &
                  + Predictor_AD%X(k, 6, COMP_OZO_IR)*POINT_5*O3_A*SQRT(SECANG(k)/PAFV%GAzp(k,ABS_O3_IR)) &
                  - Predictor_AD%X(k, 9, COMP_OZO_IR)*O3_R*O3/PAFV%GAzp(k,ABS_O3_IR)**2 &
                  + Predictor_AD%X(k,10, COMP_OZO_IR)*SECANG(k)                         &
                  + Predictor_AD%X(k,11, COMP_OZO_IR)*TWO*SECANG(k)**2*PAFV%GAzp(k,ABS_O3_IR)

        O3_R_AD = O3_R_AD                                                               &
                  + Predictor_AD%X(k, 7, COMP_OZO_IR)*DT                                &
                  + Predictor_AD%X(k, 8, COMP_OZO_IR)                                   &
                  + Predictor_AD%X(k, 9, COMP_OZO_IR)*O3/PAFV%GAzp(k,ABS_O3_IR)

!        H2O_A_AD= H2O_A_AD + Predictor_AD%X(k, 12, COMP_OZO_IR)

!        GAzp_AD(k,ABS_H2O_IR) = GAzp_AD(k,ABS_H2O_IR)                                   &
!                  + Predictor_AD%X(k, 13, COMP_OZO_IR)*SECANG(k)

        Predictor_AD%X(k, 1, COMP_OZO_IR)  = ZERO
        Predictor_AD%X(k, 2, COMP_OZO_IR)  = ZERO
        Predictor_AD%X(k, 3, COMP_OZO_IR)  = ZERO
        Predictor_AD%X(k, 4, COMP_OZO_IR)  = ZERO
        Predictor_AD%X(k, 5, COMP_OZO_IR)  = ZERO
        Predictor_AD%X(k, 6, COMP_OZO_IR)  = ZERO
        Predictor_AD%X(k, 7, COMP_OZO_IR)  = ZERO
        Predictor_AD%X(k, 8, COMP_OZO_IR)  = ZERO
        Predictor_AD%X(k, 9, COMP_OZO_IR)  = ZERO
        Predictor_AD%X(k,10, COMP_OZO_IR)  = ZERO
        Predictor_AD%X(k,11, COMP_OZO_IR)  = ZERO

        Predictor_AD%X(k,12, COMP_OZO_IR)  = ZERO
        Predictor_AD%X(k,13, COMP_OZO_IR)  = ZERO

        !  -----------------------
        !  Carbon dioxide predictors
        !  -----------------------
        T_AD      = T_AD                                                &
                    + Predictor_AD%X(k, 1, COMP_CO2_IR)*SECANG(k)       &
                    + Predictor_AD%X(k, 3, COMP_CO2_IR)                 &
                    + Predictor_AD%X(k, 10, COMP_CO2_IR)*SECANG(k)*(POINT_5*PAFV%Tzp(k)/SQRT(T))

        T2_AD     = T2_AD                                               &
                    + Predictor_AD%X(k, 2, COMP_CO2_IR)*SECANG(k)       &
                    + Predictor_AD%X(k, 4, COMP_CO2_IR)

        CO2_AD    = CO2_AD + Predictor_AD%X(k, 6, COMP_CO2_IR)*SECANG(k)

        Tzp_AD(k) = Tzp_AD(k)                                           &
                    + Predictor_AD%X(k, 7, COMP_CO2_IR)*SECANG(k)       &
                    + Predictor_AD%X(k, 9, COMP_CO2_IR)*THREE*PAFV%Tzp(k)**2 &
                    + Predictor_AD%X(k, 10, COMP_CO2_IR)*SECANG(k)*SQRT(T)

        GAzp_AD(k, ABS_CO2_IR) = GAzp_AD(k, ABS_CO2_IR)                 &
                    + Predictor_AD%X(k, 8, COMP_CO2_IR)*TWO*SECANG(k)**2*PAFV%GAzp(k, ABS_CO2_IR)


        Predictor_AD%X(k, 1, COMP_CO2_IR)  = ZERO
        Predictor_AD%X(k, 2, COMP_CO2_IR)  = ZERO
        Predictor_AD%X(k, 3, COMP_CO2_IR)  = ZERO
        Predictor_AD%X(k, 4, COMP_CO2_IR)  = ZERO
        Predictor_AD%X(k, 5, COMP_CO2_IR)  = ZERO
        Predictor_AD%X(k, 6, COMP_CO2_IR)  = ZERO
        Predictor_AD%X(k, 7, COMP_CO2_IR)  = ZERO
        Predictor_AD%X(k, 8, COMP_CO2_IR)  = ZERO
        Predictor_AD%X(k, 9, COMP_CO2_IR)  = ZERO
        Predictor_AD%X(k,10, COMP_CO2_IR)  = ZERO

        !  --------------------------
        !  Water-line predictors
        !  --------------------------

        H2O_A_AD = H2O_A_AD                                          &
                   + Predictor_AD%X(k, 1, COMP_WLO_IR)               &
                   + Predictor_AD%X(k, 2, COMP_WLO_IR)*DT            &
                   + Predictor_AD%X(k, 4, COMP_WLO_IR)*DT2           &
                   + Predictor_AD%X(k, 6, COMP_WLO_IR)*H2O_S

        DT_AD    = DT_AD                                             &
                   + Predictor_AD%X(k, 2, COMP_WLO_IR)*H2O_A         &
                   + Predictor_AD%X(k, 8, COMP_WLO_IR)*H2O_R

        H2O_S_AD = H2O_S_AD                                          &
                   + Predictor_AD%X(k, 3, COMP_WLO_IR)               &
                   + Predictor_AD%X(k, 6, COMP_WLO_IR)*H2O_A         &
                   + Predictor_AD%X(k, 9, COMP_WLO_IR)*TWO*H2O_S

        DT2_AD   = DT2_AD + Predictor_AD%X(k, 4, COMP_WLO_IR)*H2O_A

        H2O_R4_AD= H2O_R4_AD + Predictor_AD%X(k, 5, COMP_WLO_IR)

        H2O_R_AD = H2O_R_AD                                          &
                   + Predictor_AD%X(k, 7, COMP_WLO_IR)               &
                   + Predictor_AD%X(k, 8, COMP_WLO_IR)*DT            &
                   + Predictor_AD%X(k,11, COMP_WLO_IR)*H2OdH2OTzp

        H2OdH2OTzp_AD = H2OdH2OTzp_AD                                &
                   + Predictor_AD%X(k,10, COMP_WLO_IR)               &
                   + Predictor_AD%X(k,11, COMP_WLO_IR)*H2O_R

        GAzp_AD(k,ABS_H2O_IR) = GAzp_AD(k,ABS_H2O_IR)                                            &
                   + Predictor_AD%X(k,12, COMP_WLO_IR)*TWO*SECANG(k)**2*PAFV%GAzp(k,ABS_H2O_IR)  &
                   + Predictor_AD%X(k,13, COMP_WLO_IR)*SECANG(k)

        CO2_AD   = CO2_AD + Predictor_AD%X(k,15, COMP_WLO_IR)*SECANG(k)

        Predictor_AD%X(k, 1, COMP_WLO_IR) = ZERO
        Predictor_AD%X(k, 2, COMP_WLO_IR) = ZERO
        Predictor_AD%X(k, 3, COMP_WLO_IR) = ZERO
        Predictor_AD%X(k, 4, COMP_WLO_IR) = ZERO
        Predictor_AD%X(k, 5, COMP_WLO_IR) = ZERO
        Predictor_AD%X(k, 6, COMP_WLO_IR) = ZERO
        Predictor_AD%X(k, 7, COMP_WLO_IR) = ZERO
        Predictor_AD%X(k, 8, COMP_WLO_IR) = ZERO
        Predictor_AD%X(k, 9, COMP_WLO_IR) = ZERO
        Predictor_AD%X(k,10, COMP_WLO_IR) = ZERO
        Predictor_AD%X(k,11, COMP_WLO_IR) = ZERO
        Predictor_AD%X(k,12, COMP_WLO_IR) = ZERO
        Predictor_AD%X(k,13, COMP_WLO_IR) = ZERO
        Predictor_AD%X(k,14, COMP_WLO_IR) = ZERO
        Predictor_AD%X(k,15, COMP_WLO_IR) = ZERO

        ! Addtional predictors for group 1
        IF_Group1: IF( Group_ID == GROUP_1 )THEN

          CO_A_AD  = CO_A_AD   + Predictor_AD%X(k, 18, COMP_WLO_IR)         &
                               + Predictor_AD%X(k, 11, COMP_CO2_IR)
          CH4_A_AD = CH4_A_AD                                               &
                     + Predictor_AD%X(k, 16, COMP_WLO_IR)                   &
                     + Predictor_AD%X(k, 17, COMP_WLO_IR)*TWO*CH4_A*DT
          DT_AD    = DT_AD + Predictor_AD%X(k, 17, COMP_WLO_IR)*CH4_A*CH4_A

          Predictor_AD%X(k, 16, COMP_WLO_IR) = ZERO
          Predictor_AD%X(k, 17, COMP_WLO_IR) = ZERO
          Predictor_AD%X(k, 18, COMP_WLO_IR) = ZERO
          Predictor_AD%X(k, 11, COMP_CO2_IR) = ZERO

          !  -----------------------
          !  Carbon monoxide
          !  -----------------------

          CO_A_AD = CO_A_AD                                                      &
                    + Predictor_AD%X(k, 1,  COMP_CO_IR)                          &
                    + Predictor_AD%X(k, 2,  COMP_CO_IR)*DT                       &
                    + Predictor_AD%X(k, 7,  COMP_CO_IR)*DT2

          DT_AD   = DT_AD                                                        &
                    + Predictor_AD%X(k, 2,  COMP_CO_IR)*CO_A                     &
                    + Predictor_AD%X(k, 4,  COMP_CO_IR)*CO_R

          CO_R_AD = CO_R_AD                                                      &
                    + Predictor_AD%X(k, 3,  COMP_CO_IR)*POINT_5/SQRT(CO_R)       &
                    + Predictor_AD%X(k, 4,  COMP_CO_IR)*DT                       &
                    + Predictor_AD%X(k, 6,  COMP_CO_IR)                          &
                    - Predictor_AD%X(k, 9,  COMP_CO_IR)*CO_ACOdCOzp/CO_R**2

          CO_S_AD = CO_S_AD + Predictor_AD%X(k, 5,  COMP_CO_IR)

          DT2_AD  = DT2_AD + Predictor_AD%X(k, 7,  COMP_CO_IR)*CO_A

          CO_ACOdCOzp_AD = CO_ACOdCOzp_AD                                        &
                     + Predictor_AD%X(k, 8,  COMP_CO_IR)                         &
                     + Predictor_AD%X(k, 9,  COMP_CO_IR)/CO_R                    &
                     + Predictor_AD%X(k,10,  COMP_CO_IR)*SQRT(PAFV%GAzp(k, ABS_CO_IR))

          GAzp_AD(k, ABS_CO_IR) = GAzp_AD(k, ABS_CO_IR)                          &
                     + Predictor_AD%X(k, 10, COMP_CO_IR)*                        &
                       POINT_5*CO_ACOdCOzp/SQRT(PAFV%GAzp(k, ABS_CO_IR))

          Predictor_AD%X(k, 1,  COMP_CO_IR)   = ZERO
          Predictor_AD%X(k, 2,  COMP_CO_IR)   = ZERO
          Predictor_AD%X(k, 3,  COMP_CO_IR)   = ZERO
          Predictor_AD%X(k, 4,  COMP_CO_IR)   = ZERO
          Predictor_AD%X(k, 5,  COMP_CO_IR)   = ZERO
          Predictor_AD%X(k, 6,  COMP_CO_IR)   = ZERO
          Predictor_AD%X(k, 7,  COMP_CO_IR)   = ZERO
          Predictor_AD%X(k, 8,  COMP_CO_IR)   = ZERO
          Predictor_AD%X(k, 9,  COMP_CO_IR)   = ZERO
          Predictor_AD%X(k, 10, COMP_CO_IR)   = ZERO

          !  -----------------------
          !  Methane predictors
          !  -----------------------

          CH4_A_AD = CH4_A_AD                                                       &
                     + Predictor_AD%X(k, 1,  COMP_CH4_IR)*DT                        &
                     + Predictor_AD%X(k, 3,  COMP_CH4_IR)*TWO*CH4_A                 &
                     + Predictor_AD%X(k, 4,  COMP_CH4_IR)

          DT_AD    = DT_AD                                                          &
                     + Predictor_AD%X(k, 1,  COMP_CH4_IR)*CH4_A                     &
                     + Predictor_AD%X(k, 5,  COMP_CH4_IR)*CH4

          CH4_R_AD = CH4_R_AD                                                       &
                     + Predictor_AD%X(k, 2,  COMP_CH4_IR)                           &
                     + Predictor_AD%X(k, 8,  COMP_CH4_IR)*POINT_5/SQRT(CH4_R)       &
                     + Predictor_AD%X(k, 11, COMP_CH4_IR)*CH4/PAFV%GAzp(k, ABS_CH4_IR)

          CH4_AD   = CH4_AD                                                         &
                     + Predictor_AD%X(k, 5,  COMP_CH4_IR)*DT                        &
                     + Predictor_AD%X(k, 11, COMP_CH4_IR)*CH4_R/PAFV%GAzp(k, ABS_CH4_IR)

          CH4_ACH4zp_AD = CH4_ACH4zp_AD                                             &
                     + Predictor_AD%X(k, 6,  COMP_CH4_IR)                           &
                     + Predictor_AD%X(k, 7,  COMP_CH4_IR)*TWO*CH4_ACH4zp

          GATzp_AD(k, ABS_CH4_IR) = GATzp_AD(k, ABS_CH4_IR)                         &
                     + Predictor_AD%X(k, 9,  COMP_CH4_IR)                           &
                     + Predictor_AD%X(k,10,  COMP_CH4_IR)*SECANG(k)

          GAzp_AD(k, ABS_CH4_IR) = GAzp_AD(k, ABS_CH4_IR)                           &
                     - Predictor_AD%X(k, 11, COMP_CH4_IR)*                          &
                       CH4_R*CH4/PAFV%GAzp(k, ABS_CH4_IR)**2

          Predictor_AD%X(k, 1,  COMP_CH4_IR)  = ZERO
          Predictor_AD%X(k, 2,  COMP_CH4_IR)  = ZERO
          Predictor_AD%X(k, 3,  COMP_CH4_IR)  = ZERO
          Predictor_AD%X(k, 4,  COMP_CH4_IR)  = ZERO
          Predictor_AD%X(k, 5,  COMP_CH4_IR)  = ZERO
          Predictor_AD%X(k, 6,  COMP_CH4_IR)  = ZERO
          Predictor_AD%X(k, 7,  COMP_CH4_IR)  = ZERO
          Predictor_AD%X(k, 8,  COMP_CH4_IR)  = ZERO
          Predictor_AD%X(k, 9,  COMP_CH4_IR)  = ZERO
          Predictor_AD%X(k, 10, COMP_CH4_IR)  = ZERO
          Predictor_AD%X(k, 11, COMP_CH4_IR)  = ZERO

          !  -----------------------
          !    N2O predictors
          !  -----------------------

          N2O_A_AD = N2O_A_AD                                                        &
                     + Predictor_AD%X(k, 1, COMP_N2O_IR)*DT                          &
                     + Predictor_AD%X(k, 4, COMP_N2O_IR)*POINT_25*N2O_A**(-POINT_75) &
                     + Predictor_AD%X(k, 5, COMP_N2O_IR)

          DT_AD    = DT_AD                                                           &
                     + Predictor_AD%X(k, 1, COMP_N2O_IR)*N2O_A                       &
                     + Predictor_AD%X(k, 3, COMP_N2O_IR)*N2O

          N2O_R_AD = N2O_R_AD                                                        &
                     + Predictor_AD%X(k, 2, COMP_N2O_IR)                             &
                     + Predictor_AD%X(k,10, COMP_N2O_IR)*N2O/PAFV%GAzp(k, ABS_N2O_IR)

          N2O_AD   = N2O_AD                                                          &
                     + Predictor_AD%X(k, 3, COMP_N2O_IR)*DT                          &
                     + Predictor_AD%X(k,10, COMP_N2O_IR)*N2O_R/PAFV%GAzp(k, ABS_N2O_IR)

          GAzp_AD(k, ABS_N2O_IR) = GAzp_AD(k, ABS_N2O_IR)                            &
                     + Predictor_AD%X(k, 6, COMP_N2O_IR)*SECANG(k)                   &
                     - Predictor_AD%X(k,10, COMP_N2O_IR)*N2O_R*N2O/PAFV%GAzp(k, ABS_N2O_IR)**2

          GATzp_AD(k, ABS_N2O_IR) = GATzp_AD(k, ABS_N2O_IR)                          &
                     + Predictor_AD%X(k, 7, COMP_N2O_IR)*SECANG(k)                   &
                     + Predictor_AD%X(k, 9, COMP_N2O_IR)

          N2O_S_AD = N2O_S_AD + Predictor_AD%X(k, 8, COMP_N2O_IR)

          CH4_A_AD = CH4_A_AD                                                        &
                     + Predictor_AD%X(k,11, COMP_N2O_IR)                             &
                     + Predictor_AD%X(k,12, COMP_N2O_IR)*PAFV%GAzp(k, ABS_CH4_IR)

          GAzp_AD(k, ABS_CH4_IR) = GAzp_AD(k, ABS_CH4_IR)                            &
                     + Predictor_AD%X(k,12, COMP_N2O_IR)*CH4_A

          CO_A_AD = CO_A_AD                                                          &
                    + Predictor_AD%X(k,13, COMP_N2O_IR)                              &
                    + Predictor_AD%X(k,14, COMP_N2O_IR)*SECANG(k)*PAFV%GAzp(k, ABS_CO_IR)

          GAzp_AD(k, ABS_CO_IR) = GAzp_AD(k, ABS_CO_IR)                              &
                    + Predictor_AD%X(k,14, COMP_N2O_IR)*CO_A*SECANG(k)

          Predictor_AD%X(k, 1, COMP_N2O_IR)   = ZERO
          Predictor_AD%X(k, 2, COMP_N2O_IR)   = ZERO
          Predictor_AD%X(k, 3, COMP_N2O_IR)   = ZERO
          Predictor_AD%X(k, 4, COMP_N2O_IR)   = ZERO
          Predictor_AD%X(k, 5, COMP_N2O_IR)   = ZERO
          Predictor_AD%X(k, 6, COMP_N2O_IR)   = ZERO
          Predictor_AD%X(k, 7, COMP_N2O_IR)   = ZERO
          Predictor_AD%X(k, 8, COMP_N2O_IR)   = ZERO
          Predictor_AD%X(k, 9, COMP_N2O_IR)   = ZERO
          Predictor_AD%X(k,10, COMP_N2O_IR)   = ZERO

          Predictor_AD%X(k,11, COMP_N2O_IR)   = ZERO
          Predictor_AD%X(k,12, COMP_N2O_IR)   = ZERO
          Predictor_AD%X(k,13, COMP_N2O_IR)   = ZERO
          Predictor_AD%X(k,14, COMP_N2O_IR)   = ZERO

        END IF IF_Group1

        IF( Group_ID == GROUP_1 )THEN

          GAzp_AD(k, ABS_CH4_IR) = GAzp_AD(k, ABS_CH4_IR) + SECANG(k)*CH4_ACH4zp_AD
          CH4_A_AD = CH4_A_AD + (POINT_5/SQRT(CH4_A)) * CH4_R_AD
          CH4_AD = CH4_AD + SECANG(k)*CH4_A_AD
          CH4_ACH4zp_AD = ZERO
          CH4_R_AD =      ZERO
          CH4_A_AD =      ZERO

          GAzp_AD(k, ABS_CO_IR) = GAzp_AD(k, ABS_CO_IR)                              &
                                  - CO_ACOdCOzp_AD*CO_A*CO/PAFV%GAzp(k, ABS_CO_IR)**2
          CO_A_AD = CO_A_AD + CO_ACOdCOzp_AD*CO/PAFV%GAzp(k, ABS_CO_IR)              &
                            + CO_S_AD*TWO * CO_A                                     &
                            + CO_R_AD*POINT_5/SQRT(CO_A)
          CO_AD = CO_AD + CO_ACOdCOzp_AD*CO_A/PAFV%GAzp(k, ABS_CO_IR)                &
                        + CO_A_AD*SECANG(k)
          CO_ACOdCOzp_AD = ZERO
          CO_S_AD        = ZERO
          CO_R_AD        = ZERO
          CO_A_AD        = ZERO

          N2O_A_AD = N2O_A_AD + N2O_R_AD * POINT_5 / SQRT(N2O_A)                    &
                              + N2O_S_AD * TWO * N2O_A
          N2O_AD = N2O_AD + N2O_A_AD * SECANG(k)
          N2O_A_AD = ZERO
          N2O_R_AD = ZERO
          N2O_S_AD = ZERO

          Absorber_AD(k,ABS_CH4_IR) = Absorber_AD(k,ABS_CH4_IR)                      &
                                    + CH4_AD/Ref_absorber(k,ABS_CH4_IR)
          Absorber_AD(k,ABS_N2O_IR) = Absorber_AD(k,ABS_N2O_IR)                      &
                                    + N2O_AD/Ref_absorber(k,ABS_N2O_IR)
          Absorber_AD(k,ABS_CO_IR)  = Absorber_AD(k,ABS_CO_IR)                       &
                                    + CO_AD/Ref_absorber(k, ABS_CO_IR)
          CO_AD  = ZERO
          N2O_AD = ZERO
          CH4_AD = ZERO

        END IF

        ! Combinations of variables common to all predictor groups

        O3_A_AD = O3_A_AD + O3_R_AD * POINT_5 / SQRT(O3_A)
        O3_AD = O3_AD + O3_A_AD * SECANG(k)
        O3_A_AD = ZERO
        O3_R_AD = ZERO

        GATzp_AD(k, ABS_H2O_IR) = GATzp_AD(k, ABS_H2O_IR)                            &
                                - H2OdH2OTzp_AD*H2O/PAFV%GATzp(k, ABS_H2O_IR)**2
        H2O_R_AD = H2O_R_AD + H2O_R4_AD * POINT_5 / SQRT(H2O_R)
        H2O_A_AD = H2O_A_AD + H2O_S_AD * TWO * H2O_A                                 &
                 + H2O_R_AD * POINT_5 / SQRT(H2O_A)
        H2O_AD = H2O_AD + H2O_A_AD * SECANG(k)                                       &
               + H2OdH2OTzp_AD / PAFV%GATzp(k, ABS_H2O_IR)

        H2O_A_AD      = ZERO
        H2O_R_AD      = ZERO
        H2O_S_AD      = ZERO
        H2O_R4_AD     = ZERO
        H2OdH2OTzp_AD = ZERO

        IF( DT > ZERO) THEN
          DT_AD = DT_AD + DT2_AD*TWO*DT
        ELSE
          DT_AD = DT_AD - DT2_AD*TWO*DT
        ENDIF
        T_AD = T_AD + T2_AD*TWO*T
        T2_AD   = ZERO
        DT2_AD  = ZERO

        !-------------------------------------------
        !  Abosrber amount scalled by the reference
        !-------------------------------------------
        Absorber_AD(k,ABS_CO2_IR) = Absorber_AD(k,ABS_CO2_IR)            &
                                  + CO2_AD / Ref_absorber(k,ABS_CO2_IR)
        Absorber_AD(k,ABS_O3_IR)  = Absorber_AD(k,ABS_O3_IR)             &
                                  + O3_AD / Ref_absorber(k,ABS_O3_IR)
        Absorber_AD(k,ABS_H2O_IR) = Absorber_AD(k,ABS_H2O_IR)            &
                                  + H2O_AD / Ref_Absorber(k, ABS_H2O_IR)
        H2O_AD = ZERO
        O3_AD  = ZERO
        CO2_AD = ZERO

        !------------------------------------------
        !  Relative Temperature
        !------------------------------------------
        Temperature_AD(k) = Temperature_AD(k) + T_AD/Ref_Temperature(k) &
                          + dT_AD
        dT_AD = ZERO
        T_AD  = ZERO

      END DO Layer_Loop

    END SUBROUTINE ODPS_Compute_Predictor_IR_AD

    SUBROUTINE ODPS_Compute_Predictor_MW_AD()

      ! ---------------
      ! Local variables
      ! ---------------
      INTEGER  :: k ! n_Layers, n_Levels
      REAL(fp) :: DT,           DT_AD
      REAL(fp) :: T,            T_AD
      REAL(fp) :: T2,           T2_AD
      REAL(fp) :: DT2,          DT2_AD
      REAL(fp) :: H2O,          H2O_AD
      REAL(fp) :: H2O_A,        H2O_A_AD
      REAL(fp) :: H2O_R,        H2O_R_AD
      REAL(fp) :: H2O_S,        H2O_S_AD
      REAL(fp) :: H2O_R4,       H2O_R4_AD
      REAL(fp) :: H2OdH2OTzp,   H2OdH2OTzp_AD

      DT_AD         = ZERO
      T_AD          = ZERO
      T2_AD         = ZERO
      DT2_AD        = ZERO
      H2O_AD        = ZERO
      H2O_A_AD      = ZERO
      H2O_R_AD      = ZERO
      H2O_S_AD      = ZERO
      H2O_R4_AD     = ZERO
      H2OdH2OTzp_AD = ZERO

      Layer_Loop : DO k = n_Layers, 1, -1

        !------------------------------------------
        !  Relative Temperature
        !------------------------------------------
        dT = Temperature(k) - Ref_Temperature(k)
        T = Temperature(k) / Ref_Temperature(k)

        !-------------------------------------------
        !  Abosrber amount scalled by the reference
        !-------------------------------------------
        H2O = Absorber(k,ABS_H2O_MW)/Ref_Absorber(k, ABS_H2O_MW)

        ! Combinations of variables common to all predictor groups
        T2  = T*T
        DT2 = DT*ABS( DT )

        H2O_A = SECANG(k)*H2O
        H2O_R  = SQRT( H2O_A )
        H2O_S  = H2O_A*H2O_A
        H2O_R4 = SQRT( H2O_R )
        H2OdH2OTzp = H2O/PAFV%GATzp(k, ABS_H2O_MW)

       !#-------------------------------------------------------------------#
       !#        -- Predictors --                                           #
       !#-------------------------------------------------------------------#

       ! set number of predictors
        Predictor_AD%n_CP = N_PREDICTORS_G3

        !  ----------------------
        !   Fixed (Dry) predictors
        !  ----------------------

        T_AD  = T_AD                                                          &
                + Predictor_AD%X(k, 2, COMP_DRY_MW)*SECANG(k)                 &
                + Predictor_AD%X(k, 4, COMP_DRY_MW)

        T2_AD = T2_AD                                                         &
                + Predictor_AD%X(k, 3, COMP_DRY_MW)*SECANG(k)                 &
                + Predictor_AD%X(k, 6, COMP_DRY_MW)

        Tz_AD(k) = Tz_AD(k) + Predictor_AD%X(k, 7, COMP_DRY_MW)

        Predictor_AD%X(k, 1, COMP_DRY_MW)  = ZERO
        Predictor_AD%X(k, 2, COMP_DRY_MW)  = ZERO
        Predictor_AD%X(k, 3, COMP_DRY_MW)  = ZERO
        Predictor_AD%X(k, 4, COMP_DRY_MW)  = ZERO
        Predictor_AD%X(k, 5, COMP_DRY_MW)  = ZERO
        Predictor_AD%X(k, 6, COMP_DRY_MW)  = ZERO
        Predictor_AD%X(k, 7, COMP_DRY_MW)  = ZERO

       !  --------------------------------
       !  Water vapor (line and continuum)
       !  --------------------------------

        H2O_A_AD = H2O_A_AD                                                   &
                   + Predictor_AD%X(k, 1, COMP_WET_MW)/T                      &
                   + Predictor_AD%X(k, 2, COMP_WET_MW)*H2O/T                  &
                   + Predictor_AD%X(k, 3, COMP_WET_MW)*H2O/T2**2              &
                   + Predictor_AD%X(k, 4, COMP_WET_MW)/T2                     &
                   + Predictor_AD%X(k, 5, COMP_WET_MW)*H2O/T2                 &
                   + Predictor_AD%X(k, 6, COMP_WET_MW)/T2**2                  &
                   + Predictor_AD%X(k, 7, COMP_WET_MW)                        &
                   + Predictor_AD%X(k, 8, COMP_WET_MW)*DT                     &
                   + Predictor_AD%X(k,12, COMP_WET_MW)*H2O_S

        T_AD     = T_AD                                                       &
                   - Predictor_AD%X(k, 1, COMP_WET_MW)*H2O_A/T**2             &
                   - Predictor_AD%X(k, 2, COMP_WET_MW)*H2O_A*H2O/T**2

        H2O_AD   = H2O_AD                                                     &
                   + Predictor_AD%X(k, 2, COMP_WET_MW)*H2O_A/T                &
                   + Predictor_AD%X(k, 3, COMP_WET_MW)*H2O_A/T2**2            &
                   + Predictor_AD%X(k, 5, COMP_WET_MW)*H2O_A/T2

        T2_AD    = T2_AD                                                      &
                   - Predictor_AD%X(k, 3, COMP_WET_MW)*Two*H2O_A*H2O/T2**3    &
                   - Predictor_AD%X(k, 4, COMP_WET_MW)*H2O_A/T2**2            &
                   - Predictor_AD%X(k, 5, COMP_WET_MW)*H2O_A*H2O/T2**2        &
                   - Predictor_AD%X(k, 6, COMP_WET_MW)*TWO*H2O_A/T2**3

        DT_AD    = DT_AD + Predictor_AD%X(k, 8, COMP_WET_MW)*H2O_A

        GAzp_AD(k,ABS_H2O_MW) = GAzp_AD(k,ABS_H2O_MW)                                           &
                   + Predictor_AD%X(k, 9, COMP_WET_MW)*TWO*SECANG(k)**2*PAFV%GAzp(k,ABS_H2O_MW) &
                   + Predictor_AD%X(k, 10,COMP_WET_MW)*SECANG(k)

        H2O_S_AD = H2O_S_AD                                                   &
                   + Predictor_AD%X(k, 12,COMP_WET_MW)*H2O_A                  &
                   + Predictor_AD%X(k, 13,COMP_WET_MW)*TWO*H2O_S

        H2OdH2OTzp_AD = H2OdH2OTzp_AD + Predictor_AD%X(k, 14,COMP_WET_MW)

        Predictor_AD%X(k, 1, COMP_WET_MW) = ZERO
        Predictor_AD%X(k, 2, COMP_WET_MW) = ZERO
        Predictor_AD%X(k, 3, COMP_WET_MW) = ZERO
        Predictor_AD%X(k, 4, COMP_WET_MW) = ZERO
        Predictor_AD%X(k, 5, COMP_WET_MW) = ZERO
        Predictor_AD%X(k, 6, COMP_WET_MW) = ZERO
        Predictor_AD%X(k, 7, COMP_WET_MW) = ZERO
        Predictor_AD%X(k, 8, COMP_WET_MW) = ZERO
        Predictor_AD%X(k, 9, COMP_WET_MW) = ZERO
        Predictor_AD%X(k, 10,COMP_WET_MW) = ZERO
        Predictor_AD%X(k, 11,COMP_WET_MW) = ZERO
        Predictor_AD%X(k, 12,COMP_WET_MW) = ZERO
        Predictor_AD%X(k, 13,COMP_WET_MW) = ZERO
        Predictor_AD%X(k, 14,COMP_WET_MW) = ZERO

        !-------------------------------------------
        !  Abosrber amount scalled by the reference
        !-------------------------------------------

        ! Combinations of variables common to all predictor groups

        GATzp_AD(k, ABS_H2O_MW) = GATzp_AD(k, ABS_H2O_MW)                            &
                                - H2OdH2OTzp_AD*H2O/PAFV%GATzp(k, ABS_H2O_MW)**2
        H2O_R_AD = H2O_R_AD + H2O_R4_AD * POINT_5 / SQRT(H2O_R)
        H2O_A_AD = H2O_A_AD + H2O_S_AD * TWO * H2O_A                                 &
                 + H2O_R_AD * POINT_5 / SQRT(H2O_A)
        H2O_AD = H2O_AD + H2O_A_AD * SECANG(k)                                       &
               + H2OdH2OTzp_AD / PAFV%GATzp(k, ABS_H2O_MW)
        H2O_A_AD      = ZERO
        H2O_R_AD      = ZERO
        H2O_S_AD      = ZERO
        H2O_R4_AD     = ZERO
        H2OdH2OTzp_AD = ZERO

        IF( DT > ZERO) THEN
          DT_AD = DT_AD + DT2_AD*TWO*DT
        ELSE
          DT_AD = DT_AD - DT2_AD*TWO*DT
        ENDIF
        T_AD = T_AD + T2_AD*TWO*T
        T2_AD   = ZERO
        DT2_AD  = ZERO

        Absorber_AD(k,ABS_H2O_MW) = Absorber_AD(k,ABS_H2O_MW)            &
                                  + H2O_AD / Ref_Absorber(k, ABS_H2O_MW)
        H2O_AD = ZERO

        !------------------------------------------
        !  Relative Temperature
        !------------------------------------------
        Temperature_AD(k) = Temperature_AD(k) + T_AD/Ref_Temperature(k) &
                          + dT_AD
        dT_AD = ZERO
        T_AD  = ZERO

      END DO Layer_Loop

    END SUBROUTINE ODPS_Compute_Predictor_MW_AD

  END SUBROUTINE  ODPS_Compute_Predictor_AD

!============================== END OF AD

!------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Compute_Predictor_ODAS
!
! PURPOSE:
!       Subroutine to compute ODAS predictors for water vapor line
!       absorption.
!
! CALLING SEQUENCE:
!       CALL ODPS_Compute_Predictor_ODAS( &
!         Temperature,    &  ! Input
!         vapor,          &  ! Inpt
!         Level_Pressure, &  ! Input
!         Pressure,       &  ! Input
!         secant_angle,   &  ! Input
!         Alpha,          &  ! Input
!         Alpha_C1,       &  ! Input
!         Alpha_C2,       &  ! Input
!         Predictor)         ! In/Output
!
! INPUT ARGUMENTS:
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!         Vapor    :     Water vapor mixing ratio profile
!                        UNITS:      g/kg
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       Level_Pressure : level pressure profile
!                        UNITS:      hPa
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(0:n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!            Pressure :  Layer pressure profile
!                        UNITS:      hPa
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       secang_angle  :  Secant sensor zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!      Alpha, Alpha_C1, Alpha_C2  :  Coefficients for converting water vapor integrated amount
!                        to ODAS water vapor regression space
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
! IN/OUTPUT ARGUMENTS:
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_Predictor_ODAS(&
    Temperature,    &
    Vapor,          &
    Level_Pressure, &
    Pressure,       &
    secant_angle,   &
    Alpha,          &
    Alpha_C1,       &
    Alpha_C2,       &
    Predictor)
     REAL(fp),                  INTENT(IN)     :: Temperature(:)
     REAL(fp),                  INTENT(IN)     :: Vapor(:)
     REAL(fp),                  INTENT(IN)     :: Level_Pressure(0:)
     REAL(fp),                  INTENT(IN)     :: Pressure(:)
     REAL(fp),                  INTENT(IN)     :: secant_angle(:)
     REAL(fp),                  INTENT(IN)     :: Alpha
     REAL(fp),                  INTENT(IN)     :: Alpha_C1
     REAL(fp),                  INTENT(IN)     :: Alpha_C2
     TYPE(ODPS_Predictor_type), INTENT(IN OUT) :: Predictor

     !Local
     REAL(fp), DIMENSION(0:Predictor%n_Layers) :: xL_t, xL_p
     REAL(fp) :: t2, p2, s_t, s_p, Inverse, dPonG, d_Absorber
     REAL(fp) :: Int_vapor_prev, Int_vapor, AveA, ap1
     INTEGER  :: i, k

     ! Regular predictors
     DO k = 1, Predictor%n_Layers
       t2 = Temperature(k)*Temperature(k)
       p2 = Pressure(k)*Pressure(k)
       Predictor%OX(k, 1)  = Temperature(k)
       Predictor%OX(k, 2)  = Pressure(k)
       Predictor%OX(k, 3)  = t2
       Predictor%OX(k, 4)  = p2
       Predictor%OX(k, 5)  = Temperature(k) * Pressure(k)
       Predictor%OX(k, 6)  = t2 * Pressure(k)
       Predictor%OX(k, 7)  = Temperature(k) * p2
       Predictor%OX(k, 8)  = t2 * p2
       Predictor%OX(k, 9)  = Pressure(k)**POINT_25
       Predictor%OX(k, 10) = Vapor(k)
       Predictor%OX(k, 11) = Vapor(k)/t2
       Predictor%OX(k, 14) = secant_angle(k)
     END DO

     ! Integrated predictors
     Int_vapor_prev = ZERO
     s_t = ZERO
     s_p = ZERO
     xL_t(0) = ZERO
     xL_p(0) = ZERO

     DO k = 1, Predictor%n_Layers

       dPonG = RECIPROCAL_GRAVITY * (Level_Pressure(k) - Level_Pressure(k-1))
       d_Absorber = dPonG*Vapor(k)*secant_angle(k)
       Int_vapor = Int_vapor_prev + d_Absorber
       AveA = POINT_5 * (Int_vapor_prev + Int_vapor)

       Predictor%dA(k) = d_Absorber

       s_t = s_t + ( Temperature( k ) * d_Absorber )  ! T*
       s_p = s_p + ( Pressure( k )    * d_Absorber )  ! P*

       IF ( Int_vapor > MINIMUM_ABSORBER_AMOUNT ) THEN
         Inverse = ONE / Int_vapor
       ELSE
         Inverse = ZERO
       END IF

       xL_t(k) = POINT_5 * s_t * Inverse
       xL_p(k) = POINT_5 * s_p * Inverse

       Predictor%OX(k, 12) = xL_t(k) + xL_t(k-1)
       Predictor%OX(k, 13) = xL_p(k) + xL_p(k-1)

       Ap1 =  LOG((aveA - Alpha_C2) / Alpha_C1) / &
          !  ----------------------------------------------
                             Alpha

       Predictor%Ap(k, 1) = Ap1
       DO i = 2, MAX_OPTRAN_ORDER
         Predictor%Ap(k, i) = Predictor%Ap(k, i-1) * Ap1
       END DO

       Int_vapor_prev = Int_vapor

       ! Save variables for TL and AD routines
       IF(Predictor%PAFV%OPTRAN)THEN
         Predictor%PAFV%dPonG(k)      = dPonG
         Predictor%PAFV%d_Absorber(k) = d_Absorber
         Predictor%PAFV%Int_vapor(k)  = Int_vapor
         Predictor%PAFV%AveA(k)       = AveA
         Predictor%PAFV%Inverse(k)    = Inverse
         Predictor%PAFV%s_t(k)        = s_t
         Predictor%PAFV%s_p(k)        = s_p
         Predictor%PAFV%Ap1(k)        = Ap1
       END IF

     END DO

  END SUBROUTINE ODPS_Compute_Predictor_ODAS

!------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Compute_Predictor_ODAS_TL
!
! PURPOSE:
!       Subroutine to compute TL ODAS predictors for water vapor line
!       absorption.
!
! CALLING SEQUENCE:
!    CALL ODPS_Compute_Predictor_ODAS_TL( &
!      Temperature,    &  ! Input
!      vapor,          &  ! Inpt
!      Pressure,       &  ! Input
!      secant_angle,   &  ! Input
!      Alpha,          &  ! Input
!      Alpha_C2,       &  ! Input
!      Predictor,      &  ! Input
!      Temperature_TL, &  ! Input
!      Vapor_TL,       &  ! Input
!      Predictor_TL)      ! Output
!
! INPUT ARGUMENTS:
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!         Vapor    :     Water vapor mixing ratio profile
!                        UNITS:      g/kg
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!            Pressure :  Layer pressure profile
!                        UNITS:      hPa
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       secang_angle  :  Secant sensor zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!      Alpha, Alpha_C2  :  Coefficients for converting water vapor integrated amount
!                        to ODAS water vapor regression space
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:  TL Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!         Vapor_TL    :  TL water vapor mixing ratio profile
!                        UNITS:      g/kg
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
! IN/OUTPUT ARGUMENTS:
!       Predictor_TL:   TL Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_Predictor_ODAS_TL( &
    Temperature,    &
    Vapor,          &
    Pressure,       &
    secant_angle,   &
    Alpha,          &
    Alpha_C2,       &
    Predictor,      &
    Temperature_TL, &
    Vapor_TL,       &
    Predictor_TL)

     REAL(fp),                          INTENT(IN)     :: Temperature(:)
     REAL(fp),                          INTENT(IN)     :: Vapor(:)
     REAL(fp),                          INTENT(IN)     :: Pressure(:)
     REAL(fp),                          INTENT(IN)     :: secant_angle(:)
     REAL(fp),                          INTENT(IN)     :: Alpha
     REAL(fp),                          INTENT(IN)     :: Alpha_C2
     TYPE(ODPS_Predictor_type), TARGET, INTENT(IN)     :: Predictor
     REAL(fp),                          INTENT(IN)     :: Temperature_TL(:)
     REAL(fp),                          INTENT(IN)     :: Vapor_TL(:)
     TYPE(ODPS_Predictor_type),         INTENT(IN OUT) :: Predictor_TL

     !Local
     REAL(fp), DIMENSION(0:Predictor%n_Layers) :: xL_t_TL, xL_p_TL
     REAL(fp) :: t2,   p2, &
                 t2_TL,    s_t_TL, s_p_TL, Inverse_TL, d_Absorber_TL
     REAL(fp) :: Int_vapor_prev_TL, Int_vapor_TL, AveA_TL, ap1_TL
     INTEGER  :: i, k
     TYPE(PAFV_type), POINTER  :: PAFV => NULL()

     ! short name
     PAFV => Predictor%PAFV

     ! Regular predictors
     DO k = 1, Predictor%n_Layers
       t2 = Temperature(k)*Temperature(k)
       p2 = Pressure(k)*Pressure(k)
       t2_TL = TWO*Temperature(k)*Temperature_TL(k)
       Predictor_TL%OX(k, 1)  = Temperature_TL(k)
       Predictor_TL%OX(k, 2)  = ZERO
       Predictor_TL%OX(k, 3)  = t2_TL
       Predictor_TL%OX(k, 4)  = ZERO
       Predictor_TL%OX(k, 5)  = Temperature_TL(k) * Pressure(k)
       Predictor_TL%OX(k, 6)  = t2_TL * Pressure(k)
       Predictor_TL%OX(k, 7)  = Temperature_TL(k) * p2
       Predictor_TL%OX(k, 8)  = t2_TL * p2
       Predictor_TL%OX(k, 9)  = ZERO
       Predictor_TL%OX(k, 10) = Vapor_TL(k)
       Predictor_TL%OX(k, 11) = Vapor_TL(k)/t2 - (Vapor(k)/t2**2)*t2_TL
       Predictor_TL%OX(k, 14) = ZERO
     END DO

     ! Integrated predictors

     Int_vapor_prev_TL = ZERO
     s_t_TL = ZERO
     s_p_TL = ZERO
     xL_t_TL(0) = ZERO
     xL_p_TL(0) = ZERO

     DO k = 1, Predictor%n_Layers

       d_Absorber_TL = PAFV%dPonG(k)*Vapor_TL(k)*secant_angle(k)

       Int_vapor_TL  = Int_vapor_prev_TL + d_Absorber_TL
       AveA_TL = POINT_5 * (Int_vapor_prev_TL + Int_vapor_TL)

       Predictor_TL%dA(k) = d_Absorber_TL

       s_t_TL = s_t_TL + ( Temperature_TL( k )*PAFV%d_Absorber(k) +  Temperature( k )*d_Absorber_TL)
       s_p_TL = s_p_TL + ( Pressure( k )*d_Absorber_TL )

       IF ( PAFV%Int_vapor(k) > MINIMUM_ABSORBER_AMOUNT ) THEN
         Inverse_TL = -(ONE/PAFV%Int_vapor(k)**2)*Int_vapor_TL
       ELSE
         Inverse_TL = ZERO
       END IF

       xL_t_TL(k) = POINT_5 * (s_t_TL*PAFV%Inverse(k) + PAFV%s_t(k)*Inverse_TL)
       xL_p_TL(k) = POINT_5 * (s_p_TL*PAFV%Inverse(k) + PAFV%s_p(k)*Inverse_TL)

       Predictor_TL%OX(k, 12) = xL_t_TL(k) + xL_t_TL(k-1)
       Predictor_TL%OX(k, 13) = xL_p_TL(k) + xL_p_TL(k-1)

       Ap1_TL =           aveA_TL / &
        !        -----------------------------------
                 ( Alpha * (PAFV%aveA(k) - Alpha_C2 ) )

       Predictor_TL%Ap(k, 1) = Ap1_TL

       DO i = 2, MAX_OPTRAN_ORDER
         Predictor_TL%Ap(k, i) = Predictor_TL%Ap(k, i-1)*PAFV%Ap1(k) + Predictor%Ap(k, i-1)*Ap1_TL
       END DO

       Int_vapor_prev_TL = Int_vapor_TL

     END DO

  END SUBROUTINE ODPS_Compute_Predictor_ODAS_TL

!------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Compute_Predictor_ODAS_AD
!
! PURPOSE:
!       Subroutine to compute AD ODAS predictors for water vapor line
!       absorption.
!
! CALLING SEQUENCE:
!    CALL ODPS_Compute_Predictor_ODAS_AD( &
!      Temperature,    &  ! Input
!      vapor,          &  ! Inpt
!      Pressure,       &  ! Input
!      secant_angle,   &  ! Input
!      Alpha,          &  ! Input
!      Alpha_C2,       &  ! Input
!      Predictor,      &  ! Input
!      Predictor_AD,   &  ! Input
!      Temperature_AD, &  ! Input
!      Vapor_AD)          ! Input
!
! INPUT ARGUMENTS:
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!         Vapor    :     Water vapor mixing ratio profile
!                        UNITS:      g/kg
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!            Pressure :  Layer pressure profile
!                        UNITS:      hPa
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!       secang_angle  :  Secant sensor zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(IN)
!
!      Alpha, Alpha_C2  :  Coefficients for converting water vapor integrated amount
!                        to ODAS water vapor regression space
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Predictor_aD:   AD Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       ODPS_Predictor_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! IN/OUTPUT ARGUMENTS:
!       Temperature_AD:  AD Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(INoUT)
!
!         Vapor_AD    :  AD water vapor mixing ratio profile
!                        UNITS:      g/kg
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array
!                        ATTRIBUTES: INTENT(INOUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_Predictor_ODAS_AD( &
    Temperature,    &
    Vapor,          &
    Pressure,       &
    secant_angle,   &
    Alpha,          &
    Alpha_C2,       &
    Predictor,      &
    Predictor_AD,   &
    Temperature_AD, &
    Vapor_AD)

     REAL(fp),                          INTENT(IN)     :: Temperature(:)
     REAL(fp),                          INTENT(IN)     :: Vapor(:)
     REAL(fp),                          INTENT(IN)     :: Pressure(:)
     REAL(fp),                          INTENT(IN)     :: secant_angle(:)
     REAL(fp),                          INTENT(IN)     :: Alpha
     REAL(fp),                          INTENT(IN)     :: Alpha_C2
     TYPE(ODPS_Predictor_type), TARGET, INTENT(IN)     :: Predictor
     TYPE(ODPS_Predictor_type),         INTENT(IN OUT) :: Predictor_AD
     REAL(fp),                          INTENT(IN OUT) :: Temperature_AD(:)
     REAL(fp),                          INTENT(IN OUT) :: Vapor_AD(:)

     !Local
     REAL(fp), DIMENSION(0:Predictor%n_Layers) :: xL_t_AD, xL_p_AD
     REAL(fp) :: t2, p2
     REAL(fp) :: t2_AD, s_t_AD, s_p_AD, Inverse_AD, d_Absorber_AD
     REAL(fp) :: Int_vapor_prev_AD, Int_vapor_AD, AveA_AD, ap1_AD
     INTEGER  :: i, k
     TYPE(PAFV_type), POINTER  :: PAFV => NULL()

     ! short name
     PAFV => Predictor%PAFV

     ! Integrated predictors

     ! --- AD part
     Int_vapor_prev_AD = ZERO
     Int_vapor_AD      = ZERO
     Ap1_AD            = ZERO
     AveA_AD           = ZERO
     xL_t_AD(Predictor%n_Layers) = ZERO
     xL_p_AD(Predictor%n_Layers) = ZERO
     s_t_AD            = ZERO
     s_p_AD            = ZERO
     Inverse_AD        = ZERO
     d_Absorber_AD     = ZERO
     DO k = Predictor%n_Layers, 1, -1

       Int_vapor_AD = Int_vapor_AD + Int_vapor_prev_AD
       Int_vapor_prev_AD = ZERO

       DO i = MAX_OPTRAN_ORDER, 2, -1
         Ap1_AD = Ap1_AD + Predictor%Ap(k, i-1)*Predictor_AD%Ap(k, i)
         Predictor_AD%Ap(k, i-1) = Predictor_AD%Ap(k, i-1) + PAFV%Ap1(k)*Predictor_AD%Ap(k, i)
         Predictor_AD%Ap(k, i) = ZERO
       END DO

       Ap1_AD = Ap1_AD + Predictor_AD%Ap(k, 1)
       Predictor_AD%Ap(k, 1) = ZERO

       aveA_AD = aveA_AD + &
                            Ap1_AD / &
        !        -----------------------------------
                 ( Alpha * (PAFV%aveA(k) - Alpha_C2 ) )
       Ap1_AD = ZERO

       xL_t_AD(k)   = xL_t_AD(k)   + Predictor_AD%OX(k, 12)
       xL_t_AD(k-1) =                Predictor_AD%OX(k, 12)  ! combine with initialization for xL_t_AD(k-1)
       Predictor_AD%OX(k, 12) = ZERO
       xL_p_AD(k)   = xL_p_AD(k)   + Predictor_AD%OX(k, 13)
       xL_p_AD(k-1) =                Predictor_AD%OX(k, 13) ! combine with initialization for xL_p_AD(k-1)
       Predictor_AD%OX(k, 13) = ZERO

       s_p_AD     = s_p_AD     + POINT_5*PAFV%Inverse(k)*xL_p_AD(k)
       Inverse_AD = Inverse_AD + POINT_5*PAFV%s_p(k)*xL_p_AD(k)
       s_t_AD     = s_t_AD     + POINT_5*PAFV%Inverse(k)*xL_t_AD(k)
       Inverse_AD = Inverse_AD + POINT_5*PAFV%s_t(k)*xL_t_AD(k)
       xL_t_AD(k) = ZERO
       xL_p_AD(k) = ZERO

       IF ( PAFV%Int_vapor(k) > MINIMUM_ABSORBER_AMOUNT ) THEN
         Int_vapor_AD = Int_vapor_AD -(ONE/PAFV%Int_vapor(k)**2)*Inverse_AD
         Inverse_AD = ZERO
       ELSE
         Inverse_AD = ZERO
       END IF

       d_Absorber_AD = d_Absorber_AD + Pressure( k )*s_p_AD &
                                     + Temperature( k )*s_t_AD
       Temperature_AD( k ) = Temperature_AD( k ) + PAFV%d_Absorber(k)*s_t_AD

       d_Absorber_AD = d_Absorber_AD + Predictor_AD%dA(k)
       Predictor_AD%dA(k) = ZERO

       Int_vapor_prev_AD = Int_vapor_prev_AD + POINT_5 * AveA_AD
       Int_vapor_AD = Int_vapor_AD + POINT_5 *  AveA_AD
       AveA_AD = ZERO

       Int_vapor_prev_AD = Int_vapor_prev_AD + Int_vapor_AD
       d_Absorber_AD = d_Absorber_AD + Int_vapor_AD
       Int_vapor_AD = ZERO

       Vapor_AD(k) = Vapor_AD(k) + PAFV%dPonG(k)*d_Absorber_AD*secant_angle(k)
       d_Absorber_AD = ZERO

     END DO


     ! AD Regular predictors
     DO k = Predictor%n_Layers, 1, -1
       t2 = Temperature(k)*Temperature(k)
       p2 = Pressure(k)*Pressure(k)

       Temperature_AD(k) = Temperature_AD(k) &
                           +              Predictor_AD%OX(k, 1) &
                           + Pressure(k)* Predictor_AD%OX(k, 5) &
                           +          p2* Predictor_AD%OX(k, 7)
       t2_AD =                            Predictor_AD%OX(k, 3) &
               +             Pressure(k)* Predictor_AD%OX(k, 6) &
               +                      p2* Predictor_AD%OX(k, 8) &
               -        (Vapor(k)/t2**2)* Predictor_AD%OX(k, 11)

       Vapor_AD(k) = Vapor_AD(k) &
                     + Predictor_AD%OX(k, 10) &
                     + Predictor_AD%OX(k, 11)/t2

       Predictor_AD%OX(k, 1:11) = ZERO
       Predictor_AD%OX(k,   14) = ZERO

       Temperature_AD(k) = Temperature_AD(k) + TWO*Temperature(k)*t2_AD

     END DO

  END SUBROUTINE ODPS_Compute_Predictor_ODAS_AD


  PURE FUNCTION ODPS_Get_max_n_Predictors( Group_Index ) RESULT( max_n_Predictors )
    INTEGER, INTENT( IN ) :: Group_Index
    INTEGER :: max_n_Predictors
    max_n_Predictors = MAX_N_PREDICTORS_G( Group_Index )
  END FUNCTION ODPS_Get_max_n_Predictors


  PURE FUNCTION ODPS_Get_n_Components( Group_Index ) RESULT( n_Components )
    INTEGER, INTENT( IN ) :: Group_Index
    INTEGER :: n_Components
    n_Components = N_COMPONENTS_G( Group_Index )
  END FUNCTION ODPS_Get_n_Components


  PURE FUNCTION ODPS_Get_n_Absorbers( Group_Index ) RESULT( n_Absorbers )
    INTEGER, INTENT( IN ) :: Group_Index
    INTEGER :: n_Absorbers
    n_Absorbers = N_ABSORBERS_G( Group_Index )
  END FUNCTION ODPS_Get_n_Absorbers


  PURE FUNCTION ODPS_Get_Component_ID(Component_Index, Group_Index) RESULT( Component_ID )
    INTEGER, INTENT( IN ) :: Component_Index
    INTEGER, INTENT( IN ) :: Group_Index
    INTEGER :: Component_ID
    SELECT CASE( Group_Index )
      CASE( GROUP_1 )
         Component_ID = COMPONENT_ID_MAP_G1(Component_Index)
      CASE( GROUP_2 )
         Component_ID = COMPONENT_ID_MAP_G2(Component_Index)
      CASE( GROUP_3 )
         Component_ID = COMPONENT_ID_MAP_G3(Component_Index)
    END SELECT
  END FUNCTION ODPS_Get_Component_ID


  PURE FUNCTION ODPS_Get_Absorber_ID(Absorber_Index, Group_Index) RESULT(  Absorber_ID )
    INTEGER, INTENT( IN ) :: Absorber_Index
    INTEGER, INTENT( IN ) :: Group_Index
    INTEGER :: Absorber_ID
    SELECT CASE( Group_Index )
      CASE( GROUP_1 )
          Absorber_ID = ABSORBER_ID_MAP_G1(Absorber_Index)
      CASE( GROUP_2 )
          Absorber_ID = ABSORBER_ID_MAP_G2(Absorber_Index)
      CASE( GROUP_3 )
          Absorber_ID = ABSORBER_ID_MAP_G3(Absorber_Index)
    END SELECT
  END FUNCTION ODPS_Get_Absorber_ID


  PURE FUNCTION ODPS_Get_Ozone_Component_ID(Group_Index) RESULT( Ozone_Component_ID )
    INTEGER, INTENT(IN) :: Group_Index
    INTEGER :: Ozone_Component_ID
    IF( Group_Index == GROUP_1 .OR. Group_Index == GROUP_1)THEN
      Ozone_Component_ID = OZO_ComID
    ELSE
      Ozone_Component_ID = -1
    END IF
  END FUNCTION ODPS_Get_Ozone_Component_ID


  ! This function gets a flag (true or false) indicating the
  ! need for saveing the FWD variables
  PURE FUNCTION ODPS_Get_SaveFWVFlag() RESULT(Flag)
    LOGICAL :: Flag
    Flag = .TRUE.
  END FUNCTION ODPS_Get_SaveFWVFlag

END MODULE ODPS_Predictor
