!
! CRTM_Atmosphere_Define
!
! Module defining the CRTM Atmosphere structure and containing routines to
! manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Feb-2004
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_Atmosphere_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: DEFAULT_N_SIGFIG, &
                                   OPERATOR(.EqualTo.), &
                                   Compares_Within_Tolerance
  USE MinMax_Find_Utility  , ONLY: MaxVal_Find, &
                                   MaxDiff_Find
  USE File_Utility         , ONLY: File_Open, File_Exists
  USE Binary_File_Utility  , ONLY: Open_Binary_File      , &
                                   WriteGAtts_Binary_File, &
                                   ReadGAtts_Binary_File
  USE CRTM_Parameters      , ONLY: MAX_N_LAYERS
  USE CRTM_Cloud_Define    , ONLY: N_VALID_CLOUD_TYPES, &
                                   INVALID_CLOUD, &
                                   WATER_CLOUD, &
                                   ICE_CLOUD, &
                                   RAIN_CLOUD, &
                                   SNOW_CLOUD, &
                                   GRAUPEL_CLOUD, &
                                   HAIL_CLOUD, &
                                   CLOUD_TYPE_NAME, &
                                   CRTM_Cloud_type, &
                                   OPERATOR(==), &
                                   OPERATOR(+), &
                                   OPERATOR(-), &
                                   CRTM_Cloud_Associated, &
                                   CRTM_Cloud_Destroy, &
                                   CRTM_Cloud_Create, &
                                   CRTM_Cloud_AddLayerCopy, &
                                   CRTM_Cloud_Zero, &
                                   CRTM_Cloud_IsValid, &
                                   CRTM_Cloud_Inspect, &
                                   CRTM_Cloud_MaxInspect, &
                                   CRTM_Cloud_CompareInspect, &
                                   CRTM_Cloud_DefineVersion, &
                                   CRTM_Cloud_Compare, &
                                   CRTM_Cloud_SetLayers, &
                                   CRTM_Cloud_ReadFile, &
                                   CRTM_Cloud_WriteFile
  USE CRTM_Aerosol_Define  , ONLY: N_VALID_AEROSOL_TYPES, &
                                   INVALID_AEROSOL       , &
                                   DUST_AEROSOL          , &
                                   SEASALT_SSAM_AEROSOL  , &
                                   SEASALT_SSCM1_AEROSOL , &
                                   SEASALT_SSCM2_AEROSOL , &
                                   SEASALT_SSCM3_AEROSOL , &
                                   ORGANIC_CARBON_AEROSOL, &
                                   BLACK_CARBON_AEROSOL  , &
                                   SULFATE_AEROSOL       , &
                                   AEROSOL_TYPE_NAME, &
                                   CRTM_Aerosol_type, &
                                   OPERATOR(==), &
                                   OPERATOR(+), &
                                   OPERATOR(-), &
                                   CRTM_Aerosol_Associated, &
                                   CRTM_Aerosol_Destroy, &
                                   CRTM_Aerosol_Create, &
                                   CRTM_Aerosol_AddLayerCopy, &
                                   CRTM_Aerosol_Zero, &
                                   CRTM_Aerosol_IsValid, &
                                   CRTM_Aerosol_Inspect, &
                                   CRTM_Aerosol_MaxInspect, &
                                   CRTM_Aerosol_CompareInspect, &
                                   CRTM_Aerosol_DefineVersion, &
                                   CRTM_Aerosol_Compare, &
                                   CRTM_Aerosol_SetLayers, &
                                   CRTM_Aerosol_ReadFile, &
                                   CRTM_Aerosol_WriteFile
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(+)
  PUBLIC :: OPERATOR(-)
  ! Cloud entities
  ! ...Parameters
  PUBLIC :: N_VALID_CLOUD_TYPES
  PUBLIC :: INVALID_CLOUD
  PUBLIC :: WATER_CLOUD
  PUBLIC :: ICE_CLOUD
  PUBLIC :: RAIN_CLOUD
  PUBLIC :: SNOW_CLOUD
  PUBLIC :: GRAUPEL_CLOUD
  PUBLIC :: HAIL_CLOUD
  PUBLIC :: CLOUD_TYPE_NAME
  ! ...Structures
  PUBLIC :: CRTM_Cloud_type
  ! ...Procedures
  PUBLIC :: CRTM_Cloud_Associated
  PUBLIC :: CRTM_Cloud_Destroy
  PUBLIC :: CRTM_Cloud_Create
  PUBLIC :: CRTM_Cloud_Zero
  PUBLIC :: CRTM_Cloud_IsValid
  PUBLIC :: CRTM_Cloud_Inspect
  PUBLIC :: CRTM_Cloud_MaxInspect
  PUBLIC :: CRTM_Cloud_CompareInspect
  PUBLIC :: CRTM_Cloud_DefineVersion
  PUBLIC :: CRTM_Cloud_Compare
  PUBLIC :: CRTM_Cloud_SetLayers
  ! Aerosol entities
  ! ...Parameters
  PUBLIC :: N_VALID_AEROSOL_TYPES
  PUBLIC :: INVALID_AEROSOL
  PUBLIC :: DUST_AEROSOL
  PUBLIC :: SEASALT_SSAM_AEROSOL
  PUBLIC :: SEASALT_SSCM1_AEROSOL
  PUBLIC :: SEASALT_SSCM2_AEROSOL
  PUBLIC :: SEASALT_SSCM3_AEROSOL
  PUBLIC :: ORGANIC_CARBON_AEROSOL
  PUBLIC :: BLACK_CARBON_AEROSOL
  PUBLIC :: SULFATE_AEROSOL
  PUBLIC :: AEROSOL_TYPE_NAME
  ! ...Structures
  PUBLIC :: CRTM_Aerosol_type
  ! ...Procedures
  PUBLIC :: CRTM_Aerosol_Associated
  PUBLIC :: CRTM_Aerosol_Destroy
  PUBLIC :: CRTM_Aerosol_Create
  PUBLIC :: CRTM_Aerosol_Zero
  PUBLIC :: CRTM_Aerosol_IsValid
  PUBLIC :: CRTM_Aerosol_Inspect
  PUBLIC :: CRTM_Aerosol_MaxInspect
  PUBLIC :: CRTM_Aerosol_CompareInspect
  PUBLIC :: CRTM_Aerosol_DefineVersion
  PUBLIC :: CRTM_Aerosol_Compare
  PUBLIC :: CRTM_Aerosol_SetLayers
  ! Atmosphere entities
  ! ...Parameters
  PUBLIC :: N_VALID_ABSORBER_IDS
  PUBLIC :: INVALID_ABSORBER_ID
  PUBLIC ::   H2O_ID,    CO2_ID,     O3_ID,    N2O_ID,     CO_ID,    CH4_ID, &
               O2_ID,     NO_ID,    SO2_ID,    NO2_ID,    NH3_ID,   HNO3_ID, &
               OH_ID,     HF_ID,    HCL_ID,    HBR_ID,     HI_ID,    CLO_ID, &
              OCS_ID,   H2CO_ID,   HOCL_ID,     N2_ID,    HCN_ID,   CH3L_ID, &
             H2O2_ID,   C2H2_ID,   C2H6_ID,    PH3_ID,   COF2_ID,    SF6_ID, &
              H2S_ID,  HCOOH_ID
  PUBLIC :: ABSORBER_ID_NAME
  PUBLIC :: N_VALID_ABSORBER_UNITS
  PUBLIC ::       INVALID_ABSORBER_UNITS
  PUBLIC ::    VOLUME_MIXING_RATIO_UNITS
  PUBLIC ::         NUMBER_DENSITY_UNITS
  PUBLIC ::      MASS_MIXING_RATIO_UNITS
  PUBLIC ::           MASS_DENSITY_UNITS
  PUBLIC ::       PARTIAL_PRESSURE_UNITS
  PUBLIC :: DEWPOINT_TEMPERATURE_K_UNITS ! H2O only
  PUBLIC :: DEWPOINT_TEMPERATURE_C_UNITS ! H2O only
  PUBLIC ::      RELATIVE_HUMIDITY_UNITS ! H2O only
  PUBLIC ::        SPECIFIC_AMOUNT_UNITS
  PUBLIC ::        INTEGRATED_PATH_UNITS
  PUBLIC :: ABSORBER_UNITS_NAME
  PUBLIC :: H2O_ONLY_UNITS_FLAG
  PUBLIC :: N_VALID_CLIMATOLOGY_MODELS
  PUBLIC :: INVALID_MODEL
  PUBLIC :: TROPICAL
  PUBLIC :: MIDLATITUDE_SUMMER
  PUBLIC :: MIDLATITUDE_WINTER
  PUBLIC :: SUBARCTIC_SUMMER
  PUBLIC :: SUBARCTIC_WINTER
  PUBLIC :: US_STANDARD_ATMOSPHERE
  PUBLIC :: CLIMATOLOGY_MODEL_NAME
  ! ...Structures
  PUBLIC :: CRTM_Atmosphere_type
  ! ...Procedures
  PUBLIC :: CRTM_Atmosphere_Associated
  PUBLIC :: CRTM_Atmosphere_Destroy
  PUBLIC :: CRTM_Atmosphere_Create
  PUBLIC :: CRTM_Atmosphere_AddLayerCopy
  PUBLIC :: CRTM_Atmosphere_Zero
  PUBLIC :: CRTM_Atmosphere_IsValid
  PUBLIC :: CRTM_Atmosphere_Inspect
  PUBLIC :: CRTM_Atmosphere_MaxInspect
  PUBLIC :: CRTM_Atmosphere_CompareInspect
  PUBLIC :: CRTM_Atmosphere_DefineVersion
  PUBLIC :: CRTM_Atmosphere_Compare
  PUBLIC :: CRTM_Atmosphere_SetLayers
  PUBLIC :: CRTM_Atmosphere_InquireFile
  PUBLIC :: CRTM_Atmosphere_ReadFile
  PUBLIC :: CRTM_Atmosphere_WriteFile
  ! ...Utilities
  PUBLIC :: CRTM_Get_AbsorberIdx
  PUBLIC :: CRTM_Get_PressureLevelIdx


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_Atmosphere_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE CRTM_Atmosphere_Add
  END INTERFACE OPERATOR(+)

  INTERFACE OPERATOR(-)
    MODULE PROCEDURE CRTM_Atmosphere_Subtract
  END INTERFACE OPERATOR(-)

  INTERFACE CRTM_Atmosphere_Inspect
    MODULE PROCEDURE Scalar_Inspect
    MODULE PROCEDURE Rank1_Inspect
    MODULE PROCEDURE Rank2_Inspect
  END INTERFACE CRTM_Atmosphere_Inspect

  INTERFACE CRTM_Atmosphere_MaxInspect
    MODULE PROCEDURE Scalar_MaxInspect
    MODULE PROCEDURE Rank1_MaxInspect
    MODULE PROCEDURE Rank2_MaxInspect
  END INTERFACE CRTM_Atmosphere_MaxInspect

  INTERFACE CRTM_Atmosphere_CompareInspect
    MODULE PROCEDURE Scalar_CompareInspect
    MODULE PROCEDURE Rank1_CompareInspect
    MODULE PROCEDURE Rank2_CompareInspect
  END INTERFACE CRTM_Atmosphere_CompareInspect

  INTERFACE CRTM_Atmosphere_ReadFile
    MODULE PROCEDURE Read_Atmosphere_Rank1
    MODULE PROCEDURE Read_Atmosphere_Rank2
  END INTERFACE CRTM_Atmosphere_ReadFile

  INTERFACE CRTM_Atmosphere_WriteFile
    MODULE PROCEDURE Write_Atmosphere_Rank1
    MODULE PROCEDURE Write_Atmosphere_Rank2
  END INTERFACE CRTM_Atmosphere_WriteFile


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Atmosphere_Define.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'

  ! The absorber IDs. Use HITRAN definitions
  INTEGER, PARAMETER :: N_VALID_ABSORBER_IDS = 32
  INTEGER, PARAMETER :: INVALID_ABSORBER_ID =  0
  INTEGER, PARAMETER ::   H2O_ID =  1
  INTEGER, PARAMETER ::   CO2_ID =  2
  INTEGER, PARAMETER ::    O3_ID =  3
  INTEGER, PARAMETER ::   N2O_ID =  4
  INTEGER, PARAMETER ::    CO_ID =  5
  INTEGER, PARAMETER ::   CH4_ID =  6
  INTEGER, PARAMETER ::    O2_ID =  7
  INTEGER, PARAMETER ::    NO_ID =  8
  INTEGER, PARAMETER ::   SO2_ID =  9
  INTEGER, PARAMETER ::   NO2_ID = 10
  INTEGER, PARAMETER ::   NH3_ID = 11
  INTEGER, PARAMETER ::  HNO3_ID = 12
  INTEGER, PARAMETER ::    OH_ID = 13
  INTEGER, PARAMETER ::    HF_ID = 14
  INTEGER, PARAMETER ::   HCl_ID = 15
  INTEGER, PARAMETER ::   HBr_ID = 16
  INTEGER, PARAMETER ::    HI_ID = 17
  INTEGER, PARAMETER ::   ClO_ID = 18
  INTEGER, PARAMETER ::   OCS_ID = 19
  INTEGER, PARAMETER ::  H2CO_ID = 20
  INTEGER, PARAMETER ::  HOCl_ID = 21
  INTEGER, PARAMETER ::    N2_ID = 22
  INTEGER, PARAMETER ::   HCN_ID = 23
  INTEGER, PARAMETER ::  CH3l_ID = 24
  INTEGER, PARAMETER ::  H2O2_ID = 25
  INTEGER, PARAMETER ::  C2H2_ID = 26
  INTEGER, PARAMETER ::  C2H6_ID = 27
  INTEGER, PARAMETER ::   PH3_ID = 28
  INTEGER, PARAMETER ::  COF2_ID = 29
  INTEGER, PARAMETER ::   SF6_ID = 30
  INTEGER, PARAMETER ::   H2S_ID = 31
  INTEGER, PARAMETER :: HCOOH_ID = 32
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_VALID_ABSORBER_IDS ) :: &
    ABSORBER_ID_NAME = (/ 'Invalid', &
                          'H2O    ', 'CO2    ', 'O3     ', 'N2O    ', &
                          'CO     ', 'CH4    ', 'O2     ', 'NO     ', &
                          'SO2    ', 'NO2    ', 'NH3    ', 'HNO3   ', &
                          'OH     ', 'HF     ', 'HCl    ', 'HBr    ', &
                          'HI     ', 'ClO    ', 'OCS    ', 'H2CO   ', &
                          'HOCl   ', 'N2     ', 'HCN    ', 'CH3Cl  ', &
                          'H2O2   ', 'C2H2   ', 'C2H6   ', 'PH3    ', &
                          'COF2   ', 'SF6    ', 'H2S    ', 'HCOOH  ' /)

  ! The absorber units. Use LBLRTM definitions and then some.
  INTEGER, PARAMETER :: N_VALID_ABSORBER_UNITS = 10
  INTEGER, PARAMETER ::       INVALID_ABSORBER_UNITS =  0
  INTEGER, PARAMETER ::    VOLUME_MIXING_RATIO_UNITS =  1
  INTEGER, PARAMETER ::         NUMBER_DENSITY_UNITS =  2
  INTEGER, PARAMETER ::      MASS_MIXING_RATIO_UNITS =  3
  INTEGER, PARAMETER ::           MASS_DENSITY_UNITS =  4
  INTEGER, PARAMETER ::       PARTIAL_PRESSURE_UNITS =  5
  INTEGER, PARAMETER :: DEWPOINT_TEMPERATURE_K_UNITS =  6 ! H2O only
  INTEGER, PARAMETER :: DEWPOINT_TEMPERATURE_C_UNITS =  7 ! H2O only
  INTEGER, PARAMETER ::      RELATIVE_HUMIDITY_UNITS =  8 ! H2O only
  INTEGER, PARAMETER ::        SPECIFIC_AMOUNT_UNITS =  9
  INTEGER, PARAMETER ::        INTEGRATED_PATH_UNITS = 10
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_VALID_ABSORBER_UNITS ) :: &
    ABSORBER_UNITS_NAME = (/ 'Invalid units                      ', &
                             'Volume mixing ratio, ppmv          ', &
                             'Number density, cm^-3              ', &
                             'Mass mixing ratio, g/kg            ', &
                             'Mass density, g.m^-3               ', &
                             'Partial pressure, hPa              ', &
                             'Dewpoint temperature, K  (H2O ONLY)', &
                             'Dewpoint temperature, C  (H2O ONLY)', &
                             'Relative humidity, %     (H2O ONLY)', &
                             'Specific amount, g/g               ', &
                             'Integrated path, mm                ' /)
  INTEGER, PARAMETER, DIMENSION( 0:N_VALID_ABSORBER_UNITS ) :: &
    H2O_ONLY_UNITS_FLAG = (/ 0, &  ! None
                             0, &  ! Volume mixing ratio, ppmv
                             0, &  ! Number density, cm^-3
                             0, &  ! Mass mixing ratio, g/kg
                             0, &  ! Mass density, g.m^-3
                             0, &  ! Partial pressure, hPa
                             1, &  ! Dewpoint temperature, K  (H2O ONLY)
                             1, &  ! Dewpoint temperature, C  (H2O ONLY)
                             1, &  ! Relative humidity, %     (H2O ONLY)
                             0, &  ! Specific amount, g/g
                             0 /)  ! Integrated path, mm

  ! The climatology models
  INTEGER, PARAMETER :: N_VALID_CLIMATOLOGY_MODELS = 6
  INTEGER, PARAMETER :: INVALID_MODEL          = 0
  INTEGER, PARAMETER :: TROPICAL               = 1
  INTEGER, PARAMETER :: MIDLATITUDE_SUMMER     = 2
  INTEGER, PARAMETER :: MIDLATITUDE_WINTER     = 3
  INTEGER, PARAMETER :: SUBARCTIC_SUMMER       = 4
  INTEGER, PARAMETER :: SUBARCTIC_WINTER       = 5
  INTEGER, PARAMETER :: US_STANDARD_ATMOSPHERE = 6
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_VALID_CLIMATOLOGY_MODELS ) :: &
    CLIMATOLOGY_MODEL_NAME = (/ 'Invalid                 ', &
                                'Tropical                ', &
                                'Midlatitude summer      ', &
                                'Midlatitude winter      ', &
                                'Subarctic summer        ', &
                                'Subarctic winter        ', &
                                'U.S. Standard Atmosphere' /)
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! File status on close after write error
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'


  ! -------------------------------
  ! Atmosphere structure definition
  ! -------------------------------
  !:tdoc+:
  TYPE :: CRTM_Atmosphere_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimension values
    INTEGER :: Max_Layers   = 0  ! K dimension
    INTEGER :: n_Layers     = 0  ! Kuse dimension
    INTEGER :: n_Absorbers  = 0  ! J dimension
    INTEGER :: Max_Clouds   = 0  ! Nc dimension
    INTEGER :: n_Clouds     = 0  ! NcUse dimension
    INTEGER :: Max_Aerosols = 0  ! Na dimension
    INTEGER :: n_Aerosols   = 0  ! NaUse dimension
    ! Number of added layers
    INTEGER :: n_Added_Layers = 0
    ! Climatology model associated with the profile
    INTEGER :: Climatology = US_STANDARD_ATMOSPHERE
    ! Absorber ID and units
    INTEGER, ALLOCATABLE :: Absorber_ID(:)    ! J
    INTEGER, ALLOCATABLE :: Absorber_Units(:) ! J
    ! Profile LEVEL and LAYER quantities
    REAL(fp), ALLOCATABLE :: Level_Pressure(:)  ! 0:K
    REAL(fp), ALLOCATABLE :: Pressure(:)        ! K
    REAL(fp), ALLOCATABLE :: Temperature(:)     ! K
    REAL(fp), ALLOCATABLE :: Absorber(:,:)      ! K x J
    ! Clouds associated with each profile
    TYPE(CRTM_Cloud_type),   ALLOCATABLE :: Cloud(:)    ! Nc
    ! Aerosols associated with each profile
    TYPE(CRTM_Aerosol_type), ALLOCATABLE :: Aerosol(:)  ! Na
  END TYPE CRTM_Atmosphere_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a CRTM Atmosphere object.
!
! CALLING SEQUENCE:
!       Status = CRTM_Atmosphere_Associated( Atm )
!
! OBJECTS:
!       Atm:       Atmosphere structure which is to have its member's
!                  status tested.
!                  UNITS:      N/A
!                  TYPE:       CRTM_Atmosphere_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:    The return value is a logical value indicating the
!                  status of the Atmosphere members.
!                    .TRUE.  - if the array components are allocated.
!                    .FALSE. - if the array components are not allocated.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Atmosphere_Associated( Atm ) RESULT( Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atm
    ! Function result
    LOGICAL :: Status

    Status = Atm%Is_Allocated
    ! ...Clouds
    IF ( Atm%n_Clouds > 0 .AND. ALLOCATED(Atm%Cloud) ) &
      Status = Status .AND. ALL(CRTM_Cloud_Associated(Atm%Cloud))
    ! ...Aerosols
    IF ( Atm%n_Aerosols > 0 .AND. ALLOCATED(Atm%Aerosol) ) &
      Status = Status .AND. ALL(CRTM_Aerosol_Associated(Atm%Aerosol))

  END FUNCTION CRTM_Atmosphere_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize CRTM Atmosphere objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_Atmosphere_Destroy( Atm )
!
! OBJECTS:
!       Atm:          Re-initialized Atmosphere structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Atmosphere_Destroy( Atm )
    TYPE(CRTM_Atmosphere_type), INTENT(OUT) :: Atm
    Atm%Is_Allocated = .FALSE.
  END SUBROUTINE CRTM_Atmosphere_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of the CRTM Atmosphere object.
!
! CALLING SEQUENCE:
!       CALL CRTM_Atmosphere_Create( Atm        , &
!                                    n_Layers   , &
!                                    n_Absorbers, &
!                                    n_Clouds   , &
!                                    n_Aerosols   )
!
! OBJECTS:
!       Atm:          Atmosphere structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Layers:     Number of layers dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as atmosphere object
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Absorbers:  Number of absorbers dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as atmosphere object
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Clouds:     Number of clouds dimension.
!                     Can be = 0 (i.e. clear sky).
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as atmosphere object
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Aerosols:   Number of aerosols dimension.
!                     Can be = 0 (i.e. no aerosols).
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as atmosphere object
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Atmosphere_Create( &
    Atm        , &  ! Output
    n_Layers   , &  ! Input
    n_Absorbers, &  ! Input
    n_Clouds   , &  ! Input
    n_Aerosols   )  ! Input
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(OUT) :: Atm
    INTEGER                   , INTENT(IN)  :: n_Layers
    INTEGER                   , INTENT(IN)  :: n_Absorbers
    INTEGER                   , INTENT(IN)  :: n_Clouds
    INTEGER                   , INTENT(IN)  :: n_Aerosols
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Layers < 1 .OR. n_Absorbers < 1 ) RETURN
    IF ( n_Clouds < 0 .OR. n_Aerosols < 0 ) RETURN

    ! Perform the allocation
    ALLOCATE( Atm%Absorber_ID( n_Absorbers ), &
              Atm%Absorber_Units( n_Absorbers ), &
              Atm%Level_Pressure( 0:n_Layers ), &
              Atm%Pressure( n_Layers ), &
              Atm%Temperature( n_Layers ), &
              Atm%Absorber( n_Layers, n_Absorbers ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Perform the substructure allocation
    ! ...Cloud array
    IF ( n_Clouds > 0 ) THEN
      ! Allocate the structure array
      ALLOCATE( Atm%Cloud( n_Clouds ), STAT = alloc_stat )
      IF ( alloc_stat /= 0 ) THEN
        CALL CRTM_Atmosphere_Destroy( Atm )
        RETURN
      END IF
      ! Allocate the individual structures
      CALL CRTM_Cloud_Create( Atm%Cloud, n_Layers )
    END IF
    ! ...Aerosol array
    IF ( n_Aerosols > 0 ) THEN
      ! Allocate the structure array
      ALLOCATE( Atm%Aerosol( n_Aerosols ), STAT = alloc_stat )
      IF ( alloc_stat /= 0 ) THEN
        CALL CRTM_Atmosphere_Destroy( Atm )
        RETURN
      END IF
      ! Allocate the individual structures
      CALL CRTM_Aerosol_Create( Atm%Aerosol, n_Layers )
    END IF

    ! Initialise
    ! ...Dimensions
    Atm%Max_Layers   = n_Layers
    Atm%n_Layers     = n_Layers
    Atm%n_Absorbers  = n_Absorbers
    Atm%Max_Clouds   = n_Clouds
    Atm%n_Clouds     = n_Clouds
    Atm%Max_Aerosols = n_Aerosols
    Atm%n_Aerosols   = n_Aerosols
    Atm%n_Added_Layers = 0
    ! ...Arrays
    Atm%Absorber_ID    = INVALID_ABSORBER_ID
    Atm%Absorber_Units = INVALID_ABSORBER_UNITS
    Atm%Level_Pressure = ZERO
    Atm%Pressure       = ZERO
    Atm%Temperature    = ZERO
    Atm%Absorber       = ZERO

    ! Set allocation indicator
    Atm%Is_Allocated = .TRUE.

  END SUBROUTINE CRTM_Atmosphere_Create



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_AddLayerCopy
!
! PURPOSE:
!       Elemental function to copy an instance of the CRTM Atmosphere object
!       with additional layers added to the TOA of the input.
!
! CALLING SEQUENCE:
!       Atm_out = CRTM_Atmosphere_AddLayerCopy( Atm, n_Added_Layers )
!
! OBJECTS:
!       Atm:             Atmosphere structure to copy.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar or any rank
!                        ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Added_Layers:  Number of layers to add to the function result.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Same as atmosphere object
!                        ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Atm_out:         Copy of the input atmosphere structure with space for
!                        extra layers added to TOA.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Same as input.
!                        ATTRIBUTES: INTENT(OUT)
!
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Atmosphere_AddLayerCopy( &
    atm, &
    n_Added_Layers ) &
  RESULT( atm_out )
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: atm
    INTEGER,                    INTENT(IN) :: n_Added_Layers
    ! Function result
    TYPE(CRTM_Atmosphere_type) :: atm_out
    ! Local variables
    INTEGER :: i, na, no, nt

    ! Set the number of extra layers
    na = MAX(n_Added_Layers,0)

    ! Create the output structure
    CALL CRTM_Atmosphere_Create( atm_out, &
                                 atm%n_Layers+na, &
                                 atm%n_Absorbers, &
                                 atm%n_Clouds   , &
                                 atm%n_Aerosols   )
    IF ( .NOT. CRTM_Atmosphere_Associated(atm_out) ) RETURN

    ! Assign data
    atm_out%n_Added_Layers = atm%n_Added_Layers+na
    ! ...Layer independent data
    atm_out%Climatology    = atm%Climatology
    atm_out%Absorber_ID    = atm%Absorber_ID
    atm_out%Absorber_Units = atm%Absorber_Units
    ! ...Layer dependent data
    no = atm%n_Layers
    nt = atm_out%n_Layers
    atm_out%Level_Pressure(na:nt) = atm%Level_Pressure(0:no)
    atm_out%Pressure(na+1:nt)     = atm%Pressure(1:no)
    atm_out%Temperature(na+1:nt)  = atm%Temperature(1:no)
    atm_out%Absorber(na+1:nt,:)   = atm%Absorber(1:no,:)
    ! ...Cloud components
    IF ( atm%n_Clouds > 0 ) THEN
      DO i = 1, atm%n_Clouds
        atm_out%Cloud(i) = CRTM_Cloud_AddLayerCopy( atm%Cloud(i), atm_out%n_Added_Layers )
      END DO
    END IF
    ! ...Aerosol components
    IF ( atm%n_Aerosols > 0 ) THEN
      DO i = 1, atm%n_Aerosols
        atm_out%Aerosol(i) = CRTM_Aerosol_AddLayerCopy( atm%Aerosol(i), atm_out%n_Added_Layers )
      END DO
    END IF

  END FUNCTION CRTM_Atmosphere_AddLayerCopy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_Zero
!
! PURPOSE:
!       Elemental subroutine to zero out the data arrays
!       in a CRTM Atmosphere object.
!
! CALLING SEQUENCE:
!       CALL CRTM_Atmosphere_Zero( Atm )
!
! OUTPUTS:
!       Atm:          CRTM Atmosphere structure in which the data arrays
!                     are to be zeroed out.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       - The dimension components of the structure are *NOT* set to zero.
!       - The Climatology, Absorber_ID, and Absorber_Units components are
!         *NOT* reset in this routine.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Atmosphere_Zero( Atmosphere )
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atmosphere

    ! Do nothing if structure is unused
    IF ( .NOT. CRTM_Atmosphere_Associated(Atmosphere) ) RETURN

    ! Reset the added layer count
    Atmosphere%n_Added_Layers = 0

    ! Only zero out the data arrays
    Atmosphere%Level_Pressure    = ZERO
    Atmosphere%Pressure          = ZERO
    Atmosphere%Temperature       = ZERO
    Atmosphere%Absorber          = ZERO

    ! Reset the structure components
    IF ( Atmosphere%n_Clouds   > 0 ) CALL CRTM_Cloud_Zero( Atmosphere%Cloud )
    IF ( Atmosphere%n_Aerosols > 0 ) CALL CRTM_Aerosol_Zero( Atmosphere%Aerosol )

  END SUBROUTINE CRTM_Atmosphere_Zero


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_IsValid
!
! PURPOSE:
!       Non-pure function to perform some simple validity checks on a
!       CRTM Atmosphere object.
!
!       If invalid data is found, a message is printed to stdout.
!
! CALLING SEQUENCE:
!       result = CRTM_Atmosphere_IsValid( Atm )
!
!         or
!
!       IF ( CRTM_Atmosphere_IsValid( Atm ) ) THEN....
!
! OBJECTS:
!       Atm:       CRTM Atmosphere object which is to have its
!                  contents checked.
!                  UNITS:      N/A
!                  TYPE:       CRTM_Atmosphere_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:    Logical variable indicating whether or not the input
!                  passed the check.
!                  If == .FALSE., Atmosphere object is unused or contains
!                                 invalid data.
!                     == .TRUE.,  Atmosphere object can be used in CRTM.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Atmosphere_IsValid( Atm ) RESULT( IsValid )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atm
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_IsValid'
    CHARACTER(ML) :: msg
    INTEGER :: nc, na

    ! Setup
    IsValid = .FALSE.
    ! ...Check if structure is used
    IF ( .NOT. CRTM_Atmosphere_Associated(Atm) ) THEN
      msg = 'Atmosphere structure not allocated'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    ENDIF
    IF ( Atm%n_Layers < 1 .OR. Atm%n_Absorbers < 1 ) THEN
      msg = 'Atmosphere structure dimensions invalid'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    ENDIF
    IF ( Atm%n_Layers > MAX_N_LAYERS ) THEN
      WRITE(msg,'("No. of atmosphere structure layers [",i0,"(added:",i0,&
            &")] is larger than maximum allowed [",i0,"]")') &
            Atm%n_Layers, Atm%n_Added_Layers, MAX_N_LAYERS
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    ENDIF

    ! Check data
    ! ...Change default so all entries can be checked
    IsValid = .TRUE.
    ! ...The type of Atmosphere
    IF ( Atm%Climatology < 1 .OR. Atm%Climatology > N_VALID_CLIMATOLOGY_MODELS ) THEN
      msg = 'Invalid climatology'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      IsValid = .FALSE.
    ENDIF
    ! ...The absorber id range
    IF ( ANY(Atm%Absorber_ID < 1) .OR. ANY(Atm%Absorber_ID > N_VALID_ABSORBER_IDS) ) THEN
      msg = 'Invalid absorber ID'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      IsValid = .FALSE.
    ENDIF
    ! ...H2O *must* be specfied
    IF ( .NOT. Absorber_Id_IsPresent(H2O_ID) ) THEN
      msg = TRIM(ABSORBER_ID_NAME(H2O_ID))//' absorber profile must be specified'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      IsValid = .FALSE.
    ENDIF
    ! ...O3 *must* be specfied
    IF ( .NOT. Absorber_Id_IsPresent(O3_ID) ) THEN
      msg = TRIM(ABSORBER_ID_NAME(O3_ID))//' absorber profile must be specified'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      IsValid = .FALSE.
    ENDIF
    ! ...The absorber units range
    IF ( ANY(Atm%Absorber_Units < 1) .OR. ANY(Atm%Absorber_Units > N_VALID_ABSORBER_UNITS) ) THEN
      msg = 'Invalid absorber units ID'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      IsValid = .FALSE.
    ENDIF
    ! ...Data limits. Only checking negative values
    IF ( ANY(Atm%Level_Pressure < ZERO ) ) THEN
      msg = 'Negative level pressure found'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( ANY(Atm%Pressure < ZERO ) ) THEN
      msg = 'Negative layer pressure found'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( ANY(Atm%Temperature < ZERO ) ) THEN
      msg = 'Negative layer temperature found'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( ANY(Atm%Absorber < ZERO ) ) THEN
      msg = 'Negative level absorber found'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      IsValid = .FALSE.
    ENDIF
    ! ...Structure components
    IF ( Atm%n_Clouds > 0 ) THEN
      DO nc = 1, Atm%n_Clouds
        IsValid = IsValid .AND. CRTM_Cloud_IsValid( Atm%Cloud(nc) )
      END DO
    END IF
    IF ( Atm%n_Aerosols > 0 ) THEN
      DO na = 1, Atm%n_Aerosols
        IsValid = IsValid .AND. CRTM_Aerosol_IsValid( Atm%Aerosol(na) )
      END DO
    END IF

  CONTAINS

    FUNCTION Absorber_Id_IsPresent( Id ) RESULT( IsPresent )
      INTEGER, INTENT(IN) :: Id
      LOGICAL :: IsPresent
      IsPresent = ANY(Atm%Absorber_ID == Id)
    END FUNCTION Absorber_Id_IsPresent

  END FUNCTION CRTM_Atmosphere_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CRTM Atmosphere object to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_Atmosphere_Inspect( Atm )
!
! INPUTS:
!       Atm:  CRTM Atmosphere object to display.
!             UNITS:      N/A
!             TYPE:       CRTM_Atmosphere_type
!             DIMENSION:  Scalar, Rank-1, or Rank-2
!             ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Scalar_Inspect( Atm )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atm
    INTEGER :: lClimatology
    INTEGER :: j, k
    WRITE(*, '(1x,"ATMOSPHERE OBJECT")')
    ! Dimensions
    WRITE(*, '(3x,"n_Layers    :",1x,i0)') Atm%n_Layers
    WRITE(*, '(3x,"n_Absorbers :",1x,i0)') Atm%n_Absorbers
    WRITE(*, '(3x,"n_Clouds    :",1x,i0)') Atm%n_Clouds
    WRITE(*, '(3x,"n_Aerosols  :",1x,i0)') Atm%n_Aerosols
    ! Climatology
    lClimatology = Atm%Climatology
    IF ( lClimatology < 1 .OR. &
         lClimatology > N_VALID_CLIMATOLOGY_MODELS ) lClimatology = INVALID_MODEL
    WRITE(*, '(3x,"Climatology :",1x,a)') CLIMATOLOGY_MODEL_NAME(lClimatology)
    IF ( .NOT. CRTM_Atmosphere_Associated(Atm) ) RETURN
    ! Profile information
    k = Atm%n_Layers
    WRITE(*, '(3x,"Level pressure:")')
    WRITE(*, '(5(1x,es13.6,:))') Atm%Level_Pressure(0:k)
    WRITE(*, '(3x,"Layer pressure:")')
    WRITE(*, '(5(1x,es13.6,:))') Atm%Pressure(1:k)
    WRITE(*, '(3x,"Layer temperature:")')
    WRITE(*, '(5(1x,es13.6,:))') Atm%Temperature(1:k)
    WRITE(*, '(3x,"Layer absorber:")')
    DO j = 1, Atm%n_Absorbers
      WRITE(*, '(5x,a,"(",a,")")') TRIM(ABSORBER_ID_NAME(Atm%Absorber_Id(j))), &
                                   TRIM(ABSORBER_UNITS_NAME(Atm%Absorber_Units(j)))
      WRITE(*, '(5(1x,es13.6,:))') Atm%Absorber(1:k,j)
    END DO
    ! Cloud information
    IF ( Atm%n_Clouds > 0 ) CALL CRTM_Cloud_Inspect(Atm%Cloud)
    ! Aerosol information
    IF ( Atm%n_Aerosols > 0 ) CALL CRTM_Aerosol_Inspect(Atm%Aerosol)
  END SUBROUTINE Scalar_Inspect

  SUBROUTINE Rank1_Inspect( Atmosphere )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atmosphere(:)
    INTEGER :: i
    DO i = 1, SIZE(Atmosphere)
      WRITE(*, FMT='(1x,"RANK-1 INDEX:",i0," - ")', ADVANCE='NO') i
      CALL Scalar_Inspect(Atmosphere(i))
    END DO
  END SUBROUTINE Rank1_Inspect

  SUBROUTINE Rank2_Inspect( Atmosphere )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atmosphere(:,:)
    INTEGER :: i, j
    DO j = 1, SIZE(Atmosphere,2)
      DO i = 1, SIZE(Atmosphere,1)
        WRITE(*, FMT='(1x,"RANK-2 INDEX:",i0,",",i0," - ")', ADVANCE='NO') i,j
        CALL Scalar_Inspect(Atmosphere(i,j))
      END DO
    END DO
  END SUBROUTINE Rank2_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_MaxInspect
!
! PURPOSE:
!       Subroutine to print the maximum value of the various components of
!       a CRTM Atmosphere object to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_Atmosphere_MaxInspect( Atm )
!
! INPUTS:
!       Atm:  CRTM Atmosphere object to query and display.
!             UNITS:      N/A
!             TYPE:       CRTM_Atmosphere_type
!             DIMENSION:  Scalar, Rank-1, or Rank-2
!             ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Scalar_MaxInspect( Atm )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atm
    INTEGER :: lClimatology
    INTEGER :: j, k

    WRITE(*, '(1x,"ATMOSPHERE OBJECT")')
    ! Dimensions
    WRITE(*,'(3x,"n_Layers    :",1x,i0)') Atm%n_Layers
    WRITE(*,'(3x,"n_Absorbers :",1x,i0)') Atm%n_Absorbers
    WRITE(*,'(3x,"n_Clouds    :",1x,i0)') Atm%n_Clouds
    WRITE(*,'(3x,"n_Aerosols  :",1x,i0)') Atm%n_Aerosols
    ! Climatology
    lClimatology = Atm%Climatology
    IF ( lClimatology < 1 .OR. &
         lClimatology > N_VALID_CLIMATOLOGY_MODELS ) lClimatology = INVALID_MODEL
    WRITE(*,'(3x,"Climatology :",1x,a)') CLIMATOLOGY_MODEL_NAME(lClimatology)
    IF ( .NOT. CRTM_Atmosphere_Associated(Atm) ) RETURN
    ! Profile information
    k = Atm%n_Layers
    WRITE(*,'(3x,"Max. level pressure: ",es13.6)') MaxVal_Find(Atm%Level_Pressure(0:k))
    WRITE(*,'(3x,"Max. layer pressure: ",es13.6)') MaxVal_Find(Atm%Pressure(1:k))
    WRITE(*,'(3x,"Max. layer temperature: ",es13.6)') MaxVal_Find(Atm%Temperature(1:k))
    WRITE(*,'(3x,"Max. layer absorber:")')
    DO j = 1, Atm%n_Absorbers
      WRITE(*,'(5x,a,"(",a,") : ",es13.6)') &
        TRIM(ABSORBER_ID_NAME(Atm%Absorber_Id(j))), &
        TRIM(ABSORBER_UNITS_NAME(Atm%Absorber_Units(j))), &
        MaxVal_Find(Atm%Absorber(1:k,j))
    END DO
    ! Cloud information
    IF ( Atm%n_Clouds > 0 ) CALL CRTM_Cloud_MaxInspect(Atm%Cloud)
    ! Aerosol information
    IF ( Atm%n_Aerosols > 0 ) CALL CRTM_Aerosol_MaxInspect(Atm%Aerosol)
  END SUBROUTINE Scalar_MaxInspect

  SUBROUTINE Rank1_MaxInspect( Atmosphere )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atmosphere(:)
    INTEGER :: i
    DO i = 1, SIZE(Atmosphere)
      WRITE(*, FMT='(1x,"RANK-1 INDEX:",i0," - ")', ADVANCE='NO') i
      CALL Scalar_MaxInspect(Atmosphere(i))
    END DO
  END SUBROUTINE Rank1_MaxInspect

  SUBROUTINE Rank2_MaxInspect( Atmosphere )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atmosphere(:,:)
    INTEGER :: i, j
    DO j = 1, SIZE(Atmosphere,2)
      DO i = 1, SIZE(Atmosphere,1)
        WRITE(*, FMT='(1x,"RANK-2 INDEX:",i0,",",i0," - ")', ADVANCE='NO') i,j
        CALL Scalar_MaxInspect(Atmosphere(i,j))
      END DO
    END DO
  END SUBROUTINE Rank2_MaxInspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_CompareInspect
!
! PURPOSE:
!       Subroutine to compare two Atmosphere objects and print the MAXIMUM differences
!       to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_Atmosphere_Inspect( Atm1, Atm2, n_SigFig=n_SigFig )
!
! INPUTS:
!       Atme1, Atm2:     The CRTM Atmosphere objects to compare and display.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar, Rank-1, or Rank-2 array
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       n_SigFig:        Number of significant figure to compare floating point
!                        components.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Scalar_CompareInspect( x, y, n_SigFig )
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: x, y
    INTEGER,          OPTIONAL, INTENT(IN) :: n_SigFig
    ! Local variables
    INTEGER :: xlClimatology, ylClimatology
    INTEGER :: j, n
    REAL(fp) :: rxval, ryval, rdiff

    IF ( CRTM_Atmosphere_Compare(x, y, n_SigFig=n_SigFig) ) RETURN
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF


    WRITE(*, '(1x,"Atmosphere OBJECT CompareInspect")')
    WRITE(*, '(3x,"n_Layers    :",1x,i0,1x,i0)') x%n_Layers   , y%n_Layers
    WRITE(*, '(3x,"n_Absorbers :",1x,i0,1x,i0)') x%n_Absorbers, y%n_Absorbers
    WRITE(*, '(3x,"n_Clouds    :",1x,i0,1x,i0)') x%n_Clouds   , y%n_Clouds
    WRITE(*, '(3x,"n_Aerosols  :",1x,i0,1x,i0)') x%n_Aerosols , y%n_Aerosols
    xlClimatology = x%Climatology
    IF ( xlClimatology < 1 .OR. &
         xlClimatology > N_VALID_CLIMATOLOGY_MODELS ) xlClimatology = INVALID_MODEL
    ylClimatology = y%Climatology
    IF ( ylClimatology < 1 .OR. &
         ylClimatology > N_VALID_CLIMATOLOGY_MODELS ) ylClimatology = INVALID_MODEL
    WRITE(*, '(3x,"Climatology :",1x,a,", ",a)') &
             TRIM(CLIMATOLOGY_MODEL_NAME(xlClimatology)), &
             TRIM(CLIMATOLOGY_MODEL_NAME(ylClimatology))


    ! Check integer arrays
    j = x%n_Absorbers
    IF ( ANY(x%Absorber_Id(1:j) /= y%Absorber_Id(1:j)) ) THEN
      WRITE(*,'(3x,"Absorber_Id difference:")')
      WRITE(*,'(5x,"atm1 : ",99(i0,:,1x))') x%Absorber_Id(1:j)
      WRITE(*,'(5x,"atm2 : ",99(i0,:,1x))') y%Absorber_Id(1:j)
    END IF
    IF ( ANY(x%Absorber_Units(1:j) /= y%Absorber_Units(1:j)) ) THEN
      WRITE(*,'(3x,"Absorber_Units difference:")')
      WRITE(*,'(5x,"atm1 : ",99(i0,:,1x))') x%Absorber_Units(1:j)
      WRITE(*,'(5x,"atm2 : ",99(i0,:,1x))') y%Absorber_Units(1:j)
    END IF


    ! Check real arrays
    IF ( (.NOT. ALL(Compares_Within_Tolerance(x%Level_Pressure, y%Level_Pressure, n))) ) THEN
      CALL MaxDiff_Find(x%Level_Pressure, y%Level_Pressure, rxval, ryval, rdiff)
      WRITE(*,'(3x,"Level_Pressure difference:")')
      WRITE(*,'(5x,"atm1 : ",es22.15)') rxval
      WRITE(*,'(5x,"atm2 : ",es22.15)') ryval
      WRITE(*,'(5x,"diff : ",es22.15)') rdiff
    END IF
    IF ( (.NOT. ALL(Compares_Within_Tolerance(x%Pressure, y%Pressure, n))) ) THEN
      CALL MaxDiff_Find(x%Pressure, y%Pressure, rxval, ryval, rdiff)
      WRITE(*,'(3x,"Pressure difference:")')
      WRITE(*,'(5x,"atm1 : ",es22.15)') rxval
      WRITE(*,'(5x,"atm2 : ",es22.15)') ryval
      WRITE(*,'(5x,"diff : ",es22.15)') rdiff
    END IF
    IF ( (.NOT. ALL(Compares_Within_Tolerance(x%Temperature, y%Temperature, n))) ) THEN
      CALL MaxDiff_Find(x%Temperature, y%Temperature, rxval, ryval, rdiff)
      WRITE(*,'(3x,"Temperature difference:")')
      WRITE(*,'(5x,"atm1 : ",es22.15)') rxval
      WRITE(*,'(5x,"atm2 : ",es22.15)') ryval
      WRITE(*,'(5x,"diff : ",es22.15)') rdiff
    END IF
    DO j = 1, x%n_Absorbers
      IF ( (.NOT. ALL(Compares_Within_Tolerance(x%Absorber(:,j), y%Absorber(:,j), n))) ) THEN
        CALL MaxDiff_Find(x%Absorber(:,j), y%Absorber(:,j), rxval, ryval, rdiff)
        WRITE(*,'(3x,a," Absorber difference:")') TRIM(ABSORBER_ID_NAME(x%Absorber_Id(j)))
        WRITE(*,'(5x,"atm1 : ",es22.15)') rxval
        WRITE(*,'(5x,"atm2 : ",es22.15)') ryval
        WRITE(*,'(5x,"diff : ",es22.15)') rdiff
      END IF
    END DO

    ! Check structure arrays
    IF ( x%n_Clouds > 0 ) &
      CALL CRTM_Cloud_CompareInspect(x%Cloud, y%Cloud, n_SigFig=n_SigFig)
    IF ( x%n_Aerosols > 0 ) &
      CALL CRTM_Aerosol_CompareInspect(x%Aerosol, y%Aerosol, n_SigFig=n_SigFig)

  END SUBROUTINE Scalar_CompareInspect


  SUBROUTINE Rank1_CompareInspect( x, y, n_SigFig )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: x(:), y(:)
    INTEGER,          OPTIONAL, INTENT(IN) :: n_SigFig
    INTEGER :: i
    IF ( ALL(CRTM_Atmosphere_Compare(x, y, n_SigFig=n_SigFig)) ) RETURN
    WRITE(*, '(1x,"Atmosphere RANK-1 ARRAY CompareInspect")')
    DO i = 1, SIZE(x)
      WRITE(*, FMT='(1x,"RANK-1. Profile:",i0)') i
      CALL Scalar_CompareInspect(x(i),y(i),n_SigFig=n_SigFig)
    END DO
  END SUBROUTINE Rank1_CompareInspect


  SUBROUTINE Rank2_CompareInspect( x, y, n_SigFig )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: x(:,:), y(:,:)
    INTEGER,          OPTIONAL, INTENT(IN) :: n_SigFig
    INTEGER :: i,j
    IF ( ALL(CRTM_Atmosphere_Compare(x, y, n_SigFig=n_SigFig)) ) RETURN
    WRITE(*, '(1x,"Atmosphere RANK-2 ARRAY CompareInspect")')
    DO j = 1, SIZE(x,DIM=2)
      DO i = 1, SIZE(x,DIM=1)
        WRITE(*, FMT='(1x,"RANK-2. Channel:",i0,", Profile:",i0)') i,j
        CALL Scalar_CompareInspect(x(i,j),y(i,j),n_SigFig=n_SigFig)
      END DO
    END DO
  END SUBROUTINE Rank2_CompareInspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Atmosphere_DefineVersion( Id )
!
! OUTPUTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Atmosphere_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_Atmosphere_DefineVersion


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       CRTM_Atmosphere_Compare
!
! PURPOSE:
!       Elemental function to compare two CRTM_Atmosphere objects to within
!       a user specified number of significant figures.
!
! CALLING SEQUENCE:
!       is_comparable = CRTM_Atmosphere_Compare( x, y, n_SigFig=n_SigFig )
!
! OBJECTS:
!       x, y:          Two CRTM Atmosphere objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Atmosphere_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       n_SigFig:      Number of significant figure to compare floating point
!                      components.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar or same as input
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Atmosphere_Compare( &
    x, &
    y, &
    n_SigFig ) &
  RESULT( is_comparable )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: x, y
    INTEGER,          OPTIONAL, INTENT(IN) :: n_SigFig
    LOGICAL :: is_comparable
    ! Variables
    INTEGER :: j
    INTEGER :: n

    ! Set up
    is_comparable = .FALSE.
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF

    ! Check the structure association status
    IF ( (.NOT. CRTM_Atmosphere_Associated(x)) .OR. &
         (.NOT. CRTM_Atmosphere_Associated(y))      ) RETURN

    ! Check scalars
    IF ( (x%n_Layers    /= y%n_Layers   ) .OR. &
         (x%n_Absorbers /= y%n_Absorbers) .OR. &
         (x%n_Clouds    /= y%n_Clouds   ) .OR. &
         (x%n_Aerosols  /= y%n_Aerosols ) .OR. &
         (x%Climatology /= y%Climatology) ) RETURN

    ! Check integer arrays
    j = x%n_Absorbers
    IF ( ANY(x%Absorber_ID(1:j)    /= y%Absorber_ID(1:j)   ) .OR. &
         ANY(x%Absorber_Units(1:j) /= y%Absorber_Units(1:j)) ) RETURN

    ! Check floating point arrays
    IF ( (.NOT. ALL(Compares_Within_Tolerance(x%Level_Pressure,y%Level_Pressure,n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Pressure      ,y%Pressure      ,n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Temperature   ,y%Temperature   ,n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Absorber      ,y%Absorber      ,n))) ) RETURN

    ! Check clouds
    IF ( x%n_Clouds > 0 ) THEN
      IF ( .NOT. ALL(CRTM_Cloud_Compare(x%Cloud,y%Cloud,n_SigFig=n)) ) RETURN
    END IF

    ! Check aerosols
    IF ( x%n_Aerosols > 0 ) THEN
      IF ( .NOT. ALL(CRTM_Aerosol_Compare(x%Aerosol,y%Aerosol,n_SigFig=n)) ) RETURN
    END IF

    ! If we get here, the structures are comparable
    is_comparable = .TRUE.

  END FUNCTION CRTM_Atmosphere_Compare


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Get_AbsorberIdx
!
! PURPOSE:
!       Function to determine the index of the requested absorber in the
!       CRTM Atmosphere structure absorber component.
!
! CALLING SEQUENCE:
!       Idx = CRTM_Get_AbsorberIdx(Atm, AbsorberId)
!
! INPUTS:
!       Atm:          CRTM Atmosphere structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       AbsorberId:   Integer value used to identify absorbing molecular
!                     species. The accepted absorber Ids are defined in
!                     this module.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Idx:          Index of the requested absorber in the
!                     Atm%Absorber array component.
!                     If the requested absorber cannot be found,
!                     a value of -1 is returned.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Get_AbsorberIdx(Atm, AbsorberId) RESULT(AbsorberIdx)
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atm
    INTEGER                   , INTENT(IN) :: AbsorberId
    ! Function result
    INTEGER :: AbsorberIdx
    ! Local variables
    INTEGER :: j, Idx(1)

    ! Initialise result to "not found"
    AbsorberIdx = -1
    ! Return if absorber not present
    IF ( COUNT(Atm%Absorber_ID == AbsorberId) /= 1 ) RETURN
    ! Find the location
    Idx = PACK((/(j,j=1,Atm%n_Absorbers)/), Atm%Absorber_ID==AbsorberId)
    AbsorberIdx=Idx(1)

  END FUNCTION CRTM_Get_AbsorberIdx


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Get_PressureLevelIdx
!
! PURPOSE:
!       Function to determine the index in the CRTM Atmosphere structure
!       pressure level array component that corresponds to the value
!       closest to the requested level pressure.
!
! CALLING SEQUENCE:
!       Idx = CRTM_Get_PressureLevelIdx(Atm, Level_Pressure)
!
! INPUTS:
!       Atm:             CRTM Atmosphere structure.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Level_Pressure:  Level pressure for which the index in the atmosphere
!                        structure level pressure profile is required.
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Idx:             Index of the level in the Atm%Level_Pressure
!                        array component for the closest value to the
!                        input level pressure.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Get_PressureLevelIdx(Atm, Level_Pressure) RESULT(Level_Idx)
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atm
    REAL(fp)                  , INTENT(IN) :: Level_Pressure
    ! Function result
    INTEGER :: Level_Idx

    ! Find the closest pressure level
    ! Note: The "- 1" takes into account the array starting at index 0.
    Level_Idx = MINLOC(ABS(Atm%Level_Pressure - Level_Pressure), DIM=1) - 1

  END FUNCTION CRTM_Get_PressureLevelIdx


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_SetLayers
!
! PURPOSE:
!       Elemental subroutine to set the working number of layers to use
!       in a CRTM Atmosphere object.
!
! CALLING SEQUENCE:
!      CALL CRTM_Atmosphere_SetLayers( Atmosphere, n_Layers )
!
! OBJECT:
!       Atmosphere:   CRTM Atmosphere object which is to have its working number
!                     of layers updated.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       n_Layers:     The value to set the n_Layers component of the
!                     Atmosphere object.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Conformable with the Atmosphere object argument
!                     ATTRIBUTES: INTENT(IN)
!
! COMMENTS:
!       - The object is zeroed upon output.
!
!       - If n_Layers <= Atmosphere%Max_Layers, then only the n_Layers dimension
!         value of the object, as well as any contained objects, is changed.
!
!       - If n_Layers > Atmosphere%Max_Layers, then the object is reallocated
!         to the required number of layers. No other dimensions of the object
!         or contained objects are altered.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Atmosphere_SetLayers( Atmosphere, n_Layers )
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atmosphere
    INTEGER,                    INTENT(IN)     :: n_Layers
    ! Local variables
    INTEGER :: n_absorbers
    INTEGER :: max_clouds, n_clouds
    INTEGER :: max_aerosols, n_aerosols

    IF ( n_Layers < Atmosphere%Max_Layers ) THEN
      ! Just update the layer counts
      Atmosphere%n_Layers = n_Layers
      CALL CRTM_Cloud_SetLayers(Atmosphere%Cloud, n_Layers)
      CALL CRTM_Aerosol_SetLayers(Atmosphere%Aerosol, n_Layers)
      CALL CRTM_Atmosphere_Zero(Atmosphere)
    ELSE
      ! Reallocation is necessary
      ! ...Save other dimensions
      n_absorbers  = Atmosphere%n_Absorbers
      max_clouds   = MAX(Atmosphere%n_Clouds, Atmosphere%Max_Clouds)
      n_clouds     = MIN(Atmosphere%n_Clouds, Atmosphere%Max_Clouds)
      max_aerosols = MAX(Atmosphere%n_Aerosols, Atmosphere%Max_Aerosols)
      n_aerosols   = MIN(Atmosphere%n_Aerosols, Atmosphere%Max_Aerosols)
      ! ...Reallocate
      CALL CRTM_Atmosphere_Create( Atmosphere, n_Layers, n_Absorbers, Max_Clouds, Max_Aerosols )
      IF ( .NOT. CRTM_Atmosphere_Associated(Atmosphere) ) RETURN
      ! ...Restore cloud and aerosol use dimensions
      Atmosphere%n_Clouds   = n_clouds
      Atmosphere%n_Aerosols = n_aerosols
    END IF
  END SUBROUTINE CRTM_Atmosphere_SetLayers


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_InquireFile
!
! PURPOSE:
!       Function to inquire CRTM Atmosphere object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Atmosphere_InquireFile( Filename               , &
!                                                   n_Channels = n_Channels, &
!                                                   n_Profiles = n_Profiles  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       CRTM Atmosphere data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Channels:     The number of spectral channels for which there is
!                       data in the file. Note that this value will always
!                       be 0 for a profile-only dataset-- it only has meaning
!                       for K-matrix data.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       n_Profiles:     The number of profiles in the data file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file inquire was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Atmosphere_InquireFile( &
    Filename   , &  ! Input
    n_Channels , &  ! Optional output
    n_Profiles ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: l, m

    ! Set up
    err_stat = SUCCESS
    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Read the number of channels,profiles
    READ( fid, IOSTAT=io_stat,IOMSG=io_msg ) l, m
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensions from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the optional return arguments
    IF ( PRESENT(n_Channels) ) n_Channels = l
    IF ( PRESENT(n_Profiles) ) n_Profiles = m

  CONTAINS

    SUBROUTINE Inquire_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= SUCCESS ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION CRTM_Atmosphere_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_ReadFile
!
! PURPOSE:
!       Function to read CRTM Atmosphere object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Atmosphere_ReadFile( Filename                , &
!                                                Atmosphere              , &
!                                                Quiet      = Quiet      , &
!                                                n_Channels = n_Channels , &
!                                                n_Profiles = n_Profiles , &
!
! INPUTS:
!       Filename:     Character string specifying the name of an
!                     Atmosphere format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Atmosphere:   CRTM Atmosphere object array containing the Atmosphere
!                     data. Note the following meanings attributed to the
!                     dimensions of the object array:
!                     Rank-1: M profiles.
!                             Only profile data are to be read in. The file
!                             does not contain channel information. The
!                             dimension of the structure is understood to
!                             be the PROFILE dimension.
!                     Rank-2: L channels  x  M profiles
!                             Channel and profile data are to be read in.
!                             The file contains both channel and profile
!                             information. The first dimension of the
!                             structure is the CHANNEL dimension, the second
!                             is the PROFILE dimension. This is to allow
!                             K-matrix structures to be read in with the
!                             same function.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Rank-1 (M) or Rank-2 (L x M)
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Quiet:        Set this logical argument to suppress INFORMATION
!                     messages being printed to stdout
!                     If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                        == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                     If not specified, default is .FALSE.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_Channels:   The number of channels for which data was read. Note that
!                     this value will always be 0 for a profile-only dataset--
!                     it only has meaning for K-matrix data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       n_Profiles:   The number of profiles for which data was read.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the file read was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Read_Atmosphere_Rank1( &
    Filename   , &  ! Input
    Atmosphere , &  ! Output
    Quiet      , &  ! Optional input
    n_Channels , &  ! Optional output
    n_Profiles , &  ! Optional output
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: Filename
    TYPE(CRTM_Atmosphere_type), INTENT(OUT) :: Atmosphere(:)  ! M
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER,          OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER,          OPTIONAL, INTENT(OUT) :: n_Profiles
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_ReadFile(M)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    INTEGER :: fid
    INTEGER :: n_file_channels, n_file_profiles
    INTEGER :: m, n_input_profiles


    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) n_file_channels, n_file_profiles
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensions from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check that n_Channels is zero
    IF ( n_file_channels /= 0 ) THEN
      msg = 'n_Channels dimensions in '//TRIM(Filename)//' is not zero for a rank-1 '//&
            '(i.e. profiles only) Atmosphere read.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if n_Profiles in file is > size of output array
    n_input_profiles = SIZE(Atmosphere)
    IF ( n_file_profiles > n_input_profiles ) THEN
      WRITE( msg,'("Number of profiles, ",i0," > size of the output Atmosphere", &
                  &" array, ",i0,". Only the first ",i0, &
                  &" profiles will be read.")' ) &
                  n_file_profiles, n_input_profiles, n_input_profiles
      CALL Display_Message( ROUTINE_NAME, msg, WARNING )
    END IF
    n_input_profiles = MIN(n_input_profiles, n_file_profiles)


    ! Loop over all the profiles
    Profile_Loop: DO m = 1, n_input_profiles
      err_stat = Read_Record( fid, Atmosphere(m), &
                              Quiet = Quiet, &
                              Debug = Debug  )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading Atmosphere element (",i0,") from ",a)' ) m, TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF
    END DO Profile_Loop


    ! Close the file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Set the optional return values
    IF ( PRESENT(n_Channels) ) n_Channels = 0
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_input_profiles


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of profiles read from ",a,": ",i0)' ) TRIM(Filename), n_input_profiles
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL CRTM_Atmosphere_Destroy( Atmosphere )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_Atmosphere_Rank1


  FUNCTION Read_Atmosphere_Rank2( &
    Filename   , &  ! Input
    Atmosphere , &  ! Output
    Quiet      , &  ! Optional input
    n_Channels , &  ! Optional output
    n_Profiles , &  ! Optional output
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: Filename
    TYPE(CRTM_Atmosphere_type), INTENT(OUT) :: Atmosphere(:,:)  ! L x M
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER,          OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER,          OPTIONAL, INTENT(OUT) :: n_Profiles
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_ReadFile(L x M)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    INTEGER :: fid
    INTEGER :: l, n_file_channels, n_input_channels
    INTEGER :: m, n_file_profiles, n_input_profiles


    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) n_file_channels, n_file_profiles
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading n_Clouds data dimension from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if n_Channels in file is > size of output array
    n_input_channels = SIZE(Atmosphere,DIM=1)
    IF ( n_file_channels > n_input_channels ) THEN
      WRITE( msg,'("Number of channels, ",i0," > size of the output Atmosphere", &
                  &" array dimension, ",i0,". Only the first ",i0, &
                  &" channels will be read.")' ) &
                  n_file_channels, n_input_channels, n_input_channels
      CALL Display_Message( ROUTINE_NAME, msg, WARNING )
    END IF
    n_input_channels = MIN(n_input_channels, n_file_channels)
    ! ...Check if n_Profiles in file is > size of output array
    n_input_profiles = SIZE(Atmosphere,DIM=2)
    IF ( n_file_profiles > n_input_profiles ) THEN
      WRITE( msg, '( "Number of profiles, ",i0," > size of the output Atmosphere", &
                    &" array dimension, ",i0,". Only the first ",i0, &
                    &" profiles will be read.")' ) &
                    n_file_profiles, n_input_profiles, n_input_profiles
      CALL Display_Message( ROUTINE_NAME, msg, WARNING )
    END IF
    n_input_profiles = MIN(n_input_profiles, n_file_profiles)


    ! Loop over all the profiles and channels
    Profile_Loop: DO m = 1, n_input_profiles
      Channel_Loop: DO l = 1, n_input_channels
        err_stat = Read_Record( fid, Atmosphere(l,m), &
                                Quiet = Quiet, &
                                Debug = Debug )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error reading Atmosphere element (",i0,",",i0,") from ",a)' ) l, m, TRIM(Filename)
          CALL Read_Cleanup(); RETURN
        END IF
      END DO Channel_Loop
    END DO Profile_Loop


    ! Close the file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Set the optional return values
    IF ( PRESENT(n_Channels) ) n_Channels = n_input_channels
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_input_profiles


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of channels and profiles read from ",a,": ",i0,1x,i0)' ) &
             TRIM(Filename), n_input_channels, n_input_profiles
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL CRTM_Atmosphere_Destroy( Atmosphere )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_Atmosphere_Rank2


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_WriteFile
!
! PURPOSE:
!       Function to write CRTM Atmosphere object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Atmosphere_WriteFile( Filename     , &
!                                                 Atmosphere   , &
!                                                 Quiet = Quiet  )
!
! INPUTS:
!       Filename:     Character string specifying the name of the
!                     Atmosphere format data file to write.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Atmosphere:   CRTM Atmosphere object array containing the Atmosphere
!                     data. Note the following meanings attributed to the
!                     dimensions of the Atmosphere array:
!                     Rank-1: M profiles.
!                             Only profile data are to be read in. The file
!                             does not contain channel information. The
!                             dimension of the array is understood to
!                             be the PROFILE dimension.
!                     Rank-2: L channels  x  M profiles
!                             Channel and profile data are to be read in.
!                             The file contains both channel and profile
!                             information. The first dimension of the
!                             array is the CHANNEL dimension, the second
!                             is the PROFILE dimension. This is to allow
!                             K-matrix structures to be read in with the
!                             same function.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Rank-1 (M) or Rank-2 (L x M)
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:        Set this logical argument to suppress INFORMATION
!                     messages being printed to stdout
!                     If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                        == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                     If not specified, default is .FALSE.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the file write was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs during *writing*, the output file is deleted before
!         returning to the calling routine.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Write_Atmosphere_Rank1( &
    Filename   , &  ! Input
    Atmosphere , &  ! Input
    Quiet      , &  ! Optional input
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN) :: Filename
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atmosphere(:)  ! M
    LOGICAL,          OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,          OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_WriteFile(M)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    INTEGER :: fid
    INTEGER :: m, n_output_profiles

    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug


    ! Any invalid profiles?
    IF ( ANY(Atmosphere%n_Layers    == 0 .OR. &
             Atmosphere%n_Absorbers == 0      ) ) THEN
      msg = 'Zero dimension profiles in input!'
      CALL Write_Cleanup(); RETURN
    END IF
    n_output_profiles = SIZE(Atmosphere)


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, For_Output = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) 0, n_output_profiles
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensions to '//TRIM(Filename)//'- '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the data
    Profile_Loop: DO m = 1, n_output_profiles
      err_stat = Write_Record( fid, Atmosphere(m), &
                               Quiet = Quiet, &
                               Debug = Debug )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing Atmosphere element (",i0,") to ",a)' ) m, TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END DO Profile_Loop


    ! Close the file (if error, no delete)
    CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//'- '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of profiles written to ",a,": ",i0)' ) TRIM(Filename), n_output_profiles
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deleting output file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION Write_Atmosphere_Rank1


  FUNCTION Write_Atmosphere_Rank2( &
    Filename   , &  ! Input
    Atmosphere , &  ! Input
    Quiet      , &  ! Optional input
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: Filename
    TYPE(CRTM_Atmosphere_type), INTENT(IN)  :: Atmosphere(:,:)  ! L x M
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_WriteFile(L x M)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    INTEGER :: fid
    INTEGER :: l, n_output_channels
    INTEGER :: m, n_output_profiles

    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug


    ! Any invalid profiles?
    IF ( ANY(Atmosphere%n_Layers    == 0 .OR. &
             Atmosphere%n_Absorbers == 0      ) ) THEN
      msg = 'Zero dimension profiles in input!'
      CALL Write_Cleanup(); RETURN
    END IF
    n_output_channels = SIZE(Atmosphere,DIM=1)
    n_output_profiles = SIZE(Atmosphere,DIM=2)


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, For_Output = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) n_output_channels, n_output_profiles
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensions to '//TRIM(Filename)//'- '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the data
    Profile_Loop: DO m = 1, n_output_profiles
      Channel_Loop: DO l = 1, n_output_channels
        err_stat = Write_Record( fid, Atmosphere(l,m), &
                                 Quiet = Quiet, &
                                 Debug = Debug )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error writing Atmosphere element (",i0,",",i0,") to ",a)' ) l, m, TRIM(Filename)
          CALL Write_Cleanup(); RETURN
        END IF
      END DO Channel_Loop
    END DO Profile_Loop


    ! Close the file (if error, no delete)
    CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//'- '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of channels and profiles written to ",a,": ",i0,1x,i0 )' ) &
             TRIM(Filename), n_output_channels, n_output_profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deleting output file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION Write_Atmosphere_Rank2



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Atmosphere_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CRTM_Atmosphere objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CRTM_Atmosphere_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CRTM Atmosphere objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Atmosphere_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Atmosphere_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_Atmosphere_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal
    ! Variables
    INTEGER :: j, k

    ! Set up
    is_equal = .FALSE.

    ! Check the structure association status
    IF ( (.NOT. CRTM_Atmosphere_Associated(x)) .OR. &
         (.NOT. CRTM_Atmosphere_Associated(y))      ) RETURN

    ! Check contents
    ! ...Scalars
    IF ( (x%n_Layers    /= y%n_Layers   ) .OR. &
         (x%n_Absorbers /= y%n_Absorbers) .OR. &
         (x%n_Clouds    /= y%n_Clouds   ) .OR. &
         (x%n_Aerosols  /= y%n_Aerosols ) .OR. &
         (x%Climatology /= y%Climatology) ) RETURN
    ! ...Arrays
    k = x%n_Layers
    j = x%n_Absorbers
    IF ( ALL(x%Absorber_ID(1:j)    == y%Absorber_ID(1:j)   ) .AND. &
         ALL(x%Absorber_Units(1:j) == y%Absorber_Units(1:j)) .AND. &
         ALL(x%Level_Pressure(0:) .EqualTo. y%Level_Pressure(0:)) .AND. &
         ALL(x%Pressure(1:k)      .EqualTo. y%Pressure(1:k)     ) .AND. &
         ALL(x%Temperature(1:k)   .EqualTo. y%Temperature(1:k)  ) .AND. &
         ALL(x%Absorber(1:k,1:j)  .EqualTo. y%Absorber(1:k,1:j) ) ) is_equal = .TRUE.
    ! ...Clouds
    IF ( x%n_Clouds > 0 ) THEN
      IF ( ALL(CRTM_Cloud_Associated(x%Cloud)) .AND. ALL(CRTM_Cloud_Associated(y%Cloud)) ) &
        is_equal = is_equal .AND. ALL(x%Cloud == y%Cloud)
    END IF
    ! ...Aerosols
    IF ( x%n_Aerosols > 0 ) THEN
      IF ( ALL(CRTM_Aerosol_Associated(x%Aerosol)) .AND. ALL(CRTM_Aerosol_Associated(y%Aerosol)) ) &
        is_equal = is_equal .AND. ALL(x%Aerosol == y%Aerosol)
    END IF

  END FUNCTION CRTM_Atmosphere_Equal


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Atmosphere_Add
!
! PURPOSE:
!       Pure function to add two CRTM Atmosphere objects.
!       Used in OPERATOR(+) interface block.
!
! CALLING SEQUENCE:
!       atmsum = CRTM_Atmosphere_Add( atm1, atm2 )
!
!         or
!
!       atmsum = atm1 + atm2
!
!
! INPUTS:
!       atm1, atm2: The Atmosphere objects to add.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Atmosphere_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!       atmsum:     Atmosphere structure containing the added components.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Atmosphere_type
!                   DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Atmosphere_Add( atm1, atm2 ) RESULT( atmsum )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: atm1, atm2
    TYPE(CRTM_Atmosphere_type) :: atmsum
    ! Variables
    INTEGER :: i, j, k

    ! Check input
    ! ...If input structures not used, do nothing
    IF ( .NOT. CRTM_Atmosphere_Associated( atm1 ) .OR. &
         .NOT. CRTM_Atmosphere_Associated( atm2 ) ) RETURN
    ! ...If input structure for different Atmospheres, or sizes, do nothing
    IF ( atm1%Climatology    /= atm2%Climatology    .OR. &
         atm1%n_Layers       /= atm2%n_Layers       .OR. &
         atm1%n_Absorbers    /= atm2%n_Absorbers    .OR. &
         atm1%n_Clouds       /= atm2%n_Clouds       .OR. &
         atm1%n_Aerosols     /= atm2%n_Aerosols     .OR. &
         atm1%n_Added_Layers /= atm2%n_Added_Layers ) RETURN
    ! ...Dimenions the same, check absorber info
    IF ( ANY(atm1%Absorber_ID    /= atm2%Absorber_ID   ) .OR. &
         ANY(atm1%Absorber_Units /= atm2%Absorber_Units) ) RETURN

    ! Copy the first structure
    atmsum = atm1

    ! And add its components to the second one
    k = atm1%n_Layers
    j = atm1%n_Absorbers
    atmsum%Level_Pressure(0:k) = atmsum%Level_Pressure(0:k) + atm2%Level_Pressure(0:k)
    atmsum%Pressure(1:k)       = atmsum%Pressure(1:k)       + atm2%Pressure(1:k)
    atmsum%Temperature(1:k)    = atmsum%Temperature(1:k)    + atm2%Temperature(1:k)
    atmsum%Absorber(1:k,1:j)   = atmsum%Absorber(1:k,1:j)   + atm2%Absorber(1:k,1:j)
    ! ...Cloud component
    IF ( atm1%n_Clouds > 0 ) THEN
      DO i = 1, atm1%n_Clouds
        atmsum%Cloud(i) = atmsum%Cloud(i) + atm2%Cloud(i)
      END DO
    END IF
    ! ...Aerosol component
    IF ( atm1%n_Aerosols > 0 ) THEN
      DO i = 1, atm1%n_Aerosols
        atmsum%Aerosol(i) = atmsum%Aerosol(i) + atm2%Aerosol(i)
      END DO
    END IF

  END FUNCTION CRTM_Atmosphere_Add



!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Atmosphere_Subtract
!
! PURPOSE:
!       Pure function to subtract two CRTM Atmosphere objects.
!       Used in OPERATOR(-) interface block.
!
! CALLING SEQUENCE:
!       atmdiff = CRTM_Atmosphere_Subtract( atm1, atm2 )
!
!         or
!
!       atmdiff = atm1 - atm2
!
!
! INPUTS:
!       atm1, atm2: The Atmosphere objects to subtract.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Atmosphere_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!       atmdiff:    Atmosphere structure containing the differenced components.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Atmosphere_type
!                   DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Atmosphere_Subtract( atm1, atm2 ) RESULT( atmdiff )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: atm1, atm2
    TYPE(CRTM_Atmosphere_type) :: atmdiff
    ! Variables
    INTEGER :: i, j, k

    ! Check input
    ! ...If input structures not used, do nothing
    IF ( .NOT. CRTM_Atmosphere_Associated( atm1 ) .OR. &
         .NOT. CRTM_Atmosphere_Associated( atm2 ) ) RETURN
    ! ...If input structure for different Atmospheres, or sizes, do nothing
    IF ( atm1%Climatology    /= atm2%Climatology    .OR. &
         atm1%n_Layers       /= atm2%n_Layers       .OR. &
         atm1%n_Absorbers    /= atm2%n_Absorbers    .OR. &
         atm1%n_Clouds       /= atm2%n_Clouds       .OR. &
         atm1%n_Aerosols     /= atm2%n_Aerosols     .OR. &
         atm1%n_Added_Layers /= atm2%n_Added_Layers ) RETURN
    ! ...Dimenions the same, check absorber info
    IF ( ANY(atm1%Absorber_ID    /= atm2%Absorber_ID   ) .OR. &
         ANY(atm1%Absorber_Units /= atm2%Absorber_Units) ) RETURN

    ! Copy the first structure
    atmdiff = atm1

    ! And subtract the second one's components from it
    k = atm1%n_Layers
    j = atm1%n_Absorbers
    atmdiff%Level_Pressure(0:k) = atmdiff%Level_Pressure(0:k) - atm2%Level_Pressure(0:k)
    atmdiff%Pressure(1:k)       = atmdiff%Pressure(1:k)       - atm2%Pressure(1:k)
    atmdiff%Temperature(1:k)    = atmdiff%Temperature(1:k)    - atm2%Temperature(1:k)
    atmdiff%Absorber(1:k,1:j)   = atmdiff%Absorber(1:k,1:j)   - atm2%Absorber(1:k,1:j)
    ! ...Cloud component
    IF ( atm1%n_Clouds > 0 ) THEN
      DO i = 1, atm1%n_Clouds
        atmdiff%Cloud(i) = atmdiff%Cloud(i) - atm2%Cloud(i)
      END DO
    END IF
    ! ...Aerosol component
    IF ( atm1%n_Aerosols > 0 ) THEN
      DO i = 1, atm1%n_Aerosols
        atmdiff%Aerosol(i) = atmdiff%Aerosol(i) - atm2%Aerosol(i)
      END DO
    END IF

  END FUNCTION CRTM_Atmosphere_Subtract


!
! NAME:
!       Read_Record
!
! PURPOSE:
!       Utility function to read a single atmosphere data record
!

  FUNCTION Read_Record( &
    fid        , &  ! Input
    atm        , &  ! Output
    Quiet      , &  ! Optional input
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN)  :: fid
    TYPE(CRTM_Atmosphere_type), INTENT(OUT) :: atm
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_ReadFile(Record)'
    ! Function variables
    CHARACTER(ML) :: fname
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: n_layers
    INTEGER :: n_absorbers
    INTEGER :: n_clouds
    INTEGER :: n_aerosols

    ! Set up
    err_stat = SUCCESS


    ! Read the dimensions
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      n_layers, &
      n_absorbers, &
      n_clouds, &
      n_aerosols
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensions - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Allocate the Atmosphere structure
    CALL CRTM_Atmosphere_Create( atm, &
                                 n_layers, &
                                 n_absorbers, &
                                 n_clouds, &
                                 n_aerosols )
    IF ( .NOT. CRTM_Atmosphere_Associated( atm ) ) THEN
      msg = 'Error creating output object.'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the climatology model flag and absorber IDs
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      atm%Climatology, &
      atm%Absorber_ID, &
      atm%Absorber_Units
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading atmosphere climatology and absorber IDs - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the atmospheric profile data
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      atm%Level_Pressure, &
      atm%Pressure, &
      atm%Temperature, &
      atm%Absorber
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading atmospheric profile data - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the cloud data
    IF ( n_clouds > 0 ) THEN
      INQUIRE( UNIT=fid,NAME=fname )
      err_stat = CRTM_Cloud_ReadFile( fname, &
                                      atm%Cloud, &
                                      Quiet    = Quiet, &
                                      No_Close = .TRUE., &
                                      Debug    = Debug )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading cloud data'
        CALL Read_Record_Cleanup(); RETURN
      END IF
    END IF


    ! Read the aerosol data
    IF ( n_aerosols > 0 ) THEN
      INQUIRE( UNIT=fid,NAME=fname )
      err_stat = CRTM_Aerosol_ReadFile( fname, &
                                        atm%Aerosol, &
                                        Quiet    = Quiet, &
                                        No_Close = .TRUE., &
                                        Debug    = Debug )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading aerosol data'
        CALL Read_Record_Cleanup(); RETURN
      END IF
    END IF

  CONTAINS

    SUBROUTINE Read_Record_Cleanup()
      CALL CRTM_Atmosphere_Destroy( atm )
      CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup - '//TRIM(io_msg)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_Record_Cleanup

  END FUNCTION Read_Record


!
! NAME:
!       Write_Record
!
! PURPOSE:
!       Function to write a single atmosphere data record
!

  FUNCTION Write_Record( &
    fid  , &  ! Input
    atm  , &  ! Input
    Quiet, &  ! Optional input
    Debug) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN) :: fid
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: atm
    LOGICAL,          OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,          OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_WriteFile(Record)'
    ! Function variables
    CHARACTER(ML) :: fname
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat

    ! Set up
    err_stat = SUCCESS
    IF ( .NOT. CRTM_Atmosphere_Associated( atm ) ) THEN
      msg = 'Input Atmosphere object is not used.'
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the data dimensions
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      atm%n_Layers, &
      atm%n_Absorbers, &
      atm%n_Clouds, &
      atm%n_Aerosols
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensions - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the climatology model flag and absorber IDs
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      atm%Climatology, &
      atm%Absorber_ID, &
      atm%Absorber_Units
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing atmosphere climatology and absorber IDs - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the atmospheric profile data
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      atm%Level_Pressure(0:atm%n_Layers), &
      atm%Pressure(1:atm%n_Layers), &
      atm%Temperature(1:atm%n_Layers), &
      atm%Absorber(1:atm%n_Layers,:)
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing atmospheric profile data - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the cloud data
    IF ( atm%n_Clouds > 0 ) THEN
      INQUIRE( UNIT=fid,NAME=fname )
      err_stat = CRTM_Cloud_WriteFile( fname, &
                                       atm%Cloud(1:atm%n_Clouds), &
                                       Quiet    = Quiet, &
                                       No_Close = .TRUE., &
                                       Debug    = Debug )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing cloud data'
        CALL Write_Record_Cleanup(); RETURN
      END IF
    END IF


    ! Write the aerosol data
    IF ( atm%n_Aerosols > 0 ) THEN
      INQUIRE( UNIT=fid,NAME=fname )
      err_stat = CRTM_Aerosol_WriteFile( fname, &
                                         atm%Aerosol(1:atm%n_Aerosols), &
                                         Quiet    = Quiet, &
                                         No_Close = .TRUE., &
                                         Debug    = Debug )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing aerosol data'
        CALL Write_Record_Cleanup(); RETURN
      END IF
    END IF

  CONTAINS

    SUBROUTINE Write_Record_Cleanup()
      CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup - '//TRIM(io_msg)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_Record_Cleanup

  END FUNCTION Write_Record

END MODULE CRTM_Atmosphere_Define
