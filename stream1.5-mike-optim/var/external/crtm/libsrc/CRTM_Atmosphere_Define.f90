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
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                   Display_Message
  USE Compare_Float_Numbers, ONLY: DEFAULT_N_SIGFIG, &
                                   OPERATOR(.EqualTo.), &
                                   Compares_Within_Tolerance
  USE CRTM_Parameters      , ONLY: ZERO, POINT_5, ONE, &
                                   NO, YES, SET, &
                                   MAX_N_LAYERS, &
                                   TOA_PRESSURE
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
                                   CRTM_Cloud_Associated, &
                                   CRTM_Cloud_Destroy, &
                                   CRTM_Cloud_Create, &
                                   CRTM_Cloud_AddLayerCopy, &
                                   CRTM_Cloud_Zero, &
                                   CRTM_Cloud_IsValid, &
                                   CRTM_Cloud_Inspect, &
                                   CRTM_Cloud_DefineVersion, &
                                   CRTM_Cloud_Compare, &
                                   ! ...Vestige of old module.
                                   CRTM_SetLayers_Cloud
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
                                   CRTM_Aerosol_Associated, &
                                   CRTM_Aerosol_Destroy, &
                                   CRTM_Aerosol_Create, &
                                   CRTM_Aerosol_AddLayerCopy, &
                                   CRTM_Aerosol_Zero, &
                                   CRTM_Aerosol_IsValid, &
                                   CRTM_Aerosol_Inspect, &
                                   CRTM_Aerosol_DefineVersion, &
                                   CRTM_Aerosol_Compare, &
                                   ! ...Vestige of old module.
                                   CRTM_SetLayers_Aerosol
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
  PUBLIC :: CRTM_Cloud_DefineVersion
  PUBLIC :: CRTM_SetLayers_Cloud
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
  PUBLIC :: CRTM_Aerosol_DefineVersion
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
  PUBLIC :: CRTM_Atmosphere_DefineVersion
  PUBLIC :: CRTM_Atmosphere_Compare
  PUBLIC :: CRTM_SetLayers_Atmosphere
  ! ...Utilities
  PUBLIC :: CRTM_Get_AbsorberIdx

  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_Atmosphere_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE CRTM_Atmosphere_Add
  END INTERFACE OPERATOR(+)

  INTERFACE CRTM_SetLayers_Atmosphere
    MODULE PROCEDURE SetLayers_Scalar
    MODULE PROCEDURE SetLayers_Rank1
    MODULE PROCEDURE SetLayers_Rank2
  END INTERFACE CRTM_SetLayers_Atmosphere


  ! -----------------
  ! Module parameters
  ! -----------------
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

  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Atmosphere_Define.f90 7725 2010-05-10 18:20:59Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


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
      Status = Status .OR. ALL(CRTM_Cloud_Associated(Atm%Cloud))
    ! ...Aerosols
    IF ( Atm%n_Aerosols > 0 .AND. ALLOCATED(Atm%Aerosol) ) &
      Status = Status .OR. ALL(CRTM_Aerosol_Associated(Atm%Aerosol))
    
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
!             DIMENSION:  Scalar
!             ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Atmosphere_Inspect( Atm )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atm
    INTEGER :: lClimatology
    INTEGER :: j, k
    INTEGER :: nc, na

    WRITE(*, '(1x,"ATMOSPHERE OBJECT")')
    ! Dimensions
    WRITE(*, '(3x,"n_Layers   :",1x,i0)') Atm%n_Layers   
    WRITE(*, '(3x,"n_Absorbers:",1x,i0)') Atm%n_Absorbers
    WRITE(*, '(3x,"n_Clouds   :",1x,i0)') Atm%n_Clouds   
    WRITE(*, '(3x,"n_Aerosols :",1x,i0)') Atm%n_Aerosols 
    ! Climatology
    lClimatology = Atm%Climatology
    IF ( lClimatology < 1 .OR. &
         lClimatology > N_VALID_CLIMATOLOGY_MODELS ) lClimatology = INVALID_MODEL
    WRITE(*, '(3x,"Climatology    :",1x,a)') CLIMATOLOGY_MODEL_NAME(lClimatology)
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
    IF ( Atm%n_Clouds > 0 ) THEN
      DO nc = 1, Atm%n_Clouds
        CALL CRTM_Cloud_Inspect(Atm%Cloud(nc))
      END DO
    END IF
    ! Aerosol information
    IF ( Atm%n_Aerosols > 0 ) THEN
      DO na = 1, Atm%n_Aerosols
        CALL CRTM_Aerosol_Inspect(Atm%Aerosol(na))
      END DO
    END IF
  END SUBROUTINE CRTM_Atmosphere_Inspect


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
!       CRTM_SetLayers_Atmosphere
! 
! PURPOSE:
!       Function to set the number of layers to use in a CRTM Atmosphere
!       structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_SetLayers_Atmosphere( n_Layers  , &
!                                                 Atmosphere  )
!
! INPUTS:
!       n_Layers:     The value to set the n_Layers component of the 
!                     Atmosphere structure, as well as any of its
!                     Cloud or Aerosol structure components.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Atmosphere:   Atmosphere structure in which the n_Layers dimension
!                     is to be updated.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar, Rank-1, or Rank-2 array
!                     ATTRIBUTES: INTENT(IN OUT)
! OUTPUTS:
!       Atmosphere:   On output, the atmosphere structure with the updated
!                     n_Layers dimension.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar, Rank-1, or Rank-2 array
!                     ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the layer reset was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The argument Atmosphere is INTENT(IN OUT) and is modified upon output.
!
! COMMENTS:
!       - Note that the n_Layers input is *ALWAYS* scalar. Thus, all Atmosphere
!         elements will be set to the same number of layers.
!
!       - If n_Layers <= Atmosphere%Max_Layers, then only the dimension value
!         of the structure and any sub-structures are changed.
!
!       - If n_Layers > Atmosphere%Max_Layers, then the entire structure is
!         reallocated to the required number of layers. No other dimensions
!         of the structure or substructures are altered.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION SetLayers_Scalar( n_Layers   , &  ! Input
                             Atmosphere ) &  ! In/Output
                           RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN)     :: n_Layers
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atmosphere
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SetLayers_Atmosphere(scalar)'
    ! Local variables
    INTEGER :: n_Absorbers 
    INTEGER :: Max_Clouds, n_Clouds    
    INTEGER :: Max_Aerosols, n_Aerosols  

    ! Set up
    ! ------
    err_stat = SUCCESS

    
    ! Set dimension or allocate based on current size
    ! -----------------------------------------------        
    IF ( n_Layers < Atmosphere%Max_Layers ) THEN
      Atmosphere%n_Layers = n_Layers
      ! Reset cloud structure
      IF ( Atmosphere%n_Clouds > 0 ) THEN
        err_stat = CRTM_SetLayers_Cloud( n_Layers, Atmosphere%Cloud )
        IF ( err_stat /= SUCCESS ) THEN
          CALL Display_Message( ROUTINE_NAME, 'Error resetting cloud component', err_stat )
          RETURN
        END IF
      END IF
      ! Reset aerosol structure
      IF ( Atmosphere%n_Aerosols > 0 ) THEN
        err_stat = CRTM_SetLayers_Aerosol( n_Layers, Atmosphere%Aerosol )
        IF ( err_stat /= SUCCESS ) THEN
          CALL Display_Message( ROUTINE_NAME, 'Error resetting Aerosol component', err_stat )
          RETURN
        END IF
      END IF
      ! Reinitialise
      CALL CRTM_Atmosphere_Zero( Atmosphere )
    ELSE
      ! Save other dimensions
      n_Absorbers  = Atmosphere%n_Absorbers 
      Max_Clouds   = MAX(Atmosphere%n_Clouds, Atmosphere%Max_Clouds)
      n_Clouds     = MIN(Atmosphere%n_Clouds, Atmosphere%Max_Clouds)
      Max_Aerosols = MAX(Atmosphere%n_Aerosols, Atmosphere%Max_Aerosols)
      n_Aerosols   = MIN(Atmosphere%n_Aerosols, Atmosphere%Max_Aerosols)
      ! Deallocate
      CALL CRTM_Atmosphere_Destroy( Atmosphere )
      ! Reallocate
      CALL CRTM_Atmosphere_Create( Atmosphere, n_Layers, n_Absorbers, Max_Clouds, Max_Aerosols )
      IF ( .NOT. CRTM_Atmosphere_Associated(Atmosphere) ) THEN
        err_stat = FAILURE
        CALL Display_Message( ROUTINE_NAME, 'Error reallocating atmosphere structure', err_stat )
        RETURN
      END IF
      ! Restore cloud and aerosol use dimensions
      Atmosphere%n_Clouds   = n_Clouds
      Atmosphere%n_Aerosols = n_Aerosols
    END IF

  END FUNCTION SetLayers_Scalar


  FUNCTION SetLayers_Rank1( n_Layers   , &  ! Input
                            Atmosphere ) &  ! In/Output
                          RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN)     :: n_Layers
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atmosphere(:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SetLayers_Atmosphere(rank-1)'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: m, set_stat
    
    ! Set up
    ! ------
    err_stat = SUCCESS


    ! Loop over elements. If an error is encountered,
    ! report it but continue with the reset.
    ! -----------------------------------------------
    DO m = 1, SIZE(Atmosphere)
      set_stat = SetLayers_Scalar( n_Layers, Atmosphere(m) )
      IF ( set_stat /= SUCCESS ) THEN
        err_stat = FAILURE
        WRITE( msg,'("Error resetting element ",i0," Atmosphere array n_Layers")' ) m
        CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      END IF
    END DO
  END FUNCTION SetLayers_Rank1


  FUNCTION SetLayers_Rank2( n_Layers   , &  ! Input
                            Atmosphere ) &  ! In/Output
                          RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN)     :: n_Layers
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atmosphere(:,:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SetLayers_Atmosphere(rank-2)'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: l, m, set_stat
    
    ! Set up
    ! ------
    err_stat = SUCCESS


    ! Loop over elements. If an error is encountered,
    ! report it but continue with the reset.
    ! -----------------------------------------------
    DO m = 1, SIZE(Atmosphere,DIM=2)
      DO l = 1, SIZE(Atmosphere,DIM=1)
        set_stat = SetLayers_Scalar( n_Layers, Atmosphere(l,m) )
        IF ( set_stat /= SUCCESS ) THEN
          err_stat = FAILURE
          WRITE( msg,'("Error resetting element (",i0,",",i0,") Atmosphere array n_Layers")' ) l, m
          CALL Display_Message( ROUTINE_NAME, msg, err_stat )
        END IF
      END DO
    END DO
  END FUNCTION SetLayers_Rank2


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

END MODULE CRTM_Atmosphere_Define
