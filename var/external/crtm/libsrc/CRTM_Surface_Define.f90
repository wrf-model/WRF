!
! CRTM_Surface_Define
!
! Module defining the CRTM Surface structure and containing routines
! to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:  Yong Han,       yong.han@noaa.gov
!                    Quanhua Liu,    quanhua.liu@noaa.gov
!                    Paul van Delst, paul.vandelst@noaa.gov
!                    07-May-2004
!

MODULE CRTM_Surface_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds            , ONLY: fp
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers , ONLY: DEFAULT_N_SIGFIG, &
                                    OPERATOR(.EqualTo.), &
                                    Compares_Within_Tolerance
  USE CRTM_Parameters       , ONLY: ZERO, ONE, NO, YES, SET
  USE CRTM_SensorData_Define, ONLY: CRTM_SensorData_type, &
                                    OPERATOR(==), &          
                                    OPERATOR(+), &           
                                    CRTM_SensorData_Associated, & 
                                    CRTM_SensorData_Destroy, &    
                                    CRTM_SensorData_Create, &     
                                    CRTM_SensorData_Zero, &       
                                    CRTM_SensorData_IsValid, &    
                                    CRTM_SensorData_Inspect, &    
                                    CRTM_SensorData_DefineVersion, &    
                                    CRTM_SensorData_Compare 
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
  ! SensorData enitities
  ! ...Structures
  PUBLIC :: CRTM_SensorData_type
  ! ...Procedures
  PUBLIC :: CRTM_SensorData_Associated
  PUBLIC :: CRTM_SensorData_Destroy
  PUBLIC :: CRTM_SensorData_Create
  PUBLIC :: CRTM_SensorData_Zero
  PUBLIC :: CRTM_SensorData_IsValid
  PUBLIC :: CRTM_SensorData_Inspect
  PUBLIC :: CRTM_SensorData_DefineVersion
  PUBLIC :: CRTM_SensorData_Compare
  ! Surface entities
  ! ...Gross surface parameters
  PUBLIC :: INVALID_SURFACE
  PUBLIC :: LAND_SURFACE
  PUBLIC :: WATER_SURFACE
  PUBLIC :: SNOW_SURFACE
  PUBLIC :: ICE_SURFACE
  PUBLIC :: N_VALID_SURFACE_TYPES
  PUBLIC :: SURFACE_TYPE_NAME
  ! ...Land surface parameters
  PUBLIC :: N_VALID_LAND_TYPES
  PUBLIC :: INVALID_LAND
  PUBLIC :: COMPACTED_SOIL
  PUBLIC :: TILLED_SOIL
  PUBLIC :: SAND
  PUBLIC :: ROCK
  PUBLIC :: IRRIGATED_LOW_VEGETATION
  PUBLIC :: MEADOW_GRASS
  PUBLIC :: SCRUB
  PUBLIC :: BROADLEAF_FOREST
  PUBLIC :: PINE_FOREST
  PUBLIC :: TUNDRA
  PUBLIC :: GRASS_SOIL
  PUBLIC :: BROADLEAF_PINE_FOREST
  PUBLIC :: GRASS_SCRUB
  PUBLIC :: SOIL_GRASS_SCRUB
  PUBLIC :: URBAN_CONCRETE
  PUBLIC :: PINE_BRUSH
  PUBLIC :: BROADLEAF_BRUSH
  PUBLIC :: WET_SOIL
  PUBLIC :: SCRUB_SOIL
  PUBLIC :: BROADLEAF70_PINE30
  PUBLIC :: LAND_TYPE_NAME
  ! ...Water surface parameters
  PUBLIC :: N_VALID_WATER_TYPES
  PUBLIC :: INVALID_WATER
  PUBLIC :: SEA_WATER
  PUBLIC :: FRESH_WATER
  PUBLIC :: WATER_TYPE_NAME
  ! ...Snow surface parameters
  PUBLIC :: N_VALID_SNOW_TYPES
  PUBLIC :: INVALID_SNOW
  PUBLIC :: WET_SNOW
  PUBLIC :: GRASS_AFTER_SNOW
  PUBLIC :: RS_SNOW_A
  PUBLIC :: POWDER_SNOW
  PUBLIC :: RS_SNOW_B
  PUBLIC :: RS_SNOW_C
  PUBLIC :: RS_SNOW_D
  PUBLIC :: THIN_CRUST_SNOW
  PUBLIC :: RS_SNOW_E
  PUBLIC :: BOTTOM_CRUST_SNOW_A
  PUBLIC :: SHALLOW_SNOW
  PUBLIC :: DEEP_SNOW
  PUBLIC :: CRUST_SNOW
  PUBLIC :: MEDIUM_SNOW
  PUBLIC :: BOTTOM_CRUST_SNOW_B
  PUBLIC :: THICK_CRUST_SNOW
  PUBLIC :: NEW_SNOW
  PUBLIC :: OLD_SNOW
  PUBLIC :: SNOW_TYPE_NAME
  ! ...Ice surface parameters
  PUBLIC :: N_VALID_ICE_TYPES
  PUBLIC :: INVALID_ICE
  PUBLIC :: FRESH_ICE
  PUBLIC :: FIRST_YEAR_SEA_ICE
  PUBLIC :: MULTI_YEAR_SEA_ICE
  PUBLIC :: ICE_FLOE
  PUBLIC :: ICE_RIDGE
  PUBLIC :: ICE_TYPE_NAME
  ! ...Structures
  PUBLIC :: CRTM_Surface_type
  ! ...Procedures
  PUBLIC :: CRTM_Surface_Associated
  PUBLIC :: CRTM_Surface_Destroy
  PUBLIC :: CRTM_Surface_Create
  PUBLIC :: CRTM_Surface_Zero
  PUBLIC :: CRTM_Surface_IsValid
  PUBLIC :: CRTM_Surface_Inspect
  PUBLIC :: CRTM_Surface_IsCoverageValid
  PUBLIC :: CRTM_Surface_CoverageType
  PUBLIC :: CRTM_Surface_DefineVersion
  PUBLIC :: CRTM_Surface_Compare

  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_Surface_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE CRTM_Surface_Add
  END INTERFACE OPERATOR(+)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! The gross surface types. These are used for
  ! cross-checking with the coverage fractions
  ! of each gross surface types.
  INTEGER, PARAMETER :: INVALID_SURFACE = 0
  INTEGER, PARAMETER :: LAND_SURFACE    = 1
  INTEGER, PARAMETER :: WATER_SURFACE   = 2
  INTEGER, PARAMETER :: SNOW_SURFACE    = 4
  INTEGER, PARAMETER :: ICE_SURFACE     = 8
  INTEGER, PARAMETER :: N_VALID_SURFACE_TYPES = LAND_SURFACE  + &
                                                WATER_SURFACE + &
                                                SNOW_SURFACE  + &
                                                ICE_SURFACE
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_VALID_SURFACE_TYPES ) :: &
    SURFACE_TYPE_NAME = (/ 'Invalid surface type     ', &
                           'Land                     ', &
                           'Water                    ', &
                           'Land + water             ', &
                           'Snow                     ', &
                           'Land + snow              ', &
                           'Water + snow             ', &
                           'Land + water + snow      ', &
                           'Ice                      ', &
                           'Land + ice               ', &
                           'Water + ice              ', &
                           'Land + water + ice       ', &
                           'Snow + ice               ', &
                           'Land + snow + ice        ', &
                           'Water + snow + ice       ', &
                           'Land + water + snow + ice' /)


  ! For land, the land types
  INTEGER, PARAMETER :: N_VALID_LAND_TYPES = 20
  INTEGER, PARAMETER :: INVALID_LAND             =  0
  INTEGER, PARAMETER :: COMPACTED_SOIL           =  1
  INTEGER, PARAMETER :: TILLED_SOIL              =  2
  INTEGER, PARAMETER :: SAND                     =  3
  INTEGER, PARAMETER :: ROCK                     =  4
  INTEGER, PARAMETER :: IRRIGATED_LOW_VEGETATION =  5
  INTEGER, PARAMETER :: MEADOW_GRASS             =  6
  INTEGER, PARAMETER :: SCRUB                    =  7
  INTEGER, PARAMETER :: BROADLEAF_FOREST         =  8
  INTEGER, PARAMETER :: PINE_FOREST              =  9
  INTEGER, PARAMETER :: TUNDRA                   = 10
  INTEGER, PARAMETER :: GRASS_SOIL               = 11
  INTEGER, PARAMETER :: BROADLEAF_PINE_FOREST    = 12
  INTEGER, PARAMETER :: GRASS_SCRUB              = 13
  INTEGER, PARAMETER :: SOIL_GRASS_SCRUB         = 14
  INTEGER, PARAMETER :: URBAN_CONCRETE           = 15
  INTEGER, PARAMETER :: PINE_BRUSH               = 16
  INTEGER, PARAMETER :: BROADLEAF_BRUSH          = 17
  INTEGER, PARAMETER :: WET_SOIL                 = 18
  INTEGER, PARAMETER :: SCRUB_SOIL               = 19
  INTEGER, PARAMETER :: BROADLEAF70_PINE30       = 20
    CHARACTER(*), PARAMETER, DIMENSION( 0:N_VALID_LAND_TYPES ) :: &
    LAND_TYPE_NAME = (/ 'Invalid land surface type', &
                        'Compacted soil           ', &
                        'Tilled soil              ', &
                        'Sand                     ', &
                        'Rock                     ', &
                        'Irrigated low vegetation ', &
                        'Meadow grass             ', &
                        'Scrub                    ', &
                        'Broadleaf forest         ', &
                        'Pine forest              ', &
                        'Tundra                   ', &
                        'Grass-soil               ', &
                        'Broadleaf-pine forest    ', &
                        'Grass-scrub              ', &
                        'Soil-grass-scrub         ', &
                        'Urban concrete           ', &
                        'Pine brush               ', &
                        'Broadleaf brush          ', &
                        'Wet soil                 ', &
                        'Scrub-soil               ', &
                        'Broadleaf(70)-Pine(30)   ' /)

  ! For water, the water types
  INTEGER, PARAMETER :: N_VALID_WATER_TYPES = 2
  INTEGER, PARAMETER :: INVALID_WATER  =  0
  INTEGER, PARAMETER :: SEA_WATER      =  1
  INTEGER, PARAMETER :: FRESH_WATER    =  2
    CHARACTER(*), PARAMETER, DIMENSION( 0:N_VALID_WATER_TYPES ) :: &
    WATER_TYPE_NAME = (/ 'Invalid water surface type', &
                         'Sea water                 ', &
                         'Fresh water               ' /)

  ! For snow, the snow types.
  INTEGER, PARAMETER :: N_VALID_SNOW_TYPES = 16
  INTEGER, PARAMETER :: INVALID_SNOW        =  0
  INTEGER, PARAMETER :: WET_SNOW            =  1
  INTEGER, PARAMETER :: GRASS_AFTER_SNOW    =  2
  INTEGER, PARAMETER :: RS_SNOW_A           =  3
  INTEGER, PARAMETER :: POWDER_SNOW         =  4
  INTEGER, PARAMETER :: RS_SNOW_B           =  5
  INTEGER, PARAMETER :: RS_SNOW_C           =  6
  INTEGER, PARAMETER :: RS_SNOW_D           =  7
  INTEGER, PARAMETER :: THIN_CRUST_SNOW     =  8
  INTEGER, PARAMETER :: RS_SNOW_E           =  9
  INTEGER, PARAMETER :: BOTTOM_CRUST_SNOW_A = 10
  INTEGER, PARAMETER :: SHALLOW_SNOW        = 11
  INTEGER, PARAMETER :: DEEP_SNOW           = 12
  INTEGER, PARAMETER :: CRUST_SNOW          = 13
  INTEGER, PARAMETER :: MEDIUM_SNOW         = 14
  INTEGER, PARAMETER :: BOTTOM_CRUST_SNOW_B = 15
  INTEGER, PARAMETER :: THICK_CRUST_SNOW    = 16
  INTEGER, PARAMETER :: NEW_SNOW = POWDER_SNOW
  INTEGER, PARAMETER :: OLD_SNOW = THICK_CRUST_SNOW
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_VALID_SNOW_TYPES ) :: &
    SNOW_TYPE_NAME = (/ 'Invalid snow surface type', &
                        'Wet snow                 ', &
                        'Grass after snow         ', &
                        'RS snow(A)               ', &
                        'Powder snow              ', &
                        'RS snow(B)               ', &
                        'RS snow(C)               ', &
                        'RS snow(D)               ', &
                        'Thin Crust snow          ', &
                        'RS snow(E)               ', &
                        'Bottom crust snow(A)     ', &
                        'Shallow snow             ', &
                        'Deep snow                ', &
                        'Crust snow               ', &
                        'Medium snow              ', &
                        'Bottom crust snow(B)     ', &
                        'Thick crust snow         ' /)

  ! For ice, the ice types.
  INTEGER, PARAMETER :: N_VALID_ICE_TYPES = 5
  INTEGER, PARAMETER :: INVALID_ICE        =  0
  INTEGER, PARAMETER :: FRESH_ICE          =  1
  INTEGER, PARAMETER :: FIRST_YEAR_SEA_ICE =  2
  INTEGER, PARAMETER :: MULTI_YEAR_SEA_ICE =  3
  INTEGER, PARAMETER :: ICE_FLOE           =  4
  INTEGER, PARAMETER :: ICE_RIDGE          =  5
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_VALID_ICE_TYPES ) :: &
     ICE_TYPE_NAME = (/ 'Invalid ice surface type ', &
                        'Fresh ice                ', &
                        'First year sea ice       ', &
                        'Multiple year sea ice    ', &
                        'Ice floe                 ', &
                        'Ice ridge                '/)

  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Surface_Define.f90 6881 2010-03-05 23:33:01Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! ------------------------
  ! Default value parameters
  ! ------------------------
  ! Land surface type data
  INTEGER,  PARAMETER :: DEFAULT_LAND_TYPE             = GRASS_SOIL
  REAL(fp), PARAMETER :: DEFAULT_LAND_TEMPERATURE      = 283.0_fp  ! K
  REAL(fp), PARAMETER :: DEFAULT_SOIL_MOISTURE_CONTENT = 0.05_fp   ! g/cm^3
  REAL(fp), PARAMETER :: DEFAULT_CANOPY_WATER_CONTENT  = 0.05_fp   ! g/cm^3
  REAL(fp), PARAMETER :: DEFAULT_VEGETATION_FRACTION   = 0.3_fp    ! 30%
  REAL(fp), PARAMETER :: DEFAULT_SOIL_TEMPERATURE      = 283.0_fp  ! K
  ! Water type data
  INTEGER,  PARAMETER :: DEFAULT_WATER_TYPE        = SEA_WATER
  REAL(fp), PARAMETER :: DEFAULT_WATER_TEMPERATURE = 283.0_fp   ! K
  REAL(fp), PARAMETER :: DEFAULT_WIND_SPEED        = 5.0_fp     ! m/s
  REAL(fp), PARAMETER :: DEFAULT_WIND_DIRECTION    = 0.0_fp     ! North
  REAL(fp), PARAMETER :: DEFAULT_SALINITY          = 33.0_fp    ! ppmv
  ! Snow surface type data
  INTEGER,  PARAMETER :: DEFAULT_SNOW_TYPE        = NEW_SNOW
  REAL(fp), PARAMETER :: DEFAULT_SNOW_TEMPERATURE = 263.0_fp   ! K
  REAL(fp), PARAMETER :: DEFAULT_SNOW_DEPTH       = 50.0_fp    ! mm
  REAL(fp), PARAMETER :: DEFAULT_SNOW_DENSITY     = 0.2_fp     ! g/cm^3
  REAL(fp), PARAMETER :: DEFAULT_SNOW_GRAIN_SIZE  = 2.0_fp     ! mm
  ! Ice surface type data
  INTEGER,  PARAMETER :: DEFAULT_ICE_TYPE        = FRESH_ICE
  REAL(fp), PARAMETER :: DEFAULT_ICE_TEMPERATURE = 263.0_fp  ! K
  REAL(fp), PARAMETER :: DEFAULT_ICE_THICKNESS   = 10.0_fp   ! mm
  REAL(fp), PARAMETER :: DEFAULT_ICE_DENSITY     = 0.9_fp    ! g/cm^3
  REAL(fp), PARAMETER :: DEFAULT_ICE_ROUGHNESS   = ZERO


  ! ----------------------------
  ! Surface structure definition
  ! ----------------------------
  !:tdoc+:
  TYPE :: CRTM_Surface_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .TRUE.  ! Placeholder for future expansion
    ! Dimension values
    ! ...None yet
    ! Gross type of surface determined by coverage
    REAL(fp) :: Land_Coverage  = ZERO
    REAL(fp) :: Water_Coverage = ZERO
    REAL(fp) :: Snow_Coverage  = ZERO
    REAL(fp) :: Ice_Coverage   = ZERO
    ! Land surface type data
    INTEGER  :: Land_Type             = DEFAULT_LAND_TYPE
    REAL(fp) :: Land_Temperature      = DEFAULT_LAND_TEMPERATURE
    REAL(fp) :: Soil_Moisture_Content = DEFAULT_SOIL_MOISTURE_CONTENT
    REAL(fp) :: Canopy_Water_Content  = DEFAULT_CANOPY_WATER_CONTENT
    REAL(fp) :: Vegetation_Fraction   = DEFAULT_VEGETATION_FRACTION
    REAL(fp) :: Soil_Temperature      = DEFAULT_SOIL_TEMPERATURE
    ! Water type data
    INTEGER  :: Water_Type        = DEFAULT_WATER_TYPE
    REAL(fp) :: Water_Temperature = DEFAULT_WATER_TEMPERATURE
    REAL(fp) :: Wind_Speed        = DEFAULT_WIND_SPEED
    REAL(fp) :: Wind_Direction    = DEFAULT_WIND_DIRECTION
    REAL(fp) :: Salinity          = DEFAULT_SALINITY
    ! Snow surface type data
    INTEGER  :: Snow_Type        = DEFAULT_SNOW_TYPE
    REAL(fp) :: Snow_Temperature = DEFAULT_SNOW_TEMPERATURE
    REAL(fp) :: Snow_Depth       = DEFAULT_SNOW_DEPTH
    REAL(fp) :: Snow_Density     = DEFAULT_SNOW_DENSITY
    REAL(fp) :: Snow_Grain_Size  = DEFAULT_SNOW_GRAIN_SIZE
    ! Ice surface type data
    INTEGER  :: Ice_Type        = DEFAULT_ICE_TYPE
    REAL(fp) :: Ice_Temperature = DEFAULT_ICE_TEMPERATURE
    REAL(fp) :: Ice_Thickness   = DEFAULT_ICE_THICKNESS
    REAL(fp) :: Ice_Density     = DEFAULT_ICE_DENSITY
    REAL(fp) :: Ice_Roughness   = DEFAULT_ICE_ROUGHNESS
    ! SensorData containing channel brightness temperatures
    TYPE(CRTM_SensorData_type) :: SensorData
  END TYPE CRTM_Surface_type
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
!       CRTM_Surface_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a CRTM Surface object.
!
! CALLING SEQUENCE:
!       Status = CRTM_Surface_Associated( Sfc )
!
! OBJECTS:
!       Sfc:       Surface structure which is to have its member's
!                  status tested.
!                  UNITS:      N/A
!                  TYPE:       CRTM_Surface_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:    The return value is a logical value indicating the
!                  status of the Surface members.
!                    .TRUE.  - if the array components are allocated.
!                    .FALSE. - if the array components are not allocated.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Surface_Associated( Sfc ) RESULT( Status )
    TYPE(CRTM_Surface_type), INTENT(IN) :: Sfc
    LOGICAL :: Status

    Status = Sfc%Is_Allocated
    ! ...SensorData
    Status = Status .AND. CRTM_SensorData_Associated(Sfc%SensorData)
    
  END FUNCTION CRTM_Surface_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Surface_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize CRTM Surface objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_Surface_Destroy( Sfc )
!
! OBJECTS:
!       Sfc:          Re-initialized Surface structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Surface_Destroy( Sfc )
    TYPE(CRTM_Surface_type), INTENT(OUT) :: Sfc
    Sfc%Is_Allocated = .TRUE.  ! Placeholder for future expansion
  END SUBROUTINE CRTM_Surface_Destroy
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Surface_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of the CRTM Surface object.
!
! CALLING SEQUENCE:
!       CALL CRTM_Surface_Create( Sfc       , &
!                                 n_Channels  )
!
! OBJECTS:
!       Sfc:          Surface structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUT ARGUMENTS:
!       n_Channels:   Number of channels dimension of SensorData
!                     substructure
!                     ** Note: Can be = 0 (i.e. no sensor data). **
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as Surface object
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Surface_Create( &
    Sfc       , &  ! Output
    n_Channels  )  ! Input
    ! Arguments
    TYPE(CRTM_Surface_type), INTENT(OUT) :: Sfc
    INTEGER                , INTENT(IN)  :: n_Channels

    ! Check input
    IF ( n_Channels < 0 ) RETURN
    
    ! Perform the substructure allocation
    ! ...SensorData
    IF ( n_Channels > 0 ) CALL CRTM_SensorData_Create( Sfc%SensorData, n_Channels )

    ! Set allocation indicator
    Sfc%Is_Allocated = .TRUE.

  END SUBROUTINE CRTM_Surface_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Surface_Zero
! 
! PURPOSE:
!       Elemental subroutine to zero out the data arrays
!       in a CRTM Surface object.
!
! CALLING SEQUENCE:
!       CALL CRTM_Surface_Zero( Sfc )
!
! OUTPUT ARGUMENTS:
!       Sfc:          CRTM Surface structure in which the data arrays
!                     are to be zeroed out.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       - The various surface type indicator flags are
!         *NOT* reset in this routine.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Surface_Zero( Sfc )
    TYPE(CRTM_Surface_type), INTENT(IN OUT) :: Sfc

    ! Zero the components
    ! ...Coverage fractions
    Sfc%Land_Coverage  = ZERO
    Sfc%Water_Coverage = ZERO
    Sfc%Snow_Coverage  = ZERO
    Sfc%Ice_Coverage   = ZERO
    ! ...The various surface types
    CALL CRTM_LandSurface_Zero(sfc)
    CALL CRTM_WaterSurface_Zero(sfc)
    CALL CRTM_SnowSurface_Zero(sfc)
    CALL CRTM_IceSurface_Zero(sfc)

    ! Reset the structure components
    IF ( CRTM_SensorData_Associated(Sfc%SensorData) ) CALL CRTM_SensorData_Zero(Sfc%SensorData)

  END SUBROUTINE CRTM_Surface_Zero


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Surface_IsValid
!
! PURPOSE:
!       Non-pure function to perform some simple validity checks on a
!       CRTM Surface object. 
!
!       If invalid data is found, a message is printed to stdout.
!
! CALLING SEQUENCE:
!       result = CRTM_Surface_IsValid( Sfc )
!
!         or
!
!       IF ( CRTM_Surface_IsValid( Sfc ) ) THEN....
!
! OBJECTS:
!       Sfc:       CRTM Surface object which is to have its
!                  contents checked.
!                  UNITS:      N/A
!                  TYPE:       CRTM_Surface_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:    Logical variable indicating whether or not the input
!                  passed the check.
!                  If == .FALSE., Surface object is unused or contains
!                                 invalid data.
!                     == .TRUE.,  Surface object can be used in CRTM.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Surface_IsValid( Sfc ) RESULT( IsValid )
    TYPE(CRTM_Surface_type), INTENT(IN) :: Sfc
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Surface_IsValid'
    CHARACTER(ML) :: msg
    
    ! Check the gross surface type indicators
    IsValid = CRTM_Surface_IsCoverageValid(sfc)
    IF ( .NOT. IsValid ) THEN
      msg = 'Invalid surface coverage fraction(s) found'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
    ENDIF

    ! Check the various surface types
    IF ( Sfc%Land_Coverage  > ZERO ) IsValid = CRTM_LandSurface_IsValid(sfc)  .AND. IsValid
    IF ( Sfc%Water_Coverage > ZERO ) IsValid = CRTM_WaterSurface_IsValid(sfc) .AND. IsValid
    IF ( Sfc%Snow_Coverage  > ZERO ) IsValid = CRTM_SnowSurface_IsValid(sfc)  .AND. IsValid
    IF ( Sfc%Ice_Coverage   > ZERO ) IsValid = CRTM_IceSurface_IsValid(sfc)   .AND. IsValid

    ! Structure components
    IF ( CRTM_SensorData_Associated(Sfc%SensorData) ) &
      IsValid = CRTM_SensorData_IsValid( Sfc%SensorData ) .AND. IsValid

  END FUNCTION CRTM_Surface_IsValid



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Surface_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CRTM Surface object to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_Surface_Inspect( Sfc )
!
! INPUTS:
!       Sfc:  CRTM Surface object to display.
!             UNITS:      N/A
!             TYPE:       CRTM_Surface_type
!             DIMENSION:  Scalar
!             ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Surface_Inspect( Sfc )
    TYPE(CRTM_Surface_type), INTENT(IN) :: Sfc
    WRITE(*, '(1x,"Surface OBJECT")')
    ! Surface coverage
    WRITE(*, '(3x,"Land Coverage :",1x,f6.3)') Sfc%Land_Coverage 
    WRITE(*, '(3x,"Water Coverage:",1x,f6.3)') Sfc%Water_Coverage
    WRITE(*, '(3x,"Snow Coverage :",1x,f6.3)') Sfc%Snow_Coverage 
    WRITE(*, '(3x,"Ice Coverage  :",1x,f6.3)') Sfc%Ice_Coverage  
    ! The various surface types
    CALL CRTM_LandSurface_Inspect(sfc)
    CALL CRTM_WaterSurface_Inspect(sfc)
    CALL CRTM_SnowSurface_Inspect(sfc)
    CALL CRTM_IceSurface_Inspect(sfc)
    ! SensorData information
    IF ( CRTM_SensorData_Associated(Sfc%SensorData) ) &
      CALL CRTM_SensorData_Inspect(Sfc%SensorData)

  END SUBROUTINE CRTM_Surface_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Surface_IsCoverageValid
!
! PURPOSE:
!       Function to determine if the coverage fractions are valid
!       for a CRTM Surface object. 
!
! CALLING SEQUENCE:
!       result = CRTM_Surface_IsCoverageValid( Sfc )
!
! OBJECTS:
!       Sfc:       CRTM Surface object which is to have its
!                  coverage fractions checked.
!                  UNITS:      N/A
!                  TYPE:       CRTM_Surface_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:    Logical variable indicating whether or not the input
!                  passed the check.
!                  If == .FALSE., Surface object coverage fractions are invalid.
!                     == .TRUE.,  Surface object coverage fractions are valid.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Surface_IsCoverageValid( Sfc ) RESULT( IsValid )
    TYPE(CRTM_Surface_type), INTENT(IN) :: Sfc
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Surface_IsCoverageValid'
    REAL(fp)    , PARAMETER :: TOLERANCE = 1.0e-10_fp
    CHARACTER(ML) :: msg
    REAL(fp) :: Total_Coverage

    ! Compute the total coverage
    Total_Coverage = Sfc%Land_Coverage + Sfc%Water_Coverage + &
                     Sfc%Snow_Coverage + Sfc%Ice_Coverage

    ! Check coverage fractions for < 0 and > 1
    IsValid = IsCoverageValid(Sfc%Land_Coverage, 'Land') 
    IsValid = IsValid .AND. IsCoverageValid(Sfc%Water_Coverage, 'Water') 
    IsValid = IsValid .AND. IsCoverageValid(Sfc%Snow_Coverage, 'Snow') 
    IsValid = IsValid .AND. IsCoverageValid(Sfc%Ice_Coverage, 'Ice')
    
    ! Check total coverage sums to 1
    IF ( ABS(Total_Coverage-ONE) > TOLERANCE ) THEN
      WRITE( msg,'("Total coverage fraction does not sum to 1 +/- ",es13.6)' ) TOLERANCE
      CALL Display_Message( ROUTINE_NAME,msg,INFORMATION )
      IsValid = .FALSE.
    END IF

  CONTAINS
  
    FUNCTION IsCoverageValid( Coverage, Name ) RESULT( IsValid )
      REAL(fp)    , INTENT(IN) :: Coverage
      CHARACTER(*), INTENT(IN) :: Name
      LOGICAL :: IsValid
      
      IsValid = .TRUE.
      
      ! Check for coverage < -TOLERANCE
      IF ( Coverage < -TOLERANCE ) THEN
        WRITE( msg,'(a," coverage fraction is < ",es13.6)' ) TRIM(Name), -TOLERANCE
        CALL Display_Message( ROUTINE_NAME,msg,INFORMATION )
        IsValid = .FALSE.
      END IF

      ! Check for coverage > 1+TOLERANCE
      IF ( Coverage > ONE+TOLERANCE ) THEN
        WRITE( msg,'(a," coverage fraction is > 1 +",es13.6)' ) TRIM(Name), TOLERANCE
        CALL Display_Message( ROUTINE_NAME,msg,INFORMATION )
        IsValid = .FALSE.
      END IF
  
    END FUNCTION IsCoverageValid

  END FUNCTION CRTM_Surface_IsCoverageValid



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Surface_CoverageType
!
! PURPOSE:
!       Elemental function to return the gross surface type based on coverage.
!
! CALLING SEQUENCE:
!       type = CRTM_Surface_CoverageType( sfc )
!
! INPUTS:
!       Sfc:  CRTM Surface object for which the gross surface type is required.
!             UNITS:      N/A
!             TYPE:       CRTM_Surface_type
!             DIMENSION:  Scalar or any rank
!             ATTRIBUTES: INTENT(IN)
!
! FUNCTION:
!       type: Surface type indicator for the passed CRTM Surface object.
!             UNITS:      N/A
!             TYPE:       INTEGER
!             DIMENSION:  Same as input
!
! COMMENTS:
!       For a scalar Surface object, this function result can be used to
!       determine what gross surface types are included by using it to
!       index the SURFACE_TYPE_NAME parameter arrays, e.g.
!
!         WRITE(*,*) SURFACE_TYPE_NAME(CRTM_Surface_CoverageType(sfc))
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Surface_CoverageType( sfc ) RESULT( Coverage_Type )
    TYPE(CRTM_Surface_type), INTENT(IN) :: sfc
    INTEGER :: Coverage_Type
    Coverage_Type = 0
    IF ( sfc%Land_Coverage  > ZERO ) Coverage_Type = LAND_SURFACE
    IF ( sfc%Water_Coverage > ZERO ) Coverage_Type = Coverage_Type + WATER_SURFACE
    IF ( sfc%Snow_Coverage  > ZERO ) Coverage_Type = Coverage_Type + SNOW_SURFACE
    IF ( sfc%Ice_Coverage   > ZERO ) Coverage_Type = Coverage_Type + ICE_SURFACE
  END FUNCTION CRTM_Surface_CoverageType
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Surface_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Surface_DefineVersion( Id )
!
! OUTPUT ARGUMENTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Surface_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_Surface_DefineVersion


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       CRTM_Surface_Compare
!
! PURPOSE:
!       Elemental function to compare two CRTM_Surface objects to within
!       a user specified number of significant figures.
!
! CALLING SEQUENCE:
!       is_comparable = CRTM_Surface_Compare( x, y, n_SigFig=n_SigFig )
!
! OBJECTS:
!       x, y:          Two CRTM Surface objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Surface_type
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
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Surface_Compare( &
    x, &
    y, &
    n_SigFig ) &
  RESULT( is_comparable )
    TYPE(CRTM_Surface_type), INTENT(IN) :: x, y
    INTEGER,       OPTIONAL, INTENT(IN) :: n_SigFig
    LOGICAL :: is_comparable
    ! Variables
    INTEGER :: n

    ! Set up
    is_comparable = .FALSE.
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF
    
    ! Compare gross surface type coverage
    IF ( (.NOT. Compares_Within_Tolerance(x%Land_Coverage ,y%Land_Coverage ,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Water_Coverage,y%Water_Coverage,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Snow_Coverage ,y%Snow_Coverage ,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Ice_Coverage  ,y%Ice_Coverage  ,n)) ) RETURN

    ! Compare the land surface type data
    IF ( .NOT. CRTM_LandSurface_Compare(x,y,n_SigFig=n) ) RETURN

    ! Compare the water surface type data
    IF ( .NOT. CRTM_WaterSurface_Compare(x,y,n_SigFig=n) ) RETURN

    ! Compare the snow surface type data
    IF ( .NOT. CRTM_SnowSurface_Compare(x,y,n_SigFig=n) ) RETURN

    ! Compare the ice surface type data
    IF ( .NOT. CRTM_IceSurface_Compare(x,y,n_SigFig=n) ) RETURN

    ! Check the SensorData
    IF ( CRTM_SensorData_Associated(x%SensorData) .AND. &
         CRTM_SensorData_Associated(y%SensorData) ) THEN
      IF ( .NOT. CRTM_SensorData_Compare(x%SensorData,y%SensorData,n_SigFig=n) ) RETURN
    END IF

    ! If we get here, the structures are comparable
    is_comparable = .TRUE.
    
  END FUNCTION CRTM_Surface_Compare


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
!       CRTM_Surface_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CRTM_Surface objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CRTM_Surface_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CRTM Surface objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Surface_type
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

  ELEMENTAL FUNCTION CRTM_Surface_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_Surface_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    ! Check the gross surface type coverage
    is_equal = ( (x%Land_Coverage  .EqualTo. y%Land_Coverage ) .AND. &
                 (x%Water_Coverage .EqualTo. y%Water_Coverage) .AND. &
                 (x%Snow_Coverage  .EqualTo. y%Snow_Coverage ) .AND. &
                 (x%Ice_Coverage   .EqualTo. y%Ice_Coverage  )       )
    IF ( .NOT. is_equal ) RETURN

    ! Check the land surface type data
    is_equal = is_equal .AND. CRTM_LandSurface_Equal(x,y)
    IF ( .NOT. is_equal ) RETURN

    ! Check the water surface type data
    is_equal = is_equal .AND. CRTM_WaterSurface_Equal(x,y)
    IF ( .NOT. is_equal ) RETURN

    ! Check the snow surface type data
    is_equal = is_equal .AND. CRTM_SnowSurface_Equal(x,y)
    IF ( .NOT. is_equal ) RETURN

    ! Check the ice surface type data
    is_equal = is_equal .AND. CRTM_IceSurface_Equal(x,y)
    IF ( .NOT. is_equal ) RETURN

    ! Check the SensorData
    IF ( CRTM_SensorData_Associated(x%SensorData) .AND. &
         CRTM_SensorData_Associated(y%SensorData) ) THEN
      is_equal = is_equal .AND. (x%SensorData == y%SensorData)
    END IF
    
  END FUNCTION CRTM_Surface_Equal
  
  
!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Surface_Add
!
! PURPOSE:
!       Pure function to add two CRTM Surface objects.
!       Used in OPERATOR(+) interface block.
!
! CALLING SEQUENCE:
!       sfcsum = CRTM_Surface_Add( sfc1, sfc2 )
!
!         or
!
!       sfcsum = sfc1 + sfc2
!
!
! INPUTS:
!       sfc1, sfc2: The Surface objects to add.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Surface_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!       sfcsum:     Surface structure containing the added components.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Surface_type
!                   DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Surface_Add( sfc1, sfc2 ) RESULT( sfcsum )
    TYPE(CRTM_Surface_type), INTENT(IN) :: sfc1, sfc2
    TYPE(CRTM_Surface_type) :: sfcsum

    ! Copy the first structure
    sfcsum = sfc1

    ! And add its components to the second one
    sfcsum%Land_Temperature      = sfcsum%Land_Temperature      + sfc2%Land_Temperature     
    sfcsum%Soil_Moisture_Content = sfcsum%Soil_Moisture_Content + sfc2%Soil_Moisture_Content
    sfcsum%Canopy_Water_Content  = sfcsum%Canopy_Water_Content  + sfc2%Canopy_Water_Content 
    sfcsum%Vegetation_Fraction   = sfcsum%Vegetation_Fraction   + sfc2%Vegetation_Fraction  
    sfcsum%Soil_Temperature      = sfcsum%Soil_Temperature      + sfc2%Soil_Temperature 
    sfcsum%Water_Temperature     = sfcsum%Water_Temperature     + sfc2%Water_Temperature
    sfcsum%Wind_Speed            = sfcsum%Wind_Speed            + sfc2%Wind_Speed       
    sfcsum%Wind_Direction        = sfcsum%Wind_Direction        + sfc2%Wind_Direction   
    sfcsum%Salinity              = sfcsum%Salinity              + sfc2%Salinity         
    sfcsum%Snow_Temperature      = sfcsum%Snow_Temperature      + sfc2%Snow_Temperature 
    sfcsum%Snow_Depth            = sfcsum%Snow_Depth            + sfc2%Snow_Depth     
    sfcsum%Snow_Density          = sfcsum%Snow_Density          + sfc2%Snow_Density   
    sfcsum%Snow_Grain_Size       = sfcsum%Snow_Grain_Size       + sfc2%Snow_Grain_Size
    sfcsum%Ice_Temperature       = sfcsum%Ice_Temperature       + sfc2%Ice_Temperature
    sfcsum%Ice_Thickness         = sfcsum%Ice_Thickness         + sfc2%Ice_Thickness  
    sfcsum%Ice_Density           = sfcsum%Ice_Density           + sfc2%Ice_Density    
    sfcsum%Ice_Roughness         = sfcsum%Ice_Roughness         + sfc2%Ice_Roughness  
    ! ...SensorData component
    IF ( CRTM_SensorData_Associated(sfc1%SensorData) .AND. &
         CRTM_SensorData_Associated(sfc2%SensorData)       ) THEN
      sfcsum%SensorData = sfcsum%SensorData + sfc2%SensorData
    END IF

  END FUNCTION CRTM_Surface_Add



!##################################################################################
!##################################################################################
!##                                                                              ##
!##     ## PROCEDURES BELOW WILL EVENTUALLY BE MOVED TO THEIR OWN MODULE ##      ##
!##                                                                              ##
!##################################################################################
!##################################################################################

! =============================
! LAND TYPE SPECIFIC PROCEDURES
! =============================
  ELEMENTAL SUBROUTINE CRTM_LandSurface_Zero( Sfc )
    TYPE(CRTM_Surface_type), INTENT(IN OUT) :: Sfc
    ! Zero land surface type data
    Sfc%Land_Temperature      = ZERO
    Sfc%Soil_Moisture_Content = ZERO
    Sfc%Canopy_Water_Content  = ZERO
    Sfc%Vegetation_Fraction   = ZERO
    Sfc%Soil_Temperature      = ZERO
  END SUBROUTINE CRTM_LandSurface_Zero

  
  FUNCTION CRTM_LandSurface_IsValid( Sfc ) RESULT( IsValid )
    TYPE(CRTM_Surface_type), INTENT(IN) :: Sfc
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_LandSurface_IsValid'
    CHARACTER(ML) :: msg
    
    ! Setup
    IsValid = .TRUE.
    
    ! Check the data
    IF ( Sfc%Land_Type < 1 .OR. Sfc%Land_Type > N_VALID_LAND_TYPES ) THEN
      msg = 'Invalid Land Surface type'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( Sfc%Land_Temperature      < ZERO .OR. &
         Sfc%Soil_Moisture_Content < ZERO .OR. &
         Sfc%Canopy_Water_Content  < ZERO .OR. &
         Sfc%Vegetation_Fraction   < ZERO .OR. &
         Sfc%Soil_Temperature      < ZERO      ) THEN
      msg = 'Invalid Land Surface data'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    
  END FUNCTION CRTM_LandSurface_IsValid


  SUBROUTINE CRTM_LandSurface_Inspect( Sfc )
    TYPE(CRTM_Surface_type), INTENT(IN) :: Sfc
    INTEGER :: lType
    lType = Sfc%Land_Type
    IF ( lType < 1 .OR. lType > N_VALID_LAND_TYPES ) lType = INVALID_LAND
    WRITE(*, '(3x,"Land type            :",1x,a)') TRIM(LAND_TYPE_NAME(lType))
    WRITE(*, '(3x,"Land Temperature     :",1x,es13.6)') Sfc%Land_Temperature     
    WRITE(*, '(3x,"Soil Moisture Content:",1x,es13.6)') Sfc%Soil_Moisture_Content
    WRITE(*, '(3x,"Canopy Water Content :",1x,es13.6)') Sfc%Canopy_Water_Content 
    WRITE(*, '(3x,"Vegetation Fraction  :",1x,es13.6)') Sfc%Vegetation_Fraction  
    WRITE(*, '(3x,"Soil Temperature     :",1x,es13.6)') Sfc%Soil_Temperature     
  END SUBROUTINE CRTM_LandSurface_Inspect


  ELEMENTAL FUNCTION CRTM_LandSurface_Compare( x, y, n_SigFig ) RESULT( is_comparable )
    TYPE(CRTM_Surface_type), INTENT(IN) :: x, y
    INTEGER,       OPTIONAL, INTENT(IN) :: n_SigFig
    LOGICAL :: is_comparable
    ! Variables
    INTEGER :: n

    ! Set up
    is_comparable = .FALSE.
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF
    
    ! Check integers
    IF ( x%Land_Type /= y%Land_Type ) RETURN

    ! Check floats
    IF ( (.NOT. Compares_Within_Tolerance(x%Land_Temperature     ,y%Land_Temperature     ,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Soil_Moisture_Content,y%Soil_Moisture_Content,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Canopy_Water_Content ,y%Canopy_Water_Content ,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Vegetation_Fraction  ,y%Vegetation_Fraction  ,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Soil_Temperature     ,y%Soil_Temperature     ,n)) ) RETURN

    ! If we get here, the structures are comparable
    is_comparable = .TRUE.
    
  END FUNCTION CRTM_LandSurface_Compare
  
  
  ELEMENTAL FUNCTION CRTM_LandSurface_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_Surface_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal
    is_equal = ( (x%Land_Type                ==     y%Land_Type            ) .AND. &
                 (x%Land_Temperature      .EqualTo. y%Land_Temperature     ) .AND. &
                 (x%Soil_Moisture_Content .EqualTo. y%Soil_Moisture_Content) .AND. &
                 (x%Canopy_Water_Content  .EqualTo. y%Canopy_Water_Content ) .AND. &
                 (x%Vegetation_Fraction   .EqualTo. y%Vegetation_Fraction  ) .AND. &
                 (x%Soil_Temperature      .EqualTo. y%Soil_Temperature     )       )
  END FUNCTION CRTM_LandSurface_Equal
  
  
! ==============================
! WATER TYPE SPECIFIC PROCEDURES
! ==============================
  ELEMENTAL SUBROUTINE CRTM_WaterSurface_Zero( Sfc )
    TYPE(CRTM_Surface_type), INTENT(IN OUT) :: Sfc
    ! Zero the water surface type data
    Sfc%Water_Temperature = ZERO
    Sfc%Wind_Speed        = ZERO
    Sfc%Wind_Direction    = ZERO
    Sfc%Salinity          = ZERO
  END SUBROUTINE CRTM_WaterSurface_Zero
  
  
  FUNCTION CRTM_WaterSurface_IsValid( Sfc ) RESULT( IsValid )
    TYPE(CRTM_Surface_type), INTENT(IN) :: Sfc
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_WaterSurface_IsValid'
    CHARACTER(ML) :: msg
    
    ! Setup
    IsValid = .TRUE.
    
    ! Check the data
    IF ( Sfc%Water_Type < 1 .OR. Sfc%Water_Type > N_VALID_WATER_TYPES ) THEN
      msg = 'Invalid Water Surface type'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( Sfc%Water_Temperature < ZERO .OR. &
         Sfc%Wind_Speed        < ZERO .OR. &
         Sfc%Wind_Direction    < ZERO .OR. &
         Sfc%Salinity          < ZERO      ) THEN
      msg = 'Invalid Water Surface data'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    
  END FUNCTION CRTM_WaterSurface_IsValid


  SUBROUTINE CRTM_WaterSurface_Inspect( Sfc )
    TYPE(CRTM_Surface_type), INTENT(IN) :: Sfc
    INTEGER :: lType
    lType = Sfc%Water_Type
    IF ( lType < 1 .OR. lType > N_VALID_WATER_TYPES ) lType = INVALID_WATER
    WRITE(*, '(3x,"Water Type       :",1x,a)') TRIM(WATER_TYPE_NAME(lType))
    WRITE(*, '(3x,"Water Temperature:",1x,es13.6)') Sfc%Water_Temperature
    WRITE(*, '(3x,"Wind Speed       :",1x,es13.6)') Sfc%Wind_Speed    
    WRITE(*, '(3x,"Wind Direction   :",1x,es13.6)') Sfc%Wind_Direction
    WRITE(*, '(3x,"Salinity         :",1x,es13.6)') Sfc%Salinity         
  END SUBROUTINE CRTM_WaterSurface_Inspect


  ELEMENTAL FUNCTION CRTM_WaterSurface_Compare( x, y, n_SigFig ) RESULT( is_comparable )
    TYPE(CRTM_Surface_type), INTENT(IN) :: x, y
    INTEGER,       OPTIONAL, INTENT(IN) :: n_SigFig
    LOGICAL :: is_comparable
    ! Variables
    INTEGER :: n

    ! Set up
    is_comparable = .FALSE.
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF
    
    ! Check integers
    IF ( x%Water_Type /= y%Water_Type ) RETURN

    ! Check floats
    IF ( (.NOT. Compares_Within_Tolerance(x%Water_Temperature,y%Water_Temperature,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Wind_Speed       ,y%Wind_Speed       ,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Wind_Direction   ,y%Wind_Direction   ,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Salinity         ,y%Salinity         ,n)) ) RETURN

    ! If we get here, the structures are comparable
    is_comparable = .TRUE.
    
  END FUNCTION CRTM_WaterSurface_Compare
  
  
  ELEMENTAL FUNCTION CRTM_WaterSurface_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_Surface_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal
    is_equal = ( (x%Water_Type           ==     y%Water_Type       ) .AND. &
                 (x%Water_Temperature .EqualTo. y%Water_Temperature) .AND. &
                 (x%Wind_Speed        .EqualTo. y%Wind_Speed       ) .AND. &
                 (x%Wind_Direction    .EqualTo. y%Wind_Direction   ) .AND. &
                 (x%Salinity          .EqualTo. y%Salinity         )       )
  END FUNCTION CRTM_WaterSurface_Equal
  
  
! =============================
! SNOW TYPE SPECIFIC PROCEDURES
! =============================
  ELEMENTAL SUBROUTINE CRTM_SnowSurface_Zero( Sfc )
    TYPE(CRTM_Surface_type), INTENT(IN OUT) :: Sfc
    ! Zero the snow surface type data
    Sfc%Snow_Temperature = ZERO
    Sfc%Snow_Depth       = ZERO
    Sfc%Snow_Density     = ZERO
    Sfc%Snow_Grain_Size  = ZERO
  END SUBROUTINE CRTM_SnowSurface_Zero


  FUNCTION CRTM_SnowSurface_IsValid( Sfc ) RESULT( IsValid )
    TYPE(CRTM_Surface_type), INTENT(IN) :: Sfc
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SnowSurface_IsValid'
    CHARACTER(ML) :: msg
    
    ! Setup
    IsValid = .TRUE.
    
    ! Check the data
    IF ( Sfc%Snow_Type < 1 .OR. Sfc%Snow_Type > N_VALID_Snow_TYPES ) THEN
      msg = 'Invalid Snow Surface type'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( Sfc%Snow_Temperature < ZERO .OR. &
         Sfc%Snow_Depth       < ZERO .OR. &
         Sfc%Snow_Density     < ZERO .OR. &
         Sfc%Snow_Grain_Size  < ZERO      ) THEN
      msg = 'Invalid Snow Surface data'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    
  END FUNCTION CRTM_SnowSurface_IsValid


  SUBROUTINE CRTM_SnowSurface_Inspect( Sfc )
    TYPE(CRTM_Surface_type), INTENT(IN) :: Sfc
    INTEGER :: lType
    lType = Sfc%Snow_Type
    IF ( lType < 1 .OR. lType > N_VALID_SNOW_TYPES ) lType = INVALID_SNOW
    WRITE(*, '(3x,"Snow Type       :",1x,a)') TRIM(SNOW_TYPE_NAME(lType))
    WRITE(*, '(3x,"Snow Temperature:",1x,es13.6)') Sfc%Snow_Temperature
    WRITE(*, '(3x,"Snow Depth      :",1x,es13.6)') Sfc%Snow_Depth      
    WRITE(*, '(3x,"Snow Density    :",1x,es13.6)') Sfc%Snow_Density    
    WRITE(*, '(3x,"Snow Grain_Size :",1x,es13.6)') Sfc%Snow_Grain_Size 
  END SUBROUTINE CRTM_SnowSurface_Inspect


  ELEMENTAL FUNCTION CRTM_SnowSurface_Compare( x, y, n_SigFig ) RESULT( is_comparable )
    TYPE(CRTM_Surface_type), INTENT(IN) :: x, y
    INTEGER,       OPTIONAL, INTENT(IN) :: n_SigFig
    LOGICAL :: is_comparable
    ! Variables
    INTEGER :: n

    ! Set up
    is_comparable = .FALSE.
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF
    
    ! Check integers
    IF ( x%Snow_Type /= y%Snow_Type ) RETURN

    ! Check floats
    IF ( (.NOT. Compares_Within_Tolerance(x%Snow_Temperature,y%Snow_Temperature,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Snow_Depth      ,y%Snow_Depth      ,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Snow_Density    ,y%Snow_Density    ,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Snow_Grain_Size ,y%Snow_Grain_Size ,n)) ) RETURN

    ! If we get here, the structures are comparable
    is_comparable = .TRUE.
    
  END FUNCTION CRTM_SnowSurface_Compare
  
  
  ELEMENTAL FUNCTION CRTM_SnowSurface_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_Surface_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal
    is_equal = ( (x%Snow_Type           ==     y%Snow_Type       ) .AND. &
                 (x%Snow_Temperature .EqualTo. y%Snow_Temperature) .AND. &
                 (x%Snow_Depth       .EqualTo. y%Snow_Depth      ) .AND. &
                 (x%Snow_Density     .EqualTo. y%Snow_Density    ) .AND. &
                 (x%Snow_Grain_Size  .EqualTo. y%Snow_Grain_Size )       )
  END FUNCTION CRTM_SnowSurface_Equal
  
  
! ============================
! ICE TYPE SPECIFIC PROCEDURES
! ============================
  ELEMENTAL SUBROUTINE CRTM_IceSurface_Zero( Sfc )
    TYPE(CRTM_Surface_type), INTENT(IN OUT) :: Sfc
    ! Zero the ice surface type data
    Sfc%Ice_Temperature = ZERO
    Sfc%Ice_Thickness   = ZERO
    Sfc%Ice_Density     = ZERO
    Sfc%Ice_Roughness   = ZERO
  END SUBROUTINE CRTM_IceSurface_Zero
  
  
  FUNCTION CRTM_IceSurface_IsValid( Sfc ) RESULT( IsValid )
    TYPE(CRTM_Surface_type), INTENT(IN) :: Sfc
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_IceSurface_IsValid'
    CHARACTER(ML) :: msg
    
    ! Setup
    IsValid = .TRUE.
    
    ! Check the data
    IF ( Sfc%Ice_Type < 1 .OR. Sfc%Ice_Type > N_VALID_Ice_TYPES ) THEN
      msg = 'Invalid Ice Surface type'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( Sfc%Ice_Temperature < ZERO .OR. &
         Sfc%Ice_Thickness   < ZERO .OR. &
         Sfc%Ice_Density     < ZERO .OR. &
         Sfc%Ice_Roughness   < ZERO      ) THEN
      msg = 'Invalid Ice Surface data'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    
  END FUNCTION CRTM_IceSurface_IsValid


  SUBROUTINE CRTM_IceSurface_Inspect( Sfc )
    TYPE(CRTM_Surface_type), INTENT(IN) :: Sfc
    INTEGER :: lType
    lType = Sfc%Ice_Type
    IF ( lType < 1 .OR. lType > N_VALID_ICE_TYPES ) lType = INVALID_ICE
    WRITE(*, '(3x,"Ice Type       :",1x,a)') TRIM(ICE_TYPE_NAME(lType))
    WRITE(*, '(3x,"Ice Temperature:",1x,es13.6)') Sfc%Ice_Temperature
    WRITE(*, '(3x,"Ice Thickness  :",1x,es13.6)') Sfc%Ice_Thickness  
    WRITE(*, '(3x,"Ice Density    :",1x,es13.6)') Sfc%Ice_Density    
    WRITE(*, '(3x,"Ice Roughness  :",1x,es13.6)') Sfc%Ice_Roughness  
  END SUBROUTINE CRTM_IceSurface_Inspect


  ELEMENTAL FUNCTION CRTM_IceSurface_Compare( x, y, n_SigFig ) RESULT( is_comparable )
    TYPE(CRTM_Surface_type), INTENT(IN) :: x, y
    INTEGER,       OPTIONAL, INTENT(IN) :: n_SigFig
    LOGICAL :: is_comparable
    ! Variables
    INTEGER :: n

    ! Set up
    is_comparable = .FALSE.
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF
    
    ! Check integers
    IF ( x%Ice_Type /= y%Ice_Type ) RETURN

    ! Check floats
    IF ( (.NOT. Compares_Within_Tolerance(x%Ice_Temperature,y%Ice_Temperature,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Ice_Thickness  ,y%Ice_Thickness  ,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Ice_Density    ,y%Ice_Density    ,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Ice_Roughness  ,y%Ice_Roughness  ,n)) ) RETURN

    ! If we get here, the structures are comparable
    is_comparable = .TRUE.
    
  END FUNCTION CRTM_IceSurface_Compare
  
  
  ELEMENTAL FUNCTION CRTM_IceSurface_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_Surface_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal
    is_equal = ( (x%Ice_Type           ==     y%Ice_Type       ) .AND. &
                 (x%Ice_Temperature .EqualTo. y%Ice_Temperature) .AND. &
                 (x%Ice_Thickness   .EqualTo. y%Ice_Thickness  ) .AND. &
                 (x%Ice_Density     .EqualTo. y%Ice_Density    ) .AND. &
                 (x%Ice_Roughness   .EqualTo. y%Ice_Roughness  )       )
  END FUNCTION CRTM_IceSurface_Equal
  
END MODULE CRTM_Surface_Define
