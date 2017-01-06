!
! SpcCoeff_Define
!
! Module defining the SpcCoeff data structure and routines
! to manipulate them.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 18-Mar-2002
!                       paul.vandelst@noaa.gov
!

MODULE SpcCoeff_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,            ONLY: Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE Subset_Define        , ONLY: Subset_type      , &
                                   Subset_Associated, &
                                   Subset_GetValue  , &
                                   Subset_Generate
  USE SensorInfo_Parameters, ONLY: INVALID_WMO_SATELLITE_ID, &
                                   INVALID_WMO_SENSOR_ID   , &
                                   N_SENSOR_TYPES          , &
                                   INVALID_SENSOR          , &
                                   MICROWAVE_SENSOR        , &
                                   INFRARED_SENSOR         , &
                                   VISIBLE_SENSOR          , &
                                   ULTRAVIOLET_SENSOR      , &
                                   SENSOR_TYPE_NAME        , &
                                   N_POLARIZATION_TYPES    , &
                                   INVALID_POLARIZATION    , &
                                   UNPOLARIZED             , &
                                   INTENSITY               , &
                                   FIRST_STOKES_COMPONENT  , &
                                   SECOND_STOKES_COMPONENT , &
                                   THIRD_STOKES_COMPONENT  , &
                                   FOURTH_STOKES_COMPONENT , &
                                   VL_POLARIZATION         , &
                                   HL_POLARIZATION         , &
                                   plus45L_POLARIZATION    , &
                                   minus45L_POLARIZATION   , &
                                   VL_MIXED_POLARIZATION   , &
                                   HL_MIXED_POLARIZATION   , &
                                   RC_POLARIZATION         , &
                                   LC_POLARIZATION         , &
                                   POLARIZATION_TYPE_NAME
  USE ACCoeff_Define       , ONLY: ACCoeff_type          , &
                                   OPERATOR(==)          , &
                                   ACCoeff_Associated    , &
                                   ACCoeff_Destroy       , &
                                   ACCoeff_Create        , &
                                   ACCoeff_Inspect       , &
                                   ACCoeff_ValidRelease  , &
                                   ACCoeff_Info          , &
                                   ACCoeff_DefineVersion , &
                                   ACCoeff_Subset        , &
                                   ACCoeff_Concat        , &
                                   ACCoeff_ChannelReindex
  USE NLTECoeff_Define     , ONLY: NLTECoeff_type          , &
                                   OPERATOR(==)            , &
                                   NLTECoeff_Associated    , &
                                   NLTECoeff_Destroy       , &
                                   NLTECoeff_Create        , &
                                   NLTECoeff_Inspect       , &
                                   NLTECoeff_ValidRelease  , &
                                   NLTECoeff_Info          , &
                                   NLTECoeff_DefineVersion , &
                                   NLTECoeff_Subset        , &
                                   NLTECoeff_Concat        , &
                                   NLTECoeff_ChannelReindex
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: SpcCoeff_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: SpcCoeff_Associated
  PUBLIC :: SpcCoeff_Destroy
  PUBLIC :: SpcCoeff_Create
  PUBLIC :: SpcCoeff_Inspect
  PUBLIC :: SpcCoeff_ValidRelease
  PUBLIC :: SpcCoeff_Info
  PUBLIC :: SpcCoeff_DefineVersion
  PUBLIC :: SpcCoeff_Subset
  PUBLIC :: SpcCoeff_Concat
  ! ...Channel flag specific procedures
  PUBLIC :: SpcCoeff_ClearAllFlags
  PUBLIC :: SpcCoeff_IsSolar , SpcCoeff_SetSolar , SpcCoeff_ClearSolar
  PUBLIC :: SpcCoeff_IsZeeman, SpcCoeff_SetZeeman, SpcCoeff_ClearZeeman
  ! ...Sensor specific procedures
  PUBLIC :: SpcCoeff_IsMicrowaveSensor  , SpcCoeff_SetMicrowaveSensor
  PUBLIC :: SpcCoeff_IsInfraredSensor   , SpcCoeff_SetInfraredSensor
  PUBLIC :: SpcCoeff_IsVisibleSensor    , SpcCoeff_SetVisibleSensor
  PUBLIC :: SpcCoeff_IsUltravioletSensor, SpcCoeff_SetUltravioletSensor
  ! ...Inherited procedures
  PUBLIC :: ACCoeff_Associated
  PUBLIC :: ACCoeff_Destroy
  PUBLIC :: ACCoeff_Create
  PUBLIC :: ACCoeff_Inspect
  PUBLIC :: ACCoeff_ValidRelease
  PUBLIC :: ACCoeff_Info
  PUBLIC :: ACCoeff_DefineVersion
  PUBLIC :: NLTECoeff_Associated    
  PUBLIC :: NLTECoeff_Destroy       
  PUBLIC :: NLTECoeff_Create        
  PUBLIC :: NLTECoeff_Inspect       
  PUBLIC :: NLTECoeff_ValidRelease  
  PUBLIC :: NLTECoeff_Info          
  PUBLIC :: NLTECoeff_DefineVersion


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE SpcCoeff_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: SpcCoeff_Define.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512
  ! Sensor id string length
  INTEGER, PARAMETER :: SL = 20
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: SPCCOEFF_RELEASE = 8  ! This determines structure and file formats.
  INTEGER, PARAMETER :: SPCCOEFF_VERSION = 1  ! This is just the data version.
  ! The bit positions for the various channel flags
  INTEGER, PARAMETER :: SOLAR_FLAG  = 0
  INTEGER, PARAMETER :: ZEEMAN_FLAG = 1
  


  ! -----------------------------
  ! SpcCoeff data type definition
  ! -----------------------------
  TYPE :: SpcCoeff_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = SPCCOEFF_RELEASE
    INTEGER(Long) :: Version = SPCCOEFF_VERSION
    ! Dimensions
    INTEGER(Long) :: n_Channels = 0  ! L dimension
    ! Sensor info
    CHARACTER(SL) :: Sensor_Id        = ''
    INTEGER(Long) :: Sensor_Type      = INVALID_SENSOR
    INTEGER(Long) :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER(Long) :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    ! Channel data arrays
    INTEGER(Long), ALLOCATABLE :: Sensor_Channel(:)              ! L
    INTEGER(Long), ALLOCATABLE :: Polarization(:)                ! L
    INTEGER(Long), ALLOCATABLE :: Channel_Flag(:)                ! L
    REAL(Double) , ALLOCATABLE :: Frequency(:)                   ! L
    REAL(Double) , ALLOCATABLE :: Wavenumber(:)                  ! L
    REAL(Double) , ALLOCATABLE :: Planck_C1(:)                   ! L
    REAL(Double) , ALLOCATABLE :: Planck_C2(:)                   ! L
    REAL(Double) , ALLOCATABLE :: Band_C1(:)                     ! L
    REAL(Double) , ALLOCATABLE :: Band_C2(:)                     ! L
    REAL(Double) , ALLOCATABLE :: Cosmic_Background_Radiance(:)  ! L
    REAL(Double) , ALLOCATABLE :: Solar_Irradiance(:)            ! L
    ! Derived type components
    TYPE(ACCoeff_type)   :: AC  ! Antenna correction coefficients
    TYPE(NLTECoeff_type) :: NC  ! non-LTE correction coefficients
  END TYPE SpcCoeff_type


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
!       SpcCoeff_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the SpcCoeff structure.
!
! CALLING SEQUENCE:
!       Status = SpcCoeff_Associated( SpcCoeff )
!
! OBJECTS:
!       SpcCoeff:   Structure which is to have its member's
!                   status tested.
!                   UNITS:      N/A
!                   TYPE:       SpcCoeff_type
!                   DIMENSION:  Scalar or any rank
!                   ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:     The return value is a logical value indicating the
!                   status of the SpcCoeff members.
!                    .TRUE.  - if ANY of the SpcCoeff allocatable members
!                              are in use.
!                    .FALSE. - if ALL of the SpcCoeff allocatable members
!                              are not in use.
!                   UNITS:      N/A
!                   TYPE:       LOGICAL
!                   DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION SpcCoeff_Associated( SpcCoeff ) RESULT( Status )
    TYPE(SpcCoeff_type), INTENT(IN) :: SpcCoeff
    LOGICAL :: Status
    Status = SpcCoeff%Is_Allocated
  END FUNCTION SpcCoeff_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize SpcCoeff objects.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_Destroy( SpcCoeff )
!
! OBJECTS:
!       SpcCoeff:      Re-initialized SpcCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       SpcCoeff_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SpcCoeff_Destroy( SpcCoeff )
    TYPE(SpcCoeff_type), INTENT(OUT) :: SpcCoeff
    SpcCoeff%Is_Allocated = .FALSE.
    SpcCoeff%n_Channels        = 0
    SpcCoeff%Sensor_Id         = ''
    SpcCoeff%WMO_Satellite_ID  = INVALID_WMO_SATELLITE_ID
    SpcCoeff%WMO_Sensor_ID     = INVALID_WMO_SENSOR_ID
    SpcCoeff%Sensor_Type       = INVALID_SENSOR
  END SUBROUTINE SpcCoeff_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of an SpcCoeff object.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_Create( SpcCoeff  , &
!                             n_Channels  )         
!
! OBJECTS:
!       SpcCoeff:           SpcCoeff object structure.
!                           UNITS:      N/A
!                           TYPE:       SpcCoeff_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Channels:         Number of sensor channels.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SpcCoeff_Create( &
    SpcCoeff  , &  ! Output
    n_Channels  )  ! Input
    ! Arguments
    TYPE(SpcCoeff_type), INTENT(OUT) :: SpcCoeff
    INTEGER            , INTENT(IN)  :: n_Channels
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Channels < 1 ) RETURN
    
    ! Perform the allocation
    ALLOCATE( SpcCoeff%Sensor_Channel( n_Channels ),             &
              SpcCoeff%Polarization( n_Channels ),               &
              SpcCoeff%Channel_Flag( n_Channels ),               &
              SpcCoeff%Frequency( n_Channels ),                  &
              SpcCoeff%Wavenumber( n_Channels ),                 &
              SpcCoeff%Planck_C1( n_Channels ),                  &
              SpcCoeff%Planck_C2( n_Channels ),                  &
              SpcCoeff%Band_C1( n_Channels ),                    &
              SpcCoeff%Band_C2( n_Channels ),                    &
              SpcCoeff%Cosmic_Background_Radiance( n_Channels ), &
              SpcCoeff%Solar_Irradiance( n_Channels ),           &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    SpcCoeff%n_Channels = n_Channels
    ! ...Arrays
    SpcCoeff%Sensor_Channel             = 0
    SpcCoeff%Polarization               = INVALID_POLARIZATION
    SpcCoeff%Channel_Flag               = 0
    SpcCoeff%Frequency                  = ZERO
    SpcCoeff%Wavenumber                 = ZERO
    SpcCoeff%Planck_C1                  = ZERO
    SpcCoeff%Planck_C2                  = ZERO
    SpcCoeff%Band_C1                    = ZERO
    SpcCoeff%Band_C2                    = ZERO
    SpcCoeff%Cosmic_Background_Radiance = ZERO
    SpcCoeff%Solar_Irradiance           = ZERO


    ! Set allocation indicator
    SpcCoeff%Is_Allocated = .TRUE.

  END SUBROUTINE SpcCoeff_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a SpcCoeff object to stdout.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_Inspect( SpcCoeff )
!
! OBJECTS:
!       SpcCoeff:      SpcCoeff object to display.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE SpcCoeff_Inspect( SpcCoeff )
    TYPE(SpcCoeff_type), INTENT(IN) :: SpcCoeff
    INTEGER :: n
    WRITE(*,'(1x,"SpcCoeff OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version  :",1x,i0,".",i0)') SpcCoeff%Release, SpcCoeff%Version
    ! Dimensions
    WRITE(*,'(3x,"n_Channels       :",1x,i0)') SpcCoeff%n_Channels
    IF ( .NOT. SpcCoeff_Associated(SpcCoeff) ) RETURN
    ! Sensor info
    WRITE(*,'(3x,"Sensor_Id        :",1x,a )') TRIM(SpcCoeff%Sensor_Id)
    WRITE(*,'(3x,"WMO_Satellite_ID :",1x,i0)') SpcCoeff%WMO_Satellite_ID 
    WRITE(*,'(3x,"WMO_Sensor_ID    :",1x,i0)') SpcCoeff%WMO_Sensor_ID
    WRITE(*,'(3x,"Sensor_Type      :",1x,a )') TRIM(SENSOR_TYPE_NAME(SpcCoeff%Sensor_Type))
    WRITE(*,'(3x,"Sensor_Channel   :")')
    WRITE(*,'(10(1x,i5,:))') SpcCoeff%Sensor_Channel
    ! Data arrays
    IF ( SpcCoeff_IsMicrowaveSensor(SpcCoeff) ) THEN
      WRITE(*,'(3x,"Polarization               :")')
      DO n = 1, SpcCoeff%n_Channels
        WRITE(*,'(5x,"Channel ",i0,": ",a)') SpcCoeff%Sensor_Channel(n), &
                                             POLARIZATION_TYPE_NAME(SpcCoeff%Polarization(n))
      END DO
    END IF
    WRITE(*,'(3x,"Channel_Flag               :")')
    WRITE(*,'(3(1x,b32.32,:))') SpcCoeff%Channel_Flag
    WRITE(*,'(3x,"Frequency                  :")')
    WRITE(*,'(5(1x,es13.6,:))') SpcCoeff%Frequency                 
    WRITE(*,'(3x,"Wavenumber                 :")')
    WRITE(*,'(5(1x,es13.6,:))') SpcCoeff%Wavenumber                
    WRITE(*,'(3x,"Planck_C1                  :")')
    WRITE(*,'(5(1x,es13.6,:))') SpcCoeff%Planck_C1                 
    WRITE(*,'(3x,"Planck_C2                  :")')
    WRITE(*,'(5(1x,es13.6,:))') SpcCoeff%Planck_C2                 
    WRITE(*,'(3x,"Band_C1                    :")')
    WRITE(*,'(5(1x,es13.6,:))') SpcCoeff%Band_C1                   
    WRITE(*,'(3x,"Band_C2                    :")')
    WRITE(*,'(5(1x,es13.6,:))') SpcCoeff%Band_C2                   
    WRITE(*,'(3x,"Cosmic_Background_Radiance :")')
    WRITE(*,'(5(1x,es13.6,:))') SpcCoeff%Cosmic_Background_Radiance
    WRITE(*,'(3x,"Solar_Irradiance           :")')
    WRITE(*,'(5(1x,es13.6,:))') SpcCoeff%Solar_Irradiance          
    ! Derived types
    IF ( ACCoeff_Associated(   SpcCoeff%AC ) ) CALL ACCoeff_Inspect(   SpcCoeff%AC )
    IF ( NLTECoeff_Associated( SpcCoeff%NC ) ) CALL NLTECoeff_Inspect( SpcCoeff%NC )
          
  END SUBROUTINE SpcCoeff_Inspect
  

!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_ValidRelease
!
! PURPOSE:
!       Function to check the SpcCoeff Release value.
!
! CALLING SEQUENCE:
!       IsValid = SpcCoeff_ValidRelease( SpcCoeff )
!
! INPUTS:
!       SpcCoeff:      SpcCoeff object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:       Logical value defining the release validity.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION SpcCoeff_ValidRelease( SpcCoeff ) RESULT( IsValid )
    ! Arguments
    TYPE(SpcCoeff_type), INTENT(IN) :: SpcCoeff
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SpcCoeff_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( SpcCoeff%Release < SPCCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An SpcCoeff data update is needed. ", &
                  &"SpcCoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  SpcCoeff%Release, SPCCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( SpcCoeff%Release > SPCCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An SpcCoeff software update is needed. ", &
                  &"SpcCoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  SpcCoeff%Release, SPCCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION SpcCoeff_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a SpcCoeff object.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_Info( SpcCoeff, Info, NoComponents=NoComponents )
!
! OBJECTS:
!       SpcCoeff:      SpcCoeff object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the SpcCoeff object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       NoComponents:  Set this logical argument to not include the version
!                      and dimension information of structure components.
!                      If .FALSE. the substructure information is included [DEFAULT]
!                         .TRUE.  the substructure information is NOT included
!                      If not specfied the default is .FALSE.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE SpcCoeff_Info( &
    SpcCoeff    , &  ! Input
    Info        , &  ! Output
    NoComponents  )  ! Optional input
    ! Arguments
    TYPE(SpcCoeff_type), INTENT(IN)  :: SpcCoeff
    CHARACTER(*),        INTENT(OUT) :: Info
    LOGICAL,   OPTIONAL, INTENT(IN)  :: NoComponents
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    LOGICAL :: IncludeComponents
    CHARACTER(5000) :: Long_String
    CHARACTER(2000) :: AC_Info, NC_Info

    ! Setup
    IncludeComponents = .TRUE.
    IF ( PRESENT(NoComponents) ) IncludeComponents = .NOT. NoComponents
    
    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"SpcCoeff RELEASE.VERSION: ",i2,".",i2.2,a,3x, &
           &"N_CHANNELS=",i0 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           SpcCoeff%Release, SpcCoeff%Version, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           SpcCoeff%n_Channels

    ! Add derived type info strings
    IF ( IncludeComponents ) THEN
      ! ...Antenna correction structure
      IF ( ACCoeff_Associated( SpcCoeff%AC ) ) THEN
        CALL ACCoeff_Info( SpcCoeff%AC, AC_Info )
        Long_String = TRIM(Long_String)//TRIM(AC_Info)
      END IF
      ! ...non-LTE correction structure
      IF ( NLTECoeff_Associated( SpcCoeff%NC ) ) THEN
        CALL NLTECoeff_Info( SpcCoeff%NC, NC_Info )
        Long_String = TRIM(Long_String)//TRIM(NC_Info)
      END IF
    END IF
    
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE SpcCoeff_Info


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_DefineVersion
!
! PURPOSE:
!       Subroutine to return the version information for the
!       definition module(s).
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_DefineVersion( Id )
!
! OUTPUTS:
!       Id:     Character string containing the version Id information for the
!               structure definition module(s). If the string length is
!               sufficient, the version information for all the modules (this,
!               and those for the derived type components) are concatenated.
!               Otherwise only the version id for this module is returned.
!               UNITS:      N/A
!               TYPE:       CHARACTER(*)
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE SpcCoeff_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    INTEGER, PARAMETER :: SL = 256
    CHARACTER(SL)   :: AC_Id
    CHARACTER(SL)   :: NC_Id
    CHARACTER(SL*3) :: Define_Id
    CALL ACCoeff_DefineVersion( AC_Id )
    CALL NLTECoeff_DefineVersion( NC_Id )
    Define_Id = MODULE_VERSION_ID//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
                '  '//TRIM(AC_Id)//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
                '  '//TRIM(NC_Id)
    IF ( LEN_TRIM(Define_Id) <= LEN(Id) ) THEN
      Id = Define_Id
    ELSE
      Id = MODULE_VERSION_ID
    END IF
  END SUBROUTINE SpcCoeff_DefineVersion


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_Subset
!
! PURPOSE:
!       Subroutine to return a channel subset of the input SpcCoeff object.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_Subset( SpcCoeff, Subset, SC_Subset )
!
! OBJECTS:
!       SpcCoeff:     SpcCoeff object which is to be subsetted.
!                     UNITS:      N/A
!                     TYPE:       SpcCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Subset:       Subset object containing the list of indices
!                     corresponding the channels to be extracted.
!                     UNITS:      N/A
!                     TYPE:       Subset_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SC_Subset:    SpcCoeff object containing the requested channel subset
!                     of the input SpcCoeff data.
!                     UNITS:      N/A
!                     TYPE:       SpcCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE SpcCoeff_Subset( &
    SpcCoeff      , &  ! Input
    Sensor_Channel, &  ! Input
    SC_Subset       )  ! Output
    ! Arguments
    TYPE(SpcCoeff_type), INTENT(IN)  :: SpcCoeff
    INTEGER            , INTENT(IN)  :: Sensor_Channel(:)
    TYPE(SpcCoeff_type), INTENT(OUT) :: SC_Subset
    ! Local variables
    TYPE(Subset_type) :: subset
    INTEGER :: n_subset_channels
    INTEGER, ALLOCATABLE :: idx(:)
    
    ! Check input is valid
    IF ( .NOT. SpcCoeff_Associated(SpcCoeff) ) RETURN
    
    ! Generate the subset list
    CALL Subset_Generate( &
           subset, &
           SpcCoeff%Sensor_Channel, &
           Sensor_Channel )
    IF ( .NOT. Subset_Associated( subset ) ) RETURN
    
    
    ! Allocate the output subset SpcCoeff object
    CALL Subset_GetValue( subset, n_Values = n_subset_channels, Index = idx )
    CALL SpcCoeff_Create( SC_Subset, n_subset_channels )
    IF ( .NOT. SpcCoeff_Associated(SC_Subset) ) RETURN


    ! Extract out the subset channels
    ! ...First assign some scalars
    SC_Subset%Version          = SpcCoeff%Version
    SC_Subset%Sensor_Id        = SpcCoeff%Sensor_Id       
    SC_Subset%Sensor_Type      = SpcCoeff%Sensor_Type     
    SC_Subset%WMO_Satellite_ID = SpcCoeff%WMO_Satellite_ID
    SC_Subset%WMO_Sensor_ID    = SpcCoeff%WMO_Sensor_ID   
    ! ...and now extract the subset
    SC_Subset%Sensor_Channel             = SpcCoeff%Sensor_Channel(idx)
    SC_Subset%Polarization               = SpcCoeff%Polarization(idx)
    SC_Subset%Channel_Flag               = SpcCoeff%Channel_Flag(idx)
    SC_Subset%Frequency                  = SpcCoeff%Frequency(idx)
    SC_Subset%Wavenumber                 = SpcCoeff%Wavenumber(idx)
    SC_Subset%Planck_C1                  = SpcCoeff%Planck_C1(idx)
    SC_Subset%Planck_C2                  = SpcCoeff%Planck_C2(idx)
    SC_Subset%Band_C1                    = SpcCoeff%Band_C1(idx)
    SC_Subset%Band_C2                    = SpcCoeff%Band_C2(idx)
    SC_Subset%Cosmic_Background_Radiance = SpcCoeff%Cosmic_Background_Radiance(idx)
    SC_Subset%Solar_Irradiance           = SpcCoeff%Solar_Irradiance(idx)


    ! Operate on the components
    ! ...Antenna correction coefficients
    IF ( ACCoeff_Associated( SpcCoeff%AC ) ) &
      CALL ACCoeff_Subset( SpcCoeff%AC, Sensor_Channel, SC_Subset%AC )
    ! ...NLTE correction coefficients
    IF ( NLTECoeff_Associated( SpcCoeff%NC ) ) &
      CALL NLTECoeff_Subset( SpcCoeff%NC, Sensor_Channel, SC_Subset%NC )

  END SUBROUTINE SpcCoeff_Subset


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_Concat
!
! PURPOSE:
!       Subroutine to concatenate multiple SpcCoeff objects along the channel
!       dimension into a single SpcCoeff object.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_Concat( SpcCoeff, SC_Array, Sensor_Id=Sensor_Id )
!
! OBJECTS:
!       SpcCoeff:     SpcCoeff object containing the concatenated result.
!                     UNITS:      N/A
!                     TYPE:       SpcCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       SC_Array:     Array of SpcCoeff objects to be concatenated.
!                     UNITS:      N/A
!                     TYPE:       SpcCoeff_type
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Sensor_Id:    Sensor id character to string to use for the concatenated
!                     result. If not specified, the sensor id of the first valid
!                     element of SC_Array is used.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE SpcCoeff_Concat( &
    SpcCoeff , &  ! Output
    SC_Array , &  ! Input
    Sensor_Id  )  ! Optional input
    ! Arguments
    TYPE(SpcCoeff_type)   , INTENT(OUT) :: SpcCoeff
    TYPE(SpcCoeff_type)   , INTENT(IN)  :: SC_Array(:)
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Id
    ! Local variables
    INTEGER, ALLOCATABLE :: valid_index(:)
    INTEGER :: i, j, n_sc, n_valid, n_channels
    INTEGER :: ch1, ch2
    
    ! Set up
    ! ...Check input is valid
    n_sc = SIZE(SC_Array)
    IF ( n_sc < 1 ) RETURN          ! Zero-sized array
    ! ...Count valid input
    n_valid = COUNT(SpcCoeff_Associated(SC_Array))
    IF ( n_valid == 0 ) RETURN      ! All elements unallocated
    ! ...Index the valid input
    ALLOCATE( valid_index(n_valid) )
    valid_index = PACK( (/(i,i=1,n_sc)/), MASK=SpcCoeff_Associated(SC_Array) )
    ! ...Check non-channel dimensions and ids
    DO j = 1, n_valid
      i = valid_index(j)
      IF ( SC_Array(i)%Sensor_Type      /= SC_Array(valid_index(1))%Sensor_Type      .OR. & 
           SC_Array(i)%WMO_Satellite_ID /= SC_Array(valid_index(1))%WMO_Satellite_ID .OR. &
           SC_Array(i)%WMO_Sensor_ID    /= SC_Array(valid_index(1))%WMO_Sensor_ID         ) THEN
        RETURN
      END IF
    END DO


    ! Sum channel dimensions
    n_channels = SUM(SC_Array%n_Channels)
    
    
    ! Allocate the output concatenated SpcCoeff object
    CALL SpcCoeff_Create( SpcCoeff, n_channels )
    IF ( .NOT. SpcCoeff_Associated(SpcCoeff) ) RETURN


    ! Concatenate the channel data
    ! ...First assign the non-channel dependent data
    SpcCoeff%Version = SC_Array(valid_index(1))%Version
    IF ( PRESENT(Sensor_Id) ) THEN
      SpcCoeff%Sensor_Id = ADJUSTL(Sensor_Id)
    ELSE
      SpcCoeff%Sensor_Id = SC_Array(valid_index(1))%Sensor_Id
    END IF
    SpcCoeff%Sensor_Type      = SC_Array(valid_index(1))%Sensor_Type     
    SpcCoeff%WMO_Satellite_ID = SC_Array(valid_index(1))%WMO_Satellite_ID
    SpcCoeff%WMO_Sensor_ID    = SC_Array(valid_index(1))%WMO_Sensor_ID   
    ! ...and now concatenate the channel data
    ch1 = 1
    DO j = 1, n_valid
      i = valid_index(j)
      
      ch2 = ch1 + SC_Array(i)%n_Channels - 1
      
      SpcCoeff%Sensor_Channel(ch1:ch2)             = SC_Array(i)%Sensor_Channel            
      SpcCoeff%Polarization(ch1:ch2)               = SC_Array(i)%Polarization 
      SpcCoeff%Channel_Flag(ch1:ch2)               = SC_Array(i)%Channel_Flag 
      SpcCoeff%Frequency(ch1:ch2)                  = SC_Array(i)%Frequency    
      SpcCoeff%Wavenumber(ch1:ch2)                 = SC_Array(i)%Wavenumber   
      SpcCoeff%Planck_C1(ch1:ch2)                  = SC_Array(i)%Planck_C1    
      SpcCoeff%Planck_C2(ch1:ch2)                  = SC_Array(i)%Planck_C2    
      SpcCoeff%Band_C1(ch1:ch2)                    = SC_Array(i)%Band_C1      
      SpcCoeff%Band_C2(ch1:ch2)                    = SC_Array(i)%Band_C2      
      SpcCoeff%Cosmic_Background_Radiance(ch1:ch2) = SC_Array(i)%Cosmic_Background_Radiance
      SpcCoeff%Solar_Irradiance(ch1:ch2)           = SC_Array(i)%Solar_Irradiance          
      
      ch1 = ch2 + 1
    END DO


    ! Operate on the components
    ! ...Antenna correction coefficients
    CALL ACCoeff_Concat( SpcCoeff%AC, SC_Array%AC, Sensor_Id = Sensor_Id )
    CALL ACCoeff_ChannelReindex( SpcCoeff%AC, SpcCoeff%Sensor_Channel )
    ! ...NLTE correction coefficients
    CALL NLTECoeff_Concat( SpcCoeff%NC, SC_Array%NC, Sensor_Id = Sensor_Id )
    CALL NLTECoeff_ChannelReindex( SpcCoeff%NC, SpcCoeff%Sensor_Channel )


    ! Cleanup
    DEALLOCATE( valid_index )

  END SUBROUTINE SpcCoeff_Concat


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_ClearAllFlags
!
! PURPOSE:
!       Elemental subroutine to clear ALL SpcCoeff channel flags.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_ClearAllFlags( SpcCoeff, ChannelIndex=ChannelIndex )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be altered.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       ChannelIndex:  Set this to the index corresponding to a particular
!                      channel in the SpcCoeff object for which all the
!                      flags are to be cleared.
!                      If not specified, all the channels cleared.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Conformable with SpcCoeff input
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SpcCoeff_ClearAllFlags( SpcCoeff, ChannelIndex )
    TYPE(SpcCoeff_type), INTENT(IN OUT) :: SpcCoeff
    INTEGER,   OPTIONAL, INTENT(IN)     :: ChannelIndex
    INTEGER :: n
    IF ( .NOT. SpcCoeff_Associated(SpcCoeff) ) RETURN
    DO n = 0, BIT_SIZE(0_Long)
      CALL SpcCoeff_ClearFlag( SpcCoeff, n, ChannelIndex=ChannelIndex )
    END DO
  END SUBROUTINE SpcCoeff_ClearAllFlags
  

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
! NOTE: The following flag check, set, and clear procedures were generated
!       automatically using the
!         gen_flag_procedures.rb
!       script. Modify at your own risk!
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_IsSolar
!
! PURPOSE:
!       Elemental function to test if SpcCoeff channels are flagged as being
!       solar sensitive.
!
! CALLING SEQUENCE:
!       Status = SpcCoeff_IsSolar( SpcCoeff, ChannelIndex=ChannelIndex )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be tested.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       ChannelIndex:  Set this to the index corresponding to a particular
!                      channel in the SpcCoeff object to test if it is a
!                      solar sensitive channel.
!                      If not specified, all the channels are tested.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Conformable with SpcCoeff input
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value.
!                       .TRUE.  - The channel(s) is(are) solar sensitive.
!                       .FALSE. - The channel(s) is(are) NOT solar sensitive.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as SpcCoeff input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION SpcCoeff_IsSolar(SpcCoeff, ChannelIndex) RESULT(Is_Set)
    TYPE(SpcCoeff_type), INTENT(IN) :: SpcCoeff
    INTEGER,   OPTIONAL, INTENT(IN) :: ChannelIndex
    LOGICAL :: Is_Set
    Is_Set = .FALSE.
    IF ( .NOT. SpcCoeff_Associated(SpcCoeff) ) RETURN
    Is_Set = SpcCoeff_IsFlagSet(SpcCoeff, SOLAR_FLAG, ChannelIndex=ChannelIndex)
  END FUNCTION SpcCoeff_IsSolar
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_IsZeeman
!
! PURPOSE:
!       Elemental function to test if SpcCoeff channels are flagged as being
!       Zeeman affected.
!
! CALLING SEQUENCE:
!       Status = SpcCoeff_IsZeeman( SpcCoeff, ChannelIndex=ChannelIndex )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be tested.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       ChannelIndex:  Set this to the index corresponding to a particular
!                      channel in the SpcCoeff object to test if it is a
!                      Zeeman affected channel.
!                      If not specified, all the channels are tested.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Conformable with SpcCoeff input
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value.
!                       .TRUE.  - The channel(s) is(are) Zeeman affected.
!                       .FALSE. - The channel(s) is(are) NOT Zeeman affected.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as SpcCoeff input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION SpcCoeff_IsZeeman(SpcCoeff, ChannelIndex) RESULT(Is_Set)
    TYPE(SpcCoeff_type), INTENT(IN) :: SpcCoeff
    INTEGER,   OPTIONAL, INTENT(IN) :: ChannelIndex
    LOGICAL :: Is_Set
    Is_Set = .FALSE.
    IF ( .NOT. SpcCoeff_Associated(SpcCoeff) ) RETURN
    Is_Set = SpcCoeff_IsFlagSet(SpcCoeff, ZEEMAN_FLAG, ChannelIndex=ChannelIndex)
  END FUNCTION SpcCoeff_IsZeeman
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_SetSolar
!
! PURPOSE:
!       Elemental subroutine to flag a SpcCoeff channel as solar sensitive.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_SetSolar( SpcCoeff, ChannelIndex=ChannelIndex )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be altered.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       ChannelIndex:  Set this to the index corresponding to a particular
!                      channel in the SpcCoeff object to flag as a
!                      solar sensitive channel.
!                      If not specified, all the channels are flagged.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Conformable with SpcCoeff input
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SpcCoeff_SetSolar( SpcCoeff, ChannelIndex )
    TYPE(SpcCoeff_type), INTENT(IN OUT) :: SpcCoeff
    INTEGER,   OPTIONAL, INTENT(IN)     :: ChannelIndex
    IF ( .NOT. SpcCoeff_Associated(SpcCoeff) ) RETURN
    CALL SpcCoeff_SetFlag(SpcCoeff, SOLAR_FLAG, ChannelIndex=ChannelIndex)
  END SUBROUTINE SpcCoeff_SetSolar
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_SetZeeman
!
! PURPOSE:
!       Elemental subroutine to flag a SpcCoeff channel as Zeeman affected.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_SetZeeman( SpcCoeff, ChannelIndex=ChannelIndex )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be altered.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       ChannelIndex:  Set this to the index corresponding to a particular
!                      channel in the SpcCoeff object to flag as a
!                      Zeeman affected channel.
!                      If not specified, all the channels are flagged.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Conformable with SpcCoeff input
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SpcCoeff_SetZeeman( SpcCoeff, ChannelIndex )
    TYPE(SpcCoeff_type), INTENT(IN OUT) :: SpcCoeff
    INTEGER,   OPTIONAL, INTENT(IN)     :: ChannelIndex
    IF ( .NOT. SpcCoeff_Associated(SpcCoeff) ) RETURN
    CALL SpcCoeff_SetFlag(SpcCoeff, ZEEMAN_FLAG, ChannelIndex=ChannelIndex)
  END SUBROUTINE SpcCoeff_SetZeeman
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_ClearSolar
!
! PURPOSE:
!       Elemental subroutine to flag a SpcCoeff channel as NOT being
!       solar sensitive.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_ClearSolar( SpcCoeff, ChannelIndex=ChannelIndex )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be altered.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       ChannelIndex:  Set this to the index corresponding to a particular
!                      channel in the SpcCoeff object to indicate as being
!                      NOT solar sensitive.
!                      If not specified, all the channels cleared.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Conformable with SpcCoeff input
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SpcCoeff_ClearSolar( SpcCoeff, ChannelIndex )
    TYPE(SpcCoeff_type), INTENT(IN OUT) :: SpcCoeff
    INTEGER,   OPTIONAL, INTENT(IN)     :: ChannelIndex
    IF ( .NOT. SpcCoeff_Associated(SpcCoeff) ) RETURN
    CALL SpcCoeff_ClearFlag( SpcCoeff, SOLAR_FLAG, ChannelIndex=ChannelIndex )
  END SUBROUTINE SpcCoeff_ClearSolar
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_ClearZeeman
!
! PURPOSE:
!       Elemental subroutine to flag a SpcCoeff channel as NOT being
!       Zeeman affected.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_ClearZeeman( SpcCoeff, ChannelIndex=ChannelIndex )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be altered.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       ChannelIndex:  Set this to the index corresponding to a particular
!                      channel in the SpcCoeff object to indicate as being
!                      NOT Zeeman affected.
!                      If not specified, all the channels cleared.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Conformable with SpcCoeff input
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SpcCoeff_ClearZeeman( SpcCoeff, ChannelIndex )
    TYPE(SpcCoeff_type), INTENT(IN OUT) :: SpcCoeff
    INTEGER,   OPTIONAL, INTENT(IN)     :: ChannelIndex
    IF ( .NOT. SpcCoeff_Associated(SpcCoeff) ) RETURN
    CALL SpcCoeff_ClearFlag( SpcCoeff, ZEEMAN_FLAG, ChannelIndex=ChannelIndex )
  END SUBROUTINE SpcCoeff_ClearZeeman
  
  


!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
! NOTE: The following sensor check and set procedures were generated
!       automatically using the
!         gen_sensor_procedures.rb
!       script. Modify at your own risk!
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_IsMicrowaveSensor
!
! PURPOSE:
!       Elemental function to test if the SpcCoeff object is for
!       a microwave sensor.
!
! CALLING SEQUENCE:
!       Status = SpcCoeff_IsMicrowaveSensor( SpcCoeff )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be tested.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value.
!                       .TRUE.  - The sensor is a microwave instrument.
!                       .FALSE. - The sensor is NOT a microwave instrument.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as SpcCoeff input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION SpcCoeff_IsMicrowaveSensor(SpcCoeff) RESULT(Is_Set)
    TYPE(SpcCoeff_type), INTENT(IN) :: SpcCoeff
    LOGICAL :: Is_Set
    Is_Set = SpcCoeff_IsSensor(SpcCoeff, MICROWAVE_SENSOR)
  END FUNCTION SpcCoeff_IsMicrowaveSensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_IsInfraredSensor
!
! PURPOSE:
!       Elemental function to test if the SpcCoeff object is for
!       an infrared sensor.
!
! CALLING SEQUENCE:
!       Status = SpcCoeff_IsInfraredSensor( SpcCoeff )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be tested.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value.
!                       .TRUE.  - The sensor is an infrared instrument.
!                       .FALSE. - The sensor is NOT an infrared instrument.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as SpcCoeff input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION SpcCoeff_IsInfraredSensor(SpcCoeff) RESULT(Is_Set)
    TYPE(SpcCoeff_type), INTENT(IN) :: SpcCoeff
    LOGICAL :: Is_Set
    Is_Set = SpcCoeff_IsSensor(SpcCoeff, INFRARED_SENSOR)
  END FUNCTION SpcCoeff_IsInfraredSensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_IsVisibleSensor
!
! PURPOSE:
!       Elemental function to test if the SpcCoeff object is for
!       a visible sensor.
!
! CALLING SEQUENCE:
!       Status = SpcCoeff_IsVisibleSensor( SpcCoeff )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be tested.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value.
!                       .TRUE.  - The sensor is a visible instrument.
!                       .FALSE. - The sensor is NOT a visible instrument.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as SpcCoeff input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION SpcCoeff_IsVisibleSensor(SpcCoeff) RESULT(Is_Set)
    TYPE(SpcCoeff_type), INTENT(IN) :: SpcCoeff
    LOGICAL :: Is_Set
    Is_Set = SpcCoeff_IsSensor(SpcCoeff, VISIBLE_SENSOR)
  END FUNCTION SpcCoeff_IsVisibleSensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_IsUltravioletSensor
!
! PURPOSE:
!       Elemental function to test if the SpcCoeff object is for
!       an ultraviolet sensor.
!
! CALLING SEQUENCE:
!       Status = SpcCoeff_IsUltravioletSensor( SpcCoeff )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be tested.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value.
!                       .TRUE.  - The sensor is an ultraviolet instrument.
!                       .FALSE. - The sensor is NOT an ultraviolet instrument.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as SpcCoeff input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION SpcCoeff_IsUltravioletSensor(SpcCoeff) RESULT(Is_Set)
    TYPE(SpcCoeff_type), INTENT(IN) :: SpcCoeff
    LOGICAL :: Is_Set
    Is_Set = SpcCoeff_IsSensor(SpcCoeff, ULTRAVIOLET_SENSOR)
  END FUNCTION SpcCoeff_IsUltravioletSensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_SetMicrowaveSensor
!
! PURPOSE:
!       Elemental subroutine to set a SpcCoeff object as being
!       for a microwave sensor.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_SetMicrowaveSensor( SpcCoeff )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be altered.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SpcCoeff_SetMicrowaveSensor( SpcCoeff )
    TYPE(SpcCoeff_type), INTENT(IN OUT) :: SpcCoeff
    CALL SpcCoeff_SetSensor(SpcCoeff, MICROWAVE_SENSOR)
  END SUBROUTINE SpcCoeff_SetMicrowaveSensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_SetInfraredSensor
!
! PURPOSE:
!       Elemental subroutine to set a SpcCoeff object as being
!       for an infrared sensor.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_SetInfraredSensor( SpcCoeff )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be altered.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SpcCoeff_SetInfraredSensor( SpcCoeff )
    TYPE(SpcCoeff_type), INTENT(IN OUT) :: SpcCoeff
    CALL SpcCoeff_SetSensor(SpcCoeff, INFRARED_SENSOR)
  END SUBROUTINE SpcCoeff_SetInfraredSensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_SetVisibleSensor
!
! PURPOSE:
!       Elemental subroutine to set a SpcCoeff object as being
!       for a visible sensor.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_SetVisibleSensor( SpcCoeff )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be altered.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SpcCoeff_SetVisibleSensor( SpcCoeff )
    TYPE(SpcCoeff_type), INTENT(IN OUT) :: SpcCoeff
    CALL SpcCoeff_SetSensor(SpcCoeff, VISIBLE_SENSOR)
  END SUBROUTINE SpcCoeff_SetVisibleSensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_SetUltravioletSensor
!
! PURPOSE:
!       Elemental subroutine to set a SpcCoeff object as being
!       for an ultraviolet sensor.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_SetUltravioletSensor( SpcCoeff )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be altered.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SpcCoeff_SetUltravioletSensor( SpcCoeff )
    TYPE(SpcCoeff_type), INTENT(IN OUT) :: SpcCoeff
    CALL SpcCoeff_SetSensor(SpcCoeff, ULTRAVIOLET_SENSOR)
  END SUBROUTINE SpcCoeff_SetUltravioletSensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_ClearSensor
!
! PURPOSE:
!       Elemental subroutine to reinitialise the sensor type.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_ClearSensor( SpcCoeff )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be altered.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SpcCoeff_ClearSensor(SpcCoeff)
    TYPE(SpcCoeff_type), INTENT(IN OUT) :: SpcCoeff
    SpcCoeff%Sensor_Type = INVALID_SENSOR
  END SUBROUTINE SpcCoeff_ClearSensor
  

!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       SpcCoeff_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two SpcCoeff objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = SpcCoeff_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two SpcCoeff objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION SpcCoeff_Equal( x, y ) RESULT( is_equal )
    TYPE(SpcCoeff_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( (.NOT. SpcCoeff_Associated(x)) .OR. &
         (.NOT. SpcCoeff_Associated(y))      ) RETURN

    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( x%n_Channels /= y%n_Channels ) RETURN
    ! ...Scalars
    IF ( (x%Sensor_Id        /= y%Sensor_Id       ) .OR. &
         (x%WMO_Satellite_ID /= y%WMO_Satellite_ID) .OR. &
         (x%WMO_Sensor_ID    /= y%WMO_Sensor_ID   ) .OR. &
         (x%Sensor_Type      /= y%Sensor_Type     ) ) RETURN
    ! ...Structures
    IF ( ACCoeff_Associated( x%AC ) .NEQV. ACCoeff_Associated( y%AC ) ) RETURN
    IF ( ACCoeff_Associated( x%AC ) .AND.  ACCoeff_Associated( y%AC ) ) THEN
      IF ( .NOT. (x%AC == y%AC) ) RETURN
    END IF
    IF ( NLTECoeff_Associated( x%NC ) .NEQV. NLTECoeff_Associated( y%NC ) ) RETURN
    IF ( NLTECoeff_Associated( x%NC ) .AND.  NLTECoeff_Associated( y%NC ) ) THEN
      IF ( .NOT. (x%NC == y%NC) ) RETURN
    END IF
    ! ...Arrays
    IF ( ALL(x%Sensor_Channel                 ==    y%Sensor_Channel            ) .AND. &
         ALL(x%Polarization                   ==    y%Polarization              ) .AND. &
         ALL(x%Channel_Flag                   ==    y%Channel_Flag              ) .AND. &
         ALL(x%Frequency                  .EqualTo. y%Frequency                 ) .AND. &
         ALL(x%Wavenumber                 .EqualTo. y%Wavenumber                ) .AND. &
         ALL(x%Planck_C1                  .EqualTo. y%Planck_C1                 ) .AND. &
         ALL(x%Planck_C2                  .EqualTo. y%Planck_C2                 ) .AND. &
         ALL(x%Band_C1                    .EqualTo. y%Band_C1                   ) .AND. &
         ALL(x%Band_C2                    .EqualTo. y%Band_C2                   ) .AND. &
         ALL(x%Cosmic_Background_Radiance .EqualTo. y%Cosmic_Background_Radiance) .AND. &
         ALL(x%Solar_Irradiance           .EqualTo. y%Solar_Irradiance          ) ) &
      is_equal = .TRUE.

  END FUNCTION SpcCoeff_Equal

  
  ELEMENTAL FUNCTION SpcCoeff_IsSensor(SpcCoeff, Sensor_Type) RESULT(Is_Set)
    TYPE(SpcCoeff_type), INTENT(IN) :: SpcCoeff
    INTEGER,             INTENT(IN) :: Sensor_Type
    LOGICAL :: Is_Set
    Is_Set = .FALSE.
    IF ( .NOT. SpcCoeff_Associated(SpcCoeff) ) RETURN
    Is_Set = (SpcCoeff%Sensor_Type == Sensor_Type)
  END FUNCTION SpcCoeff_IsSensor


  ELEMENTAL SUBROUTINE SpcCoeff_SetSensor(SpcCoeff, Sensor_Type)
    TYPE(SpcCoeff_type), INTENT(IN OUT) :: SpcCoeff
    INTEGER            , INTENT(IN)     :: Sensor_Type
    SpcCoeff%Sensor_Type = Sensor_Type
  END SUBROUTINE SpcCoeff_SetSensor


!--------------------------------------------------------------------------------
!
! NAME:
!       SpcCoeff_IsFlagSet
!
! PURPOSE:
!       Private elemental function to test if ANY SpcCoeff channels have
!       the specified bitflags set in the Channel_Flag component.
!
! CALLING SEQUENCE:
!       Status = SpcCoeff_IsFlagSet( &
!                  SpcCoeff , &
!                  Flag_Type, &
!                  ChannelIndex=ChannelIndex )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to be tested.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Flag_Type:     Integer specifying the bitflag position.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       ChannelIndex:  Set this to the index corresponding to a particular
!                      channel in the SpcCoeff object to test.
!                      If not specified, all the channels are tested.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Conformable with SpcCoeff input
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value.
!                       .TRUE.  - The specified flag is set on ANY channel.
!                       .FALSE. - The specified flag is NOT set on ALL channels.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as SpcCoeff input
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION SpcCoeff_IsFlagSet( &
    SpcCoeff     , &  ! Input
    Flag_Type    , &  ! Input
    ChannelIndex ) &  ! Optional input
  RESULT(Is_Set)
    ! Arguments
    TYPE(SpcCoeff_type), INTENT(IN) :: SpcCoeff
    INTEGER            , INTENT(IN) :: Flag_Type
    INTEGER,  OPTIONAL , INTENT(IN) :: ChannelIndex
    ! Function result
    LOGICAL :: Is_Set
    
    ! Setup
    Is_Set = .FALSE.
    IF ( .NOT. SpcCoeff_Associated(SpcCoeff) ) RETURN
    
    ! Perform test based on presence of channel index
    IF ( PRESENT(ChannelIndex) ) THEN
      IF ( ChannelIndex < 1 .OR. ChannelIndex > SpcCoeff%n_Channels ) RETURN
      Is_Set = BTEST(SpcCoeff%Channel_Flag(ChannelIndex),Flag_Type)
    ELSE
      Is_Set = ANY(BTEST(SpcCoeff%Channel_Flag,Flag_Type))
    END IF

  END FUNCTION SpcCoeff_IsFlagSet

  
!--------------------------------------------------------------------------------
!
! NAME:
!       SpcCoeff_SetFlag
!
! PURPOSE:
!       Private elemental subroutine to set the specified bitflags in the 
!       Channel_Flag component of an SpcCoeff object.
!
! CALLING SEQUENCE:
!        CALL SpcCoeff_SetFlag( &
!               SpcCoeff , &
!               Flag_Type, &
!               ChannelIndex=ChannelIndex )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to have its channel bitflags set.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       Flag_Type:     Integer specifying the bitflag position.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       ChannelIndex:  Set this to the index corresponding to a particular
!                      channel in the SpcCoeff object for which the bitflag
!                      is to be set.
!                      If not specified, the bitflag is set for all the channels.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Conformable with SpcCoeff input
!                      ATTRIBUTES: INTENT(IN)
!
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SpcCoeff_SetFlag( &
    SpcCoeff    , &  ! In/Output
    Flag_Type   , &  ! Input
    ChannelIndex  )  ! Optional input
    ! Arguments
    TYPE(SpcCoeff_type), INTENT(IN OUT) :: SpcCoeff
    INTEGER            , INTENT(IN)     :: Flag_Type
    INTEGER,  OPTIONAL , INTENT(IN)     :: ChannelIndex

    ! Perform test based on presence of channel index
    IF ( PRESENT(ChannelIndex) ) THEN
      IF ( ChannelIndex < 1 .OR. ChannelIndex > SpcCoeff%n_Channels ) RETURN
      SpcCoeff%Channel_Flag(ChannelIndex) = IBSET(SpcCoeff%Channel_Flag(ChannelIndex),Flag_Type)
    ELSE
      SpcCoeff%Channel_Flag = IBSET(SpcCoeff%Channel_Flag,Flag_Type)
    END IF

  END SUBROUTINE SpcCoeff_SetFlag


!--------------------------------------------------------------------------------
!
! NAME:
!       SpcCoeff_ClearFlag
!
! PURPOSE:
!       Private elemental subroutine to clear the specified bitflags in the 
!       Channel_Flag component of an SpcCoeff object.
!
! CALLING SEQUENCE:
!        CALL SpcCoeff_ClearFlag( &
!               SpcCoeff , &
!               Flag_Type, &
!               ChannelIndex=ChannelIndex )
!
! OBJECTS:
!       SpcCoeff:      Structure which is to have its channel bitflags cleared.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       Flag_Type:     Integer specifying the bitflag position.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       ChannelIndex:  Set this to the index corresponding to a particular
!                      channel in the SpcCoeff object for which the bitflag
!                      is to be cleared.
!                      If not specified, the bitflag is cleared for all the
!                      channels.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Conformable with SpcCoeff input
!                      ATTRIBUTES: INTENT(IN)
!
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SpcCoeff_ClearFlag( &
    SpcCoeff    , &  ! In/Output
    Flag_Type   , &  ! Input
    ChannelIndex  )  ! Optional input
    ! Arguments
    TYPE(SpcCoeff_type), INTENT(IN OUT) :: SpcCoeff
    INTEGER            , INTENT(IN)     :: Flag_Type
    INTEGER,  OPTIONAL , INTENT(IN)     :: ChannelIndex
    
    ! Perform test based on presence of channel index
    IF ( PRESENT(ChannelIndex) ) THEN
      IF ( ChannelIndex < 1 .OR. ChannelIndex > SpcCoeff%n_Channels ) RETURN
      SpcCoeff%Channel_Flag(ChannelIndex) = IBCLR(SpcCoeff%Channel_Flag(ChannelIndex),Flag_Type)
    ELSE
      SpcCoeff%Channel_Flag = IBCLR(SpcCoeff%Channel_Flag,Flag_Type)
    END IF

  END SUBROUTINE SpcCoeff_ClearFlag
  
END MODULE SpcCoeff_Define
