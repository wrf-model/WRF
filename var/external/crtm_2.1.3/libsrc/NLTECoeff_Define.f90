!
! NLTECoeff_Define
!
! Module defining the NLTECoeff structure and containing routines to 
! manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, 08-05-2010
!                       yong.han@noaa.gov
!
!       Refactored by:  Paul van Delst, 19-Jan-2011
!                       paul.vandelst@noaa.gov
!

MODULE NLTECoeff_Define

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
                                   INVALID_WMO_SENSOR_ID   
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: NLTECoeff_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: NLTECoeff_Associated
  PUBLIC :: NLTECoeff_Destroy
  PUBLIC :: NLTECoeff_Create
  PUBLIC :: NLTECoeff_Inspect
  PUBLIC :: NLTECoeff_ValidRelease
  PUBLIC :: NLTECoeff_Info
  PUBLIC :: NLTECoeff_DefineVersion
  PUBLIC :: NLTECoeff_Subset
  PUBLIC :: NLTECoeff_Concat
  PUBLIC :: NLTECoeff_ChannelReindex


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE NLTECoeff_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: NLTECoeff_Define.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512
  ! Sensor id string length
  INTEGER, PARAMETER :: SL = 20
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: NLTECOEFF_RELEASE = 2   ! This determines structure and file formats.
  INTEGER, PARAMETER :: NLTECOEFF_VERSION = 1   ! This is just the data version.
  ! Number of layers for which mean temperatures are computed
  INTEGER, PARAMETER :: N_LAYERS = 2
  ! Integer flags corresponding to logical false/true
  INTEGER, PARAMETER :: FALSE = 0
  INTEGER, PARAMETER :: TRUE  = 1

  
  ! -----------------------
  ! Derived type definition
  ! -----------------------
  TYPE :: NLTECoeff_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = NLTECOEFF_RELEASE
    INTEGER(Long) :: Version = NLTECOEFF_VERSION
    ! Dimensions
    INTEGER(Long) :: n_Predictors     = 0  ! n1 dimension
    INTEGER(Long) :: n_Sensor_Angles  = 0  ! n2 dimension
    INTEGER(Long) :: n_Solar_Angles   = 0  ! n3 dimension
    INTEGER(Long) :: n_NLTE_Channels  = 0  ! n4 dimension
    INTEGER(Long) :: n_Channels       = 0  ! n5 dimension
    ! ..."Internal" dimension
    INTEGER(Long) :: n_Layers = N_LAYERS
    ! Sensor info
    CHARACTER(SL)              :: Sensor_Id        = ''
    INTEGER(Long)              :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER(Long)              :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER(Long), ALLOCATABLE :: Sensor_Channel(:)        ! n5  
    ! Pressure levels used for computing mean temperatures in the two layers
    REAL(Double) :: Upper_Plevel(N_LAYERS) = ZERO
    REAL(Double) :: Lower_Plevel(N_LAYERS) = ZERO
    ! Min., max. and mean layer temperatures used as the temperature predictor limits 
    REAL(Double) :: Min_Tm(N_LAYERS)  = ZERO
    REAL(Double) :: Max_Tm(N_LAYERS)  = ZERO
    REAL(Double) :: Mean_Tm(N_LAYERS) = ZERO
    ! Coefficient table dimension vectors
    REAL(Double) , ALLOCATABLE :: Secant_Sensor_Zenith(:)  ! n2  
    REAL(Double) , ALLOCATABLE :: Secant_Solar_Zenith(:)   ! n3  
    INTEGER(Long), ALLOCATABLE :: NLTE_Channel(:)          ! n4
    LOGICAL      , ALLOCATABLE :: Is_NLTE_Channel(:)       ! n5
    ! Coefficients for NLTE corrections
    INTEGER(Long), ALLOCATABLE :: C_Index(:)               ! n5
    REAL(Double) , ALLOCATABLE :: C(:,:,:,:)               ! n1 x n2 x n3 x n4 
  END TYPE NLTECoeff_type

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
!       NLTECoeff_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the NLTECoeff structure.
!
! CALLING SEQUENCE:
!       Status = NLTECoeff_Associated( NLTECoeff )
!
! OBJECTS:
!       NLTECoeff:  Structure which is to have its member's
!                   status tested.
!                   UNITS:      N/A
!                   TYPE:       NLTECoeff_type
!                   DIMENSION:  Scalar or any rank
!                   ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:     The return value is a logical value indicating the
!                   status of the NLTE members.
!                    .TRUE.  - if ANY of the NLTECoeff allocatable members
!                              are in use.
!                    .FALSE. - if ALL of the NLTECoeff allocatable members
!                              are not in use.
!                   UNITS:      N/A
!                   TYPE:       LOGICAL
!                   DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION NLTECoeff_Associated( NLTECoeff ) RESULT( Status )
    TYPE(NLTECoeff_type), INTENT(IN) :: NLTECoeff
    LOGICAL :: Status
    Status = NLTECoeff%Is_Allocated
  END FUNCTION NLTECoeff_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize NLTECoeff objects.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_Destroy( NLTECoeff )
!
! OBJECTS:
!       NLTECoeff:    Re-initialized NLTECoeff structure.
!                     UNITS:      N/A
!                     TYPE:       NLTECoeff_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE NLTECoeff_Destroy( NLTECoeff )
    TYPE(NLTECoeff_type), INTENT(OUT) :: NLTECoeff
    NLTECoeff%Is_Allocated = .FALSE.
    NLTECoeff%n_Predictors     = 0
    NLTECoeff%n_Sensor_Angles  = 0
    NLTECoeff%n_Solar_Angles   = 0
    NLTECoeff%n_NLTE_Channels  = 0
    NLTECoeff%n_Channels       = 0
    NLTECoeff%Sensor_Id        = ''
    NLTECoeff%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    NLTECoeff%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
  END SUBROUTINE NLTECoeff_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of an NLTECoeff object.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_Create( NLTECoeff       , &
!                              n_Predictors    , &      
!                              n_Sensor_Angles , &      
!                              n_Solar_Angles  , &      
!                              n_NLTE_Channels , &      
!                              n_Channels        )         
!
! OBJECTS:
!       NLTECoeff:          NLTECoeff object structure.
!                           UNITS:      N/A
!                           TYPE:       NLTECoeff_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Predictors:       Number of predictors used in NLTE correction algorithm.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Same as the NLTECoeff object
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Sensor_Angles:    Number of sensor zenith angles.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Same as the NLTECoeff object
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Solar_Angles:     Number of solar zenith angles.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Same as the NLTECoeff object
!                           ATTRIBUTES: INTENT(IN)
!
!       n_NLTE_Channels:    Number of NLTE channels for the sensor.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Same as the NLTECoeff object
!                           ATTRIBUTES: INTENT(IN)
!
!        n_Channels:        Total number of channels for the sensor.
!                           Must be >= n_NLTE_Channels.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Same as NLTECoeff object
!                           ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE NLTECoeff_Create( &
    NLTECoeff       , &  ! Output
    n_Predictors    , &  ! Input
    n_Sensor_Angles , &  ! Input
    n_Solar_Angles  , &  ! Input
    n_NLTE_Channels , &  ! Input
    n_Channels        )  ! Input
    ! Arguments
    TYPE(NLTECoeff_type), INTENT(OUT) :: NLTECoeff
    INTEGER             , INTENT(IN)  :: n_Predictors            
    INTEGER             , INTENT(IN)  :: n_Sensor_Angles              
    INTEGER             , INTENT(IN)  :: n_Solar_Angles            
    INTEGER             , INTENT(IN)  :: n_NLTE_Channels            
    INTEGER             , INTENT(IN)  :: n_Channels              
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Predictors    < 1 .OR. &
         n_Sensor_Angles < 1 .OR. &
         n_Solar_Angles  < 1 .OR. &
         n_NLTE_Channels < 1 .OR. &
         n_Channels < n_NLTE_Channels ) RETURN
    
    ! Perform the allocation
    ALLOCATE( NLTECoeff%Sensor_Channel( n_Channels ), &
              NLTECoeff%Secant_Sensor_Zenith( n_Sensor_Angles ), &
              NLTECoeff%Secant_Solar_Zenith( n_Solar_Angles ), &
              NLTECoeff%NLTE_Channel( n_NLTE_Channels ), &
              NLTECoeff%Is_NLTE_Channel( n_Channels ), &
              NLTECoeff%C_Index( n_Channels ), &
              NLTECoeff%C( n_Predictors, n_Sensor_Angles, n_Solar_Angles, n_NLTE_Channels ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    NLTECoeff%n_Predictors    = n_Predictors
    NLTECoeff%n_Sensor_Angles = n_Sensor_Angles
    NLTECoeff%n_Solar_Angles  = n_Solar_Angles
    NLTECoeff%n_NLTE_Channels = n_NLTE_Channels
    NLTECoeff%n_Channels      = n_Channels
    ! ...Arrays
    NLTECoeff%Sensor_Channel       = 0
    NLTECoeff%Secant_Sensor_Zenith = ZERO
    NLTECoeff%Secant_Solar_Zenith  = ZERO
    NLTECoeff%NLTE_Channel         = 0
    NLTECoeff%Is_NLTE_Channel      = .FALSE.
    NLTECoeff%C_Index              = 0
    NLTECoeff%C                    = ZERO

    ! Set allocation indicator
    NLTECoeff%Is_Allocated = .TRUE.

  END SUBROUTINE NLTECoeff_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a NLTECoeff object to stdout.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_Inspect( NLTECoeff )
!
! OBJECTS:
!       NLTECoeff:     NLTECoeff object to display.
!                      UNITS:      N/A
!                      TYPE:       NLTECoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE NLTECoeff_Inspect( NLTECoeff )
    TYPE(NLTECoeff_type), INTENT(IN) :: NLTECoeff
    INTEGER :: i
    CHARACTER(3) :: maybe
    WRITE(*,'(1x,"NLTECoeff OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version  :",1x,i0,".",i0)') NLTECoeff%Release, NLTECoeff%Version
    ! Dimensions
    WRITE(*,'(3x,"n_Predictors     :",1x,i0)') NLTECoeff%n_Predictors    
    WRITE(*,'(3x,"n_Sensor_Angles  :",1x,i0)') NLTECoeff%n_Sensor_Angles 
    WRITE(*,'(3x,"n_Solar_Angles   :",1x,i0)') NLTECoeff%n_Solar_Angles  
    WRITE(*,'(3x,"n_NLTE_Channels  :",1x,i0)') NLTECoeff%n_NLTE_Channels 
    WRITE(*,'(3x,"n_Channels       :",1x,i0)') NLTECoeff%n_Channels
    IF ( .NOT. NLTECoeff_Associated(NLTECoeff) ) RETURN
    ! Sensor info
    WRITE(*,'(3x,"Sensor_Id        :",1x,a )') TRIM(NLTECoeff%Sensor_Id)
    WRITE(*,'(3x,"WMO_Satellite_ID :",1x,i0)') NLTECoeff%WMO_Satellite_ID 
    WRITE(*,'(3x,"WMO_Sensor_ID    :",1x,i0)') NLTECoeff%WMO_Sensor_ID
    WRITE(*,'(3x,"Sensor_Channel   :")')
    WRITE(*,'(10(1x,i5,:))') NLTECoeff%Sensor_Channel
    ! Pressure arrays
    WRITE(*,'(3x,"Upper_Plevel :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%Upper_Plevel
    WRITE(*,'(3x,"Lower_Plevel :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%Lower_Plevel
    ! Temperature arrays
    WRITE(*,'(3x,"Min_Tm  :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%Min_Tm
    WRITE(*,'(3x,"Max_Tm  :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%Max_Tm
    WRITE(*,'(3x,"Mean_Tm :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%Mean_Tm
    ! Coefficient table dimension vectors
    WRITE(*,'(3x,"Secant_Sensor_Zenith :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%Secant_Sensor_Zenith
    WRITE(*,'(3x,"Secant_Solar_Zenith  :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%Secant_Solar_Zenith
    WRITE(*,'(3x,"NLTE_Channel         :")')
    WRITE(*,'(10(1x,i5,:))') NLTECoeff%NLTE_Channel
    ! NLTE channel flag
    WRITE(*,'(3x,"NLTE_Channel_Flag :")')
    DO i = 1, NLTECoeff%n_Channels
      IF ( MOD(i,5) == 0 .OR. i == NLTECoeff%n_Channels ) THEN
        maybe = 'yes'
      ELSE
        maybe = 'no'
      END IF
      WRITE(*,FMT='(1x,i5,":",l1,", c-index: ",i0)',ADVANCE=maybe) NLTECoeff%Sensor_Channel(i), &
                                                                   NLTECoeff%Is_NLTE_Channel(i), &
                                                                   NLTECoeff%C_Index(i)
    END DO
    ! Coefficient data
    WRITE(*,'(3x,"NLTE correction coefficients :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%C
  END SUBROUTINE NLTECoeff_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_ValidRelease
!
! PURPOSE:
!       Function to check the NLTECoeff Release value.
!
! CALLING SEQUENCE:
!       IsValid = NLTECoeff_ValidRelease( NLTECoeff )
!
! INPUTS:
!       NLTECoeff:     NLTECoeff object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       NLTECoeff_type
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

  FUNCTION NLTECoeff_ValidRelease( NLTECoeff ) RESULT( IsValid )
    ! Arguments
    TYPE(NLTECoeff_type), INTENT(IN) :: NLTECoeff
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTECoeff_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( NLTECoeff%Release < NLTECOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A NLTECoeff data update is needed. ", &
                  &"NLTECoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  NLTECoeff%Release, NLTECOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( NLTECoeff%Release > NLTECOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A NLTECoeff software update is needed. ", &
                  &"NLTECoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  NLTECoeff%Release, NLTECOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION NLTECoeff_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a NLTECoeff object.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_Info( NLTECoeff, Info )
!
! OBJECTS:
!       NLTECoeff:     NLTECoeff object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       NLTECoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the NLTECoeff object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE NLTECoeff_Info( NLTECoeff, Info )
    ! Arguments
    TYPE(NLTECoeff_type), INTENT(IN)  :: NLTECoeff
    CHARACTER(*),         INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"NLTECoeff RELEASE.VERSION: ",i2,".",i2.2,a,3x, &
           &"N_PREDICTORS=",i0,2x,&
           &"N_SENSOR_ANGLES=",i0,2x,&
           &"N_SOLAR_ANGLES=",i0,2x,&
           &"N_NLTE_CHANNELS=",i0,2x,&
           &"N_CHANNELS=",i0 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           NLTECoeff%Release, NLTECoeff%Version, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           NLTECoeff%n_Predictors    , &
           NLTECoeff%n_Sensor_Angles , &
           NLTECoeff%n_Solar_Angles  , &
           NLTECoeff%n_NLTE_Channels , &
           NLTECoeff%n_Channels
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE NLTECoeff_Info
 
 
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_DefineVersion( Id )
!
! OUTPUTS:
!       Id:    Character string containing the version Id information
!              for the module.
!              UNITS:      N/A
!              TYPE:       CHARACTER(*)
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE NLTECoeff_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE NLTECoeff_DefineVersion


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_Subset
!
! PURPOSE:
!       Subroutine to return a channel subset of the input NLTECoeff object.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_Subset( NLTECoeff, Subset, NC_Subset )
!
! OBJECTS:
!       NLTECoeff:      NLTECoeff object which is to be subsetted.
!                     UNITS:      N/A
!                     TYPE:       NLTECoeff_type
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
!       NC_Subset:    NLTECoeff object containing the requested channel subset
!                     of the input NLTECoeff data.
!                     UNITS:      N/A
!                     TYPE:       NLTECoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE NLTECoeff_Subset( &
    NLTECoeff     , &  ! Input
    Sensor_Channel, &  ! Input
    NC_Subset       )  ! Output
    ! Arguments
    TYPE(NLTECoeff_type), INTENT(IN)  :: NLTECoeff
    INTEGER             , INTENT(IN)  :: Sensor_Channel(:)
    TYPE(NLTECoeff_type), INTENT(OUT) :: NC_Subset
    ! Local variables
    TYPE(Subset_type) :: subset, nlte_subset
    INTEGER :: n_subset_channels, n_nlte_subset_channels
    INTEGER, ALLOCATABLE :: idx(:), nlte_idx(:)
    
    ! Check input is valid
    IF ( .NOT. NLTECoeff_Associated(NLTECoeff) ) RETURN
    
    
    ! Generate the channel subset list
    CALL Subset_Generate( &
           subset, &
           NLTECoeff%Sensor_Channel, &
           Sensor_Channel )
    IF ( .NOT. Subset_Associated( subset ) ) RETURN
    ! ...Generate the NLTE channel subset list
    ! ...(which is itself a subset of the sensor channel list)
    CALL Subset_Generate( &
           nlte_subset, &
           NLTECoeff%NLTE_Channel, &
           Sensor_Channel )
    IF ( .NOT. Subset_Associated( nlte_subset ) ) RETURN


    ! Allocate the output subset NLTECoeff object
    CALL Subset_GetValue( subset     , n_Values = n_subset_channels     , Index = idx )
    CALL Subset_GetValue( nlte_subset, n_Values = n_nlte_subset_channels, Index = nlte_idx )
    CALL NLTECoeff_Create( &
           NC_Subset                , &
           NLTECoeff%n_Predictors   , &
           NLTECoeff%n_Sensor_Angles, &
           NLTECoeff%n_Solar_Angles , &
           n_nlte_subset_channels   , &
           n_subset_channels          )
    IF ( .NOT. NLTECoeff_Associated(NC_Subset) ) RETURN


    ! Extract out the subset channels
    ! ...First assign the non-channel dependent data
    NC_Subset%Version              = NLTECoeff%Version
    NC_Subset%Sensor_Id            = NLTECoeff%Sensor_Id       
    NC_Subset%WMO_Satellite_ID     = NLTECoeff%WMO_Satellite_ID
    NC_Subset%WMO_Sensor_ID        = NLTECoeff%WMO_Sensor_ID   
    NC_Subset%Upper_Plevel         = NLTECoeff%Upper_Plevel        
    NC_Subset%Lower_Plevel         = NLTECoeff%Lower_Plevel        
    NC_Subset%Min_Tm               = NLTECoeff%Min_Tm              
    NC_Subset%Max_Tm               = NLTECoeff%Max_Tm              
    NC_Subset%Mean_Tm              = NLTECoeff%Mean_Tm             
    NC_Subset%Secant_Sensor_Zenith = NLTECoeff%Secant_Sensor_Zenith
    NC_Subset%Secant_Solar_Zenith  = NLTECoeff%Secant_Solar_Zenith 
    ! ...and now extract the subset
    NC_Subset%Sensor_Channel  = NLTECoeff%Sensor_Channel(idx)
    NC_Subset%NLTE_Channel    = NLTECoeff%NLTE_Channel(nlte_idx)
    NC_Subset%Is_NLTE_Channel = NLTECoeff%Is_NLTE_Channel(idx)
    NC_Subset%C_Index         = NLTECoeff%C_Index(idx)
    NC_Subset%C               = NLTECoeff%C(:,:,:,nlte_idx)
    ! ...Reindex the correction coefficient index array
    CALL NLTECoeff_Reindex( NC_Subset )

  END SUBROUTINE NLTECoeff_Subset


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_Concat
!
! PURPOSE:
!       Subroutine to concatenate multiple NLTECoeff objects along the channel
!       dimension into a single NLTECoeff object.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_Concat( NLTECoeff, NC_Array, Sensor_Id=Sensor_Id )
!
! OBJECTS:
!       NLTECoeff:    NLTECoeff object containing the concatenated result.
!                     UNITS:      N/A
!                     TYPE:       NLTECoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       NC_Array:     Array of NLTECoeff objects to be concatenated.
!                     UNITS:      N/A
!                     TYPE:       NLTECoeff_type
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Sensor_Id:    Sensor id character to string to use for the concatenated
!                     result. If not specified, the sensor id of the first valid
!                     element of NC_Array is used.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE NLTECoeff_Concat( &
    NLTECoeff, &  ! Output
    NC_Array , &  ! Input
    Sensor_Id  )  ! Optional input
    ! Arguments
    TYPE(NLTECoeff_type)  , INTENT(OUT) :: NLTECoeff
    TYPE(NLTECoeff_type)  , INTENT(IN)  :: NC_Array(:)
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Id
    ! Local variables
    INTEGER, ALLOCATABLE :: valid_index(:)
    INTEGER :: i, j, n_nc, n_valid, n_channels, n_nlte_channels
    INTEGER :: ch1, ch2, nlte_ch1, nlte_ch2

    ! Set up
    ! ...Check input is valid
    n_nc = SIZE(NC_Array)
    IF ( n_nc < 1 ) RETURN
    ! ...Count valid input
    n_valid = COUNT(NLTECoeff_Associated(NC_Array))
    IF ( n_valid == 0 ) RETURN
    ! ...Index the valid input
    ALLOCATE( valid_index(n_valid) )
    valid_index = PACK( (/(i,i=1,n_nc)/), MASK=NLTECoeff_Associated(NC_Array) )
    ! ...Check non-channel dimensions and ids
    DO j = 1, n_valid
      i = valid_index(j)
      IF ( NC_Array(i)%n_Predictors     /= NC_Array(valid_index(1))%n_Predictors     .OR. &
           NC_Array(i)%n_Sensor_Angles  /= NC_Array(valid_index(1))%n_Sensor_Angles  .OR. &
           NC_Array(i)%n_Solar_Angles   /= NC_Array(valid_index(1))%n_Solar_Angles   .OR. &
           NC_Array(i)%WMO_Satellite_ID /= NC_Array(valid_index(1))%WMO_Satellite_ID .OR. &
           NC_Array(i)%WMO_Sensor_ID    /= NC_Array(valid_index(1))%WMO_Sensor_ID         ) THEN
        RETURN
      END IF
    END DO
    
    
    ! Sum channel dimensions
    n_nlte_channels = SUM(NC_Array(valid_index)%n_NLTE_Channels)
    n_channels      = SUM(NC_Array(valid_index)%n_Channels)
    

    ! Allocate the output concatenated NLTECoeff object
    CALL NLTECoeff_Create( &
           NLTECoeff                  , &
           NC_Array(valid_index(1))%n_Predictors   , &
           NC_Array(valid_index(1))%n_Sensor_Angles, &
           NC_Array(valid_index(1))%n_Solar_Angles , &
           n_nlte_channels            , &
           n_channels                   )
    IF ( .NOT. NLTECoeff_Associated(NLTECoeff) ) RETURN
    

    ! Concatenate the channel data
    ! ...First assign the non-channel dependent data
    NLTECoeff%Version = NC_Array(valid_index(1))%Version
    IF ( PRESENT(Sensor_Id) ) THEN
      NLTECoeff%Sensor_Id = ADJUSTL(Sensor_Id)
    ELSE
      NLTECoeff%Sensor_Id = NC_Array(valid_index(1))%Sensor_Id
    END IF
    NLTECoeff%WMO_Satellite_ID     = NC_Array(valid_index(1))%WMO_Satellite_ID
    NLTECoeff%WMO_Sensor_ID        = NC_Array(valid_index(1))%WMO_Sensor_ID   
    NLTECoeff%Upper_Plevel         = NC_Array(valid_index(1))%Upper_Plevel        
    NLTECoeff%Lower_Plevel         = NC_Array(valid_index(1))%Lower_Plevel        
    NLTECoeff%Min_Tm               = NC_Array(valid_index(1))%Min_Tm              
    NLTECoeff%Max_Tm               = NC_Array(valid_index(1))%Max_Tm              
    NLTECoeff%Mean_Tm              = NC_Array(valid_index(1))%Mean_Tm             
    NLTECoeff%Secant_Sensor_Zenith = NC_Array(valid_index(1))%Secant_Sensor_Zenith
    NLTECoeff%Secant_Solar_Zenith  = NC_Array(valid_index(1))%Secant_Solar_Zenith 
    ! ...and now concatenate the channel data
    ch1      = 1
    nlte_ch1 = 1
    DO j = 1, n_valid
      i = valid_index(j)
      
      nlte_ch2 = nlte_ch1 + NC_Array(i)%n_NLTE_Channels - 1
      ch2      = ch1      + NC_Array(i)%n_Channels      - 1
      
      NLTECoeff%Sensor_Channel(ch1:ch2)         = NC_Array(i)%Sensor_Channel
      NLTECoeff%NLTE_Channel(nlte_ch1:nlte_ch2) = NC_Array(i)%NLTE_Channel
      NLTECoeff%Is_NLTE_Channel(ch1:ch2)        = NC_Array(i)%Is_NLTE_Channel
      NLTECoeff%C_Index(ch1:ch2)                = NC_Array(i)%C_Index
      NLTECoeff%C(:,:,:,nlte_ch1:nlte_ch2)      = NC_Array(i)%C
      
      nlte_ch1 = nlte_ch2 + 1
      ch1      = ch2      + 1
    END DO
    ! ...Reindex the correction coefficient index array
    CALL NLTECoeff_Reindex( NLTECoeff )


    ! Cleanup
    DEALLOCATE( valid_index )
    
  END SUBROUTINE NLTECoeff_Concat


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_ChannelReindex
!
! PURPOSE:
!       Subroutine to re-index an NLTECoeff object for a different complete
!       channel set.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_ChannelReindex( NLTECoeff, Sensor_Channels )
!
! OBJECTS:
!       NLTECoeff:      NLTECoeff object to have its channel information reindexed.
!                       UNITS:      N/A
!                       TYPE:       NLTECoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       Sensor_Channel: Array of channel numbers for which the NLTECoeff object 
!                       is to be re-indexed against.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN)
!
! COMMENTS:
!       If there is a mismatch between the channel sets, e.g. total number of
!       sensor channels is less than the number of NLTE channels, or if ANY of
!       the NLTE channels are NOT in the sensor channel list, no reindexing is
!       performed and the input structure is returned with no changes.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE NLTECoeff_ChannelReindex( NLTECoeff, Sensor_Channel )
    ! Arguments
    TYPE(NLTECoeff_type), INTENT(IN OUT) :: NLTECoeff
    INTEGER             , INTENT(IN)     :: Sensor_Channel(:)
    ! Local variables
    TYPE(NLTECoeff_type) :: nc_copy
    INTEGER :: i, i_nlte
    INTEGER :: n_channels, n_nlte_channels
    
    ! Setup
    IF ( .NOT. NLTECoeff_Associated(NLTECoeff) ) RETURN
    n_channels = SIZE(Sensor_Channel)
    IF ( n_channels < 1 ) RETURN
    IF ( n_channels < NLTECoeff%n_NLTE_Channels ) RETURN
    
    
    ! Copy the input structure
    nc_copy = NLTECoeff
    
    
    ! Allocate the reindexed NLTECoeff object
    CALL NLTECoeff_Create( &
           NLTECoeff              , &
           nc_copy%n_Predictors   , &
           nc_copy%n_Sensor_Angles, &
           nc_copy%n_Solar_Angles , &
           nc_copy%n_NLTE_Channels, &
           n_channels               )
    IF ( .NOT. NLTECoeff_Associated(NLTECoeff) ) RETURN
    
    
    ! Fill the new structure
    ! ...Copy over the non-channel related information
    NLTECoeff%Version              = nc_copy%Version
    NLTECoeff%Sensor_Id            = nc_copy%Sensor_Id
    NLTECoeff%WMO_Satellite_ID     = nc_copy%WMO_Satellite_ID
    NLTECoeff%WMO_Sensor_ID        = nc_copy%WMO_Sensor_ID   
    NLTECoeff%Upper_Plevel         = nc_copy%Upper_Plevel        
    NLTECoeff%Lower_Plevel         = nc_copy%Lower_Plevel        
    NLTECoeff%Min_Tm               = nc_copy%Min_Tm              
    NLTECoeff%Max_Tm               = nc_copy%Max_Tm              
    NLTECoeff%Mean_Tm              = nc_copy%Mean_Tm             
    NLTECoeff%Secant_Sensor_Zenith = nc_copy%Secant_Sensor_Zenith
    NLTECoeff%Secant_Solar_Zenith  = nc_copy%Secant_Solar_Zenith 
    ! ...Copy over the NLTE channel related information
    NLTECoeff%NLTE_Channel = nc_copy%NLTE_Channel
    NLTECoeff%C            = nc_copy%C
    ! ...Copy over the all-channel related information
    NLTECoeff%Sensor_Channel = Sensor_Channel

    
    ! Perform the channel reindexing
    i_nlte = 1
    Reindex_Loop: DO i = 1, n_channels
      IF ( NLTECoeff%Sensor_Channel(i) == NLTECoeff%NLTE_Channel(i_nlte) ) THEN
        NLTECoeff%Is_NLTE_Channel(i) = .TRUE.
        NLTECoeff%C_Index(i)         = i_nlte
        i_nlte = i_nlte + 1
        IF (i_nlte > NLTECoeff%n_NLTE_Channels) EXIT Reindex_Loop
      END IF
    END DO Reindex_Loop
    ! ...Unless ALL the NLTE channel were reindexed, restore the original structure
    n_nlte_channels = i_nlte - 1
    IF ( n_nlte_channels /= NLTECoeff%n_NLTE_Channels ) NLTECoeff = nc_copy

    
    ! Clean up
    CALL NLTECoeff_Destroy(nc_copy)
    
  END SUBROUTINE NLTECoeff_ChannelReindex



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
!       NLTECoeff_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two NLTECoeff objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = NLTECoeff_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two NLTECoeff objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       NLTECoeff_type
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

  ELEMENTAL FUNCTION NLTECoeff_Equal( x, y ) RESULT( is_equal )
    TYPE(NLTECoeff_type), INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( (.NOT. NLTECoeff_Associated(x)) .OR. &
         (.NOT. NLTECoeff_Associated(y))      ) RETURN

    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Predictors     /= y%n_Predictors    ) .OR. &
         (x%n_Sensor_Angles  /= y%n_Sensor_Angles ) .OR. &
         (x%n_Solar_Angles   /= y%n_Solar_Angles  ) .OR. &
         (x%n_NLTE_Channels  /= y%n_NLTE_Channels ) .OR. &
         (x%n_Channels       /= y%n_Channels      ) ) RETURN
    ! ...Scalars
    IF ( (x%Sensor_Id        /= y%Sensor_Id       ) .OR. &
         (x%WMO_Satellite_ID /= y%WMO_Satellite_ID) .OR. &
         (x%WMO_Sensor_ID    /= y%WMO_Sensor_ID   ) ) RETURN
    ! ...Arrays
    IF ( ALL(x%Sensor_Channel           ==    y%Sensor_Channel      ) .AND. &
         ALL(x%Upper_Plevel         .EqualTo. y%Upper_Plevel        ) .AND. &
         ALL(x%Lower_Plevel         .EqualTo. y%Lower_Plevel        ) .AND. &
         ALL(x%Min_Tm               .EqualTo. y%Min_Tm              ) .AND. &
         ALL(x%Max_Tm               .EqualTo. y%Max_Tm              ) .AND. &
         ALL(x%Mean_Tm              .EqualTo. y%Mean_Tm             ) .AND. &
         ALL(x%Secant_Sensor_Zenith .EqualTo. y%Secant_Sensor_Zenith) .AND. &
         ALL(x%Secant_Solar_Zenith  .EqualTo. y%Secant_Solar_Zenith ) .AND. &
         ALL(x%NLTE_Channel             ==    y%NLTE_Channel        ) .AND. &
         ALL(x%Is_NLTE_Channel        .EQV.   y%Is_NLTE_Channel     ) .AND. &
         ALL(x%C_Index                  ==    y%C_Index             ) .AND. &
         ALL(x%C                    .EqualTo. y%C                   ) ) &
      is_equal = .TRUE.

  END FUNCTION NLTECoeff_Equal


!--------------------------------------------------------------------------------
!
! NAME:
!       NLTECoeff_Reindex
!
! PURPOSE:
!       Subroutine to reindex the C_Index component of an NLTECoeff object after,
!       for example, subsetting or concatenation.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_Reindex( NLTECoeff )
!
! OBJECTS:
!       NLTECoeff:    NLTECoeff object which is to have its C_Index
!                     component reindexed.
!                     UNITS:      N/A
!                     TYPE:       NLTECoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE NLTECoeff_Reindex( NLTECoeff )
    TYPE(NLTECoeff_type), INTENT(IN OUT) :: NLTECoeff
    INTEGER :: i, j
    j = 1
    DO i = 1, NLTECoeff%n_Channels
      IF ( NLTECoeff%C_Index(i) > 0 ) THEN
        NLTECoeff%C_Index(i) = j
        j = j + 1
      END IF
    END DO
  END SUBROUTINE NLTECoeff_Reindex

END MODULE NLTECoeff_Define
