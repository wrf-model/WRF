!
! SpcCoeff_Define
!
! Module defining the SpcCoeff data structure and routines
! to manipulate them.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE SpcCoeff_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, &
                                   Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
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
  USE AntCorr_Define       , ONLY: AntCorr_type        , &
                                   Associated_AntCorr  , &
                                   Destroy_AntCorr     , &
                                   Allocate_AntCorr    , &
                                   Assign_AntCorr      , &
                                   Equal_AntCorr       , &
                                   Info_AntCorr        , &
                                   CheckRelease_AntCorr

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE

  ! Public types
  ! ------------
  PUBLIC :: SpcCoeff_type

  ! Public procedures
  ! -----------------
  PUBLIC :: Associated_SpcCoeff
  PUBLIC :: Destroy_SpcCoeff
  PUBLIC :: Allocate_SpcCoeff
  PUBLIC :: Assign_SpcCoeff
  PUBLIC :: Equal_SpcCoeff
  PUBLIC :: CheckRelease_SpcCoeff
  PUBLIC :: Info_SpcCoeff
  PUBLIC :: IsFlagSet_SpcCoeff, SetFlag_SpcCoeff, ClearFlag_SpcCoeff
  ! Use associated procedures
  PUBLIC :: Associated_AntCorr
  PUBLIC :: Destroy_AntCorr
  PUBLIC :: Allocate_AntCorr
  PUBLIC :: Assign_AntCorr
  PUBLIC :: Equal_AntCorr
  PUBLIC :: Info_AntCorr
  PUBLIC :: CheckRelease_AntCorr

  ! Public parameters
  ! -----------------
  ! Sensor Id defaults
  PUBLIC :: INVALID_WMO_SATELLITE_ID
  PUBLIC :: INVALID_WMO_SENSOR_ID
  ! Allowable sensor type values and names
  PUBLIC :: N_SENSOR_TYPES
  PUBLIC :: INVALID_SENSOR  
  PUBLIC :: MICROWAVE_SENSOR
  PUBLIC :: INFRARED_SENSOR 
  PUBLIC :: VISIBLE_SENSOR  
  PUBLIC :: ULTRAVIOLET_SENSOR  
  PUBLIC :: SENSOR_TYPE_NAME
  ! Allowable polarisation type values and names
  PUBLIC :: N_POLARIZATION_TYPES   
  PUBLIC :: INVALID_POLARIZATION   
  PUBLIC :: UNPOLARIZED            
  PUBLIC :: INTENSITY              
  PUBLIC :: FIRST_STOKES_COMPONENT 
  PUBLIC :: SECOND_STOKES_COMPONENT
  PUBLIC :: THIRD_STOKES_COMPONENT 
  PUBLIC :: FOURTH_STOKES_COMPONENT
  PUBLIC :: VL_POLARIZATION        
  PUBLIC :: HL_POLARIZATION        
  PUBLIC :: plus45L_POLARIZATION   
  PUBLIC :: minus45L_POLARIZATION  
  PUBLIC :: VL_MIXED_POLARIZATION  
  PUBLIC :: HL_MIXED_POLARIZATION  
  PUBLIC :: RC_POLARIZATION        
  PUBLIC :: LC_POLARIZATION        
  PUBLIC :: POLARIZATION_TYPE_NAME
  ! Current set of channel flags
  PUBLIC :: SOLAR_FLAG
  PUBLIC :: ZEEMAN_FLAG


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: SpcCoeff_Define.f90 3195 2009-02-13 23:02:57Z david.groff@noaa.gov $'
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  ! SpcCoeff invalid value
  INTEGER,      PARAMETER :: IP_INVALID = -1
  REAL(Double), PARAMETER :: FP_INVALID = -1.0_Double
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Sensor Id component string length
  INTEGER, PARAMETER :: SL = 20
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: SPCCOEFF_RELEASE = 7  ! This determines structure and file formats.
  INTEGER, PARAMETER :: SPCCOEFF_VERSION = 1  ! This is just the data version.
  ! ASCII codes for Version routine
  INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
  INTEGER, PARAMETER :: LINEFEED = 10
  ! The bit positions for the various channel flags
  INTEGER, PARAMETER :: SOLAR_FLAG  = 0
  INTEGER, PARAMETER :: ZEEMAN_FLAG = 1


  ! -----------------------------
  ! SpcCoeff data type definition
  ! -----------------------------
  TYPE :: SpcCoeff_type
    INTEGER :: n_Allocates = 0
    ! Release and version information
    INTEGER(Long) :: Release = SPCCOEFF_RELEASE
    INTEGER(Long) :: Version = SPCCOEFF_VERSION
    ! Dimensions
    INTEGER(Long) :: n_Channels = 0  ! L dimension
    ! Data prescence indicator(s)
    LOGICAL       :: AC_Present = .FALSE.
    ! Scalar components
    CHARACTER(SL) :: Sensor_Id        = ' '
    INTEGER(Long) :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER(Long) :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER(Long) :: Sensor_Type      = INVALID_SENSOR
    ! Channel data arrays
    INTEGER(Long), POINTER :: Sensor_Channel(:)             => NULL()  ! L
    INTEGER(Long), POINTER :: Polarization(:)               => NULL()  ! L
    INTEGER(Long), POINTER :: Channel_Flag(:)               => NULL()  ! L
    REAL(Double) , POINTER :: Frequency(:)                  => NULL()  ! L
    REAL(Double) , POINTER :: Wavenumber(:)                 => NULL()  ! L
    REAL(Double) , POINTER :: Planck_C1(:)                  => NULL()  ! L
    REAL(Double) , POINTER :: Planck_C2(:)                  => NULL()  ! L
    REAL(Double) , POINTER :: Band_C1(:)                    => NULL()  ! L
    REAL(Double) , POINTER :: Band_C2(:)                    => NULL()  ! L
    REAL(Double) , POINTER :: Cosmic_Background_Radiance(:) => NULL()  ! L
    REAL(Double) , POINTER :: Solar_Irradiance(:)           => NULL()  ! L
    ! Antenna correction structure
    TYPE(AntCorr_type) :: AC
  END TYPE SpcCoeff_type


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE SetFlag_SpcCoeff
    MODULE PROCEDURE SetFlag_Scalar
    MODULE PROCEDURE SetFlag_Rank1
  END INTERFACE SetFlag_SpcCoeff
  
  INTERFACE ClearFlag_SpcCoeff
    MODULE PROCEDURE ClearFlag_Scalar
    MODULE PROCEDURE ClearFlag_Rank1
  END INTERFACE ClearFlag_SpcCoeff
  

CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  FUNCTION IsFlagSet_SpcCoeff(Flags, Flag_Type) RESULT(Is_Set)
    INTEGER, INTENT(IN) :: Flags
    INTEGER, INTENT(IN) :: Flag_Type
    LOGICAL :: Is_Set
    Is_Set = BTEST(Flags,Flag_Type)
  END FUNCTION IsFlagSet_SpcCoeff

  
  SUBROUTINE SetFlag_Scalar(Flags, Flag_Type)
    INTEGER, INTENT(IN OUT) :: Flags
    INTEGER, INTENT(IN)     :: Flag_Type
    Flags = IBSET(Flags,Flag_Type)
  END SUBROUTINE SetFlag_Scalar
  
  SUBROUTINE SetFlag_Rank1(Flags, Flag_Type)
    INTEGER, INTENT(IN OUT) :: Flags(:)
    INTEGER, INTENT(IN)     :: Flag_Type
    INTEGER :: l
    DO l = 1, SIZE(Flags)
      CALL SetFlag_Scalar(Flags(l),Flag_Type)
    END DO
  END SUBROUTINE SetFlag_Rank1


  SUBROUTINE ClearFlag_Scalar(Flags, Flag_Type)
    INTEGER, INTENT(IN OUT) :: Flags
    INTEGER, INTENT(IN)     :: Flag_Type
    Flags = IBCLR(Flags,Flag_Type)
  END SUBROUTINE ClearFlag_Scalar
  
  SUBROUTINE ClearFlag_Rank1(Flags, Flag_Type)
    INTEGER, INTENT(IN OUT) :: Flags(:)
    INTEGER, INTENT(IN)     :: Flag_Type
    INTEGER :: l
    DO l = 1, SIZE(Flags)
      CALL ClearFlag_Scalar(Flags(l),Flag_Type)
    END DO
  END SUBROUTINE ClearFlag_Rank1
  

!--------------------------------------------------------------------------------
!
! NAME:
!       Associated_SpcCoeff
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       SpcCoeff structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_SpcCoeff( SpcCoeff         , &  ! Input
!                                                 ANY_Test=Any_Test  )  ! Optional input
!
! INPUT ARGUMENTS:
!       SpcCoeff:            SpcCoeff structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       SpcCoeff_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            SpcCoeff structure pointer members are associated.
!                            The default is to test if ALL the pointer members
!                            are associated.
!                            If ANY_Test = 0, test if ALL the pointer members
!                                             are associated.  (DEFAULT)
!                               ANY_Test = 1, test if ANY of the pointer members
!                                             are associated.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the SpcCoeff pointer members.
!                            .TRUE.  - if ALL the SpcCoeff pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the SpcCoeff pointer
!                                      members are associated.
!                            .FALSE. - some or all of the SpcCoeff pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_SpcCoeff( SpcCoeff, & ! Input
                                ANY_Test) & ! Optional input
                              RESULT( Association_Status )
    ! Arguments
    TYPE(SpcCoeff_type), INTENT(IN) :: SpcCoeff
    INTEGER,   OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test

    ! Set up
    ! ------
    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF

    ! Test the members that MUST be associated
    ! ----------------------------------------
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED( SpcCoeff%Sensor_Channel             ) .AND. &
           ASSOCIATED( SpcCoeff%Polarization               ) .AND. &
           ASSOCIATED( SpcCoeff%Channel_Flag               ) .AND. &
           ASSOCIATED( SpcCoeff%Frequency                  ) .AND. &
           ASSOCIATED( SpcCoeff%Wavenumber                 ) .AND. &
           ASSOCIATED( SpcCoeff%Planck_C1                  ) .AND. &
           ASSOCIATED( SpcCoeff%Planck_C2                  ) .AND. &
           ASSOCIATED( SpcCoeff%Band_C1                    ) .AND. &
           ASSOCIATED( SpcCoeff%Band_C2                    ) .AND. &
           ASSOCIATED( SpcCoeff%Cosmic_Background_Radiance ) .AND. &
           ASSOCIATED( SpcCoeff%Solar_Irradiance           )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( SpcCoeff%Sensor_Channel             ) .OR. &
           ASSOCIATED( SpcCoeff%Polarization               ) .OR. &
           ASSOCIATED( SpcCoeff%Channel_Flag               ) .OR. &
           ASSOCIATED( SpcCoeff%Frequency                  ) .OR. &
           ASSOCIATED( SpcCoeff%Wavenumber                 ) .OR. &
           ASSOCIATED( SpcCoeff%Planck_C1                  ) .OR. &
           ASSOCIATED( SpcCoeff%Planck_C2                  ) .OR. &
           ASSOCIATED( SpcCoeff%Band_C1                    ) .OR. &
           ASSOCIATED( SpcCoeff%Band_C2                    ) .OR. &
           ASSOCIATED( SpcCoeff%Cosmic_Background_Radiance ) .OR. &
           ASSOCIATED( SpcCoeff%Solar_Irradiance           )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF
    
    ! Test the members that MAY be associated
    ! ---------------------------------------
    ! Antenna correction
    IF ( SpcCoeff%AC_Present ) THEN
      IF ( ALL_Test ) THEN
        IF ( Association_Status .AND. &
             Associated_AntCorr(SpcCoeff%AC) ) THEN
          Association_Status = .TRUE.
        END IF
      ELSE
        IF ( Association_Status .OR. &
             Associated_AntCorr(SpcCoeff%AC) ) THEN
          Association_Status = .TRUE.
        END IF
      END IF
    END IF
    
  END FUNCTION Associated_SpcCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Destroy_SpcCoeff
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of SpcCoeff
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_SpcCoeff( SpcCoeff               , &  ! Output
!                                        RCS_Id     =RCS_Id     , &  ! Revision control
!                                        Message_Log=Message_Log  )  ! Error messaging
!
! OUTPUT ARGUMENTS:
!       SpcCoeff:     Re-initialized SpcCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       SpcCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to zero (0) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SpcCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Destroy_SpcCoeff( SpcCoeff   , &  ! Output
                             No_Clear   , &  ! Optional input
                             RCS_Id     , &  ! Revision control
                             Message_Log) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    TYPE(SpcCoeff_type)   , INTENT(IN OUT) :: SpcCoeff
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_SpcCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Clear
    INTEGER :: Destroy_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Reinitialise the dimensions
    SpcCoeff%n_Channels = 0

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_SpcCoeff( SpcCoeff )

    ! If ALL components are NOT associated, do nothing
    IF ( .NOT. Associated_SpcCoeff( SpcCoeff ) ) RETURN


    ! Deallocate the regular arrays components
    ! ----------------------------------------
    DEALLOCATE( SpcCoeff%Sensor_Channel            , &
                SpcCoeff%Polarization              , &
                SpcCoeff%Channel_Flag              , &
                SpcCoeff%Frequency                 , &
                SpcCoeff%Wavenumber                , &
                SpcCoeff%Planck_C1                 , &
                SpcCoeff%Planck_C2                 , &
                SpcCoeff%Band_C1                   , &
                SpcCoeff%Band_C2                   , &
                SpcCoeff%Cosmic_Background_Radiance, &
                SpcCoeff%Solar_Irradiance          , &
                STAT=Destroy_Status )
    IF ( Destroy_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error deallocating SpcCoeff components. STAT = ",i0)') &
                     Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      ! No return. Continue with deallocation
    END IF


    ! Destroy the Antenna Correction structure component
    ! --------------------------------------------------
    Destroy_Status = Destroy_AntCorr( SpcCoeff%AC, &
                                      No_Clear   =No_Clear, &
                                      Message_Log=Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying SpcCoeff Antenaa Correction structure(s).', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      ! No return. Continue with deallocation
    END IF
    

    ! Decrement and test allocation counter
    ! -------------------------------------
    SpcCoeff%n_Allocates = SpcCoeff%n_Allocates - 1
    IF ( SpcCoeff%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i0 )' ) &
                      SpcCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF
  END FUNCTION Destroy_SpcCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Allocate_SpcCoeff
! 
! PURPOSE:
!       Function to allocate the pointer members of a SpcCoeff
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_SpcCoeff( n_Channels             , &  ! Input
!                                         SpcCoeff               , &  ! Output
!                                         n_FOVs     =n_FOVs     , &  ! Optional Input
!                                         RCS_Id     =RCS_Id     , &  ! Revision control
!                                         Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:             Number of sensor channels.
!                               Must be > 0.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SpcCoeff:               SpcCoeff structure with allocated pointer
!                               members
!                               UNITS:      N/A
!                               TYPE:       SpcCoeff_type
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       n_FOVs:                 Number of sensor fields-of-view (FOVs). Used
!                               for microwave instrument antennae correction.
!                               If not present, or if present but == 0, the
!                               antenna correction element of the SpcCoeff
!                               is not allocated.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN)
!
!       Message_Log:            Character string specifying a filename in
!                               which any messages will be logged. If not
!                               specified, or if an error occurs opening the
!                               log file, the default action is to output
!                               messages to standard output.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER(*)
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:                 Character string containing the Revision
!                               Control System Id field for the module.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER(*)
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:           The return value is an integer defining the
!                               error status. The error codes are defined in
!                               the Message_Handler module.
!                               If == SUCCESS the structure pointer allocations
!                                             were successful
!                                  == FAILURE - an error occurred, or
!                                             - the structure internal allocation
!                                               counter is not equal to one (1)
!                                               upon exiting this function. This
!                                               value is incremented and decre-
!                                               mented for every structure
!                                               allocation and deallocation
!                                               respectively.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SpcCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Allocate_SpcCoeff( n_Channels , &  ! Input
                              SpcCoeff   , &  ! Output
                              n_FOVs     , &  ! Optional Input
                              RCS_Id     , &  ! Revision control
                              Message_Log) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    INTEGER               , INTENT(IN)     :: n_Channels
    TYPE(SpcCoeff_type)   , INTENT(IN OUT) :: SpcCoeff
    INTEGER     , OPTIONAL, INTENT(IN)     :: n_FOVs
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_SpcCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status
    LOGICAL :: Allocate_AC

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check dimension input
    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input N_CHANNELS must be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Antenna correction component is NOT allocated...
    Allocate_AC = .FALSE.
    ! ...unless the n_FOVs dimension is present and non-zero
    IF ( PRESENT( n_FOVs ) ) THEN
      IF ( n_FOVs > 0 ) Allocate_AC = .TRUE.
    END IF
    
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_SpcCoeff( SpcCoeff, ANY_Test = SET ) ) THEN
      Error_Status = Destroy_SpcCoeff( SpcCoeff, &
                                       No_Clear = SET, &
                                       Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating SpcCoeff pointer members.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF

    END IF


    ! Allocate the intrinsic type arrays
    ! ----------------------------------
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
              STAT = Allocate_Status                             )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error allocating SpcCoeff data arrays. STAT = ",i0)') &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign the dimensions and initialise arrays
    SpcCoeff%n_Channels = n_Channels

    SpcCoeff%Sensor_Channel             = 0
    SpcCoeff%Polarization               = INVALID_POLARIZATION
    SpcCoeff%Channel_Flag               = 0
    SpcCoeff%Frequency                  = FP_INVALID
    SpcCoeff%Wavenumber                 = FP_INVALID
    SpcCoeff%Planck_C1                  = FP_INVALID
    SpcCoeff%Planck_C2                  = FP_INVALID
    SpcCoeff%Band_C1                    = FP_INVALID
    SpcCoeff%Band_C2                    = FP_INVALID
    SpcCoeff%Cosmic_Background_Radiance = FP_INVALID
    SpcCoeff%Solar_Irradiance           = FP_INVALID


    ! Allocate the antenna correction component
    ! -----------------------------------------
    IF ( Allocate_AC ) THEN
    
      Error_Status = Allocate_AntCorr( n_FOVs, n_Channels, &
                                       SpcCoeff%AC, &
                                       Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error allocating AC structure.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
      
      ! Set the data indicator in the main structure
      SpcCoeff%AC_Present = .TRUE.

    END IF


    ! Increment and test allocation counter
    ! -------------------------------------
    SpcCoeff%n_Allocates = SpcCoeff%n_Allocates + 1
    IF ( SpcCoeff%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      SpcCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Allocate_SpcCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Assign_SpcCoeff
!
! PURPOSE:
!       Function to copy valid SpcCoeff structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_SpcCoeff( SpcCoeff_in            , &  ! Input
!                                       SpcCoeff_out           , &  ! Output
!                                       RCS_Id     =RCS_Id     , &  ! Revision control
!                                       Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SpcCoeff_in:   SpcCoeff structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SpcCoeff_out:  Copy of the input structure, SpcCoeff_in.
!                      UNITS:      N/A
!                      TYPE:       Same as SpcCoeff_in
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure assignment was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SpcCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Assign_SpcCoeff( SpcCoeff_in , &  ! Input
                            SpcCoeff_out, &  ! Output
                            RCS_Id      , &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    TYPE(SpcCoeff_type)   , INTENT(IN)     :: SpcCoeff_in
    TYPE(SpcCoeff_type)   , INTENT(IN OUT) :: SpcCoeff_out
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_SpcCoeff'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! ALL *input* pointers must be associated
    ! BUT the antenna correction AC component
    ! may not be.
    IF ( .NOT. Associated_SpcCoeff( SpcCoeff_In ) ) THEN
      Error_Status = Destroy_SpcCoeff( SpcCoeff_Out, &
                                       Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output SpcCoeff components.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
      END IF
      RETURN
    END IF

   
    ! Allocate the structure
    ! ----------------------
    Error_Status = Allocate_SpcCoeff( SpcCoeff_in%n_Channels, &
                                      SpcCoeff_out, &
                                      n_FOVs=SpcCoeff_in%AC%n_FOVs, &
                                      Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output SpcCoeff arrays.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign intrinsic data types
    ! ---------------------------
    SpcCoeff_out%Release = SpcCoeff_in%Release
    SpcCoeff_out%Version = SpcCoeff_in%Version

    SpcCoeff_out%Sensor_Id                  = SpcCoeff_in%Sensor_Id
    SpcCoeff_out%Sensor_Type                = SpcCoeff_in%Sensor_Type
    SpcCoeff_out%WMO_Satellite_ID           = SpcCoeff_in%WMO_Satellite_ID
    SpcCoeff_out%WMO_Sensor_ID              = SpcCoeff_in%WMO_Sensor_ID
    SpcCoeff_out%Sensor_Channel             = SpcCoeff_in%Sensor_Channel
    SpcCoeff_out%Polarization               = SpcCoeff_in%Polarization
    SpcCoeff_out%Channel_Flag               = SpcCoeff_in%Channel_Flag
    SpcCoeff_out%Frequency                  = SpcCoeff_in%Frequency
    SpcCoeff_out%Wavenumber                 = SpcCoeff_in%Wavenumber
    SpcCoeff_out%Planck_C1                  = SpcCoeff_in%Planck_C1
    SpcCoeff_out%Planck_C2                  = SpcCoeff_in%Planck_C2
    SpcCoeff_out%Band_C1                    = SpcCoeff_in%Band_C1
    SpcCoeff_out%Band_C2                    = SpcCoeff_in%Band_C2
    SpcCoeff_out%Cosmic_Background_Radiance = SpcCoeff_in%Cosmic_Background_Radiance
    SpcCoeff_out%Solar_Irradiance           = SpcCoeff_in%Solar_Irradiance


    ! Assign Antenna Correction structures
    ! ------------------------------------
    IF ( SpcCoeff_in%AC_Present ) THEN
      Error_Status = Assign_AntCorr( SpcCoeff_in%AC, &
                                     SpcCoeff_out%AC, &
                                     Message_Log=Message_Log)
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error copying AC structure for '//&
                              TRIM(SpcCoeff_in%Sensor_Id), &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF
    
  END FUNCTION Assign_SpcCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Equal_SpcCoeff
!
! PURPOSE:
!       Function to test if two SpcCoeff structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_SpcCoeff( SpcCoeff_LHS           , &  ! Input
!                                      SpcCoeff_RHS           , &  ! Input
!                                      ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                      Check_All  =Check_All  , &  ! Optional input
!                                      RCS_Id     =RCS_Id     , &  ! Revision control
!                                      Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SpcCoeff_LHS:  SpcCoeff structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( SpcCoeff_LHS == SpcCoeff_RHS ).
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       SpcCoeff_RHS:  SpcCoeff structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( SpcCoeff_LHS == SpcCoeff_RHS ).
!                      UNITS:      N/A
!                      TYPE:       Same as SpcCoeff_LHS
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:     Unit of data precision used to scale the floating
!                      point comparison. ULP stands for "Unit in the Last Place,"
!                      the smallest possible increment or decrement that can be
!                      made using a machine's floating point arithmetic.
!                      Value must be positive - if a negative value is supplied,
!                      the absolute value is used. If not specified, the default
!                      value is 1.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Check_All:     Set this argument to check ALL the floating point
!                      channel data of the SpcCoeff structures. The default
!                      action is return with a FAILURE status as soon as
!                      any difference is found. This optional argument can
!                      be used to get a listing of ALL the differences
!                      between data in SpcCoeff structures.
!                      If == 0, Return with FAILURE status as soon as
!                               ANY difference is found  *DEFAULT*
!                         == 1, Set FAILURE status if ANY difference is
!                               found, but continue to check ALL data.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structures were equal
!                         == FAILURE - an error occurred, or
!                                    - the structures were different.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Equal_SpcCoeff( SpcCoeff_LHS, &  ! Input
                           SpcCoeff_RHS, &  ! Input
                           ULP_Scale   , &  ! Optional input
                           Check_All   , &  ! Optional input
                           RCS_Id      , &  ! Revision control
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    TYPE(SpcCoeff_type)   , INTENT(IN)  :: SpcCoeff_LHS
    TYPE(SpcCoeff_type)   , INTENT(IN)  :: SpcCoeff_RHS
    INTEGER     , OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER     , OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_SpcCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: l

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Default precision is a single unit in last place
    ULP = 1
    ! ... unless the ULP_Scale argument is set and positive
    IF ( PRESENT( ULP_Scale ) ) THEN
      IF ( ULP_Scale > 0 ) ULP = ULP_Scale
    END IF

    ! Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! ...unless the Check_All argument is set
    IF ( PRESENT( Check_All ) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF

    ! Check the structure association status
    IF ( .NOT. Associated_SpcCoeff( SpcCoeff_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SpcCoeff_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_SpcCoeff( SpcCoeff_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SpcCoeff_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check Release/Version info
    ! --------------------------
    IF ( ( SpcCoeff_LHS%Release /= SpcCoeff_RHS%Release ) .OR. &
         ( SpcCoeff_LHS%Version /= SpcCoeff_RHS%Version )      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Release/Version numbers are different : ", &
                        &i2, ".", i2.2, " vs. ", i2, ".", i2.2 )' ) &
                      SpcCoeff_LHS%Release, SpcCoeff_LHS%Version, &
                      SpcCoeff_RHS%Release, SpcCoeff_RHS%Version
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! Check dimensions
    ! ----------------
    IF ( SpcCoeff_LHS%n_Channels /= SpcCoeff_RHS%n_Channels ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Channels dimensions are different : ", &
                        &i0, " vs. ", i0 )' ) &
                      SpcCoeff_LHS%n_Channels, SpcCoeff_RHS%n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Compare the values
    ! ------------------
    ! The Sensor_ID
    IF ( SpcCoeff_LHS%Sensor_Id /= SpcCoeff_RHS%Sensor_Id ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Sensor_ID values are different, ", &
                        &a, " vs. ", a )' ) &
                      TRIM( SpcCoeff_LHS%Sensor_Id), &
                      TRIM( SpcCoeff_RHS%Sensor_Id)
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    ! The Sensor_Type
    IF ( SpcCoeff_LHS%Sensor_Type /= SpcCoeff_RHS%Sensor_Type ) THEN
      WRITE( Message,'("Sensor types are different, ", &
                       &i0,"(",a,") vs. ", i0,"(",a,")")' ) &
                      SpcCoeff_LHS%Sensor_Type, &
                      TRIM(SENSOR_TYPE_NAME(SpcCoeff_LHS%Sensor_Type)), &
                      SpcCoeff_RHS%Sensor_Type, &
                      TRIM(SENSOR_TYPE_NAME(SpcCoeff_RHS%Sensor_Type))
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The WMO Satellite ID
    IF ( SpcCoeff_LHS%WMO_Satellite_ID /= SpcCoeff_RHS%WMO_Satellite_ID ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("WMO_Satellite_ID values are different, ",i0,&
                      &" vs. ",i0 )' ) &
                      SpcCoeff_LHS%WMO_Satellite_ID, &
                      SpcCoeff_RHS%WMO_Satellite_ID
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The WMO Sensor ID
    IF ( SpcCoeff_LHS%WMO_Sensor_ID /= SpcCoeff_RHS%WMO_Sensor_ID ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("WMO_Sensor_ID values are different, ",i0,&
                      &" vs. ",i0)' ) &
                      SpcCoeff_LHS%WMO_Sensor_ID, &
                      SpcCoeff_RHS%WMO_Sensor_ID
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The Sensor_Channel
    DO l = 1, SpcCoeff_RHS%n_Channels
      IF ( SpcCoeff_LHS%Sensor_Channel(l) /= SpcCoeff_RHS%Sensor_Channel(l) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Sensor_Channel values are different, ",i0,&
                        &" vs. ",i0,", for channel index # ",i0)' ) &
                        SpcCoeff_LHS%Sensor_Channel(l), &
                        SpcCoeff_RHS%Sensor_Channel(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The Polarization indicator flag
    DO l = 1, SpcCoeff_RHS%n_Channels
      IF ( SpcCoeff_LHS%Polarization(l) /= SpcCoeff_RHS%Polarization(l) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Polarization values are different, ",i0,&
                        &" vs. ",i0,", for channel index # ", i0)' ) &
                        SpcCoeff_LHS%Polarization(l), &
                        SpcCoeff_RHS%Polarization(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    
    ! The Channel_Flag
    DO l = 1, SpcCoeff_RHS%n_Channels
      IF ( SpcCoeff_LHS%Channel_Flag(l) /= SpcCoeff_RHS%Channel_Flag(l) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Channel_Flag values are different, ",b32.32,&
                        &" vs. ",b32.32,", for channel index # ", i0)' ) &
                        SpcCoeff_LHS%Channel_Flag(l), &
                        SpcCoeff_RHS%Channel_Flag(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    
    ! The central Frequency
    DO l = 1, SpcCoeff_RHS%n_Channels
      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Frequency(l), &
                                  SpcCoeff_RHS%Frequency(l), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Frequency', l, &
                                     SpcCoeff_LHS%Frequency(l), &
                                     SpcCoeff_RHS%Frequency(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The central Wavenumber
    DO l = 1, SpcCoeff_RHS%n_Channels
      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Wavenumber(l), &
                                  SpcCoeff_RHS%Wavenumber(l), &
                                  ULP = ULP                   ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Wavenumber', l, &
                                     SpcCoeff_LHS%Wavenumber(l), &
                                     SpcCoeff_RHS%Wavenumber(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The first Planck function constant, Planck_C1
    DO l = 1, SpcCoeff_RHS%n_Channels
      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Planck_C1(l), &
                                  SpcCoeff_RHS%Planck_C1(l), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Planck_C1', l, &
                                     SpcCoeff_LHS%Planck_C1(l), &
                                     SpcCoeff_RHS%Planck_C1(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The second Planck function constant, Planck_C2
    DO l = 1, SpcCoeff_RHS%n_Channels
      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Planck_C2(l), &
                                  SpcCoeff_RHS%Planck_C2(l), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Planck_C2', l, &
                                     SpcCoeff_LHS%Planck_C2(l), &
                                     SpcCoeff_RHS%Planck_C2(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    
    ! The band correction offset, Band_C1
    DO l = 1, SpcCoeff_RHS%n_Channels
      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Band_C1(l), &
                                  SpcCoeff_RHS%Band_C1(l), &
                                  ULP = ULP                ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Band_C1', l, &
                                     SpcCoeff_LHS%Band_C1(l), &
                                     SpcCoeff_RHS%Band_C1(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    
    ! The band correction slope, Band_C2
    DO l = 1, SpcCoeff_RHS%n_Channels
      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Band_C2(l), &
                                  SpcCoeff_RHS%Band_C2(l), &
                                  ULP = ULP                ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Band_C2', l, &
                                     SpcCoeff_LHS%Band_C2(l), &
                                     SpcCoeff_RHS%Band_C2(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The Cosmic_Background_Radiance
    DO l = 1, SpcCoeff_RHS%n_Channels
      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Cosmic_Background_Radiance(l), &
                                  SpcCoeff_RHS%Cosmic_Background_Radiance(l), &
                                  ULP = ULP                                   ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Cosmic_Background_Radiance', l, &
                                     SpcCoeff_LHS%Cosmic_Background_Radiance(l), &
                                     SpcCoeff_RHS%Cosmic_Background_Radiance(l) )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The Kurucz TOA Solar_Irradiance source function
    DO l = 1, SpcCoeff_RHS%n_Channels
      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Solar_Irradiance(l), &
                                  SpcCoeff_RHS%Solar_Irradiance(l), &
                                  ULP = ULP                         ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Solar_Irradiance', l, &
                                     SpcCoeff_LHS%Solar_Irradiance(l), &
                                     SpcCoeff_RHS%Solar_Irradiance(l) )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO


    ! Check the Antenna Correction structures
    ! ---------------------------------------
    IF ( SpcCoeff_LHS%AC_Present .NEQV. SpcCoeff_RHS%AC_Present ) THEN
      Error_Status = FAILURE
      WRITE( Message, '("AC_Present indicators different : ", 2(l1,1x))' ) &
                      SpcCoeff_LHS%AC_Present, SpcCoeff_RHS%AC_Present
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    IF ( SpcCoeff_LHS%AC_Present ) THEN
      Error_Status = Equal_AntCorr( SpcCoeff_LHS%AC, &
                                    SpcCoeff_RHS%AC, &
                                    Message_Log=Message_Log)
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error comparing AC structure for '//&
                              TRIM(SpcCoeff_LHS%Sensor_Id), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END IF
    
  END FUNCTION Equal_SpcCoeff


!----------------------------------------------------------------------------------
!
! NAME:
!       CheckRelease_SpcCoeff
!
! PURPOSE:
!       Function to check the SpcCoeff Release value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckRelease_SpcCoeff( SpcCoeff               , &  ! Input
!                                             RCS_Id     =RCS_Id     , &  ! Revision control
!                                             Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SpcCoeff:      SpcCoeff structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structure Release value is valid.
!                         == FAILURE the structure Release value is NOT valid
!                                    and either a data file file or software
!                                    update is required.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION CheckRelease_SpcCoeff( SpcCoeff   , &  ! Input
                                  RCS_Id     , &  ! Revision control
                                  Message_Log) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    TYPE(SpcCoeff_type)   , INTENT(IN)  :: SpcCoeff
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckRelease_SpcCoeff'
    ! Local variables
    CHARACTER(256) :: Message

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check the release
    ! -----------------
    ! Check that release is not too old
    IF ( SpcCoeff%Release < SPCCOEFF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An SpcCoeff data update is needed. ", &
                        &"SpcCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      SpcCoeff%Release, SPCCOEFF_RELEASE
      CALL Display_Message( TRIM(Routine_Name), &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check that release is not too new
    IF ( SpcCoeff%Release > SPCCOEFF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A SpcCoeff software update is needed. ", &
                        &"SpcCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      SpcCoeff%Release, SPCCOEFF_RELEASE
      CALL Display_Message( TRIM(Routine_Name), &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_SpcCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Info_SpcCoeff
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the SpcCoeff data structure.
!
! CALLING SEQUENCE:
!       CALL Info_SpcCoeff( SpcCoeff     , &  ! Input
!                           Info         , &  ! Output
!                           RCS_Id=RCS_Id  )  ! Revision control
!
! INPUT ARGUMENTS:
!       SpcCoeff:      Filled SpcCoeff structure.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed SpcCoeff data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!--------------------------------------------------------------------------------

  SUBROUTINE Info_SpcCoeff( SpcCoeff, &  ! Input
                            Info    , &  ! Output
                            RCS_Id    )  ! Revision control
    ! Arguments
    TYPE(SpcCoeff_type)   , INTENT(IN)  :: SpcCoeff
    CHARACTER(*)          , INTENT(OUT) :: Info
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    ! Local variables
    CHARACTER(2000) :: LongString
    CHARACTER(2000) :: ACString

    ! Set up
    ! ------
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Write the required data to the local string
    ! -------------------------------------------
    WRITE( LongString, '( a,1x,"SpcCoeff RELEASE.VERSION: ",i2,".",i2.2,2x,&
                          &"N_CHANNELS=",i0)' ) &
                        ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                        SpcCoeff%Release, SpcCoeff%Version, &
                        SpcCoeff%n_Channels
                        
    ! Add the antenna correction structure info if present
    ! ----------------------------------------------------
    IF ( SpcCoeff%AC_Present ) THEN
      CALL Info_AntCorr( SpcCoeff%AC, ACString )
      LongString = TRIM(LongString)//TRIM(ACString)
    END IF
    
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_SpcCoeff


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       Clear_SpcCoeff
!
! PURPOSE:
!       Subroutine to clear the scalar members of an SpcCoeff structure.
!
! CALLING SEQUENCE:
!       CALL Clear_SpcCoeff( SpcCoeff ) ! Output
!
! OUTPUT ARGUMENTS:
!       SpcCoeff:    SpcCoeff structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       SpcCoeff_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output SpcCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_SpcCoeff( SpcCoeff )
    TYPE(SpcCoeff_type), INTENT(IN OUT) :: SpcCoeff
    SpcCoeff%Release = SPCCOEFF_RELEASE
    SpcCoeff%Version = SPCCOEFF_VERSION
    SpcCoeff%AC_Present = .FALSE.
    SpcCoeff%Sensor_Id        = ' '
    SpcCoeff%Sensor_Type      = INVALID_SENSOR
    SpcCoeff%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    SpcCoeff%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
  END SUBROUTINE Clear_SpcCoeff


!----------------------------------------------------------------------------------
!
! NAME:
!       Construct_Message
!
! PURPOSE:
!       Function to to construct a standard message when a floating point
!       number comparison fails in the Equal_SpcCoeff() functions.
!
! CALLING SEQUENCE:
!       Message = Construct_Message( Label, i, x1, x2, Node )
!
! INPUT ARGUMENTS:
!       Label:       Character string containing the name of the data
!                    that was compared.
!                    UNITS:      N/A
!                    TYPE:       CHARACTER(*)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
!       i:           The index (channel or node) in the SpcCoeff structure
!                    at which the floating point comparison failed.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
!       x1, x2:      The numbers for which the floating point comparison
!                    failed. These are used to determine the best floating
!                    point format with which to output the numbers in the
!                    message.
!                    UNITS:      N/A
!                    TYPE:       REAL(Double)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Message:     Character string containing the constructed error
!                    that is passed into the Display_Message subroutine.
!                    UNITS:      N/A
!                    TYPE:       CHARACTER(*)
!                    DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION Construct_Message( Label, i, x1, x2 ) RESULT( Message )
    ! Arguments
    CHARACTER(*), INTENT(IN) :: Label
    INTEGER,      INTENT(IN) :: i
    REAL(Double), INTENT(IN) :: x1, x2
    ! Function result
    CHARACTER(256) :: Message
    ! Local parameters
    INTEGER,      PARAMETER :: WIDTH = 20
    REAL(Double), PARAMETER :: HUNDRED = 100.0_Double
    ! Local variables
    CHARACTER(6) :: Real_Format
    CHARACTER(256) :: Format_String
    INTEGER :: n
    REAL(Double) :: dx

    ! Determine the print format for the floating
    ! point numbers based on their magnitudes
    ! -------------------------------------------
    n = WIDTH - INT( MAX( LOG10(x1), LOG10(x2), 0.0_Double ) ) - 2
    WRITE( Real_Format, FMT='("f",i2.2,".",i2.2)' ) WIDTH, n

    ! Construct the format string used to create the message
    ! ------------------------------------------------------
    Format_String = '( "'//TRIM(Label)//' difference, ", '//Real_Format//&
                    ', " vs. ", '//Real_Format//', "(", es9.2, "% diff), '//&
                    'channel index # ", i4 )'

    ! Create the message
    ! ------------------
    dx = HUNDRED * ( x1-x2 ) / x1
    WRITE( Message, FMT=TRIM(Format_String) ) x1, x2, dx, i

  END FUNCTION Construct_Message

END MODULE SpcCoeff_Define
