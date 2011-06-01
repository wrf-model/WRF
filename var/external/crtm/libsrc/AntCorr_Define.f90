!
! AntCorr_Define
!
! Module defining the AntCorr data structure and containing routines to 
! manipulate it.
!
! CREATION HISTORY:
!       Written by:  Paul van Delst, CIMSS/SSEC, 08-Jun-2007 
!                    paul.vandelst@ssec.wisc.edu
!

MODULE AntCorr_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds           , ONLY: fp, Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data structure definition
  PUBLIC :: AntCorr_type
  ! Structure procedures
  PUBLIC :: Associated_AntCorr
  PUBLIC :: Destroy_AntCorr
  PUBLIC :: Allocate_AntCorr
  PUBLIC :: Assign_AntCorr
  PUBLIC :: Equal_AntCorr
  PUBLIC :: Info_AntCorr
  PUBLIC :: CheckRelease_AntCorr
    

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: AntCorr_Define.f90 2299 2008-08-11 18:18:34Z paul.vandelst@noaa.gov $'
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
  ! Keyword set flag
  INTEGER, PARAMETER :: SET = 1
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512
  ! String length
  INTEGER, PARAMETER :: SL = 20
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: ANTCORR_RELEASE = 1
  INTEGER, PARAMETER :: ANTCORR_VERSION = 1
  ! Sensor Id default values
  INTEGER, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047

  ! -----------------------
  ! Derived type definition
  ! -----------------------
  TYPE :: AntCorr_type
    INTEGER :: n_Allocates=0
    ! Release and version information
    INTEGER(Long) :: Release = ANTCORR_RELEASE
    INTEGER(Long) :: Version = ANTCORR_VERSION
    ! Dimensions
    INTEGER(Long) :: n_FOVs     = 0  ! N
    INTEGER(Long) :: n_Channels = 0  ! L
    ! Scalars
    CHARACTER(SL) :: Sensor_Id        = ' '                     
    INTEGER(Long) :: WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    INTEGER(Long) :: WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    ! Arrays
    INTEGER(Long), POINTER :: Sensor_Channel(:) => NULL()  ! L
    REAL(Double) , POINTER :: A_earth(:,:)      => NULL()  ! N x L
    REAL(Double) , POINTER :: A_space(:,:)      => NULL()  ! N x L
    REAL(Double) , POINTER :: A_platform(:,:)   => NULL()  ! N x L
  END TYPE AntCorr_type


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
!       Associated_AntCorr
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       AntCorr structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_AntCorr( AntCorr          , &  ! Input
!                                                ANY_Test=Any_Test  )  ! Optional input
!
! INPUT ARGUMENTS:
!       AntCorr:             AntCorr structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       AntCorr_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            AntCorr structure pointer members are associated.
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
!                            association status of the AntCorr pointer members.
!                            .TRUE.  - if ALL the AntCorr pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the AntCorr pointer
!                                      members are associated.
!                            .FALSE. - some or all of the AntCorr pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_AntCorr( AntCorr , &  ! Input          
                               ANY_Test) &  ! Optional input 
                             RESULT(Association_Status)      
    ! Arguments
    TYPE(AntCorr_type), INTENT(IN) :: AntCorr
    INTEGER, OPTIONAL , INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test
    
    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == 1 ) ALL_Test = .FALSE.
    END IF
    
    ! Test the structure associations    
    Association_Status = .FALSE.
    IF (ALL_Test) THEN
      IF (ASSOCIATED(AntCorr%Sensor_Channel) .AND. &
          ASSOCIATED(AntCorr%A_earth       ) .AND. &
          ASSOCIATED(AntCorr%A_space       ) .AND. &
          ASSOCIATED(AntCorr%A_platform    )) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF (ASSOCIATED(AntCorr%Sensor_Channel) .OR. &
          ASSOCIATED(AntCorr%A_earth       ) .OR. &
          ASSOCIATED(AntCorr%A_space       ) .OR. &
          ASSOCIATED(AntCorr%A_platform    )) THEN
        Association_Status = .TRUE.
      END IF
    END IF
  END FUNCTION Associated_AntCorr


!--------------------------------------------------------------------------------
!
! NAME:
!       Destroy_AntCorr
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of AntCorr
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_AntCorr( AntCorr                , &  ! Output
!                                       RCS_Id     =RCS_Id     , &  ! Revision control
!                                       Message_Log=Message_Log  )  ! Error messaging
!
! OUTPUT ARGUMENTS:
!       AntCorr:      Re-initialized AntCorr structure.
!                     UNITS:      N/A
!                     TYPE:       AntCorr_type
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
!       Note the INTENT on the output AntCorr argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Destroy_AntCorr( AntCorr    , &  ! Output
                            No_Clear   , &  ! Optional input
                            RCS_Id     , &  ! Revision control
                            Message_Log) &  ! Error messaging
                          RESULT(Error_Status)
    ! Arguments
    TYPE(AntCorr_type)    , INTENT(IN OUT) :: AntCorr
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_AntCorr'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! Reset the dimension indicators
    AntCorr%n_FOVs     = 0
    AntCorr%n_Channels = 0
    
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == 1 ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_AntCorr(AntCorr)
    
    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_AntCorr(AntCorr) ) RETURN
    
    
    ! Deallocate the pointer members
    ! ------------------------------
    DEALLOCATE( AntCorr%Sensor_Channel, &
                AntCorr%A_earth       , &
                AntCorr%A_space       , &
                AntCorr%A_platform    , &
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '("Error deallocating AntCorr. STAT = ",i0)') &
                      Allocate_Status
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Decrement and test allocation counter
    ! -------------------------------------
    AntCorr%n_Allocates = AntCorr%n_Allocates - 1
    IF ( AntCorr%n_Allocates /= 0 ) THEN
      WRITE( Message, '("Allocation counter /= 0, Value = ",i0)') &
                      AntCorr%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
  END FUNCTION Destroy_AntCorr


!--------------------------------------------------------------------------------
!
! NAME:
!       Allocate_AntCorr
! 
! PURPOSE:
!       Function to allocate the pointer members of a AntCorr data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_AntCorr( n_FOVs                 , &  ! Input
!                                        n_Channels             , &  ! Input
!                                        AntCorr                , &  ! Output
!                                        RCS_Id     =RCS_Id     , &  ! Revision control
!                                        Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_FOVs:                 Number of sensor fields-of-view (FOVs).
!                               Must be > 0.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN)
!
!       n_Channels:             Number of sensor channels.
!                               Must be > 0.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       AntCorr:                AntCorr structure with allocated pointer
!                               members
!                               UNITS:      N/A
!                               TYPE:       AntCorr_type
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL INPUT ARGUMENTS:
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
!       Note the INTENT on the output AntCorr argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Allocate_AntCorr( n_FOVs     , &  ! Input            
                             n_Channels , &  ! Input            
                             AntCorr    , &  ! Output           
                             RCS_Id     , &  ! Revision control 
                             Message_Log) &  ! Error messaging  
                           RESULT( Error_Status )               
    ! Arguments
    INTEGER               , INTENT(IN)     :: n_FOVs
    INTEGER               , INTENT(IN)     :: n_Channels
    TYPE(AntCorr_type)    , INTENT(IN OUT) :: AntCorr
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_AntCorr'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! Check dimensions
    IF (n_FOVs < 1 .OR. n_Channels < 1) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input AntCorr dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_AntCorr( AntCorr, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_AntCorr( AntCorr, &               
                                      No_Clear=SET, &            
                                      Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating AntCorr prior to allocation.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    
    ! Perform the pointer allocation
    ! ------------------------------
    ALLOCATE( AntCorr%Sensor_Channel(1:n_Channels), &
              AntCorr%A_earth(1:n_FOVs,1:n_Channels), &
              AntCorr%A_space(1:n_FOVs,1:n_Channels), &
              AntCorr%A_platform(1:n_FOVs,1:n_Channels), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE(Message,'("Error allocating AntCorr data arrays. STAT = ",i0)') &
                    Allocate_Status
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    

    ! Assign the dimensions
    ! ---------------------
    AntCorr%n_FOVs     = n_FOVs
    AntCorr%n_Channels = n_Channels


    ! Initialise the arrays
    ! ---------------------
    AntCorr%A_earth    = ONE
    AntCorr%A_space    = ZERO
    AntCorr%A_platform = ZERO


    ! Increment and test the allocation counter
    ! -----------------------------------------
    AntCorr%n_Allocates = AntCorr%n_Allocates + 1
    IF ( AntCorr%n_Allocates /= 1 ) THEN
      WRITE( Message, '("Allocation counter /= 1, Value = ",i0)') &
                      AntCorr%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
  END FUNCTION Allocate_AntCorr
  
  
!--------------------------------------------------------------------------------
!
! NAME:
!       Assign_AntCorr
!
! PURPOSE:
!       Function to copy valid AntCorr structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_AntCorr( AntCorr_in             , &  ! Input
!                                      AntCorr_out            , &  ! Output
!                                      RCS_Id     =RCS_Id     , &  ! Revision control
!                                      Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AntCorr_in:    AntCorr structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       AntCorr_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       AntCorr_out:   Copy of the input structure, AntCorr_in.
!                      UNITS:      N/A
!                      TYPE:       Same as AntCorr_in
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
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structure assignment was successful
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output AntCorr argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Assign_AntCorr( AntCorr_in , &  ! Input
                           AntCorr_out, &  ! Output
                           RCS_Id     , &  ! Revision control
                           Message_Log) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    TYPE(AntCorr_type)    , INTENT(IN)     :: AntCorr_in
    TYPE(AntCorr_type)    , INTENT(IN OUT) :: AntCorr_out
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_AntCorr'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_AntCorr( AntCorr_in ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT AntCorr_in pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    
    ! Allocate data arrays
    ! --------------------
    Error_Status = Allocate_AntCorr( AntCorr_in%n_FOVs, &
                                     AntCorr_in%n_Channels, &
                                     AntCorr_out, &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output structure', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    
    ! Assign non-dimension scalar members
    ! -----------------------------------
    AntCorr_out%Release          = AntCorr_in%Release
    AntCorr_out%Version          = AntCorr_in%Version
    AntCorr_out%Sensor_Id        = AntCorr_in%Sensor_Id
    AntCorr_out%WMO_Satellite_Id = AntCorr_in%WMO_Satellite_Id
    AntCorr_out%WMO_Sensor_Id    = AntCorr_in%WMO_Sensor_Id
    
    ! Copy array data
    ! ---------------
    AntCorr_out%Sensor_Channel = AntCorr_in%Sensor_Channel
    AntCorr_out%A_earth        = AntCorr_in%A_earth
    AntCorr_out%A_space        = AntCorr_in%A_space
    AntCorr_out%A_platform     = AntCorr_in%A_platform
    
  END FUNCTION Assign_AntCorr


!--------------------------------------------------------------------------------
!
! NAME:
!       Equal_AntCorr
!
! PURPOSE:
!       Function to test if two AntCorr structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_AntCorr( AntCorr_LHS            , &  ! Input
!                                     AntCorr_RHS            , &  ! Input
!                                     ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                     Check_All  =Check_All  , &  ! Optional input
!                                     RCS_Id     =RCS_Id     , &  ! Revision control
!                                     Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AntCorr_LHS:   AntCorr structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( AntCorr_LHS == AntCorr_RHS ).
!                      UNITS:      N/A
!                      TYPE:       AntCorr_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       AntCorr_RHS:   AntCorr structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( AntCorr_LHS == AntCorr_RHS ).
!                      UNITS:      N/A
!                      TYPE:       Same as AntCorr_LHS
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
!                      channel data of the AntCorr structures. The default
!                      action is return with a FAILURE status as soon as
!                      any difference is found. This optional argument can
!                      be used to get a listing of ALL the differences
!                      between data in AntCorr structures.
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

  FUNCTION Equal_AntCorr( AntCorr_LHS, &  ! Input
                          AntCorr_RHS, &  ! Input
                          ULP_Scale  , &  ! Optional input
                          Check_All  , &  ! Optional input
                          RCS_Id     , &  ! Revision control
                          Message_Log) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(AntCorr_type)    , INTENT(IN)  :: AntCorr_LHS
    TYPE(AntCorr_type)    , INTENT(IN)  :: AntCorr_RHS
    INTEGER,      OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER     , OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_AntCorr'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: n, l

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
    IF ( .NOT. Associated_AntCorr( AntCorr_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT AntCorr_LHS pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_AntCorr( AntCorr_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT AntCorr_RHS pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Check dimensions
    IF (AntCorr_LHS%n_FOVs     /= AntCorr_RHS%n_FOVs     .OR. &
        AntCorr_LHS%n_Channels /= AntCorr_RHS%n_Channels      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check the scalar components
    ! ---------------------------
    IF ( AntCorr_LHS%Sensor_Id /= AntCorr_RHS%Sensor_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'AntCorr Sensor_Id values are different; >'//&
                            TRIM(AntCorr_LHS%Sensor_Id)//'< vs >'//&
                            TRIM(AntCorr_RHS%Sensor_Id)//'<', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( AntCorr_LHS%WMO_Satellite_Id /= AntCorr_RHS%WMO_Satellite_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'AntCorr scalar component WMO_Satellite_Id values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( AntCorr_LHS%WMO_Sensor_Id /= AntCorr_RHS%WMO_Sensor_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'AntCorr scalar component WMO_Sensor_Id values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    
    ! Check the array components
    ! --------------------------
    DO l = 1, AntCorr_LHS%n_Channels
      IF ( AntCorr_LHS%Sensor_Channel(l) /= AntCorr_RHS%Sensor_Channel(l) ) THEN
        WRITE(Message,'("AntCorr array component Sensor_Channel values ",&
                        &"are different at index (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO n = 1, AntCorr_LHS%n_Channels
      DO l = 1, AntCorr_LHS%n_FOVs
        IF ( .NOT. Compare_Float( AntCorr_LHS%A_earth(l,n), &
                                  AntCorr_RHS%A_earth(l,n), &
                                  ULP=ULP ) ) THEN
          WRITE(Message,'("AntCorr array component A_earth values ",&
                          &"are different at indices (",2(1x,i0),")")') &
                          l, n
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM(Message), &
                                Error_Status, &
                                Message_Log=Message_Log )
          IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO
    DO n = 1, AntCorr_LHS%n_Channels
      DO l = 1, AntCorr_LHS%n_FOVs
        IF ( .NOT. Compare_Float( AntCorr_LHS%A_space(l,n), &
                                  AntCorr_RHS%A_space(l,n), &
                                  ULP=ULP ) ) THEN
          WRITE(Message,'("AntCorr array component A_space values ",&
                          &"are different at indices (",2(1x,i0),")")') &
                          l, n
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM(Message), &
                                Error_Status, &
                                Message_Log=Message_Log )
          IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO
    DO n = 1, AntCorr_LHS%n_Channels
      DO l = 1, AntCorr_LHS%n_FOVs
        IF ( .NOT. Compare_Float( AntCorr_LHS%A_platform(l,n), &
                                  AntCorr_RHS%A_platform(l,n), &
                                  ULP=ULP ) ) THEN
          WRITE(Message,'("AntCorr array component A_platform values ",&
                          &"are different at indices (",2(1x,i0),")")') &
                          l, n
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM(Message), &
                                Error_Status, &
                                Message_Log=Message_Log )
          IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO
    
  END FUNCTION Equal_AntCorr


!----------------------------------------------------------------------------------
!
! NAME:
!       CheckRelease_AntCorr
!
! PURPOSE:
!       Function to check the AntCorr Release value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckRelease_AntCorr( AntCorr                , &  ! Input
!                                            RCS_Id     =RCS_Id     , &  ! Revision control
!                                            Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AntCorr:       AntCorr structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       AntCorr_type
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

  FUNCTION CheckRelease_AntCorr( AntCorr    , &  ! Input
                                 RCS_Id     , &  ! Revision control
                                 Message_Log) &  ! Error messaging
                               RESULT( Error_Status )
    ! Arguments
    TYPE(AntCorr_type)    , INTENT(IN)  :: AntCorr
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckRelease_AntCorr'
    ! Local variables
    CHARACTER(ML) :: Message

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check release is not too old
    ! ----------------------------
    IF ( AntCorr%Release < ANTCORR_RELEASE ) THEN
      WRITE( Message, '( "An AntCorr data update is needed. ", &
                        &"AntCorr release is ", i0, &
                        &". Valid release is ",i0,"." )' ) &
                      AntCorr%Release, ANTCORR_RELEASE
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check release is not too new
    ! ----------------------------
    IF ( AntCorr%Release > ANTCORR_RELEASE ) THEN
      WRITE( Message, '( "An AntCorr software update is needed. ", &
                        &"AntCorr release is ", i0, &
                        &". Valid release is ",i0,"." )' ) &
                      AntCorr%Release, ANTCORR_RELEASE
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_AntCorr


!--------------------------------------------------------------------------------
!
! NAME:
!       Info_AntCorr
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the AntCorr data structure.
!
! CALLING SEQUENCE:
!       CALL Info_AntCorr( AntCorr      , &  ! Input
!                          Info         , &  ! Output
!                          RCS_Id=RCS_Id  )  ! Revision control
!
! INPUT ARGUMENTS:
!       AntCorr:       Filled AntCorr structure.
!                      UNITS:      N/A
!                      TYPE:       AntCorr_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed AntCorr data structure.
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

  SUBROUTINE Info_AntCorr( AntCorr, &  ! Input
                           Info   , &  ! Output
                           RCS_Id   )  ! Revision control
    ! Arguments
    TYPE(AntCorr_type)    , INTENT(IN)  :: AntCorr
    CHARACTER(*)          , INTENT(OUT) :: Info
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(256) :: FmtString
    CHARACTER(512) :: LongString

    ! Set up
    ! ------
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Write the required info to the local string
    ! -------------------------------------------
    FmtString='(a,1x,a,1x,"AntCorr RELEASE.VERSION: ",i2,".",i2.2,2x,&
               &"N_FOVS=",i0,2x,&
               &"N_CHANNELS=",i0)'
    WRITE(LongString, FMT=FmtString) &
          ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
          TRIM(AntCorr%Sensor_ID), &
          AntCorr%Release, AntCorr%Version, &
          AntCorr%n_FOVs, &
          AntCorr%n_Channels

    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_AntCorr


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  SUBROUTINE Clear_AntCorr(AntCorr)
    TYPE(AntCorr_type), INTENT(IN OUT) :: AntCorr
    AntCorr%Release = ANTCORR_RELEASE
    AntCorr%Version = ANTCORR_VERSION
    AntCorr%Sensor_Id        = ' '
    AntCorr%WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    AntCorr%WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
  END SUBROUTINE Clear_AntCorr

END MODULE AntCorr_Define
