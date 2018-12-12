!
! ODSSU_Define
!
! Module defining the ODSSU (Tau coefficient data structure for the SSU sensors).
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, NOAA/NESDIS, Oct. 6, 2009
!
!                       Yong Chen, NOAA/NESDIS, 06-Nov-2009
!                       yong.chen@noaa.gov
!

MODULE ODSSU_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: Long, Double
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE ODAS_Define,           ONLY: ODAS_type   , &
                                   Destroy_ODAS, &
                                   Associated_ODAS, ODAS_ALGORITHM
  USE ODPS_Define,           ONLY: ODPS_type   , &
                                   Destroy_ODPS, &
                                   Associated_ODPS, ODPS_ALGORITHM
  USE CRTM_Parameters,       ONLY: ODSSU_ALGORITHM
  
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE

  ! Public types
  ! ------------
  PUBLIC :: ODSSU_type
  ! The Global unique algorithm ID
  PUBLIC :: ODSSU_ALGORITHM
  ! public routines
  PUBLIC :: Associated_ODSSU
  PUBLIC :: Destroy_ODSSU
  PUBLIC :: Allocate_ODSSU
  PUBLIC :: CheckRelease_ODSSU
  PUBLIC :: CheckAlgorithm_ODSSU
  PUBLIC :: Info_ODSSU
  
  PUBLIC :: ODAS_ALGORITHM 
  PUBLIC :: ODPS_ALGORITHM 

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: ODSSU_Define.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  ! ODSSU invalid values
  INTEGER,      PARAMETER :: IP_INVALID = -1
  REAL(Double), PARAMETER :: FP_INVALID = -1.0_Double
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! String lengths
  INTEGER, PARAMETER :: SL = 20   ! Sensor Id
  INTEGER, PARAMETER :: ML = 256  ! Messages
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: ODSSU_RELEASE = 6  ! This determines structure and file formats.
  INTEGER, PARAMETER :: ODSSU_VERSION = 4  ! This is just the data version.
  ! The optical depth algorithm name
  CHARACTER(*), PARAMETER :: ODSSU_ALGORITHM_NAME = 'ODSSU'
  ! ASCII codes for Version routine
  INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
  INTEGER, PARAMETER :: LINEFEED = 10
  ! Invalid sensor ids
  INTEGER, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  ! The instrument types
  INTEGER, PARAMETER :: INVALID_SENSOR     = 0
  ! instrument type
  INTEGER, PARAMETER :: INFRARED_SENSOR    = 2

  ! -------------------------
  ! ODSSU data type definition
  ! -------------------------
  TYPE :: ODSSU_type

    ! This structure is specific for the Stratospheric Sensor Unit (SSU). To consider
    ! the variation of cell CO2 pressure, a set of absorption coefficients are derived
    ! corresponding a set of cell CO2 pressures. 
    ! The simultaneous cell CO2 pressure can be interpolated from the array data
    ! Ref_Time and Ref_CellPressure. 
    INTEGER :: n_Allocates = 0
    ! Release and version information
    INTEGER(Long) :: Release = ODSSU_RELEASE
    INTEGER(Long) :: Version = ODSSU_VERSION

    INTEGER(Long) :: Algorithm = ODSSU_ALGORITHM
    INTEGER(Long) :: subAlgorithm = 0  ! refer to the series algorithm ID 1 for ODAS, 2 for ODPS
    ! Array dimensions
    INTEGER(Long) :: n_Channels          = 0  ! L
    INTEGER(Long) :: n_Absorbers         = 0  ! J
    INTEGER(Long) :: n_TC_CellPressures  = 0  ! M
    INTEGER(Long) :: n_Ref_CellPressures = 0  ! N
    ! Scalar components
    CHARACTER(SL) :: Sensor_Id        = ' '
    INTEGER(Long) :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER(Long) :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER(Long) :: Sensor_Type      = INFRARED_SENSOR  ! fixed for SSUs
    ! The actual sensor channel numbers
    INTEGER(Long), POINTER, DIMENSION(:)   :: Sensor_Channel     => NULL()  ! L           
    ! The absorber ID and absorber space values
    INTEGER(Long), POINTER, DIMENSION(:)   :: Absorber_ID        => NULL()  ! J           

    ! cell CO2 pressures used in training coefficients, part of TauCoefficients file
    REAL(Double),  POINTER, DIMENSION(:,:) :: TC_CellPressure    => NULL()  ! M x L
    REAL(Double),  POINTER, DIMENSION(:)   :: Ref_Time           => NULL()  ! N
    REAL(Double),  POINTER, DIMENSION(:,:) :: Ref_CellPressure   => NULL()  ! N x L

    ! Tau coefficient series at different cell pressures    
    TYPE(ODAS_type), POINTER, DIMENSION(:) :: ODAS                 => NULL()  ! M  
    TYPE(ODPS_type), POINTER, DIMENSION(:) :: ODPS                 => NULL()  ! M  
    
  END TYPE ODSSU_type   

CONTAINS

!--------------------------------------------------------------------------------
!
! NAME:
!       Associated_ODSSU
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       ODSSU structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_ODSSU(ODSSU             ,&  ! Input
!                                             ANY_Test=Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       ODSSU:       ODSSU structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       ODSSU_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    ODSSU structure pointer members are associated.
!                    The default is to test if ALL the pointer members
!                    are associated.
!                    If ANY_Test = 0, test if ALL the pointer members
!                                     are associated.  (DEFAULT)
!                       ANY_Test = 1, test if ANY of the pointer members
!                                     are associated.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the ODSSU pointer members.
!                            .TRUE.  - if ALL the ODSSU pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the ODSSU pointer
!                                      members are associated.
!                            .FALSE. - some or all of the ODSSU pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_ODSSU(ODSSU   , & ! Input
                            ANY_Test) & ! Optional input
                          RESULT( Association_Status )
    ! Arguments
    TYPE(ODSSU_type) , INTENT(IN) :: ODSSU
    INTEGER, OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test
    INTEGER :: i

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
      IF ( ASSOCIATED( ODSSU%Sensor_Channel    ) .AND. &
           ASSOCIATED( ODSSU%Absorber_ID       ) .AND. &
           ASSOCIATED( ODSSU%TC_CellPressure   ) .AND. &
           ASSOCIATED( ODSSU%Ref_Time          ) .AND. &
           ASSOCIATED( ODSSU%Ref_CellPressure  ) ) THEN
         Association_Status = .TRUE.  
      ENDIF                                                                             
      IF(ODSSU%subAlgorithm == ODAS_ALGORITHM) THEN                                     
        Association_Status = Association_Status .AND. ASSOCIATED( ODSSU%ODAS )          
        DO i = 1, ODSSU%n_TC_CellPressures                                              
         Association_Status = Association_Status .AND. Associated_ODAS( ODSSU%ODAS(i) ) 
        END DO                                                                          
      ENDIF                                                                             

      IF(ODSSU%subAlgorithm == ODPS_ALGORITHM) THEN                                     
        Association_Status = Association_Status .AND. ASSOCIATED( ODSSU%ODPS )          
        DO i = 1, ODSSU%n_TC_CellPressures                                              
         Association_Status = Association_Status .AND. Associated_ODPS( ODSSU%ODPS(i) ) 
        END DO                                                                          
      ENDIF                                                                             
    ELSE
      IF ( ASSOCIATED( ODSSU%Sensor_Channel    ) .OR. &
           ASSOCIATED( ODSSU%Absorber_ID       ) .OR. &
           ASSOCIATED( ODSSU%TC_CellPressure   ) .OR. &
           ASSOCIATED( ODSSU%Ref_Time          ) .OR. &
           ASSOCIATED( ODSSU%Ref_CellPressure  ) ) THEN
         Association_Status = .TRUE.
      END IF
      IF(ODSSU%subAlgorithm == ODAS_ALGORITHM) THEN                                     
        Association_Status = Association_Status .OR. ASSOCIATED( ODSSU%ODAS )          
        DO i = 1, ODSSU%n_TC_CellPressures                                              
         Association_Status = Association_Status .OR. Associated_ODAS( ODSSU%ODAS(i) ) 
        END DO                                                                          
      ENDIF                                                                             

      IF(ODSSU%subAlgorithm == ODPS_ALGORITHM) THEN                                     
        Association_Status = Association_Status .OR. ASSOCIATED( ODSSU%ODPS )          
        DO i = 1, ODSSU%n_TC_CellPressures                                              
         Association_Status = Association_Status .OR. Associated_ODPS( ODSSU%ODPS(i) ) 
        END DO                                                                          
      ENDIF                                                                             
    END IF

  END FUNCTION Associated_ODSSU
     
!------------------------------------------------------------------------------
!
! NAME:
!       Destroy_ODSSU
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of ODSSU
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_ODSSU(ODSSU                  , &  ! Output
!                                    RCS_Id     =RCS_Id     , &  ! Revision control
!                                    Message_Log=Message_Log  )  ! Error messaging
!
! OUTPUT ARGUMENTS:
!       ODSSU:        Re-initialized ODSSU structure.
!                     UNITS:      N/A
!                     TYPE:       ODSSU_type
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
!                     ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
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
!       Note the INTENT on the output ODSSU argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Destroy_ODSSU(ODSSU      , &  ! Output
                         No_Clear   , &  ! Optional input
                         RCS_Id     , &  ! Revision control
                         Message_Log) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(ODSSU_type)      , INTENT(IN OUT) :: ODSSU
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_ODSSU'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status1, Allocate_Status2
    INTEGER :: i

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF

    ! If ALL components are NOT associated, do nothing
    IF ( .NOT. Associated_ODSSU( ODSSU ) ) RETURN

    ! Destroy ODx 
    IF(ODSSU%subAlgorithm == ODAS_ALGORITHM) THEN  
     DO i = 1, ODSSU%n_TC_CellPressures
       Error_Status = Destroy_ODAS( ODSSU%ODAS(i), &
                                    Message_Log = Message_Log)
       IF( Error_Status /= SUCCESS )THEN
         CALL Display_Message( ROUTINE_NAME,    &
                               "Error deallocating ODAS for ODSSU", &
                               Error_Status,    &
                               Message_Log=Message_Log )
         RETURN
       END IF
     END DO
    ENDIF
    IF(ODSSU%subAlgorithm == ODPS_ALGORITHM) THEN  
     DO i = 1, ODSSU%n_TC_CellPressures
       Error_Status = Destroy_ODPS( ODSSU%ODPS(i), &
                                    Message_Log = Message_Log)
       IF( Error_Status /= SUCCESS )THEN
         CALL Display_Message( ROUTINE_NAME,    &
                               "Error deallocating ODPS for ODSSU", &
                               Error_Status,    &
                               Message_Log=Message_Log )
         RETURN
       END IF
     END DO
    ENDIF
                       
    ! Deallocate the regular arrays components
    ! ----------------------------------------
    DEALLOCATE( ODSSU%Sensor_Channel  , &
                ODSSU%Absorber_ID     , &
                ODSSU%TC_CellPressure , &
                ODSSU%Ref_Time        , &
                ODSSU%Ref_CellPressure, &
                STAT=Allocate_Status1 )

    IF(ODSSU%subAlgorithm == ODAS_ALGORITHM) THEN  
       DEALLOCATE(ODSSU%ODAS,  STAT=Allocate_Status2) 
    ENDIF
    IF(ODSSU%subAlgorithm == ODPS_ALGORITHM) THEN  
       DEALLOCATE(ODSSU%ODPS,  STAT=Allocate_Status2) 
    ENDIF

    IF ( Allocate_Status1 /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error deallocating ODSSU components 1. STAT = ",i0)' ) &
                     Allocate_Status1
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF
    IF ( Allocate_Status2 /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error deallocating ODSSU components 2. STAT = ",i0)' ) &
                     Allocate_Status2
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

    ! Clear the scalar members
    IF ( Clear ) CALL Clear_ODSSU( ODSSU )

    ! Reinitialise the dimensions
    ODSSU%n_Channels          = 0
    ODSSU%n_TC_CellPressures  = 0
    ODSSU%n_Ref_CellPressures = 0


    ! Decrement and test allocation counter
    ! -------------------------------------
    ODSSU%n_Allocates = ODSSU%n_Allocates - 1
    IF ( ODSSU%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Allocation counter /= 0, Value = ",i0)' ) &
                      ODSSU%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF
  END FUNCTION Destroy_ODSSU

!------------------------------------------------------------------------------
!
! NAME:
!       Allocate_ODSSU
! 
! PURPOSE:
!       Function to allocate the pointer members of the ODSSU
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_ODSSU(n_Absorbers            , &  ! Input
!                                     n_Channels             , &  ! Input
!                                     n_TC_CellPressures     , &  ! Input
!                                     n_Ref_CellPressures    , &  ! Input
!                                     ODSSU                  , &  ! Output
!                                     RCS_Id     =RCS_Id     , &  ! Revision control
!                                     Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!
!       n_Absorbers:  Number of absorbers dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Channels:   Number of channels dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! n_TC_CellPressures: Number of TC cell pressure dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! n_Ref_CellPressures: Number of refference cell pressure dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ODSSU:        ODSSU structure with allocated
!                     pointer members
!                     UNITS:      N/A
!                     TYPE:       ODSSU_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in
!                     which any messages will be logged. If not
!                     specified, or if an error occurs opening
!                     the log file, the default action is to
!                     output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to one (1) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       The pointer members of the ODSSU structure in the ODSSU structure will not be
!       allocated in this routine.
!
!       Note the INTENT on the output ODSSU argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Allocate_ODSSU(n_Absorbers        , &  ! Input
                          n_Channels         , &  ! Input
                          n_TC_CellPressures , &  ! Input 
                          n_Ref_CellPressures, &  ! Input 
                          ODSSU              , &  ! Output           
                          RCS_Id             , &  ! Revision control 
                          Message_Log ) &         ! Error messaging  
                        RESULT( Error_Status )
    ! Arguments
    INTEGER               , INTENT(IN)     :: n_Absorbers
    INTEGER               , INTENT(IN)     :: n_Channels
    INTEGER               , INTENT(IN)     :: n_TC_CellPressures
    INTEGER               , INTENT(IN)     :: n_Ref_CellPressures
    TYPE(ODSSU_type)      , INTENT(IN OUT) :: ODSSU
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_ODSSU'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status1, Allocate_Status2

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check dimension input
    IF ( n_Absorbers          < 1 .OR. &
         n_Channels           < 1 .OR. &
         n_TC_CellPressures   < 1 .OR. &
         n_Ref_CellPressures  < 1    ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODSSU dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_ODSSU( ODSSU, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_ODSSU(ODSSU, &
                                   No_Clear=SET, &
                                   Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating ODSSU prior to reallocation.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Allocate the data arrays
    ! ------------------------
    ALLOCATE( ODSSU%TC_CellPressure( n_TC_CellPressures, n_Channels)  , &
              ODSSU%Ref_Time( n_Ref_CellPressures )                   , &
              ODSSU%Ref_CellPressure( n_Ref_CellPressures, n_Channels), &
              ODSSU%Sensor_Channel( n_Channels )                      , &
              ODSSU%Absorber_ID( n_Absorbers )                        , &
              STAT = Allocate_Status1 )
    IF(ODSSU%subAlgorithm == ODAS_ALGORITHM) THEN  
       ALLOCATE(ODSSU%ODAS( n_TC_CellPressures ),  STAT=Allocate_Status2) 
    ENDIF
    IF(ODSSU%subAlgorithm == ODPS_ALGORITHM) THEN  
       ALLOCATE(ODSSU%ODPS( n_TC_CellPressures ),  STAT=Allocate_Status2) 
    ENDIF

    IF ( Allocate_Status1 /= 0 .OR. Allocate_Status2 /= 0) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error allocating ODSSU data arrays. STAT = ",i0)' ) &
                     Allocate_Status1
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign the dimensions and initialise arrays
    ODSSU%n_Absorbers         = n_Absorbers
    ODSSU%n_Channels          = n_Channels
    ODSSU%n_TC_CellPressures  = n_TC_CellPressures
    ODSSU%n_Ref_CellPressures = n_Ref_CellPressures

    ODSSU%Sensor_Channel    = 0
    ODSSU%Absorber_ID       = IP_INVALID
    ODSSU%TC_CellPressure   = FP_INVALID
    ODSSU%Ref_Time          = FP_INVALID
    ODSSU%Ref_CellPressure  = FP_INVALID


    ! Increment and test allocation counter
    ! -------------------------------------
    ODSSU%n_Allocates = ODSSU%n_Allocates + 1
    IF ( ODSSU%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Allocation counter /= 1, Value = ",i0)' ) &
                     ODSSU%n_Allocates
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Allocate_ODSSU
     

!----------------------------------------------------------------------------------
!
! NAME:
!       CheckRelease_ODSSU
!
! PURPOSE:
!       Function to check the ODSSU Release value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckRelease_ODSSU( ODSSU                  , &  ! Input
!                                          RCS_Id     = RCS_Id    , &  ! Revision control
!                                          Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODSSU:         ODSSU structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       ODSSU_type
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
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
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

  FUNCTION CheckRelease_ODSSU(ODSSU      , &  ! Input
                              RCS_Id     , &  ! Revision control
                              Message_Log) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    TYPE(ODSSU_type)      , INTENT(IN)  :: ODSSU
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckRelease_ODSSU'
    ! Local variables
    CHARACTER(ML) :: Message

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Check the release
    ! -----------------
    ! Check that release is not too old
    IF ( ODSSU%Release < ODSSU_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An ODSSU data update is needed. ", &
                        &"ODSSU release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      ODSSU%Release, ODSSU_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check that release is not too new
    IF ( ODSSU%Release > ODSSU_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An ODSSU software update is needed. ", &
                        &"ODSSU release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      ODSSU%Release, ODSSU_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_ODSSU


!----------------------------------------------------------------------------------
!
! NAME:
!       CheckAlgorithm_ODSSU
!
! PURPOSE:
!       Function to check the ODSSU Algorithm value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckAlgorithm_ODSSU(ODSSU                  , &  ! Input
!                                           RCS_Id     = RCS_Id    , &  ! Revision control
!                                           Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODSSU:         ODSSU structure for which the Algorithm member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       ODSSU_type
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
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structure Algorithm value is valid.
!                         == FAILURE the structure Algorithm value is NOT valid.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION CheckAlgorithm_ODSSU(ODSSU      , &  ! Input
                                RCS_Id     , &  ! Revision control
                                Message_Log) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    TYPE(ODSSU_type)      , INTENT(IN)  :: ODSSU
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckAlgorithm_ODSSU'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Check the algorithm ID
    ! ----------------------
    IF ( ODSSU%Algorithm /= ODSSU_ALGORITHM ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'The ODSSU Algorithm ID check failed. '//&
                            'The data structure is not an ODSSU structure', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckAlgorithm_ODSSU

!------------------------------------------------------------------------------
!
! NAME:
!       Info_ODSSU
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the ODSSU data structure.
!
! CALLING SEQUENCE:
!       CALL Info_ODSSU(ODSSU        , &  ! Input
!                       Info         , &  ! Output
!                       RCS_Id=RCS_Id  )  ! Revision control
!
! INPUT ARGUMENTS:
!       ODSSU:         Filled ODSSU structure.
!                      UNITS:      N/A
!                      TYPE:       ODSSU_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed ODSSU data structure.
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
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Info_ODSSU( ODSSU , &  ! Input
                         Info  , &  ! Output
                         RCS_Id  )  ! Revision control
    ! Arguments
    TYPE(ODSSU_type)      , INTENT(IN)  :: ODSSU
    CHARACTER(*),           INTENT(OUT) :: Info
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    ! Local variables
    CHARACTER(2000) :: LongString

    ! Set up
    ! ------
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Write the required data to the local string
    ! -------------------------------------------
    WRITE( LongString,'( a,3x,"ODSSU RELEASE.VERSION: ",i2,".",i2.2,2x,&
                      &"SUBALGORITHM=",i2,2x,&
                      &"N_ABSORBERS=",i2,2x,&
                      &"N_CHANNELS=",i0,2x, &
                      &"N_TC_CELLPRESSURES=",i2,2x, &
                      &"N_REF_CELLPRESSURES=",i0)' ) &
                      ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                      ODSSU%Release, ODSSU%Version, &
                      ODSSU%subAlgorithm, &
                      ODSSU%n_Absorbers, &
                      ODSSU%n_Channels, &
                      ODSSU%n_TC_CellPressures, &
                      ODSSU%n_Ref_CellPressures

    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_ODSSU
  


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
!       Clear_ODSSU
!
! PURPOSE:
!       Subroutine to clear the scalar members of a ODSSU structure.
!
! CALLING SEQUENCE:
!       CALL Clear_ODSSU( ODSSU ) ! Output
!
! OUTPUT ARGUMENTS:
!       ODSSU:       ODSSU structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       ODSSU_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_ODSSU( ODSSU )
    TYPE(ODSSU_type), INTENT(IN OUT) :: ODSSU
    ODSSU%Release   = ODSSU_RELEASE
    ODSSU%Version   = ODSSU_VERSION
    ODSSU%Algorithm = ODSSU_ALGORITHM
    ODSSU%subAlgorithm = 0
    ODSSU%Sensor_Id        = ' '
    ODSSU%Sensor_Type      = INVALID_SENSOR
    ODSSU%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    ODSSU%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
  END SUBROUTINE Clear_ODSSU

END MODULE ODSSU_Define
