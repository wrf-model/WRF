!
! ODPS_Define
!
! Module defining the ODPS (Optical Depth, Pressure Space) data structure and
! containing routines to manipulate it.
!
!
! CREATION HISTORY:
!       Modified by:    Yong Han, JCSDA, NOAA/NESDIS 20-Jun-2008
!                       Based on Paul van Delst's framework
!

MODULE ODPS_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: Long, Single, fp
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE Sort_Utility,          ONLY: InsertionSort
  USE CRTM_Parameters,       ONLY: ODPS_ALGORITHM
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE

  ! Public types
  ! ------------
  PUBLIC :: ODPS_type

  ! Public procedures
  ! -----------------
  PUBLIC :: Associated_ODPS
  PUBLIC :: Destroy_ODPS
  PUBLIC :: Allocate_ODPS
  PUBLIC :: Allocate_ODPS_OPTRAN
  PUBLIC :: Assign_ODPS
  PUBLIC :: Concatenate_Channel_ODPS
  PUBLIC :: Concatenate_Absorber_ODPS
  PUBLIC :: Equal_ODPS
  PUBLIC :: CheckRelease_ODPS
  PUBLIC :: CheckAlgorithm_ODPS
  PUBLIC :: Info_ODPS
  
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
  PUBLIC :: SENSOR_TYPE_NAME
  ! The Global unique algorithm ID
  PUBLIC :: ODPS_ALGORITHM

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id:  $'
  ! ODPS invalid values
  INTEGER,      PARAMETER :: IP_INVALID = -1
  REAL(fp),     PARAMETER :: FP_INVALID = -1.0_fp
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! String lengths
  INTEGER, PARAMETER :: SL = 20   ! Sensor Id
  INTEGER, PARAMETER :: ML = 256  ! Messages
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: ODPS_RELEASE = 2  ! This determines structure and file formats.
  INTEGER, PARAMETER :: ODPS_VERSION = 1  ! This is just the data version.
  ! The optical depth algorithm name
  CHARACTER(*), PARAMETER :: ODPS_ALGORITHM_NAME = 'ODPS'
  ! ASCII codes for Version routine
  INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
  INTEGER, PARAMETER :: LINEFEED = 10
  ! Invalid sensor ids
  INTEGER, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  ! The instrument types
  INTEGER, PARAMETER :: N_SENSOR_TYPES     = 4
  INTEGER, PARAMETER :: INVALID_SENSOR     = 0
  INTEGER, PARAMETER :: MICROWAVE_SENSOR   = 1
  INTEGER, PARAMETER :: INFRARED_SENSOR    = 2
  INTEGER, PARAMETER :: VISIBLE_SENSOR     = 3
  INTEGER, PARAMETER :: ULTRAVIOLET_SENSOR = 4
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_SENSOR_TYPES ) :: &
    SENSOR_TYPE_NAME = (/ 'Invalid    ', &
                          'Microwave  ', &
                          'Infrared   ', &
                          'Visible    ', &
                          'Ultraviolet' /)
  ! number of predictors used to compute optran absorption coefficients
  INTEGER,         PARAMETER :: N_PREDICTOR_USED_OPTRAN = 6
  INTEGER, PUBLIC, PARAMETER :: SIGNIFICANCE_OPTRAN = 1

  ! -------------------------
  ! ODPS data type definition
  ! -------------------------
  TYPE :: ODPS_type
    INTEGER :: n_Allocates = 0
    ! Release and version information
    INTEGER(Long) :: Release = ODPS_RELEASE
    INTEGER(Long) :: Version = ODPS_VERSION
    ! Algorithm identifer
    INTEGER(Long) :: Algorithm = ODPS_ALGORITHM
    ! Dimensions
    INTEGER(Long) :: n_Layers     = 0    ! Iorder
    INTEGER(Long) :: n_Components = 0    ! J  - Tau component dimension
    INTEGER(Long) :: n_Absorbers  = 0    ! Jm - (Molecular) absorber dimension
    INTEGER(Long) :: n_Channels   = 0    ! L
    INTEGER(Long) :: n_Coeffs     = 0    ! Iuse

    ! Dimensions for OPTRAN component
    INTEGER(Long) :: n_OPIndex  = N_PREDICTOR_USED_OPTRAN    ! OI, should not be changed
    INTEGER(Long) :: n_OCoeffs  = 0    ! OC

    !-------------------
    ! Scalar components
    !-------------------
    ! Group ID. TCs in the same group have the same dimensions: 
    ! n_Components and n_Absorbers.
    INTEGER(Long) :: Group_Index     = 0
    ! Sensor/Satellite IDs and type
    CHARACTER(SL) :: Sensor_Id        = ' '
    INTEGER(Long) :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER(Long) :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER(Long) :: Sensor_Type      = INVALID_SENSOR

    ! Reference pressures at the layer boundaries
    REAL(fp), POINTER   :: Ref_Level_Pressure(:) => NULL()  ! 0:K
    ! Reference layer (mean) pressure and temperature
    REAL(fp), POINTER   :: Ref_Pressure(:)       => NULL()  ! K
    REAL(fp), POINTER   :: Ref_Temperature(:)    => NULL()  ! K

    ! Reference molecular content profile. The sequence of the molecules in the Jm dimension
    ! must be consistent with that of the Absorber_ID array
    REAL(fp), POINTER   :: Ref_Absorber(:,:)     => NULL()  ! K x Jm
    ! Training set molecular content ranges
    REAL(fp), POINTER   :: Min_Absorber(:,:)     => NULL()  ! K x Jm
    REAL(fp), POINTER   :: Max_Absorber(:,:)     => NULL()  ! K x Jm

    ! The actual sensor channel numbers
    INTEGER(Long), POINTER  :: Sensor_Channel(:) => NULL()  ! L     
    ! The Tau component ID 
    INTEGER(Long), POINTER  :: Component_ID(:)   => NULL()  ! J     
    ! Molecular IDs (variable absorbers):
    INTEGER(Long), POINTER  :: Absorber_ID(:)    => NULL()  ! Jm    

    !---------------------------------------------------------------------------
    !  The array C contains the Tau coefficient. It is structured
    !  with Pos_Index and n_Predictors, as the following,
    !    For channel l and component j,
    !      Pos_Index(j, l) is the starting position in array C for that 
    !            channel and component, and
    !      n_Predictors(j, l) is the number of predictors for that channel
    !            and component.
    !  The size of the coefficient data at j and l is given by
    !     Pos_Index(j+1, l) - Pos_Index(j, l)
    !  and the sub-structure of the data at j and l depends on the algorithm. The
    !  following is an example of the sub-structure:
    !    As the number layers is fixed and known for all channels and components,
    !    the positions of the coeffs for a particular layer are known. Let i be the 
    !    index to the array C for channel l, component j, layer k and coefficient m, 
    !    then
    !       i = Pos_Index(j, l) + (m-1)*n_Predictors(j, l) + k
    !    Thus, accessing C(i) is equivalent to that given by C(m, k, j, l) if C is 
    !    a 4-D array.
    !
    !    Notice: the value of n_Predictors(j, l) can be zero, which means the 
    !            coeff data for j, l does not exist. Thus, this value should
    !            be checked before accessing C.
    !---------------------------------------------------------------------------           
    INTEGER(Long), POINTER :: n_Predictors(:,:)  => NULL()  ! J x L
    INTEGER(Long), POINTER :: Pos_Index(:,:)     => NULL()  ! J x L
    REAL(Single),  POINTER :: C(:)               => NULL()  ! Iuse

    !----------------------------------------------------------------
    ! Compact OPTRAN water vapor line
    ! OSignificance - an integer number indicating if for this channel
    !                 OPTRAN should be applied.
    ! Order    - order of the polynomial
    ! OP_Index - Predictor indexes (OP_Index(0) is the number of predictors)
    ! OPos_Index - starting position for the coefficient data in the OC
    !              array
    ! OC - Coefficients
    !----------------------------------------------------------------
    INTEGER(Long), POINTER :: OSignificance(:)  => NULL()  ! L
    INTEGER(LONG), POINTER :: Order(:)          => NULL()  ! L
    INTEGER(Long), POINTER :: OP_Index(:,:)     => NULL()  ! 0:OI x L
    INTEGER(Long), POINTER :: OPos_Index(:)     => NULL()  ! J x L
    REAL(fp),      POINTER :: OC(:)             => NULL()  ! OC
    REAL(fp)               :: Alpha = 0.0_fp, Alpha_C1 = 0.0_fp, Alpha_C2 = 0.0_fp
    INTEGER(Long)          :: OComponent_Index = -1

  END TYPE ODPS_type

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
!       Associated_ODPS
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       ODPS structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_ODPS( ODPS             ,&  ! Input
!                                             ANY_Test=Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       ODPS:        ODPS structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       ODPS_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    ODPS structure pointer members are associated.
!                    The default is to test if ALL the pointer members
!                    are associated.
!                    If ANY_Test = 0, test if ALL the pointer members
!                                     are associated.  (DEFAULT)
!                       ANY_Test = 1, test if ANY of the pointer members
!                                     are associated.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the ODPS pointer members.
!                            .TRUE.  - if ALL the ODPS pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the ODPS pointer
!                                      members are associated.
!                            .FALSE. - some or all of the ODPS pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_ODPS( ODPS    , & ! Input
                            ANY_Test) & ! Optional input
                          RESULT( Association_Status )
    ! Arguments
    TYPE(ODPS_type)  , INTENT(IN) :: ODPS
    INTEGER, OPTIONAL, INTENT(IN) :: ANY_Test
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
      IF ( ASSOCIATED( ODPS%Sensor_Channel    ) .AND. &
           ASSOCIATED( ODPS%Component_ID      ) .AND. &
           ASSOCIATED( ODPS%Absorber_ID       ) .AND. &
           ASSOCIATED( ODPS%Ref_Level_Pressure) .AND. &
           ASSOCIATED( ODPS%Ref_Pressure      ) .AND. &
           ASSOCIATED( ODPS%Ref_Temperature   ) .AND. &
           ASSOCIATED( ODPS%Ref_Absorber      ) .AND. &
           ASSOCIATED( ODPS%Min_Absorber      ) .AND. &
           ASSOCIATED( ODPS%Max_Absorber      ) .AND. &
           ASSOCIATED( ODPS%n_Predictors      ) .AND. &
           ASSOCIATED( ODPS%Pos_Index         )      ) THEN
        Association_Status = .TRUE.
      END IF
      IF( ODPS%n_Coeffs > 0 )THEN
         Association_Status = Association_Status .AND. ASSOCIATED( ODPS%C ) 
      END IF
      IF( ODPS%n_OCoeffs > 0 )THEN
         Association_Status = Association_Status .AND. ASSOCIATED( ODPS%OC ) &
                                                 .AND. ASSOCIATED( ODPS%OSignificance ) &
                                                 .AND. ASSOCIATED( ODPS%Order )    &
                                                 .AND. ASSOCIATED( ODPS%OP_Index ) &
                                                 .AND. ASSOCIATED( ODPS%OPos_Index )
      END IF

    ELSE
      IF ( ASSOCIATED( ODPS%Sensor_Channel    ) .OR. &
           ASSOCIATED( ODPS%Component_ID      ) .OR. &
           ASSOCIATED( ODPS%Absorber_ID       ) .OR. &
           ASSOCIATED( ODPS%Ref_Level_Pressure) .OR. &
           ASSOCIATED( ODPS%Ref_Pressure      ) .OR. &
           ASSOCIATED( ODPS%Ref_Temperature   ) .OR. &
           ASSOCIATED( ODPS%Ref_Absorber      ) .OR. &
           ASSOCIATED( ODPS%Min_Absorber      ) .OR. &
           ASSOCIATED( ODPS%Max_Absorber      ) .OR. &
           ASSOCIATED( ODPS%n_Predictors      ) .OR. &
           ASSOCIATED( ODPS%Pos_Index         )      ) THEN
        Association_Status = .TRUE.
      END IF
      IF( ODPS%n_Coeffs > 0 )THEN
         Association_Status = Association_Status .OR. ASSOCIATED( ODPS%C ) 
      END IF
      IF( ODPS%n_OCoeffs > 0 )THEN
         Association_Status = Association_Status .OR. ASSOCIATED( ODPS%OC ) &
                                                 .OR. ASSOCIATED( ODPS%OSignificance ) &
                                                 .OR. ASSOCIATED( ODPS%Order )    &
                                                 .OR. ASSOCIATED( ODPS%OP_Index ) &
                                                 .OR. ASSOCIATED( ODPS%OPos_Index )
      END IF

    END IF

  END FUNCTION Associated_ODPS


!------------------------------------------------------------------------------
!
! NAME:
!       Destroy_ODPS
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of ODPS
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_ODPS( ODPS                   , &  ! Output
!                                    RCS_Id     =RCS_Id     , &  ! Revision control
!                                    Message_Log=Message_Log  )  ! Error messaging
!
! OUTPUT ARGUMENTS:
!       ODPS:         Re-initialized ODPS structure.
!                     UNITS:      N/A
!                     TYPE:       ODPS_type
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
!       Note the INTENT on the output ODPS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Destroy_ODPS( ODPS       , &  ! Output
                         No_Clear   , &  ! Optional input
                         RCS_Id     , &  ! Revision control
                         Message_Log) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(ODPS_type)       , INTENT(IN OUT) :: ODPS
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_ODPS'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Reinitialise the dimensions
    ODPS%n_Layers     = 0
    ODPS%n_Components = 0
    ODPS%n_Absorbers  = 0
    ODPS%n_Channels   = 0

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_ODPS( ODPS )

    ! If ALL components are NOT associated, do nothing
    IF ( .NOT. Associated_ODPS( ODPS ) ) RETURN


    ! Deallocate the regular arrays components
    ! ----------------------------------------
    DEALLOCATE( ODPS%Sensor_Channel     , &
                ODPS%Component_ID       , &
                ODPS%Absorber_ID        , &
                ODPS%Ref_Level_Pressure , &
                ODPS%Ref_Pressure       , &
                ODPS%Ref_Temperature    , &
                ODPS%Ref_Absorber       , &
                ODPS%Min_Absorber       , &
                ODPS%Max_Absorber       , &
                ODPS%n_Predictors       , &
                ODPS%Pos_Index          , &
                STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error deallocating ODPS components. STAT = ",i0)' ) &
                     Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message),   &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

    IF( ODPS%n_Coeffs > 0 )THEN
      ODPS%n_Coeffs = 0
      DEALLOCATE( ODPS%C                , &
                  STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error deallocating ODPS C component. STAT = ",i0)' ) &
                       Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM(Message), &
                              Error_Status,    &
                              Message_Log=Message_Log )
      END IF
    END IF

    IF( ODPS%n_OCoeffs > 0 )THEN
      ODPS%n_OCoeffs = 0
      DEALLOCATE( ODPS%OC              , &
                  ODPS%OSignificance   , &
                  ODPS%Order           , &
                  ODPS%OP_Index        , &
                  ODPS%OPos_Index      , &
                  STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error deallocating ODPS OPTRAN component. STAT = ",i0)' ) &
                       Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM(Message), &
                              Error_Status,    &
                              Message_Log=Message_Log )
      END IF
    END IF

    ! Decrement and test allocation counter
    ! -------------------------------------
    ODPS%n_Allocates = ODPS%n_Allocates - 1
    IF ( ODPS%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Allocation counter /= 0, Value = ",i0)' ) &
                      ODPS%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF
  END FUNCTION Destroy_ODPS


!------------------------------------------------------------------------------
!
! NAME:
!       Allocate_ODPS
! 
! PURPOSE:
!       Function to allocate the pointer members of the ODPS
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_ODPS( n_Layers               , &  ! Input
!                                     n_Components           , &  ! Input
!                                     n_Absorbers            , &  ! Input
!                                     n_Channels             , &  ! Input
!                                     n_Coeffs               , &  ! Input
!                                     ODPS                   , &  ! Output
!                                     RCS_Id     =RCS_Id     , &  ! Revision control
!                                     Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Layers:     The number of profile layers           
!                     UNITS:      N/A                        
!                     TYPE:       INTEGER                    
!                     DIMENSION:  Scalar                     
!                     ATTRIBUTES: INTENT(IN)      
!
!       n_Components: The number of transmittance components (i.g. dry & wlo)      
!                     UNITS:      N/A                                              
!                     TYPE:       INTEGER                                          
!                     DIMENSION:  Scalar                                           
!                     ATTRIBUTES: INTENT(IN)                            
!
!       n_Absorbers:  The number of absorbers dimension (i.g H2O & O3).      
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
!       n_Coeffs:     The total number of tau coefficients.                         
!                     Note, the Coeff data are now stored in a one-dimensional       
!                     array                                                         
!                     UNITS:      N/A                                               
!                     TYPE:       INTEGER                                           
!                     DIMENSION:  Scalar                                            
!                     ATTRIBUTES: INTENT(IN)                           
!
!!
! OUTPUT ARGUMENTS:
!       ODPS:         ODPS structure with allocated
!                     pointer members
!                     UNITS:      N/A
!                     TYPE:       ODPS_type
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
!       Note the INTENT on the output ODPS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Allocate_ODPS( n_Layers    , &  ! Input
                          n_Components, &  ! Input
                          n_Absorbers,  &  ! Input
                          n_Channels  , &  ! Input
                          n_Coeffs    , &  ! Input
                          ODPS        , &  ! Output
                          RCS_Id      , &  ! Revision control
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    INTEGER               , INTENT(IN)     :: n_Layers
    INTEGER               , INTENT(IN)     :: n_Components
    INTEGER               , INTENT(IN)     :: n_Absorbers
    INTEGER               , INTENT(IN)     :: n_Channels
    INTEGER               , INTENT(IN)     :: n_Coeffs
    TYPE(ODPS_type)       , INTENT(IN OUT) :: ODPS
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_ODPS'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check dimension input
    IF ( n_Layers     < 1 .OR. &
         n_Components < 1 .OR. &
         n_Absorbers  < 1 .OR. &
         n_Channels   < 1 .OR. &
         n_Coeffs     < 0    ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            "The input ODPS dimension must be >= 0 "//&
                            "and other dimensions must be > 0", &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_ODPS( ODPS, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_ODPS( ODPS, &
                                   No_Clear=SET, &
                                   Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating ODPS prior to reallocation.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Allocate the data arrays
    ! ------------------------
    ALLOCATE( ODPS%Sensor_Channel( n_Channels ),      &
              ODPS%Component_ID( n_Components ),      &
              ODPS%Absorber_ID( n_Absorbers ),        &
              ODPS%Ref_Level_Pressure( 0:n_Layers ),  &
              ODPS%Ref_Pressure( n_Layers ),          &
              ODPS%Ref_Temperature( n_Layers ),       &
              ODPS%Ref_Absorber( n_Layers, n_Absorbers ),    &
              ODPS%Min_Absorber( n_Layers, n_Absorbers ),    &
              ODPS%Max_Absorber( n_Layers, n_Absorbers ),    &
              ODPS%n_Predictors( n_Components, n_Channels ), &
              ODPS%Pos_Index( n_Components, n_Channels ), &
              STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error allocating ODPS data arrays. STAT = ",i0)' ) &
                     Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    IF( n_Coeffs > 0 )THEN
      ALLOCATE( ODPS%C( n_Coeffs ), &
                STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error allocating the ODPS C array. STAT = ",i0)' ) &
                       Allocate_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Assign the dimensions and initialise arrays
    ODPS%n_Layers     = n_Layers
    ODPS%n_Components = n_Components
    ODPS%n_Absorbers  = n_Absorbers
    ODPS%n_Channels   = n_Channels
    ODPS%n_Coeffs     = n_Coeffs

    ODPS%Sensor_Channel    = 0
    ODPS%Component_ID      = IP_INVALID
    ODPS%n_Predictors      = 0
    ODPS%Pos_Index         = 0

    ! Increment and test allocation counter
    ! -------------------------------------
    ODPS%n_Allocates = ODPS%n_Allocates + 1
    IF ( ODPS%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Allocation counter /= 1, Value = ",i0)' ) &
                     ODPS%n_Allocates
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Allocate_ODPS

!------------------------------------------------------------------------------
!
! NAME:
!       Allocate_ODPS_OPTRAN
! 
! PURPOSE:
!       Function to allocate the pointer members of the ODPS OPTRAN related members
!     *** Note: the Allocate_ODPS rouitne must be called before calling this routine
!               to allocate memory for other ODPS members
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_ODPS( n_OCoeffs              , &  ! Input
!                                     ODPS                   , &  ! IN/Output
!                                     RCS_Id     =RCS_Id     , &  ! Revision control
!                                     Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_OCoeffs:    The total number of OPTRAN tau coefficients.                         
!                     Note, the Coeff data are now stored in a one-dimensional       
!                     array                                                         
!                     UNITS:      N/A                                               
!                     TYPE:       INTEGER                                           
!                     DIMENSION:  Scalar                                            
!                     ATTRIBUTES: INTENT(IN)                           
!!
! IN/OUTPUT ARGUMENTS:
!       ODPS:         ODPS structure with allocated OPTRAN related
!                     pointer members
!                     UNITS:      N/A
!                     TYPE:       ODPS_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(INOUT)
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
!       Note the INTENT on the output ODPS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Allocate_ODPS_OPTRAN( n_OCoeffs   , &  ! Input
                                 ODPS        , &  ! Output
                                 RCS_Id      , &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    INTEGER               , INTENT(IN)     :: n_OCoeffs
    TYPE(ODPS_type)       , INTENT(IN OUT) :: ODPS
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_ODPS_OPTRAN'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    IF ( n_OCoeffs < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            "The input ODPS n_OCoeffs dimension must be > 0 ",&
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check dimension input
    IF ( ODPS%n_Channels   < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            "The input ODPS n_Channels dimension must be > 0 ",&
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! If OPTRAN data arrays have already been allocated, deallocate them
    IF ( ODPS%n_OCoeffs > 0 ) THEN
      DEALLOCATE( ODPS%OSignificance,&
                  ODPS%Order,        &
                  ODPS%OP_Index,     &
                  ODPS%OPos_Index,   &
                  ODPS%OC,           &
                  STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error deallocating ODPS OPTRAN component prior to reallocation. STAT = ",i0)' ) &
                       Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM(Message), &
                              Error_Status,    &
                              Message_Log=Message_Log )
      END IF
    END IF 
            
    ! Allocate the data arrays
    ! ------------------------
    ALLOCATE( ODPS%OSignificance( ODPS%n_Channels ),    &
              ODPS%Order( ODPS%n_Channels )        ,    &
              ODPS%OP_Index( 0:N_PREDICTOR_USED_OPTRAN, ODPS%n_Channels ), &
              ODPS%OPos_Index( ODPS%n_Channels),        &
              ODPS%OC( n_OCoeffs ),                     &
              STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error allocating ODPS OPTRAN data arrays. STAT = ",i0)' ) &
                     Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign the dimensions and initialise arrays
    ODPS%n_OCoeffs    = n_OCoeffs

  END FUNCTION Allocate_ODPS_OPTRAN

!------------------------------------------------------------------------------
!
! NAME:
!       Assign_ODPS
!
! PURPOSE:
!       Function to copy valid ODPS structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_ODPS( ODPS_in                , &  ! Input
!                                   ODPS_out               , &  ! Output
!                                   RCS_Id     =RCS_Id     , &  ! Revision control
!                                   Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODPS_in:       ODPS structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       ODPS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ODPS_out:      Copy of the input structure, ODPS_in.
!                      UNITS:      N/A
!                      TYPE:       ODPS_type
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
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
!
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
!                      If == SUCCESS the structure assignment was successful
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output ODPS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Assign_ODPS( ODPS_in     , &  ! Input
                        ODPS_out    , &  ! Output
                        RCS_Id      , &  ! Revision control
                        Message_Log ) &  ! Error messaging
                      RESULT( Error_Status )
    ! Arguments
    TYPE(ODPS_type)       , INTENT(IN)     :: ODPS_in
    TYPE(ODPS_type)       , INTENT(IN OUT) :: ODPS_out
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_ODPS'

    ! Set up
    ! ------
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_ODPS( ODPS_In ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ODPS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Allocate the structure
    ! ----------------------
    Error_Status = Allocate_ODPS( ODPS_in%n_Layers    , &
                                  ODPS_in%n_Components, &
                                  ODPS_in%n_Absorbers , &
                                  ODPS_in%n_Channels  , &
                                  ODPS_in%n_Coeffs, &
                                  ODPS_out, &
                                  Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output ODPS arrays.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign intrinsic data types
    ! ---------------------------
    ODPS_out%Release   = ODPS_in%Release
    ODPS_out%Version   = ODPS_in%Version

    ODPS_out%Group_Index       = ODPS_in%Group_Index
    ODPS_out%Sensor_Id         = ODPS_in%Sensor_Id
    ODPS_out%Sensor_Type       = ODPS_in%Sensor_Type
    ODPS_out%WMO_Satellite_ID  = ODPS_in%WMO_Satellite_ID
    ODPS_out%WMO_Sensor_ID     = ODPS_in%WMO_Sensor_ID
    ODPS_out%Sensor_Channel    = ODPS_in%Sensor_Channel
    ODPS_out%Component_ID      = ODPS_in%Component_ID
    ODPS_out%Absorber_ID       = ODPS_in%Absorber_ID
    ODPS_out%Ref_Level_Pressure= ODPS_in%Ref_Level_Pressure
    ODPS_out%Ref_Pressure      = ODPS_in%Ref_Pressure
    ODPS_out%Ref_Temperature   = ODPS_in%Ref_Temperature
    ODPS_out%Ref_Absorber      = ODPS_in%Ref_Absorber
    ODPS_out%Min_Absorber      = ODPS_in%Min_Absorber
    ODPS_out%Max_Absorber      = ODPS_in%Max_Absorber
    ODPS_out%n_Predictors      = ODPS_in%n_Predictors
    ODPS_out%Pos_Index         = ODPS_in%Pos_Index
    IF( ODPS_in%n_Coeffs > 0 )THEN
      ODPS_out%C                 = ODPS_in%C
    END IF

    ! the OPTRAN part if it is not empty
    IF(ODPS_in%n_OCoeffs > 0)THEN
      Error_Status = Allocate_ODPS_OPTRAN( ODPS_in%n_OCoeffs,  &
                                           ODPS_out, &
                                           Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error allocating output ODPS OPTRAN data arrays.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF

      ODPS_out%OC               = ODPS_in%OC             
      ODPS_out%OSignificance    = ODPS_in%OSignificance  
      ODPS_out%Order            = ODPS_in%Order
      ODPS_out%OP_Index         = ODPS_in%OP_Index       
      ODPS_out%OPos_Index       = ODPS_in%OPos_Index 
      ODPS_out%OComponent_Index = ODPS_in%OComponent_Index
      ODPS_out%Alpha            = ODPS_in%Alpha
      ODPS_out%Alpha_C1         = ODPS_in%Alpha_C1
      ODPS_out%Alpha_C2         = ODPS_in%Alpha_C2
          
    END IF

  END FUNCTION Assign_ODPS


!------------------------------------------------------------------------------
!
! NAME:
!       Concatenate_Channel_ODPS
!
! PURPOSE:
!       Function to concatenate two valid ODPS structures along
!       the channel dimension.
!
! CALLING SEQUENCE:
!       Error_Status = Concatenate_Channel_ODPS( ODPS1                  , &  ! Input/Output
!                                                ODPS2                  , &  ! Input
!                                                RCS_Id     = RCS_Id    , &  ! Revision control
!                                                Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODPS1:         First ODPS structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       ODPS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!       ODPS2:         Second ODPS structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       ODPS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ODPS1:         The concatenated ODPS structure. The order of
!                      concatenation is ODPS1,ODPS2 along the 
!                      channel dimension.
!                      UNITS:      N/A
!                      TYPE:       ODPS_type
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
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
!
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
!                      If == SUCCESS the structure concatenation was successful
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The input ODPS1 argument contains the concatenated structure
!       data (in character-speak: ODPS1//ODPS2) on output. It is
!       reallocated within this routine so if an error occurs during the
!       reallocation, the contents of the input ODPS1 structure will
!       be lost.
!
!       Because of the structure reallocation there is a potential that 
!       available memory will become fragmented. Use this routine in a
!       manner that will minimise this effect (e.g. destroying structures or
!       allocatable arrays in the opposite order in which they were created). 
!
!------------------------------------------------------------------------------

  FUNCTION Concatenate_Channel_ODPS( ODPS1      , &  ! Input/Output
                                     ODPS2      , &  ! Input
                                     RCS_Id     , &  ! Revision control
                                     Message_Log) &  ! Error messaging
                                   RESULT( Error_Status )
    ! Arguments
    TYPE(ODPS_type)       , INTENT(IN OUT)  :: ODPS1
    TYPE(ODPS_type)       , INTENT(IN)      :: ODPS2
    CHARACTER(*), OPTIONAL, INTENT(OUT)     :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)      :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Concatenate_Channel_ODPS'
    ! Local variables
    INTEGER :: Destroy_Status
    INTEGER :: n_Channels, l1, l2
    INTEGER(Long)   :: n_Coeffs, n_OCoeffs
    TYPE(ODPS_type) :: ODPS_Tmp

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check structures
    IF ( .NOT. Associated_ODPS( ODPS1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ODPS1 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_ODPS( ODPS2 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ODPS2 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Compare structure release/version
    IF ( ODPS1%Release /= ODPS2%Release ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODPS Release values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( ODPS1%Version /= ODPS2%Version ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODPS Version values are different.', &
                            WARNING, &
                            Message_Log=Message_Log )

    END IF

    ! Check non-channel dimensions
    IF ( ODPS1%n_Layers     /= ODPS2%n_Layers     .OR. &
         ODPS1%n_Components /= ODPS2%n_Components .OR. &      
         ODPS1%n_Absorbers  /= ODPS2%n_Absorbers ) THEN      
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Non-channel ODPS dimensions are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the group ID
    IF ( ODPS1%Group_Index   /= ODPS2%Group_Index  )THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ODPS Group ID values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the sensor ID values
    IF ( ODPS1%Sensor_ID        /= ODPS2%Sensor_ID        .OR. &
         ODPS1%WMO_Satellite_ID /= ODPS2%WMO_Satellite_ID .OR. &
         ODPS1%WMO_Sensor_ID    /= ODPS2%WMO_Sensor_ID         ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ODPS sensor ID values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the component ID and absorber ID
    IF ( ANY(ODPS1%Component_ID  /= ODPS2%Component_ID) .OR. &
         ANY(ODPS1%Absorber_ID /= ODPS2%Absorber_ID) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ODPS component ID or absorber ID values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
 
    ! Reallocate the first structure
    ! ------------------------------
    ! Copy it...
    Error_Status = Assign_ODPS( ODPS1, ODPS_Tmp, &
                                Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error copying ODPS1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! ... now destroy it ...
    Error_Status = Destroy_ODPS( ODPS1, &
                                 Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying ODPS1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! ... and now re-allocate it for all channels
    n_Channels   = ODPS_Tmp%n_Channels + ODPS2%n_Channels
    n_Coeffs     = ODPS_Tmp%n_Coeffs + ODPS2%n_Coeffs
    Error_Status = Allocate_ODPS( ODPS_Tmp%n_Layers, &
                                  ODPS_Tmp%n_Components, &
                                  ODPS_Tmp%n_Absorbers,  &
                                  n_Channels, &
                                  n_Coeffs, &
                                  ODPS1, &
                                  Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reallocating ODPS1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Allocate memory for the C-OPTRAN part
    n_OCoeffs    = ODPS_Tmp%n_OCoeffs + ODPS2%n_OCoeffs
    IF( n_OCoeffs > 0 )THEN
      IF( ODPS_Tmp%n_OCoeffs * ODPS2%n_OCoeffs  == 0 )THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'ODPS OPTRAN data in the two ODPS structures are not consistent.', &
                              FAILURE, &
                              Message_Log=Message_Log )
        RETURN
      END IF
        
      Error_Status = Allocate_ODPS_OPTRAN( n_OCoeffs, &
                                           ODPS1, &
                                           Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reallocating ODPS1 OPTRAN data arrays.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Assign the non-channel data
    ! ---------------------------------
    ODPS1%Version           = MAX(ODPS_Tmp%Version, ODPS2%Version)
    ODPS1%Group_Index       = ODPS_Tmp%Group_Index
    ODPS1%Sensor_ID         = ODPS_Tmp%Sensor_ID
    ODPS1%Sensor_type       = ODPS_Tmp%Sensor_type
    ODPS1%WMO_Satellite_ID  = ODPS_Tmp%WMO_Satellite_ID
    ODPS1%WMO_Sensor_ID     = ODPS_Tmp%WMO_Sensor_ID
    ODPS1%Component_ID      = ODPS_Tmp%Component_ID
    ODPS1%Absorber_ID       = ODPS_Tmp%Absorber_ID
    ODPS1%Ref_Level_Pressure= ODPS_Tmp%Ref_Level_Pressure
    ODPS1%Ref_Pressure      = ODPS_Tmp%Ref_Pressure
    ODPS1%Ref_Temperature   = ODPS_Tmp%Ref_Temperature
    ODPS1%Ref_Absorber      = ODPS_Tmp%Ref_Absorber
    ODPS1%Min_Absorber      = ODPS_Tmp%Min_Absorber
    ODPS1%Max_Absorber      = ODPS_Tmp%Max_Absorber
    ! OPTRAN
    ODPS1%OComponent_Index  = ODPS_Tmp%OComponent_Index
    ODPS1%Alpha             = ODPS_Tmp%Alpha
    ODPS1%Alpha_C1          = ODPS_Tmp%Alpha_C1
    ODPS1%Alpha_C2          = ODPS_Tmp%Alpha_C2
     
    ! Concatenate the channel array data
    ! ----------------------------------
    ! The first part...
    l1 = 1
    l2 = ODPS_Tmp%n_Channels
    ODPS1%Sensor_Channel(l1:l2)      = ODPS_Tmp%Sensor_Channel
    ODPS1%n_Predictors(:,l1:l2)      = ODPS_Tmp%n_Predictors
    ODPS1%Pos_Index(:,l1:l2)         = ODPS_Tmp%Pos_Index

    IF( ODPS_Tmp%n_Coeffs > 0 )THEN 
      ODPS1%C(l1:ODPS_Tmp%n_Coeffs)    = ODPS_Tmp%C
    END IF

      ! COPTRAN part
    IF( ODPS_Tmp%n_OCoeffs > 0 )THEN 
      ODPS1%OC(l1:ODPS_Tmp%n_OCoeffs)= ODPS_Tmp%OC
      ODPS1%OSignificance(l1:l2)     = ODPS_Tmp%OSignificance
      ODPS1%Order(l1:l2)             = ODPS_Tmp%Order
      ODPS1%OP_Index(:,l1:l2)        = ODPS_Tmp%OP_Index      
      ODPS1%OPos_Index(l1:l2)        = ODPS_Tmp%OPos_Index
    END IF

    ! ...and the second part
    l1 = l2 + 1
    l2 = n_Channels
    ODPS1%Sensor_Channel(l1:l2)      = ODPS2%Sensor_Channel
    ODPS1%n_Predictors(:,l1:l2)      = ODPS2%n_Predictors
    ODPS1%Pos_Index(:,l1:l2)         = ODPS2%Pos_Index + ODPS_Tmp%n_Coeffs

    IF( ODPS2%n_Coeffs > 0 )THEN
      ODPS1%C(ODPS_Tmp%n_Coeffs+1:n_Coeffs) = ODPS2%C
    END IF

      ! COPTRAN part
    IF( ODPS2%n_OCoeffs > 0 )THEN 
      ODPS1%OC(ODPS_Tmp%n_OCoeffs+1:n_OCoeffs)= ODPS2%OC
      ODPS1%OSignificance(l1:l2)     = ODPS2%OSignificance
      ODPS1%Order(l1:l2)             = ODPS2%Order
      ODPS1%OP_Index(:,l1:l2)        = ODPS2%OP_Index    
      ODPS1%OPos_Index(l1:l2)        = ODPS2%OPos_Index
    END IF

    ! Destroy the temporary structure
    ! -------------------------------
    Destroy_Status = Destroy_ODPS( ODPS_Tmp, &
                                   Message_Log=Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying ODPS_Tmp structure.', &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Concatenate_Channel_ODPS


!------------------------------------------------------------------------------
!
! NAME:
!       Concatenate_Absorber_ODPS
!
! PURPOSE:
!       Function to concatenate two valid ODPS structures along
!       the absorber dimension.
!
! CALLING SEQUENCE:
!       Error_Status = Concatenate_Absorber_ODPS( ODPS1                  , &  ! Input/Output
!                                                 ODPS2                  , &  ! Input
!                                                 RCS_Id     = RCS_Id    , &  ! Revision control
!                                                 Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODPS1:         First ODPS structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       ODPS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!       ODPS2:         Second ODPS structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       ODPS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ODPS1:         The concatenated ODPS structure. The order of
!                      concatenation is ODPS1,ODPS2 along the 
!                      absorber dimension.
!                      UNITS:      N/A
!                      TYPE:       ODPS_type
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
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
!
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
!                      If == SUCCESS the structure concatenation was successful
!                         == FAILURE an error occurred.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The input ODPS1 argument contains the concatenated structure
!       data (in character-speak: ODPS1//ODPS2) on output. It is
!       reallocated within this routine so if an error occurs during the
!       reallocation, the contents of the input ODPS1 structure will
!       be lost.
!
!       Because of the structure reallocation there is a potential that 
!       available memory will become fragmented. Use this routine in a
!       manner that will minimise this effect (e.g. destroying structures or
!       allocatable arrays in the opposite order in which they were created). 
!
!------------------------------------------------------------------------------

  FUNCTION Concatenate_Absorber_ODPS( ODPS1      , &  ! Input/Output
                                      ODPS2      , &  ! Input
                                      RCS_Id     , &  ! Revision control
                                      Message_Log) &  ! Error messaging
                                    RESULT( Error_Status )
    ! Arguments
    TYPE(ODPS_type)       , INTENT(IN OUT) :: ODPS1
    TYPE(ODPS_type)       , INTENT(IN)     :: ODPS2
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Concatenate_Absorber_ODPS'
    ! Local variables
    INTEGER :: Destroy_Status
    INTEGER :: i, j, l, n_Components, n_Layers, n_Absorbers
    INTEGER(Long) :: j1, j2, m, n, n_Coeffs
    INTEGER :: indx(32)
    TYPE(ODPS_type) :: ODPS_Tmp

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check structures
    IF ( .NOT. Associated_ODPS( ODPS1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ODPS1 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_ODPS( ODPS2 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ODPS2 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Compare structure release/version
    IF ( ODPS1%Release /= ODPS2%Release ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODPS Release values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( ODPS1%Version /= ODPS2%Version ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODPS Version values are different.', &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

    ! Check the Layer dimension
    IF ( ODPS1%n_Layers     /= ODPS2%n_Layers     .OR. &
         ODPS1%n_Channels   /= ODPS2%n_Channels  ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Non-absorber ODPS dimensions are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the group ID values
    IF ( ODPS1%Group_Index /= ODPS2%Group_Index )THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ODPS group ID values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the sensor ID values
    IF ( ODPS1%Sensor_ID        /= ODPS2%Sensor_ID        .OR. &
         ODPS1%WMO_Satellite_ID /= ODPS2%WMO_Satellite_ID .OR. &
         ODPS1%WMO_Sensor_ID    /= ODPS2%WMO_Sensor_ID         ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ODPS sensor ID values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the channels
    IF ( ANY( ( ODPS1%Sensor_Channel - ODPS2%Sensor_Channel ) /= 0 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ODPS channel values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Reallocate the first structure
    ! ------------------------------
    ! Copy it...
    Error_Status = Assign_ODPS( ODPS1, ODPS_Tmp, &
                                Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error copying ODPS1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Get indexes for Union of the absorber ID for the reference absorber 
    ! profile array
    n_Absorbers = ODPS1%n_Absorbers
    n = 0
    DO i = 1, ODPS2%n_Absorbers
      DO j = 1, ODPS1%n_Absorbers
        IF(ODPS2%Absorber_ID(i) == ODPS1%Absorber_ID(j)) EXIT
      END DO
      ! an absorber ID in ODPS2 not found in ODSP1, so, add the ID in the Union 
      IF( j > ODPS1%n_Absorbers)THEN  
        n = n + 1
        indx(n) = i
      END IF
    END DO
    n_Absorbers = n_Absorbers + n
        
    ! ... now destroy it ...
    Error_Status = Destroy_ODPS( ODPS1, &
                                 Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying ODPS1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! ... and now re-allocate it for all absorbers
    n_Components = ODPS_Tmp%n_Components + ODPS2%n_Components
    n_Coeffs     = ODPS_Tmp%n_Coeffs + ODPS2%n_Coeffs
    Error_Status = Allocate_ODPS( ODPS_Tmp%n_Layers, &
                                  n_Components, &
                                  n_Absorbers,  &
                                  ODPS_Tmp%n_Channels, &
                                  n_Coeffs, &
                                  ODPS1, &
                                  Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reallocating ODPS1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign the reference pressure and temperature
    ODPS1%Ref_Level_Pressure  = ODPS_Tmp%Ref_Level_Pressure
    ODPS1%Ref_Pressure        = ODPS_Tmp%Ref_Pressure
    ODPS1%Ref_Temperature     = ODPS_Tmp%Ref_Temperature

    ! Assign the absorber profile data
    ODPS1%Ref_Absorber(:, 1:ODPS_Tmp%n_Absorbers) = ODPS_Tmp%Ref_Absorber
    ODPS1%Min_Absorber(:, 1:ODPS_Tmp%n_Absorbers) = ODPS_Tmp%Min_Absorber
    ODPS1%Max_Absorber(:, 1:ODPS_Tmp%n_Absorbers) = ODPS_Tmp%Max_Absorber
    ODPS1%Absorber_ID(1:ODPS_Tmp%n_Absorbers) = ODPS_Tmp%Absorber_ID
    DO j = 1, n
      ODPS1%Ref_Absorber(:, ODPS_Tmp%n_Absorbers + j) = &
                                    ODPS2%Ref_Absorber(:, indx(j))           
      ODPS1%Min_Absorber(:, ODPS_Tmp%n_Absorbers + j) = &
                                    ODPS2%Min_Absorber(:, indx(j))           
      ODPS1%Max_Absorber(:, ODPS_Tmp%n_Absorbers + j) = &
                                    ODPS2%Max_Absorber(:, indx(j))           
      ODPS1%Absorber_ID(ODPS_Tmp%n_Absorbers + j) = &
                                    ODPS2%Absorber_ID(indx(j))
    END DO

    ! Assign the non-absorber data
    ! ----------------------------------
    ODPS1%Version           = MAX( ODPS_Tmp%Version, ODPS2%Version )
    ODPS1%Group_Index       = ODPS_Tmp%Group_Index
    ODPS1%Sensor_ID         = ODPS_Tmp%Sensor_ID
    ODPS1%Sensor_type       = ODPS_Tmp%Sensor_type
    ODPS1%WMO_Satellite_ID  = ODPS_Tmp%WMO_Satellite_ID
    ODPS1%WMO_Sensor_ID     = ODPS_Tmp%WMO_Sensor_ID
    ODPS1%Sensor_Channel    = ODPS_Tmp%Sensor_Channel
    ! OPTRAN
    ODPS1%OComponent_Index  = ODPS_Tmp%OComponent_Index
    ODPS1%Alpha             = ODPS_Tmp%Alpha
    ODPS1%Alpha_C1          = ODPS_Tmp%Alpha_C1
    ODPS1%Alpha_C2          = ODPS_Tmp%Alpha_C2
     
    !--------------------------------
    ! Concatenate absorber array data
    !--------------------------------

    ! The first part...                                            
    j1 = 1                                                         
    j2 = ODPS_Tmp%n_Components                                      
    ODPS1%Component_ID(j1:j2)       = ODPS_Tmp%Component_ID         
    ODPS1%n_Predictors(j1:j2,:)     = ODPS_Tmp%n_Predictors

    ! ...the second part                                       
    j1 = ODPS_Tmp%n_Components + 1                              
    j2 = n_Components                                           
    ODPS1%Component_ID(j1:j2)       = ODPS2%Component_ID        
    ODPS1%n_Predictors(j1:j2,:)     = ODPS2%n_Predictors  

    !--- The C and Pos_Index arrays ---
    m = 1
    n_Layers = ODPS_Tmp%n_Layers
    DO l = 1, ODPS1%n_Channels
      ! The first part...    
      DO j = 1, ODPS_Tmp%n_Components
        n = n_Layers*ODPS_Tmp%n_Predictors(j, l)
        IF( n > 0 )THEN 
          j1 = ODPS_Tmp%Pos_Index(j, l)
          j2 = j1 + n - 1
          ODPS1%Pos_Index(j,l) = m
          ODPS1%C(m:m+n-1)= ODPS_Tmp%C(j1:j2)
          m = m + n
        END IF         
      END DO

      ! ...the second part
      DO j = 1, ODPS2%n_Components
        n = n_Layers*ODPS2%n_Predictors(j, l) 
        IF( n > 0 )THEN 
          j1 = ODPS2%Pos_Index(j, l)
          j2 = j1 + n - 1
          ODPS1%Pos_Index(ODPS_Tmp%n_Components+j,l) = m
          ODPS1%C(m:m+n-1)= ODPS2%C(j1:j2)
          m = m + n
        END IF
      END DO

    END DO

    ! Destroy the temporary structure
    ! -------------------------------
    Destroy_Status = Destroy_ODPS( ODPS_Tmp, &
                                 Message_Log=Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying ODPS_Tmp structure.', &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Concatenate_Absorber_ODPS


!------------------------------------------------------------------------------
!
! NAME:
!       Equal_ODPS
!
! PURPOSE:
!       Function to test if two ODPS structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_ODPS( ODPS_LHS               , &  ! Input
!                                  ODPS_RHS               , &  ! Input
!                                  ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                  Check_All  =Check_All  , &  ! Optional input
!                                  RCS_Id     =RCS_Id     , &  ! Revision control
!                                  Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODPS_LHS:      ODPS structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( ODPS_LHS == ODPS_RHS ).
!                      UNITS:      N/A
!                      TYPE:       ODPS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       ODPS_RHS:      ODPS structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( ODPS_LHS == ODPS_RHS ).
!                      UNITS:      N/A
!                      TYPE:       ODPS_type
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
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Check_All:     Set this argument to check ALL the *floating point*
!                      channel data of the ODPS structures. The default
!                      action is return with a FAILURE status as soon as
!                      any difference is found. This optional argument can
!                      be used to get a listing of ALL the differences
!                      between data in ODPS structures.
!                      If == 0, Return with FAILURE status as soon as
!                               ANY difference is found  *DEFAULT*
!                         == 1, Set FAILURE status if ANY difference is
!                               found, but continue to check ALL data.
!                      Note: Setting this argument has no effect if, for
!                            example, the structure dimensions are different,
!                            or the sensor ids/channels are different, or the
!                            absorber ids are different, etc. 
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
!
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
!                      If == SUCCESS the structures were equal
!                         == FAILURE - an error occurred, or
!                                    - the structures were different.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! COMMENTS:
!       Congruency of the structure data is a prerequisite of equality.
!       That is, the *order* of the data is important. For example, if
!       two structures contain the same absorber information, but in a
!       different order, the structures are not considered equal. 
! 
!------------------------------------------------------------------------------

  FUNCTION Equal_ODPS( ODPS_LHS   , &  ! Input
                       ODPS_RHS   , &  ! Input
                       ULP_Scale  , &  ! Optional input
                       Check_All  , &  ! Optional input
                       RCS_Id     , &  ! Revision control
                       Message_Log) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    TYPE(ODPS_type)       , INTENT(IN)  :: ODPS_LHS
    TYPE(ODPS_type)       , INTENT(IN)  :: ODPS_RHS
    INTEGER,      OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,      OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_ODPS'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: j, l
    INTEGER(Long) :: i

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
      IF ( Check_All == 1 ) Check_Once = .FALSE.
    END IF

    ! Check the structure association status
    IF ( .NOT. Associated_ODPS( ODPS_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT ODPS_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_ODPS( ODPS_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT ODPS_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check structure Release/Version
    ! -------------------------------
    IF ( ( ODPS_LHS%Release /= ODPS_RHS%Release ) .OR. &
         ( ODPS_LHS%Version /= ODPS_RHS%Version )      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Release/Version numbers are different : ", &
                        &i2, ".", i2.2, " vs. ", i2, ".", i2.2 )' ) &
                      ODPS_LHS%Release, ODPS_LHS%Version, &
                      ODPS_RHS%Release, ODPS_RHS%Version
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! Check dimensions
    ! ----------------
    IF ( ODPS_LHS%n_Layers     /= ODPS_RHS%n_Layers     .OR. &
         ODPS_LHS%n_Components /= ODPS_RHS%n_Components .OR. &
         ODPS_LHS%n_Absorbers  /= ODPS_RHS%n_Absorbers  .OR. &
         ODPS_LHS%n_Channels   /= ODPS_RHS%n_Channels   .OR. &
         ODPS_LHS%n_Coeffs     /= ODPS_RHS%n_Coeffs     .OR. &
         ODPS_LHS%n_OCoeffs    /= ODPS_RHS%n_OCoeffs      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Compare the values
    ! ------------------
    ! The Group_Index
    IF ( ODPS_LHS%Group_Index /= ODPS_RHS%Group_Index ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Group_Index values are different, ", &
                        &i0, " vs. ", i0 )' ) &
                      ODPS_LHS%Group_Index, ODPS_RHS%Group_Index
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The Sensor_ID
    IF ( ODPS_LHS%Sensor_Id /= ODPS_RHS%Sensor_Id ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Sensor_ID values are different, ", &
                        &a, " vs. ", a )' ) &
                      TRIM( ODPS_LHS%Sensor_Id), &
                      TRIM( ODPS_RHS%Sensor_Id)
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    ! The Sensor_Type
    IF ( ODPS_LHS%Sensor_Type /= ODPS_RHS%Sensor_Type ) THEN
      WRITE( Message,'("Sensor types are different, ", &
                       &i0,"(",a,") vs. ", i0,"(",a,")")' ) &
                      ODPS_LHS%Sensor_Type, &
                      TRIM(SENSOR_TYPE_NAME(ODPS_LHS%Sensor_Type)), &
                      ODPS_RHS%Sensor_Type, &
                      TRIM(SENSOR_TYPE_NAME(ODPS_RHS%Sensor_Type))
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The WMO Satellite ID
    IF ( ODPS_LHS%WMO_Satellite_ID /= ODPS_RHS%WMO_Satellite_ID ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("WMO_Satellite_ID values are different, ",i0,&
                      &" vs. ",i0 )' ) &
                      ODPS_LHS%WMO_Satellite_ID, &
                      ODPS_RHS%WMO_Satellite_ID
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The WMO Sensor ID
    IF ( ODPS_LHS%WMO_Sensor_ID /= ODPS_RHS%WMO_Sensor_ID ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("WMO_Sensor_ID values are different, ",i0,&
                      &" vs. ",i0)' ) &
                      ODPS_LHS%WMO_Sensor_ID, &
                      ODPS_RHS%WMO_Sensor_ID
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The Sensor_Channel
    DO l = 1, ODPS_RHS%n_Channels
      IF ( ODPS_LHS%Sensor_Channel(l) /= ODPS_RHS%Sensor_Channel(l) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Sensor_Channel values are different, ",i0,&
                        &" vs. ",i0,", for channel index # ",i0)' ) &
                        ODPS_LHS%Sensor_Channel(l), &
                        ODPS_RHS%Sensor_Channel(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The Component_ID
    DO j = 1, ODPS_RHS%n_Components
      IF ( ODPS_LHS%Component_ID(j) /= ODPS_RHS%Component_ID(j) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Component_ID values are different, ",i0,&
                        &" vs. ",i0,", for absorber index # ",i0)' ) &
                        ODPS_LHS%Component_ID(j), &
                        ODPS_RHS%Component_ID(j), &
                        j
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO


    ! The n_Predictors
    DO l = 1, ODPS_RHS%n_Channels
      DO j = 1, ODPS_RHS%n_Components
        IF ( ODPS_LHS%n_Predictors(j,l) /= ODPS_RHS%n_Predictors(j,l) ) THEN  
          Error_Status = FAILURE                                              
          WRITE( Message,'("n_Predictors values are different, ",i0,&         
                          &" vs. ",i0,", for index (",i0,1x,i0,")")' ) &       
                          ODPS_LHS%n_Predictors(j,l), &                       
                          ODPS_RHS%n_Predictors(j,l), &                       
                          j,l                                                 
          CALL Display_Message( ROUTINE_NAME, &                               
                                TRIM(Message), &                              
                                Error_Status, &                               
                                Message_Log=Message_Log )                     
          IF ( Check_Once ) RETURN                                            
        END IF                                                                
      END DO
    END DO

    ! The Pos_Index
    DO l = 1, ODPS_RHS%n_Channels
      DO j = 1, ODPS_RHS%n_Components
        IF ( ODPS_LHS%Pos_Index(j,l) /= ODPS_RHS%Pos_Index(j,l) ) THEN  
          Error_Status = FAILURE                                                          
          WRITE( Message,'("Pos_Index values are different, ",i0,&                  
                          &" vs. ",i0,", for index (",i0,1x,i0,")")' ) &            
                          ODPS_LHS%Pos_Index(j,l), &                             
                          ODPS_RHS%Pos_Index(j,l), &                             
                          j,l                                                          
          CALL Display_Message( ROUTINE_NAME, &                                           
                                TRIM(Message), &                                          
                                Error_Status, &                                           
                                Message_Log=Message_Log )                                 
          IF ( Check_Once ) RETURN                                                        
        END IF                                                                            
      END DO
    END DO

    ! The Coefficients
    DO i = 1, ODPS_RHS%n_Coeffs
      IF ( ODPS_LHS%C(i) /= ODPS_RHS%C(i) ) THEN                      
        Error_Status = FAILURE                                        
        WRITE( Message,'("C values are different, ",i0,&              
                        &" vs. ",i0,", for index (",i0,")")' ) &      
                        ODPS_LHS%C(i), &                              
                        ODPS_RHS%C(i), &                              
                        i                                             
        CALL Display_Message( ROUTINE_NAME, &                         
                              TRIM(Message), &                        
                              Error_Status, &                         
                              Message_Log=Message_Log )               
        IF ( Check_Once ) RETURN                                      
      END IF                                                          
    END DO

    ! C-OPTRAN data
    ! ----------------
    IF(ODPS_RHS%n_OCoeffs > 0)THEN
      IF(ANY(ODPS_LHS%OC /= ODPS_RHS%OC) .OR. &
         ANY(ODPS_LHS%OSignificance /= ODPS_RHS%OSignificance) .OR. &
         ANY(ODPS_LHS%Order /= ODPS_RHS%Order) .OR. &
         ANY(ODPS_LHS%OP_Index /= ODPS_RHS%OP_Index) .OR. &
         ANY(ODPS_LHS%OPos_Index /= ODPS_RHS%OPos_Index) .OR. &
         ODPS_LHS%OComponent_Index /= ODPS_RHS%OComponent_Index .OR. &
         ODPS_LHS%Alpha     /= ODPS_RHS%Alpha     .OR. &
         ODPS_LHS%Alpha_C1  /= ODPS_RHS%Alpha_C1  .OR. &
         ODPS_LHS%Alpha_C2  /= ODPS_RHS%Alpha_C2  )THEN
        Error_Status = FAILURE                                        
        CALL Display_Message( ROUTINE_NAME, &                         
                              "ODPS OPTRAN data are different", &                        
                              Error_Status, &                         
                              Message_Log=Message_Log )               
        IF ( Check_Once ) RETURN                                      
      END IF  
    END IF                                                        

  END FUNCTION Equal_ODPS


!----------------------------------------------------------------------------------
!
! NAME:
!       CheckRelease_ODPS
!
! PURPOSE:
!       Function to check the ODPS Release value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckRelease_ODPS( ODPS                   , &  ! Input
!                                         RCS_Id     = RCS_Id    , &  ! Revision control
!                                         Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODPS:          ODPS structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       ODPS_type
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

  FUNCTION CheckRelease_ODPS( ODPS       , &  ! Input
                              RCS_Id     , &  ! Revision control
                              Message_Log) &  ! Error messaging
                             RESULT( Error_Status )
    ! Arguments
    TYPE(ODPS_type)       , INTENT(IN)  :: ODPS
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckRelease_ODPS'
    ! Local variables
    CHARACTER(ML) :: Message

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Check the release
    ! -----------------
    ! Check that release is not too old
    IF ( ODPS%Release < ODPS_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An ODPS data update is needed. ", &
                        &"ODPS release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      ODPS%Release, ODPS_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check that release is not too new
    IF ( ODPS%Release > ODPS_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An ODPS software update is needed. ", &
                        &"ODPS release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      ODPS%Release, ODPS_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_ODPS


!----------------------------------------------------------------------------------
!
! NAME:
!       CheckAlgorithm_ODPS
!
! PURPOSE:
!       Function to check the ODPS Algorithm value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckAlgorithm_ODPS( ODPS                   , &  ! Input
!                                           RCS_Id     = RCS_Id    , &  ! Revision control
!                                           Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODPS:          ODPS structure for which the Algorithm member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       ODPS_type
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

  FUNCTION CheckAlgorithm_ODPS( ODPS       , &  ! Input
                                RCS_Id     , &  ! Revision control
                                Message_Log) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    TYPE(ODPS_type)       , INTENT(IN)  :: ODPS
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckAlgorithm_ODPS'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Check the algorithm ID
    ! ----------------------
    IF ( ODPS%Algorithm /= ODPS_ALGORITHM ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'The ODPS Algorithm ID check failed. '//&
                            'The data structure is not an ODPS structure', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckAlgorithm_ODPS


!------------------------------------------------------------------------------
!
! NAME:
!       Info_ODPS
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the ODPS data structure.
!
! CALLING SEQUENCE:
!       CALL Info_ODPS( ODPS         , &  ! Input
!                       Info         , &  ! Output
!                       RCS_Id=RCS_Id  )  ! Revision control
!
! INPUT ARGUMENTS:
!       ODPS:          Filled ODPS structure.
!                      UNITS:      N/A
!                      TYPE:       ODPS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed ODPS data structure.
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

  SUBROUTINE Info_ODPS( ODPS  , &  ! Input
                        Info  , &  ! Output
                        RCS_Id  )  ! Revision control
    ! Arguments
    TYPE(ODPS_type)       , INTENT(IN)  :: ODPS
    CHARACTER(*),           INTENT(OUT) :: Info
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    ! Local variables
    CHARACTER(2000) :: LongString

    ! Set up
    ! ------
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Write the required data to the local string
    ! -------------------------------------------
    WRITE( LongString,'( a,3x,"ODPS RELEASE.VERSION: ",i2,".",i2.2,2x,&
                      &"N_LAYERS=",i0,2x,&
                      &"N_COMPONENTS=",i0,2x,&
                      &"N_ABSORBERS=",i0,2x,&
                      &"N_CHANNELS=",i0,2x, &
                      &"N_COEFFS=",i0)' ) &
                      ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                      ODPS%Release, ODPS%Version, &
                      ODPS%n_Layers,     &
                      ODPS%n_Components, &
                      ODPS%n_Absorbers,  &
                      ODPS%n_Channels,   &
                      ODPS%n_Coeffs

    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_ODPS


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
!       Clear_ODPS
!
! PURPOSE:
!       Subroutine to clear the scalar members of a ODPS structure.
!
! CALLING SEQUENCE:
!       CALL Clear_ODPS( ODPS ) ! Output
!
! OUTPUT ARGUMENTS:
!       ODPS:    ODPS structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       ODPS_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output ODPS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_ODPS( ODPS )
    TYPE(ODPS_type), INTENT(IN OUT) :: ODPS
    ODPS%Release   = ODPS_RELEASE
    ODPS%Version   = ODPS_VERSION
    ODPS%Algorithm = ODPS_ALGORITHM
    ODPS%Group_Index      = IP_INVALID
    ODPS%Sensor_Id        = ' '
    ODPS%Sensor_Type      = INVALID_SENSOR
    ODPS%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    ODPS%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
  END SUBROUTINE Clear_ODPS

END MODULE ODPS_Define
