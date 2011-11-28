!
! EmisCoeff_Define
!
! Module defining the EmisCoeff data structure and containing
! routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE EmisCoeff_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: Long, Double
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Parameters
  PUBLIC :: N_EMISCOEFF_ITEMS
  PUBLIC :: EMISCOEFF_DATA_TYPE
  PUBLIC :: EMISCOEFF_DATA_NAME
  PUBLIC :: SPECTRAL_EMISCOEFF_TYPE
  PUBLIC :: SENSOR_EMISCOEFF_TYPE
  ! Datatypes
  PUBLIC :: EmisCoeff_type
  ! Procedures
  PUBLIC :: Associated_EmisCoeff
  PUBLIC :: Destroy_EmisCoeff
  PUBLIC :: Allocate_EmisCoeff
  PUBLIC :: Assign_EmisCoeff
  PUBLIC :: Equal_EmisCoeff
  PUBLIC :: Check_EmisCoeff_Release
  PUBLIC :: Info_EmisCoeff


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*),  PARAMETER :: MODULE_RCS_ID = &
  '$Id: EmisCoeff_Define.f90 1879 2008-03-03 19:45:30Z paul.vandelst@noaa.gov $'
  ! EmisCoeff init values
  REAL(Double),  PARAMETER :: FP_INIT = 0.0_Double
  INTEGER,       PARAMETER :: IP_INIT = -1
  ! Keyword set value
  INTEGER,       PARAMETER :: SET = 1
  ! Current valid release and version numbers
  INTEGER(Long), PARAMETER :: EMISCOEFF_RELEASE = 2  ! This determines structure and file formats.
  INTEGER(Long), PARAMETER :: EMISCOEFF_VERSION = 1  ! This is just the data version.
  ! The types of EmisCoeff structures
  INTEGER(Long), PARAMETER :: SPECTRAL_EMISCOEFF_TYPE = 1
  INTEGER(Long), PARAMETER ::   SENSOR_EMISCOEFF_TYPE = 2
  ! Number of EmisCoeff data items
  INTEGER(Long), PARAMETER :: N_EMISCOEFF_ITEMS = 4_Long
  ! Internal data type descriptors for the EmisCoeff data
  !    5 = Double (i.e. 8-byte float)
  !    4 = Single (i.e. 4-byte float)
  !    3 = Long   (i.e. 4-byte integer)
  INTEGER(Long), PARAMETER ::      LONG_TYPE = 3_Long
  INTEGER(Long), PARAMETER ::    SINGLE_TYPE = 4_Long
  INTEGER(Long), PARAMETER ::    DOUBLE_TYPE = 5_Long
  INTEGER(Long), PARAMETER :: CHARACTER_TYPE = 7_Long
  INTEGER(Long), PARAMETER, DIMENSION( N_EMISCOEFF_ITEMS ) :: &
    EMISCOEFF_DATA_TYPE = (/ DOUBLE_TYPE, &  ! Angle
                             DOUBLE_TYPE, &  ! Frequency
                             DOUBLE_TYPE, &  ! Wind speed
                             DOUBLE_TYPE /)  ! Emissivity
  ! Names of the data items (for error processing)
  CHARACTER(*), PARAMETER, DIMENSION( N_EMISCOEFF_ITEMS ) :: &
    EMISCOEFF_DATA_NAME = (/ 'Angle     ', &
                             'Frequency ', &
                             'Wind speed', &
                             'Emissivity' /)


  ! ------------------------------
  ! EmisCoeff data type definition
  ! ------------------------------
  TYPE :: EmisCoeff_type
    INTEGER :: n_Allocates = 0
    ! Release and version information
    INTEGER(Long) :: Release = EMISCOEFF_RELEASE 
    INTEGER(Long) :: Version = EMISCOEFF_VERSION 
    ! Data type
    INTEGER(Long) :: Data_Type = SPECTRAL_EMISCOEFF_TYPE
    ! The dimensions
    INTEGER(Long) :: n_Angles      = 0   ! I dimension
    INTEGER(Long) :: n_Frequencies = 0   ! L dimension
    INTEGER(Long) :: n_Wind_Speeds = 0   ! N dimension
    ! The dimension ordinate data
    REAL(Double), POINTER :: Angle(:)      => NULL()  ! I
    REAL(Double), POINTER :: Frequency(:)  => NULL()  ! L
    REAL(Double), POINTER :: Wind_Speed(:) => NULL()  ! N
    ! The spectral emissivity
    REAL(Double), POINTER :: Emissivity(:,:,:) => NULL()  ! I x L x N
  END TYPE EmisCoeff_type


CONTAINS


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
!       Clear_EmisCoeff
!
! PURPOSE:
!       Subroutine to clear the scalar members of a EmisCoeff structure.
!
! CALLING SEQUENCE:
!       CALL Clear_EmisCoeff( EmisCoeff ) ! Output
!
! OUTPUT ARGUMENTS:
!       EmisCoeff:   EmisCoeff structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       EmisCoeff_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output EmisCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_EmisCoeff( EmisCoeff )
    TYPE(EmisCoeff_type), INTENT(IN OUT) :: EmisCoeff
    EmisCoeff%Release = EMISCOEFF_RELEASE 
    EmisCoeff%Version = EMISCOEFF_VERSION 
  END SUBROUTINE Clear_EmisCoeff


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
!       Associated_EmisCoeff
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       EmisCoeff structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_EmisCoeff( EmisCoeff,          &  ! Input
!                                                  ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       EmisCoeff:   EmisCoeff structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       EmisCoeff_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    EmisCoeff structure pointer members are associated.
!                    The default is to test if ALL the pointer members
!                    are associated.
!                    If ANY_Test = 0, test if ALL the pointer members
!                                     are associated.  (DEFAULT)
!                       ANY_Test = 1, test if ANY of the pointer members
!                                     are associated.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the EmisCoeff pointer members.
!                            .TRUE.  - if ALL the EmisCoeff pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the EmisCoeff pointer
!                                      members are associated.
!                            .FALSE. - some or all of the EmisCoeff pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_EmisCoeff( EmisCoeff, & ! Input
                                 ANY_Test ) & ! Optional input
                               RESULT( Association_Status )
    ! Arguments
    TYPE(EmisCoeff_type), INTENT(IN) :: EmisCoeff
    INTEGER,      OPTIONAL, INTENT(IN) :: ANY_Test
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
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED( EmisCoeff%Angle      ) .AND. &
           ASSOCIATED( EmisCoeff%Frequency  ) .AND. &
           ASSOCIATED( EmisCoeff%Wind_Speed ) .AND. &
           ASSOCIATED( EmisCoeff%Emissivity )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( EmisCoeff%Angle      ) .OR. &
           ASSOCIATED( EmisCoeff%Frequency  ) .OR. &
           ASSOCIATED( EmisCoeff%Wind_Speed ) .OR. &
           ASSOCIATED( EmisCoeff%Emissivity )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_EmisCoeff





!------------------------------------------------------------------------------
!
! NAME:
!       Destroy_EmisCoeff
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of EmisCoeff
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_EmisCoeff( EmisCoeff,                &  ! Output
!                                         RCS_Id = RCS_Id,          &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
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
! OUTPUT ARGUMENTS:
!       EmisCoeff:    Re-initialized EmisCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       EmisCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
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
!       Note the INTENT on the output EmisCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Destroy_EmisCoeff( EmisCoeff,    &  ! Output
                              No_Clear,     &  ! Optional input
                              RCS_Id,       &  ! Revision control
                              Message_Log ) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    TYPE(EmisCoeff_type),   INTENT(IN OUT) :: EmisCoeff
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_EmisCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Reinitialise the dimensions
    EmisCoeff%n_Angles      = 0
    EmisCoeff%n_Frequencies = 0
    EmisCoeff%n_Wind_Speeds = 0
    
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == 1 ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_EmisCoeff( EmisCoeff )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_EmisCoeff( EmisCoeff ) ) RETURN


    ! Deallocate the array components
    ! -------------------------------
    DEALLOCATE( EmisCoeff%Angle     , &
                EmisCoeff%Frequency , &
                EmisCoeff%Wind_Speed, &
                EmisCoeff%Emissivity, &
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating EmisCoeff. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

    ! Decrement and test allocation counter
    ! -------------------------------------
    EmisCoeff%n_Allocates = EmisCoeff%n_Allocates - 1
    IF ( EmisCoeff%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      EmisCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_EmisCoeff


!------------------------------------------------------------------------------
!
! NAME:
!       Allocate_EmisCoeff
! 
! PURPOSE:
!       Function to allocate the pointer members of the EmisCoeff
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_EmisCoeff( n_Angles,                  &  ! Input
!                                          n_Frequencies,             &  ! Input
!                                          n_Wind_Speeds,             &  ! Input
!                                          EmisCoeff,                 &  ! Output
!                                          RCS_Id = RCS_Id,           &  ! Revision control
!                                          Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Angles:      Number of angles dimension.
!                      Must be > 0.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       n_Frequencies: Number of frequencies dimension.
!                      Must be > 0.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       n_Wind_Speeds: Number of wind speeds dimension.
!                      Must be > 0.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in
!                      which any messages will be logged. If not
!                      specified, or if an error occurs opening
!                      the log file, the default action is to
!                      output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       EmisCoeff:     EmisCoeff structure with allocated
!                      pointer members
!                      UNITS:      N/A
!                      TYPE:       EmisCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
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
!                      If == SUCCESS the structure allocation was successful
!                         == FAILURE - an error occurred, or
!                                    - the structure internal allocation counter
!                                      is not equal to one (1) upon exiting this
!                                      function. This value is incremented and
!                                      decremented for every structure allocation
!                                      and deallocation respectively.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output EmisCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Allocate_EmisCoeff( n_Angles,      &  ! Input
                               n_Frequencies, &  ! Input
                               n_Wind_Speeds, &  ! Input
                               EmisCoeff,     &  ! Output
                               RCS_Id,        &  ! Revision control
                               Message_Log )  &  ! Error messaging
                             RESULT( Error_Status )
    ! Arguments
    INTEGER,                INTENT(IN)     :: n_Angles
    INTEGER,                INTENT(IN)     :: n_Frequencies
    INTEGER,                INTENT(IN)     :: n_Wind_Speeds
    TYPE(EmisCoeff_type),   INTENT(IN OUT) :: EmisCoeff
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_EmisCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Dimensions
    IF ( n_Angles      < 1 .OR. &
         n_Frequencies < 1 .OR. &
         n_Wind_Speeds < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input EmisCoeff dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_EmisCoeff( EmisCoeff, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_EmisCoeff( EmisCoeff, &
                                        No_Clear=SET, &
                                        Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating EmisCoeff pointer members.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Perform the pointer allocation
    ! ------------------------------
     ALLOCATE( EmisCoeff%Angle( n_Angles ), &
              EmisCoeff%Frequency( n_Frequencies ), &
              EmisCoeff%Wind_Speed( n_Wind_Speeds ), &
              EmisCoeff%Emissivity( n_Angles, n_Frequencies, n_Wind_Speeds ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating EmisCoeff data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Assign the dimensions
    ! ---------------------
    EmisCoeff%n_Angles      = n_Angles     
    EmisCoeff%n_Frequencies = n_Frequencies
    EmisCoeff%n_Wind_Speeds = n_Wind_Speeds


    ! Initialise the arrays
    ! ---------------------
    EmisCoeff%Angle      = FP_INIT
    EmisCoeff%Frequency  = FP_INIT
    EmisCoeff%Wind_Speed = FP_INIT
    EmisCoeff%Emissivity = FP_INIT


    ! Increment and test the allocation counter
    ! -----------------------------------------
    EmisCoeff%n_Allocates = EmisCoeff%n_Allocates + 1
    IF ( EmisCoeff%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      EmisCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Allocate_EmisCoeff


!------------------------------------------------------------------------------
!
! NAME:
!       Assign_EmisCoeff
!
! PURPOSE:
!       Function to copy valid EmisCoeff structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_EmisCoeff( EmisCoeff_in,             &  ! Input
!                                        EmisCoeff_out,            &  ! Output
!                                        RCS_Id = RCS_Id,          &  ! Revision control
!                                        Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       EmisCoeff_in:  EmisCoeff structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       EmisCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
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
! OUTPUT ARGUMENTS:
!       EmisCoeff_out: Copy of the input structure, EmisCoeff_in.
!                      UNITS:      N/A
!                      TYPE:       EmisCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
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
!       Note the INTENT on the output EmisCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Assign_EmisCoeff( EmisCoeff_in,  &  ! Input
                             EmisCoeff_out, &  ! Output
                             RCS_Id,        &  ! Revision control
                             Message_Log )  &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    TYPE(EmisCoeff_type),   INTENT(IN)     :: EmisCoeff_in
    TYPE(EmisCoeff_type),   INTENT(IN OUT) :: EmisCoeff_out
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_EmisCoeff'

    ! Set up
    ! ------
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_EmisCoeff( EmisCoeff_In ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT EmisCoeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Allocate the structure
    ! ----------------------
    Error_Status = Allocate_EmisCoeff( EmisCoeff_in%n_Angles     , &
                                       EmisCoeff_in%n_Frequencies, &
                                       EmisCoeff_in%n_Wind_Speeds, &
                                       EmisCoeff_out, &
                                       Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output  EmisCoeff arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Assign structure data
    ! ---------------------
    ! Assign non-dimension scalar members
    EmisCoeff_out%Release = EmisCoeff_in%Release
    EmisCoeff_out%Version = EmisCoeff_in%Version

    ! Assign array data
    EmisCoeff_out%Angle      = EmisCoeff_in%Angle
    EmisCoeff_out%Frequency  = EmisCoeff_in%Frequency 
    EmisCoeff_out%Wind_Speed = EmisCoeff_in%Wind_Speed
    EmisCoeff_out%Emissivity = EmisCoeff_in%Emissivity

  END FUNCTION Assign_EmisCoeff


!------------------------------------------------------------------------------
!
! NAME:
!       Equal_EmisCoeff
!
! PURPOSE:
!       Function to test if two EmisCoeff structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_EmisCoeff( EmisCoeff_LHS          , &  ! Input
!                                       EmisCoeff_RHS          , &  ! Input
!                                       ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                       Check_All  =Check_All  , &  ! Optional input
!                                       RCS_Id     =RCS_Id     , &  ! Optional output
!                                       Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       EmisCoeff_LHS: EmisCoeff structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( EmisCoeff_LHS == EmisCoeff_RHS ).
!                      UNITS:      N/A
!                      TYPE:       EmisCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       EmisCoeff_RHS: EmisCoeff structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( EmisCoeff_LHS == EmisCoeff_RHS ).
!                      UNITS:      N/A
!                      TYPE:       EmisCoeff_type
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
!       Check_All:     Set this argument to check ALL the data in the
!                      EmisCoeff structures. The default action is return
!                      with a FAILURE status as soon as any difference is
!                      found. This optional argument can be used to get a
!                      listing of ALL the differences between data in
!                      the EmisCoeff structures. This does not apply if
!                      the structure dimensions are different, or the
!                      instrument/channel IDs are different.
!                      If == 0, Return with FAILURE status as soon as
!                               ANY difference is found  *DEFAULT*
!                         == 1, Set FAILURE status if ANY difference is
!                               found, but continue to check ALL data if
!                               structure dimensions are conformable.
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
!
! COMMENTS:
!       Congruency of the structure data is a prerequisite of equality.
!       That is, the *order* of the data is also important.
!
!------------------------------------------------------------------------------

  FUNCTION Equal_EmisCoeff( EmisCoeff_LHS, &  ! Input
                            EmisCoeff_RHS, &  ! Input
                            ULP_Scale    , &  ! Optional input
                            Check_All    , &  ! Optional input
                            RCS_Id       , &  ! Revision control
                            Message_Log  ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    TYPE(EmisCoeff_type),   INTENT(IN)  :: EmisCoeff_LHS
    TYPE(EmisCoeff_type),   INTENT(IN)  :: EmisCoeff_RHS
    INTEGER,      OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,      OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_EmisCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: i, l, n

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
    IF ( .NOT. Associated_EmisCoeff( EmisCoeff_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT EmisCoeff_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_EmisCoeff( EmisCoeff_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT EmisCoeff_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Check structure Release/Version
    ! -------------------------------
    IF ( ( EmisCoeff_LHS%Release /= EmisCoeff_RHS%Release ) .OR. &
         ( EmisCoeff_LHS%Version /= EmisCoeff_RHS%Version )      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Release/Version numbers are different : ", &
                        &i2, ".", i2.2, " vs. ", i2, ".", i2.2 )' ) &
                      EmisCoeff_LHS%Release, EmisCoeff_LHS%Version, &
                      EmisCoeff_RHS%Release, EmisCoeff_RHS%Version
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! Check dimensions
    ! ----------------
    IF ( EmisCoeff_LHS%n_Angles      /= EmisCoeff_RHS%n_Angles      .OR. &
         EmisCoeff_LHS%n_Frequencies /= EmisCoeff_RHS%n_Frequencies .OR. &
         EmisCoeff_LHS%n_Wind_Speeds /= EmisCoeff_RHS%n_Wind_Speeds      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    
    
    ! Check the dimension data
    ! ------------------------
    ! Angle values
    DO i = 1, EmisCoeff_LHS%n_Angles
      IF ( .NOT. Compare_Float( EmisCoeff_LHS%Angle(i), &
                                EmisCoeff_RHS%Angle(i), &
                                ULP = ULP                ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Angle values are different at #", i0,&
                        &":",2(1x,es13.6))') &
                       i, EmisCoeff_LHS%Angle(i), EmisCoeff_LHS%Angle(i)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! Frequency values
    DO l = 1, EmisCoeff_LHS%n_Frequencies
      IF ( .NOT. Compare_Float( EmisCoeff_LHS%Frequency(l), &
                                EmisCoeff_RHS%Frequency(l), &
                                ULP = ULP                ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Frequency values are different at #", i0,&
                        &":",2(1x,es13.6))') &
                       l, EmisCoeff_LHS%Frequency(l), EmisCoeff_LHS%Frequency(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! Wind_Speed values
    DO n = 1, EmisCoeff_LHS%n_Wind_Speeds
      IF ( .NOT. Compare_Float( EmisCoeff_LHS%Wind_Speed(n), &
                                EmisCoeff_RHS%Wind_Speed(n), &
                                ULP = ULP                ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Wind speed values are different at #", i0,&
                        &":",2(1x,es13.6))') &
                       n, EmisCoeff_LHS%Wind_Speed(n), EmisCoeff_LHS%Wind_Speed(n)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO


    ! Check the emissivity data
    ! -------------------------
    DO n = 1, EmisCoeff_LHS%n_Wind_Speeds
      DO l = 1, EmisCoeff_LHS%n_Frequencies
        DO i = 1, EmisCoeff_LHS%n_Angles
          IF ( .NOT. Compare_Float( EmisCoeff_LHS%Emissivity(i,l,n), &
                                    EmisCoeff_RHS%Emissivity(i,l,n), &
                                    ULP = ULP                ) ) THEN
            Error_Status = FAILURE
            WRITE( Message,'("Emissivity values are different at (",i0,",",i0,",",i0,")",&
                            &":",3(1x,es13.6))') &
                           i,l,n, &
                           EmisCoeff_LHS%Emissivity(i,l,n),EmisCoeff_LHS%Emissivity(i,l,n), &
                           EmisCoeff_LHS%Emissivity(i,l,n)-EmisCoeff_LHS%Emissivity(i,l,n)
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM(Message), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            IF ( Check_Once ) RETURN
          END IF
        END DO
      END DO
    END DO

  END FUNCTION Equal_EmisCoeff



!----------------------------------------------------------------------------------
!
! NAME:
!       Check_EmisCoeff_Release
!
! PURPOSE:
!       Function to check the EmisCoeff Release value.
!
! CALLING SEQUENCE:
!       Error_Status = Check_EmisCoeff_Release( EmisCoeff,                &  ! Input
!                                               RCS_Id      = RCS_Id,     &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       EmisCoeff:      EmisCoeff structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       EmisCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
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

  FUNCTION Check_EmisCoeff_Release( EmisCoeff,    &  ! Input
                                    RCS_Id,       &  ! Revision control
                                    Message_Log ) &  ! Error messaging
                                  RESULT( Error_Status )
    ! Arguments
    TYPE(EmisCoeff_type),   INTENT(IN)  :: EmisCoeff
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Check_EmisCoeff_Release'
    ! Local variables
    CHARACTER(256) :: Message

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Check release is not too old
    ! ----------------------------
    IF ( EmisCoeff%Release < EMISCOEFF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An EmisCoeff data update is needed. ", &
                        &"EmisCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      EmisCoeff%Release, EMISCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Check release is not too new
    ! ----------------------------
    IF ( EmisCoeff%Release > EMISCOEFF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An EmisCoeff software update is needed. ", &
                        &"EmisCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      EmisCoeff%Release, EMISCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Check_EmisCoeff_Release


!------------------------------------------------------------------------------
!
! NAME:
!       Info_EmisCoeff
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the EmisCoeff data structure.
!
! CALLING SEQUENCE:
!       CALL Info_EmisCoeff( EmisCoeff,      &  ! Input
!                            Info,           &  ! Output
!                            RCS_Id = RCS_Id )  ! Revision control
!
! INPUT ARGUMENTS:
!       EmisCoeff:  Filled EmisCoeff structure.
!                   UNITS:      N/A
!                   TYPE:       EmisCoeff_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:       String containing version and dimension information
!                   about the passed EmisCoeff data structure.
!                   UNITS:      N/A
!                   TYPE:       CHARACTER(*)
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:     Character string containing the Revision Control
!                   System Id field for the module.
!                   UNITS:      N/A
!                   TYPE:       CHARACTER(*)
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!------------------------------------------------------------------------------

  SUBROUTINE Info_EmisCoeff( EmisCoeff, &  ! Input
                             Info,      &  ! Output
                             RCS_Id     )  ! Revision control
    ! Arguments
    TYPE(EmisCoeff_type),   INTENT(IN)  :: EmisCoeff
    CHARACTER(*),           INTENT(OUT) :: Info
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(1000) :: Long_String

    ! Set up
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! Write the required data to the local string
    WRITE( Long_String, '( a,1x,"EmisCoeff RELEASE.VERSION: ", i2, ".", i2.2, 2x, &
                           &"N_ANGLES=",i3,2x,&
                           &"N_FREQUENCIES=",i5,2x,&
                           &"N_WIND_SPEEDS=",i3 )' ) &
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         EmisCoeff%Release, EmisCoeff%Version, &
                         EmisCoeff%n_Angles, &
                         EmisCoeff%n_Frequencies, &
                         EmisCoeff%n_Wind_Speeds

    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN( LEN(Info), LEN_TRIM(Long_String) ))

  END SUBROUTINE Info_EmisCoeff

END MODULE EmisCoeff_Define
