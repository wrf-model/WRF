!
! CRTM_SensorData_Define
!
! Module defining the CRTM SensorData structure and containing
! routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Jul-2004
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_SensorData_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: DEFAULT_N_SIGFIG, &
                                   OPERATOR(.EqualTo.), &
                                   Compares_Within_Tolerance
  USE File_Utility         , ONLY: File_Open, File_Exists
  USE Binary_File_Utility  , ONLY: Open_Binary_File      , &
                                   WriteGAtts_Binary_File, &
                                   ReadGAtts_Binary_File
  USE CRTM_Parameters      , ONLY: STRLEN, &
                                   INVALID_WMO_SATELLITE_ID, &
                                   INVALID_WMO_SENSOR_ID
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Parameters
  PUBLIC :: INVALID_WMO_SATELLITE_ID
  PUBLIC :: INVALID_WMO_SENSOR_ID
  ! Datatypes
  PUBLIC :: CRTM_SensorData_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(+)
  PUBLIC :: OPERATOR(-)
  ! Procedures
  PUBLIC :: CRTM_SensorData_Associated
  PUBLIC :: CRTM_SensorData_Destroy
  PUBLIC :: CRTM_SensorData_Create
  PUBLIC :: CRTM_SensorData_Zero
  PUBLIC :: CRTM_SensorData_IsValid
  PUBLIC :: CRTM_SensorData_Inspect
  PUBLIC :: CRTM_SensorData_DefineVersion
  PUBLIC :: CRTM_SensorData_Compare
  PUBLIC :: CRTM_SensorData_InquireFile
  PUBLIC :: CRTM_SensorData_ReadFile
  PUBLIC :: CRTM_SensorData_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_SensorData_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE CRTM_SensorData_Add
  END INTERFACE OPERATOR(+)

  INTERFACE OPERATOR(-)
    MODULE PROCEDURE CRTM_SensorData_Subtract
  END INTERFACE OPERATOR(-)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_SensorData_Define.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! File status on close after write error
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'


  ! -------------------------------
  ! SensorData structure definition
  ! -------------------------------
  !:tdoc+:
  TYPE :: CRTM_SensorData_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimension values
    INTEGER :: n_Channels = 0  ! L
    ! The data sensor IDs
    CHARACTER(STRLEN) :: Sensor_Id        = ' '
    INTEGER           :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER           :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    ! The sensor channels and brightness temperatures
    INTEGER , ALLOCATABLE :: Sensor_Channel(:)   ! L
    REAL(fp), ALLOCATABLE :: Tb(:)               ! L
  END TYPE CRTM_SensorData_type
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
!       CRTM_SensorData_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a CRTM SensorData object.
!
! CALLING SEQUENCE:
!       Status = CRTM_SensorData_Associated( SensorData )
!
! OBJECTS:
!       SensorData:  SensorData structure which is to have its member's
!                    status tested.
!                    UNITS:      N/A
!                    TYPE:       CRTM_SensorData_type
!                    DIMENSION:  Scalar or any rank
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:      The return value is a logical value indicating the
!                    status of the SensorData members.
!                      .TRUE.  - if the array components are allocated.
!                      .FALSE. - if the array components are not allocated.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Same as input SensorData argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SensorData_Associated( SensorData ) RESULT( Status )
    TYPE(CRTM_SensorData_type), INTENT(IN) :: SensorData
    LOGICAL :: Status
    Status = SensorData%Is_Allocated
  END FUNCTION CRTM_SensorData_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize CRTM SensorData objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_SensorData_Destroy( SensorData )
!
! OBJECTS:
!       SensorData:   Re-initialized SensorData structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_SensorData_type
!                     DIMENSION:  Scalar OR any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_SensorData_Destroy( SensorData )
    TYPE(CRTM_SensorData_type), INTENT(OUT) :: SensorData
    SensorData%Is_Allocated = .FALSE.
  END SUBROUTINE CRTM_SensorData_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of the CRTM SensorData object.
!
! CALLING SEQUENCE:
!       CALL CRTM_SensorData_Create( SensorData, n_Channels )
!
! OBJECTS:
!       SensorData:   SensorData structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_SensorData_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Channels:   Number of sensor channels.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as SensorData object
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_SensorData_Create( SensorData, n_Channels )
    ! Arguments
    TYPE(CRTM_SensorData_type), INTENT(OUT) :: SensorData
    INTEGER,                    INTENT(IN)  :: n_Channels
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Channels < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( SensorData%Sensor_Channel( n_Channels ), &
              SensorData%Tb( n_Channels ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    SensorData%n_Channels = n_Channels
    ! ...Arrays
    SensorData%Sensor_Channel = 0
    SensorData%Tb             = ZERO

    ! Set allocation indicator
    SensorData%Is_Allocated = .TRUE.

  END SUBROUTINE CRTM_SensorData_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_Zero
!
! PURPOSE:
!       Elemental subroutine to zero out the data arrays in a
!       CRTM SensorData object.
!
! CALLING SEQUENCE:
!       CALL CRTM_SensorData_Zero( SensorData )
!
! OBJECTS:
!       SensorData:    CRTM SensorData structure in which the data arrays are
!                      to be zeroed out.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SensorData_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       - The dimension components of the structure are *NOT* set to zero.
!       - The SensorData sensor id and channel components are *NOT* reset.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_SensorData_Zero( SensorData )
    TYPE(CRTM_SensorData_type), INTENT(IN OUT) :: SensorData
    ! Do nothing if structure is unused
    IF ( .NOT. CRTM_SensorData_Associated(SensorData) ) RETURN
    ! Only zero out the data arrays
    SensorData%Tb = ZERO
  END SUBROUTINE CRTM_SensorData_Zero


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_IsValid
!
! PURPOSE:
!       Non-pure function to perform some simple validity checks on a
!       CRTM SensorData object.
!
!       If invalid data is found, a message is printed to stdout.
!
! CALLING SEQUENCE:
!       result = CRTM_SensorData_IsValid( SensorData )
!
!         or
!
!       IF ( CRTM_SensorData_IsValid( SensorData ) ) THEN....
!
! OBJECTS:
!       SensorData:    CRTM SensorData object which is to have its
!                      contents checked.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SensorData_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:        Logical variable indicating whether or not the input
!                      passed the check.
!                      If == .FALSE., SensorData object is unused or contains
!                                     invalid data.
!                         == .TRUE.,  SensorData object can be used in CRTM.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_SensorData_IsValid( SensorData ) RESULT( IsValid )
    TYPE(CRTM_SensorData_type), INTENT(IN) :: SensorData
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SensorData_IsValid'
    CHARACTER(ML) :: msg

    ! Setup
    IsValid = .FALSE.
    ! ...Check if structure is used
    IF ( .NOT. CRTM_SensorData_Associated(SensorData) ) THEN
      msg = 'SensorData structure not allocated'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    ENDIF
    IF ( SensorData%n_channels < 1 ) THEN
      msg = 'SensorData structure dimension invalid'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    ENDIF

    ! Check data
    ! ...Change default so all entries can be checked
    IsValid = .TRUE.
    ! ...Data sensor ids
    IF ( LEN_TRIM(SensorData%Sensor_Id) == 0 ) THEN
      msg = 'Invalid Sensor Id found'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( SensorData%WMO_Satellite_Id == INVALID_WMO_SATELLITE_ID ) THEN
      WRITE(msg,'("Invalid WMO Satellite Id found, ",i0,", for ",a)') &
                SensorData%WMO_Satellite_Id, TRIM(SensorData%Sensor_Id)
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( SensorData%WMO_Sensor_Id == INVALID_WMO_SENSOR_ID ) THEN
      WRITE(msg,'("Invalid WMO Sensor Id found, ",i0,", for ",a)') &
                SensorData%WMO_Sensor_Id, TRIM(SensorData%Sensor_Id)
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      IsValid = .FALSE.
    ENDIF
    ! ...Listed sensor channels
    IF ( ANY(SensorData%Sensor_Channel < 1) ) THEN
      msg = 'Invalid Sensor Channel found'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      IsValid = .FALSE.
    ENDIF
    ! ...Data
    IF ( ALL(SensorData%Tb < ZERO ) ) THEN
      msg = 'All input SensorData brightness temperatures are negative'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      IsValid = .FALSE.
    ENDIF

  END FUNCTION CRTM_SensorData_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CRTM SensorData object to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_SensorData_Inspect( SensorData )
!
! INPUTS:
!       SensorData:    CRTM SensorData object to display.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SensorData_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_SensorData_Inspect( SensorData )
    TYPE(CRTM_SensorData_type), INTENT(IN) :: SensorData
    WRITE(*, '(1x,"SENSORDATA OBJECT")')
    ! Dimensions
    WRITE(*, '(3x,"n_Channels:",1x,i0)') SensorData%n_Channels
    ! Scalar components
    WRITE(*, '(3x,"Sensor Id       :",1x,a)') SensorData%Sensor_Id
    WRITE(*, '(3x,"WMO Satellite Id:",1x,i0)') SensorData%WMO_Satellite_Id
    WRITE(*, '(3x,"WMO Sensor Id   :",1x,i0)') SensorData%WMO_Sensor_Id
    IF ( .NOT. CRTM_SensorData_Associated(SensorData) ) RETURN
    ! Array components
    WRITE(*, '(3x,"Sensor channels:")')
    WRITE(*, '(10(1x,i5))') SensorData%Sensor_Channel
    WRITE(*, '(3x,"Brightness temperatures:")')
    WRITE(*, '(10(1x,es13.6))') SensorData%Tb
  END SUBROUTINE CRTM_SensorData_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_SensorData_DefineVersion( Id )
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

  SUBROUTINE CRTM_SensorData_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_SensorData_DefineVersion


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       CRTM_SensorData_Compare
!
! PURPOSE:
!       Elemental function to compare two CRTM_SensorData objects to within
!       a user specified number of significant figures.
!
! CALLING SEQUENCE:
!       is_comparable = CRTM_SensorData_Compare( x, y, n_SigFig=n_SigFig )
!
! OBJECTS:
!       x, y:          Two CRTM SensorData objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SensorData_type
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

  ELEMENTAL FUNCTION CRTM_SensorData_Compare( &
    x, &
    y, &
    n_SigFig ) &
  RESULT( is_comparable )
    TYPE(CRTM_SensorData_type), INTENT(IN) :: x, y
    INTEGER,          OPTIONAL, INTENT(IN) :: n_SigFig
    LOGICAL :: is_comparable
    ! Variables
    INTEGER :: l, n

    ! Set up
    is_comparable = .FALSE.
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF

    ! Check the structure association status
    IF ( (.NOT. CRTM_SensorData_Associated(x)) .OR. &
         (.NOT. CRTM_SensorData_Associated(y)) ) RETURN

    ! Check scalars
    IF ( (x%n_Channels       /= y%n_Channels      ) .OR. &
         (x%Sensor_Id        /= y%Sensor_Id       ) .OR. &
         (x%WMO_Satellite_ID /= y%WMO_Satellite_ID) .OR. &
         (x%WMO_Sensor_ID    /= y%WMO_Sensor_ID   ) ) RETURN

    ! Check arrays
    l = x%n_Channels
    IF ( ANY(x%Sensor_Channel(1:l) /= y%Sensor_Channel(1:l)) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Tb(1:l),y%Tb(1:l),n))) ) RETURN

    ! If we get here, the structures are comparable
    is_comparable = .TRUE.

  END FUNCTION CRTM_SensorData_Compare


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_InquireFile
!
! PURPOSE:
!       Function to inquire CRTM SensorData object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_SensorData_InquireFile( Filename           , &
!                                                   n_DataSets = n_DataSets  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       CRTM SensorData data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_DataSets:     The number of datasets in the file.
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

  FUNCTION CRTM_SensorData_InquireFile( &
    Filename  , &  ! Input
    n_DataSets) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),      INTENT(IN)  :: Filename
    INTEGER, OPTIONAL, INTENT(OUT) :: n_DataSets
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SensorData_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: n

    ! Setup
    err_stat = SUCCESS
    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Open the SensorData data file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Read the dimensions
    READ( fid,IOSTAT=io_stat ) n
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dataset dimensions from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! Close the file
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Set the return arguments
    IF ( PRESENT(n_DataSets) ) n_DataSets = n

  CONTAINS

    SUBROUTINE Inquire_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          CLOSE( fid,IOSTAT=io_stat )
          IF ( io_stat /= SUCCESS ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup'
        END IF
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION CRTM_SensorData_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_ReadFile
!
! PURPOSE:
!       Function to read CRTM SensorData object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_SensorData_ReadFile( Filename               , &
!                                                SensorData             , &
!                                                Quiet      = Quiet     , &
!                                                No_Close   = No_Close  , &
!                                                n_DataSets = n_DataSets  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       SensorData format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SensorData:     CRTM SensorData object array containing the sensor data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SensorData_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       No_Close:       Set this logical argument to NOT close the file upon exit.
!                       If == .FALSE., the input file is closed upon exit [DEFAULT]
!                          == .TRUE.,  the input file is NOT closed upon exit.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_DataSets:     The actual number of datasets read in.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file read was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_SensorData_ReadFile( &
    Filename  , &  ! Input
    SensorData, &  ! Output
    Quiet     , &  ! Optional input
    No_Close  , &  ! Optional input
    n_DataSets, &  ! Optional output
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: Filename
    TYPE(CRTM_SensorData_type), INTENT(OUT) :: SensorData(:)
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,          OPTIONAL, INTENT(IN)  :: No_Close
    INTEGER,          OPTIONAL, INTENT(OUT) :: n_DataSets
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SensorData_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    LOGICAL :: Yes_Close
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: i, n

    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet
    ! ...Check file close argument
    Yes_Close = .TRUE.
    IF ( PRESENT(No_Close) ) Yes_Close = .NOT. No_Close
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) Noisy = .TRUE.
    END IF


    ! Check if the file is open
    IF ( File_Open( FileName ) ) THEN
      ! Yes, the file is already open
      ! ...Get the file id
      INQUIRE( FILE=Filename,NUMBER=fid )
      IF ( fid == -1 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its fid'
        CALL Read_Cleanup(); RETURN
      END IF
    ELSE
      ! No, the file is not open
      ! ...Check that the file exists
      IF ( .NOT. File_Exists( Filename ) ) THEN
        msg = 'File '//TRIM(Filename)//' not found.'
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Open the file
      err_stat = Open_Binary_File( Filename, fid )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Read the dimensions
    READ( fid,IOSTAT=io_stat ) n
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dataset dimensions from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ...Check if output array large enough
    IF ( n > SIZE(SensorData) ) THEN
      WRITE( msg,'("Number of SensorData sets, ",i0," > size of the output ",&
             &"SensorData object array, ",i0,".")' ) &
             n, SIZE(SensorData)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Read the SensorData data
    SensorData_Loop: DO i = 1, n
      err_stat = Read_Record( fid, SensorData(i) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading SensorData element #",i0," from ",a)' ) &
               i, TRIM(Filename)
        CALL Read_Cleanup(Close_File=.TRUE.); RETURN
      END IF
    END DO SensorData_Loop


    ! Close the file
    IF ( Yes_Close ) THEN
      CLOSE( fid,IOSTAT=io_stat )
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Set the optional return values
    IF ( PRESENT(n_DataSets) ) n_DataSets = n


    ! Output an info message
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of datasets read from ",a,": ",i0)' ) TRIM(Filename), n
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Read_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          CLOSE( fid,IOSTAT=io_stat )
          IF ( io_stat /= 0 ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup.'
        END IF
      END IF
      ! Destroy the structure
      CALL CRTM_SensorData_Destroy( SensorData )
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION CRTM_SensorData_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_WriteFile
!
! PURPOSE:
!       Function to write CRTM SensorData object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_SensorData_WriteFile( Filename           , &
!                                                 SensorData         , &
!                                                 Quiet    = Quiet   , &
!                                                 No_Close = No_Close  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       SensorData format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       SensorData:     CRTM SensorData object array containing the datasets.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SensorData_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       No_Close:       Set this logical argument to NOT close the file upon exit.
!                       If == .FALSE., the input file is closed upon exit [DEFAULT]
!                          == .TRUE.,  the input file is NOT closed upon exit.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file write was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs during *writing*, the output file is deleted before
!         returning to the calling routine.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_SensorData_WriteFile( &
    Filename  , &  ! Input
    SensorData, &  ! Input
    Quiet     , &  ! Optional input
    No_Close  , &  ! Optional input
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN) :: Filename
    TYPE(CRTM_SensorData_type), INTENT(IN) :: SensorData(:)
    LOGICAL,          OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,          OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL,          OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SensorData_WriteFile'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    LOGICAL :: Yes_Close
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: i, n

    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet
    ! ...Check file close argument
    Yes_Close = .TRUE.
    IF ( PRESENT(No_Close) ) Yes_Close = .NOT. No_Close
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) Noisy = .TRUE.
    END IF

    ! Check the SensorData structure dimensions
    IF ( ANY(SensorData%n_Channels < 1) ) THEN
      msg = 'Dimensions of SensorData structures are < or = 0.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Check if the file is open
    IF ( File_Open( FileName ) ) THEN
      ! Yes, the file is already open
      INQUIRE( FILE=Filename,NUMBER=fid )
      IF ( fid == -1 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its fid'
        CALL Write_Cleanup(); RETURN
      END IF
    ELSE
      ! No, the file is not open
      err_stat = Open_Binary_File( Filename, fid, For_Output = .TRUE. )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Write the number of SensorDatas dimension
    n = SIZE(SensorData)
    WRITE( fid,IOSTAT=io_stat) n
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing dataset dimensions to ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the SensorData data
    SensorData_Loop: DO i = 1, n
      err_stat = Write_Record( fid, SensorData(i) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing SensorData element #",i0," to ",a)' ) &
               i, TRIM(Filename)
        CALL Write_Cleanup(Close_File=.TRUE.); RETURN
      END IF
    END DO SensorData_Loop


    ! Close the file (if error, no delete)
    IF ( Yes_Close ) THEN
      CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat )
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of datasets written to ",a,": ",i0)' ) TRIM(Filename), n
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat )
          IF ( io_stat /= 0 ) &
            msg = TRIM(msg)//'; Error deleting output file during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION CRTM_SensorData_WriteFile


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
!       CRTM_SensorData_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CRTM_SensorData objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CRTM_SensorData_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CRTM SensorData objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SensorData_type
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

  ELEMENTAL FUNCTION CRTM_SensorData_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_SensorData_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal
    ! Variables
    INTEGER :: n

    ! Set up
    is_equal = .FALSE.

    ! Check the structure association status
    IF ( (.NOT. CRTM_SensorData_Associated(x)) .OR. &
         (.NOT. CRTM_SensorData_Associated(y))      ) RETURN

    ! Check contents
    ! ...Scalars
    IF ( (x%n_Channels       /= y%n_Channels      ) .OR. &
         (x%Sensor_Id        /= y%Sensor_Id       ) .OR. &
         (x%WMO_Satellite_ID /= y%WMO_Satellite_ID) .OR. &
         (x%WMO_Sensor_ID    /= y%WMO_Sensor_ID   ) ) RETURN
    ! ...Arrays
    n = x%n_Channels
    IF ( ALL(x%Sensor_Channel(1:n) == y%Sensor_Channel(1:n)  ) .AND. &
         ALL(x%Tb(1:n) .EqualTo. y%Tb(1:n)) ) &
      is_equal = .TRUE.

  END FUNCTION CRTM_SensorData_Equal


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_SensorData_Add
!
! PURPOSE:
!       Pure function to add two CRTM SensorData objects.
!       Used in OPERATOR(+) interface block.
!
! CALLING SEQUENCE:
!       sDatasum = CRTM_SensorData_Add( sData1, sData2 )
!
!         or
!
!       sDatasum = sData1 + sData2
!
!
! INPUTS:
!       sData1, sData2: The SensorData objects to add.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SensorData_type
!                       DIMENSION:  Scalar or any rank
!                       ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!       sDatasum:       SensorData structure containing the added components.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SensorData_type
!                       DIMENSION:  Same as input
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SensorData_Add( sData1, sData2 ) RESULT( sDatasum )
    TYPE(CRTM_SensorData_type), INTENT(IN) :: sData1, sData2
    TYPE(CRTM_SensorData_type) :: sDatasum
    ! Variables
    INTEGER :: n

    ! Check input
    ! ...If input structures not used, do nothing
    IF ( .NOT. CRTM_SensorData_Associated( sData1 ) .OR. &
         .NOT. CRTM_SensorData_Associated( sData2 )      ) RETURN
    ! ...If input structure for different sensors, or sizes, do nothing
    IF ( sData1%n_Channels         /= sData2%n_Channels        .OR. &
         sData1%Sensor_Id          /= sData2%Sensor_Id         .OR. &
         sData1%WMO_Satellite_ID   /= sData2%WMO_Satellite_ID  .OR. &
         sData1%WMO_Sensor_ID      /= sData2%WMO_Sensor_ID     .OR. &
         ANY(sData1%Sensor_Channel /= sData2%Sensor_Channel) ) RETURN

    ! Copy the first structure
    sDatasum = sData1

    ! And add its components to the second one
    n = sData1%n_Channels
    sDatasum%Tb(1:n) = sDatasum%Tb(1:n) + sData2%Tb(1:n)

  END FUNCTION CRTM_SensorData_Add

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_SensorData_Subtract
!
! PURPOSE:
!       Pure function to subtract two CRTM SensorData objects.
!       Used in OPERATOR(-) interface block.
!
! CALLING SEQUENCE:
!       sDatadiff = CRTM_SensorData_Subtract( sData1, sData2 )
!
!         or
!
!       sDatadiff = sData1 - sData2
!
!
! INPUTS:
!       sData1, sData2: The SensorData objects to difference.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SensorData_type
!                       DIMENSION:  Scalar or any rank
!                       ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!       sDatadiff:      SensorData structure containing the differenced components.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SensorData_type
!                       DIMENSION:  Same as input
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SensorData_Subtract( sData1, sData2 ) RESULT( sDatadiff )
    TYPE(CRTM_SensorData_type), INTENT(IN) :: sData1, sData2
    TYPE(CRTM_SensorData_type) :: sDatadiff
    ! Variables
    INTEGER :: n

    ! Check input
    ! ...If input structures not used, do nothing
    IF ( .NOT. CRTM_SensorData_Associated( sData1 ) .OR. &
         .NOT. CRTM_SensorData_Associated( sData2 )      ) RETURN
    ! ...If input structure for different sensors, or sizes, do nothing
    IF ( sData1%n_Channels         /= sData2%n_Channels        .OR. &
         sData1%Sensor_Id          /= sData2%Sensor_Id         .OR. &
         sData1%WMO_Satellite_ID   /= sData2%WMO_Satellite_ID  .OR. &
         sData1%WMO_Sensor_ID      /= sData2%WMO_Sensor_ID     .OR. &
         ANY(sData1%Sensor_Channel /= sData2%Sensor_Channel) ) RETURN

    ! Copy the first structure
    sDatadiff = sData1

    ! And subtract the second one's components from it
    n = sData1%n_Channels
    sDatadiff%Tb(1:n) = sDatadiff%Tb(1:n) - sData2%Tb(1:n)

  END FUNCTION CRTM_SensorData_Subtract


!----------------------------------------------------------------------------------
!
! NAME:
!       Read_Record
!
! PURPOSE:
!       Utility function to read a single CRTM SensorData object
!
! CALLING SEQUENCE:
!       Error_Status = Read_Record( FileID, SensorData )
!
! INPUTS:
!       FileID:       Logical unit number from which to read data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SensorData:   CRTM SensorData object containing the data read in.
!                     UNITS:      N/A
!                     TYPE:       CRTM_SensorData_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the record read was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION Read_Record( &
    fid       , &  ! Input
    SensorData) &  ! Output
  RESULT( err_stat )
    ! Arguments
    INTEGER                   , INTENT(IN)  :: fid
    TYPE(CRTM_SensorData_type), INTENT(OUT) :: SensorData
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SensorData_ReadFile(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: n_Channels

    ! Set up
    err_stat = SUCCESS


    ! Read the dimensions
    READ( fid,IOSTAT=io_stat ) n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Allocate the structure
    CALL CRTM_SensorData_Create( SensorData, n_Channels )
    IF ( .NOT. CRTM_SensorData_Associated( SensorData ) ) THEN
      msg = 'SensorData object allocation failed.'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the SensorData data
    READ( fid,IOSTAT=io_stat ) SensorData%Sensor_Id       , &
                               SensorData%WMO_Satellite_ID, &
                               SensorData%WMO_Sensor_ID   , &
                               SensorData%Sensor_Channel  , &
                               SensorData%Tb
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading SensorData data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE Read_Record_Cleanup()
      ! Deallocate SensorData structure if necessary
      CALL CRTM_SensorData_Destroy( SensorData )
      ! Close input file
      CLOSE( fid,IOSTAT=io_stat )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup'
      ! Report error(s)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Read_Record_Cleanup

  END FUNCTION Read_Record


!----------------------------------------------------------------------------------
!
! NAME:
!       Write_Record
!
! PURPOSE:
!       Function to write a single CRTM SensorData object
!
! CALLING SEQUENCE:
!       Error_Status = Write_Record( FileID, SensorData )
!
! INPUT ARGUMENTS:
!       FileID:       Logical unit number to which data is written
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       SensorData:   CRTM SensorData object containing the data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_SensorData_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the write was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION Write_Record( &
    fid       , &  ! Input
    SensorData) &  ! Input
  RESULT( err_stat )
    ! Arguments
    INTEGER                   , INTENT(IN) :: fid
    TYPE(CRTM_SensorData_type), INTENT(IN) :: SensorData
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SensorData_WriteFile(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat

    ! Setup
    err_stat = SUCCESS
    IF ( .NOT. CRTM_SensorData_Associated( SensorData ) ) THEN
      msg = 'Input SensorData object is not used.'
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid,IOSTAT=io_stat ) SensorData%n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the data
    WRITE( fid,IOSTAT=io_stat ) SensorData%Sensor_Id       , &
                                SensorData%WMO_Satellite_ID, &
                                SensorData%WMO_Sensor_ID   , &
                                SensorData%Sensor_Channel  , &
                                SensorData%Tb
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing SensorData data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE Write_Record_Cleanup()
      ! Close and delete output file
      CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup'
      ! Report error(s)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Write_Record_Cleanup

  END FUNCTION Write_Record

END MODULE CRTM_SensorData_Define
