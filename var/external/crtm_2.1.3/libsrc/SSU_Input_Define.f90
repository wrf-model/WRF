!
! SSU_Input_Define
!
! Module containing the structure definition and associated routines
! for CRTM inputs specific to SSU
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, NOAA/NESDIS, Oct 6, 2009
!                       yong.han@noaa.gov
!
!                       Paul van Delst, 20-Oct-2009
!                       paul.vandelst@noaa.gov
!

MODULE SSU_Input_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp, Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE File_Utility         , ONLY: File_Open, File_Exists
  USE Binary_File_Utility  , ONLY: Open_Binary_File      , &
                                   WriteGAtts_Binary_File, &
                                   ReadGAtts_Binary_File
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Datatypes
  PUBLIC :: SSU_Input_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: SSU_Input_IsValid
  PUBLIC :: SSU_Input_Inspect
  PUBLIC :: SSU_Input_ValidRelease
  PUBLIC :: SSU_Input_DefineVersion
  PUBLIC :: SSU_Input_GetValue
  PUBLIC :: SSU_Input_SetValue
  PUBLIC :: SSU_Input_CellPressureIsSet
  PUBLIC :: SSU_Input_ReadFile
  PUBLIC :: SSU_Input_WriteFile


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE SSU_Input_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: SSU_Input_Define.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Release and version
  INTEGER, PARAMETER :: SSU_INPUT_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: SSU_INPUT_VERSION = 1  ! This is just the default data version.
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  ! Message length
  INTEGER,  PARAMETER :: ML = 256
  ! SSU instrument specific data
  INTEGER,  PARAMETER :: MAX_N_CHANNELS = 3


  !--------------------
  ! Structure defintion
  !--------------------
  !:tdoc+:
  TYPE :: SSU_Input_type
    PRIVATE
    ! Release and version information
    INTEGER(Long) :: Release = SSU_INPUT_RELEASE
    INTEGER(Long) :: Version = SSU_INPUT_VERSION
    ! Time in decimal year (e.g. 2009.08892694 corresponds to 11:00 Feb. 2, 2009)
    REAL(Double) :: Time = ZERO
    ! SSU CO2 cell pressures (hPa)
    REAL(Double) :: Cell_Pressure(MAX_N_CHANNELS) = ZERO
  END TYPE SSU_Input_type
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
!       SSU_Input_IsValid
!
! PURPOSE:
!       Non-pure function to perform some simple validity checks on a
!       SSU_Input object.
!
!       If invalid data is found, a message is printed to stdout.
!
! CALLING SEQUENCE:
!       result = SSU_Input_IsValid( ssu )
!
!         or
!
!       IF ( SSU_Input_IsValid( ssu ) ) THEN....
!
! OBJECTS:
!       ssu:       SSU_Input object which is to have its
!                  contents checked.
!                  UNITS:      N/A
!                  TYPE:       SSU_Input_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:    Logical variable indicating whether or not the input
!                  passed the check.
!                  If == .FALSE., object is unused or contains
!                                 invalid data.
!                     == .TRUE.,  object can be used.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION SSU_Input_IsValid( ssu ) RESULT( IsValid )
    TYPE(SSU_Input_type), INTENT(IN) :: ssu
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SSU_Input_IsValid'
    CHARACTER(ML) :: msg

    ! Setup
    IsValid = .TRUE.

    ! Check time
    IF ( ssu%Time < ZERO ) THEN
      msg = 'Invalid mission time'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF

    ! Check cell pressures
    IF ( ANY(ssu%Cell_Pressure < ZERO) ) THEN
      msg = 'Invalid cell pressures'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF

  END FUNCTION SSU_Input_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SSU_Input_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of an SSU_Input object to stdout.
!
! CALLING SEQUENCE:
!       CALL SSU_Input_Inspect( ssu )
!
! INPUTS:
!       ssu:           SSU_Input object to display.
!                      UNITS:      N/A
!                      TYPE:       SSU_Input_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE SSU_Input_Inspect(ssu)
    TYPE(SSU_Input_type), INTENT(IN) :: ssu
    WRITE(*,'(3x,"SSU_Input OBJECT")')
    WRITE(*,'(5x,"Mission time:",1x,es22.15)') ssu%Time
    WRITE(*,'(5x,"Channel cell pressures:",10(1x,es22.15,:))') ssu%Cell_Pressure
  END SUBROUTINE SSU_Input_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SSU_Input_ValidRelease
!
! PURPOSE:
!       Function to check the SSU_Input Release value.
!
! CALLING SEQUENCE:
!       IsValid = SSU_Input_ValidRelease( SSU_Input )
!
! INPUTS:
!       SSU_Input:    SSU_Input object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       SSU_Input_type
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

  FUNCTION SSU_Input_ValidRelease( self ) RESULT( IsValid )
    ! Arguments
    TYPE(SSU_Input_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SSU_Input_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( self%Release < SSU_INPUT_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An SSU_Input data update is needed. ", &
                  &"SSU_Input release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, SSU_INPUT_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF


    ! Check release is not too new
    IF ( self%Release > SSU_INPUT_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An SSU_Input software update is needed. ", &
                  &"SSU_Input release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, SSU_INPUT_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF

  END FUNCTION SSU_Input_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SSU_Input_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL SSU_Input_DefineVersion( Id )
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

  SUBROUTINE SSU_Input_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE SSU_Input_DefineVersion


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SSU_Input_SetValue
!
! PURPOSE:
!       Elemental subroutine to set the values of SSU_Input
!       object components.
!
! CALLING SEQUENCE:
!       CALL SSU_Input_SetValue( SSU_Input                    , &
!                                Time          = Time         , &
!                                Cell_Pressure = Cell_Pressure, &
!                                Channel       = Channel        )
!
! OBJECTS:
!       SSU_Input:            SSU_Input object for which component values
!                             are to be set.
!                             UNITS:      N/A
!                             TYPE:       SSU_Input_type
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Time:                 SSU instrument mission time.
!                             UNITS:      decimal year
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as SSU_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Cell_Pressure:        SSU channel CO2 cell pressure. Must be
!                             specified with the Channel optional dummy
!                             argument.
!                             UNITS:      hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as SSU_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Channel:              SSU channel for which the CO2 cell pressure
!                             is to be set. Must be specified with the
!                             Cell_Pressure optional dummy argument.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar or same as SSU_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SSU_Input_SetValue ( &
    SSU_Input    , &
    Time         , &
    Cell_Pressure, &
    Channel        )
    ! Arguments
    TYPE(SSU_Input_type), INTENT(IN OUT) :: SSU_Input
    REAL(fp),   OPTIONAL, INTENT(IN)     :: Time
    REAL(fp),   OPTIONAL, INTENT(IN)     :: Cell_Pressure
    INTEGER,    OPTIONAL, INTENT(IN)     :: Channel
    ! Variables
    INTEGER :: n

    ! Set values
    IF ( PRESENT(Time) ) SSU_Input%Time = Time
    IF ( PRESENT(Channel) .AND. PRESENT(Cell_Pressure) ) THEN
      n = MAX(MIN(Channel,MAX_N_CHANNELS),1)
      SSU_Input%Cell_Pressure(n) = Cell_Pressure
    END IF

  END SUBROUTINE SSU_Input_SetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SSU_Input_GetValue
!
! PURPOSE:
!       Elemental subroutine to Get the values of SSU_Input
!       object components.
!
! CALLING SEQUENCE:
!       CALL SSU_Input_GetValue( SSU_Input                    , &
!                                Channel       = Channel      , &
!                                Time          = Time         , &
!                                Cell_Pressure = Cell_Pressure, &
!                                n_Channels    = n_Channels     )
!
! OBJECTS:
!       SSU_Input:            SSU_Input object for which component values
!                             are to be set.
!                             UNITS:      N/A
!                             TYPE:       SSU_Input_type
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Channel:              SSU channel for which the CO2 cell pressure
!                             is required.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar or same as SSU_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       Time:                 SSU instrument mission time.
!                             UNITS:      decimal year
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as SSU_Input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Cell_Pressure:        SSU channel CO2 cell pressure. Must be
!                             specified with the Channel optional input
!                             dummy argument.
!                             UNITS:      hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as SSU_Input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:           Number of SSU channels..
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar or same as SSU_Input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SSU_Input_GetValue( &
    SSU_Input    , &
    Channel      , &
    Time         , &
    Cell_Pressure, &
    n_Channels     )
    ! Arguments
    TYPE(SSU_Input_type), INTENT(IN)  :: SSU_Input
    INTEGER,    OPTIONAL, INTENT(IN)  :: Channel
    REAL(fp),   OPTIONAL, INTENT(OUT) :: Time
    REAL(fp),   OPTIONAL, INTENT(OUT) :: Cell_Pressure
    INTEGER,    OPTIONAL, INTENT(OUT) :: n_Channels
    ! Variables
    INTEGER :: n

    ! Get values
    IF ( PRESENT(Time) ) Time = SSU_Input%Time
    IF ( PRESENT(Channel) .AND. PRESENT(Cell_Pressure) ) THEN
      n = MAX(MIN(Channel,MAX_N_CHANNELS),1)
      Cell_Pressure = SSU_Input%Cell_Pressure(n)
    END IF
    IF ( PRESENT(n_Channels) ) n_Channels = MAX_N_CHANNELS

  END SUBROUTINE SSU_Input_GetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SSU_Input_CellPressureIsSet
!
! PURPOSE:
!       Elemental function to determine if SSU_Input object cell pressures
!       are set (i.e. > zero).
!
! CALLING SEQUENCE:
!       result = SSU_Input_CellPressureIsSet( ssu )
!
!         or
!
!       IF ( SSU_Input_CellPressureIsSet( ssu ) ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       ssu:       SSU_Input object for which the cell pressures
!                  are to be tested.
!                  UNITS:      N/A
!                  TYPE:       SSU_Input_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:    Logical variable indicating whether or not all the
!                  SSU cell pressures are set.
!                  If == .FALSE., cell pressure values are <= 0.0hPa and
!                                 thus are considered to be NOT set or valid.
!                     == .TRUE.,  cell pressure values are > 0.0hPa and
!                                 thus are considered to be set and valid.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION SSU_Input_CellPressureIsSet( ssu ) RESULT( Is_Set )
    TYPE(SSU_Input_type), INTENT(IN) :: ssu
    LOGICAL :: Is_Set
    Is_Set = ALL(ssu%Cell_Pressure > ZERO)
  END FUNCTION SSU_Input_CellPressureIsSet


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SSU_Input_ReadFile
!
! PURPOSE:
!       Function to read SSU_Input object files.
!
! CALLING SEQUENCE:
!       Error_Status = SSU_Input_ReadFile( &
!                        SSU_Input          , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! OBJECTS:
!       SSU_Input:      SSU_Input object containing the data read from file.
!                       UNITS:      N/A
!                       TYPE:       SSU_Input_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       SSU_Input data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the SSU_Input data is embedded within another file.
!                       If == .FALSE., File is closed upon function exit [DEFAULT].
!                          == .TRUE.,  File is NOT closed upon function exit
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
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

  FUNCTION SSU_Input_ReadFile( &
    SSU_Input, &  ! Output
    Filename , &  ! Input
    No_Close , &  ! Optional input
    Quiet    , &  ! Optional input
    Title    , &  ! Optional output
    History  , &  ! Optional output
    Comment  , &  ! Optional output
    Debug    ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(SSU_Input_type),   INTENT(OUT) :: SSU_Input
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SSU_Input_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: n_channels
    TYPE(SSU_Input_type) :: dummy

    ! Setup
    err_stat = SUCCESS
    ! ...Check No_Close argument
    close_file = .TRUE.
    IF ( PRESENT(No_Close) ) close_file = .NOT. No_Close
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF


    ! Check if the file is open.
    IF ( File_Open( Filename ) ) THEN
      ! ...Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=fid )
      ! ...Ensure it's valid
      IF ( fid < 0 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        CALL Read_CleanUp(); RETURN
      END IF
    ELSE
      ! ...Open the file if it exists
      IF ( File_Exists( Filename ) ) THEN
        err_stat = Open_Binary_File( Filename, fid )
        IF ( err_Stat /= SUCCESS ) THEN
          msg = 'Error opening '//TRIM(Filename)
          CALL Read_CleanUp(); RETURN
        END IF
      ELSE
        msg = 'File '//TRIM(Filename)//' not found.'
        CALL Read_CleanUp(); RETURN
      END IF
    END IF


    ! Read and check the release and version
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      dummy%Release, &
      dummy%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( .NOT. SSU_Input_ValidRelease( dummy ) ) THEN
      msg = 'SSU_Input Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      n_channels
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading data dimensions - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...and check 'em
    IF ( n_channels /= MAX_N_CHANNELS ) THEN
      msg = 'Invalid channel dimension'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Explicitly assign the version number
    SSU_Input%Version = dummy%Version


    ! Read the global attributes
    err_stat = ReadGAtts_Binary_File( &
                 fid, &
                 Title   = Title  , &
                 History = History, &
                 Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the decimal time
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      SSU_Input%Time
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading decimal time - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the cell pressures
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      SSU_Input%Cell_Pressure
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading CO2 cell pressures - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Close the file
    IF ( close_file ) THEN
      CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Read_Cleanup(); RETURN
      END IF
    END IF

   CONTAINS

     SUBROUTINE Read_CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Read_CleanUp

  END FUNCTION SSU_Input_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SSU_Input_WriteFile
!
! PURPOSE:
!       Function to write SSU_Input object files.
!
! CALLING SEQUENCE:
!       Error_Status = SSU_Input_WriteFile( &
!                        SSU_Input          , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! OBJECTS:
!       SSU_Input:      SSU_Input object containing the data to write to file.
!                       UNITS:      N/A
!                       TYPE:       SSU_Input_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       SSU_Input format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the SSU_Input data is to be embedded within another file.
!                       If == .FALSE., File is closed upon function exit [DEFAULT].
!                          == .TRUE.,  File is NOT closed upon function exit
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
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
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file write was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION SSU_Input_WriteFile( &
    SSU_Input, &  ! Input
    Filename , &  ! Input
    No_Close , &  ! Optional input
    Quiet    , &  ! Optional input
    Title    , &  ! Optional input
    History  , &  ! Optional input
    Comment  , &  ! Optional input
    Debug    ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(SSU_Input_type),   INTENT(IN) :: SSU_Input
    CHARACTER(*),           INTENT(IN) :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SSU_Input_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid


    ! Setup
    err_stat = SUCCESS
    ! ...Check No_Close argument
    close_file = .TRUE.
    IF ( PRESENT(No_Close) ) close_file = .NOT. No_Close
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF


    ! Check if the file is open.
    IF ( File_Open( FileName ) ) THEN
      ! ...Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=fid )
      ! ...Ensure it's valid
      IF ( fid < 0 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        CALL Write_CleanUp(); RETURN
      END IF
    ELSE
      ! ...Open the file for output
      err_stat = Open_Binary_File( Filename, fid, For_Output=.TRUE. )
      IF ( err_Stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        CALL Write_CleanUp(); RETURN
      END IF
    END IF


    ! Write the release and version
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      SSU_Input%Release, &
      SSU_Input%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Release/Version - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      MAX_N_CHANNELS
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing channel dimension - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the global attributes
    err_stat = WriteGAtts_Binary_File( &
                 fid, &
                 Write_Module = MODULE_VERSION_ID, &
                 Title        = Title  , &
                 History      = History, &
                 Comment      = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attributes'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the decimal time
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      SSU_Input%Time
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing decimal time - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the cell_pressures
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      SSU_Input%Cell_Pressure
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing cell pressures - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Close the file
    IF ( close_file ) THEN
      CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF

   CONTAINS

     SUBROUTINE Write_Cleanup()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing output file during error cleanup - '//TRIM(io_msg)
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Write_Cleanup

  END FUNCTION SSU_Input_WriteFile



!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ELEMENTAL FUNCTION SSU_Input_Equal(x, y) RESULT(is_equal)
    TYPE(SSU_Input_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    is_equal = (x%Time .EqualTo. y%Time) .AND. &
               ALL(x%Cell_Pressure .EqualTo. y%Cell_Pressure)
  END FUNCTION SSU_Input_Equal

END MODULE SSU_Input_Define
