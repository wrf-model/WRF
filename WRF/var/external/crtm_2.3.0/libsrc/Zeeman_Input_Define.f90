!
! Zeeman_Input_Define
!
! Module containing the structure definition and associated routines
! for CRTM inputs specific to Zeeman
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 26-Oct-2009
!                       paul.vandelst@noaa.gov
!

MODULE Zeeman_Input_Define

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
  PUBLIC :: Zeeman_Input_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: Zeeman_Input_GetValue
  PUBLIC :: Zeeman_Input_SetValue
  PUBLIC :: Zeeman_Input_IsValid
  PUBLIC :: Zeeman_Input_Inspect
  PUBLIC :: Zeeman_Input_DefineVersion
  PUBLIC :: Zeeman_Input_ValidRelease
  PUBLIC :: Zeeman_Input_ReadFile
  PUBLIC :: Zeeman_Input_WriteFile


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE Zeeman_Input_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Zeeman_Input_Define.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  ! Release and version
  INTEGER, PARAMETER :: ZEEMAN_INPUT_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: ZEEMAN_INPUT_VERSION = 1  ! This is just the default data version.
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  ! Zeeman specific data
  REAL(Double), PARAMETER :: DEFAULT_MAGENTIC_FIELD = 0.3_Double


  !--------------------
  ! Structure defintion
  !--------------------
  !:tdoc+:
  TYPE :: Zeeman_Input_type
    PRIVATE
    ! Release and version information
    INTEGER(Long) :: Release = ZEEMAN_INPUT_RELEASE
    INTEGER(Long) :: Version = ZEEMAN_INPUT_VERSION
    ! Earth magnetic field strength in Gauss
    REAL(Double) :: Be = DEFAULT_MAGENTIC_FIELD
    ! Cosine of the angle between the Earth
    ! magnetic field and wave propagation direction
    REAL(Double) :: Cos_ThetaB = ZERO
    ! Cosine of the azimuth angle of the Be vector.
    REAL(Double) :: Cos_PhiB = ZERO
    ! Doppler frequency shift caused by Earth-rotation.
    REAL(Double) :: Doppler_Shift = ZERO
  END TYPE Zeeman_Input_type
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
!       Zeeman_Input_SetValue
!
! PURPOSE:
!       Elemental subroutine to set the values of Zeeman_Input
!       object components.
!
! CALLING SEQUENCE:
!       CALL Zeeman_Input_SetValue( Zeeman_Input                   , &
!                                   Field_Strength = Field_Strength, &
!                                   Cos_ThetaB     = Cos_ThetaB    , &
!                                   Cos_PhiB       = Cos_PhiB      , &
!                                   Doppler_Shift  = Doppler_Shift   )
!
! OBJECTS:
!       Zeeman_Input:         Zeeman_Input object for which component values
!                             are to be set.
!                             UNITS:      N/A
!                             TYPE:       Zeeman_Input_type
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Field_Strength:       Earth's magnetic filed strength
!                             UNITS:      Gauss
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Cos_ThetaB:           Cosine of the angle between the Earth magnetic
!                             field and wave propagation vectors.
!                             UNITS:      N/A
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Cos_PhiB:             Cosine of the azimuth angle of the Earth magnetic
!                             field vector.
!                             UNITS:      N/A
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Doppler_Shift:        Doppler frequency shift caused by Earth-rotation.
!                             Positive towards sensor.
!                             UNITS:      KHz
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Zeeman_Input_SetValue( &
    Zeeman_Input  , &
    Field_Strength, &
    Cos_ThetaB    , &
    Cos_PhiB      , &
    Doppler_Shift   )
    ! Arguments
    TYPE(Zeeman_Input_type), INTENT(IN OUT) :: Zeeman_Input
    REAL(fp),      OPTIONAL, INTENT(IN)     :: Field_Strength
    REAL(fp),      OPTIONAL, INTENT(IN)     :: Cos_ThetaB
    REAL(fp),      OPTIONAL, INTENT(IN)     :: Cos_PhiB
    REAL(fp),      OPTIONAL, INTENT(IN)     :: Doppler_Shift
    ! Set components
    IF ( PRESENT(Field_Strength) ) Zeeman_Input%Be            = Field_Strength
    IF ( PRESENT(Cos_ThetaB    ) ) Zeeman_Input%Cos_ThetaB    = Cos_ThetaB
    IF ( PRESENT(Cos_PhiB      ) ) Zeeman_Input%Cos_PhiB      = Cos_PhiB
    IF ( PRESENT(Doppler_Shift ) ) Zeeman_Input%Doppler_Shift = Doppler_Shift
  END SUBROUTINE Zeeman_Input_SetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Zeeman_Input_GetValue
!
! PURPOSE:
!       Elemental subroutine to get the values of Zeeman_Input
!       object components.
!
! CALLING SEQUENCE:
!       CALL Zeeman_Input_GetValue( Zeeman_Input                   , &
!                                   Field_Strength = Field_Strength, &
!                                   Cos_ThetaB     = Cos_ThetaB    , &
!                                   Cos_PhiB       = Cos_PhiB      , &
!                                   Doppler_Shift  = Doppler_Shift   )
!
! OBJECTS:
!       Zeeman_Input:         Zeeman_Input object for which component values
!                             are to be set.
!                             UNITS:      N/A
!                             TYPE:       Zeeman_Input_type
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUTS:
!       Field_Strength:       Earth's magnetic filed strength
!                             UNITS:      Gauss
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Cos_ThetaB:           Cosine of the angle between the Earth magnetic
!                             field and wave propagation vectors.
!                             UNITS:      N/A
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Cos_PhiB:             Cosine of the azimuth angle of the Earth magnetic
!                             field vector.
!                             UNITS:      N/A
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Doppler_Shift:        Doppler frequency shift caused by Earth-rotation.
!                             Positive towards sensor.
!                             UNITS:      KHz
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Zeeman_Input_GetValue( &
    Zeeman_Input  , &
    Field_Strength, &
    Cos_ThetaB    , &
    Cos_PhiB      , &
    Doppler_Shift   )
    ! Arguments
    TYPE(Zeeman_Input_type),INTENT(IN)  :: Zeeman_Input
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Field_Strength
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Cos_ThetaB
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Cos_PhiB
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Doppler_Shift
    ! Get components
    IF ( PRESENT(Field_Strength) ) Field_Strength = Zeeman_Input%Be
    IF ( PRESENT(Cos_ThetaB    ) ) Cos_ThetaB     = Zeeman_Input%Cos_ThetaB
    IF ( PRESENT(Cos_PhiB      ) ) Cos_PhiB       = Zeeman_Input%Cos_PhiB
    IF ( PRESENT(Doppler_Shift ) ) Doppler_Shift  = Zeeman_Input%Doppler_Shift
  END SUBROUTINE Zeeman_Input_GetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Zeeman_Input_IsValid
!
! PURPOSE:
!       Non-pure function to perform some simple validity checks on a
!       Zeeman_Input object.
!
!       If invalid data is found, a message is printed to stdout.
!
! CALLING SEQUENCE:
!       result = Zeeman_Input_IsValid( z )
!
!         or
!
!       IF ( Zeeman_Input_IsValid( z ) ) THEN....
!
! OBJECTS:
!       z:         Zeeman_Input object which is to have its
!                  contents checked.
!                  UNITS:      N/A
!                  TYPE:       Zeeman_Input_type
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

  FUNCTION Zeeman_Input_IsValid( z ) RESULT( IsValid )
    TYPE(Zeeman_Input_type), INTENT(IN) :: z
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Zeeman_Input_IsValid'
!!!
real(fp), parameter :: big_number = 1.0e+09_fp
!!!
    CHARACTER(ML) :: msg

    ! Setup
    IsValid = .TRUE.

    ! Check components
    IF ( z%Be < ZERO ) THEN
      msg = 'Invalid field strength'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    IF ( z%Cos_ThetaB > big_number ) THEN
      msg = 'Invalid COS(ThetaB)'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    IF ( z%Cos_PhiB > big_number ) THEN
      msg = 'Invalid COS(PhiB)'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    IF ( ABS(z%Doppler_Shift) > big_number ) THEN
      msg = 'Invalid Doppler shift'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF

  END FUNCTION Zeeman_Input_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Zeeman_Input_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of an Zeeman_Input object to stdout.
!
! CALLING SEQUENCE:
!       CALL Zeeman_Input_Inspect( z )
!
! INPUTS:
!       z:             Zeeman_Input object to display.
!                      UNITS:      N/A
!                      TYPE:       Zeeman_Input_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Zeeman_Input_Inspect(z)
    TYPE(Zeeman_Input_type), INTENT(IN) :: z
    WRITE(*,'(3x,"Zeeman_Input OBJECT")')
    WRITE(*,'(5x,"Field strength (gauss):",1x,es22.15)') z%Be
    WRITE(*,'(5x,"COS(ThetaB)           :",1x,es22.15)') z%Cos_ThetaB
    WRITE(*,'(5x,"COS(PhiB)             :",1x,es22.15)') z%Cos_PhiB
    WRITE(*,'(5x,"Doppler shift (KHz)   :",1x,es22.15)') z%Doppler_Shift
  END SUBROUTINE Zeeman_Input_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Zeeman_Input_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL Zeeman_Input_DefineVersion( Id )
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

  SUBROUTINE Zeeman_Input_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE Zeeman_Input_DefineVersion


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Zeeman_Input_ValidRelease
!
! PURPOSE:
!       Function to check the Zeeman_Input Release value.
!
! CALLING SEQUENCE:
!       IsValid = Zeeman_Input_ValidRelease( Zeeman_Input )
!
! INPUTS:
!       Zeeman_Input:  Zeeman_Input object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       Zeeman_Input_type
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

  FUNCTION Zeeman_Input_ValidRelease( self ) RESULT( IsValid )
    ! Arguments
    TYPE(Zeeman_Input_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Zeeman_Input_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( self%Release < ZEEMAN_INPUT_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An Zeeman_Input data update is needed. ", &
                  &"Zeeman_Input release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, ZEEMAN_INPUT_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF


    ! Check release is not too new
    IF ( self%Release > ZEEMAN_INPUT_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An Zeeman_Input software update is needed. ", &
                  &"Zeeman_Input release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, ZEEMAN_INPUT_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF

  END FUNCTION Zeeman_Input_ValidRelease



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Zeeman_Input_ReadFile
!
! PURPOSE:
!       Function to read Zeeman_Input object files.
!
! CALLING SEQUENCE:
!       Error_Status = Zeeman_Input_ReadFile( &
!                        Zeeman_Input       , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! OBJECTS:
!       Zeeman_Input:   Zeeman_Input object containing the data read from file.
!                       UNITS:      N/A
!                       TYPE:       Zeeman_Input_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       Zeeman_Input data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the Zeeman_Input data is embedded within another file.
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

  FUNCTION Zeeman_Input_ReadFile( &
    Zeeman_Input, &  ! Output
    Filename    , &  ! Input
    No_Close    , &  ! Optional input
    Quiet       , &  ! Optional input
    Title       , &  ! Optional output
    History     , &  ! Optional output
    Comment     , &  ! Optional output
    Debug       ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(Zeeman_Input_type), INTENT(OUT) :: Zeeman_Input
    CHARACTER(*),            INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL,  INTENT(IN)  :: No_Close
    LOGICAL,      OPTIONAL,  INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL,  INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL,  INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL,  INTENT(OUT) :: Comment
    LOGICAL,      OPTIONAL,  INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Zeeman_Input_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(Zeeman_Input_type) :: dummy

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
    IF ( .NOT. Zeeman_Input_ValidRelease( dummy ) ) THEN
      msg = 'Zeeman_Input Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Explicitly assign the version number
    Zeeman_Input%Version = dummy%Version


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


    ! Read the scalars
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      Zeeman_Input%Be           , &
      Zeeman_Input%Cos_ThetaB   , &
      Zeeman_Input%Cos_PhiB     , &
      Zeeman_Input%Doppler_Shift
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading data - '//TRIM(io_msg)
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

  END FUNCTION Zeeman_Input_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Zeeman_Input_WriteFile
!
! PURPOSE:
!       Function to write Zeeman_Input object files.
!
! CALLING SEQUENCE:
!       Error_Status = Zeeman_Input_WriteFile( &
!                        Zeeman_Input       , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! OBJECTS:
!       Zeeman_Input:   Zeeman_Input object containing the data to write to file.
!                       UNITS:      N/A
!                       TYPE:       Zeeman_Input_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       Zeeman_Input format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the Zeeman_Input data is to be embedded within another file.
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

  FUNCTION Zeeman_Input_WriteFile( &
    Zeeman_Input, &  ! Input
    Filename , &  ! Input
    No_Close , &  ! Optional input
    Quiet    , &  ! Optional input
    Title    , &  ! Optional input
    History  , &  ! Optional input
    Comment  , &  ! Optional input
    Debug    ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(Zeeman_Input_type), INTENT(IN) :: Zeeman_Input
    CHARACTER(*),            INTENT(IN) :: Filename
    LOGICAL,      OPTIONAL,  INTENT(IN) :: No_Close
    LOGICAL,      OPTIONAL,  INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL,  INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL,  INTENT(IN) :: History
    CHARACTER(*), OPTIONAL,  INTENT(IN) :: Comment
    LOGICAL,      OPTIONAL,  INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Zeeman_Input_WriteFile'
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
      Zeeman_Input%Release, &
      Zeeman_Input%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Release/Version - '//TRIM(io_msg)
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


    ! Write the scalars
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      Zeeman_Input%Be           , &
      Zeeman_Input%Cos_ThetaB   , &
      Zeeman_Input%Cos_PhiB     , &
      Zeeman_Input%Doppler_Shift
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing data - '//TRIM(io_msg)
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

  END FUNCTION Zeeman_Input_WriteFile



!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ELEMENTAL FUNCTION Zeeman_Input_Equal(x, y) RESULT(is_equal)
    TYPE(Zeeman_Input_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    is_equal = (x%Be            .EqualTo. y%Be           ) .AND. &
               (x%Cos_ThetaB    .EqualTo. y%Cos_ThetaB   ) .AND. &
               (x%Cos_PhiB      .EqualTo. y%Cos_PhiB     ) .AND. &
               (x%Doppler_Shift .EqualTo. y%Doppler_Shift)
  END FUNCTION Zeeman_Input_Equal

END MODULE Zeeman_Input_Define
