!
! EmisCoeff_Binary_IO
!
! Module containing routines to inquire, read and write Binary format
! EmisCoeff files.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE EmisCoeff_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,          ONLY: Long
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE EmisCoeff_Define,    ONLY: EmisCoeff_Type, &
                                 N_EMISCOEFF_ITEMS, &
                                 EMISCOEFF_DATA_TYPE, &
                                 EMISCOEFF_DATA_NAME, &
                                 SPECTRAL_EMISCOEFF_TYPE, &
                                 SENSOR_EMISCOEFF_TYPE, &
                                 Associated_EmisCoeff, &
                                 Allocate_EmisCoeff, &
                                 Destroy_EmisCoeff, &
                                 Check_EmisCoeff_Release, &
                                 Info_EmisCoeff
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_EmisCoeff_Binary
  PUBLIC :: Read_EmisCoeff_Binary
  PUBLIC :: Write_EmisCoeff_Binary


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: EmisCoeff_Binary_IO.f90 7839 2010-05-13 13:20:20Z david.groff@noaa.gov $'
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1


CONTAINS


!------------------------------------------------------------------------------
!
! NAME:
!       Inquire_EmisCoeff_Binary
!
! PURPOSE:
!       Function to inquire a Binary EmisCoeff format file to obtain
!       the dimension values and release information.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_EmisCoeff_Binary( Filename,                      &  ! Input
!                                                n_Angles      = n_Angles,      &  ! Optional output
!                                                n_Frequencies = n_Frequencies, &  ! Optional output
!                                                n_Wind_Speeds = n_Wind_Speeds, &  ! Optional output
!                                                Release       = Release,       &  ! Optional Output
!                                                Version       = Version,       &  ! Optional Output
!                                                RCS_Id        = RCS_Id,        &  ! Revision control
!                                                Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:         Character string specifying the name of the binary
!                         EmisCoeff data file to inquire.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         Messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output Messages to standard output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Angles:         The angle dimension of the emissivity data.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       n_Frequencies:    The frequency dimension of the emissivity data.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       n_Wind_Speeds:    The wind speed dimension of the emissivity data.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Release:          The EmisCoeff data/file release number. Used to check
!                         for data/software mismatch.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:          The EmisCoeff data/file version number. Used for
!                         purposes only in identifying the dataset for
!                         a particular release.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the Binary file inquiry was successful
!                            == FAILURE an unrecoverable error occurred.
!                            == WARNING an error occurred closing the file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Inquire_EmisCoeff_Binary( Filename,       &  ! Input
                                     n_Angles,       &  ! Optional output
                                     n_Frequencies,  &  ! Optional output
                                     n_Wind_Speeds,  &  ! Optional output
                                     Release,        &  ! Optional Output
                                     Version,        &  ! Optional Output
                                     RCS_Id,         &  ! Revision control
                                     Message_Log )   &  ! Error messaging
                                   RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Angles
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Frequencies
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Wind_Speeds
    INTEGER,      OPTIONAL, INTENT(OUT) :: Release
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_EmisCoeff_Binary'
    ! Function variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER(Long) :: Spectral_or_Sensor
    INTEGER(Long) :: File_Release
    INTEGER(Long) :: File_Version
    INTEGER(Long) :: File_n_Angles
    INTEGER(Long) :: File_n_Frequencies
    INTEGER(Long) :: File_n_Wind_Speeds
 
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Open the Binary format EmisCoeff file
    ! -------------------------------------
    Error_Status = Open_Binary_File( Filename, &
                                     FileID,   &
                                     Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening EmisCoeff file '//&
                            TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Read and check the EmisCoeff type
    ! ---------------------------------
    READ( FileID, IOSTAT = IO_Status ) Spectral_or_Sensor
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( message, '( "Error reading EmisCoeff file data type from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF
    IF ( Spectral_or_Sensor /= SPECTRAL_EMISCOEFF_TYPE) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'EmisCoeff file '//TRIM( Filename )//&
                            ' not a Spectral file.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! Read the Release/Version information
    ! ------------------------------------
    READ( FileID, IOSTAT = IO_Status ) File_Release, &
                                       File_Version
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( message, '( "Error reading EmisCoeff file Release/Version values from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! Read the dimensions
    ! -------------------
    READ( FileID, IOSTAT = IO_Status ) File_n_Angles, &
                                       File_n_Frequencies, &
                                       File_n_Wind_Speeds
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( message, '( "Error reading data dimensions from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! Close the file
    ! --------------
    CLOSE( FileID, STATUS = 'KEEP', &
                   IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Assign the return arguments
    ! ---------------------------
    IF ( PRESENT( n_Angles      ) ) n_Angles      = File_n_Angles
    IF ( PRESENT( n_Frequencies ) ) n_Frequencies = File_n_Frequencies
    IF ( PRESENT( n_Wind_Speeds ) ) n_Wind_Speeds = File_n_Wind_Speeds
    IF ( PRESENT( Release       ) ) Release       = File_Release
    IF ( PRESENT( Version       ) ) Version       = File_Version

  END FUNCTION Inquire_EmisCoeff_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       Read_EmisCoeff_Binary
!
! PURPOSE:
!       Function to read a Binary format EmisCoeff file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_EmisCoeff_Binary( Filename,                              &  ! Input
!                                             EmisCoeff,                             &  ! Output
!                                             Quiet             = Quiet,             &  ! Optional input
!                                             Process_ID        = Process_ID,        &  ! Optional input
!                                             Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                             RCS_Id            = RCS_Id,            &  ! Revision control
!                                             Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the Binary
!                           format EmisCoeff data file to read.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:              Set this argument to suppress INFORMATION messages
!                           being printed to standard output (or the message
!                           log file if the Message_Log optional argument is
!                           used.) By default, INFORMATION messages are printed.
!                           If QUIET = 0, INFORMATION messages are OUTPUT.
!                              QUIET = 1, INFORMATION messages are SUPPRESSED.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATION message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Output_Process_ID:  Set this argument to the MPI process ID in which
!                           all INFORMATION messages are to be output. If
!                           the passed Process_ID value agrees with this value
!                           the INFORMATION messages are output. 
!                           This argument is ignored if the Quiet argument
!                           is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           Messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output Messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       EmisCoeff:          Structure containing the emmisivity coefficient data
!                           read from the file.
!                           UNITS:      N/A
!                           TYPE:       EmisCoeff_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the Binary file read was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If the EmisCoeff argument is defined upon input, it is redefined (or
!       reinitialised) at output.
!
! COMMENTS:
!       Note the INTENT on the output EmisCoeff argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_EmisCoeff_Binary( Filename,          &  ! Input
                                  EmisCoeff,         &  ! Output
                                  Quiet,             &  ! Optional input
                                  Process_ID,        &  ! Optional input
                                  Output_Process_ID, &  ! Optional input
                                  RCS_Id,            &  ! Revision control
                                  Message_Log )      &  ! Error messaging
                                RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: Filename
    TYPE(EmisCoeff_type),   INTENT(IN OUT) :: EmisCoeff
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER,      OPTIONAL, INTENT(IN)     :: Process_ID
    INTEGER,      OPTIONAL, INTENT(IN)     :: Output_Process_ID
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_EmisCoeff_Binary'
    ! Function variables
    CHARACTER(1000) :: Message
    CHARACTER(128) :: Process_ID_Tag
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER(Long) :: Spectral_or_Sensor
    INTEGER(Long) :: n_Angles
    INTEGER(Long) :: n_Frequencies
    INTEGER(Long) :: n_Wind_Speeds
    INTEGER(Long) :: n_Items, n
    INTEGER(Long), DIMENSION(N_EMISCOEFF_ITEMS) :: Data_Type
 
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless....
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF
    IF ( Noisy .AND. PRESENT(Process_ID) .AND. PRESENT(Output_Process_ID) ) THEN
      IF ( Process_ID /= Output_Process_ID ) Noisy = .FALSE.
    END IF

    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '( ";  MPI Prcess ID: ", i5 )' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF


    ! Open the EmisCoeff file
    ! ------------------------
    Error_Status = Open_Binary_File( Filename, &
                                     FileID,   &
                                     Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Read and check the EmisCoeff structure type
    ! -------------------------------------------
    READ( FileID, IOSTAT = IO_Status ) Spectral_or_Sensor
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading EmisCoeff file data type from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF
    IF ( Spectral_or_Sensor /= SPECTRAL_EMISCOEFF_type) THEN
      Message = 'EmisCoeff file '//TRIM( Filename )//&
                ' not a Spectral file.'
      GOTO 2000
    END IF


    ! Read the Release/Version information
    ! ------------------------------------
    READ( FileID, IOSTAT = IO_Status ) EmisCoeff%Release, &
                                       EmisCoeff%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading EmisCoeff file Release/Version values from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF

    ! Check the release
    Error_Status = Check_EmisCoeff_Release( EmisCoeff, &
                                            Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'EmisCoeff Release check failed for '//TRIM( Filename )
      GOTO 2000
    END IF


    ! Read the dimensions
    ! -------------------
    READ( FileID, IOSTAT = IO_Status ) n_Angles, &
                                       n_Frequencies, &
                                       n_Wind_Speeds
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading data dimensions from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF

    ! Read the number of data items
    READ( FileID, IOSTAT = IO_Status ) n_Items
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading the number of data items from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF

    ! Check that the number of data items is correct
    IF ( n_Items /= N_EMISCOEFF_ITEMS ) THEN
      WRITE( Message, '( "Number of data items in ", a, " (", i2, &
                        &") is inconsistent with definition (", i2, ")." )' ) &
                      TRIM( Filename ), n_Items, N_EMISCOEFF_ITEMS
      GOTO 2000
    END IF

    ! Read the data types
    READ( FileID, IOSTAT = IO_Status ) Data_Type
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading the data items types from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF


    ! Check that the data items types are correct
    DO n = 1, n_Items
      IF ( Data_Type( n ) /= EMISCOEFF_DATA_TYPE(n) ) THEN
        WRITE( Message, '( "Invalid type for data item #", i2, &
                          &", ", a, ", in ", a )' ) &
                        n, TRIM( EMISCOEFF_DATA_NAME(n) ), TRIM( Filename )
        GOTO 2000
      END IF
    END DO


    ! Allocate the EmisCoeff structure for reading
    ! ---------------------------------------------
    Error_Status = Allocate_EmisCoeff( n_Angles, &
                                       n_Frequencies, &
                                       n_Wind_Speeds, &
                                       EmisCoeff, &
                                       Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error occurred allocating EmisCoeff structure.'//TRIM(Process_ID_Tag)
      GOTO 2000
    END IF


    ! Read the dimension data vectors
    ! -------------------------------
    READ( FileID, IOSTAT = IO_Status ) EmisCoeff%Angle
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading angle data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    READ( FileID, IOSTAT = IO_Status ) EmisCoeff%Frequency
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading frequency data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    READ( FileID, IOSTAT = IO_Status ) EmisCoeff%Wind_Speed
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading wind speed data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF


    ! Read the emissivities
    ! ---------------------
    READ( FileID, IOSTAT = IO_Status ) EmisCoeff%Emissivity
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading emissivity data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Close the file
    ! --------------
    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message)//TRIM( Process_ID_Tag ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_EmisCoeff( EmisCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Destroy_Status = Destroy_EmisCoeff(EmisCoeff, Message_Log=Message_Log)
    IF ( Destroy_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error destroying EmisCoeff during error cleanup.'

    2000 CONTINUE
    CLOSE(FileID)
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_EmisCoeff_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       Write_EmisCoeff_Binary
!
! PURPOSE:
!       Function to write a Binary format EmisCoeff data file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_EmisCoeff_Binary( Filename,                 &   ! Input
!                                              EmisCoeff,                &   ! Input
!                                              Quiet       = Quiet,      &   ! Optional input
!                                              RCS_Id      = RCS_Id,     &   ! Revision control
!                                              Message_Log = Message_Log )   ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     EmisCoeff format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       EmisCoeff:    Structure containing the emissivity coefficient data.
!                     UNITS:      N/A
!                     TYPE:       EmisCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information Messages being
!                     printed to standard output (or the Message log file if 
!                     the Message_Log optional argument is used.) By default,
!                     information Messages are printed.
!                     If QUIET = 0, information Messages are OUTPUT.
!                        QUIET = 1, information Messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     Messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output Messages to standard output.
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
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file write was successful
!                        == FAILURE an unrecoverable write error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs in this routine, the output file is deleted
!         before returning to the calling routine.
!
!------------------------------------------------------------------------------

  FUNCTION Write_EmisCoeff_Binary( Filename,     &  ! Input
                                   EmisCoeff,    &  ! Input
                                   Quiet,        &  ! Optional input
                                   RCS_Id,       &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(EmisCoeff_type),   INTENT(IN)  :: EmisCoeff
    INTEGER,        OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_EmisCoeff_Binary'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER(256) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Check structure
    IF ( .NOT. Associated_EmisCoeff( EmisCoeff ) ) THEN
      Message = 'Some or all INPUT EmisCoeff pointer members are NOT associated.'
      GOTO 2000
    END IF

    ! Check the EmisCoeff structure Release
    Error_Status = Check_EmisCoeff_Release( EmisCoeff, &
                                            Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'EmisCoeff Release check failed.'
      GOTO 2000
    END IF

    ! Check the EmisCoeff structure dimensions
    IF ( EmisCoeff%n_Angles      < 1 .OR. &
         EmisCoeff%n_Frequencies < 1 .OR. &
         EmisCoeff%n_Wind_Speeds < 1      ) THEN
      Message = 'One or more dimensions of EmisCoeff structure are < or = 0.'
      GOTO 2000
    END IF

    ! Open the EmisCoeff data file
    ! ----------------------------
    Error_Status = Open_Binary_File( TRIM( Filename ),         &
                                     FileID,                   &
                                     For_Output  = 1,          &
                                     Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM( Filename )
      GOTO 2000
    END IF


    ! Write the type of EmisCoeff structure
    ! -------------------------------------
    WRITE( FileID, IOSTAT = IO_Status ) SPECTRAL_EMISCOEFF_TYPE
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing EmisCoeff structure type to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF


    ! Write the Release/Version information
    ! -------------------------------------
    WRITE( FileID, IOSTAT = IO_Status ) EmisCoeff%Release, &
                                        EmisCoeff%Version 
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing EmisCoeff file Release/Version values to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Write the dimensions
    ! --------------------
    WRITE( FileID, IOSTAT = IO_Status ) EmisCoeff%n_Angles, &
                                        EmisCoeff%n_Frequencies, &
                                        EmisCoeff%n_Wind_Speeds
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing data dimensions to ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Write the number of data items
    WRITE( FileID, IOSTAT = IO_Status ) N_EMISCOEFF_ITEMS
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing the number of data items to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Write the data item types
    WRITE( FileID, IOSTAT = IO_Status ) EMISCOEFF_DATA_TYPE
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing the data item types to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF


    ! Write the dimension data vectors
    ! --------------------------------
    WRITE( FileID, IOSTAT = IO_Status ) EmisCoeff%Angle(1:EmisCoeff%n_Angles)
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing angle data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    WRITE( FileID, IOSTAT = IO_Status ) EmisCoeff%Frequency(1:EmisCoeff%n_Frequencies)
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing frequency data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    WRITE( FileID, IOSTAT = IO_Status ) EmisCoeff%Wind_Speed(1:EmisCoeff%n_Wind_Speeds)
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing wind speed data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF


    ! Write the emissivities
    ! ----------------------
    WRITE( FileID, IOSTAT = IO_Status ) EmisCoeff%Emissivity(1:EmisCoeff%n_Angles, &
                                                             1:EmisCoeff%n_Frequencies, &
                                                             1:EmisCoeff%n_Wind_Speeds)
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing emissivity data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF


    ! Close the file
    ! --------------
    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_EmisCoeff( EmisCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    CLOSE(FileID,STATUS=FILE_STATUS_ON_ERROR)

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
  END FUNCTION Write_EmisCoeff_Binary

END MODULE EmisCoeff_Binary_IO
