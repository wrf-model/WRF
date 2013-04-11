!
! ODPS_Binary_IO
!
! Module containing routines to read and write Binary format
! ODPS files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!       Modified by:    Yong Han, 10-July-2008
!                       Adapted the original code to work for ODPS
!                       algorithm
!

MODULE ODPS_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds         , ONLY: Long
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE ODPS_Define        , ONLY: ODPS_Type            , &
                                 Associated_ODPS      , &
                                 Allocate_ODPS        , &
                                 Allocate_ODPS_OPTRAN , &
                                 Destroy_ODPS         , &
                                 CheckRelease_ODPS    , &
                                 CheckAlgorithm_ODPS  , &
                                 Info_ODPS
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Structure procedures
  PUBLIC :: Inquire_ODPS_Binary
  PUBLIC :: Read_ODPS_Binary
  PUBLIC :: Write_ODPS_Binary
  PUBLIC :: Read_ODPS_Data
  PUBLIC :: Write_ODPS_Data


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id: ODPS_Binary_IO.f90 2169 2008-06-12 15:07:56Z paul.vandelst@noaa.gov $'
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Message character length
  INTEGER, PARAMETER :: ML = 512


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Inquire_ODPS_Binary
!
! PURPOSE:
!       Function to inquire a Binary format ODPS file.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_ODPS_Binary( Filename                           , &  ! Input
!                                           n_Layers         = n_Layers        , &  ! Optional output
!                                           n_Components     = n_Components    , &  ! Optional output
!                                           n_Absorbers      = n_Absorbers     , &  ! Optional output
!                                           n_Channels       = n_Channels      , &  ! Optional output
!                                           n_Coeffs         = n_Coeffs        , &  ! Optional output
!                                           n_OCoeffs        = n_OCoeffs       , &  ! Optional output
!                                           Release          = Release         , &  ! Optional Output
!                                           Version          = Version         , &  ! Optional Output
!                                           Sensor_Id        = Sensor_Id       , &  ! Optional output
!                                           WMO_Satellite_Id = WMO_Satellite_Id, &  ! Optional output
!                                           WMO_Sensor_Id    = WMO_Sensor_Id   , &  ! Optional output
!                                           RCS_Id           = RCS_Id          , &  ! Revision control
!                                           Message_Log      = Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           ODPS data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           Messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output Messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Layers:           The number of profile layers
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Components:       The number of transmittance components (i.g. dry & wlo)
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Absorbers:        The number of absorbers dimension (i.g H2O & O3).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:         The number of channels dimension of the ODPS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Coeffs:           The total number of tau coefficients.
!                           Note, the Coeff data are now stored in a one-dimensional 
!                           array
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_OCoeffs:          The total number of OPTRAN tau coefficients.
!                           Note, the Coeff data are now stored in a one-dimensional 
!                           array
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The ODPS data/file release number. Used to check
!                           for data/software mismatch.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The ODPS data/file version number. Used for
!                           purposes only in identifying the dataset for
!                           a particular release.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Id:          Character string sensor/platform identifier.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_Id:   The WMO code used to identify satellite platforms.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_Id:      The WMO code used to identify sensors.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file inquiry was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Inquire_ODPS_Binary( Filename        , &  ! Input
                                n_Layers        , &  ! Optional output
                                n_Components    , &  ! Optional output
                                n_Absorbers     , &  ! Optional output
                                n_Channels      , &  ! Optional output
                                n_Coeffs        , &  ! Optional output
                                n_OCoeffs       , &  ! Optional output
                                Release         , &  ! Optional Output
                                Version         , &  ! Optional Output
                                Sensor_Id       , &  ! Optional Output
                                WMO_Satellite_Id, &  ! Optional Output
                                WMO_Sensor_Id   , &  ! Optional Output
                                RCS_Id          , &  ! Revision control
                                Message_Log     ) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Layers
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Components
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Absorbers
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Coeffs
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_OCoeffs
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_ODPS_Binary'
    ! Function variables
    CHARACTER(ML) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    TYPE(ODPS_type) :: ODPS  

 
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      Message = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( Filename, &
                                     FileID, &
                                     Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening ODPS Binary file '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the Release and Version information
    ! ----------------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODPS%Release, ODPS%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading Release/Version values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF


    ! Read the Alorithm ID
    ! --------------------
    READ( FileID, IOSTAT=IO_Status ) ODPS%Algorithm
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading Algorithm ID from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF


    ! Read the data dimensions
    ! ------------------------
    READ( FileID, IOSTAT=IO_Status ) ODPS%n_Layers    , &
                                     ODPS%n_Components, &
                                     ODPS%n_Absorbers , &
                                     ODPS%n_Channels  , &
                                     ODPS%n_Coeffs    , &
                                     ODPS%n_OPIndex   , &
                                     ODPS%n_OCoeffs
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading dimension values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF


    ! Read the sensor ids
    ! -------------------
    READ( FileID, IOSTAT=IO_Status ) ODPS%Sensor_Id       , &
                                     ODPS%WMO_Satellite_Id, &
                                     ODPS%WMO_Sensor_Id    
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading sensor information from ",a,&
                       &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF

    
    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error closing ",a,". IOSTAT = ",i0)' ) &
                    TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Assign the return arguments
    ! ---------------------------
    ! Dimensions
    IF ( PRESENT(n_Layers    ) ) n_Layers     = ODPS%n_Layers    
    IF ( PRESENT(n_Components) ) n_Components = ODPS%n_Components 
    IF ( PRESENT(n_Absorbers ) ) n_Absorbers  = ODPS%n_Absorbers 
    IF ( PRESENT(n_Channels  ) ) n_Channels   = ODPS%n_Channels
    IF ( PRESENT(n_Coeffs) )     n_Coeffs     = ODPS%n_Coeffs
    IF ( PRESENT(n_OCoeffs) )    n_OCoeffs    = ODPS%n_OCoeffs

    ! Release/Version information
    IF ( PRESENT(Release) ) Release = ODPS%Release
    IF ( PRESENT(Version) ) Version = ODPS%Version

    ! Sensor ids
    IF ( PRESENT(Sensor_Id       ) ) Sensor_Id        = ODPS%Sensor_Id(1:MIN(LEN(Sensor_Id),LEN_TRIM(ODPS%Sensor_Id)))
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = ODPS%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = ODPS%WMO_Sensor_Id   
    
  CONTAINS
  
    SUBROUTINE Inquire_CleanUp( Close_File )
      INTEGER, OPTIONAL, INTENT(IN) :: Close_File
      CHARACTER(256) :: Close_Message
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File == SET ) THEN
          CLOSE( FileID, IOSTAT=IO_Status )
          IF ( IO_Status /= 0 ) THEN
            WRITE( Close_Message,'("; Error closing input file during error cleanup. IOSTAT=",i0)') &
                                 IO_Status
            Message = TRIM(Message)//TRIM(Close_Message)
          END IF
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION Inquire_ODPS_Binary


!--------------------------------------------------------------------------------
!
! NAME:
!       Read_ODPS_Binary
!
! PURPOSE:
!       Function to read data into an ODPS structure from a Binary format file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_ODPS_Binary( Filename                             , &  ! Input
!                                        ODPS                                 , &  ! Output
!                                        Quiet             = Quiet            , &  ! Optional input
!                                        Process_ID        = Process_ID       , &  ! Optional input
!                                        Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                        RCS_Id            = RCS_Id           , &  ! Revision control
!                                        Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           format ODPS data file to read.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ODPS:               Structure containing the gas absorption coefficient
!                           data read from the file.
!                           UNITS:      N/A
!                           TYPE:       ODPS_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
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
!                           solely for controlling INFORMATIOn message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Output_Process_ID:  Set this argument to the MPI process ID, specified
!                           via the Process_ID argument, in which all INFORMATION
!                           messages are to be output. If the passed Process_ID
!                           value agrees with this value the INFORMATION messages
!                           are output. If MPI is not being used, ignore this
!                           argument.
!                           This argument is ignored if:
!                             - the optional Process_ID argument is not present.
!                             - the optional Quiet argument is set.
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
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the Binary file read was successful
!                              == FAILURE an unrecoverable read error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If the ODPS argument is defined upon input, it is redefined (or
!       reinitialised) at output.
!
! COMMENTS:
!       Note the INTENT on the output ODPS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_ODPS_Binary( Filename         , &  ! Input
                             ODPS             , &  ! Output
                             Quiet            , &  ! Optional input
                             Process_ID       , &  ! Optional input
                             Output_Process_ID, &  ! Optional input
                             RCS_Id           , &  ! Revision control
                             Message_Log      ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: Filename
    TYPE(ODPS_type)       , INTENT(IN OUT) :: ODPS
    INTEGER     , OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER     , OPTIONAL, INTENT(IN)     :: Process_ID
    INTEGER     , OPTIONAL, INTENT(IN)     :: Output_Process_ID
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_ODPS_Binary'
    ! Function variables
    CHARACTER(ML) :: Message
    CHARACTER(ML) :: Process_ID_Tag
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file is present
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      Message = 'File '//TRIM(Filename)//' not found.'
      Error_Status = FAILURE
      RETURN
    END IF 

    ! Output informational messages....
    Noisy = .TRUE.
    ! ...unless...
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF
    IF ( Noisy .AND. PRESENT(Process_ID) .AND. PRESENT(Output_Process_ID) ) THEN
      IF ( Process_ID /= Output_Process_ID ) Noisy = .FALSE.
    END IF

    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT(Process_ID) ) THEN
      WRITE( Process_ID_Tag,'(";  MPI Process ID: ",i0)' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF


    ! Open the ODPS file
    ! ------------------
    Error_Status = Open_Binary_File( Filename, &
                                     FileID  , &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM(Filename)
      Error_Status = FAILURE
      RETURN
    END IF

    ! Read data and put them in ODAS
    ! --------------------------------------------
    Error_Status =  Read_ODPS_Data( Filename       , & 
                                    FileID         , &   
                                    ODPS           , &   
                                    Process_ID_Tag , &   
                             Message_Log = Message_Log ) 
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading data from '//TRIM(Filename)
      Error_Status = FAILURE
      RETURN
    END IF

    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error closing ",a," after read. IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message)//TRIM(Process_ID_Tag), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_ODPS( ODPS, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF


  END FUNCTION Read_ODPS_Binary

  FUNCTION Read_ODPS_Data(   Filename         , &  ! Input
                             FileID           , &  ! Input         
                             ODPS             , &  ! Output
                             Process_ID_Tag   , &  ! Optional input
                             Message_Log      ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: Filename
    INTEGER               , INTENT(IN)     :: FileID
    TYPE(ODPS_type)       , INTENT(IN OUT) :: ODPS
    CHARACTER(*)          , INTENT(IN)     :: Process_ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_ODPS_Data'
    ! Function variables
    CHARACTER(ML) :: Message
    INTEGER :: IO_Status
    INTEGER(Long) :: Version
    INTEGER(Long) :: Algorithm
    INTEGER(Long) :: n_Layers
    INTEGER(Long) :: n_Components
    INTEGER(Long) :: n_Absorbers
    INTEGER(Long) :: n_Channels
    INTEGER(Long) :: n_Coeffs
    INTEGER(Long) :: n_OPIndex
    INTEGER(Long) :: n_OCoeffs

    ! Read the Release and Version information
    ! ----------------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODPS%Release, Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading Release/Version values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Check the release
    Error_Status = CheckRelease_ODPS( ODPS,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ODPS Release check failed for '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the Alorithm ID
    ! --------------------
    READ( FileID, IOSTAT=IO_Status ) Algorithm
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading Algorithm ID from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Check the algorithm id
    Error_Status = CheckAlgorithm_ODPS( ODPS,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ODPS Algorithm check failed for '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the data dimensions
    ! ------------------------
    READ( FileID, IOSTAT=IO_Status ) n_Layers    , &
                                     n_Components, &
                                     n_Absorbers , &
                                     n_Channels  , &
                                     n_Coeffs    , &
                                     n_OPIndex   , &
                                     n_OCoeffs
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading dimension values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Allocate the output structure
    ! -----------------------------
    Error_Status = Allocate_ODPS( n_Layers    , &
                                  n_Components, &
                                  n_Absorbers , &
                                  n_Channels  , &
                                  n_Coeffs    , &
                                  ODPS        , &
                                  Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ODPS allocation failed'
      CALL Read_Cleanup(); RETURN
    END IF
    
    ! Assign the version number (which may be different)
    ODPS%Version = Version


    ! Read the TC Group ID
    ! -------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODPS%Group_Index
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading Group ID from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Read the sensor info
    ! --------------------
    READ( FileID, IOSTAT=IO_Status ) ODPS%Sensor_Id       , &
                                     ODPS%WMO_Satellite_Id, &
                                     ODPS%WMO_Sensor_Id   , &
                                     ODPS%Sensor_Type
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading sensor information from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the sensor channel numbers
    ! -------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODPS%Sensor_Channel
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading sensor channel data from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Read the transmittance component ID 
    ! ----------------------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODPS%Component_ID
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading tansmittance component ID from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Read the absorber ID 
    ! ----------------------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODPS%Absorber_ID
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading absorber ID from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Read the reference profiles 
    ! ----------------------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODPS%Ref_Level_Pressure, &
                                     ODPS%Ref_Pressure,       &
                                     ODPS%Ref_Temperature,    & 
                                     ODPS%Ref_Absorber,       &
                                     ODPS%Min_Absorber,       &
                                     ODPS%Max_Absorber
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading reference profiles from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Read the n_Predictors and Pos_Index data
    ! ----------------------------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODPS%n_Predictors, &
                                     ODPS%Pos_Index
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading n_Predictors and Pos_Index data from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the regression coefficients
    ! --------------------------------
    IF( ODPS%n_Coeffs > 0 )THEN
      READ( FileID, IOSTAT=IO_Status ) ODPS%C
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message,'("Error reading regression coefficients from ",a,&
                        &". IOSTAT = ",i0)' ) &
                        TRIM(Filename), IO_Status
        CALL Read_Cleanup(); RETURN
      END IF
    END IF

    IF( n_OCoeffs > 0 )THEN

      Error_Status = Allocate_ODPS_OPTRAN( n_OCoeffs    , &
                                           ODPS         , &
                                    Message_Log=Message_Log)
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'ODPS OPTRAN array allocation failed'
        CALL Read_Cleanup(); RETURN
      END IF

      READ( FileID, IOSTAT=IO_Status )  ODPS%OSignificance, &
                                        ODPS%Order,      &
                                        ODPS%OP_Index,   &
                                        ODPS%OPos_Index, &
                                        ODPS%OC, &
                                        ODPS%Alpha, ODPS%Alpha_C1, ODPS%Alpha_C2, &
                                        ODPS%OComponent_Index
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message,'("Error reading ODPS OPTRAN data to ",a,&
                        &". IOSTAT = ",i0)' ) &
                        TRIM(Filename), IO_Status
        CALL Read_Cleanup(); RETURN
      END IF
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      CHARACTER(ML) :: Close_Message
      INTEGER :: Destroy_Status
      ! Close file if necessary
      IF ( File_Exists( Filename ) ) THEN
        IF ( File_Open( Filename ) ) THEN
          CLOSE( FileID, IOSTAT=IO_Status )
          IF ( IO_Status /= 0 ) THEN
            WRITE( Close_Message,'("; Error closing ",a," during error cleanup. IOSTAT=",i0)') &
                                 TRIM(Filename), IO_Status
            Message = TRIM(Message)//TRIM(Close_Message)
          END IF
        END IF
      END IF
      ! Destroy the structure
      Destroy_Status = Destroy_ODPS( ODPS, Message_Log=Message_Log )
      IF ( Destroy_Status /= SUCCESS ) &
        Message = TRIM(Message)//'; Error destroying ODPS structure during error cleanup.'
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message)//TRIM(Process_ID_Tag), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_ODPS_Data

!--------------------------------------------------------------------------------
!
! NAME:
!       Write_ODPS_Binary
!
! PURPOSE:
!       Function to write an ODPS structure to a Binary format file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_ODPS_Binary( Filename                 , &  ! Input
!                                         ODPS                     , &  ! Input
!                                         Quiet       = Quiet      , &  ! Optional input
!                                         RCS_Id      = RCS_Id     , &  ! Revision control
!                                         Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     ODPS format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       ODPS:         Structure containing the gas absorption coefficient
!                     data to write to the file.
!                     UNITS:      N/A
!                     TYPE:       ODPS_type
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
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file write was successful
!                        == FAILURE - the input ODPS structure contains
!                                     unassociated pointer members, or
!                                   - a unrecoverable write error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs, the output file is deleted.
!
!--------------------------------------------------------------------------------

  FUNCTION Write_ODPS_Binary( Filename   , &  ! Input
                              ODPS       , &  ! Input
                              Quiet      , &  ! Optional input
                              RCS_Id     , &  ! Revision control
                              Message_Log) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: Filename
    TYPE(ODPS_type)       , INTENT(IN)  :: ODPS
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_ODPS_Binary'
    ! Function variables
    CHARACTER(ML) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Open the ODPS data file
    ! -----------------------
    Error_Status = Open_Binary_File( Filename, &
                                     FileID,   &
                                     For_Output =SET, &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM( Filename )
      Error_Status = FAILURE
      RETURN
    END IF

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    Error_Status = Write_ODPS_Data( Filename,  &
                                    FileID,    &
                                    ODPS,      &
                                    Message_Log=Message_Log )
                                    
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing data to '//TRIM( Filename )
      Error_Status = FAILURE
      RETURN
    END IF


    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error closing ",a," after write. IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_ODPS( ODPS, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_ODPS_Binary

  FUNCTION Write_ODPS_Data( Filename   , &
                            FileID     , &  
                            ODPS       , &            
                            Message_Log) & 
                            RESULT( Error_Status )
    CHARACTER(*)          , INTENT(IN)  :: Filename
    INTEGER               , INTENT(IN)  :: FileID
    TYPE(ODPS_type)       , INTENT(IN)  :: ODPS
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_ODPS_Data'
    ! Function result
    INTEGER :: Error_Status
    ! Function variables
    CHARACTER(ML) :: Message
    INTEGER       :: IO_Status

    ! Check structure association status
    IF ( .NOT. Associated_ODPS( ODPS ) ) THEN
      Message = 'Some or all INPUT ODPS pointer members are NOT associated.'
      CALL Write_Cleanup(); RETURN
    END IF
    
    ! Check the release
    Error_Status = CheckRelease_ODPS( ODPS, Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ODPS structure Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF

    ! Check the algorithm id
    Error_Status = CheckAlgorithm_ODPS( ODPS, Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ODPS Algorithm check failed'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Check the ODPS structure dimensions
    IF ( ODPS%n_Layers     < 1 .OR. &
         ODPS%n_Components < 1 .OR. &
         ODPS%n_Absorbers  < 1 .OR. &
         ODPS%n_Channels   < 1 .OR. &
         ODPS%n_Coeffs     < 0 .OR. &
         ODPS%n_OPIndex    < 1 .OR. &
         ODPS%n_OCoeffs    < 0      ) THEN
       Message = "One or more ODPS dimension variables have incorrect values"
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the Release and Version information
    ! -----------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODPS%Release, ODPS%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing Release/Version values to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the Alorithm ID
    ! ---------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODPS%Algorithm
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing Algorithm ID to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the data dimensions
    ! -------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODPS%n_Layers    , &
                                      ODPS%n_Components, &
                                      ODPS%n_Absorbers , &
                                      ODPS%n_Channels  , &
                                      ODPS%n_Coeffs    , &
                                      ODPS%n_OPIndex   , &
                                      ODPS%n_OCoeffs
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing dimension values to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write the TC Group ID 
    ! -----------------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODPS%Group_Index
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing Group ID to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write the sensor info
    ! ---------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODPS%Sensor_Id       , &
                                      ODPS%WMO_Satellite_Id, &
                                      ODPS%WMO_Sensor_Id   , &
                                      ODPS%Sensor_Type
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing sensor information to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the sensor channel numbers
    ! --------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODPS%Sensor_Channel
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing sensor channel data to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the Component ID 
    ! -----------------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODPS%Component_ID
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing component ID to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write the absorber ID 
    ! -----------------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODPS%Absorber_ID
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing absorber ID to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write reference profile data
    ! -----------------------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODPS%Ref_Level_Pressure, &
                                      ODPS%Ref_Pressure,       &
                                      ODPS%Ref_Temperature,    &
                                      ODPS%Ref_Absorber,       &
                                      ODPS%Min_Absorber,       &
                                      ODPS%Max_Absorber

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing reference profile data to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write the n_Predictors and Pos_Index data
    ! -----------------------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODPS%n_Predictors, &
                                      ODPS%Pos_Index
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing n_Predictors and Pos_Index data to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the regression coefficients
    ! ---------------------------------
    IF( ODPS%n_Coeffs > 0 )THEN
      WRITE( FileID, IOSTAT=IO_Status ) ODPS%C
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message,'("Error writing regression coefficients to ",a,&
                        &". IOSTAT = ",i0)' ) &
                        TRIM(Filename), IO_Status
        CALL Write_Cleanup(); RETURN
      END IF
    END IF

    IF( ODPS%n_OCoeffs > 0 )THEN
      WRITE( FileID, IOSTAT=IO_Status ) ODPS%OSignificance, &
                                        ODPS%Order,      &
                                        ODPS%OP_Index,   &
                                        ODPS%OPos_Index, &
                                        ODPS%OC, &
                                        ODPS%Alpha, ODPS%Alpha_C1, ODPS%Alpha_C2, &
                                        ODPS%OComponent_Index
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message,'("Error writing ODPS OPTRAN data to ",a,&
                        &". IOSTAT = ",i0)' ) &
                        TRIM(Filename), IO_Status
        CALL Write_Cleanup(); RETURN
      END IF
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp()
      CHARACTER(ML) :: Close_Message
      ! Close file if necessary
      IF ( File_Exists( Filename ) ) THEN
        IF ( File_Open( Filename ) ) THEN
          CLOSE( FileID, IOSTAT=IO_Status, STATUS='DELETE' )
          IF ( IO_Status /= 0 ) THEN
            WRITE( Close_Message,'("; Error deleting ",a," during error cleanup. IOSTAT=",i0)') &
                                 TRIM(Filename), IO_Status
            Message = TRIM(Message)//TRIM(Close_Message)
          END IF
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Write_CleanUp

  END FUNCTION Write_ODPS_Data

END MODULE ODPS_Binary_IO
