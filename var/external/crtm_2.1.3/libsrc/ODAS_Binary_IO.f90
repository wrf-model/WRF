!
! ODAS_Binary_IO
!
! Module containing routines to read and write Binary format
! ODAS files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!

MODULE ODAS_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds         , ONLY: Long, Double
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE ODAS_Define        , ONLY: ODAS_Type          , &
                                 Associated_ODAS    , &
                                 Allocate_ODAS      , &
                                 Destroy_ODAS       , &
                                 CheckRelease_ODAS  , &
                                 CheckAlgorithm_ODAS, &
                                 Info_ODAS
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Structure procedures
  PUBLIC :: Inquire_ODAS_Binary
  PUBLIC :: Read_ODAS_Binary
  PUBLIC :: Write_ODAS_Binary
  PUBLIC :: Read_ODAS_Data
  PUBLIC :: Write_ODAS_Data

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id: ODAS_Binary_IO.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Message character length
  INTEGER, PARAMETER :: ML = 512
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1


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
!       Inquire_ODAS_Binary
!
! PURPOSE:
!       Function to inquire a Binary format ODAS file.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_ODAS_Binary( Filename                           , &  ! Input
!                                           n_Predictors     = n_Predictors    , &  ! Optional output
!                                           n_Absorbers      = n_Absorbers     , &  ! Optional output
!                                           n_Channels       = n_Channels      , &  ! Optional output
!                                           n_Alphas         = n_Alphas        , &  ! Optional output
!                                           n_Coeffs         = n_Coeffs        , &  ! Optional output
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
!                           ODAS data file to inquire.
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
!       n_Predictors:       The number of predictor functions used in generating
!                           the ODAS data.
!                           NOTE: The data arrays using this dimension value are
!                                 dimensioned as 0:n_Predictors, where the 0'th
!                                 term is the offset. Therefore the actual number
!                                 of array elements along this dimension is
!                                 n_Predictors+1
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Absorbers:        The number of absorbers dimension of the ODAS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:         The number of channels dimension of the ODAS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Alphas:           The number of alpha coefficients used to compute the absorber level.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Coeffs:           The number of the C coeffcients.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The ODAS data/file release number. Used to check
!                           for data/software mismatch.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The ODAS data/file version number. Used for
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

  FUNCTION Inquire_ODAS_Binary( Filename        , &  ! Input
                                n_Predictors    , &  ! Optional output
                                n_Absorbers     , &  ! Optional output
                                n_Channels      , &  ! Optional output
                                n_Alphas        , &  ! Optional output  
                                n_Coeffs        , &  ! Optional output  
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
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Predictors
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Absorbers
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Alphas
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Coeffs
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_ODAS_Binary'
    ! Function variables
    CHARACTER(ML) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    TYPE(ODAS_type) :: ODAS  

 
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
    Error_Status = Open_Binary_File( Filename, FileID )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening ODAS Binary file '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the Release and Version information
    ! ----------------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODAS%Release, ODAS%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading Release/Version values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF


    ! Read the Alorithm ID
    ! --------------------
    READ( FileID, IOSTAT=IO_Status ) ODAS%Algorithm
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading Algorithm ID from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF


    ! Read the data dimensions
    ! ------------------------
    READ( FileID, IOSTAT=IO_Status ) ODAS%n_Predictors, &
                                     ODAS%n_Absorbers , &
                                     ODAS%n_Channels  , &
                                     ODAS%n_Alphas    , &
                                     ODAS%n_Coeffs
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading dimension values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF


    ! Read the sensor ids
    ! -------------------
    READ( FileID, IOSTAT=IO_Status ) ODAS%Sensor_Id       , &
                                     ODAS%WMO_Satellite_Id, &
                                     ODAS%WMO_Sensor_Id    
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
    IF ( PRESENT(n_Predictors) ) n_Predictors = ODAS%n_Predictors
    IF ( PRESENT(n_Absorbers ) ) n_Absorbers  = ODAS%n_Absorbers 
    IF ( PRESENT(n_Channels  ) ) n_Channels   = ODAS%n_Channels
    IF ( PRESENT(n_Alphas    ) ) n_Alphas     = ODAS%n_Alphas    
    IF ( PRESENT(n_Coeffs    ) ) n_Coeffs     = ODAS%n_Coeffs    

    ! Release/Version information
    IF ( PRESENT(Release) ) Release = ODAS%Release
    IF ( PRESENT(Version) ) Version = ODAS%Version

    ! Sensor ids
    IF ( PRESENT(Sensor_Id       ) ) Sensor_Id        = ODAS%Sensor_Id(1:MIN(LEN(Sensor_Id),LEN_TRIM(ODAS%Sensor_Id)))
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = ODAS%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = ODAS%WMO_Sensor_Id   
    
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

  END FUNCTION Inquire_ODAS_Binary


!--------------------------------------------------------------------------------
!
! NAME:
!       Read_ODAS_Binary
!
! PURPOSE:
!       Function to read data into an ODAS structure from a Binary format file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_ODAS_Binary( Filename                             , &  ! Input
!                                        ODAS                                 , &  ! Output
!                                        Quiet             = Quiet            , &  ! Optional input
!                                        Process_ID        = Process_ID       , &  ! Optional input
!                                        Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                        RCS_Id            = RCS_Id           , &  ! Revision control
!                                        Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           format ODAS data file to read.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ODAS:               Structure containing the gas absorption coefficient
!                           data read from the file.
!                           UNITS:      N/A
!                           TYPE:       ODAS_type
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
!       If the ODAS argument is defined upon input, it is redefined (or
!       reinitialised) at output.
!
! COMMENTS:
!       Note the INTENT on the output ODAS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_ODAS_Binary( Filename         , &  ! Input
                             ODAS             , &  ! Output
                             Quiet            , &  ! Optional input
                             Process_ID       , &  ! Optional input
                             Output_Process_ID, &  ! Optional input
                             RCS_Id           , &  ! Revision control
                             Message_Log      ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: Filename
    TYPE(ODAS_type)       , INTENT(IN OUT) :: ODAS
    INTEGER     , OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER     , OPTIONAL, INTENT(IN)     :: Process_ID
    INTEGER     , OPTIONAL, INTENT(IN)     :: Output_Process_ID
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_ODAS_Binary'
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
    ! ....unless....
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

    ! Open the ODAS file
    ! ------------------
    Error_Status = Open_Binary_File( Filename, FileID )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM(Filename)
      Error_Status = FAILURE
      RETURN
    END IF

    ! Read data and put them in ODAS
    ! --------------------------------------------
    Error_Status =  Read_ODAS_Data( Filename       , & 
                                    FileID         , &   
                                    ODAS           , &   
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
      CALL Info_ODAS( ODAS, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF


  END FUNCTION Read_ODAS_Binary

  FUNCTION Read_ODAS_Data(   Filename         , & 
                             FileID           , &
                             ODAS             , &
                             Process_ID_Tag   , &
                             Message_Log )      &
                           RESULT( Error_Status )

    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: Filename
    INTEGER               , INTENT(IN)     :: FileID
    TYPE(ODAS_type)       , INTENT(IN OUT) :: ODAS
    CHARACTER(*)          , INTENT(IN)     :: Process_ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_ODAS_Data'
    ! Function variables
    CHARACTER(ML) :: Message
    INTEGER       :: IO_Status
    INTEGER(Long) :: Version
    INTEGER(Long) :: Algorithm
    INTEGER(Long) :: n_Predictors
    INTEGER(Long) :: n_Absorbers
    INTEGER(Long) :: n_Channels
    INTEGER(Long) :: n_Alphas
    INTEGER(Long) :: n_Coeffs

    ! Read the Release and Version information
    ! ----------------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODAS%Release, Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading Release/Version values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Check the release
    Error_Status = CheckRelease_ODAS( ODAS,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ODAS Release check failed for '//TRIM(Filename)
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
    Error_Status = CheckAlgorithm_ODAS( ODAS,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ODAS Algorithm check failed for '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the data dimensions
    ! ------------------------
    READ( FileID, IOSTAT=IO_Status ) n_Predictors, &
                                     n_Absorbers , &
                                     n_Channels  , &
                                     n_Alphas    , &
                                     n_Coeffs
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading dimension values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output structure
    ! -----------------------------
    Error_Status = Allocate_ODAS( n_Predictors, &
                                  n_Absorbers , &
                                  n_Channels  , &
                                  n_Alphas    , &
                                  n_Coeffs    , &
                                  ODAS        , &
                                  Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ODAS allocation failed'
      CALL Read_Cleanup(); RETURN
    END IF
    
    ! Assign the version number (which may be different)
    ODAS%Version = Version


    ! Read the sensor info
    ! --------------------
    READ( FileID, IOSTAT=IO_Status ) ODAS%Sensor_Id       , &
                                     ODAS%WMO_Satellite_Id, &
                                     ODAS%WMO_Sensor_Id   , &
                                     ODAS%Sensor_Type
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading sensor information from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the sensor channel numbers
    ! -------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODAS%Sensor_Channel
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading sensor channel data from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the max order, absorber ID and absorber space values
    ! ----------------------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODAS%Absorber_ID, &
                                     ODAS%Max_Order,   &
                                     ODAS%Alpha     
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading absorber information from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the polynomial order, predictor index and position index arrays
    ! --------------------------------------------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODAS%Order,     &
                                     ODAS%Pre_Index, &
                                     ODAS%Pos_Index
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading order and index arrays from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the regression coefficients
    ! --------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODAS%C
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading regression coefficients from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
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
      Destroy_Status = Destroy_ODAS( ODAS, Message_Log=Message_Log )
      IF ( Destroy_Status /= SUCCESS ) &
        Message = TRIM(Message)//'; Error destroying ODAS structure during error cleanup.'
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message)//TRIM(Process_ID_Tag), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_ODAS_Data


!--------------------------------------------------------------------------------
!
! NAME:
!       Write_ODAS_Binary
!
! PURPOSE:
!       Function to write an ODAS structure to a Binary format file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_ODAS_Binary( Filename                 , &  ! Input
!                                         ODAS                     , &  ! Input
!                                         Quiet       = Quiet      , &  ! Optional input
!                                         RCS_Id      = RCS_Id     , &  ! Revision control
!                                         Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     ODAS format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       ODAS:         Structure containing the gas absorption coefficient
!                     data to write to the file.
!                     UNITS:      N/A
!                     TYPE:       ODAS_type
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
!                        == FAILURE - the input ODAS structure contains
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

  FUNCTION Write_ODAS_Binary( Filename   , &  ! Input
                              ODAS       , &  ! Input
                              Quiet      , &  ! Optional input
                              RCS_Id     , &  ! Revision control
                              Message_Log) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: Filename
    TYPE(ODAS_type)       , INTENT(IN)  :: ODAS
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_ODAS_Binary'
    ! Function variables
    CHARACTER(ML) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Open the ODAS data file
    ! -----------------------
    Error_Status = Open_Binary_File( Filename, FileID, For_Output = .TRUE. )
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

    Error_Status = Write_ODAS_Data( Filename,  &
                                    FileID,    &
                                    ODAS,      &
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
      CALL Info_ODAS( ODAS, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_ODAS_Binary

  FUNCTION Write_ODAS_Data( Filename   , &
                            FileID     , &  
                            ODAS       , &            
                            Message_Log) & 
                            RESULT( Error_Status )
                            
    CHARACTER(*)          , INTENT(IN)  :: Filename
    INTEGER               , INTENT(IN)  :: FileID
    TYPE(ODAS_type)       , INTENT(IN)  :: ODAS
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_ODAS_Data'
    ! Function result
    INTEGER :: Error_Status
    ! Function variables
    CHARACTER(ML) :: Message
    INTEGER       :: IO_Status
    
    ! Check structure association status
    IF ( .NOT. Associated_ODAS( ODAS ) ) THEN
      Message = 'Some or all INPUT ODAS pointer members are NOT associated.'
      CALL Write_Cleanup(); RETURN
    END IF
    
    ! Check the release
    Error_Status = CheckRelease_ODAS( ODAS, Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ODAS structure Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF

    ! Check the algorithm id
    Error_Status = CheckAlgorithm_ODAS( ODAS, Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ODAS Algorithm check failed'
      CALL Write_Cleanup(); RETURN
    END IF

    ! Check the ODAS structure dimensions
    IF ( ODAS%n_Predictors < 1 .OR. &
         ODAS%n_Absorbers  < 1 .OR. &
         ODAS%n_Channels   < 1 .OR. &
         ODAS%n_Alphas     < 1 .OR. &
         ODAS%n_Coeffs     < 1  ) THEN
      Message = 'One or more dimensions of ODAS structure are < or = 0.'
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write the Release and Version information
    ! -----------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODAS%Release, ODAS%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing Release/Version values to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the Alorithm ID
    ! ---------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODAS%Algorithm
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing Algorithm ID to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the data dimensions
    ! -------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODAS%n_Predictors, &
                                      ODAS%n_Absorbers , &
                                      ODAS%n_Channels  , &
                                      ODAS%n_Alphas    , &
                                      ODAS%n_Coeffs
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing dimension values to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the sensor info
    ! ---------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODAS%Sensor_Id       , &
                                      ODAS%WMO_Satellite_Id, &
                                      ODAS%WMO_Sensor_Id   , &
                                      ODAS%Sensor_Type
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing sensor information to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the sensor channel numbers
    ! --------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODAS%Sensor_Channel
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing sensor channel data to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the absorber ID and absorber space values
    ! -----------------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODAS%Absorber_ID, &
                                      ODAS%Max_Order  , &
                                      ODAS%Alpha
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing absorber information to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the polynomial order, predictor index and position index arrays
    ! ----------------------------------------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODAS%Order    , &
                                      ODAS%Pre_Index, &
                                      ODAS%Pos_Index
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing order and index arrays to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the regression coefficients
    ! ---------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODAS%C
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing regression coefficients to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
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


  END FUNCTION Write_ODAS_Data

END MODULE ODAS_Binary_IO
