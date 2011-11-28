!
! AntCorr_Binary_IO
!
! Module containing routines to read and write Binary format
! AntCorr data files.
!
! CREATION HISTORY:
!       Written by:  Paul van Delst, CIMSS/SSEC, 08-Jun-2007
!                    paul.vandelst@ssec.wisc.edu
!

MODULE AntCorr_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds         , ONLY: Long
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE AntCorr_Define     , ONLY: AntCorr_type, &
                                 Associated_AntCorr, &
                                 Destroy_AntCorr, &
                                 Allocate_AntCorr, &
                                 CheckRelease_AntCorr, &
                                 Info_AntCorr
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Structure procedures
  PUBLIC :: Inquire_AntCorr_Binary
  PUBLIC :: Read_AntCorr_Binary
  PUBLIC :: Write_AntCorr_Binary
    

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: AntCorr_Binary_IO.f90 3254 2009-02-19 23:02:07Z paul.vandelst@noaa.gov $'
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
!:sdoc+:
!
! NAME:
!       Inquire_AntCorr_Binary
!
! PURPOSE:
!       Function to inquire a Binary format AntCorr file.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_AntCorr_Binary( Filename                           , &
!                                              n_FOVs           = n_FOVs          , &
!                                              n_Channels       = n_Channels      , &
!                                              Release          = Release         , &
!                                              Version          = Version         , &
!                                              Sensor_Id        = Sensor_Id       , &
!                                              WMO_Satellite_Id = WMO_Satellite_Id, &
!                                              WMO_Sensor_Id    = WMO_Sensor_Id   , &
!                                              RCS_Id           = RCS_Id          , &
!                                              Message_Log      = Message_Log       )
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           AntCorr data file to inquire.
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
!       n_FOVs:             The number of fields-of-view for the sensor.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:         The number of spectral channels for the sensor.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The AntCorr data/file release number. Used to check
!                           for data/software mismatch.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The AntCorr data/file version number. Used for
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
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Inquire_AntCorr_Binary( Filename        , &  ! Input
                                   n_FOVs          , &  ! Optional Output
                                   n_Channels      , &  ! Optional Output
                                   Release         , &  ! Optional Output
                                   Version         , &  ! Optional Output
                                   Sensor_Id       , &  ! Optional Output
                                   WMO_Satellite_Id, &  ! Optional Output
                                   WMO_Sensor_Id   , &  ! Optional Output
                                   RCS_Id          , &  ! Revision Control
                                   Message_Log     ) &  ! Error Messaging
                                 RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_FOVs
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_AntCorr_Binary'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    TYPE(AntCorr_type) :: AntCorr

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      Message = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF
    
    
    ! Open the Binary format AntCorr file
    Error_Status = Open_Binary_File( Filename,FileID,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening AntCorr Binary file '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the Release and Version information
    READ(FileID, IOSTAT=IO_Status) AntCorr%Release, AntCorr%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error reading AntCorr Release/Version values from ", a, &
                     &". IOSTAT = ", i0 )' ) TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the data dimensions
    READ(FileID, IOSTAT=IO_Status) AntCorr%n_FOVs, AntCorr%n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error reading AntCorr dimension values from ", a, &
                     &". IOSTAT = ", i0 )' ) TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the sensor id information
    READ(FileID, IOSTAT=IO_Status) AntCorr%Sensor_Id       , &
                                   AntCorr%WMO_Satellite_Id, &
                                   AntCorr%WMO_Sensor_Id    
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading AntCorr sensor information from ",a,&
                      &". IOSTAT = ",i0)' ) TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(); RETURN
    END IF
    

    ! Assign the return arguments
    IF ( PRESENT(n_FOVs          ) ) n_FOVs     = AntCorr%n_FOVs
    IF ( PRESENT(n_Channels      ) ) n_Channels = AntCorr%n_Channels
    IF ( PRESENT(Release         ) ) Release = AntCorr%Release
    IF ( PRESENT(Version         ) ) Version = AntCorr%Version
    IF ( PRESENT(Sensor_Id       ) ) Sensor_Id        = AntCorr%Sensor_Id
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = AntCorr%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = AntCorr%WMO_Sensor_Id   

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      CHARACTER(ML) :: Close_Message
      ! Close file if necessary
      IF ( File_Open( Filename ) ) THEN
        CLOSE( FileID, IOSTAT=IO_Status )
        IF ( IO_Status /= 0 ) THEN
          WRITE( Close_Message,'("; Error closing input file during error cleanup. IOSTAT=",i0)') &
                               IO_Status
          Message = TRIM(Message)//TRIM(Close_Message)
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION Inquire_AntCorr_Binary


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Read_AntCorr_Binary
!
! PURPOSE:
!       Function to read data into an AntCorr structure from a Binary format file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_AntCorr_Binary( Filename                   , &
!                                           AntCorr                    , &
!                                           No_File_Close=No_File_Close, &
!                                           No_Allocate  =No_Allocate  , &
!                                           Quiet        =Quiet        , &
!                                           RCS_Id       =RCS_Id       , &
!                                           Message_Log  =Message_Log    )
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           format AntCorr data file to read.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       AntCorr:            Structure containing the antenna correction
!                           data read from the file.
!                           UNITS:      N/A
!                           TYPE:       AntCorr_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       No_File_Close:      Set this argument to *not* close the datafile
!                           upon exiting this routine. The default action
!                           is to close the file on exit.
!                           This option is required if the AntCorr data is
!                           embedded within another file (e.g. SpcCoeff file.)
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       No_Allocate:        Set this argument to *not* allocate the AntCorr
!                           structure after the dimensions are read. The default
!                           action is to allocate the structure.
!                           This option is required if the AntCorr data is
!                           embedded within another file (e.g. SpcCoeff file.)
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
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
! COMMENTS:
!       Note the INTENT on the output AntCorr argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_AntCorr_Binary( Filename     , &  ! Input
                                AntCorr      , &  ! Output
                                No_File_Close, &  ! Optional input
                                No_Allocate  , &  ! Optional input
                                Quiet        , &  ! Optional input
                                RCS_Id       , &  ! Revision Control
                                Message_Log  ) &  ! Error Messaging
                              RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: Filename
    TYPE(AntCorr_type)    , INTENT(IN OUT) :: AntCorr
    INTEGER     , OPTIONAL, INTENT(IN)     :: No_File_Close
    INTEGER     , OPTIONAL, INTENT(IN)     :: No_Allocate
    INTEGER     , OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_AntCorr_Binary'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Yes_Close
    LOGICAL :: Yes_Allocate
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER(Long) :: n_Channels, n_FOVs

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! ..Default action is to close the file on exit
    Yes_Close = .TRUE.
    ! ..unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == SET ) Yes_Close = .FALSE.
    END IF
    
    ! ..Default action is to allocate the structure
    Yes_Allocate = .TRUE.
    ! ..unless the No_Allocate optional argument is set.
    IF ( PRESENT( No_Allocate ) ) THEN
      IF ( No_Allocate == SET ) Yes_Allocate = .FALSE.
    END IF
    
    ! ..Default action is to output informational messages
    Noisy = .TRUE.
    ! ..unless the Quiet keyword is set
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF
    
    
    ! Check that the file is open. If not, open it.
    ! Otherwise get its FileID.
    IF ( .NOT. File_Open( FileName ) ) THEN
      ! ..Check that the file exists
      IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
        Message = 'File '//TRIM(Filename)//' not found.'
        CALL Read_CleanUp(); RETURN
      END IF 
      ! ..Open the file
      Error_Status = Open_Binary_File( Filename,FileID,Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM(Filename)
        CALL Read_CleanUp(); RETURN
      END IF
    ELSE
      ! ..Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=FileID )
      ! ..Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        CALL Read_CleanUp(); RETURN
      END IF
    END IF


    ! Read the Release and Version information
    READ( FileID, IOSTAT=IO_Status ) AntCorr%Release, AntCorr%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error reading AntCorr Release/Version values from ", a, &
                     &". IOSTAT = ", i0 )' ) &
                     TRIM(Filename), IO_Status
      CALL Read_CleanUp(); RETURN
    END IF
    Error_Status = CheckRelease_AntCorr( AntCorr,Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'AntCorr Release check failed for '//TRIM(Filename)
      CALL Read_CleanUp(); RETURN
    END IF


    ! Read the data dimensions
    READ(FileID, IOSTAT=IO_Status) n_FOVs, n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error reading AntCorr dimension values from ",a,". IOSTAT = ",i0)' ) &
                     TRIM(Filename), IO_Status
      CALL Read_CleanUp(); RETURN
    END IF


    ! Allocate the output structure if required
    IF ( Yes_Allocate ) THEN
      Error_Status = Allocate_AntCorr( n_FOVs, n_Channels, &
                                       AntCorr, &
                                       Message_Log=Message_Log)
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'AntCorr allocation failed'
        CALL Read_CleanUp(); RETURN
      END IF
    END IF


    ! Read the sensor id information
    READ(FileID, IOSTAT=IO_Status) AntCorr%Sensor_Id       , &
                                   AntCorr%WMO_Satellite_Id, &
                                   AntCorr%WMO_Sensor_Id    
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading AntCorr sensor information from ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_CleanUp(); RETURN
    END IF


    ! Read the antenna correction data
    READ(FileID, IOSTAT=IO_Status) AntCorr%Sensor_Channel, &
                                   AntCorr%A_earth       , &
                                   AntCorr%A_space       , &
                                   AntCorr%A_platform
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading AntCorr data from ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_CleanUp(); RETURN
    END IF


    ! Close the file if required
    IF ( Yes_Close ) THEN
      CLOSE( FileID, IOSTAT=IO_Status )
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), IO_Status
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      CALL Info_AntCorr( AntCorr, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      CHARACTER(ML) :: Close_Message
      ! Close file if necessary
      IF ( File_Open( Filename ) ) THEN
        CLOSE( FileID,IOSTAT=IO_Status )
        IF ( IO_Status /= 0 ) THEN
          WRITE( Close_Message,'("; Error closing ",a," during error cleanup. IOSTAT=",i0)') &
                               TRIM(Filename), IO_Status
          Message = TRIM(Message)//TRIM(Close_Message)
        END IF
      END IF
      ! Destroy the structure
      IF ( Associated_AntCorr( AntCorr ) ) THEN
        Error_Status = Destroy_AntCorr( AntCorr, Message_Log=Message_Log )
        IF ( Error_Status /= SUCCESS ) &
          Message = TRIM(Message)//'; Error destroying AntCorr structure during error cleanup.'
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_AntCorr_Binary


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Write_AntCorr_Binary
!
! PURPOSE:
!       Function to write data from an AntCorr structure into a Binary format file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_AntCorr_Binary( Filename                   , &
!                                           AntCorr                    , &
!                                           No_File_Close=No_File_Close, &
!                                           Quiet        =Quiet        , &
!                                           RCS_Id       =RCS_Id       , &
!                                           Message_Log  =Message_Log    )
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           format AntCorr data file to read.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AntCorr:            Structure containing the antenna correction
!                           data written to file.
!                           UNITS:      N/A
!                           TYPE:       AntCorr_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       No_File_Close:      Set this argument to *not* close the datafile
!                           upon exiting this routine. The default action
!                           is to close the file on exit.
!                           This option is required if the AntCorr data is
!                           embedded within another file (e.g. SpcCoeff file.)
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
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
!                           If == SUCCESS the Binary file write was successful
!                              == FAILURE an unrecoverable write error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Write_AntCorr_Binary( Filename     , &  ! Input            
                                 AntCorr      , &  ! Input           
                                 No_File_Close, &  ! Optional input
                                 Quiet        , &  ! Optional input   
                                 RCS_Id       , &  ! Revision Control 
                                 Message_Log  ) &  ! Error Messaging  
                               RESULT( Error_Status )               
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: Filename
    TYPE(AntCorr_type)    , INTENT(IN)  :: AntCorr
    INTEGER,      OPTIONAL, INTENT(IN)  :: No_File_Close
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_AntCorr_Binary'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Yes_Close
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Default action is to close the file on exit....
    Yes_Close = .TRUE.
    ! ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == 1 ) Yes_Close = .FALSE.
    END IF
    
    ! Output informational messages...
    Noisy = .TRUE.
    ! ...unless the Quiet keyword is set
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF

    ! Check structure association status
    IF ( .NOT. Associated_AntCorr( AntCorr ) ) THEN
      Message = 'Some or all INPUT AntCorr pointer members are NOT associated.'
      CALL Write_Cleanup(); RETURN
    END IF
    
    ! Check the release
    Error_Status = CheckRelease_AntCorr( AntCorr,Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'AntCorr structure Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF

    ! Check the structure dimensions
    IF ( AntCorr%n_FOVs < 1 .OR. AntCorr%n_Channels < 1 ) THEN
      Message = 'Dimensions of AntCorr structure are < or = 0.'
      CALL Write_Cleanup(); RETURN
    END IF



    ! Check that the file is open. If not, open it.
    ! Otherwise get the file ID.
    ! ---------------------------------------------
    IF ( .NOT. File_Open( FileName ) ) THEN
      Error_Status = Open_Binary_File( TRIM(Filename), &
                                       FileID, &
                                       For_Output=SET, &
                                       Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    ELSE
      ! Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=FileID )
      ! Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Write the Release and Version information
    ! -----------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) AntCorr%Release, AntCorr%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error writing AntCorr Release/Version values to ", a, &
                     &". IOSTAT = ", i0 )' ) &
                     TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the data dimensions
    ! -------------------------
    WRITE(FileID, IOSTAT=IO_Status) AntCorr%n_FOVs, AntCorr%n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error writing AntCorr dimension values to ", a, &
                     &". IOSTAT = ", i0 )' ) &
                     TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the sensor id information
    ! -------------------------------
    WRITE(FileID, IOSTAT=IO_Status) AntCorr%Sensor_Id       , &
                                    AntCorr%WMO_Satellite_Id, &
                                    AntCorr%WMO_Sensor_Id    
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error writing AntCorr sensor information to ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the antenna correction data
    ! ---------------------------------
    WRITE(FileID, IOSTAT=IO_Status) AntCorr%Sensor_Channel, &
                                    AntCorr%A_earth       , &
                                    AntCorr%A_space       , &
                                    AntCorr%A_platform
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error writing AntCorr data to ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    
    ! Close the file
    ! --------------
    IF ( Yes_Close ) THEN
      CLOSE( FileID, IOSTAT=IO_Status )
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '("Error closing ",a,". IOSTAT = ",i0)' ) &
                        TRIM(Filename), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              WARNING, &
                              Message_Log=Message_Log )
      END IF
    END IF
    
    
    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_AntCorr( AntCorr, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp()
      CHARACTER(ML) :: Close_Message
      ! Close file if it's open
      IF ( File_Open( Filename ) ) THEN
        CLOSE( FileID,IOSTAT=IO_Status )
        IF ( IO_Status /= 0 ) THEN
          WRITE( Close_Message,'("; Error closing ",a," during error cleanup. IOSTAT=",i0)') &
                               TRIM(Filename), IO_Status
          Message = TRIM(Message)//TRIM(Close_Message)
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Write_CleanUp

  END FUNCTION Write_AntCorr_Binary

END MODULE AntCorr_Binary_IO
