!
! ODSSU_Binary_IO
!
! Module containing routines to read and write Binary format
! ODSSU files.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, NOAA/NESDIS, Oct. 6, 2009
!
!                       Yong Chen, NOAA/NESDIS, 06-Nov-2009
!                       yong.chen@noaa.gov
!                       

MODULE ODSSU_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds         , ONLY: Long, Double
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE ODSSU_Define       , ONLY: ODSSU_Type          , &
                                 Allocate_ODSSU      , &
                                 Destroy_ODSSU       , &
                                 CheckRelease_ODSSU  , &
                                 CheckAlgorithm_ODSSU, &
                                 Info_ODSSU, ODAS_ALGORITHM, ODPS_ALGORITHM
                                 
  USE ODAS_Binary_IO     , ONLY: Read_ODAS_Data, &
                                 Write_ODAS_Data

  USE ODPS_Binary_IO     , ONLY: Read_ODPS_Data, &
                                 Write_ODPS_Data
  
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Structure procedures
  PUBLIC :: Read_ODSSU_Binary
  PUBLIC :: Write_ODSSU_Binary

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id: ODSSU_Binary_IO.f90 5689 2009-11-12 20:49:39Z paul.vandelst@noaa.gov $'
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Message character length
  INTEGER, PARAMETER :: ML = 512

CONTAINS

!--------------------------------------------------------------------------------
!
! NAME:
!       Read_ODSSU_Binary
!
! PURPOSE:
!       Function to read data into an ODSSU structure from a Binary format file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_ODSSU_Binary(Filename                             , &  ! Input
!                                        ODSSU                                , &  ! Output
!                                        Quiet             = Quiet            , &  ! Optional input
!                                        Process_ID        = Process_ID       , &  ! Optional input
!                                        Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                        RCS_Id            = RCS_Id           , &  ! Revision control
!                                        Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           format ODSSU data file to read.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ODSSU:              Structure containing the gas absorption coefficient
!                           data read from the file.
!                           UNITS:      N/A
!                           TYPE:       ODSSU_type
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
!       If the ODSSU argument is defined upon input, it is redefined (or
!       reinitialised) at output.
!
! COMMENTS:
!       Note the INTENT on the output ODSSU argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_ODSSU_Binary(Filename         , &  ! Input
                             ODSSU            , &  ! Output
                             Quiet            , &  ! Optional input
                             Process_ID       , &  ! Optional input
                             Output_Process_ID, &  ! Optional input
                             RCS_Id           , &  ! Revision control
                             Message_Log      ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: Filename
    TYPE(ODSSU_type)      , INTENT(IN OUT) :: ODSSU
    INTEGER     , OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER     , OPTIONAL, INTENT(IN)     :: Process_ID
    INTEGER     , OPTIONAL, INTENT(IN)     :: Output_Process_ID
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_ODSSU_Binary'
    ! Function variables
    CHARACTER(ML) :: Message
    CHARACTER(ML) :: Process_ID_Tag
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER(Long) :: Version
    INTEGER(Long) :: Algorithm
    INTEGER(Long) :: n_Absorbers
    INTEGER(Long) :: n_Channels
    INTEGER(Long) :: n_TC_CellPressures
    INTEGER(Long) :: n_Ref_CellPressures
    INTEGER       :: i
 
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
    IF ( PRESENT(Quiet) ) THEN
      ! ....unless the QUIET keyword is set
      IF ( Quiet == SET ) Noisy = .FALSE.
    ELSE
      ! ....or the Process_ID is not selected for output
      IF ( PRESENT(Process_ID) .AND. PRESENT(Output_Process_ID) ) THEN
        IF ( Process_ID /= Output_Process_ID ) Noisy = .FALSE.
      END IF
    END IF

    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT(Process_ID) ) THEN
      WRITE( Process_ID_Tag,'(";  MPI Process ID: ",i0)' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF

    ! Open the ODSSU file
    ! ------------------
    Error_Status = Open_Binary_File( Filename, &
                                     FileID  , &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM(Filename)
      Error_Status = FAILURE
      RETURN
    END IF

    ! Read the Release and Version information
    ! ----------------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODSSU%Release, Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading Release/Version values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Check the release
    Error_Status = CheckRelease_ODSSU( ODSSU,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ODSSU Release check failed for '//TRIM(Filename)
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
    Error_Status = CheckAlgorithm_ODSSU( ODSSU,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ODSSU Algorithm check failed for '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF

    ! Read the subAlorithm ID
    ! --------------------
    READ( FileID, IOSTAT=IO_Status ) ODSSU%subAlgorithm
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading subAlgorithm ID from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF
    
    !--------------------------------------------
    ! Allocate memory and read data
    !--------------------------------------------
    
    ! Read the call pressure array dimensions
    READ( FileID, IOSTAT=IO_Status ) n_Channels         , &
                                     n_Absorbers        , &
                                     n_TC_CellPressures , &
                                     n_Ref_CellPressures
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading dimension values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    IF( n_Channels < 1 .OR. n_TC_CellPressures < 1 .OR. n_Ref_CellPressures < 1 )THEN
      Message = 'One or more dimensions of the cell pressure arrays are < or = 0.'
      CALL Read_Cleanup(); RETURN
    END IF

    Error_Status = Allocate_ODSSU(n_Absorbers        , &
                                  n_Channels         , &
                                  n_TC_CellPressures , &
                                  n_Ref_CellPressures, &
                                  ODSSU              , &
                                  Message_Log = Message_Log) 
    IF ( Error_Status /= SUCCESS ) THEN                     
      Message = 'Error allocating memory for the ODSSU structure '  
      CALL Read_Cleanup(); RETURN
      RETURN                                                
    END IF                                                  
 
    ! Read the cell pressuresa and time data 
    ! ----------------------------------------------
    READ( FileID, IOSTAT=IO_Status ) ODSSU%TC_CellPressure, &
                                     ODSSU%Ref_Time,        &
                                     ODSSU%Ref_CellPressure
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading cell pressure and time data from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Read coefficient data and put them into the ODx structure 
    IF(ODSSU%subAlgorithm == ODAS_ALGORITHM) THEN   
                                      
     DO i = 1, n_TC_CellPressures
    
       Error_Status = Read_ODAS_Data( Filename         , &
                                      FileID           , &
                                      ODSSU%ODAS(i)    , &
                                      Process_ID_Tag   , &
                                      Message_Log = Message_Log) 
       IF ( Error_Status /= SUCCESS ) THEN
         Message = 'Error reading data from '//TRIM(Filename)
         CALL Read_Cleanup(); RETURN
         RETURN
       END IF

     END DO
     
     ! assign values taken from an ODx to ODSSU 
     ODSSU%Sensor_Channel   = ODSSU%ODAS(1)%Sensor_Channel
     ODSSU%Absorber_ID      = ODSSU%ODAS(1)%Absorber_ID
     ODSSU%Sensor_Id        = ODSSU%ODAS(1)%Sensor_Id
     ODSSU%WMO_Satellite_ID = ODSSU%ODAS(1)%WMO_Satellite_ID
     ODSSU%WMO_Sensor_ID    = ODSSU%ODAS(1)%WMO_Sensor_ID
     ODSSU%Sensor_Type      = ODSSU%ODAS(1)%Sensor_Type
    
    ENDIF

    IF(ODSSU%subAlgorithm == ODPS_ALGORITHM) THEN  
                                       
     DO i = 1, n_TC_CellPressures
    
       Error_Status = Read_ODPS_Data( Filename         , &
                                      FileID           , &
                                      ODSSU%ODPS(i)    , &
                                      Process_ID_Tag   , &
                                      Message_Log = Message_Log) 
       IF ( Error_Status /= SUCCESS ) THEN
         Message = 'Error reading data from '//TRIM(Filename)
         CALL Read_Cleanup(); RETURN
         RETURN
       END IF

     END DO
     ! assign values taken from an ODx to ODSSU 
     ODSSU%Sensor_Channel   = ODSSU%ODPS(1)%Sensor_Channel   
     ODSSU%Absorber_ID      = ODSSU%ODPS(1)%Absorber_ID      
     ODSSU%Sensor_Id        = ODSSU%ODPS(1)%Sensor_Id        
     ODSSU%WMO_Satellite_ID = ODSSU%ODPS(1)%WMO_Satellite_ID 
     ODSSU%WMO_Sensor_ID    = ODSSU%ODPS(1)%WMO_Sensor_ID    
     ODSSU%Sensor_Type      = ODSSU%ODPS(1)%Sensor_Type      
                                                                
    ENDIF

                                       
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
      CALL Info_ODSSU( ODSSU, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
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
      Destroy_Status = Destroy_ODSSU( ODSSU, Message_Log=Message_Log )
      IF ( Destroy_Status /= SUCCESS ) &
        Message = TRIM(Message)//'; Error destroying ODSSU structure during error cleanup.'
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message)//TRIM(Process_ID_Tag), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_ODSSU_Binary

!--------------------------------------------------------------------------------
!
! NAME:
!       Write_ODSSU_Binary
!
! PURPOSE:
!       Function to write an ODSSU structure to a Binary format file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_ODSSU_Binary(Filename                 , &  ! Input
!                                         ODSSU                    , &  ! Input
!                                         Quiet       = Quiet      , &  ! Optional input
!                                         RCS_Id      = RCS_Id     , &  ! Revision control
!                                         Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     ODSSU format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       ODSSU:        Structure containing the gas absorption coefficient
!                     data to write to the file.
!                     UNITS:      N/A
!                     TYPE:       ODSSU_type
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
!                        == FAILURE - the input ODSSU structure contains
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
                                                          
  FUNCTION Write_ODSSU_Binary( Filename   , &  
                               ODSSU      , &  
                               Quiet      , &  
                               RCS_Id     , &  
                               Message_Log) &  
                            RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: Filename
    TYPE(ODSSU_type)      , INTENT(IN)  :: ODSSU
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_ODSSU_Binary'
    ! Function variables
    CHARACTER(ML) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: i

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    !
    IF( ODSSU%n_TC_CellPressures  < 1 .OR. &
        ODSSU%n_Ref_CellPressures < 1 .OR. &
        ODSSU%n_Channels          < 1 .OR. &
        ODSSU%n_Absorbers         < 1) THEN
      Message = 'One or more dimensions in ODSSU are < or = 0.'
      CALL Write_Cleanup(); RETURN
    END IF

    ! Open the ODSSU data file
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

    ! Write the Release and Version information
    ! -----------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODSSU%Release, ODSSU%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing Release/Version values to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the Alorithm ID
    ! ---------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODSSU%Algorithm
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing Algorithm ID to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write the subAlorithm ID
    ! ---------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODSSU%subAlgorithm
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing Algorithm ID to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write the array dimensions
    WRITE( FileID, IOSTAT=IO_Status ) ODSSU%n_Channels         , &
                                      ODSSU%n_Absorbers        , &
                                      ODSSU%n_TC_CellPressures , &
                                      ODSSU%n_Ref_CellPressures
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing dimension values for ODSSU to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write the cell pressuresa and time data 
    ! ----------------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ODSSU%TC_CellPressure, &
                                      ODSSU%Ref_Time,        &
                                      ODSSU%Ref_CellPressure
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing cell pressure and time data to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF
      
    ! Write coefficient data    
    IF(ODSSU%subAlgorithm == ODAS_ALGORITHM) THEN  
                                       
     DO i = 1, ODSSU%n_TC_CellPressures
       Error_Status = Write_ODAS_Data( Filename      , &
                                       FileID        , &  
                                       ODSSU%ODAS(i) , &           
                                       Message_Log = Message_Log)
       IF ( Error_Status /= SUCCESS ) THEN
         Message = 'Error writing data to '//TRIM(Filename)
         Error_Status = FAILURE
         RETURN
       END IF
       
     END DO
     
    ENDIF                                                                             

    IF(ODSSU%subAlgorithm == ODPS_ALGORITHM) THEN  
                                       
     DO i = 1, ODSSU%n_TC_CellPressures
       Error_Status = Write_ODPS_Data( Filename      , &
                                       FileID        , &  
                                       ODSSU%ODPS(i) , &           
                                       Message_Log = Message_Log)
       IF ( Error_Status /= SUCCESS ) THEN
         Message = 'Error writing data to '//TRIM(Filename)
         Error_Status = FAILURE
         RETURN
       END IF
       
     END DO
     
    ENDIF                                                                             

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
      CALL Info_ODSSU( ODSSU, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
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

  END FUNCTION Write_ODSSU_Binary
  
END MODULE ODSSU_Binary_IO
