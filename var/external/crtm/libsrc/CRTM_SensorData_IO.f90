!
! CRTM_SensorData_IO
!
! Module containing routines to inquire, read, and write CRTM
! SensorData object datafiles.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 11-Nov-2009
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_SensorData_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE File_Utility          , ONLY: File_Open, File_Exists
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility   , ONLY: Open_Binary_File
  USE CRTM_Parameters       , ONLY: SET
  USE CRTM_SensorData_Define, ONLY: CRTM_SensorData_type, &
                                    CRTM_SensorData_Associated, &
                                    CRTM_SensorData_Destroy, &
                                    CRTM_SensorData_Create
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_SensorData_InquireFile
  PUBLIC :: CRTM_SensorData_ReadFile
  PUBLIC :: CRTM_SensorData_WriteFile
  PUBLIC :: CRTM_SensorData_IOVersion

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
    '$Id: CRTM_SensorData_IO.f90 6942 2010-03-10 02:30:02Z paul.vandelst@noaa.gov $'
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Default message length
  INTEGER, PARAMETER :: ML = 256


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
      err_stat = Open_Binary_File( Filename, fid, For_Output=SET )
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


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_SensorData_IOVersion( Id )
!
! OUTPUT ARGUMENTS:
!       Id:    Character string containing the version Id information
!              for the module.
!              UNITS:      N/A
!              TYPE:       CHARACTER(*)
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_SensorData_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_SensorData_IOVersion


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################

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

END MODULE CRTM_SensorData_IO
