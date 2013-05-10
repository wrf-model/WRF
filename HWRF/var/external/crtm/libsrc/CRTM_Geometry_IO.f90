!
! CRTM_Geometry_IO
!
! Module containing routines to inquire, read, and write CRTM
! Geometry object datafiles.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 13-Apr-2009
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_Geometry_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE File_Utility        , ONLY: File_Open, File_Exists
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility , ONLY: Open_Binary_File
  USE CRTM_Parameters     , ONLY: SET
  USE CRTM_Geometry_Define, ONLY: CRTM_Geometry_type, &
                                  CRTM_Geometry_Destroy
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_Geometry_InquireFile
  PUBLIC :: CRTM_Geometry_ReadFile
  PUBLIC :: CRTM_Geometry_WriteFile
  PUBLIC :: CRTM_Geometry_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
    '$Id: CRTM_Geometry_IO.f90 6125 2009-12-18 20:19:59Z paul.vandelst@noaa.gov $'
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
!       CRTM_Geometry_InquireFile
!
! PURPOSE:
!       Function to inquire CRTM Geometry object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Geometry_InquireFile( Filename               , &
!                                                 n_Profiles = n_Profiles  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       CRTM Geometry data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Profiles:     The number of profiles for which their is geometry 
!                       information in the data file.
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

  FUNCTION CRTM_Geometry_InquireFile( &
    Filename  , &  ! Input
    n_Profiles) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Geometry_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m
 
    ! Set up
    err_stat = SUCCESS
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Read the number of profiles
    READ( fid,IOSTAT=io_stat ) m
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimensions from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Close the file
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Set the return arguments
    IF ( PRESENT(n_Profiles) ) n_Profiles = m

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      ! Close file if necessary
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,IOSTAT=io_stat )
        IF ( io_stat /= SUCCESS ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup'
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION CRTM_Geometry_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Geometry_ReadFile
!
! PURPOSE:
!       Function to read CRTM Geometry object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Geometry_ReadFile( Filename               , &
!                                              Geometry               , &
!                                              Quiet      = Quiet     , &
!                                              n_Profiles = n_Profiles  )
!
! INPUTS:
!       Filename:     Character string specifying the name of an
!                     a Geometry data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Geometry:     CRTM Geometry object array containing the
!                     data read from file.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Geometry_type
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Quiet:        Set this logical argument to suppress INFORMATION
!                     messages being printed to stdout
!                     If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                        == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                     If not specified, default is .FALSE.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_Profiles:   The number of profiles for which data was read.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the file read was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Geometry_ReadFile( &
    Filename  , &  ! Input
    Geometry  , &  ! Output
    Quiet     , &  ! Optional input
    n_Profiles, &  ! Optional output
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),             INTENT(IN)  :: Filename
    TYPE(CRTM_Geometry_type), INTENT(OUT) :: Geometry(:)
    LOGICAL,        OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER,        OPTIONAL, INTENT(OUT) :: n_Profiles
    LOGICAL,        OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Geometry_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: n_File_Profiles
    INTEGER :: m, n_Input_Profiles
 

    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) Noisy = .TRUE.
    END IF
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions     
    READ( fid,IOSTAT=io_stat ) n_File_Profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimensions from ",a,". IOSTAT = ",i0)' ) &
                 TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if n_Profiles in file is > size of output array
    n_Input_Profiles = SIZE(Geometry)
    IF ( n_File_Profiles > n_Input_Profiles ) THEN
      WRITE( msg,'("Number of profiles, ",i0," > size of the output Geometry ", &
                  &" array, ",i0,". Only the first ",i0, &
                  &" profiles will be read.")' ) &
                  n_File_Profiles, n_Input_Profiles, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING )
    END IF
    n_Input_Profiles = MIN(n_Input_Profiles, n_File_Profiles)


    ! Loop over all the profiles
    Profile_Loop: DO m = 1, n_Input_Profiles
      err_stat = Read_Record( fid, Geometry(m) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading Geometry element (",i0,") from ",a)' ) &
               m, TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF
    END DO Profile_Loop


    ! Close the file
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_Input_Profiles


    ! Output an info message
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of profiles read from ",a,": ",i0)' ) TRIM(Filename), n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      CALL CRTM_Geometry_Destroy( Geometry )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION CRTM_Geometry_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Geometry_WriteFile
!
! PURPOSE:
!       Function to write CRTM Geometry object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Geometry_WriteFile( Filename     , &
!                                               Geometry     , &
!                                               Quiet = Quiet  )
!
! INPUTS:
!       Filename:     Character string specifying the name of the
!                     Geometry format data file to write.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Geometry:     CRTM Geometry object array containing the Geometry
!                     data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Geometry_type
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:        Set this logical argument to suppress INFORMATION
!                     messages being printed to stdout
!                     If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                        == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                     If not specified, default is .FALSE.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the file write was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs during *writing*, the output file is deleted before
!         returning to the calling routine.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Geometry_WriteFile( &
    Filename, &  ! Input
    Geometry, &  ! Input
    Quiet   , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),             INTENT(IN) :: Filename
    TYPE(CRTM_Geometry_type), INTENT(IN) :: Geometry(:)
    LOGICAL,        OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,        OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Geometry_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m, n_Output_Profiles
 
    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) Noisy = .TRUE.
    END IF
    n_Output_Profiles = SIZE(Geometry)


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, For_Output=SET )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid,IOSTAT=io_stat ) n_Output_Profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing data dimensions to ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF

    
    ! Write the data
    Profile_Loop: DO m = 1, n_Output_Profiles
      err_stat = Write_Record( fid, Geometry(m) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing Geometry element (",i0,") to ",a)' ) m, TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END DO Profile_Loop


    ! Close the file (if error, no delete)
    CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of profiles written to ",a,": ",i0)' ) &
             TRIM(Filename), n_Output_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deleting output file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION CRTM_Geometry_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Geometry_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Geometry_IOVersion( Id )
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

  SUBROUTINE CRTM_Geometry_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_Geometry_IOVersion


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
!       Read_Record
!
! PURPOSE:
!       Utility function to read a single Geometry data record
!
! CALLING SEQUENCE:
!       Error_Status = Read_Record( FileID, Geometry )
!
! INPUTS:
!       FileID:       Logical unit number from which to read data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Geometry:     CRTM Geometry object containing the data read in.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Geometry_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the read was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION Read_Record( fid, geo ) RESULT( err_stat )
    ! Arguments
    INTEGER,                  INTENT(IN)  :: fid
    TYPE(CRTM_Geometry_type), INTENT(OUT) :: geo
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Geometry_ReadFile(Record)'
    ! Function variables
    CHARACTER(ML)  :: msg
    INTEGER :: io_stat

    ! Set up
    err_stat = SUCCESS

    ! Read the data record
    READ( fid,IOSTAT=io_stat ) &
      geo%iFOV                , &
      geo%Longitude           , &
      geo%Latitude            , &
      geo%Surface_Altitude    , &
      geo%Sensor_Scan_Angle   , &
      geo%Sensor_Zenith_Angle , &
      geo%Sensor_Azimuth_Angle, &
      geo%Source_Zenith_Angle , &
      geo%Source_Azimuth_Angle, &
      geo%Flux_Zenith_Angle   , &
      geo%Year                , &
      geo%Month               , &
      geo%Day  
           
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Geometry data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Read_Record_Cleanup()
      CALL CRTM_Geometry_Destroy( geo )
      CLOSE( fid,IOSTAT=io_stat )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup'
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
!       Function to write a single Geometry data record
!
! CALLING SEQUENCE:
!       Error_Status = Write_Record( FileID, Geometry )
!
! INPUTS:
!       FileID:       Logical unit number to which data is written
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Geometry:     CRTM Geometry object containing the data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Geometry_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the record write was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION Write_Record( fid, geo ) RESULT( err_stat )
    ! Arguments
    INTEGER,                  INTENT(IN)  :: fid
    TYPE(CRTM_Geometry_type), INTENT(IN)  :: geo
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Geometry_WriteFile(Record)'
    ! Function variables
    CHARACTER(ML)  :: msg
    INTEGER :: io_stat
 
    ! Set up
    err_stat = SUCCESS


    ! Write the data record
    WRITE( fid,IOSTAT=io_stat ) &
      geo%iFOV                , &
      geo%Longitude           , &
      geo%Latitude            , &
      geo%Surface_Altitude    , &
      geo%Sensor_Scan_Angle   , &
      geo%Sensor_Zenith_Angle , &
      geo%Sensor_Azimuth_Angle, &
      geo%Source_Zenith_Angle , &
      geo%Source_Azimuth_Angle, &
      geo%Flux_Zenith_Angle   , &
      geo%Year                , &
      geo%Month               , &
      geo%Day     
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing Geometry data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Write_Record_Cleanup()
      CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup'
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Write_Record_Cleanup
    
  END FUNCTION Write_Record

END MODULE CRTM_Geometry_IO
