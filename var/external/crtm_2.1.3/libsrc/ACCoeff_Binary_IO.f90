!
! ACCoeff_Binary_IO
!
! Module containing routines to read and write Binary format
! ACCoeff data files.
!
!
! CREATION HISTORY:
!       Written by:  Paul van Delst, 08-Jun-2007
!                    paul.vandelst@noaa.gov
!

MODULE ACCoeff_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds         , ONLY: Long, Double
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE ACCoeff_Define     , ONLY: ACCoeff_type          , &
                                 ACCoeff_Associated    , &
                                 ACCoeff_Destroy       , &
                                 ACCoeff_Create        , &
                                 ACCoeff_ValidRelease  , &
                                 ACCoeff_Info
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: ACCoeff_Binary_InquireFile
  PUBLIC :: ACCoeff_Binary_ReadFile
  PUBLIC :: ACCoeff_Binary_WriteFile
  PUBLIC :: ACCoeff_Binary_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
    '$Id: ACCoeff_Binary_IO.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
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
!       ACCoeff_Binary_InquireFile
!
! PURPOSE:
!       Function to inquire a Binary format ACCoeff file.
!
! CALLING SEQUENCE:
!       Error_Status = ACCoeff_Binary_InquireFile( &
!                        Filename                           , &
!                        n_FOVs           = n_FOVs          , &
!                        n_Channels       = n_Channels      , &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Sensor_Id        = Sensor_Id       , &
!                        WMO_Satellite_Id = WMO_Satellite_Id, &
!                        WMO_Sensor_Id    = WMO_Sensor_Id     )
!
! INPUTS:
!       Filename:           Character string specifying the name of the binary
!                           antenna correction data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_FOVs:             Number of sensor fields-of-view (FOVs).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Channels:         Number of sensor channels.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Release:            The data/file release number. Used to check
!                           for data/software mismatch.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The data/file version number. Used for
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
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the file inquire was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION ACCoeff_Binary_InquireFile( &
    Filename        , &  ! Input
    n_FOVs          , &  ! Optional output  
    n_Channels      , &  ! Optional output  
    Release         , &  ! Optional Output
    Version         , &  ! Optional Output
    Sensor_Id       , &  ! Optional Output
    WMO_Satellite_Id, &  ! Optional Output
    WMO_Sensor_Id   ) &  ! Optional Output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_FOVs
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ACCoeff_InquireFile(Binary)'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(ACCoeff_type) :: ACCoeff

 
    ! Setup
    err_stat = SUCCESS
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( Filename ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the release and version
    READ( fid,IOSTAT=io_stat ) ACCoeff%Release, ACCoeff%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat ) &
      ACCoeff%n_FOVs    , &
      ACCoeff%n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimension values from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the sensor ids
    READ( fid, IOSTAT=io_stat ) &
      ACCoeff%Sensor_Id       , &
      ACCoeff%WMO_Satellite_Id, &
      ACCoeff%WMO_Sensor_Id    
    IF ( io_stat /= 0 ) THEN
      WRITE( msg, '("Error reading sensor information from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Assign the return arguments
    IF ( PRESENT(n_FOVs          ) ) n_FOVs           = ACCoeff%n_FOVs
    IF ( PRESENT(n_Channels      ) ) n_Channels       = ACCoeff%n_Channels    
    IF ( PRESENT(Release         ) ) Release          = ACCoeff%Release
    IF ( PRESENT(Version         ) ) Version          = ACCoeff%Version
    IF ( PRESENT(Sensor_Id       ) ) Sensor_Id        = TRIM(ACCoeff%Sensor_Id)
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = ACCoeff%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = ACCoeff%WMO_Sensor_Id   
    
  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      ! Close file if necessary
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup'
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp
    
  END FUNCTION ACCoeff_Binary_InquireFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ACCoeff_Binary_ReadFile
!
! PURPOSE:
!       Function to read ACCoeff object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = ACCoeff_Binary_ReadFile( &
!                        Filename           , &
!                        ACCoeff            , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       ACCoeff format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       ACCoeff:        ACCoeff object containing the antenna correction
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       ACCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the ACCoeff data is embedded within another file
!                       (e.g. SpcCoeff file.)
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

  FUNCTION ACCoeff_Binary_ReadFile( &
    Filename, &  ! Input
    ACCoeff , &  ! Output
    No_Close, &  ! Optional input
    Quiet   , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),       INTENT(IN)  :: Filename
    TYPE(ACCoeff_type), INTENT(OUT) :: ACCoeff
    LOGICAL,  OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL,  OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,  OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ACCoeff_ReadFile(Binary)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(ACCoeff_type) :: dummy
    

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
    READ( fid,IOSTAT=io_stat ) dummy%Release, dummy%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( .NOT. ACCoeff_ValidRelease( dummy ) ) THEN
      msg = 'ACCoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the antenna correction coefficient data
    ! ...Read the dimensions
    READ( fid, IOSTAT=io_stat ) dummy%n_FOVs    , &
                                dummy%n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the object
    CALL ACCoeff_Create( ACCoeff         , &
                         dummy%n_FOVs    , &        
                         dummy%n_Channels  )                  
    IF ( .NOT. ACCoeff_Associated( ACCoeff ) ) THEN
      msg = 'ACCoeff object allocation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the sensor ids
    READ( fid, IOSTAT=io_stat ) ACCoeff%Sensor_Id       , &
                                ACCoeff%WMO_Satellite_Id, &
                                ACCoeff%WMO_Sensor_Id   
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading sensor ids. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the sensor channels
    READ( fid, IOSTAT=io_stat ) ACCoeff%Sensor_Channel
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading sensor channels. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the antenna correction coefficients
    READ( fid, IOSTAT=io_stat ) ACCoeff%A_earth   , &
                                ACCoeff%A_space   , &
                                ACCoeff%A_platform
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading antenna correction coefficients. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF


    ! Explicitly assign the version number
    ACCoeff%Version = dummy%Version
    
    
    ! Close the file
    IF ( close_file ) THEN
      CLOSE( fid,IOSTAT=io_stat )
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Output an info message
     IF ( noisy ) THEN
       CALL ACCoeff_Info( ACCoeff, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS
   
     SUBROUTINE Read_CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid,IOSTAT=io_stat )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing input file during error cleanup.'
       END IF
       CALL ACCoeff_Destroy( ACCoeff )
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Read_CleanUp

  END FUNCTION ACCoeff_Binary_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ACCoeff_Binary_WriteFile
!
! PURPOSE:
!       Function to write ACCoeff object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = ACCoeff_Binary_WriteFile( &
!                        Filename           , &
!                        ACCoeff            , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       ACCoeff format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       ACCoeff:        ACCoeff object containing the antenna correction
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       ACCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the ACCoeff data is to be embedded within another file
!                       (e.g. SpcCoeff file.)
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

  FUNCTION ACCoeff_Binary_WriteFile( &
    Filename, &  ! Input
    ACCoeff , &  ! Input
    No_Close, &  ! Optional input
    Quiet   , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),       INTENT(IN) :: Filename
    TYPE(ACCoeff_type), INTENT(IN) :: ACCoeff
    LOGICAL,  OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL,  OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,  OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ACCoeff_WriteFile(Binary)'
    ! Function variables
    CHARACTER(ML) :: msg
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
    ! ...Check there is data to write
    IF ( .NOT. ACCoeff_Associated( ACCoeff ) ) THEN
      msg = 'ACCoeff object is empty.'
      CALL Write_Cleanup(); RETURN
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
    WRITE( fid,IOSTAT=io_stat ) ACCoeff%Release, ACCoeff%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the antenna correction coefficient data
    ! ...Write the dimensions
    WRITE( fid, IOSTAT=io_stat ) ACCoeff%n_FOVs    , &
                                 ACCoeff%n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the sensor info
    WRITE( fid, IOSTAT=io_stat ) ACCoeff%Sensor_Id       , &
                                 ACCoeff%WMO_Satellite_Id, &
                                 ACCoeff%WMO_Sensor_Id   
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing sensor ids. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the sensor channels
    WRITE( fid, IOSTAT=io_stat ) ACCoeff%Sensor_Channel
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing sensor channels. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the antenna correction coefficients
    WRITE( fid, IOSTAT=io_stat ) ACCoeff%A_earth   , &
                                 ACCoeff%A_space   , &
                                 ACCoeff%A_platform
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing antenna correction coefficients. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Close the file
    IF ( close_file ) THEN
      CLOSE( fid,IOSTAT=io_stat )
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Output an info message
     IF ( noisy ) THEN
       CALL ACCoeff_Info( ACCoeff, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS
   
     SUBROUTINE Write_Cleanup()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid,IOSTAT=io_stat )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing input file during error cleanup.'
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Write_Cleanup

  END FUNCTION ACCoeff_Binary_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ACCoeff_Binary_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL ACCoeff_Binary_IOVersion( Id )
!
! OUTPUT ARGUMENTS:
!       Id:   Character string containing the version Id information
!             for the module.
!             UNITS:      N/A
!             TYPE:       CHARACTER(*)
!             DIMENSION:  Scalar
!             ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE ACCoeff_Binary_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE ACCoeff_Binary_IOVersion
  
END MODULE ACCoeff_Binary_IO
