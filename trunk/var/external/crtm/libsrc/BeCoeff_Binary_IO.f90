!
! BeCoeff_Binary_IO
!
! Module containing routines to inquire, read, and write Binary
! BeCoeff object datafiles.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 10-Feb-2010
!                       paul.vandelst@noaa.gov
!

MODULE BeCoeff_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE BeCoeff_Define     , ONLY: BeCoeff_type        , &
                                 BeCoeff_Associated  , &
                                 BeCoeff_Destroy     , &
                                 BeCoeff_Create      , &
                                 BeCoeff_ValidRelease, &
                                 BeCoeff_Info
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: BeCoeff_Binary_InquireFile
  PUBLIC :: BeCoeff_Binary_ReadFile
  PUBLIC :: BeCoeff_Binary_WriteFile
  PUBLIC :: BeCoeff_Binary_IOVersion

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
    '$Id: BeCoeff_Binary_IO.f90 6624 2010-02-13 19:17:31Z paul.vandelst@noaa.gov $'
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Default message length
  INTEGER, PARAMETER :: ML = 256
  ! Old integer flag setting
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
!:sdoc+:
!
! NAME:
!       BeCoeff_Binary_InquireFile
!
! PURPOSE:
!       Function to inquire BeCoeff object Binary format files.
!
! CALLING SEQUENCE:
!       Error_Status = BeCoeff_Binary_InquireFile( &
!                        Filename, &
!                        n_Latitudes  = n_Latitudes , &
!                        n_Longitudes = n_Longitudes, &
!                        Release      = Release     , &
!                        Version      = Version       )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       BeCoeff data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Latitudes:    Number of regularly-spaced latitudes from 90S to 90N for
!                       which there is geomagnetic field data. 
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Longitudes:   Number of regularly-spaced longitudes from 0 to 360E for
!                       which there is geomagnetic field data.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:        The release number of the BeCoeff file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:        The version number of the BeCoeff file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
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

  FUNCTION BeCoeff_Binary_InquireFile( &
    Filename    , &  ! Input
    n_Latitudes , &  ! Optional output
    n_Longitudes, &  ! Optional output
    Release     , &  ! Optional output
    Version     ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),      INTENT(IN)  :: Filename
    INTEGER, OPTIONAL, INTENT(OUT) :: n_Latitudes 
    INTEGER, OPTIONAL, INTENT(OUT) :: n_Longitudes
    INTEGER, OPTIONAL, INTENT(OUT) :: Release         
    INTEGER, OPTIONAL, INTENT(OUT) :: Version         
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'BeCoeff_InquireFile(Binary)'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(BeCoeff_type) :: BeCoeff

    ! Setup
    err_stat = SUCCESS
    fid = -100
    ! Check that the file exists
    IF ( .NOT. File_Exists( Filename ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Open the BeCoeff data file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Read the release and version
    READ( fid,IOSTAT=io_stat ) BeCoeff%Release, BeCoeff%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF
    
    ! Read the dimensions
    READ( fid,IOSTAT=io_stat ) BeCoeff%n_Latitudes, BeCoeff%n_Longitudes
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
    IF ( PRESENT(n_Latitudes ) ) n_Latitudes  = BeCoeff%n_Latitudes 
    IF ( PRESENT(n_Longitudes) ) n_Longitudes = BeCoeff%n_Longitudes
    IF ( PRESENT(Release     ) ) Release      = BeCoeff%Release     
    IF ( PRESENT(Version     ) ) Version      = BeCoeff%Version     

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

  END FUNCTION BeCoeff_Binary_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       BeCoeff_Binary_ReadFile
!
! PURPOSE:
!       Function to read BeCoeff object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = BeCoeff_Binary_ReadFile( &
!                        Filename     , &
!                        BeCoeff      , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       BeCoeff format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       BeCoeff:        BeCoeff object containing the geomagnetic field data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(BeCoeff_type)
!                       DIMENSION:  Scalar
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

  FUNCTION BeCoeff_Binary_ReadFile( &
    Filename, &  ! Input
    BeCoeff , &  ! Output
    Quiet   , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),       INTENT(IN)  :: Filename
    TYPE(BeCoeff_type), INTENT(OUT) :: BeCoeff
    LOGICAL,  OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,  OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'BeCoeff_ReadFile(Binary)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(BeCoeff_type) :: dummy

    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF

    
    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read and check the release and version
    READ( fid,IOSTAT=io_stat ) dummy%Release, dummy%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( .NOT. BeCoeff_ValidRelease( dummy ) ) THEN
      msg = 'BeCoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    
    
    ! Read the geomagnetic field data
    ! ...Read the dimensions
    READ( fid,IOSTAT=io_stat ) dummy%n_Latitudes, dummy%n_Longitudes
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the object
    CALL BeCoeff_Create( BeCoeff, dummy%n_Latitudes, dummy%n_Longitudes )
    IF ( .NOT. BeCoeff_Associated( BeCoeff ) ) THEN
      msg = 'BeCoeff object allocation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the BeCoeff data
    READ( fid,IOSTAT=io_stat ) BeCoeff%LUT
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading BeCoeff data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Assign the version number read in
    BeCoeff%Version = dummy%Version


    ! Close the file
    CLOSE( fid,IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF

 
    ! Output an info message
    IF ( noisy ) THEN
      CALL BeCoeff_Info( BeCoeff, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      IF ( File_Open(Filename) ) THEN
        CLOSE( fid,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      CALL BeCoeff_Destroy( BeCoeff )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION BeCoeff_Binary_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       BeCoeff_Binary_WriteFile
!
! PURPOSE:
!       Function to write BeCoeff object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = BeCoeff_Binary_WriteFile( &
!                        Filename, &
!                        BeCoeff , &
!                        Quiet = Quiet )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       BeCoeff format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       BeCoeff:        Object containing the geomagnetic field data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(BeCoeff_type)
!                       DIMENSION:  Scalar
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
!       - If an error occurs, the output file is deleted before
!         returning to the calling routine.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION BeCoeff_Binary_WriteFile( &
    Filename, &  ! Input
    BeCoeff , &  ! Input
    Quiet   , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),       INTENT(IN)  :: Filename
    TYPE(BeCoeff_type), INTENT(IN)  :: BeCoeff
    LOGICAL,  OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,  OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'BeCoeff_WriteFile(Binary)'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
 
    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF


    ! Check the BeCoeff object
    ! ...Is there any data?
    IF ( .NOT. BeCoeff_Associated( BeCoeff ) ) THEN 
      msg = 'Input BeCoeff object is not allocated.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. BeCoeff_ValidRelease( BeCoeff ) ) THEN
      msg = 'BeCoeff Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Open the file for writing
    err_stat = Open_Binary_File( Filename, fid, For_Output=SET )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the release and version
    WRITE( fid,IOSTAT=io_stat ) BeCoeff%Release, BeCoeff%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    
    
    ! Write the geomagnetic field data
    ! ...Write the dimensions
    WRITE( fid,IOSTAT=io_stat ) BeCoeff%n_Latitudes, BeCoeff%n_Longitudes
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the BeCoeff data
    WRITE( fid,IOSTAT=io_stat ) BeCoeff%LUT
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing BeCoeff data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL BeCoeff_Info( BeCoeff, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp()
      IF ( File_Open(Filename) ) THEN
        CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deleting output file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION BeCoeff_Binary_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       BeCoeff_Binary_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL BeCoeff_Binary_IOVersion( Id )
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

  SUBROUTINE BeCoeff_Binary_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE BeCoeff_Binary_IOVersion

END MODULE BeCoeff_Binary_IO
