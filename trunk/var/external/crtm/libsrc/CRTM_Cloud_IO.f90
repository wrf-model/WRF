!
! CRTM_Cloud_IO
!
! Module containing routines to inquire, read, and write CRTM
! Cloud object datafiles.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 16-Mar-2005
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_Cloud_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE File_Utility,        ONLY: File_Open, File_Exists
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE CRTM_Parameters    , ONLY: SET
  USE CRTM_Cloud_Define  , ONLY: CRTM_Cloud_type, &
                                 CRTM_Cloud_Associated, &
                                 CRTM_Cloud_Destroy, &
                                 CRTM_Cloud_Create
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_Cloud_InquireFile
  PUBLIC :: CRTM_Cloud_ReadFile
  PUBLIC :: CRTM_Cloud_WriteFile
  PUBLIC :: CRTM_Cloud_IOVersion

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
    '$Id: CRTM_Cloud_IO.f90 6125 2009-12-18 20:19:59Z paul.vandelst@noaa.gov $'
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
!       CRTM_Cloud_InquireFile
!
! PURPOSE:
!       Function to inquire CRTM Cloud object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Cloud_InquireFile( Filename           , &
!                                              n_Clouds = n_Clouds  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       CRTM Cloud data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Clouds:       The number of Cloud profiles in the data file.
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

  FUNCTION CRTM_Cloud_InquireFile( &
    Filename, &  ! Input
    n_Clouds) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Clouds
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Cloud_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: na

    ! Setup
    err_stat = SUCCESS
    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Open the cloud data file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Read the number of clouds dimension
    READ( fid,IOSTAT=io_stat ) na
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading n_Clouds dimensions from ",a,". IOSTAT = ",i0)' ) &
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
    IF ( PRESENT(n_Clouds) ) n_Clouds = na

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

  END FUNCTION CRTM_Cloud_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_ReadFile
!
! PURPOSE:
!       Function to read CRTM Cloud object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Cloud_ReadFile( Filename           , &
!                                           Cloud              , &
!                                           Quiet    = Quiet   , &
!                                           No_Close = No_Close, &
!                                           n_Clouds = n_Clouds  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       Cloud format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Cloud:          CRTM Cloud object array containing the Cloud data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Cloud_type
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
!       n_Clouds:       The actual number of cloud profiles read in.
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

  FUNCTION CRTM_Cloud_ReadFile( &
    Filename, &  ! Input
    Cloud   , &  ! Output
    Quiet   , &  ! Optional input
    No_Close, &  ! Optional input
    n_Clouds, &  ! Optional output
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(CRTM_Cloud_type) , INTENT(OUT) :: Cloud(:)
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,      OPTIONAL, INTENT(IN)  :: No_Close
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Clouds
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Cloud_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    LOGICAL :: Yes_Close
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m
    INTEGER :: nc

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


    ! Read the number of clouds dimension
    READ( fid,IOSTAT=io_stat ) nc
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading n_Clouds data dimension from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ...Check if output array large enough
    IF ( nc > SIZE(Cloud) ) THEN
      WRITE( msg,'("Number of clouds, ",i0," > size of the output ",&
             &"Cloud object array, ",i0,".")' ) &
             nc, SIZE(Cloud)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Read the cloud data
    Cloud_Loop: DO m = 1, nc
      err_stat = Read_Record( fid, Cloud(m) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading Cloud element #",i0," from ",a)' ) &
               m, TRIM(Filename)
        CALL Read_Cleanup(Close_File=.TRUE.); RETURN
      END IF
    END DO Cloud_Loop


    ! Close the file
    IF ( Yes_Close ) THEN
      CLOSE( fid,IOSTAT=io_stat )
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
        CALL Read_Cleanup(); RETURN
      END IF
    END IF
    
    
    ! Set the optional return values
    IF ( PRESENT(n_Clouds) ) n_Clouds = nc

 
    ! Output an info message
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of clouds read from ",a,": ",i0)' ) TRIM(Filename), nc
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
      CALL CRTM_Cloud_Destroy( Cloud )
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION CRTM_Cloud_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_WriteFile
!
! PURPOSE:
!       Function to write CRTM Cloud object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Cloud_WriteFile( Filename           , &
!                                            Cloud              , &
!                                            Quiet    = Quiet   , &
!                                            No_Close = No_Close  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       Cloud format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Cloud:          CRTM Cloud object array containing the Cloud data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Cloud_type
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

  FUNCTION CRTM_Cloud_WriteFile( &
    Filename, &  ! Input
    Cloud   , &  ! Input
    Quiet   , &  ! Optional input
    No_Close, &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(CRTM_Cloud_type) , INTENT(IN)  :: Cloud(:)
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,      OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Cloud_WriteFile'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    LOGICAL :: Yes_Close
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m, nc
 
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

    ! Check the Cloud structure dimensions
    IF ( ANY(Cloud%n_Layers < 1) ) THEN 
      msg = 'Dimensions of Cloud structures are < or = 0.'
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


    ! Write the number of clouds dimension
    nc = SIZE(Cloud)    
    WRITE( fid,IOSTAT=io_stat) nc
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing n_Clouds data dimension to ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the cloud data
    Cloud_Loop: DO m = 1, nc
      err_stat = Write_Record( fid, Cloud(m) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing Cloud element #",i0," to ",a)' ) &
               m, TRIM(Filename)
        CALL Write_Cleanup(Close_File=.TRUE.); RETURN
      END IF
    END DO Cloud_Loop


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
      WRITE( msg,'("Number of clouds written to ",a,": ",i0)' ) TRIM(Filename), nc
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

  END FUNCTION CRTM_Cloud_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Cloud_IOVersion( Id )
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

  SUBROUTINE CRTM_Cloud_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_Cloud_IOVersion


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
!       Utility function to read a single CRTM Cloud object
!
! CALLING SEQUENCE:
!       Error_Status = Read_Record( FileID, Cloud )
!
! INPUTS:
!       FileID:       Logical unit number from which to read data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Cloud:        CRTM Cloud object containing the data read in.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
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
    fid  , &  ! Input
    cloud  ) &  ! Output
  RESULT( err_stat )
    ! Arguments
    INTEGER               , INTENT(IN)     :: fid
    TYPE(CRTM_Cloud_type) , INTENT(IN OUT) :: cloud
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Cloud_ReadFile(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: n_Layers

    ! Set up
    err_stat = SUCCESS


    ! Read the dimensions
    READ( fid,IOSTAT=io_stat ) n_Layers
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Allocate the structure
    CALL CRTM_Cloud_Create( cloud, n_Layers )
    IF ( .NOT. CRTM_Cloud_Associated( cloud ) ) THEN
      msg = 'Cloud object allocation failed.'
      CALL Read_Record_Cleanup(); RETURN
    END IF

    
    ! Read the cloud data
    READ( fid,IOSTAT=io_stat ) Cloud%Type, &
                               Cloud%Effective_Radius, &
                               Cloud%Effective_Variance, &
                               Cloud%Water_Content
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Cloud data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Read_Record_Cleanup()
      ! Deallocate cloud structure if necessary
      CALL CRTM_Cloud_Destroy( cloud )
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
!       Function to write a single CRTM Cloud object
!
! CALLING SEQUENCE:
!       Error_Status = Write_Record( FileID, Cloud )
!
! INPUT ARGUMENTS:
!       FileID:       Logical unit number to which data is written
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Cloud:        CRTM Cloud object containing the data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
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
    fid   , &  ! Input
    cloud ) &  ! Input
  RESULT( err_stat )
    ! Arguments
    INTEGER               , INTENT(IN)  :: fid
    TYPE(CRTM_Cloud_type) , INTENT(IN)  :: cloud
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Cloud_WriteFile(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
 
    ! Setup
    err_stat = SUCCESS
    IF ( .NOT. CRTM_Cloud_Associated( cloud ) ) THEN
      msg = 'Input Cloud object is not used.'
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid,IOSTAT=io_stat ) Cloud%n_Layers
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the data
    WRITE( fid,IOSTAT=io_stat ) Cloud%Type, &
                                Cloud%Effective_Radius, &
                                Cloud%Effective_Variance, &
                                Cloud%Water_Content
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing Cloud data. IOSTAT = ",i0)' ) io_stat
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

END MODULE CRTM_Cloud_IO
