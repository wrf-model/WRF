!
! NLTE_Predictor_IO
!
! Module containing routines to read and write Binary format
! NLTE_Predictor data files.
!
!
! CREATION HISTORY:
!       Written by:  Paul van Delst, 15-Mar-2011
!                    paul.vandelst@noaa.gov
!

MODULE NLTE_Predictor_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds           , ONLY: Long, Double
  USE File_Utility         , ONLY: File_Open, File_Exists
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility  , ONLY: Open_Binary_File
  USE NLTE_Predictor_Define, ONLY: NLTE_Predictor_type        , &
                                   NLTE_Predictor_Destroy     , &
                                   NLTE_Predictor_ValidRelease, &
                                   NLTE_Predictor_Info
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: NLTE_Predictor_InquireFile
  PUBLIC :: NLTE_Predictor_ReadFile
  PUBLIC :: NLTE_Predictor_WriteFile
  PUBLIC :: NLTE_Predictor_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
    '$Id: NLTE_Predictor_IO.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
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
!       NLTE_Predictor_InquireFile
!
! PURPOSE:
!       Function to inquire a Binary format NLTE_Predictor file.
!
! CALLING SEQUENCE:
!       Error_Status = NLTE_Predictor_InquireFile( &
!                        Filename               , &
!                        n_Profiles = n_Profiles, &
!                        Release    = Release   , &
!                        Version    = Version     )
!
! INPUTS:
!       Filename:      Character string specifying the name of the NLTE
!                      predictor data file to inquire.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Profiles:    The number of profiles for which there is NLTE
!                      predictor information in the data file.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Release:       The data/file release number. Used to check
!                      for data/software mismatch.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:       The data/file version number. Used for
!                      purposes only in identifying the dataset for
!                      a particular release.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error
!                      status. The error codes are defined in the
!                      Message_Handler module.
!                      If == SUCCESS the file inquire was successful
!                         == FAILURE an unrecoverable error occurred.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION NLTE_Predictor_InquireFile( &
    Filename  , &  ! Input
    n_Profiles, &  ! Optional output  
    Release   , &  ! Optional Output
    Version   ) &  ! Optional Output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTE_Predictor_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: rel, ver, m

 
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
    READ( fid, IOSTAT=io_stat ) rel, ver
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the number of profiles
    READ( fid, IOSTAT=io_stat ) m
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimensions from ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Assign the return arguments
    IF ( PRESENT(n_Profiles) ) n_Profiles = m
    IF ( PRESENT(Release   ) ) Release    = rel
    IF ( PRESENT(Version   ) ) Version    = ver
    
  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      ! Close file if necessary
      IF ( File_Open(fid) ) THEN
        CLOSE( fid, IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup'
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp
    
  END FUNCTION NLTE_Predictor_InquireFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTE_Predictor_ReadFile
!
! PURPOSE:
!       Function to read NLTE_Predictor object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = NLTE_Predictor_ReadFile( &
!                        Filename      , &
!                        NLTE_Predictor, &
!                        Quiet      = Quiet     , &
!                        n_Profiles = n_Profiles  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       NLTE_Predictor format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       NLTE_Predictor: NLTE_Predictor object containing the NLTE correction
!                       algorithm predictor data.
!                       UNITS:      N/A
!                       TYPE:       NLTE_Predictor_type
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
! OPTIONAL OUTPUTS:
!       n_Profiles:     The number of profiles for which data was read.
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

  FUNCTION NLTE_Predictor_ReadFile( &
    Filename      , &  ! Input
    NLTE_Predictor, &  ! Output
    Quiet         , &  ! Optional input
    n_Profiles    , &  ! Optional output
    Debug         ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),              INTENT(IN)  :: Filename
    TYPE(NLTE_Predictor_type), INTENT(OUT) :: NLTE_Predictor(:)
    LOGICAL,         OPTIONAL, INTENT(IN)  :: Quiet          
    INTEGER,         OPTIONAL, INTENT(OUT) :: n_Profiles
    LOGICAL,         OPTIONAL, INTENT(IN)  :: Debug          
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTE_Predictor_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: n_file_profiles
    INTEGER :: m, n_input_profiles
    TYPE(NLTE_Predictor_type) :: dummy
    

    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF

   
    ! Open the file if it exists
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


    ! Read and check the release and version
    READ( fid, IOSTAT=io_stat ) dummy%Release, dummy%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( .NOT. NLTE_Predictor_ValidRelease( dummy ) ) THEN
      msg = 'NLTE_Predictor Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions     
    READ( fid, IOSTAT=io_stat ) n_file_profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading profile dimension from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if n_Profiles in file is > size of output array
    n_input_profiles = SIZE(NLTE_Predictor)
    IF ( n_file_profiles > n_input_profiles ) THEN
      WRITE( msg,'("Number of profiles, ",i0," > size of the output NLTE_Predictor ", &
                  &" array, ",i0,". Only the first ",i0, &
                  &" profiles will be read.")' ) &
                  n_file_profiles, n_input_profiles, n_input_profiles
      CALL Display_Message( ROUTINE_NAME, msg, WARNING )
    END IF
    n_input_profiles = MIN(n_input_profiles, n_file_profiles)
    
    
    ! Loop over all the profiles
    Profile_Loop: DO m = 1, n_input_profiles

      ! Read the NLTE predictor data
      ! ...Read the dimensions
      READ( fid, IOSTAT=io_stat ) &
        NLTE_Predictor(m)%n_Layers    , &
        NLTE_Predictor(m)%n_Predictors
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error reading data dimensions for profile ",i0, &
                    &". IOSTAT = ",i0)' ) m, io_stat
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Read the logical indicators
      READ( fid, IOSTAT=io_stat ) &
        NLTE_Predictor(m)%Is_Active , &
        NLTE_Predictor(m)%Compute_Tm
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error reading logical indicators for profile ",i0, &
                    &". IOSTAT = ",i0)' ) m, io_stat
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Read the array indices
      READ( fid, IOSTAT=io_stat ) &
        NLTE_Predictor(m)%k1, &
        NLTE_Predictor(m)%k2, &
        NLTE_Predictor(m)%isen, &
        NLTE_Predictor(m)%isol
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error reading array indices for profile ",i0, &
                    &". IOSTAT = ",i0)' ) m, io_stat
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Read the predictors and interpolation weights
      READ( fid, IOSTAT=io_stat ) &
        NLTE_Predictor(m)%Tm       , &
        NLTE_Predictor(m)%Predictor, &
        NLTE_Predictor(m)%w        
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error reading predictors and interpolation weights for profile ",i0, &
                    &". IOSTAT = ",i0)' ) m, io_stat
        CALL Read_Cleanup(); RETURN
      END IF

      ! Explicitly assign the version number
      NLTE_Predictor(m)%Version = dummy%Version

    END DO Profile_Loop
        
    
    ! Close the file
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_input_profiles


    ! Output an info message
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of profiles read from ",a,": ",i0)' ) TRIM(Filename), n_input_profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
    END IF

   CONTAINS
   
     SUBROUTINE Read_CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing input file during error cleanup.'
       END IF
       CALL NLTE_Predictor_Destroy( NLTE_Predictor )
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Read_CleanUp

  END FUNCTION NLTE_Predictor_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTE_Predictor_WriteFile
!
! PURPOSE:
!       Function to write NLTE_Predictor object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = NLTE_Predictor_WriteFile( &
!                        Filename           , &
!                        NLTE_Predictor     , &
!                        Quiet    = Quiet     )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       NLTE_Predictor format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       NLTE_Predictor: NLTE_Predictor object containing the NLTE correction
!                       algorithm predictor data.
!                       UNITS:      N/A
!                       TYPE:       NLTE_Predictor_type
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

  FUNCTION NLTE_Predictor_WriteFile( &
    Filename      , &  ! Input
    NLTE_Predictor, &  ! Input
    Quiet         , &  ! Optional input
    Debug         ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),              INTENT(IN) :: Filename
    TYPE(NLTE_Predictor_type), INTENT(IN) :: NLTE_Predictor(:)
    LOGICAL,         OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,         OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTE_Predictor_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m, n_output_profiles
    

    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF

   
    ! Open the file for output
    err_stat = Open_Binary_File( Filename, fid, For_Output=.TRUE. )
    IF ( err_Stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_CleanUp(); RETURN
    END IF


    ! Write the release and version
    WRITE( fid,IOSTAT=io_stat ) NLTE_Predictor(1)%Release, NLTE_Predictor(1)%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    n_output_profiles = SIZE(NLTE_Predictor)
    WRITE( fid, IOSTAT=io_stat ) n_output_profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing profile dimension to ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    
    
    ! Loop over all the profiles
    Profile_Loop: DO m = 1, n_output_profiles

      ! Write the NLTE predictor data
      ! ...Write the dimensions
      WRITE( fid, IOSTAT=io_stat ) &
        NLTE_Predictor(m)%n_Layers    , &
        NLTE_Predictor(m)%n_Predictors
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error writing data dimensions for profile ",i0, &
                    &". IOSTAT = ",i0)' ) m, io_stat
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Write the logical indicators
      WRITE( fid, IOSTAT=io_stat ) &
        NLTE_Predictor(m)%Is_Active , &
        NLTE_Predictor(m)%Compute_Tm
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error writing logical indicators for profile ",i0, &
                    &". IOSTAT = ",i0)' ) m, io_stat
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Write the array indices
      WRITE( fid, IOSTAT=io_stat ) &
        NLTE_Predictor(m)%k1, &
        NLTE_Predictor(m)%k2, &
        NLTE_Predictor(m)%isen, &
        NLTE_Predictor(m)%isol
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error writing array indices for profile ",i0, &
                    &". IOSTAT = ",i0)' ) m, io_stat
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Write the predictors and interpolation weights
      WRITE( fid, IOSTAT=io_stat ) &
        NLTE_Predictor(m)%Tm       , &
        NLTE_Predictor(m)%Predictor, &
        NLTE_Predictor(m)%w        
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error writing predictors and interpolation weights for profile ",i0, &
                    &". IOSTAT = ",i0)' ) m, io_stat
        CALL Write_Cleanup(); RETURN
      END IF

    END DO Profile_Loop
    

    ! Close the file
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of profiles written to ",a,": ",i0)' ) TRIM(Filename), n_output_profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
    END IF

   CONTAINS
   
     SUBROUTINE Write_Cleanup()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat, STATUS=WRITE_ERROR_STATUS )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing output file during error cleanup.'
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Write_Cleanup

  END FUNCTION NLTE_Predictor_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTE_Predictor_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL NLTE_Predictor_IOVersion( Id )
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

  SUBROUTINE NLTE_Predictor_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE NLTE_Predictor_IOVersion
  
END MODULE NLTE_Predictor_IO
