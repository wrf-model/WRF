!
! CRTM_Surface_IO
!
! Module containing routines to inquire, read, and write CRTM
! Surface object datafiles.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 21-Jul-2004
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_Surface_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE CRTM_Parameters    , ONLY: SET
  USE CRTM_Surface_Define, ONLY: SURFACE_TYPE_NAME, &
                                 N_VALID_LAND_TYPES, N_VALID_WATER_TYPES, &
                                 N_VALID_SNOW_TYPES, N_VALID_ICE_TYPES, &
                                 CRTM_Surface_type, &
                                 CRTM_Surface_Associated, &
                                 CRTM_Surface_Destroy, &
                                 CRTM_Surface_Create, &
                                 CRTM_Surface_CoverageType, &
                                 CRTM_Surface_IsCoverageValid, &
                                 CRTM_SensorData_Associated, &
                                 CRTM_SensorData_Create
  USE CRTM_SensorData_IO , ONLY: CRTM_SensorData_ReadFile, &
                                 CRTM_SensorData_WriteFile
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_Surface_InquireFile
  PUBLIC :: CRTM_Surface_ReadFile
  PUBLIC :: CRTM_Surface_WriteFile
  PUBLIC :: CRTM_Surface_IOVersion


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE CRTM_Surface_ReadFile
    MODULE PROCEDURE Read_Surface_Rank1
    MODULE PROCEDURE Read_Surface_Rank2
  END INTERFACE CRTM_Surface_ReadFile
  
  INTERFACE CRTM_Surface_WriteFile
    MODULE PROCEDURE Write_Surface_Rank1
    MODULE PROCEDURE Write_Surface_Rank2
  END INTERFACE CRTM_Surface_WriteFile
  

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: CRTM_Surface_IO.f90 6946 2010-03-10 03:05:03Z paul.vandelst@noaa.gov $'
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Message string length
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
!       CRTM_Surface_InquireFile
!
! PURPOSE:
!       Function to inquire CRTM Surface object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Surface_InquireFile( Filename               , &
!                                                n_Channels = n_Channels, &
!                                                n_Profiles = n_Profiles  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       CRTM Surface data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Channels:     The number of spectral channels for which there is
!                       data in the file. Note that this value will always
!                       be 0 for a profile-only dataset-- it only has meaning
!                       for K-matrix data.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       n_Profiles:     The number of profiles in the data file.
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

  FUNCTION CRTM_Surface_InquireFile( &
    Filename   , &  ! Input
    n_Channels , &  ! Optional output
    n_Profiles ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Surface_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: l, m
 
    ! Set up
    err_stat = SUCCESS
    ! Check that the file exists
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

    ! Read the number of channels,profiles
    READ( fid, IOSTAT=io_stat ) l, m
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
    IF ( PRESENT(n_Channels) ) n_Channels = l
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

  END FUNCTION CRTM_Surface_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Surface_ReadFile
!
! PURPOSE:
!       Function to read CRTM Surface object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Surface_ReadFile( Filename                , &
!                                             Surface                 , &
!                                             Quiet      = Quiet      , &
!                                             n_Channels = n_Channels , &
!                                             n_Profiles = n_Profiles , &
!
! INPUTS:
!       Filename:     Character string specifying the name of an
!                     Surface format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Surface:      CRTM Surface object array containing the Surface
!                     data. Note the following meanings attributed to the
!                     dimensions of the object array:
!                     Rank-1: M profiles.
!                             Only profile data are to be read in. The file
!                             does not contain channel information. The
!                             dimension of the structure is understood to
!                             be the PROFILE dimension.
!                     Rank-2: L channels  x  M profiles
!                             Channel and profile data are to be read in.
!                             The file contains both channel and profile
!                             information. The first dimension of the 
!                             structure is the CHANNEL dimension, the second
!                             is the PROFILE dimension. This is to allow
!                             K-matrix structures to be read in with the
!                             same function.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
!                     DIMENSION:  Rank-1 (M) or Rank-2 (L x M)
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
!       n_Channels:   The number of channels for which data was read. Note that
!                     this value will always be 0 for a profile-only dataset--
!                     it only has meaning for K-matrix data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       n_Profiles:   The number of profiles for which data was read.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
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

  FUNCTION Read_Surface_Rank1( &
    Filename  , &  ! Input
    Surface   , &  ! Output
    Quiet     , &  ! Optional input
    n_Channels, &  ! Optional output
    n_Profiles, &  ! Optional output
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(CRTM_Surface_type), INTENT(OUT) :: Surface(:)  ! M
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Profiles
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Surface_ReadFile(M)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: n_File_Channels, n_File_Profiles
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
    READ( fid,IOSTAT=io_stat ) n_File_Channels, n_File_Profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimensions from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check that n_Channels is zero
    IF ( n_File_Channels /= 0 ) THEN
      WRITE( msg,'("n_Channels dimensions in ",a," is not zero for a rank-1 ",&
                  &"(i.e. profiles only) Surface read.")' ) TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if n_Profiles in file is > size of output array
    n_Input_Profiles = SIZE(Surface)
    IF ( n_File_Profiles > n_Input_Profiles ) THEN
      WRITE( msg,'("Number of profiles, ",i0," > size of the output Surface", &
                  &" array, ",i0,". Only the first ",i0, &
                  &" profiles will be read.")' ) &
                  n_File_Profiles, n_Input_Profiles, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING )
    END IF
    n_Input_Profiles = MIN(n_Input_Profiles, n_File_Profiles)


    ! Loop over all the profiles
    Profile_Loop: DO m = 1, n_Input_Profiles
      err_stat = Read_Record( fid, Surface(m), &
                              Quiet = Quiet, &
                              Debug = Debug  )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading Surface element (",i0,") from ",a)' ) &
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
    IF ( PRESENT(n_Channels) ) n_Channels = 0
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
      CALL CRTM_Surface_Destroy( Surface )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION Read_Surface_Rank1


  FUNCTION Read_Surface_Rank2( &
    Filename  , &  ! Input
    Surface   , &  ! Output
    Quiet     , &  ! Optional input
    n_Channels, &  ! Optional output
    n_Profiles, &  ! Optional output
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(CRTM_Surface_type), INTENT(OUT) :: Surface(:,:)  ! L x M
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Profiles
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Surface_ReadFile(L x M)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: l, n_File_Channels, n_Input_Channels
    INTEGER :: m, n_File_Profiles, n_Input_Profiles
 

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
    READ( fid,IOSTAT=io_stat ) n_File_Channels, n_File_Profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimensions from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if n_Channels in file is > size of output array
    n_Input_Channels = SIZE(Surface,DIM=1)
    IF ( n_File_Channels > n_Input_Channels ) THEN
      WRITE( msg,'("Number of channels, ",i0," > size of the output Surface", &
                  &" array dimension, ",i0,". Only the first ",i0, &
                  &" channels will be read.")' ) &
                  n_File_Channels, n_Input_Channels, n_Input_Channels
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING )
    END IF
    n_Input_Channels = MIN(n_Input_Channels, n_File_Channels)
    ! ...Check if n_Profiles in file is > size of output array
    n_Input_Profiles = SIZE(Surface,DIM=2)
    IF ( n_File_Profiles > n_Input_Profiles ) THEN
      WRITE( msg, '( "Number of profiles, ",i0," > size of the output Surface", &
                    &" array dimension, ",i0,". Only the first ",i0, &
                    &" profiles will be read.")' ) &
                    n_File_Profiles, n_Input_Profiles, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING )
    END IF
    n_Input_Profiles = MIN(n_Input_Profiles, n_File_Profiles)


    ! Loop over all the profiles and channels
    Profile_Loop: DO m = 1, n_Input_Profiles
      Channel_Loop: DO l = 1, n_Input_Channels
        err_stat = Read_Record( fid, Surface(l,m), &
                                Quiet = Quiet, &
                                Debug = Debug )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error reading Surface element (",i0,",",i0,") from ",a)' ) &
                 l, m, TRIM(Filename)
          CALL Read_Cleanup(); RETURN
        END IF
      END DO Channel_Loop
    END DO Profile_Loop


    ! Close the file
    CLOSE( fid,IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Channels) ) n_Channels = n_Input_Channels
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_Input_Profiles


    ! Output an info message
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of channels and profiles read from ",a,": ",i0,1x,i0)' ) &
             TRIM(Filename), n_Input_Channels, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      CALL CRTM_Surface_Destroy( Surface )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION Read_Surface_Rank2


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Surface_WriteFile
!
! PURPOSE:
!       Function to write CRTM Surface object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Surface_WriteFile( Filename     , &
!                                              Surface   , &
!                                              Quiet = Quiet  )
!
! INPUTS:
!       Filename:     Character string specifying the name of the
!                     Surface format data file to write.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Surface:      CRTM Surface object array containing the Surface
!                     data. Note the following meanings attributed to the
!                     dimensions of the Surface array:
!                     Rank-1: M profiles.
!                             Only profile data are to be read in. The file
!                             does not contain channel information. The
!                             dimension of the array is understood to
!                             be the PROFILE dimension.
!                     Rank-2: L channels  x  M profiles
!                             Channel and profile data are to be read in.
!                             The file contains both channel and profile
!                             information. The first dimension of the 
!                             array is the CHANNEL dimension, the second
!                             is the PROFILE dimension. This is to allow
!                             K-matrix structures to be read in with the
!                             same function.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
!                     DIMENSION:  Rank-1 (M) or Rank-2 (L x M)
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

  FUNCTION Write_Surface_Rank1( &
    Filename, &  ! Input
    Surface , &  ! Input
    Quiet   , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN) :: Filename
    TYPE(CRTM_Surface_type), INTENT(IN) :: Surface(:)  ! M
    LOGICAL,       OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,       OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Surface_WriteFile(M)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m, n_Output_Profiles
 
    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) Noisy = .TRUE.
    END IF
    ! Dimensions
    n_Output_Profiles = SIZE(Surface)


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, For_Output=SET )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid,IOSTAT=io_stat ) 0, n_Output_Profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing dimensions to ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF

    
    ! Write the data
    Profile_Loop: DO m = 1, n_Output_Profiles
      err_stat = Write_Record( fid, Surface(m), &
                               Quiet = Quiet, &
                               Debug = Debug )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing Surface element (",i0,") to ",a)' ) m, TRIM(Filename)
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

  END FUNCTION Write_Surface_Rank1


  FUNCTION Write_Surface_Rank2( &
    Filename, &  ! Input
    Surface , &  ! Input
    Quiet   , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(CRTM_Surface_type), INTENT(IN)  :: Surface(:,:)  ! L x M
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Surface_WriteFile(L x M)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: l, n_Output_Channels
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
    ! Dimensions
    n_Output_Channels = SIZE(Surface,DIM=1)
    n_Output_Profiles = SIZE(Surface,DIM=2)


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, For_Output=SET )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid,IOSTAT=io_stat ) n_Output_Channels, n_Output_Profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing dimensions to ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the data
    Profile_Loop: DO m = 1, n_Output_Profiles
      Channel_Loop: DO l = 1, n_Output_Channels
        err_stat = Write_Record( fid, Surface(l,m), &
                                 Quiet = Quiet, &
                                 Debug = Debug )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error writing Surface element (",i0,",",i0,") to ",a)' ) &
                 l, m, TRIM(Filename)
          CALL Write_Cleanup(); RETURN
        END IF
      END DO Channel_Loop
    END DO Profile_Loop


    ! Close the file (if error, no delete)
    CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of channels and profiles written to ",a,": ",i0,1x,i0 )' ) &
             TRIM(Filename), n_Output_Channels, n_Output_Profiles
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

  END FUNCTION Write_Surface_Rank2


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Surface_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Surface_IOVersion( Id )
!
! OUTPUTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Surface_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_Surface_IOVersion


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
!       Utility function to read a single surface data record
!
! CALLING SEQUENCE:
!       Error_Status = Read_Record( FileID       , &
!                                   Surface      , &
!                                   Quiet = Quiet  )
!
! INPUTS:
!       FileID:       Logical unit number from which to read data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Surface:      CRTM Surface object containing the data read in.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
!                     DIMENSION:  Scalar
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

  FUNCTION Read_Record( &
    fid        , &  ! Input
    sfc        , &  ! Output
    Quiet      , &  ! Optional input
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    INTEGER,                 INTENT(IN)  :: fid
    TYPE(CRTM_Surface_type), INTENT(OUT) :: sfc
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Surface_ReadFile(Record)'
    ! Function variables
    CHARACTER(ML)  :: msg
    LOGICAL :: Noisy
    INTEGER :: io_stat
    INTEGER :: Coverage_Type
    INTEGER :: n_Channels

    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) Noisy = .TRUE.
    END IF


    ! Read the gross surface type coverage
    READ( fid,IOSTAT=io_stat ) Coverage_Type, &
                               sfc%Land_Coverage, &
                               sfc%Water_Coverage, &
                               sfc%Snow_Coverage, &
                               sfc%Ice_Coverage
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'( "Error reading gross surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF
    ! ...Check the coverage fractions
    IF ( .NOT. CRTM_Surface_IsCoverageValid(sfc) ) THEN
      msg = 'Invalid surface coverage fraction(s) found'
      CALL Read_Record_Cleanup(); RETURN
    END IF
    ! ...Check the coverge surface type
    IF ( CRTM_Surface_CoverageType( sfc ) /= Coverage_Type ) THEN
      msg = 'Coverage surface type, '//&
            TRIM(SURFACE_TYPE_NAME(CRTM_Surface_CoverageType(sfc)))//&
            ', inconsistent with that specified in file.'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the surface type independent data
    READ( fid,IOSTAT=io_stat ) sfc%Wind_Speed
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading surface type independent data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the land surface type data
    READ( fid,IOSTAT=io_stat ) sfc%Land_Type, &
                               sfc%Land_Temperature, &
                               sfc%Soil_Moisture_Content, &
                               sfc%Canopy_Water_Content , &
                               sfc%Vegetation_Fraction, &
                               sfc%Soil_Temperature
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading land surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF
    ! ...Check the type
    IF ( sfc%Land_Type < 0 .OR. sfc%Land_Type > N_VALID_LAND_TYPES ) THEN
      msg = 'Invalid land surface type'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the water surface type data
    READ( fid,IOSTAT=io_stat ) sfc%Water_Type, &
                               sfc%Water_Temperature, &
                               sfc%Wind_Direction, &
                               sfc%Salinity
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading water surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF
    ! ...Check the type
    IF ( sfc%Water_Type < 0 .OR. sfc%Water_Type > N_VALID_WATER_TYPES ) THEN
      msg = 'Invalid water surface type'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the snow surface type data
    READ( fid,IOSTAT=io_stat ) sfc%Snow_Type, &
                               sfc%Snow_Temperature, &
                               sfc%Snow_Depth, &
                               sfc%Snow_Density, &
                               sfc%Snow_Grain_Size
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading snow surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF
    ! ...Check the type
    IF ( sfc%Snow_Type < 0 .OR. sfc%Snow_Type > N_VALID_SNOW_TYPES ) THEN
      msg = 'Invalid snow surface type'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the ice surface type data
    READ( fid,IOSTAT=io_stat ) sfc%Ice_Type, &
                               sfc%Ice_Temperature, &
                               sfc%Ice_Thickness, &
                               sfc%Ice_Density, &
                               sfc%Ice_Roughness
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading ice surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF
    ! ...Check the type
    IF ( sfc%Ice_Type < 0 .OR. sfc%Ice_Type > N_VALID_ICE_TYPES ) THEN
      msg = 'Invalid ice surface type'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the SensorData
    ! ...The dimensions
    READ( fid,IOSTAT=io_stat ) n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading SensorData dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF
    ! ...The data
    IF ( n_Channels > 0 ) THEN
      CALL CRTM_SensorData_Create(sfc%SensorData, n_Channels )
      IF ( .NOT. CRTM_SensorData_Associated(sfc%SensorData) ) THEN
        msg = 'Error creating SensorData object.'
        CALL Read_Record_Cleanup(); RETURN
      END IF
      READ( fid,IOSTAT=io_stat ) sfc%SensorData%Sensor_ID           , &  
                                 sfc%SensorData%WMO_Satellite_ID    , &
                                 sfc%SensorData%WMO_Sensor_ID       , &
                                 sfc%SensorData%Sensor_Channel      , &
                                 sfc%SensorData%Tb
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error reading SensorData. IOSTAT = ",i0)' ) io_stat
        CALL Read_Record_Cleanup(); RETURN
      END IF
    END IF

  CONTAINS
  
    SUBROUTINE Read_Record_Cleanup()
      CALL CRTM_Surface_Destroy( sfc )
      CLOSE( fid,IOSTAT=io_stat )
      IF ( io_stat /= 0 ) &
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
!       Function to write a single Surface data record
!
! CALLING SEQUENCE:
!       Error_Status = Write_Record( FileID       , &
!                                    Surface      , &
!                                    Quiet = Quiet  )
!
! INPUTS:
!       FileID:       Logical unit number to which data is written
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Surface:      CRTM Surface object containing the data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
!                     DIMENSION:  Scalar
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
!                     If == SUCCESS the record write was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION Write_Record( &
    fid  , &  ! Input
    sfc  , &  ! Input
    Quiet, &  ! Optional input
    Debug) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    INTEGER,                 INTENT(IN) :: fid
    TYPE(CRTM_Surface_type), INTENT(IN) :: sfc
    LOGICAL,       OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,       OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Surface_WriteFile(Record)'
    ! Function variables
    CHARACTER(ML)  :: msg
    LOGICAL :: Noisy
    INTEGER :: io_stat


    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) Noisy = .TRUE.
    END IF


    ! Write the gross surface type coverage
    WRITE( fid,IOSTAT=io_stat ) CRTM_Surface_CoverageType( sfc ), &
                                sfc%Land_Coverage, &
                                sfc%Water_Coverage, &
                                sfc%Snow_Coverage, &
                                sfc%Ice_Coverage
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing gross surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the surface type independent data
    WRITE( fid,IOSTAT=io_stat ) sfc%Wind_Speed
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing surface type independent data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the land surface type data
    WRITE( fid,IOSTAT=io_stat ) sfc%Land_Type, &
                                sfc%Land_Temperature, &
                                sfc%Soil_Moisture_Content, &
                                sfc%Canopy_Water_Content, &
                                sfc%Vegetation_Fraction, &
                                sfc%Soil_Temperature
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing land surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the water surface type data
    WRITE( fid,IOSTAT=io_stat ) sfc%Water_Type, &
                                sfc%Water_Temperature, &
                                sfc%Wind_Direction, &
                                sfc%Salinity
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing water surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the snow surface type data
    WRITE( fid,IOSTAT=io_stat ) sfc%Snow_Type, &
                                sfc%Snow_Temperature, &
                                sfc%Snow_Depth, &
                                sfc%Snow_Density, &
                                sfc%Snow_Grain_Size
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing snow surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the ice surface type data
    WRITE( fid,IOSTAT=io_stat ) sfc%Ice_Type, &
                                sfc%Ice_Temperature, &
                                sfc%Ice_Thickness, &
                                sfc%Ice_Density, &
                                sfc%Ice_Roughness
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing ice surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the SensorData object
    ! ...The dimensions
    WRITE( fid,IOSTAT=io_stat ) sfc%SensorData%n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing SensorData dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF
    ! ...The data
    IF ( sfc%SensorData%n_Channels > 0 ) THEN
      WRITE( fid,IOSTAT=io_stat ) sfc%SensorData%Sensor_ID           , &
                                  sfc%SensorData%WMO_Satellite_ID    , &
                                  sfc%SensorData%WMO_Sensor_ID       , &
                                  sfc%SensorData%Sensor_Channel      , &
                                  sfc%SensorData%Tb
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error writing SensorData. IOSTAT = ",i0)' ) io_stat
        CALL Write_Record_Cleanup(); RETURN
      END IF
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

END MODULE CRTM_Surface_IO
