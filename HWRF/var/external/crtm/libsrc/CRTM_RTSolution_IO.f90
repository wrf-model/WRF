!
! CRTM_RTSolution_IO
!
! Module containing routines to inquire, read, and write Binary format
! CRTM_RTSolution files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 10-May-2007
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_RTSolution_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE File_Utility          , ONLY: File_Exists, File_Open
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility   , ONLY: Open_Binary_File
  USE CRTM_Parameters       , ONLY: SET
  USE CRTM_RTSolution_Define, ONLY: CRTM_RTSolution_type, &
                                    CRTM_RTSolution_Associated, &
                                    CRTM_RTSolution_Destroy, &
                                    CRTM_RTSolution_Create
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_RTSolution_InquireFile
  PUBLIC :: CRTM_RTSolution_ReadFile
  PUBLIC :: CRTM_RTSolution_WriteFile
  PUBLIC :: CRTM_RTSolution_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: CRTM_RTSolution_IO.f90 6125 2009-12-18 20:19:59Z paul.vandelst@noaa.gov $'
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
!       CRTM_RTSolution_InquireFile
!
! PURPOSE:
!       Function to inquire CRTM RTSolution object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_RTSolution_InquireFile( Filename               , &
!                                                   n_Channels = n_Channels, &
!                                                   n_Profiles = n_Profiles  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       CRTM RTSolution data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Channels:     The number of spectral channels for which there is
!                       data in the file.
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

  FUNCTION CRTM_RTSolution_InquireFile( &
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_RTSolution_InquireFile'
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

  END FUNCTION CRTM_RTSolution_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_RTSolution_ReadFile
!
! PURPOSE:
!       Function to read CRTM RTSolution object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_RTSolution_ReadFile( Filename                , &
!                                                RTSolution              , &
!                                                Quiet      = Quiet      , &
!                                                n_Channels = n_Channels , &
!                                                n_Profiles = n_Profiles , &
!
! INPUTS:
!       Filename:     Character string specifying the name of an
!                     RTSolution format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       RTSolution:   CRTM RTSolution object array containing the RTSolution
!                     data.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
!                     DIMENSION:  Rank-2 (n_Channels x n_Profiles)
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
!       n_Channels:   The number of channels for which data was read.
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

  FUNCTION CRTM_RTSolution_ReadFile( &
    Filename   , &  ! Input
    RTSolution , &  ! Output
    Quiet      , &  ! Optional input
    n_Channels , &  ! Optional output
    n_Profiles , &  ! Optional output
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: Filename
    TYPE(CRTM_RTSolution_type), INTENT(OUT) :: RTSolution(:,:)
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER,          OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER,          OPTIONAL, INTENT(OUT) :: n_Profiles
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_RTSolution_ReadFile'
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
    n_Input_Channels = SIZE(RTSolution,DIM=1)
    IF ( n_File_Channels > n_Input_Channels ) THEN
      WRITE( msg,'("Number of channels, ",i0," > size of the output RTSolution", &
                  &" array dimension, ",i0,". Only the first ",i0, &
                  &" channels will be read.")' ) &
                  n_File_Channels, n_Input_Channels, n_Input_Channels
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING )
    END IF
    n_Input_Channels = MIN(n_Input_Channels, n_File_Channels)
    ! ...Check if n_Profiles in file is > size of output array
    n_Input_Profiles = SIZE(RTSolution,DIM=2)
    IF ( n_File_Profiles > n_Input_Profiles ) THEN
      WRITE( msg, '( "Number of profiles, ",i0," > size of the output RTSolution", &
                    &" array dimension, ",i0,". Only the first ",i0, &
                    &" profiles will be read.")' ) &
                    n_File_Profiles, n_Input_Profiles, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING )
    END IF
    n_Input_Profiles = MIN(n_Input_Profiles, n_File_Profiles)


    ! Loop over all the profiles and channels
    Profile_Loop: DO m = 1, n_Input_Profiles
      Channel_Loop: DO l = 1, n_Input_Channels
        err_stat = Read_Record( fid, RTSolution(l,m) )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error reading RTSolution element (",i0,",",i0,") from ",a)' ) &
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
      CALL CRTM_RTSolution_Destroy( RTSolution )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION CRTM_RTSolution_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_RTSolution_WriteFile
!
! PURPOSE:
!       Function to write CRTM RTSolution object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_RTSolution_WriteFile( Filename     , &
!                                                 RTSolution   , &
!                                                 Quiet = Quiet  )
!
! INPUTS:
!       Filename:     Character string specifying the name of the
!                     RTSolution format data file to write.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       RTSolution:   CRTM RTSolution object array containing the RTSolution
!                     data.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
!                     DIMENSION:  Rank-2 (n_Channels x n_Profiles)
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

  FUNCTION CRTM_RTSolution_WriteFile( &
    Filename   , &  ! Input
    RTSolution , &  ! Input
    Quiet      , &  ! Optional input
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN) :: Filename
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: RTSolution(:,:)
    LOGICAL,          OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,          OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_RTSolution_WriteFile'
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
    n_Output_Channels = SIZE(RTSolution,DIM=1)
    n_Output_Profiles = SIZE(RTSolution,DIM=2)


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
        err_stat = Write_Record( fid, RTSolution(l,m) )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error writing RTSolution element (",i0,",",i0,") to ",a)' ) &
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

  END FUNCTION CRTM_RTSolution_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_RTSolution_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_RTSolution_IOVersion( Id )
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

  SUBROUTINE CRTM_RTSolution_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_RTSolution_IOVersion



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
!       Utility function to read a single RTSolution data record
!
! CALLING SEQUENCE:
!       Error_Status = Read_Record( FileID    , &
!                                   RTSolution  )
!
! INPUTS:
!       FileID:       Logical unit number from which to read data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       RTSolution:   CRTM RTSolution structure containing the data read in.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the record read was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION Read_Record( &
    fid, &  ! Input
    rts) &  ! Output
  RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN)  :: fid
    TYPE(CRTM_RTSolution_type), INTENT(OUT) :: rts
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_RTSolution_ReadFile(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: n_Layers

    ! Set up
    err_stat = SUCCESS


    ! Read the dimensions
    READ( fid,IOSTAT=io_stat ) n_Layers
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'( "Error reading dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Allocate the RTSolution structure if necessary
    IF ( n_Layers > 0 ) THEN
      CALL CRTM_RTSolution_Create( rts, n_Layers )
      IF ( .NOT. CRTM_RTSolution_Associated( rts ) ) THEN
        msg = 'Error creating output object.'
        CALL Read_Record_Cleanup(); RETURN
      END IF
    END IF


    ! Read the forward radiative transfer intermediate results
    READ( fid,IOSTAT=io_stat ) rts%Surface_Emissivity     , &
                               rts%Up_Radiance            , &
                               rts%Down_Radiance          , &
                               rts%Down_Solar_Radiance    , &
                               rts%Surface_Planck_Radiance
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading scalar intermediate results. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF
    IF ( n_Layers > 0 ) THEN
      READ( fid,IOSTAT=io_stat ) rts%Upwelling_Radiance , &
                                 rts%Layer_Optical_Depth
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error reading array intermediate results. IOSTAT = ",i0)' ) io_stat
        CALL Read_Record_Cleanup(); RETURN
      END IF
    END IF


    ! Read the radiative transfer results
    READ( fid,IOSTAT=io_stat ) rts%Radiance              , &
                               rts%Brightness_Temperature
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading result data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Read_Record_Cleanup()
      CALL CRTM_RTSolution_Destroy( rts )
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
!       Function to write a single RTSolution data record
!
! CALLING SEQUENCE:
!       Error_Status = Write_Record( FileID    , &
!                                    RTSolution  )
!
! INPUTS:
!       FileID:       Logical unit number to which data is written
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       RTSolution:   CRTM RTSolution structure containing the data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
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

  FUNCTION Write_Record( &
    fid, &  ! Input
    rts) &  ! Input
  RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN) :: fid
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: rts
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_RTSolution_WriteFile(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
 
    ! Set up
    err_stat = SUCCESS


    ! Write the data dimensions
    WRITE( fid,IOSTAT=io_stat ) rts%n_Layers
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF

    
    ! Write the forward radiative transfer intermediate results
    WRITE( fid,IOSTAT=io_stat ) rts%Surface_Emissivity     , &
                                rts%Up_Radiance            , &
                                rts%Down_Radiance          , &
                                rts%Down_Solar_Radiance    , &
                                rts%Surface_Planck_Radiance
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing scalar intermediate results. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF
    IF ( rts%n_Layers > 0 ) THEN
      WRITE( fid,IOSTAT=io_stat ) rts%Upwelling_Radiance , &
                                  rts%Layer_Optical_Depth
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error writing array intermediate results. IOSTAT = ",i0)' ) io_stat
        CALL Write_Record_Cleanup(); RETURN
      END IF
    END IF


    ! Write the radiative transfer results
    WRITE( fid,IOSTAT=io_stat ) rts%Radiance              , &
                                rts%Brightness_Temperature
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing result data. IOSTAT = ",i0)' ) io_stat
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

END MODULE CRTM_RTSolution_IO
