!
! SpcCoeff_Binary_IO
!
! Module containing routines to read and write Binary format
! SpcCoeff data files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 18-Mar-2002
!                       paul.vandelst@noaa.gov
!

MODULE SpcCoeff_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds         , ONLY: Long, Double
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE SpcCoeff_Define    , ONLY: SpcCoeff_type        , &
                                 SpcCoeff_Associated  , &
                                 SpcCoeff_Destroy     , &
                                 SpcCoeff_Create      , &
                                 SpcCoeff_ValidRelease, &
                                 SpcCoeff_Info
  USE ACCoeff_Define     , ONLY: ACCoeff_Associated
  USE ACCoeff_Binary_IO  , ONLY: ACCoeff_Binary_ReadFile , &
                                 ACCoeff_Binary_WriteFile, &
                                 ACCoeff_Binary_IOVersion
  USE NLTECoeff_Define   , ONLY: NLTECoeff_Associated
  USE NLTECoeff_Binary_IO, ONLY: NLTECoeff_Binary_ReadFile , &
                                 NLTECoeff_Binary_WriteFile, &
                                 NLTECoeff_Binary_IOVersion
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: SpcCoeff_Binary_InquireFile
  PUBLIC :: SpcCoeff_Binary_ReadFile
  PUBLIC :: SpcCoeff_Binary_WriteFile
  PUBLIC :: SpcCoeff_Binary_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: SpcCoeff_Binary_IO.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Default message length
  INTEGER, PARAMETER :: ML = 512
  ! Ancillary data indicator
  INTEGER, PARAMETER :: DATA_MISSING = 0
  INTEGER, PARAMETER :: DATA_PRESENT = 1


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
!       SpcCoeff_Binary_InquireFile
!
! PURPOSE:
!       Function to inquire a Binary format SpcCoeff file.
!
! CALLING SEQUENCE:
!       Error_Status = SpcCoeff_Binary_InquireFile( &
!                        Filename                           , &
!                        n_Channels       = n_Channels      , &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Sensor_Id        = Sensor_Id       , &
!                        WMO_Satellite_Id = WMO_Satellite_Id, &
!                        WMO_Sensor_Id    = WMO_Sensor_Id     )
!
! INPUTS:
!       Filename:           Character string specifying the name of the binary
!                           SpcCoeff data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
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

  FUNCTION SpcCoeff_Binary_InquireFile( &
    Filename        , &  ! Input
    n_Channels      , &  ! Optional output  
    Release         , &  ! Optional Output
    Version         , &  ! Optional Output
    Sensor_Id       , &  ! Optional Output
    WMO_Satellite_Id, &  ! Optional Output
    WMO_Sensor_Id   ) &  ! Optional Output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SpcCoeff_InquireFile(Binary)'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(SpcCoeff_type) :: SpcCoeff

 
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
    READ( fid,IOSTAT=io_stat ) SpcCoeff%Release, SpcCoeff%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat ) &
      SpcCoeff%n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimension values from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the sensor ids
    READ( fid, IOSTAT=io_stat ) &
      SpcCoeff%Sensor_Id       , &
      SpcCoeff%Sensor_Type     , &
      SpcCoeff%WMO_Satellite_Id, &
      SpcCoeff%WMO_Sensor_Id    
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
    IF ( PRESENT(n_Channels      ) ) n_Channels       = SpcCoeff%n_Channels    
    IF ( PRESENT(Release         ) ) Release          = SpcCoeff%Release
    IF ( PRESENT(Version         ) ) Version          = SpcCoeff%Version
    IF ( PRESENT(Sensor_Id       ) ) Sensor_Id        = SpcCoeff%Sensor_Id
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = SpcCoeff%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = SpcCoeff%WMO_Sensor_Id   
    
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
    
  END FUNCTION SpcCoeff_Binary_InquireFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_Binary_ReadFile
!
! PURPOSE:
!       Function to read SpcCoeff object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = SpcCoeff_Binary_ReadFile( &
!                        Filename     , &
!                        SpcCoeff     , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       SpcCoeff format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SpcCoeff:       SpcCoeff object containing the spectral
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       SpcCoeff_type
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

  FUNCTION SpcCoeff_Binary_ReadFile( &
    Filename, &  ! Input
    SpcCoeff, &  ! Output
    Quiet   , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),        INTENT(IN)  :: Filename
    TYPE(SpcCoeff_type), INTENT(OUT) :: SpcCoeff
    LOGICAL,   OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,   OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SpcCoeff_ReadFile(Binary)'
    ! Function variables
    CHARACTER(ML) :: msg, io_msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER(Long) :: ac_present
    INTEGER(Long) :: nc_present
    TYPE(SpcCoeff_type) :: dummy
    

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
    READ( fid, IOSTAT=io_stat ) &
      dummy%Release, &
      dummy%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( .NOT. SpcCoeff_ValidRelease( dummy ) ) THEN
      msg = 'SpcCoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the spectral coefficient data
    ! ...Read the dimensions
    READ( fid, IOSTAT=io_stat ) dummy%n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the object
    CALL SpcCoeff_Create( SpcCoeff        , &
                          dummy%n_Channels  )                  
    IF ( .NOT. SpcCoeff_Associated( SpcCoeff ) ) THEN
      msg = 'SpcCoeff object allocation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the sensor info
    READ( fid, IOSTAT=io_stat ) &
      SpcCoeff%Sensor_Id       , &
      SpcCoeff%Sensor_Type     , &
      SpcCoeff%WMO_Satellite_Id, &
      SpcCoeff%WMO_Sensor_Id   
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading sensor ids. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the channel data
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      SpcCoeff%Sensor_Channel            , &
      SpcCoeff%Polarization              , &
      SpcCoeff%Channel_Flag              , &
      SpcCoeff%Frequency                 , &
      SpcCoeff%Wavenumber                , &
      SpcCoeff%Planck_C1                 , &
      SpcCoeff%Planck_C2                 , &
      SpcCoeff%Band_C1                   , &
      SpcCoeff%Band_C2                   , &
      SpcCoeff%Cosmic_Background_Radiance, &
      SpcCoeff%Solar_Irradiance
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading channel data. '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Explicitly assign the version number
    SpcCoeff%Version = dummy%Version
    
    
    ! Read the antenna correction data if it's present
    ! ...Read the data indicator
    READ( fid, IOSTAT=io_stat ) ac_present
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading antenna correction data indicator. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the antenna correction data
    IF ( ac_present == DATA_PRESENT ) THEN
      err_stat = ACCoeff_Binary_ReadFile( &
                   Filename         , &
                   SpcCoeff%AC      , &
                   No_Close = .TRUE., &
                   Quiet    = Quiet , &
                   Debug    = Debug   )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading antenna correction data.'
        CALL Read_Cleanup(); RETURN
      END IF
      ! ..Check that the ACCoeff data is for the same sensor
      IF ( SpcCoeff%Sensor_Id           /= SpcCoeff%AC%Sensor_Id        .OR. &
           SpcCoeff%WMO_Satellite_Id    /= SpcCoeff%AC%WMO_Satellite_Id .OR. &
           SpcCoeff%WMO_Sensor_Id       /= SpcCoeff%AC%WMO_Sensor_Id    .OR. &
           ANY( SpcCoeff%Sensor_Channel /= SpcCoeff%AC%Sensor_Channel ) ) THEN
        msg = 'Antenna correction sensor information is inconsistent with SpcCoeff'
        CALL Read_CleanUp(); RETURN
      END IF
    END IF
    
    
    ! Read the NLTE correction data if it's present
    ! ...Read the data indicator
    READ( fid, IOSTAT=io_stat ) nc_present
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading NLTE correction data indicator. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the NLTE correction data
    IF ( nc_present == DATA_PRESENT ) THEN
      err_stat = NLTECoeff_Binary_ReadFile( &
                   Filename         , &
                   SpcCoeff%NC      , &
                   No_Close = .TRUE., &
                   Quiet    = Quiet , &
                   Debug    = Debug   )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading NLTE correction data.'
        CALL Read_Cleanup(); RETURN
      END IF
      ! ..Check that the NLTECoeff data is for the same sensor
      IF ( SpcCoeff%Sensor_Id           /= SpcCoeff%NC%Sensor_Id        .OR. &
           SpcCoeff%WMO_Satellite_Id    /= SpcCoeff%NC%WMO_Satellite_Id .OR. &
           SpcCoeff%WMO_Sensor_Id       /= SpcCoeff%NC%WMO_Sensor_Id    .OR. &
           ANY( SpcCoeff%Sensor_Channel /= SpcCoeff%NC%Sensor_Channel ) ) THEN
        msg = 'non-LTE correction sensor information is inconsistent with SpcCoeff'
        CALL Read_CleanUp(); RETURN
      END IF
    END IF
    
    
    ! Close the file
    CLOSE( fid,IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF


    ! Output an info message
     IF ( noisy ) THEN
       CALL SpcCoeff_Info( SpcCoeff, msg, NoComponents = .TRUE. )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS
   
     SUBROUTINE Read_CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid,IOSTAT=io_stat )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing input file during error cleanup.'
       END IF
       CALL SpcCoeff_Destroy( SpcCoeff )
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Read_CleanUp

  END FUNCTION SpcCoeff_Binary_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_Binary_WriteFile
!
! PURPOSE:
!       Function to write SpcCoeff object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = SpcCoeff_Binary_WriteFile( &
!                        Filename     , &
!                        SpcCoeff     , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       SpcCoeff format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       SpcCoeff:       SpcCoeff object containing the spectral
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       SpcCoeff_type
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
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION SpcCoeff_Binary_WriteFile( &
    Filename, &  ! Input
    SpcCoeff, &  ! Output
    Quiet   , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),        INTENT(IN) :: Filename
    TYPE(SpcCoeff_type), INTENT(IN) :: SpcCoeff
    LOGICAL,   OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,   OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SpcCoeff_WriteFile(Binary)'
    CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: ac_present
    INTEGER :: nc_present
    

    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF
    ! ...Check there is data to write
    IF ( .NOT. SpcCoeff_Associated( SpcCoeff ) ) THEN
      msg = 'SpcCoeff object is empty.'
      CALL Write_Cleanup(); RETURN
    END IF

   
    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, For_Output=.TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_CleanUp(); RETURN
    END IF


    ! Write the release and version
    WRITE( fid,IOSTAT=io_stat ) &
      SpcCoeff%Release, &
      SpcCoeff%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the spectral coefficient data
    ! ...Write the dimensions
    WRITE( fid, IOSTAT=io_stat ) SpcCoeff%n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the sensor info
    WRITE( fid, IOSTAT=io_stat ) &
      SpcCoeff%Sensor_Id       , &
      SpcCoeff%Sensor_Type     , &
      SpcCoeff%WMO_Satellite_Id, &
      SpcCoeff%WMO_Sensor_Id   
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing sensor ids. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the channel data
    WRITE( fid, IOSTAT=io_stat ) &
      SpcCoeff%Sensor_Channel            , &
      SpcCoeff%Polarization              , &
      SpcCoeff%Channel_Flag              , &
      SpcCoeff%Frequency                 , &
      SpcCoeff%Wavenumber                , &
      SpcCoeff%Planck_C1                 , &
      SpcCoeff%Planck_C2                 , &
      SpcCoeff%Band_C1                   , &
      SpcCoeff%Band_C2                   , &
      SpcCoeff%Cosmic_Background_Radiance, &
      SpcCoeff%Solar_Irradiance             
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing channel data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the antenna correction data if it's present
    IF ( ACCoeff_Associated( SpcCoeff%AC ) ) THEN
      ac_present = DATA_PRESENT
    ELSE
      ac_present = DATA_MISSING
    END IF
    ! ...Write the data indicator
    WRITE( fid, IOSTAT=io_stat ) ac_present
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing antenna correction data indicator. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the antenna correction data
    IF ( ac_present == DATA_PRESENT ) THEN
      err_stat = ACCoeff_Binary_WriteFile( &
                   Filename         , &
                   SpcCoeff%AC      , &
                   No_Close = .TRUE., &
                   Quiet    = Quiet , &
                   Debug    = Debug   )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing antenna correction data.'
        CALL Write_Cleanup(); RETURN
      END IF
      ! ..Check that the ACCoeff data is for the same sensor
      IF ( SpcCoeff%Sensor_Id           /= SpcCoeff%AC%Sensor_Id        .OR. &
           SpcCoeff%WMO_Satellite_Id    /= SpcCoeff%AC%WMO_Satellite_Id .OR. &
           SpcCoeff%WMO_Sensor_Id       /= SpcCoeff%AC%WMO_Sensor_Id    .OR. &
           ANY( SpcCoeff%Sensor_Channel /= SpcCoeff%AC%Sensor_Channel ) ) THEN
        msg = 'Antenna correction sensor information is inconsistent with SpcCoeff'
        CALL Write_CleanUp(); RETURN
      END IF
    END IF
    
    
    ! Write the NLTE correction data if it's present
    IF ( NLTECoeff_Associated( SpcCoeff%NC ) ) THEN
      nc_present = DATA_PRESENT
    ELSE
      nc_present = DATA_MISSING
    END IF
    ! ...Write the data indicator
    WRITE( fid, IOSTAT=io_stat ) nc_present
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing NLTE correction data indicator. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the NLTE correction data
    IF ( nc_present == DATA_PRESENT ) THEN
      err_stat = NLTECoeff_Binary_WriteFile( &
                   Filename         , &
                   SpcCoeff%NC      , &
                   No_Close = .TRUE., &
                   Quiet    = Quiet , &
                   Debug    = Debug   )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing NLTE correction data.'
        CALL Write_Cleanup(); RETURN
      END IF
      ! ..Check that the NLTECoeff data is for the same sensor
      IF ( SpcCoeff%Sensor_Id           /= SpcCoeff%NC%Sensor_Id        .OR. &
           SpcCoeff%WMO_Satellite_Id    /= SpcCoeff%NC%WMO_Satellite_Id .OR. &
           SpcCoeff%WMO_Sensor_Id       /= SpcCoeff%NC%WMO_Sensor_Id    .OR. &
           ANY( SpcCoeff%Sensor_Channel /= SpcCoeff%NC%Sensor_Channel ) ) THEN
        msg = 'non-LTE correction sensor information is inconsistent with SpcCoeff'
        CALL Write_CleanUp(); RETURN
      END IF
    END IF
    
    
    ! Close the file
    CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
     IF ( noisy ) THEN
       CALL SpcCoeff_Info( SpcCoeff, msg, NoComponents = .TRUE. )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS
   
     SUBROUTINE Write_CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, STATUS=WRITE_ERROR_STATUS, IOSTAT=io_stat )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing input file during error cleanup.'
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Write_CleanUp

  END FUNCTION SpcCoeff_Binary_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_Binary_IOVersion
!
! PURPOSE:
!       Subroutine to return the version information for the
!       I/O module(s).
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_IOVersion( Id )
!
! OUTPUTS:
!       Id:     Character string containing the version Id information for the
!               structure I/O module(s). If the string length is sufficient,
!               the version information for all the modules (this, and those
!               for the derived type components) are concatenated. Otherwise
!               only the version id for this module is returned.
!               UNITS:      N/A
!               TYPE:       CHARACTER(*)
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE SpcCoeff_Binary_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    INTEGER, PARAMETER :: SL = 256
    CHARACTER(SL)   :: AC_Id
    CHARACTER(SL)   :: NC_Id
    CHARACTER(SL*3) :: IO_Id
    CALL ACCoeff_Binary_IOVersion( AC_Id )
    CALL NLTECoeff_Binary_IOVersion( NC_Id )
    IO_Id = MODULE_VERSION_ID//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
            '    '//TRIM(AC_Id)//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
            '    '//TRIM(NC_Id)
    IF ( LEN_TRIM(IO_Id) <= LEN(Id) ) THEN
      Id = IO_Id
    ELSE
      Id = MODULE_VERSION_ID
    END IF
  END SUBROUTINE SpcCoeff_Binary_IOVersion

END MODULE SpcCoeff_Binary_IO
