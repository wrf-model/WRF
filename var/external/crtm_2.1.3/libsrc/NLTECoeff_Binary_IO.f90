!
! NLTECoeff_Binary_IO
!
! Module containing routines to inquire, read, and write Binary
! NLTECoeff object datafiles.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, 08-05-2010
!                       yong.han@noaa.gov
!
!       Refactored by:  Paul van Delst, 20-Jan-2011
!                       paul.vandelst@noaa.gov
!

MODULE NLTECoeff_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds         , ONLY: Long, Double
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE NLTECoeff_Define   , ONLY: NLTECoeff_type        , &
                                 NLTECoeff_Associated  , &
                                 NLTECoeff_Destroy     , &
                                 NLTECoeff_Create      , &
                                 NLTECoeff_ValidRelease, &
                                 NLTECoeff_Info
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: NLTECoeff_Binary_InquireFile
  PUBLIC :: NLTECoeff_Binary_ReadFile
  PUBLIC :: NLTECoeff_Binary_WriteFile
  PUBLIC :: NLTECoeff_Binary_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
    '$Id: NLTECoeff_Binary_IO.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
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
!       NLTECoeff_Binary_InquireFile
!
! PURPOSE:
!       Function to inquire a Binary format NLTE file.
!
! CALLING SEQUENCE:
!       Error_Status = NLTECoeff_Binary_InquireFile( &
!                        Filename                           , &
!                        n_Predictors     = n_Predictors    , &
!                        n_Sensor_Angles  = n_Sensor_Angles , &
!                        n_Solar_Angles   = n_Solar_Angles  , &
!                        n_NLTE_Channels  = n_NLTE_Channels , &
!                        n_Channels       = n_Channels      , &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Sensor_Id        = Sensor_Id       , &
!                        WMO_Satellite_Id = WMO_Satellite_Id, &
!                        WMO_Sensor_Id    = WMO_Sensor_Id     )
!
! INPUTS:
!       Filename:           Character string specifying the name of the binary
!                           NLTE data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Predictors:       The number of predictor functions used in generating
!                           the NLTE correction coefficients.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Sensor_Angles:    Number of sensor zenith angles.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Solar_Angles:     Number of solar zenith angles.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_NLTE_Channels:    Number of NLTE channels for the sensor.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:         Total number of sensor channels.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
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

  FUNCTION NLTECoeff_Binary_InquireFile( &
    Filename        , &  ! Input
    n_Predictors    , &  ! Optional output
    n_Sensor_Angles , &  ! Optional output
    n_Solar_Angles  , &  ! Optional output
    n_NLTE_Channels , &  ! Optional output  
    n_Channels      , &  ! Optional output  
    Release         , &  ! Optional Output
    Version         , &  ! Optional Output
    Sensor_Id       , &  ! Optional Output
    WMO_Satellite_Id, &  ! Optional Output
    WMO_Sensor_Id   ) &  ! Optional Output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Predictors
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Sensor_Angles
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Solar_Angles
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_NLTE_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTECoeff_Binary_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(NLTECoeff_type) :: NLTECoeff

 
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
    READ( fid,IOSTAT=io_stat ) NLTECoeff%Release, NLTECoeff%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat ) &
      NLTECoeff%n_Predictors    , &
      NLTECoeff%n_Sensor_Angles , &
      NLTECoeff%n_Solar_Angles  , &
      NLTECoeff%n_NLTE_Channels , &
      NLTECoeff%n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimension values from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the sensor ids
    READ( fid, IOSTAT=io_stat ) &
      NLTECoeff%Sensor_Id       , &
      NLTECoeff%WMO_Satellite_Id, &
      NLTECoeff%WMO_Sensor_Id    
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
    IF ( PRESENT(n_Predictors    ) ) n_Predictors     = NLTECoeff%n_Predictors
    IF ( PRESENT(n_Sensor_Angles ) ) n_Sensor_Angles  = NLTECoeff%n_Sensor_Angles 
    IF ( PRESENT(n_Solar_Angles  ) ) n_Solar_Angles   = NLTECoeff%n_Solar_Angles
    IF ( PRESENT(n_NLTE_Channels ) ) n_NLTE_Channels  = NLTECoeff%n_NLTE_Channels    
    IF ( PRESENT(n_Channels      ) ) n_Channels       = NLTECoeff%n_Channels    
    IF ( PRESENT(Release         ) ) Release          = NLTECoeff%Release
    IF ( PRESENT(Version         ) ) Version          = NLTECoeff%Version
    IF ( PRESENT(Sensor_Id       ) ) Sensor_Id        = TRIM(NLTECoeff%Sensor_Id)
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = NLTECoeff%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = NLTECoeff%WMO_Sensor_Id   
    
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
    
  END FUNCTION NLTECoeff_Binary_InquireFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_Binary_ReadFile
!
! PURPOSE:
!       Function to read NLTECoeff object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = NLTECoeff_Binary_ReadFile( &
!                        Filename           , &
!                        NLTECoeff          , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       NLTECoeff format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       NLTECoeff:      NLTECoeff object containing the NLTE correction
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       NLTECoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the NLTECoeff data is embedded within another file
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

  FUNCTION NLTECoeff_Binary_ReadFile( &
    Filename , &  ! Input
    NLTECoeff, &  ! Output
    No_Close , &  ! Optional input
    Quiet    , &  ! Optional input
    Debug    ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),         INTENT(IN)  :: Filename
    TYPE(NLTECoeff_type), INTENT(OUT) :: NLTECoeff
    LOGICAL,    OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL,    OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,    OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTECoeff_ReadFile(Binary)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(NLTECoeff_type) :: dummy
    

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
    IF ( File_Open( FileName ) ) THEN
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
    IF ( .NOT. NLTECoeff_ValidRelease( dummy ) ) THEN
      msg = 'NLTECoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the NLTE correction coefficient data
    ! ...Read the dimensions
    READ( fid, IOSTAT=io_stat ) dummy%n_Predictors    , &
                                dummy%n_Sensor_Angles , &
                                dummy%n_Solar_Angles  , &
                                dummy%n_NLTE_Channels , &
                                dummy%n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the object
    CALL NLTECoeff_Create( NLTECoeff             , &
                           dummy%n_Predictors    , &        
                           dummy%n_Sensor_Angles , &          
                           dummy%n_Solar_Angles  , &          
                           dummy%n_NLTE_Channels , &          
                           dummy%n_Channels        )                  
    IF ( .NOT. NLTECoeff_Associated( NLTECoeff ) ) THEN
      msg = 'NLTECoeff object allocation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the sensor info
    READ( fid, IOSTAT=io_stat ) NLTECoeff%Sensor_Id       , &
                                NLTECoeff%WMO_Satellite_Id, &
                                NLTECoeff%WMO_Sensor_Id   , &
                                NLTECoeff%Sensor_Channel
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading sensor information. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the upper and lower pressure levels
    READ( fid, IOSTAT=io_stat ) NLTECoeff%Upper_Plevel, &
                                NLTECoeff%Lower_Plevel
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading pressure level data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the min/max/mean layer temperatures
    READ( fid, IOSTAT=io_stat ) NLTECoeff%Min_Tm , &
                                NLTECoeff%Max_Tm , &
                                NLTECoeff%Mean_Tm
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading min/max/mean temperature data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the NLTE channel numbers
    READ( fid, IOSTAT=io_stat ) NLTECoeff%NLTE_Channel                                     
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading NLTE channel numbers. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read sensor and solar angle values
    READ( fid, IOSTAT=io_stat ) NLTECoeff%Secant_Sensor_Zenith, &
                                NLTECoeff%Secant_Solar_Zenith
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading sensor and solar angle data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the NLTE correction coefficients
    READ( fid, IOSTAT=io_stat ) NLTECoeff%C_Index, NLTECoeff%C
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading NLTE correction coefficients. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    

    ! Explicitly assign the version number
    NLTECoeff%Version = dummy%Version
    
    
    ! Set the logical flag based on the C_Index values
    WHERE( NLTECoeff%C_Index > 0 ) NLTECoeff%Is_NLTE_Channel = .TRUE.
    

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
       CALL NLTECoeff_Info( NLTECoeff, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS
   
     SUBROUTINE Read_CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid,IOSTAT=io_stat )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing input file during error cleanup.'
       END IF
       CALL NLTECoeff_Destroy( NLTECoeff )
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Read_CleanUp

  END FUNCTION NLTECoeff_Binary_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_Binary_WriteFile
!
! PURPOSE:
!       Function to write NLTECoeff object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = NLTECoeff_Binary_WriteFile( &
!                        Filename           , &
!                        NLTECoeff          , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       NLTECoeff format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       NLTECoeff:      NLTECoeff object containing the NLTE correction
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       NLTECoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the NLTECoeff data is to be embedded within another file
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

  FUNCTION NLTECoeff_Binary_WriteFile( &
    Filename , &  ! Input
    NLTECoeff, &  ! Input
    No_Close , &  ! Optional input
    Quiet    , &  ! Optional input
    Debug    ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),         INTENT(IN) :: Filename
    TYPE(NLTECoeff_type), INTENT(IN) :: NLTECoeff
    LOGICAL,    OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL,    OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,    OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTECoeff_WriteFile(Binary)'
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
    IF ( .NOT. NLTECoeff_Associated( NLTECoeff ) ) THEN
      msg = 'NLTECoeff object is empty.'
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
    WRITE( fid,IOSTAT=io_stat ) NLTECoeff%Release, NLTECoeff%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the NLTE correction coefficient data
    ! ...Write the dimensions
    WRITE( fid, IOSTAT=io_stat ) NLTECoeff%n_Predictors    , &
                                 NLTECoeff%n_Sensor_Angles , &
                                 NLTECoeff%n_Solar_Angles  , &
                                 NLTECoeff%n_NLTE_Channels , &
                                 NLTECoeff%n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the sensor info
    WRITE( fid, IOSTAT=io_stat ) NLTECoeff%Sensor_Id       , &
                                 NLTECoeff%WMO_Satellite_Id, &
                                 NLTECoeff%WMO_Sensor_Id   , &
                                 NLTECoeff%Sensor_Channel
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing sensor information. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the upper and lower pressure levels
    WRITE( fid, IOSTAT=io_stat ) NLTECoeff%Upper_Plevel, &
                                 NLTECoeff%Lower_Plevel
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing pressure level data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the min/max/mean layer temperatures
    WRITE( fid, IOSTAT=io_stat ) NLTECoeff%Min_Tm , &
                                 NLTECoeff%Max_Tm , &
                                 NLTECoeff%Mean_Tm
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing min/max/mean temperature data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the NLTE channel numbers
    WRITE( fid, IOSTAT=io_stat ) NLTECoeff%NLTE_Channel                                     
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing NLTE channel numbers. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write sensor and solar angle values
    WRITE( fid, IOSTAT=io_stat ) NLTECoeff%Secant_Sensor_Zenith, &
                                 NLTECoeff%Secant_Solar_Zenith

    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing sensor and solar angle data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the NLTE correction coefficients
    WRITE( fid, IOSTAT=io_stat ) NLTECoeff%C_Index, NLTECoeff%C
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing NLTE correction coefficients. IOSTAT = ",i0)' ) io_stat
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
       CALL NLTECoeff_Info( NLTECoeff, msg )
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

  END FUNCTION NLTECoeff_Binary_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_Binary_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_Binary_IOVersion( Id )
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

  SUBROUTINE NLTECoeff_Binary_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE NLTECoeff_Binary_IOVersion
  
END MODULE NLTECoeff_Binary_IO
