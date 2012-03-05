!
! CRTM_LifeCycle
!
! Module containing CRTM life cycle functions to initialize and destroy
! the CRTM space.
!
! Written by:     Paul van Delst, 21-May-2004
!                 paul.vandelst@noaa.gov
!

MODULE CRTM_LifeCycle

  
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler
  USE CRTM_SpcCoeff
  USE CRTM_TauCoeff
  USE CRTM_AerosolCoeff      , ONLY: CRTM_AerosolCoeff_Load, &
                                     CRTM_AerosolCoeff_Destroy
  USE CRTM_CloudCoeff        , ONLY: CRTM_CloudCoeff_Load, &
                                     CRTM_CloudCoeff_Destroy
  USE CRTM_EmisCoeff
  USE CRTM_ChannelInfo_Define, ONLY: CRTM_ChannelInfo_type, &
                                     CRTM_ChannelInfo_Associated, &
                                     CRTM_ChannelInfo_Destroy, &
                                     CRTM_ChannelInfo_Create
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public procedures
  PUBLIC :: CRTM_Init
  PUBLIC :: CRTM_Destroy
  PUBLIC :: CRTM_LifeCycleVersion

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_LifeCycle.f90 7825 2010-05-12 19:20:11Z david.groff@noaa.gov $'
  ! String lengths
  INTEGER, PARAMETER :: ML = 256   ! Error message length
  INTEGER, PARAMETER :: SL = 2000  ! Maximum length for path+filenames


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Init
!
! PURPOSE:
!       Function to initialise the CRTM.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Init( Sensor_ID                            , &
!                                 ChannelInfo                          , &
!                                 CloudCoeff_File   = CloudCoeff_File  , &
!                                 AerosolCoeff_File = AerosolCoeff_File, &
!                                 EmisCoeff_File    = EmisCoeff_File   , &
!                                 File_Path         = File_Path        , &
!                                 Load_CloudCoeff   = Load_CloudCoeff  , &
!                                 Load_AerosolCoeff = Load_AerosolCoeff, &
!                                 Quiet             = Quiet            , &
!                                 Process_ID        = Process_ID       , &
!                                 Output_Process_ID = Output_Process_ID  )
!
! INPUTS:
!       Sensor_ID:          List of the sensor IDs (e.g. hirs3_n17, amsua_n18,
!                           ssmis_f16, etc) with which the CRTM is to be
!                           initialised. These sensor ids are used to construct
!                           the sensor specific SpcCoeff and TauCoeff filenames
!                           containing the necessary coefficient data, i.e.
!                             <Sensor_ID>.SpcCoeff.bin
!                           and
!                             <Sensor_ID>.TauCoeff.bin
!                           for each sensor Id in the list.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Rank-1 (n_Sensors)
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUTS:
!       ChannelInfo:        ChannelInfo structure array populated based on
!                           the contents of the coefficient files and the
!                           user inputs.
!                           UNITS:      N/A
!                           TYPE:       CRTM_ChannelInfo_type
!                           DIMENSION:  Same as input Sensor_Id argument
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       CloudCoeff_File:    Name of the CRTM Binary format CloudCoeff file
!                           containing the scattering coefficient data. If not
!                           specified the default filename is "CloudCoeff.bin".
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       AerosolCoeff_File:  Name of the CRTM Binary format AerosolCoeff file
!                           containing the aerosol absorption and scattering
!                           coefficient data. If not specified the default
!                           filename is "AerosolCoeff.bin".
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       EmisCoeff_File:     Name of the CRTM Binary format EmisCoeff file
!                           containing the IRSSEM coefficient data. If not
!                           specified the default filename is "EmisCoeff.bin".
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       File_Path:          Character string specifying a file path for the
!                           input data files. If not specified, the current
!                           directory is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Load_CloudCoeff:    Set this logical argument for not loading the CloudCoeff data
!                           to save memory space under the clear conditions 
!                           If == .FALSE., the CloudCoeff data will not be loaded;
!                              == .TRUE.,  the CloudCoeff data will be loaded.
!                           If not specified, default is .TRUE. (will be loaded)
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Load_AerosolCoeff:  Set this logical argument for not loading the AerosolCoeff data
!                           to save memory space under the clear conditions 
!                           If == .FALSE., the AerosolCoeff data will not be loaded;
!                              == .TRUE.,  the AerosolCoeff data will be loaded.
!                           If not specified, default is .TRUE. (will be loaded)
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:              Set this logical argument to suppress INFORMATION
!                           messages being printed to stdout
!                           If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                              == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                           If not specified, default is .FALSE.
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATION message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Output_Process_ID:  Set this argument to the MPI process ID in which
!                           all INFORMATION messages are to be output. If
!                           the passed Process_ID value agrees with this value
!                           the INFORMATION messages are output. 
!                           This argument is ignored if the Quiet argument
!                           is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the CRTM initialisation was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       All public data arrays accessed by this module and its dependencies
!       are overwritten.
!
! RESTRICTIONS:
!       If specified, the length of the combined file path and filename strings
!       cannot exceed 2000 characters.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Init( &
    Sensor_ID        , &  ! Input
    ChannelInfo      , &  ! Output
    CloudCoeff_File  , &  ! Optional input
    AerosolCoeff_File, &  ! Optional input
    EmisCoeff_File   , &  ! Optional input
    File_Path        , &  ! Optional input
    Load_CloudCoeff  , &  ! Optional input
    Load_AerosolCoeff, &  ! Optional input
    Quiet            , &  ! Optional input
    Process_ID       , &  ! Optional input
    Output_Process_ID) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*)               , INTENT(IN)  :: Sensor_ID(:)
    TYPE(CRTM_ChannelInfo_type), INTENT(OUT) :: ChannelInfo(:)
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: CloudCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: AerosolCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: EmisCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: File_Path
    LOGICAL     ,      OPTIONAL, INTENT(IN)  :: Load_CloudCoeff
    LOGICAL     ,      OPTIONAL, INTENT(IN)  :: Load_AerosolCoeff
    LOGICAL     ,      OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER     ,      OPTIONAL, INTENT(IN)  :: Process_ID
    INTEGER     ,      OPTIONAL, INTENT(IN)  :: Output_Process_ID
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Init'
    ! Local variables
    CHARACTER(ML) :: msg, pid_msg
    CHARACTER(SL) :: Default_CloudCoeff_File
    CHARACTER(SL) :: Default_AerosolCoeff_File
    CHARACTER(SL) :: Default_EmisCoeff_File
    INTEGER :: l, n, n_Sensors
    LOGICAL :: Local_Load_CloudCoeff
    LOGICAL :: Local_Load_AerosolCoeff
    
    ! ******
    ! TEMPORARY UNTIL LOAD ROUTINE INTERFACES HAVE BEEN MODIFIED
    INTEGER :: iQuiet
    iQuiet = 0
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet ) THEN
        iQuiet = 1  ! Set
      END IF
    END IF
    ! ******

    ! Set up
    err_stat = SUCCESS
    ! ...Create a process ID message tag for error messages
    IF ( PRESENT(Process_Id) ) THEN
      WRITE( pid_msg,'("; Process ID: ",i0)' ) Process_ID
    ELSE
      pid_msg = ''
    END IF
    ! ...Check coefficient loading flags
    Local_Load_CloudCoeff = .TRUE.
    IF( PRESENT(Load_CloudCoeff) ) Local_Load_CloudCoeff = Load_CloudCoeff
    Local_Load_AerosolCoeff = .TRUE.
    IF( PRESENT(Load_AerosolCoeff) ) Local_Load_AerosolCoeff = Load_AerosolCoeff
    ! ...Check dimensionality
    n_Sensors = SIZE(Sensor_ID)
    IF ( SIZE(ChannelInfo) /= n_Sensors ) THEN
      err_stat = FAILURE
      msg = 'Inconsistent Sensor_ID and ChannelInfo dimensions'
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
      RETURN
    END IF

    ! Specify sensor-independent coefficient filenames
    ! ...Default filenames
    Default_CloudCoeff_File   = 'CloudCoeff.bin'
    Default_AerosolCoeff_File = 'AerosolCoeff.bin'
    Default_EmisCoeff_File    = 'EmisCoeff.bin'
    ! ...Were other filenames specified?
    IF ( PRESENT(CloudCoeff_File  ) ) Default_CloudCoeff_File   = TRIM(ADJUSTL(CloudCoeff_File))
    IF ( PRESENT(AerosolCoeff_File) ) Default_AerosolCoeff_File = TRIM(ADJUSTL(AerosolCoeff_File))
    IF ( PRESENT(EmisCoeff_File   ) ) Default_EmisCoeff_File    = TRIM(ADJUSTL(EmisCoeff_File))
    ! ...Was a path specified?
    IF ( PRESENT(File_Path) ) THEN
      Default_CloudCoeff_File   = TRIM(ADJUSTL(File_Path)) // TRIM(Default_CloudCoeff_File)
      Default_AerosolCoeff_File = TRIM(ADJUSTL(File_Path)) // TRIM(Default_AerosolCoeff_File)
      Default_EmisCoeff_File    = TRIM(ADJUSTL(File_Path)) // TRIM(Default_EmisCoeff_File)
    END IF


    ! Load the spectral coefficients
    err_stat = CRTM_Load_SpcCoeff( &
                 Sensor_ID         = Sensor_ID        , &
                 File_Path         = File_Path        , &
                 Quiet             = iQuiet           , &  ! *** Use of iQuiet temporary
                 Process_ID        = Process_ID       , &
                 Output_Process_ID = Output_Process_ID  )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,'Error loading SpcCoeff data'//TRIM(pid_msg),err_stat )
      RETURN
    END IF


    ! Load the transmittance model coefficients
    err_stat = CRTM_Load_TauCoeff( &
                 Sensor_ID         = Sensor_ID        , &
                 File_Path         = File_Path        , &
                 Quiet             = iQuiet           , &  ! *** Use of iQuiet temporary
                 Process_ID        = Process_ID       , &
                 Output_Process_ID = Output_Process_ID  )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,'Error loading TauCoeff data'//TRIM(pid_msg),err_stat )
      RETURN
    END IF

    ! Load the cloud coefficients
    IF ( Local_Load_CloudCoeff ) THEN
      err_stat = CRTM_CloudCoeff_Load( &
                   Default_CloudCoeff_File, &
                   Quiet             = Quiet            , &
                   Process_ID        = Process_ID       , &
                   Output_Process_ID = Output_Process_ID  )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error loading CloudCoeff data from '//TRIM(Default_CloudCoeff_File)
        CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
        RETURN
      END IF
    END IF


    ! Load the aerosol coefficients
    IF ( Local_Load_AerosolCoeff ) THEN
      err_stat = CRTM_AerosolCoeff_Load( &
                   Default_AerosolCoeff_File, &
                   Quiet             = Quiet            , &
                   Process_ID        = Process_ID       , &
                   Output_Process_ID = Output_Process_ID  )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error loading AerosolCoeff data from '//TRIM(Default_AerosolCoeff_File)
        CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
        RETURN
      END IF
    END IF


    ! Load the emissivity model coefficients
    ! ...IR Water
    err_stat = CRTM_Load_EmisCoeff( &
                 Default_EmisCoeff_File, &
                 Quiet             = iQuiet           , &  ! *** Use of iQuiet temporary
                 Process_ID        = Process_ID       , &
                 Output_Process_ID = Output_Process_ID  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error loading IR Water EmisCoeff data from '//TRIM(Default_EmisCoeff_File)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
      RETURN
    END IF
    
    
    ! Load the ChannelInfo structure
    ! **** THIS CODE ASSUMES USING ALL CHANNELS ****
    DO n = 1, n_Sensors
      ! ...Allocate the ChannelInfo structure
      CALL CRTM_ChannelInfo_Create( ChannelInfo(n), SC(n)%n_Channels )
      IF ( .NOT. CRTM_ChannelInfo_Associated(ChannelInfo(n)) ) THEN
        msg = 'ChannelInfo allocation failed for '//TRIM(Sensor_Id(n))//' sensor'
        CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
        RETURN
      END IF
      ! ...Set the Sensor_Index component
      ChannelInfo(n)%Sensor_Index = n
      ! ...Fill the Channel_Index component
      ! **** THIS IS WHERE CHANNEL SELECTION COULD OCCUR ****
      ChannelInfo(n)%Channel_Index = (/(l, l=1,SC(n)%n_Channels)/)
      ! ...Fill the rest of the ChannelInfo structure
      ChannelInfo(n)%Sensor_ID        = SC(n)%Sensor_Id
      ChannelInfo(n)%WMO_Satellite_ID = SC(n)%WMO_Satellite_ID
      ChannelInfo(n)%WMO_Sensor_ID    = SC(n)%WMO_Sensor_ID
      ChannelInfo(n)%Sensor_Channel   = SC(n)%Sensor_Channel
    END DO
    
  END FUNCTION CRTM_Init


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Destroy
!
! PURPOSE:
!       Function to deallocate all the shared data arrays allocated and
!       populated during the CRTM initialization.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy( ChannelInfo            , &
!                                    Process_ID = Process_ID  )
!
! OUTPUTS:
!       ChannelInfo:  Reinitialized ChannelInfo structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_ChannelInfo_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Process_ID:   Set this argument to the MPI process ID that this
!                     function call is running under. This value is used
!                     solely for controlling message output. If MPI is not
!                     being used, ignore this argument.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error
!                     status. The error codes are defined in the
!                     Message_Handler module.
!                     If == SUCCESS the CRTM deallocations were successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       All CRTM shared data arrays and structures are deallocated.
!
! COMMENTS:
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy( &
    ChannelInfo, &  ! Output
    Process_ID ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo(:)
    INTEGER,           OPTIONAL, INTENT(IN)     :: Process_ID
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy'
    ! Local variables
    CHARACTER(ML) :: msg, pid_msg
    INTEGER :: Destroy_Status

    ! Set up
    err_stat = SUCCESS
    ! ...Create a process ID message tag for error messages
    IF ( PRESENT(Process_Id) ) THEN
      WRITE( pid_msg,'("; Process ID: ",i0)' ) Process_ID
    ELSE
      pid_msg = ''
    END IF


    ! Destroy all the ChannelInfo structures
    CALL CRTM_ChannelInfo_Destroy( ChannelInfo )
    IF ( ANY(CRTM_ChannelInfo_Associated(ChannelInfo)) ) THEN
      err_stat = FAILURE
      msg = 'Error deallocating ChannelInfo structure(s)'
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF


    ! Destroy the shared data structure
    Destroy_Status = CRTM_Destroy_EmisCoeff( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared EmisCoeff data structure'//TRIM(pid_msg), &
                            err_stat )
    END IF
    Destroy_Status = CRTM_AerosolCoeff_Destroy( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared AerosolCoeff data structure'//TRIM(pid_msg), &
                            err_stat )
    END IF
    Destroy_Status = CRTM_CloudCoeff_Destroy( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared CloudCoeff data structure'//TRIM(pid_msg), &
                            err_stat )
    END IF
    Destroy_Status = CRTM_Destroy_TauCoeff( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared TauCoeff data structure(s)'//TRIM(pid_msg), &
                            err_stat )
    END IF
    Destroy_Status = CRTM_Destroy_SpcCoeff( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared SpcCoeff data structure(s)'//TRIM(pid_msg), &
                            err_stat )
    END IF

  END FUNCTION CRTM_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_LifeCycleVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_LifeCycleVersion( Id )
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

  SUBROUTINE CRTM_LifeCycleVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_LifeCycleVersion

END MODULE CRTM_LifeCycle
