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
  USE CRTM_ChannelInfo_Define, ONLY: CRTM_ChannelInfo_type, &
                                     CRTM_ChannelInfo_Associated, &
                                     CRTM_ChannelInfo_Destroy, &
                                     CRTM_ChannelInfo_Create
  ! ...Spectral coefficients
  USE CRTM_SpcCoeff          , ONLY: SC, &
                                     CRTM_SpcCoeff_Load, &
                                     CRTM_SpcCoeff_Destroy
  ! ...Transmittance model coefficients
  USE CRTM_TauCoeff
  ! ...Aerosol optical properties
  USE CRTM_AerosolCoeff      , ONLY: CRTM_AerosolCoeff_Load, &
                                     CRTM_AerosolCoeff_Destroy
  ! ...Cloud optical properties
  USE CRTM_CloudCoeff        , ONLY: CRTM_CloudCoeff_Load, &
                                     CRTM_CloudCoeff_Destroy
  ! ...Infrared surface emissivities
  USE CRTM_IRwaterCoeff      , ONLY: CRTM_IRwaterCoeff_Load, &
                                     CRTM_IRwaterCoeff_Destroy
  USE CRTM_IRlandCoeff       , ONLY: CRTM_IRlandCoeff_Load, &
                                     CRTM_IRlandCoeff_Destroy
  USE CRTM_IRsnowCoeff       , ONLY: CRTM_IRsnowCoeff_Load, &
                                     CRTM_IRsnowCoeff_Destroy
  USE CRTM_IRiceCoeff        , ONLY: CRTM_IRiceCoeff_Load, &
                                     CRTM_IRiceCoeff_Destroy
  ! ...Visible surface emissivities
  USE CRTM_VISwaterCoeff     , ONLY: CRTM_VISwaterCoeff_Load, &
                                     CRTM_VISwaterCoeff_Destroy
  USE CRTM_VISlandCoeff      , ONLY: CRTM_VISlandCoeff_Load, &
                                     CRTM_VISlandCoeff_Destroy
  USE CRTM_VISsnowCoeff      , ONLY: CRTM_VISsnowCoeff_Load, &
                                     CRTM_VISsnowCoeff_Destroy
  USE CRTM_VISiceCoeff       , ONLY: CRTM_VISiceCoeff_Load, &
                                     CRTM_VISiceCoeff_Destroy
  ! ...Microwave surface emissivities
  USE CRTM_MWwaterCoeff      , ONLY: CRTM_MWwaterCoeff_Load, &
                                     CRTM_MWwaterCoeff_Destroy
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
  PUBLIC :: CRTM_IsInitialized
  PUBLIC :: CRTM_LifeCycleVersion

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_LifeCycle.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! String lengths
  INTEGER, PARAMETER :: ML = 256   ! Error message length
  INTEGER, PARAMETER :: SL = 5000  ! Maximum length for path+filenames


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
!       Error_Status = CRTM_Init( Sensor_ID  , &
!                                 ChannelInfo, &
!                                 CloudCoeff_File    = CloudCoeff_File   , &
!                                 AerosolCoeff_File  = AerosolCoeff_File , &
!                                 Load_CloudCoeff    = Load_CloudCoeff   , &
!                                 Load_AerosolCoeff  = Load_AerosolCoeff , &
!                                 IRwaterCoeff_File  = IRwaterCoeff_File , &
!                                 IRlandCoeff_File   = IRlandCoeff_File  , &
!                                 IRsnowCoeff_File   = IRsnowCoeff_File  , &
!                                 IRiceCoeff_File    = IRiceCoeff_File   , &
!                                 VISwaterCoeff_File = VISwaterCoeff_File, &
!                                 VISlandCoeff_File  = VISlandCoeff_File , &
!                                 VISsnowCoeff_File  = VISsnowCoeff_File , &
!                                 VISiceCoeff_File   = VISiceCoeff_File  , &
!                                 MWwaterCoeff_File  = MWwaterCoeff_File , &
!                                 File_Path          = File_Path         , &
!                                 Quiet              = Quiet             , &
!                                 Process_ID         = Process_ID        , &
!                                 Output_Process_ID  = Output_Process_ID   )
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
!       CloudCoeff_File:    Name of the data file containing the cloud optical
!                           properties data for scattering calculations.
!                           Available datafiles:
!                           - CloudCoeff.bin  [DEFAULT]
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       AerosolCoeff_File:  Name of the data file containing the aerosol optical
!                           properties data for scattering calculations.
!                           Available datafiles:
!                           - AerosolCoeff.bin  [DEFAULT]
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
!       MWwaterCoeff_File:  Name of the data file containing the coefficient
!                           data for the microwave water emissivity model.
!                           Available datafiles:
!                           - FASTEM5.MWwater.EmisCoeff.bin  [DEFAULT]
!                           - FASTEM4.MWwater.EmisCoeff.bin
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       IRwaterCoeff_File:  Name of the data file containing the coefficient
!                           data for the infrared water emissivity model.
!                           Available datafiles:
!                           - Nalli.IRwater.EmisCoeff.bin  [DEFAULT]
!                           - WuSmith.IRwater.EmisCoeff.bin
!                           If not specified the Nalli datafile is read.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       IRlandCoeff_File:   Name of the data file containing the coefficient
!                           data for the infrared land emissivity model.
!                           Available datafiles:
!                           - NPOESS.IRland.EmisCoeff.bin  [DEFAULT]
!                           - IGBP.IRland.EmisCoeff.bin
!                           - USGS.IRland.EmisCoeff.bin
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       IRsnowCoeff_File:   Name of the data file containing the coefficient
!                           data for the infrared snow emissivity model.
!                           Available datafiles:
!                           - NPOESS.IRsnow.EmisCoeff.bin  [DEFAULT]
!                           - IGBP.IRsnow.EmisCoeff.bin
!                           - USGS.IRsnow.EmisCoeff.bin
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       IRiceCoeff_File:    Name of the data file containing the coefficient
!                           data for the infrared ice emissivity model.
!                           Available datafiles:
!                           - NPOESS.IRice.EmisCoeff.bin  [DEFAULT]
!                           - IGBP.IRice.EmisCoeff.bin
!                           - USGS.IRice.EmisCoeff.bin
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       VISwaterCoeff_File: Name of the data file containing the coefficient
!                           data for the visible water emissivity model.
!                           Available datafiles:
!                           - NPOESS.VISwater.EmisCoeff.bin  [DEFAULT]
!                           - IGBP.VISwater.EmisCoeff.bin
!                           - USGS.VISwater.EmisCoeff.bin
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       VISlandCoeff_File:  Name of the data file containing the coefficient
!                           data for the visible land emissivity model.
!                           Available datafiles:
!                           - NPOESS.VISland.EmisCoeff.bin  [DEFAULT]
!                           - IGBP.VISland.EmisCoeff.bin
!                           - USGS.VISland.EmisCoeff.bin
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       VISsnowCoeff_File:  Name of the data file containing the coefficient
!                           data for the visible snow emissivity model.
!                           Available datafiles:
!                           - NPOESS.VISsnow.EmisCoeff.bin  [DEFAULT]
!                           - IGBP.VISsnow.EmisCoeff.bin
!                           - USGS.VISsnow.EmisCoeff.bin
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       VISiceCoeff_File:   Name of the data file containing the coefficient
!                           data for the visible ice emissivity model.
!                           Available datafiles:
!                           - NPOESS.VISice.EmisCoeff.bin  [DEFAULT]
!                           - IGBP.VISice.EmisCoeff.bin
!                           - USGS.VISice.EmisCoeff.bin
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
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Init( &
    Sensor_ID         , &  ! Input
    ChannelInfo       , &  ! Output
    CloudCoeff_File   , &  ! Optional input
    AerosolCoeff_File , &  ! Optional input
    EmisCoeff_File    , &  ! Optional input  ! *** DEPRECATED. Replaced by IRwaterCoeff_File
    IRwaterCoeff_File , &  ! Optional input
    IRlandCoeff_File  , &  ! Optional input
    IRsnowCoeff_File  , &  ! Optional input
    IRiceCoeff_File   , &  ! Optional input
    VISwaterCoeff_File, &  ! Optional input
    VISlandCoeff_File , &  ! Optional input
    VISsnowCoeff_File , &  ! Optional input
    VISiceCoeff_File  , &  ! Optional input
    MWwaterCoeff_File , &  ! Optional input
    File_Path         , &  ! Optional input
    Load_CloudCoeff   , &  ! Optional input
    Load_AerosolCoeff , &  ! Optional input
    Quiet             , &  ! Optional input
    Process_ID        , &  ! Optional input
    Output_Process_ID ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*)               , INTENT(IN)  :: Sensor_ID(:)
    TYPE(CRTM_ChannelInfo_type), INTENT(OUT) :: ChannelInfo(:)
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: CloudCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: AerosolCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: EmisCoeff_File  ! *** DEPRECATED. Replaced by IRwaterCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: IRwaterCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: IRlandCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: IRsnowCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: IRiceCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: VISwaterCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: VISlandCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: VISsnowCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: VISiceCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: MWwaterCoeff_File
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
    CHARACTER(SL) :: Default_IRwaterCoeff_File
    CHARACTER(SL) :: Default_IRlandCoeff_File
    CHARACTER(SL) :: Default_IRsnowCoeff_File
    CHARACTER(SL) :: Default_IRiceCoeff_File
    CHARACTER(SL) :: Default_VISwaterCoeff_File
    CHARACTER(SL) :: Default_VISlandCoeff_File
    CHARACTER(SL) :: Default_VISsnowCoeff_File
    CHARACTER(SL) :: Default_VISiceCoeff_File
    CHARACTER(SL) :: Default_MWwaterCoeff_File

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
    Default_CloudCoeff_File    = 'CloudCoeff.bin'
    Default_AerosolCoeff_File  = 'AerosolCoeff.bin'
    Default_IRwaterCoeff_File  = 'Nalli.IRwater.EmisCoeff.bin'
    Default_IRlandCoeff_File   = 'NPOESS.IRland.EmisCoeff.bin'
    Default_IRsnowCoeff_File   = 'NPOESS.IRsnow.EmisCoeff.bin'
    Default_IRiceCoeff_File    = 'NPOESS.IRice.EmisCoeff.bin'
    Default_VISwaterCoeff_File = 'NPOESS.VISwater.EmisCoeff.bin'
    Default_VISlandCoeff_File  = 'NPOESS.VISland.EmisCoeff.bin'
    Default_VISsnowCoeff_File  = 'NPOESS.VISsnow.EmisCoeff.bin'
    Default_VISiceCoeff_File   = 'NPOESS.VISice.EmisCoeff.bin'
    Default_MWwaterCoeff_File  = 'FASTEM5.MWwater.EmisCoeff.bin'
    ! ...Were other filenames specified?
    IF ( PRESENT(CloudCoeff_File   ) ) Default_CloudCoeff_File    = TRIM(ADJUSTL(CloudCoeff_File))
    IF ( PRESENT(AerosolCoeff_File ) ) Default_AerosolCoeff_File  = TRIM(ADJUSTL(AerosolCoeff_File))
    IF ( PRESENT(EmisCoeff_File    ) ) Default_IRwaterCoeff_File  = TRIM(ADJUSTL(EmisCoeff_File))  ! *** DEPRECATED. Delete in next release
    IF ( PRESENT(IRwaterCoeff_File ) ) Default_IRwaterCoeff_File  = TRIM(ADJUSTL(IRwaterCoeff_File))
    IF ( PRESENT(IRlandCoeff_File  ) ) Default_IRlandCoeff_File   = TRIM(ADJUSTL(IRlandCoeff_File))
    IF ( PRESENT(IRsnowCoeff_File  ) ) Default_IRsnowCoeff_File   = TRIM(ADJUSTL(IRsnowCoeff_File))
    IF ( PRESENT(IRiceCoeff_File   ) ) Default_IRiceCoeff_File    = TRIM(ADJUSTL(IRiceCoeff_File))
    IF ( PRESENT(VISwaterCoeff_File) ) Default_VISwaterCoeff_File = TRIM(ADJUSTL(VISwaterCoeff_File))
    IF ( PRESENT(VISlandCoeff_File ) ) Default_VISlandCoeff_File  = TRIM(ADJUSTL(VISlandCoeff_File))
    IF ( PRESENT(VISsnowCoeff_File ) ) Default_VISsnowCoeff_File  = TRIM(ADJUSTL(VISsnowCoeff_File))
    IF ( PRESENT(VISiceCoeff_File  ) ) Default_VISiceCoeff_File   = TRIM(ADJUSTL(VISiceCoeff_File))
    IF ( PRESENT(MWwaterCoeff_File ) ) Default_MWwaterCoeff_File  = TRIM(ADJUSTL(MWwaterCoeff_File))
    ! ...Was a path specified?
    IF ( PRESENT(File_Path) ) THEN
      Default_CloudCoeff_File    = TRIM(ADJUSTL(File_Path)) // TRIM(Default_CloudCoeff_File)
      Default_AerosolCoeff_File  = TRIM(ADJUSTL(File_Path)) // TRIM(Default_AerosolCoeff_File)
      Default_IRwaterCoeff_File  = TRIM(ADJUSTL(File_Path)) // TRIM(Default_IRwaterCoeff_File)
      Default_IRlandCoeff_File   = TRIM(ADJUSTL(File_Path)) // TRIM(Default_IRlandCoeff_File)
      Default_IRsnowCoeff_File   = TRIM(ADJUSTL(File_Path)) // TRIM(Default_IRsnowCoeff_File)
      Default_IRiceCoeff_File    = TRIM(ADJUSTL(File_Path)) // TRIM(Default_IRiceCoeff_File)
      Default_VISwaterCoeff_File = TRIM(ADJUSTL(File_Path)) // TRIM(Default_VISwaterCoeff_File)
      Default_VISlandCoeff_File  = TRIM(ADJUSTL(File_Path)) // TRIM(Default_VISlandCoeff_File)
      Default_VISsnowCoeff_File  = TRIM(ADJUSTL(File_Path)) // TRIM(Default_VISsnowCoeff_File)
      Default_VISiceCoeff_File   = TRIM(ADJUSTL(File_Path)) // TRIM(Default_VISiceCoeff_File)
      Default_MWwaterCoeff_File  = TRIM(ADJUSTL(File_Path)) // TRIM(Default_MWwaterCoeff_File)
    END IF


    ! Load the spectral coefficients
    err_stat = CRTM_SpcCoeff_Load( &
                 Sensor_ID                            , &
                 File_Path         = File_Path        , &
                 Quiet             = Quiet            , &
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
    ! ...IR land
    err_stat = CRTM_IRlandCoeff_Load( &
                 Default_IRlandCoeff_File, &
                 Quiet             = Quiet            , &
                 Process_ID        = Process_ID       , &
                 Output_Process_ID = Output_Process_ID  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error loading IRlandCoeff data from '//TRIM(Default_IRlandCoeff_File)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
      RETURN
    END IF
    ! ...IR Water
    err_stat = CRTM_IRwaterCoeff_Load( &
                 Default_IRwaterCoeff_File, &
                 Quiet             = Quiet            , &
                 Process_ID        = Process_ID       , &
                 Output_Process_ID = Output_Process_ID  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error loading IRwaterCoeff data from '//TRIM(Default_IRwaterCoeff_File)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
      RETURN
    END IF
    ! ...IR snow
    err_stat = CRTM_IRsnowCoeff_Load( &
                 Default_IRsnowCoeff_File, &
                 Quiet             = Quiet            , &
                 Process_ID        = Process_ID       , &
                 Output_Process_ID = Output_Process_ID  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error loading IRsnowCoeff data from '//TRIM(Default_IRsnowCoeff_File)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
      RETURN
    END IF
    ! ...IR ice
    err_stat = CRTM_IRiceCoeff_Load( &
                 Default_IRiceCoeff_File, &
                 Quiet             = Quiet            , &
                 Process_ID        = Process_ID       , &
                 Output_Process_ID = Output_Process_ID  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error loading IRiceCoeff data from '//TRIM(Default_IRiceCoeff_File)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
      RETURN
    END IF
    ! ...VIS land
    err_stat = CRTM_VISlandCoeff_Load( &
                 Default_VISlandCoeff_File, &
                 Quiet             = Quiet            , &
                 Process_ID        = Process_ID       , &
                 Output_Process_ID = Output_Process_ID  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error loading VISlandCoeff data from '//TRIM(Default_VISlandCoeff_File)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
      RETURN
    END IF
    ! ...VIS water
    err_stat = CRTM_VISwaterCoeff_Load( &
                 Default_VISwaterCoeff_File, &
                 Quiet             = Quiet            , &
                 Process_ID        = Process_ID       , &
                 Output_Process_ID = Output_Process_ID  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error loading VISwaterCoeff data from '//TRIM(Default_VISwaterCoeff_File)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
      RETURN
    END IF
    ! ...VIS snow
    err_stat = CRTM_VISsnowCoeff_Load( &
                 Default_VISsnowCoeff_File, &
                 Quiet             = Quiet            , &
                 Process_ID        = Process_ID       , &
                 Output_Process_ID = Output_Process_ID  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error loading VISsnowCoeff data from '//TRIM(Default_VISsnowCoeff_File)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
      RETURN
    END IF
    ! ...VIS ice
    err_stat = CRTM_VISiceCoeff_Load( &
                 Default_VISiceCoeff_File, &
                 Quiet             = Quiet            , &
                 Process_ID        = Process_ID       , &
                 Output_Process_ID = Output_Process_ID  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error loading VISiceCoeff data from '//TRIM(Default_VISiceCoeff_File)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
      RETURN
    END IF


    ! Load the emissivity model coefficients
    ! ...MW water
    err_stat = CRTM_MWwaterCoeff_Load( &
                 Default_MWwaterCoeff_File, &
                 Quiet             = Quiet            , &
                 Process_ID        = Process_ID       , &
                 Output_Process_ID = Output_Process_ID  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error loading MWwaterCoeff data from '//TRIM(Default_MWwaterCoeff_File)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
      RETURN
    END IF


    ! Load the ChannelInfo structure
    DO n = 1, n_Sensors
      ! ...Allocate the ChannelInfo structure
      CALL CRTM_ChannelInfo_Create( ChannelInfo(n), SC(n)%n_Channels )
      IF ( .NOT. CRTM_ChannelInfo_Associated(ChannelInfo(n)) ) THEN
        err_stat = FAILURE
        msg = 'ChannelInfo allocation failed for '//TRIM(Sensor_Id(n))//' sensor'
        CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
        RETURN
      END IF
      ! ...Fill the structure for the current sensor
      ChannelInfo(n)%Sensor_Index     = n
      ChannelInfo(n)%Channel_Index    = (/(l, l=1,SC(n)%n_Channels)/)
      ChannelInfo(n)%Sensor_ID        = SC(n)%Sensor_Id
      ChannelInfo(n)%Sensor_Type      = SC(n)%Sensor_Type
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
!                     DIMENSION:  Rank-1
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
    Destroy_Status = CRTM_VISiceCoeff_Destroy( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      msg = 'Error deallocating shared VISiceCoeff data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF

    Destroy_Status = CRTM_VISsnowCoeff_Destroy( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      msg = 'Error deallocating shared VISsnowCoeff data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF

    Destroy_Status = CRTM_VISwaterCoeff_Destroy( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      msg = 'Error deallocating shared VISwaterCoeff data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF

    Destroy_Status = CRTM_VISlandCoeff_Destroy( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      msg = 'Error deallocating shared VISlandCoeff data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF

    Destroy_Status = CRTM_IRiceCoeff_Destroy( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      msg = 'Error deallocating shared IRiceCoeff data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF

    Destroy_Status = CRTM_IRsnowCoeff_Destroy( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      msg = 'Error deallocating shared IRsnowCoeff data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF

    Destroy_Status = CRTM_IRwaterCoeff_Destroy( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      msg = 'Error deallocating shared IRwaterCoeff data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF

    Destroy_Status = CRTM_IRlandCoeff_Destroy( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      msg = 'Error deallocating shared IRlandCoeff data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF

    Destroy_Status = CRTM_AerosolCoeff_Destroy( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      msg = 'Error deallocating shared AerosolCoeff data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF

    Destroy_Status = CRTM_CloudCoeff_Destroy( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      msg = 'Error deallocating shared CloudCoeff data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF

    Destroy_Status = CRTM_Destroy_TauCoeff( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      msg = 'Error deallocating shared TauCoeff data structure(s)'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF

    Destroy_Status = CRTM_SpcCoeff_Destroy( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      msg = 'Error deallocating shared SpcCoeff data structure(s)'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF

    Destroy_Status = CRTM_MWwaterCoeff_Destroy( Process_ID = Process_ID )
    IF ( Destroy_Status /= SUCCESS ) THEN
      err_stat = Destroy_Status
      msg = 'Error deallocating shared MWwaterCoeff data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
    END IF

  END FUNCTION CRTM_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_IsInitialized
!
! PURPOSE:
!       Logical function to test if the CRTM has been correctly initialized.
!
! CALLING SEQUENCE:
!       status = CRTM_IsInitialized( ChannelInfo )
!
! INPUTS:
!       ChannelInfo:  ChannelInfo structure array.
!                     UNITS:      N/A
!                     TYPE:       CRTM_ChannelInfo_type
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:       The return value is a logical result indicating if the
!                     CRTM has been correctly initialised.
!                     If == .TRUE., all the ChannelInfo entries are valid.
!                        == .FALSE., any of the ChannelInfo entries are invalid.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_IsInitialized( ChannelInfo ) RESULT( Status )
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo(:)
    LOGICAL :: Status
    Status = ALL(CRTM_ChannelInfo_Associated(ChannelInfo))
  END FUNCTION CRTM_IsInitialized


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
