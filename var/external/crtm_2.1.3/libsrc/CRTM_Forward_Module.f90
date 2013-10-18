!
! CRTM_Forward_Module
!
! Module containing the CRTM forward model function.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 29-Jun-2004
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_Forward_Module


  ! ------------
  ! Module usage
  ! ------------
  USE Type_Kinds,                 ONLY: fp
  USE Message_Handler,            ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE CRTM_Parameters,            ONLY: SET,NOT_SET,ZERO,ONE, &
                                        MAX_N_LAYERS        , &
                                        MAX_N_PHASE_ELEMENTS, &
                                        MAX_N_LEGENDRE_TERMS, &
                                        MAX_N_STOKES        , &
                                        MAX_N_ANGLES        , &
                                        MAX_N_AZIMUTH_FOURIER, &
                                        MAX_SOURCE_ZENITH_ANGLE, &
                                        MAX_N_STREAMS, &
                                        AIRCRAFT_PRESSURE_THRESHOLD
  USE CRTM_SpcCoeff,              ONLY: SC, &
                                        SpcCoeff_IsVisibleSensor
  USE CRTM_Atmosphere_Define,     ONLY: CRTM_Atmosphere_type, &
                                        CRTM_Atmosphere_Destroy, &
                                        CRTM_Atmosphere_IsValid, &
                                        CRTM_Get_PressureLevelIdx
  USE CRTM_Surface_Define,        ONLY: CRTM_Surface_type, &
                                        CRTM_Surface_IsValid
  USE CRTM_Geometry_Define,       ONLY: CRTM_Geometry_type, &
                                        CRTM_Geometry_IsValid
  USE CRTM_ChannelInfo_Define,    ONLY: CRTM_ChannelInfo_type, &
                                        CRTM_ChannelInfo_n_Channels
  USE CRTM_Options_Define,        ONLY: CRTM_Options_type, &
                                        CRTM_Options_IsValid
  USE CRTM_Atmosphere,            ONLY: CRTM_Atmosphere_AddLayers
  USE CRTM_GeometryInfo_Define,   ONLY: CRTM_GeometryInfo_type, &
                                        CRTM_GeometryInfo_SetValue, &
                                        CRTM_GeometryInfo_GetValue
  USE CRTM_GeometryInfo,          ONLY: CRTM_GeometryInfo_Compute
  USE CRTM_Predictor_Define,      ONLY: CRTM_Predictor_type      , &
                                        CRTM_Predictor_Associated, &
                                        CRTM_Predictor_Destroy   , &
                                        CRTM_Predictor_Create
  USE CRTM_Predictor,             ONLY: CRTM_PVar_type => iVar_type, &
                                        CRTM_Compute_Predictors
  USE CRTM_AtmAbsorption,         ONLY: CRTM_AAvar_type => iVar_type, &
                                        CRTM_Compute_AtmAbsorption
  USE CRTM_AtmOptics_Define,      ONLY: CRTM_AtmOptics_type      , &
                                        CRTM_AtmOptics_Associated, &
                                        CRTM_AtmOptics_Create    , &
                                        CRTM_AtmOptics_Destroy   , &
                                        CRTM_AtmOptics_Zero
  USE CRTM_AerosolScatter,        ONLY: CRTM_Compute_AerosolScatter
  USE CRTM_CloudScatter,          ONLY: CRTM_Compute_CloudScatter
  USE CRTM_AtmOptics,             ONLY: CRTM_AOVariables_type     , &
                                        CRTM_Compute_Transmittance, &
                                        CRTM_Combine_AtmOptics
  USE CRTM_SfcOptics_Define,      ONLY: CRTM_SfcOptics_type      , &
                                        CRTM_SfcOptics_Associated, &
                                        CRTM_SfcOptics_Create    , &
                                        CRTM_SfcOptics_Destroy
  USE CRTM_SfcOptics,             ONLY: CRTM_Compute_SurfaceT
  USE CRTM_RTSolution,            ONLY: CRTM_RTSolution_type    , &
                                        CRTM_Compute_nStreams   , &
                                        CRTM_Compute_RTSolution
  USE CRTM_AntennaCorrection,     ONLY: CRTM_Compute_AntCorr
  USE CRTM_MoleculeScatter,       ONLY: CRTM_Compute_MoleculeScatter
  USE CRTM_AncillaryInput_Define, ONLY: CRTM_AncillaryInput_type
  USE CRTM_CloudCoeff,            ONLY: CRTM_CloudCoeff_IsLoaded
  USE CRTM_AerosolCoeff,          ONLY: CRTM_AerosolCoeff_IsLoaded
  USE CRTM_NLTECorrection,        ONLY: NLTE_Predictor_type    , &
                                        NLTE_Predictor_IsActive, &
                                        Compute_NLTE_Predictor , &
                                        Compute_NLTE_Correction
  USE ACCoeff_Define,             ONLY: ACCoeff_Associated
  USE NLTECoeff_Define,           ONLY: NLTECoeff_Associated
  USE CRTM_Planck_Functions,      ONLY: CRTM_Planck_Temperature

  ! Internal variable definition modules
  ! ...CloudScatter
  USE CSvar_Define, ONLY: CSvar_type, &
                          CSvar_Associated, &
                          CSvar_Destroy   , &
                          CSvar_Create
  ! ...AerosolScatter
  USE ASvar_Define, ONLY: ASvar_type, &
                          ASvar_Associated, &
                          ASvar_Destroy   , &
                          ASvar_Create
  ! ...Radiative transfer
  USE RTV_Define,   ONLY: RTV_type      , &
                          RTV_Associated, &
                          RTV_Destroy   , &
                          RTV_Create


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public procedures
  PUBLIC :: CRTM_Forward
  PUBLIC :: CRTM_Forward_Version


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Forward_Module.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Forward
!
! PURPOSE:
!       Function that calculates top-of-atmosphere (TOA) radiances
!       and brightness temperatures for an input atmospheric profile or
!       profile set and user specified satellites/channels.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Forward( Atmosphere       , &
!                                    Surface          , &
!                                    Geometry         , &
!                                    ChannelInfo      , &
!                                    RTSolution       , &
!                                    Options = Options  )
!
! INPUTS:
!       Atmosphere:     Structure containing the Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Rank-1 (n_Profiles)
!                       ATTRIBUTES: INTENT(IN)
!
!       Surface:        Structure containing the Surface data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN)
!
!       Geometry:       Structure containing the view geometry
!                       information.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Geometry_type
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN)
!
!       ChannelInfo:    Structure returned from the CRTM_Init() function
!                       that contains the satellite/sensor channel index
!                       information.
!                       UNITS:      N/A
!                       TYPE:       CRTM_ChannelInfo_type
!                       DIMENSION:  Rank-1 (n_Sensors)
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       RTSolution:     Structure containing the soluition to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-2 (n_Channels x n_Profiles)
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Options:        Options structure containing the optional arguments
!                       for the CRTM.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Options_type
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the computation was sucessful
!                          == FAILURE an unrecoverable error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!       - The Options optional input structure argument contains
!         spectral information (e.g. emissivity) that must have the same
!         spectral dimensionality (the "L" dimension) as the output
!         RTSolution structure.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Forward( &
    Atmosphere , &  ! Input, M
    Surface    , &  ! Input, M
    Geometry   , &  ! Input, M
    ChannelInfo, &  ! Input, n_Sensors
    RTSolution , &  ! Output, L x M
    Options    ) &  ! Optional input, M
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type),        INTENT(IN)     :: Atmosphere(:)     ! M
    TYPE(CRTM_Surface_type),           INTENT(IN)     :: Surface(:)        ! M
    TYPE(CRTM_Geometry_type),          INTENT(IN)     :: Geometry(:)       ! M
    TYPE(CRTM_ChannelInfo_type),       INTENT(IN)     :: ChannelInfo(:)    ! n_Sensors
    TYPE(CRTM_RTSolution_type),        INTENT(IN OUT) :: RTSolution(:,:)   ! L x M
    TYPE(CRTM_Options_type), OPTIONAL, INTENT(IN)     :: Options(:)        ! M
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Forward'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Options_Present
    LOGICAL :: Check_Input
    LOGICAL :: User_Emissivity, User_Direct_Reflectivity, User_N_Streams
    LOGICAL :: User_AntCorr, Compute_AntCorr
    LOGICAL :: Apply_NLTE_Correction
    LOGICAL :: Atmosphere_Invalid, Surface_Invalid, Geometry_Invalid, Options_Invalid
    INTEGER :: RT_Algorithm_Id
    INTEGER :: iFOV
    INTEGER :: n, n_Sensors,  SensorIndex
    INTEGER :: l, n_Channels, ChannelIndex
    INTEGER :: m, n_Profiles
    INTEGER :: ln
    INTEGER :: n_Full_Streams, mth_Azi
    REAL(fp) :: Source_ZA
    REAL(fp) :: Wavenumber
    REAL(fp) :: Aircraft_Pressure
    REAL(fp) :: transmittance
    ! Local ancillary input structure
    TYPE(CRTM_AncillaryInput_type) :: AncillaryInput
    ! Local options structure for default values
    TYPE(CRTM_Options_type) :: Default_Options
    ! Local atmosphere structure for extra layering
    TYPE(CRTM_Atmosphere_type) :: Atm
    ! Component variables
    TYPE(CRTM_GeometryInfo_type) :: GeometryInfo
    TYPE(CRTM_Predictor_type)    :: Predictor
    TYPE(CRTM_AtmOptics_type)    :: AtmOptics
    TYPE(CRTM_SfcOptics_type)    :: SfcOptics
    ! Component variable internals
    TYPE(CRTM_PVar_type)  :: PVar   ! Predictor
    TYPE(CRTM_AAvar_type) :: AAvar  ! AtmAbsorption
    TYPE(CSVar_type)      :: CSvar  ! CloudScatter
    TYPE(ASVar_type)      :: ASvar  ! AerosolScatter
    TYPE(CRTM_AOVariables_type) :: AOV  ! AtmOptics
    TYPE(RTV_type)        :: RTV  ! RTSolution
    ! NLTE correction term predictor
    TYPE(NLTE_Predictor_type)   :: NLTE_Predictor


    ! ------
    ! SET UP
    ! ------
    Error_Status = SUCCESS


    ! If no sensors or channels, simply return
    n_Sensors  = SIZE(ChannelInfo)
    n_Channels = SUM(CRTM_ChannelInfo_n_Channels(ChannelInfo))
    IF ( n_Sensors == 0 .OR. n_Channels == 0 ) RETURN


    ! Check output array
    IF ( SIZE(RTSolution,DIM=1) < n_Channels ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Output RTSolution structure array too small (",i0,&
             &") to hold results for the number of requested channels (",i0,")")') &
             SIZE(RTSolution,DIM=1), n_Channels
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF


    ! Check the number of profiles
    ! ...Number of atmospheric profiles.
    n_Profiles = SIZE(Atmosphere)
    ! ...Check the profile dimensionality of the other mandatory arguments
    IF ( SIZE(Surface)          /= n_Profiles .OR. &
         SIZE(Geometry)         /= n_Profiles .OR. &
         SIZE(RTSolution,DIM=2) /= n_Profiles      ) THEN
      Error_Status = FAILURE
      Message = 'Inconsistent profile dimensionality for input arguments.'
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF
    ! ...Check the profile dimensionality of the other optional arguments
    Options_Present = .FALSE.
    IF ( PRESENT(Options) ) THEN
      Options_Present = .TRUE.
      IF ( SIZE(Options) /= n_Profiles ) THEN
        Error_Status = FAILURE
        Message = 'Inconsistent profile dimensionality for Options optional input argument.'
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
    END IF


    ! Allocate the profile independent surface opticss local structure
    CALL CRTM_SfcOptics_Create( SfcOptics, MAX_N_ANGLES, MAX_N_STOKES )
    IF ( .NOT. CRTM_SfcOptics_Associated(SfcOptics) ) THEN
      Error_Status = FAILURE
      Message = 'Error allocating SfcOptics data structures'
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF


    ! ------------
    ! PROFILE LOOP
    ! ------------
    Profile_Loop: DO m = 1, n_Profiles


      ! Check the cloud and aerosol coeff. data for cases with clouds and aerosol
      IF( Atmosphere(m)%n_Clouds > 0 .AND. .NOT. CRTM_CloudCoeff_IsLoaded() )THEN
         Error_Status = FAILURE
         WRITE( Message,'("The CloudCoeff data must be loaded (with CRTM_Init routine) ", &
                &"for the cloudy case profile #",i0)' ) m
         CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
         RETURN
      END IF
      IF( Atmosphere(m)%n_Aerosols > 0 .AND. .NOT. CRTM_AerosolCoeff_IsLoaded() )THEN
         Error_Status = FAILURE
         WRITE( Message,'("The AerosolCoeff data must be loaded (with CRTM_Init routine) ", &
                &"for the aerosol case profile #",i0)' ) m
         CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
         RETURN
      END IF


      ! Check the optional Options structure argument
      ! ...Specify default actions
      Check_Input           = Default_Options%Check_Input
      User_Emissivity       = Default_Options%Use_Emissivity
      User_AntCorr          = Default_Options%Use_Antenna_Correction
      Apply_NLTE_Correction = Default_Options%Apply_NLTE_Correction
      RT_Algorithm_Id       = Default_Options%RT_Algorithm_Id
      User_N_Streams        = Default_Options%Use_N_Streams
      Aircraft_Pressure     = Default_Options%Aircraft_Pressure
      ! ...Check the Options argument
      IF (Options_Present) THEN
        ! Override input checker with option
        Check_Input = Options(m)%Check_Input
        ! Check if the supplied emissivity should be used
        User_Emissivity = Options(m)%Use_Emissivity
        IF ( Options(m)%Use_Emissivity ) THEN
          ! Are the channel dimensions consistent
          IF ( Options(m)%n_Channels < n_Channels ) THEN
            Error_Status = FAILURE
            WRITE( Message,'( "Input Options channel dimension (", i0, ") is less ", &
                   &"than the number of requested channels (",i0, ")" )' ) &
                   Options(m)%n_Channels, n_Channels
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF
          ! Check if the supplied direct reflectivity should be used
          User_Direct_Reflectivity = Options(m)%Use_Direct_Reflectivity
        END IF
        ! Check if antenna correction should be attempted
        User_AntCorr = Options(m)%Use_Antenna_Correction
        ! Set NLTE correction option
        Apply_NLTE_Correction = Options(m)%Apply_NLTE_Correction
        ! Set aircraft pressure altitude
        Aircraft_Pressure = Options(m)%Aircraft_Pressure

        ! Copy over ancillary input
        AncillaryInput%SSU    = Options(m)%SSU
        AncillaryInput%Zeeman = Options(m)%Zeeman
        ! Copy over surface optics input
        SfcOptics%Use_New_MWSSEM = .NOT. Options(m)%Use_Old_MWSSEM
        ! Specify the RT algorithm
        RT_Algorithm_Id = Options(m)%RT_Algorithm_Id
        ! Check if n_Streams should be used
        User_N_Streams = Options(m)%Use_N_Streams
        ! Check value for nstreams
        IF ( User_N_Streams ) THEN
          IF ( Options(m)%n_Streams <= 0 .OR. MOD(Options(m)%n_Streams,2) /= 0 .OR. &
               Options(m)%n_Streams > MAX_N_STREAMS ) THEN
              Error_Status = FAILURE
              WRITE( Message,'( "Input Options n_Streams (", i0, ") is invalid" )' ) &
                     Options(m)%n_Streams
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
          END IF
        END IF
      END IF


      ! Check the input data if required
      IF ( Check_Input ) THEN
        ! ...Mandatory inputs
        Atmosphere_Invalid = .NOT. CRTM_Atmosphere_IsValid( Atmosphere(m) )
        Surface_Invalid    = .NOT. CRTM_Surface_IsValid( Surface(m) )
        Geometry_Invalid   = .NOT. CRTM_Geometry_IsValid( Geometry(m) )
        IF ( Atmosphere_Invalid .OR. Surface_Invalid .OR. Geometry_Invalid ) THEN
          Error_Status = FAILURE
          WRITE( Message,'("Input data check failed for profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
        ! ...Optional input
        IF ( Options_Present ) THEN
          Options_Invalid = .NOT. CRTM_Options_IsValid( Options(m) )
          IF ( Options_Invalid ) THEN
            Error_Status = FAILURE
            WRITE( Message,'("Options data check failed for profile #",i0)' ) m
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF
        END IF
      END IF


      ! Process geometry
      ! ...Compute derived geometry
      CALL CRTM_GeometryInfo_SetValue( GeometryInfo, Geometry=Geometry(m) )
      CALL CRTM_GeometryInfo_Compute( GeometryInfo )
      ! ...Retrieve components into local variable
      CALL CRTM_GeometryInfo_GetValue( &
             GeometryInfo, &
             iFOV = iFOV, &
             Source_Zenith_Angle = Source_ZA )


      ! Average surface skin temperature for multi-surface types
      CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics )


      ! Add extra layers to current atmosphere profile
      ! if necessary to handle upper atmosphere
      Error_Status = CRTM_Atmosphere_AddLayers( Atmosphere(m), Atm )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error adding extra layers to profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
      IF ( Atm%n_Layers > MAX_N_LAYERS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Added layers [",i0,"] cause total [",i0,"] to exceed the ",&
               &"maximum allowed [",i0,"] for profile #",i0)' ) &
               Atm%n_Added_Layers, Atm%n_Layers, MAX_N_LAYERS, m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
      ! ...Allocate the atmospheric optics structures based on Atm extension
      CALL CRTM_AtmOptics_Create( AtmOptics, &
                                  Atm%n_Layers        , &
                                  MAX_N_LEGENDRE_TERMS, &
                                  MAX_N_PHASE_ELEMENTS  )
      IF (Options_Present) THEN
        ! Set Scattering Switch
        AtmOptics%Include_Scattering = Options(m)%Include_Scattering
      END IF
      IF ( .NOT. CRTM_AtmOptics_Associated( Atmoptics ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error allocating AtmOptics data structure for profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF

      ! Process aircraft pressure altitude
      IF ( Aircraft_Pressure > ZERO ) THEN
        RTV%aircraft%rt = .TRUE.
        RTV%aircraft%idx = CRTM_Get_PressureLevelIdx(Atm, Aircraft_Pressure)
        ! ...Issue warning if profile level is TOO different from flight level
        IF ( ABS(Atm%Level_Pressure(RTV%aircraft%idx)-Aircraft_Pressure) > AIRCRAFT_PRESSURE_THRESHOLD ) THEN
          WRITE( Message,'("Difference between aircraft pressure level (",es13.6,&
                          &"hPa) and closest input profile level (",es13.6,&
                          &"hPa) is larger than recommended (",f4.1,"hPa) for profile #",i0)') &
                          Aircraft_Pressure, Atm%Level_Pressure(RTV%aircraft%idx), &
                          AIRCRAFT_PRESSURE_THRESHOLD, m
          CALL Display_Message( ROUTINE_NAME, Message, WARNING )
        END IF
      ELSE
        RTV%aircraft%rt = .FALSE.
      END IF


      ! Allocate the scattering internal variables if necessary
      ! ...Cloud
      IF ( Atm%n_Clouds > 0 ) THEN
        CALL CSvar_Create( CSvar, &
                           MAX_N_LEGENDRE_TERMS, &
                           MAX_N_PHASE_ELEMENTS, &
                           Atm%n_Layers        , &
                           Atm%n_Clouds          )
      END IF
      ! ...Aerosol
      IF ( Atm%n_Aerosols > 0 ) THEN
        CALL ASvar_Create( ASvar, &
                           MAX_N_LEGENDRE_TERMS, &
                           MAX_N_PHASE_ELEMENTS, &
                           Atm%n_Layers        , &
                           Atm%n_Aerosols        )
      END IF


      ! -----------
      ! SENSOR LOOP
      ! -----------
      ! Initialise channel counter for channel(l)/sensor(n) count
      ln = 0

      Sensor_Loop: DO n = 1, n_Sensors


        ! Shorter name
        SensorIndex = ChannelInfo(n)%Sensor_Index


        ! Check if antenna correction to be applied for current sensor
        IF ( User_AntCorr                             .AND. &
             ACCoeff_Associated( SC(SensorIndex)%AC ) .AND. &
             iFOV /= 0 ) THEN
          Compute_AntCorr = .TRUE.
        ELSE
          Compute_AntCorr = .FALSE.
        END IF


        ! Compute predictors for AtmAbsorption calcs
        ! ...Allocate the predictor structure
        CALL CRTM_Predictor_Create( &
               Predictor   , &
               atm%n_Layers, &
               SensorIndex   )
        IF ( .NOT. CRTM_Predictor_Associated(Predictor) ) THEN
          Error_Status=FAILURE
          WRITE( Message,'("Error allocating predictor structure for profile #",i0, &
                 &" and ",a," sensor.")' ) m, SC(SensorIndex)%Sensor_Id
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
        ! ...And now fill them
        CALL CRTM_Compute_Predictors( SensorIndex   , &  ! Input
                                      Atm           , &  ! Input
                                      GeometryInfo  , &  ! Input
                                      AncillaryInput, &  ! Input
                                      Predictor     , &  ! Output
                                      PVar            )  ! Internal variable output


        ! Allocate the RTV structure if necessary
        IF( (Atm%n_Clouds   > 0 .OR. &
            Atm%n_Aerosols > 0 .OR. &
            SpcCoeff_IsVisibleSensor( SC(SensorIndex) ) ) .and. AtmOptics%Include_Scattering ) THEN
          CALL RTV_Create( RTV, MAX_N_ANGLES, MAX_N_LEGENDRE_TERMS, Atm%n_Layers )
          IF ( .NOT. RTV_Associated(RTV) ) THEN
            Error_Status=FAILURE
            WRITE( Message,'("Error allocating RTV structure for profile #",i0, &
                   &" and ",a," sensor.")' ) m, TRIM(SC(SensorIndex)%Sensor_Id)
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF
          ! Assign algorithm selector
          RTV%RT_Algorithm_Id = RT_Algorithm_Id
        END IF


        ! Compute NLTE correction predictors
        IF ( Apply_NLTE_Correction ) THEN
          CALL Compute_NLTE_Predictor( &
                 SC(SensorIndex)%NC, &  ! Input
                 Atm               , &  ! Input
                 GeometryInfo      , &  ! Input
                 NLTE_Predictor      )  ! Output
        END IF


        ! ------------
        ! CHANNEL LOOP
        ! ------------
        Channel_Loop: DO l = 1, ChannelInfo(n)%n_Channels

          ! Channel setup
          ! ...Skip channel if requested
          IF ( .NOT. ChannelInfo(n)%Process_Channel(l) ) CYCLE Channel_Loop
          ! ...Shorter name
          ChannelIndex = ChannelInfo(n)%Channel_Index(l)
          ! ...Increment the processed channel counter
          ln = ln + 1
          ! ...Assign sensor+channel information to output
          RTSolution(ln,m)%Sensor_Id        = ChannelInfo(n)%Sensor_Id
          RTSolution(ln,m)%WMO_Satellite_Id = ChannelInfo(n)%WMO_Satellite_Id
          RTSolution(ln,m)%WMO_Sensor_Id    = ChannelInfo(n)%WMO_Sensor_Id
          RTSolution(ln,m)%Sensor_Channel   = ChannelInfo(n)%Sensor_Channel(l)


          ! Initialisations
          CALL CRTM_AtmOptics_Zero( AtmOptics )

          ! Determine the number of streams (n_Full_Streams) in up+downward directions
          IF ( User_N_Streams ) THEN
            n_Full_Streams = Options(m)%n_Streams
            RTSolution(ln,m)%n_Full_Streams = n_Full_Streams + 2
            RTSolution(ln,m)%Scattering_Flag = .TRUE.
          ELSE
            n_Full_Streams = CRTM_Compute_nStreams( Atm             , &  ! Input
                                                    SensorIndex     , &  ! Input
                                                    ChannelIndex    , &  ! Input
                                                    RTSolution(ln,m)  )  ! Output
          END IF
          ! ...Transfer stream count to scattering structure
          AtmOptics%n_Legendre_Terms = n_Full_Streams


          ! Compute the gas absorption
          CALL CRTM_Compute_AtmAbsorption( SensorIndex   , &  ! Input
                                           ChannelIndex  , &  ! Input
                                           AncillaryInput, &  ! Input
                                           Predictor     , &  ! Input
                                           AtmOptics     , &  ! Output
                                           AAvar           )  ! Internal variable output
          

          ! Compute and save the total atmospheric transmittance
          ! for use in surface optics reflection corrections
          CALL CRTM_Compute_Transmittance(AtmOptics,transmittance)
          SfcOptics%Transmittance = transmittance


          ! Compute the molecular scattering properties
          ! ...Solar radiation
          IF( SC(SensorIndex)%Solar_Irradiance(ChannelIndex) > ZERO .AND. &
              Source_ZA < MAX_SOURCE_ZENITH_ANGLE ) THEN
             RTV%Solar_Flag_true = .TRUE.
          END IF
          ! ...Visible channel with solar radiation
          IF( SpcCoeff_IsVisibleSensor( SC(SensorIndex) ) .AND. RTV%Solar_Flag_true ) THEN
            RTV%Visible_Flag_true = .TRUE.
            ! Rayleigh phase function has 0, 1, 2 components.
            IF( AtmOptics%n_Legendre_Terms < 4 ) THEN
              AtmOptics%n_Legendre_Terms = 4
              RTSolution(ln,m)%Scattering_FLAG = .TRUE.
              RTSolution(ln,m)%n_Full_Streams = AtmOptics%n_Legendre_Terms + 2
            END IF
            RTV%n_Azi = MIN( AtmOptics%n_Legendre_Terms - 1, MAX_N_AZIMUTH_FOURIER )
            ! Get molecular scattering and extinction
            Wavenumber = SC(SensorIndex)%Wavenumber(ChannelIndex)
            Error_Status = CRTM_Compute_MoleculeScatter( &
                             Wavenumber, &
                             Atm       , &
                             AtmOptics   )
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing MoleculeScatter for ",a,&
                     &", channel ",i0,", profile #",i0)') &
                     TRIM(ChannelInfo(n)%Sensor_ID), &
                     ChannelInfo(n)%Sensor_Channel(l), &
                     m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          ELSE
            RTV%Visible_Flag_true = .FALSE.
            RTV%n_Azi = 0
          END IF


          ! Compute the cloud particle absorption/scattering properties
          IF( Atm%n_Clouds > 0 ) THEN
            Error_Status = CRTM_Compute_CloudScatter( Atm         , &  ! Input
                                                      SensorIndex , &  ! Input
                                                      ChannelIndex, &  ! Input
                                                      AtmOptics   , &  ! Output
                                                      CSvar         )  ! Internal variable output
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing CloudScatter for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
            ! ...Switch off any reflection correction for multi-stream RT
            SfcOptics%Transmittance = -ONE
          END IF

          ! Compute the aerosol absorption/scattering properties
          IF ( Atm%n_Aerosols > 0 ) THEN
            Error_Status = CRTM_Compute_AerosolScatter( Atm         , &  ! Input
                                                        SensorIndex , &  ! Input
                                                        ChannelIndex, &  ! Input
                                                        AtmOptics   , &  ! In/Output
                                                        ASvar         )  ! Internal variable output
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing AerosolScatter for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
            ! ...Switch off any reflection correction for multi-stream RT
            SfcOptics%Transmittance = -ONE
          END IF


          ! Compute the combined atmospheric optical properties

          IF( AtmOptics%Include_Scattering ) THEN
            CALL CRTM_Combine_AtmOptics( AtmOptics, AOV )
          END IF
          ! ...Save vertically integrated scattering optical depth fro output
          RTSolution(ln,m)%SOD = AtmOptics%Scattering_Optical_Depth


          ! Fill the SfcOptics structure for the optional emissivity input case.
          ! ...Indicate SfcOptics ARE to be computed
          SfcOptics%Compute = .TRUE.
          ! ...Change SfcOptics emissivity/reflectivity contents/computation status
          IF ( User_Emissivity ) THEN
            SfcOptics%Compute = .FALSE.
            SfcOptics%Emissivity(1,1)       = Options(m)%Emissivity(ln)
            SfcOptics%Reflectivity(1,1,1,1) = ONE - Options(m)%Emissivity(ln)
            IF ( User_Direct_Reflectivity ) THEN
              SfcOptics%Direct_Reflectivity(1,1) = Options(m)%Direct_Reflectivity(ln)
            ELSE
              SfcOptics%Direct_Reflectivity(1,1) = SfcOptics%Reflectivity(1,1,1,1)
            END IF
          END IF


          ! Fourier component loop for azimuth angles (VIS).
          ! mth_Azi = 0 is for an azimuth-averaged value (IR, MW)
          ! ...Initialise radiance
          RTSolution(ln,m)%Radiance = ZERO
          ! ...Fourier expansion over azimuth angle
          Azimuth_Fourier_Loop: DO mth_Azi = 0, RTV%n_Azi

            ! Set dependent component counters
            RTV%mth_Azi = mth_Azi
            SfcOptics%mth_Azi = mth_Azi

            ! Solve the radiative transfer problem
            Error_Status = CRTM_Compute_RTSolution( &
                             Atm             , &  ! Input
                             Surface(m)      , &  ! Input
                             AtmOptics       , &  ! Input
                             SfcOptics       , &  ! Input
                             GeometryInfo    , &  ! Input
                             SensorIndex     , &  ! Input
                             ChannelIndex    , &  ! Input
                             RTSolution(ln,m), &  ! Output
                             RTV               )  ! Internal variable output
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'( "Error computing RTSolution for ", a, &
                     &", channel ", i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          END DO Azimuth_Fourier_Loop

          ! Compute non-LTE correction to radiance if required
          IF ( Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
            CALL Compute_NLTE_Correction( &
                   SC(SensorIndex)%NC       , &  ! Input
                   ChannelIndex             , &  ! Input
                   NLTE_Predictor           , &  ! Input
                   RTSolution(ln,m)%Radiance  )  ! In/Output
          END IF

          ! Convert the radiance to brightness temperature
          CALL CRTM_Planck_Temperature( &
                 SensorIndex                            , & ! Input
                 ChannelIndex                           , & ! Input
                 RTSolution(ln,m)%Radiance              , & ! Input
                 RTSolution(ln,m)%Brightness_Temperature  ) ! Output

          ! Compute Antenna correction to brightness temperature if required
          IF ( Compute_AntCorr ) THEN
            CALL CRTM_Compute_AntCorr( &
                   GeometryInfo    , &  ! Input
                   SensorIndex     , &  ! Input
                   ChannelIndex    , &  ! Input
                   RTSolution(ln,m)  )  ! Output
          END IF
        END DO Channel_Loop


        ! Deallocate local sensor dependent data structures
        ! ...RTV structure
        IF ( RTV_Associated(RTV) ) CALL RTV_Destroy(RTV)
        ! ...Predictor structure
        CALL CRTM_Predictor_Destroy( Predictor )

      END DO Sensor_Loop


      ! Deallocate local sensor independent data structures
      ! ...Atmospheric optics
      CALL CRTM_AtmOptics_Destroy( AtmOptics )

    END DO Profile_Loop


    ! Destroy any remaining structures
    CALL CRTM_SfcOptics_Destroy( SfcOptics )
    CALL CRTM_Atmosphere_Destroy( Atm )

  END FUNCTION CRTM_Forward


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Forward_Version
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Forward_Version( Id )
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

  SUBROUTINE CRTM_Forward_Version( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_Forward_Version

END MODULE CRTM_Forward_Module
