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
                                        AIRCRAFT_PRESSURE_THRESHOLD, &
                                        SCATTERING_ALBEDO_THRESHOLD
  USE CRTM_SpcCoeff,              ONLY: SC, &
                                        SpcCoeff_IsVisibleSensor, &
                                        SpcCoeff_IsMicrowaveSensor
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
  USE CRTM_RTSolution_Define,     ONLY: CRTM_RTSolution_type   , &
                                        CRTM_RTSolution_Destroy, &
                                        CRTM_RTSolution_Zero
  USE CRTM_Options_Define,        ONLY: CRTM_Options_type, &
                                        CRTM_Options_IsValid
  USE CRTM_Atmosphere,            ONLY: CRTM_Atmosphere_AddLayers   , &
                                        CRTM_Atmosphere_IsFractional, &
                                        CRTM_Atmosphere_Coverage, &
                                        CRTM_Atmosphere_ClearSkyCopy
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
  USE CRTM_AtmOptics,             ONLY: CRTM_Include_Scattering   , &
                                        CRTM_Compute_Transmittance, &
                                        CRTM_AtmOptics_Combine    , &
                                        CRTM_AtmOptics_NoScatterCopy
  USE CRTM_SfcOptics_Define,      ONLY: CRTM_SfcOptics_type      , &
                                        CRTM_SfcOptics_Associated, &
                                        CRTM_SfcOptics_Create    , &
                                        CRTM_SfcOptics_Destroy
  USE CRTM_SfcOptics,             ONLY: CRTM_Compute_SurfaceT
  USE CRTM_RTSolution,            ONLY: CRTM_Compute_nStreams   , &
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
  USE CRTM_CloudCover_Define,     ONLY: CRTM_CloudCover_type

  ! Internal variable definition modules
  ! ...AtmOptics
  USE AOvar_Define, ONLY: AOvar_type, &
                          AOvar_Associated, &
                          AOvar_Destroy   , &
                          AOvar_Create
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
  USE RTV_Define,   ONLY: RTV_type, &
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
  '$Id: CRTM_Forward_Module.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'


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
    LOGICAL :: compute_antenna_correction
    LOGICAL :: Atmosphere_Invalid, Surface_Invalid, Geometry_Invalid, Options_Invalid
    INTEGER :: iFOV
    INTEGER :: n, n_Sensors,  SensorIndex
    INTEGER :: l, n_Channels, ChannelIndex
    INTEGER :: m, n_Profiles
    INTEGER :: ln
    INTEGER :: n_Full_Streams, mth_Azi
    INTEGER :: cloud_coverage_flag
    REAL(fp) :: Source_ZA
    REAL(fp) :: Wavenumber
    REAL(fp) :: transmittance, transmittance_clear
    ! Local ancillary input structure
    TYPE(CRTM_AncillaryInput_type) :: AncillaryInput
    ! Local options structure for default and use values
    TYPE(CRTM_Options_type) :: Default_Options, Opt
    ! Local atmosphere structure for extra layering
    TYPE(CRTM_Atmosphere_type) :: Atm
    ! Clear sky structures
    TYPE(CRTM_Atmosphere_type) :: Atm_Clear
    TYPE(CRTM_AtmOptics_type)  :: AtmOptics_Clear
    TYPE(CRTM_SfcOptics_type)  :: SfcOptics_Clear
    TYPE(CRTM_RTSolution_type) :: RTSolution_Clear
    TYPE(RTV_type)             :: RTV_Clear
    ! Component variables
    TYPE(CRTM_GeometryInfo_type) :: GeometryInfo
    TYPE(CRTM_Predictor_type)    :: Predictor
    TYPE(CRTM_AtmOptics_type)    :: AtmOptics
    TYPE(CRTM_SfcOptics_type)    :: SfcOptics
    ! Component variable internals
    TYPE(CRTM_PVar_type)  :: PVar   ! Predictor
    TYPE(CRTM_AAvar_type) :: AAvar  ! AtmAbsorption
    TYPE(CSvar_type)      :: CSvar  ! CloudScatter
    TYPE(ASvar_type)      :: ASvar  ! AerosolScatter
    TYPE(AOvar_type)      :: AOvar  ! AtmOptics
    TYPE(RTV_type)        :: RTV    ! RTSolution
    ! NLTE correction term predictor
    TYPE(NLTE_Predictor_type)   :: NLTE_Predictor
    ! Cloud cover object
    TYPE(CRTM_CloudCover_type) :: CloudCover


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


    ! Reinitialise the output RTSolution
    CALL CRTM_RTSolution_Zero(RTSolution)


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
      Opt = Default_Options
      IF ( Options_Present ) THEN
        Opt = Options(m)
        ! Copy over ancillary input (just add AncillaryInput structure to options?)
        AncillaryInput%SSU    = Options(m)%SSU
        AncillaryInput%Zeeman = Options(m)%Zeeman
      END IF
      ! ...Assign the option specific SfcOptics input
      SfcOptics%Use_New_MWSSEM = .NOT. Opt%Use_Old_MWSSEM


      ! Check the input data if required
      IF ( Opt%Check_Input ) THEN
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
          ! Are the channel dimensions consistent if emissivity is passed?
          IF ( Options(m)%Use_Emissivity ) THEN
            IF ( Options(m)%n_Channels < n_Channels ) THEN
              Error_Status = FAILURE
              WRITE( Message,'( "Input Options channel dimension (", i0, ") is less ", &
                     &"than the number of requested channels (",i0, ")" )' ) &
                     Options(m)%n_Channels, n_Channels
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          END IF
          ! Check value for user-defined n_Streams
          IF ( Options(m)%Use_N_Streams ) THEN
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


      ! Prepare the atmospheric optics structures
      ! ...Allocate the AtmOptics structure based on Atm extension
      CALL CRTM_AtmOptics_Create( AtmOptics, &
                                  Atm%n_Layers        , &
                                  MAX_N_LEGENDRE_TERMS, &
                                  MAX_N_PHASE_ELEMENTS  )
      IF ( .NOT. CRTM_AtmOptics_Associated( Atmoptics ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error allocating AtmOptics data structure for profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
      ! ...Set the scattering switch
      AtmOptics%Include_Scattering = Opt%Include_Scattering
      ! ...Allocate the atmospheric optics internal structure
      CALL AOvar_Create( AOvar, Atm%n_Layers )


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


      ! Determine the type of cloud coverage
      cloud_coverage_flag = CRTM_Atmosphere_Coverage( atm )


      ! Setup for fractional cloud coverage
      IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
      
        ! Compute cloudcover
        Error_Status = CloudCover%Compute_CloudCover(atm, Overlap = opt%Overlap_Id)
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error computing cloud cover in profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF

        ! Allocate all the CLEAR sky structures for fractional cloud coverage
        ! ...A clear sky atmosphere
        Error_Status = CRTM_Atmosphere_ClearSkyCopy(Atm, Atm_Clear)
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error copying CLEAR SKY atmopshere in profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
        ! ...Clear sky SfcOptics
        CALL CRTM_SfcOptics_Create( SfcOptics_Clear, MAX_N_ANGLES, MAX_N_STOKES )
        IF ( .NOT. CRTM_SfcOptics_Associated(SfcOptics_Clear) ) THEN
          Error_Status = FAILURE
          WRITE( Message,'("Error allocating CLEAR SKY SfcOptics data structure for profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
        ! ...Copy over surface optics input
        SfcOptics_Clear%Use_New_MWSSEM = .NOT. Opt%Use_Old_MWSSEM
        ! ...CLEAR SKY average surface skin temperature for multi-surface types
        CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics_Clear )
      END IF


      ! Average surface skin temperature for multi-surface types
      CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics )


      ! Process aircraft pressure altitude
      IF ( Opt%Aircraft_Pressure > ZERO ) THEN
        RTV%aircraft%rt = .TRUE.
        RTV%aircraft%idx = CRTM_Get_PressureLevelIdx(Atm, Opt%Aircraft_Pressure)
        ! ...Issue warning if profile level is TOO different from flight level
        IF ( ABS(Atm%Level_Pressure(RTV%aircraft%idx)-Opt%Aircraft_Pressure) > AIRCRAFT_PRESSURE_THRESHOLD ) THEN
          WRITE( Message,'("Difference between aircraft pressure level (",es13.6,&
                          &"hPa) and closest input profile level (",es13.6,&
                          &"hPa) is larger than recommended (",f4.1,"hPa) for profile #",i0)') &
                          Opt%Aircraft_Pressure, Atm%Level_Pressure(RTV%aircraft%idx), &
                          AIRCRAFT_PRESSURE_THRESHOLD, m
          CALL Display_Message( ROUTINE_NAME, Message, WARNING )
        END IF
      ELSE
        RTV%aircraft%rt = .FALSE.
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
        compute_antenna_correction = ( Opt%Use_Antenna_Correction               .AND. &
                                       ACCoeff_Associated( SC(SensorIndex)%AC ) .AND. &
                                       iFOV /= 0 )


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
        IF( ( Atm%n_Clouds   > 0 .OR. &
              Atm%n_Aerosols > 0 .OR. &
              SpcCoeff_IsVisibleSensor(SC(SensorIndex)) ) .AND. &
            AtmOptics%Include_Scattering ) THEN
          CALL RTV_Create( RTV, MAX_N_ANGLES, MAX_N_LEGENDRE_TERMS, Atm%n_Layers )
          IF ( .NOT. RTV_Associated(RTV) ) THEN
            Error_Status=FAILURE
            WRITE( Message,'("Error allocating RTV structure for profile #",i0, &
                   &" and ",a," sensor.")' ) m, TRIM(SC(SensorIndex)%Sensor_Id)
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF
          ! Assign algorithm selector
          RTV%RT_Algorithm_Id = Opt%RT_Algorithm_Id
        END IF


        ! Compute NLTE correction predictors
        IF ( Opt%Apply_NLTE_Correction ) THEN
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
          CALL CRTM_AtmOptics_Zero( AtmOptics_Clear )
          CALL CRTM_RTSolution_Zero( RTSolution_Clear )


          ! Determine the number of streams (n_Full_Streams) in up+downward directions
          IF ( Opt%Use_N_Streams ) THEN
            n_Full_Streams = Opt%n_Streams
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


          ! Gamma correction to optical depth
          AtmOptics%Optical_Depth = AtmOptics%Optical_Depth * (RTSolution(ln,m)%Gamma + ONE)


          ! Compute the molecular scattering properties
          ! ...Solar radiation
          IF ( SC(SensorIndex)%Solar_Irradiance(ChannelIndex) > ZERO .AND. &
               Source_ZA < MAX_SOURCE_ZENITH_ANGLE ) THEN
            RTV%Solar_Flag_true = .TRUE.
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) RTV_Clear%Solar_Flag_true = .TRUE.
          END IF
          ! ...Visible channel with solar radiation
          IF ( SpcCoeff_IsVisibleSensor(SC(SensorIndex)) .AND. RTV%Solar_Flag_true ) THEN
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
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
              RTV_Clear%Visible_Flag_true = .FALSE.
              RTV_Clear%n_Azi = 0
            END IF
          END IF


          ! Copy the clear-sky AtmOptics
          IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
            Error_Status = CRTM_AtmOptics_NoScatterCopy( AtmOptics, AtmOptics_Clear )
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error copying CLEAR SKY AtmOptics for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
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
          END IF


          ! Compute the combined atmospheric optical properties
          IF( AtmOptics%Include_Scattering ) THEN
            CALL CRTM_AtmOptics_Combine( AtmOptics, AOvar )
          END IF
          ! ...Save vertically integrated scattering optical depth for output
          RTSolution(ln,m)%SOD = AtmOptics%Scattering_Optical_Depth


          ! Compute the all-sky atmospheric transmittance
          ! for use in FASTEM-X reflection correction
          CALL CRTM_Compute_Transmittance(AtmOptics,transmittance)
          SfcOptics%Transmittance = transmittance
          ! ...Clear sky for fractional cloud cover
          IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
            CALL CRTM_Compute_Transmittance(AtmOptics_Clear,transmittance_clear)
            SfcOptics_Clear%Transmittance = transmittance_clear
          END IF


          ! Fill the SfcOptics structures for the optional emissivity input case.
          SfcOptics%Compute       = .TRUE.
          SfcOptics_Clear%Compute = .TRUE.
          IF ( Opt%Use_Emissivity ) THEN
            ! ...Cloudy/all-sky case
            SfcOptics%Compute = .FALSE.
            SfcOptics%Emissivity(1,1)       = Opt%Emissivity(ln)
            SfcOptics%Reflectivity(1,1,1,1) = ONE - Opt%Emissivity(ln)
            IF ( Opt%Use_Direct_Reflectivity ) THEN
              SfcOptics%Direct_Reflectivity(1,1) = Opt%Direct_Reflectivity(ln)
            ELSE
              SfcOptics%Direct_Reflectivity(1,1) = SfcOptics%Reflectivity(1,1,1,1)
            END IF
            ! ...Repeat for fractional clear-sky case
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
              SfcOptics_Clear%Compute = .FALSE.
              SfcOptics_Clear%Emissivity(1,1)       = Opt%Emissivity(ln)
              SfcOptics_Clear%Reflectivity(1,1,1,1) = ONE - Opt%Emissivity(ln)
              IF ( Opt%Use_Direct_Reflectivity ) THEN
                SfcOptics_Clear%Direct_Reflectivity(1,1) = Opt%Direct_Reflectivity(ln)
              ELSE
                SfcOptics_Clear%Direct_Reflectivity(1,1) = SfcOptics%Reflectivity(1,1,1,1)
              END IF
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


            ! Repeat clear sky for fractionally cloudy atmospheres
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
              RTV_Clear%mth_Azi = mth_Azi
              SfcOptics_Clear%mth_Azi = mth_Azi
              Error_Status = CRTM_Compute_RTSolution( &
                               Atm_Clear       , &  ! Input
                               Surface(m)      , &  ! Input
                               AtmOptics_Clear , &  ! Input
                               SfcOptics_Clear , &  ! Input
                               GeometryInfo    , &  ! Input
                               SensorIndex     , &  ! Input
                               ChannelIndex    , &  ! Input
                               RTSolution_Clear, &  ! Output
                               RTV_Clear         )  ! Internal variable output
              IF ( Error_Status /= SUCCESS ) THEN
                WRITE( Message,'( "Error computing CLEAR SKY RTSolution for ", a, &
                       &", channel ", i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                RETURN
              END IF
            END IF

          END DO Azimuth_Fourier_Loop


          ! Combine cloudy and clear radiances for fractional cloud coverage
          IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
            RTSolution(ln,m)%Radiance = &
                ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_Clear%Radiance) + &
                (CloudCover%Total_Cloud_Cover * RTSolution(ln,m)%Radiance)
            ! ...Save the cloud cover in the output structure
            RTSolution(ln,m)%Total_Cloud_Cover = CloudCover%Total_Cloud_Cover
          END IF


          ! The radiance post-processing
          CALL Post_Process_RTSolution(RTSolution(ln,m))


          ! Perform clear-sky post-processing
          IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
            CALL Post_Process_RTSolution(RTSolution_Clear)
            ! ...Save the results in the output structure
            RTSolution(ln,m)%R_Clear  = RTSolution_Clear%Radiance
            RTSolution(ln,m)%Tb_Clear = RTSolution_Clear%Brightness_Temperature
          END IF

        END DO Channel_Loop

      END DO Sensor_Loop

    END DO Profile_Loop


    ! Clean up
    CALL CRTM_Predictor_Destroy( Predictor )
    CALL CRTM_AtmOptics_Destroy( AtmOptics )
    CALL CRTM_AtmOptics_Destroy( AtmOptics_Clear )
    CALL CRTM_SfcOptics_Destroy( SfcOptics )
    CALL CRTM_SfcOptics_Destroy( SfcOptics_Clear )
    CALL CRTM_Atmosphere_Destroy( Atm )
    CALL CRTM_Atmosphere_Destroy( Atm_Clear )
    ! ...Internal variables
    CALL AOvar_Destroy( AOvar )
    CALL CSvar_Destroy( CSvar )
    CALL ASvar_Destroy( ASvar )
    CALL RTV_Destroy( RTV ) 


  CONTAINS


    ! ----------------------------------------------------------------
    ! Local subroutine to post-process the radiance, as it is the same
    ! for all-sky and fractional clear-sky cases.
    !
    !   1. Apply non-LTE correction to radiance
    !   2. Convert radiance to brightness temperature
    !   3. Apply antenna correction to brightness temperature
    ! ----------------------------------------------------------------

    SUBROUTINE Post_Process_RTSolution(rts)
      TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: rts

      ! Compute non-LTE correction to radiance if required
      IF ( Opt%Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
        CALL Compute_NLTE_Correction( &
               SC(SensorIndex)%NC, &  ! Input
               ChannelIndex      , &  ! Input
               NLTE_Predictor    , &  ! Input
               rts%Radiance        )  ! In/Output
      END IF
      ! Convert the radiance to brightness temperature
      CALL CRTM_Planck_Temperature( &
             SensorIndex               , & ! Input
             ChannelIndex              , & ! Input
             rts%Radiance              , & ! Input
             rts%Brightness_Temperature  ) ! Output
      ! Compute Antenna correction to brightness temperature if required
      IF ( compute_antenna_correction ) THEN
        CALL CRTM_Compute_AntCorr( &
               GeometryInfo, &  ! Input
               SensorIndex , &  ! Input
               ChannelIndex, &  ! Input
               rts           )  ! Output
      END IF

    END SUBROUTINE Post_Process_RTSolution

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
