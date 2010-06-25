!
! CRTM_Tangent_Linear_Module
!
! Module containing the CRTM tangent-linear model function.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 27-Jan-2005
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_Tangent_Linear_Module


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
                                        MAX_SOURCE_ZENITH_ANGLE
  USE CRTM_SpcCoeff,              ONLY: SC, VISIBLE_SENSOR
  USE CRTM_Atmosphere_Define,     ONLY: CRTM_Atmosphere_type, &
                                        CRTM_Atmosphere_Destroy, &
                                        CRTM_Atmosphere_IsValid
  USE CRTM_Surface_Define,        ONLY: CRTM_Surface_type, &
                                        CRTM_Surface_IsValid
  USE CRTM_Geometry_Define,       ONLY: CRTM_Geometry_type, &
                                        CRTM_Geometry_IsValid
  USE CRTM_ChannelInfo_Define,    ONLY: CRTM_ChannelInfo_type
  USE CRTM_Options_Define,        ONLY: CRTM_Options_type, &
                                        CRTM_Options_IsValid
  USE CRTM_Atmosphere,            ONLY: CRTM_Atmosphere_AddLayers, &
                                        CRTM_Atmosphere_AddLayers_TL
  USE CRTM_GeometryInfo_Define,   ONLY: CRTM_GeometryInfo_type, &
                                        CRTM_GeometryInfo_SetValue, &
                                        CRTM_GeometryInfo_GetValue
  USE CRTM_GeometryInfo,          ONLY: CRTM_GeometryInfo_Compute
  USE CRTM_AtmAbsorption,         ONLY: CRTM_AAVariables_type        , &
                                        CRTM_Compute_AtmAbsorption, &
                                        CRTM_Compute_AtmAbsorption_TL, &
                                        CRTM_Destroy_Predictor       , &
                                        CRTM_Allocate_Predictor      , &
                                        CRTM_Compute_Predictors      , &
                                        CRTM_Compute_Predictors_TL   , &
                                        CRTM_Predictor_type          , &
                                        CRTM_APVariables_type    

! **** REPLACE
  USE CRTM_AtmScatter_Define,     ONLY: CRTM_AtmOptics_type     => CRTM_AtmScatter_type    , &
                                        CRTM_Allocate_AtmOptics => CRTM_Allocate_AtmScatter, &
                                        CRTM_Destroy_AtmOptics  => CRTM_Destroy_AtmScatter
! **** WITH THE FOLLOWING
!  USE CRTM_AtmOptics_Define,      ONLY: CRTM_AtmOptics_type    , &
!                                        CRTM_Allocate_AtmOptics, &
!                                        CRTM_Destroy_AtmOptics 
! ****

  USE CRTM_AerosolScatter,        ONLY: CRTM_ASVariables_type         , &
                                        CRTM_Compute_AerosolScatter   , &
                                        CRTM_Compute_AerosolScatter_TL
  USE CRTM_CloudScatter,          ONLY: CRTM_CSVariables_type       , &
                                        CRTM_Compute_CloudScatter   , &
                                        CRTM_Compute_CloudScatter_TL
  USE CRTM_AtmOptics,             ONLY: CRTM_AOVariables_type    , &
                                        CRTM_Combine_AtmOptics   , &
                                        CRTM_Combine_AtmOptics_TL
  USE CRTM_SfcOptics,             ONLY: CRTM_SfcOptics_type     , &
                                        CRTM_Allocate_SfcOptics , &
                                        CRTM_Destroy_SfcOptics  , &
                                        CRTM_Compute_SurfaceT   , &
                                        CRTM_Compute_SurfaceT_TL  
  USE CRTM_RTSolution,            ONLY: CRTM_RTSolution_type      , &
                                        CRTM_Compute_nStreams     , &
                                        CRTM_Compute_RTSolution   , &
                                        CRTM_Compute_RTSolution_TL
  USE RTV_Define,                 ONLY: RTV_type      , &
                                        RTV_Associated, &
                                        RTV_Destroy   , &
                                        RTV_Create
  USE CRTM_AntCorr,               ONLY: CRTM_Compute_AntCorr, &
                                        CRTM_Compute_AntCorr_TL
  USE CRTM_MoleculeScatter,       ONLY: CRTM_Compute_MoleculeScatter, &
                                        CRTM_Compute_MoleculeScatter_TL
  USE CRTM_AncillaryInput_Define, ONLY: CRTM_AncillaryInput_type

  USE CRTM_CloudCoeff,            ONLY: CRTM_CloudCoeff_IsLoaded
  USE CRTM_AerosolCoeff,          ONLY: CRTM_AerosolCoeff_IsLoaded

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
  PUBLIC :: CRTM_Tangent_Linear
  PUBLIC :: CRTM_Tangent_Linear_Version


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Tangent_Linear_Module.f90 8133 2010-05-28 20:26:31Z paul.vandelst@noaa.gov $'


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Tangent_Linear
!
! PURPOSE:
!       Function that calculates tangent-linear top-of-atmosphere (TOA)
!       radiances and brightness temperatures for an input atmospheric
!       profile or profile set and user specified satellites/channels.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Tangent_Linear( Atmosphere       , &
!                                           Surface          , &
!                                           Atmosphere_TL    , &
!                                           Surface_TL       , &
!                                           Geometry         , &
!                                           ChannelInfo      , &
!                                           RTSolution       , &
!                                           RTSolution_TL    , &
!                                           Options = Options  )
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
!       Atmosphere_TL:  Structure containing the tangent-linear Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN)
!
!       Surface_TL:     Structure containing the tangent-linear Surface data.
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
!       RTSolution:     Structure containing the solution to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-2 (n_Channels x n_Profiles)
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       RTSolution_TL:  Structure containing the solution to the tangent-
!                       linear RT equation for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-2 (n_Channels x n_Profiles)
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Options:        Options structure containing the optional forward model
!                       arguments for the CRTM.
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
!       - The Options optional input structure arguments contain
!         spectral information (e.g. emissivity) that must have the same
!         spectral dimensionality (the "L" dimension) as the output
!         RTSolution structures.
!
!       - The INTENT on the output RTSolution arguments are IN OUT rather
!         than just OUT. This is necessary because the arguments may be defined
!         upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Tangent_Linear( &
    Atmosphere   , &  ! FWD Input, M
    Surface      , &  ! FWD Input, M
    Atmosphere_TL, &  ! TL  Input, M
    Surface_TL   , &  ! TL  Input, M
    Geometry     , &  ! Input, M
    ChannelInfo  , &  ! Input, n_Sensors  
    RTSolution   , &  ! FWD Output, L x M  
    RTSolution_TL, &  ! TL  Output, L x M  
    Options      ) &  ! Optional FWD input, M
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type)       , INTENT(IN)     :: Atmosphere(:)      ! M
    TYPE(CRTM_Surface_type)          , INTENT(IN)     :: Surface(:)         ! M
    TYPE(CRTM_Atmosphere_type)       , INTENT(IN)     :: Atmosphere_TL(:)   ! M
    TYPE(CRTM_Surface_type)          , INTENT(IN)     :: Surface_TL(:)      ! M
    TYPE(CRTM_Geometry_type)         , INTENT(IN)     :: Geometry(:)        ! M
    TYPE(CRTM_ChannelInfo_type)      , INTENT(IN)     :: ChannelInfo(:)     ! n_Sensors
    TYPE(CRTM_RTSolution_type)       , INTENT(IN OUT) :: RTSolution(:,:)    ! L x M
    TYPE(CRTM_RTSolution_type)       , INTENT(IN OUT) :: RTSolution_TL(:,:) ! L x M
    TYPE(CRTM_Options_type), OPTIONAL, INTENT(IN)     :: Options(:)         ! M
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Tangent_Linear'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Options_Present
    LOGICAL :: Check_Input
    LOGICAL :: User_Emissivity, User_Direct_Reflectivity
    LOGICAL :: User_AntCorr, Compute_AntCorr
    LOGICAL :: Atmosphere_Invalid, Surface_Invalid, Geometry_Invalid, Options_Invalid
    INTEGER :: Status_FWD, Status_TL
    INTEGER :: iFOV
    INTEGER :: n, n_Sensors,  SensorIndex
    INTEGER :: l, n_Channels, ChannelIndex
    INTEGER :: m, n_Profiles
    INTEGER :: ln
    INTEGER :: n_Full_Streams, mth_Azi
    INTEGER :: AllocStatus(2), AllocStatus_TL(2)
    REAL(fp) :: Source_ZA
    REAL(fp) :: Wavenumber
    ! Local ancillary input structure
    TYPE(CRTM_AncillaryInput_type) :: AncillaryInput
    ! Local atmosphere structure for extra layering
    TYPE(CRTM_Atmosphere_type) :: Atm, Atm_TL
    ! Component variables
    TYPE(CRTM_GeometryInfo_type) :: GeometryInfo
    TYPE(CRTM_Predictor_type)    :: Predictor, Predictor_TL
    TYPE(CRTM_AtmOptics_type)    :: AtmOptics, AtmOptics_TL
    TYPE(CRTM_SfcOptics_type)    :: SfcOptics, SfcOptics_TL
    ! Component variable internals
    TYPE(CRTM_APVariables_type) :: APV  ! Predictor
    TYPE(CRTM_AAVariables_type) :: AAV  ! AtmAbsorption
    TYPE(CRTM_CSVariables_type) :: CSV  ! CloudScatter
    TYPE(CRTM_ASVariables_type) :: ASV  ! AerosolScatter
    TYPE(CRTM_AOVariables_type) :: AOV  ! AtmOptics
    TYPE(RTV_type) :: RTV  ! RTSolution


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! ----------------------------------------
    ! If no sensors or channels, simply return
    ! ----------------------------------------
    n_Sensors  = SIZE(ChannelInfo)
    n_Channels = SUM(ChannelInfo%n_Channels)
    IF ( n_Sensors == 0 .OR. n_Channels == 0 ) RETURN


    ! ---------------------------------
    ! Output RTSolution array too small
    ! ---------------------------------
    IF ( SIZE(RTSolution,   DIM=1) < n_Channels .OR. &
         SIZE(RTSolution_TL,DIM=1) < n_Channels      ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Output RTSolution structure arrays too small (",i0," and ",i0,&
             &") to hold results for the number of requested channels (",i0,")")') &
             SIZE(RTSolution,DIM=1), SIZE(RTSolution_TL,DIM=1), n_Channels
      CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
      RETURN
    END IF


    ! ----------------------------
    ! Check the number of profiles
    ! ----------------------------
    ! Number of atmospheric profiles.
    n_Profiles = SIZE(Atmosphere)

    ! Check the profile dimensionality
    ! of the other mandatory arguments
    IF ( SIZE(Surface)             /= n_Profiles .OR. &
         SIZE(Atmosphere_TL)       /= n_Profiles .OR. &
         SIZE(Surface_TL)          /= n_Profiles .OR. &
         SIZE(Geometry)            /= n_Profiles .OR. &
         SIZE(RTSolution,   DIM=2) /= n_Profiles .OR. &
         SIZE(RTSolution_TL,DIM=2) /= n_Profiles      ) THEN
      Error_Status = FAILURE
      Message = 'Inconsistent profile dimensionality for input arguments.'
      CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
      RETURN
    END IF

    ! Check the profile dimensionality
    ! of the other optional arguments
    Options_Present = .FALSE.
    IF ( PRESENT(Options) ) THEN
      Options_Present = .TRUE.
      IF ( SIZE(Options) /= n_Profiles ) THEN
        Error_Status = FAILURE
        Message = 'Inconsistent profile dimensionality for Options optional input argument.'
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#                           -- PROFILE LOOP --                             #
    !#--------------------------------------------------------------------------#
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
         CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )                      
         RETURN                                                                                 
      END IF

      ! Check the optional Options structure argument
      ! ...Specify default actions
      Check_Input     = .TRUE.
      User_Emissivity = .FALSE.
      User_AntCorr    = .FALSE.
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
            CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
            RETURN
          END IF
          ! Check if the supplied direct reflectivity should be used
          User_Direct_Reflectivity = Options(m)%Use_Direct_Reflectivity
        END IF
        ! Check if antenna correction should be attempted
        User_AntCorr = Options(m)%Use_Antenna_Correction
        ! Copy over ancillary input
        AncillaryInput%SSU    = Options(m)%SSU
        AncillaryInput%Zeeman = Options(m)%Zeeman
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


      ! ----------------------------------------------
      ! Add extra layers to current atmosphere profile
      ! if necessary to handle upper atmosphere
      ! ----------------------------------------------
      ! ...Forward model
      Error_Status = CRTM_Atmosphere_AddLayers( Atmosphere(m), &  ! Input
                                                Atm            )  ! Output
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error adding FWD extra layers to profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN
      END IF
      ! ...Tangent-linear model
      Error_Status = CRTM_Atmosphere_AddLayers_TL( Atmosphere(m)   , &  ! Input
                                                   Atmosphere_TL(m), &  ! Input
                                                   Atm_TL            )  ! Output
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error adding TL extra layers to profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN
      END IF
      ! ...Check the total number of Atm layers
      IF ( Atm%n_Layers > MAX_N_LAYERS .OR. Atm_TL%n_Layers > MAX_N_LAYERS) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Added layers [",i0,"] cause total [",i0,"] to exceed the ",&
               &"maximum allowed [",i0,"] for profile #",i0)' ) &
               Atm%n_Added_Layers, Atm%n_Layers, MAX_N_LAYERS, m
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN
      END IF


      ! ------------------------------------------------
      ! Allocate all local sensor-independent structures
      ! ------------------------------------------------
      ! The AtmOptics structure
      AllocStatus(1)   =CRTM_Allocate_AtmOptics( Atm%n_Layers        , &  ! Input
                                                 MAX_N_LEGENDRE_TERMS, &  ! Input
                                                 MAX_N_PHASE_ELEMENTS, &  ! Input
                                                 AtmOptics             )  ! Output
      AllocStatus_TL(1)=CRTM_Allocate_AtmOptics( Atm%n_Layers        , &  ! Input
                                                 MAX_N_LEGENDRE_TERMS, &  ! Input
                                                 MAX_N_PHASE_ELEMENTS, &  ! Input
                                                 AtmOptics_TL          )  ! Output
      ! The SfcOptics structure
      AllocStatus(2)   =CRTM_Allocate_SfcOptics( MAX_N_ANGLES, &  ! Input
                                                 MAX_N_STOKES, &  ! Input
                                                 SfcOptics     )  ! Output
      AllocStatus_TL(2)=CRTM_Allocate_SfcOptics( MAX_N_ANGLES, &  ! Input
                                                 MAX_N_STOKES, &  ! Input
                                                 SfcOptics_TL  )  ! Output

      IF ( ANY(AllocStatus    /= SUCCESS ) .OR. &
           ANY(AllocStatus_TL /= SUCCESS ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error deallocating local sensor independent data structures for profile #",i0)' ) m  
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN
      END IF


      ! --------------------------
      ! Preprocess some input data
      ! --------------------------
      ! Average surface skin temperature for multi-surface types
      CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics )
      CALL CRTM_Compute_SurfaceT_TL( Surface(m), Surface_TL(m), SfcOptics_TL )


      ! Initialise channel counter for sensor(n)/channel(l) count
      ln = 0
    
      ! -----------
      ! Sensor loop
      ! -----------
      Sensor_Loop: DO n = 1, n_Sensors

        ! Shorter name
        SensorIndex = ChannelInfo(n)%Sensor_Index

        ! Check if antenna correction to be applied for current sensor
        IF ( User_AntCorr .AND. SC(SensorIndex)%AC_Present .AND. iFOV /= 0 ) THEN
          Compute_AntCorr = .TRUE.
        ELSE
          Compute_AntCorr = .FALSE.
        END IF

        ! Compute predictors for AtmAbsorption calcs
        ! ...Allocate the predictor structures
        AllocStatus(1) = CRTM_Allocate_Predictor( SensorIndex , &  ! Input
                                                  Atm%n_Layers, &  ! Input
                                                  Predictor   , &  ! Output
                                                  SaveFWV = 1   )  ! Optional input
        AllocStatus_TL(1) = CRTM_Allocate_Predictor( SensorIndex , &  ! Input
                                                     Atm%n_Layers, &  ! Input
                                                     Predictor_TL  )  ! Output
        IF ( AllocStatus(1) /= SUCCESS .OR. AllocStatus_TL(1) /= SUCCESS ) THEN
          Error_Status=FAILURE
          WRITE( Message,'("Error allocating predictor structures for profile #",i0, &
                 &" and ",a," sensor.")' ) m, SC(SensorIndex)%Sensor_Id
          CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
          RETURN
        END IF
        ! ...And now fill them
        CALL CRTM_Compute_Predictors( SensorIndex   , &  ! Input
                                      Atm           , &  ! Input
                                      GeometryInfo  , &  ! Input
                                      AncillaryInput, &  ! Input
                                      Predictor     , &  ! Output
                                      APV             )  ! Internal variable output
        CALL CRTM_Compute_Predictors_TL( SensorIndex   , &  ! Input
                                         Atm           , &  ! Input
                                         Predictor     , &  ! Input
                                         Atm_TL        , &  ! Input
                                         GeometryInfo  , &  ! Input
                                         AncillaryInput, &  ! Input
                                         Predictor_TL  , &  ! Output
                                         APV             )  ! Internal variable input

        ! Allocate the RTV structure if necessary
        IF( Atm%n_Clouds   > 0 .OR. &
            Atm%n_Aerosols > 0 .OR. &
            SC(SensorIndex)%Sensor_Type == VISIBLE_SENSOR ) THEN
          CALL RTV_Create( RTV, MAX_N_ANGLES, MAX_N_LEGENDRE_TERMS, Atm%n_Layers )
          IF ( .NOT. RTV_Associated(RTV) ) THEN
            Error_Status=FAILURE
            WRITE( Message,'("Error allocating RTV structure for profile #",i0, &
                   &" and ",a," sensor.")' ) m, TRIM(SC(SensorIndex)%Sensor_Id)
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF
        END IF


        ! ------------
        ! Channel loop
        ! ------------
        Channel_Loop: DO l = 1, ChannelInfo(n)%n_Channels

          ! Shorter name
          ChannelIndex = ChannelInfo(n)%Channel_Index(l)
          
          ! Increment channel counter
          ln = ln + 1

          ! Initialisations
          AtmOptics%Optical_Depth         = ZERO
          AtmOptics%Phase_Coefficient     = ZERO
          AtmOptics%Delta_Truncation      = ZERO
          AtmOptics%Single_Scatter_Albedo = ZERO
   
          AtmOptics_TL%Optical_Depth         = ZERO
          AtmOptics_TL%Phase_Coefficient     = ZERO
          AtmOptics_TL%Delta_Truncation      = ZERO
          AtmOptics_TL%Single_Scatter_Albedo = ZERO          

          ! ---------------------------------------------------------------
          ! Determine the number of streams (n_Full_Streams) in up+downward
          ! directions. Currently, n_Full_Streams is determined from the
          ! cloud parameters only. It will also use the aerosol parameters 
          ! when aerosol scattering is included.
          ! ---------------------------------------------------------------
          n_Full_Streams = CRTM_Compute_nStreams( Atm             , &  ! Input
                                                  SensorIndex     , &  ! Input
                                                  ChannelIndex    , &  ! Input
                                                  RTSolution(ln,m)  )  ! Output
          ! Transfer the number of streams
          ! to all the scattering structures
          AtmOptics%n_Legendre_Terms    = n_Full_Streams
          AtmOptics_TL%n_Legendre_Terms = n_Full_Streams


          ! --------------------------
          ! Compute the gas absorption
          ! --------------------------
          CALL CRTM_Compute_AtmAbsorption( SensorIndex   , &  ! Input
                                           ChannelIndex  , &  ! Input
                                           AncillaryInput, &  ! Input
                                           Predictor     , &  ! Input
                                           AtmOptics     , &  ! Output
                                           AAV             )  ! Internal variable output
          CALL CRTM_Compute_AtmAbsorption_TL( SensorIndex     , &  ! Input
                                              ChannelIndex    , &  ! Input
                                              Predictor       , &  ! Input
                                              Predictor_TL    , &  ! Input
                                              AtmOptics_TL    , &  ! Output
                                              AAV               )  ! Internal variable input


          ! -------------------------------------------
          ! Compute the molecular scattering properties
          ! -------------------------------------------
          ! Solar radiation
          IF( SC(SensorIndex)%Solar_Irradiance(ChannelIndex) > ZERO .AND. &
              Source_ZA < MAX_SOURCE_ZENITH_ANGLE) THEN
             RTV%Solar_Flag_true = .TRUE.
          END IF
          
          IF( SC(SensorIndex)%Sensor_Type == VISIBLE_SENSOR .and. RTV%Solar_Flag_true ) THEN 
            RTV%Visible_Flag_true = .true.
            ! Rayleigh phase function has 0, 1, 2 components.
            IF( AtmOptics%n_Legendre_Terms < 4 ) THEN
              AtmOptics%n_Legendre_Terms = 4
              RTSolution(ln,m)%Scattering_FLAG = .TRUE.
              RTSolution(ln,m)%n_Full_Streams = AtmOptics%n_Legendre_Terms + 2
            END IF
            RTV%n_Azi = MIN( AtmOptics%n_Legendre_Terms - 1, MAX_N_AZIMUTH_FOURIER )
            ! Get molecular scattering and extinction
            Wavenumber = SC(SensorIndex)%Wavenumber(ChannelIndex)
            Status_FWD = CRTM_Compute_MoleculeScatter( &
                             Wavenumber, &
                             Atm       , &
                             AtmOptics   )
            Status_TL  = CRTM_Compute_MoleculeScatter_TL( &
                             Wavenumber  , &
                             Atm_TL      , &
                             AtmOptics_TL  )
            IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS) THEN
              Error_Status = FAILURE
              WRITE( Message,'("Error computing MoleculeScatter for ",a,&
                     &", channel ",i0,", profile #",i0)') &
                     TRIM(ChannelInfo(n)%Sensor_ID), &
                     ChannelInfo(n)%Sensor_Channel(l), &
                     m
              CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
              RETURN
            END IF
          ELSE
            RTV%Visible_Flag_true = .FALSE.
            RTV%n_Azi = 0
          END IF        

          
          ! -----------------------------------------------------------
          ! Compute the cloud particle absorption/scattering properties
          ! -----------------------------------------------------------
          IF( Atm%n_Clouds > 0 ) THEN
            Status_FWD = CRTM_Compute_CloudScatter( Atm         , &  ! Input
                                                    SensorIndex , &  ! Input
                                                    ChannelIndex, &  ! Input
                                                    AtmOptics   , &  ! Output
                                                    CSV           )  ! Internal variable output
            Status_TL = CRTM_Compute_CloudScatter_TL( Atm         , &  ! FWD Input
                                                      AtmOptics   , &  ! FWD Input
                                                      Atm_TL      , &  ! TL  Input
                                                      SensorIndex , &  ! Input
                                                      ChannelIndex, &  ! Input
                                                      AtmOptics_TL, &  ! TL  Output
                                                      CSV           )  ! Internal variable input
            IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS) THEN
              Error_Status = FAILURE
              WRITE( Message,'("Error computing CloudScatter for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
              RETURN
            END IF
          END IF



          ! ----------------------------------------------------
          ! Compute the aerosol absorption/scattering properties
          ! ----------------------------------------------------
          IF ( Atm%n_Aerosols > 0 ) THEN
            Status_FWD = CRTM_Compute_AerosolScatter( Atm         , &  ! Input
                                                      SensorIndex , &  ! Input
                                                      ChannelIndex, &  ! Input
                                                      AtmOptics   , &  ! In/Output
                                                      ASV           )  ! Internal variable output
            Status_TL  = CRTM_Compute_AerosolScatter_TL( Atm         , &  ! FWD Input
                                                         AtmOptics   , &  ! FWD Input
                                                         Atm_TL      , &  ! TL  Input
                                                         SensorIndex , &  ! Input
                                                         ChannelIndex, &  ! Input
                                                         AtmOptics_TL, &  ! TL  Output  
                                                         ASV           )  ! Internal variable input
            IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS) THEN
              Error_Status = FAILURE
              WRITE( Message,'("Error computing AerosolScatter for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
              RETURN
            END IF
          END IF


          ! ---------------------------------------------------
          ! Compute the combined atmospheric optical properties
          ! ---------------------------------------------------
          CALL CRTM_Combine_AtmOptics( AtmOptics, & ! In/Output
                                       AOV        ) ! Internal variable output                                       
          CALL CRTM_Combine_AtmOptics_TL( AtmOptics   , & ! FWD Input
                                          AtmOptics_TL, & ! TL  In/Output
                                          AOV           ) ! Internal variable input


          ! ------------------------------------
          ! Fill the SfcOptics structure for the
          ! optional emissivity input case.
          ! ------------------------------------
          ! Indicate SfcOptics ARE to be computed
          SfcOptics%Compute_Switch = SET
          ! Change SfcOptics emissivity/reflectivity
          ! contents/computation status
          IF ( User_Emissivity ) THEN
            SfcOptics%Compute_Switch  = NOT_SET
            SfcOptics%Emissivity(1,1)       = Options(m)%Emissivity(ln)
            SfcOptics%Reflectivity(1,1,1,1) = ONE - Options(m)%Emissivity(ln)
            IF ( User_Direct_Reflectivity ) THEN
              SfcOptics%Direct_Reflectivity(1,1) = Options(m)%Direct_Reflectivity(ln)
            ELSE
              SfcOptics%Direct_Reflectivity(1,1) = SfcOptics%Reflectivity(1,1,1,1)
            END IF
          END IF


        ! ------------
        ! Fourier component loop for azimuth angles,
        ! mth_Azi = 0 is for an azimuth-averaged value,
        ! for example IR and MW thermal radiation
        ! ------------

        RTSolution(ln,m)%Radiance = ZERO
        RTSolution_TL(ln,m)%Radiance = ZERO

          
        Azi_Fourier_Loop: DO mth_Azi = 0, RTV%n_Azi
          RTV%mth_Azi = mth_Azi
          SfcOptics%mth_Azi = mth_Azi
                     
          ! ------------------------------------
          ! Solve the radiative transfer problem
          ! ------------------------------------
          ! Forward model
          Error_Status = CRTM_Compute_RTSolution( Atm             , &  ! Input
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
            CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
            RETURN
          END IF
        
          ! Tangent-linear model
          Error_Status = CRTM_Compute_RTSolution_TL( Atm                , &  ! FWD Input
                                                     Surface(m)         , &  ! FWD Input
                                                     AtmOptics          , &  ! FWD Input
                                                     SfcOptics          , &  ! FWD Input
                                                     RTSolution(ln,m)   , &  ! FWD Input
                                                     Atm_TL             , &  ! TL  Input
                                                     Surface_TL(m)      , &  ! TL  Input
                                                     AtmOptics_TL       , &  ! TL  Input
                                                     SfcOptics_TL       , &  ! TL  Input 
                                                     GeometryInfo       , &  ! Input
                                                     SensorIndex        , &  ! Input
                                                     ChannelIndex       , &  ! Input
                                                     RTSolution_TL(ln,m), &  ! TL  Output
                                                     RTV                  )  ! Internal variable input
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'( "Error computing RTSolution_TL for ", a, &
                   &", channel ", i0,", profile #",i0)' ) &
                   TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
            CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
            RETURN
          END IF

        END DO Azi_Fourier_Loop
 

          ! --------------------------------------
          ! Compute Antenna correction if required
          ! --------------------------------------
          IF ( Compute_AntCorr ) THEN
            CALL CRTM_Compute_AntCorr( GeometryInfo    , &  ! Input
                                       SensorIndex     , &  ! Input
                                       ChannelIndex    , &  ! Input
                                       RTSolution(ln,m)  )  ! Output
            CALL CRTM_Compute_AntCorr_TL( GeometryInfo       , &  ! Input
                                          SensorIndex        , &  ! Input
                                          ChannelIndex       , &  ! Input
                                          RTSolution_TL(ln,m)  )  ! Output
          END IF
          
        END DO Channel_Loop


        ! Deallocate local sensor dependent data structures
        ! ...RTV structure
        IF ( RTV_Associated(RTV) ) CALL RTV_Destroy(RTV)
        ! ...Predictor structures
        AllocStatus(1)    = CRTM_Destroy_Predictor( SensorIndex, Predictor )                     
        AllocStatus_TL(1) = CRTM_Destroy_Predictor( SensorIndex, Predictor_TL )                     
        IF ( AllocStatus(1) /= SUCCESS .OR. AllocStatus_TL(1) /= SUCCESS ) THEN
          WRITE( Message,'("Error deallocating predictor structures for profile #",i0, &
                 &" and ",a," sensor.")' ) m, SC(SensorIndex)%Sensor_Id
          CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
          RETURN
        END IF

      END DO Sensor_Loop


      ! --------------------------------------------------- 
      ! Deallocate local sensor independent data structures   
      ! ---------------------------------------------------
      AllocStatus_TL(1)=CRTM_Destroy_SfcOptics( SfcOptics_TL )
      AllocStatus(1)   =CRTM_Destroy_SfcOptics( SfcOptics )
      AllocStatus_TL(2)=CRTM_Destroy_AtmOptics( AtmOptics_TL )
      AllocStatus(2)   =CRTM_Destroy_AtmOptics( AtmOptics )

      IF ( ANY(AllocStatus /= SUCCESS ) .OR. ANY(AllocStatus_TL /= SUCCESS ) ) THEN                                              
        Error_Status = FAILURE                                                              
        WRITE( Message,'("Error deallocating local sensor independent data structures for profile #",i0)' ) m  
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN
      END IF                                    

    END DO Profile_Loop


    ! ---------------------------------
    ! Destroy "extra layers" structures
    ! ---------------------------------
    CALL CRTM_Atmosphere_Destroy( Atm_TL )
    CALL CRTM_Atmosphere_Destroy( Atm )

  END FUNCTION CRTM_Tangent_Linear


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Tangent_Linear_Version
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Tangent_Linear_Version( Id )
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

  SUBROUTINE CRTM_Tangent_Linear_Version( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_Tangent_Linear_Version

END MODULE CRTM_Tangent_Linear_Module
